use std::cmp::Ordering;
use w_errors::Message;
use w_lexer::{AmbiguousOp, BinOp, Lexer, Span, Token, UnOp};

mod handler;
mod primaryatom;

pub use handler::Handler;
use w_ast::{Atom, AtomVariant, IdentPair, IncDec, Indir, Program, Type, WFn};

pub struct Parser<'a, H, I>
where
    H: Handler<LexerInput = I>,
    I: Iterator<Item = char>,
{
    session: &'a H,
    src_ref: H::SourceRef,
    lex: Lexer<I>,
    token_buffer: Option<Token>,
}

impl<'a, H, I> Parser<'a, H, I>
where
    H: Handler<LexerInput = I>,
    I: Iterator<Item = char>,
{
    pub fn new(session: &'a H, src_ref: H::SourceRef) -> Self {
        let src = session.get_source(&src_ref);

        Parser {
            session,
            src_ref,
            lex: Lexer::new(src),
            token_buffer: None,
        }
    }

    fn next(&mut self) -> Option<Token> {
        self.token_buffer.take().or_else(|| self.lex.next())
    }

    pub fn error(&self, msg: Message, span: Span) {
        self.session.error(&self.src_ref, msg, span);
    }

    pub fn block(&mut self) -> Option<Atom> {
        let mut r = Vec::new();
        let start = self.lex.span();

        let mut last = None;

        loop {
            if let Some(t) = last.take() {
                r.push(t);
            }

            match self.next() {
                Some(Token::RightBracket) => {
                    last = None;
                    break;
                }
                None => {}
                t => {
                    self.token_buffer = t;

                    let a = self.atom();
                    match a {
                        Some(_) => last = a,
                        None => {
                            // panic!!
                            loop {
                                let t = self.next();
                                match t {
                                    Some(Token::RightBracket | Token::Semicolon) => {
                                        self.token_buffer = t;
                                        break;
                                    }
                                    None => break,
                                    _ => {}
                                }
                            }
                        }
                    }
                }
            }

            let err_span = self.lex.span();

            match self.next() {
                Some(Token::RightBracket) => break,
                Some(Token::Semicolon) => {}
                None => {
                    self.error(Message::MissingClosingBracket, err_span);
                    break;
                }
                t => {
                    // try to continue
                    self.error(Message::MissingSemicolon, err_span);
                    self.token_buffer = t;
                }
            }
        }

        let span = start.to(self.lex.span());
        Some(Atom {
            t: last.as_ref().map_or_else(Type::void, |a| a.t),
            v: AtomVariant::Block(r, last.map(Box::new)),
            span,
        })
    }

    fn parse_type(&mut self) -> Option<Type> {
        let mut indir = Indir::none();
        let mut len = 0;
        loop {
            match self.next() {
                Some(Token::AmbiguousOp(AmbiguousOp::Asterisk)) => {
                    match len.partial_cmp(&5).unwrap() {
                        Ordering::Less => {
                            indir = indir.add(match self.next() {
                                Some(Token::Mut) => true,
                                t => {
                                    self.token_buffer = t;
                                    false
                                }
                            });
                        }
                        Ordering::Equal | Ordering::Greater => {
                            self.error(Message::TooMuchIndirection, self.lex.span());
                        }
                    }
                    len += 1;
                }
                Some(Token::Ident(s)) => {
                    return Some(Type::with_indir(s.into(), indir));
                }
                _ => {
                    self.error(Message::MalformedType, self.lex.span());
                    return None;
                }
            }
        }
    }

    fn ident_type_pair(&mut self, require_type: bool) -> Option<IdentPair> {
        let mut t = self.next();
        let mutable = match t {
            Some(Token::Mut) => {
                t = self.next();
                true
            }
            t_ => {
                t = t_;
                false
            }
        };

        let ident = match t {
            Some(Token::Ident(s)) => {
                t = self.next();
                s
            }
            Some(Token::Colon) => {
                self.error(Message::MissingIdentifier, self.lex.span());
                "unknown".to_owned()
            }
            _ => {
                t = self.next();
                self.error(Message::MalformedIdentifier, self.lex.span());
                "unknown".to_owned()
            }
        };

        let t = match t {
            Some(Token::Colon) => self.parse_type()?,
            t => {
                self.token_buffer = t;

                if require_type {
                    self.error(Message::MissingType, self.lex.span());
                    return None;
                }

                Type::auto()
            }
        };

        Some(IdentPair { mutable, ident, t })
    }

    fn postfixatom(&mut self, mut lhs: Atom) -> Option<Atom> {
        loop {
            match self.next() {
                Some(Token::UnOp(UnOp::Dec)) => {
                    lhs = Atom {
                        span: lhs.span.to(self.lex.span()),
                        v: AtomVariant::PostIncDec(Box::new(lhs), IncDec::Dec),
                        t: Type::auto(),
                    }
                }

                Some(Token::UnOp(UnOp::Inc)) => {
                    lhs = Atom {
                        span: lhs.span.to(self.lex.span()),
                        v: AtomVariant::PostIncDec(Box::new(lhs), IncDec::Inc),
                        t: Type::auto(),
                    }
                }

                Some(Token::Period) => {
                    let period_span = self.lex.span();
                    match self.next() {
                        Some(Token::Ident(s)) => {
                            lhs = Atom {
                                span: lhs.span.to(self.lex.span()),
                                v: AtomVariant::Access(Box::new(lhs), s),
                                t: Type::auto(),
                            }
                        }
                        t => {
                            self.token_buffer = t;
                            self.error(Message::MissingIdentifier, period_span.move_by(1));
                            return None;
                        }
                    }
                }

                Some(Token::LeftSqBracket) => {
                    let atom = self.atom()?;

                    match self.next() {
                        Some(Token::RightSqBracket) => {
                            lhs = Atom {
                                span: lhs.span.to(self.lex.span()),
                                v: AtomVariant::Index(Box::new(lhs), Box::new(atom)),
                                t: Type::auto(),
                            }
                        }
                        t => {
                            self.token_buffer = t;
                            self.error(Message::MissingClosingSqBracket, self.lex.span());
                            return None;
                        }
                    }
                }

                Some(Token::LeftParen) => {
                    let mut args = Vec::new();

                    match self.next() {
                        Some(Token::RightParen) => {}
                        t => {
                            self.token_buffer = t;
                            // TODO: remove

                            loop {
                                args.push(self.atom()?);

                                match self.next() {
                                    Some(Token::Comma) => {}
                                    Some(Token::RightParen) => break,
                                    t => {
                                        self.token_buffer = t;
                                        self.error(Message::MissingClosingParen, self.lex.span());
                                        return None;
                                    }
                                }
                            }
                        }
                    }

                    lhs = Atom {
                        span: lhs.span.to(self.lex.span()),
                        v: AtomVariant::Call(Box::new(lhs), args),
                        t: Type::auto(),
                    }
                }

                t => {
                    self.token_buffer = t;
                    break;
                }
            }
        }

        Some(lhs)
    }

    fn simpleatom(&mut self, t: Option<Token>) -> Option<Atom> {
        let lhs = match t {
            Some(Token::LeftParen) => {
                let start = self.lex.span();
                let e = self.atom()?;

                match self.next() {
                    Some(Token::RightParen) => {}
                    t => {
                        self.token_buffer = t;
                        self.error(Message::MissingClosingParen, self.lex.span());
                        return None;
                    }
                }

                Atom {
                    t: e.t,
                    v: AtomVariant::Paren(Box::new(e)),
                    span: start.to(self.lex.span()),
                }
            }
            Some(Token::Float(f)) => Atom {
                v: AtomVariant::Float(f),
                span: self.lex.span(),
                t: Type::auto(),
            },
            Some(Token::Integer(i)) => Atom {
                v: AtomVariant::Integer(i),
                span: self.lex.span(),
                t: Type::auto(),
            },
            Some(Token::Ident(s)) => Atom {
                v: AtomVariant::Ident(s),
                span: self.lex.span(),
                t: Type::auto(),
            },
            Some(Token::String(s)) => Atom {
                v: AtomVariant::String(s),
                span: self.lex.span(),
                t: Type::auto(),
            },
            Some(Token::Char(s)) => Atom {
                v: AtomVariant::Char(s),
                span: self.lex.span(),
                t: Type::auto(),
            },
            Some(Token::LeftBracket) => self.block()?,
            _ => {
                self.error(Message::UnexpectedToken, self.lex.span());
                return None;
            }
        };

        self.postfixatom(lhs)
    }

    fn subatom(&mut self, mut lhs: Atom, min_prec: u8) -> Option<Atom> {
        // https://en.wikipedia.org/wiki/Operator-precedence_parser
        let mut l = self.next();
        loop {
            let t = match l {
                Some(Token::BinOp(t)) => t,
                Some(Token::AmbiguousOp(ref t)) => BinOp::Regular(t.binary()),
                _ => {
                    self.token_buffer = l;
                    break;
                }
            };

            // from the article:
            // binary operator whose precedence is >= min_precedence
            let current_prec = t.prec();
            if t.prec() >= min_prec {
                let mut rhs = self.primaryatom()?;

                l = self.next();
                loop {
                    // Wikipedia's pseudocode is wrong. if t1 is a right associative op
                    // with equal precedence, then trying to match t2 with a higher
                    // min_prec is impossible
                    let (cond, next_prec) = if let Some(Token::BinOp(BinOp::Compound(v))) = l {
                        let lookahead_prec = v.prec();
                        (current_prec == lookahead_prec, current_prec + 1)
                    } else {
                        let lookahead_prec = match l {
                            Some(Token::BinOp(t)) => t.prec(),
                            Some(Token::AmbiguousOp(t)) => t.binary().prec(),
                            _ => break,
                        };

                        (lookahead_prec > current_prec, current_prec)
                    };

                    if cond {
                        self.token_buffer = l;
                        rhs = self.subatom(rhs, next_prec)?;
                        l = self.next();
                    } else {
                        break;
                    }
                }

                lhs = Atom {
                    span: lhs.span.to(rhs.span),
                    v: AtomVariant::BinOp(Box::new(lhs), t, Box::new(rhs)),
                    t: Type::auto(),
                };
            } else {
                self.token_buffer = l;
                break;
            }
        }
        Some(lhs)
    }

    pub fn atom(&mut self) -> Option<Atom> {
        let lhs = self.primaryatom()?;
        self.subatom(lhs, 0)
    }

    pub fn function(&mut self) -> Option<WFn> {
        let name = match self.next() {
            Some(Token::Ident(s)) => s,
            t => {
                self.token_buffer = t;
                return None;
            }
        };

        match self.next() {
            Some(Token::LeftParen) => {}
            t => {
                self.token_buffer = t;
                return None;
            }
        }

        let mut params = Vec::new();

        match self.next() {
            Some(Token::RightParen) => {}
            t => {
                self.token_buffer = t;
                // TODO: remove

                loop {
                    params.push(self.ident_type_pair(true)?);
                    match self.next() {
                        Some(Token::Comma) => {}
                        Some(Token::RightParen) => break,
                        t => {
                            self.token_buffer = t;
                            self.error(Message::MissingClosingParen, self.lex.span());
                            return None;
                        }
                    }
                }
            }
        }

        let t = match self.next() {
            Some(Token::Colon) => self.parse_type()?,
            t => {
                self.token_buffer = t;
                Type::void()
            }
        };

        let atom = self.atom()?;

        Some(WFn {
            name,
            params,
            atom,
            t,
        })
    }

    pub fn panic_top_level(&mut self) {
        loop {
            let t = self.next();
            match t {
                None | Some(Token::Fn) => {
                    self.token_buffer = t;
                    break;
                }
                _ => {}
            }
        }
    }

    pub fn parse(mut self) -> Program {
        let mut fns = Vec::new();
        while let Some(t) = self.next() {
            if t == Token::Fn {
                match self.function() {
                    Some(f) => fns.push(f),
                    None => self.panic_top_level(),
                }
            } else {
                self.error(Message::InvalidTopLevel, self.lex.span());
                self.panic_top_level();
            }
        }

        Program { fns }
    }
}