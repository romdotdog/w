use w_errors::Message;
use w_lexer::{AmbiguousOp, BinOp, Lexer, Span, Token};

mod handler;
mod primaryatom;
mod simpleatom;

pub use handler::Handler;
use w_ast::{Atom, AtomVariant, IdentPair, Indir, Program, Type, WFn};

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

    pub(crate) fn error(&self, msg: Message, span: Span) {
        self.session.error(&self.src_ref, msg, span);
    }

    pub(crate) fn backtrack(&mut self, t: Option<Token>) {
        assert!(
            self.token_buffer.is_none(),
            "double backtrack is not allowed"
        );
        self.token_buffer = t;
    }

    fn parse_type(&mut self) -> Option<Type> {
        let mut indir = Indir::none();
        let mut len = 0;
        loop {
            match self.next() {
                Some(Token::AmbiguousOp(AmbiguousOp::Asterisk)) => {
                    if len < 5 {
                        indir = indir.add(match self.next() {
                            Some(Token::Mut) => true,
                            t => {
                                self.backtrack(t);
                                false
                            }
                        });
                    } else {
                        self.error(Message::TooMuchIndirection, self.lex.span());
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
            Some(Token::Colon) => {
                self.error(Message::MissingIdentifier, self.lex.span());
                "<unknown>".to_owned()
            }
            tt => {
                t = self.next();
                match tt {
                    Some(Token::Ident(s)) => s,
                    Some(Token::Label(s)) => {
                        self.error(Message::LabelIsNotIdentifier, self.lex.span());
                        format!("${}", s)
                    }
                    _ => {
                        self.error(Message::MalformedIdentifier, self.lex.span());
                        "<unknown>".to_owned()
                    }
                }
            }
        };

        let t = match t {
            Some(Token::Colon) => self.parse_type()?,
            t => {
                self.backtrack(t);

                if require_type {
                    self.error(Message::MissingType, self.lex.span());
                    return None;
                }

                Type::auto()
            }
        };

        Some(IdentPair { mutable, ident, t })
    }

    fn subatom(&mut self, mut lhs: Atom, min_prec: u8) -> Option<Atom> {
        // https://en.wikipedia.org/wiki/Operator-precedence_parser
        let mut l = self.next();
        loop {
            let t = match l {
                Some(Token::BinOp(t)) => t,
                Some(Token::AmbiguousOp(ref t)) => BinOp::Regular(t.binary()),
                _ => {
                    self.backtrack(l);
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
                        self.backtrack(l);
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
                self.backtrack(l);
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
                self.backtrack(t);
                return None;
            }
        };

        match self.next() {
            Some(Token::LeftParen) => {}
            t => {
                self.backtrack(t);
                return None;
            }
        }

        let mut params = Vec::new();

        match self.next() {
            Some(Token::RightParen) => {}
            t => {
                self.backtrack(t);
                // TODO: remove

                loop {
                    params.push(self.ident_type_pair(true)?);
                    match self.next() {
                        Some(Token::Comma) => {}
                        Some(Token::RightParen) => break,
                        t => {
                            self.backtrack(t);
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
                self.backtrack(t);
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
                    self.backtrack(t);
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
