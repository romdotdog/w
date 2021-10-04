/*
    TODO:
    * Diagnostics
    * Fill all `todo!()`s
*/

use std::cmp::Ordering;

use crate::{
    diag::Message,
    lexer::{AmbiguousOp, BinOp, BinOpVariant, Lexer, Token, UnOp},
    parser::ast::{Type, WFn},
    Session, SourceRef,
};

pub mod codegen;
pub mod indir;
use indir::Indir;
mod ast;
pub use ast::Atom;

use self::ast::{AtomVariant, IdentPair, IncDec, Program};

pub struct Parser<'a> {
    session: &'a Session,
    lex: Lexer<'a>,
    token_buffer: Option<Token>,
}

impl<'a> Parser<'a> {
    pub fn new(session: &'a Session, src: SourceRef) -> Self {
        Parser {
            session,
            lex: Lexer::new(session, src),
            token_buffer: None,
        }
    }

    fn next(&mut self) -> Option<Token> {
        self.token_buffer.take().or_else(|| self.lex.next())
    }

    pub fn block(&mut self) -> Option<Atom> {
        let mut r = Vec::new();
        let start = self.lex.span();

        let mut last = None;

        loop {
            match last.take() {
                Some(t) => r.push(t),
                None => {}
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
                                    Some(Token::RightBracket) | Some(Token::Semicolon) => {
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
                    self.session.error(Message::MissingClosingBracket, err_span);
                    break;
                }
                t => {
                    // try to continue
                    self.session.error(Message::MissingSemicolon, err_span);
                    self.token_buffer = t;
                }
            }
        }

        let span = start.to(self.lex.span());
        Some(Atom {
            t: last.as_ref().map_or_else(|| Type::void(), |a| a.t),
            v: AtomVariant::Block(r, last.map(|a| Box::new(a))),
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
                        Ordering::Less => indir.add(match self.next() {
                            Some(Token::Mut) => true,
                            t => {
                                self.token_buffer = t;
                                false
                            }
                        }),
                        Ordering::Equal => self
                            .session
                            .error(Message::TooMuchIndirection, self.lex.span()),
                        Ordering::Greater => {}
                    }
                    len += 1;
                }
                Some(Token::Ident(s)) => {
                    return Some(Type::with_indir(s.into(), indir));
                }
                _ => {
                    self.session.error(Message::MalformedType, self.lex.span());
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
                self.session
                    .error(Message::MissingIdentifier, self.lex.span());
                "unknown".to_owned()
            }
            _ => {
                t = self.next();
                self.session
                    .error(Message::MalformedIdentifier, self.lex.span());
                "unknown".to_owned()
            }
        };

        let t = match t {
            Some(Token::Colon) => self.parse_type()?,
            t => {
                self.token_buffer = t;
                if require_type {
                    self.session.error(Message::MissingType, self.lex.span());
                    return None;
                } else {
                    Type::auto()
                }
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
                            self.session
                                .error(Message::MissingIdentifier, period_span.move_by(1));
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
                            self.session
                                .error(Message::MissingClosingSqBracket, self.lex.span());
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
                                        self.session
                                            .error(Message::MissingClosingParen, self.lex.span());
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
                        self.session
                            .error(Message::MissingClosingParen, self.lex.span());
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
                self.session
                    .error(Message::UnexpectedToken, self.lex.span());
                return None;
            }
        };

        self.postfixatom(lhs)
    }

    fn primaryatom(&mut self) -> Option<Atom> {
        Some(match self.next() {
            Some(Token::Let) => {
                let start = self.lex.span();
                let pair = self.ident_type_pair(false)?;

                let rhs = match self.next() {
                    Some(Token::BinOp(BinOp::Compound(BinOpVariant::Id))) => {
                        Some(Box::new(self.atom()?))
                    }
                    t => {
                        self.token_buffer = t;
                        None
                    }
                };

                Atom {
                    t: Type::void(),
                    v: AtomVariant::Let(pair, rhs),
                    span: start.to(self.lex.span()),
                }
            }
            Some(Token::BinOp(BinOp::Regular(BinOpVariant::Lt))) => {
                let start = self.lex.span();
                let t = self.parse_type()?;
                let mut op = UnOp::Cast;

                match self.next() {
                    Some(Token::BinOp(BinOp::Regular(BinOpVariant::Gt))) => {}
                    t1 => {
                        match t1 {
                            Some(Token::UnOp(UnOp::LNot)) => op = UnOp::Reinterpret,
                            t => self.token_buffer = t,
                        }

                        match self.next() {
                            Some(Token::BinOp(BinOp::Regular(BinOpVariant::Gt))) => {}
                            t => {
                                self.session
                                    .error(Message::MissingClosingAngleBracket, self.lex.span());
                                self.token_buffer = t;
                                return None;
                            }
                        }
                    }
                }

                Atom {
                    v: AtomVariant::UnOp(op, Box::new(self.primaryatom()?)),
                    span: start.to(self.lex.span()),
                    t,
                }
            }
            Some(Token::If) => {
                let start = self.lex.span();
                let cond = Box::new(self.atom()?);
                let body = Box::new(self.atom()?);
                let mut else_body = None;
                match self.next() {
                    Some(Token::Else) => {
                        else_body = Some(Box::new(self.atom()?));
                    }
                    t => self.token_buffer = t,
                }

                Atom {
                    t: Type::auto(),
                    v: AtomVariant::If(cond, body, else_body),
                    span: start.to(self.lex.span()),
                }
            }
            Some(Token::Loop) => {
                let start = self.lex.span();
                let initial = Box::new(self.atom()?);
                let loop_body = Box::new(self.atom()?);

                Atom {
                    t: Type::auto(),
                    v: AtomVariant::Loop(initial, loop_body),
                    span: start.to(self.lex.span()),
                }
            }
            Some(Token::Br) => {
                let start = self.lex.span();

                Atom {
                    t: Type::auto(),
                    v: AtomVariant::Br(match self.next() {
                        Some(Token::If) => Some(Box::new(self.atom()?)),
                        t => {
                            self.token_buffer = t;
                            None
                        }
                    }),
                    span: start.to(self.lex.span()),
                }
            }
            Some(Token::Return) => {
                let start = self.lex.span();
                let e = self.atom()?;

                Atom {
                    t: e.t,
                    v: AtomVariant::Return(Box::new(e)),
                    span: start.to(self.lex.span()),
                }
            }
            Some(Token::AmbiguousOp(o)) => {
                let start = self.lex.span();
                let e = self.primaryatom()?;

                Atom {
                    t: e.t,
                    v: AtomVariant::UnOp(o.to_unary(), Box::new(e)),
                    span: start.to(self.lex.span()),
                }
            }
            Some(Token::UnOp(u)) => {
                let start = self.lex.span();
                let e = self.primaryatom()?;

                Atom {
                    t: e.t,
                    v: AtomVariant::UnOp(u, Box::new(e)),
                    span: start.to(self.lex.span()),
                }
            }
            t => self.simpleatom(t)?,
        })
    }

    fn subatom(&mut self, mut lhs: Atom, min_prec: u8) -> Option<Atom> {
        // https://en.wikipedia.org/wiki/Operator-precedence_parser
        let mut l = self.next();
        loop {
            let t = match l {
                Some(Token::BinOp(t)) => t,
                Some(Token::AmbiguousOp(ref t)) => BinOp::Regular(t.to_binary()),
                _ => {
                    self.token_buffer = l;
                    break;
                }
            };

            // from the article:
            // binary operator whose precedence is >= min_precedence
            let t_prec = t.prec();
            if t.prec() >= min_prec {
                let mut rhs = self.primaryatom()?;

                l = self.next();
                loop {
                    // Wikipedia's pseudocode is wrong. if t1 is a right associative op
                    // with equal precedence, then trying to match t2 with a higher
                    // min_prec is impossible
                    let (cond, next_prec) = match l {
                        Some(Token::BinOp(BinOp::Compound(v))) => {
                            let t2_prec = v.prec();
                            (t_prec == t2_prec, t_prec + 1)
                        }

                        _ => {
                            let t2_prec = match l {
                                Some(Token::BinOp(t)) => t.prec(),
                                Some(Token::AmbiguousOp(t)) => t.to_binary().prec(),
                                _ => break,
                            };

                            (t2_prec > t_prec, t_prec)
                        }
                    };

                    if cond {
                        self.token_buffer = l;
                        rhs = self.subatom(rhs, next_prec)?;
                        l = self.next()
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
                            self.session
                                .error(Message::MissingClosingParen, self.lex.span());
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
            match t {
                Token::Fn => match self.function() {
                    Some(f) => fns.push(f),
                    None => self.panic_top_level(),
                },
                _ => {
                    self.session
                        .error(Message::InvalidTopLevel, self.lex.span());
                    self.panic_top_level()
                }
            }
        }

        Program { fns }
    }
}
