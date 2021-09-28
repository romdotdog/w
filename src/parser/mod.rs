/*
    TODO:
    * Diagnostics
    * Fill all `todo!()`s
*/

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

use self::ast::{AtomVariant, Declaration, Program};

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

                    let a = self.expr();
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

            match self.next() {
                Some(Token::RightBracket) => break,
                Some(Token::Semicolon) => {}
                None => {
                    self.session
                        .error(Message::MissingClosingBracket, self.lex.span());
                    break;
                }
                t => {
                    // try to continue
                    self.session
                        .error(Message::MissingSemicolon, self.lex.span());
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
        loop {
            match self.next() {
                Some(Token::AmbiguousOp(AmbiguousOp::Asterisk)) => indir.add(match self.next() {
                    Some(Token::Mut) => true,
                    t => {
                        self.token_buffer = t;
                        false
                    }
                }),
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

    fn ident_type_pair(&mut self) -> Option<(String, Type)> {
        let t = self.next();
        let name = match t {
            Some(Token::Ident(s)) => s,
            Some(Token::Colon) => {
                self.session
                    .error(Message::MissingIdentifier, self.lex.span());
                self.token_buffer = t;
                "unknown".to_owned()
            }
            _ => {
                self.session
                    .error(Message::MalformedIdentifier, self.lex.span());
                "unknown".to_owned()
            }
        };

        match self.next() {
            Some(Token::Colon) => {}
            t => {
                self.session.error(Message::MissingType, self.lex.span());
                self.token_buffer = t;
                return None;
            }
        }

        Some((name, self.parse_type()?))
    }

    fn simpleexpr(&mut self, t: Option<Token>) -> Option<Atom> {
        Some(match t {
            Some(Token::LeftParen) => {
                let start = self.lex.span();
                let e = self.expr()?;

                match self.next() {
                    Some(Token::RightParen) => {}
                    _ => {
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
        })
    }

    fn primaryexpr(&mut self) -> Option<Atom> {
        Some(match self.next() {
            Some(Token::Let) => {
                let start = self.lex.span();
                let mut mutable = false;
                match self.next() {
                    Some(Token::Mut) => mutable = true,
                    t => self.token_buffer = t,
                }

                let mut v = Vec::with_capacity(1);
                loop {
                    let lvalue = self.primaryexpr()?;
                    let mut type_ = Type::auto();

                    match self.next() {
                        Some(Token::Colon) => {
                            type_ = self.parse_type()?;
                        }
                        t => self.token_buffer = t,
                    }

                    let rvalue = match self.next() {
                        Some(Token::BinOp(BinOp::Compound(BinOpVariant::Id))) => self.expr()?,
                        _ => {
                            self.session
                                .error(Message::InitializerRequired, self.lex.span());
                            return None;
                        }
                    };

                    v.push(Declaration {
                        lvalue,
                        rvalue,
                        t: type_,
                    });

                    match self.next() {
                        Some(Token::Comma) => {}
                        t => {
                            self.token_buffer = t;
                            break;
                        }
                    }
                }

                Atom {
                    t: Type::void(),
                    v: AtomVariant::Let(mutable, v),
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
                    v: AtomVariant::UnOp(op, Box::new(self.primaryexpr()?)),
                    span: start.to(self.lex.span()),
                    t,
                }
            }
            Some(Token::If) => {
                println!("hi");
                let start = self.lex.span();
                let cond = Box::new(self.expr()?);
                let body = Box::new(self.expr()?);
                let mut else_body = None;
                match self.next() {
                    Some(Token::Else) => {
                        else_body = Some(Box::new(self.expr()?));
                    }
                    t => self.token_buffer = t,
                }

                Atom {
                    t: Type::auto(),
                    v: AtomVariant::If(cond, body, else_body),
                    span: start.to(self.lex.span()),
                }
            }
            Some(Token::Return) => {
                let start = self.lex.span();
                let e = self.expr()?;

                Atom {
                    t: e.t,
                    v: AtomVariant::Return(Box::new(e)),
                    span: start.to(self.lex.span()),
                }
            }
            Some(Token::AmbiguousOp(o)) => {
                let start = self.lex.span();
                let e = self.primaryexpr()?;

                Atom {
                    t: e.t,
                    v: AtomVariant::UnOp(o.to_unary(), Box::new(e)),
                    span: start.to(self.lex.span()),
                }
            }
            Some(Token::UnOp(u)) => {
                let start = self.lex.span();
                let e = self.primaryexpr()?;

                Atom {
                    t: e.t,
                    v: AtomVariant::UnOp(u, Box::new(e)),
                    span: start.to(self.lex.span()),
                }
            }
            t => self.simpleexpr(t)?,
        })
    }

    fn subexpr(&mut self, mut lhs: Atom, min_prec: u8) -> Option<Atom> {
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
                let mut rhs = self.primaryexpr()?;

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
                        rhs = self.subexpr(rhs, next_prec)?;
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

    pub fn expr(&mut self) -> Option<Atom> {
        let lhs = self.primaryexpr()?;
        self.subexpr(lhs, 0)
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
                    params.push(self.ident_type_pair()?);
                    match self.next() {
                        Some(Token::Comma) => {}
                        Some(Token::RightParen) => break,
                        _ => todo!(),
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

        let atom = self.expr()?;

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
