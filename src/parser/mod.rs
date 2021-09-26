/*
    TODO:
    * Diagnostics
    * Fill all `todo!()`s
*/

use crate::{
    diag::{Diagnostic, Lexeme, Message},
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

macro_rules! expect_or_error {
    ($parser: ident, |$t: ident| $error: block, $( $pattern:pat )|+ $( if $guard: expr )? => $r: ident) => {{
		let mut errored = false;
		let start = $parser.lex.span();
		loop {
			let $t = $parser.next();
			match $t {
				$( $pattern )|+ $( if $guard )? => break $r,
				None => todo!(),
				_ if errored => {}
				$t => {
					$parser.session.error(Diagnostic::new(start.to($parser.lex.span()), $error));
					errored = true;
				}
			}
		}
    }};

	($parser: ident, $t: ident) => {
		expect_or_error!(
			$parser,
			|t| { Message::ExpectedGot(Lexeme::$t, t.into()) },
			Some(Token::$t) => t
		);
	}
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

    pub fn expect(&mut self, t: Option<Token>) {
        assert_eq!(self.next(), t);
    }

    pub fn block(&mut self) -> Atom {
        let mut r = Vec::new();
        let start = self.lex.span();

        let mut last = None;

        loop {
            match last {
                Some(t) => r.push(t),
                None => {}
            }

            match self.next() {
                Some(Token::RightBracket) => {
                    last = None;
                    break;
                }
                t => {
                    self.token_buffer = t;
                    last = Some(self.expr());
                }
            }

            match self.next() {
                Some(Token::Semicolon) => continue,
                Some(Token::RightBracket) => break,
                t => {
                    expect_or_error!(
                        self,
                        |t| { Message::MissingSemicolon },
                        Some(Token::RightBracket) => t
                    );
                    break;
                }
            }
        }

        let span = start.to(self.lex.span());
        let last = last.unwrap_or_else(|| Atom::new(AtomVariant::Null, span, Type::void()));
        let t = last.t;
        Atom::new(AtomVariant::Block(r, Box::new(last)), span, t)
    }

    fn recover(&mut self) -> Atom {
        let start = self.lex.span();
        loop {
            match self.next() {
                Some(Token::Semicolon) | None => {
                    break Atom::new(AtomVariant::Null, start.to(self.lex.span()), Type::void())
                }
                _ => {}
            }
        }
    }

    fn parse_type(&mut self) -> Type {
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
                    return Type::with_indir(s.into(), indir);
                }
                _ => todo!(),
            }
        }
    }

    fn ident_type_pair(&mut self) -> (String, Type) {
        let name = expect_or_error!(
            self,
            |t| { Message::ExpectedGot(Lexeme::Ident, t.into()) },
            Some(Token::Ident(r)) => r
        );

        expect_or_error!(self, Colon);

        (name, self.parse_type())
    }

    fn recover_fn(&mut self) -> Atom {
        let start = self.lex.span();
        loop {
            match self.next() {
                Some(Token::Fn) | None => {
                    break Atom::new(AtomVariant::Null, start.to(self.lex.span()), Type::void())
                }
                _ => {}
            }
        }
    }

    fn primaryexpr(&mut self) -> Atom {
        match self.next() {
            Some(Token::LeftParen) => {
                let start = self.lex.span();
                let e = self.expr();
                let t = e.t;
                expect_or_error!(self, RightParen);

                Atom::new(
                    AtomVariant::Paren(Box::new(e)),
                    start.to(self.lex.span()),
                    t,
                )
            }
            Some(Token::Float(f)) => {
                Atom::new(AtomVariant::Float(f), self.lex.span(), Type::auto())
            }
            Some(Token::Integer(f)) => {
                Atom::new(AtomVariant::Integer(f), self.lex.span(), Type::auto())
            }
            Some(Token::Ident(s)) => {
                Atom::new(AtomVariant::Ident(s), self.lex.span(), Type::auto())
            }
            Some(Token::String(s)) => {
                Atom::new(AtomVariant::String(s), self.lex.span(), Type::auto())
            }
            Some(Token::Char(s)) => Atom::new(AtomVariant::Char(s), self.lex.span(), Type::auto()),
            Some(Token::Let) => {
                let start = self.lex.span();
                let mut mutable = false;
                match self.next() {
                    Some(Token::Mut) => mutable = true,
                    t => self.token_buffer = t,
                }

                let mut v = Vec::with_capacity(1);
                loop {
                    let lvalue = self.primaryexpr();
                    let mut type_ = Type::auto();

                    match self.next() {
                        Some(Token::Colon) => {
                            type_ = self.parse_type();
                        }
                        t => self.token_buffer = t,
                    }

                    let rvalue = match self.next() {
                        Some(Token::BinOp(BinOp::Compound(BinOpVariant::Id))) => self.expr(),
                        _ => todo!(),
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

                Atom::new(
                    AtomVariant::Let(mutable, v),
                    start.to(self.lex.span()),
                    Type::void(),
                )
            }
            Some(Token::LeftBracket) => self.block(),
            Some(Token::BinOp(BinOp::Regular(BinOpVariant::Lt))) => {
                let start = self.lex.span();
                let t = self.parse_type();
                let mut op = UnOp::Cast;

                match self.next() {
                    Some(Token::BinOp(BinOp::Regular(BinOpVariant::Gt))) => {}
                    t1 => {
                        // !x
                        match t1 {
                            Some(Token::UnOp(UnOp::LNot)) => op = UnOp::Reinterpret,
                            _ => {}
                        }

                        expect_or_error!(
                            self,
                            |t| { Message::ExpectedGot(Lexeme::RightAngBracket, t.into()) },
                            Some(Token::BinOp(BinOp::Regular(BinOpVariant::Gt))) => t
                        );
                    }
                }

                Atom::new(
                    AtomVariant::UnOp(op, Box::new(self.primaryexpr())),
                    start.to(self.lex.span()),
                    t,
                )
            }
            Some(Token::If) => {
                let start = self.lex.span();
                let cond = Box::new(self.expr());
                let body = Box::new(self.expr());
                let mut else_body = None;
                match self.next() {
                    Some(Token::Else) => {
                        else_body = Some(Box::new(self.expr()));
                    }
                    t => self.token_buffer = t,
                }

                // TODO: body return checking
                Atom::new(
                    AtomVariant::If(cond, body, else_body),
                    start.to(self.lex.span()),
                    Type::void(),
                )
            }
            Some(Token::Return) => {
                let start = self.lex.span();
                let e = self.expr();
                let t = e.t;

                Atom::new(
                    AtomVariant::Return(Box::new(e)),
                    start.to(self.lex.span()),
                    t,
                )
            }
            Some(Token::AmbiguousOp(AmbiguousOp::Minus)) => {
                let start = self.lex.span();
                let e = self.expr();
                let t = e.t;

                Atom::new(
                    AtomVariant::UnOp(UnOp::Minus, Box::new(e)),
                    start.to(self.lex.span()),
                    t,
                )
            }
            t => panic!("{:?}", t),
        }
    }

    fn subexpr(&mut self, mut lhs: Atom, min_prec: u8) -> Atom {
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
                let mut rhs = self.primaryexpr();

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
                        rhs = self.subexpr(rhs, next_prec);
                        l = self.next()
                    } else {
                        break;
                    }
                }

                let span = lhs.span.to(rhs.span);
                lhs = Atom::new(
                    AtomVariant::BinOp(Box::new(lhs), t, Box::new(rhs)),
                    span,
                    Type::auto(),
                );
            } else {
                self.token_buffer = l;
                break;
            }
        }
        lhs
    }

    pub fn expr(&mut self) -> Atom {
        let lhs = self.primaryexpr();
        self.subexpr(lhs, 0)
    }

    pub fn parse(mut self) -> Program {
        let mut fns = Vec::new();
        while let Some(t) = self.next() {
            match t {
                Token::Fn => {
                    let name = expect_or_error!(
                        self,
                        |t| { Message::ExpectedGot(Lexeme::Ident, t.into()) },
                        Some(Token::Ident(r)) => r
                    );

                    expect_or_error!(self, LeftParen);
                    let mut params = Vec::new();

                    match self.next() {
                        Some(Token::RightParen) => {}
                        t => {
                            self.token_buffer = t;
                            // TODO: remove

                            loop {
                                params.push(self.ident_type_pair());
                                match self.next() {
                                    Some(Token::Comma) => {}
                                    Some(Token::RightParen) => break,
                                    _ => todo!(),
                                }
                            }
                        }
                    }

                    expect_or_error!(self, Colon);

                    let t = self.parse_type();
                    let atom = self.expr();

                    fns.push(WFn {
                        name,
                        params,
                        atom,
                        t,
                    })
                }
                _ => todo!(),
            }
        }

        Program { fns }
    }
}
