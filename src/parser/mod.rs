/*
    TODO:
    * Refactor right associativity
*/

use crate::{
    diag::{Diagnostic, Lexeme, Message},
    lexer::{Lexer, Op, Token},
    parser::ast::Type,
    Session,
};

mod ast;
pub use ast::Atom;
use ast::TopLevel;

use self::ast::{AtomVariant, TypeVariant};

pub struct Parser<'a> {
    session: &'a Session,
    lex: Lexer<'a>,
}

macro_rules! expect_or_error {
    ($parser: ident, |$t: ident| $error: block, $( $pattern:pat )|+ $( if $guard: expr )? => $r: ident) => {{
		let mut errored = false;
		loop {
			let $t = $parser.lex.next();
			match $t {
				$( $pattern )|+ $( if $guard )? => break $r,
				None => todo!(),
				_ if errored => {}
				$t => {
					$parser.session.error(Diagnostic::new($parser.lex.span(), $error));
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
    pub fn new(session: &'a Session, src: &'a str) -> Self {
        Parser {
            session,
            lex: Lexer::new(session, src),
        }
    }

    pub fn expect(&mut self, t: Option<Token>) {
        assert_eq!(self.lex.next(), t);
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

            last = Some(self.expr());
            match self.lex.next() {
                Some(Token::Semicolon) => continue,
                _ => {
                    expect_or_error!(self, RightBracket);
                    break;
                }
            }
        }

        let span = start.to(self.lex.span());
        let last = last
            .unwrap_or_else(|| Atom::new(AtomVariant::Null, span, Type::new(TypeVariant::Null, 0)));
        let t = last.t;
        Atom::new(AtomVariant::Block(r, Box::new(last)), span, t)
    }

    fn recover(&mut self) -> Atom {
        let start = self.lex.span();
        loop {
            match self.lex.next() {
                Some(Token::Semicolon) | None => {
                    break Atom::new(
                        AtomVariant::Null,
                        start.to(self.lex.span()),
                        Type::new(TypeVariant::Null, 0),
                    )
                }
                _ => {}
            }
        }
    }

    fn parse_type(&mut self) -> Type {
        let mut indir: u8 = 0;
        loop {
            match self.lex.next() {
                Some(Token::Op {
                    t: Op::And,
                    is_assignment: false,
                }) => {
                    indir = indir.checked_add(1).unwrap_or_else(|| todo!());
                }
                Some(Token::Ident(s)) => {
                    return Type::new(s.into(), indir);
                }
                _ => todo!(),
            }
        }
    }

    fn recover_fn(&mut self) -> Atom {
        let start = self.lex.span();
        loop {
            match self.lex.next() {
                Some(Token::Fn) | None => {
                    break Atom::new(
                        AtomVariant::Null,
                        start.to(self.lex.span()),
                        Type::new(TypeVariant::Null, 0),
                    )
                }
                _ => {}
            }
        }
    }

    fn primaryexpr(&mut self) -> Atom {
        match self.lex.next() {
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
            Some(Token::LeftBracket) => self.block(),
            Some(Token::Op { t: Op::Lt, .. }) => {
                let start = self.lex.span();
                let t = self.parse_type();
                let mut op = Op::Cast;

                match self.lex.next() {
                    Some(Token::Op { t: Op::Gt, .. }) => {}
                    t1 => {
                        if t1 == Some(Token::Exclamation) {
                            op = Op::Reinterpret;
                        }

                        expect_or_error!(
                            self,
                            |t| { Message::ExpectedGot(Lexeme::RightAngBracket, t.into()) },
                            Some(Token::Op { t: Op::Gt, .. }) => t
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
                match self.lex.next() {
                    Some(Token::Else) => {
                        else_body = Some(Box::new(self.expr()));
                    }
                    t => self.lex.insert(t),
                }

                // TODO: body return checking
                Atom::new(
                    AtomVariant::If(cond, body, else_body),
                    start.to(self.lex.span()),
                    Type::new(TypeVariant::Null, 0),
                )
            }
            _ => self.recover(),
        }
    }

    fn subexpr(&mut self, mut lhs: Atom, min_prec: u8) -> Atom {
        // https://en.wikipedia.org/wiki/Operator-precedence_parser

        let mut l = self.lex.next();
        loop {
            let mut t1_prec = 0;
            match l {
                Some(Token::Op { t, is_assignment })
                    if t.is_binary() && {
                        // from the article:
                        // binary operator whose precedence is >= min_precedence
                        t1_prec = t.prec(is_assignment);
                        t1_prec >= min_prec
                    } =>
                {
                    let mut rhs = self.primaryexpr();

                    l = self.lex.next();
                    loop {
                        // what's this variable?
                        // Wikipedia's pseudocode is wrong. if t1 is a right associative op
                        // with equal precedence, then trying to match t2 with a higher
                        // min_prec is impossible
                        let mut right_associative = false;
                        match l {
                            Some(Token::Op {
                                ref t,
                                is_assignment,
                            }) if t.is_binary() && {
                                // from the article:
                                // binary operator whose precedence is greater than op's
                                // or a right-associative operator whose precedence
                                // is equal to op's
                                let prec = t.prec(is_assignment);
                                prec > t1_prec || {
                                    right_associative = is_assignment && prec == t1_prec;
                                    right_associative
                                }
                            } =>
                            {
                                self.lex.insert(l);
                                rhs = self.subexpr(
                                    rhs,
                                    if right_associative {
                                        t1_prec
                                    } else {
                                        t1_prec + 1
                                    },
                                );
                                l = self.lex.next()
                            }
                            _ => break,
                        }
                    }

                    let span = lhs.span.to(rhs.span);
                    lhs = Atom::new(
                        AtomVariant::BinOp(Box::new(lhs), t, Box::new(rhs)),
                        span,
                        Type::auto(),
                    );
                }
                _ => {
                    self.lex.insert(l);
                    break;
                }
            }
        }
        lhs
    }

    pub fn expr(&mut self) -> Atom {
        let lhs = self.primaryexpr();
        self.subexpr(lhs, 0)
    }

    pub fn parse(mut self) -> Vec<TopLevel> {
        let mut r = Vec::new();
        while let Some(t) = self.lex.next() {
            match t {
                Token::Fn => {
                    let name = expect_or_error!(
                        self,
                        |t| { Message::ExpectedGot(Lexeme::Ident, t.into()) },
                        Some(Token::Ident(r)) => r
                    );

                    expect_or_error!(self, LeftParen);
                    expect_or_error!(self, RightParen);
                    expect_or_error!(self, Colon);

                    let t = match self.lex.next() {
                        Some(Token::Ident(s)) => s.into(),
                        t => {
                            self.session.error(Diagnostic::new(
                                self.lex.span(),
                                Message::ExpectedGot(Lexeme::Type, t.into()),
                            ));

                            self.recover_fn();
                            continue;
                        }
                    };

                    let e = self.expr();
                    r.push(TopLevel::Fn(name, e, t))
                }
                _ => todo!(),
            }
        }
        r
    }
}
