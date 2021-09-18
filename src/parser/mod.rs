/*
    TODO:
    * Refactor right associativity
*/

use crate::{
    diag::{Diagnostic, Lexeme, Message},
    lexer::{Lexer, Token},
    Session,
};

mod ast;
pub use ast::Atom;
use ast::TopLevel;

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
        let mut last = Atom::Null;

        loop {
            match last {
                Atom::Null => {}
                t => r.push(t),
            }

            last = self.expr();
            match self.lex.next() {
                Some(Token::Semicolon) => continue,
                _ => {
                    expect_or_error!(self, RightBracket);
                    break;
                }
            }
        }

        Atom::Block(r, Box::new(last))
    }

    fn primaryexpr(&mut self) -> Atom {
        match self.lex.next() {
            Some(Token::LeftParen) => {
                let e = self.expr();
                expect_or_error!(self, RightParen);
                Atom::Paren(Box::new(e))
            }
            Some(Token::Float(f)) => Atom::Float(f),
            Some(Token::Integer(f)) => Atom::Integer(f),
            Some(Token::Ident(s)) => Atom::Ident(s),
            Some(Token::LeftBracket) => self.block(),
            _ => todo!(),
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
                                println!("{:?}", t);
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

                    lhs = Atom::BinOp(Box::new(lhs), t, Box::new(rhs));
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

                    let e = self.expr();
                    r.push(TopLevel::Fn(name, e))
                }
                _ => todo!(),
            }
        }
        r
    }
}
