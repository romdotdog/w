use crate::lexer::{Lexer, Token};

mod ast;
pub use ast::Atom;

pub struct Parser<'a> {
    lex: Lexer<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(src: &'a str) -> Self {
        Parser {
            lex: Lexer::new(src),
        }
    }

    pub fn from_lexer(lex: Lexer<'a>) -> Self {
        Parser { lex }
    }

    pub fn expect(&mut self, t: Option<Token>) {
        assert_eq!(self.lex.next(), t);
    }

    fn primaryexpr(&mut self) -> Atom {
        match self.lex.next() {
            Some(Token::LeftParen) => {
                let e = self.expr();
                self.expect(Some(Token::RightParen));
                Atom::Paren(Box::new(e))
            }
            Some(Token::Float(f)) => Atom::Float(f),
            Some(Token::Integer(f)) => Atom::Integer(f),
            Some(Token::Ident(s)) => Atom::Ident(s),
            _ => todo!(),
        }
    }

    fn subexpr(&mut self, mut lhs: Atom, min_prec: u8) -> Atom {
        // https://en.wikipedia.org/wiki/Operator-precedence_parser

        let mut l = self.lex.next();
        loop {
            let mut t1_prec = 0;
            match l {
                Some(Token::Op {
                    t,
                    ref is_assignment,
                }) if t.is_binary() && {
                    // binary operator whose precedence is >= min_precedence
                    t1_prec = t.prec(*is_assignment);
                    t1_prec >= min_prec
                } =>
                {
                    let mut rhs = self.primaryexpr();

                    l = self.lex.next();
                    loop {
                        match l {
                            Some(Token::Op {
                                ref t,
                                is_assignment,
                            }) if t.is_binary() && {
                                // binary operator whose precedence is greater
                                // than op's, or a right-associative operator
                                // whose precedence is equal to op's
                                let prec = t.prec(is_assignment);
                                prec > t1_prec || is_assignment && prec == t1_prec
                            } =>
                            {
                                self.lex.insert(l);
                                rhs = self.subexpr(rhs, t1_prec + 1);
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
}
