use super::{Handler, Parser};
use w_ast::{Atom, AtomVariant, Type};
use w_errors::Message;
use w_lexer::{BinOp, BinOpVariant, Token, UnOp};

impl<'a, H, I> Parser<'a, H, I>
where
    H: Handler<LexerInput = I>,
    I: Iterator<Item = char>,
{
    pub(crate) fn parse_let(&mut self) -> Option<Atom> {
        let start = self.lex.span();
        let pair = self.ident_type_pair(false)?;

        let rhs = match self.next() {
            Some(Token::BinOp(BinOp::Compound(BinOpVariant::Id))) => Some(Box::new(self.atom()?)),
            t => {
                self.backtrack(t);
                None
            }
        };

        Some(Atom {
            t: Type::void(),
            v: AtomVariant::Let(pair, rhs),
            span: start.to(self.lex.span()),
        })
    }

    fn parse_cast(&mut self) -> Option<Atom> {
        let start = self.lex.span();
        let t = self.parse_type()?;
        let mut op = UnOp::Cast;

        match self.next() {
            Some(Token::BinOp(BinOp::Regular(BinOpVariant::Gt))) => {}
            t1 => {
                match t1 {
                    Some(Token::UnOp(UnOp::LNot)) => op = UnOp::Reinterpret,
                    t => self.backtrack(t),
                }

                match self.next() {
                    Some(Token::BinOp(BinOp::Regular(BinOpVariant::Gt))) => {}
                    t => {
                        self.error(Message::MissingClosingAngleBracket, self.lex.span());
                        self.backtrack(t);
                        return None;
                    }
                }
            }
        }

        Some(Atom {
            v: AtomVariant::UnOp(op, Box::new(self.primaryatom()?)),
            span: start.to(self.lex.span()),
            t,
        })
    }

    fn parse_br(&mut self) -> Option<Atom> {
        let start = self.lex.span();

        let mut t = self.next();
        let ret = if t == Some(Token::Arrow) {
            None
        } else {
            t = self.next();
            Some(Box::new(self.atom()?))
        };

        let label = if t == Some(Token::Arrow) {
            t = self.next();
            match t {
                Some(Token::Label(x)) => {
                    t = self.next();
                    x
                }
                Some(Token::Ident(x)) => {
                    self.error(Message::IdentifierIsNotLabel, self.lex.span());
                    t = self.next();
                    format!("${}", x)
                }
                _ => {
                    self.error(Message::MissingLabel, self.lex.span());
                    "<unknown>".to_owned()
                }
            }
        } else {
            self.error(Message::MissingLabel, self.lex.span());
            "<unknown>".to_owned()
        };

        let cond = if t == Some(Token::If) {
            Some(Box::new(self.atom()?))
        } else {
            self.backtrack(t);
            None
        };

        Some(Atom {
            t: Type::auto(),
            v: AtomVariant::Br(ret, label, cond),
            span: start.to(self.lex.span()),
        })
    }

    fn parse_ret(&mut self) -> Option<Atom> {
        let start = self.lex.span();
        let e = self.atom()?;

        Some(Atom {
            t: Type::auto(),
            v: AtomVariant::Return(Box::new(e)),
            span: start.to(self.lex.span()),
        })
    }

    fn unop(&mut self, u: UnOp) -> Option<Atom> {
        let start = self.lex.span();
        let e = self.primaryatom()?;

        Some(Atom {
            t: Type::auto(),
            v: AtomVariant::UnOp(u, Box::new(e)),
            span: start.to(self.lex.span()),
        })
    }

    pub(crate) fn primaryatom(&mut self) -> Option<Atom> {
        match self.next() {
            Some(Token::Let) => self.parse_let(),
            Some(Token::BinOp(BinOp::Regular(BinOpVariant::Lt))) => self.parse_cast(),

            Some(Token::Br) => self.parse_br(),
            Some(Token::Return) => self.parse_ret(),

            // any ambiguous ops here become unary
            Some(Token::AmbiguousOp(o)) => self.unop(o.unary()),
            Some(Token::UnOp(u)) => self.unop(u),

            t => self.simpleatom(t),
        }
    }
}
