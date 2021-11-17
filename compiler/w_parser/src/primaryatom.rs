use super::{Handler, Parser};
use w_ast::{Atom, AtomVariant, Type};
use w_errors::Message;
use w_lexer::{BinOp, BinOpVariant, Token, UnOp};

impl<'a, H, I> Parser<'a, H, I>
where
    H: Handler<LexerInput = I>,
    I: Iterator<Item = char>,
{
    fn parse_let(&mut self) -> Option<Atom> {
        let start = self.lex.span();
        let pair = self.ident_type_pair(false)?;

        let rhs = match self.next() {
            Some(Token::BinOp(BinOp::Compound(BinOpVariant::Id))) => Some(Box::new(self.atom()?)),
            t => {
                self.token_buffer = t;
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
                    t => self.token_buffer = t,
                }

                match self.next() {
                    Some(Token::BinOp(BinOp::Regular(BinOpVariant::Gt))) => {}
                    t => {
                        self.error(Message::MissingClosingAngleBracket, self.lex.span());
                        self.token_buffer = t;
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

    fn parse_if(&mut self) -> Option<Atom> {
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

        Some(Atom {
            t: Type::auto(),
            v: AtomVariant::If(cond, body, else_body),
            span: start.to(self.lex.span()),
        })
    }

    fn parse_loop(&mut self) -> Option<Atom> {
        let start = self.lex.span();
        let initial = Box::new(self.atom()?);
        let loop_body = Box::new(self.atom()?);

        Some(Atom {
            t: Type::auto(),
            v: AtomVariant::Loop(initial, loop_body),
            span: start.to(self.lex.span()),
        })
    }

    fn parse_br(&mut self) -> Option<Atom> {
        let start = self.lex.span();

        Some(Atom {
            t: Type::auto(),
            v: AtomVariant::Br(match self.next() {
                Some(Token::If) => Some(Box::new(self.atom()?)),
                t => {
                    self.token_buffer = t;
                    None
                }
            }),
            span: start.to(self.lex.span()),
        })
    }

    fn parse_ret(&mut self) -> Option<Atom> {
        let start = self.lex.span();
        let e = self.atom()?;

        Some(Atom {
            t: e.t,
            v: AtomVariant::Return(Box::new(e)),
            span: start.to(self.lex.span()),
        })
    }

    fn unop(&mut self, u: UnOp) -> Option<Atom> {
        let start = self.lex.span();
        let e = self.primaryatom()?;

        Some(Atom {
            t: e.t,
            v: AtomVariant::UnOp(u, Box::new(e)),
            span: start.to(self.lex.span()),
        })
    }

    pub fn primaryatom(&mut self) -> Option<Atom> {
        match self.next() {
            Some(Token::Let) => self.parse_let(),
            Some(Token::BinOp(BinOp::Regular(BinOpVariant::Lt))) => self.parse_cast(),
            Some(Token::If) => self.parse_if(),
            Some(Token::Loop) => self.parse_loop(),
            Some(Token::Br) => self.parse_br(),
            Some(Token::Return) => self.parse_ret(),

            // any ambiguous ops here become unary
            Some(Token::AmbiguousOp(o)) => self.unop(o.unary()),
            Some(Token::UnOp(u)) => self.unop(u),

            t => self.simpleatom(t),
        }
    }
}
