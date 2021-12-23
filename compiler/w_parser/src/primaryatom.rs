use super::{Handler, Parser};
use w_ast::{Atom, Spanned};
use w_errors::Message;
use w_lexer::{BinOp, BinOpVariant, Span, Token, UnOp};

impl<'a, H, I> Parser<'a, H, I>
where
    H: Handler<LexerInput = I>,
    I: Iterator<Item = char>,
{
    pub(crate) fn parse_let(&mut self) -> Option<Spanned<Atom>> {
        let start = self.start;
        assert_eq!(self.tk, Some(Token::Let));
        self.next();

        let pair = self.ident_type_pair(false)?;
        let rhs = match self.tk {
            Some(Token::BinOp(BinOp::Compound(BinOpVariant::Id))) => {
                self.next();
                Some(Box::new(self.atom()?))
            }
            _ => None,
        };

        Some(Spanned(Atom::Let(pair, rhs), Span::new(start, self.end)))
    }

    fn parse_cast(&mut self) -> Option<Spanned<Atom>> {
        let start = self.start;
        assert_eq!(
            self.tk,
            Some(Token::BinOp(BinOp::Regular(BinOpVariant::Lt)))
        );
        self.next();

        let t = self.parse_type()?;
        let is_reinterpret = match self.tk {
            Some(Token::BinOp(BinOp::Regular(BinOpVariant::Gt))) => {
                self.next();
                false
            }
            Some(Token::UnOp(UnOp::LNot)) => {
                self.next();
                match self.tk {
                    Some(Token::BinOp(BinOp::Regular(BinOpVariant::Gt))) => self.next(),
                    _ => self.error(Message::MissingClosingAngleBracket, self.span()),
                }

                true
            }
            _ => {
                self.error(Message::MissingClosingAngleBracket, self.span());
                false
            }
        };

        Some(Spanned(
            Atom::Cast(t, Box::new(self.primaryatom()?), is_reinterpret),
            Span::new(start, self.end),
        ))
    }

    fn parse_br(&mut self) -> Option<Spanned<Atom>> {
        let start = self.start;
        assert_eq!(self.tk, Some(Token::Br));
        self.next();

        // TODO: refactor double arrow check
        let ret = match self.tk {
            Some(Token::Arrow) => None,
            _ => Some(Box::new(self.atom()?)),
        };

        match self.tk {
            Some(Token::Arrow) => self.next(),
            _ => {
                // TODO: fix error
                self.error(Message::MissingLabel, self.span());
            }
        };

        let label = match self.take() {
            Some(Token::Label(x)) => {
                let span = self.span();
                self.next(); // fill
                Spanned(x, span)
            }
            Some(Token::Ident(x)) => {
                let span = self.span();
                self.error(Message::IdentifierIsNotLabel, span);
                self.next(); // fill
                Spanned(format!("${}", x), span)
            }
            tk => {
                // br -> if ...
                //      ^
                self.fill(tk);
                let span = self.span();
                self.error(Message::MissingLabel, span);
                Spanned("<unknown>".to_owned(), span)
            }
        };

        let cond = match self.tk {
            Some(Token::If) => {
                self.next();
                Some(Box::new(self.atom()?))
            }
            _ => None,
        };

        Some(Spanned(
            Atom::Br(ret, label, cond),
            Span::new(start, self.end),
        ))
    }

    fn parse_ret(&mut self) -> Option<Spanned<Atom>> {
        // TODO: return without atom
        let start = self.start;
        debug_assert_eq!(self.tk, Some(Token::Return));
        self.next();

        let e = self.atom()?;

        Some(Spanned(
            Atom::Return(Box::new(e)),
            Span::new(start, self.end),
        ))
    }

    fn unop(&mut self, u: UnOp) -> Option<Spanned<Atom>> {
        let start = self.start;
        self.next();

        let e = self.primaryatom()?;

        Some(Spanned(
            Atom::UnOp(u, Box::new(e)),
            Span::new(start, self.end),
        ))
    }

    pub(crate) fn primaryatom(&mut self) -> Option<Spanned<Atom>> {
        match self.tk {
            Some(Token::Let) => self.parse_let(),
            Some(Token::BinOp(BinOp::Regular(BinOpVariant::Lt))) => self.parse_cast(),

            Some(Token::Br) => self.parse_br(),
            Some(Token::Return) => self.parse_ret(),

            // any ambiguous ops here become unary
            Some(Token::AmbiguousOp(o)) => self.unop(o.unary()),
            Some(Token::UnOp(u)) => self.unop(u),

            _ => self.simpleatom(),
        }
    }
}
