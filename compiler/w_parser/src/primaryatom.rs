use super::{Fill, Handler, Next, Parser};
use w_ast::{Atom, Span, Spanned};
use w_errors::Message;
use w_lexer::token::{BinOp, BinOpVariant, Token, UnOp};

impl<'ast, H: Handler<'ast>> Parser<'ast, H> {
    pub(crate) fn parse_let(&mut self) -> Option<Spanned<Atom<'ast>>> {
        let start = self.start;
        assert_eq!(self.tk, Some(Token::Let));
        self.next();

        let pair = self.ident_type_pair(false)?;
        let mut end = pair.1.end;
        let rhs = match self.tk {
            Some(Token::BinOp(BinOp::Compound(BinOpVariant::Id))) => {
                self.next();
                let atom = self.atom()?;
                end = atom.1.end;
                Some(Box::new(atom))
            }
            _ => None,
        };

        Some(Spanned(Atom::Let(pair, rhs), Span::new(start, end)))
    }

    fn parse_cast(&mut self) -> Option<Spanned<Atom<'ast>>> {
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

        let atom = self.primaryatom()?;
        let end = atom.1.end;
        Some(Spanned(
            match is_reinterpret {
                true => Atom::Reinterpret(t, Box::new(atom)),
                false => Atom::Cast(t, Box::new(atom)),
            },
            Span::new(start, end),
        ))
    }

    fn parse_br(&mut self) -> Option<Spanned<Atom<'ast>>> {
        let start = self.start;
        assert_eq!(self.tk, Some(Token::Br));
        let mut end = self.end;
        self.next();

        // TODO: refactor double arrow check
        let ret = match self.tk {
            Some(Token::Arrow | Token::If) => None,
            _ => {
                let a = self.atom()?;
                end = a.1.end;
                Some(Box::new(a))
            }
        };

        let label = match self.tk {
            Some(Token::Arrow) => {
                self.next();
                Some(self.take(|this, t| match t {
                    Some(Token::Label(x)) => {
                        end = this.end;
                        let span = this.span();
                        Next(Spanned(x, span))
                    }
                    Some(Token::Ident(_)) => {
                        end = this.end;
                        let span = this.span();
                        this.error(Message::IdentifierIsNotLabel, span);
                        Next(Spanned("<unknown>", span))
                    }
                    tk => {
                        // br -> if ...
                        //      ^
                        let span = this.span();
                        this.error(Message::MissingLabel, span);
                        Fill(Spanned("<unknown>", span), tk)
                    }
                }))
            }
            _ => None,
        };

        let cond = match self.tk {
            Some(Token::If) => {
                self.next();
                let atom = self.atom()?;
                end = atom.1.end;
                Some(Box::new(atom))
            }
            _ => None,
        };

        Some(Spanned(
            Atom::Br { ret, label, cond },
            Span::new(start, end),
        ))
    }

    fn parse_ret(&mut self) -> Option<Spanned<Atom<'ast>>> {
        let start = self.start;
        let mut end = self.end;
        debug_assert_eq!(self.tk, Some(Token::Return));
        self.next();

        let a = if self.can_begin_atom() {
            let a_ = self.atom()?;
            end = a_.1.end;
            Some(Box::new(a_))
        } else {
            None
        };

        Some(Spanned(Atom::Return(a), Span::new(start, end)))
    }

    fn unop(&mut self, u: UnOp) -> Option<Spanned<Atom<'ast>>> {
        let start = self.start;
        self.next();

        let a = self.primaryatom()?;
        let end = a.1.end;
        Some(Spanned(Atom::UnOp(u, Box::new(a)), Span::new(start, end)))
    }

    pub(crate) fn primaryatom(&mut self) -> Option<Spanned<Atom<'ast>>> {
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
