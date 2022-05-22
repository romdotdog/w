use crate::{
    spanned,
    symbol_stack::Binding,
    types::{
        expression::Expression,
        typ::{Type, U32, VOID},
    },
    Value,
};

use super::{Compiler, Fill, Handler, Next};
use w_codegen::Serializer;
use w_errors::Message;
use w_lexer::token::{BinOp, BinOpVariant, Token, UnOp};

impl<'ast, H: Handler<'ast>, S: Serializer> Compiler<'ast, H, S> {
    pub(crate) fn parse_let(&mut self) -> Value<S> {
        matches!(self.tk, Some(Token::Let));
        self.next();

        let (ident, init) = self.parse_decl().unwrap();
        match init {
            Value::Expression(Expression(x, xt)) => {
                self.flow.register_local(ident.to_owned(), xt); // TODO: &'ast str?
                self.symbols.push(ident, Binding::Type(xt));
                Value::Expression(Expression(self.module.local_tee(ident, x), xt))
            }
            Value::Constant(x) => {
                self.symbols.push(ident, Binding::Constant(x));
                Value::Constant(x)
            }
        }
    }

    fn parse_cast(&mut self) -> Value<S> {
        let start = self.start;
        assert_eq!(self.tk, Some(Token::BinOp(BinOp(false, BinOpVariant::Lt))));
        self.next();

        let t = self.parse_type().unwrap(); // TODO
        let is_reinterpret = match self.tk {
            Some(Token::BinOp(BinOp(false, BinOpVariant::Gt))) => {
                self.next();
                false
            }
            Some(Token::UnOp(UnOp::LNot)) => {
                self.next();
                match self.tk {
                    Some(Token::BinOp(BinOp(false, BinOpVariant::Gt))) => self.next(),
                    _ => self.error(Message::MissingClosingAngleBracket, self.span()),
                }

                true
            }
            _ => {
                self.error(Message::MissingClosingAngleBracket, self.span());
                false
            }
        };

        let atom = self.primaryatom(Some(t));
        //let end = atom.1.end;
        /*Some(Spanned(
            if is_reinterpret {
                Atom::Reinterpret(t, Box::new(atom))
            } else {
                Atom::Cast(t, Box::new(atom))
            },
            Span::new(start, end),
        ))*/
        todo!()
    }

    fn parse_br(&mut self) -> Value<S> {
        assert_eq!(self.tk, Some(Token::Br));
        self.next();

        // TODO: refactor double arrow check
        let ret = match self.tk {
            Some(Token::Arrow | Token::If) => None,
            _ => Some(self.atom(None)), // TODO: br contextual type
        };

        let label = match self.tk {
            Some(Token::Arrow) => {
                self.next();
                self.take(|this, t| match t {
                    Some(Token::Label(x)) => Next(Some(x)),
                    Some(Token::Ident(_)) => {
                        this.error(Message::IdentifierIsNotLabel, this.span());
                        Next(None)
                    }
                    tk => {
                        this.error(Message::MissingLabel, this.span());
                        Fill(None, tk)
                    }
                })
            }
            _ => None,
        };

        let cond = match self.tk {
            Some(Token::If) => {
                self.next();
                Some(self.atom(Some(U32)))
            }
            _ => None,
        };

        //Some(Spanned(
        //    Atom::Br { ret, label, cond },
        //    Span::new(start, end),
        //))

        todo!()
    }

    fn parse_ret(&mut self) -> Value<S> {
        debug_assert_eq!(self.tk, Some(Token::Return));
        let span = self.span();
        self.next();

        let typ = self.flow.get_return_type().unwrap();
        let r = if self.can_begin_atom() {
            let (atom, span) = spanned!(self, { self.atom(Some(typ)) });
            let Expression(atom, atom_type) = atom.compile(&mut self.module, Some(typ));
            if atom_type == typ {
                Some(atom)
            } else {
                self.error(Message::TypeMismatch, span);
                Some(self.module.unreachable())
            }
        } else if typ == VOID {
            None
        } else {
            self.error(Message::MustReturnValue, span);
            Some(self.module.unreachable())
        };

        Value::Expression(Expression(self.module.return_(r), VOID))
    }

    fn unop(&mut self, u: UnOp, contextual_type: Option<Type>) -> Value<S> {
        let start = self.start;
        self.next();

        let a = self.primaryatom(None); // TODO: contextual type

        //Some(Spanned(Atom::UnOp(u, Box::new(a)), Span::new(start, end)))
        todo!()
    }

    pub(crate) fn primaryatom(&mut self, contextual_type: Option<Type>) -> Value<S> {
        match self.tk {
            Some(Token::Let) => self.parse_let(),
            Some(Token::BinOp(BinOp(false, BinOpVariant::Lt))) => self.parse_cast(),

            Some(Token::Br) => self.parse_br(),
            Some(Token::Return) => self.parse_ret(),

            // any ambiguous ops here become unary
            Some(Token::AmbiguousOp(o)) => self.unop(o.unary(), contextual_type),
            Some(Token::UnOp(u)) => self.unop(u, contextual_type),

            _ => self.simpleatom(contextual_type),
        }
    }
}
