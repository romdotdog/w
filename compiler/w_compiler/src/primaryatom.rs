use crate::{
    spanned,
    types::{
        expression::Expression,
        typ::{Type, U32, VOID},
    },
    util::symbol_stack::Binding,
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

        let ident_span = self.span();
        let (ident, init) = self.parse_decl().unwrap();
        match init {
            Value::Expression(Expression(x, xt)) => {
                self.flow.register_local(ident.to_owned(), xt); // TODO: &'ast str?
                if !self.symbols.push(ident, Binding::Type(xt)) {
                    self.error(Message::AlreadyDefined, ident_span);
                }
                Value::Expression(Expression(self.module.local_tee(ident, x), xt))
            }
            Value::Constant(x) => {
                if !self.symbols.push(ident, Binding::Constant(x)) {
                    self.error(Message::AlreadyDefined, ident_span);
                }
                Value::Constant(x)
            }
        }
    }

    fn parse_cast(&mut self) -> Value<S> {
        assert_eq!(self.tk, Some(Token::BinOp(BinOp(false, BinOpVariant::Lt))));

        let ((to, is_reinterpret), span) = spanned!(self, {
            self.next();

            let to = self.parse_type().unwrap(); // TODO
            let is_reinterpret = match self.tk {
                Some(Token::UnOp(UnOp::LNot)) => {
                    self.next();
                    true
                }
                _ => false,
            };

            match self.tk {
                Some(Token::BinOp(BinOp(false, BinOpVariant::Gt))) => self.next(),
                _ => self.error(Message::MissingClosingAngleBracket, self.span()),
            };

            (to, is_reinterpret)
        });

        let atom = self.primaryatom(Some(to));
        if is_reinterpret {
            todo!()
        } else {
            let r = match atom {
                Value::Expression(x) => x.cast(&mut self.module, to).map(Value::Expression),
                Value::Constant(x) => x.cast(to).map(Value::Constant),
            };

            if let Some(r) = r {
                r
            } else {
                println!("{:?}", span);
                self.error(Message::ReinterpretNeeded, span);
                self.unreachable()
            }
        }
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
