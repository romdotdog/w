use crate::{
    spanned,
    types::{
        constant::Constant,
        itemref::ItemRef,
        typ::{Type, I32, U32, UNREACHABLE, VOID},
    },
    util::registry::Item,
    util::symbol_stack::Binding,
    Expression, Value,
};

use super::{Compiler, Handler};
use w_codegen::Serializer;
use w_errors::Message;
use w_lexer::token::{AmbiguousOp, BinOp, BinOpVariant, Token, UnOp};
use w_utils::span::Span;

impl<'ast, H: Handler<'ast>, S: Serializer> Compiler<'ast, H, S> {
    pub(crate) fn parse_block(
        &mut self,
        label: Option<&'ast str>,
        contextual_type: Option<Type>,
    ) -> Value<S> {
        let start = self.start;
        debug_assert_eq!(self.tk, Some(Token::LeftBracket));
        self.next();

        let symbol_top = self.symbols.get_top();
        let mut contents = Vec::new();
        let mut typ = VOID;

        let end = 'm: loop {
            macro_rules! panic_block {
                () => {
                    loop {
                        match self.tk {
                            Some(Token::Semicolon) => {
                                self.next();
                                continue 'm;
                            }
                            Some(Token::RightBracket) | None => {
                                let end_ = self.end;
                                self.next();
                                break 'm end_;
                            }
                            _ => self.next(),
                        }
                    }
                };
            }

            //if self.can_begin_toplevel() && !self.parse_toplevel() {
            //    panic_block!();
            //}

            if self.can_begin_toplevel() {
                self.parse_toplevel();
            }

            let atom = match self.tk {
                Some(Token::RightBracket) => {
                    typ = VOID;

                    let end_ = self.end;
                    self.next();
                    break end_;
                }
                None => {
                    let end_ = self.end;
                    self.error(Message::MissingClosingBracket, Span::new(start, end_));
                    break end_;
                }
                _ => self.atom(None),
            };

            match self.tk {
                Some(Token::RightBracket) => {
                    match atom {
                        Value::Expression(Expression(x, xt)) => {
                            contents.push(x);
                            typ = xt;
                        }
                        Value::Constant(x) => {
                            if let Some(contextual_type) = contextual_type {
                                let Expression(x, xt) =
                                    x.compile_to_type(&mut self.module, contextual_type);
                                contents.push(x);
                                typ = xt;
                            } else {
                                self.error(Message::NeedType, self.span());
                                contents.push(self.module.unreachable());
                                typ = UNREACHABLE;
                            }
                        }
                    }

                    let end_ = self.end;
                    self.next();
                    break end_;
                }
                Some(Token::Semicolon) => {
                    if let Value::Expression(Expression(x, _)) = atom {
                        contents.push(x);
                    }
                    self.next();
                }
                None => {
                    let end_ = self.end;
                    self.error(Message::MissingClosingBracket, Span::new(start, end_));
                    break end_;
                }
                _ => {
                    // try to continue
                    self.error(Message::MissingSemicolon, self.span());
                }
            }
        };

        self.symbols.free_frame(symbol_top);

        if typ.is_strict(VOID) {
            Value::Expression(Expression(self.module.block(label, &contents, None), VOID))
        } else {
            Value::Expression(Expression(
                self.module.block(label, &contents, Some(typ.resolve())),
                typ,
            ))
        }
    }

    pub(crate) fn parse_loop(
        &mut self,
        label: Option<&'ast str>,
        contextual_type: Option<Type>,
    ) -> Value<S> {
        let start = self.start;
        debug_assert_eq!(self.tk, Some(Token::Loop));
        self.next();

        let binding = match self.tk {
            Some(Token::Let) => Some(self.parse_let()),
            _ => None,
        };

        // TODO: results
        let body = match self.atom(contextual_type) {
            Value::Expression(Expression(x, _)) => x,
            Value::Constant(_) => {
                self.error(Message::UselessConstant, self.span());
                return self.unreachable();
            }
        };

        Value::Expression(Expression(self.module.loop_(label, body), VOID))
    }

    fn parse_if(&mut self, contextual_type: Option<Type>) -> Value<S> {
        debug_assert_eq!(self.tk, Some(Token::If));
        self.next();

        let (cond, cond_span) = spanned!(self, { self.atom(Some(U32)) });
        let true_branch = self.atom(contextual_type);
        let false_branch = match self.tk {
            Some(Token::Else) => {
                self.next();

                let contextual_type = contextual_type.or_else(|| {
                    if let Value::Expression(Expression(_, t)) = true_branch {
                        Some(t)
                    } else {
                        None
                    }
                });

                Some(self.atom(contextual_type))
            }
            _ => None,
        };

        let cond = match cond {
            Value::Expression(x @ Expression(_, xt)) => {
                if xt == I32 || xt == U32 {
                    x
                } else {
                    self.error(Message::TypeMismatch, cond_span);
                    self.unreachable_expr()
                }
            }
            Value::Constant(x) => {
                if let Some(b) = x.is_true() {
                    if b {
                        // TODO: fix?
                        self.error(Message::ConditionTrue, cond_span);
                        return true_branch;
                    }
                    self.error(Message::ConditionFalse, cond_span);
                    return false_branch.unwrap();
                }
                self.error(Message::TypeMismatch, cond_span);
                todo!()
            }
        };

        if let Some(false_branch) = false_branch {
            if let Some((true_branch, false_branch)) =
                true_branch.coerce(&mut self.module, false_branch)
            {
                let true_branch = true_branch.compile(&mut self.module, contextual_type);
                let false_branch = false_branch.compile(&mut self.module, contextual_type);

                Value::Expression(Expression(
                    self.module.if_(cond.0, true_branch.0, Some(false_branch.0)),
                    true_branch.1,
                ))
            } else {
                self.error(Message::BranchesSameTypes, self.span()); // TODO: fix span
                self.unreachable()
            }
        } else {
            match true_branch {
                Value::Expression(Expression(x, xt)) if xt == VOID => {
                    Value::Expression(Expression(self.module.if_(cond.0, x, None), VOID))
                }
                _ => {
                    self.error(Message::BranchReturnVoid, self.span()); // TODO: fix span
                    self.unreachable()
                }
            }
        }
    }

    pub(crate) fn field_access(&mut self, lhs: Value<S>) -> Value<S> {
        debug_assert_eq!(self.tk, Some(Token::Period));
        let period_span = self.span();
        self.next();
        let ident_span = self.span();
        if let Some(field) = self.expect_ident(&None) {
            let lhs_type = lhs.to_type();
            match lhs_type.item {
                ItemRef::Unreachable => return self.unreachable(),
                ItemRef::Void | ItemRef::HeapType(_) | ItemRef::StackType(_) => {
                    self.error(Message::InvalidAccess, period_span);
                    return self.unreachable();
                }
                ItemRef::Ref(r) => {
                    let item = self.registry.get(r).unwrap();
                    match item {
                        Item::Enum(_) | Item::Global(_, _) | Item::Fn(_, _, _) => {
                            self.error(Message::InvalidAccess, period_span);
                            self.unreachable()
                        }

                        Item::Struct(x) => {
                            if let Some(&(typ, offset)) = x.get(field) {
                                let dest_type = Type {
                                    meta: lhs_type.meta,
                                    item: typ.item,
                                };

                                let offset = Constant::i32(offset);
                                let op = BinOp(false, BinOpVariant::Add);
                                // TODO: refactor
                                match lhs {
                                    Value::Expression(x) => {
                                        let offset = offset.compile(&mut self.module);
                                        Value::Expression(Expression(
                                            x.binop(&mut self.module, offset, op).unwrap().0,
                                            dest_type,
                                        ))
                                    }
                                    Value::Constant(x) => Value::Constant(Constant {
                                        typ: dest_type,
                                        constant: x.binop(offset, op).unwrap().constant,
                                    }),
                                }
                            } else {
                                self.error(Message::InvalidField, ident_span);
                                self.unreachable()
                            }
                        }
                        Item::Union(x) => {
                            if let Some(&typ) = x.get(field) {
                                let dest_type = Type {
                                    meta: lhs_type.meta,
                                    item: typ.item,
                                };

                                match lhs {
                                    Value::Expression(x) => {
                                        Value::Expression(Expression(x.0, dest_type))
                                    }
                                    Value::Constant(x) => Value::Constant(Constant {
                                        typ: dest_type,
                                        constant: x.constant,
                                    }),
                                }
                            } else {
                                self.error(Message::InvalidField, ident_span);
                                self.unreachable()
                            }
                        }
                    }
                }
            }
        } else {
            self.unreachable()
        }
    }

    fn postfixatom(&mut self, mut lhs: Value<S>) -> Value<S> {
        loop {
            match self.tk {
                Some(Token::UnOp(UnOp::Inc | UnOp::Dec)) => {
                    self.error(Message::PostfixIncDec, self.span());
                    self.next();
                }

                Some(Token::Period) => {
                    self.next();
                    /*lhs = self.take(|self, t| match t {
                        Some(Token::Ident(s)) => Next(Some(Spanned(
                            Atom::Access(Box::new(lhs), Spanned(s, self.span())),
                            Span::new(start, self.end),
                        ))),
                        tk => {
                            // TODO: review
                            self.error(Message::MissingIdentifier, self.span());
                            Fill(None, tk)
                        }
                    })?;*/
                }

                Some(Token::LeftSqBracket) => {
                    self.next();
                    let atom = self.atom(Some(U32));

                    match self.tk {
                        Some(Token::RightSqBracket) => {
                            /*lhs = Spanned(
                                Atom::Index(Box::new(lhs), Box::new(atom)),
                                Span::new(start, self.end),
                            );*/
                            self.next();
                        }
                        _ => {
                            // TODO: review
                            self.error(Message::MissingClosingSqBracket, self.span());
                            todo!()
                        }
                    }
                }

                Some(Token::LeftParen) => {
                    self.next();
                    // typecheck lhs
                    let item = match lhs {
                        Value::Expression(Expression(_, xt)) => {
                            if let ItemRef::Ref(itemref) = xt.item {
                                if let Some(item) = self.registry.get(itemref) {
                                    item
                                } else {
                                    todo!();
                                }
                            } else {
                                self.error(Message::TypeMismatch, self.span());
                                todo!();
                            }
                        }
                        Value::Constant(_) => {
                            self.error(Message::TypeMismatch, self.span());
                            todo!();
                        }
                    };

                    if let Item::Fn(name, params, ret) = item {
                        let param_types: Vec<Type> = params.iter().map(|p| p.typ).collect();
                        let mut args = Vec::new();

                        let mut i = 0;
                        match self.tk {
                            Some(Token::RightParen) => break,
                            _ => loop {
                                let &t = param_types.get(i).unwrap_or_else(|| {
                                    self.error(Message::TooManyArgs, self.span());
                                    todo!()
                                });

                                let atom = self.atom(Some(t));
                                let Expression(atom, atom_type) =
                                    atom.compile(&mut self.module, Some(t));
                                if atom_type == t {
                                    args.push(atom);
                                } else {
                                    self.error(Message::TypeMismatch, self.span());
                                    todo!()
                                }

                                match self.tk {
                                    Some(Token::Comma) => self.next(),
                                    Some(Token::RightParen) => break,
                                    _ => {
                                        self.error(Message::MissingClosingParen, self.span());
                                        todo!()
                                    }
                                }

                                i += 1;
                            },
                        };

                        //lhs = Spanned(Atom::Call(Box::new(lhs), args), Span::new(start, self.end));
                        self.next(); // skip paren
                    } else {
                        self.error(Message::TypeMismatch, self.span());
                        todo!()
                    }
                }

                _ => break,
            }
        }

        lhs
    }

    pub(crate) fn simpleatom(&mut self, contextual_type: Option<Type>) -> Value<S> {
        let lhs: Value<S> = match self.tk {
            Some(Token::LeftParen) => {
                self.next();

                let e = self.atom(contextual_type);

                if self.tk != Some(Token::RightParen) {
                    self.error(Message::MissingClosingParen, self.span());
                }

                self.next();
                e
            }
            Some(Token::Sizeof) => {
                self.next();

                if self.tk != Some(Token::LeftParen) {
                    self.error(Message::MissingOpeningParen, self.span());
                }

                self.next();
                let t = self.parse_type().unwrap(); // TODO

                if self.tk != Some(Token::RightParen) {
                    self.error(Message::MissingClosingParen, self.span());
                }

                self.next();
                todo!()
            }
            Some(Token::If) => self.parse_if(contextual_type),
            Some(Token::LeftBracket) => self.parse_block(None, contextual_type),
            Some(Token::Loop) => self.parse_loop(None, contextual_type),
            Some(Token::Overflown) => {
                self.error(Message::LiteralTooLarge, self.span());
                return self.unreachable();
            }
            _ => {
                match self.tk {
                    Some(Token::Integer(n)) => {
                        self.next();
                        Value::Constant(Constant::i64(n))
                    }
                    Some(Token::Uinteger(n)) => {
                        self.next();
                        Value::Constant(Constant::u64(n))
                    }
                    Some(Token::Float(n)) => {
                        self.next();
                        Value::Constant(Constant::f64(n))
                    }
                    Some(Token::String(s)) => todo!(),
                    Some(Token::Char(s)) => todo!(),
                    Some(Token::Ident(s)) => {
                        let span = self.span();
                        self.next(); // fill
                        if let Some(Token::Colon) = self.tk {
                            self.error(Message::IdentifierIsNotLabel, span);
                            // TODO: not label behavior
                        };
                        self.resolve_ident(s, span)
                    }
                    Some(Token::Label(x)) => {
                        self.next();
                        match self.tk {
                            Some(Token::Colon) => self.next(),
                            _ => self.error(Message::MissingColon, self.span()),
                        };

                        match self.tk {
                            Some(Token::LeftBracket) => self.parse_block(Some(x), contextual_type),
                            Some(Token::Loop) => self.parse_loop(Some(x), contextual_type),
                            _ => {
                                // TODO: parse atom?
                                self.error(Message::CannotFollowLabel, self.span());
                                self.unreachable()
                            }
                        }
                    }
                    _ => {
                        self.error(Message::UnexpectedToken, self.span());
                        self.unreachable()
                    }
                }
            }
        };

        self.postfixatom(lhs)
    }

    pub(crate) fn resolve_ident(&mut self, s: &'ast str, span: Span) -> Value<S> {
        if let Some(binding) = self.symbols.find(s) {
            match binding {
                Binding::Type(t) => Value::Expression(Expression(self.module.local_get(s), t)),
                Binding::Constant(c) => Value::Constant(c),
            }
        } else {
            self.error(Message::UnresolvedIdentifier, span);
            self.unreachable()
        }
    }

    pub(crate) fn lvalue(&mut self, contextual_type: Option<Type>) -> LValue<S> {
        match self.tk {
            Some(Token::AmbiguousOp(AmbiguousOp::Asterisk)) => {
                self.next();
                LValue::Pointer(self.primaryatom(None))
            }
            Some(Token::Ident(s)) => {
                let span = self.span();
                self.next();
                match self.tk {
                    Some(Token::Period | Token::LeftSqBracket) => {
                        let mut lhs_ptr = None;
                        loop {
                            let lhs = match lhs_ptr {
                                Some(v) => self.deref(match v {
                                    Value::Expression(x) => x,
                                    Value::Constant(c) => todo!(),
                                }),
                                None => self.resolve_ident(s, span),
                            };

                            lhs_ptr = Some(match self.tk {
                                Some(Token::Period) => self.field_access(lhs),
                                Some(Token::LeftSqBracket) => todo!(),
                                _ => break LValue::Pointer(lhs),
                            });
                        }
                    }
                    _ => LValue::Ident(s),
                }
            }
            _ => LValue::RValue(self.primaryatom(contextual_type)),
        }
    }
}
pub(crate) enum LValue<'ast, S: Serializer> {
    Pointer(Value<S>),
    Ident(&'ast str),
    RValue(Value<S>),
}
