use crate::{
    registry::Item,
    spanned,
    symbol_stack::Binding,
    types::{
        constant::Constant,
        itemref::ItemRef,
        typ::{Type, I32, U32, UNREACHABLE, VOID},
    },
    Expression, Value,
};

use super::{Compiler, Fill, Handler, Next, NoFill};
use w_codegen::Serializer;
use w_errors::Message;
use w_lexer::token::{Token, UnOp};
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
                                    x.compile(&mut self.module, Some(contextual_type));
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
                    self.next()
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
                if xt == I32 {
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
                self.error(Message::IfCannotReturn, self.span());
                return self.unreachable();
            }
        } else {
            match true_branch {
                Value::Expression(Expression(x, xt)) if xt == VOID => {
                    Value::Expression(Expression(self.module.if_(cond.0, x, None), VOID))
                }
                _ => {
                    self.error(Message::IfCannotReturn, self.span());
                    return self.unreachable();
                }
            }
        }
    }

    fn postfixatom(&mut self, mut lhs: Value<S>) -> Value<S> {
        loop {
            match self.tk {
                Some(Token::UnOp(UnOp::Dec)) => {
                    //lhs = Spanned(
                    //    Atom::PostIncDec(Box::new(lhs), IncDec::Dec),
                    //    Span::new(start, self.end),
                    //);
                    self.next();
                }

                Some(Token::UnOp(UnOp::Inc)) => {
                    //lhs = Spanned(
                    //    Atom::PostIncDec(Box::new(lhs), IncDec::Inc),
                    //    Span::new(start, self.end),
                    //);
                    self.next();
                }

                Some(Token::Period) => {
                    self.next();
                    /*lhs = self.take(|this, t| match t {
                        Some(Token::Ident(s)) => Next(Some(Spanned(
                            Atom::Access(Box::new(lhs), Spanned(s, this.span())),
                            Span::new(start, this.end),
                        ))),
                        tk => {
                            // TODO: review
                            this.error(Message::MissingIdentifier, this.span());
                            Fill(None, tk)
                        }
                    })?;*/
                }

                // TODO: figure out syntax
                Some(Token::Arrow) => {
                    self.next();
                    /*lhs = self.take(|this, t| match t {
                        Some(Token::Ident(s)) => Next(Some(Spanned(
                            Atom::Offsetof(Box::new(lhs), Spanned(s, this.span())),
                            Span::new(start, this.end),
                        ))),
                        tk => {
                            // TODO: review
                            this.error(Message::MissingIdentifier, this.span());
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
                self.take(|this, t| match t {
                    Some(Token::Integer(n)) => Next(Value::Constant(Constant::i64(n))),
                    Some(Token::Uinteger(n)) => Next(Value::Constant(Constant::u64(n))),
                    Some(Token::Float(n)) => Next(Value::Constant(Constant::f64(n))),
                    Some(Token::String(s)) => todo!(),
                    Some(Token::Char(s)) => todo!(),
                    Some(Token::Ident(s)) => {
                        let span = this.span();
                        this.next(); // fill
                        if let Some(Token::Colon) = this.tk {
                            this.error(Message::IdentifierIsNotLabel, span);
                            // TODO: not label behavior
                        };
                        if let Some(binding) = this.symbols.find(s) {
                            match binding {
                                Binding::Type(t) => NoFill(Value::Expression(Expression(
                                    this.module.local_get(s),
                                    t,
                                ))),
                                Binding::Constant(c) => NoFill(Value::Constant(c)),
                            }
                        } else {
                            this.error(Message::UnresolvedIdentifier, span);
                            NoFill(this.unreachable())
                        }
                    }
                    Some(Token::Label(x)) => {
                        this.next(); // fill
                        match this.tk {
                            Some(Token::Colon) => this.next(),
                            _ => this.error(Message::MissingColon, this.span()),
                        };

                        NoFill(match this.tk {
                            Some(Token::LeftBracket) => this.parse_block(Some(x), contextual_type),
                            Some(Token::Loop) => this.parse_loop(Some(x), contextual_type),
                            _ => {
                                // TODO: parse atom?
                                this.error(Message::CannotFollowLabel, this.span());
                                this.unreachable()
                            }
                        })
                    }
                    tk => {
                        this.error(Message::UnexpectedToken, this.span());
                        Fill(this.unreachable(), tk)
                    }
                })
            }
        };

        self.postfixatom(lhs)
    }
}
