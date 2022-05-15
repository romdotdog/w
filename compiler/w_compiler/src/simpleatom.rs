use crate::{
    registry::Item,
    types::{
        constant::Constant,
        typ::{Type, UNREACHABLE, VOID},
    },
    Expression, Value,
};

use super::{Compiler, Fill, Handler, Next, NoFill};
use w_codegen::Serializer;
use w_errors::Message;
use w_lexer::token::{Token, UnOp};
use w_utils::span::Span;

impl<'ast, H: Handler<'ast>, S: Serializer> Compiler<'ast, H, S> {
    pub(crate) fn parse_block(&mut self, label: Option<&'ast str>) -> Value<S> {
        let start = self.start;
        debug_assert_eq!(self.tk, Some(Token::LeftBracket));
        self.next();

        let mut contents = Vec::new();
        let typ = None;

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

            if self.can_begin_toplevel() && !self.parse_toplevel() {
                panic_block!();
            }

            match self.tk {
                Some(Token::RightBracket) => {
                    typ = None;

                    let end_ = self.end;
                    self.next();
                    break end_;
                }
                None => {}
                _ => match self.atom() {
                    Value::Expression(Expression(x, xt)) => {
                        contents.push(x);
                        typ = Some(xt);
                    }
                    Value::Constant(x) => {
                        self.error(Message::UselessConstant, self.span());
                    }
                },
            }

            match self.tk {
                Some(Token::RightBracket) => {
                    let end_ = self.end;
                    self.next();
                    break end_;
                }
                Some(Token::Semicolon) => self.next(),
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

        /*Spanned(
            Atom::Block {
                label,
                contents,
                ret: last.map(Box::new),
            },
            Span::new(start, end),
        )*/
        Value::Expression(Expression(
            self.module.block(label, &contents, typ.map(Type::resolve)),
            typ.unwrap_or(VOID),
        ))
    }

    pub(crate) fn parse_loop(&mut self, label: Option<&'ast str>) -> Value<S> {
        let start = self.start;
        debug_assert_eq!(self.tk, Some(Token::Loop));
        self.next();

        let binding = match self.tk {
            Some(Token::Let) => Some(Box::new(self.parse_let())),
            _ => None,
        };

        // TODO: results
        Value::Expression(Expression(
            self.module.loop_(
                label,
                match self.atom() {
                    Value::Expression(Expression(x, _)) => x,
                    Value::Constant(_) => {
                        self.error(Message::UselessConstant, self.span());
                        return self.unreachable();
                    }
                },
            ),
            VOID,
        ))
    }

    fn parse_if(&mut self) -> Value<S> {
        let start = self.start;
        debug_assert_eq!(self.tk, Some(Token::If));
        self.next();

        let cond = self.atom();
        let true_branch = self.atom();
        let false_branch = match self.tk {
            Some(Token::Else) => {
                self.next();
                Some(self.atom())
            }
            _ => None,
        };

        let cond = match cond {
            Value::Expression(x @ Expression(_, xt)) => {
                if xt.is_number() && !xt.is_high() && !xt.is_float() {
                    x
                } else {
                    self.error(Message::TypeMismatch, self.span());
                    self.unreachable_expr()
                }
            }
            Value::Constant(x) => {
                if x.is_true() {
                    // TODO: fix?
                    self.error(Message::ConditionTrue, self.span());
                } else {
                    self.error(Message::ConditionFalse, self.span());
                }
                self.unreachable_expr()
            }
        };

        if let Some(false_branch) = false_branch {
            if let Some((true_branch, false_branch)) = true_branch.coerce(self.module, false_branch)
            {
                let true_branch = match true_branch.compile(self.module, None) {
                    Some(x) => x,
                    None => {
                        self.error(Message::NeedType, self.span());
                        self.unreachable_expr()
                    }
                };

                let false_branch = match false_branch.compile(self.module, None) {
                    Some(x) => x,
                    None => {
                        self.error(Message::NeedType, self.span());
                        self.unreachable_expr()
                    }
                };

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
                Value::Expression(Expression(x, xt)) if xt == UNREACHABLE || xt == VOID => {
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
                    let atom = self.atom()?;

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
                            return (self.module.unreachable(), TypeVariant::Unreachable.into());
                        }
                    }
                }

                Some(Token::LeftParen) => {
                    self.next();
                    // typecheck lhs
                    let item = match lhs {
                        Value::Expression(Expression(x, xt)) => {
                            if let Some(item) = self.registry.get(xt.ref_index()) {
                                item
                            } else {
                                return self.unreachable();
                            }
                        }
                        Value::Constant(x) => {
                            self.error(Message::TypeMismatch, self.span());
                            return self.unreachable();
                        }
                    };

                    if let Item::Fn(name, params, ret) = item {
                        let mut args = Vec::new();

                        let mut i = 0;
                        match self.tk {
                            Some(Token::RightParen) => break,
                            _ => loop {
                                let t = if let Some(p) = params.get(i) {
                                    p.typ
                                } else {
                                    self.error(Message::TooManyArgs, self.span());
                                    return self.unreachable();
                                };

                                if let Some(Expression(x, xt)) =
                                    self.atom().compile(self.module, Some(t))
                                {
                                    args.push(x);
                                } else {
                                    self.error(Message::NeedType, self.span());
                                    return self.unreachable();
                                }

                                match self.tk {
                                    Some(Token::Comma) => self.next(),
                                    Some(Token::RightParen) => break,
                                    _ => {
                                        self.error(Message::MissingClosingParen, self.span());
                                        return self.unreachable();
                                    }
                                }

                                i += 1;
                            },
                        };

                        //lhs = Spanned(Atom::Call(Box::new(lhs), args), Span::new(start, self.end));
                        self.next(); // skip paren
                    } else {
                        self.error(Message::TypeMismatch, self.span());
                        return self.unreachable();
                    }
                }

                _ => break,
            }
        }

        lhs
    }

    pub(crate) fn simpleatom(&mut self) -> Value<S> {
        let lhs: Value<S> = match self.tk {
            Some(Token::LeftParen) => {
                self.next();

                let e = self.atom();

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
                let t = self.parse_type()?;

                if self.tk != Some(Token::RightParen) {
                    self.error(Message::MissingClosingParen, self.span());
                }

                self.next();
                todo!()
            }
            Some(Token::If) => self.parse_if(),
            Some(Token::LeftBracket) => self.parse_block(None),
            Some(Token::Loop) => self.parse_loop(None),
            Some(Token::Overflown) => {
                self.error(Message::LiteralTooLarge, self.span());
                return self.unreachable();
            }
            _ => {
                self.take(|this, t| match t {
                    Some(Token::Integer(n)) => Next(Value::Constant(Constant::Integer(n))),
                    Some(Token::Uinteger(n)) => Next(Value::Constant(Constant::Uinteger(n))),
                    Some(Token::Float(n)) => Next(Value::Constant(Constant::Float(n))),
                    Some(Token::String(s)) => todo!(),
                    Some(Token::Char(s)) => todo!(),
                    Some(Token::Ident(s)) => {
                        let span = this.span();
                        this.next(); // fill
                        if let Some(Token::Colon) = this.tk {
                            this.error(Message::IdentifierIsNotLabel, this.span());
                            // TODO: not label behavior
                        };
                        todo!()
                    }
                    Some(Token::Label(x)) => {
                        this.next(); // fill
                        match this.tk {
                            Some(Token::Colon) => this.next(),
                            _ => this.error(Message::MissingColon, this.span()),
                        };

                        NoFill(match this.tk {
                            Some(Token::LeftBracket) => this.parse_block(Some(x)),
                            Some(Token::Loop) => this.parse_loop(Some(x)),
                            _ => {
                                // TODO: parse atom?
                                this.error(Message::CannotFollowLabel, this.span());
                                self.unreachable()
                            }
                        })
                    }
                    tk => {
                        this.error(Message::UnexpectedToken, this.span());
                        Fill(self.unreachable(), tk)
                    }
                })
            }
        };

        self.postfixatom(lhs)
    }
}
