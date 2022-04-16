use std::convert::TryInto;

use crate::{
    types::{Constant, Type, TypeVariant},
    Expression, Value,
};

use super::{Compiler, Fill, Handler, Next, NoFill};
use w_codegen::Serializer;
use w_errors::Message;
use w_lexer::token::{Token, UnOp};

impl<'ast, H: Handler<'ast>, S: Serializer> Compiler<'ast, H, S> {
    pub(crate) fn parse_block(&mut self, label: Option<Spanned<&'ast str>>) -> Spanned<Atom<'ast>> {
        let start = self.start;
        debug_assert_eq!(self.tk, Some(Token::LeftBracket));
        self.next();

        let mut contents = Vec::new();
        let mut last = None;

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

            if let Some(t) = last.take() {
                contents.push(t);
            }

            if self.can_begin_toplevel() && !self.parse_toplevel() {
                panic_block!();
            }

            match self.tk {
                Some(Token::RightBracket) => {
                    last = None;

                    let end_ = self.end;
                    self.next();
                    break end_;
                }
                None => {}
                _ => match self.atom() {
                    Some(a) => last = Some(a),
                    None => panic_block!(),
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
        todo!()
    }

    pub(crate) fn parse_loop(
        &mut self,
        label: Option<Spanned<&'ast str>>,
    ) -> Option<Spanned<Atom<'ast>>> {
        let start = self.start;
        debug_assert_eq!(self.tk, Some(Token::Loop));
        self.next();

        let binding = match self.tk {
            Some(Token::Let) => Some(Box::new(self.parse_let()?)),
            _ => None,
        };

        let block = {
            let a = self.atom()?;
            match a.0 {
                Atom::Block { .. } => {}
                _ => self.error(Message::LoopBodyBlock, a.1),
            }
            Box::new(a)
        };

        /*let end = block.1.end;
        Some(Spanned(
            Atom::Loop {
                label,
                binding,
                block,
            },
            Span::new(start, end),
        ))*/
        todo!()
    }

    fn postfixatom(&mut self, mut lhs: (S::ExpressionRef, Type)) -> (S::ExpressionRef, Type) {
        let start = lhs.1.start;
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
                    let mut args = Vec::new();

                    match self.tk {
                        Some(Token::RightParen) => break,
                        _ => loop {
                            args.push(self.atom()?);

                            match self.tk {
                                Some(Token::Comma) => self.next(),
                                Some(Token::RightParen) => break,
                                _ => {
                                    self.error(Message::MissingClosingParen, self.span());
                                    return None;
                                }
                            }
                        },
                    };

                    //lhs = Spanned(Atom::Call(Box::new(lhs), args), Span::new(start, self.end));
                    self.next(); // skip paren
                }

                _ => break,
            }
        }

        lhs
    }

    fn parse_if(&mut self) -> Value<S> {
        let start = self.start;
        debug_assert_eq!(self.tk, Some(Token::If));
        self.next();

        let cond = self.atom(None);
        let true_branch = self.atom(None);
        let false_branch = match self.tk {
            Some(Token::Else) => {
                self.next();
                self.atom(Some(true_branch.1))
            }
            _ => None,
        };

        // TODO: merge
        if let Some(branch_expr) = false_branch {
            if false_branch.1 == true_branch.1 {
                (
                    self.module.if_(cond.0, true_branch.0, false_branch.0),
                    false_branch.1,
                )
            } else {
                self.error(Message::IfCannotReturn, self.span());
                return Value::Expression(Expression(
                    self.module.unreachable(),
                    TypeVariant::Unreachable.into(),
                ));
            }
        } else if true_branch.1 != TypeVariant::Void.into() {
            self.error(Message::IfCannotReturn, self.span());
            return Value::Expression(Expression(
                self.module.unreachable(),
                TypeVariant::Unreachable.into(),
            ));
        }
        Value::Expression(Expression(
            self.module.if_(cond.0, true_branch.0, false_branch.0),
            TypeVariant::Void.into(),
        ))
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
            Some(Token::Loop) => self.parse_loop(None)?,
            Some(Token::Overflown) => {
                self.error(Message::LiteralTooLarge, self.span());
                Value::Expression(Expression(
                    self.module.unreachable(),
                    TypeVariant::Unreachable.into(),
                ))
            }
            _ => {
                self.take(|this, t| match t {
                    Some(Token::Integer(n)) => Next(Value::Constant(Constant::from_i64(n))),
                    Some(Token::Uinteger(n)) => Next(Value::Constant(Constant::from_u64(n))),
                    Some(Token::Float(n)) => Next(Value::Constant(Constant::from_f64(n))),
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
                                Value::Expression(Expression(
                                    self.module.unreachable(),
                                    Type::from(TypeVariant::Unreachable),
                                ))
                            }
                        })
                    }
                    tk => {
                        this.error(Message::UnexpectedToken, this.span());
                        Fill(
                            (
                                self.module.unreachable(),
                                Type::from(TypeVariant::Unreachable),
                            ),
                            tk,
                        )
                    }
                })
            }
        };

        self.postfixatom(lhs)
    }
}
