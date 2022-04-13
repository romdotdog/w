use std::convert::TryInto;

use crate::types::{Type, TypeVariant};

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

            if self.can_begin_toplevel() && !self.parse_toplevel(){
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
                    lhs = Spanned(
                        Atom::PostIncDec(Box::new(lhs), IncDec::Dec),
                        Span::new(start, self.end),
                    );
                    self.next();
                }

                Some(Token::UnOp(UnOp::Inc)) => {
                    lhs = Spanned(
                        Atom::PostIncDec(Box::new(lhs), IncDec::Inc),
                        Span::new(start, self.end),
                    );
                    self.next();
                }

                Some(Token::Period) => {
                    self.next();
                    lhs = self.take(|this, t| match t {
                        Some(Token::Ident(s)) => Next(Some(Spanned(
                            Atom::Access(Box::new(lhs), Spanned(s, this.span())),
                            Span::new(start, this.end),
                        ))),
                        tk => {
                            // TODO: review
                            this.error(Message::MissingIdentifier, this.span());
                            Fill(None, tk)
                        }
                    })?;
                }

                // TODO: figure out syntax
                Some(Token::Arrow) => {
                    self.next();
                    lhs = self.take(|this, t| match t {
                        Some(Token::Ident(s)) => Next(Some(Spanned(
                            Atom::Offsetof(Box::new(lhs), Spanned(s, this.span())),
                            Span::new(start, this.end),
                        ))),
                        tk => {
                            // TODO: review
                            this.error(Message::MissingIdentifier, this.span());
                            Fill(None, tk)
                        }
                    })?;
                }

                Some(Token::LeftSqBracket) => {
                    self.next();
                    let atom = self.atom()?;

                    match self.tk {
                        Some(Token::RightSqBracket) => {
                            lhs = Spanned(
                                Atom::Index(Box::new(lhs), Box::new(atom)),
                                Span::new(start, self.end),
                            );
                            self.next();
                        }
                        _ => {
                            // TODO: review
                            self.error(Message::MissingClosingSqBracket, self.span());
                            return None;
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

                    lhs = Spanned(Atom::Call(Box::new(lhs), args), Span::new(start, self.end));
                    self.next(); // skip paren
                }

                _ => break,
            }
        }

        Some(lhs)
    }

    fn parse_if(&mut self) -> (S::ExpressionRef, Type) {
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
                (self.module.unreachable(), TypeVariant::Unreachable.into())
            }
        } else if true_branch.1 != TypeVariant::Void.into() {
            self.error(Message::IfCannotReturn, self.span())(
                self.module.unreachable(),
                TypeVariant::Unreachable.into(),
            )
        }
        (
            self.module.if_(cond.0, true_branch.0, false_branch.0),
            TypeVariant::Void.into(),
        )
    }

    pub(crate) fn simpleatom(&mut self, contextual_type: Option<Type>) -> (S::ExpressionRef, Type) {
        let lhs = match self.tk {
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
                let t = self.parse_type()?;

                if self.tk != Some(Token::RightParen) {
                    self.error(Message::MissingClosingParen, self.span());
                }

                self.next();
                todo!()
            }
            Some(Token::If) => self.parse_if()?,
            Some(Token::LeftBracket) => self.parse_block(None),
            Some(Token::Loop) => self.parse_loop(None)?,
            _ => {
                /*
                    ┌───┐┌─────┐
                    │f32││u31  │
                    └┬──┘└┬───┬┘
                     │┌───▽─┐┌▽──┐
                     ││u32  ││i32│
                     │└┬───┬┘└┬┬─┘
                     │┌│───│──┘│
                    ┌▽▽▽┐┌─▽──┐│
                    │f64││u63 ││
                    └───┘└┬──┬┘│
                    ┌─────▽┐┌▽─▽┐
                    │u64   ││i64│
                    └──────┘└───┘
                */
                self.take(|this, t| match t {
                    Some(Token::U31(n)) => {
                        if let Some(t) = contextual_type {
                            if t.is_reference() {
                                this.error(Message::ReferenceCoercion, this.span())
                            } else {
                                match t.v {
                                    TypeVariant::U32 => Next((
                                        self.module.i32_const(reinterpret_u32(n)),
                                        TypeVariant::U32.into(),
                                    )),
                                    TypeVariant::U64 => Next((
                                        self.module.i64_const(reinterpret_u64(n)),
                                        TypeVariant::U32.into(),
                                    )),
                                    TypeVariant::I32 => {
                                        Next((
                                            self.module.i32_const(n.try_into().unwrap()),
                                            TypeVariant::I32,
                                        )) // invariant
                                    }
                                    TypeVariant::I64 => {
                                        Next((self.module.i64_const(n.into()), TypeVariant::I64))
                                    }
                                    TypeVariant::F64 => {
                                        Next((self.module.f64_const(n.into()), TypeVariant::F64))
                                    }
                                    _ => this.error(Message::InvalidCoercion, this.span()),
                                }
                            }
                        } else {
                            this.error(Message::UncoercedU31, this.span());
                        }

                        Next((self.module.unreachable(), TypeVariant::Unreachable.into()))
                    }
                    Some(Token::U63(n)) => {
                        if let Some(t) = contextual_type {
                            if t.is_reference() {
                                this.error(Message::ReferenceCoercion, this.span())
                            } else {
                                match t.v {
                                    TypeVariant::U64 => Next((
                                        self.module.i64_const(reinterpret_u64(n)),
                                        TypeVariant::U32.into(),
                                    )),
                                    TypeVariant::I64 => {
                                        Next((self.module.i64_const(n.into()), TypeVariant::I64))
                                    }
                                    _ => this.error(Message::InvalidCoercion, this.span()),
                                }
                            }
                        } else {
                            this.error(Message::UncoercedU63, this.span());
                        }

                        Next((self.module.unreachable(), TypeVariant::Unreachable.into()))
                    }
                    Some(Token::Fxx(n)) => {
                        if let Some(t) = contextual_type {
                            if t.is_reference() {
                                this.error(Message::ReferenceCoercion, this.span())
                            } else {
                                match t.v {
                                    TypeVariant::F32 => {
                                        Next((self.module.f32_const(n), TypeVariant::F32))
                                    }
                                    TypeVariant::F64 => {
                                        Next((self.module.f64_const(n.into()), TypeVariant::F64))
                                    }
                                    _ => this.error(Message::InvalidCoercion, this.span()),
                                }
                            }
                        } else {
                            this.error(Message::UncoercedFxx, this.span());
                        }

                        Next((self.module.unreachable(), TypeVariant::Unreachable.into()))
                    }

                    Some(Token::I32(n)) => (self.module.i32_const(n), TypeVariant::I32),
                    Some(Token::I64(n)) => (self.module.i64_const(n), TypeVariant::I64),
                    Some(Token::U32(n)) => {
                        (self.module.i32_const(reinterpret_u32(n)), TypeVariant::U32)
                    }
                    Some(Token::U64(n)) => {
                        (self.module.i64_const(reinterpret_u64(n)), TypeVariant::U64)
                    }
                    Some(Token::F64(n)) => (self.module.f64_const(n), TypeVariant::F64),
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
                            Some(Token::LeftBracket) => Some(this.parse_block(Some(x))),
                            Some(Token::Loop) => this.parse_loop(Some(x)),
                            _ => {
                                // TODO: parse atom?
                                this.error(Message::CannotFollowLabel, this.span());
                                None
                            }
                        })
                    }
                    tk => {
                        this.error(Message::UnexpectedToken, this.span());
                        Fill(None, tk)
                    }
                })
            }
        };

        self.postfixatom(lhs)
    }
}

fn reinterpret_u32(n: u32) -> i32 {
    unsafe { std::mem::transmute::<u32, i32>(n) }
}

fn reinterpret_u64(n: u64) -> i64 {
    unsafe { std::mem::transmute::<u64, i64>(n) }
}
