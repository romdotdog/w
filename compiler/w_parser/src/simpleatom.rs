use super::{Fill, Handler, Next, NoFill, Parser};
use w_ast::{ASTObject, Atom, IncDec, Span, Spanned};
use w_errors::Message;
use w_lexer::token::{Token, UnOp};

impl<'ast, H: Handler<'ast>> Parser<'ast, H> {
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
                contents.push(ASTObject::Atom(t));
            }

            if self.can_begin_toplevel() {
                match self.parse_toplevel() {
                    Some(t) => contents.push(ASTObject::TopLevel(t)),
                    None => panic_block!(),
                }
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

        Spanned(
            Atom::Block {
                label,
                contents,
                ret: last.map(Box::new),
            },
            Span::new(start, end),
        )
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

        let end = block.1.end;
        Some(Spanned(
            Atom::Loop {
                label,
                binding,
                block,
            },
            Span::new(start, end),
        ))
    }

    fn postfixatom(&mut self, mut lhs: Spanned<Atom<'ast>>) -> Option<Spanned<Atom<'ast>>> {
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

    fn parse_if(&mut self) -> Option<Spanned<Atom<'ast>>> {
        let start = self.start;
        debug_assert_eq!(self.tk, Some(Token::If));
        self.next();

        let cond = Box::new(self.atom()?);
        let true_branch = Box::new(self.atom()?);

        let mut end = true_branch.1.end;
        let false_branch = match self.tk {
            Some(Token::Else) => {
                self.next();
                let a = self.atom()?;
                end = a.1.end;
                Some(Box::new(a))
            }
            _ => None,
        };

        Some(Spanned(
            Atom::If {
                cond,
                true_branch,
                false_branch,
            },
            Span::new(start, end),
        ))
    }

    pub(crate) fn simpleatom(&mut self) -> Option<Spanned<Atom<'ast>>> {
        let lhs = match self.tk {
            Some(Token::LeftParen) => {
                let start = self.start;
                self.next();

                let e = self.atom()?;

                if self.tk != Some(Token::RightParen) {
                    self.error(Message::MissingClosingParen, self.span());
                }

                let span = Span::new(start, self.end);
                self.next();
                Spanned(Atom::Paren(Box::new(e)), span)
            }
            Some(Token::Sizeof) => {
                let start = self.start;
                self.next();

                if self.tk != Some(Token::LeftParen) {
                    self.error(Message::MissingOpeningParen, self.span());
                }

                self.next();
                let t = self.parse_type()?;

                if self.tk != Some(Token::RightParen) {
                    self.error(Message::MissingClosingParen, self.span());
                }

                let span = Span::new(start, self.end);
                self.next();
                Spanned(Atom::Sizeof(t), span)
            }
            Some(Token::If) => self.parse_if()?,
            Some(Token::LeftBracket) => self.parse_block(None),
            Some(Token::Loop) => self.parse_loop(None)?,
            _ => {
                self.take(|this, t| match t {
                    Some(Token::U31(n)) => Next(Some(Spanned(Atom::U31(n), this.span()))),
                    Some(Token::U63(n)) => Next(Some(Spanned(Atom::U63(n), this.span()))),
                    Some(Token::Fxx(n)) => Next(Some(Spanned(Atom::Fxx(n), this.span()))),
                    Some(Token::I32(n)) => Next(Some(Spanned(Atom::I32(n), this.span()))),
                    Some(Token::I64(n)) => Next(Some(Spanned(Atom::I64(n), this.span()))),
                    Some(Token::U32(n)) => Next(Some(Spanned(Atom::U32(n), this.span()))),
                    Some(Token::U64(n)) => Next(Some(Spanned(Atom::U64(n), this.span()))),
                    Some(Token::F64(n)) => Next(Some(Spanned(Atom::F64(n), this.span()))),
                    Some(Token::String(s)) => Next(Some(Spanned(Atom::String(s), this.span()))),
                    Some(Token::Char(s)) => Next(Some(Spanned(Atom::Char(s), this.span()))),
                    Some(Token::Ident(s)) => {
                        let span = this.span();
                        this.next(); // fill
                        if let Some(Token::Colon) = this.tk {
                            this.error(Message::IdentifierIsNotLabel, this.span());
                            // TODO: not label behavior
                        };
                        NoFill(Some(Spanned(Atom::Ident(s), span)))
                    }
                    Some(Token::Label(x)) => {
                        let x = Spanned(x, this.span());
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
                })?
            }
        };

        self.postfixatom(lhs)
    }
}
