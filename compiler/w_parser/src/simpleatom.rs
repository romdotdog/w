use super::{Handler, Parser};
use w_ast::{Atom, IncDec, Spanned};
use w_errors::Message;
use w_lexer::{Span, Token, UnOp};

impl<'a, H, I> Parser<'a, H, I>
where
    H: Handler<LexerInput = I>,
    I: Iterator<Item = char>,
{
    pub(crate) fn parse_block(&mut self, label: Option<Spanned<String>>) -> Spanned<Atom> {
        let start = self.start;
        debug_assert_eq!(self.tk, Some(Token::LeftBracket));
        self.next();

        let mut r = Vec::new();
        let mut last = None;

        let end = 'm: loop {
            if let Some(t) = last.take() {
                r.push(t);
            }

            match self.tk {
                Some(Token::RightBracket) => {
                    last = None;

                    let _end = self.end;
                    self.next();
                    break _end;
                }
                None => {}
                _ => {
                    match self.atom() {
                        Some(a) => last = Some(a),
                        None => {
                            // panic!!
                            loop {
                                match self.tk {
                                    Some(Token::Semicolon) => {
                                        self.next();
                                        continue 'm;
                                    }
                                    Some(Token::RightBracket) | None => {
                                        let _end = self.end;
                                        self.next();
                                        break 'm _end;
                                    }
                                    _ => self.next(),
                                }
                            }
                        }
                    }
                }
            }

            match self.tk {
                Some(Token::RightBracket) => {
                    let _end = self.end;
                    self.next();
                    break _end;
                }
                Some(Token::Semicolon) => self.next(),
                None => {
                    let _end = self.end;
                    self.error(Message::MissingClosingBracket, Span::new(start, _end));
                    break _end;
                }
                _ => {
                    // try to continue
                    println!("eoa at: {:?}", self.tk);
                    self.error(Message::MissingSemicolon, self.span());
                }
            }
        };

        Spanned(
            Atom::Block(label, r, last.map(Box::new)),
            Span::new(start, end),
        )
    }

    pub(crate) fn parse_loop(&mut self, label: Option<Spanned<String>>) -> Option<Spanned<Atom>> {
        let start = self.start;
        debug_assert_eq!(self.tk, Some(Token::Loop));
        self.next();

        let initial = match self.tk {
            Some(Token::Let) => Some(Box::new(self.parse_let()?)),
            _ => None,
        };

        let loop_body = {
            let a = self.atom()?;
            match a.0 {
                Atom::Block(..) => {}
                _ => self.error(Message::LoopBodyBlock, a.1),
            }
            Box::new(a)
        };

        let end = loop_body.1.end;
        Some(Spanned(
            Atom::Loop(label, initial, loop_body),
            Span::new(start, end),
        ))
    }

    fn postfixatom(&mut self, mut lhs: Spanned<Atom>) -> Option<Spanned<Atom>> {
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
                    match self.take() {
                        Some(Token::Ident(s)) => {
                            lhs = Spanned(
                                Atom::Access(Box::new(lhs), Spanned(s, self.span())),
                                Span::new(start, self.end),
                            );
                            self.next(); // fill
                        }
                        tk => {
                            // TODO: review
                            self.fill(tk); // fill
                            self.error(Message::MissingIdentifier, self.span());
                            return None;
                        }
                    }
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

    fn parse_if(&mut self) -> Option<Spanned<Atom>> {
        let start = self.start;
        debug_assert_eq!(self.tk, Some(Token::If));
        self.next();

        let cond = Box::new(self.atom()?);
        let body = Box::new(self.atom()?);

        let mut end = body.1.end;
        let else_body = match self.tk {
            Some(Token::Else) => {
                self.next();
                let a = self.atom()?;
                end = a.1.end;
                Some(Box::new(a))
            }
            _ => None,
        };

        Some(Spanned(
            Atom::If(cond, body, else_body),
            Span::new(start, end),
        ))
    }

    pub(crate) fn simpleatom(&mut self) -> Option<Spanned<Atom>> {
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
            Some(Token::If) => self.parse_if()?,
            Some(Token::LeftBracket) => self.parse_block(None),
            Some(Token::Loop) => self.parse_loop(None)?,
            _ => {
                match self.take() {
                    Some(Token::Float(f)) => {
                        let span = self.span();
                        self.next(); // fill
                        Spanned(Atom::Float(f), span)
                    }
                    Some(Token::Integer(i)) => {
                        let span = self.span();
                        self.next(); // fill
                        Spanned(Atom::Integer(i), span)
                    }
                    Some(Token::Ident(s)) => {
                        let span = self.span();
                        self.next(); // fill
                        if let Some(Token::Colon) = self.tk {
                            self.error(Message::IdentifierIsNotLabel, self.span());
                            // TODO: not label behavior
                        };
                        Spanned(Atom::Ident(s), span)
                    }
                    Some(Token::String(s)) => {
                        let span = self.span();
                        self.next(); // fill
                        Spanned(Atom::String(s), span)
                    }
                    Some(Token::Char(s)) => {
                        let span = self.span();
                        self.next(); // fill
                        Spanned(Atom::Char(s), span)
                    }
                    Some(Token::Label(x)) => {
                        let x = Spanned(x, self.span());
                        self.next(); // fill
                        match self.tk {
                            Some(Token::Colon) => self.next(),
                            _ => self.error(Message::MissingColon, self.span()),
                        };

                        match self.tk {
                            Some(Token::LeftBracket) => self.parse_block(Some(x)),
                            Some(Token::Loop) => self.parse_loop(Some(x))?,
                            _ => {
                                // TODO: parse atom?
                                self.error(Message::CannotFollowLabel, self.span());
                                return None;
                            }
                        }
                    }
                    tk => {
                        self.fill(tk); // fill
                        self.error(Message::UnexpectedToken, self.span());
                        return None;
                    }
                }
            }
        };

        self.postfixatom(lhs)
    }
}
