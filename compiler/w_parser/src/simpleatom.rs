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

        'm: loop {
            if let Some(t) = last.take() {
                r.push(t);
            }

            match self.tk {
                Some(Token::RightBracket) => {
                    last = None;
                    self.next();
                    break;
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
                                        self.next();
                                        break 'm;
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
                    self.next();
                    break;
                }
                Some(Token::Semicolon) => self.next(),
                None => {
                    self.error(Message::MissingClosingBracket, Span::new(start, self.end));
                    break;
                }
                _ => {
                    // try to continue
                    println!("eoa at: {:?}", self.tk);
                    self.error(Message::MissingSemicolon, self.span());
                }
            }
        }

        Spanned(
            Atom::Block(label, r, last.map(Box::new)),
            Span::new(start, self.end),
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

        Some(Spanned(
            Atom::Loop(label, initial, loop_body),
            Span::new(start, self.end),
        ))
    }

    fn postfixatom(&mut self, mut lhs: Spanned<Atom>) -> Option<Spanned<Atom>> {
        loop {
            match self.tk {
                Some(Token::UnOp(UnOp::Dec)) => {
                    let start = lhs.1.start;
                    lhs = Spanned(
                        Atom::PostIncDec(Box::new(lhs), IncDec::Dec),
                        Span::new(start, self.end),
                    );
                    self.next();
                }

                Some(Token::UnOp(UnOp::Inc)) => {
                    let start = lhs.1.start;
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
                            let start = lhs.1.start;
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
                            let start = lhs.1.start;
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
                        Some(Token::RightParen) => self.next(),
                        _ => loop {
                            args.push(self.atom()?);

                            match self.tk {
                                Some(Token::Comma) => self.next(),
                                Some(Token::RightParen) => {
                                    self.next();
                                    break;
                                }
                                _ => {
                                    self.error(Message::MissingClosingParen, self.span());
                                    return None;
                                }
                            }
                        },
                    }

                    let start = lhs.1.start;
                    lhs = Spanned(Atom::Call(Box::new(lhs), args), Span::new(start, self.end));
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
        let else_body = match self.tk {
            Some(Token::Else) => {
                self.next();
                Some(Box::new(self.atom()?))
            }
            _ => None,
        };

        Some(Spanned(
            Atom::If(cond, body, else_body),
            Span::new(start, self.end),
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
