use super::{Handler, Parser};
use w_ast::{Atom, AtomVariant, IncDec, Type};
use w_errors::Message;
use w_lexer::{Token, UnOp};

impl<'a, H, I> Parser<'a, H, I>
where
    H: Handler<LexerInput = I>,
    I: Iterator<Item = char>,
{
    pub(crate) fn parse_block(&mut self, label: Option<String>) -> Atom {
        let mut r = Vec::new();
        let start = self.lex.span();

        let mut last = None;

        'm: loop {
            if let Some(t) = last.take() {
                r.push(t);
            }

            match self.next() {
                Some(Token::RightBracket) => {
                    last = None;
                    break;
                }
                None => {}
                t => {
                    self.backtrack(t);

                    let a = self.atom();
                    match a {
                        Some(_) => last = a,
                        None => {
                            // panic!!
                            loop {
                                let t = self.next();
                                match t {
                                    Some(Token::RightBracket) => {
                                        break 'm;
                                    }
                                    Some(Token::Semicolon) => {
                                        continue 'm;
                                    }
                                    None => break 'm,
                                    _ => {}
                                }
                            }
                        }
                    }
                }
            }

            let err_span = self.lex.span();

            match self.next() {
                Some(Token::RightBracket) => break,
                Some(Token::Semicolon) => {}
                None => {
                    self.error(Message::MissingClosingBracket, err_span);
                    break;
                }
                t => {
                    // try to continue
                    self.error(Message::MissingSemicolon, err_span);
                    self.backtrack(t);
                }
            }
        }

        let span = start.to(self.lex.span());
        Atom {
            t: last.as_ref().map_or_else(Type::void, |a| a.t),
            v: AtomVariant::Block(label, r, last.map(Box::new)),
            span,
        }
    }

    pub(crate) fn parse_loop(&mut self, label: Option<String>) -> Option<Atom> {
        let start = self.lex.span();
        let initial = match self.next() {
            Some(Token::Let) => Some(Box::new(self.parse_let()?)),
            _ => None,
        };

        let loop_body = {
            let a = self.atom()?;
            match a.v {
                AtomVariant::Block(..) => {}
                _ => self.error(Message::LoopBodyBlock, a.span),
            }
            Box::new(a)
        };

        Some(Atom {
            t: Type::auto(),
            v: AtomVariant::Loop(label, initial, loop_body),
            span: start.to(self.lex.span()),
        })
    }

    fn postfixatom(&mut self, mut lhs: Atom) -> Option<Atom> {
        loop {
            match self.next() {
                Some(Token::UnOp(UnOp::Dec)) => {
                    lhs = Atom {
                        span: lhs.span.to(self.lex.span()),
                        v: AtomVariant::PostIncDec(Box::new(lhs), IncDec::Dec),
                        t: Type::auto(),
                    }
                }

                Some(Token::UnOp(UnOp::Inc)) => {
                    lhs = Atom {
                        span: lhs.span.to(self.lex.span()),
                        v: AtomVariant::PostIncDec(Box::new(lhs), IncDec::Inc),
                        t: Type::auto(),
                    }
                }

                Some(Token::Period) => {
                    let period_span = self.lex.span();
                    match self.next() {
                        Some(Token::Ident(s)) => {
                            lhs = Atom {
                                span: lhs.span.to(self.lex.span()),
                                v: AtomVariant::Access(Box::new(lhs), s),
                                t: Type::auto(),
                            }
                        }
                        t => {
                            self.backtrack(t);
                            self.error(Message::MissingIdentifier, period_span.move_by(1));
                            return None;
                        }
                    }
                }

                Some(Token::LeftSqBracket) => {
                    let atom = self.atom()?;

                    match self.next() {
                        Some(Token::RightSqBracket) => {
                            lhs = Atom {
                                span: lhs.span.to(self.lex.span()),
                                v: AtomVariant::Index(Box::new(lhs), Box::new(atom)),
                                t: Type::auto(),
                            }
                        }
                        t => {
                            self.backtrack(t);
                            self.error(Message::MissingClosingSqBracket, self.lex.span());
                            return None;
                        }
                    }
                }

                Some(Token::LeftParen) => {
                    let mut args = Vec::new();

                    match self.next() {
                        Some(Token::RightParen) => {}
                        t => {
                            self.backtrack(t);
                            // TODO: remove

                            loop {
                                args.push(self.atom()?);

                                match self.next() {
                                    Some(Token::Comma) => {}
                                    Some(Token::RightParen) => break,
                                    t => {
                                        self.backtrack(t);
                                        self.error(Message::MissingClosingParen, self.lex.span());
                                        return None;
                                    }
                                }
                            }
                        }
                    }

                    lhs = Atom {
                        span: lhs.span.to(self.lex.span()),
                        v: AtomVariant::Call(Box::new(lhs), args),
                        t: Type::auto(),
                    }
                }

                t => {
                    self.backtrack(t);
                    break;
                }
            }
        }

        Some(lhs)
    }

    fn parse_if(&mut self) -> Option<Atom> {
        let start = self.lex.span();
        let cond = Box::new(self.atom()?);
        let body = Box::new(self.atom()?);
        let else_body = match self.next() {
            Some(Token::Else) => Some(Box::new(self.atom()?)),
            t => {
                self.backtrack(t);
                None
            }
        };

        Some(Atom {
            t: Type::auto(),
            v: AtomVariant::If(cond, body, else_body),
            span: start.to(self.lex.span()),
        })
    }

    pub(crate) fn simpleatom(&mut self, t: Option<Token>) -> Option<Atom> {
        let lhs = match t {
            Some(Token::LeftParen) => {
                let start = self.lex.span();
                let e = self.atom()?;

                let t = self.next();
                if t != Some(Token::RightParen) {
                    self.backtrack(t);
                    self.error(Message::MissingClosingParen, self.lex.span());
                    return None;
                }

                Atom {
                    t: e.t,
                    v: AtomVariant::Paren(Box::new(e)),
                    span: start.to(self.lex.span()),
                }
            }
            Some(Token::Float(f)) => Atom {
                v: AtomVariant::Float(f),
                span: self.lex.span(),
                t: Type::auto(),
            },
            Some(Token::Integer(i)) => Atom {
                v: AtomVariant::Integer(i),
                span: self.lex.span(),
                t: Type::auto(),
            },
            Some(Token::Ident(s)) => {
                let r = Atom {
                    v: AtomVariant::Ident(s),
                    span: self.lex.span(),
                    t: Type::auto(),
                };
                match self.next() {
                    Some(Token::Colon) => {
                        self.error(Message::IdentifierIsNotLabel, self.lex.span());
                    }
                    t => self.backtrack(t),
                };
                r
            }
            Some(Token::String(s)) => Atom {
                v: AtomVariant::String(s),
                span: self.lex.span(),
                t: Type::auto(),
            },
            Some(Token::Char(s)) => Atom {
                v: AtomVariant::Char(s),
                span: self.lex.span(),
                t: Type::auto(),
            },
            Some(Token::If) => self.parse_if()?,
            Some(Token::LeftBracket) => self.parse_block(None),
            Some(Token::Loop) => self.parse_loop(None)?,
            Some(Token::Label(x)) => {
                let t = match self.next() {
                    Some(Token::Colon) => self.next(),
                    t => {
                        self.error(Message::MissingColon, self.lex.span());
                        t
                    }
                };

                match t {
                    Some(Token::LeftBracket) => self.parse_block(Some(x)),
                    Some(Token::Loop) => self.parse_loop(Some(x))?,
                    _ => {
                        self.error(Message::CannotFollowLabel, self.lex.span());
                        return None;
                    }
                }
            }
            _ => {
                self.error(Message::UnexpectedToken, self.lex.span());
                return None;
            }
        };

        self.postfixatom(lhs)
    }
}
