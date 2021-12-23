use w_errors::Message;
use w_lexer::{AmbiguousOp, BinOp, Lexer, Span, Token};

mod handler;
mod primaryatom;
mod simpleatom;

pub use handler::Handler;
use w_ast::{Atom, IdentPair, Indir, Program, Spanned, Type, TypeVariant, WFn, WStruct, WUnion};

pub struct Parser<'a, H, I>
where
    H: Handler<LexerInput = I>,
    I: Iterator<Item = char>,
{
    session: &'a H,
    src_ref: H::SourceRef,
    lex: Lexer<I>,

    start: usize,
    end: usize,
    tk: Option<Token>,
}

impl<'a, H, I> Parser<'a, H, I>
where
    H: Handler<LexerInput = I>,
    I: Iterator<Item = char>,
{
    pub fn new(session: &'a H, src_ref: H::SourceRef) -> Self {
        let src = session.get_source(&src_ref);
        let lex = Lexer::new(src);

        let mut parser = Parser {
            session,
            src_ref,
            lex,

            start: 0,
            end: 0,
            tk: None,
        };

        parser.next();
        parser
    }

    fn next(&mut self) {
        match self.lex.next() {
            Some((tk, start, end)) => {
                self.tk = Some(tk);
                self.start = start;
                self.end = end;
            }
            _ => {
                self.tk = None;
            }
        }
    }

    /// for owning enum fields
    fn take(&mut self) -> Option<Token> {
        // TODO: maybe turn into a function that
        // accepts a closure?

        self.tk.take()
    }

    fn fill(&mut self, value: Option<Token>) {
        debug_assert!(self.tk.is_none());
        self.tk = value;
    }

    pub(crate) fn error(&self, msg: Message, span: Span) {
        self.session.error(&self.src_ref, msg, span);
    }

    fn span(&self) -> Span {
        Span::new(self.start, self.end)
    }

    fn type_body(
        &mut self,
        allow_no_trailing_semi: bool,
    ) -> Option<Spanned<Vec<Spanned<IdentPair>>>> {
        let start = self.start;
        if let Some(Token::LeftBracket) = self.tk {
            let mut v = Vec::new();
            self.next();

            let end = loop {
                if let Some(Token::RightBracket) = self.tk {
                    let end_ = self.end;
                    self.next();
                    break end_;
                }

                // TODO: add panic behavior
                let pair = self.ident_type_pair(true)?;
                v.push(pair);

                match self.tk {
                    Some(Token::Semicolon) => self.next(),
                    Some(Token::RightBracket) if allow_no_trailing_semi => {
                        let end_ = self.end;
                        self.next();
                        break end_;
                    }
                    None => {
                        self.error(Message::MissingClosingBracket, Span::new(start, self.end));
                        return None;
                    }
                    _ => {
                        // mut ident: type }
                        //                ^
                        // mut ident: type}
                        //                ^
                        let last_pos = v.last().unwrap().1.end + 1;
                        self.error(Message::MissingSemicolon, Span::new(last_pos, last_pos + 1));
                    }
                }
            };

            Some(Spanned(v, Span::new(start, end)))
        } else {
            self.error(Message::MissingOpeningBracket, self.span());
            None
        }
    }

    fn parse_type(&mut self) -> Option<Spanned<Type>> {
        let start = self.start;
        let mut asterisk_overflow_start = None;
        let mut indir = Indir::none();
        let mut len = 0;
        loop {
            match self.tk {
                Some(Token::AmbiguousOp(AmbiguousOp::Asterisk)) => {
                    if len < 5 {
                        self.next();
                        indir = indir.add(match self.tk {
                            Some(Token::Mut) => {
                                self.next();
                                true
                            }
                            _ => false,
                        });
                    } else {
                        // *******type
                        //      ^^
                        if asterisk_overflow_start.is_none() {
                            asterisk_overflow_start = Some(self.start);
                        }

                        self.next();

                        match self.tk {
                            Some(Token::AmbiguousOp(AmbiguousOp::Asterisk)) => {}
                            _ => self.error(
                                Message::TooMuchIndirection,
                                Span::new(asterisk_overflow_start.unwrap(), self.end),
                            ),
                        }
                    }
                    len += 1;
                }
                // TODO: deduplicate struct and union
                Some(Token::Struct) => {
                    self.next();
                    let body = self.type_body(true)?;
                    let end = body.1.end;
                    return Some(Spanned(
                        Type::with_indir(TypeVariant::Struct(body), indir),
                        Span::new(start, end),
                    ));
                }
                Some(Token::Union) => {
                    self.next();
                    let body = self.type_body(true)?;
                    let end = body.1.end;
                    return Some(Spanned(
                        Type::with_indir(TypeVariant::Union(body), indir),
                        Span::new(start, end),
                    ));
                }
                _ => match self.take() {
                    Some(Token::Ident(s)) => {
                        self.next();
                        return Some(Spanned(
                            Type::with_indir(s.into(), indir),
                            Span::new(start, self.end),
                        ));
                    }
                    t => {
                        self.fill(t);
                        self.error(Message::MalformedType, self.span());
                        return None;
                    }
                },
            }
        }
    }

    fn ident_type_pair(&mut self, require_type: bool) -> Option<Spanned<IdentPair>> {
        let start = self.start;

        // mut ident: type
        // ^^^
        let mutable = match self.tk {
            Some(Token::Mut) => {
                let span = self.span();
                self.next();
                Some(span)
            }
            _ => None,
        };

        let ident = {
            let span = self.span();
            match self.tk {
                Some(Token::Colon) => {
                    // mut : type
                    //     ^
                    self.error(Message::MissingIdentifier, span);
                    Spanned("<unknown>".to_owned(), span)
                }
                _ => {
                    let res = match self.take() {
                        Some(Token::Ident(s)) => Spanned(s, span),
                        Some(Token::Label(s)) => {
                            self.error(Message::LabelIsNotIdentifier, span);
                            Spanned(format!("${}", s), span)
                        }
                        _ => {
                            // mut }: type
                            //     ^
                            self.error(Message::MalformedIdentifier, span);
                            Spanned("<unknown>".to_owned(), span)
                        }
                    };

                    self.next(); // fill and move to colon
                    res
                }
            }
        };

        let mut end = ident.1.end;
        let t = match self.tk {
            Some(Token::Colon) => {
                self.next();
                let t = self.parse_type()?;
                end = t.1.end;
                Some(t)
            }
            _ => {
                if require_type {
                    // mut ident ...
                    //          ^
                    let pos = ident.1.end + 1;
                    self.error(Message::MissingType, Span::new(pos, pos + 1));
                    return None;
                }
                None
            }
        };

        Some(Spanned(
            IdentPair { mutable, ident, t },
            Span::new(start, end),
        ))
    }

    fn subatom(&mut self, mut lhs: Spanned<Atom>, min_prec: u8) -> Option<Spanned<Atom>> {
        // https://en.wikipedia.org/wiki/Operator-precedence_parser

        // while [t] is a
        loop {
            // binary operator
            let t = match self.tk {
                Some(Token::BinOp(t)) => t,
                Some(Token::AmbiguousOp(ref t)) => BinOp::Regular(t.binary()),
                _ => break,
            };

            // whose precedence is >= [min_prec]
            let current_prec = t.prec();
            if t.prec() >= min_prec {
                self.next();
                let mut rhs = self.primaryatom()?;
                loop {
                    let (right_associative, next_prec) = match self.tk {
                        Some(Token::BinOp(BinOp::Compound(v))) => (true, v.prec()),
                        Some(Token::BinOp(BinOp::Regular(v))) => (false, v.prec()),
                        Some(Token::AmbiguousOp(v)) => (false, v.binary().prec()),
                        _ => break,
                    };

                    if next_prec > current_prec {
                        rhs = self.subatom(rhs, current_prec + 1)?;
                    } else if right_associative && next_prec == current_prec {
                        rhs = self.subatom(rhs, current_prec)?;
                    } else {
                        break;
                    }
                }

                let span = Span::new(lhs.1.start, rhs.1.end);
                lhs = Spanned(Atom::BinOp(Box::new(lhs), t, Box::new(rhs)), span);
            } else {
                break;
            }
        }
        Some(lhs)
    }

    pub fn atom(&mut self) -> Option<Spanned<Atom>> {
        let lhs = self.primaryatom()?;
        self.subatom(lhs, 0)
    }

    pub fn function(&mut self) -> Option<Spanned<WFn>> {
        let start = self.start;
        debug_assert_eq!(self.tk, Some(Token::Fn));
        self.next();

        let name = match self.take() {
            Some(Token::Ident(s)) => {
                let span = self.span();
                self.next(); // fill
                Spanned(s, span)
            }
            tk => {
                self.error(Message::MissingIdentifier, self.span());
                self.fill(tk);
                return None;
            }
        };

        match self.tk {
            Some(Token::LeftParen) => self.next(),
            _ => {
                self.error(Message::MissingOpeningParen, self.span());
                return None;
            }
        }

        let mut params = Vec::new();

        match self.tk {
            Some(Token::RightParen) => self.next(),
            _ => loop {
                params.push(self.ident_type_pair(true)?);
                match self.tk {
                    Some(Token::Comma) => self.next(),
                    Some(Token::RightParen) => {
                        self.next();
                        break;
                    }
                    _ => {
                        // fn ident(mut ident: type
                        //                         ^
                        // TODO: refactor
                        let pos = params[params.len() - 1].1.end + 1;
                        self.error(Message::MissingClosingParen, Span::new(pos, pos + 1));
                        return None;
                    }
                }
            },
        }

        let t = match self.tk {
            Some(Token::Colon) => {
                self.next();
                Some(self.parse_type()?)
            }
            _ => None,
        };

        let atom = self.atom()?;
        let end = atom.1.end;
        Some(Spanned(
            WFn {
                name,
                params,
                atom,
                t,
            },
            Span::new(start, end),
        ))
    }

    pub fn skip_bracket(&mut self) {
        debug_assert_ne!(self.tk, Some(Token::LeftBracket));
        loop {
            match self.tk {
                Some(Token::RightBracket) => break,
                Some(Token::LeftBracket) => {
                    self.next();
                    self.skip_bracket();
                }
                _ => self.next(),
            }
        }
    }

    pub fn panic_top_level(&mut self, in_bracket: bool) {
        if in_bracket {
            self.skip_bracket();
        }

        loop {
            match self.tk {
                None | Some(Token::Fn | Token::Struct | Token::Union) => break,
                Some(Token::LeftBracket) => {
                    self.next();
                    self.skip_bracket();
                }
                _ => self.next(),
            }
        }
    }

    pub fn parse(mut self) -> Program {
        let mut fns = Vec::new();
        let mut structs = Vec::new();
        let mut unions = Vec::new();
        loop {
            match self.tk {
                Some(Token::Fn) => match self.function() {
                    Some(f) => fns.push(f),
                    None => self.panic_top_level(false),
                },
                // TODO: deduplicate struct and union
                Some(Token::Struct) => {
                    let start = self.start;
                    let struct_pos = self.end;
                    self.next();
                    let name = match self.tk {
                        Some(Token::LeftBracket) => {
                            // struct  {
                            //       ^

                            let pos = struct_pos + 1;
                            let span = Span::new(pos, pos + 1);
                            self.next();
                            self.error(Message::MissingIdentifier, span);
                            Spanned("<unknown>".to_owned(), span)
                        }
                        _ => match self.take() {
                            Some(Token::Ident(s)) => {
                                // struct ident {
                                //        ^^^^^

                                let span = self.span();
                                self.next(); // fill
                                Spanned(s, span)
                            }
                            _ => {
                                // struct ! {
                                //        ^
                                let span = self.span();
                                self.next(); // fill
                                self.error(Message::MalformedIdentifier, span);
                                Spanned("<unknown>".to_owned(), span)
                            }
                        },
                    };

                    match self.type_body(false) {
                        Some(fields) => {
                            let end = fields.1.end;
                            structs.push(Spanned(WStruct { name, fields }, Span::new(start, end)));
                        }
                        None => self.panic_top_level(true),
                    }
                }
                Some(Token::Union) => {
                    let start = self.start;
                    let struct_pos = self.end;
                    self.next();
                    let name = match self.tk {
                        Some(Token::LeftBracket) => {
                            // union  {
                            //      ^

                            let pos = struct_pos + 1;
                            let span = Span::new(pos, pos + 1);
                            self.next();
                            self.error(Message::MissingIdentifier, span);
                            Spanned("<unknown>".to_owned(), span)
                        }
                        _ => match self.take() {
                            Some(Token::Ident(s)) => {
                                // union ident {
                                //       ^^^^^
                                let span = self.span();
                                self.next(); // fill
                                Spanned(s, span)
                            }
                            _ => {
                                // union ! {
                                //       ^
                                let span = self.span();
                                self.next(); // fill
                                self.error(Message::MalformedIdentifier, span);
                                Spanned("<unknown>".to_owned(), span)
                            }
                        },
                    };

                    match self.type_body(false) {
                        Some(fields) => {
                            let end = fields.1.end;
                            unions.push(Spanned(WUnion { name, fields }, Span::new(start, end)));
                        }
                        None => self.panic_top_level(true),
                    }
                }
                Some(_) => {
                    self.error(Message::InvalidTopLevel, self.span());
                    self.panic_top_level(false);
                }
                None => break,
            }
        }

        Program {
            fns,
            structs,
            unions,
        }
    }
}
