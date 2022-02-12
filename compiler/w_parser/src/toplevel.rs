use super::{Fill, Handler, Next, Parser};
use std::collections::{hash_map::Entry, HashMap};
use w_ast::{IdentPair, Program, Span, Spanned, WEnum, WFn, WStruct, WUnion, WStatic};
use w_errors::Message;
use w_lexer::token::{BinOp, BinOpVariant, Token};

impl<'ast, H: Handler<'ast>> Parser<'ast, H> {
	// static

	pub(crate) fn parse_static(&mut self) -> Option<Spanned<WStatic<'ast>>> {
        let start = self.start;
        assert_eq!(self.tk, Some(Token::Static));
        self.next();

		let decl = self.parse_decl()?;
		let end = decl.1.end;
        Some(Spanned(WStatic(decl.0), Span::new(start, end)))
    }

    // enums

    fn enum_body(&mut self) -> Option<Spanned<HashMap<&'ast str, i64>>> {
        let mut discriminant = 0;
        let start = self.start;
        let mut h = HashMap::new();
        self.next();

        let end = loop {
            match self.tk {
                Some(Token::RightBracket) => {
                    let end_ = self.end;
                    self.next();
                    break end_;
                }
                Some(Token::Comma) => {
                    self.error(
                        Message::MissingIdentifier,
                        Span::new(self.start - 1, self.start),
                    );
                    self.next();
                    continue;
                }
                _ => {
                    let (sident, s) = self.take(|this, t| match t {
                        // take
                        Some(Token::Ident(s)) => Next(Some((this.span(), s))),
                        t => Fill(None, t),
                    })?;

                    match self.tk {
                        Some(Token::RightBracket) => {
                            match h.entry(s) {
                                Entry::Vacant(e) => {
                                    e.insert(discriminant);
                                }
                                Entry::Occupied(_) => {
                                    self.error(Message::DuplicateEnumField, sident);
                                }
                            }

                            let end_ = self.end;
                            self.next();
                            break end_;
                        }
                        Some(Token::Comma) => {
                            match h.entry(s) {
                                Entry::Vacant(e) => {
                                    e.insert(discriminant);
                                }
                                Entry::Occupied(_) => {
                                    self.error(Message::DuplicateEnumField, sident);
                                }
                            }

                            discriminant += 1;
                            self.next();
                            continue;
                        }
                        Some(Token::BinOp(BinOp::Compound(BinOpVariant::Id))) => {
                            self.next();
                            discriminant = self.take(|this, t| match t {
                                // take
                                Some(Token::Integer(i)) => {
                                    match h.entry(s) {
                                        Entry::Vacant(e) => {
                                            e.insert(i);
                                        }
                                        Entry::Occupied(_) => this.error(
                                            Message::DuplicateEnumField,
                                            Span::new(sident.start, this.end),
                                        ),
                                    }
                                    Next(i + 1)
                                }
                                Some(Token::UInteger(_)) => {
                                    match h.entry(s) {
                                        Entry::Vacant(e) => {
                                            e.insert(discriminant);
                                        }
                                        Entry::Occupied(_) => {
                                            this.error(Message::DuplicateEnumField, sident);
                                        }
                                    }

                                    this.error(Message::IntegerNoFit, this.span());
                                    Next(discriminant + 1)
                                }
                                t => {
                                    this.error(Message::MissingInteger, this.span());
                                    Fill(discriminant + 1, t)
                                }
                            });
                            match self.tk {
                                Some(Token::RightBracket) => {
                                    let end_ = self.end;
                                    self.next();
                                    break end_;
                                }
                                Some(Token::Comma) => {
                                    self.next();
                                    continue;
                                }
                                _ => {
                                    self.error(Message::UnexpectedToken, self.span());
                                    return None;
                                }
                            }
                        }
                        _ => {
                            self.error(Message::UnexpectedToken, self.span());
                            return None;
                        }
                    }
                }
            }
        };

        Some(Spanned(h, Span::new(start, end)))
    }

    pub fn parse_enum(&mut self) -> Option<Spanned<WEnum<'ast>>> {
        let start = self.start;
        debug_assert_eq!(self.tk, Some(Token::Enum));
        self.next();

        let name = self.expect_ident(&Some(Token::LeftBracket));
        if let Some(Token::LeftBracket) = self.tk {
            let fields = self.enum_body()?;
            let end = fields.1.end;
            Some(Spanned(WEnum { name, fields }, Span::new(start, end)))
        } else {
            self.error(Message::MissingOpeningBracket, self.span());
            None
        }
    }

    // structs

    pub(crate) fn type_body(
        &mut self,
        allow_no_trailing_semi: bool,
    ) -> Option<Spanned<Vec<Spanned<IdentPair<'ast>>>>> {
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

    pub fn struct_or_union(
        &mut self,
        is_struct: bool,
        structs: &mut Vec<Spanned<WStruct<'ast>>>,
        unions: &mut Vec<Spanned<WUnion<'ast>>>,
    ) {
        let start = self.start;
        debug_assert!(matches!(self.tk, Some(Token::Struct | Token::Union)));
        self.next();

        let name = self.expect_ident(&Some(Token::LeftBracket));
        if let Some(fields) = self.type_body(false) {
            let end = fields.1.end;
            if is_struct {
                structs.push(Spanned(WStruct { name, fields }, Span::new(start, end)));
            } else {
                unions.push(Spanned(WUnion { name, fields }, Span::new(start, end)));
            }
        } else {
            self.panic_top_level(true);
        }
    }

    // functions

    pub fn function(&mut self, exported: bool) -> Option<Spanned<WFn<'ast>>> {
        let start = self.start;
        debug_assert_eq!(self.tk, Some(Token::Fn));
        self.next();

        let name = self.expect_ident(&Some(Token::LeftParen));
        let paren_end = match self.tk {
            Some(Token::LeftParen) => {
                self.next();
                self.end
            }
            _ => {
                self.error(Message::MissingOpeningParen, self.span());
                return None;
            }
        };

        let mut params = Vec::new();

        match self.tk {
            Some(Token::RightParen) => self.next(),
            Some(Token::Ident(_) | Token::Mut) => loop {
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
                        let pos = params[params.len() - 1].1.end;
                        self.error(Message::MissingClosingParen, Span::new(pos, pos + 1));
                        return None;
                    }
                }
            },
            _ => {
                // fn ident(
                //          ^
                self.error(
                    Message::MissingClosingParen,
                    Span::new(paren_end, paren_end + 1),
                );
                return None;
            }
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
                exported,
            },
            Span::new(start, end),
        ))
    }

    // panicking behavior

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
                None | Some(Token::Fn | Token::Struct | Token::Union | Token::Enum) => break,
                Some(Token::LeftBracket) => {
                    self.next();
                    self.skip_bracket();
                }
                _ => self.next(),
            }
        }
    }

    // main

    pub fn parse(mut self) -> Program<'ast> {
        let mut fns = Vec::new();
        let mut structs = Vec::new();
        let mut unions = Vec::new();
        let mut enums = Vec::new();
		let mut statics = Vec::new();
        loop {
            match self.tk {
                Some(Token::Export) => {
                    self.next();
                    match self.function(true) {
                        Some(f) => fns.push(f),
                        None => self.panic_top_level(false),
                    }
                }
                Some(Token::Fn) => match self.function(false) {
                    Some(f) => fns.push(f),
                    None => self.panic_top_level(false),
                },
                Some(Token::Struct) => {
                    self.struct_or_union(true, &mut structs, &mut unions);
                }
                Some(Token::Union) => {
                    self.struct_or_union(false, &mut structs, &mut unions);
                }
                Some(Token::Enum) => match self.parse_enum() {
                    Some(f) => enums.push(f),
                    None => self.panic_top_level(true),
                },
				Some(Token::Static) => match self.parse_static() {
                    Some(f) => statics.push(f),
                    None => self.panic_top_level(false),
                },
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
            enums,
			statics,
        }
    }
}
