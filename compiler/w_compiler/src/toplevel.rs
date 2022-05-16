use crate::{
    registry::{self, Item},
    types::{
        typ::{Type, VOID},
        IdentPair,
    },
};

use super::{Compiler, Fill, Handler, Next};
use std::{
    collections::{hash_map::Entry, HashMap},
    convert::TryInto,
};
use w_codegen::{Serializer, WASMType};
use w_errors::Message;
use w_lexer::token::{BinOp, BinOpVariant, Token};
use w_utils::span::Span;

impl<'ast, H: Handler<'ast>, S: Serializer> Compiler<'ast, H, S> {
    pub fn can_begin_toplevel(&self) -> bool {
        matches!(
            self.tk,
            Some(
                Token::Export
                    | Token::Fn
                    | Token::Struct
                    | Token::Union
                    | Token::Enum
                    | Token::Static
            )
        )
    }

    // static

    pub(crate) fn parse_static(&mut self) {
        let start = self.start;
        assert_eq!(self.tk, Some(Token::Static));
        self.next();

        if let Some(Token::Fn) = self.tk {
            return self.function(false, true);
        }

        let decl = self.parse_decl().unwrap();

        if let Some(Token::Semicolon) = self.tk {
            self.next();
        } else {
            self.error(Message::MissingSemicolon, self.span());
        }

        // Some(Spanned(TopLevel::Static(decl.0), Span::new(start, end)))
        todo!()
    }

    // enums

    fn enum_body(&mut self) -> Option<HashMap<&'ast str, i64>> {
        let mut discriminant = 0;
        let start = self.start;
        let mut h = HashMap::new();
        self.next();

        let end = loop {
            macro_rules! bracketcomma {
                ($b: block, $c: block) => {
                    match self.tk {
                        Some(Token::RightBracket) => {
                            $b;
                            let end_ = self.end;
                            self.next();
                            break end_;
                        }
                        Some(Token::Comma) => {
                            $c;
                            self.next();
                            continue;
                        }
                        _ => {}
                    }
                };
            }

            bracketcomma!({}, {
                self.error(
                    Message::MissingIdentifier,
                    Span::new(self.start - 1, self.start),
                );
            });

            // take identifier
            let (sident, s) = self.take(|this, t| match t {
                // take
                Some(Token::Ident(s)) => Next(Some((this.span(), s))),
                t => Fill(None, t),
            })?;

            bracketcomma!(
                {
                    match h.entry(s) {
                        Entry::Vacant(e) => {
                            e.insert(discriminant);
                        }
                        Entry::Occupied(_) => {
                            self.error(Message::DuplicateEnumField, sident);
                        }
                    }
                },
                {
                    match h.entry(s) {
                        Entry::Vacant(e) => {
                            e.insert(discriminant);
                        }
                        Entry::Occupied(_) => {
                            self.error(Message::DuplicateEnumField, sident);
                        }
                    }

                    discriminant += 1;
                }
            );

            if let Some(Token::BinOp(BinOp(true, BinOpVariant::Id))) = self.tk {
                self.next();
                discriminant = self.take(|this, t| {
                    // take
                    let i = match t {
                        Some(Token::Integer(n)) => n,
                        Some(Token::Uinteger(n)) => {
                            if let Ok(n) = n.try_into() {
                                n
                            } else {
                                match h.entry(s) {
                                    Entry::Vacant(e) => {
                                        e.insert(discriminant);
                                    }
                                    Entry::Occupied(_) => {
                                        this.error(Message::DuplicateEnumField, sident);
                                    }
                                }

                                this.error(Message::IntegerNoFit, this.span());
                                return Next(discriminant + 1);
                            }
                        }
                        t => {
                            this.error(Message::MissingInteger, this.span());
                            return Fill(discriminant + 1, t);
                        }
                    };

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
                });

                bracketcomma!({}, {});
            }

            self.error(Message::UnexpectedToken, self.span());
            return None;
        };

        Some(h)
    }

    pub fn parse_enum(&mut self) {
        let start = self.start;
        debug_assert_eq!(self.tk, Some(Token::Enum));
        self.next();

        let name = self.expect_ident(&Some(Token::LeftBracket)).unwrap(); // TODO
        if let Some(Token::LeftBracket) = self.tk {
            let fields = self.enum_body().unwrap(); // TODO
            self.registry.push(name, Item::Enum(fields));
        } else {
            self.error(Message::MissingOpeningBracket, self.span());
            todo!()
        }
    }

    // structs

    pub(crate) fn type_body(&mut self, allow_no_trailing_semi: bool) -> Vec<IdentPair> {
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
                let pair = self.ident_type_pair().unwrap(); // TODO
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
                        todo!();
                    }
                    _ => {
                        // mut ident: type }
                        //                ^
                        // mut ident: type}
                        //                ^
                        //let last_pos = v.last().unwrap().1.end + 1;
                        //self.error(Message::MissingSemicolon, Span::new(last_pos, last_pos + 1));
                        todo!();
                    }
                }
            };

            v
        } else {
            self.error(Message::MissingOpeningBracket, self.span());
            todo!()
        }
    }

    pub fn struct_or_union(&mut self, is_struct: bool) {
        let start = self.start;
        debug_assert!(matches!(self.tk, Some(Token::Struct | Token::Union)));
        self.next();

        let name = self.expect_ident(&Some(Token::LeftBracket));
        let fields = self.type_body(false);
        /*let end = fields.1.end;
        Some(Spanned(
            if is_struct {
                TopLevel::Struct(name, fields)
            } else {
                TopLevel::Union(name, fields)
            },
            Span::new(start, end),
        ))*/
        todo!()
    }

    // functions

    pub fn function(&mut self, exported: bool, static_: bool) {
        // TODO: lambda lifting

        let start = self.start;
        debug_assert_eq!(self.tk, Some(Token::Fn));
        self.next();

        let name = self.expect_ident(&Some(Token::LeftParen)).unwrap(); // TODO
        let paren_end = match self.tk {
            Some(Token::LeftParen) => {
                self.next();
                self.end
            }
            _ => {
                self.error(Message::MissingOpeningParen, self.span());
                todo!();
            }
        };

        let mut params = Vec::new();

        match self.tk {
            Some(Token::RightParen) => self.next(),
            Some(Token::Ident(_) | Token::Mut) => loop {
                params.push(self.ident_type_pair().unwrap()); // TODO
                match self.tk {
                    Some(Token::Comma) => self.next(),
                    Some(Token::RightParen) => {
                        self.next();
                        break;
                    }
                    _ => {
                        // fn ident(mut ident: type
                        //                         ^
                        //let pos = params[params.len() - 1].1.end;
                        //self.error(Message::MissingClosingParen, Span::new(pos, pos + 1));
                        todo!();
                    }
                }
            },
            _ => {
                // fn ident(
                //          ^
                //self.error(
                //    Message::MissingClosingParen,
                //    Span::new(paren_end, paren_end + 1),
                //);
                //return None;
                todo!();
            }
        }

        let return_type = match self.tk {
            Some(Token::Colon) => {
                self.next();
                self.parse_type().unwrap()
            }
            _ => VOID,
        };

        let atom = self.atom();
        let ret = atom
            .compile(&mut self.module, Some(return_type))
            .unwrap_or_else(|| {
                self.error(Message::TypeMismatch, self.span());
                self.unreachable_expr()
            });

        let vars = self.flow.vars();
        let vars_hack: Vec<(&str, WASMType)> = vars.iter().map(|(s, t)| (s.as_str(), *t)).collect(); // TODO

        // TODO: exports, mangling
        self.module.add_function(
            name,
            params.iter().map(|p| (p.ident, p.typ.resolve())).collect(),
            match return_type {
                t if t != VOID => vec![t.resolve()],
                _ => Vec::new(),
            },
            vars_hack,
            ret.0,
        );
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

    pub fn panic_top_level(&mut self) {
        loop {
            if self.can_begin_toplevel() {
                break;
            }

            match self.tk {
                None => break,
                Some(Token::LeftBracket) => {
                    self.next();
                    self.skip_bracket();
                }
                _ => self.next(),
            }
        }
    }

    pub fn parse_toplevel(&mut self) {
        match self.tk {
            Some(Token::Export) => {
                self.next();
                self.function(true, false)
            }
            Some(Token::Fn) => self.function(false, false),
            Some(Token::Struct) => self.struct_or_union(true),
            Some(Token::Union) => self.struct_or_union(false),
            Some(Token::Enum) => self.parse_enum(),
            Some(Token::Static) => self.parse_static(),
            _ => todo!(),
        }
    }

    // main

    pub fn parse(mut self, report_finish: bool) -> S {
        loop {
            if self.tk.is_none() {
                break;
            }

            self.parse_toplevel();
            //if !self.parse_toplevel() {
            //    self.panic_top_level();
            //}
        }
        if report_finish {
            self.session.finish(self.src_ref);
        }
        self.module
    }
}
