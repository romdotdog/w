use w_codegen::Serializer;
use w_errors::Message;
use w_lexer::token::{AmbiguousOp, BinOp, BinOpVariant, Token};
use w_lexer::Lexer;
use types::{IdentPair, ReferenceKind, Type};

pub mod handler;
mod primaryatom;
mod simpleatom;
mod toplevel;
mod types;
mod symbol_stack;

use handler::{Handler, Status, ImportlessHandler, ImportlessHandlerHandler};
use symbol_stack::SymbolStack;


pub struct Compiler<'ast, H: Handler<'ast>, S: Serializer> {
    session: &'ast H, // TODO: lifetime review
    src_ref: &'ast H::SourceRef,
	
    lex: Lexer<'ast>,
    start: usize,
    end: usize,
    tk: Option<Token<'ast>>,

	//

	module: &'ast mut S,
	symbols: SymbolStack<'ast>
}

enum Take<'ast, T> {
    Next(T),
    Fill(T, Option<Token<'ast>>),
    // self.next() already called
    // dangerous if used improperly
    NoFill(T),
}

use Take::{Fill, Next, NoFill};
use w_utils::span::Span;

impl<'ast, H: ImportlessHandler<'ast>, S: Serializer> Compiler<'ast, ImportlessHandlerHandler<'ast, H>, S> {
    pub fn compile_string(session: &'ast ImportlessHandlerHandler<'ast, H>, module: &'ast mut S, src: &'ast str) -> Self {
        let lex = Lexer::from_str(src);

        let mut compiler = Compiler {
            session,
            src_ref: &(),
            
			lex,
            start: 0,
            end: 0,
            tk: None,

			module,
			symbols: SymbolStack::default()
        };

        compiler.next();
        compiler
    }
}

impl<'ast, H: Handler<'ast>, S: Serializer> Compiler<'ast, H, S> {
    pub fn compile(session: &'ast H, module: &'ast mut S, src_ref: &'ast H::SourceRef) {
        let src = session.get_source(src_ref);
        let mut lex = Lexer::from_str(src);

        // includes
        for (path, start, end) in lex.process_includes() {
            if let Some((new_src_ref, status)) = session.load_source(src_ref, path) {
                match status {
                    Status::NotParsing => {
                        Compiler::compile(session, module, new_src_ref);
                    }
                    Status::CurrentlyParsing => {
                        session.error(
                            src_ref,
                            Message::RecursiveInclude,
                            Span::new(start, end),
                        );
                    }
                    Status::AlreadyParsed => {}
                }
            } else {
                session.error(
                    src_ref,
                    Message::FileNotFound,
                    Span::new(start, end),
                );
            }
        }

        let mut compiler = Compiler {
            session,
            src_ref,
			
            lex,
            start: 0,
            end: 0,
            tk: None,

			module,
			symbol_stack: SymbolStack::default()
        };

        compiler.next();
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

    /// returns true if the current token can begin
    /// an atom
    pub fn can_begin_atom(&self) -> bool {
        matches!(
            self.tk,
            Some(
                // keywords
                Token::Let |
                Token::Loop |
                Token::Br |
                Token::Return |
                Token::If |

                // unary ops
                Token::BinOp(BinOp::Regular(BinOpVariant::Lt)) | // cast
                Token::AmbiguousOp(_) |
                Token::UnOp(_) |
                
                // symbols
                Token::LeftParen |
                Token::LeftBracket |
                
                // literals

                Token::U31(_) |
                Token::U63(_) |
                Token::I32(_) |
                Token::I64(_) |
                Token::U32(_) |
                Token::U64(_) |
                Token::Fxx(_) |
                Token::F64(_) |
                Token::Ident(_) |
                Token::String(_) |
                Token::Char(_) |
                Token::Label(_)
            )
        )
    }

    fn span(&self) -> Span {
        Span::new(self.start, self.end)
    }

    pub(crate) fn error(&self, msg: Message, span: Span) {
        self.session.error(self.src_ref, msg, span);
    }

    /// for owning enum fields
    fn take<T, F>(&mut self, f: F) -> T
    where
        F: FnOnce(&mut Self, Option<Token<'ast>>) -> Take<'ast, T>,
    {
        let t = self.tk.take();
        match f(self, t) {
            Take::Next(t) => {
                self.next();
                t
            }
            Take::Fill(t, tk) => {
                self.tk = tk;
                t
            }
            Take::NoFill(t) => t,
        }
    }

    pub fn expect_ident(&mut self, token_after: &Option<Token>) -> Option<&'ast str> {
        if &self.tk == token_after {
            // struct  {
            //        ^
            let pos = self.start;
            let span = Span::new(pos - 1, pos);
            self.error(Message::MissingIdentifier, span);
            None
        } else {
            self.take(|this, t| {
                Next(match t {
                    // take
                    Some(Token::Ident(s)) => {
                        // struct ident {
                        //        ^^^^^
                        Some(s)
                    }
                    Some(Token::Label(_)) => {
                        // struct $label {
                        //        ^^^^^^
                        this.error(Message::LabelIsNotIdentifier, this.span());
                        None
                    }
                    _ => {
                        // struct ! {
                        //        ^
                        this.error(Message::MalformedIdentifier, this.span());
                        None
                    }
                })
            })
        }
    }

    fn parse_type(&mut self) -> Option<Type> {
        let start = self.start;
		let refkind = match self.tk {
			Some(Token::AmbiguousOp(AmbiguousOp::Ampersand)) => {
				self.next();
				match self.tk {
					Some(Token::Mut) => {
						self.next();
						ReferenceKind::Mutable
					}
					_ => ReferenceKind::Immutable
				}
			}
			_ => ReferenceKind::None
		};

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

                        let end = self.end;
                        self.next();

                        match self.tk {
                            Some(Token::AmbiguousOp(AmbiguousOp::Asterisk)) => {}
                            _ => self.error(
                                Message::TooMuchIndirection,
                                Span::new(asterisk_overflow_start.unwrap(), end),
                            ),
                        }
                    }
                    len += 1;
                }
                Some(ref ch @ (Token::Union | Token::Struct)) => {
                    let is_struct = ch == &Token::Struct;
                    self.next();
                    let body = self.type_body(true)?;
                    let end = body.1.end;
                    /*return Some(Spanned(
                        Type::with_indir(
                            if is_struct {
                                TypeVariant::Struct(body)
                            } else {
                                TypeVariant::Union(body)
                            },
                            indir,
							refkind
                        ),
                        Span::new(start, end),
                    ));*/
					todo!()
                }
                _ => {
                    return self.take(|this, t| match t {
                        Some(Token::Ident(s)) => {
                            let end = this.end;
                            Next(Some(Spanned(
                                Type::with_indir(s.into(), indir, refkind),
                                Span::new(start, end),
                            )))
                        }
                        t => {
                            this.error(Message::MalformedType, this.span());
                            Fill(None, t)
                        }
                    })
                }
            }
        }
    }

    fn parse_decl(&mut self) -> Option<Spanned<Decl<'ast>>> {
        let start = self.start;

        let pair = self.ident_type_pair(false)?;
        let mut end = pair.1.end;
        let rhs = match self.tk {
            Some(Token::BinOp(BinOp::Compound(BinOpVariant::Id))) => {
                self.next();
                let atom = self.atom()?;
                end = atom.1.end;
                Some(Box::new(atom))
            }
            _ => None,
        };

        Some(Spanned(Decl { pair, rhs }, Span::new(start, end)))
    }

    fn ident_type_pair(&mut self, require_type: bool) -> Option<Spanned<IdentPair<'ast>>> {
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

        let ident = self.expect_ident(&Some(Token::Colon));
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
                }
                None
            }
        };

        Some(Spanned(
            IdentPair { mutable, ident, t },
            Span::new(start, end),
        ))
    }

    fn subatom(
        &mut self,
        mut lhs: (S::ExpressionRef, Type),
        min_prec: u8,
    ) -> (S::ExpressionRef, Type) {
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
                //lhs = Spanned(Atom::BinOp(Box::new(lhs), t, Box::new(rhs)), span);
				todo!()
            } else {
                break;
            }
        }
        Some(lhs)
    }

    pub fn atom(&mut self, contextual_type: Option<Type>) -> (S::ExpressionRef, Type) {
        let lhs = self.primaryatom()?;
        self.subatom(lhs, 0)
    }
}
