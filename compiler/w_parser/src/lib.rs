use w_lexer::{AmbiguousOp, BinOp, BinOpVariant, Lexer, Token};
use w_ast::{Atom, IdentPair, Indir, Span, Spanned, Type, TypeVariant};
use w_errors::Message;


mod handler;
mod primaryatom;
mod simpleatom;
mod toplevel;

pub use handler::Handler;

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

enum Take<T> {
	Next(T),
	Fill(T, Option<Token>),
	// self.next() already called
	// dangerous if used improperly
	NoFill(T)
}

use Take::{Fill, Next, NoFill};

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

    /// returns true if the current token can begin
    /// an atom
    pub fn can_begin_atom(&self) -> bool {
        matches!(
            self.tk,
            Some(
                // keywords
                Token::Let  |
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
                Token::Float(_) |
                Token::Integer(_) |
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
        self.session.error(&self.src_ref, msg, span);
    }

    /// for owning enum fields
    fn take<T, F>(&mut self, f: F) -> T
	where 
		F: FnOnce(&mut Self, Option<Token>) -> Take<T> {
		let t = self.tk.take();
        match f(self, t) {
            Take::Next(t) => {
				self.next();
				t
			},
            Take::Fill(t, tk) => {
				self.tk = tk;
				t
			},
			Take::NoFill(t) => t
        }
    }

	pub fn expect_ident(&mut self, token_after: &Option<Token>) -> Spanned<String> {
		if &self.tk == token_after {
            // struct  {
            //        ^
            let pos = self.start;
            let span = Span::new(pos - 1, pos);
            self.error(Message::MissingIdentifier, span);
            Spanned("<unknown>".to_owned(), span)
        } else {
            self.take(|this, t| Next(match t {
                // take
                Some(Token::Ident(s)) => {
                    // struct ident {
                    //        ^^^^^
                    Spanned(s, this.span())
                }
                Some(Token::Label(s)) => {
                    // struct $label {
                    //        ^^^^^^
                    let span = this.span();
                    this.error(Message::LabelIsNotIdentifier, span);
                    Spanned(format!("${}", s), span)
                }
                _ => {
                    // struct ! {
                    //        ^
                    let span = this.span();
                    this.error(Message::MalformedIdentifier, span);
                    Spanned("<unknown>".to_owned(), span)
                }
            }))
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
                    return Some(Spanned(
                        Type::with_indir(
                            if is_struct {
                                TypeVariant::Struct(body)
                            } else {
                                TypeVariant::Union(body)
                            },
                            indir,
                        ),
                        Span::new(start, end),
                    ));
                }
                _ => return self.take(|this, t| {
					match t {
						Some(Token::Ident(s)) => {
							let end = this.end;
							Next(Some(Spanned(
								Type::with_indir(s.into(), indir),
								Span::new(start, end),
							)))
						}
						t => {
							this.error(Message::MalformedType, this.span());
							Fill(None, t)
						}
					}
				}),
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
}
