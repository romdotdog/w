/*
    BIP - Below Ident Priority
    AIP - Above Ident Priority
*/

mod token;

use std::iter::FromIterator;
pub use token::{AmbiguousOp, BinOp, BinOpVariant, Token, UnOp};

pub struct Lexer<'a>
{
    buffer: &'a [u8],
    done: bool,
    pos: usize,
}

impl<'a> Lexer<'a>
{
    pub fn new(buffer: &'a [u8]) -> Self {
        Lexer {
            buffer,
			pos: 0,
            done: false,
        }
    }

	fn skip(&mut self) {
		self.buffer = match self.buffer.first() {
			Some(0x00..=0x7F) => &self.buffer[1..],
			Some(0x80..=0xBF) => panic!("misaligned"),
			Some(0xC0..=0xDF) => &self.buffer[2..],
			Some(0xE0..=0xEF) => &self.buffer[3..],
			Some(0xF0..=0xF7) => &self.buffer[4..],
			_ => panic!("not unicode")
		}
	}

    fn parse_string(&mut self) -> String {
        let mut res = String::new();

        loop {
            match self.advance() {
                Some('\\') => {
                    // check the first character
                    let header_char = self.advance().expect("expected escape character, got <eof>");

                    if let Some(radix) = char_to_radix(header_char) {
                        let radix = u32::from(radix);

                        //  ^ \x6e, \d71
                        // if there is no applicable digit after, then treat as normal escape
                        match self.advance() {
                            Some(header_digit) if header_digit.is_digit(radix) => {
                                // go on parsing as normal
                                let mut num = header_digit.to_string();
                                loop {
                                    match self.advance() {
                                        Some(t) if t.is_digit(radix) => {
                                            num.push(t);
                                        }
                                        c1 => {
                                            self.backtrack(c1);
                                            break;
                                        }
                                    }
                                }

                                let codepoint = u32::from_str_radix(&num, radix);
                                assert!(codepoint.is_ok(), "max escape value is {}", u32::MAX);

                                let escaped = std::char::from_u32(codepoint.unwrap());
                                assert!(
                                    escaped.is_some(),
                                    "invalid codepoint, cannot add \\{}{} as part of escape",
                                    header_char,
                                    num
                                );

                                // push the codepoint
                                res.push(escaped.unwrap());
                            }
                            t => self.backtrack(t),
                        }
                    } else {
                        res.push(match header_char {
                            't' => '\t',
                            'r' => '\r',
                            'n' => '\n',
                            _ => header_char, // is not an escape
                        });
                    }
                }
                Some('"') => {
                    break;
                }
                Some(c) => res.push(c),
                None => panic!("incomplete string literal"),
            }
        }

        res
    }

    fn parse_ident(&mut self, ident: &mut String) {
        fn start_of_token(c: char) -> bool {
            matches!(
                c,
                ':' | ';' | ',' | '.' | 
                // sep
                '{'| '}' | '(' | ')' | '[' | ']' | 
                // sep
                '*' | '/' | '%' | '^' | '&' | '|' | '~' |
                // sep
                '>'  | '<' | '=' | '!' | '+' | '-' |
                // sep
                '\'' | '"'
            )
        }

        loop {
			self.advance();
			
            if c2.is_whitespace() {
                break;
            }

            if start_of_token(c2) {
                self.backtrack(Some(c2));
                break;
            }

            ident.push(c2);
        }
    }

    fn parse_char(&mut self) -> Token {
        let c2 = self.advance();
        match self.advance() {
            Some('\'') => Token::Char(c2.unwrap()),
            t => {
                // Errors are idents
                let mut ident = match t {
                    Some(t) => String::from_iter(&['\'', c2.unwrap(), t]),
                    None => match c2 {
                        Some(t) => String::from_iter(&['\'', t]),
                        None => "\'".to_string(),
                    },
                };
                self.parse_ident(&mut ident);
                Token::Ident(ident)
            }
        }
    }

	 fn try_take_equals(&mut self) -> bool {
        match self.byte {
            Some(b'=') => {
				self.advance(true);
				true
			},
            _ => false
        }
    }

    fn try_aip(&mut self, c: u8) -> Option<Token> {
        macro_rules! op {
            (@ambiguous $t: ident) => {{
                let amb = AmbiguousOp::$t;
                match self.try_take_equals() {
                    true => Token::BinOp(BinOp::Compound(amb.binary())),
                    false => Token::AmbiguousOp(amb),
                }
            }};

            (@binary $t: ident) => {
                Token::BinOp(match self.try_take_equals() {
                    true => BinOp::Compound(BinOpVariant::$t),
                    false => BinOp::Regular(BinOpVariant::$t),
                })
            };

            (@compound $t: ident) => {
                Token::BinOp(BinOp::Compound(BinOpVariant::$t))
            };

            (@simple $t: ident) => {
                Token::BinOp(BinOp::Regular(BinOpVariant::$t))
            };

            (@unary $t: ident) => {
                Token::UnOp(UnOp::$t)
            };
        }

        Some(match c {
            b':' => {self.advance(true); Token::Colon},
            b';' => {self.advance(true); Token::Semicolon},
            b',' => {self.advance(true); Token::Comma},
            b'{' => {self.advance(true); Token::LeftBracket},
            b'}' => {self.advance(true); Token::RightBracket},
            b'(' => {self.advance(true); Token::LeftParen},
            b')' => {self.advance(true); Token::RightParen},
            b'[' => {self.advance(true); Token::LeftSqBracket},
            b']' => {self.advance(true); Token::RightSqBracket},

            b'*' => {self.advance(true); op!(@ambiguous Asterisk)},
            b'/' => {self.advance(true); op!(@binary Div)},

            b'%' => {self.advance(true); op!(@binary Mod)},
            b'^' => {self.advance(true); op!(@binary Xor)},
            b'&' => {self.advance(true); op!(@ambiguous Ampersand)},
            b'|' => {self.advance(true); op!(@binary Or)},
            b'~' => {self.advance(true); op!(@unary BNot)},

            b'\'' => self.parse_char(),
            b'"' => Token::String(self.parse_string()),

            b'>' | b'<' | b'=' | b'!' | b'+' | b'-' => {
				self.advance(true);
				// two char ops
                let c2 = self.byte;
                match (c, c2) {
                    (b'>', Some(b'=')) => {self.advance(true); op!(@binary Ge)},
                    (b'<', Some(b'=')) => {self.advance(true); op!(@binary Le)},
                    (b'<', Some(b'<')) => {self.advance(true); op!(@binary Lsh)},
                    (b'>', Some(b'>')) => {self.advance(true); op!(@binary Rsh)},
                    (b'<', _) => op!(@simple Lt),
                    (b'>', _) => op!(@simple Gt),

                    (b'=', Some(b'=')) => {self.advance(true); op!(@binary EqC)},
                    (b'=', _) => op!(@compound Id),

                    (b'!', Some(b'=')) => {self.advance(true); op!(@binary Neq)},
                    (b'!', _) => op!(@unary LNot),

                    (b'+', Some(b'+')) => {self.advance(true); op!(@unary Inc)},
                    (b'+', _) => {
                        op!(@ambiguous Plus)
                    }

                    (b'-', Some(b'-')) => {self.advance(true); op!(@unary Dec)},
                    (b'-', Some(b'>')) => {self.advance(true); Token::Arrow},
                    (b'-', _) => op!(@ambiguous Minus),

                    (_, _) => panic!(),
                }
            }
            _ => return None,
        })
    }

    fn try_bip(&mut self, b: u8) -> Option<Token> {
        Some(match b {
            b'.' | b'0'..=b'9' => {
				self.advance(true);
                let mut radix = 10_u8;
				
                if b == b'0' {
                    radix = match self.byte {
						Some(b'b') => {self.advance(true); 2}
						Some(b'q') => {self.advance(true); 4}
						Some(b'h') => {self.advance(true); 6}
                        Some(b'o') => {self.advance(true); 8}
						Some(b'x') => {self.advance(true); 16}
						_ => 10,
                        None => return Some(Token::Integer(0)),
                    }
                }

				let mut num = 0;

				loop {
					match self.byte {
						Some(b @ b'0'..=b'9') => {
							let v = b & 0xf;
							if v < radix {
								num = (num * radix) + v;
							} else {
								break;
							}
						}
						Some(b @ b'A'..=b'F') => {
							let v = b - b'A' + 10;
							if v < radix {
								num = (num * radix) + v;
							} else {
								break;
							}
						}
						Some(b @ b'a'..=b'f') => {
							let v = b - b'a' + 10;
							if v < radix {
								num = (num * radix) + v;
							} else {
								break;
							}
						}
					}

					self.advance(true);
				}

                self.skip_digits(&mut num, radix.into());

                let seen_period = if c == '.' {
                    true
                } else if radix == 10_u8 {
                    match self.advance() {
                        Some('.') => {
                            num.push('.');
                            self.skip_digits(&mut num, 10);
                            true
                        }
                        t => {
                            self.backtrack(t);
                            false
                        }
                    }
                } else {
                    false
                };

                match self.advance() {
                    Some('E' | 'e') if radix == 10_u8 => {
                        match self.advance() {
                            Some(ch @ ('+' | '-')) => num.push(ch),
                            ch @ Some(_) => self.backtrack(ch),
                            None => todo!(),
                        }
                        self.skip_digits(&mut num, 10);
                    }
                    ch => self.backtrack(ch),
                }

                if seen_period {
                    Token::Float(0)
                } else {
                    Token::Integer(0)
                }
            }
            _ => return self.try_aip(b),
        })
    }

	#[inline(always)]
    fn advance(&mut self, next_pos: bool) {
        self.byte = self.stream.next();
		if next_pos {
			self.pos += 1;
		}
    }

	fn is_whitespace(&mut self) -> bool {
		// check if this is whitespace
		let c = match self.buffer.first() {
			Some(c) => c,
			None => return false
		};

		match c {
			0x20 | // space
			0x09 | // tab
			0x0A | // \n
			0x0B | // vertical tab
			0x0C | // form feed
			0x0D   // \r
			=> {
				return true;
			},

			// U+0085 - NEXT LINE (NEL)
			// C2 85 - 11000010 10000101
			0xC2 => {
				self.advance(false);
				match self.byte.unwrap_or(0) {
					0x85 => {
						self.advance(true);
						continue;
					}
					b => ident!(0xC2, b)
				}
			}

			// U+200E - LEFT-TO-RIGHT MARK
			// E2 80 8E - 11100010 10000000 10001110
			// U+200F - RIGHT-TO-LEFT MARK
			// E2 80 8F - 11100010 10000000 10001111
			// U+2028 - LINE SEPARATOR
			// E2 80 A8 - 11100010 10000000 10101000
			// U+2029 - PARAGRAPH SEPARATOR
			// E2 80 A9 - 11100010 10000000 10101001
			0xE2 => {
				self.advance(false);
				let b1 = self.byte.unwrap_or(0);
				self.advance(false);
				let b2 = self.byte.unwrap_or(0);
				match (b1, b2) {
					(0x80, 0x8E | 0x8F | 0xA8 | 0xA9) => {
						self.advance(true);
						continue 'm;
					}
					(_, _) => ident!(0xE2, b1, b2)
				}
			}
			_ => break c
		}
	}

	fn next_token(&mut self) -> Option<(Token, usize, usize)> {
		let mut start = 0;
		let b = 'm: loop {
            match self.byte {
                Some(b'/') => {
					self.advance(true);
					match self.byte {
						// single comment
						Some(b'/') => loop {
							self.advance();
							match self.byte {
								Some(b'\n') => break,
								None => return None,
								_ => {}
							}
						},
						// multi-line comment
						Some(b'*') => loop {
							self.advance();
							match self.byte {
								Some(b'*') => {
									self.advance();
									match self.byte {
										Some(b'/') => break,
										_ => {}
									}
								},
								None => todo!(),
								_ => {}
							}
						},
						// found /=
						Some(b'=') => {
							self.advance(true);
							return Some((Token::BinOp(BinOp::Compound(BinOpVariant::Div)), self.pos - 2, self.pos));
						}
						// just interpret as /
						_ => return Some((Token::BinOp(BinOp::Regular(BinOpVariant::Div)), self.pos - 1, self.pos))
					}
				},
                Some(c) => {
					
				},
                None => return None,
            }
        };


		let start = self.pos;
        self.try_bip(c)
            .or_else(|| {
                let is_label = c == '$';
                let mut ident = if is_label {
                    String::new()
                } else {
                    c.to_string()
                };

                self.parse_ident(&mut ident);

                Some(if is_label {
                    if ident.is_empty() {
                        Token::Ident("$".to_owned())
                    } else {
                        Token::Label(ident)
                    }
                } else {
                    keyword(ident)
                })
            })
            .map(|t| (t, start, self.pos + 1))
	}
}

fn keyword(s: String) -> Token {
    match s.as_str() {
        "fn" => Token::Fn,
        "return" => Token::Return,
        "if" => Token::If,
        "else" => Token::Else,
        "let" => Token::Let,
        "mut" => Token::Mut,
        "loop" => Token::Loop,
        "br" => Token::Br,
        "struct" => Token::Struct,
        "union" => Token::Union,
        "enum" => Token::Enum,
        _ => Token::Ident(s),
    }
}

impl<'a> Iterator for Lexer<'a>
{
    type Item = (Token, usize, usize);

    fn next(&mut self) -> Option<(Token, usize, usize)> {
        if self.done {
            return None;
        }

		let t = self.next_token();
		self.done = t.is_none();
		t
    }
}
