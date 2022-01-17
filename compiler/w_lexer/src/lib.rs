use std::ptr;

use dec2flt::{
    convert_sign_and_mantissa,
    parse::{parse_decimal, parse_number},
};

pub mod token;
use token::{AmbiguousOp, BinOp, BinOpVariant, Token, UnOp};

// a reduced version of the rust std string-to-float parsing library
mod dec2flt;
pub struct Lexer<'a> {
    buffer: &'a [u8],
    pos: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(buffer: &'a [u8]) -> Self {
        Lexer { buffer, pos: 0 }
    }

    unsafe fn step_by(&mut self, n: usize) {
        self.buffer = self.buffer.get_unchecked(n..);
    }

    unsafe fn step(&mut self) {
        self.step_by(1);
    }

    fn parse_radix(&mut self, radix: u8) -> Option<u64> {
        assert!(radix <= 36, "parse_radix: radix is too high (maximum 36)");
        let mut x: u64 = 0;
        let mut not_overflown = true;
        while let Some(&c) = self.buffer.first() {
            let mut v = c.wrapping_sub(b'0');
            if radix > 10 && v > 10 {
                // Force the 6th bit to be set to ensure ascii is lower case.
                v = (c | 0b10_0000).wrapping_sub(b'a').saturating_add(10);
            }
            if v < radix {
                if not_overflown {
                    x
                        .checked_mul(u64::from(radix))
                        .and_then(|x| x.checked_add(u64::from(v)))
						.map_or_else(|| not_overflown = false, |nx| x = nx);
                }

                unsafe {
                    self.step();
                }
            } else {
                break;
            }
        }

        if not_overflown {
            Some(x)
        } else {
            None
        }
    }

    fn check_radix(&self) -> Option<u8> {
        if let Some(&radixc) = self.buffer.get(1) {
            return char_to_radix(radixc);
        }
        None
    }

    fn try_bip(&mut self, b: u8) -> Option<Token> {
        let start = self.buffer;

        let negative = match b {
            b'-' => {
				unsafe { self.step() };
                true
            }
            _ => false,
        };

        match self.buffer.first() {
            Some(b'1'..=b'9') => self.load_number(parse_number(self.buffer, negative)),
            Some(b'.') => self.load_number(parse_decimal(start, negative)),
            Some(b'0') =>
            {
                #[allow(clippy::option_if_let_else)]
                if let Some(radix) = self.check_radix() {
                    Some(match self.parse_radix(radix) {
                        Some(mantissa) => convert_sign_and_mantissa(negative, mantissa),
                        None => Token::Overflown,
                    })
                } else {
                    self.load_number(parse_number(start, negative))
                }
            }
            Some(_) => {
				self.buffer = start;
				self.try_aip(b)
			},
			None => {
				self.buffer = start;
				None
			}
        }
    }

    fn skip(&mut self, b: u8) {
        self.buffer = match b {
            0x00..=0x7F => &self.buffer[1..],
            0x80..=0xBF => panic!("misaligned"),
            0xC0..=0xDF => &self.buffer[2..],
            0xE0..=0xEF => &self.buffer[3..],
            0xF0..=0xF7 => &self.buffer[4..],
            _ => panic!("not unicode"),
        }
    }

    fn parse_string(&mut self) -> Token {
        unsafe { self.step() };
        self.pos += 1;
        let start = self.buffer;
        loop {
            match self.buffer.first() {
                Some(b'\\') => {
                    unsafe { self.step() };
                    self.pos += 1;
                    if let Some(b'\"') = self.buffer.first() {
                        unsafe { self.step() };
                        self.pos += 1;
                    }
                }
                Some(b'\"') => {
                    unsafe { self.step() };
                    self.pos += 1;
                    break;
                }
                Some(&b) => self.skip(b),
                None => break,
            }
        }
        let bytes_read = start.len() - self.buffer.len();
        Token::String(String::from_utf8_lossy(&start[0..bytes_read]).to_string())
    }

    fn parse_char(&mut self) -> Option<Token> {
        unsafe { self.step() };
        self.pos += 1;

        let r = match self.buffer.first() {
            Some(b'\\') => {
                unsafe { self.step() };
                self.pos += 1;

                self.buffer.first().and_then(|b| {
                    unsafe { self.step() };
                    self.pos += 1;
                    // TODO: add more escapes
                    match b {
                        b't' => Some(Token::Char('\t')),
                        b'r' => Some(Token::Char('\r')),
                        b'n' => Some(Token::Char('\n')),
                        _ => None,
                    }
                })
            }
            Some(&b) => {
                /// Mask of the value bits of a continuation byte.
                const CONT_MASK: u8 = 0b0011_1111;
                fn utf8_acc_cont_byte(ch: u32, byte: u8) -> u32 {
                    (ch << 6) | u32::from(byte & CONT_MASK)
                }

                // get codepoint from following bytes
                unsafe { self.step() };
                self.pos += 1;
                let codepoint = if b < 128 {
                    u32::from(b)
                } else {
                    let init = u32::from(b & 0x1F);
                    let y = self.buffer.first().copied().unwrap_or(0);
                    let mut ch = utf8_acc_cont_byte(init, y);
                    if b >= 0xE0 {
                        let z = self.buffer.get(1).copied().unwrap_or(0);
                        let y_z = utf8_acc_cont_byte(u32::from(y & CONT_MASK), z);
                        ch = init << 12 | y_z;
                        if b >= 0xF0 {
                            let w = self.buffer.get(2).copied().unwrap_or(0);
                            ch = (init & 7) << 18 | utf8_acc_cont_byte(y_z, w);
                        }
                    }
                    ch
                };

                // TODO: unchecked
                Some(Token::Char(std::char::from_u32(codepoint).unwrap()))
            }
            None => return None,
        };

        if let Some(b'\'') = self.buffer.first() {
            unsafe { self.step() };
            self.pos += 1;
            r
        } else {
            None
        }
    }

    fn try_aip(&mut self, b: u8) -> Option<Token> {
        macro_rules! token {
            (@ambiguous $t: ident, $n: expr) => {
                token!(Token::AmbiguousOp(AmbiguousOp::$t), $n)
            };

            (@compound $t: ident, $n: expr) => {
                token!(Token::BinOp(BinOp::Compound(BinOpVariant::$t)), $n)
            };

            (@simple $t: ident, $n: expr) => {
                token!(Token::BinOp(BinOp::Regular(BinOpVariant::$t)), $n)
            };

            (@unop $t: ident, $n: expr) => {
                token!(Token::UnOp(UnOp::$t), $n)
            };

            (@token $t: ident, $n: expr) => {
                token!(Token::$t, $n)
            };

            ($t: expr, $n: expr) => {{
                unsafe { self.step_by($n) };
                self.pos += $n;
                $t
            }};
        }

        Some(match b {
            b':' => token!(@token Colon, 1),
            b';' => token!(@token Semicolon, 1),
            b',' => token!(@token Comma, 1),
            b'{' => token!(@token LeftBracket, 1),
            b'}' => token!(@token RightBracket, 1),
            b'(' => token!(@token LeftParen, 1),
            b')' => token!(@token RightParen, 1),
            b'[' => token!(@token LeftSqBracket, 1),
            b']' => token!(@token RightSqBracket, 1),
            b'~' => token!(@unop BNot, 1),
            b'\'' => self.parse_char()?,
            b'"' => self.parse_string(),
            _ => match (b, self.buffer.get(1)) {
                (b'+', Some(b'+')) => token!(@unop Inc, 2),
                (b'+', Some(b'=')) => token!(@compound Add, 2),
                (b'+', _) => token!(@ambiguous Plus, 1),

                (b'-', Some(b'-')) => token!(@unop Dec, 2),
                (b'-', Some(b'>')) => token!(@token Arrow, 2),
                (b'-', Some(b'=')) => token!(@compound Sub, 2),
                (b'-', _) => token!(@ambiguous Minus, 1),

                (b'*', Some(b'=')) => token!(@compound Mul, 2),
                (b'*', _) => token!(@ambiguous Asterisk, 1),

                (b'/', Some(b'=')) => token!(@compound Div, 2),
                (b'/', _) => token!(@simple Div, 1),

                (b'%', Some(b'=')) => token!(@compound Mod, 2),
                (b'%', _) => token!(@simple Mod, 1),

                (b'^', Some(b'=')) => token!(@compound Xor, 2),
                (b'^', _) => token!(@simple Xor, 1),

                (b'&', Some(b'=')) => token!(@compound And, 2),
                (b'&', _) => token!(@ambiguous Ampersand, 1),

                (b'|', Some(b'=')) => token!(@compound Or, 2),
                (b'|', _) => token!(@simple Or, 1),

                (_, c) => match (b, c, self.buffer.get(2)) {
                    (b'>', Some(b'>'), Some(b'=')) => token!(@compound Rsh, 3),
                    (b'>', Some(b'>'), _) => token!(@simple Rsh, 2),
                    (b'>', Some(b'='), Some(b'=')) => token!(@compound Ge, 3),
                    (b'>', Some(b'='), _) => token!(@simple Ge, 2),
                    (b'>', _, _) => token!(@simple Gt, 1),

                    (b'<', Some(b'<'), Some(b'=')) => token!(@compound Lsh, 3),
                    (b'<', Some(b'<'), _) => token!(@simple Lsh, 2),
                    (b'<', Some(b'='), Some(b'=')) => token!(@compound Le, 3),
                    (b'<', Some(b'='), _) => token!(@simple Le, 2),
                    (b'<', _, _) => token!(@simple Lt, 1),

                    (b'=', Some(b'='), Some(b'=')) => token!(@compound EqC, 3),
                    (b'=', Some(b'='), _) => token!(@simple EqC, 2),
                    (b'=', _, _) => token!(@compound Id, 1),

                    (b'!', Some(b'='), Some(b'=')) => token!(@compound Neq, 3),
                    (b'!', Some(b'='), _) => token!(@simple Neq, 2),
                    (b'!', _, _) => token!(@unop LNot, 1),

                    _ => return None,
                },
            },
        })
    }

    fn is_whitespace(&mut self, c: u8) -> bool {
        match c {
			0x20 | // space
			0x09 | // tab
			0x0A | // \n
			0x0B | // vertical tab
			0x0C | // form feed
			0x0D   // \r
			=> {
				unsafe { self.step() };
				self.pos += 1;
				true
			},

			// U+0085 - NEXT LINE (NEL)
			// C2 85 - 11000010 10000101
			0xC2 => {
				match self.buffer.get(1) {
					Some(0x85) => {
						unsafe { self.step_by(2) };
						self.pos += 1;
						true
					}
					_ => false
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
				match (self.buffer.get(1), self.buffer.get(2)) {
					(Some(0x80), Some(0x8E | 0x8F | 0xA8 | 0xA9)) => {
						unsafe { self.step_by(3) };
						self.pos += 1;
						true
					}
					(_, _) => false
				}
			}
			_ => false
		}
    }

    fn skip_comments_and_whitespace(&mut self) -> bool {
        loop {
            match self.buffer.first() {
                Some(b'/') => {
                    match self.buffer.get(1) {
                        // single comment
                        Some(b'/') => {
                            unsafe { self.step_by(2) };
                            self.pos += 2;
                            loop {
                                self.pos += 1;
								match self.buffer.first() {
									Some(b'\n') => {
										unsafe { self.step() };
										break;
									}
									Some(&b) => self.skip(b),
									None => break
								}                                
                            }
                        }
                        // multi-line comment
                        Some(b'*') => {
                            unsafe { self.step_by(2) };
                            self.pos += 2;
                            loop {
								match self.buffer.first() {
									Some(b'*') => {
										self.pos += 1;
										unsafe { self.step() };
										if let Some(b'/') = self.buffer.first() {
											self.pos += 1;
											unsafe { self.step() };
											break;
										}
									}
									Some(&b) => self.skip(b),
									None => break
								}
                            }
                        }
                        _ => break true,
                    };
                }
                Some(&c) if self.is_whitespace(c) => {}
                Some(_) => break true,
                None => break false,
            }
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = (Token, usize, usize);

    fn next(&mut self) -> Option<Self::Item> {
		fn start_of_token(c: u8) -> bool {
			matches!(
				c,
				b':' | b';' | b',' | b'.' | 
				// sep
				b'{' | b'}' | b'(' | b')' | b'[' | b']' | 
				// sep
				b'*' | b'/' | b'%' | b'^' | b'&' | b'|' | b'~' |
				// sep
				b'>'  | b'<' | b'=' | b'!' | b'+' | b'-' |
				// sep
				b'\'' | b'"'
			)
		}

        if !self.skip_comments_and_whitespace() {
            return None;
        }

        let start = self.buffer;
		let start_pos = self.pos;
        let &c = start.first()?;
		let t = if let Some(t) = self.try_bip(c) {
			t
		} else {
			let is_label = c == b'$';
			while let Some(&b) = self.buffer.first() {
				if self.is_whitespace(b) {
					break;
				}

				if start_of_token(b) {
					break;
				}

				self.skip(b);
				self.pos += 1;
			}

			let len = start.len() - self.buffer.len();
			if is_label {
				if len == 1 {
					Token::Ident('$'.to_string())
				} else {
					Token::Label(String::from_utf8_lossy(&start[1..len]).to_string())
				}
			} else {
				keyword(&start[0..len])
			}
		};
		Some((t, start_pos - 1, self.pos))
    }
}

fn keyword(s: &[u8]) -> Token {
    match s {
        b"fn" => Token::Fn,
        b"return" => Token::Return,
        b"if" => Token::If,
        b"else" => Token::Else,
        b"let" => Token::Let,
        b"mut" => Token::Mut,
        b"loop" => Token::Loop,
        b"br" => Token::Br,
        b"struct" => Token::Struct,
        b"union" => Token::Union,
        b"enum" => Token::Enum,
        _ => Token::Ident(String::from_utf8_lossy(s).to_string()),
    }
}

fn char_to_radix(c: u8) -> Option<u8> {
    match c {
        b'b' => Some(2),
        b't' => Some(3),
        b'q' => Some(4),
        b'p' => Some(5),
        b'h' => Some(6),
        b's' => Some(7),
        b'o' => Some(8),
        b'e' => Some(9),
        b'd' => Some(10),
        b'l' => Some(11),
        b'z' => Some(12),
        b'x' => Some(16),
        _ => None,
    }
}
