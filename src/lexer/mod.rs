/*
	TODO:
	* Refactor

	Terminology:
		BIP - Below Ident Priority
		AIP - Above Ident Priority
*/

use crate::span::Span;

mod token;
pub use token::{Token, TokenType, Op};

use std::str::Chars;

pub struct Lexer<'a> {
	stream: Chars<'a>,
	buffer: Option<char>, 
	token_buffer: Option<Token>,

	p: isize
}

impl<'a> Lexer<'a> {
	pub fn new(stream: &'a str) -> Self {
		Lexer {
			stream: stream.chars(),
			buffer: None,
			token_buffer: None,
			p: -1
		}
	}

	fn keyword(s: String) -> TokenType {
		match s.as_str() {
			"fn" => TokenType::Fn,
			"i32" => TokenType::I32,
			_ => TokenType::Identifier(s)
		}
	}

	fn try_take_equals(&mut self) -> bool {
		match self.nextc() {
			Some('=') => true,
			t => {
				self.buffer = t; 
				false
			},
		}
	}

	fn skip_digits(&mut self, buf: &mut String, radix: u32) {
		loop {
			match self.nextc() {
				Some(t) if t.is_digit(radix) => {
					buf.push(t);
				},
				c1 => {
					self.buffer = c1;
					break
				}
			}
		}
	}

	fn try_aip(&mut self, c: char) -> Option<TokenType> {
		macro_rules! op {
			($t: ident) => {
				TokenType::Op {
					t: Op::$t,
					is_assignment: self.try_take_equals()
				}
			}
		}

		Some(match c {
			':' => TokenType::Colon,
			';' => TokenType::Colon,
			'{' => TokenType::LeftBracket,
			'}' => TokenType::RightBracket,
			'(' => TokenType::LeftParen,
			')' => TokenType::RightParen,

			'*' => op!(Mul),
			'/' => op!(Div),
			'%' => op!(Mod),
			'^' => op!(Xor),
			'&' => op!(And),
			'|' => op!(Or),

			'\'' => {
				let c2 = self.nextc();
				match self.nextc() {
					Some('\'') => TokenType::Char(c2.expect("reached <eof>, expected character in quote")),
					Some(c3) => panic!("unexpected character {}, expected '", c3),
					None => panic!("reached <eof>, expected '")
				}
			}

			'"' => {
				let mut res = String::new();

				loop {
					match self.nextc() {
						Some('\\') => {
							// check the first character
							let header_char = self.nextc()
								.expect("expected escape character, got <eof>");

							if let Some(radix) = char_to_radix(header_char) {
								let radix = radix as u32;
								//  ^ \x6e, \d71
								// if there is no applicable digit after, then treat as normal escape
								let header_digit = self.nextc()
									.expect("expected integer escape, got <eof>");

								if header_digit.is_digit(radix) {
									// go on parsing as normal
									let mut num = String::from(header_digit);
									loop {
										match self.nextc() {
											Some(t) if t.is_digit(radix) => {
												num.push(t);
											},
											c1 => {
												self.buffer = c1;
												break
											}
										}
									}


									let codepoint = u32::from_str_radix(&num, radix);
									assert!(codepoint.is_ok(), "max escape value is {}", u32::MAX);

									let escaped = char::from_u32(codepoint.unwrap());
									assert!(
										escaped.is_some(),
										"invalid codepoint, cannot add \\{}{} as part of escape",
										header_char,
										num
									);

									// push the codepoint
									res.push(escaped.unwrap());
								} else {
									// treat header character as escaped
									res.push(header_char);
								}
							} else {
								// is not an escape
								res.push(header_char);
							}
						}
						Some('"') => {
							break;
						}
						Some(c) => res.push(c),
						None => panic!("incomplete string literal"),
					}
				}

				TokenType::String(res)
			}


			'>' | '<' | '=' | '!' | '+' | '-' => {
				enum Assignment {
					Unset,
					True,
					False
				}

				// two char ops
				let mut is_assignment = Assignment::Unset;
				let c2 = self.nextc();

				let t = match (c, c2) {
					('>', Some('=')) => Op::Ge,
					('<', Some('=')) => Op::Le,
					('<', Some('<')) => Op::Lsh,
					('>', Some('>')) => Op::Rsh,
					('<', _) => { 
						self.buffer = c2;
						is_assignment = Assignment::False;
						Op::Lt 
					},
					('>', _) => { 
						self.buffer = c2;
						is_assignment = Assignment::False;
						Op::Gt 
					},
					
					
					('=', Some('=')) => Op::EqC,
					('=', _) => {
						self.buffer = c2;
						is_assignment = Assignment::True;
						Op::Id
					},

					('!', Some('=')) => Op::Neq,
					('!', _) => {
						self.buffer = c2;
						is_assignment = Assignment::False;
						Op::LNot
					},

					('+', Some('+')) => {
						// Please don't
						is_assignment = Assignment::False;
						Op::Inc
					},

					('-', Some('-')) => {
						is_assignment = Assignment::False;
						Op::Dec
					},

					('+', _) => { 
						self.buffer = c2;
						Op::Add 
					},
					('-', _) => {
						self.buffer = c2;
						Op::Sub 
					},

					(_, _) => {
						self.buffer = c2;
						return None 
					}
				};

				TokenType::Op {
					t,
					is_assignment: match is_assignment {
						Assignment::Unset => self.try_take_equals(),
						Assignment::True => true,
						Assignment::False => false
					}
				}
			}
			_ => return None
		})
	}

	fn try_bip(&mut self, c: char) -> Option<TokenType> {
		Some(match c {
			'.' | '0' ..= '9' => {
				#[derive(PartialEq)]
				enum Floatable {
					Maybe,
					True,
					False
				}

				let mut float = Floatable::Maybe;

				let mut radix: u32 = 10;
				if c == '0' {
					let t = self.nextc();
					match t {
						Some(header_char) => {
							if let Some(radix_) = char_to_radix(header_char) {
								float = Floatable::False;
								radix = radix_ as u32;
							} else {
								self.buffer = t;
							}
						},
						_ => self.buffer = t
					}
				}

				let mut num = String::from(c);
				self.skip_digits(&mut num, radix);

				if c != '.' && match self.nextc() {
					Some('.') => true,
					t => { self.buffer = t; false }
				} {
					if float == Floatable::False {
						panic!("floats in different radix are not supported")
					}

					float = Floatable::True;
					
					num.push('.');
					self.skip_digits(&mut num, 10);
				}

				if float == Floatable::True {
					TokenType::Float(num.parse::<f64>().unwrap())
				} else {
					let num = i64::from_str_radix(&num, radix);
					TokenType::Integer(num.unwrap())
				}
			},
			_ => return self.try_aip(c)
		})
	}

	fn try_tk_aip(&mut self, c: char) -> Option<Token> {
		let start = self.p;
		self.try_aip(c).map(|t|
			Token::new(t, Span::new(start as usize, self.p as usize))
		)
	}

	fn try_tk_bip(&mut self, c: char) -> Option<Token> {
		let start = self.p;
		self.try_bip(c).map(|t|
			Token::new(t, Span::new(start as usize, self.p as usize))
		)
	}

	fn nextc(&mut self) -> Option<char> {
		self.p += 1;
		self.buffer.take().or_else(|| self.stream.next())
	}

	pub fn insert(&mut self, t: char) {
		assert!(self.buffer.is_none());
		self.buffer = Some(t);
	}
}

impl<'a> Iterator for Lexer<'a> {
	type Item = Token;

	fn next(&mut self) -> Option<Token> {
		if let Some(token) = self.token_buffer.take() {
			return Some(token);
		}

		let c = loop {
			match self.nextc() {
				Some(c) if !c.is_whitespace() => break c,
				None => return None,
				_ => {}
			}
		};

		self.try_tk_bip(c).or_else(|| {
			let mut ident = String::from(c);
			let start = self.p;
			
			while let Some(c2) = self.nextc() {
				if c2.is_whitespace() {
					break;
				}

				if let Some(t) = self.try_tk_aip(c2) {
					self.token_buffer = Some(t); 
					break;
				}

				ident.push(c2);
			}
			
			
			Some(Token::new(Lexer::keyword(ident), Span::new(start as usize, self.p as usize - 1)))
		})
	}
}

fn char_to_radix(c: char) -> Option<u8> {
    match c {
        'b' => Some(2),
        't' => Some(3),
        'q' => Some(4),
        'p' => Some(5),
        'h' => Some(6),
        's' => Some(7),
        'o' => Some(8),
        'e' => Some(9),
        'd' => Some(10),
        'l' => Some(11),
        'z' => Some(12),
        'x' => Some(16),
        _ => None,
    }
}