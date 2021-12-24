/*
    BIP - Below Ident Priority
    AIP - Above Ident Priority
*/

mod token;

use std::iter::FromIterator;
pub use token::{AmbiguousOp, BinOp, BinOpVariant, Token, UnOp};

pub struct Lexer<I>
where
    I: Iterator<Item = char>,
{
    stream: I,
    buffer: Option<char>,
    skip_first: bool,
    done: bool,
    pos: usize,
}

impl<I> Lexer<I>
where
    I: Iterator<Item = char>,
{
    pub fn new(stream: I) -> Self {
        Lexer {
            stream,
            buffer: None,
            skip_first: false,
            done: false,
            pos: 0,
        }
    }

    fn try_take_equals(&mut self) -> bool {
        match self.nextc() {
            Some('=') => true,
            t => {
                self.backtrack(t);
                false
            }
        }
    }

    fn skip_digits(&mut self, buf: &mut String, radix: u32) {
        loop {
            match self.nextc() {
                Some(t) if t.is_digit(radix) => {
                    buf.push(t);
                }
                c1 => {
                    self.backtrack(c1);
                    break;
                }
            }
        }
    }

    fn parse_string(&mut self) -> String {
        let mut res = String::new();

        loop {
            match self.nextc() {
                Some('\\') => {
                    // check the first character
                    let header_char = self.nextc().expect("expected escape character, got <eof>");

                    if let Some(radix) = char_to_radix(header_char) {
                        let radix = u32::from(radix);

                        //  ^ \x6e, \d71
                        // if there is no applicable digit after, then treat as normal escape
                        match self.nextc() {
                            Some(header_digit) if header_digit.is_digit(radix) => {
                                // go on parsing as normal
                                let mut num = header_digit.to_string();
                                loop {
                                    match self.nextc() {
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

        while let Some(c2) = self.nextc() {
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
        let c2 = self.nextc();
        match self.nextc() {
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

    fn try_aip(&mut self, c: char) -> Option<Token> {
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
            ':' => Token::Colon,
            ';' => Token::Semicolon,
            ',' => Token::Comma,
            '{' => Token::LeftBracket,
            '}' => Token::RightBracket,
            '(' => Token::LeftParen,
            ')' => Token::RightParen,
            '[' => Token::LeftSqBracket,
            ']' => Token::RightSqBracket,

            '*' => op!(@ambiguous Asterisk),
            '/' => op!(@binary Div),

            '%' => op!(@binary Mod),
            '^' => op!(@binary Xor),
            '&' => op!(@ambiguous Ampersand),
            '|' => op!(@binary Or),
            '~' => op!(@unary BNot),

            '\'' => self.parse_char(),
            '"' => Token::String(self.parse_string()),

            '>' | '<' | '=' | '!' | '+' | '-' => {
                // two char ops
                let c2 = self.nextc();
                match (c, c2) {
                    ('>', Some('=')) => op!(@binary Ge),
                    ('<', Some('=')) => op!(@binary Le),
                    ('<', Some('<')) => op!(@binary Lsh),
                    ('>', Some('>')) => op!(@binary Rsh),
                    ('<', _) => {
                        self.backtrack(c2);
                        op!(@simple Lt)
                    }
                    ('>', _) => {
                        self.backtrack(c2);
                        op!(@simple Gt)
                    }

                    ('=', Some('=')) => op!(@binary EqC),
                    ('=', _) => {
                        self.backtrack(c2);
                        op!(@compound Id)
                    }

                    ('!', Some('=')) => op!(@binary Neq),
                    ('!', _) => {
                        self.backtrack(c2);
                        op!(@unary LNot)
                    }

                    ('+', Some('+')) => op!(@unary Inc),
                    ('+', _) => {
                        self.backtrack(c2);
                        op!(@ambiguous Plus)
                    }

                    ('-', Some('-')) => op!(@unary Dec),
                    ('-', Some('>')) => Token::Arrow,
                    ('-', _) => {
                        self.backtrack(c2);
                        op!(@ambiguous Minus)
                    }

                    (_, _) => panic!(),
                }
            }
            _ => return None,
        })
    }

    fn try_bip(&mut self, c: char) -> Option<Token> {
        Some(match c {
            '.' | '0'..='9' => {
                let mut num = if c == '.' {
                    match self.nextc() {
                        Some(n @ '0'..='9') => {
                            format!("0.{}", n)
                        }
                        ch => {
                            self.backtrack(ch);
                            return Some(Token::Period);
                        }
                    }
                } else {
                    c.to_string()
                };

                let mut radix = 10_u8;

                if c == '0' {
                    let t = self.nextc();
                    match t {
                        Some(header_char) => {
                            if let Some(radix_) = char_to_radix(header_char) {
                                radix = radix_;
                            } else {
                                self.backtrack(t);
                            }
                        }
                        None => return Some(Token::Integer(0)),
                    }
                }

                self.skip_digits(&mut num, radix.into());

                let seen_period = if c == '.' {
                    true
                } else if radix == 10_u8 {
                    match self.nextc() {
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

                match self.nextc() {
                    Some('E' | 'e') if radix == 10_u8 => {
                        match self.nextc() {
                            Some(ch @ ('+' | '-')) => num.push(ch),
                            ch @ Some(_) => self.backtrack(ch),
                            None => todo!(),
                        }
                        self.skip_digits(&mut num, 10);
                    }
                    ch => self.backtrack(ch),
                }

                if seen_period {
                    Token::Float(num.parse::<f64>().unwrap())
                } else {
                    let num = u64::from_str_radix(&num, radix.into()).unwrap();
                    Token::Integer(unsafe { std::mem::transmute::<u64, i64>(num) })
                }
            }
            _ => return self.try_aip(c),
        })
    }

    fn try_tk_bip(&mut self, c: char) -> Option<Token> {
        let t = self.try_bip(c)?;
        Some(t)
    }

    fn nextc(&mut self) -> Option<char> {
        self.pos += self.skip_first as usize;
        self.skip_first = true;
        self.buffer.take().or_else(|| self.stream.next())
    }

    fn backtrack(&mut self, t: Option<char>) {
        self.buffer = t;
        self.pos -= 1;
    }

    /// returns the first non-comment character
    #[allow(clippy::single_match)]
    fn skip_comments(&mut self) -> Option<char> {
        loop {
            match self.nextc() {
                Some('/') => match self.nextc() {
                    // single comment
                    Some('/') => loop {
                        match self.nextc() {
                            Some('\n') => break,
                            None => return None,
                            _ => {}
                        }
                    },
                    // multi-line comment
                    Some('*') => loop {
                        match self.nextc() {
                            Some('*') => match self.nextc() {
                                Some('/') => break,
                                _ => {}
                            },
                            None => todo!(),
                            _ => {}
                        }
                    },
                    // neither, break loop
                    _ => break Some('/'),
                },
                Some(c) if !c.is_whitespace() => break Some(c),
                None => return None,
                _ => {}
            }
        }
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
        _ => Token::Ident(s),
    }
}

impl<I> Iterator for Lexer<I>
where
    I: Iterator<Item = char>,
{
    type Item = (Token, usize, usize);

    fn next(&mut self) -> Option<(Token, usize, usize)> {
        if self.done {
            return None;
        }

        let c = if let Some(c) = self.skip_comments() {
            c
        } else {
            self.done = true;
            return None;
        };

        let start = self.pos;
        self.try_tk_bip(c)
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
