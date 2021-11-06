/*
    TODO:
    * Refactor

    Terminology:
        BIP - Below Ident Priority
        AIP - Above Ident Priority
*/

use crate::{span::Span, Session, SourceRef};

mod token;
pub use token::{AmbiguousOp, BinOp, BinOpVariant, Token, UnOp};

use std::str::Chars;

pub struct Lexer<'a> {
    session: &'a Session,

    stream: Chars<'a>,
    src: SourceRef,

    buffer: Option<char>,
    token_buffer: Option<(Token, Span)>,

    p: isize,

    start: usize,
    end: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(session: &'a Session, src: SourceRef) -> Self {
        let s = session.get_source(&src);

        Lexer {
            session,

            stream: s.content().chars(),
            src,

            buffer: None,
            token_buffer: None,
            p: -1,

            start: 0,
            end: 0,
        }
    }

    pub fn span(&self) -> Span {
        Span::new(self.src, self.start, self.end)
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
            _ => Token::Ident(s),
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

    fn try_aip(&mut self, c: char) -> Option<Token> {
        // TODO: Refactor for code size
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
            '.' => Token::Period,
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

            '\'' => {
                let c2 = self.nextc();
                match self.nextc() {
                    Some('\'') => {
                        Token::Char(c2.expect("reached <eof>, expected character in quote"))
                    }
                    Some(c3) => panic!("unexpected character {}, expected '", c3),
                    None => panic!("reached <eof>, expected '"),
                }
            }

            '"' => {
                let mut res = String::new();

                loop {
                    match self.nextc() {
                        Some('\\') => {
                            // check the first character
                            let header_char =
                                self.nextc().expect("expected escape character, got <eof>");

                            if let Some(radix) = char_to_radix(header_char) {
                                let radix = radix as u32;

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
                                        assert!(
                                            codepoint.is_ok(),
                                            "max escape value is {}",
                                            u32::MAX
                                        );

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
                                // TODO: Figure out the semantics of this
                                res.push(match header_char {
                                    't' => '\t',
                                    'r' => '\r',
                                    'n' => '\n',
                                    _ => {
                                        // is not an escape
                                        header_char
                                    }
                                })
                            }
                        }
                        Some('"') => {
                            break;
                        }
                        Some(c) => res.push(c),
                        None => panic!("incomplete string literal"),
                    }
                }

                Token::String(res)
            }

            '>' | '<' | '=' | '!' | '+' | '-' => {
                // two char ops
                let c2 = self.nextc();
                match (c, c2) {
                    ('>', Some('=')) => op!(@binary Ge),
                    ('<', Some('=')) => op!(@binary Ge),
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

                    ('+', Some('+')) => {
                        // Please don't
                        op!(@unary Inc)
                    }

                    ('-', Some('-')) => {
                        op!(@unary Dec)
                    }

                    ('+', _) => {
                        self.backtrack(c2);
                        op!(@ambiguous Plus)
                    }

                    ('-', _) => {
                        self.backtrack(c2);
                        op!(@ambiguous Minus)
                    }

                    (_, _) => {
                        self.backtrack(c2);
                        return None;
                    }
                }
            }
            _ => return None,
        })
    }

    fn try_bip(&mut self, c: char) -> Option<Token> {
        Some(match c {
            '.' | '0'..='9' => {
                #[derive(PartialEq)]
                enum Floatable {
                    Maybe,
                    True,
                    False,
                }

                let mut float = if c == '.' {
                    Floatable::True
                } else {
                    Floatable::Maybe
                };

                let mut radix: u32 = 10;
                if c == '0' {
                    let t = self.nextc();
                    match t {
                        Some(header_char) => {
                            if let Some(radix_) = char_to_radix(header_char) {
                                float = Floatable::False;
                                radix = radix_ as u32;
                            } else {
                                self.backtrack(t);
                            }
                        }
                        _ => self.backtrack(t),
                    }
                }

                let mut num = c.to_string();
                self.skip_digits(&mut num, radix);

                match self.nextc() {
                    Some('.') if c != '.' => {
                        if float == Floatable::False {
                            panic!("floats in different radix are not supported")
                        }

                        float = Floatable::True;

                        num.push('.');
                        self.skip_digits(&mut num, 10);
                    }
                    t => {
                        self.backtrack(t);
                    }
                }

                if float == Floatable::True {
                    Token::Float(num.parse::<f64>().unwrap())
                } else {
                    let num = u64::from_str_radix(&num, radix).unwrap();
                    Token::Integer(unsafe { std::mem::transmute::<u64, i64>(num) })
                }
            }
            _ => return self.try_aip(c),
        })
    }

    fn try_tk_aip(&mut self, c: char) -> Option<(Token, Span)> {
        let start = self.p as usize;
        let r = self.try_aip(c);
        let end = self.p as usize + 1;
        r.map(|t| (t, Span::new(self.src, start, end)))
    }

    fn try_tk_bip(&mut self, c: char) -> Option<Token> {
        self.start = self.p as usize;
        let r = self.try_bip(c);
        self.end = self.p as usize + 1;
        r
    }

    fn nextc(&mut self) -> Option<char> {
        self.p += 1;
        self.buffer.take().or_else(|| self.stream.next())
    }

    fn backtrack(&mut self, t: Option<char>) {
        self.buffer = t;
        self.p -= 1;
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        if let Some((token, span)) = self.token_buffer.take() {
            self.start = span.start;
            self.end = span.end;

            return Some(token);
        }

        let c = loop {
            match self.nextc() {
                Some('/') => match self.nextc() {
                    Some('/') => loop {
                        match self.nextc() {
                            Some('\n') => break,
                            None => return None,
                            _ => {}
                        }
                    },
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
                    _ => break '/',
                },
                Some(c) if !c.is_whitespace() => break c,
                None => return None,
                _ => {}
            }
        };

        self.try_tk_bip(c).or_else(|| {
            let mut ident = c.to_string();
            let start = self.p as usize;
            let mut end = start + 1;

            while let Some(c2) = self.nextc() {
                if c2.is_whitespace() {
                    break;
                }

                end = self.p as usize;
                if let Some(t) = self.try_tk_aip(c2) {
                    self.token_buffer = Some(t);
                    break;
                }

                ident.push(c2);
            }

            self.start = start;
            self.end = end;
            Some(Lexer::keyword(ident))
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
