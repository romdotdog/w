use crate::{lexer::Token, span::Span};
use std::fmt::Display;

pub struct Diagnostic {
    span: Span,
    message: Message,
}

impl Diagnostic {
    pub fn new(span: Span, message: Message) -> Self {
        Diagnostic { span, message }
    }
}

// TODO: Remove
#[derive(Debug)]
pub enum Lexeme {
    // Tokens
    Fn,
    Return,
    If,
    Else,
    Semicolon,
    Comma,
    Exclamation,
    Colon,
    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    LeftAngBracket,
    RightAngBracket,

    Op,
    Integer,
    UInteger,
    Float,
    String,
    Char,
    Ident,

    // Misc
    Eof,
    PrimaryExpr,
    Type,
}

impl From<Token> for Lexeme {
    fn from(t: Token) -> Self {
        match t {
            Token::Fn => Lexeme::Fn,
            Token::If => Lexeme::If,
            Token::Else => Lexeme::Else,
            Token::Return => Lexeme::Return,
            Token::Semicolon => Lexeme::Semicolon,
            Token::Comma => Lexeme::Comma,
            Token::Colon => Lexeme::Colon,
            Token::Exclamation => Lexeme::Exclamation,
            Token::LeftParen => Lexeme::LeftParen,
            Token::RightParen => Lexeme::LeftParen,
            Token::LeftBracket => Lexeme::LeftParen,
            Token::RightBracket => Lexeme::LeftParen,
            Token::Op { .. } => Lexeme::Op,
            Token::UInteger(_) => Lexeme::UInteger,
            Token::Integer(_) => Lexeme::Integer,
            Token::Float(_) => Lexeme::Float,
            Token::String(_) => Lexeme::String,
            Token::Char(_) => Lexeme::Char,
            Token::Ident(_) => Lexeme::Ident,
        }
    }
}

impl From<Option<Token>> for Lexeme {
    fn from(t: Option<Token>) -> Self {
        match t {
            Some(t) => t.into(),
            None => Lexeme::Eof,
        }
    }
}

pub enum Message {
    ExpectedGot(Lexeme, Lexeme),
}

impl Display for Message {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Message::ExpectedGot(a, b) => write!(f, "expected {:?}, got {:?}", a, b),
        }
    }
}
