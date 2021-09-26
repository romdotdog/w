use crate::lexer::Token;
use std::fmt::Display;

// TODO: Remove
#[derive(Debug)]
pub enum Lexeme {
    // Tokens
    Fn,
    Return,
    If,
    Let,
    Mut,
    Else,
    Semicolon,
    Comma,
    Colon,
    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    LeftAngBracket,
    RightAngBracket,

    BinOp,
    UnOp,
    Op,

    Integer,
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
            Token::Let => Lexeme::Let,
            Token::Mut => Lexeme::Mut,
            Token::Semicolon => Lexeme::Semicolon,
            Token::Comma => Lexeme::Comma,
            Token::Colon => Lexeme::Colon,
            Token::LeftParen => Lexeme::LeftParen,
            Token::RightParen => Lexeme::LeftParen,
            Token::LeftBracket => Lexeme::LeftParen,
            Token::RightBracket => Lexeme::LeftParen,
            Token::BinOp(_) => Lexeme::BinOp,
            Token::UnOp(_) => Lexeme::UnOp,
            Token::AmbiguousOp(_) => Lexeme::Op,
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
    MissingSemicolon,
}

impl Display for Message {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Message::ExpectedGot(a, b) => write!(f, "expected {:?}, got {:?}", a, b),
            Message::MissingSemicolon => write!(f, "missing a semicolon or closing brace"),
        }
    }
}
