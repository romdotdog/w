use crate::lexer::Token;
use std::fmt::Display;
pub enum Message {
    CompilerError,
    MissingIdentifier,
    MalformedIdentifier,
    MissingSemicolon,
    MissingType,
    MalformedType,
    MissingClosingParen,
    MissingClosingAngleBracket,
    InitializerRequired,
    ParserPanicFailed,
}

impl Display for Message {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Message::MissingSemicolon => write!(f, "missing a semicolon or closing brace"),
            Message::CompilerError => write!(f, "an internal compiler error has occurred"),
            Message::MissingIdentifier => write!(f, "missing identifier here"),
            Message::MalformedIdentifier => write!(f, "expected identifier here"),
            Message::MissingType => write!(f, "missing type here"),
            Message::MalformedType => write!(f, "found malformed type"),
            Message::MissingClosingParen => write!(f, "')' expected here"),
            Message::MissingClosingAngleBracket => write!(f, "'>' expected here"),
            Message::InitializerRequired => write!(f, "declaration must have an initializer"),
            Message::ParserPanicFailed => {
                write!(f, "parser panicked incorrectly (not a code error)")
            }
        }
    }
}
