use super::{Diagnostics, Message};
use std::{fmt::Display, rc::Rc};

impl Display for Message {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Message::UnexpectedToken => write!(f, "unexpected token"),
            Message::MissingSemicolon => write!(f, "missing a semicolon or closing brace"),
            Message::MissingIdentifier => write!(f, "missing identifier here"),
            Message::MalformedIdentifier => write!(f, "invalid identifier here"),
            Message::MissingType => write!(f, "missing type here"),
            Message::MalformedType => write!(f, "found malformed type"),
            Message::MissingClosingParen => write!(f, "')' expected here"),
            Message::MissingClosingBracket => write!(f, "missing '}}'"),
            Message::MissingClosingAngleBracket => write!(f, "'>' expected here"),
            Message::MissingClosingSqBracket => write!(f, "']' expected here"),
            Message::InvalidTopLevel => {
                write!(f, "only functions, globals and directives are allowed here")
            }
            Message::TooMuchIndirection => {
                write!(f, "at most only 5 levels of indirection are allowed")
            }
        }
    }
}

impl Diagnostics {
    /// # Panics
    /// error span start (inclusive) >= error span end (exclusive)
    pub fn diagnostics(&self) {
        for error in &self.errors {
            // TODO: consider whether to keep this function as &mut self
            // or to convert to self
            let src = Rc::clone(&error.source);
            let start_pos = error.span.start;
            let end_pos = error.span.end;

            assert!(
                start_pos < end_pos,
                "{} - {} / {}",
                start_pos,
                end_pos,
                error.message
            );

            let (line, column, (start_of_line, end_of_line)) = src.line_col(start_pos);
            let spaces = src.content()[start_of_line..start_pos]
                .chars()
                .map(|c| if c.is_whitespace() { c } else { ' ' })
                .collect::<String>();

            println!(
                "\x1b[1m{}:{}:{}: \x1b[91merror:\x1b[0m {}\n{}\n{}\x1b[91m\x1b[1m{}\x1b[0m",
                src.name(),
                line,
                column,
                error.message,
                src.content()[start_of_line..end_of_line].trim_end(),
                spaces,
                "^".repeat(end_pos - start_pos),
            )
        }
    }
}
