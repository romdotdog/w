use std::fmt::Display;

//#[cfg(not(wasm))]
//mod for_humans;

#[derive(PartialEq, Eq)]
pub enum Message {
    UnexpectedToken,
    InvalidTopLevel,
    MissingIdentifier,
    MalformedIdentifier,
    LabelIsNotIdentifier,
    IdentifierIsNotLabel,
    CannotFollowLabel,
    MissingLabel,
    MissingSemicolon,
    MissingColon,
    MissingType,
    MalformedType,
    MissingOpeningBracket,
    MissingOpeningParen,
    MissingClosingParen,
    MissingClosingBracket,
    MissingClosingAngleBracket,
    MissingClosingSqBracket,
    TooMuchIndirection,
    LoopBodyBlock,
}

// TODO: target wasm
impl Display for Message {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Message::UnexpectedToken => write!(f, "unexpected token"),
            Message::MissingSemicolon => write!(f, "missing a semicolon or closing brace"),
            Message::MissingIdentifier => write!(f, "missing identifier here"),
            Message::MalformedIdentifier => write!(f, "invalid identifier here"),
            Message::MissingType => write!(f, "missing type here"),
            Message::MalformedType => write!(f, "found malformed type"),
            Message::LabelIsNotIdentifier => write!(f, "labels cannot be used as identifiers"),
            Message::IdentifierIsNotLabel => write!(f, "identifiers cannot be used as labels"),
            Message::MissingLabel => write!(f, "missing label here"),
            Message::CannotFollowLabel => write!(f, "only a loop or block can follow a label"),
            Message::MissingOpeningBracket => write!(f, "'{{' expected here"),
            Message::MissingOpeningParen => write!(f, "'(' expected here"),
            Message::MissingClosingParen => write!(f, "')' expected here"),
            Message::MissingClosingBracket => write!(f, "'}}' expected here"),
            Message::MissingClosingAngleBracket => write!(f, "'>' expected here"),
            Message::MissingClosingSqBracket => write!(f, "']' expected here"),
            Message::MissingColon => write!(f, "':' expected here"),
            Message::InvalidTopLevel => {
                write!(f, "only functions, globals and directives are allowed here")
            }
            Message::TooMuchIndirection => {
                write!(f, "at most only 5 levels of indirection are allowed")
            }
            Message::LoopBodyBlock => write!(f, "loop body may only be a block"),
        }
    }
}
