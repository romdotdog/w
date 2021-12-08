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
    MissingSemicolon,
    MissingType,
    MalformedType,
    MissingClosingParen,
    MissingClosingBracket,
    MissingClosingAngleBracket,
    MissingClosingSqBracket,
    TooMuchIndirection,
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
