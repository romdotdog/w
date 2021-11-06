use crate::{source::Source, span::Span};
use std::rc::Rc;

#[cfg(not(wasm))]
mod for_humans;

pub enum Message {
    UnexpectedToken,
    InvalidTopLevel,
    MissingIdentifier,
    MalformedIdentifier,
    MissingSemicolon,
    MissingType,
    MalformedType,
    MissingClosingParen,
    MissingClosingBracket,
    MissingClosingAngleBracket,
    MissingClosingSqBracket,
    TooMuchIndirection,
}

#[derive(Default)]
pub struct Diagnostics {
    errors: Vec<Diagnostic>,
}

impl Diagnostics {
    pub fn error(&mut self, diagnostic: Diagnostic) {
        self.errors.push(diagnostic);
    }

    pub fn errors(&self) -> &[Diagnostic] {
        &self.errors
    }
}

pub struct Diagnostic {
    pub source: Rc<Source>,
    pub span: Span,
    pub message: Message,
}
