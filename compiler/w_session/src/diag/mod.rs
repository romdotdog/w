use std::rc::Rc;

use w_errors::Message;
use w_lexer::Span;

use crate::source::Source;

pub mod emitter;

pub struct Diagnostic {
    pub source: Rc<Source>,
    pub span: Span,
    pub msg: Message,
}

pub struct Diagnostics<E: emitter::Emitter> {
    emitter: E,
    errors: Vec<Diagnostic>,
}

impl<E: emitter::Emitter> Diagnostics<E> {
    pub fn new(emitter: E) -> Self {
        Self {
            emitter,
            errors: Vec::new(),
        }
    }

    pub fn error(&mut self, diagnostic: Diagnostic) {
        self.errors.push(diagnostic);
    }

    pub fn emit_errors(self) {
        self.emitter.emit_errors(self.errors);
    }
}
