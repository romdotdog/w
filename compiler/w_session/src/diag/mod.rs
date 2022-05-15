use appendlist::AppendList;
use w_errors::Message;
use w_utils::span::Span;

use crate::source_map::source::Source;

pub mod emitter;

pub struct Diagnostic<'ast> {
    pub source: &'ast Source,
    pub span: Span,
    pub msg: Message,
}

pub struct Diagnostics<'ast, E: emitter::Emitter> {
    emitter: E,
    errors: AppendList<Diagnostic<'ast>>,
}

impl<'ast, E: emitter::Emitter> Diagnostics<'ast, E> {
    pub fn new(emitter: E) -> Self {
        Self {
            emitter,
            errors: AppendList::new(),
        }
    }

    pub fn error(&self, diagnostic: Diagnostic<'ast>) {
        self.errors.push(diagnostic);
    }

    pub fn emit_errors(self) {
        self.emitter.emit_errors(self.errors);
    }
}
