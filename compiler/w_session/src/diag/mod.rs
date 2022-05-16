use std::{cell::RefCell, slice::SliceIndex};

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

pub struct Diagnostics<E: emitter::Emitter> {
    emitter: RefCell<E>,
}

impl<E: emitter::Emitter> Diagnostics<E> {
    pub fn new(emitter: E) -> Self {
        Self {
            emitter: RefCell::new(emitter),
        }
    }

    pub fn error(&self, diagnostic: Diagnostic) {
        self.emitter.borrow_mut().emit_error(diagnostic);
    }
}
