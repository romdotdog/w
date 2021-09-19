pub mod diag;
pub mod lexer;
pub mod parser;
pub mod source;
pub mod span;

/**
    TODO:
    * remove RefCell overhead
*/
use std::cell::RefCell;

use diag::Diagnostic;
use lexer::Lexer;
use parser::Parser;
use source::Source;

pub struct Session {
    sources: Vec<Source>,
    warnings: RefCell<Vec<Diagnostic>>,
    errors: RefCell<Vec<Diagnostic>>,
}

impl Session {
    pub fn new() -> Self {
        Session {
            sources: Vec::new(),
            warnings: RefCell::new(Vec::new()),
            errors: RefCell::new(Vec::new()),
        }
    }

    pub fn lexer<'a>(&'a self, src: &'a str) -> Lexer<'a> {
        Lexer::new(self, src)
    }

    pub fn parse<'a>(&'a self, src: &'a str) -> Parser<'a> {
        Parser::new(self, src)
    }

    pub fn error(&self, diag: Diagnostic) {
        let mut w = self.errors.borrow_mut();
        w.push(diag);
    }
}
