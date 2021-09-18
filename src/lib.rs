pub mod diag;
pub mod lexer;
pub mod parser;
pub mod span;

/**
    TODO:
    * remove RefCell overhead
*/
use std::cell::RefCell;

use diag::Diagnostic;
use parser::Parser;

pub struct Session {
    warnings: RefCell<Vec<Diagnostic>>,
    errors: RefCell<Vec<Diagnostic>>,
}

impl Session {
    pub fn new() -> Self {
        Session {
            warnings: RefCell::new(Vec::new()),
            errors: RefCell::new(Vec::new()),
        }
    }

    pub fn parse<'a>(&'a self, src: &'a str) -> Parser<'a> {
        Parser::new(self, src)
    }

    pub fn error(&self, diag: Diagnostic) {
        let mut w = self.errors.borrow_mut();
        w.push(diag);
    }
}
