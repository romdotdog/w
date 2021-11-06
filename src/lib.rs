#![allow(clippy::must_use_candidate)]

pub mod diag;
pub mod lexer;
pub mod parser;
pub mod source;
pub mod source_map;
pub mod span;

use std::cell::RefCell;

use diag::{Diagnostic, Diagnostics, Message};
use lexer::Lexer;
use parser::Parser;
use source_map::{Loader, SourceMap, SourceRef};
use span::Span;

pub struct Session {
    source_map: SourceMap,
    diags: RefCell<Diagnostics>,
}

impl Session {
    pub fn new(loader: Box<dyn Loader>) -> Self {
        Session {
            diags: RefCell::new(Diagnostics::default()),
            source_map: SourceMap::new(loader),
        }
    }

    pub fn lexer(&self, src: SourceRef) -> Lexer<'_> {
        Lexer::new(self, src)
    }

    pub fn parse(&self, src: SourceRef) -> Parser<'_> {
        Parser::new(self, src)
    }

    pub fn source_map(&self) -> &SourceMap {
        &self.source_map
    }

    pub fn diagnostics(&mut self) -> std::cell::RefMut<'_, diag::Diagnostics> {
        self.diags.borrow_mut()
    }

    pub fn error(&self, message: Message, span: Span) {
        self.diags.borrow_mut().error(Diagnostic {
            source: self.source_map().get_source(span.src),
            span,
            message,
        });
    }
}

#[cfg(test)]
mod tests {
    use crate::source_map::FileLoader;

    use super::lexer::Token;
    use super::Session;

    #[test]
    fn lexer() {
        let sess = Session::new(Box::new(FileLoader));
        let src = sess
            .source_map()
            .register_source("lexer_test".to_owned(), "a 1.2 b //".to_owned());
        let mut lex = sess.lexer(src);

        assert_eq!(lex.next(), Some(Token::Ident("a".to_owned())));
        assert_eq!(lex.next(), Some(Token::Float(1.2)));
        assert_eq!(lex.next(), Some(Token::Ident("b".to_owned())));
        assert_eq!(lex.next(), None);
    }
}
