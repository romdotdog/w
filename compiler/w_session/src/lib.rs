#![allow(clippy::must_use_candidate)]

use std::cell::RefCell;
use std::rc::Rc;

pub mod diag;
pub mod source_map;

use diag::emitter::Emitter;
use diag::Diagnostic;
use diag::Diagnostics;
use source_map::{
    loader::Loader,
    source::{Source, SourceReader},
    SourceMap,
};
use w_ast::Span;
use w_errors::Message;
use w_parser::Parser;

pub struct Session<L: Loader, E: Emitter> {
    pub source_map: SourceMap<L>,
    diags: RefCell<Diagnostics<E>>,
}

impl<L: Loader, E: Emitter> Session<L, E> {
    pub fn new(loader: L, emitter: E) -> Self {
        Session {
            diags: RefCell::new(Diagnostics::new(emitter)),
            source_map: SourceMap::new(loader),
        }
    }

    pub fn parse(&self, src: Rc<Source>) -> Parser<'_, Self, SourceReader> {
        Parser::new(self, src)
    }
}

impl<L: Loader, E: Emitter> w_parser::Handler for Session<L, E> {
    type SourceRef = Rc<Source>;
    type LexerInput = SourceReader;

    fn error(&self, src_ref: &Self::SourceRef, msg: Message, span: Span) {
        self.diags.borrow_mut().error(Diagnostic {
            source: Rc::clone(src_ref),
            span,
            msg,
        });
    }

    fn load_source(&self, name: String) -> Option<Self::SourceRef> {
        // TODO: remove .ok()
        self.source_map.load_source(name).ok()
    }

    fn get_source(&self, src_ref: &Self::SourceRef) -> Self::LexerInput {
        SourceReader::new(Rc::clone(src_ref))
    }
}

// #[cfg(test)]
// mod tests {
//     use source_map::loader::FileLoader;

//     use crate::source_map;

//     use super::Session;
//     use w_lexer::Token;

//     #[test]
//     fn lexer() {
//         let sess = Session::new(Box::new(FileLoader));
//         let src = sess
//             .source_map()
//             .register_source("lexer_test".to_owned(), "a 1.2 b //".to_owned());
//         let mut lex = sess.lexer(src);

//         assert_eq!(lex.next(), Some(Token::Ident("a".to_owned())));
//         assert_eq!(lex.next(), Some(Token::Float(1.2)));
//         assert_eq!(lex.next(), Some(Token::Ident("b".to_owned())));
//         assert_eq!(lex.next(), None);
//     }
//}
