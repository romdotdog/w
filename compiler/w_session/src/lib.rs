#![allow(clippy::must_use_candidate)]

use std::cell::RefCell;
use std::rc::Rc;

pub mod diag;
pub mod source_map;

use diag::emitter::Emitter;
use diag::Diagnostic;
use diag::Diagnostics;
use source_map::{loader::Loader, source::Source, SourceMap};
use w_ast::Program;
use w_ast::Span;
use w_errors::Message;
use w_parser::Parser;

pub struct Session<'src, L: Loader, E: Emitter> {
    pub source_map: SourceMap<L>,
    diags: Diagnostics<'src, E>,
}

impl<'ast, L: Loader, E: Emitter> Session<'ast, L, E> {
    pub fn new(loader: L, emitter: E) -> Self {
        Session {
            diags: Diagnostics::new(emitter),
            source_map: SourceMap::new(loader),
        }
    }

    pub fn parse(&'ast self, src: &'ast Source) -> Program<'ast> {
        Parser::new(self, &src).parse()
    }
}

impl<'ast, L: Loader, E: Emitter> w_parser::Handler<'ast> for Session<'ast, L, E> {
    type SourceRef = Source;

    fn error(&self, src_ref: &'ast Source, msg: Message, span: Span) {
        self.diags.error(Diagnostic {
            source: src_ref,
            span,
            msg,
        });
    }

    fn load_source(&'ast self, name: String) -> Option<&'ast Source> {
        // TODO: remove .ok()
        self.source_map.load_source(name).ok()
    }

    fn get_source(&self, src_ref: &'ast Source) -> &'ast [u8] {
        src_ref.src().as_bytes()
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
