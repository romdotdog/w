#![allow(clippy::must_use_candidate)]

pub mod diag;
pub mod source_map;

use std::cell::RefCell;
use std::path::Path;

use diag::emitter::Emitter;
use diag::Diagnostic;
use diag::Diagnostics;
use source_map::{loader::Loader, source::Source, SourceMap};
use w_ast::Span;
use w_ast::AST;
use w_errors::Message;
use w_parser::handler::Handler;
use w_parser::handler::Status;
use w_parser::Parser;

type Program<'a> = Vec<(&'a Source, AST<'a>)>;
pub struct Session<'src, L: Loader, E: Emitter> {
    source_map: SourceMap<L>,
    diags: Diagnostics<'src, E>,
    prog: RefCell<Program<'src>>,
}

impl<'ast, L: Loader, E: Emitter> Session<'ast, L, E> {
    pub fn new(loader: L, emitter: E) -> Self {
        Session {
            diags: Diagnostics::new(emitter),
            source_map: SourceMap::new(loader),
            prog: RefCell::new(Vec::new()),
        }
    }

    pub fn parse(&'ast self, src: &'ast Source) -> Program<'ast> {
        Parser::full_parse(self, src);
        self.prog.replace(Vec::new())
    }

    pub fn source_map(&self) -> &SourceMap<L> {
        &self.source_map
    }
}

impl<'ast, L: Loader, E: Emitter> Handler<'ast> for Session<'ast, L, E> {
    type SourceRef = Source;

    fn error(&self, src_ref: &'ast Source, msg: Message, span: Span) {
        self.diags.error(Diagnostic {
            source: src_ref,
            span,
            msg,
        });
    }

    fn load_source(
        &'ast self,
        src_ref: &'ast Self::SourceRef,
        path: &'ast str,
    ) -> Option<(&'ast Source, Status)> {
        let new_path = src_ref.path.parent().unwrap().join(Path::new(path));
        if let Ok(src_ref) = self.source_map.load_source(new_path) {
            let status = src_ref.get_status();
            src_ref.set_status(Status::CurrentlyParsing);
            Some((src_ref, status))
        } else {
            None
        }
    }

    fn get_source(&self, src_ref: &'ast Source) -> &'ast str {
        src_ref.src()
    }

    fn set_ast(&self, src_ref: &'ast Self::SourceRef, prog: AST<'ast>) {
        src_ref.set_status(Status::AlreadyParsed);
        self.prog.borrow_mut().push((src_ref, prog));
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
