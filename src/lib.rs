pub mod diag;
pub mod lexer;
pub mod parser;
pub mod source;
pub mod span;

use appendlist::AppendList;
use diag::Diagnostic;
use lexer::Lexer;
use parser::Parser;
use source::Source;

pub struct Session {
    sources: AppendList<Source>,
    warnings: AppendList<Diagnostic>,
    errors: AppendList<Diagnostic>,
}

#[derive(Debug, Clone, Copy)]
pub struct SourceRef(usize);

impl Session {
    pub fn new() -> Self {
        Session {
            sources: AppendList::new(),
            warnings: AppendList::new(),
            errors: AppendList::new(),
        }
    }

    pub fn lexer<'a>(&'a self, src: SourceRef) -> Lexer<'a> {
        Lexer::new(self, src)
    }

    pub fn parse<'a>(&'a self, src: SourceRef) -> Parser<'a> {
        Parser::new(self, src)
    }

    pub fn register_source(&self, name: String, src: String) -> SourceRef {
        let r = self.sources.len();
        self.sources.push(Source::new(name, src));
        SourceRef(r)
    }

    pub fn get_source(&self, i: &SourceRef) -> &Source {
        self.sources.get(i.0).unwrap()
    }

    pub fn error(&self, diag: Diagnostic) {
        self.errors.push(diag);
    }
}
