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

/*
	TODO:
	* Create fork of AppendList
*/

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
        &self.sources[i.0]
    }

    pub fn error(&self, diag: Diagnostic) {
        self.errors.push(diag);
    }

    // TODO: Limit to non-wasm targets
    // Diagnostics will be handled on the JS side
    pub fn diagnostics(&self) {
        for i in 0..self.errors.len() {
            let v = self.errors.get(i).unwrap();
            let src = self.get_source(&v.span.src);
            let start_pos = v.span.start;
            let end_pos = v.span.end;

            assert!(start_pos < end_pos);

            let (line, col, (sol, eol)) = src.line_col(start_pos);
            println!(
                "\x1b[1m{}:{}:{}: \x1b[91merror:\x1b[0m {}\n{}\n{}\x1bZ[91m\x1b[1m{}\x1b[0m",
                src.name(),
                line,
                col,
                v,
                &src.content()[sol..eol].trim_end(),
                " ".repeat(start_pos - sol),
                "^".repeat(v.span.end - start_pos),
            )
        }
    }
}
