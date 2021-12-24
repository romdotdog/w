#![allow(clippy::missing_panics_doc)]
use std::str::Chars;

use criterion::{black_box, criterion_group, criterion_main, Criterion};
use w_ast::Span;
use w_errors::Message;
use w_lexer::Lexer;
use w_parser::{Handler, Parser};

struct Session<'a> {
    src: &'a str,
}

impl<'a> Handler for Session<'a> {
    type SourceRef = ();
    type LexerInput = Chars<'a>;

    fn error(&self, _src_ref: &Self::SourceRef, _msg: Message, _span: Span) {
        panic!("source errored in bench");
    }

    fn load_source(&self, _name: String) -> Option<Self::SourceRef> {
        panic!("imports are not allowed in benches");
    }

    fn get_source(&self, _src_ref: &Self::SourceRef) -> Self::LexerInput {
        self.src.chars()
    }
}

pub fn criterion_benchmark(c: &mut Criterion) {
    let src = std::fs::read_to_string("D:/w/benches/log10/log10.w").unwrap();

    let count = Lexer::new(src.chars()).count();
    c.bench_function(format!("lexer {} tk", count).as_str(), |b| {
        b.iter(|| for _t in Lexer::new(black_box(&src).chars()) {});
    });

    c.bench_function(format!("parser {} tk log10", count).as_str(), |b| {
        b.iter(|| {
            let handler = Session {
                src: black_box(&src),
            };

            let parser = Parser::new(&handler, ());
            parser.parse();
        });
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
