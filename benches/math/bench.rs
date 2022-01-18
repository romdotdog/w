#![allow(clippy::missing_panics_doc)]

use criterion::{black_box, criterion_group, criterion_main, Criterion};
use w_ast::Span;
use w_errors::Message;
use w_lexer::Lexer;
use w_parser::{Handler, Parser};

struct Session<'a> {
    src: &'a str,
}

impl<'a> Handler<'a> for Session<'a> {
    type SourceRef = ();

    fn error(&self, _src_ref: &Self::SourceRef, _msg: Message, _span: Span) {
        panic!("source errored in bench");
    }

    fn load_source(&'a self, _name: String) -> Option<&'a Self::SourceRef> {
        panic!("imports are not allowed in parser tests.");
    }

    fn get_source(&self, _src_ref: &'a Self::SourceRef) -> &'a [u8] {
        self.src.as_bytes()
    }
}

pub fn criterion_benchmark(c: &mut Criterion) {
    let src = std::fs::read_to_string("math/math.w").unwrap();

    let count = Lexer::new(src.as_bytes()).count();
    c.bench_function(format!("lexer {} tk", count).as_str(), |b| {
        b.iter(|| for _t in Lexer::new(black_box(&src).as_bytes()) {});
    });

    c.bench_function(format!("parser {} tk", count).as_str(), |b| {
        b.iter(|| {
            let handler = Session {
                src: black_box(&src),
            };

            let parser = Parser::new(&handler, &());
            parser.parse();
        });
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
