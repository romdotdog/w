#![allow(clippy::missing_panics_doc)]

use criterion::{black_box, criterion_group, criterion_main, Criterion};
use w_ast::Span;
use w_errors::Message;
use w_lexer::Lexer;
use w_parser::{
    handler::{ImportlessHandler, ImportlessHandlerHandler},
    Parser,
};

struct Session;
impl<'ast> ImportlessHandler<'ast> for Session {
    fn error(&self, _msg: Message, _span: Span) {
        panic!("source errored in bench");
    }
}

pub fn criterion_benchmark(c: &mut Criterion) {
    let src = std::fs::read_to_string("math/math.w").unwrap();

    let count = Lexer::from_str(&src).count();
    c.bench_function(format!("lexer {} tk", count).as_str(), |b| {
        b.iter(|| for _t in Lexer::from_str(black_box(&src)) {});
    });

    c.bench_function(format!("parser {} tk", count).as_str(), |b| {
        b.iter(|| {
            let session = Session;
            let handler = ImportlessHandlerHandler { handler: &session };
            let parser = Parser::partial_parse(&handler, black_box(&src));
            parser.parse();
        });
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
