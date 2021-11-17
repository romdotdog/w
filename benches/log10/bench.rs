use criterion::{black_box, criterion_group, criterion_main, Criterion};
use w_lexer::Lexer;

pub fn criterion_benchmark(c: &mut Criterion) {
    let src = std::fs::read_to_string("D:/w/benches/log10/log10.w").unwrap();

    let mut count = 0;
    for _ in Lexer::new(src.chars()) {
        count += 1;
    }

    c.bench_function(format!("lexer {} tk", count).as_str(), |b| {
        b.iter(|| for _ in Lexer::new(black_box(&src).chars()) {})
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
