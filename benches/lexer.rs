use criterion::{black_box, criterion_group, criterion_main, Criterion};
use w::Session;

pub fn criterion_benchmark(c: &mut Criterion) {
    let sess = Session::new();
    let file = sess.register_source(
        "fn.w".to_string(),
        std::fs::read_to_string("benches/fn.w").unwrap(),
    );

    let mut count = 0;
    for _ in sess.lexer(file) {
        count += 1;
    }

    c.bench_function(format!("lexer {} tk", count).as_str(), |b| {
        b.iter(|| for _ in sess.lexer(black_box(file)) {})
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
