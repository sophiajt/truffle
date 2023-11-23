use criterion::{black_box, criterion_group, criterion_main, Criterion};

#[cfg(feature = "async")]
fn run_file(filename: &str) {
    use futures::executor::block_on;
    use truffle::Engine;

    let contents = std::fs::read(filename).expect("couldn't find file");

    let engine = Engine::new();

    let _ = block_on(engine.eval_source_async(filename, &contents, false));
}

#[cfg(not(feature = "async"))]
fn run_file(filename: &str) {
    use truffle::Engine;

    let contents = std::fs::read(filename).expect("couldn't find file");

    let engine = Engine::new();

    let _ = engine.eval_source(filename, &contents, false);
}

fn parser_benchmark(c: &mut Criterion) {
    c.bench_function("parser_experiment: hot_loop_no_print.truffle", |b| {
        b.iter(|| run_file(black_box("samples/hot_loop_no_print.truffle")))
    });
}

criterion_group!(benches, parser_benchmark);
criterion_main!(benches);
