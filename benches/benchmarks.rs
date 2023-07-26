use criterion::{black_box, criterion_group, criterion_main, Criterion};

use truffle::{Evaluator, FunctionId, Parser, Translater, TypeChecker};

#[cfg(feature = "async")]
fn run_file(filename: &str) {
    use futures::executor::block_on;
    use truffle::Lexer;

    let contents = std::fs::read(filename).expect("couldn't find file");

    let mut lexer = Lexer::new(contents.clone(), 0);

    let tokens = lexer.lex();
    let mut parser = Parser::new(tokens, contents, 0);
    parser.parse();

    let mut typechecker = TypeChecker::new(parser.results);
    typechecker.typecheck();

    let mut translater = Translater::new(typechecker);

    #[allow(unused_mut)]
    let mut output = translater.translate();

    let mut evaluator = Evaluator::default();
    evaluator.add_function(output);

    let _ = block_on(evaluator.eval(FunctionId(0), &translater.typechecker.functions));
}

#[cfg(not(feature = "async"))]
fn run_file(filename: &str) {
    let contents = std::fs::read(filename).expect("couldn't find file");

    let mut parser = Parser::new(contents, 0, 0);
    parser.parse();

    let mut typechecker = TypeChecker::new();
    typechecker.typecheck(&parser.results);

    let mut translater = Translater::new();

    #[allow(unused_mut)]
    let mut output = translater.translate(&parser.results, &typechecker);

    let mut evaluator = Evaluator::default();
    evaluator.add_function(output);

    let _ = evaluator.eval(FunctionId(0), &typechecker.functions);
}

fn parser_benchmark(c: &mut Criterion) {
    c.bench_function("parser_experiment: hot_loop_no_print.truffle", |b| {
        b.iter(|| run_file(black_box("samples/hot_loop_no_print.truffle")))
    });
}

criterion_group!(benches, parser_benchmark);
criterion_main!(benches);
