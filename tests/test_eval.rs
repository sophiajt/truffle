use truffle::{
    register_fn, ErrorBatch, Evaluator, FnRegister, FunctionId, Lexer, Parser, ReturnValue,
    Translater, TypeChecker,
};

#[cfg(feature = "async")]
pub fn eval_source(source: &str) -> Result<ReturnValue, ErrorBatch> {
    use futures::executor::block_on;

    let mut lexer = Lexer::new(source.as_bytes().to_vec(), 0);

    let tokens = lexer.lex()?;
    let mut parser = Parser::new(tokens, source.as_bytes().to_vec(), 0);
    parser.parse()?;

    let mut typechecker = TypeChecker::new(parser.results);
    register_fn!(typechecker, "print", print::<i64>);
    register_fn!(typechecker, "print", print::<f64>);
    register_fn!(typechecker, "print", print::<bool>);
    register_fn!(typechecker, "print", print::<String>);
    register_fn!(typechecker, "add", add::<i64>);
    register_fn!(typechecker, "add", add::<f64>);
    register_fn!(typechecker, "test_async_fn", test_async_fn);

    typechecker.typecheck()?;

    let mut translater = Translater::new(typechecker);

    #[allow(unused_mut)]
    let mut output = translater.translate();

    let mut evaluator = Evaluator::default();
    evaluator.add_function(output);

    match block_on(evaluator.eval(FunctionId(0), &translater.typechecker.functions)) {
        ReturnValue::Error(error) => Err(error.into()),
        return_value => Ok(return_value),
    }
}

#[cfg(not(feature = "async"))]
pub fn eval_source(source: &str) -> Result<ReturnValue, ErrorBatch> {
    let mut lexer = Lexer::new(source.as_bytes().to_vec(), 0);

    let tokens = lexer.lex()?;
    let mut parser = Parser::new(tokens, source.as_bytes().to_vec(), 0);
    parser.parse()?;

    let mut typechecker = TypeChecker::new(parser.results);
    register_fn!(typechecker, "print", print::<i64>);
    register_fn!(typechecker, "print", print::<f64>);
    register_fn!(typechecker, "print", print::<bool>);
    register_fn!(typechecker, "print", print::<String>);
    register_fn!(typechecker, "add", add::<i64>);
    register_fn!(typechecker, "add", add::<f64>);

    typechecker.typecheck()?;

    let mut translater = Translater::new(typechecker);

    #[allow(unused_mut)]
    let mut output = translater.translate();

    let mut evaluator = Evaluator::default();
    evaluator.add_function(output);

    match evaluator.eval(FunctionId(0), &translater.typechecker.functions) {
        ReturnValue::Error(error) => Err(error.into()),
        return_value => Ok(return_value),
    }
}

// Script Builtins
pub fn add<T: std::ops::Add>(lhs: T, rhs: T) -> T::Output {
    lhs + rhs
}

#[allow(unused)]
pub fn print<T: std::fmt::Display>(value: T) {
    println!("value: {value}")
}

pub fn test_async_fn() -> i64 {
    42
}
