use truffle::{
    register_fn, Evaluator, FnRegister, FunctionId, Lexer, Parser, ReturnValue, ScriptError,
    Translater, TypeChecker,
};

#[allow(unused)]
pub fn compile_to_error(source: &str) -> Vec<ScriptError> {
    let mut lexer = Lexer::new(source.as_bytes().to_vec(), 0);

    let tokens = lexer.lex();
    if !lexer.errors.is_empty() {
        return lexer.errors;
    }

    let mut parser = Parser::new(tokens, source.as_bytes().to_vec(), 0);
    parser.parse();
    if !parser.errors.is_empty() {
        return parser.errors;
    }

    let mut typechecker = TypeChecker::new(parser.results);
    typechecker.typecheck();
    if !typechecker.errors.is_empty() {
        return typechecker.errors;
    }

    vec![]
}

#[allow(unused)]
#[cfg(feature = "async")]
pub fn run_to_error(source: &str) -> Vec<ScriptError> {
    use futures::executor::block_on;
    let mut lexer = Lexer::new(source.as_bytes().to_vec(), 0);

    let tokens = lexer.lex();

    let mut parser = Parser::new(tokens, source.as_bytes().to_vec(), 0);
    parser.parse();

    let mut typechecker = TypeChecker::new(parser.results);
    typechecker.typecheck();

    let mut translater = Translater::new(typechecker);

    #[allow(unused_mut)]
    let mut output = translater.translate();

    let mut evaluator = Evaluator::default();
    evaluator.add_function(output);

    let result = block_on(evaluator.eval(FunctionId(0), &translater.typechecker.functions));

    match result {
        ReturnValue::Error(error) => vec![error],
        _ => vec![],
    }
}

#[allow(unused)]
#[cfg(not(feature = "async"))]
pub fn run_to_error(source: &str) -> Vec<ScriptError> {
    let mut lexer = Lexer::new(source.as_bytes().to_vec(), 0);

    let tokens = lexer.lex();
    if !lexer.errors.is_empty() {
        return lexer.errors;
    }

    let mut parser = Parser::new(tokens, source.as_bytes().to_vec(), 0);
    parser.parse();
    if !parser.errors.is_empty() {
        return parser.errors;
    }

    let mut typechecker = TypeChecker::new(parser.results);
    typechecker.typecheck();
    if !typechecker.errors.is_empty() {
        return typechecker.errors;
    }

    let mut translater = Translater::new(typechecker);

    #[allow(unused_mut)]
    let mut output = translater.translate();

    let mut evaluator = Evaluator::default();
    evaluator.add_function(output);

    let result = evaluator.eval(FunctionId(0), &translater.typechecker.functions);

    match result {
        ReturnValue::Error(error) => vec![error],
        _ => vec![],
    }
}

#[allow(unused)]
pub fn error_contains(errors: &[ScriptError], expected: &str) -> bool {
    for error in errors {
        if error.message.contains(expected) {
            return true;
        }
    }

    false
}

#[cfg(feature = "async")]
pub fn eval_source(source: &str) -> ReturnValue {
    use futures::executor::block_on;

    let mut lexer = Lexer::new(source.as_bytes().to_vec(), 0);

    let tokens = lexer.lex();
    let mut parser = Parser::new(tokens, source.as_bytes().to_vec(), 0);
    parser.parse();

    let mut typechecker = TypeChecker::new(parser.results);
    register_fn!(typechecker, "add", add::<i64>);
    register_fn!(typechecker, "add", add::<f64>);

    typechecker.typecheck();

    let mut translater = Translater::new(typechecker);

    #[allow(unused_mut)]
    let mut output = translater.translate();

    let mut evaluator = Evaluator::default();
    evaluator.add_function(output);

    block_on(evaluator.eval(FunctionId(0), &translater.typechecker.functions))
}

#[cfg(not(feature = "async"))]
pub fn eval_source(source: &str) -> Result<ReturnValue, Vec<ScriptError>> {
    let mut lexer = Lexer::new(source.as_bytes().to_vec(), 0);

    let tokens = lexer.lex();
    let mut parser = Parser::new(tokens, source.as_bytes().to_vec(), 0);
    parser.parse();

    let mut typechecker = TypeChecker::new(parser.results);
    register_fn!(typechecker, "print", print::<i64>);
    register_fn!(typechecker, "print", print::<f64>);
    register_fn!(typechecker, "print", print::<bool>);
    register_fn!(typechecker, "print", print::<String>);
    register_fn!(typechecker, "add", add::<i64>);
    register_fn!(typechecker, "add", add::<f64>);

    match typechecker.typecheck() {
        Ok(_) => {}
        Err(errors) => return Err(errors),
    }

    let mut translater = Translater::new(typechecker);

    #[allow(unused_mut)]
    let mut output = translater.translate();

    let mut evaluator = Evaluator::default();
    evaluator.add_function(output);

    Ok(evaluator.eval(FunctionId(0), &translater.typechecker.functions))
}

// Script Builtins
pub fn add<T: std::ops::Add>(lhs: T, rhs: T) -> T::Output {
    lhs + rhs
}

#[allow(unused)]
pub fn print<T: std::fmt::Display>(value: T) {
    println!("value: {value}")
}
