mod line_editor;
use std::{collections::HashMap, path::Path};

use line_editor::{LineEditor, ReadLineOutput};

use truffle::{
    register_fn, ErrorBatch, Evaluator, FnRegister, FunctionId, Lexer, Parser, ReturnValue,
    Translater, TypeChecker,
};

fn main() {
    let args = std::env::args();

    let mut debug_output = false;

    if args.len() > 1 {
        for arg in args.skip(1) {
            let contents = std::fs::read_to_string(&arg).expect("couldn't find file");

            run_line(&arg, &contents, debug_output);
        }
        return;
    }

    let mut line_editor = LineEditor::new();

    loop {
        match line_editor.read_line() {
            Ok(ReadLineOutput::Continue) => {
                continue;
            }
            Ok(ReadLineOutput::Break) => {
                break;
            }
            Ok(ReadLineOutput::Success(line)) => {
                if line == "exit" || line == "quit" {
                    break;
                } else if line == "debug" {
                    debug_output = !debug_output;
                } else {
                    run_line("<repl>", &line, debug_output);
                }
            }
            Err(err) => {
                println!("{:?}", err);
            }
        }
    }
}

fn print_result(_typechecker: &TypeChecker, fname: &Path, result: ReturnValue, contents: &[u8]) {
    match result {
        ReturnValue::Unit => {
            println!("result -> () (unit)");
        }
        ReturnValue::Bool(value) => {
            println!("result -> {value} (bool)")
        }
        ReturnValue::I64(value) => {
            println!("result -> {value} (i64)")
        }
        ReturnValue::F64(value) => {
            println!("result -> {value} (f64)")
        }
        ReturnValue::String(string) => {
            println!("result -> {string} (String)")
        }
        ReturnValue::Custom(_) => {
            println!("result -> <?> (Custom User Type)");
        }
        ReturnValue::Error(script_error) => {
            let errors = ErrorBatch::from(script_error);
            errors.print_with(fname, contents);
        }
    }
}

#[cfg(feature = "async")]
fn run_line<P>(fname: P, source: &str, debug_output: bool) -> Option<()>
where
    P: AsRef<Path>,
{
    use futures::executor::block_on;
    let contents = source.as_bytes();
    let fname = fname.as_ref();

    let mut lexer = Lexer::new(source.as_bytes().to_vec(), 0);
    let tokens = match lexer.lex() {
        Ok(tokens) => tokens,
        Err(errors) => {
            errors.print_with(fname, contents);
            return None;
        }
    };

    let mut parser = Parser::new(tokens, source.as_bytes().to_vec(), 0);
    match parser.parse() {
        Ok(()) => {}
        Err(errors) => {
            errors.print_with(fname, contents);
            return None;
        }
    }
    if debug_output {
        parser.results.print();
    }

    let mut typechecker = TypeChecker::new(parser.results);
    register_fn!(typechecker, "print", print::<i64>);
    register_fn!(typechecker, "print", print::<f64>);
    register_fn!(typechecker, "print", print::<bool>);
    register_fn!(typechecker, "add", add::<i64>);
    register_fn!(typechecker, "add", add::<f64>);
    register_fn!(typechecker, "new_env", Env::new_env);
    register_fn!(typechecker, "set_var", Env::set_var);
    register_fn!(typechecker, "read_var", Env::read_var);
    typechecker.add_async_call();

    match typechecker.typecheck() {
        Ok(_) => {}
        Err(errors) => {
            errors.print_with(fname, contents);
            return None;
        }
    }
    if debug_output {
        typechecker.print_node_types();
    }

    let mut translater = Translater::new(typechecker);

    #[allow(unused_mut)]
    let mut output = translater.translate();

    let mut evaluator = Evaluator::default();
    evaluator.add_function(output);

    let result = block_on(evaluator.eval(FunctionId(0), &translater.typechecker.functions));

    print_result(&translater.typechecker, fname, result, source.as_bytes());

    Some(())
}

#[cfg(not(feature = "async"))]
fn run_line<P>(fname: P, source: &str, debug_output: bool) -> Option<()>
where
    P: AsRef<Path>,
{
    let fname = fname.as_ref();
    let contents = source.as_bytes();
    let mut lexer = Lexer::new(source.as_bytes().to_vec(), 0);

    let tokens = match lexer.lex() {
        Ok(tokens) => tokens,
        Err(errors) => {
            errors.print_with(fname, contents);
            return None;
        }
    };

    let mut parser = Parser::new(tokens, source.as_bytes().to_vec(), 0);

    match parser.parse() {
        Ok(()) => {}
        Err(errors) => {
            errors.print_with(fname, contents);
            return None;
        }
    }
    if debug_output {
        parser.results.print();
    }

    let mut typechecker = TypeChecker::new(parser.results);
    register_fn!(typechecker, "print", print::<i64>);
    register_fn!(typechecker, "print", print::<f64>);
    register_fn!(typechecker, "print", print::<bool>);
    register_fn!(typechecker, "add", add::<i64>);
    register_fn!(typechecker, "add", add::<f64>);
    register_fn!(typechecker, "new_env", Env::new_env);
    register_fn!(typechecker, "set_var", Env::set_var);
    register_fn!(typechecker, "read_var", Env::read_var);

    match typechecker.typecheck() {
        Ok(_) => {}
        Err(errors) => {
            errors.print_with(fname, contents);
            return None;
        }
    }

    let mut translater = Translater::new(typechecker);

    #[allow(unused_mut)]
    let mut output = translater.translate();

    let mut evaluator = Evaluator::default();
    evaluator.add_function(output);

    let result = evaluator.eval(FunctionId(0), &translater.typechecker.functions);

    print_result(&translater.typechecker, fname, result, source.as_bytes());

    Some(())
}

pub fn print<T: std::fmt::Display>(value: T) {
    println!("value: {value}")
}

pub fn add<T: std::ops::Add>(lhs: T, rhs: T) -> T::Output {
    lhs + rhs
}

pub struct Env {
    vars: HashMap<i64, i64>,
}

impl Env {
    pub fn new_env() -> Env {
        Env {
            vars: HashMap::new(),
        }
    }

    pub fn set_var(&mut self, var: i64, value: i64) {
        self.vars.insert(var, value);
    }

    pub fn read_var(&self, var: i64) -> i64 {
        *self.vars.get(&var).unwrap()
    }
}
