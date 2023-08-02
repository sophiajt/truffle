mod line_editor;
use std::collections::HashMap;

use line_editor::{LineEditor, ReadLineOutput};

use truffle::{
    print_error, register_fn, Evaluator, FnRegister, FunctionId, Lexer, Parser, ReturnValue,
    Translater, TypeChecker, BOOL_TYPE, F64_TYPE,
};

fn main() {
    let args = std::env::args();

    if args.len() > 1 {
        for arg in args.skip(1) {
            let contents = std::fs::read_to_string(&arg).expect("couldn't find file");

            run_line(&arg, &contents);
        }
        return;
    }

    let mut line_editor = LineEditor::new();

    let mut debug_output = false;
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
                    run_line("<repl>", &line);
                }
            }
            Err(err) => {
                println!("{:?}", err);
            }
        }
    }
}

fn print_result(typechecker: &TypeChecker, fname: &str, result: ReturnValue, contents: &[u8]) {
    match result {
        ReturnValue::Bool(value) => {
            println!("result -> {} (bool)", value)
        }
        ReturnValue::I64(value) => {
            println!("result -> {} (i64)", value)
        }
        ReturnValue::F64(value) => {
            println!("result -> {} (f64)", value)
        }
        ReturnValue::Custom { value, type_id } => {
            if type_id == F64_TYPE {
                println!(
                    "result -> {} ({})",
                    f64::from_bits(value as u64),
                    typechecker.stringify_type(type_id)
                );
            } else if type_id == BOOL_TYPE {
                println!(
                    "result -> {} ({})",
                    value != 0,
                    typechecker.stringify_type(type_id)
                );
            } else {
                println!(
                    "result -> {} ({})",
                    value,
                    typechecker.stringify_type(type_id)
                );
            }
        }
        ReturnValue::Error(script_error) => print_error(fname, &script_error, contents),
    }
}

#[cfg(feature = "async")]
fn run_line(fname: &str, source: &str) -> Option<()> {
    use futures::executor::block_on;

    let mut lexer = Lexer::new(source.as_bytes().to_vec(), 0);

    if !lexer.errors.is_empty() {
        for err in &lexer.errors {
            print_error(fname, err, source.as_bytes())
        }
        return None;
    }

    let tokens = lexer.lex();
    let mut parser = Parser::new(tokens, source.as_bytes().to_vec(), 0);
    parser.parse();

    if !parser.errors.is_empty() {
        for err in &parser.errors {
            print_error(fname, err, source.as_bytes())
        }
        return None;
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

    typechecker.typecheck();

    if !typechecker.errors.is_empty() {
        for err in &typechecker.errors {
            print_error(fname, err, source.as_bytes())
        }
        return None;
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
fn run_line(fname: &str, source: &str) -> Option<()> {
    let mut lexer = Lexer::new(source.as_bytes().to_vec(), 0);

    if !lexer.errors.is_empty() {
        for err in &lexer.errors {
            print_error(fname, err, source.as_bytes())
        }
        return None;
    }

    let tokens = lexer.lex();
    let mut parser = Parser::new(tokens, source.as_bytes().to_vec(), 0);
    parser.parse();

    if !parser.errors.is_empty() {
        for err in &parser.errors {
            print_error(fname, err, source.as_bytes())
        }
        return None;
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
            for err in &errors {
                print_error(fname, err, source.as_bytes())
            }
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
