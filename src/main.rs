mod line_editor;
use std::collections::HashMap;

use line_editor::{LineEditor, ReadLineOutput};

use truffle::{
    print_error, register_fn, Evaluator, FnRegister, FunctionCodegen, FunctionId, Parser,
    Translater, TypeChecker, TypeId, BOOL_TYPE, F64_TYPE,
};

fn main() {
    let args = std::env::args();

    if args.len() > 1 {
        for arg in args.skip(1) {
            let contents = std::fs::read_to_string(&arg).expect("couldn't find file");

            run_line(&arg, &contents, true);
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
                    run_line("<repl>", &line, debug_output);
                }
            }
            Err(err) => {
                println!("{:?}", err);
            }
        }
    }
}

fn parse_line(fname: &str, line: &str, debug_output: bool) -> Option<FunctionCodegen> {
    let mut parser = Parser::new(line.as_bytes(), 0, 0);

    let mut typechecker = TypeChecker::new();
    register_fn!(typechecker, "print", print::<i64>);
    register_fn!(typechecker, "print", print::<f64>);
    register_fn!(typechecker, "print", print::<bool>);
    register_fn!(typechecker, "add", add::<i64>);
    register_fn!(typechecker, "add", add::<f64>);
    register_fn!(typechecker, "new_env", Env::new_env);
    register_fn!(typechecker, "set_var", Env::set_var);
    register_fn!(typechecker, "read_var", Env::read_var);

    if debug_output {
        println!("line: {line}");
    }

    parser.parse();

    if !parser.errors.is_empty() {
        for err in &parser.errors {
            print_error(fname, err, &parser)
        }
        return None;
    }

    let result = &parser.delta;

    if debug_output {
        println!();
        println!("parse result:");
        result.print();
    }

    typechecker.typecheck(&parser.delta);

    if !typechecker.errors.is_empty() {
        for err in &typechecker.errors {
            print_error(fname, err, &parser)
        }
        return None;
    }

    let result = &parser.delta;

    if debug_output {
        println!();
        println!("parse result:");
        result.print();
    }
    let mut idx = 0;

    if debug_output {
        println!();
        println!("typed nodes:");
        while idx < result.ast_nodes.len() {
            println!(
                "  {}: {:?} ({})",
                idx,
                result.ast_nodes[idx],
                typechecker.stringify_type(typechecker.node_types[idx])
            );
            idx += 1;
        }
    }

    let mut translater = Translater::new();

    #[allow(unused_mut)]
    let mut output = translater.translate(&parser.delta, &typechecker);

    if debug_output {
        println!();
        println!("===stdout===");
    }

    Some(output)
}

fn print_result(typechecker: &TypeChecker, result: (i64, TypeId)) {
    if result.1 == F64_TYPE {
        println!(
            "result -> {} ({})",
            f64::from_bits(result.0 as u64),
            typechecker.stringify_type(result.1)
        );
    } else if result.1 == BOOL_TYPE {
        println!(
            "result -> {} ({})",
            result.0 != 0,
            typechecker.stringify_type(result.1)
        );
    } else {
        println!(
            "result -> {} ({})",
            result.0,
            typechecker.stringify_type(result.1)
        );
    }
}

#[cfg(feature = "async")]
fn run_line(fname: &str, line: &str, debug_output: bool) {
    use futures::executor::block_on;

    let mut typechecker = TypeChecker::new();
    register_fn!(typechecker, "print", print::<i64>);
    register_fn!(typechecker, "print", print::<f64>);
    register_fn!(typechecker, "print", print::<bool>);
    register_fn!(typechecker, "add", add::<i64>);
    register_fn!(typechecker, "add", add::<f64>);
    register_fn!(typechecker, "new_env", Env::new_env);
    register_fn!(typechecker, "set_var", Env::set_var);
    register_fn!(typechecker, "read_var", Env::read_var);

    let output = parse_line(fname, line, debug_output);

    if let Some(output) = output {
        let mut evaluator = Evaluator::default();
        evaluator.add_function(output);

        let result = block_on(evaluator.eval(FunctionId(0), &typechecker.functions));
        if debug_output {
            println!("============");
            println!();
            evaluator.debug_print(&typechecker);
            println!();
        }

        print_result(&typechecker, result);
    }
}

#[cfg(not(feature = "async"))]
fn run_line(fname: &str, line: &str, debug_output: bool) {
    let mut typechecker = TypeChecker::new();
    register_fn!(typechecker, "print", print::<i64>);
    register_fn!(typechecker, "print", print::<f64>);
    register_fn!(typechecker, "print", print::<bool>);
    register_fn!(typechecker, "add", add::<i64>);
    register_fn!(typechecker, "add", add::<f64>);
    register_fn!(typechecker, "new_env", Env::new_env);
    register_fn!(typechecker, "set_var", Env::set_var);
    register_fn!(typechecker, "read_var", Env::read_var);

    let output = parse_line(fname, line, debug_output);

    if let Some(output) = output {
        let mut evaluator = Evaluator::default();
        evaluator.add_function(output);

        let result = evaluator.eval(FunctionId(0), &typechecker.functions);
        if debug_output {
            println!("============");
            println!();
            evaluator.debug_print(&typechecker);
            println!();
        }

        print_result(&typechecker, result);
    }
}

// FIXME: move these later when we build up cranelift registration
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
