mod line_editor;
use std::collections::HashMap;

use line_editor::{LineEditor, ReadLineOutput};

use truffle::{register_fn, FnRegister, Parser, Translater, TypeChecker, BOOL_TYPE, F64_TYPE};

fn main() {
    let args = std::env::args();

    if args.len() > 1 {
        for arg in args.skip(1) {
            let contents = std::fs::read_to_string(arg).expect("couldn't find file");

            run_line(&contents, true);
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
                    run_line(&line, debug_output);
                }
            }
            Err(err) => {
                println!("{:?}", err);
            }
        }
    }
}

fn run_line(line: &str, debug_output: bool) {
    if debug_output {
        println!("line: {line}");
    }

    let mut parser = Parser::new(line.as_bytes(), 0, 0);

    parser.parse();

    for error in &parser.errors {
        println!("error: {:?}", error);
    }

    if !parser.errors.is_empty() {
        return;
    }

    let result = &parser.delta;

    if debug_output {
        println!();
        println!("parse result:");
        result.print();
    }

    let mut typechecker = TypeChecker::new();
    register_fn!(typechecker, "print", print::<i64>);
    register_fn!(typechecker, "print", print::<f64>);
    register_fn!(typechecker, "print", print::<bool>);
    register_fn!(typechecker, "add", add::<i64>);
    register_fn!(typechecker, "add", add::<f64>);
    register_fn!(typechecker, "new_env", Env::new_env);
    register_fn!(typechecker, "set_var", Env::set_var);
    register_fn!(typechecker, "read_var", Env::read_var);
    typechecker.typecheck(&parser.delta);

    for error in &typechecker.errors {
        println!("error: {:?}", error);
    }

    if !typechecker.errors.is_empty() {
        return;
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
    let result = output.eval(&typechecker.functions);
    if debug_output {
        println!("============");
        println!();
        output.debug_print(&typechecker);
        println!();
    }

    if result.1 == F64_TYPE {
        println!(
            "result -> {} ({})",
            unsafe { std::mem::transmute::<i64, f64>(result.0) },
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
        Env { vars: HashMap::new() }
    }

    pub fn set_var(&self, var: i64, value: i64) {
        todo!()
    }

    pub fn read_var(&self, var: i64) -> i64 {
        todo!()
    }
}