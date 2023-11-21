mod line_editor;

use std::path::Path;

use line_editor::{LineEditor, ReadLineOutput};

use truffle::{register_fn, FnRegister, ReturnValue};

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

fn print_result(result: ReturnValue) {
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
    }
}

#[cfg(feature = "async")]
fn run_line<P>(fname: P, source: &str, debug_output: bool) -> Option<()>
where
    P: AsRef<Path>,
{
    use futures::executor::block_on;
    use truffle::Engine;

    let fname = fname.as_ref();
    let contents = source.as_bytes();

    let mut engine = Engine::new();
    engine.set_app_name("repl");

    register_fn!(engine, "print", print::<i64>);
    register_fn!(engine, "print", print::<f64>);
    register_fn!(engine, "print", print::<bool>);
    register_fn!(engine, "add", add::<i64>);
    register_fn!(engine, "add", add::<f64>);

    let result = block_on(engine.eval_source_async(fname, source.as_bytes(), debug_output));
    match result {
        Ok(result) => {
            print_result(result);
        }
        Err(errors) => {
            errors.print_with(fname, contents);
            return None;
        }
    }

    Some(())
}

#[cfg(not(feature = "async"))]
fn run_line<P>(fname: P, source: &str, debug_output: bool) -> Option<()>
where
    P: AsRef<Path>,
{
    use truffle::Engine;

    let fname = fname.as_ref();
    let contents = source.as_bytes();

    let mut engine = Engine::new();
    engine.set_app_name("repl");

    register_fn!(engine, "print", print::<i64>);
    register_fn!(engine, "print", print::<f64>);
    register_fn!(engine, "print", print::<bool>);
    register_fn!(engine, "add", add::<i64>);
    register_fn!(engine, "add", add::<f64>);
    engine.print_fn_infos();

    let result = engine.eval_source(fname, contents, debug_output);
    match result {
        Ok(result) => {
            print_result(result);
        }
        Err(errors) => {
            errors.print_with(fname, contents);
            return None;
        }
    }

    Some(())
}

#[cfg_attr(any(feature = "async", feature = "lsp"), truffle::export)]
pub fn print<T: std::fmt::Display>(value: T) {
    println!("value: {value}")
}

#[cfg_attr(any(feature = "async", feature = "lsp"), truffle::export)]
pub fn add<T: std::ops::Add>(lhs: T, rhs: T) -> T::Output {
    lhs + rhs
}
