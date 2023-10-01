mod line_editor;

use std::{collections::HashMap, path::Path};

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

    register_fn!(engine, "print", print::<i64>);
    register_fn!(engine, "print", print::<f64>);
    register_fn!(engine, "print", print::<bool>);
    register_fn!(engine, "add", add::<i64>);
    register_fn!(engine, "add", add::<f64>);
    register_fn!(engine, "new_env", Env::new_env);
    register_fn!(engine, "set_var", Env::set_var);
    register_fn!(engine, "read_var", Env::read_var);

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

    register_fn!(engine, "print", print::<i64>);
    register_fn!(engine, "print", print::<f64>);
    register_fn!(engine, "print", print::<bool>);
    register_fn!(engine, "add", add::<i64>);
    register_fn!(engine, "add", add::<f64>);
    register_fn!(engine, "new_env", Env::new_env);
    register_fn!(engine, "set_var", Env::set_var);
    register_fn!(engine, "read_var", Env::read_var);

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

    // JT: FIXME
    // let mut lexer = Lexer::new(source.as_bytes().to_vec(), 0);

    // let tokens = match lexer.lex() {
    //     Ok(tokens) => tokens,
    //     Err(errors) => {
    //         errors.print_with(fname, contents);
    //         return None;
    //     }
    // };

    // let mut parser = Parser::new(tokens, source.as_bytes().to_vec(), 0);

    // match parser.parse() {
    //     Ok(()) => {}
    //     Err(errors) => {
    //         errors.print_with(fname, contents);
    //         return None;
    //     }
    // }
    // if debug_output {
    //     parser.results.print();
    // }

    // let mut typechecker = TypeChecker::new(parser.results);

    // match typechecker.typecheck() {
    //     Ok(_) => {}
    //     Err(errors) => {
    //         errors.print_with(fname, contents);
    //         return None;
    //     }
    // }

    // let mut translater = Translater::new(typechecker);

    // #[allow(unused_mut)]
    // let mut output = translater.translate();

    // // output.debug_output();

    // let mut evaluator = Evaluator::default();
    // evaluator.add_function(output);

    // let result = evaluator.eval(FunctionId(0), &translater.typechecker.local_functions);

    // print_result(&translater.typechecker, fname, result, source.as_bytes());

    Some(())
}

#[cfg_attr(feature = "async", truffle::export)]
pub fn print<T: std::fmt::Display>(value: T) {
    println!("value: {value}")
}

#[cfg_attr(feature = "async", truffle::export)]
pub fn add<T: std::ops::Add>(lhs: T, rhs: T) -> T::Output {
    lhs + rhs
}

pub struct Env {
    vars: HashMap<i64, i64>,
}

impl Env {
    #[cfg_attr(feature = "async", truffle::export)]
    pub fn new_env() -> Env {
        Env {
            vars: HashMap::new(),
        }
    }

    #[cfg_attr(feature = "async", truffle::export)]
    pub fn set_var(&mut self, var: i64, value: i64) {
        self.vars.insert(var, value);
    }

    #[cfg_attr(feature = "async", truffle::export)]
    pub fn read_var(&self, var: i64) -> i64 {
        *self.vars.get(&var).unwrap()
    }
}

// FIXME: test functions
#[cfg(feature = "async")]
async fn modify_this(this: i64) -> i64 {
    this + 100
}

#[cfg(feature = "async")]
fn wrapped_fn(
    mut this: Box<dyn std::any::Any + Send>,
) -> futures::future::BoxFuture<'static, Result<Box<dyn std::any::Any>, String>> {
    use futures::FutureExt;

    async move {
        let this = this.downcast_mut().unwrap();
        Ok(Box::new(modify_this(*this).await) as Box<dyn std::any::Any>)
    }
    .boxed()
}
