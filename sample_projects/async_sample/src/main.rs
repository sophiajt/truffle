use std::fmt::Display;
use truffle::{export, register_fn, Engine, FnRegister};

const SCRIPT_NAME: &str = "scripts/example.async.truffle";

#[export]
fn print<T: Display>(x: T) {
    println!("{}", x)
}

#[export]
async fn async_print(x: i64) {
    println!("hello from async: {}", x)
}

fn main() {
    let mut engine = Engine::new();
    engine.set_app_name("async");

    register_fn!(engine, "print", print::<i64>);
    register_fn!(engine, "print", print::<String>);
    register_fn!(engine, "async_print", async_print);

    let contents = std::fs::read_to_string(SCRIPT_NAME).unwrap();
    let results = futures::executor::block_on(engine.eval_source_async(
        SCRIPT_NAME,
        contents.as_bytes(),
        false,
    ));
    println!("{:?}", results)
}
