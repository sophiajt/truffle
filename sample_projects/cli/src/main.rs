use std::fmt::Display;

use truffle::{export, register_fn, Engine, FnRegister};

#[export]
fn print<T: Display>(x: T) {
    println!("{}", x)
}

fn main() {
    let mut engine = Engine::new();
    engine.set_app_name("cli");

    register_fn!(engine, "print", print::<i64>);

    for x in std::env::args().skip(1) {
        let contents = std::fs::read_to_string(&x).unwrap();
        println!("{:?}", engine.eval_source(x, contents.as_bytes(), false))
    }
}
