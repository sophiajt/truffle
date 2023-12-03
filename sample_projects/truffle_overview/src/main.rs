use std::fmt::Display;
use truffle::{export, register_fn, Engine, FnRegister};

const SCRIPT_NAME: &'static str = "scripts/example.overview.truffle";

struct State {
    age: i64
}

impl State {
    #[export]
    fn new(age: i64) -> Self {
        Self { age }
    }

    #[export]
    fn set_age(&mut self, age: i64) {
        self.age = age;
    }

    #[export]
    fn get_age(&mut self) -> i64 {
        self.age
    }
}

#[export]
fn print<T: Display>(x: T) {
    println!("{}", x)
}

fn main() {
    let mut engine = Engine::new();
    engine.set_app_name("overview");

    register_fn!(engine, "print", print::<i64>);
    register_fn!(engine, "print", print::<String>);
    register_fn!(engine, "new_state", State::new);
    register_fn!(engine, "set_age", State::set_age);
    register_fn!(engine, "get_age", State::get_age);

    let contents = std::fs::read_to_string(SCRIPT_NAME).unwrap();
    println!("{:?}", engine.eval_source(SCRIPT_NAME, contents.as_bytes(), false))
}
