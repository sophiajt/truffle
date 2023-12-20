use std::fmt::Display;
use truffle::{export, register_fn, Engine, FnRegister};

const SCRIPT_NAME: &str = "scripts/example.async.truffle";

#[export]
fn print<T: Display>(x: T) {
    println!("{}", x)
}

#[export]
async fn async_print() {
    println!("hello from async")
}

#[export]
async fn async_print_int(i: i64) {
    println!("hello from async {i}")
}

#[export]
async fn async_print_4int(i: i64, j: f64, k: bool, l: String) {
    println!("hello from async {i}, {j}, {k}, {l}")
}

#[derive(Debug, Clone)]
struct MutableCustomType {
    content: String,
}

impl MutableCustomType {
    #[export]
    fn new() -> Self {
        Self {
            content: Default::default(),
        }
    }
}

#[export]
async fn async_print_custom(mut t: MutableCustomType) -> MutableCustomType {
    t.content.push_str("hello");
    println!("t: {t:?}");
    t
}

fn main() {
    let mut engine = Engine::new();
    engine.set_app_name("async");

    register_fn!(engine, "print", print::<i64>);
    register_fn!(engine, "print", print::<String>);
    register_fn!(engine, "async_print", async_print);
    register_fn!(engine, "async_print", async_print_int);
    register_fn!(engine, "async_print", async_print_4int);
    // register_fn!(engine, "async_print", async_print_custom);
    engine.with(register_async_print_custom("async_print"));
    register_fn!(engine, "custom_new", MutableCustomType::new);

    let contents = std::fs::read_to_string(SCRIPT_NAME).unwrap();
    let results = futures::executor::block_on(engine.eval_source_async(
        SCRIPT_NAME,
        contents.as_bytes(),
        false,
    ));
    println!("{:?}", results)
}
