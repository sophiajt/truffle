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

// #[export]
async fn async_print_int(i: i64) {
    println!("hello from async {i}")
}
fn register_async_print_int() -> impl Fn(&mut truffle::Engine) {
    use futures::FutureExt;
    fn wrapped_fn(
        mut i: &mut Box<dyn std::any::Any + Send>,
    ) -> futures::future::BoxFuture<'static, Result<Box<dyn std::any::Any + '_>, String>> {
        async move {
            let i = i
                .downcast_mut()
                .expect("downcast type should match the actual type");
            Ok(Box::new(async_print_int(*i).await) as Box<dyn std::any::Any>)
        }
        .boxed()
    }
    |engine: &mut Engine| {
        use truffle::Function;
        engine.add_async_call(
            <[_]>::into_vec(Box::new([engine
                .get_type::<i64>()
                .expect("engine should already know about this type")])),
            engine
                .get_type::<()>()
                .expect("engine should already know about this type"),
            Function::ExternalAsyncFn1(wrapped_fn),
            "async_print_int",
            async_print_int_location(),
        );
    }
}
fn async_print_int_is_async() -> bool {
    true
}
fn async_print_int_location() -> &'static std::panic::Location<'static> {
    std::panic::Location::caller()
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
