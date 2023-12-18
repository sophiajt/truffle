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
async fn async_print_4int(i: i64, j: i64, k: i64, l: i64) {
    println!("hello from async {i}, {j}, {k}, {l}")
}

#[derive(Debug)]
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
async fn async_print_custom(t: &mut MutableCustomType) {
    t.content.push_str("hello");
    println!("t: {t:?}")
}
// fn register_async_print_custom(name: &'static str) -> impl Fn(&mut truffle::Engine) {
//     use futures::FutureExt;
//     fn wrapped_fn<'a>(
//         mut t: &'a mut ::truffle::Value,
//     ) -> futures::future::BoxFuture<'a, Result<::truffle::Value, String>> {
//         async move {
//             let t = t
//                 .downcast_mut()
//                 .expect("downcast type should match the actual type");
//             Ok(Box::new(async_print_custom(t).await) as ::truffle::Value)
//         }
//             .boxed()
//     }
//     |engine: &mut Engine| {
//         use truffle::Function;
//         engine
//             .add_async_call(
//                 <[_]>::into_vec(
//                     #[rustc_box]
//                     ::alloc::boxed::Box::new([
//                         engine
//                             .get_type::<&mut MutableCustomType>()
//                             .expect("engine should already know about this type"),
//                     ]),
//                 ),
//                 engine
//                     .get_type::<()>()
//                     .expect("engine should already know about this type"),
//                 Function::ExternalAsyncFn1(wrapped_fn),
//                 name,
//                 async_print_custom_location(),
//             );
//     }
// }
// fn async_print_custom_is_async() -> bool {
//     true
// }
// fn async_print_custom_location() -> &'static std::panic::Location<'static> {
//     std::panic::Location::caller()
// }

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
