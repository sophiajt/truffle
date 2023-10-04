#![cfg(feature = "lsp")]
use truffle::{register_fn, Engine, ErrorBatch, FnRegister};

// Script Builtins
#[cfg_attr(feature = "async", truffle::export)]
pub fn add<T: std::ops::Add>(lhs: T, rhs: T) -> T::Output {
    lhs + rhs
}

#[allow(unused)]
#[cfg_attr(feature = "async", truffle::export)]
pub fn print<T: std::fmt::Display>(value: T) {
    println!("value: {value}")
}

#[test]
pub fn serialize() -> Result<(), ErrorBatch> {
    let mut engine = Engine::new();
    register_fn!(engine, "print", print::<i64>);
    register_fn!(engine, "print", print::<f64>);
    register_fn!(engine, "print", print::<bool>);
    register_fn!(engine, "print", print::<String>);
    register_fn!(engine, "add", add::<i64>);
    register_fn!(engine, "add", add::<f64>);

    #[cfg(feature = "async")]
    {
        #[truffle::export]
        async fn modify_this(this: i64) -> i64 {
            this + 100
        }

        #[truffle::export]
        async fn modify_that(that: i64) -> i64 {
            that - 50
        }
        register_fn!(engine, "modify_this", modify_this);
        register_fn!(engine, "modify_that", modify_that);
    }

    let data = postcard::to_allocvec(&engine).unwrap();
    dbg!(&data);
    let _engine_clone: Engine = postcard::from_bytes(&data).unwrap();
    Ok(())
}
