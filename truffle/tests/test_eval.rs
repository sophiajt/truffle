use std::collections::HashMap;

use truffle::{register_fn, Engine, ErrorBatch, FnRegister, ReturnValue};

#[cfg(feature = "async")]
pub fn eval_source(source: &str) -> Result<ReturnValue, ErrorBatch> {
    use futures::executor::block_on;

    #[truffle::export]
    async fn modify_this(this: i64) -> i64 {
        this + 100
    }

    #[truffle::export]
    async fn modify_that(that: i64) -> i64 {
        that - 50
    }

    let mut engine = Engine::new();
    register_fn!(engine, "print", print::<i64>);
    register_fn!(engine, "print", print::<f64>);
    register_fn!(engine, "print", print::<bool>);
    register_fn!(engine, "print", print::<String>);
    register_fn!(engine, "add", add::<i64>);
    register_fn!(engine, "add", add::<f64>);
    register_fn!(engine, "modify_this", modify_this);
    register_fn!(engine, "modify_that", modify_that);
    register_fn!(engine, "new_env", Env::new_env);
    register_fn!(engine, "set_var", Env::set_var);
    register_fn!(engine, "read_var", Env::read_var);

    block_on(engine.eval_source_async("test", source.as_bytes(), false))
}

#[cfg(not(feature = "async"))]
pub fn eval_source(source: &str) -> Result<ReturnValue, ErrorBatch> {
    let mut engine = Engine::new("test_eval");
    register_fn!(engine, "print", print::<i64>);
    register_fn!(engine, "print", print::<f64>);
    register_fn!(engine, "print", print::<bool>);
    register_fn!(engine, "print", print::<String>);
    register_fn!(engine, "add", add::<i64>);
    register_fn!(engine, "add", add::<f64>);
    register_fn!(engine, "new_env", Env::new_env);
    register_fn!(engine, "set_var", Env::set_var);
    register_fn!(engine, "read_var", Env::read_var);

    engine.eval_source("test", source.as_bytes(), false)
}

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

pub struct Env {
    vars: HashMap<String, i64>,
}

impl Env {
    #[cfg_attr(feature = "async", truffle::export)]
    pub fn new_env() -> Env {
        Env {
            vars: HashMap::new(),
        }
    }

    #[cfg_attr(feature = "async", truffle::export)]
    pub fn set_var(&mut self, var: String, value: i64) {
        self.vars.insert(var, value);
    }

    #[cfg_attr(feature = "async", truffle::export)]
    pub fn read_var(&mut self, var: String) -> i64 {
        *self.vars.get(&var).unwrap()
    }
}
