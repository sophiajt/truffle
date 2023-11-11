#![cfg(feature = "lsp")]
use truffle::{register_fn, Engine, ErrorBatch, FnRegister, ScriptError, Span};

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
pub fn serialize_and_test_lsp() -> Result<(), ErrorBatch> {
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
    let engine_clone: Engine = postcard::from_bytes(&data).unwrap();
    let hover = engine_clone.hover(2, b"1234567");

    assert_eq!(hover, "i64");

    let result = engine_clone.goto_definition(16, b"let abc = 123\nabc");

    assert_eq!(result, Some(Span {start: 4,end: 7}));

    let result = engine_clone.find_all_references(16, b"let abc = 123\nabc");

    assert_eq!(result, Some(vec![Span {start: 4, end: 7}, Span { start: 14, end: 17}]));

    let result = engine_clone.check_script(b"let abc = \n");

    eprintln!("result: {:?}", result);

    assert_eq!(
        result,
        Some(ErrorBatch::one(ScriptError {
            message: "incomplete math expression".into(),
            span: Span { start: 11, end: 11 },
        }))
    );
    Ok(())
}
