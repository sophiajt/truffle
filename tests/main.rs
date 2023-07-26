mod test_eval;
use test_eval::*;
use truffle::ReturnValue;

#[test]
fn math() {
    assert_eq!(eval_source("1 + 2"), ReturnValue::I64(3));
    assert_eq!(eval_source("3 - 2"), ReturnValue::I64(1));
    assert_eq!(eval_source("3 * 2"), ReturnValue::I64(6));
    assert_eq!(eval_source("8 / 2"), ReturnValue::I64(4));
    assert_eq!(eval_source("1 + 3 * 2"), ReturnValue::I64(7));
    assert_eq!(eval_source("2 < 3"), ReturnValue::Bool(true));
    assert_eq!(eval_source("2 <= 2"), ReturnValue::Bool(true));
    assert_eq!(eval_source("2 > 3"), ReturnValue::Bool(false));
    assert_eq!(eval_source("2 >= 2"), ReturnValue::Bool(true));
}

#[test]
fn boolean_operations() {
    // Also test shortcircuiting

    assert_eq!(eval_source("false && false"), ReturnValue::Bool(false));
    assert_eq!(eval_source("true && false"), ReturnValue::Bool(false));
    assert_eq!(eval_source("false && true"), ReturnValue::Bool(false));
    assert_eq!(eval_source("true && true"), ReturnValue::Bool(true));
    assert_eq!(
        eval_source("false && ((1 / 0) > 3)"),
        ReturnValue::Bool(false)
    );

    assert_eq!(eval_source("false || false"), ReturnValue::Bool(false));
    assert_eq!(eval_source("true || false"), ReturnValue::Bool(true));
    assert_eq!(eval_source("false || true"), ReturnValue::Bool(true));
    assert_eq!(eval_source("true || true"), ReturnValue::Bool(true));
    assert_eq!(
        eval_source("true || ((1 / 0) > 3)"),
        ReturnValue::Bool(true)
    );
}

#[test]
fn float_math() {
    assert_eq!(eval_source("1.2 + 2.3"), ReturnValue::F64(3.5));
    assert_eq!(eval_source("1.3 - 0.3"), ReturnValue::F64(1.0));
    assert_eq!(eval_source("1.2 * 2.3"), ReturnValue::F64(2.76));
    assert_eq!(eval_source("6.0 / 3.0"), ReturnValue::F64(2.0));
    assert_eq!(eval_source("1.2 < 2.3"), ReturnValue::Bool(true));
    assert_eq!(eval_source("1.2 <= 2.3"), ReturnValue::Bool(true));
    assert_eq!(eval_source("1.2 > 2.3"), ReturnValue::Bool(false));
    assert_eq!(eval_source("1.2 >= 2.3"), ReturnValue::Bool(false));
}

#[test]
fn variables() {
    assert_eq!(eval_source("let x = 1; x + 10"), ReturnValue::I64(11));
    assert_eq!(
        eval_source("let mut x = 1; x = 10; x"),
        ReturnValue::I64(10)
    );
}

#[test]
fn if_expression() {
    assert_eq!(eval_source("if true { 3 } else { 4 }"), ReturnValue::I64(3));
    assert_eq!(
        eval_source("if false { 3 } else { 4 }"),
        ReturnValue::I64(4)
    );
    assert_eq!(
        eval_source("let x = if false { 3 } else { 4 }; x"),
        ReturnValue::I64(4)
    );
}

#[test]
fn while_loop() {
    assert_eq!(
        eval_source("let mut x = 0; while x < 10 { x = x + 1 }; x"),
        ReturnValue::I64(10)
    );
}

#[test]
fn external_call() {
    assert_eq!(eval_source("add(3, 4)"), ReturnValue::I64(7));
    assert_eq!(eval_source("add(6.0, 3.0)"), ReturnValue::F64(9.0));
}

#[test]
fn typecheck_errors() {
    assert!(error_contains(
        &compile_to_error("let x = 123; x = 4566"),
        "assignment to immutable variable"
    ))
}

#[test]
fn runtime_errors() {
    assert!(error_contains(&run_to_error("3 / 0"), "division by zero"))
}
