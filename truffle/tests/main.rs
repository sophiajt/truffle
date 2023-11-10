#![allow(illegal_floating_point_literal_pattern)]
mod test_eval;

use assert_matches::assert_matches;
use test_eval::*;
use truffle::ReturnValue;
#[cfg(feature = "lsp")]
use truffle::{Engine, ErrorBatch, ScriptError, Span};

#[test]
fn math() {
    assert_matches!(eval_source("1 + 2"), Ok(ReturnValue::I64(3)));
    assert_matches!(eval_source("3 - 2"), Ok(ReturnValue::I64(1)));
    assert_matches!(eval_source("3 * 2"), Ok(ReturnValue::I64(6)));
    assert_matches!(eval_source("8 / 2"), Ok(ReturnValue::I64(4)));
    assert_matches!(eval_source("1 + 3 * 2"), Ok(ReturnValue::I64(7)));
    assert_matches!(eval_source("2 < 3"), Ok(ReturnValue::Bool(true)));
    assert_matches!(eval_source("2 <= 2"), Ok(ReturnValue::Bool(true)));
    assert_matches!(eval_source("2 > 3"), Ok(ReturnValue::Bool(false)));
    assert_matches!(eval_source("2 >= 2"), Ok(ReturnValue::Bool(true)));
}

#[test]
fn boolean_operations() {
    // Also test shortcircuiting

    assert_matches!(eval_source("false && false"), Ok(ReturnValue::Bool(false)));
    assert_matches!(eval_source("true && false"), Ok(ReturnValue::Bool(false)));
    assert_matches!(eval_source("false && true"), Ok(ReturnValue::Bool(false)));
    assert_matches!(eval_source("true && true"), Ok(ReturnValue::Bool(true)));
    assert_matches!(
        eval_source("false && ((1 / 0) > 3)"),
        Ok(ReturnValue::Bool(false))
    );

    assert_matches!(eval_source("false || false"), Ok(ReturnValue::Bool(false)));
    assert_matches!(eval_source("true || false"), Ok(ReturnValue::Bool(true)));
    assert_matches!(eval_source("false || true"), Ok(ReturnValue::Bool(true)));
    assert_matches!(eval_source("true || true"), Ok(ReturnValue::Bool(true)));
    assert_matches!(
        eval_source("true || ((1 / 0) > 3)"),
        Ok(ReturnValue::Bool(true))
    );
}

#[test]
fn float_math() {
    assert_matches!(eval_source("1.2 + 2.3"), Ok(ReturnValue::F64(3.5)));
    assert_matches!(eval_source("1.3 - 0.3"), Ok(ReturnValue::F64(1.0)));
    assert_matches!(eval_source("1.2 * 2.3"), Ok(ReturnValue::F64(2.76)));
    assert_matches!(eval_source("6.0 / 3.0"), Ok(ReturnValue::F64(2.0)));
    assert_matches!(eval_source("1.2 < 2.3"), Ok(ReturnValue::Bool(true)));
    assert_matches!(eval_source("1.2 <= 2.3"), Ok(ReturnValue::Bool(true)));
    assert_matches!(eval_source("1.2 > 2.3"), Ok(ReturnValue::Bool(false)));
    assert_matches!(eval_source("1.2 >= 2.3"), Ok(ReturnValue::Bool(false)));
}

#[test]
fn variables() {
    assert_matches!(eval_source("let x = 1; x + 10"), Ok(ReturnValue::I64(11)));
    assert_matches!(
        eval_source("let mut x = 1; x = 10; x"),
        Ok(ReturnValue::I64(10))
    );
}

#[test]
fn if_expression() {
    assert_matches!(
        eval_source("if true { 3 } else { 4 }"),
        Ok(ReturnValue::I64(3))
    );
    assert_matches!(
        eval_source("if false { 3 } else { 4 }"),
        Ok(ReturnValue::I64(4))
    );
    assert_matches!(
        eval_source("let x = if false { 3 } else { 4 }; x"),
        Ok(ReturnValue::I64(4))
    );
}

#[test]
fn while_loop() {
    assert_matches!(
        eval_source("let mut x = 0; while x < 10 { x = x + 1 }; x"),
        Ok(ReturnValue::I64(10))
    );
}

#[test]
fn external_call() {
    assert_matches!(eval_source("add(3, 4)"), Ok(ReturnValue::I64(7)));
    assert_matches!(eval_source("add(6.0, 3.0)"), Ok(ReturnValue::F64(9.0)));
}

#[test]
fn method_and_mutation() {
    assert_matches!(
        eval_source(r#"let env = new_env(); env.set_var("kris", 3); env.read_var("kris")"#),
        Ok(ReturnValue::I64(3))
    );
}

#[test]
fn typecheck_errors() {
    eval_source("let x = 123; x = 4566")
        .expect_err("test should fail due to missing mut")
        .assert_contains("assignment to immutable variable");
}

#[test]
fn runtime_errors() {
    eval_source("3 / 0")
        .expect_err("it should not be possible to divide by zero")
        .assert_contains("division by zero");
}

#[test]
#[cfg(feature = "lsp")]
fn lsp_hover() {
    let engine = Engine::new("test_hover");
    let hover = engine.hover(2, b"1234567");

    assert_eq!(hover, "i64")
}

#[test]
#[cfg(feature = "lsp")]
fn lsp_goto_definition() {
    let engine = Engine::new("test_gotodef");
    let result = engine.goto_definition(16, b"let abc = 123\nabc");

    assert_eq!(result, Some(Span { start: 4, end: 7 }))
}

#[test]
#[cfg(feature = "lsp")]
fn lsp_find_all_references() {
    let engine = Engine::new("test_find_refs");
    let result = engine.find_all_references(16, b"let abc = 123\nabc");

    assert_eq!(
        result,
        Some(vec![Span { start: 4, end: 7 }, Span { start: 14, end: 17 }])
    )
}

#[test]
#[cfg(feature = "lsp")]
fn lsp_check_script() {
    let engine = Engine::new("test_check_script");
    let result = engine.check_script(b"let abc = \n");

    eprintln!("result: {:?}", result);

    assert_eq!(
        result,
        Some(ErrorBatch::one(ScriptError {
            message: "incomplete math expression".into(),
            span: Span { start: 11, end: 11 }
        }))
    )
}
