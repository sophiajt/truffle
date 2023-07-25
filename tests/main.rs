mod test_eval;
use test_eval::*;

#[test]
fn math() {
    assert_eq!(eval_source("1 + 2"), 3);
    assert_eq!(eval_source("3 - 2"), 1);
    assert_eq!(eval_source("3 * 2"), 6);
    assert_eq!(eval_source("8 / 2"), 4);
    assert_eq!(eval_source("1 + 3 * 2"), 7);
    assert_eq!(eval_source("2 < 3"), 1);
    assert_eq!(eval_source("2 <= 2"), 1);
    assert_eq!(eval_source("2 > 3"), 0);
    assert_eq!(eval_source("2 >= 2"), 1);
}

#[test]
fn boolean_operations() {
    // Also test shortcircuiting

    assert_eq!(eval_source("false && false"), 0);
    assert_eq!(eval_source("true && false"), 0);
    assert_eq!(eval_source("false && true"), 0);
    assert_eq!(eval_source("true && true"), 1);
    assert_eq!(eval_source("false && ((1 / 0) > 3)"), 0);

    assert_eq!(eval_source("false || false"), 0);
    assert_eq!(eval_source("true || false"), 1);
    assert_eq!(eval_source("false || true"), 1);
    assert_eq!(eval_source("true || true"), 1);
    assert_eq!(eval_source("true || ((1 / 0) > 3)"), 1);
}

#[cfg(test)]
pub fn eval_source_into_float(source: &str) -> f64 {
    f64::from_bits(eval_source_with_type(source).0 as u64)
}

#[test]
fn float_math() {
    assert_eq!(eval_source_into_float("1.2 + 2.3"), 3.5);
    assert_eq!(eval_source_into_float("1.3 - 0.3"), 1.0);
    assert_eq!(eval_source_into_float("1.2 * 2.3"), 2.76);
    assert_eq!(eval_source_into_float("6.0 / 3.0"), 2.0);
    assert_eq!(eval_source("1.2 < 2.3"), 1);
    assert_eq!(eval_source("1.2 <= 2.3"), 1);
    assert_eq!(eval_source("1.2 > 2.3"), 0);
    assert_eq!(eval_source("1.2 >= 2.3"), 0);
}

#[test]
fn variables() {
    assert_eq!(eval_source("let x = 1; x + 10"), 11);
    assert_eq!(eval_source("let mut x = 1; x = 10; x"), 10);
}

#[test]
fn if_expression() {
    assert_eq!(eval_source("if true { 3 } else { 4 }"), 3);
    assert_eq!(eval_source("if false { 3 } else { 4 }"), 4);
    assert_eq!(eval_source("let x = if false { 3 } else { 4 }; x"), 4);
}

#[test]
fn while_loop() {
    assert_eq!(
        eval_source("let mut x = 0; while x < 10 { x = x + 1 }; x"),
        10
    );
}

#[test]
fn external_call() {
    assert_eq!(eval_source("add(3, 4)"), 7);
    assert_eq!(eval_source_into_float("add(6.0, 3.0)"), 9.0);
}

#[test]
fn typecheck_errors() {
    assert!(error_contains(
        &compile_to_error("let x = 123; x = 4566"),
        "assignment to immutable variable"
    ))
}
