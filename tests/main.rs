#[cfg(test)]
mod tests {
    use truffle::{
        register_fn, Evaluator, FnRegister, FunctionId, Parser, Translater, TypeChecker, TypeId,
    };
    fn eval_source_into_float(source: &str) -> f64 {
        f64::from_bits(eval_source_with_type(source).0 as u64)
    }

    fn eval_source(source: &str) -> i64 {
        eval_source_with_type(source).0
    }

    pub fn add<T: std::ops::Add>(lhs: T, rhs: T) -> T::Output {
        lhs + rhs
    }

    #[cfg(feature = "async")]
    fn eval_source_with_type(source: &str) -> (i64, TypeId) {
        use futures::executor::block_on;

        let mut parser = Parser::new(source.as_bytes(), 0, 0);
        parser.parse();

        let mut typechecker = TypeChecker::new();
        register_fn!(typechecker, "add", add::<i64>);
        register_fn!(typechecker, "add", add::<f64>);

        typechecker.typecheck(&parser.delta);

        let mut translater = Translater::new();

        #[allow(unused_mut)]
        let mut output = translater.translate(&parser.delta, &typechecker);

        let mut evaluator = Evaluator::default();
        evaluator.add_function(output);

        block_on(evaluator.eval(FunctionId(0), &typechecker.functions))
    }

    #[cfg(not(feature = "async"))]
    fn eval_source_with_type(source: &str) -> (i64, TypeId) {
        let mut parser = Parser::new(source.as_bytes(), 0, 0);
        parser.parse();

        let mut typechecker = TypeChecker::new();
        register_fn!(typechecker, "add", add::<i64>);
        register_fn!(typechecker, "add", add::<f64>);

        typechecker.typecheck(&parser.delta);

        let mut translater = Translater::new();

        #[allow(unused_mut)]
        let mut output = translater.translate(&parser.delta, &typechecker);

        let mut evaluator = Evaluator::default();
        evaluator.add_function(output);

        evaluator.eval(FunctionId(0), &typechecker.functions)
    }

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
}
