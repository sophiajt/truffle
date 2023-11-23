# Truffle - Getting Started

Truffle is an easy-to-use, embedded scripting language that's built for working with Rust. The current version of Truffle allows you to:

- Call Rust functions
- Call async Rust functions
- Work with user-defined Rust types in the script, including mutation
- Do common script activities, like loops, function calls, working with variables, and more

Truffle should also be very familiar to programmers who have used Rust before, as Truffle's syntax is a subset of Rust.

## Registering normal Rust functions

Starting up the Truffle engine, registering a Rust function, and running the engine are each just a single line of Rust code.

Let's look at an example:

```rust
    let mut engine = Engine::new();

    register_fn!(engine, "print", print);

    let result = engine.eval_source(fname, contents, debug_output);
```

Truffle also supports registering monomorphized generic Rust functions:

```rust
    register_fn!(engine, "print", print::<i64>);
```

You can even overload the function, allowing Truffle to call the correct function based on the type of the argument:

```rust
    register_fn!(engine, "print", print::<i64>);
    register_fn!(engine, "print", print::<f64>);
    register_fn!(engine, "print", print::<bool>);
```

With that, we now can call `print` in the Truffle script one of three ways:

```rust
print(10)
print(5.2)
print(false)
```

## Registering async Rust functions

Truffle also supports registering and calling async Rust functions. For async support, build Truffle with the `async` feature.

NOTE: in the current version of Truffle, the `.await` is implicit in the script. Calls to async functions are immediately awaited for their value.

```rust
    use futures::executor::block_on;

    #[truffle::export]
    async fn add_one_hundred(this: i64) -> i64 {
        this + 100
    }

    let mut engine = Engine::new();

    register_fn!(engine, "modify_this", modify_this);

    block_on(engine.eval_source_async(fname, contents, debug_output))
```

Async functions are registered similarly to normal Rust functions, using the `register_fn!` macro. Additionally, async functions need to use the `#[truffle::export]` proc macro to make them visible to the Truffle scripting engine.

With that, we can now eval the source. Here, we use `eval_source_async` to allow the scripting engine to run asynchronously, letting us use `block_on` to run the script to completion.
