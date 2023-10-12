# Truffle - Getting Started

Truffle is an easy-to-use, embedded scripting language that's built for working with Rust. The current version of Truffle allows you to:

* Call Rust functions
* Call async Rust functions
* Work with user-defined Rust types in the script, including mutation
* Do common script activities, like loops, function calls, working with variables, and more

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

## Truffle language

### Basic types

The Truffle scripting language includes a few types to help you get started:

* i64 (eg, `123`)
* f64 (eg, `5.1`)
* bool (eg, `false`)
* strings (eg, `"hello world"`)

### User-defined types

When Rust functions are registered, the Truffle engine also learns of user-defined types if those functions make use of them as parameter or return types. Currently, Truffle doesn't support creating user-defined types in Truffle itself.

See "Method calls" below for an example of working with a user-defined data type.

### Variables

Following Rust, Truffle allows for creating immutable and mutable variables, like so:

```rust
let x = 123     // immutable variable
let mut y = 456 // mutable variable
```

### Expressions

Truffle also supports expression forms like math over numbers. For example:

```rust
1 + 2 * 8
```

### Function calls

Calls in Truffle work with the same syntax as many C-family languages, and identical to that of Rust:

```rust
foo(1, 2)
```

Functions are defined in the Rust code and registered to the Truffle engine. Once registered, the script is able to call them.

### Async function calls

Async function calls look the same as function calls in Truffle. The current version of Truffle elides the requirement to have `.await`. Instead, it runs the async function to completion for you, handing you the result after it has completed.

```rust
async_foo(1, 2)   // runs to completions without the need for `.await`
```

### Method calls

Methods in Truffle also follow closely to Rust's syntax:

```rust
env.set_var("abc", 123)
```

Just as functions, methods are registered on the Rust side before the script is called. Here's the Rust registration of the above method call:

```rust
register_fn!(engine, "set_var", Env::set_var);
```

and the definition of Env and `set_var`:
```rust
pub struct Env {
    vars: HashMap<String, i64>,
}

impl Env {
    pub fn set_var(&mut self, var: String, value: i64) {
        self.vars.insert(var, value);
    }

    // ...
}
```

### Loops

Truffle supports using `while` to create loops:

```rust
let mut x = 0
while x < 10000000 {
  x = x + 1
}
```

## Roadmap

Truffle is still in its early stages of development. We're looking for folks who will use it to give us feedback on what they need in practice.

Some possible future featuers:

* support for arrays (and likely also iteration)
* support for user-defined functions in Truffle scripts

## Performance

Truffle's current hot loop performance is close to Lua's performance (180ms vs 130ms for a hot loop of 10mil iterations, on a 8 × Intel® Core™ i7-8809G CPU @ 3.10GHz machine with 16 gigs of RAM).

We're hoping to continue improving Truffle's performance to make it flexible to be used in more scripting scenarios.

## Typechecking

Truffle scripts are typechecked before they're run. This allows for a few things:

* Early error detection of type errors that would result in runtime errors
* Overloading of function resolution, allowing for more flexible scripts
* Much faster code evaluation, as type-specific code can be generated where possible

