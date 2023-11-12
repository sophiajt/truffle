# Truffle language

## Basic types

The Truffle scripting language includes a few types to help you get started:

- i64 (eg, `123`)
- f64 (eg, `5.1`)
- bool (eg, `false`)
- strings (eg, `"hello world"`)

## User-defined types

When Rust functions are registered, the Truffle engine also learns of user-defined types if those functions make use of them as parameter or return types. Currently, Truffle doesn't support creating user-defined types in Truffle itself.

See "Method calls" below for an example of working with a user-defined data type.

## Variables

Following Rust, Truffle allows for creating immutable and mutable variables, like so:

```rust
let x = 123     // immutable variable
let mut y = 456 // mutable variable
```

## Expressions

Truffle also supports expression forms like math over numbers. For example:

```rust
1 + 2 * 8
```

## Function calls

Calls in Truffle work with the same syntax as many C-family languages, and identical to that of Rust:

```rust
foo(1, 2)
```

Functions are defined in the Rust code and registered to the Truffle engine. Once registered, the script is able to call them.

## Async function calls

Async function calls look the same as function calls in Truffle. The current version of Truffle elides the requirement to have `.await`. Instead, it runs the async function to completion for you, handing you the result after it has completed.

```rust
async_foo(1, 2)   // runs to completions without the need for `.await`
```

## Method calls

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

## Loops

Truffle supports using `while` to create loops:

```rust
let mut x = 0
while x < 10000000 {
  x = x + 1
}
```

## Typechecking

Truffle scripts are typechecked before they're run. This allows for a few things:

- Early error detection of type errors that would result in runtime errors
- Overloading of function resolution, allowing for more flexible scripts
- Much faster code evaluation, as type-specific code can be generated where possible
