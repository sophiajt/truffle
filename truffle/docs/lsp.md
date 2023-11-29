# LSP support

Truffle supports LSP for working with Truffle source files and Truffle->Rust go-to-definition. This allows you to easily work on Truffle scripts as a normal part of your IDE workflow.

To get LSP support working:

* Go to the `truffle-lsp` directory and run `cargo install --path .`. This will install the Truffle LSP server.
* For VSCode support, go to the `editors/vscode` directory and run `npm install` followed by `vsce package`. Make sure you have a recent `vsce` installed by doing `npm install -g vsce`. Once packaged, you can install this package from inside VSCode. Restart VSCode, and Truffle scripts should now have highlighting and initial LSP support.

For your project to include Truffle->Rust LSP interop, you'll need to do the following:

* Point your Cargo.toml in your project to Truffle and be sure to include the `lsp` feature, eg: `truffle = { path = "../../truffle", features = ["lsp"] }`
* Then, you'll need to register each function in your application:

First, make sure each function has an `#[export]` over top of it:
```rust
use truffle::{export, register_fn, Engine, FnRegister};

#[export]
fn print<T: Display>(x: T) {
    println!("{}", x)
}
```

Next, create and name your engine:

```rust
let mut engine = Engine::new();
engine.set_app_name("cli");
```

Then, register each function using the `register_fn` macro:

```rust
register_fn!(engine, "print", print::<i64>);
```

Finally, make sure each script carries your project name as part of the extension. For example:

```rust
// filename: use_print.cli.truffle

let xyz = 123

xyz

print(100)
```

* Once you've updated the source to register the functions, build your project with `cargo build`
* Run your project. Once run, the macros you use to register functions will write to a cache that the Truffle LSP support can see

With these steps, the Truffle LSP support will be able to see not only the Truffle code, but also the Rust code that works with it.
