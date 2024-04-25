# Truffle - Fast Embeddable Scripting for Rust

Truffle is an experimental embeddable scripting language for Rust that offers high performance, ease of embedding, lightweight requirements (no dependencies by default and low compile times), and a familiar syntax (a subset of the Rust syntax).

Truffle offers the following compile-time features:

- **async** - async version of Truffle
- **lsp** - LSP-powered IDE support

## Documentation

You can read more about how to get started with truffle in [truffle/docs/getting_started.md](truffle/docs/getting_started.md).

Additionally, you can learn more about the Truffle language in [truffle/docs/language.md](truffle/docs/language.md).

If you'd like to use the Truffle IDE support (including the Truffle->Rust IDE interop), you can learn about how to set it up in [truffle/docs/lsp.md](truffle/docs/lsp.md).

## Roadmap

Truffle is still in its early stages of development. You'll notice that as a language, Truffle has relatively few features, and it's focused largely on interaction with the Rust application. We're doing this on purpose to help focus which features are added to only what is needed. We're looking for folks who will use it to give us feedback on what they need in practice.

Some possible future features:

- support for arrays (and likely also iteration)
- support for user-defined functions in Truffle scripts

## Performance

Truffle's current hot loop performance is close to Lua's performance (180ms vs 130ms for a hot loop of 10mil iterations, on a macOS 8 × Intel® Core™ i7-8809G CPU @ 3.10GHz machine with 16 gigs of RAM).

We're hoping to continue improving Truffle's performance to make it flexible to be used in more scripting scenarios.

## REPL

Truffle also comes with a rudimentary REPL you can use to interact with a system. You can enhance this REPL by enabling the 'reedline' feature flag.

Note: each REPL line is independent of the other REPL lines. Variables defined in previous REPL lines are not visible in the following lines. You can think of each line as a distinct script being run.

