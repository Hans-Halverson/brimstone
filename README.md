# Brimstone

Brimstone is a JavaScript engine written from scratch in Rust, aiming to have full support for the JavaScript language.

Brimstone is a work in progress but already supports most of the JavaScript language (>94% of the ECMAScript language in test262). Not ready for use in production.

Implements the [ECMAScript specification](https://tc39.es/ecma262/). Heavy inspiration is taken from the design of [V8](https://v8.dev/) and SerenityOS's [LibJS](https://github.com/LadybirdBrowser/ladybird). Brimstone chooses to implement almost all components of the engine from scratch with minimal dependencies, with the notable exception of [ICU4X](https://github.com/unicode-org/icu4x).

Brimstone features:

- Bytecode VM, heavily inspired by the design of V8's Ignition
- Compacting garbage collector, written in *very* unsafe Rust
- Custom RegExp engine
- Custom parser
- Almost all builtin objects and functions implemented to spec

## Building and testing

Standard `cargo` commands to build and run.
- `cargo build` to build the `bs` executable
- `cargo run` to run from source

JavaScript files can be executed with `bs`:

```
# Build brimstone
cargo build

# Execute a JavaScript file
./target/debug/bs ./hello.js
Hello world!
```

## Testing

Brimstone relies heavily on [test262](https://github.com/tc39/test262). A [custom runner](./tests/test262/README.md) for test262 is included. This can be run with:

```
cd tests/test262
cargo run
```

Unit tests can be run with `cargo test`.

For more information on testing see the [testing README](./tests/README.md).

## Missing features

All features up to ES2024 have been implemented (as well as all stage 4 proposals as of 2024-09-15), except for the following features:

- UnicodeSets proposal
- SharedArrayBuffer
- Atomics
