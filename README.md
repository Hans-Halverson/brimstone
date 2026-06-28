# Brimstone

Brimstone is a JavaScript engine written from scratch in Rust with full support for the JavaScript language.

Brimstone is a work in progress but already supports almost all of the JavaScript language (>97% of the ECMAScript language in test262). Not ready for use in production.

Implements the [ECMAScript specification](https://tc39.es/ecma262/). Heavy inspiration is taken from the design of [V8](https://v8.dev/) and SerenityOS's [LibJS](https://github.com/LadybirdBrowser/ladybird). Brimstone chooses to implement almost all components of the engine from scratch with minimal dependencies, with the notable exceptions of [ICU4X](https://github.com/unicode-org/icu4x) and [temporal_rs](https://github.com/boa-dev/temporal).

Brimstone features:

- Complete JavaScript language and standard library, other than SharedArrayBuffer and Atomics
- Up to date with the latest features (ES2026) and includes Temporal
- Bytecode VM, heavily inspired by the design of V8's Ignition
- Compacting garbage collector, written in *very* unsafe Rust
- Custom RegExp engine
- Custom parser

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

Brimstone relies heavily on a set of first and third party integration test suites, most notably the official [test262](https://github.com/tc39/test262) test suite. A custom [integration test runner](./tests/README.md) is included. This can be run with:

```
cargo btest
```

Unit and snapshot tests can be run with `cargo test`.

Fuzz testing can be run with `./tests/fuzz/run.sh`.

For more information on testing see the [testing README](./tests/README.md).

## Development

Brimstone includes aliases for `cargo` commands in `.cargo/config.toml`:
- `cargo bfmt` - runs the [custom brimstone formatter](./tools/brimstone_fmt/README.md)
- `cargo btest` - runs the custom integration test runner

## Missing features

All features up to ES2026 have been implemented except for the following:

- SharedArrayBuffer
- Atomics
