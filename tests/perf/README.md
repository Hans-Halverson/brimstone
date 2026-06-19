# Performance suite

Runs standard third-party JavaScript performance suites against brimstone.

Supports [Octane](https://github.com/chromium/octane), [JetStream](https://github.com/WebKit/JetStream), and [Web Tooling Benchmark](https://github.com/v8/web-tooling-benchmark/).

## Installation

Suites are vendored into `vendor/` with `./install.sh`. Requires `git`, and `npm` for the Web Tooling Benchmark build.

## Run

```
# Build bs in release mode and run Octane:
cargo run -p brimstone_perf -- --suite octane

# Run only specific benchmarks (case-insensitive substring match, repeatable):
cargo run -p brimstone_perf -- --suite octane --bench richards --bench splay

# Structured JSON, written to a file:
cargo run -p brimstone_perf -- --suite octane --format json --out octane.json
```

Useful flags: `--bs-path <path>` (use an existing binary instead of building),
`--vendor-dir <dir>`, `--format pretty|json`, `--out <file>`, `--flamegraph [<file>]`.

## Profiling a run (flamegraph)

Pass `--flamegraph` to profile the `bs` process for a run and write a flamegraph SVG,
using the [`flamegraph`](https://github.com/flamegraph-rs/flamegraph) CLI.

```
# Initial setup: make sure flamegraph is installed
cargo install flamegraph

# Run a single benchmark with profiling and write flamegraph output
cargo run --release -p brimstone_perf -- --suite octane --bench raytrace --flamegraph
```

Notes:

- **Flamegraph arguments.** `--flamegraph-arg=<arg>` forwards any raw `flamegraph` argument
- **Output file.** `--flamegraph=<file>` writes the output to a paricular file. Defaults to
  `flamegraph.svg` if no file name is provided.
- **Build with symbols.** When `--flamegraph` is set and the harness builds `bs` itself, it
  builds release *with debug info* (`CARGO_PROFILE_RELEASE_DEBUG=true`) so frames are named.
  If you supply your own `--bs-path`, build it with debug symbols yourself.

## How it works

The harness runs `bs` as a subprocess and prints results to stdout. `bs` is run with:

```
bs --expose-test-shell-compat <suite files...> <driver> [-- <forwarded cli args>]
```

`--expose-test-shell-compat` installs the shell host functions that benchmark suites
expect. Everything after `--` is exposed as `globalThis.arguments`.
