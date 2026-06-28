# brimstone_fmt

A complete formatter for the brimstone project. rustfmt doesn't format the tokens inside a
function-like macro invocation like `runtime_fn! { ... }` that doesn't strictly follow Rust syntax.
`brimstone_fmt` does everything rustfmt does *and* formats those blocks.

It runs two passes, each a single rustfmt process:

1. **Whole-file** — the pinned rustfmt over every file, like `cargo fmt` (also
   surfaces any syntax error, with real paths).
2. **Macro blocks** — for each `MACRO_NAMES` invocation (any delimiter), formats
   the top-level `{ }` blocks rustfmt left untouched and splices them back.

Adding a macro is just another entry in `MACRO_NAMES`.

## Usage

From the repo root (`bfmt` / `brimstone-fmt` aliases in `.cargo/config.toml`):

```sh
cargo bfmt                 # format ./src in place
cargo bfmt --check         # report instead of writing (non-zero exit on change/error)
cargo bfmt --install       # install the pinned toolchain + rustfmt, then format
```

It uses a pinned version of rustfmt. Run `--install` once if that toolchain is missing.
