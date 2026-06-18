# Benchmarks

Brimstone's first party performance microbenchmark testing is found in this directory.

## Installation

Some tests use fixtures installed from various NPM modules. To install run:

```
./install.sh
```

## Running

To run all benchmarks, run the following from any directory in the workspace:

```
cargo bench -p brimstone
```