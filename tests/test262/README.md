# brimstone-test262

Brimstone test runner for the official test262 test suite.

## Installation

The test runner requires the https://github.com/tc39/test262 repo. For consistency the test runner uses the test262 repo pinned to a specific commit. The current commit hash is:

`2024-08-27: b69e9d5e722291dcb6ada5582c71887133626c63`.

Install the test262 repo at the pinned commit by running the following script:

```
./install_test262.sh
```

## Running the tests

The test runner is run with `cargo run` in this directory. Run `cargo run -- --help` to see the full set of options for the test runner.

Before running any tests, the test runner must first create an index of the test262 tests for fast consumption later on. This only needs to be run after you pull in a new version of the test272 repo.:

```
cargo run -- --reindex
```

Then the test262 suite can be run with:

```
# Run all tests
cargo run

# Run only the tests that match a filter string
cargo run -- language/expressions
```

Other important options include:
- `--report-test262-progress` report the overall progress against the test262 suite, properly categorizing ignored tests
- `--ignore-unimplemented` ignore tests for features that are not yet implemented
- `-t, --threads` specify the number of threads to use for running tests, defaults to 8

## Ignored Tests

The file `ignored_tests.jsonc` contains sets of test which should be ignored by the test runner. These sets of tests are specified by regexp patterns and by feature.

- `known_failures`, which count against test262 progress
- `unimplemented` features, which count against test262 progress
- `non_standard` tests, which do not count against test262 progress
- `slow` tests which are skipped by default
- `gc_stress_test`, which are slow tests that are skipped by default in GC stress test mode
