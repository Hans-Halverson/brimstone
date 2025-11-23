# Tests

Brimstone uses a variety of testing strategies all located in this directory.

# Snapshot Tests

A simple, generic snapshot test runner is found in the `snapshot` directory. This runner is used to run the following snapshot test suites:

All snapshot tests are run with `cargo test`.

- `js_bytecode` contains snapshot tests for bytecode generation.
- `js_error` contains snapshot tests for error messages.
- `js_parser` contains snapshot tests for the parser.
- `js_regexp_bytecode` contains snapshot tests for regexp bytecode generation.

# Integration Tests

Brimstone has a generic integration test runner which can be used to run multiple suites of tests. This includes both first and third party tests suites, such as the official test262 test suite.

The integration test runner is located in the `harness` directory.

## Test Suites

- `integration` contains first party integration tests.
- `test262` contains the third party official test262 tests.

## Installation

The test runner requires the https://github.com/tc39/test262 repo. For consistency the test runner uses the test262 repo pinned to a specific commit. The current commit is defined in `install_test262.sh`.

Install the test262 repo at the pinned commit by running the following script:

```
./tests/test262/install_test262.sh
```

## Running the tests

The test runner is run with `cargo brimstone-test` anywhere in the `brimstone` workspace. Run `cargo brimstone-test -- --help` to see the full set of options for the test runner.

Before running any tests, the test runner must first create an index of the all test suites for fast consumption later on. This only needs to be run when the set of tests has changed, e.g. after you pull in a new version of the test262 repo.

```
cargo brimstone-test -- --reindex
```

Then the all integration test suites can be run with:

```
# Run all integration test suites
cargo brimstone-test

# Run only the tests that match a filter string
cargo run -- language/expressions
```

Other important options include:
- `-s, --suite` specify the test suite to run, e.g. `integration` or `test262`
- `--ignore-unimplemented` ignore tests for features that are not yet implemented
- `--report-test262-progress` report the overall progress against the test262 suite, properly categorizing ignored tests
- `-t, --threads` specify the number of threads to use for running tests, defaults to 8
- `--save-result-files` saves files listing all successful and failed tests
- `--save-time-file` saves a file with the time taken to run each test

## Ignored Tests

The file `ignored_tests.jsonc` for each test suite contains sets of tests which should be ignored by the test runner. These sets of tests are specified by regexp patterns and by feature.

- `known_failures`, which count against test262 progress
- `unimplemented` features, which count against test262 progress
- `non_standard` tests, which do not count against test262 progress
- `slow` tests which are skipped by default
- `gc_stress_test`, which are slow tests that are skipped by default in GC stress test mode

## Manifest

Integration test suites are defined in the root `test_manifest.jsonc` file. This allows configuring the name, paths, and other options for each test suite. The test runner will automatically discover the `brimestone` repo's test manifest if run within the `brimstone` workspace. The path to the test manifest can be specified with the `--test-manifest-path` option.

# Fuzz Tests

Brimstone supports fuzz testing with [googleprojectzero/fuzzilli](https://github.com/googleprojectzero/fuzzilli). Brimstone support for fuzzilli is added in [this fork](https://github.com/Hans-Halverson/fuzzilli-brimstone). A custom harness for Brimstone is included in the `fuzz` directory to integrate with fuzzilli.

The fuzzer can be built and run with:

```
./tests/fuzz/run.sh
```
