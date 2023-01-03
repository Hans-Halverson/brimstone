# brimstone-test26

Brimstone test runner for the official test262 test suite.

# Installation

The test runner requires the https://github.com/tc39/test262 repo. For consistency the test runner uses the test262 repo pinned to a specific commit. The current commit hash is:

`Dec 2 2022: 3d939ef566456d6a5609bcba48643b14177a77fe`.

Install the test262 repo at the pinned commit by running the following script:

```
./install_test262.sh
```

# Running the tests

The test runner is run with `cargo run` in this directory. Run `cargo run -- --help` to see the full set of options for the test runner.

Before running any tests, the test runner must first create an index of the test262 tests for fast consumption later on. This only needs to be run once:

```
cargo run -- --reindex
```

Then the test262 suite can be run with:

```
# Run all tests
cargo run

# Run only the tests that match a filter string
cargo run -- language/expressions

# Run only the tests that match a feature
cargo run -- --feature computed-property-names
```