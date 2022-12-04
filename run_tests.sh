#!/bin/bash

set -e

CURRENT_DIR=$(cd "$(dirname "$0")" && pwd)

"$CURRENT_DIR/tests/test262-runner/src/run.js" --host-path="$CURRENT_DIR/target/debug/brimstone" "$@"