#!/bin/bash

set -e

CURRENT_DIR=$(cd "$(dirname "$0")" && pwd)
TEST262_REPO_DIR="$CURRENT_DIR/test262"

# Pinned commit hash of test262 repo that we test off of
# Last updated on 2024-08-27
TEST262_COMMIT_SHA="b69e9d5e722291dcb6ada5582c71887133626c63"

# Clone the test262 repo if it is not already present
if [ ! -d "$TEST262_REPO_DIR" ]; then
  git clone https://github.com/tc39/test262.git "$TEST262_REPO_DIR"
fi

# Checkout the pinned commit
cd "$TEST262_REPO_DIR"
git fetch
git checkout "$TEST262_COMMIT_SHA"