#!/bin/bash

set -e

CURRENT_DIR=$(cd "$(dirname "$0")" && pwd)
TEST262_REPO_DIR="$CURRENT_DIR/test262"

# Pinned commit hash of test262 repo that we test off of. Be sure to also update README.md.
# Last updated on 2025-02-07
TEST262_COMMIT_SHA="bc5c14176e2b11a78859571eb693f028c8822458"

# Clone the test262 repo if it is not already present
if [ ! -d "$TEST262_REPO_DIR" ]; then
  git clone https://github.com/tc39/test262.git "$TEST262_REPO_DIR"
fi

# Checkout the pinned commit
cd "$TEST262_REPO_DIR"
git fetch
git checkout "$TEST262_COMMIT_SHA"