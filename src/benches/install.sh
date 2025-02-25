#!/bin/bash

set -e

CURRENT_DIR=$(cd "$(dirname "$0")" && pwd)
FIXTURES_DIR="$CURRENT_DIR/fixtures"

rm -rf "$FIXTURES_DIR"
mkdir -p "$FIXTURES_DIR"

# Install the necessary dependencies and extract fixtures
npm install typescript@5.7.3
cp "$CURRENT_DIR/node_modules/typescript/lib/typescript.js" "$FIXTURES_DIR/typescript.js"

npm install acorn@8.14.0
cp "$CURRENT_DIR/node_modules/acorn/dist/acorn.js" "$FIXTURES_DIR/acorn.js"

npm install react@19.0.0
cp "$CURRENT_DIR/node_modules/react/cjs/react.production.js" "$FIXTURES_DIR/react.js"

# Clean up all generated files outside fixtures directory
rm -rf "$CURRENT_DIR/node_modules"
rm -f "$CURRENT_DIR/package-lock.json"
rm -f "$CURRENT_DIR/package.json"