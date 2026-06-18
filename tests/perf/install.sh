#!/bin/bash

# Installs the 3p performance suites into ./vendor.

set -e

CURRENT_DIR=$(cd "$(dirname "$0")" && pwd)
VENDOR_DIR="$CURRENT_DIR/vendor"

# Pinned commits for each 3p suite
OCTANE_COMMIT=570ad1ccfe86e3eecba0636c8f932ac08edec517
JETSTREAM_COMMIT=b7babdf323e64e69bd2f6c376189c15825f5c73a
WEB_TOOLING_COMMIT=4a12828c6a1eed02a70c011bd080445dd319a05f

OCTANE_DIR="$VENDOR_DIR/octane"
JETSTREAM_DIR="$VENDOR_DIR/jetstream"
WEB_TOOLING_BENCHMARK_DIR="$VENDOR_DIR/web-tooling-benchmark"

mkdir -p "$VENDOR_DIR"

# Shallow-fetch a single pinned commit: clone_pinned <dir> <url> <commit>.
clone_pinned() {
  rm -rf "$1" &&
  git init -q "$1" &&
  git -C "$1" remote add origin "$2" &&
  git -C "$1" fetch -q --depth 1 origin "$3" &&
  git -C "$1" checkout -q FETCH_HEAD
}

if [ ! -d "$OCTANE_DIR/.git" ]; then
  echo "==> Installing Octane"
  clone_pinned "$OCTANE_DIR" https://github.com/chromium/octane "$OCTANE_COMMIT"
else
  echo "==> Octane already installed"
fi

if [ ! -d "$JETSTREAM_DIR/.git" ]; then
  echo "==> Installing JetStream"
  clone_pinned "$JETSTREAM_DIR" https://github.com/WebKit/JetStream "$JETSTREAM_COMMIT"
else
  echo "==> JetStream already installed"
fi

WEB_TOOLING_BENCHMARKS="acorn babel babel-minify babylon buble chai coffeescript espree \
  esprima jshint lebab postcss prepack prettier source-map terser typescript uglify-js"

if [ ! -f "$WEB_TOOLING_BENCHMARK_DIR/dist/cli-acorn.js" ]; then
  echo "==> Installing Web Tooling Benchmark"
  if [ ! -d "$WEB_TOOLING_BENCHMARK_DIR/.git" ]; then
    clone_pinned "$WEB_TOOLING_BENCHMARK_DIR" https://github.com/v8/web-tooling-benchmark "$WEB_TOOLING_COMMIT"
  fi
  (
    cd "$WEB_TOOLING_BENCHMARK_DIR"
    [ -d node_modules ] || npm ci

    # Build separate bundles for each individual benchmark so they can be run independently.
    for bench in $WEB_TOOLING_BENCHMARKS; do
      echo "    building standalone bundle for $bench"
      npm run build -- --env.only "$bench"
      mv dist/cli.js "dist/cli-$bench.js"
    done
  )
else
  echo "==> Web Tooling Benchmark already installed"
fi

echo "Done. Vendored suites are in $VENDOR_DIR"
