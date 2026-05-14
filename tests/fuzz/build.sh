#!/bin/bash

# This script is used to build the brimstone_fuzz executable for use with fuzzilli

set -e

CURRENT_DIR=$(cd "$(dirname "$0")" && pwd)
BUILD_DIR="$CURRENT_DIR/build"

mkdir -p "$BUILD_DIR"

# Build shared library for sanitizer runtime
clang -c -g -fPIC "$CURRENT_DIR/coverage.c" -o "$BUILD_DIR/coverage.o"
clang -shared -o "$BUILD_DIR/libcoverage.so" "$BUILD_DIR/coverage.o"

# Sanitizer Level 3: All blocks and critical edges (but no indirect calls)
#
# https://github.com/llvm/llvm-project/blob/31127b9e225c50ebadcdc268bccb16319b8db72d/llvm/lib/Transforms/Instrumentation/SanitizerCoverage.cpp#L95-L96

# Pass flags via CARGO_TARGET_<TRIPLE>_RUSTFLAGS (and an explicit --target) so that they only apply
# to the target binary and not to build dependencies like proc macros.
HOST_TRIPLE=$(rustc --print host-tuple)
HOST_TRIPLE_ENV_VAR=$(tr 'a-z-' 'A-Z_' <<< "$HOST_TRIPLE")

env "CARGO_TARGET_${HOST_TRIPLE_ENV_VAR}_RUSTFLAGS=\
  -C link-arg=-L$BUILD_DIR \
  -C link-arg=-lcoverage \
  -C codegen-units=1 \
  -C passes=sancov-module \
  -C llvm-args=-sanitizer-coverage-trace-pc-guard \
  -C llvm-args=-sanitizer-coverage-level=3" \
  cargo build --features build_fuzz -p brimstone_fuzz --target "$HOST_TRIPLE"

# Clone fuzzilli repo if not present
FUZZILLI_DIR="$CURRENT_DIR/fuzzilli"

if [ ! -d "$FUZZILLI_DIR" ]; then
  git clone https://github.com/Hans-Halverson/fuzzilli-brimstone.git "$FUZZILLI_DIR"
fi