#!/bin/bash

# This script is used to generate the data files for ICU4X

set -e

CURRENT_DIR=$(cd "$(dirname "$0")" && pwd)
DATA_DIR="$CURRENT_DIR/data"

# This version must match the version of icu in Cargo.toml
ICU_VERSION="2.1.1"

cargo install --version "$ICU_VERSION" icu4x-datagen

icu4x-datagen \
  --markers $(cat "$CURRENT_DIR/markers.txt") \
  --locales en \
  --format baked \
  --out "$DATA_DIR" \
  --overwrite \
  --pretty \
  --use-separate-crates