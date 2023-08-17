#!/bin/bash

# This script is used to generate the data files for ICU4X

set -e

CURRENT_DIR=$(cd "$(dirname "$0")" && pwd)
DATA_DIR="$CURRENT_DIR/data"

# This version must match the version of icu in Cargo.toml
ICU_VERSION="1.2.0"

cargo install --version "$ICU_VERSION" icu_datagen

icu4x-datagen \
  --key-file "$CURRENT_DIR/keys.txt" \
  --locales en \
  --format mod \
  --out "$DATA_DIR" \
  --overwrite \
  --pretty \
  --use-separate-crates