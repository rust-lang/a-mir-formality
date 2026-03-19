#!/bin/bash
# Update the vendored minirust-rs crate from the minirust repository.
#
# This script:
# 1. Clones https://github.com/minirust/minirust into a temp directory
# 2. Runs specr-transpile to generate the minirust-rs crate
# 3. Copies the generated sources into crates/minirust-rs/
#
# Prerequisites: cargo install specr-transpile@0.1.36
#   (the script will attempt to install it if missing)

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
DEST="$REPO_ROOT/crates/minirust-rs"

SPECR_VERSION="0.1.36"

# Ensure specr-transpile is installed
if ! command -v specr-transpile &>/dev/null; then
    echo "Installing specr-transpile@${SPECR_VERSION}..."
    cargo install "specr-transpile@${SPECR_VERSION}"
fi

# Create a temp directory and ensure cleanup
TMPDIR="$(mktemp -d)"
trap 'rm -rf "$TMPDIR"' EXIT

echo "Cloning minirust into $TMPDIR..."
git clone --depth 1 https://github.com/minirust/minirust "$TMPDIR/minirust"

echo "Running specr-transpile..."
cd "$TMPDIR/minirust"
specr-transpile specr.toml

GENERATED="$TMPDIR/minirust/tooling/minirust-rs"

if [ ! -d "$GENERATED/src" ]; then
    echo "Error: specr-transpile did not produce tooling/minirust-rs/src"
    exit 1
fi

# Replace source files and Cargo.toml, preserve target/ for caching
echo "Updating $DEST..."
rm -rf "$DEST/src"
cp -r "$GENERATED/src" "$DEST/src"
cp "$GENERATED/Cargo.toml" "$DEST/Cargo.toml"

# Sync rust-toolchain.toml (minirust pins a specific nightly for compatibility)
MINIRUST_TOOLCHAIN="$TMPDIR/minirust/rust-toolchain.toml"
if [ -f "$MINIRUST_TOOLCHAIN" ]; then
    echo "Updating rust-toolchain.toml..."
    cp "$MINIRUST_TOOLCHAIN" "$REPO_ROOT/rust-toolchain.toml"
fi

# Record the commit we built from
COMMIT="$(cd "$TMPDIR/minirust" && git rev-parse HEAD)"
echo "Updated minirust-rs from minirust commit $COMMIT"
