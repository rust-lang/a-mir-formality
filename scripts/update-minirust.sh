#!/bin/bash
# Update the vendored minirust-rs crate from the minirust repository.
#
# This script:
# 1. Clones https://github.com/minirust/minirust into a temp directory
# 2. Detects the specr-transpile version from upstream's Cargo.lock
# 3. Installs the matching specr-transpile if needed
# 4. Runs specr-transpile to generate the minirust-rs crate
# 5. Copies the generated sources into crates/minirust-rs/
# 6. Updates libspecr version in crates/formality-rust/Cargo.toml

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
DEST="$REPO_ROOT/crates/minirust-rs"

# Create a temp directory and ensure cleanup
TMPDIR="$(mktemp -d)"
trap 'rm -rf "$TMPDIR"' EXIT

echo "Cloning minirust into $TMPDIR..."
git clone --depth 1 https://github.com/minirust/minirust "$TMPDIR/minirust"

# Detect specr/libspecr version from upstream's Cargo.lock
SPECR_VERSION=$(grep -A 1 'name = "libspecr"' "$TMPDIR/minirust/tooling/Cargo.lock" | grep 'version' | sed 's/.*"\(.*\)".*/\1/')
if [ -z "$SPECR_VERSION" ]; then
    echo "Error: could not detect libspecr version from upstream Cargo.lock"
    exit 1
fi
echo "Detected specr version: $SPECR_VERSION"

# Ensure the matching specr-transpile is installed
INSTALLED_VERSION=$(cargo install --list 2>/dev/null | grep "^specr-transpile " | sed 's/.*v\(.*\):/\1/' || echo "")
if [ "$INSTALLED_VERSION" != "$SPECR_VERSION" ]; then
    echo "Installing specr-transpile@${SPECR_VERSION}..."
    cargo install "specr-transpile@${SPECR_VERSION}"
fi

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

# Sync libspecr version in formality-rust
FORMALITY_CARGO="$REPO_ROOT/crates/formality-rust/Cargo.toml"
if [ -f "$FORMALITY_CARGO" ]; then
    sed -i "s/libspecr = \"=[^\"]*\"/libspecr = \"=${SPECR_VERSION}\"/" "$FORMALITY_CARGO"
    echo "Updated libspecr in formality-rust/Cargo.toml to =${SPECR_VERSION}"
fi

# Sync rust-toolchain.toml (minirust pins a specific nightly for compatibility)
MINIRUST_TOOLCHAIN="$TMPDIR/minirust/rust-toolchain.toml"
if [ -f "$MINIRUST_TOOLCHAIN" ]; then
    echo "Updating rust-toolchain.toml..."
    cp "$MINIRUST_TOOLCHAIN" "$REPO_ROOT/rust-toolchain.toml"
fi

# Record the commit we built from
COMMIT="$(cd "$TMPDIR/minirust" && git rev-parse HEAD)"
echo "Updated minirust-rs from minirust commit $COMMIT"
