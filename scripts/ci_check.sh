#!/bin/bash
# Local CI validation script
# Run this before pushing to catch CI failures early

set -e

echo "=== Running formatting check ==="
cargo fmt --all -- --check

echo -e "\n=== Running clippy on stable ==="
cargo +stable clippy --all-targets --all-features -- -D warnings

echo -e "\n=== Running clippy on beta ==="
cargo +beta clippy --all-targets --all-features -- -D warnings

echo -e "\n=== Running build ==="
cargo build --verbose --all-features

echo -e "\n✅ All CI checks passed locally!"
