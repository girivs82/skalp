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

echo -e "\n=== Running tests ==="
cargo test --all-features --workspace

# Optional: Run tarpaulin coverage check (requires cargo-tarpaulin and z3)
# Note: This will fail if you don't have z3 installed (required for skalp-verify formal feature)
# To skip this check, don't pass --with-coverage flag
if [[ "$1" == "--with-coverage" ]]; then
    echo -e "\n=== Running tarpaulin coverage check ==="
    if ! command -v cargo-tarpaulin &> /dev/null; then
        echo "⚠️  cargo-tarpaulin not found. Install with: cargo install cargo-tarpaulin"
        exit 1
    fi
    cargo tarpaulin --verbose --all-features --workspace --timeout 120 --out xml
fi

echo -e "\n✅ All CI checks passed locally!"
