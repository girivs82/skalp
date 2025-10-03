# SKALP Testing Guide

This document describes the comprehensive testing infrastructure for SKALP.

## Table of Contents

- [Test Organization](#test-organization)
- [Running Tests](#running-tests)
- [Test Suites](#test-suites)
- [Writing Tests](#writing-tests)
- [Golden File Testing](#golden-file-testing)
- [Continuous Integration](#continuous-integration)
- [Coverage](#coverage)

## Test Organization

SKALP tests are organized into several categories:

```
skalp/
├── crates/
│   ├── skalp-frontend/
│   │   └── tests/
│   │       ├── parser_regression.rs      # Parser regression tests (156 tests)
│   │       ├── hir_builder_regression.rs # HIR builder tests (53 tests)
│   │       ├── integration_tests.rs      # Integration tests
│   │       └── ...
│   ├── skalp-mir/tests/                  # MIR compilation tests
│   ├── skalp-sir/tests/                  # SIR tests
│   ├── skalp-sim/tests/                  # Simulation tests
│   └── skalp-testing/src/
│       └── golden.rs                     # Golden file framework
├── tests/
│   ├── golden_file_tests.rs              # Golden file tests (11 tests)
│   ├── e2e_examples_test.rs              # End-to-end tests (30 tests)
│   └── golden/                           # Stored golden files
└── .github/workflows/                    # CI/CD pipelines
```

## Running Tests

### Run All Tests

```bash
# Run all tests in workspace
cargo test --workspace

# Run with verbose output
cargo test --workspace --verbose

# Run in parallel (default)
cargo test --workspace --test-threads=8
```

### Run Specific Test Suites

```bash
# Parser regression tests (156 tests)
cargo test -p skalp-frontend --test parser_regression

# HIR builder regression tests (53 tests)
cargo test -p skalp-frontend --test hir_builder_regression

# Golden file tests (11 tests)
cargo test --test golden_file_tests

# End-to-end example tests (30 tests)
cargo test --test e2e_examples_test

# All frontend tests
cargo test -p skalp-frontend

# All MIR tests
cargo test -p skalp-mir

# All simulation tests
cargo test -p skalp-sim
```

### Run Specific Tests

```bash
# Run single test by name
cargo test test_binary_add

# Run tests matching pattern
cargo test parser_regression

# Run with output capture disabled
cargo test --nocapture

# Run with backtrace on failure
RUST_BACKTRACE=1 cargo test
```

## Test Suites

### 1. Parser Regression Tests

**Location:** `crates/skalp-frontend/tests/parser_regression.rs`
**Count:** 156 tests
**Purpose:** Ensure parser correctly handles all SKALP syntax

**Coverage:**
- Binary expressions (all operators: +, -, *, /, &, |, ^, <<, >>)
- Unary expressions (!, ~, -)
- Index expressions (single bit: `data[0]`)
- Range expressions (bit slicing: `data[7:0]`)
- If expressions (simple and nested)
- Match expressions (literals, patterns)
- Parenthesized expressions
- Complex nested patterns (tests the fixed parser bug)
- Real-world examples (ALU, overflow detection)
- Edge cases

**Example:**
```rust
#[test]
fn test_binary_add() {
    assert_parses("a + b");
}

#[test]
fn test_regression_complex_nested() {
    // Tests the exact bug that was fixed
    assert_parses("(~a[31] & ~b[31] & c[31]) | (a[31] & b[31] & ~c[31])");
}
```

### 2. HIR Builder Regression Tests

**Location:** `crates/skalp-frontend/tests/hir_builder_regression.rs`
**Count:** 53 tests
**Purpose:** Verify HIR construction from parsed syntax trees

**Coverage:**
- Entity declarations (simple, multiple ports, clock/reset)
- Implementation blocks (signals, variables, constants)
- Event blocks (rising edge, falling edge)
- Expressions (binary, unary, index, range, if, match)
- Type extraction (bit, nat, int, clock, reset)
- Instances and connections
- Pattern matching
- Real-world examples

**Example:**
```rust
#[test]
fn test_entity_simple() {
    let source = r#"
entity Test {
    in a: bit[8]
    out b: bit[8]
}
impl Test {
    b = a
}
"#;
    let hir = assert_builds(source);
    assert_eq!(hir.entities.len(), 1);
}
```

### 3. Golden File Tests

**Location:** `tests/golden_file_tests.rs`
**Count:** 11 tests
**Purpose:** Detect regressions in generated Verilog output

**Covered Examples:**
- ALU
- Counter
- Simple adder
- Mux (2-to-1, 4-to-1)
- Bit operations
- Shift operations
- Comparisons
- Bit indexing
- Nested if
- Complex expressions

**Usage:**
```bash
# Run tests (compares against golden files)
cargo test --test golden_file_tests

# Update golden files after intentional changes
SKALP_UPDATE_GOLDEN=1 cargo test --test golden_file_tests
```

**Example:**
```rust
#[test]
fn test_alu_codegen_golden() {
    let source = include_str!("../examples/alu.sk");
    let mut golden = GoldenTest::new("alu");
    let verilog = compile_to_verilog(source);
    golden.assert_eq("sv", &verilog);
}
```

### 4. End-to-End Tests

**Location:** `tests/e2e_examples_test.rs`
**Count:** 30 tests
**Purpose:** Test complete compilation pipeline on real examples

**Covered:**
- All examples/ files (ALU, Counter, FIFO, SPI, Processor)
- All stdlib components (Adder, Counter, FIFO, Multiplier, Shift Register, UART, AXI4-Lite)
- Inline test cases (mux, register, bit operations, etc.)
- Complex patterns (state machines, overflow detection)

**Example:**
```rust
#[test]
fn test_example_alu() {
    let source = include_str!("../examples/alu.sk");
    let verilog = assert_compiles(source);
    assert_verilog_contains(&verilog, "module ALU");
}
```

## Writing Tests

### Parser Tests

Add to `crates/skalp-frontend/tests/parser_regression.rs`:

```rust
#[test]
fn test_my_new_feature() {
    assert_parses("my new syntax");
}

#[test]
fn test_my_feature_in_context() {
    let source = r#"
entity Test {
    in a: bit[8]
    out b: bit[8]
}
impl Test {
    b = my_new_syntax
}
"#;
    let tree = parse(source);
    assert_eq!(tree.kind(), SyntaxKind::SourceFile);
}
```

### HIR Tests

Add to `crates/skalp-frontend/tests/hir_builder_regression.rs`:

```rust
#[test]
fn test_my_hir_feature() {
    let source = r#"
entity Test {
    in a: bit[8]
    out b: bit[8]
}
impl Test {
    b = my_feature(a)
}
"#;
    let hir = assert_builds(source);
    let impl_block = get_first_impl(&hir);
    // Assert HIR structure
    assert_eq!(impl_block.assignments.len(), 1);
}
```

### Golden File Tests

Add to `tests/golden_file_tests.rs`:

```rust
#[test]
fn test_my_feature_golden() {
    let source = r#"
entity MyFeature {
    in a: bit[8]
    out b: bit[8]
}
impl MyFeature {
    b = my_feature(a)
}
"#;
    let mut golden = GoldenTest::new("my_feature");
    let verilog = compile_to_verilog(source);
    golden.assert_eq("sv", &verilog);
}
```

Run with `SKALP_UPDATE_GOLDEN=1` first time to create golden file.

### End-to-End Tests

Add to `tests/e2e_examples_test.rs`:

```rust
#[test]
fn test_my_example() {
    let source = include_str!("../examples/my_example.sk");
    let verilog = assert_compiles(source);
    assert_verilog_contains(&verilog, "module MyExample");
}
```

## Golden File Testing

Golden file testing compares actual output against stored expected output.

### Creating Golden Files

1. Write test using `GoldenTest::new("test_name")`
2. Run with update flag:
   ```bash
   SKALP_UPDATE_GOLDEN=1 cargo test --test golden_file_tests
   ```
3. Verify generated file in `tests/golden/test_name.sv`
4. Commit golden file to git

### Updating Golden Files

When you intentionally change output:

```bash
SKALP_UPDATE_GOLDEN=1 cargo test --test golden_file_tests
git diff tests/golden/  # Review changes
git add tests/golden/
git commit -m "Update golden files for feature X"
```

### Golden File Mismatch

When a test fails:

```
Golden file mismatch for test 'alu'
Golden file: tests/golden/alu.sv

Differences:
=============

Line 42:
  Expected: assign result = a + b;
  Actual:   assign result = a + b + 1;

To update the golden file if this change is intentional:
SKALP_UPDATE_GOLDEN=1 cargo test alu
```

## Continuous Integration

See [.github/workflows/README.md](../.github/workflows/README.md) for CI/CD details.

### CI Workflows

1. **CI** (`ci.yml`) - Comprehensive testing on every push/PR
2. **Quick Check** (`quick-check.yml`) - Fast checks on PRs
3. **Release** (`release.yml`) - Automated releases

### Pre-Commit Checks

Before committing:

```bash
# Format check
cargo fmt --all -- --check

# Clippy
cargo clippy --all-targets --all-features -- -D warnings

# Tests
cargo test --workspace

# All in one
cargo fmt --all && cargo clippy --all-targets --all-features && cargo test --workspace
```

## Coverage

### Generate Coverage Report

```bash
# Install tarpaulin
cargo install cargo-tarpaulin

# Generate coverage
cargo tarpaulin --all-features --workspace --timeout 120

# Generate HTML report
cargo tarpaulin --all-features --workspace --out html
open tarpaulin-report.html
```

### Coverage Goals

- **Parser**: >90% line coverage
- **HIR Builder**: >85% line coverage
- **MIR**: >80% line coverage
- **Overall**: >80% line coverage

Current coverage is tracked in CI via Codecov.

## Test Guidelines

### DO

- ✅ Write tests for all new features
- ✅ Test edge cases and error conditions
- ✅ Use descriptive test names
- ✅ Keep tests focused and isolated
- ✅ Update golden files when output changes intentionally
- ✅ Run full test suite before committing

### DON'T

- ❌ Skip tests with `#[ignore]` without good reason
- ❌ Use `unwrap()` or `expect()` in tests (use `assert!` instead)
- ❌ Write tests with side effects
- ❌ Commit failing tests
- ❌ Update golden files without reviewing changes

## Debugging Tests

### Run Single Test with Output

```bash
cargo test test_name -- --nocapture --test-threads=1
```

### Debug Test with GDB/LLDB

```bash
# Build test binary
cargo test --no-run

# Find binary
ls target/debug/deps/parser_regression-*

# Debug with lldb (macOS) or gdb (Linux)
lldb target/debug/deps/parser_regression-xxx
```

### Print Debug Info

```rust
#[test]
fn test_debug() {
    let tree = parse("a + b");
    eprintln!("Tree: {:#?}", tree);  // Always prints
    println!("This only prints with --nocapture");
    assert!(true);
}
```

## Performance Testing

### Benchmark Critical Paths

```bash
cargo bench
```

### Profile Tests

```bash
# Install flamegraph
cargo install flamegraph

# Profile test
cargo flamegraph --test parser_regression -- test_name

# Open flamegraph.svg
```

## Test Maintenance

### Quarterly Review

- Remove redundant tests
- Update outdated examples
- Review coverage gaps
- Update golden files for major changes
- Check for flaky tests

### After Major Refactoring

1. Run full test suite: `cargo test --workspace`
2. Update golden files: `SKALP_UPDATE_GOLDEN=1 cargo test --test golden_file_tests`
3. Review coverage: `cargo tarpaulin`
4. Check CI passes
5. Update documentation

## FAQ

**Q: Why did my golden file test fail?**
A: Either you changed output (expected) or introduced a bug (unexpected). Review the diff and update golden file if intentional.

**Q: Tests pass locally but fail in CI?**
A: Check Rust version, environment variables, and file paths. CI uses different OS and Rust versions.

**Q: How do I test GPU simulation?**
A: Use tests in `crates/skalp-sim/tests/` and ensure Metal framework is available (macOS only).

**Q: Can I run tests in parallel?**
A: Yes, `cargo test` runs in parallel by default. Use `--test-threads=N` to control parallelism.

**Q: How do I test error cases?**
A: Use `assert!(result.is_err())` or `#[should_panic]` attribute for expected panics.
