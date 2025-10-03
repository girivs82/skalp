# GitHub Actions CI/CD Workflows

This directory contains GitHub Actions workflows for automated testing, building, and releasing SKALP.

## Workflows

### `ci.yml` - Continuous Integration

**Triggers:** Push to main/master/develop, pull requests

Runs comprehensive test suite and quality checks:

- **Test Suite Job**
  - Tests on Rust stable and beta
  - Parser regression tests (156 tests)
  - HIR builder regression tests (53 tests)
  - Golden file tests (11 tests)
  - All unit tests across all crates
  - Integration tests
  - Documentation tests

- **Coverage Job**
  - Generates code coverage using tarpaulin
  - Uploads to Codecov for tracking

- **Lint Job**
  - Formatting check (`cargo fmt`)
  - Clippy lints with warnings as errors

- **Build Job**
  - Cross-platform builds (Linux, macOS, Windows)
  - Uploads release binaries as artifacts

- **Security Job**
  - Security audit using `cargo audit`
  - Checks for known vulnerabilities in dependencies

- **Documentation Job**
  - Builds API documentation
  - Uploads as artifact

### `quick-check.yml` - Fast Pre-Commit Checks

**Triggers:** Pull requests, manual dispatch

Runs fast essential checks:

- Format checking
- Clippy lints
- Quick compilation check
- Core regression tests only

Use this for rapid feedback during development.

### `release.yml` - Release Automation

**Triggers:** Version tags (v*), manual dispatch

Automates release process:

1. Creates GitHub release from tag
2. Builds release binaries for all platforms
3. Uploads binaries to release
4. Publishes to crates.io (if configured)

## Usage

### Running Tests Locally

```bash
# Run all tests
cargo test --workspace

# Run specific test suites
cargo test -p skalp-frontend --test parser_regression
cargo test -p skalp-frontend --test hir_builder_regression
cargo test --test golden_file_tests

# Update golden files
SKALP_UPDATE_GOLDEN=1 cargo test --test golden_file_tests

# Run with coverage
cargo tarpaulin --all-features --workspace
```

### Creating a Release

1. Update version in `Cargo.toml`
2. Update `CHANGELOG.md`
3. Commit changes
4. Create and push tag:
   ```bash
   git tag -a v0.2.0 -m "Release v0.2.0"
   git push origin v0.2.0
   ```
5. GitHub Actions will automatically:
   - Run all tests
   - Build binaries for all platforms
   - Create GitHub release
   - Upload binaries

### Badges

Add these to your README.md:

```markdown
![CI](https://github.com/YOUR_ORG/skalp/workflows/CI/badge.svg)
![Quick Check](https://github.com/YOUR_ORG/skalp/workflows/Quick%20Check/badge.svg)
[![codecov](https://codecov.io/gh/YOUR_ORG/skalp/branch/main/graph/badge.svg)](https://codecov.io/gh/YOUR_ORG/skalp)
```

## Configuration

### Secrets Required

- `GITHUB_TOKEN` - Automatically provided by GitHub Actions
- `CARGO_TOKEN` - (Optional) For publishing to crates.io
- `CODECOV_TOKEN` - (Optional) For Codecov upload

### Caching

All workflows use caching to speed up builds:

- Cargo registry cache
- Cargo git cache
- Build target cache

Caches are keyed by `Cargo.lock` hash and OS.

## Test Suite Overview

### Parser Regression Tests
- **156 tests** covering all expression types
- Tests the exact parser bug that was fixed
- Located: `crates/skalp-frontend/tests/parser_regression.rs`

### HIR Builder Regression Tests
- **53 tests** covering entity declarations, implementations, expressions
- Located: `crates/skalp-frontend/tests/hir_builder_regression.rs`

### Golden File Tests
- **11 tests** with stored expected outputs
- Easy update via `SKALP_UPDATE_GOLDEN=1`
- Located: `tests/golden_file_tests.rs`

### End-to-End Tests
- **30 tests** covering all examples and stdlib components
- Full pipeline: Parse → HIR → MIR → Verilog
- Located: `tests/e2e_examples_test.rs`

## Troubleshooting

### Tests Failing in CI but Passing Locally

Check:
- Rust version (CI uses stable + beta)
- Environment differences
- File paths (use absolute paths in tests)

### Slow CI Builds

- Check if caches are working
- Consider splitting test jobs
- Use `quick-check.yml` for rapid feedback

### Release Failures

- Ensure version tags follow `v*` format
- Check secrets are configured
- Verify `Cargo.toml` versions match tag

## Contributing

When adding new tests:

1. Add to appropriate test file (parser, HIR, golden, e2e)
2. Ensure tests pass locally: `cargo test --workspace`
3. Run format and clippy: `cargo fmt && cargo clippy`
4. Push and verify CI passes

## Maintenance

### Weekly Tasks
- Check for failed builds
- Review security audit results
- Update dependencies: `cargo update`

### Monthly Tasks
- Review coverage reports
- Update Rust version if needed
- Check for outdated actions versions
