# Claude Development Guidelines

This document contains guidelines for Claude Code (or developers using Claude) when working on the SKALP project.

## Before Pushing Changes

**ALWAYS run the local CI validation script before pushing:**

```bash
./scripts/ci_check.sh
```

This script runs the essential CI checks:
- Code formatting (`cargo fmt --check`)
- Clippy lints on stable toolchain
- Clippy lints on beta toolchain
- Build with all features

Optionally, if you have z3 installed and want to test code coverage:
```bash
./scripts/ci_check.sh --with-coverage
```

**Why?** Running these checks locally catches issues before they reach CI, avoiding the slow push-fail-fix cycle.

**Note:** The coverage check is optional and requires z3 SMT solver. The formal verification module (`skalp-verify`) is excluded from tarpaulin coverage due to ouroboros macro compatibility issues with tarpaulin's instrumentation.

## CI Pipeline Overview

The project uses GitHub Actions with multiple jobs:

1. **Test Suite** (stable & beta Rust)
   - Formatting check
   - Clippy with `-D warnings`
   - Build
   - All test suites

2. **Code Coverage**
   - Uses `cargo tarpaulin`
   - May expose different compilation issues than regular clippy

3. **Lint**
   - Standalone formatting and clippy checks

4. **Build**
   - Cross-platform builds (Linux, macOS, Windows)

5. **Security Audit**
   - Runs `cargo audit`

## Common CI Failure Patterns

### 1. Formatting Issues
- **Symptom**: `cargo fmt --check` fails
- **Fix**: Run `cargo fmt --all` before committing
- **Prevention**: Always include in pre-push checks

### 2. Clippy Lint Differences Between Stable and Beta
- **Symptom**: Passes on one toolchain, fails on another
- **Cause**: Beta has stricter/newer lints
- **Fix**: Test with both toolchains using the CI check script
- **Common issues**:
  - New lints in beta not recognized by stable (use `#[allow(unknown_lints)]`)
  - Unstable features available in beta but not stable

### 3. Platform-Specific Code
- **Symptom**: macOS-only features fail on Linux CI
- **Fix**: Use `#[cfg(target_os = "macos")]` for platform-specific code
- **Example**: Metal GPU code, some test utilities

### 4. Tarpaulin Coverage Issues
- **Symptom**: Code compiles normally but fails under `cargo tarpaulin`
- **Cause**: Tarpaulin uses `--cfg=tarpaulin` which can affect macro expansion
- **Fix**: Test with tarpaulin locally if modifying complex macros (like ouroboros)

## Best Practices

### Commits
- Run `./scripts/ci_check.sh` before every push
- Use conventional commit messages
- Include context in commit messages (what and why)
- Co-author commits with Claude when appropriate

### Testing
- Write tests for new features
- Ensure tests pass on all platforms
- Use `#[cfg(test)]` and `#[cfg(target_os = "...")]` appropriately

### Dependencies
- Prefer stable, well-maintained crates
- Check for security advisories with `cargo audit`
- Document why a dependency is needed

### Code Quality
- Address all clippy warnings (CI uses `-D warnings`)
- Keep functions focused and documented
- Use meaningful variable names
- Add comments for complex logic

## Toolchain Requirements

- **Rust stable**: Primary development toolchain
- **Rust beta**: For testing upcoming lints
- **cargo-tarpaulin**: For coverage (optional locally)
- **cargo-audit**: For security audits (optional locally)

Install beta toolchain:
```bash
rustup install beta
```

## Debugging CI Failures

1. **Check the logs**:
   ```bash
   gh run list --limit 1
   gh run view <run-id> --log-failed
   ```

2. **Reproduce locally**:
   ```bash
   ./scripts/ci_check.sh
   ```

3. **Test specific toolchain**:
   ```bash
   cargo +beta clippy --all-targets --all-features -- -D warnings
   ```

4. **Test with tarpaulin** (if coverage fails):
   ```bash
   cargo tarpaulin --all-features
   ```

## Quick Reference

| Command | Purpose |
|---------|---------|
| `./scripts/ci_check.sh` | Run all CI checks locally |
| `cargo fmt --all` | Format code |
| `cargo fmt --all -- --check` | Check formatting without modifying |
| `cargo clippy --all-targets --all-features -- -D warnings` | Run clippy |
| `cargo +beta clippy ...` | Run clippy on beta |
| `cargo test --all-features` | Run all tests |
| `gh run list` | List recent CI runs |
| `gh run watch <id>` | Watch a CI run in progress |

## Getting Help

- Check CI logs for specific errors
- Review this document for common patterns
- Check `scripts/README.md` for script documentation
- Review existing code for examples
