# Scripts

Development and CI helper scripts for the SKALP project.

## ci_check.sh

Local validation script that runs the same checks as CI before you push.

**Usage:**
```bash
# Basic checks (formatting, clippy, build)
./scripts/ci_check.sh

# Include coverage check (optional, requires z3)
./scripts/ci_check.sh --with-coverage
```

**What it checks:**
- Code formatting (`cargo fmt --check`)
- Clippy lints on stable toolchain
- Clippy lints on beta toolchain
- Build with all features
- All tests (including regression tests)
- Code coverage with tarpaulin (optional, with `--with-coverage` flag)

**Requirements:**
- Rust stable and beta toolchains installed
- Run from the repository root
- For coverage: `cargo install cargo-tarpaulin` and z3 SMT solver installed

**Tip:** Run this script before pushing to catch CI failures early and avoid the push-fail-fix cycle.

**Note on coverage:** The coverage check requires z3 to be installed. If you don't have it, the basic checks (without `--with-coverage`) are sufficient for most development.
