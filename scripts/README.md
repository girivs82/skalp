# Scripts

Development and CI helper scripts for the SKALP project.

## ci_check.sh

Local validation script that runs the same checks as CI before you push.

**Usage:**
```bash
./scripts/ci_check.sh
```

**What it checks:**
- Code formatting (`cargo fmt --check`)
- Clippy lints on stable toolchain
- Clippy lints on beta toolchain
- Build with all features

**Requirements:**
- Rust stable and beta toolchains installed
- Run from the repository root

**Tip:** Run this script before pushing to catch CI failures early and avoid the push-fail-fix cycle.
