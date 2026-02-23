# Contributing to SKALP

## Build from source

```bash
git clone https://github.com/girivs82/skalp.git
cd skalp
cargo build --release
```

Requires Rust stable (1.70+). Install the beta toolchain too for lint checking:

```bash
rustup install beta
```

## Run tests

```bash
cargo test --all-features
```

## Run CI checks locally

Before pushing, always run the local CI validation script:

```bash
./scripts/ci_check.sh
```

This checks formatting, clippy on stable and beta, and builds with all features. It catches the same issues that CI will catch, so you avoid the slow push-fail-fix cycle.

## Code style

- Run `cargo fmt --all` before committing
- Fix all clippy warnings — CI uses `-D warnings` on both stable and beta
- Keep functions focused; add comments only where the logic isn't self-evident

## Submitting changes

1. Fork the repository
2. Create a feature branch (`git checkout -b my-feature`)
3. Make your changes and add tests
4. Run `./scripts/ci_check.sh` to verify
5. Open a pull request against `master`

## Reporting issues

- Use the [bug report template](https://github.com/girivs82/skalp/issues/new?template=bug_report.md) for bugs
- Use the [feature request template](https://github.com/girivs82/skalp/issues/new?template=feature_request.md) for ideas

## Architecture

See the [docs/](docs/) directory for specifications and architecture documents:

- [Compiler Architecture](docs/COMPILER_ARCHITECTURE.md) — IR pipeline design
- [Simulation Architecture](docs/SIMULATION_ARCHITECTURE.md) — CPU and GPU backends
- [Full Flow Architecture](docs/FULL_FLOW_ARCHITECTURE.md) — end-to-end FPGA flow
