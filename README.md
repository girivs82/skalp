# SKALP

**Intent-driven hardware synthesis language and compiler.**

[![CI](https://github.com/girivs82/skalp/actions/workflows/ci.yml/badge.svg)](https://github.com/girivs82/skalp/actions/workflows/ci.yml)

## What is SKALP?

SKALP is a hardware description language that preserves design intent through progressive refinement, from algorithm to gates. It combines a modern type system (traits, generics, pattern matching) with compile-time clock domain safety, an integrated synthesis and simulation toolchain, and formal verification — all in a single binary.

The name comes from Sanskrit *Sankalpana* (संकल्पना), meaning "conception with purpose."

## Key Features

- **Clock domain safety** — clock domains as compile-time lifetimes; CDC bugs caught before simulation
- **Four-stage IR pipeline** — HIR → MIR → LIR → SIR with optimization at every level
- **Integrated toolchain** — compiler, simulator, synthesizer, formatter, linter, package manager, and LSP in one binary
- **GPU-accelerated fault simulation** — Metal backend for parallel fault injection on macOS
- **iCE40 FPGA backend** — synthesis, place-and-route, and programmer support for iCE40 devices
- **Formal verification** — equivalence checking (simulation + SAT), SVA generation, bounded model checking
- **ISO 26262 safety analysis** — fault injection-driven FMEA/FMEDA with measured diagnostic coverage
- **NCL async circuits** — Null Convention Logic support for clockless, delay-insensitive designs
- **ML-guided synthesis** — train and apply ML models for optimization pass ordering

## Quick Example

```
// examples/counter.sk — an 8-bit counter

entity Counter {
    in clk: clock
    in rst: reset
    out count: nat[8]
}

impl Counter {
    signal counter: nat[8] = 0

    on(clk.rise) {
        if (rst) {
            counter = 0
        } else {
            counter = counter + 1
        }
    }

    count = counter
}
```

## Installation

### Pre-built binaries

Download the latest release for your platform from [GitHub Releases](https://github.com/girivs82/skalp/releases).

Binaries are available for Linux (x86_64), macOS (x86_64 and ARM64), and Windows (x86_64).

### Build from source

```bash
git clone https://github.com/girivs82/skalp.git
cd skalp
cargo build --release
```

The binary is at `target/release/skalp`.

Requires Rust 1.70+ (stable). GPU simulation requires macOS with Metal support.

## Getting Started

```bash
# Create a new project
skalp new my_design
cd my_design

# Build to SystemVerilog (default)
skalp build

# Simulate the design
skalp sim build/design.lir --duration 1000

# Synthesize for iCE40 FPGA
skalp synth src/main.sk --device ice40-hx8k
```

See the [tutorial](https://mikaana.com/projects/skalp/) for a walkthrough.

## CLI Commands

| Command | Description |
|---------|-------------|
| `skalp new <name>` | Create a new project from a starter template |
| `skalp build` | Compile SKALP source to HDL or gate-level netlists |
| `skalp sim <file>` | Run behavioral or gate-level simulation (CPU or GPU) |
| `skalp synth <file>` | Synthesize for iCE40 FPGA with optional place-and-route |
| `skalp pnr <netlist>` | Place and route an existing gate-level netlist |
| `skalp program <bitstream>` | Program an iCE40 FPGA board |
| `skalp fmt [files]` | Format SKALP source files |
| `skalp test [filter]` | Run tests |
| `skalp analyze <file>` | Gate-level analysis and fault simulation |
| `skalp ec <file>` | Equivalence checking between RTL and gate-level |
| `skalp safety` | ISO 26262 fault injection safety analysis |
| `skalp compile <file>` | Compile to pre-compiled IP format (.skb) |
| `skalp trace <file> <signal>` | Trace a signal through the gate-level netlist |
| `skalp train` | Train ML pass ordering model from collected data |
| `skalp add <pkg>` | Add a dependency |
| `skalp remove <pkg>` | Remove a dependency |
| `skalp update [pkg]` | Update dependencies |
| `skalp search <query>` | Search the package registry |
| `skalp cache` | Manage the package cache |

Run `skalp <command> --help` for detailed options.

## Documentation

- [Tutorial](https://mikaana.com/projects/skalp/) — guided introduction
- [Language Specification](docs/LANGUAGE_SPECIFICATION.md) — complete language reference
- [Formal Grammar](docs/GRAMMAR.ebnf) — EBNF grammar
- [Compiler Architecture](docs/COMPILER_ARCHITECTURE.md) — multi-layer IR design
- [Simulation Architecture](docs/SIMULATION_ARCHITECTURE.md) — CPU and GPU simulation
- [Full Flow Architecture](docs/FULL_FLOW_ARCHITECTURE.md) — end-to-end FPGA compilation

## Project Structure

```
skalp/
├── src/                          # CLI binary
├── crates/
│   ├── skalp-frontend/           # Lexer, parser, HIR
│   ├── skalp-mir/                # Mid-level IR and optimization
│   ├── skalp-lir/                # Low-level IR and netlist
│   ├── skalp-sir/                # Simulation IR
│   ├── skalp-codegen/            # Code generation (SystemVerilog, VHDL, Verilog)
│   ├── skalp-backends/           # Backend trait and target definitions
│   ├── skalp-sim/                # Simulation engine (CPU + GPU)
│   ├── skalp-place-route/        # iCE40 place-and-route and programmer
│   ├── skalp-verify/             # Formal verification (SVA, equivalence checking)
│   ├── skalp-formal/             # SAT-based formal methods
│   ├── skalp-safety/             # ISO 26262 safety analysis
│   ├── skalp-lint/               # Hardware-aware linter
│   ├── skalp-lsp/                # Language Server Protocol
│   ├── skalp-stdlib/             # Standard library (bitops, math, vectors, fixed-point)
│   ├── skalp-testing/            # Test infrastructure and testbench API
│   ├── skalp-parallel/           # Parallel compilation
│   ├── skalp-incremental/        # Incremental compilation
│   ├── skalp-asic/               # ASIC backend
│   ├── skalp-ml/                 # ML-guided synthesis optimization
│   ├── skalp-resolve/            # Dependency resolution
│   ├── skalp-manifest/           # Project manifest (skalp.toml)
│   └── skalp-package/            # Package manager
├── docs/                         # Specifications and architecture docs
├── examples/                     # Example SKALP designs
└── scripts/                      # CI and development scripts
```

## Platform Support

| Platform | Arch | Simulation | GPU Accel |
|----------|------|------------|-----------|
| Linux | x86_64 | CPU | — |
| macOS | x86_64 | CPU + GPU | Metal |
| macOS | ARM64 | CPU + GPU | Metal |
| Windows | x86_64 | CPU | — |

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md) for build instructions, testing, and how to submit changes.

## License

[MIT](LICENSE)
