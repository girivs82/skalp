# Changelog

All notable changes to SKALP will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.1.0] - 2026-02-22

Initial release of the SKALP intent-driven hardware synthesis language and compiler.

### Language

- Intent-first hardware design with progressive abstraction from dataflow to cycle-accurate RTL
- Clock domains as compile-time lifetimes preventing CDC bugs automatically
- Modern type system with traits, generics, const generics, and pattern matching
- Trait-based polymorphism for zero-duplication hardware component reuse
- Built-in verification with assertions, assumptions, and formal properties as first-class citizens
- Power intent attributes (`#[retention]`, `#[isolation]`, `#[level_shift]`, `#[pdc]`)
- Memory configuration attributes (block RAM, distributed, UltraRAM, register files)

### Compiler

- Multi-stage compilation pipeline: Frontend -> MIR -> LIR -> SIR -> Backend
- Code generation targeting SystemVerilog, Verilog, and VHDL
- Hierarchical gate-level synthesis with per-instance optimization
- NCL asynchronous circuit support for clockless, delay-insensitive designs
- ML-guided logic synthesis with AIG-based optimization and learned pass ordering
- iCE40 FPGA backend with place-and-route and programmer support
- Clock domain crossing analysis with automatic synchronizer generation

### Simulation

- Compiled CPU simulation via C++ code generation
- Gate-level CPU simulation
- GPU-accelerated simulation via Metal on macOS
- Debug breakpoints with conditions (`#[breakpoint]`)
- Signal tracing with automatic waveform export (`#[trace]`)

### Tooling

- `skalp build` — compile SKALP source to HDL or gate-level netlists
- `skalp sim` — run simulations with CPU or GPU backends
- `skalp lint` — hardware-aware static analysis
- `skalp fmt` — code formatter
- `skalp new` — project scaffolding with starter templates
- `skalp verify` — formal verification via SVA generation
- Package manager with dependency management (`add`, `remove`, `search`, `update`)
- Language Server Protocol implementation for IDE support (VSCode, Vim, Emacs)
- VSCode extension (`vscode-skalp`)

### Standard Library

- Bitwise operations (CLZ, CTZ, popcount, bit reversal)
- Math functions (trigonometric, exponential, logarithmic, roots)
- Vector operations (arithmetic, dot products, cross products, normalization)
- Fixed-point arithmetic with Q-format, saturation, and rounding
- Common hardware primitives (FIFO, counters, UART, I2C, SPI)

### Platform Support

- Linux (x86_64)
- macOS (x86_64 and ARM64)
- Windows (x86_64)

[0.1.0]: https://github.com/girivs82/skalp/releases/tag/v0.1.0
