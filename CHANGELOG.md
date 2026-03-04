# Changelog

All notable changes to SKALP will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.2.0] - 2026-03-04

### Added

- **VHDL Frontend** — first-class VHDL-2008/2019 input language
  - Complete lexer and parser for synthesizable VHDL subset
  - HIR lowering from VHDL AST into the shared compiler pipeline
  - VHDL-2019 interfaces and mode views
  - Generic type parameters and generic package instantiation
  - Synthesizable subset enforcement at parse time
  - Rust async testbench support for VHDL designs
  - Hierarchical VHDL-2019 support with end-to-end behavioral simulation
  - VHDL generics wired into native const eval flow
  - Deferred type resolution and attribute evaluation through the generic pipeline
  - VHDL-to-SystemVerilog transpilation
- **Rich Diagnostics** — rustc-style error reporting using `codespan-reporting`
- **VHDL LSP Support** — language server protocol support for VHDL files, including semantic tokens and schematic support
- **VHDL Formatter** — Wadler-Lindig pretty-printing formatter for VHDL source
- **skalp Source Formatter** — `skalp fmt` rewritten with Wadler-Lindig pretty-printing
- **HIR Codegen** — HIR-based code generation for skalp, VHDL, and SystemVerilog with comment preservation, entity deduplication, and per-entity file output
- **InputTiming** — testbench control for waveform-aligned input drives
- **Stack Overflow Protection** — `stacker`-based overflow protection in `skalp-sir` and `skalp-mir`
- **Cross-process Serialization** — `flock`-based compilation serialization replacing in-process `Mutex`
- **Compiler Fingerprinting** — compiler fingerprint included in SIR cache key to prevent stale cache hits
- **VS Code Extension** — `.vsix` included in GitHub releases, with VHDL support, waveform viewer, schematic viewer, debugger, and testbench scaffolding
- VHDL file support in `skalp-debug` server
- VHDL frontend architecture design document

### Fixed

- Async reset pattern causing double-increment in simulator
- Dynamic array read/write and conditional default propagation in hierarchical elaboration
- Multi-entity elaboration bugs causing simulation failures
- Signal initial value propagation in MIR→SIR
- 4 codegen bugs and adder.sk width issue
- Struct literal with entity field access crash (Bug #85)
- CDC clock lifetime parameters: lexer and parser support
- Widening add (`+:`) operator across all compiler stages
- Redundant `if(clk)` in `always_ff` and recursive target collection
- `VariableId` collision in `entity_instance_outputs` across entities
- Parser infinite loop on real-world VHDL
- All VHDL parser gaps found across 6 stress-test projects
- 3 VHDL features to work end-to-end through MIR/codegen
- C++ and Metal shader compilation OOM/SIGKILL via serialization
- Clippy warnings across `skalp-lsp`, `skalp-verify`, `skalp-frontend`, `skalp-vhdl`

### New Crates

- `skalp-vhdl` — VHDL frontend (lex → parse → HIR lowering)
- `skalp-hir-codegen` — HIR-based code generation

## [0.1.1] - 2026-02-23

### Changed

- Rewrote README with accurate project structure, CLI command reference, and correct URLs
- Added CONTRIBUTING.md with build, test, and submission guidelines
- Added GitHub issue templates for bug reports and feature requests
- Set repository topics, homepage, and enabled Discussions

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
- AIG-based logic optimization with configurable pass sequences
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

[0.2.0]: https://github.com/girivs82/skalp/releases/tag/v0.2.0
[0.1.1]: https://github.com/girivs82/skalp/releases/tag/v0.1.1
[0.1.0]: https://github.com/girivs82/skalp/releases/tag/v0.1.0
