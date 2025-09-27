# SKALP - Intent-Driven Hardware Synthesis

> **संकल्पना** *(Sankalpana)* - Where conception becomes circuit

SKALP is a modern hardware description language that preserves design intent through progressive refinement, from algorithm to gates.

## Why SKALP?

The name SKALP comes from Sanskrit 'Sankalpana' (संकल्पना), meaning "conception with purpose" or "intent-driven creation". Just as Sankalpana represents the mental conception before physical manifestation, SKALP captures your design intent and transforms it into efficient hardware.

## Key Features

- **Intent-First Design**: Express what you want to achieve, not just how
- **Clock Domain Safety**: Clock domains as lifetimes - CDC bugs caught at compile time
- **Progressive Abstraction**: From high-level dataflow to cycle-accurate RTL
- **Modern Type System**: Traits, generics, pattern matching from Rust
- **Built-in Verification**: Assertions and formal properties as first-class citizens
- **Protocol Abstractions**: Define protocols once, implement anywhere

## Quick Example

```rust
// Design with intent in SKALP
entity Accelerator {
    in data: stream<'clk>[32]    // Clock domain in type system
    out result: stream<'clk>[32]
} with intent {
    throughput: 100M_samples_per_sec,
    architecture: systolic_array,
    optimization: balanced(speed: 0.7, area: 0.3)
}

impl Accelerator {
    flow {
        result = data
            |> map(transform)
            |> filter(threshold)
            |> reduce(accumulate)
    }
}
```

## Documentation

- [Vision & Overview](docs/VISION.md) - Why SKALP exists and what it solves
- [Language Specification](docs/LANGUAGE_SPECIFICATION.md) - Complete language reference
- [Formal Grammar](docs/GRAMMAR.ebnf) - EBNF grammar specification
- [Compiler Architecture](docs/COMPILER_ARCHITECTURE.md) - Multi-layer IR design and synthesis flow
- [Full Flow Architecture](docs/FULL_FLOW_ARCHITECTURE.md) - End-to-end compilation for open FPGAs
- [Simulation Architecture](docs/SIMULATION_ARCHITECTURE.md) - GPU-accelerated simulation

## Installation

```bash
# Coming soon
cargo install skalp

# Create a new project
skalp new my_design
cd my_design

# Write your design
edit src/main.sk

# Build to SystemVerilog
skalp build --target sv
```

## File Extensions

- `.sk` - SKALP source files
- `.skalp` - Alternative extension

## Tool Ecosystem (Planned)

- `skalp` - Main compiler and build tool
- `skalpfmt` - Code formatter
- `skalpdoc` - Documentation generator
- `skalptest` - Test framework
- `skalp-lsp` - Language server for IDE support

## Design Philosophy

SKALP embodies the Sanskrit concept of Sankalpana - the power of conception and intention. In hardware design, the intent behind a circuit is often lost in implementation details. SKALP preserves this intent throughout the design flow:

1. **Conceive** - Express your algorithm and intent
2. **Refine** - Progressively add architectural details
3. **Synthesize** - Generate optimized RTL guided by intent
4. **Verify** - Ensure implementation matches conception

## Comparison with Other HDLs

| Feature | SKALP | SystemVerilog | VHDL | Chisel |
|---------|-------|---------------|------|--------|
| Type Safety | ✅ Strong | ❌ Weak | ✅ Strong | ✅ Strong |
| Clock Domain Safety | ✅ Compile-time | ❌ None | ❌ None | ❌ None |
| Intent Preservation | ✅ First-class | ❌ None | ❌ None | ❌ None |
| Modern Abstractions | ✅ Traits, Generics | ⚠️ Limited | ❌ None | ✅ Scala |
| Progressive Refinement | ✅ Built-in | ❌ None | ❌ None | ⚠️ Limited |
| Verification | ✅ Built-in | ⚠️ SVA | ⚠️ PSL | ❌ External |

## Current Status

SKALP is currently in early development. We have completed:

- ✅ Language design and specification (complete)
- ✅ Compiler architecture design (complete)
- ✅ Full flow architecture for open FPGAs (complete)
- ✅ Formal grammar specification (complete)
- ✅ Rust project structure setup (complete)
- 🚧 Building the compiler implementation (in progress)
- 📋 Standard library development (planned)
- 📋 iCE40 backend implementation (planned)
- 📋 Place & route engine (planned)

## Project Structure

```\nskalp/\n├── src/                    # Main CLI binary\n├── crates/                 # Workspace crates\n│   ├── skalp-frontend/     # Lexer, parser, HIR\n│   ├── skalp-mir/          # Mid-level IR and optimization\n│   ├── skalp-lir/          # Low-level IR and netlist\n│   ├── skalp-codegen/      # Code generation (SV/VHDL/Verilog)\n│   ├── skalp-sim/          # GPU simulation engine\n│   └── skalp-place-route/  # Place & route for open FPGAs\n├── docs/                   # Documentation\n├── examples/               # Example SKALP designs\n└── skalp.toml             # Project configuration\n```\n\n## Contributing\n\nWe welcome contributions! SKALP is being built by the community, for the community.

## License

MIT

---

*संकल्पना - Sankalpana - From conception to silicon*