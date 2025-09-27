# SKALP: Intent-Driven Hardware Synthesis
*From Sanskrit 'Sankalpana' (संकल्पना): Conception with Purpose*

## Vision & Overview

### The Problem

Current hardware description approaches force designers into a false dichotomy:

1. **Low-level RTL (Verilog/VHDL)**: Full control but tedious, error-prone, and focused on implementation rather than intent
2. **High-level Synthesis (C-based HLS)**: Higher abstraction but unpredictable results, poor QoR, and loss of architectural control

Both approaches share fundamental flaws:
- **Intent is lost**: The "why" disappears in the "how"
- **Abstraction cliff**: No gradual path from algorithm to implementation
- **Clock domain chaos**: CDC bugs are discovered late and fixed manually
- **Protocol proliferation**: Every interface is reimplemented from scratch
- **Verification afterthought**: Correctness is bolted on, not built in

### Our Solution: SKALP

SKALP (derived from Sanskrit 'Sankalpana' - conception with intent) is a hardware description language that preserves design intent through progressive refinement, from algorithm to gates.

#### Core Principles

1. **Intent-First Design**
   - Express WHAT you want to achieve, not just HOW
   - Intent guides synthesis, verification, and optimization
   - Intent is preserved through compilation, not discarded

2. **Progressive Abstraction**
   - Start with dataflow and algorithms
   - Gradually add architectural constraints
   - Drop to cycle-accurate when needed
   - Never forced to over-specify

3. **Correct by Construction**
   - Clock domains in the type system (compile-time CDC safety)
   - Protocols as first-class abstractions
   - Built-in verification properties
   - Strong static typing with inference

4. **Modern Developer Experience**
   - Clean syntax inspired by Rust and VHDL's semantics
   - Powerful type system with traits and generics
   - Pattern matching for state machines
   - Stream operations for dataflow

### What Makes SKALP Different

#### From Verilog/SystemVerilog
- **No reg/wire confusion**: Everything is a signal
- **No blocking/non-blocking ambiguity**: Clear sequential vs combinational
- **Clock domains are typed**: CDC bugs caught at compile time
- **Real type safety**: No silent truncations or extensions

#### From VHDL
- **Concise syntax**: No unnecessary ceremony
- **Type inference**: Strong types without the pain
- **Modern abstractions**: Traits, generics, pattern matching
- **Built-in dataflow**: Not just RTL in disguise

#### From C-based HLS
- **Predictable**: You know what hardware is generated
- **Hardware-native**: Think in clock cycles when needed
- **Gradual**: Mix high-level and low-level in same design
- **Verifiable**: Formal properties part of the language

### Key Innovations

#### 1. Clock Domains as Lifetimes
```rust
signal data: logic<'fast>[32]  // Data "lives" in fast clock domain
signal sync: logic<'slow>[32]  // Different domain

sync = data;  // COMPILE ERROR: Clock domain mismatch
sync = synchronize(data);  // Explicit CDC with automatic insertion
```

#### 2. Protocols, Not Interfaces
```rust
protocol AXI4Stream {
    transaction Beat {
        data: logic[32],
        last: bit
    }
    semantics {
        ordered_delivery,
        backpressure_capable
    }
}
// Same protocol, different physical implementations
```

#### 3. Intent Specifications
```rust
entity Accelerator {
    in data: stream[int[32]]
    out result: stream[int[32]]
} with intent {
    throughput: 100M_samples_per_sec,
    latency: < 10_cycles,
    architecture: systolic_array
}
```

#### 4. Dataflow and RTL in Harmony
```rust
// High-level dataflow
flow {
    output = input
        |> filter(coefficients)
        |> normalize()
        |> quantize(8)
}

// Drop to RTL when needed
process(clock.rise) {
    if (reset) {
        state := Idle
    } else {
        // Explicit cycle-level control
    }
}
```

### Use Cases

#### Ideal For:
- **SoC Development**: From algorithm to optimized RTL
- **DSP/ML Accelerators**: Dataflow with architectural control
- **Protocol Bridges**: Safe, verified protocol conversion
- **Safety-Critical**: Formal verification built-in
- **Reusable IP**: Generic, parameterized components

#### Migration Path:
1. Import existing Verilog/VHDL as black boxes
2. Write new components in Flux
3. Gradually replace legacy code
4. Full Flux design with optimal QoR

### Development Roadmap

#### Phase 1: Core Language (Current)
- Language specification
- Basic compiler to SystemVerilog
- Clock domain safety
- Intent system

#### Phase 2: Synthesis
- Advanced optimization based on intent
- Multiple target architectures
- Resource estimation
- Timing analysis

#### Phase 3: Verification
- Formal verification integration
- Coverage-driven verification
- Assertion synthesis
- Property checking

#### Phase 4: Ecosystem
- IDE support (VS Code, Neovim)
- Package manager
- IP library
- Documentation generation

### Example: Complete Design

```rust
// High-level algorithm with intent in SKALP
entity FIRFilter {
    in samples: stream[fixed[16.16]] @ 'audio_clk
    out filtered: stream[fixed[16.16]] @ 'audio_clk
} with intent {
    filter_type: lowpass(cutoff: 1kHz),
    implementation: transposed_form,
    resource_sharing: full
}

impl FIRFilter {
    const coefficients = calculate_coefficients();

    flow {
        filtered = samples
            |> window(32)
            |> convolve(coefficients)
            |> saturate()
    }
}

// Instantiation with different intents
let filter_fast = FIRFilter with intent { optimization: speed };
let filter_small = FIRFilter with intent { optimization: area };
```

### Getting Started

```bash
# Install SKALP
cargo install skalp

# Create new project
skalp new my_accelerator
cd my_accelerator

# Write your design
edit src/main.sk

# Compile to SystemVerilog
skalp build --target systemverilog

# Verify design
skalp verify --formal

# Generate documentation
skalpdoc
```

### Philosophy

SKALP believes that hardware description should be:
- **Intentional**: Design decisions are explicit and preserved
- **Progressive**: Abstract when possible, detailed when necessary
- **Safe**: Entire classes of bugs eliminated at compile time
- **Composable**: Build complex from simple, reliably
- **Verifiable**: Correctness is intrinsic, not extrinsic

### Join Us

SKALP is open source and community-driven. We're building the hardware description language we've always wanted.

- GitHub: github.com/skalp-lang/skalp
- Discord: discord.gg/skalp
- Documentation: docs.skalp-lang.org

---

*"संकल्पना - Where conception becomes circuit"*

*"Hardware description should describe intent, not just implementation"*