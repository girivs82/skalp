# SKALP Documentation

Welcome to the SKALP hardware description language documentation!

## Quick Links

- [Getting Started](getting-started.md) - Installation and first steps
- [Language Reference](reference/README.md) - Complete language specification
- [Standard Library](stdlib/README.md) - Built-in components and utilities
- [Examples](examples/README.md) - Sample designs and tutorials

## What is SKALP?

SKALP (Semantically Knowledgeable Architecture Language with Pragmatic intent) is an intent-driven hardware synthesis language designed for modern hardware development. It combines:

- **Safety by Design**: Clock domain safety, formal verification, ISO 26262 compliance
- **Modern Abstractions**: Pattern matching, traits, generics, flow blocks
- **Performance**: GPU-accelerated simulation, advanced synthesis optimizations
- **Developer Experience**: LSP support, comprehensive tooling, clear error messages

## Key Features

### 1. Clock Domain Safety
SKALP treats clock domains as lifetimes, preventing metastability issues at compile time:

```skalp
entity AsyncFIFO<'wr, 'rd> {
    in wr_clk: clock<'wr>;
    in rd_clk: clock<'rd>;
    in data<'wr>: bit<8>;
    out q<'rd>: bit<8>;
}
```

### 2. Intent-Driven Design
Express design intent for automatic optimization:

```skalp
@intent("minimize_area")
@intent("power < 100mW")
entity LowPowerCore {
    // SKALP optimizes for area and power
}
```

### 3. Modern Language Features
- Pattern matching with `match`
- Trait system for abstraction
- Generic entities with const parameters
- Flow blocks for pipeline design
- Async/await testbenches

### 4. Comprehensive Verification
Built-in support for assertions, coverage, and formal verification:

```skalp
assert property (req |-> ##[1:3] ack);
cover property (state == ACTIVE);
```

### 5. Safety-Critical Support
ISO 26262 compliance with ASIL levels, FMEA generation, and safety mechanisms:

```skalp
@safety(asil = "ASIL-D")
@requirement("REQ-SAFETY-001: Dual-redundant computation")
entity SafetyCore {
    // Automatic FMEA and safety metric calculation
}
```

## Documentation Structure

- **Getting Started** - Installation, setup, and your first SKALP design
- **Language Reference** - Detailed language specification and syntax
- **Standard Library** - Pre-built components (FIFO, UART, etc.)
- **Examples** - Complete working examples with explanations
- **API Documentation** - Compiler and tool APIs
- **Migration Guide** - Moving from Verilog/VHDL to SKALP

## Installation

### From Source

```bash
# Clone the repository
git clone https://github.com/skalp-lang/skalp.git
cd skalp

# Build the compiler
cargo build --release

# Add to PATH
export PATH=$PATH:$(pwd)/target/release

# Verify installation
skalp --version
```

### VS Code Extension

1. Install the SKALP extension from the marketplace
2. Configure the language server path if needed
3. Open any `.sk` or `.skalp` file to activate

## Hello World

```skalp
entity Counter {
    in clk: clock;
    in rst: reset(active_high);
    out count: nat<8>;
}

impl Counter {
    on(rst.active) {
        count <= 0;
    }

    on(clk.rise) {
        count <= count + 1;
    }
}
```

## Community

- **GitHub**: [github.com/skalp-lang/skalp](https://github.com/skalp-lang/skalp)
- **Discord**: Join our community server
- **Forum**: [discuss.skalp-lang.org](https://discuss.skalp-lang.org)

## License

SKALP is open source under the MIT license.