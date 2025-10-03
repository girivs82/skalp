# Getting Started with SKALP

This guide will get you up and running with SKALP in under 30 minutes.

## Installation

### Prerequisites
- Rust 1.70+ 
- Metal-compatible GPU (for simulation)
- Optional: VSCode for IDE support

### Install SKALP

```bash
# Clone the repository
git clone https://github.com/your-org/skalp.git
cd skalp

# Build the compiler
cargo build --release

# Add to PATH
export PATH=$PATH:$(pwd)/target/release
```

### Verify Installation

```bash
skalp --version
# Should output: SKALP 0.1.0
```

## Your First SKALP Design

Create a file called `counter.sk`:

```skalp
entity counter {
    in clk: clock;
    in rst: reset;
    out count: bit<32>;
}

impl counter {
    signal counter_reg: bit<32>;

    on(clk.rise) {
        if rst.active {
            counter_reg <= 0;
        } else {
            counter_reg <= counter_reg + 1;
        }
    }

    count = counter_reg;
}
```

## Compile and Simulate

```bash
# Build to SystemVerilog (for synthesis)
skalp build -s counter.sk -o build -t sv

# Build to MIR (for simulation)
skalp build -s counter.sk -o build -t mir

# Run GPU-accelerated simulation
skalp sim build/design.mir --duration 100

# Build to other formats
skalp build -s counter.sk -o build -t vhdl  # VHDL output
skalp build -s counter.sk -o build -t lir   # LIR output

# Format the code
skalp fmt counter.sk

# Synthesize for FPGA
skalp synth counter.sk --device ice40-hx8k

# Program the FPGA device (if connected)
skalp program bitstream.bin
```

## IDE Setup

### VSCode Extension

1. Install the SKALP extension from the marketplace
2. Open a `.sk` file
3. Get syntax highlighting, error checking, and auto-completion

### Language Server

The LSP server provides:
- **Real-time error checking** - See syntax and type errors as you type
- **Code completion** - Auto-complete entities, signals, and types
- **Go to definition** - Navigate to signal and entity definitions
- **Hover information** - See type information and documentation

## Next Steps

- [Language Tutorial](tutorial.md) - Learn SKALP syntax and concepts
- [Examples](examples/) - Explore example designs
- [Language Specification](language-spec.md) - Complete reference

## Common Issues

### GPU Simulation Not Working
- Ensure you have a Metal-compatible GPU
- Check that Metal development tools are installed
- Try CPU simulation with `--cpu` flag

### Compilation Errors
- Check syntax against the [language specification](language-spec.md)
- Ensure all signals and entities are properly declared
- Use `skalp check` for detailed error information

### Performance Issues
- Large designs may need optimization passes
- Consider using `--opt-level 2` for better performance
- See [Performance Guide](performance.md) for tuning tips
