# Getting Started with SKALP

This guide will get you up and running with SKALP in under 30 minutes.

## Installation

### Prerequisites
- Rust 1.70+
- Optional: Metal-compatible GPU (for `skalp sim --gpu`)
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
# Should output: skalp 0.2.0
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
            counter_reg = 0;
        } else {
            counter_reg = counter_reg + 1;
        }
    }

    count = counter_reg;
}
```

## Compile and Simulate

```bash
# Build to SystemVerilog (for synthesis)
skalp build counter.sk -o build

# Simulate directly from source (writes a .skw.gz waveform)
skalp sim counter.sk --duration 100

# Build to other formats
skalp build counter.sk -o build -t vhdl   # VHDL output
skalp build counter.sk -o build -t mir    # Mid-level IR (debugging)
skalp build counter.sk -o build -t gates  # Gate-level netlist

# Format the code
skalp fmt counter.sk

# Synthesize for FPGA
skalp synth counter.sk --device ice40-hx8k

# Program the FPGA device (if connected)
skalp program bitstream.bin

# Formal equivalence check (RTL vs synthesized gates)
skalp ec counter.sk
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
- [Syntax Reference](user/reference/syntax.md) - Complete reference

## Common Issues

### GPU Simulation Not Working
- GPU acceleration is opt-in: pass `--gpu` to `skalp sim`
- Ensure you have a Metal-compatible GPU (macOS)
- The default is CPU simulation, which always works

### Compilation Errors
- Check syntax against the [syntax reference](user/reference/syntax.md)
- Ensure all signals and entities are properly declared
- Every declared output must be driven — an undriven output is a compile error
- Run with `skalp -v build` for detailed phase-by-phase information

### Performance Issues
- Synthesis optimization presets: `skalp build --optimize <quick|balanced|full|timing|area>`
- `--no-synth-opt` disables synthesis optimization for debugging
