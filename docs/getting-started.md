# Getting Started with SKALP

This guide will help you get up and running with SKALP in minutes.

## Prerequisites

- Rust 1.70+ (for building from source)
- VS Code (recommended for IDE support)
- Verilator or another HDL simulator (optional, for simulation)

## Installation

### Building from Source

```bash
# Clone the repository
git clone https://github.com/skalp-lang/skalp.git
cd skalp

# Build all components
cargo build --release

# Run tests to verify installation
cargo test

# Install globally (optional)
cargo install --path .
```

### Setting up VS Code

1. Build the language server:
```bash
cd crates/skalp-lsp
cargo build --release
```

2. Install the VS Code extension:
```bash
cd vscode-skalp
npm install
npm run compile
```

3. Open VS Code and press F5 to run the extension in development mode

## Your First SKALP Design

### 1. Create a Simple Counter

Create a file named `counter.sk`:

```skalp
// A simple 8-bit counter with reset
entity Counter {
    in clk: clock;
    in rst: reset(active_high);
    in enable: bit;
    out count: bit<8>;
}

impl Counter {
    on(rst.active) {
        count <= 0;
    }

    on(clk.rise) {
        if enable {
            count <= count + 1;
        }
    }
}
```

### 2. Compile to SystemVerilog

```bash
skalp compile counter.sk -o counter.sv
```

This generates SystemVerilog code that can be used with any standard tools.

### 3. Create a Testbench

Create `counter_tb.sk`:

```skalp
testbench CounterTest {
    signal clk: clock;
    signal rst: reset(active_high);
    signal enable: bit;
    signal count: bit<8>;

    // Instantiate the counter
    Counter dut {
        .clk(clk),
        .rst(rst),
        .enable(enable),
        .count(count)
    };

    // Clock generation
    initial {
        clk = 0;
        forever #5 clk = !clk;
    }

    // Test sequence
    async test "basic_counting" {
        // Reset
        rst = 1;
        enable = 0;
        await clk.rise;
        await clk.rise;

        // Release reset
        rst = 0;
        await clk.rise;

        // Enable counting
        enable = 1;

        // Check counting
        for i in 0..10 {
            assert count == i;
            await clk.rise;
        }

        // Disable counting
        enable = 0;
        let prev_count = count;
        await clk.rise;
        assert count == prev_count;

        $display("Test passed!");
    }
}
```

### 4. Run Simulation

```bash
skalp sim counter_tb.sk
```

## Key Concepts

### Event-Driven Blocks

SKALP uses `on()` blocks for sequential logic:

```skalp
on(clk.rise) {
    // Synchronous logic on rising edge
}

on(clk.fall) {
    // Synchronous logic on falling edge
}

on(rst.active) {
    // Reset logic
}
```

### Clock Domain Safety

SKALP automatically tracks clock domains:

```skalp
entity CDCExample<'fast, 'slow> {
    in fast_clk: clock<'fast>;
    in slow_clk: clock<'slow>;
    in data<'fast>: bit<8>;
    out synced<'slow>: bit<8>;
}
```

### Pattern Matching

Use `match` for clear conditional logic:

```skalp
match state {
    IDLE => next_state = START;
    START => next_state = PROCESS;
    PROCESS => {
        if done {
            next_state = IDLE;
        }
    }
    _ => next_state = IDLE;
}
```

### Generics

Create reusable components with generics:

```skalp
entity FIFO<const WIDTH: nat = 8, const DEPTH: nat = 16> {
    in wr_data: bit<WIDTH>;
    // ...
}

// Instantiate with specific parameters
FIFO<16, 32> data_fifo { /* ... */ };
```

## Next Steps

- Read the [Language Reference](reference/README.md) for complete syntax
- Explore the [Standard Library](stdlib/README.md) for pre-built components
- Check out [Examples](examples/README.md) for more complex designs
- Learn about [Verification](verification.md) features
- Understand [Safety Features](safety.md) for critical designs

## Common Commands

```bash
# Compile to SystemVerilog
skalp compile design.sk -o design.sv

# Run simulation
skalp sim testbench.sk

# Generate FPGA bitstream (iCE40)
skalp synth design.sk --target ice40 -o design.bit

# Check design for errors
skalp check design.sk

# Format code
skalp fmt design.sk

# Generate documentation
skalp doc design.sk -o docs/
```

## Troubleshooting

### Clock Domain Errors

If you see clock domain crossing errors, ensure proper synchronization:

```skalp
// Use explicit clock domain annotations
signal data<'clk1>: bit<8>;
signal synced<'clk2>: bit<8>;

// Use proper CDC techniques
sync #(.STAGES(2)) cdc {
    .in(data),
    .out(synced)
};
```

### Simulation Issues

- Ensure Verilator is installed: `verilator --version`
- Check that all files are in the correct path
- Use `--verbose` flag for detailed output

### VS Code Extension

- Verify the language server is built: `skalp-lsp --version`
- Check the server path in VS Code settings
- View the output panel for LSP logs

## Getting Help

- **Documentation**: This guide and reference materials
- **Examples**: Working code in the `examples/` directory
- **Community**: Discord and forums
- **Issues**: GitHub issue tracker