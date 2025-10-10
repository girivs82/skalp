# SKALP CLI Reference

Complete reference for all `skalp` command-line tools.

---

## Installation and Setup

```bash
# Clone and build
git clone https://github.com/skalp-lang/skalp.git
cd skalp
cargo build --release

# Add to PATH
export PATH=$PATH:$(pwd)/target/release

# Verify installation
skalp --version
```

---

## Global Options

All commands support these global options:

```bash
skalp [OPTIONS] <COMMAND>
```

**Options:**
- `-v, --verbose` - Increase verbosity (can be repeated: `-vv`, `-vvv`)
- `-h, --help` - Print help information
- `-V, --version` - Print version information

**Examples:**
```bash
skalp -v build              # Verbose output
skalp -vv build             # Very verbose (debug info)
skalp --version             # Show version
```

---

## Commands Overview

| Command | Purpose | Example |
|---------|---------|---------|
| `new` | Create new project | `skalp new counter` |
| `build` | Compile design | `skalp build -s main.sk` |
| `sim` | Simulate design | `skalp sim build/design.lir` |
| `synth` | Synthesize for FPGA | `skalp synth --device ice40` |
| `program` | Program FPGA | `skalp program bitstream.bin` |
| `fmt` | Format source files | `skalp fmt src/*.sk` |
| `test` | Run tests | `skalp test` |

---

## `skalp new` - Create New Project

Create a new SKALP project with scaffolding.

### Usage
```bash
skalp new <NAME>
```

### Arguments
- `<NAME>` - Project name (required)

### Options
- `-h, --help` - Print help
- `-V, --version` - Print version

### Examples

**Basic project:**
```bash
skalp new my_counter
cd my_counter
```

**Creates:**
```
my_counter/
├── Cargo.toml          # Rust project config (for testbenches)
├── README.md           # Project documentation
├── src/
│   └── main.sk         # Main SKALP source (counter example)
├── tests/              # Test directory
└── examples/           # Examples directory
```

**Generated `src/main.sk`:**
```skalp
entity Counter {
    in clk: clock
    in rst: reset
    out count: bit[8]
}

impl Counter {
    signal count_reg: bit[8] = 0

    on(clk.rise) {
        if (rst) {
            count_reg <= 0
        } else {
            count_reg <= count_reg + 1
        }
    }

    count = count_reg
}
```

---

## `skalp build` - Compile Design

Compile SKALP source to SystemVerilog, VHDL, or intermediate representations.

### Usage
```bash
skalp build [OPTIONS]
```

### Options
- `-s, --source <SOURCE>` - Source file (default: `src/main.sk`)
- `-t, --target <TARGET>` - Target output format (default: `sv`)
- `-o, --output <OUTPUT>` - Output directory (default: `build`)
- `-h, --help` - Print help
- `-V, --version` - Print version

### Target Formats

| Target | Description | Output File |
|--------|-------------|-------------|
| `sv` | SystemVerilog (default) | `design.sv` |
| `vhdl` | VHDL | `design.vhd` |
| `verilog` | Verilog-2001 | `design.v` |
| `mir` | Mid-level IR (for debugging) | `design.mir` |
| `sir` | Structural IR (for debugging) | `design.sir` |
| `lir` | Low-level IR (for simulation) | `design.lir` |

### Examples

**Build to SystemVerilog (default):**
```bash
skalp build
# Output: build/design.sv
```

**Build specific file:**
```bash
skalp build -s examples/fifo.sk
# Output: build/design.sv
```

**Build to VHDL:**
```bash
skalp build -s src/main.sk -t vhdl
# Output: build/design.vhd
```

**Build to custom output directory:**
```bash
skalp build -o output/
# Output: output/design.sv
```

**Build to LIR for simulation:**
```bash
skalp build -t lir -o build/
# Output: build/design.lir
```

**Verbose build (see compiler phases):**
```bash
skalp -v build
```

### Compiler Phases

When you run `skalp build`, you'll see:

```
Phase 1: HIR to MIR transformation
Phase 2: Clock Domain Crossing (CDC) analysis
Phase 3: Applying optimizations (level: None)
✅ Build complete!
📄 Output: "build/design.sv"
```

**Phase 1:** Parse and type-check
**Phase 2:** Analyze clock domain crossings
**Phase 3:** Optimize (currently disabled)

### Common Build Errors

**Error: File not found**
```
Error: Could not read source file 'src/main.sk'
```
→ Check file path with `-s` option

**Error: Syntax error**
```
Error: Unexpected token 'module' at line 5
```
→ Check SKALP syntax (not SystemVerilog!)

**Error: Type mismatch**
```
Error: Type mismatch: expected bit[8], found bit[16]
```
→ Check signal widths

---

## `skalp sim` - Simulate Design

Run GPU-accelerated simulation of your design.

### Usage
```bash
skalp sim [OPTIONS] <DESIGN>
```

### Arguments
- `<DESIGN>` - Design file to simulate (`.lir` file)

### Options
- `-d, --duration <DURATION>` - Simulation duration in cycles
- `-h, --help` - Print help
- `-V, --version` - Print version

### Examples

**Simulate for 100 cycles:**
```bash
# First build to LIR
skalp build -t lir

# Then simulate
skalp sim build/design.lir -d 100
```

**Quick simulation workflow:**
```bash
skalp build -t lir && skalp sim build/design.lir -d 1000
```

### Output

Simulation produces:
- Console output with signal values
- VCD waveform file (future feature)
- Coverage reports (future feature)

### GPU Acceleration

SKALP automatically uses GPU acceleration (Metal on macOS) if available.

**Check GPU support:**
```bash
skalp -v sim build/design.lir -d 10
# Will show: "Using GPU acceleration: YES" or "Using CPU fallback"
```

---

## `skalp synth` - Synthesize for FPGA

Synthesize design for FPGA targets (using open-source tools).

### Usage
```bash
skalp synth [OPTIONS] <SOURCE>
```

### Options
- `--device <DEVICE>` - Target FPGA device
- `-o, --output <OUTPUT>` - Output bitstream file
- `-h, --help` - Print help

### Supported Devices

| Device | Family | Tools Used |
|--------|--------|------------|
| `ice40-hx1k` | Lattice iCE40 | Yosys + nextpnr |
| `ice40-hx8k` | Lattice iCE40 | Yosys + nextpnr |
| `ice40-up5k` | Lattice iCE40 | Yosys + nextpnr |
| `ecp5-25k` | Lattice ECP5 | Yosys + nextpnr |
| `ecp5-45k` | Lattice ECP5 | Yosys + nextpnr |

### Examples

**Synthesize for iCE40:**
```bash
skalp synth src/main.sk --device ice40-hx8k
```

**Custom output:**
```bash
skalp synth src/main.sk --device ice40-hx8k -o counter.bin
```

### Requirements

For synthesis, you need:
- **Yosys** - Logic synthesis
- **nextpnr** - Place and route
- **icepack** / **ecppack** - Bitstream generation

**Install on macOS:**
```bash
brew install yosys nextpnr-ice40 icestorm
```

**Install on Linux:**
```bash
apt-get install yosys nextpnr-ice40 fpga-icestorm
```

---

## `skalp program` - Program FPGA

Program FPGA device with bitstream.

### Usage
```bash
skalp program <BITSTREAM>
```

### Arguments
- `<BITSTREAM>` - Bitstream file to program

### Examples

**Program iCE40:**
```bash
skalp program bitstream.bin
```

**Full workflow:**
```bash
# Synthesize
skalp synth src/main.sk --device ice40-hx8k -o counter.bin

# Program
skalp program counter.bin
```

### Requirements

- **iceprog** - For iCE40 devices
- **openocd** - For ECP5 devices
- FPGA board connected via USB

---

## `skalp fmt` - Format Source Files

Automatically format SKALP source files.

### Usage
```bash
skalp fmt [OPTIONS] [FILES]...
```

### Arguments
- `[FILES]...` - Files to format (default: all `.sk` files in `src/`)

### Options
- `--check` - Check formatting without modifying files
- `-h, --help` - Print help
- `-V, --version` - Print version

### Examples

**Format all files in project:**
```bash
skalp fmt
```

**Format specific files:**
```bash
skalp fmt src/main.sk examples/fifo.sk
```

**Check formatting (CI mode):**
```bash
skalp fmt --check
```

**Format with wildcard:**
```bash
skalp fmt src/*.sk
```

### Formatting Rules

SKALP formatter applies:
- Consistent indentation (4 spaces)
- Trailing whitespace removal
- Newline at end of file
- Consistent spacing around operators
- Aligned signal declarations

**Before:**
```skalp
entity Counter{in clk:clock in rst:reset out count:bit[8]}
impl Counter{signal c:bit[8]=0
on(clk.rise){if(rst){c<=0}else{c<=c+1}}count=c}
```

**After:**
```skalp
entity Counter {
    in clk: clock
    in rst: reset
    out count: bit[8]
}

impl Counter {
    signal c: bit[8] = 0

    on(clk.rise) {
        if (rst) {
            c <= 0
        } else {
            c <= c + 1
        }
    }

    count = c
}
```

---

## `skalp test` - Run Tests

Run project tests (future feature).

### Usage
```bash
skalp test [OPTIONS]
```

### Options
- `--all` - Run all tests
- `--test <NAME>` - Run specific test
- `-h, --help` - Print help

### Examples

**Run all tests:**
```bash
skalp test
```

**Run specific test:**
```bash
skalp test --test counter_test
```

---

## Common Workflows

### Development Workflow
```bash
# 1. Create project
skalp new my_design
cd my_design

# 2. Edit src/main.sk
vim src/main.sk

# 3. Build and check
skalp build

# 4. Format code
skalp fmt

# 5. Simulate
skalp build -t lir
skalp sim build/design.lir -d 100
```

### FPGA Workflow
```bash
# 1. Build to SystemVerilog
skalp build

# 2. Synthesize
skalp synth src/main.sk --device ice40-hx8k -o bitstream.bin

# 3. Program device
skalp program bitstream.bin
```

### Multi-Target Build
```bash
# SystemVerilog for Vivado/Quartus
skalp build -t sv -o build/sv/

# VHDL for legacy tools
skalp build -t vhdl -o build/vhdl/

# LIR for simulation
skalp build -t lir -o build/sim/
```

---

## Environment Variables

SKALP respects these environment variables:

| Variable | Purpose | Default |
|----------|---------|---------|
| `SKALP_TARGET` | Default build target | `sv` |
| `SKALP_OUTPUT` | Default output directory | `build` |
| `RUST_LOG` | Logging level | (none) |

**Example:**
```bash
export SKALP_TARGET=vhdl
export RUST_LOG=debug
skalp build  # Builds to VHDL with debug logging
```

---

## Troubleshooting

### Command not found
```
bash: skalp: command not found
```
→ Add to PATH: `export PATH=$PATH:/path/to/skalp/target/release`

### Build fails with "file not found"
```
Error: Could not read source file
```
→ Check path: `skalp build -s src/main.sk`

### Simulation fails
```
Error: Invalid design file
```
→ Build to LIR first: `skalp build -t lir`

### Permission denied (macOS)
```
Error: Permission denied
```
→ Allow in System Preferences → Security & Privacy

---

## See Also

- [Quick Start Guide](../quick-start.md) - Getting started tutorial
- [Syntax Reference](syntax.md) - Language syntax
- [Examples](../examples/) - Complete working examples
- [Troubleshooting](../guides/troubleshooting.md) - Common errors and solutions

---

**Quick Reference Card:**

```bash
# Create, build, simulate
skalp new project && cd project
skalp build
skalp build -t lir && skalp sim build/design.lir -d 100

# Format and check
skalp fmt
skalp fmt --check

# Synthesize and program
skalp synth src/main.sk --device ice40-hx8k
skalp program bitstream.bin

# Get help
skalp --help
skalp <command> --help
```
