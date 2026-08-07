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
| `build` | Compile design | `skalp build design.sk` |
| `sim` | Simulate design | `skalp sim design.sk -d 100` |
| `synth` | Synthesize for FPGA | `skalp synth design.sk --device ice40-hx8k` |
| `program` | Program FPGA | `skalp program bitstream.bin` |
| `fmt` | Format source files | `skalp fmt src/*.sk` |
| `test` | Run tests | `skalp test` |
| `ec` | Formal equivalence check (RTL vs gates) | `skalp ec design.sk` |

Run `skalp --help` for the full command list (including `pnr`, `analyze`,
`safety`, package management, and more) and `skalp <command> --help` for every
option of a command.

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
├── skalp.toml          # SKALP manifest ([package], [dependencies], [build])
├── Cargo.toml          # Rust project config (for testbenches)
├── README.md           # Project documentation
├── src/
│   └── main.sk         # Main SKALP source (counter example)
├── tests/              # Test directory
└── examples/           # Examples directory
```

**Generated `skalp.toml`:**
```toml
[package]
name = "my_counter"
version = "0.1.0"

[dependencies]

# Build defaults used by `skalp build` when no source argument is given.
[build]
main = "src/main.sk"
out_dir = "build"
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
            count_reg = 0
        } else {
            count_reg = count_reg + 1
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
skalp build [OPTIONS] [SOURCE]
```

The positional form `skalp build design.sk` is the usual way to build a single
file. With no source argument, `skalp build` reads the `[build]` section of
`skalp.toml` (falling back to `src/main.sk`).

### Common Options
- `[SOURCE]` - Source file, positional form: `skalp build design.sk`
- `-s, --source <SOURCE>` - Source file (default: `src/main.sk` / `skalp.toml` `[build] main`)
- `-t, --target <TARGET>` - Target output format (default: `sv`)
- `-o, --output <OUTPUT>` - Output directory (default: `build` / `skalp.toml` `[build] out_dir`)
- `--emit <STAGE>` - Dump an intermediate representation and exit (`hir`, `mir`, `lir`)
- `--optimize <PRESET>` - Synthesis optimization preset (default: `compress2`)
- `--no-synth-opt` - Disable synthesis optimization entirely
- `-h, --help` - Print help (see `skalp build --help` for the full list,
  including safety analysis, ML-guided optimization, and library options)

### Target Formats

| Target | Description | Output File |
|--------|-------------|-------------|
| `sv` | SystemVerilog (default) | `design.sv` |
| `vhdl` | VHDL | `<entity>.vhd` |
| `mir` | Mid-level IR (for debugging) | `design.mir` |
| `gates` | Gate-level netlist | `design_gates.v` + `design_gates.json` |

(`verilog` and `lir` targets are currently disabled — use `sv` and `mir`.)

When the design contains top-level `power_domain` declarations, the `sv`
target additionally writes IEEE 1801 power intent to `design.upf` next to
`design.sv`, reported as:

```
📄 Power intent: "build/design.upf"
```

See the [Power Intent Guide](../guides/power-intent.md). Designs without
`power_domain` declarations produce no UPF file.

### Examples

**Build a file (positional form):**
```bash
skalp build design.sk
# Output: build/design.sv
```

**Build using the project manifest:**
```bash
skalp build
# Uses skalp.toml [build] main / out_dir
```

**Build to VHDL:**
```bash
skalp build src/main.sk -t vhdl
# Output: build/<entity>.vhd
```

**Build to custom output directory:**
```bash
skalp build design.sk -o output/
# Output: output/design.sv
```

**Verbose build (see compiler phases):**
```bash
skalp -v build design.sk
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

Simulate your design directly from source.

### Usage
```bash
skalp sim [OPTIONS] <DESIGN>
```

### Arguments
- `<DESIGN>` - Design file to simulate (a `.sk` source file)

### Options
- `-d, --duration <DURATION>` - Simulation duration (cycles)
- `-o, --output <OUTPUT>` - Output waveform file (`.skw.gz`); defaults to `<name>.skw.gz` next to the source
- `--gate-level` - Gate-level simulation (HIR→MIR→LIR→SIR) instead of behavioral
- `--gpu` - Use GPU acceleration (Metal on macOS)
- `--ncl` - Simulate as NCL (Null Convention Logic) async circuit
- `-h, --help` - Print help

### Examples

**Simulate for 100 cycles:**
```bash
skalp sim design.sk -d 100
```

**Simulate and write a waveform file:**
```bash
skalp sim design.sk -d 1000 -o waves.skw.gz
```

**Gate-level simulation:**
```bash
skalp sim design.sk --gate-level -d 100
```

### Output

Simulation produces:
- Console output with design statistics and signal values
- A compressed waveform file (`.skw.gz`)

### GPU Acceleration

GPU acceleration (Metal on macOS) is opt-in with the `--gpu` flag:

```bash
skalp sim design.sk -d 1000 --gpu
```

---

## `skalp synth` - Synthesize for FPGA

Synthesize design for FPGA targets (using open-source tools).

### Usage
```bash
skalp synth [OPTIONS] --device <DEVICE> <SOURCE>
```

### Options
- `-d, --device <DEVICE>` - Target FPGA device (required)
- `-f, --full-flow` - Full flow (place, route, bitstream)
- `-o, --output <OUTPUT>` - Output directory (default: `build`)
- `--optimize <PRESET>` - Synthesis optimization preset
- `--pnr-preset <PRESET>` - P&R quality preset (`fast`, `default`, `high_quality`)
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

**Full flow (place, route, bitstream) to a custom directory:**
```bash
skalp synth src/main.sk --device ice40-hx8k --full-flow -o out/
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
skalp program [OPTIONS] [BITSTREAM]
```

### Arguments
- `[BITSTREAM]` - Bitstream file to program (`.bin`)

### Options
- `-b, --board <BOARD>` - Target board (`icebreaker`, `icebreaker-bitsy`, `hx8k-breakout`, `upduino3`, `auto`; default `auto`)
- `--reset-only` - Reset the FPGA without programming
- `--list` - List detected boards and exit

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
on(clk.rise){if(rst){c=0}else{c=c+1}}count=c}
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
            c = 0
        } else {
            c = c + 1
        }
    }

    count = c
}
```

---

## `skalp test` - Run Tests

Run project tests.

### Usage
```bash
skalp test [FILTER]
```

### Arguments
- `[FILTER]` - Optional test name filter

### Examples

**Run all tests:**
```bash
skalp test
```

**Run tests matching a filter:**
```bash
skalp test counter
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
skalp sim src/main.sk -d 100
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
skalp build design.sk -t sv -o build/sv/

# VHDL for legacy tools
skalp build design.sk -t vhdl -o build/vhdl/

# Gate-level netlist
skalp build design.sk -t gates -o build/gates/
```

---

## Environment Variables

SKALP respects these environment variables:

| Variable | Purpose | Default |
|----------|---------|---------|
| `SKALP_STDLIB_PATH` | Location of the SKALP standard library | (bundled) |
| `RUST_LOG` | Logging level | (none) |

**Example:**
```bash
export SKALP_STDLIB_PATH=/path/to/skalp/crates/skalp-stdlib
export RUST_LOG=debug
skalp build design.sk
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
→ `skalp sim` takes a `.sk` source file: `skalp sim design.sk -d 100`

### Permission denied (macOS)
```
Error: Permission denied
```
→ Allow in System Preferences → Security & Privacy

---

## See Also

- [Quick Start Guide](../quick-start.md) - Getting started tutorial
- [Syntax Reference](syntax.md) - Language syntax
- [Examples](../../../examples/) - Complete working examples

---

**Quick Reference Card:**

```bash
# Create, build, simulate
skalp new project && cd project
skalp build
skalp sim src/main.sk -d 100

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
