# Quick Start: SKALP in 5 Minutes

Get SKALP running and compile your first hardware design in under 5 minutes.

---

## Step 1: Install SKALP (1 minute)

```bash
# Clone the repository
git clone https://github.com/skalp-lang/skalp.git
cd skalp

# Build the compiler (takes ~2 minutes first time)
cargo build --release

# Add to your PATH (or use full path)
export PATH=$PATH:$(pwd)/target/release
```

**Verify installation:**
```bash
skalp --version
```

Expected output:
```
skalp 0.1.0
```

---

## Step 2: Create Your First Project (30 seconds)

```bash
# Create a new project
skalp new my_counter
cd my_counter

# See what was generated
ls -la
```

You'll see:
```
my_counter/
├── Cargo.toml          # Rust project config (for tests)
├── README.md           # Project documentation
├── src/
│   └── main.sk         # Your SKALP design (a counter!)
├── tests/              # Test directory
└── examples/           # Example directory
```

---

## Step 3: Look at the Code (1 minute)

Open `src/main.sk`:

```skalp
// Main SKALP design file

entity Counter {
    in clk: clock       // Clock input
    in rst: reset       // Reset input
    out count: bit[8]   // 8-bit counter output
}

impl Counter {
    signal count_reg: bit[8] = 0    // Internal register

    on(clk.rise) {                  // On rising clock edge
        if (rst) {
            count_reg <= 0          // Reset to 0
        } else {
            count_reg <= count_reg + 1  // Increment
        }
    }

    count = count_reg   // Connect output
}
```

**What's happening:**
- `entity Counter` - Declares a hardware module with ports
- `impl Counter` - Implements the counter logic
- `signal count_reg` - Internal 8-bit register
- `on(clk.rise)` - Sequential logic triggered on clock rising edge
- `count_reg <= ...` - Non-blocking assignment (sequential)
- `count = count_reg` - Combinational assignment

---

## Step 4: Compile to SystemVerilog (10 seconds)

```bash
skalp build
```

**Output:**
```
Phase 1: HIR to MIR transformation
Phase 2: Clock Domain Crossing (CDC) analysis
Phase 3: Applying optimizations (level: None)
✅ Build complete!
📄 Output: "build/design.sv"
```

**Look at the generated SystemVerilog:**
```bash
cat build/design.sv
```

You'll see clean, synthesizable SystemVerilog:
```systemverilog
module Counter (
    input clk,
    input rst,
    output [7:0] count
);
    reg [7:0] count_reg;

    always_ff @(posedge clk) begin
        if (rst) begin
            count_reg <= 8'd0;
        end else begin
            count_reg <= (count_reg + 8'd1);
        end
    end

    assign count = count_reg;
endmodule
```

---

## Step 5: Understand What You Built (1 minute)

**SKALP provides:**
- ✅ **Type safety** - `bit[8]` prevents width mismatches
- ✅ **Clean syntax** - `on(clk.rise)` instead of `always_ff @(posedge clk)`
- ✅ **CDC analysis** - Automatically checks clock domain crossings
- ✅ **Readable output** - Generated SystemVerilog is clean

**Compare to hand-written SystemVerilog:**

| SKALP (14 lines) | SystemVerilog (16 lines) |
|------------------|--------------------------|
| `in clk: clock` | `input wire clk` |
| `signal count_reg: bit[8] = 0` | `reg [7:0] count_reg;` + init in always block |
| `on(clk.rise)` | `always_ff @(posedge clk)` |
| `count = count_reg` | `assign count = count_reg;` |

SKALP is **more concise** and **type-safe**.

---

## Next Steps: Build Something Real

### Option 1: Modify the Counter
Try these modifications to `src/main.sk`:

**Add a load input:**
```skalp
entity Counter {
    in clk: clock
    in rst: reset
    in load: bit           // Add load input
    in load_value: bit[8]  // Value to load
    out count: bit[8]
}

impl Counter {
    signal count_reg: bit[8] = 0

    on(clk.rise) {
        if (rst) {
            count_reg <= 0
        } else if (load) {
            count_reg <= load_value  // Load new value
        } else {
            count_reg <= count_reg + 1
        }
    }

    count = count_reg
}
```

Rebuild:
```bash
skalp build
```

---

### Option 2: Build a Different Design

**Create an adder (`src/adder.sk`):**
```skalp
entity Adder {
    in a: bit[8]
    in b: bit[8]
    out sum: bit[8]
    out carry: bit
}

impl Adder {
    let result: bit[9] = a + b
    sum = result[7:0]
    carry = result[8]
}
```

Build it:
```bash
skalp build -s src/adder.sk -o build_adder
cat build_adder/design.sv
```

---

### Option 3: Try a Real Example

SKALP comes with real-world examples:

```bash
cd ../skalp/examples

# Build a FIFO
skalp build -s fifo.sk -o /tmp/fifo_build

# Build an ALU
skalp build -s alu.sk -o /tmp/alu_build

# Build a counter
skalp build -s counter.sk -o /tmp/counter_build
```

Explore more examples:
```bash
ls examples/real_world/
```

You'll find:
- FIFO (circular buffer)
- UART transmitter
- SPI master
- I2C master
- AXI4-Lite interface
- Memory arbiter
- Register file
- ALU

---

## What You've Learned

In 5 minutes, you:
✅ Installed SKALP
✅ Created a project
✅ Compiled SKALP to SystemVerilog
✅ Understood basic syntax
✅ Modified a design

---

## Where to Go Next

### Learn More SKALP:
- 📘 [**Tutorial**](../tutorial/01-first-design.md) - Step-by-step guide to all language features
- 📖 [**Reference Manual**](../reference/syntax.md) - Complete syntax reference
- 👨‍🍳 [**Cookbook**](../cookbook/README.md) - Design patterns and recipes
- 🧪 [**Testing Guide**](../guides/testbench.md) - How to test your designs

### Coming from SystemVerilog?
- 🔄 [**Migration Guide**](../migration/from-systemverilog.md) - Translate SV to SKALP

### Ready for Real Designs?
- 💡 [**Examples**](../examples/README.md) - Complete working designs
- 🎯 [**FIFO Tutorial**](../tutorial/03-sequential.md#fifo) - Build a production FIFO

### Want to Understand Why SKALP?
- 🎯 [**Why SKALP?**](../../comparison/why-skalp.md) - Comparison with SystemVerilog
- ⚖️ [**SKALP vs Veryl**](../../comparison/skalp-vs-veryl.md) - Feature comparison

---

## Common Questions

**Q: Can I use this with Vivado/Quartus?**
A: Yes! SKALP generates standard SystemVerilog that works with all synthesis tools.

**Q: How do I test my design?**
A: SKALP has a built-in testbench API. See the [Testing Guide](../guides/testbench.md).

**Q: Can I use existing SystemVerilog IP?**
A: Yes, you can instantiate SV modules in SKALP (future feature).

**Q: Is SKALP production-ready?**
A: SKALP is in active development. The core language and compiler work, but some features are still being added.

---

## Quick Command Reference

```bash
# Create new project
skalp new <name>

# Build to SystemVerilog (default)
skalp build

# Build specific file
skalp build -s path/to/file.sk

# Build to different output
skalp build -o output_dir

# Build to other formats
skalp build -t vhdl    # VHDL output
skalp build -t lir     # Low-level IR
skalp build -t mir     # Mid-level IR

# Get help
skalp --help
skalp build --help
```

---

**🎉 Congratulations! You've compiled your first SKALP design.**

**Next:** [Start the tutorial →](../tutorial/01-first-design.md)
