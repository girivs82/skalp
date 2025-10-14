# Build Guide - Graphics Pipeline Accelerator

Step-by-step instructions for building and using this complex example project.

## Prerequisites

1. **SKALP Compiler** - Build from source
   ```bash
   cd /path/to/skalp
   cargo build --release
   export PATH=$PATH:$(pwd)/target/release
   ```

2. **Optional Tools** (for full FPGA flow):
   - Yosys (synthesis)
   - nextpnr (place & route)
   - iverilog (Verilog simulation)

## Project Structure

```
complex_project/
├── README.md              # Project overview
├── TUTORIAL.md            # Learning guide
├── BUILD_GUIDE.md         # This file
├── skalp.toml             # Project manifest
└── src/
    ├── main.sk            # Top-level (850 lines)
    ├── types.sk           # Data structures (200 lines)
    ├── async_fifo.sk      # Generic FIFO (250 lines)
    └── geometry_processor.sk  # Pipeline (280 lines)

Total: ~1580 lines of SKALP code
```

## Build Commands

### 1. SystemVerilog Generation (Default)

Generate synthesizable SystemVerilog:

```bash
cd examples/complex_project
skalp build -s src/main.sk -o build/
```

**Output**: `build/design.sv` (~3000-4000 lines of generated SystemVerilog)

**What to expect**:
- All parametric types instantiated
- Clock domain crossing logic expanded
- Pipeline stages unrolled
- Memory arrays synthesized

### 2. MIR Generation (for Debugging)

Generate Mid-level IR to inspect compiler optimizations:

```bash
skalp build -s src/main.sk -t mir -o build/
```

**Output**: `build/design.mir` (JSON format)

**Inspect**:
```bash
cat build/design.mir | jq '.modules[0].name'
```

### 3. Verbose Build

See all compiler phases:

```bash
skalp -vv build -s src/main.sk -o build/
```

**Phases shown**:
1. Parsing and HIR generation
2. Type checking with generics
3. MIR lowering
4. CDC analysis (multi-clock validation)
5. Optimization passes
6. Code generation

## Expected Build Output

### Success Case

```
Parsing SKALP source...
Building HIR...
Lowering to MIR...
  - Instantiating AsyncFifo<Vertex, 16>
  - Instantiating AsyncFifo<TransformedVertex, 64>
  - Instantiating GeometryProcessor<4>
Analyzing clock domains...
  - Found 3 clock domains: sys_clk, geom_clk, pixel_clk
  - CDC crossings: 2 (all properly synchronized)
Generating SystemVerilog...
✅ Build complete!
📄 Output: build/design.sv
```

### Common Issues

#### Issue 1: Module Not Found

```
Error: Cannot find module 'types'
```

**Solution**: Ensure you're running from `complex_project/` directory:
```bash
pwd  # Should show: .../examples/complex_project
skalp build -s src/main.sk
```

#### Issue 2: Syntax Errors

If you modified the code and see parsing errors, check:
- Matching braces `{ }`
- Semicolons after statements
- Signal declarations before use

#### Issue 3: Type Errors

```
Error: Type mismatch in FIFO instantiation
```

**Solution**: Ensure consistent types:
```skalp
// ❌ Wrong
let fifo = AsyncFifo<Vertex, 16> { wr_data: some_transformed_vertex }

// ✅ Correct
let fifo = AsyncFifo<Vertex, 16> { wr_data: some_vertex }
```

## Verification Steps

### 1. Check Generated SystemVerilog

```bash
wc -l build/design.sv
# Should show ~3000-4000 lines
```

Look for:
```bash
grep "module " build/design.sv
# Should show instantiated modules
```

### 2. Lint with Verilator (Optional)

```bash
verilator --lint-only build/design.sv
```

### 3. Syntax Check with Icarus Verilog

```bash
iverilog -tnull build/design.sv
```

## Understanding Generated Code

### Parametric Types Expanded

**Source**:
```skalp
let vertex_fifo = AsyncFifo<Vertex, 16> { ... }
```

**Generated** (simplified):
```systemverilog
module AsyncFifo_Vertex_16 (
    input clk_wr,
    input [VERTEX_WIDTH-1:0] wr_data,
    // ...
    reg [VERTEX_WIDTH-1:0] mem [0:15];  // 16 deep
);
```

### Clock Domains Separated

**Source**:
```skalp
on(sys_clk.rise) { /* ... */ }
on(geom_clk.rise) { /* ... */ }
```

**Generated**:
```systemverilog
always @(posedge sys_clk) begin
    // sys_clk domain logic
end

always @(posedge geom_clk) begin
    // geom_clk domain logic
end
```

### Pattern Matching to Case Statements

**Source**:
```skalp
match state {
    Idle => { state <= Active }
    Active => { state <= Done }
}
```

**Generated**:
```systemverilog
case (state)
    STATE_IDLE: state <= STATE_ACTIVE;
    STATE_ACTIVE: state <= STATE_DONE;
endcase
```

## Module-by-Module Build

You can build individual modules for testing:

### Build Just the FIFO

```bash
skalp build -s src/async_fifo.sk -o build/fifo/
```

This tests the FIFO module in isolation.

### Build Just the Geometry Processor

```bash
skalp build -s src/geometry_processor.sk -o build/geom/
```

Note: This may fail due to missing dependencies (types module). Use full build instead.

## Build Configurations

### Debug Build (No Optimization)

```bash
skalp build -s src/main.sk -o build/debug/ --no-optimize
```

Benefits:
- Easier to understand generated code
- Faster compilation
- Better error messages

### Release Build (Full Optimization)

```bash
skalp build -s src/main.sk -o build/release/ --optimize
```

Benefits:
- Smaller area
- Higher clock frequency
- Better timing

## Size Estimates

### FPGA Resource Usage (Estimated)

For iCE40-HX8K:

| Component | LUTs | FFs | BRAMs | Notes |
|-----------|------|-----|-------|-------|
| Geometry Processor | 2500 | 1800 | 4 | 4-stage pipeline |
| Async FIFOs (2x) | 800 | 600 | 2 | 16-deep + 64-deep |
| AXI Interface | 400 | 300 | 0 | Slave only |
| Control Logic | 600 | 400 | 0 | State machines |
| **Total** | **4300** | **3100** | **6** | ~55% of HX8K |

### Code Size

| Metric | Count | Notes |
|--------|-------|-------|
| Source Lines | 1580 | Hand-written SKALP |
| Generated SV Lines | ~3500 | Compiler output |
| Modules | 8 | Flattened hierarchy |
| Signals | ~200 | Internal + ports |

## Performance Characteristics

### Latency

| Operation | Cycles | Time @ 200MHz |
|-----------|--------|---------------|
| Vertex Transform | 4 | 20 ns |
| Full Pipeline | 12 | 60 ns |
| CDC (FIFO) | 2-3 | 10-15 ns |

### Throughput

| Component | Rate | Notes |
|-----------|------|-------|
| Geometry Processor | 1 vertex/cycle | When pipeline full |
| FIFO Bandwidth | 1 entry/cycle | Both read and write |
| System Bus | 32 bits/cycle | AXI4-Lite |

## Synthesis Notes

### Clock Constraints

For FPGA synthesis, add these constraints:

```tcl
# System clock: 100 MHz
create_clock -period 10.0 [get_ports sys_clk]

# Geometry clock: 200 MHz
create_clock -period 5.0 [get_ports geom_clk]

# Pixel clock: 25 MHz
create_clock -period 40.0 [get_ports pixel_clk]

# Asynchronous clock groups
set_clock_groups -asynchronous \
    -group [get_clocks sys_clk] \
    -group [get_clocks geom_clk] \
    -group [get_clocks pixel_clk]

# False paths on CDC synchronizers
set_false_path -from [get_pins *_sync1*/D] -to [get_pins *_sync2*/D]
```

### Area Optimization

To reduce area:

1. **Reduce FIFO depths** in `main.sk`:
   ```skalp
   let vertex_fifo = AsyncFifo<Vertex, 8> { ... }  // Was 16
   ```

2. **Reduce pipeline stages**:
   ```skalp
   let geometry = GeometryProcessor<2> { ... }  // Was 4
   ```

3. **Remove unused features**:
   Comment out texture sampling, advanced lighting, etc.

### Timing Optimization

To improve clock frequency:

1. **Add pipeline stages**:
   ```skalp
   let geometry = GeometryProcessor<8> { ... }  // More stages = lower logic depth
   ```

2. **Register FIFO outputs**:
   Add extra pipeline registers after FIFO reads

3. **Simplify math**:
   Use lookup tables for complex functions

## Troubleshooting

### Build Hangs

If build seems stuck:
- Check for infinite loops in const evaluation
- Verify all modules have finite parameter values
- Use `-vv` flag to see where it's stuck

### Out of Memory

For large designs:
- Reduce FIFO depths
- Lower pipeline stage count
- Build in release mode (uses less memory)

### CDC Warnings

If you see CDC warnings:
```
Warning: Possible metastability in signal 'foo'
```

Ensure all clock crossings use:
1. Async FIFOs, OR
2. 2-flip-flop synchronizers, OR
3. Handshake protocols

## Next Steps

1. ✅ **Build successfully** - You're here!
2. 📝 **Read TUTORIAL.md** - Understand the code
3. 🔧 **Modify parameters** - Change FIFO depths, pipeline stages
4. 🧪 **Add tests** - Create testbenches
5. 🎯 **Synthesize** - Target real FPGA

## Questions?

See:
- [TUTORIAL.md](TUTORIAL.md) - Detailed walkthrough
- [README.md](README.md) - Project overview
- [SKALP Docs](../../docs/user/quick-start.md) - Language reference
