# Skalp User Feedback
## From: Claude (LLM) as Karythra User
## Date: 2025-12-09 (Updated)
## Project: Karythra 256-bit Content-Addressed Processor

---

## Executive Summary

Skalp has matured significantly. It sits in a **sweet spot between RTL and traditional HLS** - you think in hardware terms but with modern language ergonomics. After the December 2025 feature additions (generate blocks, pipeline annotations, named generics), it's now **feature-complete for most accelerator designs**.

**Overall Rating: 9/10** - Production-ready for accelerator development, with only niche features missing.

---

## Core Strengths

### 1. Type System and Bit-Width Safety
```skalp
let a: bit[32] = ...;
let b: bit[8] = a[7:0];  // Explicit slice - no implicit truncation bugs
let c: bit[256] = {a, b, ...};  // Explicit concatenation
```
**Impact**: Eliminates the #1 source of RTL bugs - width mismatches. Catches at compile time what Verilog finds at simulation.

### 2. First-Class FP32 Support
```skalp
let discriminant = b * b - 4.0fp32 * a * c;
let sqrt_d = sqrt(discriminant);
let x1 = (-b + sqrt_d) / (2.0fp32 * a);
```
**Impact**: FP arithmetic that would require manual IEEE 754 manipulation in Verilog is natural in Skalp. Used extensively in Karythra's L4-L5 graphics operations.

### 3. Match Expressions for Clean Mux Trees
```skalp
return match opcode {
    0 => a + b,
    1 => a - b,
    2 => a * b,
    _ => 0
}
```
**Impact**: Compiles to clean mux trees without latch inference bugs. Intent declarations can optimize to parallel (one-hot) or priority muxes.

### 4. Generate Blocks for Metaprogramming
```skalp
// Compile-time loop unrolling
generate for i in 0..32 {
    result[i] <= input[31-i];  // Bit reversal
}

// Conditional generation
generate if ENABLE_DEBUG {
    debug_out <= internal_state;
}

// Parameterized generation
generate match DATA_WIDTH {
    8 => { ... }
    16 => { ... }
    32 => { ... }
}
```
**Impact**: Full Verilog generate capability with cleaner syntax. Essential for parameterized designs.

### 5. Pipeline Annotations for Timing Control
```skalp
#[pipeline(stages=4)]
pub fn quadratic_solve(a: bit[32], b: bit[32], c: bit[32]) -> (bit, bit[32], bit[32]) {
    // Compiler inserts pipeline registers at optimal cut points
    ...
}

#[pipeline(stages=3, target_freq=100_000_000)]  // With timing hints
pub fn bezier_eval(...) -> bit[32] { ... }
```
**Impact**: Eliminates manual pipeline register insertion. Backend computes logic levels and places FlipFlops automatically.

### 6. Named Generic Arguments for Clarity
```skalp
// Positional (existing)
let fifo = Fifo<16, 32> { ... }

// Named (clearer for complex entities)
let fifo = Fifo<DEPTH: 16, WIDTH: 32> { ... }
```
**Impact**: Self-documenting entity instantiation. Critical for designs with many generic parameters.

**Note**: Named generics are supported for both **entity instantiation and function calls**:
```skalp
// Entity instantiation
let adder = GenericAdder<W: 32> { a: x, b: y }

// Function calls - named generics also work!
let result = generic_add::<W: 32>(x, y)
let result2 = exec_l0_l1::<W: 32>(opcode, data1, data2)
```

### 7. GPU-Accelerated Simulation
The Metal backend provides fast simulation for combinational logic. 500+ cycles of Karythra's CLE runs in ~50-100 seconds - fast enough for TDD iteration.

---

## What Works Well But Has Caveats

### 1. Mutable Variables in Functions
```skalp
pub fn parity32(x: bit[32]) -> bit {
    let mut p = x;
    p = p ^ (p >> 16);
    p = p ^ (p >> 8);
    ...
    return p[0:0]
}
```
**Status**: Works after Bug #118 fix. Complex nested mutations may still have edge cases.

### 2. 256-bit Wide Datapaths
```skalp
pub fn exec_l4_l5(opcode: bit[6], data1: bit[256], data2: bit[256]) -> bit[256]
```
**Status**: Fully supported. Backend decomposes to 4x64-bit for Metal. Simulation works, but may be slower than narrower designs.

### 3. Complex Control Flow
Nested if/match with multiple returns works, but:
- Very deep nesting may hit edge cases
- Sometimes requires restructuring code

---

## Remaining Pain Points / Future Enhancements

### 1. ~~No RAM/Memory Inference~~ ✅ **DONE** (Dec 2025)
```skalp
// NOW SUPPORTED:
#[memory(depth=1024, style=block)]
signal mem: bit[64],  // Infers BRAM

#[memory(depth=16, style=distributed)]
signal lutram: bit[16],  // Infers distributed RAM

#[memory(depth=4096, style=ultra)]
signal ultraram: bit[72],  // Infers UltraRAM

#[memory(depth=8, style=register)]
signal regfile: bit[16],  // Register file
```
**Status**: Implemented. Parser, HIR, and MIR fully support memory attributes.

### 2. ~~No Formal Verification Integration~~ ✅ **DONE** (Dec 2025)
```skalp
// NOW SUPPORTED:
assert!(result < MAX_VALUE, "Overflow check");
assume!(input != 0, "Non-zero input");
cover!(state == IDLE, "Can reach idle");
```
**Status**: Implemented. Parser, HIR, MIR, and SystemVerilog codegen fully support formal verification macros. Generates standard SVA (SystemVerilog Assertions) compatible with formal tools like SymbiYosys, Jasper, etc.

### 3. Debug/Trace Infrastructure - Partial ✅ (Dec 2025)
```skalp
// NOW SUPPORTED:
#[trace]
signal debug_bus: bit[32];  // Auto-exported to VCD trace

#[trace(radix = hex)]
signal data_bus: bit[64];  // Display as hex in waveform viewer

// FUTURE (string parsing TBD):
#[trace(group = "control")]
signal fsm_state: bit[4];  // Grouped signals in VCD

#[breakpoint(condition = "error_flag == 1")]  // Not yet implemented
```
**Status**: Basic `#[trace]` attribute implemented. Signals marked with `#[trace]` are propagated through HIR/MIR and the VCD export system supports grouped signals and radix hints. String literal parsing in attributes (for group/display_name) requires lexer enhancement.

**Impact**: Low-Medium. Debug workflow improved - can now mark signals for automatic VCD export.

### 4. No Clock Domain Crossing Helpers - Partial ✅ (Dec 2025)
```skalp
// WANTED:
#[cdc(sync_stages=2)]
signal async_input: bit[8];  // Auto-inserts synchronizers
```

**NOW SUPPORTED:**
- `#[cdc]` - Basic CDC attribute, defaults to 2-stage synchronizer
- `#[cdc(sync_stages = 3)]` - Custom number of synchronizer stages
- `#[cdc(cdc_type = gray)]` - CDC type: `gray`, `pulse`, `handshake`, `async_fifo`, `two_ff`
- `#[cdc(sync_stages = 2, cdc_type = gray)]` - Combined parameters
- `#[cdc(from = clk_a, to = clk_b)]` - Domain references with from/to keywords
- `#[cdc(source = 'fast, destination = 'slow)]` - Lifetime-style domain references (integrates with type system)

**NOT YET IMPLEMENTED:**
- Automatic synchronizer logic generation in SystemVerilog codegen

**Impact**: Medium. CDC attribute parsing complete; codegen for synchronizer instantiation pending.

### 5. No Power Intent Support
```skalp
// WANTED:
#[power_domain("always_on")]
entity CriticalReg { ... }

#[retention]
signal saved_state: bit[32];
```
**Impact**: Low-Medium. Not critical for FPGA, but needed for ASIC power management.

### 6. No Vendor IP Integration
```skalp
// WANTED:
#[xilinx_ip("xpm_fifo_sync")]
entity VendorFifo { ... }
```
**Impact**: Medium. Currently must wrap vendor IP in Verilog and instantiate as black box.

---

## Comparison to Alternatives (Updated Dec 2025)

| Feature | Skalp | SystemVerilog | Chisel | Clash | Bluespec |
|---------|-------|---------------|--------|-------|----------|
| Type Safety | ★★★★★ | ★★☆☆☆ | ★★★★☆ | ★★★★★ | ★★★★☆ |
| Generate/Meta | ★★★★★ | ★★★★☆ | ★★★★★ | ★★★★☆ | ★★★★☆ |
| Pipeline Control | ★★★★★ | ★★☆☆☆ | ★★★☆☆ | ★★★☆☆ | ★★★★★ |
| FP32 Native | ★★★★★ | ☆☆☆☆☆ | ★★☆☆☆ | ★★★★☆ | ★★☆☆☆ |
| Simulation Speed | ★★★★☆ | ★★★★★ | ★★★☆☆ | ★★★☆☆ | ★★★☆☆ |
| Learning Curve | ★★★★☆ | ★★★☆☆ | ★★★☆☆ | ★★☆☆☆ | ★★☆☆☆ |
| Memory Inference | ★★★★☆ | ★★★★★ | ★★★★☆ | ★★★☆☆ | ★★★★☆ |
| Formal Support | ★★★★☆ | ★★★★☆ | ★★☆☆☆ | ★★★☆☆ | ★★★★☆ |

**Skalp's Sweet Spot**: Accelerators with complex arithmetic (FP32, wide datapaths), algorithmic complexity (match expressions, control flow), and timing requirements (pipeline annotations).

---

## Karythra Experience Summary

### Features Used Successfully:
1. **Pipeline Annotations**: Added `#[pipeline(stages=4)]` to `quadratic_solve`, `bezier_eval`, `ray_aabb_intersect`
2. **FP32 Operations**: Quadratic solver, Bezier curves, ray intersection - all natural to express
3. **256-bit Datapaths**: Full support for content-addressed processor's wide data model
4. **Bitops Library**: `clz32`, `popcount32`, `parity32` - all synthesizable
5. **Match-based Dispatch**: CLE function unit opcode dispatch compiles cleanly
6. **Mutable Variables**: Accumulator patterns in bitops functions work
7. **Const Generic Functions**: `exec_l0_l1::<32>(...)` for parameterized width operations
8. **Generate-for Loops**: Demonstrated in stdlib for bit manipulation entities (BitReverser, Popcount)

### Karythra-Specific Notes:
- The CLE's L4-L5 graphics operations benefit heavily from pipeline annotations
- 256-bit datapath support was essential for content-addressed model
- FP32 native support enabled direct implementation of math kernels
- Const generic functions work well: `exec_l0_l1::<32>()` for parameterized operations
- Named generics (`<W: 32>`) work for both entity instantiation and function calls

---

## Recommendations

### For Skalp Development (Next Priorities):
1. ~~**Priority 1**: RAM/Memory inference~~ ✅ **DONE** (Dec 2025)
2. ~~**Priority 2**: Formal verification integration (SVA assertions → property checking)~~ ✅ **DONE** (Dec 2025)
3. **Priority 3**: Debug infrastructure (trace signals, waveform export)
4. **Priority 4**: CDC helpers (synchronizer inference)
5. ~~**Priority 5**: Named generics for function calls~~ ✅ **DONE** (Dec 2025)

### For Skalp Users:
1. **Use pipeline annotations** on timing-critical functions - the backend handles register insertion
2. **Use the stdlib** - bitops, fp, vector are battle-tested
3. **Named generics** improve readability for complex parameterized designs
4. **Generate blocks** replace manual unrolling - use them
5. **`SKALP_DUMP_SHADER=1`** for debugging codegen issues
6. **Keep functions small** - helps inlining decisions

---

## Conclusion

Skalp is now **production-ready for accelerator development**. The December 2025 additions addressed the major pain points:
- Generate blocks enable metaprogramming
- Pipeline annotations handle timing
- Named generics improve clarity
- Error messages have source locations

**Would I use Skalp again?** Absolutely, for:
- Arithmetic accelerators (FP32, wide datapaths)
- Algorithmic hardware (complex control flow)
- Rapid prototyping (fast simulation)
- AI-assisted development (Rust-like syntax works well with LLMs)

**Would I avoid Skalp for?**
- Multi-clock designs with complex CDC
- ASIC designs needing power intent

**Final Note**: The compiler is actively improving. Bugs encountered during Karythra development (#85, #86, #110, #118) were all fixed. The development velocity is impressive.
