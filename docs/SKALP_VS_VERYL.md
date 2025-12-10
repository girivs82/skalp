# SKALP vs Veryl: Comprehensive Comparison

Both SKALP and Veryl are modern HDLs that compile to SystemVerilog. This document provides an objective comparison of their features, philosophy, and capabilities.

## Quick Summary

| Aspect | SKALP | Veryl |
|--------|-------|-------|
| **Philosophy** | Rust-like, type-safe, formal verification | SystemVerilog evolution, interoperability |
| **Syntax** | Expression-based, pattern matching | Simplified SV, statement-based |
| **Type System** | Strong (structs, enums, traits) | Moderate (improved SV types) |
| **Simulation** | Built-in GPU/CPU simulator | External (cocotb/SV testbenches) |
| **Formal Verification** | Integrated BMC + properties | Not mentioned |
| **CDC Analysis** | Automatic compile-time | Manual annotation required |
| **Maturity** | Research/development | Exploration phase |
| **Target Users** | Developers wanting Rust-like safety | SV users wanting modern syntax |

---

## 1. Syntax Comparison

### Counter Example

**Veryl:**
```veryl
module Counter #(
    param WIDTH: u32 = 1,
)(
    i_clk: input clock,
    i_rst: input reset,
    o_cnt: output logic<WIDTH>,
){
    var r_cnt: logic<WIDTH>;

    always_ff {
        if_reset {
            r_cnt = 0;
        } else {
            r_cnt += 1;
        }
    }

    always_comb {
        o_cnt = r_cnt;
    }
}
```

**SKALP:**
```skalp
entity Counter<const WIDTH: nat = 1> {
    in clk: clock
    in rst: reset
    out count: nat[WIDTH]
}

impl Counter {
    signal r_cnt: nat[WIDTH]

    count = r_cnt

    on(clk.rise) {
        if rst {
            r_cnt <= 0
        } else {
            r_cnt <= r_cnt + 1
        }
    }
}
```

**Analysis:**
- **SKALP**: More concise (15 lines vs 22), separates entity/impl, declarative output
- **Veryl**: Closer to SystemVerilog idioms, explicit always blocks
- Both eliminate `[WIDTH-1:0]` notation
- Both have dedicated `clock`/`reset` types

---

## 2. Feature-by-Feature Comparison

### 2.1 Clock and Reset Handling

**Veryl:**
```veryl
i_clk: input clock,
i_rst: input reset,

always_ff {
    if_reset {
        // Reset logic - polarity configured at build time
    }
}
```

**Advantages:**
- ✅ Polarity configurable at build time (ASIC vs FPGA)
- ✅ No need to specify `@(posedge clk or negedge rst)`
- ✅ `if_reset` syntax sugar

**SKALP:**
```skalp
in clk: clock
in rst: reset(active_high)

on(clk.rise) {
    if rst {
        // Reset logic
    }
}
```

**Advantages:**
- ✅ Declarative polarity in type (`active_high`/`active_low`)
- ✅ Clean event syntax `on(clk.rise)`
- ✅ No boilerplate

**Winner**: **Draw** - Both solve the problem well, different approaches

---

### 2.2 Type System

**Veryl:**
```veryl
var r_cnt: logic<WIDTH>;
param WIDTH: u32 = 8;
```

**Features:**
- `logic<WIDTH>` instead of `[WIDTH-1:0]`
- Basic type system
- No structs/enums mentioned in docs

**SKALP:**
```skalp
struct PacketHeader {
    src_addr: nat[32]
    dst_addr: nat[32]
    length: nat[16]
    checksum: nat[16]
}

enum State {
    Idle,
    Processing,
    Done
}

signal header: PacketHeader
signal state: State
```

**Features:**
- Full structs with named fields
- Enums with pattern matching
- Traits with trait bounds and methods
- Strong type safety

**Winner**: **SKALP** - Much richer type system

---

### 2.3 Assignment Operators

**Veryl (Unique Feature):**
```veryl
always_ff {
    r_cnt = r_cnt + 1;     // No `<=` needed! Inferred as non-blocking
    r_cnt += 1;            // Compound operators work!
}

always_comb {
    o_cnt = r_cnt;         // Inferred as blocking
}
```

**Advantage:**
- ✅ No confusion between `=` and `<=`
- ✅ Context-aware assignment inference
- ✅ Compound operators (`+=`, `-=`, etc.)

**SKALP:**
```skalp
on(clk.rise) {
    r_cnt <= r_cnt + 1     // Explicit non-blocking
}

count = r_data             // Combinational
```

**Approach:**
- Explicit `<=` for sequential
- Explicit `=` for combinational
- No compound operators in sequential context

**Winner**: **Veryl** - Assignment inference is elegant

---

### 2.4 Pattern Matching

**SKALP:**
```skalp
result = match op {
    0b000 => a + b,
    0b001 => a - b,
    0b010 => a & b,
    0b011 => a | b,
    _ => 0
};
```

**Features:**
- Full Rust-style pattern matching
- Expression-based (can assign inline)
- Exhaustiveness checking
- Works with enums

**Veryl:**
```veryl
// Uses case expressions
case op {
    3'b000: result = a + b;
    3'b001: result = a - b;
    default: result = 0;
}
```

**Features:**
- If/case expressions (improvement over SV)
- Statement-based

**Winner**: **SKALP** - More powerful pattern matching

---

### 2.5 CDC (Clock Domain Crossing) Analysis

**Veryl:**
```veryl
// Manual annotation required
unsafe (cdc) {
    // Code that crosses clock domains
}
```

**Features:**
- Explicit CDC blocks
- Requires developer to mark CDC regions
- Clock domain tracking

**SKALP:**
```
Phase 2: Clock Domain Crossing (CDC) analysis
✓ Detecting violations automatically
```

**Features:**
- **Automatic** CDC analysis at compile time
- No manual annotations needed
- Detects 4 violation types:
  - DirectCrossing
  - CombinationalMixing
  - AsyncResetCrossing
  - ArithmeticMixing
- Integrated into MIR compiler pass

**Winner**: **SKALP** - Automatic vs manual annotation

---

### 2.6 Simulation & Testing

**Veryl:**
```veryl
// Uses external testbenches
#[test(sv)]
embed (inline) sv{{{
    // SystemVerilog testbench
}}}

#[test(cocotb)]
// Python cocotb tests
```

**Features:**
- Embedded SystemVerilog testbenches
- cocotb integration
- `veryl test` command
- No built-in simulator

**SKALP:**
```rust
let mut tb = Testbench::new("examples/fifo.sk").await.unwrap();

// Type-safe, ergonomic API
tb.set("wr_en", 1u8).set("wr_data", 0xAA);
tb.clock(2).await;
tb.expect("empty", 0u8).await;
```

**Features:**
- **Built-in simulator** (GPU-accelerated on macOS)
- CPU fallback
- Type-safe Rust testbench API
- Automatic clock handling
- VCD waveform generation
- No external tools needed

**Winner**: **SKALP** - Integrated simulation is huge

---

### 2.7 Formal Verification

**Veryl:**
- Not mentioned in documentation
- No built-in formal verification features

**SKALP:**
```rust
// Built into skalp-verify crate
- Bounded model checking (BMC)
- Property verification
- Invariant checking
- Coverage analysis
```

**Features:**
- Integrated formal verification
- Works directly on SKALP code
- No external tools needed

**Winner**: **SKALP** - Veryl has no formal verification

---

### 2.8 Generics & Metaprogramming

**Veryl:**
```veryl
module Example #(
    param WIDTH: u32 = 8,
    const DEPTH: u32 = 16,  // Compile-time constant
) {
    // Advanced code generation capabilities
}
```

**Features:**
- `param` and `const` parameters
- Advanced code generation
- Parameterizable functions

**SKALP:**
```skalp
entity FIFO<const WIDTH: nat = 8, const DEPTH: nat = 16> {
    // ...
}

impl FIFO {
    signal wr_ptr: nat[clog2(DEPTH)]  // Const expressions!
    signal count: nat[clog2(DEPTH+1)]
}
```

**Features:**
- Const expressions in types
- `clog2()`, arithmetic in widths
- Generic type parameters
- Traits with bounds and methods

**Winner**: **SKALP** - Const expressions in types is powerful

---

### 2.9 Development Tools

**Veryl:**
- ✅ Package manager
- ✅ Build tools (`veryl build`)
- ✅ Auto-formatter (`veryl fmt`)
- ✅ Real-time checker (LSP)
- ✅ Editor integration (VSCode, Vim, Emacs)
- ✅ Auto-completion

**SKALP:**
- ✅ Compiler (`skalp build`)
- ✅ Integrated simulator (`skalp sim`)
- ✅ Test runner
- ⚠️ No LSP yet
- ⚠️ No package manager yet
- ⚠️ No auto-formatter yet

**Winner**: **Veryl** - More mature tooling ecosystem

---

### 2.10 Code Conciseness

**FIFO Example Line Count:**

| Implementation | Lines |
|----------------|-------|
| SystemVerilog | 59 |
| Veryl | ~45 (estimated) |
| SKALP | 44 |

**Winner**: **SKALP** (slightly) - But both are concise

---

## 3. What Veryl Has That SKALP Doesn't

### 1. **Assignment Inference** ⭐
```veryl
always_ff {
    cnt = cnt + 1;  // No `<=` needed!
}
```
**Impact**: High - Eliminates `=` vs `<=` confusion

### 2. **Build-Time Clock/Reset Polarity Configuration** ⭐
```veryl
// Same code generates for both ASIC and FPGA
always_ff {
    if_reset { ... }  // Polarity set at build time
}
```
**Impact**: Medium - Useful for multi-target designs

### 3. **Mature Tooling Ecosystem** ⭐⭐
- Package manager
- LSP server
- Auto-formatter
- Editor plugins

**Impact**: High - Critical for productivity

### 4. **Embedded Testbenches**
```veryl
#[test(sv)]
embed (inline) sv{{{
    // SystemVerilog test
}}}
```
**Impact**: Low - SKALP's approach is more ergonomic

### 5. **Trailing Comma Support**
```veryl
module Foo (
    a: input logic,
    b: input logic,  // <-- Trailing comma allowed
) { }
```
**Impact**: Low - Nice to have

### 6. **Range-Based For Loops**
```veryl
for i in 0..10 {
    // Loop body
}
```
**Impact**: Medium - More readable than C-style

### 7. **`msb` Notation**
```veryl
logic<msb:0>  // Instead of [msb:0]
```
**Impact**: Low - Syntactic sugar

---

## 4. What SKALP Has That Veryl Doesn't

### 1. **Rich Type System** ⭐⭐⭐
```skalp
struct PacketHeader { ... }
enum State { Idle, Processing, Done }
trait Bus { ... }  // Complete with bounds and methods
```
**Impact**: Very High - Essential for large designs

### 2. **Automatic CDC Analysis** ⭐⭐⭐
```
Phase 2: Clock Domain Crossing (CDC) analysis
✓ Detecting DirectCrossing violations
```
**Impact**: Very High - Catches bugs at compile time

### 3. **Built-in Simulator** ⭐⭐⭐
```rust
let mut tb = Testbench::new("design.sk").await.unwrap();
// GPU-accelerated simulation
```
**Impact**: Very High - No external tools needed

### 4. **Formal Verification** ⭐⭐
- Bounded model checking
- Property verification
- Coverage analysis

**Impact**: High - Critical for safety-critical designs

### 5. **Expression-Based Syntax** ⭐⭐
```skalp
let result = match op {
    0b000 => a + b,
    _ => 0
};
```
**Impact**: High - More composable

### 6. **Const Expressions in Types** ⭐⭐
```skalp
signal ptr: nat[clog2(DEPTH)]     // Compiler evaluates!
signal cnt: nat[clog2(DEPTH+1)]
```
**Impact**: High - Type-level metaprogramming

### 7. **Pattern Matching with Exhaustiveness** ⭐
```skalp
match state {
    State::Idle => ...,
    State::Processing => ...,
    // Compiler warns if State::Done is missing
}
```
**Impact**: Medium - Prevents bugs

### 8. **Multi-Clock Testbench API** ⭐
```rust
tb.clock_multi(&[("wr_clk", 2), ("rd_clk", 3)]).await;
```
**Impact**: Medium - Essential for CDC testing

---

## 5. Philosophy Comparison

### Veryl Philosophy
- **"Evolutionary, not revolutionary"**
- Based on SystemVerilog, improve incrementally
- Prioritize interoperability
- Keep it close to what SV developers know
- Generate readable SystemVerilog
- Tool-friendly

**Target User**: *"I know SystemVerilog. I want something cleaner but still familiar."*

### SKALP Philosophy
- **"Modern language for hardware"**
- Rust-like syntax and safety
- Strong type system
- Formal verification first-class
- Integrated toolchain
- Expression-based

**Target User**: *"I want Rust's safety guarantees for hardware design."*

---

## 6. Interoperability & Generated Code

### Veryl
**Focus**: Generates **human-readable** SystemVerilog
```systemverilog
// Clean, maintainable SystemVerilog output
module Counter (
    input logic clk,
    input logic rst,
    output logic [7:0] cnt
);
    // Readable generated code
endmodule
```

**Advantage**: Easy to debug generated code, integrate with existing SV

### SKALP
**Focus**: Generates **correct** SystemVerilog
```systemverilog
// Functional SystemVerilog (may have nested ternaries)
assign result = ((op == 3'b000) ? (a + b) : ...);
```

**Advantage**: Correctness over readability, but still works with standard tools

**Winner**: **Veryl** - Readability of generated code

---

## 7. Maturity & Ecosystem

### Veryl (as of 2024)
- Presented at DVCon Japan 2024
- Active GitHub repository
- "Exploration phase of language design"
- Package manager exists
- LSP server works
- Growing community

### SKALP
- Research/development phase
- Comprehensive compiler pipeline
- Built-in simulator works
- Formal verification implemented
- No LSP yet
- Smaller community

**Winner**: **Veryl** - More mature tooling

---

## 8. Use Case Recommendations

### Choose Veryl If:
- ✅ You're a SystemVerilog expert
- ✅ You want incremental improvements to SV workflow
- ✅ You need to integrate with large existing SV codebases
- ✅ You want mature editor support NOW
- ✅ You prefer statement-based programming
- ✅ You want readable generated code for debugging

### Choose SKALP If:
- ✅ You come from Rust/modern language background
- ✅ You want strong type safety (structs, enums, traits)
- ✅ You need automatic CDC analysis
- ✅ You need integrated simulation (especially GPU)
- ✅ You need formal verification
- ✅ You prefer expression-based programming
- ✅ You want one tool for everything
- ✅ You're building new designs from scratch

---

## 9. Detailed Feature Matrix

| Feature | SKALP | Veryl | Winner |
|---------|-------|-------|--------|
| **Language Features** | | | |
| Dedicated clock/reset types | ✅ | ✅ | Draw |
| Assignment inference | ❌ | ✅ | Veryl |
| Structs | ✅ | ❌ | SKALP |
| Enums | ✅ | ❌ | SKALP |
| Traits | ✅ Complete | ❌ | SKALP |
| Pattern matching | ✅ Full | ⚠️ Basic | SKALP |
| If expressions | ✅ | ✅ | Draw |
| Const expressions in types | ✅ | ❌ | SKALP |
| Generics | ✅ | ✅ | Draw |
| **Safety & Verification** | | | |
| CDC analysis | ✅ Automatic | ⚠️ Manual | SKALP |
| Formal verification | ✅ Built-in | ❌ | SKALP |
| Type safety | ✅ Strong | ⚠️ Moderate | SKALP |
| Exhaustiveness checking | ✅ | ❌ | SKALP |
| **Tooling** | | | |
| Compiler | ✅ | ✅ | Draw |
| Simulator | ✅ Built-in | ❌ | SKALP |
| Package manager | ❌ | ✅ | Veryl |
| LSP server | ❌ | ✅ | Veryl |
| Auto-formatter | ❌ | ✅ | Veryl |
| Editor integration | ❌ | ✅ | Veryl |
| **Testing** | | | |
| Integrated testbench | ✅ Rust API | ⚠️ Embedded | SKALP |
| GPU simulation | ✅ | ❌ | SKALP |
| Multi-clock testing | ✅ | ❌ | SKALP |
| **Output** | | | |
| Generated code readability | ⚠️ Good | ✅ Excellent | Veryl |
| SystemVerilog interop | ✅ | ✅ | Draw |
| **Maturity** | | | |
| Tooling ecosystem | ⚠️ Growing | ✅ Mature | Veryl |
| Community | Small | Medium | Veryl |
| Documentation | ✅ Good | ✅ Good | Draw |

---

## 10. Combined Strengths (What Both Get Right)

✅ **Modern syntax** - Both eliminate SystemVerilog cruft
✅ **Dedicated clock/reset types** - Safety improvement
✅ **Generates SystemVerilog** - Compatible with existing tools
✅ **Parametric designs** - Generics/parameters
✅ **Active development** - Both evolving
✅ **Open source** - Community-driven

---

## 11. Hypothetical: What If They Combined?

Imagine a language with:
- ✅ SKALP's type system (structs, enums, traits)
- ✅ SKALP's automatic CDC analysis
- ✅ SKALP's built-in simulator
- ✅ SKALP's formal verification
- ✅ Veryl's assignment inference (no `<=` vs `=`)
- ✅ Veryl's build-time clock polarity
- ✅ Veryl's LSP server and tooling
- ✅ Veryl's package manager
- ✅ Both's clean syntax

**That would be the ultimate HDL!** 🚀

---

## 12. Conclusion

### Key Takeaway

**Veryl and SKALP solve different problems:**

**Veryl** = *"SystemVerilog, but better"*
- Evolutionary improvement
- Focus on familiarity and tooling
- Gentle learning curve for SV developers

**SKALP** = *"Rust for hardware"*
- Revolutionary approach
- Focus on type safety and verification
- Steep learning curve, but powerful

### Recommendation

**For Industry/Production** (2024-2025):
- **Veryl** is safer bet - More mature, better tooling, familiar

**For Research/New Projects**:
- **SKALP** has more potential - Richer features, integrated verification

**Long-term Vision**:
- Both languages push HDL design forward
- Competition drives innovation
- Learn from each other's strengths

### Final Verdict

**Neither is strictly "better"** - they target different audiences:
- **Veryl** wins on: Tooling, readability, incremental adoption
- **SKALP** wins on: Type safety, verification, simulation

Both are **vastly better** than hand-written SystemVerilog! 🎯
