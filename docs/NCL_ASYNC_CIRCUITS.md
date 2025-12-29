# NCL (Null Convention Logic) Asynchronous Circuit Support

## Overview

SKALP supports **Null Convention Logic (NCL)**, a delay-insensitive asynchronous circuit design methodology that eliminates clocks entirely. NCL circuits use **dual-rail encoding** and **threshold gates** to achieve self-timed operation, making them ideal for:

- **Low-power designs**: No clock distribution network, activity-based power consumption
- **Noise-immune systems**: Delay-insensitive operation tolerates timing variations
- **Security-critical applications**: No clock to analyze for side-channel attacks
- **Mixed-timing domains**: Natural interface between different timing regions
- **Radiation-hardened designs**: Self-checking properties detect transient faults

## Core Concepts

### Dual-Rail Encoding

NCL uses **dual-rail encoding** where each logical bit is represented by two physical wires:

| Logical Value | Rail T (True) | Rail F (False) | State |
|---------------|---------------|----------------|-------|
| NULL          | 0             | 0              | No data present |
| DATA 0        | 0             | 1              | Logic false |
| DATA 1        | 1             | 0              | Logic true |
| (Invalid)     | 1             | 1              | Illegal state |

The circuit alternates between **NULL wavefronts** (all 00) and **DATA wavefronts** (valid data), ensuring delay-insensitive operation.

### Threshold Gates (THmn)

NCL uses **threshold gates** instead of traditional Boolean gates. A THmn gate:
- Has **n inputs**
- Output goes **HIGH** when at least **m** inputs are HIGH
- Output goes **LOW** when **all** inputs are LOW
- Output **holds previous value** otherwise (hysteresis)

This hysteresis property is crucial for proper NCL operation.

#### Common Threshold Gates

| Gate | Behavior | Boolean Equivalent |
|------|----------|-------------------|
| TH12 | 1-of-2 threshold | OR with hysteresis |
| TH22 | 2-of-2 threshold | C-element (Muller C-gate) |
| TH23 | 2-of-3 threshold | Majority with hysteresis |
| TH33 | 3-of-3 threshold | 3-input C-element |
| TH34 | 3-of-4 threshold | 3-of-4 majority |
| TH44 | 4-of-4 threshold | 4-input C-element |

### NCL Logic Mappings

Boolean operations map to NCL gate combinations:

| Operation | True Rail (Y_t) | False Rail (Y_f) |
|-----------|-----------------|------------------|
| AND(A,B)  | TH22(A_t, B_t)  | TH12(A_f, B_f)   |
| OR(A,B)   | TH12(A_t, B_t)  | TH22(A_f, B_f)   |
| NOT(A)    | A_f             | A_t              |
| XOR(A,B)  | TH22(TH12(A_t,B_f), TH12(A_f,B_t)) | TH22(TH12(A_t,B_t), TH12(A_f,B_f)) |

---

## Language Syntax

### Async Entity Declaration

Use the `async` keyword before `entity` to declare an NCL module:

```skalp
// Synchronous entity (traditional clocked design)
entity Counter {
    in clk: clock
    in reset: bit[1]
    out count: bit[8]
}

// Asynchronous NCL entity (clockless)
async entity NclAdder {
    in a: bit[8]
    in b: bit[8]
    out sum: bit[8]
}
```

Async entities:
- Have **no clock input** (clockless by design)
- Use **dual-rail encoding** internally (automatic transformation)
- Generate **completion detection** logic automatically
- Support **barrier statements** for pipeline stages

### Barrier Statement

The `barrier` statement marks pipeline stage boundaries in NCL designs:

```skalp
async entity NclPipeline {
    in data: bit[32]
    out result: bit[32]
}

impl NclPipeline {
    // Stage 1: Multiply by 2
    let stage1 = data << 1

    barrier  // Completion detection inserted here

    // Stage 2: Add offset
    let stage2 = stage1 + 0x100

    barrier  // Completion detection inserted here

    // Stage 3: Final masking
    result = stage2 & 0xFFFF
}
```

Barriers:
- Insert **completion detection** logic between stages
- Enable **wavefront-based pipelining** (multiple data items in flight)
- Automatically generate **acknowledge signals** for flow control

### NCL Type (Explicit Dual-Rail)

For explicit control over dual-rail signals, use the `ncl<N>` type:

```skalp
async entity NclMux {
    in sel: ncl<1>      // 1-bit dual-rail (2 wires)
    in a: ncl<8>        // 8-bit dual-rail (16 wires)
    in b: ncl<8>
    out y: ncl<8>
}
```

The `ncl<N>` type:
- Explicitly declares a dual-rail signal of N logical bits
- Generates 2N physical wires (N true rails + N false rails)
- Useful for module boundaries and explicit control

---

## Compilation Flow

### Phase 1: Frontend Parsing

The frontend recognizes NCL-specific syntax:

```
Source Code → Lexer → Parser → AST → HIR
                ↓
         Tokens: async, barrier
         Types: ncl<N>
         Flags: is_async on entities
```

### Phase 2: MIR Generation

HIR is lowered to MIR with NCL annotations:

```rust
// MIR Module with NCL support
Module {
    name: "NclAdder",
    is_async: true,           // NCL module flag
    barriers: vec![           // Pipeline stage boundaries
        BarrierStage { id: 0, signals: [...] },
        BarrierStage { id: 1, signals: [...] },
    ],
    processes: vec![
        Process {
            kind: ProcessKind::Async,  // NCL process
            ...
        }
    ],
    ...
}
```

### Phase 3: NCL Expansion (MIR → LIR)

The **ncl_expand** pass transforms Boolean logic to dual-rail NCL:

```
Boolean LIR                    NCL LIR
───────────                    ───────
signal a: bit[1]    →    signal a_t: bit[1]
                         signal a_f: bit[1]

y = a & b           →    y_t = TH22(a_t, b_t)
                         y_f = TH12(a_f, b_f)

y = a | b           →    y_t = TH12(a_t, b_t)
                         y_f = TH22(a_f, b_f)

y = ~a              →    y_t = a_f
                         y_f = a_t
```

#### Arithmetic Expansion

NCL arithmetic uses specialized gate structures:

**NCL Full Adder:**
```
         ┌─────────────────────────────────────┐
    A ───┤                                     ├─── Sum
    B ───┤      NCL Full Adder Cell            ├─── Carry
  Cin ───┤  (TH22, TH23, TH33 combinations)    │
         └─────────────────────────────────────┘
```

**NCL Multiplier:** Array of NCL AND gates feeding NCL adder tree

**NCL Comparator:** Magnitude comparison using TH22/TH12 cascades

### Phase 4: Technology Mapping

NCL primitives map to library cells:

| NCL Operation | Gate Structure |
|---------------|----------------|
| NclAnd        | TH22 + TH12 pair per bit |
| NclOr         | TH12 + TH22 pair per bit |
| NclXor        | 4× TH12 + 2× TH22 per bit |
| NclAdd        | NCL full-adder chain |
| NclMul        | NCL AND array + adder tree |
| NclReg        | DATA/NULL latch with Ki/Ko |
| NclComplete   | Completion detection tree |

### Phase 5: Verilog Generation

NCL designs generate structural Verilog with THmn cells:

```verilog
// Generated NCL module
module NclAdder (
    input a_t0, a_f0,    // Dual-rail input a[0]
    input b_t0, b_f0,    // Dual-rail input b[0]
    output sum_t0, sum_f0 // Dual-rail output sum[0]
);
    // NCL gates
    TH22 U0 (.A(a_t0), .B(b_t0), .Y(and_t));
    TH12 U1 (.A(a_f0), .B(b_f0), .Y(and_f));
    ...
endmodule

// NCL Cell Library (auto-generated)
module TH22 (input A, B, output reg Y);
    wire [2:0] sum = {2'b0, A} + {2'b0, B};
    always @(A, B) begin
        if (sum >= 3'd2) Y <= 1'b1;
        else if (~A & ~B) Y <= 1'b0;
        // else hold (hysteresis)
    end
endmodule
```

---

## Simulation

### Wavefront Simulation

NCL circuits are simulated using **wavefront propagation** rather than clock edges:

```rust
use skalp_sim::{NclSimulator, NclPhase, NclValue};

// Create NCL simulator
let mut sim = NclSimulator::new(netlist, NclSimConfig::default());

// Apply NULL wavefront (reset)
sim.set_null_wavefront();
sim.run_until_stable();

// Apply DATA wavefront
sim.set_dual_rail("a", NclValue::DataTrue);   // a = 1
sim.set_dual_rail("b", NclValue::DataFalse);  // b = 0
sim.run_until_stable();

// Read output
let result = sim.get_dual_rail("sum");
assert_eq!(result, NclValue::DataTrue);  // 1 + 0 = 1
```

### Simulation States

The NCL simulator tracks three phases:

| Phase | Description |
|-------|-------------|
| `Null` | All dual-rail signals are 00 (NULL) |
| `Data` | All dual-rail signals are valid (01 or 10) |
| `Transitioning` | Mixed NULL and DATA (wavefront propagating) |

### THmn Gate Evaluation

Threshold gates maintain state for hysteresis:

```rust
/// Evaluate THmn gate with state
pub fn evaluate_thmn_stateful(
    m: usize,           // Threshold
    n: usize,           // Number of inputs
    inputs: &[bool],    // Current input values
    prev_state: bool,   // Previous output value
) -> bool {
    let count = inputs.iter().take(n).filter(|&&x| x).count();

    if count >= m {
        true           // Threshold met → HIGH
    } else if count == 0 {
        false          // All LOW → LOW
    } else {
        prev_state     // Hold previous (hysteresis)
    }
}
```

---

## Examples

### Simple NCL Inverter

```skalp
async entity NclInverter {
    in a: bit[1]
    out y: bit[1]
}

impl NclInverter {
    y = ~a
}
```

Generated NCL (conceptual):
```
a_t ─────┬───── y_f
         │
a_f ─────┴───── y_t
```

### NCL 8-bit Adder

```skalp
async entity NclAdder8 {
    in a: bit[8]
    in b: bit[8]
    out sum: bit[8]
    out carry: bit[1]
}

impl NclAdder8 {
    let result = a +: b    // Widening add
    sum = result[7:0]
    carry = result[8]
}
```

### NCL Pipelined Multiplier

```skalp
async entity NclPipelinedMul {
    in a: bit[16]
    in b: bit[16]
    out product: bit[32]
}

impl NclPipelinedMul {
    // Stage 1: Partial products (lower bits)
    let pp_low = a[7:0] * b[7:0]

    barrier

    // Stage 2: Partial products (cross terms)
    let pp_mid1 = a[15:8] * b[7:0]
    let pp_mid2 = a[7:0] * b[15:8]

    barrier

    // Stage 3: Partial products (upper bits)
    let pp_high = a[15:8] * b[15:8]

    barrier

    // Stage 4: Final accumulation
    product = pp_low + (pp_mid1 << 8) + (pp_mid2 << 8) + (pp_high << 16)
}
```

### NCL State Machine

```skalp
async entity NclFSM {
    in start: bit[1]
    in data: bit[8]
    out ready: bit[1]
    out result: bit[8]
}

impl NclFSM {
    // NCL state machines use completion detection
    // for state transitions instead of clock edges

    let state: bit[2] = 0

    match state {
        0 => {
            // IDLE: Wait for start
            ready = 1
            if start {
                state = 1
            }
        }
        1 => {
            // PROCESS: Transform data
            ready = 0
            result = data ^ 0xAA

            barrier  // Wait for computation

            state = 2
        }
        2 => {
            // DONE: Signal completion
            ready = 1
            state = 0
        }
    }
}
```

---

## NCL Cell Library Reference

### Threshold Gates

| Cell | Inputs | Function | Verilog |
|------|--------|----------|---------|
| TH12 | 2 | Y=1 if ≥1 HIGH | `module TH12(input A,B, output reg Y)` |
| TH22 | 2 | Y=1 if ≥2 HIGH | `module TH22(input A,B, output reg Y)` |
| TH13 | 3 | Y=1 if ≥1 HIGH | `module TH13(input A,B,C, output reg Y)` |
| TH23 | 3 | Y=1 if ≥2 HIGH | `module TH23(input A,B,C, output reg Y)` |
| TH33 | 3 | Y=1 if ≥3 HIGH | `module TH33(input A,B,C, output reg Y)` |
| TH14 | 4 | Y=1 if ≥1 HIGH | `module TH14(input A,B,C,D, output reg Y)` |
| TH24 | 4 | Y=1 if ≥2 HIGH | `module TH24(input A,B,C,D, output reg Y)` |
| TH34 | 4 | Y=1 if ≥3 HIGH | `module TH34(input A,B,C,D, output reg Y)` |
| TH44 | 4 | Y=1 if ≥4 HIGH | `module TH44(input A,B,C,D, output reg Y)` |

### Special Cells

| Cell | Function | Ports |
|------|----------|-------|
| NCL_COMPLETION | Completion detection | `I[N-1:0]` → `Y` |
| NCL_REG | DATA/NULL latch | `D, Ki` → `Q, Ko` |

---

## Design Guidelines

### When to Use NCL

**Good candidates:**
- Ultra-low-power designs (activity-based power)
- Security-sensitive circuits (no clock side-channel)
- Mixed-timing integration (natural async interfaces)
- Radiation-tolerant systems (self-checking)
- Variable-latency operations (data-dependent timing)

**Poor candidates:**
- High-frequency designs (NCL has overhead)
- Designs requiring precise timing control
- Extremely area-constrained designs (2x wire overhead)

### Performance Considerations

| Aspect | NCL | Synchronous |
|--------|-----|-------------|
| Area | ~2-3x (dual-rail) | 1x baseline |
| Power | Lower (no clock) | Clock dominates |
| Latency | Data-dependent | Fixed cycles |
| Throughput | Matches sync | Clock-limited |
| Timing closure | Not needed | Critical path |

### Best Practices

1. **Use barriers strategically**: More barriers = more pipeline parallelism but more area
2. **Balance stage depths**: Uneven stages create throughput bottlenecks
3. **Minimize feedback loops**: NCL feedback requires careful handshaking
4. **Test with wavefront simulation**: Verify NULL/DATA alternation
5. **Check completion coverage**: Ensure all outputs are monitored

---

## Implementation Details

### Source Files

| File | Description |
|------|-------------|
| `crates/skalp-frontend/src/lexer.rs` | `Barrier` token |
| `crates/skalp-frontend/src/parse.rs` | `async entity`, `barrier` parsing |
| `crates/skalp-frontend/src/ast.rs` | `is_async`, `Barrier` statement |
| `crates/skalp-frontend/src/hir.rs` | `HirType::Ncl`, `HirBarrier` |
| `crates/skalp-mir/src/mir.rs` | `ProcessKind::Async`, `DataType::Ncl` |
| `crates/skalp-lir/src/lir.rs` | NCL `PrimitiveType`, `LirOp` variants |
| `crates/skalp-lir/src/ncl_expand.rs` | Boolean→NCL transformation |
| `crates/skalp-lir/src/tech_mapper.rs` | NCL gate mapping (~1400 lines) |
| `crates/skalp-lir/src/tech_library.rs` | NCL `CellFunction` variants |
| `crates/skalp-sim/src/ncl_sim.rs` | Wavefront simulation |
| `crates/skalp-lir/src/gate_netlist.rs` | NCL Verilog generation |

### Key Data Structures

```rust
// NCL Phase tracking
pub enum NclPhase {
    Null,          // All signals NULL (00)
    Data,          // All signals DATA (01 or 10)
    Transitioning, // Mixed (wavefront in progress)
}

// Dual-rail value
pub enum NclValue {
    Null,       // 00 - No data
    DataFalse,  // 01 - Logic 0
    DataTrue,   // 10 - Logic 1
    Invalid,    // 11 - Error state
}

// NCL primitive types
pub enum PrimitiveType {
    // ... standard gates ...
    Th12, Th22, Th13, Th23, Th33,
    Th14, Th24, Th34, Th44,
    Thmn { m: u8, n: u8 },
    NclCompletion { width: u32 },
}

// NCL LIR operations
pub enum LirOp {
    // ... standard ops ...
    NclEncode { width: u32 },
    NclDecode { width: u32 },
    NclAnd { width: u32 },
    NclOr { width: u32 },
    NclXor { width: u32 },
    NclNot { width: u32 },
    NclAdd { width: u32 },
    NclSub { width: u32 },
    NclMul { width: u32 },
    NclLt { width: u32 },
    NclEq { width: u32 },
    NclMux2 { width: u32 },
    NclReg { width: u32 },
    NclComplete { width: u32 },
    NclNull { width: u32 },
}
```

---

## References

1. Fant, K.M. & Brandt, S.A. (1996). "NULL Convention Logic: A Complete and Consistent Logic for Asynchronous Digital Circuit Synthesis"
2. Smith, S.C. (2009). "Designing Asynchronous Circuits using NULL Convention Logic (NCL)"
3. Bandapati, S.K. et al. (2003). "Design and Characterization of NULL Convention Self-Timed Multipliers"

---

## See Also

- [Compiler Architecture](COMPILER_ARCHITECTURE.md)
- [Simulation Architecture](SIMULATION_ARCHITECTURE.md)
- [Gate-Level Simulation](implementation/GATE_LEVEL_SIMULATION.md)
- [CDC Support](CDC_SUPPORT.md) - For mixed sync/async boundaries
