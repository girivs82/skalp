# Intent System: Comprehensive Design

**A Language Feature for Expressing Hardware Synthesis Intent**

This document describes the complete design of SKALP's intent system, which allows users to express resource allocation, timing, and optimization preferences in an idiomatic, composable way.

---

## Table of Contents

1. [Design Philosophy](#1-design-philosophy)
2. [Syntax Specification](#2-syntax-specification)
3. [Architecture and Compiler Flow](#3-architecture-and-compiler-flow)
4. [Primitive Properties Vocabulary](#4-primitive-properties-vocabulary)
5. [Stdlib Intent Definitions](#5-stdlib-intent-definitions)
6. [Custom Intent Creation](#6-custom-intent-creation)
7. [Target Backend Mapping](#7-target-backend-mapping)
8. [Hardware Context: Dedicated vs SIMD Multipliers](#8-hardware-context-dedicated-vs-simd-multipliers)
9. [Complete Examples](#9-complete-examples)
10. [Implementation Plan](#10-implementation-plan)

---

## 1. Design Philosophy

### 1.1 Core Principles

1. **Explicit Named Intents** - Always use `intent::name` syntax for clarity
2. **Stdlib-Defined** - All intents (including "built-in" ones) defined in stdlib
3. **No Special-Casing** - Compiler has no knowledge of specific intents, only primitives
4. **Composable** - Intents decompose into primitive properties
5. **Extensible** - Users can define custom intents using the same mechanism

### 1.2 The Intent Hierarchy

```
User Code
    ↓ (uses intent::name)
Intent Definitions (stdlib or user-defined)
    ↓ (decompose to primitives)
Compiler (knows only primitives)
    ↓ (applies primitives)
Target Backend (maps primitives to implementation)
```

### 1.3 Why Named Intents?

Instead of implicit keywords or cryptic attributes, named intents are:

- **Explicit**: `with intent::throughput` is self-documenting
- **Extensible**: Add new intents without language changes
- **Composable**: Combine intents with clear semantics
- **Consistent**: No distinction between stdlib and user intents

---

## 2. Syntax Specification

### 2.1 Expression-Level Intent

Apply intent to a single expression:

```skalp
// Binary operation with intent
let result = (a * b) with intent::throughput;

// Function call with intent
let sqrt_val = sqrt(x) with intent::low_latency;

// Chained operations
let normalized = (v / length(v)) with intent::dedicated;
```

### 2.2 Block-Level Intent

Apply intent to an entire block:

```skalp
with intent::simd_friendly {
    let a = x * y;
    let b = z * w;
    let c = a + b;
}
```

### 2.3 Entity-Level Intent

Apply intent as default for all operations in an entity:

```skalp
entity FastProcessor with intent::throughput {
    in a: fp32
    in b: fp32
    out result: fp32
}

impl FastProcessor {
    // All operations inherit intent::throughput
    result = sqrt(a * a + b * b);
}
```

### 2.4 Intent Composition

Combine multiple intents with `+`:

```skalp
// Additive composition
let result = expr with intent::throughput + intent::low_power;

// Block with composed intent
with intent::throughput + intent::replicated {
    // operations
}
```

### 2.5 Intent Override

Override parent intent in nested scope:

```skalp
entity Parent with intent::area_efficient {
    in data: fp32
    out result: fp32
}

impl Parent {
    // Most operations use area_efficient
    let intermediate = process(data);

    // Critical path needs different intent
    result = (intermediate * scale) with intent::throughput;
}
```

### 2.6 Grammar Summary

```ebnf
intent_application := expr "with" intent_expr
                   | "with" intent_expr block

intent_expr := "intent" "::" ident
            | intent_expr "+" intent_expr

entity_decl := "entity" ident ["with" intent_expr] "{" ... "}"
```

---

## 3. Architecture and Compiler Flow

### 3.1 Layered Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                       USER CODE                                  │
│  let result = (a * b) with intent::throughput;                  │
└───────────────────────────────────────────────┬─────────────────┘
                                                │
                                                ▼
┌─────────────────────────────────────────────────────────────────┐
│                   INTENT DEFINITIONS                             │
│  (defined in stdlib, loaded at compile time)                    │
│                                                                  │
│  intent throughput {                                            │
│      resource: dedicated,                                       │
│      timing: single_cycle,                                      │
│      constraints: { latency: 1 }                                │
│  }                                                              │
└───────────────────────────────────────────────┬─────────────────┘
                                                │
                                                ▼
┌─────────────────────────────────────────────────────────────────┐
│                      COMPILER                                    │
│  (only understands primitive properties)                        │
│                                                                  │
│  Receives: { resource: dedicated, timing: single_cycle, ... }   │
│  Does NOT know what "throughput" means                          │
└───────────────────────────────────────────────┬─────────────────┘
                                                │
                                                ▼
┌─────────────────────────────────────────────────────────────────┐
│                   TARGET BACKEND                                 │
│  (maps primitives to actual implementation)                     │
│                                                                  │
│  resource: dedicated → Instantiate dedicated multiplier         │
│  timing: single_cycle → No pipelining, parallel execution       │
└─────────────────────────────────────────────────────────────────┘
```

### 3.2 Compiler Phases

#### Phase 1: Parse and Collect Intents

```rust
// Parser produces AST with intent annotations
struct ExprWithIntent {
    expr: Expr,
    intent: IntentRef,  // Reference to "intent::throughput"
}
```

#### Phase 2: Resolve Intent Definitions

```rust
// Load intent definitions from stdlib
fn resolve_intent(name: &str) -> Result<PrimitiveSet, Error> {
    // Look up intent definition
    let def = stdlib.get_intent(name)?;

    // Expand to primitives
    Ok(def.to_primitives())
}
```

#### Phase 3: Apply Primitives During Lowering

```rust
// During HIR → MIR lowering
fn lower_expr_with_intent(expr: &Expr, primitives: &PrimitiveSet) -> MirExpr {
    match expr {
        Expr::BinOp(op, lhs, rhs) => {
            let mir_op = match (op, primitives.resource) {
                (Mul, Resource::Dedicated) => MirOp::DedicatedMul,
                (Mul, Resource::Shared) => MirOp::SharedMul,
                (Mul, Resource::Simd { partitions }) => MirOp::SimdMul(partitions),
                _ => lower_default(op),
            };
            // ...
        }
    }
}
```

#### Phase 4: Backend Code Generation

```rust
// MIR → LIR or RTL
fn generate_multiplier(op: &MirOp, target: &Target) -> HardwareUnit {
    match op {
        MirOp::DedicatedMul => target.instantiate_dedicated_mul(),
        MirOp::SimdMul(4) => target.instantiate_simd_mul_4way(),
        MirOp::SharedMul => target.instantiate_shared_mul_with_arbiter(),
        // ...
    }
}
```

---

## 4. Primitive Properties Vocabulary

The compiler only understands these primitive properties. All intents must decompose into combinations of these.

### 4.1 Resource

Controls hardware resource allocation:

```skalp
// Values
resource::dedicated     // Dedicated hardware unit per operation
resource::shared        // Time-multiplexed shared unit
resource::simd          // SIMD/partitionable unit
resource::auto          // Compiler decides

// Parameterized
resource::simd { partitions: 4 }  // 4-way SIMD
resource::replicated { copies: 2 } // Duplicate for throughput
```

### 4.2 Timing

Controls timing characteristics:

```skalp
// Values
timing::single_cycle    // Complete in one clock cycle
timing::multi_cycle     // May take multiple cycles
timing::pipelined       // Pipelined with initiation interval
timing::combinational   // Pure combinational logic

// Parameterized
timing::pipelined { ii: 1 }      // Initiation interval of 1
timing::latency { cycles: 4 }    // Fixed latency
```

### 4.3 Precision

Controls numerical precision:

```skalp
// Values
precision::full         // Full precision of data type
precision::reduced      // Reduced precision allowed
precision::truncated    // Truncation allowed

// Parameterized
precision::bits { mantissa: 16, exponent: 8 }
precision::ulp { max_error: 2 }  // Max ULP error allowed
```

### 4.4 Redundancy

Controls fault tolerance:

```skalp
// Values
redundancy::none        // No redundancy
redundancy::tmr         // Triple Modular Redundancy
redundancy::dmr         // Dual Modular Redundancy
redundancy::ecc         // Error-correcting codes

// Parameterized
redundancy::nmr { copies: 5 }    // N-modular redundancy
redundancy::coded { scheme: "hamming" }
```

### 4.5 Power

Controls power optimization:

```skalp
// Values
power::full             // No power constraints
power::low              // Low power mode
power::clock_gated      // Enable clock gating
power::power_gated      // Enable power gating

// Parameterized
power::budget { milliwatts: 100 }
power::dvfs { min_voltage: 0.8 }
```

### 4.6 Memory

Controls memory implementation:

```skalp
// Values
memory::registers       // Implement in registers
memory::lutram          // Implement in LUT RAM
memory::bram            // Implement in Block RAM
memory::external        // External memory
memory::auto            // Compiler decides

// Parameterized
memory::bram { ports: 2 }
memory::cached { levels: 2 }
```

### 4.7 Arithmetic

Controls arithmetic implementation:

```skalp
// Values
arithmetic::ieee754     // Strict IEEE 754 compliance
arithmetic::fast_math   // Allow optimizations (associativity, etc.)
arithmetic::posit       // Posit arithmetic
arithmetic::fixed_point // Fixed-point arithmetic

// Parameterized
arithmetic::custom { rounding: "round_to_nearest_even" }
```

### 4.8 Constraints

Hard constraints that must be met:

```skalp
// Values and parameters
constraints::latency { max_cycles: 4 }
constraints::area { max_luts: 1000 }
constraints::frequency { min_mhz: 200 }
constraints::throughput { min_ops_per_cycle: 1 }
```

### 4.9 Preferences

Soft preferences for optimization direction:

```skalp
// Values
preferences::optimize_latency    // Prioritize low latency
preferences::optimize_area       // Prioritize small area
preferences::optimize_throughput // Prioritize high throughput
preferences::optimize_power      // Prioritize low power
preferences::balanced           // Balanced optimization
```

---

## 5. Stdlib Intent Definitions

All "standard" intents are defined in the stdlib, not hardcoded in the compiler.

### 5.1 File: `skalp/intent/throughput.sk`

```skalp
/// High-throughput intent for maximum operations per cycle.
/// Uses dedicated hardware to ensure single-cycle execution.
pub intent throughput {
    resource: dedicated,
    timing: single_cycle,
    preferences: optimize_throughput,
}
```

### 5.2 File: `skalp/intent/area.sk`

```skalp
/// Area-efficient intent for minimal hardware usage.
/// May share resources and accept multi-cycle operations.
pub intent area_efficient {
    resource: shared,
    timing: multi_cycle,
    preferences: optimize_area,
}
```

### 5.3 File: `skalp/intent/simd.sk`

```skalp
/// SIMD-friendly intent for partitionable operations.
/// Operations can be packed into SIMD units when data types allow.
pub intent simd_friendly {
    resource: simd,
    timing: single_cycle,
    arithmetic: fast_math,
}

/// 4-way SIMD for FP8/INT8 operations
pub intent simd_4way {
    resource: simd { partitions: 4 },
    timing: single_cycle,
    precision: reduced,
}

/// 2-way SIMD for FP16/INT16 operations
pub intent simd_2way {
    resource: simd { partitions: 2 },
    timing: single_cycle,
}
```

### 5.4 File: `skalp/intent/latency.sk`

```skalp
/// Low-latency intent for timing-critical paths.
/// Uses combinational logic and dedicated resources.
pub intent low_latency {
    resource: dedicated,
    timing: combinational,
    preferences: optimize_latency,
}

/// Fixed latency constraint
pub intent latency_4 {
    timing: latency { cycles: 4 },
}
```

### 5.5 File: `skalp/intent/power.sk`

```skalp
/// Low-power intent with aggressive gating.
pub intent low_power {
    power: clock_gated,
    resource: shared,
    preferences: optimize_power,
}

/// Ultra-low-power with power gating
pub intent ultra_low_power {
    power: power_gated,
    resource: shared,
    timing: multi_cycle,
    preferences: optimize_power,
}
```

### 5.6 File: `skalp/intent/safety.sk`

```skalp
/// Safety-critical intent with TMR.
pub intent safety_critical {
    redundancy: tmr,
    arithmetic: ieee754,
    precision: full,
}

/// Automotive ASIL-D compliant
pub intent asil_d {
    redundancy: tmr,
    arithmetic: ieee754,
    precision: full,
    power: full,  // No power gating that could cause glitches
}
```

### 5.7 File: `skalp/intent/precision.sk`

```skalp
/// Full IEEE 754 precision
pub intent ieee_compliant {
    arithmetic: ieee754,
    precision: full,
}

/// Allow fast math optimizations
pub intent fast_math {
    arithmetic: fast_math,
    precision: reduced,
}
```

### 5.8 Stdlib Directory Structure

```
skalp-stdlib/
├── skalp/
│   ├── intent/
│   │   ├── mod.sk          // Re-exports all intents
│   │   ├── throughput.sk
│   │   ├── area.sk
│   │   ├── simd.sk
│   │   ├── latency.sk
│   │   ├── power.sk
│   │   ├── safety.sk
│   │   └── precision.sk
│   └── ...
```

---

## 6. Custom Intent Creation

Users can define custom intents using the same mechanism as stdlib.

### 6.1 Basic Custom Intent

```skalp
// In user's project: intents.sk

/// Custom intent for our specific FPGA target
intent our_fpga_optimized {
    resource: simd { partitions: 2 },
    timing: pipelined { ii: 2 },
    memory: bram,
    preferences: balanced,
}
```

### 6.2 Intent Composition

```skalp
// Compose existing intents
use skalp::intent::{throughput, low_power};

/// Balanced intent combining throughput with power awareness
intent balanced_performance {
    // Start with throughput settings
    ..throughput,

    // Override power settings
    power: clock_gated,
}
```

### 6.3 Parameterized Intents

```skalp
/// Parameterized SIMD intent
intent simd<const N: nat> {
    resource: simd { partitions: N },
    timing: single_cycle,
}

// Usage
let result = (a * b) with intent::simd<4>;
```

### 6.4 Conditional Intents

```skalp
/// Intent that varies by compile-time condition
intent adaptive<const TARGET: Target> {
    resource: if TARGET == Target::FPGA {
        dedicated
    } else {
        shared
    },

    timing: if TARGET == Target::ASIC {
        pipelined { ii: 1 }
    } else {
        single_cycle
    },
}
```

### 6.5 Target-Specific Backend Hooks

For custom intents, users can provide target-specific mappings:

```skalp
// Define the intent
intent our_custom_multiply {
    resource: dedicated,
    timing: single_cycle,

    // Backend hooks - called during code generation
    #[backend(xilinx)]
    fn instantiate() -> String {
        // Return Xilinx-specific primitive instantiation
        "DSP48E2 #(.USE_MULT(\"MULTIPLY\")) dsp_inst (...)"
    }

    #[backend(asic)]
    fn instantiate() -> String {
        // Return ASIC-specific implementation
        "mul_dedicated_24x24 mul_inst (...)"
    }
}
```

---

## 7. Target Backend Mapping

### 7.1 Backend Responsibility

The backend is responsible for mapping primitive properties to actual hardware:

```rust
trait TargetBackend {
    /// Map primitive properties to hardware implementation
    fn map_operation(&self, op: Operation, primitives: &PrimitiveSet) -> HardwareUnit;

    /// Check if primitive combination is supported
    fn supports(&self, primitives: &PrimitiveSet) -> bool;

    /// Estimate resources for primitive combination
    fn estimate_resources(&self, primitives: &PrimitiveSet) -> ResourceEstimate;
}
```

### 7.2 Xilinx Backend Example

```rust
impl TargetBackend for XilinxBackend {
    fn map_operation(&self, op: Operation, primitives: &PrimitiveSet) -> HardwareUnit {
        match (op, &primitives.resource, &primitives.timing) {
            // Dedicated single-cycle multiply → DSP48
            (Mul, Resource::Dedicated, Timing::SingleCycle) => {
                HardwareUnit::Dsp48 {
                    mode: DspMode::Multiply,
                    pipeline_stages: 0,
                }
            }

            // SIMD 4-way multiply → Use DSP48 SIMD mode
            (Mul, Resource::Simd { partitions: 4 }, _) => {
                HardwareUnit::Dsp48Simd {
                    mode: DspMode::Simd4x12bit,
                }
            }

            // Shared multiply → Multiplexed DSP with arbiter
            (Mul, Resource::Shared, Timing::MultiCycle) => {
                HardwareUnit::SharedDsp {
                    arbiter: true,
                    max_users: 4,
                }
            }

            // ... other mappings
        }
    }
}
```

### 7.3 ASIC Backend Example

```rust
impl TargetBackend for AsicBackend {
    fn map_operation(&self, op: Operation, primitives: &PrimitiveSet) -> HardwareUnit {
        match (op, &primitives.resource, &primitives.timing) {
            // Dedicated single-cycle multiply → Parallel multiplier
            (Mul, Resource::Dedicated, Timing::SingleCycle) => {
                HardwareUnit::ParallelMultiplier {
                    width: self.default_width,
                }
            }

            // Pipelined multiply → Booth multiplier with stages
            (Mul, Resource::Dedicated, Timing::Pipelined { ii }) => {
                HardwareUnit::BoothMultiplier {
                    stages: self.frequency_to_stages(*ii),
                }
            }

            // SIMD multiply → Partitionable multiplier array
            (Mul, Resource::Simd { partitions }, _) => {
                HardwareUnit::PartitionableMultiplier {
                    partitions: *partitions,
                }
            }

            // ... other mappings
        }
    }
}
```

### 7.4 Backend Warnings and Errors

When primitives cannot be satisfied:

```rust
fn validate_primitives(&self, primitives: &PrimitiveSet) -> Result<(), BackendError> {
    // Check for unsatisfiable combinations
    if primitives.timing == Timing::SingleCycle
        && primitives.resource == Resource::Shared
    {
        return Err(BackendError::Conflict {
            message: "Cannot guarantee single-cycle with shared resources",
            suggestion: "Use timing::multi_cycle or resource::dedicated",
        });
    }

    // Check for unsupported features
    if primitives.redundancy == Redundancy::Tmr && !self.supports_tmr {
        return Err(BackendError::Unsupported {
            feature: "TMR",
            target: self.name(),
        });
    }

    Ok(())
}
```

---

## 8. Hardware Context: Dedicated vs SIMD Multipliers

Understanding the hardware difference motivates the intent system.

### 8.1 Dedicated Multipliers

A dedicated multiplier is a full-width unit for a specific operation:

```
Dedicated 24x24 Multiplier:
┌──────────────────────────────┐
│          24x24               │
│        Multiplier            │
│                              │
│   A[23:0] ───┐   ┌─── Result │
│              │   │   [47:0]  │
│   B[23:0] ───┴───┘           │
└──────────────────────────────┘
```

**Characteristics:**
- Full precision for FP32 mantissa multiplication
- Single-cycle result
- One operation per cycle
- Higher area cost

### 8.2 SIMD/Partitionable Multipliers

A partitionable multiplier can operate in multiple modes:

```
Partitionable 24-bit Multiplier:

Mode 1: 1x24x24 (FP32)
┌──────────────────────────────┐
│          24x24               │
│        Multiplier            │
└──────────────────────────────┘

Mode 2: 2x12x12 (FP16 or 2x INT12)
┌──────────────┬───────────────┐
│    12x12     │     12x12     │
│    Mult A    │     Mult B    │
└──────────────┴───────────────┘

Mode 3: 4x6x6 (FP8 or 4x INT6)
┌───────┬───────┬───────┬───────┐
│ 6x6   │ 6x6   │ 6x6   │ 6x6   │
│ M0    │ M1    │ M2    │ M3    │
└───────┴───────┴───────┴───────┘
```

**Characteristics:**
- Can pack multiple smaller operations
- Higher throughput for smaller data types
- Same silicon area as dedicated
- Requires mode control logic

### 8.3 When to Use Each

| Use Case | Recommended Intent |
|----------|-------------------|
| FP32 operations on critical path | `intent::throughput` |
| Many independent FP16 operations | `intent::simd_2way` |
| Batch processing of FP8 data | `intent::simd_4way` |
| Area-constrained design | `intent::area_efficient` |
| Mixed precision neural network | `intent::simd_friendly` |

### 8.4 Example: FP32 vs FP16 Processing

```skalp
// FP32 - needs dedicated multiplier
fn process_fp32(a: fp32, b: fp32) -> fp32 {
    // Intent: use dedicated for full precision
    (a * b) with intent::throughput
}

// FP16 - can use SIMD for 2x throughput
fn process_fp16_pair(a: (fp16, fp16), b: (fp16, fp16)) -> (fp16, fp16) {
    // Intent: use SIMD mode for parallelism
    with intent::simd_2way {
        (a.0 * b.0, a.1 * b.1)
    }
}
```

---

## 9. Complete Examples

### 9.1 Image Processing Pipeline

```skalp
use skalp::intent::{throughput, simd_4way, low_power};

entity ImageProcessor {
    in pixels: stream<rgba8>
    out processed: stream<rgba8>
}

// Most processing uses SIMD for 4 channels
impl ImageProcessor with intent::simd_4way {
    flow {
        pixels
        |> brightness_adjust()
        |> contrast_adjust()
        // Color conversion needs full precision
        |> (color_transform() with intent::throughput)
        |> output_stage()
        => processed
    }
}

// SIMD-friendly brightness: operates on 4 channels simultaneously
fn brightness_adjust(px: rgba8) -> rgba8 {
    // All 4 channel multiplies happen in one SIMD op
    px * brightness_factor
}

// Color transform needs higher precision
fn color_transform(px: rgba8) -> rgba8 with intent::throughput {
    // Uses dedicated multipliers for matrix multiply
    let r = px.r * matrix[0][0] + px.g * matrix[0][1] + px.b * matrix[0][2];
    let g = px.r * matrix[1][0] + px.g * matrix[1][1] + px.b * matrix[1][2];
    let b = px.r * matrix[2][0] + px.g * matrix[2][1] + px.b * matrix[2][2];
    rgba8 { r, g, b, a: px.a }
}
```

### 9.2 Neural Network Accelerator

```skalp
use skalp::intent::{simd_4way, throughput, area_efficient};

entity ConvLayer<const IN_CH: nat, const OUT_CH: nat> {
    in features: tensor<fp8, [IN_CH, H, W]>
    in weights: tensor<fp8, [OUT_CH, IN_CH, 3, 3]>
    out output: tensor<fp8, [OUT_CH, H, W]>
}

impl ConvLayer with intent::simd_4way {
    // Most operations use 4-way SIMD for FP8
    let conv_result = convolve(features, weights);

    // Accumulation needs higher precision
    let accumulated = with intent::throughput {
        reduce_sum(conv_result)  // Uses dedicated FP32 adder
    };

    // Activation can use SIMD again
    output = relu(accumulated);
}
```

### 9.3 Safety-Critical System

```skalp
use skalp::intent::{safety_critical, throughput};

entity BrakeController {
    in sensor_a: fp32
    in sensor_b: fp32
    in sensor_c: fp32
    out brake_force: fp32
}

impl BrakeController with intent::safety_critical {
    // All computations use TMR
    // Each operation is triplicated with voting

    let avg = (sensor_a + sensor_b + sensor_c) / 3.0;
    let force = calculate_brake_force(avg);

    // Output stage still uses TMR
    brake_force = force;
}

// Explicitly mark non-critical diagnostics
fn log_diagnostics(value: fp32) with intent::area_efficient {
    // This can use shared resources, no TMR needed
    diagnostic_buffer.push(value);
}
```

### 9.4 Custom Intent for Specific Application

```skalp
// Define application-specific intent
intent karythra_alu_decode {
    // Parallel mux for instruction decode
    mux_style: parallel,
    // Timing critical
    timing: single_cycle,
    // Full precision
    precision: full,
}

entity InstructionDecoder {
    in opcode: bit<8>
    out alu_op: AluOp
}

impl InstructionDecoder with intent::karythra_alu_decode {
    // Match generates parallel mux (OR of ANDs)
    alu_op = match opcode {
        0x00 => AluOp::Add,
        0x01 => AluOp::Sub,
        0x02 => AluOp::Mul,
        0x03 => AluOp::Div,
        // ... more opcodes
        _ => AluOp::Nop,
    };
}
```

---

## 10. Implementation Plan

### 10.1 Phase 1: Syntax and Parsing

1. Add `with intent::name` syntax to parser
2. Implement intent composition (`+`)
3. Parse intent definitions in `.sk` files

### 10.2 Phase 2: Intent Resolution

1. Load stdlib intent definitions
2. Implement intent lookup and resolution
3. Expand intents to primitive sets

### 10.3 Phase 3: Compiler Integration

1. Thread primitives through HIR → MIR
2. Implement primitive-aware operation lowering
3. Add validation for conflicting primitives

### 10.4 Phase 4: Backend Mapping

1. Define `TargetBackend` trait
2. Implement Xilinx backend mapping
3. Implement generic RTL backend mapping

### 10.5 Phase 5: Stdlib Population

1. Create stdlib intent directory structure
2. Define core intents (throughput, area, simd, etc.)
3. Document all stdlib intents

### 10.6 Phase 6: Documentation and Testing

1. Write user guide for intent system
2. Create comprehensive test suite
3. Add intent-related error messages

---

## Summary

The intent system provides:

| Feature | Benefit |
|---------|---------|
| `with intent::name` syntax | Explicit, readable code |
| Stdlib-defined intents | Extensible without compiler changes |
| Primitive properties | Clear compiler/backend separation |
| Intent composition | Flexible customization |
| Custom intents | Application-specific optimization |

The key architectural insight is the **separation of concerns**:

1. **User code** uses high-level intent names
2. **Intent definitions** map names to primitives (in stdlib)
3. **Compiler** only understands primitives
4. **Backend** maps primitives to hardware

This design ensures:
- No special-casing in the compiler
- Easy addition of new intents
- Clear semantics for custom intents
- Target-independent intent specifications

---

## Appendix: Migration from Attribute Syntax

### Previous Syntax (Deprecated)

The previous implementation used attribute syntax for synthesis intents:

```skalp
// OLD - deprecated
#[impl_style::parallel]
let tmp = a + b;

#[parallel]
match sel { ... }
```

### New Unified Syntax

The new unified syntax uses `with intent::`:

```skalp
// NEW - current syntax
let tmp = (a + b) with intent::impl_style_parallel;

// Or use block form
with intent::parallel {
    let result = match sel { ... };
}
```

### Migration Guide

| Old Syntax | New Syntax |
|------------|------------|
| `#[impl_style::parallel]` | `with intent::impl_style_parallel` |
| `#[impl_style::tree]` | `with intent::impl_style_tree` |
| `#[impl_style::sequential]` | `with intent::impl_style_sequential` |
| `#[parallel]` | `with intent::parallel` |
| `#[priority]` | `with intent::priority` |

### What Stays as Attributes

Attributes (`#[...]`) are now reserved for **metadata annotations** that don't affect synthesis:

- `#[safety_mechanism(type = ecc)]` - Safety metadata
- `#[detection_signal]` - Safety signal marking
- `#[memory(depth = 1024)]` - Memory configuration
- `#[pipeline(stages = 3)]` - Pipeline configuration (metadata)
- `#[unroll]` / `#[unroll(N)]` - Loop unroll directives

### Rationale

The separation provides clarity:

1. **`with intent::name`** - Synthesis intent that affects how hardware is generated
2. **`#[attribute]`** - Metadata/annotation that doesn't change hardware structure

This distinction makes code more readable and intentions clearer.
