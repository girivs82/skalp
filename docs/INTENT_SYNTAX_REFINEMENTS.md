# Intent Syntax Refinements

> **DEPRECATION NOTICE**: The attribute-based syntax (`#[foo]`) for synthesis intents has been
> unified with the `with intent::name` syntax. See [INTENT_SYSTEM_COMPREHENSIVE_DESIGN.md](INTENT_SYSTEM_COMPREHENSIVE_DESIGN.md)
> for the current syntax specification.
>
> **Key change**: Use `expr with intent::name` instead of `#[name] expr`.
> Attributes (`#[...]`) are now reserved for metadata annotations (safety, memory config, etc.),
> not synthesis intents.

---

**Ergonomic Syntax for Common Intent Patterns**

This document describes syntactic sugar and shorthand forms for the intent system, making common patterns more concise while maintaining the full power of intent blocks.

---

## 1. Design Principles

1. **Attributes are sugar for intents** - `#[foo]` desugars to intent application, not a separate mechanism
2. **Single-attribute intents don't need blocks** - Avoid ceremony for simple cases
3. **Composition is first-class** - Combine intents with `+` operator
4. **User-defined intents work as attributes** - No distinction between stdlib and user intents

---

## 2. Intent Declaration Forms

### 2.1 Block Form (Multi-Attribute)

For intents with multiple constraints:

```skalp
intent low_power {
    mux_style: priority,
    clock_gating: aggressive,
    pipeline: shallow,
}
```

### 2.2 Single-Line Form (Single-Attribute)

For intents with a single constraint:

```skalp
intent parallel = mux_style::parallel;
intent priority = mux_style::priority;
intent critical = timing::critical_path;
intent pipelined = pipeline::enabled;
```

The `namespace::value` syntax clearly identifies the constraint domain.

### 2.3 Equivalence

These are semantically identical:

```skalp
// Block form
intent parallel {
    mux_style: parallel,
}

// Single-line form
intent parallel = mux_style::parallel;
```

---

## 3. Attribute Syntax (Sugar for Intent Application)

### 3.1 Basic Usage

Attributes apply intents to expressions, statements, or items:

```skalp
// Apply 'parallel' intent to this match expression
#[parallel]
match sel {
    0 => val_a,
    1 => val_b,
    2 => val_c,
    _ => val_default,
}
```

### 3.2 Desugaring

`#[foo]` looks up `intent foo` in scope and applies it:

```skalp
// User writes:
#[parallel]
match sel { ... }

// Desugars to (conceptually):
match sel with intent parallel { ... }
```

### 3.3 Multiple Attributes

Multiple attributes are applied in order:

```skalp
#[parallel]
#[critical]
match opcode { ... }

// Or combined:
#[parallel, critical]
match opcode { ... }
```

### 3.4 Scope of Application

Attributes can apply to different scopes:

```skalp
// Expression-level
let result = #[parallel] match sel { ... };

// Statement-level
#[pipelined]
for i in 0..N {
    process(data[i]);
}

// Module-level
#[low_power]
module sensor_hub {
    // All constructs in module inherit this intent
}

// Function-level
#[parallel]
fn decode(sel: bit[3]) -> bit[8] {
    match sel { ... }
}
```

---

## 4. Intent Composition

### 4.1 Plus Operator (`+`)

Combine intents with `+`, rightmost wins on conflicts:

```skalp
intent parallel = mux_style::parallel;
intent critical = timing::critical_path;

// Combine two intents
intent fast_path = parallel + critical;
```

### 4.2 Conflict Resolution

When intents have overlapping constraints, rightmost wins:

```skalp
intent a = mux_style::parallel;
intent b = mux_style::priority;

intent c = a + b;  // mux_style is priority (b wins)
intent d = b + a;  // mux_style is parallel (a wins)
```

### 4.3 Inline Composition at Use Site

```skalp
#[parallel + critical]
match opcode { ... }

// Extend existing intent inline
#[low_power + mux_style::parallel]
match sel { ... }
```

### 4.4 Spread Syntax for Extension

Extend an intent with overrides using spread `..`:

```skalp
intent low_power {
    mux_style: priority,
    clock_gating: aggressive,
    pipeline: shallow,
}

// Extend and override
intent low_power_fast {
    ..low_power,                  // Include all from low_power
    mux_style: parallel,          // Override mux_style
}
```

---

## 5. Standard Library Intents

The stdlib provides common intents:

```skalp
// Mux selection
intent parallel = mux_style::parallel;
intent priority = mux_style::priority;

// Timing
intent critical = timing::critical_path;
intent relaxed = timing::relaxed;

// Pipeline
intent pipelined = pipeline::enabled;
intent combinational = pipeline::disabled;

// Power
intent low_power {
    clock_gating: aggressive,
    mux_style: priority,
    pipeline: shallow,
}

intent high_performance {
    mux_style: parallel,
    pipeline: deep,
    clock_gating: disabled,
}
```

---

## 6. User-Defined Intents as Attributes

User-defined intents are first-class and work as attributes:

```skalp
// User defines in their project:
intent karythra_decode {
    mux_style: parallel,
    timing: critical_path,
}

// Use as attribute - no different from stdlib intents:
#[karythra_decode]
match opcode {
    OP_ADD => ...,
    OP_SUB => ...,
    OP_MUL => ...,
}
```

---

## 7. Constraint Namespaces

Available constraint namespaces and values:

### 7.1 `mux_style`
```skalp
mux_style::parallel   // One-hot parallel mux (OR of ANDs)
mux_style::priority   // Priority encoder (cascaded ternary)
mux_style::auto       // Compiler chooses based on analysis
```

### 7.2 `timing`
```skalp
timing::critical_path // On critical timing path
timing::relaxed       // Not timing-critical
timing::dont_touch    // Preserve exact structure
```

### 7.3 `pipeline`
```skalp
pipeline::enabled     // Insert pipeline registers
pipeline::disabled    // Pure combinational
pipeline::auto        // Compiler decides
```

### 7.4 `clock_gating`
```skalp
clock_gating::aggressive  // Gate whenever possible
clock_gating::conservative // Gate only obvious cases
clock_gating::disabled    // No clock gating
```

### 7.5 `resource`
```skalp
resource::dsp         // Map to DSP blocks
resource::lut         // Map to LUTs
resource::bram        // Map to block RAM
resource::share       // Enable resource sharing
```

### 7.6 `pipeline_style`
```skalp
pipeline_style::auto          // Compiler decides based on timing analysis
pipeline_style::combinational // Fully combinational - no pipeline registers
pipeline_style::manual        // Explicit manual stages via |> operator
pipeline_style::retimed       // Auto-retiming - compiler inserts registers for target frequency
```

### 7.7 `unroll` (Loop Attribute)

The `#[unroll]` attribute controls compile-time loop expansion. Unlike namespace-based intents, `unroll` is used directly as an attribute on for loops.

```skalp
// Full unroll - completely expand the loop
#[unroll]
for i in 0..8 {
    result[i] = input[7 - i];  // Generates 8 parallel assignments
}

// Partial unroll - unroll by factor N
#[unroll(4)]
for i in 0..32 {
    acc = acc + data[i];  // Unrolls 4 iterations, loops 8 times
}
```

**Unroll behavior:**
- `#[unroll]` - Full unroll: expands all iterations at compile time
- `#[unroll(N)]` - Partial unroll: expands N iterations per loop iteration

### 7.8 `impl_style` (Implementation Selection)

Controls which implementation variant is selected for functions that have multiple valid implementations with different area/performance tradeoffs:

```skalp
impl_style::parallel      // Fully unrolled single-cycle (more area, lower latency)
impl_style::tree          // Parallel prefix tree (balanced area/latency) [default]
impl_style::sequential    // Multi-cycle iterative (minimal area, higher latency)
impl_style::auto          // Compiler decides based on context and timing constraints
```

**Usage:**

```skalp
// Force single-cycle parallel implementation
#[impl_style::parallel]
let count = popcount32(value);  // Generates 32-input OR tree

// Use compact tree implementation (default)
#[impl_style::tree]
let count = popcount32(value);  // Generates parallel prefix adder tree

// Allow multi-cycle for area-constrained designs
#[impl_style::sequential]
let count = popcount32(value);  // Generates FSM with counter
```

**Key insight:** The same function call should select different implementations based on intent, rather than requiring explicit function name variants like `popcount32_unrolled()`. This keeps user code clean while allowing the compiler to optimize based on design constraints.

**Applies to:** Standard library functions with multiple implementation strategies:
- `clz32`, `ctz32` - Leading/trailing zero count
- `popcount32` - Population count (Hamming weight)
- `bitreverse32` - Bit reversal
- `parity32` - XOR fold
- Other functions where area/latency tradeoffs exist

---

## 8. Complete Examples

### 8.1 Decoder with Parallel Mux

```skalp
intent parallel = mux_style::parallel;

fn decode(sel: bit[3]) -> bit[8] {
    #[parallel]
    match sel {
        0 => 0b00000001,
        1 => 0b00000010,
        2 => 0b00000100,
        3 => 0b00001000,
        4 => 0b00010000,
        5 => 0b00100000,
        6 => 0b01000000,
        7 => 0b10000000,
    }
}
```

**Generated SystemVerilog:**
```systemverilog
assign decode = ({8{sel==3'b000}} & 8'b00000001)
              | ({8{sel==3'b001}} & 8'b00000010)
              | ({8{sel==3'b010}} & 8'b00000100)
              | ({8{sel==3'b011}} & 8'b00001000)
              | ({8{sel==3'b100}} & 8'b00010000)
              | ({8{sel==3'b101}} & 8'b00100000)
              | ({8{sel==3'b110}} & 8'b01000000)
              | ({8{sel==3'b111}} & 8'b10000000);
```

### 8.2 Module with Composed Intents

```skalp
intent fast_decode {
    mux_style: parallel,
    timing: critical_path,
}

#[fast_decode]
module instruction_decoder {
    in opcode: bit[6]
    out alu_op: AluOp
    out mem_op: MemOp

    impl {
        // Both matches use parallel mux due to module-level intent
        alu_op = match opcode[5:3] { ... };
        mem_op = match opcode[2:0] { ... };
    }
}
```

### 8.3 Local Override

```skalp
#[parallel]
module decoder {
    in opcode: bit[8]
    out result: bit[32]

    impl {
        // Uses parallel from module
        let fast_path = match opcode[7:4] { ... };

        // Override to priority for complex conditions
        #[priority]
        let complex_path = match opcode {
            x if x > 100 => ...,  // Overlapping conditions need priority
            x if x > 50 => ...,
            _ => ...,
        };

        result = fast_path | complex_path;
    }
}
```

### 8.4 Unrolled Bit Reversal

```skalp
/// Reverse bits using fully unrolled loop
pub fn bit_reverse_8(input: bit[8]) -> bit[8] {
    let mut result = 0 as bit[8];
    #[unroll]
    for i in 0..8 {
        // Each iteration becomes a parallel assignment
        result = result | ((input >> i) & 1) << (7 - i);
    }
    return result;
}
```

### 8.5 Pipelined Flow with Manual Stages

```skalp
intent pipelined = pipeline_style::manual;

#[pipelined]
flow {
    input_data
    |> stage1_compute()   // Pipeline register inserted
    |> stage2_compute()   // Pipeline register inserted
    |> output
}
```

### 8.6 XOR Reduction with Unroll

```skalp
/// Compute parity (XOR fold) of 8-bit input
pub fn parity8(input: bit[8]) -> bit[1] {
    let mut result = 0 as bit[1];
    #[unroll]
    for i in 0..8 {
        result = result ^ ((input >> i) & 1) as bit[1];
    }
    return result;
}
```

---

## 9. Error Messages

### 9.1 Unknown Intent

```skalp
#[nonexistent]
match sel { ... }
```

```
error[E0451]: no intent named 'nonexistent' found
 --> src/decoder.sk:15:3
  |
15 |   #[nonexistent]
  |     ^^^^^^^^^^^ unknown intent
  |
  = help: available intents: parallel, priority, critical, low_power
  = help: define custom intent: `intent nonexistent = ...;`
```

### 9.2 Conflicting Intents

```skalp
#[parallel + priority]  // Both set mux_style
match sel { ... }
```

```
warning[W0312]: conflicting intent values for 'mux_style'
 --> src/decoder.sk:15:3
  |
15 |   #[parallel + priority]
  |     ^^^^^^^^^^^^^^^^^^^^^
  |
  = note: 'parallel' sets mux_style::parallel
  = note: 'priority' sets mux_style::priority
  = note: rightmost wins: using mux_style::priority
```

---

## 10. Implementation Notes

### 10.1 Parser Changes

1. Add single-line intent syntax: `intent name = namespace::value;`
2. Add attribute parsing: `#[name]`, `#[name, name]`, `#[name + name]`
3. Add spread syntax in intent blocks: `..other_intent`

### 10.2 Resolution

1. `#[foo]` → lookup `intent foo` in scope
2. If not found → error with suggestions
3. If found → apply intent constraints to AST node

### 10.3 Lowering

1. Intent constraints propagate through HIR → MIR → SIR
2. `mux_style::parallel` → generate `ParallelMux` in MIR
3. `mux_style::priority` → generate `PriorityMux` in MIR (default)

---

## Summary

| Syntax | Meaning |
|--------|---------|
| `intent foo { a: x, b: y }` | Multi-attribute intent block |
| `intent foo = ns::val;` | Single-attribute intent |
| `#[foo]` | Apply intent to next item |
| `#[foo, bar]` | Apply multiple intents |
| `#[foo + bar]` | Compose intents |
| `intent x { ..y, a: z }` | Extend intent with override |
| `#[unroll]` | Full loop unroll (expand all iterations) |
| `#[unroll(N)]` | Partial loop unroll (expand N iterations) |
| `pipeline_style::manual` | Explicit pipeline stages via `\|>` |
| `pipeline_style::retimed` | Auto-retiming for target frequency |
| `impl_style::parallel` | Force single-cycle unrolled implementation |
| `impl_style::tree` | Use parallel prefix tree (default) |
| `impl_style::sequential` | Use multi-cycle iterative (minimal area) |

The key insight: **attributes are not a separate feature—they're ergonomic syntax for the intent system.**
