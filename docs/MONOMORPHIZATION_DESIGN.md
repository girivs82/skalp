# Monomorphization Engine Design

**Phase 7 of Parametric Types Implementation**
**Date:** 2025-10-11

## Overview

The monomorphization engine transforms generic entities with type parameters into concrete, specialized instances. This is a critical compiler pass that enables parametric types (`fp<F>`, `fixed<W,F,S>`, `vec<T,N>`) and intent-driven optimization to function.

## Architecture

### Pipeline Position

```
Source Code
    ↓
Parser → AST
    ↓
HIR Builder → HIR (with generics)
    ↓
Type Checking
    ↓
[NEW] Monomorphization Pass → Monomorphic HIR
    ↓
HIR to MIR → MIR
    ↓
MIR Optimizations (CDC, const folding, etc.)
    ↓
MIR to LIR → LIR
    ↓
Code Generation → SystemVerilog/Verilog/VHDL
```

### Key Insight

Monomorphization happens **after type checking but before MIR generation**. This allows:
1. Type checker validates generic constraints
2. Monomorphizer creates concrete instances
3. MIR generator works only with concrete types (no generics in MIR)

## Components

### 1. Const Expression Evaluator

**Purpose:** Evaluate compile-time constant expressions to concrete values.

**Location:** `crates/skalp-frontend/src/const_eval.rs` (new file)

**Capabilities:**
- Arithmetic: `N + 1`, `W * 2`, `clog2(N)`
- Field access: `F.total_bits`, `F.exponent_bits`
- Conditional: `if condition { a } else { b }`
- Function calls: Built-in const functions like `clog2()`, `max()`, `min()`
- Type queries: `T::TOTAL_BITS`, `T::IS_SIGNED`

**Data Structures:**
```rust
/// Const expression evaluation result
pub enum ConstValue {
    Nat(usize),
    Int(i64),
    Bool(bool),
    String(String),
    FloatFormat(FloatFormatValue),
    Struct(HashMap<String, ConstValue>),
}

/// Float format const value
pub struct FloatFormatValue {
    pub total_bits: usize,
    pub exponent_bits: usize,
    pub mantissa_bits: usize,
    pub bias: i64,
    pub name: String,
}

/// Const expression evaluator
pub struct ConstEvaluator {
    /// Symbol table for const bindings
    const_bindings: HashMap<String, ConstValue>,
    /// Built-in const functions
    builtin_fns: HashMap<String, BuiltinConstFn>,
}
```

**Built-in Functions:**
```rust
fn clog2(n: usize) -> usize {
    if n <= 1 { 0 } else { (n - 1).ilog2() as usize + 1 }
}

fn is_power_of_2(n: usize) -> bool {
    n > 0 && (n & (n - 1)) == 0
}

fn max(a: usize, b: usize) -> usize { a.max(b) }
fn min(a: usize, b: usize) -> usize { a.min(b) }
```

### 2. Instantiation Collector

**Purpose:** Find all generic entity/impl instantiations in the HIR.

**Location:** `crates/skalp-frontend/src/monomorphization/collector.rs` (new file)

**Algorithm:**
1. Walk the HIR starting from top-level entities
2. When encountering `ModuleInstance` with generic entity:
   - Record the entity name
   - Record the concrete type arguments
   - Record the const parameter values
3. Recursively visit instantiated entities (for transitive dependencies)

**Data Structures:**
```rust
/// Generic instantiation record
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct Instantiation {
    /// Name of generic entity/impl
    pub entity_name: String,
    /// Concrete type arguments (T -> fp32, etc.)
    pub type_args: HashMap<String, HirType>,
    /// Const parameter values (N -> 8, F -> IEEE754_32)
    pub const_args: HashMap<String, ConstValue>,
    /// Intent parameter values (I -> FAST_INTENT)
    pub intent_args: HashMap<String, IntentValue>,
}

/// Instantiation collector
pub struct InstantiationCollector {
    /// All discovered instantiations
    instantiations: HashSet<Instantiation>,
    /// Const evaluator
    evaluator: ConstEvaluator,
}
```

### 3. Monomorphization Engine

**Purpose:** Generate concrete entity/impl definitions from generic ones.

**Location:** `crates/skalp-frontend/src/monomorphization/engine.rs` (new file)

**Algorithm:**
```rust
fn monomorphize(hir: &Hir) -> MonomorphicHir {
    // 1. Collect all instantiations
    let collector = InstantiationCollector::new();
    let instantiations = collector.collect_instantiations(hir);

    // 2. For each instantiation, generate concrete entity
    let mut concrete_entities = Vec::new();
    for inst in instantiations {
        let generic_entity = hir.find_entity(&inst.entity_name);
        let concrete_entity = specialize_entity(
            generic_entity,
            &inst.type_args,
            &inst.const_args,
            &inst.intent_args
        );
        concrete_entities.push(concrete_entity);
    }

    // 3. Update module instances to use concrete entity names
    let updated_instances = update_module_instances(hir, &instantiations);

    // 4. Build new HIR with concrete entities only
    MonomorphicHir {
        entities: concrete_entities,
        instances: updated_instances,
    }
}
```

**Specialization Process:**
```rust
fn specialize_entity(
    generic: &HirEntity,
    type_args: &HashMap<String, HirType>,
    const_args: &HashMap<String, ConstValue>,
    intent_args: &HashMap<String, IntentValue>,
) -> HirEntity {
    // 1. Generate specialized name
    let specialized_name = generate_specialized_name(generic.name, type_args, const_args);

    // 2. Substitute type parameters
    let specialized_ports = substitute_types_in_ports(&generic.ports, type_args);

    // 3. Substitute const parameters in expressions
    let specialized_body = substitute_consts_in_body(&generic.body, const_args);

    // 4. Evaluate intent-based conditionals
    let specialized_body = evaluate_intent_conditionals(&specialized_body, intent_args);

    // 5. Create concrete entity
    HirEntity {
        name: specialized_name,
        generics: vec![], // No generics in specialized version
        ports: specialized_ports,
        body: specialized_body,
    }
}
```

### 4. Intent Evaluator

**Purpose:** Evaluate intent-based conditional expressions at compile time.

**Location:** `crates/skalp-frontend/src/monomorphization/intent_eval.rs` (new file)

**Algorithm:**
```rust
fn evaluate_intent_conditionals(
    body: &HirBlock,
    intent: &IntentValue,
) -> HirBlock {
    walk_expressions(body, |expr| {
        match expr {
            // if I.optimize == "latency" { a } else { b }
            HirExpression::If { condition, then_branch, else_branch } => {
                if is_intent_condition(condition) {
                    let result = evaluate_intent_condition(condition, intent);
                    if result {
                        return then_branch.clone();
                    } else if let Some(else_b) = else_branch {
                        return else_b.clone();
                    }
                }
            }
            _ => {}
        }
        expr
    })
}

fn is_intent_condition(expr: &HirExpression) -> bool {
    // Check if expression references intent parameter (I.optimize, I.latency, etc.)
    matches!(expr, HirExpression::FieldAccess { object, field }
        if is_intent_param(object))
}
```

### 5. Name Mangling

**Purpose:** Generate unique names for specialized entities.

**Scheme:**
```
Original: FpAdd<F>
Specialized: FpAdd_fp32

Original: MatMul<M, N, P, F, I>
Specialized: MatMul_4_4_4_fp32_FAST

Original: FFT<1024, IEEE754_32, FAST_INTENT>
Specialized: FFT_1024_fp32_FAST
```

**Implementation:**
```rust
fn generate_specialized_name(
    base_name: &str,
    type_args: &HashMap<String, HirType>,
    const_args: &HashMap<String, ConstValue>,
) -> String {
    let mut parts = vec![base_name.to_string()];

    // Add const args in order
    for (_, value) in const_args.iter() {
        parts.push(mangle_const_value(value));
    }

    // Add type args
    for (_, ty) in type_args.iter() {
        parts.push(mangle_type(ty));
    }

    parts.join("_")
}

fn mangle_type(ty: &HirType) -> String {
    match ty {
        HirType::Named(name) => name.clone(),
        HirType::Bit(width) => format!("bit{}", width),
        HirType::FpParametric { format } => {
            // Evaluate format to get name
            "fp32".to_string()  // Example
        }
        _ => "generic".to_string()
    }
}
```

## Example Transformations

### Example 1: Simple Generic Entity

**Input HIR:**
```skalp
entity FpAdd<const F: FloatFormat> {
    in a: fp<F>
    in b: fp<F>
    out result: fp<F>
}

impl<const F: FloatFormat> FpAdd<F> {
    result = fp_add::<F>(a, b)
}

// Instantiation
entity Top {
    let adder = FpAdd<IEEE754_32> { ... }
}
```

**Monomorphization Output:**
```skalp
// Specialized entity (no generics)
entity FpAdd_fp32 {
    in a: bit<32>
    in b: bit<32>
    out result: bit<32>
}

impl FpAdd_fp32 {
    result = fp_add_fp32(a, b)  // Also monomorphized
}

// Updated instance
entity Top {
    let adder = FpAdd_fp32 { ... }
}
```

### Example 2: Intent-Driven Selection

**Input HIR:**
```skalp
entity Sqrt<const F: FloatFormat, intent I: Intent> {
    in x: fp<F>
    out result: fp<F>
}

impl<const F, intent I> Sqrt<F, I> {
    result = if I.optimize == "latency" {
        lut_sqrt::<F>(x)
    } else if I.optimize == "area" {
        nr_sqrt::<F>(x, 1)
    } else {
        nr_sqrt::<F>(x, 2)
    }
}

// Instantiations
entity TopFast {
    let s = Sqrt<IEEE754_32, FAST_INTENT> { ... }
}

entity TopSmall {
    let s = Sqrt<IEEE754_32, SMALL_INTENT> { ... }
}
```

**Monomorphization Output:**
```skalp
// Fast version (I.optimize == "latency" evaluated to true)
entity Sqrt_fp32_FAST {
    in x: bit<32>
    out result: bit<32>
}

impl Sqrt_fp32_FAST {
    result = lut_sqrt_fp32(x)  // Only this branch kept
}

// Small version (I.optimize == "area" evaluated to true)
entity Sqrt_fp32_SMALL {
    in x: bit<32>
    out result: bit<32>
}

impl Sqrt_fp32_SMALL {
    result = nr_sqrt_fp32(x, 1)  // Only this branch kept
}

// Updated instances
entity TopFast {
    let s = Sqrt_fp32_FAST { ... }
}

entity TopSmall {
    let s = Sqrt_fp32_SMALL { ... }
}
```

### Example 3: Const Expression Evaluation

**Input HIR:**
```skalp
entity Vec<T, const N: nat> {
    in data: [T; N]
    out sum: T
}

impl<T, const N> Vec<T, N>
where T: Numeric
{
    signal temp: [T; N+1]

    for i in 0..N {
        temp[i] = data[i]
    }
}

// Instantiation
entity Top {
    let v = Vec<fp32, 4> { ... }
}
```

**Monomorphization Output:**
```skalp
entity Vec_fp32_4 {
    in data: [bit<32>; 4]
    out sum: bit<32>
}

impl Vec_fp32_4 {
    signal temp: [bit<32>; 5]  // N+1 = 4+1 = 5

    // Loop unrolled
    temp[0] = data[0]
    temp[1] = data[1]
    temp[2] = data[2]
    temp[3] = data[3]
}

entity Top {
    let v = Vec_fp32_4 { ... }
}
```

## Implementation Plan

### Step 1: Const Expression Evaluator (Week 1)
- [ ] Create `const_eval.rs` module
- [ ] Implement `ConstValue` enum
- [ ] Implement arithmetic evaluation
- [ ] Implement field access (F.total_bits)
- [ ] Implement built-in functions (clog2, etc.)
- [ ] Add tests

### Step 2: Instantiation Collector (Week 1)
- [ ] Create `monomorphization/collector.rs`
- [ ] Implement HIR walker
- [ ] Collect generic instantiations
- [ ] Handle transitive dependencies
- [ ] Add tests

### Step 3: Monomorphization Engine (Week 2)
- [ ] Create `monomorphization/engine.rs`
- [ ] Implement type substitution
- [ ] Implement const substitution
- [ ] Implement name mangling
- [ ] Generate concrete entities
- [ ] Update module instances
- [ ] Add tests

### Step 4: Intent Evaluator (Week 2)
- [ ] Create `monomorphization/intent_eval.rs`
- [ ] Detect intent-based conditionals
- [ ] Evaluate intent field access
- [ ] Perform dead code elimination for non-selected branches
- [ ] Add tests

### Step 5: Integration (Week 3)
- [ ] Add monomorphization pass to compiler pipeline
- [ ] Update `MirCompiler` to call monomorphization before HIR→MIR
- [ ] Update error messages to include specialized names
- [ ] Integration tests with real examples
- [ ] Performance optimization

### Step 6: Documentation (Week 3)
- [ ] Document monomorphization algorithm
- [ ] Add debugging output (--dump-mono flag)
- [ ] Error messages for monomorphization failures
- [ ] Tutorial on generic programming

## Testing Strategy

### Unit Tests
1. **Const Evaluator Tests:**
   - Arithmetic: `2 + 3 == 5`
   - Field access: `IEEE754_32.total_bits == 32`
   - Functions: `clog2(1024) == 10`

2. **Instantiation Collector Tests:**
   - Single instantiation
   - Multiple instantiations of same entity
   - Transitive dependencies

3. **Monomorphization Tests:**
   - Type substitution
   - Const substitution
   - Name mangling
   - Intent evaluation

### Integration Tests
1. **Real Examples:**
   - FFT with different sizes
   - MatMul with different dimensions
   - Sqrt with different intents
   - Video pipeline

2. **Edge Cases:**
   - Recursive generic entities
   - Generic entities instantiating other generics
   - Default parameter values

## Error Handling

### Compile Errors
1. **Const Evaluation Failure:**
   ```
   error: cannot evaluate const expression at compile time
     --> examples/fft.sk:12:5
      |
   12 |     signal temp: [T; N * M]
      |                      ^^^^^ expression depends on runtime value
   ```

2. **Intent Field Not Found:**
   ```
   error: intent has no field `unknown_field`
     --> examples/sqrt.sk:8:12
      |
   8  |     if I.unknown_field == "value" {
      |            ^^^^^^^^^^^^^ not a field of Intent
   ```

3. **Type Argument Mismatch:**
   ```
   error: wrong number of type arguments
     --> examples/top.sk:5:10
      |
   5  |     let a = FpAdd<fp32, fp16> { ... }
      |              ^^^^^^^^^^^^^^^^ expected 1 type argument, found 2
   ```

## Performance Considerations

### Compilation Time
- Monomorphization can increase compilation time for heavily generic code
- Mitigation: Cache monomorphized entities across compilation units

### Code Size
- Each instantiation creates a new entity (code duplication)
- Mitigation: Detect identical instantiations and reuse

### Optimization
- Intent-driven dead code elimination reduces final code size
- Loop unrolling with const parameters enables better optimization

## Future Extensions

1. **Partial Monomorphization:**
   - Keep some parameters generic if not all are known
   - Allows library compilation without full instantiation

2. **Cross-Module Monomorphization:**
   - Share monomorphized entities across compilation units
   - Require module system and linking

3. **Monomorphization Hints:**
   - User annotations to control specialization
   - `#[mono_inline]`, `#[mono_cache]`, etc.

4. **Profile-Guided Monomorphization:**
   - Use synthesis results to guide optimization
   - Iterative refinement of intent profiles

## Conclusion

The monomorphization engine is the critical infrastructure that makes parametric types functional. It transforms generic, intent-driven code into concrete, optimized hardware implementations.

**Key Benefits:**
- ✅ Type-safe generic programming
- ✅ Zero runtime overhead (all resolved at compile time)
- ✅ Intent-driven optimization with dead code elimination
- ✅ Single source, multiple optimized implementations

**Timeline:** 3 weeks
**Dependencies:** Phases 1-6 (Complete ✅)
**Enables:** Phases 8-10 (Stdlib migration, testing, docs)
