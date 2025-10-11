# Parametric Types and Intent System - Implementation Plan

**Goal:** Implement unified parametric numeric types and intent-as-type for SKALP

---

## Overview

This plan implements two major features documented in LANGUAGE_SPECIFICATION.md §3.1.1 and §8:

1. **Parametric Numeric Types** - Unified fp/fixed/int types with compile-time specialization
2. **Intent as First-Class Type** - Intent type parameter for HLS optimization

---

## Phase 1: Type System Foundation (2-3 weeks)

### 1.1 Extend Parser for Const Generics

**Files to modify:**
- `crates/skalp-frontend/src/parse.rs`
- `crates/skalp-frontend/src/syntax.rs`

**Tasks:**
- [ ] Add `const` keyword to lexer
- [ ] Parse `<const N: nat>` generic parameters
- [ ] Parse `<const F: FloatFormat>` format parameters
- [ ] Parse `<intent I: Intent>` intent parameters
- [ ] Parse const generic constraints: `where FRAC <= WIDTH`

**Syntax to support:**
```skalp
entity Sqrt<const F: FloatFormat, intent I: Intent>
type fixed<const WIDTH: nat, const FRAC: nat, const SIGNED: bool>
type vec<T, const N: nat>
```

**Tests:**
```skalp
// Test files to create:
tests/parser/const_generics.sk
tests/parser/intent_params.sk
tests/parser/format_params.sk
```

### 1.2 HIR Representation

**Files to modify:**
- `crates/skalp-hir/src/lib.rs`
- `crates/skalp-hir/src/types.rs`

**Tasks:**
- [ ] Add `HirGenericParam` enum with `Type`, `Const`, `Intent` variants
- [ ] Extend `HirType` with parametric types:
  - `Fp { format: ConstExpr }`
  - `Fixed { width: ConstExpr, frac: ConstExpr, signed: ConstExpr }`
  - `Int { width: ConstExpr, signed: ConstExpr }`
  - `Vec { elem_type: Box<HirType>, dim: ConstExpr }`
- [ ] Add `HirIntent` type with fields from spec
- [ ] Add const expression evaluation

**Data structures:**
```rust
enum HirGenericParam {
    Type { name: Ident, bounds: Vec<TraitBound> },
    Const { name: Ident, ty: ConstType },
    Intent { name: Ident },
}

enum ConstType {
    Nat,
    Int,
    Bool,
    FloatFormat,
    Intent,
}

struct HirIntent {
    latency: Option<ConstExpr>,
    throughput: Option<ConstExpr>,
    accuracy: Option<Accuracy>,
    optimize: Optimize,
    // ...
}
```

### 1.3 Const Expression Evaluation

**Files to create:**
- `crates/skalp-hir/src/const_eval.rs`

**Tasks:**
- [ ] Implement const expression evaluator
- [ ] Support arithmetic: `+`, `-`, `*`, `/`, `%`
- [ ] Support comparisons: `<`, `<=`, `>`, `>=`, `==`
- [ ] Support logical: `&&`, `||`, `!`
- [ ] Support field access: `F.total_bits`, `F.mantissa_bits`
- [ ] Support function calls: `clog2(N)`, `min(a, b)`, `max(a, b)`

**Example usage:**
```rust
// Evaluate: F.total_bits where F = IEEE754_32
const_eval(&FieldAccess { base: F, field: "total_bits" }, &ctx)
// Returns: ConstValue::Nat(32)

// Evaluate: FRAC <= WIDTH where FRAC=16, WIDTH=32
const_eval(&BinOp { op: Le, lhs: FRAC, rhs: WIDTH }, &ctx)
// Returns: ConstValue::Bool(true)
```

---

## Phase 2: FloatFormat Type (1-2 weeks)

### 2.1 FloatFormat Definition

**Files to create:**
- `crates/skalp-stdlib/components/numeric/formats.sk`

**Tasks:**
- [ ] Define `FloatFormat` struct as per spec
- [ ] Define standard format constants:
  - `IEEE754_16`, `IEEE754_32`, `IEEE754_64`
  - `BFLOAT16`, `TFLOAT32`
- [ ] Add format validation functions

**Content:**
```skalp
struct FloatFormat {
    total_bits: nat,
    exponent_bits: nat,
    mantissa_bits: nat,
    bias: int,
    name: &str,
}

const IEEE754_32: FloatFormat = FloatFormat {
    total_bits: 32,
    exponent_bits: 8,
    mantissa_bits: 23,
    bias: 127,
    name: "IEEE754-single"
}

// ... other formats
```

### 2.2 Parametric FP Type

**Files to create:**
- `crates/skalp-stdlib/components/numeric/fp.sk`

**Tasks:**
- [ ] Define `fp<const F: FloatFormat>` type alias
- [ ] Implement component access methods:
  - `sign()`, `exponent()`, `mantissa()`
  - `is_nan()`, `is_inf()`, `is_zero()`
- [ ] Define convenience aliases: `fp16`, `fp32`, `fp64`, etc.

**Content:**
```skalp
type fp<const F: FloatFormat> = bit[F.total_bits]

impl<const F: FloatFormat> fp<F> {
    fn sign(self) -> bit {
        self[F.total_bits - 1]
    }

    fn exponent(self) -> bit[F.exponent_bits] {
        self[F.mantissa_bits .. F.mantissa_bits + F.exponent_bits]
    }

    fn mantissa(self) -> bit[F.mantissa_bits] {
        self[0 .. F.mantissa_bits]
    }

    fn is_nan(self) -> bit {
        let exp_all_ones = self.exponent() == ((1 << F.exponent_bits) - 1)
        let mant_nonzero = self.mantissa() != 0
        exp_all_ones && mant_nonzero
    }

    // ... other methods
}

// Aliases
type fp16 = fp<IEEE754_16>
type fp32 = fp<IEEE754_32>
type fp64 = fp<IEEE754_64>
```

---

## Phase 3: Fixed-Point and Integer Types (1 week)

### 3.1 Parametric Fixed Type

**Files to create:**
- `crates/skalp-stdlib/components/numeric/fixed.sk`

**Tasks:**
- [ ] Define `fixed<WIDTH, FRAC, SIGNED>` type
- [ ] Add Q-notation aliases: `q16_16`, `q8_8`, etc.
- [ ] Implement fixed-point arithmetic helpers

**Content:**
```skalp
type fixed<
    const WIDTH: nat,
    const FRAC: nat,
    const SIGNED: bool = true
> = bit[WIDTH]
where
    FRAC <= WIDTH

type q16_16 = fixed<32, 16, true>
type q8_8 = fixed<16, 8, true>
type uq16_16 = fixed<32, 16, false>
```

### 3.2 Integer Types

**Files to create:**
- `crates/skalp-stdlib/components/numeric/int.sk`

**Tasks:**
- [ ] Define `int<WIDTH, SIGNED>` as alias for `fixed<WIDTH, 0, SIGNED>`
- [ ] Add standard aliases: `i8`, `i16`, `i32`, `i64`, `u8`, `u16`, `u32`, `u64`

**Content:**
```skalp
type int<const WIDTH: nat, const SIGNED: bool = true> =
    fixed<WIDTH, 0, SIGNED>

type i8  = int<8, true>
type i16 = int<16, true>
type i32 = int<32, true>
type i64 = int<64, true>
type u8  = int<8, false>
type u16 = int<16, false>
type u32 = int<32, false>
type u64 = int<64, false>
```

---

## Phase 4: Numeric Trait and Generic Operations (2 weeks)

### 4.1 Numeric Trait Definition

**Files to create:**
- `crates/skalp-stdlib/components/numeric/traits.sk`

**Tasks:**
- [ ] Define `Numeric` trait with associated constants
- [ ] Implement `Numeric` for `fp<F>`
- [ ] Implement `Numeric` for `fixed<W, F, S>`
- [ ] Implement `Numeric` for `int<W, S>`

**Content:**
```skalp
trait Numeric {
    const TOTAL_BITS: nat;
    const IS_SIGNED: bool;
    const IS_FLOATING: bool;
    const IS_FIXED: bool;

    fn add(self, other: Self) -> Self;
    fn sub(self, other: Self) -> Self;
    fn mul(self, other: Self) -> Self;
    fn div(self, other: Self) -> Self;
}

impl<const F: FloatFormat> Numeric for fp<F> {
    const TOTAL_BITS = F.total_bits
    const IS_SIGNED = true
    const IS_FLOATING = true
    const IS_FIXED = false
    // ... operations
}

// Similar impls for fixed and int
```

### 4.2 Generic Arithmetic Operations

**Files to create:**
- `crates/skalp-stdlib/components/ops/arithmetic.sk`

**Tasks:**
- [ ] Implement generic `Add<T, intent I>`
- [ ] Implement generic `Sub<T, intent I>`
- [ ] Implement generic `Mul<T, intent I>` (handles fixed-point scaling)
- [ ] Implement generic `Div<T, intent I>` (handles fixed-point scaling)

**Content:**
```skalp
entity Add<T, intent I: Intent = Intent::default()>
where
    T: Numeric
{
    in a: T
    in b: T
    out result: T
}

impl<T, intent I> Add<T, I> {
    result = if T.IS_FLOATING {
        fp_add(a, b)
    } else {
        // Integer/fixed-point addition
        a + b  // Bit-level add
    }
}

entity Mul<T, intent I: Intent = Intent::default()>
where
    T: Numeric
{
    in a: T
    in b: T
    out result: T
}

impl<T, intent I> Mul<T, I> {
    result = if T.IS_FLOATING {
        fp_mul(a, b)
    } else if T.IS_FIXED {
        // Fixed-point multiply with shift
        signal prod_wide = a * b
        prod_wide >> T.FRAC_BITS  // Scale result
    } else {
        // Integer multiply
        a * b
    }
}
```

---

## Phase 5: Intent Type System (2 weeks)

### 5.1 Intent Type Definition

**Files to create:**
- `crates/skalp-hir/src/intent.rs`

**Tasks:**
- [ ] Define `Intent` struct with all fields from spec
- [ ] Define supporting enums: `Optimize`, `Accuracy`, `OverflowMode`, `RoundingMode`
- [ ] Implement intent merging: `Intent::merge()`
- [ ] Implement intent satisfaction checking: `Intent::satisfies()`

**Data structures:**
```rust
#[derive(Clone, Debug, PartialEq)]
pub struct Intent {
    pub latency: Option<Cycles>,
    pub throughput: Option<PerCycle>,
    pub accuracy: Option<Accuracy>,
    pub area: Option<LUT>,
    pub power: Option<Milliwatts>,
    pub optimize: Optimize,
    pub overflow: Option<OverflowMode>,
    pub rounding: Option<RoundingMode>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Optimize {
    Latency,
    Throughput,
    Area,
    Power,
    Balanced,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Accuracy {
    Low,    // ~12 bits
    Medium, // ~18 bits
    High,   // Full precision
}

impl Intent {
    pub fn merge(&self, other: &Intent) -> Intent {
        Intent {
            latency: other.latency.or(self.latency),
            throughput: other.throughput.or(self.throughput),
            accuracy: other.accuracy.or(self.accuracy),
            // ... merge other fields
            optimize: other.optimize,
        }
    }

    pub fn satisfies(&self, constraint: &Intent) -> bool {
        // Check if self satisfies all constraints in constraint
        if let (Some(lat), Some(max_lat)) = (self.latency, constraint.latency) {
            if lat > max_lat {
                return false;
            }
        }
        // ... check other fields
        true
    }
}
```

### 5.2 Intent Parsing and HIR Building

**Files to modify:**
- `crates/skalp-frontend/src/parse.rs`
- `crates/skalp-hir/src/builder.rs`

**Tasks:**
- [ ] Parse `@intent(...)` attributes
- [ ] Parse `intent I: Intent` generic parameters
- [ ] Parse intent expressions: `I.latency < 4`, `I.optimize == Latency`
- [ ] Build intent context during HIR construction
- [ ] Propagate intents through module hierarchy

**Example:**
```rust
// In HIR builder
struct IntentContext {
    parent_intent: Option<Intent>,
    local_intent: Option<Intent>,
    signal_intents: HashMap<HirNodeId, Intent>,
}

impl HirBuilder {
    fn resolve_intent(&self, node_id: HirNodeId) -> Intent {
        let parent = self.intent_ctx.parent_intent;
        let local = self.intent_ctx.signal_intents.get(&node_id);

        match (parent, local) {
            (Some(p), Some(l)) => p.merge(l),
            (Some(p), None) => p.clone(),
            (None, Some(l)) => l.clone(),
            (None, None) => Intent::default(),
        }
    }
}
```

---

## Phase 6: Parametric Vector Types (1 week)

### 6.1 Generic Vector Type

**Files to create:**
- `crates/skalp-stdlib/components/vector/vec.sk`

**Tasks:**
- [ ] Define `vec<T, const N: nat>` type
- [ ] Add aliases: `vec2<T>`, `vec3<T>`, `vec4<T>`
- [ ] Implement named component access for vec2/vec3/vec4

**Content:**
```skalp
type vec<T, const N: nat> = [T; N]
where
    T: Synthesizable

type vec2<T> = vec<T, 2>
type vec3<T> = vec<T, 3>
type vec4<T> = vec<T, 4>

impl<T> vec<T, 2> {
    fn x(self) -> T { self[0] }
    fn y(self) -> T { self[1] }
}

impl<T> vec<T, 3> {
    fn x(self) -> T { self[0] }
    fn y(self) -> T { self[1] }
    fn z(self) -> T { self[2] }
}

impl<T> vec<T, 4> {
    fn x(self) -> T { self[0] }
    fn y(self) -> T { self[1] }
    fn z(self) -> T { self[2] }
    fn w(self) -> T { self[3] }
}
```

### 6.2 Generic Vector Operations

**Files to create:**
- `crates/skalp-stdlib/components/vector/ops.sk`

**Tasks:**
- [ ] Implement `VecAdd<T, N, intent I>`
- [ ] Implement `VecSub<T, N, intent I>`
- [ ] Implement `VecMul<T, N, intent I>` (component-wise)
- [ ] Implement `VecScale<T, N, intent I>` (scalar multiply)
- [ ] Implement `VecDot<T, N, intent I>`

**Content:**
```skalp
entity VecAdd<T, const N: nat, intent I: Intent = Intent::default()>
where
    T: Numeric
{
    in a: vec<T, N>
    in b: vec<T, N>
    out result: vec<T, N>
}

impl<T, const N: nat, intent I> VecAdd<T, N, I> {
    result = if I.optimize == Optimize::Latency && I.area > N * 100 {
        // Fully parallel
        @unroll
        for i in 0..N {
            result[i] = a[i] + b[i]
        }
    } else if I.optimize == Optimize::Area {
        // Sequential
        @sequential
        for i in 0..N {
            result[i] = a[i] + b[i]
        }
    } else {
        // Balanced - partial unroll
        @unroll(factor: min(4, N))
        for i in 0..N {
            result[i] = a[i] + b[i]
        }
    }
}
```

---

## Phase 7: Monomorphization (3 weeks)

### 7.1 Monomorphization Pass

**Files to create:**
- `crates/skalp-mir/src/monomorphize.rs`

**Tasks:**
- [ ] Implement monomorphization algorithm
- [ ] Instantiate generic entities with concrete types
- [ ] Instantiate intent parameters with resolved intents
- [ ] Generate specialized code for each unique instantiation
- [ ] Cache instantiations to avoid duplication

**Algorithm:**
```rust
struct Monomorphizer {
    instantiations: HashMap<MonomorphizationKey, EntityId>,
}

#[derive(Hash, Eq, PartialEq)]
struct MonomorphizationKey {
    entity_id: EntityId,
    type_args: Vec<Type>,
    const_args: Vec<ConstValue>,
    intent_arg: Option<Intent>,
}

impl Monomorphizer {
    fn monomorphize_entity(
        &mut self,
        entity_id: EntityId,
        type_args: Vec<Type>,
        const_args: Vec<ConstValue>,
        intent: Option<Intent>,
    ) -> EntityId {
        let key = MonomorphizationKey {
            entity_id,
            type_args: type_args.clone(),
            const_args: const_args.clone(),
            intent_arg: intent.clone(),
        };

        // Check cache
        if let Some(&specialized_id) = self.instantiations.get(&key) {
            return specialized_id;
        }

        // Specialize entity
        let specialized = self.specialize_entity(
            entity_id,
            &type_args,
            &const_args,
            intent.as_ref(),
        );

        // Cache and return
        let specialized_id = self.add_entity(specialized);
        self.instantiations.insert(key, specialized_id);
        specialized_id
    }

    fn specialize_entity(
        &self,
        entity_id: EntityId,
        type_args: &[Type],
        const_args: &[ConstValue],
        intent: Option<&Intent>,
    ) -> Entity {
        let generic_entity = self.get_entity(entity_id);

        // Substitute type parameters
        let mut entity = generic_entity.clone();
        for (param, arg) in generic_entity.type_params.iter().zip(type_args) {
            entity = substitute_type(&entity, param, arg);
        }

        // Substitute const parameters
        for (param, arg) in generic_entity.const_params.iter().zip(const_args) {
            entity = substitute_const(&entity, param, arg);
        }

        // Substitute intent parameter
        if let Some(intent_param) = &generic_entity.intent_param {
            if let Some(intent_arg) = intent {
                entity = substitute_intent(&entity, intent_param, intent_arg);

                // Evaluate intent conditionals
                entity = evaluate_intent_conditionals(&entity, intent_arg);
            }
        }

        entity
    }

    fn evaluate_intent_conditionals(
        &self,
        entity: &Entity,
        intent: &Intent,
    ) -> Entity {
        // Evaluate: if I.latency < 4 { ... } else { ... }
        // Replace with chosen branch based on intent value
        // ...
    }
}
```

### 7.2 Integration with MIR Lowering

**Files to modify:**
- `crates/skalp-mir/src/lower.rs`

**Tasks:**
- [ ] Run monomorphization before MIR lowering
- [ ] Resolve all generic instantiations
- [ ] Generate specialized MIR for each instantiation

---

## Phase 8: Stdlib Migration (2 weeks)

### 8.1 Convert Existing Operations to Parametric

**Files to convert:**
- `crates/skalp-stdlib/components/fp/fp32_add.sk` → Generic `FPAdd<F, I>`
- `crates/skalp-stdlib/components/fp/fp32_sqrt.sk` → Generic `Sqrt<F, I>`
- `crates/skalp-stdlib/components/vec/vec_ops.sk` → Generic `VecOp<T, N, I>`

**Tasks:**
- [ ] Convert FP operations to use `fp<F>`
- [ ] Add intent parameters to all operations
- [ ] Implement compile-time specialization
- [ ] Add intent-driven algorithm selection

**Example conversion:**

Before:
```skalp
entity FP32Sqrt {
    in x: fp32
    out result: fp32
}

impl FP32Sqrt {
    // Single implementation
    result = newton_raphson(x, iterations: 2)
}
```

After:
```skalp
entity Sqrt<const F: FloatFormat, intent I: Intent = Intent::default()> {
    in x: fp<F>
    out result: fp<F>
}

impl<const F: FloatFormat, intent I> Sqrt<F, I> {
    // Multiple implementations selected by intent
    result = if I.latency < 4 {
        lut_sqrt::<F>(x)
    } else if I.latency < 8 {
        newton_raphson::<F>(x, iterations: 1)
    } else {
        newton_raphson::<F>(x, iterations: 2)
    }
}
```

### 8.2 Maintain Backward Compatibility

**Tasks:**
- [ ] Keep type aliases: `fp16 = fp<IEEE754_16>`, `fp32 = fp<IEEE754_32>`
- [ ] Keep vec aliases: `vec2<T> = vec<T, 2>`, etc.
- [ ] Keep function aliases: `fp32_sqrt(x) = sqrt::<IEEE754_32>(x)`

---

## Phase 9: Testing (2 weeks)

### 9.1 Unit Tests

**Test categories:**
- [ ] Parser tests for new syntax
- [ ] Const evaluation tests
- [ ] Monomorphization tests
- [ ] Intent resolution tests
- [ ] Type compatibility tests

**Example tests:**
```rust
#[test]
fn test_parse_const_generic() {
    let source = "entity Foo<const N: nat> { }";
    let result = parse(source);
    assert!(result.is_ok());
}

#[test]
fn test_const_eval_field_access() {
    let expr = "IEEE754_32.total_bits";
    let result = const_eval(expr);
    assert_eq!(result, ConstValue::Nat(32));
}

#[test]
fn test_monomorphize_fp_add() {
    let generic = parse("entity Add<const F: FloatFormat> { ... }");
    let specialized = monomorphize(generic, &[IEEE754_32]);
    assert_eq!(specialized.inputs[0].ty.width, 32);
}

#[test]
fn test_intent_propagation() {
    let source = r#"
        @intent(latency: 4_cycles)
        impl Parent {
            inst child: Child { }
        }
    "#;
    let hir = build_hir(source);
    let child_intent = resolve_intent(&hir, child_id);
    assert_eq!(child_intent.latency, Some(4));
}
```

### 9.2 Integration Tests

**Test files to create:**
```skalp
// tests/parametric/fp_formats.sk
// Test all FP formats work
signal x16: fp16 = 1.5
signal x32: fp32 = 1.5
signal x64: fp64 = 1.5
signal xbf: bf16 = 1.5

signal r16: fp16 = sqrt(x16)
signal r32: fp32 = sqrt(x32)
signal r64: fp64 = sqrt(x64)
signal rbf: bf16 = sqrt(xbf)

// tests/parametric/fixed_point.sk
// Test fixed-point arithmetic
signal a: q16_16 = 3.14159
signal b: q16_16 = 2.71828
signal sum: q16_16 = a + b
signal prod: q16_16 = a * b

// tests/parametric/vectors.sk
// Test generic vectors
signal v2: vec2<fp32> = {1.0, 2.0}
signal v3: vec3<fp32> = {1.0, 2.0, 3.0}
signal v4: vec4<fp64> = {1.0, 2.0, 3.0, 4.0}

signal sum2: vec2<fp32> = v2 + v2
signal dot3: fp32 = dot(v3, v3)

// tests/intent/specialization.sk
// Test intent-driven specialization
@intent(latency: 2_cycles)
impl Fast {
    signal r: fp32 = sqrt(x)  // Should use LUT
}

@intent(accuracy: High)
impl Precise {
    signal r: fp32 = sqrt(x)  // Should use iterative
}

// tests/intent/propagation.sk
// Test intent propagation
@intent(optimize: Area)
impl Parent {
    inst child: Child { }  // Child inherits Area optimization
}
```

### 9.3 Golden File Tests

**Tasks:**
- [ ] Add golden files for parametric type synthesis
- [ ] Verify generated code matches expected output
- [ ] Test multiple instantiations produce correct specialized code

---

## Phase 10: Documentation and Examples (1 week)

### 10.1 Update Documentation

**Files to update:**
- `docs/LANGUAGE_SPECIFICATION.md` - Already done! ✓
- `crates/skalp-stdlib/README.md` - Add parametric types section
- `docs/user/quick-start.md` - Add examples with new types

### 10.2 Create Examples

**Example files to create:**
```
examples/parametric_types/
├── custom_fp_format.sk    - Custom FP24 format
├── fixed_point_dsp.sk     - Fixed-point signal processing
├── generic_fir.sk         - FIR filter for any type
├── multi_precision.sk     - Mixed fp16/fp32/fp64 design
└── intent_optimization.sk - Intent-driven specialization
```

---

## Timeline Summary

| Phase | Duration | Dependencies |
|-------|----------|--------------|
| 1. Type System Foundation | 2-3 weeks | None |
| 2. FloatFormat Type | 1-2 weeks | Phase 1 |
| 3. Fixed/Int Types | 1 week | Phase 1 |
| 4. Numeric Trait | 2 weeks | Phases 2, 3 |
| 5. Intent Type System | 2 weeks | Phase 1 |
| 6. Vector Types | 1 week | Phase 4 |
| 7. Monomorphization | 3 weeks | All previous |
| 8. Stdlib Migration | 2 weeks | Phase 7 |
| 9. Testing | 2 weeks | Phase 8 |
| 10. Documentation | 1 week | Phase 9 |

**Total: ~17-19 weeks (4-5 months)**

---

## Success Criteria

✅ **Syntax Support**
- [ ] Parse `entity Foo<const N: nat, intent I: Intent>`
- [ ] Parse `type fp<const F: FloatFormat>`
- [ ] Parse `type fixed<const W: nat, const F: nat, const S: bool>`
- [ ] Parse `@intent(...)` attributes

✅ **Type System**
- [ ] Create parametric FP types with any format
- [ ] Create parametric fixed-point types
- [ ] Create parametric vector types
- [ ] Intent as generic parameter

✅ **Compile-Time Features**
- [ ] Const expression evaluation
- [ ] Intent-driven conditional compilation
- [ ] Monomorphization generates specialized code
- [ ] Type constraints validated at compile-time

✅ **Stdlib**
- [ ] All operations work with fp16/32/64/bf16/custom
- [ ] All operations work with fixed-point
- [ ] All operations work with integers
- [ ] All operations take intent parameter

✅ **Testing**
- [ ] 100+ unit tests pass
- [ ] Integration tests pass
- [ ] Golden file tests pass
- [ ] No regression in existing tests

✅ **Performance**
- [ ] Monomorphization caching prevents code bloat
- [ ] Compile times remain reasonable (<2x slowdown)
- [ ] Generated code quality matches hand-written

---

## Risk Mitigation

### Risk 1: Compile Time Explosion

**Risk:** Monomorphization generates too many instantiations

**Mitigation:**
- Aggressive instantiation caching
- Lazy monomorphization (only instantiate what's used)
- Compile-time limits on instantiation depth

### Risk 2: Complex Type Inference

**Risk:** Type inference becomes intractable with many parameters

**Mitigation:**
- Require explicit type annotations where ambiguous
- Good error messages guide users
- Fallback to default intents when unspecified

### Risk 3: Backward Compatibility

**Risk:** Breaking existing code

**Mitigation:**
- Maintain all existing type aliases
- Keep legacy syntax working alongside new syntax
- Gradual migration path documented

### Risk 4: Implementation Complexity

**Risk:** Const evaluation and monomorphization are complex

**Mitigation:**
- Implement incrementally (Phase 1 before Phase 7)
- Extensive testing at each phase
- Reference Rust compiler implementation patterns

---

## Implementation Order Recommendation

**Start with:**
1. Phase 1 (Type System Foundation) - Enables everything else
2. Phase 2 (FloatFormat) - Concrete example to validate approach
3. Phase 7 (Monomorphization) - Core mechanism needed for testing

**Then:**
4. Phase 5 (Intent System) - Can develop in parallel with stdlib work
5. Phase 4 (Numeric Trait) + Phase 6 (Vectors) - Builds on foundation

**Finally:**
6. Phase 8 (Stdlib Migration) - Apply to real code
7. Phase 9 (Testing) - Validate everything
8. Phase 10 (Documentation) - Polish and release

---

## Next Steps

1. **Review this plan** - Get feedback from team
2. **Create tracking issues** - One issue per phase in GitHub
3. **Start Phase 1** - Parser and HIR extensions
4. **Set up CI** - Ensure tests run on every commit
5. **Weekly progress meetings** - Review progress, adjust timeline

**SKALP will have the most advanced type system of any HDL!** 🚀
