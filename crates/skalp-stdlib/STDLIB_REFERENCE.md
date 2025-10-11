# SKALP Standard Library Reference

> **Version:** 0.1.0
> **Status:** Early Development - Core types complete, operations in progress

This document provides a comprehensive reference for the SKALP Standard Library, including type definitions, implemented operations, and usage examples.

---

## Table of Contents

1. [Overview](#overview)
2. [Type System](#type-system)
3. [Floating-Point Types](#floating-point-types)
4. [Vector Types](#vector-types)
5. [Vector Operations](#vector-operations)
6. [Floating-Point Operations](#floating-point-operations)
7. [Usage Examples](#usage-examples)
8. [Implementation Status](#implementation-status)
9. [Testing](#testing)

---

## Overview

The SKALP Standard Library provides hardware-synthesizable implementations of:

- **Floating-point types** (fp16, fp32, fp64) - IEEE 754 compliant
- **Vector types** (vec2, vec3, vec4) - Parameterized over element types
- **Arithmetic operations** - Component-wise and vector-specific operations
- **Geometric operations** - Dot product, cross product, distance, etc.

### Design Principles

1. **Synthesizable RTL** - All code compiles to hardware (no behavioral constructs)
2. **Generic Programming** - Parameterized types work with multiple element types
3. **Type Safety** - Compile-time type checking prevents errors
4. **Composability** - Complex operations built from simple, tested primitives
5. **Standards Compliance** - IEEE 754 for floating-point, standard conventions for vectors

---

## Type System

### Built-in Primitive Types

| Type | Width | Description |
|------|-------|-------------|
| `bit` | 1 | Single bit |
| `bit<N>` | N | N-bit unsigned integer |
| `nat` | Variable | Natural number (compile-time constant) |

### Floating-Point Types

| Type | Width | Sign | Exponent | Mantissa | Bias | Standard |
|------|-------|------|----------|----------|------|----------|
| `fp16` | 16 | 1 | 5 | 10 | 15 | IEEE 754 half-precision |
| `fp32` | 32 | 1 | 8 | 23 | 127 | IEEE 754 single-precision |
| `fp64` | 64 | 1 | 11 | 52 | 1023 | IEEE 754 double-precision |

**Bit Layout:**
```
fp32: [31] sign | [30:23] exponent | [22:0] mantissa
      ┌─────┬──────────────┬────────────────────────┐
      │  S  │   Exponent   │       Mantissa         │
      └─────┴──────────────┴────────────────────────┘
       31    30         23  22                      0

Normalized: (-1)^S × 1.mantissa × 2^(exponent - 127)
```

### Vector Types

Vector types are **parameterized** over their element type:

| Type | Components | Syntax | Example |
|------|------------|--------|---------|
| `vec2<T>` | 2 | `vec2<T> { x: T, y: T }` | `vec2<fp32>` |
| `vec3<T>` | 3 | `vec3<T> { x: T, y: T, z: T }` | `vec3<bit<16>>` |
| `vec4<T>` | 4 | `vec4<T> { x: T, y: T, z: T, w: T }` | `vec4<fp64>` |

**Common Instantiations:**
- `vec2<fp32>` - 2D vector of 32-bit floats (64 bits total)
- `vec3<fp32>` - 3D vector of 32-bit floats (96 bits total)
- `vec4<fp32>` - 4D vector of 32-bit floats (128 bits total)

### Component Access

Vector components are accessed using field syntax:

```skalp
signal v: vec3<fp32> = /* ... */
signal x_component: fp32 = v.x
signal y_component: fp32 = v.y
signal z_component: fp32 = v.z
```

---

## Floating-Point Types

### Special Values

| Value | Exponent | Mantissa | Representation (fp32) |
|-------|----------|----------|----------------------|
| +Zero | 0 | 0 | `0x00000000` |
| -Zero | 0 | 0 | `0x80000000` |
| +Infinity | 255 | 0 | `0x7F800000` |
| -Infinity | 255 | 0 | `0xFF800000` |
| NaN (quiet) | 255 | ≠0 | `0xFFC00000` |

### Supported Operations

#### Arithmetic (Built-in Operators)

| Operation | Syntax | Description | Status |
|-----------|--------|-------------|--------|
| Addition | `a + b` | IEEE 754 addition | ✅ Type system complete |
| Subtraction | `a - b` | IEEE 754 subtraction | ✅ Type system complete |
| Multiplication | `a * b` | IEEE 754 multiplication | ✅ Type system complete |
| Division | `a / b` | IEEE 754 division | ✅ Type system complete |
| Negation | `-x` | Sign bit flip | ✅ Type system complete |

#### Comparison (Built-in Operators)

| Operation | Syntax | Description | Status |
|-----------|--------|-------------|--------|
| Less than | `a < b` | FP comparison | ✅ Type system complete |
| Less or equal | `a <= b` | FP comparison | ✅ Type system complete |
| Greater than | `a > b` | FP comparison | ✅ Type system complete |
| Greater or equal | `a >= b` | FP comparison | ✅ Type system complete |
| Equal | `a == b` | FP equality | ✅ Type system complete |
| Not equal | `a != b` | FP inequality | ✅ Type system complete |

#### Hardware Entities

Entity implementations for explicit instantiation:

| Entity | Inputs | Output | File | Status |
|--------|--------|--------|------|--------|
| `FP32Add` | `a: fp32, b: fp32` | `result: fp32` | `fp32_add.sk` | 🚧 Defined (needs testing) |
| `FP32Sub` | `a: fp32, b: fp32` | `result: fp32` | `fp32_sub.sk` | 🚧 Defined (needs testing) |
| `FP32Mul` | `a: fp32, b: fp32` | `result: fp32` | `fp32_mul.sk` | 🚧 Defined (needs testing) |
| `FP32Compare` | `a: fp32, b: fp32` | `lt, eq, gt: bit` | `fp32_compare.sk` | 🚧 Defined (needs testing) |

### Usage Examples

#### Direct Operator Usage (Recommended)

```skalp
entity FloatCalculator {
    in x: fp32
    in y: fp32
    out sum: fp32
    out product: fp32
    out is_larger: bit
}

impl FloatCalculator {
    // Use built-in operators
    sum = x + y
    product = x * y
    is_larger = x > y
}
```

#### Entity Instantiation (Explicit)

```skalp
entity FloatAdderExplicit {
    in a: fp32
    in b: fp32
    out result: fp32
}

impl FloatAdderExplicit {
    // Instantiate FP adder entity explicitly
    inst adder: FP32Add {
        a = a,
        b = b,
        result => result
    }
}
```

---

## Vector Types

### Construction

Vectors are constructed using struct initialization syntax:

```skalp
// 2D vector
signal pos: vec2<fp32> = vec2::<fp32> {
    x: 1.0,
    y: 2.0
}

// 3D vector
signal velocity: vec3<fp32> = vec3::<fp32> {
    x: 0.0,
    y: -9.8,
    z: 0.0
}

// 4D vector (homogeneous coordinates)
signal point: vec4<fp32> = vec4::<fp32> {
    x: 1.0,
    y: 2.0,
    z: 3.0,
    w: 1.0
}
```

### Component-wise Operations

Operations that work element-by-element on vector components.

#### Built-in Operators

All standard arithmetic operators work component-wise on vectors of the same type:

```skalp
signal a: vec3<fp32> = /* ... */
signal b: vec3<fp32> = /* ... */

// Component-wise operations using built-in operators
signal sum: vec3<fp32> = a + b
// Equivalent to: vec3 { x: a.x + b.x, y: a.y + b.y, z: a.z + b.z }

signal diff: vec3<fp32> = a - b
signal hadamard: vec3<fp32> = a * b  // Component-wise multiply
```

---

## Vector Operations

### Vec2 Operations

| Entity | Inputs | Output | Description | File |
|--------|--------|--------|-------------|------|
| `Vec2Add<T>` | `a, b: vec2<T>` | `result: vec2<T>` | Component-wise addition | `vec_ops.sk:23` |
| `Vec2Sub<T>` | `a, b: vec2<T>` | `result: vec2<T>` | Component-wise subtraction | `vec_ops.sk:37` |
| `Vec2Mul<T>` | `a, b: vec2<T>` | `result: vec2<T>` | Hadamard product | `vec_ops.sk:51` |
| `Vec2Scale<T>` | `v: vec2<T>, s: T` | `result: vec2<T>` | Scalar multiplication | `vec_ops.sk:65` |
| `Vec2Dot<T>` | `a, b: vec2<T>` | `result: T` | Dot product | `vec_ops.sk:79` |
| `Vec2Cross<T>` | `a, b: vec2<T>` | `result: T` | 2D cross product (scalar) | `vec_ops.sk:90` |
| `Vec2Perp<T>` | `v: vec2<T>` | `result: vec2<T>` | Perpendicular (90° rotation) | `vec_ops.sk:101` |

### Vec3 Operations

| Entity | Inputs | Output | Description | File |
|--------|--------|--------|-------------|------|
| `Vec3Add<T>` | `a, b: vec3<T>` | `result: vec3<T>` | Component-wise addition | `vec_ops.sk:122` |
| `Vec3Sub<T>` | `a, b: vec3<T>` | `result: vec3<T>` | Component-wise subtraction | `vec_ops.sk:137` |
| `Vec3Scale<T>` | `v: vec3<T>, s: T` | `result: vec3<T>` | Scalar multiplication | `vec_ops.sk:152` |
| `Vec3Dot<T>` | `a, b: vec3<T>` | `result: T` | Dot product | `vec_ops.sk:167` |
| `Vec3Cross<T>` | `a, b: vec3<T>` | `result: vec3<T>` | 3D cross product | `vec_ops.sk:179` |

### Vec4 Operations

| Entity | Inputs | Output | Description | File |
|--------|--------|--------|-------------|------|
| `Vec4Add<T>` | `a, b: vec4<T>` | `result: vec4<T>` | Component-wise addition | `vec_ops.sk:198` |
| `Vec4Sub<T>` | `a, b: vec4<T>` | `result: vec4<T>` | Component-wise subtraction | `vec_ops.sk:214` |
| `Vec4Scale<T>` | `v: vec4<T>, s: T` | `result: vec4<T>` | Scalar multiplication | `vec_ops.sk:230` |
| `Vec4Dot<T>` | `a, b: vec4<T>` | `result: T` | Dot product | `vec_ops.sk:246` |

### Floating-Point Specific Operations

These operations are specialized for `fp32` vectors:

| Entity | Inputs | Output | Description | File |
|--------|--------|--------|-------------|------|
| `Vec3LengthSq` | `v: vec3<fp32>` | `result: fp32` | Length squared (avoids sqrt) | `vec_ops.sk:261` |
| `Vec3DistanceSq` | `a, b: vec3<fp32>` | `result: fp32` | Distance squared | `vec_ops.sk:275` |
| `Vec3Lerp` | `a, b: vec3<fp32>, t: fp32` | `result: vec3<fp32>` | Linear interpolation | `vec_ops.sk:294` |
| `Vec3Min` | `a, b: vec3<fp32>` | `result: vec3<fp32>` | Component-wise minimum | `vec_ops.sk:320` |
| `Vec3Max` | `a, b: vec3<fp32>` | `result: vec3<fp32>` | Component-wise maximum | `vec_ops.sk:335` |
| `Vec3Clamp` | `v, min_val, max_val: vec3<fp32>` | `result: vec3<fp32>` | Component-wise clamp | `vec_ops.sk:350` |

---

## Floating-Point Operations

### FP32 Arithmetic Entities

Detailed implementations in `components/fp/`:

#### FP32Add (fp32_add.sk)

Single-precision addition following IEEE 754-2008.

**Ports:**
```skalp
in a: fp32          // First operand
in b: fp32          // Second operand
out result: fp32    // a + b
out flags: bit<5>   // [4]=invalid [3]=div0 [2]=overflow [1]=underflow [0]=inexact
```

**Features:**
- ✅ Exponent alignment
- ✅ Mantissa addition with guard/round/sticky bits
- ✅ Normalization
- ✅ Rounding (round to nearest, ties to even)
- ✅ Special case handling (NaN, Inf, zero, denormals)
- ✅ Exception flags

**Algorithm:**
1. Unpack operands (sign, exponent, mantissa)
2. Detect special cases (NaN, Inf, zero)
3. Align exponents by shifting smaller mantissa
4. Add/subtract aligned mantissas based on sign
5. Normalize result (shift to get leading 1)
6. Round using guard/round/sticky bits
7. Pack result with sign, exponent, mantissa
8. Set exception flags

#### FP32Sub (fp32_sub.sk)

Subtraction via negation + addition.

**Ports:**
```skalp
in a: fp32
in b: fp32
out result: fp32
```

**Implementation:**
```skalp
inst adder: FP32Add {
    a = a,
    b = {~b[31], b[30:0]},  // Flip sign bit
    result => result
}
```

#### FP32Mul (fp32_mul.sk)

Single-precision multiplication.

**Ports:**
```skalp
in a: fp32
in b: fp32
out result: fp32
```

**Features:**
- ✅ Mantissa multiplication (24×24 → 48 bits)
- ✅ Exponent addition with bias correction
- ✅ Normalization and rounding
- ✅ Special case handling
- ✅ Sign calculation (XOR)

#### FP32Compare (fp32_compare.sk)

Comparison operations with NaN handling.

**Ports:**
```skalp
in a: fp32
in b: fp32
out lt: bit         // a < b
out eq: bit         // a == b
out gt: bit         // a > b
out unordered: bit  // NaN present
```

---

## Usage Examples

### Example 1: Vector Addition

```skalp
entity Vec3Adder {
    in a: vec3<fp32>
    in b: vec3<fp32>
    out result: vec3<fp32>
}

impl Vec3Adder {
    // Option 1: Use built-in operator (recommended)
    result = a + b

    // Option 2: Explicit entity instantiation
    // inst add: Vec3Add<fp32> {
    //     a = a,
    //     b = b,
    //     result => result
    // }

    // Option 3: Manual component-wise (verbose)
    // result = vec3::<fp32> {
    //     x: a.x + b.x,
    //     y: a.y + b.y,
    //     z: a.z + b.z
    // }
}
```

### Example 2: Dot Product

```skalp
entity DotProductExample {
    in a: vec3<fp32>
    in b: vec3<fp32>
    out dot_result: fp32
}

impl DotProductExample {
    // Instantiate dot product entity
    inst dot: Vec3Dot<fp32> {
        a = a,
        b = b,
        result => dot_result
    }
}
```

### Example 3: Vector Normalization (Conceptual)

```skalp
// NOTE: This requires sqrt(), which is not yet implemented
entity Normalize {
    in v: vec3<fp32>
    out normalized: vec3<fp32>
}

impl Normalize {
    // Compute length squared
    inst len_sq: Vec3LengthSq {
        v = v
    }

    // Would need: length = sqrt(len_sq.result)
    // Would need: normalized = v / length

    // For now, this is conceptual - sqrt not yet implemented
}
```

### Example 4: Ray-Sphere Intersection (Advanced)

From `examples/ray_sphere_intersection.sk`:

```skalp
entity RaySphereIntersection {
    in ray_origin: vec3<fp32>
    in ray_direction: vec3<fp32>
    in sphere_center: vec3<fp32>
    in sphere_radius: fp32

    out hit: bit
    out hit_distance: fp32
}

impl RaySphereIntersection {
    // Vector from ray origin to sphere center
    inst compute_oc: Vec3Sub<fp32> {
        a = ray_origin,
        b = sphere_center
    }

    // Compute coefficients for quadratic equation
    inst compute_a: Vec3Dot<fp32> {
        a = ray_direction,
        b = ray_direction
    }

    inst compute_half_b: Vec3Dot<fp32> {
        a = compute_oc.result,
        b = ray_direction
    }

    inst compute_c_part1: Vec3Dot<fp32> {
        a = compute_oc.result,
        b = compute_oc.result
    }

    // Radius squared
    inst radius_sq: FP32Mul {
        a = sphere_radius,
        b = sphere_radius
    }

    inst compute_c: FP32Sub {
        a = compute_c_part1.result,
        b = radius_sq.result
    }

    // Discriminant = b² - ac
    // ... (continued in actual file)
}
```

---

## Implementation Status

### ✅ Complete

- **Type system**: fp16, fp32, fp64, vec2/3/4<T>
- **Component access**: Field syntax (`.x`, `.y`, `.z`, `.w`)
- **Built-in operators**: Arithmetic and comparison for FP and vectors
- **Simulation**: CPU and GPU (Metal) simulation support
- **Synthesis**: MIR/LIR lowering complete

### 🚧 In Progress

- **FP arithmetic entities**: Defined but need comprehensive testing
- **Entity-based operations**: Parsing and instantiation work, validation needed

### ⏳ Planned

- **Transcendental functions**: sqrt, exp, log, sin, cos, etc.
- **Format conversions**: fp16↔fp32↔fp64, int↔float
- **Advanced vector ops**: normalize, reflect, refract
- **Traits implementation**: Full trait system for generic programming
- **Optimizations**: Pipelined variants, area-optimized versions

---

## Testing

### Test Organization

| Test File | Purpose | Coverage |
|-----------|---------|----------|
| `test_stdlib_synthesis.rs` | Verify types synthesize to LIR | FP/vector field access, component ops |
| `test_vec_arithmetic.rs` | Simulation tests for vectors | Vec2/3 add/mul on CPU and GPU |
| `test_cpu_fp_simulation.rs` | CPU FP arithmetic validation | Add, mul, comparison, negation |
| `test_stdlib_fp.rs` | FP type tests | Type definitions, special values |
| `test_stdlib_vec.rs` | Vector type tests | Construction, field access |

### Running Tests

```bash
# All stdlib tests
cargo test --test test_stdlib_synthesis
cargo test --test test_vec_arithmetic
cargo test --test test_cpu_fp_simulation

# Specific test
cargo test --test test_vec_arithmetic test_vec2_component_addition_cpu

# With features
cargo test --all-features

# GPU tests (macOS only)
cargo test --test test_vec_arithmetic test_vec2_component_addition_gpu
```

### Test Results (Current)

```
test_stdlib_synthesis.rs:
  ✅ test_vec2_field_access_synthesizes
  ✅ test_vec2_addition_synthesizes
  ✅ test_fp32_binary_ops_synthesize
  ✅ test_vec3_component_operations_synthesize
  ✅ test_vec4_field_access_synthesizes
  ✅ test_nested_field_and_binary_synthesizes
  ✅ test_mixed_fp_types_synthesize

test_vec_arithmetic.rs:
  ✅ test_vec2_component_addition_cpu
  ✅ test_vec2_component_addition_gpu (macOS only)
  ✅ test_vec3_component_multiply_cpu
  ✅ test_vec3_component_multiply_gpu (macOS only)

test_cpu_fp_simulation.rs:
  ✅ test_fp32_addition_cpu
  ✅ test_fp32_multiplication_cpu
  ✅ test_fp32_comparison_cpu
  ✅ test_fp32_negation_cpu
```

---

## Advanced Features

### Generic Programming

Vector operations use type parameters to work with any element type:

```skalp
// Generic entity works with any synthesizable type T
entity Vec2Add<T> where T: Synthesizable {
    in a: vec2<T>
    in b: vec2<T>
    out result: vec2<T>
}

// Can instantiate with different types:
inst fp_add: Vec2Add<fp32> { /* ... */ }
inst int_add: Vec2Add<bit<16>> { /* ... */ }
```

### Trait System (Defined, Not Yet Fully Integrated)

Traits define interfaces and enable polymorphism:

```skalp
trait FloatingPoint {
    const WIDTH: nat
    const EXP_WIDTH: nat
    const MANT_WIDTH: nat

    fn add(self, other: Self) -> Self
    fn mul(self, other: Self) -> Self
    // ...
}

trait Vector<T> where T: Synthesizable {
    const DIM: nat
    type Scalar = T

    fn dot(self, other: Self) -> T
    fn length_sq(self) -> T
    // ...
}
```

See `components/fp/traits.sk` and `components/vec/traits.sk` for full definitions.

### Module Composition

Complex operations built from simple primitives:

```skalp
entity Vec3Clamp {
    in v: vec3<fp32>
    in min_val: vec3<fp32>
    in max_val: vec3<fp32>
    out result: vec3<fp32>
}

impl Vec3Clamp {
    // First clamp to minimum using max
    inst clamped_min: Vec3Max {
        a = v,
        b = min_val
    }

    // Then clamp to maximum using min
    inst clamped: Vec3Min {
        a = clamped_min.result,
        b = max_val,
        result => result
    }
}
```

---

## References

### Standards

- **IEEE 754-2019**: Standard for Floating-Point Arithmetic
- **IEEE 754-2008**: Previous floating-point standard (basis for current impl)

### Literature

- "Handbook of Floating-Point Arithmetic" by Muller, Brisebarre, de Dinechin, et al.
- "Computer Arithmetic Algorithms" by Koren
- "Digital Arithmetic" by Ercegovac and Lang

### Related Files

- `components/fp/` - Floating-point arithmetic entities
- `components/vec/` - Vector operation entities
- `components/fp/traits.sk` - FP trait definitions
- `components/vec/traits.sk` - Vector trait definitions
- `ADVANCED_PATTERNS.md` - Advanced language features and patterns
- `README.md` - High-level overview

---

## Contributing

### Adding New Operations

1. **Define the entity** in appropriate file (e.g., `fp/fp32_sqrt.sk`)
2. **Implement the logic** using SKALP syntax
3. **Add tests** in `tests/test_stdlib_*.rs`
4. **Update this reference** with new operation
5. **Run CI checks**: `./scripts/ci_check.sh`

### Code Style

- Use descriptive entity and signal names
- Include inline comments for complex logic
- Document special cases and edge conditions
- Add usage examples in comments
- Follow existing file organization patterns

### Testing Requirements

- **Synthesis test**: Verify entity synthesizes to LIR
- **Simulation test**: Validate behavior on known inputs
- **Special cases**: Test NaN, Inf, zero, denormals (for FP)
- **Edge cases**: Test min/max values, overflow/underflow

---

## Changelog

### Version 0.1.0 (Current)

**Added:**
- FP types (fp16, fp32, fp64) with IEEE 754 format
- Vector types (vec2, vec3, vec4) with generic element types
- Component-wise arithmetic using built-in operators
- Vec2/3/4 operations (add, sub, scale, dot, cross)
- FP-specific vec ops (lerp, min, max, clamp)
- FP arithmetic entities (add, sub, mul, compare)
- CPU and GPU simulation support
- Comprehensive test suite
- Trait definitions (not yet integrated into compiler)

**Status:**
- ✅ Type system complete and tested
- ✅ Basic operations working in simulation
- 🚧 Entity-based operations defined, need validation
- ⏳ Transcendental functions planned
- ⏳ Optimizations and variants planned

---

## License

Same license as the main SKALP project.
