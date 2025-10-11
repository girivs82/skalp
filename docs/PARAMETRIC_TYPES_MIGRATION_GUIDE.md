# Migration Guide: Parametric Types

**Version:** 1.0
**Date:** 2025-10-11
**Audience:** SKALP developers migrating code to parametric types

---

## Overview

This guide helps you migrate existing SKALP code to use the new parametric type system. The parametric types provide:

- ✅ Better code reuse through generics
- ✅ Type-safe bit width handling
- ✅ Unified numeric operations via the Numeric trait
- ✅ Intent-driven optimization

---

## Table of Contents

1. [Before You Start](#before-you-start)
2. [Simple Bit Width Parameters](#simple-bit-width-parameters)
3. [Floating-Point Migration](#floating-point-migration)
4. [Fixed-Point Migration](#fixed-point-migration)
5. [Integer Migration](#integer-migration)
6. [Vector Operations](#vector-operations)
7. [Generic Algorithms](#generic-algorithms)
8. [Common Patterns](#common-patterns)
9. [Troubleshooting](#troubleshooting)

---

## Before You Start

### Compatibility

The new parametric types are **fully compatible** with existing SKALP code. You can migrate gradually:

- ✅ Old and new code can coexist
- ✅ No breaking changes to existing syntax
- ✅ Migrate module-by-module at your own pace

### Import Statements

Add these imports to use parametric types:

```skalp
// Float formats
use skalp::numeric::formats::{fp16, fp32, fp64, bf16, IEEE754_32, ...}

// Fixed-point and integer types
use skalp::numeric::{fixed, int, i8, i16, i32, i64, u8, u16, u32, u64}
use skalp::numeric::{q16_16, q8_8, q4_12, q15, q31}

// Vector types
use skalp::numeric::{vec, vec2, vec3, vec4}

// Numeric trait
use skalp::numeric::Numeric

// Intent profiles
use skalp::hls::{FAST_INTENT, SMALL_INTENT, LOW_POWER_INTENT, ...}
```

---

## Simple Bit Width Parameters

### Before: Hardcoded Bit Widths

```skalp
entity Adder8 {
    in a: bit<8>
    in b: bit<8>
    out sum: bit<8>
    out carry: bit
}

impl Adder8 {
    signal extended: bit<9>
    extended = a + b
    sum = extended[7:0]
    carry = extended[8]
}

// Duplicate for different widths
entity Adder16 {
    in a: bit<16>
    in b: bit<16>
    out sum: bit<16>
    out carry: bit
}
// ... more duplication ...
```

### After: Parametric Width

```skalp
entity Adder<const WIDTH: nat> {
    in a: bit[WIDTH]
    in b: bit[WIDTH]
    out sum: bit[WIDTH]
    out carry: bit
}

impl Adder<const WIDTH: nat> {
    signal extended: bit[WIDTH+1]
    extended = a + b
    sum = extended[WIDTH-1:0]
    carry = extended[WIDTH]
}

// Single definition, multiple instantiations
entity Top {
    let add8 = Adder<8> { ... }
    let add16 = Adder<16> { ... }
    let add32 = Adder<32> { ... }
}
```

### Benefits

- ✅ Write logic once, reuse everywhere
- ✅ Compile-time verification of bit widths
- ✅ Automatic width calculations (`WIDTH+1`)
- ✅ Reduced code duplication (1 entity vs N entities)

---

## Floating-Point Migration

### Before: Format-Specific Entities

```skalp
// FP32 addition
entity FP32Add {
    in a: fp32
    in b: fp32
    out result: fp32
    out flags: bit<5>
}

impl FP32Add {
    // Hardcoded for 32-bit float
    signal a_sign: bit = a[31]
    signal a_exp: bit<8> = a[30:23]
    signal a_mant: bit<23> = a[22:0]

    // ... FP32-specific logic ...
}

// FP16 addition (duplicated logic!)
entity FP16Add {
    in a: fp16
    in b: fp16
    out result: fp16
}

impl FP16Add {
    signal a_sign: bit = a[15]
    signal a_exp: bit<5> = a[14:10]
    signal a_mant: bit<10> = a[9:0]

    // ... same logic, different bit positions ...
}
```

### After: Generic Float Format

```skalp
use skalp::numeric::formats::FloatFormat

entity FpAdd<const F: FloatFormat> {
    in a: fp<F>
    in b: fp<F>
    out result: fp<F>
    out flags: bit<5>
}

impl FpAdd<const F: FloatFormat> {
    // Use format properties
    const TOTAL_BITS: nat = F.total_bits
    const EXP_BITS: nat = F.exponent_bits
    const MANT_BITS: nat = F.mantissa_bits
    const BIAS: int = F.bias

    // Extract fields using computed positions
    signal a_sign: bit = a[TOTAL_BITS-1]
    signal a_exp: bit[EXP_BITS] = a[TOTAL_BITS-2 : MANT_BITS]
    signal a_mant: bit[MANT_BITS] = a[MANT_BITS-1:0]

    // ... format-independent logic ...
}

// Single definition works for all formats
entity MixedPrecision {
    let add_fp16 = FpAdd<IEEE754_16> { ... }
    let add_fp32 = FpAdd<IEEE754_32> { ... }
    let add_bf16 = FpAdd<BFLOAT16> { ... }
}
```

### Migration Steps

1. **Identify format-specific constants:**
   ```skalp
   // Before
   const EXP_WIDTH: nat = 8
   const MANT_WIDTH: nat = 23

   // After
   const EXP_WIDTH: nat = F.exponent_bits
   const MANT_WIDTH: nat = F.mantissa_bits
   ```

2. **Replace bit slicing:**
   ```skalp
   // Before
   signal exp: bit<8> = value[30:23]

   // After
   signal exp: bit[F.exponent_bits] =
       value[F.total_bits-2 : F.mantissa_bits]
   ```

3. **Update type annotations:**
   ```skalp
   // Before
   in a: fp32

   // After
   in a: fp<F>
   ```

---

## Fixed-Point Migration

### Before: Q-Format Specific

```skalp
// Q16.16 multiplier
entity Q16_16_Mul {
    in a: bit<32>
    in b: bit<32>
    out result: bit<32>
}

impl Q16_16_Mul {
    signal product: bit<64> = a * b
    result = product[47:16]  // Extract Q16.16 result
}

// Q8.8 multiplier (duplicated!)
entity Q8_8_Mul {
    in a: bit<16>
    in b: bit<16>
    out result: bit<16>
}

impl Q8_8_Mul {
    signal product: bit<32> = a * b
    result = product[23:8]   // Extract Q8.8 result
}
```

### After: Generic Fixed-Point

```skalp
entity FixedMul<
    const WIDTH: nat,
    const FRAC: nat,
    const SIGNED: bool = true
> {
    in a: fixed<WIDTH, FRAC, SIGNED>
    in b: fixed<WIDTH, FRAC, SIGNED>
    out result: fixed<WIDTH, FRAC, SIGNED>
}

impl FixedMul<const WIDTH: nat, const FRAC: nat, const SIGNED: bool> {
    signal product: bit[WIDTH * 2] = a * b

    // Extract properly scaled result
    result = product[FRAC + WIDTH - 1 : FRAC]
}

// Single definition, multiple instantiations
entity DSPCore {
    let mul_q16_16 = FixedMul<32, 16, true> { ... }
    let mul_q8_8 = FixedMul<16, 8, true> { ... }

    // Or use type aliases
    let mul_q16 = FixedMul<q16_16> { ... }
}
```

### Common Q-Format Aliases

```skalp
// Before: manual bit widths
signal audio: bit<32>     // Q16.16
signal coeff: bit<16>     // Q4.12

// After: descriptive type aliases
signal audio: q16_16      // Q16.16
signal coeff: q4_12       // Q4.12
```

---

## Integer Migration

### Before: Width-Specific Integers

```skalp
entity Add32 {
    in a: bit<32>
    in b: bit<32>
    out sum: bit<32>
    out overflow: bit
}

entity Add64 {
    in a: bit<64>
    in b: bit<64>
    out sum: bit<64>
    out overflow: bit
}
```

### After: Generic Integer

```skalp
entity IntAdd<const WIDTH: nat, const SIGNED: bool = true> {
    in a: int<WIDTH, SIGNED>
    in b: int<WIDTH, SIGNED>
    out sum: int<WIDTH, SIGNED>
    out overflow: bit
}

impl IntAdd<const WIDTH: nat, const SIGNED: bool> {
    signal extended: int<WIDTH+1, SIGNED> = a + b

    sum = extended[WIDTH-1:0]

    // Overflow detection
    overflow = if SIGNED {
        (a[WIDTH-1] == b[WIDTH-1]) && (sum[WIDTH-1] != a[WIDTH-1])
    } else {
        extended[WIDTH]
    }
}

// Use standard aliases
entity Processor {
    let add32 = IntAdd<i32> { ... }
    let add64 = IntAdd<i64> { ... }
    let add_byte = IntAdd<u8> { ... }
}
```

---

## Vector Operations

### Before: Component-by-Component

```skalp
entity Vec3Add {
    in a_x: fp32
    in a_y: fp32
    in a_z: fp32
    in b_x: fp32
    in b_y: fp32
    in b_z: fp32
    out c_x: fp32
    out c_y: fp32
    out c_z: fp32
}

impl Vec3Add {
    c_x = a_x + b_x
    c_y = a_y + b_y
    c_z = a_z + b_z
}
```

### After: Vector Type

```skalp
entity VecAdd<T, const N: nat> {
    in a: vec<T, N>
    in b: vec<T, N>
    out c: vec<T, N>
}

impl VecAdd<T, const N: nat> {
    c = a + b  // Component-wise addition
}

// Clean, type-safe usage
entity Graphics {
    in pos_a: vec3<fp32>
    in pos_b: vec3<fp32>
    out pos_sum: vec3<fp32>
}

impl Graphics {
    let vec_add = VecAdd<fp32, 3> {
        a: pos_a,
        b: pos_b,
        c: pos_sum
    }

    // Or use operator directly
    pos_sum = pos_a + pos_b
}
```

### Vector Component Access

```skalp
// Before: separate signals
signal x: fp32
signal y: fp32
signal z: fp32

// After: vector with component access
signal position: vec3<fp32>
signal x: fp32 = position.x()
signal y: fp32 = position.y()
signal z: fp32 = position.z()

// Or use array indexing
signal x: fp32 = position[0]
signal y: fp32 = position[1]
signal z: fp32 = position[2]
```

---

## Generic Algorithms

### Before: Type-Specific Algorithms

```skalp
// FP32 accumulator
entity FP32Accumulator {
    in clk: clock
    in data: fp32
    out sum: fp32
}

impl FP32Accumulator {
    signal accumulator: fp32 = 0.0

    on(clk.rise) {
        accumulator <= accumulator + data
    }

    sum = accumulator
}

// i32 accumulator (duplicated logic!)
entity I32Accumulator {
    in clk: clock
    in data: i32
    out sum: i32
}

impl I32Accumulator {
    signal accumulator: i32 = 0

    on(clk.rise) {
        accumulator <= accumulator + data
    }

    sum = accumulator
}
```

### After: Generic with Numeric Trait

```skalp
entity Accumulator<T: Numeric> {
    in clk: clock
    in data: T
    in enable: bit
    out sum: T
}

impl Accumulator<T: Numeric> {
    signal accumulator: T = T::ZERO

    on(clk.rise) {
        if enable {
            accumulator <= accumulator.add(data)
        }
    }

    sum = accumulator
}

// Works with ANY numeric type!
entity DataPath {
    in fp_data: fp32
    in int_data: i32
    in fixed_data: q16_16

    out fp_sum: fp32
    out int_sum: i32
    out fixed_sum: q16_16
}

impl DataPath {
    let fp_acc = Accumulator<fp32> {
        data: fp_data,
        sum: fp_sum,
        ...
    }

    let int_acc = Accumulator<i32> {
        data: int_data,
        sum: int_sum,
        ...
    }

    let fixed_acc = Accumulator<q16_16> {
        data: fixed_data,
        sum: fixed_sum,
        ...
    }
}
```

### Trait-Based Operations

```skalp
// Generic min/max
entity MinMax<T: Numeric> {
    in a: T
    in b: T
    out min_val: T
    out max_val: T
}

impl MinMax<T: Numeric> {
    min_val = if a.lt(b) { a } else { b }
    max_val = if a.gt(b) { a } else { b }
}

// Works with fp32, i32, fixed, etc.
```

---

## Common Patterns

### Pattern 1: Adding Intent Parameters

```skalp
// Before: single implementation
entity FFT<const N: nat> {
    in data: vec<fp32, N>
    out result: vec<fp32, N>
}

// After: intent-driven specialization
entity FFT<const N: nat, intent I: Intent = DEFAULT_INTENT> {
    in data: vec<fp32, N>
    out result: vec<fp32, N>
}

impl FFT<const N: nat, intent I: Intent> {
    result = if is_latency_optimized(I) {
        fft_parallel(data)      // Fast, large area
    } else if is_area_optimized(I) {
        fft_sequential(data)    // Slow, small area
    } else {
        fft_pipelined(data)     // Balanced
    }
}

// Application chooses
let fft_fast = FFT<1024, FAST_INTENT> { ... }
let fft_small = FFT<1024, SMALL_INTENT> { ... }
```

### Pattern 2: Type Aliases for Common Configurations

```skalp
// Define common instantiations
type Adder8 = Adder<8>
type Adder16 = Adder<16>
type Adder32 = Adder<32>

type FastFFT1K = FFT<1024, FAST_INTENT>
type SmallFFT1K = FFT<1024, SMALL_INTENT>

// Use like concrete types
entity Top {
    let add = Adder32 { ... }
    let fft = FastFFT1K { ... }
}
```

### Pattern 3: Nested Generics

```skalp
// Before: specific combinations
entity FP32_Vec3_Add { ... }
entity FP16_Vec3_Add { ... }
entity FP32_Vec4_Add { ... }
// ... combinatorial explosion ...

// After: fully generic
entity VecAdd<T, const N: nat> {
    in a: vec<T, N>
    in b: vec<T, N>
    out c: vec<T, N>
}

// Any combination
let add1 = VecAdd<fp32, 3> { ... }  // vec3<fp32>
let add2 = VecAdd<fp16, 3> { ... }  // vec3<fp16>
let add3 = VecAdd<fp32, 4> { ... }  // vec4<fp32>
let add4 = VecAdd<i32, 2> { ... }   // vec2<i32>
```

---

## Troubleshooting

### Issue 1: Const Expression Errors

**Problem:**
```skalp
entity Buffer<const SIZE: nat> {
    signal data: bit[SIZE/8]  // Error: division not supported
}
```

**Solution:** Use supported const operations or precompute:
```skalp
entity Buffer<const SIZE: nat> {
    const BYTE_SIZE: nat = SIZE / 8  // Precompute
    signal data: bit[BYTE_SIZE]

    // Or use clog2 for power-of-2
    const ADDR_WIDTH: nat = clog2(SIZE)
}
```

### Issue 2: Type Inference Failures

**Problem:**
```skalp
entity Generic<T> {
    in a: T
}

let x = Generic { a: input }  // Error: cannot infer T
```

**Solution:** Provide explicit type:
```skalp
let x = Generic<fp32> { a: input }  // OK
```

### Issue 3: Where Clause Violations

**Problem:**
```skalp
entity Fixed<const WIDTH: nat, const FRAC: nat>
where FRAC <= WIDTH
{
    ...
}

let x = Fixed<8, 10> { ... }  // Error: 10 > 8
```

**Solution:** Ensure parameters satisfy constraints:
```skalp
let x = Fixed<16, 10> { ... }  // OK: 10 <= 16
```

### Issue 4: Intent Not Propagating

**Problem:**
```skalp
entity Top<intent I: Intent> {
    // Want to pass I to child
}

impl Top<intent I: Intent> {
    let child = Child<DEFAULT_INTENT> { ... }  // Uses default, not I
}
```

**Solution:** Explicitly pass intent parameter:
```skalp
impl Top<intent I: Intent> {
    let child = Child<I> { ... }  // Propagates intent
}
```

---

## Checklist

Use this checklist when migrating a module:

- [ ] Add necessary imports (`use skalp::numeric::...`)
- [ ] Replace hardcoded widths with const parameters
- [ ] Convert format-specific types to parametric types:
  - [ ] `fp32` → `fp<F>` or keep `fp32` (it's an alias)
  - [ ] `bit<32>` (fixed) → `fixed<W, F, S>` or `q16_16`
  - [ ] `bit<32>` (integer) → `int<W, S>` or `i32`/`u32`
- [ ] Update bit slicing to use computed positions
- [ ] Replace type-specific algorithms with `Numeric` trait
- [ ] Add intent parameters where appropriate
- [ ] Create type aliases for common configurations
- [ ] Update tests
- [ ] Document parameter constraints

---

## Benefits Summary

After migration, you get:

✅ **Reusability:** Write once, use with any width/format
✅ **Type Safety:** Compile-time verification of constraints
✅ **Maintainability:** Single source of truth
✅ **Optimization:** Intent-driven specialization
✅ **Flexibility:** Easy to support new formats/widths
✅ **Performance:** Zero runtime overhead

---

## Need Help?

- Check `PARAMETRIC_TYPES_GUIDE.md` for detailed usage
- See `examples/` for working code
- Review `LANGUAGE_SPECIFICATION.md` for formal spec
- Open an issue on GitHub for questions

Happy migrating! 🚀
