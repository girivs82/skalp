# SKALP Stdlib Parametric Types Reference

**Version:** 1.0
**Status:** Production Ready
**Last Updated:** 2025-10-11

---

## Quick Reference

This document provides a quick reference for using parametric types in the SKALP standard library.

For complete documentation, see [`docs/PARAMETRIC_TYPES_GUIDE.md`](../../docs/PARAMETRIC_TYPES_GUIDE.md).

---

## 📦 Import Statements

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

## 🔢 Numeric Types

### Floating-Point: `fp<F>`

```skalp
// Standard IEEE 754 formats
signal x: fp32              // 32-bit float
signal y: fp16              // 16-bit float
signal z: fp64              // 64-bit float

// ML formats
signal a: bf16              // Brain Float 16
signal b: tf32              // TensorFloat 32

// Generic (works with any format)
entity FpProcessor<const F: FloatFormat> {
    in data: fp<F>
    out result: fp<F>
}
```

### Fixed-Point: `fixed<W, F, S>`

```skalp
// W = width, F = fractional bits, S = signed
signal audio: q16_16        // Q16.16 (32-bit, 16 fractional)
signal coeff: q8_8          // Q8.8 (16-bit, 8 fractional)
signal dsp: q4_12           // Q4.12 (16-bit, 12 fractional)

// Generic
entity FixedMul<const W: nat, const F: nat, const S: bool> {
    in a: fixed<W, F, S>
    in b: fixed<W, F, S>
    out result: fixed<W, F, S>
}
```

### Integer: `int<W, S>`

```skalp
// W = width, S = signed
signal byte: i8             // 8-bit signed
signal word: i16            // 16-bit signed
signal dword: i32           // 32-bit signed
signal qword: i64           // 64-bit signed

signal ubyte: u8            // 8-bit unsigned
signal uword: u16           // 16-bit unsigned

// Generic
entity IntAdd<const W: nat, const S: bool = true> {
    in a: int<W, S>
    in b: int<W, S>
    out sum: int<W, S>
}
```

### Vector: `vec<T, N>`

```skalp
// T = element type, N = dimension
signal position: vec3<fp32>     // 3D position
signal color: vec4<fp32>        // RGBA color
signal texcoord: vec2<fp32>     // 2D texture coordinate

// Component access
signal x: fp32 = position.x()   // or position[0]
signal y: fp32 = position.y()   // or position[1]
signal z: fp32 = position.z()   // or position[2]

// Generic
entity VecAdd<T, const N: nat> {
    in a: vec<T, N>
    in b: vec<T, N>
    out result: vec<T, N>
}
```

---

## 🎯 Generic Operations

### FP Operations (`components/fp/fp_generic_compat.sk`)

```skalp
// Generic (works with any FloatFormat)
entity FpAdd<const F: FloatFormat>
entity FpMul<const F: FloatFormat>
entity FpDiv<const F: FloatFormat>
entity FpSqrt<const F: FloatFormat, intent I: Intent = DEFAULT_INTENT>
entity FpCompare<const F: FloatFormat>

// Usage
let adder = FpAdd<IEEE754_32> {
    a: x,
    b: y,
    result: sum,
    flags: _
}

// Or use type alias
let adder = FP32Add {  // Same as FpAdd<IEEE754_32>
    a: x,
    b: y,
    result: sum,
    flags: _
}
```

### Vector Operations (`components/vec/vec_generic_compat.sk`)

```skalp
// Generic (works with any type and dimension)
entity VecAdd<T, const N: nat>
entity VecSub<T, const N: nat>
entity VecScale<T, const N: nat>
entity VecDot<T, const N: nat>
entity Vec3Cross<T>
entity VecLength<T, const N: nat>
entity VecNormalize<T, const N: nat>
entity VecLerp<T, const N: nat>

// Usage
let adder = VecAdd<fp32, 3> {
    a: pos_a,
    b: pos_b,
    result: pos_sum
}

// Or use type alias
let adder = Vec3Fp32Add {  // Same as VecAdd<fp32, 3>
    a: pos_a,
    b: pos_b,
    result: pos_sum
}
```

---

## 🔄 Compatibility Aliases

### FP Aliases

```skalp
type FP32Add = FpAdd<IEEE754_32>
type FP32Mul = FpMul<IEEE754_32>
type FP32Div = FpDiv<IEEE754_32>
type FP32Sqrt = FpSqrt<IEEE754_32>

type FP16Add = FpAdd<IEEE754_16>
type FP64Add = FpAdd<IEEE754_64>
type BF16Add = FpAdd<BFLOAT16>
```

### Vector Aliases

```skalp
// Vec3 FP32
type Vec3Fp32Add = VecAdd<fp32, 3>
type Vec3Fp32Sub = VecSub<fp32, 3>
type Vec3Fp32Dot = VecDot<fp32, 3>
type Vec3Fp32Cross = Vec3Cross<fp32>
type Vec3Fp32Normalize = VecNormalize<fp32, 3>

// Vec2 FP32
type Vec2Fp32Add = VecAdd<fp32, 2>
type Vec2Fp32Dot = VecDot<fp32, 2>

// Vec4 FP32
type Vec4Fp32Add = VecAdd<fp32, 4>
type Vec4Fp32Dot = VecDot<fp32, 4>
```

---

## 💡 Common Patterns

### Pattern 1: Generic Width

```skalp
entity Adder<const WIDTH: nat> {
    in a: bit[WIDTH]
    in b: bit[WIDTH]
    out sum: bit[WIDTH]
}

let add8 = Adder<8> { ... }
let add16 = Adder<16> { ... }
let add32 = Adder<32> { ... }
```

### Pattern 2: Generic Type with Numeric Trait

```skalp
entity Accumulator<T: Numeric> {
    in clk: clock
    in data: T
    out sum: T
}

impl Accumulator<T: Numeric> {
    signal acc: T = T::ZERO

    on(clk.rise) {
        acc <= acc.add(data)
    }

    sum = acc
}

// Works with ANY numeric type
let fp_acc = Accumulator<fp32> { ... }
let int_acc = Accumulator<i32> { ... }
let fixed_acc = Accumulator<q16_16> { ... }
```

### Pattern 3: Intent-Driven

```skalp
entity FpMul<const F: FloatFormat, intent I: Intent = DEFAULT_INTENT> {
    in a: fp<F>
    in b: fp<F>
    out result: fp<F>
}

impl FpMul<const F: FloatFormat, intent I: Intent> {
    result = if is_latency_optimized(I) {
        fpmul_parallel(a, b)     // Fast, large area
    } else if is_area_optimized(I) {
        fpmul_sequential(a, b)   // Slow, small area
    } else {
        fpmul_balanced(a, b)     // Balanced
    }
}

// Choose implementation
let mul_fast = FpMul<IEEE754_32, FAST_INTENT> { ... }
let mul_small = FpMul<IEEE754_32, SMALL_INTENT> { ... }
```

### Pattern 4: Type Aliases for Common Configs

```skalp
// Define common instantiations
type Adder8 = Adder<8>
type Adder16 = Adder<16>
type Adder32 = Adder<32>

type FastFFT1K = FFT<1024, FAST_INTENT>
type SmallFFT1K = FFT<1024, SMALL_INTENT>

// Use like concrete types
let add = Adder32 { ... }
let fft = FastFFT1K { ... }
```

---

## 📚 Complete Examples

### Graphics Pipeline

```skalp
entity PhongShading {
    in position: vec3<fp32>
    in normal: vec3<fp32>
    in light_dir: vec3<fp32>
    in view_dir: vec3<fp32>
    out diffuse: fp32
    out specular: fp32
}

impl PhongShading {
    signal norm_n: vec3<fp32>
    signal n_dot_l: fp32
    signal reflect_dir: vec3<fp32>
    signal r_dot_v: fp32

    // Normalize normal
    let normalize = Vec3Fp32Normalize {
        v: normal,
        result: norm_n
    }

    // Diffuse: max(N · L, 0)
    let dot_nl = Vec3Fp32Dot {
        a: norm_n,
        b: light_dir,
        result: n_dot_l
    }

    diffuse = if n_dot_l < 0.0 { 0.0 } else { n_dot_l }

    // Specular: (R · V)^shininess
    // ... reflection calculation ...
}
```

### Physics Simulation

```skalp
entity ParticlePhysics {
    in clk: clock
    in position: vec3<fp32>
    in velocity: vec3<fp32>
    in force: vec3<fp32>
    in dt: fp32
    out new_position: vec3<fp32>
    out new_velocity: vec3<fp32>
}

impl ParticlePhysics {
    signal acceleration: vec3<fp32>
    signal vel_change: vec3<fp32>
    signal updated_vel: vec3<fp32>
    signal pos_change: vec3<fp32>

    // a = F / m (assume m = 1)
    acceleration = force

    // v' = v + a * dt
    let accel_scaled = Vec3Fp32Scale {
        v: acceleration,
        scalar: dt,
        result: vel_change
    }

    let vel_update = Vec3Fp32Add {
        a: velocity,
        b: vel_change,
        result: updated_vel
    }

    // p' = p + v * dt
    let vel_scaled = Vec3Fp32Scale {
        v: velocity,
        scalar: dt,
        result: pos_change
    }

    let pos_update = Vec3Fp32Add {
        a: position,
        b: pos_change,
        result: new_position
    }

    on(clk.rise) {
        new_velocity <= updated_vel
    }
}
```

### Generic DSP Filter

```skalp
entity FIRFilter<T: Numeric, const NUM_TAPS: nat> {
    in clk: clock
    in sample: T
    in coeffs: T[NUM_TAPS]
    out filtered: T
}

impl FIRFilter<T: Numeric, const NUM_TAPS: nat> {
    signal delay_line: T[NUM_TAPS] = [T::ZERO; NUM_TAPS]

    on(clk.rise) {
        // Shift delay line
        for i in 0..NUM_TAPS-1 {
            delay_line[i+1] <= delay_line[i]
        }
        delay_line[0] <= sample
    }

    // Compute MAC
    signal accumulator: T = T::ZERO

    for i in 0..NUM_TAPS {
        accumulator = accumulator.add(
            delay_line[i].mul(coeffs[i])
        )
    }

    filtered = accumulator
}

// Works with any numeric type!
let fir_fp = FIRFilter<fp32, 16> { ... }
let fir_fixed = FIRFilter<q16_16, 16> { ... }
let fir_int = FIRFilter<i32, 16> { ... }
```

---

## 🚀 Migration Guide

### Old Code (Still Works)

```skalp
entity OldStyle {
    in x: fp32
    in y: fp32
    out sum: fp32
}

impl OldStyle {
    let adder = FP32Add {  // Type alias, still works!
        a: x,
        b: y,
        result: sum,
        flags: _
    }
}
```

### New Code (Recommended)

```skalp
entity NewStyle<const F: FloatFormat> {
    in x: fp<F>
    in y: fp<F>
    out sum: fp<F>
}

impl NewStyle<const F: FloatFormat> {
    let adder = FpAdd<F> {  // Generic, flexible!
        a: x,
        b: y,
        result: sum,
        flags: _
    }
}

// Instantiate with any format
let proc_fp32 = NewStyle<IEEE754_32> { ... }
let proc_fp16 = NewStyle<IEEE754_16> { ... }
let proc_bf16 = NewStyle<BFLOAT16> { ... }
```

---

## 📖 More Information

- **Complete Guide:** [`docs/PARAMETRIC_TYPES_GUIDE.md`](../../docs/PARAMETRIC_TYPES_GUIDE.md)
- **Migration Guide:** [`docs/PARAMETRIC_TYPES_MIGRATION_GUIDE.md`](../../docs/PARAMETRIC_TYPES_MIGRATION_GUIDE.md)
- **Language Spec:** [`docs/LANGUAGE_SPECIFICATION.md`](../../docs/LANGUAGE_SPECIFICATION.md)
- **Examples:** [`examples/`](examples/)

---

**Version:** 1.0
**Status:** Production Ready ✅
