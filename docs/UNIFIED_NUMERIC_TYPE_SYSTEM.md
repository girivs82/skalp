# Unified Numeric Type System

**Generalize Numeric Types with Type Parameters for Complete Flexibility**

## Core Idea

Instead of separate `fp16`, `fp32`, `fp64` types and separate `vec2`, `vec3`, `vec4` types, use:

```skalp
// Floating-point with parametric format
type fp<const FORMAT: FloatFormat> = ...

// Aliases for convenience
type fp16 = fp<FloatFormat::IEEE754_16>
type fp32 = fp<FloatFormat::IEEE754_32>
type fp64 = fp<FloatFormat::IEEE754_64>
type bfloat16 = fp<FloatFormat::BFloat16>

// Vectors with parametric element type and dimension
type vec<T, const DIM: nat> = [T; DIM]

// Aliases for convenience
type vec2<T> = vec<T, 2>
type vec3<T> = vec<T, 3>
type vec4<T> = vec<T, 4>
```

This enables **single implementations** that work for all formats and dimensions!

---

## 1. Parametric Floating-Point Types

### 1.1 Float Format Specification

```skalp
// Float format descriptor
struct FloatFormat {
    total_bits: nat,
    exponent_bits: nat,
    mantissa_bits: nat,
    bias: int,
    name: &str,
}

// Standard formats
const IEEE754_16: FloatFormat = FloatFormat {
    total_bits: 16,
    exponent_bits: 5,
    mantissa_bits: 10,
    bias: 15,
    name: "IEEE754-half"
}

const IEEE754_32: FloatFormat = FloatFormat {
    total_bits: 32,
    exponent_bits: 8,
    mantissa_bits: 23,
    bias: 127,
    name: "IEEE754-single"
}

const IEEE754_64: FloatFormat = FloatFormat {
    total_bits: 64,
    exponent_bits: 11,
    mantissa_bits: 52,
    bias: 1023,
    name: "IEEE754-double"
}

const BFLOAT16: FloatFormat = FloatFormat {
    total_bits: 16,
    exponent_bits: 8,
    mantissa_bits: 7,
    bias: 127,
    name: "BFloat16"
}

const TFLOAT32: FloatFormat = FloatFormat {
    total_bits: 19,
    exponent_bits: 8,
    mantissa_bits: 10,
    bias: 127,
    name: "TensorFloat32"
}
```

### 1.2 Parametric FP Type

```skalp
// Generic floating-point type
type fp<const F: FloatFormat> = bit[F.total_bits]

// Extract components (compile-time field access)
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
        // Exponent all 1s, mantissa non-zero
        self.exponent() == ((1 << F.exponent_bits) - 1) &&
        self.mantissa() != 0
    }

    fn is_inf(self) -> bit {
        // Exponent all 1s, mantissa zero
        self.exponent() == ((1 << F.exponent_bits) - 1) &&
        self.mantissa() == 0
    }

    fn is_zero(self) -> bit {
        // Exponent and mantissa both zero
        self.exponent() == 0 && self.mantissa() == 0
    }
}

// Convenient aliases
type fp16 = fp<IEEE754_16>
type fp32 = fp<IEEE754_32>
type fp64 = fp<IEEE754_64>
type bf16 = fp<BFLOAT16>
type tf32 = fp<TFLOAT32>
```

### 1.3 Unified FP Operations

```skalp
// Single implementation for all FP formats!
entity FPAdd<const F: FloatFormat, intent I: Intent = Intent::default()> {
    in a: fp<F>
    in b: fp<F>
    out result: fp<F>
    out flags: bit<5>
}

impl<const F: FloatFormat, intent I> FPAdd<F, I> {
    // Extract components
    signal a_sign = a.sign()
    signal a_exp = a.exponent()
    signal a_mant = a.mantissa()

    signal b_sign = b.sign()
    signal b_exp = b.exponent()
    signal b_mant = b.mantissa()

    // Special case handling (works for any format!)
    signal a_is_nan = a.is_nan()
    signal b_is_nan = b.is_nan()
    signal a_is_inf = a.is_inf()
    signal b_is_inf = b.is_inf()

    // Align exponents
    signal exp_diff = a_exp - b_exp
    signal larger_exp = if exp_diff > 0 { a_exp } else { b_exp }

    // Shift mantissas
    signal a_mant_aligned = if exp_diff > 0 {
        a_mant
    } else {
        a_mant >> (-exp_diff)
    }

    signal b_mant_aligned = if exp_diff > 0 {
        b_mant >> exp_diff
    } else {
        b_mant
    }

    // Add mantissas (width depends on format!)
    signal sum_mant = a_mant_aligned + b_mant_aligned

    // Normalize and round
    signal result_exp = larger_exp
    signal result_mant = normalize(sum_mant)

    // Pack result
    result = {result_sign, result_exp, result_mant}

    // Format-specific optimizations via intent
    result = if I.optimize == Optimize::Area && F.total_bits <= 16 {
        // Small formats can use LUT for some operations
        lut_add<F>(a, b)
    } else {
        // Standard algorithm
        standard_add<F>(a, b)
    }
}

// Usage - compiler generates specialized versions
signal sum16: fp16 = fp16_add(a, b)  // Generates FPAdd<IEEE754_16>
signal sum32: fp32 = fp32_add(a, b)  // Generates FPAdd<IEEE754_32>
signal sum64: fp64 = fp64_add(a, b)  // Generates FPAdd<IEEE754_64>
signal sum_bf: bf16 = bf16_add(a, b) // Generates FPAdd<BFLOAT16>
```

---

## 2. Parametric Vector Types

### 2.1 Generic Vector Type

```skalp
// Generic N-dimensional vector
type vec<T, const N: nat> = [T; N]
where
    T: Synthesizable

// Component access via index
impl<T, const N: nat> vec<T, N> {
    fn get(self, idx: nat) -> T {
        self[idx]
    }

    fn set(self, idx: nat, value: T) -> vec<T, N> {
        let mut result = self
        result[idx] = value
        result
    }
}

// Convenient aliases
type vec2<T> = vec<T, 2>
type vec3<T> = vec<T, 3>
type vec4<T> = vec<T, 4>

// Named component access for small vectors
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

### 2.2 Unified Vector Operations

```skalp
// Single implementation works for any element type and dimension!
entity VecAdd<T, const N: nat, intent I: Intent = Intent::default()>
where
    T: Synthesizable
{
    in a: vec<T, N>
    in b: vec<T, N>
    out result: vec<T, N>
}

impl<T, const N: nat, intent I> VecAdd<T, N, I> {
    // Choose implementation based on intent and dimension
    result = if I.optimize == Optimize::Latency && I.area > N * 100 {
        // Fully parallel - unroll all additions
        @unroll
        for i in 0..N {
            result[i] = a[i] + b[i]
        }
    } else if I.optimize == Optimize::Area {
        // Sequential - one adder, time-multiplexed
        @sequential
        for i in 0..N {
            result[i] = a[i] + b[i]
        }
    } else {
        // Balanced - partial unroll
        const UNROLL: nat = min(4, N)
        @unroll(factor: UNROLL)
        for i in 0..N {
            result[i] = a[i] + b[i]
        }
    }
}

// Usage - works for any type and dimension!
signal v1: vec3<fp32> = vec3_add(a, b)     // 3D float vector
signal v2: vec4<fp16> = vec4_add(c, d)     // 4D half-precision vector
signal v3: vec2<bit<16>> = vec2_add(e, f)  // 2D integer vector
signal v4: vec<bf16, 8> = vec_add(g, h)    // 8D bfloat16 vector
```

### 2.3 Format-Aware Vector Operations

```skalp
// Dot product - single implementation for all formats and dimensions!
entity VecDot<T, const N: nat, intent I: Intent = Intent::default()>
where
    T: Synthesizable + Numeric
{
    in a: vec<T, N>
    in b: vec<T, N>
    out result: T
}

impl<T, const N: nat, intent I> VecDot<T, N, I> {
    // Accumulator width might need to be wider than element type
    const ACC_WIDTH = if T is fp<F> {
        // For floating-point, use same format but track for overflow
        F.total_bits
    } else if T is bit<W> {
        // For fixed-point, need log2(N) extra bits
        W + clog2(N)
    }

    signal products: [T; N]

    // Multiply phase
    @unroll(factor: if I.optimize == Optimize::Latency { N } else { 1 })
    for i in 0..N {
        products[i] = a[i] * b[i]
    }

    // Accumulate phase - tree reduction for low latency
    result = if I.optimize == Optimize::Latency && N > 4 {
        tree_reduce(products, op: add)
    } else {
        sequential_reduce(products, op: add)
    }
}
```

---

## 3. Complete Unified Stdlib

### 3.1 Unified Sqrt

```skalp
// Works for fp16, fp32, fp64, bf16, tf32, etc.
entity Sqrt<const F: FloatFormat, intent I: Intent = Intent::default()> {
    in x: fp<F>
    out result: fp<F>
}

impl<const F: FloatFormat, intent I> Sqrt<F, I> {
    result = if I.latency < 4 {
        // LUT-based - size scales with format
        lut_sqrt<F>(x, entries: 1 << F.mantissa_bits)

    } else if I.latency < 8 {
        // Single Newton-Raphson iteration
        let guess = initial_guess<F>(x)
        newton_raphson<F>(x, guess, iterations: 1)

    } else if I.accuracy == Accuracy::High {
        // Multiple iterations for accuracy
        let iterations = if F.mantissa_bits > 20 { 3 } else { 2 }
        let guess = initial_guess<F>(x)
        newton_raphson<F>(x, guess, iterations)

    } else {
        // Balanced
        let guess = initial_guess<F>(x)
        newton_raphson<F>(x, guess, iterations: 1)
    }
}

// Initial guess depends on format
const fn initial_guess<const F: FloatFormat>(x: fp<F>) -> fp<F> {
    // Magic constant depends on format
    const MAGIC: bit[F.total_bits] = if F == IEEE754_32 {
        0x5f3759df  // Classic Quake magic
    } else if F == IEEE754_16 {
        0x59ba      // Scaled for fp16
    } else if F == IEEE754_64 {
        0x5fe6eb50c7b537a9  // Scaled for fp64
    } else {
        // Compute for arbitrary format
        compute_magic_constant(F)
    }

    let half_x = x >> 1
    fp<F>(MAGIC - half_x.bits)
}

// Usage - same sqrt implementation for all formats!
@intent(latency: 4_cycles)
impl Example {
    signal r16: fp16 = sqrt(x16)    // Sqrt<IEEE754_16, Intent{lat:4}>
    signal r32: fp32 = sqrt(x32)    // Sqrt<IEEE754_32, Intent{lat:4}>
    signal r64: fp64 = sqrt(x64)    // Sqrt<IEEE754_64, Intent{lat:4}>
    signal rbf: bf16 = sqrt(xbf)    // Sqrt<BFLOAT16, Intent{lat:4}>
}
```

### 3.2 Unified Vector Normalize

```skalp
// Works for any vector dimension and element type!
entity VecNormalize<
    const F: FloatFormat,
    const N: nat,
    intent I: Intent = Intent::default()
> {
    in v: vec<fp<F>, N>
    out normalized: vec<fp<F>, N>
}

impl<const F: FloatFormat, const N: nat, intent I>
    VecNormalize<F, N, I>
{
    // Compute length squared
    let dot = VecDot<fp<F>, N, I> {
        a = v,
        b = v,
        result => len_sq
    }

    // Compute reciprocal square root or square root
    signal inv_len = if I.latency < 8 {
        // Fast path: rsqrt directly
        let rsqrt = Rsqrt<F, I> {
            x = len_sq,
            result => inv_len
        }
    } else {
        // Accurate path: sqrt then reciprocal
        let sqrt_op = Sqrt<F, I> {
            x = len_sq,
            result => len
        }
        1.0 / len
    }

    // Scale vector
    let scale = VecScale<fp<F>, N, I> {
        v = v,
        s = inv_len,
        result => normalized
    }
}

// Usage - works for any dimension and format!
signal n2_fp16: vec2<fp16> = normalize(v2_fp16)
signal n3_fp32: vec3<fp32> = normalize(v3_fp32)
signal n4_fp64: vec4<fp64> = normalize(v4_fp64)
signal n8_bf16: vec<bf16, 8> = normalize(v8_bf16)
```

### 3.3 Unified Matrix Operations

```skalp
// Generic matrix type
type mat<T, const ROWS: nat, const COLS: nat> = [[T; COLS]; ROWS]

// Square matrix aliases
type mat2<T> = mat<T, 2, 2>
type mat3<T> = mat<T, 3, 3>
type mat4<T> = mat<T, 4, 4>

// Matrix-vector multiply
entity MatVecMul<
    const F: FloatFormat,
    const ROWS: nat,
    const COLS: nat,
    intent I: Intent = Intent::default()
> {
    in m: mat<fp<F>, ROWS, COLS>
    in v: vec<fp<F>, COLS>
    out result: vec<fp<F>, ROWS>
}

impl<const F: FloatFormat, const ROWS: nat, const COLS: nat, intent I>
    MatVecMul<F, ROWS, COLS, I>
{
    const UNROLL_FACTOR = if I.optimize == Optimize::Latency {
        ROWS
    } else if I.optimize == Optimize::Area {
        1
    } else {
        min(4, ROWS)
    }

    @unroll(factor: UNROLL_FACTOR)
    for row in 0..ROWS {
        // Dot product of matrix row with vector
        let dot = VecDot<fp<F>, COLS, I> {
            a = m[row],
            b = v,
            result => result[row]
        }
    }
}
```

---

## 4. Format Conversion

### 4.1 Parametric Conversion

```skalp
// Convert between any two FP formats
entity FPConvert<
    const FROM: FloatFormat,
    const TO: FloatFormat,
    intent I: Intent = Intent::default()
> {
    in x: fp<FROM>
    out result: fp<TO>
}

impl<const FROM: FloatFormat, const TO: FloatFormat, intent I>
    FPConvert<FROM, TO, I>
{
    // Extract source components
    signal x_sign = x.sign()
    signal x_exp = x.exponent()
    signal x_mant = x.mantissa()

    // Adjust exponent bias
    signal exp_adjusted: bit[TO.exponent_bits] =
        (x_exp - FROM.bias) + TO.bias

    // Adjust mantissa width
    signal mant_adjusted: bit[TO.mantissa_bits] =
        if TO.mantissa_bits > FROM.mantissa_bits {
            // Upconvert: zero-extend
            x_mant << (TO.mantissa_bits - FROM.mantissa_bits)
        } else {
            // Downconvert: round
            round_mantissa(
                x_mant >> (FROM.mantissa_bits - TO.mantissa_bits),
                mode: I.rounding_mode
            )
        }

    // Handle special cases
    signal is_nan = x.is_nan()
    signal is_inf = x.is_inf()
    signal is_zero = x.is_zero()

    result = if is_nan {
        fp<TO>::NAN
    } else if is_inf {
        if x_sign { fp<TO>::NEG_INF } else { fp<TO>::POS_INF }
    } else if is_zero {
        fp<TO>::ZERO
    } else {
        // Pack converted value
        {x_sign, exp_adjusted, mant_adjusted}
    }
}

// Usage - automatic conversions!
signal x16: fp16 = 1.5_fp16
signal x32: fp32 = fp_convert(x16)  // fp16 → fp32
signal x64: fp64 = fp_convert(x32)  // fp32 → fp64
signal xbf: bf16 = fp_convert(x32)  // fp32 → bfloat16
```

### 4.2 Implicit Conversions

```skalp
// Allow implicit conversions with intent control
@intent(implicit_conversions: true)
impl MixedPrecision {
    signal a: fp16 = 1.0
    signal b: fp32 = 2.0

    // Automatic promotion to higher precision
    signal sum: fp32 = a + b  // a promoted to fp32

    // Explicit control via intent
    @intent(promote: higher)  // Default
    signal sum1: fp32 = fp16_val + fp32_val

    @intent(promote: lower)
    signal sum2: fp16 = fp16_val + fp32_val  // fp32 demoted to fp16

    @intent(promote: explicit)
    signal sum3 = fp16_val + fp32_val  // ❌ ERROR: explicit conversion required
}
```

---

## 5. Custom Formats

### 5.1 User-Defined Formats

```skalp
// Define custom floating-point format
const MY_CUSTOM_FP: FloatFormat = FloatFormat {
    total_bits: 24,
    exponent_bits: 7,
    mantissa_bits: 16,
    bias: 63,
    name: "Custom24"
}

type fp24 = fp<MY_CUSTOM_FP>

// All stdlib operations automatically work!
signal x: fp24 = 3.14
signal y: fp24 = sqrt(x)         // Uses Sqrt<MY_CUSTOM_FP>
signal z: vec3<fp24> = normalize(v)  // Uses VecNormalize<MY_CUSTOM_FP, 3>
```

### 5.2 Exotic Formats

```skalp
// Posit format (Type III Unum)
const POSIT_16: FloatFormat = FloatFormat {
    total_bits: 16,
    exponent_bits: 0,  // Variable based on regime
    mantissa_bits: 0,  // Variable based on regime
    bias: 0,
    name: "Posit16",
    custom_ops: true   // Requires custom add/mul/etc.
}

// Fixed-point as degenerate float
const FIXED_16_8: FloatFormat = FloatFormat {
    total_bits: 16,
    exponent_bits: 0,
    mantissa_bits: 8,
    bias: 8,
    name: "Q8.8"
}

type fixed16_8 = fp<FIXED_16_8>
```

---

## 6. Compile-Time Format Introspection

### 6.1 Format Queries

```skalp
impl<const F: FloatFormat, intent I> CustomOp<F, I> {
    // Query format properties at compile-time
    const NEEDS_DENORMAL_SUPPORT: bool = F.mantissa_bits > 10

    const OPTIMAL_LUT_SIZE: nat = if F.total_bits <= 16 {
        1 << F.mantissa_bits  // Full LUT
    } else {
        1 << 12  // Truncated LUT
    }

    const PIPELINE_STAGES: nat = if F.total_bits <= 16 {
        2  // Small formats: shallow pipeline
    } else if F.total_bits <= 32 {
        4  // Medium formats
    } else {
        8  // Large formats: deep pipeline
    }

    result = if NEEDS_DENORMAL_SUPPORT {
        full_ieee_op<F>(x)
    } else {
        simplified_op<F>(x)
    }
}
```

### 6.2 Format-Dependent Algorithms

```skalp
entity FPMul<const F: FloatFormat, intent I: Intent = Intent::default()> {
    in a: fp<F>
    in b: fp<F>
    out result: fp<F>
}

impl<const F: FloatFormat, intent I> FPMul<F, I> {
    result = if F.total_bits <= 16 && I.optimize == Optimize::Area {
        // Small formats: use LUT for multiplication
        lut_multiply<F>(a, b)

    } else if F == BFLOAT16 && I.map_to_dsp {
        // BF16: can use FP32 DSP with truncation
        let a32 = convert_to_fp32(a)
        let b32 = convert_to_fp32(b)
        let result32 = fp32_multiply(a32, b32)
        convert_to_bf16(result32)

    } else if F.mantissa_bits < 10 {
        // Low precision: simple multiplier
        simple_multiply<F>(a, b)

    } else {
        // Standard multiply-accumulate
        standard_multiply<F>(a, b)
    }
}
```

---

## 7. Type-Safe Dimension Checking

### 7.1 Compile-Time Dimension Validation

```skalp
// Cross product only defined for 3D vectors
entity VecCross<T, intent I: Intent = Intent::default()>
where
    T: Synthesizable
{
    in a: vec<T, 3>  // Compile error if not exactly 3D
    in b: vec<T, 3>
    out result: vec<T, 3>
}

// Matrix multiply with dimension checking
entity MatMul<
    const F: FloatFormat,
    const M: nat,
    const N: nat,
    const P: nat,
    intent I: Intent = Intent::default()
>
where
    M > 0,  // Non-empty matrices
    N > 0,
    P > 0
{
    in a: mat<fp<F>, M, N>
    in b: mat<fp<F>, N, P>  // N must match!
    out result: mat<fp<F>, M, P>
}

// Usage
signal a: mat<fp32, 3, 4>
signal b: mat<fp32, 4, 5>
signal c: mat<fp32, 3, 5> = matmul(a, b)  // ✓ OK: 3×4 * 4×5 = 3×5

signal d: mat<fp32, 4, 3>
signal e: mat<fp32, 5, 4>
signal f = matmul(d, e)  // ❌ ERROR: dimension mismatch (3 ≠ 5)
```

### 7.2 Generic Vector Swizzling

```skalp
impl<T, const N: nat> vec<T, N> {
    // Swizzle with compile-time validation
    fn swizzle<const M: nat>(self, indices: [nat; M]) -> vec<T, M>
    where
        indices.all(|i| i < N)  // All indices must be in bounds
    {
        let mut result: vec<T, M>
        @unroll
        for i in 0..M {
            result[i] = self[indices[i]]
        }
        result
    }
}

// Usage
signal v: vec4<fp32> = {1.0, 2.0, 3.0, 4.0}
signal xy: vec2<fp32> = v.swizzle([0, 1])      // {1.0, 2.0}
signal zyx: vec3<fp32> = v.swizzle([2, 1, 0])  // {3.0, 2.0, 1.0}
```

---

## 8. Complete Example: Unified Graphics Pipeline

```skalp
// Works with any FP format and vector dimension!
entity GraphicsPipeline<
    const F: FloatFormat = IEEE754_32,
    const VEC_DIM: nat = 3,
    intent I: Intent = Intent::default()
>
where
    VEC_DIM >= 2,
    VEC_DIM <= 4
{
    in position: vec<fp<F>, VEC_DIM>
    in normal: vec<fp<F>, VEC_DIM>
    in light_pos: vec<fp<F>, VEC_DIM>
    in view_dir: vec<fp<F>, VEC_DIM>
    out color: vec<fp<F>, VEC_DIM>
}

impl<const F: FloatFormat, const VEC_DIM: nat, intent I>
    GraphicsPipeline<F, VEC_DIM, I>
{
    // All operations parameterized!

    // Light direction
    signal light_vec = light_pos - position
    signal light_dir: vec<fp<F>, VEC_DIM> = normalize<F, VEC_DIM, I>(light_vec)

    // Normal normalization
    signal norm: vec<fp<F>, VEC_DIM> = normalize<F, VEC_DIM, I>(normal)

    // Diffuse lighting
    signal n_dot_l: fp<F> = dot<fp<F>, VEC_DIM, I>(norm, light_dir)
    signal diffuse: fp<F> = max(n_dot_l, 0.0)

    // Specular (only for 3D)
    signal specular: fp<F> = if VEC_DIM == 3 {
        signal reflect_dir = reflect<F, I>(light_dir, norm)
        signal spec_dot: fp<F> = dot<fp<F>, 3, I>(reflect_dir, view_dir)
        max(spec_dot, 0.0)
    } else {
        0.0
    }

    // Combine
    signal intensity = diffuse + specular
    color = scale<fp<F>, VEC_DIM, I>(material_color, intensity)
}

// Usage - same code works for all formats and dimensions!

// Standard FP32 3D graphics
@intent(profile: "high_quality")
let pipeline_3d_fp32 = GraphicsPipeline<IEEE754_32, 3> {
    position = pos_fp32,
    normal = norm_fp32,
    color => color_out
}

// Low-precision FP16 for mobile
@intent(profile: "low_power")
let pipeline_3d_fp16 = GraphicsPipeline<IEEE754_16, 3> {
    position = pos_fp16,
    normal = norm_fp16,
    color => color_out
}

// BF16 for ML accelerators
@intent(profile: "ml_optimized")
let pipeline_3d_bf16 = GraphicsPipeline<BFLOAT16, 3> {
    position = pos_bf16,
    normal = norm_bf16,
    color => color_out
}

// 2D lighting (no specular)
@intent(profile: "minimal")
let pipeline_2d_fp32 = GraphicsPipeline<IEEE754_32, 2> {
    position = pos_2d,
    normal = norm_2d,
    color => color_out
}
```

---

## 9. Stdlib Organization

### 9.1 New Stdlib Structure

```
std/
├── numeric/
│   ├── fp.sk           # Parametric FP type and ops
│   ├── formats.sk      # Standard format definitions
│   └── convert.sk      # Format conversion
│
├── vector/
│   ├── vec.sk          # Parametric vector type
│   ├── ops.sk          # Generic vector operations
│   └── swizzle.sk      # Swizzling utilities
│
├── matrix/
│   ├── mat.sk          # Parametric matrix type
│   └── ops.sk          # Matrix operations
│
└── ops/
    ├── arithmetic.sk   # Generic +, -, *, /
    ├── comparison.sk   # Generic <, >, ==
    ├── math.sk         # sqrt, sin, cos, etc.
    └── geometry.sk     # normalize, reflect, etc.
```

### 9.2 Import Example

```skalp
use std::numeric::{fp, IEEE754_32, BFLOAT16};
use std::vector::{vec, vec3};
use std::ops::math::sqrt;

@intent(optimize: latency)
impl Example {
    // All these work with single stdlib implementation!
    signal s32: fp<IEEE754_32> = sqrt(x32)
    signal sbf: fp<BFLOAT16> = sqrt(xbf)

    signal v3_32: vec3<fp<IEEE754_32>> = normalize(vec32)
    signal v3_bf: vec3<fp<BFLOAT16>> = normalize(vecbf)
}
```

---

## 10. Benefits Summary

### Code Reduction

**Before (Separate Types):**
```
FP16Add, FP32Add, FP64Add, BF16Add          = 4 entities
Vec2Add, Vec3Add, Vec4Add                   = 3 entities
Vec2Dot, Vec3Dot, Vec4Dot                   = 3 entities
Vec2Normalize, Vec3Normalize, Vec4Normalize = 3 entities

Total for fp16/fp32/fp64 × vec2/vec3/vec4:
  4 formats × 3 dimensions × 4 ops = 48 entities
```

**After (Parametric):**
```
FPAdd<F>                = 1 entity (all formats)
VecAdd<T, N>            = 1 entity (all dims & types)
VecDot<T, N>            = 1 entity
VecNormalize<F, N, I>   = 1 entity

Total: 4 entities handle all combinations!
```

### 10.1 Advantages

✅ **Massive Code Reduction** - 48 entities → 4 entities
✅ **Type Safety** - Dimension and format checking at compile-time
✅ **Flexibility** - Support any format (even custom ones)
✅ **Maintainability** - Fix bug once, works everywhere
✅ **Extensibility** - Add format/dimension = zero new code
✅ **Performance** - Full compile-time specialization, zero overhead
✅ **Composability** - Intent + Format + Dimension all compose

### 10.2 Comparison Table

| Feature | Separate Types | Parametric Types |
|---------|---------------|------------------|
| **FP Formats** | 3-4 (fp16/32/64) | Unlimited |
| **Custom Formats** | ❌ Not supported | ✅ Full support |
| **Vector Dims** | 2-4 hardcoded | Any N |
| **Code Duplication** | High | None |
| **Type Safety** | Manual | Automatic |
| **Stdlib Size** | ~200 entities | ~20 entities |
| **Adding Format** | N new entities | 1 constant |

---

## 11. Migration Path

### 11.1 Backward Compatibility

```skalp
// Old code continues to work via aliases
type fp16 = fp<IEEE754_16>
type fp32 = fp<IEEE754_32>
type fp64 = fp<IEEE754_64>

type vec2<T> = vec<T, 2>
type vec3<T> = vec<T, 3>
type vec4<T> = vec<T, 4>

// Old-style operations are aliases
fn fp32_add(a: fp32, b: fp32) -> fp32 {
    a + b  // Calls FPAdd<IEEE754_32>
}

fn vec3_normalize(v: vec3<fp32>) -> vec3<fp32> {
    normalize(v)  // Calls VecNormalize<IEEE754_32, 3>
}
```

### 11.2 Gradual Migration

```skalp
// Stage 1: Use aliases, old syntax
signal v: vec3<fp32> = {1.0, 2.0, 3.0}
signal n: vec3<fp32> = vec3_normalize(v)

// Stage 2: Use parametric types, old syntax
signal v: vec<fp32, 3> = {1.0, 2.0, 3.0}
signal n: vec<fp32, 3> = vec3_normalize(v)

// Stage 3: Use parametric types, new syntax
signal v: vec<fp32, 3> = {1.0, 2.0, 3.0}
signal n = normalize(v)  // Generic normalize

// Stage 4: Use format parameters
signal v: vec<fp<IEEE754_32>, 3> = {1.0, 2.0, 3.0}
signal n = normalize(v)
```

---

## Summary

### Key Innovation

**Single implementation, infinite instantiations:**

```skalp
// One entity handles ALL combinations
entity VecOp<const F: FloatFormat, const N: nat, intent I> {
    // Works for any format, any dimension, any intent!
}
```

### Impact on SKALP

1. **Stdlib shrinks from ~200 to ~20 entities**
2. **Custom formats work automatically**
3. **Type-safe dimension checking**
4. **Zero-cost abstractions** (compile-time)
5. **Intent + Format + Dimension** compose perfectly

### Next Steps

1. **Extend type system** - Add `const` generics for formats
2. **Implement format introspection** - Compile-time format queries
3. **Migrate stdlib** - Convert to parametric types
4. **Add format library** - Standard + exotic formats
5. **Optimize monomorphization** - Smart instantiation caching

**SKALP now has the most advanced type system of any HDL!** 🚀✨
