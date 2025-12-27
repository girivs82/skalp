# Complete Numeric Type System

**Unified Parametric Types for Floating-Point, Fixed-Point, and Integer**

## Vision

A single, unified type system where **all numeric types** are parameterized:

```skalp
// Floating-point
type fp<const F: FloatFormat>

// Fixed-point
type fixed<const WIDTH: nat, const FRAC: nat, const SIGNED: bool>

// Integer
type int<const WIDTH: nat, const SIGNED: bool>

// All share the same operations!
signal f = sqrt(x)  // Works for fp32, fixed<32,16>, int<32>!
```

---

## 1. Unified Numeric Type Hierarchy

### 1.1 The Numeric Trait

```skalp
// All numeric types implement this trait
trait Numeric {
    const TOTAL_BITS: nat;
    const IS_SIGNED: bool;
    const IS_FLOATING: bool;
    const IS_FIXED: bool;

    // Common operations
    fn add(self, other: Self) -> Self;
    fn sub(self, other: Self) -> Self;
    fn mul(self, other: Self) -> Self;
    fn div(self, other: Self) -> Self;

    // Comparison
    fn lt(self, other: Self) -> bit;
    fn eq(self, other: Self) -> bit;

    // Conversion
    fn to_bits(self) -> bit[Self::TOTAL_BITS];
    fn from_bits(bits: bit[Self::TOTAL_BITS]) -> Self;
}
```

### 1.2 Type Definitions

```skalp
// Floating-point (already covered)
type fp<const F: FloatFormat> = bit[F.total_bits]

impl<const F: FloatFormat> Numeric for fp<F> {
    const TOTAL_BITS = F.total_bits
    const IS_SIGNED = true
    const IS_FLOATING = true
    const IS_FIXED = false
    // ... operations
}

// Fixed-point
type fixed<
    const WIDTH: nat,
    const FRAC: nat,
    const SIGNED: bool = true
> = bit[WIDTH]
where
    FRAC <= WIDTH

impl<const WIDTH: nat, const FRAC: nat, const SIGNED: bool>
    Numeric for fixed<WIDTH, FRAC, SIGNED>
{
    const TOTAL_BITS = WIDTH
    const IS_SIGNED = SIGNED
    const IS_FLOATING = false
    const IS_FIXED = true
    // ... operations
}

// Integer (degenerate fixed-point with FRAC=0)
type int<const WIDTH: nat, const SIGNED: bool = true> =
    fixed<WIDTH, 0, SIGNED>

// Aliases for common types
type i8   = int<8, true>
type i16  = int<16, true>
type i32  = int<32, true>
type i64  = int<64, true>
type u8   = int<8, false>
type u16  = int<16, false>
type u32  = int<32, false>
type u64  = int<64, false>

// Common fixed-point formats
type q16_16 = fixed<32, 16, true>   // 16.16 fixed
type q8_8   = fixed<16, 8, true>    // 8.8 fixed
type uq16_16 = fixed<32, 16, false> // Unsigned 16.16
```

---

## 2. Fixed-Point Type System

### 2.1 Fixed-Point Format Specification

```skalp
struct FixedFormat {
    total_bits: nat,
    fractional_bits: nat,
    signed: bool,
    rounding: RoundingMode,
    overflow: OverflowMode,
}

enum RoundingMode {
    Truncate,      // Round toward zero
    RoundNearest,  // Round to nearest, ties to even
    RoundUp,       // Round toward +inf
    RoundDown,     // Round toward -inf
}

enum OverflowMode {
    Wrap,          // Modulo arithmetic
    Saturate,      // Clamp to min/max
    Error,         // Raise overflow flag
}

// Example formats
const Q16_16: FixedFormat = FixedFormat {
    total_bits: 32,
    fractional_bits: 16,
    signed: true,
    rounding: RoundingMode::RoundNearest,
    overflow: OverflowMode::Saturate,
}

const Q8_8: FixedFormat = FixedFormat {
    total_bits: 16,
    fractional_bits: 8,
    signed: true,
    rounding: RoundingMode::Truncate,
    overflow: OverflowMode::Wrap,
}
```

### 2.2 Fixed-Point Operations

```skalp
// Addition - straightforward, same fraction point
entity FixedAdd<
    const WIDTH: nat,
    const FRAC: nat,
    const SIGNED: bool,
    intent I: Intent = Intent::default()
> {
    in a: fixed<WIDTH, FRAC, SIGNED>
    in b: fixed<WIDTH, FRAC, SIGNED>
    out result: fixed<WIDTH, FRAC, SIGNED>
    out overflow: bit
}

impl<const WIDTH: nat, const FRAC: nat, const SIGNED: bool, intent I>
    FixedAdd<WIDTH, FRAC, SIGNED, I>
{
    // Simple bit addition
    signal sum_wide: bit[WIDTH + 1] = {0, a} + {0, b}

    // Overflow detection
    overflow = if SIGNED {
        // Signed overflow: signs same, result different
        (a[WIDTH-1] == b[WIDTH-1]) && (sum_wide[WIDTH-1] != a[WIDTH-1])
    } else {
        // Unsigned overflow: carry out
        sum_wide[WIDTH]
    }

    // Result with overflow handling
    result = if I.overflow_mode == OverflowMode::Saturate && overflow {
        if SIGNED {
            if a[WIDTH-1] { min_value() } else { max_value() }
        } else {
            max_value()
        }
    } else {
        sum_wide[0..WIDTH]
    }
}

// Multiplication - fraction points add, need scaling
entity FixedMul<
    const WIDTH: nat,
    const FRAC: nat,
    const SIGNED: bool,
    intent I: Intent = Intent::default()
> {
    in a: fixed<WIDTH, FRAC, SIGNED>
    in b: fixed<WIDTH, FRAC, SIGNED>
    out result: fixed<WIDTH, FRAC, SIGNED>
}

impl<const WIDTH: nat, const FRAC: nat, const SIGNED: bool, intent I>
    FixedMul<WIDTH, FRAC, SIGNED, I>
{
    // Full-width multiply
    signal product_wide: bit[WIDTH * 2] = a * b

    // Shift to align fraction point: product has FRAC*2 bits after point
    // We want FRAC bits after point, so shift right by FRAC
    signal shifted = product_wide >> FRAC

    // Round if needed
    signal rounded = if I.rounding == RoundingMode::RoundNearest {
        // Check bit FRAC-1 (the bit being shifted out)
        if product_wide[FRAC - 1] {
            shifted + 1
        } else {
            shifted
        }
    } else {
        shifted
    }

    // Extract result
    result = rounded[0..WIDTH]
}

// Division - fraction points subtract, need scaling
entity FixedDiv<
    const WIDTH: nat,
    const FRAC: nat,
    const SIGNED: bool,
    intent I: Intent = Intent::default()
> {
    in a: fixed<WIDTH, FRAC, SIGNED>
    in b: fixed<WIDTH, FRAC, SIGNED>
    out result: fixed<WIDTH, FRAC, SIGNED>
    out div_by_zero: bit
}

impl<const WIDTH: nat, const FRAC: nat, const SIGNED: bool, intent I>
    FixedDiv<WIDTH, FRAC, SIGNED, I>
{
    div_by_zero = (b == 0)

    // Scale numerator up by FRAC bits before division
    signal a_scaled: bit[WIDTH + FRAC] = {a, 0b[FRAC]}

    // Divide
    signal quotient = a_scaled / b

    // Extract result (already has FRAC fractional bits)
    result = quotient[0..WIDTH]
}
```

### 2.3 Fixed-Point to Floating-Point Conversion

```skalp
entity FixedToFloat<
    const WIDTH: nat,
    const FRAC: nat,
    const SIGNED: bool,
    const F: FloatFormat,
    intent I: Intent = Intent::default()
> {
    in x: fixed<WIDTH, FRAC, SIGNED>
    out result: fp<F>
}

impl<const WIDTH: nat, const FRAC: nat, const SIGNED: bool, const F: FloatFormat, intent I>
    FixedToFloat<WIDTH, FRAC, SIGNED, F, I>
{
    // Extract sign
    signal sign: bit = if SIGNED { x[WIDTH - 1] } else { 0 }

    // Get absolute value
    signal abs_val: bit[WIDTH] = if sign { -x } else { x }

    // Find leading one (for normalization)
    signal leading_one_pos = count_leading_zeros(abs_val)

    // Compute exponent (position of leading 1 relative to fraction point)
    signal raw_exp = (WIDTH - FRAC - 1) - leading_one_pos
    signal biased_exp = raw_exp + F.bias

    // Extract mantissa (normalize and shift)
    signal normalized = abs_val << leading_one_pos
    signal mantissa = normalized[WIDTH - F.mantissa_bits - 1 .. WIDTH - 1]

    // Handle special cases
    signal is_zero = (abs_val == 0)

    result = if is_zero {
        fp<F>::ZERO
    } else if biased_exp <= 0 {
        // Underflow to denormal or zero
        fp<F>::ZERO  // Simplified
    } else if biased_exp >= (1 << F.exponent_bits) - 1 {
        // Overflow to infinity
        if sign { fp<F>::NEG_INF } else { fp<F>::POS_INF }
    } else {
        // Normal case
        {sign, biased_exp[0..F.exponent_bits], mantissa}
    }
}
```

---

## 3. Integer Type System

### 3.1 Integer as Degenerate Fixed-Point

```skalp
// Integer is fixed-point with FRAC=0
type int<const WIDTH: nat, const SIGNED: bool = true> =
    fixed<WIDTH, 0, SIGNED>

// All fixed-point operations work automatically!
signal a: i32 = 42
signal b: i32 = 17
signal sum: i32 = a + b      // Uses FixedAdd<32, 0, true>
signal prod: i32 = a * b     // Uses FixedMul<32, 0, true> (no shift needed!)
```

### 3.2 Integer-Specific Operations

```skalp
// Bitwise operations
entity BitwiseAnd<const WIDTH: nat> {
    in a: bit[WIDTH]
    in b: bit[WIDTH]
    out result: bit[WIDTH]
}

impl<const WIDTH: nat> BitwiseAnd<WIDTH> {
    result = a & b
}

// Shift operations
entity ShiftLeft<const WIDTH: nat, intent I: Intent = Intent::default()> {
    in x: bit[WIDTH]
    in shift: nat[clog2(WIDTH)]
    out result: bit[WIDTH]
}

impl<const WIDTH: nat, intent I> ShiftLeft<WIDTH, I> {
    result = if I.optimize == Optimize::Area {
        // Barrel shifter - area-efficient
        barrel_shift_left(x, shift)
    } else {
        // Fully parallel - low latency
        parallel_shift_left(x, shift)
    }
}

// Modulo/remainder
entity Modulo<const WIDTH: nat, const SIGNED: bool> {
    in a: int<WIDTH, SIGNED>
    in b: int<WIDTH, SIGNED>
    out result: int<WIDTH, SIGNED>
}

impl<const WIDTH: nat, const SIGNED: bool> Modulo<WIDTH, SIGNED> {
    signal quotient = a / b
    result = a - (quotient * b)
}
```

---

## 4. Unified Operations Across All Types

### 4.1 Generic Sqrt

```skalp
// Works for fp, fixed, and int!
entity Sqrt<T, intent I: Intent = Intent::default()>
where
    T: Numeric
{
    in x: T
    out result: T
}

impl<T, intent I> Sqrt<T, I>
where
    T: Numeric
{
    result = if T is fp<F> {
        // Floating-point sqrt
        fp_sqrt::<F, I>(x)

    } else if T is fixed<WIDTH, FRAC, SIGNED> {
        // Fixed-point sqrt using Newton-Raphson
        fixed_sqrt::<WIDTH, FRAC, SIGNED, I>(x)

    } else if T is int<WIDTH, SIGNED> {
        // Integer sqrt using binary search or Newton-Raphson
        int_sqrt::<WIDTH, SIGNED, I>(x)
    }
}

// Fixed-point sqrt implementation
const fn fixed_sqrt<
    const WIDTH: nat,
    const FRAC: nat,
    const SIGNED: bool,
    intent I: Intent
>(x: fixed<WIDTH, FRAC, SIGNED>) -> fixed<WIDTH, FRAC, SIGNED> {
    // Newton-Raphson: x_new = 0.5 * (x_old + n / x_old)

    signal guess: fixed<WIDTH, FRAC, SIGNED> = if I.latency < 5 {
        // Fast initial guess using bit manipulation
        fast_sqrt_guess(x)
    } else {
        // Better initial guess
        x >> 1  // Start with x/2
    }

    // Iterate
    const ITERATIONS: nat = if I.accuracy == Accuracy::High {
        4
    } else if I.latency < 8 {
        1
    } else {
        2
    }

    signal result = guess
    @unroll
    for i in 0..ITERATIONS {
        signal div = x / result
        signal sum = result + div
        result = sum >> 1  // Divide by 2
    }

    result
}

// Integer sqrt (always rounds down)
const fn int_sqrt<
    const WIDTH: nat,
    const SIGNED: bool,
    intent I: Intent
>(x: int<WIDTH, SIGNED>) -> int<WIDTH, SIGNED> {
    if SIGNED && x < 0 {
        // Error: sqrt of negative
        0  // Or raise error flag
    } else {
        if I.optimize == Optimize::Latency && WIDTH <= 8 {
            // Small integers: use LUT
            lut_sqrt(x)
        } else {
            // Binary search or Newton-Raphson
            newton_raphson_int(x)
        }
    }
}

// Usage - same sqrt function!
signal f: fp32 = sqrt(fp_val)         // FP sqrt
signal q: q16_16 = sqrt(fixed_val)    // Fixed-point sqrt
signal i: u32 = sqrt(int_val)         // Integer sqrt
```

### 4.2 Generic Normalize

```skalp
// Works for any numeric type!
entity VecNormalize<T, const N: nat, intent I: Intent = Intent::default()>
where
    T: Numeric
{
    in v: vec<T, N>
    out normalized: vec<T, N>
}

impl<T, const N: nat, intent I> VecNormalize<T, N, I>
where
    T: Numeric
{
    // Compute length squared
    signal len_sq: T = dot(v, v)

    // Compute length
    signal len: T = sqrt(len_sq)

    // Scale
    normalized = if T.IS_FLOATING {
        // Floating-point: divide by length
        v / len

    } else if T.IS_FIXED {
        // Fixed-point: need to scale properly
        // len is in same format as v, so division works
        v / len

    } else {
        // Integer: scale to unit vector in integer space
        // This is tricky - need to scale to some fixed range
        const MAX_INT: T = (1 << (T.TOTAL_BITS - 1)) - 1
        v * (MAX_INT / len)
    }
}

// Usage
signal v_fp: vec3<fp32> = {1.0, 2.0, 3.0}
signal n_fp: vec3<fp32> = normalize(v_fp)

signal v_fixed: vec3<q16_16> = {...}
signal n_fixed: vec3<q16_16> = normalize(v_fixed)

signal v_int: vec3<i32> = {...}
signal n_int: vec3<i32> = normalize(v_int)
```

### 4.3 Type-Aware Operations

```skalp
entity Multiply<T, intent I: Intent = Intent::default()>
where
    T: Numeric
{
    in a: T
    in b: T
    out result: T
}

impl<T, intent I> Multiply<T, I> {
    result = if T is fp<F> {
        // FP multiply: align exponents, multiply mantissas
        fp_multiply::<F, I>(a, b)

    } else if T is fixed<WIDTH, FRAC, SIGNED> {
        // Fixed-point: multiply and shift
        signal product = a * b
        product >> FRAC  // Align fraction point

    } else if T is int<WIDTH, SIGNED> {
        // Integer: direct multiplication
        a * b
    }

    // Intent-driven optimization
    result = if I.map_to_dsp && T.TOTAL_BITS <= 18 {
        // Map to DSP block (Xilinx DSP48)
        dsp_multiply(a, b)
    } else if I.optimize == Optimize::Area {
        // Sequential multiplier
        sequential_multiply(a, b)
    } else {
        // Standard multiply
        result
    }
}
```

---

## 5. Mixed-Type Operations

### 5.1 Automatic Type Promotion

```skalp
// Define promotion rules
trait Promote<T> {
    type Output;
    fn promote(self) -> Self::Output;
}

// i8 → i16 → i32 → i64
impl Promote<i16> for i8 {
    type Output = i16
    fn promote(self) -> i16 {
        sign_extend(self, 16)
    }
}

// Fixed-point promotion: increase width
impl Promote<fixed<32, 16, true>> for fixed<16, 8, true> {
    type Output = fixed<32, 16, true>
    fn promote(self) -> fixed<32, 16, true> {
        // Sign-extend integer part, shift fraction part
        {sign_extend(self[8..16], 16), self[0..8], 0b[8]}
    }
}

// Fixed → Float
impl Promote<fp32> for fixed<32, 16, true> {
    type Output = fp32
    fn promote(self) -> fp32 {
        fixed_to_float::<32, 16, true, IEEE754_32>(self)
    }
}

// Usage
@intent(promote: auto)
impl MixedTypes {
    signal a: i16 = 100
    signal b: i32 = 200
    signal sum: i32 = a + b  // a promoted to i32

    signal x: q16_16 = 3.14159
    signal y: fp32 = 2.71828
    signal product: fp32 = x * y  // x promoted to fp32
}
```

### 5.2 Explicit Conversions

```skalp
// Generic conversion
entity Convert<From, To, intent I: Intent = Intent::default()>
where
    From: Numeric,
    To: Numeric
{
    in x: From
    out result: To
}

impl<From, To, intent I> Convert<From, To, I> {
    result = if From is fp<F1> && To is fp<F2> {
        // FP → FP conversion
        fp_convert::<F1, F2, I>(x)

    } else if From is fixed<W1, F1, S1> && To is fixed<W2, F2, S2> {
        // Fixed → Fixed conversion
        fixed_convert::<W1, F1, S1, W2, F2, S2, I>(x)

    } else if From is int<W1, S1> && To is int<W2, S2> {
        // Int → Int conversion
        int_convert::<W1, S1, W2, S2, I>(x)

    } else if From is fixed<W, F, S> && To is fp<FP> {
        // Fixed → Float
        fixed_to_float::<W, F, S, FP, I>(x)

    } else if From is fp<FP> && To is fixed<W, F, S> {
        // Float → Fixed
        float_to_fixed::<FP, W, F, S, I>(x)

    } else if From is int<W, S> && To is fp<FP> {
        // Int → Float
        int_to_float::<W, S, FP, I>(x)

    } else if From is fp<FP> && To is int<W, S> {
        // Float → Int
        float_to_int::<FP, W, S, I>(x)
    }
}

// Usage
signal i: i32 = 42
signal f: fp32 = convert(i)              // Int → Float
signal q: q16_16 = convert(f)            // Float → Fixed
signal j: i64 = convert(i)               // Int → Int (widen)
```

---

## 6. Complete Example: Digital Signal Processing

```skalp
// Generic FIR filter works with ANY numeric type!
entity FIRFilter<
    T,
    const TAPS: nat,
    intent I: Intent = Intent::default()
>
where
    T: Numeric
{
    in clk: clock
    in sample: T
    out result: T
}

impl<T, const TAPS: nat, intent I> FIRFilter<T, I> {
    // Coefficient storage
    signal coeffs: [T; TAPS]

    // Delay line
    signal delay_line: [T; TAPS]

    on(clk.rise) {
        // Shift delay line
        @unroll
        for i in (1..TAPS).rev() {
            delay_line[i] <= delay_line[i - 1]
        }
        delay_line[0] <= sample
    }

    // Compute output - choose strategy based on type and intent
    result = if I.optimize == Optimize::Latency {
        // Fully parallel MAC
        @unroll
        for i in 0..TAPS {
            acc += delay_line[i] * coeffs[i]
        }

    } else if I.optimize == Optimize::Area {
        // Sequential MAC
        @sequential
        for i in 0..TAPS {
            acc += delay_line[i] * coeffs[i]
        }

    } else {
        // Balanced - partial unroll
        const UNROLL: nat = min(4, TAPS)
        @unroll(factor: UNROLL)
        for i in 0..TAPS {
            acc += delay_line[i] * coeffs[i]
        }
    }

    // Type-specific optimizations
    result = if T is fixed<WIDTH, FRAC, SIGNED> {
        // Fixed-point: watch for accumulator overflow
        saturate(acc, T::MIN, T::MAX)

    } else if T is fp<F> {
        // Floating-point: no overflow, but watch for NaN
        if acc.is_nan() { T::ZERO } else { acc }

    } else {
        // Integer: modulo or saturate based on intent
        if I.overflow == OverflowMode::Saturate {
            saturate(acc, T::MIN, T::MAX)
        } else {
            acc  // Wrap
        }
    }
}

// Usage - same FIR filter for all numeric types!

// FP32 filter for high precision
@intent(profile: "high_precision")
let fir_fp32 = FIRFilter<fp32, 64> {
    clk = clk,
    sample = audio_in_fp32,
    result => audio_out_fp32
}

// Fixed-point filter for efficient implementation
@intent(profile: "low_power", overflow: saturate)
let fir_fixed = FIRFilter<q16_16, 64> {
    clk = clk,
    sample = audio_in_fixed,
    result => audio_out_fixed
}

// Integer filter for simple processing
@intent(profile: "minimal_area")
let fir_int = FIRFilter<i16, 32> {
    clk = clk,
    sample = audio_in_int,
    result => audio_out_int
}

// BFloat16 for ML accelerators
@intent(profile: "ml_optimized", map_to_dsp: true)
let fir_bf16 = FIRFilter<bf16, 128> {
    clk = clk,
    sample = ml_feature_in,
    result => ml_feature_out
}
```

---

## 7. Type System Summary

### 7.1 Complete Type Hierarchy

```
Numeric (trait)
├── fp<const F: FloatFormat>
│   ├── fp16 = fp<IEEE754_16>
│   ├── fp32 = fp<IEEE754_32>
│   ├── fp64 = fp<IEEE754_64>
│   ├── bf16 = fp<BFLOAT16>
│   ├── tf32 = fp<TFLOAT32>
│   └── ... (custom formats)
│
├── fixed<const WIDTH: nat, const FRAC: nat, const SIGNED: bool>
│   ├── q16_16 = fixed<32, 16, true>
│   ├── q8_8 = fixed<16, 8, true>
│   ├── uq16_16 = fixed<32, 16, false>
│   └── ... (custom formats)
│
└── int<const WIDTH: nat, const SIGNED: bool>
    ├── i8, i16, i32, i64
    ├── u8, u16, u32, u64
    └── int<W, S> for any W, S
```

### 7.2 Unified Operations

All operations work for all numeric types:

| Operation | FP | Fixed | Int |
|-----------|----|----|-----|
| `+`, `-` | ✓ | ✓ | ✓ |
| `*` | ✓ | ✓ (scaled) | ✓ |
| `/` | ✓ | ✓ (scaled) | ✓ |
| `<`, `>`, `==` | ✓ | ✓ | ✓ |
| `sqrt` | ✓ | ✓ (NR) | ✓ (NR/LUT) |
| `sin`, `cos` | ✓ | ✓ (CORDIC) | ✓ (LUT) |
| `abs` | ✓ | ✓ | ✓ |
| `min`, `max` | ✓ | ✓ | ✓ |
| `&`, `\|`, `^` | bit ops | bit ops | ✓ |

### 7.3 Type Conversion Matrix

| From ↓ To → | fp | fixed | int |
|-------------|-------|-------|-----|
| **fp** | Format convert | Quantize | Truncate |
| **fixed** | Normalize | Rescale | Truncate |
| **int** | Float convert | Scale | Resize |

---

## 8. Benefits Summary

### 8.1 Code Reduction

**Before (Separate Types):**
```
Operations: Add, Sub, Mul, Div, Sqrt, ...  (10 ops)
Types: fp16, fp32, fp64, q16_16, i32      (5 types)
Total entities: 10 × 5 = 50 entities
```

**After (Unified):**
```
Operations: Add, Sub, Mul, Div, Sqrt, ...  (10 ops)
Total entities: 10 generic entities
```

**Reduction: 50 → 10 entities (80% reduction)**

### 8.2 Flexibility

✅ **Any floating-point format** (IEEE 754, BFloat16, custom)
✅ **Any fixed-point format** (Q notation, custom width/fraction)
✅ **Any integer width** (8, 16, 32, 64, or arbitrary)
✅ **Mixed-type operations** with automatic promotion
✅ **Type-safe conversions** with compile-time checking
✅ **Intent-driven optimization** for all types

### 8.3 Comparison

| Feature | Traditional HDL | SKALP Unified |
|---------|----------------|---------------|
| FP Formats | 3-4 hardcoded | Unlimited |
| Fixed-Point | Manual | First-class |
| Integer Widths | Few standard | Any width |
| Mixed Types | Manual convert | Auto promote |
| Operations | Type-specific | Generic |
| Stdlib Size | ~500 entities | ~50 entities |
| Custom Types | ❌ Not supported | ✅ Full support |

---

## 9. Migration and Compatibility

### 9.1 Backward Compatibility

```skalp
// Old code continues to work
signal a: i32 = 42
signal b: fp32 = 3.14
signal c: vec3<fp32> = {1.0, 2.0, 3.0}

// All operations work as before
signal sum = a + a
signal product = b * b
signal normalized = normalize(c)
```

### 9.2 Gradual Adoption

```skalp
// Stage 1: Use concrete types
signal x: i32 = 100

// Stage 2: Use parametric types
signal y: int<32, true> = 100

// Stage 3: Use generic functions
fn process<T: Numeric>(x: T) -> T {
    sqrt(x * x + x)
}

// Stage 4: Full generic design
entity Pipeline<T, intent I>
where T: Numeric
{
    in data: stream<T>
    out result: stream<T>
}
```

---

## Summary

### Key Innovations

1. **Unified Type System** - fp, fixed, int all implement `Numeric`
2. **Type Introspection** - `T.IS_FLOATING`, `T.TOTAL_BITS`, etc.
3. **Generic Operations** - Single implementation for all types
4. **Automatic Promotion** - Mixed-type operations work seamlessly
5. **Intent Integration** - Optimization applies to all numeric types

### Impact

**SKALP now has the most advanced numeric type system of any HDL:**

✅ **Floating-point**: Unlimited formats (IEEE 754, BFloat16, custom)
✅ **Fixed-point**: First-class with automatic scaling
✅ **Integer**: Arbitrary width with full operation support
✅ **Generic**: Single implementation works for ALL types
✅ **Safe**: Compile-time type checking
✅ **Efficient**: Zero-cost abstractions

**With this system, SKALP is truly a complete HLS tool!** 🚀✨🎯
