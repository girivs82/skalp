# SKALP Parametric Types User Guide

**Version:** 1.0
**Last Updated:** 2025-10-11
**Status:** Production Ready

---

## Table of Contents

1. [Introduction](#introduction)
2. [Quick Start](#quick-start)
3. [Const Generic Parameters](#const-generic-parameters)
4. [Parametric Numeric Types](#parametric-numeric-types)
5. [Parametric Vector Types](#parametric-vector-types)
6. [Intent Parameters](#intent-parameters)
7. [The Numeric Trait](#the-numeric-trait)
8. [Monomorphization](#monomorphization)
9. [Best Practices](#best-practices)
10. [Examples](#examples)

---

## Introduction

SKALP's parametric type system enables compile-time specialization of hardware designs. This provides:

- **Reusable Components:** Write generic logic once, instantiate with different parameters
- **Type Safety:** Compile-time verification of bit widths and formats
- **Zero Overhead:** Specialized code generated at compile time
- **Hardware Optimization:** Different implementations based on parameters

### What Are Parametric Types?

Parametric types are types that take parameters. In SKALP, these parameters can be:

- **Const Parameters:** Compile-time constants (bit widths, sizes, etc.)
- **Type Parameters:** Generic types (T, U, etc.)
- **Intent Parameters:** HLS optimization hints

---

## Quick Start

### Basic Generic Entity

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
```

### Using the Generic Entity

```skalp
entity Top {
    in x: bit[8]
    in y: bit[8]
    out result: bit[8]
}

impl Top {
    signal s: bit[8]
    signal c: bit

    // Instantiate with WIDTH=8
    let adder8 = Adder<8> {
        a: x,
        b: y,
        sum: s,
        carry: c
    }

    result = s
}
```

When compiled, SKALP generates a specialized `Adder_8` entity with WIDTH=8.

---

## Const Generic Parameters

### Declaration Syntax

```skalp
entity MyEntity<
    const N: nat,           // Natural number (0, 1, 2, ...)
    const W: nat = 32,      // With default value
    const SIGNED: bool = true
> {
    // Entity ports using parameters
    in data: bit[W]
    out result: bit[N]
}
```

### Supported Const Types

- **`nat`:** Natural numbers (0, 1, 2, ...) - most common
- **`int`:** Integers (can be negative)
- **`bool`:** Boolean values (true/false)
- **`FloatFormat`:** Floating-point format descriptors
- **`Intent`:** HLS optimization intent structures

### Const Expressions

Parameters can be used in expressions:

```skalp
entity Buffer<const SIZE: nat> {
    in data: bit[8]
    out valid: bit
}

impl Buffer<const SIZE: nat> {
    // Const expressions
    signal buffer: bit[SIZE * 8]              // SIZE * 8
    signal addr: bit[clog2(SIZE)]             // Built-in function
    signal double_size: bit[SIZE + SIZE]      // Arithmetic
    signal comparison: bit = (SIZE > 16)      // Comparisons

    // Conditional on const parameter
    signal result: bit[8] = if SIZE > 256 {
        // Large buffer implementation
        buffer[addr * 8 +: 8]
    } else {
        // Small buffer implementation
        buffer[7:0]
    }
}
```

### Built-in Const Functions

- **`clog2(n)`:** Ceiling log base 2
- **`is_power_of_2(n)`:** Check if power of 2
- **`min(a, b)`:** Minimum of two values
- **`max(a, b)`:** Maximum of two values
- **`abs(n)`:** Absolute value

---

## Parametric Numeric Types

### Floating-Point: `fp<F>`

Generic floating-point type parameterized by format:

```skalp
// Import format definitions
use skalp::numeric::formats::*

entity FpAdd<const F: FloatFormat> {
    in a: fp<F>
    in b: fp<F>
    out result: fp<F>
}

impl FpAdd<const F: FloatFormat> {
    // Access format properties
    signal exp_width: nat = F.exponent_bits
    signal mant_width: nat = F.mantissa_bits
    signal total_width: nat = F.total_bits

    // ... FP addition logic ...

    result = /* implementation */
}

// Instantiate with IEEE 754 formats
entity Top {
    in x: fp32
    in y: fp32
    out z: fp32
}

impl Top {
    signal s: fp32

    let adder = FpAdd<IEEE754_32> {
        a: x,
        b: y,
        result: s
    }

    z = s
}
```

### Available Float Formats

```skalp
// IEEE 754 standard formats
fp16      // IEEE 754 half precision (16-bit)
fp32      // IEEE 754 single precision (32-bit)
fp64      // IEEE 754 double precision (64-bit)

// Machine learning formats
bf16      // Brain Float 16 (Google)
tf32      // TensorFloat 32 (NVIDIA)

// Custom formats
const FP24: FloatFormat = FloatFormat {
    total_bits: 24,
    exponent_bits: 8,
    mantissa_bits: 15,
    bias: 127,
    name: "FP24"
}
```

### Fixed-Point: `fixed<W, F, S>`

```skalp
// W = total width, F = fractional bits, S = signed
entity FixedMul<
    const W: nat,
    const F: nat,
    const S: bool = true
> {
    in a: fixed<W, F, S>
    in b: fixed<W, F, S>
    out result: fixed<W, F, S>
}

impl FixedMul<const W: nat, const F: nat, const S: bool> {
    // Multiply with proper scaling
    signal product: bit[W * 2] = a * b
    result = product[F + W - 1:F]  // Extract properly scaled result
}

// Common Q-notation aliases
entity DSPCore {
    signal audio: q16_16      // Q16.16 format
    signal coeff: q4_12       // Q4.12 format
    signal output: q16_16
}
```

### Integer: `int<W, S>`

```skalp
// W = width, S = signed
entity IntAdd<const W: nat, const S: bool = true> {
    in a: int<W, S>
    in b: int<W, S>
    out sum: int<W, S>
    out overflow: bit
}

// Standard integer type aliases
signal byte: i8       // 8-bit signed
signal word: i16      // 16-bit signed
signal dword: i32     // 32-bit signed
signal qword: i64     // 64-bit signed

signal ubyte: u8      // 8-bit unsigned
signal uword: u16     // 16-bit unsigned
signal udword: u32    // 32-bit unsigned
signal uqword: u64    // 64-bit unsigned
```

---

## Parametric Vector Types

### Vector: `vec<T, N>`

```skalp
// T = element type, N = dimension
entity VecAdd<T, const N: nat> {
    in a: vec<T, N>
    in b: vec<T, N>
    out result: vec<T, N>
}

impl VecAdd<T, const N: nat> {
    // Component-wise addition
    result = a + b
}

// Common dimension aliases
entity Graphics {
    signal position: vec3<fp32>    // 3D position
    signal color: vec4<fp32>       // RGBA color
    signal texcoord: vec2<fp32>    // 2D texture coordinate
}

// Access components
impl Graphics {
    signal x: fp32 = position.x()
    signal y: fp32 = position.y()
    signal z: fp32 = position.z()

    signal r: fp32 = color.x()  // or color.r()
    signal g: fp32 = color.y()  // or color.g()
    signal b: fp32 = color.z()  // or color.b()
    signal a: fp32 = color.w()  // or color.a()
}
```

### Vector Operations

```skalp
entity VectorOps {
    in a: vec3<fp32>
    in b: vec3<fp32>
    in scalar: fp32
}

impl VectorOps {
    signal sum: vec3<fp32> = a + b        // Addition
    signal diff: vec3<fp32> = a - b       // Subtraction
    signal scaled: vec3<fp32> = a * scalar // Scalar multiply
    signal dot: fp32 = a.dot(b)           // Dot product
    signal cross: vec3<fp32> = a.cross(b) // Cross product (3D only)
    signal len: fp32 = a.length()         // Length
    signal normalized: vec3<fp32> = a.normalize()  // Normalize
}
```

---

## Intent Parameters

Intent parameters enable architecture selection based on optimization goals:

### Basic Intent Usage

```skalp
entity FpMul<
    const F: FloatFormat,
    intent I: Intent
> {
    in a: fp<F>
    in b: fp<F>
    out result: fp<F>
}

impl FpMul<const F: FloatFormat, intent I: Intent> {
    // Select implementation based on intent
    result = if is_latency_optimized(I) {
        // Fully pipelined, parallel implementation
        fpmul_fast(a, b)
    } else if is_area_optimized(I) {
        // Sequential, resource-sharing implementation
        fpmul_small(a, b)
    } else {
        // Balanced implementation
        fpmul_balanced(a, b)
    }
}

// Instantiate with different intents
entity VideoProcessor {
    signal result_fast: fp32
    signal result_small: fp32
}

impl VideoProcessor {
    // Fast path (minimize latency)
    let mul_fast = FpMul<IEEE754_32, FAST_INTENT> {
        a: input1,
        b: input2,
        result: result_fast
    }

    // Compact path (minimize area)
    let mul_small = FpMul<IEEE754_32, SMALL_INTENT> {
        a: input3,
        b: input4,
        result: result_small
    }
}
```

### Predefined Intent Profiles

```skalp
// Minimize latency (fully pipelined)
FAST_INTENT

// Minimize area (resource sharing)
SMALL_INTENT

// Minimize power consumption
LOW_POWER_INTENT

// Maximize throughput (streaming)
HIGH_THROUGHPUT_INTENT

// Balanced (default)
DEFAULT_INTENT
```

### Custom Intent

```skalp
const MY_INTENT: Intent = Intent {
    latency: Some(5),              // Target 5 cycles
    optimize: Optimize::Latency,   // Primary goal
    optimize_secondary: Some(Optimize::Power),
    pipeline_mode: PipelineMode::Full,
    max_dsps: Some(10),            // Resource limits
    ...
}
```

---

## The Numeric Trait

The `Numeric` trait provides a unified interface for all numeric types:

### Using Numeric Trait

```skalp
entity GenericAccumulator<T: Numeric> {
    in clk: clock
    in data: T
    in enable: bit
    out sum: T
}

impl GenericAccumulator<T: Numeric> {
    signal accumulator: T = T::ZERO

    on(clk.rise) {
        if enable {
            let (result, overflow) = accumulator.add_with_overflow(data)

            if !overflow {
                accumulator <= result
            }
        }
    }

    sum = accumulator
}

// Works with any numeric type!
entity Top {
    signal fp_sum: fp32
    signal int_sum: i32
    signal fixed_sum: q16_16
}

impl Top {
    let fp_acc = GenericAccumulator<fp32> { ... }
    let int_acc = GenericAccumulator<i32> { ... }
    let fixed_acc = GenericAccumulator<q16_16> { ... }
}
```

### Numeric Trait Operations

```skalp
trait Numeric {
    // Constants
    const TOTAL_BITS: nat
    const IS_SIGNED: bool
    const IS_FLOATING: bool
    const MIN_VALUE: Self
    const MAX_VALUE: Self
    const ZERO: Self
    const ONE: Self

    // Arithmetic
    fn add(self, other: Self) -> Self
    fn sub(self, other: Self) -> Self
    fn mul(self, other: Self) -> Self
    fn div(self, other: Self) -> Self
    fn neg(self) -> Self
    fn abs(self) -> Self

    // Comparison
    fn eq(self, other: Self) -> bit
    fn lt(self, other: Self) -> bit
    fn le(self, other: Self) -> bit

    // Queries
    fn is_zero(self) -> bit
    fn is_positive(self) -> bit
    fn is_negative(self) -> bit
}
```

---

## Monomorphization

### What is Monomorphization?

Monomorphization is the process of generating specialized code for each unique set of generic parameters at compile time.

**Generic Code (written once):**
```skalp
entity Adder<const WIDTH: nat> {
    in a: bit[WIDTH]
    in b: bit[WIDTH]
    out sum: bit[WIDTH]
}
```

**Instantiations:**
```skalp
let adder8 = Adder<8> { ... }    // Instantiate with WIDTH=8
let adder16 = Adder<16> { ... }  // Instantiate with WIDTH=16
let adder32 = Adder<32> { ... }  // Instantiate with WIDTH=32
```

**Generated Code (automatically):**
```skalp
entity Adder_8 {     // Specialized for WIDTH=8
    in a: bit[8]
    in b: bit[8]
    out sum: bit[8]
}

entity Adder_16 {    // Specialized for WIDTH=16
    in a: bit[16]
    in b: bit[16]
    out sum: bit[16]
}

entity Adder_32 {    // Specialized for WIDTH=32
    in a: bit[32]
    in b: bit[32]
    out sum: bit[32]
}
```

### Benefits

1. **Zero Runtime Overhead:** All decisions made at compile time
2. **Optimized Code:** Each specialization can be independently optimized
3. **Type Safety:** Mismatched parameters caught at compile time
4. **Code Reuse:** Write generic logic once, use everywhere

### Deduplication

SKALP automatically deduplicates identical instantiations:

```skalp
// These use the same Adder_8 specialization
let adder_a = Adder<8> { ... }
let adder_b = Adder<8> { ... }  // Reuses Adder_8
```

---

## Best Practices

### 1. Use Descriptive Parameter Names

```skalp
// Good
entity Memory<const ADDR_WIDTH: nat, const DATA_WIDTH: nat>

// Less clear
entity Memory<const A: nat, const D: nat>
```

### 2. Provide Sensible Defaults

```skalp
entity Processor<
    const DATA_WIDTH: nat = 32,     // Common default
    const ADDR_WIDTH: nat = 32,
    const CACHE_SIZE: nat = 1024
>
```

### 3. Add Constraints

```skalp
entity FixedPoint<
    const WIDTH: nat,
    const FRAC: nat
>
where
    FRAC <= WIDTH,                  // Fractional bits can't exceed total
    WIDTH > 0,                      // Must have at least 1 bit
    WIDTH <= 128                    // Reasonable upper limit
```

### 4. Use Type Aliases for Common Instantiations

```skalp
// Define common configurations
type Adder8 = Adder<8>
type Adder16 = Adder<16>
type Adder32 = Adder<32>

// Easier to use
let add = Adder32 { ... }
```

### 5. Document Parameter Constraints

```skalp
/// N-way parallel adder
///
/// # Parameters
/// - WIDTH: Bit width of each operand (must be 1-64)
/// - N: Number of parallel additions (must be 1-16)
///
/// # Constraints
/// - WIDTH must be a power of 2 for optimal synthesis
/// - N * WIDTH should not exceed 1024 for area efficiency
entity ParallelAdder<const WIDTH: nat, const N: nat>
where
    WIDTH >= 1,
    WIDTH <= 64,
    N >= 1,
    N <= 16
```

### 6. Intent-Driven Specialization

```skalp
// Provide intent parameter for critical operations
entity FFT<
    const N: nat,
    intent I: Intent = DEFAULT_INTENT
> {
    // Implementation varies based on intent
}

// Application code chooses intent
let fft_fast = FFT<1024, FAST_INTENT> { ... }     // Low latency
let fft_small = FFT<1024, SMALL_INTENT> { ... }   // Low area
```

---

## Examples

### Example 1: Parameterized FIFO

```skalp
entity FIFO<
    const DEPTH: nat,
    const DATA_WIDTH: nat
>
where
    is_power_of_2(DEPTH)    // DEPTH must be power of 2
{
    in clk: clock
    in rst: reset
    in wr_en: bit
    in wr_data: bit[DATA_WIDTH]
    in rd_en: bit
    out rd_data: bit[DATA_WIDTH]
    out empty: bit
    out full: bit
}

impl FIFO<const DEPTH: nat, const DATA_WIDTH: nat> {
    const ADDR_WIDTH: nat = clog2(DEPTH)

    signal memory: bit[DATA_WIDTH][DEPTH]
    signal wr_ptr: bit[ADDR_WIDTH] = 0
    signal rd_ptr: bit[ADDR_WIDTH] = 0
    signal count: bit[ADDR_WIDTH+1] = 0

    on(clk.rise) {
        if rst {
            wr_ptr <= 0
            rd_ptr <= 0
            count <= 0
        } else {
            if wr_en && !full {
                memory[wr_ptr] <= wr_data
                wr_ptr <= wr_ptr + 1
                count <= count + 1
            }

            if rd_en && !empty {
                rd_ptr <= rd_ptr + 1
                count <= count - 1
            }
        }
    }

    rd_data = memory[rd_ptr]
    empty = (count == 0)
    full = (count == DEPTH)
}

// Usage
entity Top {
    signal fifo8_data: bit[8]
    signal fifo32_data: bit[32]
}

impl Top {
    let fifo_small = FIFO<16, 8> {     // 16-deep, 8-bit wide
        clk: clk,
        ...
    }

    let fifo_large = FIFO<256, 32> {   // 256-deep, 32-bit wide
        clk: clk,
        ...
    }
}
```

### Example 2: Generic FIR Filter

```skalp
entity FIRFilter<
    T: Numeric,
    const NUM_TAPS: nat
> {
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

// Different numeric types
entity AudioProcessor {
    signal audio_fp: fp32
    signal audio_fixed: q16_16
}

impl AudioProcessor {
    let fir_fp = FIRFilter<fp32, 16> { ... }
    let fir_fixed = FIRFilter<q16_16, 16> { ... }
}
```

### Example 3: Intent-Driven Matrix Multiply

```skalp
entity MatMul<
    const M: nat,
    const N: nat,
    const K: nat,
    intent I: Intent = DEFAULT_INTENT
> {
    in a: fp32[M][K]
    in b: fp32[K][N]
    out c: fp32[M][N]
}

impl MatMul<const M: nat, const N: nat, const K: nat, intent I: Intent> {
    c = if is_latency_optimized(I) {
        // Fully parallel: M*N*K multipliers
        matrix_mul_parallel(a, b)
    } else if is_area_optimized(I) {
        // Sequential: single multiplier, resource sharing
        matrix_mul_sequential(a, b)
    } else {
        // Pipelined: balanced latency and area
        matrix_mul_pipelined(a, b)
    }
}

// Application chooses architecture
entity ML_Accelerator {
    in features: fp32[128][256]
    in weights: fp32[256][10]
    out predictions: fp32[128][10]
}

impl ML_Accelerator {
    // Fast inference: fully parallel
    let matmul = MatMul<128, 10, 256, HIGH_THROUGHPUT_INTENT> {
        a: features,
        b: weights,
        c: predictions
    }
}
```

---

## Advanced Topics

### Nested Generic Types

```skalp
entity VecFIFO<
    T,
    const N: nat,      // Vector dimension
    const DEPTH: nat   // FIFO depth
> {
    in wr_data: vec<T, N>
    out rd_data: vec<T, N>
}

// Instantiate with vec<fp32, 3>
let fifo = VecFIFO<fp32, 3, 16> { ... }
```

### Multiple Trait Bounds

```skalp
entity Interpolator<T>
where
    T: Numeric,
    T: Comparable,
    T: Convertible<fp32>
{
    in samples: T[4]
    in t: T
    out result: T
}
```

### Const Functions

```skalp
const fn optimal_pipeline_depth(width: nat, target_fmax: nat) -> nat {
    if width > 64 {
        clog2(width)
    } else {
        width / 16
    }
}

entity Pipeline<const WIDTH: nat> {
    const DEPTH: nat = optimal_pipeline_depth(WIDTH, 100)
}
```

---

## Conclusion

SKALP's parametric type system provides powerful compile-time specialization for hardware designs. Key takeaways:

1. **Generic components** reduce code duplication
2. **Const parameters** enable flexible bit widths and configurations
3. **Parametric numeric types** provide unified FP/fixed/int operations
4. **Intent parameters** enable architecture selection
5. **Monomorphization** generates optimized specialized code
6. **The Numeric trait** enables truly generic algorithms

For more information, see:
- `LANGUAGE_SPECIFICATION.md` - Complete language reference
- `MONOMORPHIZATION_COMPLETE.md` - Implementation details
- `examples/` - Working code examples
