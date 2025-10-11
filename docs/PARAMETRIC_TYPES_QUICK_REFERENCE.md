# Parametric Types Quick Reference

**Quick lookup for common parametric type patterns in SKALP**

---

## 🚀 Quick Start

### Basic Generic Entity

```skalp
entity MyEntity<const WIDTH: nat> {
    in data: bit[WIDTH]
    out result: bit[WIDTH]
}

impl MyEntity<const WIDTH: nat> {
    result = data
}

// Use it
let entity8 = MyEntity<8> { ... }
let entity16 = MyEntity<16> { ... }
```

---

## 📦 Type System Cheat Sheet

### Floating-Point

```skalp
// Predefined formats
fp16, fp32, fp64          // IEEE 754
bf16, tf32                // ML formats

// Generic
entity FpOp<const F: FloatFormat> {
    in x: fp<F>
    out y: fp<F>
}
```

### Fixed-Point

```skalp
// Common aliases
q16_16, q8_8, q4_12       // Q-notation

// Generic
entity FixedOp<const W: nat, const F: nat, const S: bool> {
    in x: fixed<W, F, S>
    out y: fixed<W, F, S>
}
```

### Integer

```skalp
// Signed
i8, i16, i32, i64

// Unsigned
u8, u16, u32, u64

// Generic
entity IntOp<const W: nat, const S: bool> {
    in x: int<W, S>
    out y: int<W, S>
}
```

### Vector

```skalp
// Dimension aliases
vec2<T>, vec3<T>, vec4<T>

// Generic
entity VecOp<T, const N: nat> {
    in x: vec<T, N>
    out y: vec<T, N>
}

// Component access
signal pos: vec3<fp32>
signal x: fp32 = pos.x()  // or pos[0]
signal y: fp32 = pos.y()  // or pos[1]
signal z: fp32 = pos.z()  // or pos[2]
```

---

## 🎯 Common Patterns

### Pattern 1: Generic Width

```skalp
entity Adder<const WIDTH: nat> {
    in a: bit[WIDTH]
    in b: bit[WIDTH]
    out sum: bit[WIDTH]
}
```

### Pattern 2: Generic Type

```skalp
entity Processor<T> {
    in data: T
    out result: T
}
```

### Pattern 3: Numeric Trait Bound

```skalp
entity Accumulator<T: Numeric> {
    in data: T
    out sum: T
}

impl Accumulator<T: Numeric> {
    signal acc: T = T::ZERO
    // Use: acc.add(data), acc.mul(x), etc.
}
```

### Pattern 4: Intent-Driven

```skalp
entity Processor<intent I: Intent = DEFAULT_INTENT> {
    in data: fp32
    out result: fp32
}

impl Processor<intent I: Intent> {
    result = if is_latency_optimized(I) {
        fast_impl(data)
    } else {
        small_impl(data)
    }
}
```

### Pattern 5: Multiple Parameters

```skalp
entity Memory<
    const ADDR_WIDTH: nat,
    const DATA_WIDTH: nat,
    const DEPTH: nat = 1024
> {
    in addr: bit[ADDR_WIDTH]
    in data: bit[DATA_WIDTH]
}
```

### Pattern 6: Constraints

```skalp
entity FIFO<const DEPTH: nat>
where
    DEPTH > 0,
    is_power_of_2(DEPTH)
{
    // ...
}
```

---

## 🔧 Const Expressions

### Operators

```skalp
const SIZE: nat = 16

signal double: bit[SIZE * 2]        // Arithmetic
signal addr: bit[clog2(SIZE)]       // Functions
signal large: bool = (SIZE > 32)    // Comparisons
```

### Built-in Functions

```skalp
clog2(n)           // Ceiling log2
is_power_of_2(n)   // Power of 2 check
min(a, b)          // Minimum
max(a, b)          // Maximum
abs(n)             // Absolute value
```

### Field Access

```skalp
entity FpOp<const F: FloatFormat> {
    const TOTAL: nat = F.total_bits
    const EXP: nat = F.exponent_bits
    const MANT: nat = F.mantissa_bits
}
```

---

## 📚 Stdlib Operations

### FP Operations

```skalp
FpAdd<F>         // Addition
FpMul<F>         // Multiplication
FpDiv<F>         // Division
FpSqrt<F, I>     // Square root
FpCompare<F>     // Comparison

// Aliases
FP32Add = FpAdd<IEEE754_32>
FP16Mul = FpMul<IEEE754_16>
```

### Vector Operations

```skalp
VecAdd<T, N>         // Addition
VecSub<T, N>         // Subtraction
VecScale<T, N>       // Scalar multiply
VecDot<T, N>         // Dot product
Vec3Cross<T>         // Cross product (3D)
VecLength<T, N>      // Length
VecNormalize<T, N>   // Normalize
VecLerp<T, N>        // Linear interp

// Aliases
Vec3Fp32Add = VecAdd<fp32, 3>
Vec3Fp32Dot = VecDot<fp32, 3>
```

---

## 🎓 Examples

### Generic Adder

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

### Generic Accumulator

```skalp
entity Accumulator<T: Numeric> {
    in clk: clock
    in data: T
    in enable: bit
    out sum: T
}

impl Accumulator<T: Numeric> {
    signal acc: T = T::ZERO

    on(clk.rise) {
        if enable {
            acc <= acc.add(data)
        }
    }

    sum = acc
}
```

### Graphics Vector Math

```skalp
entity VectorOps {
    in a: vec3<fp32>
    in b: vec3<fp32>
    out dot: fp32
    out cross: vec3<fp32>
}

impl VectorOps {
    let dot_op = Vec3Fp32Dot {
        a: a,
        b: b,
        result: dot
    }

    let cross_op = Vec3Fp32Cross {
        a: a,
        b: b,
        result: cross
    }
}
```

### Intent-Driven FFT

```skalp
entity FFT<const N: nat, intent I: Intent> {
    in data: vec<fp32, N>
    out result: vec<fp32, N>
}

impl FFT<const N: nat, intent I: Intent> {
    result = if is_latency_optimized(I) {
        fft_parallel(data)
    } else if is_area_optimized(I) {
        fft_sequential(data)
    } else {
        fft_pipelined(data)
    }
}

// Use
let fast_fft = FFT<1024, FAST_INTENT> { ... }
let small_fft = FFT<1024, SMALL_INTENT> { ... }
```

---

## 💡 Tips & Tricks

### Tip 1: Use Type Aliases

```skalp
// Define common configurations
type Adder8 = Adder<8>
type Adder16 = Adder<16>
type Adder32 = Adder<32>

// Use them
let add = Adder32 { ... }
```

### Tip 2: Default Parameters

```skalp
entity Entity<
    const WIDTH: nat = 32,
    const DEPTH: nat = 1024
> {
    // Can omit parameters
}

let e1 = Entity { ... }           // Uses defaults
let e2 = Entity<16> { ... }       // WIDTH=16, DEPTH=1024
let e3 = Entity<16, 512> { ... }  // Both specified
```

### Tip 3: Const Expressions

```skalp
entity Buffer<const SIZE: nat> {
    // Compute derived values
    const ADDR_BITS: nat = clog2(SIZE)
    const BYTE_SIZE: nat = SIZE * 8

    signal addr: bit[ADDR_BITS]
    signal data: bit[BYTE_SIZE]
}
```

### Tip 4: Numeric Trait

```skalp
// Generic algorithm works with ANY numeric type
entity GenericOp<T: Numeric> {
    // Use T::ZERO, T::ONE, T::MIN_VALUE, T::MAX_VALUE
    // Call .add(), .sub(), .mul(), .div(), etc.
}
```

### Tip 5: Backward Compatibility

```skalp
// Old code still works
let adder = FP32Add { ... }

// New code is more flexible
let adder = FpAdd<IEEE754_32> { ... }

// Both compile to the same thing!
```

---

## ⚠️ Common Pitfalls

### Pitfall 1: Const Expressions

```skalp
// ❌ Wrong - division not supported directly
signal data: bit[SIZE/8]

// ✅ Right - precompute
const BYTE_SIZE: nat = SIZE / 8
signal data: bit[BYTE_SIZE]
```

### Pitfall 2: Type Inference

```skalp
// ❌ Wrong - can't infer T
let proc = Processor { ... }

// ✅ Right - specify type
let proc = Processor<fp32> { ... }
```

### Pitfall 3: Where Clauses

```skalp
entity FIFO<const DEPTH: nat>
where DEPTH <= 1024  // Constraint
{
    // ...
}

// ❌ Wrong - violates constraint
let fifo = FIFO<2048> { ... }

// ✅ Right - satisfies constraint
let fifo = FIFO<1024> { ... }
```

---

## 📖 See Also

- **Complete Guide:** `PARAMETRIC_TYPES_GUIDE.md`
- **Migration Guide:** `PARAMETRIC_TYPES_MIGRATION_GUIDE.md`
- **Language Spec:** `LANGUAGE_SPECIFICATION.md`
- **Examples:** `crates/skalp-stdlib/examples/`

---

**Version:** 1.0
**Last Updated:** 2025-10-11
**Status:** Production Ready ✅
