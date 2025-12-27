# SKALP Standard Library - Quick Start Guide

This guide gets you started with the SKALP standard library in 5 minutes.

## What's Available Right Now

The stdlib provides **IEEE 754 floating-point types** and **generic vector types** with full hardware synthesis support:

- **Types:** `fp16`, `fp32`, `fp64`, `vec2<T>`, `vec3<T>`, `vec4<T>`
- **Operators:** `+`, `-`, `*`, `/`, `<`, `>`, `==`, etc.
- **Simulation:** CPU and GPU (Metal on macOS)
- **Synthesis:** Full hardware generation

## Basic Examples

### 1. Floating-Point Arithmetic

```skalp
entity SimpleAdder {
    in a: fp32
    in b: fp32
    out sum: fp32
}

impl SimpleAdder {
    sum = a + b  // Built-in FP addition
}
```

**What you get:**
- IEEE 754 compliant addition
- NaN, Inf, and denormal handling
- Hardware-synthesizable RTL

### 2. Vector Operations

```skalp
entity Vec3Adder {
    in v1: vec3<fp32>
    in v2: vec3<fp32>
    out result: vec3<fp32>
}

impl Vec3Adder {
    // Component-wise addition
    result = v1 + v2

    // Equivalent to:
    // result.x = v1.x + v2.x
    // result.y = v1.y + v2.y
    // result.z = v1.z + v2.z
}
```

### 3. Dot Product

```skalp
entity DotProduct {
    in a: vec3<fp32>
    in b: vec3<fp32>
    out dot: fp32
}

impl DotProduct {
    // Use stdlib entity
    let compute_dot = Vec3Dot<fp32> {
        a = a,
        b = b,
        result => dot
    }
}
```

### 4. Vector Construction

```skalp
entity VectorBuilder {
    in x: fp32
    in y: fp32
    in z: fp32
    out position: vec3<fp32>
}

impl VectorBuilder {
    // Create vector from components
    position = vec3::<fp32> {
        x: x,
        y: y,
        z: z
    }
}
```

### 5. Component Access

```skalp
entity VectorDecompose {
    in v: vec3<fp32>
    out x: fp32
    out y: fp32
    out z: fp32
}

impl VectorDecompose {
    // Access individual components
    x = v.x
    y = v.y
    z = v.z
}
```

### 6. Mixed Operations

```skalp
entity PhysicsStep {
    in position: vec3<fp32>
    in velocity: vec3<fp32>
    in dt: fp32
    out new_position: vec3<fp32>
}

impl PhysicsStep {
    // Scalar multiplication
    let scaled_vel = Vec3Scale<fp32> {
        v = velocity,
        s = dt
    }

    // Add to position
    new_position = position + scaled_vel.result
}
```

## Generic Programming

Vector operations work with **any element type**:

```skalp
entity GenericVectorAdd<T> where T: Synthesizable {
    in a: vec2<T>
    in b: vec2<T>
    out result: vec2<T>
}

impl<T> GenericVectorAdd<T> {
    result = a + b
}

// Use with different types:
// let fp_add = GenericVectorAdd<fp32> { ... }
// let int_add = GenericVectorAdd<bit<16>> { ... }
```

## Available Operations

### Floating-Point (fp16, fp32, fp64)

| Operation | Syntax | Example |
|-----------|--------|---------|
| Addition | `a + b` | `sum = x + y` |
| Subtraction | `a - b` | `diff = x - y` |
| Multiplication | `a * b` | `prod = x * y` |
| Division | `a / b` | `quot = x / y` |
| Negation | `-a` | `neg = -x` |
| Less than | `a < b` | `if x < y { ... }` |
| Greater than | `a > b` | `if x > y { ... }` |
| Equal | `a == b` | `if x == y { ... }` |

### Vectors (vec2, vec3, vec4)

| Operation | Entity | Description |
|-----------|--------|-------------|
| Addition | `Vec3Add<T>` | Component-wise `a + b` |
| Subtraction | `Vec3Sub<T>` | Component-wise `a - b` |
| Scalar multiply | `Vec3Scale<T>` | `v * scalar` |
| Dot product | `Vec3Dot<T>` | `a.x*b.x + a.y*b.y + a.z*b.z` |
| Cross product | `Vec3Cross<T>` | 3D cross product |
| Linear interp | `Vec3Lerp` | `a + (b-a)*t` |
| Min/Max | `Vec3Min`, `Vec3Max` | Component-wise min/max |

## Testing Your Code

### 1. Build Check

```bash
cargo build
```

### 2. Run Tests

```bash
# All stdlib tests
cargo test --test test_stdlib_synthesis
cargo test --test test_vec_arithmetic

# Specific test
cargo test --test test_cpu_fp_simulation test_fp32_addition_cpu
```

### 3. GPU Simulation (macOS only)

```bash
cargo test --test test_vec_arithmetic test_vec2_component_addition_gpu
```

## What's NOT Available Yet

⏳ **Coming later:**
- Square root, exp, log, trig functions
- Vector normalize, reflect, refract
- Format conversions (fp16↔fp32↔fp64)
- Integer↔float conversions
- Pipelined versions of operations

These require more complex implementations (CORDIC, Taylor series, etc.) and are planned for future releases.

## Common Patterns

### Pattern 1: Combining Operations

```skalp
entity VectorMagnitudeSq {
    in v: vec3<fp32>
    out mag_sq: fp32
}

impl VectorMagnitudeSq {
    // Magnitude squared = dot(v, v)
    let dot_self = Vec3Dot<fp32> {
        a = v,
        b = v,
        result => mag_sq
    }
}
```

### Pattern 2: Building Complex Operations

```skalp
entity DistanceSq {
    in a: vec3<fp32>
    in b: vec3<fp32>
    out dist_sq: fp32
}

impl DistanceSq {
    // Compute a - b
    let diff = Vec3Sub<fp32> {
        a = a,
        b = b
    }

    // Compute |a - b|²
    let mag = VectorMagnitudeSq {
        v = diff.result,
        mag_sq => dist_sq
    }
}
```

### Pattern 3: Conditionals with FP

```skalp
entity Clamp {
    in value: fp32
    in min_val: fp32
    in max_val: fp32
    out result: fp32
}

impl Clamp {
    result = if value < min_val {
        min_val
    } else if value > max_val {
        max_val
    } else {
        value
    }
}
```

## Next Steps

1. **Read the full reference:** [STDLIB_REFERENCE.md](STDLIB_REFERENCE.md)
2. **Learn advanced patterns:** [ADVANCED_PATTERNS.md](ADVANCED_PATTERNS.md)
3. **Check implementation details:** [components/README.md](components/README.md)
4. **Browse examples:** `examples/` directory and test files in `tests/`

## Getting Help

- **Main README:** [../README.md](../README.md)
- **Examples:** Look at test files in `tests/test_stdlib_*.rs`
- **Issues:** Report problems on the GitHub repository

## Summary

✅ **You can use right now:**
- All FP types with arithmetic operators
- All vector types with component-wise operations
- Dot products, cross products, scaling
- FP comparisons in conditionals
- Full simulation and synthesis

🚧 **Coming soon:**
- More complex math functions
- Normalization and geometric operations
- Format conversions

Happy hardware designing! 🚀
