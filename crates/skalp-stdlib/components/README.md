# SKALP Standard Library

This directory contains the SKALP standard library, providing hardware implementations of common operations for ASIC/FPGA synthesis.

## Overview

The standard library is organized into modules:

- `fp/` - IEEE 754 floating-point arithmetic
- `vec/` - Vector math operations
- `math/` - General math functions
- `util/` - Utility functions

## Design Philosophy

Unlike FPGA vendor IP or hard macros, these implementations are:

1. **Synthesizable RTL** - Written in pure SKALP, compilable to any target
2. **Parameterizable** - Adjust precision, latency, area tradeoffs
3. **ASIC-friendly** - No vendor-specific primitives
4. **HLS-style** - Called like software functions, synthesized to hardware modules
5. **Context-agnostic** - Same component works in both sync and async (NCL) contexts

### Context-Agnostic Pattern

Core arithmetic and datapath components use `barrier` annotations instead of
explicit clk/rst ports. The parent entity's context determines interpretation:

| Parent context | `barrier` becomes | Behavior |
|---|---|---|
| `async entity` (NCL) | Completion detection point | Self-timed pipeline, wave pipelining |
| Sync clocked entity | Pipeline register | One stage per clock cycle |
| Sync combinational | Ignored (no-op) | Single-cycle dataflow |

**Example**: `a * b` in user code lowers to `std_multiplier`, which decomposes
into a chain of `std_adder` stages separated by barriers. In an NCL context,
each adder stage completes independently via handshake. In a clocked context,
each stage becomes a pipeline register. The user writes the same code either way.

**Rules for new context-agnostic components:**
- No `clk`/`rst` ports — pure dataflow
- Use `barrier` between pipeline stages
- Arithmetic operators (`+`, `-`) map to `std_adder` → CARRY chains
- Never use `*` inside multiplier (breaks to shift-and-add with `+`)
- Use `generate for` for parameterized stage counts
- `ImplStyle` (`#[impl_style::tree]`) selects between variants at call site

## Floating-Point Library (`fp/`)

### Supported Operations

**Basic Arithmetic:**
- `fp32_add(a, b)` - Single-precision addition
- `fp32_sub(a, b)` - Single-precision subtraction
- `fp32_mul(a, b)` - Single-precision multiplication
- `fp32_div(a, b)` - Single-precision division

**Transcendental Functions:**
- `fp32_sqrt(x)` - Square root
- `fp32_rsqrt(x)` - Reciprocal square root (1/√x)
- `fp32_exp(x)` - Exponential (e^x)
- `fp32_log(x)` - Natural logarithm
- `fp32_pow(x, y)` - Power (x^y)

**Trigonometric:**
- `fp32_sin(x)`, `fp32_cos(x)`, `fp32_tan(x)`
- `fp32_asin(x)`, `fp32_acos(x)`, `fp32_atan(x)`
- `fp32_atan2(y, x)` - Two-argument arctangent

**Fused Operations:**
- `fp32_fma(a, b, c)` - Fused multiply-add (a*b + c)
- `fp32_fms(a, b, c)` - Fused multiply-subtract (a*b - c)

**Conversions:**
- `fp16_to_fp32(x)`, `fp32_to_fp16(x)`
- `fp32_to_fp64(x)`, `fp64_to_fp32(x)`
- `fp32_to_int(x)`, `int_to_fp32(x)`

### Implementation Notes

FP operations use IEEE 754 format:
- FP16: 1 sign + 5 exp + 10 mantissa
- FP32: 1 sign + 8 exp + 23 mantissa
- FP64: 1 sign + 11 exp + 52 mantissa

Implementations can be:
- **Combinational** - Single-cycle, larger area
- **Pipelined** - Multi-cycle, higher throughput
- **Iterative** - Multi-cycle, smaller area

## Vector Math Library (`vec/`)

### Supported Operations

**Vector Arithmetic:**
- `vec_add<N, T>(a, b)` - Element-wise addition
- `vec_sub<N, T>(a, b)` - Element-wise subtraction
- `vec_mul<N, T>(a, b)` - Element-wise multiplication (Hadamard product)
- `vec_scale<N, T>(v, s)` - Scalar multiplication

**Vector Operations:**
- `dot<N, T>(a, b)` - Dot product (scalar result)
- `cross(a, b)` - Cross product (vec3 only)
- `length<N, T>(v)` - Vector magnitude/length
- `normalize<N, T>(v)` - Unit vector
- `distance<N, T>(a, b)` - Euclidean distance

**Geometric:**
- `reflect<N, T>(v, n)` - Reflect vector across normal
- `refract<N, T>(v, n, eta)` - Refract vector
- `project<N, T>(a, b)` - Project a onto b

### Example Usage

```skalp
entity RayTracer {
    in ray_origin: vec3<fp32>
    in ray_dir: vec3<fp32>
    in sphere_center: vec3<fp32>
    in sphere_radius: fp32
    out hit: bit<1>
    out hit_point: vec3<fp32>
}

impl RayTracer {
    // Vector from ray origin to sphere center
    signal oc: vec3<fp32> = vec_sub(sphere_center, ray_origin)

    // Project onto ray direction
    signal t: fp32 = dot(oc, ray_dir)

    // Closest point on ray to sphere center
    signal closest: vec3<fp32> = vec_add(ray_origin, vec_scale(ray_dir, t))

    // Distance from sphere center to closest point
    signal dist: fp32 = length(vec_sub(sphere_center, closest))

    // Check if ray intersects sphere
    hit = dist < sphere_radius
    hit_point = closest
}
```

## Math Library (`math/`)

General math functions that work with multiple types:

- `abs<T>(x)` - Absolute value
- `min<T>(a, b)`, `max<T>(a, b)` - Min/max
- `clamp<T>(x, min, max)` - Clamp to range
- `lerp<T>(a, b, t)` - Linear interpolation
- `saturate<T>(x)` - Clamp to [0, 1]

## Implementation Status

| Module | Status | Notes |
|--------|--------|-------|
| **Types** | ✅ Complete | fp16/32/64, vec2/3/4<T> fully working |
| **Built-in Operators** | ✅ Complete | +, -, *, /, <, >, ==, etc. for FP and vectors |
| **Simulation** | ✅ Complete | CPU and GPU (Metal) simulation working |
| **Synthesis** | ✅ Complete | MIR/LIR lowering complete |
| fp/basic entities | 🚧 Defined | Add/sub/mul/compare entities exist, need validation |
| vec/basic entities | ✅ Working | Vec2/3/4 Add/Sub/Dot/Cross tested and working |
| vec/geometric | 🚧 Partial | Distance, lerp, min/max work; normalize needs sqrt |
| fp/transcendental | ⏳ Planned | Requires CORDIC or Taylor series |
| fp/conversions | ⏳ Planned | Format conversions (fp16↔32↔64) |
| math/util | ⏳ Planned | Generic utilities |

**Legend:**
- ✅ Complete and tested
- 🚧 Implemented but needs more testing/validation
- ⏳ Planned for future implementation

## Usage

### Option 1: Direct Entity Instantiation

```skalp
entity MyDesign {
    in a: fp32
    in b: fp32
    out sum: fp32
}

impl MyDesign {
    // Instantiate FP adder as submodule
    let adder = FP32Add {
        a = a,
        b = b,
        result => sum
    }
}
```

### Option 2: Function-Style (Future)

```skalp
impl MyDesign {
    // Call as function (compiler instantiates module)
    sum = fp32_add(a, b)
}
```

## Contributing

When adding new library functions:

1. Implement as SKALP entity in `components/`
2. Prefer context-agnostic pattern (no clk/rst, use barriers) for datapath components
3. Add tests in `tests/stdlib/`
4. Update this README with function signature and description
5. Ensure synthesizable (no behavioral constructs)
6. Document latency and area characteristics

## References

- IEEE 754-2019 Floating-Point Arithmetic Standard
- "Handbook of Floating-Point Arithmetic" by Muller et al.
- "Computer Arithmetic Algorithms" by Koren
- CORDIC algorithm for transcendental functions
