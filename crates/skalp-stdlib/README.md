# skalp-stdlib

Standard library of reusable hardware components for SKALP.

> **🚀 New to stdlib?** Start with the [Quick Start Guide](QUICK_START.md)
>
> **📖 Complete Reference:** See [STDLIB_REFERENCE.md](STDLIB_REFERENCE.md) for comprehensive type documentation, operation reference, and usage examples.

## Overview

This crate provides a collection of common hardware modules written in SKALP, ready for instantiation in your designs. All components are:

- **Synthesizable** - Pure RTL, no behavioral constructs
- **Portable** - Work with any synthesis tool (ASIC or FPGA)
- **Tested** - Comprehensive test coverage
- **Documented** - Clear interfaces and usage examples

## Quick Links

- **[Complete Reference](STDLIB_REFERENCE.md)** - Full type system and operations documentation
- **[Advanced Patterns](ADVANCED_PATTERNS.md)** - Generic programming, traits, and design patterns
- **[FP Types](components/fp/README.md)** - Floating-point implementation details
- **[Vector Operations](components/README.md)** - Vector math library overview

## Status: Rapidly Expanding

**✅ Complete:**
- Type system (fp16/32/64, vec2/3/4<T>)
- Built-in operators for FP and vector arithmetic
- CPU and GPU simulation
- Synthesis to hardware (MIR/LIR)

**✨ NEW - Just Added (48 operations):**
- FP utilities (min, max, abs, clamp, lerp, saturate, FMA/FMS)
- FP division (IEEE 754 compliant)
- FP square root & reciprocal square root
- Vector normalization (accurate & fast variants)
- Vector reflection & refraction (Snell's law)
- Vector projection & rejection
- Vector distance operations
- Phong/Blinn-Phong shading examples

> **📄 See [NEW_OPERATIONS.md](NEW_OPERATIONS.md)** for complete details on all new operations!

**🚧 In Progress:**
- Module system / import mechanism for stdlib entities
- Comprehensive simulation tests

> **📋 [STDLIB_USAGE_DESIGN.md](STDLIB_USAGE_DESIGN.md)** - Design document for how users will import/use stdlib

**⏳ Planned:**
- Transcendental functions (exp, log, sin, cos, tan)
- Format conversions (fp16↔fp32↔fp64, int↔float)
- Matrix operations (mat2x2, mat3x3, mat4x4)

## Components

### Floating-Point & Vector Math ⭐

**Core Types:**
- **fp16, fp32, fp64** - IEEE 754 floating-point types
- **vec2/3/4<T>** - Generic vector types (work with any element type)

**Operations:**
- Component-wise arithmetic (add, sub, mul using `+`, `-`, `*`)
- Vector operations (dot, cross, scale, lerp)
- FP comparisons (`<`, `>`, `==`, etc.)

See [STDLIB_REFERENCE.md](STDLIB_REFERENCE.md) for complete details.

### Basic Building Blocks

- **adder.sk** - Parameterized adder with carry
- **counter.sk** - Configurable up/down counter
- **shift_register.sk** - Shift register with parallel load
- **multiplier.sk** - Pipelined multiplier

### Data Structures

- **fifo.sk** - Synchronous FIFO with configurable depth

### Communication Protocols

- **uart.sk** - UART transmitter/receiver (planned)
- **axi4_lite.sk** - AXI4-Lite interface components (planned)

## Usage

### Vector Arithmetic (Currently Working)

```skalp
entity VectorExample {
    in a: vec3<fp32>
    in b: vec3<fp32>
    out sum: vec3<fp32>
    out dot_product: fp32
}

impl VectorExample {
    // Component-wise addition using built-in operator
    sum = a + b

    // Dot product using entity
    let dot = Vec3Dot<fp32> {
        a = a,
        b = b,
        result => dot_product
    }
}
```

### Floating-Point Operations (Currently Working)

```skalp
entity FloatExample {
    in x: fp32
    in y: fp32
    out sum: fp32
    out product: fp32
    out is_greater: bit
}

impl FloatExample {
    // Built-in FP operators
    sum = x + y
    product = x * y
    is_greater = x > y
}
```

### FIFO Example

```skalp
entity MyDesign {
    in clk: clock
    in data_in: bit<8>
    out data_out: bit<8>
}

impl MyDesign {
    // Instantiate a FIFO from stdlib
    let buffer = FIFO<DEPTH=16, WIDTH=8> {
        clk = clk,
        write_enable = 1,
        write_data = data_in,
        read_enable = 1,
        read_data => data_out
    }
}
```

### Adding New Components

1. Create new `.sk` file in `components/` directory
2. Follow existing naming and documentation style
3. Add tests in `tests/` directory
4. Update this README with component description

## Directory Structure

```
skalp-stdlib/
├── Cargo.toml          # Crate metadata
├── README.md           # This file
├── components/         # SKALP source files
│   ├── adder.sk
│   ├── fifo.sk
│   ├── fp/            # Floating-point library
│   └── ...
├── src/               # Rust code for component metadata
│   └── lib.rs
└── tests/             # Component tests
```

## Features

The crate supports optional features for different use cases:

- `simulation` - Enable simulation-specific features
- `synthesis` - Enable synthesis optimizations

## Version

Current version: 0.1.0

This is an early-stage library. APIs may change as SKALP evolves.

## Contributing

When contributing new components:

1. Ensure code is synthesizable (no `$display`, etc.)
2. Use meaningful parameter names
3. Include inline documentation
4. Add test cases
5. Follow SKALP style guide (when available)

## License

Same license as the main SKALP project.
