# Floating-Point Arithmetic Library

This directory contains IEEE 754-compliant floating-point arithmetic implementations in SKALP.

## Status

🚧 **Work in Progress** - Infrastructure and type system complete, arithmetic units in development

### Completed
- ✅ FP16, FP32, FP64 type definitions
- ✅ Type system integration in compiler
- ✅ Bit-accurate representation (IEEE 754 format)
- ✅ Test infrastructure

### In Development
- 🚧 Basic arithmetic (add, sub, mul, div)
- 🚧 Comparison operations
- ⏳ Transcendental functions (sqrt, log, exp, etc.)
- ⏳ Format conversions

## IEEE 754 Format

```
FP16: [15]    [14:10]  [9:0]
      sign    exp(5)   mant(10)

FP32: [31]    [30:23]  [22:0]
      sign    exp(8)   mant(23)

FP64: [63]    [62:52]  [51:0]
      sign    exp(11)  mant(52)
```

## Implementation Approach

For ASIC synthesis, we implement FP operations as:

1. **Combinational units** - For simple ops (comparison, abs, negate)
2. **Pipelined units** - For complex ops (mul, div, sqrt)
3. **Iterative units** - For area-constrained designs

Unlike FPGA IP cores, these are:
- Portable across any synthesis tool
- Parameterizable (latency vs. area tradeoffs)
- Fully inspectable RTL

## Next Steps

The FP arithmetic units require careful implementation of:
1. Operand unpacking (sign, exp, mantissa extraction)
2. Special case handling (NaN, Inf, denormals, zero)
3. Exponent alignment
4. Mantissa arithmetic with guard bits
5. Normalization and rounding
6. Result packing

This is a significant undertaking comparable to implementing a complete FPU. The type system foundation is complete and ready for these implementations.
