# SKALP TODO

## Recently Completed ✅ (2025-11-18)

### Language Features
- ✅ **Trait System** - Full trait definitions with implementations and bounds
- ✅ **Trait Methods** - `self` parameters with type annotations
- ✅ **Const Generics** - `fn foo<const W: nat>` for parameterized designs
- ✅ **Monomorphization** - Compile-time specialization of generic code
- ✅ **Match Let Bindings** - Block expressions with let statements in match arms

### Tooling
- ✅ **Linter** - Static analyzer with 10 lint types:
  - Unused code detection (variables, functions, dead code)
  - Type-based lints (width mismatches, sign confusion)
  - Hardware-specific lints (long combinational paths, inferred latches)
- ✅ **LSP Server** - Full IDE support with completions, diagnostics, hover
- ✅ **Formatter** - `skalp fmt` for consistent code style
- ✅ **Package Manager** - Dependency management (add/remove/update/search)

### Standard Library
- ✅ **Bitops** - 12 functions: clz, ctz, popcount, bitreverse, ffs, fls, parity, etc.
- ✅ **Math Operations** - Comprehensive math library
- ✅ **Vector Operations** - Vector arithmetic and operations
- ✅ **Fixed Point** - Fixed-point arithmetic support

## Deferred Tasks

### Physical Routing (Third-party toolchain integration)

These tests are ignored as they require deep integration with specific third-party FPGA tools that we likely won't use extensively:

1. **VTR Routing** (`tests/test_native_place_route.rs:1183`)
   - Test: `test_vtr_routing_flow`
   - Status: Ignored - "VTR routing implementation incomplete"
   - Notes: Requires VTR (Verilog-to-Routing) toolchain integration

2. **OpenFPGA Routing** (`tests/test_native_place_route.rs:1363`)
   - Test: `test_openfpga_routing_flow`
   - Status: Ignored - "OpenFPGA routing implementation incomplete"
   - Notes: Requires OpenFPGA toolchain integration

**Priority**: Low - These are specific to third-party FPGA backend tools. Will revisit if/when we need deep integration with these specific toolchains.
