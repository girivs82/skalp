# SKALP Graphics Pipeline - Comprehensive Reference Implementation

**A professional-grade hardware graphics accelerator showcasing SKALP's advanced features**

## Overview

This project is a **complete reference implementation** demonstrating how to organize and build large-scale hardware projects with SKALP. It implements a 3D graphics pipeline with:

- Multiple clock domains (system, geometry, pixel)
- AXI4-Lite register interface  
- Geometry processing with matrix transforms
- Async FIFOs for clock domain crossing
- Video timing generation
- Complete verification infrastructure

## What This Project Demonstrates

### Language Features

- ✅ **Parametric Types**: `fp<F>`, `fixed<W,F,S>`, `int<W,S>`, `vec<T,N>`
- ✅ **Generic Entities**: Entities parameterized by type and const parameters
- ✅ **Trait System**: Generic algorithms using `T: Numeric` bounds
- ✅ **Intent-Driven Design**: Architecture selection based on optimization goals
- ✅ **Module System**: Multi-file projects with `mod` and `use`
- ✅ **Functions**: Utility functions with generic parameters
- ✅ **Multiple Clock Domains**: Explicit clock domain management
- ✅ **Verification**: Assertions, properties, covergroups
- ✅ **Physical Constraints**: Pin assignments, timing constraints

### Project Organization

- ✅ **Separation of Concerns**: Modular architecture with clear boundaries
- ✅ **Reusable Libraries**: Generic FIFOs, CDC utilities, numeric functions
- ✅ **Complete Documentation**: Architecture docs, API reference, tutorials
- ✅ **Verification Infrastructure**: Testbenches, golden files, properties
- ✅ **Build Automation**: Makefile with common targets
- ✅ **Multi-Platform Support**: FPGA (iCE40, Xilinx) and ASIC constraints

## Quick Start

### Build the Design

```bash
make all                # Build complete design
make build-geometry     # Build specific module
```

### Run Simulations

```bash
make sim                # Run all testbenches
make test-fifo          # Test FIFO module
```

### Synthesize

```bash
make synth-ice40        # For iCE40 FPGA
make synth-xilinx       # For Xilinx FPGA
```

## Project Structure

See [ARCHITECTURE.md](docs/ARCHITECTURE.md) for detailed documentation.

## License

MIT License
