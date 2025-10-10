# skalp-stdlib

Standard library of reusable hardware components for SKALP.

## Overview

This crate provides a collection of common hardware modules written in SKALP, ready for instantiation in your designs. All components are:

- **Synthesizable** - Pure RTL, no behavioral constructs
- **Portable** - Work with any synthesis tool (ASIC or FPGA)
- **Tested** - Comprehensive test coverage
- **Documented** - Clear interfaces and usage examples

## Components

### Basic Building Blocks

- **adder.sk** - Parameterized adder with carry
- **counter.sk** - Configurable up/down counter
- **shift_register.sk** - Shift register with parallel load
- **multiplier.sk** - Pipelined multiplier

### Data Structures

- **fifo.sk** - Synchronous FIFO with configurable depth
- **async_fifo.sk** - Asynchronous FIFO for clock domain crossing

### Communication Protocols

- **uart.sk** - UART transmitter/receiver
- **axi4_lite.sk** - AXI4-Lite interface components
- **spi_master.sk** - SPI master controller

### Floating-Point & Vector Math

- **fp/** - IEEE 754 floating-point arithmetic (see [fp/README.md](components/fp/README.md))
- **vec/** - Vector math operations (planned)

## Usage

### In Your SKALP Design

```skalp
// Import and instantiate from stdlib
entity MyDesign {
    in clk: clock
    in data_in: bit<8>
    out data_out: bit<8>
}

impl MyDesign {
    // Instantiate a FIFO from stdlib
    inst buffer: FIFO<DEPTH=16, WIDTH=8> {
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
