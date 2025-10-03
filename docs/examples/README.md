# SKALP Examples

This directory contains ready-to-run SKALP examples demonstrating various language features and design patterns.

## Basic Examples

### [counter.sk](counter.sk)
- **Purpose**: Simple 32-bit counter with enable and overflow detection
- **Features**: Basic sequential logic, combinational outputs
- **Concepts**: Entity/impl, clock events, reset handling

```bash
# Compile and simulate
skalp build counter.sk
skalp sim counter.sk --cycles 100
```

### [fifo.sk](fifo.sk)  
- **Purpose**: Configurable synchronous FIFO buffer
- **Features**: Generic parameters, arrays, complex control logic
- **Concepts**: Parameterization, memory inference, assertions

```bash
# Test with 8-bit data, 16-entry depth
skalp build fifo.sk --param WIDTH=8 --param DEPTH=16
skalp verify fifo.sk --formal
```

### [uart.sk](uart.sk)
- **Purpose**: UART transmitter with configurable baud rate
- **Features**: State machines, clock generation, bit manipulation
- **Concepts**: Enums, pattern matching, timing calculations

```bash
# 115200 baud on 100MHz clock
skalp build uart.sk --param CLK_FREQ=100000000 --param BAUD_RATE=115200
skalp sim uart.sk --testbench uart_test.sk
```

## Intermediate Examples

### [axi_slave.sk](axi_slave.sk)
- **Purpose**: AXI4-Lite slave interface
- **Features**: Protocol implementation, handshaking
- **Concepts**: Protocols, interfaces, bus timing

### [dsp_filter.sk](dsp_filter.sk)
- **Purpose**: Digital signal processing filter
- **Features**: Fixed-point arithmetic, pipelining
- **Concepts**: Mathematical operations, performance optimization

### [cache_controller.sk](cache_controller.sk)
- **Purpose**: Simple cache controller
- **Features**: Memory hierarchies, state machines
- **Concepts**: Complex control logic, performance considerations

## Advanced Examples

### [processor_core.sk](processor_core.sk)
- **Purpose**: Simple RISC processor core
- **Features**: Instruction decode, pipeline stages, forwarding
- **Concepts**: Large-scale design, modular architecture

### [ddr_controller.sk](ddr_controller.sk)
- **Purpose**: DDR memory controller
- **Features**: Timing-critical design, complex protocols
- **Concepts**: Advanced timing, protocol compliance

### [safety_system.sk](safety_system.sk)
- **Purpose**: ISO 26262 compliant safety system
- **Features**: FMEA integration, safety mechanisms
- **Concepts**: Safety analysis, redundancy, fail-safe design

## Running Examples

### Basic Simulation
```bash
# Compile to intermediate representation
skalp build example.sk

# Run behavioral simulation
skalp sim example.sk --cycles 1000

# Generate waveforms
skalp sim example.sk --vcd output.vcd
```

### Advanced Simulation
```bash
# GPU-accelerated simulation
skalp sim example.sk --gpu --cycles 1000000

# Performance analysis
skalp sim example.sk --profile --report timing.json

# Verification
skalp verify example.sk --assertions --coverage
```

### Synthesis Flows
```bash
# FPGA synthesis (iCE40)
skalp synthesize example.sk --target ice40 --output example.bin

# ASIC synthesis (SKY130)
skalp synthesize example.sk --target sky130 --output example.gds

# Timing analysis
skalp analyze example.sk --timing --report timing.rpt
```

## Example Structure

Each example includes:
- **Source code** (`.sk` files) - The main SKALP design
- **Testbench** (`*_test.sk`) - Verification and simulation setup
- **Documentation** - Design description and usage notes
- **Makefile** - Automated build and test scripts

## Getting Help

- Read the [Language Tutorial](../tutorial.md) for step-by-step learning
- Check [Language Specification](../language-spec.md) for syntax reference
- Visit [Verification Guide](../verification.md) for testing best practices
- See [Performance Guide](../performance.md) for optimization tips

## Contributing Examples

We welcome new examples! Please ensure your contributions:
1. Include complete, working code
2. Have clear documentation and comments
3. Follow SKALP coding style guidelines
4. Include testbenches and verification
5. Demonstrate specific language features or design patterns
