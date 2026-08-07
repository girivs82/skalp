# SKALP Examples

This directory contains ready-to-run SKALP examples demonstrating various language features and design patterns.

## Examples

### [counter.sk](counter.sk)
- **Purpose**: Simple 32-bit counter with enable and overflow detection
- **Features**: Basic sequential logic, combinational outputs
- **Concepts**: Entity/impl, clock events, reset handling

```bash
# Compile to SystemVerilog
skalp build counter.sk -o build/counter

# Run behavioral simulation (writes counter.skw.gz waveform)
skalp sim counter.sk
```

### [fifo.sk](fifo.sk)
- **Purpose**: Configurable synchronous FIFO buffer
- **Features**: Generic parameters, arrays, complex control logic
- **Concepts**: Parameterization, memory inference, immediate assertions

```bash
skalp build fifo.sk -o build/fifo

# Formal equivalence check between RTL and synthesized netlist
skalp ec fifo.sk
```

### [uart.sk](uart.sk)
- **Purpose**: UART transmitter with configurable baud rate
- **Features**: State machines, clock generation, bit manipulation
- **Concepts**: Enums, pattern matching, timing calculations

```bash
skalp build uart.sk -o build/uart
skalp sim uart.sk
```

### [power_domains.sk](../../examples/power_domains.sk)
- **Purpose**: Power-domain modeling — supply tree, checked binding, UPF output
- **Features**: `power_domain` declarations (`external` / `regulated` / `switched`), `#[power_domain]` containment binding, dependent-failure (CCF) check on a `#[safety_mechanism]`, UPF emission
- **Concepts**: Supply-tree independence, common-cause failure, power intent as checked language model
- **Location**: repository `examples/` directory

```bash
# Emits build/design.sv and build/design.upf (power intent)
skalp build examples/power_domains.sk -o build
```

## Running Examples

### Build and Simulate
```bash
# Compile to SystemVerilog (default target)
skalp build example.sk -o build

# Run behavioral simulation
skalp sim example.sk

# Gate-level simulation
skalp sim example.sk --gate-level

# GPU-accelerated simulation
skalp sim example.sk --gpu
```

### Synthesis Flows
```bash
# FPGA synthesis (iCE40)
skalp synth example.sk --device ice40-hx8k -o build

# Full flow including place, route, and bitstream
skalp synth example.sk --device ice40-hx8k --full-flow -o build
```

### Verification
```bash
# Equivalence check RTL vs. synthesized gate-level netlist
skalp ec example.sk

# Fast simulation-only check
skalp ec example.sk --quick
```

## Starting a New Project

```bash
# Scaffold a project with skalp.toml and src/main.sk
skalp new my_project
cd my_project
skalp build src/main.sk
```

## Getting Help

- Read the [Quick Start](../user/quick-start.md) to get going
- Check the [Syntax Reference](../user/reference/syntax.md) for language details
- Browse the [Cookbook](../user/cookbook/README.md) for design patterns
- See the [CLI Reference](../user/reference/cli.md) for all commands

## Contributing Examples

We welcome new examples! Please ensure your contributions:
1. Include complete, working code (`skalp build` must succeed)
2. Have clear documentation and comments
3. Follow SKALP coding style guidelines
4. Demonstrate specific language features or design patterns
