# SKALP Examples

This directory contains example SKALP designs demonstrating various language features.

## Examples

### 1. Counter (`counter.skalp`)
A simple 8-bit counter with enable and reset signals. Demonstrates:
- Clock edge triggering (`on(clk.rise)`)
- Non-blocking assignments (`<=`)
- Reset handling
- Continuous assignments

### 2. Adder (`adder.skalp`)
A combinational 8-bit adder. Demonstrates:
- Pure combinational logic
- Continuous assignments (`=`)

### 3. FIFO (`fifo.skalp`)
A simple FIFO buffer. Demonstrates:
- Multiple processes
- Read and write pointers
- Full/empty flags

### 4. Top Module (`top_module.skalp`)
A hierarchical design showing module instantiation. Demonstrates:
- Module instances
- Port connections
- Hierarchical design

## Building Examples

To compile a SKALP file to SystemVerilog:

```bash
# TODO: Add compiler command once integrated
skalp compile counter.skalp -o counter.sv
```

## Simulating Examples

The generated SystemVerilog can be simulated with Verilator:

```bash
cd ../tests/sim
cp ../../examples/counter.sv .
./verilate.sh
```

## Language Features Demonstrated

- **Entity Declaration**: Hardware module interfaces
- **Implementation Blocks**: Module behavior
- **Event Blocks**: Clock and reset sensitivity
- **Signal Types**: `bit`, `logic`, `clock`, `reset`
- **Assignment Types**: Blocking, non-blocking, continuous
- **Control Flow**: if/else statements
- **Module Instantiation**: Hierarchical designs