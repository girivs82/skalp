# Example 1: Simple Synchronous FIFO

## Description

A basic First-In-First-Out (FIFO) buffer with 4 entries of 8 bits each. This is a foundational building block used in almost every digital design for data buffering and clock domain crossing.

## Complexity Level
⭐ **Basic** - Introductory hardware design

## Features Used

### Language Features
- ✅ `entity` declaration with ports
- ✅ `impl` block with signals
- ✅ `signal` declarations with initialization
- ✅ `on(clk.rise)` sequential logic blocks
- ✅ `reset` handling
- ✅ `if/else` conditionals
- ✅ Arithmetic operations (`+`, `-`, `%`)
- ✅ Comparison operators (`==`, `<`, `>`)
- ✅ Combinational assignments (`=`)
- ✅ Sequential assignments (`<=`)
- ✅ `bit` and `nat[N]` types

### Design Patterns
- **Circular buffer** with wraparound pointers
- **Full/empty detection** via count tracking
- **Read/write control** with enable signals
- **Multiplexed read** data path

## Real-World Use Cases

1. **Asynchronous FIFO** - Can be extended for CDC
2. **Data buffering** - Between slow producer and fast consumer
3. **Protocol adaptation** - Width or rate adaptation
4. **Flow control** - Backpressure handling

## Compilation

```bash
skalp build -s simple_fifo.sk -o build/
```

## Generated Output

- ✅ Compiles successfully
- ✅ Generates SystemVerilog module
- SystemVerilog: `/tmp/simple_fifo_test/design.sv`

## Known Issues

1. **Comparison logic**: Generated SV has incorrect comparison logic for `if (wr_ptr == 0)` - generates `if (wr_ptr)` instead
2. **Count update**: Count update logic doesn't check full/empty conditions properly
3. **No overflow protection**: Writing to full FIFO still increments pointer

These are **compiler bugs**, not language design issues.

## Next Steps

- Add testbench
- Simulate with example data
- Fix compiler codegen bugs
- Add parameterized version with generics
