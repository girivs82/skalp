# SKALP Phase 2 User Guide

## Overview

Phase 2 of SKALP introduces core language features for hardware design including:
- Entity definitions and implementations
- Sequential and combinational logic
- Module hierarchy
- SystemVerilog code generation

## Supported Features

### Entity Declaration

Define hardware module interfaces:

```skalp
entity ModuleName {
    in port_name: type
    out port_name: type
    inout port_name: type
}
```

### Implementation Blocks

Define module behavior:

```skalp
impl ModuleName {
    // Signal declarations
    signal sig_name: type = initial_value

    // Variable declarations
    var var_name: type

    // Event blocks
    on(trigger) {
        // Sequential logic
    }

    // Continuous assignments
    output_port = expression
}
```

### Data Types

| Type | Description | Example |
|------|-------------|---------|
| `bit[N]` | N-bit vector (2-state) | `bit[8]` |
| `logic[N]` | N-bit vector (4-state) | `logic[16]` |
| `int[N]` | Signed integer | `int[32]` |
| `nat[N]` | Unsigned integer | `nat[8]` |
| `clock` | Clock signal | `clk: clock` |
| `reset` | Reset signal | `rst: reset` |

### Event Triggers

Sequential logic:
```skalp
on(clk.rise) {
    // Triggered on rising edge
}

on(clk.fall) {
    // Triggered on falling edge
}
```

Reset events:
```skalp
on(reset.active) {
    // Triggered when reset is active (asynchronous)
}

on(reset.inactive) {
    // Triggered when reset becomes inactive
}
```

Combined events:
```skalp
on(clk.rise, reset.active) {
    // Triggered on clock edge OR reset active
    if (reset) {
        // Reset logic
    } else {
        // Normal clocked logic
    }
}
```

Combinational logic:
```skalp
on(signal.change) {
    // Triggered on any change
}
```

### Assignment Types

| Operator | Type | Usage |
|----------|------|-------|
| `<=` | Non-blocking | Sequential logic |
| `:=` | Blocking | Procedural blocks |
| `=` | Continuous | Combinational logic |

### Control Flow

If-else statements:
```skalp
if (condition) {
    // statements
} else {
    // statements
}
```

Case statements:
```skalp
match (expression) {
    value1 -> { statements }
    value2 -> { statements }
    _ -> { default statements }
}
```

## Examples

### Simple Counter

```skalp
entity Counter {
    in clk: clock
    in reset: reset
    in enable: bit
    out count: bit[8]
}

impl Counter {
    signal count_reg: bit[8] = 0

    on(clk.rise) {
        if (reset) {
            count_reg <= 0
        } else {
            if (enable) {
                count_reg <= count_reg + 1
            }
        }
    }

    count = count_reg
}
```

### Asynchronous Reset Counter

```skalp
entity AsyncCounter {
    in clk: clock
    in reset: reset
    out count: bit[8]
}

impl AsyncCounter {
    signal count_reg: bit[8] = 0

    // Asynchronous reset
    on(reset.active) {
        count_reg <= 0
    }

    // Normal counting
    on(clk.rise) {
        count_reg <= count_reg + 1
    }

    count = count_reg
}
```

### Combinational Adder

```skalp
entity Adder {
    in a: bit[8]
    in b: bit[8]
    out sum: bit[9]
}

impl Adder {
    sum = a + b
}
```

### Hierarchical Design

```skalp
entity Top {
    in sys_clk: clock
    in sys_reset: reset
    out result: bit[8]
}

impl Top {
    instance counter1: Counter {
        clk: sys_clk
        reset: sys_reset
        enable: 1'b1
        count: result
    }
}
```

## Compilation Process

### HIR Generation
The frontend parses SKALP code and generates HIR (High-level IR).

### MIR Transformation
HIR is transformed to MIR with:
- Process generation from event blocks
- Type resolution
- Hierarchy flattening

### Optimization
Optional optimization passes:
- Dead code elimination
- Constant folding

### Code Generation
MIR is translated to SystemVerilog with:
- Modern SV syntax
- Proper sensitivity lists
- Name preservation

## Generated Code Example

Input SKALP:
```skalp
entity Simple {
    in a: bit[4]
    in b: bit[4]
    out c: bit[4]
}

impl Simple {
    c = a & b
}
```

Output SystemVerilog:
```systemverilog
module Simple (
    input  wire [3:0] a,
    input  wire [3:0] b,
    output wire [3:0] c
);

    assign c = a & b;

endmodule
```

## Simulation

Generated SystemVerilog can be simulated with:
- Verilator (recommended)
- iverilog
- Commercial simulators (VCS, ModelSim, etc.)

### Using Verilator

1. Generate SystemVerilog from SKALP
2. Create testbench
3. Compile with Verilator:
```bash
verilator --cc --exe --trace module.sv testbench.cpp
```
4. Run simulation:
```bash
./obj_dir/Vmodule
```

## Current Limitations

### Not Yet Supported
- Interfaces
- Packages
- Generate blocks
- Assertions (SVA)
- Functions and tasks
- Memories/arrays
- Parameterized modules

### Workarounds
- Use explicit port connections instead of interfaces
- Inline functions manually
- Use fixed-size arrays

## Best Practices

1. **Use descriptive names**: Signal names are preserved in output
2. **Separate concerns**: Use different event blocks for different functionality
3. **Reset handling**: Always include reset logic for sequential blocks
4. **Type consistency**: Match port and signal types carefully
5. **Hierarchy planning**: Design module hierarchy upfront

## Troubleshooting

### Common Issues

**Issue**: Signal not found in generated code
**Solution**: Check that signal is actually used (dead code elimination)

**Issue**: Incorrect sensitivity list
**Solution**: Verify event trigger specification

**Issue**: Type mismatch errors
**Solution**: Ensure consistent bit widths

## Future Features

Phase 3 will introduce:
- Clock domain crossing
- Advanced type system
- Formal verification support
- Advanced optimizations

## Getting Help

- Check examples in `examples/` directory
- Review test cases in `tests/`
- Consult architecture docs in `docs/`