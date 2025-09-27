# MIR (Mid-level Intermediate Representation) Design

## Overview

The MIR serves as an intermediate representation between the high-level HIR and the target HDL code (SystemVerilog). It represents hardware at a level close to RTL but remains target-agnostic.

## Architecture

### Core Components

1. **Module** - Represents a hardware module
   - Ports (input/output/inout)
   - Internal signals
   - Variables
   - Processes (always blocks)
   - Continuous assignments
   - Module instances (hierarchy)

2. **Process** - Represents behavioral blocks
   - Sequential (always_ff)
   - Combinational (always_comb)
   - Sensitivity lists

3. **Data Types**
   - Bit vectors (bit[N])
   - Logic vectors (logic[N])
   - Integer types (int, nat)
   - Special types (clock, reset, event)

4. **Statements**
   - Assignments (blocking/non-blocking)
   - Control flow (if/else, case)
   - Loops (for, while)
   - Blocks

5. **Expressions**
   - Literals
   - References (signals, variables, ports)
   - Binary operations
   - Unary operations
   - Conditional expressions
   - Concatenation

## HIR to MIR Transformation

### Mapping Rules

| HIR Construct | MIR Construct |
|--------------|---------------|
| HirEntity | Module |
| HirPort | Port |
| HirSignal | Signal |
| HirVariable | Variable |
| HirEventBlock | Process |
| HirAssignment | Assignment/ContinuousAssign |
| HirExpression | Expression |

### Process Generation

Event blocks are transformed into processes based on their triggers:

- `on(clk.rise)` → Sequential process with posedge sensitivity
- `on(signal.change)` → Combinational process with signal sensitivity
- Continuous assignments → ContinuousAssign

### Type Conversion

HIR types are mapped to appropriate MIR data types:

- `bit[N]` → DataType::Bit(N)
- `logic[N]` → DataType::Logic(N)
- `clock` → DataType::Clock
- `reset` → DataType::Reset

## Optimization Passes

### Dead Code Elimination
Removes unused signals and variables by tracking references throughout the module.

### Constant Folding
Evaluates constant expressions at compile time:
- Arithmetic operations on literals
- Conditional expressions with constant conditions

### Future Optimizations
- Common subexpression elimination
- Loop unrolling
- Resource sharing
- Timing-driven optimizations

## Code Generation

### SystemVerilog Backend

The MIR is translated to SystemVerilog with:
- Human-readable output
- Modern SV features (always_ff, always_comb)
- Proper sensitivity lists
- Hierarchical instantiation support

### Name Preservation

Signal and port names from the original SKALP source are preserved through the compilation pipeline for better debugging and readability.

## Example Transformation

### SKALP Source
```skalp
entity Counter {
    in clk: clock
    in reset: reset
    out count: bit[8]
}

impl Counter {
    signal cnt: bit[8] = 0

    on(clk.rise) {
        if (reset) {
            cnt <= 0
        } else {
            cnt <= cnt + 1
        }
    }

    count = cnt
}
```

### MIR Representation
```rust
Module {
    name: "Counter",
    ports: [
        Port { name: "clk", direction: Input, type: Clock },
        Port { name: "reset", direction: Input, type: Reset },
        Port { name: "count", direction: Output, type: Bit(8) }
    ],
    signals: [
        Signal { name: "cnt", type: Bit(8), initial: Some(0) }
    ],
    processes: [
        Process {
            kind: Sequential,
            sensitivity: Edge([posedge clk]),
            body: Block {
                statements: [
                    If {
                        condition: Ref(Port("reset")),
                        then_block: Assignment(Signal("cnt") <= 0),
                        else_block: Assignment(Signal("cnt") <= Signal("cnt") + 1)
                    }
                ]
            }
        }
    ],
    assignments: [
        ContinuousAssign { lhs: Port("count"), rhs: Signal("cnt") }
    ]
}
```

### Generated SystemVerilog
```systemverilog
module Counter (
    input wire clk,
    input wire reset,
    output wire [7:0] count
);

    logic [7:0] cnt = 0;

    always_ff @(posedge clk) begin
        if (reset) begin
            cnt <= 8'b0;
        end else begin
            cnt <= cnt + 1;
        end
    end

    assign count = cnt;

endmodule
```

## Design Decisions

1. **Explicit Process Types**: Distinguishing between sequential and combinational processes enables better code generation.

2. **Flexible Data Types**: Supporting both `bit` and `logic` types allows users to choose between 2-state and 4-state simulation.

3. **Hierarchical Support**: Module instances are first-class citizens in the MIR, enabling complex hierarchical designs.

4. **Optimization-Ready**: The MIR structure is designed to support various optimization passes efficiently.

## Limitations and Future Work

- **Interfaces**: SystemVerilog interfaces not yet supported
- **Packages**: Package definitions and imports not implemented
- **Assertions**: SVA assertions not yet supported
- **Generate Blocks**: Parameterized generate blocks not implemented

## Testing

The MIR implementation includes comprehensive tests:
- Unit tests for each component
- Integration tests for HIR to MIR transformation
- End-to-end tests with code generation
- Optimization pass tests