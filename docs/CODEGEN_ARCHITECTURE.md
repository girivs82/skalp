# SystemVerilog Code Generation Architecture

## Overview

The code generation module transforms MIR (Mid-level Intermediate Representation) into synthesizable SystemVerilog code. It prioritizes human-readability while maintaining correctness and synthesizability.

## Design Principles

1. **Readability First**: Generated code should be as readable as hand-written code
2. **Modern SystemVerilog**: Use modern SV constructs (always_ff, always_comb, logic)
3. **Name Preservation**: Maintain original signal names from source
4. **Proper Formatting**: Consistent indentation and structure

## Architecture Components

### SystemVerilogGenerator

The main generator class with:
- **State Management**: Tracks current module for name lookups
- **Indentation Control**: Maintains proper code formatting
- **Name Resolution**: Maps IDs to original names

### Code Generation Pipeline

```
MIR Module → Port Generation → Signal Declaration → Process Generation → Output
```

## Generation Rules

### Module Structure

```systemverilog
module <name> (
    // Port declarations
);
    // Signal declarations
    // Variable declarations
    // Process blocks
    // Continuous assignments
    // Module instances
endmodule
```

### Process Generation

| MIR Process Type | SystemVerilog Construct |
|-----------------|------------------------|
| Sequential + Edge | always_ff @(edge list) |
| Combinational | always_comb |
| Level Sensitive | always @(signal list) |

### Data Type Mapping

| MIR Type | SystemVerilog Type |
|----------|-------------------|
| Bit(n) | wire [n-1:0] / logic [n-1:0] |
| Logic(n) | logic [n-1:0] |
| Int(n) | int [n-1:0] |
| Clock | wire |
| Reset | wire |
| Event | event |

### Assignment Types

- **Blocking** (`:=`) → `=`
- **Non-blocking** (`<=`) → `<=`
- **Continuous** (`=`) → `assign`

## Special Handling

### Sensitivity Lists

Edge sensitivity:
```systemverilog
always_ff @(posedge clk or negedge rst_n)
```

Level sensitivity:
```systemverilog
always @(a or b or c)
```

### Hierarchical Instantiation

```systemverilog
module_name instance_name (
    .port1(signal1),
    .port2(signal2)
);
```

### Reset Handling

Active-high reset:
```systemverilog
if (reset) begin
    // Reset logic
end
```

Active-low reset:
```systemverilog
if (!reset_n) begin
    // Reset logic
end
```

## Formatting Rules

### Indentation
- 4 spaces per level
- Consistent block alignment

### Port Declarations
```systemverilog
module example (
    input  wire       clk,
    input  wire       reset,
    output logic [7:0] data
);
```

### Process Blocks
```systemverilog
always_ff @(posedge clk) begin
    if (reset) begin
        signal <= 0;
    end else begin
        signal <= next_value;
    end
end
```

## Optimizations

### Expression Simplification
- Remove unnecessary parentheses
- Combine similar operations

### Constant Propagation
- Replace constant expressions with values
- Simplify conditional expressions

## Error Handling

### Missing Names
If a signal/port name cannot be resolved, fall back to generic names:
- `sig_<id>` for signals
- `var_<id>` for variables
- `port_<id>` for ports

### Type Mismatches
Generate appropriate width conversions when needed.

## Testing Strategy

### Unit Tests
- Test each generation method independently
- Verify formatting correctness

### Integration Tests
- End-to-end MIR to SystemVerilog
- Hierarchical designs
- Complex expressions

### Simulation Tests
- Verify generated code with Verilator
- Compare behavior with reference designs

## Example Generation

### Input MIR
```rust
Module {
    name: "adder",
    ports: [
        Port { name: "a", direction: Input, type: Bit(8) },
        Port { name: "b", direction: Input, type: Bit(8) },
        Port { name: "sum", direction: Output, type: Bit(9) }
    ],
    assignments: [
        ContinuousAssign {
            lhs: Port("sum"),
            rhs: Binary(Add, Port("a"), Port("b"))
        }
    ]
}
```

### Output SystemVerilog
```systemverilog
module adder (
    input  wire [7:0] a,
    input  wire [7:0] b,
    output wire [8:0] sum
);

    assign sum = a + b;

endmodule
```

## Future Enhancements

1. **Comment Preservation**: Maintain comments from source
2. **Attribute Support**: Generate SV attributes/pragmas
3. **Interface Generation**: Support for SV interfaces
4. **Package Support**: Generate package definitions
5. **Parameterization**: Support for parameterized modules
6. **Assertion Generation**: SVA assertion support

## Performance Considerations

- **String Building**: Efficient string concatenation
- **Memory Usage**: Stream large outputs
- **Parallel Generation**: Generate independent modules in parallel

## Configuration Options

Future configuration support:
- Output style (compact/expanded)
- Naming conventions
- Comment generation
- Synthesis pragmas