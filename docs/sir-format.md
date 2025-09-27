# SIR Format Reference

## Overview

The SIR (Simulation Intermediate Representation) is SKALP's GPU-optimized intermediate representation for hardware simulation. It is designed specifically for parallel execution on GPU compute shaders while maintaining bit-accurate hardware semantics.

## Design Goals

1. **GPU Optimization**: Structure data for efficient GPU parallel execution
2. **Separation of Logic Types**: Distinguish combinational and sequential logic
3. **Minimal Data Transfer**: Reduce CPU-GPU memory bandwidth requirements
4. **Bit Accuracy**: Preserve exact hardware behavior
5. **Scalability**: Support designs from small modules to large systems

## Core Data Structures

### SIR Root

```rust
pub struct Sir {
    /// Design name
    pub name: String,
    /// Top-level module
    pub top_module: SirModule,
    /// All modules in the design (hierarchical)
    pub modules: HashMap<String, SirModule>,
}
```

The `Sir` struct represents a complete hardware design. Most simulations operate on the `top_module`, with `modules` providing hierarchical context.

### SIR Module

```rust
pub struct SirModule {
    /// Module name
    pub name: String,
    /// All signals in this module (flattened)
    pub signals: Vec<SirSignal>,
    /// Combinational logic blocks (GPU parallel)
    pub comb_blocks: Vec<CombinationalBlock>,
    /// Sequential logic blocks (CPU sequential)
    pub seq_blocks: Vec<SequentialBlock>,
    /// Module instances (hierarchy)
    pub instances: Vec<SirInstance>,
    /// Signal connections between instances
    pub connections: Vec<SirConnection>,
}
```

#### Key Features:
- **Flattened Signals**: All signals in a single vector for GPU-friendly access
- **Logic Separation**: Combinational blocks execute on GPU, sequential on CPU
- **Hierarchical Support**: Instances and connections preserve design hierarchy

### SIR Signal

```rust
pub struct SirSignal {
    /// Unique signal ID within module
    pub id: SirSignalId,
    /// Signal name for debugging
    pub name: String,
    /// Signal width in bits
    pub width: usize,
    /// Signal type (Wire, Register, Port)
    pub signal_type: SirSignalType,
    /// Initial value for registers
    pub initial_value: Option<BitVec>,
}

pub enum SirSignalType {
    Wire,
    Register {
        clock: SirSignalId,
        reset: Option<SirSignalId>,
        reset_active_high: bool,
    },
    Port {
        direction: SirPortDirection,
    },
}
```

#### Signal Type Details:

- **Wire**: Combinational signal driven by logic gates
- **Register**: Sequential signal updated on clock edges
- **Port**: Module interface signal (Input/Output/InOut)

### Combinational Block

```rust
pub struct CombinationalBlock {
    /// Block ID
    pub id: CombBlockId,
    /// Input signals this block depends on
    pub inputs: Vec<SirSignalId>,
    /// Output signals this block drives
    pub outputs: Vec<SirSignalId>,
    /// Operations in this block
    pub operations: Vec<SirOperation>,
    /// GPU workgroup size hint
    pub workgroup_size_hint: Option<u32>,
}
```

#### GPU Execution Model:
- Each block can execute independently in parallel
- `inputs` must be available before execution
- `outputs` are produced after execution
- `workgroup_size_hint` optimizes GPU resource allocation

### Sequential Block

```rust
pub struct SequentialBlock {
    /// Block ID
    pub id: SeqBlockId,
    /// Clock signal
    pub clock: SirSignalId,
    /// Clock edge type (Rising/Falling)
    pub clock_edge: EdgeType,
    /// Reset signal and configuration
    pub reset: Option<ResetSpec>,
    /// Registers updated by this block
    pub registers: Vec<SirSignalId>,
    /// Operations in this block
    pub operations: Vec<SirOperation>,
}

pub struct ResetSpec {
    pub signal: SirSignalId,
    pub active_high: bool,
    pub edge: Option<EdgeType>, // For async reset
}
```

#### CPU Execution Model:
- Blocks execute on clock edges
- Reset conditions are checked first
- Operations update register values
- Execution is inherently sequential

## Operations

### SIR Operations

```rust
pub enum SirOperation {
    /// Simple assignment: target = source
    Assign {
        target: SirSignalId,
        source: SirExpression,
    },
    /// Conditional assignment: if (cond) target = source
    ConditionalAssign {
        condition: SirExpression,
        target: SirSignalId,
        source: SirExpression,
    },
    /// Case statement with multiple branches
    Case {
        selector: SirExpression,
        cases: Vec<CaseItem>,
        default: Option<Vec<SirOperation>>,
    },
}
```

### SIR Expressions

```rust
pub enum SirExpression {
    /// Signal reference
    Signal(SirSignalId),
    /// Constant bit vector
    Constant(BitVec),
    /// Binary operation (Add, And, etc.)
    Binary {
        op: BinaryOp,
        left: Box<SirExpression>,
        right: Box<SirExpression>,
    },
    /// Unary operation (Not, Reduce, etc.)
    Unary {
        op: UnaryOp,
        operand: Box<SirExpression>,
    },
    /// Bit selection: signal[index]
    BitSelect {
        signal: SirSignalId,
        index: Box<SirExpression>,
    },
    /// Range selection: signal[high:low]
    RangeSelect {
        signal: SirSignalId,
        high: Box<SirExpression>,
        low: Box<SirExpression>,
    },
    /// Concatenation: {a, b, c}
    Concat(Vec<SirExpression>),
    /// Replication: {n{value}}
    Replicate {
        count: Box<SirExpression>,
        value: Box<SirExpression>,
    },
}
```

#### Expression Types:

1. **Arithmetic**: Add, Sub, Mul, Div, Mod
2. **Logical**: And, Or, Xor (boolean operations)
3. **Bitwise**: BitwiseAnd, BitwiseOr, BitwiseXor (bit-level)
4. **Comparison**: Equal, NotEqual, Less, Greater, etc.
5. **Shift**: LeftShift, RightShift
6. **Reduction**: Reduce all bits to single bit (And, Or, Xor, etc.)

## MIR to SIR Transformation

### Process Classification

The transformer analyzes MIR processes and classifies them:

```rust
match process.kind {
    ProcessKind::Combinational => {
        // → CombinationalBlock
        // Executes on GPU in parallel
    }
    ProcessKind::Sequential => {
        // → SequentialBlock
        // Executes on CPU on clock edges
    }
}
```

### Signal Mapping

1. **ID Assignment**: Each signal gets unique `SirSignalId`
2. **Type Classification**: Wire, Register, or Port
3. **Width Extraction**: Bit width for GPU buffer sizing
4. **Initial Values**: Register reset values

### Dependency Analysis

The transformer builds dependency graphs:

```rust
// For each combinational block
for input_signal in block.inputs {
    // Find which block drives this input
    for other_block in blocks {
        if other_block.outputs.contains(input_signal) {
            // Add dependency: other_block → block
            dependency_graph.add_edge(other_block, block);
        }
    }
}
```

## GPU Shader Generation

### Buffer Layout

SIR signals map to GPU buffers:

```metal
struct InputBuffer {
    uint32_t signal_0;   // 32-bit signal
    uint16_t signal_1;   // 16-bit signal
    uint8_t signal_2;    // 8-bit signal
    // Packed for memory efficiency
};
```

### Operation Translation

SIR operations become Metal shader code:

| SIR Operation | Metal Code |
|---------------|------------|
| `Binary::Add` | `left + right` |
| `Binary::BitwiseAnd` | `left & right` |
| `Unary::Not` | `!operand` |
| `BitSelect` | `(signal >> index) & 1` |
| `RangeSelect` | `(signal >> low) & mask` |

### Workgroup Optimization

```metal
kernel void cone_kernel(
    device const InputBuffer* input [[buffer(0)]],
    device OutputBuffer* output [[buffer(1)]],
    uint gid [[thread_position_in_grid]]
) {
    // Optimized for GPU architecture
    // - Coalesced memory access
    // - Minimal divergent branches
    // - Efficient ALU utilization
}
```

## Performance Considerations

### Memory Layout

1. **Signal Ordering**: Group related signals for cache efficiency
2. **Bit Packing**: Pack narrow signals into wider types
3. **Alignment**: Align to GPU memory requirements

### Execution Order

1. **Dependency Sorting**: Topological order for combinational blocks
2. **Parallel Batches**: Group independent blocks
3. **Load Balancing**: Distribute work across GPU cores

### Optimization Strategies

1. **Cone Merging**: Combine small blocks to reduce overhead
2. **Cone Splitting**: Split large blocks for better parallelism
3. **Constant Folding**: Pre-compute constant expressions
4. **Dead Code Elimination**: Remove unused signals and operations

## Validation and Testing

### Correctness Checks

1. **Signal Consistency**: All signal references are valid
2. **Type Compatibility**: Operations match signal types
3. **Dependency Validation**: No circular dependencies in combinational logic
4. **Reset Consistency**: Reset signals properly connected

### Performance Validation

1. **GPU vs CPU**: Compare results for correctness
2. **Timing Analysis**: Measure execution performance
3. **Resource Usage**: Monitor GPU memory and compute utilization

## Examples

### Simple Counter

```rust
// MIR Input
Process {
    kind: Sequential,
    sensitivity: Edge(clk, Rising),
    body: [
        Assignment {
            target: counter,
            source: Binary::Add(counter, Constant(1))
        }
    ]
}

// SIR Output
SequentialBlock {
    id: SeqBlockId(0),
    clock: SirSignalId(0), // clk
    clock_edge: Rising,
    registers: [SirSignalId(2)], // counter
    operations: [
        Assign {
            target: SirSignalId(2),
            source: Binary {
                op: Add,
                left: Signal(SirSignalId(2)),
                right: Constant(bitvec![1])
            }
        }
    ]
}
```

### Combinational Adder

```rust
// MIR Input
Process {
    kind: Combinational,
    sensitivity: Level([a, b]),
    body: [
        Assignment {
            target: sum,
            source: Binary::Add(a, b)
        }
    ]
}

// SIR Output
CombinationalBlock {
    id: CombBlockId(0),
    inputs: [SirSignalId(0), SirSignalId(1)], // a, b
    outputs: [SirSignalId(2)], // sum
    operations: [
        Assign {
            target: SirSignalId(2),
            source: Binary {
                op: Add,
                left: Signal(SirSignalId(0)),
                right: Signal(SirSignalId(1))
            }
        }
    ]
}
```

## Best Practices

### SIR Generation

1. **Minimize Cone Count**: Reduce GPU kernel launch overhead
2. **Balance Cone Size**: Optimize for GPU workgroup efficiency
3. **Preserve Semantics**: Maintain exact hardware behavior
4. **Optimize Memory**: Pack signals efficiently

### GPU Execution

1. **Batch Operations**: Group similar operations together
2. **Minimize Transfers**: Reduce CPU-GPU communication
3. **Use GPU Features**: Leverage parallel ALUs effectively
4. **Monitor Performance**: Track GPU utilization metrics

## See Also

- [GPU Simulation Architecture](gpu-simulation-architecture.md)
- [Testbench User Guide](testbench-guide.md)
- [Performance Optimization](gpu-performance.md)