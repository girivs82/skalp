# SKALP Advanced Language Features

This document describes the advanced language features available in SKALP, the hardware synthesis language.

## Table of Contents
1. [Pattern Matching](#pattern-matching)
2. [Flow Blocks and Pipeline Design](#flow-blocks)
3. [Traits and Interfaces](#traits)
4. [Generic Entities](#generic-entities)
5. [Design Intents](#design-intents)

## Pattern Matching {#pattern-matching}

SKALP supports pattern matching for control flow, making it easier to express complex state machines and decoders.

### Basic Match Expression

```skalp
entity Decoder {
    in opcode: nat[4]
    out control: nat[8]
}

impl Decoder {
    match opcode {
        0b0001 -> control = 0x10,  // ADD
        0b0010 -> control = 0x20,  // SUB
        0b0100 -> control = 0x40,  // MUL
        0b1000 -> control = 0x80,  // DIV
        _      -> control = 0x00,  // Default
    }
}
```

### Pattern Types

- **Literal patterns**: Binary (`0b1010`), hexadecimal (`0xFA`), decimal (`42`)
- **Identifier patterns**: Bind matched value to a variable
- **Wildcard pattern**: `_` matches any value
- **Tuple patterns**: Match multiple values simultaneously

### Advanced Pattern Example

```skalp
entity ALU {
    in op: nat[3]
    in a: nat[32]
    in b: nat[32]
    out result: nat[32]
}

impl ALU {
    match op {
        0 -> result = a + b,
        1 -> result = a - b,
        2 -> result = a & b,
        3 -> result = a | b,
        4 -> result = a ^ b,
        5 -> result = a << b[4:0],
        6 -> result = a >> b[4:0],
        _ -> result = 0,
    }
}
```

## Flow Blocks and Pipeline Design {#flow-blocks}

Flow blocks provide a high-level abstraction for designing pipelined hardware with automatic register insertion.

### Pipeline Operator `|>`

The pipeline operator (`|>`) connects stages in a dataflow pipeline, with implicit register insertion between stages.

```skalp
entity Pipeline {
    in data_in: nat[32]
    out data_out: nat[32]
}

impl Pipeline {
    flow {
        data_in
        |> stage1
        |> stage2
        |> stage3
        |> data_out
    }
}
```

### Block Stages

Complex pipeline stages can be expressed as blocks:

```skalp
entity Processor {
    in instruction: nat[32]
    out result: nat[32]
}

impl Processor {
    flow {
        instruction
        |> {
            decoded = decode(instruction)
            operands = fetch_operands(decoded)
        }
        |> {
            exec_result = execute(decoded, operands)
        }
        |> {
            result = writeback(exec_result)
        }
    }
}
```

### Benefits

- **Automatic register insertion**: Registers are automatically inserted between pipeline stages
- **Clear dataflow**: Visual representation of data movement through the pipeline
- **Synthesis optimization**: The synthesis tool can optimize pipeline timing and resource usage

## Traits and Interfaces {#traits}

Traits define interfaces that entities can implement, enabling polymorphic hardware design and code reuse.

### Trait Definition

```skalp
trait Resetable {
    type ResetValue: nat[32];
    const DEFAULT_RESET: nat[32] = 0;

    reset() -> bit;
    get_reset_value() -> ResetValue;
}
```

### Trait Implementation

```skalp
entity Counter {
    in clk: clock
    in rst: reset
    out count: nat[32]
}

impl Resetable for Counter {
    type ResetValue = nat[32];

    reset() -> bit {
        count <= DEFAULT_RESET
    }

    get_reset_value() -> nat[32] {
        DEFAULT_RESET
    }
}
```

### Super Traits

Traits can extend other traits:

```skalp
trait Verifiable {
    verify() -> bit;
}

trait Testable : Verifiable {
    test() -> bit;

    run_all_tests() -> bit {
        verify() && test()
    }
}
```

## Generic Entities {#generic-entities}

Generic entities allow parameterization of hardware components for reusability.

### Type Parameters

```skalp
entity Buffer<T> {
    in data_in: T
    in write_en: bit
    out data_out: T
}

impl<T> Buffer<T> {
    signal storage: T

    on(write_en.rise) {
        storage <= data_in
    }

    data_out = storage
}
```

### Const Generic Parameters

```skalp
entity FIFO<const DEPTH: nat[16], const WIDTH: nat[8]> {
    in push_data: nat[WIDTH]
    in push: bit
    in pop: bit
    out pop_data: nat[WIDTH]
    out empty: bit
    out full: bit
}

impl<const DEPTH: nat[16], const WIDTH: nat[8]> FIFO<DEPTH, WIDTH> {
    signal memory: nat[WIDTH][DEPTH]
    signal read_ptr: nat[16] = 0
    signal write_ptr: nat[16] = 0

    on(push.rise) {
        if (!full) {
            memory[write_ptr] <= push_data
            write_ptr <= (write_ptr + 1) % DEPTH
        }
    }

    on(pop.rise) {
        if (!empty) {
            read_ptr <= (read_ptr + 1) % DEPTH
        }
    }

    pop_data = memory[read_ptr]
    empty = (read_ptr == write_ptr)
    full = ((write_ptr + 1) % DEPTH == read_ptr)
}
```

### Trait Bounds on Generics

```skalp
trait Arithmetic {
    add(self, other: Self) -> Self;
    sub(self, other: Self) -> Self;
}

entity ALU<T: Arithmetic, const WIDTH: nat[32]> {
    in a: T[WIDTH]
    in b: T[WIDTH]
    in op: bit
    out result: T[WIDTH]
}

impl<T: Arithmetic, const WIDTH: nat[32]> ALU<T, WIDTH> {
    result = op ? a.add(b) : a.sub(b)
}
```

### Where Clauses

```skalp
entity Pipeline<T> where T: Arithmetic + Verifiable {
    in input: T
    out output: T
}
```

## Design Intents {#design-intents}

Design intents specify optimization goals and constraints for the synthesis process.

### Intent Declaration

```skalp
intent HighPerformance {
    timing: 250 MHz
    throughput: maximize
    power: < 10 W
    area: minimize
}
```

### Intent for Specific Entity

```skalp
entity Processor {
    in clk: clock
    in data: nat[64]
    out result: nat[64]
}

intent FastProcessor for Processor {
    timing: 300 MHz
    latency: < 5 cycles
    power: < 5 W
}

impl Processor with FastProcessor {
    // Implementation guided by intent constraints
    // Synthesis tool will optimize for specified goals
}
```

### Constraint Types

- **timing**: Clock frequency constraints (e.g., `100 MHz`, `> 200 MHz`)
- **power**: Power consumption constraints (e.g., `< 5 W`, `minimize`)
- **area**: Resource usage constraints (e.g., `< 1000 LUTs`, `minimize`)
- **throughput**: Data throughput constraints (e.g., `maximize`, `> 1 Gbps`)
- **latency**: Pipeline latency constraints (e.g., `< 10 cycles`, `minimize`)

### Optimization Directives

```skalp
intent Balanced {
    power: minimize
    timing: 100 MHz
    area: < 5000 LUTs + 2000 FFs
}

intent LowPower {
    power: minimize
    timing: 50 MHz  // Lower frequency for power savings
}

intent MaxThroughput {
    throughput: maximize
    latency: minimize
    power: < 15 W  // Power budget constraint
}
```

### Hierarchical Intent Propagation

Intents can be propagated through the design hierarchy:

```skalp
intent SystemLevel {
    power: < 20 W
    timing: 100 MHz
}

entity System with SystemLevel {
    signal proc1: Processor with { power: < 5 W, timing: 100 MHz }
    signal proc2: Processor with { power: < 5 W, timing: 100 MHz }
    signal mem: Memory with { power: < 10 W, timing: 100 MHz }
}
```

## Best Practices

### 1. Pattern Matching
- Always include a default case (`_`) to avoid synthesis warnings
- Order patterns from most specific to least specific
- Use binary/hex literals for bit patterns in decoders

### 2. Flow Blocks
- Keep pipeline stages balanced in complexity
- Use block stages for multi-operation stages
- Consider timing constraints when designing pipelines

### 3. Traits
- Define minimal interfaces in traits
- Provide default implementations where sensible
- Use trait bounds to express requirements clearly

### 4. Generics
- Use const generics for compile-time configuration
- Apply trait bounds to ensure type safety
- Consider synthesis implications of generic instantiation

### 5. Design Intents
- Start with high-level system intents
- Propagate constraints to sub-modules
- Balance competing objectives (power vs. performance)
- Use `minimize`/`maximize` for optimization goals
- Specify concrete values for hard constraints

## Examples

### Complete Example: Generic Pipelined FFT

```skalp
trait Complex<T> {
    type Real: T;
    type Imag: T;

    multiply(self, other: Self) -> Self;
    add(self, other: Self) -> Self;
}

entity FFT<T: Complex, const SIZE: nat[16]> {
    in samples: T[SIZE]
    in valid: bit
    out spectrum: T[SIZE]
    out done: bit
}

intent FastFFT<T: Complex, const SIZE: nat[16]> for FFT<T, SIZE> {
    timing: 200 MHz
    throughput: 1 sample per cycle
    latency: < SIZE * log2(SIZE) cycles
}

impl<T: Complex, const SIZE: nat[16]> FFT<T, SIZE> with FastFFT<T, SIZE> {
    flow {
        samples
        |> { stage1_out = butterfly_stage1(samples) }
        |> { stage2_out = butterfly_stage2(stage1_out) }
        |> { stage3_out = butterfly_stage3(stage2_out) }
        |> { spectrum = final_stage(stage3_out) }
    }

    done = valid.delayed(log2(SIZE))
}
```

This example combines:
- Generic entities with trait bounds
- Flow blocks for pipeline design
- Design intents for optimization
- Complex number arithmetic abstraction

## Conclusion

SKALP's advanced features enable:
- **Modular design** through traits and generics
- **High-performance** implementations with flow blocks
- **Optimization guidance** through design intents
- **Clear control flow** with pattern matching
- **Reusable components** with parameterization

These features work together to create a powerful, expressive language for hardware synthesis that bridges the gap between high-level design intent and low-level hardware implementation.