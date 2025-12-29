# NCL (Null Convention Logic) Examples

This directory contains examples of asynchronous circuit design using NCL.

## What is NCL?

NCL (Null Convention Logic) is a delay-insensitive asynchronous design methodology that:
- Eliminates clocks entirely
- Uses dual-rail encoding (2 wires per logical bit)
- Uses threshold gates (THmn) with hysteresis
- Achieves self-timed operation through completion detection

## Examples

### Basic Examples

| File | Description |
|------|-------------|
| `ncl_inverter.sk` | Simplest NCL circuit - dual-rail inversion |
| `ncl_adder.sk` | 8-bit ripple-carry adder |
| `ncl_alu.sk` | Full ALU with arithmetic and logic ops |

### Advanced Examples

| File | Description |
|------|-------------|
| `ncl_pipeline.sk` | Multi-stage pipeline with barriers |

## Key Concepts

### Async Entity

```skalp
async entity MyModule {
    in a: bit[8]
    out y: bit[8]
}
```

The `async` keyword declares a clockless NCL module.

### Barrier Statement

```skalp
impl MyPipeline {
    let stage1 = input * 2
    barrier                    // Completion detection here
    let stage2 = stage1 + 1
    barrier                    // And here
    output = stage2
}
```

Barriers mark pipeline stage boundaries and insert completion detection.

### Dual-Rail Encoding

Each bit becomes two wires:
- `signal_t` (true rail)
- `signal_f` (false rail)

| Logical | True | False | Meaning |
|---------|------|-------|---------|
| NULL    | 0    | 0     | No data |
| 0       | 0    | 1     | Logic 0 |
| 1       | 1    | 0     | Logic 1 |

## Building Examples

```bash
# Build an NCL example to Verilog
skalp build -s examples/ncl/ncl_adder.sk -o output/

# The output includes:
# - Structural Verilog with THmn gates
# - NCL cell library definitions
```

## Documentation

See [NCL Async Circuits](../../docs/NCL_ASYNC_CIRCUITS.md) for complete documentation.
