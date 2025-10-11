# Graphics Pipeline Accelerator - Complex SKALP Example

This is a comprehensive example demonstrating all advanced features of SKALP through a multi-module hierarchical design of a 3D graphics pipeline accelerator.

## Architecture Overview

```
┌─────────────────────────────────────────────────────────────┐
│                 GraphicsPipelineTop                         │
│                                                             │
│  ┌────────────┐    ┌──────────────┐    ┌──────────────┐  │
│  │   AXI4     │───▶│  Geometry    │───▶│  Rasterizer  │  │
│  │  Slave     │    │  Processor   │    │              │  │
│  │            │    │              │    │              │  │
│  │ (System    │    │ (Fast Clock) │    │ (Pixel Clk)  │  │
│  │   Clock)   │    └──────────────┘    └──────────────┘  │
│  └────────────┘            │                    │         │
│                            ▼                    ▼         │
│                    ┌──────────────┐    ┌──────────────┐  │
│                    │  AsyncFIFO   │    │ AsyncFIFO    │  │
│                    │   (CDC)      │    │   (CDC)      │  │
│                    └──────────────┘    └──────────────┘  │
│                            │                    │         │
│                            ▼                    ▼         │
│                    ┌──────────────┐    ┌──────────────┐  │
│                    │  Vertex      │    │   Fragment   │  │
│                    │  Shader      │    │   Shader     │  │
│                    └──────────────┘    └──────────────┘  │
└─────────────────────────────────────────────────────────────┘
```

## Features Demonstrated

### 1. Module Hierarchy
- Top-level system integrating multiple subsystems
- Hierarchical instantiation with parameter passing
- Clean separation of concerns

### 2. Parametric Types (Generics)
- `Vector<T, const N: nat>` - Generic N-dimensional vectors
- `Transform<const STAGES: nat>` - Configurable pipeline depth
- `FIFO<T, const DEPTH: nat>` - Generic FIFO with any data type

### 3. Traits and Generic Programming
- `Numeric` trait for arithmetic operations
- `Transformable` trait for geometric operations
- Constraint-based generic functions

### 4. Multiple Clock Domains
- System clock (AXI interface)
- Geometry clock (high-speed processing)
- Pixel clock (display timing)
- Proper CDC with async FIFOs and Gray code pointers

### 5. Pattern Matching
- Command decoder using `match` statements
- State machine with enum types
- Opcode dispatch

### 6. Advanced Data Structures
- Vertex structure with position, normal, UV
- Transformation matrices
- Packed data formats

### 7. Realistic Interfaces
- AXI4-Lite slave for CPU communication
- Handshake protocols
- Back-pressure handling

### 8. Floating-Point and Fixed-Point Math
- Vector operations (dot, cross, normalize)
- Matrix transformations
- Interpolation

## Project Structure

```
complex_project/
├── README.md                      # This file
├── skalp.toml                     # Project manifest
├── src/
│   ├── main.sk                    # Top-level module
│   ├── axi4_interface.sk          # AXI4-Lite slave interface
│   ├── geometry_processor.sk      # Vertex transformation pipeline
│   ├── rasterizer.sk              # Triangle rasterization
│   ├── vertex_shader.sk           # Programmable vertex shader
│   ├── fragment_shader.sk         # Programmable fragment shader
│   ├── async_fifo.sk              # CDC FIFO
│   ├── types.sk                   # Common type definitions
│   └── utils.sk                   # Utility functions
├── tests/
│   ├── test_geometry.sk           # Geometry processor tests
│   └── test_pipeline.sk           # Full pipeline tests
└── examples/
    └── simple_triangle.sk         # Example: Draw a triangle
```

## Building and Running

### Build SystemVerilog
```bash
cd examples/complex_project
skalp build -s src/main.sk -o build/
```

### Simulate
```bash
skalp build -t mir -o build/
skalp sim build/design.mir -d 1000
```

### Synthesize for FPGA
```bash
skalp synth src/main.sk --device ice40-hx8k
```

## Key Concepts

### Parametric Module Instantiation
```skalp
let fifo = AsyncFifo<Vertex, 16> {  // 16-deep FIFO of Vertex structs
    wr_clk: geom_clk,
    rd_clk: pixel_clk,
    // ...
}
```

### Generic Vector Operations
```skalp
let normalized = VecNormalize<fp32, 3> {
    v: surface_normal,
    result: normal_out
}
```

### Pattern Matching
```skalp
match command.opcode {
    CMD_DRAW_TRIANGLE => { /* ... */ }
    CMD_SET_TRANSFORM => { /* ... */ }
    CMD_CLEAR_BUFFER => { /* ... */ }
    _ => { /* Unknown command */ }
}
```

### Trait-Based Generic Functions
```skalp
fn dot_product<T: Numeric, const N: nat>(a: T[N], b: T[N]) -> T {
    let mut sum = T::ZERO
    for i in 0..N {
        sum = sum + a[i] * b[i]
    }
    sum
}
```

## Performance Characteristics

| Component | Latency | Throughput |
|-----------|---------|------------|
| Vertex Transform | 12 cycles | 1 vertex/cycle (pipelined) |
| Rasterizer | Variable | ~1 pixel/cycle |
| Vertex Shader | 20 cycles | 1 vertex/cycle |
| Fragment Shader | 15 cycles | 4 pixels/cycle (SIMD) |

## Learning Path

1. **Start with types.sk** - Understand data structures
2. **Review async_fifo.sk** - Learn CDC patterns
3. **Study geometry_processor.sk** - See pipeline architecture
4. **Examine main.sk** - Understand system integration
5. **Read shader files** - Learn computation patterns

## Next Steps

- Extend with texture sampling unit
- Add Z-buffer for depth testing
- Implement alpha blending
- Add more shader operations

## References

- [SKALP Language Guide](../../docs/user/quick-start.md)
- [Parametric Types](../../docs/PARAMETRIC_TYPES_GUIDE.md)
- [Clock Domain Crossing](../../docs/CDC.md)
- [Library System](../../docs/LIBRARY_SYSTEM.md)
