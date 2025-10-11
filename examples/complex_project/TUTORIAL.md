# Graphics Pipeline Tutorial - Learning Advanced SKALP

This tutorial walks you through understanding the complex graphics pipeline example, explaining each advanced feature step by step.

## Table of Contents

1. [Module Organization](#module-organization)
2. [Parametric Types (Generics)](#parametric-types)
3. [Multiple Clock Domains](#multiple-clock-domains)
4. [Hierarchical Module Instantiation](#hierarchical-module-instantiation)
5. [Pattern Matching](#pattern-matching)
6. [Async FIFOs and CDC](#async-fifos-and-cdc)
7. [Building and Testing](#building-and-testing)

---

## Module Organization

The project is organized into multiple files, each with a specific purpose:

```
src/
├── main.sk                 # Top-level integration
├── types.sk                # Shared data structures
├── async_fifo.sk           # Generic async FIFO
└── geometry_processor.sk   # Vertex transformation pipeline
```

### Module Imports

In `main.sk`, we import other modules:

```skalp
mod types;                     // Declare module
mod async_fifo;
mod geometry_processor;

use types::{Vertex, Color};    // Import specific types
use async_fifo::{AsyncFifo};   // Import generic FIFO
```

**Key Concept**: Use `mod` to declare modules, `use` to import specific items.

---

## Parametric Types (Generics)

### Example 1: Generic FIFO

The AsyncFIFO is generic over both data type and depth:

```skalp
entity AsyncFifo<T, const DEPTH: nat> {
    in wr_data: T           // Can be any type
    out rd_data: T
    // ... ports ...
}

impl AsyncFifo<T, const DEPTH: nat> {
    signal mem: T[DEPTH]    // Array size determined by parameter

    const ADDR_WIDTH: nat = clog2(DEPTH)  // Computed at compile-time
}
```

**Instantiation**:
```skalp
// 16-deep FIFO of Vertex structures
let vertex_fifo = AsyncFifo<Vertex, 16> {
    wr_clk: sys_clk,
    wr_data: my_vertex,
    // ...
}

// 64-deep FIFO of transformed vertices
let output_fifo = AsyncFifo<TransformedVertex, 64> {
    // ...
}
```

**Key Concepts**:
- `<T>` - Type parameter (can be any struct or primitive)
- `<const DEPTH: nat>` - Compile-time constant parameter
- The same code works for any type and size
- No runtime overhead - all resolved at compile time

### Example 2: Configurable Pipeline Depth

```skalp
entity GeometryProcessor<const STAGES: nat = 4> {
    // ... ports ...
}

impl GeometryProcessor<const STAGES: nat> {
    signal pipeline_valid: bit[STAGES]      // Array size from parameter
    signal stage_data: Vec4[STAGES]

    // Conditional compilation based on parameter
    if STAGES > 2 && pipeline_valid[2] {
        // Only exists if STAGES > 2
    }
}
```

**Usage**:
```skalp
// 4-stage pipeline (balanced)
let geom4 = GeometryProcessor<4> { ... }

// 8-stage pipeline (high throughput)
let geom8 = GeometryProcessor<8> { ... }

// 2-stage pipeline (low latency)
let geom2 = GeometryProcessor<2> { ... }
```

---

## Multiple Clock Domains

This design has three independent clock domains:

```
┌─────────────┐     ┌─────────────┐     ┌─────────────┐
│  System     │     │  Geometry   │     │   Pixel     │
│  100 MHz    │────▶│  200 MHz    │────▶│  25 MHz     │
│             │     │             │     │             │
│ AXI Control │     │ Vertex Xform│     │ Raster/Video│
└─────────────┘     └─────────────┘     └─────────────┘
     FIFO                 FIFO
    (CDC)                (CDC)
```

### Defining Multiple Clocks

```skalp
entity GraphicsPipelineTop {
    in sys_clk: clock       // System clock
    in geom_clk: clock      // Geometry clock (faster)
    in pixel_clk: clock     // Pixel clock (slower)

    in sys_rst: reset(active_high)
    in geom_rst: reset(active_high)
    in pixel_rst: reset(active_high)

    // ... other ports ...
}
```

### Clock Domain Assignment

Signals and processes belong to specific clock domains:

```skalp
// System clock domain
on(sys_clk.rise) {
    if sys_rst {
        // Reset logic
    } else {
        // System clock operations
        model_matrix <= new_matrix
    }
}

// Geometry clock domain
on(geom_clk.rise) {
    if geom_rst {
        // Reset logic
    } else {
        // Geometry operations at 200 MHz
        transformed_vertex <= transform(vertex)
    }
}

// Pixel clock domain
on(pixel_clk.rise) {
    if pixel_rst {
        // Reset logic
    } else {
        // Pixel operations at 25 MHz
        video_out <= framebuffer[pixel_addr]
    }
}
```

**Key Concept**: Each `on(clk.rise)` block defines logic in that clock domain. SKALP tracks domains and enforces CDC rules.

---

## Hierarchical Module Instantiation

### Pattern: Component Instantiation

```skalp
// Declare signals for component connections
signal geom_vertex_valid: bit
signal geom_vertex: Vertex
signal geom_vertex_ready: bit
signal geom_output: TransformedVertex

// Instantiate component
let geometry = GeometryProcessor4 {
    // Connect clock and reset
    clk: geom_clk,
    rst: geom_rst,

    // Connect input ports
    vertex_valid: geom_vertex_valid,
    vertex: geom_vertex,
    vertex_ready: geom_vertex_ready,

    // Connect configuration
    model_matrix: model_matrix,
    view_matrix: view_matrix,
    proj_matrix: proj_matrix,

    // Connect outputs
    output: geom_output,
    output_valid: geom_output_valid,
    // ...
}
```

### Multiple Instances

```skalp
// Two FIFOs with different parameters
let vertex_fifo = AsyncFifo<Vertex, 16> {
    wr_clk: sys_clk,
    rd_clk: geom_clk,
    // ...
}

let transform_fifo = AsyncFifo<TransformedVertex, 64> {
    wr_clk: geom_clk,
    rd_clk: pixel_clk,
    // ...
}
```

**Key Points**:
- Each instance gets unique parameter values
- Signals connect instances together
- Clock domains are explicit in connections

---

## Pattern Matching

Pattern matching provides clean conditional logic:

### Example 1: State Machine

```skalp
signal state: GeometryState  // Enum type

on(clk.rise) {
    match state {
        GeometryState::Idle => {
            if vertex_valid {
                state <= GeometryState::LoadVertex
            }
        }

        GeometryState::LoadVertex => {
            pipeline_valid[0] <= 1
            state <= GeometryState::Transform
        }

        GeometryState::Transform => {
            if pipeline_done {
                state <= GeometryState::WriteOutput
            }
        }

        GeometryState::WriteOutput => {
            state <= GeometryState::Idle
        }

        _ => {  // Default case
            state <= GeometryState::Idle
        }
    }
}
```

### Example 2: Command Decoder

```skalp
match current_command.opcode {
    CommandOpcode::DrawTriangle => {
        start_draw <= 1
        triangle_addr <= command.vertex_addr
    }

    CommandOpcode::SetTransform => {
        load_matrix <= 1
        matrix_addr <= command.vertex_addr
    }

    CommandOpcode::ClearBuffer => {
        clear_enable <= 1
    }

    _ => {
        // Unknown command - ignore
    }
}
```

### Example 3: Register Map Decoder

```skalp
match axi_awaddr[15:0] {
    0x0000 => current_command <= axi_wdata
    0x0100 => model_matrix.col0.x <= axi_wdata
    0x0104 => model_matrix.col0.y <= axi_wdata
    0x0200 => light_direction.x <= axi_wdata
    _ => { /* Ignore unknown addresses */ }
}
```

**Benefits**:
- More readable than nested if/else
- Compiler ensures all cases are handled
- Efficient synthesis to multiplexers

---

## Async FIFOs and CDC

Async FIFOs safely cross clock domain boundaries using Gray code pointers.

### Why Gray Code?

When pointers cross clock domains, only one bit changes at a time, preventing metastability issues:

```
Binary:  0 -> 1 -> 2 -> 3 -> 4 -> 5
         000  001  010  011  100  101
         (3 bits change at once - dangerous!)

Gray:    0 -> 1 -> 3 -> 2 -> 6 -> 7
         000  001  011  010  110  111
         (only 1 bit changes - safe!)
```

### FIFO Structure

```skalp
impl AsyncFifo<T, const DEPTH: nat> {
    // Write domain signals
    signal wr_ptr: bit[PTR_WIDTH]          // Binary pointer
    signal wr_ptr_gray: bit[PTR_WIDTH]     // Gray code pointer
    signal rd_ptr_gray_sync2: bit[PTR_WIDTH]  // Synchronized from read domain

    // Read domain signals
    signal rd_ptr: bit[PTR_WIDTH]
    signal rd_ptr_gray: bit[PTR_WIDTH]
    signal wr_ptr_gray_sync2: bit[PTR_WIDTH]  // Synchronized from write domain

    // Write clock domain
    on(wr_clk.rise) {
        // Synchronize read pointer (2 flip-flops)
        rd_ptr_gray_sync1 <= rd_ptr_gray
        rd_ptr_gray_sync2 <= rd_ptr_gray_sync1

        if wr_en && !wr_full {
            mem[wr_addr] <= wr_data
            wr_ptr <= wr_ptr + 1
            wr_ptr_gray <= binary_to_gray(wr_ptr + 1)
        }
    }

    // Read clock domain
    on(rd_clk.rise) {
        // Synchronize write pointer (2 flip-flops)
        wr_ptr_gray_sync1 <= wr_ptr_gray
        wr_ptr_gray_sync2 <= wr_ptr_gray_sync1

        if rd_en && !rd_empty {
            rd_ptr <= rd_ptr + 1
            rd_ptr_gray <= binary_to_gray(rd_ptr + 1)
        }
    }
}
```

### CDC Safety Rules

1. **Two-flip-flop synchronizers**: Prevent metastability
2. **Gray code for multi-bit signals**: Only one bit changes
3. **Separate full/empty logic**: Each calculated in its own domain
4. **No combinational paths**: All signals registered

### Using the FIFO

```skalp
// Write side (sys_clk domain)
if !vertex_fifo_wr_full {
    vertex_fifo_wr_en <= 1
    vertex_fifo_wr_data <= new_vertex
}

// Read side (geom_clk domain)
if !vertex_fifo_rd_empty {
    vertex_fifo_rd_en <= 1
    let vertex = vertex_fifo_rd_data
    process_vertex(vertex)
}
```

---

## Building and Testing

### Step 1: Build to SystemVerilog

```bash
cd examples/complex_project
skalp build -s src/main.sk -o build/
```

This generates `build/design.sv` with:
- All modules flattened and instantiated
- Proper clock domain crossing
- Optimized logic

### Step 2: Build for Simulation

```bash
skalp build -s src/main.sk -t mir -o build/
```

This generates `build/design.mir` (mid-level IR) for simulation.

### Step 3: Simulate

```bash
skalp sim build/design.mir -d 1000
```

Simulates for 1000 cycles and produces waveform.

### Step 4: Synthesize for FPGA

```bash
skalp synth src/main.sk --device ice40-hx8k
```

---

## Key Takeaways

### 1. **Parametric Types Enable Reuse**
```skalp
AsyncFifo<Vertex, 16>         // Reuse same FIFO for different types
AsyncFifo<TransformedVertex, 64>
GeometryProcessor<4>          // Same processor, different pipeline depth
```

### 2. **Explicit Clock Domains Prevent Bugs**
```skalp
on(sys_clk.rise) { /* sys_clk operations */ }
on(geom_clk.rise) { /* geom_clk operations */ }
// Compiler tracks domains and enforces CDC rules
```

### 3. **Pattern Matching Improves Readability**
```skalp
match state {
    Idle => { ... }
    Active => { ... }
    Done => { ... }
}
```

### 4. **Hierarchical Design Manages Complexity**
```
Top
├── AXI Interface
├── FIFO (CDC)
├── Geometry Processor
│   ├── Transform Stage
│   ├── Lighting Stage
│   └── Output Stage
├── FIFO (CDC)
└── Rasterizer
```

### 5. **Types Make Intent Clear**
```skalp
struct Vertex { position: Vec3, normal: Vec3, ... }
struct TransformedVertex { position: Vec4, ... }
// Clear data flow through pipeline
```

---

## Next Steps

1. **Modify parameters**: Change FIFO depths, pipeline stages
2. **Add features**: Implement texture sampling, z-buffer
3. **Optimize**: Adjust clock frequencies, add parallelism
4. **Extend**: Add more shader operations, multiple lights

---

## Further Reading

- [SKALP Parametric Types Guide](../../docs/PARAMETRIC_TYPES_GUIDE.md)
- [Clock Domain Crossing](../../docs/CDC.md)
- [Module System](../../docs/LIBRARY_SYSTEM.md)
- [Pattern Matching](../../docs/user/reference/syntax.md#pattern-matching)
