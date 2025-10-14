# Graphics Pipeline Architecture

## Overview

The SKALP Graphics Pipeline is a hardware 3D rendering accelerator demonstrating professional project organization and SKALP's advanced language features.

## System Architecture

```
                     +------------------+
                     |   CPU (AXI Bus)  |
                     +--------+---------+
                              |
                    sys_clk (100 MHz)
                              |
        +---------------------+-----------------------+
        |          AXI4-Lite Interface               |
        |    (Register Map + Command Decoder)        |
        +-----+---------------------+----------+
              |                     |          |
         Commands             Configuration   Status
              |                     |          |
      +-------v---------+           |          |
      | Command Buffer  |           |          |
      +-------+---------+           |          |
              |                     |          |
      [AsyncFIFO] CDC: sys_clk → geom_clk     |
              |                     |          |
       geom_clk (200 MHz)           |          |
              |                     |          |
        +-----v---------------------v-----+    |
        |    Geometry Processor          |    |
        |  - Vertex Transform (MVP)      |    |
        |  - Lighting Calculation        |    |
        |  - Viewport Transform          |    |
        +-------+------------------------+    |
                |                             |
      [AsyncFIFO] CDC: geom_clk → pixel_clk  |
                |                             |
         pixel_clk (25 MHz)                   |
                |                             |
          +-----v---------+                   |
          |  Rasterizer   |                   |
          +-------+-------+                   |
                  |                           |
          +-------v----------+                |
          | Framebuffer      |                |
          +-------+----------+                |
                  |                           |
          +-------v----------+                |
          | Video Timing Gen |<---------------+
          +-------+----------+
                  |
        +--------+--------+---------+
        |        |        |         |
     hsync    vsync      de      rgb[23:0]
```

## Module Hierarchy

### Top Level (`src/main.sk`)

The `GraphicsPipelineTop` entity integrates all subsystems and manages clock domain crossings.

**Key Features:**
- Multiple clock domain management
- AXI4-Lite slave interface
- Command processing
- Statistics tracking

### AXI Interface (`src/axi/`)

Implements AXI4-Lite slave for CPU communication.

**Register Map:**
- `0x0000-0x00FF`: Command registers
- `0x0100-0x01FF`: Matrix configuration (Model, View, Projection)
- `0x0200-0x02FF`: Lighting configuration  
- `0x0300-0x03FF`: Viewport configuration
- `0x1000-0x10FF`: Status registers (read-only)

### Geometry Processing (`src/geometry/`)

High-speed vertex transformation and lighting.

**Pipeline Stages:**
1. Vertex fetch
2. Model-View-Projection (MVP) transform
3. Lighting calculation (Phong model)
4. Viewport transform
5. Clipping

**Clock Domain:** `geom_clk` (200 MHz for high throughput)

### Rasterization (`src/rasterizer/`)

Converts triangles to pixels.

**Features:**
- Scanline rasterization
- Perspective-correct interpolation
- Z-buffer for depth testing

### Video Output (`src/video/`)

Generates VGA/HDMI timing signals.

**Supported Modes:**
- 640x480 @ 60Hz (pixel_clk = 25.175 MHz)
- Extensible to other resolutions

## Clock Domain Crossing (CDC)

All clock domain crossings use **gray-code async FIFOs** from `lib/fifo/`:

### Crossing Points:

1. **sys_clk → geom_clk**: Command and vertex data
2. **geom_clk → pixel_clk**: Transformed vertices and fragments
3. **pixel_clk → sys_clk**: Status and statistics (synchronized)

### Safety Properties:

- No metastability: All signals cross through proper synchronizers
- FIFO overflow protection: Backpressure via ready/valid handshaking
- Gray-code pointers: Safe for clock domain crossing

## Data Flow

### 1. Command Phase

```
CPU → AXI → Command Decoder → Command FIFO → Geometry Processor
```

### 2. Geometry Phase

```
Vertex Data → Transform → Lighting → Viewport → Vertex FIFO
```

### 3. Rasterization Phase

```
Vertex FIFO → Triangle Setup → Rasterizer → Fragment FIFO
```

### 4. Output Phase

```
Fragment FIFO → Framebuffer → Video Timing → Display
```

## Parametric Type Usage

This project demonstrates SKALP's parametric type system:

### Core Types (`src/types.sk`)

```skalp
// Parametric vector type (instead of manual struct)
pub type Vec3 = vec3<fp32>;

// Parametric matrix type
pub type Matrix4x4 = [[fp32; 4]; 4];

// Fixed-point for less critical paths
pub type ScreenCoord = fixed<16, 8, true>;  // Q8.8 format

// Integer types with explicit width
pub type PixelIndex = u32;
pub type CommandOpcode = u8;
```

## Verification Strategy

### Unit Tests (`verif/testbenches/`)

- `tb_fifo.sk`: FIFO correctness and CDC safety
- `tb_geometry.sk`: Transform and lighting accuracy
- `tb_axi.sk`: AXI protocol compliance
- `tb_video.sk`: Video timing correctness

### Integration Tests

- `tb_top.sk`: Full system integration
- Test vectors in `verif/test_vectors/`
- Golden outputs in `verif/golden/`

### Formal Properties (`verif/properties/`)

- FIFO overflow/underflow protection
- AXI protocol assertions
- CDC metastability prevention
- Clock domain isolation

### Coverage (`verif/coverage/`)

- Functional coverage: All commands, edge cases
- Code coverage: Line and branch coverage targets

## Performance Targets

| Metric | Target | Actual |
|--------|--------|--------|
| Geometry Throughput | 100M vertices/s | TBD |
| Pixel Fill Rate | 25M pixels/s | TBD |
| Latency (vertex→pixel) | < 100 cycles | TBD |
| Max Frequency (geom_clk) | 200 MHz | TBD |

## Resource Utilization

### iCE40-HX8K Estimates:

| Resource | Usage | Percentage |
|----------|-------|------------|
| LUTs | ~4000 | ~50% |
| FFs | ~3000 | ~40% |
| BRAMs | ~20 | ~60% |
| DSPs | 0 | 0% |

## Design Decisions

### Why Three Clock Domains?

1. **sys_clk (100 MHz)**: AXI interface at standard system bus speed
2. **geom_clk (200 MHz)**: 2x speed for geometry to maintain throughput
3. **pixel_clk (25 MHz)**: Locked to video timing requirements

### Why Gray-Code FIFOs?

- Industry standard for CDC
- Proven metastability safety
- Simple and reliable

### Why Parametric Types?

- Type safety: `vec3<fp32>` vs raw `bit[96]`
- Reusability: Generic FIFOs, math functions
- Clarity: `u32` is obviously an integer, `bit[32]` is ambiguous

## Future Enhancements

- [ ] Texture mapping
- [ ] Programmable shaders
- [ ] Multiple render targets
- [ ] Hardware anti-aliasing
- [ ] DMA for vertex streaming

## References

- AXI4 Specification: ARM IHI 0022E
- VGA Timings: VESA DMT Standard  
- SKALP Language Specification
