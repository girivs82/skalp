# Graphics Pipeline Accelerator - Complete Example Index

**A comprehensive, production-quality example demonstrating all advanced SKALP features**

## 📋 Quick Start

```bash
# Navigate to project
cd examples/complex_project

# Build to SystemVerilog
skalp build -s src/main.sk -o build/

# Read the tutorial
cat TUTORIAL.md

# Explore the code
ls -l src/
```

## 📚 Documentation

### For Learning
1. **[README.md](README.md)** - Start here
   - Architecture overview
   - Feature summary
   - Quick examples
   - ~60 lines

2. **[TUTORIAL.md](TUTORIAL.md)** - Deep dive
   - Step-by-step explanations
   - Code walkthroughs
   - Key concepts
   - ~400 lines

3. **[BUILD_GUIDE.md](BUILD_GUIDE.md)** - Practical guide
   - Build commands
   - Troubleshooting
   - Synthesis notes
   - ~350 lines

### For Reference
- **[skalp.toml](skalp.toml)** - Project configuration
- **[INDEX.md](INDEX.md)** - This file

## 🗂️ Source Code Organization

### Core Modules (1580 lines total)

#### 1. [src/types.sk](src/types.sk) - 200 lines
**Data Structures and Type Definitions**

Defines all shared types used throughout the pipeline:
```skalp
pub struct Vec3 { x, y, z }
pub struct Vertex { position, normal, texcoord, color }
pub struct TransformedVertex { ... }
pub enum CommandOpcode { DrawTriangle, SetTransform, ... }
```

**Key Features**:
- Structured data types
- Enums for commands
- Helper functions
- Type aliases

**Learning Focus**: How to organize shared types in a multi-module project

---

#### 2. [src/async_fifo.sk](src/async_fifo.sk) - 250 lines
**Generic Clock Domain Crossing FIFO**

Production-quality async FIFO demonstrating:
```skalp
entity AsyncFifo<T, const DEPTH: nat> {
    in wr_clk: clock
    in rd_clk: clock
    // Safe CDC with Gray code pointers
}
```

**Key Features**:
- **Parametric types**: `<T, const DEPTH: nat>`
- **Multiple clocks**: Independent read/write domains
- **Gray code pointers**: Metastability protection
- **Compile-time evaluation**: `clog2(DEPTH)` computed at compile time

**Learning Focus**:
- How to write generic hardware modules
- Safe clock domain crossing patterns
- Compile-time parameter computation

**CDC Safety**:
- 2-flip-flop synchronizers
- Only Gray code crosses domains
- Separate full/empty logic per domain

---

#### 3. [src/geometry_processor.sk](src/geometry_processor.sk) - 280 lines
**Configurable Vertex Transformation Pipeline**

Multi-stage pipelined processor:
```skalp
entity GeometryProcessor<const STAGES: nat = 4> {
    // Configurable pipeline depth
}
```

**Key Features**:
- **Parametric pipeline depth**: 2, 4, or 8 stages
- **State machine**: Using pattern matching
- **Complex data flow**: Multiple transformation stages
- **Conditional compilation**: Logic depends on STAGES parameter

**Pipeline Stages**:
1. Load vertex data
2. Model transformation (object → world space)
3. View transformation (world → camera space)
4. Projection + lighting (camera → clip space)

**Learning Focus**:
- Pipelined architecture
- Pattern matching for state machines
- Conditional logic based on parameters
- Complex arithmetic operations

---

#### 4. [src/main.sk](src/main.sk) - 850 lines
**Top-Level System Integration**

Complete system with three clock domains:
```skalp
entity GraphicsPipelineTop {
    in sys_clk: clock      // 100 MHz - Control
    in geom_clk: clock     // 200 MHz - Processing
    in pixel_clk: clock    // 25 MHz - Display
}
```

**Key Features**:
- **Multiple clock domains**: System, geometry, pixel
- **Hierarchical instantiation**: FIFOs, processor, rasterizer
- **AXI4-Lite interface**: CPU communication
- **Module imports**: Uses all other modules

**Architecture**:
```
AXI Interface → AsyncFIFO → GeometryProcessor → AsyncFIFO → Rasterizer → Video Out
(sys_clk)       (CDC)       (geom_clk)          (CDC)       (pixel_clk)
```

**Learning Focus**:
- System-level integration
- Multi-clock design
- Interface protocols
- Module hierarchy

## 🎯 Feature Showcase

### 1. Parametric Types (Generics)

**Type Parameters**:
```skalp
entity AsyncFifo<T, const DEPTH: nat> { ... }

// Usage:
let fifo1 = AsyncFifo<Vertex, 16> { ... }
let fifo2 = AsyncFifo<Color, 64> { ... }
```

**Files**: `async_fifo.sk`, `geometry_processor.sk`

### 2. Multiple Clock Domains

**Three Independent Clocks**:
```skalp
on(sys_clk.rise) { /* System logic */ }
on(geom_clk.rise) { /* Geometry logic */ }
on(pixel_clk.rise) { /* Pixel logic */ }
```

**Files**: `main.sk`

### 3. Clock Domain Crossing

**Safe CDC with Gray Code**:
```skalp
// Write domain
wr_ptr_gray <= binary_to_gray(wr_ptr + 1)

// Read domain (after 2-FF sync)
wr_ptr_gray_sync2 <= wr_ptr_gray_sync1
```

**Files**: `async_fifo.sk`

### 4. Pattern Matching

**State Machine**:
```skalp
match state {
    GeometryState::Idle => { ... }
    GeometryState::LoadVertex => { ... }
    GeometryState::Transform => { ... }
    _ => { /* Default */ }
}
```

**Files**: `geometry_processor.sk`, `main.sk`

### 5. Hierarchical Instantiation

**Module Composition**:
```skalp
let vertex_fifo = AsyncFifo<Vertex, 16> { ... }
let geometry = GeometryProcessor4 { ... }
let transform_fifo = AsyncFifo<TransformedVertex, 64> { ... }
```

**Files**: `main.sk`

### 6. Structured Data Types

**Complex Structures**:
```skalp
pub struct Vertex {
    position: Vec3,
    normal: Vec3,
    texcoord: Vec2,
    color: Color
}
```

**Files**: `types.sk`

### 7. Enumeration Types

**Type-Safe Commands**:
```skalp
pub enum CommandOpcode {
    DrawTriangle = 0,
    SetTransform = 1,
    ClearBuffer = 3
}
```

**Files**: `types.sk`

### 8. Module System

**Imports and Exports**:
```skalp
// In main.sk
mod types;
mod async_fifo;
use types::{Vertex, Color};
use async_fifo::AsyncFifo;
```

**Files**: All modules

## 📊 Complexity Metrics

### Lines of Code

| File | Lines | Purpose |
|------|-------|---------|
| types.sk | 200 | Data structures |
| async_fifo.sk | 250 | Generic CDC FIFO |
| geometry_processor.sk | 280 | Pipelined processor |
| main.sk | 850 | System integration |
| **Total** | **1580** | Complete system |

### Module Instances

| Module | Instances | Parameters |
|--------|-----------|------------|
| AsyncFifo | 2 | Different types & depths |
| GeometryProcessor | 1 | 4 stages |
| Total modules | 3 | All parameterized |

### Clock Domains

| Domain | Frequency | Purpose |
|--------|-----------|---------|
| sys_clk | 100 MHz | Control, AXI interface |
| geom_clk | 200 MHz | Vertex processing |
| pixel_clk | 25 MHz | Display output |

### Signal Types

| Type | Count | Examples |
|------|-------|----------|
| Structs | 12 | Vertex, Vec3, Color |
| Enums | 3 | CommandOpcode, State |
| Clock signals | 3 | sys_clk, geom_clk, pixel_clk |
| Reset signals | 3 | One per domain |

## 🚀 Usage Patterns

### Pattern 1: Generic Module

**Goal**: Write reusable hardware

**Example**: AsyncFifo
```skalp
// Define generic
entity AsyncFifo<T, const DEPTH: nat> { ... }

// Use with different types
AsyncFifo<Vertex, 16>
AsyncFifo<TransformedVertex, 64>
AsyncFifo<bit[32], 8>
```

### Pattern 2: Configurable Pipeline

**Goal**: Trade latency vs throughput

**Example**: GeometryProcessor
```skalp
// Low latency
GeometryProcessor<2> { ... }  // 2 stages

// Balanced
GeometryProcessor<4> { ... }  // 4 stages (default)

// High throughput
GeometryProcessor<8> { ... }  // 8 stages
```

### Pattern 3: Multi-Clock System

**Goal**: Different parts run at different speeds

**Example**: Graphics Pipeline
```skalp
// Slow control interface
on(sys_clk.rise) { /* 100 MHz */ }

// Fast computation
on(geom_clk.rise) { /* 200 MHz */ }

// Display timing
on(pixel_clk.rise) { /* 25 MHz */ }
```

### Pattern 4: Type-Safe Commands

**Goal**: Avoid magic numbers

**Example**: Command decoder
```skalp
// Bad
if command == 0 { ... }
if command == 1 { ... }

// Good
match command {
    CommandOpcode::DrawTriangle => { ... }
    CommandOpcode::SetTransform => { ... }
}
```

## 📈 Learning Path

### Level 1: Beginner
**Goal**: Understand basic structure

1. Read [README.md](README.md) - Get overview
2. Look at [types.sk](src/types.sk) - See data structures
3. Run: `skalp build -s src/types.sk`

**Time**: 30 minutes

### Level 2: Intermediate
**Goal**: Understand single module

1. Read TUTORIAL.md sections 1-3
2. Study [async_fifo.sk](src/async_fifo.sk)
3. Understand Gray code CDC
4. Run: `skalp build -s src/async_fifo.sk`

**Time**: 2 hours

### Level 3: Advanced
**Goal**: Understand pipelined processing

1. Read TUTORIAL.md sections 4-5
2. Study [geometry_processor.sk](src/geometry_processor.sk)
3. Trace data flow through pipeline
4. Modify STAGES parameter

**Time**: 3 hours

### Level 4: Expert
**Goal**: Understand full system

1. Read complete TUTORIAL.md
2. Study [main.sk](src/main.sk)
3. Trace signals across clock domains
4. Build and inspect generated SystemVerilog
5. Modify and extend

**Time**: 4-6 hours

## 🔧 Modification Guide

### Easy Modifications

1. **Change FIFO Depths**
   - File: `main.sk`
   - Change: `AsyncFifo<Vertex, 16>` → `AsyncFifo<Vertex, 32>`
   - Impact: More buffering, higher area

2. **Change Pipeline Stages**
   - File: `main.sk`
   - Change: `GeometryProcessor4` → `GeometryProcessor8`
   - Impact: Higher throughput, more latency

3. **Add Status Registers**
   - File: `main.sk`
   - Add new case in AXI read decoder
   - Impact: More visibility

### Medium Modifications

1. **Add New Vertex Attributes**
   - File: `types.sk`
   - Add field to `Vertex` struct
   - Update `geometry_processor.sk` to handle it

2. **Add Pipeline Stage**
   - File: `geometry_processor.sk`
   - Add new stage in pipeline logic
   - Update stage counter

3. **Change Clock Frequencies**
   - File: `main.sk` (comments)
   - Update constraints
   - Verify timing

### Hard Modifications

1. **Add Texture Sampling**
   - New module: `texture_sampler.sk`
   - Add memory interface
   - Integrate into fragment shader

2. **Implement Z-Buffer**
   - New module: `depth_buffer.sk`
   - Add depth comparison
   - Handle read-modify-write

3. **Add Multiple Geometry Engines**
   - Duplicate processor instances
   - Add arbitration logic
   - Balance load

## 🐛 Common Issues

### Build Errors

**Error**: "Cannot find module 'types'"
**Fix**: Run from project root: `cd examples/complex_project`

**Error**: "Type mismatch in AsyncFifo"
**Fix**: Check type consistency in instantiation

### Understanding Issues

**Question**: "Why Gray code?"
**Answer**: See TUTORIAL.md section on CDC - prevents metastability

**Question**: "How does pipeline work?"
**Answer**: See TUTORIAL.md - data advances each clock cycle

### Performance Issues

**Issue**: "Design doesn't meet timing"
**Fix**: Add more pipeline stages, reduce clock frequency

**Issue**: "FIFOs overflow"
**Fix**: Increase DEPTH parameter or add flow control

## 📖 Related Documentation

- [SKALP Language Reference](../../docs/user/reference/syntax.md)
- [Parametric Types Guide](../../docs/PARAMETRIC_TYPES_GUIDE.md)
- [Clock Domain Crossing](../../docs/CDC.md)
- [Module System](../../docs/LIBRARY_SYSTEM.md)
- [Quick Start](../../docs/user/quick-start.md)

## ✅ Checklist

After going through this example, you should understand:

- [ ] How to organize multi-file projects
- [ ] How to write generic/parametric modules
- [ ] How to handle multiple clock domains safely
- [ ] How to use pattern matching effectively
- [ ] How to instantiate and connect modules hierarchically
- [ ] How to define and use complex data types
- [ ] How to write pipelined processing logic
- [ ] How to integrate modules into a complete system

## 🎓 What You've Learned

By studying this example, you've seen:

1. **Software Engineering** in hardware
   - Module organization
   - Type safety
   - Code reuse through generics

2. **Hardware Design Patterns**
   - Pipelined processing
   - Clock domain crossing
   - Hierarchical composition

3. **SKALP Language Features**
   - All major syntax elements
   - Parametric types
   - Pattern matching
   - Module system

4. **Real-World Considerations**
   - Timing closure
   - Area optimization
   - Interface protocols

## 🚀 Next Steps

1. **Experiment**: Modify parameters and rebuild
2. **Extend**: Add new features (texture, z-buffer)
3. **Optimize**: Improve timing, reduce area
4. **Apply**: Use patterns in your own designs

---

**Total Project Size**: ~1600 lines of SKALP → ~3500 lines of SystemVerilog

**Demonstrates**: All advanced SKALP features in a realistic, production-quality design

**Time Investment**: 30 min to get started, 8-12 hours to master

**Ready to dive in? Start with [README.md](README.md)!**
