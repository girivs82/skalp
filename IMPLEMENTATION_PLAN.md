# SKALP Implementation Plan

## Overview

This document outlines the implementation plan for SKALP, progressing from foundational components to advanced features. The plan is organized in phases to enable early testing and validation.

## Phase 1: Foundation (Weeks 1-4)

### 1.1 Project Setup
- [ ] Initialize Rust workspace with cargo
- [ ] Set up crate structure:
  - `skalp-frontend` - Lexer, parser, AST, HIR
  - `skalp-mir` - Mid-level IR and optimization
  - `skalp-lir` - Low-level IR and netlist
  - `skalp-codegen` - Code generation (SystemVerilog initially)
  - `skalp-sim` - Simulation engine (start with basic, GPU later)
  - `skalp` - Main CLI tool
- [ ] Set up CI/CD with GitHub Actions
- [ ] Configure testing framework

### 1.2 Lexer & Parser
- [ ] Implement lexer for SKALP tokens
- [ ] Design AST structure
- [ ] Implement parser using `nom` or `pest`
- [ ] Parse basic entities and implementations
- [ ] Parse type declarations
- [ ] Error reporting with spans

### 1.3 Basic Type System
- [ ] Primitive types: `bit`, `logic`, `int`, `nat`
- [ ] Arrays and vectors
- [ ] Type inference for literals
- [ ] Basic type checking

### 1.4 HIR Generation
- [ ] AST to HIR transformation
- [ ] Name resolution
- [ ] Basic semantic analysis
- [ ] Symbol table construction

## Phase 2: Core Language (Weeks 5-8)

### 2.1 Entities and Hierarchies
- [ ] Entity declaration parsing
- [ ] Port declarations (in/out/inout)
- [ ] Entity instantiation
- [ ] Port mapping (positional and named)
- [ ] Hierarchy building

### 2.2 Sequential Logic
- [ ] `on(clock.rise)` blocks
- [ ] Signal vs variable semantics
- [ ] Non-blocking assignments (`<=`)
- [ ] Immediate assignments (`:=`)
- [ ] OR notation for events (`clock.rise | reset.rise`)

### 2.3 Combinational Logic
- [ ] Continuous assignments (`=`)
- [ ] Expression evaluation
- [ ] Operator precedence
- [ ] Bit slicing and concatenation

### 2.4 Basic Code Generation
- [ ] HIR to SystemVerilog
- [ ] Entity to module mapping
- [ ] Signal declarations
- [ ] Always blocks generation
- [ ] Simple testbench generation

## Phase 3: Clock Domains & Types (Weeks 9-12)

### 3.1 Clock Domain System
- [ ] Clock domain as lifetime syntax (`<'clk>`)
- [ ] Clock domain inference
- [ ] CDC detection at compile time
- [ ] Synchronizer inference
- [ ] Multiple clock domain support

### 3.2 Advanced Type System
- [ ] Generics and parameters
- [ ] Struct types (packed/unpacked)
- [ ] Enum types
- [ ] Union types
- [ ] Fixed-point types

### 3.3 Protocols
- [ ] Protocol definition parsing
- [ ] Master/slave endpoints
- [ ] Direction inference (`->` and `<-`)
- [ ] Protocol composition (extends)
- [ ] Valid/Ready protocol implementation

### 3.4 MIR Generation
- [ ] HIR to MIR lowering
- [ ] Clock domain resolution
- [ ] Protocol expansion
- [ ] Dead code elimination
- [ ] Basic optimizations

## Phase 5: Advanced Features (Weeks 17-20)

### 5.1 Pattern Matching
- [ ] Match expressions
- [ ] Structural pattern matching
- [ ] Guards and conditions
- [ ] Exhaustiveness checking

### 5.2 Flow Blocks
- [ ] Dataflow syntax (`|>` operator)
- [ ] Pipeline inference
- [ ] Automatic buffering
- [ ] Fork/join operations

### 5.3 Traits and Generics
- [ ] Trait definitions
- [ ] Trait implementations
- [ ] Generic entities
- [ ] Trait bounds
- [ ] Type inference with traits

### 5.4 Intent System
- [ ] Intent parsing
- [ ] Intent propagation through hierarchy
- [ ] Intent-guided optimization
- [ ] Intent verification

## Phase 6: Synthesis & Optimization (Weeks 21-24)

### 5.1 LIR Generation
- [ ] MIR to LIR lowering
- [ ] Netlist generation
- [ ] Technology mapping
- [ ] Gate-level representation

### 5.2 Optimization Passes
- [ ] Constant folding
- [ ] Dead code elimination
- [ ] Common subexpression elimination
- [ ] Resource sharing
- [ ] Retiming
- [ ] Pipeline balancing

### 5.3 Timing Analysis
- [ ] Critical path analysis
- [ ] Setup/hold time checking
- [ ] Clock domain crossing analysis
- [ ] Timing constraint propagation

### 5.4 Area Optimization
- [ ] Resource estimation
- [ ] Sharing analysis
- [ ] Multiplexer optimization
- [ ] State encoding optimization

## Phase 6: Verification Features (Weeks 21-24)

### 6.1 Assertions
- [ ] Immediate assertions
- [ ] Concurrent assertions
- [ ] Property definitions
- [ ] Sequence definitions
- [ ] Coverage points

### 6.2 Formal Verification
- [ ] Property extraction
- [ ] SMT-LIB generation
- [ ] Bounded model checking
- [ ] Invariant checking
- [ ] Integration with external solvers

### 6.3 Requirements Traceability
- [ ] Requirement declarations
- [ ] Entity satisfaction tracking
- [ ] Hierarchical requirement flow
- [ ] Coverage analysis
- [ ] Traceability matrix generation

### 6.4 Testbench Support
- [ ] Async/await for testbenches
- [ ] Clock generation utilities
- [ ] Stimulus generation
- [ ] Response checking
- [ ] Coverage collection

## Phase 7: Safety Features (Weeks 25-28)

### 7.1 Functional Safety
- [ ] Safety requirement declarations
- [ ] PSM/LSM mechanism support
- [ ] Safety mechanism verification
- [ ] Diagnostic coverage calculation
- [ ] ISO 26262 metrics (SPFM, LFM, PMHF)

### 7.2 FMEA Generation
- [ ] Intent-based hazard analysis
- [ ] Failure mode extraction
- [ ] Gate-to-intent tracing
- [ ] Cone of influence analysis
- [ ] RPN calculation
- [ ] Detection point generation

### 7.3 Power Management
- [ ] Power domain declarations
- [ ] Clock gating inference
- [ ] Power state machines
- [ ] Retention strategy
- [ ] Isolation cell insertion
- [ ] Level shifter insertion

## Phase 4: GPU-Native Simulation (Weeks 13-16) - MOVED UP!

**Critical Design Decision**: We build GPU-native from the start. No throwaway CPU simulation code.

### Why GPU-First is Non-Negotiable:
- Our entire architecture assumes GPU parallelism (combinational cone extraction)
- CPU simulation would require event queues (which we explicitly don't want)
- The data structures for GPU are fundamentally different (SIR vs traditional event-driven)
- Building CPU first means 100% wasted code - nothing transfers
- GPU simulation is our key differentiator - must validate early

### 4.1 SIR Generation
- [ ] MIR to SIR transformation
- [ ] Combinational cone extraction algorithm
- [ ] Cone dependency analysis
- [ ] Cone packing for GPU dispatch
- [ ] Memory layout design for GPU

### 4.2 Metal Foundation
- [ ] Metal device initialization
- [ ] Command queue setup
- [ ] Buffer management system
- [ ] Compute pipeline state setup
- [ ] Basic shader template

### 4.3 Shader Generation
- [ ] Combinational logic to Metal shader
- [ ] State update kernels
- [ ] Memory copy kernels
- [ ] Synchronization barriers
- [ ] Shader caching system

### 4.4 CPU-GPU Runtime
- [ ] Tokio async runtime (CPU side)
- [ ] Metal async runtime (GPU side)
- [ ] Input/output queues
- [ ] Cycle-accurate synchronization
- [ ] Testbench interface

## Phase 9: Advanced Backends (Weeks 33-36)

### 9.1 FPGA Support
- [ ] iCE40 backend
- [ ] Lattice ECP5 backend
- [ ] Xilinx 7-series support
- [ ] Bitstream generation
- [ ] Place & route integration

### 9.2 ASIC Support
- [ ] Standard cell mapping
- [ ] Liberty file parsing
- [ ] LEF/DEF generation
- [ ] Timing constraint generation
- [ ] Power analysis

### 9.3 Photonic Support (Future)
- [ ] PIR generation
- [ ] Waveguide routing
- [ ] Phase management
- [ ] Optical simulation

## Phase 10: Tooling & Polish (Weeks 37-40)

### 10.1 Language Server (LSP)
- [ ] Incremental parsing
- [ ] Semantic highlighting
- [ ] Go-to definition
- [ ] Hover information
- [ ] Refactoring support
- [ ] Error diagnostics

### 10.2 Documentation
- [ ] User guide
- [ ] Language reference
- [ ] Tutorial series
- [ ] Example designs
- [ ] API documentation

### 10.3 Standard Library
- [ ] Common protocols (AXI, AHB, etc.)
- [ ] FIFO implementations
- [ ] Arithmetic units
- [ ] Memory controllers
- [ ] Clock utilities

### 10.4 Testing & Quality
- [ ] Comprehensive test suite
- [ ] Regression testing
- [ ] Performance benchmarks
- [ ] Fuzzing
- [ ] Integration tests

## Development Priorities

### Critical Path (Must Have for MVP):
1. Basic parser and type system (Phase 1)
2. Entities and basic logic (Phase 2)
3. SystemVerilog codegen (Phase 2.4)
4. Clock domains (Phase 3.1)
5. **GPU-native simulation (Phase 4)** - Build right from the start!

### High Priority (Version 1.0):
1. Protocols (Phase 3.3)
2. Intent system (Phase 4.4)
3. Optimization passes (Phase 5.2)
4. Assertions (Phase 6.1)
5. Requirements (Phase 6.3)

### Medium Priority (Version 1.5):
1. Safety features (Phase 7)
2. FPGA support (Phase 9.1)
3. LSP (Phase 10.1)
4. Performance optimizations

### Future (Version 2.0+):
1. Photonic support
2. Advanced formal verification
3. Machine learning optimizations
4. Cloud compilation service

## Resource Allocation

### Team Structure (Ideal):
- **Core Compiler**: 2-3 developers
- **Simulation**: 1-2 developers
- **Backends**: 1-2 developers
- **Safety/Verification**: 1 developer
- **Tooling/LSP**: 1 developer

### Solo Development Strategy:
If developing solo, focus on:
1. Vertical slice first (parser → codegen for simple designs)
2. Iterate on features (add one feature through all layers)
3. Prioritize user-visible functionality
4. Defer optimizations until core works

## Testing Strategy

### Unit Tests:
- Each module has comprehensive unit tests
- Property-based testing for parser
- Snapshot testing for codegen

### Integration Tests:
- End-to-end compilation tests
- Simulation vs reference comparison
- Multi-file projects
- Real-world designs

### Validation:
- Compile known designs (RISC-V cores, etc.)
- Compare with hand-written HDL
- Formal equivalence checking
- Silicon validation (long-term)

## Risk Mitigation

### Technical Risks:
1. **Parser Complexity**: Use parser generator initially, hand-write later
2. **Type System Complexity**: Start simple, add features incrementally
3. **GPU Simulation**: Fall back to CPU simulation initially
4. **CDC Analysis**: Leverage existing algorithms, cite papers

### Schedule Risks:
1. **Scope Creep**: Strict MVP definition, defer features
2. **Integration Issues**: Continuous integration from day 1
3. **Performance**: Profile early, optimize later
4. **Compatibility**: Target SystemVerilog-2017 subset first

## Success Metrics

### Phase 1-2 Success:
- Can parse and generate SystemVerilog for simple counters
- Basic type checking works
- Hierarchical designs compile

### Phase 3-4 Success:
- Clock domain safety demonstrated
- Protocols make design cleaner
- Intent guides optimization

### Phase 5-6 Success:
- Optimization improves area/timing by 20%+
- Requirements fully traceable
- Assertions work in simulation

### Phase 7-8 Success:
- FMEA generation accurate
- GPU simulation 10x faster than Verilator
- Safety metrics calculated correctly

### Overall Success:
- Real designs compile and work
- Users prefer SKALP over Verilog/VHDL
- Community adoption begins
- Silicon tape-out successful

## Implementation Notes

### Technology Choices:
- **Language**: Rust (for performance and safety)
- **Parser**: Start with `pest`, migrate to hand-written
- **GPU**: Metal for macOS, Vulkan for cross-platform
- **Serialization**: `serde` for IR persistence
- **CLI**: `clap` for argument parsing
- **Testing**: `cargo test` + `proptest`

### Code Organization:
- Monorepo with workspace
- Clear module boundaries
- Minimal dependencies
- Extensive documentation
- Clean error handling

### Development Process:
- Test-driven development
- Continuous integration
- Code review (if team)
- Weekly progress tracking
- User feedback integration

## Next Steps

1. **Review and refine this plan**
2. **Set up development environment**
3. **Create project skeleton**
4. **Implement Phase 1.1 (Project Setup)**
5. **Begin Phase 1.2 (Lexer)**

## Questions to Resolve

1. Should we support Verilog import for gradual migration?
2. Should we target LLVM-IR for software simulation?
3. Should we implement our own place & route or use existing tools?
4. Should we support Python bindings for scripting?
5. How much SystemVerilog compatibility do we need?

---

*This plan is a living document and will be updated as development progresses.*