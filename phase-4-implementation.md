# Phase 4: GPU Simulation

**Goal:** GPU-native simulation from the start - achieve faster simulation than CPU for parallel designs

**Duration:** 4 weeks (Weeks 13-16)

**Success Test:** Simulate a counter on GPU faster than CPU

---

## 🎯 TASKS

**Core Work:**
- [ ] Design SIR (Simulation IR) structure for GPU execution
- [ ] Implement SIR generation from MIR
- [ ] Extract combinational cones for parallel evaluation
- [ ] Generate Metal compute shaders from SIR
- [ ] Build CPU-GPU async runtime with Tokio
- [ ] Create basic testbench interface
- [ ] Implement state synchronization between CPU and GPU
- [ ] Add waveform capture support

**Testing:**
- [ ] Test simple combinational circuits
- [ ] Test sequential circuits (counters, state machines)
- [ ] Test hierarchical designs
- [ ] Benchmark GPU vs CPU performance
- [ ] Test memory bandwidth optimization

**Documentation:**
- [ ] Document SIR format and generation
- [ ] Document Metal shader architecture
- [ ] Create GPU simulation user guide

---

## ✅ COMPLETION CRITERIA

**This phase is done when:**
- [ ] Can generate SIR from any MIR design
- [ ] Metal shaders compile and execute correctly
- [ ] GPU simulation produces same results as SystemVerilog simulation
- [ ] Performance benchmarks show GPU advantage for parallel designs
- [ ] Testbench interface allows easy testing

**Success Test:** Counter simulation on GPU runs 2x faster than CPU simulation for designs with >1000 parallel elements

---

## 📈 PROGRESS

**Daily Log:**
```
[Sep 30, 2024] - Phase 4 implementation file created - Ready to begin GPU simulation work
```

**Blockers:**
- [ ] Need to understand Metal compute shader limitations
- [ ] Need to design efficient state transfer mechanism

---

## 🔧 TECHNICAL DETAILS

### SIR Structure
- Represents circuit as data flow graph
- Separates combinational and sequential logic
- Optimized for GPU parallel execution

### Metal Shader Architecture
- Combinational logic as compute kernels
- State storage in GPU buffers
- Event-driven sequential updates

### CPU-GPU Runtime
- Tokio async for CPU coordination
- Metal command buffers for GPU work
- Zero-copy state transfers where possible

---

**When done, run `/complete-phase` again for Phase 5: Advanced Features**