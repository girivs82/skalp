# Phase 4: GPU Simulation

**Goal:** Build GPU-native simulation from the start - no throwaway CPU simulation code

**Duration:** 4 weeks

**Success Test:** Simulate a counter on GPU faster than CPU simulation

---

## 🎯 TASKS

**Core Work:**
- [ ] **SIR (Simulation IR) generation from MIR**
  - Design SIR structure optimized for GPU execution
  - Transform MIR modules into simulation-ready representation
  - Handle combinational and sequential logic separately

- [ ] **Combinational cone extraction**
  - Identify independent combinational logic blocks
  - Extract dependency graphs for parallel execution
  - Optimize for GPU workgroup structure

- [ ] **Metal shader generation**
  - Generate Metal compute shaders from SIR
  - Implement logic operations in GPU-native code
  - Handle bit-level operations efficiently

- [ ] **CPU-GPU async runtime with Tokio**
  - Build async interface between CPU control and GPU simulation
  - Implement efficient data transfer patterns
  - Handle simulation state management

- [ ] **Basic testbench interface**
  - Create simple testbench API for driving GPU simulation
  - Implement signal monitoring and waveform capture
  - Add basic assertion checking

**Testing:**
- [ ] Test SIR generation for counter, adder, FIFO designs
- [ ] Validate Metal shader correctness vs reference simulation
- [ ] Benchmark GPU vs CPU performance on increasing design sizes
- [ ] Test async runtime stability under load

**Documentation:**
- [ ] Document SIR format and generation process
- [ ] Add GPU simulation architecture documentation
- [ ] Create user guide for testbench interface

---

## ✅ COMPLETION CRITERIA

**This phase is done when:**
- [ ] Can simulate a simple counter design entirely on GPU
- [ ] GPU simulation is measurably faster than equivalent CPU simulation
- [ ] Testbench can drive signals and capture results
- [ ] All GPU simulation tests pass with correct results

**Success Test:** Run a 32-bit counter for 10,000 cycles on GPU and demonstrate 2x+ speedup vs CPU

---

## 📈 PROGRESS

**Daily Log:**
```
[Date] - [What got done] - [Any blockers]
```

**Blockers:**
- [ ] None currently identified

---

## 🧪 TECHNICAL DETAILS

**SIR Design Considerations:**
- Separate combinational and sequential logic for optimal GPU scheduling
- Minimize CPU-GPU data transfers during simulation
- Support bit-accurate simulation while leveraging GPU parallelism

**Metal Shader Strategy:**
- Use compute shaders for parallel logic evaluation
- Implement custom bit manipulation functions
- Optimize memory access patterns for GPU architecture

**Performance Targets:**
- 2x speedup vs CPU for moderate-sized designs (1K-10K gates)
- 10x+ speedup for large designs (100K+ gates) due to parallelism
- Sub-millisecond latency for single-cycle evaluation

---

**When done, run `/complete-phase` again for Phase 5: Advanced Features**