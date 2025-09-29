# Phase 2: Core Language

**Goal:** Implement core SKALP language features including entity hierarchies, sequential/combinational logic, and SystemVerilog code generation

**Duration:** 4 weeks (Weeks 5-8)

**Success Test:** Compile a counter entity to SystemVerilog that simulates correctly

---

## 🎯 TASKS

### Week 1: MIR Generation
**Core Work:**
- [x] Design MIR (Mid-level IR) data structures - DONE (already exists)
- [x] Implement HIR to MIR transformation - DONE (working, generates modules)
- [ ] Fix MIR to LIR port-to-signal conversion (HIGH PRIORITY)
- [ ] Handle entity instantiation in MIR
- [ ] Support port mapping and connections
- [ ] Create MIR validation and debugging

**Testing:**
- [ ] Test HIR to MIR conversion for basic entities
- [ ] Verify port connections are preserved
- [ ] Test hierarchical entity structures

### Week 2: Sequential Logic Support
**Core Work:**
- [ ] Implement `on(clock.rise)` event blocks in MIR
- [ ] Handle non-blocking assignments (`<=`)
- [ ] Support reset conditions (`on(reset.active)`)
- [ ] Implement signal vs variable semantics
- [ ] Handle edge detection (rise/fall/change)

**Testing:**
- [ ] Test counter with clock events
- [ ] Test flip-flop implementations
- [ ] Verify reset behavior
- [ ] Test multiple event blocks

### Week 3: Combinational Logic
**Core Work:**
- [ ] Implement continuous assignments (`=`)
- [ ] Support combinational expressions
- [ ] Handle blocking assignments (`:=`) in procedures
- [ ] Implement if/else statements
- [ ] Support case/match statements

**Testing:**
- [ ] Test pure combinational circuits
- [ ] Test mixed sequential/combinational
- [ ] Verify assignment ordering
- [ ] Test conditional logic

### Week 4: SystemVerilog Codegen
**Core Work:**
- [ ] Design codegen architecture
- [ ] Implement MIR to SystemVerilog translation
- [ ] Generate module declarations
- [ ] Handle always blocks (always_ff, always_comb)
- [ ] Generate proper sensitivity lists
- [ ] Support hierarchical instantiation

**Testing:**
- [ ] Generate and simulate counter
- [ ] Test with Verilator or iverilog
- [ ] Verify timing behavior
- [ ] Test hierarchical designs
- [ ] Compare with hand-written Verilog

**Documentation:**
- [ ] Document MIR structure and design decisions
- [ ] Create codegen architecture docs
- [ ] Write user guide for supported features
- [ ] Document limitations and future work

---

## ✅ COMPLETION CRITERIA

**This phase is done when:**
- [ ] MIR properly represents all HIR constructs
- [ ] Sequential logic with clock events works
- [ ] Combinational logic generates correctly
- [ ] SystemVerilog output compiles without errors
- [ ] Generated code simulates correctly
- [ ] Counter example works end-to-end
- [ ] Tests prove correctness

**Success Test:**
```skalp
entity Counter {
    in clk: clock
    in reset: reset
    out count: nat[8]
}

impl Counter {
    signal counter: nat[8] = 0

    on(clk.rise) {
        if (reset) {
            counter <= 0
        } else {
            counter <= counter + 1
        }
    }

    count = counter
}
```
This should generate valid SystemVerilog that simulates correctly and counts from 0 to 255.

---

## 📈 PROGRESS

**Daily Log:**
```
[Dec 29, 2024] - Phase 1 completed. Started Phase 2:
  - Found that HIR→MIR is partially working (generates 1 module with 3 ports)
  - Identified issue: MIR→LIR not converting ports to signals properly
  - SystemVerilog generation produces minimal output due to missing signals
  - Next: Fix MIR→LIR port conversion
```

**Week 1 Goals:**
- MIR design and basic transformation
- Entity hierarchy support

**Week 2 Goals:**
- Sequential logic and clock events
- Non-blocking assignments

**Week 3 Goals:**
- Combinational logic
- Control flow statements

**Week 4 Goals:**
- SystemVerilog generation
- End-to-end testing

**Blockers:**
- [ ] _Any current issues blocking progress_

---

## 🔧 TECHNICAL NOTES

### MIR Design Considerations
- Should be lower-level than HIR but still hardware-agnostic
- Explicit clock domains and reset signals
- Separate sequential and combinational processes
- Ready for optimization passes

### Codegen Strategy
- Generate human-readable SystemVerilog
- Preserve comments and structure where possible
- Use modern SystemVerilog features (always_ff, always_comb)
- Support both synthesis and simulation

### Testing Infrastructure
- Set up Verilator or iverilog for automated testing
- Create test harness for generated code
- Compare outputs with expected results
- Performance benchmarks vs hand-written code

---

## 🎯 KEY DELIVERABLES

1. **MIR Module:** `crates/skalp-mir/`
   - Data structures
   - HIR to MIR transformer
   - Validation and analysis

2. **Codegen Module:** `crates/skalp-codegen/`
   - SystemVerilog backend
   - Code formatting
   - Comment preservation

3. **Test Suite:**
   - Unit tests for each component
   - Integration tests with real designs
   - End-to-end simulation tests

4. **Examples:**
   - Counter
   - Flip-flop
   - Simple ALU
   - FIFO (stretch goal)

---

**When done, run `/complete-phase` again to move to Phase 3: Clock Domains & Types**