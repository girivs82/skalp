# Phase 7: Verification Features

**Goal:** Build comprehensive verification capabilities including assertions, properties, and formal verification

**Duration:** 4 weeks (Weeks 25-28)

**Success Test:** Verify a FIFO design with assertions and achieve 100% coverage

---

## 🎯 TASKS

**Core Work:**
- [ ] Implement immediate assertions
  - [ ] Assert statements in HIR/MIR
  - [ ] Synthesis to SystemVerilog assertions
  - [ ] Runtime checking in simulation

- [ ] Implement concurrent assertions
  - [ ] Property definitions
  - [ ] Sequence operators
  - [ ] Temporal logic support
  - [ ] Clock-based evaluation

- [ ] Add requirement declarations
  - [ ] Requirement syntax in language
  - [ ] Traceability to implementation
  - [ ] Coverage tracking
  - [ ] Requirement verification status

- [ ] Build coverage infrastructure
  - [ ] Statement coverage
  - [ ] Branch coverage
  - [ ] Condition coverage
  - [ ] FSM state coverage
  - [ ] Toggle coverage
  - [ ] Cross coverage

- [ ] Testbench async/await support
  - [ ] Async task definitions
  - [ ] Await on conditions
  - [ ] Fork/join semantics
  - [ ] Timeout handling

- [ ] Formal verification integration
  - [ ] Property extraction to SMT-LIB
  - [ ] Bounded model checking
  - [ ] Integration with Z3/Yices
  - [ ] Counterexample generation

**Testing:**
- [ ] Test assertion generation and checking
- [ ] Test coverage collection accuracy
- [ ] Test async/await in testbenches
- [ ] Verify formal property extraction
- [ ] Test requirement traceability

**Documentation:**
- [ ] Document assertion syntax and semantics
- [ ] Create verification methodology guide
- [ ] Document coverage metrics
- [ ] Add formal verification tutorial

---

## ✅ COMPLETION CRITERIA

**This phase is done when:**
- [ ] Can write immediate and concurrent assertions in SKALP
- [ ] Assertions generate correct SystemVerilog/SVA
- [ ] Coverage metrics are collected and reported
- [ ] Async/await works in testbenches
- [ ] Can extract properties for formal verification
- [ ] FIFO design verified with >90% coverage

**Success Test:**
1. Write a FIFO design with assertions for:
   - No overflow
   - No underflow
   - Data integrity
   - Correct pointer behavior
2. Run simulation and collect coverage
3. Achieve >90% code coverage
4. Extract properties and verify formally
5. Generate verification report

---

## 📈 PROGRESS

**Daily Log:**
```
[Dec 28, 2024] - Phase 7 started - Planning verification architecture
```

**Blockers:**
- [ ] Need to decide on formal verification backend (Z3, Yices, or custom)
- [ ] Consider SystemVerilog Assertions (SVA) compatibility

---

## 🔧 TECHNICAL DECISIONS

**Assertion Philosophy:**
- Support both immediate (procedural) and concurrent (temporal) assertions
- Generate SystemVerilog Assertions (SVA) for compatibility
- Provide simulation-time and formal verification paths
- Make assertions synthesizable where possible

**Coverage Strategy:**
- Instrument code during MIR generation
- Collect coverage in simulation
- Support industry-standard metrics
- Generate UCDB or similar format for tools

**Formal Verification Approach:**
- Extract properties from assertions
- Generate SMT-LIB format
- Support bounded model checking initially
- Consider full proof capabilities later

---

## 📋 IMPLEMENTATION NOTES

**Assertion Syntax Examples:**
```skalp
// Immediate assertion
assert(fifo_count <= MAX_DEPTH, "FIFO overflow");

// Concurrent assertion
property no_overflow {
    @(posedge clk) push && full |-> !push_en
}

// Sequence definition
sequence req_then_ack {
    req ##1 ack ##[1:3] !req
}

// Coverage point
cover property (@(posedge clk) state == IDLE ##1 state == ACTIVE);
```

**Requirement Example:**
```skalp
requirement SafetyReq1 {
    description: "FIFO shall not overflow"
    property: no_overflow
    severity: critical
    trace_to: ["FIFO.push", "FIFO.full"]
}
```

---

**When done, run `/complete-phase` again for Phase 8: Safety Features**