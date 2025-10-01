# Phase 5: Complex Conditional Logic Fix

**Goal:** Fix MIR to SIR conversion for complex if-else-if chains to enable GPU simulation of complex hardware designs

**Duration:** 2 weeks

**Success Test:** Pipelined processor test achieves 11/11 passing (currently 8/11)

---

## 🎯 TASKS

**Core Work:**
- [ ] Debug assignment value extraction in if-else-if chains
  - Add debug prints to `find_assignment_in_block()` and `convert_if_to_sequential_mux()`
  - Verify MIR representation correctly captures conditional structure
  - Identify why different branches return identical assignment values
- [ ] Fix recursive mux tree generation for nested conditionals
  - Correct `convert_if_to_sequential_mux()` function to handle else-if chains properly
  - Ensure each condition gets its own unique assignment value
  - Fix recursive descent through else-if structures
- [ ] Validate expression node creation for different operations
  - Verify `create_expression_node()` generates different nodes for different arithmetic operations
  - Check binary operations (add, sub, mul, xor) are created properly
  - Ensure comparisons (`decode_opcode == 1`, `decode_opcode == 2`, etc.) generate distinct condition nodes

**Testing:**
- [ ] Fix pipelined processor test (4-stage pipeline with ALU)
  - Target: All 11 test cases passing
  - Current status: 8/11 passing (failing on cycles where ALU operations should produce results)
  - Expected outputs: ADD(10+5)=15, SUB(20-3)=17, XOR(15^7)=8
- [ ] Create additional test cases with complex conditional logic
  - Complex ALU with 8+ operations
  - Multi-state FSM with complex transitions
  - Nested conditionals (if-else-if within if-else-if)
- [ ] Test edge cases and integration
  - Mixed simple and complex conditional structures
  - Multiple signals with different conditional assignments
  - Performance with complex designs on GPU

**Documentation:**
- [ ] Document the fix in conditional logic compilation
- [ ] Update technical notes on MIR to SIR conversion
- [ ] Add comments to fixed functions explaining the correct approach

---

## ✅ COMPLETION CRITERIA

**This phase is done when:**
- [ ] Pipelined processor test shows 11/11 passing tests
- [ ] Generated Metal shaders show proper nested mux structures with different values
- [ ] Multiple complex examples demonstrate robust conditional compilation
- [ ] GPU simulation performance maintains efficiency with complex designs

**Success Test:** Run `cargo test --test test_pipelined_processor test_pipelined_processor_gpu` and see all 11 tests pass

---

## 📈 PROGRESS

**Daily Log:**
```
[Date] - [What got done] - [Any blockers]
```

**Blockers:**
- [ ] Any current issues blocking progress

---

## 🔧 TECHNICAL DETAILS

### Problem Analysis
**File:** `/Users/girivs/src/hw/hls/crates/skalp-sir/src/mir_to_sir.rs` (lines 263-313)

**Current Issue:** Complex if-else-if chains generate incorrect Metal shader code:
```metal
signals->node_6_out = signals->node_0_out ? signals->node_5_out : signals->node_5_out;
```

**Root Cause:** The recursive `convert_if_to_sequential_mux()` function doesn't correctly extract different assignment values for each branch.

**Expected Output:** Proper nested mux structure:
```metal
decode_opcode == 1 ? (decode_operand + data_in) :
  (decode_opcode == 2 ? (decode_operand - data_in) :
    (decode_opcode == 3 ? (decode_operand * 2) :
      (decode_opcode == 4 ? (decode_operand ^ data_in) :
        decode_operand)))
```

### Key Functions to Fix
1. **`convert_if_in_sequential()`** - Main entry point for sequential if statements
2. **`convert_if_to_sequential_mux()`** - Recursive mux tree generation
3. **`find_else_value()`** - Handling of else-if chains
4. **`find_assignment_in_block()`** - Assignment extraction from blocks

### Test Case Reference
**HDL Input:**
```hdl
if (decode_opcode == 1) {
    execute_result <= decode_operand + data_in
} else if (decode_opcode == 2) {
    execute_result <= decode_operand - data_in
} else if (decode_opcode == 3) {
    execute_result <= decode_operand * 2
} else if (decode_opcode == 4) {
    execute_result <= decode_operand ^ data_in
} else {
    execute_result <= decode_operand
}
```

**Current Test Results:**
- Tests 9, 10, 11 failing (cycles 18, 20, 22)
- Expected results: 15, 17, 8
- Actual results: 0, 0, 0

---

## 🎯 SUCCESS METRICS

1. **Functional:** Pipelined processor test: 11/11 passing
2. **Correctness:** Metal shaders show distinct values in mux branches
3. **Performance:** Complex designs run efficiently on GPU
4. **Robustness:** Multiple complex examples work correctly

---

## 📊 CURRENT STATUS

**Architecture Status:** ✅ GPU simulation backend fully functional
- Counter test: 7/7 passing
- Three-buffer architecture working correctly
- Metal shader generation framework operational
- Async GPU runtime with proper synchronization

**Issue Scope:** ❌ Frontend compilation only
- GPU simulation architecture is not the problem
- Issue isolated to MIR → SIR conditional logic conversion
- All other GPU simulation features working correctly

---

**When done, run `/complete-phase` again for the next phase**