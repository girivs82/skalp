# Phase 5: Complex Conditional Logic Compilation Fix

## Objective

Fix the MIR to SIR conversion for complex conditional structures (if-else-if chains) to enable GPU simulation of complex hardware designs without simplification or workarounds.

## Pass Criteria

1. **Pipelined processor test must pass**: All 11 test cases in `test_pipelined_processor_gpu` must pass
2. **Complex ALU operations work**: Multi-condition if-else-if chains compile correctly
3. **Multiple complex examples**: Create and test additional complex designs with nested conditionals
4. **No simplification**: Fix the root cause without avoiding complex cases

## Problem Statement

Currently, complex if-else-if chains like this HDL code:

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

Are being compiled to incorrect Metal shader code like:
```metal
signals->node_6_out = signals->node_0_out ? signals->node_5_out : signals->node_5_out;
```

Instead of the correct nested mux structure.

## Root Cause Analysis

### Issue Location
- **File**: `/Users/girivs/src/hw/hls/crates/skalp-sir/src/mir_to_sir.rs`
- **Functions**: Lines 263-313
  - `convert_if_in_sequential()`
  - `convert_if_to_sequential_mux()`
  - `find_else_value()`
  - `find_assignment_in_block()`

### Technical Problem
The recursive if-else-if conversion is not correctly extracting the different assignment values for each branch. The function `find_assignment_in_block()` is returning the same value for different conditions, resulting in mux operations where both true and false branches have identical values.

### Expected Behavior
Should generate nested mux structure:
```
decode_opcode == 1 ? (decode_operand + data_in) :
  (decode_opcode == 2 ? (decode_operand - data_in) :
    (decode_opcode == 3 ? (decode_operand * 2) :
      (decode_opcode == 4 ? (decode_operand ^ data_in) :
        decode_operand)))
```

## Implementation Plan

### Step 1: Debug Assignment Value Extraction
- Add debug prints to `find_assignment_in_block()` to see what expressions are being found
- Verify that different branches of if-else-if chain contain different assignment expressions
- Check if the MIR representation correctly captures the conditional structure

### Step 2: Fix Recursive Mux Tree Generation
- Correct the `convert_if_to_sequential_mux()` function to properly handle nested if statements
- Ensure that each condition gets its own unique assignment value
- Fix the recursive descent through else-if chains

### Step 3: Validate Expression Node Creation
- Verify that `create_expression_node()` correctly generates different nodes for different arithmetic operations
- Ensure binary operations (add, sub, mul, xor) are being created properly
- Check that comparisons (`decode_opcode == 1`, `decode_opcode == 2`, etc.) generate distinct condition nodes

### Step 4: Test with Complex Examples
- Fix pipelined processor test (4-stage pipeline with ALU)
- Create additional test cases with complex conditional logic
- Test multi-level nested if-else-if structures
- Verify performance with complex designs

## Test Cases to Validate

### Primary Test Case
- **File**: `/Users/girivs/src/hw/hls/tests/test_pipelined_processor.rs`
- **Current Status**: 8/11 passing (failing on cycles where ALU operations should produce results)
- **Expected**: All 11 test cases passing

### Additional Test Cases to Create
1. **Complex ALU**: Standalone ALU with 8+ operations
2. **State Machine**: Multi-state FSM with complex transitions
3. **Nested Conditionals**: If-else-if chains within if-else-if chains
4. **Mixed Logic**: Combination of simple and complex conditional structures

## Success Metrics

1. **Functional**: Pipelined processor test shows 11/11 passing
2. **Correctness**: Generated Metal shaders show proper nested mux structures with different values
3. **Performance**: Complex designs run efficiently on GPU without degradation
4. **Completeness**: Multiple complex examples demonstrate robust conditional compilation

## Files to Modify

### Primary Focus
- `/Users/girivs/src/hw/hls/crates/skalp-sir/src/mir_to_sir.rs` (lines 263-313)

### Secondary (if needed)
- `/Users/girivs/src/hw/hls/crates/skalp-mir/` (if MIR representation issues found)
- `/Users/girivs/src/hw/hls/tests/` (additional test cases)

## Dependencies

- Phase 4 GPU simulation architecture (✅ completed and working)
- Basic MIR to SIR conversion (✅ working for simple cases)
- Metal shader generation framework (✅ working)

## Constraints

- **No simplification**: Must fix complex cases properly, not avoid them
- **No workarounds**: Must address root cause in conditional compilation
- **Maintain performance**: Fix should not degrade GPU simulation performance
- **Preserve architecture**: Must not break existing working functionality