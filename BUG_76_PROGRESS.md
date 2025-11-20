# Bug #76: Tuple Width Fix - COMPLETE ✅

## Summary
**Bug #76 is now COMPLETELY FIXED!** Mixed-width tuples like `(bit, bit[32], bit[32])` now work correctly throughout the entire compiler pipeline (HIR → MIR → SIR → Metal codegen).

## What Was Fixed ✅

### 1. Expression Type Annotation (MIR)
- **File**: `crates/skalp-mir/src/hir_to_mir.rs`
- **Function**: `annotate_expression_with_type()` (line 5402)
- **Fix**: Recursively annotates expressions with type information:
  - Concat expressions: Each part annotated with its tuple element type
  - Conditional expressions: Both branches recursively annotated
  - Handles nested structures correctly

### 2. Type Propagation Through Compilation
- HIR type inference correctly identifies `(bit, bit[32], bit[32])` as return type
- MIR expressions carry type information through conversion
- SIR conversion receives correct types and extracts widths properly
- Width calculation: 1 + 32 + 32 = 65 bits ✅

### 3. Metal Shader Slice Extraction (uint4 and uint2)
- **File**: `crates/skalp-sir/src/metal_codegen.rs`
- **Lines**: 1625-1730
- **Problem**: Was using component access (`.x`, `.y`, `.z`, `.w`) which extracts full 32-bit values
- **Fix**: Added bit-level extraction using shifts and masks for sub-32-bit or non-aligned slices
- **Example**:
  - Before: `signals->output = input.x;` (extracts bits [31:0])
  - After: `signals->output = (input.x >> 0) & 0x1;` (extracts bit 0 only)

### 4. Metal Shader Concat Generation (uint4)
- **File**: `crates/skalp-sir/src/metal_codegen.rs`
- **Lines**: 2256-2368
- **Problem**: Multiple bugs in concat generation:
  - Used `component_idx = bit_offset / 32` causing multiple inputs to map to same component and overwrite each other
  - Reversed component order incorrectly (thought uint4 needed reversal but it doesn't)
  - No bit-level packing for inputs spanning multiple 32-bit components

- **Fix**: Complete rewrite with proper bit-level packing:
  - `ComponentContribution` struct tracks which bits from each input go to which component
  - Handles inputs spanning multiple 32-bit components correctly
  - Generates proper bit shifts, masks, and OR operations
  - Correct component ordering: `uint4(comp[0], comp[1], comp[2], comp[3])` where comp[0] is LSB

- **Example for (bit, bit[32], bit[32])**:
  ```metal
  // Component 0 (bits 0-31): bit 0 from valid + bits 1-31 from x1
  // Component 1 (bits 32-63): bit 31 from x1 + bits 0-30 from x2
  // Component 2 (bits 64-65): bit 31 from x2
  signals->result = uint4(
    (valid & 0x1) | ((x1 & 0x7FFFFFFF) << 1),
    ((x1 >> 31) & 0x1) | ((x2 & 0x7FFFFFFF) << 1),
    (x2 >> 31) & 0x1,
    0u
  );
  ```

## Test Results ✅

Both test cases now **PASS**:
- `test_quadratic_solver_no_real_roots`: ✅ (expects valid=0, gets 0)
- `test_quadratic_solver_two_real_roots`: ✅ (expects valid=1 + x₁=2.0 + x₂=3.0, gets all correct)

```bash
$ cargo test --test test_l4_quadratic_minimal
running 2 tests
test test_quadratic_solver_no_real_roots ... ok
test test_quadratic_solver_two_real_roots ... ok

test result: ok. 2 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out
```

## Files Modified

1. **`/Users/girivs/src/hw/hls/crates/skalp-mir/src/hir_to_mir.rs`**
   - Added `annotate_expression_with_type()` with recursive Conditional handling
   - Lines 5402-5464

2. **`/Users/girivs/src/hw/hls/crates/skalp-sir/src/metal_codegen.rs`**
   - Fixed slice extraction for uint4 (lines 1625-1683)
   - Fixed slice extraction for uint2 (lines 1684-1730)
   - Complete rewrite of uint4 concat generation with bit-level packing (lines 2256-2368)

3. **`/Users/girivs/src/hw/karythra/tests/quadratic_unit.sk`**
   - Updated to use `(bit, bit[32], bit[32])` instead of `(bit[32], bit[32], bit[32])`
   - Lines 4, 35, 41

4. **`/Users/girivs/src/hw/karythra/tests/test_l4_quadratic_minimal.rs`**
   - Updated to read `valid` as `u8` instead of `u32`
   - Lines 22, 59

## Technical Details

### Key Insight: uint4 Component Ordering
The critical bug was incorrectly reversing components when constructing uint4. In Metal:
- `uint4(x, y, z, w)` maps to:
  - `.x` = bits [0:31] (LSB)
  - `.y` = bits [32:63]
  - `.z` = bits [64:95]
  - `.w` = bits [96:127] (MSB)

Components should NOT be reversed - the constructor already has the LSB first.

### Bit-Level Packing Algorithm
For mixed-width tuples, the concat generator:
1. Iterates through input values and their widths
2. For each input, calculates which 32-bit component(s) it spans
3. Tracks all contributions to each component in `ComponentContribution` structs
4. Builds component expressions by OR'ing shifted and masked contributions
5. Handles three cases:
   - Empty: `0u`
   - Full aligned 32-bit: use value directly
   - Partial/non-aligned: generate `(value & mask) << shift`

## Verification

Run the test suite:
```bash
cargo test --test test_l4_quadratic_minimal
```

Both tests should pass with correct values for all outputs.

## Status: COMPLETE ✅

Bug #76 is fully resolved. Mixed-width tuples with bit-level fields now work correctly in all compilation stages including Metal shader generation.
