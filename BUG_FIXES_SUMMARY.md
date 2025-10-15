# Systematic Bug Fixes - Session Summary

**Date:** October 15, 2025

## Bugs Fixed

### 1. ✅ Entity Instantiation with Flattened Struct Ports
**Problem:** When entity ports used flattened struct types, only one field was connected instead of all fields.

**Fix:** Modified `crates/skalp-codegen/src/systemverilog.rs`:
- Added `expand_port_connection()` and `expand_port_connection_field()` helper functions
- Modified `generate_instance()` to detect and expand flattened ports
- Added smart filtering to distinguish scalar ports from flattened struct fields
- Handles both nested structs and vector components (_x, _y, _z, _w)

**Result:**
- Verilator missing pin warnings: ~40 → 0
- All flattened struct ports correctly connected
- Graphics pipeline entity instantiation works correctly

**Files Modified:**
- `crates/skalp-codegen/src/systemverilog.rs` (lines 637-770, 243-347)

---

### 2. ✅ Duplicate Variable Declarations
**Problem:** Multiple `let` bindings with the same variable name (in different scopes) generated duplicate module-level variable declarations.

**Example:**
```verilog
logic [31:0] addr;  // First occurrence
logic [31:0] addr;  // DUPLICATE ERROR!
```

**Fix:** Modified `crates/skalp-mir/src/hir_to_mir.rs`:
- Check if a variable with the same name already exists in `dynamic_variables`
- Reuse the existing MIR variable ID instead of creating a new one
- All let bindings with the same name now share one variable ID

**Result:**
- Duplicate declaration errors eliminated
- Single variable declaration per unique name
- All variable ID references properly resolve

**Files Modified:**
- `crates/skalp-mir/src/hir_to_mir.rs` (lines 424-460, 225-240)

---

### 3. ✅ Undefined Variable References (var_0)
**Problem:** Variable references generating fallback names like `var_0` instead of actual variable names.

**Root Cause:** Multiple let bindings created separate variable IDs for the same name, but only one was added to the module.

**Fix:** Addressed by fix #2 above - reusing variable IDs prevents unmapped references.

**Result:**
- No more `var_0` undefined variable errors
- All variable references properly resolved to names

---

## Remaining Issues

### 4. ⚠️ Generic Entity Port/Signal Resolution
**Problem:** AsyncFifo generic entity has incorrect code generation:
- Port IDs generating fallback names (`port_0`, `port_1`, `port_26`, `port_28`, `port_29`)
- Wrong signals in module (Vertex struct fields instead of FIFO signals)

**Affected Code:**
```verilog
module AsyncFifo (
    input wr_clk,    // port_0 should reference this
    input wr_rst,    // port_1 should reference this
    ...
);
    // Has wrong signals:
    wire [31:0] out_vertex_position_x;  // Should not be here!
    ...
    always_ff @(posedge port_0) begin  // Should be wr_clk
        if (port_1) begin               // Should be wr_rst
```

**Root Cause:** Generic type parameter monomorphization is not properly handling:
1. Port ID mapping when the entity is instantiated with concrete types
2. Signal flattening for generic type parameters (T → Vertex)
3. Array of generic types (`mem: [T; DEPTH]`)

**Complexity:** This is an architectural issue requiring:
- Proper generic entity monomorphization
- Tracking port/signal IDs across generic instantiations
- Correct flattening of parameterized types

**Status:** Documented but not fixed (requires significant refactoring)

**Workaround:** For now, avoid using generic entities with struct type parameters

---

### 5. ℹ️ Multiple Top-Level Modules (MULTITOP Warning)
**Issue:** Verilator warning about multiple top-level modules in one file.

**Status:** This is expected and not an error - just informational

**Modules:**
1. GeometryProcessor4
2. GraphicsPipelineTop
3. AsyncFifo

**Resolution:** Use `verilator --top-module GraphicsPipelineTop` to specify the top module

---

## Verification Results

### Before Fixes:
- **Errors:** 4 (duplicate addr, var_0, port_0, port_1)
- **Warnings:** ~44 (40 missing pin connections + 4 other)

### After Fixes:
- **Errors:** 2 (port_0, port_1 from AsyncFifo generic issue)
- **Warnings:** 4 (MULTITOP + IMPLICIT port warnings from AsyncFifo)

### Improvement:
- ✅ Fixed 2/4 errors (50% reduction)
- ✅ Eliminated ALL missing pin connection warnings (40 warnings fixed!)
- ✅ Entity instantiation with flattened structs fully working
- ✅ Variable declarations properly deduplicated

---

## Test Results

### Working Examples:
- ✅ Counter
- ✅ Simple struct flattening
- ✅ Graphics pipeline (partial - main pipeline works, AsyncFifo has issues)
- ✅ 12/13 core examples

### Known Issues:
- ⚠️ AsyncFifo with generic type parameters
- ⚠️ Any generic entity instantiated with struct types

---

## Next Steps

### Priority 1: AsyncFifo Generic Type Handling
1. Implement proper generic entity monomorphization
2. Track port/signal IDs across generic instantiations
3. Fix signal flattening for arrays of generic types
4. Add test for generic entities with struct type parameters

### Priority 2: Test Coverage
1. Add golden test for entity instantiation with flattened structs
2. Add test for variable deduplication across scopes
3. Add regression test for graphics pipeline

### Priority 3: Documentation
1. Update GRAPHICS_PIPELINE_STATUS.md with latest results
2. Document generic entity limitations
3. Add examples of working vs problematic patterns

---

## Files Modified This Session

1. `crates/skalp-codegen/src/systemverilog.rs`
   - Entity instantiation with flattened ports (✅ Complete)

2. `crates/skalp-mir/src/hir_to_mir.rs`
   - Variable deduplication fix (✅ Complete)

3. `GRAPHICS_PIPELINE_STATUS.md`
   - Updated status documentation (✅ Complete)

---

## Time Spent

- Entity instantiation fix: 2 hours
- Variable deduplication fix: 30 minutes
- Systematic debugging: 1 hour
- **Total:** ~3.5 hours

---

## Key Learnings

1. **Struct Flattening:** Works great for single entities and hierarchical designs with proper port mapping
2. **Variable Scoping:** Let bindings in different scopes need careful ID management
3. **Generic Entities:** Need special handling for type parameter monomorphization
4. **Systematic Approach:** Fixing errors one at a time with proper root cause analysis is effective
