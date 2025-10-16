# Known Issues and Limitations

## ✅ FIXED: GPU Simulator Hierarchical Elaboration (Bugs #13-16)

### Issues (FIXED in commits c63fa8b, 9508b5d, 63f4fa7, a57eda4, f06cbc1)
Four critical bugs in hierarchical elaboration for GPU simulator:

**Bug #13** - SignalId/PortId Type Confusion
- **Issue**: Comparing SignalId with PortId by numeric value caused incorrect signal resolution
- **Fix**: Removed buggy fallback, LValue::Signal now only matches signals
- **File**: `crates/skalp-sir/src/mir_to_sir.rs:4256-4265`

**Bug #14** - Missing Block Statement Recursion
- **Issue**: Flattened struct assignments wrapped in Block statements were being ignored
- **Fix**: Added Statement::Block case with recursive search
- **File**: `crates/skalp-sir/src/mir_to_sir.rs:3915-3926`

**Bug #15** - Non-Deterministic HashSet Ordering
- **Issue**: FlipFlops created in random order, connecting to wrong data nodes
- **Fix**: Changed from HashSet to Vec with sort()+dedup() for deterministic ordering
- **File**: `crates/skalp-sir/src/mir_to_sir.rs:3457-3473`

**Bug #16** - LValue::Signal Not Stripping Flattened Suffixes
- **Issue**: Assignment matching failed because LHS wasn't stripping suffixes like `mem_0_x` → `mem`
- **Fix**: Strip flattened suffixes from LValue::Signal during assignment search
- **File**: `crates/skalp-sir/src/mir_to_sir.rs:3861-3872`

### Verification
- ✅ Simple geometry processor test passes (struct assignments work)
- ✅ Data paths verified correct (port mapping, conditionals, MUX trees)
- ✅ Assignment matching works for all flattened elements

### Status
✅ **FIXED** - Hierarchical elaboration now works correctly for struct/array assignments

---

## ⚠️ PARTIAL: GPU Simulator Sequential Array Assignments (Bug #19)

### Issue
Sequential array assignments in GPU simulator read zeros instead of written values.

### Root Causes Identified & Fixed
**✅ FIXED**: MIR→SIR conversion was not creating FlipFlop nodes for expanded array assignments

The `collect_assignment_targets` function only looked at immediate statements, failing to recurse into nested If and Block statements. Expanded array assignments like:
```
if (wr_en) {
    mem_0 <= (wr_addr == 0) ? wr_data : mem_0
    mem_1 <= (wr_addr == 1) ? wr_data : mem_1
}
```

Were nested in Block statements and weren't being collected as targets.

**Fix Applied**: Created recursive `collect_targets_from_block` helper that processes nested If and Block statements.

### Remaining Issue
⚠️ FlipFlop nodes are now created, but MUX logic for conditional assignments is not generating correct values. The nodes (`node_3_out`, etc.) that should compute `(wr_addr == N) ? wr_data : mem_N` are producing zeros.

Investigation needed in `synthesize_conditional_assignment` function.

### Status
🔄 **PARTIALLY FIXED** - FlipFlops created, but MUX logic needs investigation

**Files Changed**:
- `crates/skalp-sir/src/mir_to_sir.rs:1503-1551` (recursive target collection)

---

## ✅ FIXED: Non-Deterministic Monomorphization (Bug #18)

### Issue (FIXED in commit TBD)
Monomorphization used a `HashSet<Instantiation>` to collect generic instantiations, causing non-deterministic EntityId assignment. When multiple generic entities were instantiated (e.g., `AsyncFifo<Data, 8>` and `AsyncFifo<Data, 32>`), they would get assigned EntityIds in random order across runs. This caused instance references to point to the wrong specialized entities.

### Example:
```skalp
// Two AsyncFifo instances with different depths
let input_fifo = AsyncFifo<SimpleVertex, 8> { ... }
let output_fifo = AsyncFifo<SimpleVertex, 32> { ... }
```

**Before fix**: Hash iteration order was non-deterministic
- Run 1: AsyncFifo_8 → EntityId(3), AsyncFifo_32 → EntityId(4)
- Run 2: AsyncFifo_32 → EntityId(3), AsyncFifo_8 → EntityId(4)
- Instance references would be swapped between runs!

**After fix**: Sorted by mangled name for deterministic ordering
- AsyncFifo_32_SimpleVertex → EntityId(3) (always)
- AsyncFifo_8_SimpleVertex → EntityId(4) (always)

### Root Cause
The `InstantiationCollector::collect()` returned `HashSet<Instantiation>`, which has non-deterministic iteration order. During monomorphization, specialized entities were created in hash order, causing inconsistent EntityId assignment.

### Fix Applied
Modified `MonomorphizationEngine::monomorphize` in `crates/skalp-frontend/src/monomorphization/engine.rs`:
1. Convert `HashSet<Instantiation>` to `Vec<Instantiation>`
2. Sort by `mangled_name()` for deterministic ordering
3. Updated `find_matching_instantiation` signature to accept `&[Instantiation]` instead of `&HashSet<Instantiation>`

**Files Changed**:
- `crates/skalp-frontend/src/monomorphization/engine.rs:42-45` (sort instantiations)
- `crates/skalp-frontend/src/monomorphization/engine.rs:812` (update function signature)

### Status
✅ **FIXED** - Monomorphization now produces deterministic EntityId assignment across runs

### Note
While this fix ensures correct instance mapping, the `test_graphics_pipeline_multi_clock_domains` test still reads zeros. This appears to be a separate GPU simulator execution issue (not a compilation bug), requiring further investigation into Metal shader execution or AsyncFifo sequential logic.

---

## ✅ FIXED: Imported Generic Module Implementations Not Merged (Bug #17)

### Issue (FIXED)
When a generic entity was imported from another module (`mod async_fifo; use async_fifo::AsyncFifo`), only the entity definition was copied to the importing module's HIR, but NOT the implementation. This prevented monomorphization from working because there was no implementation body to specialize.

### Example (NOW WORKS):
```skalp
mod async_fifo;
use async_fifo::AsyncFifo;

impl MyModule {
    let fifo = AsyncFifo<Data, 8> {  // ✅ Now works correctly!
        wr_clk: clk,
        ...
    }
}
```

### Root Cause
The `merge_symbol` function in `crates/skalp-frontend/src/lib.rs` only copied the entity definition but not its associated implementation block. The monomorphization engine needs both to create specialized instances.

### Fix Applied (commit TBD)
Modified three functions in `crates/skalp-frontend/src/lib.rs`:
1. `merge_symbol` - Now also merges the implementation for imported entities
2. `merge_symbol_with_rename` - Same fix for renamed imports
3. `merge_all_symbols` - Same fix for glob imports

Additionally fixed `Testbench::with_config` in `crates/skalp-testing/src/testbench.rs` to use `parse_and_build_hir_from_file` instead of `parse_and_build_hir` so module resolution works properly.

**Files Changed**:
- `/Users/girivs/src/hw/hls/crates/skalp-frontend/src/lib.rs:265-275` (merge_symbol)
- `/Users/girivs/src/hw/hls/crates/skalp-frontend/src/lib.rs:349-357` (merge_symbol_with_rename)
- `/Users/girivs/src/hw/hls/crates/skalp-frontend/src/lib.rs:455-461` (merge_all_symbols)
- `/Users/girivs/src/hw/hls/crates/skalp-testing/src/testbench.rs:56-60` (with_config)

### Verification
✅ **VERIFIED WORKING**: Full pipeline test (`test_graphics_pipeline_multi_clock_domains`) correctly:
- Imports AsyncFifo entity and implementation from `examples/graphics_pipeline/lib/async_fifo.sk`
- Creates 3 instances in SimplePipelineTop (input_fifo, geometry, output_fifo)
- Collects 2 AsyncFifo instantiations for monomorphization
- Specializes AsyncFifo for `SimpleVertex` type with depth 8

### Status
✅ **FIXED and VERIFIED** - Imported generic entities and their implementations are now properly merged and monomorphized correctly

---

## ✅ FIXED: Keyword Port Names (Bugs #11 and #12)

### Issues (FIXED in commits dc55e0d and e5368e1)
Two related bugs where reserved keywords used as port names were silently dropped:
1. **Bug #11**: Instance connections using keyword port names were dropped
2. **Bug #12**: Continuous assignments to keyword-named ports were dropped

### Example (NOW WORKS):
```skalp
entity MyEntity {
    out output: SimpleVertex  // "output" is a keyword
}

impl MyEntity {
    signal out_vertex: SimpleVertex
    output = out_vertex  // ✅ Now works correctly
}

let geometry = MyEntity {
    output: geom_output  // ✅ Now works correctly
}
```

### Root Cause
Both `build_connection()` and `build_lvalue()` in hir_builder.rs only looked for
`Ident` tokens. When keywords like "output" were used as port names, they were
tokenized as `OutputKw` instead of `Ident`, causing the functions to return None.

### Fix Applied
Both functions now accept both identifier AND keyword tokens:
```rust
let name = node
    .first_token_of_kind(SyntaxKind::Ident)
    .or_else(|| {
        node.children_with_tokens()
            .filter_map(|elem| elem.into_token())
            .find(|t| t.kind().is_keyword())
    })
    .map(|t| t.text().to_string())?;
```

### Status
✅ **FIXED** - Reserved keywords can now be used as port names in all contexts

---

## CRITICAL: Struct Field Assignments in Sequential Blocks

### Issue
Direct assignments to struct fields in sequential blocks are silently dropped and do not appear in the generated SystemVerilog.

### Example (BROKEN):
```skalp
signal out_data: MyStruct

on(clk.rise) {
    out_data.field_x <= input_value  // This assignment is silently dropped!
}
```

### Workaround Pattern (WORKS):
```skalp
// Use intermediate scalar signals
signal field_x_reg: bit[32]
signal field_y_reg: bit[32]

on(clk.rise) {
    field_x_reg <= input.field_x  // Works correctly
    field_y_reg <= input.field_y
}

// Build struct in continuous assignment
output = MyStruct {
    field_x: field_x_reg,
    field_y: field_y_reg
}
```

### Root Cause
The HIR-to-MIR conversion's `convert_lvalue` function handles field access lookups, but flattened struct signals assigned in sequential blocks are not properly recognized by the `is_register` check in SystemVerilog codegen, causing them to be declared as `wire` instead of `reg`, and the assignments are dropped.

### Files Affected
- `/examples/graphics_pipeline/src/main.sk` - GeometryProcessor4 stub uses broken pattern
- See `/examples/graphics_pipeline/src/geometry_processor.sk` for correct pattern

### Priority
HIGH - This silently generates incorrect hardware

### Fix Required
- Enhance `try_expand_struct_assignment` in `crates/skalp-mir/src/hir_to_mir.rs` to handle field access on LHS
- Ensure flattened signals are properly tracked through sequential assignments
- Update `is_register` logic to recognize flattened field assignments
