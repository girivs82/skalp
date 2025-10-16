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

## Imported Generic Modules Not Instantiated

### Issue
When a generic entity is imported from another module and instantiated with `let`, the monomorphized instance is not added to the MIR module's instances list. This prevents hierarchical elaboration from processing the child module.

### Example (BROKEN):
```skalp
mod async_fifo;
use async_fifo::AsyncFifo;

impl MyModule {
    let fifo = AsyncFifo<Data, 8> {  // Instance not created in MIR!
        wr_clk: clk,
        ...
    }
}
```

### Workaround (WORKS):
Define the generic entity inline in the same file instead of importing:
```skalp
// Define inline
entity AsyncFifo<T, const DEPTH: nat> { ... }

impl MyModule {
    let fifo = AsyncFifo<Data, 8> {  // ✅ Instance created correctly
        wr_clk: clk,
        ...
    }
}
```

### Evidence
- Simple FIFO test (imports AsyncFifo): 0 instances in MIR
- Full pipeline test (defines inline): 3 instances in MIR (input_fifo, geometry, output_fifo)

### Root Cause
The MIR compiler's monomorphization doesn't properly register instances when the generic entity is imported from another module. This is a **frontend/MIR bug**, not a SIR/elaboration bug.

### Impact
Cannot use shared library modules for generic components like FIFOs. Each top-level module must define its own copy.

### Priority
HIGH - Severely limits code reuse and modularity

### Fix Required
- Investigate monomorphization in `crates/skalp-mir` or `crates/skalp-frontend`
- Ensure imported generic entities create instances in the importing module
- Add test case for imported generic instantiation

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
