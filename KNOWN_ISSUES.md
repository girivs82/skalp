# Known Issues and Limitations

## GPU Simulator Data Flow Limitations

### Issue
The GPU-accelerated simulator (Metal-based) has issues with data flow:
- Input ports read in sequential blocks return 0
- FIFO read data returns 0
- Only constant assignments work correctly in sequential blocks

### Impact
Functional testing is limited. Complex designs compile to correct SystemVerilog but cannot be fully simulated with the GPU simulator.

### Workaround
Use Verilator or other SystemVerilog simulators for functional verification.

### Priority
MEDIUM - Affects testing but not production code generation

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
