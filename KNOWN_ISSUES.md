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

## CRITICAL: HIR Builder Drops Continuous Assignments to Struct-Typed Output Ports

### Issue
Continuous assignments to struct-typed output ports in impl blocks are silently dropped and never appear in the HIR. Single-field output ports (like `bit`) work correctly.

### Example (BROKEN):
```skalp
entity MyEntity {
    out output: SimpleVertex  // Struct-typed output port
}

impl MyEntity {
    signal out_vertex: SimpleVertex

    output_valid = (state == 1)  // ✅ Works - single bit port
    output = out_vertex           // ❌ Silently dropped - struct port
}
```

### Evidence
- HIR port ID for `output`: PortId(16)
- Port successfully flattened into: output_x, output_y, output_z
- NO assignment with LHS=Port(PortId(16)) found in HIR
- Only assignments to single-field ports reach MIR

### Root Cause
The HIR builder (hir_builder.rs) `build_implementation()` function is filtering out or not parsing continuous assignments to struct-typed output ports. This may be:
- Parser not recognizing the syntax
- HIR builder deliberately filtering output port assignments
- Scope/visibility issue where output ports aren't in symbol table during impl parsing

### Files Affected
- `/examples/graphics_pipeline/verif/testbenches/tb_pipeline_e2e.sk` - SimpleGeometryProcessor uses broken pattern

### Priority
CRITICAL - This silently generates incorrect hardware

### Fix Required
Investigate `build_implementation()` in `crates/skalp-frontend/src/hir_builder.rs` to understand why continuous assignments to struct-typed output ports aren't being added to the impl block's assignments list.

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
