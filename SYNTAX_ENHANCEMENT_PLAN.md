# SKALP Syntax Enhancement Implementation Plan

**Purpose**: Systematic implementation to support original showcase syntax with NO shortcuts or workarounds.

**Based on**: Analysis in `/examples/complex_project/SYNTAX_ISSUES_AND_ENHANCEMENTS.md`

---

## Three Critical Features

1. ✅ **Let bindings in `on()` blocks** - Most critical
2. ✅ **Functions in `impl` blocks** - Essential for organization
3. ✅ **Const functions** - User requested, enables generic designs

All will be implemented properly through the entire compiler pipeline.

---

## Feature 1: Let Bindings in Sequential Blocks

### Example
```skalp
on(wr_clk.rise) {
    let next_wr_ptr = wr_ptr + 1
    let gray = next_wr_ptr ^ (next_wr_ptr >> 1)
    wr_ptr <= next_wr_ptr
    wr_ptr_gray <= gray
}
```

### Current Status
- ✅ Lexer has `LetKw` token
- ❌ Parser doesn't handle it in blocks
- ❌ AST has no `Let` variant
- ❌ No HIR/codegen support

### Implementation (Est: 1-2 days)

**Files to modify: 5-6**

1. `crates/skalp-frontend/src/ast.rs` - Add Let to Statement enum
2. `crates/skalp-frontend/src/parse.rs` - Add parse_let_statement
3. `crates/skalp-frontend/src/syntax.rs` - Add LetStmt node
4. `crates/skalp-frontend/src/hir_builder.rs` - Transform Let statements
5. `crates/skalp-codegen/` - Generate local wires

**Generated SystemVerilog**:
```systemverilog
logic [8:0] next_wr_ptr;
logic [8:0] gray;
assign next_wr_ptr = wr_ptr + 1;
assign gray = next_wr_ptr ^ (next_wr_ptr >> 1);
wr_ptr <= next_wr_ptr;
wr_ptr_gray <= gray;
```

---

## Feature 2: Functions in impl Blocks

### Example
```skalp
impl GeometryProcessor<const STAGES: nat> {
    fn vec3_to_vec4(v: Vec3, w: fp32) -> Vec4 {
        Vec4 { x: v.x, y: v.y, z: v.z, w: w }
    }

    on(clk.rise) {
        result <= vec3_to_vec4(input_vec, 1.0)
    }
}
```

### Current Status
- ✅ Lexer has `FnKw` token
- ❌ No function parsing anywhere
- ❌ No function AST nodes
- ❌ No function support in HIR

### Implementation (Est: 2-3 days)

**Files to modify: 8-10**

1. `ast.rs` - Add FunctionDecl to Item and ImplItem
2. `parse.rs` - Add function parsing
3. `hir_builder.rs` - Function scoping and lookup
4. `typeck.rs` - Function type checking
5. `codegen/` - Generate SystemVerilog functions

**Generated SystemVerilog**:
```systemverilog
function automatic vec4 vec3_to_vec4(vec3 v, real w);
    return '{v.x, v.y, v.z, w};
endfunction
```

---

## Feature 3: Const Functions

### Example
```skalp
const fn clog2(n: nat) -> nat {
    if n <= 1 { 0 } else { 1 + clog2(n / 2) }
}

signal addr: nat[clog2(DEPTH)]
```

### Current Status
- ✅ `const_eval.rs` exists
- ❌ Doesn't support function calls
- ❌ Not integrated with type system

### Implementation (Est: 3-5 days)

**Complex - requires const evaluation engine**

1. Mark functions as `const`
2. Const interpreter for SKALP expressions
3. Recursive function evaluation
4. Integration with type parameters

---

## Implementation Order

### Week 1: Let Bindings
**Goal**: async_fifo_original.sk compiles

- [ ] Day 1-2: Implement let statements
- [ ] Day 3: Test and verify codegen
- [ ] Day 4-5: Polish and edge cases

### Week 2: Functions
**Goal**: geometry_processor.sk with functions compiles

- [ ] Day 6-7: Basic function parsing
- [ ] Day 8-9: Scoping and lookup
- [ ] Day 10: Codegen and testing

### Week 3-4: Const Functions
**Goal**: Parameterized designs with compile-time evaluation

- [ ] Day 11-13: Const evaluator
- [ ] Day 14-15: Integration and testing

---

## Testing Strategy

### For Each Feature:

1. **Parser Tests**
   ```rust
   #[test]
   fn test_parse_let_statement() { ... }
   ```

2. **Codegen Tests**
   - Verify SystemVerilog output
   - Test with iverilog/verilator

3. **Integration Tests**
   - Compile showcase examples
   - Verify no regressions

4. **Real Usage**
   - async_fifo.sk
   - geometry_processor.sk
   - main.sk (full system)

---

## Success Criteria

✅ **Phase 1 Complete When**:
- `async_fifo_original.sk` compiles unchanged
- Generated Verilog simulates correctly
- No expression duplication needed

✅ **Phase 2 Complete When**:
- `geometry_processor.sk` with all 8 helper functions compiles
- Functions generate correct SystemVerilog
- Hierarchical calls work

✅ **Phase 3 Complete When**:
- `clog2` defined in SKALP compiles
- Used in signal width declarations
- Correctly evaluated at compile time

✅ **All Complete When**:
- Full `examples/complex_project/` compiles
- Simulates correctly
- Synthesizes to working hardware

---

## Ready to Start

**Confirmation needed to proceed with Phase 1 (Let Bindings)**

Will implement systematically with:
- ✅ No shortcuts
- ✅ No workarounds
- ✅ Complete implementation
- ✅ Proper testing
- ✅ Quality over speed
