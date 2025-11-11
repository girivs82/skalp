# Bug #9: Struct Field Binary Operations Incorrectly Include Parent Struct

## Status: FIXED ✅

## Summary
When compiling binary operations on struct fields (e.g., `a.x * b.x`), SKALP was incorrectly generating code that multiplied the entire parent struct with its field: `(a * a.x) * (b * b.x)` instead of just the fields: `a.x * b.x`. This caused massive expression inflation (128-bit × 32-bit × 128-bit × 32-bit = 512+ bits) and incorrect hardware behavior.

## Example
For the vec4 component-wise multiply code:
```skalp
let result = vec4 {
    x: a.x * b.x,  // Should multiply two 32-bit fields
    y: a.y * b.y,
    z: a.z * b.z,
    w: a.w * b.w
};
```

**Before Fix:**
```systemverilog
assign match_4_5_result = {
    (((match_4_5_a * match_4_5_a[31:0]) * match_4_5_b) * match_4_5_b[31:0]),  // 128*32*128*32 bits!
    (((match_4_5_a * match_4_5_a[63:32]) * match_4_5_b) * match_4_5_b[63:32]),
    (((match_4_5_a * match_4_5_a[95:64]) * match_4_5_b) * match_4_5_b[95:64]),
    (((match_4_5_a * match_4_5_a[127:96]) * match_4_5_b) * match_4_5_b[127:96])
};
```

**After Fix:**
```systemverilog
assign match_4_5_result = {
    (match_4_5_a[31:0] * match_4_5_b[31:0]),      // 32*32 bits ✓
    (match_4_5_a[63:32] * match_4_5_b[63:32]),    // 32*32 bits ✓
    (match_4_5_a[95:64] * match_4_5_b[95:64]),    // 32*32 bits ✓
    (match_4_5_a[127:96] * match_4_5_b[127:96])   // 32*32 bits ✓
};
```

## Root Cause
The bug occurred in the HIR (High-level IR) builder during AST construction. When the parser encountered `a.x * b.x`, it created sibling nodes:
```
BinaryExpr(*)
  - IdentExpr(a)
  - FieldExpr(.x)
  - IdentExpr(b)
  - FieldExpr(.x)
```

The `build_binary_expr` function collected ALL these as expression children and treated them as separate operands:
1. `expr_children[0]` = `IdentExpr(a)` → built as `Variable(a)`
2. `expr_children[1]` = `FieldExpr(.x)` → built as `FieldAccess{base: a, field: "x"}` (correctly finding the preceding sibling)
3. Created: `Binary { left: Variable(a), op: Mul, right: FieldAccess{...} }` ❌

This created nested binary operations: `((a * a.x) * b) * b.x` instead of the correct `a.x * b.x`.

## Impact
- **Massive expression width inflation**: 32-bit × 32-bit operations became 512+ bit operations
- **Incorrect hardware behavior**: Multiplying entire structs instead of individual fields
- **Synthesis issues**: Excessive logic resource usage
- **Functional errors**: Wrong computation results

## Solution
Modified `build_binary_expr()` in `/Users/girivs/src/hw/hls/crates/skalp-frontend/src/hir_builder.rs` (lines 3815-3843):

Added filtering logic to **skip expressions that are immediately followed by a FieldExpr**, since the FieldExpr handles them as its base:

```rust
// BUG FIX #9: Filter out expressions that are immediately followed by FieldExpr
// When the parser creates "a.x", it makes siblings [IdentExpr(a), FieldExpr(.x)]
// The FieldExpr will handle the IdentExpr as its base, so we shouldn't treat
// the IdentExpr as a separate operand in the binary expression
let mut indices_to_skip = Vec::new();
for i in 0..expr_children.len() {
    if i + 1 < expr_children.len() && expr_children[i + 1].kind() == SyntaxKind::FieldExpr {
        // Check if they're adjacent siblings (not separated by other nodes)
        if let Some(parent) = expr_children[i].parent() {
            if parent == expr_children[i + 1].parent().unwrap_or(parent.clone()) {
                let siblings: Vec<_> = parent.children().collect();
                if let (Some(curr_pos), Some(next_pos)) = (
                    siblings.iter().position(|n| n == &expr_children[i]),
                    siblings.iter().position(|n| n == &expr_children[i + 1]),
                ) {
                    if next_pos == curr_pos + 1 {
                        // They're adjacent - skip the current node
                        indices_to_skip.push(i);
                    }
                }
            }
        }
    }
}

// Remove skipped indices in reverse order
for &idx in indices_to_skip.iter().rev() {
    expr_children.remove(idx);
}
```

This ensures that:
- `IdentExpr(a)` is skipped when followed by `FieldExpr(.x)`
- Only `FieldExpr(.x)` is processed (which correctly finds `a` as its base and builds `FieldAccess{base: a, field: "x"}`)
- Binary expression correctly becomes: `Binary { left: FieldAccess{base: a, field: "x"}, op: Mul, right: FieldAccess{base: b, field: "x"} }`

## Verification
After fix:
- ✅ `a.x * b.x` correctly generates `match_4_5_a[31:0] * match_4_5_b[31:0]`
- ✅ No parent struct included in operations
- ✅ Expression widths correct (32 × 32 = 32 bits for fp32 multiply)
- ✅ Karythra CLE compiles successfully
- ✅ Verilator reports **0 errors** (only shortreal warnings)
- ✅ All component-wise vector operations work correctly

## Debug Output Example
HIR before fix showed:
```
Binary(HirBinaryExpr {
    left: Binary(HirBinaryExpr {
        left: Binary(HirBinaryExpr {
            left: Variable(VariableId(43)),    // a (WRONG!)
            op: Mul,
            right: FieldAccess { base: Variable(VariableId(43)), field: "x" }
        }),
        op: Mul,
        right: Variable(VariableId(44))   // b (WRONG!)
    }),
    op: Mul,
    right: FieldAccess { base: Variable(VariableId(44)), field: "x" }
})
```

HIR after fix (expected):
```
Binary(HirBinaryExpr {
    left: FieldAccess { base: Variable(VariableId(43)), field: "x" },  // a.x ✓
    op: Mul,
    right: FieldAccess { base: Variable(VariableId(44)), field: "x" }  // b.x ✓
})
```

## Files Modified
- `/Users/girivs/src/hw/hls/crates/skalp-frontend/src/hir_builder.rs`:
  - Modified `build_binary_expr()` to filter out expressions followed by FieldExpr (lines 3815-3843)

## Related Issues
- This bug affected all binary operations with struct field access (addition, subtraction, multiplication, etc.)
- Related to Bug #71 Part 3b (FieldExpr with no children looking for preceding sibling)
- Fixed the expression width inflation that was preventing proper hardware generation

## Test Cases
Successfully tested on:
- vec4 component-wise multiply (`a.x * b.x`, `a.y * b.y`, etc.)
- vec4 component-wise add
- vec3 operations
- Nested struct field operations
- Karythra CLE ray-sphere and ray-triangle intersection code
