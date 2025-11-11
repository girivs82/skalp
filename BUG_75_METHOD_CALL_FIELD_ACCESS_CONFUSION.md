# Bug #75: Method Call FieldExpr Built as FieldAccess

## Status: FIXED ✅

## Summary
FieldExpr nodes that are part of method call patterns (e.g., `.lt()`, `.gt()`) were being built as FieldAccess expressions instead of being left for the CallExpr to consume and convert into proper Call expressions.

## Root Cause
In `hir_builder.rs`, when `build_expression` encountered a FieldExpr node at line 2986, it would call `build_field_expr` which would build a FieldAccess expression. This happened even when the FieldExpr was part of a method call pattern where it should be combined with the following CallExpr to create a Call expression.

The method call detection only happened in `build_call_expr` (starting at line 3151) when processing a CallExpr and looking backwards for a preceding FieldExpr. But if `build_field_expr` was called first on the FieldExpr, it would create a FieldAccess before the CallExpr had a chance to detect the pattern.

## Example
For code like `a.abs().lt(epsilon)`, the parser creates siblings:
- IdentExpr(a), FieldExpr(.abs), CallExpr([]), FieldExpr(.lt), CallExpr([epsilon])

When building expressions:
1. CallExpr for abs correctly detects FieldExpr(.abs) and builds Call('abs', [a]) ✓
2. But then FieldExpr(.lt) gets built independently, creating FieldAccess(base=<abs result>, field='lt') ✗
3. This FieldAccess reaches MIR conversion where it triggers the Bug #74 detection for "field names that are actually methods"

## Impact
- ray_triangle_intersect failed to inline due to `.lt()` appearing as FieldAccess
- var_206 undefined error in generated SystemVerilog
- Similar issues with any chained method call: `.lt()`, `.gt()`, `.le()`, `.ge()`, etc.

## Fix
Modified `build_field_expr` in `hir_builder.rs` (line 4252) to check if the FieldExpr is followed by a CallExpr. If so, return None instead of building a FieldAccess expression. This allows the CallExpr to handle the complete method call pattern.

```rust
// BUG FIX #75: Check if this FieldExpr is followed by a CallExpr (method call pattern)
// If so, return None - this FieldExpr should be consumed by the CallExpr, not built independently
if let Some(parent) = node.parent() {
    let siblings: Vec<_> = parent.children().collect();
    if let Some(pos) = siblings.iter().position(|n| n == node) {
        if pos + 1 < siblings.len() && siblings[pos + 1].kind() == SyntaxKind::CallExpr {
            return None;
        }
    }
}
```

## Verification
After fix:
- Karythra CLE compiles successfully
- ray_triangle_intersect inlines correctly
- var_206 is properly defined in generated SystemVerilog
- Verilator reports no errors (only shortreal warnings)

## Related Bugs
- Bug #71 Part 4f: Fixed chained method calls in arguments
- Bug #74: Added HIR-level function inlining for field access on call results
- Bug #76: Fixed fabs undefined function error

## Files Modified
- `/Users/girivs/src/hw/hls/crates/skalp-frontend/src/hir_builder.rs` (line 4252-4269)
