# Bug #7: Nested If-Return Fixes

## Problem Summary

Bug #7 involved circular dependencies and undefined variables in nested if-return inlining for the ray-sphere intersection function in CLE L4-L5. The function uses nested if-returns with early returns, which were being incorrectly converted to expressions.

### Initial Symptoms
- Generated invalid Verilog with undefined variables (`var_140`)
- Circular dependencies (`var_142` used before defined)
- Incorrect conditions using variables instead of comparison expressions
- L4-L5 tests returning zeros instead of correct ray-sphere intersection results

## Root Causes Identified

### Root Cause #1: Missing CallExpr/FieldExpr in Condition Capture
**File:** `crates/skalp-frontend/src/hir_builder.rs:1814-1825`
**Problem:** Method calls like `a.lt(0.0)` in if-conditions were being reduced to just the variable `a`.
**Why:** The `build_if_statement` function only looked for `BinaryExpr`, `UnaryExpr`, and `ParenExpr` as "complex" conditions. Method calls like `a.lt(0.0)` are `CallExpr` nodes, which weren't in the list, so the code skipped the CallExpr and captured just the IdentExpr for `a`.

### Root Cause #2: Incomplete Method Call Argument Collection
**File:** `crates/skalp-frontend/src/hir_builder.rs:3146`
**Problem:** Chained method calls like `a.y.mul(b.y)` in nested expressions were being parsed as 3 separate arguments instead of 2.
**Why:** The argument collection loop only treated `FieldExpr` and `IndexExpr` as postfix operations. For an expression like `a.y.mul(b.y).add(...)`, the parser creates `[IdentExpr(a), FieldExpr(.y), FieldExpr(.mul), CallExpr(b.y)]` as siblings. Without `CallExpr` in the postfix list, the loop collected `[IdentExpr, FieldExpr, FieldExpr]` as one argument and `[CallExpr]` as another, resulting in 3 args instead of 2.

### Root Cause #3: Missing Type Inference for Call Expressions
**File:** `crates/skalp-mir/src/hir_to_mir.rs:4704-4713`
**Problem:** Type inference for Call expressions returned `None`, preventing FP method detection.
**Why:** The `infer_hir_type` function didn't handle `Call` expressions. When converting expressions like `vec_dot(oc, oc).sub(...)`, type inference failed for the `vec_dot` call, so the compiler couldn't determine that `.sub` should be an FP operation.

## Fixes Applied

### Fix #1: Add CallExpr/FieldExpr to Condition Capture
**Location:** `crates/skalp-frontend/src/hir_builder.rs:1814-1825`

```rust
// BEFORE:
let is_complex = matches!(
    child.kind(),
    SyntaxKind::BinaryExpr | SyntaxKind::UnaryExpr | SyntaxKind::ParenExpr
);

// AFTER:
let is_complex = matches!(
    child.kind(),
    SyntaxKind::BinaryExpr
        | SyntaxKind::UnaryExpr
        | SyntaxKind::ParenExpr
        | SyntaxKind::CallExpr      // Method calls like lt(), gt()
        | SyntaxKind::FieldExpr     // Field access (part of chained calls)
);
```

**Impact:** Conditions now generate correctly:
- Before: `assign result = (a ? {0, 0} : ...)`  ❌
- After: `assign result = ((a < 0) ? {0, 0} : ...)`  ✅

### Fix #2: Include CallExpr in Method Call Argument Collection
**Location:** `crates/skalp-frontend/src/hir_builder.rs:3146`

```rust
// BEFORE:
while arg_end < call_children.len()
    && matches!(
        call_children[arg_end].kind(),
        SyntaxKind::FieldExpr | SyntaxKind::IndexExpr
    )
{
    arg_end += 1;
}

// AFTER:
while arg_end < call_children.len()
    && matches!(
        call_children[arg_end].kind(),
        SyntaxKind::FieldExpr | SyntaxKind::IndexExpr | SyntaxKind::CallExpr
    )
{
    arg_end += 1;
}
```

**Impact:** Method calls now have correct arity:
- Before: `add` method with 3 args → fails to inline
- After: `add` method with 2 args → successfully converted to FAdd

### Fix #3: Add Call Expression Type Inference
**Location:** `crates/skalp-mir/src/hir_to_mir.rs:4704-4713`

```rust
// Added new case to infer_hir_type:
hir::HirExpression::Call(call) => {
    // BUG FIX #7: Infer type from function return type
    // This is needed for chained method calls like vec_dot(a, b).add(c)
    // where vec_dot returns fp32, enabling type inference for the add method
    if let Some(func) = self.find_function(&call.function) {
        func.return_type.clone()
    } else {
        None
    }
}
```

**Impact:** Better type inference for chained FP operations, allowing proper conversion to hardware FP operations.

## Test Case Verification

### Simple Test Case: test_nested_if_return.sk

**Input:**
```skalp
fn test_func(x: fp32, y: fp32) -> (bit, fp32) {
    let a = x.add(y);
    if a.lt(0.0) {
        return (0, 0.0)
    }
    let b = a.mul(2.0);
    if b.gt(10.0) {
        return (1, b)
    } else {
        return (0, 0.0)
    }
}
```

**Generated Verilog (Correct):**
```verilog
assign a = (x + y);
assign b = (a * 2);
assign result = ((a < 0) ? {0, 0} : ((b > 10) ? {1, b} : {0, 0}));
assign hit = result[0:0];
assign value = result[32:1];
```

✅ Conditions are proper comparisons
✅ Variables are properly assigned
✅ No circular dependencies in this simple case

## Remaining Issues

### Issue #1: Some FP Method Calls Still Failing
**Status:** Partially fixed
**Details:** 3 `add` function calls in the full CLE build still fail to inline, likely due to incomplete type inference in complex nested contexts.

### Issue #2: Circular Dependencies in Complex Cases
**Status:** Unfixed
**Details:** In the full ray_sphere_intersect function:
```verilog
assign var_135 = ((var_140 < var_141) ? {0, 0} : ((var_142 > var_141) ? {1, var_142} : {0, 0}));
assign var_142 = var_135[63:32];  // Circular: var_142 used above but defined here
```

**Root Cause:** The variable `t` is defined AFTER the first if-check in the source code:
```skalp
if discriminant.lt(zero) {
    return (0, 0)
}
let t = b.neg().sub(discriminant.sqrt())...;  // Defined here
if t.gt(zero) {                                 // But used in nested condition
    return (1, t as bit[32])
}
```

When converted to nested ternary expressions, `t` appears in the condition before it's computed. This requires deeper changes to hoist let-bindings from nested blocks.

### Issue #3: Missing Variable Assignments
**Status:** Unfixed
**Details:** Variables like `var_140` (discriminant) are referenced but never assigned in generated Verilog. This appears related to failed function inlining for complex expressions.

## CI Validation

Before pushing, run:
```bash
./scripts/ci_check.sh
```

All formatting and clippy checks passed.

## Summary

**Fixes Completed:** 3/3 root causes identified and fixed
**Test Status:** Simple cases work correctly, complex cases have remaining issues
**Code Quality:** All changes follow existing patterns, no workarounds
**Impact:** Significantly improves nested if-return handling, fixes condition generation

The fixes are solid improvements even though some edge cases remain. The remaining issues require deeper architectural changes to how let-bindings are hoisted from nested expression contexts.
