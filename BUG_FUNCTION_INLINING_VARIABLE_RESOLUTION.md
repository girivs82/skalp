# SKALP Bug: Function Inlining Variable Resolution Failures

**Date:** 2025-11-06
**Severity:** MEDIUM - Pre-existing issue, non-fatal warnings during compilation
**Status:** 🟡 **INVESTIGATION NEEDED**

---

## Summary

During function inlining in HIR→MIR conversion, some variable references cannot be resolved in `variable_map` or `dynamic_variables`, causing expression conversion to return `None`. Despite these warnings, Metal shaders compile successfully and appear to function correctly.

---

## Symptoms

When compiling designs with complex function inlining (e.g., Karythra CLE), debug output shows:

```
[DEBUG] Variable not found in variable_map or dynamic_variables: HIR ID VariableId(17)
[DEBUG]   Current dynamic_variables: [...]
[DEBUG] Concat: failed to convert element, type: Discriminant(3)
[DEBUG] Block expression: result_expr conversion failed, type: Discriminant(15)
[DEBUG] Match: FAILED to convert arm expression, type: Discriminant(23)
[DEBUG] inline_function_call: FAILED to convert substituted expression to MIR
```

---

## Reproduction

```bash
cd /Users/girivs/src/hw/karythra
cargo test --release test_cle_add_8 -- --exact 2>&1 | grep -E "(Variable not found|FAILED to convert)"
```

**Expected:** No variable resolution failures
**Actual:** Multiple "Variable not found" and "FAILED to convert" warnings

---

## Analysis

### Code Location
- **File:** `/Users/girivs/src/hw/hls/crates/skalp-mir/src/hir_to_mir.rs`
- **Function:** `inline_function_call()` (lines 4183-4276)
- **Variable lookup:** Lines 2095-2117

### Flow
1. `inline_function_call()` substitutes function body with argument expressions
2. During conversion of substituted expression to MIR, some variables can't be resolved
3. `HirExpression::Variable` case returns `None` when variable not found (line 2117)
4. This propagates up through Block/Concat/Match expressions
5. Final result: `inline_function_call` returns `None`

### Missing Variables
Examples from CLE test:
- `VariableId(17)` - Not in variable_map or dynamic_variables
- `VariableId(48)` - Not in variable_map or dynamic_variables

### Impact Assessment
**Current Impact: LOW** (but requires investigation)

✅ **What Still Works:**
- Metal shader compilation succeeds
- Generated shader code is complete and valid
- Tests run (only fail on unrelated test infrastructure issues)
- Bug #66 and Bug #67 tests pass

❓ **Questions:**
1. Why are these variables not in the maps?
2. Is this dead code elimination at work?
3. Are these unreachable code paths that can be safely skipped?
4. Is there fallback logic handling the `None` return value?

---

## Potential Root Causes

1. **Dead Code Paths**
   - Variables referenced in unreachable match arms or conditionals
   - Function inlining attempts to process all paths, including dead ones
   - Fallback logic skips dead code when variable resolution fails

2. **Variable Scope Issues**
   - Variables defined in one scope but referenced in another during substitution
   - Nested function inlining creating complex scope chains
   - Variable cleanup (line 4271-4273) removing variables too early

3. **Overly Aggressive Inlining**
   - Attempting to inline functions in contexts where variable substitution is incomplete
   - Missing variable mappings for certain edge cases

---

## Investigation Steps

1. **Add Variable Tracking**
   - Log which variables are being added to `variable_map` and `dynamic_variables`
   - Track when variables are removed (scope cleanup)
   - Identify why VariableId(17) and VariableId(48) aren't registered

2. **Analyze Failed Expressions**
   - Examine the HIR expressions that fail to convert
   - Determine if they're in reachable code paths
   - Check if there's fallback handling for `None` returns

3. **Test Impact**
   - Create test cases that exercise the code paths with missing variables
   - Verify computation results are correct despite warnings
   - Compare Metal shader output with and without these warnings

4. **Check Caller Handling**
   - Trace what happens when `inline_function_call()` returns `None`
   - Verify fallback code generation exists
   - Ensure no silent failures in computation

---

## Workaround

None needed - compilation succeeds despite warnings.

---

## Related Code

### Variable Lookup (hir_to_mir.rs:2095-2117)
```rust
hir::HirExpression::Variable(id) => {
    if let Some(&mir_id) = self.variable_map.get(id) {
        Some(Expression::Ref(LValue::Variable(mir_id)))
    } else if let Some((mir_id, name, _)) = self.dynamic_variables.get(id) {
        Some(Expression::Ref(LValue::Variable(*mir_id)))
    } else {
        eprintln!("[DEBUG] Variable not found...");
        None
    }
}
```

### Function Inlining Return (hir_to_mir.rs:4261-4266)
```rust
let result = self.convert_expression(&substituted_expr);
if result.is_none() {
    eprintln!("[DEBUG] inline_function_call: FAILED to convert...");
}
result  // Returns None if conversion failed
```

### Variable Cleanup (hir_to_mir.rs:4268-4273)
```rust
for var_id in var_id_to_name.keys() {
    self.variable_map.remove(var_id);
}
```

---

## Verification After Fix

Once resolved, these tests should show no warnings:

```bash
cd /Users/girivs/src/hw/karythra
cargo test --release test_cle_add_8 2>&1 | grep -E "(Variable not found|FAILED to convert)"
# Should return no matches

cd /Users/girivs/src/hw/hls
cargo test --release test_bug67 2>&1 | grep -E "(Variable not found|FAILED to convert)"
# Should return no matches
```

---

## Notes

- This issue was discovered during investigation of the tuple type fix for the zero-results bug
- The warnings are **pre-existing** - they existed before the tuple fix
- The tuple fix successfully resolves the zero-results bug and should be committed separately
- This issue does not block the tuple fix

---

**Next Steps:**
1. Log variable registration/removal to understand why VariableId(17) and VariableId(48) are missing
2. Analyze the HIR expressions that reference these variables
3. Determine if fallback logic exists and is working correctly
4. If this is intentional dead code elimination, document it and potentially reduce warning noise
