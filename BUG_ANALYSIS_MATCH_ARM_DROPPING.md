# CRITICAL BUG ANALYSIS: Silent Match Arm Dropping

**Date**: 2025-10-21
**Severity**: CRITICAL - Silent Data Corruption
**Impact**: Karythra CLE (29% functionality loss), All match expressions with block arms

---

## Executive Summary

SKALP silently drops match arms containing block expressions with `let` bindings during HIR construction, with NO error or warning. This causes silent data corruption in generated hardware.

**Evidence**: Test case with 7 match arms compiles successfully but only generates 5 arms in output.

---

## Root Cause

**File**: `crates/skalp-frontend/src/hir_builder.rs`
**Function**: `build_match_arm_expr`
**Lines**: 4104-4118

```rust
// Code that finds the arm expression after the `=>` arrow
let expr_nodes_after_arrow: Vec<_> = node
    .children_with_tokens()
    .skip_while(|e| { /* skip until FatArrow */ })
    .skip(1) // Skip the arrow itself
    .filter_map(|e| {
        e.as_node()
            .filter(|n| {
                matches!(
                    n.kind(),
                    SyntaxKind::LiteralExpr
                        | SyntaxKind::IdentExpr
                        | SyntaxKind::BinaryExpr
                        | SyntaxKind::UnaryExpr
                        | SyntaxKind::FieldExpr
                        | SyntaxKind::IndexExpr
                        | SyntaxKind::PathExpr
                        | SyntaxKind::ParenExpr
                        | SyntaxKind::CallExpr
                        | SyntaxKind::ArrayLiteral
                        | SyntaxKind::IfExpr
                        | SyntaxKind::MatchExpr
                        // ❌ SyntaxKind::BlockExpr IS MISSING!
                )
            })
            .cloned()
    })
    .collect();

let expr_node = expr_nodes_after_arrow.last()?; // ❌ Returns None for BlockExpr!
let expr = self.build_expression(expr_node)?;
```

**The Problem:**
1. Block expressions are NOT in the filter list
2. When a match arm has a block expression, it's filtered out
3. `expr_nodes_after_arrow.last()` returns `None`
4. Function returns `None` for the entire arm
5. Caller in `build_match_expr` (line 3989-3991) **silently drops** arms that return `None`:

```rust
for arm_node in arm_list.children().filter(|n| n.kind() == SyntaxKind::MatchArm) {
    if let Some(arm) = self.build_match_arm_expr(&arm_node) {
        arms.push(arm);  // ❌ If None, arm is silently dropped!
    }
}
```

---

## Reproduction Test Case

**File**: `/tmp/test_match_arm_dropping.sk`

```skalp
pub fn test_match_arms(opcode: bit[4], a: bit[8], b: bit[8]) -> bit[8] {
    return match opcode {
        0 => a + b,           // ✓ Works - simple expression
        1 => a - b,           // ✓ Works - simple expression
        2 => {                // ❌ DROPPED - block with let binding
            let shifted = a >> b[2:0];
            shifted
        },
        3 => a & b,           // ✓ Works - simple expression
        4 => {                // ❌ DROPPED - block with let binding
            let sign = a[7];
            if sign { 0xFF } else { 0x00 }
        },
        5 => a | b,           // ✓ Works - simple expression
        _ => 0
    }
}
```

**Expected**: 7 arms (opcodes 0,1,2,3,4,5, wildcard)
**Actual**: 5 arms (opcodes 0,1,3,5, wildcard) - arms 2 and 4 silently dropped

**Build Output**:
```
[DEBUG] substitute_expression_with_var_map: Match expression with 5 arms
[DEBUG] Match: processing arm 0, pattern: Literal(Integer(0))
[DEBUG] Match: processing arm 1, pattern: Literal(Integer(1))
[DEBUG] Match: processing arm 2, pattern: Literal(Integer(3))  ❌ Should be 2!
[DEBUG] Match: processing arm 3, pattern: Literal(Integer(5))  ❌ Should be 3!
[DEBUG] Match: processing arm 4, pattern: Wildcard
✅ Build complete!  ❌ FALSE SUCCESS!
```

---

## Impact Assessment

### Karythra CLE
- **12/41 operations** functional (29%)
- **29/41 operations** disabled due to dropped match arms
- L0-L1: 3 operations broken (SRA, LTU, GEU)
- L2: All 10 FP operations non-functional
- L3: All 6 vector operations non-functional
- L4-L5: All 7 specialized operations non-functional

### General Impact
- **ANY match expression with block arms is broken**
- **Silent data corruption** - no compile error
- **False success reporting** - build shows "✅ Build complete!"
- **Production safety risk** - incorrect hardware behavior

---

## Immediate Fixes Required

### Fix 1: Add BlockExpr to Filter (CRITICAL - Immediate)

**File**: `crates/skalp-frontend/src/hir_builder.rs:4104-4118`

```rust
// BEFORE (broken):
matches!(
    n.kind(),
    SyntaxKind::LiteralExpr
        | SyntaxKind::IdentExpr
        | SyntaxKind::BinaryExpr
        | SyntaxKind::UnaryExpr
        | SyntaxKind::FieldExpr
        | SyntaxKind::IndexExpr
        | SyntaxKind::PathExpr
        | SyntaxKind::ParenExpr
        | SyntaxKind::CallExpr
        | SyntaxKind::ArrayLiteral
        | SyntaxKind::IfExpr
        | SyntaxKind::MatchExpr
)

// AFTER (fixed):
matches!(
    n.kind(),
    SyntaxKind::LiteralExpr
        | SyntaxKind::IdentExpr
        | SyntaxKind::BinaryExpr
        | SyntaxKind::UnaryExpr
        | SyntaxKind::FieldExpr
        | SyntaxKind::IndexExpr
        | SyntaxKind::PathExpr
        | SyntaxKind::ParenExpr
        | SyntaxKind::CallExpr
        | SyntaxKind::ArrayLiteral
        | SyntaxKind::IfExpr
        | SyntaxKind::MatchExpr
        | SyntaxKind::BlockExpr  // ← ADD THIS!
)
```

This will prevent arm dropping, but arms with `let` bindings will still fail during function inlining.

### Fix 2: Emit Compile Error for Failed Arms (CRITICAL - Immediate)

**File**: `crates/skalp-frontend/src/hir_builder.rs:3989-3992`

```rust
// BEFORE (silently drops):
for arm_node in arm_list.children().filter(|n| n.kind() == SyntaxKind::MatchArm) {
    if let Some(arm) = self.build_match_arm_expr(&arm_node) {
        arms.push(arm);
    }
}

// AFTER (emits error):
for arm_node in arm_list.children().filter(|n| n.kind() == SyntaxKind::MatchArm) {
    match self.build_match_arm_expr(&arm_node) {
        Some(arm) => arms.push(arm),
        None => {
            self.errors.push(HirError {
                message: format!(
                    "Failed to build match arm - unsupported pattern or expression"
                ),
                span: arm_node.text_range(),
            });
            return None;  // Fail compilation
        }
    }
}
```

---

## Medium-Term Fixes (Phase 2)

### Fix 3: Support Let Bindings in Match Arm Blocks

**Required Changes**:

1. **HIR Builder** (`hir_builder.rs:build_block_expr`):
   - Already supports block expressions with let bindings
   - No changes needed here

2. **Function Inlining** (`hir_to_mir.rs:substitute_expression_with_var_map`):
   - Block expression substitution at line 3064-3127 needs enhancement
   - Currently fails when let statements reference parameters
   - Need to substitute params in let statement RHS expressions

3. **MIR Conversion** (`hir_to_mir.rs:convert_expression`):
   - Block expression conversion at line 2356-2406 needs enhancement
   - Need to handle let bindings in combinational context
   - Generate intermediate signals for let bindings

**Implementation Strategy**:
- Let bindings in match arms should become **intermediate expressions**
- Each let binding contributes to a chain of combinational logic
- Final expression uses all intermediate values
- Example:
  ```skalp
  match x {
      2 => {
          let shifted = a >> 1;
          let masked = shifted & 0x7F;
          masked
      }
  }
  ```
  Should generate:
  ```systemverilog
  assign _tmp_shifted = (x == 2) ? (a >> 1) : ...;
  assign _tmp_masked = (x == 2) ? (_tmp_shifted & 7'h7F) : ...;
  assign result = (x == 2) ? _tmp_masked : ...;
  ```

---

## Long-Term Improvements (Phase 3)

### Fix 4: Better Error Messages

Add specific error messages for each failure mode:
- "Match arm contains block expression with let bindings (not yet supported)"
- "Match arm expression contains unsupported syntax: ..."
- "Failed to inline function with complex control flow"

### Fix 5: Error Tracking System

Add compilation error tracking:
- Track all errors during HIR/MIR/SIR conversion
- Return non-zero exit code if any errors occurred
- Don't print "✅ Build complete!" if there were errors
- Print error summary at end of build

---

## Testing Strategy

### Test 1: Verify Fix 1 (BlockExpr filtering)
```bash
./target/release/skalp build -s /tmp/test_match_arm_dropping.sk -o /tmp/out
# Should now process all 7 arms (but may fail during function inlining)
```

### Test 2: Verify Fix 2 (Error reporting)
```bash
./target/release/skalp build -s /tmp/test_match_arm_dropping.sk -o /tmp/out
# Should emit compile error instead of silent success
```

### Test 3: Verify Fix 3 (Let binding support)
```bash
./target/release/skalp build -s /tmp/test_match_arm_dropping.sk -o /tmp/out
# Should successfully compile and generate correct SV
```

### Test 4: Karythra CLE Integration
```bash
cd /Users/girivs/src/hw/karythra/rtl/skalp/cle
skalp build -s src/main.sk -o /tmp/cle_test
# Should compile all 41 function units successfully
```

---

## Priority Ranking

1. **CRITICAL (Immediate)**: Fix 1 + Fix 2 - Prevent silent arm dropping
2. **HIGH (Week 1)**: Fix 3 - Support let bindings in match arms
3. **MEDIUM (Week 2)**: Fix 4 - Better error messages
4. **MEDIUM (Week 2)**: Fix 5 - Error tracking system

---

## References

- Feature requests: `/Users/girivs/src/hw/karythra/SKALP_FEATURE_REQUESTS.md`
- Build analysis: `/Users/girivs/src/hw/karythra/SKALP_BUILD_ANALYSIS.md`
- Test case: `/tmp/test_match_arm_dropping.sk`
- Karythra CLE: `/Users/girivs/src/hw/karythra/rtl/skalp/cle/src/main.sk`
- SKALP known issues: `/Users/girivs/src/hw/hls/KNOWN_ISSUES.md`
