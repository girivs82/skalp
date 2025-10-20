# Block Expression Bug Investigation

**Date**: 2025-10-21
**Status**: IN PROGRESS - Root cause partially identified
**Files Modified**: `crates/skalp-mir/src/hir_to_mir.rs`

## Summary

Investigating why block expressions with let bindings in match arms produce incorrect SystemVerilog output. Variables are declared and assigned correctly, but variable REFERENCES in conditional expressions are incorrectly mapped to the wrong variables.

## Bug Manifestation

### Example Code (Karythra CLE func_units_l0_l1.sk:45-55):
```skalp
9 => {
    let sign = a[31];
    let shifted = a >> b[4:0];
    let shift_amt = b[4:0];
    if sign && shift_amt > 0 {
        let mask = (0xFFFFFFFF << (32 - shift_amt));
        shifted | mask
    } else {
        shifted
    }
},
```

### Generated SystemVerilog (INCORRECT):
```systemverilog
logic [31:0] sign;
logic [31:0] shifted;
logic [31:0] shift_amt;
logic [31:0] mask;

assign sign = pipe2_data1[31];
assign shifted = (pipe2_data1 >> pipe2_data2[4:0]);
assign shift_amt = pipe2_data2[4:0];
assign mask = (4294967295 << (32 - shift_amt));

// BUG: Uses shift_amt instead of sign!
((pipe2_func == 9) ? ((shift_amt && (shift_amt > 0)) ? (shifted | mask) : shifted) : ...)
```

**Expected**: `((sign && (shift_amt > 0)) ? ...`
**Actual**: `((shift_amt && (shift_amt > 0)) ? ...`

## Investigation Progress

### ✅ What's Working

1. **Variable Declaration**: All 4 let bindings (sign, shifted, shift_amt, mask) ARE declared in SystemVerilog
2. **Variable Assignment**: All 4 variables ARE assigned with correct values
3. **Recursive Collection** (NEW): Implemented `collect_let_bindings()` and `collect_let_bindings_from_expr()` to recursively find ALL let bindings in nested structures (lines 3511-3570)

### ❌ What's Broken

**Variable References in Conditionals**: When a conditional expression references a let-bound variable, it gets mapped to the WRONG MIR variable ID.

Test case `/tmp/test_three_lets.sk`:
```skalp
_ => {
    let x = a[7];
    let y = a + b;
    let z = b[4:0];
    if x && z > 0 {  // Should use 'x'
        let w = 0xFF << z;
        y | w
    } else {
        y
    }
}
```

Generates: `((z && (z > 0)) ? ...` instead of `((x && (z > 0)) ? ...`

## Root Cause Analysis

### Phase 1: HIR Substitution (WORKING ✅)

The function `inline_function_call` (line 3572):
1. Collects let bindings recursively → **4 entries found** ✅
2. Substitutes function parameters → **Working** ✅
3. Variables `x`, `y`, `z`, `w` are processed during substitution ✅

Debug output shows:
```
[DEBUG] inline_function_call: built var_id map with 4 entries (recursive)
[DEBUG] Block: substituting let statement 0 (x)
[DEBUG] Block: substituting let statement 1 (y)
[DEBUG] Block: substituting let statement 2 (z)
[DEBUG] Block: substituting let statement 3 (w)
```

### Phase 2: MIR Conversion (BROKEN ❌)

When converting `HirExpression::Variable` to MIR (line 3027-3097):
1. HIR variable ID is looked up in `variable_map`
2. **BUG**: Multiple different HIR variables incorrectly map to the same MIR variable ID
3. Result: `x` (VariableId(0)) somehow resolves to the same MIR variable as `z` (VariableId(2))

Debug evidence:
```
[DEBUG] Variable: processing var_id=VariableId(2)
[DEBUG] Variable: var_id maps to name 'z'
[DEBUG] Variable: name 'z' not found in param_map (keys: ["b", "a", "sel"])
[DEBUG] Variable: keeping as-is (not in substitution map)
```

Notice: VariableId(0) (`x`) is NEVER seen during variable processing, yet `x` is declared and assigned in the output!

## Hypothesis

The issue is in how **pending statements** from block expressions are converted to MIR:

1. When `convert_expression` encounters a Block expression (line 2357-2389):
   - It pushes let statements to `self.pending_statements`
   - Each statement becomes a MIR `Statement::Assignment`

2. Later, pending statements are drained (line 309) and added to the module

3. **Suspected Bug**: When creating MIR variable IDs for these assignments:
   - The `variable_map` (HIR var ID → MIR var ID) may be incorrectly populated
   - Multiple HIR variables might be assigned the same MIR variable ID
   - Or the MIR variable ID counter might be reused/reset improperly

## CRITICAL FINDING

**Added Debug Output** (line 1877-1886):
```rust
hir::HirExpression::Variable(id) => {
    eprintln!("[DEBUG] Converting Variable({:?}) to MIR", id);
    if let Some(&mir_id) = self.variable_map.get(id) {
        eprintln!("[DEBUG]   Found in variable_map: MIR Variable({})", mir_id.0);
        ...
```

**Test Output** (`/tmp/test_three_lets.sk`):
```
[DEBUG] Converting Variable(VariableId(2)) to MIR   <- z
[DEBUG] Converting Variable(VariableId(2)) to MIR   <- z again!
[DEBUG] Converting Variable(VariableId(2)) to MIR   <- z third time!
[DEBUG] Converting Variable(VariableId(1)) to MIR   <- y
[DEBUG] Converting Variable(VariableId(3)) to MIR   <- w
[DEBUG] Converting Variable(VariableId(1)) to MIR   <- y
```

**SMOKING GUN**: VariableId(0) (`x`) is NEVER converted to MIR!

The conditional should reference: `x && z > 0` which should be `VariableId(0) && VariableId(2) > 0`
But instead it's referencing: `z && z > 0` which is `VariableId(2) && VariableId(2) > 0`

**Conclusion**: The bug is NOT in MIR conversion. The HIR already has the wrong variable ID before MIR conversion starts. This must be happening during:
1. Function inlining substitution OR
2. Block expression construction with `build_let_expression` OR
3. The original HIR parsing/construction

## Next Steps

1. **Add Debug to build_let_expression**:
   - Check what variable IDs are in the let statements vs result expression

2. **Check Variable Map State**:
   - Print `variable_map` contents after converting each block expression
   - Verify that each unique HIR VariableId maps to a unique MIR VariableId

3. **Examine Pending Statement Conversion**:
   - In `convert_statement` for Let statements (likely around line 400-500)
   - Check how the LValue is created: `LValue::Variable(mir_var_id)`
   - Verify the mir_var_id is unique and correctly stored in variable_map

## Files to Examine

1. `hir_to_mir.rs:convert_statement` - How let statements become MIR assignments
2. `hir_to_mir.rs:convert_expression` for Block (line 2357) - How pending statements are populated
3. `hir_to_mir.rs:convert_expression` for Variable (line 3027) - How variable references are resolved
4. `hir_to_mir.rs:variable_map` usage - Check all places where entries are added/retrieved

## Test Cases

Minimal reproducible test: `/tmp/test_three_lets.sk`

Expected output:
```systemverilog
assign result = ((sel == 0) ? a : ((x && (z > 0)) ? (y | w) : y));
```

Actual output:
```systemverilog
assign result = ((sel == 0) ? a : ((z && (z > 0)) ? (y | w) : y));
```

## Changes Made

**File**: `crates/skalp-mir/src/hir_to_mir.rs`

**Lines 3511-3570**: Added recursive let binding collection:
- `collect_let_bindings()` - Recursively collect from statements
- `collect_let_bindings_from_expr()` - Recursively collect from expressions
- Handles: Block, Match, If expressions
- Handles: Let, Return, If statements

**Lines 3626-3633**: Modified `inline_function_call`:
```rust
// OLD: Only collected top-level let statements
for stmt in &body {
    if let hir::HirStatement::Let(let_stmt) = stmt {
        var_id_to_name.insert(let_stmt.id, let_stmt.name.clone());
    }
}

// NEW: Recursively collect ALL let bindings
let mut var_id_to_name = HashMap::new();
self.collect_let_bindings(&body, &mut var_id_to_name);
```

**Result**: var_id_to_name now has 4 entries instead of 0, but bug persists. This proves the issue is NOT in HIR substitution.

## FINAL ROOT CAUSE IDENTIFIED - 2025-10-21

**Location**: Parser (not HIR builder, not MIR converter)  
**File**: crates/skalp-frontend/src/parser.rs (or grammar definition)

### The Bug

The parser incorrectly creates the AST for if expression conditions containing binary logical operators. For an expression like `x && z > 0`, the parser creates:

**WRONG** (Current):
```
IfExpr
  ├─ IdentExpr(x)          ← First expression child
  ├─ BinaryExpr(z > 0)     ← Second expression child (siblings!)
  └─ BlockExpr { ... }
```

**CORRECT** (Expected):
```
IfExpr
  ├─ BinaryExpr(&&)
  │  ├─ IdentExpr(x)
  │  └─ BinaryExpr(>)
  │     ├─ IdentExpr(z)
  │     └─ Literal(0)
  └─ BlockExpr { ... }
```

### Evidence

Debug output from `hir_builder.rs:3821`:
```
[HIR] build_if_expr: found potential condition node kind=IdentExpr
[HIR] build_if_expr: found potential condition node kind=BinaryExpr  
```

Two separate expression nodes found as children of the IfExpr node!

### Impact

Affects any if expression with:
- Compound boolean conditions using `&&` or `||`  
- Especially noticeable in match arm block expressions with multiple let bindings

Real-world failure: Karythra CLE operations (SRA, LTU, GEU) return incorrect values

### Attempted Workaround

`hir_builder.rs` lines 3841-3882: Detect multiple expression nodes and combine with `LogicalAnd`

**Result**: Partial success but still incorrect due to recursive issue - generates `x && (z && (z > 0))` instead of `x && (z > 0)`

### Proper Fix Required

The parser grammar needs to be fixed to correctly build binary expression trees for logical operators in if conditions. This is beyond the scope of an HIR builder patch.

### Test Cases

Minimal reproducible: `/tmp/test_three_lets.sk`  
Real-world: `/Users/girivs/src/hw/karythra/rtl/skalp/cle/lib/func_units_l0_l1.sk` lines 45-55 (SRA operation)

