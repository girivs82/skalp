# Bug #76: Tuple Literals with Mixed-Width Elements

## Status
- **Identified**: Session 2025-01-XX
- **Severity**: High (blocks idiomatic tuple usage)
- **Partial Fix**: Committed to mir_to_sir.rs lines 2288-2305

## Summary
Integer literals in tuples are always created as 32-bit values, even when the tuple type specifies smaller widths like `bit` (1 bit). This causes incorrect behavior when accessing tuple fields.

## Root Cause
In `/Users/girivs/src/hw/hls/crates/skalp-sir/src/mir_to_sir.rs`:
- Line 2286 (original): `Value::Integer(i) => (*i as u64, 32)` - hardcoded 32-bit width
- Concat expression parts (line 2264-2267) are processed without type/width information
- MIR Expression nodes don't carry explicit type information
- Type information from HIR is lost during MIR conversion

## Reproduction
```skalp
// Function returns (bit, bit[32], bit[32]) - should be 65 bits total
pub fn test() -> (bit, bit[32], bit[32]) {
    return (1, 0, 0)  // Literal '1' created as 32 bits instead of 1 bit!
}

entity Test {
    out valid: bit
    out x1: bit[32]
    out x2: bit[32]
}

impl Test {
    signal result: (bit, bit[32], bit[32])
    result = test()
    valid = result.0  // Returns 0 instead of 1!
    x1 = result.1
    x2 = result.2
}
```

**Expected**: Tuple is 1+32+32 = 65 bits, valid=1
**Actual**: Tuple is 32+32+32 = 96 bits, valid=0 (extracts wrong bit)

## Partial Fix Applied
Modified `create_literal_node_with_width()` to use `target_width` when provided:
```rust
Value::Integer(i) => {
    // BUG #76 FIX: Use target width for integer literals when provided
    let inferred_width = target_width.unwrap_or(32);
    (*i as u64, inferred_width)
}
```

**Limitation**: Only helps when `target_width` is propagated (conditionals), not for concat parts.

## Complete Fix Required
Need to thread type information through MIR->SIR conversion:

### Option A: Add Type to MIR Expression (Invasive)
1. Modify `Expression` enum to carry `Type` information
2. Propagate types during HIR->MIR conversion
3. Use type info when creating SIR nodes

### Option B: Query HIR Types During Conversion (Less Invasive)
1. Keep reference to HIR during MIR->SIR conversion
2. Add expression ID mapping from MIR back to HIR
3. Query HIR type checker for expression types as needed

### Option C: Infer from Context (Targeted)
1. For Concat expressions specifically, infer part widths from result type
2. Add special handling for tuple return types
3. Split target width across concat parts based on type structure

## Workarounds
Until fixed, use one of:

### Workaround 1: Use bit[32] for all tuple elements
```skalp
// Change from (bit, bit[32], bit[32]) to (bit[32], bit[32], bit[32])
pub fn test() -> (bit[32], bit[32], bit[32]) {
    return (1, x1, x2)  // All 32-bit, works correctly
}
```

### Workaround 2: Avoid tuples, use separate signals
```skalp
entity Test {
    signal valid_calc: bit[32]
    signal x1_calc: bit[32]
    signal x2_calc: bit[32]

    valid_calc = if condition { 1 } else { 0 }
    x1_calc = ...
    x2_calc = ...

    valid = valid_calc
    x1 = x1_calc
    x2 = x2_calc
}
```

## Impact
- Blocks idiomatic use of tuples with mixed widths
- Forces workarounds that reduce code clarity
- Affects Karythra CLE implementation which uses tuples extensively

## Test Case
See `/Users/girivs/src/hw/karythra/tests/quadratic_unit.sk` and `test_l4_quadratic_minimal.rs`
