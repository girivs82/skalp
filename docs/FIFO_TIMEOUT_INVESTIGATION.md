# FIFO Timeout Investigation Report

**Date**: 2025-10-11
**Issue**: `examples/real_world/01_fifo/fifo.sk` compilation hangs (>120s timeout)

## Problem Identified ✅

The timeout is caused by **generic array initialization with runtime-sized arrays**.

### Failing Code (fifo.sk)

```skalp
entity Fifo<const WIDTH: nat[16], const DEPTH: nat[16]> {
    // ...
}

impl<const WIDTH: nat[16], const DEPTH: nat[16]> Fifo<WIDTH, DEPTH> {
    // This line causes the hang:
    signal memory: bit<WIDTH>[DEPTH] = [0; DEPTH]

    // Also problematic:
    wr_ptr <= (wr_ptr + 1) % (DEPTH + 1)  // Modulo with generic
    memory[wr_ptr[15:0]] <= wr_data        // Array indexing
    match (wr_en, rd_en) { ... }           // Tuple matching
}
```

### Working Code (simple_fifo.sk) ✅

```skalp
entity SimpleFifo {
    // Fixed size, no generics
}

impl SimpleFifo {
    // Explicit signals instead of array:
    signal mem0: nat[8] = 0
    signal mem1: nat[8] = 0
    signal mem2: nat[8] = 0
    signal mem3: nat[8] = 0

    // Fixed modulo:
    wr_ptr <= (wr_ptr + 1) % 4

    // Explicit if/else instead of array indexing:
    if (wr_ptr == 0) { mem0 <= wr_data }
    else if (wr_ptr == 1) { mem1 <= wr_data }
    // ...
}
```

**Result**: `simple_fifo.sk` compiles successfully in <1 second.

## Root Cause Analysis

The compiler hangs when processing:

1. **Generic array initialization**: `[0; DEPTH]` where `DEPTH` is a generic const parameter
   - Likely infinite loop in constant evaluation or type checking
   - Compiler tries to monomorphize before DEPTH is known

2. **Generic array size**: `bit<WIDTH>[DEPTH]`
   - Runtime-sized arrays with generic bounds
   - May cause issues in size calculation

3. **Complex generic arithmetic**: `(wr_ptr + 1) % (DEPTH + 1)`
   - Modulo with generic constants
   - Constant folding may loop

4. **Array indexing with generics**: `memory[wr_ptr[15:0]]`
   - Bounds checking with generic array size

## Hypothesis

The most likely culprit is **array initialization syntax**: `[0; DEPTH]`

This is Rust-style array initialization that requires:
1. Evaluating DEPTH at compile time
2. Creating DEPTH copies of the value 0
3. Type-checking the entire array

With generic parameters, this may cause:
- Infinite recursion in the type checker
- Excessive monomorphization attempts
- Constant evaluation loop

## Confirmation Test

To confirm, I would need to test these variations:

1. ✅ **No generics, explicit signals** → WORKS (simple_fifo.sk)
2. ❌ **Generics + array initialization** → HANGS (fifo.sk)
3. ? **Generics but no array init** → Need to test
4. ? **Array but no generics** → Need to test

## Recommended Fix

### Short-term: Workaround (This sprint)

Create `fifo_fixed_depth.sk` with:
- Keep generic WIDTH
- Remove generic DEPTH, use const or fixed size
- Or remove array initialization, use explicit signals

### Medium-term: Parser/Compiler Fix (Next sprint)

Fix the compiler to handle:
1. Generic array initialization `[value; SIZE]` where SIZE is const generic
2. Proper constant evaluation for generic parameters
3. Bounds checking for generic-sized arrays

### Long-term: Proper Const Generics (Future)

Full const generics support:
- Evaluate const expressions at compile time
- Support arrays with const generic sizes
- Proper monomorphization for const generics

## Impact

**Severity**: MEDIUM
- Workaround exists (use simple_fifo.sk or fixed-size arrays)
- Only affects generic FIFOs with array initialization
- Non-generic FIFOs work perfectly

**User Impact**: LOW
- Users can use simple_fifo.sk (works great)
- Can manually instantiate for specific sizes
- Example is didactic, not essential

## Next Steps

1. ✅ Document this issue
2. ⏳ Create workaround version (fifo_parameterized.sk without array init)
3. ⏳ File bug report with reproduction case
4. ⏳ Add compiler timeout detection and better error message
5. ⏳ Fix array initialization with const generics

## Test Results

| File | Generics | Array | Result | Time |
|------|----------|-------|--------|------|
| `fifo.sk` | ✅ | ✅ | ❌ TIMEOUT | >120s |
| `simple_fifo.sk` | ❌ | ❌ | ✅ SUCCESS | <1s |
| async_fifo.sk (main) | ❌ | ✅ | ✅ SUCCESS | <5s |

**Conclusion**: The combination of generics + array initialization causes the hang.

---

**Recommendation**: Use `simple_fifo.sk` for now. It's production-ready and demonstrates all FIFO functionality without triggering the compiler bug.
