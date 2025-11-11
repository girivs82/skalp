# Metal Backend: Array Field Access Fix

## Status: PARTIALLY FIXED (2/4 locations) ⚠️

## Summary
Metal backend was attempting to access `.x/.y/.z/.w` components on array types (`uint[8]`), which is invalid in Metal. This occurred when the codegen assumed that signals with widths 33-128 bits were always vector types (`uint2`/`uint4`), but they can also be arrays.

## Errors Before Fix
```metal
signals->node_1165_out = registers->pipe4_hash.x;
// ERROR: member reference base type 'device uint[8]' is not a structure
```

## Root Cause
Multiple locations in `metal_codegen.rs` (lines 360, 1930, 2241, 2532) had code that assumed:
```rust
if source_width > 32 && source_width <= 128 {
    // Assumes source is uint2/uint4 vector
    format!("signals->{}[{}] = {}.{};\n", output, i, source, component_names[i])
    // Generates: signals->out[0] = registers->pipe4_hash.x  // ❌ ERROR if pipe4_hash is uint[8]
}
```

The code didn't check whether the source signal was actually a **vector type** (which supports `.x/.y/.z/.w`) or an **array type** (which requires `[i]` indexing).

## Solution (Applied to 2/4 Locations)
Modified the code to check the actual `SirType` of the source signal before deciding the access pattern:

```rust
// Check if source is actually a vector type
let source_sir_type = self.get_signal_sir_type(sir, signal);
let source_is_vector = matches!(
    source_sir_type,
    Some(SirType::Vec2(_)) | Some(SirType::Vec3(_)) | Some(SirType::Vec4(_))
) && (source_width == 64 || source_width == 96 || source_width == 128);

if source_is_vector {
    // Source is float2/float3/float4 or uint2/uint4 - use component access
    format!("signals->{}[{}] = {}.{};\n", output, i, source, component_names[i])
    // Generates: signals->out[0] = registers->vec_data.x  ✓
} else {
    // Source is uint[N] array - use array indexing
    format!("signals->{}[{}] = {}[{}];\n", output, i, source, i)
    // Generates: signals->out[0] = registers->pipe4_hash[0]  ✓
}
```

## Locations Fixed

### 1. Line 1904-1998: Signal reference vector-to-array conversion
**Context**: Converting source signal (from inputs/registers/signals) to output array

**Before:**
```rust
// Assumes source is uint2/uint4
for i in 0..vector_components.min(4) {
    write!("signals->{}[{}] = {}.{};\n", output, i, source_location, component_names[i]);
}
```

**After:**
```rust
let source_is_vector = matches!(source_sir_type, Some(SirType::Vec2(_)) | ...);
if source_is_vector {
    // Use .x/.y/.z/.w access
    write!("signals->{}[{}] = {}.{};\n", output, i, source_location, component_names[i]);
} else {
    // Use [i] array indexing
    write!("signals->{}[{}] = {}[{}];\n", output, i, source_location, i);
}
```

### 2. Line 345-415: Output signal vector-to-array conversion
**Context**: Converting node output signal to output port array

**Before:**
```rust
// Assumes node output is uint2/uint4
for i in 0..vector_components.min(4) {
    write!("signals->{}[{}] = signals->{}.{};\n",
           output.name, i, node_output.signal_id, component_names[i]);
}
```

**After:**
```rust
let source_is_vector = matches!(source_sir_type, Some(SirType::Vec2(_)) | ...);
if source_is_vector {
    // Use .x/.y/.z/.w access
    write!("signals->{}[{}] = signals->{}.{};\n",
           output.name, i, node_output.signal_id, component_names[i]);
} else {
    // Use [i] array indexing
    write!("signals->{}[{}] = signals->{}[{}];\n",
           output.name, i, node_output.signal_id, i);
}
```

## Locations Remaining (TODO)

### 3. Line 2241: Additional output vector-to-array conversion
Similar pattern for additional outputs from nodes

### 4. Line 2532: (Need to investigate)
Additional location with component_names pattern

## Verification
After fix:
- ✅ Compiles successfully
- ⚠️ Still need to fix 2 more locations
- ⚠️ Need to test Metal shader compilation with actual GPU

## Related Issues
- Complements Metal backend type system fix (vec2/vec3/vec4 preservation)
- Part of comprehensive Metal backend type correctness improvements
- Addresses 3 of the 13 Metal type errors (register access category)

## Files Modified
- `/Users/girivs/src/hw/hls/crates/skalp-sir/src/metal_codegen.rs`:
  - Modified vector-to-array conversion logic (lines 345-415, 1904-1998)
  - Added source type checking before component access
  - Falls back to array indexing for non-vector types

## Debug Output
```
🎯 Vector->Array: pipe4_data (128 bits, 4 components) -> result (256 bits, uint[8])
// BUG FIX #65: Unpack 128-bit vector into 256-bit array
signals->result[0] = registers->pipe4_data.x;
...

🎯 Array->Array: pipe4_hash (256 bits, uint[8]) -> result (512 bits, uint[16])
// Metal Backend Fix: Copy 256-bit array to 512-bit array
signals->result[0] = registers->pipe4_hash[0];
signals->result[1] = registers->pipe4_hash[1];
...
```
