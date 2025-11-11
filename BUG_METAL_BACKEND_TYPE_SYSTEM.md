# Metal Backend Type System Fix

## Status: FIXED ✅

## Summary
The Metal backend was generating incorrect type mappings for vec2/vec3/vec4 structs. Instead of preserving them as native Metal vector types (`float2`, `float3`, `float4`), they were being flattened to bit arrays (`uint4`, `uint[8]`), causing type mismatch errors during Metal shader compilation.

## Errors Before Fix
```
error: assigning to 'float4' from incompatible type 'device uint4'
error: member reference base type 'const device uint[8]' is not a structure
```

These errors occurred because:
1. Vec4 structs were converted to `SirType::Bits(128)` instead of `SirType::Vec4(Float32)`
2. Metal codegen generated `uint4` for bit vectors, but the code tried to use them as `float4`
3. Code tried to access `.x/.y/.z/.w` fields on uint arrays, which don't support component access

## Root Cause
In the MIR to SIR type conversion (`mir_to_sir.rs`), the `convert_type()` function was handling `DataType::Struct` by flattening all struct types to `SirType::Bits(total_width)`, losing the vector type information:

**Before:**
```rust
DataType::Struct(struct_type) => {
    let total_width: usize = struct_type.fields.iter()
        .map(|f| self.convert_type(&f.field_type).width())
        .sum();
    SirType::Bits(total_width)  // ❌ Lost vector type info!
}
```

This meant:
- `vec2 { x: fp32, y: fp32 }` → `SirType::Bits(64)` → Metal `uint2` ❌
- `vec3 { x: fp32, y: fp32, z: fp32 }` → `SirType::Bits(96)` → Metal `uint4` (padded) ❌
- `vec4 { x: fp32, y: fp32, z: fp32, w: fp32 }` → `SirType::Bits(128)` → Metal `uint4` ❌

## Impact
- **Metal shader compilation failures**: Type mismatches prevented GPU simulation
- **Incorrect component access**: `.x/.y/.z/.w` access on bit arrays caused compiler errors
- **Type system confusion**: Float vector data stored as uint arrays required explicit bit casts
- **Device memory access errors**: `device uint4*` couldn't be assigned to `float4` variables

## Solution
Modified `convert_type()` in `/Users/girivs/src/hw/hls/crates/skalp-sir/src/mir_to_sir.rs` (lines 3205-3254) to detect vec2/vec3/vec4 structs and preserve them as proper vector types:

```rust
DataType::Struct(struct_type) => {
    // BUG FIX: Metal Backend - Detect vec2/vec3/vec4 structs and preserve them as vector types
    let struct_name_lower = struct_type.name.to_lowercase();

    // Check if this is a vec2/vec3/vec4 struct
    if (struct_name_lower == "vec2" || struct_name_lower == "vector2") && struct_type.fields.len() >= 2 {
        let elem_type = self.convert_type(&struct_type.fields[0].field_type);
        return SirType::Vec2(Box::new(elem_type)); ✓
    } else if (struct_name_lower == "vec3" || struct_name_lower == "vector3") && struct_type.fields.len() >= 3 {
        let elem_type = self.convert_type(&struct_type.fields[0].field_type);
        return SirType::Vec3(Box::new(elem_type)); ✓
    } else if (struct_name_lower == "vec4" || struct_name_lower == "vector4") && struct_type.fields.len() >= 4 {
        let elem_type = self.convert_type(&struct_type.fields[0].field_type);
        return SirType::Vec4(Box::new(elem_type)); ✓
    }

    // Other structs/tuples - calculate total width
    let total_width: usize = struct_type.fields.iter()
        .map(|f| self.convert_type(&f.field_type).width())
        .sum();
    SirType::Bits(total_width)
}
```

This ensures:
- `vec2 { x: fp32, y: fp32 }` → `SirType::Vec2(Float32)` → Metal `float2` ✓
- `vec3 { x: fp32, y: fp32, z: fp32 }` → `SirType::Vec3(Float32)` → Metal `float3` ✓
- `vec4 { x: fp32, y: fp32, z: fp32, w: fp32 }` → `SirType::Vec4(Float32)` → Metal `float4` ✓

## Metal Codegen Type Mapping
With the fix, Metal codegen (`metal_codegen.rs`) properly maps vector types:

```rust
SirType::Vec2(elem) => {
    let (base, _) = self.get_metal_type_parts(elem);
    (format!("{}2", base), String::new())  // float2, uint2, etc.
}
SirType::Vec3(elem) => {
    let (base, _) = self.get_metal_type_parts(elem);
    (format!("{}3", base), String::new())  // float3, uint3, etc.
}
SirType::Vec4(elem) => {
    let (base, _) = self.get_metal_type_parts(elem);
    (format!("{}4", base), String::new())  // float4, uint4, etc.
}
```

So `SirType::Vec4(Float32)` correctly becomes `float4` in Metal.

## Component Access
The Metal codegen includes code that unpacks vector components (lines 360-370):

```rust
// Unpack vector components into array elements
let component_names = ["x", "y", "z", "w"];
for i in 0..vector_components.min(4) {
    self.write_indented(&format!(
        "signals->{}[{}] = signals->{}.{};\n",
        output_name, i, source_signal, component_names[i]
    ));
}
```

This code relies on the source signal being a proper Metal vector type (like `float4`) that supports `.x/.y/.z/.w` access. With our fix, vec4 signals are typed as `float4`, so this component access works correctly.

## Verification
After fix:
- ✅ vec2/vec3/vec4 structs preserved as `SirType::Vec2/Vec3/Vec4` in SIR
- ✅ Metal codegen generates proper `float2/float3/float4` types
- ✅ Component access `.x/.y/.z/.w` works on Metal vector types
- ✅ No type mismatch errors in Metal shader compilation
- ✅ Device buffer assignments work correctly (`device float4*` → `float4`)

## Debug Output Example
```
🔧 Metal Backend Fix: Converting struct 'vec4' to SirType::Vec4(Float32)
🔧 Metal Backend Fix: Converting struct 'vec3' to SirType::Vec3(Float32)
```

## Files Modified
- `/Users/girivs/src/hw/hls/crates/skalp-sir/src/mir_to_sir.rs`:
  - Modified `convert_type()` to detect and preserve vec2/vec3/vec4 structs (lines 3207-3234)

## Related Issues
- This fixes Metal GPU simulation failures
- Enables proper floating-point vector operations on Metal backend
- Preserves type information through the MIR → SIR → Metal pipeline
- Complements Bug #9 fix (struct field operations in SystemVerilog)

## Test Cases
Successfully handles:
- vec2/vec3/vec4 with Float32 elements
- Component-wise vector operations (add, multiply, etc.)
- Vector field access (`.x`, `.y`, `.z`, `.w`)
- Device buffer operations with vector types
- Mixed vector and scalar operations

## Future Improvements
Consider:
- Supporting integer vector types (`vec4<bit[32]>`)
- Handling custom vector struct names beyond "vec2/3/4"
- Optimizing vector operations with Metal SIMD intrinsics
