# Modifications Required to Compile Complex Example Project

This document lists every modification made to get the showcase example to compile.

## File: async_fifo.sk

### Modification 1: Remove let bindings from on() blocks

**Location:** Lines 70-77 (write logic)

**Original:**
```skalp
if wr_en && !wr_full {
    let wr_addr = wr_ptr % DEPTH
    mem[wr_addr] <= wr_data

    let next_wr_ptr = wr_ptr + 1
    wr_ptr <= next_wr_ptr
    // Convert to Gray code: G = B XOR (B >> 1)
    wr_ptr_gray <= next_wr_ptr ^ (next_wr_ptr >> 1)
}
```

**Modified:**
```skalp
if wr_en && !wr_full {
    mem[wr_ptr % DEPTH] <= wr_data
    wr_ptr <= wr_ptr + 1
    // Convert to Gray code: G = B XOR (B >> 1)
    wr_ptr_gray <= (wr_ptr + 1) ^ ((wr_ptr + 1) >> 1)
}
```

**Reason:** Let bindings not supported inside on() blocks
**Impact:**
- Expression `wr_ptr + 1` duplicated 3 times (error-prone)
- Less readable
- Gray code computation harder to understand

---

### Modification 2: Remove let bindings from on() blocks (read side)

**Location:** Lines 105-108 (read logic)

**Original:**
```skalp
if rd_en && !rd_empty {
    let next_rd_ptr = rd_ptr + 1
    rd_ptr <= next_rd_ptr
    rd_ptr_gray <= next_rd_ptr ^ (next_rd_ptr >> 1)
}
```

**Modified:**
```skalp
if rd_en && !rd_empty {
    rd_ptr <= rd_ptr + 1
    rd_ptr_gray <= (rd_ptr + 1) ^ ((rd_ptr + 1) >> 1)
}
```

**Reason:** Let bindings not supported inside on() blocks
**Impact:**
- Expression `rd_ptr + 1` duplicated 2 times
- Same maintainability issues

---

**async_fifo.sk Result:** ✅ Compiles after modifications

---

## File: geometry_processor.sk

### Modification 3: Remove helper functions from impl block

**Location:** Lines 202-259 (all helper functions)

**Original:**
```skalp
impl GeometryProcessor<const STAGES: nat> {
    signal world_pos: Vec4
    // ... other signals and logic ...

    /// Convert Vec3 to Vec4 with w component
    fn vec3_to_vec4(v: Vec3, w: fp32) -> Vec4 {
        Vec4 { x: v.x, y: v.y, z: v.z, w: w }
    }

    /// Matrix-vector multiplication (4x4 * 4x1)
    fn matrix_mul_vec4(m: Matrix4x4, v: Vec4) -> Vec4 {
        Vec4 {
            x: m.col0.x * v.x + m.col1.x * v.y + m.col2.x * v.z + m.col3.x * v.w,
            y: m.col0.y * v.x + m.col1.y * v.y + m.col2.y * v.z + m.col3.y * v.w,
            z: m.col0.z * v.x + m.col1.z * v.y + m.col2.z * v.z + m.col3.z * v.w,
            w: m.col0.w * v.x + m.col1.w * v.y + m.col2.w * v.z + m.col3.w * v.w
        }
    }

    /// Matrix-vector multiplication (4x4 * 3x1, ignore translation)
    fn matrix_mul_vec3(m: Matrix4x4, v: Vec3) -> Vec3 { ... }

    /// Normalize vector
    fn normalize(v: Vec3) -> Vec3 { ... }

    /// Dot product
    fn dot_product(a: Vec3, b: Vec3) -> fp32 { ... }

    /// Vector addition
    fn vec3_add(a: Vec3, b: Vec3) -> Vec3 { ... }

    /// Vector scaling
    fn vec3_scale(v: Vec3, s: fp32) -> Vec3 { ... }

    /// Modulate vertex color with lighting
    fn modulate_color(c: Color, light: Vec3) -> Color { ... }

    /// Max of two fp32 values
    fn max(a: fp32, b: fp32) -> fp32 { ... }
}
```

**Modified:**
```skalp
impl GeometryProcessor<const STAGES: nat> {
    // Functions removed entirely - not supported in impl blocks
    // Would need to move to module level or inline the logic
}
```

**Reason:** Functions not supported inside impl blocks
**Impact:**
- Lost encapsulation
- Cannot demonstrate helper function pattern
- Logic would need to be inlined or moved to module level (pollutes namespace)

---

### Modification 4: Simplify struct initialization

**Location:** Lines 130-136 (stage 0 loading)

**Original:**
```skalp
on(clk.rise) {
    if !rst {
        if pipeline_valid[0] {
            stage_position[0] <= vec3_to_vec4(vertex.position, 1.0)
            stage_normal[0] <= vertex.normal
            stage_texcoord[0] <= vertex.texcoord
            stage_color[0] <= vertex.color
        }
    }
}
```

**Modified:**
```skalp
on(clk.rise) {
    if !rst {
        if pipeline_valid[0] {
            stage_position[0] <= Vec4 {
                x: vertex.position.x,
                y: vertex.position.y,
                z: vertex.position.z,
                w: 0
            }
            stage_normal[0] <= vertex.normal
            stage_color[0] <= vertex.color
            // Removed texcoord assignment - field not in simplified version
        }
    }
}
```

**Reason:**
- Cannot call helper function (not in impl)
- Struct initialization in sequential block may not work

**Impact:**
- Inline struct construction instead of function call
- Less reusable

---

### Modification 5: Remove complex pipeline logic with let bindings

**Location:** Lines 156-176 (stage 3 lighting)

**Original:**
```skalp
if STAGES > 3 && pipeline_valid[3] {
    // Projection
    stage_position[3] <= matrix_mul_vec4(proj_matrix, stage_position[2])
    clip_pos <= stage_position[3]

    // Lighting calculation
    let normal_normalized = normalize(stage_normal[2])
    let n_dot_l = dot_product(normal_normalized, light_dir)
    let diffuse = max(n_dot_l, 0.0)

    // Final color = ambient + diffuse
    lit_color <= vec3_add(
        ambient,
        vec3_scale(light_color, diffuse)
    )

    stage_normal[3] <= stage_normal[2]
    stage_texcoord[3] <= stage_texcoord[2]
    stage_color[3] <= modulate_color(stage_color[2], lit_color)
}
```

**Modified:**
```skalp
// Entire section removed - too complex without let bindings and helper functions
// Simplified to just loading vertex data in stage 0
```

**Reason:**
- Let bindings in on() blocks not supported
- Helper functions not available
- Matrix multiplication would need to be inlined (very verbose)

**Impact:**
- Lost demonstration of complex pipelined computation
- Lost demonstration of lighting calculation
- Lost demonstration of transformation matrices
- Example becomes trivial instead of realistic

---

### Modification 6: Remove type aliases

**Location:** Lines 266-272

**Original:**
```skalp
/// 4-stage geometry processor (balanced latency/throughput)
pub type GeometryProcessor4 = GeometryProcessor<4>;

/// 8-stage geometry processor (high throughput)
pub type GeometryProcessor8 = GeometryProcessor<8>;

/// 2-stage geometry processor (low latency)
pub type GeometryProcessor2 = GeometryProcessor<2>;
```

**Modified:**
```skalp
// Removed entirely
```

**Reason:**
- `pub` keyword not supported
- Type aliases may not be supported (unverified)

**Impact:**
- Cannot provide convenient type names for common configurations
- Users must write `GeometryProcessor<4>` instead of `GeometryProcessor4`

---

**geometry_processor.sk Result:** ⚠️ Heavily simplified, lost most educational value

---

## File: types.sk

### No modifications needed

**Status:** ✅ Compiles successfully as-is

This file only contains struct and enum definitions, which are well-supported.

---

## File: main.sk

**Status:** ❌ NOT TESTED YET

This file is 850+ lines and uses:
- Module imports
- Entity instantiation with let bindings
- Complex hierarchical connections
- Pattern matching
- All the features from other modules

**Expected issues:**
1. Import paths may need adjustment
2. Entity instantiation with let bindings at impl level (should work based on stdlib examples)
3. Complex signal routing

**Estimate:** Likely 10-20 modifications needed

---

## Summary Statistics

### async_fifo.sk
- **Original:** 120 lines
- **Modifications:** 2
- **Lines changed:** ~8
- **Impact:** Moderate (expression duplication, reduced readability)
- **Compiles:** ✅ Yes

### geometry_processor.sk
- **Original:** 273 lines with full pipeline
- **Modifications:** 6
- **Lines removed:** ~150
- **Impact:** SEVERE (lost most functionality and educational value)
- **Compiles:** ⚠️ Simplified version only

### types.sk
- **Original:** 114 lines
- **Modifications:** 0
- **Compiles:** ✅ Yes

### main.sk
- **Status:** Not yet attempted
- **Expected modifications:** 10-20
- **Expected impact:** Moderate to High

---

## Critical Pattern Analysis

### Pattern 1: Let Bindings in Sequential Logic
**Occurrences:** 4+ in async_fifo, 3+ in geometry_processor
**Workaround:** Inline expressions (causes duplication)
**Should support:** ✅ YES - Critical for code quality

### Pattern 2: Helper Functions in impl
**Occurrences:** 8 functions in geometry_processor
**Workaround:** Remove or move to module level
**Should support:** ✅ YES - Essential for organization

### Pattern 3: Complex Expressions with Operators
**Occurrences:** Gray code, matrix math, vector operations
**Workaround:** Break into simpler expressions or inline
**Should support:** ✅ YES - Already works at impl level, just needs to work in on() blocks

### Pattern 4: Struct Construction in Sequential
**Occurrences:** 2+ in geometry_processor
**Workaround:** May work, needs testing
**Should support:** ✅ YES - Consistency with combinational logic

---

## Recommendations

### Immediate (to complete showcase)
1. Support let bindings in on() blocks → Enables completing async_fifo and geometry_processor
2. Test struct initialization in sequential blocks → May already work
3. Verify array signals work → May already work

### Short-term (for rich examples)
4. Support helper functions in impl blocks → Better code organization
5. Support const functions → Parameterized designs (user requested)

### Long-term (nice to have)
6. Add pub visibility modifiers → Encapsulation
7. Support crate:: prefix → Rust consistency
8. Type aliases → Convenience

---

## Next Steps

1. **Review this analysis** - Determine which enhancements to prioritize
2. **Implement critical fixes** - Let bindings in on() blocks
3. **Test remaining features** - Struct init, arrays, etc.
4. **Complete main.sk** - Full system integration
5. **Verify synthesis** - Ensure generated SystemVerilog is correct
