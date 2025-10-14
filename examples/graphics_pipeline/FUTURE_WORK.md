# Future Work and Known Limitations

This document tracks improvements and limitations for the graphics_pipeline reference implementation.

## Priority: High

### 1. Intent-Driven Architecture Selection (Currently Not Implemented)

**Issue:** The `INTENT` parameter in `GeometryProcessor<T, STAGES, INTENT>` is currently just a type parameter used for documentation. It does not actually change the generated hardware architecture.

**Current Behavior:**
```skalp
// These three instantiations generate identical internal logic:
GeometryProcessor<fp32, 5, OptimizeLatency>
GeometryProcessor<fp32, 5, OptimizeArea>
GeometryProcessor<fp32, 5, OptimizeBalanced>
```

The differentiation only comes from:
- Type parameter `T` (fp32 vs fp16 vs fixed-point)
- Pipeline depth `STAGES`
- But NOT from `INTENT`

**Desired Behavior:**
- `OptimizeLatency` → Fully parallel matrix multiply (16 multipliers, 1 cycle)
- `OptimizeArea` → Sequential multiply with resource sharing (1 multiplier, 16 cycles)
- `OptimizeBalanced` → Partially parallel (4 multipliers, 4 cycles)

**Possible Solutions:**

#### Option A: Const-If (Language Feature)
```skalp
const if INTENT == OptimizeLatency {
    let matrix_mult = ParallelMatrixMultiplier<T> { ... };
} else if INTENT == OptimizeArea {
    let matrix_mult = SequentialMatrixMultiplier<T> { ... };
} else {
    let matrix_mult = BalancedMatrixMultiplier<T> { ... };
}
```

Requires: Language support for compile-time conditionals based on const parameters

#### Option B: Trait-Based Strategy Pattern
```skalp
trait OptimizationStrategy<T: Numeric> {
    fn transform(matrix: [[T; 4]; 4], vec: vec4<T>) -> vec4<T>;
    const LATENCY: nat;
    const AREA: nat;
}

entity GeometryProcessor<T: Numeric, S: OptimizationStrategy<T>> {
    // Use S::transform() for operations
}
```

Requires: Associated constants in traits, static dispatch

#### Option C: Synthesis Directives
```skalp
entity GeometryProcessor<T: Numeric, const STAGES: nat, const INTENT: OptimizationIntent> {
    // Compiler/synthesizer interprets INTENT and applies optimizations:
    // - Resource sharing for OptimizeArea
    // - Parallel instantiation for OptimizeLatency
    // - Balanced retiming for OptimizeBalanced

    #[synthesis_hint(intent = INTENT)]
    world_position <= matrix_vector_multiply::<T>(model_matrix, pos_homogeneous);
}
```

Requires: Synthesis tool integration, optimization passes

**Recommendation:** Start with Option C (synthesis directives) as it's least invasive to language, then add Option A (const-if) for explicit control.

**Workaround (Current):**
- Create separate implementations: `ParallelGeometryProcessor`, `SequentialGeometryProcessor`
- Use type aliases to select: `type Fast = ParallelGeometryProcessor<fp32, 5>`

**Files Affected:**
- `src/geometry/processor.sk`
- `lib/numeric/matrix.sk` (would need parallel/sequential variants)

---

## Priority: Medium

### 2. Module System Performance with Deep Nesting

**Issue:** Module resolution has O(n²) behavior with deeply nested imports, causing timeouts on full `main.sk`.

**Current Status:** Individual modules compile fine, but importing everything together times out.

**Workaround:** Test/compile individual modules separately.

**Long-term Fix:** Optimize module resolution algorithm (caching, topological sort).

---

### 3. Matrix Inverse Implementation

**Issue:** `matrix_inverse()` in `lib/numeric/matrix.sk` is a placeholder that returns identity matrix.

**Reason:** Full 4x4 matrix inversion is expensive in hardware and rarely needed for graphics.

**Alternatives:**
- For orthogonal matrices (rotation only): inverse = transpose (free!)
- For general case: Use LU decomposition or Gauss-Jordan elimination
- Precompute on CPU and load as constant

**If Needed:**
- Implement cofactor method (expensive: 4× det3x3, many multiplies)
- Or implement LU decomposition (better numerically)
- Or implement iterative Newton-Raphson approximation

---

### 4. Look-At Matrix Implementation

**Issue:** `matrix_look_at()` in `lib/numeric/matrix.sk` returns placeholder identity matrix.

**Requires:** Vector cross product and normalization (available in vector.sk)

**Implementation:**
```skalp
pub fn matrix_look_at<T: Numeric>(
    eye: vec3<T>,
    target: vec3<T>,
    up: vec3<T>
) -> [[T; 4]; 4] {
    let forward = vec_normalize(vec_sub(target, eye));
    let right = vec_normalize(vec_cross(forward, up));
    let up_corrected = vec_cross(right, forward);

    // Build rotation + translation matrix
    return [
        [right.x, right.y, right.z, -vec_dot(right, eye)],
        [up_corrected.x, up_corrected.y, up_corrected.z, -vec_dot(up_corrected, eye)],
        [-forward.x, -forward.y, -forward.z, vec_dot(forward, eye)],
        [T::ZERO, T::ZERO, T::ZERO, T::ONE]
    ]
}
```

---

### 5. Rasterizer Implementation

**Issue:** `SimpleRasterizer` entity is declared but not fully implemented.

**Required Components:**
1. Triangle assembly (collect 3 vertices)
2. Viewport transformation (clip space → screen space)
3. Bounding box calculation
4. Edge equation setup
5. Scanline traversal or tile-based rasterization
6. Barycentric coordinate computation
7. Attribute interpolation (color, normal, depth)
8. Z-buffer depth testing

**Complexity:** This is a substantial implementation (~500-1000 lines).

**Reference:** See STATUS.md for original complex_project design notes.

---

### 6. Frame Buffer Implementation

**Issue:** `SimpleFrameBuffer` needs memory organization and timing.

**Considerations:**
- Single vs. double buffering
- Block RAM vs. external DRAM
- Read-during-write hazards
- Bandwidth requirements

**For 800×600×32bpp:**
- Memory: 1.92 MB (too large for block RAM)
- Bandwidth: 60 fps × 800×600 = 28.8 MB/s read + write
- Needs: External SDRAM or reduced bit depth

---

### 7. Clock Domain Crossing Verification

**Issue:** Async FIFOs use gray code synchronizers but formal verification not complete.

**Need:**
- Formal proofs that CDC is safe
- Timing analysis for metastability
- Constraint generation for different clock ratios

**Files:**
- `lib/fifo/async_fifo.sk`
- `verif/properties/fifo_props.sk` (has properties but needs formal tool integration)

---

## Priority: Low

### 8. CORDIC Lookup Table Initialization

**Issue:** `CORDIC_ATAN_TABLE` in `lib/numeric/cordic.sk` uses hardcoded values.

**Enhancement:** Generate table at compile-time or make parametric by width.

---

### 9. Texture Mapping

**Issue:** `TransformedVertex` has `tex_coord` field but texture sampling not implemented.

**Would Require:**
- Texture memory interface
- Bilinear/trilinear filtering
- Mipmap support (for quality)

---

### 10. Specular Lighting

**Issue:** Geometry processor computes diffuse + ambient but no specular.

**Requires:** View vector (camera position) to compute reflection vector.

**Formula:** `specular = pow(max(0, reflect · view), shininess)`

---

### 11. Clipping

**Issue:** No triangle clipping against view frustum.

**Problem:** Triangles partially outside clip space will render incorrectly.

**Solution:** Implement Sutherland-Hodgeman clipping algorithm.

---

### 12. Backface Culling

**Issue:** All triangles are rasterized, even if facing away from camera.

**Optimization:** Check if `(v1-v0) × (v2-v0) · view < 0` and discard.

---

## Testing and Validation

### 13. Golden Reference Outputs

**Issue:** `verif/golden/transformed_vertices.txt` has hand-calculated values.

**Need:** Automated golden model (Python/C++) to generate expected outputs.

---

### 14. End-to-End Simulation

**Issue:** No full pipeline testbench yet.

**Need:**
- Stimulus generation (vertex streams)
- Output checking (video frames)
- Performance measurement

---

### 15. FPGA Implementation Results

**Issue:** Designs not yet synthesized on actual hardware.

**Need:**
- Synthesize for iCE40-HX8K
- Synthesize for Artix-7 XC7A35T/XC7A100T
- Measure actual resource usage, frequency, power
- Compare to estimates in examples

---

## Documentation

### 16. Tutorial for Graphics Pipeline Concepts

**Need:** Step-by-step guide explaining:
- Coordinate spaces (object/world/camera/clip/screen)
- Transformation matrices (what each does)
- Lighting models
- Rasterization algorithms

---

### 17. Performance Profiling Guide

**Need:** How to:
- Identify bottlenecks
- Measure throughput
- Optimize for different metrics

---

## Language/Compiler Features Needed

### 18. Const Generics with Arithmetic

**Example:** `vec<T, N*2>` or `fixed<W, W/2, true>`

**Use Case:** Derive dimensions from other parameters.

---

### 19. Const Functions

**Need:** Functions that execute at compile-time to compute constants.

```skalp
const fn compute_table_entry(i: nat) -> i32 {
    // Evaluated at compile time
}
```

---

### 20. Module Visibility Control

**Need:** Better control over what's exported from modules.

---

## Notes

- This document will be updated as features are implemented
- Priority is subjective and may change based on use cases
- Many "missing" features are intentional simplifications for demonstration
- Real production graphics pipeline would need all of these plus more

---

**Last Updated:** October 14, 2025
**Status:** Reference implementation demonstrates core concepts, not production-ready
