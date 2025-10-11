# SKALP Syntax Issues and Proposed Enhancements

This document lists all modifications required to make the complex example project compile, and analyzes whether SKALP should be enhanced to support the original syntax.

## Summary

The complex graphics pipeline example was designed to showcase SKALP's advanced features:
- Parametric types with generics
- Multi-clock domain crossing
- Gray code CDC
- Pattern matching state machines
- Hierarchical module instantiation
- Complex data structures

However, several syntax patterns that seem natural and are used in real hardware design require workarounds.

---

## Issue 1: Let Bindings Inside Clocked Blocks (on() blocks)

### Original Code (Natural Syntax)
```skalp
on(wr_clk.rise) {
    if wr_en && !wr_full {
        let wr_addr = wr_ptr % DEPTH
        mem[wr_addr] <= wr_data

        let next_wr_ptr = wr_ptr + 1
        wr_ptr <= next_wr_ptr
        wr_ptr_gray <= next_wr_ptr ^ (next_wr_ptr >> 1)
    }
}
```

### Required Workaround
```skalp
on(wr_clk.rise) {
    if wr_en && !wr_full {
        mem[wr_ptr % DEPTH] <= wr_data
        wr_ptr <= wr_ptr + 1
        wr_ptr_gray <= (wr_ptr + 1) ^ ((wr_ptr + 1) >> 1)
    }
}
```

### Analysis

**Why the original is better:**
1. **Readability**: Let bindings make complex expressions readable
2. **Reusability**: `next_wr_ptr` is used twice - computing it once is clearer
3. **Maintainability**: Easier to debug when intermediate values have names
4. **Common in HDL**: SystemVerilog allows local variables in always blocks
5. **Avoids duplication**: Without let bindings, you repeat `wr_ptr + 1` which is error-prone

**Comparison to other HDLs:**

SystemVerilog (supports this pattern):
```systemverilog
always_ff @(posedge wr_clk) begin
    if (wr_en && !wr_full) begin
        automatic logic [8:0] next_wr_ptr = wr_ptr + 1;
        mem[wr_ptr % DEPTH] <= wr_data;
        wr_ptr <= next_wr_ptr;
        wr_ptr_gray <= next_wr_ptr ^ (next_wr_ptr >> 1);
    end
end
```

Chisel (supports this pattern):
```scala
when(wr_en && !wr_full) {
  val nextWrPtr = wr_ptr + 1.U
  mem(wr_ptr % DEPTH.U) := wr_data
  wr_ptr := nextWrPtr
  wr_ptr_gray := nextWrPtr ^ (nextWrPtr >> 1)
}
```

**Recommendation:** ✅ **SKALP SHOULD SUPPORT THIS**

Let bindings inside on() blocks are essential for:
- Code clarity and maintainability
- Matching hardware designer expectations
- Avoiding error-prone expression duplication
- Consistency with combinational logic (where let bindings work)

**Implementation note:** The compiler can treat let bindings in sequential blocks as:
- Local wires in the generated Verilog
- Or inline the expressions (current behavior) but allow the syntax

---

## Issue 2: Helper Functions Inside impl Blocks

### Original Code (Natural Syntax)
```skalp
impl GeometryProcessor<const STAGES: nat> {
    signal world_pos: Vec4
    signal view_pos: Vec4

    // ... clocked logic ...

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
}
```

### Current Limitation
Functions must be defined at module level, not inside impl blocks.

### Analysis

**Why the original is better:**
1. **Encapsulation**: Helper functions belong to the entity they help
2. **Namespace clarity**: Functions scoped to impl don't pollute module namespace
3. **Organization**: Related functions grouped with the entity
4. **OOP patterns**: Similar to methods in Rust/C++/other languages

**Comparison to other HDLs:**

SystemVerilog (supports local functions):
```systemverilog
module GeometryProcessor;
    function automatic vec4 vec3_to_vec4(vec3 v, real w);
        return '{v.x, v.y, v.z, w};
    endfunction

    always_comb begin
        result = vec3_to_vec4(input_vec, 1.0);
    end
endmodule
```

**Recommendation:** ✅ **SKALP SHOULD SUPPORT THIS**

Helper functions inside impl blocks provide:
- Better code organization
- Proper scoping and encapsulation
- Alignment with Rust-like syntax that SKALP uses
- Common pattern in hardware design

**Alternative:** If not supported in impl blocks, could support them as associated functions on entities:
```skalp
entity GeometryProcessor<const STAGES: nat> {
    // ports...

    fn vec3_to_vec4(v: Vec3, w: fp32) -> Vec4 {
        // implementation
    }
}
```

---

## Issue 3: Struct Initialization in Assignments

### Original Code
```skalp
on(clk.rise) {
    if pipeline_valid[0] {
        stage_position[0] <= Vec4 {
            x: vertex.position.x,
            y: vertex.position.y,
            z: vertex.position.z,
            w: 0
        }
    }
}
```

### Error
Parse error: "expected assignment operator"

### Analysis

**Current Status:**
- Struct initialization works at impl level (combinational)
- Does NOT work inside on() blocks
- Vec concatenation `{a, b, c}` works for bit vectors but not struct fields

**Example that works:**
```skalp
// At impl level (combinational)
output = TransformedVertex {
    position: clip_pos,
    normal: world_normal,
    color: stage_color[0]
}
```

**Recommendation:** ✅ **SKALP SHOULD SUPPORT THIS**

Struct initialization should work consistently:
- In combinational assignments (already works)
- In sequential assignments inside on() blocks (currently fails)
- This is a consistency issue, not a new feature

---

## Issue 4: Array Signal Type Syntax

### Attempted Syntax
```skalp
signal pipeline_valid: [bit; STAGES]
signal stage_position: [Vec4; STAGES]
```

### Analysis

This syntax is consistent with Rust's array syntax `[T; N]`.

**Current working syntax:**
```skalp
signal mem: [T; DEPTH]  // This works in async_fifo
```

**Status:** Need to verify if this actually fails or if it was another issue.

**Recommendation:** ⚠️ **VERIFY FIRST**

If arrays of custom types don't work, they should be supported.

---

## Issue 5: Use Statements with crate:: Prefix

### Original Code
```skalp
use crate::types::{Vertex, TransformedVertex};
```

### Working Syntax
```skalp
use types::{Vertex, TransformedVertex};
```

### Analysis

**Current Status:**
- `crate::` prefix causes parse errors
- Module system uses simple paths without `crate::`

**Recommendation:** 🤔 **DESIGN DECISION**

Options:
1. Support `crate::` for Rust consistency
2. Keep simple paths (current behavior)
3. Support both

**Rust uses:**
- `crate::module` - absolute path from crate root
- `self::module` - relative to current module
- `super::module` - parent module
- `module` - could be relative or from extern crate

**For SKALP:** Given the simpler module system, current behavior might be fine. But `crate::` support would help Rust developers.

---

## Issue 6: pub Keyword on Structs and Functions

### Original Attempt (in types.sk)
```skalp
pub struct Vec3 {
    pub x: bit[32],
    pub y: bit[32],
    pub z: bit[32]
}

pub fn matrix_mul(m: Matrix4x4, v: Vec4) -> Vec4 {
    // ...
}
```

### Current Status
`pub` keyword not supported - all items are public by default.

### Analysis

**Recommendation:** 🤔 **DESIGN DECISION - LATER**

Visibility modifiers are useful for:
- Large projects with encapsulation
- Library design
- API boundaries

But not critical for the initial showcase. Can be added later.

---

## Issue 7: Const Functions for Compile-Time Evaluation

### Desired Syntax
```skalp
const fn clog2(n: nat) -> nat {
    // Ceiling log2 computation
    if n <= 1 {
        0
    } else {
        1 + clog2(n / 2)
    }
}

// Usage
signal addr_width: nat[clog2(DEPTH)]
```

### Analysis

**Current Status:**
- `clog2` is likely a built-in intrinsic
- User-defined const functions not supported

**Recommendation:** ✅ **SKALP SHOULD SUPPORT THIS (HIGH PRIORITY)**

Compile-time evaluation is crucial for:
- Parameterized hardware designs
- Computing derived parameters (address widths, counter sizes, etc.)
- Generic reusable components

**User specifically mentioned:**
> "clog2 can be a compile time intrinsic rather than a built-in, if we can define it within the skalp language as a macro/function"

This is a key feature request.

---

## Issue 8: Type Aliases with pub

### Original Attempt
```skalp
pub type GeometryProcessor4 = GeometryProcessor<4>;
pub type GeometryProcessor8 = GeometryProcessor<8>;
```

### Analysis

**Recommendation:** ✅ **SHOULD SUPPORT** (after visibility modifiers)

Type aliases are useful for:
- Giving meaningful names to specialized versions
- API design
- Reducing verbosity

---

## Priority Ranking

### HIGH PRIORITY (Critical for complex designs)

1. **Let bindings in on() blocks** - Essential for readable sequential logic
2. **Const functions** - Core feature for parameterized designs
3. **Struct initialization in sequential blocks** - Consistency fix

### MEDIUM PRIORITY (Improves ergonomics)

4. **Helper functions in impl blocks** - Better organization
5. **Type aliases** - Convenience feature

### LOW PRIORITY (Nice to have)

6. **crate:: prefix support** - Rust consistency
7. **pub visibility** - For large projects

---

## Verification Examples

### Test 1: Let Bindings in Sequential Logic
```skalp
entity TestLetInSeq {
    in clk: clock
    in rst: reset(active_high)
    in data: bit[8]
    out result: bit[8]
}

impl TestLetInSeq {
    signal counter: bit[8]

    on(clk.rise) {
        if rst {
            counter <= 0
        } else {
            let next = counter + 1
            let masked = next & data
            counter <= masked  // Use let-bound value
        }
    }

    result = counter
}
```

**Expected:** Should compile and generate correct SystemVerilog
**Actual:** Parse error - "expected statement"

### Test 2: Struct Init in Sequential Logic
```skalp
struct Point {
    x: bit[16],
    y: bit[16]
}

entity TestStructSeq {
    in clk: clock
    in x: bit[16]
    in y: bit[16]
    out point: Point
}

impl TestStructSeq {
    signal pt: Point

    on(clk.rise) {
        pt <= Point { x: x, y: y }
    }

    point = pt
}
```

**Expected:** Should compile
**Actual:** Parse error

---

## Conclusion

The original showcase code uses natural, intuitive syntax that matches hardware designer expectations and patterns from other HDLs. The required workarounds make the code:
- Less readable
- More error-prone (expression duplication)
- Harder to maintain
- Less pedagogical for the example

**Recommendation:** Enhance SKALP to support these patterns rather than requiring workarounds.

The two most critical enhancements are:
1. **Let bindings in sequential blocks** - Fundamental for code quality
2. **Const functions** - Requested by user, essential for generic hardware

These align with SKALP's goal of being a modern, expressive HDL.
