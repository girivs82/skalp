# Standard Library Usage Design

## Problem Statement

Users need an idiomatic way to use stdlib operations in their SKALP code. Currently, stdlib entities are defined in `.sk` files but there's no mechanism to import/use them.

## Requirements

1. **Idiomatic** - Should feel natural to SKALP users (like Rust/Python/SystemVerilog)
2. **Explicit** - Clear what's being imported
3. **Type-safe** - Work with SKALP's generic/trait system
4. **Scalable** - Support growing stdlib without parser changes
5. **Backward compatible** - Built-in types (fp32, vec3) continue to work

## Proposed Solutions

### Option 1: Module System with `use` (RECOMMENDED)

**Syntax:**
```skalp
// Import specific entities
use std::vec::Vec3Normalize;
use std::fp::FP32Sqrt;

// Import all from module
use std::vec::*;

// Import with alias
use std::fp::FP32Sqrt as Sqrt;

entity MyDesign {
    in v: vec3<fp32>
    out normalized: vec3<fp32>
}

impl MyDesign {
    inst norm: Vec3Normalize {
        v = v,
        normalized => normalized
    }
}
```

**Pros:**
- Familiar to Rust/Python users
- Explicit imports
- Can control namespace pollution
- Supports aliasing
- Keywords already exist (UseKw, ModKw)

**Cons:**
- Requires implementing full module system
- Need to define stdlib module hierarchy

**Implementation:**
1. Add `use` statement parsing
2. Implement module resolution
3. Load stdlib files from known location
4. Add to HIR builder context

---

### Option 2: Implicit Stdlib (Like C++ iostream)

**Syntax:**
```skalp
// Stdlib entities automatically available, just use them
entity MyDesign {
    in v: vec3<fp32>
    out normalized: vec3<fp32>
}

impl MyDesign {
    inst norm: Vec3Normalize {  // Automatically resolved
        v = v,
        normalized => normalized
    }
}
```

**Pros:**
- Zero boilerplate
- Easy for beginners
- Like how fp32/vec3 already work

**Cons:**
- Namespace pollution
- Slower compilation (load everything)
- Unclear what's available
- Name conflicts harder to debug

**Implementation:**
1. Preload all stdlib during parser init
2. Add stdlib entities to global scope

---

### Option 3: Explicit Include (Like C/Verilog)

**Syntax:**
```skalp
include "std/vec/vec_advanced.sk"
include "std/fp/fp32_sqrt.sk"

entity MyDesign {
    in v: vec3<fp32>
    out normalized: vec3<fp32>
}

impl MyDesign {
    inst norm: Vec3Normalize {
        v = v,
        normalized => normalized
    }
}
```

**Pros:**
- Explicit file dependencies
- Familiar to C/Verilog users
- Easy to implement

**Cons:**
- Exposes file structure
- Fragile (file renames break code)
- Not modern

---

### Option 4: Package/Library System (Like VHDL)

**Syntax:**
```skalp
library std;
use std.vec.all;
use std.fp.FP32Sqrt;

entity MyDesign {
    in v: vec3<fp32>
    out normalized: vec3<fp32>
}

impl MyDesign {
    inst norm: Vec3Normalize {
        v = v,
        normalized => normalized
    }
}
```

**Pros:**
- Explicit scope control
- Familiar to VHDL users

**Cons:**
- Verbose
- Two-step (library + use)

---

## Recommended Approach: Hybrid

**Combine implicit built-ins with explicit imports for advanced features**

### Tier 1: Implicit (Built-in Types)
Already working, no import needed:
- Types: `fp16`, `fp32`, `fp64`, `vec2<T>`, `vec3<T>`, `vec4<T>`
- Operators: `+`, `-`, `*`, `/`, `<`, `>`, `==`

### Tier 2: Explicit Import (Stdlib Entities)
Require `use` statement:
- Advanced ops: `Vec3Normalize`, `FP32Sqrt`, `Vec3Reflect`
- Complex entities: `PhongShading`, `RaySphereIntersection`

**Example:**
```skalp
use std::vec::{Vec3Normalize, Vec3Reflect};
use std::fp::FP32Sqrt;

entity MyShader {
    in normal: vec3<fp32>
    in view: vec3<fp32>
    out reflected: vec3<fp32>
}

impl MyShader {
    // Use built-in operator (no import needed)
    signal negated: vec3<fp32> = -view

    // Use stdlib entity (imported above)
    inst refl: Vec3Reflect {
        v = negated,
        n = normal,
        reflected => reflected
    }
}
```

---

## Proposed Module Hierarchy

```
std/
├── fp/
│   ├── basic       - FP32Add, FP32Sub, FP32Mul, FP32Div
│   ├── util        - FP32Min, FP32Max, FP32Abs, FP32Clamp
│   ├── math        - FP32Sqrt, FP32Rsqrt, FP32Reciprocal
│   └── transcend   - (Future: sin, cos, exp, log)
│
├── vec/
│   ├── basic       - Vec3Add, Vec3Sub, Vec3Dot, Vec3Cross
│   ├── geometry    - Vec3Normalize, Vec3Reflect, Vec3Refract
│   └── projection  - Vec3Project, Vec3Reject, Vec3FaceForward
│
├── matrix/
│   └── (Future)
│
├── memory/
│   ├── fifo
│   └── ram
│
└── io/
    ├── uart
    └── axi
```

**Usage:**
```skalp
use std::fp::math::*;        // FP32Sqrt, FP32Rsqrt, etc.
use std::vec::geometry::*;   // Vec3Normalize, Vec3Reflect, etc.
```

Or shorthand:
```skalp
use std::fp::*;    // Everything from fp
use std::vec::*;   // Everything from vec
```

---

## Implementation Plan

### Phase 1: Minimal (Current Workaround)
**Status:** What we do now for tests

```rust
// In tests, concatenate stdlib source
let source = format!("{}\n{}",
    std::fs::read_to_string("stdlib/fp/fp32_sqrt.sk")?,
    user_code
);
```

**Pros:** Works immediately
**Cons:** Not user-facing, test-only

---

### Phase 2: Compiler Flag (Short-term)

Add compiler flag to include stdlib:

```bash
skalp build --stdlib main.sk
# Or
skalp build -L stdlib/ main.sk
```

**Implementation:**
1. Add `-L` / `--stdlib` flag to CLI
2. Preload all `.sk` files from stdlib directory
3. Concatenate with user source before parsing

**Pros:**
- Easy to implement (~100 lines)
- Works for all use cases
- No language changes needed

**Cons:**
- No fine-grained control
- Loads everything (slow)

---

### Phase 3: Use Statement (Medium-term)

Implement `use` statement:

```skalp
use std::vec::Vec3Normalize;

entity MyDesign { ... }
```

**Implementation Steps:**
1. **Parser** - Parse `use` statements
   - Add `UseItem` to syntax tree
   - Store in `SourceFile`

2. **Module Resolution**
   - Map `std::vec` → `<stdlib>/components/vec/vec_ops.sk`
   - Load and parse imported files
   - Build dependency graph

3. **HIR Builder**
   - Add imported entities to scope
   - Resolve entity names during instantiation

4. **Name Resolution**
   - Check use statements first
   - Fall back to current file
   - Error on ambiguous names

**Estimated effort:** 2-3 days

---

### Phase 4: Full Module System (Long-term)

Complete module system with:
- Nested modules
- Visibility control (`pub`)
- Path resolution
- Circular dependency detection

---

## Immediate Action Item

**For current stdlib development:**

Create a prelude that auto-loads commonly used operations:

```skalp
// std/prelude.sk - Automatically included
use std::fp::util::*;
use std::fp::math::{FP32Sqrt, FP32Rsqrt};
use std::vec::basic::*;
use std::vec::geometry::{Vec3Normalize, Vec3Reflect};
```

Then add compiler flag:
```bash
skalp build --prelude main.sk
```

This gives users immediate access to common operations without imports.

---

## Comparison with Other HDLs

### Verilog/SystemVerilog
```systemverilog
`include "defines.svh"
import my_pkg::*;
```
- File-based includes
- Package imports for type definitions

### VHDL
```vhdl
library work;
use work.my_package.all;
```
- Explicit library + use
- Verbose but clear

### Chisel (Scala)
```scala
import chisel3._
import chisel3.util._
```
- Standard Scala imports
- Leverages host language

### Bluespec
```bluespec
import Vector::*;
import FIFOF::*;
```
- Module-based imports
- Similar to our proposed `use`

**SKALP should follow Rust/Chisel pattern** - modern, explicit, tree-based imports.

---

## Decision Matrix

| Feature | Implicit | Include | Use Statement | Package |
|---------|----------|---------|---------------|---------|
| Ease of use | ⭐⭐⭐⭐⭐ | ⭐⭐⭐ | ⭐⭐⭐⭐ | ⭐⭐ |
| Explicit | ⭐ | ⭐⭐⭐ | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ |
| Modern | ⭐⭐ | ⭐ | ⭐⭐⭐⭐⭐ | ⭐⭐⭐ |
| Impl effort | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐ | ⭐⭐⭐ | ⭐⭐ |
| Scalability | ⭐ | ⭐⭐ | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ |

**Winner:** Use Statement (Phase 3)

---

## Recommendation

1. **Immediately**: Document that stdlib requires concatenation (Phase 1)
2. **Next PR**: Add `--stdlib` compiler flag (Phase 2) - ~2 hours work
3. **Future**: Implement `use` statement (Phase 3) - ~2-3 days work

## Example User Experience (Final)

```skalp
// main.sk
use std::vec::{Vec3Normalize, Vec3Reflect};
use std::fp::FP32Sqrt;

entity RayTracer {
    in ray_dir: vec3<fp32>
    in normal: vec3<fp32>
    out reflected: vec3<fp32>
}

impl RayTracer {
    // Built-in types and operators work without import
    signal dir_normalized: vec3<fp32> = ray_dir  // Will use stdlib internally

    // Stdlib entities require import (above)
    inst norm: Vec3Normalize {
        v = dir_normalized,
        normalized => dir_normalized
    }

    inst refl: Vec3Reflect {
        v = dir_normalized,
        n = normal,
        reflected => reflected
    }
}
```

Compile:
```bash
skalp build main.sk
# Automatically finds stdlib in <skalp-install>/lib/stdlib/
```

Clean, modern, and scalable! 🚀
