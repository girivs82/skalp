# Implicit Instantiation Design

## Overview

Allow SKALP to support function-like syntax that implicitly instantiates stdlib entities based on type inference, making stdlib usage feel more natural while maintaining the explicit hardware semantics underneath.

## Motivation

**Current explicit instantiation:**
```skalp
entity MyDesign {
    in x: fp32
    out result: fp32
}

impl MyDesign {
    inst s: FP32Sqrt {
        x = x,
        result => result
    }
}
```

**Proposed implicit instantiation:**
```skalp
entity MyDesign {
    in x: fp32
    out result: fp32
}

impl MyDesign {
    result = sqrt(x)  // Compiler infers FP32Sqrt instantiation
}
```

**Benefits:**
- More ergonomic for simple operations
- Familiar to software engineers
- Reduces boilerplate for common operations
- Still explicitly hardware (module instantiation)
- Strong typing enables unambiguous resolution

## Syntax

### Function-Like Call Expressions

```skalp
// Single argument
result = sqrt(x)

// Multiple arguments
min_val = min(a, b)
clamped = clamp(value, min_val, max_val)

// Chaining
normalized = normalize(cross(a, b))

// Mixed with operators
result = sqrt(x * x + y * y)

// As part of larger expressions
z = a + sqrt(b * b - 4.0 * a * c)
```

### Type-Driven Resolution

The compiler uses type information to determine which entity to instantiate:

```skalp
// Example 1: FP32 sqrt
signal x: fp32 = /* ... */
signal result: fp32 = sqrt(x)
// → Instantiates FP32Sqrt

// Example 2: Different type, different implementation
signal x: fp64 = /* ... */
signal result: fp64 = sqrt(x)
// → Instantiates FP64Sqrt (when available)

// Example 3: Vector normalize
signal v: vec3<fp32> = /* ... */
signal n: vec3<fp32> = normalize(v)
// → Instantiates Vec3Normalize
```

### Explicit vs Implicit Instantiation

Both can coexist in the same design:

```skalp
impl MyShader {
    // Implicit - simple, one-liner
    signal len: fp32 = length(position)

    // Explicit - complex configuration, multiple outputs
    inst phong: PhongShading {
        position = position,
        normal = normal,
        view_dir = view_dir,
        light_pos = light_pos,
        light_color = light_color,
        light_intensity = intensity,
        material_color = material,
        shininess = 32.0,
        color => output_color
    }
}
```

**Rule of thumb:**
- **Implicit** for single-output operations with simple arguments
- **Explicit** for multi-output, complex configuration, or when you need named instances

## Type Inference and Resolution

### Resolution Algorithm

1. **Parse function call** - Recognize `identifier(args)` pattern
2. **Infer argument types** - Type-check each argument expression
3. **Build signature** - Create function signature: `(name, arg_types...) → ?`
4. **Search stdlib registry** - Find entity matching signature
5. **Infer return type** - Determine return type from matched entity
6. **Generate instantiation** - Create implicit `inst` in HIR/MIR

### Stdlib Function Registry

The compiler maintains a registry of stdlib entities that can be implicitly instantiated:

```rust
// Compiler's internal registry
struct StdlibFunction {
    name: String,           // "sqrt"
    entity: String,         // "FP32Sqrt"
    signature: FunctionSig, // (fp32) → fp32
    module_path: String,    // "std::fp::math"
}

// Example registry entries
[
    StdlibFunction {
        name: "sqrt",
        entity: "FP32Sqrt",
        signature: (fp32) → fp32,
        module_path: "std::fp::math"
    },
    StdlibFunction {
        name: "sqrt",
        entity: "FP64Sqrt",
        signature: (fp64) → fp64,
        module_path: "std::fp::math"
    },
    StdlibFunction {
        name: "normalize",
        entity: "Vec3Normalize",
        signature: (vec3<fp32>) → vec3<fp32>,
        module_path: "std::vec::geometry"
    },
    StdlibFunction {
        name: "normalize",
        entity: "Vec2Normalize",
        signature: (vec2<fp32>) → vec2<fp32>,
        module_path: "std::vec::geometry"
    },
    StdlibFunction {
        name: "min",
        entity: "FP32Min",
        signature: (fp32, fp32) → fp32,
        module_path: "std::fp::util"
    },
    StdlibFunction {
        name: "clamp",
        entity: "FP32Clamp",
        signature: (fp32, fp32, fp32) → fp32,
        module_path: "std::fp::util"
    },
]
```

### Overload Resolution

Multiple entities can share the same function name (overloading):

```skalp
// These all use the name "sqrt" but different types
signal f32_result: fp32 = sqrt(fp32_value)     // → FP32Sqrt
signal f64_result: fp64 = sqrt(fp64_value)     // → FP64Sqrt
signal f16_result: fp16 = sqrt(fp16_value)     // → FP16Sqrt
```

**Resolution rules:**
1. Exact type match (no conversions)
2. If multiple matches → error (ambiguous)
3. If no matches → error (not found)

### Generic Functions

Generic entities can work with implicit instantiation:

```skalp
// Vec3Dot<T> is generic over element type
entity Vec3Dot<T> where T: Synthesizable {
    in a: vec3<T>
    in b: vec3<T>
    out result: T
}

// Usage - compiler infers generic parameter
signal a: vec3<fp32> = /* ... */
signal b: vec3<fp32> = /* ... */
signal d: fp32 = dot(a, b)  // → Instantiates Vec3Dot<fp32>

// Different type, same function name
signal a_i32: vec3<bit<32>> = /* ... */
signal b_i32: vec3<bit<32>> = /* ... */
signal d_i32: bit<32> = dot(a_i32, b_i32)  // → Instantiates Vec3Dot<bit<32>>
```

## Namespace and Name Conflicts

### Function Name Scoping

Function names follow the same scoping rules as other identifiers:

1. **Current module** - Check for local entities first
2. **Imported modules** - Check `use` statements
3. **Stdlib prelude** - Check common stdlib functions
4. **Error** - If still not found

```skalp
use std::fp::math::*;  // Imports sqrt, rsqrt, etc.

entity MyDesign {
    in x: fp32
    out result: fp32
}

impl MyDesign {
    result = sqrt(x)  // Found via use statement
}
```

### Disambiguation

If multiple functions match, the user must disambiguate:

**Option 1: Explicit module path**
```skalp
result = std::fp::math::sqrt(x)
```

**Option 2: Type annotation**
```skalp
signal result: fp32 = sqrt(x)  // Type forces fp32 version
```

**Option 3: Explicit instantiation**
```skalp
inst s: FP32Sqrt {
    x = x,
    result => result
}
```

### User-Defined Functions

Users can define their own entities that work with implicit instantiation:

```skalp
// User defines custom sqrt for a fixed-point type
entity FixedSqrt {
    in x: bit<32>
    out result: bit<32>
}

impl FixedSqrt {
    // ... implementation
}

// Use it implicitly
signal fixed_x: bit<32> = /* ... */
signal fixed_result: bit<32> = sqrt(fixed_x)  // → Instantiates FixedSqrt
```

**How:** The compiler builds the registry from:
1. Stdlib entities (built-in)
2. Entities in current file
3. Entities from `use` statements

## Integration with Use Statements

Implicit instantiation **complements** the `use` statement system from STDLIB_USAGE_DESIGN.md:

### Imports Bring Names Into Scope

Operations must be imported with `use` to bring them into namespace:

```skalp
use std::fp::math::{sqrt, abs, min, max};
use std::vec::geometry::{normalize, length};

impl MyDesign {
    // Now these names are in scope, use function-like syntax
    signal result: fp32 = sqrt(x)
    signal min_val: fp32 = min(a, b)
    signal len: fp32 = length(v)
}
```

**Without import, functions are not in scope:**
```skalp
impl MyDesign {
    signal result: fp32 = sqrt(x)  // ❌ Error: 'sqrt' not found
}
```

### Wildcard Imports for Convenience

```skalp
use std::fp::math::*;     // All FP math operations
use std::vec::geometry::*; // All vector geometry operations

impl MyShader {
    signal reflected: vec3<fp32> = reflect(incident, normal)
    signal inv_len: fp32 = rsqrt(length_sq)
}
```

### Combined Example

```skalp
use std::vec::geometry::{normalize, length, reflect};

entity RayTracer {
    in ray_dir: vec3<fp32>
    in hit_point: vec3<fp32>
    in normal: vec3<fp32>
    out reflected_ray: vec3<fp32>
}

impl RayTracer {
    // Imported functions - use with function-like syntax (no port mapping)
    signal normalized_dir: vec3<fp32> = normalize(ray_dir)
    signal len: fp32 = length(ray_dir)
    signal refl: vec3<fp32> = reflect(normalized_dir, normal)

    // Explicit instantiation for complex multi-output operations
    // (still requires explicit inst with port mapping)
    inst intersection: RaySphereIntersection {
        ray_origin = hit_point,
        ray_dir = refl,
        sphere_center = sphere_pos,
        sphere_radius = sphere_r,
        hit => has_hit,
        t => hit_distance,
        normal => hit_normal
    }

    reflected_ray = refl
}
```

## Limitations and Edge Cases

### Single-Output Only

Implicit instantiation only works for single-output entities:

```skalp
// ✅ Works - single output
signal result: fp32 = sqrt(x)

// ❌ Doesn't work - multiple outputs
signal (quotient, remainder) = divmod(a, b)  // Error: use explicit inst
```

For multiple outputs, use explicit `inst`:

```skalp
inst dm: DivMod {
    a = a,
    b = b,
    quotient => q,
    remainder => r
}
```

### No Named Parameters

Implicit calls use positional arguments only:

```skalp
// ✅ Works
signal clamped: fp32 = clamp(value, 0.0, 1.0)

// ❌ Doesn't work - no named parameters
signal clamped: fp32 = clamp(value: value, min_val: 0.0, max_val: 1.0)
```

### No Partial Application

All arguments must be provided:

```skalp
// ❌ Doesn't work - partial application not supported
signal clamp_01 = clamp(_, 0.0, 1.0)  // Error
```

### Return Type Must Be Inferrable

The compiler must be able to infer the return type:

```skalp
// ✅ Works - return type clear from context
signal result: fp32 = sqrt(x)

// ✅ Works - return type inferred from usage
signal y: fp32 = sqrt(x) + 1.0

// ⚠️ Ambiguous - might need type annotation
signal z = sqrt(x)  // Warning or error if multiple sqrt overloads
```

## Implementation Plan

### Phase 1: Parser Changes

Add support for function call expressions:

```rust
// In syntax/expr.rs
enum Expr {
    // ... existing variants
    FunctionCall {
        name: Ident,
        args: Vec<Expr>,
    }
}
```

Parser recognizes `identifier(arg1, arg2, ...)` as function call.

### Phase 2: Stdlib Registry

Build registry of stdlib functions at compiler initialization:

```rust
// In stdlib/registry.rs
pub struct StdlibRegistry {
    functions: HashMap<String, Vec<StdlibFunction>>,
}

impl StdlibRegistry {
    pub fn new() -> Self {
        let mut registry = StdlibRegistry::default();

        // Register FP operations
        registry.register("sqrt", "FP32Sqrt", vec![Type::Fp32], Type::Fp32);
        registry.register("sqrt", "FP64Sqrt", vec![Type::Fp64], Type::Fp64);
        registry.register("min", "FP32Min", vec![Type::Fp32, Type::Fp32], Type::Fp32);

        // Register vector operations
        registry.register("normalize", "Vec3Normalize",
            vec![Type::Vec3(Box::new(Type::Fp32))],
            Type::Vec3(Box::new(Type::Fp32)));

        // ... etc

        registry
    }

    pub fn resolve(&self, name: &str, arg_types: &[Type]) -> Option<&StdlibFunction> {
        // Find exact match on (name, arg_types)
    }
}
```

### Phase 3: HIR Builder Integration

During HIR building, transform function calls to explicit instantiations:

```rust
// In hir/builder.rs
fn lower_function_call(&mut self, name: Ident, args: Vec<Expr>) -> Result<HirExpr> {
    // 1. Type-check arguments
    let arg_types: Vec<Type> = args.iter()
        .map(|arg| self.infer_type(arg))
        .collect()?;

    // 2. Resolve function
    let stdlib_fn = self.stdlib_registry.resolve(&name.text, &arg_types)
        .ok_or_else(|| Error::FunctionNotFound { name, arg_types })?;

    // 3. Generate unique instance name
    let inst_name = self.generate_inst_name(&stdlib_fn.entity);

    // 4. Create implicit instantiation (same as explicit inst)
    let inst = HirInst {
        name: inst_name.clone(),
        entity: stdlib_fn.entity.clone(),
        inputs: /* map args to entity inputs */,
        outputs: /* map to anonymous signal */,
    };

    self.add_inst(inst);

    // 5. Return reference to output signal
    Ok(HirExpr::Signal(inst_name + ".result"))
}
```

### Phase 4: Type Inference

Enhance type inference to handle function calls:

```rust
fn infer_expr_type(&self, expr: &Expr) -> Result<Type> {
    match expr {
        Expr::FunctionCall { name, args } => {
            let arg_types = args.iter()
                .map(|arg| self.infer_expr_type(arg))
                .collect::<Result<Vec<_>>>()?;

            let stdlib_fn = self.stdlib_registry.resolve(name, &arg_types)?;
            Ok(stdlib_fn.return_type.clone())
        }
        // ... other cases
    }
}
```

### Phase 5: Error Messages

Provide helpful errors for resolution failures:

```skalp
signal result: fp32 = sqrt(x, y)  // Wrong number of args
```

Error:
```
error: function 'sqrt' expects 1 argument, found 2
  --> example.sk:5:23
   |
5  |     signal result: fp32 = sqrt(x, y)
   |                           ^^^^^^^^^^
   |
note: function signature is: sqrt(fp32) -> fp32
```

```skalp
signal result = sqrt(x)  // Ambiguous type
```

Error:
```
error: cannot infer return type for function 'sqrt'
  --> example.sk:5:21
   |
5  |     signal result = sqrt(x)
   |                     ^^^^^^^
   |
help: add a type annotation to disambiguate
   |
5  |     signal result: fp32 = sqrt(x)
   |                  +++++++
```

## Registry Data Format

The stdlib function registry can be defined declaratively:

```yaml
# stdlib/registry.yaml
functions:
  - name: sqrt
    entity: FP32Sqrt
    signature: (fp32) -> fp32
    module: std::fp::math
    prelude: true

  - name: sqrt
    entity: FP64Sqrt
    signature: (fp64) -> fp64
    module: std::fp::math
    prelude: true

  - name: min
    entity: FP32Min
    signature: (fp32, fp32) -> fp32
    module: std::fp::util
    prelude: true

  - name: clamp
    entity: FP32Clamp
    signature: (fp32, fp32, fp32) -> fp32
    module: std::fp::util
    prelude: true

  - name: normalize
    entity: Vec3Normalize
    signature: (vec3<fp32>) -> vec3<fp32>
    module: std::vec::geometry
    prelude: true

  - name: reflect
    entity: Vec3Reflect
    signature: (vec3<fp32>, vec3<fp32>) -> vec3<fp32>
    module: std::vec::geometry
    prelude: false  # Requires import

  - name: refract
    entity: Vec3Refract
    signature: (vec3<fp32>, vec3<fp32>, fp32) -> vec3<fp32>
    module: std::vec::geometry
    prelude: false
```

## Comparison with Other HDLs

### SystemVerilog Functions

```systemverilog
function automatic [31:0] sqrt(input [31:0] x);
    // ... implementation
endfunction

// Usage
result = sqrt(x);
```

**Difference:** SystemVerilog functions are behavioral. SKALP implicit instantiation creates actual hardware modules.

### Chisel Apply Methods

```scala
class FP32Sqrt extends Module {
    val io = IO(new Bundle {
        val x = Input(FP32())
        val result = Output(FP32())
    })
    // ...
}

// Usage
val result = FP32Sqrt(x)  // apply method creates instance
```

**Similarity:** Similar ergonomics, but Chisel uses object-oriented apply methods. SKALP uses type-driven resolution.

### VHDL Functions

```vhdl
function sqrt(x : fp32) return fp32 is
begin
    -- ... implementation
end function;

-- Usage
result <= sqrt(x);
```

**Difference:** VHDL functions are synthesizable but limited. SKALP implicit instantiation can handle arbitrarily complex entities.

## Example: Before and After

### Before (Explicit Instantiation Only)

```skalp
use std::fp::math::{FP32Sqrt, FP32Rsqrt};
use std::vec::geometry::{Vec3Normalize, Vec3Reflect};

entity Shader {
    in position: vec3<fp32>
    in normal: vec3<fp32>
    in view_dir: vec3<fp32>
    out reflected: vec3<fp32>
}

impl Shader {
    // Normalize view direction
    inst view_norm: Vec3Normalize {
        v = view_dir,
        normalized => view_normalized
    }

    // Normalize normal
    inst normal_norm: Vec3Normalize {
        v = normal,
        normalized => normal_normalized
    }

    // Reflect
    inst refl: Vec3Reflect {
        v = view_normalized,
        n = normal_normalized,
        reflected => reflected
    }
}
```

### After (With Implicit Instantiation)

```skalp
use std::vec::geometry::{normalize, reflect};  // Import brings names into scope

entity Shader {
    in position: vec3<fp32>
    in normal: vec3<fp32>
    in view_dir: vec3<fp32>
    out reflected: vec3<fp32>
}

impl Shader {
    // Function-like syntax instead of port-by-port instantiation
    reflected = reflect(normalize(view_dir), normalize(normal))
}
```

**Lines of code:** 27 → 12 (55% reduction)
**Clarity:** Much clearer intent
**Hardware:** Identical synthesized result

## Import vs Instantiation

**Key distinction:** Implicit instantiation is about *how you call*, not *whether you import*.

### Import Model (Same as STDLIB_USAGE_DESIGN.md)

All stdlib operations require explicit imports:

```skalp
use std::fp::math::{sqrt, min, max};
use std::vec::geometry::{normalize, reflect};
```

### Instantiation Syntax (New with This Design)

Once imported, you choose *how* to instantiate:

**Option 1: Implicit (function-like)**
```skalp
signal result: fp32 = sqrt(x)               // Compiler creates inst implicitly
```

**Option 2: Explicit (port mapping)**
```skalp
inst s: FP32Sqrt { x = x, result => result } // Manual inst with ports
```

### When to Use Each

**Implicit instantiation:**
- ✅ Single output entity
- ✅ Simple argument passing
- ✅ Result used inline in expressions
- ✅ Cleaner, more readable

**Explicit instantiation:**
- ✅ Multiple outputs
- ✅ Named instance needed for debugging
- ✅ Optional ports (not all inputs provided)
- ✅ Complex configuration

### Common Stdlib Modules

**std::fp::math** - Mathematical operations
- `sqrt`, `rsqrt`, `abs`, `min`, `max`, `clamp`, `lerp`, `saturate`

**std::vec::basic** - Basic vector operations
- `dot`, `cross`, `length`, `distance`

**std::vec::geometry** - Geometric operations
- `normalize`, `reflect`, `refract`, `project`, `reject`

## Open Questions

1. **Should all single-output entities be implicitly callable?**
   - Option A: Only stdlib entities with registry entries
   - Option B: Any entity with single output can be called as function
   - **Recommendation:** Start with Option A for control

2. **How to handle entity parameters (generic types)?**
   - Current: Infer from argument types
   - Alternative: Allow explicit type parameters `dot<fp32>(a, b)`
   - **Recommendation:** Infer by default, allow explicit when ambiguous

3. **Should we support method call syntax?**
   - Example: `v.normalize()` instead of `normalize(v)`
   - **Recommendation:** Future enhancement, start with function calls

4. **Error recovery for typos?**
   - Example: User types `normailze(v)` instead of `normalize(v)`
   - Suggestion: "Did you mean 'normalize'?"
   - **Recommendation:** Yes, implement fuzzy matching for suggestions

## Timeline

- **Week 1-2:** Parser changes (function call expressions)
- **Week 3:** Stdlib registry implementation and data
- **Week 4:** HIR builder integration
- **Week 5:** Type inference enhancements
- **Week 6:** Error messages and testing
- **Week 7-8:** Documentation and refinement

**Total:** ~2 months for full implementation

## Success Criteria

- ✅ Function-like syntax works for all prelude operations
- ✅ Type-based overload resolution works correctly
- ✅ Implicit and explicit instantiation coexist without conflicts
- ✅ Clear error messages for resolution failures
- ✅ No performance degradation in compilation time
- ✅ Identical synthesized hardware as explicit instantiation
- ✅ Comprehensive test coverage

## Summary: Two Orthogonal Features

This design works in conjunction with STDLIB_USAGE_DESIGN.md:

| Feature | Purpose | Example |
|---------|---------|---------|
| **`use` statements** | Bring entity names into scope | `use std::fp::math::sqrt;` |
| **Implicit instantiation** | Call imported entities function-like | `result = sqrt(x)` |

**Both are needed:**
1. `use` statement brings `sqrt` into namespace
2. Implicit instantiation lets you write `sqrt(x)` instead of `inst s: FP32Sqrt { x = x, result => result }`

## Conclusion

Implicit instantiation makes SKALP stdlib ergonomic and familiar while maintaining explicit hardware semantics. The strong type system enables unambiguous resolution, and the registry-based approach scales to large stdlib catalogs.

**Key insight:** `result = sqrt(x)` looks like a software function call but **is** a hardware module instantiation - SKALP gets the best of both worlds.

**Import model:** All stdlib entities require `use` statements (from STDLIB_USAGE_DESIGN.md)
**Instantiation model:** Imported entities can be instantiated implicitly via function syntax (this design)
