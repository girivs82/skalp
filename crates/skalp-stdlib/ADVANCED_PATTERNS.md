# Advanced SKALP Design Patterns

This document showcases advanced SKALP language features through real-world examples from the standard library.

## Table of Contents

1. [Parametric Types and Generics](#parametric-types-and-generics)
2. [Trait System](#trait-system)
3. [Associated Types and Constants](#associated-types-and-constants)
4. [Default Implementations](#default-implementations)
5. [Trait Bounds and Where Clauses](#trait-bounds-and-where-clauses)
6. [Trait Composition](#trait-composition)
7. [Module Composition](#module-composition)
8. [Type-Level Programming](#type-level-programming)

---

## Parametric Types and Generics

SKALP supports generic types that are parameterized over element types, enabling code reuse while maintaining type safety.

### Built-in Parametric Types

```skalp
// Vector types with element type parameter
vec2<fp32>    // 2D vector of 32-bit floats
vec3<bit<16>> // 3D vector of 16-bit integers
vec4<fp64>    // 4D vector of 64-bit floats
```

### Generic Entity Definitions

From `vec_ops.sk`:

```skalp
/// Component-wise addition of two 2D vectors
entity Vec2Add<T> where T: Synthesizable {
    in a: vec2<T>
    in b: vec2<T>
    out result: vec2<T>
}

impl<T> Vec2Add<T> {
    result = vec2::<T> {
        x: a.x + b.x,
        y: a.y + b.y
    }
}
```

**Key Features:**
- `<T>` declares a type parameter
- `where T: Synthesizable` constrains T to hardware-synthesizable types
- Same entity works for vec2<fp32>, vec2<bit<8>>, etc.
- Hardware instantiates specialized versions at compile time

### Using Generic Entities

```skalp
entity MyDesign {
    in a: vec3<fp32>
    in b: vec3<fp32>
    out sum: vec3<fp32>
}

impl MyDesign {
    // Instantiate Vec3Add with T = fp32
    inst adder: Vec3Add<fp32> {
        a = a,
        b = b,
        result => sum
    }
}
```

---

## Trait System

Traits define interfaces that types can implement, enabling polymorphism and generic programming.

### Basic Trait Definition

From `fp/traits.sk`:

```skalp
trait FloatingPoint {
    /// Bit width of this floating-point format
    const WIDTH: nat

    /// Exponent width
    const EXP_WIDTH: nat

    /// Mantissa width
    const MANT_WIDTH: nat

    // Basic arithmetic operations
    fn add(self, other: Self) -> Self
    fn sub(self, other: Self) -> Self
    fn mul(self, other: Self) -> Self
    fn div(self, other: Self) -> Self

    // Comparison operations
    fn less_than(self, other: Self) -> bit
    fn equal(self, other: Self) -> bit

    // Classification
    fn is_nan(self) -> bit
    fn is_zero(self) -> bit

    // Factory methods
    fn zero() -> Self
    fn one() -> Self
}
```

**Key Concepts:**
- Traits define required methods and constants
- `Self` refers to the implementing type
- Associated constants provide compile-time type information
- Methods can be instance methods (`self`) or static methods (no `self`)

### Implementing Traits

```skalp
impl Synthesizable for fp32 {
    const IS_SYNTH: bit = 1
}

impl Comparable for fp32 {
    fn compare(self, other: Self) -> bit<2> {
        if self.is_nan() || other.is_nan() {
            0b11  // Unordered
        } else if self < other {
            0b00  // Less than
        } else if self == other {
            0b01  // Equal
        } else {
            0b10  // Greater than
        }
    }
}
```

---

## Associated Types and Constants

Traits can define associated types and constants that implementers must specify.

### Associated Constants

From `vec/traits.sk`:

```skalp
trait Vector<T> where T: Synthesizable {
    /// Number of components in this vector
    const DIM: nat

    /// Type of each component
    type Scalar = T

    // ... methods
}

impl Vector<fp32> for vec2<fp32> {
    const DIM: nat = 2
    type Scalar = fp32

    // ... method implementations
}
```

**Benefits:**
- Compile-time type information
- Generic algorithms can query type properties
- Enables dimension-aware generic code

### Using Associated Constants

```skalp
entity GenericVectorSum<V, T> where V: Vector<T> {
    in v: V
    out component_count: nat
}

impl<V, T> GenericVectorSum<V, T> {
    // Access associated constant
    component_count = V::DIM
}
```

---

## Default Implementations

Traits can provide default implementations that derive complex operations from simpler ones.

### Default Method Implementations

From `fp/traits.sk`:

```skalp
trait FloatingPoint {
    // Required methods
    fn add(self, other: Self) -> Self
    fn sub(self, other: Self) -> Self
    fn is_negative(self) -> bit
    fn greater_than(self, other: Self) -> bit

    // Default implementations
    fn abs(self) -> Self {
        if self.is_negative() {
            Self::zero().sub(self)
        } else {
            self
        }
    }

    fn max(self, other: Self) -> Self {
        if self.greater_than(other) { self } else { other }
    }

    fn min(self, other: Self) -> Self {
        if self.less_than(other) { self } else { other }
    }

    fn clamp(self, min_val: Self, max_val: Self) -> Self {
        self.max(min_val).min(max_val)
    }
}
```

**Key Advantages:**
- Define complex operations in terms of primitives
- Implementers only need to implement required methods
- Default implementations can be overridden for optimization

### Layered Defaults

From `vec/traits.sk`:

```skalp
trait Vector<T> {
    fn dot(self, other: Self) -> T  // Required

    // Default: length squared using dot product
    fn length_sq(self) -> T {
        self.dot(self)
    }
}

trait FloatVector<T>: Vector<T> where T: FloatingPoint {
    // Default: length using sqrt of length_sq
    fn length(self) -> T {
        self.length_sq().sqrt()
    }

    fn distance(self, other: Self) -> T {
        self.sub(other).length()
    }

    fn distance_sq(self, other: Self) -> T {
        self.sub(other).length_sq()
    }
}
```

---

## Trait Bounds and Where Clauses

Generic types can be constrained using trait bounds to ensure they support required operations.

### Basic Trait Bounds

```skalp
// T must implement Synthesizable
entity Vec2Add<T> where T: Synthesizable {
    in a: vec2<T>
    in b: vec2<T>
    out result: vec2<T>
}
```

### Multiple Bounds

```skalp
// T must implement both FloatingPoint and Synthesizable
trait FloatVector<T>: Vector<T> where T: FloatingPoint + Synthesizable {
    fn normalize(self) -> Self
    fn length(self) -> T
}
```

### Method-Specific Bounds

From `vec/traits.sk`:

```skalp
trait Vector<T> where T: Synthesizable {
    // This method has an additional bound on T
    fn lerp(self, other: Self, t: T) -> Self where T: FloatingPoint {
        let diff = other.sub(self);
        self.add(diff.scale(t))
    }

    // This method requires even more specific operations
    fn refract(self, normal: Self, eta: T) -> Self where T: FloatMath {
        // Can use sqrt(), exp(), etc. from FloatMath
        // ...
    }
}
```

**Pattern:**
- Trait-level bounds apply to all methods
- Method-level bounds add additional constraints
- Enables methods to require different capabilities

---

## Trait Composition

Traits can build on other traits, creating hierarchies of capabilities.

### Trait Inheritance

From `vec/traits.sk`:

```skalp
// Base trait
trait Vector<T> where T: Synthesizable {
    fn add(self, other: Self) -> Self
    fn dot(self, other: Self) -> T
}

// Specialized trait extends Vector
trait Vector2<T>: Vector<T> where T: Synthesizable {
    fn x(self) -> T
    fn y(self) -> T
    fn new(x: T, y: T) -> Self

    // 2D-specific operation
    fn cross_scalar(self, other: Self) -> T
}

// Another specialized trait
trait Vector3<T>: Vector<T> where T: Synthesizable {
    fn x(self) -> T
    fn y(self) -> T
    fn z(self) -> T
    fn new(x: T, y: T, z: T) -> Self

    // 3D-specific operation
    fn cross(self, other: Self) -> Self
}
```

### Multiple Trait Bounds

```skalp
trait FloatVector<T>: Vector<T> where T: FloatingPoint {
    // Inherits all Vector<T> methods
    // Plus adds floating-point specific operations
    fn normalize(self) -> Self
    fn reflect(self, normal: Self) -> Self
}

// Implementation must satisfy both Vector<T> and FloatVector<T>
impl FloatVector<fp32> for vec2<fp32> where fp32: FloatingPoint {
    // ...
}
```

---

## Module Composition

Complex hardware modules are built by instantiating and connecting simpler modules.

### Hierarchical Module Instantiation

From `ray_sphere_intersection.sk`:

```skalp
entity RaySphereIntersection {
    in ray_origin: vec3<fp32>
    in ray_direction: vec3<fp32>
    in sphere_center: vec3<fp32>
    in sphere_radius: fp32

    out hit: bit
    out hit_point: vec3<fp32>
}

impl RaySphereIntersection {
    // Step 1: Vector subtraction
    inst compute_oc: Vec3Sub<fp32> {
        a = ray_origin,
        b = sphere_center
    }

    signal oc: vec3<fp32> = compute_oc.result

    // Step 2: Dot product
    inst compute_a: Vec3Dot<fp32> {
        a = ray_direction,
        b = ray_direction
    }

    signal a: fp32 = compute_a.result

    // Step 3: More dot products
    inst compute_half_b: Vec3Dot<fp32> {
        a = oc,
        b = ray_direction
    }

    // Step 4: FP multiplication
    inst radius_sq_mul: FP32Mul {
        a = sphere_radius,
        b = sphere_radius
    }

    // ... and so on
}
```

**Pattern:**
- Break complex operations into submodules
- Connect submodule outputs to signals
- Use signals as inputs to subsequent submodules
- Creates a dataflow graph that synthesizes to hardware

### Reusing Submodules

```skalp
entity Vec3Clamp {
    in v: vec3<fp32>
    in min_val: vec3<fp32>
    in max_val: vec3<fp32>
    out result: vec3<fp32>
}

impl Vec3Clamp {
    // First clamp to minimum
    inst clamped_min: Vec3Max {
        a = v,
        b = min_val
    }

    // Then clamp to maximum
    inst clamped: Vec3Min {
        a = clamped_min.result,
        b = max_val,
        result => result
    }
}
```

---

## Type-Level Programming

Use const generics and associated constants for compile-time computation.

### Const Generic Parameters

From `vec/traits.sk`:

```skalp
/// Generic N-dimensional vector operations
trait VectorN<T, const N: nat>: Vector<T>
where
    T: Synthesizable,
    N: > 0
{
    /// Sum of all components
    fn sum(self) -> T

    /// Product of all components
    fn product(self) -> T

    /// Average of all components
    fn average(self) -> T where T: FloatingPoint {
        let n = T::from_int(N);  // Convert compile-time N to runtime value
        self.sum().div(n)
    }
}
```

**Features:**
- `const N: nat` is a compile-time constant parameter
- Can be used in computations: `N: > 0` constraint
- Can be converted to runtime values: `T::from_int(N)`

### Type-Level Computation Example

```skalp
entity PackedArray<T, const N: nat> where T: Synthesizable {
    in elements: [T; N]
    out packed: bit<{T::WIDTH * N}>  // Computed bit width
}

impl<T, const N: nat> PackedArray<T, N> {
    // Width automatically computed from T's width and N
    packed = /* pack elements */
}
```

---

## Complete Example: Putting It All Together

Here's a complete example showing multiple patterns:

```skalp
// Generic vector math entity using traits
entity VectorGeometry<V, T>
where
    V: FloatVector<T> + Vector3<T>,
    T: FloatingPoint + FloatMath
{
    in v1: V
    in v2: V
    in normal: V

    out dot_product: T
    out cross_product: V
    out reflected: V
    out angle: T
}

impl<V, T> VectorGeometry<V, T> {
    // Use trait methods
    dot_product = v1.dot(v2)
    cross_product = v1.cross(v2)
    reflected = v1.reflect(normal)

    // Use method with additional trait bound
    angle = v1.angle_between(v2)  // Requires FloatMath for acos
}
```

**This example demonstrates:**
1. ✅ Generic type parameters (`V`, `T`)
2. ✅ Multiple trait bounds (`FloatVector<T> + Vector3<T>`)
3. ✅ Trait composition (FloatVector extends Vector)
4. ✅ Associated types (T is the scalar type)
5. ✅ Trait methods (dot, cross, reflect, angle_between)
6. ✅ Method-specific bounds (angle_between needs FloatMath)

---

## Summary of Advanced Features

| Feature | Example | Hardware Benefit |
|---------|---------|------------------|
| **Parametric Types** | `vec3<T>` | Code reuse across data types |
| **Generic Entities** | `Vec2Add<T>` | Flexible, type-safe modules |
| **Traits** | `FloatingPoint` | Interface definitions |
| **Associated Constants** | `const DIM: nat` | Compile-time type info |
| **Default Implementations** | `fn clamp()` | Reduce code duplication |
| **Trait Bounds** | `where T: Synthesizable` | Ensure hardware compatibility |
| **Trait Composition** | `FloatVector: Vector` | Hierarchical capabilities |
| **Module Composition** | `inst adder: FP32Add` | Build complex from simple |
| **Const Generics** | `const N: nat` | Parameterize by size |

---

## Best Practices

### 1. Use Traits for Interfaces

Define operations as traits, implement as entities:

```skalp
// Define interface
trait Adder<T> {
    fn add(self, other: T) -> T
}

// Implement for specific type
entity FP32AdderImpl { ... }

impl Adder<fp32> for FP32AdderImpl {
    fn add(self, other: fp32) -> fp32 { ... }
}
```

### 2. Leverage Default Implementations

Build complex operations from primitives:

```skalp
trait Vector<T> {
    fn add(self, other: Self) -> Self  // Required
    fn scale(self, s: T) -> Self       // Required

    // Default: derived from add and scale
    fn lerp(self, other: Self, t: T) -> Self {
        self.add(other.sub(self).scale(t))
    }
}
```

### 3. Use Type Bounds for Safety

Constrain types to ensure hardware compatibility:

```skalp
// Only synthesizable types allowed
entity VectorProcessor<T> where T: Synthesizable {
    // ...
}

// Only FP types with math operations
fn normalize<T>(v: vec3<T>) -> vec3<T> where T: FloatingPoint + FloatMath {
    // ...
}
```

### 4. Compose Modules Hierarchically

Build complex designs from simple, tested components:

```skalp
entity RayTracer {
    inst intersection: RaySphereIntersection { ... }
    inst shading: PhongShading { ... }
    inst compositor: Blend { ... }
}
```

---

## Conclusion

SKALP's advanced features enable:
- **Generic, reusable hardware modules**
- **Type-safe interfaces via traits**
- **Compile-time optimization via const generics**
- **Hierarchical design through module composition**
- **Powerful abstractions that map to efficient hardware**

These patterns are demonstrated throughout the `skalp-stdlib` crate and serve as reference implementations for advanced SKALP programming.
