# SKALP Standard Library - Documentation Index

Complete guide to finding what you need in the stdlib documentation.

## Start Here

### 👋 First Time User?
**[QUICK_START.md](QUICK_START.md)** - Get started in 5 minutes with working examples

### 📚 Need Complete Details?
**[STDLIB_REFERENCE.md](STDLIB_REFERENCE.md)** - Comprehensive reference with all types, operations, and examples

### ✨ NEW Operations!
**[NEW_OPERATIONS.md](NEW_OPERATIONS.md)** - 48 newly added operations including sqrt, normalize, reflect, refract, and more!

### 🎓 Want to Learn Advanced Features?
**[ADVANCED_PATTERNS.md](ADVANCED_PATTERNS.md)** - Generic programming, traits, and design patterns

## Documentation by Topic

### Types

| Topic | Document | Section |
|-------|----------|---------|
| FP types overview | [STDLIB_REFERENCE.md](STDLIB_REFERENCE.md) | Type System → Floating-Point Types |
| Vector types overview | [STDLIB_REFERENCE.md](STDLIB_REFERENCE.md) | Type System → Vector Types |
| FP implementation details | [components/fp/README.md](components/fp/README.md) | IEEE 754 Format |
| Generic types | [ADVANCED_PATTERNS.md](ADVANCED_PATTERNS.md) | Parametric Types and Generics |

### Operations

| Topic | Document | Section |
|-------|----------|---------|
| FP arithmetic | [QUICK_START.md](QUICK_START.md) | Example 1 |
| Vector operations | [STDLIB_REFERENCE.md](STDLIB_REFERENCE.md) | Vector Operations |
| Built-in operators | [QUICK_START.md](QUICK_START.md) | Available Operations |
| Entity instantiation | [QUICK_START.md](QUICK_START.md) | Example 3 |

### Examples

| Topic | Document | Section |
|-------|----------|---------|
| Basic examples | [QUICK_START.md](QUICK_START.md) | Basic Examples |
| Usage patterns | [QUICK_START.md](QUICK_START.md) | Common Patterns |
| Complex examples | [STDLIB_REFERENCE.md](STDLIB_REFERENCE.md) | Usage Examples |
| Advanced patterns | [ADVANCED_PATTERNS.md](ADVANCED_PATTERNS.md) | Complete Example |

### Testing

| Topic | Document | Section |
|-------|----------|---------|
| Running tests | [QUICK_START.md](QUICK_START.md) | Testing Your Code |
| Test organization | [STDLIB_REFERENCE.md](STDLIB_REFERENCE.md) | Testing |
| Test results | [STDLIB_REFERENCE.md](STDLIB_REFERENCE.md) | Test Results |

### Status & Roadmap

| Topic | Document | Section |
|-------|----------|---------|
| What's working now | [README.md](README.md) | Status: Early Development |
| What's not available | [QUICK_START.md](QUICK_START.md) | What's NOT Available Yet |
| Implementation status | [STDLIB_REFERENCE.md](STDLIB_REFERENCE.md) | Implementation Status |
| Detailed status table | [components/README.md](components/README.md) | Implementation Status |

## Quick Reference Tables

### All Available Operations

**See:** [STDLIB_REFERENCE.md → Vector Operations](STDLIB_REFERENCE.md#vector-operations)

Comprehensive tables listing all Vec2/3/4 operations with signatures and file locations.

### All FP Operations

**See:** [STDLIB_REFERENCE.md → Floating-Point Operations](STDLIB_REFERENCE.md#floating-point-operations)

Detailed FP arithmetic entities with algorithm descriptions.

### Type Specifications

**See:** [STDLIB_REFERENCE.md → Type System](STDLIB_REFERENCE.md#type-system)

Complete type specifications including bit layouts and IEEE 754 details.

## By Use Case

### "I want to add two vectors"
→ [QUICK_START.md → Example 2](QUICK_START.md#2-vector-operations)

### "I need to compute a dot product"
→ [QUICK_START.md → Example 3](QUICK_START.md#3-dot-product)

### "How do I use floating-point numbers?"
→ [QUICK_START.md → Example 1](QUICK_START.md#1-floating-point-arithmetic)

### "I want to write generic code"
→ [ADVANCED_PATTERNS.md → Parametric Types](ADVANCED_PATTERNS.md#parametric-types-and-generics)

### "How do I instantiate stdlib entities?"
→ [QUICK_START.md → Example 3](QUICK_START.md#3-dot-product)

### "What operations are available?"
→ [QUICK_START.md → Available Operations](QUICK_START.md#available-operations)

### "Can I simulate my code?"
→ [QUICK_START.md → Testing Your Code](QUICK_START.md#testing-your-code)

### "What's the implementation status?"
→ [README.md → Status](README.md#status-early-development)

## Component Documentation

### FP Library
**[components/fp/README.md](components/fp/README.md)**
- IEEE 754 format details
- Special values (NaN, Inf, denormals)
- FP arithmetic entity implementations
- Algorithm descriptions

### Vector Library
**[components/README.md](components/README.md)**
- Vector operations overview
- Generic vector programming
- Geometric operations
- Implementation status

## Advanced Topics

### Trait System
**[ADVANCED_PATTERNS.md → Trait System](ADVANCED_PATTERNS.md#trait-system)**
- Trait definitions
- Implementing traits
- Associated types and constants
- Default implementations

### Module Composition
**[ADVANCED_PATTERNS.md → Module Composition](ADVANCED_PATTERNS.md#module-composition)**
- Hierarchical design
- Building complex from simple
- Entity instantiation patterns

### Type-Level Programming
**[ADVANCED_PATTERNS.md → Type-Level Programming](ADVANCED_PATTERNS.md#type-level-programming)**
- Const generics
- Compile-time computation
- Generic constraints

## File Organization

```
skalp-stdlib/
├── README.md                    # Overview and quick links
├── QUICK_START.md              # 5-minute getting started guide
├── STDLIB_REFERENCE.md         # Comprehensive reference
├── ADVANCED_PATTERNS.md        # Advanced features and patterns
├── DOCUMENTATION_INDEX.md      # This file
│
├── components/
│   ├── README.md               # Components overview
│   ├── fp/
│   │   ├── README.md          # FP library details
│   │   ├── fp32_add.sk        # FP addition implementation
│   │   ├── fp32_mul.sk        # FP multiplication
│   │   └── traits.sk          # FP trait definitions
│   ├── vec/
│   │   ├── vec_ops.sk         # Vector operations
│   │   └── traits.sk          # Vector trait definitions
│   └── ...                     # Other components
│
├── examples/
│   └── ray_sphere_intersection.sk  # Complex example
│
└── tests/                      # Test suite
    ├── test_stdlib_synthesis.rs
    ├── test_vec_arithmetic.rs
    └── ...
```

## Contributing

When adding new features, update:
1. **[STDLIB_REFERENCE.md](STDLIB_REFERENCE.md)** - Add to operation tables and implementation status
2. **[README.md](README.md)** - Update status section if complete
3. **Component README** - Add details to relevant components/*/README.md
4. **This index** - Add entries if new topics are covered

## Changelog Location

**[STDLIB_REFERENCE.md → Changelog](STDLIB_REFERENCE.md#changelog)**

Version history and feature additions.

---

**Last Updated:** Version 0.1.0

**Maintained by:** SKALP Project

**License:** Same as main SKALP project
