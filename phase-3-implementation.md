# Phase 3: Clock Domains & Types

**Goal:** Implement clock domain safety and advanced type system for hardware design

**Duration:** 4 weeks

**Success Test:** Detect CDC violations at compile time and implement Valid/Ready protocol

---

## 🎯 TASKS

**Core Work:**
- [ ] Implement clock domain as lifetime syntax (`<'clk>`)
- [ ] Add clock domain inference and tracking throughout HIR/MIR
- [ ] Implement CDC (Clock Domain Crossing) detection at compile time
- [ ] Design and implement protocol definition syntax
- [ ] Add master/slave endpoint support for protocols
- [ ] Implement generics and parameters for entities
- [ ] Add struct types (packed/unpacked) to type system
- [ ] Implement enum and union types
- [ ] Add fixed-point type support
- [ ] Enhanced MIR generation with protocol expansion
- [ ] Advanced optimization passes (CSE, resource sharing)

**Testing:**
- [ ] Test clock domain inference on multi-clock designs
- [ ] Test CDC detection with various crossing patterns
- [ ] Test protocol definitions and instantiation
- [ ] Test generic entity parameterization
- [ ] Test advanced type system features
- [ ] Integration tests with existing Phase 2 functionality

**Documentation:**
- [ ] Update language specification with clock domain syntax
- [ ] Document protocol definition and usage patterns
- [ ] Create examples showing CDC detection
- [ ] Document generic and advanced type usage
- [ ] Update Phase 3 user guide

---

## ✅ COMPLETION CRITERIA

**This phase is done when:**
- [ ] Clock domains can be declared and tracked through compilation
- [ ] CDC violations are detected and reported with clear error messages
- [ ] Protocol definitions work with master/slave endpoints
- [ ] Generic entities can be parameterized and instantiated
- [ ] Advanced types (struct, enum, union, fixed-point) are fully supported
- [ ] All existing functionality continues to work with new features

**Success Test:** Create a multi-clock design with protocols that correctly detects CDC violations and compiles when fixed

---

## 📈 PROGRESS

**Daily Log:**
```
[Date] - [What got done] - [Any blockers]
```

**Blockers:**
- [ ] Any current issues blocking progress

---

## 🔧 TECHNICAL DETAILS

**Clock Domain System:**
- Lifetime-based syntax: `entity<'clk> Counter { in clk: clock<'clk>, ... }`
- Clock domain inference algorithm
- Cross-domain signal detection
- Synchronizer insertion points

**Protocol System:**
- Protocol definitions with direction inference
- Master/slave endpoint types
- Protocol composition and inheritance
- Valid/Ready protocol implementation

**Advanced Types:**
- Struct types with packing control
- Enum types with explicit values
- Union types for hardware multiplexing
- Fixed-point arithmetic types
- Generic type parameters

**MIR Enhancements:**
- Protocol expansion in MIR
- Clock domain annotations
- Advanced optimization passes
- Resource sharing analysis

---

**When done, run `/complete-phase` again for Phase 4: GPU Simulation**