# Phase 3: Clock Domains & Types (Remaining Tasks)

**Goal:** Complete clock domain safety and remaining advanced type features

**Duration:** 2-3 weeks remaining (Advanced types already complete)

**Success Test:** Detect CDC violations at compile time, implement complete type system with clock domains

---

## 🎯 REMAINING TASKS

**Core Work:**
- [ ] **Clock domain as lifetime syntax (`<'clk>`)**
  - Parse clock domain lifetime annotations in entity definitions
  - Add clock domain tracking to HIR types
  - Implement lifetime inference for signals and ports
- [ ] **Clock domain inference and tracking throughout HIR/MIR**
  - Propagate clock domain information through compilation pipeline
  - Add clock domain checking to signal assignments
  - Track domain boundaries in hierarchical designs
- [ ] **CDC (Clock Domain Crossing) detection at compile time**
  - Implement CDC violation detection between different clock domains
  - Add compile-time warnings/errors for unsafe crossings
  - Suggest proper synchronizer usage
- [ ] **Protocol definitions with master/slave endpoints**
  - Add protocol syntax to parser (AXI, custom protocols)
  - Implement protocol interface validation
  - Add protocol compatibility checking
- [ ] **Generics and parameters for entities**
  - Extend entity syntax to support generic parameters
  - Add parameter resolution and monomorphization
  - Implement generic type checking

**Testing:**
- [ ] Test clock domain annotation parsing
- [ ] Test CDC detection with multiple clock domains
- [ ] Test protocol definition and usage
- [ ] Test generic entity instantiation
- [ ] Test integration with existing advanced types

**Documentation:**
- [ ] Update language specification with clock domain syntax
- [ ] Document CDC detection capabilities
- [ ] Document protocol definition syntax

---

## ✅ COMPLETION CRITERIA

**This phase is done when:**
- [ ] Clock domain syntax fully working with lifetime annotations
- [ ] CDC violations detected at compile time with clear error messages
- [ ] Protocol definitions can be created and validated
- [ ] Generic entities can be defined and instantiated
- [ ] All features integrate with existing advanced types system

**Success Test:** Compile a multi-clock design with CDC detection and protocol interfaces

---

## 📈 PROGRESS

**Already Complete (Week 3):**
- ✅ Struct types (packed/unpacked) with field access and bit slicing
- ✅ Enum and union types with variant resolution
- ✅ Enhanced type checker for composite types
- ✅ SystemVerilog typedef generation
- ✅ Complete end-to-end compilation pipeline for advanced types
- ✅ Fixed continuous assignment generation
- ✅ Proper enum value resolution
- ✅ Integration testing and conflict resolution

**Daily Log:**
```
Sep 30, 2024 - Advanced types system completed - All struct/enum features working
```

**Next Priority:** Clock domain lifetime syntax implementation

---

**When done, run `/complete-phase` again for Phase 4**