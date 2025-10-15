# SKALP IR Architecture Refactoring Plan

## Executive Summary

**Problem:** The SKALP compiler has duplication of critical transformations (struct flattening, type width calculation, signal naming) across multiple stages (HIR→MIR, MIR→SystemVerilog, MIR→SIR, SIR→Metal). This leads to:
- Code duplication and maintenance burden
- Bugs (e.g., struct signals not flattened in MIR-to-SIR causing simulation failures)
- Inconsistent behavior across codegens
- Difficulty adding new target backends

**Solution:** Systematically consolidate transformations into the appropriate IR level, ensuring each transformation happens exactly once and later stages properly leverage earlier IRs.

---

## Current Architecture Issues

### Issue 1: Incomplete Struct Flattening in HIR→MIR
**Location:** `crates/skalp-mir/src/hir_to_mir.rs`

**Problem:**
- Ports ARE flattened (lines 114-141): `vertex.position.x` → `vertex_position_x`
- Signals are PARTIALLY flattened (lines 169-199): Only created, but not tracked properly
- Signal ASSIGNMENTS to structs are NOT fully expanded
- Instance connections with struct ports not fully handled

**Impact:**
- MIR still contains `DataType::Struct(_)` in signal types
- Downstream passes (SIR, SystemVerilog) must handle struct expansion themselves
- Inconsistent flattening across different constructs

**Evidence:**
```rust
// In hir_to_mir.rs:
for signal in &impl_block.signals {
    // This flattens the signal declaration...
    let flattened_signals = self.flatten_signal(signal, ...);
    // ...but assignments to these signals aren't expanded
}
```

### Issue 2: SystemVerilog Codegen Re-implements Flattening
**Location:** `crates/skalp-codegen/src/systemverilog.rs`

**Problem:**
- Lines 960-1010: `flatten_port_to_signals()` - duplicates HIR→MIR logic
- Lines 1014-1140: `expand_struct_assignment()` - recursively expands struct assignments
- Lines 1149-1179: `expand_field_assignment()` - nested field expansion
- Lines 1184-1265: `expand_port_connection()` - instance port expansion

**Impact:**
- Same flattening logic implemented twice
- Different naming conventions possible
- MIR should already have flattened everything

**Evidence:**
```rust
// SystemVerilog codegen duplicates flattening:
fn expand_struct_assignment(&mut self, ...) {
    match &struct_type {
        HirType::Struct(st) => {
            for field in &st.fields {
                // Recursively flatten - should have been done in MIR!
            }
        }
    }
}
```

### Issue 3: Type Width Calculation Duplicated 4x
**Locations:**
1. `crates/skalp-codegen/src/systemverilog.rs:913-956` - `get_type_width()`
2. `crates/skalp-sim/src/mir_to_sir.rs:478-489` - `get_data_type_width()`
3. `crates/skalp-sir/src/mir_to_sir.rs:2928-2955` - `convert_type()`
4. `crates/skalp-sir/src/metal_codegen.rs:756-767` - `get_signal_width_from_sir()`

**Impact:**
- Same recursion pattern in 4 different places
- Potential for inconsistencies
- Harder to add new types

### Issue 4: MIR-to-SIR Treats Structs as 1-bit
**Location:** `crates/skalp-sir/src/mir_to_sir.rs:2946-2953`

**Problem:**
```rust
DataType::Struct(_) | DataType::Enum(_) | DataType::Union(_) => {
    eprintln!("Warning: Struct/Enum/Union types not yet supported in SIR, treating as 1-bit");
    SirType::Bits(1)  // ← BUG: Should never reach here if MIR is properly flattened
}
```

**Impact:**
- Struct-typed signals lose their data
- Simulation produces incorrect results (all zeros)
- Comment says "should be decomposed at a higher level" but they're not

### Issue 5: Signal Naming Conventions Computed Multiple Times
**Locations:**
- HIR→MIR: Computes `flattened_ports` HashMap
- SystemVerilog: Re-computes field names during codegen
- Metal: Re-computes signal locations in buffers

---

## Refactoring Principles

### Principle 1: Single Responsibility per IR Level
Each IR level should have a well-defined purpose:

- **HIR**: Preserve programmer intent; no transformations beyond type checking and monomorphization
- **MIR**: Hardware-ready IR; ALL structural flattening complete; suitable for ANY backend
- **LIR**: Gate-level netlist (technology mapping)
- **SIR**: GPU-optimized representation (derived from fully-flattened MIR)
- **Codegen**: Text emission only; NO transformations

### Principle 2: One Transformation, One Place
Each transformation should happen exactly once:

- Struct flattening: HIR→MIR ONLY
- Type lowering: HIR→MIR ONLY
- Assignment expansion: HIR→MIR ONLY
- Gate mapping: MIR→LIR ONLY
- Parallel extraction: MIR→SIR ONLY

### Principle 3: Downstream Leverage
Later stages should trust earlier transformations:

- MIR should NEVER contain `DataType::Struct(...)` in signal/port types
- SIR should NEVER need to handle struct expansion
- Codegen should NEVER need to compute type widths

---

## Refactoring Plan

### Phase 1: Create Shared Utility Modules

#### Step 1.1: Create `type_flattening.rs` Module
**Location:** `crates/skalp-mir/src/type_flattening.rs`

**Purpose:** Consolidate all struct/vector flattening logic

**API:**
```rust
pub struct TypeFlattener {
    next_id: u32,
}

impl TypeFlattener {
    /// Flatten a type into scalar fields
    pub fn flatten_type(
        &mut self,
        base_name: &str,
        data_type: &DataType,
    ) -> Vec<FlattenedField>;

    /// Get the flattened field path for a field access expression
    pub fn get_field_path(
        base_name: &str,
        field_chain: &[String],
    ) -> String;

    /// Expand a struct assignment into field-level assignments
    pub fn expand_struct_assignment(
        &mut self,
        lhs: &LValue,
        rhs: &Expression,
        struct_type: &StructType,
    ) -> Vec<Assignment>;
}

pub struct FlattenedField {
    pub name: String,
    pub data_type: DataType,  // Always scalar: Bit, Int, Nat, Float*, Clock, Reset
    pub field_path: Vec<String>,
    pub bit_offset: usize,
}
```

**Consumers:**
- `hir_to_mir.rs`: Use for port/signal flattening
- Remove from `systemverilog.rs`: Delete duplicate logic

#### Step 1.2: Create `type_width.rs` Module
**Location:** `crates/skalp-mir/src/type_width.rs`

**Purpose:** Consolidate type width calculations

**API:**
```rust
/// Calculate the bit width of a MIR data type
pub fn get_type_width(data_type: &DataType) -> usize;

/// Calculate total width of a flattened struct
pub fn get_struct_total_width(struct_type: &StructType) -> usize;

/// Calculate the bit offset of a field within a struct
pub fn get_field_offset(struct_type: &StructType, field_name: &str) -> usize;
```

**Consumers:**
- `hir_to_mir.rs`
- `mir_to_sir.rs` (both files)
- `systemverilog.rs`
- `metal_codegen.rs`

#### Step 1.3: Create `signal_naming.rs` Module
**Location:** `crates/skalp-mir/src/signal_naming.rs`

**Purpose:** Unified naming convention for flattened signals

**API:**
```rust
/// Standard naming for flattened struct fields
pub fn make_field_name(base: &str, field: &str) -> String {
    format!("{}_{}", base, field)
}

/// Standard naming for array elements
pub fn make_array_element_name(base: &str, index: usize) -> String {
    format!("{}_{}", base, index)
}

/// Standard naming for hierarchical instances
pub fn make_instance_signal_name(instance: &str, signal: &str) -> String {
    format!("{}.{}", instance, signal)
}
```

---

### Phase 2: Fix HIR→MIR Transformation

#### Step 2.1: Complete Signal Flattening
**Location:** `crates/skalp-mir/src/hir_to_mir.rs`

**Changes:**

1. **Flatten ALL signals** (currently incomplete)
```rust
// BEFORE: Signals created but not fully tracked
for signal in &impl_block.signals {
    let flattened_signals = self.flatten_signal(signal, ...);
    // Missing: Track flattened_signals properly
    for sig in flattened_signals {
        module.signals.push(sig);
    }
}

// AFTER: Full tracking and mapping
for signal in &impl_block.signals {
    let flattened_fields = self.flatten_signal(signal, ...);

    // Store flattening info for later lookups
    self.flattened_signals.insert(signal.id, flattened_fields.clone());

    // Create MIR signals for each flattened field
    let mut mir_signals = Vec::new();
    for field in &flattened_fields {
        let sig_id = self.next_signal_id();
        mir_signals.push(Signal {
            id: sig_id,
            name: field.name.clone(),
            signal_type: field.data_type.clone(),  // Always scalar!
            initial: None,
            clock_domain: None,
        });
    }

    // Map HIR signal ID to first MIR signal (for simple references)
    if let Some(first) = mir_signals.first() {
        self.signal_map.insert(signal.id, first.id);
    }

    module.signals.extend(mir_signals);
}
```

2. **Expand ALL struct assignments**
```rust
// BEFORE: Struct assignments passed through as-is
fn convert_assignment(&mut self, assign: &HirAssignment) -> Statement {
    Statement::Assignment(Assignment {
        lhs: self.convert_lvalue(&assign.lhs),
        rhs: self.convert_expression(&assign.rhs),
        kind: ...
    })
}

// AFTER: Expand struct assignments to field-level
fn convert_assignment(&mut self, assign: &HirAssignment) -> Statement {
    // Check if LHS is a struct-typed signal
    if let Some(struct_type) = self.get_lvalue_struct_type(&assign.lhs) {
        // Expand into multiple field assignments
        let field_assignments = self.type_flattener.expand_struct_assignment(
            &assign.lhs,
            &assign.rhs,
            struct_type,
        );

        // Return a block of field assignments
        Statement::Block(Block {
            statements: field_assignments
                .into_iter()
                .map(Statement::Assignment)
                .collect(),
        })
    } else {
        // Scalar assignment - pass through
        Statement::Assignment(...)
    }
}
```

3. **Expand struct field access expressions**
```rust
// BEFORE: Field access might create complex expressions
HirExpression::FieldAccess { base, field } => {
    Expression::Ref(...)  // Complex nested LValue
}

// AFTER: Resolve to flattened signal name directly
HirExpression::FieldAccess { base, field } => {
    let flattened_name = self.resolve_field_access(base, field);
    let signal_id = self.signal_map.get(&flattened_name).unwrap();
    Expression::Ref(LValue::Signal(*signal_id))
}
```

#### Step 2.2: Assert MIR Invariants
**Location:** `crates/skalp-mir/src/hir_to_mir.rs`

**Add validation after transformation:**
```rust
impl HirToMir {
    pub fn transform(&mut self, hir: &Hir) -> Mir {
        // ... existing transformation ...
        let mir = ...;

        // CRITICAL: Validate MIR invariants
        self.validate_mir_invariants(&mir);

        mir
    }

    fn validate_mir_invariants(&self, mir: &Mir) {
        for module in &mir.modules {
            // Invariant 1: No struct-typed ports
            for port in &module.ports {
                assert!(
                    !matches!(port.port_type, DataType::Struct(_)),
                    "BUG: Port '{}' has struct type after HIR→MIR. \
                     All ports must be flattened to scalars.",
                    port.name
                );
            }

            // Invariant 2: No struct-typed signals
            for signal in &module.signals {
                assert!(
                    !matches!(signal.signal_type, DataType::Struct(_)),
                    "BUG: Signal '{}' has struct type after HIR→MIR. \
                     All signals must be flattened to scalars.",
                    signal.name
                );
            }

            // Invariant 3: No field access expressions
            for process in &module.processes {
                self.validate_no_field_access(&process.body);
            }
        }
    }
}
```

---

### Phase 3: Simplify MIR→SIR Transformation

#### Step 3.1: Remove Struct Handling
**Location:** `crates/skalp-sir/src/mir_to_sir.rs:2946-2953`

**Changes:**
```rust
// BEFORE: Handle structs as 1-bit (WRONG)
DataType::Struct(_) | DataType::Enum(_) | DataType::Union(_) => {
    eprintln!("Warning: Struct/Enum/Union types not yet supported in SIR, treating as 1-bit");
    SirType::Bits(1)
}

// AFTER: Structs should NEVER appear in MIR - panic if they do
DataType::Struct(st) | DataType::Enum(e) | DataType::Union(u) => {
    panic!(
        "INTERNAL ERROR: MIR contains composite type '{:?}' which should have been \
         flattened during HIR→MIR transformation. This indicates a bug in hir_to_mir.rs.",
        data_type
    );
}
```

#### Step 3.2: Use Shared Width Calculation
**Location:** `crates/skalp-sir/src/mir_to_sir.rs`

**Changes:**
```rust
// BEFORE: Local implementation
fn get_data_type_width(&self, data_type: &DataType) -> usize {
    match data_type {
        DataType::Bit(w) => *w,
        DataType::Logic(w) => *w,
        // ... etc
    }
}

// AFTER: Use shared utility
use skalp_mir::type_width::get_type_width;

// Remove local implementation, use:
let width = get_type_width(&signal.signal_type);
```

---

### Phase 4: Simplify SystemVerilog Codegen

#### Step 4.1: Remove Duplicate Flattening Logic
**Location:** `crates/skalp-codegen/src/systemverilog.rs`

**Remove:**
- Lines 960-1010: `flatten_port_to_signals()` - DELETE
- Lines 1014-1140: `expand_struct_assignment()` - DELETE
- Lines 1149-1179: `expand_field_assignment()` - DELETE
- Lines 1184-1265: `expand_port_connection()` - DELETE

**Rationale:** MIR is already fully flattened, so SystemVerilog codegen just emits the flattened signals as-is.

#### Step 4.2: Simplify Port/Signal Emission
**Changes:**
```rust
// BEFORE: Complex expansion logic
fn emit_port(&mut self, port: &Port) {
    if matches!(port.port_type, DataType::Struct(_)) {
        self.flatten_port_to_signals(port);  // COMPLEX!
    } else {
        self.emit_scalar_port(port);
    }
}

// AFTER: Simple emission (all ports are scalar)
fn emit_port(&mut self, port: &Port) {
    // All ports are guaranteed to be scalar after HIR→MIR
    let width = get_type_width(&port.port_type);
    let direction = match port.direction {
        PortDirection::Input => "input",
        PortDirection::Output => "output",
        PortDirection::InOut => "inout",
    };

    writeln!(self.output, "    {} logic [{}:0] {},",
             direction, width - 1, port.name);
}
```

#### Step 4.3: Simplify Assignment Emission
**Changes:**
```rust
// BEFORE: Check for struct assignments and expand
fn emit_assignment(&mut self, assign: &Assignment) {
    if self.is_struct_assignment(assign) {
        self.expand_struct_assignment(assign);  // COMPLEX!
    } else {
        self.emit_scalar_assignment(assign);
    }
}

// AFTER: Simple emission (all assignments are scalar)
fn emit_assignment(&mut self, assign: &Assignment) {
    // All assignments are guaranteed to be scalar after HIR→MIR
    write!(self.output, "{} = ", self.emit_lvalue(&assign.lhs));
    self.emit_expression(&assign.rhs);
    writeln!(self.output, ";");
}
```

---

### Phase 5: Simplify Metal Codegen

#### Step 5.1: Use Shared Width Calculation
**Location:** `crates/skalp-sir/src/metal_codegen.rs:756-767`

**Changes:**
```rust
// BEFORE: Local signal width calculation
fn get_signal_width_from_sir(&self, signal_name: &str) -> usize {
    if let Some(signal) = self.sir.signals.iter().find(|s| s.name == signal_name) {
        return signal.width;
    }
    // ... fallback logic
}

// AFTER: Trust SIR signal widths (which come from MIR type widths)
fn get_signal_width(&self, signal_name: &str) -> usize {
    self.sir.signals
        .iter()
        .find(|s| s.name == signal_name)
        .map(|s| s.width)
        .unwrap_or_else(|| panic!("Signal '{}' not found in SIR", signal_name))
}
```

---

### Phase 6: Update MIR DataType Definition

#### Step 6.1: Mark Composite Types as Pre-Flattening Only
**Location:** `crates/skalp-mir/src/mir.rs:114-164`

**Add documentation:**
```rust
/// Data types in MIR
///
/// IMPORTANT INVARIANT: After HIR→MIR transformation, the following types
/// should NEVER appear in Port::port_type or Signal::signal_type:
/// - DataType::Struct(_)
/// - DataType::Enum(_)
/// - DataType::Union(_)
///
/// These types exist in the MIR DataType enum ONLY for use during the
/// HIR→MIR transformation itself (for recursive flattening). Once transformation
/// is complete, all ports and signals must use only scalar types:
/// - Bit, Logic, Int, Nat
/// - Clock, Reset, Event
/// - Float16, Float32, Float64
/// - Vec2/Vec3/Vec4 (these are flattened to component fields)
/// - Array (flattened to individual elements)
///
/// If you see a "composite type should have been flattened" panic, it means
/// hir_to_mir.rs did not properly flatten a struct/enum/union.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum DataType {
    /// Bit vector (synthesis-friendly)
    Bit(usize),
    // ... etc

    /// Struct type - ONLY used during HIR→MIR transformation, never in final MIR
    Struct(Box<StructType>),
    /// Enum type - ONLY used during HIR→MIR transformation, never in final MIR
    Enum(Box<EnumType>),
    /// Union type - ONLY used during HIR→MIR transformation, never in final MIR
    Union(Box<UnionType>),
}
```

---

### Phase 7: Update LIR (Future Work)

**Scope:** Currently LIR is minimally used. Future refactoring should:

1. Define clear MIR→LIR transformation
2. Implement technology mapping
3. Use LIR for physical design tools
4. Potentially use LIR for formal verification

**Out of scope for current refactoring.**

---

## Testing Strategy

### Test 1: Struct Signal Flattening
**File:** `tests/test_struct_signal_flattening.rs`

```rust
#[test]
fn test_struct_signal_flattened_in_mir() {
    let source = r#"
        struct Point { x: bit[32], y: bit[32] }
        entity Test {
            in clk: clock
        }
        impl Test {
            signal pos: Point
            on(clk.rise) {
                pos <= Point { x: 10, y: 20 }
            }
        }
    "#;

    let hir = parse_and_build_hir(source).unwrap();
    let mir = HirToMir::new().transform(&hir);

    // Verify: No struct types in signals
    for module in &mir.modules {
        for signal in &module.signals {
            assert!(
                !matches!(signal.signal_type, DataType::Struct(_)),
                "Signal '{}' should be flattened", signal.name
            );
        }
    }

    // Verify: Flattened signals exist
    let module = &mir.modules[0];
    assert!(module.signals.iter().any(|s| s.name == "pos_x"));
    assert!(module.signals.iter().any(|s| s.name == "pos_y"));
}
```

### Test 2: MIR-to-SIR No Structs
**File:** `tests/test_mir_to_sir_no_structs.rs`

```rust
#[test]
#[should_panic(expected = "should have been flattened")]
fn test_mir_to_sir_panics_on_struct() {
    // Manually construct invalid MIR with struct type
    let mut mir = Mir::new("test".to_string());
    let module = Module {
        signals: vec![Signal {
            id: SignalId(0),
            name: "bad_signal".to_string(),
            signal_type: DataType::Struct(Box::new(StructType {
                name: "BadStruct".to_string(),
                fields: vec![],
                packed: false,
            })),
            initial: None,
            clock_domain: None,
        }],
        ..Default::default()
    };
    mir.modules.push(module);

    // Should panic because structs should never reach MIR→SIR
    convert_mir_to_sir_with_hierarchy(&mir, &mir.modules[0]);
}
```

### Test 3: End-to-End Pipeline Test
**File:** Current failing test should pass after refactoring

```rust
#[tokio::test]
async fn test_graphics_pipeline_multi_clock_domains() {
    // This test currently fails because struct signals aren't flattened
    // After refactoring, it should pass
    // ... existing test code ...
}
```

---

## Implementation Order

### Week 1: Foundation
1. **Day 1-2:** Create shared utility modules
   - `type_flattening.rs`
   - `type_width.rs`
   - `signal_naming.rs`

2. **Day 3-5:** Fix HIR→MIR transformation
   - Complete signal flattening
   - Expand struct assignments
   - Add MIR validation

### Week 2: Simplification
3. **Day 1-2:** Update MIR→SIR transformation
   - Remove struct handling
   - Use shared width calculation
   - Add panics for composite types

4. **Day 3-4:** Simplify SystemVerilog codegen
   - Remove duplicate flattening
   - Simplify port/signal emission
   - Remove struct expansion logic

5. **Day 5:** Simplify Metal codegen
   - Use shared utilities
   - Remove duplicate logic

### Week 3: Testing & Documentation
6. **Day 1-3:** Comprehensive testing
   - Write tests for each phase
   - Fix any issues found
   - Run full test suite

7. **Day 4-5:** Documentation & Cleanup
   - Update architecture docs
   - Add code comments
   - Clean up dead code

---

## Success Criteria

### Functional Criteria
- ✅ All existing tests pass
- ✅ Graphics pipeline end-to-end test passes (currently failing)
- ✅ No struct types in MIR ports/signals after transformation
- ✅ SystemVerilog codegen generates correct output
- ✅ Metal simulation produces correct results

### Code Quality Criteria
- ✅ No duplicate flattening logic
- ✅ No duplicate type width calculation
- ✅ Clear separation of responsibilities
- ✅ Single source of truth for each transformation
- ✅ Panics added for invalid states

### Maintainability Criteria
- ✅ New backends can rely on fully-flattened MIR
- ✅ Type system changes only require updating one place
- ✅ Clear documentation of IR invariants
- ✅ Validation catches bugs early

---

## Risks & Mitigation

### Risk 1: Breaking Existing Functionality
**Mitigation:**
- Incremental refactoring with tests after each step
- Keep existing code alongside new code initially
- Feature flag for new behavior

### Risk 2: Performance Regression
**Mitigation:**
- Benchmark before/after
- Profile hot paths
- Shared utilities should be zero-cost abstractions

### Risk 3: Incomplete Transformation
**Mitigation:**
- Comprehensive validation in HIR→MIR
- Panics in downstream passes if invariants violated
- Extensive test coverage

---

## Future Extensions

### Extension 1: Enum Flattening
- Similar to struct flattening
- Expand to base type + tag field

### Extension 2: Union Flattening
- Expand to largest field width
- Add tag field for variant tracking

### Extension 3: Array Flattening
- Expand to individual element signals
- Handle constant vs. dynamic indices

### Extension 4: Better LIR Utilization
- Complete MIR→LIR transformation
- Use for formal verification
- Use for physical design

---

## Conclusion

This refactoring will:
1. **Fix the immediate bug** (struct signals not working in simulation)
2. **Eliminate code duplication** (4+ implementations of same logic)
3. **Establish clear architecture** (single responsibility per IR level)
4. **Enable future backends** (trust MIR invariants)
5. **Improve maintainability** (changes in one place)

The key insight is: **HIR→MIR should produce fully hardware-ready IR**. All structural abstractions (structs, vectors, enums, unions) should be completely flattened. Downstream passes (MIR→SIR, MIR→SystemVerilog, MIR→Metal) should be pure lowering operations that don't need to understand high-level constructs.

This follows the principle: **Each IR level adds value; each transformation happens once.**
