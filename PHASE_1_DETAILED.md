# Phase 1 Detailed Implementation Plan: Complete Frontend

**Duration:** 4-6 weeks
**Priority:** Critical
**Team:** 2-3 Senior Engineers

## Overview

Phase 1 transforms the SKALP frontend from stub implementations to a production-ready parser, type checker, and HIR builder capable of handling the complete SKALP language specification.

## Current State Analysis

### Existing Placeholders in Frontend:
- **Parser**: 23 TODO/stub implementations
- **Type Checker**: 15 simplified implementations
- **HIR Builder**: 12 placeholder functions
- **Lexer**: 3 unknown token handlers
- **Generics**: 5 unimplemented type operations

## 1.1 Complete Parser Implementation (Week 1-2)

### Week 1: Core Grammar Implementation

#### Day 1-2: Lexer Enhancement
**File:** `crates/skalp-frontend/src/lexer.rs`

**Current Issues:**
```rust
// Placeholder for unknown tokens - Logos 0.13+ handles errors differently
```

**Implementation Tasks:**
- [ ] **Complete token definitions** for all SKALP keywords
- [ ] **String literal parsing** with escape sequences
- [ ] **Number literal parsing** (binary, hex, decimal, with underscores)
- [ ] **Comment handling** (single-line, multi-line, doc comments)
- [ ] **Error recovery** for malformed tokens
- [ ] **Position tracking** for precise error reporting

**Code Structure:**
```rust
#[derive(Logos, Debug, PartialEq, Clone)]
pub enum Token {
    // Keywords
    #[token("entity")] Entity,
    #[token("impl")] Impl,
    #[token("trait")] Trait,
    #[token("protocol")] Protocol,
    #[token("intent")] Intent,
    #[token("requirement")] Requirement,

    // Hardware-specific tokens
    #[token("clock")] Clock,
    #[token("reset")] Reset,
    #[token("bit")] Bit,
    #[token("signal")] Signal,
    #[token("wire")] Wire,

    // Advanced literals
    #[regex(r"0b[01_]+", parse_binary)]
    #[regex(r"0x[0-9a-fA-F_]+", parse_hex)]
    #[regex(r"[0-9][0-9_]*", parse_decimal)]
    Number(u64),

    // String with escape sequences
    #[regex(r#""([^"\\]|\\.)*""#, parse_string)]
    String(String),

    // Enhanced error handling
    #[error]
    Error,
}

impl Token {
    pub fn parse_binary(s: &str) -> Option<u64> { /* implementation */ }
    pub fn parse_hex(s: &str) -> Option<u64> { /* implementation */ }
    pub fn parse_string(s: &str) -> Option<String> { /* implementation */ }
}
```

#### Day 3-5: Core Language Constructs
**File:** `crates/skalp-frontend/src/parse.rs`

**Current Issues:**
```rust
errors: Vec::new(), // TODO: Return collected errors
// TODO: Collect errors for later reporting instead of just printing
```

**Implementation Tasks:**
- [ ] **Entity declarations** with generic parameters
- [ ] **Implementation blocks** with trait bounds
- [ ] **Signal declarations** with timing specifications
- [ ] **Process blocks** with sensitivity lists
- [ ] **Expression parsing** with proper precedence
- [ ] **Pattern matching** syntax

**Parser Structure:**
```rust
pub struct Parser<'a> {
    tokens: Peekable<TokenIterator<'a>>,
    errors: Vec<ParseError>,
    recovery_points: Vec<RecoveryPoint>,
}

impl<'a> Parser<'a> {
    pub fn parse_entity(&mut self) -> Result<Entity, ParseError> {
        // entity Counter<WIDTH: nat = 8> {
        //     in clk: clock,
        //     in reset: reset,
        //     out count: bit[WIDTH]
        // }
        let name = self.expect_identifier()?;
        let generics = self.parse_generic_params()?;
        let ports = self.parse_port_list()?;

        Ok(Entity { name, generics, ports })
    }

    pub fn parse_implementation(&mut self) -> Result<Implementation, ParseError> {
        // impl Counter {
        //     signal count_reg: bit[8] = 0;
        //
        //     on(clk.rise) {
        //         count_reg <= count_reg + 1;
        //     }
        // }
    }

    pub fn parse_expression(&mut self) -> Result<Expression, ParseError> {
        // Operator precedence parsing for hardware expressions
        self.parse_conditional_expression()
    }
}
```

### Week 2: Advanced Language Features

#### Day 6-8: Generic Parameters and Constraints
**File:** `crates/skalp-frontend/src/parse.rs`

**Current Issues:**
```rust
/// Parse generic parameters (stub)
```

**Implementation Tasks:**
- [ ] **Generic parameter parsing** with bounds and defaults
- [ ] **Trait bound parsing** (T: Trait + Send + 'static)
- [ ] **Associated type parsing** (type Output: BitWidth)
- [ ] **Const generic parsing** (const N: usize)
- [ ] **Lifetime parameter parsing** ('clk, 'reset)

#### Day 9-10: Intent and Requirement Parsing
**Current Issues:** Complete stub implementations

**Implementation Tasks:**
- [ ] **Intent block parsing** - Hardware design intentions
- [ ] **Requirement parsing** - Safety/timing requirements
- [ ] **Protocol definitions** - Interface protocols
- [ ] **Constraint expressions** - Timing/power constraints

```rust
pub fn parse_intent(&mut self) -> Result<Intent, ParseError> {
    // intent {
    //     throughput: 100M_samples_per_sec,
    //     architecture: systolic_array,
    //     optimization: balanced(speed: 0.7, area: 0.3)
    // }
}

pub fn parse_requirement(&mut self) -> Result<Requirement, ParseError> {
    // requirement safety {
    //     sil: 2,
    //     fault_tolerance: double_redundancy,
    //     verification: formal_proof
    // }
}
```

## 1.2 Complete Type System (Week 2-3)

### Week 2-3: Advanced Type Checking
**File:** `crates/skalp-frontend/src/typeck.rs`

**Current Issues:**
```rust
// TODO: Create proper entity type
// TODO: Add pattern variables to scope
// TODO: Implement tuple pattern checking
Type::Unknown // Placeholder
```

**Implementation Tasks:**

#### Day 8-12: Core Type System
- [ ] **Hardware type semantics** - Bit widths, signed/unsigned
- [ ] **Clock domain types** - Temporal type safety
- [ ] **Signal type inference** - Automatic width inference
- [ ] **Constraint propagation** - Type constraint solving
- [ ] **Pattern type checking** - Match exhaustiveness

```rust
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    // Hardware types
    Bit(Option<usize>),           // bit[8], bit (inferred width)
    Signed(Option<usize>),        // int[16]
    Clock(ClockDomain),           // clock<'domain>
    Reset(ResetType),             // reset(active_high)

    // Temporal types
    Stream(Box<Type>, ClockDomain), // stream<'clk>[32]
    Signal(Box<Type>),            // signal data: bit[8]

    // Aggregate types
    Array(Box<Type>, ArraySize),   // bit[8][16]
    Struct(StructId),             // Custom struct
    Enum(EnumId),                 // Custom enum

    // Generic types
    Generic(GenericId),           // T
    Associated(TraitId, String),  // T::Output

    // Type constructors
    Function(Vec<Type>, Box<Type>), // (bit[8], bit[8]) -> bit[9]

    // Special types
    Inferred(InferenceVar),       // Unresolved type
    Error,                        // Type error marker
}

pub struct TypeChecker {
    scopes: Vec<Scope>,
    inference_vars: HashMap<InferenceVar, Type>,
    constraints: Vec<TypeConstraint>,
    clock_domains: HashMap<ClockDomain, ClockInfo>,
}

impl TypeChecker {
    pub fn check_entity(&mut self, entity: &Entity) -> Result<TypedEntity, TypeError> {
        // Check port types and generic parameters
        for port in &entity.ports {
            self.check_port_type(&port.port_type)?;
        }

        // Verify clock domain consistency
        self.check_clock_domains(&entity.ports)?;

        Ok(TypedEntity { /* ... */ })
    }

    pub fn check_implementation(&mut self, impl_block: &Implementation) -> Result<TypedImplementation, TypeError> {
        // Type check all signals and processes
        for signal in &impl_block.signals {
            self.check_signal_type(&signal)?;
        }

        for process in &impl_block.processes {
            self.check_process(&process)?;
        }

        Ok(TypedImplementation { /* ... */ })
    }

    fn check_clock_domain_safety(&mut self, expr: &Expression) -> Result<ClockDomain, TypeError> {
        // Ensure all signals in expression belong to same clock domain
        // This is critical for CDC (Clock Domain Crossing) safety
    }
}
```

#### Day 13-14: Clock Domain Type Safety
- [ ] **Clock domain inference** - Automatic domain assignment
- [ ] **CDC violation detection** - Cross-domain signal usage
- [ ] **Synchronizer insertion** - Automatic CDC safety
- [ ] **Temporal type checking** - Time-aware type system

```rust
#[derive(Debug, Clone, PartialEq, Hash)]
pub struct ClockDomain {
    name: String,
    frequency: Option<f64>,
    phase: Option<f64>,
    reset: Option<ResetSignal>,
}

pub struct CdcChecker {
    domains: HashMap<ClockDomain, DomainInfo>,
    violations: Vec<CdcViolation>,
}

impl CdcChecker {
    pub fn check_expression(&mut self, expr: &Expression) -> Result<ClockDomain, CdcError> {
        match expr {
            Expression::Signal(signal) => {
                self.get_signal_domain(signal)
            }
            Expression::Binary(op, lhs, rhs) => {
                let lhs_domain = self.check_expression(lhs)?;
                let rhs_domain = self.check_expression(rhs)?;

                if lhs_domain != rhs_domain {
                    self.violations.push(CdcViolation {
                        location: expr.span(),
                        domains: (lhs_domain.clone(), rhs_domain.clone()),
                        severity: CdcSeverity::Error,
                    });
                    return Err(CdcError::DomainMismatch);
                }

                Ok(lhs_domain)
            }
            // ... other expressions
        }
    }
}
```

## 1.3 Complete HIR Builder (Week 3-4)

### Week 3: HIR Construction
**File:** `crates/skalp-frontend/src/hir_builder.rs`

**Current Issues:**
```rust
clock_domains: Vec::new(),  // TODO: Parse clock domain parameters
instances: vec![], // TODO: Parse instance declarations
// TODO: Handle indexed and ranged L-values
// TODO: Convert from Type to HirType
HirType::Bit(8) // Placeholder
```

**Implementation Tasks:**

#### Day 15-17: Complete HIR Construction
- [ ] **Full expression HIR** - All expression types
- [ ] **Pattern HIR building** - Match patterns, destructuring
- [ ] **Process HIR** - Sensitivity lists, timing
- [ ] **Module instantiation HIR** - Hierarchical designs
- [ ] **Clock domain HIR** - Temporal specifications

```rust
pub struct HirBuilder {
    ast: SourceFile,
    hir: Hir,
    symbol_table: SymbolTable,
    type_checker: TypeChecker,
}

impl HirBuilder {
    pub fn build_entity(&mut self, entity: &ast::Entity) -> Result<hir::Entity, HirError> {
        let name = entity.name.clone();
        let generics = self.build_generics(&entity.generics)?;
        let ports = self.build_ports(&entity.ports)?;

        // Extract clock domains from port specifications
        let clock_domains = self.extract_clock_domains(&ports)?;

        Ok(hir::Entity {
            name,
            generics,
            ports,
            clock_domains,
            span: entity.span,
        })
    }

    pub fn build_implementation(&mut self, impl_block: &ast::Implementation) -> Result<hir::Implementation, HirError> {
        let signals = self.build_signals(&impl_block.signals)?;
        let processes = self.build_processes(&impl_block.processes)?;
        let instances = self.build_instances(&impl_block.instances)?;

        // Verify implementation matches entity interface
        self.check_implementation_consistency(&signals, &processes)?;

        Ok(hir::Implementation {
            signals,
            processes,
            instances,
            span: impl_block.span,
        })
    }

    fn build_process(&mut self, process: &ast::Process) -> Result<hir::Process, HirError> {
        let sensitivity = self.build_sensitivity_list(&process.sensitivity)?;
        let body = self.build_block(&process.body)?;

        // Verify process is synthesizable
        self.check_synthesizability(&sensitivity, &body)?;

        Ok(hir::Process {
            kind: hir::ProcessKind::Sequential, // or Combinational
            sensitivity,
            body,
            span: process.span,
        })
    }
}
```

### Week 4: Advanced HIR Features

#### Day 18-21: Trait System HIR
- [ ] **Trait definition HIR** - Complete trait system
- [ ] **Trait implementation HIR** - Associated types/functions
- [ ] **Trait bound resolution** - Generic constraint solving
- [ ] **Associated type projection** - Complex type relationships

```rust
pub fn build_trait_definition(&mut self, trait_def: &ast::TraitDefinition) -> Result<hir::TraitDefinition, HirError> {
    let name = trait_def.name.clone();
    let generics = self.build_generics(&trait_def.generics)?;
    let associated_types = self.build_associated_types(&trait_def.associated_types)?;
    let methods = self.build_trait_methods(&trait_def.methods)?;

    Ok(hir::TraitDefinition {
        name,
        generics,
        associated_types,
        methods,
        span: trait_def.span,
    })
}

pub fn build_trait_implementation(&mut self, trait_impl: &ast::TraitImplementation) -> Result<hir::TraitImplementation, HirError> {
    let trait_ref = self.resolve_trait_reference(&trait_impl.trait_name)?;
    let target_type = self.build_type(&trait_impl.target_type)?;
    let associated_types = self.build_associated_type_implementations(&trait_impl.associated_types)?;
    let methods = self.build_method_implementations(&trait_impl.methods)?;

    // Verify implementation completeness
    self.check_trait_implementation_completeness(&trait_ref, &associated_types, &methods)?;

    Ok(hir::TraitImplementation {
        trait_ref,
        target_type,
        associated_types,
        methods,
        span: trait_impl.span,
    })
}
```

#### Day 22-28: Intent and Requirement HIR
- [ ] **Intent specification HIR** - Hardware design intentions
- [ ] **Requirement HIR** - Safety/performance requirements
- [ ] **Protocol HIR** - Interface protocol specifications
- [ ] **Constraint HIR** - Design constraints

```rust
pub fn build_intent(&mut self, intent: &ast::Intent) -> Result<hir::Intent, HirError> {
    let category = intent.category.clone();
    let specifications = self.build_intent_specifications(&intent.specifications)?;

    Ok(hir::Intent {
        category: match category.as_str() {
            "performance" => hir::IntentCategory::Performance,
            "area" => hir::IntentCategory::Area,
            "power" => hir::IntentCategory::Power,
            "safety" => hir::IntentCategory::Safety,
            _ => return Err(HirError::UnknownIntentCategory(category)),
        },
        specifications,
        span: intent.span,
    })
}

pub fn build_requirement(&mut self, req: &ast::Requirement) -> Result<hir::Requirement, HirError> {
    let requirement_type = self.determine_requirement_type(&req.requirement_type)?;
    let constraints = self.build_requirement_constraints(&req.constraints)?;
    let verification_method = self.build_verification_method(&req.verification)?;

    Ok(hir::Requirement {
        requirement_type,
        constraints,
        verification_method,
        span: req.span,
    })
}
```

## Testing Strategy

### Unit Testing (Continuous)
- [ ] **Parser unit tests** - Each grammar construct
- [ ] **Type checker tests** - Type inference, error cases
- [ ] **HIR builder tests** - AST to HIR transformation
- [ ] **Error handling tests** - Graceful error recovery

### Integration Testing (End of each week)
- [ ] **End-to-end parsing** - Complete SKALP programs
- [ ] **Type checking integration** - Complex type scenarios
- [ ] **HIR consistency** - Verified HIR generation
- [ ] **Error reporting** - User-friendly error messages

### Regression Testing (Continuous)
- [ ] **Existing examples** - All current SKALP examples
- [ ] **Performance benchmarks** - Compilation speed
- [ ] **Memory usage** - Parser memory efficiency

## Deliverables

### Week 1 Deliverables
- [ ] **Complete lexer** with all SKALP tokens
- [ ] **Core parser** for entities and implementations
- [ ] **Basic expression parsing** with precedence

### Week 2 Deliverables
- [ ] **Generic parameter parsing** complete
- [ ] **Intent/requirement parsing** functional
- [ ] **Advanced type checking** framework

### Week 3 Deliverables
- [ ] **Clock domain type safety** implemented
- [ ] **HIR builder** for core constructs
- [ ] **Pattern matching** support

### Week 4 Deliverables
- [ ] **Complete trait system** HIR
- [ ] **Intent/requirement** HIR
- [ ] **Full frontend integration** testing

## Success Criteria

### Functional Requirements
- [ ] **Parse all SKALP language constructs** without placeholders
- [ ] **Type check complex generic code** with proper inference
- [ ] **Generate complete HIR** for all language features
- [ ] **Provide excellent error messages** with suggestions
- [ ] **Support IDE integration** through LSP foundation

### Performance Requirements
- [ ] **Parse 10,000 line files** in <1 second
- [ ] **Type check complex generics** in <5 seconds
- [ ] **Memory usage** <100MB for typical projects
- [ ] **Incremental compilation** ready foundation

### Quality Requirements
- [ ] **95% test coverage** on all new code
- [ ] **Zero compiler warnings** on all code
- [ ] **Comprehensive documentation** for all APIs
- [ ] **Examples for all features** in test suite

This detailed plan provides a concrete roadmap for transforming the SKALP frontend from placeholder implementations to production-ready code capable of handling the complete SKALP language specification with excellent error handling, performance, and extensibility.