# Getting Started with SKALP Implementation

## Quick Start Guide

This guide helps developers immediately start implementing Phase 1 of the SKALP compiler.

## Prerequisites

### Development Environment
```bash
# Ensure you have Rust 1.70+ installed
rustc --version

# Clone and build current state
git clone https://github.com/skalp-lang/skalp
cd skalp
cargo build

# Run existing tests to verify setup
cargo test
```

### Tools Needed
- **Rust 1.70+** with clippy and rustfmt
- **Git** for version control
- **IDE** with Rust support (VS Code + rust-analyzer recommended)

## Phase 1 Implementation Order

### Step 1: Enhanced Lexer (Start Here!)
**File:** `crates/skalp-frontend/src/lexer.rs`
**Time Estimate:** 1-2 days

**Current Issue:**
```rust
// Placeholder for unknown tokens - Logos 0.13+ handles errors differently
```

**What to Implement:**
1. Complete all SKALP keyword tokens
2. Enhanced number literal parsing (binary, hex, with underscores)
3. String literal parsing with escape sequences
4. Proper error handling and recovery

**Implementation Template:**
```rust
#[derive(Logos, Debug, PartialEq, Clone)]
pub enum Token {
    // Add missing keywords
    #[token("intent")] Intent,
    #[token("requirement")] Requirement,
    #[token("protocol")] Protocol,
    // ... add all keywords from language spec

    // Enhanced number parsing
    #[regex(r"0b[01_]+", parse_binary)]
    #[regex(r"0x[0-9a-fA-F_]+", parse_hex)]
    #[regex(r"[0-9][0-9_]*", parse_decimal)]
    Number(u64),

    // String literals with escapes
    #[regex(r#""([^"\\]|\\.)*""#, parse_string)]
    String(String),

    #[error]
    Error,
}
```

**Test as you go:**
```bash
cd crates/skalp-frontend
cargo test lexer
```

### Step 2: Core Parser Implementation
**File:** `crates/skalp-frontend/src/parse.rs`
**Time Estimate:** 2-3 days

**Current Issues:**
```rust
errors: Vec::new(), // TODO: Return collected errors
// TODO: Collect errors for later reporting instead of just printing
```

**What to Implement:**
1. Entity declaration parsing with generics
2. Implementation block parsing
3. Expression parsing with operator precedence
4. Error collection and reporting

**Key Functions to Replace:**
```rust
// Replace these stub implementations:
pub fn parse_entity(&mut self) -> Result<Entity, ParseError>
pub fn parse_implementation(&mut self) -> Result<Implementation, ParseError>
pub fn parse_expression(&mut self) -> Result<Expression, ParseError>
pub fn parse_generic_params(&mut self) -> Result<Vec<GenericParam>, ParseError>
```

### Step 3: Type System Implementation
**File:** `crates/skalp-frontend/src/typeck.rs`
**Time Estimate:** 3-4 days

**Current Issues:**
```rust
// TODO: Create proper entity type
// TODO: Add pattern variables to scope
Type::Unknown // Placeholder
```

**Implementation Focus:**
1. Hardware type semantics (bit widths, signed/unsigned)
2. Clock domain types for CDC safety
3. Type inference engine
4. Pattern type checking

## Development Workflow

### 1. Pick a Specific TODO
Choose one specific TODO comment to implement:
```bash
# Find TODOs in current working area
grep -r "TODO\|FIXME\|placeholder" crates/skalp-frontend/src/
```

### 2. Write Tests First
Create failing tests for the functionality:
```rust
#[test]
fn test_entity_parsing() {
    let source = r#"
        entity Counter<WIDTH: nat = 8> {
            in clk: clock,
            in reset: reset,
            out count: bit[WIDTH]
        }
    "#;

    let mut parser = Parser::new(source);
    let entity = parser.parse_entity().unwrap();

    assert_eq!(entity.name, "Counter");
    assert_eq!(entity.generics.len(), 1);
    assert_eq!(entity.ports.len(), 3);
}
```

### 3. Implement the Feature
Replace placeholder with real implementation:
```rust
// Before (placeholder):
pub fn parse_entity(&mut self) -> Result<Entity, ParseError> {
    Ok(Entity::default()) // Placeholder
}

// After (real implementation):
pub fn parse_entity(&mut self) -> Result<Entity, ParseError> {
    self.expect_keyword("entity")?;
    let name = self.expect_identifier()?;
    let generics = self.parse_generic_params()?;
    let ports = self.parse_port_list()?;

    Ok(Entity { name, generics, ports })
}
```

### 4. Run Tests
```bash
# Test your specific changes
cargo test --package skalp-frontend parse_entity

# Test the whole frontend
cargo test --package skalp-frontend

# Test everything (make sure you didn't break anything)
cargo test
```

### 5. Update Progress
Update `PROGRESS_TRACKER.md` with completed items:
```markdown
#### Week 1: Parser Enhancement
- [x] Complete lexer with all SKALP tokens (6/6 tasks) ✅
- [ ] Core language constructs parsing (2/5 tasks)
- [ ] Expression parsing with precedence (0/3 tasks)
```

## Common Implementation Patterns

### Error Handling Pattern
```rust
pub fn parse_something(&mut self) -> Result<Something, ParseError> {
    let start_pos = self.current_position();

    match self.current_token() {
        Token::Expected => {
            // Parse successfully
            Ok(Something { /* ... */ })
        }
        token => {
            let error = ParseError::UnexpectedToken {
                expected: "expected description",
                found: token,
                position: start_pos,
            };
            self.errors.push(error.clone());
            Err(error)
        }
    }
}
```

### Type Checking Pattern
```rust
pub fn check_expression(&mut self, expr: &Expression) -> Result<Type, TypeError> {
    match expr {
        Expression::Literal(lit) => self.check_literal(lit),
        Expression::Variable(var) => self.lookup_variable_type(var),
        Expression::Binary(op, lhs, rhs) => {
            let lhs_type = self.check_expression(lhs)?;
            let rhs_type = self.check_expression(rhs)?;
            self.check_binary_operation(op, &lhs_type, &rhs_type)
        }
        // ... handle all expression types
    }
}
```

### HIR Building Pattern
```rust
pub fn build_entity(&mut self, ast_entity: &ast::Entity) -> Result<hir::Entity, HirError> {
    // Convert AST nodes to HIR nodes with additional semantic information
    let name = ast_entity.name.clone();
    let generics = self.build_generics(&ast_entity.generics)?;
    let ports = self.build_ports(&ast_entity.ports)?;

    // Add semantic analysis
    let clock_domains = self.extract_clock_domains(&ports)?;

    Ok(hir::Entity {
        name,
        generics,
        ports,
        clock_domains,
        span: ast_entity.span,
    })
}
```

## Testing Strategy

### Unit Tests
Test each function individually:
```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_bit_type() {
        let mut parser = Parser::new("bit[8]");
        let bit_type = parser.parse_type().unwrap();
        assert_eq!(bit_type, Type::Bit(Some(8)));
    }
}
```

### Integration Tests
Test complete compilation pipeline:
```rust
#[test]
fn test_complete_entity_compilation() {
    let source = include_str!("../../../examples/counter.sk");
    let frontend = Frontend::new();
    let hir = frontend.compile_to_hir(source).unwrap();

    // Verify HIR structure
    assert_eq!(hir.entities.len(), 1);
    assert_eq!(hir.entities[0].name, "Counter");
}
```

## Quick Reference

### Key Files and Their Purpose
- `lexer.rs` - Tokenization of SKALP source code
- `parse.rs` - AST generation from tokens
- `typeck.rs` - Type checking and inference
- `hir_builder.rs` - AST to HIR transformation
- `hir.rs` - HIR data structures

### Language Features to Implement
1. **Entities** - Hardware module declarations
2. **Implementations** - Hardware behavior definitions
3. **Signals/Wires** - Hardware interconnects
4. **Processes** - Sequential/combinational logic
5. **Generics** - Parameterized hardware
6. **Traits** - Interface definitions
7. **Intents** - Design specifications
8. **Requirements** - Safety/performance constraints

### SKALP-Specific Concepts
- **Clock Domains** - Temporal type safety
- **Reset Signals** - Initialization behavior
- **Sensitivity Lists** - Process triggering
- **Hardware Types** - bit[N], signed[N], etc.
- **Timing Constraints** - Performance requirements

## Getting Help

### Documentation
- `docs/LANGUAGE_SPECIFICATION.md` - Complete language reference
- `docs/GRAMMAR.ebnf` - Formal grammar specification
- `IMPLEMENTATION_PHASES.md` - Overall implementation plan

### Code Examples
- `examples/*.sk` - Example SKALP designs
- `crates/*/tests/` - Unit and integration tests
- `tests/` - CLI integration tests

### Progress Tracking
- `PROGRESS_TRACKER.md` - Current implementation status
- `PHASE_1_DETAILED.md` - Detailed Phase 1 plan

Start with the lexer enhancement and work your way through the parser. Each small step builds towards a complete, production-ready SKALP compiler!