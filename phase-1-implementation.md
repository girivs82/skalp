# Phase 1: Foundation - Lexer, Parser & Type System

**Goal:** Build the foundation that can parse and type-check SKALP code

**Duration:** 4 weeks (Sept 28 - Oct 25, 2024)

**Success Test:** Parse and type-check a simple counter entity with no errors

---

## 🎯 TASKS

### Week 1: Logos Lexer Implementation
- [ ] Set up Logos in skalp-frontend crate
- [ ] Define Token enum with Logos derive
- [ ] Implement keyword tokens and identifiers
- [ ] Number literals (binary 0b1010, hex 0xFF, decimal)
- [ ] String literals and all operators
- [ ] Handle whitespace and comments
- [ ] Logos lexer test suite

### Week 2: Rowan Parser Foundation
- [ ] Set up Rowan with SyntaxKind enum
- [ ] Design syntax node structure
- [ ] Implement entity declaration parsing
- [ ] Parse port declarations (in/out/inout)
- [ ] Parse type annotations and generics
- [ ] Handle `on(clock.rise | reset.rise)` syntax
- [ ] Rowan error recovery and reporting

### Week 3: Type System Foundation
- [ ] Implement type representation
- [ ] Basic types: `bit[N]`, `logic[N]`, `int[N]`, `nat[N]`
- [ ] Clock types: `clock`, `reset`
- [ ] Type inference for literals
- [ ] Type checking for assignments
- [ ] Width inference and checking
- [ ] Array types

### Week 4: HIR Generation & Testing
- [ ] AST to HIR transformation
- [ ] Symbol table construction
- [ ] Name resolution
- [ ] Basic semantic checks
- [ ] Comprehensive test suite
- [ ] Error message quality
- [ ] Parse example designs

---

## 🧪 TEST CASES

### Lexer Tests:
```rust
// Should tokenize correctly:
entity Counter {
    in clk: clock
    in reset: bit
    out count: nat[8]
}
```

### Parser Tests:
```rust
// Should parse into correct AST:
impl Counter {
    signal counter: nat[8] = 0

    on(clk.rise) {
        if (reset) {
            counter <= 0
        } else {
            counter <= counter + 1
        }
    }

    count = counter
}
```

### Type Checking Tests:
```rust
// Should catch type errors:
signal a: bit[8]
signal b: bit[16]
a = b  // ERROR: Width mismatch

// Should infer correctly:
signal x = 42  // Infers nat with minimum width
signal y = 0b1010  // Infers bit[4]
```

---

## ✅ COMPLETION CRITERIA

**This phase is done when:**
- [ ] Lexer tokenizes all SKALP syntax correctly
- [ ] Parser generates correct AST for entity/impl blocks
- [ ] Type system catches width mismatches
- [ ] `on(clock.rise | reset.rise)` syntax works
- [ ] Error messages point to correct source location
- [ ] Can parse and type-check counter example
- [ ] Test coverage > 80%

**Success Test:**
```bash
cargo test --package skalp-frontend
# All tests pass

cargo run -- parse examples/counter.sk
# Outputs: "✓ Parsed successfully with no type errors"
```

---

## 📈 PROGRESS

**Daily Log:**
```
[Date] - [What got done] - [Any blockers]
Sept 28 - Set up pest grammar structure - None
```

**Current Focus:** Setting up parser framework

**Blockers:**
- [ ] None yet

---

## 🔧 TECHNICAL DECISIONS

### Parser Framework:
**Selected Approach: Logos + Rowan ✅**

**Logos (Lexer):**
- Declarative token definitions with regex
- Excellent performance and error handling
- Source location tracking built-in

**Rowan (Parser/Tree):**
- Lossless syntax trees (preserves comments, whitespace)
- Excellent error recovery
- Perfect foundation for LSP later
- Incremental re-parsing support

**Rationale:**
- Proven track record on previous HDL projects
- Better than pest for complex languages
- Rowan's error recovery handles malformed code gracefully
- Lossless trees essential for language server features
- Logos generates fast, reliable lexers

### Error Handling:
- Use `codespan-reporting` for nice error messages
- Track source locations in every AST node
- Implement error recovery to report multiple errors

### Type Representation:
```rust
enum Type {
    Bit(Width),
    Logic(Width),
    Int(Width),
    Nat(Width),
    Clock(Option<ClockDomain>),
    Array(Box<Type>, Size),
    // ...
}
```

---

## 📚 REFERENCES

- [Logos Lexer](https://github.com/maciejhirsz/logos)
- [Rowan Syntax Trees](https://github.com/rust-analyzer/rowan)
- [codespan-reporting](https://github.com/brendanzab/codespan)
- [rust-analyzer Parser](https://github.com/rust-lang/rust-analyzer/tree/master/crates/parser) (Rowan example)
- Our own [Language Specification](docs/LANGUAGE_SPECIFICATION.md)

---

**When done, run `/complete-phase` to move to Phase 2!**