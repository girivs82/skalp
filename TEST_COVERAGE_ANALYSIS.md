# Test Coverage Analysis - Phase 12

**Date:** October 3, 2025
**Analysis:** Current test structure and coverage gaps

---

## Executive Summary

**Total Test Code:** ~11,300 lines across 49 test files
**Test Distribution:** Scattered across crates with inconsistent coverage
**Critical Gap:** **No parser regression tests for the bug we just fixed**

---

## Current Test Structure

### Frontend Tests (skalp-frontend)
**Location:** `crates/skalp-frontend/tests/`
**Files:** 4 integration test files
**Unit Tests:** 47 in-module tests

#### Covered Areas:
- ✅ Lexer: Basic tokenization (17 tests)
  - Keywords, operators, literals
  - Identifiers, numbers, strings
  - Intent/requirement/verification keywords
- ✅ Generics: Type system (3 tests)
  - Type inference, substitution
  - Polymorphic instantiation
- ✅ HIR Builder: Basic entities (2 tests)
- ✅ Macros: Code generation (2 tests)
- ✅ Integration: Counter example parsing

#### Critical Gaps:
- ❌ **No parser regression tests**
- ❌ **No tests for complex nested expressions**
- ❌ **No tests for bit indexing/slicing**
- ❌ **No tests for binary expression parsing**
- ❌ **No tests for ParenExpr with multiple operators**
- ❌ **No tests for if-expression else-if chains**
- ❌ **No tests for match expressions with complex patterns**
- ❌ **No tests for unary operators with indexing**

### MIR Tests (skalp-mir)
**Location:** `crates/skalp-mir/tests/`
**Files:** 5 integration test files

#### Covered Areas:
- ✅ HIR to MIR transformation (hir_to_mir_test.rs)
- ✅ Code generation (codegen_test.rs)
- ✅ Optimization passes (optimize_test.rs)
- ✅ Reset event handling (reset_event_test.rs)
- ✅ End-to-end compilation (e2e_test.rs - 4 tests)

#### Gaps:
- ⚠️ Tests manually construct HIR (not from parsing)
- ⚠️ No golden file comparison
- ⚠️ Limited optimization validation

### Simulation Tests (skalp-sim)
**Location:** `crates/skalp-sim/tests/`
**Files:** 1 comprehensive test file

#### Covered Areas:
- ✅ GPU simulation infrastructure
- ✅ Metal shader generation
- ✅ Basic simulation execution

#### Gaps:
- ⚠️ No systematic example testing
- ⚠️ No performance benchmarks
- ⚠️ No simulation result validation

### LIR Tests (skalp-lir)
**Location:** `crates/skalp-lir/tests/`
**Files:** 1 test file

#### Covered Areas:
- ✅ Basic LIR generation

#### Gaps:
- ⚠️ Minimal coverage
- ⚠️ No optimization testing

### Verification Tests (skalp-verify)
**Location:** `crates/skalp-verify/tests/`
**Files:** 1 test file

#### Covered Areas:
- ✅ Verification infrastructure

#### Gaps:
- ⚠️ Limited property testing

### Root-Level Tests
**Location:** `tests/`
**Files:** 37 test files (~8,500 lines)

#### Covered Areas:
- ✅ CLI integration
- ✅ Complete hardware flow
- ✅ Formal verification
- ✅ GPU simulation
- ✅ Phase-specific features
- ✅ Technology mapping
- ✅ Place & route
- ✅ Timing analysis

#### Gaps:
- ⚠️ Many debug/milestone tests (not systematic regression)
- ⚠️ No golden file testing
- ⚠️ Tests are one-off validations, not comprehensive

---

## Critical Issues Identified

### 1. **No Parser Regression Tests** (CRITICAL)
The bug we just fixed (deeply nested expressions with bit indexing) had **ZERO tests** covering:
- Nested binary expressions inside parentheses
- Index expressions in complex contexts
- Unary operators with bit indexing
- Multiple operators in parentheses

**Impact:** Major bugs can ship undetected

**Solution Required:**
- Comprehensive parser test suite
- Test every expression type in isolation
- Test combinations and nesting
- Test edge cases (the fix we just made)

### 2. **No Golden File Testing** (CRITICAL)
Tests validate basic functionality but don't catch:
- Changes in generated code
- Regression in optimization
- Formatting changes

**Impact:** Subtle regressions go unnoticed

**Solution Required:**
- Golden file framework
- Store expected outputs
- Auto-diff on test run
- Easy update mechanism

### 3. **Tests Don't Use Real Parsing** (HIGH)
Many tests manually construct HIR/MIR instead of parsing SKALP code.

**Impact:** Parser bugs not caught by downstream tests

**Solution Required:**
- Tests should parse real SKALP code
- Only lowest-level unit tests should construct manually

### 4. **No CI/CD** (HIGH)
All testing is manual.

**Impact:**
- Tests not run regularly
- PR validation missing
- Performance regression untracked

**Solution Required:**
- GitHub Actions workflow
- Automated test runs
- Performance tracking

### 5. **Scattered Test Organization** (MEDIUM)
Tests are spread across:
- In-module unit tests (lexer, etc.)
- Integration tests in `tests/` subdirs
- Root-level integration tests
- Debug/milestone tests

**Impact:** Hard to find tests, unclear coverage

**Solution Required:**
- Consistent organization
- Clear categorization
- Documentation

---

## Test Coverage Gaps by Component

### Parser (CRITICAL GAPS)
| Feature | Coverage | Gap |
|---------|----------|-----|
| Binary expressions | ❌ None | Need nested, complex operators |
| Index expressions | ❌ None | Need base/index separation tests |
| Range expressions | ❌ None | Need slice notation tests |
| Unary expressions | ❌ None | Need with/without indexing |
| ParenExpr | ❌ None | Need multiple operators inside |
| If-expressions | ❌ None | Need else-if chains |
| Match expressions | ⚠️ Minimal | Need complex arms |
| Patterns | ⚠️ Minimal | Need all pattern types |

### HIR Builder (HIGH GAPS)
| Feature | Coverage | Gap |
|---------|----------|-----|
| Expression building | ⚠️ Minimal | Only 2 basic tests |
| Pattern matching | ❌ None | No literal/wildcard/path tests |
| Signal assignments | ⚠️ Minimal | Need complex RHS |
| Event blocks | ⚠️ Minimal | Need nested statements |
| Type annotations | ❌ None | Need all type forms |

### Code Generation (MEDIUM GAPS)
| Feature | Coverage | Gap |
|---------|----------|-----|
| Basic Verilog | ✅ Good | Working |
| Complex expressions | ❌ None | ALU just started working! |
| Always blocks | ⚠️ Minimal | Need both types |
| Wire declarations | ⚠️ Minimal | Need all scenarios |
| Module structure | ⚠️ Minimal | Need ports, params |

### Examples (HIGH GAPS)
| Example | Test Coverage | Gap |
|---------|--------------|-----|
| ALU | ❌ None | Just fixed, needs tests |
| Counter | ⚠️ Parse only | Need full compilation |
| FIFO | ❌ None | Need full test |
| UART | ❌ None | Need full test |
| AXI4-Lite | ❌ None | Need full test |
| Adder | ⚠️ Minimal | One basic test |
| Multiplier | ❌ None | Need full test |
| Shift Register | ❌ None | Need full test |

---

## Recommended Test Additions (Priority Order)

### Phase 12.1: Parser Regression Suite (Week 1)
**Priority: CRITICAL**
**Effort: 3-4 days**

Add comprehensive parser tests in `crates/skalp-frontend/tests/parser_regression.rs`:

1. **Binary Expressions** (50+ tests)
   - All operators: +, -, *, /, %, &, |, ^, <<, >>, ==, !=, <, <=, >, >=, &&, ||
   - Left-associative chaining: `a + b + c`
   - Right-associative chaining: `a = b = c`
   - Mixed precedence: `a + b * c`
   - Parentheses: `(a + b) * c`
   - Deep nesting: `((a + b) * (c - d)) / e`
   - With indexing: `a[0] + b[1]`
   - With ranging: `a[3:0] + b[3:0]`

2. **Index Expressions** (30+ tests)
   - Simple index: `a[0]`
   - Variable index: `a[i]`
   - Expression index: `a[i + 1]`
   - Nested: `a[b[0]]`
   - In binary: `a[0] + b[1]`
   - In unary: `~a[31]`
   - Multiple in expression: `a[31] & b[31] & c[31]`

3. **Range Expressions** (20+ tests)
   - Simple range: `a[7:0]`
   - Reverse range: `a[0:7]`
   - Variable bounds: `a[hi:lo]`
   - In operations: `a << b[4:0]`
   - Nested: `data[addr[3:0]]`

4. **Parenthesized Expressions** (25+ tests)
   - Single operator: `(a + b)`
   - Multiple same: `(a & b & c)`
   - Mixed operators: `(a & b | c)`
   - With indexing: `(a[31] & b[31])`
   - **The bug we fixed:** `(~a[31] & ~b[31] & result[31]) | (a[31] & b[31] & ~result[31])`
   - Deeply nested: `((a & b) | (c & d))`

5. **If Expressions** (20+ tests)
   - Simple: `if cond { a } else { b }`
   - Else-if: `if a { x } else if b { y } else { z }`
   - Nested: if inside if
   - With complex conditions: `if (a & b) | c { ... }`
   - With bit indexing: `if a[0] { ... }`

6. **Match Expressions** (30+ tests)
   - Simple patterns: literals, wildcards
   - Multiple arms
   - Guards (if supported)
   - Complex RHS expressions
   - With bit slicing in arms
   - Nested matches

7. **Unary Expressions** (15+ tests)
   - Not: `~a`, `!a`
   - Negation: `-a`
   - With indexing: `~a[31]`
   - Nested: `~~a`
   - In binary: `~a & b`

### Phase 12.2: HIR Builder Tests (Week 1)
**Priority: CRITICAL**
**Effort: 2-3 days**

Add tests in `crates/skalp-frontend/tests/hir_builder_regression.rs`:

1. **Expression Building** (40+ tests)
   - All expression types from parsed AST
   - Verify HIR structure matches intent
   - Test error cases

2. **Pattern Matching** (20+ tests)
   - Literal patterns
   - Wildcard patterns
   - Path patterns (Enum::Variant)
   - Complex patterns

### Phase 12.3: Golden File Framework (Week 1)
**Priority: CRITICAL**
**Effort: 2-3 days**

Create `crates/skalp-testing/src/golden.rs`:

```rust
/// Golden file testing framework
pub struct GoldenTest {
    name: String,
    input: String,
    expected_dir: PathBuf,
}

impl GoldenTest {
    pub fn new(name: &str, input: &str) -> Self { ... }

    pub fn assert_output(&self, actual: &str, extension: &str) {
        // Compare with golden/<name>.<extension>
        // On mismatch, show diff and provide update command
    }

    pub fn update_golden(&self, actual: &str, extension: &str) {
        // Write new golden file
    }
}
```

### Phase 12.4: End-to-End Example Tests (Week 2)
**Priority: HIGH**
**Effort: 3-4 days**

Add tests in `tests/example_regression.rs`:

For each example (ALU, Counter, FIFO, UART, etc.):
1. Parse successfully
2. Build HIR
3. Compile to MIR
4. Generate SystemVerilog
5. Compare with golden file
6. Simulate (if applicable)
7. Verify outputs

### Phase 12.5: CI/CD Pipeline (Week 3)
**Priority: HIGH**
**Effort: 2-3 days**

Create `.github/workflows/test.yml`:
- Run all tests on push/PR
- Multiple platforms (macOS, Linux)
- Performance benchmarks
- Coverage reporting

---

## Test Metrics Targets

| Metric | Current | Target | Gap |
|--------|---------|--------|-----|
| **Parser Tests** | ~0 | 200+ | Need all |
| **HIR Builder Tests** | ~2 | 60+ | Need 58 |
| **End-to-End Tests** | ~10 | 50+ | Need 40 |
| **Code Coverage** | Unknown | 80%+ | Measure first |
| **Example Coverage** | 1/8 | 8/8 | Need 7 |
| **Golden Files** | 0 | 100+ | Need all |
| **CI Tests** | Manual | Automated | Need CI |

---

## Immediate Action Items

### This Week (Week 1)
1. ✅ Complete this analysis
2. ⬜ Create parser regression test file
3. ⬜ Add 50+ parser tests covering the bug we fixed
4. ⬜ Set up golden file framework
5. ⬜ Add HIR builder regression tests
6. ⬜ Document test patterns

### Next Week (Week 2)
1. ⬜ Add end-to-end tests for all examples
2. ⬜ Generate golden files for examples
3. ⬜ Add SystemVerilog generation tests
4. ⬜ Performance baseline benchmarks

### Week 3
1. ⬜ Set up GitHub Actions
2. ⬜ Integrate all tests into CI
3. ⬜ Coverage reporting
4. ⬜ LSP testing

### Week 4
1. ⬜ Bug triage
2. ⬜ Improve error messages
3. ⬜ Documentation
4. ⬜ Contributor guidelines

---

## Risk Assessment

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|------------|
| More parser bugs exist | High | Critical | Add comprehensive tests NOW |
| Performance regressions | Medium | High | Add benchmarks to CI |
| Breaking changes undetected | High | High | Golden file testing |
| Test maintenance burden | Medium | Medium | Good organization, docs |
| CI costs | Low | Low | GitHub Actions free tier |

---

## Conclusion

**Current State:**
- We have ~11,300 lines of test code
- But tests are scattered and gaps are critical
- **The parser bug we just fixed had ZERO tests covering it**

**Required Action:**
- Add 200+ parser regression tests (CRITICAL)
- Set up golden file testing (CRITICAL)
- Add end-to-end example tests (HIGH)
- Set up CI/CD pipeline (HIGH)

**Timeline:** 4 weeks for Phase 12 completion

**Success Criteria:**
- 80%+ code coverage
- All examples tested
- CI pipeline operational
- No known bugs without regression tests

---

*Generated: October 3, 2025*
*Phase: 12 - Regression Testing & Stability*
