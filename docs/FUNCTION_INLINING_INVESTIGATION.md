# Function Inlining Investigation

## Current State

### Problem
Function calls in SKALP combinational and sequential logic are **not implemented**. When encountered, they return `None` and the assignment is silently dropped, resulting in **broken hardware** with missing logic.

### Evidence
**Test case** (`/tmp/test_function_inline.sk`):
```skalp
fn add(a: bit[8], b: bit[8]) -> bit[8] {
    return a + b
}

impl FunctionTest {
    signal r1: bit[8]
    r1 = add(x, y)  // Function call
}
```

**Generated SystemVerilog** (BROKEN):
```systemverilog
module FunctionTest (...);
    wire [7:0] r1;  // ❌ Declared but has NO ASSIGNMENT!
endmodule
```

**Root Cause** (`crates/skalp-mir/src/hir_to_mir.rs:1887-1890`):
```rust
hir::HirExpression::Call(_call) => {
    // TODO: Handle function calls
    None  // ❌ Returns None - not implemented!
}
```

When `convert_expression()` returns `None`:
1. The assignment becomes `None` at line 1309
2. Empty vector returned at line 1292
3. Assignment silently dropped
4. **No error, no warning** - just broken hardware

### What Works Today

**✅ Const Functions** (type parameters only):
```skalp
entity Decoder<const SIZE: nat> {
    in addr: bit[clog2(SIZE)]  // ✅ Works - const context
}
```

**✅ Entity Instantiation** (module reuse):
```skalp
entity Adder {
    in a: bit[8]
    in b: bit[8]
    out sum: bit[8]
}

impl MyModule {
    let adder = Adder { a: x, b: y, sum: result }  // ✅ Works
}
```

### What Doesn't Work

**❌ Combinational function calls**:
```skalp
impl MyModule {
    result = add(x, y)  // ❌ Silently generates broken hardware
}
```

**❌ Sequential function calls**:
```skalp
impl MyModule {
    on(clk.rise) {
        temp <= hash(data)  // ❌ Silently generates broken hardware
    }
}
```

## Architecture Analysis

### HIR Representation

Functions are stored in two locations:
1. **Top-level functions**: `Hir.functions: Vec<HirFunction>`
2. **Impl-block functions**: `HirImplementation.functions: Vec<HirFunction>`

**HirFunction structure**:
```rust
pub struct HirFunction {
    pub id: FunctionId,
    pub is_const: bool,
    pub name: String,
    pub params: Vec<HirParameter>,  // Parameters
    pub return_type: Option<HirType>,
    pub body: Vec<HirStatement>,    // Function body
}

pub struct HirParameter {
    pub name: String,
    pub param_type: HirType,
    pub default_value: Option<HirExpression>,
}
```

**HirCallExpr structure**:
```rust
pub struct HirCallExpr {
    pub function: String,           // Function name
    pub args: Vec<HirExpression>,   // Arguments
}
```

### MIR Conversion Pipeline

**Current path** (`hir_to_mir.rs`):
```
HirAssignment
  ├─ lhs: HirLValue
  └─ rhs: HirExpression::Call  ─────┐
                                     │
                            convert_expression()
                                     │
                                     ├─ HirExpression::Binary ✅ Works
                                     ├─ HirExpression::Index  ✅ Works
                                     ├─ HirExpression::Call   ❌ Returns None
                                     └─ ...
```

When `None` is returned:
```rust
fn convert_continuous_assignment(&mut self, assign: &hir::HirAssignment)
    -> Option<ContinuousAssign>
{
    let lhs = self.convert_lvalue(&assign.lhs)?;
    let rhs = self.convert_expression(&assign.rhs)?;  // ❌ None for function calls
    //                                             ^^^
    //                                             Propagates None upward
    Some(ContinuousAssign { lhs, rhs })
}
```

## Implementation Requirements

### Option 1: Function Inlining (Recommended)

**Approach**: Replace function call with function body, substituting parameters.

**Algorithm**:
1. **Lookup function** by name in `hir.functions` or current impl's functions
2. **Check arity**: Verify `args.len() == params.len()`
3. **Recursion detection**: Fail if function calls itself (no recursion in hardware)
4. **Parameter substitution**: Create mapping `param_name -> arg_expression`
5. **Body inlining**: Transform function body statements into a single expression
6. **Return expression**: Extract the `return` statement's expression

**Example transformation**:
```skalp
// Source
fn add(a: bit[8], b: bit[8]) -> bit[8] {
    return a + b
}
r1 = add(x, y)

// After inlining
r1 = x + y
```

**Complex example**:
```skalp
// Source
fn add_mul(a: bit[8], b: bit[8], c: bit[8]) -> bit[8] {
    let sum = a + b    // ❌ PROBLEM: local variable!
    return sum * c
}
r2 = add_mul(x, y, z)

// After inlining (need to handle let bindings)
r2 = (x + y) * z
```

### Option 2: Function as Module (Alternative)

**Approach**: Convert functions to entities, instantiate as modules.

**Pros**:
- No variable substitution needed
- Can share logic (one module, multiple instances)
- Clear hardware boundaries

**Cons**:
- Adds latency (combinational path through module)
- Can't inline into expressions
- More complex for simple operations

**Example**:
```skalp
// Function becomes entity
entity add_entity {
    in a: bit[8]
    in b: bit[8]
    out result: bit[8]
}
impl add_entity {
    result = a + b
}

// Call becomes instantiation
impl MyModule {
    let add_inst = add_entity { a: x, b: y, result: temp }
    r1 = temp
}
```

## Detailed Inlining Design

### Phase 1: Simple Functions (No Local Variables)

**Supported**:
```skalp
fn add(a: bit[8], b: bit[8]) -> bit[8] {
    return a + b
}
```

**Implementation**:
```rust
fn inline_function_call(&mut self, call: &hir::HirCallExpr)
    -> Option<Expression>
{
    // 1. Find function
    let func = self.find_function(&call.function)?;

    // 2. Check arity
    if func.params.len() != call.args.len() {
        return None;  // Error: wrong number of arguments
    }

    // 3. Check for return statement
    let return_expr = self.extract_return_expr(&func.body)?;

    // 4. Build substitution map
    let mut subst = HashMap::new();
    for (param, arg) in func.params.iter().zip(&call.args) {
        subst.insert(&param.name, arg);
    }

    // 5. Substitute and convert
    let inlined = self.substitute_and_convert(return_expr, &subst)?;

    Some(inlined)
}
```

### Phase 2: Functions with Local Variables

**Supported**:
```skalp
fn complex(a: bit[8], b: bit[8], c: bit[8]) -> bit[8] {
    let sum = a + b
    let prod = sum * c
    return prod
}
```

**Challenge**: Local variables need to be eliminated by inlining their definitions.

**Algorithm** (SSA-like transformation):
1. Parse function body into DAG of dependencies
2. Inline each variable's definition into uses
3. Final expression has no variables, only parameters
4. Substitute parameters with arguments

**Example**:
```
Original:
  let sum = a + b
  let prod = sum * c
  return prod

Step 1 - Inline 'prod':
  let sum = a + b
  return (sum * c)

Step 2 - Inline 'sum':
  return ((a + b) * c)

Final:
  return (a + b) * c
```

### Phase 3: Control Flow

**If expressions**:
```skalp
fn max(a: bit[8], b: bit[8]) -> bit[8] {
    if a > b {
        return a
    } else {
        return b
    }
}

// Inlines to:
result = (a > b) ? a : b
```

**Match expressions**:
```skalp
fn decode(op: bit[2]) -> bit[8] {
    match op {
        0 => return 0x00,
        1 => return 0x01,
        2 => return 0x02,
        _ => return 0xFF,
    }
}

// Inlines to MIR match expression
```

### Limitations

**❌ Not Supported**:

1. **Recursion**:
```skalp
fn factorial(n: nat) -> nat {
    if n == 0 {
        return 1
    } else {
        return n * factorial(n - 1)  // ❌ Infinite inlining
    }
}
```
**Solution**: Detect cycles, error on recursive calls

2. **Mutable variables**:
```skalp
fn increment_twice(x: bit[8]) -> bit[8] {
    let mut y = x
    y = y + 1  // ❌ Mutation not supported
    y = y + 1
    return y
}
```
**Solution**: Only allow immutable `let` bindings

3. **Loops**:
```skalp
fn sum_array(arr: [bit[8]; 4]) -> bit[16] {
    let mut sum = 0
    for i in 0..4 {  // ❌ Loop in combinational logic
        sum = sum + arr[i]
    }
    return sum
}
```
**Solution**: Loop unrolling (future enhancement) or reject

## Implementation Plan

### Step 1: Add Error Handling (Bug Fix)

**Current bug**: Function calls silently generate broken hardware.

**Fix**: Make `convert_expression()` return `Result` instead of `Option`, error on unimplemented features.

```rust
// Before
hir::HirExpression::Call(_call) => None,

// After
hir::HirExpression::Call(call) => {
    return Err(format!("Function calls not yet supported: {}", call.function));
}
```

### Step 2: Simple Function Inlining

**Scope**: Functions with:
- No local variables
- Single `return` statement
- No recursion

**Files to modify**:
- `crates/skalp-mir/src/hir_to_mir.rs`
  - Add `find_function()` method
  - Add `inline_simple_function()` method
  - Replace `None` with inline logic at line 1887

**Test cases**:
```skalp
fn add(a: bit[8], b: bit[8]) -> bit[8] { return a + b }
fn sub(a: bit[8], b: bit[8]) -> bit[8] { return a - b }
fn and(a: bit[8], b: bit[8]) -> bit[8] { return a & b }
```

### Step 3: Functions with Let Bindings

**Scope**: Eliminate local variables by inlining definitions.

**Algorithm**:
1. Build dependency graph of variables
2. Topological sort
3. Inline from leaves to root
4. Substitute parameters

**Test cases**:
```skalp
fn add_mul(a: bit[8], b: bit[8], c: bit[8]) -> bit[8] {
    let sum = a + b
    return sum * c
}
```

### Step 4: Control Flow

**Scope**: If-expressions and match-expressions.

**Test cases**:
```skalp
fn max(a: bit[8], b: bit[8]) -> bit[8] {
    if a > b { return a } else { return b }
}

fn clamp(x: bit[8], min: bit[8], max: bit[8]) -> bit[8] {
    if x < min {
        return min
    } else if x > max {
        return max
    } else {
        return x
    }
}
```

### Step 5: Recursion Detection

**Requirement**: Detect and reject recursive calls.

**Algorithm**:
1. Maintain call stack during inlining
2. Before inlining, check if function is already in stack
3. If yes, error with "Recursive functions not supported"

### Step 6: Documentation and Examples

**Create**:
- `examples/function_examples.sk` - Working examples
- `docs/FUNCTIONS.md` - User guide
- Update `KNOWN_ISSUES.md` - Document limitations

## Expected Benefits

1. **Code Reuse**: Share common logic across designs
2. **Readability**: Named functions instead of inline expressions
3. **Maintainability**: Change in one place, affects all call sites
4. **Library Support**: Enable stdlib functions like `vec_add()`, `vec_dot()`

## Estimated Effort

- **Step 1 (Error handling)**: 2 hours
- **Step 2 (Simple inlining)**: 8 hours
- **Step 3 (Let bindings)**: 12 hours
- **Step 4 (Control flow)**: 8 hours
- **Step 5 (Recursion detection)**: 4 hours
- **Step 6 (Documentation)**: 4 hours

**Total**: ~38 hours (5 days)

## Risks and Mitigations

**Risk 1**: Variable substitution introduces bugs
- **Mitigation**: Comprehensive test suite with edge cases

**Risk 2**: Type checking complexity
- **Mitigation**: Leverage existing HIR type information

**Risk 3**: Breaking existing code
- **Mitigation**: Currently no working function calls, so no breakage

**Risk 4**: Performance impact from deep inlining
- **Mitigation**: Add inlining depth limit (configurable)

## Alternative: Macro System

Instead of function inlining, could implement a macro system like Rust's `macro_rules!`:

```skalp
macro_rules! add {
    ($a:expr, $b:expr) => { $a + $b }
}

impl MyModule {
    result = add!(x, y)  // Expands at parse time
}
```

**Pros**:
- More powerful (pattern matching)
- Explicit about expansion
- No recursion issues

**Cons**:
- Completely different feature
- Requires new syntax
- More complex to implement

## Recommendation

**Implement function inlining** (Option 1) because:
1. Functions already have syntax support
2. More familiar to users (from Rust, C, etc.)
3. Gradual rollout possible (simple → complex)
4. Enables stdlib development

**Priority**: **HIGH** - This is a frequently requested feature that significantly improves code reuse.
