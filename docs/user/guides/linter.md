# SKALP Linter Guide

The SKALP linter is a static analyzer that catches common mistakes, suggests improvements, and enforces hardware design best practices.

---

## Quick Start

```bash
# Lint a single file
$ skalp lint src/main.sk

# Lint all files in a project
$ skalp lint src/**/*.sk

# Lint with specific severity levels
$ skalp lint src/main.sk -W unused_variable -D width_mismatch

# Allow specific lints
$ skalp lint src/main.sk -A long_combinational
```

---

## Lint Categories

### 1. Unused Code Detection

Finds code elements that are defined but never used.

#### `unused_variable`
**Default:** Warn
**Description:** Detects unused parameters and local variables

```skalp
fn process(data: bit[32], unused: bit[8]) -> bit[32] {
//                        ^^^^^^ warning: unused parameter `unused`
    return data
}
```

**Fix:** Prefix with underscore to indicate intentional:
```skalp
fn process(data: bit[32], _unused: bit[8]) -> bit[32] {
    return data
}
```

---

#### `unused_function`
**Default:** Warn
**Description:** Finds functions that are never called

```skalp
fn never_called() -> bit[32] {
// ^^^^^^^^^^^^ warning: function `never_called` is never used
    return 42
}
```

**Note:** Public functions and test functions are excluded from this check.

---

#### `dead_code`
**Default:** Warn
**Description:** Identifies unreachable code

```skalp
fn example(x: bit[32]) -> bit[32] {
    return x;
    let y = x + 1;  // warning: unreachable code
}
```

---

### 2. Type-Based Lints

Catches type-related issues that could cause unexpected behavior.

#### `width_mismatch`
**Default:** Warn
**Description:** Warns about potential width mismatches in operations

```skalp
fn add_mismatch(a: bit[32], b: bit[64]) -> bit[32] {
    return a + b  // warning: operands have different widths
//             ^ bit[64] will be truncated to bit[32]
}
```

**Fix:** Make widths explicit:
```skalp
fn add_fixed(a: bit[32], b: bit[64]) -> bit[32] {
    return a + b[31:0]  // Explicit truncation
}
```

---

#### `sign_confusion`
**Default:** Warn
**Description:** Detects mixing of signed and unsigned types

```skalp
fn compare(a: bit[32], b: nat[32]) -> bit {
    return a < b  // warning: comparing signed bit[32] with unsigned nat[32]
}
```

---

#### `implicit_truncation`
**Default:** Warn
**Description:** Catches unintended truncation in assignments

```skalp
let x: bit[32] = some_64_bit_value;
// warning: assigning bit[64] to bit[32] causes implicit truncation
```

---

### 3. Hardware-Specific Lints

Enforces hardware design best practices.

#### `long_combinational`
**Default:** Warn
**Description:** Suggests pipelining for long combinational paths

```skalp
fn complex_logic(a, b, c, d, e, f, g, h: bit[32]) -> bit[32] {
    return a & b & c & d & e & f & g & h
//         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ warning: long combinational path
//         help: consider adding pipeline registers
}
```

**Fix:** Add pipeline stages:
```skalp
fn pipelined_logic(clk: bit, a, b, c, d, e, f, g, h: bit[32]) -> bit[32] {
    // Stage 1
    let stage1_a = a & b & c & d;
    let stage1_b = e & f & g & h;

    // Stage 2
    return stage1_a & stage1_b
}
```

---

#### `large_constant`
**Default:** Warn
**Description:** Recommends named constants for large literals

```skalp
let mask = 0xFFFFFFFFFFFFFFFF;
// warning: large constant, consider using a named constant
```

**Fix:**
```skalp
pub const FULL_MASK: bit[64] = 0xFFFFFFFFFFFFFFFF;
let mask = FULL_MASK;
```

---

#### `inferred_latch`
**Default:** Warn
**Description:** Warns about incomplete match expressions

```skalp
fn decode(opcode: bit[3]) -> bit[32] {
    return match opcode {
        0 => 1,
        1 => 2,
        2 => 3
        // warning: match is not exhaustive, may create latch
    }
}
```

**Fix:** Add default case:
```skalp
fn decode(opcode: bit[3]) -> bit[32] {
    return match opcode {
        0 => 1,
        1 => 2,
        2 => 3,
        _ => 0  // Default case prevents latch
    }
}
```

---

#### `clock_domain_crossing`
**Default:** Deny (Error)
**Description:** Detects unsafe clock domain crossings

```skalp
fn unsafe_cdc(clk_a: bit, clk_b: bit, data: bit[32]) -> bit[32] {
    // error: unsafe clock domain crossing from clk_a to clk_b
    // help: use proper CDC synchronizer
}
```

---

## Configuration

### Command-Line Flags

**Warning Level:**
```bash
# Warn about specific lint
$ skalp lint -W unused_variable src/main.sk

# Treat lint as error
$ skalp lint -D width_mismatch src/main.sk

# Allow (disable) specific lint
$ skalp lint -A long_combinational src/main.sk
```

**Multiple Lints:**
```bash
$ skalp lint -W unused_variable -W unused_function -D width_mismatch src/main.sk
```

---

### In-Code Configuration

**File-level:**
```skalp
#![allow(unused_variable)]
#![warn(width_mismatch)]
#![deny(clock_domain_crossing)]

// Rest of file
```

**Function-level:**
```skalp
#[allow(long_combinational)]
fn intentionally_long_path() -> bit[32] {
    // Long path is intentional here
}
```

**Item-level:**
```skalp
#[allow(unused_variable)]
fn debug_function(
    data: bit[32],
    debug_info: bit[64]  // Unused but kept for debugging
) -> bit[32] {
    return data
}
```

---

## Configuration File

Create `.skalp-lint.toml` in your project root:

```toml
# Default lint levels
[lints]
unused_variable = "warn"
unused_function = "warn"
dead_code = "warn"
width_mismatch = "warn"
sign_confusion = "warn"
implicit_truncation = "warn"
long_combinational = "warn"
large_constant = "warn"
inferred_latch = "warn"
clock_domain_crossing = "deny"

# Project-specific overrides
[project]
# Allow intentionally long paths in DSP module
allow_in = ["src/dsp/**"]
lints = { long_combinational = "allow" }
```

---

## Best Practices

### 1. Run Linter Before Commit

Add to your pre-commit hook:
```bash
#!/bin/bash
skalp lint src/**/*.sk
if [ $? -ne 0 ]; then
    echo "Linter failed. Fix issues before committing."
    exit 1
fi
```

---

### 2. Fix Warnings Early

Don't accumulate lint warnings. Fix them as you write code.

**Good:**
```skalp
// Clean code, no warnings
fn process(data: bit[32]) -> bit[32] {
    return data + 1
}
```

**Bad:**
```skalp
// Accumulates warnings
fn process(data: bit[32], unused1: bit, unused2: bit, unused3: bit) -> bit[32] {
    let temp = 0;  // unused
    return data + 1
}
```

---

### 3. Use Underscore for Intentional Unused

```skalp
// Good: Indicates intention
fn debug_wrapper(_debug_data: bit[64], actual_data: bit[32]) -> bit[32] {
    return actual_data
}

// Bad: Looks like mistake
fn debug_wrapper(debug_data: bit[64], actual_data: bit[32]) -> bit[32] {
//                ^^^^^^^^^^^ warning: unused parameter
    return actual_data
}
```

---

### 4. Add Comments for Suppressed Lints

```skalp
// Long combinational path is required by spec
#[allow(long_combinational)]
fn required_single_cycle(inputs: bit[256]) -> bit[32] {
    // Complex single-cycle computation
}
```

---

## Integration with CI/CD

### GitHub Actions

```yaml
name: SKALP Lint

on: [push, pull_request]

jobs:
  lint:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - name: Install SKALP
        run: cargo install skalp
      - name: Run Linter
        run: skalp lint src/**/*.sk
```

---

### GitLab CI

```yaml
skalp-lint:
  stage: test
  script:
    - cargo install skalp
    - skalp lint src/**/*.sk
```

---

## IDE Integration

The SKALP LSP server automatically runs lint checks and displays them in your editor.

**VSCode:**
- Warnings appear with yellow squiggles
- Errors appear with red squiggles
- Hover for detailed message
- Quick fixes available

**Vim/Neovim:**
```vim
" Use with ALE or coc.nvim
let g:ale_linters = {'skalp': ['skalp-lint']}
```

---

## Custom Lints (Advanced)

You can create custom project-specific lints:

```rust
// my_project_lints/src/lib.rs

use skalp_lint::{Lint, LintPass, LintContext};

pub static MY_CUSTOM_LINT: Lint = Lint {
    name: "my_custom_lint",
    description: "Project-specific lint rule",
    default_level: LintLevel::Warn,
};

pub struct MyCustomLint;

impl LintPass for MyCustomLint {
    fn check_function(&mut self, func: &HirFunction, ctx: &mut LintContext) {
        // Your custom logic here
    }
}
```

---

## FAQ

**Q: Can I disable all lints?**
A: Yes, but not recommended. Use `#![allow(clippy::all)]` at file level.

**Q: How do I report a false positive?**
A: File an issue at https://github.com/skalp-lang/skalp/issues

**Q: Performance impact?**
A: Minimal. Linting is incremental and runs in parallel with compilation.

**Q: Can I run linter standalone?**
A: Yes! `skalp lint` works without building.

---

## Summary

The SKALP linter helps you:
- ✅ Catch mistakes early
- ✅ Maintain code quality
- ✅ Follow hardware best practices
- ✅ Learn good design patterns

**Start using it today:**
```bash
$ skalp lint src/**/*.sk
```

---

## Next Steps

- [Trait System Guide](../reference/traits.md)
- [Testing Guide](testing.md)
- [Simulation Guide](simulation.md)
- [Examples](../../examples/)
