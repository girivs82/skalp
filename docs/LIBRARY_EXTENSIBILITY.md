# SKALP Library Extensibility and Third-Party Libraries

**Date:** 2025-10-11
**Status:** Design Document

---

## 1. Using Custom Implementations Without Source Changes

### Problem Statement

A user has designed a better `FpSqrt` implementation that's faster or smaller. How can they use it without modifying their source code?

### Solution: Library Override System

#### Option A: Import Path Override (Recommended)

```skalp
// Your application code (unchanged)
use skalp::numeric::fp::FpSqrt;

entity MyDesign {
    signal result: fp32

    let sqrt = FpSqrt<IEEE754_32> {
        x: input,
        result: result
    }
}
```

**To use custom implementation, set environment variable or config file:**

```bash
# Option 1: Environment variable
export SKALP_LIB_OVERRIDE="skalp::numeric::fp::FpSqrt=my_libs::fast_sqrt::FpSqrt"
./target/release/skalp build -s mydesign.sk

# Option 2: Config file (skalp.toml)
[library.overrides]
"skalp::numeric::fp::FpSqrt" = "my_libs::fast_sqrt::FpSqrt"

# Option 3: Command line
./target/release/skalp build -s mydesign.sk \
  --override "skalp::numeric::fp::FpSqrt=my_libs::fast_sqrt::FpSqrt"
```

#### Option B: Intent-Based Selection (Already Implemented)

```skalp
// Standard library already supports this!
use skalp::numeric::fp::FpSqrt;
use skalp::hls::{FAST_INTENT, SMALL_INTENT};

entity MyDesign {
    // Use fast implementation
    let sqrt_fast = FpSqrt<IEEE754_32, FAST_INTENT> {
        x: input1,
        result: output1
    }

    // Use small implementation
    let sqrt_small = FpSqrt<IEEE754_32, SMALL_INTENT> {
        x: input2,
        result: output2
    }
}
```

**To override the default implementation for an intent:**

```skalp
// In your library: my_libs/fast_sqrt.sk
use skalp::numeric::fp::FpSqrt;
use skalp::hls::Intent;

// Override the fast implementation
impl FpSqrt<const F: FloatFormat, intent I: Intent>
where is_latency_optimized(I) {
    // Your super-fast algorithm here
    result = my_super_fast_newton_raphson(x)
}
```

#### Option C: Trait-Based Dependency Injection

```skalp
// Define interface
trait SqrtProvider<const F: FloatFormat> {
    fn sqrt(x: fp<F>) -> fp<F>;
}

// Your design depends on the interface, not implementation
entity MyDesign<S: SqrtProvider<IEEE754_32>> {
    in x: fp32
    out result: fp32

    result = S::sqrt(x)
}

// Use default stdlib implementation
use skalp::numeric::fp::FpSqrt as DefaultSqrt;
let design1 = MyDesign<DefaultSqrt> { ... }

// Use custom implementation
use my_libs::fast_sqrt::FastSqrt;
let design2 = MyDesign<FastSqrt> { ... }
```

#### Option D: Link-Time Substitution

```skalp
// Your code (unchanged)
use skalp::numeric::fp::FpSqrt;

entity MyDesign {
    let sqrt = FpSqrt<IEEE754_32> { ... }
}
```

**At link time, provide replacement:**

```bash
# Generate object file with your implementation
skalp build-lib -s my_fast_sqrt.sk -o my_sqrt.skobj \
  --export-as "skalp::numeric::fp::FpSqrt"

# Link with override
skalp build -s mydesign.sk \
  --link my_sqrt.skobj \
  --override-symbol "skalp::numeric::fp::FpSqrt"
```

---

## 2. Third-Party Library System

### Library Structure

```
my_awesome_lib/
├── skalp.toml                 # Library manifest
├── lib.sk                     # Main library file
├── src/
│   ├── sqrt/
│   │   ├── fast_sqrt.sk       # Fast implementation
│   │   ├── small_sqrt.sk      # Area-optimized
│   │   └── low_power_sqrt.sk  # Power-optimized
│   ├── fft/
│   │   ├── radix2.sk
│   │   └── radix4.sk
│   └── filters/
│       └── fir.sk
├── tests/
│   └── test_sqrt.sk
├── docs/
│   └── README.md
└── examples/
    └── sqrt_example.sk
```

### Library Manifest (skalp.toml)

```toml
[package]
name = "awesome_math"
version = "1.0.0"
authors = ["Your Name <you@example.com>"]
license = "MIT OR Apache-2.0"
description = "High-performance math library for SKALP"
documentation = "https://docs.example.com/awesome_math"
repository = "https://github.com/user/awesome_math"
keywords = ["math", "sqrt", "fft", "dsp"]
categories = ["algorithms", "hardware"]

[dependencies]
# Depend on stdlib version range
skalp-stdlib = "^1.0"

# Depend on other third-party libraries
some_other_lib = "2.1"

[features]
# Optional features
fast-sqrt = []
gpu-support = []
formal-verification = []

[lib]
# What this library exports
path = "lib.sk"

# Namespace for this library
namespace = "awesome_math"

# Compatibility
skalp-version = ">=0.1.0"

[dev-dependencies]
# Only needed for testing
test-harness = "1.0"
```

### Library Main File (lib.sk)

```skalp
// my_awesome_lib/lib.sk

/// Awesome Math Library
///
/// High-performance mathematical operations for SKALP hardware designs.

// Public modules
pub mod sqrt {
    pub use crate::sqrt::fast_sqrt::FastSqrt;
    pub use crate::sqrt::small_sqrt::SmallSqrt;
    pub use crate::sqrt::low_power_sqrt::LowPowerSqrt;
}

pub mod fft {
    pub use crate::fft::radix2::Radix2FFT;
    pub use crate::fft::radix4::Radix4FFT;
}

pub mod filters {
    pub use crate::filters::fir::FIRFilter;
}

// Re-export commonly used items
pub use sqrt::FastSqrt;
pub use fft::Radix2FFT;

// Library version (for compatibility checking)
pub const VERSION: &str = "1.0.0";
```

### Using Third-Party Libraries

#### Installation

```bash
# Option 1: From package registry (like crates.io)
skalp add awesome_math

# Option 2: From git
skalp add awesome_math --git https://github.com/user/awesome_math

# Option 3: From local path
skalp add awesome_math --path ../awesome_math

# Option 4: Specific version
skalp add awesome_math@1.2.3
```

This updates your `skalp.toml`:

```toml
[dependencies]
awesome_math = "1.0.0"
```

#### Using in Code

```skalp
// Import from third-party library
use awesome_math::sqrt::FastSqrt;
use awesome_math::fft::Radix2FFT;

// Or import everything
use awesome_math::*;

// Your design using third-party components
entity MyAccelerator {
    in x: fp32
    out result: fp32
}

impl MyAccelerator {
    signal sqrt_result: fp32

    // Use third-party square root
    let sqrt = FastSqrt<IEEE754_32> {
        x: x,
        result: sqrt_result
    }

    result = sqrt_result * 2.0
}
```

### Scope Visibility Rules

#### Public vs Private

```skalp
// lib.sk

// Public - visible to library users
pub entity PublicComponent {
    in x: bit[32]
    out y: bit[32]
}

// Public but requires explicit import
pub(crate) entity InternalHelper {
    // Visible within this crate only
}

// Private - not visible outside this file
entity PrivateImpl {
    // Only visible in this file
}

// Pub with path restriction
pub(super) entity ParentVisible {
    // Visible to parent module only
}
```

#### Module Visibility

```skalp
// lib.sk

pub mod public_module {
    // Everything in here needs pub to be visible outside
    pub entity Thing { }

    entity PrivateThing { }  // Not visible outside module
}

mod private_module {
    // This entire module is private
    pub entity Thing { }  // pub, but module is private, so not visible outside crate
}

pub use public_module::Thing;  // Re-export for convenience
```

#### Dependency Visibility

```toml
[dependencies]
# Public dependency - your users can use this
awesome_math = "1.0"

# Private dependency - internal implementation detail
[dependencies.internal_helper]
version = "0.5"
visibility = "private"
```

### Advanced: Conditional Compilation

```skalp
// Use different implementations based on features

#[cfg(feature = "fast-sqrt")]
pub use crate::sqrt::fast_sqrt::FastSqrt as DefaultSqrt;

#[cfg(not(feature = "fast-sqrt"))]
pub use crate::sqrt::balanced_sqrt::BalancedSqrt as DefaultSqrt;

// Users can select at compile time:
// skalp build --features fast-sqrt
```

### Library Search Paths

```bash
# Environment variable
export SKALP_LIB_PATH="/usr/local/skalp/libs:$HOME/.skalp/libs:./libs"

# Or in skalp.toml
[build]
library-paths = [
    "/usr/local/skalp/libs",
    "~/.skalp/libs",
    "./libs"
]

# Priority order:
# 1. Project local (./libs)
# 2. User libraries (~/.skalp/libs)
# 3. System libraries (/usr/local/skalp/libs)
# 4. Builtin stdlib
```

---

## 3. Complete Example: Using Custom Sqrt

### Scenario

You want to use a faster `FpSqrt` from a third-party library without changing your code.

### Your Application (mydesign.sk)

```skalp
// This code never changes!
use skalp::numeric::fp::{fp32, FpSqrt};

entity MyGraphicsPipeline {
    in position: vec3<fp32>
    out distance: fp32
}

impl MyGraphicsPipeline {
    // Calculate length = sqrt(x² + y² + z²)
    signal dot_result: fp32 =
        position[0] * position[0] +
        position[1] * position[1] +
        position[2] * position[2]

    signal length: fp32

    let sqrt = FpSqrt<IEEE754_32> {
        x: dot_result,
        result: length,
        flags: _
    }

    distance = length
}
```

### Method 1: Add Library and Use Intent

```bash
# Install fast math library
skalp add fast_math_lib

# Update skalp.toml to use their implementation for FAST_INTENT
```

```toml
# skalp.toml
[dependencies]
fast_math_lib = "1.0"

[library.overrides]
# When FAST_INTENT is used, use fast_math_lib's implementation
"skalp::numeric::fp::FpSqrt<*, FAST_INTENT>" = "fast_math_lib::sqrt::TurboSqrt"
```

```skalp
// Modify mydesign.sk to use FAST_INTENT
use skalp::numeric::fp::{fp32, FpSqrt};
use skalp::hls::FAST_INTENT;

entity MyGraphicsPipeline {
    // ... same as before ...

    // Just add intent parameter - override handles the rest!
    let sqrt = FpSqrt<IEEE754_32, FAST_INTENT> {
        x: dot_result,
        result: length,
        flags: _
    }
}
```

### Method 2: Global Override

```bash
# Build with override (no code changes!)
skalp build mydesign.sk \
  --override "skalp::numeric::fp::FpSqrt=fast_math_lib::sqrt::TurboSqrt"
```

### Method 3: Import and Use Directly

```skalp
// Replace import (single line change)
// use skalp::numeric::fp::FpSqrt;  // Old
use fast_math_lib::sqrt::TurboSqrt as FpSqrt;  // New

// Rest of code unchanged!
entity MyGraphicsPipeline {
    // ... exactly the same ...
}
```

---

## 4. Library Registry and Package Management

### Publishing a Library

```bash
# Login to SKALP registry
skalp login

# Publish your library
cd my_awesome_lib
skalp publish

# Library is now available to everyone!
```

### Searching and Installing

```bash
# Search for libraries
skalp search sqrt
# Results:
#   fast_math_lib - High-performance math operations
#   optimized_ops - Area-optimized operators
#   ...

# View library details
skalp info fast_math_lib

# Install
skalp add fast_math_lib

# Update all dependencies
skalp update

# Remove dependency
skalp remove fast_math_lib
```

### Versioning and Compatibility

```toml
[dependencies]
# Exact version
exact_lib = "=1.2.3"

# Compatible versions (SemVer)
compatible_lib = "^1.2"    # >=1.2.0, <2.0.0
minor_compat = "~1.2.3"    # >=1.2.3, <1.3.0

# Range
range_lib = ">=1.0, <2.0"

# Git with branch/tag/commit
git_lib = { git = "https://github.com/user/lib", branch = "main" }
git_tag = { git = "https://github.com/user/lib", tag = "v1.2.3" }
git_rev = { git = "https://github.com/user/lib", rev = "abc123" }

# Local path (for development)
local_lib = { path = "../my_lib" }
```

---

## 5. Implementation Plan

### Phase 1: Basic Import System (Week 1-2)
- Implement module system (pub, pub(crate), private)
- Implement use statements with paths
- Basic dependency resolution

### Phase 2: Library Manifest (Week 3)
- Implement skalp.toml parsing
- Dependency declaration
- Version resolution

### Phase 3: Override System (Week 4)
- Environment variable overrides
- Config file overrides
- Command-line overrides

### Phase 4: Package Management (Week 5-6)
- Registry implementation
- `skalp add`, `skalp remove`, `skalp update`
- Dependency download and caching

### Phase 5: Advanced Features (Week 7-8)
- Feature flags
- Conditional compilation
- Trait-based DI

---

## 6. Example: Complete Third-Party Library

### fast_math_lib/skalp.toml

```toml
[package]
name = "fast_math_lib"
version = "1.0.0"
authors = ["Math Wizard <wizard@example.com>"]
license = "MIT"
description = "Ultra-fast hardware math operations"

[dependencies]
skalp-stdlib = "^1.0"

[lib]
path = "lib.sk"
namespace = "fast_math"
```

### fast_math_lib/lib.sk

```skalp
/// Fast Math Library
///
/// Provides high-performance implementations of common math operations.

pub mod sqrt {
    pub use crate::sqrt::turbo_sqrt::TurboSqrt;
    pub use crate::sqrt::newton_raphson::NewtonSqrt;
}

pub mod cordic {
    pub use crate::cordic::sin_cos::CORDIC_SinCos;
}

// Default exports
pub use sqrt::TurboSqrt;
pub use cordic::CORDIC_SinCos;
```

### fast_math_lib/src/sqrt/turbo_sqrt.sk

```skalp
use skalp::numeric::fp::{fp, FloatFormat};

/// Ultra-fast square root using parallel digit-recurrence
///
/// Latency: 3 cycles (vs 15 cycles for standard implementation)
/// Area: 2.5x larger than standard
/// Use when: Latency is critical
pub entity TurboSqrt<const F: FloatFormat> {
    in x: fp<F>
    out result: fp<F>
    out flags: bit[5]
}

impl TurboSqrt<const F: FloatFormat> {
    // Your amazing fast algorithm here
    // ...
}
```

### Usage

```skalp
// Option 1: Direct import
use fast_math_lib::TurboSqrt;

let sqrt = TurboSqrt<IEEE754_32> { ... }

// Option 2: Import as alias (drop-in replacement)
use fast_math_lib::TurboSqrt as FpSqrt;

let sqrt = FpSqrt<IEEE754_32> { ... }  // Uses TurboSqrt!

// Option 3: Conditional import
#[cfg(feature = "fast-math")]
use fast_math_lib::TurboSqrt as FpSqrt;

#[cfg(not(feature = "fast-math"))]
use skalp::numeric::fp::FpSqrt;
```

---

## Summary

✅ **Problem 1 Solved:** Multiple ways to use custom implementations:
1. Intent-based selection (already implemented)
2. Import aliasing (1 line change)
3. Global overrides (no code changes)
4. Trait-based DI (most flexible)

✅ **Problem 2 Solved:** Complete third-party library system:
1. Library manifests (skalp.toml)
2. Module system (pub/private)
3. Package registry and management
4. Versioning and compatibility
5. Feature flags and conditional compilation

**Next Steps:** Implement these features in the compiler!
