# SKALP Library System

The SKALP library system provides a complete package management solution for hardware description code, inspired by Cargo's design but tailored for HDL development.

## Overview

The library system consists of four main crates that work together:

1. **skalp-resolve** - Module resolution and import handling
2. **skalp-manifest** - Manifest (skalp.toml) parsing and validation
3. **skalp-package** - Dependency resolution and package management
4. Integration with the SKALP compiler for seamless library usage

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                      SKALP Compiler                         │
└────────────────────┬────────────────────────────────────────┘
                     │
                     ├─> skalp-resolve (Module Resolution)
                     │   - Import statement parsing
                     │   - Module path resolution
                     │   - Symbol visibility checking
                     │
                     ├─> skalp-manifest (Manifest Parsing)
                     │   - skalp.toml parsing
                     │   - Dependency specifications
                     │   - Feature flags
                     │   - Override system (patch/replace)
                     │
                     └─> skalp-package (Package Management)
                         - Dependency resolution
                         - Lockfile generation (skalp.lock)
                         - Package caching
                         - Download management
```

## Key Features

### Module System (skalp-resolve)

- **Import Statements**: `use` and `mod` declarations
- **Visibility Control**: `pub`, `pub(crate)` modifiers
- **Path Resolution**: Absolute and relative module paths
- **Symbol Management**: Tracks exported symbols and their visibility
- **Glob Imports**: Support for `use foo::*`
- **Renamed Imports**: Support for `use foo as bar`

**Example:**
```rust
// Import from standard library
use skalp_stdlib::adder::Adder;

// Import with renaming
use my_lib::Counter as MyCounter;

// Glob import
use common::*;

// Module declaration
mod submodule;
```

### Manifest System (skalp-manifest)

- **Package Metadata**: Name, version, authors, license, etc.
- **Dependencies**: Multiple source types (registry, git, path)
- **Feature Flags**: Optional dependencies and conditional compilation
- **Workspace Support**: Multi-package projects
- **Override System**: `[patch]` and `[replace]` sections for development

**Example skalp.toml:**
```toml
[package]
name = "my-dsp-lib"
version = "1.0.0"
authors = ["Your Name <you@example.com>"]
description = "DSP library for SKALP"
license = "MIT"

[lib]
name = "dsp"
path = "src/lib.sk"

[dependencies]
# Registry dependency
skalp-numeric = "2.0"

# Git dependency
skalp-experimental = { git = "https://github.com/skalp/experimental", branch = "main" }

# Path dependency (local development)
skalp-utils = { path = "../utils" }

# Optional dependency
skalp-gpu = { version = "1.0", optional = true }

[features]
default = ["fft"]
fft = []
gpu = ["skalp-gpu"]

# Override for local development
[replace]
skalp-numeric = { path = "../local-numeric" }

[patch."https://github.com/skalp/experimental"]
skalp-experimental = { path = "../local-experimental" }
```

### Package Management (skalp-package)

- **Dependency Resolution**: Resolves transitive dependencies
- **Lockfile Generation**: Creates `skalp.lock` for reproducible builds
- **Package Caching**: SHA256-based caching system
- **Source Support**: Registry, Git (branch/tag/rev), local paths
- **Integrity Checks**: Checksum verification
- **Circular Detection**: Prevents circular dependencies

**Lockfile (skalp.lock):**
```toml
version = 1

[[package]]
name = "skalp-numeric"
type = "registry"
version = "2.0.0"
dependencies = ["skalp-stdlib"]
checksum = "abc123..."

[[package]]
name = "skalp-stdlib"
type = "registry"
version = "1.5.0"
dependencies = []
checksum = "def456..."

[metadata]
generated = "2024-01-01T00:00:00Z"
root = "my-dsp-lib"
```

## Workflow

### Creating a New Library

```bash
# Create a new library project
skalp new my-lib --lib

# This creates:
# my-lib/
#   skalp.toml       # Manifest
#   src/
#     lib.sk         # Library entry point
```

### Adding Dependencies

Edit `skalp.toml`:
```toml
[dependencies]
skalp-numeric = "2.0"
```

### Building with Dependencies

```bash
# Resolve dependencies and generate lockfile
skalp build

# This:
# 1. Reads skalp.toml
# 2. Resolves all dependencies
# 3. Downloads packages to cache
# 4. Generates skalp.lock
# 5. Compiles the project
```

### Using Dependencies in Code

```rust
// In src/lib.sk
use skalp_numeric::Vector;

entity MyProcessor {
    pub input data: Bit<8>,
    pub output result: Bit<16>,
}

impl MyProcessor {
    on clk {
        let v = Vector::new(data);
        result = v.magnitude();
    }
}
```

## Dependency Resolution Algorithm

The resolver implements a depth-first search with cycle detection:

1. **Parse Manifest**: Load and validate `skalp.toml`
2. **Apply Overrides**: Process `[patch]` and `[replace]` sections
3. **Resolve Features**: Determine which features are enabled
4. **Collect Dependencies**: Build dependency graph
5. **Check Cycles**: Detect circular dependencies
6. **Download Packages**: Fetch from registry/git/path
7. **Generate Lockfile**: Write `skalp.lock` with exact versions
8. **Verify Checksums**: Ensure package integrity

## Package Sources

### Registry (Default)

```toml
[dependencies]
lib = "1.0"  # Simple version
lib = { version = "1.0", features = ["extra"] }  # With features
```

### Git Repository

```toml
[dependencies]
lib = { git = "https://github.com/user/lib", branch = "main" }
lib = { git = "https://github.com/user/lib", tag = "v1.0" }
lib = { git = "https://github.com/user/lib", rev = "abc123" }
```

### Local Path

```toml
[dependencies]
lib = { path = "../local-lib" }
lib = { path = "/absolute/path/lib" }
```

## Override System

### Replace (Global Override)

Replaces a dependency everywhere it appears:

```toml
[replace]
skalp-numeric = { path = "../my-numeric" }
```

### Patch (Source-Specific Override)

Patches dependencies from a specific source:

```toml
[patch."https://github.com/skalp/monorepo"]
lib1 = { path = "../local-lib1" }
lib2 = { path = "../local-lib2" }
```

**Precedence**: `replace` > `patch` > original

## Feature Flags

Features enable conditional compilation and optional dependencies:

```toml
[dependencies]
base = "1.0"
optional-lib = { version = "2.0", optional = true }

[features]
default = ["basic"]
basic = []
advanced = ["optional-lib"]
full = ["basic", "advanced"]
```

Enable features:
```bash
skalp build --features advanced
skalp build --features full
skalp build --no-default-features
```

## Workspace Support

For multi-package projects:

```toml
[workspace]
members = [
    "crates/lib1",
    "crates/lib2",
    "examples/*"
]
exclude = ["old-crates"]
```

Each member has its own `skalp.toml`.

## Cache Management

Packages are cached in `~/.cache/skalp/` (or `$SKALP_CACHE_DIR`):

```
~/.cache/skalp/
  ab/
    cd1234.../  # SHA256 hash of cache key
      skalp.toml
      src/
        lib.sk
```

Commands:
```bash
skalp cache --list      # List cached packages
skalp cache --size      # Show cache size
skalp cache --clean     # Clear cache
```

## API Usage

### Programmatic Usage

```rust
use skalp_manifest::Manifest;
use skalp_package::{Resolver, RegistryConfig};

// Load manifest
let manifest = Manifest::from_path("skalp.toml")?;

// Create resolver
let config = RegistryConfig::default();
let resolver = Resolver::new(config).with_manifest(manifest);

// Resolve dependencies
let lockfile = resolver.resolve()?;

// Save lockfile
lockfile.save("skalp.lock")?;
```

## Implementation Status

### ✅ Completed (Phases 1-4)

- [x] Module system (skalp-resolve)
- [x] Manifest parsing (skalp-manifest)
- [x] Dependency overrides (patch/replace)
- [x] Package management core (skalp-package)
- [x] Lockfile generation
- [x] Local caching
- [x] Source abstraction (registry/git/path)

### 🚧 Future Work (Phases 5-8)

- [ ] Registry server implementation
- [ ] HTTP client for package downloads
- [ ] CLI integration (`skalp add`, `skalp remove`, etc.)
- [ ] Workspace member resolution
- [ ] Incremental builds with caching
- [ ] Package publishing workflow
- [ ] Dependency update management
- [ ] Security audit integration

## Testing

All crates have comprehensive test coverage:

- **skalp-resolve**: 9 tests (module resolution, imports, visibility)
- **skalp-manifest**: 31 tests (22 parsing + 9 override)
- **skalp-package**: 21 tests (resolution, caching, lockfiles)

Run tests:
```bash
cargo test -p skalp-resolve
cargo test -p skalp-manifest
cargo test -p skalp-package
```

## Design Principles

1. **Cargo-Inspired**: Familiar workflow for Rust developers
2. **Reproducible Builds**: Lockfile ensures consistency
3. **Local Development**: Override system supports rapid iteration
4. **Type Safety**: Strong typing throughout the API
5. **Validation**: Comprehensive error checking
6. **Extensibility**: Modular design for future features

## Related Documentation

- [Module System Details](MODULE_SYSTEM_PHASE1_COMPLETE.md)
- [Manifest Specification](../examples/skalp.toml.example)
- API docs: `cargo doc --open -p skalp-package`

## Examples

See `examples/` directory for complete examples:
- Simple library
- Library with dependencies
- Workspace project
- Override usage
