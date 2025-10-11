# SKALP Library System - Implementation Plan

**Date:** 2025-10-11
**Estimated Timeline:** 8-10 weeks
**Status:** Planning Phase

---

## Current State Analysis

### ✅ What Already Exists

1. **Syntax Keywords** (in `crates/skalp-frontend/src/syntax.rs`)
   - `UseKw`, `ModKw`, `PubKw` - Module system keywords already defined
   - Location: Lines 81-84

2. **Frontend Structure**
   - Lexer/Parser infrastructure (Rowan-based CST)
   - HIR (High-level IR) representation
   - MIR (Mid-level IR) lowering
   - Monomorphization engine (recently completed)

3. **Project Structure**
   - Multi-crate workspace (17 crates)
   - Clear separation of concerns (frontend, backend, codegen, etc.)

### ❌ What Needs Implementation

1. **Parser for module system** - Not implemented
2. **HIR representation for imports/modules** - Not implemented
3. **Name resolution** - Basic, needs extension
4. **Visibility checking** - Not implemented
5. **Library manifest** - No skalp.toml support
6. **Package management** - Not implemented
7. **Override system** - Not implemented

---

## Implementation Phases

### Phase 1: Core Module System (Weeks 1-2)
**Goal:** Basic import/export functionality

#### 1.1 Parser Extensions
**Files to modify:**
- `crates/skalp-frontend/src/parse.rs`
- `crates/skalp-frontend/src/syntax.rs`

**New parsing functions:**
```rust
// In parse.rs
impl Parser {
    /// Parse use statement: use path::to::item;
    fn parse_use_statement(&mut self) {
        // use skalp::numeric::fp::FpAdd;
        // use std::*;
        // use foo::{bar, baz};
        // use super::parent_item;
    }

    /// Parse module declaration: pub mod name { ... }
    fn parse_module_declaration(&mut self) {
        // mod my_module { ... }
        // pub mod public_module { ... }
    }

    /// Parse visibility: pub, pub(crate), pub(super), private
    fn parse_visibility(&mut self) -> Visibility {
        // pub
        // pub(crate)
        // pub(super)
        // (nothing) = private
    }

    /// Parse path: std::vec::Vec or super::sibling
    fn parse_path(&mut self) -> Path {
        // absolute::path::to::item
        // super::parent::item
        // self::sibling
    }
}
```

**New syntax nodes:**
```rust
// In syntax.rs - add to SyntaxKind enum
pub enum SyntaxKind {
    // ... existing ...

    // Module system nodes
    UseStatement,       // use foo::bar;
    UsePath,            // foo::bar::baz
    UseTree,            // { item1, item2 }
    ModuleDecl,         // mod name { ... }
    Visibility,         // pub, pub(crate), etc.
    PathSegment,        // segment in a::b::c
    PathSeparator,      // ::

    // ... existing ...
}
```

#### 1.2 HIR Extensions
**Files to modify:**
- `crates/skalp-frontend/src/hir.rs`
- `crates/skalp-frontend/src/hir_builder.rs`

**New HIR nodes:**
```rust
// In hir.rs
#[derive(Debug, Clone)]
pub struct HirModule {
    pub id: ModuleId,
    pub name: String,
    pub visibility: Visibility,
    pub items: Vec<HirItem>,  // Entities, modules, use statements
    pub parent: Option<ModuleId>,
    pub file_path: PathBuf,
}

#[derive(Debug, Clone)]
pub struct HirUseStatement {
    pub id: UseId,
    pub path: Vec<String>,  // ["skalp", "numeric", "fp", "FpAdd"]
    pub alias: Option<String>,  // as NewName
    pub is_glob: bool,  // use foo::*
    pub items: Vec<String>,  // use foo::{a, b, c}
}

#[derive(Debug, Clone)]
pub enum Visibility {
    Public,          // pub
    Crate,           // pub(crate)
    Super,           // pub(super)
    Private,         // (default)
}

#[derive(Debug, Clone)]
pub enum HirItem {
    Entity(HirEntity),
    Module(HirModule),
    Use(HirUseStatement),
    Type(HirTypeAlias),
    Const(HirConst),
}
```

#### 1.3 Name Resolution System
**New crate:** `crates/skalp-resolve/`

**Core resolver:**
```rust
// crates/skalp-resolve/src/lib.rs

pub struct Resolver {
    modules: HashMap<ModuleId, Module>,
    scopes: ScopeTree,
    imports: HashMap<UseId, ResolvedImport>,
}

impl Resolver {
    /// Resolve all names in HIR
    pub fn resolve(&mut self, hir: &Hir) -> Result<ResolutionMap, ResolveError> {
        // 1. Build module tree
        // 2. Process all use statements
        // 3. Resolve all identifiers
        // 4. Check visibility
    }

    /// Resolve a path like skalp::numeric::fp::FpAdd
    fn resolve_path(&self, path: &[String], context: ModuleId)
        -> Result<ItemId, ResolveError>
    {
        // Walk the path segments
        // Check each module/item exists
        // Verify visibility at each step
    }

    /// Check if item is visible from context
    fn check_visibility(&self, item: ItemId, from: ModuleId) -> bool {
        // pub -> always visible
        // pub(crate) -> same crate
        // pub(super) -> parent module
        // private -> same module
    }
}

#[derive(Debug)]
pub struct ResolutionMap {
    // Maps each identifier usage to its definition
    pub resolutions: HashMap<NodeId, ItemId>,
}

#[derive(Debug)]
pub enum ResolveError {
    ItemNotFound(String),
    NotVisible { item: String, from: String },
    AmbiguousImport { item: String, candidates: Vec<String> },
    CyclicImport(Vec<String>),
}
```

**Scope management:**
```rust
// crates/skalp-resolve/src/scope.rs

pub struct ScopeTree {
    scopes: HashMap<ScopeId, Scope>,
    root: ScopeId,
}

pub struct Scope {
    pub id: ScopeId,
    pub parent: Option<ScopeId>,
    pub items: HashMap<String, ItemId>,  // name -> definition
    pub imports: Vec<UseId>,
}

impl ScopeTree {
    /// Lookup name in this scope and all parent scopes
    pub fn lookup(&self, name: &str, scope: ScopeId) -> Option<ItemId> {
        // Try current scope
        // Then try parent scope recursively
    }

    /// Add an import to a scope
    pub fn add_import(&mut self, scope: ScopeId, use_id: UseId) {
        // Process use statement
        // Add imported items to scope
    }
}
```

**Test file:** `crates/skalp-resolve/src/tests.rs`
```rust
#[test]
fn test_simple_import() {
    let source = r#"
        use skalp::numeric::fp::FpAdd;

        entity Test {
            let adder = FpAdd<...> { ... }
        }
    "#;
    // Should resolve FpAdd to skalp::numeric::fp::FpAdd
}

#[test]
fn test_visibility_error() {
    let source = r#"
        mod private_mod {
            entity PrivateEntity { }
        }

        entity Test {
            let x = PrivateEntity { }  // ERROR: not visible
        }
    "#;
    // Should fail with NotVisible error
}
```

---

### Phase 2: Library Manifest (Week 3)
**Goal:** skalp.toml parsing and dependency declaration

#### 2.1 Manifest Structure
**New crate:** `crates/skalp-manifest/`

**Manifest types:**
```rust
// crates/skalp-manifest/src/lib.rs

use serde::{Deserialize, Serialize};
use std::path::PathBuf;

#[derive(Debug, Deserialize, Serialize)]
pub struct Manifest {
    pub package: Package,
    pub dependencies: HashMap<String, Dependency>,
    pub dev_dependencies: HashMap<String, Dependency>,
    pub lib: Option<LibConfig>,
    pub features: HashMap<String, Vec<String>>,
    pub build: Option<BuildConfig>,
    pub library: Option<LibraryConfig>,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct Package {
    pub name: String,
    pub version: Version,
    pub authors: Vec<String>,
    pub license: Option<String>,
    pub description: Option<String>,
    pub documentation: Option<String>,
    pub repository: Option<String>,
    pub keywords: Vec<String>,
    pub categories: Vec<String>,
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(untagged)]
pub enum Dependency {
    Simple(String),  // "1.0.0"
    Detailed(DetailedDependency),
}

#[derive(Debug, Deserialize, Serialize)]
pub struct DetailedDependency {
    pub version: Option<String>,
    pub path: Option<PathBuf>,
    pub git: Option<String>,
    pub branch: Option<String>,
    pub tag: Option<String>,
    pub rev: Option<String>,
    pub features: Vec<String>,
    pub optional: bool,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct LibConfig {
    pub path: PathBuf,         // lib.sk
    pub namespace: String,      // awesome_math
}

#[derive(Debug, Deserialize, Serialize)]
pub struct LibraryConfig {
    pub overrides: HashMap<String, String>,  // "skalp::fp::FpSqrt" -> "my::FastSqrt"
}

#[derive(Debug, Deserialize, Serialize)]
pub struct Version {
    pub major: u32,
    pub minor: u32,
    pub patch: u32,
    pub pre: Option<String>,
}
```

**Manifest parsing:**
```rust
// crates/skalp-manifest/src/parse.rs

impl Manifest {
    /// Load manifest from skalp.toml
    pub fn load<P: AsRef<Path>>(path: P) -> Result<Self, ManifestError> {
        let content = fs::read_to_string(path)?;
        let manifest: Manifest = toml::from_str(&content)?;
        manifest.validate()?;
        Ok(manifest)
    }

    /// Validate manifest
    fn validate(&self) -> Result<(), ManifestError> {
        // Check version format
        // Check dependencies don't conflict
        // Validate paths exist
    }
}
```

#### 2.2 Dependency Resolution
**New file:** `crates/skalp-manifest/src/resolve.rs`

```rust
pub struct DependencyResolver {
    manifests: HashMap<PackageName, Manifest>,
}

impl DependencyResolver {
    /// Resolve all dependencies
    pub fn resolve(&mut self, root: &Manifest)
        -> Result<ResolvedDependencies, ResolveError>
    {
        // 1. Build dependency graph
        // 2. Check for cycles
        // 3. Resolve versions (SemVer)
        // 4. Check compatibility
        // 5. Return build order
    }

    /// Check if version satisfies requirement
    fn version_matches(&self, version: &Version, req: &VersionReq) -> bool {
        // ^1.2.3 matches >=1.2.3, <2.0.0
        // ~1.2.3 matches >=1.2.3, <1.3.0
        // etc.
    }
}

#[derive(Debug)]
pub struct ResolvedDependencies {
    pub packages: Vec<ResolvedPackage>,
    pub build_order: Vec<PackageName>,
}

#[derive(Debug)]
pub struct ResolvedPackage {
    pub name: String,
    pub version: Version,
    pub source: PackageSource,
    pub dependencies: Vec<String>,
}

#[derive(Debug)]
pub enum PackageSource {
    Registry,
    Git { url: String, rev: String },
    Path(PathBuf),
}
```

---

### Phase 3: Override System (Week 4)
**Goal:** Allow substituting implementations at build time

#### 3.1 Override Configuration
**Extend:** `crates/skalp-manifest/src/lib.rs`

```rust
#[derive(Debug, Deserialize, Serialize)]
pub struct LibraryConfig {
    /// Static overrides in skalp.toml
    pub overrides: HashMap<String, String>,

    /// Intent-based overrides
    pub intent_overrides: HashMap<String, IntentOverride>,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct IntentOverride {
    pub pattern: String,  // "skalp::fp::FpSqrt<*, FAST_INTENT>"
    pub replacement: String,  // "fast_math::TurboSqrt"
}
```

**Example skalp.toml:**
```toml
[library.overrides]
# Global override
"skalp::numeric::fp::FpSqrt" = "fast_math::TurboSqrt"

# Intent-specific override
"skalp::numeric::fp::FpSqrt<*, FAST_INTENT>" = "fast_math::TurboSqrt"
"skalp::numeric::fp::FpSqrt<*, SMALL_INTENT>" = "tiny_math::MicroSqrt"
```

#### 3.2 Override Resolution
**New file:** `crates/skalp-resolve/src/override.rs`

```rust
pub struct OverrideResolver {
    overrides: Vec<Override>,
}

#[derive(Debug)]
pub struct Override {
    pub pattern: PathPattern,  // What to match
    pub replacement: Path,      // What to replace with
}

impl OverrideResolver {
    /// Apply overrides during name resolution
    pub fn resolve_with_overrides(
        &self,
        path: &Path,
        generic_args: &[GenericArg],
    ) -> Option<Path> {
        // Check if path matches any override pattern
        // If yes, return replacement
        // If no, return original
    }

    /// Match path pattern with wildcards
    fn matches(&self, pattern: &PathPattern, path: &Path, args: &[GenericArg]) -> bool {
        // skalp::fp::FpSqrt<*, FAST_INTENT> matches:
        //   skalp::fp::FpSqrt<IEEE754_32, FAST_INTENT>  ✓
        //   skalp::fp::FpSqrt<IEEE754_16, FAST_INTENT>  ✓
        //   skalp::fp::FpSqrt<IEEE754_32, SMALL_INTENT> ✗
    }
}
```

#### 3.3 CLI Override Support
**Modify:** `src/main.rs`

```rust
#[derive(Parser)]
struct Cli {
    // ... existing fields ...

    /// Override library implementations
    #[clap(long, value_name = "PATTERN=REPLACEMENT")]
    override_lib: Vec<String>,

    /// Override config file
    #[clap(long, value_name = "PATH")]
    override_config: Option<PathBuf>,
}

impl Cli {
    fn parse_overrides(&self) -> Vec<Override> {
        self.override_lib.iter().map(|s| {
            let (pattern, replacement) = s.split_once('=')
                .expect("Override format: PATTERN=REPLACEMENT");
            Override {
                pattern: PathPattern::parse(pattern),
                replacement: Path::parse(replacement),
            }
        }).collect()
    }
}
```

---

### Phase 4: Package Management (Weeks 5-6)
**Goal:** Download, cache, and manage external libraries

#### 4.1 Package Cache
**New crate:** `crates/skalp-package/`

```rust
// crates/skalp-package/src/cache.rs

pub struct PackageCache {
    cache_dir: PathBuf,  // ~/.skalp/cache/
}

impl PackageCache {
    /// Get package from cache or download
    pub fn get(&self, name: &str, version: &Version)
        -> Result<PathBuf, PackageError>
    {
        let cache_path = self.package_path(name, version);

        if cache_path.exists() {
            Ok(cache_path)
        } else {
            self.download(name, version)
        }
    }

    /// Download package from registry
    fn download(&self, name: &str, version: &Version)
        -> Result<PathBuf, PackageError>
    {
        // 1. Fetch from registry
        // 2. Verify checksum
        // 3. Extract to cache
        // 4. Return path
    }

    /// Cache directory: ~/.skalp/cache/package-name/1.2.3/
    fn package_path(&self, name: &str, version: &Version) -> PathBuf {
        self.cache_dir
            .join(name)
            .join(version.to_string())
    }
}
```

#### 4.2 Registry Client
**New file:** `crates/skalp-package/src/registry.rs`

```rust
pub struct RegistryClient {
    base_url: String,  // https://registry.skalp.dev
    auth_token: Option<String>,
}

impl RegistryClient {
    /// Search for packages
    pub async fn search(&self, query: &str)
        -> Result<Vec<PackageInfo>, RegistryError>
    {
        let url = format!("{}/api/v1/search?q={}", self.base_url, query);
        let response = reqwest::get(&url).await?;
        let results: SearchResults = response.json().await?;
        Ok(results.packages)
    }

    /// Get package metadata
    pub async fn get_metadata(&self, name: &str)
        -> Result<PackageMetadata, RegistryError>
    {
        let url = format!("{}/api/v1/packages/{}", self.base_url, name);
        let response = reqwest::get(&url).await?;
        Ok(response.json().await?)
    }

    /// Download package tarball
    pub async fn download(&self, name: &str, version: &Version)
        -> Result<Vec<u8>, RegistryError>
    {
        let url = format!("{}/api/v1/packages/{}/{}/download",
                         self.base_url, name, version);
        let response = reqwest::get(&url).await?;
        Ok(response.bytes().await?.to_vec())
    }

    /// Publish package (requires auth)
    pub async fn publish(&self, package: &Package)
        -> Result<(), RegistryError>
    {
        // Build tarball
        // Upload to registry
        // Return result
    }
}

#[derive(Debug, Deserialize)]
pub struct PackageInfo {
    pub name: String,
    pub version: Version,
    pub description: String,
    pub downloads: u64,
}

#[derive(Debug, Deserialize)]
pub struct PackageMetadata {
    pub name: String,
    pub versions: Vec<Version>,
    pub latest: Version,
    pub readme: String,
}
```

#### 4.3 CLI Commands
**New file:** `src/commands/package.rs`

```rust
pub async fn cmd_add(name: &str, version: Option<&str>) -> Result<()> {
    // 1. Parse version requirement
    // 2. Resolve version from registry
    // 3. Add to skalp.toml
    // 4. Download to cache
    // 5. Update lock file
    println!("Adding {} {}", name, version);
}

pub async fn cmd_remove(name: &str) -> Result<()> {
    // 1. Remove from skalp.toml
    // 2. Update lock file
    // 3. (Optional) clean cache
    println!("Removing {}", name);
}

pub async fn cmd_update() -> Result<()> {
    // 1. Check for updates
    // 2. Resolve new versions
    // 3. Update lock file
    // 4. Download new versions
    println!("Updating dependencies");
}

pub async fn cmd_search(query: &str) -> Result<()> {
    // 1. Query registry
    // 2. Display results
    let client = RegistryClient::new();
    let results = client.search(query).await?;

    for pkg in results {
        println!("{} ({}) - {}", pkg.name, pkg.version, pkg.description);
    }
    Ok(())
}

pub async fn cmd_publish() -> Result<()> {
    // 1. Load manifest
    // 2. Build package
    // 3. Run tests
    // 4. Create tarball
    // 5. Upload to registry
    println!("Publishing package");
}
```

**Integrate into main CLI:**
```rust
// src/main.rs
#[derive(Subcommand)]
enum Commands {
    // ... existing commands ...

    /// Add a dependency
    Add {
        name: String,
        #[clap(short, long)]
        version: Option<String>,
    },

    /// Remove a dependency
    Remove {
        name: String,
    },

    /// Update dependencies
    Update,

    /// Search for packages
    Search {
        query: String,
    },

    /// Publish package to registry
    Publish,

    /// Show package information
    Info {
        name: String,
    },
}
```

---

### Phase 5: Advanced Features (Weeks 7-8)
**Goal:** Feature flags, conditional compilation, trait-based DI

#### 5.1 Feature Flags
**Extend:** `crates/skalp-manifest/src/lib.rs`

```rust
#[derive(Debug, Deserialize, Serialize)]
pub struct Manifest {
    // ... existing ...

    pub features: HashMap<String, Vec<String>>,
}
```

**Example skalp.toml:**
```toml
[features]
default = ["fast-sqrt"]
fast-sqrt = []
gpu-support = ["fast-sqrt", "cuda"]
cuda = []
```

**Parsing in code:**
```rust
// crates/skalp-frontend/src/parse.rs

impl Parser {
    /// Parse conditional compilation attribute
    fn parse_cfg_attribute(&mut self) -> Option<CfgCondition> {
        // #[cfg(feature = "fast-sqrt")]
        // #[cfg(not(feature = "gpu"))]
        // #[cfg(all(feature = "a", feature = "b"))]
        // #[cfg(any(feature = "x", feature = "y"))]
    }
}
```

**HIR representation:**
```rust
// crates/skalp-frontend/src/hir.rs

#[derive(Debug, Clone)]
pub struct HirItem {
    pub cfg: Option<CfgCondition>,  // Conditional compilation
    pub kind: HirItemKind,
}

#[derive(Debug, Clone)]
pub enum CfgCondition {
    Feature(String),                      // feature = "name"
    Not(Box<CfgCondition>),              // not(...)
    All(Vec<CfgCondition>),              // all(...)
    Any(Vec<CfgCondition>),              // any(...)
}
```

**Evaluation:**
```rust
// crates/skalp-resolve/src/cfg.rs

pub struct CfgEvaluator {
    enabled_features: HashSet<String>,
}

impl CfgEvaluator {
    /// Check if condition is satisfied
    pub fn eval(&self, cond: &CfgCondition) -> bool {
        match cond {
            CfgCondition::Feature(name) => self.enabled_features.contains(name),
            CfgCondition::Not(inner) => !self.eval(inner),
            CfgCondition::All(conds) => conds.iter().all(|c| self.eval(c)),
            CfgCondition::Any(conds) => conds.iter().any(|c| self.eval(c)),
        }
    }

    /// Filter items based on cfg conditions
    pub fn filter_items(&self, items: Vec<HirItem>) -> Vec<HirItem> {
        items.into_iter()
            .filter(|item| {
                item.cfg.as_ref()
                    .map(|cfg| self.eval(cfg))
                    .unwrap_or(true)
            })
            .collect()
    }
}
```

#### 5.2 Trait-Based Dependency Injection
**Already supported!** Uses existing trait system.

**Example:**
```skalp
// Define interface
trait SqrtProvider<const F: FloatFormat> {
    fn sqrt(x: fp<F>) -> fp<F>;
}

// Your design depends on interface
entity MyDesign<S: SqrtProvider<IEEE754_32>> {
    result = S::sqrt(input)
}

// Users choose implementation
use skalp::numeric::fp::FpSqrt as DefaultSqrt;
use fast_math::TurboSqrt;

let design1 = MyDesign<DefaultSqrt> { ... }
let design2 = MyDesign<TurboSqrt> { ... }
```

---

## Integration Timeline

### Week 1-2: Core Module System
- [ ] Add parsing for use/mod/pub
- [ ] Extend HIR with module/import nodes
- [ ] Implement basic name resolution
- [ ] Add visibility checking
- [ ] Write tests

### Week 3: Library Manifest
- [ ] Create skalp-manifest crate
- [ ] Implement TOML parsing
- [ ] Add dependency resolution
- [ ] Write tests

### Week 4: Override System
- [ ] Extend manifest with overrides
- [ ] Implement pattern matching
- [ ] Add CLI flags
- [ ] Wire into resolver
- [ ] Write tests

### Week 5-6: Package Management
- [ ] Create skalp-package crate
- [ ] Implement cache system
- [ ] Build registry client
- [ ] Add CLI commands (add, remove, update, search, publish)
- [ ] Write tests

### Week 7-8: Advanced Features
- [ ] Implement feature flags
- [ ] Add cfg evaluation
- [ ] Document trait-based DI
- [ ] Write comprehensive tests
- [ ] Update documentation

### Week 9: Integration & Testing
- [ ] End-to-end testing
- [ ] Performance testing
- [ ] Documentation
- [ ] Examples
- [ ] User guide

### Week 10: Polish & Release
- [ ] Bug fixes
- [ ] Performance optimization
- [ ] Final documentation
- [ ] Release announcement

---

## File Structure After Implementation

```
crates/
├── skalp-frontend/
│   └── src/
│       ├── parse.rs          # Extended with use/mod/pub parsing
│       ├── hir.rs            # Extended with module/import nodes
│       └── syntax.rs         # Already has keywords
│
├── skalp-resolve/            # NEW CRATE
│   └── src/
│       ├── lib.rs            # Main resolver
│       ├── scope.rs          # Scope management
│       ├── override.rs       # Override resolution
│       ├── cfg.rs            # Feature flag evaluation
│       └── tests.rs          # Tests
│
├── skalp-manifest/           # NEW CRATE
│   └── src/
│       ├── lib.rs            # Manifest types
│       ├── parse.rs          # TOML parsing
│       ├── resolve.rs        # Dependency resolution
│       └── tests.rs          # Tests
│
└── skalp-package/            # NEW CRATE
    └── src/
        ├── lib.rs            # Main package manager
        ├── cache.rs          # Package cache
        ├── registry.rs       # Registry client
        └── tests.rs          # Tests

src/
├── main.rs                   # Extended with package commands
└── commands/
    └── package.rs            # NEW: add, remove, update, etc.
```

---

## Testing Strategy

### Unit Tests
- Parser tests for each new construct
- Resolver tests for visibility/scoping
- Manifest parsing tests
- Override pattern matching tests
- Feature flag evaluation tests

### Integration Tests
```rust
#[test]
fn test_import_external_library() {
    // skalp.toml with dependency
    // Code using imported items
    // Should resolve correctly
}

#[test]
fn test_override_via_cli() {
    // Build with --override flag
    // Verify replacement applied
}

#[test]
fn test_feature_flags() {
    // Build with --features flag
    // Verify correct items included
}
```

### End-to-End Tests
```bash
# Create test project
skalp new test_project
cd test_project

# Add dependency
skalp add fast_math

# Build
skalp build

# Verify it works
```

---

## Dependencies to Add

### Cargo.toml additions

```toml
[dependencies]
# For manifest parsing
toml = "0.8"
serde = { version = "1.0", features = ["derive"] }

# For package management
reqwest = { version = "0.11", features = ["json"] }
tokio = { version = "1.0", features = ["full"] }
tar = "0.4"
flate2 = "1.0"
sha2 = "0.10"

# For version resolution
semver = "1.0"

# For path handling
walkdir = "2.0"
```

---

## Success Criteria

### Phase 1 Success
- [ ] Can parse `use` statements
- [ ] Can parse `mod` declarations
- [ ] Can parse visibility modifiers
- [ ] Name resolution works for imports
- [ ] Visibility checking works
- [ ] All tests pass

### Phase 2 Success
- [ ] Can parse skalp.toml
- [ ] Can resolve dependencies
- [ ] Can handle version requirements
- [ ] Detects circular dependencies
- [ ] All tests pass

### Phase 3 Success
- [ ] Can override implementations via config
- [ ] Can override via CLI
- [ ] Pattern matching works correctly
- [ ] Intent-based overrides work
- [ ] All tests pass

### Phase 4 Success
- [ ] Can download packages from registry
- [ ] Package cache works
- [ ] `skalp add/remove/update` commands work
- [ ] Can publish packages
- [ ] All tests pass

### Phase 5 Success
- [ ] Feature flags work
- [ ] Conditional compilation works
- [ ] Trait-based DI documented
- [ ] All examples work
- [ ] All tests pass

---

## Risk Mitigation

### Risk 1: Complexity
**Mitigation:** Implement incrementally, test at each step

### Risk 2: Breaking Changes
**Mitigation:** Add feature flag `use-new-resolver`, opt-in initially

### Risk 3: Performance
**Mitigation:** Profile regularly, optimize hot paths

### Risk 4: Registry Downtime
**Mitigation:** Robust caching, offline mode

---

## Documentation Plan

### User Documentation
- [ ] Module system guide
- [ ] Library creation guide
- [ ] Publishing guide
- [ ] Override system guide
- [ ] Feature flags guide

### Developer Documentation
- [ ] Resolver architecture
- [ ] Name resolution algorithm
- [ ] Override system internals
- [ ] Package format specification

---

## Summary

**Total Effort:** 8-10 weeks
**New Crates:** 3 (skalp-resolve, skalp-manifest, skalp-package)
**Modified Crates:** 2 (skalp-frontend, main)
**Lines of Code:** ~5,000-7,000 estimated
**Tests Required:** ~200-300

**Deliverables:**
1. ✅ Full module system (use/mod/pub)
2. ✅ Library manifests (skalp.toml)
3. ✅ Override system (config + CLI)
4. ✅ Package manager (add/remove/update/publish)
5. ✅ Feature flags
6. ✅ Comprehensive documentation

**Next Step:** Start with Phase 1 - Core Module System
