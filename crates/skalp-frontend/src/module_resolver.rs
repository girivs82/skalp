//! Module Resolution System
//!
//! Handles finding and loading SKALP modules based on import paths.
//! Resolves `use` statements to actual file paths and loads dependencies.
//!
//! Supports both `.sk` source files and `.skh` header files for compiled IP.
//! When resolving imports, `.sk` files take precedence over `.skh` files.

use anyhow::{bail, Context, Result};
use indexmap::IndexMap;
use std::collections::HashSet;
use std::path::{Path, PathBuf};
use tracing::{debug, trace};

use crate::hir::{Hir, HirImport, HirImportPath};
use crate::hir_builder::HirBuilderContext;
use crate::parse;

/// Supported file extensions for SKALP modules
/// `.sk` - Source files (full implementation)
/// `.skh` - Header files (compiled IP declarations)
const MODULE_EXTENSIONS: [&str; 2] = ["sk", "skh"];

/// Try to find a module file with either `.sk` or `.skh` extension
/// Returns the first existing file path, preferring `.sk` over `.skh`
fn try_module_extensions(base_path: &Path) -> Option<PathBuf> {
    for ext in MODULE_EXTENSIONS {
        let mut path = base_path.to_path_buf();
        path.set_extension(ext);
        if path.exists() {
            return Some(path);
        }
    }
    None
}

/// Module resolver handles finding and loading modules
pub struct ModuleResolver {
    /// Search paths for modules (like SKALP_PATH or lib directories)
    search_paths: Vec<PathBuf>,

    /// Cache of loaded modules (path -> HIR)
    loaded_modules: IndexMap<PathBuf, Hir>,

    /// Set of modules currently being loaded (for cycle detection)
    loading: HashSet<PathBuf>,

    /// Root directory of the current project
    root_dir: PathBuf,
}

impl ModuleResolver {
    /// Create a new module resolver
    pub fn new(root_dir: PathBuf) -> Self {
        let mut search_paths = vec![root_dir.clone()];

        // Check for sibling lib/ directory (common pattern: src/ and lib/)
        if let Some(parent) = root_dir.parent() {
            let lib_dir = parent.join("lib");
            if lib_dir.exists() && lib_dir.is_dir() {
                search_paths.push(lib_dir);
            }
        }

        // Try to read skalp.toml to get src_dirs configuration
        let manifest_path = root_dir.join("skalp.toml");
        if manifest_path.exists() {
            trace!("[MODULE_RESOLVER] Found skalp.toml at: {:?}", manifest_path);
            if let Ok(contents) = std::fs::read_to_string(&manifest_path) {
                if let Ok(manifest) = toml::from_str::<serde_json::Value>(&contents) {
                    // Try to extract src_dirs from [build] section
                    if let Some(build) = manifest.get("build") {
                        if let Some(src_dirs_value) = build.get("src_dirs") {
                            if let Some(src_dirs) = src_dirs_value.as_array() {
                                for dir in src_dirs {
                                    if let Some(dir_str) = dir.as_str() {
                                        let dir_path = root_dir.join(dir_str);
                                        if dir_path.exists() {
                                            search_paths.push(dir_path);
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        // Add standard library path(s) if they exist
        // Supports colon-separated paths (e.g., "/path1:/path2")
        if let Ok(stdlib_paths) = std::env::var("SKALP_STDLIB_PATH") {
            for path_str in stdlib_paths.split(':') {
                if !path_str.is_empty() {
                    search_paths.push(PathBuf::from(path_str));
                }
            }
        } else {
            // Default stdlib location relative to root
            let default_stdlib = root_dir.join("stdlib");
            if default_stdlib.exists() {
                search_paths.push(default_stdlib);
            }

            // Search upward through parent directories for crates/skalp-stdlib
            // This handles projects at different nesting levels (e.g., Karythra vs SKALP own tests)
            // First, convert root_dir to absolute path
            let abs_root = root_dir
                .canonicalize()
                .unwrap_or_else(|_| std::env::current_dir().unwrap().join(&root_dir));

            let mut current = Some(abs_root.as_path());
            let mut found_stdlib = false;

            while let Some(dir) = current {
                // Check current/crates/skalp-stdlib
                let potential_stdlib = dir.join("crates/skalp-stdlib");
                if potential_stdlib.exists() && potential_stdlib.is_dir() {
                    search_paths.push(potential_stdlib);
                    found_stdlib = true;
                    break;
                }

                // Also check sibling 'hls' directory: ../hls/crates/skalp-stdlib
                // This handles the case where we have /src/hw/karythra and /src/hw/hls as siblings
                if let Some(parent) = dir.parent() {
                    let hls_stdlib = parent.join("hls/crates/skalp-stdlib");
                    if hls_stdlib.exists() && hls_stdlib.is_dir() {
                        search_paths.push(hls_stdlib);
                        found_stdlib = true;
                        break;
                    }
                }

                current = dir.parent();
            }

            if !found_stdlib {
                trace!("[MODULE_RESOLVER] Warning: Could not find SKALP stdlib. Imports like 'use bitops::*' will fail.");
                trace!("[MODULE_RESOLVER] Hint: Set SKALP_STDLIB_PATH environment variable or ensure crates/skalp-stdlib exists in a parent directory.");
            }
        }

        Self {
            search_paths,
            loaded_modules: IndexMap::new(),
            loading: HashSet::new(),
            root_dir,
        }
    }

    /// Add a search path for modules
    pub fn add_search_path(&mut self, path: PathBuf) {
        self.search_paths.push(path);
    }

    /// Get the search paths (for stdlib detection)
    pub fn search_paths(&self) -> &[PathBuf] {
        &self.search_paths
    }

    /// Preload stdlib numeric modules for fp32/fp16/fp64 trait implementations
    ///
    /// This ensures that trait implementations like `impl Sqrt for fp32` are available
    /// even when the source file doesn't explicitly import them.
    ///
    /// Bug Fix: Without this, calls like `sqrt(fp32_value)` fail because the trait
    /// implementation isn't found.
    pub fn preload_stdlib_numeric(&mut self) -> Result<()> {
        // Find the stdlib skalp/numeric directory in search paths
        for search_path in self.search_paths.clone() {
            // Check for skalp/numeric/fp.sk (contains fp32 trait implementations)
            let fp_path = search_path.join("skalp/numeric/fp.sk");
            if fp_path.exists() {
                let _ = self.load_module(&fp_path);
            }

            // Check for skalp/numeric/traits.sk (contains trait definitions)
            let traits_path = search_path.join("skalp/numeric/traits.sk");
            if traits_path.exists() {
                let _ = self.load_module(&traits_path);
            }
        }
        Ok(())
    }

    /// Resolve an import path to a file path
    ///
    /// Examples:
    /// - `use skalp::numeric::fp` -> `skalp/numeric/fp.sk`
    /// - `use foo::bar::Baz` -> `foo/bar.sk` (Baz is a symbol in bar.sk)
    /// - `use crate::foo::bar` -> `<root_dir>/foo/bar.sk`
    pub fn resolve_import_path(&self, import: &HirImport) -> Result<PathBuf> {
        let segments = match &import.path {
            HirImportPath::Simple { segments } => segments,
            HirImportPath::Renamed { segments, .. } => segments,
            HirImportPath::Glob { segments } => segments,
            HirImportPath::Nested { prefix, .. } => prefix,
        };

        if segments.is_empty() {
            bail!("Empty import path");
        }

        // Handle `crate::` prefix - resolve relative to root_dir
        if segments.first().map(|s| s.as_str()) == Some("crate") {
            trace!("[MODULE_RESOLVER] Resolving crate:: import");
            // Skip the "crate" segment and resolve from root_dir
            let remaining_segments = &segments[1..];

            // Try full path as module (.sk or .skh)
            let mut full_path = self.root_dir.clone();
            for segment in remaining_segments {
                full_path.push(segment);
            }

            if let Some(resolved) = try_module_extensions(&full_path) {
                return Ok(resolved);
            }

            // Try without last segment (last segment is symbol name)
            if remaining_segments.len() > 1 {
                let mut partial_path = self.root_dir.clone();
                for segment in &remaining_segments[..remaining_segments.len() - 1] {
                    partial_path.push(segment);
                }

                if let Some(resolved) = try_module_extensions(&partial_path) {
                    return Ok(resolved);
                }
            }

            // Try with lib.sk or lib.skh (module directory with lib file)
            let mut lib_path = self.root_dir.clone();
            for segment in remaining_segments {
                lib_path.push(segment);
            }
            lib_path.push("lib");

            if let Some(resolved) = try_module_extensions(&lib_path) {
                return Ok(resolved);
            }

            bail!(
                "Could not find crate module for import: {}",
                segments.join("::")
            );
        }

        // Try to resolve the path
        // The last segment might be a symbol name or a module name
        // We try both: foo/bar/baz.sk and foo/bar.sk (where baz is a symbol)
        // Also supports .skh files for compiled IP headers (with .sk taking precedence)

        for search_path in &self.search_paths {
            // Try full path as module (.sk or .skh)
            let mut full_path = search_path.clone();
            for segment in segments {
                full_path.push(segment);
            }

            if let Some(resolved) = try_module_extensions(&full_path) {
                return Ok(resolved);
            }

            // Try without last segment (last segment is symbol name)
            if segments.len() > 1 {
                let mut partial_path = search_path.clone();
                for segment in &segments[..segments.len() - 1] {
                    partial_path.push(segment);
                }

                if let Some(resolved) = try_module_extensions(&partial_path) {
                    return Ok(resolved);
                }
            }

            // Try with lib.sk or lib.skh (module directory with lib file)
            let mut lib_path = search_path.clone();
            for segment in segments {
                lib_path.push(segment);
            }
            lib_path.push("lib");

            if let Some(resolved) = try_module_extensions(&lib_path) {
                return Ok(resolved);
            }
        }

        bail!("Could not find module for import: {}", segments.join("::"))
    }

    /// Load a module from a file path
    pub fn load_module(&mut self, path: &Path) -> Result<()> {
        trace!("[MODULE_RESOLVER] Loading module: {:?}", path);

        // Check if already loaded
        if self.loaded_modules.contains_key(path) {
            return Ok(());
        }

        // Check for circular dependency
        if self.loading.contains(path) {
            trace!("[MODULE_RESOLVER] Circular dependency detected: {:?}", path);
            bail!("Circular dependency detected: {:?}", path);
        }

        // Mark as loading
        trace!("[MODULE_RESOLVER] Marking as loading: {:?}", path);
        self.loading.insert(path.to_path_buf());

        // Read and parse the file
        trace!("[MODULE_RESOLVER] Reading file: {:?}", path);
        let source = std::fs::read_to_string(path)
            .with_context(|| format!("Failed to read module file: {:?}", path))?;

        // Parse to syntax tree
        let (syntax_tree, parse_errors) = parse::parse_with_errors(&source);

        if !parse_errors.is_empty() {
            let error_msg = format!(
                "Parsing failed with {} errors: {}",
                parse_errors.len(),
                parse_errors
                    .first()
                    .map(|e| &e.message)
                    .unwrap_or(&"unknown error".to_string())
            );
            bail!(error_msg);
        }

        // Build HIR
        trace!("[MODULE_RESOLVER] Building HIR for {:?}", path);
        let mut builder = HirBuilderContext::new();
        let mut hir = builder.build(&syntax_tree).map_err(|errors| {
            anyhow::anyhow!(
                "Failed to build HIR: {}",
                errors
                    .first()
                    .map(|e| e.message.clone())
                    .unwrap_or_else(|| "unknown error".to_string())
            )
        })?;

        // Recursively load dependencies first
        let mut dep_paths = Vec::new();
        for import in &hir.imports {
            let dep_path = self.resolve_import_path(import)?;
            trace!("[MODULE_RESOLVER] Resolved import to: {:?}", dep_path);
            // Recursively load (this will use cache if already loaded)
            self.load_module(&dep_path)?;
            dep_paths.push((import.clone(), dep_path));
        }

        // NOW merge imported symbols INTO the module's HIR before caching
        // This ensures that when we later use this module as a source for merging,
        // it has all the trait definitions from its imports
        for (import, dep_path) in &dep_paths {
            if let Some(dep_hir) = self.loaded_modules.get(dep_path) {
                // Merge trait definitions from dependency if this import references them
                Self::merge_import_into_hir(&mut hir, dep_hir, import);
            }
        }

        // BUG FIX #117: Rebuild module with imported type aliases for correct type inference.
        // During the initial build above, type aliases from imported modules (e.g., q8_8 = fixed<16,8,true>)
        // aren't available yet, causing wider_type() to use default 32-bit widths for Custom types.
        // After loading dependencies, rebuild with type aliases preregistered from dependency HIRs
        // so get_type_width() can properly resolve Custom type widths through the alias chain.
        {
            let has_dep_type_aliases = dep_paths.iter().any(|(_, dep_path)| {
                self.loaded_modules
                    .get(dep_path)
                    .map_or(false, |dep_hir| !dep_hir.type_aliases.is_empty())
            });

            if has_dep_type_aliases {
                let mut rebuild_builder = HirBuilderContext::new();

                // Preregister type aliases and other symbols from dependency HIRs
                for (_, dep_path) in &dep_paths {
                    if let Some(dep_hir) = self.loaded_modules.get(dep_path) {
                        for type_alias in &dep_hir.type_aliases {
                            rebuild_builder.preregister_type_alias(type_alias);
                        }
                        for distinct in &dep_hir.distinct_types {
                            rebuild_builder.preregister_distinct_type(distinct);
                        }
                        for function in &dep_hir.functions {
                            rebuild_builder.preregister_function(function);
                        }
                        for implementation in &dep_hir.implementations {
                            for function in &implementation.functions {
                                rebuild_builder.preregister_function(function);
                            }
                            for constant in &implementation.constants {
                                rebuild_builder.preregister_constant(constant);
                            }
                        }
                    }
                }
                // Also preregister from already-merged hir (local + merged content)
                for type_alias in &hir.type_aliases {
                    rebuild_builder.preregister_type_alias(type_alias);
                }
                for distinct in &hir.distinct_types {
                    rebuild_builder.preregister_distinct_type(distinct);
                }
                for entity in &hir.entities {
                    rebuild_builder.preregister_entity(entity);
                }
                for function in &hir.functions {
                    rebuild_builder.preregister_function(function);
                }
                for implementation in &hir.implementations {
                    for function in &implementation.functions {
                        rebuild_builder.preregister_function(function);
                    }
                    for constant in &implementation.constants {
                        rebuild_builder.preregister_constant(constant);
                    }
                }

                if let Ok(mut rebuilt_hir) = rebuild_builder.build(&syntax_tree) {
                    // Re-merge imported symbols into rebuilt HIR
                    for (import, dep_path) in &dep_paths {
                        if let Some(dep_hir) = self.loaded_modules.get(dep_path) {
                            Self::merge_import_into_hir(&mut rebuilt_hir, dep_hir, import);
                        }
                    }
                    hir = rebuilt_hir;
                }
            }
        }

        // Mark as done loading
        self.loading.remove(path);

        // Cache the module (now with merged imports)
        self.loaded_modules.insert(path.to_path_buf(), hir);

        Ok(())
    }

    /// Load all dependencies for a HIR
    pub fn resolve_dependencies(&mut self, hir: &Hir) -> Result<Vec<PathBuf>> {
        let mut resolved = Vec::new();

        for import in &hir.imports {
            let path = self.resolve_import_path(import)?;
            self.load_module(&path)?;
            resolved.push(path);
        }

        Ok(resolved)
    }

    /// Get a loaded module by path
    pub fn get_module(&self, path: &Path) -> Option<&Hir> {
        self.loaded_modules.get(path)
    }

    /// Get all loaded modules
    pub fn loaded_modules(&self) -> impl Iterator<Item = (&PathBuf, &Hir)> {
        self.loaded_modules.iter()
    }

    /// Merge imported symbols from a dependency INTO the target HIR
    /// This ensures that when a module imports trait definitions, those definitions
    /// are included in the module's HIR when it's cached.
    fn merge_import_into_hir(target: &mut Hir, source: &Hir, import: &HirImport) {
        // Extract the symbol names being imported
        let symbol_names: Vec<String> = match &import.path {
            HirImportPath::Simple { segments } => {
                segments.last().map(|s| vec![s.clone()]).unwrap_or_default()
            }
            HirImportPath::Renamed { segments, alias: _ } => {
                segments.last().map(|s| vec![s.clone()]).unwrap_or_default()
            }
            HirImportPath::Glob { .. } => {
                // For glob imports, get all trait definition names
                source
                    .trait_definitions
                    .iter()
                    .map(|t| t.name.clone())
                    .collect()
            }
            HirImportPath::Nested { paths, .. } => paths
                .iter()
                .filter_map(|p| match p {
                    HirImportPath::Simple { segments } => segments.last().cloned(),
                    HirImportPath::Renamed { segments, .. } => segments.last().cloned(),
                    _ => None,
                })
                .collect(),
        };

        // Merge trait definitions that are being imported
        for name in &symbol_names {
            if let Some(trait_def) = source.trait_definitions.iter().find(|t| &t.name == name) {
                // Only add if not already present
                if !target
                    .trait_definitions
                    .iter()
                    .any(|t| t.name == trait_def.name)
                {
                    target.trait_definitions.push(trait_def.clone());
                }
            }
        }

        // FP ENTITY FIX: Merge entities and their implementations from source
        // This is critical for trait method inlining when trait methods instantiate entities
        // (e.g., impl Add for fp32 { fn add() { let adder = FpAdd<IEEE754_32> {...}; }})
        // For glob imports, merge ALL public entities. For specific imports, only merge named entities.
        //
        // BUG FIX: Build entity ID mapping for ALL source entities so that instances
        // referencing other entities (e.g., FpSub's 'adder' instance referencing FpAdd)
        // get their entity IDs properly remapped.

        // Phase 1: Build entity ID mapping (source entity ID -> target entity ID)
        let mut entity_id_map: IndexMap<crate::hir::EntityId, crate::hir::EntityId> =
            IndexMap::new();

        // Track impl blocks to merge (source entity ID, impl block reference)
        let mut impl_blocks_to_merge: Vec<(crate::hir::EntityId, crate::hir::HirImplementation)> =
            Vec::new();

        for entity in &source.entities {
            if entity.visibility == crate::hir::HirVisibility::Public
                || matches!(import.path, HirImportPath::Glob { .. })
            {
                // Check if entity already exists in target
                if let Some(existing) = target.entities.iter().find(|e| e.name == entity.name) {
                    // Entity already exists - map source ID to existing target ID
                    entity_id_map.insert(entity.id, existing.id);
                } else {
                    // Assign new entity ID to avoid collision
                    let new_entity_id = crate::hir::EntityId(
                        target.entities.iter().map(|e| e.id.0).max().unwrap_or(0) + 1,
                    );
                    entity_id_map.insert(entity.id, new_entity_id);

                    let mut new_entity = entity.clone();
                    new_entity.id = new_entity_id;
                    target.entities.push(new_entity);

                    // Remember to merge the corresponding implementation
                    if let Some(impl_block) = source
                        .implementations
                        .iter()
                        .find(|i| i.entity == entity.id)
                    {
                        impl_blocks_to_merge.push((entity.id, impl_block.clone()));
                    }
                }
            }
        }

        // Phase 2: Merge impl blocks and remap all entity references (including instances)
        for (old_entity_id, impl_block) in impl_blocks_to_merge {
            let new_entity_id = entity_id_map
                .get(&old_entity_id)
                .copied()
                .unwrap_or(old_entity_id);

            let mut new_impl = impl_block;
            new_impl.entity = new_entity_id;

            // CRITICAL: Remap entity IDs in instances that reference other entities
            // This fixes the bug where FpSub's 'adder' instance had wrong EntityId
            for instance in &mut new_impl.instances {
                if let Some(remapped_entity_id) = entity_id_map.get(&instance.entity) {
                    instance.entity = *remapped_entity_id;
                }
            }

            target.implementations.push(new_impl);
        }

        // FP TRAIT FIX: Merge all trait IMPLEMENTATIONS from source
        // This is critical for trait-based operator resolution (e.g., impl Add for fp32)
        // For glob imports, merge ALL trait implementations. For specific imports, we still
        // merge all because trait implementations don't have visibility and are needed
        // for any code that uses the imported types.
        for trait_impl in &source.trait_implementations {
            // Check if we already have this trait implementation
            let already_exists = target.trait_implementations.iter().any(|ti| {
                ti.trait_name == trait_impl.trait_name && {
                    // Compare targets - both Custom and primitive float types
                    match (&ti.target, &trait_impl.target) {
                        (
                            crate::hir::TraitImplTarget::Type(t1),
                            crate::hir::TraitImplTarget::Type(t2),
                        ) => {
                            // Compare type names
                            let n1 = match t1 {
                                crate::hir::HirType::Custom(n) => Some(n.as_str()),
                                crate::hir::HirType::Float32 => Some("fp32"),
                                crate::hir::HirType::Float64 => Some("fp64"),
                                crate::hir::HirType::Float16 => Some("fp16"),
                                _ => None,
                            };
                            let n2 = match t2 {
                                crate::hir::HirType::Custom(n) => Some(n.as_str()),
                                crate::hir::HirType::Float32 => Some("fp32"),
                                crate::hir::HirType::Float64 => Some("fp64"),
                                crate::hir::HirType::Float16 => Some("fp16"),
                                _ => None,
                            };
                            n1 == n2
                        }
                        _ => false,
                    }
                }
            });

            if !already_exists {
                target.trait_implementations.push(trait_impl.clone());

                // BUG #207 FIX: Also merge entities that this trait impl's methods depend on
                // When a trait impl like `impl Add for fp32` has a method that instantiates
                // an entity (e.g., FpAdd<IEEE754_32>), we need to merge that entity
                for method in &trait_impl.method_implementations {
                    let entity_names = Self::extract_entity_refs_from_statements(&method.body);
                    for entity_name in entity_names {
                        // Check if entity already exists in target
                        if !target.entities.iter().any(|e| e.name == entity_name) {
                            // Find and merge the entity from source
                            if let Some(entity) =
                                source.entities.iter().find(|e| e.name == entity_name)
                            {
                                // Assign new entity ID
                                let new_entity_id = crate::hir::EntityId(
                                    target.entities.iter().map(|e| e.id.0).max().unwrap_or(0) + 1,
                                );
                                let old_entity_id = entity.id;

                                let mut new_entity = entity.clone();
                                new_entity.id = new_entity_id;
                                target.entities.push(new_entity);

                                // Also merge the entity's implementation
                                if let Some(impl_block) = source
                                    .implementations
                                    .iter()
                                    .find(|i| i.entity == old_entity_id)
                                {
                                    let mut new_impl = impl_block.clone();
                                    new_impl.entity = new_entity_id;
                                    target.implementations.push(new_impl);
                                }
                            }
                        }
                    }
                }
            }
        }

        // BUG #170 FIX: Also merge constants from the source module's global impl blocks
        // This is critical for const generic resolution: FpAdd<IEEE754_32> needs IEEE754_32
        // to be available when fp.sk is parsed, not just when the user's module is parsed.
        // Always merge all GLOBAL_IMPL constants — imported entities may reference them
        // (e.g., CordicRotate16 uses CORDIC_GAIN_INV_16 from cordic.sk's GLOBAL_IMPL).
        let is_glob = matches!(import.path, HirImportPath::Glob { .. });

        for impl_block in &source.implementations {
            if impl_block.entity == crate::hir::EntityId::GLOBAL_IMPL {
                for constant in &impl_block.constants {
                    // Always merge GLOBAL_IMPL constants — imported entities may reference them.
                    // E.g., fp.sk imports CordicSqrt from cordic.sk (Simple import), and
                    // CordicRotate16's impl uses CORDIC_GAIN_INV_16 from cordic.sk's GLOBAL_IMPL.
                    // Constants are deduplicated by name below, so this is safe.
                    {
                        // Ensure target has a global impl block
                        if target.implementations.is_empty()
                            || !target
                                .implementations
                                .iter()
                                .any(|i| i.entity == crate::hir::EntityId::GLOBAL_IMPL)
                        {
                            target.implementations.push(crate::hir::HirImplementation {
                                entity: crate::hir::EntityId::GLOBAL_IMPL,
                                signals: Vec::new(),
                                variables: Vec::new(),
                                constants: Vec::new(),
                                functions: Vec::new(),
                                event_blocks: Vec::new(),
                                assignments: Vec::new(),
                                instances: Vec::new(),
                                covergroups: Vec::new(),
                                formal_blocks: Vec::new(),
                                statements: Vec::new(),
                            });
                        }

                        // Find the global impl block and add the constant if not already present
                        if let Some(global_impl) = target
                            .implementations
                            .iter_mut()
                            .find(|i| i.entity == crate::hir::EntityId::GLOBAL_IMPL)
                        {
                            if !global_impl
                                .constants
                                .iter()
                                .any(|c| c.name == constant.name)
                            {
                                global_impl.constants.push(constant.clone());
                            }
                        }
                    }
                }
            }
        }

        // BUG #207 FIX: Merge distinct types from source into target
        // This is critical for re-exporting types like fp32 from fp.sk
        // When a module does `use skalp::numeric::formats::fp32`, the distinct type
        // needs to be added to the module's HIR so it can be re-exported.
        for distinct in &source.distinct_types {
            // For glob imports, include all public distinct types
            // For specific imports, only include if the type name is in the import list
            if is_glob && distinct.visibility == crate::hir::HirVisibility::Public
                || symbol_names.contains(&distinct.name)
            {
                // Check if we already have this distinct type
                if !target
                    .distinct_types
                    .iter()
                    .any(|d| d.name == distinct.name)
                {
                    // For pub use re-exports, preserve the public visibility
                    // For private imports, the type becomes private in the target module
                    let mut imported_distinct = distinct.clone();
                    if import.visibility == crate::hir::HirVisibility::Public {
                        imported_distinct.visibility = crate::hir::HirVisibility::Public;
                    }
                    target.distinct_types.push(imported_distinct);
                }
            }
        }
    }

    /// BUG #207 FIX: Extract entity names referenced in statements (via StructLiteral)
    /// This is used to find entities that trait impl methods depend on
    fn extract_entity_refs_from_statements(stmts: &[crate::hir::HirStatement]) -> Vec<String> {
        let mut entity_names = Vec::new();
        for stmt in stmts {
            Self::extract_entity_refs_from_stmt(stmt, &mut entity_names);
        }
        entity_names
    }

    fn extract_entity_refs_from_stmt(stmt: &crate::hir::HirStatement, names: &mut Vec<String>) {
        match stmt {
            crate::hir::HirStatement::Let(let_stmt) => {
                Self::extract_entity_refs_from_expr(&let_stmt.value, names);
            }
            crate::hir::HirStatement::Assignment(assign) => {
                Self::extract_entity_refs_from_expr(&assign.rhs, names);
            }
            crate::hir::HirStatement::If(if_stmt) => {
                Self::extract_entity_refs_from_expr(&if_stmt.condition, names);
                for then_stmt in &if_stmt.then_statements {
                    Self::extract_entity_refs_from_stmt(then_stmt, names);
                }
                if let Some(else_stmts) = &if_stmt.else_statements {
                    for else_stmt in else_stmts {
                        Self::extract_entity_refs_from_stmt(else_stmt, names);
                    }
                }
            }
            crate::hir::HirStatement::Match(match_stmt) => {
                Self::extract_entity_refs_from_expr(&match_stmt.expr, names);
                for arm in &match_stmt.arms {
                    for arm_stmt in &arm.statements {
                        Self::extract_entity_refs_from_stmt(arm_stmt, names);
                    }
                }
            }
            crate::hir::HirStatement::Block(stmts) => {
                for sub_stmt in stmts {
                    Self::extract_entity_refs_from_stmt(sub_stmt, names);
                }
            }
            crate::hir::HirStatement::Return(Some(expr)) => {
                Self::extract_entity_refs_from_expr(expr, names);
            }
            crate::hir::HirStatement::Return(None) => {}
            _ => {}
        }
    }

    fn extract_entity_refs_from_expr(expr: &crate::hir::HirExpression, names: &mut Vec<String>) {
        match expr {
            crate::hir::HirExpression::StructLiteral(struct_lit) => {
                // This might be an entity instantiation
                if !names.contains(&struct_lit.type_name) {
                    names.push(struct_lit.type_name.clone());
                }
                // Also check field values
                for field in &struct_lit.fields {
                    Self::extract_entity_refs_from_expr(&field.value, names);
                }
            }
            crate::hir::HirExpression::Binary(bin) => {
                Self::extract_entity_refs_from_expr(&bin.left, names);
                Self::extract_entity_refs_from_expr(&bin.right, names);
            }
            crate::hir::HirExpression::Unary(unary) => {
                Self::extract_entity_refs_from_expr(&unary.operand, names);
            }
            crate::hir::HirExpression::Call(call) => {
                for arg in &call.args {
                    Self::extract_entity_refs_from_expr(arg, names);
                }
            }
            crate::hir::HirExpression::Index(base, index) => {
                Self::extract_entity_refs_from_expr(base, names);
                Self::extract_entity_refs_from_expr(index, names);
            }
            crate::hir::HirExpression::Range(base, start, end) => {
                Self::extract_entity_refs_from_expr(base, names);
                Self::extract_entity_refs_from_expr(start, names);
                Self::extract_entity_refs_from_expr(end, names);
            }
            crate::hir::HirExpression::Cast(cast) => {
                Self::extract_entity_refs_from_expr(&cast.expr, names);
            }
            crate::hir::HirExpression::Concat(parts) => {
                for part in parts {
                    Self::extract_entity_refs_from_expr(part, names);
                }
            }
            crate::hir::HirExpression::Ternary {
                condition,
                true_expr,
                false_expr,
            } => {
                Self::extract_entity_refs_from_expr(condition, names);
                Self::extract_entity_refs_from_expr(true_expr, names);
                Self::extract_entity_refs_from_expr(false_expr, names);
            }
            crate::hir::HirExpression::FieldAccess { base, .. } => {
                Self::extract_entity_refs_from_expr(base, names);
            }
            _ => {}
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::TempDir;

    #[test]
    fn test_module_resolver_basic() {
        let temp_dir = TempDir::new().unwrap();
        let root = temp_dir.path().to_path_buf();

        // Create a simple module
        let module_dir = root.join("test_module");
        fs::create_dir(&module_dir).unwrap();
        fs::write(
            module_dir.join("lib.sk"),
            "entity TestEntity { in clk: clock }",
        )
        .unwrap();

        let resolver = ModuleResolver::new(root);
        assert_eq!(resolver.search_paths.len(), 1);
    }
}
