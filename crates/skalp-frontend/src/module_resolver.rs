//! Module Resolution System
//!
//! Handles finding and loading SKALP modules based on import paths.
//! Resolves `use` statements to actual file paths and loads dependencies.
//!
//! Supports both `.sk` source files and `.skh` header files for compiled IP.
//! When resolving imports, `.sk` files take precedence over `.skh` files.

use anyhow::{bail, Context, Result};
use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};

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
    loaded_modules: HashMap<PathBuf, Hir>,

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
                eprintln!(
                    "[MODULE_RESOLVER] Found sibling lib directory: {:?}",
                    lib_dir
                );
                search_paths.push(lib_dir);
            }
        }

        // Try to read skalp.toml to get src_dirs configuration
        let manifest_path = root_dir.join("skalp.toml");
        if manifest_path.exists() {
            eprintln!("[MODULE_RESOLVER] Found skalp.toml at: {:?}", manifest_path);
            if let Ok(contents) = std::fs::read_to_string(&manifest_path) {
                if let Ok(manifest) = toml::from_str::<serde_json::Value>(&contents) {
                    // Try to extract src_dirs from [build] section
                    if let Some(build) = manifest.get("build") {
                        if let Some(src_dirs_value) = build.get("src_dirs") {
                            if let Some(src_dirs) = src_dirs_value.as_array() {
                                eprintln!(
                                    "[MODULE_RESOLVER] Found {} src_dirs in skalp.toml",
                                    src_dirs.len()
                                );
                                for dir in src_dirs {
                                    if let Some(dir_str) = dir.as_str() {
                                        let dir_path = root_dir.join(dir_str);
                                        if dir_path.exists() {
                                            eprintln!(
                                                "[MODULE_RESOLVER] Adding src_dir: {:?}",
                                                dir_path
                                            );
                                            search_paths.push(dir_path);
                                        } else {
                                            eprintln!(
                                                "[MODULE_RESOLVER] Warning: src_dir does not exist: {:?}",
                                                dir_path
                                            );
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        // Add standard library path if it exists
        if let Ok(stdlib_path) = std::env::var("SKALP_STDLIB_PATH") {
            eprintln!(
                "[MODULE_RESOLVER] Using SKALP_STDLIB_PATH from environment: {:?}",
                stdlib_path
            );
            search_paths.push(PathBuf::from(stdlib_path));
        } else {
            // Default stdlib location relative to root
            let default_stdlib = root_dir.join("stdlib");
            if default_stdlib.exists() {
                eprintln!(
                    "[MODULE_RESOLVER] Found stdlib relative to root: {:?}",
                    default_stdlib
                );
                search_paths.push(default_stdlib);
            }

            // Search upward through parent directories for crates/skalp-stdlib
            // This handles projects at different nesting levels (e.g., Karythra vs SKALP own tests)
            // First, convert root_dir to absolute path
            let abs_root = root_dir
                .canonicalize()
                .unwrap_or_else(|_| std::env::current_dir().unwrap().join(&root_dir));

            eprintln!(
                "[MODULE_RESOLVER] Searching for stdlib starting from: {:?}",
                abs_root
            );

            let mut current = Some(abs_root.as_path());
            let mut found_stdlib = false;

            while let Some(dir) = current {
                // Check current/crates/skalp-stdlib
                let potential_stdlib = dir.join("crates/skalp-stdlib");
                eprintln!(
                    "[MODULE_RESOLVER] Checking for stdlib at: {:?}",
                    potential_stdlib
                );
                if potential_stdlib.exists() && potential_stdlib.is_dir() {
                    eprintln!(
                        "[MODULE_RESOLVER] ✅ Found development stdlib: {:?}",
                        potential_stdlib
                    );
                    search_paths.push(potential_stdlib);
                    found_stdlib = true;
                    break;
                }

                // Also check sibling 'hls' directory: ../hls/crates/skalp-stdlib
                // This handles the case where we have /src/hw/karythra and /src/hw/hls as siblings
                if let Some(parent) = dir.parent() {
                    let hls_stdlib = parent.join("hls/crates/skalp-stdlib");
                    eprintln!(
                        "[MODULE_RESOLVER] Checking for stdlib in sibling hls: {:?}",
                        hls_stdlib
                    );
                    if hls_stdlib.exists() && hls_stdlib.is_dir() {
                        eprintln!(
                            "[MODULE_RESOLVER] ✅ Found stdlib in sibling hls directory: {:?}",
                            hls_stdlib
                        );
                        search_paths.push(hls_stdlib);
                        found_stdlib = true;
                        break;
                    }
                }

                current = dir.parent();
            }

            if !found_stdlib {
                eprintln!("[MODULE_RESOLVER] Warning: Could not find SKALP stdlib. Imports like 'use bitops::*' will fail.");
                eprintln!("[MODULE_RESOLVER] Hint: Set SKALP_STDLIB_PATH environment variable or ensure crates/skalp-stdlib exists in a parent directory.");
            }
        }

        eprintln!(
            "[MODULE_RESOLVER] Initialized with {} search paths:",
            search_paths.len()
        );
        for (i, path) in search_paths.iter().enumerate() {
            eprintln!("[MODULE_RESOLVER]   {}: {:?}", i + 1, path);
        }

        Self {
            search_paths,
            loaded_modules: HashMap::new(),
            loading: HashSet::new(),
            root_dir,
        }
    }

    /// Add a search path for modules
    pub fn add_search_path(&mut self, path: PathBuf) {
        self.search_paths.push(path);
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
            eprintln!("[MODULE_RESOLVER] Resolving crate:: import");
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
        eprintln!("[MODULE_RESOLVER] Loading module: {:?}", path);

        // Check if already loaded
        if self.loaded_modules.contains_key(path) {
            eprintln!(
                "[MODULE_RESOLVER] Module already loaded (cached): {:?}",
                path
            );
            return Ok(());
        }

        // Check for circular dependency
        if self.loading.contains(path) {
            eprintln!("[MODULE_RESOLVER] Circular dependency detected: {:?}", path);
            bail!("Circular dependency detected: {:?}", path);
        }

        // Mark as loading
        eprintln!("[MODULE_RESOLVER] Marking as loading: {:?}", path);
        self.loading.insert(path.to_path_buf());

        // Read and parse the file
        eprintln!("[MODULE_RESOLVER] Reading file: {:?}", path);
        let source = std::fs::read_to_string(path)
            .with_context(|| format!("Failed to read module file: {:?}", path))?;

        // Parse to syntax tree
        eprintln!(
            "[MODULE_RESOLVER] Parsing {} bytes from {:?}",
            source.len(),
            path
        );
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
        eprintln!("[MODULE_RESOLVER] Building HIR for {:?}", path);
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

        eprintln!(
            "[MODULE_RESOLVER] HIR built successfully for {:?}, found {} imports",
            path,
            hir.imports.len()
        );

        // Recursively load dependencies first
        let mut dep_paths = Vec::new();
        for (i, import) in hir.imports.iter().enumerate() {
            eprintln!(
                "[MODULE_RESOLVER] Processing import {}/{} from {:?}",
                i + 1,
                hir.imports.len(),
                path
            );
            let dep_path = self.resolve_import_path(import)?;
            eprintln!("[MODULE_RESOLVER] Resolved import to: {:?}", dep_path);
            // Recursively load (this will use cache if already loaded)
            self.load_module(&dep_path)?;
            eprintln!(
                "[MODULE_RESOLVER] Successfully loaded dependency: {:?}",
                dep_path
            );
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

        // BUG #170 FIX: Also merge constants from the source module's global impl blocks
        // This is critical for const generic resolution: FpAdd<IEEE754_32> needs IEEE754_32
        // to be available when fp.sk is parsed, not just when the user's module is parsed.
        // For glob imports, merge ALL global constants. For specific imports, merge by name.
        let is_glob = matches!(import.path, HirImportPath::Glob { .. });

        for impl_block in &source.implementations {
            if impl_block.entity == crate::hir::EntityId(0) {
                for constant in &impl_block.constants {
                    // For glob imports, include all constants
                    // For specific imports, only include if the constant name is in the import list
                    if is_glob || symbol_names.contains(&constant.name) {
                        // Ensure target has a global impl block
                        if target.implementations.is_empty()
                            || !target
                                .implementations
                                .iter()
                                .any(|i| i.entity == crate::hir::EntityId(0))
                        {
                            target.implementations.push(crate::hir::HirImplementation {
                                entity: crate::hir::EntityId(0),
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
                            .find(|i| i.entity == crate::hir::EntityId(0))
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
