//! Module Resolution System
//!
//! Handles finding and loading SKALP modules based on import paths.
//! Resolves `use` statements to actual file paths and loads dependencies.

use anyhow::{bail, Context, Result};
use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};

use crate::hir::{Hir, HirImport, HirImportPath};
use crate::hir_builder::HirBuilderContext;
use crate::parse;

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
            search_paths.push(PathBuf::from(stdlib_path));
        } else {
            // Default stdlib location relative to root
            let default_stdlib = root_dir.join("stdlib");
            if default_stdlib.exists() {
                search_paths.push(default_stdlib);
            }

            // Try crates/skalp-stdlib for development
            let dev_stdlib = root_dir
                .parent()
                .and_then(|p| p.parent())
                .map(|p| p.join("crates/skalp-stdlib"));
            if let Some(dev_path) = dev_stdlib {
                if dev_path.exists() {
                    search_paths.push(dev_path);
                }
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

        // Try to resolve the path
        // The last segment might be a symbol name or a module name
        // We try both: foo/bar/baz.sk and foo/bar.sk (where baz is a symbol)

        for search_path in &self.search_paths {
            // Try full path as module
            let mut full_path = search_path.clone();
            for segment in segments {
                full_path.push(segment);
            }
            full_path.set_extension("sk");

            if full_path.exists() {
                return Ok(full_path);
            }

            // Try without last segment (last segment is symbol name)
            if segments.len() > 1 {
                let mut partial_path = search_path.clone();
                for segment in &segments[..segments.len() - 1] {
                    partial_path.push(segment);
                }
                partial_path.set_extension("sk");

                if partial_path.exists() {
                    return Ok(partial_path);
                }
            }

            // Try with lib.sk (module directory with lib.sk file)
            let mut lib_path = search_path.clone();
            for segment in segments {
                lib_path.push(segment);
            }
            lib_path.push("lib.sk");

            if lib_path.exists() {
                return Ok(lib_path);
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
        let hir = builder.build(&syntax_tree).map_err(|errors| {
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

        // Recursively load dependencies
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
        }

        // Mark as done loading
        self.loading.remove(path);

        // Cache the module
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
