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

    /// When true, allow loading modules with parse errors (for stdlib components
    /// where some secondary entities may use syntax the parser doesn't yet support)
    tolerant_parse: bool,

    /// For each file of the design, the design-wide names of the entities it
    /// names differently (see [`ModuleResolver::plan_entity_names`])
    entity_names: IndexMap<PathBuf, IndexMap<String, String>>,
}

/// What one file of a design defines and imports, for naming its entities
struct FileEntities {
    /// Entities defined in the file
    defined: Vec<String>,
    /// Imports: the file imported from and what it takes from it
    imports: Vec<(PathBuf, ImportedNames)>,
}

/// The names one `use` takes from a file
enum ImportedNames {
    /// Each name in the file, under a local name (the same name unless renamed)
    Named(Vec<(String, String)>),
    /// Every name in the file
    Glob,
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

        // Resolve standard library path.
        // Priority order:
        //   1. SKALP_STDLIB_PATH env var (colon-separated, explicit)
        //   2. <binary_dir>/../share/skalp/stdlib (cargo install / packaged)
        //   3. ~/.skalp/stdlib (user-local)
        //   4. <project_root>/stdlib (project-local override)
        let mut found_stdlib = false;

        if let Ok(stdlib_paths) = std::env::var("SKALP_STDLIB_PATH") {
            for path_str in stdlib_paths.split(':') {
                if !path_str.is_empty() {
                    let p = PathBuf::from(path_str);
                    if p.exists() && p.is_dir() {
                        trace!("[MODULE_RESOLVER] stdlib from SKALP_STDLIB_PATH: {:?}", p);
                        search_paths.push(p);
                        found_stdlib = true;
                    }
                }
            }
        }

        if !found_stdlib {
            // Relative to binary: <binary_dir>/../share/skalp/stdlib
            if let Ok(exe) = std::env::current_exe() {
                if let Some(bin_dir) = exe.parent() {
                    let share_stdlib = bin_dir.join("../share/skalp/stdlib");
                    if share_stdlib.exists() && share_stdlib.is_dir() {
                        if let Ok(canonical) = share_stdlib.canonicalize() {
                            trace!(
                                "[MODULE_RESOLVER] stdlib from binary share dir: {:?}",
                                canonical
                            );
                            search_paths.push(canonical);
                            found_stdlib = true;
                        }
                    }
                }
            }
        }

        if !found_stdlib {
            // User-local: ~/.skalp/stdlib
            if let Ok(home) = std::env::var("HOME") {
                let home_stdlib = PathBuf::from(home).join(".skalp/stdlib");
                if home_stdlib.exists() && home_stdlib.is_dir() {
                    trace!(
                        "[MODULE_RESOLVER] stdlib from ~/.skalp/stdlib: {:?}",
                        home_stdlib
                    );
                    search_paths.push(home_stdlib);
                    found_stdlib = true;
                }
            }
        }

        if !found_stdlib {
            // Project-local: <root>/stdlib
            let project_stdlib = root_dir.join("stdlib");
            if project_stdlib.exists() && project_stdlib.is_dir() {
                trace!(
                    "[MODULE_RESOLVER] stdlib from project dir: {:?}",
                    project_stdlib
                );
                search_paths.push(project_stdlib);
                found_stdlib = true;
            }
        }

        if !found_stdlib {
            trace!("[MODULE_RESOLVER] Warning: Could not find skalp stdlib.");
            trace!("[MODULE_RESOLVER] Set SKALP_STDLIB_PATH or install stdlib to ~/.skalp/stdlib");
        }

        Self {
            search_paths,
            loaded_modules: IndexMap::new(),
            loading: HashSet::new(),
            root_dir,
            tolerant_parse: false,
            entity_names: IndexMap::new(),
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

            // Check for skalp/arithmetic/ops.sk (contains impl Add/Sub/Mul for bit)
            // These route arithmetic operators through stdlib entities with barrier
            // annotations for NCL/sync context-agnostic behavior.
            let arith_path = search_path.join("skalp/arithmetic/ops.sk");
            if arith_path.exists() {
                trace!("Loading arithmetic ops from: {:?}", arith_path);
                match self.load_module(&arith_path) {
                    Ok(_) => trace!("Loaded ops.sk successfully"),
                    Err(e) => debug!("Failed to load ops.sk: {:?}", e),
                }
            }

            // Load stdlib component entities needed by arithmetic trait impls.
            for component in &["adder", "multiplier"] {
                let comp_path = search_path.join(format!("components/{}.sk", component));
                if comp_path.exists() {
                    match self.load_module(&comp_path) {
                        Ok(_) => trace!("Loaded {}.sk successfully", component),
                        Err(e) => debug!("Failed to load {}.sk: {:?}", component, e),
                    }
                }
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

    /// Give every entity of the design rooted at `main_path` a name no other
    /// entity of the design has.
    ///
    /// Two files may each define an entity of the same name (a module and a
    /// trimmed copy of it, say). Merged into one design, the two must stay
    /// two entities: each file that names the entity means its own one, and
    /// each importer means the one it imported. So an entity whose name is
    /// defined in more than one file keeps that name only in the main file;
    /// elsewhere it is named after its file as well (`Unit` of `fast.sk`
    /// becomes `fast_Unit`). Each file's builder then maps the name the
    /// file uses (in definitions, implementations, instances and imports) to
    /// the design-wide one. Designs without such names are unchanged.
    ///
    /// A file that sees two different entities under one name, from two
    /// imports of the same kind (two named imports, or two globs), is an
    /// error: nothing says which one it means. A name the file defines comes
    /// before an imported one, and a named import before a glob.
    pub fn plan_entity_names(
        &mut self,
        main_path: &Path,
        main_tree: &crate::syntax::SyntaxNode,
    ) -> Result<()> {
        self.entity_names.clear();

        // Every file reachable from the main file, with what it defines and
        // imports. Files that cannot be read or parsed are left out here;
        // loading them reports the problem.
        let mut files: IndexMap<PathBuf, FileEntities> = IndexMap::new();
        let mut pending = vec![(main_path.to_path_buf(), Some(main_tree.clone()))];
        while let Some((path, tree)) = pending.pop() {
            if files.contains_key(&path) {
                continue;
            }
            let tree = match tree {
                Some(tree) => tree,
                None => {
                    let Ok(source) = std::fs::read_to_string(&path) else {
                        continue;
                    };
                    parse::parse_with_errors(&source).0
                }
            };
            let (defined, hir_imports) = crate::hir_builder::scan_entities_and_imports(&tree);
            let mut imports = Vec::new();
            for import in &hir_imports {
                let Ok(dep_path) = self.resolve_import_path(import) else {
                    continue;
                };
                let names = match &import.path {
                    HirImportPath::Glob { .. } => ImportedNames::Glob,
                    HirImportPath::Simple { segments } => ImportedNames::Named(
                        segments
                            .last()
                            .map(|n| (n.clone(), n.clone()))
                            .into_iter()
                            .collect(),
                    ),
                    HirImportPath::Renamed { segments, alias } => ImportedNames::Named(
                        segments
                            .last()
                            .map(|n| (n.clone(), alias.clone()))
                            .into_iter()
                            .collect(),
                    ),
                    HirImportPath::Nested { paths, .. } => ImportedNames::Named(
                        paths
                            .iter()
                            .filter_map(|p| match p {
                                HirImportPath::Simple { segments } => {
                                    segments.last().map(|n| (n.clone(), n.clone()))
                                }
                                HirImportPath::Renamed { segments, alias } => {
                                    segments.last().map(|n| (n.clone(), alias.clone()))
                                }
                                _ => None,
                            })
                            .collect(),
                    ),
                };
                pending.push((dep_path.clone(), None));
                imports.push((dep_path, names));
            }
            files.insert(path, FileEntities { defined, imports });
        }

        // Names defined in more than one file
        let mut defining: IndexMap<&str, Vec<&PathBuf>> = IndexMap::new();
        for (path, file) in &files {
            for name in &file.defined {
                let paths = defining.entry(name.as_str()).or_default();
                if !paths.contains(&path) {
                    paths.push(path);
                }
            }
        }
        let shared: Vec<&str> = defining
            .iter()
            .filter(|(_, paths)| paths.len() > 1)
            .map(|(name, _)| *name)
            .collect();
        if shared.is_empty() {
            return Ok(());
        }

        // Design-wide names of the definitions outside the main file
        let mut taken: HashSet<String> = defining.keys().map(|n| n.to_string()).collect();
        let mut design_name: IndexMap<(PathBuf, String), String> = IndexMap::new();
        for name in &shared {
            for path in &defining[name] {
                if path.as_path() == main_path {
                    continue;
                }
                let global = self.file_qualified_name(path, name, &taken);
                taken.insert(global.clone());
                design_name.insert(((*path).clone(), name.to_string()), global);
            }
        }

        // Each file's own names for those entities
        let shared: HashSet<&str> = shared.into_iter().collect();
        let mut names: IndexMap<PathBuf, IndexMap<String, String>> = IndexMap::new();
        for (path, file) in &files {
            let mut local: Vec<String> = file
                .defined
                .iter()
                .filter(|n| shared.contains(n.as_str()))
                .cloned()
                .collect();
            for (_, imported) in &file.imports {
                match imported {
                    ImportedNames::Named(pairs) => local.extend(
                        pairs
                            .iter()
                            .filter(|(name, _)| shared.contains(name.as_str()))
                            .map(|(_, local)| local.clone()),
                    ),
                    ImportedNames::Glob => local.extend(shared.iter().map(|n| n.to_string())),
                }
            }
            let mut file_names = IndexMap::new();
            for name in local {
                if file_names.contains_key(&name) {
                    continue;
                }
                let target =
                    Self::entity_seen_as(&files, &design_name, path, &name, &mut Vec::new())?;
                if let Some(global) = target.filter(|global| *global != name) {
                    file_names.insert(name, global);
                }
            }
            if !file_names.is_empty() {
                names.insert(path.clone(), file_names);
            }
        }
        self.entity_names = names;
        Ok(())
    }

    /// The design-wide name of the entity `file` knows as `name`: its own
    /// definition, else the one its imports bring in. `None` when it sees no
    /// entity of that name.
    fn entity_seen_as(
        files: &IndexMap<PathBuf, FileEntities>,
        design_name: &IndexMap<(PathBuf, String), String>,
        file: &Path,
        name: &str,
        visiting: &mut Vec<PathBuf>,
    ) -> Result<Option<String>> {
        let Some(entities) = files.get(file) else {
            return Ok(None);
        };
        if entities.defined.iter().any(|n| n == name) {
            return Ok(Some(
                design_name
                    .get(&(file.to_path_buf(), name.to_string()))
                    .cloned()
                    .unwrap_or_else(|| name.to_string()),
            ));
        }
        if visiting.iter().any(|p| p == file) {
            return Ok(None);
        }
        visiting.push(file.to_path_buf());
        // Names imported by name come before names a glob brings in, as in
        // Rust: `use a::*; use b::Unit;` means b's `Unit`
        let mut found: Option<(String, &Path)> = None;
        for globs in [false, true] {
            for (dep, imported) in &entities.imports {
                let seen = match imported {
                    ImportedNames::Named(pairs) if !globs => {
                        match pairs.iter().find(|(_, local)| local == name) {
                            Some((original, _)) => {
                                Self::entity_seen_as(files, design_name, dep, original, visiting)?
                            }
                            None => None,
                        }
                    }
                    ImportedNames::Glob if globs => {
                        Self::entity_seen_as(files, design_name, dep, name, visiting)?
                    }
                    _ => None,
                };
                if let Some(seen) = seen {
                    match &found {
                        Some((earlier, earlier_dep)) if *earlier != seen => {
                            visiting.pop();
                            bail!(
                                "`{}` in {} is ambiguous: it is imported from both {} and {}, \
                                 which define different entities of that name. Import it from \
                                 only one of them",
                                name,
                                file.display(),
                                earlier_dep.display(),
                                dep.display()
                            );
                        }
                        Some(_) => {}
                        None => found = Some((seen, dep)),
                    }
                }
            }
            if found.is_some() {
                break;
            }
        }
        visiting.pop();
        Ok(found.map(|(seen, _)| seen))
    }

    /// `name` qualified by the file defining it, unlike any name in `taken`:
    /// the file's stem first, then as many parent directories as it takes
    fn file_qualified_name(&self, path: &Path, name: &str, taken: &HashSet<String>) -> String {
        let relative = path
            .strip_prefix(&self.root_dir)
            .unwrap_or(path)
            .with_extension("");
        let parts: Vec<String> = relative
            .components()
            .filter_map(|c| match c {
                std::path::Component::Normal(part) => Some(
                    part.to_string_lossy()
                        .chars()
                        .map(|c| if c.is_ascii_alphanumeric() { c } else { '_' })
                        .collect(),
                ),
                _ => None,
            })
            .collect();
        for depth in 1..=parts.len().max(1) {
            let prefix = parts[parts.len().saturating_sub(depth)..].join("_");
            let candidate = if prefix.is_empty() {
                name.to_string()
            } else {
                format!("{prefix}_{name}")
            };
            if !taken.contains(&candidate) {
                return candidate;
            }
        }
        let base = format!("{}_{}", parts.join("_"), name);
        (2..)
            .map(|n| format!("{base}_{n}"))
            .find(|candidate| !taken.contains(candidate))
            .expect("an unused name")
    }

    /// The design-wide names of the entities `path` names differently, for
    /// its builder (empty for most files)
    pub fn entity_names_for(&self, path: &Path) -> IndexMap<String, String> {
        self.entity_names.get(path).cloned().unwrap_or_default()
    }

    /// The design-wide name of the entity `module` exports as `name`, when
    /// it differs from `name`
    pub fn design_entity_name(&self, module: &Path, name: &str) -> Option<&str> {
        self.entity_names
            .get(module)
            .and_then(|names| names.get(name))
            .map(String::as_str)
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
            // For stdlib component files, allow partial parsing — the entities that
            // parsed successfully are still usable even if secondary entities have
            // syntax the parser doesn't yet support (e.g., $signed, Array<>).
            if self.tolerant_parse {
                trace!(
                    "[MODULE_RESOLVER] Tolerant mode: ignoring {} parse errors in {:?}",
                    parse_errors.len(),
                    path
                );
            } else {
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
        }

        // Build HIR
        trace!("[MODULE_RESOLVER] Building HIR for {:?}", path);
        let mut builder = HirBuilderContext::new();
        builder.set_entity_names(self.entity_names_for(path));
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
                    .is_some_and(|dep_hir| !dep_hir.type_aliases.is_empty())
            });

            if has_dep_type_aliases {
                let mut rebuild_builder = HirBuilderContext::new();
                rebuild_builder.set_entity_names(self.entity_names_for(path));

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

        // Instances of imported entities could not be built above: the
        // entities were not known yet. Merge the imported entities (with the
        // entities they instantiate) and rebuild this file's own entities with
        // them preregistered, so the module's implementations hold every
        // instance. Without this, a file that imports an entity its importer
        // does not also import lost that instance: the entity never reached
        // the design, and the instance's outputs were left undriven.
        if !hir.unresolved_instances.is_empty() {
            hir = self.rebuild_with_imported_entities(hir, path, &syntax_tree, &dep_paths)?;
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

    /// Rebuild a module whose first build left instances unresolved, with the
    /// entities it imports merged in. The imported entities keep the ids the
    /// merge gave them; the file's own entities and implementations are
    /// replaced by the rebuilt ones, which now include those instances.
    fn rebuild_with_imported_entities(
        &self,
        hir: Hir,
        path: &Path,
        syntax_tree: &crate::syntax::SyntaxNode,
        dep_paths: &[(HirImport, PathBuf)],
    ) -> Result<Hir> {
        let mut merged = hir;
        for (import, dep_path) in dep_paths {
            let Some(dep_hir) = self.loaded_modules.get(dep_path) else {
                continue;
            };
            let names: Vec<&String> = match &import.path {
                HirImportPath::Simple { segments } | HirImportPath::Renamed { segments, .. } => {
                    segments.last().into_iter().collect()
                }
                HirImportPath::Nested { paths, .. } => paths
                    .iter()
                    .filter_map(|p| match p {
                        HirImportPath::Simple { segments }
                        | HirImportPath::Renamed { segments, .. } => segments.last(),
                        _ => None,
                    })
                    .collect(),
                HirImportPath::Glob { .. } => dep_hir.entities.iter().map(|e| &e.name).collect(),
            };
            for name in names {
                let name = self.design_entity_name(dep_path, name).unwrap_or(name);
                if dep_hir.entities.iter().any(|e| e.name == name) {
                    crate::merge_symbol(&mut merged, dep_hir, name)?;
                }
            }
        }

        let mut builder = HirBuilderContext::new();
        builder.set_entity_names(self.entity_names_for(path));
        for type_alias in &merged.type_aliases {
            builder.preregister_type_alias(type_alias);
        }
        for distinct in &merged.distinct_types {
            builder.preregister_distinct_type(distinct);
        }
        for entity in &merged.entities {
            builder.preregister_entity(entity);
        }
        for function in &merged.functions {
            builder.preregister_function(function);
        }
        for implementation in &merged.implementations {
            for function in &implementation.functions {
                builder.preregister_function(function);
            }
            for constant in &implementation.constants {
                builder.preregister_constant(constant);
            }
        }
        let rebuilt = builder.build(syntax_tree).map_err(|errors| {
            anyhow::anyhow!(
                "Failed to rebuild HIR with imported entities: {}",
                errors
                    .first()
                    .map(|e| e.message.clone())
                    .unwrap_or_else(|| "unknown error".to_string())
            )
        })?;

        // The file's own entities are the ones the rebuild built: the merged
        // HIR also holds the entities its imports brought in (before this
        // rebuild too, for public ones), and those must survive it.
        let own_entities: HashSet<String> =
            rebuilt.entities.iter().map(|e| e.name.clone()).collect();

        // Preregistration kept every entity's id, so the rebuilt entities and
        // implementations replace their first-build versions one for one.
        let rebuilt_ids: HashSet<crate::hir::EntityId> = rebuilt
            .entities
            .iter()
            .filter(|e| own_entities.contains(&e.name))
            .map(|e| e.id)
            .collect();
        merged.entities.retain(|e| !own_entities.contains(&e.name));
        merged
            .implementations
            .retain(|i| !rebuilt_ids.contains(&i.entity));
        merged.entities.extend(
            rebuilt
                .entities
                .into_iter()
                .filter(|e| own_entities.contains(&e.name)),
        );
        merged.implementations.extend(
            rebuilt
                .implementations
                .into_iter()
                .filter(|i| rebuilt_ids.contains(&i.entity)),
        );
        merged.unresolved_instances = rebuilt.unresolved_instances;
        Ok(merged)
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
                                name: None,
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
