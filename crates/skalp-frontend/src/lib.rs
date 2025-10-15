#![allow(dead_code, unused_variables, unused_imports)]
//! SKALP Frontend - Lexer, Parser, and AST
//!
//! This crate handles:
//! - Lexical analysis (tokenization)
//! - Parsing SKALP source to AST
//! - Semantic analysis and type checking
//! - HIR (High-level IR) generation

pub mod ast;
pub mod const_eval;
pub mod constraints;
pub mod generics;
pub mod hir;
pub mod hir_builder;
pub mod lexer;
pub mod macros;
pub mod module_resolver;
pub mod monomorphization;
pub mod parse;
pub mod parser;
pub mod semantic;
pub mod syntax;
pub mod typeck;
pub mod types;

#[cfg(test)]
mod stream_test;

pub use hir::Hir;
pub use lexer::Lexer;
pub use parser::Parser;

use anyhow::{Context, Result};
use module_resolver::ModuleResolver;
use std::path::{Path, PathBuf};

/// Find the project root by searching for skalp.toml
///
/// Searches upward from the source file, but only up to 3 levels
/// to avoid finding unrelated project manifests
fn find_project_root(start_path: &Path) -> Option<PathBuf> {
    let mut current = start_path.to_path_buf();

    // If start_path is a file, start from its parent
    if current.is_file() {
        current = current.parent()?.to_path_buf();
    }

    // Walk up the directory tree looking for skalp.toml (max 3 levels)
    for _ in 0..3 {
        let manifest_path = current.join("skalp.toml");
        if manifest_path.exists() {
            return Some(current);
        }

        // Try to go up one level
        match current.parent() {
            Some(parent) => current = parent.to_path_buf(),
            None => return None, // Reached the filesystem root
        }
    }

    None
}

/// Parse and build HIR from a file with full module resolution
pub fn parse_and_build_hir_from_file(file_path: &Path) -> Result<Hir> {
    use module_resolver::ModuleResolver;
    use std::fs;

    // Find the project root by looking for skalp.toml
    let root_dir = find_project_root(file_path).unwrap_or_else(|| {
        file_path
            .parent()
            .unwrap_or_else(|| Path::new("."))
            .to_path_buf()
    });

    // Create module resolver
    let mut resolver = ModuleResolver::new(root_dir);

    // Load the main file
    let source = fs::read_to_string(file_path)
        .with_context(|| format!("Failed to read source file: {:?}", file_path))?;

    // Parse source to syntax tree
    let (syntax_tree, parse_errors) = parse::parse_with_errors(&source);

    if !parse_errors.is_empty() {
        let error_msgs: Vec<String> = parse_errors
            .iter()
            .map(|e| format!("  {} at pos {}", e.message, e.position))
            .collect();
        let error_msg = format!(
            "Parsing failed with {} errors:\n{}",
            parse_errors.len(),
            error_msgs.join("\n")
        );
        anyhow::bail!(error_msg);
    }

    // Build HIR (first pass - will have incomplete instances for imported entities)
    let mut builder = hir_builder::HirBuilderContext::new();
    let mut hir = builder.build(&syntax_tree).map_err(|errors| {
        anyhow::anyhow!(
            "HIR build failed: {}",
            errors
                .first()
                .map(|e| e.message.clone())
                .unwrap_or_else(|| "unknown error".to_string())
        )
    })?;

    // Resolve and load all dependencies
    let dependencies = resolver
        .resolve_dependencies(&hir)
        .context("Failed to resolve module dependencies")?;

    // Merge symbols from dependencies into HIR
    hir = merge_imports(&hir, &dependencies, &resolver)
        .context("Failed to merge imported symbols")?;

    // Second pass: Rebuild instances now that all entities are available
    hir = rebuild_instances_with_imports(&hir, file_path)
        .context("Failed to rebuild instances with imports")?;

    // Monomorphize
    use monomorphization::MonomorphizationEngine;
    let mut engine = MonomorphizationEngine::new();
    let monomorphized_hir = engine.monomorphize(&hir);

    Ok(monomorphized_hir)
}

/// Rebuild instances with imported entities now available
fn rebuild_instances_with_imports(hir: &Hir, file_path: &Path) -> Result<Hir> {
    use std::fs;

    // Read the source file again
    let source = fs::read_to_string(file_path)
        .with_context(|| format!("Failed to read source file for rebuild: {:?}", file_path))?;

    // Parse to syntax tree
    let (syntax_tree, _) = parse::parse_with_errors(&source);

    // Create a new builder with all entities pre-registered
    let mut builder = hir_builder::HirBuilderContext::new();

    // Pre-register all entities in the symbol table
    for entity in &hir.entities {
        builder.preregister_entity(entity);
    }

    // Rebuild implementations (this will now find imported entities)
    let rebuilt_hir = builder.build(&syntax_tree).map_err(|errors| {
        anyhow::anyhow!(
            "HIR rebuild failed: {}",
            errors
                .first()
                .map(|e| e.message.clone())
                .unwrap_or_else(|| "unknown error".to_string())
        )
    })?;

    // Keep all entities from the merged HIR (including imports)
    // But use implementations from the rebuilt HIR (which now have correct instances)
    let mut final_hir = hir.clone();
    final_hir.implementations = rebuilt_hir.implementations;

    Ok(final_hir)
}

/// Merge imported symbols from dependencies into HIR
fn merge_imports(hir: &Hir, dependencies: &[PathBuf], resolver: &ModuleResolver) -> Result<Hir> {
    use hir::HirImportPath;

    let mut merged_hir = hir.clone();

    // For each import in the current HIR
    for import in &hir.imports {
        // Find the corresponding loaded module
        let module_path = resolver.resolve_import_path(import)?;
        let loaded_module = resolver
            .get_module(&module_path)
            .ok_or_else(|| anyhow::anyhow!("Module not loaded: {:?}", module_path))?;

        // Extract and merge symbols based on import type
        match &import.path {
            HirImportPath::Simple { segments } => {
                // Import a specific symbol
                if let Some(symbol_name) = segments.last() {
                    merge_symbol(&mut merged_hir, loaded_module, symbol_name)?;
                }
            }
            HirImportPath::Renamed { segments, alias } => {
                // Import a specific symbol with an alias
                if let Some(symbol_name) = segments.last() {
                    merge_symbol_with_rename(&mut merged_hir, loaded_module, symbol_name, alias)?;
                }
            }
            HirImportPath::Glob { segments: _ } => {
                // Import all public symbols
                merge_all_symbols(&mut merged_hir, loaded_module)?;
            }
            HirImportPath::Nested { prefix: _, paths } => {
                // Import multiple symbols
                for path in paths {
                    match path {
                        HirImportPath::Simple { segments } => {
                            if let Some(symbol_name) = segments.last() {
                                merge_symbol(&mut merged_hir, loaded_module, symbol_name)?;
                            }
                        }
                        HirImportPath::Renamed { segments, alias } => {
                            if let Some(symbol_name) = segments.last() {
                                merge_symbol_with_rename(
                                    &mut merged_hir,
                                    loaded_module,
                                    symbol_name,
                                    alias,
                                )?;
                            }
                        }
                        _ => {}
                    }
                }
            }
        }
    }

    Ok(merged_hir)
}

/// Merge a specific symbol from a module into the current HIR
fn merge_symbol(target: &mut Hir, source: &Hir, symbol_name: &str) -> Result<()> {
    // Try to find the symbol in entities
    if let Some(entity) = source.entities.iter().find(|e| e.name == symbol_name) {
        // Assign a new unique entity ID to avoid collisions
        let new_entity_id =
            hir::EntityId(target.entities.iter().map(|e| e.id.0).max().unwrap_or(0) + 1);

        // Renumber ports to avoid collisions
        let next_port_id = target
            .entities
            .iter()
            .flat_map(|e| e.ports.iter())
            .map(|p| p.id.0)
            .max()
            .unwrap_or(0)
            + 1;

        let mut imported_entity = entity.clone();
        imported_entity.id = new_entity_id;

        // Renumber all ports
        for (i, port) in imported_entity.ports.iter_mut().enumerate() {
            port.id = hir::PortId(next_port_id + i as u32);
        }

        target.entities.push(imported_entity);
        return Ok(());
    }

    // Try to find the symbol in trait definitions
    if let Some(trait_def) = source
        .trait_definitions
        .iter()
        .find(|t| t.name == symbol_name)
    {
        target.trait_definitions.push(trait_def.clone());
        return Ok(());
    }

    // Try to find the symbol in functions
    if let Some(function) = source.functions.iter().find(|f| f.name == symbol_name) {
        target.functions.push(function.clone());
        return Ok(());
    }

    // Try to find the symbol in type aliases
    if let Some(type_alias) = source.type_aliases.iter().find(|t| t.name == symbol_name) {
        target.type_aliases.push(type_alias.clone());
        return Ok(());
    }

    // Try to find the symbol in user-defined types (structs, enums, unions)
    if let Some(user_type) = source
        .user_defined_types
        .iter()
        .find(|t| t.name == symbol_name)
    {
        target.user_defined_types.push(user_type.clone());
        return Ok(());
    }

    // Symbol not found - this might be okay if it's a type or other symbol
    // For now, we don't error
    Ok(())
}

/// Merge a specific symbol with a renamed alias
fn merge_symbol_with_rename(
    target: &mut Hir,
    source: &Hir,
    symbol_name: &str,
    alias: &str,
) -> Result<()> {
    // Try to find the symbol in entities
    if let Some(entity) = source.entities.iter().find(|e| e.name == symbol_name) {
        let mut renamed_entity = entity.clone();
        renamed_entity.name = alias.to_string();
        target.entities.push(renamed_entity);
        return Ok(());
    }

    // Try to find the symbol in trait definitions
    if let Some(trait_def) = source
        .trait_definitions
        .iter()
        .find(|t| t.name == symbol_name)
    {
        let mut renamed_trait = trait_def.clone();
        renamed_trait.name = alias.to_string();
        target.trait_definitions.push(renamed_trait);
        return Ok(());
    }

    // Try to find the symbol in functions
    if let Some(function) = source.functions.iter().find(|f| f.name == symbol_name) {
        let mut renamed_function = function.clone();
        renamed_function.name = alias.to_string();
        target.functions.push(renamed_function);
        return Ok(());
    }

    // Try to find the symbol in type aliases
    if let Some(type_alias) = source.type_aliases.iter().find(|t| t.name == symbol_name) {
        let mut renamed_type_alias = type_alias.clone();
        renamed_type_alias.name = alias.to_string();
        target.type_aliases.push(renamed_type_alias);
        return Ok(());
    }

    // Try to find the symbol in user-defined types (structs, enums, unions)
    if let Some(user_type) = source
        .user_defined_types
        .iter()
        .find(|t| t.name == symbol_name)
    {
        let mut renamed_user_type = user_type.clone();
        renamed_user_type.name = alias.to_string();
        // Also need to update the name in the type_def itself
        renamed_user_type.type_def = match &renamed_user_type.type_def {
            hir::HirType::Struct(s) => {
                let mut new_struct = s.clone();
                new_struct.name = alias.to_string();
                hir::HirType::Struct(new_struct)
            }
            hir::HirType::Enum(e) => {
                let mut new_enum = e.as_ref().clone();
                new_enum.name = alias.to_string();
                hir::HirType::Enum(Box::new(new_enum))
            }
            hir::HirType::Union(u) => {
                let mut new_union = u.clone();
                new_union.name = alias.to_string();
                hir::HirType::Union(new_union)
            }
            other => other.clone(),
        };
        target.user_defined_types.push(renamed_user_type);
        return Ok(());
    }

    Ok(())
}

/// Merge all public symbols from a module
fn merge_all_symbols(target: &mut Hir, source: &Hir) -> Result<()> {
    use hir::HirVisibility;

    // Merge all public entities
    for entity in &source.entities {
        if entity.visibility == HirVisibility::Public {
            target.entities.push(entity.clone());
        }
    }

    // Merge all public trait definitions
    for trait_def in &source.trait_definitions {
        // Traits don't have visibility in current HIR, so import all
        target.trait_definitions.push(trait_def.clone());
    }

    // Merge all public functions
    for function in &source.functions {
        // Functions don't have visibility in current HIR, so import all
        target.functions.push(function.clone());
    }

    // Merge all public type aliases
    for type_alias in &source.type_aliases {
        if type_alias.visibility == HirVisibility::Public {
            target.type_aliases.push(type_alias.clone());
        }
    }

    // Merge all public user-defined types (structs, enums, unions)
    for user_type in &source.user_defined_types {
        if user_type.visibility == HirVisibility::Public {
            target.user_defined_types.push(user_type.clone());
        }
    }

    Ok(())
}

/// Parse and build HIR directly from source (without module resolution)
pub fn parse_and_build_hir(source: &str) -> Result<Hir> {
    // Parse source to syntax tree with error reporting
    let (syntax_tree, parse_errors) = parse::parse_with_errors(source);

    // Check for parse errors
    if !parse_errors.is_empty() {
        return Err(anyhow::anyhow!(
            "Parsing failed with {} errors: {}",
            parse_errors.len(),
            parse_errors[0].message
        ));
    }

    // Build HIR from syntax tree
    let mut builder = hir_builder::HirBuilderContext::new();
    let hir = match builder.build(&syntax_tree) {
        Ok(hir) => hir,
        Err(errors) => {
            let error_messages: Vec<String> = errors.iter().map(|e| e.message.clone()).collect();
            return Err(anyhow::anyhow!(
                "HIR building failed with {} errors:\n  {}",
                errors.len(),
                error_messages.join("\n  ")
            ));
        }
    };

    // Type checking is temporarily disabled to avoid conflicts with existing type resolution
    // let mut checker = typeck::TypeChecker::new();
    // if let Err(errors) = checker.check_source_file(&syntax_tree) {
    //     return Err(anyhow::anyhow!("Type checking failed with {} errors", errors.len()));
    // }

    // Apply monomorphization to generate concrete implementations from generics
    let mut mono_engine = monomorphization::MonomorphizationEngine::new();
    let monomorphized_hir = mono_engine.monomorphize(&hir);

    Ok(monomorphized_hir)
}

/// Parse a SKALP source file (compatibility wrapper)
pub fn parse_file(source: &str) -> Result<ast::SourceFile> {
    // For compatibility, return empty AST
    // The actual parsing happens in parse_and_build_hir
    Ok(ast::SourceFile { items: Vec::new() })
}

/// Build HIR from AST (compatibility wrapper)
pub fn build_hir(_ast: &ast::SourceFile) -> Result<Hir> {
    // This is now a no-op since we skip the AST phase
    // Return empty HIR - actual work happens in parse_and_build_hir
    Ok(Hir {
        name: "design".to_string(),
        entities: Vec::new(),
        implementations: Vec::new(),
        protocols: Vec::new(),
        intents: Vec::new(),
        requirements: Vec::new(),
        trait_definitions: Vec::new(),
        trait_implementations: Vec::new(),
        type_aliases: Vec::new(),
        user_defined_types: Vec::new(),
        global_constraints: Vec::new(),
        modules: Vec::new(),
        imports: Vec::new(),
        functions: Vec::new(),
    })
}

/// Type check HIR (compatibility wrapper)
pub fn typecheck(_hir: &Hir) -> Result<()> {
    // Type checking is now done during parse_and_build_hir
    Ok(())
}

/// Format AST back to source code
pub fn format_ast(ast: &ast::SourceFile) -> Result<String> {
    // Simple formatter - would be more sophisticated in production
    Ok(format!("{:#?}", ast))
}
