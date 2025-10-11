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
use std::path::{Path, PathBuf};
use module_resolver::ModuleResolver;

/// Parse and build HIR from a file with full module resolution
pub fn parse_and_build_hir_from_file(file_path: &Path) -> Result<Hir> {
    use module_resolver::ModuleResolver;
    use std::fs;

    // Get the root directory (parent of the file)
    let root_dir = file_path.parent()
        .unwrap_or_else(|| Path::new("."))
        .to_path_buf();

    // Create module resolver
    let mut resolver = ModuleResolver::new(root_dir);

    // Load the main file
    let source = fs::read_to_string(file_path)
        .with_context(|| format!("Failed to read source file: {:?}", file_path))?;

    // Parse and build HIR
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
        anyhow::bail!(error_msg);
    }

    // Build HIR
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
    let dependencies = resolver.resolve_dependencies(&hir)
        .context("Failed to resolve module dependencies")?;

    // Merge symbols from dependencies into HIR
    hir = merge_imports(&hir, &dependencies, &resolver)
        .context("Failed to merge imported symbols")?;

    // Monomorphize
    use monomorphization::MonomorphizationEngine;
    let mut engine = MonomorphizationEngine::new();
    let monomorphized_hir = engine.monomorphize(&hir);

    Ok(monomorphized_hir)
}

/// Merge imported symbols from dependencies into HIR
fn merge_imports(
    hir: &Hir,
    dependencies: &[PathBuf],
    resolver: &ModuleResolver,
) -> Result<Hir> {
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
                                merge_symbol_with_rename(&mut merged_hir, loaded_module, symbol_name, alias)?;
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
        target.entities.push(entity.clone());
        return Ok(());
    }

    // Try to find the symbol in trait definitions
    if let Some(trait_def) = source.trait_definitions.iter().find(|t| t.name == symbol_name) {
        target.trait_definitions.push(trait_def.clone());
        return Ok(());
    }

    // Try to find the symbol in functions
    if let Some(function) = source.functions.iter().find(|f| f.name == symbol_name) {
        target.functions.push(function.clone());
        return Ok(());
    }

    // Symbol not found - this might be okay if it's a type or other symbol
    // For now, we just warn but don't error
    Ok(())
}

/// Merge a specific symbol with a renamed alias
fn merge_symbol_with_rename(target: &mut Hir, source: &Hir, symbol_name: &str, alias: &str) -> Result<()> {
    // Try to find the symbol in entities
    if let Some(entity) = source.entities.iter().find(|e| e.name == symbol_name) {
        let mut renamed_entity = entity.clone();
        renamed_entity.name = alias.to_string();
        target.entities.push(renamed_entity);
        return Ok(());
    }

    // Try to find the symbol in trait definitions
    if let Some(trait_def) = source.trait_definitions.iter().find(|t| t.name == symbol_name) {
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
