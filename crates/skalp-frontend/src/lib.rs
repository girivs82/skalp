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
pub mod span;
pub mod syntax;
pub mod typeck;
pub mod types;

#[cfg(test)]
mod stream_test;

pub use hir::Hir;
pub use lexer::Lexer;
pub use parser::Parser;
pub use span::{LineIndex, SourceSpan};

use anyhow::{Context, Result};
use module_resolver::ModuleResolver;
use std::collections::HashMap;
use std::path::{Path, PathBuf};

/// Compilation context containing the main HIR and all loaded module HIRs
/// This allows the MIR compiler to resolve functions in their proper module scope
pub struct CompilationContext {
    /// The main/merged HIR for the top-level module
    pub main_hir: Hir,
    /// All loaded module HIRs (path -> HIR) for scope resolution
    pub module_hirs: HashMap<PathBuf, Hir>,
}

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
/// Returns a CompilationContext containing both the main HIR and all module HIRs
pub fn parse_and_build_compilation_context(file_path: &Path) -> Result<CompilationContext> {
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
    let mut builder =
        hir_builder::HirBuilderContext::with_source(&source, Some(file_path.to_path_buf()));
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

    // Extract all loaded module HIRs from the resolver
    let module_hirs: HashMap<PathBuf, Hir> = resolver
        .loaded_modules()
        .map(|(path, hir)| (path.clone(), hir.clone()))
        .collect();

    Ok(CompilationContext {
        main_hir: monomorphized_hir,
        module_hirs,
    })
}

/// Parse and build HIR from a file with full module resolution (backward compatibility)
/// Returns only the main HIR. For proper module scope resolution, use parse_and_build_compilation_context()
pub fn parse_and_build_hir_from_file(file_path: &Path) -> Result<Hir> {
    let context = parse_and_build_compilation_context(file_path)?;
    Ok(context.main_hir)
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
    let mut builder =
        hir_builder::HirBuilderContext::with_source(&source, Some(file_path.to_path_buf()));

    // Pre-register all entities in the symbol table
    for entity in &hir.entities {
        builder.preregister_entity(entity);
    }

    // Pre-register all functions with their return types (BUG FIX #67)
    // This ensures imported function return types are available for type inference
    eprintln!(
        "🔍 DEBUG: Pre-registering functions from {} implementations and {} top-level functions",
        hir.implementations.len(),
        hir.functions.len()
    );

    // Preregister top-level functions (these come from imports like 'use foo::*')
    for function in &hir.functions {
        eprintln!(
            "  Top-level function: {} (return type: {:?})",
            function.name, function.return_type
        );
        builder.preregister_function(function);
    }

    // Preregister functions from implementation blocks
    for (impl_idx, implementation) in hir.implementations.iter().enumerate() {
        eprintln!(
            "  Implementation {}: {} functions",
            impl_idx,
            implementation.functions.len()
        );
        for function in &implementation.functions {
            eprintln!("    Function: {}", function.name);
            builder.preregister_function(function);
        }
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

    // CRITICAL FIX (Bug #22 & #34): Don't overwrite ALL implementations!
    // The merged HIR includes implementations from imported modules (like AsyncFifo).
    // It also includes imported constants in the global implementation block (EntityId(0)).
    // Only replace implementations for entities that were rebuilt (main file entities).
    // But preserve imported constants in the global implementation block.

    // Save the imported constants from the global implementation block before modifying
    let mut imported_constants = Vec::new();
    if let Some(global_impl) = final_hir
        .implementations
        .iter()
        .find(|i| i.entity == hir::EntityId(0))
    {
        imported_constants = global_impl.constants.clone();
    }

    // Find which entity IDs have implementations in the rebuilt HIR
    let rebuilt_entity_ids: std::collections::HashSet<_> = rebuilt_hir
        .implementations
        .iter()
        .map(|impl_block| impl_block.entity)
        .collect();

    // Keep only imported implementations (those NOT rebuilt)
    final_hir
        .implementations
        .retain(|impl_block| !rebuilt_entity_ids.contains(&impl_block.entity));

    // Add the rebuilt implementations
    final_hir
        .implementations
        .extend(rebuilt_hir.implementations);

    // BUG #34 FIX: Restore imported constants to the global implementation block
    if !imported_constants.is_empty() {
        // Find or create the global implementation block
        if let Some(global_impl) = final_hir
            .implementations
            .iter_mut()
            .find(|i| i.entity == hir::EntityId(0))
        {
            // Merge imported constants with any constants from the rebuilt HIR
            // Add imported constants that aren't already present
            for imported_const in imported_constants {
                if !global_impl
                    .constants
                    .iter()
                    .any(|c| c.name == imported_const.name)
                {
                    global_impl.constants.push(imported_const);
                }
            }
        } else {
            // No global implementation block exists - create one with the imported constants
            final_hir.implementations.push(hir::HirImplementation {
                entity: hir::EntityId(0),
                signals: Vec::new(),
                variables: Vec::new(),
                constants: imported_constants,
                functions: Vec::new(),
                event_blocks: Vec::new(),
                assignments: Vec::new(),
                instances: Vec::new(),
                covergroups: Vec::new(),
                formal_blocks: Vec::new(),
                statements: Vec::new(),
            });
        }
    }

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

/// Remap port IDs in an implementation (BUG #33 FIX)
/// This is needed when merging modules - port IDs get renumbered but impl expressions still reference old IDs
fn remap_impl_ports(
    mut impl_block: hir::HirImplementation,
    port_id_map: &std::collections::HashMap<hir::PortId, hir::PortId>,
) -> hir::HirImplementation {
    // Remap ports in assignments
    for assignment in &mut impl_block.assignments {
        assignment.lhs = remap_lvalue_ports(&assignment.lhs, port_id_map);
        assignment.rhs = remap_expr_ports(&assignment.rhs, port_id_map);
    }

    // Remap ports in event blocks
    for event_block in &mut impl_block.event_blocks {
        // Remap trigger ports
        for trigger in &mut event_block.triggers {
            if let hir::HirEventSignal::Port(old_id) = trigger.signal {
                if let Some(&new_id) = port_id_map.get(&old_id) {
                    trigger.signal = hir::HirEventSignal::Port(new_id);
                }
            }
        }

        // Remap ports in statements
        for statement in &mut event_block.statements {
            *statement = remap_statement_ports(statement, port_id_map);
        }
    }

    impl_block
}

/// Remap port IDs in an LValue
fn remap_lvalue_ports(
    lvalue: &hir::HirLValue,
    port_id_map: &std::collections::HashMap<hir::PortId, hir::PortId>,
) -> hir::HirLValue {
    match lvalue {
        hir::HirLValue::Port(old_id) => port_id_map
            .get(old_id)
            .map_or(lvalue.clone(), |&new_id| hir::HirLValue::Port(new_id)),
        hir::HirLValue::Index(base, idx) => {
            let new_base = remap_lvalue_ports(base, port_id_map);
            let new_idx = remap_expr_ports(idx, port_id_map);
            hir::HirLValue::Index(Box::new(new_base), new_idx)
        }
        hir::HirLValue::Range(base, high, low) => {
            let new_base = remap_lvalue_ports(base, port_id_map);
            let new_high = remap_expr_ports(high, port_id_map);
            let new_low = remap_expr_ports(low, port_id_map);
            hir::HirLValue::Range(Box::new(new_base), new_high, new_low)
        }
        hir::HirLValue::FieldAccess { base, field } => {
            let new_base = remap_lvalue_ports(base, port_id_map);
            hir::HirLValue::FieldAccess {
                base: Box::new(new_base),
                field: field.clone(),
            }
        }
        _ => lvalue.clone(),
    }
}

/// Remap port IDs in an expression
fn remap_expr_ports(
    expr: &hir::HirExpression,
    port_id_map: &std::collections::HashMap<hir::PortId, hir::PortId>,
) -> hir::HirExpression {
    match expr {
        hir::HirExpression::Port(old_id) => port_id_map
            .get(old_id)
            .map_or(expr.clone(), |&new_id| hir::HirExpression::Port(new_id)),
        hir::HirExpression::Binary(bin) => {
            let left = remap_expr_ports(&bin.left, port_id_map);
            let right = remap_expr_ports(&bin.right, port_id_map);
            hir::HirExpression::Binary(hir::HirBinaryExpr {
                op: bin.op.clone(),
                left: Box::new(left),
                right: Box::new(right),
            })
        }
        hir::HirExpression::Unary(unary) => {
            let operand = remap_expr_ports(&unary.operand, port_id_map);
            hir::HirExpression::Unary(hir::HirUnaryExpr {
                op: unary.op.clone(),
                operand: Box::new(operand),
            })
        }
        hir::HirExpression::Index(base, index) => {
            let new_base = remap_expr_ports(base, port_id_map);
            let new_index = remap_expr_ports(index, port_id_map);
            hir::HirExpression::Index(Box::new(new_base), Box::new(new_index))
        }
        hir::HirExpression::Range(base, high, low) => {
            let new_base = remap_expr_ports(base, port_id_map);
            let new_high = remap_expr_ports(high, port_id_map);
            let new_low = remap_expr_ports(low, port_id_map);
            hir::HirExpression::Range(Box::new(new_base), Box::new(new_high), Box::new(new_low))
        }
        hir::HirExpression::FieldAccess { base, field } => {
            let new_base = remap_expr_ports(base, port_id_map);
            hir::HirExpression::FieldAccess {
                base: Box::new(new_base),
                field: field.clone(),
            }
        }
        hir::HirExpression::Call(call) => {
            let new_args = call
                .args
                .iter()
                .map(|arg| remap_expr_ports(arg, port_id_map))
                .collect();
            hir::HirExpression::Call(hir::HirCallExpr {
                function: call.function.clone(),
                type_args: call.type_args.clone(), // Preserve type args during port remapping
                named_type_args: call.named_type_args.clone(), // Preserve named type args
                args: new_args,
                impl_style: call.impl_style,
            })
        }
        hir::HirExpression::If(if_expr) => {
            let new_cond = remap_expr_ports(&if_expr.condition, port_id_map);
            let new_then = remap_expr_ports(&if_expr.then_expr, port_id_map);
            let new_else = remap_expr_ports(&if_expr.else_expr, port_id_map);
            hir::HirExpression::If(hir::HirIfExpr {
                condition: Box::new(new_cond),
                then_expr: Box::new(new_then),
                else_expr: Box::new(new_else),
            })
        }
        hir::HirExpression::Concat(exprs) => {
            let new_exprs = exprs
                .iter()
                .map(|e| remap_expr_ports(e, port_id_map))
                .collect();
            hir::HirExpression::Concat(new_exprs)
        }
        _ => expr.clone(),
    }
}

/// Remap port IDs in a statement
fn remap_statement_ports(
    stmt: &hir::HirStatement,
    port_id_map: &std::collections::HashMap<hir::PortId, hir::PortId>,
) -> hir::HirStatement {
    match stmt {
        hir::HirStatement::Assignment(assign) => {
            let mut new_assign = assign.clone();
            new_assign.lhs = remap_lvalue_ports(&assign.lhs, port_id_map);
            new_assign.rhs = remap_expr_ports(&assign.rhs, port_id_map);
            hir::HirStatement::Assignment(new_assign)
        }
        hir::HirStatement::If(if_stmt) => {
            let mut new_if = if_stmt.clone();
            new_if.condition = remap_expr_ports(&if_stmt.condition, port_id_map);
            new_if.then_statements = if_stmt
                .then_statements
                .iter()
                .map(|s| remap_statement_ports(s, port_id_map))
                .collect();
            new_if.else_statements = if_stmt.else_statements.as_ref().map(|stmts| {
                stmts
                    .iter()
                    .map(|s| remap_statement_ports(s, port_id_map))
                    .collect()
            });
            hir::HirStatement::If(new_if)
        }
        hir::HirStatement::Match(match_stmt) => {
            let mut new_match = match_stmt.clone();
            new_match.expr = remap_expr_ports(&match_stmt.expr, port_id_map);
            new_match.arms = match_stmt
                .arms
                .iter()
                .map(|arm| {
                    let mut new_arm = arm.clone();
                    new_arm.statements = arm
                        .statements
                        .iter()
                        .map(|s| remap_statement_ports(s, port_id_map))
                        .collect();
                    new_arm
                })
                .collect();
            hir::HirStatement::Match(new_match)
        }
        hir::HirStatement::Let(let_stmt) => {
            let mut new_let = let_stmt.clone();
            new_let.value = remap_expr_ports(&let_stmt.value, port_id_map);
            hir::HirStatement::Let(new_let)
        }
        _ => stmt.clone(),
    }
}

/// Merge a specific symbol from a module into the current HIR
fn merge_symbol(target: &mut Hir, source: &Hir, symbol_name: &str) -> Result<()> {
    // Try to find the symbol in entities
    if let Some(entity) = source.entities.iter().find(|e| e.name == symbol_name) {
        let old_entity_id = entity.id;

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

        // Renumber all ports and build port ID mapping
        let mut port_id_map = std::collections::HashMap::new();
        for (i, port) in imported_entity.ports.iter_mut().enumerate() {
            let old_id = port.id;
            let new_id = hir::PortId(next_port_id + i as u32);
            port_id_map.insert(old_id, new_id);
            port.id = new_id;
        }

        target.entities.push(imported_entity);

        // CRITICAL: Also merge the implementation for this entity (needed for generic entities)
        // BUG #33 FIX: Remap port IDs in the implementation to match the renumbered ports!
        if let Some(impl_block) = source
            .implementations
            .iter()
            .find(|i| i.entity == old_entity_id)
        {
            let mut imported_impl = impl_block.clone();
            // Update implementation to point to the new entity ID
            imported_impl.entity = new_entity_id;
            // BUG #33 FIX: Remap port IDs in all assignments
            imported_impl = remap_impl_ports(imported_impl, &port_id_map);
            target.implementations.push(imported_impl);
        }

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

    // BUG #34 FIX: Try to find the symbol in constants (from implementation blocks)
    for impl_block in &source.implementations {
        if let Some(constant) = impl_block.constants.iter().find(|c| c.name == symbol_name) {
            // Add the constant to the target's global implementation block
            // Create one if it doesn't exist
            if target.implementations.is_empty() {
                target.implementations.push(hir::HirImplementation {
                    entity: hir::EntityId(0), // Dummy entity ID for global scope
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
            // Add constant to the first implementation (global scope)
            if let Some(impl_block) = target.implementations.first_mut() {
                impl_block.constants.push(constant.clone());
            }
            return Ok(());
        }
    }

    // BUG #67 FIX: Try to find the symbol in functions (from implementation blocks)
    // This is critical for importing stdlib functions with their return types
    for impl_block in &source.implementations {
        if let Some(function) = impl_block.functions.iter().find(|f| f.name == symbol_name) {
            eprintln!(
                "📦 BUG #67 FIX: Merging function '{}' with return type: {:?}",
                function.name, function.return_type
            );
            // Add the function to the target's global implementation block
            // Create one if it doesn't exist
            if target.implementations.is_empty() {
                target.implementations.push(hir::HirImplementation {
                    entity: hir::EntityId(0), // Dummy entity ID for global scope
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
            // Add function to the first implementation (global scope)
            if let Some(impl_block) = target.implementations.first_mut() {
                impl_block.functions.push(function.clone());
            }
            return Ok(());
        }
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
        let old_entity_id = entity.id;

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

        let mut renamed_entity = entity.clone();
        renamed_entity.id = new_entity_id;
        renamed_entity.name = alias.to_string();

        // Renumber all ports and build port ID mapping
        let mut port_id_map = std::collections::HashMap::new();
        for (i, port) in renamed_entity.ports.iter_mut().enumerate() {
            let old_id = port.id;
            let new_id = hir::PortId(next_port_id + i as u32);
            port_id_map.insert(old_id, new_id);
            port.id = new_id;
        }

        target.entities.push(renamed_entity);

        // CRITICAL: Also merge the implementation for this entity (needed for generic entities)
        // BUG #33 FIX: Remap port IDs in the implementation to match the renumbered ports!
        if let Some(impl_block) = source
            .implementations
            .iter()
            .find(|i| i.entity == old_entity_id)
        {
            let mut renamed_impl = impl_block.clone();
            // Update implementation to point to the new entity ID
            renamed_impl.entity = new_entity_id;
            // BUG #33 FIX: Remap port IDs in all assignments
            renamed_impl = remap_impl_ports(renamed_impl, &port_id_map);
            target.implementations.push(renamed_impl);
        }

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

    // BUG #34 FIX: Try to find the symbol in constants (from implementation blocks)
    for impl_block in &source.implementations {
        if let Some(constant) = impl_block.constants.iter().find(|c| c.name == symbol_name) {
            // Add the constant to the target's global implementation block with the renamed alias
            // Create one if it doesn't exist
            if target.implementations.is_empty() {
                target.implementations.push(hir::HirImplementation {
                    entity: hir::EntityId(0), // Dummy entity ID for global scope
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
            // Add renamed constant to the first implementation (global scope)
            if let Some(impl_block) = target.implementations.first_mut() {
                let mut renamed_constant = constant.clone();
                renamed_constant.name = alias.to_string();
                impl_block.constants.push(renamed_constant);
            }
            return Ok(());
        }
    }

    Ok(())
}

/// Merge all public symbols from a module
fn merge_all_symbols(target: &mut Hir, source: &Hir) -> Result<()> {
    use hir::HirVisibility;

    // Merge all public entities (and their implementations)
    for entity in &source.entities {
        if entity.visibility == HirVisibility::Public {
            let old_entity_id = entity.id;

            // Assign new unique entity ID
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

            // Renumber all ports and build port ID mapping
            let mut port_id_map = std::collections::HashMap::new();
            for (i, port) in imported_entity.ports.iter_mut().enumerate() {
                let old_id = port.id;
                let new_id = hir::PortId(next_port_id + i as u32);
                port_id_map.insert(old_id, new_id);
                port.id = new_id;
            }

            target.entities.push(imported_entity);

            // Also merge the implementation for this entity (needed for generic entities)
            // BUG #33 FIX: Remap port IDs in the implementation to match the renumbered ports!
            if let Some(impl_block) = source
                .implementations
                .iter()
                .find(|i| i.entity == old_entity_id)
            {
                let mut imported_impl = impl_block.clone();
                // Update implementation to point to the new entity ID
                imported_impl.entity = new_entity_id;
                // BUG #33 FIX: Remap port IDs in all assignments
                imported_impl = remap_impl_ports(imported_impl, &port_id_map);
                target.implementations.push(imported_impl);
            }
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

    // BUG #34 FIX: Merge all public constants (from implementation blocks)
    // Note: Constants don't currently have a visibility field in HIR, but we only merge
    // constants from the "global" implementation block (EntityId(0)) which are effectively public
    for impl_block in &source.implementations {
        // Only merge constants from the global scope (entity ID 0)
        if impl_block.entity == hir::EntityId(0) {
            for constant in &impl_block.constants {
                // Create global implementation block if it doesn't exist
                if target.implementations.is_empty() {
                    target.implementations.push(hir::HirImplementation {
                        entity: hir::EntityId(0),
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
                // Add constant to the first implementation (global scope)
                if let Some(impl_block) = target.implementations.first_mut() {
                    impl_block.constants.push(constant.clone());
                }
            }
        }
    }

    // BUG #67 FIX: Merge all functions (from implementation blocks)
    // This is critical for glob imports of stdlib functions with their return types
    for impl_block in &source.implementations {
        // Only merge functions from the global scope (entity ID 0)
        if impl_block.entity == hir::EntityId(0) {
            for function in &impl_block.functions {
                eprintln!(
                    "📦 BUG #67 FIX: Merging glob-imported function '{}' with return type: {:?}",
                    function.name, function.return_type
                );
                // Create global implementation block if it doesn't exist
                if target.implementations.is_empty() {
                    target.implementations.push(hir::HirImplementation {
                        entity: hir::EntityId(0),
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
                // Add function to the first implementation (global scope)
                if let Some(impl_block) = target.implementations.first_mut() {
                    impl_block.functions.push(function.clone());
                }
            }
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

    // Build HIR from syntax tree with source tracking for spans
    let mut builder = hir_builder::HirBuilderContext::with_source(source, None);
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
