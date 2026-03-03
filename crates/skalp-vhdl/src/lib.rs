#![allow(dead_code, unused_variables, unused_imports)]

pub mod builtins;
pub mod diagnostics;
pub mod formatter;
pub mod hir_lower;
pub mod lexer;
pub mod parse;
pub mod syntax;
pub mod vhdl_types;

use anyhow::Result;
use skalp_frontend::CompilationContext;
use skalp_frontend::Hir;
use std::path::Path;

/// Parse a VHDL file and produce a CompilationContext for the skalp pipeline
pub fn parse_vhdl(file_path: &Path) -> Result<CompilationContext> {
    let source = std::fs::read_to_string(file_path)
        .map_err(|e| anyhow::anyhow!("Failed to read VHDL file {:?}: {}", file_path, e))?;

    let hir = parse_vhdl_source(&source, Some(file_path))?;

    Ok(CompilationContext {
        main_hir: hir,
        module_hirs: indexmap::IndexMap::new(),
    })
}

/// Parse VHDL source text and produce HIR
pub fn parse_vhdl_source(source: &str, file_path: Option<&Path>) -> Result<Hir> {
    let (hir, _diagnostics) = parse_vhdl_source_with_diagnostics(source, file_path)?;
    Ok(hir)
}

/// Parse VHDL source text and produce HIR along with lowering diagnostics
pub fn parse_vhdl_source_with_diagnostics(
    source: &str,
    file_path: Option<&Path>,
) -> Result<(Hir, Vec<diagnostics::VhdlError>)> {
    // Lex
    let _tokens = lexer::tokenize(source);

    // Parse to syntax tree
    let parse_result = parse::parse_vhdl(source);

    if !parse_result.errors.is_empty() {
        let error_msgs: Vec<String> = parse_result
            .errors
            .iter()
            .map(|e| format!("  {} at pos {}", e.message, e.position))
            .collect();
        anyhow::bail!(
            "VHDL parsing failed with {} errors:\n{}",
            parse_result.errors.len(),
            error_msgs.join("\n")
        );
    }

    // Lower to HIR
    let mut lowerer = hir_lower::VhdlHirBuilder::new(file_path);
    let hir = lowerer.lower(&parse_result.root)?;
    let diags = lowerer.diagnostics().to_vec();

    Ok((hir, diags))
}
