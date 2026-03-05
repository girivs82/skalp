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
use skalp_frontend::diagnostic_render::{DiagnosticMessage, DiagnosticRenderer, Severity};
use skalp_frontend::CompilationContext;
use skalp_frontend::Hir;
use std::path::{Path, PathBuf};

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
    let filename = file_path
        .map(|p| p.display().to_string())
        .unwrap_or_else(|| "<input>".to_string());

    // Lex
    let _tokens = lexer::tokenize(source);

    // Parse to syntax tree
    let parse_result = parse::parse_vhdl(source);

    if !parse_result.errors.is_empty() {
        let renderer = DiagnosticRenderer::new(&filename, source);
        let diags: Vec<DiagnosticMessage> = parse_result
            .errors
            .iter()
            .map(|e| DiagnosticMessage {
                severity: Severity::Error,
                message: &e.message,
                start: e.position,
                end: e.end_position,
                labels: &[],
            })
            .collect();

        // Emit to stderr for interactive use
        for d in &diags {
            renderer.emit(d);
        }

        // Also produce a string for the anyhow error
        let rendered = renderer.render_all_to_string(&diags);
        anyhow::bail!(
            "VHDL parsing failed with {} error(s):\n{}",
            parse_result.errors.len(),
            rendered
        );
    }

    // Lower to HIR
    let mut lowerer = hir_lower::VhdlHirBuilder::new(file_path);
    let hir = lowerer.lower(&parse_result.root)?;
    let diags = lowerer.diagnostics().to_vec();

    // Emit lowering warnings/errors to stderr
    if !diags.is_empty() {
        let renderer = DiagnosticRenderer::new(&filename, source);
        for d in &diags {
            let sev = match d.severity {
                diagnostics::VhdlSeverity::Error => Severity::Error,
                diagnostics::VhdlSeverity::Warning => Severity::Warning,
            };
            renderer.emit(&DiagnosticMessage {
                severity: sev,
                message: &d.message,
                start: d.position,
                end: d.end_position,
                labels: &[],
            });
        }
    }

    Ok((hir, diags))
}

/// Parse multiple VHDL files into a single CompilationContext.
/// Entity declarations in one file can be referenced by architectures in another.
pub fn parse_vhdl_project(files: &[PathBuf]) -> Result<CompilationContext> {
    if files.is_empty() {
        anyhow::bail!("No VHDL files provided");
    }
    if files.len() == 1 {
        return parse_vhdl(&files[0]);
    }

    let design_name = files[0]
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("vhdl_design")
        .to_string();

    // Parse all files into syntax trees, collecting errors
    let mut parsed: Vec<(PathBuf, String, parse::ParseResult)> = Vec::new();
    for file_path in files {
        let source = std::fs::read_to_string(file_path)
            .map_err(|e| anyhow::anyhow!("Failed to read VHDL file {:?}: {}", file_path, e))?;
        let parse_result = parse::parse_vhdl(&source);

        if !parse_result.errors.is_empty() {
            let filename = file_path.display().to_string();
            let renderer = DiagnosticRenderer::new(&filename, &source);
            let diags: Vec<DiagnosticMessage> = parse_result
                .errors
                .iter()
                .map(|e| DiagnosticMessage {
                    severity: Severity::Error,
                    message: &e.message,
                    start: e.position,
                    end: e.end_position,
                    labels: &[],
                })
                .collect();
            for d in &diags {
                renderer.emit(d);
            }
            let rendered = renderer.render_all_to_string(&diags);
            anyhow::bail!(
                "VHDL parsing failed for {:?} with {} error(s):\n{}",
                file_path,
                parse_result.errors.len(),
                rendered
            );
        }

        parsed.push((file_path.clone(), source, parse_result));
    }

    // Lower all roots with a shared builder
    let mut lowerer = hir_lower::VhdlHirBuilder::new(Some(&files[0]));
    let roots: Vec<_> = parsed.iter().map(|(_, _, pr)| pr.root.clone()).collect();
    let hir = lowerer.lower_multiple_roots(design_name, &roots)?;

    // Emit diagnostics from all files
    let diags = lowerer.diagnostics().to_vec();
    if !diags.is_empty() {
        // Use the first file for diagnostic rendering (lowering diagnostics
        // don't carry per-file info currently)
        let filename = files[0].display().to_string();
        let source = &parsed[0].1;
        let renderer = DiagnosticRenderer::new(&filename, source);
        for d in &diags {
            let sev = match d.severity {
                diagnostics::VhdlSeverity::Error => Severity::Error,
                diagnostics::VhdlSeverity::Warning => Severity::Warning,
            };
            renderer.emit(&DiagnosticMessage {
                severity: sev,
                message: &d.message,
                start: d.position,
                end: d.end_position,
                labels: &[],
            });
        }
    }

    Ok(CompilationContext {
        main_hir: hir,
        module_hirs: indexmap::IndexMap::new(),
    })
}

/// Discover and parse all VHDL files in a directory into a single CompilationContext.
/// The `main_file` is listed first; companion `.vhd`/`.vhdl` files in the same directory
/// are included after it.
pub fn parse_vhdl_directory(main_file: &Path) -> Result<CompilationContext> {
    let dir = main_file
        .parent()
        .ok_or_else(|| anyhow::anyhow!("Cannot determine parent directory of {:?}", main_file))?;

    let mut files = vec![main_file.to_path_buf()];

    if let Ok(entries) = std::fs::read_dir(dir) {
        let mut companions: Vec<PathBuf> = entries
            .filter_map(|e| e.ok())
            .map(|e| e.path())
            .filter(|p| {
                p != main_file
                    && matches!(
                        p.extension().and_then(|s| s.to_str()),
                        Some("vhd") | Some("vhdl")
                    )
            })
            .collect();
        companions.sort();
        files.extend(companions);
    }

    if files.len() == 1 {
        return parse_vhdl(main_file);
    }

    parse_vhdl_project(&files)
}
