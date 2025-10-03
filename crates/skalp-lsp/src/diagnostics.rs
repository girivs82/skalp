//! Diagnostics generation for SKALP source files

use skalp_frontend::parse::{parse, parse_with_errors, ParseError};
use tower_lsp::lsp_types::{Diagnostic, DiagnosticSeverity, Position, Range};

/// Analyze a document and generate diagnostics
pub fn analyze_document(content: &str) -> Vec<Diagnostic> {
    let mut diagnostics = Vec::new();

    // Try to parse with the real SKALP frontend
    let (_syntax_tree, parse_errors) = parse_with_errors(content);

    if parse_errors.is_empty() {
        // Parsing succeeded - add style warnings
        check_style_warnings(content, &mut diagnostics);
    } else {
        // Convert parse errors to LSP diagnostics
        for error in parse_errors {
            if let Some(diagnostic) = parse_error_to_diagnostic(&error) {
                diagnostics.push(diagnostic);
            }
        }
    }

    // Additional static analysis checks
    let lines: Vec<&str> = content.lines().collect();

    for (line_num, line) in lines.iter().enumerate() {
        // Check for missing semicolons after statements
        if should_have_semicolon(line) && !line.trim_end().ends_with(';') {
            diagnostics.push(Diagnostic {
                range: Range {
                    start: Position {
                        line: line_num as u32,
                        character: line.len() as u32 - 1,
                    },
                    end: Position {
                        line: line_num as u32,
                        character: line.len() as u32,
                    },
                },
                severity: Some(DiagnosticSeverity::ERROR),
                code: Some(tower_lsp::lsp_types::NumberOrString::String(
                    "E001".to_string(),
                )),
                source: Some("skalp".to_string()),
                message: "Missing semicolon".to_string(),
                related_information: None,
                tags: None,
                code_description: None,
                data: None,
            });
        }

        // Note: Style warnings are now handled in check_style_warnings()
    }

    diagnostics
}

/// Check if a line should have a semicolon
fn should_have_semicolon(line: &str) -> bool {
    let trimmed = line.trim();

    // Skip empty lines and comments
    if trimmed.is_empty() || trimmed.starts_with("//") {
        return false;
    }

    // Skip lines that are part of block structures
    if trimmed.ends_with('{')
        || trimmed.ends_with('}')
        || trimmed.contains("entity")
        || trimmed.contains("impl")
        || trimmed.contains("on(")
    {
        return false;
    }

    // Lines with assignments or signal declarations should have semicolons
    trimmed.contains("<=") || trimmed.contains("signal") || trimmed.contains("let")
}

/// Detect potential clock domain crossings
fn detect_potential_cdc(line: &str) -> bool {
    // Simple heuristic: look for signals with different clock annotations
    line.contains("<'clk1>") && line.contains("<'clk2>")
}

/// Convert a parse error to an LSP diagnostic
fn parse_error_to_diagnostic(error: &ParseError) -> Option<Diagnostic> {
    // For now, create a simple diagnostic
    // TODO: Extract proper position information from ParseError
    Some(Diagnostic {
        range: Range {
            start: Position {
                line: 0,
                character: 0,
            },
            end: Position {
                line: 0,
                character: 1,
            },
        },
        severity: Some(DiagnosticSeverity::ERROR),
        code: Some(tower_lsp::lsp_types::NumberOrString::String(
            "E100".to_string(),
        )),
        source: Some("skalp-parser".to_string()),
        message: format!("Parse error: {:?}", error),
        related_information: None,
        tags: None,
        code_description: None,
        data: None,
    })
}

/// Check for style and best practice warnings
fn check_style_warnings(content: &str, diagnostics: &mut Vec<Diagnostic>) {
    let lines: Vec<&str> = content.lines().collect();

    for (line_num, line) in lines.iter().enumerate() {
        // Check for deprecated syntax
        if line.contains("always @") {
            diagnostics.push(Diagnostic {
                range: Range {
                    start: Position {
                        line: line_num as u32,
                        character: line.find("always @").unwrap() as u32,
                    },
                    end: Position {
                        line: line_num as u32,
                        character: (line.find("always @").unwrap() + 8) as u32,
                    },
                },
                severity: Some(DiagnosticSeverity::INFORMATION),
                code: Some(tower_lsp::lsp_types::NumberOrString::String(
                    "I001".to_string(),
                )),
                source: Some("skalp".to_string()),
                message: "Use 'on(clock.rise)' instead of 'always @'".to_string(),
                related_information: None,
                tags: None,
                code_description: None,
                data: None,
            });
        }

        // Check for potential clock domain crossing
        if detect_potential_cdc(line) {
            diagnostics.push(Diagnostic {
                range: Range {
                    start: Position {
                        line: line_num as u32,
                        character: 0,
                    },
                    end: Position {
                        line: line_num as u32,
                        character: line.len() as u32,
                    },
                },
                severity: Some(DiagnosticSeverity::WARNING),
                code: Some(tower_lsp::lsp_types::NumberOrString::String(
                    "W001".to_string(),
                )),
                source: Some("skalp".to_string()),
                message: "Potential clock domain crossing detected".to_string(),
                related_information: None,
                tags: None,
                code_description: None,
                data: None,
            });
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_missing_semicolon_detection() {
        let content = "signal data: bit<8>\ndata <= 0xFF";
        let diagnostics = analyze_document(content);
        // Should have parse errors since this isn't valid SKALP syntax
        assert!(!diagnostics.is_empty());
        // Check if any diagnostic mentions parsing issues or missing semicolons
        assert!(diagnostics
            .iter()
            .any(|d| d.message.contains("Parse error") || d.message.contains("Missing semicolon")));
    }

    #[test]
    fn test_cdc_warning() {
        // Test the CDC detection function directly
        assert!(detect_potential_cdc(
            "signal data<'clk1>: bit<8> = input<'clk2>;"
        ));
        assert!(!detect_potential_cdc("signal data<'clk>: bit<8>;"));
    }

    #[test]
    fn test_deprecated_syntax() {
        // Test the deprecated syntax directly in check_style_warnings
        let mut diagnostics = Vec::new();
        check_style_warnings("always @ (posedge clk)", &mut diagnostics);
        assert!(diagnostics
            .iter()
            .any(|d| d.message.contains("on(clock.rise)")));
    }
}
