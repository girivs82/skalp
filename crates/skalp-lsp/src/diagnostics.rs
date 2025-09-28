//! Diagnostics generation for SKALP source files

use tower_lsp::lsp_types::{Diagnostic, DiagnosticSeverity, Position, Range};

/// Analyze a document and generate diagnostics
pub fn analyze_document(content: &str) -> Vec<Diagnostic> {
    let mut diagnostics = Vec::new();

    // Check for basic syntax issues
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
                code: Some(tower_lsp::lsp_types::NumberOrString::String("E001".to_string())),
                source: Some("skalp".to_string()),
                message: "Missing semicolon".to_string(),
                related_information: None,
                tags: None,
                code_description: None,
                data: None,
            });
        }

        // Check for clock domain crossing without explicit handling
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
                code: Some(tower_lsp::lsp_types::NumberOrString::String("W001".to_string())),
                source: Some("skalp".to_string()),
                message: "Potential clock domain crossing detected".to_string(),
                related_information: None,
                tags: None,
                code_description: None,
                data: None,
            });
        }

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
                code: Some(tower_lsp::lsp_types::NumberOrString::String("I001".to_string())),
                source: Some("skalp".to_string()),
                message: "Use 'on(clock.rise)' instead of 'always @'".to_string(),
                related_information: None,
                tags: None,
                code_description: None,
                data: None,
            });
        }
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
    if trimmed.ends_with('{') || trimmed.ends_with('}') || trimmed.contains("entity")
        || trimmed.contains("impl") || trimmed.contains("on(") {
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_missing_semicolon_detection() {
        let content = "signal data: bit<8>\ndata <= 0xFF";
        let diagnostics = analyze_document(content);
        assert_eq!(diagnostics.len(), 2);
        assert_eq!(diagnostics[0].message, "Missing semicolon");
    }

    #[test]
    fn test_cdc_warning() {
        let content = "signal data<'clk1>: bit<8> = input<'clk2>;";
        let diagnostics = analyze_document(content);
        assert!(diagnostics.iter().any(|d| d.message.contains("clock domain crossing")));
    }

    #[test]
    fn test_deprecated_syntax() {
        let content = "always @ (posedge clk)";
        let diagnostics = analyze_document(content);
        assert!(diagnostics.iter().any(|d| d.message.contains("on(clock.rise)")));
    }
}