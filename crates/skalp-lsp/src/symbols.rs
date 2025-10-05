//! Symbol support for SKALP (document symbols, go-to-definition, find references)

use crate::DocumentState;
use tower_lsp::lsp_types::{
    GotoDefinitionResponse, Location, Position, Range, SymbolInformation, SymbolKind, Url,
};

/// Get all symbols in a document
pub fn get_document_symbols(doc: &DocumentState) -> Vec<SymbolInformation> {
    let mut symbols = Vec::new();
    let content = doc.content.to_string();

    // Parse entities
    for (line_num, line) in content.lines().enumerate() {
        if let Some(symbol) = parse_entity_symbol(line, line_num) {
            symbols.push(symbol);
        } else if let Some(symbol) = parse_protocol_symbol(line, line_num) {
            symbols.push(symbol);
        } else if let Some(symbol) = parse_trait_symbol(line, line_num) {
            symbols.push(symbol);
        } else if let Some(symbol) = parse_signal_symbol(line, line_num) {
            symbols.push(symbol);
        }
    }

    symbols
}

/// Go to definition for a symbol at position
pub fn goto_definition(
    doc: &DocumentState,
    position: Position,
    uri: &Url,
) -> Option<GotoDefinitionResponse> {
    let line_idx = position.line as usize;

    if line_idx >= doc.content.len_lines() {
        return None;
    }

    let line = doc.content.line(line_idx).as_str().unwrap_or("");

    // Extract the word at position
    let word = extract_word_at_position(line, position.character as usize)?;

    // Search for definition in document
    let content = doc.content.to_string();
    for (def_line_num, def_line) in content.lines().enumerate() {
        if is_definition_of(&word, def_line) {
            let range = Range {
                start: Position {
                    line: def_line_num as u32,
                    character: 0,
                },
                end: Position {
                    line: def_line_num as u32,
                    character: def_line.len() as u32,
                },
            };

            return Some(GotoDefinitionResponse::Scalar(Location {
                uri: uri.clone(),
                range,
            }));
        }
    }

    None
}

/// Find all references to a symbol
pub fn find_references(
    doc: &DocumentState,
    position: Position,
    uri: &Url,
) -> Option<Vec<Location>> {
    let line_idx = position.line as usize;

    if line_idx >= doc.content.len_lines() {
        return None;
    }

    let line = doc.content.line(line_idx).as_str().unwrap_or("");

    // Extract the word at position
    let word = extract_word_at_position(line, position.character as usize)?;

    // Find all occurrences
    let mut references = Vec::new();
    let content = doc.content.to_string();

    for (ref_line_num, ref_line) in content.lines().enumerate() {
        if let Some(col) = ref_line.find(&word) {
            references.push(Location {
                uri: uri.clone(),
                range: Range {
                    start: Position {
                        line: ref_line_num as u32,
                        character: col as u32,
                    },
                    end: Position {
                        line: ref_line_num as u32,
                        character: (col + word.len()) as u32,
                    },
                },
            });
        }
    }

    if references.is_empty() {
        None
    } else {
        Some(references)
    }
}

/// Parse an entity declaration into a symbol
fn parse_entity_symbol(line: &str, line_num: usize) -> Option<SymbolInformation> {
    let trimmed = line.trim();
    if trimmed.starts_with("entity ") {
        let parts: Vec<&str> = trimmed.split_whitespace().collect();
        if parts.len() >= 2 {
            let name = parts[1].trim_end_matches(['{', '<']);
            return Some(SymbolInformation {
                name: name.to_string(),
                kind: SymbolKind::CLASS,
                location: Location {
                    uri: Url::parse("file://").ok()?, // Placeholder
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
                },
                container_name: None,
                tags: None,
                #[allow(deprecated)]
                deprecated: None,
            });
        }
    }
    None
}

/// Parse a protocol declaration
fn parse_protocol_symbol(line: &str, line_num: usize) -> Option<SymbolInformation> {
    let trimmed = line.trim();
    if trimmed.starts_with("protocol ") {
        let parts: Vec<&str> = trimmed.split_whitespace().collect();
        if parts.len() >= 2 {
            let name = parts[1].trim_end_matches('{');
            return Some(SymbolInformation {
                name: name.to_string(),
                kind: SymbolKind::INTERFACE,
                location: Location {
                    uri: Url::parse("file://").ok()?,
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
                },
                container_name: None,
                tags: None,
                #[allow(deprecated)]
                deprecated: None,
            });
        }
    }
    None
}

/// Parse a trait declaration
fn parse_trait_symbol(line: &str, line_num: usize) -> Option<SymbolInformation> {
    let trimmed = line.trim();
    if trimmed.starts_with("trait ") {
        let parts: Vec<&str> = trimmed.split_whitespace().collect();
        if parts.len() >= 2 {
            let name = parts[1].trim_end_matches('{');
            return Some(SymbolInformation {
                name: name.to_string(),
                kind: SymbolKind::INTERFACE,
                location: Location {
                    uri: Url::parse("file://").ok()?,
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
                },
                container_name: None,
                tags: None,
                #[allow(deprecated)]
                deprecated: None,
            });
        }
    }
    None
}

/// Parse a signal declaration
fn parse_signal_symbol(line: &str, line_num: usize) -> Option<SymbolInformation> {
    let trimmed = line.trim();
    if let Some(stripped) = trimmed.strip_prefix("signal ") {
        let parts: Vec<&str> = stripped.split(':').collect();
        if !parts.is_empty() {
            let name = parts[0].trim();
            return Some(SymbolInformation {
                name: name.to_string(),
                kind: SymbolKind::VARIABLE,
                location: Location {
                    uri: Url::parse("file://").ok()?,
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
                },
                container_name: None,
                tags: None,
                #[allow(deprecated)]
                deprecated: None,
            });
        }
    }

    // Also check for port declarations
    if trimmed.starts_with("in ") || trimmed.starts_with("out ") || trimmed.starts_with("inout ") {
        let prefix_len = if trimmed.starts_with("inout") { 5 } else { 3 };
        let parts: Vec<&str> = trimmed[prefix_len..].trim().split(':').collect();
        if !parts.is_empty() {
            let name = parts[0].trim();
            return Some(SymbolInformation {
                name: name.to_string(),
                kind: SymbolKind::FIELD,
                location: Location {
                    uri: Url::parse("file://").ok()?,
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
                },
                container_name: None,
                tags: None,
                #[allow(deprecated)]
                deprecated: None,
            });
        }
    }

    None
}

/// Extract word at a given position
fn extract_word_at_position(line: &str, pos: usize) -> Option<String> {
    if pos > line.len() {
        return None;
    }

    let chars: Vec<char> = line.chars().collect();

    // Find word boundaries
    let mut start = pos;
    while start > 0 && (chars[start - 1].is_alphanumeric() || chars[start - 1] == '_') {
        start -= 1;
    }

    let mut end = pos;
    while end < chars.len() && (chars[end].is_alphanumeric() || chars[end] == '_') {
        end += 1;
    }

    if start == end {
        return None;
    }

    Some(line[start..end].to_string())
}

/// Check if a line is a definition of a given word
fn is_definition_of(word: &str, line: &str) -> bool {
    let patterns = vec![
        format!("entity {} ", word),
        format!("entity {}<", word),
        format!("entity {}{}", word, "{"),
        format!("protocol {} ", word),
        format!("protocol {}{}", word, "{"),
        format!("trait {} ", word),
        format!("trait {}{}", word, "{"),
        format!("signal {}: ", word),
        format!("in {}: ", word),
        format!("out {}: ", word),
        format!("inout {}: ", word),
    ];

    patterns
        .iter()
        .any(|pattern| line.trim().starts_with(pattern))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_entity_symbol() {
        let symbol = parse_entity_symbol("entity Counter {", 0);
        assert!(symbol.is_some());
        assert_eq!(symbol.unwrap().name, "Counter");
    }

    #[test]
    fn test_parse_signal_symbol() {
        let symbol = parse_signal_symbol("    signal data: bit<8>;", 5);
        assert!(symbol.is_some());
        assert_eq!(symbol.unwrap().name, "data");
    }

    #[test]
    fn test_extract_word() {
        assert_eq!(
            extract_word_at_position("entity Counter {", 7),
            Some("Counter".to_string())
        );
        assert_eq!(
            extract_word_at_position("signal data_bus: bit", 10),
            Some("data_bus".to_string())
        );
    }

    #[test]
    fn test_is_definition() {
        assert!(is_definition_of("Counter", "entity Counter {"));
        assert!(is_definition_of("data", "    signal data: bit<8>;"));
        assert!(!is_definition_of("Counter", "impl Counter {"));
    }
}
