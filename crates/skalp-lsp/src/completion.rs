//! Auto-completion support for SKALP

use crate::DocumentState;
use tower_lsp::lsp_types::{CompletionItem, CompletionItemKind, InsertTextFormat, Position};

/// Get completion items for the current position
pub fn get_completions(doc: &DocumentState, position: Position) -> Vec<CompletionItem> {
    let mut completions = Vec::new();

    // Get the current line
    let line_idx = position.line as usize;
    if line_idx < doc.content.len_lines() {
        let line_text = doc.content.line(line_idx).as_str().unwrap_or("");
        let char_pos = position.character as usize;
        let prefix = &line_text[..char_pos.min(line_text.len())];

        // Check context for appropriate completions
        if prefix.trim().is_empty() || prefix.ends_with(' ') {
            // Top-level keywords
            completions.extend(get_keyword_completions());
        } else if prefix.contains("on(") && !prefix.contains(')') {
            // Event completions
            completions.extend(get_event_completions());
        } else if prefix.ends_with(':') {
            // Type completions
            completions.extend(get_type_completions());
        } else if prefix.ends_with('.') {
            // Member completions (for protocols, structs, etc.)
            completions.extend(get_member_completions(prefix));
        }
    }

    completions
}

/// Get keyword completions
fn get_keyword_completions() -> Vec<CompletionItem> {
    vec![
        CompletionItem {
            label: "entity".to_string(),
            kind: Some(CompletionItemKind::KEYWORD),
            detail: Some("Define a hardware entity".to_string()),
            insert_text: Some("entity ${1:name} {\n    $0\n}".to_string()),
            insert_text_format: Some(InsertTextFormat::SNIPPET),
            ..Default::default()
        },
        CompletionItem {
            label: "impl".to_string(),
            kind: Some(CompletionItemKind::KEYWORD),
            detail: Some("Implementation block for entity".to_string()),
            insert_text: Some("impl ${1:EntityName} {\n    $0\n}".to_string()),
            insert_text_format: Some(InsertTextFormat::SNIPPET),
            ..Default::default()
        },
        CompletionItem {
            label: "on".to_string(),
            kind: Some(CompletionItemKind::KEYWORD),
            detail: Some("Event-driven block".to_string()),
            insert_text: Some("on(${1:clock.rise}) {\n    $0\n}".to_string()),
            insert_text_format: Some(InsertTextFormat::SNIPPET),
            ..Default::default()
        },
        CompletionItem {
            label: "signal".to_string(),
            kind: Some(CompletionItemKind::KEYWORD),
            detail: Some("Declare a signal".to_string()),
            insert_text: Some("signal ${1:name}: ${2:type};".to_string()),
            insert_text_format: Some(InsertTextFormat::SNIPPET),
            ..Default::default()
        },
        CompletionItem {
            label: "protocol".to_string(),
            kind: Some(CompletionItemKind::KEYWORD),
            detail: Some("Define a communication protocol".to_string()),
            insert_text: Some("protocol ${1:name} {\n    $0\n}".to_string()),
            insert_text_format: Some(InsertTextFormat::SNIPPET),
            ..Default::default()
        },
        CompletionItem {
            label: "trait".to_string(),
            kind: Some(CompletionItemKind::KEYWORD),
            detail: Some("Define a trait".to_string()),
            insert_text: Some("trait ${1:name} {\n    $0\n}".to_string()),
            insert_text_format: Some(InsertTextFormat::SNIPPET),
            ..Default::default()
        },
        CompletionItem {
            label: "match".to_string(),
            kind: Some(CompletionItemKind::KEYWORD),
            detail: Some("Pattern matching".to_string()),
            insert_text: Some(
                "match ${1:expr} {\n    ${2:pattern} => ${3:value},\n    _ => ${4:default}\n}"
                    .to_string(),
            ),
            insert_text_format: Some(InsertTextFormat::SNIPPET),
            ..Default::default()
        },
        CompletionItem {
            label: "assert".to_string(),
            kind: Some(CompletionItemKind::KEYWORD),
            detail: Some("Assertion for verification".to_string()),
            insert_text: Some("assert ${1:condition};".to_string()),
            insert_text_format: Some(InsertTextFormat::SNIPPET),
            ..Default::default()
        },
        CompletionItem {
            label: "intent".to_string(),
            kind: Some(CompletionItemKind::KEYWORD),
            detail: Some("Design intent declaration".to_string()),
            insert_text: Some("@intent(${1:\"description\"})".to_string()),
            insert_text_format: Some(InsertTextFormat::SNIPPET),
            ..Default::default()
        },
    ]
}

/// Get event completions for on() blocks
fn get_event_completions() -> Vec<CompletionItem> {
    vec![
        CompletionItem {
            label: "clock.rise".to_string(),
            kind: Some(CompletionItemKind::VALUE),
            detail: Some("Rising edge of clock".to_string()),
            ..Default::default()
        },
        CompletionItem {
            label: "clock.fall".to_string(),
            kind: Some(CompletionItemKind::VALUE),
            detail: Some("Falling edge of clock".to_string()),
            ..Default::default()
        },
        CompletionItem {
            label: "reset.active".to_string(),
            kind: Some(CompletionItemKind::VALUE),
            detail: Some("Reset signal active".to_string()),
            ..Default::default()
        },
        CompletionItem {
            label: "reset.inactive".to_string(),
            kind: Some(CompletionItemKind::VALUE),
            detail: Some("Reset signal inactive".to_string()),
            ..Default::default()
        },
    ]
}

/// Get type completions
fn get_type_completions() -> Vec<CompletionItem> {
    vec![
        CompletionItem {
            label: "bit".to_string(),
            kind: Some(CompletionItemKind::CLASS),
            detail: Some("Single bit type".to_string()),
            ..Default::default()
        },
        CompletionItem {
            label: "bit<>".to_string(),
            kind: Some(CompletionItemKind::CLASS),
            detail: Some("Bit vector type".to_string()),
            insert_text: Some("bit<${1:width}>".to_string()),
            insert_text_format: Some(InsertTextFormat::SNIPPET),
            ..Default::default()
        },
        CompletionItem {
            label: "logic".to_string(),
            kind: Some(CompletionItemKind::CLASS),
            detail: Some("4-state logic type".to_string()),
            ..Default::default()
        },
        CompletionItem {
            label: "logic<>".to_string(),
            kind: Some(CompletionItemKind::CLASS),
            detail: Some("Logic vector type".to_string()),
            insert_text: Some("logic<${1:width}>".to_string()),
            insert_text_format: Some(InsertTextFormat::SNIPPET),
            ..Default::default()
        },
        CompletionItem {
            label: "int".to_string(),
            kind: Some(CompletionItemKind::CLASS),
            detail: Some("Signed integer type".to_string()),
            insert_text: Some("int<${1:width}>".to_string()),
            insert_text_format: Some(InsertTextFormat::SNIPPET),
            ..Default::default()
        },
        CompletionItem {
            label: "nat".to_string(),
            kind: Some(CompletionItemKind::CLASS),
            detail: Some("Unsigned integer type".to_string()),
            insert_text: Some("nat<${1:width}>".to_string()),
            insert_text_format: Some(InsertTextFormat::SNIPPET),
            ..Default::default()
        },
        CompletionItem {
            label: "clock".to_string(),
            kind: Some(CompletionItemKind::CLASS),
            detail: Some("Clock signal type".to_string()),
            ..Default::default()
        },
        CompletionItem {
            label: "reset".to_string(),
            kind: Some(CompletionItemKind::CLASS),
            detail: Some("Reset signal type".to_string()),
            ..Default::default()
        },
        CompletionItem {
            label: "array".to_string(),
            kind: Some(CompletionItemKind::CLASS),
            detail: Some("Array type".to_string()),
            insert_text: Some("array<${1:type}, ${2:size}>".to_string()),
            insert_text_format: Some(InsertTextFormat::SNIPPET),
            ..Default::default()
        },
    ]
}

/// Get member completions based on context
fn get_member_completions(_prefix: &str) -> Vec<CompletionItem> {
    // This would normally look up the type of the expression before the dot
    // For now, provide common members
    vec![
        CompletionItem {
            label: "rise".to_string(),
            kind: Some(CompletionItemKind::FIELD),
            detail: Some("Rising edge event".to_string()),
            ..Default::default()
        },
        CompletionItem {
            label: "fall".to_string(),
            kind: Some(CompletionItemKind::FIELD),
            detail: Some("Falling edge event".to_string()),
            ..Default::default()
        },
        CompletionItem {
            label: "active".to_string(),
            kind: Some(CompletionItemKind::FIELD),
            detail: Some("Active state".to_string()),
            ..Default::default()
        },
        CompletionItem {
            label: "inactive".to_string(),
            kind: Some(CompletionItemKind::FIELD),
            detail: Some("Inactive state".to_string()),
            ..Default::default()
        },
        CompletionItem {
            label: "valid".to_string(),
            kind: Some(CompletionItemKind::FIELD),
            detail: Some("Valid signal".to_string()),
            ..Default::default()
        },
        CompletionItem {
            label: "ready".to_string(),
            kind: Some(CompletionItemKind::FIELD),
            detail: Some("Ready signal".to_string()),
            ..Default::default()
        },
        CompletionItem {
            label: "data".to_string(),
            kind: Some(CompletionItemKind::FIELD),
            detail: Some("Data field".to_string()),
            ..Default::default()
        },
    ]
}

#[cfg(test)]
mod tests {
    use super::*;
    use ropey::Rope;

    #[test]
    fn test_keyword_completions() {
        let doc = DocumentState {
            content: Rope::from_str(""),
            version: 1,
            ast: None,
            diagnostics: Vec::new(),
        };

        let completions = get_completions(
            &doc,
            Position {
                line: 0,
                character: 0,
            },
        );
        assert!(completions.iter().any(|c| c.label == "entity"));
        assert!(completions.iter().any(|c| c.label == "protocol"));
    }

    #[test]
    fn test_event_completions() {
        let doc = DocumentState {
            content: Rope::from_str("on("),
            version: 1,
            ast: None,
            diagnostics: Vec::new(),
        };

        let completions = get_completions(
            &doc,
            Position {
                line: 0,
                character: 3,
            },
        );
        assert!(completions.iter().any(|c| c.label == "clock.rise"));
        assert!(completions.iter().any(|c| c.label == "reset.active"));
    }
}
