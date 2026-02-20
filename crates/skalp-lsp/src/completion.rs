//! Auto-completion support for SKALP

use crate::analysis::AnalysisContext;
use crate::DocumentState;
use tower_lsp::lsp_types::{CompletionItem, CompletionItemKind, InsertTextFormat, Position};

/// Get completion items for the current position
pub fn get_completions(
    doc: &DocumentState,
    position: Position,
    analysis: Option<&AnalysisContext>,
) -> Vec<CompletionItem> {
    let mut completions = Vec::new();

    // Get the current line
    let line_idx = position.line as usize;
    if line_idx >= doc.content.len_lines() {
        return completions;
    }

    let line_text = doc.content.line(line_idx).to_string();
    let char_pos = position.character as usize;
    let prefix = &line_text[..char_pos.min(line_text.len())];

    // Determine the word being typed (for filtering)
    let typing_word = extract_typing_word(prefix);

    // Check context for appropriate completions
    if prefix.contains("on(") && !prefix.contains(')') {
        // Event completions — use actual clock/reset port names if available
        completions.extend(get_event_completions(analysis, position.line));
    } else if prefix.ends_with('.') {
        // Member completions — try instance-aware completions
        let before_dot = extract_word_before_dot(prefix);
        completions.extend(get_member_completions(
            before_dot.as_deref(),
            analysis,
            position.line,
        ));
    } else if is_type_position(prefix) {
        // Type completions: after `:` in declarations
        completions.extend(get_type_completions(analysis));
    } else if is_expression_position(prefix) {
        // Expression context: suggest signals, ports, constants, instances, enum variants
        if let Some(ctx) = analysis {
            completions.extend(get_expression_completions(
                ctx,
                position.line,
                typing_word.as_deref(),
            ));
        }
    } else if prefix.trim().is_empty() || prefix.ends_with(' ') {
        // Top-level or after space — keywords + context-aware suggestions
        completions.extend(get_keyword_completions());
        if let Some(ctx) = analysis {
            // If inside an impl block, suggest signals/ports/constants
            if ctx.scope_map.contains_key(&position.line) {
                completions.extend(get_expression_completions(ctx, position.line, None));
            }
        }
    } else {
        // General typing — suggest everything in scope
        if let Some(ctx) = analysis {
            completions.extend(get_expression_completions(
                ctx,
                position.line,
                typing_word.as_deref(),
            ));
        }
    }

    completions
}

/// Extract the word currently being typed (partial identifier before cursor)
fn extract_typing_word(prefix: &str) -> Option<String> {
    let bytes = prefix.as_bytes();
    let mut end = bytes.len();
    while end > 0 && (bytes[end - 1].is_ascii_alphanumeric() || bytes[end - 1] == b'_') {
        end -= 1;
    }
    if end < bytes.len() {
        Some(prefix[end..].to_string())
    } else {
        None
    }
}

/// Extract the word before a trailing dot (e.g., "instance_name" from "  instance_name.")
fn extract_word_before_dot(prefix: &str) -> Option<String> {
    let without_dot = prefix.strip_suffix('.')?;
    let bytes = without_dot.as_bytes();
    let mut start = bytes.len();
    while start > 0 && (bytes[start - 1].is_ascii_alphanumeric() || bytes[start - 1] == b'_') {
        start -= 1;
    }
    if start < bytes.len() {
        Some(without_dot[start..].to_string())
    } else {
        None
    }
}

/// Check if cursor is in a type position (after `:` in declarations)
fn is_type_position(prefix: &str) -> bool {
    let trimmed = prefix.trim();
    // After `: ` in signal/port/const declarations
    if prefix.ends_with(": ") || prefix.ends_with(':') {
        return true;
    }
    // Check if we're after a colon with some partial type typed
    // e.g., "    signal foo: bi" — find if there's a `:` with only identifier chars after
    if let Some(colon_pos) = prefix.rfind(':') {
        let after_colon = prefix[colon_pos + 1..].trim();
        if !after_colon.is_empty()
            && after_colon.chars().all(|c| c.is_alphanumeric() || c == '_')
            && (trimmed.starts_with("signal ")
                || trimmed.starts_with("in ")
                || trimmed.starts_with("out ")
                || trimmed.starts_with("inout ")
                || trimmed.starts_with("const ")
                || trimmed.starts_with("pub const ")
                || trimmed.starts_with("let "))
        {
            return true;
        }
    }
    false
}

/// Check if cursor is in an expression position (after `=`, `<=`, inside `if`, etc.)
fn is_expression_position(prefix: &str) -> bool {
    let trimmed = prefix.trim();
    // After assignment operators
    if prefix.ends_with("= ") || prefix.ends_with("<=") || prefix.ends_with("<= ") {
        return true;
    }
    // After `if ` or `else if `
    if trimmed.starts_with("if ") || trimmed.contains("else if ") {
        return true;
    }
    // After arithmetic/comparison operators
    if prefix.ends_with("+ ")
        || prefix.ends_with("- ")
        || prefix.ends_with("* ")
        || prefix.ends_with("/ ")
        || prefix.ends_with("== ")
        || prefix.ends_with("!= ")
        || prefix.ends_with("> ")
        || prefix.ends_with("< ")
        || prefix.ends_with("&& ")
        || prefix.ends_with("|| ")
    {
        return true;
    }
    // Inside match arms: after `=>`
    if prefix.contains("=>") {
        return true;
    }
    false
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
            label: "let".to_string(),
            kind: Some(CompletionItemKind::KEYWORD),
            detail: Some("Instance or variable binding".to_string()),
            insert_text: Some("let ${1:name} = ${2:value}".to_string()),
            insert_text_format: Some(InsertTextFormat::SNIPPET),
            ..Default::default()
        },
        CompletionItem {
            label: "const".to_string(),
            kind: Some(CompletionItemKind::KEYWORD),
            detail: Some("Constant declaration".to_string()),
            insert_text: Some("const ${1:NAME}: ${2:type} = ${3:value};".to_string()),
            insert_text_format: Some(InsertTextFormat::SNIPPET),
            ..Default::default()
        },
        CompletionItem {
            label: "generate for".to_string(),
            kind: Some(CompletionItemKind::KEYWORD),
            detail: Some("Generate-for loop (compile-time unrolling)".to_string()),
            insert_text: Some("generate for ${1:i} in ${2:0}..${3:N} {\n    $0\n}".to_string()),
            insert_text_format: Some(InsertTextFormat::SNIPPET),
            ..Default::default()
        },
    ]
}

/// Get event completions for on() blocks — uses actual clock/reset port names
fn get_event_completions(
    analysis: Option<&AnalysisContext>,
    cursor_line: u32,
) -> Vec<CompletionItem> {
    let mut completions = Vec::new();

    if let Some(ctx) = analysis {
        // Find clock and reset ports in the current entity scope
        let scope = ctx.scope_map.get(&cursor_line);
        for port in &ctx.ports {
            let in_scope = scope.is_none_or(|s| port.entity == *s);
            if !in_scope {
                continue;
            }
            let type_lower = port.type_str.to_lowercase();
            if type_lower.contains("clock") {
                completions.push(CompletionItem {
                    label: format!("{}.rise", port.name),
                    kind: Some(CompletionItemKind::VALUE),
                    detail: Some(format!("Rising edge of {}", port.name)),
                    ..Default::default()
                });
                completions.push(CompletionItem {
                    label: format!("{}.fall", port.name),
                    kind: Some(CompletionItemKind::VALUE),
                    detail: Some(format!("Falling edge of {}", port.name)),
                    ..Default::default()
                });
            } else if type_lower.contains("reset") {
                completions.push(CompletionItem {
                    label: format!("{}.active", port.name),
                    kind: Some(CompletionItemKind::VALUE),
                    detail: Some(format!("{} active", port.name)),
                    ..Default::default()
                });
                completions.push(CompletionItem {
                    label: format!("{}.inactive", port.name),
                    kind: Some(CompletionItemKind::VALUE),
                    detail: Some(format!("{} inactive", port.name)),
                    ..Default::default()
                });
            }
        }
    }

    // Fallback generic completions if no context or no ports found
    if completions.is_empty() {
        completions.extend(vec![
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
        ]);
    }

    completions
}

/// Get type completions — built-in types + user-defined types from analysis context
fn get_type_completions(analysis: Option<&AnalysisContext>) -> Vec<CompletionItem> {
    let mut completions = vec![
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
            label: "int<>".to_string(),
            kind: Some(CompletionItemKind::CLASS),
            detail: Some("Signed integer type".to_string()),
            insert_text: Some("int<${1:width}>".to_string()),
            insert_text_format: Some(InsertTextFormat::SNIPPET),
            ..Default::default()
        },
        CompletionItem {
            label: "nat<>".to_string(),
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
            label: "fp32".to_string(),
            kind: Some(CompletionItemKind::CLASS),
            detail: Some("32-bit IEEE 754 floating point".to_string()),
            ..Default::default()
        },
        CompletionItem {
            label: "fp16".to_string(),
            kind: Some(CompletionItemKind::CLASS),
            detail: Some("16-bit IEEE 754 floating point".to_string()),
            ..Default::default()
        },
    ];

    // Add user-defined types from analysis context
    if let Some(ctx) = analysis {
        // Type aliases (e.g., MilliVolts, Amps)
        for ta in &ctx.type_aliases {
            let detail = ta.type_str.as_deref().unwrap_or("type alias");
            let kind = if detail.starts_with("struct") {
                CompletionItemKind::STRUCT
            } else if detail.starts_with("enum") {
                CompletionItemKind::ENUM
            } else {
                CompletionItemKind::CLASS
            };
            completions.push(CompletionItem {
                label: ta.name.clone(),
                kind: Some(kind),
                detail: Some(detail.to_string()),
                ..Default::default()
            });
        }

        // Entity names (can be used as types for ports)
        for entity in &ctx.entities {
            completions.push(CompletionItem {
                label: entity.name.clone(),
                kind: Some(CompletionItemKind::CLASS),
                detail: Some("entity".to_string()),
                ..Default::default()
            });
        }
    }

    completions
}

/// Get expression completions: signals, ports, constants, instances, enum variants in scope
fn get_expression_completions(
    ctx: &AnalysisContext,
    cursor_line: u32,
    filter: Option<&str>,
) -> Vec<CompletionItem> {
    let mut completions = Vec::new();
    let scope = ctx.scope_map.get(&cursor_line);

    // Helper: check if item matches the typing filter
    let matches_filter = |name: &str| -> bool {
        match filter {
            Some(f) if !f.is_empty() => name.to_lowercase().starts_with(&f.to_lowercase()),
            _ => true,
        }
    };

    // Signals in scope
    for signal in &ctx.signals {
        if !matches_filter(&signal.name) {
            continue;
        }
        let in_scope = match (&signal.scope, scope) {
            (Some(sig_scope), Some(cur_scope)) => sig_scope == cur_scope,
            (None, _) => true,
            _ => true,
        };
        if in_scope {
            let type_str = signal.type_str.as_deref().unwrap_or("signal");
            completions.push(CompletionItem {
                label: signal.name.clone(),
                kind: Some(CompletionItemKind::VARIABLE),
                detail: Some(format!("signal: {}", type_str)),
                ..Default::default()
            });
        }
    }

    // Ports in scope
    for port in &ctx.ports {
        if !matches_filter(&port.name) {
            continue;
        }
        let in_scope = scope.is_none_or(|s| port.entity == *s);
        if in_scope {
            let icon = match port.direction.as_str() {
                "in" => CompletionItemKind::FIELD,
                "out" => CompletionItemKind::FIELD,
                _ => CompletionItemKind::FIELD,
            };
            completions.push(CompletionItem {
                label: port.name.clone(),
                kind: Some(icon),
                detail: Some(format!("{} port: {}", port.direction, port.type_str)),
                ..Default::default()
            });
        }
    }

    // Constants
    for constant in &ctx.constants {
        if !matches_filter(&constant.name) {
            continue;
        }
        let detail = constant.type_str.as_deref().unwrap_or("const");
        completions.push(CompletionItem {
            label: constant.name.clone(),
            kind: Some(CompletionItemKind::CONSTANT),
            detail: Some(format!("const: {}", detail)),
            ..Default::default()
        });
    }

    // Generic parameters in scope
    for generic in &ctx.generics {
        if !matches_filter(&generic.name) {
            continue;
        }
        let in_scope = scope.is_none_or(|s| generic.entity == *s);
        if in_scope {
            let default = generic
                .default_value
                .as_deref()
                .map(|v| format!(" = {}", v))
                .unwrap_or_default();
            completions.push(CompletionItem {
                label: generic.name.clone(),
                kind: Some(CompletionItemKind::TYPE_PARAMETER),
                detail: Some(format!("generic: {}{}", generic.type_str, default)),
                ..Default::default()
            });
        }
    }

    // Instances in scope
    for instance in &ctx.instances {
        if !matches_filter(&instance.name) {
            continue;
        }
        let in_scope = scope.is_none_or(|s| instance.scope == *s);
        if in_scope {
            completions.push(CompletionItem {
                label: instance.name.clone(),
                kind: Some(CompletionItemKind::VARIABLE),
                detail: Some(format!("instance: {}", instance.entity_type)),
                ..Default::default()
            });
        }
    }

    // Enum variants (from type_aliases with enum type)
    for ta in &ctx.type_aliases {
        let underlying = ta.type_str.as_deref().unwrap_or("");
        if underlying.starts_with("enum {") {
            // Offer EnumName::Variant completions
            if let Some(inner) = underlying
                .strip_prefix("enum { ")
                .and_then(|s| s.strip_suffix(" }"))
            {
                for variant in inner.split(", ") {
                    let variant_name = variant.split(['=', ':']).next().unwrap_or(variant).trim();
                    let qualified = format!("{}::{}", ta.name, variant_name);
                    if !matches_filter(&qualified)
                        && !matches_filter(variant_name)
                        && !matches_filter(&ta.name)
                    {
                        continue;
                    }
                    completions.push(CompletionItem {
                        label: qualified,
                        kind: Some(CompletionItemKind::ENUM_MEMBER),
                        detail: Some(format!("variant of {}", ta.name)),
                        ..Default::default()
                    });
                }
            }
        }
    }

    // Entity names (for `let x = Entity(...)` instantiation)
    for entity in &ctx.entities {
        if !matches_filter(&entity.name) {
            continue;
        }
        completions.push(CompletionItem {
            label: entity.name.clone(),
            kind: Some(CompletionItemKind::CLASS),
            detail: Some("entity".to_string()),
            insert_text: Some(format!("{}($0)", entity.name)),
            insert_text_format: Some(InsertTextFormat::SNIPPET),
            ..Default::default()
        });
    }

    completions
}

/// Get member completions after a dot — instance ports, struct fields, clock/reset events
fn get_member_completions(
    before_dot: Option<&str>,
    analysis: Option<&AnalysisContext>,
    cursor_line: u32,
) -> Vec<CompletionItem> {
    let mut completions = Vec::new();

    if let (Some(name), Some(ctx)) = (before_dot, analysis) {
        let scope = ctx.scope_map.get(&cursor_line);

        // Check if it's an instance — suggest ports of that entity type
        if let Some(inst) = ctx.instances.iter().find(|i| i.name == name) {
            let entity_type = &inst.entity_type;
            for port in &ctx.ports {
                if port.entity == *entity_type {
                    completions.push(CompletionItem {
                        label: port.name.clone(),
                        kind: Some(CompletionItemKind::FIELD),
                        detail: Some(format!("{}: {}", port.direction, port.type_str)),
                        ..Default::default()
                    });
                }
            }
        }

        // Check if it's a port — suggest .rise/.fall for clock, .active/.inactive for reset
        if let Some(port) = ctx.ports.iter().find(|p| p.name == name) {
            let type_lower = port.type_str.to_lowercase();
            if type_lower.contains("clock") {
                completions.push(CompletionItem {
                    label: "rise".to_string(),
                    kind: Some(CompletionItemKind::FIELD),
                    detail: Some("Rising edge event".to_string()),
                    ..Default::default()
                });
                completions.push(CompletionItem {
                    label: "fall".to_string(),
                    kind: Some(CompletionItemKind::FIELD),
                    detail: Some("Falling edge event".to_string()),
                    ..Default::default()
                });
            } else if type_lower.contains("reset") {
                completions.push(CompletionItem {
                    label: "active".to_string(),
                    kind: Some(CompletionItemKind::FIELD),
                    detail: Some("Active state".to_string()),
                    ..Default::default()
                });
                completions.push(CompletionItem {
                    label: "inactive".to_string(),
                    kind: Some(CompletionItemKind::FIELD),
                    detail: Some("Inactive state".to_string()),
                    ..Default::default()
                });
            }
        }

        // Check if it's a signal with a struct type — suggest struct fields
        if let Some(signal) = ctx.signals.iter().find(|s| s.name == name) {
            if let Some(ref type_name) = signal.type_str {
                if let Some(ta) = ctx.type_aliases.iter().find(|t| t.name == *type_name) {
                    if let Some(ref underlying) = ta.type_str {
                        if underlying.starts_with("struct {") {
                            if let Some(inner) = underlying
                                .strip_prefix("struct { ")
                                .and_then(|s| s.strip_suffix(" }"))
                            {
                                for field in inner.split(", ") {
                                    let parts: Vec<&str> = field.splitn(2, ':').collect();
                                    let fname = parts[0].trim();
                                    let ftype = parts.get(1).map(|t| t.trim()).unwrap_or("unknown");
                                    completions.push(CompletionItem {
                                        label: fname.to_string(),
                                        kind: Some(CompletionItemKind::FIELD),
                                        detail: Some(ftype.to_string()),
                                        ..Default::default()
                                    });
                                }
                            }
                        }
                    }
                }
            }
        }

        // Also check ports with struct types
        if let Some(port) = ctx.ports.iter().find(|p| p.name == name) {
            if let Some(ta) = ctx.type_aliases.iter().find(|t| t.name == port.type_str) {
                if let Some(ref underlying) = ta.type_str {
                    if underlying.starts_with("struct {") {
                        if let Some(inner) = underlying
                            .strip_prefix("struct { ")
                            .and_then(|s| s.strip_suffix(" }"))
                        {
                            for field in inner.split(", ") {
                                let parts: Vec<&str> = field.splitn(2, ':').collect();
                                let fname = parts[0].trim();
                                let ftype = parts.get(1).map(|t| t.trim()).unwrap_or("unknown");
                                completions.push(CompletionItem {
                                    label: fname.to_string(),
                                    kind: Some(CompletionItemKind::FIELD),
                                    detail: Some(ftype.to_string()),
                                    ..Default::default()
                                });
                            }
                        }
                    }
                }
            }
        }
    }

    // Fallback generic completions if no context matches
    if completions.is_empty() {
        completions.extend(vec![
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
        ]);
    }

    completions
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
            analysis: None,
        };

        let completions = get_completions(
            &doc,
            Position {
                line: 0,
                character: 0,
            },
            None,
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
            analysis: None,
        };

        let completions = get_completions(
            &doc,
            Position {
                line: 0,
                character: 3,
            },
            None,
        );
        assert!(completions.iter().any(|c| c.label == "clock.rise"));
        assert!(completions.iter().any(|c| c.label == "reset.active"));
    }

    #[test]
    fn test_context_aware_type_completions() {
        let source = "entity Foo {\n  in clk: clock\n}\npub type MilliVolts = int<16>\n";
        let ctx = AnalysisContext::from_source(source);

        let doc = DocumentState {
            content: Rope::from_str("  signal x: "),
            version: 1,
            ast: None,
            diagnostics: Vec::new(),
            analysis: Some(ctx.clone()),
        };

        let completions = get_completions(
            &doc,
            Position {
                line: 0,
                character: 12,
            },
            Some(&ctx),
        );
        // Should have built-in types
        assert!(completions.iter().any(|c| c.label == "bit"));
        assert!(completions.iter().any(|c| c.label == "fp32"));
        // Should also have user-defined type
        assert!(completions.iter().any(|c| c.label == "MilliVolts"));
    }

    #[test]
    fn test_context_aware_event_completions() {
        let source =
            "entity Foo {\n  in sys_clk: clock\n  in rst_n: reset\n}\nimpl Foo {\n  on(\n}";
        let ctx = AnalysisContext::from_source(source);

        let doc = DocumentState {
            content: Rope::from_str(source),
            version: 1,
            ast: None,
            diagnostics: Vec::new(),
            analysis: Some(ctx.clone()),
        };

        let completions = get_completions(
            &doc,
            Position {
                line: 5,
                character: 5,
            },
            Some(&ctx),
        );
        // Should suggest actual port names
        assert!(completions.iter().any(|c| c.label == "sys_clk.rise"));
        assert!(completions.iter().any(|c| c.label == "rst_n.active"));
    }

    #[test]
    fn test_expression_completions() {
        let source = "entity Foo {\n  in x: bit<8>\n  out y: bit<8>\n}\nimpl Foo {\n  signal temp: nat<8>\n  const MAX: nat<8> = 255\n  y <= \n}";
        let ctx = AnalysisContext::from_source(source);

        let completions = get_expression_completions(&ctx, 7, None);
        // Should suggest signals, ports, constants in scope
        assert!(completions.iter().any(|c| c.label == "temp"));
        assert!(completions.iter().any(|c| c.label == "x"));
        assert!(completions.iter().any(|c| c.label == "y"));
        assert!(completions.iter().any(|c| c.label == "MAX"));
    }
}
