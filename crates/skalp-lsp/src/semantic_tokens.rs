//! Semantic token provider for SKALP
//!
//! Provides semantic highlighting by classifying tokens via text-based parsing.
//! Token types: entity, signal, port, type, trait, constant, variable, function, keyword

use tower_lsp::lsp_types::*;

/// SKALP semantic token types (indices into the legend)
pub const TOKEN_ENTITY: u32 = 0;
pub const TOKEN_SIGNAL: u32 = 1;
pub const TOKEN_PORT: u32 = 2;
pub const TOKEN_TYPE: u32 = 3;
pub const TOKEN_TRAIT: u32 = 4;
pub const TOKEN_CONSTANT: u32 = 5;
pub const TOKEN_VARIABLE: u32 = 6;
pub const TOKEN_FUNCTION: u32 = 7;
pub const TOKEN_KEYWORD: u32 = 8;
pub const TOKEN_OPERATOR: u32 = 9;
pub const TOKEN_NUMBER: u32 = 10;
pub const TOKEN_COMMENT: u32 = 11;

/// Token modifier bits
pub const MOD_DECLARATION: u32 = 0;
pub const MOD_DEFINITION: u32 = 1;
pub const MOD_READONLY: u32 = 2;

pub fn semantic_token_legend() -> SemanticTokensLegend {
    SemanticTokensLegend {
        token_types: vec![
            SemanticTokenType::CLASS,        // 0: entity
            SemanticTokenType::VARIABLE,     // 1: signal
            SemanticTokenType::PROPERTY,     // 2: port
            SemanticTokenType::TYPE,         // 3: type
            SemanticTokenType::INTERFACE,    // 4: trait
            SemanticTokenType::new("constant"), // 5: constant
            SemanticTokenType::VARIABLE,     // 6: variable
            SemanticTokenType::FUNCTION,     // 7: function
            SemanticTokenType::KEYWORD,      // 8: keyword
            SemanticTokenType::OPERATOR,     // 9: operator
            SemanticTokenType::NUMBER,       // 10: number
            SemanticTokenType::COMMENT,      // 11: comment
        ],
        token_modifiers: vec![
            SemanticTokenModifier::DECLARATION,
            SemanticTokenModifier::DEFINITION,
            SemanticTokenModifier::READONLY,
        ],
    }
}

/// Generate semantic tokens for a SKALP document
pub fn get_semantic_tokens(content: &str) -> Vec<SemanticToken> {
    let mut tokens = Vec::new();
    let mut prev_line = 0u32;
    let mut prev_start = 0u32;

    // Known keywords
    let keywords: &[&str] = &[
        "entity", "impl", "on", "signal", "in", "out", "inout", "let", "if", "else",
        "match", "for", "while", "return", "use", "import", "fn", "trait", "protocol",
        "generate", "async", "const", "type", "struct", "enum", "pub", "mod",
        "true", "false", "as", "ref", "mut",
    ];

    // Known types
    let types: &[&str] = &[
        "bit", "nat", "int", "bool", "clock", "reset", "logic",
        "fp16", "fp32", "fp64", "IEEE754_16",
    ];

    // Track declarations for context
    let mut in_entity_decl = false;
    let mut in_port_section = false;

    for (line_num, line) in content.lines().enumerate() {
        let line_u32 = line_num as u32;
        let trimmed = line.trim();

        // Skip empty lines
        if trimmed.is_empty() {
            continue;
        }

        // Comment lines
        if trimmed.starts_with("//") {
            let start = line.find("//").unwrap() as u32;
            push_token(
                &mut tokens,
                &mut prev_line,
                &mut prev_start,
                line_u32,
                start,
                line.len() as u32 - start,
                TOKEN_COMMENT,
                0,
            );
            continue;
        }

        // Tokenize the line
        let chars: Vec<char> = line.chars().collect();
        let mut i = 0;

        while i < chars.len() {
            // Skip whitespace
            if chars[i].is_whitespace() {
                i += 1;
                continue;
            }

            // Inline comments
            if i + 1 < chars.len() && chars[i] == '/' && chars[i + 1] == '/' {
                push_token(
                    &mut tokens,
                    &mut prev_line,
                    &mut prev_start,
                    line_u32,
                    i as u32,
                    (chars.len() - i) as u32,
                    TOKEN_COMMENT,
                    0,
                );
                break;
            }

            // Numbers (decimal and hex literals)
            if chars[i].is_ascii_digit()
                || (chars[i] == '0' && i + 1 < chars.len() && (chars[i + 1] == 'x' || chars[i + 1] == 'b'))
            {
                let start = i;
                while i < chars.len()
                    && (chars[i].is_ascii_alphanumeric() || chars[i] == '_' || chars[i] == '.')
                {
                    i += 1;
                }
                // Check for type suffix like fp32, fp16
                let word: String = chars[start..i].iter().collect();
                if word.ends_with("fp32") || word.ends_with("fp16") || word.ends_with("fp64") {
                    push_token(
                        &mut tokens,
                        &mut prev_line,
                        &mut prev_start,
                        line_u32,
                        start as u32,
                        (i - start) as u32,
                        TOKEN_NUMBER,
                        0,
                    );
                } else {
                    push_token(
                        &mut tokens,
                        &mut prev_line,
                        &mut prev_start,
                        line_u32,
                        start as u32,
                        (i - start) as u32,
                        TOKEN_NUMBER,
                        0,
                    );
                }
                continue;
            }

            // Identifiers and keywords
            if chars[i].is_alphabetic() || chars[i] == '_' {
                let start = i;
                while i < chars.len() && (chars[i].is_alphanumeric() || chars[i] == '_') {
                    i += 1;
                }
                let word: String = chars[start..i].iter().collect();

                let (token_type, modifiers) = classify_word(
                    &word,
                    keywords,
                    types,
                    trimmed,
                    start,
                    &mut in_entity_decl,
                    &mut in_port_section,
                );

                push_token(
                    &mut tokens,
                    &mut prev_line,
                    &mut prev_start,
                    line_u32,
                    start as u32,
                    word.len() as u32,
                    token_type,
                    modifiers,
                );
                continue;
            }

            // Operators
            if "<>=!&|^~+-*/%".contains(chars[i]) {
                let start = i;
                i += 1;
                // Multi-char operators
                if i < chars.len() && "<>=".contains(chars[i]) {
                    i += 1;
                }
                push_token(
                    &mut tokens,
                    &mut prev_line,
                    &mut prev_start,
                    line_u32,
                    start as u32,
                    (i - start) as u32,
                    TOKEN_OPERATOR,
                    0,
                );
                continue;
            }

            i += 1;
        }

        // Track entity context
        if trimmed.starts_with("entity ") {
            in_entity_decl = true;
            in_port_section = true;
        }
        if trimmed == "}" {
            in_entity_decl = false;
            in_port_section = false;
        }
        if trimmed.starts_with("impl ") {
            in_entity_decl = false;
            in_port_section = false;
        }
    }

    tokens
}

fn classify_word(
    word: &str,
    keywords: &[&str],
    types: &[&str],
    line: &str,
    _pos: usize,
    in_entity_decl: &mut bool,
    in_port_section: &mut bool,
) -> (u32, u32) {
    // Keywords
    if keywords.contains(&word) {
        // Check if it's a declaration keyword
        if word == "entity" || word == "trait" || word == "protocol" {
            return (TOKEN_KEYWORD, 1 << MOD_DEFINITION);
        }
        if word == "signal" || word == "let" || word == "const" {
            return (TOKEN_KEYWORD, 1 << MOD_DECLARATION);
        }
        return (TOKEN_KEYWORD, 0);
    }

    // Types
    if types.contains(&word) {
        return (TOKEN_TYPE, 0);
    }

    // Context-based classification
    let trimmed = line.trim();

    // Entity name (after "entity" keyword)
    if trimmed.starts_with("entity ") && trimmed.split_whitespace().nth(1).map(|s| s.trim_end_matches(|c: char| !c.is_alphanumeric() && c != '_')) == Some(word) {
        return (TOKEN_ENTITY, 1 << MOD_DEFINITION);
    }

    // Impl target (after "impl" keyword)
    if trimmed.starts_with("impl ") && trimmed.split_whitespace().nth(1).map(|s| s.trim_end_matches(|c: char| !c.is_alphanumeric() && c != '_')) == Some(word) {
        return (TOKEN_ENTITY, 0);
    }

    // Port declaration
    if (trimmed.starts_with("in ") || trimmed.starts_with("out ") || trimmed.starts_with("inout ")) && *in_port_section {
        // The name after in/out/inout
        if trimmed.split_whitespace().nth(1).map(|s| s.trim_end_matches(':')) == Some(word) {
            return (TOKEN_PORT, 1 << MOD_DECLARATION);
        }
        // Type after ":"
        if trimmed.contains(':') {
            let after_colon = trimmed.split(':').nth(1).unwrap_or("").trim();
            if after_colon.starts_with(word) {
                return (TOKEN_TYPE, 0);
            }
        }
    }

    // Signal name (after "signal" keyword)
    if trimmed.starts_with("signal ") {
        if trimmed.split_whitespace().nth(1).map(|s| s.trim_end_matches(':')) == Some(word) {
            return (TOKEN_SIGNAL, 1 << MOD_DECLARATION);
        }
    }

    // Constant (after "const" keyword or ALL_CAPS)
    if trimmed.starts_with("const ") {
        if trimmed.split_whitespace().nth(1).map(|s| s.trim_end_matches(':')) == Some(word) {
            return (TOKEN_CONSTANT, 1 << MOD_DECLARATION | 1 << MOD_READONLY);
        }
    }
    if word.chars().all(|c| c.is_ascii_uppercase() || c == '_') && word.len() > 1 {
        return (TOKEN_CONSTANT, 1 << MOD_READONLY);
    }

    // Trait name (after "trait" keyword)
    if trimmed.starts_with("trait ") {
        if trimmed.split_whitespace().nth(1).map(|s| s.trim_end_matches(|c: char| !c.is_alphanumeric() && c != '_')) == Some(word) {
            return (TOKEN_TRAIT, 1 << MOD_DEFINITION);
        }
    }

    // Function call (word followed by "(")
    if line.get(_pos + word.len().._pos + word.len() + 1) == Some("(") {
        return (TOKEN_FUNCTION, 0);
    }

    // Let binding
    if trimmed.starts_with("let ") {
        if trimmed.split_whitespace().nth(1).map(|s| s.trim_end_matches(|c: char| !c.is_alphanumeric() && c != '_')) == Some(word) {
            return (TOKEN_VARIABLE, 1 << MOD_DECLARATION);
        }
    }

    // PascalCase → likely entity/type reference
    if word.len() > 1 && word.chars().next().unwrap().is_uppercase() && word.contains(|c: char| c.is_lowercase()) {
        return (TOKEN_ENTITY, 0);
    }

    // Default: variable
    (TOKEN_VARIABLE, 0)
}

fn push_token(
    tokens: &mut Vec<SemanticToken>,
    prev_line: &mut u32,
    prev_start: &mut u32,
    line: u32,
    start: u32,
    length: u32,
    token_type: u32,
    token_modifiers_bitset: u32,
) {
    let delta_line = line - *prev_line;
    let delta_start = if delta_line == 0 {
        start - *prev_start
    } else {
        start
    };

    tokens.push(SemanticToken {
        delta_line,
        delta_start,
        length,
        token_type,
        token_modifiers_bitset,
    });

    *prev_line = line;
    *prev_start = start;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_semantic_tokens_basic() {
        let source = "entity Counter {\n  in clk: clock;\n  out count: nat<8>;\n}";
        let tokens = get_semantic_tokens(source);
        assert!(!tokens.is_empty());
    }

    #[test]
    fn test_token_legend() {
        let legend = semantic_token_legend();
        assert!(!legend.token_types.is_empty());
        assert!(!legend.token_modifiers.is_empty());
    }
}
