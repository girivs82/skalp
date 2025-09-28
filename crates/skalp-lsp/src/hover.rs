//! Hover information provider for SKALP

use tower_lsp::lsp_types::{Hover, HoverContents, MarkupContent, MarkupKind, Position};
use crate::DocumentState;

/// Get hover information for a position in the document
pub fn get_hover(doc: &DocumentState, position: Position) -> Option<Hover> {
    let line_idx = position.line as usize;
    let char_pos = position.character as usize;

    // Get the word at the current position
    if line_idx >= doc.content.len_lines() {
        return None;
    }

    let line = doc.content.line(line_idx);
    let line_str = line.as_str().unwrap_or("");

    // Find word boundaries
    let word = get_word_at_position(line_str, char_pos)?;

    // Generate hover content based on the word
    let hover_content = generate_hover_content(&word);

    hover_content.map(|content| Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value: content,
        }),
        range: None,
    })
}

/// Extract the word at a given position in a line
fn get_word_at_position(line: &str, pos: usize) -> Option<String> {
    if pos > line.len() {
        return None;
    }

    let chars: Vec<char> = line.chars().collect();

    // Find word start
    let mut start = pos;
    while start > 0 && is_word_char(chars.get(start - 1)?) {
        start -= 1;
    }

    // Find word end
    let mut end = pos;
    while end < chars.len() && is_word_char(&chars[end]) {
        end += 1;
    }

    if start == end {
        return None;
    }

    Some(line[start..end].to_string())
}

/// Check if a character is part of a word
fn is_word_char(c: &char) -> bool {
    c.is_alphanumeric() || *c == '_'
}

/// Generate hover content for different SKALP constructs
fn generate_hover_content(word: &str) -> Option<String> {
    match word {
        // Keywords
        "entity" => Some(format!(
            "## entity\n\n\
            Defines a hardware entity (module) in SKALP.\n\n\
            ### Syntax\n\
            ```skalp\n\
            entity MyEntity {{\n\
                in clk: clock;\n\
                in data: bit<8>;\n\
                out result: bit<8>;\n\
            }}\n\
            ```"
        )),

        "impl" => Some(format!(
            "## impl\n\n\
            Implementation block for an entity.\n\n\
            ### Syntax\n\
            ```skalp\n\
            impl MyEntity {{\n\
                signal internal: bit<8>;\n\
                \n\
                on(clk.rise) {{\n\
                    result <= data + 1;\n\
                }}\n\
            }}\n\
            ```"
        )),

        "on" => Some(format!(
            "## on\n\n\
            Event-driven block that executes on specific events.\n\n\
            ### Common Events\n\
            - `clock.rise` - Rising edge of clock\n\
            - `clock.fall` - Falling edge of clock\n\
            - `reset.active` - Reset signal active\n\
            - `reset.inactive` - Reset signal inactive\n\n\
            ### Example\n\
            ```skalp\n\
            on(clk.rise) {{\n\
                counter <= counter + 1;\n\
            }}\n\
            ```"
        )),

        "signal" => Some(format!(
            "## signal\n\n\
            Declares an internal signal within an entity.\n\n\
            ### Syntax\n\
            ```skalp\n\
            signal name: type;\n\
            signal counter: nat<8> = 0;\n\
            ```"
        )),

        "protocol" => Some(format!(
            "## protocol\n\n\
            Defines a communication protocol interface.\n\n\
            ### Syntax\n\
            ```skalp\n\
            protocol AXI4 {{\n\
                signal valid: bit;\n\
                signal ready: bit;\n\
                signal data: bit<32>;\n\
            }}\n\
            ```"
        )),

        "match" => Some(format!(
            "## match\n\n\
            Pattern matching for conditional logic.\n\n\
            ### Syntax\n\
            ```skalp\n\
            match state {{\n\
                IDLE => next_state = START;\n\
                START => next_state = PROCESS;\n\
                _ => next_state = IDLE;\n\
            }}\n\
            ```"
        )),

        "assert" => Some(format!(
            "## assert\n\n\
            Assertion for verification and validation.\n\n\
            ### Types\n\
            - Immediate assertions: Check conditions instantly\n\
            - Concurrent assertions: Check temporal properties\n\n\
            ### Example\n\
            ```skalp\n\
            assert data != 0;\n\
            assert property (req |-> ##[1:3] ack);\n\
            ```"
        )),

        "intent" => Some(format!(
            "## @intent\n\n\
            Design intent annotation for optimization hints.\n\n\
            ### Syntax\n\
            ```skalp\n\
            @intent(\"low_power\")\n\
            @intent(\"high_performance\")\n\
            @intent(\"minimize_area\")\n\
            ```"
        )),

        // Types
        "bit" => Some(format!(
            "## bit\n\n\
            Single bit type (0 or 1).\n\n\
            ### Bit Vector\n\
            ```skalp\n\
            bit<8>  // 8-bit vector\n\
            bit<32> // 32-bit vector\n\
            ```"
        )),

        "logic" => Some(format!(
            "## logic\n\n\
            4-state logic type (0, 1, X, Z).\n\n\
            ### Logic Vector\n\
            ```skalp\n\
            logic<8>  // 8-bit logic vector\n\
            logic<32> // 32-bit logic vector\n\
            ```"
        )),

        "int" => Some(format!(
            "## int\n\n\
            Signed integer type.\n\n\
            ### Syntax\n\
            ```skalp\n\
            int<8>  // 8-bit signed integer (-128 to 127)\n\
            int<32> // 32-bit signed integer\n\
            ```"
        )),

        "nat" => Some(format!(
            "## nat\n\n\
            Natural number (unsigned integer) type.\n\n\
            ### Syntax\n\
            ```skalp\n\
            nat<8>  // 8-bit unsigned (0 to 255)\n\
            nat<32> // 32-bit unsigned\n\
            ```"
        )),

        "clock" => Some(format!(
            "## clock\n\n\
            Clock signal type with domain safety.\n\n\
            ### Clock Domains\n\
            ```skalp\n\
            in clk: clock<'sys>;\n\
            in clk_fast: clock<'fast>;\n\
            ```"
        )),

        "reset" => Some(format!(
            "## reset\n\n\
            Reset signal type with polarity.\n\n\
            ### Example\n\
            ```skalp\n\
            in rst_n: reset(active_low);\n\
            in rst: reset(active_high);\n\
            ```"
        )),

        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ropey::Rope;

    #[test]
    fn test_get_word_at_position() {
        let line = "entity Counter {";
        assert_eq!(get_word_at_position(line, 0), Some("entity".to_string()));
        assert_eq!(get_word_at_position(line, 7), Some("Counter".to_string()));
        assert_eq!(get_word_at_position(line, 15), None); // on '{'
    }

    #[test]
    fn test_hover_content() {
        assert!(generate_hover_content("entity").is_some());
        assert!(generate_hover_content("bit").is_some());
        assert!(generate_hover_content("unknown").is_none());
    }

    #[test]
    fn test_get_hover() {
        let doc = DocumentState {
            content: Rope::from_str("entity Counter {\n    in clk: clock;\n}"),
            version: 1,
            ast: None,
            diagnostics: Vec::new(),
        };

        let hover = get_hover(&doc, Position { line: 0, character: 3 });
        assert!(hover.is_some());

        if let Some(hover) = hover {
            if let HoverContents::Markup(markup) = hover.contents {
                assert!(markup.value.contains("entity"));
            }
        }
    }
}