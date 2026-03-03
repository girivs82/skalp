//! Hover information provider for SKALP

use crate::{DocumentState, FileLanguage};
use tower_lsp::lsp_types::{Hover, HoverContents, MarkupContent, MarkupKind, Position};

/// Get hover information for a position in the document
pub fn get_hover(doc: &DocumentState, position: Position, language: FileLanguage) -> Option<Hover> {
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

    // Generate hover content based on the word and language
    let hover_content = match language {
        FileLanguage::Vhdl => generate_vhdl_hover_content(&word).or_else(|| generate_hover_content(&word)),
        FileLanguage::Skalp => generate_hover_content(&word),
    };

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
        "entity" => Some(
            "## entity\n\n\
            Defines a hardware entity (module) in SKALP.\n\n\
            ### Syntax\n\
            ```skalp\n\
            entity MyEntity {\n\
                in clk: clock;\n\
                in data: bit<8>;\n\
                out result: bit<8>;\n\
            }\n\
            ```"
            .to_string(),
        ),

        "impl" => Some(
            "## impl\n\n\
            Implementation block for an entity.\n\n\
            ### Syntax\n\
            ```skalp\n\
            impl MyEntity {\n\
                signal internal: bit<8>;\n\
                \n\
                on(clk.rise) {\n\
                    result <= data + 1;\n\
                }\n\
            }\n\
            ```"
            .to_string(),
        ),

        "on" => Some(
            "## on\n\n\
            Event-driven block that executes on specific events.\n\n\
            ### Common Events\n\
            - `clock.rise` - Rising edge of clock\n\
            - `clock.fall` - Falling edge of clock\n\
            - `reset.active` - Reset signal active\n\
            - `reset.inactive` - Reset signal inactive\n\n\
            ### Example\n\
            ```skalp\n\
            on(clk.rise) {\n\
                counter <= counter + 1;\n\
            }\n\
            ```"
            .to_string(),
        ),

        "signal" => Some(
            "## signal\n\n\
            Declares an internal signal within an entity.\n\n\
            ### Syntax\n\
            ```skalp\n\
            signal name: type;\n\
            signal counter: nat<8> = 0;\n\
            ```"
            .to_string(),
        ),

        "protocol" => Some(
            "## protocol\n\n\
            Defines a communication protocol interface.\n\n\
            ### Syntax\n\
            ```skalp\n\
            protocol AXI4 {\n\
                signal valid: bit;\n\
                signal ready: bit;\n\
                signal data: bit<32>;\n\
            }\n\
            ```"
            .to_string(),
        ),

        "match" => Some(
            "## match\n\n\
            Pattern matching for conditional logic.\n\n\
            ### Syntax\n\
            ```skalp\n\
            match state {\n\
                IDLE => next_state = START;\n\
                START => next_state = PROCESS;\n\
                _ => next_state = IDLE;\n\
            }\n\
            ```"
            .to_string(),
        ),

        "assert" => Some(
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
            .to_string(),
        ),

        "intent" => Some(
            "## @intent\n\n\
            Design intent annotation for optimization hints.\n\n\
            ### Syntax\n\
            ```skalp\n\
            @intent(\"low_power\")\n\
            @intent(\"high_performance\")\n\
            @intent(\"minimize_area\")\n\
            ```"
            .to_string(),
        ),

        // Types
        "bit" => Some(
            "## bit\n\n\
            Single bit type (0 or 1).\n\n\
            ### Bit Vector\n\
            ```skalp\n\
            bit<8>  // 8-bit vector\n\
            bit<32> // 32-bit vector\n\
            ```"
            .to_string(),
        ),

        "logic" => Some(
            "## logic\n\n\
            4-state logic type (0, 1, X, Z).\n\n\
            ### Logic Vector\n\
            ```skalp\n\
            logic<8>  // 8-bit logic vector\n\
            logic<32> // 32-bit logic vector\n\
            ```"
            .to_string(),
        ),

        "int" => Some(
            "## int\n\n\
            Signed integer type.\n\n\
            ### Syntax\n\
            ```skalp\n\
            int<8>  // 8-bit signed integer (-128 to 127)\n\
            int<32> // 32-bit signed integer\n\
            ```"
            .to_string(),
        ),

        "nat" => Some(
            "## nat\n\n\
            Natural number (unsigned integer) type.\n\n\
            ### Syntax\n\
            ```skalp\n\
            nat<8>  // 8-bit unsigned (0 to 255)\n\
            nat<32> // 32-bit unsigned\n\
            ```"
            .to_string(),
        ),

        "clock" => Some(
            "## clock\n\n\
            Clock signal type with domain safety.\n\n\
            ### Clock Domains\n\
            ```skalp\n\
            in clk: clock<'sys>;\n\
            in clk_fast: clock<'fast>;\n\
            ```"
            .to_string(),
        ),

        "reset" => Some(
            "## reset\n\n\
            Reset signal type with polarity.\n\n\
            ### Example\n\
            ```skalp\n\
            in rst_n: reset(active_low);\n\
            in rst: reset(active_high);\n\
            ```"
            .to_string(),
        ),

        _ => None,
    }
}

/// Generate hover content for VHDL constructs
fn generate_vhdl_hover_content(word: &str) -> Option<String> {
    let w = word.to_lowercase();
    match w.as_str() {
        "entity" => Some("## entity\n\nVHDL entity declaration. Defines the external interface (ports and generics) of a design unit.\n\n```vhdl\nentity counter is\n  port (\n    clk : in std_logic;\n    count : out unsigned(7 downto 0)\n  );\nend entity counter;\n```".to_string()),
        "architecture" => Some("## architecture\n\nVHDL architecture body. Contains the implementation of an entity.\n\n```vhdl\narchitecture rtl of counter is\nbegin\n  -- implementation\nend architecture rtl;\n```".to_string()),
        "process" => Some("## process\n\nVHDL process statement. A sequential region with a sensitivity list.\n\n```vhdl\nprocess(clk)\nbegin\n  if rising_edge(clk) then\n    count <= count + 1;\n  end if;\nend process;\n```".to_string()),
        "signal" => Some("## signal\n\nVHDL signal declaration. Represents a wire or register.\n\n```vhdl\nsignal data : std_logic_vector(7 downto 0);\nsignal counter : unsigned(7 downto 0) := (others => '0');\n```".to_string()),
        "variable" => Some("## variable\n\nVHDL variable declaration. Used inside processes, updated immediately.\n\n```vhdl\nvariable temp : integer := 0;\n```".to_string()),
        "constant" => Some("## constant\n\nVHDL constant declaration.\n\n```vhdl\nconstant WIDTH : integer := 8;\n```".to_string()),
        "component" => Some("## component\n\nVHDL component declaration. Declares the interface of an entity for structural instantiation.\n\n```vhdl\ncomponent counter is\n  port (clk : in std_logic; count : out unsigned(7 downto 0));\nend component;\n```".to_string()),
        "std_logic" => Some("## std_logic\n\nIEEE 1164 9-valued logic type. Values: '0', '1', 'Z', 'X', 'U', 'W', 'L', 'H', '-'.\n\nMost commonly used signal type in VHDL designs.".to_string()),
        "std_logic_vector" => Some("## std_logic_vector\n\nIEEE 1164 array of std_logic. Unconstrained — must specify range.\n\n```vhdl\nsignal data : std_logic_vector(7 downto 0);\n```".to_string()),
        "unsigned" => Some("## unsigned\n\nIEEE numeric_std unsigned integer type. Supports arithmetic operations.\n\n```vhdl\nlibrary ieee;\nuse ieee.numeric_std.all;\nsignal count : unsigned(7 downto 0);\n```".to_string()),
        "signed" => Some("## signed\n\nIEEE numeric_std signed integer type. Two's complement representation.\n\n```vhdl\nlibrary ieee;\nuse ieee.numeric_std.all;\nsignal value : signed(15 downto 0);\n```".to_string()),
        "rising_edge" => Some("## rising_edge\n\nDetects a rising edge (0→1 transition) on a signal.\n\n```vhdl\nif rising_edge(clk) then\n  -- sequential logic\nend if;\n```".to_string()),
        "falling_edge" => Some("## falling_edge\n\nDetects a falling edge (1→0 transition) on a signal.\n\n```vhdl\nif falling_edge(clk) then\n  -- sequential logic\nend if;\n```".to_string()),
        "generic" => Some("## generic\n\nVHDL generic parameter. Allows parameterization of entities and components.\n\n```vhdl\nentity fifo is\n  generic (\n    WIDTH : integer := 8;\n    DEPTH : integer := 16\n  );\n```".to_string()),
        "port" => Some("## port\n\nVHDL port declaration. Defines the I/O interface of an entity.\n\n```vhdl\nport (\n  clk : in std_logic;\n  data_out : out std_logic_vector(7 downto 0)\n);\n```".to_string()),
        "generate" => Some("## generate\n\nVHDL generate statement. Creates replicated or conditional structure.\n\n```vhdl\ngen: for i in 0 to N-1 generate\n  inst: component_name port map (...);\nend generate gen;\n```".to_string()),
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
            analysis: None,
            language: FileLanguage::Skalp,
        };

        let hover = get_hover(
            &doc,
            Position {
                line: 0,
                character: 3,
            },
            FileLanguage::Skalp,
        );
        assert!(hover.is_some());

        if let Some(hover) = hover {
            if let HoverContents::Markup(markup) = hover.contents {
                assert!(markup.value.contains("entity"));
            }
        }
    }
}
