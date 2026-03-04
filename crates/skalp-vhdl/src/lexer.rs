use logos::Logos;

fn normalize_ident(lex: &mut logos::Lexer<Token>) -> String {
    lex.slice().to_ascii_lowercase()
}

#[derive(Logos, Debug, Clone, PartialEq)]
#[logos(skip r"[ \t\r\n\f]+")]
pub enum Token {
    // ========================================================================
    // VHDL keywords (case-insensitive)
    // ========================================================================

    // Design units
    #[token("entity", ignore(case))]
    Entity,
    #[token("architecture", ignore(case))]
    Architecture,
    #[token("package", ignore(case))]
    Package,
    #[token("body", ignore(case))]
    Body,
    #[token("configuration", ignore(case))]
    Configuration,
    #[token("library", ignore(case))]
    Library,
    #[token("use", ignore(case))]
    Use,

    // Port/generic/component
    #[token("port", ignore(case))]
    Port,
    #[token("generic", ignore(case))]
    Generic,
    #[token("map", ignore(case))]
    Map,
    #[token("component", ignore(case))]
    Component,

    // Directions
    #[token("in", ignore(case))]
    In,
    #[token("out", ignore(case))]
    Out,
    #[token("inout", ignore(case))]
    Inout,
    #[token("buffer", ignore(case))]
    Buffer,

    // Declarations
    #[token("signal", ignore(case))]
    Signal,
    #[token("variable", ignore(case))]
    Variable,
    #[token("constant", ignore(case))]
    Constant,
    #[token("type", ignore(case))]
    Type,
    #[token("subtype", ignore(case))]
    Subtype,
    #[token("alias", ignore(case))]
    Alias,
    #[token("attribute", ignore(case))]
    Attribute,

    // Type constructors
    #[token("array", ignore(case))]
    Array,
    #[token("record", ignore(case))]
    Record,
    #[token("range", ignore(case))]
    Range,

    // Concurrent statements
    #[token("process", ignore(case))]
    Process,
    #[token("begin", ignore(case))]
    Begin,
    #[token("end", ignore(case))]
    End,
    #[token("generate", ignore(case))]
    Generate,
    #[token("block", ignore(case))]
    Block,

    // Sequential statements
    #[token("if", ignore(case))]
    If,
    #[token("then", ignore(case))]
    Then,
    #[token("elsif", ignore(case))]
    Elsif,
    #[token("else", ignore(case))]
    Else,
    #[token("case", ignore(case))]
    Case,
    #[token("when", ignore(case))]
    When,
    #[token("for", ignore(case))]
    For,
    #[token("loop", ignore(case))]
    Loop,
    #[token("while", ignore(case))]
    While,
    #[token("next", ignore(case))]
    Next,
    #[token("exit", ignore(case))]
    Exit,
    #[token("return", ignore(case))]
    Return,
    #[token("null", ignore(case))]
    Null,
    #[token("assert", ignore(case))]
    Assert,
    #[token("report", ignore(case))]
    Report,
    #[token("severity", ignore(case))]
    Severity,

    // Control flow
    #[token("is", ignore(case))]
    Is,
    #[token("of", ignore(case))]
    Of,
    #[token("all", ignore(case))]
    All,
    #[token("others", ignore(case))]
    Others,
    #[token("open", ignore(case))]
    Open,
    #[token("with", ignore(case))]
    With,
    #[token("select", ignore(case))]
    Select,
    #[token("unaffected", ignore(case))]
    Unaffected,

    // Boolean/logical
    #[token("and", ignore(case))]
    And,
    #[token("or", ignore(case))]
    Or,
    #[token("xor", ignore(case))]
    Xor,
    #[token("nand", ignore(case))]
    Nand,
    #[token("nor", ignore(case))]
    Nor,
    #[token("xnor", ignore(case))]
    Xnor,
    #[token("not", ignore(case))]
    Not,
    #[token("mod", ignore(case))]
    Mod,
    #[token("rem", ignore(case))]
    Rem,
    #[token("abs", ignore(case))]
    Abs,

    // Shift operators
    #[token("sll", ignore(case))]
    Sll,
    #[token("srl", ignore(case))]
    Srl,
    #[token("sla", ignore(case))]
    Sla,
    #[token("sra", ignore(case))]
    Sra,
    #[token("rol", ignore(case))]
    Rol,
    #[token("ror", ignore(case))]
    Ror,

    // Range directions
    #[token("to", ignore(case))]
    To,
    #[token("downto", ignore(case))]
    Downto,

    // Functions/procedures
    #[token("function", ignore(case))]
    Function,
    #[token("procedure", ignore(case))]
    Procedure,
    #[token("impure", ignore(case))]
    Impure,
    #[token("pure", ignore(case))]
    Pure,

    // Unsynthesizable (reject at parse time)
    #[token("wait", ignore(case))]
    Wait,
    #[token("after", ignore(case))]
    After,
    #[token("transport", ignore(case))]
    Transport,
    #[token("reject", ignore(case))]
    Reject,
    #[token("file", ignore(case))]
    File,
    #[token("access", ignore(case))]
    Access,
    #[token("shared", ignore(case))]
    Shared,

    // VHDL-2019 interface/view/generics
    #[token("new", ignore(case))]
    New,
    #[token("interface", ignore(case))]
    Interface,
    #[token("view", ignore(case))]
    View,
    #[token("private", ignore(case))]
    Private,

    // Boolean literals
    #[token("true", ignore(case))]
    True,
    #[token("false", ignore(case))]
    False,

    // IEEE standard types (recognized as keywords for clarity in type resolution)
    #[token("std_logic", ignore(case))]
    StdLogic,
    #[token("std_ulogic", ignore(case))]
    StdUlogic,
    #[token("std_logic_vector", ignore(case))]
    StdLogicVector,
    #[token("std_ulogic_vector", ignore(case))]
    StdUlogicVector,
    #[token("unsigned", ignore(case))]
    Unsigned,
    #[token("signed", ignore(case))]
    Signed,
    #[token("boolean", ignore(case))]
    Boolean,
    #[token("integer", ignore(case))]
    Integer,
    #[token("natural", ignore(case))]
    Natural,
    #[token("positive", ignore(case))]
    Positive,
    #[token("real", ignore(case))]
    Real,
    #[token("string", ignore(case))]
    StringKw,
    #[token("bit", ignore(case))]
    Bit,
    #[token("bit_vector", ignore(case))]
    BitVector,

    // Built-in functions (recognized as keywords)
    #[token("rising_edge", ignore(case))]
    RisingEdge,
    #[token("falling_edge", ignore(case))]
    FallingEdge,
    #[token("to_unsigned", ignore(case))]
    ToUnsigned,
    #[token("to_signed", ignore(case))]
    ToSigned,
    #[token("to_integer", ignore(case))]
    ToInteger,
    #[token("resize", ignore(case))]
    Resize,
    #[token("conv_integer", ignore(case))]
    ConvInteger,
    #[token("conv_std_logic_vector", ignore(case))]
    ConvStdLogicVector,

    // ========================================================================
    // Literals
    // ========================================================================

    // Character literal: 'X' (single character in single quotes)
    #[regex(r"'[^']'")]
    CharLiteral,

    // Bit string literals: X"FF", O"77", B"1010"
    #[regex(r#"[xXoObBdD]"[0-9a-fA-F_]+""#)]
    BitStringLiteral,

    // String literal (handles VHDL doubled-quote escaping: "" = literal ")
    #[regex(r#""([^"]|"")*""#)]
    StringLiteral,

    // Based literal: 16#FF#, 2#1010#, 8#77#
    #[regex(r"[0-9]+#[0-9a-fA-F_]+#")]
    BasedLiteral,

    // Decimal literal (integer or real)
    #[regex(r"[0-9][0-9_]*\.[0-9][0-9_]*([eE][+-]?[0-9]+)?")]
    RealLiteral,

    #[regex(r"[0-9][0-9_]*([eE][+-]?[0-9]+)?")]
    IntLiteral,

    // Identifier (normalized to lowercase)
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", normalize_ident)]
    Ident(String),

    // ========================================================================
    // Operators and delimiters
    // ========================================================================
    #[token("<=")]
    SignalAssign, // or LessEqual in expression context
    #[token(":=")]
    VarAssign,
    #[token("=>")]
    Arrow,
    #[token("<>")]
    Box, // unconstrained range
    #[token("**")]
    DoubleStar,
    #[token("/=")]
    NotEqual,
    #[token(">=")]
    GreaterEqual,
    #[token("<<")]
    DoubleLess, // external names
    #[token(">>")]
    DoubleGreater, // external names

    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,
    #[token(",")]
    Comma,
    #[token(";")]
    Semicolon,
    #[token(":")]
    Colon,
    #[token(".")]
    Dot,
    #[token("'")]
    Tick, // attribute access
    #[token("&")]
    Ampersand, // concatenation
    #[token("|")]
    Bar, // choice separator
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("=")]
    Equal,
    #[token("<")]
    LessThan,
    #[token(">")]
    GreaterThan,
    #[token("@")]
    At,

    // ========================================================================
    // Comments (captured for lossless tree)
    // ========================================================================
    #[regex(r"--[^\n]*")]
    Comment,
}

/// Token with position information for building the rowan tree
#[derive(Debug, Clone)]
pub struct TokenWithPos {
    pub token: Token,
    pub text: String,
    pub offset: usize,
}

/// Tokenize VHDL source into a stream of tokens with positions
pub fn tokenize(source: &str) -> Vec<TokenWithPos> {
    let mut tokens = Vec::new();
    let mut lexer = Token::lexer(source);

    while let Some(result) = lexer.next() {
        let span = lexer.span();
        let raw_text = lexer.slice().to_string();
        match result {
            Ok(token) => {
                // For identifiers, use the normalized (lowercased) text so the
                // rowan tree and all downstream consumers see case-insensitive names.
                let text = match &token {
                    Token::Ident(normalized) => normalized.clone(),
                    _ => raw_text,
                };
                tokens.push(TokenWithPos {
                    token,
                    text,
                    offset: span.start,
                });
            }
            Err(()) => {
                // Capture error tokens for lossless tree
                tokens.push(TokenWithPos {
                    token: Token::Ident("error".to_string()),
                    text: raw_text,
                    offset: span.start,
                });
            }
        }
    }

    tokens
}

/// Check if a token is a VHDL keyword (as opposed to identifier)
pub fn is_keyword(token: &Token) -> bool {
    !matches!(
        token,
        Token::Ident(_)
            | Token::IntLiteral
            | Token::RealLiteral
            | Token::StringLiteral
            | Token::CharLiteral
            | Token::BitStringLiteral
            | Token::BasedLiteral
            | Token::Comment
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_case_insensitive_keywords() {
        let tokens = tokenize("ENTITY entity Entity eNtItY");
        assert_eq!(tokens.len(), 4);
        for t in &tokens {
            assert!(matches!(t.token, Token::Entity));
        }
    }

    #[test]
    fn test_identifier_normalization() {
        let tokens = tokenize("MyCounter my_counter MYCOUNTER");
        assert_eq!(tokens.len(), 3);
        for t in &tokens {
            match &t.token {
                Token::Ident(name) => assert_eq!(name, &t.text.to_ascii_lowercase()),
                _ => panic!("expected ident"),
            }
        }
    }

    #[test]
    fn test_vhdl_operators() {
        let tokens = tokenize("<= := => /= >= ** <>");
        let expected = [
            Token::SignalAssign,
            Token::VarAssign,
            Token::Arrow,
            Token::NotEqual,
            Token::GreaterEqual,
            Token::DoubleStar,
            Token::Box,
        ];
        assert_eq!(tokens.len(), expected.len());
        for (t, e) in tokens.iter().zip(expected.iter()) {
            assert_eq!(&t.token, e);
        }
    }

    #[test]
    fn test_bit_string_literals() {
        let tokens = tokenize(r#"X"FF" B"1010" O"77" x"dead_beef""#);
        assert_eq!(tokens.len(), 4);
        for t in &tokens {
            assert!(matches!(t.token, Token::BitStringLiteral));
        }
    }

    #[test]
    fn test_character_literal() {
        let tokens = tokenize("'0' '1' 'Z'");
        assert_eq!(tokens.len(), 3);
        for t in &tokens {
            assert!(matches!(t.token, Token::CharLiteral));
        }
    }

    #[test]
    fn test_based_literals() {
        let tokens = tokenize("16#FF# 2#1010# 8#77#");
        assert_eq!(tokens.len(), 3);
        for t in &tokens {
            assert!(matches!(t.token, Token::BasedLiteral));
        }
    }

    #[test]
    fn test_comment_preservation() {
        let tokens = tokenize("signal clk : std_logic; -- clock signal");
        let comments: Vec<_> = tokens
            .iter()
            .filter(|t| matches!(t.token, Token::Comment))
            .collect();
        assert_eq!(comments.len(), 1);
        assert!(comments[0].text.contains("clock signal"));
    }

    #[test]
    fn test_skalp_pragma_comment() {
        let tokens = tokenize("-- skalp: safety_mechanism(tmr)");
        assert_eq!(tokens.len(), 1);
        assert!(matches!(tokens[0].token, Token::Comment));
        assert!(tokens[0].text.starts_with("-- skalp:"));
    }

    #[test]
    fn test_logical_operators() {
        let tokens = tokenize("and or xor nand nor xnor not");
        let expected = [
            Token::And,
            Token::Or,
            Token::Xor,
            Token::Nand,
            Token::Nor,
            Token::Xnor,
            Token::Not,
        ];
        assert_eq!(tokens.len(), expected.len());
        for (t, e) in tokens.iter().zip(expected.iter()) {
            assert_eq!(&t.token, e);
        }
    }

    #[test]
    fn test_shift_operators() {
        let tokens = tokenize("sll srl sla sra rol ror");
        let expected = [
            Token::Sll,
            Token::Srl,
            Token::Sla,
            Token::Sra,
            Token::Rol,
            Token::Ror,
        ];
        assert_eq!(tokens.len(), expected.len());
        for (t, e) in tokens.iter().zip(expected.iter()) {
            assert_eq!(&t.token, e);
        }
    }

    #[test]
    fn test_full_entity_tokenization() {
        let source = r#"
entity Counter is
    generic (
        WIDTH : integer := 8
    );
    port (
        clk   : in  std_logic;
        rst   : in  std_logic;
        en    : in  std_logic;
        count : out std_logic_vector(WIDTH-1 downto 0)
    );
end entity Counter;
"#;
        let tokens = tokenize(source);
        assert!(!tokens.is_empty());
        // Check first meaningful tokens
        assert!(matches!(tokens[0].token, Token::Entity));
        assert!(matches!(&tokens[1].token, Token::Ident(n) if n == "counter"));
        assert!(matches!(tokens[2].token, Token::Is));
    }

    #[test]
    fn test_string_literal() {
        let tokens = tokenize(r#""hello world""#);
        assert_eq!(tokens.len(), 1);
        assert!(matches!(tokens[0].token, Token::StringLiteral));
    }

    #[test]
    fn test_real_literal() {
        let tokens = tokenize("3.14 1.0e10 2.5E-3");
        assert_eq!(tokens.len(), 3);
        for t in &tokens {
            assert!(matches!(t.token, Token::RealLiteral));
        }
    }

    #[test]
    fn test_builtin_functions() {
        let tokens = tokenize("rising_edge falling_edge to_unsigned to_signed to_integer resize");
        let expected = [
            Token::RisingEdge,
            Token::FallingEdge,
            Token::ToUnsigned,
            Token::ToSigned,
            Token::ToInteger,
            Token::Resize,
        ];
        assert_eq!(tokens.len(), expected.len());
        for (t, e) in tokens.iter().zip(expected.iter()) {
            assert_eq!(&t.token, e);
        }
    }

    #[test]
    fn test_ieee_type_keywords() {
        let tokens =
            tokenize("std_logic std_logic_vector unsigned signed boolean integer natural positive");
        let expected = [
            Token::StdLogic,
            Token::StdLogicVector,
            Token::Unsigned,
            Token::Signed,
            Token::Boolean,
            Token::Integer,
            Token::Natural,
            Token::Positive,
        ];
        assert_eq!(tokens.len(), expected.len());
        for (t, e) in tokens.iter().zip(expected.iter()) {
            assert_eq!(&t.token, e);
        }
    }
}
