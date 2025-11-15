//! SKALP Lexer using Logos
//!
//! Tokenizes SKALP source code into a stream of tokens with position information.

use logos::Logos;
use std::fmt;

/// Token types for SKALP
#[derive(Logos, Debug, Clone, PartialEq)]
pub enum Token {
    // Core Hardware Description (12)
    #[token("entity")]
    Entity,
    #[token("impl")]
    Impl,
    #[token("signal")]
    Signal,
    #[token("var")]
    Var,
    #[token("const")]
    Const,
    #[token("in")]
    In,
    #[token("input")]
    Input,
    #[token("out")]
    Out,
    #[token("output")]
    Output,
    #[token("inout")]
    Inout,
    #[token("port")]
    Port,
    #[token("on")]
    On,
    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("assign")]
    AssignKw,

    // Type System (15) - Updated to include numeric, floating-point, and string types
    #[token("bit")]
    Bit,
    #[token("bool")]
    Bool,
    #[token("string")]
    String,
    #[token("nat")]
    Nat,
    #[token("int")]
    Int,
    #[token("logic")]
    Logic,
    #[token("clock")]
    Clock,
    #[token("reset")]
    Reset,
    #[token("type")]
    Type,
    #[token("stream")]
    Stream,
    #[token("fp16")]
    Fp16,
    #[token("fp32")]
    Fp32,
    #[token("fp64")]
    Fp64,
    #[token("struct")]
    Struct,
    #[token("enum")]
    Enum,
    #[token("union")]
    Union,

    // Boolean Literals (2)
    #[token("true")]
    True,
    #[token("false")]
    False,

    // Traits and Generics (5)
    #[token("trait")]
    Trait,
    #[token("protocol")]
    Protocol,
    #[token("where")]
    Where,
    #[token("self")]
    SelfKeyword,
    #[token("Self")]
    SelfType,

    // Event Control (2)
    #[token("rise")]
    Rise,
    #[token("fall")]
    Fall,

    // Control Flow (2)
    #[token("match")]
    Match,
    #[token("for")]
    For,

    // Design Intent (3)
    #[token("intent")]
    Intent,
    #[token("flow")]
    Flow,
    #[token("requirement")]
    Requirement,

    // Testbench Only (5)
    #[token("async")]
    Async,
    #[token("await")]
    Await,
    #[token("fn")]
    Fn,
    #[token("return")]
    Return,
    #[token("let")]
    Let,
    #[token("mut")]
    Mut,

    // Type Conversion (1)
    #[token("as")]
    As,

    // Module System (4)
    #[token("use")]
    Use,
    #[token("mod")]
    Mod,
    #[token("pub")]
    Pub,
    #[token("with")]
    With,

    // Verification (24)
    #[token("assert")]
    Assert,
    #[token("property")]
    Property,
    #[token("cover")]
    Cover,
    #[token("sequence")]
    Sequence,
    #[token("assume")]
    Assume,
    #[token("expect")]
    Expect,
    #[token("always")]
    Always,
    #[token("eventually")]
    Eventually,
    #[token("until")]
    Until,
    #[token("strong")]
    Strong,
    #[token("weak")]
    Weak,
    #[token("throughout")]
    Throughout,
    #[token("covergroup")]
    Covergroup,
    #[token("coverpoint")]
    Coverpoint,
    #[token("bins")]
    Bins,
    #[token("ignore_bins")]
    IgnoreBins,
    #[token("illegal_bins")]
    IllegalBins,
    #[token("cross")]
    Cross,
    #[token("invariant")]
    Invariant,
    #[token("safety")]
    Safety,
    #[token("liveness")]
    Liveness,
    #[token("bounded")]
    Bounded,
    #[token("formal")]
    Formal,
    #[token("prove")]
    Prove,

    // Safety Features (ISO 26262)
    #[token("asil")]
    Asil,
    #[token("safety_req")]
    SafetyReq,
    #[token("safety_goal")]
    SafetyGoal,
    #[token("fmea")]
    Fmea,
    #[token("fmeda")]
    Fmeda,
    #[token("psm")]
    Psm,
    #[token("lsm")]
    Lsm,
    #[token("spfm")]
    Spfm,
    #[token("lfm")]
    Lfm,
    #[token("pmhf")]
    Pmhf,
    #[token("power_domain")]
    PowerDomain,
    #[token("isolation")]
    Isolation,
    #[token("diagnostic_coverage")]
    DiagnosticCoverage,

    // Physical Constraints (21)
    #[token("constraint")]
    Constraint,
    #[token("physical")]
    Physical,
    #[token("pin")]
    Pin,
    #[token("pins")]
    Pins,
    #[token("pin_p")]
    PinP,
    #[token("pin_n")]
    PinN,
    #[token("io_standard")]
    IoStandard,
    #[token("drive")]
    Drive,
    #[token("slew")]
    Slew,
    #[token("pull")]
    Pull,
    #[token("diff_term")]
    DiffTerm,
    #[token("schmitt")]
    Schmitt,
    #[token("bank")]
    Bank,
    #[token("floorplan")]
    Floorplan,
    #[token("region")]
    Region,
    #[token("area")]
    Area,
    #[token("instances")]
    Instances,
    #[token("boundary")]
    Boundary,
    #[token("keep_together")]
    KeepTogether,
    #[token("preferred_region")]
    PreferredRegion,
    #[token("io_defaults")]
    IoDefaults,
    #[token("voltage")]
    Voltage,
    #[token("device")]
    Device,
    #[token("group")]
    Group,

    // Slew Rate Values (3)
    #[token("fast")]
    Fast,
    #[token("slow")]
    Slow,
    #[token("medium")]
    Medium,

    // Termination Values (4)
    #[token("up")]
    Up,
    #[token("down")]
    Down,
    #[token("none")]
    None,
    #[token("keeper")]
    Keeper,

    // Identifiers and literals
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice().to_owned())]
    Identifier(String),

    // Number literals with underscore support
    #[regex(r"0b[01_]+", |lex| parse_binary(lex.slice()))]
    BinaryLiteral(u64),

    #[regex(r"0x[0-9a-fA-F_]+", |lex| parse_hex(lex.slice()))]
    HexLiteral(u64),

    #[regex(r"[0-9][0-9_]*", |lex| parse_decimal(lex.slice()))]
    DecimalLiteral(u64),

    // Floating-point literals
    // Matches: 1.0, 3.14, 1.5e10, 1.5e-10, 1.0f, 1.0f32, 1.0f64
    #[regex(r"[0-9][0-9_]*\.[0-9][0-9_]*([eE][+-]?[0-9]+)?(f|f16|f32|f64)?", |lex| parse_float(lex.slice()))]
    FloatLiteral(f64),

    // String literals
    #[regex(r#""([^"\\]|\\["\\nt])*""#, |lex| parse_string(lex.slice()))]
    StringLiteral(String),

    // Operators
    #[token("<=", priority = 2)]
    NonBlockingAssign,

    #[token(":=")]
    BlockingAssign,

    #[token("=")]
    Assign,

    #[token("==")]
    Equal,

    #[token("!=")]
    NotEqual,

    #[token("<")]
    Less,

    #[token(">")]
    Greater,

    #[token("<=", priority = 1)]
    LessEqual,

    #[token(">=")]
    GreaterEqual,

    #[token("+")]
    Plus,

    #[token("-")]
    Minus,

    #[token("*")]
    Star,

    #[token("/")]
    Slash,

    #[token("%")]
    Percent,

    #[token("&")]
    Ampersand,

    #[token("|")]
    Pipe,

    #[token("^")]
    Caret,

    #[token("!")]
    Bang,

    #[token("~")]
    Tilde,

    #[token("&&")]
    LogicalAnd,

    #[token("||")]
    LogicalOr,

    #[token("<<")]
    LeftShift,

    #[token(">>")]
    RightShift,

    #[token("|>")]
    Pipeline,

    #[token("=>")]
    FatArrow,

    #[token("->")]
    Arrow,

    #[token("<-")]
    LeftArrow,

    // Assertion operators
    #[token("|->")]
    Implies,

    #[token("|=>")]
    ImpliesOverlap,

    #[token("##")]
    HashHash,

    #[token("[*")]
    RepeatOpen,

    #[token("*]")]
    RepeatClose,

    #[token("[+")]
    RepeatPlusOpen,

    #[token("+]")]
    RepeatPlusClose,

    #[token("[=")]
    RepeatEqualOpen,

    #[token("=]")]
    RepeatEqualClose,

    #[token("@")]
    At,

    #[token("$")]
    Dollar,

    // Delimiters
    #[token("(")]
    LeftParen,

    #[token(")")]
    RightParen,

    #[token("[")]
    LeftBracket,

    #[token("]")]
    RightBracket,

    #[token("{")]
    LeftBrace,

    #[token("}")]
    RightBrace,

    #[token(",")]
    Comma,

    #[token(";")]
    Semicolon,

    #[token("::")]
    ColonColon,

    #[token(":")]
    Colon,

    #[token(".")]
    Dot,

    #[token("..=")]
    DotDotEq,

    #[token("..")]
    DotDot,

    #[token("?")]
    Question,

    #[token("'")]
    Apostrophe,

    // Verilog-style sized literals (must come before Lifetime to take precedence)
    // Matches: 8'hFF, 31'b0, 4'd15, 16'h1234
    #[regex(r"[0-9]+'\s*[bhd][0-9a-fA-F_]+", |lex| parse_sized_literal(lex.slice()))]
    SizedLiteral(u64),

    // Lifetime token - 'identifier for clock domain lifetimes
    // This pattern now only matches when the char after ' is alphabetic (not bhd followed by digits)
    #[regex(r"'[a-zA-Z_&&[^bhd]][a-zA-Z0-9_]*", |lex| lex.slice()[1..].to_owned())]
    #[regex(r"'[bhd][a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice()[1..].to_owned())]
    Lifetime(String),

    // Whitespace and comments (skipped but tracked for position)
    #[regex(r"[ \t\n\f]+", logos::skip)]
    #[regex(r"//[^\n]*", logos::skip)]
    #[regex(r"/\*[^*]*\*+([^/*][^*]*\*+)*/", logos::skip)]
    // Error token for unknown/invalid input
    Error,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::Entity => write!(f, "entity"),
            Token::Impl => write!(f, "impl"),
            Token::Identifier(name) => write!(f, "{}", name),
            Token::DecimalLiteral(n) => write!(f, "{}", n),
            Token::BinaryLiteral(n) => write!(f, "0b{:b}", n),
            Token::HexLiteral(n) => write!(f, "0x{:x}", n),
            Token::SizedLiteral(n) => write!(f, "{}", n),
            Token::FloatLiteral(fl) => write!(f, "{}", fl),
            Token::StringLiteral(s) => write!(f, "\"{}\"", s),
            Token::Lifetime(name) => write!(f, "'{}", name),
            Token::LeftParen => write!(f, "("),
            Token::RightParen => write!(f, ")"),
            _ => write!(f, "{:?}", self),
        }
    }
}

/// Parse binary literal (0b1010 -> 10) with underscore support
pub fn parse_binary(input: &str) -> Option<u64> {
    let without_prefix = &input[2..]; // Remove "0b"
    let without_underscores = without_prefix.replace('_', "");
    u64::from_str_radix(&without_underscores, 2).ok()
}

/// Parse hex literal (0xFF -> 255) with underscore support
pub fn parse_hex(input: &str) -> Option<u64> {
    let without_prefix = &input[2..]; // Remove "0x"
    let without_underscores = without_prefix.replace('_', "");
    u64::from_str_radix(&without_underscores, 16).ok()
}

/// Parse decimal literal (1_000_000 -> 1000000) with underscore support
pub fn parse_decimal(input: &str) -> Option<u64> {
    let without_underscores = input.replace('_', "");
    without_underscores.parse::<u64>().ok()
}

/// Parse floating-point literal
/// Supports: 1.0, 3.14, 1.5e10, 1.5e-10, 1.0f, 1.0f32, 1.0f64
pub fn parse_float(input: &str) -> Option<f64> {
    // Remove underscores and optional type suffix (f, f16, f32, f64)
    let mut cleaned = input.replace('_', "");

    // Remove type suffix if present
    if cleaned.ends_with("f64") || cleaned.ends_with("f32") || cleaned.ends_with("f16") {
        cleaned = cleaned[..cleaned.len() - 3].to_string();
    } else if cleaned.ends_with('f') {
        cleaned = cleaned[..cleaned.len() - 1].to_string();
    }

    cleaned.parse::<f64>().ok()
}

/// Parse Verilog-style sized literal (e.g., 8'hFF, 31'b0, 4'd15)
/// Format: <width>'<base><value>
pub fn parse_sized_literal(input: &str) -> Option<u64> {
    // Split on apostrophe
    let parts: Vec<&str> = input.split('\'').collect();
    if parts.len() != 2 {
        return None;
    }

    // Parse the width (not used for the value, but validates format)
    let _width = parts[0].trim().parse::<u64>().ok()?;

    // Get base and value
    let base_and_value = parts[1].trim();
    if base_and_value.is_empty() {
        return None;
    }

    let base_char = base_and_value.chars().next()?;
    let value_str = &base_and_value[1..].replace('_', "");

    match base_char {
        'b' | 'B' => u64::from_str_radix(value_str, 2).ok(),
        'h' | 'H' => u64::from_str_radix(value_str, 16).ok(),
        'd' | 'D' => value_str.parse::<u64>().ok(),
        _ => None,
    }
}

/// Parse string literal (remove quotes and handle escapes)
fn parse_string(input: &str) -> Option<String> {
    let without_quotes = &input[1..input.len() - 1]; // Remove quotes
    let mut result = String::new();
    let mut chars = without_quotes.chars();

    while let Some(ch) = chars.next() {
        match ch {
            '\\' => match chars.next() {
                Some('"') => result.push('"'),
                Some('\\') => result.push('\\'),
                Some('n') => result.push('\n'),
                Some('t') => result.push('\t'),
                Some(c) => {
                    result.push('\\');
                    result.push(c);
                }
                None => result.push('\\'),
            },
            c => result.push(c),
        }
    }

    Some(result)
}

/// Token with position information
#[derive(Debug, Clone)]
pub struct TokenWithPos {
    pub token: Token,
    pub span: std::ops::Range<usize>,
}

/// SKALP Lexer
pub struct Lexer<'a> {
    inner: logos::Lexer<'a, Token>,
}

impl<'a> Lexer<'a> {
    /// Create a new lexer for the given source
    pub fn new(source: &'a str) -> Self {
        Self {
            inner: Token::lexer(source),
        }
    }

    /// Get the next token with position
    pub fn next_token(&mut self) -> Option<TokenWithPos> {
        self.inner.next().map(|result| {
            let token = result.unwrap_or(Token::Error);
            let span = self.inner.span();
            TokenWithPos { token, span }
        })
    }

    /// Tokenize entire source into a vector
    pub fn tokenize(&mut self) -> Vec<TokenWithPos> {
        let mut tokens = Vec::new();
        while let Some(token) = self.next_token() {
            tokens.push(token);
        }
        tokens
    }

    /// Get current source slice
    pub fn slice(&self) -> &'a str {
        self.inner.slice()
    }

    /// Get current span
    pub fn span(&self) -> std::ops::Range<usize> {
        self.inner.span()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_keywords() {
        let mut lexer = Lexer::new("entity impl in out signal on");
        let tokens: Vec<_> = lexer.tokenize().into_iter().map(|t| t.token).collect();

        assert_eq!(
            tokens,
            vec![
                Token::Entity,
                Token::Impl,
                Token::In,
                Token::Out,
                Token::Signal,
                Token::On,
            ]
        );
    }

    #[test]
    fn test_identifiers() {
        let mut lexer = Lexer::new("counter clk reset_n my_signal");
        let tokens: Vec<_> = lexer.tokenize().into_iter().map(|t| t.token).collect();

        assert_eq!(
            tokens,
            vec![
                Token::Identifier("counter".to_string()),
                Token::Identifier("clk".to_string()),
                Token::Identifier("reset_n".to_string()),
                Token::Identifier("my_signal".to_string()),
            ]
        );
    }

    #[test]
    fn test_literals() {
        let mut lexer = Lexer::new("42 0xFF 0b1010 \"hello world\"");
        let tokens: Vec<_> = lexer.tokenize().into_iter().map(|t| t.token).collect();

        assert_eq!(
            tokens,
            vec![
                Token::DecimalLiteral(42),
                Token::HexLiteral(255),
                Token::BinaryLiteral(10),
                Token::StringLiteral("hello world".to_string()),
            ]
        );
    }

    #[test]
    fn test_operators() {
        let mut lexer = Lexer::new("<= := = |> -> <-");
        let tokens: Vec<_> = lexer.tokenize().into_iter().map(|t| t.token).collect();

        assert_eq!(
            tokens,
            vec![
                Token::NonBlockingAssign,
                Token::BlockingAssign,
                Token::Assign,
                Token::Pipeline,
                Token::Arrow,
                Token::LeftArrow,
            ]
        );
    }

    #[test]
    fn test_simple_entity() {
        let source = r#"
entity Counter {
    in clk: clock
    out count: nat[8]
}
"#;

        let mut lexer = Lexer::new(source);
        let tokens: Vec<_> = lexer.tokenize().into_iter().map(|t| t.token).collect();

        // Should tokenize without errors
        assert!(tokens.contains(&Token::Entity));
        assert!(tokens.contains(&Token::Identifier("Counter".to_string())));
    }

    #[test]
    fn test_on_syntax() {
        let mut lexer = Lexer::new("on(clk.rise | reset.rise)");
        let tokens: Vec<_> = lexer.tokenize().into_iter().map(|t| t.token).collect();

        assert_eq!(
            tokens,
            vec![
                Token::On,
                Token::LeftParen,
                Token::Identifier("clk".to_string()),
                Token::Dot,
                Token::Rise,
                Token::Pipe,
                Token::Reset,
                Token::Dot,
                Token::Rise,
                Token::RightParen,
            ]
        );
    }

    #[test]
    fn test_underscore_numbers() {
        let mut lexer = Lexer::new("1_000_000 0xFF_FF 0b1010_1010");
        let tokens: Vec<_> = lexer.tokenize().into_iter().map(|t| t.token).collect();

        assert_eq!(
            tokens,
            vec![
                Token::DecimalLiteral(1_000_000),
                Token::HexLiteral(0xFFFF),
                Token::BinaryLiteral(0b10101010),
            ]
        );
    }

    #[test]
    fn test_advanced_keywords() {
        let mut lexer = Lexer::new("intent requirement protocol async await flow");
        let tokens: Vec<_> = lexer.tokenize().into_iter().map(|t| t.token).collect();

        assert_eq!(
            tokens,
            vec![
                Token::Intent,
                Token::Requirement,
                Token::Protocol,
                Token::Async,
                Token::Await,
                Token::Flow,
            ]
        );
    }

    #[test]
    fn test_event_keywords() {
        let mut lexer = Lexer::new("rise fall");
        let tokens: Vec<_> = lexer.tokenize().into_iter().map(|t| t.token).collect();

        assert_eq!(tokens, vec![Token::Rise, Token::Fall,]);
    }

    #[test]
    fn test_verification_keywords() {
        let mut lexer = Lexer::new("assert requirement");
        let tokens: Vec<_> = lexer.tokenize().into_iter().map(|t| t.token).collect();

        assert_eq!(tokens, vec![Token::Assert, Token::Requirement,]);
    }

    #[test]
    fn test_safety_keywords() {
        let mut lexer = Lexer::new("asil safety_req psm lsm fmea power_domain");
        let tokens: Vec<_> = lexer.tokenize().into_iter().map(|t| t.token).collect();

        assert_eq!(
            tokens,
            vec![
                Token::Asil,
                Token::SafetyReq,
                Token::Psm,
                Token::Lsm,
                Token::Fmea,
                Token::PowerDomain,
            ]
        );
    }

    #[test]
    fn test_complete_intent_block() {
        let source = r#"
        intent {
            target_freq: 100_000_000,
            optimization: area
        }
        "#;

        let mut lexer = Lexer::new(source);
        let tokens: Vec<_> = lexer.tokenize().into_iter().map(|t| t.token).collect();

        // Should contain intent keyword and identifiers
        assert!(tokens.contains(&Token::Intent));
        assert!(tokens.contains(&Token::Identifier("target_freq".to_string())));
        assert!(tokens.contains(&Token::Identifier("optimization".to_string())));
        // 'area' is now a keyword for physical constraints, not an identifier
        assert!(tokens.contains(&Token::Area));
    }

    #[test]
    fn test_error_recovery() {
        let mut lexer = Lexer::new("entity @ invalid $ Counter");
        let tokens: Vec<_> = lexer.tokenize().into_iter().map(|t| t.token).collect();

        // Should handle tokens gracefully
        assert_eq!(tokens[0], Token::Entity);
        assert_eq!(tokens[1], Token::At); // @ is a valid token
        assert_eq!(tokens[2], Token::Identifier("invalid".to_string()));
        assert_eq!(tokens[3], Token::Dollar); // $ is a valid token
        assert_eq!(tokens[4], Token::Identifier("Counter".to_string()));
    }

    #[test]
    fn test_block_comments() {
        let source = "entity /* comment */ Test";
        let mut lexer = Lexer::new(source);
        let tokens: Vec<_> = lexer.tokenize().into_iter().map(|t| t.token).collect();

        // Should skip the comment and only see: entity, Test
        println!("Tokens: {:?}", tokens);
        assert_eq!(tokens.len(), 2);
        assert_eq!(tokens[0], Token::Entity);
        assert_eq!(tokens[1], Token::Identifier("Test".to_string()));
    }
}
