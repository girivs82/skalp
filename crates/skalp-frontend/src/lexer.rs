//! SKALP Lexer using Logos
//!
//! Tokenizes SKALP source code into a stream of tokens with position information.

use logos::Logos;
use std::fmt;

/// Token types for SKALP
#[derive(Logos, Debug, Clone, PartialEq)]
pub enum Token {
    // Keywords
    #[token("entity")]
    Entity,

    #[token("impl")]
    Impl,

    #[token("in")]
    In,

    #[token("out")]
    Out,

    #[token("inout")]
    Inout,

    #[token("signal")]
    Signal,

    #[token("var")]
    Var,

    #[token("let")]
    Let,

    #[token("const")]
    Const,

    #[token("on")]
    On,

    #[token("if")]
    If,

    #[token("else")]
    Else,

    #[token("match")]
    Match,

    #[token("flow")]
    Flow,

    #[token("with")]
    With,

    #[token("intent")]
    Intent,

    #[token("protocol")]
    Protocol,

    #[token("requirement")]
    Requirement,

    #[token("async")]
    Async,

    #[token("await")]
    Await,

    #[token("trait")]
    Trait,

    #[token("for")]
    For,

    #[token("type")]
    Type,

    #[token("where")]
    Where,

    #[token("fn")]
    Fn,

    #[token("return")]
    Return,

    #[token("break")]
    Break,

    #[token("continue")]
    Continue,

    #[token("loop")]
    Loop,

    #[token("while")]
    While,

    #[token("pub")]
    Pub,

    #[token("mod")]
    Mod,

    #[token("use")]
    Use,

    #[token("crate")]
    Crate,

    #[token("super")]
    Super,

    #[token("self")]
    SelfKeyword,

    #[token("Self")]
    SelfType,

    #[token("static")]
    Static,

    #[token("mut")]
    Mut,

    #[token("ref")]
    Ref,

    // Intent-related keywords
    #[token("timing")]
    Timing,

    #[token("power")]
    Power,

    #[token("area")]
    Area,

    #[token("throughput")]
    Throughput,

    #[token("latency")]
    Latency,

    #[token("minimize")]
    Minimize,

    #[token("maximize")]
    Maximize,

    // Type keywords
    #[token("bit")]
    Bit,

    #[token("logic")]
    Logic,

    #[token("int")]
    Int,

    #[token("nat")]
    Nat,

    #[token("fixed")]
    Fixed,

    #[token("clock")]
    Clock,

    #[token("reset")]
    Reset,

    #[token("event")]
    Event,

    #[token("stream")]
    Stream,

    // Special keywords
    #[token("rise")]
    Rise,

    #[token("fall")]
    Fall,

    #[token("edge")]
    Edge,

    #[token("posedge")]
    Posedge,

    #[token("negedge")]
    Negedge,

    #[token("always")]
    Always,

    #[token("initial")]
    Initial,

    #[token("final")]
    Final,

    #[token("fork")]
    Fork,

    #[token("join")]
    Join,

    #[token("disable")]
    Disable,

    #[token("wait")]
    Wait,

    #[token("assign")]
    AssignKeyword,

    #[token("force")]
    Force,

    #[token("release")]
    Release,

    // Safety and verification keywords
    #[token("assert")]
    Assert,

    #[token("assume")]
    Assume,

    #[token("cover")]
    Cover,

    #[token("property")]
    Property,

    #[token("sequence")]
    Sequence,

    #[token("eventually")]
    Eventually,

    #[token("always_ff")]
    AlwaysFF,

    #[token("always_comb")]
    AlwaysComb,

    #[token("unique")]
    Unique,

    #[token("priority")]
    Priority,

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

    #[token("->")]
    Arrow,

    #[token("<-")]
    LeftArrow,

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

    #[token(":")]
    Colon,

    #[token(".")]
    Dot,

    #[token("?")]
    Question,

    #[token("'")]
    Apostrophe,

    // Whitespace and comments (skipped but tracked for position)
    #[regex(r"[ \t\n\f]+", logos::skip)]
    #[regex(r"//[^\n]*", logos::skip)]
    #[regex(r"/\*([^*]|\*+[^*/])*\*+/", logos::skip)]

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
            Token::StringLiteral(s) => write!(f, "\"{}\"", s),
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

/// Parse string literal (remove quotes and handle escapes)
fn parse_string(input: &str) -> Option<String> {
    let without_quotes = &input[1..input.len()-1]; // Remove quotes
    let mut result = String::new();
    let mut chars = without_quotes.chars();

    while let Some(ch) = chars.next() {
        match ch {
            '\\' => {
                match chars.next() {
                    Some('"') => result.push('"'),
                    Some('\\') => result.push('\\'),
                    Some('n') => result.push('\n'),
                    Some('t') => result.push('\t'),
                    Some(c) => {
                        result.push('\\');
                        result.push(c);
                    }
                    None => result.push('\\'),
                }
            }
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

        assert_eq!(tokens, vec![
            Token::Entity,
            Token::Impl,
            Token::In,
            Token::Out,
            Token::Signal,
            Token::On,
        ]);
    }

    #[test]
    fn test_identifiers() {
        let mut lexer = Lexer::new("counter clk reset_n my_signal");
        let tokens: Vec<_> = lexer.tokenize().into_iter().map(|t| t.token).collect();

        assert_eq!(tokens, vec![
            Token::Identifier("counter".to_string()),
            Token::Identifier("clk".to_string()),
            Token::Identifier("reset_n".to_string()),
            Token::Identifier("my_signal".to_string()),
        ]);
    }

    #[test]
    fn test_literals() {
        let mut lexer = Lexer::new("42 0xFF 0b1010 \"hello world\"");
        let tokens: Vec<_> = lexer.tokenize().into_iter().map(|t| t.token).collect();

        assert_eq!(tokens, vec![
            Token::DecimalLiteral(42),
            Token::HexLiteral(255),
            Token::BinaryLiteral(10),
            Token::StringLiteral("hello world".to_string()),
        ]);
    }

    #[test]
    fn test_operators() {
        let mut lexer = Lexer::new("<= := = |> -> <-");
        let tokens: Vec<_> = lexer.tokenize().into_iter().map(|t| t.token).collect();

        assert_eq!(tokens, vec![
            Token::NonBlockingAssign,
            Token::BlockingAssign,
            Token::Assign,
            Token::Pipeline,
            Token::Arrow,
            Token::LeftArrow,
        ]);
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

        assert_eq!(tokens, vec![
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
        ]);
    }

    #[test]
    fn test_underscore_numbers() {
        let mut lexer = Lexer::new("1_000_000 0xFF_FF 0b1010_1010");
        let tokens: Vec<_> = lexer.tokenize().into_iter().map(|t| t.token).collect();

        assert_eq!(tokens, vec![
            Token::DecimalLiteral(1_000_000),
            Token::HexLiteral(0xFFFF),
            Token::BinaryLiteral(0b10101010),
        ]);
    }

    #[test]
    fn test_advanced_keywords() {
        let mut lexer = Lexer::new("intent requirement protocol assert property always_ff");
        let tokens: Vec<_> = lexer.tokenize().into_iter().map(|t| t.token).collect();

        assert_eq!(tokens, vec![
            Token::Intent,
            Token::Requirement,
            Token::Protocol,
            Token::Assert,
            Token::Property,
            Token::AlwaysFF,
        ]);
    }

    #[test]
    fn test_safety_keywords() {
        let mut lexer = Lexer::new("assert assume cover property sequence eventually");
        let tokens: Vec<_> = lexer.tokenize().into_iter().map(|t| t.token).collect();

        assert_eq!(tokens, vec![
            Token::Assert,
            Token::Assume,
            Token::Cover,
            Token::Property,
            Token::Sequence,
            Token::Eventually,
        ]);
    }

    #[test]
    fn test_complete_intent_block() {
        let source = r#"
        intent {
            throughput: 100M_samples_per_sec,
            latency: minimize,
            power: 10mW
        }
        "#;

        let mut lexer = Lexer::new(source);
        let tokens: Vec<_> = lexer.tokenize().into_iter().map(|t| t.token).collect();

        // Should contain all intent-related tokens
        assert!(tokens.contains(&Token::Intent));
        assert!(tokens.contains(&Token::Throughput));
        assert!(tokens.contains(&Token::Latency));
        assert!(tokens.contains(&Token::Minimize));
        assert!(tokens.contains(&Token::Power));
    }

    #[test]
    fn test_error_recovery() {
        let mut lexer = Lexer::new("entity @ invalid $ Counter");
        let tokens: Vec<_> = lexer.tokenize().into_iter().map(|t| t.token).collect();

        // Should handle errors gracefully
        assert_eq!(tokens[0], Token::Entity);
        assert_eq!(tokens[1], Token::Error); // @
        assert_eq!(tokens[2], Token::Identifier("invalid".to_string()));
        assert_eq!(tokens[3], Token::Error); // $
        assert_eq!(tokens[4], Token::Identifier("Counter".to_string()));
    }
}