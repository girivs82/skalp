//! Syntax tree definitions for SKALP using Rowan
//!
//! This module defines the syntax kinds and tree structure for the SKALP language

use logos::Logos;

/// Syntax kinds for SKALP language
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u16)]
pub enum SyntaxKind {
    // === Tokens (Terminal nodes) ===

    // Keywords
    ENTITY_KW,
    IMPL_KW,
    IN_KW,
    OUT_KW,
    INOUT_KW,
    SIGNAL_KW,
    VAR_KW,
    LET_KW,
    CONST_KW,
    ON_KW,
    IF_KW,
    ELSE_KW,
    MATCH_KW,
    WITH_KW,
    INTENT_KW,
    PROTOCOL_KW,
    REQUIREMENT_KW,
    ASYNC_KW,
    AWAIT_KW,

    // Type keywords
    BIT_KW,
    LOGIC_KW,
    INT_KW,
    NAT_KW,
    FIXED_KW,
    CLOCK_KW,
    RESET_KW,
    EVENT_KW,
    STREAM_KW,

    // Special keywords
    RISE_KW,
    FALL_KW,
    EDGE_KW,

    // Literals
    IDENT,
    INT_LITERAL,
    BIN_LITERAL,
    HEX_LITERAL,
    STRING_LITERAL,

    // Operators
    NON_BLOCKING_ASSIGN, // <=
    BLOCKING_ASSIGN,     // :=
    ASSIGN,              // =
    EQ,                  // ==
    NEQ,                 // !=
    LT,                  // <
    GT,                  // >
    LE,                  // <=
    GE,                  // >=
    PLUS,                // +
    MINUS,               // -
    STAR,                // *
    SLASH,               // /
    PERCENT,             // %
    AMP,                 // &
    PIPE,                // |
    CARET,               // ^
    BANG,                // !
    TILDE,               // ~
    AMP_AMP,             // &&
    PIPE_PIPE,           // ||
    SHL,                 // <<
    SHR,                 // >>
    PIPELINE,            // |>
    ARROW,               // ->
    LEFT_ARROW,          // <-

    // Delimiters
    L_PAREN,             // (
    R_PAREN,             // )
    L_BRACKET,           // [
    R_BRACKET,           // ]
    L_BRACE,             // {
    R_BRACE,             // }
    COMMA,               // ,
    SEMICOLON,           // ;
    COLON,               // :
    DOT,                 // .
    QUESTION,            // ?
    APOSTROPHE,          // '

    // Trivia
    WHITESPACE,
    COMMENT,

    // Error
    ERROR,

    // === Non-terminal nodes ===

    // Top level
    SOURCE_FILE,

    // Items
    ENTITY_DECL,
    IMPL_BLOCK,
    PROTOCOL_DECL,
    INTENT_DECL,
    REQUIREMENT_DECL,

    // Entity parts
    PORT_LIST,
    PORT_DECL,
    PORT_DIRECTION,

    // Implementation parts
    SIGNAL_DECL,
    VARIABLE_DECL,
    CONSTANT_DECL,
    EVENT_BLOCK,

    // Event parts
    EVENT_TRIGGER_LIST,
    EVENT_TRIGGER,
    EDGE_TYPE,

    // Statements
    ASSIGNMENT_STMT,
    IF_STMT,
    MATCH_STMT,
    BLOCK_STMT,

    // Expressions
    LITERAL_EXPR,
    IDENT_EXPR,
    BINARY_EXPR,
    UNARY_EXPR,
    CALL_EXPR,
    FIELD_EXPR,
    INDEX_EXPR,
    PAREN_EXPR,

    // Types
    TYPE_ANNOTATION,
    BIT_TYPE,
    LOGIC_TYPE,
    INT_TYPE,
    NAT_TYPE,
    CLOCK_TYPE,
    RESET_TYPE,
    EVENT_TYPE,
    ARRAY_TYPE,
    CUSTOM_TYPE,

    // Type parts
    WIDTH_SPEC,
    TYPE_PARAMS,

    // Patterns
    LITERAL_PATTERN,
    IDENT_PATTERN,
    WILDCARD_PATTERN,
    TUPLE_PATTERN,

    // Match parts
    MATCH_ARM_LIST,
    MATCH_ARM,

    // Protocol parts
    PROTOCOL_SIGNAL_LIST,
    PROTOCOL_SIGNAL,
    PROTOCOL_DIRECTION,

    // Intent parts
    INTENT_CONSTRAINT_LIST,
    INTENT_CONSTRAINT,

    // Generic parameters
    GENERIC_PARAM_LIST,
    GENERIC_PARAM,

    // Arguments
    ARG_LIST,
    ARG,

    // Miscellaneous
    NAME,
    NAME_REF,
    PATH,
    PATH_SEGMENT,
    ATTRIBUTE,
    VISIBILITY,

    // Placeholder for the end
    __LAST,
}

use SyntaxKind::*;

impl SyntaxKind {
    /// Check if this is a trivia token (whitespace or comment)
    pub fn is_trivia(self) -> bool {
        matches!(self, WHITESPACE | COMMENT)
    }

    /// Check if this is a keyword
    pub fn is_keyword(self) -> bool {
        matches!(
            self,
            ENTITY_KW
                | IMPL_KW
                | IN_KW
                | OUT_KW
                | INOUT_KW
                | SIGNAL_KW
                | VAR_KW
                | LET_KW
                | CONST_KW
                | ON_KW
                | IF_KW
                | ELSE_KW
                | MATCH_KW
                | WITH_KW
                | INTENT_KW
                | PROTOCOL_KW
                | REQUIREMENT_KW
                | ASYNC_KW
                | AWAIT_KW
                | BIT_KW
                | LOGIC_KW
                | INT_KW
                | NAT_KW
                | FIXED_KW
                | CLOCK_KW
                | RESET_KW
                | EVENT_KW
                | STREAM_KW
                | RISE_KW
                | FALL_KW
                | EDGE_KW
        )
    }

    /// Check if this is an operator
    pub fn is_operator(self) -> bool {
        matches!(
            self,
            // Assignment operators are not binary operators in expressions
            // NON_BLOCKING_ASSIGN | BLOCKING_ASSIGN | ASSIGN
            EQ
                | NEQ
                | LT
                | GT
                | LE
                | GE
                | PLUS
                | MINUS
                | STAR
                | SLASH
                | PERCENT
                | AMP
                | PIPE
                | CARET
                | BANG
                | TILDE
                | AMP_AMP
                | PIPE_PIPE
                | SHL
                | SHR
                | PIPELINE
                | ARROW
                | LEFT_ARROW
        )
    }

    /// Check if this is a literal
    pub fn is_literal(self) -> bool {
        matches!(
            self,
            INT_LITERAL | BIN_LITERAL | HEX_LITERAL | STRING_LITERAL
        )
    }

    /// Get a human-readable description of this syntax kind
    pub fn description(self) -> &'static str {
        match self {
            ENTITY_KW => "'entity'",
            IMPL_KW => "'impl'",
            IN_KW => "'in'",
            OUT_KW => "'out'",
            INOUT_KW => "'inout'",
            SIGNAL_KW => "'signal'",
            VAR_KW => "'var'",
            LET_KW => "'let'",
            CONST_KW => "'const'",
            ON_KW => "'on'",
            IF_KW => "'if'",
            ELSE_KW => "'else'",
            MATCH_KW => "'match'",
            WITH_KW => "'with'",
            INTENT_KW => "'intent'",
            PROTOCOL_KW => "'protocol'",
            REQUIREMENT_KW => "'requirement'",
            ASYNC_KW => "'async'",
            AWAIT_KW => "'await'",

            BIT_KW => "'bit'",
            LOGIC_KW => "'logic'",
            INT_KW => "'int'",
            NAT_KW => "'nat'",
            FIXED_KW => "'fixed'",
            CLOCK_KW => "'clock'",
            RESET_KW => "'reset'",
            EVENT_KW => "'event'",
            STREAM_KW => "'stream'",

            RISE_KW => "'rise'",
            FALL_KW => "'fall'",
            EDGE_KW => "'edge'",

            IDENT => "identifier",
            INT_LITERAL => "integer literal",
            BIN_LITERAL => "binary literal",
            HEX_LITERAL => "hexadecimal literal",
            STRING_LITERAL => "string literal",

            NON_BLOCKING_ASSIGN => "'<='",
            BLOCKING_ASSIGN => "':='",
            ASSIGN => "'='",
            EQ => "'=='",
            NEQ => "'!='",
            LT => "'<'",
            GT => "'>'",
            LE => "'<='",
            GE => "'>='",
            PLUS => "'+'",
            MINUS => "'-'",
            STAR => "'*'",
            SLASH => "'/'",
            PERCENT => "'%'",
            AMP => "'&'",
            PIPE => "'|'",
            CARET => "'^'",
            BANG => "'!'",
            TILDE => "'~'",
            AMP_AMP => "'&&'",
            PIPE_PIPE => "'||'",
            SHL => "'<<'",
            SHR => "'>>'",
            PIPELINE => "'|>'",
            ARROW => "'->'",
            LEFT_ARROW => "'<-'",

            L_PAREN => "'('",
            R_PAREN => "')'",
            L_BRACKET => "'['",
            R_BRACKET => "']'",
            L_BRACE => "'{'",
            R_BRACE => "'}'",
            COMMA => "','",
            SEMICOLON => "';'",
            COLON => "':'",
            DOT => "'.'",
            QUESTION => "'?'",
            APOSTROPHE => "'''",

            WHITESPACE => "whitespace",
            COMMENT => "comment",
            ERROR => "error",

            SOURCE_FILE => "source file",
            ENTITY_DECL => "entity declaration",
            IMPL_BLOCK => "implementation block",
            PROTOCOL_DECL => "protocol declaration",
            INTENT_DECL => "intent declaration",
            REQUIREMENT_DECL => "requirement declaration",

            _ => "unknown",
        }
    }
}

/// Convert our lexer tokens to syntax kinds
impl From<crate::lexer::Token> for SyntaxKind {
    fn from(token: crate::lexer::Token) -> Self {
        use crate::lexer::Token;

        match token {
            Token::Entity => ENTITY_KW,
            Token::Impl => IMPL_KW,
            Token::In => IN_KW,
            Token::Out => OUT_KW,
            Token::Inout => INOUT_KW,
            Token::Signal => SIGNAL_KW,
            Token::Var => VAR_KW,
            Token::Let => LET_KW,
            Token::Const => CONST_KW,
            Token::On => ON_KW,
            Token::If => IF_KW,
            Token::Else => ELSE_KW,
            Token::Match => MATCH_KW,
            Token::With => WITH_KW,
            Token::Intent => INTENT_KW,
            Token::Protocol => PROTOCOL_KW,
            Token::Requirement => REQUIREMENT_KW,
            Token::Async => ASYNC_KW,
            Token::Await => AWAIT_KW,

            Token::Bit => BIT_KW,
            Token::Logic => LOGIC_KW,
            Token::Int => INT_KW,
            Token::Nat => NAT_KW,
            Token::Fixed => FIXED_KW,
            Token::Clock => CLOCK_KW,
            Token::Reset => RESET_KW,
            Token::Event => EVENT_KW,
            Token::Stream => STREAM_KW,

            Token::Rise => RISE_KW,
            Token::Fall => FALL_KW,
            Token::Edge => EDGE_KW,

            Token::Identifier(_) => IDENT,
            Token::DecimalLiteral(_) => INT_LITERAL,
            Token::BinaryLiteral(_) => BIN_LITERAL,
            Token::HexLiteral(_) => HEX_LITERAL,
            Token::StringLiteral(_) => STRING_LITERAL,

            Token::NonBlockingAssign => NON_BLOCKING_ASSIGN,
            Token::BlockingAssign => BLOCKING_ASSIGN,
            Token::Assign => ASSIGN,
            Token::Equal => EQ,
            Token::NotEqual => NEQ,
            Token::Less => LT,
            Token::Greater => GT,
            Token::LessEqual => LE,
            Token::GreaterEqual => GE,
            Token::Plus => PLUS,
            Token::Minus => MINUS,
            Token::Star => STAR,
            Token::Slash => SLASH,
            Token::Percent => PERCENT,
            Token::Ampersand => AMP,
            Token::Pipe => PIPE,
            Token::Caret => CARET,
            Token::Bang => BANG,
            Token::Tilde => TILDE,
            Token::LogicalAnd => AMP_AMP,
            Token::LogicalOr => PIPE_PIPE,
            Token::LeftShift => SHL,
            Token::RightShift => SHR,
            Token::Pipeline => PIPELINE,
            Token::Arrow => ARROW,
            Token::LeftArrow => LEFT_ARROW,

            Token::LeftParen => L_PAREN,
            Token::RightParen => R_PAREN,
            Token::LeftBracket => L_BRACKET,
            Token::RightBracket => R_BRACKET,
            Token::LeftBrace => L_BRACE,
            Token::RightBrace => R_BRACE,
            Token::Comma => COMMA,
            Token::Semicolon => SEMICOLON,
            Token::Colon => COLON,
            Token::Dot => DOT,
            Token::Question => QUESTION,
            Token::Apostrophe => APOSTROPHE,

            Token::Unknown => ERROR,
        }
    }
}

/// Language definition for Rowan
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SkalplLanguage {}

impl rowan::Language for SkalplLanguage {
    type Kind = SyntaxKind;

    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        assert!(raw.0 <= (__LAST as u16));
        unsafe { std::mem::transmute::<u16, SyntaxKind>(raw.0) }
    }

    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        rowan::SyntaxKind(kind as u16)
    }
}

/// Type aliases for convenience
pub type SyntaxNode = rowan::SyntaxNode<SkalplLanguage>;
pub type SyntaxToken = rowan::SyntaxToken<SkalplLanguage>;
pub type SyntaxElement = rowan::NodeOrToken<SyntaxNode, SyntaxToken>;
pub type SyntaxNodeChildren = rowan::SyntaxNodeChildren<SkalplLanguage>;
pub type SyntaxElementChildren = rowan::SyntaxElementChildren<SkalplLanguage>;
pub type PreorderWithTokens = rowan::api::PreorderWithTokens<SkalplLanguage>;

/// Extension methods for SyntaxNode
pub trait SyntaxNodeExt {
    /// Get the first child of a specific kind
    fn first_child_of_kind(&self, kind: SyntaxKind) -> Option<SyntaxNode>;

    /// Get all children of a specific kind
    fn children_of_kind(&self, kind: SyntaxKind) -> Vec<SyntaxNode>;

    /// Get the first token of a specific kind
    fn first_token_of_kind(&self, kind: SyntaxKind) -> Option<SyntaxToken>;
}

impl SyntaxNodeExt for SyntaxNode {
    fn first_child_of_kind(&self, kind: SyntaxKind) -> Option<SyntaxNode> {
        self.children().find(|n| n.kind() == kind)
    }

    fn children_of_kind(&self, kind: SyntaxKind) -> Vec<SyntaxNode> {
        self.children().filter(|n| n.kind() == kind).collect()
    }

    fn first_token_of_kind(&self, kind: SyntaxKind) -> Option<SyntaxToken> {
        self.children_with_tokens()
            .filter_map(|it| it.into_token())
            .find(|t| t.kind() == kind)
    }
}

// Note: Cannot impl Display for external type SyntaxNode directly