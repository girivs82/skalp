//! Syntax tree definitions for SKALP using Rowan
//!
//! This module defines the syntax kinds and tree structure for the SKALP language


/// Syntax kinds for SKALP language
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u16)]
pub enum SyntaxKind {
    // === Tokens (Terminal nodes) ===

    // Keywords
    EntityKw,
    ImplKw,
    InKw,
    OutKw,
    InoutKw,
    SignalKw,
    VarKw,
    LetKw,
    ConstKw,
    OnKw,
    IfKw,
    ElseKw,
    MatchKw,
    FlowKw,
    WithKw,
    IntentKw,
    ProtocolKw,
    RequirementKw,
    AsyncKw,
    AwaitKw,
    TraitKw,
    ForKw,
    TypeKw,
    WhereKw,
    TimingKw,
    PowerKw,
    AreaKw,
    ThroughputKw,
    LatencyKw,
    MinimizeKw,
    MaximizeKw,

    // Type keywords
    BitKw,
    LogicKw,
    IntKw,
    NatKw,
    FixedKw,
    ClockKw,
    ResetKw,
    EventKw,
    StreamKw,

    // Special keywords
    RiseKw,
    FallKw,
    EdgeKw,

    // Literals
    Ident,
    IntLiteral,
    BinLiteral,
    HexLiteral,
    StringLiteral,

    // Operators
    NonBlockingAssign, // <=
    BlockingAssign,     // :=
    Assign,              // =
    Eq,                  // ==
    Neq,                 // !=
    Lt,                  // <
    Gt,                  // >
    Le,                  // <=
    Ge,                  // >=
    Plus,                // +
    Minus,               // -
    Star,                // *
    Slash,               // /
    Percent,             // %
    Amp,                 // &
    Pipe,                // |
    Caret,               // ^
    Bang,                // !
    Tilde,               // ~
    AmpAmp,             // &&
    PipePipe,           // ||
    Shl,                 // <<
    Shr,                 // >>
    Pipeline,            // |>
    Arrow,               // ->
    LeftArrow,          // <-

    // Delimiters
    LParen,             // (
    RParen,             // )
    LBracket,           // [
    RBracket,           // ]
    LBrace,             // {
    RBrace,             // }
    Comma,               // ,
    Semicolon,           // ;
    Colon,               // :
    Dot,                 // .
    Question,            // ?
    Apostrophe,          // '

    // Trivia
    Whitespace,
    Comment,

    // Error
    Error,

    // === Non-terminal nodes ===

    // Top level
    SourceFile,

    // Items
    EntityDecl,
    ImplBlock,
    ProtocolDecl,
    IntentDecl,
    RequirementDecl,
    TraitDef,
    TraitImpl,

    // Entity parts
    PortList,
    PortDecl,
    PortDirection,

    // Implementation parts
    SignalDecl,
    VariableDecl,
    ConstantDecl,
    EventBlock,

    // Event parts
    EventTriggerList,
    EventTrigger,
    EdgeType,

    // Statements
    AssignmentStmt,
    IfStmt,
    MatchStmt,
    BlockStmt,
    FlowStmt,

    // Flow pipeline
    FlowPipeline,
    PipelineStage,

    // Expressions
    LiteralExpr,
    IdentExpr,
    BinaryExpr,
    UnaryExpr,
    CallExpr,
    FieldExpr,
    IndexExpr,
    ParenExpr,
    ArrayLiteral,

    // Types
    TypeAnnotation,
    BitType,
    LogicType,
    IntType,
    NatType,
    ClockType,
    ResetType,
    EventType,
    ArrayType,
    TupleType,
    CustomType,

    // Type parts
    WidthSpec,
    TypeParams,

    // Patterns
    LiteralPattern,
    IdentPattern,
    WildcardPattern,
    TuplePattern,

    // Match parts
    MatchArmList,
    MatchArm,

    // Protocol parts
    ProtocolSignalList,
    ProtocolSignal,
    ProtocolDirection,

    // Intent parts
    IntentConstraintList,
    IntentConstraint,

    // Generic parameters
    GenericParamList,
    GenericParam,

    // Arguments
    ArgList,
    Arg,
    Parameter,
    ParameterList,

    // Trait parts
    TraitItemList,
    TraitItem,
    TraitMethod,
    TraitConst,
    TraitType,
    TraitBoundList,
    TraitBound,
    WhereClause,
    WherePredicate,

    // Miscellaneous
    Name,
    NameRef,
    Path,
    PathSegment,
    Attribute,
    Visibility,

    // Placeholder for the end
    __Last,
}

use SyntaxKind::*;

impl SyntaxKind {
    /// Check if this is a trivia token (whitespace or comment)
    pub fn is_trivia(self) -> bool {
        matches!(self, Whitespace | Comment)
    }

    /// Check if this is a keyword
    pub fn is_keyword(self) -> bool {
        matches!(
            self,
            EntityKw
                | ImplKw
                | InKw
                | OutKw
                | InoutKw
                | SignalKw
                | VarKw
                | LetKw
                | ConstKw
                | OnKw
                | IfKw
                | ElseKw
                | MatchKw
                | WithKw
                | IntentKw
                | ProtocolKw
                | RequirementKw
                | AsyncKw
                | AwaitKw
                | TraitKw
                | ForKw
                | TypeKw
                | WhereKw
                | BitKw
                | LogicKw
                | IntKw
                | NatKw
                | FixedKw
                | ClockKw
                | ResetKw
                | EventKw
                | StreamKw
                | RiseKw
                | FallKw
                | EdgeKw
        )
    }

    /// Check if this is an operator
    pub fn is_operator(self) -> bool {
        matches!(
            self,
            // Assignment operators are not binary operators in expressions
            // NonBlockingAssign | BlockingAssign | Assign
            Eq
                | Neq
                | Lt
                | Gt
                | Le
                | Ge
                | Plus
                | Minus
                | Star
                | Slash
                | Percent
                | Amp
                | Pipe
                | Caret
                | Bang
                | Tilde
                | AmpAmp
                | PipePipe
                | Shl
                | Shr
                // Note: Pipeline (|>) is not a binary expression operator
                // It's only used in flow statements
                | Arrow
                | LeftArrow
        )
    }

    /// Check if this is a literal
    pub fn is_literal(self) -> bool {
        matches!(
            self,
            IntLiteral | BinLiteral | HexLiteral | StringLiteral
        )
    }

    /// Get a human-readable description of this syntax kind
    pub fn description(self) -> &'static str {
        match self {
            EntityKw => "'entity'",
            ImplKw => "'impl'",
            InKw => "'in'",
            OutKw => "'out'",
            InoutKw => "'inout'",
            SignalKw => "'signal'",
            VarKw => "'var'",
            LetKw => "'let'",
            ConstKw => "'const'",
            OnKw => "'on'",
            IfKw => "'if'",
            ElseKw => "'else'",
            MatchKw => "'match'",
            WithKw => "'with'",
            IntentKw => "'intent'",
            ProtocolKw => "'protocol'",
            RequirementKw => "'requirement'",
            AsyncKw => "'async'",
            AwaitKw => "'await'",

            BitKw => "'bit'",
            LogicKw => "'logic'",
            IntKw => "'int'",
            NatKw => "'nat'",
            FixedKw => "'fixed'",
            ClockKw => "'clock'",
            ResetKw => "'reset'",
            EventKw => "'event'",
            StreamKw => "'stream'",

            RiseKw => "'rise'",
            FallKw => "'fall'",
            EdgeKw => "'edge'",

            Ident => "identifier",
            IntLiteral => "integer literal",
            BinLiteral => "binary literal",
            HexLiteral => "hexadecimal literal",
            StringLiteral => "string literal",

            NonBlockingAssign => "'<='",
            BlockingAssign => "':='",
            Assign => "'='",
            Eq => "'=='",
            Neq => "'!='",
            Lt => "'<'",
            Gt => "'>'",
            Le => "'<='",
            Ge => "'>='",
            Plus => "'+'",
            Minus => "'-'",
            Star => "'*'",
            Slash => "'/'",
            Percent => "'%'",
            Amp => "'&'",
            Pipe => "'|'",
            Caret => "'^'",
            Bang => "'!'",
            Tilde => "'~'",
            AmpAmp => "'&&'",
            PipePipe => "'||'",
            Shl => "'<<'",
            Shr => "'>>'",
            Pipeline => "'|>'",
            Arrow => "'->'",
            LeftArrow => "'<-'",

            LParen => "'('",
            RParen => "')'",
            LBracket => "'['",
            RBracket => "']'",
            LBrace => "'{'",
            RBrace => "'}'",
            Comma => "','",
            Semicolon => "';'",
            Colon => "':'",
            Dot => "'.'",
            Question => "'?'",
            Apostrophe => "'''",

            Whitespace => "whitespace",
            Comment => "comment",
            Error => "error",

            SourceFile => "source file",
            EntityDecl => "entity declaration",
            ImplBlock => "implementation block",
            ProtocolDecl => "protocol declaration",
            IntentDecl => "intent declaration",
            RequirementDecl => "requirement declaration",

            _ => "unknown",
        }
    }
}

/// Convert our lexer tokens to syntax kinds
impl From<crate::lexer::Token> for SyntaxKind {
    fn from(token: crate::lexer::Token) -> Self {
        use crate::lexer::Token;

        match token {
            Token::Entity => EntityKw,
            Token::Impl => ImplKw,
            Token::In => InKw,
            Token::Out => OutKw,
            Token::Inout => InoutKw,
            Token::Signal => SignalKw,
            Token::Var => VarKw,
            Token::Let => LetKw,
            Token::Const => ConstKw,
            Token::On => OnKw,
            Token::If => IfKw,
            Token::Else => ElseKw,
            Token::Match => MatchKw,
            Token::Flow => FlowKw,
            Token::With => WithKw,
            Token::Intent => IntentKw,
            Token::Protocol => ProtocolKw,
            Token::Requirement => RequirementKw,
            Token::Async => AsyncKw,
            Token::Await => AwaitKw,
            Token::Trait => TraitKw,
            Token::For => ForKw,
            Token::Type => TypeKw,
            Token::Where => WhereKw,
            Token::Timing => TimingKw,
            Token::Power => PowerKw,
            Token::Area => AreaKw,
            Token::Throughput => ThroughputKw,
            Token::Latency => LatencyKw,
            Token::Minimize => MinimizeKw,
            Token::Maximize => MaximizeKw,

            Token::Bit => BitKw,
            Token::Logic => LogicKw,
            Token::Int => IntKw,
            Token::Nat => NatKw,
            Token::Fixed => FixedKw,
            Token::Clock => ClockKw,
            Token::Reset => ResetKw,
            Token::Event => EventKw,
            Token::Stream => StreamKw,

            Token::Rise => RiseKw,
            Token::Fall => FallKw,
            Token::Edge => EdgeKw,

            Token::Identifier(_) => Ident,
            Token::DecimalLiteral(_) => IntLiteral,
            Token::BinaryLiteral(_) => BinLiteral,
            Token::HexLiteral(_) => HexLiteral,
            Token::StringLiteral(_) => StringLiteral,

            Token::NonBlockingAssign => NonBlockingAssign,
            Token::BlockingAssign => BlockingAssign,
            Token::Assign => Assign,
            Token::Equal => Eq,
            Token::NotEqual => Neq,
            Token::Less => Lt,
            Token::Greater => Gt,
            Token::LessEqual => Le,
            Token::GreaterEqual => Ge,
            Token::Plus => Plus,
            Token::Minus => Minus,
            Token::Star => Star,
            Token::Slash => Slash,
            Token::Percent => Percent,
            Token::Ampersand => Amp,
            Token::Pipe => Pipe,
            Token::Caret => Caret,
            Token::Bang => Bang,
            Token::Tilde => Tilde,
            Token::LogicalAnd => AmpAmp,
            Token::LogicalOr => PipePipe,
            Token::LeftShift => Shl,
            Token::RightShift => Shr,
            Token::Pipeline => Pipeline,
            Token::Arrow => Arrow,
            Token::LeftArrow => LeftArrow,

            Token::LeftParen => LParen,
            Token::RightParen => RParen,
            Token::LeftBracket => LBracket,
            Token::RightBracket => RBracket,
            Token::LeftBrace => LBrace,
            Token::RightBrace => RBrace,
            Token::Comma => Comma,
            Token::Semicolon => Semicolon,
            Token::Colon => Colon,
            Token::Dot => Dot,
            Token::Question => Question,
            Token::Apostrophe => Apostrophe,

            Token::Unknown => Error,
        }
    }
}

/// Language definition for Rowan
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SkalplLanguage {}

impl rowan::Language for SkalplLanguage {
    type Kind = SyntaxKind;

    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        assert!(raw.0 <= (__Last as u16));
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