//! Syntax tree definitions for SKALP using Rowan
//!
//! This module defines the syntax kinds and tree structure for the SKALP language

/// Syntax kinds for SKALP language
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u16)]
pub enum SyntaxKind {
    // === Tokens (Terminal nodes) ===

    // Core Hardware Description (12)
    EntityKw,
    ImplKw,
    SignalKw,
    VarKw,
    ConstKw,
    InKw,
    InputKw,
    OutKw,
    OutputKw,
    InoutKw,
    PortKw,
    OnKw,
    IfKw,
    ElseKw,
    AssignKw,

    // Type System (13) - Updated to include numeric and floating-point types
    BitKw,
    BoolKw,
    NatKw,
    IntKw,
    LogicKw,
    ClockKw,
    ResetKw,
    TypeKw,
    StreamKw,
    Fp16Kw,
    Fp32Kw,
    Fp64Kw,
    Vec2Kw,
    Vec3Kw,
    Vec4Kw,
    StructKw,
    EnumKw,
    UnionKw,

    // Boolean Literals (2)
    TrueKw,
    FalseKw,

    // Traits and Generics (5)
    TraitKw,
    ProtocolKw,
    WhereKw,
    SelfKw,
    SelfTypeKw,

    // Event Control (2)
    RiseKw,
    FallKw,

    // Control Flow (2)
    MatchKw,
    ForKw,

    // Design Intent (3)
    IntentKw,
    FlowKw,
    RequirementKw,

    // Testbench Only (5)
    AsyncKw,
    AwaitKw,
    FnKw,
    ReturnKw,
    LetKw,

    // Type Conversion (1)
    AsKw,

    // Module System (4)
    UseKw,
    ModKw,
    PubKw,
    WithKw,

    // Verification (24)
    AssertKw,
    PropertyKw,
    CoverKw,
    SequenceKw,
    AssumeKw,
    ExpectKw,
    AlwaysKw,
    EventuallyKw,
    UntilKw,
    StrongKw,
    WeakKw,
    ThroughoutKw,
    CovergroupKw,
    CoverpointKw,
    BinsKw,
    IgnoreBinsKw,
    IllegalBinsKw,
    CrossKw,
    InvariantKw,
    SafetyKw,
    LivenessKw,
    BoundedKw,
    FormalKw,
    ProveKw,

    // Safety Features (ISO 26262)
    AsilKw,
    SafetyReqKw,
    SafetyGoalKw,
    FmeaKw,
    FmedaKw,
    PsmKw,
    LsmKw,
    SpfmKw,
    LfmKw,
    PmhfKw,
    PowerDomainKw,
    IsolationKw,
    DiagnosticCoverageKw,

    // Physical Constraints (28)
    ConstraintKw,
    PhysicalKw,
    PinKw,
    PinsKw,
    PinPKw,
    PinNKw,
    IoStandardKw,
    DriveKw,
    SlewKw,
    PullKw,
    DiffTermKw,
    SchmittKw,
    BankKw,
    FloorplanKw,
    RegionKw,
    AreaKw,
    InstancesKw,
    BoundaryKw,
    KeepTogetherKw,
    PreferredRegionKw,
    IoDefaultsKw,
    VoltageKw,
    DeviceKw,
    GroupKw,
    FastKw,
    SlowKw,
    MediumKw,
    UpKw,
    DownKw,
    NoneKw,
    KeeperKw,

    // Literals
    Ident,
    IntLiteral,
    BinLiteral,
    HexLiteral,
    FloatLiteral,
    StringLiteral,
    Lifetime,

    // Operators
    NonBlockingAssign, // <=
    BlockingAssign,    // :=
    Assign,            // =
    Eq,                // ==
    Neq,               // !=
    Lt,                // <
    Gt,                // >
    Le,                // <=
    Ge,                // >=
    Plus,              // +
    Minus,             // -
    Star,              // *
    Slash,             // /
    Percent,           // %
    Amp,               // &
    Pipe,              // |
    Caret,             // ^
    Bang,              // !
    Tilde,             // ~
    AmpAmp,            // &&
    PipePipe,          // ||
    Shl,               // <<
    Shr,               // >>
    Pipeline,          // |>
    FatArrow,          // =>
    Arrow,             // ->
    LeftArrow,         // <-
    Implies,           // |->
    ImpliesOverlap,    // |=>
    HashHash,          // ##
    RepeatOpen,        // [*
    RepeatClose,       // *]
    RepeatPlusOpen,    // [+
    RepeatPlusClose,   // +]
    RepeatEqualOpen,   // [=
    RepeatEqualClose,  // =]
    At,                // @
    Dollar,            // $

    // Delimiters
    LParen,     // (
    RParen,     // )
    LBracket,   // [
    RBracket,   // ]
    LBrace,     // {
    RBrace,     // }
    Comma,      // ,
    Semicolon,  // ;
    ColonColon, // ::
    Colon,      // :
    Dot,        // .
    DotDot,     // ..
    DotDotEq,   // ..=
    Question,   // ?
    Apostrophe, // '

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
    StructDecl,
    EnumDecl,
    UnionDecl,
    TypeAlias,
    UseDecl,
    ModuleDecl,

    // Module system parts
    UsePath,
    UseTree,

    // Entity parts
    PortList,
    PortDecl,
    PortDirection,

    // Implementation parts
    SignalDecl,
    VariableDecl,
    ConstantDecl,
    EventBlock,
    FunctionDecl,

    // Event parts
    EventTriggerList,
    EventTrigger,
    EdgeType,

    // Statements
    AssignmentStmt,
    ReturnStmt,
    ExprStmt,
    IfStmt,
    MatchStmt,
    ForStmt,
    BlockStmt,
    LetStmt,
    FlowStmt,
    AssertStmt,
    PropertyStmt,
    CoverStmt,
    SequenceStmt,
    AssumeStmt,
    ExpectStmt,

    // Temporal logic
    TemporalExpr,
    PropertyExpr,
    SequenceExpr,
    ClockingEvent,
    ImplicationExpr,
    RepetitionExpr,
    DelayExpr,

    // Coverage
    CovergroupDecl,
    CoverpointDecl,
    BinsDecl,
    CrossDecl,
    CoverageItem,

    // Formal Verification
    InvariantDecl,
    SafetyProperty,
    LivenessProperty,
    BoundedProperty,
    FormalBlock,
    ProveStmt,
    InstanceDecl,
    ConnectionList,
    Connection,

    // Physical Constraints
    GlobalConstraintBlock,
    PhysicalConstraintBlock,
    ConstraintPair,
    PinLocation,
    PinArray,
    DriveStrength,
    SlewRate,
    Termination,
    BankBlock,
    FloorplanBlock,
    RegionBlock,
    AreaSpec,
    IoDefaultsBlock,
    DeviceSpec,
    GroupBlock,

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
    StructLiteral,
    StructFieldInit,
    PathExpr,
    IfExpr,
    MatchExpr,
    CastExpr,
    ConcatExpr,
    TernaryExpr,
    TupleExpr,
    RangeExpr,
    ClosureExpr,
    ClosureParamList,
    ClosureParam,

    // Types
    TypeExpr,
    TypeAnnotation,
    BitType,
    BoolType,
    LogicType,
    IntType,
    NatType,
    ClockType,
    ResetType,
    EventType,
    StreamType,
    Fp16Type,
    Fp32Type,
    Fp64Type,
    Vec2Type,
    Vec3Type,
    Vec4Type,
    IdentType,
    ArrayType,
    ArraySize,
    TupleType,
    CustomType,
    SelfType,
    StructType,
    EnumType,

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
    MatchGuard,

    // Protocol parts
    ProtocolSignalList,
    ProtocolSignal,
    ProtocolDirection,

    // Intent parts
    IntentConstraintList,
    IntentConstraint,

    // Struct/Enum/Union parts
    StructFieldList,
    StructField,
    EnumVariantList,
    EnumVariant,
    UnionFieldList,
    UnionField,

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
    TraitSignal,
    TraitBound,
    TraitBoundList,
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
            // Core Hardware Description (14)
            EntityKw | ImplKw | SignalKw | VarKw | ConstKw
                | InKw | InputKw | OutKw | OutputKw | InoutKw | PortKw | OnKw | IfKw | ElseKw | AssignKw
            // Type System (14)
            | BitKw | BoolKw | NatKw | IntKw | LogicKw | ClockKw | ResetKw | TypeKw | StreamKw | Fp16Kw | Fp32Kw | Fp64Kw | Vec2Kw | Vec3Kw | Vec4Kw | StructKw | EnumKw | UnionKw
            // Boolean Literals (2)
            | TrueKw | FalseKw
            // Traits and Generics (5)
            | TraitKw | ProtocolKw | WhereKw | SelfKw | SelfTypeKw
            // Event Control (2)
            | RiseKw | FallKw
            // Control Flow (2)
            | MatchKw | ForKw
            // Design Intent (3)
            | IntentKw | FlowKw | RequirementKw
            // Testbench Only (5)
            | AsyncKw | AwaitKw | FnKw | ReturnKw | LetKw
            // Type Conversion (1)
            | AsKw
            // Module System (4)
            | UseKw | ModKw | PubKw | WithKw
            // Verification (24)
            | AssertKw | PropertyKw | CoverKw | SequenceKw | AssumeKw | ExpectKw
            | AlwaysKw | EventuallyKw | UntilKw | StrongKw | WeakKw | ThroughoutKw
            | CovergroupKw | CoverpointKw | BinsKw | IgnoreBinsKw | IllegalBinsKw | CrossKw
            | InvariantKw | SafetyKw | LivenessKw | BoundedKw | FormalKw | ProveKw
            // Safety Features (ISO 26262)
            | AsilKw | SafetyReqKw | SafetyGoalKw | FmeaKw | FmedaKw
            | PsmKw | LsmKw | SpfmKw | LfmKw | PmhfKw
            | PowerDomainKw | IsolationKw | DiagnosticCoverageKw
            // Physical Constraints (28)
            | ConstraintKw | PhysicalKw | PinKw | PinsKw | PinPKw | PinNKw
            | IoStandardKw | DriveKw | SlewKw | PullKw | DiffTermKw | SchmittKw
            | BankKw | FloorplanKw | RegionKw | AreaKw | InstancesKw | BoundaryKw
            | KeepTogetherKw | PreferredRegionKw | IoDefaultsKw | VoltageKw
            | DeviceKw | GroupKw | FastKw | SlowKw | MediumKw
            | UpKw | DownKw | NoneKw | KeeperKw
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
            IntLiteral | BinLiteral | HexLiteral | FloatLiteral | StringLiteral
        )
    }

    /// Get a human-readable description of this syntax kind
    pub fn description(self) -> &'static str {
        match self {
            // Core Hardware Description (12)
            EntityKw => "'entity'",
            ImplKw => "'impl'",
            SignalKw => "'signal'",
            VarKw => "'var'",
            ConstKw => "'const'",
            InKw => "'in'",
            InputKw => "'input'",
            OutKw => "'out'",
            OutputKw => "'output'",
            InoutKw => "'inout'",
            OnKw => "'on'",
            IfKw => "'if'",
            ElseKw => "'else'",
            AssignKw => "'assign'",

            // Type System (14)
            BitKw => "'bit'",
            BoolKw => "'bool'",
            NatKw => "'nat'",
            IntKw => "'int'",
            LogicKw => "'logic'",
            ClockKw => "'clock'",
            ResetKw => "'reset'",
            TypeKw => "'type'",
            StreamKw => "'stream'",
            Fp16Kw => "'fp16'",
            Fp32Kw => "'fp32'",
            Fp64Kw => "'fp64'",
            Vec2Kw => "'vec2'",
            Vec3Kw => "'vec3'",
            Vec4Kw => "'vec4'",
            StructKw => "'struct'",
            EnumKw => "'enum'",
            UnionKw => "'union'",

            // Boolean Literals (2)
            TrueKw => "'true'",
            FalseKw => "'false'",

            // Traits and Generics (5)
            TraitKw => "'trait'",
            ProtocolKw => "'protocol'",
            WhereKw => "'where'",
            SelfKw => "'self'",
            SelfTypeKw => "'Self'",

            // Event Control (2)
            RiseKw => "'rise'",
            FallKw => "'fall'",

            // Control Flow (2)
            MatchKw => "'match'",
            ForKw => "'for'",

            // Design Intent (3)
            IntentKw => "'intent'",
            FlowKw => "'flow'",
            RequirementKw => "'requirement'",

            // Testbench Only (5)
            AsyncKw => "'async'",
            AwaitKw => "'await'",
            FnKw => "'fn'",
            ReturnKw => "'return'",
            LetKw => "'let'",

            // Type Conversion (1)
            AsKw => "'as'",

            // Module System (4)
            UseKw => "'use'",
            ModKw => "'mod'",
            PubKw => "'pub'",
            WithKw => "'with'",

            // Verification (24)
            AssertKw => "'assert'",
            PropertyKw => "'property'",
            CoverKw => "'cover'",
            SequenceKw => "'sequence'",
            AssumeKw => "'assume'",
            ExpectKw => "'expect'",
            AlwaysKw => "'always'",
            EventuallyKw => "'eventually'",
            UntilKw => "'until'",
            StrongKw => "'strong'",
            WeakKw => "'weak'",
            ThroughoutKw => "'throughout'",
            CovergroupKw => "'covergroup'",
            CoverpointKw => "'coverpoint'",
            BinsKw => "'bins'",
            IgnoreBinsKw => "'ignore_bins'",
            IllegalBinsKw => "'illegal_bins'",
            CrossKw => "'cross'",
            InvariantKw => "'invariant'",
            SafetyKw => "'safety'",
            LivenessKw => "'liveness'",
            BoundedKw => "'bounded'",
            FormalKw => "'formal'",
            ProveKw => "'prove'",

            // Safety Features (ISO 26262)
            AsilKw => "'asil'",
            SafetyReqKw => "'safety_req'",
            SafetyGoalKw => "'safety_goal'",
            FmeaKw => "'fmea'",
            FmedaKw => "'fmeda'",
            PsmKw => "'psm'",
            LsmKw => "'lsm'",
            SpfmKw => "'spfm'",
            LfmKw => "'lfm'",
            PmhfKw => "'pmhf'",
            PowerDomainKw => "'power_domain'",
            IsolationKw => "'isolation'",
            DiagnosticCoverageKw => "'diagnostic_coverage'",

            // Physical Constraints (28)
            ConstraintKw => "'constraint'",
            PhysicalKw => "'physical'",
            PinKw => "'pin'",
            PinsKw => "'pins'",
            PinPKw => "'pin_p'",
            PinNKw => "'pin_n'",
            IoStandardKw => "'io_standard'",
            DriveKw => "'drive'",
            SlewKw => "'slew'",
            PullKw => "'pull'",
            DiffTermKw => "'diff_term'",
            SchmittKw => "'schmitt'",
            BankKw => "'bank'",
            FloorplanKw => "'floorplan'",
            RegionKw => "'region'",
            AreaKw => "'area'",
            InstancesKw => "'instances'",
            BoundaryKw => "'boundary'",
            KeepTogetherKw => "'keep_together'",
            PreferredRegionKw => "'preferred_region'",
            IoDefaultsKw => "'io_defaults'",
            VoltageKw => "'voltage'",
            DeviceKw => "'device'",
            GroupKw => "'group'",
            FastKw => "'fast'",
            SlowKw => "'slow'",
            MediumKw => "'medium'",
            UpKw => "'up'",
            DownKw => "'down'",
            NoneKw => "'none'",
            KeeperKw => "'keeper'",

            Ident => "identifier",
            IntLiteral => "integer literal",
            BinLiteral => "binary literal",
            HexLiteral => "hexadecimal literal",
            FloatLiteral => "floating-point literal",
            StringLiteral => "string literal",
            Lifetime => "lifetime",

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
            Implies => "'|->'",
            ImpliesOverlap => "'|=>'",
            HashHash => "'##'",
            RepeatOpen => "'[*'",
            RepeatClose => "'*]'",
            RepeatPlusOpen => "'[+'",
            RepeatPlusClose => "'+]'",
            RepeatEqualOpen => "'[='",
            RepeatEqualClose => "'=]'",
            At => "'@'",
            Dollar => "'$'",

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
            DotDot => "'..'",
            DotDotEq => "'..='",
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
            TypeAlias => "type alias",
            UseDecl => "use declaration",
            ModuleDecl => "module declaration",
            UsePath => "use path",
            UseTree => "use tree",
            Visibility => "visibility",

            _ => "unknown",
        }
    }
}

/// Convert our lexer tokens to syntax kinds
impl From<crate::lexer::Token> for SyntaxKind {
    fn from(token: crate::lexer::Token) -> Self {
        use crate::lexer::Token;

        match token {
            // Core Hardware Description (12)
            Token::Entity => EntityKw,
            Token::Impl => ImplKw,
            Token::Signal => SignalKw,
            Token::Var => VarKw,
            Token::Const => ConstKw,
            Token::In => InKw,
            Token::Input => InputKw,
            Token::Out => OutKw,
            Token::Output => OutputKw,
            Token::Inout => InoutKw,
            Token::Port => PortKw,
            Token::On => OnKw,
            Token::If => IfKw,
            Token::Else => ElseKw,
            Token::AssignKw => AssignKw,

            // Type System (14) - Updated to include numeric types, bool, and floating-point
            Token::Bit => BitKw,
            Token::Bool => BoolKw,
            Token::Nat => NatKw,
            Token::Int => IntKw,
            Token::Logic => LogicKw,
            Token::Clock => ClockKw,
            Token::Reset => ResetKw,
            Token::Type => TypeKw,
            Token::Stream => StreamKw,
            Token::Fp16 => Fp16Kw,
            Token::Fp32 => Fp32Kw,
            Token::Fp64 => Fp64Kw,
            Token::Vec2 => Vec2Kw,
            Token::Vec3 => Vec3Kw,
            Token::Vec4 => Vec4Kw,
            Token::Struct => StructKw,
            Token::Enum => EnumKw,
            Token::Union => UnionKw,

            // Boolean Literals (2)
            Token::True => TrueKw,
            Token::False => FalseKw,

            // Traits and Generics (5)
            Token::Trait => TraitKw,
            Token::Protocol => ProtocolKw,
            Token::Where => WhereKw,
            Token::SelfKeyword => SelfKw,
            Token::SelfType => SelfTypeKw,

            // Event Control (2)
            Token::Rise => RiseKw,
            Token::Fall => FallKw,

            // Control Flow (2)
            Token::Match => MatchKw,
            Token::For => ForKw,

            // Design Intent (3)
            Token::Intent => IntentKw,
            Token::Flow => FlowKw,
            Token::Requirement => RequirementKw,

            // Testbench Only (5)
            Token::Async => AsyncKw,
            Token::Await => AwaitKw,
            Token::Fn => FnKw,
            Token::Return => ReturnKw,
            Token::Let => LetKw,

            // Type Conversion (1)
            Token::As => AsKw,

            // Module System (4)
            Token::Use => UseKw,
            Token::Mod => ModKw,
            Token::Pub => PubKw,
            Token::With => WithKw,

            // Verification (24)
            Token::Assert => AssertKw,
            Token::Property => PropertyKw,
            Token::Cover => CoverKw,
            Token::Sequence => SequenceKw,
            Token::Assume => AssumeKw,
            Token::Expect => ExpectKw,
            Token::Always => AlwaysKw,
            Token::Eventually => EventuallyKw,
            Token::Until => UntilKw,
            Token::Strong => StrongKw,
            Token::Weak => WeakKw,
            Token::Throughout => ThroughoutKw,
            Token::Covergroup => CovergroupKw,
            Token::Coverpoint => CoverpointKw,
            Token::Bins => BinsKw,
            Token::IgnoreBins => IgnoreBinsKw,
            Token::IllegalBins => IllegalBinsKw,
            Token::Cross => CrossKw,
            Token::Invariant => InvariantKw,
            Token::Safety => SafetyKw,
            Token::Liveness => LivenessKw,
            Token::Bounded => BoundedKw,
            Token::Formal => FormalKw,
            Token::Prove => ProveKw,

            Token::Identifier(_) => Ident,
            Token::DecimalLiteral(_) => IntLiteral,
            Token::BinaryLiteral(_) => BinLiteral,
            Token::HexLiteral(_) => HexLiteral,
            Token::SizedLiteral(_) => IntLiteral, // Treat sized literals as integer literals
            Token::FloatLiteral(_) => FloatLiteral,
            Token::StringLiteral(_) => StringLiteral,
            Token::Lifetime(_) => Lifetime,

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
            Token::FatArrow => FatArrow,
            Token::Arrow => Arrow,
            Token::LeftArrow => LeftArrow,
            Token::Implies => Implies,
            Token::ImpliesOverlap => ImpliesOverlap,
            Token::HashHash => HashHash,
            Token::RepeatOpen => RepeatOpen,
            Token::RepeatClose => RepeatClose,
            Token::RepeatPlusOpen => RepeatPlusOpen,
            Token::RepeatPlusClose => RepeatPlusClose,
            Token::RepeatEqualOpen => RepeatEqualOpen,
            Token::RepeatEqualClose => RepeatEqualClose,
            Token::At => At,
            Token::Dollar => Dollar,

            Token::LeftParen => LParen,
            Token::RightParen => RParen,
            Token::LeftBracket => LBracket,
            Token::RightBracket => RBracket,
            Token::LeftBrace => LBrace,
            Token::RightBrace => RBrace,
            Token::Comma => Comma,
            Token::Semicolon => Semicolon,
            Token::ColonColon => ColonColon,
            Token::Colon => Colon,
            Token::Dot => Dot,
            Token::DotDot => DotDot,
            Token::DotDotEq => DotDotEq,
            Token::Question => Question,
            Token::Apostrophe => Apostrophe,

            // Safety Features (ISO 26262)
            Token::Asil => AsilKw,
            Token::SafetyReq => SafetyReqKw,
            Token::SafetyGoal => SafetyGoalKw,
            Token::Fmea => FmeaKw,
            Token::Fmeda => FmedaKw,
            Token::Psm => PsmKw,
            Token::Lsm => LsmKw,
            Token::Spfm => SpfmKw,
            Token::Lfm => LfmKw,
            Token::Pmhf => PmhfKw,
            Token::PowerDomain => PowerDomainKw,
            Token::Isolation => IsolationKw,
            Token::DiagnosticCoverage => DiagnosticCoverageKw,

            // Physical Constraints (28)
            Token::Constraint => ConstraintKw,
            Token::Physical => PhysicalKw,
            Token::Pin => PinKw,
            Token::Pins => PinsKw,
            Token::PinP => PinPKw,
            Token::PinN => PinNKw,
            Token::IoStandard => IoStandardKw,
            Token::Drive => DriveKw,
            Token::Slew => SlewKw,
            Token::Pull => PullKw,
            Token::DiffTerm => DiffTermKw,
            Token::Schmitt => SchmittKw,
            Token::Bank => BankKw,
            Token::Floorplan => FloorplanKw,
            Token::Region => RegionKw,
            Token::Area => AreaKw,
            Token::Instances => InstancesKw,
            Token::Boundary => BoundaryKw,
            Token::KeepTogether => KeepTogetherKw,
            Token::PreferredRegion => PreferredRegionKw,
            Token::IoDefaults => IoDefaultsKw,
            Token::Voltage => VoltageKw,
            Token::Device => DeviceKw,
            Token::Group => GroupKw,
            Token::Fast => FastKw,
            Token::Slow => SlowKw,
            Token::Medium => MediumKw,
            Token::Up => UpKw,
            Token::Down => DownKw,
            Token::None => NoneKw,
            Token::Keeper => KeeperKw,

            Token::Error => Error,
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
