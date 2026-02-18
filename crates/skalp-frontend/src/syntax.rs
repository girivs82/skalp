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

    // Type System (15) - Updated to include numeric, floating-point, string, and distinct types
    BitKw,
    BoolKw,
    StringKw,
    NatKw,
    IntKw,
    LogicKw,
    ClockKw,
    ResetKw,
    TypeKw,
    StreamKw,
    // NOTE: Fp16Kw, Fp32Kw, Fp64Kw removed - fp types are now regular identifiers
    StructKw,
    EnumKw,
    UnionKw,
    DistinctKw,

    // Boolean Literals (2)
    TrueKw,
    FalseKw,

    // Traits and Generics (5)
    TraitKw,
    ProtocolKw,
    WhereKw,
    SelfKw,
    SelfTypeKw,

    // Event Control (4)
    RiseKw,
    FallKw,
    ActiveKw,
    InactiveKw,

    // Control Flow (4)
    MatchKw,
    ForKw,
    GenerateKw,
    StepKw,

    // Design Intent (3)
    IntentKw,
    FlowKw,
    RequirementKw,

    // Async/NCL and Testbench (7)
    AsyncKw,
    AwaitKw,
    BarrierKw, // NCL pipeline stage boundary
    FnKw,
    ReturnKw,
    LetKw,
    MutKw,

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
    SafetyEntityKw,
    SafetyTraitKw,
    FmeaTraitKw,
    HsiTraitKw,
    FmeaKw,
    FmedaKw,
    FmedaLibraryKw,
    PsmKw,
    LsmKw,
    HsrKw,
    DhsrKw,
    HsiKw,
    CoversKw,
    FttiKw,
    // TargetKw removed - "target" is now a contextual keyword
    TracesToKw,
    ImplementsKw,
    DecomposesKw,
    VerificationKw,
    DetectionTimeKw,
    DcKw,
    LcKw,
    IntervalKw,
    SpfmKw,
    LfmKw,
    PmhfKw,
    FailureModeKw,
    SeverityKw,
    ComponentKw,
    LibraryKw,
    PartKw,
    ExcludeKw,
    MaxLatencyKw,
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
    // VoltageKw removed - now parsed as identifier
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
    // NOTE: NonBlockingAssign (<= for assignment) and BlockingAssign (:=) both removed
    // Using unified `=` operator with context-based inference for all assignments
    // `<=` is now always parsed as LessEqual (Le) comparison operator
    Assign,           // =
    Eq,               // ==
    Neq,              // !=
    Lt,               // <
    Gt,               // >
    Le,               // <=
    Ge,               // >=
    Plus,             // +
    Minus,            // -
    Star,             // *
    Slash,            // /
    Percent,          // %
    Amp,              // &
    Pipe,             // |
    Caret,            // ^
    Bang,             // !
    Tilde,            // ~
    AmpAmp,           // &&
    PipePipe,         // ||
    Shl,              // <<
    Shr,              // >>
    Pipeline,         // |>
    FatArrow,         // =>
    Arrow,            // ->
    LeftArrow,        // <-
    Implies,          // |->
    ImpliesOverlap,   // |=>
    HashHash,         // ##
    HashBracket,      // #[ (for attributes)
    RepeatOpen,       // [*
    RepeatClose,      // *]
    RepeatPlusOpen,   // [+
    RepeatPlusClose,  // +]
    RepeatEqualOpen,  // [=
    RepeatEqualClose, // =]
    At,               // @
    Dollar,           // $

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
    EntityAlias,
    ImplBlock,
    ProtocolDecl,
    IntentDecl,
    IntentValue,   // Single-line intent value: mux_style::parallel
    Attribute,     // #[parallel] or #[parallel + critical]
    AttributeList, // List of attributes
    RequirementDecl,
    TraitDef,
    TraitImpl,
    StructDecl,
    EnumDecl,
    UnionDecl,
    TypeAlias,
    DistinctTypeDecl,
    UseDecl,
    ModuleDecl,

    // Safety declarations (ISO 26262)
    SafetyGoalDecl,
    SafetyEntityDecl,
    SafetyTraitDecl,
    FmeaTraitDecl,
    HsiTraitDecl,
    FmedaLibraryDecl,

    // Safety goal parts
    SafetyGoalBody,
    SafetyTargetBlock,
    HsrDecl,
    DhsrDecl,
    PsmDecl,
    LsmDecl,
    HsiDecl,
    HsiTimingBlock,
    FmeaBlock,
    FmeaComponentDecl,
    FailureModeDecl,
    TracesToList,
    VerificationList,
    CoversBlock,

    // Safety entity parts
    SafetyEntityBody,
    SafetyEntityInstance,
    SafetyTraitUsage,
    PsmOverride,
    LsmOverride,

    // FMEDA library parts
    FmedaLibraryBody,
    FmedaComponentDecl,
    FmedaMechanismDecl,

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
    BarrierStmt, // NCL pipeline stage boundary statement
    AssertStmt,
    PropertyStmt,
    CoverStmt,
    SequenceStmt,
    AssumeStmt,
    ExpectStmt,
    GenerateForStmt,
    GenerateIfStmt,
    GenerateMatchStmt,

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
    SeveritySpec,    // severity: Error/Warning/Info/Fatal specification
    AssumeMacroStmt, // Simple assume!(cond) statement (without property keyword)
    CoverMacroStmt,  // Simple cover!(cond) statement (without property keyword)
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
    ReplicateExpr, // Verilog-style replication: {N{expr}}
    TernaryExpr,
    TupleExpr,
    RangeExpr,
    BlockExpr,
    ClosureExpr,
    ClosureParamList,
    ClosureParam,
    WithIntentExpr, // expr with intent::name
    IntentRef,      // intent::name reference

    // Types
    TypeExpr,
    TypeAnnotation,
    BitType,
    BoolType,
    StringType,
    LogicType,
    IntType,
    NatType,
    ClockType,
    ResetType,
    EventType,
    StreamType,
    // NOTE: Fp16Type, Fp32Type, Fp64Type removed - fp types are now IdentType
    NclType, // NCL dual-rail type
    IdentType,
    ArrayType,
    ArraySize,
    TupleType,
    CustomType,
    SelfType,
    StructType,
    EnumType,
    InlineStructType,
    InlineEnumType,
    InlineUnionType,

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
    NamedArg, // For named generic arguments like `WIDTH: 32`
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
            // Type System (13) - fp16/fp32/fp64 removed as keywords
            | BitKw | BoolKw | StringKw | NatKw | IntKw | LogicKw | ClockKw | ResetKw | TypeKw | StreamKw | StructKw | EnumKw | UnionKw | DistinctKw
            // Boolean Literals (2)
            | TrueKw | FalseKw
            // Traits and Generics (5)
            | TraitKw | ProtocolKw | WhereKw | SelfKw | SelfTypeKw
            // Event Control (4)
            | RiseKw | FallKw | ActiveKw | InactiveKw
            // Control Flow (4)
            | MatchKw | ForKw | GenerateKw | StepKw
            // Design Intent (3)
            | IntentKw | FlowKw | RequirementKw
            // Async/NCL and Testbench (7)
            | AsyncKw | AwaitKw | BarrierKw | FnKw | ReturnKw | LetKw | MutKw
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
            | AsilKw | SafetyReqKw | SafetyGoalKw | SafetyEntityKw | SafetyTraitKw
            | FmeaTraitKw | HsiTraitKw | FmeaKw | FmedaKw | FmedaLibraryKw
            | PsmKw | LsmKw | HsrKw | DhsrKw | HsiKw
            | CoversKw | FttiKw | TracesToKw | ImplementsKw | DecomposesKw
            | VerificationKw | DetectionTimeKw | DcKw | LcKw | IntervalKw
            | SpfmKw | LfmKw | PmhfKw | FailureModeKw | SeverityKw
            | ComponentKw | LibraryKw | PartKw | ExcludeKw | MaxLatencyKw
            | PowerDomainKw | IsolationKw | DiagnosticCoverageKw
            // Physical Constraints (28)
            | ConstraintKw | PhysicalKw | PinKw | PinsKw | PinPKw | PinNKw
            | IoStandardKw | DriveKw | SlewKw | PullKw | DiffTermKw | SchmittKw
            | BankKw | FloorplanKw | RegionKw | AreaKw | InstancesKw | BoundaryKw
            | KeepTogetherKw | PreferredRegionKw | IoDefaultsKw
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

            // Type System (15)
            BitKw => "'bit'",
            BoolKw => "'bool'",
            StringKw => "'string'",
            NatKw => "'nat'",
            IntKw => "'int'",
            LogicKw => "'logic'",
            ClockKw => "'clock'",
            ResetKw => "'reset'",
            TypeKw => "'type'",
            StreamKw => "'stream'",
            // Fp16Kw, Fp32Kw, Fp64Kw removed - fp types are now identifiers
            StructKw => "'struct'",
            EnumKw => "'enum'",
            UnionKw => "'union'",
            DistinctKw => "'distinct'",

            // Boolean Literals (2)
            TrueKw => "'true'",
            FalseKw => "'false'",

            // Traits and Generics (5)
            TraitKw => "'trait'",
            ProtocolKw => "'protocol'",
            WhereKw => "'where'",
            SelfKw => "'self'",
            SelfTypeKw => "'Self'",

            // Event Control (4)
            RiseKw => "'rise'",
            FallKw => "'fall'",
            ActiveKw => "'active'",
            InactiveKw => "'inactive'",

            // Control Flow (4)
            MatchKw => "'match'",
            ForKw => "'for'",
            GenerateKw => "'generate'",
            StepKw => "'step'",

            // Design Intent (3)
            IntentKw => "'intent'",
            FlowKw => "'flow'",
            RequirementKw => "'requirement'",

            // Async/NCL and Testbench (7)
            AsyncKw => "'async'",
            AwaitKw => "'await'",
            BarrierKw => "'barrier'",
            FnKw => "'fn'",
            ReturnKw => "'return'",
            LetKw => "'let'",
            MutKw => "'mut'",

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
            SafetyEntityKw => "'safety_entity'",
            SafetyTraitKw => "'safety_trait'",
            FmeaTraitKw => "'fmea_trait'",
            HsiTraitKw => "'hsi_trait'",
            FmeaKw => "'fmea'",
            FmedaKw => "'fmeda'",
            FmedaLibraryKw => "'fmeda_library'",
            PsmKw => "'psm'",
            LsmKw => "'lsm'",
            HsrKw => "'hsr'",
            DhsrKw => "'dhsr'",
            HsiKw => "'hsi'",
            CoversKw => "'covers'",
            FttiKw => "'ftti'",
            TracesToKw => "'traces_to'",
            ImplementsKw => "'implements'",
            DecomposesKw => "'decomposes'",
            VerificationKw => "'verification'",
            DetectionTimeKw => "'detection_time'",
            DcKw => "'dc'",
            LcKw => "'lc'",
            IntervalKw => "'interval'",
            SpfmKw => "'spfm'",
            LfmKw => "'lfm'",
            PmhfKw => "'pmhf'",
            FailureModeKw => "'failure_mode'",
            SeverityKw => "'severity'",
            ComponentKw => "'component'",
            LibraryKw => "'library'",
            PartKw => "'part'",
            ExcludeKw => "'exclude'",
            MaxLatencyKw => "'max_latency'",
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

            // NonBlockingAssign and BlockingAssign removed - using unified `=` operator
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
            HashBracket => "'#['",
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
            EntityAlias => "entity alias",
            ImplBlock => "implementation block",
            ProtocolDecl => "protocol declaration",
            IntentDecl => "intent declaration",
            IntentValue => "intent value",
            Attribute => "attribute",
            AttributeList => "attribute list",
            RequirementDecl => "requirement declaration",
            TypeAlias => "type alias",
            DistinctTypeDecl => "distinct type declaration",
            UseDecl => "use declaration",
            ModuleDecl => "module declaration",
            UsePath => "use path",
            UseTree => "use tree",
            Visibility => "visibility",
            WithIntentExpr => "expression with intent",
            IntentRef => "intent reference",

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

            // Type System (15) - Updated to include numeric types, bool, string, and floating-point
            Token::Bit => BitKw,
            Token::Bool => BoolKw,
            Token::String => StringKw,
            Token::Nat => NatKw,
            Token::Int => IntKw,
            Token::Logic => LogicKw,
            Token::Clock => ClockKw,
            Token::Reset => ResetKw,
            Token::Type => TypeKw,
            Token::Stream => StreamKw,
            // NOTE: Token::Fp16/32/64 removed - fp types are now regular identifiers
            Token::Struct => StructKw,
            Token::Enum => EnumKw,
            Token::Union => UnionKw,
            Token::Distinct => DistinctKw,

            // Boolean Literals (2)
            Token::True => TrueKw,
            Token::False => FalseKw,

            // Traits and Generics (5)
            Token::Trait => TraitKw,
            Token::Protocol => ProtocolKw,
            Token::Where => WhereKw,
            Token::SelfKeyword => SelfKw,
            Token::SelfType => SelfTypeKw,

            // Event Control (4)
            Token::Rise => RiseKw,
            Token::Fall => FallKw,
            Token::Active => ActiveKw,
            Token::Inactive => InactiveKw,

            // Control Flow (4)
            Token::Match => MatchKw,
            Token::For => ForKw,
            Token::Generate => GenerateKw,
            Token::Step => StepKw,

            // Design Intent (3)
            Token::Intent => IntentKw,
            Token::Flow => FlowKw,
            Token::Requirement => RequirementKw,

            // Async/NCL and Testbench (7)
            Token::Async => AsyncKw,
            Token::Await => AwaitKw,
            Token::Barrier => BarrierKw,
            Token::Fn => FnKw,
            Token::Return => ReturnKw,
            Token::Let => LetKw,
            Token::Mut => MutKw,

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

            // Token::NonBlockingAssign and Token::BlockingAssign removed - using unified `=`
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
            Token::HashBracket => HashBracket,
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
            Token::SafetyEntity => SafetyEntityKw,
            Token::SafetyTrait => SafetyTraitKw,
            Token::FmeaTrait => FmeaTraitKw,
            Token::HsiTrait => HsiTraitKw,
            Token::Fmea => FmeaKw,
            Token::Fmeda => FmedaKw,
            Token::FmedaLibrary => FmedaLibraryKw,
            Token::Psm => PsmKw,
            Token::Lsm => LsmKw,
            Token::Hsr => HsrKw,
            Token::Dhsr => DhsrKw,
            Token::Hsi => HsiKw,
            Token::Covers => CoversKw,
            Token::Ftti => FttiKw,
            // Token::Target removed - now a contextual keyword
            Token::TracesTo => TracesToKw,
            Token::Implements => ImplementsKw,
            Token::Decomposes => DecomposesKw,
            Token::Verification => VerificationKw,
            Token::DetectionTime => DetectionTimeKw,
            Token::Dc => DcKw,
            Token::Lc => LcKw,
            Token::Interval => IntervalKw,
            Token::Spfm => SpfmKw,
            Token::Lfm => LfmKw,
            Token::Pmhf => PmhfKw,
            Token::FailureMode => FailureModeKw,
            Token::Severity => SeverityKw,
            Token::Component => ComponentKw,
            Token::Library => LibraryKw,
            Token::Part => PartKw,
            Token::Exclude => ExcludeKw,
            Token::MaxLatency => MaxLatencyKw,
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
            // Token::Voltage removed - now parsed as identifier
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
