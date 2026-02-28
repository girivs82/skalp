/// Syntax kinds for the VHDL language rowan tree
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u16)]
pub enum SyntaxKind {
    // ====================================================================
    // Terminal tokens
    // ====================================================================

    // Design unit keywords
    EntityKw,
    ArchitectureKw,
    PackageKw,
    BodyKw,
    ConfigurationKw,
    LibraryKw,
    UseKw,

    // Port/generic/component
    PortKw,
    GenericKw,
    MapKw,
    ComponentKw,

    // Directions
    InKw,
    OutKw,
    InoutKw,
    BufferKw,

    // Declarations
    SignalKw,
    VariableKw,
    ConstantKw,
    TypeKw,
    SubtypeKw,
    AliasKw,
    AttributeKw,

    // Type constructors
    ArrayKw,
    RecordKw,
    RangeKw,

    // Concurrent
    ProcessKw,
    BeginKw,
    EndKw,
    GenerateKw,
    BlockKw,

    // Sequential
    IfKw,
    ThenKw,
    ElsifKw,
    ElseKw,
    CaseKw,
    WhenKw,
    ForKw,
    LoopKw,
    WhileKw,
    NextKw,
    ExitKw,
    ReturnKw,
    NullKw,
    AssertKw,
    ReportKw,
    SeverityKw,

    // Control
    IsKw,
    OfKw,
    AllKw,
    OthersKw,
    OpenKw,
    WithKw,
    SelectKw,
    UnaffectedKw,

    // Logical operators
    AndKw,
    OrKw,
    XorKw,
    NandKw,
    NorKw,
    XnorKw,
    NotKw,
    ModKw,
    RemKw,
    AbsKw,

    // Shift operators
    SllKw,
    SrlKw,
    SlaKw,
    SraKw,
    RolKw,
    RorKw,

    // Range direction
    ToKw,
    DowntoKw,

    // Subprograms
    FunctionKw,
    ProcedureKw,
    ImpureKw,
    PureKw,

    // Unsynthesizable (detected at parse time)
    WaitKw,
    AfterKw,
    TransportKw,
    RejectKw,
    FileKw,
    AccessKw,
    SharedKw,

    // VHDL-2019
    InterfaceKw,
    ViewKw,
    PrivateKw,

    // Boolean literals
    TrueKw,
    FalseKw,

    // IEEE type keywords
    StdLogicKw,
    StdUlogicKw,
    StdLogicVectorKw,
    StdUlogicVectorKw,
    UnsignedKw,
    SignedKw,
    BooleanKw,
    IntegerKw,
    NaturalKw,
    PositiveKw,
    RealKw,
    StringKw,
    BitKw,
    BitVectorKw,

    // Built-in functions
    RisingEdgeKw,
    FallingEdgeKw,
    ToUnsignedKw,
    ToSignedKw,
    ToIntegerKw,
    ResizeKw,
    ConvIntegerKw,
    ConvStdLogicVectorKw,

    // Literals
    Ident,
    IntLiteral,
    RealLiteral,
    StringLiteral,
    CharLiteral,
    BitStringLiteral,
    BasedLiteral,

    // Operators
    SignalAssign,     // <=
    VarAssign,        // :=
    Arrow,            // =>
    BoxOp,            // <>
    DoubleStar,       // **
    NotEqual,         // /=
    GreaterEqual,     // >=
    DoubleLess,       // <<
    DoubleGreater,    // >>

    // Delimiters
    LParen,
    RParen,
    LBracket,
    RBracket,
    Comma,
    Semicolon,
    Colon,
    Dot,
    Tick,             // '
    Ampersand,        // &
    Bar,              // |
    Plus,
    Minus,
    Star,
    Slash,
    Equal,
    LessThan,
    GreaterThan,
    At,

    // Trivia
    Whitespace,
    Comment,

    // Error
    Error,

    // ====================================================================
    // Non-terminal nodes
    // ====================================================================

    // Top level
    SourceFile,

    // Design units
    LibraryClause,
    UseClause,
    EntityDecl,
    ArchitectureBody,
    PackageDecl,
    PackageBody,

    // Entity parts
    PortClause,
    PortDecl,
    GenericClause,
    GenericDecl,
    PortDirection,

    // Architecture parts
    SignalDecl,
    ConstantDecl,
    VariableDecl,
    TypeDecl,
    SubtypeDecl,
    AliasDecl,
    ComponentDecl,

    // Type definitions
    EnumTypeDef,
    RecordTypeDef,
    RecordField,
    ArrayTypeDef,
    RangeConstraint,

    // Concurrent statements
    ProcessStmt,
    SensitivityList,
    ConcurrentSignalAssign,
    ConditionalAssign,
    SelectedAssign,
    ComponentInst,
    ForGenerate,
    IfGenerate,

    // Sequential statements
    IfStmt,
    CaseStmt,
    CaseAlternative,
    ForLoopStmt,
    WhileLoopStmt,
    SequentialSignalAssign,
    VariableAssignStmt,
    NullStmt,
    ReturnStmt,
    AssertStmt,
    NextStmt,
    ExitStmt,

    // Expressions
    BinaryExpr,
    UnaryExpr,
    ParenExpr,
    NameExpr,
    LiteralExpr,
    FunctionCallExpr,
    IndexExpr,
    SliceExpr,
    FieldAccessExpr,
    QualifiedExpr,
    TypeConversionExpr,
    AggregateExpr,
    AggregateElement,
    AttributeExpr,
    ConcatExpr,

    // Choices (case/aggregate)
    ChoiceList,
    Choice,

    // Subtype indication
    SubtypeIndication,
    RangeExpr,
    DiscreteRange,

    // Port/generic map
    PortMap,
    GenericMap,
    AssociationList,
    AssociationElement,

    // Subprograms
    FunctionDecl,
    FunctionBody,
    ProcedureDecl,
    ProcedureBody,
    ParamList,
    ParamDecl,

    // VHDL-2019 interfaces
    InterfaceDecl,
    ViewDecl,
    ViewFieldDirection,

    // Names
    SelectedName,
    Name,
}

/// Number of SyntaxKind variants (update when adding new variants)
const SYNTAX_KIND_MAX: usize = 256;

impl From<SyntaxKind> for rowan::SyntaxKind {
    fn from(kind: SyntaxKind) -> Self {
        Self(kind as u16)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum VhdlLanguage {}

impl rowan::Language for VhdlLanguage {
    type Kind = SyntaxKind;

    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        assert!((raw.0 as usize) < SYNTAX_KIND_MAX);
        unsafe { std::mem::transmute::<u16, SyntaxKind>(raw.0) }
    }

    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        kind.into()
    }
}

pub type SyntaxNode = rowan::SyntaxNode<VhdlLanguage>;
pub type SyntaxToken = rowan::SyntaxToken<VhdlLanguage>;
pub type SyntaxElement = rowan::NodeOrToken<SyntaxNode, SyntaxToken>;
