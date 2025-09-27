//! Abstract Syntax Tree for SKALP
//!
//! Represents the parsed structure of SKALP source code

use serde::{Deserialize, Serialize};

/// Root AST node representing a complete SKALP source file
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SourceFile {
    /// Items in this source file
    pub items: Vec<Item>,
}

/// Top-level items in SKALP
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Item {
    /// Entity declaration
    Entity(EntityDecl),
    /// Implementation block
    Impl(ImplBlock),
    /// Protocol definition
    Protocol(ProtocolDecl),
    /// Intent declaration
    Intent(IntentDecl),
    /// Requirement specification
    Requirement(RequirementDecl),
}

/// Entity declaration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EntityDecl {
    /// Entity name
    pub name: String,
    /// Generic parameters
    pub generics: Vec<Generic>,
    /// Port declarations
    pub ports: Vec<PortDecl>,
    /// Span in source code
    pub span: std::ops::Range<usize>,
}

/// Implementation block
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ImplBlock {
    /// Target entity name
    pub entity: String,
    /// Generic parameters
    pub generics: Vec<Generic>,
    /// Items in the implementation
    pub items: Vec<ImplItem>,
    /// Span in source code
    pub span: std::ops::Range<usize>,
}

/// Items inside an implementation block
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ImplItem {
    /// Signal declaration
    Signal(SignalDecl),
    /// Variable declaration
    Variable(VariableDecl),
    /// Constant declaration
    Constant(ConstantDecl),
    /// Event-triggered block
    EventBlock(EventBlock),
    /// Assignment statement
    Assignment(Assignment),
}

/// Port declaration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PortDecl {
    /// Port name
    pub name: String,
    /// Port direction
    pub direction: PortDirection,
    /// Port type
    pub port_type: Type,
    /// Span in source code
    pub span: std::ops::Range<usize>,
}

/// Port direction
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum PortDirection {
    In,
    Out,
    Inout,
}

/// Signal declaration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SignalDecl {
    /// Signal name
    pub name: String,
    /// Signal type
    pub signal_type: Type,
    /// Initial value (optional)
    pub initial_value: Option<Expression>,
    /// Span in source code
    pub span: std::ops::Range<usize>,
}

/// Variable declaration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VariableDecl {
    /// Variable name
    pub name: String,
    /// Variable type
    pub var_type: Type,
    /// Initial value (optional)
    pub initial_value: Option<Expression>,
    /// Span in source code
    pub span: std::ops::Range<usize>,
}

/// Constant declaration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConstantDecl {
    /// Constant name
    pub name: String,
    /// Constant type
    pub const_type: Type,
    /// Constant value
    pub value: Expression,
    /// Span in source code
    pub span: std::ops::Range<usize>,
}

/// Event-triggered block (on clause)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EventBlock {
    /// Event triggers
    pub events: Vec<Event>,
    /// Statements in the block
    pub body: Vec<Statement>,
    /// Span in source code
    pub span: std::ops::Range<usize>,
}

/// Event trigger
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Event {
    /// Signal being monitored
    pub signal: String,
    /// Edge type
    pub edge: EdgeType,
    /// Span in source code
    pub span: std::ops::Range<usize>,
}

/// Edge types for events
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum EdgeType {
    Rise,
    Fall,
    Edge,
}

/// Assignment statement
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Assignment {
    /// Left-hand side
    pub lhs: Expression,
    /// Assignment operator
    pub op: AssignmentOp,
    /// Right-hand side
    pub rhs: Expression,
    /// Span in source code
    pub span: std::ops::Range<usize>,
}

/// Assignment operators
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum AssignmentOp {
    /// Blocking assignment (:=)
    Blocking,
    /// Non-blocking assignment (<=)
    NonBlocking,
    /// Combinational assignment (=)
    Combinational,
}

/// Statements in SKALP
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Statement {
    /// Assignment
    Assignment(Assignment),
    /// If statement
    If(IfStatement),
    /// Match statement
    Match(MatchStatement),
    /// Block statement
    Block(BlockStatement),
}

/// If statement
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IfStatement {
    /// Condition
    pub condition: Expression,
    /// Then block
    pub then_block: Box<Statement>,
    /// Else block (optional)
    pub else_block: Option<Box<Statement>>,
    /// Span in source code
    pub span: std::ops::Range<usize>,
}

/// Match statement
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MatchStatement {
    /// Expression being matched
    pub expr: Expression,
    /// Match arms
    pub arms: Vec<MatchArm>,
    /// Span in source code
    pub span: std::ops::Range<usize>,
}

/// Match arm
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MatchArm {
    /// Pattern
    pub pattern: Pattern,
    /// Body
    pub body: Statement,
    /// Span in source code
    pub span: std::ops::Range<usize>,
}

/// Block statement
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BlockStatement {
    /// Statements in the block
    pub statements: Vec<Statement>,
    /// Span in source code
    pub span: std::ops::Range<usize>,
}

/// Expressions in SKALP
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Expression {
    /// Literal value
    Literal(Literal),
    /// Identifier
    Identifier(String),
    /// Binary operation
    Binary(BinaryExpr),
    /// Unary operation
    Unary(UnaryExpr),
    /// Function call
    Call(CallExpr),
    /// Field access
    Field(FieldExpr),
    /// Index access
    Index(IndexExpr),
}

/// Literal values
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Literal {
    /// Decimal number
    Decimal(u64),
    /// Binary number
    Binary(u64),
    /// Hexadecimal number
    Hex(u64),
    /// String literal
    String(String),
    /// Boolean literal
    Bool(bool),
}

/// Binary expression
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BinaryExpr {
    /// Left operand
    pub left: Box<Expression>,
    /// Operator
    pub op: BinaryOp,
    /// Right operand
    pub right: Box<Expression>,
}

/// Binary operators
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    And,
    Or,
    Xor,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    LogicalAnd,
    LogicalOr,
    LeftShift,
    RightShift,
}

/// Unary expression
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UnaryExpr {
    /// Operator
    pub op: UnaryOp,
    /// Operand
    pub operand: Box<Expression>,
}

/// Unary operators
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum UnaryOp {
    Not,
    Neg,
    BitNot,
}

/// Function call expression
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CallExpr {
    /// Function name
    pub function: String,
    /// Arguments
    pub args: Vec<Expression>,
}

/// Field access expression
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FieldExpr {
    /// Base expression
    pub base: Box<Expression>,
    /// Field name
    pub field: String,
}

/// Index access expression
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IndexExpr {
    /// Base expression
    pub base: Box<Expression>,
    /// Index expression
    pub index: Box<Expression>,
}

/// Types in SKALP
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Type {
    /// Bit type
    Bit(Option<u32>),
    /// Logic type
    Logic(Option<u32>),
    /// Integer type
    Int(Option<u32>),
    /// Natural number type
    Nat(Option<u32>),
    /// Fixed-point type
    Fixed(u32, u32),
    /// Clock type
    Clock,
    /// Reset type
    Reset,
    /// Event type
    Event,
    /// Stream type
    Stream(Box<Type>),
    /// Array type
    Array(Box<Type>, u32),
    /// Custom type
    Custom(String),
}

/// Generic parameter
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Generic {
    /// Parameter name
    pub name: String,
    /// Parameter kind
    pub kind: GenericKind,
}

/// Generic parameter kinds
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum GenericKind {
    /// Type parameter
    Type,
    /// Const parameter
    Const(Type),
    /// Width parameter
    Width,
}

/// Pattern for match statements
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Pattern {
    /// Literal pattern
    Literal(Literal),
    /// Identifier pattern
    Identifier(String),
    /// Wildcard pattern
    Wildcard,
    /// Tuple pattern
    Tuple(Vec<Pattern>),
}

/// Protocol declaration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProtocolDecl {
    /// Protocol name
    pub name: String,
    /// Protocol signals
    pub signals: Vec<ProtocolSignal>,
    /// Span in source code
    pub span: std::ops::Range<usize>,
}

/// Signal in a protocol
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProtocolSignal {
    /// Signal name
    pub name: String,
    /// Signal direction
    pub direction: ProtocolDirection,
    /// Signal type
    pub signal_type: Type,
}

/// Protocol signal directions
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ProtocolDirection {
    /// Master to slave (->)
    MasterToSlave,
    /// Slave to master (<-)
    SlaveToMaster,
}

/// Intent declaration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IntentDecl {
    /// Intent name
    pub name: String,
    /// Intent description
    pub description: String,
    /// Intent constraints
    pub constraints: Vec<IntentConstraint>,
    /// Span in source code
    pub span: std::ops::Range<usize>,
}

/// Intent constraint
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IntentConstraint {
    /// Constraint type
    pub constraint_type: ConstraintType,
    /// Constraint expression
    pub expr: Expression,
}

/// Types of intent constraints
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ConstraintType {
    Timing,
    Power,
    Area,
    Performance,
}

/// Requirement declaration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RequirementDecl {
    /// Requirement ID
    pub id: String,
    /// Requirement description
    pub description: String,
    /// Verification method
    pub verification: VerificationMethod,
    /// Span in source code
    pub span: std::ops::Range<usize>,
}

/// Verification methods for requirements
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum VerificationMethod {
    Simulation,
    FormalVerification,
    Testing,
    Review,
}