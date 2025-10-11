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
    /// Trait definition
    Trait(TraitDef),
    /// Trait implementation
    TraitImpl(TraitImpl),
    /// Use statement (import)
    Use(UseDecl),
    /// Module declaration
    Module(ModuleDecl),
}

/// Entity declaration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EntityDecl {
    /// Visibility
    pub visibility: Visibility,
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
    /// Flow statement
    Flow(FlowStatement),
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

/// Flow statement for pipelined designs
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FlowStatement {
    /// Pipeline stages connected by |> operators
    pub pipeline: FlowPipeline,
    /// Span in source code
    pub span: std::ops::Range<usize>,
}

/// Flow pipeline with stages
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FlowPipeline {
    /// Starting expression or stage
    pub start: PipelineStage,
    /// Subsequent stages connected by |>
    pub stages: Vec<PipelineStage>,
    /// Span in source code
    pub span: std::ops::Range<usize>,
}

/// Pipeline stage - can be an expression or a block
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum PipelineStage {
    /// Simple expression
    Expression(Expression),
    /// Block of statements
    Block(BlockStatement),
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
    /// Named type (for generics)
    Named(String),
    /// Generic type with arguments
    Generic(String, Vec<Type>),
    /// Const value type
    Const(i64),
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

/// Trait definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TraitDef {
    /// Visibility
    pub visibility: Visibility,
    /// Trait name
    pub name: String,
    /// Generic parameters
    pub generics: Vec<Generic>,
    /// Super traits
    pub super_traits: Vec<String>,
    /// Trait items
    pub items: Vec<TraitItem>,
    /// Where clause
    pub where_clause: Option<WhereClause>,
    /// Span in source code
    pub span: std::ops::Range<usize>,
}

/// Trait implementation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TraitImpl {
    /// Trait name
    pub trait_name: String,
    /// Target type (entity or type being implemented for)
    pub target: String,
    /// Generic parameters
    pub generics: Vec<Generic>,
    /// Implementation items
    pub items: Vec<TraitImplItem>,
    /// Where clause
    pub where_clause: Option<WhereClause>,
    /// Span in source code
    pub span: std::ops::Range<usize>,
}

/// Items in a trait definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TraitItem {
    /// Method declaration (without body)
    Method(TraitMethod),
    /// Associated type
    Type(TraitType),
    /// Associated constant
    Const(TraitConst),
}

/// Trait method declaration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TraitMethod {
    /// Method name
    pub name: String,
    /// Parameters
    pub params: Vec<(String, Type)>,
    /// Return type
    pub return_type: Option<Type>,
    /// Default implementation
    pub default_impl: Option<Vec<Statement>>,
    /// Span in source code
    pub span: std::ops::Range<usize>,
}

/// Trait associated type
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TraitType {
    /// Type name
    pub name: String,
    /// Bounds on the type
    pub bounds: Vec<String>,
    /// Default type
    pub default: Option<Type>,
    /// Span in source code
    pub span: std::ops::Range<usize>,
}

/// Trait associated constant
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TraitConst {
    /// Constant name
    pub name: String,
    /// Constant type
    pub const_type: Type,
    /// Default value
    pub default: Option<Expression>,
    /// Span in source code
    pub span: std::ops::Range<usize>,
}

/// Items in a trait implementation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TraitImplItem {
    /// Method implementation
    Method(TraitImplMethod),
    /// Associated type definition
    Type(TraitImplType),
    /// Associated constant definition
    Const(TraitImplConst),
}

/// Method implementation in a trait
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TraitImplMethod {
    /// Method name
    pub name: String,
    /// Parameters
    pub params: Vec<(String, Type)>,
    /// Return type
    pub return_type: Option<Type>,
    /// Method body
    pub body: Vec<Statement>,
    /// Span in source code
    pub span: std::ops::Range<usize>,
}

/// Associated type implementation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TraitImplType {
    /// Type name
    pub name: String,
    /// Type value
    pub value: Type,
    /// Span in source code
    pub span: std::ops::Range<usize>,
}

/// Associated constant implementation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TraitImplConst {
    /// Constant name
    pub name: String,
    /// Constant value
    pub value: Expression,
    /// Span in source code
    pub span: std::ops::Range<usize>,
}

/// Where clause for generic constraints
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WhereClause {
    /// Predicates in the where clause
    pub predicates: Vec<WherePredicate>,
    /// Span in source code
    pub span: std::ops::Range<usize>,
}

/// Predicate in a where clause
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WherePredicate {
    /// Type being constrained
    pub type_param: String,
    /// Trait bounds
    pub bounds: Vec<String>,
    /// Span in source code
    pub span: std::ops::Range<usize>,
}

/// Use declaration (import statement)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UseDecl {
    /// Visibility (pub, pub(crate), etc.)
    pub visibility: Visibility,
    /// Path being imported
    pub path: UsePath,
    /// Span in source code
    pub span: std::ops::Range<usize>,
}

/// Path in a use statement
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum UsePath {
    /// Simple path: use foo::bar::Baz;
    Simple { path: Vec<String> },
    /// Renamed import: use foo::bar::Baz as Qux;
    Renamed { path: Vec<String>, alias: String },
    /// Glob import: use foo::bar::*;
    Glob { path: Vec<String> },
    /// Nested imports: use foo::bar::{Baz, Qux};
    Nested {
        prefix: Vec<String>,
        paths: Vec<UsePath>,
    },
}

/// Module declaration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ModuleDecl {
    /// Visibility
    pub visibility: Visibility,
    /// Module name
    pub name: String,
    /// Module items (if inline)
    pub items: Option<Vec<Item>>,
    /// Span in source code
    pub span: std::ops::Range<usize>,
}

/// Visibility modifier
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum Visibility {
    /// Public (pub)
    Public,
    /// Crate-local (pub(crate))
    Crate,
    /// Super-local (pub(super))
    Super,
    /// Private (default)
    Private,
}
