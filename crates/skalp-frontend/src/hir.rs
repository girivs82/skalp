//! HIR - High-level Intermediate Representation
//!
//! Converts the AST into a simplified IR suitable for further processing

use crate::ast::SourceFile;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// High-level Intermediate Representation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Hir {
    /// Module name
    pub name: String,
    /// Entities in this HIR
    pub entities: Vec<HirEntity>,
    /// Implementations
    pub implementations: Vec<HirImplementation>,
    /// Protocols
    pub protocols: Vec<HirProtocol>,
    /// Intents
    pub intents: Vec<HirIntent>,
    /// Requirements
    pub requirements: Vec<HirRequirement>,
}

/// Entity in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirEntity {
    /// Entity identifier
    pub id: EntityId,
    /// Entity name
    pub name: String,
    /// Ports
    pub ports: Vec<HirPort>,
    /// Generic parameters
    pub generics: Vec<HirGeneric>,
}

/// Implementation in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirImplementation {
    /// Target entity
    pub entity: EntityId,
    /// Signals
    pub signals: Vec<HirSignal>,
    /// Variables
    pub variables: Vec<HirVariable>,
    /// Constants
    pub constants: Vec<HirConstant>,
    /// Event blocks
    pub event_blocks: Vec<HirEventBlock>,
    /// Assignments
    pub assignments: Vec<HirAssignment>,
    /// Module instances
    pub instances: Vec<HirInstance>,
}

/// Module instance in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirInstance {
    /// Instance identifier
    pub id: InstanceId,
    /// Instance name
    pub name: String,
    /// Entity to instantiate
    pub entity: EntityId,
    /// Port connections
    pub connections: Vec<HirConnection>,
}

/// Port connection in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirConnection {
    /// Port name
    pub port: String,
    /// Connected expression
    pub expr: HirExpression,
}

/// Port in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirPort {
    /// Port identifier
    pub id: PortId,
    /// Port name
    pub name: String,
    /// Port direction
    pub direction: HirPortDirection,
    /// Port type
    pub port_type: HirType,
}

/// Port direction in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum HirPortDirection {
    Input,
    Output,
    Bidirectional,
}

/// Signal in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirSignal {
    /// Signal identifier
    pub id: SignalId,
    /// Signal name
    pub name: String,
    /// Signal type
    pub signal_type: HirType,
    /// Initial value
    pub initial_value: Option<HirExpression>,
}

/// Variable in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirVariable {
    /// Variable identifier
    pub id: VariableId,
    /// Variable name
    pub name: String,
    /// Variable type
    pub var_type: HirType,
    /// Initial value
    pub initial_value: Option<HirExpression>,
}

/// Constant in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirConstant {
    /// Constant identifier
    pub id: ConstantId,
    /// Constant name
    pub name: String,
    /// Constant type
    pub const_type: HirType,
    /// Constant value
    pub value: HirExpression,
}

/// Event block in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirEventBlock {
    /// Block identifier
    pub id: BlockId,
    /// Event triggers
    pub triggers: Vec<HirEventTrigger>,
    /// Statements
    pub statements: Vec<HirStatement>,
}

/// Event trigger in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirEventTrigger {
    /// Signal being monitored
    pub signal: SignalId,
    /// Edge type
    pub edge: HirEdgeType,
}

/// Edge types in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum HirEdgeType {
    Rising,
    Falling,
    Both,
    Active,    // For reset.active (active level)
    Inactive,  // For reset.inactive (inactive level)
}

/// Assignment in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirAssignment {
    /// Assignment identifier
    pub id: AssignmentId,
    /// Left-hand side
    pub lhs: HirLValue,
    /// Assignment type
    pub assignment_type: HirAssignmentType,
    /// Right-hand side
    pub rhs: HirExpression,
}

/// Assignment types in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum HirAssignmentType {
    Blocking,
    NonBlocking,
    Combinational,
}

/// Left-hand side value in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum HirLValue {
    Signal(SignalId),
    Variable(VariableId),
    Index(Box<HirLValue>, HirExpression),
    Range(Box<HirLValue>, HirExpression, HirExpression),
}

/// Statements in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum HirStatement {
    Assignment(HirAssignment),
    If(HirIfStatement),
    Match(HirMatchStatement),
    Block(Vec<HirStatement>),
}

/// If statement in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirIfStatement {
    /// Condition
    pub condition: HirExpression,
    /// Then statements
    pub then_statements: Vec<HirStatement>,
    /// Else statements
    pub else_statements: Option<Vec<HirStatement>>,
}

/// Match statement in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirMatchStatement {
    /// Expression being matched
    pub expr: HirExpression,
    /// Match arms
    pub arms: Vec<HirMatchArm>,
}

/// Match arm in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirMatchArm {
    /// Pattern
    pub pattern: HirPattern,
    /// Statements
    pub statements: Vec<HirStatement>,
}

/// Expressions in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum HirExpression {
    Literal(HirLiteral),
    Signal(SignalId),
    Variable(VariableId),
    Constant(ConstantId),
    Binary(HirBinaryExpr),
    Unary(HirUnaryExpr),
    Call(HirCallExpr),
    Index(Box<HirExpression>, Box<HirExpression>),
    Range(Box<HirExpression>, Box<HirExpression>, Box<HirExpression>),
}

/// Literals in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum HirLiteral {
    Integer(u64),
    Boolean(bool),
    String(String),
    BitVector(Vec<bool>),
}

/// Binary expression in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirBinaryExpr {
    /// Left operand
    pub left: Box<HirExpression>,
    /// Operator
    pub op: HirBinaryOp,
    /// Right operand
    pub right: Box<HirExpression>,
}

/// Binary operators in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum HirBinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    And,
    Or,
    Xor,
    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    LogicalAnd,
    LogicalOr,
    LeftShift,
    RightShift,
}

/// Unary expression in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirUnaryExpr {
    /// Operator
    pub op: HirUnaryOp,
    /// Operand
    pub operand: Box<HirExpression>,
}

/// Unary operators in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum HirUnaryOp {
    Not,
    Negate,
    BitwiseNot,
}

/// Function call in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirCallExpr {
    /// Function name
    pub function: String,
    /// Arguments
    pub args: Vec<HirExpression>,
}

/// Types in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum HirType {
    Bit(u32),
    Logic(u32),
    Int(u32),
    Nat(u32),
    Clock,
    Reset,
    Event,
    Array(Box<HirType>, u32),
    Custom(String),
}

/// Patterns in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum HirPattern {
    Literal(HirLiteral),
    Variable(String),
    Wildcard,
    Tuple(Vec<HirPattern>),
}

/// Generic parameter in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirGeneric {
    /// Parameter name
    pub name: String,
    /// Parameter type
    pub param_type: HirGenericType,
}

/// Generic parameter types
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum HirGenericType {
    Type,
    Const(HirType),
    Width,
}

/// Protocol in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirProtocol {
    /// Protocol identifier
    pub id: ProtocolId,
    /// Protocol name
    pub name: String,
    /// Protocol signals
    pub signals: Vec<HirProtocolSignal>,
}

/// Protocol signal in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirProtocolSignal {
    /// Signal name
    pub name: String,
    /// Signal direction
    pub direction: HirProtocolDirection,
    /// Signal type
    pub signal_type: HirType,
}

/// Protocol signal directions
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum HirProtocolDirection {
    MasterToSlave,
    SlaveToMaster,
}

/// Intent in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirIntent {
    /// Intent identifier
    pub id: IntentId,
    /// Intent name
    pub name: String,
    /// Intent description
    pub description: String,
    /// Intent constraints
    pub constraints: Vec<HirIntentConstraint>,
}

/// Intent constraint in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirIntentConstraint {
    /// Constraint type
    pub constraint_type: HirConstraintType,
    /// Constraint expression
    pub expr: HirExpression,
}

/// Types of intent constraints
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum HirConstraintType {
    Timing,
    Power,
    Area,
    Performance,
}

/// Requirement in HIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirRequirement {
    /// Requirement identifier
    pub id: RequirementId,
    /// Requirement name
    pub name: String,
    /// Requirement description
    pub description: String,
    /// Verification method
    pub verification: HirVerificationMethod,
}

/// Verification methods for requirements
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum HirVerificationMethod {
    Simulation,
    FormalVerification,
    Testing,
    Review,
}

// ID types for strong typing
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct EntityId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct PortId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct SignalId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct VariableId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ConstantId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct BlockId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct AssignmentId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ProtocolId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct IntentId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct RequirementId(pub u32);

/// Instance identifier
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct InstanceId(pub u32);

/// HIR builder for converting AST to HIR
pub struct HirBuilder {
    /// Next entity ID
    next_entity_id: u32,
    /// Next port ID
    next_port_id: u32,
    /// Next signal ID
    next_signal_id: u32,
    /// Next variable ID
    next_variable_id: u32,
    /// Next constant ID
    next_constant_id: u32,
    /// Next block ID
    next_block_id: u32,
    /// Next assignment ID
    next_assignment_id: u32,
    /// Next protocol ID
    next_protocol_id: u32,
    /// Next intent ID
    next_intent_id: u32,
    /// Next requirement ID
    next_requirement_id: u32,
}

impl HirBuilder {
    /// Create a new HIR builder
    pub fn new() -> Self {
        Self {
            next_entity_id: 0,
            next_port_id: 0,
            next_signal_id: 0,
            next_variable_id: 0,
            next_constant_id: 0,
            next_block_id: 0,
            next_assignment_id: 0,
            next_protocol_id: 0,
            next_intent_id: 0,
            next_requirement_id: 0,
        }
    }

    /// Build HIR from AST
    pub fn build(&mut self, _source_file: &SourceFile) -> Hir {
        // Stub implementation - will be expanded in Week 4
        Hir {
            name: "main".to_string(),
            entities: Vec::new(),
            implementations: Vec::new(),
            protocols: Vec::new(),
            intents: Vec::new(),
            requirements: Vec::new(),
        }
    }

    /// Generate next entity ID
    fn next_entity_id(&mut self) -> EntityId {
        let id = EntityId(self.next_entity_id);
        self.next_entity_id += 1;
        id
    }

    /// Generate next port ID
    fn next_port_id(&mut self) -> PortId {
        let id = PortId(self.next_port_id);
        self.next_port_id += 1;
        id
    }

    /// Generate next signal ID
    fn next_signal_id(&mut self) -> SignalId {
        let id = SignalId(self.next_signal_id);
        self.next_signal_id += 1;
        id
    }
}

impl Default for HirBuilder {
    fn default() -> Self {
        Self::new()
    }
}

impl Hir {
    /// Create a new empty HIR
    pub fn new(name: String) -> Self {
        Self {
            name,
            entities: Vec::new(),
            implementations: Vec::new(),
            protocols: Vec::new(),
            intents: Vec::new(),
            requirements: Vec::new(),
        }
    }
}