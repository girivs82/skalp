//! MIR - Mid-level Intermediate Representation
//!
//! This represents hardware designs at a level suitable for:
//! - SystemVerilog code generation
//! - Optimization passes
//! - Simulation preparation
//!
//! MIR is lower-level than HIR but still hardware-agnostic

use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Mid-level Intermediate Representation for a design
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Mir {
    /// Design name
    pub name: String,
    /// Top-level modules
    pub modules: Vec<Module>,
}

/// A hardware module (corresponds to entity in SKALP)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Module {
    /// Module identifier
    pub id: ModuleId,
    /// Module name
    pub name: String,
    /// Generic parameters
    pub parameters: Vec<GenericParameter>,
    /// Input/output ports
    pub ports: Vec<Port>,
    /// Internal signals
    pub signals: Vec<Signal>,
    /// Variables (for procedural blocks)
    pub variables: Vec<Variable>,
    /// Process blocks (always blocks in Verilog)
    pub processes: Vec<Process>,
    /// Continuous assignments
    pub assignments: Vec<ContinuousAssign>,
    /// Module instances (hierarchy)
    pub instances: Vec<ModuleInstance>,
    /// Clock domains in this module
    pub clock_domains: Vec<ClockDomain>,
}

/// Module identifier
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ModuleId(pub u32);

/// Port of a module
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Port {
    /// Port identifier
    pub id: PortId,
    /// Port name
    pub name: String,
    /// Port direction
    pub direction: PortDirection,
    /// Port type
    pub port_type: DataType,
}

/// Port identifier
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct PortId(pub u32);

/// Port direction
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum PortDirection {
    Input,
    Output,
    InOut,
}

/// Internal signal in a module
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Signal {
    /// Signal identifier
    pub id: SignalId,
    /// Signal name
    pub name: String,
    /// Signal type
    pub signal_type: DataType,
    /// Initial value (if any)
    pub initial: Option<Value>,
    /// Clock domain this signal belongs to (for CDC analysis)
    pub clock_domain: Option<ClockDomainId>,
}

/// Signal identifier
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct SignalId(pub u32);

/// Variable (for procedural context)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Variable {
    /// Variable identifier
    pub id: VariableId,
    /// Variable name
    pub name: String,
    /// Variable type
    pub var_type: DataType,
    /// Initial value
    pub initial: Option<Value>,
}

/// Variable identifier
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct VariableId(pub u32);

/// Data types in MIR
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum DataType {
    /// Bit vector (synthesis-friendly)
    Bit(usize),
    /// Boolean type - true/false (distinct from single-bit hardware signals)
    Bool,
    /// Logic vector (4-state for simulation)
    Logic(usize),
    /// Signed integer
    Int(usize),
    /// Unsigned natural
    Nat(usize),
    /// Clock signal with optional domain
    Clock { domain: Option<ClockDomainId> },
    /// Reset signal with optional domain
    Reset {
        active_high: bool,
        domain: Option<ClockDomainId>,
    },
    /// Event type
    Event,
    /// Struct type
    Struct(Box<StructType>),
    /// Enum type
    Enum(Box<EnumType>),
    /// Union type
    Union(Box<UnionType>),
    /// Array type
    Array(Box<DataType>, usize),
    /// Bit vector with parametric width
    BitParam { param: String, default: usize },
    /// Logic vector with parametric width
    LogicParam { param: String, default: usize },
    /// Signed integer with parametric width
    IntParam { param: String, default: usize },
    /// Unsigned natural with parametric width
    NatParam { param: String, default: usize },
}

/// Clock domain identifier in MIR
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ClockDomainId(pub u32);

/// Clock domain information in MIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ClockDomain {
    /// Domain identifier
    pub id: ClockDomainId,
    /// Domain name
    pub name: String,
    /// Clock signal for this domain
    pub clock_signal: Option<SignalId>,
    /// Reset signal for this domain
    pub reset_signal: Option<SignalId>,
}

/// Process block (maps to always block)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Process {
    /// Process identifier
    pub id: ProcessId,
    /// Process kind
    pub kind: ProcessKind,
    /// Sensitivity list
    pub sensitivity: SensitivityList,
    /// Process body
    pub body: Block,
}

/// Process identifier
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ProcessId(pub u32);

/// Kind of process
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum ProcessKind {
    /// Sequential logic (always_ff)
    Sequential,
    /// Combinational logic (always_comb)
    Combinational,
    /// General process (always)
    General,
}

/// Sensitivity list for a process
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum SensitivityList {
    /// Edge-triggered (posedge/negedge)
    Edge(Vec<EdgeSensitivity>),
    /// Level-sensitive (combinational)
    Level(Vec<LValue>),
    /// Always (continuous)
    Always,
}

/// Edge sensitivity
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EdgeSensitivity {
    /// Signal to monitor
    pub signal: LValue,
    /// Edge type
    pub edge: EdgeType,
}

/// Edge type
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum EdgeType {
    Rising,
    Falling,
    Both,
    Active,   // For reset active level
    Inactive, // For reset inactive level
}

/// Block of statements
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Block {
    /// Statements in the block
    pub statements: Vec<Statement>,
}

/// Statement in a process
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Statement {
    /// Assignment statement
    Assignment(Assignment),
    /// Conditional statement
    If(IfStatement),
    /// Case/match statement
    Case(CaseStatement),
    /// Block of statements
    Block(Block),
    /// Loop statement
    Loop(LoopStatement),
    /// Synthesis-resolved conditional assignment (replaces complex if-else-if chains)
    ResolvedConditional(ResolvedConditional),
}

/// Assignment in a process
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Assignment {
    /// Left-hand side
    pub lhs: LValue,
    /// Right-hand side expression
    pub rhs: Expression,
    /// Assignment kind
    pub kind: AssignmentKind,
}

/// Assignment kind
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum AssignmentKind {
    /// Non-blocking assignment (<=)
    NonBlocking,
    /// Blocking assignment (:= or =)
    Blocking,
}

/// Left-hand value (assignable)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum LValue {
    /// Port reference
    Port(PortId),
    /// Signal reference
    Signal(SignalId),
    /// Variable reference
    Variable(VariableId),
    /// Bit selection
    BitSelect {
        base: Box<LValue>,
        index: Box<Expression>,
    },
    /// Range selection
    RangeSelect {
        base: Box<LValue>,
        high: Box<Expression>,
        low: Box<Expression>,
    },
    /// Concatenation
    Concat(Vec<LValue>),
}

/// Expression (right-hand side)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Expression {
    /// Literal value
    Literal(Value),
    /// LValue reference
    Ref(LValue),
    /// Binary operation
    Binary {
        op: BinaryOp,
        left: Box<Expression>,
        right: Box<Expression>,
    },
    /// Unary operation
    Unary {
        op: UnaryOp,
        operand: Box<Expression>,
    },
    /// Conditional expression (ternary)
    Conditional {
        cond: Box<Expression>,
        then_expr: Box<Expression>,
        else_expr: Box<Expression>,
    },
    /// Concatenation
    Concat(Vec<Expression>),
    /// Replication
    Replicate {
        count: Box<Expression>,
        value: Box<Expression>,
    },
    /// Function call
    FunctionCall { name: String, args: Vec<Expression> },
}

/// Literal value
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Value {
    /// Integer literal
    Integer(i64),
    /// Bit vector literal
    BitVector { width: usize, value: u64 },
    /// String literal
    String(String),
    /// High impedance
    HighZ,
    /// Unknown/undefined
    Unknown,
}

/// Binary operators
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum BinaryOp {
    // Arithmetic
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    // Logical
    And,
    Or,
    Xor,
    // Comparison
    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    // Bitwise
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    // Shift
    LeftShift,
    RightShift,
    // Logical (boolean)
    LogicalAnd,
    LogicalOr,
}

/// Unary operators
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum UnaryOp {
    Not,
    BitwiseNot,
    Negate,
    Reduce(ReduceOp),
}

/// Reduction operators
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum ReduceOp {
    And,
    Or,
    Xor,
    Nand,
    Nor,
    Xnor,
}

/// If statement
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IfStatement {
    /// Condition
    pub condition: Expression,
    /// Then branch
    pub then_block: Block,
    /// Else branch (optional)
    pub else_block: Option<Block>,
}

/// Case statement
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CaseStatement {
    /// Expression to match
    pub expr: Expression,
    /// Case items
    pub items: Vec<CaseItem>,
    /// Default case
    pub default: Option<Block>,
}

/// Case item
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CaseItem {
    /// Values to match
    pub values: Vec<Expression>,
    /// Block to execute
    pub block: Block,
}

/// Loop statement
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum LoopStatement {
    /// For loop
    For {
        init: Box<Assignment>,
        condition: Expression,
        update: Box<Assignment>,
        body: Block,
    },
    /// While loop
    While { condition: Expression, body: Block },
}

/// Continuous assignment (outside processes)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ContinuousAssign {
    /// Left-hand side
    pub lhs: LValue,
    /// Right-hand side
    pub rhs: Expression,
}

/// Generic parameter definition in MIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GenericParameter {
    /// Parameter name
    pub name: String,
    /// Parameter type
    pub param_type: GenericParameterType,
    /// Default value (if any)
    pub default: Option<Value>,
}

/// Types of generic parameters
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum GenericParameterType {
    /// Type parameter
    Type,
    /// Constant parameter with specified type
    Const(DataType),
    /// Width parameter (integer)
    Width,
    /// Clock domain parameter
    ClockDomain,
}

/// Struct type definition in MIR
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct StructType {
    /// Struct name
    pub name: String,
    /// Struct fields
    pub fields: Vec<StructField>,
    /// Whether this is a packed struct (affects layout)
    pub packed: bool,
}

/// Struct field in MIR
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct StructField {
    /// Field name
    pub name: String,
    /// Field type
    pub field_type: DataType,
}

/// Enum type definition in MIR
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct EnumType {
    /// Enum name
    pub name: String,
    /// Enum variants
    pub variants: Vec<EnumVariant>,
    /// Base type for enum values
    pub base_type: DataType,
}

/// Enum variant in MIR
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct EnumVariant {
    /// Variant name
    pub name: String,
    /// Variant value (optional, auto-assigned if None)
    pub value: Option<Value>,
}

/// Union type definition in MIR
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct UnionType {
    /// Union name
    pub name: String,
    /// Union fields (all share the same memory)
    pub fields: Vec<StructField>,
    /// Whether this is a packed union
    pub packed: bool,
}

/// Module instance (hierarchy)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ModuleInstance {
    /// Instance name
    pub name: String,
    /// Module to instantiate
    pub module: ModuleId,
    /// Port connections
    pub connections: HashMap<String, Expression>,
    /// Generic/parameter overrides
    pub parameters: HashMap<String, Value>,
}

/// Synthesis-resolved conditional assignment
/// Contains both original form (for analysis tools) and resolved form (for SIR/LIR generation)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResolvedConditional {
    /// Target signal being assigned
    pub target: LValue,
    /// Assignment kind (blocking/non-blocking)
    pub kind: AssignmentKind,
    /// Original if-else-if chain (preserved for analysis tools)
    pub original: Box<IfStatement>,
    /// Synthesis-resolved priority mux tree
    pub resolved: PriorityMux,
}

/// Priority-encoded multiplexer tree (synthesis-resolved form of if-else-if)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PriorityMux {
    /// Ordered list of condition-value pairs (highest priority first)
    pub cases: Vec<ConditionalCase>,
    /// Default value (when no conditions match)
    pub default: Expression,
}

/// A single condition-value pair in a priority mux
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConditionalCase {
    /// Condition expression
    pub condition: Expression,
    /// Value to select when condition is true
    pub value: Expression,
}

impl Mir {
    /// Create a new MIR
    pub fn new(name: String) -> Self {
        Self {
            name,
            modules: Vec::new(),
        }
    }

    /// Add a module to the MIR
    pub fn add_module(&mut self, module: Module) {
        self.modules.push(module);
    }
}

impl Module {
    /// Create a new module
    pub fn new(id: ModuleId, name: String) -> Self {
        Self {
            id,
            name,
            parameters: Vec::new(),
            ports: Vec::new(),
            signals: Vec::new(),
            variables: Vec::new(),
            processes: Vec::new(),
            assignments: Vec::new(),
            instances: Vec::new(),
            clock_domains: Vec::new(),
        }
    }
}
