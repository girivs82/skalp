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
    /// Logic vector (4-state for simulation)
    Logic(usize),
    /// Signed integer
    Int(usize),
    /// Unsigned natural
    Nat(usize),
    /// Clock signal
    Clock,
    /// Reset signal
    Reset { active_high: bool },
    /// Event type
    Event,
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
    Active,    // For reset active level
    Inactive,  // For reset inactive level
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
    BitSelect { base: Box<LValue>, index: Box<Expression> },
    /// Range selection
    RangeSelect { base: Box<LValue>, high: Box<Expression>, low: Box<Expression> },
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
    Binary { op: BinaryOp, left: Box<Expression>, right: Box<Expression> },
    /// Unary operation
    Unary { op: UnaryOp, operand: Box<Expression> },
    /// Conditional expression (ternary)
    Conditional { cond: Box<Expression>, then_expr: Box<Expression>, else_expr: Box<Expression> },
    /// Concatenation
    Concat(Vec<Expression>),
    /// Replication
    Replicate { count: Box<Expression>, value: Box<Expression> },
    /// Function call
    FunctionCall { name: String, args: Vec<Expression> },
}

/// Literal value
#[derive(Debug, Clone, Serialize, Deserialize)]
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
    Add, Sub, Mul, Div, Mod,
    // Logical
    And, Or, Xor,
    // Comparison
    Equal, NotEqual, Less, LessEqual, Greater, GreaterEqual,
    // Bitwise
    BitwiseAnd, BitwiseOr, BitwiseXor,
    // Shift
    LeftShift, RightShift,
    // Logical (boolean)
    LogicalAnd, LogicalOr,
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
    And, Or, Xor, Nand, Nor, Xnor,
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
    While {
        condition: Expression,
        body: Block,
    },
}

/// Continuous assignment (outside processes)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ContinuousAssign {
    /// Left-hand side
    pub lhs: LValue,
    /// Right-hand side
    pub rhs: Expression,
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
            ports: Vec::new(),
            signals: Vec::new(),
            variables: Vec::new(),
            processes: Vec::new(),
            assignments: Vec::new(),
            instances: Vec::new(),
        }
    }
}