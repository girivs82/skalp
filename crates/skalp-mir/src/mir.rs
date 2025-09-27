//! MIR - Mid-level Intermediate Representation
//!
//! Architecture-aware representation with optimization opportunities

use serde::{Deserialize, Serialize};

/// Mid-level Intermediate Representation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Mir {
    /// Module name
    pub name: String,
    /// Operations in the design
    pub operations: Vec<Operation>,
    /// Data flow graph
    pub dataflow: DataFlowGraph,
    /// Resource constraints
    pub constraints: ResourceConstraints,
}

/// Hardware operation in MIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Operation {
    /// Operation identifier
    pub id: String,
    /// Operation type
    pub op_type: OperationType,
    /// Input operands
    pub inputs: Vec<Operand>,
    /// Output operands
    pub outputs: Vec<Operand>,
    /// Scheduling information
    pub schedule: Option<ScheduleInfo>,
}

/// Types of operations in MIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum OperationType {
    /// Arithmetic operations
    Add,
    Sub,
    Mul,
    Div,
    /// Logic operations
    And,
    Or,
    Not,
    Xor,
    /// Comparison operations
    Equal,
    Less,
    Greater,
    /// Memory operations
    Load,
    Store,
    /// Control flow
    Branch,
    Merge,
    /// Register operations
    RegisterRead,
    RegisterWrite,
}

/// Operand in MIR operations
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Operand {
    /// Operand identifier
    pub id: String,
    /// Data type
    pub data_type: DataType,
    /// Value (for constants)
    pub value: Option<u64>,
}

/// Data types in MIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum DataType {
    Bit(usize),
    Logic(usize),
    Int(usize),
    Nat(usize),
    Clock,
    Reset,
}

/// Data flow graph representation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DataFlowGraph {
    /// Nodes in the graph
    pub nodes: Vec<DataFlowNode>,
    /// Edges between nodes
    pub edges: Vec<DataFlowEdge>,
}

/// Node in data flow graph
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DataFlowNode {
    /// Node identifier
    pub id: String,
    /// Associated operation
    pub operation: String,
    /// Node properties
    pub properties: NodeProperties,
}

/// Edge in data flow graph
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DataFlowEdge {
    /// Source node
    pub from: String,
    /// Destination node
    pub to: String,
    /// Data carried on this edge
    pub data: String,
}

/// Properties of a data flow node
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NodeProperties {
    /// Execution latency
    pub latency: u32,
    /// Resource requirements
    pub resources: Vec<ResourceRequirement>,
    /// Power consumption
    pub power: f64,
}

/// Resource requirement
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResourceRequirement {
    /// Resource type
    pub resource_type: ResourceType,
    /// Amount required
    pub amount: u32,
}

/// Types of hardware resources
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ResourceType {
    Adder,
    Multiplier,
    Memory,
    Register,
    LogicBlock,
}

/// Resource constraints for the design
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResourceConstraints {
    /// Available resources
    pub available: Vec<ResourceRequirement>,
    /// Timing constraints
    pub timing: TimingConstraints,
}

/// Timing constraints
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TimingConstraints {
    /// Clock period (picoseconds)
    pub clock_period: u32,
    /// Setup time requirements
    pub setup_time: u32,
    /// Hold time requirements
    pub hold_time: u32,
}

/// Scheduling information for an operation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ScheduleInfo {
    /// Clock cycle when operation starts
    pub start_cycle: u32,
    /// Clock cycle when operation completes
    pub end_cycle: u32,
    /// Assigned resource
    pub resource: Option<String>,
}

impl Mir {
    /// Create a new empty MIR
    pub fn new(name: String) -> Self {
        Self {
            name,
            operations: Vec::new(),
            dataflow: DataFlowGraph {
                nodes: Vec::new(),
                edges: Vec::new(),
            },
            constraints: ResourceConstraints {
                available: Vec::new(),
                timing: TimingConstraints {
                    clock_period: 1000, // 1ns default
                    setup_time: 100,
                    hold_time: 50,
                },
            },
        }
    }
}