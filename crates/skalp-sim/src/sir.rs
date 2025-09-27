//! SIR - Simulation Intermediate Representation
//!
//! The SIR is optimized for GPU-parallel simulation execution.
//! It separates combinational and sequential logic for optimal GPU scheduling.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use bitvec::prelude::*;

/// Clock domain identifier
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ClockDomainId(pub u32);

/// Simulation Intermediate Representation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Sir {
    /// Design name
    pub name: String,
    /// Top-level module
    pub top_module: SirModule,
    /// All modules in the design
    pub modules: HashMap<String, SirModule>,
}

/// Module in SIR format optimized for GPU simulation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SirModule {
    /// Module name
    pub name: String,
    /// All signals in this module (flattened)
    pub signals: Vec<SirSignal>,
    /// Combinational logic blocks (can run in parallel)
    pub comb_blocks: Vec<CombinationalBlock>,
    /// Sequential logic blocks (must run sequentially)
    pub seq_blocks: Vec<SequentialBlock>,
    /// Module instances
    pub instances: Vec<SirInstance>,
    /// Signal connections between instances
    pub connections: Vec<SirConnection>,
}

/// Signal in SIR - represents a wire or register
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SirSignal {
    /// Unique signal ID within module
    pub id: SirSignalId,
    /// Signal name for debugging
    pub name: String,
    /// Signal width in bits
    pub width: usize,
    /// Signal type
    pub signal_type: SirSignalType,
    /// Initial value (for registers)
    pub initial_value: Option<BitVec>,
}

/// Signal ID type
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct SirSignalId(pub u32);

/// Type of signal in simulation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum SirSignalType {
    /// Wire (combinational)
    Wire,
    /// Register (sequential)
    Register {
        /// Clock signal ID
        clock: SirSignalId,
        /// Reset signal ID (optional)
        reset: Option<SirSignalId>,
        /// Reset is active high
        reset_active_high: bool,
    },
    /// Port (interface to parent module)
    Port {
        direction: SirPortDirection,
    },
}

/// Port direction
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum SirPortDirection {
    Input,
    Output,
    InOut,
}

/// Combinational logic block - can execute in parallel on GPU
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CombinationalBlock {
    /// Block ID
    pub id: CombBlockId,
    /// Input signals this block depends on
    pub inputs: Vec<SirSignalId>,
    /// Output signals this block drives
    pub outputs: Vec<SirSignalId>,
    /// Operations in this block
    pub operations: Vec<SirOperation>,
    /// GPU workgroup size hint
    pub workgroup_size_hint: Option<u32>,
}

/// Combinational block ID
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct CombBlockId(pub u32);

/// Sequential logic block - executes on clock edges
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SequentialBlock {
    /// Block ID
    pub id: SeqBlockId,
    /// Clock signal
    pub clock: SirSignalId,
    /// Clock edge type
    pub clock_edge: EdgeType,
    /// Reset signal (optional)
    pub reset: Option<ResetSpec>,
    /// Registers updated by this block
    pub registers: Vec<SirSignalId>,
    /// Operations in this block
    pub operations: Vec<SirOperation>,
}

/// Sequential block ID
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct SeqBlockId(pub u32);

/// Reset specification
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResetSpec {
    /// Reset signal ID
    pub signal: SirSignalId,
    /// Active high or low
    pub active_high: bool,
    /// Edge type (for async reset)
    pub edge: Option<EdgeType>,
}

/// Clock/reset edge type
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum EdgeType {
    Rising,
    Falling,
    Both,
}

/// Single operation in simulation IR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum SirOperation {
    /// Assignment: target = source
    Assign {
        target: SirSignalId,
        source: SirExpression,
    },
    /// Conditional assignment: if (cond) target = source
    ConditionalAssign {
        condition: SirExpression,
        target: SirSignalId,
        source: SirExpression,
    },
    /// Case statement
    Case {
        selector: SirExpression,
        cases: Vec<CaseItem>,
        default: Option<Vec<SirOperation>>,
    },
}

/// Case item in case statement
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CaseItem {
    /// Case values
    pub values: Vec<SirExpression>,
    /// Operations for this case
    pub operations: Vec<SirOperation>,
}

/// Expression in SIR - optimized for GPU evaluation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum SirExpression {
    /// Signal reference
    Signal(SirSignalId),
    /// Constant value
    Constant(BitVec),
    /// Binary operation
    Binary {
        op: BinaryOp,
        left: Box<SirExpression>,
        right: Box<SirExpression>,
    },
    /// Unary operation
    Unary {
        op: UnaryOp,
        operand: Box<SirExpression>,
    },
    /// Bit selection: signal[index]
    BitSelect {
        signal: SirSignalId,
        index: Box<SirExpression>,
    },
    /// Range selection: signal[high:low]
    RangeSelect {
        signal: SirSignalId,
        high: Box<SirExpression>,
        low: Box<SirExpression>,
    },
    /// Concatenation: {a, b, c}
    Concat(Vec<SirExpression>),
    /// Replication: {n{value}}
    Replicate {
        count: Box<SirExpression>,
        value: Box<SirExpression>,
    },
}

/// Binary operations
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum BinaryOp {
    // Arithmetic
    Add, Sub, Mul, Div, Mod,

    // Logical
    And, Or, Xor,

    // Bitwise
    BitwiseAnd, BitwiseOr, BitwiseXor,

    // Comparison
    Equal, NotEqual, Less, LessEqual, Greater, GreaterEqual,

    // Shift
    LeftShift, RightShift,
}

/// Unary operations
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum UnaryOp {
    /// Logical NOT
    Not,
    /// Bitwise NOT
    BitwiseNot,
    /// Arithmetic negation
    Negate,
    /// Reduction operations
    Reduce(ReduceOp),
}

/// Reduction operations
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ReduceOp {
    And, Or, Xor, Nand, Nor, Xnor,
}

/// Module instance in SIR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SirInstance {
    /// Instance name
    pub name: String,
    /// Module being instantiated
    pub module_name: String,
    /// Instance ID
    pub id: InstanceId,
}

/// Instance ID
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct InstanceId(pub u32);

/// Signal connection between modules
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SirConnection {
    /// Source signal (driver)
    pub source: SirSignalId,
    /// Target signal (driven)
    pub target: SirSignalId,
    /// Instance this connection belongs to (if any)
    pub instance: Option<InstanceId>,
}

impl Sir {
    /// Create a new empty SIR
    pub fn new(name: String) -> Self {
        Self {
            name: name.clone(),
            top_module: SirModule::new(name),
            modules: HashMap::new(),
        }
    }

    /// Get the dependency graph for combinational blocks
    /// This is used to determine execution order on GPU
    pub fn get_comb_dependency_graph(&self) -> petgraph::Graph<CombBlockId, ()> {
        use petgraph::Graph;

        let mut graph = Graph::new();
        let mut block_nodes = HashMap::new();

        // Add all combinational blocks as nodes
        for block in &self.top_module.comb_blocks {
            let node = graph.add_node(block.id);
            block_nodes.insert(block.id, node);
        }

        // Add edges based on signal dependencies
        for block in &self.top_module.comb_blocks {
            for input_signal in &block.inputs {
                // Find which block drives this input
                for other_block in &self.top_module.comb_blocks {
                    if other_block.id != block.id && other_block.outputs.contains(input_signal) {
                        // Add dependency edge: other_block -> block
                        if let (Some(&source_node), Some(&target_node)) =
                            (block_nodes.get(&other_block.id), block_nodes.get(&block.id)) {
                            graph.add_edge(source_node, target_node, ());
                        }
                    }
                }
            }
        }

        graph
    }
}

impl SirModule {
    /// Create a new empty module
    pub fn new(name: String) -> Self {
        Self {
            name,
            signals: Vec::new(),
            comb_blocks: Vec::new(),
            seq_blocks: Vec::new(),
            instances: Vec::new(),
            connections: Vec::new(),
        }
    }
}