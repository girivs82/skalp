//! SIR - Simulation Intermediate Representation
//!
//! The SIR is optimized for GPU-parallel simulation execution.
//! It separates combinational and sequential logic for optimal GPU scheduling.
//!
//! # Unified Behavioral + Structural Model
//!
//! SIR supports both behavioral (expression-based) and structural (primitive-based)
//! representations in a unified IR. This enables:
//! - Fast functional simulation using behavioral expressions
//! - Gate-level fault injection using structural primitives
//! - Single simulator infrastructure for both modes

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use bitvec::prelude::*;

// Re-export primitive types from LIR for gate-level simulation
pub use skalp_lir::lir::{PrimitiveId, PrimitiveType, NetId};

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
///
/// In behavioral mode, operations are expression-based (Assign, ConditionalAssign, Case).
/// In structural mode, operations are primitive-based (Primitive).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CombinationalBlock {
    /// Block ID
    pub id: CombBlockId,
    /// Input signals this block depends on
    pub inputs: Vec<SirSignalId>,
    /// Output signals this block drives
    pub outputs: Vec<SirSignalId>,
    /// Operations in this block (behavioral or structural)
    pub operations: Vec<SirOperation>,
    /// GPU workgroup size hint
    pub workgroup_size_hint: Option<u32>,
    /// Structural mode info (only set when operations contain Primitives)
    #[serde(default)]
    pub structural_info: Option<StructuralBlockInfo>,
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
///
/// This enum supports both behavioral (expression-based) and structural
/// (primitive-based) operations for unified simulation.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum SirOperation {
    // === Behavioral Operations (expression-based) ===

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

    // === Structural Operations (primitive-based for gate-level simulation) ===

    /// Single primitive gate evaluation
    ///
    /// Used for gate-level simulation with fault injection support.
    /// Each primitive has a unique ID that can be targeted for fault injection.
    Primitive {
        /// Unique primitive ID for fault injection targeting
        id: PrimitiveId,
        /// Type of primitive (AND2, DFF, MUX2, etc.)
        ptype: PrimitiveType,
        /// Input signal IDs
        inputs: Vec<SirSignalId>,
        /// Output signal IDs (most gates have 1, adders have 2)
        outputs: Vec<SirSignalId>,
        /// Hierarchical path for RTL traceability (e.g., "top.cpu.alu.add_0")
        path: String,
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

// ============================================================================
// Gate-Level Simulation Support
// ============================================================================

/// Simulation mode selection
///
/// Controls whether SIR uses behavioral (expression-based) or structural
/// (primitive-based) evaluation during simulation.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub enum SimMode {
    /// Behavioral simulation using SirExpression evaluation
    ///
    /// This is the default mode - fast functional simulation without
    /// gate-level detail. Uses expression trees like `a + b * c`.
    #[default]
    Behavioral,

    /// Structural simulation using primitive evaluation
    ///
    /// Gate-level simulation with support for fault injection.
    /// Each primitive is evaluated individually, enabling precise
    /// fault targeting at the gate/flop level.
    Structural {
        /// Optional fault injection configuration
        fault_config: Option<FaultInjectionConfig>,
    },
}

/// Configuration for fault injection during structural simulation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FaultInjectionConfig {
    /// The specific primitive to inject fault into
    pub target_primitive: PrimitiveId,
    /// Type of fault to inject
    pub fault_type: FaultType,
    /// Cycle number when fault injection starts
    pub inject_at_cycle: u64,
    /// Duration of fault in cycles (None = permanent/stuck fault)
    pub duration: Option<u64>,
}

/// Types of faults that can be injected
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum FaultType {
    /// Output stuck at logic 0 (SA0)
    StuckAt0,
    /// Output stuck at logic 1 (SA1)
    StuckAt1,
    /// Output inverted (bit flip)
    BitFlip,
    /// Transient upset (single-cycle flip, then recovers)
    Transient,
    /// Timing violation (output delayed by N cycles)
    TimingDelay {
        /// Number of cycles to delay
        cycles: u32,
    },
    /// Bridge fault (short between two nets)
    Bridge {
        /// Net ID of the bridged net
        bridged_net: SirSignalId,
        /// Bridge type: AND (wired-AND) or OR (wired-OR)
        bridge_type: BridgeType,
    },
}

/// Type of bridge fault
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum BridgeType {
    /// Wired-AND bridge (output = A & B)
    WiredAnd,
    /// Wired-OR bridge (output = A | B)
    WiredOr,
    /// Dominant fault (bridged signal dominates)
    Dominant,
}

impl FaultInjectionConfig {
    /// Create a stuck-at-0 fault
    pub fn stuck_at_0(target: PrimitiveId, at_cycle: u64) -> Self {
        Self {
            target_primitive: target,
            fault_type: FaultType::StuckAt0,
            inject_at_cycle: at_cycle,
            duration: None, // Permanent
        }
    }

    /// Create a stuck-at-1 fault
    pub fn stuck_at_1(target: PrimitiveId, at_cycle: u64) -> Self {
        Self {
            target_primitive: target,
            fault_type: FaultType::StuckAt1,
            inject_at_cycle: at_cycle,
            duration: None, // Permanent
        }
    }

    /// Create a transient (single-cycle) fault
    pub fn transient(target: PrimitiveId, at_cycle: u64) -> Self {
        Self {
            target_primitive: target,
            fault_type: FaultType::Transient,
            inject_at_cycle: at_cycle,
            duration: Some(1), // Single cycle
        }
    }

    /// Create a bit-flip fault with duration
    pub fn bit_flip(target: PrimitiveId, at_cycle: u64, duration: Option<u64>) -> Self {
        Self {
            target_primitive: target,
            fault_type: FaultType::BitFlip,
            inject_at_cycle: at_cycle,
            duration,
        }
    }

    /// Check if the fault is active at the given cycle
    pub fn is_active_at(&self, cycle: u64) -> bool {
        if cycle < self.inject_at_cycle {
            return false;
        }
        match self.duration {
            None => true, // Permanent fault
            Some(dur) => cycle < self.inject_at_cycle + dur,
        }
    }
}

/// Extended combinational block info for structural mode
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct StructuralBlockInfo {
    /// If true, this block contains Primitive operations (gate-level)
    pub is_structural: bool,
    /// Topological evaluation order for primitives (indices into operations)
    pub eval_order: Option<Vec<usize>>,
    /// Total FIT (Failure In Time) contribution of primitives in this block
    pub total_fit: f64,
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fault_injection_config_stuck_at_0() {
        let config = FaultInjectionConfig::stuck_at_0(PrimitiveId(42), 100);
        assert_eq!(config.target_primitive, PrimitiveId(42));
        assert_eq!(config.fault_type, FaultType::StuckAt0);
        assert_eq!(config.inject_at_cycle, 100);
        assert!(config.duration.is_none()); // Permanent

        // Before injection
        assert!(!config.is_active_at(99));
        // At and after injection (permanent)
        assert!(config.is_active_at(100));
        assert!(config.is_active_at(1000));
        assert!(config.is_active_at(u64::MAX - 1));
    }

    #[test]
    fn test_fault_injection_config_stuck_at_1() {
        let config = FaultInjectionConfig::stuck_at_1(PrimitiveId(7), 50);
        assert_eq!(config.fault_type, FaultType::StuckAt1);
        assert!(!config.is_active_at(49));
        assert!(config.is_active_at(50));
    }

    #[test]
    fn test_fault_injection_config_transient() {
        let config = FaultInjectionConfig::transient(PrimitiveId(99), 200);
        assert_eq!(config.fault_type, FaultType::Transient);
        assert_eq!(config.duration, Some(1)); // Single cycle

        assert!(!config.is_active_at(199));
        assert!(config.is_active_at(200)); // Active at injection cycle
        assert!(!config.is_active_at(201)); // Recovered
    }

    #[test]
    fn test_fault_injection_config_bit_flip_with_duration() {
        let config = FaultInjectionConfig::bit_flip(PrimitiveId(5), 10, Some(5));
        assert_eq!(config.fault_type, FaultType::BitFlip);

        assert!(!config.is_active_at(9));
        assert!(config.is_active_at(10)); // Start
        assert!(config.is_active_at(14)); // Last active cycle
        assert!(!config.is_active_at(15)); // Recovered
    }

    #[test]
    fn test_sim_mode_default_is_behavioral() {
        let mode = SimMode::default();
        assert!(matches!(mode, SimMode::Behavioral));
    }

    #[test]
    fn test_sim_mode_structural() {
        let config = FaultInjectionConfig::stuck_at_0(PrimitiveId(1), 0);
        let mode = SimMode::Structural {
            fault_config: Some(config),
        };
        match mode {
            SimMode::Structural { fault_config } => {
                assert!(fault_config.is_some());
            }
            _ => panic!("Expected Structural mode"),
        }
    }

    #[test]
    fn test_sir_operation_primitive() {
        let op = SirOperation::Primitive {
            id: PrimitiveId(123),
            ptype: PrimitiveType::And { inputs: 2 },
            inputs: vec![SirSignalId(1), SirSignalId(2)],
            outputs: vec![SirSignalId(3)],
            path: "top.cpu.alu.and_0".to_string(),
        };

        match op {
            SirOperation::Primitive { id, ptype, inputs, outputs, path } => {
                assert_eq!(id, PrimitiveId(123));
                assert!(matches!(ptype, PrimitiveType::And { inputs: 2 }));
                assert_eq!(inputs.len(), 2);
                assert_eq!(outputs.len(), 1);
                assert_eq!(path, "top.cpu.alu.and_0");
            }
            _ => panic!("Expected Primitive operation"),
        }
    }

    #[test]
    fn test_structural_block_info_default() {
        let info = StructuralBlockInfo::default();
        assert!(!info.is_structural);
        assert!(info.eval_order.is_none());
        assert_eq!(info.total_fit, 0.0);
    }

    #[test]
    fn test_bridge_fault_type() {
        let fault = FaultType::Bridge {
            bridged_net: SirSignalId(42),
            bridge_type: BridgeType::WiredAnd,
        };
        match fault {
            FaultType::Bridge { bridged_net, bridge_type } => {
                assert_eq!(bridged_net, SirSignalId(42));
                assert!(matches!(bridge_type, BridgeType::WiredAnd));
            }
            _ => panic!("Expected Bridge fault type"),
        }
    }

    #[test]
    fn test_timing_delay_fault() {
        let fault = FaultType::TimingDelay { cycles: 3 };
        match fault {
            FaultType::TimingDelay { cycles } => {
                assert_eq!(cycles, 3);
            }
            _ => panic!("Expected TimingDelay fault type"),
        }
    }

    #[test]
    fn test_combinational_block_with_structural_info() {
        let block = CombinationalBlock {
            id: CombBlockId(1),
            inputs: vec![SirSignalId(1)],
            outputs: vec![SirSignalId(2)],
            operations: vec![
                SirOperation::Primitive {
                    id: PrimitiveId(1),
                    ptype: PrimitiveType::Inv,
                    inputs: vec![SirSignalId(1)],
                    outputs: vec![SirSignalId(2)],
                    path: "top.inv_0".to_string(),
                },
            ],
            workgroup_size_hint: Some(64),
            structural_info: Some(StructuralBlockInfo {
                is_structural: true,
                eval_order: Some(vec![0]),
                total_fit: 0.05,
            }),
        };

        assert!(block.structural_info.is_some());
        let info = block.structural_info.unwrap();
        assert!(info.is_structural);
        assert_eq!(info.total_fit, 0.05);
    }
}