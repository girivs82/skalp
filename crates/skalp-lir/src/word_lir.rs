//! Word-Level LIR - Higher-level intermediate representation
//!
//! This module provides a word-level representation of hardware designs,
//! preserving multi-bit operations for smarter technology mapping.
//!
//! Unlike the gate-level `Lir`, `WordLir` keeps operations like `Add { width: 8 }`
//! intact, deferring decomposition to the technology mapping phase.
//!
//! # Flow
//!
//! ```text
//! MIR → WordLir (word-level) → TechMapper → GateNetlist (gate-level with library cells)
//! ```
//!
//! # Why Word-Level?
//!
//! - Technology libraries may have compound cells (ADDER8, MUX4, AOI22)
//! - Eager decomposition loses information needed for optimal mapping
//! - Decomposition decisions are technology-dependent

use crate::lir::LirSafetyInfo;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

// ============================================================================
// Word-Level Operations
// ============================================================================

/// Word-level operation types.
///
/// These represent multi-bit operations that will be decomposed
/// during technology mapping based on available library cells.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum WordOp {
    // === Arithmetic ===
    /// Addition: result = a + b (with optional carry-out)
    Add { width: u32, has_carry: bool },
    /// Subtraction: result = a - b (with optional borrow-out)
    Sub { width: u32, has_borrow: bool },
    /// Multiplication: result = a * b
    Mul { width: u32, result_width: u32 },

    // === Bitwise Logic ===
    /// Bitwise AND
    And { width: u32 },
    /// Bitwise OR
    Or { width: u32 },
    /// Bitwise XOR
    Xor { width: u32 },
    /// Bitwise NOT (invert)
    Not { width: u32 },
    /// Bitwise NAND
    Nand { width: u32 },
    /// Bitwise NOR
    Nor { width: u32 },

    // === Comparison ===
    /// Equality: result = (a == b)
    Eq { width: u32 },
    /// Not equal: result = (a != b)
    Ne { width: u32 },
    /// Less than (unsigned): result = (a < b)
    Lt { width: u32 },
    /// Less than or equal (unsigned): result = (a <= b)
    Le { width: u32 },
    /// Greater than (unsigned): result = (a > b)
    Gt { width: u32 },
    /// Greater than or equal (unsigned): result = (a >= b)
    Ge { width: u32 },
    /// Less than (signed)
    Slt { width: u32 },

    // === Multiplexing ===
    /// 2:1 Multiplexer: result = sel ? b : a
    Mux2 { width: u32 },
    /// N:1 Multiplexer
    MuxN { width: u32, ways: u32 },

    // === Shift ===
    /// Logical left shift
    Shl { width: u32 },
    /// Logical right shift
    Shr { width: u32 },
    /// Arithmetic right shift (sign-extend)
    Sar { width: u32 },
    /// Rotate left
    Rol { width: u32 },
    /// Rotate right
    Ror { width: u32 },

    // === Reduction ===
    /// Reduction AND: result = &a (all bits ANDed)
    RedAnd { width: u32 },
    /// Reduction OR: result = |a (all bits ORed)
    RedOr { width: u32 },
    /// Reduction XOR: result = ^a (all bits XORed, parity)
    RedXor { width: u32 },

    // === Bit Manipulation ===
    /// Concatenation: result = {a, b, ...}
    Concat { widths: Vec<u32> },
    /// Bit select: result = a[index]
    BitSelect { width: u32 },
    /// Range select: result = a[high:low]
    RangeSelect { width: u32, high: u32, low: u32 },
    /// Zero extend
    ZeroExtend { from: u32, to: u32 },
    /// Sign extend
    SignExtend { from: u32, to: u32 },

    // === Sequential ===
    /// Register (D flip-flop array)
    Reg {
        width: u32,
        has_enable: bool,
        has_reset: bool,
        reset_value: Option<u64>,
    },
    /// Latch array
    Latch { width: u32 },

    // === Memory ===
    /// Memory read port
    MemRead { data_width: u32, addr_width: u32 },
    /// Memory write port
    MemWrite { data_width: u32, addr_width: u32 },

    // === Special ===
    /// Constant value
    Constant { width: u32, value: u64 },
    /// Buffer (for fanout or pipeline)
    Buffer { width: u32 },
    /// Tristate buffer
    Tristate { width: u32 },
}

impl WordOp {
    /// Returns the output width of this operation
    pub fn output_width(&self) -> u32 {
        match self {
            WordOp::Add { width, has_carry } => {
                if *has_carry {
                    width + 1
                } else {
                    *width
                }
            }
            WordOp::Sub { width, has_borrow } => {
                if *has_borrow {
                    width + 1
                } else {
                    *width
                }
            }
            WordOp::Mul { result_width, .. } => *result_width,
            WordOp::And { width }
            | WordOp::Or { width }
            | WordOp::Xor { width }
            | WordOp::Not { width }
            | WordOp::Nand { width }
            | WordOp::Nor { width } => *width,
            WordOp::Eq { .. }
            | WordOp::Ne { .. }
            | WordOp::Lt { .. }
            | WordOp::Le { .. }
            | WordOp::Gt { .. }
            | WordOp::Ge { .. }
            | WordOp::Slt { .. } => 1, // Comparison results are 1-bit
            WordOp::Mux2 { width } => *width,
            WordOp::MuxN { width, .. } => *width,
            WordOp::Shl { width }
            | WordOp::Shr { width }
            | WordOp::Sar { width }
            | WordOp::Rol { width }
            | WordOp::Ror { width } => *width,
            WordOp::RedAnd { .. } | WordOp::RedOr { .. } | WordOp::RedXor { .. } => 1,
            WordOp::Concat { widths } => widths.iter().sum(),
            WordOp::BitSelect { .. } => 1,
            WordOp::RangeSelect { high, low, .. } => high - low + 1,
            WordOp::ZeroExtend { to, .. } | WordOp::SignExtend { to, .. } => *to,
            WordOp::Reg { width, .. } | WordOp::Latch { width } => *width,
            WordOp::MemRead { data_width, .. } => *data_width,
            WordOp::MemWrite { .. } => 0, // Write has no data output
            WordOp::Constant { width, .. }
            | WordOp::Buffer { width }
            | WordOp::Tristate { width } => *width,
        }
    }

    /// Returns the number of input operands
    pub fn input_count(&self) -> usize {
        match self {
            WordOp::Not { .. }
            | WordOp::RedAnd { .. }
            | WordOp::RedOr { .. }
            | WordOp::RedXor { .. } => 1,
            WordOp::Add { .. }
            | WordOp::Sub { .. }
            | WordOp::Mul { .. }
            | WordOp::And { .. }
            | WordOp::Or { .. }
            | WordOp::Xor { .. }
            | WordOp::Nand { .. }
            | WordOp::Nor { .. }
            | WordOp::Eq { .. }
            | WordOp::Ne { .. }
            | WordOp::Lt { .. }
            | WordOp::Le { .. }
            | WordOp::Gt { .. }
            | WordOp::Ge { .. }
            | WordOp::Slt { .. }
            | WordOp::Shl { .. }
            | WordOp::Shr { .. }
            | WordOp::Sar { .. }
            | WordOp::Rol { .. }
            | WordOp::Ror { .. } => 2,
            WordOp::Mux2 { .. } => 3,                        // sel, a, b
            WordOp::MuxN { ways, .. } => 1 + *ways as usize, // sel + inputs
            WordOp::Concat { widths } => widths.len(),
            WordOp::BitSelect { .. } => 2,   // data, index
            WordOp::RangeSelect { .. } => 1, // data (range is fixed)
            WordOp::ZeroExtend { .. } | WordOp::SignExtend { .. } => 1,
            WordOp::Reg {
                has_enable,
                has_reset,
                ..
            } => {
                1 + if *has_enable { 1 } else { 0 } + if *has_reset { 1 } else { 0 }
                // d + optional en + optional rst
            }
            WordOp::Latch { .. } => 2,    // en, d
            WordOp::MemRead { .. } => 1,  // addr
            WordOp::MemWrite { .. } => 3, // addr, data, we
            WordOp::Constant { .. } => 0,
            WordOp::Buffer { .. } => 1,
            WordOp::Tristate { .. } => 2, // data, enable
        }
    }

    /// Returns true if this operation is sequential (state-holding)
    pub fn is_sequential(&self) -> bool {
        matches!(
            self,
            WordOp::Reg { .. }
                | WordOp::Latch { .. }
                | WordOp::MemRead { .. }
                | WordOp::MemWrite { .. }
        )
    }
}

// ============================================================================
// Word-Level Netlist
// ============================================================================

/// Unique identifier for a word-level node
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct WordNodeId(pub u32);

/// Unique identifier for a word-level signal
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct WordSignalId(pub u32);

/// A node in the word-level netlist (an operation instance)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WordNode {
    /// Unique ID
    pub id: WordNodeId,
    /// Operation type
    pub op: WordOp,
    /// Input signal IDs
    pub inputs: Vec<WordSignalId>,
    /// Output signal ID
    pub output: WordSignalId,
    /// Hierarchical path for traceability
    pub path: String,
    /// Clock signal (for sequential ops)
    pub clock: Option<WordSignalId>,
    /// Reset signal (for sequential ops)
    pub reset: Option<WordSignalId>,
}

/// A signal in the word-level netlist
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WordSignal {
    /// Unique ID
    pub id: WordSignalId,
    /// Signal name
    pub name: String,
    /// Bit width
    pub width: u32,
    /// Driver node (None for primary inputs)
    pub driver: Option<WordNodeId>,
    /// Is this a primary input?
    pub is_input: bool,
    /// Is this a primary output?
    pub is_output: bool,
}

/// Word-level intermediate representation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WordLir {
    /// Design name
    pub name: String,
    /// All nodes (operations)
    pub nodes: Vec<WordNode>,
    /// All signals
    pub signals: Vec<WordSignal>,
    /// Primary input signal IDs
    pub inputs: Vec<WordSignalId>,
    /// Primary output signal IDs
    pub outputs: Vec<WordSignalId>,
    /// Clock signals
    pub clocks: Vec<WordSignalId>,
    /// Reset signals
    pub resets: Vec<WordSignalId>,
    /// Signal name to ID mapping
    signal_map: HashMap<String, WordSignalId>,
    /// Module-level safety information (from MIR SafetyContext)
    /// Propagated to all cells during technology mapping
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub module_safety_info: Option<LirSafetyInfo>,
}

impl WordLir {
    /// Create a new empty word-level LIR
    pub fn new(name: String) -> Self {
        Self {
            name,
            nodes: Vec::new(),
            signals: Vec::new(),
            inputs: Vec::new(),
            outputs: Vec::new(),
            clocks: Vec::new(),
            resets: Vec::new(),
            signal_map: HashMap::new(),
            module_safety_info: None,
        }
    }

    /// Add a signal and return its ID
    pub fn add_signal(&mut self, name: String, width: u32) -> WordSignalId {
        let id = WordSignalId(self.signals.len() as u32);
        self.signal_map.insert(name.clone(), id);
        self.signals.push(WordSignal {
            id,
            name,
            width,
            driver: None,
            is_input: false,
            is_output: false,
        });
        id
    }

    /// Add a primary input signal
    pub fn add_input(&mut self, name: String, width: u32) -> WordSignalId {
        let id = self.add_signal(name, width);
        self.signals[id.0 as usize].is_input = true;
        self.inputs.push(id);
        id
    }

    /// Add a primary output signal
    pub fn add_output(&mut self, name: String, width: u32) -> WordSignalId {
        let id = self.add_signal(name, width);
        self.signals[id.0 as usize].is_output = true;
        self.outputs.push(id);
        id
    }

    /// Add a node (operation) and return its ID
    pub fn add_node(
        &mut self,
        op: WordOp,
        inputs: Vec<WordSignalId>,
        output: WordSignalId,
        path: String,
    ) -> WordNodeId {
        let id = WordNodeId(self.nodes.len() as u32);
        self.signals[output.0 as usize].driver = Some(id);
        self.nodes.push(WordNode {
            id,
            op,
            inputs,
            output,
            path,
            clock: None,
            reset: None,
        });
        id
    }

    /// Add a sequential node with clock and reset
    pub fn add_seq_node(
        &mut self,
        op: WordOp,
        inputs: Vec<WordSignalId>,
        output: WordSignalId,
        path: String,
        clock: WordSignalId,
        reset: Option<WordSignalId>,
    ) -> WordNodeId {
        let id = WordNodeId(self.nodes.len() as u32);
        self.signals[output.0 as usize].driver = Some(id);
        self.nodes.push(WordNode {
            id,
            op,
            inputs,
            output,
            path,
            clock: Some(clock),
            reset,
        });
        id
    }

    /// Get a signal by name
    pub fn get_signal(&self, name: &str) -> Option<&WordSignal> {
        self.signal_map
            .get(name)
            .map(|id| &self.signals[id.0 as usize])
    }

    /// Get total number of operations
    pub fn node_count(&self) -> usize {
        self.nodes.len()
    }

    /// Get total number of signals
    pub fn signal_count(&self) -> usize {
        self.signals.len()
    }

    /// Calculate total bit-width of all signals (for statistics)
    pub fn total_bits(&self) -> u32 {
        self.signals.iter().map(|s| s.width).sum()
    }
}

// ============================================================================
// Statistics
// ============================================================================

/// Statistics for word-level LIR
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct WordLirStats {
    /// Total nodes
    pub total_nodes: usize,
    /// Total signals
    pub total_signals: usize,
    /// Total bits
    pub total_bits: u32,
    /// Arithmetic ops
    pub arithmetic_ops: usize,
    /// Logic ops
    pub logic_ops: usize,
    /// Comparison ops
    pub comparison_ops: usize,
    /// Mux ops
    pub mux_ops: usize,
    /// Sequential ops
    pub sequential_ops: usize,
}

impl WordLirStats {
    /// Calculate statistics from a WordLir
    pub fn from_word_lir(lir: &WordLir) -> Self {
        let mut stats = Self {
            total_nodes: lir.nodes.len(),
            total_signals: lir.signals.len(),
            total_bits: lir.total_bits(),
            ..Default::default()
        };

        for node in &lir.nodes {
            match &node.op {
                WordOp::Add { .. } | WordOp::Sub { .. } | WordOp::Mul { .. } => {
                    stats.arithmetic_ops += 1;
                }
                WordOp::And { .. }
                | WordOp::Or { .. }
                | WordOp::Xor { .. }
                | WordOp::Not { .. }
                | WordOp::Nand { .. }
                | WordOp::Nor { .. } => {
                    stats.logic_ops += 1;
                }
                WordOp::Eq { .. }
                | WordOp::Ne { .. }
                | WordOp::Lt { .. }
                | WordOp::Le { .. }
                | WordOp::Gt { .. }
                | WordOp::Ge { .. }
                | WordOp::Slt { .. } => {
                    stats.comparison_ops += 1;
                }
                WordOp::Mux2 { .. } | WordOp::MuxN { .. } => {
                    stats.mux_ops += 1;
                }
                WordOp::Reg { .. } | WordOp::Latch { .. } => {
                    stats.sequential_ops += 1;
                }
                _ => {}
            }
        }

        stats
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_word_op_output_width() {
        assert_eq!(
            WordOp::Add {
                width: 8,
                has_carry: false
            }
            .output_width(),
            8
        );
        assert_eq!(
            WordOp::Add {
                width: 8,
                has_carry: true
            }
            .output_width(),
            9
        );
        assert_eq!(WordOp::Eq { width: 32 }.output_width(), 1);
        assert_eq!(WordOp::Mux2 { width: 16 }.output_width(), 16);
    }

    #[test]
    fn test_word_lir_creation() {
        let mut lir = WordLir::new("test".to_string());

        let a = lir.add_input("a".to_string(), 8);
        let b = lir.add_input("b".to_string(), 8);
        let sum = lir.add_signal("sum".to_string(), 8);
        let out = lir.add_output("out".to_string(), 8);

        lir.add_node(
            WordOp::Add {
                width: 8,
                has_carry: false,
            },
            vec![a, b],
            sum,
            "add".to_string(),
        );

        lir.add_node(
            WordOp::Buffer { width: 8 },
            vec![sum],
            out,
            "out_buf".to_string(),
        );

        assert_eq!(lir.node_count(), 2);
        assert_eq!(lir.signal_count(), 4);
        assert_eq!(lir.inputs.len(), 2);
        assert_eq!(lir.outputs.len(), 1);
    }

    #[test]
    fn test_word_op_is_sequential() {
        assert!(WordOp::Reg {
            width: 8,
            has_enable: false,
            has_reset: true,
            reset_value: Some(0)
        }
        .is_sequential());
        assert!(!WordOp::Add {
            width: 8,
            has_carry: false
        }
        .is_sequential());
    }
}
