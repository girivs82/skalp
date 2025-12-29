//! NCL (Null Convention Logic) Expansion Pass
//!
//! This module transforms synchronous boolean logic into NCL dual-rail logic.
//! It is the core transformation for asynchronous circuit synthesis.
//!
//! # NCL Basics
//!
//! NCL uses dual-rail encoding:
//! - Each logical bit is represented by two wires: (t, f) for true and false rails
//! - NULL state: t=0, f=0 (spacer between data)
//! - DATA_0: t=0, f=1 (logical 0)
//! - DATA_1: t=1, f=0 (logical 1)
//! - INVALID: t=1, f=1 (never occurs in valid NCL)
//!
//! # Threshold Gates (THmn)
//!
//! THmn gate: output = 1 when ≥m of n inputs are 1, output = 0 when all inputs are 0
//! Key gates:
//! - TH12: 1-of-2 (OR with hysteresis)
//! - TH22: 2-of-2 (C-element, AND with hysteresis)
//!
//! # NCL Gate Mappings
//!
//! | Boolean Op | True Rail           | False Rail          |
//! |------------|---------------------|---------------------|
//! | AND(a,b)   | TH22(a_t, b_t)      | TH12(a_f, b_f)      |
//! | OR(a,b)    | TH12(a_t, b_t)      | TH22(a_f, b_f)      |
//! | NOT(a)     | a_f                 | a_t                 |
//! | XOR(a,b)   | complex (see impl)  | complex (see impl)  |

use crate::lir::{Lir, LirNode, LirNodeId, LirOp, LirSignal, LirSignalId};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Configuration for NCL expansion
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NclConfig {
    /// Use weak completion (only monitor outputs) vs strong (monitor all signals)
    pub use_weak_completion: bool,
    /// Maximum depth for completion detection tree (None = automatic)
    pub completion_tree_depth: Option<u32>,
    /// Insert NULL generator at inputs
    pub generate_null_wavefront: bool,
}

impl Default for NclConfig {
    fn default() -> Self {
        Self {
            use_weak_completion: true,
            completion_tree_depth: None,
            generate_null_wavefront: true,
        }
    }
}

/// Result of NCL expansion
#[derive(Debug)]
pub struct NclExpandResult {
    /// The transformed NCL LIR
    pub lir: Lir,
    /// Mapping from original signals to their dual-rail pairs (t, f)
    pub dual_rail_map: HashMap<LirSignalId, DualRailPair>,
    /// Completion signals for each barrier stage
    pub stage_completions: Vec<LirSignalId>,
}

/// A pair of signals representing dual-rail encoding
#[derive(Debug, Clone, Copy)]
pub struct DualRailPair {
    /// True rail signal ID
    pub t: LirSignalId,
    /// False rail signal ID
    pub f: LirSignalId,
}

/// NCL expansion context
struct NclExpander {
    /// The LIR being built
    lir: Lir,
    /// Next signal ID
    next_signal_id: u32,
    /// Next node ID
    next_node_id: u32,
    /// Mapping from original signal to dual-rail pair
    dual_rail_map: HashMap<LirSignalId, DualRailPair>,
    /// Configuration
    config: NclConfig,
}

impl NclExpander {
    fn new(original_name: &str, config: NclConfig) -> Self {
        Self {
            lir: Lir::new(format!("{}_ncl", original_name)),
            next_signal_id: 0,
            next_node_id: 0,
            dual_rail_map: HashMap::new(),
            config,
        }
    }

    /// Allocate a new signal
    fn alloc_signal(&mut self, name: String, width: u32) -> LirSignalId {
        let id = LirSignalId(self.next_signal_id);
        self.next_signal_id += 1;
        self.lir.signals.push(LirSignal {
            id,
            name,
            width,
            driver: None,
            is_input: false,
            is_output: false,
            is_detection: false,
        });
        id
    }

    /// Allocate a new node
    fn alloc_node(
        &mut self,
        op: LirOp,
        inputs: Vec<LirSignalId>,
        output: LirSignalId,
    ) -> LirNodeId {
        let id = LirNodeId(self.next_node_id);
        self.next_node_id += 1;
        self.lir.nodes.push(LirNode {
            id,
            op,
            inputs,
            output,
            path: String::new(),
            clock: None,
            reset: None,
        });
        id
    }

    /// Create dual-rail signals for an original signal
    fn create_dual_rail(&mut self, original: &LirSignal) -> DualRailPair {
        let t = self.alloc_signal(format!("{}_t", original.name), original.width);
        let f = self.alloc_signal(format!("{}_f", original.name), original.width);
        DualRailPair { t, f }
    }

    /// Expand a single-rail AND to NCL dual-rail
    /// AND(a,b): t = TH22(a_t, b_t), f = TH12(a_f, b_f)
    fn expand_and(&mut self, a: DualRailPair, b: DualRailPair, width: u32) -> DualRailPair {
        let t = self.alloc_signal(format!("and_t_{}", self.next_signal_id), width);
        let f = self.alloc_signal(format!("and_f_{}", self.next_signal_id), width);

        // True rail: TH22(a_t, b_t) - both inputs must be true
        self.alloc_node(LirOp::NclAnd { width }, vec![a.t, b.t], t);

        // Actually the AND operation in NCL:
        // - For the TRUE rail: we need a TH22 (C-element) of the true rails
        // - For the FALSE rail: we need a TH12 (OR) of the false rails
        // The NclAnd LirOp should handle this internally

        DualRailPair { t, f }
    }

    /// Expand a single-rail OR to NCL dual-rail
    /// OR(a,b): t = TH12(a_t, b_t), f = TH22(a_f, b_f)
    fn expand_or(&mut self, a: DualRailPair, b: DualRailPair, width: u32) -> DualRailPair {
        let t = self.alloc_signal(format!("or_t_{}", self.next_signal_id), width);
        let f = self.alloc_signal(format!("or_f_{}", self.next_signal_id), width);

        self.alloc_node(LirOp::NclOr { width }, vec![a.t, a.f, b.t, b.f], t);

        DualRailPair { t, f }
    }

    /// Expand a single-rail NOT to NCL dual-rail
    /// NOT(a): t = a_f, f = a_t (just swap rails)
    fn expand_not(&mut self, a: DualRailPair, width: u32) -> DualRailPair {
        let t = self.alloc_signal(format!("not_t_{}", self.next_signal_id), width);
        let f = self.alloc_signal(format!("not_f_{}", self.next_signal_id), width);

        self.alloc_node(LirOp::NclNot { width }, vec![a.t, a.f], t);

        // For NOT, the output t rail is connected to input f rail, and vice versa
        // This is handled by the NclNot operation
        DualRailPair { t, f }
    }

    /// Expand XOR to NCL dual-rail
    /// XOR is more complex:
    /// t = TH22(TH12(a_t,b_f), TH12(a_f,b_t))
    /// f = TH22(TH12(a_t,b_t), TH12(a_f,b_f))
    fn expand_xor(&mut self, a: DualRailPair, b: DualRailPair, width: u32) -> DualRailPair {
        let t = self.alloc_signal(format!("xor_t_{}", self.next_signal_id), width);
        let f = self.alloc_signal(format!("xor_f_{}", self.next_signal_id), width);

        self.alloc_node(LirOp::NclXor { width }, vec![a.t, a.f, b.t, b.f], t);

        DualRailPair { t, f }
    }

    /// Create completion detection for a set of dual-rail signals
    fn create_completion(&mut self, signals: &[DualRailPair]) -> LirSignalId {
        let completion = self.alloc_signal(format!("completion_{}", self.next_signal_id), 1);

        // Collect all t and f rails
        let mut inputs = Vec::new();
        for pair in signals {
            inputs.push(pair.t);
            inputs.push(pair.f);
        }

        let width = signals.len() as u32;
        self.alloc_node(LirOp::NclComplete { width }, inputs, completion);

        completion
    }

    /// Expand an NCL adder (ripple-carry)
    fn expand_add(&mut self, a: DualRailPair, b: DualRailPair, width: u32) -> DualRailPair {
        let t = self.alloc_signal(format!("add_t_{}", self.next_signal_id), width);
        let f = self.alloc_signal(format!("add_f_{}", self.next_signal_id), width);

        // NCL adder takes both rails of both inputs
        self.alloc_node(LirOp::NclAdd { width }, vec![a.t, a.f, b.t, b.f], t);

        DualRailPair { t, f }
    }

    /// Expand an NCL multiplier
    fn expand_mul(&mut self, a: DualRailPair, b: DualRailPair, width: u32) -> DualRailPair {
        let t = self.alloc_signal(format!("mul_t_{}", self.next_signal_id), width);
        let f = self.alloc_signal(format!("mul_f_{}", self.next_signal_id), width);

        self.alloc_node(LirOp::NclMul { width }, vec![a.t, a.f, b.t, b.f], t);

        DualRailPair { t, f }
    }

    /// Expand an NCL less-than comparator
    fn expand_lt(&mut self, a: DualRailPair, b: DualRailPair, width: u32) -> DualRailPair {
        let t = self.alloc_signal(format!("lt_t_{}", self.next_signal_id), 1);
        let f = self.alloc_signal(format!("lt_f_{}", self.next_signal_id), 1);

        self.alloc_node(LirOp::NclLt { width }, vec![a.t, a.f, b.t, b.f], t);

        DualRailPair { t, f }
    }

    /// Expand an NCL equality comparator
    fn expand_eq(&mut self, a: DualRailPair, b: DualRailPair, width: u32) -> DualRailPair {
        let t = self.alloc_signal(format!("eq_t_{}", self.next_signal_id), 1);
        let f = self.alloc_signal(format!("eq_f_{}", self.next_signal_id), 1);

        self.alloc_node(LirOp::NclEq { width }, vec![a.t, a.f, b.t, b.f], t);

        DualRailPair { t, f }
    }

    /// Expand an NCL 2:1 multiplexer
    fn expand_mux2(
        &mut self,
        sel: DualRailPair,
        a: DualRailPair,
        b: DualRailPair,
        width: u32,
    ) -> DualRailPair {
        let t = self.alloc_signal(format!("mux_t_{}", self.next_signal_id), width);
        let f = self.alloc_signal(format!("mux_f_{}", self.next_signal_id), width);

        self.alloc_node(
            LirOp::NclMux2 { width },
            vec![sel.t, sel.f, a.t, a.f, b.t, b.f],
            t,
        );

        DualRailPair { t, f }
    }

    /// Expand a register to NCL register (NULL/DATA latch)
    fn expand_reg(&mut self, d: DualRailPair, width: u32) -> DualRailPair {
        let t = self.alloc_signal(format!("reg_t_{}", self.next_signal_id), width);
        let f = self.alloc_signal(format!("reg_f_{}", self.next_signal_id), width);

        self.alloc_node(LirOp::NclReg { width }, vec![d.t, d.f], t);

        DualRailPair { t, f }
    }

    /// Encode a single-rail input to dual-rail
    fn encode_input(&mut self, signal: LirSignalId, width: u32) -> DualRailPair {
        let t = self.alloc_signal(
            format!("{}_t", self.lir.signals[signal.0 as usize].name),
            width,
        );
        let f = self.alloc_signal(
            format!("{}_f", self.lir.signals[signal.0 as usize].name),
            width,
        );

        self.alloc_node(LirOp::NclEncode { width }, vec![signal], t);

        DualRailPair { t, f }
    }

    /// Decode dual-rail to single-rail output
    fn decode_output(&mut self, pair: DualRailPair, width: u32) -> LirSignalId {
        let output = self.alloc_signal(format!("decoded_{}", self.next_signal_id), width);

        self.alloc_node(LirOp::NclDecode { width }, vec![pair.t, pair.f], output);

        output
    }
}

/// Expand a synchronous LIR to NCL dual-rail logic
///
/// # Arguments
/// * `lir` - The synchronous LIR to transform
/// * `config` - NCL expansion configuration
///
/// # Returns
/// * `NclExpandResult` containing the transformed NCL LIR
pub fn expand_to_ncl(lir: &Lir, config: &NclConfig) -> NclExpandResult {
    let mut expander = NclExpander::new(&lir.name, config.clone());

    // Step 1: Create dual-rail signals for all original signals
    for signal in &lir.signals {
        let pair = expander.create_dual_rail(signal);
        expander.dual_rail_map.insert(signal.id, pair);
    }

    // Step 2: Mark inputs and create encoders if needed
    for &input_id in &lir.inputs {
        if let Some(&pair) = expander.dual_rail_map.get(&input_id) {
            // Mark both rails as inputs
            expander.lir.signals[pair.t.0 as usize].is_input = true;
            expander.lir.signals[pair.f.0 as usize].is_input = true;
            expander.lir.inputs.push(pair.t);
            expander.lir.inputs.push(pair.f);
        }
    }

    // Step 3: Process each node and expand to NCL equivalent
    for node in &lir.nodes {
        let output_pair = expander.dual_rail_map.get(&node.output).copied();

        match &node.op {
            LirOp::And { width } => {
                if node.inputs.len() >= 2 {
                    let a = expander.dual_rail_map.get(&node.inputs[0]).copied();
                    let b = expander.dual_rail_map.get(&node.inputs[1]).copied();
                    if let (Some(a), Some(b), Some(_out)) = (a, b, output_pair) {
                        let result = expander.expand_and(a, b, *width);
                        expander.dual_rail_map.insert(node.output, result);
                    }
                }
            }
            LirOp::Or { width } => {
                if node.inputs.len() >= 2 {
                    let a = expander.dual_rail_map.get(&node.inputs[0]).copied();
                    let b = expander.dual_rail_map.get(&node.inputs[1]).copied();
                    if let (Some(a), Some(b), Some(_out)) = (a, b, output_pair) {
                        let result = expander.expand_or(a, b, *width);
                        expander.dual_rail_map.insert(node.output, result);
                    }
                }
            }
            LirOp::Not { width } => {
                if !node.inputs.is_empty() {
                    let a = expander.dual_rail_map.get(&node.inputs[0]).copied();
                    if let (Some(a), Some(_out)) = (a, output_pair) {
                        let result = expander.expand_not(a, *width);
                        expander.dual_rail_map.insert(node.output, result);
                    }
                }
            }
            LirOp::Xor { width } => {
                if node.inputs.len() >= 2 {
                    let a = expander.dual_rail_map.get(&node.inputs[0]).copied();
                    let b = expander.dual_rail_map.get(&node.inputs[1]).copied();
                    if let (Some(a), Some(b), Some(_out)) = (a, b, output_pair) {
                        let result = expander.expand_xor(a, b, *width);
                        expander.dual_rail_map.insert(node.output, result);
                    }
                }
            }
            LirOp::Add { width, .. } => {
                if node.inputs.len() >= 2 {
                    let a = expander.dual_rail_map.get(&node.inputs[0]).copied();
                    let b = expander.dual_rail_map.get(&node.inputs[1]).copied();
                    if let (Some(a), Some(b), Some(_out)) = (a, b, output_pair) {
                        let result = expander.expand_add(a, b, *width);
                        expander.dual_rail_map.insert(node.output, result);
                    }
                }
            }
            LirOp::Mul { width, .. } => {
                if node.inputs.len() >= 2 {
                    let a = expander.dual_rail_map.get(&node.inputs[0]).copied();
                    let b = expander.dual_rail_map.get(&node.inputs[1]).copied();
                    if let (Some(a), Some(b), Some(_out)) = (a, b, output_pair) {
                        let result = expander.expand_mul(a, b, *width);
                        expander.dual_rail_map.insert(node.output, result);
                    }
                }
            }
            LirOp::Lt { width } => {
                if node.inputs.len() >= 2 {
                    let a = expander.dual_rail_map.get(&node.inputs[0]).copied();
                    let b = expander.dual_rail_map.get(&node.inputs[1]).copied();
                    if let (Some(a), Some(b), Some(_out)) = (a, b, output_pair) {
                        let result = expander.expand_lt(a, b, *width);
                        expander.dual_rail_map.insert(node.output, result);
                    }
                }
            }
            LirOp::Eq { width } => {
                if node.inputs.len() >= 2 {
                    let a = expander.dual_rail_map.get(&node.inputs[0]).copied();
                    let b = expander.dual_rail_map.get(&node.inputs[1]).copied();
                    if let (Some(a), Some(b), Some(_out)) = (a, b, output_pair) {
                        let result = expander.expand_eq(a, b, *width);
                        expander.dual_rail_map.insert(node.output, result);
                    }
                }
            }
            LirOp::Mux2 { width } => {
                if node.inputs.len() >= 3 {
                    let sel = expander.dual_rail_map.get(&node.inputs[0]).copied();
                    let a = expander.dual_rail_map.get(&node.inputs[1]).copied();
                    let b = expander.dual_rail_map.get(&node.inputs[2]).copied();
                    if let (Some(sel), Some(a), Some(b), Some(_out)) = (sel, a, b, output_pair) {
                        let result = expander.expand_mux2(sel, a, b, *width);
                        expander.dual_rail_map.insert(node.output, result);
                    }
                }
            }
            LirOp::Reg { width, .. } => {
                if !node.inputs.is_empty() {
                    let d = expander.dual_rail_map.get(&node.inputs[0]).copied();
                    if let (Some(d), Some(_out)) = (d, output_pair) {
                        let result = expander.expand_reg(d, *width);
                        expander.dual_rail_map.insert(node.output, result);
                    }
                }
            }
            // Handle other operations by passing through or generating placeholders
            _ => {
                // For unsupported operations, create pass-through
                // This will be expanded in later phases
            }
        }
    }

    // Step 4: Mark outputs
    for &output_id in &lir.outputs {
        if let Some(&pair) = expander.dual_rail_map.get(&output_id) {
            expander.lir.signals[pair.t.0 as usize].is_output = true;
            expander.lir.signals[pair.f.0 as usize].is_output = true;
            expander.lir.outputs.push(pair.t);
            expander.lir.outputs.push(pair.f);
        }
    }

    // Step 5: Create completion detection for outputs (weak completion)
    let mut stage_completions = Vec::new();
    if config.use_weak_completion {
        let output_pairs: Vec<DualRailPair> = lir
            .outputs
            .iter()
            .filter_map(|id| expander.dual_rail_map.get(id).copied())
            .collect();

        if !output_pairs.is_empty() {
            let completion = expander.create_completion(&output_pairs);
            stage_completions.push(completion);
        }
    }

    NclExpandResult {
        lir: expander.lir,
        dual_rail_map: expander.dual_rail_map,
        stage_completions,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ncl_config_default() {
        let config = NclConfig::default();
        assert!(config.use_weak_completion);
        assert!(config.completion_tree_depth.is_none());
        assert!(config.generate_null_wavefront);
    }

    #[test]
    fn test_dual_rail_creation() {
        let config = NclConfig::default();
        let mut expander = NclExpander::new("test", config);

        let original = LirSignal {
            id: LirSignalId(0),
            name: "data".to_string(),
            width: 8,
            driver: None,
            is_input: true,
            is_output: false,
            is_detection: false,
        };

        let pair = expander.create_dual_rail(&original);

        // Check that two signals were created
        assert_eq!(expander.lir.signals.len(), 2);
        assert_eq!(expander.lir.signals[pair.t.0 as usize].name, "data_t");
        assert_eq!(expander.lir.signals[pair.f.0 as usize].name, "data_f");
        assert_eq!(expander.lir.signals[pair.t.0 as usize].width, 8);
        assert_eq!(expander.lir.signals[pair.f.0 as usize].width, 8);
    }

    #[test]
    fn test_expand_simple_and() {
        let config = NclConfig::default();
        let mut expander = NclExpander::new("test", config);

        // Create two input signals
        let a = DualRailPair {
            t: expander.alloc_signal("a_t".to_string(), 1),
            f: expander.alloc_signal("a_f".to_string(), 1),
        };
        let b = DualRailPair {
            t: expander.alloc_signal("b_t".to_string(), 1),
            f: expander.alloc_signal("b_f".to_string(), 1),
        };

        let result = expander.expand_and(a, b, 1);

        // Check that output signals were created
        assert!(result.t.0 != a.t.0 && result.t.0 != b.t.0);
        assert!(result.f.0 != a.f.0 && result.f.0 != b.f.0);

        // Check that a node was created
        assert_eq!(expander.lir.nodes.len(), 1);
        assert!(matches!(
            expander.lir.nodes[0].op,
            LirOp::NclAnd { width: 1 }
        ));
    }

    #[test]
    fn test_expand_not_swaps_rails() {
        let config = NclConfig::default();
        let mut expander = NclExpander::new("test", config);

        let a = DualRailPair {
            t: expander.alloc_signal("a_t".to_string(), 1),
            f: expander.alloc_signal("a_f".to_string(), 1),
        };

        let result = expander.expand_not(a, 1);

        // NOT should create new signals that reference the swapped inputs
        assert!(result.t.0 != a.t.0);
        assert!(result.f.0 != a.f.0);

        // Check that a NclNot node was created
        assert_eq!(expander.lir.nodes.len(), 1);
        assert!(matches!(
            expander.lir.nodes[0].op,
            LirOp::NclNot { width: 1 }
        ));
    }

    #[test]
    fn test_completion_detection() {
        let config = NclConfig::default();
        let mut expander = NclExpander::new("test", config);

        let signals = vec![
            DualRailPair {
                t: expander.alloc_signal("sig0_t".to_string(), 1),
                f: expander.alloc_signal("sig0_f".to_string(), 1),
            },
            DualRailPair {
                t: expander.alloc_signal("sig1_t".to_string(), 1),
                f: expander.alloc_signal("sig1_f".to_string(), 1),
            },
        ];

        let completion = expander.create_completion(&signals);

        // Check that completion signal was created
        assert!(expander.lir.signals[completion.0 as usize]
            .name
            .contains("completion"));
        assert_eq!(expander.lir.signals[completion.0 as usize].width, 1);

        // Check that completion node was created
        assert_eq!(expander.lir.nodes.len(), 1);
        assert!(matches!(
            expander.lir.nodes[0].op,
            LirOp::NclComplete { width: 2 }
        ));
    }
}
