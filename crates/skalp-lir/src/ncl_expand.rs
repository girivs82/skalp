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

/// A signal representing dual-rail encoding.
///
/// For a W-bit logical signal, this is a 2W-bit signal where:
/// - Bits [0, 2, 4, ...] are the true rails
/// - Bits [1, 3, 5, ...] are the false rails
///
/// This allows the tech_mapper to correctly map NCL operations.
#[derive(Debug, Clone, Copy)]
pub struct DualRailSignal {
    /// The combined dual-rail signal ID
    pub id: LirSignalId,
    /// The original logical width (actual signal width is 2x this)
    pub logical_width: u32,
}

/// Legacy pair representation (for backward compatibility during transition)
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
    /// For now, approximate with regular And/Or until TH gates are implemented
    /// Uses the destination dual-rail pair to ensure proper signal connectivity
    fn expand_and(&mut self, a: DualRailPair, b: DualRailPair, dest: DualRailPair, width: u32) {
        // NCL AND:
        // - True rail: dest.t = a_t AND b_t (approximates TH22)
        // - False rail: dest.f = a_f OR b_f (approximates TH12)
        self.alloc_node(LirOp::And { width }, vec![a.t, b.t], dest.t);
        self.alloc_node(LirOp::Or { width }, vec![a.f, b.f], dest.f);
    }

    /// Expand a single-rail OR to NCL dual-rail
    /// OR(a,b): t = TH12(a_t, b_t), f = TH22(a_f, b_f)
    /// For now, approximate with regular Or/And until TH gates are implemented
    fn expand_or(&mut self, a: DualRailPair, b: DualRailPair, dest: DualRailPair, width: u32) {
        // NCL OR:
        // - True rail: dest.t = a_t OR b_t (approximates TH12)
        // - False rail: dest.f = a_f AND b_f (approximates TH22)
        self.alloc_node(LirOp::Or { width }, vec![a.t, b.t], dest.t);
        self.alloc_node(LirOp::And { width }, vec![a.f, b.f], dest.f);
    }

    /// Expand a single-rail NOT to NCL dual-rail
    /// NOT(a): t = a_f, f = a_t (just swap rails)
    /// Uses the destination dual-rail pair to ensure proper signal connectivity
    fn expand_not(&mut self, a: DualRailPair, dest: DualRailPair, width: u32) {
        // NCL NOT just swaps rails: dest.t gets a.f, dest.f gets a.t
        // Create two buffer operations to implement the swap
        // dest.t = buffer(a.f)
        self.alloc_node(LirOp::Buf { width }, vec![a.f], dest.t);
        // dest.f = buffer(a.t)
        self.alloc_node(LirOp::Buf { width }, vec![a.t], dest.f);
    }

    /// Expand a buffer (identity) to NCL dual-rail
    /// BUF(a): t = a_t, f = a_f (copy both rails)
    /// Uses the destination dual-rail pair to ensure proper signal connectivity
    fn expand_buf(&mut self, a: DualRailPair, dest: DualRailPair, width: u32) {
        // NCL buffer copies both rails: dest.t gets a.t, dest.f gets a.f
        // dest.t = buffer(a.t)
        self.alloc_node(LirOp::Buf { width }, vec![a.t], dest.t);
        // dest.f = buffer(a.f)
        self.alloc_node(LirOp::Buf { width }, vec![a.f], dest.f);
    }

    /// Expand XOR to NCL dual-rail
    /// XOR in dual-rail:
    /// - True rail: (a_t AND b_f) OR (a_f AND b_t)
    /// - False rail: (a_t AND b_t) OR (a_f AND b_f)
    fn expand_xor(&mut self, a: DualRailPair, b: DualRailPair, dest: DualRailPair, width: u32) {
        // Intermediate signals
        let at_bf = self.alloc_signal(format!("xor_at_bf_{}", self.next_signal_id), width);
        let af_bt = self.alloc_signal(format!("xor_af_bt_{}", self.next_signal_id), width);
        let at_bt = self.alloc_signal(format!("xor_at_bt_{}", self.next_signal_id), width);
        let af_bf = self.alloc_signal(format!("xor_af_bf_{}", self.next_signal_id), width);

        // a_t AND b_f
        self.alloc_node(LirOp::And { width }, vec![a.t, b.f], at_bf);
        // a_f AND b_t
        self.alloc_node(LirOp::And { width }, vec![a.f, b.t], af_bt);
        // True rail: (a_t AND b_f) OR (a_f AND b_t)
        self.alloc_node(LirOp::Or { width }, vec![at_bf, af_bt], dest.t);

        // a_t AND b_t
        self.alloc_node(LirOp::And { width }, vec![a.t, b.t], at_bt);
        // a_f AND b_f
        self.alloc_node(LirOp::And { width }, vec![a.f, b.f], af_bf);
        // False rail: (a_t AND b_t) OR (a_f AND b_f)
        self.alloc_node(LirOp::Or { width }, vec![at_bt, af_bf], dest.f);
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
    /// For now, use regular Add for true rail, and compute false rail as NOT(true)
    fn expand_add(&mut self, a: DualRailPair, b: DualRailPair, dest: DualRailPair, width: u32) {
        // Simplified approach: compute add using true rails, then derive false rail
        // This is an approximation - proper NCL adder would use threshold gates
        self.alloc_node(
            LirOp::Add {
                width,
                has_carry: false,
            },
            vec![a.t, b.t],
            dest.t,
        );
        // For false rail, invert the true rail (approximation)
        self.alloc_node(LirOp::Not { width }, vec![dest.t], dest.f);
    }

    /// Expand an NCL subtractor
    /// Simplified approach: compute sub using true rails, derive false rail from NOT(true)
    fn expand_sub(&mut self, a: DualRailPair, b: DualRailPair, dest: DualRailPair, width: u32) {
        // Simplified approach: compute subtract using true rails, then derive false rail
        // This is an approximation - proper NCL subtractor would use threshold gates
        self.alloc_node(
            LirOp::Sub {
                width,
                has_borrow: false,
            },
            vec![a.t, b.t],
            dest.t,
        );
        // For false rail, invert the true rail (approximation)
        self.alloc_node(LirOp::Not { width }, vec![dest.t], dest.f);
    }

    /// Expand an NCL multiplier
    /// Simplified approach: compute mul using true rails, derive false rail from NOT(true)
    fn expand_mul(
        &mut self,
        a: DualRailPair,
        b: DualRailPair,
        dest: DualRailPair,
        input_width: u32,
        result_width: u32,
    ) {
        // Simplified approach: compute multiply using true rails, then derive false rail
        // This is an approximation - proper NCL multiplier would use threshold gates
        self.alloc_node(
            LirOp::Mul {
                width: input_width,
                result_width,
            },
            vec![a.t, b.t],
            dest.t,
        );
        // For false rail, invert the true rail (approximation)
        self.alloc_node(
            LirOp::Not {
                width: result_width,
            },
            vec![dest.t],
            dest.f,
        );
    }

    /// Expand an NCL less-than comparator
    /// Simplified approach: compute lt using true rails, derive false rail from NOT(true)
    fn expand_lt(&mut self, a: DualRailPair, b: DualRailPair, dest: DualRailPair, width: u32) {
        // Simplified approach: compute less-than using true rails
        self.alloc_node(LirOp::Lt { width }, vec![a.t, b.t], dest.t);
        // For false rail, invert the true rail (approximation)
        self.alloc_node(LirOp::Not { width: 1 }, vec![dest.t], dest.f);
    }

    /// Expand an NCL equality comparator
    /// Simplified approach: compute eq using true rails, derive false rail from NOT(true)
    fn expand_eq(&mut self, a: DualRailPair, b: DualRailPair, dest: DualRailPair, width: u32) {
        // Simplified approach: compute equality using true rails
        self.alloc_node(LirOp::Eq { width }, vec![a.t, b.t], dest.t);
        // For false rail, invert the true rail (approximation)
        self.alloc_node(LirOp::Not { width: 1 }, vec![dest.t], dest.f);
    }

    /// Expand an NCL 2:1 multiplexer
    /// Simplified approach: compute mux using true rails, derive false rail from NOT(true)
    fn expand_mux2(
        &mut self,
        sel: DualRailPair,
        a: DualRailPair,
        b: DualRailPair,
        dest: DualRailPair,
        width: u32,
    ) {
        // Simplified approach: compute mux using true rails
        self.alloc_node(LirOp::Mux2 { width }, vec![sel.t, a.t, b.t], dest.t);
        // For false rail, invert the true rail (approximation)
        self.alloc_node(LirOp::Not { width }, vec![dest.t], dest.f);
    }

    /// Expand a register to NCL register (NULL/DATA latch)
    /// Simplified approach: use regular register, both rails get same input treated as t/f
    fn expand_reg(&mut self, d: DualRailPair, dest: DualRailPair, width: u32) {
        // Simplified approach: register both rails separately
        // This is an approximation - proper NCL register needs special handling
        self.alloc_node(
            LirOp::Reg {
                width,
                has_enable: false,
                has_reset: false,
                reset_value: None,
            },
            vec![d.t],
            dest.t,
        );
        self.alloc_node(
            LirOp::Reg {
                width,
                has_enable: false,
                has_reset: false,
                reset_value: None,
            },
            vec![d.f],
            dest.f,
        );
    }

    /// Expand a constant to NCL dual-rail encoding
    /// For constant value v with width w:
    /// - t rail has bits set where v has 1s (DATA_TRUE)
    /// - f rail has bits set where v has 0s (DATA_FALSE)
    fn expand_constant(&mut self, value: u64, dest: DualRailPair, width: u32) {
        // Compute the mask for the width
        let mask = if width >= 64 {
            u64::MAX
        } else {
            (1u64 << width) - 1
        };

        // t rail: bits that are 1 in the constant
        self.alloc_node(LirOp::Constant { width, value }, vec![], dest.t);

        // f rail: bits that are 0 in the constant (inverted)
        let f_value = (!value) & mask;
        self.alloc_node(
            LirOp::Constant {
                width,
                value: f_value,
            },
            vec![],
            dest.f,
        );
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

    // Debug: print original LIR structure
    eprintln!("=== NCL Expand: Original LIR ===");
    eprintln!("Signals ({}):", lir.signals.len());
    for sig in &lir.signals {
        eprintln!(
            "  {:?} '{}' w={} in={} out={}",
            sig.id, sig.name, sig.width, sig.is_input, sig.is_output
        );
    }
    eprintln!("Nodes ({}):", lir.nodes.len());
    for node in &lir.nodes {
        eprintln!(
            "  {:?} op={:?} inputs={:?} output={:?}",
            node.id, node.op, node.inputs, node.output
        );
    }
    eprintln!("Inputs: {:?}", lir.inputs);
    eprintln!("Outputs: {:?}", lir.outputs);
    eprintln!("================================");

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
                    if let (Some(a), Some(b), Some(dest)) = (a, b, output_pair) {
                        expander.expand_and(a, b, dest, *width);
                    }
                }
            }
            LirOp::Or { width } => {
                if node.inputs.len() >= 2 {
                    let a = expander.dual_rail_map.get(&node.inputs[0]).copied();
                    let b = expander.dual_rail_map.get(&node.inputs[1]).copied();
                    if let (Some(a), Some(b), Some(dest)) = (a, b, output_pair) {
                        expander.expand_or(a, b, dest, *width);
                    }
                }
            }
            LirOp::Not { width } => {
                if !node.inputs.is_empty() {
                    let a = expander.dual_rail_map.get(&node.inputs[0]).copied();
                    if let (Some(a), Some(dest)) = (a, output_pair) {
                        // Use the existing output dual-rail pair as destination
                        expander.expand_not(a, dest, *width);
                        // No need to update dual_rail_map - dest is already the output's pair
                    }
                }
            }
            LirOp::Xor { width } => {
                if node.inputs.len() >= 2 {
                    let a = expander.dual_rail_map.get(&node.inputs[0]).copied();
                    let b = expander.dual_rail_map.get(&node.inputs[1]).copied();
                    if let (Some(a), Some(b), Some(dest)) = (a, b, output_pair) {
                        expander.expand_xor(a, b, dest, *width);
                    }
                }
            }
            LirOp::Add { width, .. } => {
                if node.inputs.len() >= 2 {
                    let a = expander.dual_rail_map.get(&node.inputs[0]).copied();
                    let b = expander.dual_rail_map.get(&node.inputs[1]).copied();
                    if let (Some(a), Some(b), Some(dest)) = (a, b, output_pair) {
                        expander.expand_add(a, b, dest, *width);
                    }
                }
            }
            LirOp::Sub { width, .. } => {
                if node.inputs.len() >= 2 {
                    let a = expander.dual_rail_map.get(&node.inputs[0]).copied();
                    let b = expander.dual_rail_map.get(&node.inputs[1]).copied();
                    if let (Some(a), Some(b), Some(dest)) = (a, b, output_pair) {
                        expander.expand_sub(a, b, dest, *width);
                    }
                }
            }
            LirOp::Mul { result_width, .. } => {
                if node.inputs.len() >= 2 {
                    let a = expander.dual_rail_map.get(&node.inputs[0]).copied();
                    let b = expander.dual_rail_map.get(&node.inputs[1]).copied();
                    // Get actual input width from the input signals
                    let input_width = lir.signals[node.inputs[0].0 as usize].width;
                    if let (Some(a), Some(b), Some(dest)) = (a, b, output_pair) {
                        expander.expand_mul(a, b, dest, input_width, *result_width);
                    }
                }
            }
            LirOp::Lt { width } => {
                if node.inputs.len() >= 2 {
                    let a = expander.dual_rail_map.get(&node.inputs[0]).copied();
                    let b = expander.dual_rail_map.get(&node.inputs[1]).copied();
                    if let (Some(a), Some(b), Some(dest)) = (a, b, output_pair) {
                        expander.expand_lt(a, b, dest, *width);
                    }
                }
            }
            LirOp::Eq { width } => {
                if node.inputs.len() >= 2 {
                    let a = expander.dual_rail_map.get(&node.inputs[0]).copied();
                    let b = expander.dual_rail_map.get(&node.inputs[1]).copied();
                    if let (Some(a), Some(b), Some(dest)) = (a, b, output_pair) {
                        expander.expand_eq(a, b, dest, *width);
                    }
                }
            }
            LirOp::Mux2 { width } => {
                if node.inputs.len() >= 3 {
                    let sel = expander.dual_rail_map.get(&node.inputs[0]).copied();
                    let a = expander.dual_rail_map.get(&node.inputs[1]).copied();
                    let b = expander.dual_rail_map.get(&node.inputs[2]).copied();
                    if let (Some(sel), Some(a), Some(b), Some(dest)) = (sel, a, b, output_pair) {
                        expander.expand_mux2(sel, a, b, dest, *width);
                    }
                }
            }
            LirOp::Reg { width, .. } => {
                if !node.inputs.is_empty() {
                    let d = expander.dual_rail_map.get(&node.inputs[0]).copied();
                    if let (Some(d), Some(dest)) = (d, output_pair) {
                        expander.expand_reg(d, dest, *width);
                    }
                }
            }
            // Buffer operations - just copy dual-rail signals
            LirOp::Buf { width } | LirOp::Buffer { width } => {
                if !node.inputs.is_empty() {
                    let a = expander.dual_rail_map.get(&node.inputs[0]).copied();
                    if let (Some(a), Some(dest)) = (a, output_pair) {
                        // Buffer copies both rails from input to output
                        expander.expand_buf(a, dest, *width);
                    }
                }
            }
            // Constants - create dual-rail encoding
            LirOp::Constant { width, value } => {
                if let Some(dest) = output_pair {
                    expander.expand_constant(*value, dest, *width);
                }
            }
            // Shifts - pass through to regular shift operations on true rail
            LirOp::Shl { width } => {
                if node.inputs.len() >= 2 {
                    let a = expander.dual_rail_map.get(&node.inputs[0]).copied();
                    let shamt = expander.dual_rail_map.get(&node.inputs[1]).copied();
                    if let (Some(a), Some(shamt), Some(dest)) = (a, shamt, output_pair) {
                        // Simplified: shift the true rail, invert for false rail
                        expander.alloc_node(
                            LirOp::Shl { width: *width },
                            vec![a.t, shamt.t],
                            dest.t,
                        );
                        expander.alloc_node(LirOp::Not { width: *width }, vec![dest.t], dest.f);
                    }
                }
            }
            LirOp::Shr { width } => {
                if node.inputs.len() >= 2 {
                    let a = expander.dual_rail_map.get(&node.inputs[0]).copied();
                    let shamt = expander.dual_rail_map.get(&node.inputs[1]).copied();
                    if let (Some(a), Some(shamt), Some(dest)) = (a, shamt, output_pair) {
                        // Simplified: shift the true rail, invert for false rail
                        expander.alloc_node(
                            LirOp::Shr { width: *width },
                            vec![a.t, shamt.t],
                            dest.t,
                        );
                        expander.alloc_node(LirOp::Not { width: *width }, vec![dest.t], dest.f);
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
        // Create destination signals
        let dest = DualRailPair {
            t: expander.alloc_signal("out_t".to_string(), 1),
            f: expander.alloc_signal("out_f".to_string(), 1),
        };

        expander.expand_and(a, b, dest, 1);

        // Check that two nodes were created (And for t, Or for f)
        assert_eq!(expander.lir.nodes.len(), 2);
        assert!(matches!(expander.lir.nodes[0].op, LirOp::And { width: 1 }));
        assert!(matches!(expander.lir.nodes[1].op, LirOp::Or { width: 1 }));
    }

    #[test]
    fn test_expand_not_swaps_rails() {
        let config = NclConfig::default();
        let mut expander = NclExpander::new("test", config);

        let a = DualRailPair {
            t: expander.alloc_signal("a_t".to_string(), 1),
            f: expander.alloc_signal("a_f".to_string(), 1),
        };
        // Create destination signals
        let dest = DualRailPair {
            t: expander.alloc_signal("out_t".to_string(), 1),
            f: expander.alloc_signal("out_f".to_string(), 1),
        };

        expander.expand_not(a, dest, 1);

        // NOT should create two buffer nodes that swap the rails
        assert_eq!(expander.lir.nodes.len(), 2);
        assert!(matches!(expander.lir.nodes[0].op, LirOp::Buf { width: 1 }));
        assert!(matches!(expander.lir.nodes[1].op, LirOp::Buf { width: 1 }));
        // First buffer: a.f -> dest.t
        assert_eq!(expander.lir.nodes[0].inputs[0], a.f);
        assert_eq!(expander.lir.nodes[0].output, dest.t);
        // Second buffer: a.t -> dest.f
        assert_eq!(expander.lir.nodes[1].inputs[0], a.t);
        assert_eq!(expander.lir.nodes[1].output, dest.f);
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
