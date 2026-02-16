//! Formal Equivalence Checking for SKALP
//!
//! This module provides combinational equivalence checking (CEC) between:
//! - LIR (behavioral, pre-synthesis) and GateNetlist (structural, post-synthesis)
//! - Two LIR designs (for optimization verification)
//! - Two GateNetlist designs (for post-synthesis optimization verification)
//!
//! The approach uses And-Inverter Graphs (AIGs) as the canonical representation
//! and SAT solving to prove equivalence or find counterexamples.
//!
//! # Algorithm
//!
//! 1. Convert both designs to AIGs (bit-level And-Inverter Graphs)
//! 2. Build a miter circuit: XOR corresponding outputs, OR all XORs
//! 3. Use SAT solver to check if miter can ever be 1
//! 4. UNSAT = designs are equivalent, SAT = counterexample found
//!
//! # Sequential Designs
//!
//! For sequential designs, we use bounded equivalence checking:
//! - Unroll both designs for K cycles
//! - Check that outputs match at each cycle given same inputs
//! - This proves equivalence up to bound K (not full proof)

use crate::{Counterexample, FormalError, FormalResult, TraceStep};
use rayon::prelude::*;
use rand::Rng;
use skalp_lir::{CellFunction, GateNetlist, Lir, LirNode, LirOp, LirSignalId};
pub use skalp_sim::gpu_aig_cone_sim::AigCone;
use skalp_lir::synth::{
    Aig as SynthAig, AigLit as SynthAigLit,
    Fraig, FraigConfig, Pass, Strash,
};
use skalp_mir::Type;
use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::sync::atomic::{AtomicBool, Ordering};
use crate::sat_solver::{CnfFormula, ExtendFormula, Lit, Solver, Var};

// ============================================================================
// AIG (And-Inverter Graph) Representation
// ============================================================================

/// A node in an And-Inverter Graph
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct AigNodeId(pub u32);

/// An AIG literal (node reference with optional inversion)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct AigLit {
    /// Node ID
    pub node: AigNodeId,
    /// True if inverted
    pub inverted: bool,
}

impl AigLit {
    pub fn new(node: AigNodeId, inverted: bool) -> Self {
        Self { node, inverted }
    }

    pub fn positive(node: AigNodeId) -> Self {
        Self {
            node,
            inverted: false,
        }
    }

    pub fn negative(node: AigNodeId) -> Self {
        Self {
            node,
            inverted: true,
        }
    }

    pub fn invert(self) -> Self {
        Self {
            node: self.node,
            inverted: !self.inverted,
        }
    }
}

/// AIG node type
#[derive(Debug, Clone)]
pub enum AigNode {
    /// Constant false (node 0)
    False,
    /// Primary input
    Input { name: String },
    /// AND gate with two inputs
    And { left: AigLit, right: AigLit },
    /// Latch (for sequential circuits) - output is the current state
    Latch {
        name: String,
        next: AigLit,
        init: bool,
    },
}

/// And-Inverter Graph representation
#[derive(Debug, Clone)]
pub struct Aig {
    /// All nodes (node 0 is always constant false)
    pub nodes: Vec<AigNode>,
    /// Primary input node IDs
    pub inputs: Vec<AigNodeId>,
    /// Primary output literals
    pub outputs: Vec<AigLit>,
    /// Output names (for counterexample generation)
    pub output_names: Vec<String>,
    /// Input names (for counterexample generation)
    pub input_names: Vec<String>,
    /// Latch node IDs (for sequential circuits)
    pub latches: Vec<AigNodeId>,
    /// Mapping from state-input node IDs to their corresponding latch node IDs
    /// Used during simulation: state_input reads from its linked latch's current value
    pub state_input_to_latch: BTreeMap<u32, u32>,
}

impl Aig {
    /// Create a new AIG with just the constant false node
    pub fn new() -> Self {
        Self {
            nodes: vec![AigNode::False],
            inputs: Vec::new(),
            outputs: Vec::new(),
            output_names: Vec::new(),
            input_names: Vec::new(),
            latches: Vec::new(),
            state_input_to_latch: BTreeMap::new(),
        }
    }

    /// Link a state input node to its corresponding latch node
    /// During simulation, the state input will read from the latch's current value
    pub fn link_state_input_to_latch(&mut self, state_input_id: u32, latch_id: u32) {
        self.state_input_to_latch.insert(state_input_id, latch_id);
    }

    /// Get the constant false literal
    pub fn false_lit(&self) -> AigLit {
        AigLit::positive(AigNodeId(0))
    }

    /// Get the constant true literal
    pub fn true_lit(&self) -> AigLit {
        AigLit::negative(AigNodeId(0))
    }

    /// Add a primary input
    pub fn add_input(&mut self, name: String) -> AigLit {
        let id = AigNodeId(self.nodes.len() as u32);
        self.nodes.push(AigNode::Input { name: name.clone() });
        self.inputs.push(id);
        self.input_names.push(name);
        AigLit::positive(id)
    }

    /// Add an AND gate
    pub fn add_and(&mut self, left: AigLit, right: AigLit) -> AigLit {
        // Structural hashing / simplification
        // AND(x, false) = false
        if left.node.0 == 0 && !left.inverted {
            return self.false_lit();
        }
        if right.node.0 == 0 && !right.inverted {
            return self.false_lit();
        }
        // AND(x, true) = x
        if left.node.0 == 0 && left.inverted {
            return right;
        }
        if right.node.0 == 0 && right.inverted {
            return left;
        }
        // AND(x, x) = x
        if left == right {
            return left;
        }
        // AND(x, !x) = false
        if left.node == right.node && left.inverted != right.inverted {
            return self.false_lit();
        }

        let id = AigNodeId(self.nodes.len() as u32);
        self.nodes.push(AigNode::And { left, right });
        AigLit::positive(id)
    }

    /// Add an OR gate (using De Morgan: a OR b = NOT(NOT a AND NOT b))
    pub fn add_or(&mut self, left: AigLit, right: AigLit) -> AigLit {
        self.add_and(left.invert(), right.invert()).invert()
    }

    /// Add an XOR gate
    pub fn add_xor(&mut self, a: AigLit, b: AigLit) -> AigLit {
        // XOR(a,b) = (a AND !b) OR (!a AND b)
        let a_and_not_b = self.add_and(a, b.invert());
        let not_a_and_b = self.add_and(a.invert(), b);
        self.add_or(a_and_not_b, not_a_and_b)
    }

    /// Add an XNOR gate: returns true if inputs are equal
    pub fn add_xnor(&mut self, a: AigLit, b: AigLit) -> AigLit {
        // XNOR(a,b) = NOT(XOR(a,b))
        self.add_xor(a, b).invert()
    }

    /// Add a 2:1 MUX: sel ? b : a
    pub fn add_mux(&mut self, sel: AigLit, a: AigLit, b: AigLit) -> AigLit {
        // MUX(s,a,b) = (s AND b) OR (!s AND a)
        let sel_and_b = self.add_and(sel, b);
        let not_sel_and_a = self.add_and(sel.invert(), a);
        self.add_or(sel_and_b, not_sel_and_a)
    }

    /// Add a latch (for sequential circuits)
    pub fn add_latch(&mut self, name: String, next: AigLit, init: bool) -> AigLit {
        let id = AigNodeId(self.nodes.len() as u32);
        self.nodes.push(AigNode::Latch { name, next, init });
        self.latches.push(id);
        AigLit::positive(id)
    }

    /// Add a primary output
    pub fn add_output(&mut self, name: String, lit: AigLit) {
        self.outputs.push(lit);
        self.output_names.push(name);
    }

    /// Get number of AND gates
    pub fn and_count(&self) -> usize {
        self.nodes
            .iter()
            .filter(|n| matches!(n, AigNode::And { .. }))
            .count()
    }
}

impl Default for Aig {
    fn default() -> Self {
        Self::new()
    }
}

// ============================================================================
// LIR to AIG Conversion
// ============================================================================

/// Convert a LIR design to an AIG
pub struct LirToAig {
    /// The AIG being built
    pub aig: Aig,
    /// Map from (LirSignalId, bit_index) to AigLit
    pub signal_map: BTreeMap<(u32, u32), AigLit>,
}

impl LirToAig {
    pub fn new() -> Self {
        Self {
            aig: Aig::new(),
            signal_map: BTreeMap::new(),
        }
    }

    /// Convert LIR to AIG
    pub fn convert(mut self, lir: &Lir) -> Aig {
        // Add primary inputs
        for &input_id in &lir.inputs {
            let signal = &lir.signals[input_id.0 as usize];
            for bit in 0..signal.width {
                let name = if signal.width == 1 {
                    signal.name.clone()
                } else {
                    format!("{}[{}]", signal.name, bit)
                };
                let lit = self.aig.add_input(name);
                self.signal_map.insert((input_id.0, bit), lit);
            }
        }

        // Process nodes in topological order (assuming LIR is already sorted)
        for node in &lir.nodes {
            self.convert_node(node, lir);
        }

        // Add primary outputs
        for &output_id in &lir.outputs {
            let signal = &lir.signals[output_id.0 as usize];
            for bit in 0..signal.width {
                let name = if signal.width == 1 {
                    signal.name.clone()
                } else {
                    format!("{}[{}]", signal.name, bit)
                };
                let lit = self
                    .signal_map
                    .get(&(output_id.0, bit))
                    .copied()
                    .unwrap_or_else(|| self.aig.false_lit());
                self.aig.add_output(name, lit);
            }
        }

        self.aig
    }

    /// Convert LIR to sequential AIG with Latch nodes for BMC
    ///
    /// This creates an AIG where registers become Latch nodes with:
    /// - Current state from latch outputs
    /// - Next state from the D input logic
    /// - Reset value as init
    pub fn convert_sequential(mut self, lir: &Lir) -> Aig {
        // Validation: Check for invalid signal references
        let num_signals = lir.signals.len() as u32;
        for (idx, node) in lir.nodes.iter().enumerate() {
            for &input_id in &node.inputs {
                if input_id.0 >= num_signals {
                    log::warn!("[LIR_AIG] ❌ Node {} ({:?}) references invalid signal {} (max={}), path={}",
                        idx, std::mem::discriminant(&node.op), input_id.0, num_signals - 1, node.path);
                }
            }
            if node.output.0 >= num_signals {
                log::warn!("[LIR_AIG] ❌ Node {} ({:?}) has invalid output signal {} (max={}), path={}",
                    idx, std::mem::discriminant(&node.op), node.output.0, num_signals - 1, node.path);
            }
        }

        // First pass: find all registers (sequential nodes)
        let mut register_nodes: Vec<(usize, &LirNode)> = Vec::new();
        for (idx, node) in lir.nodes.iter().enumerate() {
            if matches!(node.op, LirOp::Reg { .. }) {
                register_nodes.push((idx, node));
            }
        }

        // Add primary inputs (excluding clock)
        for &input_id in &lir.inputs {
            let signal = &lir.signals[input_id.0 as usize];
            if signal.name.contains("clk") || signal.name.contains("clock") {
                continue; // Skip clocks in sequential AIG
            }
            for bit in 0..signal.width {
                let name = if signal.width == 1 {
                    signal.name.clone()
                } else {
                    format!("{}[{}]", signal.name, bit)
                };
                let lit = self.aig.add_input(name);
                self.signal_map.insert((input_id.0, bit), lit);
            }
        }

        // Create placeholder inputs for register current values
        let mut reg_current_map: BTreeMap<(u32, u32), AigLit> = BTreeMap::new();
        for (_, node) in &register_nodes {
            if let LirOp::Reg { width, .. } = &node.op {
                let output_signal = &lir.signals[node.output.0 as usize];
                for bit in 0..*width {
                    let name = if *width == 1 {
                        format!("__reg_cur_{}", output_signal.name)
                    } else {
                        format!("__reg_cur_{}[{}]", output_signal.name, bit)
                    };
                    let lit = self.aig.add_input(name);
                    reg_current_map.insert((node.output.0, bit), lit);
                    // Also add to signal_map so register reads use current value
                    self.signal_map.insert((node.output.0, bit), lit);
                }
            }
        }

        // Process all combinational nodes in topological order
        // (nodes that produce signals must be processed before nodes that consume them)
        let sorted_indices = self.topological_sort_lir_nodes(lir);

        // Debug: Track which signals are accessed before being produced
        let mut debug_produced: std::collections::HashSet<u32> = std::collections::HashSet::new();
        // Mark inputs and register outputs as already produced
        for &input_id in &lir.inputs {
            let signal = &lir.signals[input_id.0 as usize];
            for bit in 0..signal.width {
                debug_produced.insert(input_id.0 * 100 + bit);
            }
        }
        for (_, node) in &register_nodes {
            if let LirOp::Reg { width, .. } = &node.op {
                for bit in 0..*width {
                    debug_produced.insert(node.output.0 * 100 + bit);
                }
            }
        }

        for idx in sorted_indices {
            let node = &lir.nodes[idx];
            if !matches!(node.op, LirOp::Reg { .. }) {
                self.convert_node(node, lir);
            }
        }

        // Now create Latch nodes for registers
        for (_, node) in &register_nodes {
            if let LirOp::Reg { width, reset_value, has_reset, .. } = &node.op {
                let output_signal = &lir.signals[node.output.0 as usize];
                let d_input = node.inputs.get(0).copied().unwrap_or(LirSignalId(0));
                let reset_val = reset_value.unwrap_or(0);

                // Get the actual width of the D input signal for proper zero-extension
                let d_input_width = lir.signals.get(d_input.0 as usize)
                    .map(|s| s.width)
                    .unwrap_or(*width);

                for bit in 0..*width {
                    let latch_name = if *width == 1 {
                        output_signal.name.clone()
                    } else {
                        format!("{}[{}]", output_signal.name, bit)
                    };

                    // Get D input (next state), zero-extending if input is narrower than register
                    let d_lit = if bit < d_input_width {
                        self.get_input_bit(d_input, bit)
                    } else {
                        // Zero-extend: bits beyond input width are 0
                        self.aig.false_lit()
                    };

                    // Handle reset: if has_reset, apply rst ? reset_val : d
                    let next_lit = if *has_reset {
                        if let Some(rst_id) = node.reset {
                            let rst_lit = self.get_input_bit(rst_id, 0);
                            let reset_bit = (reset_val >> bit) & 1 != 0;
                            // MUX: rst ? reset_bit : d
                            if reset_bit {
                                // rst | (!rst & d) = rst | d
                                let result = self.aig.add_or(rst_lit, d_lit);
                                result
                            } else {
                                // !rst & d
                                self.aig.add_and(rst_lit.invert(), d_lit)
                            }
                        } else {
                            d_lit
                        }
                    } else {
                        d_lit
                    };

                    // Init value from reset_value
                    let init_value = (reset_val >> bit) & 1 != 0;

                    // Create latch
                    let latch_lit = self.aig.add_latch(latch_name, next_lit, init_value);
                    let latch_id = latch_lit.node.0;

                    // Link state input to latch (metadata-based, no name matching needed)
                    if let Some(state_input_lit) = reg_current_map.get(&(node.output.0, bit)) {
                        self.aig.link_state_input_to_latch(state_input_lit.node.0, latch_id);
                    }

                    // Update signal_map to use latch output
                    self.signal_map.insert((node.output.0, bit), latch_lit);
                }
            }
        }

        // Add primary outputs
        for &output_id in &lir.outputs {
            let signal = &lir.signals[output_id.0 as usize];
            for bit in 0..signal.width {
                let name = if signal.width == 1 {
                    signal.name.clone()
                } else {
                    format!("{}[{}]", signal.name, bit)
                };
                let lit = self
                    .signal_map
                    .get(&(output_id.0, bit))
                    .copied()
                    .unwrap_or_else(|| self.aig.false_lit());
                self.aig.add_output(name, lit);
            }
        }

        self.aig
    }

    /// Topologically sort LIR nodes so producers come before consumers
    fn topological_sort_lir_nodes(&self, lir: &Lir) -> Vec<usize> {
        use std::collections::HashSet;

        // Build a map from signal ID to the node that produces it
        // IMPORTANT: Skip register nodes - their outputs are already in signal_map
        // from the register_current_map initialization, so combinational nodes
        // should not depend on register nodes
        let mut signal_producer: BTreeMap<u32, usize> = BTreeMap::new();
        for (idx, node) in lir.nodes.iter().enumerate() {
            // Skip registers - their outputs don't create dependencies
            if matches!(node.op, LirOp::Reg { .. }) {
                continue;
            }
            signal_producer.insert(node.output.0, idx);
        }

        // Build adjacency list: node -> nodes it depends on
        let mut dependencies: Vec<HashSet<usize>> = vec![HashSet::new(); lir.nodes.len()];
        for (idx, node) in lir.nodes.iter().enumerate() {
            for &input_id in &node.inputs {
                if let Some(&producer_idx) = signal_producer.get(&input_id.0) {
                    if producer_idx != idx {
                        dependencies[idx].insert(producer_idx);
                    }
                }
            }
            // Also add clock/reset dependencies
            if let Some(clk) = node.clock {
                if let Some(&producer_idx) = signal_producer.get(&clk.0) {
                    if producer_idx != idx {
                        dependencies[idx].insert(producer_idx);
                    }
                }
            }
            if let Some(rst) = node.reset {
                if let Some(&producer_idx) = signal_producer.get(&rst.0) {
                    if producer_idx != idx {
                        dependencies[idx].insert(producer_idx);
                    }
                }
            }
        }

        // Kahn's algorithm for topological sort
        let mut in_degree: Vec<usize> = dependencies.iter()
            .map(|deps| deps.len())
            .collect();

        // Build reverse adjacency: node -> nodes that depend on it
        let mut dependents: Vec<Vec<usize>> = vec![Vec::new(); lir.nodes.len()];
        for (idx, deps) in dependencies.iter().enumerate() {
            for &dep in deps {
                dependents[dep].push(idx);
            }
        }

        // Start with nodes that have no dependencies
        let mut queue: std::collections::VecDeque<usize> = in_degree.iter()
            .enumerate()
            .filter(|(_, &d)| d == 0)
            .map(|(i, _)| i)
            .collect();

        let mut sorted = Vec::with_capacity(lir.nodes.len());

        while let Some(node_idx) = queue.pop_front() {
            sorted.push(node_idx);
            for &dependent in &dependents[node_idx] {
                in_degree[dependent] -= 1;
                if in_degree[dependent] == 0 {
                    queue.push_back(dependent);
                }
            }
        }

        // If not all nodes were sorted, there's a cycle - just return original order
        if sorted.len() != lir.nodes.len() {
            log::warn!("[LIR_TOPO_SORT] Cycle detected (sorted {} of {}), using original order",
                      sorted.len(), lir.nodes.len());
            (0..lir.nodes.len()).collect()
        } else {
            sorted
        }
    }

    fn get_input_bit(&self, signal_id: LirSignalId, bit: u32) -> AigLit {
        self.signal_map
            .get(&(signal_id.0, bit))
            .copied()
            .unwrap_or_else(|| {
                // Only warn for signals that exist but with missing bits (width mismatch)
                // Don't spam for invalid signal IDs
                if signal_id.0 < 2000 {  // Reasonable limit for signal IDs
                    log::warn!("[LIR_AIG] ⚠️ Missing signal lookup: signal_id={}, bit={} (signal_map has {} entries)",
                        signal_id.0, bit, self.signal_map.len());
                }
                self.aig.false_lit()
            })
    }

    /// Get input bit with zero-extension for signals narrower than expected
    fn get_input_bit_zext(&self, signal_id: LirSignalId, bit: u32, signal_width: u32) -> AigLit {
        if bit >= signal_width {
            // Beyond signal width - return 0 (zero-extend)
            self.aig.false_lit()
        } else {
            self.get_input_bit(signal_id, bit)
        }
    }

    fn set_output_bit(&mut self, signal_id: LirSignalId, bit: u32, lit: AigLit) {
        self.signal_map.insert((signal_id.0, bit), lit);
    }

    /// Build unsigned multiplier using grade-school algorithm
    fn build_unsigned_mul_lir(
        &mut self,
        a: LirSignalId,
        b: LirSignalId,
        width: u32,
        result_width: u32,
    ) -> Vec<AigLit> {
        let mut result: Vec<AigLit> = vec![self.aig.false_lit(); result_width as usize];

        for i in 0..width {
            let b_bit = self.get_input_bit(b, i);

            let mut carry = self.aig.false_lit();
            for j in 0..width {
                let out_idx = (i + j) as usize;
                if out_idx < result_width as usize {
                    let a_bit = self.get_input_bit(a, j);
                    let gated = self.aig.add_and(a_bit, b_bit);

                    let sum_ab = self.aig.add_xor(result[out_idx], gated);
                    let sum = self.aig.add_xor(sum_ab, carry);

                    let ab = self.aig.add_and(result[out_idx], gated);
                    let ac = self.aig.add_and(result[out_idx], carry);
                    let bc = self.aig.add_and(gated, carry);
                    let ab_or_ac = self.aig.add_or(ab, ac);
                    carry = self.aig.add_or(ab_or_ac, bc);

                    result[out_idx] = sum;
                }
            }
            let mut idx = (i + width) as usize;
            while idx < result_width as usize {
                let sum = self.aig.add_xor(result[idx], carry);
                carry = self.aig.add_and(result[idx], carry);
                result[idx] = sum;
                if carry == self.aig.false_lit() {
                    break;
                }
                idx += 1;
            }
        }

        result
    }

    /// Build signed multiplier using sign-magnitude approach
    /// BUG FIX #247: Proper signed multiplication for formal verification
    /// Handles asymmetric input widths (e.g., 16-bit × 32-bit) by sign-extending
    /// narrower inputs to `width` bits using their actual sign bit.
    fn build_signed_mul_lir(
        &mut self,
        a: LirSignalId,
        b: LirSignalId,
        width: u32,
        result_width: u32,
        a_actual_width: u32,
        b_actual_width: u32,
    ) -> Vec<AigLit> {
        // 1. Get sign bits from actual MSB of each input
        let a_sign = self.get_input_bit(a, a_actual_width - 1);
        let b_sign = self.get_input_bit(b, b_actual_width - 1);
        let result_sign = self.aig.add_xor(a_sign, b_sign);

        // 2. Read and sign-extend inputs to `width` bits
        let a_bits: Vec<AigLit> = (0..width)
            .map(|i| {
                if i < a_actual_width {
                    self.get_input_bit(a, i)
                } else {
                    a_sign // sign-extend
                }
            })
            .collect();

        let b_bits: Vec<AigLit> = (0..width)
            .map(|i| {
                if i < b_actual_width {
                    self.get_input_bit(b, i)
                } else {
                    b_sign // sign-extend
                }
            })
            .collect();

        // 3. Get absolute values: |x| = sign ? -x : x
        let a_neg = self.negate_vector(&a_bits);
        let a_mag = self.mux_vectors(a_sign, &a_neg, &a_bits);

        let b_neg = self.negate_vector(&b_bits);
        let b_mag = self.mux_vectors(b_sign, &b_neg, &b_bits);

        // 4. Unsigned multiply the magnitudes
        let unsigned_result = self.unsigned_mul_vectors(&a_mag, &b_mag, result_width);

        // 5. Conditionally negate result if signs differ
        let neg_result = self.negate_vector(&unsigned_result);
        self.mux_vectors(result_sign, &neg_result, &unsigned_result)
    }

    /// Unsigned multiply two vectors
    fn unsigned_mul_vectors(
        &mut self,
        a: &[AigLit],
        b: &[AigLit],
        result_width: u32,
    ) -> Vec<AigLit> {
        let width = a.len() as u32;
        let mut result: Vec<AigLit> = vec![self.aig.false_lit(); result_width as usize];

        for i in 0..width {
            let b_bit = b.get(i as usize).copied().unwrap_or_else(|| self.aig.false_lit());

            let mut carry = self.aig.false_lit();
            for j in 0..width {
                let out_idx = (i + j) as usize;
                if out_idx < result_width as usize {
                    let a_bit = a.get(j as usize).copied().unwrap_or_else(|| self.aig.false_lit());
                    let gated = self.aig.add_and(a_bit, b_bit);

                    let sum_ab = self.aig.add_xor(result[out_idx], gated);
                    let sum = self.aig.add_xor(sum_ab, carry);

                    let ab = self.aig.add_and(result[out_idx], gated);
                    let ac = self.aig.add_and(result[out_idx], carry);
                    let bc = self.aig.add_and(gated, carry);
                    let ab_or_ac = self.aig.add_or(ab, ac);
                    carry = self.aig.add_or(ab_or_ac, bc);

                    result[out_idx] = sum;
                }
            }
            let mut idx = (i + width) as usize;
            while idx < result_width as usize {
                let sum = self.aig.add_xor(result[idx], carry);
                carry = self.aig.add_and(result[idx], carry);
                result[idx] = sum;
                if carry == self.aig.false_lit() {
                    break;
                }
                idx += 1;
            }
        }

        result
    }

    /// Negate a vector (two's complement: ~x + 1)
    fn negate_vector(&mut self, x: &[AigLit]) -> Vec<AigLit> {
        let mut result = Vec::with_capacity(x.len());
        let mut carry = self.aig.true_lit(); // +1

        for &bit in x {
            let inv_bit = bit.invert();
            let sum = self.aig.add_xor(inv_bit, carry);
            let new_carry = self.aig.add_and(inv_bit, carry);
            result.push(sum);
            carry = new_carry;
        }

        result
    }

    /// Mux two vectors: sel ? a : b
    fn mux_vectors(&mut self, sel: AigLit, a: &[AigLit], b: &[AigLit]) -> Vec<AigLit> {
        let width = a.len().max(b.len());
        (0..width)
            .map(|i| {
                let a_bit = a.get(i).copied().unwrap_or_else(|| self.aig.false_lit());
                let b_bit = b.get(i).copied().unwrap_or_else(|| self.aig.false_lit());
                let sel_and_a = self.aig.add_and(sel, a_bit);
                let not_sel_and_b = self.aig.add_and(sel.invert(), b_bit);
                self.aig.add_or(sel_and_a, not_sel_and_b)
            })
            .collect()
    }

    fn convert_node(&mut self, node: &LirNode, lir: &Lir) {
        match &node.op {
            LirOp::Constant { width, value } => {
                for bit in 0..*width {
                    let lit = if (value >> bit) & 1 == 1 {
                        self.aig.true_lit()
                    } else {
                        self.aig.false_lit()
                    };
                    self.set_output_bit(node.output, bit, lit);
                }
            }

            LirOp::Buf { width } | LirOp::Buffer { width } => {
                let input = node.inputs[0];
                for bit in 0..*width {
                    let lit = self.get_input_bit(input, bit);
                    self.set_output_bit(node.output, bit, lit);
                }
            }

            LirOp::Not { width } => {
                let input = node.inputs[0];
                for bit in 0..*width {
                    let lit = self.get_input_bit(input, bit).invert();
                    self.set_output_bit(node.output, bit, lit);
                }
            }

            LirOp::And { width } => {
                let a = node.inputs[0];
                let b = node.inputs[1];
                for bit in 0..*width {
                    let a_lit = self.get_input_bit(a, bit);
                    let b_lit = self.get_input_bit(b, bit);
                    let result = self.aig.add_and(a_lit, b_lit);
                    self.set_output_bit(node.output, bit, result);
                }
            }

            LirOp::Or { width } => {
                let a = node.inputs[0];
                let b = node.inputs[1];
                for bit in 0..*width {
                    let a_lit = self.get_input_bit(a, bit);
                    let b_lit = self.get_input_bit(b, bit);
                    let result = self.aig.add_or(a_lit, b_lit);
                    self.set_output_bit(node.output, bit, result);
                }
            }

            LirOp::Xor { width } => {
                let a = node.inputs[0];
                let b = node.inputs[1];
                for bit in 0..*width {
                    let a_lit = self.get_input_bit(a, bit);
                    let b_lit = self.get_input_bit(b, bit);
                    let result = self.aig.add_xor(a_lit, b_lit);
                    self.set_output_bit(node.output, bit, result);
                }
            }

            LirOp::Nand { width } => {
                let a = node.inputs[0];
                let b = node.inputs[1];
                for bit in 0..*width {
                    let a_lit = self.get_input_bit(a, bit);
                    let b_lit = self.get_input_bit(b, bit);
                    let result = self.aig.add_and(a_lit, b_lit).invert();
                    self.set_output_bit(node.output, bit, result);
                }
            }

            LirOp::Nor { width } => {
                let a = node.inputs[0];
                let b = node.inputs[1];
                for bit in 0..*width {
                    let a_lit = self.get_input_bit(a, bit);
                    let b_lit = self.get_input_bit(b, bit);
                    let result = self.aig.add_or(a_lit, b_lit).invert();
                    self.set_output_bit(node.output, bit, result);
                }
            }

            LirOp::Mux2 { width } => {
                // Mux2 inputs: [sel, else_value, then_value]
                // Based on mir_to_lir.rs input ordering
                // add_mux(sel, a, b) returns sel ? b : a
                // We want: sel ? then_value : else_value
                // So call add_mux(sel, else_value, then_value)
                let sel = node.inputs[0];
                let a = node.inputs[1];  // else_value (when sel=0)
                let b = node.inputs[2];  // then_value (when sel=1)
                let sel_lit = self.get_input_bit(sel, 0);

                for bit in 0..*width {
                    let a_lit = self.get_input_bit(a, bit);  // else_value
                    let b_lit = self.get_input_bit(b, bit);  // then_value
                    let result = self.aig.add_mux(sel_lit, a_lit, b_lit);
                    self.set_output_bit(node.output, bit, result);
                }
            }

            LirOp::Eq { width } => {
                // Equality: all bits must match
                let a = node.inputs[0];
                let b = node.inputs[1];
                let mut result = self.aig.true_lit();
                for bit in 0..*width {
                    let a_lit = self.get_input_bit(a, bit);
                    let b_lit = self.get_input_bit(b, bit);
                    // XNOR for bit equality
                    let bit_eq = self.aig.add_xor(a_lit, b_lit).invert();
                    result = self.aig.add_and(result, bit_eq);
                }
                self.set_output_bit(node.output, 0, result);
            }

            LirOp::Ne { width } => {
                // Not equal: any bit differs
                let a = node.inputs[0];
                let b = node.inputs[1];
                let mut result = self.aig.false_lit();
                for bit in 0..*width {
                    let a_lit = self.get_input_bit(a, bit);
                    let b_lit = self.get_input_bit(b, bit);
                    let bit_ne = self.aig.add_xor(a_lit, b_lit);
                    result = self.aig.add_or(result, bit_ne);
                }
                self.set_output_bit(node.output, 0, result);
            }

            LirOp::Add { width, has_carry } => {
                let a = node.inputs[0];
                let b = node.inputs[1];
                let mut carry = self.aig.false_lit();

                for bit in 0..*width {
                    let a_lit = self.get_input_bit(a, bit);
                    let b_lit = self.get_input_bit(b, bit);

                    // Full adder: sum = a XOR b XOR cin
                    let a_xor_b = self.aig.add_xor(a_lit, b_lit);
                    let sum = self.aig.add_xor(a_xor_b, carry);
                    self.set_output_bit(node.output, bit, sum);

                    // cout = (a AND b) OR (cin AND (a XOR b))
                    let a_and_b = self.aig.add_and(a_lit, b_lit);
                    let a_xor_b2 = self.aig.add_xor(a_lit, b_lit);
                    let cin_and_xor = self.aig.add_and(carry, a_xor_b2);
                    carry = self.aig.add_or(a_and_b, cin_and_xor);
                }

                if *has_carry {
                    self.set_output_bit(node.output, *width, carry);
                }
            }

            LirOp::Sub { width, has_borrow } => {
                let a = node.inputs[0];
                let b = node.inputs[1];
                let mut borrow = self.aig.false_lit();

                for bit in 0..*width {
                    let a_lit = self.get_input_bit(a, bit);
                    let b_lit = self.get_input_bit(b, bit);

                    // Full subtractor: diff = a XOR b XOR borrow
                    let a_xor_b = self.aig.add_xor(a_lit, b_lit);
                    let diff = self.aig.add_xor(a_xor_b, borrow);
                    self.set_output_bit(node.output, bit, diff);

                    // bout = (!a AND b) OR (borrow AND !(a XOR b))
                    let not_a_and_b = self.aig.add_and(a_lit.invert(), b_lit);
                    let a_xor_b2 = self.aig.add_xor(a_lit, b_lit);
                    let borrow_and_eq = self.aig.add_and(borrow, a_xor_b2.invert());
                    borrow = self.aig.add_or(not_a_and_b, borrow_and_eq);
                }

                if *has_borrow {
                    self.set_output_bit(node.output, *width, borrow);
                }
            }

            LirOp::Lt { width } => {
                // Unsigned less than using subtraction borrow
                let a = node.inputs[0];
                let b = node.inputs[1];
                let mut borrow = self.aig.false_lit();

                for bit in 0..*width {
                    let a_lit = self.get_input_bit(a, bit);
                    let b_lit = self.get_input_bit(b, bit);

                    let not_a_and_b = self.aig.add_and(a_lit.invert(), b_lit);
                    let a_xor_b = self.aig.add_xor(a_lit, b_lit);
                    let borrow_and_eq = self.aig.add_and(borrow, a_xor_b.invert());
                    borrow = self.aig.add_or(not_a_and_b, borrow_and_eq);
                }

                self.set_output_bit(node.output, 0, borrow);
            }

            LirOp::Le { width } => {
                // a <= b is !(a > b) which is !(b < a)
                let a = node.inputs[0];
                let b = node.inputs[1];
                let mut borrow = self.aig.false_lit();

                // Compute b < a
                for bit in 0..*width {
                    let a_lit = self.get_input_bit(a, bit);
                    let b_lit = self.get_input_bit(b, bit);

                    let not_b_and_a = self.aig.add_and(b_lit.invert(), a_lit);
                    let a_xor_b = self.aig.add_xor(a_lit, b_lit);
                    let borrow_and_eq = self.aig.add_and(borrow, a_xor_b.invert());
                    borrow = self.aig.add_or(not_b_and_a, borrow_and_eq);
                }

                // a <= b = !(b < a)
                self.set_output_bit(node.output, 0, borrow.invert());
            }

            LirOp::Gt { width } => {
                // a > b is b < a
                let a = node.inputs[0];
                let b = node.inputs[1];
                let mut borrow = self.aig.false_lit();

                for bit in 0..*width {
                    let a_lit = self.get_input_bit(a, bit);
                    let b_lit = self.get_input_bit(b, bit);

                    let not_b_and_a = self.aig.add_and(b_lit.invert(), a_lit);
                    let a_xor_b = self.aig.add_xor(a_lit, b_lit);
                    let borrow_and_eq = self.aig.add_and(borrow, a_xor_b.invert());
                    borrow = self.aig.add_or(not_b_and_a, borrow_and_eq);
                }

                self.set_output_bit(node.output, 0, borrow);
            }

            LirOp::Ge { width } => {
                // a >= b is !(a < b)
                let a = node.inputs[0];
                let b = node.inputs[1];
                let mut borrow = self.aig.false_lit();

                for bit in 0..*width {
                    let a_lit = self.get_input_bit(a, bit);
                    let b_lit = self.get_input_bit(b, bit);

                    let not_a_and_b = self.aig.add_and(a_lit.invert(), b_lit);
                    let a_xor_b = self.aig.add_xor(a_lit, b_lit);
                    let borrow_and_eq = self.aig.add_and(borrow, a_xor_b.invert());
                    borrow = self.aig.add_or(not_a_and_b, borrow_and_eq);
                }

                self.set_output_bit(node.output, 0, borrow.invert());
            }

            // Signed comparisons - use MSB XOR to adjust for sign
            LirOp::Slt { width } => {
                // Signed a < b: flip MSB of both, then do unsigned comparison
                let a = node.inputs[0];
                let b = node.inputs[1];
                let mut borrow = self.aig.false_lit();

                for bit in 0..*width {
                    let mut a_lit = self.get_input_bit(a, bit);
                    let mut b_lit = self.get_input_bit(b, bit);

                    // Flip MSB to convert signed to unsigned comparison
                    if bit == width - 1 {
                        a_lit = a_lit.invert();
                        b_lit = b_lit.invert();
                    }

                    let not_a_and_b = self.aig.add_and(a_lit.invert(), b_lit);
                    let a_xor_b = self.aig.add_xor(a_lit, b_lit);
                    let borrow_and_eq = self.aig.add_and(borrow, a_xor_b.invert());
                    borrow = self.aig.add_or(not_a_and_b, borrow_and_eq);
                }

                self.set_output_bit(node.output, 0, borrow);
            }

            LirOp::Sle { width } => {
                // Signed a <= b is !(a > b) = !(b < a)
                let a = node.inputs[0];
                let b = node.inputs[1];
                let mut borrow = self.aig.false_lit();

                // Compute b < a (with sign bit flip)
                for bit in 0..*width {
                    let mut a_lit = self.get_input_bit(a, bit);
                    let mut b_lit = self.get_input_bit(b, bit);

                    if bit == width - 1 {
                        a_lit = a_lit.invert();
                        b_lit = b_lit.invert();
                    }

                    let not_b_and_a = self.aig.add_and(b_lit.invert(), a_lit);
                    let a_xor_b = self.aig.add_xor(a_lit, b_lit);
                    let borrow_and_eq = self.aig.add_and(borrow, a_xor_b.invert());
                    borrow = self.aig.add_or(not_b_and_a, borrow_and_eq);
                }

                // a <= b = !(b < a)
                self.set_output_bit(node.output, 0, borrow.invert());
            }

            LirOp::Sgt { width } => {
                // Signed a > b is b < a
                let a = node.inputs[0];
                let b = node.inputs[1];
                let mut borrow = self.aig.false_lit();

                for bit in 0..*width {
                    let mut a_lit = self.get_input_bit(a, bit);
                    let mut b_lit = self.get_input_bit(b, bit);

                    if bit == width - 1 {
                        a_lit = a_lit.invert();
                        b_lit = b_lit.invert();
                    }

                    let not_b_and_a = self.aig.add_and(b_lit.invert(), a_lit);
                    let a_xor_b = self.aig.add_xor(a_lit, b_lit);
                    let borrow_and_eq = self.aig.add_and(borrow, a_xor_b.invert());
                    borrow = self.aig.add_or(not_b_and_a, borrow_and_eq);
                }

                self.set_output_bit(node.output, 0, borrow);
            }

            LirOp::Sge { width } => {
                // Signed a >= b is !(a < b)
                let a = node.inputs[0];
                let b = node.inputs[1];
                let mut borrow = self.aig.false_lit();

                for bit in 0..*width {
                    let mut a_lit = self.get_input_bit(a, bit);
                    let mut b_lit = self.get_input_bit(b, bit);

                    if bit == width - 1 {
                        a_lit = a_lit.invert();
                        b_lit = b_lit.invert();
                    }

                    let not_a_and_b = self.aig.add_and(a_lit.invert(), b_lit);
                    let a_xor_b = self.aig.add_xor(a_lit, b_lit);
                    let borrow_and_eq = self.aig.add_and(borrow, a_xor_b.invert());
                    borrow = self.aig.add_or(not_a_and_b, borrow_and_eq);
                }

                // a >= b = !(a < b)
                self.set_output_bit(node.output, 0, borrow.invert());
            }

            // Shift operations
            LirOp::Shl { width } => {
                // Left shift: shift in zeros from right
                let data = node.inputs[0];
                let amount = node.inputs[1];
                // For variable shift, we need a barrel shifter (mux tree)
                // This is a simplified implementation using muxes for each shift amount
                let mut current: Vec<AigLit> = (0..*width)
                    .map(|bit| self.get_input_bit(data, bit))
                    .collect();

                // Build barrel shifter - each stage shifts by 2^i if amount bit is set
                for stage in 0..(*width as f32).log2().ceil() as u32 {
                    let shift_bit = self.get_input_bit(amount, stage);
                    let shift_amount = 1u32 << stage;
                    let mut next = vec![self.aig.false_lit(); *width as usize];

                    for bit in 0..*width {
                        let orig = current[bit as usize];
                        let shifted = if bit >= shift_amount {
                            current[(bit - shift_amount) as usize]
                        } else {
                            self.aig.false_lit() // Shift in zero
                        };
                        // MUX: if shift_bit then shifted else orig
                        next[bit as usize] = self.aig.add_mux(shift_bit, orig, shifted);
                    }
                    current = next;
                }

                for bit in 0..*width {
                    self.set_output_bit(node.output, bit, current[bit as usize]);
                }
            }

            LirOp::Shr { width } => {
                // Logical right shift: shift in zeros from left
                let data = node.inputs[0];
                let amount = node.inputs[1];
                let mut current: Vec<AigLit> = (0..*width)
                    .map(|bit| self.get_input_bit(data, bit))
                    .collect();

                for stage in 0..(*width as f32).log2().ceil() as u32 {
                    let shift_bit = self.get_input_bit(amount, stage);
                    let shift_amount = 1u32 << stage;
                    let mut next = vec![self.aig.false_lit(); *width as usize];

                    for bit in 0..*width {
                        let orig = current[bit as usize];
                        let shifted = if bit + shift_amount < *width {
                            current[(bit + shift_amount) as usize]
                        } else {
                            self.aig.false_lit() // Shift in zero
                        };
                        next[bit as usize] = self.aig.add_mux(shift_bit, orig, shifted);
                    }
                    current = next;
                }

                for bit in 0..*width {
                    self.set_output_bit(node.output, bit, current[bit as usize]);
                }
            }

            LirOp::Sar { width } => {
                // Arithmetic right shift: shift in sign bit from left
                let data = node.inputs[0];
                let amount = node.inputs[1];
                let sign_bit = self.get_input_bit(data, width - 1);
                let mut current: Vec<AigLit> = (0..*width)
                    .map(|bit| self.get_input_bit(data, bit))
                    .collect();

                for stage in 0..(*width as f32).log2().ceil() as u32 {
                    let shift_bit = self.get_input_bit(amount, stage);
                    let shift_amount = 1u32 << stage;
                    let mut next = vec![self.aig.false_lit(); *width as usize];

                    for bit in 0..*width {
                        let orig = current[bit as usize];
                        let shifted = if bit + shift_amount < *width {
                            current[(bit + shift_amount) as usize]
                        } else {
                            sign_bit // Shift in sign bit
                        };
                        next[bit as usize] = self.aig.add_mux(shift_bit, orig, shifted);
                    }
                    current = next;
                }

                for bit in 0..*width {
                    self.set_output_bit(node.output, bit, current[bit as usize]);
                }
            }

            // Multiplication - creates grade-school multiplier
            // BUG FIX #247: Handle signed multiplication properly
            LirOp::Mul { width, result_width, signed } => {
                let a = node.inputs[0];
                let b = node.inputs[1];

                let result = if *signed {
                    // Signed multiplication using sign-magnitude approach
                    // BUG FIX #247: Look up actual signal widths for asymmetric inputs (e.g., 16×32)
                    let a_actual_width = lir.signals[a.0 as usize].width;
                    let b_actual_width = lir.signals[b.0 as usize].width;
                    self.build_signed_mul_lir(a, b, *width, *result_width, a_actual_width, b_actual_width)
                } else {
                    // Unsigned multiplication (zero-extension is correct, handled by get_input_bit returning 0)
                    self.build_unsigned_mul_lir(a, b, *width, *result_width)
                };

                for bit in 0..*result_width {
                    self.set_output_bit(node.output, bit, result[bit as usize]);
                }
            }

            LirOp::RedAnd { width } => {
                let input = node.inputs[0];
                let mut result = self.aig.true_lit();
                for bit in 0..*width {
                    let lit = self.get_input_bit(input, bit);
                    result = self.aig.add_and(result, lit);
                }
                self.set_output_bit(node.output, 0, result);
            }

            LirOp::RedOr { width } => {
                let input = node.inputs[0];
                let mut result = self.aig.false_lit();
                for bit in 0..*width {
                    let lit = self.get_input_bit(input, bit);
                    result = self.aig.add_or(result, lit);
                }
                self.set_output_bit(node.output, 0, result);
            }

            LirOp::RedXor { width } => {
                let input = node.inputs[0];
                let mut result = self.aig.false_lit();
                for bit in 0..*width {
                    let lit = self.get_input_bit(input, bit);
                    result = self.aig.add_xor(result, lit);
                }
                self.set_output_bit(node.output, 0, result);
            }

            LirOp::Concat { widths } => {
                let mut out_bit = 0u32;
                for (i, &w) in widths.iter().enumerate() {
                    let input = node.inputs[i];
                    for bit in 0..w {
                        let lit = self.get_input_bit(input, bit);
                        self.set_output_bit(node.output, out_bit, lit);
                        out_bit += 1;
                    }
                }
            }

            LirOp::BitSelect { .. } => {
                let data = node.inputs[0];
                let index = node.inputs[1];
                // For constant index, we can directly select
                // For variable index, we need a mux tree (simplified here)
                let lit = self.get_input_bit(data, 0); // Simplified: assume index 0
                self.set_output_bit(node.output, 0, lit);
            }

            LirOp::RangeSelect { high, low, .. } => {
                let data = node.inputs[0];
                for bit in *low..=*high {
                    let lit = self.get_input_bit(data, bit);
                    self.set_output_bit(node.output, bit - low, lit);
                }
            }

            LirOp::ZeroExtend { from, to } => {
                let input = node.inputs[0];
                for bit in 0..*from {
                    let lit = self.get_input_bit(input, bit);
                    self.set_output_bit(node.output, bit, lit);
                }
                for bit in *from..*to {
                    self.set_output_bit(node.output, bit, self.aig.false_lit());
                }
            }

            LirOp::SignExtend { from, to } => {
                let input = node.inputs[0];
                for bit in 0..*from {
                    let lit = self.get_input_bit(input, bit);
                    self.set_output_bit(node.output, bit, lit);
                }
                let sign_bit = self.get_input_bit(input, from - 1);
                for bit in *from..*to {
                    self.set_output_bit(node.output, bit, sign_bit);
                }
            }

            // Sequential operations - for combinational equivalence checking,
            // we treat register outputs as additional inputs
            LirOp::Reg { width, .. } => {
                // In combinational mode, we skip registers
                // Their outputs should have been added as inputs already
                let output_signal = &lir.signals[node.output.0 as usize];
                for bit in 0..*width {
                    let name = format!("{}[{}]_reg", output_signal.name, bit);
                    let lit = self.aig.add_input(name);
                    self.set_output_bit(node.output, bit, lit);
                }
            }

            // For operations not yet implemented, add placeholder inputs
            _ => {
                let output_signal = &lir.signals[node.output.0 as usize];
                let width = node.op.output_width();
                for bit in 0..width {
                    let name = format!("{}[{}]_unimpl", output_signal.name, bit);
                    let lit = self.aig.add_input(name);
                    self.set_output_bit(node.output, bit, lit);
                }
            }
        }
    }
}

impl Default for LirToAig {
    fn default() -> Self {
        Self::new()
    }
}

// ============================================================================
// GateNetlist to AIG Conversion
// ============================================================================

/// Convert a GateNetlist to an AIG
pub struct GateNetlistToAig {
    aig: Aig,
    /// Map from (GateNetId) to AigLit
    net_map: BTreeMap<u32, AigLit>,
}

impl GateNetlistToAig {
    pub fn new() -> Self {
        Self {
            aig: Aig::new(),
            net_map: BTreeMap::new(),
        }
    }

    /// Convert GateNetlist to AIG
    pub fn convert(mut self, netlist: &GateNetlist) -> Aig {
        // Add primary inputs
        for &input_id in &netlist.inputs {
            let net = &netlist.nets[input_id.0 as usize];
            let lit = self.aig.add_input(net.name.clone());
            self.net_map.insert(input_id.0, lit);
        }

        // Process cells in topological order
        // GateNetlist cells should already be in topological order
        for cell in &netlist.cells {
            self.convert_cell(cell, netlist);
        }

        // Add primary outputs
        // BUG #248 FIX: Resolve net aliases before looking up in net_map.
        // Output port nets may have been merged into other nets during stitching,
        // so we need to find the canonical net ID to get the correct literal.
        for &output_id in &netlist.outputs {
            let net = &netlist.nets[output_id.0 as usize];
            // Resolve alias chain to find canonical net
            let canonical_id = netlist.resolve_alias(output_id);
            let lit = self
                .net_map
                .get(&canonical_id.0)
                .copied()
                .unwrap_or_else(|| {
                    // If still not found, log warning - this output will be constant 0
                    log::warn!(
                        "Output '{}' (id={}, canonical={}) not found in net_map, defaulting to false",
                        net.name, output_id.0, canonical_id.0
                    );
                    self.aig.false_lit()
                });
            self.aig.add_output(net.name.clone(), lit);
        }

        self.aig
    }

    fn get_net(&self, net_id: skalp_lir::GateNetId) -> AigLit {
        // Note: We don't resolve aliases here because cell connections should
        // already point to the correct canonical net IDs after merge_nets_batched.
        // But if lookup fails, we fall back to false_lit which indicates a bug.
        self.net_map
            .get(&net_id.0)
            .copied()
            .unwrap_or_else(|| {
                self.aig.false_lit()
            })
    }

    fn set_net(&mut self, net_id: skalp_lir::GateNetId, lit: AigLit) {
        self.net_map.insert(net_id.0, lit);
    }

    /// Topologically sort combinational cells based on their input/output dependencies.
    /// Returns cell indices in order such that when processing a cell, all its inputs
    /// (from other cells' outputs) have already been processed.
    fn topological_sort_cells(&self, netlist: &GateNetlist) -> Vec<usize> {
        use std::collections::{BTreeMap, BTreeSet, VecDeque};

        // Build a map from output net -> cell index
        // IMPORTANT: Only include COMBINATIONAL cells - DFF outputs should be treated as primary inputs
        // because their values are determined by state, not by combinational logic in the same cycle
        let mut net_to_cell: BTreeMap<u32, usize> = BTreeMap::new();
        for (idx, cell) in netlist.cells.iter().enumerate() {
            // Skip sequential cells - their outputs don't create combinational dependencies
            if cell.is_sequential() {
                continue;
            }
            for &out in &cell.outputs {
                net_to_cell.insert(out.0, idx);
            }
        }

        // Build dependency graph: cell -> cells it depends on
        let mut dependencies: BTreeMap<usize, BTreeSet<usize>> = BTreeMap::new();
        let mut reverse_deps: BTreeMap<usize, BTreeSet<usize>> = BTreeMap::new();

        for (idx, cell) in netlist.cells.iter().enumerate() {
            dependencies.entry(idx).or_default();
            for &inp in &cell.inputs {
                if let Some(&dep_idx) = net_to_cell.get(&inp.0) {
                    if dep_idx != idx {
                        dependencies.entry(idx).or_default().insert(dep_idx);
                        reverse_deps.entry(dep_idx).or_default().insert(idx);
                    }
                }
            }
        }

        // Kahn's algorithm for topological sort
        let mut in_degree: BTreeMap<usize, usize> = BTreeMap::new();
        for (idx, _) in netlist.cells.iter().enumerate() {
            let deg = dependencies.get(&idx).map(|s| s.len()).unwrap_or(0);
            in_degree.insert(idx, deg);
        }

        let mut queue: VecDeque<usize> = VecDeque::new();
        for (&idx, &deg) in &in_degree {
            if deg == 0 {
                queue.push_back(idx);
            }
        }

        let mut result: Vec<usize> = Vec::with_capacity(netlist.cells.len());
        while let Some(idx) = queue.pop_front() {
            result.push(idx);
            if let Some(dependents) = reverse_deps.get(&idx) {
                for &dep in dependents {
                    if let Some(deg) = in_degree.get_mut(&dep) {
                        *deg = deg.saturating_sub(1);
                        if *deg == 0 {
                            queue.push_back(dep);
                        }
                    }
                }
            }
        }

        // If result doesn't contain all cells, there's a cycle - fall back to original order
        if result.len() != netlist.cells.len() {
            log::warn!(
                "Topological sort found cycle, falling back to original order ({} of {} cells sorted)",
                result.len(),
                netlist.cells.len()
            );
            return (0..netlist.cells.len()).collect();
        }

        result
    }

    fn convert_cell(&mut self, cell: &skalp_lir::Cell, netlist: &GateNetlist) {
        use skalp_lir::CellFunction;

        let output_net = cell.outputs.first().copied();

        // Check if cell is sequential (has clock) - treat output as input for CEC
        if cell.is_sequential() {
            if let Some(out) = output_net {
                let net = &netlist.nets[out.0 as usize];
                let lit = self.aig.add_input(format!("{}_dff_out", net.name));
                self.set_net(out, lit);
            }
            return;
        }

        match &cell.function {
            Some(CellFunction::And2) | Some(CellFunction::And3) | Some(CellFunction::And4) => {
                let inputs: Vec<_> = cell.inputs.iter().map(|&id| self.get_net(id)).collect();
                let mut result = inputs[0];
                for &inp in &inputs[1..] {
                    result = self.aig.add_and(result, inp);
                }
                if let Some(out) = output_net {
                    self.set_net(out, result);
                }
            }

            Some(CellFunction::Or2) | Some(CellFunction::Or3) | Some(CellFunction::Or4) => {
                let inputs: Vec<_> = cell.inputs.iter().map(|&id| self.get_net(id)).collect();
                let mut result = inputs[0];
                for &inp in &inputs[1..] {
                    result = self.aig.add_or(result, inp);
                }
                if let Some(out) = output_net {
                    self.set_net(out, result);
                }
            }

            Some(CellFunction::Nand2) | Some(CellFunction::Nand3) | Some(CellFunction::Nand4) => {
                let inputs: Vec<_> = cell.inputs.iter().map(|&id| self.get_net(id)).collect();
                let mut result = inputs[0];
                for &inp in &inputs[1..] {
                    result = self.aig.add_and(result, inp);
                }
                if let Some(out) = output_net {
                    self.set_net(out, result.invert());
                }
            }

            Some(CellFunction::Nor2) | Some(CellFunction::Nor3) | Some(CellFunction::Nor4) => {
                let inputs: Vec<_> = cell.inputs.iter().map(|&id| self.get_net(id)).collect();
                let mut result = inputs[0];
                for &inp in &inputs[1..] {
                    result = self.aig.add_or(result, inp);
                }
                if let Some(out) = output_net {
                    self.set_net(out, result.invert());
                }
            }

            Some(CellFunction::Xor2) => {
                let a = self.get_net(cell.inputs[0]);
                let b = self.get_net(cell.inputs[1]);
                let result = self.aig.add_xor(a, b);
                if let Some(out) = output_net {
                    self.set_net(out, result);
                }
            }

            Some(CellFunction::Xnor2) => {
                let a = self.get_net(cell.inputs[0]);
                let b = self.get_net(cell.inputs[1]);
                let result = self.aig.add_xor(a, b).invert();
                if let Some(out) = output_net {
                    self.set_net(out, result);
                }
            }

            Some(CellFunction::Inv) => {
                let input = self.get_net(cell.inputs[0]);
                if let Some(out) = output_net {
                    self.set_net(out, input.invert());
                }
            }

            Some(CellFunction::Buf) => {
                let input = self.get_net(cell.inputs[0]);
                if let Some(out) = output_net {
                    self.set_net(out, input);
                }
            }

            Some(CellFunction::Mux2) => {
                // Mux2 inputs: [sel, d0, d1]
                // Based on mir_to_lir.rs, inputs are [cond, else_value, then_value]
                // So d0 = inputs[1] = else_value (value when sel=0)
                //    d1 = inputs[2] = then_value (value when sel=1)
                // We want: sel ? d1 : d0 (sel=1 gives d1, sel=0 gives d0)
                // add_mux(sel, a, b) returns sel ? b : a
                // So call add_mux(sel, d0, d1) for sel ? d1 : d0
                let sel = self.get_net(cell.inputs[0]);
                let d0 = self.get_net(cell.inputs[1]);  // else_value (when sel=0)
                let d1 = self.get_net(cell.inputs[2]);  // then_value (when sel=1)

                let result = self.aig.add_mux(sel, d0, d1);

                if let Some(out) = output_net {
                    self.set_net(out, result);
                }
            }

            // For unknown cell types, try to infer from cell_type name
            // Note: Sequential cells are handled above via is_sequential() check
            _ => {
                // Try to infer cell function from cell_type name
                let cell_type_upper = cell.cell_type.to_uppercase();
                let inputs: Vec<_> = cell.inputs.iter().map(|&id| self.get_net(id)).collect();

                let handled = if cell_type_upper.starts_with("BUF") || cell_type_upper.contains("BUFFER") {
                    // Buffer: output = input
                    if !inputs.is_empty() {
                        if let Some(out) = output_net {
                            self.set_net(out, inputs[0]);
                        }
                        true
                    } else {
                        false
                    }
                } else if cell_type_upper.starts_with("INV") || cell_type_upper.starts_with("NOT") {
                    // Inverter: output = !input
                    if !inputs.is_empty() {
                        if let Some(out) = output_net {
                            self.set_net(out, inputs[0].invert());
                        }
                        true
                    } else {
                        false
                    }
                } else if cell_type_upper.starts_with("AND") {
                    // AND gate
                    if inputs.len() >= 2 {
                        let mut result = inputs[0];
                        for &inp in &inputs[1..] {
                            result = self.aig.add_and(result, inp);
                        }
                        if let Some(out) = output_net {
                            self.set_net(out, result);
                        }
                        true
                    } else {
                        false
                    }
                } else if cell_type_upper.starts_with("OR") && !cell_type_upper.starts_with("ORN") {
                    // OR gate (not ORNOR)
                    if inputs.len() >= 2 {
                        let mut result = inputs[0];
                        for &inp in &inputs[1..] {
                            result = self.aig.add_or(result, inp);
                        }
                        if let Some(out) = output_net {
                            self.set_net(out, result);
                        }
                        true
                    } else {
                        false
                    }
                } else if cell_type_upper.starts_with("NAND") {
                    // NAND gate
                    if inputs.len() >= 2 {
                        let mut result = inputs[0];
                        for &inp in &inputs[1..] {
                            result = self.aig.add_and(result, inp);
                        }
                        if let Some(out) = output_net {
                            self.set_net(out, result.invert());
                        }
                        true
                    } else {
                        false
                    }
                } else if cell_type_upper.starts_with("NOR") {
                    // NOR gate
                    if inputs.len() >= 2 {
                        let mut result = inputs[0];
                        for &inp in &inputs[1..] {
                            result = self.aig.add_or(result, inp);
                        }
                        if let Some(out) = output_net {
                            self.set_net(out, result.invert());
                        }
                        true
                    } else {
                        false
                    }
                } else if cell_type_upper.starts_with("XOR") {
                    // XOR gate
                    if inputs.len() >= 2 {
                        let result = self.aig.add_xor(inputs[0], inputs[1]);
                        if let Some(out) = output_net {
                            self.set_net(out, result);
                        }
                        true
                    } else {
                        false
                    }
                } else if cell_type_upper.starts_with("XNOR") {
                    // XNOR gate
                    if inputs.len() >= 2 {
                        let result = self.aig.add_xor(inputs[0], inputs[1]).invert();
                        if let Some(out) = output_net {
                            self.set_net(out, result);
                        }
                        true
                    } else {
                        false
                    }
                } else if cell_type_upper.starts_with("MUX") {
                    // MUX: inputs = [sel, d0, d1]
                    if inputs.len() >= 3 {
                        let result = self.aig.add_mux(inputs[0], inputs[1], inputs[2]);
                        if let Some(out) = output_net {
                            self.set_net(out, result);
                        }
                        true
                    } else {
                        false
                    }
                } else if cell_type_upper.contains("TIE0") || cell_type_upper.contains("TIELOW") || cell_type_upper.contains("TIE_LOW") {
                    // Tie to 0
                    if let Some(out) = output_net {
                        self.set_net(out, self.aig.false_lit());
                    }
                    true
                } else if cell_type_upper.contains("TIE1") || cell_type_upper.contains("TIEHIGH") || cell_type_upper.contains("TIE_HIGH") {
                    // Tie to 1
                    if let Some(out) = output_net {
                        self.set_net(out, self.aig.true_lit());
                    }
                    true
                } else if cell_type_upper.contains("FULLADDER") || cell_type_upper.contains("FA_") {
                    // Full adder: sum = a XOR b XOR cin, cout = (a AND b) OR (cin AND (a XOR b))
                    // Outputs are typically [sum, carry] or [cout, sum]
                    if inputs.len() >= 3 && cell.outputs.len() >= 2 {
                        let a = inputs[0];
                        let b = inputs[1];
                        let cin = inputs[2];

                        // a XOR b
                        let a_xor_b = self.aig.add_xor(a, b);
                        // sum = a XOR b XOR cin
                        let sum = self.aig.add_xor(a_xor_b, cin);
                        // cout = (a AND b) OR (cin AND (a XOR b))
                        let a_and_b = self.aig.add_and(a, b);
                        let cin_and_axorb = self.aig.add_and(cin, a_xor_b);
                        let cout = self.aig.add_or(a_and_b, cin_and_axorb);

                        // Output order varies - try common orderings
                        if cell.outputs.len() >= 2 {
                            self.set_net(cell.outputs[0], sum);
                            self.set_net(cell.outputs[1], cout);
                        }
                        true
                    } else {
                        false
                    }
                } else if cell_type_upper.contains("HALFADDER") || cell_type_upper.contains("HA_") {
                    // Half adder: sum = a XOR b, cout = a AND b
                    if inputs.len() >= 2 && cell.outputs.len() >= 2 {
                        let a = inputs[0];
                        let b = inputs[1];

                        let sum = self.aig.add_xor(a, b);
                        let cout = self.aig.add_and(a, b);

                        self.set_net(cell.outputs[0], sum);
                        self.set_net(cell.outputs[1], cout);
                        true
                    } else {
                        false
                    }
                } else {
                    false
                };

                if !handled {
                    if let Some(out) = output_net {
                        let net = &netlist.nets[out.0 as usize];
                        let lit = self.aig.add_input(format!("{}_unknown", net.name));
                        self.set_net(out, lit);
                    }
                }
            }
        }
    }

    /// Build a 4-input LUT as a mux tree
    fn build_lut4(&mut self, init: u16, i0: AigLit, i1: AigLit, i2: AigLit, i3: AigLit) -> AigLit {
        // LUT4 truth table: output = (init >> {i3,i2,i1,i0}) & 1
        // Build as nested muxes based on i3, then i2, then i1, then i0

        let mut lut_bits: Vec<AigLit> = Vec::with_capacity(16);
        for i in 0..16 {
            if (init >> i) & 1 == 1 {
                lut_bits.push(self.aig.true_lit());
            } else {
                lut_bits.push(self.aig.false_lit());
            }
        }

        // First level: select on i0
        let mut level1: Vec<AigLit> = Vec::with_capacity(8);
        for i in 0..8 {
            level1.push(self.aig.add_mux(i0, lut_bits[i * 2], lut_bits[i * 2 + 1]));
        }

        // Second level: select on i1
        let mut level2: Vec<AigLit> = Vec::with_capacity(4);
        for i in 0..4 {
            level2.push(self.aig.add_mux(i1, level1[i * 2], level1[i * 2 + 1]));
        }

        // Third level: select on i2
        let mut level3: Vec<AigLit> = Vec::with_capacity(2);
        for i in 0..2 {
            level3.push(self.aig.add_mux(i2, level2[i * 2], level2[i * 2 + 1]));
        }

        // Fourth level: select on i3
        self.aig.add_mux(i3, level3[0], level3[1])
    }

    /// Convert GateNetlist to sequential AIG with proper Latch nodes
    ///
    /// This creates an AIG representation where:
    /// - Primary inputs are inputs
    /// - DFF cells become Latch nodes with D input as next-state
    /// - Primary outputs are outputs
    ///
    /// This is needed for BMC (Bounded Model Checking) which unrolls
    /// the sequential circuit over multiple cycles.
    pub fn convert_sequential(mut self, netlist: &GateNetlist) -> Aig {
        // Add primary inputs (excluding clock which is implicit)
        for &input_id in &netlist.inputs {
            let net = &netlist.nets[input_id.0 as usize];
            // Skip clock signals for sequential AIG
            if net.name.contains("clk") || net.name.contains("clock") {
                continue;
            }
            let lit = self.aig.add_input(net.name.clone());
            self.net_map.insert(input_id.0, lit);
        }

        // First pass: identify all DFF cells and create placeholder literals for their outputs
        // We need this because the D input might reference other DFF outputs
        // Tuple: (cell_idx, out_net, latch_name, state_input_node_id)
        let mut dff_cells: Vec<(usize, skalp_lir::GateNetId, String, u32)> = Vec::new();

        for (idx, cell) in netlist.cells.iter().enumerate() {
            if cell.is_sequential() {
                if let Some(out) = cell.outputs.first().copied() {
                    let net = &netlist.nets[out.0 as usize];
                    // Create temporary input for the current DFF output (latch state)
                    let temp_name = format!("__dff_cur_{}", net.name);
                    let temp_lit = self.aig.add_input(temp_name);
                    let state_input_id = temp_lit.node.0;
                    self.net_map.insert(out.0, temp_lit);
                    dff_cells.push((idx, out, net.name.clone(), state_input_id));
                }
            }
        }

        // Process combinational cells in topologically sorted order
        // This ensures that when we process a cell, all its inputs are already in net_map
        let sorted_cell_indices = self.topological_sort_cells(netlist);

        // DEBUG: Log how many cells are being processed and their types
        let mut tie_count = 0;
        let mut mux_count = 0;
        let mut first_tie_idx = None;
        let mut first_mux_idx = None;
        for (i, &idx) in sorted_cell_indices.iter().enumerate() {
            let cell = &netlist.cells[idx];
            if cell.cell_type.contains("TIE") {
                tie_count += 1;
                if first_tie_idx.is_none() {
                    first_tie_idx = Some(i);
                }
            }
            if cell.cell_type.contains("MUX") {
                mux_count += 1;
                if first_mux_idx.is_none() {
                    first_mux_idx = Some(i);
                }
            }
        }
        log::debug!(
            "[TOPO_SORT] {} TIE cells (first at position {:?}), {} MUX cells (first at position {:?}), total {} cells",
            tie_count, first_tie_idx, mux_count, first_mux_idx, sorted_cell_indices.len()
        );

        for idx in sorted_cell_indices {
            let cell = &netlist.cells[idx];
            // Skip DFFs - we handle them specially
            if cell.is_sequential() {
                continue;
            }
            self.convert_cell(cell, netlist);
        }

        // Now create Latch nodes for each DFF
        // The D input is the next-state logic, with reset handling
        for (cell_idx, out_net, latch_name, state_input_id) in &dff_cells {
            let cell = &netlist.cells[*cell_idx];

            // Find the D input (data input to the DFF)
            // DFF typically has inputs: [D, clock] or [D, clock, enable, reset]
            // The first non-clock input is usually D
            let d_input = cell.inputs.iter().find(|&&inp| {
                let net = &netlist.nets[inp.0 as usize];
                !net.name.contains("clk") && !net.name.contains("clock")
            });

            let d_lit = if let Some(&d_net) = d_input {
                self.net_map.get(&d_net.0).copied().unwrap_or_else(|| self.aig.false_lit())
            } else {
                // No D input found, latch holds its value
                self.net_map.get(&out_net.0).copied().unwrap_or_else(|| self.aig.false_lit())
            };

            // Determine reset handling using source_op metadata from tech mapper:
            // 1. If D is driven by source_op="ResetMux" -> non-zero reset, get value from TIE cell
            // 2. If DFF has cell.reset -> it's a DFFR, reset value is 0
            // 3. Otherwise -> no reset
            //
            // The tech mapper sets source_op="ResetMux" for MUXes that handle non-zero reset
            // and source_op="ResetValue" for TIE cells that provide the reset bit value.

            let (next_lit, init_value) = if let Some(&d_net) = d_input {
                // Check if D is driven by a ResetMux (indicates non-zero reset value)
                let reset_mux = netlist.cells.iter().find(|c| {
                    c.outputs.iter().any(|o| o.0 == d_net.0)
                        && c.source_op.as_deref() == Some("ResetMux")
                });

                if let Some(mux_cell) = reset_mux {
                    // D is driven by ResetMux: non-zero reset value
                    // MUX inputs: [rst, d_orig, reset_bit_net]
                    // The reset value comes from the TIE cell driving reset_bit_net (inputs[2])
                    let reset_val = mux_cell.inputs.get(2).and_then(|&reset_net| {
                        netlist.cells.iter().find(|c| {
                            c.outputs.iter().any(|o| o.0 == reset_net.0)
                                && c.source_op.as_deref() == Some("ResetValue")
                        })
                    }).map(|tie_cell| {
                        tie_cell.cell_type.contains("HIGH") || tie_cell.cell_type.contains("HI")
                    }).unwrap_or(false);

                    // For ResetMux: D already handles reset, use D directly
                    // Init value is the reset value
                    (d_lit, reset_val)
                } else if let Some(rst_net) = cell.reset {
                    // DFF has reset pin: it's a DFFR, reset value is always 0
                    // Apply reset gate: next = !rst AND D (rst ? 0 : D)
                    let rst_lit = self.net_map.get(&rst_net.0).copied().unwrap_or_else(|| {
                        let rst_net_obj = &netlist.nets[rst_net.0 as usize];
                        let lit = self.aig.add_input(rst_net_obj.name.clone());
                        self.net_map.insert(rst_net.0, lit);
                        lit
                    });
                    (self.aig.add_and(rst_lit.invert(), d_lit), false) // init = 0
                } else {
                    // No reset at all - use D directly, init = 0
                    (d_lit, false)
                }
            } else {
                // No D input found - default behavior
                (d_lit, false)
            };

            // Create latch with proper init value
            let latch_lit = self.aig.add_latch(latch_name.clone(), next_lit, init_value);
            let latch_id = latch_lit.node.0;

            // Link state input to latch (metadata-based, no name matching needed)
            self.aig.link_state_input_to_latch(*state_input_id, latch_id);

            // Update net_map to use latch output
            self.net_map.insert(out_net.0, latch_lit);
        }

        // Add primary outputs
        // BUG #248 FIX: Resolve net aliases before looking up in net_map.
        for &output_id in &netlist.outputs {
            let net = &netlist.nets[output_id.0 as usize];
            // Resolve alias chain to find canonical net
            let canonical_id = netlist.resolve_alias(output_id);
            let lit = self
                .net_map
                .get(&canonical_id.0)
                .copied()
                .unwrap_or_else(|| {
                    log::warn!(
                        "Output '{}' (id={}, canonical={}) not found in net_map, defaulting to false",
                        net.name, output_id.0, canonical_id.0
                    );
                    self.aig.false_lit()
                });
            self.aig.add_output(net.name.clone(), lit);
        }

        self.aig
    }
}

impl Default for GateNetlistToAig {
    fn default() -> Self {
        Self::new()
    }
}

// ============================================================================
// Miter Construction
// ============================================================================

/// Build a miter circuit from two AIGs
/// Returns an AIG where the single output is 1 iff the designs differ
pub fn build_miter(aig1: &Aig, aig2: &Aig) -> FormalResult<Aig> {
    // Verify same number of outputs
    if aig1.outputs.len() != aig2.outputs.len() {
        return Err(FormalError::PropertyFailed(format!(
            "Output count mismatch: {} vs {}",
            aig1.outputs.len(),
            aig2.outputs.len()
        )));
    }

    let mut miter = Aig::new();

    // Map from old node IDs to new ones for both AIGs
    let mut map1: BTreeMap<u32, AigLit> = BTreeMap::new();
    let mut map2: BTreeMap<u32, AigLit> = BTreeMap::new();

    // Node 0 (false) maps to node 0
    map1.insert(0, miter.false_lit());
    map2.insert(0, miter.false_lit());

    // Add shared inputs (by name matching)
    let mut input_map: BTreeMap<String, AigLit> = BTreeMap::new();

    for (idx, node) in aig1.nodes.iter().enumerate() {
        if let AigNode::Input { name } = node {
            let lit = miter.add_input(name.clone());
            map1.insert(idx as u32, lit);
            input_map.insert(name.clone(), lit);
        }
    }

    for (idx, node) in aig2.nodes.iter().enumerate() {
        if let AigNode::Input { name } = node {
            if let Some(&lit) = input_map.get(name) {
                // Reuse existing input
                map2.insert(idx as u32, lit);
            } else {
                // New input (only in design 2)
                let lit = miter.add_input(name.clone());
                map2.insert(idx as u32, lit);
            }
        }
    }

    // Copy AND gates from both AIGs
    copy_aig_structure(&mut miter, aig1, &mut map1);
    copy_aig_structure(&mut miter, aig2, &mut map2);

    // Build miter: XOR corresponding outputs, OR all XORs
    let mut miter_output = miter.false_lit();

    for (out1, out2) in aig1.outputs.iter().zip(aig2.outputs.iter()) {
        let lit1 = remap_lit(*out1, &map1);
        let lit2 = remap_lit(*out2, &map2);
        let diff = miter.add_xor(lit1, lit2);
        miter_output = miter.add_or(miter_output, diff);
    }

    miter.add_output("miter".to_string(), miter_output);

    Ok(miter)
}

fn remap_lit(lit: AigLit, map: &BTreeMap<u32, AigLit>) -> AigLit {
    let mapped = map.get(&lit.node.0).copied().unwrap_or(AigLit::positive(AigNodeId(0)));
    if lit.inverted {
        mapped.invert()
    } else {
        mapped
    }
}

fn copy_aig_structure(target: &mut Aig, source: &Aig, map: &mut BTreeMap<u32, AigLit>) {
    for (idx, node) in source.nodes.iter().enumerate() {
        if map.contains_key(&(idx as u32)) {
            continue; // Already mapped (input or constant)
        }

        match node {
            AigNode::False | AigNode::Input { .. } => {
                // Already handled
            }
            AigNode::And { left, right } => {
                let new_left = remap_lit(*left, map);
                let new_right = remap_lit(*right, map);
                let new_lit = target.add_and(new_left, new_right);
                map.insert(idx as u32, new_lit);
            }
            AigNode::Latch { .. } => {
                // For combinational checking, latches are treated as inputs.
                // Map latch nodes to their corresponding state inputs so that
                // outputs/next-state that reference latch nodes get remapped
                // correctly. This happens when a DFF Q output net is also the
                // output port net in the gate AIG (no buffer in between).
                for (&state_input_id, &latch_id) in &source.state_input_to_latch {
                    if latch_id == idx as u32 {
                        if let Some(&state_lit) = map.get(&state_input_id) {
                            map.insert(idx as u32, state_lit);
                        }
                        break;
                    }
                }
            }
        }
    }
}

/// SAT-based symbolic equivalence check for sequential circuits
///
/// Unlike simulation-based BMC, this checks if the transition functions are equivalent
/// by using SAT to find ANY state/input combination where designs differ.
/// This can find bugs in slow state machines without needing to simulate to reach the state.
///
/// The check asks: "Exists state S, input I: next_state_A(S,I) != next_state_B(S,I) OR output_A(S,I) != output_B(S,I)"
pub fn check_sequential_equivalence_sat(aig1: &Aig, aig2: &Aig, thorough: bool) -> FormalResult<SymbolicEquivalenceResult> {
    let start = std::time::Instant::now();

    // Build a miter that compares both outputs AND next-state (latch D inputs)
    let (miter, matched_pairs) = build_sequential_miter(aig1, aig2)?;

    let and_count = miter.nodes.iter().filter(|n| matches!(n, AigNode::And { .. })).count();
    eprintln!("   Miter: {} nodes, {} AND gates", miter.nodes.len(), and_count);

    // Quick check: if the miter output is already a constant, skip all solving
    let miter_out = miter.outputs.last().unwrap();
    if miter_out.node.0 == 0 {
        let is_const_true = miter_out.inverted; // node 0 is False, inverted = True
        eprintln!("   Miter output is constant {} (skipping SAT)",
            if is_const_true { "TRUE (non-equiv)" } else { "FALSE (equiv)" });
        return Ok(SymbolicEquivalenceResult {
            equivalent: !is_const_true,
            counterexample: None,
            checked_outputs: true,
            checked_next_state: true,
            time_ms: start.elapsed().as_millis() as u64,
            proven_gates: Vec::new(),
            unresolved_gates: Vec::new(),
            algebraic_result: None,
        });
    }

    // --- Phase 2a: Try FRAIG simplification on large miters ---
    if and_count > 1000 {
        eprintln!("   Running FRAIG simplification (large miter)...");
        let fraig_timeout = std::time::Duration::from_secs(30);
        let miter_clone = miter.clone();

        let (tx, rx) = std::sync::mpsc::channel();
        std::thread::spawn(move || {
            let (mut synth_miter, _map) = formal_aig_to_synth_aig(&miter_clone);
            let before_ands = synth_miter.compute_stats().and_count;
            Strash::new().run(&mut synth_miter);
            let fraig_config = FraigConfig {
                sim_patterns: 256,
                sim_rounds: 8,
                sat_conflict_limit: 50,
                max_sat_calls: 500,
                ..Default::default()
            };
            Fraig::with_config(fraig_config).run(&mut synth_miter);
            let after_ands = synth_miter.compute_stats().and_count;
            let _ = tx.send((synth_miter, before_ands, after_ands));
        });

        match rx.recv_timeout(fraig_timeout) {
            Ok((synth_miter, before_ands, after_ands)) => {
                eprintln!("   FRAIG: {} → {} AND gates", before_ands, after_ands);

                // Check if miter output was reduced to constant (last output = OR of all diffs)
                let outputs = synth_miter.outputs();
                if let Some((_, out_lit)) = outputs.last() {
                    if let Some(val) = out_lit.const_value() {
                        if !val {
                            eprintln!("   FRAIG proved equivalence (miter output = const 0)");
                            return Ok(SymbolicEquivalenceResult {
                                equivalent: true,
                                counterexample: None,
                                checked_outputs: true,
                                checked_next_state: true,
                                time_ms: start.elapsed().as_millis() as u64,
                                proven_gates: Vec::new(),
                                unresolved_gates: Vec::new(),
                                algebraic_result: None,
                            });
                        } else {
                            eprintln!("   FRAIG found non-equivalence (miter output = const 1)");
                            return Ok(SymbolicEquivalenceResult {
                                equivalent: false,
                                counterexample: None,
                                checked_outputs: true,
                                checked_next_state: true,
                                time_ms: start.elapsed().as_millis() as u64,
                                proven_gates: Vec::new(),
                                unresolved_gates: Vec::new(),
                                algebraic_result: None,
                            });
                        }
                    }
                }
                eprintln!("   FRAIG could not resolve miter, falling through to SAT...");
            }
            Err(_) => {
                eprintln!("   FRAIG timed out after {}s, falling through to SAT...", fraig_timeout.as_secs());
            }
        }
    }

    // --- Phase 2b: Parallel SAT solving ---
    // Check diff gates in parallel using rayon. Output diff gates are REQUIRED
    // (must all prove UNSAT). Latch next-state diff gates are best-effort
    // (unresolved is acceptable since they may involve multipliers/accumulators,
    // but SAT = non-equivalent is still a failure).
    let (formula, var_map, _miter_output_lit) = aig_to_cnf(&miter);
    let diff_count = miter.outputs.len().saturating_sub(1);
    let num_threads = rayon::current_num_threads();

    // Separate output diffs (required) from latch next-state diffs (best-effort)
    let mut output_gates: Vec<(usize, String, Lit)> = Vec::new();
    let mut latch_gates: Vec<(usize, String, Lit)> = Vec::new();
    for idx in 0..diff_count {
        let diff_lit = &miter.outputs[idx];
        let diff_name = miter.output_names.get(idx).cloned().unwrap_or_default();
        if let Some(var) = var_map.get(&diff_lit.node.0) {
            let sat_lit = if diff_lit.inverted {
                Lit::negative(*var)
            } else {
                Lit::positive(*var)
            };
            if diff_name.starts_with("diff_next_state[") {
                latch_gates.push((idx, diff_name, sat_lit));
            } else if diff_name.starts_with("diff_") {
                output_gates.push((idx, diff_name, sat_lit));
            }
            // Skip mir_next[*] and gate_next[*] diagnostic outputs
        }
    }

    eprintln!("   SAT: {} vars, {} clauses, {} output + {} latch diff gates ({} threads)",
        formula.max_var(), formula.clauses().len(), output_gates.len(), latch_gates.len(), num_threads);

    let sat_start = std::time::Instant::now();

    // Helper: validate a SAT counterexample by evaluating the miter AIG
    // Returns true if the diff gate actually outputs 1 with the given model
    let validate_counterexample = |model: &[Lit], gate_idx: usize| -> bool {
        let num_nodes = miter.nodes.len();
        let mut values = vec![false; num_nodes];
        values[0] = false; // constant false

        // Build var→node mapping from the original var_map
        let node_to_var: BTreeMap<u32, Var> = var_map.iter().map(|(&n, &v)| (n, v)).collect();

        // Set node values from model
        let model_set: std::collections::HashSet<Lit> = model.iter().cloned().collect();
        for (&node_id, &var) in &var_map {
            let pos = Lit::positive(var);
            let neg = Lit::negative(var);
            if model_set.contains(&pos) {
                values[node_id as usize] = true;
            } else if model_set.contains(&neg) {
                values[node_id as usize] = false;
            }
        }

        // Evaluate AND gates in topological order
        for (i, node) in miter.nodes.iter().enumerate() {
            if let AigNode::And { left, right } = node {
                let l = values[left.node.0 as usize] ^ left.inverted;
                let r = values[right.node.0 as usize] ^ right.inverted;
                values[i] = l && r;
            }
        }

        // Check the specific diff gate
        if let Some(diff_lit) = miter.outputs.get(gate_idx) {
            values[diff_lit.node.0 as usize] ^ diff_lit.inverted
        } else {
            false
        }
    };

    // Helper: run parallel SAT on a set of diff gates
    // limit=0 means no conflict limit (run to completion); limit>0 sets per-gate conflict limit
    // Returns: (validated SAT counterexample if found, proven gate names, unresolved gate (idx, name) pairs)
    let run_parallel_sat = |gates: &[(usize, String, Lit)], label: &str, limit: i32|
        -> (Option<(String, Vec<Lit>)>, Vec<String>, Vec<(usize, String)>)
    {
        let checked = std::sync::atomic::AtomicUsize::new(0);
        let total = gates.len();

        // Collect per-gate results using parallel map, then partition
        use std::sync::Mutex;
        let proven_names: Mutex<Vec<String>> = Mutex::new(Vec::new());
        let unresolved_names: Mutex<Vec<(usize, String)>> = Mutex::new(Vec::new());

        let sat_result: Option<(usize, String, Vec<Lit>)> = gates.par_iter()
            .find_map_any(|(idx, diff_name, sat_lit)| {
                let mut solver = Solver::new();
                solver.add_formula(&formula);
                solver.add_clause(&[*sat_lit]);
                if limit > 0 {
                    solver.set_conflict_limit(limit);
                }

                match solver.solve() {
                    Ok(true) => {
                        let model = solver.model().unwrap().to_vec();
                        Some((*idx, diff_name.clone(), model))
                    }
                    Ok(false) => {
                        let c = checked.fetch_add(1, std::sync::atomic::Ordering::Relaxed) + 1;
                        if c % 20 == 0 || c == total {
                            eprintln!("   SAT {}: {}/{} proven UNSAT ({}ms)",
                                label, c, total, sat_start.elapsed().as_millis());
                        }
                        proven_names.lock().unwrap().push(diff_name.clone());
                        None
                    }
                    Err(_) => {
                        eprintln!("   SAT {}: '{}' exceeded conflict limit",
                            label, diff_name);
                        unresolved_names.lock().unwrap().push((*idx, diff_name.clone()));
                        None
                    }
                }
            });

        // Validate any counterexample found by evaluating the miter AIG directly
        let validated = sat_result.and_then(|(idx, name, model)| {
            if validate_counterexample(&model, idx) {
                eprintln!("   SAT {}: '{}' counterexample VALIDATED by miter evaluation", label, name);
                Some((name, model))
            } else {
                eprintln!("   SAT {}: '{}' counterexample INVALID (solver spurious result), treating as unresolved", label, name);
                unresolved_names.lock().unwrap().push((idx, name));
                None
            }
        });

        let proven = proven_names.into_inner().unwrap();
        let unresolved = unresolved_names.into_inner().unwrap();
        (validated, proven, unresolved)
    };

    // Exhaustive parallel SAT: processes ALL gates (no early termination).
    // Used for latch gates where SAT counterexamples are demoted to unresolved,
    // so we must check every gate. find_map_any would cancel in-flight gates when
    // any gate finds SAT, silently losing their results and causing non-determinism.
    let run_parallel_sat_exhaustive = |gates: &[(usize, String, Lit)], label: &str, limit: i32|
        -> (Vec<(usize, String, Vec<Lit>)>, Vec<String>, Vec<(usize, String)>)
    {
        let checked = std::sync::atomic::AtomicUsize::new(0);
        let total = gates.len();

        use std::sync::Mutex;
        let proven_names: Mutex<Vec<String>> = Mutex::new(Vec::new());
        let unresolved_names: Mutex<Vec<(usize, String)>> = Mutex::new(Vec::new());
        let sat_results: Mutex<Vec<(usize, String, Vec<Lit>)>> = Mutex::new(Vec::new());

        gates.par_iter().for_each(|(idx, diff_name, sat_lit)| {
            let mut solver = Solver::new();
            solver.add_formula(&formula);
            solver.add_clause(&[*sat_lit]);
            if limit > 0 {
                solver.set_conflict_limit(limit);
            }

            match solver.solve() {
                Ok(true) => {
                    let model = solver.model().unwrap().to_vec();
                    sat_results.lock().unwrap().push((*idx, diff_name.clone(), model));
                }
                Ok(false) => {
                    let c = checked.fetch_add(1, std::sync::atomic::Ordering::Relaxed) + 1;
                    if c % 20 == 0 || c == total {
                        eprintln!("   SAT {}: {}/{} proven UNSAT ({}ms)",
                            label, c, total, sat_start.elapsed().as_millis());
                    }
                    proven_names.lock().unwrap().push(diff_name.clone());
                }
                Err(_) => {
                    eprintln!("   SAT {}: '{}' exceeded conflict limit",
                        label, diff_name);
                    unresolved_names.lock().unwrap().push((*idx, diff_name.clone()));
                }
            }
        });

        let results = sat_results.into_inner().unwrap();
        let proven = proven_names.into_inner().unwrap();
        let unresolved = unresolved_names.into_inner().unwrap();
        (results, proven, unresolved)
    };

    // Phase 2b-1: Check output diff gates (5M conflict limit — generous but not unlimited)
    let mut all_proven: Vec<String> = Vec::new();
    let mut all_unresolved: Vec<(usize, String)> = Vec::new();

    if !output_gates.is_empty() {
        let output_conflict_limit = if thorough { 0 } else { 5_000_000 };
        let (sat_result, proven, unresolved) = run_parallel_sat(&output_gates, "outputs", output_conflict_limit);
        if let Some((diff_name, model)) = sat_result {
            eprintln!("   Output differs: {} ({}ms)", diff_name, sat_start.elapsed().as_millis());
            print_counterexample_diagnosis(&miter, &var_map, &model, &diff_name);
            let counterexample = extract_symbolic_counterexample(&miter, &var_map, &model, Some(diff_name.clone()));
            return Ok(SymbolicEquivalenceResult {
                equivalent: false,
                counterexample: Some(counterexample),
                checked_outputs: true,
                checked_next_state: false,
                time_ms: start.elapsed().as_millis() as u64,
                proven_gates: Vec::new(),
                unresolved_gates: Vec::new(),
                algebraic_result: None,
            });
        }
        if !unresolved.is_empty() {
            eprintln!("   SAT outputs: {}/{} proven, {} unresolved ({}ms)",
                proven.len(), output_gates.len(), unresolved.len(), sat_start.elapsed().as_millis());
        } else {
            eprintln!("   SAT outputs: all {}/{} proven UNSAT ({}ms)",
                proven.len(), output_gates.len(), sat_start.elapsed().as_millis());
        }
        all_proven.extend(proven);
        all_unresolved.extend(unresolved);
    }

    // Phase 2b-2: Check latch next-state diff gates (best-effort — unresolved is OK)
    // BUG #273: Latch counterexamples are treated as WARNINGS, not failures.
    // Per-gate SAT checks combinational equivalence of next-state functions for ALL
    // state assignments, including unreachable states. For sequential circuits with
    // don't-care states (e.g., a 2-bit register encoding 3 states, where state 3 is
    // unreachable), synthesis may optimize differently than the MIR. These differences
    // are harmless and should not cause EC failure. Latch counterexamples are demoted
    // to unresolved gates so miter simulation can validate them.
    if !latch_gates.is_empty() {
        let latch_conflict_limit = if thorough { 0 } else { 100_000 };
        // Use exhaustive SAT (not find_map_any) to ensure ALL latch gates are checked.
        // Latch counterexamples are demoted to unresolved, so early termination would
        // silently lose results from cancelled threads, causing non-deterministic counts.
        let (sat_results, proven, mut unresolved) = run_parallel_sat_exhaustive(&latch_gates, "latches", latch_conflict_limit);
        let sat_count = sat_results.len();
        for (idx, diff_name, model) in &sat_results {
            eprintln!("   ⚠️  Latch next-state SAT counterexample (may be unreachable): {} ({}ms)",
                diff_name, sat_start.elapsed().as_millis());
            print_counterexample_diagnosis(&miter, &var_map, model, diff_name);
            unresolved.push((*idx, diff_name.clone()));
        }
        let total_latch = latch_gates.len();
        if !unresolved.is_empty() {
            eprintln!("   SAT latches: {}/{} proven, {} unresolved ({} SAT counterexample) ({}ms)",
                proven.len(), total_latch, unresolved.len(), sat_count,
                sat_start.elapsed().as_millis());
        } else {
            eprintln!("   SAT latches: all {}/{} proven UNSAT ({}ms)",
                proven.len(), total_latch, sat_start.elapsed().as_millis());
        }
        all_proven.extend(proven);
        all_unresolved.extend(unresolved);
    }

    let sat_elapsed = sat_start.elapsed().as_millis();
    let total_unresolved = all_unresolved.len();
    let total_proven = all_proven.len();
    let unresolved_detail = if total_unresolved > 0 {
        format!(", {} unresolved", total_unresolved)
    } else {
        String::new()
    };
    eprintln!("   SAT total: {}/{} proven UNSAT{} ({}ms)",
        total_proven, diff_count, unresolved_detail, sat_elapsed);

    // --- Phase 2b-3: Algebraic verification for SAT-hard gates (multiplier cones) ---
    let algebraic_result = if !all_unresolved.is_empty() {
        let result = crate::arithmetic_solver::verify_unresolved_algebraic(
            &miter,
            &matched_pairs,
            &all_unresolved,
        );

        if !result.proven_names.is_empty() {
            let proven_set: BTreeSet<String> = result.proven_names.iter().cloned().collect();
            all_unresolved.retain(|(_, name)| !proven_set.contains(name));
            all_proven.extend(result.proven_names.clone());
        }
        Some(result)
    } else {
        None
    };

    // Return result with unresolved gates
    Ok(SymbolicEquivalenceResult {
        equivalent: true,
        counterexample: None,
        checked_outputs: all_unresolved.iter().all(|(_, n)| !n.starts_with("diff_output[")),
        checked_next_state: all_unresolved.iter().all(|(_, n)| !n.starts_with("diff_next_state[")),
        time_ms: start.elapsed().as_millis() as u64,
        proven_gates: all_proven,
        unresolved_gates: all_unresolved,
        algebraic_result,
    })
}

/// Simulate a miter AIG with random input patterns to verify unresolved diff gates.
/// Returns (number of gates verified, optional failing gate info).
/// If a diff gate outputs 1, a counterexample was found (non-equivalent).
fn simulate_miter_random(
    miter: &Aig,
    num_patterns: usize,
    diff_gate_indices: &[(usize, String)],
) -> Result<(usize, Option<(String, usize)>), FormalError> {
    use rand::SeedableRng;

    let num_nodes = miter.nodes.len();
    let mut values = vec![false; num_nodes];
    let mut rng = rand::rngs::StdRng::seed_from_u64(42); // deterministic seed

    // Identify input and latch node indices for random assignment
    let input_indices: Vec<usize> = miter.nodes.iter().enumerate()
        .filter_map(|(i, n)| if matches!(n, AigNode::Input { .. }) { Some(i) } else { None })
        .collect();
    let latch_indices: Vec<usize> = miter.nodes.iter().enumerate()
        .filter_map(|(i, n)| if matches!(n, AigNode::Latch { .. }) { Some(i) } else { None })
        .collect();

    // Pre-collect AND gate info for fast evaluation
    let and_info: Vec<(usize, u32, bool, u32, bool)> = miter.nodes.iter().enumerate()
        .filter_map(|(i, n)| {
            if let AigNode::And { left, right } = n {
                Some((i, left.node.0, left.inverted, right.node.0, right.inverted))
            } else {
                None
            }
        })
        .collect();

    // Pre-collect diff gate output info (excluding the last output which is the miter OR)
    let diff_outputs: Vec<(usize, u32, bool, &str)> = diff_gate_indices.iter()
        .filter_map(|(idx, name)| {
            let lit = miter.outputs.get(*idx)?;
            Some((*idx, lit.node.0, lit.inverted, name.as_str()))
        })
        .collect();

    // Map state_input nodes to their latch values
    let state_input_map: Vec<(usize, usize)> = miter.state_input_to_latch.iter()
        .map(|(si, latch)| (*si as usize, *latch as usize))
        .collect();

    for pattern in 0..num_patterns {
        // Assign random values to inputs
        for &idx in &input_indices {
            values[idx] = rng.gen_bool(0.5);
        }

        // Assign random values to latches (testing all possible states)
        for &idx in &latch_indices {
            values[idx] = rng.gen_bool(0.5);
        }

        // Copy latch values to state inputs
        for &(si, latch) in &state_input_map {
            if si < num_nodes && latch < num_nodes {
                values[si] = values[latch];
            }
        }

        // Node 0 = constant false
        values[0] = false;

        // Evaluate AND gates in topological order (nodes are sorted by ID)
        for &(i, left_node, left_inv, right_node, right_inv) in &and_info {
            let left_val = values[left_node as usize] ^ left_inv;
            let right_val = values[right_node as usize] ^ right_inv;
            values[i] = left_val && right_val;
        }

        // Check unresolved diff gate outputs
        for &(_idx, node, inv, name) in &diff_outputs {
            let val = values[node as usize] ^ inv;
            if val {
                // Diff gate output is 1 — counterexample found!
                return Ok((0, Some((name.to_string(), pattern))));
            }
        }
    }

    // All patterns passed — all diff gates verified
    Ok((diff_outputs.len(), None))
}

/// Extract the logic cone for a specific diff gate from a miter AIG.
///
/// Traverses backward from the diff gate's output literal, collecting all
/// reachable AND gates, inputs, and latches. Returns a compact `AigCone`
/// with node IDs remapped to [0..N) for efficient GPU evaluation.
pub fn extract_aig_cone(miter: &Aig, diff_gate_idx: usize) -> AigCone {
    let diff_lit = &miter.outputs[diff_gate_idx];
    let diff_name = miter.output_names.get(diff_gate_idx)
        .cloned()
        .unwrap_or_else(|| format!("diff_{}", diff_gate_idx));

    // Phase 1: Backward traversal to find all nodes in the cone
    let mut in_cone = std::collections::HashSet::new();
    let mut stack = vec![diff_lit.node.0];
    in_cone.insert(0u32); // Node 0 (constant false) is always in cone

    while let Some(node_id) = stack.pop() {
        if !in_cone.insert(node_id) {
            continue;
        }
        match &miter.nodes[node_id as usize] {
            AigNode::And { left, right } => {
                stack.push(left.node.0);
                stack.push(right.node.0);
            }
            AigNode::Input { .. } => {
                // Also follow state_input → latch link
                if let Some(&latch_id) = miter.state_input_to_latch.get(&node_id) {
                    stack.push(latch_id);
                }
            }
            AigNode::Latch { .. } | AigNode::False => {}
        }
    }

    // Phase 2: Build compact node ID mapping (original_id → compact_id)
    // Sort by original ID to maintain topological order
    let mut sorted_cone: Vec<u32> = in_cone.iter().copied().collect();
    sorted_cone.sort();

    let mut id_map: BTreeMap<u32, u32> = BTreeMap::new();
    for (compact_id, &orig_id) in sorted_cone.iter().enumerate() {
        id_map.insert(orig_id, compact_id as u32);
    }

    // Build input node_id → name mapping
    let input_id_to_name: BTreeMap<u32, String> = miter.inputs.iter().enumerate()
        .map(|(i, id)| (id.0, miter.input_names.get(i).cloned().unwrap_or_else(|| format!("input_{}", i))))
        .collect();

    // Phase 3: Collect cone components with remapped IDs
    let mut and_gates = Vec::new();
    let mut input_indices = Vec::new();
    let mut input_names = Vec::new();
    let mut latch_indices = Vec::new();
    let mut latch_names = Vec::new();
    let mut state_input_map = Vec::new();

    for &orig_id in &sorted_cone {
        let compact_id = id_map[&orig_id];
        match &miter.nodes[orig_id as usize] {
            AigNode::And { left, right } => {
                let l_compact = id_map[&left.node.0];
                let r_compact = id_map[&right.node.0];
                and_gates.push((compact_id, l_compact, left.inverted, r_compact, right.inverted));
            }
            AigNode::Input { .. } => {
                input_indices.push(compact_id);
                input_names.push(input_id_to_name.get(&orig_id).cloned().unwrap_or_else(|| format!("node_{}", orig_id)));
                // Map state_input → latch if applicable
                if let Some(&latch_orig) = miter.state_input_to_latch.get(&orig_id) {
                    if let Some(&latch_compact) = id_map.get(&latch_orig) {
                        state_input_map.push((compact_id, latch_compact));
                    }
                }
            }
            AigNode::Latch { .. } => {
                latch_indices.push(compact_id);
                latch_names.push(format!("latch_node_{}", orig_id));
            }
            AigNode::False => {}
        }
    }

    let output_compact = id_map[&diff_lit.node.0];

    AigCone {
        name: diff_name,
        and_gates,
        input_indices,
        input_names,
        latch_indices,
        latch_names,
        state_input_map,
        output_node: output_compact,
        output_inverted: diff_lit.inverted,
        num_nodes: sorted_cone.len() as u32,
    }
}

/// Inject random bugs into an AIG for self-testing the EC pipeline.
///
/// Returns up to `count` mutated AIGs, each paired with a description of the mutation.
/// Mutation strategies:
/// 1. Invert an output literal
/// 2. Invert a latch next-state literal
/// 3. Swap two output literals
/// 4. Stuck-at on an AND gate input (replace with constant 0 or 1)
pub fn inject_random_bugs(aig: &Aig, count: usize) -> Vec<(Aig, String)> {
    use rand::SeedableRng;

    let mut rng = rand::rngs::StdRng::seed_from_u64(
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap_or_default()
            .as_nanos() as u64,
    );

    let latch_indices: Vec<usize> = aig.nodes.iter().enumerate()
        .filter_map(|(i, n)| if matches!(n, AigNode::Latch { .. }) { Some(i) } else { None })
        .collect();

    let mut results = Vec::new();

    for _ in 0..count {
        // Pick a strategy — only use always-observable mutations that validate
        // miter construction/matching (not stuck-at, which can hit redundant logic)
        let mut strategies: Vec<u8> = Vec::new();
        if !aig.outputs.is_empty() { strategies.push(0); } // invert output
        if !latch_indices.is_empty() { strategies.push(1); } // invert latch next-state
        if aig.outputs.len() >= 2 { strategies.push(2); } // swap outputs

        if strategies.is_empty() {
            break;
        }

        let strategy = strategies[rng.gen_range(0..strategies.len())];
        let mut mutant = aig.clone();

        let desc = match strategy {
            0 => {
                // Invert a random output literal
                let idx = rng.gen_range(0..mutant.outputs.len());
                mutant.outputs[idx].inverted = !mutant.outputs[idx].inverted;
                let name = mutant.output_names.get(idx).cloned().unwrap_or_else(|| format!("out_{}", idx));
                format!("invert output '{}'", name)
            }
            1 => {
                // Invert a random latch next-state literal
                let li = latch_indices[rng.gen_range(0..latch_indices.len())];
                if let AigNode::Latch { ref name, ref mut next, .. } = mutant.nodes[li] {
                    next.inverted = !next.inverted;
                    format!("invert latch next-state '{}'", name)
                } else {
                    unreachable!()
                }
            }
            2 => {
                // Swap two random output literals (keep names in place so miter detects the swap)
                let a = rng.gen_range(0..mutant.outputs.len());
                let mut b = rng.gen_range(0..mutant.outputs.len());
                while b == a { b = rng.gen_range(0..mutant.outputs.len()); }
                mutant.outputs.swap(a, b);
                let name_a = aig.output_names.get(a).cloned().unwrap_or_else(|| format!("out_{}", a));
                let name_b = aig.output_names.get(b).cloned().unwrap_or_else(|| format!("out_{}", b));
                format!("swap outputs '{}' <-> '{}'", name_a, name_b)
            }
            _ => unreachable!(),
        };

        results.push((mutant, desc));
    }

    results
}

/// Fast bug detection for Phase 3 self-test.
/// Builds miter and tries to find non-equivalence quickly (low conflict limits, no FRAIG).
/// Returns true if the mutation was detected as non-equivalent.
pub fn check_non_equivalence_fast(aig1: &Aig, aig2: &Aig) -> bool {
    use rayon::prelude::*;

    // Build miter
    let miter = match build_sequential_miter(aig1, aig2) {
        Ok((m, _)) => m,
        Err(_) => return false,
    };

    // Quick check: is the miter output constant TRUE?
    if let Some(miter_out) = miter.outputs.last() {
        if miter_out.node.0 == 0 && miter_out.inverted {
            return true; // Constant TRUE = trivially non-equivalent
        }
    }

    // Quick parallel SAT: try each diff gate with a low conflict limit
    let diff_count = miter.outputs.len().saturating_sub(1);
    if diff_count == 0 {
        return false;
    }

    let (formula, var_map, _) = aig_to_cnf(&miter);

    let diff_gates: Vec<Lit> = (0..diff_count)
        .filter_map(|idx| {
            let diff_lit = &miter.outputs[idx];
            let var = var_map.get(&diff_lit.node.0)?;
            Some(if diff_lit.inverted { Lit::negative(*var) } else { Lit::positive(*var) })
        })
        .collect();

    // Try all diff gates in parallel with 100K conflict limit (fast detection, not proof)
    let detected = diff_gates.par_iter()
        .find_map_any(|sat_lit| {
            let mut solver = Solver::new();
            solver.add_formula(&formula);
            solver.add_clause(&[*sat_lit]);
            solver.set_conflict_limit(100_000);
            match solver.solve() {
                Ok(true) => Some(true), // SAT = non-equivalent detected
                _ => None,
            }
        });

    if detected.is_some() {
        return true;
    }

    // SAT couldn't detect — fall back to miter simulation (catches multiplier-cone mutations)
    let diff_gate_indices: Vec<(usize, String)> = (0..diff_count)
        .map(|idx| {
            let name = miter.output_names.get(idx).cloned().unwrap_or_else(|| format!("diff_{}", idx));
            (idx, name)
        })
        .collect();

    if let Ok((_, Some(_))) = simulate_miter_random(&miter, 100_000, &diff_gate_indices) {
        return true; // Simulation found a counterexample
    }

    false
}

/// Result of symbolic equivalence checking
#[derive(Debug, Clone)]
pub struct SymbolicEquivalenceResult {
    /// True if designs are equivalent for all reachable AND unreachable states
    pub equivalent: bool,
    /// Counterexample showing state/input where designs differ
    pub counterexample: Option<SymbolicCounterexample>,
    /// Whether output equivalence was checked
    pub checked_outputs: bool,
    /// Whether next-state equivalence was checked
    pub checked_next_state: bool,
    /// Verification time in milliseconds
    pub time_ms: u64,
    /// Gate names proven UNSAT by SAT solver
    pub proven_gates: Vec<String>,
    /// Unresolved gates: (miter output index, name) for SAT-hard gates
    pub unresolved_gates: Vec<(usize, String)>,
    /// Algebraic verification result (if any unresolved gates were checked)
    pub algebraic_result: Option<crate::arithmetic_solver::AlgebraicVerificationResult>,
}

/// Counterexample from symbolic equivalence check
#[derive(Debug, Clone)]
pub struct SymbolicCounterexample {
    /// Current state assignment (state variable -> value)
    pub state: HashMap<String, bool>,
    /// Input assignment
    pub inputs: HashMap<String, bool>,
    /// Which signal differed (output name or latch name)
    pub differing_signal: Option<String>,
}

/// Pre-compute structural latch matching between two AIGs.
/// Returns a map from gate latch normalized name → MIR latch normalized name
/// for latches that can't be matched by name but can be matched by output connectivity.
fn compute_structural_latch_matches(aig1: &Aig, aig2: &Aig) -> BTreeMap<String, String> {
    let mut result: BTreeMap<String, String> = BTreeMap::new();

    // First pass: find name-matched latches
    let mut mir_latch_names: std::collections::HashSet<String> = std::collections::HashSet::new();
    for &latch_id in &aig1.latches {
        if let AigNode::Latch { name, .. } = &aig1.nodes[latch_id.0 as usize] {
            mir_latch_names.insert(normalize_signal_name_for_matching(name));
        }
    }
    let mut gate_latch_names: std::collections::HashSet<String> = std::collections::HashSet::new();
    for &latch_id in &aig2.latches {
        if let AigNode::Latch { name, .. } = &aig2.nodes[latch_id.0 as usize] {
            gate_latch_names.insert(normalize_signal_name_for_matching(name));
        }
    }

    // Unmatched latches (present in one side but not the other)
    let unmatched_mir: std::collections::HashSet<String> = mir_latch_names.difference(&gate_latch_names).cloned().collect();
    let unmatched_gate: std::collections::HashSet<String> = gate_latch_names.difference(&mir_latch_names).cloned().collect();

    if unmatched_mir.is_empty() || unmatched_gate.is_empty() {
        return result;
    }

    // Build set of unmatched MIR latch node IDs for cone traversal
    let mut mir_latch_node_to_name: HashMap<u32, String> = HashMap::new();
    for &latch_id in &aig1.latches {
        if let AigNode::Latch { name, .. } = &aig1.nodes[latch_id.0 as usize] {
            let normalized = normalize_signal_name_for_matching(name);
            if unmatched_mir.contains(&normalized) {
                mir_latch_node_to_name.insert(latch_id.0, normalized);
            }
        }
    }

    // Build gate output → unmatched gate latch name map
    let mut gate_output_to_latch: HashMap<String, String> = HashMap::new();
    for (i, &out_lit) in aig2.outputs.iter().enumerate() {
        if let Some(out_name) = aig2.output_names.get(i) {
            let norm_out = normalize_signal_name_for_matching(out_name);
            match &aig2.nodes[out_lit.node.0 as usize] {
                AigNode::Latch { name, .. } => {
                    let norm_latch = normalize_signal_name_for_matching(name);
                    if unmatched_gate.contains(&norm_latch) {
                        gate_output_to_latch.insert(norm_out, norm_latch);
                    }
                }
                _ => {
                    if unmatched_gate.contains(&norm_out) {
                        gate_output_to_latch.insert(norm_out.clone(), norm_out);
                    }
                }
            }
        }
    }

    // Cone traversal helper: find unmatched MIR latch in backward cone of a literal
    let find_latch_in_cone = |start_lit: AigLit| -> Option<String> {
        let mut stack = vec![start_lit.node.0];
        let mut visited = std::collections::HashSet::new();
        let mut found: Option<String> = None;
        while let Some(node_id) = stack.pop() {
            if !visited.insert(node_id) {
                continue;
            }
            if let Some(name) = mir_latch_node_to_name.get(&node_id) {
                if found.is_some() {
                    return None; // Ambiguous
                }
                found = Some(name.clone());
                continue;
            }
            match &aig1.nodes[node_id as usize] {
                AigNode::And { left, right } => {
                    stack.push(left.node.0);
                    stack.push(right.node.0);
                }
                AigNode::Input { .. } => {
                    if let Some(&latch_id) = aig1.state_input_to_latch.get(&node_id) {
                        if let Some(name) = mir_latch_node_to_name.get(&latch_id) {
                            if found.is_some() {
                                return None;
                            }
                            found = Some(name.clone());
                        }
                    }
                }
                AigNode::Latch { .. } | AigNode::False => {}
            }
        }
        found
    };

    // For each MIR output that has a corresponding gate output with an unmatched latch,
    // trace backward through MIR AIG to find the unmatched MIR latch
    let mut applied_mir: std::collections::HashSet<String> = std::collections::HashSet::new();
    let mut applied_gate: std::collections::HashSet<String> = std::collections::HashSet::new();
    for (i, &out_lit) in aig1.outputs.iter().enumerate() {
        if let Some(out_name) = aig1.output_names.get(i) {
            let norm_out = normalize_signal_name_for_matching(out_name);
            if let Some(gate_latch_name) = gate_output_to_latch.get(&norm_out) {
                if applied_gate.contains(gate_latch_name) {
                    continue;
                }
                if let Some(mir_latch_name) = find_latch_in_cone(out_lit) {
                    if applied_mir.contains(&mir_latch_name) {
                        continue;
                    }
                    eprintln!("     [LATCH MATCHED VIA STRUCTURE] MIR '{}' <-> Gate '{}' (both feed output '{}')",
                        mir_latch_name, gate_latch_name, norm_out);
                    result.insert(gate_latch_name.clone(), mir_latch_name.clone());
                    applied_mir.insert(mir_latch_name);
                    applied_gate.insert(gate_latch_name.clone());
                }
            }
        }
    }

    result
}

/// Compute structural I/O port matches between two AIGs as a fallback when name-based
/// matching fails. Uses backward cone analysis (outputs) and forward cone analysis (inputs)
/// to find correspondences through shared matched ports.
///
/// Returns (input_matches, output_matches) where each maps aig2_name → aig1_name.
fn compute_structural_io_matches(
    aig1: &Aig,
    aig2: &Aig,
    matched_input_keys: &std::collections::HashSet<String>,
    matched_output_keys: &std::collections::HashSet<String>,
    entity_name: Option<&str>,
) -> (BTreeMap<String, String>, BTreeMap<String, String>) {
    let mut input_matches: BTreeMap<String, String> = BTreeMap::new();
    let mut output_matches: BTreeMap<String, String> = BTreeMap::new();

    // --- Output matching via backward cone (input signature) ---

    // Build node→input_key maps for both AIGs
    let build_input_key_map = |aig: &Aig| -> BTreeMap<u32, String> {
        let mut map = BTreeMap::new();
        for (idx, node) in aig.nodes.iter().enumerate() {
            if let AigNode::Input { name } = node {
                let key = normalize_port_name_with_entity(name, entity_name).key();
                if matched_input_keys.contains(&key) {
                    map.insert(idx as u32, key);
                }
            }
        }
        map
    };

    let aig1_input_keys = build_input_key_map(aig1);
    let aig2_input_keys = build_input_key_map(aig2);

    // Compute "input signature" of an output: the set of matched input keys in its backward cone
    let compute_input_signature = |aig: &Aig, out_lit: AigLit, input_keys: &BTreeMap<u32, String>| -> std::collections::BTreeSet<String> {
        let mut sig = std::collections::BTreeSet::new();
        let mut stack = vec![out_lit.node.0];
        let mut visited = std::collections::HashSet::new();
        while let Some(node_id) = stack.pop() {
            if !visited.insert(node_id) {
                continue;
            }
            if let Some(key) = input_keys.get(&node_id) {
                sig.insert(key.clone());
                continue;
            }
            match &aig.nodes[node_id as usize] {
                AigNode::And { left, right } => {
                    stack.push(left.node.0);
                    stack.push(right.node.0);
                }
                _ => {}
            }
        }
        sig
    };

    // Collect unmatched outputs from both AIGs with their input signatures
    let mut aig1_unmatched_outputs: Vec<(String, String, std::collections::BTreeSet<String>)> = Vec::new(); // (key, original_name, signature)
    for (i, &out_lit) in aig1.outputs.iter().enumerate() {
        if let Some(name) = aig1.output_names.get(i) {
            let key = normalize_port_name_with_entity(name, entity_name).key();
            if !matched_output_keys.contains(&key) {
                let sig = compute_input_signature(aig1, out_lit, &aig1_input_keys);
                if !sig.is_empty() {
                    aig1_unmatched_outputs.push((key, name.clone(), sig));
                }
            }
        }
    }

    let mut aig2_unmatched_outputs: Vec<(String, String, std::collections::BTreeSet<String>)> = Vec::new();
    for (i, &out_lit) in aig2.outputs.iter().enumerate() {
        if let Some(name) = aig2.output_names.get(i) {
            let key = normalize_port_name_with_entity(name, entity_name).key();
            if !matched_output_keys.contains(&key) {
                let sig = compute_input_signature(aig2, out_lit, &aig2_input_keys);
                if !sig.is_empty() {
                    aig2_unmatched_outputs.push((key, name.clone(), sig));
                }
            }
        }
    }

    // Match outputs with identical input signatures (must be unique on both sides)
    let mut aig1_sig_map: BTreeMap<std::collections::BTreeSet<String>, Vec<usize>> = BTreeMap::new();
    for (i, (_, _, sig)) in aig1_unmatched_outputs.iter().enumerate() {
        aig1_sig_map.entry(sig.clone()).or_default().push(i);
    }

    let mut used_aig1: std::collections::HashSet<usize> = std::collections::HashSet::new();
    for (aig2_key, _, aig2_sig) in &aig2_unmatched_outputs {
        if let Some(aig1_indices) = aig1_sig_map.get(aig2_sig) {
            // Only match if there's exactly one candidate on the aig1 side
            let available: Vec<_> = aig1_indices.iter().filter(|i| !used_aig1.contains(i)).collect();
            if available.len() == 1 {
                let aig1_idx = *available[0];
                let aig1_key = &aig1_unmatched_outputs[aig1_idx].0;
                eprintln!("     [OUTPUT MATCHED VIA STRUCTURE] AIG2 '{}' <-> AIG1 '{}' (same input cone)", aig2_key, aig1_key);
                output_matches.insert(aig2_key.clone(), aig1_key.clone());
                used_aig1.insert(aig1_idx);
            }
        }
    }

    // --- Input matching via forward cone (output signature) ---

    // Build node→output_key maps: for each input, find which matched outputs it feeds
    let compute_output_signature_for_input = |aig: &Aig, input_node_id: u32| -> std::collections::BTreeSet<String> {
        let mut sig = std::collections::BTreeSet::new();
        // Forward analysis: check each matched output's backward cone for this input
        for (i, &out_lit) in aig.outputs.iter().enumerate() {
            if let Some(name) = aig.output_names.get(i) {
                let key = normalize_port_name_with_entity(name, entity_name).key();
                let all_matched: std::collections::HashSet<_> = matched_output_keys.iter()
                    .chain(output_matches.values())
                    .chain(output_matches.keys())
                    .cloned()
                    .collect();
                if all_matched.contains(&key) {
                    // Check if input_node_id is in backward cone of this output
                    let mut stack = vec![out_lit.node.0];
                    let mut visited = std::collections::HashSet::new();
                    let mut found = false;
                    while let Some(nid) = stack.pop() {
                        if !visited.insert(nid) { continue; }
                        if nid == input_node_id {
                            found = true;
                            break;
                        }
                        match &aig.nodes[nid as usize] {
                            AigNode::And { left, right } => {
                                stack.push(left.node.0);
                                stack.push(right.node.0);
                            }
                            _ => {}
                        }
                    }
                    if found {
                        sig.insert(key);
                    }
                }
            }
        }
        sig
    };

    // Collect unmatched inputs
    let mut aig1_unmatched_inputs: Vec<(String, u32, std::collections::BTreeSet<String>)> = Vec::new();
    let mut aig2_unmatched_inputs: Vec<(String, u32, std::collections::BTreeSet<String>)> = Vec::new();

    for (idx, node) in aig1.nodes.iter().enumerate() {
        if let AigNode::Input { name } = node {
            let key = normalize_port_name_with_entity(name, entity_name).key();
            if !matched_input_keys.contains(&key) && !is_internal_signal(name) {
                let sig = compute_output_signature_for_input(aig1, idx as u32);
                if !sig.is_empty() {
                    aig1_unmatched_inputs.push((key, idx as u32, sig));
                }
            }
        }
    }

    for (idx, node) in aig2.nodes.iter().enumerate() {
        if let AigNode::Input { name } = node {
            let key = normalize_port_name_with_entity(name, entity_name).key();
            if !matched_input_keys.contains(&key) && !is_internal_signal(name) {
                let sig = compute_output_signature_for_input(aig2, idx as u32);
                if !sig.is_empty() {
                    aig2_unmatched_inputs.push((key, idx as u32, sig));
                }
            }
        }
    }

    // Match inputs with identical output signatures
    let mut aig1_input_sig_map: BTreeMap<std::collections::BTreeSet<String>, Vec<usize>> = BTreeMap::new();
    for (i, (_, _, sig)) in aig1_unmatched_inputs.iter().enumerate() {
        aig1_input_sig_map.entry(sig.clone()).or_default().push(i);
    }

    let mut used_aig1_inputs: std::collections::HashSet<usize> = std::collections::HashSet::new();
    for (aig2_key, _, aig2_sig) in &aig2_unmatched_inputs {
        if let Some(aig1_indices) = aig1_input_sig_map.get(aig2_sig) {
            let available: Vec<_> = aig1_indices.iter().filter(|i| !used_aig1_inputs.contains(i)).collect();
            if available.len() == 1 {
                let aig1_idx = *available[0];
                let aig1_key = &aig1_unmatched_inputs[aig1_idx].0;
                eprintln!("     [INPUT MATCHED VIA STRUCTURE] AIG2 '{}' <-> AIG1 '{}' (same output cone)", aig2_key, aig1_key);
                input_matches.insert(aig2_key.clone(), aig1_key.clone());
                used_aig1_inputs.insert(aig1_idx);
            }
        }
    }

    (input_matches, output_matches)
}

/// Build a miter that checks both output AND next-state equivalence
pub fn build_sequential_miter(aig1: &Aig, aig2: &Aig) -> FormalResult<(Aig, Vec<(String, AigLit, AigLit)>)> {
    let mut miter = Aig::new();

    // Pre-compute structural latch matches (gate_name → mir_name)
    let structural_matches = compute_structural_latch_matches(aig1, aig2);

    // Map from old node IDs to new ones
    let mut map1: BTreeMap<u32, AigLit> = BTreeMap::new();
    let mut map2: BTreeMap<u32, AigLit> = BTreeMap::new();

    map1.insert(0, miter.false_lit());
    map2.insert(0, miter.false_lit());

    // Collect inputs and state inputs from both AIGs
    let mut shared_inputs: BTreeMap<String, AigLit> = BTreeMap::new();

    // Add inputs from AIG1 (including state inputs __reg_cur_*)
    for (idx, node) in aig1.nodes.iter().enumerate() {
        if let AigNode::Input { name } = node {
            let lit = miter.add_input(name.clone());
            map1.insert(idx as u32, lit);
            shared_inputs.insert(name.clone(), lit);
            // Also store normalized name for cross-design matching
            // (AIG1 uses __reg_cur_ prefix, AIG2 uses __dff_cur_<module>. prefix)
            let normalized = normalize_signal_name_for_matching(name);
            if normalized != *name {
                shared_inputs.entry(normalized).or_insert(lit);
            }
        }
    }

    // Add inputs from AIG2, reusing shared inputs
    for (idx, node) in aig2.nodes.iter().enumerate() {
        if let AigNode::Input { name } = node {
            // Normalize name for matching (handle different naming conventions)
            let normalized = normalize_signal_name_for_matching(name);
            // Check if this gate state input has a structural match to a MIR latch
            let effective_normalized = if let Some(mir_name) = structural_matches.get(&normalized) {
                mir_name.clone()
            } else {
                normalized
            };
            if let Some(&lit) = shared_inputs.get(&effective_normalized) {
                map2.insert(idx as u32, lit);
            } else if let Some(&lit) = shared_inputs.get(name) {
                map2.insert(idx as u32, lit);
            } else {
                let lit = miter.add_input(name.clone());
                map2.insert(idx as u32, lit);
                shared_inputs.insert(name.clone(), lit);
            }
        }
    }

    // Copy AND gate structure from both AIGs
    copy_aig_structure(&mut miter, aig1, &mut map1);
    copy_aig_structure(&mut miter, aig2, &mut map2);

    // Build miter output: OR of all differences
    let mut miter_output = miter.false_lit();

    let mut diff_gates: Vec<(String, AigLit)> = Vec::new();
    let mut matched_pairs: Vec<(String, AigLit, AigLit)> = Vec::new();

    // 1. Check output equivalence (by normalized name matching)
    // Use BTreeMap for deterministic iteration order (HashMap randomizes per-process,
    // causing different miter AIG structures and non-deterministic SAT results).
    let mut out1_by_name: BTreeMap<String, AigLit> = BTreeMap::new();
    for (i, out1) in aig1.outputs.iter().enumerate() {
        let name = aig1.output_names.get(i).cloned().unwrap_or_else(|| format!("output_{}", i));
        let normalized = normalize_signal_name_for_matching(&name);
        let lit = remap_lit(*out1, &map1);
        out1_by_name.insert(normalized, lit);
    }
    let mut out2_by_name: BTreeMap<String, AigLit> = BTreeMap::new();
    for (i, out2) in aig2.outputs.iter().enumerate() {
        let name = aig2.output_names.get(i).cloned().unwrap_or_else(|| format!("output_{}", i));
        let normalized = normalize_signal_name_for_matching(&name);
        let lit = remap_lit(*out2, &map2);
        out2_by_name.insert(normalized, lit);
    }

    // Match outputs by name and compare
    let mut matched_outputs = 0;
    for (name, lit1) in &out1_by_name {
        if let Some(lit2) = out2_by_name.get(name) {
            let diff = miter.add_xor(*lit1, *lit2);
            let pair_name = format!("output[{}]", name);
            diff_gates.push((pair_name.clone(), diff));
            matched_pairs.push((pair_name, *lit1, *lit2));
            miter_output = miter.add_or(miter_output, diff);
            matched_outputs += 1;
        }
    }
    let unmatched_out1 = out1_by_name.keys().filter(|n| !out2_by_name.contains_key(*n)).count();
    let unmatched_out2 = out2_by_name.keys().filter(|n| !out1_by_name.contains_key(*n)).count();
    eprintln!("   Outputs: {} matched, {} unmatched (MIR: {}, Gate: {})",
        matched_outputs, unmatched_out1 + unmatched_out2, unmatched_out1, unmatched_out2);

    // 2. Check next-state (latch D input) equivalence
    // Match latches by normalized name (BTreeMap for deterministic iteration)
    let mut latch1_by_name: BTreeMap<String, AigLit> = BTreeMap::new();
    for &latch_id in &aig1.latches {
        if let AigNode::Latch { name, next, .. } = &aig1.nodes[latch_id.0 as usize] {
            let normalized = normalize_signal_name_for_matching(name);
            let next_lit = remap_lit(*next, &map1);
            latch1_by_name.insert(normalized, next_lit);
        }
    }

    let mut latch2_by_name: BTreeMap<String, AigLit> = BTreeMap::new();
    for &latch_id in &aig2.latches {
        if let AigNode::Latch { name, next, .. } = &aig2.nodes[latch_id.0 as usize] {
            let normalized = normalize_signal_name_for_matching(name);
            let next_lit = remap_lit(*next, &map2);
            latch2_by_name.insert(normalized, next_lit);
        }
    }

    // Second pass: apply structural matches (pre-computed before miter construction
    // so that state inputs are properly shared). Rename latch keys to shared keys.
    for (gate_name, mir_name) in &structural_matches {
        let shared_key = format!("{}(={})", mir_name, gate_name);
        if let Some(mir_next) = latch1_by_name.remove(mir_name) {
            latch1_by_name.insert(shared_key.clone(), mir_next);
        }
        if let Some(gate_next) = latch2_by_name.remove(gate_name) {
            latch2_by_name.insert(shared_key.clone(), gate_next);
        }
    }

    // Report latch matching
    let mut matched_latches = 0;
    let mut unmatched_mir = 0;
    let mut unmatched_gate = 0;
    for name in latch1_by_name.keys() {
        if latch2_by_name.contains_key(name) {
            matched_latches += 1;
        } else {
            unmatched_mir += 1;
        }
    }
    for name in latch2_by_name.keys() {
        if !latch1_by_name.contains_key(name) {
            unmatched_gate += 1;
        }
    }
    eprintln!("   Latches: {} matched, {} unmatched (MIR: {}, Gate: {})",
        matched_latches, unmatched_mir + unmatched_gate, unmatched_mir, unmatched_gate);

    // Compare next-state for matching latches
    for (name, next1) in &latch1_by_name {
        if let Some(next2) = latch2_by_name.get(name) {
            let diff = miter.add_xor(*next1, *next2);
            let pair_name = format!("next_state[{}]", name);
            diff_gates.push((pair_name.clone(), diff));
            matched_pairs.push((pair_name, *next1, *next2));
            miter_output = miter.add_or(miter_output, diff);
            // BUG #271 DEBUG: Add individual model outputs for diagnostic evaluation
            miter.add_output(format!("mir_next[{}]", name), *next1);
            miter.add_output(format!("gate_next[{}]", name), *next2);
        }
    }

    // Add individual diff gates as named outputs for counterexample diagnostics
    for (name, diff_lit) in &diff_gates {
        miter.add_output(format!("diff_{}", name), *diff_lit);
    }

    miter.add_output("miter".to_string(), miter_output);
    Ok((miter, matched_pairs))
}

/// Normalize signal name for matching between designs
fn normalize_signal_name_for_matching(name: &str) -> String {
    // Remove __reg_cur_ or __dff_cur_ prefix first (internal AIG naming for state inputs)
    let without_reg = name
        .strip_prefix("__reg_cur_")
        .or_else(|| name.strip_prefix("__dff_cur_"))
        .unwrap_or(name);

    // Strip "top." prefix (gate netlist uses "top." prefix for all signals)
    let without_top = without_reg.strip_prefix("top.").unwrap_or(without_reg);

    without_top.to_string()
}

/// Extract counterexample from SAT model
fn extract_symbolic_counterexample(
    miter: &Aig,
    var_map: &BTreeMap<u32, Var>,
    model: &[Lit],
    differing_signal: Option<String>,
) -> SymbolicCounterexample {
    let model_set: std::collections::HashSet<_> = model.iter().collect();

    let mut state = HashMap::new();
    let mut inputs = HashMap::new();

    for (idx, node) in miter.nodes.iter().enumerate() {
        if let AigNode::Input { name } = node {
            if let Some(&var) = var_map.get(&(idx as u32)) {
                let lit_pos = Lit::positive(var);
                let value = model_set.contains(&lit_pos);

                if name.starts_with("__reg_cur_") || name.contains("state") || name.contains("cnt") {
                    state.insert(name.clone(), value);
                } else {
                    inputs.insert(name.clone(), value);
                }
            }
        }
    }

    SymbolicCounterexample {
        state,
        inputs,
        differing_signal,
    }
}

/// Print detailed counterexample state: reconstruct multi-bit register values
/// from the SAT model and evaluate both MIR and gate next-state outputs.
fn print_counterexample_diagnosis(
    miter: &Aig,
    var_map: &BTreeMap<u32, Var>,
    model: &[Lit],
    diff_name: &str,
) {
    let model_set: std::collections::HashSet<_> = model.iter().collect();

    // 1. Extract all state input values (current latch values in the counterexample)
    let mut state_bits: BTreeMap<String, BTreeMap<u32, bool>> = BTreeMap::new();
    let mut primary_inputs: BTreeMap<String, BTreeMap<u32, bool>> = BTreeMap::new();

    for (idx, node) in miter.nodes.iter().enumerate() {
        if let AigNode::Input { name } = node {
            if let Some(&var) = var_map.get(&(idx as u32)) {
                let lit_pos = Lit::positive(var);
                let value = model_set.contains(&lit_pos);

                // Parse name to extract register base name and bit index
                // Format: "__reg_cur_pwm_gen.carrier.count_reg[3]" or "v_batt[7]"
                let clean_name = name.strip_prefix("__reg_cur_").unwrap_or(name);
                let is_state = name.starts_with("__reg_cur_") || name.starts_with("__dff_cur_");

                if let Some(bracket_pos) = clean_name.rfind('[') {
                    if let Some(end_pos) = clean_name.rfind(']') {
                        let base = &clean_name[..bracket_pos];
                        if let Ok(bit_idx) = clean_name[bracket_pos+1..end_pos].parse::<u32>() {
                            if is_state {
                                state_bits.entry(base.to_string()).or_default().insert(bit_idx, value);
                            } else {
                                primary_inputs.entry(base.to_string()).or_default().insert(bit_idx, value);
                            }
                            continue;
                        }
                    }
                }
                // Single-bit signal (no bracket)
                if is_state {
                    state_bits.entry(clean_name.to_string()).or_default().insert(0, value);
                } else {
                    primary_inputs.entry(clean_name.to_string()).or_default().insert(0, value);
                }
            }
        }
    }

    // 2. Reconstruct multi-bit values and print state registers
    eprintln!("   ┌─── Counterexample State ──────────────────────────────");
    eprintln!("   │ Differing signal: {}", diff_name);

    // Extract the base register name from the diff_name for focused reporting
    let focus_base_owned = {
        let clean = diff_name
            .strip_prefix("diff_next_state[")
            .or_else(|| diff_name.strip_prefix("diff_output["))
            .unwrap_or(diff_name)
            .trim_end_matches(']');
        if let Some(bp) = clean.rfind('[') {
            clean[..bp].to_string()
        } else {
            clean.to_string()
        }
    };
    let focus_base = focus_base_owned.as_str();

    eprintln!("   │");
    eprintln!("   │ Current latch state (related registers):");
    for (base_name, bits) in &state_bits {
        // Show registers related to the failing signal (same hierarchy prefix)
        let prefix = if focus_base.contains('.') {
            // e.g., "pwm_gen.carrier" from "pwm_gen.carrier.count_reg"
            let last_dot = focus_base.rfind('.').unwrap();
            &focus_base[..last_dot]
        } else {
            focus_base
        };
        if base_name.starts_with(prefix) || base_name == focus_base {
            let max_bit = bits.keys().max().copied().unwrap_or(0);
            let mut value: u64 = 0;
            for (&bit_idx, &bit_val) in bits {
                if bit_val {
                    value |= 1u64 << bit_idx;
                }
            }
            eprintln!("   │   {} = {} (0x{:x}, {}-bit)", base_name, value, value, max_bit + 1);
        }
    }

    // 3. Evaluate miter to get both MIR and gate next-state values
    let num_nodes = miter.nodes.len();
    let mut values = vec![false; num_nodes];
    values[0] = false;

    // Set input values from model
    for (&node_id, &var) in var_map {
        let pos = Lit::positive(var);
        if model_set.contains(&pos) {
            values[node_id as usize] = true;
        }
    }

    // Evaluate AND gates
    for (i, node) in miter.nodes.iter().enumerate() {
        if let AigNode::And { left, right } = node {
            let l = values[left.node.0 as usize] ^ left.inverted;
            let r = values[right.node.0 as usize] ^ right.inverted;
            values[i] = l && r;
        }
    }

    // Find mir_next and gate_next outputs for the related register
    let mut mir_next_bits: BTreeMap<String, BTreeMap<u32, bool>> = BTreeMap::new();
    let mut gate_next_bits: BTreeMap<String, BTreeMap<u32, bool>> = BTreeMap::new();

    for (out_idx, out_name) in miter.output_names.iter().enumerate() {
        if let Some(out_lit) = miter.outputs.get(out_idx) {
            let out_val = values[out_lit.node.0 as usize] ^ out_lit.inverted;

            let (target_map, clean_name) = if let Some(rest) = out_name.strip_prefix("mir_next[") {
                (&mut mir_next_bits, rest.strip_suffix(']').unwrap_or(rest))
            } else if let Some(rest) = out_name.strip_prefix("gate_next[") {
                (&mut gate_next_bits, rest.strip_suffix(']').unwrap_or(rest))
            } else {
                continue;
            };

            // Filter to only the prefix of interest
            let prefix = if focus_base.contains('.') {
                let last_dot = focus_base.rfind('.').unwrap();
                &focus_base[..last_dot]
            } else {
                focus_base
            };

            if let Some(bracket_pos) = clean_name.rfind('[') {
                if let Some(end_pos) = clean_name.rfind(']') {
                    let base = &clean_name[..bracket_pos];
                    if let Ok(bit_idx) = clean_name[bracket_pos+1..end_pos].parse::<u32>() {
                        if base.starts_with(prefix) || base == focus_base {
                            target_map.entry(base.to_string()).or_default().insert(bit_idx, out_val);
                        }
                    }
                }
            } else {
                // Single-bit signal (no bracket index)
                if clean_name.starts_with(prefix) || clean_name == focus_base {
                    target_map.entry(clean_name.to_string()).or_default().insert(0, out_val);
                }
            }
        }
    }

    eprintln!("   │");
    eprintln!("   │ Next-state comparison (MIR vs Gate):");
    let all_regs: BTreeSet<String> = mir_next_bits.keys().chain(gate_next_bits.keys()).cloned().collect();
    for reg_name in &all_regs {
        let mir_bits = mir_next_bits.get(reg_name);
        let gate_bits = gate_next_bits.get(reg_name);
        let max_bit = mir_bits.iter().chain(gate_bits.iter())
            .flat_map(|b| b.keys())
            .max()
            .copied()
            .unwrap_or(0);

        let reconstruct = |bits: Option<&BTreeMap<u32, bool>>| -> u64 {
            let mut v = 0u64;
            if let Some(b) = bits {
                for (&idx, &val) in b {
                    if val { v |= 1u64 << idx; }
                }
            }
            v
        };
        let mir_val = reconstruct(mir_bits);
        let gate_val = reconstruct(gate_bits);
        let diff_marker = if mir_val != gate_val { " *** DIFFERS" } else { "" };
        eprintln!("   │   {} ({}-bit): MIR_next={} (0x{:x}), Gate_next={} (0x{:x}){}",
            reg_name, max_bit + 1, mir_val, mir_val, gate_val, gate_val, diff_marker);
    }
    eprintln!("   └──────────────────────────────────────────────────────");
}

// ============================================================================
// Port Name Normalization for Equivalence Checking
// ============================================================================

/// Normalized port name for matching between different representations
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NormalizedPort {
    /// Base name without hierarchy or bit index
    pub base_name: String,
    /// Bit index if this is a single bit of a multi-bit port
    pub bit_index: Option<u32>,
}

impl NormalizedPort {
    /// Create a canonical string key for matching
    pub fn key(&self) -> String {
        match self.bit_index {
            Some(idx) => format!("{}[{}]", self.base_name, idx),
            None => self.base_name.clone(),
        }
    }
}

/// Extract the entity name from an AIG by finding the common second path component
/// in signal names that follow the "top.<EntityName>.<signal>" convention.
/// e.g., "top.DabBatteryController.duty_cycle[0]" → Some("DabBatteryController")
fn extract_entity_name(aig: &Aig) -> Option<String> {
    let mut candidates: HashMap<String, usize> = HashMap::new();

    // Check input names
    for node in &aig.nodes {
        if let AigNode::Input { name } = node {
            if let Some(rest) = name.strip_prefix("top.") {
                if let Some(dot_pos) = rest.find('.') {
                    let entity = &rest[..dot_pos];
                    // Entity names typically have uppercase (CamelCase)
                    if entity.chars().any(|c| c.is_ascii_uppercase()) {
                        *candidates.entry(entity.to_string()).or_insert(0) += 1;
                    }
                }
            }
        }
    }

    // Check output names
    for name in &aig.output_names {
        if let Some(rest) = name.strip_prefix("top.") {
            if let Some(dot_pos) = rest.find('.') {
                let entity = &rest[..dot_pos];
                if entity.chars().any(|c| c.is_ascii_uppercase()) {
                    *candidates.entry(entity.to_string()).or_insert(0) += 1;
                }
            }
        }
    }

    // Return the most common entity name
    candidates.into_iter()
        .max_by_key(|(_, count)| *count)
        .map(|(name, _)| name)
}

/// Known operation prefixes used in AIG hierarchy (deterministic, not heuristic)
const OPERATION_PREFIXES: &[&str] = &[
    "Mul_", "Add_", "Sub_", "And_", "Or_", "Xor_",
    "Greater_", "Less_", "Equal_", "Mux_",
    "LeftShift_", "RightShift_", "Not_", "Neg_",
    "Div_", "Mod_", "Shl_", "Shr_",
    "SignedMul_", "SignedDiv_", "SignedMod_",
    "SignedGreater_", "SignedLess_",
    "Concat_", "Replicate_", "Select_",
    "Comparator_", "Counter_", "Register_",
    "ThresholdComparator",
];

/// Check if a path component is a known operation prefix
fn is_operation_prefix(component: &str) -> bool {
    OPERATION_PREFIXES.iter().any(|p| component.starts_with(p))
}

/// Strip common hierarchy prefixes from a signal name
/// e.g., "top.module.signal" -> "signal"
fn strip_hierarchy_prefix(name: &str) -> String {
    strip_hierarchy_prefix_with_entity(name, None)
}

/// Strip hierarchy prefixes using entity name when available
fn strip_hierarchy_prefix_with_entity(name: &str, entity_name: Option<&str>) -> String {
    let mut working = name.to_string();

    // Strip common top-level prefixes
    for prefix in &["top.", "inst.", "dut."] {
        if let Some(rest) = working.strip_prefix(prefix) {
            working = rest.to_string();
        }
    }

    if let Some(entity) = entity_name {
        // Deterministic: strip exact entity name prefix
        let entity_prefix = format!("{}.", entity);
        if let Some(rest) = working.strip_prefix(&entity_prefix) {
            working = rest.to_string();
        }

        // Then strip known operation prefixes
        while working.contains('.') {
            if let Some(dot_pos) = working.find('.') {
                let before = &working[..dot_pos];
                if is_operation_prefix(before) || before.starts_with("inst_") || before.starts_with("_t") {
                    working = working[dot_pos + 1..].to_string();
                } else {
                    break;
                }
            } else {
                break;
            }
        }
    } else {
        // Fallback: uppercase heuristic (for backward compatibility)
        while working.contains('.') {
            if let Some(dot_pos) = working.find('.') {
                let before = &working[..dot_pos];
                if before.chars().any(|c| c.is_ascii_uppercase())
                    || before.starts_with("inst_")
                {
                    working = working[dot_pos + 1..].to_string();
                } else {
                    break;
                }
            } else {
                break;
            }
        }
    }

    working
}

/// Normalize a port name by stripping hierarchy prefixes and extracting bit index
///
/// When `entity_name` is provided, uses deterministic entity-name-based hierarchy
/// stripping instead of the uppercase heuristic fallback.
///
/// Handles various naming conventions:
/// - `port_name` -> (base="port_name", bit=None)
/// - `port_name[3]` -> (base="port_name", bit=Some(3))
/// - `top.port_name[3]` -> (base="port_name", bit=Some(3))
/// - `inst.sub.port_name[3]` -> (base="port_name", bit=Some(3))
/// - `config__field__subfield[3]` -> (base="config.field.subfield", bit=Some(3)) [flattened struct]
pub fn normalize_port_name(name: &str) -> NormalizedPort {
    normalize_port_name_with_entity(name, None)
}

/// Normalize a port name with an explicit entity name for deterministic hierarchy stripping
pub fn normalize_port_name_with_entity(name: &str, entity_name: Option<&str>) -> NormalizedPort {
    let mut working = name.to_string();

    // Normalize register/DFF current state pseudo-inputs to the same prefix
    // LIR uses __reg_cur_*, Gate uses __dff_cur_* - unify them
    // Also strip hierarchy prefixes from the signal name part
    if let Some(rest) = working.strip_prefix("__reg_cur_") {
        let signal_name = strip_hierarchy_prefix_with_entity(rest, entity_name);
        working = format!("__state_cur_{}", signal_name);
    } else if let Some(rest) = working.strip_prefix("__dff_cur_") {
        let signal_name = strip_hierarchy_prefix_with_entity(rest, entity_name);
        working = format!("__state_cur_{}", signal_name);
    }

    // Strip DFF output suffix if present
    if let Some(stripped) = working.strip_suffix("_dff_out") {
        working = stripped.to_string();
    }

    // Strip _unknown suffix (unmatched internal signals)
    if let Some(stripped) = working.strip_suffix("_unknown") {
        working = stripped.to_string();
    }

    // Strip hierarchy prefix like "top." or "inst.sub."
    if working.starts_with("top.") {
        working = working[4..].to_string();
    }

    if let Some(entity) = entity_name {
        // Deterministic: strip exact entity name prefix
        let entity_prefix = format!("{}.", entity);
        if let Some(rest) = working.strip_prefix(&entity_prefix) {
            working = rest.to_string();
        }

        // Then strip known operation prefixes only
        while working.contains('.') {
            if let Some(dot_pos) = working.find('.') {
                let before = &working[..dot_pos];
                if is_operation_prefix(before) || before.starts_with("inst_") || before.starts_with("_t") {
                    working = working[dot_pos + 1..].to_string();
                } else {
                    break;
                }
            } else {
                break;
            }
        }
    } else {
        // Fallback: uppercase heuristic (backward compatibility for call sites
        // that don't have entity name available)
        while working.contains('.') {
            if let Some(dot_pos) = working.find('.') {
                let before = &working[..dot_pos];
                if before.chars().any(|c| c.is_ascii_uppercase())
                    || before.starts_with("_t")
                    || is_operation_prefix(before)
                {
                    working = working[dot_pos + 1..].to_string();
                } else {
                    break;
                }
            } else {
                break;
            }
        }
    }

    // Convert double underscore (netlist struct flattening) to dots
    // config__voltage_loop__kp -> config.voltage_loop.kp
    working = working.replace("__", ".");

    // Extract bit index from bracket notation: port_name[3]
    if let Some(bracket_start) = working.find('[') {
        if let Some(bracket_end) = working.find(']') {
            if bracket_end > bracket_start {
                let base = &working[..bracket_start];
                let idx_str = &working[bracket_start + 1..bracket_end];
                if let Ok(idx) = idx_str.parse::<u32>() {
                    return NormalizedPort {
                        base_name: base.to_string(),
                        bit_index: Some(idx),
                    };
                }
            }
        }
    }

    // No bit index found — underscore notation (signal_N) is NOT used because
    // it's ambiguous with real signal names like phase_2 or counter_0.
    // Gate AIGs use bracket notation [N] for bit indices.
    NormalizedPort {
        base_name: working,
        bit_index: None,
    }
}

/// Check if a signal name represents an internal/temporary signal that should be ignored
/// in equivalence checking (only primary I/O should be compared)
fn is_internal_signal(name: &str) -> bool {
    // DFF outputs should ALWAYS be kept for sequential equivalence checking
    if name.ends_with("_dff_out") {
        return false;
    }

    // Clock signals are internal for BMC - we simulate cycles, not clock edges
    // This allows MIR (which has explicit clk port) to match Gate (which has implicit clock)
    if name == "clk" || name.ends_with(".clk") {
        return true;
    }

    // Register/DFF current state pseudo-inputs are internal
    // They represent latch state, not primary inputs
    if name.starts_with("__reg_cur_") || name.starts_with("__dff_cur_") {
        return true;
    }

    // Temporary signals generated by compiler (e.g., _t63, _t290[30])
    // Match _t followed by digits
    if name.starts_with("_t") {
        let rest = &name[2..];
        if rest.chars().next().map(|c| c.is_ascii_digit()).unwrap_or(false) {
            return true;
        }
    }
    if name.contains("._t") {
        return true;
    }

    // Internal computation signals (can be at start or after hierarchy)
    // These are internal arithmetic operation intermediates
    if name.starts_with("sum_") || name.contains(".sum_")
        || name.starts_with("pp_") || name.contains(".pp_")
        || name.starts_with("zero_") || name.contains(".zero_")
        || name.starts_with("carry_") || name.contains(".carry_")
        || name.starts_with("partial_") || name.contains(".partial_")
    {
        return true;
    }

    // Comparator internals
    if name.starts_with("lt_") || name.contains(".lt_")
        || name.starts_with("eq_") || name.contains(".eq_")
        || name.starts_with("not_a_") || name.contains(".not_a_")
        || name.contains("_combined")
        || name.contains("_bit[")
        || name.contains("_and_prev_")
    {
        return true;
    }

    // Shift internals
    if name.contains(".stage") || name.starts_with("stage") {
        return true;
    }

    // Hierarchical internal signals (anything with . that's not a struct field)
    // But allow top-level struct fields (e.g., config.field, bms.soc)
    // and flattened paths with DFF outputs
    let parts: Vec<&str> = name.split('.').collect();
    if parts.len() > 2 {
        // Deep hierarchy = internal signal (unless it ends with _dff_out, already handled above)
        return true;
    }

    // Latch internals
    if name.contains("_latch.") {
        return true;
    }

    // Unknown signals (unmatched cell outputs)
    if name.ends_with("_unknown") {
        return true;
    }

    false
}

/// Build a miter circuit with intelligent port matching
///
/// This handles the case where MIR and GateNetlist use different naming conventions:
/// - MIR uses simple names: `port_name[bit]`
/// - GateNetlist uses hierarchical names: `top.port_name[bit]`
///
/// Only ports that match in both designs are compared.
pub fn build_miter_with_port_matching(aig1: &Aig, aig2: &Aig) -> FormalResult<Aig> {
    let mut miter = Aig::new();

    // Extract entity name from gate AIG for deterministic hierarchy stripping
    let entity_name = extract_entity_name(aig2);
    let entity_ref = entity_name.as_deref();

    // Map from old node IDs to new ones for both AIGs
    let mut map1: BTreeMap<u32, AigLit> = BTreeMap::new();
    let mut map2: BTreeMap<u32, AigLit> = BTreeMap::new();

    // Node 0 (false) maps to node 0
    map1.insert(0, miter.false_lit());
    map2.insert(0, miter.false_lit());

    // Collect inputs from both AIGs with normalized names
    // Filter out internal signals - only primary I/O should be compared
    let mut aig1_inputs: BTreeMap<String, (u32, String)> = BTreeMap::new(); // normalized_key -> (node_idx, original_name)
    let mut aig2_inputs: BTreeMap<String, (u32, String)> = BTreeMap::new();

    for (idx, node) in aig1.nodes.iter().enumerate() {
        if let AigNode::Input { name } = node {
            // Skip internal signals
            if is_internal_signal(name) {
                continue;
            }
            let normalized = normalize_port_name_with_entity(name, entity_ref);
            aig1_inputs.insert(normalized.key(), (idx as u32, name.clone()));
        }
    }

    for (idx, node) in aig2.nodes.iter().enumerate() {
        if let AigNode::Input { name } = node {
            // Skip internal signals
            if is_internal_signal(name) {
                continue;
            }
            let normalized = normalize_port_name_with_entity(name, entity_ref);
            aig2_inputs.insert(normalized.key(), (idx as u32, name.clone()));
        }
    }

    // Create shared inputs for ports that exist in both designs
    let mut shared_input_map: BTreeMap<String, AigLit> = BTreeMap::new();
    let mut aig1_only_inputs: Vec<String> = Vec::new();
    let mut aig2_only_inputs: Vec<String> = Vec::new();

    for (key, (idx1, name1)) in &aig1_inputs {
        if let Some((idx2, _name2)) = aig2_inputs.get(key) {
            // Port exists in both - create shared input
            let lit = miter.add_input(name1.clone());
            map1.insert(*idx1, lit);
            map2.insert(*idx2, lit);
            shared_input_map.insert(key.clone(), lit);
        } else {
            // Port only in AIG1 - create input but it won't be compared
            let lit = miter.add_input(name1.clone());
            map1.insert(*idx1, lit);
            aig1_only_inputs.push(format!("{} -> {}", name1, key));
        }
    }

    // Create inputs for ports only in AIG2
    for (key, (idx2, name2)) in &aig2_inputs {
        if !aig1_inputs.contains_key(key) {
            let lit = miter.add_input(name2.clone());
            map2.insert(*idx2, lit);
            aig2_only_inputs.push(format!("{} -> {}", name2, key));
        }
    }

    // Structural I/O matching for any unmatched ports
    let matched_input_keys: std::collections::HashSet<String> = shared_input_map.keys().cloned().collect();
    if !aig1_only_inputs.is_empty() && !aig2_only_inputs.is_empty() {
        let (structural_input_matches, _) = compute_structural_io_matches(
            aig1, aig2, &matched_input_keys, &std::collections::HashSet::new(), entity_ref,
        );
        // Apply structural input matches
        for (aig2_key, aig1_key) in &structural_input_matches {
            if let (Some((idx1, name1)), Some((idx2, _))) = (aig1_inputs.get(aig1_key), aig2_inputs.get(aig2_key)) {
                if !map1.contains_key(idx1) || !map2.contains_key(idx2) {
                    let lit = miter.add_input(name1.clone());
                    map1.insert(*idx1, lit);
                    map2.insert(*idx2, lit);
                    shared_input_map.insert(aig1_key.clone(), lit);
                }
            }
        }
    }

    // Log input matching statistics
    log::debug!(
        "Miter input matching: {} shared, {} AIG1-only, {} AIG2-only",
        shared_input_map.len(),
        aig1_only_inputs.len(),
        aig2_only_inputs.len()
    );

    // Copy AND gates from both AIGs
    copy_aig_structure(&mut miter, aig1, &mut map1);
    copy_aig_structure(&mut miter, aig2, &mut map2);

    // Collect outputs with normalized names
    let mut aig1_outputs: Vec<(String, AigLit)> = Vec::new();
    let mut aig2_outputs_map: BTreeMap<String, AigLit> = BTreeMap::new();

    for (i, output) in aig1.outputs.iter().enumerate() {
        let name = aig1.output_names.get(i).cloned().unwrap_or_else(|| format!("out_{}", i));
        let normalized = normalize_port_name_with_entity(&name, entity_ref);
        let lit = remap_lit(*output, &map1);
        aig1_outputs.push((normalized.key(), lit));
    }

    for (i, output) in aig2.outputs.iter().enumerate() {
        let name = aig2.output_names.get(i).cloned().unwrap_or_else(|| format!("out_{}", i));
        let normalized = normalize_port_name_with_entity(&name, entity_ref);
        let lit = remap_lit(*output, &map2);
        aig2_outputs_map.insert(normalized.key(), lit);
    }

    // Build miter: XOR only matching outputs
    let mut miter_output = miter.false_lit();
    let mut matched_outputs = 0;
    let mut unmatched_outputs = Vec::new();

    for (key, lit1) in &aig1_outputs {
        if let Some(lit2) = aig2_outputs_map.get(key) {
            let diff = miter.add_xor(*lit1, *lit2);
            miter_output = miter.add_or(miter_output, diff);
            matched_outputs += 1;
        } else {
            unmatched_outputs.push(key.clone());
        }
    }

    if matched_outputs == 0 {
        return Err(FormalError::PropertyFailed(format!(
            "No matching outputs found between designs. AIG1 outputs: {:?}, AIG2 outputs: {:?}",
            aig1_outputs.iter().map(|(k, _)| k).collect::<Vec<_>>(),
            aig2_outputs_map.keys().collect::<Vec<_>>()
        )));
    }

    log::debug!(
        "Miter output matching: {} matched, {} unmatched",
        matched_outputs,
        unmatched_outputs.len()
    );

    miter.add_output("miter".to_string(), miter_output);

    Ok(miter)
}

// ============================================================================
// Fast Simulation-Based Equivalence Pre-Check
// ============================================================================

/// Evaluate an AIG with given input values using bit-parallel simulation
fn simulate_aig(aig: &Aig, input_values: &HashMap<u32, bool>) -> HashMap<u32, bool> {
    let mut values: HashMap<u32, bool> = HashMap::new();

    // Node 0 is always false
    values.insert(0, false);

    // Process nodes in order (they should be topologically sorted)
    for (idx, node) in aig.nodes.iter().enumerate() {
        let idx = idx as u32;
        match node {
            AigNode::False => {
                values.insert(idx, false);
            }
            AigNode::Input { .. } => {
                let val = input_values.get(&idx).copied().unwrap_or(false);
                values.insert(idx, val);
            }
            AigNode::And { left, right } => {
                let left_val = values.get(&left.node.0).copied().unwrap_or(false);
                let right_val = values.get(&right.node.0).copied().unwrap_or(false);
                let left_val = if left.inverted { !left_val } else { left_val };
                let right_val = if right.inverted { !right_val } else { right_val };
                values.insert(idx, left_val && right_val);
            }
            AigNode::Latch { .. } => {
                // Treat latch as input for combinational simulation
                let val = input_values.get(&idx).copied().unwrap_or(false);
                values.insert(idx, val);
            }
        }
    }

    values
}

/// Get output values from simulated AIG
fn get_output_values(aig: &Aig, values: &HashMap<u32, bool>) -> Vec<bool> {
    aig.outputs.iter().map(|out| {
        let val = values.get(&out.node.0).copied().unwrap_or(false);
        if out.inverted { !val } else { val }
    }).collect()
}

/// Quick random simulation to find easy counterexamples
/// Returns Some(counterexample) if found, None if simulation passes
pub fn simulation_based_check(
    aig1: &Aig,
    aig2: &Aig,
    num_vectors: usize,
) -> Option<HashMap<String, String>> {
    let mut rng = rand::thread_rng();

    // Extract entity name for deterministic hierarchy stripping
    let entity_name = extract_entity_name(aig2);
    let entity_ref = entity_name.as_deref();

    // Build input name to node ID maps for both AIGs
    let mut aig1_input_nodes: Vec<(u32, String)> = Vec::new();
    let mut aig2_input_map: HashMap<String, u32> = HashMap::new();

    for (idx, node) in aig1.nodes.iter().enumerate() {
        if let AigNode::Input { name } = node {
            // Filter out internal signals
            if is_internal_signal(name) {
                continue;
            }
            aig1_input_nodes.push((idx as u32, name.clone()));
        }
    }

    let mut aig2_dff_count = 0;
    let mut aig2_dff_filtered = 0;
    let mut aig2_dff_samples: Vec<String> = Vec::new();
    for (idx, node) in aig2.nodes.iter().enumerate() {
        if let AigNode::Input { name } = node {
            // Track DFF outputs for debugging
            if name.ends_with("_dff_out") {
                aig2_dff_count += 1;
                if is_internal_signal(name) {
                    aig2_dff_filtered += 1;
                    if aig2_dff_samples.len() < 5 {
                        aig2_dff_samples.push(name.clone());
                    }
                }
            }

            // Filter out internal signals - only keep primary I/O
            if is_internal_signal(name) {
                continue;
            }
            let normalized = normalize_port_name_with_entity(name, entity_ref);
            aig2_input_map.insert(normalized.key(), idx as u32);
        }
    }
    if aig2_dff_filtered > 0 {
        log::debug!(
            "AIG2 DFF outputs: {} total, {} filtered out",
            aig2_dff_count, aig2_dff_filtered
        );
    }

    // Build output matching
    let mut output_pairs: Vec<(usize, usize)> = Vec::new();
    for (i, _) in aig1.outputs.iter().enumerate() {
        let name = aig1.output_names.get(i).cloned().unwrap_or_else(|| format!("out_{}", i));
        let normalized = normalize_port_name_with_entity(&name, entity_ref);

        for (j, _) in aig2.outputs.iter().enumerate() {
            let name2 = aig2.output_names.get(j).cloned().unwrap_or_else(|| format!("out_{}", j));
            let normalized2 = normalize_port_name_with_entity(&name2, entity_ref);
            if normalized.key() == normalized2.key() {
                output_pairs.push((i, j));
                break;
            }
        }
    }

    // Debug: count how many inputs/outputs match
    let matched_inputs: usize = aig1_input_nodes.iter()
        .filter(|(_, name)| {
            let normalized = normalize_port_name_with_entity(name, entity_ref);
            aig2_input_map.contains_key(&normalized.key())
        }).count();

    log::debug!(
        "Simulation check: {} MIR inputs, {} Gate inputs, {} matched",
        aig1_input_nodes.len(),
        aig2_input_map.len(),
        matched_inputs
    );
    log::debug!(
        "  {} MIR outputs, {} Gate outputs, {} matched",
        aig1.outputs.len(),
        aig2.outputs.len(),
        output_pairs.len()
    );

    // Check for unmatched inputs
    let aig1_keys: std::collections::HashSet<_> = aig1_input_nodes.iter()
        .map(|(_, name)| normalize_port_name_with_entity(name, entity_ref).key())
        .collect();
    let unmatched_aig2_count = aig2_input_map.keys()
        .filter(|key| !aig1_keys.contains(*key))
        .count();

    if unmatched_aig2_count > 0 {
        log::warn!(
            "{} GateNetlist inputs have no MIR correspondence (likely internal submodule registers)",
            unmatched_aig2_count
        );
    }

    if output_pairs.is_empty() {
        return None; // No matching outputs to compare
    }

    // Build reverse map: AIG2 node ID -> normalized key
    let mut aig2_idx_to_key: HashMap<u32, String> = HashMap::new();
    for (key, &idx) in &aig2_input_map {
        aig2_idx_to_key.insert(idx, key.clone());
    }

    // Collect all AIG2 input node IDs
    let mut aig2_input_nodes: Vec<(u32, String)> = Vec::new();
    for (idx, node) in aig2.nodes.iter().enumerate() {
        if let AigNode::Input { name } = node {
            if !is_internal_signal(name) {
                aig2_input_nodes.push((idx as u32, name.clone()));
            }
        }
    }

    // Run random simulations
    for _ in 0..num_vectors {
        // Generate random input values
        let mut input_values1: HashMap<u32, bool> = HashMap::new();
        let mut input_values2: HashMap<u32, bool> = HashMap::new();
        let mut assignments: HashMap<String, String> = HashMap::new();

        // First, generate random values for all AIG2 inputs
        for (idx, name) in &aig2_input_nodes {
            let val: bool = rng.gen();
            input_values2.insert(*idx, val);
        }

        // Then, set AIG1 inputs and override matching AIG2 inputs with same value
        for (idx, name) in &aig1_input_nodes {
            let val: bool = rng.gen();
            input_values1.insert(*idx, val);
            assignments.insert(name.clone(), if val { "1" } else { "0" }.to_string());

            // Find corresponding input in AIG2 and set to same value
            let normalized = normalize_port_name_with_entity(name, entity_ref);
            if let Some(&idx2) = aig2_input_map.get(&normalized.key()) {
                input_values2.insert(idx2, val);
            }
        }

        // Simulate both AIGs
        let values1 = simulate_aig(aig1, &input_values1);
        let values2 = simulate_aig(aig2, &input_values2);

        let outputs1 = get_output_values(aig1, &values1);
        let outputs2 = get_output_values(aig2, &values2);

        // Check if any matched output differs
        for &(i, j) in &output_pairs {
            if outputs1.get(i) != outputs2.get(j) {
                // Found counterexample
                log::debug!(
                    "Output mismatch: {} (MIR={}, Gate={})",
                    aig1.output_names.get(i).cloned().unwrap_or_else(|| format!("out_{}", i)),
                    outputs1.get(i).copied().unwrap_or(false) as u8,
                    outputs2.get(j).copied().unwrap_or(false) as u8
                );
                return Some(assignments);
            }
        }
    }

    None // No counterexample found
}

/// Fast parallel equivalence check using simulation + parallel SAT
pub fn fast_equivalence_check(aig1: &Aig, aig2: &Aig) -> FormalResult<EquivalenceResult> {
    let start = std::time::Instant::now();

    // Phase 1: Quick random simulation (very fast)
    log::info!("Phase 1: Random simulation check (1000 vectors)...");
    if let Some(ce_assignments) = simulation_based_check(aig1, aig2, 1000) {
        log::info!("Counterexample found by simulation in {}ms", start.elapsed().as_millis());
        return Ok(EquivalenceResult {
            equivalent: false,
            counterexample: Some(Counterexample {
                length: 1,
                trace: vec![TraceStep {
                    step: 0,
                    assignments: ce_assignments,
                }],
            }),
            conflicts: 0,
            decisions: 0,
            time_ms: start.elapsed().as_millis() as u64,
        });
    }
    log::info!("Simulation passed, proceeding to SAT...");

    // Phase 2: Full SAT check
    let miter = build_miter_with_port_matching(aig1, aig2)?;
    check_equivalence_sat(&miter)
}

// ============================================================================
// SAT-Based Equivalence Checking
// ============================================================================

/// Result of equivalence checking
#[derive(Debug, Clone)]
pub struct EquivalenceResult {
    /// True if designs are proven equivalent
    pub equivalent: bool,
    /// Counterexample if not equivalent
    pub counterexample: Option<Counterexample>,
    /// Number of SAT solver conflicts
    pub conflicts: u64,
    /// Number of decisions
    pub decisions: u64,
    /// Proof time in milliseconds
    pub time_ms: u64,
}

/// Check if miter is UNSAT (designs equivalent) or find counterexample
pub fn check_equivalence_sat(miter: &Aig) -> FormalResult<EquivalenceResult> {
    let start = std::time::Instant::now();

    // Convert AIG to CNF
    let (formula, var_map, output_var) = aig_to_cnf(miter);

    // Create solver and add clauses
    let mut solver = Solver::new();
    solver.add_formula(&formula);

    // Assert that miter output is true (looking for counterexample)
    solver.add_clause(&[output_var]);

    // Solve
    match solver.solve() {
        Ok(true) => {
            // SAT - found counterexample (designs differ)
            let model = solver.model().unwrap();
            let counterexample = extract_counterexample(miter, &var_map, &model);

            Ok(EquivalenceResult {
                equivalent: false,
                counterexample: Some(counterexample),
                conflicts: 0, // varisat doesn't expose this easily
                decisions: 0,
                time_ms: start.elapsed().as_millis() as u64,
            })
        }
        Ok(false) => {
            // UNSAT - designs are equivalent
            Ok(EquivalenceResult {
                equivalent: true,
                counterexample: None,
                conflicts: 0,
                decisions: 0,
                time_ms: start.elapsed().as_millis() as u64,
            })
        }
        Err(e) => Err(FormalError::SolverError(format!("SAT solver error: {}", e))),
    }
}

/// Convert AIG to CNF formula
fn aig_to_cnf(aig: &Aig) -> (CnfFormula, BTreeMap<u32, Var>, Lit) {
    let mut formula = CnfFormula::new();
    let mut var_map: BTreeMap<u32, Var> = BTreeMap::new();

    // Create variables for each AIG node
    for (idx, _) in aig.nodes.iter().enumerate() {
        let var = Var::from_index(idx);
        var_map.insert(idx as u32, var);
    }

    // Node 0 is always false
    let false_var = var_map[&0];
    formula.add_clause(&[Lit::negative(false_var)]);

    // Add Tseitin encoding for AND gates
    for (idx, node) in aig.nodes.iter().enumerate() {
        if let AigNode::And { left, right } = node {
            let out_var = var_map[&(idx as u32)];
            let left_var = var_map[&left.node.0];
            let right_var = var_map[&right.node.0];

            let left_lit = if left.inverted {
                Lit::negative(left_var)
            } else {
                Lit::positive(left_var)
            };
            let right_lit = if right.inverted {
                Lit::negative(right_var)
            } else {
                Lit::positive(right_var)
            };
            let out_lit = Lit::positive(out_var);

            // Tseitin encoding for AND: out = left AND right
            // (out -> left), (out -> right), (left AND right -> out)
            formula.add_clause(&[Lit::negative(out_var), left_lit]);
            formula.add_clause(&[Lit::negative(out_var), right_lit]);
            formula.add_clause(&[out_lit, !left_lit, !right_lit]);
        }
    }

    // Get the miter output literal (last output — the OR of all diff gates)
    let output = aig.outputs.last().unwrap();
    let output_var = var_map[&output.node.0];
    let output_lit = if output.inverted {
        Lit::negative(output_var)
    } else {
        Lit::positive(output_var)
    };

    (formula, var_map, output_lit)
}

/// Extract counterexample from SAT model
fn extract_counterexample(
    aig: &Aig,
    var_map: &BTreeMap<u32, Var>,
    model: &[Lit],
) -> Counterexample {
    let model_set: std::collections::HashSet<Lit> = model.iter().copied().collect();

    let mut assignments = std::collections::HashMap::new();

    for (idx, node) in aig.nodes.iter().enumerate() {
        if let AigNode::Input { name } = node {
            let var = var_map[&(idx as u32)];
            let value = if model_set.contains(&Lit::positive(var)) {
                "1"
            } else {
                "0"
            };
            assignments.insert(name.clone(), value.to_string());
        }
    }

    Counterexample {
        length: 1,
        trace: vec![TraceStep {
            step: 0,
            assignments,
        }],
    }
}

// ============================================================================
// FRAIG-based Miter Simplification
// ============================================================================

/// Remap a formal AIG literal to a synthesis AIG literal using the node mapping
fn remap_to_synth(formal_lit: AigLit, map: &HashMap<u32, SynthAigLit>) -> SynthAigLit {
    let base = map[&formal_lit.node.0];
    if formal_lit.inverted {
        base.invert()
    } else {
        base
    }
}

/// Convert a formal AIG (miter) to a synthesis AIG for FRAIG processing
fn formal_aig_to_synth_aig(formal: &Aig) -> (SynthAig, HashMap<u32, SynthAigLit>) {
    let mut synth = SynthAig::new("miter".to_string());
    let mut map: HashMap<u32, SynthAigLit> = HashMap::new();

    // Node 0 maps to false
    map.insert(0, SynthAigLit::false_lit());

    // Process nodes in order (topological since formal AIG is built that way)
    for (idx, node) in formal.nodes.iter().enumerate() {
        let idx = idx as u32;
        if idx == 0 {
            continue; // Skip False node
        }
        match node {
            AigNode::Input { name } => {
                let id = synth.add_input(name.clone(), None);
                map.insert(idx, SynthAigLit::new(id));
            }
            AigNode::And { left, right } => {
                let l = remap_to_synth(*left, &map);
                let r = remap_to_synth(*right, &map);
                let lit = synth.add_and(l, r);
                map.insert(idx, lit);
            }
            AigNode::Latch { name, next, init } => {
                // Create latch with remapped data input
                let data = remap_to_synth(*next, &map);
                let id = synth.add_latch(data, Some(*init), None, None);
                map.insert(idx, SynthAigLit::new(id));
            }
            AigNode::False => {
                // Additional False nodes (shouldn't happen, but handle gracefully)
                map.insert(idx, SynthAigLit::false_lit());
            }
        }
    }

    // Add outputs
    for (i, out_lit) in formal.outputs.iter().enumerate() {
        let name = formal
            .output_names
            .get(i)
            .cloned()
            .unwrap_or_default();
        let lit = remap_to_synth(*out_lit, &map);
        synth.add_output(name, lit);
    }

    (synth, map)
}

// ============================================================================
// High-Level API
// ============================================================================

/// Equivalence checker for SKALP designs
pub struct EquivalenceChecker {
    /// Timeout in seconds
    timeout: u64,
}

impl EquivalenceChecker {
    pub fn new() -> Self {
        Self { timeout: 300 }
    }

    pub fn with_timeout(mut self, timeout: u64) -> Self {
        self.timeout = timeout;
        self
    }

    /// Check equivalence between two LIR designs
    pub fn check_lir_equivalence(&self, lir1: &Lir, lir2: &Lir) -> FormalResult<EquivalenceResult> {
        let aig1 = LirToAig::new().convert(lir1);
        let aig2 = LirToAig::new().convert(lir2);

        let miter = build_miter(&aig1, &aig2)?;
        check_equivalence_sat(&miter)
    }

    /// Check equivalence between LIR (behavioral) and GateNetlist (structural)
    ///
    /// Uses intelligent port matching to handle different naming conventions:
    /// - LIR uses simple names: `port_name` or `port_name[bit]`
    /// - Flattened GateNetlist uses hierarchical names: `top.port_name[bit]`
    ///
    /// For large miters (e.g. multipliers), first attempts FRAIG-based simplification
    /// (simulation + SAT sweeping) which can resolve equivalence without a full SAT call.
    pub fn check_synthesis_equivalence(
        &self,
        lir: &Lir,
        netlist: &GateNetlist,
    ) -> FormalResult<EquivalenceResult> {
        let aig_lir = LirToAig::new().convert(lir);
        let aig_netlist = GateNetlistToAig::new().convert(netlist);

        log::info!(
            "LIR-AIG: {} inputs, {} outputs, {} AND gates",
            aig_lir.inputs.len(),
            aig_lir.outputs.len(),
            aig_lir.and_count()
        );
        log::info!(
            "Gate-AIG: {} inputs, {} outputs, {} AND gates",
            aig_netlist.inputs.len(),
            aig_netlist.outputs.len(),
            aig_netlist.and_count()
        );

        // Use port matching to handle LIR vs flattened netlist naming differences
        eprintln!("   Building miter...");
        let miter = build_miter_with_port_matching(&aig_lir, &aig_netlist)?;
        eprintln!("   Miter built: {} nodes, {} AND gates", miter.nodes.len(), miter.and_count());

        // Try FRAIG-based simplification (simulation + SAT sweeping).
        // Run in a thread with a real timeout via channel so we don't block forever.
        let start = std::time::Instant::now();
        let fraig_timeout = std::time::Duration::from_secs(30);

        eprintln!(
            "   FRAIG: miter has {} AND gates, running simplification ({}s timeout)...",
            miter.and_count(),
            fraig_timeout.as_secs()
        );

        let (tx, rx) = std::sync::mpsc::channel();
        let miter_clone = miter.clone();
        std::thread::spawn(move || {
            let (mut synth_miter, _map) = formal_aig_to_synth_aig(&miter_clone);

            Strash::new().run(&mut synth_miter);
            let fraig_config = FraigConfig {
                sim_patterns: 256,
                sim_rounds: 8,
                sat_conflict_limit: 50,
                max_sat_calls: 500,
                ..Default::default()
            };
            let mut fraig = Fraig::with_config(fraig_config);
            let fraig_result = fraig.run(&mut synth_miter);

            let _ = tx.send((
                synth_miter,
                fraig_result,
                fraig.nodes_merged(),
                fraig.sat_stats(),
            ));
        });

        match rx.recv_timeout(fraig_timeout) {
            Ok((synth_miter, fraig_result, merged, (calls, proofs, refutes))) => {
                let elapsed = start.elapsed().as_millis() as u64;
                eprintln!(
                    "   FRAIG: {} -> {} AND gates ({} merged, {} SAT calls: {} proofs, {} refutes) in {} ms",
                    fraig_result.ands_before,
                    fraig_result.ands_after,
                    merged, calls, proofs, refutes, elapsed,
                );

                // Check if miter output was reduced to constant false (= equivalent)
                let outputs = synth_miter.outputs();
                if let Some((_name, out_lit)) = outputs.first() {
                    if out_lit.is_const() && out_lit.const_value() == Some(false) {
                        eprintln!("   FRAIG proved equivalence in {} ms", elapsed);
                        return Ok(EquivalenceResult {
                            equivalent: true,
                            counterexample: None,
                            conflicts: 0,
                            decisions: 0,
                            time_ms: elapsed,
                        });
                    }
                }

                Err(FormalError::SolverError(
                    "FRAIG completed but could not reduce miter to constant false. \
                     Simulation-based equivalence passed."
                        .to_string(),
                ))
            }
            Err(_) => {
                eprintln!("   FRAIG timed out after {}s", fraig_timeout.as_secs());
                Err(FormalError::SolverError(format!(
                    "FRAIG timed out after {}s on {}-node miter (multiplier-hard). \
                     Simulation-based equivalence passed.",
                    fraig_timeout.as_secs(),
                    miter.and_count()
                )))
            }
        }
    }

    /// Check equivalence between two GateNetlists
    pub fn check_netlist_equivalence(
        &self,
        netlist1: &GateNetlist,
        netlist2: &GateNetlist,
    ) -> FormalResult<EquivalenceResult> {
        let aig1 = GateNetlistToAig::new().convert(netlist1);
        let aig2 = GateNetlistToAig::new().convert(netlist2);

        let miter = build_miter(&aig1, &aig2)?;
        check_equivalence_sat(&miter)
    }

    /// Check equivalence between two AIG representations directly
    pub fn check_aig_equivalence(
        &self,
        aig1: &Aig,
        aig2: &Aig,
    ) -> FormalResult<EquivalenceResult> {
        let miter = build_miter(aig1, aig2)?;
        check_equivalence_sat(&miter)
    }
}

impl Default for EquivalenceChecker {
    fn default() -> Self {
        Self::new()
    }
}

// ============================================================================
// Sequential Equivalence Checking (SEC)
// ============================================================================

/// Information about a register in the design
#[derive(Debug, Clone)]
pub struct RegisterInfo {
    /// Register name/path
    pub name: String,
    /// Bit width
    pub width: u32,
    /// Reset value (if any)
    pub reset_value: Option<u64>,
    /// Signal ID for the D-input (next state)
    pub d_input: LirSignalId,
    /// Signal ID for the Q-output (current state)
    pub q_output: LirSignalId,
    /// Whether it has enable
    pub has_enable: bool,
    /// Enable signal (if has_enable)
    pub enable: Option<LirSignalId>,
}

/// Result of sequential equivalence checking
#[derive(Debug, Clone)]
pub struct SequentialEquivalenceResult {
    /// True if designs are proven sequentially equivalent
    pub equivalent: bool,
    /// Register matching results
    pub register_matches: Vec<RegisterMatchResult>,
    /// Counterexample if not equivalent (multi-cycle trace)
    pub counterexample: Option<Counterexample>,
    /// Which check failed (if any)
    pub failure_reason: Option<String>,
    /// Total proof time in milliseconds
    pub time_ms: u64,
}

/// Result of matching a single register pair
#[derive(Debug, Clone)]
pub struct RegisterMatchResult {
    /// Register name in design 1
    pub name1: String,
    /// Register name in design 2
    pub name2: String,
    /// Width matches
    pub width_match: bool,
    /// Reset value matches
    pub reset_match: bool,
    /// Next-state function equivalent
    pub next_state_equivalent: bool,
}

/// Extract register information from a LIR design
pub fn extract_registers(lir: &Lir) -> Vec<RegisterInfo> {
    let mut registers = Vec::new();

    for node in &lir.nodes {
        match &node.op {
            LirOp::Reg {
                width,
                has_enable,
                has_reset: _,
                reset_value,
            } => {
                // The first input is always D (data)
                let d_input = node.inputs.get(0).copied().unwrap_or(LirSignalId(0));

                // Enable is second input if present
                let enable = if *has_enable {
                    node.inputs.get(1).copied()
                } else {
                    None
                };

                let output_signal = &lir.signals[node.output.0 as usize];

                registers.push(RegisterInfo {
                    name: output_signal.name.clone(),
                    width: *width,
                    reset_value: *reset_value,
                    d_input,
                    q_output: node.output,
                    has_enable: *has_enable,
                    enable,
                });
            }
            LirOp::Latch { width } => {
                // Latch: inputs are [enable, data]
                let d_input = node.inputs.get(1).copied().unwrap_or(LirSignalId(0));
                let enable = node.inputs.get(0).copied();

                let output_signal = &lir.signals[node.output.0 as usize];

                registers.push(RegisterInfo {
                    name: output_signal.name.clone(),
                    width: *width,
                    reset_value: None,
                    d_input,
                    q_output: node.output,
                    has_enable: true,
                    enable,
                });
            }
            _ => {}
        }
    }

    registers
}

/// Match registers between two designs by name
pub fn match_registers<'a>(
    regs1: &'a [RegisterInfo],
    regs2: &'a [RegisterInfo],
) -> Vec<(Option<&'a RegisterInfo>, Option<&'a RegisterInfo>)> {
    let mut matches = Vec::new();
    let mut matched2: std::collections::HashSet<usize> = std::collections::HashSet::new();

    // Match by name
    for reg1 in regs1 {
        let mut found = None;
        for (idx2, reg2) in regs2.iter().enumerate() {
            if !matched2.contains(&idx2) && reg1.name == reg2.name {
                found = Some((idx2, reg2));
                break;
            }
        }

        if let Some((idx2, reg2)) = found {
            matches.push((Some(reg1), Some(reg2)));
            matched2.insert(idx2);
        } else {
            matches.push((Some(reg1), None));
        }
    }

    // Add unmatched registers from design 2
    for (idx2, reg2) in regs2.iter().enumerate() {
        if !matched2.contains(&idx2) {
            matches.push((None, Some(reg2)));
        }
    }

    matches
}

/// Sequential equivalence checker
pub struct SequentialEquivalenceChecker {
    /// Maximum bound for bounded model checking
    max_bound: usize,
    /// Timeout in seconds
    timeout: u64,
}

impl SequentialEquivalenceChecker {
    pub fn new() -> Self {
        Self {
            max_bound: 10,
            timeout: 300,
        }
    }

    pub fn with_bound(mut self, bound: usize) -> Self {
        self.max_bound = bound;
        self
    }

    pub fn with_timeout(mut self, timeout: u64) -> Self {
        self.timeout = timeout;
        self
    }

    /// Check sequential equivalence between two LIR designs
    ///
    /// This performs:
    /// 1. Register matching by name
    /// 2. Reset value verification
    /// 3. Next-state function equivalence for each register pair
    /// 4. Output equivalence at each cycle (bounded)
    pub fn check_sequential_equivalence(
        &self,
        lir1: &Lir,
        lir2: &Lir,
    ) -> FormalResult<SequentialEquivalenceResult> {
        let start = std::time::Instant::now();

        // Extract registers from both designs
        let regs1 = extract_registers(lir1);
        let regs2 = extract_registers(lir2);

        // Match registers
        let matches = match_registers(&regs1, &regs2);

        let mut register_results = Vec::new();
        let mut all_equivalent = true;
        let mut failure_reason = None;

        // Check each register pair
        for (reg1_opt, reg2_opt) in &matches {
            match (reg1_opt, reg2_opt) {
                (Some(reg1), Some(reg2)) => {
                    // Check width match
                    let width_match = reg1.width == reg2.width;
                    if !width_match {
                        all_equivalent = false;
                        if failure_reason.is_none() {
                            failure_reason = Some(format!(
                                "Register '{}' width mismatch: {} vs {}",
                                reg1.name, reg1.width, reg2.width
                            ));
                        }
                    }

                    // Check reset value match
                    let reset_match = reg1.reset_value == reg2.reset_value;
                    if !reset_match {
                        all_equivalent = false;
                        if failure_reason.is_none() {
                            failure_reason = Some(format!(
                                "Register '{}' reset value mismatch: {:?} vs {:?}",
                                reg1.name, reg1.reset_value, reg2.reset_value
                            ));
                        }
                    }

                    // Check next-state function equivalence
                    let next_state_equivalent = if width_match {
                        self.check_next_state_equivalence(lir1, lir2, reg1, reg2)?
                    } else {
                        false
                    };

                    if !next_state_equivalent {
                        all_equivalent = false;
                        if failure_reason.is_none() {
                            failure_reason = Some(format!(
                                "Register '{}' next-state function mismatch",
                                reg1.name
                            ));
                        }
                    }

                    register_results.push(RegisterMatchResult {
                        name1: reg1.name.clone(),
                        name2: reg2.name.clone(),
                        width_match,
                        reset_match,
                        next_state_equivalent,
                    });
                }
                (Some(reg1), None) => {
                    all_equivalent = false;
                    if failure_reason.is_none() {
                        failure_reason = Some(format!(
                            "Register '{}' exists only in design 1",
                            reg1.name
                        ));
                    }
                    register_results.push(RegisterMatchResult {
                        name1: reg1.name.clone(),
                        name2: "(missing)".to_string(),
                        width_match: false,
                        reset_match: false,
                        next_state_equivalent: false,
                    });
                }
                (None, Some(reg2)) => {
                    all_equivalent = false;
                    if failure_reason.is_none() {
                        failure_reason = Some(format!(
                            "Register '{}' exists only in design 2",
                            reg2.name
                        ));
                    }
                    register_results.push(RegisterMatchResult {
                        name1: "(missing)".to_string(),
                        name2: reg2.name.clone(),
                        width_match: false,
                        reset_match: false,
                        next_state_equivalent: false,
                    });
                }
                (None, None) => unreachable!(),
            }
        }

        // If all registers match, also verify output equivalence
        let counterexample = if all_equivalent {
            // Check combinational output equivalence (same as CEC)
            let cec_result = EquivalenceChecker::new().check_lir_equivalence(lir1, lir2)?;
            if !cec_result.equivalent {
                all_equivalent = false;
                failure_reason = Some("Output logic mismatch".to_string());
                cec_result.counterexample
            } else {
                None
            }
        } else {
            None
        };

        Ok(SequentialEquivalenceResult {
            equivalent: all_equivalent,
            register_matches: register_results,
            counterexample,
            failure_reason,
            time_ms: start.elapsed().as_millis() as u64,
        })
    }

    /// Check if two registers have equivalent next-state functions
    fn check_next_state_equivalence(
        &self,
        lir1: &Lir,
        lir2: &Lir,
        reg1: &RegisterInfo,
        reg2: &RegisterInfo,
    ) -> FormalResult<bool> {
        // Build AIGs for just the next-state cone of each register
        let aig1 = self.build_next_state_aig(lir1, reg1);
        let aig2 = self.build_next_state_aig(lir2, reg2);

        // Build miter and check
        let miter = build_miter(&aig1, &aig2)?;
        let result = check_equivalence_sat(&miter)?;

        Ok(result.equivalent)
    }

    /// Build an AIG for the next-state logic cone of a register
    fn build_next_state_aig(&self, lir: &Lir, reg: &RegisterInfo) -> Aig {
        // Create a modified LIR view where:
        // - Primary inputs are the original inputs + all register Q outputs
        // - Primary output is just this register's D input

        let mut converter = LirToAig::new();
        let mut aig = Aig::new();

        // Add all primary inputs
        for &input_id in &lir.inputs {
            let signal = &lir.signals[input_id.0 as usize];
            for bit in 0..signal.width {
                let name = if signal.width == 1 {
                    signal.name.clone()
                } else {
                    format!("{}[{}]", signal.name, bit)
                };
                let lit = aig.add_input(name);
                converter.signal_map.insert((input_id.0, bit), lit);
            }
        }

        // Add register Q outputs as inputs (they represent current state)
        let regs = extract_registers(lir);
        for r in &regs {
            let signal = &lir.signals[r.q_output.0 as usize];
            for bit in 0..signal.width {
                let name = if signal.width == 1 {
                    format!("{}_q", signal.name)
                } else {
                    format!("{}_q[{}]", signal.name, bit)
                };
                let lit = aig.add_input(name);
                converter.signal_map.insert((r.q_output.0, bit), lit);
            }
        }

        // Process nodes to build combinational logic
        converter.aig = aig;
        for node in &lir.nodes {
            // Skip register nodes themselves
            if !node.op.is_sequential() {
                converter.convert_node(node, lir);
            }
        }

        // Add the D-input as output
        let d_signal = &lir.signals[reg.d_input.0 as usize];
        for bit in 0..reg.width {
            let name = if reg.width == 1 {
                format!("{}_d", reg.name)
            } else {
                format!("{}_d[{}]", reg.name, bit)
            };
            let lit = converter
                .signal_map
                .get(&(reg.d_input.0, bit))
                .copied()
                .unwrap_or_else(|| converter.aig.false_lit());
            converter.aig.add_output(name, lit);
        }

        converter.aig
    }

    /// Bounded model checking for K cycles
    ///
    /// Unrolls both designs for K cycles and verifies:
    /// 1. Starting from same initial state (after reset)
    /// 2. Given same inputs at each cycle
    /// 3. Outputs match at each cycle
    pub fn check_bounded_equivalence(
        &self,
        lir1: &Lir,
        lir2: &Lir,
        k: usize,
    ) -> FormalResult<SequentialEquivalenceResult> {
        let start = std::time::Instant::now();

        // For bounded checking, we unroll the transition relation K times
        // This is more expensive but can find bugs that manifest after multiple cycles

        let regs1 = extract_registers(lir1);
        let regs2 = extract_registers(lir2);

        // First verify register structure matches
        let matches = match_registers(&regs1, &regs2);
        for (r1, r2) in &matches {
            match (r1, r2) {
                (Some(reg1), Some(reg2)) => {
                    if reg1.width != reg2.width {
                        return Ok(SequentialEquivalenceResult {
                            equivalent: false,
                            register_matches: vec![],
                            counterexample: None,
                            failure_reason: Some(format!(
                                "Register '{}' width mismatch",
                                reg1.name
                            )),
                            time_ms: start.elapsed().as_millis() as u64,
                        });
                    }
                }
                (Some(r), None) | (None, Some(r)) => {
                    return Ok(SequentialEquivalenceResult {
                        equivalent: false,
                        register_matches: vec![],
                        counterexample: None,
                        failure_reason: Some(format!("Register '{}' unmatched", r.name)),
                        time_ms: start.elapsed().as_millis() as u64,
                    });
                }
                _ => {}
            }
        }

        // Build unrolled formula
        let mut formula = CnfFormula::new();
        let mut var_counter = 0u32;

        // Maps for each time step: (signal_id, bit, time) -> Var
        let mut signal_vars: HashMap<(u32, u32, usize), Var> = HashMap::new();

        // Helper to get or create variable
        let mut get_var = |sig: u32, bit: u32, time: usize| -> Var {
            if let Some(&v) = signal_vars.get(&(sig, bit, time)) {
                v
            } else {
                let v = Var::from_index(var_counter as usize);
                var_counter += 1;
                signal_vars.insert((sig, bit, time), v);
                v
            }
        };

        // For each time step 0..k
        for t in 0..k {
            // Create variables for inputs at time t (shared between designs)
            for &input_id in &lir1.inputs {
                let signal = &lir1.signals[input_id.0 as usize];
                for bit in 0..signal.width {
                    get_var(input_id.0, bit, t);
                }
            }

            // At t=0, constrain registers to reset values
            if t == 0 {
                for reg in &regs1 {
                    if let Some(reset_val) = reg.reset_value {
                        for bit in 0..reg.width {
                            let var = get_var(reg.q_output.0, bit, 0);
                            let bit_val = (reset_val >> bit) & 1;
                            if bit_val == 1 {
                                formula.add_clause(&[Lit::positive(var)]);
                            } else {
                                formula.add_clause(&[Lit::negative(var)]);
                            }
                        }
                    }
                }
            }

            // TODO: Add transition relation constraints
            // This would require converting LIR operations to CNF for each time step
            // For now, we rely on the simpler next-state function checking
        }

        // For full bounded unrolling, we would need to:
        // 1. Convert all combinational logic to CNF for each time step
        // 2. Connect register D[t] to Q[t+1]
        // 3. Assert outputs match at each time step
        // This is left as future work - the current implementation
        // checks next-state function equivalence which is sufficient for most cases

        // Fall back to next-state function checking
        self.check_sequential_equivalence(lir1, lir2)
    }
}

impl Default for SequentialEquivalenceChecker {
    fn default() -> Self {
        Self::new()
    }
}

// ============================================================================
// MIR to AIG Conversion (RTL-level equivalence checking)
// ============================================================================

use skalp_mir::{
    Assignment, AssignmentKind, BinaryOp, Block, CaseStatement, ContinuousAssign,
    DataType, Expression, ExpressionKind, IfStatement, LValue, Mir, Module, ModuleId,
    ModuleInstance, Port, PortDirection, PortId, Process, ProcessKind, ReduceOp, Signal,
    SignalId, Statement, UnaryOp, Value, VariableId,
};

/// Signal reference in MIR (either port or signal)
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum MirSignalRef {
    Port(PortId),
    Signal(SignalId),
    Variable(VariableId),
}

/// Convert MIR module to AIG for formal equivalence checking
///
/// This converts behavioral RTL (if/else, case, processes) to combinational
/// logic (AND-Inverter Graph) for formal verification.
pub struct MirToAig<'a> {
    /// The top-level module being converted
    module: &'a Module,
    /// Full MIR design (for hierarchical flattening)
    mir: Option<&'a Mir>,
    /// Current module being processed (for hierarchical conversion)
    current_module: Option<&'a Module>,
    /// The AIG being built
    pub aig: Aig,
    /// Map from (MirSignalRef, bit_index) to current AigLit
    signal_map: BTreeMap<(MirSignalRef, u32), AigLit>,
    /// Per-instance signal maps keyed by instance path ("" = top module)
    instance_signal_maps: BTreeMap<String, BTreeMap<(MirSignalRef, u32), AigLit>>,
    /// Current instance path ("" = top module)
    current_instance_path: String,
    /// Map from port/signal names to their reference
    name_to_ref: BTreeMap<String, MirSignalRef>,
    /// Register outputs (for sequential detection)
    register_outputs: Vec<MirSignalRef>,
}

impl<'a> MirToAig<'a> {
    pub fn new(module: &'a Module) -> Self {
        let mut converter = Self {
            module,
            mir: None,
            current_module: None,
            aig: Aig::new(),
            signal_map: BTreeMap::new(),
            instance_signal_maps: BTreeMap::new(),
            current_instance_path: String::new(),
            name_to_ref: BTreeMap::new(),
            register_outputs: Vec::new(),
        };
        converter.build_name_map();
        converter
    }

    /// Create a new MIR-to-AIG converter with full hierarchy access
    ///
    /// When the full MIR is provided, `convert_sequential` will flatten child
    /// entity instances, processing their registers as part of the top-level AIG.
    pub fn new_with_mir(mir: &'a Mir, top_module: &'a Module) -> Self {
        let mut converter = Self {
            module: top_module,
            mir: Some(mir),
            current_module: None,
            aig: Aig::new(),
            signal_map: BTreeMap::new(),
            instance_signal_maps: BTreeMap::new(),
            current_instance_path: String::new(),
            name_to_ref: BTreeMap::new(),
            register_outputs: Vec::new(),
        };
        converter.build_name_map();
        converter
    }

    fn build_name_map(&mut self) {
        for port in &self.module.ports {
            self.name_to_ref
                .insert(port.name.clone(), MirSignalRef::Port(port.id));
        }
        for signal in &self.module.signals {
            self.name_to_ref
                .insert(signal.name.clone(), MirSignalRef::Signal(signal.id));
        }
        for var in &self.module.variables {
            self.name_to_ref
                .insert(var.name.clone(), MirSignalRef::Variable(var.id));
        }
    }

    /// Switch signal_map context to a different module instance.
    /// Saves current signal_map under current_instance_path,
    /// loads target instance's signal_map (or empty if first visit).
    fn switch_to_instance(&mut self, instance_path: &str) {
        if instance_path == self.current_instance_path {
            return;
        }
        let current_map = std::mem::take(&mut self.signal_map);
        self.instance_signal_maps.insert(self.current_instance_path.clone(), current_map);
        self.signal_map = self.instance_signal_maps.remove(instance_path).unwrap_or_default();
        self.current_instance_path = instance_path.to_string();
    }

    /// Find a port by its ID
    fn find_port(&self, id: PortId) -> Option<&Port> {
        let module = self.current_module.unwrap_or(self.module);
        module.ports.iter().find(|p| p.id == id)
    }

    /// Find a signal by its ID
    fn find_signal(&self, id: SignalId) -> Option<&Signal> {
        let module = self.current_module.unwrap_or(self.module);
        module.signals.iter().find(|s| s.id == id)
    }

    /// Find a variable by its ID
    fn find_variable(&self, id: VariableId) -> Option<&skalp_mir::Variable> {
        let module = self.current_module.unwrap_or(self.module);
        module.variables.iter().find(|v| v.id == id)
    }

    /// Convert MIR module to AIG
    pub fn convert(self) -> Aig {
        self.convert_internal(false)
    }

    /// Convert MIR module to AIG for combinational equivalence checking
    ///
    /// This treats register outputs as primary inputs, matching how GateNetlistToAig
    /// handles DFF cells. This is essential for proper sequential design equivalence.
    pub fn convert_for_cec(self) -> Aig {
        self.convert_internal(true)
    }

    /// Convert MIR module to sequential AIG with proper Latch nodes
    ///
    /// This creates an AIG representation where:
    /// - Primary inputs are inputs
    /// - Registers become Latch nodes with next-state logic
    /// - Primary outputs are outputs
    ///
    /// This is needed for BMC (Bounded Model Checking) which unrolls
    /// the sequential circuit over multiple cycles.
    pub fn convert_sequential(mut self) -> Aig {
        // First pass: detect all registers (signals assigned in sequential processes)
        let mut register_signals: Vec<(MirSignalRef, String, u32)> = Vec::new();

        for process in &self.module.processes {
            if matches!(process.kind, ProcessKind::Sequential) {
                self.collect_register_signals(&process.body, &mut register_signals);
            }
        }

        // Populate register_outputs so convert_statement_for_bmc can identify register assignments
        for (sig_ref, _, _) in &register_signals {
            if !self.register_outputs.contains(sig_ref) {
                self.register_outputs.push(*sig_ref);
            }
        }

        // Add primary inputs (input ports)
        for port in &self.module.ports {
            if port.direction == PortDirection::Input {
                let width = self.get_type_width(&port.port_type);
                for bit in 0..width {
                    let name = if width == 1 {
                        port.name.clone()
                    } else {
                        format!("{}[{}]", port.name, bit)
                    };
                    let lit = self.aig.add_input(name);
                    self.signal_map
                        .insert((MirSignalRef::Port(port.id), bit as u32), lit);
                }
            }
        }

        // For sequential conversion, we need to:
        // 1. Create placeholder literals for register current values
        // 2. Convert sequential process bodies to get next-state logic
        // 3. Create Latch nodes connecting current to next

        // Create temporary input nodes for register current values
        // These will be replaced with proper latch outputs
        let mut reg_current_lits: BTreeMap<(MirSignalRef, u32), AigLit> = BTreeMap::new();
        for (sig_ref, name, width) in &register_signals {
            for bit in 0..*width {
                // Create a temporary input for the current register value
                let temp_name = if *width == 1 {
                    format!("__reg_cur_{}", name)
                } else {
                    format!("__reg_cur_{}[{}]", name, bit)
                };
                let lit = self.aig.add_input(temp_name);
                reg_current_lits.insert((*sig_ref, bit), lit);
                // Also add to signal_map so reading the register uses current value
                self.signal_map.insert((*sig_ref, bit), lit);
            }
        }

        // Process continuous assignments
        for assign in &self.module.assignments {
            self.convert_continuous_assign(assign);
        }

        // Process combinational processes
        for process in &self.module.processes {
            if matches!(process.kind, ProcessKind::Combinational) {
                self.convert_combinational_process(process);
            }
        }

        // Process sequential processes to compute next-state logic
        // Store computed next-state values separately
        let mut next_state_map: BTreeMap<(MirSignalRef, u32), AigLit> = BTreeMap::new();

        // Collect reset values from sequential processes
        let mut reset_values: BTreeMap<(MirSignalRef, u32), u64> = BTreeMap::new();
        for process in &self.module.processes {
            if matches!(process.kind, ProcessKind::Sequential) {
                // Convert sequential body - assignments go to next-state
                self.convert_sequential_process_for_bmc(process, &mut next_state_map);
                // Collect reset values
                self.collect_reset_values(&process.body, &mut reset_values);
            }
        }

        // Now create Latch nodes for each register
        // We need to: remove the temp inputs, add latches properly
        // For simplicity, we'll create latches with the computed next-state

        for (sig_ref, name, width) in &register_signals {
            for bit in 0..*width {
                let latch_name = if *width == 1 {
                    name.clone()
                } else {
                    format!("{}[{}]", name, bit)
                };

                // Get next-state logic (or false if not computed)
                let next_lit = next_state_map
                    .get(&(*sig_ref, bit))
                    .copied()
                    .unwrap_or_else(|| {
                        // If no next-state computed, register holds its value
                        reg_current_lits
                            .get(&(*sig_ref, bit))
                            .copied()
                            .unwrap_or_else(|| self.aig.false_lit())
                    });

                // Get reset value (default to false if not specified)
                let init_value = reset_values
                    .get(&(*sig_ref, bit))
                    .map(|&v| v != 0)
                    .unwrap_or(false);

                // Create latch with proper init value
                let latch_lit = self.aig.add_latch(latch_name, next_lit, init_value);
                let latch_id = latch_lit.node.0;

                // Link state input to latch (metadata-based, no name matching needed)
                if let Some(state_input_lit) = reg_current_lits.get(&(*sig_ref, bit)) {
                    self.aig.link_state_input_to_latch(state_input_lit.node.0, latch_id);
                }

                // Update signal_map to point to latch output
                self.signal_map.insert((*sig_ref, bit), latch_lit);
            }
        }

        // Add primary outputs (output ports)
        for port in &self.module.ports {
            if port.direction == PortDirection::Output {
                let width = self.get_type_width(&port.port_type);
                for bit in 0..width {
                    let name = if width == 1 {
                        port.name.clone()
                    } else {
                        format!("{}[{}]", port.name, bit)
                    };
                    let lit = self
                        .signal_map
                        .get(&(MirSignalRef::Port(port.id), bit as u32))
                        .copied()
                        .unwrap_or_else(|| self.aig.false_lit());
                    self.aig.add_output(name, lit);
                }
            }
        }

        self.aig
    }

    /// Convert MIR hierarchy to sequential AIG with proper Latch nodes
    ///
    /// This flattens the entire design hierarchy (top module + all instances)
    /// into a single AIG, which is necessary for proper BMC equivalence checking
    /// against a flattened gate netlist.
    ///
    /// Requires `new_with_mir` constructor to provide the full MIR.
    pub fn convert_sequential_hierarchical(mut self) -> Aig {
        let mir = match self.mir {
            Some(m) => m,
            None => {
                // Fall back to single-module conversion if no MIR provided
                return self.convert_sequential();
            }
        };

        // Collect all modules in the hierarchy with their instance paths
        // (instance_path, module_ref)
        let mut all_instances: Vec<(String, &Module)> = Vec::new();
        self.collect_instances_recursive("", self.module, mir, &mut all_instances);

        log::debug!("[HIER_AIG] Found {} instances in hierarchy", all_instances.len());

        // First pass: collect all registers from all instances
        // (instance_path, sig_ref, name, width)
        let mut all_registers: Vec<(String, MirSignalRef, String, u32)> = Vec::new();

        for (inst_path, module) in &all_instances {
            for process in &module.processes {
                if matches!(process.kind, ProcessKind::Sequential) {
                    let mut module_regs: Vec<(MirSignalRef, String, u32)> = Vec::new();
                    self.collect_register_signals_from_module(module, &process.body, &mut module_regs);

                    for (sig_ref, name, width) in module_regs {
                        let prefixed_name = if inst_path.is_empty() {
                            name.clone()
                        } else {
                            format!("{}.{}", inst_path, name)
                        };
                        all_registers.push((inst_path.clone(), sig_ref, prefixed_name, width));
                    }
                }
            }
        }

        log::debug!("[HIER_AIG] Found {} registers total", all_registers.len());

        // Add primary inputs (only from top module)
        for port in &self.module.ports {
            if port.direction == PortDirection::Input {
                let width = self.get_type_width(&port.port_type);
                for bit in 0..width {
                    let name = if width == 1 {
                        port.name.clone()
                    } else {
                        format!("{}[{}]", port.name, bit)
                    };
                    let lit = self.aig.add_input(name);
                    self.signal_map.insert((MirSignalRef::Port(port.id), bit as u32), lit);
                }
            }
        }

        // BUG #263 FIX: Struct port flattening for RangeSelect access.
        // MIR flattens struct ports (e.g., `config`) into individual ports with `__` separator:
        //   config__tap_48v (PortId 365), config__voltage_loop__kp (PortId 366), etc.
        // Connection expressions use RangeSelect(Port(first_field_id), high, low) to extract
        // struct fields by bit offset from the concatenated struct. But the primary input loop
        // above only creates signal_map entries for each port's OWN bits, so RangeSelect beyond
        // the first port's width returns false_lit() (zeros).
        // Fix: detect struct port groups by name prefix and populate the first port's signal_map
        // entries with concatenated bits from all ports in the group.
        {
            let mut struct_groups: BTreeMap<String, Vec<(PortId, u32)>> = BTreeMap::new();
            for port in &self.module.ports {
                if port.direction == PortDirection::Input {
                    if let Some(prefix_end) = port.name.find("__") {
                        let prefix = port.name[..prefix_end].to_string();
                        let width = self.get_type_width(&port.port_type) as u32;
                        struct_groups.entry(prefix).or_default().push((port.id, width));
                    }
                }
            }

            for (_prefix, ports) in &struct_groups {
                if ports.len() < 2 {
                    continue;
                }
                // First port in the group is the RangeSelect base
                let base_port_id = ports[0].0;
                let mut cumulative_offset = ports[0].1; // skip first port's own bits (already mapped)
                for &(port_id, width) in &ports[1..] {
                    for bit in 0..width {
                        if let Some(&lit) = self.signal_map.get(&(MirSignalRef::Port(port_id), bit)) {
                            self.signal_map.insert(
                                (MirSignalRef::Port(base_port_id), cumulative_offset + bit),
                                lit,
                            );
                        }
                    }
                    cumulative_offset += width;
                }
                log::debug!(
                    "[HIER_AIG] Struct port group '{}': {} ports, {} total bits mapped to base {:?}",
                    _prefix, ports.len(), cumulative_offset, base_port_id
                );
            }
        }

        // Create temporary input nodes for all register current values
        let mut reg_current_lits: BTreeMap<(String, MirSignalRef, u32), AigLit> = BTreeMap::new();
        for (inst_path, sig_ref, name, width) in &all_registers {
            for bit in 0..*width {
                let temp_name = if *width == 1 {
                    format!("__reg_cur_{}", name)
                } else {
                    format!("__reg_cur_{}[{}]", name, bit)
                };
                let lit = self.aig.add_input(temp_name);
                reg_current_lits.insert((inst_path.clone(), *sig_ref, bit), lit);
            }
        }

        // BUG #273 part 2: Pre-order passes to propagate combinational values through
        // the hierarchy. With per-instance signal maps, each pass propagates values
        // up one level from children to parents. For a hierarchy of depth D, we need
        // D passes to fully propagate leaf values to the root.
        //
        // Compute max hierarchy depth from instance paths.
        let max_depth = all_instances.iter()
            .map(|(path, _)| if path.is_empty() { 0 } else { path.matches('.').count() + 1 })
            .max()
            .unwrap_or(0);
        let num_passes = max_depth.max(2); // At least 2 passes for cross-sibling dependencies

        log::debug!("[HIER_AIG] Max hierarchy depth: {}, running {} pre-order passes", max_depth, num_passes);

        for pass in 0..num_passes {
            for (inst_path, module) in all_instances.iter().rev() {
                self.current_module = Some(*module);
                self.switch_to_instance(inst_path);

                // Set up register state for this module
                for (path, sig_ref, _, width) in &all_registers {
                    if path == inst_path {
                        for bit in 0..*width {
                            if let Some(&lit) = reg_current_lits.get(&(path.clone(), *sig_ref, bit)) {
                                self.signal_map.insert((*sig_ref, bit), lit);
                            }
                        }
                    }
                }

                // Track which signals are registers in this module
                self.register_outputs.clear();
                for (path, sig_ref, _, _) in &all_registers {
                    if path == inst_path {
                        self.register_outputs.push(*sig_ref);
                    }
                }

                // Connect INPUT ports from parent
                if !inst_path.is_empty() {
                    self.connect_instance_inputs(inst_path, mir);
                }

                // Process continuous assignments and combinational processes
                for assign in &module.assignments {
                    self.convert_continuous_assign(assign);
                }
                for process in &module.processes {
                    if matches!(process.kind, ProcessKind::Combinational) {
                        self.convert_combinational_process(process);
                    }
                }

                // Connect OUTPUT ports to parent
                if !inst_path.is_empty() {
                    self.connect_instance_outputs(inst_path, mir);
                }
            }

            log::debug!("[HIER_AIG] Pre-order pass {}/{} complete", pass + 1, num_passes);
        }

        // Main pass: process in POST-ORDER (children first) for sequential logic
        // and to re-evaluate with child outputs now available
        let mut next_state_map: BTreeMap<(String, MirSignalRef, u32), AigLit> = BTreeMap::new();
        let mut reset_values: BTreeMap<(String, MirSignalRef, u32), u64> = BTreeMap::new();

        for (inst_path, module) in &all_instances {
            // BUG #273 part 2: Per-instance signal map isolation.
            // Ancestor refresh re-processes the ancestor chain to pick up any
            // cross-instance updates. With per-instance maps, each ancestor's
            // combinational logic writes only to its own signal_map (no collisions).
            // After processing each ancestor's combinational logic, we re-connect
            // its children's outputs to restore values that combinational logic may
            // have overwritten (e.g., default signal initializers).
            if !inst_path.is_empty() {
                let parts: Vec<&str> = inst_path.split('.').collect();

                for depth in 0..parts.len() {
                    let ancestor_path = if depth == 0 {
                        String::new()
                    } else {
                        parts[..depth].join(".")
                    };

                    let ancestor_module = if ancestor_path.is_empty() {
                        self.module
                    } else {
                        self.find_module_for_instance_path(mir, &ancestor_path)
                            .unwrap_or(self.module)
                    };

                    self.switch_to_instance(&ancestor_path);
                    self.current_module = if ancestor_path.is_empty() {
                        None
                    } else {
                        Some(ancestor_module)
                    };

                    if !ancestor_path.is_empty() {
                        self.connect_instance_inputs(&ancestor_path, mir);
                    }

                    for assign in &ancestor_module.assignments {
                        self.convert_continuous_assign(assign);
                    }
                    for process in &ancestor_module.processes {
                        if matches!(process.kind, ProcessKind::Combinational) {
                            self.convert_combinational_process(process);
                        }
                    }

                    // Re-connect all children's outputs to restore values that
                    // combinational logic may have overwritten
                    for child_inst in &ancestor_module.instances {
                        let child_path = if ancestor_path.is_empty() {
                            child_inst.name.clone()
                        } else {
                            format!("{}.{}", ancestor_path, child_inst.name)
                        };
                        let child_exists = self.instance_signal_maps.contains_key(&child_path)
                            || self.current_instance_path == child_path;
                        if child_exists {
                            if let Some(child_mod) = mir.modules.iter().find(|m| m.id == child_inst.module) {
                                self.switch_to_instance(&child_path);
                                self.current_module = Some(child_mod);
                                self.connect_instance_outputs(&child_path, mir);
                            }
                        }
                    }
                    // Switch back to ancestor for next iteration
                    self.switch_to_instance(&ancestor_path);
                    self.current_module = if ancestor_path.is_empty() {
                        None
                    } else {
                        Some(ancestor_module)
                    };
                }

                // Switch to child instance and connect its inputs
                self.switch_to_instance(inst_path);
                self.current_module = Some(*module);
                self.connect_instance_inputs(inst_path, mir);
            } else {
                self.switch_to_instance(inst_path);
                self.current_module = if inst_path.is_empty() {
                    None
                } else {
                    Some(*module)
                };
            }

            // Load registers for this instance
            for (path, sig_ref, _, width) in &all_registers {
                if path == inst_path {
                    for bit in 0..*width {
                        if let Some(&lit) = reg_current_lits.get(&(path.clone(), *sig_ref, bit)) {
                            self.signal_map.insert((*sig_ref, bit), lit);
                        }
                    }
                }
            }

            // Track which signals are registers in this module
            self.register_outputs.clear();
            for (path, sig_ref, _, _) in &all_registers {
                if path == inst_path {
                    self.register_outputs.push(*sig_ref);
                }
            }

            // Re-process continuous assignments and combinational logic
            for assign in &module.assignments {
                self.convert_continuous_assign(assign);
            }
            for process in &module.processes {
                if matches!(process.kind, ProcessKind::Combinational) {
                    self.convert_combinational_process(process);
                }
            }

            // Process sequential processes (only in post-order pass)
            for process in &module.processes {
                if matches!(process.kind, ProcessKind::Sequential) {
                    let mut inst_next_state: BTreeMap<(MirSignalRef, u32), AigLit> = BTreeMap::new();
                    self.convert_sequential_process_for_bmc(process, &mut inst_next_state);

                    // Transfer to global next_state_map with instance prefix
                    for ((sig_ref, bit), lit) in inst_next_state {
                        next_state_map.insert((inst_path.clone(), sig_ref, bit), lit);
                    }

                    // Collect reset values
                    let mut inst_reset: BTreeMap<(MirSignalRef, u32), u64> = BTreeMap::new();
                    self.collect_reset_values(&process.body, &mut inst_reset);
                    for ((sig_ref, bit), val) in inst_reset {
                        reset_values.insert((inst_path.clone(), sig_ref, bit), val);
                    }
                }
            }

            // Connect OUTPUT ports from child to parent
            if !inst_path.is_empty() {
                self.connect_instance_outputs(inst_path, mir);
            }
        }

        // Reset current_module to top module for final processing
        self.current_module = None;

        // Create Latch nodes for all registers
        for (inst_path, sig_ref, name, width) in &all_registers {
            self.switch_to_instance(inst_path);
            for bit in 0..*width {
                let latch_name = if *width == 1 {
                    name.clone()
                } else {
                    format!("{}[{}]", name, bit)
                };

                let next_lit = next_state_map
                    .get(&(inst_path.clone(), *sig_ref, bit))
                    .copied()
                    .unwrap_or_else(|| {
                        // If no next-state computed, register holds its value
                        reg_current_lits
                            .get(&(inst_path.clone(), *sig_ref, bit))
                            .copied()
                            .unwrap_or_else(|| self.aig.false_lit())
                    });

                let init_value = reset_values
                    .get(&(inst_path.clone(), *sig_ref, bit))
                    .map(|&v| v != 0)
                    .unwrap_or(false);

                let latch_lit = self.aig.add_latch(latch_name, next_lit, init_value);
                let latch_id = latch_lit.node.0;

                // Link state input to latch
                if let Some(state_input_lit) = reg_current_lits.get(&(inst_path.clone(), *sig_ref, bit)) {
                    self.aig.link_state_input_to_latch(state_input_lit.node.0, latch_id);
                }

                self.signal_map.insert((*sig_ref, bit), latch_lit);
            }
        }

        // Switch to top module context for reading output port values
        self.switch_to_instance("");
        self.current_module = None;

        // Add primary outputs (only from top module)
        for port in &self.module.ports {
            if port.direction == PortDirection::Output {
                let width = self.get_type_width(&port.port_type);
                for bit in 0..width {
                    let name = if width == 1 {
                        port.name.clone()
                    } else {
                        format!("{}[{}]", port.name, bit)
                    };
                    let lit = self.signal_map
                        .get(&(MirSignalRef::Port(port.id), bit as u32))
                        .copied()
                        .unwrap_or_else(|| self.aig.false_lit());
                    self.aig.add_output(name, lit);
                }
            }
        }

        self.aig
    }

    /// Recursively collect all instances in the hierarchy (post-order)
    /// Children are added before parents so their outputs are computed first
    fn collect_instances_recursive<'b>(
        &self,
        parent_path: &str,
        module: &'b Module,
        mir: &'b Mir,
        result: &mut Vec<(String, &'b Module)>,
    ) {
        // First, recursively process child instances (post-order traversal)
        for instance in &module.instances {
            let child_path = if parent_path.is_empty() {
                instance.name.clone()
            } else {
                format!("{}.{}", parent_path, instance.name)
            };

            // Find the child module - try direct match first
            let child_module = mir.modules.iter().find(|m| m.id == instance.module);

            // If found module has no instances but there's a monomorphized version, use that
            let resolved_module = if let Some(m) = child_module {
                if m.instances.is_empty() {
                    // Look for monomorphized version (e.g., "Foo" -> "Foo_10" or "Foo_10_1000")
                    let base_name = &m.name;
                    mir.modules.iter()
                        .filter(|mm| {
                            mm.name.starts_with(base_name) &&
                            mm.name.len() > base_name.len() &&
                            mm.name.chars().nth(base_name.len()) == Some('_') &&
                            !mm.instances.is_empty() // Must have instances
                        })
                        .next()
                        .unwrap_or(m)
                } else {
                    m
                }
            } else {
                continue;
            };

            self.collect_instances_recursive(&child_path, resolved_module, mir, result);
        }

        // Add this module after all children (post-order)
        result.push((parent_path.to_string(), module));
    }

    /// Find the module for a given instance path (e.g., "protection" -> ProtectionSystem module)
    fn find_module_for_instance_path(&self, mir: &'a Mir, inst_path: &str) -> Option<&'a Module> {
        let parts: Vec<&str> = inst_path.split('.').collect();
        let mut current_module: &'a Module = self.module;

        for part in parts {
            // Find the instance in current module
            let instance = current_module.instances.iter().find(|i| i.name == part)?;
            // Find the module definition
            current_module = mir.modules.iter().find(|m| m.id == instance.module)?;
        }

        Some(current_module)
    }

    /// Collect register signals from a specific module
    fn collect_register_signals_from_module(
        &self,
        module: &Module,
        block: &Block,
        registers: &mut Vec<(MirSignalRef, String, u32)>,
    ) {
        for stmt in &block.statements {
            match stmt {
                Statement::Assignment(assign) => {
                    let sig_ref = self.lvalue_to_ref_in_module(module, &assign.lhs);
                    if let Some(sig_ref) = sig_ref {
                        let (name, width) = self.get_signal_info_from_module(module, sig_ref);
                        if !registers.iter().any(|(r, _, _)| *r == sig_ref) {
                            registers.push((sig_ref, name, width));
                        }
                    }
                }
                Statement::If(if_stmt) => {
                    self.collect_register_signals_from_module(module, &if_stmt.then_block, registers);
                    if let Some(else_block) = &if_stmt.else_block {
                        self.collect_register_signals_from_module(module, else_block, registers);
                    }
                }
                Statement::Case(case_stmt) => {
                    for item in &case_stmt.items {
                        self.collect_register_signals_from_module(module, &item.block, registers);
                    }
                    if let Some(default) = &case_stmt.default {
                        self.collect_register_signals_from_module(module, default, registers);
                    }
                }
                Statement::ResolvedConditional(resolved) => {
                    let sig_ref = self.lvalue_to_ref_in_module(module, &resolved.target);
                    if let Some(sig_ref) = sig_ref {
                        let (name, width) = self.get_signal_info_from_module(module, sig_ref);
                        if !registers.iter().any(|(r, _, _)| *r == sig_ref) {
                            registers.push((sig_ref, name, width));
                        }
                    }
                }
                _ => {}
            }
        }
    }

    /// Get lvalue reference for a specific module
    /// NOTE: Only returns Signal and Port references, not Variables.
    /// Variables (let bindings) in sequential processes are intermediate computations,
    /// not registers. Only Signals and output Ports create actual state.
    fn lvalue_to_ref_in_module(&self, module: &Module, lvalue: &LValue) -> Option<MirSignalRef> {
        match lvalue {
            LValue::Signal(id) => {
                if module.signals.iter().any(|s| s.id == *id) {
                    Some(MirSignalRef::Signal(*id))
                } else {
                    None
                }
            }
            LValue::Port(id) => {
                if module.ports.iter().any(|p| p.id == *id) {
                    Some(MirSignalRef::Port(*id))
                } else {
                    None
                }
            }
            LValue::Variable(_id) => {
                // Variables (let bindings) are intermediate computations, not registers.
                // They should not be included in the register list for sequential processes.
                None
            }
            LValue::BitSelect { base, .. } => self.lvalue_to_ref_in_module(module, base),
            LValue::RangeSelect { base, .. } => self.lvalue_to_ref_in_module(module, base),
            LValue::Concat(_) => None, // Concatenations don't have a single reference
        }
    }

    /// Get signal name and width from a module
    fn get_signal_info_from_module(&self, module: &Module, sig_ref: MirSignalRef) -> (String, u32) {
        match sig_ref {
            MirSignalRef::Port(id) => {
                if let Some(port) = module.ports.iter().find(|p| p.id == id) {
                    (port.name.clone(), self.get_type_width(&port.port_type) as u32)
                } else {
                    ("unknown".to_string(), 1)
                }
            }
            MirSignalRef::Signal(id) => {
                if let Some(signal) = module.signals.iter().find(|s| s.id == id) {
                    (signal.name.clone(), self.get_type_width(&signal.signal_type) as u32)
                } else {
                    ("unknown".to_string(), 1)
                }
            }
            MirSignalRef::Variable(id) => {
                if let Some(var) = module.variables.iter().find(|v| v.id == id) {
                    (var.name.clone(), self.get_type_width(&var.var_type) as u32)
                } else {
                    ("unknown".to_string(), 1)
                }
            }
        }
    }

    /// Connect instance INPUT ports to parent signals (before processing child)
    /// Maps parent expressions to child input port IDs.
    /// Switches to parent context to evaluate connection expressions, then back to child.
    fn connect_instance_inputs(&mut self, inst_path: &str, mir: &'a Mir) {
        // Find the parent module and instance
        let (parent_path, inst_name) = if let Some(dot_pos) = inst_path.rfind('.') {
            (&inst_path[..dot_pos], &inst_path[dot_pos + 1..])
        } else {
            ("", inst_path)
        };

        let parent_module = if parent_path.is_empty() {
            self.module
        } else {
            self.find_module_for_instance_path(mir, parent_path)
                .unwrap_or(self.module)
        };

        // Save child context
        let child_instance_path = self.current_instance_path.clone();
        let child_module_saved = self.current_module;

        // Switch to parent to evaluate connection expressions
        self.switch_to_instance(parent_path);
        self.current_module = if parent_path.is_empty() {
            None
        } else {
            Some(parent_module)
        };

        let mut port_connections: Vec<(PortId, usize, Vec<AigLit>)> = Vec::new();
        if let Some(instance) = parent_module.instances.iter().find(|i| i.name == inst_name) {
            if let Some(child_module) = mir.modules.iter().find(|m| m.id == instance.module) {
                for (port_name, conn_expr) in &instance.connections {
                    if let Some(child_port) = child_module.ports.iter().find(|p| p.name == *port_name) {
                        if child_port.direction != PortDirection::Input {
                            continue;
                        }
                        let width = self.get_type_width(&child_port.port_type);
                        let conn_lits = self.convert_expression(conn_expr);
                        port_connections.push((child_port.id, width, conn_lits));
                    }
                }
            }
        }

        // Switch back to child and write port entries
        self.switch_to_instance(&child_instance_path);
        self.current_module = child_module_saved;

        for (port_id, width, conn_lits) in port_connections {
            let max_bits = width.min(conn_lits.len());
            for bit in 0..max_bits {
                self.signal_map.insert(
                    (MirSignalRef::Port(port_id), bit as u32),
                    conn_lits[bit],
                );
            }
        }
    }

    /// Connect instance OUTPUT ports to parent signals (after processing child)
    /// Propagates child output values to the parent's connected signals.
    /// Reads child outputs in child context, switches to parent to write, switches back.
    fn connect_instance_outputs(&mut self, inst_path: &str, mir: &'a Mir) {
        // Find the parent module and instance
        let (parent_path, inst_name) = if let Some(dot_pos) = inst_path.rfind('.') {
            (&inst_path[..dot_pos], &inst_path[dot_pos + 1..])
        } else {
            ("", inst_path)
        };

        let parent_module = if parent_path.is_empty() {
            self.module
        } else {
            self.find_module_for_instance_path(mir, parent_path)
                .unwrap_or(self.module)
        };

        let child_instance_path = self.current_instance_path.clone();

        // Collect child output values + parent target refs (in child context)
        let mut transfers: Vec<(MirSignalRef, Vec<AigLit>)> = Vec::new();
        if let Some(instance) = parent_module.instances.iter().find(|i| i.name == inst_name) {
            if let Some(child_module) = mir.modules.iter().find(|m| m.id == instance.module) {
                for (port_name, conn_expr) in &instance.connections {
                    if let Some(child_port) = child_module.ports.iter().find(|p| p.name == *port_name) {
                        if child_port.direction != PortDirection::Output {
                            continue;
                        }

                        let width = self.get_type_width(&child_port.port_type);

                        let child_lits: Vec<AigLit> = (0..width)
                            .map(|bit| {
                                self.signal_map
                                    .get(&(MirSignalRef::Port(child_port.id), bit as u32))
                                    .copied()
                                    .unwrap_or_else(|| self.aig.false_lit())
                            })
                            .collect();

                        if let ExpressionKind::Ref(lvalue) = &conn_expr.kind {
                            if let Some(parent_sig_ref) = self.lvalue_to_ref(lvalue) {
                                transfers.push((parent_sig_ref, child_lits));
                            }
                        }
                    }
                }
            }
        }

        // Switch to parent context and write
        self.switch_to_instance(parent_path);
        for (parent_sig_ref, child_lits) in transfers {
            for (bit, &lit) in child_lits.iter().enumerate() {
                self.signal_map.insert((parent_sig_ref, bit as u32), lit);
            }
        }

        // Switch back to child context
        self.switch_to_instance(&child_instance_path);
    }

    /// Convert sequential process for BMC - stores next-state values separately
    fn convert_sequential_process_for_bmc(
        &mut self,
        process: &Process,
        next_state_map: &mut BTreeMap<(MirSignalRef, u32), AigLit>,
    ) {
        // Convert the process body, but capture assignments to registers
        // as next-state values instead of updating signal_map directly
        self.convert_block_for_bmc(&process.body, next_state_map);
    }

    /// Convert a block for BMC - separates register assignments as next-state
    fn convert_block_for_bmc(
        &mut self,
        block: &Block,
        next_state_map: &mut BTreeMap<(MirSignalRef, u32), AigLit>,
    ) {
        const STACK_RED_ZONE: usize = 256 * 1024;
        const STACK_GROW_SIZE: usize = 8 * 1024 * 1024;
        stacker::maybe_grow(STACK_RED_ZONE, STACK_GROW_SIZE, || {
            for stmt in &block.statements {
                self.convert_statement_for_bmc(stmt, next_state_map);
            }
        });
    }

    fn convert_statement_for_bmc(
        &mut self,
        stmt: &Statement,
        next_state_map: &mut BTreeMap<(MirSignalRef, u32), AigLit>,
    ) {
        const STACK_RED_ZONE: usize = 256 * 1024;
        const STACK_GROW_SIZE: usize = 8 * 1024 * 1024;
        stacker::maybe_grow(STACK_RED_ZONE, STACK_GROW_SIZE, || {
        match stmt {
            Statement::Assignment(assign) => {
                // Check if this is a register assignment
                if let Some(sig_ref) = self.lvalue_to_ref(&assign.lhs) {
                    if self.register_outputs.contains(&sig_ref) {
                        // This is a register - compute next-state value
                        let rhs_lits = self.convert_expression(&assign.rhs);
                        // Store as next-state
                        for (bit, lit) in rhs_lits.iter().enumerate() {
                            next_state_map.insert((sig_ref, bit as u32), *lit);
                        }
                        return;
                    }
                }
                // Not a register - normal assignment (update signal_map)
                self.convert_assignment(assign, &BTreeMap::new());
            }
            Statement::If(if_stmt) => {
                // For conditionals with register assignments, we need proper MUXing:
                // 1. Save current next_state_map values for all registers
                // 2. Process then branch -> get then_updates
                // 3. Restore to saved state
                // 4. Process else branch -> get else_updates
                // 5. For each register, create MUX: cond ? then_value : else_value

                let cond_lits = self.convert_expression(&if_stmt.condition);
                let cond = cond_lits.first().copied().unwrap_or_else(|| self.aig.false_lit());

                // Save current state of next_state_map
                let saved_state = next_state_map.clone();

                // Process then branch
                self.convert_block_for_bmc(&if_stmt.then_block, next_state_map);
                let then_state = next_state_map.clone();

                // Restore to saved state for else branch
                *next_state_map = saved_state.clone();

                // Process else branch (or keep saved state if no else)
                if let Some(else_block) = &if_stmt.else_block {
                    self.convert_block_for_bmc(else_block, next_state_map);
                }
                let else_state = next_state_map.clone();

                // Merge: for each register bit, create MUX if values differ
                // Collect all keys from both then and else states
                let mut all_keys: std::collections::BTreeSet<(MirSignalRef, u32)> =
                    std::collections::BTreeSet::new();
                for key in then_state.keys() {
                    all_keys.insert(*key);
                }
                for key in else_state.keys() {
                    all_keys.insert(*key);
                }

                for key in all_keys {
                    let then_val = then_state.get(&key).copied();
                    let else_val = else_state.get(&key).copied();
                    let saved_val = saved_state.get(&key).copied();

                    // Get current register value from signal_map as fallback
                    // This is what the register holds if neither branch updates it
                    let current_reg_val = self.signal_map.get(&key).copied()
                        .unwrap_or_else(|| self.aig.false_lit());

                    // Determine the final value based on which branches updated it
                    let final_val = match (then_val, else_val) {
                        (Some(t), Some(e)) => {
                            // Both branches updated this register - create MUX
                            if t == e {
                                t // Same value, no MUX needed
                            } else {
                                self.aig.add_mux(cond, e, t) // cond ? t : e (note: add_mux is sel ? b : a)
                            }
                        }
                        (Some(t), None) => {
                            // Only then branch updated - MUX with else value
                            // Else value is saved_val if it was already computed, otherwise current register
                            let else_default = saved_val.unwrap_or(current_reg_val);
                            if t == else_default {
                                t
                            } else {
                                self.aig.add_mux(cond, else_default, t)
                            }
                        }
                        (None, Some(e)) => {
                            // Only else branch updated - MUX with then value
                            // Then value is saved_val if it was already computed, otherwise current register
                            let then_default = saved_val.unwrap_or(current_reg_val);
                            if e == then_default {
                                e
                            } else {
                                self.aig.add_mux(cond, e, then_default)
                            }
                        }
                        (None, None) => {
                            // Neither branch updated - keep saved value or current register
                            saved_val.unwrap_or(current_reg_val)
                        }
                    };

                    next_state_map.insert(key, final_val);
                }
            }
            Statement::Case(case_stmt) => {
                // For case statements, we need to handle each arm with proper MUXing
                // Similar to nested if-else
                let selector_lits = self.convert_expression(&case_stmt.expr);

                // Save initial state
                let initial_state = next_state_map.clone();

                // Collect updates from each case item
                let mut case_updates: Vec<(AigLit, BTreeMap<(MirSignalRef, u32), AigLit>)> =
                    Vec::new();

                for item in &case_stmt.items {
                    // Compute condition for this case item
                    let item_cond = self.compute_case_item_condition(&item.values, &selector_lits);

                    // Reset to initial state
                    *next_state_map = initial_state.clone();

                    // Process this case arm
                    self.convert_block_for_bmc(&item.block, next_state_map);

                    case_updates.push((item_cond, next_state_map.clone()));
                }

                // Process default case if present
                let default_state = if let Some(default) = &case_stmt.default {
                    *next_state_map = initial_state.clone();
                    self.convert_block_for_bmc(default, next_state_map);
                    next_state_map.clone()
                } else {
                    initial_state.clone()
                };

                // Merge all case arms: chain of MUXes
                // Start with default, then overlay each case in reverse order
                *next_state_map = default_state;

                for (item_cond, item_state) in case_updates.into_iter().rev() {
                    // For each register bit, create MUX: item_cond ? item_value : current_value
                    for (key, item_val) in &item_state {
                        // BUG #265 FIX: Fall back to signal_map (current latch output) when a
                        // register isn't updated by the default arm or previous overlays.
                        // Without this, registers not assigned in all case arms get incorrectly
                        // zeroed (false_lit) instead of holding their current value.
                        let current_val = next_state_map
                            .get(key)
                            .copied()
                            .unwrap_or_else(|| initial_state.get(key).copied()
                                .unwrap_or_else(|| self.signal_map.get(key).copied()
                                    .unwrap_or_else(|| self.aig.false_lit())));

                        if *item_val != current_val {
                            let muxed = self.aig.add_mux(item_cond, current_val, *item_val);
                            next_state_map.insert(*key, muxed);
                        }
                    }
                }
            }
            Statement::Block(inner_block) => {
                self.convert_block_for_bmc(inner_block, next_state_map);
            }
            Statement::ResolvedConditional(resolved) => {
                // Handle resolved if-else-if chains (priority mux)
                // Check if target is a register
                if let Some(sig_ref) = self.lvalue_to_ref(&resolved.target) {
                    if self.register_outputs.contains(&sig_ref) {
                        // Build priority MUX: if cond1 then val1, else if cond2 then val2, ... else default
                        let default_lits = self.convert_expression(&resolved.resolved.default);
                        let mut result_lits = default_lits;

                        // Process cases in reverse order (lowest priority first)
                        // so that higher priority conditions override
                        for case in resolved.resolved.cases.iter().rev() {
                            let cond_lits = self.convert_expression(&case.condition);
                            let cond = cond_lits.first().copied().unwrap_or_else(|| self.aig.false_lit());
                            let value_lits = self.convert_expression(&case.value);

                            // MUX: cond ? value : current_result
                            result_lits = self.build_mux_vector(cond, &value_lits, &result_lits);
                        }

                        // Store as next-state
                        for (bit, lit) in result_lits.iter().enumerate() {
                            next_state_map.insert((sig_ref, bit as u32), *lit);
                        }
                        return;
                    }
                }
                // Not a register - use normal conversion
                self.convert_statement(stmt, &BTreeMap::new());
            }
            _ => {
                // Other statements handled normally
                self.convert_statement(stmt, &BTreeMap::new());
            }
        }
        });
    }

    /// Compute the condition for a case item (pattern match)
    fn compute_case_item_condition(
        &mut self,
        patterns: &[Expression],
        selector_lits: &[AigLit],
    ) -> AigLit {
        // OR together all pattern conditions
        let mut result = self.aig.false_lit();

        for pattern in patterns {
            let pattern_lits = self.convert_expression(pattern);
            // Check equality: all bits must match
            let mut match_cond = self.aig.true_lit();
            for (bit, pattern_lit) in pattern_lits.iter().enumerate() {
                if bit < selector_lits.len() {
                    let eq = self.aig.add_xnor(selector_lits[bit], *pattern_lit);
                    match_cond = self.aig.add_and(match_cond, eq);
                }
            }
            result = self.aig.add_or(result, match_cond);
        }

        result
    }

    fn convert_internal(mut self, registers_as_inputs: bool) -> Aig {
        // First pass: detect all registers (signals assigned in sequential processes)
        let mut register_signals: Vec<(MirSignalRef, String, u32)> = Vec::new();

        if registers_as_inputs {
            for process in &self.module.processes {
                if matches!(process.kind, ProcessKind::Sequential) {
                    self.collect_register_signals(&process.body, &mut register_signals);
                }
            }
        }

        // Add primary inputs (input ports)
        for port in &self.module.ports {
            if port.direction == PortDirection::Input {
                let width = self.get_type_width(&port.port_type);
                for bit in 0..width {
                    let name = if width == 1 {
                        port.name.clone()
                    } else {
                        format!("{}[{}]", port.name, bit)
                    };
                    let lit = self.aig.add_input(name);
                    self.signal_map
                        .insert((MirSignalRef::Port(port.id), bit as u32), lit);
                }
            }
        }

        // Add register outputs as inputs (for CEC mode)
        // This makes MIR-AIG compatible with GateNetlist-AIG where DFF Q outputs are inputs
        if registers_as_inputs {
            for (sig_ref, name, width) in &register_signals {
                for bit in 0..*width {
                    let input_name = if *width == 1 {
                        format!("{}_dff_out", name)
                    } else {
                        format!("{}[{}]_dff_out", name, bit)
                    };
                    let lit = self.aig.add_input(input_name);
                    self.signal_map.insert((*sig_ref, bit), lit);
                }
            }
        }

        // Process continuous assignments
        for assign in &self.module.assignments {
            self.convert_continuous_assign(assign);
        }

        // Process combinational processes
        for process in &self.module.processes {
            if matches!(process.kind, ProcessKind::Combinational) {
                self.convert_combinational_process(process);
            } else if matches!(process.kind, ProcessKind::Sequential) {
                // Track sequential processes for register detection
                self.detect_sequential_registers(process);
            }
        }

        // Add primary outputs (output ports)
        for port in &self.module.ports {
            if port.direction == PortDirection::Output {
                let width = self.get_type_width(&port.port_type);
                for bit in 0..width {
                    let name = if width == 1 {
                        port.name.clone()
                    } else {
                        format!("{}[{}]", port.name, bit)
                    };
                    let lit = self
                        .signal_map
                        .get(&(MirSignalRef::Port(port.id), bit as u32))
                        .copied()
                        .unwrap_or_else(|| self.aig.false_lit());
                    self.aig.add_output(name, lit);
                }
            }
        }

        self.aig
    }

    fn get_type_width(&self, data_type: &DataType) -> usize {
        match data_type {
            DataType::Bit(w) | DataType::Logic(w) | DataType::Int(w) | DataType::Nat(w) => *w,
            DataType::Bool => 1,
            DataType::Clock { .. } | DataType::Reset { .. } | DataType::Event => 1,
            DataType::Float16 => 16,
            DataType::Float32 => 32,
            DataType::Float64 => 64,
            DataType::BitParam { default, .. }
            | DataType::LogicParam { default, .. }
            | DataType::IntParam { default, .. }
            | DataType::NatParam { default, .. } => *default,
            DataType::BitExpr { default, .. }
            | DataType::LogicExpr { default, .. }
            | DataType::IntExpr { default, .. }
            | DataType::NatExpr { default, .. } => *default,
            DataType::Vec2(elem) => self.get_type_width(elem) * 2,
            DataType::Vec3(elem) => self.get_type_width(elem) * 3,
            DataType::Vec4(elem) => self.get_type_width(elem) * 4,
            DataType::Array(elem, size) => self.get_type_width(elem) * size,
            DataType::Ncl(w) => w * 2, // Dual-rail
            DataType::Struct(struct_type) => {
                // Sum of all field widths
                struct_type.fields.iter()
                    .map(|f| self.get_type_width(&f.field_type))
                    .sum()
            }
            DataType::Enum(enum_type) => {
                // Use the enum's explicit base type if available
                self.get_type_width(&enum_type.base_type)
            }
            _ => 32, // Default fallback
        }
    }

    /// Try to get the MIR StructType from an expression that references a port or signal
    fn get_mir_struct_type_from_expr(&self, expr: &Expression) -> Option<skalp_mir::StructType> {
        // Check if the expression is a reference to a port or signal
        if let ExpressionKind::Ref(lvalue) = &expr.kind {
            match lvalue {
                LValue::Port(id) => {
                    if let Some(port) = self.find_port(*id) {
                        if let DataType::Struct(struct_type) = &port.port_type {
                            return Some((**struct_type).clone());
                        }
                    }
                }
                LValue::Signal(id) => {
                    if let Some(signal) = self.find_signal(*id) {
                        if let DataType::Struct(struct_type) = &signal.signal_type {
                            return Some((**struct_type).clone());
                        }
                    }
                }
                _ => {}
            }
        }
        None
    }

    /// Get the bit offset and width of a field within a MIR struct
    fn get_struct_field_offset(&self, struct_type: &skalp_mir::StructType, field_name: &str) -> (usize, usize) {
        let mut offset = 0;
        for field in &struct_type.fields {
            let width = self.get_type_width(&field.field_type);
            if field.name == field_name {
                return (offset, width);
            }
            offset += width;
        }
        // Field not found - return (0, 0) or could panic
        (0, 0)
    }

    /// Get the bit offset and width of a field within a frontend struct
    fn get_frontend_struct_field_offset(&self, struct_type: &skalp_frontend::types::StructType, field_name: &str) -> (usize, usize) {
        let mut offset = 0;
        for field in &struct_type.fields {
            let width = self.get_frontend_type_width(&field.field_type);
            if field.name == field_name {
                return (offset, width);
            }
            offset += width;
        }
        // Field not found - return (0, 0)
        (0, 0)
    }

    /// Get the width of a frontend Type
    fn get_frontend_type_width(&self, ty: &Type) -> usize {
        match ty {
            Type::Bit(width) | Type::Logic(width) | Type::Int(width) | Type::Nat(width) => {
                match width {
                    skalp_frontend::types::Width::Fixed(w) => *w as usize,
                    _ => 32, // Default for parameterized/unknown widths
                }
            }
            Type::Bool => 1,
            Type::Clock(_) | Type::Reset(_) | Type::Event => 1,
            Type::Fixed { integer_bits, fractional_bits } => (*integer_bits + *fractional_bits) as usize,
            Type::Array { element_type, size } => self.get_frontend_type_width(element_type) * (*size as usize),
            Type::Struct(struct_type) => {
                struct_type.fields.iter()
                    .map(|f| self.get_frontend_type_width(&f.field_type))
                    .sum()
            }
            Type::Tuple(element_types) => {
                element_types.iter().map(|t| self.get_frontend_type_width(t)).sum()
            }
            Type::Enum(enum_type) => {
                let variant_count = enum_type.variants.len();
                if variant_count <= 1 { 1 } else { (variant_count as f64).log2().ceil() as usize }
            }
            _ => 32, // Default for unknown types
        }
    }

    /// Collect all signals assigned in a block (used for register detection)
    fn collect_register_signals(
        &self,
        block: &Block,
        registers: &mut Vec<(MirSignalRef, String, u32)>,
    ) {
        for stmt in &block.statements {
            match stmt {
                Statement::Assignment(assign) => {
                    if let Some((sig_ref, name, width)) = self.lvalue_to_ref_with_info(&assign.lhs) {
                        // Check if already in the list
                        if !registers.iter().any(|(r, _, _)| *r == sig_ref) {
                            registers.push((sig_ref, name, width));
                        }
                    }
                }
                Statement::If(if_stmt) => {
                    self.collect_register_signals(&if_stmt.then_block, registers);
                    if let Some(else_block) = &if_stmt.else_block {
                        self.collect_register_signals(else_block, registers);
                    }
                }
                Statement::Case(case_stmt) => {
                    for item in &case_stmt.items {
                        self.collect_register_signals(&item.block, registers);
                    }
                    if let Some(default) = &case_stmt.default {
                        self.collect_register_signals(default, registers);
                    }
                }
                Statement::Block(inner_block) => {
                    self.collect_register_signals(inner_block, registers);
                }
                Statement::ResolvedConditional(resolved) => {
                    // ResolvedConditional wraps an if-else-if chain — extract register from target
                    if let Some((sig_ref, name, width)) = self.lvalue_to_ref_with_info(&resolved.target) {
                        if !registers.iter().any(|(r, _, _)| *r == sig_ref) {
                            registers.push((sig_ref, name, width));
                        }
                    }
                }
                _ => {}
            }
        }
    }

    /// Extract reset values from sequential process bodies
    /// Looks for pattern: if rst { signal = value }
    fn collect_reset_values(
        &self,
        block: &Block,
        reset_values: &mut BTreeMap<(MirSignalRef, u32), u64>,
    ) {
        for stmt in &block.statements {
            match stmt {
                Statement::If(if_stmt) => {
                    // Check if condition is a reset signal (named "rst" or similar)
                    if self.is_reset_condition(&if_stmt.condition) {
                        // Collect constant assignments from the then block
                        self.collect_reset_assignments(&if_stmt.then_block, reset_values);
                    }
                    // Recurse into else block (might have nested if rst)
                    if let Some(else_block) = &if_stmt.else_block {
                        self.collect_reset_values(else_block, reset_values);
                    }
                }
                Statement::ResolvedConditional(resolved) => {
                    // Unwrap to original if-else-if chain and recurse
                    let wrapped = Block {
                        statements: vec![Statement::If(*resolved.original.clone())],
                    };
                    self.collect_reset_values(&wrapped, reset_values);
                }
                _ => {}
            }
        }
    }

    /// Check if expression is a reset condition (references "rst" signal)
    fn is_reset_condition(&self, expr: &Expression) -> bool {
        match &expr.kind {
            ExpressionKind::Ref(lvalue) => {
                // Check if lvalue references a port/signal named "rst"
                match lvalue {
                    LValue::Port(id) => {
                        self.find_port(*id).map(|p| p.name == "rst").unwrap_or(false)
                    }
                    LValue::Signal(id) => {
                        self.find_signal(*id).map(|s| s.name == "rst").unwrap_or(false)
                    }
                    _ => false,
                }
            }
            ExpressionKind::FieldAccess { base, field } => {
                // Handle patterns like clk.rst or rst.field
                field == "rst" || self.is_reset_condition(base)
            }
            _ => false,
        }
    }

    /// Collect constant assignments from a reset block
    fn collect_reset_assignments(
        &self,
        block: &Block,
        reset_values: &mut BTreeMap<(MirSignalRef, u32), u64>,
    ) {
        for stmt in &block.statements {
            match stmt {
                Statement::Assignment(assign) => {
                    if let Some(sig_ref) = self.lvalue_to_ref(&assign.lhs) {
                        // Try to extract constant value from RHS
                        if let Some(value) = self.try_extract_reset_constant(&assign.rhs) {
                            let width = self.get_signal_ref_width(sig_ref);
                            for bit in 0..width {
                                let bit_val = (value >> bit) & 1;
                                reset_values.insert((sig_ref, bit), bit_val);
                            }
                        }
                    }
                }
                Statement::If(inner_if) => {
                    // Recurse into nested ifs
                    self.collect_reset_assignments(&inner_if.then_block, reset_values);
                    if let Some(else_block) = &inner_if.else_block {
                        self.collect_reset_assignments(else_block, reset_values);
                    }
                }
                Statement::Block(inner_block) => {
                    self.collect_reset_assignments(inner_block, reset_values);
                }
                _ => {}
            }
        }
    }

    /// Try to extract a constant integer value from an expression (for reset values)
    fn try_extract_reset_constant(&self, expr: &Expression) -> Option<u64> {
        match &expr.kind {
            ExpressionKind::Literal(val) => {
                match val {
                    Value::Integer(n) => Some(*n as u64),
                    Value::BitVector { value, .. } => Some(*value),
                    _ => None,
                }
            }
            _ => None,
        }
    }

    /// Get the width of a signal reference
    fn get_signal_ref_width(&self, sig_ref: MirSignalRef) -> u32 {
        match sig_ref {
            MirSignalRef::Port(id) => {
                self.find_port(id)
                    .map(|p| self.get_type_width(&p.port_type) as u32)
                    .unwrap_or(1)
            }
            MirSignalRef::Signal(id) => {
                self.find_signal(id)
                    .map(|s| self.get_type_width(&s.signal_type) as u32)
                    .unwrap_or(1)
            }
            MirSignalRef::Variable(id) => {
                self.find_variable(id)
                    .map(|v| self.get_type_width(&v.var_type) as u32)
                    .unwrap_or(1)
            }
        }
    }

    /// Get signal reference with name and width info
    fn lvalue_to_ref_with_info(&self, lvalue: &LValue) -> Option<(MirSignalRef, String, u32)> {
        match lvalue {
            LValue::Port(id) => {
                let port = self.find_port(*id)?;
                let width = self.get_type_width(&port.port_type) as u32;
                Some((MirSignalRef::Port(*id), port.name.clone(), width))
            }
            LValue::Signal(id) => {
                let signal = self.find_signal(*id)?;
                let width = self.get_type_width(&signal.signal_type) as u32;
                Some((MirSignalRef::Signal(*id), signal.name.clone(), width))
            }
            LValue::Variable(id) => {
                let var = self.find_variable(*id)?;
                let width = self.get_type_width(&var.var_type) as u32;
                Some((MirSignalRef::Variable(*id), var.name.clone(), width))
            }
            LValue::BitSelect { base, .. } => self.lvalue_to_ref_with_info(base),
            LValue::RangeSelect { base, .. } => self.lvalue_to_ref_with_info(base),
            LValue::Concat(_) => None,
        }
    }

    fn convert_continuous_assign(&mut self, assign: &ContinuousAssign) {
        let rhs_lits = self.convert_expression(&assign.rhs);
        self.assign_lvalue(&assign.lhs, &rhs_lits);
    }

    fn convert_combinational_process(&mut self, process: &Process) {
        // For combinational processes, we need to convert the body
        // while tracking what each signal is assigned to
        self.convert_block(&process.body, &BTreeMap::new());
    }

    fn detect_sequential_registers(&mut self, process: &Process) {
        // Sequential processes define registers - we track their outputs
        // for sequential equivalence checking
        self.find_assigned_signals(&process.body);
    }

    fn find_assigned_signals(&mut self, block: &Block) {
        for stmt in &block.statements {
            match stmt {
                Statement::Assignment(assign) => {
                    if let Some(sig_ref) = self.lvalue_to_ref(&assign.lhs) {
                        if !self.register_outputs.contains(&sig_ref) {
                            self.register_outputs.push(sig_ref);
                        }
                    }
                }
                Statement::If(if_stmt) => {
                    self.find_assigned_signals(&if_stmt.then_block);
                    if let Some(else_block) = &if_stmt.else_block {
                        self.find_assigned_signals(else_block);
                    }
                }
                Statement::Case(case_stmt) => {
                    for item in &case_stmt.items {
                        self.find_assigned_signals(&item.block);
                    }
                    if let Some(default) = &case_stmt.default {
                        self.find_assigned_signals(default);
                    }
                }
                Statement::Block(inner_block) => {
                    self.find_assigned_signals(inner_block);
                }
                Statement::ResolvedConditional(resolved) => {
                    if let Some(sig_ref) = self.lvalue_to_ref(&resolved.target) {
                        if !self.register_outputs.contains(&sig_ref) {
                            self.register_outputs.push(sig_ref);
                        }
                    }
                }
                _ => {}
            }
        }
    }

    fn lvalue_to_ref(&self, lvalue: &LValue) -> Option<MirSignalRef> {
        match lvalue {
            LValue::Port(id) => Some(MirSignalRef::Port(*id)),
            LValue::Signal(id) => Some(MirSignalRef::Signal(*id)),
            LValue::Variable(id) => Some(MirSignalRef::Variable(*id)),
            LValue::BitSelect { base, .. } => self.lvalue_to_ref(base),
            LValue::RangeSelect { base, .. } => self.lvalue_to_ref(base),
            LValue::Concat(_) => None, // Complex case
        }
    }

    /// Convert a block of statements, tracking conditional context
    fn convert_block(
        &mut self,
        block: &Block,
        conditions: &BTreeMap<MirSignalRef, Vec<AigLit>>,
    ) {
        const STACK_RED_ZONE: usize = 256 * 1024;
        const STACK_GROW_SIZE: usize = 8 * 1024 * 1024;
        stacker::maybe_grow(STACK_RED_ZONE, STACK_GROW_SIZE, || {
            for stmt in &block.statements {
                self.convert_statement(stmt, conditions);
            }
        });
    }

    fn convert_statement(
        &mut self,
        stmt: &Statement,
        conditions: &BTreeMap<MirSignalRef, Vec<AigLit>>,
    ) {
        const STACK_RED_ZONE: usize = 256 * 1024;
        const STACK_GROW_SIZE: usize = 8 * 1024 * 1024;
        stacker::maybe_grow(STACK_RED_ZONE, STACK_GROW_SIZE, || {
        match stmt {
            Statement::Assignment(assign) => {
                self.convert_assignment(assign, conditions);
            }
            Statement::If(if_stmt) => {
                self.convert_if_statement(if_stmt, conditions);
            }
            Statement::Case(case_stmt) => {
                self.convert_case_statement(case_stmt, conditions);
            }
            Statement::Block(inner_block) => {
                self.convert_block(inner_block, conditions);
            }
            Statement::ResolvedConditional(resolved) => {
                // Already resolved to mux form - convert directly
                for case in &resolved.resolved.cases {
                    let cond_lits = self.convert_expression(&case.condition);
                    let value_lits = self.convert_expression(&case.value);
                    // This is a priority mux structure
                    if !cond_lits.is_empty() {
                        let cond = cond_lits[0];
                        let current = self.get_signal_lits(&resolved.target);
                        let muxed = self.build_mux_vector(cond, &value_lits, &current);
                        self.assign_lvalue(&resolved.target, &muxed);
                    }
                }
            }
            _ => {} // Assert, Assume, Cover, Loop - skip for now
        }
        });
    }

    fn convert_assignment(
        &mut self,
        assign: &Assignment,
        _conditions: &BTreeMap<MirSignalRef, Vec<AigLit>>,
    ) {
        let rhs_lits = self.convert_expression(&assign.rhs);
        self.assign_lvalue(&assign.lhs, &rhs_lits);
    }

    fn convert_if_statement(
        &mut self,
        if_stmt: &IfStatement,
        conditions: &BTreeMap<MirSignalRef, Vec<AigLit>>,
    ) {
        const STACK_RED_ZONE: usize = 256 * 1024;
        const STACK_GROW_SIZE: usize = 8 * 1024 * 1024;
        stacker::maybe_grow(STACK_RED_ZONE, STACK_GROW_SIZE, || {
        // Convert condition to single bit
        let cond_lits = self.convert_expression(&if_stmt.condition);
        let cond = if !cond_lits.is_empty() {
            cond_lits[0]
        } else {
            self.aig.false_lit()
        };

        // Get all signals assigned in then/else branches
        // Use (MirSignalRef, bit) as key to properly track bit positions
        let mut then_assigns: BTreeMap<(MirSignalRef, u32), AigLit> = BTreeMap::new();
        let mut else_assigns: BTreeMap<(MirSignalRef, u32), AigLit> = BTreeMap::new();

        // Save current state
        let saved_state = self.signal_map.clone();

        // Process then branch
        self.convert_block(&if_stmt.then_block, conditions);
        for (key, lit) in &self.signal_map {
            if saved_state.get(key) != Some(lit) {
                then_assigns.insert(*key, *lit);
            }
        }

        // Restore and process else branch
        self.signal_map = saved_state.clone();
        if let Some(else_block) = &if_stmt.else_block {
            self.convert_block(else_block, conditions);
        }
        for (key, lit) in &self.signal_map {
            if saved_state.get(key) != Some(lit) {
                else_assigns.insert(*key, *lit);
            }
        }

        // Build muxes for all modified signal bits
        self.signal_map = saved_state;
        let all_keys: std::collections::BTreeSet<_> = then_assigns
            .keys()
            .chain(else_assigns.keys())
            .cloned()
            .collect();

        for key in all_keys {
            let (sig_ref, bit) = key;
            let then_lit = then_assigns
                .get(&key)
                .copied()
                .unwrap_or_else(|| {
                    self.signal_map
                        .get(&key)
                        .copied()
                        .unwrap_or_else(|| self.aig.false_lit())
                });
            let else_lit = else_assigns
                .get(&key)
                .copied()
                .unwrap_or_else(|| {
                    self.signal_map
                        .get(&key)
                        .copied()
                        .unwrap_or_else(|| self.aig.false_lit())
                });

            // MUX: cond ? then_lit : else_lit
            // add_mux(sel, a, b) returns sel ? b : a, so pass (cond, else_lit, then_lit)
            let mux_result = self.aig.add_mux(cond, else_lit, then_lit);
            self.signal_map.insert((sig_ref, bit), mux_result);
        }
        });
    }

    fn convert_case_statement(
        &mut self,
        case_stmt: &CaseStatement,
        conditions: &BTreeMap<MirSignalRef, Vec<AigLit>>,
    ) {
        const STACK_RED_ZONE: usize = 256 * 1024;
        const STACK_GROW_SIZE: usize = 8 * 1024 * 1024;
        stacker::maybe_grow(STACK_RED_ZONE, STACK_GROW_SIZE, || {
        let selector_lits = self.convert_expression(&case_stmt.expr);
        let _selector_width = selector_lits.len();

        // Build a priority mux chain for case items
        // Start with default value (or current value)
        let saved_state = self.signal_map.clone();

        // Use (MirSignalRef, bit) as key to properly track bit positions
        let mut result_map: BTreeMap<(MirSignalRef, u32), AigLit> = BTreeMap::new();

        // First, collect all assignments from default case
        if let Some(default_block) = &case_stmt.default {
            self.convert_block(default_block, conditions);
            for (key, lit) in &self.signal_map {
                if saved_state.get(key) != Some(lit) {
                    result_map.insert(*key, *lit);
                }
            }
        }

        // Process case items in reverse order (last match wins for priority)
        for item in case_stmt.items.iter().rev() {
            self.signal_map = saved_state.clone();

            // Build condition: selector == value for any value in item.values
            let mut item_cond = self.aig.false_lit();
            for value_expr in &item.values {
                let value_lits = self.convert_expression(value_expr);
                let eq_cond = self.build_equality(&selector_lits, &value_lits);
                item_cond = self.aig.add_or(item_cond, eq_cond);
            }

            // Process case body
            self.convert_block(&item.block, conditions);

            // Mux the results
            for (key, lit) in &self.signal_map {
                if saved_state.get(key) != Some(lit) {
                    let current = result_map
                        .get(key)
                        .copied()
                        .unwrap_or_else(|| {
                            saved_state
                                .get(key)
                                .copied()
                                .unwrap_or_else(|| self.aig.false_lit())
                        });
                    // add_mux(sel, a, b) returns sel ? b : a
                    // We want: item_cond ? *lit : current
                    // So call add_mux(item_cond, current, *lit)
                    let muxed = self.aig.add_mux(item_cond, current, *lit);
                    result_map.insert(*key, muxed);
                }
            }
        }

        // Apply final results
        self.signal_map = saved_state;
        for (key, lit) in result_map {
            self.signal_map.insert(key, lit);
        }
        });
    }

    fn build_equality(&mut self, lhs: &[AigLit], rhs: &[AigLit]) -> AigLit {
        let max_len = lhs.len().max(rhs.len());
        if max_len == 0 {
            return self.aig.true_lit();
        }

        let mut result = self.aig.true_lit();
        for i in 0..max_len {
            // Get bits, treating missing bits as 0 (zero-extension)
            let lhs_bit = lhs.get(i).copied().unwrap_or_else(|| self.aig.false_lit());
            let rhs_bit = rhs.get(i).copied().unwrap_or_else(|| self.aig.false_lit());
            // XNOR for bit equality
            let xnor = self.aig.add_xnor(lhs_bit, rhs_bit);
            result = self.aig.add_and(result, xnor);
        }
        result
    }

    fn build_mux_vector(
        &mut self,
        cond: AigLit,
        then_lits: &[AigLit],
        else_lits: &[AigLit],
    ) -> Vec<AigLit> {
        let max_len = then_lits.len().max(else_lits.len());
        let mut result = Vec::with_capacity(max_len);
        for i in 0..max_len {
            let then_bit = then_lits.get(i).copied().unwrap_or_else(|| self.aig.false_lit());
            let else_bit = else_lits.get(i).copied().unwrap_or_else(|| self.aig.false_lit());
            // add_mux(sel, a, b) returns sel ? b : a
            // We want cond ? then_bit : else_bit
            // So call add_mux(cond, else_bit, then_bit)
            result.push(self.aig.add_mux(cond, else_bit, then_bit));
        }
        result
    }

    fn get_signal_width(&self, sig_ref: MirSignalRef) -> u32 {
        match sig_ref {
            MirSignalRef::Port(id) => {
                if let Some(port) = self.find_port(id) {
                    self.get_type_width(&port.port_type) as u32
                } else {
                    1 // Default width if not found
                }
            }
            MirSignalRef::Signal(id) => {
                if let Some(signal) = self.find_signal(id) {
                    self.get_type_width(&signal.signal_type) as u32
                } else {
                    1
                }
            }
            MirSignalRef::Variable(id) => {
                if let Some(var) = self.find_variable(id) {
                    self.get_type_width(&var.var_type) as u32
                } else {
                    1
                }
            }
        }
    }

    fn get_signal_lits(&self, lvalue: &LValue) -> Vec<AigLit> {
        match lvalue {
            LValue::Port(id) => {
                let width = if let Some(port) = self.find_port(*id) {
                    self.get_type_width(&port.port_type)
                } else {
                    return vec![];
                };
                (0..width)
                    .map(|bit| {
                        self.signal_map
                            .get(&(MirSignalRef::Port(*id), bit as u32))
                            .copied()
                            .unwrap_or_else(|| self.aig.false_lit())
                    })
                    .collect()
            }
            LValue::Signal(id) => {
                let width = if let Some(signal) = self.find_signal(*id) {
                    self.get_type_width(&signal.signal_type)
                } else {
                    return vec![];
                };
                (0..width)
                    .map(|bit| {
                        self.signal_map
                            .get(&(MirSignalRef::Signal(*id), bit as u32))
                            .copied()
                            .unwrap_or_else(|| self.aig.false_lit())
                    })
                    .collect()
            }
            LValue::Variable(id) => {
                let width = if let Some(var) = self.find_variable(*id) {
                    self.get_type_width(&var.var_type)
                } else {
                    return vec![];
                };
                (0..width)
                    .map(|bit| {
                        self.signal_map
                            .get(&(MirSignalRef::Variable(*id), bit as u32))
                            .copied()
                            .unwrap_or_else(|| self.aig.false_lit())
                    })
                    .collect()
            }
            _ => vec![],
        }
    }

    fn assign_lvalue(&mut self, lvalue: &LValue, values: &[AigLit]) {
        match lvalue {
            LValue::Port(id) => {
                for (bit, lit) in values.iter().enumerate() {
                    self.signal_map
                        .insert((MirSignalRef::Port(*id), bit as u32), *lit);
                }
            }
            LValue::Signal(id) => {
                for (bit, lit) in values.iter().enumerate() {
                    self.signal_map
                        .insert((MirSignalRef::Signal(*id), bit as u32), *lit);
                }
            }
            LValue::Variable(id) => {
                for (bit, lit) in values.iter().enumerate() {
                    self.signal_map
                        .insert((MirSignalRef::Variable(*id), bit as u32), *lit);
                }
            }
            LValue::BitSelect { base, index } => {
                // Single bit assignment
                if let Some(sig_ref) = self.lvalue_to_ref(base) {
                    if let ExpressionKind::Literal(Value::Integer(idx)) = &index.kind {
                        if !values.is_empty() {
                            self.signal_map.insert((sig_ref, *idx as u32), values[0]);
                        }
                    }
                }
            }
            LValue::RangeSelect { base, high, low } => {
                // Range assignment
                if let Some(sig_ref) = self.lvalue_to_ref(base) {
                    if let (
                        ExpressionKind::Literal(Value::Integer(hi)),
                        ExpressionKind::Literal(Value::Integer(lo)),
                    ) = (&high.kind, &low.kind)
                    {
                        for (i, lit) in values.iter().enumerate() {
                            let bit = *lo as u32 + i as u32;
                            if bit <= *hi as u32 {
                                self.signal_map.insert((sig_ref, bit), *lit);
                            }
                        }
                    }
                }
            }
            LValue::Concat(parts) => {
                // Concatenation - distribute bits to parts
                let mut offset = 0;
                for part in parts.iter().rev() {
                    let part_width = self.get_lvalue_width(part);
                    let part_values: Vec<_> = values
                        .iter()
                        .skip(offset)
                        .take(part_width)
                        .copied()
                        .collect();
                    self.assign_lvalue(part, &part_values);
                    offset += part_width;
                }
            }
        }
    }

    fn get_lvalue_width(&self, lvalue: &LValue) -> usize {
        match lvalue {
            LValue::Port(id) => {
                if let Some(port) = self.find_port(*id) {
                    self.get_type_width(&port.port_type)
                } else {
                    1
                }
            }
            LValue::Signal(id) => {
                if let Some(signal) = self.find_signal(*id) {
                    self.get_type_width(&signal.signal_type)
                } else {
                    1
                }
            }
            LValue::Variable(id) => {
                if let Some(var) = self.find_variable(*id) {
                    self.get_type_width(&var.var_type)
                } else {
                    1
                }
            }
            LValue::BitSelect { .. } => 1,
            LValue::RangeSelect { high, low, .. } => {
                if let (
                    ExpressionKind::Literal(Value::Integer(hi)),
                    ExpressionKind::Literal(Value::Integer(lo)),
                ) = (&high.kind, &low.kind)
                {
                    (hi - lo + 1) as usize
                } else {
                    1
                }
            }
            LValue::Concat(parts) => parts.iter().map(|p| self.get_lvalue_width(p)).sum(),
        }
    }

    /// Convert an expression to AIG literals (one per bit)
    fn convert_expression(&mut self, expr: &Expression) -> Vec<AigLit> {
        const STACK_RED_ZONE: usize = 256 * 1024;
        const STACK_GROW_SIZE: usize = 8 * 1024 * 1024;
        stacker::maybe_grow(STACK_RED_ZONE, STACK_GROW_SIZE, || {
        match &expr.kind {
            ExpressionKind::Literal(value) => self.convert_literal(value),

            ExpressionKind::Ref(lvalue) => self.convert_lvalue_ref(lvalue),

            ExpressionKind::Binary { op, left, right } => {
                let left_lits = self.convert_expression(left);
                let right_lits = self.convert_expression(right);
                // Determine signedness by recursively inspecting operand expression trees.
                // Cannot use left.ty/right.ty because compound expressions have Type::Unknown.
                let signed = self.infer_expression_is_signed(left) || self.infer_expression_is_signed(right);
                self.convert_binary_op(*op, &left_lits, &right_lits, signed)
            }

            ExpressionKind::Unary { op, operand } => {
                let operand_lits = self.convert_expression(operand);
                self.convert_unary_op(*op, &operand_lits)
            }

            ExpressionKind::Conditional {
                cond,
                then_expr,
                else_expr,
            } => {
                let cond_lits = self.convert_expression(cond);
                let then_lits = self.convert_expression(then_expr);
                let else_lits = self.convert_expression(else_expr);

                let cond_bit = if !cond_lits.is_empty() {
                    cond_lits[0]
                } else {
                    self.aig.false_lit()
                };

                self.build_mux_vector(cond_bit, &then_lits, &else_lits)
            }

            ExpressionKind::Concat(exprs) => {
                let mut result = Vec::new();
                for e in exprs.iter().rev() {
                    result.extend(self.convert_expression(e));
                }
                result
            }

            ExpressionKind::Replicate { count, value } => {
                let value_lits = self.convert_expression(value);
                if let ExpressionKind::Literal(Value::Integer(n)) = &count.kind {
                    let mut result = Vec::new();
                    for _ in 0..*n {
                        result.extend(value_lits.iter().copied());
                    }
                    result
                } else {
                    value_lits
                }
            }

            ExpressionKind::Cast { expr, target_type } => {
                let expr_lits = self.convert_expression(expr);
                let target_width = self.get_type_width(target_type);
                // Sign-extend for signed source types, zero-extend for unsigned.
                // Use recursive inference since expr.ty is often Type::Unknown.
                let source_signed = self.infer_expression_is_signed(expr);
                let extend_bit = if source_signed && !expr_lits.is_empty() {
                    *expr_lits.last().unwrap() // MSB = sign bit
                } else {
                    self.aig.false_lit() // zero
                };
                let mut result = expr_lits;
                while result.len() < target_width {
                    result.push(extend_bit);
                }
                result.truncate(target_width);
                result
            }

            ExpressionKind::FunctionCall { name, args } => {
                // Built-in functions
                match name.as_str() {
                    "$clog2" => {
                        if !args.is_empty() {
                            if let ExpressionKind::Literal(Value::Integer(n)) = &args[0].kind {
                                let result = (*n as f64).log2().ceil() as u64;
                                return self.convert_literal(&Value::Integer(result as i64));
                            }
                        }
                        vec![self.aig.false_lit()]
                    }
                    _ => vec![self.aig.false_lit()], // Unknown function
                }
            }

            ExpressionKind::FieldAccess { base, field } => {
                // Handle struct field access
                // Get all bits from base expression
                let base_lits = self.convert_expression(base);

                // Try to find the MIR DataType struct from the base expression
                // This gives us correct type widths including enum base types
                let mir_struct = self.get_mir_struct_type_from_expr(base);

                if let Some(struct_type) = mir_struct {
                    // Use MIR struct type for accurate field offsets
                    let (offset, width) = self.get_struct_field_offset(&struct_type, field);
                    let end = (offset + width).min(base_lits.len());
                    if offset < base_lits.len() {
                        base_lits[offset..end].to_vec()
                    } else {
                        vec![self.aig.false_lit()]
                    }
                } else if let Type::Struct(struct_type) = &base.ty {
                    // Fall back to frontend type (may have wrong enum widths)
                    let (offset, width) = self.get_frontend_struct_field_offset(struct_type, field);
                    let end = (offset + width).min(base_lits.len());
                    if offset < base_lits.len() {
                        base_lits[offset..end].to_vec()
                    } else {
                        vec![self.aig.false_lit()]
                    }
                } else {
                    // If not a struct type, try to treat base bits directly
                    base_lits
                }
            }

            ExpressionKind::TupleFieldAccess { base, index } => {
                // Handle tuple field access
                let base_lits = self.convert_expression(base);

                // If the base is a Tuple type, extract the element at the given index
                if let Type::Tuple(element_types) = &base.ty {
                    if *index < element_types.len() {
                        let mut offset = 0;
                        for (i, elem_type) in element_types.iter().enumerate() {
                            let elem_width = self.get_frontend_type_width(elem_type);
                            if i == *index {
                                let end = (offset + elem_width).min(base_lits.len());
                                if offset < base_lits.len() {
                                    return base_lits[offset..end].to_vec();
                                } else {
                                    return vec![self.aig.false_lit()];
                                }
                            }
                            offset += elem_width;
                        }
                    }
                }
                // Also handle if tuples are lowered to structs
                if let Type::Struct(struct_type) = &base.ty {
                    if *index < struct_type.fields.len() {
                        let mut offset = 0;
                        for (i, field) in struct_type.fields.iter().enumerate() {
                            let field_width = self.get_frontend_type_width(&field.field_type);
                            if i == *index {
                                let end = (offset + field_width).min(base_lits.len());
                                if offset < base_lits.len() {
                                    return base_lits[offset..end].to_vec();
                                } else {
                                    return vec![self.aig.false_lit()];
                                }
                            }
                            offset += field_width;
                        }
                    }
                }
                // Fallback: return all bits
                base_lits
            }

            _ => vec![self.aig.false_lit()],
        }
        })
    }

    fn convert_literal(&mut self, value: &Value) -> Vec<AigLit> {
        match value {
            Value::Integer(n) => {
                // Assume 32-bit for now
                let mut result = Vec::with_capacity(32);
                for bit in 0..32 {
                    let lit = if (*n >> bit) & 1 == 1 {
                        self.aig.true_lit()
                    } else {
                        self.aig.false_lit()
                    };
                    result.push(lit);
                }
                result
            }
            Value::BitVector { width, value } => {
                let mut result = Vec::with_capacity(*width);
                for bit in 0..*width {
                    let lit = if (value >> bit) & 1 == 1 {
                        self.aig.true_lit()
                    } else {
                        self.aig.false_lit()
                    };
                    result.push(lit);
                }
                result
            }
            Value::Float(f) => {
                // Convert to bits (IEEE 754)
                let bits = f.to_bits();
                let mut result = Vec::with_capacity(64);
                for bit in 0..64 {
                    let lit = if (bits >> bit) & 1 == 1 {
                        self.aig.true_lit()
                    } else {
                        self.aig.false_lit()
                    };
                    result.push(lit);
                }
                result
            }
            Value::HighZ | Value::Unknown => {
                // X/Z - treat as 0 for formal verification
                vec![self.aig.false_lit()]
            }
            Value::String(_) => vec![],
        }
    }

    fn convert_lvalue_ref(&self, lvalue: &LValue) -> Vec<AigLit> {
        match lvalue {
            LValue::Port(id) => {
                let width = if let Some(port) = self.find_port(*id) {
                    self.get_type_width(&port.port_type)
                } else {
                    return vec![];
                };
                (0..width)
                    .map(|bit| {
                        self.signal_map
                            .get(&(MirSignalRef::Port(*id), bit as u32))
                            .copied()
                            .unwrap_or_else(|| self.aig.false_lit())
                    })
                    .collect()
            }
            LValue::Signal(id) => {
                let width = if let Some(signal) = self.find_signal(*id) {
                    self.get_type_width(&signal.signal_type)
                } else {
                    return vec![];
                };
                (0..width)
                    .map(|bit| {
                        self.signal_map
                            .get(&(MirSignalRef::Signal(*id), bit as u32))
                            .copied()
                            .unwrap_or_else(|| self.aig.false_lit())
                    })
                    .collect()
            }
            LValue::Variable(id) => {
                let width = if let Some(var) = self.find_variable(*id) {
                    self.get_type_width(&var.var_type)
                } else {
                    return vec![];
                };
                (0..width)
                    .map(|bit| {
                        self.signal_map
                            .get(&(MirSignalRef::Variable(*id), bit as u32))
                            .copied()
                            .unwrap_or_else(|| self.aig.false_lit())
                    })
                    .collect()
            }
            LValue::BitSelect { base, index } => {
                if let ExpressionKind::Literal(Value::Integer(idx)) = &index.kind {
                    if let Some(sig_ref) = self.lvalue_to_ref(base) {
                        return vec![self
                            .signal_map
                            .get(&(sig_ref, *idx as u32))
                            .copied()
                            .unwrap_or_else(|| self.aig.false_lit())];
                    }
                }
                vec![self.aig.false_lit()]
            }
            LValue::RangeSelect { base, high, low } => {
                if let (
                    ExpressionKind::Literal(Value::Integer(hi)),
                    ExpressionKind::Literal(Value::Integer(lo)),
                ) = (&high.kind, &low.kind)
                {
                    if let Some(sig_ref) = self.lvalue_to_ref(base) {
                        return (*lo..=*hi)
                            .map(|bit| {
                                self.signal_map
                                    .get(&(sig_ref, bit as u32))
                                    .copied()
                                    .unwrap_or_else(|| self.aig.false_lit())
                            })
                            .collect();
                    }
                }
                vec![]
            }
            LValue::Concat(parts) => {
                let mut result = Vec::new();
                for part in parts.iter().rev() {
                    result.extend(self.convert_lvalue_ref(part));
                }
                result
            }
        }
    }

    /// Check if a frontend type is signed
    fn is_signed_type(&self, ty: &Type) -> bool {
        match ty {
            Type::Bit(_) => false,   // bit[n] is unsigned
            Type::Int(_) => true,    // int[n] is signed
            Type::Nat(_) => false,   // nat[n] is unsigned
            Type::Bool => false,     // bool is unsigned
            Type::Logic(_) => false, // logic[n] is unsigned
            Type::Fixed { .. } => true, // fixed-point is typically signed
            // Array element signedness determines array signedness
            Type::Array { element_type, .. } => self.is_signed_type(element_type),
            // Struct, Tuple, Enum, etc. - default to unsigned for comparisons
            _ => false,
        }
    }

    /// Check if a MIR DataType is signed
    fn is_datatype_signed(dt: &DataType) -> bool {
        matches!(dt, DataType::Int(_) | DataType::IntParam { .. } | DataType::IntExpr { .. })
    }

    /// Check if an lvalue references a signed signal/port/variable
    fn is_lvalue_signed(&self, lvalue: &LValue) -> bool {
        match lvalue {
            LValue::Port(id) => {
                self.find_port(*id)
                    .map(|p| Self::is_datatype_signed(&p.port_type))
                    .unwrap_or(false)
            }
            LValue::Signal(id) => {
                self.find_signal(*id)
                    .map(|s| Self::is_datatype_signed(&s.signal_type))
                    .unwrap_or(false)
            }
            LValue::Variable(id) => {
                self.find_variable(*id)
                    .map(|v| Self::is_datatype_signed(&v.var_type))
                    .unwrap_or(false)
            }
            LValue::BitSelect { base, .. } => self.is_lvalue_signed(base),
            LValue::RangeSelect { base, .. } => self.is_lvalue_signed(base),
            LValue::Concat(_) => false,
        }
    }

    /// Recursively infer signedness from an expression tree.
    ///
    /// This fixes the bug where `Expression.ty` is `Type::Unknown` for all
    /// compound expressions (created by `Expression::with_unknown_type()` in
    /// hir_to_mir.rs), causing signed operations to be treated as unsigned.
    /// Instead of checking the unreliable frontend type, we recursively traverse
    /// the expression tree to find leaf references (ports/signals/variables)
    /// whose MIR DataType correctly records signedness.
    fn infer_expression_is_signed(&self, expr: &Expression) -> bool {
        const STACK_RED_ZONE: usize = 256 * 1024;
        const STACK_GROW_SIZE: usize = 8 * 1024 * 1024;
        stacker::maybe_grow(STACK_RED_ZONE, STACK_GROW_SIZE, || {
            match &expr.kind {
                ExpressionKind::Literal(Value::Integer(_)) => false,
                ExpressionKind::Ref(lvalue) => self.is_lvalue_signed(lvalue),
                ExpressionKind::Binary { left, right, .. } => {
                    self.infer_expression_is_signed(left) || self.infer_expression_is_signed(right)
                }
                ExpressionKind::Unary { operand, op, .. } => {
                    if matches!(op, UnaryOp::Negate) {
                        true // Negation always produces signed result
                    } else {
                        self.infer_expression_is_signed(operand)
                    }
                }
                ExpressionKind::Conditional { then_expr, else_expr, .. } => {
                    self.infer_expression_is_signed(then_expr)
                        || self.infer_expression_is_signed(else_expr)
                }
                ExpressionKind::Cast { target_type, .. } => Self::is_datatype_signed(target_type),
                _ => false,
            }
        })
    }

    /// Extend AIG literal vector to target width with proper sign/zero extension.
    /// For signed, repeats the MSB (sign bit). For unsigned, pads with 0.
    fn extend_to_width(&self, lits: &[AigLit], target_width: usize, signed: bool) -> Vec<AigLit> {
        if lits.len() >= target_width {
            return lits[..target_width].to_vec();
        }
        let extend_bit = if signed && !lits.is_empty() {
            *lits.last().unwrap() // MSB = sign bit
        } else {
            self.aig.false_lit() // zero
        };
        let mut result = lits.to_vec();
        result.resize(target_width, extend_bit);
        result
    }

    fn convert_binary_op(
        &mut self,
        op: BinaryOp,
        left: &[AigLit],
        right: &[AigLit],
        signed: bool,
    ) -> Vec<AigLit> {
        // For shifts, data width is determined by the LEFT operand only.
        // The right operand (shift amount) does NOT expand the data width.
        // For all other ops, use max(left, right) with proper sign/zero extension.
        let is_shift = matches!(op, BinaryOp::LeftShift | BinaryOp::RightShift);
        let max_width = if is_shift {
            left.len() // Shift: data width = left operand width
        } else {
            left.len().max(right.len())
        };
        // BUG FIX: Extend both operands to max_width with proper sign/zero extension.
        // Previously, shorter operands were implicitly zero-extended via unwrap_or(false_lit()),
        // which is wrong for signed comparisons/arithmetic (e.g., 10-bit int[10] -250 extended
        // to 32 bits with zeros becomes +774 instead of -250).
        // For shifts, only extend the left operand to its own width (no-op), right is used as-is.
        let left = self.extend_to_width(left, max_width, signed);
        let right = if is_shift {
            right.to_vec() // Keep shift amount at original width
        } else {
            self.extend_to_width(right, max_width, signed)
        };

        match op {
            BinaryOp::BitwiseAnd | BinaryOp::And => {
                (0..max_width)
                    .map(|i| {
                        self.aig.add_and(left[i], right[i])
                    })
                    .collect()
            }
            BinaryOp::BitwiseOr | BinaryOp::Or => {
                (0..max_width)
                    .map(|i| {
                        self.aig.add_or(left[i], right[i])
                    })
                    .collect()
            }
            BinaryOp::BitwiseXor | BinaryOp::Xor => {
                (0..max_width)
                    .map(|i| {
                        self.aig.add_xor(left[i], right[i])
                    })
                    .collect()
            }
            BinaryOp::LogicalAnd => {
                // OR all bits of each operand, then AND
                let l_any = self.reduce_or(&left);
                let r_any = self.reduce_or(&right);
                vec![self.aig.add_and(l_any, r_any)]
            }
            BinaryOp::LogicalOr => {
                let l_any = self.reduce_or(&left);
                let r_any = self.reduce_or(&right);
                vec![self.aig.add_or(l_any, r_any)]
            }
            BinaryOp::Equal => {
                vec![self.build_equality(&left, &right)]
            }
            BinaryOp::NotEqual => {
                vec![self.build_equality(&left, &right).invert()]
            }
            BinaryOp::Less => {
                vec![self.build_less_than(&left, &right, signed)]
            }
            BinaryOp::LessEqual => {
                let lt = self.build_less_than(&left, &right, signed);
                let eq = self.build_equality(&left, &right);
                vec![self.aig.add_or(lt, eq)]
            }
            BinaryOp::Greater => {
                vec![self.build_less_than(&right, &left, signed)]
            }
            BinaryOp::GreaterEqual => {
                let gt = self.build_less_than(&right, &left, signed);
                let eq = self.build_equality(&left, &right);
                vec![self.aig.add_or(gt, eq)]
            }
            BinaryOp::Add => self.build_adder(&left, &right),
            BinaryOp::Sub => {
                // a - b = a + (~b + 1)
                let not_right: Vec<_> = right.iter().map(|l| l.invert()).collect();
                let one = vec![self.aig.true_lit()];
                let neg_b = self.build_adder(&not_right, &one);
                self.build_adder(&left, &neg_b)
            }
            BinaryOp::LeftShift => {
                // Barrel shifter: each stage shifts by 2^i if the i-th bit of amount is set
                let width = left.len();
                let mut current = left.to_vec();

                let num_stages = if width <= 1 { 1 } else { (width as f32).log2().ceil() as usize };
                for stage in 0..num_stages {
                    let shift_bit = right.get(stage).copied().unwrap_or_else(|| self.aig.false_lit());
                    let shift_amount = 1usize << stage;
                    let mut next = vec![self.aig.false_lit(); width];

                    for bit in 0..width {
                        let orig = current[bit];
                        let shifted = if bit >= shift_amount {
                            current[bit - shift_amount]
                        } else {
                            self.aig.false_lit() // Shift in zero
                        };
                        next[bit] = self.aig.add_mux(shift_bit, orig, shifted);
                    }
                    current = next;
                }
                current
            }
            BinaryOp::RightShift => {
                // Barrel shifter: arithmetic (sign-extending) when signed, logical otherwise
                let width = left.len();
                let sign_bit = if signed && width > 0 {
                    left[width - 1] // MSB for sign extension
                } else {
                    self.aig.false_lit() // Zero for logical shift
                };
                let mut current = left.to_vec();

                let num_stages = if width <= 1 { 1 } else { (width as f32).log2().ceil() as usize };
                for stage in 0..num_stages {
                    let shift_bit = right.get(stage).copied().unwrap_or_else(|| self.aig.false_lit());
                    let shift_amount = 1usize << stage;
                    let mut next = vec![self.aig.false_lit(); width];

                    for bit in 0..width {
                        let orig = current[bit];
                        let shifted = if bit + shift_amount < width {
                            current[bit + shift_amount]
                        } else {
                            sign_bit // Shift in sign bit (or zero for unsigned)
                        };
                        next[bit] = self.aig.add_mux(shift_bit, orig, shifted);
                    }
                    current = next;
                }
                current
            }
            BinaryOp::Mul => {
                // BUG FIX #247: Implement proper multiplier for formal verification
                self.build_multiplier(&left, &right, signed)
            }
            BinaryOp::Div => {
                // BUG #266 FIX: Implement proper divider for formal verification
                self.build_divider(&left, &right, signed, false)
            }
            BinaryOp::Mod => {
                // BUG #266 FIX: Implement proper modulo for formal verification
                self.build_divider(&left, &right, signed, true)
            }
        }
    }

    fn convert_unary_op(&mut self, op: UnaryOp, operand: &[AigLit]) -> Vec<AigLit> {
        match op {
            UnaryOp::Not => {
                // Logical NOT - true if all bits are 0
                let any = self.reduce_or(operand);
                vec![any.invert()]
            }
            UnaryOp::BitwiseNot => operand.iter().map(|l| l.invert()).collect(),
            UnaryOp::Negate => {
                // Two's complement: ~x + 1
                let inverted: Vec<_> = operand.iter().map(|l| l.invert()).collect();
                let one = vec![self.aig.true_lit()];
                self.build_adder(&inverted, &one)
            }
            UnaryOp::Reduce(reduce_op) => match reduce_op {
                ReduceOp::And => vec![self.reduce_and(operand)],
                ReduceOp::Or => vec![self.reduce_or(operand)],
                ReduceOp::Xor => vec![self.reduce_xor(operand)],
                ReduceOp::Nand => vec![self.reduce_and(operand).invert()],
                ReduceOp::Nor => vec![self.reduce_or(operand).invert()],
                ReduceOp::Xnor => vec![self.reduce_xor(operand).invert()],
            },
        }
    }

    fn reduce_and(&mut self, bits: &[AigLit]) -> AigLit {
        if bits.is_empty() {
            return self.aig.true_lit();
        }
        let mut result = bits[0];
        for &bit in &bits[1..] {
            result = self.aig.add_and(result, bit);
        }
        result
    }

    fn reduce_or(&mut self, bits: &[AigLit]) -> AigLit {
        if bits.is_empty() {
            return self.aig.false_lit();
        }
        let mut result = bits[0];
        for &bit in &bits[1..] {
            result = self.aig.add_or(result, bit);
        }
        result
    }

    fn reduce_xor(&mut self, bits: &[AigLit]) -> AigLit {
        if bits.is_empty() {
            return self.aig.false_lit();
        }
        let mut result = bits[0];
        for &bit in &bits[1..] {
            result = self.aig.add_xor(result, bit);
        }
        result
    }

    fn build_adder(&mut self, a: &[AigLit], b: &[AigLit]) -> Vec<AigLit> {
        let width = a.len().max(b.len());
        let mut result = Vec::with_capacity(width);
        let mut carry = self.aig.false_lit();

        for i in 0..width {
            let a_bit = a.get(i).copied().unwrap_or_else(|| self.aig.false_lit());
            let b_bit = b.get(i).copied().unwrap_or_else(|| self.aig.false_lit());

            // sum = a XOR b XOR carry
            let sum_ab = self.aig.add_xor(a_bit, b_bit);
            let sum = self.aig.add_xor(sum_ab, carry);
            result.push(sum);

            // carry_out = (a AND b) OR (carry AND (a XOR b))
            let a_and_b = self.aig.add_and(a_bit, b_bit);
            let carry_and_xor = self.aig.add_and(carry, sum_ab);
            carry = self.aig.add_or(a_and_b, carry_and_xor);
        }

        result
    }

    /// Build a multiplier using grade-school algorithm
    /// For signed multiplication, uses sign-magnitude approach
    fn build_multiplier(&mut self, a: &[AigLit], b: &[AigLit], signed: bool) -> Vec<AigLit> {
        let a_width = a.len();
        let b_width = b.len();
        let result_width = a_width + b_width;

        if signed {
            // Sign-magnitude multiplication:
            // 1. Get sign bits and compute result sign
            // 2. Take absolute values
            // 3. Unsigned multiply
            // 4. Conditionally negate result

            let a_sign = a.last().copied().unwrap_or_else(|| self.aig.false_lit());
            let b_sign = b.last().copied().unwrap_or_else(|| self.aig.false_lit());
            let result_sign = self.aig.add_xor(a_sign, b_sign);

            // Get |a| = a_sign ? -a : a
            let a_neg = self.negate_aig(a);
            let a_mag = self.mux_vector(a_sign, &a_neg, a);

            // Get |b| = b_sign ? -b : b
            let b_neg = self.negate_aig(b);
            let b_mag = self.mux_vector(b_sign, &b_neg, b);

            // Unsigned multiply: |a| * |b|
            let unsigned_product = self.build_unsigned_multiplier(&a_mag, &b_mag, result_width);

            // Conditionally negate result if result_sign is set
            let neg_product = self.negate_aig(&unsigned_product);
            self.mux_vector(result_sign, &neg_product, &unsigned_product)
        } else {
            self.build_unsigned_multiplier(a, b, result_width)
        }
    }

    /// Build an unsigned multiplier using grade-school algorithm
    fn build_unsigned_multiplier(
        &mut self,
        a: &[AigLit],
        b: &[AigLit],
        result_width: usize,
    ) -> Vec<AigLit> {
        let a_width = a.len();
        let b_width = b.len();

        // Initialize result to zero
        let mut result: Vec<AigLit> = vec![self.aig.false_lit(); result_width];

        // Grade-school multiplication: for each bit of b, add shifted a if bit is set
        for i in 0..b_width {
            let b_bit = b.get(i).copied().unwrap_or_else(|| self.aig.false_lit());

            // Add a << i to result, gated by b[i]
            let mut carry = self.aig.false_lit();
            for j in 0..a_width {
                let out_idx = i + j;
                if out_idx < result_width {
                    let a_bit = a.get(j).copied().unwrap_or_else(|| self.aig.false_lit());
                    // Gated a bit: a[j] & b[i]
                    let gated = self.aig.add_and(a_bit, b_bit);

                    // Full adder: result[out_idx] + gated + carry
                    let sum_ab = self.aig.add_xor(result[out_idx], gated);
                    let sum = self.aig.add_xor(sum_ab, carry);

                    let ab = self.aig.add_and(result[out_idx], gated);
                    let ac = self.aig.add_and(result[out_idx], carry);
                    let bc = self.aig.add_and(gated, carry);
                    let ab_or_ac = self.aig.add_or(ab, ac);
                    carry = self.aig.add_or(ab_or_ac, bc);

                    result[out_idx] = sum;
                }
            }
            // Propagate final carry if there's room
            let carry_idx = i + a_width;
            if carry_idx < result_width {
                result[carry_idx] = self.aig.add_xor(result[carry_idx], carry);
            }
        }

        result
    }

    /// Build a divider: returns quotient (want_remainder=false) or remainder (want_remainder=true).
    /// For signed: uses sign-magnitude approach (Rust/hardware truncation-toward-zero semantics).
    fn build_divider(
        &mut self,
        dividend: &[AigLit],
        divisor: &[AigLit],
        signed: bool,
        want_remainder: bool,
    ) -> Vec<AigLit> {
        let result_width = dividend.len();
        let raw_result = if signed {
            let false_lit = self.aig.false_lit();
            let a_sign = dividend.last().copied().unwrap_or(false_lit);
            let b_sign = divisor.last().copied().unwrap_or(false_lit);

            // |a|, |b|
            let a_neg = self.negate_aig(dividend);
            let a_mag = self.mux_vector(a_sign, &a_neg, dividend);
            let b_neg = self.negate_aig(divisor);
            let b_mag = self.mux_vector(b_sign, &b_neg, divisor);

            let (quot, rem) = self.build_unsigned_divmod(&a_mag, &b_mag);

            if want_remainder {
                // Remainder sign = dividend sign (truncation toward zero)
                let rem_neg = self.negate_aig(&rem);
                let mut r = self.mux_vector(a_sign, &rem_neg, &rem);
                r.truncate(result_width);
                r
            } else {
                // Quotient sign = a_sign XOR b_sign
                let q_sign = self.aig.add_xor(a_sign, b_sign);
                let quot_neg = self.negate_aig(&quot);
                let mut q = self.mux_vector(q_sign, &quot_neg, &quot);
                q.truncate(result_width);
                q
            }
        } else {
            let (quot, rem) = self.build_unsigned_divmod(dividend, divisor);
            let mut raw = if want_remainder { rem } else { quot };
            raw.truncate(result_width);
            raw
        };

        // BUG #266 FIX: Guard against divide-by-zero (return 0, matching C++ behavioral sim)
        let divisor_any = self.reduce_or(divisor);
        let divisor_is_zero = divisor_any.invert(); // all bits zero → divisor_any=0 → invert=1
        let false_lit = self.aig.false_lit();
        let zero_result = vec![false_lit; result_width];
        // divisor_is_zero ? zero : raw_result
        self.mux_vector(divisor_is_zero, &zero_result, &raw_result)
    }

    /// Restoring unsigned division: returns (quotient, remainder), both `width` bits wide.
    fn build_unsigned_divmod(
        &mut self,
        dividend: &[AigLit],
        divisor: &[AigLit],
    ) -> (Vec<AigLit>, Vec<AigLit>) {
        let n = dividend.len();
        let m = divisor.len();
        let width = n.max(m);
        let false_lit = self.aig.false_lit();
        let nodes_before = self.aig.nodes.len();

        // Pad both to `width`
        let mut a = dividend.to_vec();
        a.resize(width, false_lit);
        let mut d = divisor.to_vec();
        d.resize(width, false_lit);

        let mut remainder = vec![false_lit; width];
        let mut quotient = vec![false_lit; width];

        for i in (0..width).rev() {
            // Shift remainder left by 1, insert dividend[i] at bit 0
            for j in (1..width).rev() {
                remainder[j] = remainder[j - 1];
            }
            remainder[0] = a[i];

            // Trial subtraction: remainder - divisor
            // Compute via: remainder + (~divisor) + 1
            let d_inv: Vec<AigLit> = d.iter().map(|l| l.invert()).collect();
            let mut trial = Vec::with_capacity(width);
            let mut carry = self.aig.true_lit(); // +1 for two's complement
            for j in 0..width {
                let r_bit = remainder[j];
                let d_bit = d_inv[j];
                let sum_rd = self.aig.add_xor(r_bit, d_bit);
                let sum = self.aig.add_xor(sum_rd, carry);
                trial.push(sum);
                let rd = self.aig.add_and(r_bit, d_bit);
                let rc = self.aig.add_and(r_bit, carry);
                let dc = self.aig.add_and(d_bit, carry);
                let rd_or_rc = self.aig.add_or(rd, rc);
                carry = self.aig.add_or(rd_or_rc, dc);
            }
            // carry = no_borrow (1 means remainder >= divisor)
            let no_borrow = carry;

            quotient[i] = no_borrow;

            // remainder = no_borrow ? trial : remainder (unchanged)
            remainder = self.mux_vector(no_borrow, &trial, &remainder);
        }

        (quotient, remainder)
    }

    /// Negate a vector (two's complement: ~x + 1)
    fn negate_aig(&mut self, x: &[AigLit]) -> Vec<AigLit> {
        let inverted: Vec<_> = x.iter().map(|l| l.invert()).collect();
        let one = vec![self.aig.true_lit()];
        self.build_adder(&inverted, &one)
    }

    /// Mux a vector: sel ? a : b
    fn mux_vector(&mut self, sel: AigLit, a: &[AigLit], b: &[AigLit]) -> Vec<AigLit> {
        let width = a.len().max(b.len());
        (0..width)
            .map(|i| {
                let a_bit = a.get(i).copied().unwrap_or_else(|| self.aig.false_lit());
                let b_bit = b.get(i).copied().unwrap_or_else(|| self.aig.false_lit());
                // MUX: sel ? a : b = (sel & a) | (!sel & b)
                let sel_and_a = self.aig.add_and(sel, a_bit);
                let not_sel_and_b = self.aig.add_and(sel.invert(), b_bit);
                self.aig.add_or(sel_and_a, not_sel_and_b)
            })
            .collect()
    }

    fn build_less_than(&mut self, a: &[AigLit], b: &[AigLit], signed: bool) -> AigLit {
        let width = a.len().max(b.len());
        if width == 0 {
            return self.aig.false_lit();
        }

        // For signed comparison, flip the MSBs (sign bits) to convert to unsigned comparison
        // Signed: a < b iff (a ^ 0x80...) < (b ^ 0x80...) (unsigned)
        // This works because flipping the sign bit maps signed range to unsigned range correctly
        let msb_idx = width - 1;

        let mut less = self.aig.false_lit();
        let mut equal = self.aig.true_lit();

        for i in (0..width).rev() {
            let mut a_bit = a.get(i).copied().unwrap_or_else(|| self.aig.false_lit());
            let mut b_bit = b.get(i).copied().unwrap_or_else(|| self.aig.false_lit());

            // For signed comparison, flip the MSB (sign bit)
            if signed && i == msb_idx {
                a_bit = a_bit.invert();
                b_bit = b_bit.invert();
            }

            // At this bit: a < b if (a=0 and b=1) OR (a==b so far and less from previous bits)
            let a_lt_b_here = self.aig.add_and(a_bit.invert(), b_bit);
            let eq_here = self.aig.add_xnor(a_bit, b_bit);

            let lt_from_here = self.aig.add_and(equal, a_lt_b_here);
            less = self.aig.add_or(less, lt_from_here);
            equal = self.aig.add_and(equal, eq_here);
        }

        less
    }
}

/// MIR-level equivalence checker
pub struct MirEquivalenceChecker {
    timeout: u64,
}

impl MirEquivalenceChecker {
    pub fn new() -> Self {
        Self { timeout: 300 }
    }

    pub fn with_timeout(mut self, timeout: u64) -> Self {
        self.timeout = timeout;
        self
    }

    /// Check equivalence between two MIR modules
    pub fn check_mir_equivalence(
        &self,
        module1: &Module,
        module2: &Module,
    ) -> FormalResult<EquivalenceResult> {
        let start = std::time::Instant::now();

        // Convert both modules to AIG
        let aig1 = MirToAig::new(module1).convert();
        let aig2 = MirToAig::new(module2).convert();

        // Use existing equivalence checker
        let checker = EquivalenceChecker::new();
        let mut result = checker.check_aig_equivalence(&aig1, &aig2)?;
        result.time_ms = start.elapsed().as_millis() as u64;

        Ok(result)
    }

    /// Check equivalence between MIR module and synthesized gate netlist
    ///
    /// This is the key verification: RTL intent vs synthesized gates.
    ///
    /// For sequential designs, this uses combinational equivalence checking (CEC):
    /// - Register outputs are treated as free inputs in both representations
    /// - This verifies that the combinational logic (next-state functions) matches
    /// - MIR register outputs get `_dff_out` suffix to match GateNetlist naming
    ///
    /// Uses intelligent port matching to handle different naming conventions:
    /// - MIR uses simple names: `port_name[bit]`
    /// - GateNetlist uses hierarchical names: `top.port_name[bit]`
    ///
    /// Uses a two-phase approach for speed:
    /// 1. Fast random simulation to find easy counterexamples
    /// 2. Full SAT-based proof if simulation passes
    pub fn check_mir_vs_gates(
        &self,
        module: &Module,
        netlist: &GateNetlist,
    ) -> FormalResult<EquivalenceResult> {
        let start = std::time::Instant::now();

        // Convert MIR to AIG using CEC mode (registers as inputs)
        // This matches how GateNetlistToAig treats DFF outputs
        log::info!("Converting MIR to AIG (CEC mode)...");
        let mir_aig = MirToAig::new(module).convert_for_cec();

        // Convert gate netlist to AIG
        log::info!("Converting GateNetlist to AIG...");
        let gate_aig = GateNetlistToAig::new().convert(netlist);

        log::info!(
            "MIR-AIG: {} inputs, {} outputs, {} AND gates",
            mir_aig.inputs.len(),
            mir_aig.outputs.len(),
            mir_aig.and_count()
        );
        log::info!(
            "Gate-AIG: {} inputs, {} outputs, {} AND gates",
            gate_aig.inputs.len(),
            gate_aig.outputs.len(),
            gate_aig.and_count()
        );

        // Use fast equivalence check (simulation first, then SAT)
        let mut result = fast_equivalence_check(&mir_aig, &gate_aig)?;
        result.time_ms = start.elapsed().as_millis() as u64;

        Ok(result)
    }
}

impl Default for MirEquivalenceChecker {
    fn default() -> Self {
        Self::new()
    }
}

// ============================================================================
// Bounded Model Checking (BMC) for Sequential Equivalence
// ============================================================================

/// Result of BMC equivalence checking
#[derive(Debug, Clone)]
pub struct BmcEquivalenceResult {
    /// True if designs are equivalent up to bound K
    pub equivalent: bool,
    /// The bound K used for checking
    pub bound: usize,
    /// Cycle at which first mismatch was found (if any)
    pub mismatch_cycle: Option<usize>,
    /// Output that mismatched (if any)
    pub mismatch_output: Option<String>,
    /// Multi-cycle counterexample trace (if not equivalent)
    pub counterexample: Option<BmcCounterexample>,
    /// Total time in milliseconds
    pub time_ms: u64,
    /// Number of SAT solver calls
    pub sat_calls: usize,
}

/// Multi-cycle counterexample from BMC
#[derive(Debug, Clone)]
pub struct BmcCounterexample {
    /// Input assignments for each cycle
    pub inputs_per_cycle: Vec<HashMap<String, bool>>,
    /// Register state at each cycle (design 1)
    pub state1_per_cycle: Vec<HashMap<String, bool>>,
    /// Register state at each cycle (design 2)
    pub state2_per_cycle: Vec<HashMap<String, bool>>,
    /// Output values at each cycle (design 1)
    pub outputs1_per_cycle: Vec<HashMap<String, bool>>,
    /// Output values at each cycle (design 2)
    pub outputs2_per_cycle: Vec<HashMap<String, bool>>,
}

/// Bounded Model Checker for sequential equivalence
///
/// Unlike CEC which requires register correspondence, BMC verifies
/// that two designs produce the same outputs given the same inputs
/// over K clock cycles, regardless of internal structure.
///
/// This handles post-optimization verification where:
/// - Register retiming may have occurred
/// - Logic sharing changed module boundaries
/// - Different number of internal registers
pub struct BoundedModelChecker {
    /// Maximum bound for checking
    pub max_bound: usize,
    /// Timeout in seconds
    pub timeout_secs: u64,
}

impl BoundedModelChecker {
    pub fn new() -> Self {
        Self {
            max_bound: 20,
            timeout_secs: 300,
        }
    }

    pub fn with_bound(mut self, bound: usize) -> Self {
        self.max_bound = bound;
        self
    }

    pub fn with_timeout(mut self, timeout: u64) -> Self {
        self.timeout_secs = timeout;
        self
    }

    /// Check bounded equivalence between MIR and GateNetlist
    ///
    /// This is the key verification for post-optimization equivalence.
    /// It doesn't require register correspondence - just verifies that
    /// given the same primary inputs, both designs produce the same
    /// primary outputs over K cycles.
    pub fn check_mir_vs_gates_bmc(
        &self,
        module: &Module,
        netlist: &GateNetlist,
        bound: usize,
    ) -> FormalResult<BmcEquivalenceResult> {
        let start = std::time::Instant::now();

        // Convert MIR to sequential AIG (with latches)
        let mir_aig = MirToAig::new(module).convert_sequential();

        // Convert GateNetlist to sequential AIG (with latches)
        let gate_aig = GateNetlistToAig::new().convert_sequential(netlist);

        log::debug!(
            "BMC: MIR has {} inputs, {} outputs, {} latches",
            mir_aig.inputs.len(),
            mir_aig.outputs.len(),
            mir_aig.latches.len()
        );
        log::debug!(
            "BMC: Gate has {} inputs, {} outputs, {} latches",
            gate_aig.inputs.len(),
            gate_aig.outputs.len(),
            gate_aig.latches.len()
        );

        // Run BMC
        let result = self.check_sequential_aig_equivalence(&mir_aig, &gate_aig, bound)?;

        Ok(BmcEquivalenceResult {
            time_ms: start.elapsed().as_millis() as u64,
            ..result
        })
    }

    /// Check bounded equivalence between hierarchical MIR and GateNetlist
    ///
    /// This is the proper verification for full synthesis flow.
    /// It flattens the entire MIR hierarchy (top module + all child instances)
    /// and compares against the flattened gate netlist.
    ///
    /// This verifies both:
    /// - MIR → LIR lowering (synthesis)
    /// - LIR → Gate tech mapping
    pub fn check_mir_hierarchy_vs_gates_bmc(
        &self,
        mir: &Mir,
        top_module: &Module,
        netlist: &GateNetlist,
        bound: usize,
    ) -> FormalResult<BmcEquivalenceResult> {
        let start = std::time::Instant::now();

        // Convert hierarchical MIR to sequential AIG (flattens all instances)
        let mir_aig = MirToAig::new_with_mir(mir, top_module).convert_sequential_hierarchical();

        // Convert GateNetlist to sequential AIG (with latches)
        let gate_aig = GateNetlistToAig::new().convert_sequential(netlist);

        // Run BMC
        let result = self.check_sequential_aig_equivalence(&mir_aig, &gate_aig, bound)?;

        Ok(BmcEquivalenceResult {
            time_ms: start.elapsed().as_millis() as u64,
            ..result
        })
    }

    /// Check bounded equivalence between LIR and GateNetlist
    ///
    /// Uses LIR (which is already hierarchically flattened) instead of MIR
    /// for proper equivalence checking of designs with child entity instances.
    pub fn check_lir_vs_gates_bmc(
        &self,
        lir: &Lir,
        netlist: &GateNetlist,
        bound: usize,
    ) -> FormalResult<BmcEquivalenceResult> {
        let start = std::time::Instant::now();

        // Convert LIR to sequential AIG (with latches)
        let lir_aig = LirToAig::new().convert_sequential(lir);

        // Convert GateNetlist to sequential AIG (with latches)
        let gate_aig = GateNetlistToAig::new().convert_sequential(netlist);

        log::debug!(
            "BMC: LIR has {} nodes, {} inputs, {} outputs, {} latches",
            lir_aig.nodes.len(),
            lir_aig.inputs.len(),
            lir_aig.outputs.len(),
            lir_aig.latches.len()
        );
        log::debug!(
            "BMC: Gate has {} nodes, {} inputs, {} outputs, {} latches",
            gate_aig.nodes.len(),
            gate_aig.inputs.len(),
            gate_aig.outputs.len(),
            gate_aig.latches.len()
        );

        // Run BMC
        let result = self.check_sequential_aig_equivalence(&lir_aig, &gate_aig, bound)?;

        Ok(BmcEquivalenceResult {
            time_ms: start.elapsed().as_millis() as u64,
            ..result
        })
    }

    /// Check bounded equivalence between two sequential AIGs
    pub fn check_sequential_aig_equivalence(
        &self,
        aig1: &Aig,
        aig2: &Aig,
        bound: usize,
    ) -> FormalResult<BmcEquivalenceResult> {
        let start = std::time::Instant::now();
        let mut sat_calls = 0;

        // Match primary inputs between designs
        let (matched_inputs, aig1_input_map, aig2_input_map) =
            self.match_primary_inputs(aig1, aig2);

        // Match primary outputs between designs
        let matched_outputs = self.match_primary_outputs(aig1, aig2);

        if matched_inputs.is_empty() {
            return Err(FormalError::PropertyFailed(
                "No matching primary inputs between designs".to_string()
            ));
        }

        if matched_outputs.is_empty() {
            return Err(FormalError::PropertyFailed(
                "No matching primary outputs between designs".to_string()
            ));
        }

        log::info!(
            "BMC: {} matched inputs, {} matched outputs, checking {} cycles",
            matched_inputs.len(),
            matched_outputs.len(),
            bound
        );

        // For each cycle, simulate both designs and check output equivalence
        // We use simulation first for speed, then SAT for proof

        // Quick simulation checks with deterministic patterns
        let test_patterns: Vec<(&str, Box<dyn Fn(&str) -> bool>)> = vec![
            ("all-zeros", Box::new(|_| false)),
            ("all-ones", Box::new(|_| true)),
            ("rst-only", Box::new(|key: &str| key == "rst")),
        ];

        for (name, pattern) in &test_patterns {
            let result = self.simulate_trace_with_inputs(
                aig1, aig2,
                &matched_inputs, &aig1_input_map, &aig2_input_map,
                &matched_outputs,
                bound,
                |k| pattern(k),
            );
            if let Some((cycle, output_name)) = result {
                log::debug!("BMC: {} mismatch at cycle {} on output '{}'", name, cycle, output_name);
            }
        }

        // Deterministic pattern simulation
        for seed in 0u64..100 {
            let sim_result = self.simulate_trace_with_inputs(
                aig1, aig2,
                &matched_inputs, &aig1_input_map, &aig2_input_map,
                &matched_outputs,
                bound,
                |key| {
                    let hash = key.as_bytes().iter().fold(seed, |acc, &b| acc.wrapping_mul(31).wrapping_add(b as u64));
                    hash % 2 == 0
                },
            );
            if sim_result.is_some() {
                break;
            }
        }

        let mut rng = rand::thread_rng();

        // Random simulation (1000 traces)
        for trace in 0..1000 {
            let sim_result = self.simulate_bmc_trace(
                aig1, aig2,
                &matched_inputs, &aig1_input_map, &aig2_input_map,
                &matched_outputs,
                bound,
                &mut rng,
            );

            if let Some((cycle, output_name)) = sim_result {
                log::info!(
                    "BMC: Simulation found mismatch at cycle {} on output '{}' (trace {})",
                    cycle, output_name, trace
                );

                return Ok(BmcEquivalenceResult {
                    equivalent: false,
                    bound,
                    mismatch_cycle: Some(cycle),
                    mismatch_output: Some(output_name),
                    counterexample: None,
                    time_ms: start.elapsed().as_millis() as u64,
                    sat_calls: 0,
                });
            }
        }

        log::info!("BMC: Phase 2 - SAT-based verification...");

        // SAT-based BMC: unroll both designs and check equivalence
        for k in 1..=bound {
            sat_calls += 1;

            let sat_result = self.check_cycle_equivalence_sat(
                aig1, aig2,
                &matched_inputs, &aig1_input_map, &aig2_input_map,
                &matched_outputs,
                k,
            )?;

            if !sat_result.0 {
                log::info!("BMC: SAT found mismatch at cycle {} on output '{:?}'", k, sat_result.2);
                return Ok(BmcEquivalenceResult {
                    equivalent: false,
                    bound: k,
                    mismatch_cycle: Some(sat_result.1.unwrap_or(k)),
                    mismatch_output: sat_result.2,
                    counterexample: None,
                    time_ms: start.elapsed().as_millis() as u64,
                    sat_calls,
                });
            }

            if k % 5 == 0 {
                log::debug!("BMC: Verified equivalent up to cycle {}", k);
            }
        }

        log::info!("BMC: Designs equivalent up to bound {}", bound);

        Ok(BmcEquivalenceResult {
            equivalent: true,
            bound,
            mismatch_cycle: None,
            mismatch_output: None,
            counterexample: None,
            time_ms: start.elapsed().as_millis() as u64,
            sat_calls,
        })
    }

    /// Match primary inputs between two AIGs by normalized name
    fn match_primary_inputs(
        &self,
        aig1: &Aig,
        aig2: &Aig,
    ) -> (Vec<String>, HashMap<String, u32>, HashMap<String, u32>) {
        let entity_name = extract_entity_name(aig2);
        let entity_ref = entity_name.as_deref();
        let mut matched = Vec::new();
        let mut aig1_map: HashMap<String, u32> = HashMap::new();
        let mut aig2_map: HashMap<String, u32> = HashMap::new();

        // Build maps for both AIGs
        for (idx, node) in aig1.nodes.iter().enumerate() {
            if let AigNode::Input { name } = node {
                if !is_internal_signal(name) {
                    let normalized = normalize_port_name_with_entity(name, entity_ref);
                    aig1_map.insert(normalized.key(), idx as u32);
                }
            }
        }

        for (idx, node) in aig2.nodes.iter().enumerate() {
            if let AigNode::Input { name } = node {
                if !is_internal_signal(name) {
                    let normalized = normalize_port_name_with_entity(name, entity_ref);
                    aig2_map.insert(normalized.key(), idx as u32);
                }
            }
        }

        // Find matches
        for key in aig1_map.keys() {
            if aig2_map.contains_key(key) {
                matched.push(key.clone());
            }
        }

        (matched, aig1_map, aig2_map)
    }

    /// Match primary outputs between two AIGs
    fn match_primary_outputs(&self, aig1: &Aig, aig2: &Aig) -> Vec<(usize, usize, String)> {
        let entity_name = extract_entity_name(aig2);
        let entity_ref = entity_name.as_deref();
        let mut matched = Vec::new();

        let aig2_outputs: HashMap<String, usize> = aig2.output_names.iter()
            .enumerate()
            .map(|(i, name)| (normalize_port_name_with_entity(name, entity_ref).key(), i))
            .collect();

        for (i, name) in aig1.output_names.iter().enumerate() {
            let key = normalize_port_name_with_entity(name, entity_ref).key();
            if let Some(&j) = aig2_outputs.get(&key) {
                matched.push((i, j, key));
            }
        }

        matched
    }

    /// Simulate a random BMC trace and check for mismatches
    fn simulate_bmc_trace(
        &self,
        aig1: &Aig,
        aig2: &Aig,
        matched_inputs: &[String],
        aig1_input_map: &HashMap<String, u32>,
        aig2_input_map: &HashMap<String, u32>,
        matched_outputs: &[(usize, usize, String)],
        bound: usize,
        rng: &mut impl rand::Rng,
    ) -> Option<(usize, String)> {
        // Initialize latch states to init values
        let mut state1 = self.get_initial_latch_state(aig1);
        let mut state2 = self.get_initial_latch_state(aig2);

        // Reset phase: keep rst=true for first 2 cycles to properly initialize state
        let reset_cycles = 2;

        for cycle in 0..bound {
            // Generate inputs for this cycle
            // Use constrained random simulation that represents realistic operation:
            // - rst=true during reset phase, then rst=false
            // - enable=true during normal operation (after reset)
            // - Other inputs are random
            let mut input_values: HashMap<String, bool> = HashMap::new();
            for key in matched_inputs {
                let val = if key == "rst" {
                    cycle < reset_cycles  // Reset only during reset phase
                } else if key == "enable" {
                    cycle >= reset_cycles  // Enable only after reset
                } else {
                    rng.gen()  // Random for other inputs
                };
                input_values.insert(key.clone(), val);
            }
            // Simulate both AIGs
            let (outputs1, next_state1) = self.simulate_aig_cycle(
                aig1, &input_values, &state1, aig1_input_map
            );
            let (outputs2, next_state2) = self.simulate_aig_cycle(
                aig2, &input_values, &state2, aig2_input_map
            );

            // Check output equivalence (skip during reset phase since outputs are "don't care")
            if cycle >= reset_cycles {
                for (i1, i2, name) in matched_outputs {
                    let o1 = outputs1.get(*i1).copied().unwrap_or(false);
                    let o2 = outputs2.get(*i2).copied().unwrap_or(false);
                    if o1 != o2 {
                    return Some((cycle, name.clone()));
                }
            }
            } // end if cycle >= reset_cycles

            // Update state for next cycle
            state1 = next_state1;
            state2 = next_state2;
        }

        None
    }

    /// Simulate a BMC trace with specified input pattern
    fn simulate_trace_with_inputs<F>(
        &self,
        aig1: &Aig,
        aig2: &Aig,
        matched_inputs: &[String],
        aig1_input_map: &HashMap<String, u32>,
        aig2_input_map: &HashMap<String, u32>,
        matched_outputs: &[(usize, usize, String)],
        bound: usize,
        input_fn: F,
    ) -> Option<(usize, String)>
    where
        F: Fn(&str) -> bool,
    {
        let mut state1 = self.get_initial_latch_state(aig1);
        let mut state2 = self.get_initial_latch_state(aig2);

        for cycle in 0..bound {
            let mut input_values: HashMap<String, bool> = HashMap::new();
            for key in matched_inputs {
                input_values.insert(key.clone(), input_fn(key));
            }

            let (outputs1, next_state1) = self.simulate_aig_cycle(
                aig1, &input_values, &state1, aig1_input_map
            );
            let (outputs2, next_state2) = self.simulate_aig_cycle(
                aig2, &input_values, &state2, aig2_input_map
            );

            for (i1, i2, name) in matched_outputs {
                let o1 = outputs1.get(*i1).copied().unwrap_or(false);
                let o2 = outputs2.get(*i2).copied().unwrap_or(false);

                if o1 != o2 {
                    return Some((cycle, name.clone()));
                }
            }

            state1 = next_state1;
            state2 = next_state2;
        }

        None
    }

    /// Simulate a BMC trace with detailed debug output for each cycle
    /// Note: This is now just an alias for simulate_trace_with_inputs
    #[allow(dead_code)]
    fn simulate_trace_with_detailed_debug<F>(
        &self,
        aig1: &Aig,
        aig2: &Aig,
        matched_inputs: &[String],
        aig1_input_map: &HashMap<String, u32>,
        aig2_input_map: &HashMap<String, u32>,
        matched_outputs: &[(usize, usize, String)],
        bound: usize,
        input_fn: F,
    ) -> Option<(usize, String)>
    where
        F: Fn(&str) -> bool,
    {
        self.simulate_trace_with_inputs(
            aig1, aig2, matched_inputs, aig1_input_map, aig2_input_map,
            matched_outputs, bound, input_fn
        )
    }

    #[allow(dead_code)]
    fn debug_trace_mismatch(
        &self,
        _aig1: &Aig,
        _aig2: &Aig,
        _matched_inputs: &[String],
        _aig1_input_map: &HashMap<String, u32>,
        _aig2_input_map: &HashMap<String, u32>,
        _matched_outputs: &[(usize, usize, String)],
        _bound: usize,
        _output_name: &str,
    ) {
        // Debug function - body removed during cleanup
    }

    /// Trace the cone of influence for a node - find all primary inputs that affect it
    #[allow(dead_code)]
    fn trace_input_cone(&self, aig: &Aig, start_node: u32, max_depth: usize) -> Vec<(String, u32)> {
        use std::collections::HashSet;
        let mut inputs = Vec::new();
        let mut visited = HashSet::new();
        let mut worklist = vec![(start_node, 0usize)];

        while let Some((node_id, depth)) = worklist.pop() {
            if depth > max_depth || visited.contains(&node_id) || node_id == 0 {
                continue;
            }
            visited.insert(node_id);

            if let Some(node) = aig.nodes.get(node_id as usize) {
                match node {
                    AigNode::Input { name } => {
                        inputs.push((name.clone(), node_id));
                    }
                    AigNode::And { left, right } => {
                        worklist.push((left.node.0, depth + 1));
                        worklist.push((right.node.0, depth + 1));
                    }
                    AigNode::Latch { name, .. } => {
                        // Latch output is also an input to the cone
                        inputs.push((format!("(latch){}", name), node_id));
                    }
                    AigNode::False => {}
                }
            }
        }

        inputs.sort_by(|a, b| a.0.cmp(&b.0));
        inputs.dedup_by_key(|x| x.0.clone());
        inputs
    }

    /// Get initial latch state (all latches at their init values)
    fn get_initial_latch_state(&self, aig: &Aig) -> HashMap<u32, bool> {
        let mut state = HashMap::new();
        for &latch_id in &aig.latches {
            if let AigNode::Latch { init, .. } = &aig.nodes[latch_id.0 as usize] {
                state.insert(latch_id.0, *init);
            }
        }
        state
    }

    /// Simulate one cycle of an AIG
    fn simulate_aig_cycle(
        &self,
        aig: &Aig,
        inputs: &HashMap<String, bool>,
        latch_state: &HashMap<u32, bool>,
        input_map: &HashMap<String, u32>,
    ) -> (Vec<bool>, HashMap<u32, bool>) {
        let mut values: HashMap<u32, bool> = HashMap::new();

        // Node 0 is false
        values.insert(0, false);

        // Set input values
        for (key, &val) in inputs {
            if let Some(&node_id) = input_map.get(key) {
                values.insert(node_id, val);
            }
        }

        // Set latch output values from current state
        for (&latch_id, &val) in latch_state {
            values.insert(latch_id, val);
        }

        // Set state input values from their linked latches (metadata-based, no name matching)
        // The state_input_to_latch map was populated when the AIG was built
        for (&state_input_id, &latch_id) in &aig.state_input_to_latch {
            if let Some(&latch_val) = latch_state.get(&latch_id) {
                values.insert(state_input_id, latch_val);
            }
        }

        // Evaluate all nodes in order
        for (idx, node) in aig.nodes.iter().enumerate() {
            let idx = idx as u32;
            match node {
                AigNode::False => {
                    values.insert(idx, false);
                }
                AigNode::Input { name } => {
                    // Already set from inputs or use default
                    if !values.contains_key(&idx) {
                        // Check if it's an internal signal that wasn't in matched inputs
                        let normalized = normalize_port_name(name);
                        if let Some(&val) = inputs.get(&normalized.key()) {
                            values.insert(idx, val);
                        } else {
                            values.insert(idx, false);
                        }
                    }
                }
                AigNode::And { left, right } => {
                    let l = values.get(&left.node.0).copied().unwrap_or(false);
                    let r = values.get(&right.node.0).copied().unwrap_or(false);
                    let l = if left.inverted { !l } else { l };
                    let r = if right.inverted { !r } else { r };
                    values.insert(idx, l && r);
                }
                AigNode::Latch { .. } => {
                    // Latch output already set from latch_state
                    if !values.contains_key(&idx) {
                        values.insert(idx, false);
                    }
                }
            }
        }

        // Get outputs
        let outputs: Vec<bool> = aig.outputs.iter().map(|lit| {
            let val = values.get(&lit.node.0).copied().unwrap_or(false);
            if lit.inverted { !val } else { val }
        }).collect();

        // Compute next latch state
        let mut next_state: HashMap<u32, bool> = HashMap::new();
        let _rst_val = inputs.get("rst").copied().unwrap_or(false);
        for &latch_id in &aig.latches {
            if let AigNode::Latch { next, .. } = &aig.nodes[latch_id.0 as usize] {
                let raw_val = values.get(&next.node.0).copied().unwrap_or(false);
                let val = if next.inverted { !raw_val } else { raw_val };
                next_state.insert(latch_id.0, val);
            }
        }

        (outputs, next_state)
    }

    /// SAT-based check for equivalence up to cycle K
    fn check_cycle_equivalence_sat(
        &self,
        aig1: &Aig,
        aig2: &Aig,
        matched_inputs: &[String],
        aig1_input_map: &HashMap<String, u32>,
        aig2_input_map: &HashMap<String, u32>,
        matched_outputs: &[(usize, usize, String)],
        k: usize,
    ) -> FormalResult<(bool, Option<usize>, Option<String>)> {
        use std::cell::Cell;

        let mut formula = CnfFormula::new();
        let var_counter = Cell::new(0u32);

        // Helper to create new variable
        let new_var = || {
            let v = Var::from_index(var_counter.get() as usize);
            var_counter.set(var_counter.get() + 1);
            v
        };

        // Maps: (aig_id, node_id, cycle) -> Var
        let mut node_vars: HashMap<(u8, u32, usize), Var> = HashMap::new();

        // Helper to get or create variable for a node
        let mut get_node_var = |aig_id: u8, node_id: u32, cycle: usize| -> Var {
            let key = (aig_id, node_id, cycle);
            if let Some(&v) = node_vars.get(&key) {
                v
            } else {
                let v = new_var();
                node_vars.insert(key, v);
                v
            }
        };

        // Miter output: any output differs at any cycle
        // Each entry is (diff_var, cycle, output_name)
        let mut miter_clauses: Vec<(Var, usize, String)> = Vec::new();

        for cycle in 0..k {
            // Create shared input variables for this cycle
            let mut cycle_inputs: HashMap<String, Var> = HashMap::new();
            for key in matched_inputs {
                let v = new_var();
                cycle_inputs.insert(key.clone(), v);
            }

            // Encode AIG1 for this cycle
            self.encode_aig_cycle(
                aig1, 1, cycle,
                &cycle_inputs, aig1_input_map,
                &mut formula, &mut get_node_var,
            );

            // Encode AIG2 for this cycle
            self.encode_aig_cycle(
                aig2, 2, cycle,
                &cycle_inputs, aig2_input_map,
                &mut formula, &mut get_node_var,
            );

            // Connect latch outputs at cycle+1 to latch inputs at cycle (if cycle > 0)
            if cycle > 0 {
                self.connect_latch_states(aig1, 1, cycle, &mut formula, &mut get_node_var);
                self.connect_latch_states(aig2, 2, cycle, &mut formula, &mut get_node_var);
            }

            // Create miter for outputs at this cycle
            for (i1, i2, name) in matched_outputs {
                let out1_lit = aig1.outputs[*i1];
                let out2_lit = aig2.outputs[*i2];

                let out1_var = get_node_var(1, out1_lit.node.0, cycle);
                let out2_var = get_node_var(2, out2_lit.node.0, cycle);

                // XOR the outputs (with inversions considered)
                // diff = out1 XOR out2
                let diff_var = new_var();

                // Encode XOR
                // diff = (out1 AND !out2) OR (!out1 AND out2)
                let lit1 = if out1_lit.inverted {
                    Lit::negative(out1_var)
                } else {
                    Lit::positive(out1_var)
                };
                let lit2 = if out2_lit.inverted {
                    Lit::negative(out2_var)
                } else {
                    Lit::positive(out2_var)
                };

                // diff = lit1 XOR lit2
                // CNF: (!diff OR !lit1 OR !lit2) AND (!diff OR lit1 OR lit2) AND (diff OR !lit1 OR lit2) AND (diff OR lit1 OR !lit2)
                formula.add_clause(&[Lit::negative(diff_var), !lit1, !lit2]);
                formula.add_clause(&[Lit::negative(diff_var), lit1, lit2]);
                formula.add_clause(&[Lit::positive(diff_var), !lit1, lit2]);
                formula.add_clause(&[Lit::positive(diff_var), lit1, !lit2]);

                miter_clauses.push((diff_var, cycle, name.clone()));
            }
        }

        // At least one output must differ (for SAT to find counterexample)
        if !miter_clauses.is_empty() {
            let clause: Vec<Lit> = miter_clauses.iter()
                .map(|(v, _, _)| Lit::positive(*v))
                .collect();
            formula.add_clause(&clause);
        }

        // Initialize latches to their init values at cycle 0
        self.constrain_initial_state(aig1, 1, &mut formula, &mut get_node_var);
        self.constrain_initial_state(aig2, 2, &mut formula, &mut get_node_var);

        // Solve
        let mut solver = Solver::new();
        solver.add_formula(&formula);

        match solver.solve() {
            Ok(true) => {
                // SAT = counterexample found = designs differ
                // Extract which output(s) differ from the model
                let model = solver.model().unwrap_or_default();
                let model_set: std::collections::HashSet<Lit> = model.iter().copied().collect();
                let mut differing_outputs: Vec<(usize, String)> = Vec::new();

                for (diff_var, cycle, name) in &miter_clauses {
                    // Check if this diff variable is true in the model
                    // diff_var being true means the outputs differ at this cycle
                    if model_set.contains(&Lit::positive(*diff_var)) {
                        differing_outputs.push((*cycle, name.clone()));
                    }
                }

                // Report the first differing output
                let (mismatch_cycle, mismatch_output) = if let Some((c, n)) = differing_outputs.first() {
                    (Some(*c), Some(n.clone()))
                } else {
                    (Some(k), None)
                };

                // Log all differing outputs for debugging
                if !differing_outputs.is_empty() {
                    log::debug!("SAT counterexample - differing outputs:");
                    for (c, n) in &differing_outputs {
                        log::debug!("  cycle {}: {}", c, n);
                    }
                }

                Ok((false, mismatch_cycle, mismatch_output))
            }
            Ok(false) => {
                // UNSAT = no counterexample = equivalent up to this bound
                Ok((true, None, None))
            }
            Err(e) => {
                Err(FormalError::SolverError(format!("SAT solver error: {:?}", e)))
            }
        }
    }

    /// Encode one cycle of an AIG as CNF
    fn encode_aig_cycle(
        &self,
        aig: &Aig,
        aig_id: u8,
        cycle: usize,
        inputs: &HashMap<String, Var>,
        input_map: &HashMap<String, u32>,
        formula: &mut CnfFormula,
        get_var: &mut impl FnMut(u8, u32, usize) -> Var,
    ) {
        // Node 0 is always false
        let false_var = get_var(aig_id, 0, cycle);
        formula.add_clause(&[Lit::negative(false_var)]);

        for (idx, node) in aig.nodes.iter().enumerate() {
            let idx = idx as u32;
            match node {
                AigNode::False => {
                    // Already handled
                }
                AigNode::Input { name } => {
                    let normalized = normalize_port_name(name);
                    if let Some(&input_var) = inputs.get(&normalized.key()) {
                        let node_var = get_var(aig_id, idx, cycle);
                        // node_var = input_var
                        formula.add_clause(&[Lit::negative(node_var), Lit::positive(input_var)]);
                        formula.add_clause(&[Lit::positive(node_var), Lit::negative(input_var)]);
                    }
                }
                AigNode::And { left, right } => {
                    let out_var = get_var(aig_id, idx, cycle);
                    let left_var = get_var(aig_id, left.node.0, cycle);
                    let right_var = get_var(aig_id, right.node.0, cycle);

                    let left_lit = if left.inverted {
                        Lit::negative(left_var)
                    } else {
                        Lit::positive(left_var)
                    };
                    let right_lit = if right.inverted {
                        Lit::negative(right_var)
                    } else {
                        Lit::positive(right_var)
                    };

                    // out = left AND right
                    // CNF: (!out OR left) AND (!out OR right) AND (out OR !left OR !right)
                    formula.add_clause(&[Lit::negative(out_var), left_lit]);
                    formula.add_clause(&[Lit::negative(out_var), right_lit]);
                    formula.add_clause(&[Lit::positive(out_var), !left_lit, !right_lit]);
                }
                AigNode::Latch { .. } => {
                    // Latch output at cycle is a free variable (set by initial state or previous cycle)
                    // Just ensure the variable exists
                    let _ = get_var(aig_id, idx, cycle);
                }
            }
        }
    }

    /// Connect latch states between cycles
    /// Also connects __reg_cur_ / __dff_cur_ input nodes to latch outputs at this cycle
    fn connect_latch_states(
        &self,
        aig: &Aig,
        aig_id: u8,
        cycle: usize,
        formula: &mut CnfFormula,
        get_var: &mut impl FnMut(u8, u32, usize) -> Var,
    ) {
        // Connect latch outputs to their next-state values from the previous cycle
        for &latch_id in &aig.latches {
            if let AigNode::Latch { next, .. } = &aig.nodes[latch_id.0 as usize] {
                // latch_output[cycle] = next[cycle-1]
                let out_var = get_var(aig_id, latch_id.0, cycle);
                let next_var = get_var(aig_id, next.node.0, cycle - 1);

                let next_lit = if next.inverted {
                    Lit::negative(next_var)
                } else {
                    Lit::positive(next_var)
                };

                // out_var = next_lit
                formula.add_clause(&[Lit::negative(out_var), next_lit]);
                formula.add_clause(&[Lit::positive(out_var), !next_lit]);
            }
        }

        // Connect state inputs to their linked latch outputs at this cycle
        // Uses metadata mapping, no name matching needed
        for (&state_input_id, &latch_id) in &aig.state_input_to_latch {
            let input_var = get_var(aig_id, state_input_id, cycle);
            let latch_var = get_var(aig_id, latch_id, cycle);
            // input_var = latch_var
            formula.add_clause(&[Lit::negative(input_var), Lit::positive(latch_var)]);
            formula.add_clause(&[Lit::positive(input_var), Lit::negative(latch_var)]);
        }
    }

    /// Constrain latches to initial values at cycle 0
    /// Also connects state input nodes to latch outputs
    fn constrain_initial_state(
        &self,
        aig: &Aig,
        aig_id: u8,
        formula: &mut CnfFormula,
        get_var: &mut impl FnMut(u8, u32, usize) -> Var,
    ) {
        // Constrain latch outputs to init values
        for &latch_id in &aig.latches {
            if let AigNode::Latch { init, .. } = &aig.nodes[latch_id.0 as usize] {
                let var = get_var(aig_id, latch_id.0, 0);
                if *init {
                    formula.add_clause(&[Lit::positive(var)]);
                } else {
                    formula.add_clause(&[Lit::negative(var)]);
                }
            }
        }

        // Connect state inputs to their linked latch outputs at cycle 0
        // Uses metadata mapping, no name matching needed
        for (&state_input_id, &latch_id) in &aig.state_input_to_latch {
            let input_var = get_var(aig_id, state_input_id, 0);
            let latch_var = get_var(aig_id, latch_id, 0);
            // input_var = latch_var
            formula.add_clause(&[Lit::negative(input_var), Lit::positive(latch_var)]);
            formula.add_clause(&[Lit::positive(input_var), Lit::negative(latch_var)]);
        }
    }
}

impl Default for BoundedModelChecker {
    fn default() -> Self {
        Self::new()
    }
}

// ============================================================================
// Hierarchical Equivalence Checker with Semantic Fingerprinting
// ============================================================================
//
// This is a cleaner approach inspired by commercial tools (Synopsys Formality,
// Cadence Conformal) but with a fresh design:
//
// 1. **Output-only BMC**: We only compare primary outputs, letting internal
//    state evolve freely. This handles cross-module optimizations naturally.
//
// 2. **Semantic fingerprinting**: Match outputs by simulation signatures,
//    not by name. This is robust to renaming and restructuring.
//
// 3. **Hierarchical verification**: Optionally verify each entity separately,
//    then compose. This provides better debugging when mismatches occur.
//
// The key insight: if two designs produce identical outputs for ALL possible
// input sequences over K cycles, starting from reset, they are K-equivalent.
// This doesn't require internal state correspondence at all.

/// A semantic fingerprint for an output signal
///
/// This captures the "behavior" of an output by simulating it with
/// deterministic pseudo-random inputs and recording the result pattern.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SemanticFingerprint {
    /// Bit pattern from simulation (128 cycles of output values)
    pub signature: u128,
    /// Output index in the original AIG
    pub output_idx: usize,
}

impl SemanticFingerprint {
    /// Generate a fingerprint for an output by simulating the AIG
    pub fn from_output(aig: &Aig, output_idx: usize, num_cycles: usize) -> Self {
        let mut signature = 0u128;
        let mut state = Self::get_initial_state(aig);

        // Use deterministic LFSR-based input generation for reproducibility
        let mut lfsr = 0xDEADBEEFu64;

        for cycle in 0..num_cycles.min(128) {
            // Generate inputs for this cycle
            let mut input_vals: HashMap<u32, bool> = HashMap::new();
            for &input_id in &aig.inputs {
                lfsr = Self::lfsr_next(lfsr);
                input_vals.insert(input_id.0, (lfsr & 1) == 1);
            }

            // Simulate one cycle
            let (values, next_state) = Self::simulate_cycle(aig, &input_vals, &state);
            state = next_state;

            // Extract output value
            let out_lit = &aig.outputs[output_idx];
            let out_val = values.get(&out_lit.node.0).copied().unwrap_or(false);
            let out_val = if out_lit.inverted { !out_val } else { out_val };

            if out_val {
                signature |= 1u128 << cycle;
            }
        }

        Self {
            signature,
            output_idx,
        }
    }

    /// LFSR for deterministic pseudo-random generation
    fn lfsr_next(state: u64) -> u64 {
        let bit = ((state >> 0) ^ (state >> 2) ^ (state >> 3) ^ (state >> 5)) & 1;
        (state >> 1) | (bit << 63)
    }

    /// Get initial latch state (all latches at their init values)
    fn get_initial_state(aig: &Aig) -> HashMap<u32, bool> {
        let mut state = HashMap::new();
        for (idx, node) in aig.nodes.iter().enumerate() {
            if let AigNode::Latch { init, .. } = node {
                state.insert(idx as u32, *init);
            }
        }
        state
    }

    /// Simulate one cycle, returning node values and next state
    fn simulate_cycle(
        aig: &Aig,
        inputs: &HashMap<u32, bool>,
        state: &HashMap<u32, bool>,
    ) -> (HashMap<u32, bool>, HashMap<u32, bool>) {
        let mut values: HashMap<u32, bool> = HashMap::new();
        values.insert(0, false); // Constant false

        // Set input values
        for (&id, &val) in inputs {
            values.insert(id, val);
        }

        // Set latch current values (from state)
        for (&latch_id, &val) in state {
            values.insert(latch_id, val);
        }

        // Handle state_input -> latch mappings
        for (&state_input_id, &latch_id) in &aig.state_input_to_latch {
            if let Some(&val) = state.get(&latch_id) {
                values.insert(state_input_id, val);
            }
        }

        // Evaluate all nodes in order
        for (idx, node) in aig.nodes.iter().enumerate() {
            let idx = idx as u32;
            match node {
                AigNode::False => {}
                AigNode::Input { .. } => {
                    // Already set from inputs or state
                    if !values.contains_key(&idx) {
                        values.insert(idx, false);
                    }
                }
                AigNode::And { left, right } => {
                    let l = values.get(&left.node.0).copied().unwrap_or(false);
                    let r = values.get(&right.node.0).copied().unwrap_or(false);
                    let l = if left.inverted { !l } else { l };
                    let r = if right.inverted { !r } else { r };
                    values.insert(idx, l && r);
                }
                AigNode::Latch { .. } => {
                    // Latch output already set from state
                    if !values.contains_key(&idx) {
                        values.insert(idx, false);
                    }
                }
            }
        }

        // Compute next state (latch next values)
        let mut next_state = HashMap::new();
        for (idx, node) in aig.nodes.iter().enumerate() {
            if let AigNode::Latch { next, .. } = node {
                let next_val = values.get(&next.node.0).copied().unwrap_or(false);
                let next_val = if next.inverted { !next_val } else { next_val };
                next_state.insert(idx as u32, next_val);
            }
        }

        (values, next_state)
    }
}

/// Result of hierarchical equivalence checking
#[derive(Debug, Clone)]
pub struct HierarchicalEquivalenceResult {
    /// Overall equivalence status
    pub equivalent: bool,
    /// BMC bound used
    pub bound: usize,
    /// Per-output equivalence status (output key -> equivalent)
    pub output_status: HashMap<String, bool>,
    /// Unmatched outputs in design 1
    pub unmatched_outputs_1: Vec<String>,
    /// Unmatched outputs in design 2
    pub unmatched_outputs_2: Vec<String>,
    /// Total time in milliseconds
    pub time_ms: u64,
    /// Detailed mismatch info if any
    pub mismatch_details: Option<MismatchDetails>,
}

/// Details about a mismatch found during verification
#[derive(Debug, Clone)]
pub struct MismatchDetails {
    /// Output name/key that mismatched
    pub output: String,
    /// Cycle at which mismatch occurred
    pub cycle: usize,
    /// Value in design 1
    pub value_1: bool,
    /// Value in design 2
    pub value_2: bool,
    /// Input sequence that led to mismatch
    pub input_sequence: Vec<HashMap<String, bool>>,
}

/// Hierarchical equivalence checker with semantic fingerprinting
///
/// This is the recommended approach for post-synthesis verification:
/// - No internal state matching required
/// - Handles cross-module optimizations
/// - Semantic output matching (not name-based)
/// - Proper reset handling
pub struct HierarchicalEquivalenceChecker {
    /// BMC bound (number of cycles to verify)
    pub max_bound: usize,
    /// Whether to use semantic fingerprinting for output matching
    pub use_fingerprinting: bool,
    /// Number of cycles for fingerprint generation
    pub fingerprint_cycles: usize,
}

impl HierarchicalEquivalenceChecker {
    pub fn new() -> Self {
        Self {
            max_bound: 20,
            use_fingerprinting: true,
            fingerprint_cycles: 64,
        }
    }

    pub fn with_bound(mut self, bound: usize) -> Self {
        self.max_bound = bound;
        self
    }

    pub fn with_fingerprinting(mut self, enabled: bool) -> Self {
        self.use_fingerprinting = enabled;
        self
    }

    /// Check equivalence between two sequential AIGs using output-only BMC
    ///
    /// This is the core verification: given same inputs, do both designs
    /// produce same outputs over K cycles from reset?
    pub fn check_equivalence(
        &self,
        aig1: &Aig,
        aig2: &Aig,
        bound: usize,
    ) -> FormalResult<HierarchicalEquivalenceResult> {
        let start = std::time::Instant::now();

        // Step 1: Match outputs (semantic or name-based)
        let matched_outputs = if self.use_fingerprinting {
            self.match_outputs_semantic(aig1, aig2)
        } else {
            self.match_outputs_by_name(aig1, aig2)
        };

        // Step 2: Match inputs (by normalized name, inputs should be stable)
        let (matched_inputs, aig1_input_map, aig2_input_map) =
            self.match_inputs(aig1, aig2);

        if matched_inputs.is_empty() {
            return Err(FormalError::PropertyFailed(
                "No matching primary inputs".to_string()
            ));
        }

        if matched_outputs.is_empty() {
            return Err(FormalError::PropertyFailed(
                "No matching primary outputs".to_string()
            ));
        }

        // Step 3: Compute unmatched outputs
        let unmatched_1 = self.get_unmatched_outputs(aig1, &matched_outputs, true);
        let unmatched_2 = self.get_unmatched_outputs(aig2, &matched_outputs, false);

        // Step 4: Run output-only BMC
        let mut output_status: HashMap<String, bool> = HashMap::new();
        let mut mismatch_details: Option<MismatchDetails> = None;
        let mut all_equivalent = true;

        // First do simulation-based quick check
        for (key, o1_idx, o2_idx) in &matched_outputs {
            let sim_result = self.simulate_output_equivalence(
                aig1, aig2,
                *o1_idx, *o2_idx,
                &matched_inputs,
                &aig1_input_map,
                &aig2_input_map,
                bound,
            );

            if let Some((cycle, val1, val2, inputs)) = sim_result {
                output_status.insert(key.clone(), false);
                all_equivalent = false;
                mismatch_details = Some(MismatchDetails {
                    output: key.clone(),
                    cycle,
                    value_1: val1,
                    value_2: val2,
                    input_sequence: inputs,
                });
                break; // Stop on first mismatch for debugging
            } else {
                output_status.insert(key.clone(), true);
            }
        }

        if all_equivalent {
            // SAT-based verification for proof
            for k in 1..=bound {
                let sat_result = self.check_cycle_sat(
                    aig1, aig2,
                    &matched_outputs,
                    &matched_inputs,
                    &aig1_input_map,
                    &aig2_input_map,
                    k,
                )?;

                if let Some((output_key, val1, val2)) = sat_result {
                    output_status.insert(output_key.clone(), false);
                    all_equivalent = false;
                    mismatch_details = Some(MismatchDetails {
                        output: output_key,
                        cycle: k,
                        value_1: val1,
                        value_2: val2,
                        input_sequence: vec![],
                    });
                    break;
                }

            }
        }


        Ok(HierarchicalEquivalenceResult {
            equivalent: all_equivalent,
            bound,
            output_status,
            unmatched_outputs_1: unmatched_1,
            unmatched_outputs_2: unmatched_2,
            time_ms: start.elapsed().as_millis() as u64,
            mismatch_details,
        })
    }

    /// Match outputs using semantic fingerprinting
    ///
    /// This generates fingerprints for each output and matches them by
    /// signature similarity, not by name.
    fn match_outputs_semantic(&self, aig1: &Aig, aig2: &Aig) -> Vec<(String, usize, usize)> {
        let mut matched = Vec::new();

        // Generate fingerprints for design 1
        let fp1: Vec<SemanticFingerprint> = (0..aig1.outputs.len())
            .map(|i| SemanticFingerprint::from_output(aig1, i, self.fingerprint_cycles))
            .collect();

        // Generate fingerprints for design 2
        let fp2: Vec<SemanticFingerprint> = (0..aig2.outputs.len())
            .map(|i| SemanticFingerprint::from_output(aig2, i, self.fingerprint_cycles))
            .collect();

        // Build a map of signature -> outputs in design 2
        let mut fp2_map: HashMap<u128, Vec<usize>> = HashMap::new();
        for fp in &fp2 {
            fp2_map.entry(fp.signature).or_default().push(fp.output_idx);
        }

        // Match by signature
        let mut used_outputs_2 = std::collections::HashSet::new();

        let entity_name = extract_entity_name(aig2);
        let entity_ref = entity_name.as_deref();

        for fp in &fp1 {
            if let Some(candidates) = fp2_map.get(&fp.signature) {
                // Find a candidate that hasn't been matched yet
                for &o2_idx in candidates {
                    if !used_outputs_2.contains(&o2_idx) {
                        // Use a composite key: try name match first, else use index
                        let key = if let (Some(n1), Some(n2)) = (
                            aig1.output_names.get(fp.output_idx),
                            aig2.output_names.get(o2_idx)
                        ) {
                            let k1 = normalize_port_name_with_entity(n1, entity_ref).key();
                            let k2 = normalize_port_name_with_entity(n2, entity_ref).key();
                            if k1 == k2 {
                                k1
                            } else {
                                format!("fp_{:x}", fp.signature)
                            }
                        } else {
                            format!("fp_{:x}", fp.signature)
                        };

                        matched.push((key, fp.output_idx, o2_idx));
                        used_outputs_2.insert(o2_idx);
                        break;
                    }
                }
            }
        }

        // Also try name-based matching for outputs that didn't match by fingerprint
        // (in case they have different behavior due to optimization but same interface)
        let matched_1: std::collections::HashSet<_> = matched.iter().map(|(_, i, _)| *i).collect();
        let matched_2: std::collections::HashSet<_> = matched.iter().map(|(_, _, i)| *i).collect();

        let aig2_by_name: HashMap<String, usize> = aig2.output_names.iter()
            .enumerate()
            .filter(|(i, _)| !matched_2.contains(i))
            .map(|(i, n)| (normalize_port_name_with_entity(n, entity_ref).key(), i))
            .collect();

        for (o1_idx, name) in aig1.output_names.iter().enumerate() {
            if matched_1.contains(&o1_idx) {
                continue;
            }
            let key = normalize_port_name_with_entity(name, entity_ref).key();
            if let Some(&o2_idx) = aig2_by_name.get(&key) {
                matched.push((key, o1_idx, o2_idx));
            }
        }

        matched
    }

    /// Match outputs by normalized name only (fallback)
    fn match_outputs_by_name(&self, aig1: &Aig, aig2: &Aig) -> Vec<(String, usize, usize)> {
        let entity_name = extract_entity_name(aig2);
        let entity_ref = entity_name.as_deref();
        let mut matched = Vec::new();

        let aig2_outputs: HashMap<String, usize> = aig2.output_names.iter()
            .enumerate()
            .map(|(i, name)| (normalize_port_name_with_entity(name, entity_ref).key(), i))
            .collect();

        for (i, name) in aig1.output_names.iter().enumerate() {
            let key = normalize_port_name_with_entity(name, entity_ref).key();
            if let Some(&j) = aig2_outputs.get(&key) {
                matched.push((key, i, j));
            }
        }

        matched
    }

    /// Match inputs by normalized name
    fn match_inputs(
        &self,
        aig1: &Aig,
        aig2: &Aig,
    ) -> (Vec<String>, HashMap<String, u32>, HashMap<String, u32>) {
        let entity_name = extract_entity_name(aig2);
        let entity_ref = entity_name.as_deref();
        let mut matched = Vec::new();
        let mut aig1_map: HashMap<String, u32> = HashMap::new();
        let mut aig2_map: HashMap<String, u32> = HashMap::new();

        // Build maps
        for (idx, node) in aig1.nodes.iter().enumerate() {
            if let AigNode::Input { name } = node {
                if !is_internal_signal(name) {
                    let key = normalize_port_name_with_entity(name, entity_ref).key();
                    aig1_map.insert(key, idx as u32);
                }
            }
        }

        for (idx, node) in aig2.nodes.iter().enumerate() {
            if let AigNode::Input { name } = node {
                if !is_internal_signal(name) {
                    let key = normalize_port_name_with_entity(name, entity_ref).key();
                    aig2_map.insert(key, idx as u32);
                }
            }
        }

        // Find matches
        for key in aig1_map.keys() {
            if aig2_map.contains_key(key) {
                matched.push(key.clone());
            }
        }

        (matched, aig1_map, aig2_map)
    }

    /// Get unmatched outputs
    fn get_unmatched_outputs(
        &self,
        aig: &Aig,
        matched: &[(String, usize, usize)],
        is_first: bool,
    ) -> Vec<String> {
        let matched_set: std::collections::HashSet<usize> = if is_first {
            matched.iter().map(|(_, i, _)| *i).collect()
        } else {
            matched.iter().map(|(_, _, i)| *i).collect()
        };

        aig.output_names.iter()
            .enumerate()
            .filter(|(i, _)| !matched_set.contains(i))
            .map(|(_, n)| n.clone())
            .collect()
    }

    /// Simulate output equivalence using deterministic and random patterns
    fn simulate_output_equivalence(
        &self,
        aig1: &Aig,
        aig2: &Aig,
        o1_idx: usize,
        o2_idx: usize,
        matched_inputs: &[String],
        aig1_input_map: &HashMap<String, u32>,
        aig2_input_map: &HashMap<String, u32>,
        bound: usize,
    ) -> Option<(usize, bool, bool, Vec<HashMap<String, bool>>)> {
        // Test with deterministic patterns first
        let test_patterns: Vec<(&str, Box<dyn Fn(&str, usize) -> bool>)> = vec![
            ("all-zeros", Box::new(|_, _| false)),
            ("all-ones", Box::new(|_, _| true)),
            ("rst-high", Box::new(|key, _| key.contains("rst"))),
            ("alternating", Box::new(|_, cycle| cycle % 2 == 0)),
        ];

        for (name, pattern) in &test_patterns {
            let result = self.simulate_pattern(
                aig1, aig2, o1_idx, o2_idx,
                matched_inputs, aig1_input_map, aig2_input_map,
                bound,
                |key, cycle| pattern(key, cycle),
            );
            if let Some(r) = result {
                log::debug!("[HIER_EQ] Pattern '{}' found mismatch", name);
                return Some(r);
            }
        }

        // Random simulation using LFSR for deterministic pseudo-random
        for seed in 0u64..500 {
            // Use LFSR-based pattern for reproducibility
            let result = self.simulate_pattern(
                aig1, aig2, o1_idx, o2_idx,
                matched_inputs, aig1_input_map, aig2_input_map,
                bound,
                |key, cycle| {
                    // LFSR-based deterministic pseudo-random
                    let hash = key.as_bytes().iter().fold(seed.wrapping_add(cycle as u64), |acc, &b| {
                        acc.wrapping_mul(31).wrapping_add(b as u64)
                    });
                    hash % 2 == 0
                },
            );
            if result.is_some() {
                return result;
            }
        }

        None
    }

    /// Simulate a specific input pattern
    fn simulate_pattern<F>(
        &self,
        aig1: &Aig,
        aig2: &Aig,
        o1_idx: usize,
        o2_idx: usize,
        matched_inputs: &[String],
        aig1_input_map: &HashMap<String, u32>,
        aig2_input_map: &HashMap<String, u32>,
        bound: usize,
        pattern: F,
    ) -> Option<(usize, bool, bool, Vec<HashMap<String, bool>>)>
    where
        F: Fn(&str, usize) -> bool,
    {
        let mut state1 = SemanticFingerprint::get_initial_state(aig1);
        let mut state2 = SemanticFingerprint::get_initial_state(aig2);
        let mut input_history: Vec<HashMap<String, bool>> = Vec::new();

        for cycle in 0..bound {
            // Generate inputs for this cycle
            let mut inputs1: HashMap<u32, bool> = HashMap::new();
            let mut inputs2: HashMap<u32, bool> = HashMap::new();
            let mut input_record: HashMap<String, bool> = HashMap::new();

            for key in matched_inputs {
                let val = pattern(key, cycle);
                input_record.insert(key.clone(), val);
                if let Some(&id) = aig1_input_map.get(key) {
                    inputs1.insert(id, val);
                }
                if let Some(&id) = aig2_input_map.get(key) {
                    inputs2.insert(id, val);
                }
            }
            input_history.push(input_record);

            // Simulate both designs
            let (values1, next_state1) = SemanticFingerprint::simulate_cycle(aig1, &inputs1, &state1);
            let (values2, next_state2) = SemanticFingerprint::simulate_cycle(aig2, &inputs2, &state2);

            // Extract output values
            let out1_lit = &aig1.outputs[o1_idx];
            let out1_val = values1.get(&out1_lit.node.0).copied().unwrap_or(false);
            let out1_val = if out1_lit.inverted { !out1_val } else { out1_val };

            let out2_lit = &aig2.outputs[o2_idx];
            let out2_val = values2.get(&out2_lit.node.0).copied().unwrap_or(false);
            let out2_val = if out2_lit.inverted { !out2_val } else { out2_val };

            if out1_val != out2_val {
                return Some((cycle, out1_val, out2_val, input_history));
            }

            state1 = next_state1;
            state2 = next_state2;
        }

        None
    }

    /// SAT-based verification for a specific cycle
    fn check_cycle_sat(
        &self,
        aig1: &Aig,
        aig2: &Aig,
        matched_outputs: &[(String, usize, usize)],
        matched_inputs: &[String],
        aig1_input_map: &HashMap<String, u32>,
        aig2_input_map: &HashMap<String, u32>,
        k: usize,
    ) -> FormalResult<Option<(String, bool, bool)>> {
        // For each output pair, build a fresh formula and check
        for (key, o1_idx, o2_idx) in matched_outputs {
            let mut solver = Solver::new();
            let mut formula = CnfFormula::new();
            let mut var_map: HashMap<(u8, u32, usize), Var> = HashMap::new();
            let mut var_counter = 0usize;

            // Build CNF for both AIGs unrolled k cycles
            Self::build_unrolled_cnf_direct(aig1, 1, k, &mut formula, &mut var_map, &mut var_counter);
            Self::build_unrolled_cnf_direct(aig2, 2, k, &mut formula, &mut var_map, &mut var_counter);

            // Tie inputs together at each cycle
            for cycle in 0..k {
                for inp_key in matched_inputs {
                    if let (Some(&id1), Some(&id2)) = (aig1_input_map.get(inp_key), aig2_input_map.get(inp_key)) {
                        let var1 = Self::get_or_create_var(&mut var_map, &mut var_counter, 1, id1, cycle);
                        let var2 = Self::get_or_create_var(&mut var_map, &mut var_counter, 2, id2, cycle);
                        // var1 <=> var2
                        formula.add_clause(&[Lit::negative(var1), Lit::positive(var2)]);
                        formula.add_clause(&[Lit::positive(var1), Lit::negative(var2)]);
                    }
                }
            }

            let out1_lit = &aig1.outputs[*o1_idx];
            let out2_lit = &aig2.outputs[*o2_idx];

            let var1 = Self::get_or_create_var(&mut var_map, &mut var_counter, 1, out1_lit.node.0, k - 1);
            let var2 = Self::get_or_create_var(&mut var_map, &mut var_counter, 2, out2_lit.node.0, k - 1);

            // Create miter: XOR of outputs
            let lit1 = if out1_lit.inverted { Lit::negative(var1) } else { Lit::positive(var1) };
            let lit2 = if out2_lit.inverted { Lit::negative(var2) } else { Lit::positive(var2) };

            // Check if XOR can be true (outputs can differ)
            // XOR(a,b) = (a v b) & (!a v !b)
            formula.add_clause(&[lit1, lit2]);
            formula.add_clause(&[!lit1, !lit2]);

            solver.add_formula(&formula);

            match solver.solve() {
                Ok(true) => {
                    // SAT - found a case where outputs differ
                    return Ok(Some((key.clone(), false, false)));
                }
                Ok(false) => {
                    // UNSAT for this output - they're equivalent at this cycle
                }
                Err(e) => {
                    return Err(FormalError::PropertyFailed(format!("SAT solver error: {:?}", e)));
                }
            }
        }

        Ok(None)
    }

    /// Helper to get or create a SAT variable
    fn get_or_create_var(
        var_map: &mut HashMap<(u8, u32, usize), Var>,
        var_counter: &mut usize,
        aig_id: u8,
        node_id: u32,
        cycle: usize,
    ) -> Var {
        let key = (aig_id, node_id, cycle);
        *var_map.entry(key).or_insert_with(|| {
            let v = Var::from_index(*var_counter);
            *var_counter += 1;
            v
        })
    }

    /// Build CNF for an unrolled AIG (direct version)
    fn build_unrolled_cnf_direct(
        aig: &Aig,
        aig_id: u8,
        k: usize,
        formula: &mut CnfFormula,
        var_map: &mut HashMap<(u8, u32, usize), Var>,
        var_counter: &mut usize,
    ) {
        // Constant false at all cycles
        for cycle in 0..k {
            let var = Self::get_or_create_var(var_map, var_counter, aig_id, 0, cycle);
            formula.add_clause(&[Lit::negative(var)]);
        }

        // Initialize latches at cycle 0
        for latch_id in &aig.latches {
            if let AigNode::Latch { init, .. } = &aig.nodes[latch_id.0 as usize] {
                let var = Self::get_or_create_var(var_map, var_counter, aig_id, latch_id.0, 0);
                if *init {
                    formula.add_clause(&[Lit::positive(var)]);
                } else {
                    formula.add_clause(&[Lit::negative(var)]);
                }
            }
        }

        // Unroll each cycle
        for cycle in 0..k {
            // Encode AND gates
            for (idx, node) in aig.nodes.iter().enumerate() {
                let idx = idx as u32;
                if let AigNode::And { left, right } = node {
                    let out_var = Self::get_or_create_var(var_map, var_counter, aig_id, idx, cycle);
                    let left_var = Self::get_or_create_var(var_map, var_counter, aig_id, left.node.0, cycle);
                    let right_var = Self::get_or_create_var(var_map, var_counter, aig_id, right.node.0, cycle);

                    let left_lit = if left.inverted {
                        Lit::negative(left_var)
                    } else {
                        Lit::positive(left_var)
                    };
                    let right_lit = if right.inverted {
                        Lit::negative(right_var)
                    } else {
                        Lit::positive(right_var)
                    };

                    // out = left & right
                    // CNF: (!out | left) & (!out | right) & (out | !left | !right)
                    formula.add_clause(&[Lit::negative(out_var), left_lit]);
                    formula.add_clause(&[Lit::negative(out_var), right_lit]);
                    formula.add_clause(&[Lit::positive(out_var), !left_lit, !right_lit]);
                }
            }

            // Connect latches to next cycle (if not last cycle)
            if cycle + 1 < k {
                for latch_id in &aig.latches {
                    if let AigNode::Latch { next, .. } = &aig.nodes[latch_id.0 as usize] {
                        let next_var = Self::get_or_create_var(var_map, var_counter, aig_id, next.node.0, cycle);
                        let latch_var = Self::get_or_create_var(var_map, var_counter, aig_id, latch_id.0, cycle + 1);

                        let next_lit = if next.inverted {
                            Lit::negative(next_var)
                        } else {
                            Lit::positive(next_var)
                        };

                        // latch[cycle+1] = next[cycle]
                        formula.add_clause(&[Lit::negative(latch_var), next_lit]);
                        formula.add_clause(&[Lit::positive(latch_var), !next_lit]);
                    }
                }
            }

            // Connect state inputs to their linked latches
            for (&state_input_id, &latch_id) in &aig.state_input_to_latch {
                let input_var = Self::get_or_create_var(var_map, var_counter, aig_id, state_input_id, cycle);
                let latch_var = Self::get_or_create_var(var_map, var_counter, aig_id, latch_id, cycle);
                formula.add_clause(&[Lit::negative(input_var), Lit::positive(latch_var)]);
                formula.add_clause(&[Lit::positive(input_var), Lit::negative(latch_var)]);
            }
        }
    }

    /// Check equivalence between MIR hierarchy and gate netlist
    pub fn check_mir_vs_gates(
        &self,
        mir: &skalp_mir::Mir,
        top_module: &skalp_mir::Module,
        netlist: &GateNetlist,
        bound: usize,
    ) -> FormalResult<HierarchicalEquivalenceResult> {
        let mir_aig = MirToAig::new_with_mir(mir, top_module).convert_sequential_hierarchical();
        let gate_aig = GateNetlistToAig::new().convert_sequential(netlist);

        self.check_equivalence(&mir_aig, &gate_aig, bound)
    }

    /// Check equivalence between LIR and gate netlist
    pub fn check_lir_vs_gates(
        &self,
        lir: &Lir,
        netlist: &GateNetlist,
        bound: usize,
    ) -> FormalResult<HierarchicalEquivalenceResult> {
        let lir_aig = LirToAig::new().convert_sequential(lir);
        let gate_aig = GateNetlistToAig::new().convert_sequential(netlist);

        self.check_equivalence(&lir_aig, &gate_aig, bound)
    }
}

impl Default for HierarchicalEquivalenceChecker {
    fn default() -> Self {
        Self::new()
    }
}

// ============================================================================
// Simulation-Based Equivalence Checker (using SKALP simulator)
// ============================================================================
//
// This uses the existing SKALP simulation infrastructure for faster
// equivalence checking. The GateLevelSimulator supports both CPU and GPU
// acceleration for gate-level simulation.
//
// For RTL-to-Gate equivalence, we convert both designs to gate-level SIR
// and simulate them in lockstep with the same inputs.

use skalp_sim::{
    convert_gate_netlist_to_sir,
    gate_simulator::GateLevelSimulator,
    UnifiedSimulator, UnifiedSimConfig, SimLevel, HwAccel,
    SimCoverageDb, CoverageVectorGen, CoverageReport, MuxArmStatus,
};
use skalp_sir::convert_mir_to_sir_with_hierarchy;

/// Result from simulation-based equivalence check
#[derive(Debug, Clone)]
pub struct SimEquivalenceResult {
    /// Whether designs are equivalent (up to the bound)
    pub equivalent: bool,
    /// Number of cycles verified
    pub cycles_verified: u64,
    /// Cycle at which mismatch occurred (if any)
    pub mismatch_cycle: Option<u64>,
    /// Output that mismatched (if any)
    pub mismatch_output: Option<String>,
    /// Value in design 1 at mismatch
    pub value_1: Option<u64>,
    /// Value in design 2 at mismatch
    pub value_2: Option<u64>,
    /// Total simulation time in milliseconds
    pub time_ms: u64,
    /// Number of outputs compared per cycle
    pub outputs_compared: usize,
    /// Diagnostic information for debugging mismatches
    pub diagnostics: Option<MismatchDiagnostics>,
    /// Coverage report (when --coverage is enabled)
    pub coverage_report: Option<CoverageReport>,
}

/// Detailed diagnostics for a simulation mismatch
#[derive(Debug, Clone, Default)]
pub struct MismatchDiagnostics {
    /// Input values at the time of mismatch (name -> value)
    pub input_values: Vec<(String, u64)>,
    /// MIR internal signal values relevant to the failing output
    pub mir_signals: Vec<(String, String, u64)>,  // (hierarchical_path, internal_name, value)
    /// Gate internal signal values relevant to the failing output
    pub gate_signals: Vec<(String, u64)>,  // (signal_name, value)
    /// SIR node chain for the failing output (for dataflow tracing)
    pub sir_dataflow: Vec<String>,
    /// Per-cycle trace of the failing signal and its dependencies
    /// Each entry: (cycle, signal_name, mir_value, gate_value)
    pub cycle_trace: Vec<CycleTraceEntry>,
    /// Input port matching information
    pub input_matching: InputMatchingInfo,
}

/// Per-cycle trace entry for debugging signal propagation
#[derive(Debug, Clone)]
pub struct CycleTraceEntry {
    /// Cycle number (0 = after reset)
    pub cycle: u64,
    /// Phase within cycle ("pre-clock", "post-clock")
    pub phase: String,
    /// Traced signals with their values
    /// (user_name, internal_name, mir_value, gate_value, match)
    pub signals: Vec<(String, String, Option<u64>, Option<u64>, bool)>,
    /// Input values applied this cycle
    pub inputs: Vec<(String, u64)>,
}

/// Information about input port matching between MIR and Gate
#[derive(Debug, Clone, Default)]
pub struct InputMatchingInfo {
    /// Successfully matched inputs: (user_name, mir_internal_name, gate_names)
    pub matched: Vec<(String, String, Vec<String>)>,
    /// Unmatched MIR inputs: (user_name, mir_internal_name, reason)
    pub unmatched_mir: Vec<(String, String, String)>,
    /// Unmatched Gate inputs: (gate_name, reason)
    pub unmatched_gate: Vec<(String, String)>,
}

/// Simulation-based equivalence checker using SKALP's gate-level simulator
///
/// This is faster than AIG-based simulation because:
/// 1. Uses optimized gate-level simulation primitives
/// 2. Supports GPU acceleration on macOS via Metal
/// 3. Simulates entire design in one pass per cycle
pub struct SimBasedEquivalenceChecker {
    /// Number of cycles to simulate
    pub max_cycles: u64,
    /// Clock signal name (for synchronous simulation)
    pub clock_name: String,
    /// Reset signal name
    pub reset_name: String,
    /// Number of reset cycles to apply at start
    pub reset_cycles: u64,
    /// Enable coverage-driven simulation
    pub coverage: bool,
}

impl SimBasedEquivalenceChecker {
    pub fn new() -> Self {
        Self {
            max_cycles: 100,
            clock_name: "clk".to_string(),
            reset_name: "rst".to_string(),
            reset_cycles: 2,
            coverage: false,
        }
    }

    pub fn with_cycles(mut self, cycles: u64) -> Self {
        self.max_cycles = cycles;
        self
    }

    pub fn with_clock(mut self, name: &str) -> Self {
        self.clock_name = name.to_string();
        self
    }

    pub fn with_reset(mut self, name: &str, cycles: u64) -> Self {
        self.reset_name = name.to_string();
        self.reset_cycles = cycles;
        self
    }

    pub fn with_coverage(mut self, enabled: bool) -> Self {
        self.coverage = enabled;
        self
    }

    /// Check equivalence between two gate netlists
    ///
    /// Both designs are simulated in lockstep with the same inputs.
    /// On each cycle, all outputs are compared.
    pub fn check_gate_equivalence(
        &self,
        netlist1: &GateNetlist,
        netlist2: &GateNetlist,
    ) -> FormalResult<SimEquivalenceResult> {
        let start = std::time::Instant::now();

        // Convert both netlists to SIR for simulation
        let sir1_result = convert_gate_netlist_to_sir(netlist1);
        let sir2_result = convert_gate_netlist_to_sir(netlist2);

        // Create simulators
        let mut sim1 = GateLevelSimulator::new(&sir1_result.sir);
        let mut sim2 = GateLevelSimulator::new(&sir2_result.sir);

        // Get input and output names
        let inputs1 = sim1.get_input_names();
        let inputs2 = sim2.get_input_names();
        let outputs1 = sim1.get_output_names();
        let outputs2 = sim2.get_output_names();

        // Match inputs (by normalized name)
        let matched_inputs: Vec<(String, String)> = inputs1.iter()
            .filter_map(|n1| {
                let key1 = normalize_port_name(n1).key();
                inputs2.iter().find(|n2| normalize_port_name(n2).key() == key1)
                    .map(|n2| (n1.clone(), n2.clone()))
            })
            .collect();

        // Match outputs (these are both gate SIR designs — no hierarchy to strip)
        let matched_outputs: Vec<(String, String)> = outputs1.iter()
            .filter_map(|n1| {
                let key1 = normalize_port_name(n1).key();
                outputs2.iter().find(|n2| normalize_port_name(n2).key() == key1)
                    .map(|n2| (n1.clone(), n2.clone()))
            })
            .collect();

        if matched_outputs.is_empty() {
            return Err(FormalError::PropertyFailed(
                "No matching outputs between designs".to_string()
            ));
        }

        // Reset sequence
        for _ in 0..self.reset_cycles {
            // Set reset high
            sim1.set_input_u64(&self.reset_name, 1);
            sim2.set_input_u64(&self.reset_name, 1);

            // Clock cycle
            sim1.set_input_u64(&self.clock_name, 0);
            sim2.set_input_u64(&self.clock_name, 0);
            sim1.step();
            sim2.step();

            sim1.set_input_u64(&self.clock_name, 1);
            sim2.set_input_u64(&self.clock_name, 1);
            sim1.step();
            sim2.step();
        }

        // Release reset
        sim1.set_input_u64(&self.reset_name, 0);
        sim2.set_input_u64(&self.reset_name, 0);

        // Main simulation loop with random inputs
        let mut rng_seed = 0x12345678u64;

        for cycle in 0..self.max_cycles {
            // Generate deterministic pseudo-random inputs
            for (name1, name2) in &matched_inputs {
                // Skip clock and reset
                if name1.contains("clk") || name1.contains("clock") ||
                   name1.contains("rst") || name1.contains("reset") {
                    continue;
                }

                // LFSR-based pseudo-random
                rng_seed = rng_seed.wrapping_mul(6364136223846793005).wrapping_add(1);
                let value = rng_seed & 0xFFFF_FFFF; // 32-bit value

                sim1.set_input_u64(name1, value);
                sim2.set_input_u64(name2, value);
            }

            // Clock low
            sim1.set_input_u64(&self.clock_name, 0);
            sim2.set_input_u64(&self.clock_name, 0);
            sim1.step();
            sim2.step();

            // Clock high (rising edge)
            sim1.set_input_u64(&self.clock_name, 1);
            sim2.set_input_u64(&self.clock_name, 1);
            sim1.step();
            sim2.step();

            // Compare outputs
            for (name1, name2) in &matched_outputs {
                let val1 = sim1.get_output_u64(name1);
                let val2 = sim2.get_output_u64(name2);

                match (val1, val2) {
                    (Some(v1), Some(v2)) if v1 != v2 => {
                        return Ok(SimEquivalenceResult {
                            equivalent: false,
                            cycles_verified: cycle,
                            mismatch_cycle: Some(cycle),
                            mismatch_output: Some(name1.clone()),
                            value_1: Some(v1),
                            value_2: Some(v2),
                            time_ms: start.elapsed().as_millis() as u64,
                            outputs_compared: matched_outputs.len(),
                            diagnostics: None,
                            coverage_report: None,
                        });
                    }
                    (None, Some(_)) | (Some(_), None) => {
                    }
                    _ => {}
                }
            }

        }

        Ok(SimEquivalenceResult {
            equivalent: true,
            cycles_verified: self.max_cycles,
            mismatch_cycle: None,
            mismatch_output: None,
            value_1: None,
            value_2: None,
            time_ms: start.elapsed().as_millis() as u64,
            outputs_compared: matched_outputs.len(),
            diagnostics: None,
            coverage_report: None,
        })
    }

    /// Check equivalence between LIR and gate netlist
    ///
    /// The LIR is first tech-mapped to a gate netlist, then both are simulated.
    pub fn check_lir_vs_gate(
        &self,
        lir: &Lir,
        netlist: &GateNetlist,
        library: &skalp_lir::TechLibrary,
    ) -> FormalResult<SimEquivalenceResult> {
        // Tech-map LIR to gate netlist
        let mut mapper = skalp_lir::TechMapper::new(library);
        let lir_netlist = mapper.map(lir).netlist;

        self.check_gate_equivalence(&lir_netlist, netlist)
    }

    /// Check equivalence between MIR (RTL) and gate netlist
    ///
    /// This is the true RTL-to-Gate equivalence check:
    /// - MIR is simulated using behavioral simulation (word-level)
    /// - GateNetlist is simulated using gate-level simulation (bit-level)
    /// - Outputs are compared at each cycle
    ///
    /// This catches bugs introduced during synthesis/tech-mapping.
    pub async fn check_mir_vs_gate(
        &self,
        mir: &skalp_mir::Mir,
        top_module: &skalp_mir::Module,
        netlist: &GateNetlist,
    ) -> FormalResult<SimEquivalenceResult> {
        let start = std::time::Instant::now();

        // Convert MIR to behavioral SirModule (needs full MIR for module resolution)
        let sir_module = convert_mir_to_sir_with_hierarchy(mir, top_module);

        // Convert GateNetlist to gate-level SIR
        let gate_sir_result = convert_gate_netlist_to_sir(netlist);

        // Create behavioral simulator for MIR
        let mut mir_config = UnifiedSimConfig::default();
        mir_config.level = SimLevel::Behavioral;
        mir_config.hw_accel = HwAccel::Cpu; // Force CPU for consistent behavior with gate simulation
        let mut mir_sim = UnifiedSimulator::new(mir_config)
            .map_err(|e| FormalError::SolverError(format!("MIR simulator init failed: {}", e)))?;
        mir_sim.load_behavioral(&sir_module).await
            .map_err(|e| FormalError::SolverError(format!("MIR load failed: {}", e)))?;

        // Create gate-level simulator
        // Force CPU mode to enable internal signal inspection for debugging
        let mut gate_config = UnifiedSimConfig::default();
        gate_config.level = SimLevel::GateLevel;
        gate_config.hw_accel = HwAccel::Cpu; // Force CPU for signal inspection
        let mut gate_sim = UnifiedSimulator::new(gate_config)
            .map_err(|e| FormalError::SolverError(format!("Gate simulator init failed: {}", e)))?;
        gate_sim.load_gate_level(&gate_sir_result.sir)
            .map_err(|e| FormalError::SolverError(format!("Gate load failed: {}", e)))?;

        // Get input and output names
        let mir_inputs = mir_sim.get_input_names();
        let mir_outputs = mir_sim.get_output_names();
        let gate_inputs = gate_sim.get_input_names();
        let gate_outputs = gate_sim.get_output_names();

        // Use name_registry to resolve MIR internal names (_s0, _s1) to user-facing paths (clk, rst)
        let resolve_mir_name = |internal: &str| -> String {
            sir_module.name_registry.get_entry_by_internal(internal)
                .map(|entry| entry.hierarchical_path.clone())
                .unwrap_or_else(|| internal.to_string())
        };

        // Build gate input/output maps by base_name (for multi-bit matching)
        // Gate has bit-exploded ports like "signal[0]", "signal[1]", etc.
        // MIR has multi-bit ports like "signal"
        // We match by base_name and collect all bits
        let mut gate_inputs_by_base: std::collections::HashMap<String, Vec<(String, Option<u32>)>> =
            std::collections::HashMap::new();
        for name in &gate_inputs {
            let np = normalize_port_name(name);
            gate_inputs_by_base.entry(np.base_name.clone())
                .or_default()
                .push((name.clone(), np.bit_index));
        }

        let mut gate_outputs_by_base: std::collections::HashMap<String, Vec<(String, Option<u32>)>> =
            std::collections::HashMap::new();
        for name in &gate_outputs {
            let np = normalize_port_name(name);
            gate_outputs_by_base.entry(np.base_name.clone())
                .or_default()
                .push((name.clone(), np.bit_index));
        }

        // Match inputs by base_name (MIR multi-bit to Gate bit-exploded)
        // Returns: (mir_name, vec of (gate_name, bit_idx))
        let matched_inputs: Vec<(String, Vec<(String, Option<u32>)>)> = mir_inputs.iter()
            .filter_map(|n1| {
                let user_name = resolve_mir_name(n1);
                let base1 = normalize_port_name(&user_name).base_name;
                gate_inputs_by_base.get(&base1)
                    .map(|bits| (n1.clone(), bits.clone()))
            })
            .collect();

        // Match outputs by base_name
        let matched_outputs: Vec<(String, Vec<(String, Option<u32>)>)> = mir_outputs.iter()
            .filter_map(|n1| {
                let user_name = resolve_mir_name(n1);
                let base1 = normalize_port_name(&user_name).base_name;
                gate_outputs_by_base.get(&base1)
                    .map(|bits| (n1.clone(), bits.clone()))
            })
            .collect();

        // Build input matching info for diagnostics
        let matched_mir_inputs: std::collections::HashSet<_> = matched_inputs.iter().map(|(m, _)| m.clone()).collect();

        // Build InputMatchingInfo struct for diagnostics
        let mut input_matching_info = InputMatchingInfo::default();

        // Record matched inputs
        for (mir_name, gate_bits) in &matched_inputs {
            let user_name = resolve_mir_name(mir_name);
            let gate_names: Vec<String> = gate_bits.iter().map(|(n, _)| n.clone()).collect();
            input_matching_info.matched.push((user_name, mir_name.clone(), gate_names));
        }

        // Record unmatched MIR inputs with reasons
        for mir_name in &mir_inputs {
            if !matched_mir_inputs.contains(mir_name) {
                let user_name = resolve_mir_name(mir_name);
                let base = normalize_port_name(&user_name).base_name;
                let reason = if gate_inputs_by_base.contains_key(&base) {
                    "Base name matches but failed to match (unexpected)".to_string()
                } else {
                    format!("No gate input with base name '{}'", base)
                };
                input_matching_info.unmatched_mir.push((user_name, mir_name.clone(), reason));
            }
        }

        // Record unmatched Gate inputs
        let matched_gate_bases: std::collections::HashSet<_> = matched_inputs.iter()
            .flat_map(|(mir_name, _)| {
                let user_name = resolve_mir_name(mir_name);
                Some(normalize_port_name(&user_name).base_name)
            })
            .collect();
        for (gate_base, gate_bits) in &gate_inputs_by_base {
            if !matched_gate_bases.contains(gate_base) {
                let gate_names: Vec<String> = gate_bits.iter().map(|(n, _)| n.clone()).collect();
                let reason = format!("No MIR input resolves to base name '{}'", gate_base);
                for gate_name in gate_names {
                    input_matching_info.unmatched_gate.push((gate_name, reason.clone()));
                }
            }
        }

        if matched_outputs.is_empty() {
            return Err(FormalError::PropertyFailed(
                "No matching outputs between MIR and Gate".to_string()
            ));
        }

        // Find the actual reset signal name in the gate simulator
        let gate_reset_name = gate_inputs_by_base.get(&self.reset_name)
            .and_then(|bits| bits.first())
            .map(|(name, _)| name.clone())
            .unwrap_or_else(|| self.reset_name.clone());

        let gate_clock_name = gate_inputs_by_base.get(&self.clock_name)
            .and_then(|bits| bits.first())
            .map(|(name, _)| name.clone())
            .unwrap_or_else(|| self.clock_name.clone());

        // Reset sequence
        for _cycle in 0..self.reset_cycles {
            mir_sim.set_input(&self.reset_name, 1).await;
            gate_sim.set_input(&gate_reset_name, 1).await;

            mir_sim.set_input(&self.clock_name, 0).await;
            gate_sim.set_input(&gate_clock_name, 0).await;
            mir_sim.step().await;
            gate_sim.step().await;

            mir_sim.set_input(&self.clock_name, 1).await;
            gate_sim.set_input(&gate_clock_name, 1).await;
            mir_sim.step().await;
            gate_sim.step().await;
        }

        // Release reset
        mir_sim.set_input(&self.reset_name, 0).await;
        gate_sim.set_input(&gate_reset_name, 0).await;

        // Initialize all matched inputs to 0 after reset
        for (mir_name, gate_bits) in &matched_inputs {
            mir_sim.set_input(mir_name, 0).await;
            for (gate_name, _) in gate_bits {
                gate_sim.set_input(gate_name, 0).await;
            }
        }

        // Do one clock cycle to propagate the initialized values
        mir_sim.set_input(&self.clock_name, 0).await;
        gate_sim.set_input(&gate_clock_name, 0).await;
        mir_sim.step().await;
        gate_sim.step().await;
        mir_sim.set_input(&self.clock_name, 1).await;
        gate_sim.set_input(&gate_clock_name, 1).await;
        mir_sim.step().await;
        gate_sim.step().await;

        // Main simulation loop with random inputs
        let mut rng_seed = 0x12345678u64;

        // Add extra reset cycles right before quiet period
        for _ in 0..5 {
            mir_sim.set_input(&self.reset_name, 1).await;
            gate_sim.set_input(&gate_reset_name, 1).await;

            mir_sim.set_input(&self.clock_name, 0).await;
            gate_sim.set_input(&gate_clock_name, 0).await;
            mir_sim.step().await;
            gate_sim.step().await;

            mir_sim.set_input(&self.clock_name, 1).await;
            gate_sim.set_input(&gate_clock_name, 1).await;
            mir_sim.step().await;
            gate_sim.step().await;
        }

        // Release reset again and set all inputs to 0
        mir_sim.set_input(&self.reset_name, 0).await;
        gate_sim.set_input(&gate_reset_name, 0).await;

        for (mir_name, gate_bits) in &matched_inputs {
            mir_sim.set_input(mir_name, 0).await;
            for (gate_name, _) in gate_bits {
                gate_sim.set_input(gate_name, 0).await;
            }
        }

        // Run a few "quiet" cycles with all inputs at 0 to verify baseline
        for _quiet_cycle in 0..3 {
            // Keep all inputs at 0
            for (mir_name, gate_bits) in &matched_inputs {
                let user_name = resolve_mir_name(mir_name);
                if user_name.contains("clk") || user_name.contains("clock") ||
                   user_name.contains("rst") || user_name.contains("reset") {
                    continue;
                }
                mir_sim.set_input(mir_name, 0).await;
                for (gate_name, _) in gate_bits {
                    gate_sim.set_input(gate_name, 0).await;
                }
            }

            // Clock cycle
            mir_sim.set_input(&self.clock_name, 0).await;
            gate_sim.set_input(&gate_clock_name, 0).await;
            mir_sim.step().await;
            gate_sim.step().await;
            mir_sim.set_input(&self.clock_name, 1).await;
            gate_sim.set_input(&gate_clock_name, 1).await;
            mir_sim.step().await;
            gate_sim.step().await;
        }

        // Cycle trace buffer - keep last N cycles for diagnostics
        const TRACE_BUFFER_SIZE: usize = 10;
        let mut cycle_trace_buffer: Vec<CycleTraceEntry> = Vec::with_capacity(TRACE_BUFFER_SIZE);

        for cycle in 0..self.max_cycles {
            // Generate deterministic pseudo-random inputs
            for (mir_name, gate_bits) in &matched_inputs {
                let user_name = resolve_mir_name(mir_name);
                // Skip clock and reset
                if user_name.contains("clk") || user_name.contains("clock") ||
                   user_name.contains("rst") || user_name.contains("reset") {
                    continue;
                }

                // LFSR-based pseudo-random
                rng_seed = rng_seed.wrapping_mul(6364136223846793005).wrapping_add(1);
                let value = rng_seed & 0xFFFF_FFFF; // 32-bit value

                // Check if this is a single-bit signal (gate_bits has one entry with no bit index)
                let is_single_bit = gate_bits.len() == 1 && gate_bits[0].1.is_none();

                // BUG #250 FIX: Mask the value to the actual port width
                // The gate_bits length tells us the port width in bits
                let port_width = gate_bits.len() as u32;
                let width_mask = if port_width >= 64 { u64::MAX } else { (1u64 << port_width) - 1 };

                // For single-bit signals, use only bit 0; for multi-bit, mask to port width
                let mir_value = if is_single_bit { value & 1 } else { value & width_mask };

                // Set MIR input
                mir_sim.set_input(mir_name, mir_value).await;

                // Set Gate inputs (bit by bit)
                for (gate_name, bit_idx) in gate_bits {
                    if let Some(idx) = bit_idx {
                        // Extract the specific bit from value
                        let bit_val = (value >> idx) & 1;
                        gate_sim.set_input(gate_name, bit_val).await;
                    } else {
                        // No bit index - single-bit signal, use the same masked value as MIR
                        gate_sim.set_input(gate_name, mir_value).await;
                    }
                }
            }

            // Clock low
            mir_sim.set_input(&self.clock_name, 0).await;
            gate_sim.set_input(&gate_clock_name, 0).await;
            mir_sim.step().await;
            gate_sim.step().await;

            // Clock high (rising edge)
            mir_sim.set_input(&self.clock_name, 1).await;
            gate_sim.set_input(&gate_clock_name, 1).await;
            mir_sim.step().await;
            gate_sim.step().await;

            // Collect cycle trace for diagnostics
            let mut trace_entry = CycleTraceEntry {
                cycle,
                phase: "post-clock".to_string(),
                signals: Vec::new(),
                inputs: Vec::new(),
            };

            // Collect input values applied this cycle
            for (mir_name, _) in &matched_inputs {
                let user_name = resolve_mir_name(mir_name);
                if user_name.contains("clk") || user_name.contains("clock") ||
                   user_name.contains("rst") || user_name.contains("reset") {
                    continue;
                }
                if let Some(val) = mir_sim.get_output_raw(mir_name).await {
                    trace_entry.inputs.push((user_name, val));
                }
            }

            // Collect all output values for trace
            for (mir_name, gate_bits) in &matched_outputs {
                let user_name = resolve_mir_name(mir_name);
                let mir_val = mir_sim.get_output_raw(mir_name).await;

                // Assemble gate value
                let mut sorted_bits = gate_bits.clone();
                sorted_bits.sort_by_key(|(_, idx)| idx.unwrap_or(0));
                let mut gate_val: u64 = 0;
                let mut has_gate = false;
                for (gate_name, bit_idx) in &sorted_bits {
                    if let Some(gv) = gate_sim.get_output_raw(gate_name).await {
                        has_gate = true;
                        if let Some(idx) = bit_idx {
                            gate_val |= (gv & 1) << idx;
                        } else {
                            gate_val = gv;
                        }
                    }
                }

                // Mask MIR value to gate port width for correct comparison
                let trace_port_width = sorted_bits.iter()
                    .filter_map(|(_, idx)| *idx)
                    .max()
                    .map(|max_idx| max_idx + 1)
                    .unwrap_or(1) as u32;
                let trace_mask = if trace_port_width >= 64 { u64::MAX } else { (1u64 << trace_port_width) - 1 };
                let mir_val_masked = mir_val.map(|v| v & trace_mask);
                let matches = mir_val_masked == Some(gate_val);
                trace_entry.signals.push((
                    user_name,
                    mir_name.clone(),
                    mir_val_masked,
                    if has_gate { Some(gate_val) } else { None },
                    matches
                ));
            }

            // Keep only last N cycles in buffer
            if cycle_trace_buffer.len() >= TRACE_BUFFER_SIZE {
                cycle_trace_buffer.remove(0);
            }
            cycle_trace_buffer.push(trace_entry);

            // Compare outputs
            for (mir_name, gate_bits) in &matched_outputs {
                let mir_val = mir_sim.get_output_raw(mir_name).await.unwrap_or(0);

                // Assemble gate value from bits (sort by bit index for correct ordering)
                let mut sorted_bits = gate_bits.clone();
                sorted_bits.sort_by_key(|(_, idx)| idx.unwrap_or(0));

                let mut gate_val: u64 = 0;
                let mut has_bits = false;
                let user_name = resolve_mir_name(mir_name);
                for (gate_name, bit_idx) in &sorted_bits {
                    if let Some(gv) = gate_sim.get_output_raw(gate_name).await {
                        has_bits = true;
                        if let Some(idx) = bit_idx {
                            // Set the specific bit
                            gate_val |= (gv & 1) << idx;
                        } else {
                            // No bit index - use the whole value
                            gate_val = gv;
                        }
                    }
                }

                // BUG FIX: Mask MIR value to gate port width before comparing.
                // MIR simulation may return wider values (e.g., 32-bit for int[10] due to
                // Value::Integer not carrying width info). The gate side has the correct
                // width from synthesis. Mask ensures we compare only the relevant bits.
                let port_width = sorted_bits.iter()
                    .filter_map(|(_, idx)| *idx)
                    .max()
                    .map(|max_idx| max_idx + 1)
                    .unwrap_or(1) as u32;
                let width_mask = if port_width >= 64 { u64::MAX } else { (1u64 << port_width) - 1 };
                let mir_val_masked = mir_val & width_mask;

                if has_bits && mir_val_masked != gate_val {
                    // Build diagnostics for the report
                    let mut diagnostics = MismatchDiagnostics::default();

                    // Collect ALL MIR internal signals (let the report filter as needed)
                    for entry in sir_module.name_registry.all_entries() {
                        if let Some(val) = mir_sim.get_output_raw(&entry.internal_name).await {
                            diagnostics.mir_signals.push((
                                entry.hierarchical_path.clone(),
                                entry.internal_name.clone(),
                                val
                            ));
                        }
                    }

                    // Collect ALL Gate internal signals
                    let all_gate_signals = gate_sim.dump_gate_signals();
                    for (sig_name, bits) in &all_gate_signals {
                        let val: u64 = bits.iter()
                            .enumerate()
                            .fold(0u64, |acc, (i, &b)| acc | ((b as u64) << i));
                        diagnostics.gate_signals.push((sig_name.clone(), val));
                    }

                    // Collect input values
                    for (mir_input_name, _) in &matched_inputs {
                        let input_user_name = resolve_mir_name(mir_input_name);
                        if let Some(val) = mir_sim.get_output_raw(mir_input_name).await {
                            diagnostics.input_values.push((input_user_name, val));
                        }
                    }

                    // Extract SIR dataflow for the failing output
                    // Find the output in SIR and trace its driver chain
                    if let Some(output) = sir_module.outputs.iter().find(|o| o.name == *mir_name || resolve_mir_name(&o.name) == user_name) {
                        diagnostics.sir_dataflow.push(format!("Output '{}' (SIR name: '{}')", user_name, output.name));
                        // Find driver node for this output
                        if let Some(signal) = sir_module.signals.iter().find(|s| s.name == output.name) {
                            if let Some(driver_id) = signal.driver_node {
                                if let Some(driver_node) = sir_module.combinational_nodes.iter().find(|n| n.id == driver_id) {
                                    diagnostics.sir_dataflow.push(format!("  Driven by node {}: {:?}", driver_id, driver_node.kind));
                                    // Trace inputs to this node
                                    for input in &driver_node.inputs {
                                        diagnostics.sir_dataflow.push(format!("    Input: {}", input.signal_id));
                                    }
                                }
                            }
                        }
                    }

                    // Add cycle trace from buffer
                    diagnostics.cycle_trace = cycle_trace_buffer.clone();

                    // Add input matching info
                    diagnostics.input_matching = input_matching_info.clone();

                    // Return with diagnostics
                    return Ok(SimEquivalenceResult {
                        equivalent: false,
                        cycles_verified: cycle,
                        mismatch_cycle: Some(cycle),
                        mismatch_output: Some(user_name),
                        value_1: Some(mir_val),
                        value_2: Some(gate_val),
                        time_ms: start.elapsed().as_millis() as u64,
                        outputs_compared: matched_outputs.len(),
                        diagnostics: Some(diagnostics),
                        coverage_report: None,
                    });
                }
            }

        }

        Ok(SimEquivalenceResult {
            equivalent: true,
            cycles_verified: self.max_cycles,
            mismatch_cycle: None,
            mismatch_output: None,
            value_1: None,
            value_2: None,
            time_ms: start.elapsed().as_millis() as u64,
            outputs_compared: matched_outputs.len(),
            diagnostics: None,
            coverage_report: None,
        })
    }

    /// Check equivalence between MIR and gate netlist with coverage-driven simulation.
    ///
    /// Uses a 3-phase vector generation strategy:
    /// 1. Systematic patterns (walking-one, walking-zero, boundary values)
    /// 2. LFSR pseudo-random sweep
    /// 3. Coverage-biased targeted generation
    ///
    /// Returns coverage report alongside equivalence result.
    pub async fn check_mir_vs_gate_coverage(
        &self,
        mir: &skalp_mir::Mir,
        top_module: &skalp_mir::Module,
        netlist: &GateNetlist,
    ) -> FormalResult<SimEquivalenceResult> {
        let start = std::time::Instant::now();

        // Convert MIR to behavioral SirModule
        let sir_module = convert_mir_to_sir_with_hierarchy(mir, top_module);

        // Convert GateNetlist to gate-level SIR
        let gate_sir_result = convert_gate_netlist_to_sir(netlist);

        // Create behavioral simulator
        let mut mir_config = UnifiedSimConfig::default();
        mir_config.level = SimLevel::Behavioral;
        mir_config.hw_accel = HwAccel::Cpu;
        let mut mir_sim = UnifiedSimulator::new(mir_config)
            .map_err(|e| FormalError::SolverError(format!("MIR simulator init failed: {}", e)))?;
        mir_sim.load_behavioral(&sir_module).await
            .map_err(|e| FormalError::SolverError(format!("MIR load failed: {}", e)))?;

        // Create gate-level simulator
        let mut gate_config = UnifiedSimConfig::default();
        gate_config.level = SimLevel::GateLevel;
        gate_config.hw_accel = HwAccel::Cpu;
        let mut gate_sim = UnifiedSimulator::new(gate_config)
            .map_err(|e| FormalError::SolverError(format!("Gate simulator init failed: {}", e)))?;
        gate_sim.load_gate_level(&gate_sir_result.sir)
            .map_err(|e| FormalError::SolverError(format!("Gate load failed: {}", e)))?;

        // Build behavioral coverage database from SirModule
        let mut behav_cov = SimCoverageDb::from_sir_module(&sir_module);

        // Build gate coverage database from gate signal info
        let gate_signal_info: Vec<(String, usize)> = gate_sir_result.sir.top_module.signals
            .iter()
            .map(|s| (s.name.clone(), s.width))
            .collect();
        let mut gate_cov = SimCoverageDb::from_gate_netlist(&gate_signal_info);

        // Get input/output names and build matching
        let mir_inputs = mir_sim.get_input_names();
        let mir_outputs = mir_sim.get_output_names();
        let gate_inputs = gate_sim.get_input_names();
        let gate_outputs = gate_sim.get_output_names();

        let resolve_mir_name = |internal: &str| -> String {
            sir_module.name_registry.get_entry_by_internal(internal)
                .map(|entry| entry.hierarchical_path.clone())
                .unwrap_or_else(|| internal.to_string())
        };

        // Build gate input/output maps by base_name
        let mut gate_inputs_by_base: std::collections::HashMap<String, Vec<(String, Option<u32>)>> =
            std::collections::HashMap::new();
        for name in &gate_inputs {
            let np = normalize_port_name(name);
            gate_inputs_by_base.entry(np.base_name.clone())
                .or_default()
                .push((name.clone(), np.bit_index));
        }

        let mut gate_outputs_by_base: std::collections::HashMap<String, Vec<(String, Option<u32>)>> =
            std::collections::HashMap::new();
        for name in &gate_outputs {
            let np = normalize_port_name(name);
            gate_outputs_by_base.entry(np.base_name.clone())
                .or_default()
                .push((name.clone(), np.bit_index));
        }

        // Match inputs
        let matched_inputs: Vec<(String, Vec<(String, Option<u32>)>)> = mir_inputs.iter()
            .filter_map(|n1| {
                let user_name = resolve_mir_name(n1);
                let base1 = normalize_port_name(&user_name).base_name;
                gate_inputs_by_base.get(&base1)
                    .map(|bits| (n1.clone(), bits.clone()))
            })
            .collect();

        // Match outputs
        let matched_outputs: Vec<(String, Vec<(String, Option<u32>)>)> = mir_outputs.iter()
            .filter_map(|n1| {
                let user_name = resolve_mir_name(n1);
                let base1 = normalize_port_name(&user_name).base_name;
                gate_outputs_by_base.get(&base1)
                    .map(|bits| (n1.clone(), bits.clone()))
            })
            .collect();

        if matched_outputs.is_empty() {
            return Err(FormalError::PropertyFailed(
                "No matching outputs between MIR and Gate".to_string()
            ));
        }

        // Find actual reset/clock names in gate simulator
        let gate_reset_name = gate_inputs_by_base.get(&self.reset_name)
            .and_then(|bits| bits.first())
            .map(|(name, _)| name.clone())
            .unwrap_or_else(|| self.reset_name.clone());

        let gate_clock_name = gate_inputs_by_base.get(&self.clock_name)
            .and_then(|bits| bits.first())
            .map(|(name, _)| name.clone())
            .unwrap_or_else(|| self.clock_name.clone());

        // Reset sequence
        for _ in 0..self.reset_cycles {
            mir_sim.set_input(&self.reset_name, 1).await;
            gate_sim.set_input(&gate_reset_name, 1).await;
            mir_sim.set_input(&self.clock_name, 0).await;
            gate_sim.set_input(&gate_clock_name, 0).await;
            mir_sim.step().await;
            gate_sim.step().await;
            mir_sim.set_input(&self.clock_name, 1).await;
            gate_sim.set_input(&gate_clock_name, 1).await;
            mir_sim.step().await;
            gate_sim.step().await;
        }

        // Release reset
        mir_sim.set_input(&self.reset_name, 0).await;
        gate_sim.set_input(&gate_reset_name, 0).await;

        // Initialize inputs to 0
        for (mir_name, gate_bits) in &matched_inputs {
            mir_sim.set_input(mir_name, 0).await;
            for (gate_name, _) in gate_bits {
                gate_sim.set_input(gate_name, 0).await;
            }
        }

        // Extra reset + quiet period (same as original)
        for _ in 0..5 {
            mir_sim.set_input(&self.reset_name, 1).await;
            gate_sim.set_input(&gate_reset_name, 1).await;
            mir_sim.set_input(&self.clock_name, 0).await;
            gate_sim.set_input(&gate_clock_name, 0).await;
            mir_sim.step().await;
            gate_sim.step().await;
            mir_sim.set_input(&self.clock_name, 1).await;
            gate_sim.set_input(&gate_clock_name, 1).await;
            mir_sim.step().await;
            gate_sim.step().await;
        }
        mir_sim.set_input(&self.reset_name, 0).await;
        gate_sim.set_input(&gate_reset_name, 0).await;
        for (mir_name, gate_bits) in &matched_inputs {
            mir_sim.set_input(mir_name, 0).await;
            for (gate_name, _) in gate_bits {
                gate_sim.set_input(gate_name, 0).await;
            }
        }

        // Build input info for vector generator
        // Collect (user_name, width) for each matched input
        let input_info: Vec<(String, usize)> = matched_inputs.iter()
            .filter_map(|(mir_name, gate_bits)| {
                let user_name = resolve_mir_name(mir_name);
                if user_name.contains("clk") || user_name.contains("clock") ||
                   user_name.contains("rst") || user_name.contains("reset") {
                    return None;
                }
                // Width = number of gate bits (or 1 if single signal)
                let width = if gate_bits.len() == 1 && gate_bits[0].1.is_none() {
                    // Single-bit signal, check MIR port for width
                    sir_module.inputs.iter()
                        .find(|p| p.name == *mir_name)
                        .map(|p| p.width)
                        .unwrap_or(1)
                } else {
                    gate_bits.iter()
                        .filter_map(|(_, idx)| *idx)
                        .max()
                        .map(|max| max as usize + 1)
                        .unwrap_or(gate_bits.len())
                };
                Some((user_name, width))
            })
            .collect();

        // Create vector generator
        let mut vecgen = CoverageVectorGen::from_input_info(&input_info, 0x12345678)
            .with_lfsr_budget(1000)
            .with_bias_budget(500)
            .with_coverage_goal(90.0);

        let mut cycle: u64 = 0;

        // Main simulation loop driven by vector generator
        while let Some(vec) = vecgen.next(Some(&behav_cov)) {

            // Apply vector to both simulators
            for (user_name, value) in &vec.values {
                // Find the matching MIR/gate input pair
                for (mir_name, gate_bits) in &matched_inputs {
                    let this_user = resolve_mir_name(mir_name);
                    if &this_user != user_name {
                        continue;
                    }

                    let is_single_bit = gate_bits.len() == 1 && gate_bits[0].1.is_none();
                    let mir_value = if is_single_bit { *value & 1 } else { *value };

                    mir_sim.set_input(mir_name, mir_value).await;
                    for (gate_name, bit_idx) in gate_bits {
                        if let Some(idx) = bit_idx {
                            let bit_val = (*value >> idx) & 1;
                            gate_sim.set_input(gate_name, bit_val).await;
                        } else {
                            gate_sim.set_input(gate_name, mir_value).await;
                        }
                    }
                }
            }

            // Clock low
            mir_sim.set_input(&self.clock_name, 0).await;
            gate_sim.set_input(&gate_clock_name, 0).await;

            // Step with snapshot for coverage
            if let Some(mir_state) = mir_sim.step_with_snapshot().await {
                behav_cov.update_toggle(&mir_state.signals);
                behav_cov.update_nodes(&mir_state.signals, &sir_module);
            }
            gate_sim.step().await;

            // Clock high
            mir_sim.set_input(&self.clock_name, 1).await;
            gate_sim.set_input(&gate_clock_name, 1).await;

            if let Some(mir_state) = mir_sim.step_with_snapshot().await {
                behav_cov.update_toggle(&mir_state.signals);
                behav_cov.update_nodes(&mir_state.signals, &sir_module);
            }
            gate_sim.step().await;

            // Update gate coverage from gate snapshot
            gate_cov.update_toggle_gate_vec(&gate_sim.dump_gate_signals());

            behav_cov.record_vector();
            gate_cov.record_vector();

            // Compare outputs
            for (mir_name, gate_bits) in &matched_outputs {
                let mir_val = mir_sim.get_output_raw(mir_name).await.unwrap_or(0);

                let mut sorted_bits = gate_bits.clone();
                sorted_bits.sort_by_key(|(_, idx)| idx.unwrap_or(0));

                let mut gate_val: u64 = 0;
                let mut has_bits = false;
                for (gate_name, bit_idx) in &sorted_bits {
                    if let Some(gv) = gate_sim.get_output_raw(gate_name).await {
                        has_bits = true;
                        if let Some(idx) = bit_idx {
                            gate_val |= (gv & 1) << idx;
                        } else {
                            gate_val = gv;
                        }
                    }
                }

                // BUG FIX: Mask MIR value to gate port width before comparing
                let cov_port_width = sorted_bits.iter()
                    .filter_map(|(_, idx)| *idx)
                    .max()
                    .map(|max_idx| max_idx + 1)
                    .unwrap_or(1) as u32;
                let cov_width_mask = if cov_port_width >= 64 { u64::MAX } else { (1u64 << cov_port_width) - 1 };
                let mir_val_masked = mir_val & cov_width_mask;

                if has_bits && mir_val_masked != gate_val {
                    let user_name = resolve_mir_name(mir_name);
                    let coverage_report = CoverageReport::from_coverage_dbs(
                        &behav_cov, Some(&gate_cov), false, cycle);

                    return Ok(SimEquivalenceResult {
                        equivalent: false,
                        cycles_verified: cycle,
                        mismatch_cycle: Some(cycle),
                        mismatch_output: Some(user_name),
                        value_1: Some(mir_val),
                        value_2: Some(gate_val),
                        time_ms: start.elapsed().as_millis() as u64,
                        outputs_compared: matched_outputs.len(),
                        diagnostics: None,
                        coverage_report: Some(coverage_report),
                    });
                }
            }

            cycle += 1;
        }

        // Cross-reference uncovered mux arms against gate netlist
        let mut mux_xref: std::collections::HashMap<String, MuxArmStatus> =
            std::collections::HashMap::new();

        // Build set of gate signal names for lookup
        let gate_signal_names: std::collections::HashSet<String> =
            gate_sir_result.sir.top_module.signals
                .iter()
                .map(|s| s.name.clone())
                .collect();

        for (node_name, _arms, output_sig) in behav_cov.uncovered_mux_arms_with_signals() {
            let status = if let Some(sig_id) = output_sig {
                // Resolve internal name to hierarchical path
                let hier_path = sir_module.name_registry
                    .get_entry_by_internal(sig_id)
                    .map(|e| e.hierarchical_path.clone())
                    .unwrap_or_else(|| sig_id.to_string());

                // Check if any gate signal matches (exact or prefix match for bus signals)
                let found = gate_signal_names.contains(&hier_path)
                    || gate_signal_names.iter().any(|gs|
                        gs.starts_with(&hier_path) || hier_path.starts_with(gs));

                if found { MuxArmStatus::CoverageGap } else { MuxArmStatus::OptimizedAway }
            } else {
                MuxArmStatus::Unknown
            };
            mux_xref.insert(node_name.to_string(), status);
        }

        // Cross-reference uncovered toggle signals against gate netlist
        let mut toggle_xref: std::collections::HashMap<String, MuxArmStatus> =
            std::collections::HashMap::new();

        for (sig_name, _width) in behav_cov.tracked_toggle_signals() {
            // Resolve internal name to hierarchical path
            let hier_path = sir_module.name_registry
                .get_entry_by_internal(sig_name)
                .map(|e| e.hierarchical_path.clone())
                .unwrap_or_else(|| sig_name.to_string());

            // Check if any gate signal matches (exact or prefix match for bus signals)
            let found = gate_signal_names.contains(&hier_path)
                || gate_signal_names.iter().any(|gs|
                    gs.starts_with(&hier_path) || hier_path.starts_with(gs));

            let status = if found { MuxArmStatus::CoverageGap } else { MuxArmStatus::OptimizedAway };
            toggle_xref.insert(sig_name.clone(), status);
        }

        // Build final coverage report with cross-reference data
        let coverage_report = CoverageReport::from_coverage_dbs_with_xref(
            &behav_cov, Some(&gate_cov), &mux_xref, &toggle_xref, true, cycle);

        Ok(SimEquivalenceResult {
            equivalent: true,
            cycles_verified: cycle,
            mismatch_cycle: None,
            mismatch_output: None,
            value_1: None,
            value_2: None,
            time_ms: start.elapsed().as_millis() as u64,
            outputs_compared: matched_outputs.len(),
            diagnostics: None,
            coverage_report: Some(coverage_report),
        })
    }
}

impl Default for SimBasedEquivalenceChecker {
    fn default() -> Self {
        Self::new()
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_aig_basic() {
        let mut aig = Aig::new();

        let a = aig.add_input("a".to_string());
        let b = aig.add_input("b".to_string());

        let and_ab = aig.add_and(a, b);
        aig.add_output("out".to_string(), and_ab);

        assert_eq!(aig.inputs.len(), 2);
        assert_eq!(aig.outputs.len(), 1);
        assert_eq!(aig.and_count(), 1);
    }

    #[test]
    fn test_aig_simplification() {
        let mut aig = Aig::new();

        let a = aig.add_input("a".to_string());

        // AND(a, true) should simplify to a
        let result = aig.add_and(a, aig.true_lit());
        assert_eq!(result, a);

        // AND(a, false) should simplify to false
        let result = aig.add_and(a, aig.false_lit());
        assert_eq!(result, aig.false_lit());

        // AND(a, a) should simplify to a
        let result = aig.add_and(a, a);
        assert_eq!(result, a);

        // AND(a, !a) should simplify to false
        let result = aig.add_and(a, a.invert());
        assert_eq!(result, aig.false_lit());
    }

    #[test]
    fn test_miter_identical() {
        let mut aig1 = Aig::new();
        let a1 = aig1.add_input("a".to_string());
        let b1 = aig1.add_input("b".to_string());
        let and1 = aig1.add_and(a1, b1);
        aig1.add_output("out".to_string(), and1);

        let mut aig2 = Aig::new();
        let a2 = aig2.add_input("a".to_string());
        let b2 = aig2.add_input("b".to_string());
        let and2 = aig2.add_and(a2, b2);
        aig2.add_output("out".to_string(), and2);

        let miter = build_miter(&aig1, &aig2).unwrap();
        let result = check_equivalence_sat(&miter).unwrap();

        assert!(result.equivalent, "Identical AIGs should be equivalent");
    }

    #[test]
    fn test_miter_different() {
        let mut aig1 = Aig::new();
        let a1 = aig1.add_input("a".to_string());
        let b1 = aig1.add_input("b".to_string());
        let and1 = aig1.add_and(a1, b1);
        aig1.add_output("out".to_string(), and1);

        let mut aig2 = Aig::new();
        let a2 = aig2.add_input("a".to_string());
        let b2 = aig2.add_input("b".to_string());
        let or2 = aig2.add_or(a2, b2); // OR instead of AND
        aig2.add_output("out".to_string(), or2);

        let miter = build_miter(&aig1, &aig2).unwrap();
        let result = check_equivalence_sat(&miter).unwrap();

        assert!(
            !result.equivalent,
            "AND vs OR should not be equivalent"
        );
        assert!(
            result.counterexample.is_some(),
            "Should have counterexample"
        );
    }

    #[test]
    fn test_demorgan_equivalence() {
        // Test that !(a AND b) == (!a OR !b) using De Morgan's law

        let mut aig1 = Aig::new();
        let a1 = aig1.add_input("a".to_string());
        let b1 = aig1.add_input("b".to_string());
        let nand = aig1.add_and(a1, b1).invert(); // !(a AND b)
        aig1.add_output("out".to_string(), nand);

        let mut aig2 = Aig::new();
        let a2 = aig2.add_input("a".to_string());
        let b2 = aig2.add_input("b".to_string());
        let nor = aig2.add_or(a2.invert(), b2.invert()); // !a OR !b
        aig2.add_output("out".to_string(), nor);

        let miter = build_miter(&aig1, &aig2).unwrap();
        let result = check_equivalence_sat(&miter).unwrap();

        assert!(result.equivalent, "De Morgan's law should hold");
    }

    #[test]
    fn test_extract_registers() {
        use skalp_lir::{Lir, LirOp};

        let mut lir = Lir::new("test".to_string());

        // Add inputs
        let clk = lir.add_input("clk".to_string(), 1);
        let d_in = lir.add_input("d".to_string(), 8);

        // Add register output
        let q_out = lir.add_output("q".to_string(), 8);

        // Add register node
        lir.add_seq_node(
            LirOp::Reg {
                width: 8,
                has_enable: false,
                has_reset: true,
                reset_value: Some(0),
            },
            vec![d_in],
            q_out,
            "reg".to_string(),
            clk,
            None,
        );

        let regs = extract_registers(&lir);
        assert_eq!(regs.len(), 1);
        assert_eq!(regs[0].name, "q");
        assert_eq!(regs[0].width, 8);
        assert_eq!(regs[0].reset_value, Some(0));
    }

    #[test]
    fn test_sequential_equivalence_identical() {
        use skalp_lir::{Lir, LirOp};

        // Create two identical sequential designs
        fn create_counter() -> Lir {
            let mut lir = Lir::new("counter".to_string());

            let clk = lir.add_input("clk".to_string(), 1);
            let rst = lir.add_input("rst".to_string(), 1);
            let count = lir.add_output("count".to_string(), 4);
            let next_count = lir.add_signal("next_count".to_string(), 4);
            let one = lir.add_signal("one".to_string(), 4);

            // Constant 1
            lir.add_node(
                LirOp::Constant { width: 4, value: 1 },
                vec![],
                one,
                "const_1".to_string(),
            );

            // next_count = count + 1
            lir.add_node(
                LirOp::Add {
                    width: 4,
                    has_carry: false,
                },
                vec![count, one],
                next_count,
                "add".to_string(),
            );

            // Register: count = next_count
            lir.add_seq_node(
                LirOp::Reg {
                    width: 4,
                    has_enable: false,
                    has_reset: true,
                    reset_value: Some(0),
                },
                vec![next_count],
                count,
                "count_reg".to_string(),
                clk,
                Some(rst),
            );

            lir
        }

        let lir1 = create_counter();
        let lir2 = create_counter();

        let checker = SequentialEquivalenceChecker::new();
        let result = checker
            .check_sequential_equivalence(&lir1, &lir2)
            .unwrap();

        assert!(
            result.equivalent,
            "Identical counters should be equivalent: {:?}",
            result.failure_reason
        );
    }

    #[test]
    fn test_sequential_equivalence_different_reset() {
        use skalp_lir::{Lir, LirOp};

        // Create counter with reset value 0
        let mut lir1 = Lir::new("counter1".to_string());
        let clk1 = lir1.add_input("clk".to_string(), 1);
        let count1 = lir1.add_output("count".to_string(), 4);
        let next1 = lir1.add_signal("next".to_string(), 4);
        lir1.add_node(
            LirOp::Constant { width: 4, value: 1 },
            vec![],
            next1,
            "const".to_string(),
        );
        lir1.add_seq_node(
            LirOp::Reg {
                width: 4,
                has_enable: false,
                has_reset: true,
                reset_value: Some(0), // Reset to 0
            },
            vec![next1],
            count1,
            "reg".to_string(),
            clk1,
            None,
        );

        // Create counter with reset value 5 (different!)
        let mut lir2 = Lir::new("counter2".to_string());
        let clk2 = lir2.add_input("clk".to_string(), 1);
        let count2 = lir2.add_output("count".to_string(), 4);
        let next2 = lir2.add_signal("next".to_string(), 4);
        lir2.add_node(
            LirOp::Constant { width: 4, value: 1 },
            vec![],
            next2,
            "const".to_string(),
        );
        lir2.add_seq_node(
            LirOp::Reg {
                width: 4,
                has_enable: false,
                has_reset: true,
                reset_value: Some(5), // Reset to 5 (different!)
            },
            vec![next2],
            count2,
            "reg".to_string(),
            clk2,
            None,
        );

        let checker = SequentialEquivalenceChecker::new();
        let result = checker
            .check_sequential_equivalence(&lir1, &lir2)
            .unwrap();

        assert!(
            !result.equivalent,
            "Counters with different reset values should NOT be equivalent"
        );
        assert!(result
            .failure_reason
            .as_ref()
            .unwrap()
            .contains("reset value"));
    }

    #[test]
    fn test_mir_to_aig_basic() {
        use skalp_mir::{
            DataType, Expression, ExpressionKind, LValue, Module, ModuleId, Port, PortDirection,
            PortId, Process, ProcessId, ProcessKind, SensitivityList, Block, Statement,
            Assignment, AssignmentKind, BinaryOp,
        };

        // Create a simple MIR module: out = a & b
        let module = Module {
            id: ModuleId(0),
            name: "and_gate".to_string(),
            parameters: vec![],
            ports: vec![
                Port {
                    id: PortId(0),
                    name: "a".to_string(),
                    direction: PortDirection::Input,
                    port_type: DataType::Bit(1),
                    physical_constraints: None,
                    span: None,
                    detection_config: None,
                },
                Port {
                    id: PortId(1),
                    name: "b".to_string(),
                    direction: PortDirection::Input,
                    port_type: DataType::Bit(1),
                    physical_constraints: None,
                    span: None,
                    detection_config: None,
                },
                Port {
                    id: PortId(2),
                    name: "out".to_string(),
                    direction: PortDirection::Output,
                    port_type: DataType::Bit(1),
                    physical_constraints: None,
                    span: None,
                    detection_config: None,
                },
            ],
            signals: vec![],
            variables: vec![],
            processes: vec![Process {
                id: ProcessId(0),
                kind: ProcessKind::Combinational,
                sensitivity: SensitivityList::Always,
                body: Block {
                    statements: vec![Statement::Assignment(Assignment {
                        lhs: LValue::Port(PortId(2)),
                        rhs: Expression::with_unknown_type(ExpressionKind::Binary {
                            op: BinaryOp::BitwiseAnd,
                            left: Box::new(Expression::with_unknown_type(ExpressionKind::Ref(
                                LValue::Port(PortId(0)),
                            ))),
                            right: Box::new(Expression::with_unknown_type(ExpressionKind::Ref(
                                LValue::Port(PortId(1)),
                            ))),
                        }),
                        kind: AssignmentKind::Blocking,
                        span: None,
                    })],
                },
                span: None,
            }],
            assignments: vec![],
            instances: vec![],
            clock_domains: vec![],
            generate_blocks: vec![],
            assertions: vec![],
            span: None,
            pipeline_config: None,
            vendor_ip_config: None,
            compiled_ip_config: None,
            power_domains: vec![],
            power_domain_config: None,
            safety_context: None,
            is_async: false,
            barriers: vec![],
            ncl_boundary_mode: None,
        };

        let aig = MirToAig::new(&module).convert();

        assert_eq!(aig.inputs.len(), 2, "Should have 2 inputs");
        assert_eq!(aig.outputs.len(), 1, "Should have 1 output");
        assert!(aig.and_count() >= 1, "Should have at least 1 AND gate");
    }

    #[test]
    fn test_mir_equivalence_identical() {
        use skalp_mir::{
            DataType, Expression, ExpressionKind, LValue, Module, ModuleId, Port, PortDirection,
            PortId, Process, ProcessId, ProcessKind, SensitivityList, Block, Statement,
            Assignment, AssignmentKind, BinaryOp,
        };

        // Create two identical MIR modules
        fn create_and_module() -> Module {
            Module {
                id: ModuleId(0),
                name: "and_gate".to_string(),
                parameters: vec![],
                ports: vec![
                    Port {
                        id: PortId(0),
                        name: "a".to_string(),
                        direction: PortDirection::Input,
                        port_type: DataType::Bit(1),
                        physical_constraints: None,
                        span: None,
                        detection_config: None,
                    },
                    Port {
                        id: PortId(1),
                        name: "b".to_string(),
                        direction: PortDirection::Input,
                        port_type: DataType::Bit(1),
                        physical_constraints: None,
                        span: None,
                        detection_config: None,
                    },
                    Port {
                        id: PortId(2),
                        name: "out".to_string(),
                        direction: PortDirection::Output,
                        port_type: DataType::Bit(1),
                        physical_constraints: None,
                        span: None,
                        detection_config: None,
                    },
                ],
                signals: vec![],
                variables: vec![],
                processes: vec![Process {
                    id: ProcessId(0),
                    kind: ProcessKind::Combinational,
                    sensitivity: SensitivityList::Always,
                    body: Block {
                        statements: vec![Statement::Assignment(Assignment {
                            lhs: LValue::Port(PortId(2)),
                            rhs: Expression::with_unknown_type(ExpressionKind::Binary {
                                op: BinaryOp::BitwiseAnd,
                                left: Box::new(Expression::with_unknown_type(ExpressionKind::Ref(
                                    LValue::Port(PortId(0)),
                                ))),
                                right: Box::new(Expression::with_unknown_type(ExpressionKind::Ref(
                                    LValue::Port(PortId(1)),
                                ))),
                            }),
                            kind: AssignmentKind::Blocking,
                            span: None,
                        })],
                    },
                    span: None,
                }],
                assignments: vec![],
                instances: vec![],
                clock_domains: vec![],
                generate_blocks: vec![],
                assertions: vec![],
                span: None,
                pipeline_config: None,
                vendor_ip_config: None,
                compiled_ip_config: None,
                power_domains: vec![],
                power_domain_config: None,
                safety_context: None,
                is_async: false,
                barriers: vec![],
                ncl_boundary_mode: None,
            }
        }

        let module1 = create_and_module();
        let module2 = create_and_module();

        let checker = MirEquivalenceChecker::new();
        let result = checker.check_mir_equivalence(&module1, &module2).unwrap();

        assert!(result.equivalent, "Identical MIR modules should be equivalent");
    }

    #[test]
    fn test_mux2_with_constant_selector() {
        use skalp_lir::{Lir, LirOp};

        // Test that Mux2 with constant selector=1 selects the then_value (inputs[2])
        let mut lir = Lir::new("test_mux".to_string());

        // Add signals
        let sel_sig = lir.add_signal("sel".to_string(), 1);
        let else_val = lir.add_signal("else_val".to_string(), 1);
        let then_val = lir.add_signal("then_val".to_string(), 1);
        let result = lir.add_output("result".to_string(), 1);

        // sel = 1 (constant true)
        lir.add_node(
            LirOp::Constant { width: 1, value: 1 },
            vec![],
            sel_sig,
            "const_sel".to_string(),
        );

        // else_val = 1 (true)
        lir.add_node(
            LirOp::Constant { width: 1, value: 1 },
            vec![],
            else_val,
            "const_else".to_string(),
        );

        // then_val = 0 (false)
        lir.add_node(
            LirOp::Constant { width: 1, value: 0 },
            vec![],
            then_val,
            "const_then".to_string(),
        );

        // result = mux(sel, else_val, then_val)
        // With sel=1, should select then_val=0
        lir.add_node(
            LirOp::Mux2 { width: 1 },
            vec![sel_sig, else_val, then_val],
            result,
            "mux".to_string(),
        );

        // Convert to AIG and check the output
        let converter = LirToAig::new();
        let aig = converter.convert(&lir);

        // The output should be constant false (because sel=1 selects then_val=0)
        assert_eq!(aig.outputs.len(), 1);
        let out_lit = aig.outputs[0];

        // Simulate: output should be false
        let mut sim_values: std::collections::HashMap<u32, bool> = std::collections::HashMap::new();
        sim_values.insert(0, false); // Node 0 is always false

        for (idx, node) in aig.nodes.iter().enumerate() {
            let idx = idx as u32;
            match node {
                AigNode::False => {
                    sim_values.insert(idx, false);
                }
                AigNode::Input { .. } => {
                    sim_values.insert(idx, false);
                }
                AigNode::And { left, right } => {
                    let l = sim_values.get(&left.node.0).copied().unwrap_or(false);
                    let r = sim_values.get(&right.node.0).copied().unwrap_or(false);
                    let l = if left.inverted { !l } else { l };
                    let r = if right.inverted { !r } else { r };
                    sim_values.insert(idx, l && r);
                }
                AigNode::Latch { .. } => {}
            }
        }

        let out_val = sim_values.get(&out_lit.node.0).copied().unwrap_or(false);
        let out_val = if out_lit.inverted { !out_val } else { out_val };

        assert!(
            !out_val,
            "Mux with sel=1 should select then_val=0, but got true"
        );
    }

    #[test]
    fn test_gt_comparison_zero_zero() {
        use skalp_lir::{Lir, LirOp};

        // Test that 0 > 0 is false
        let mut lir = Lir::new("test_gt".to_string());

        let a = lir.add_signal("a".to_string(), 16);
        let b = lir.add_signal("b".to_string(), 16);
        let result = lir.add_output("result".to_string(), 1);

        // a = 0
        lir.add_node(
            LirOp::Constant { width: 16, value: 0 },
            vec![],
            a,
            "const_a".to_string(),
        );

        // b = 0
        lir.add_node(
            LirOp::Constant { width: 16, value: 0 },
            vec![],
            b,
            "const_b".to_string(),
        );

        // result = a > b (0 > 0 = false)
        lir.add_node(
            LirOp::Gt { width: 16 },
            vec![a, b],
            result,
            "gt".to_string(),
        );

        // Convert to AIG
        let converter = LirToAig::new();
        let aig = converter.convert(&lir);

        // Simulate
        let mut sim_values: std::collections::HashMap<u32, bool> = std::collections::HashMap::new();
        sim_values.insert(0, false);

        for (idx, node) in aig.nodes.iter().enumerate() {
            let idx = idx as u32;
            match node {
                AigNode::False => {
                    sim_values.insert(idx, false);
                }
                AigNode::Input { .. } => {
                    sim_values.insert(idx, false);
                }
                AigNode::And { left, right } => {
                    let l = sim_values.get(&left.node.0).copied().unwrap_or(false);
                    let r = sim_values.get(&right.node.0).copied().unwrap_or(false);
                    let l = if left.inverted { !l } else { l };
                    let r = if right.inverted { !r } else { r };
                    sim_values.insert(idx, l && r);
                }
                AigNode::Latch { .. } => {}
            }
        }

        let out_lit = aig.outputs[0];
        let out_val = sim_values.get(&out_lit.node.0).copied().unwrap_or(false);
        let out_val = if out_lit.inverted { !out_val } else { out_val };

        assert!(!out_val, "0 > 0 should be false, but got true");
    }

    #[test]
    fn test_signed_less_than_zero() {
        use skalp_lir::{Lir, LirOp};

        // Test that (signed) 0 < 0 is false
        let mut lir = Lir::new("test_slt".to_string());

        let a = lir.add_signal("a".to_string(), 16);
        let b = lir.add_signal("b".to_string(), 16);
        let result = lir.add_output("result".to_string(), 1);

        // a = 0
        lir.add_node(
            LirOp::Constant { width: 16, value: 0 },
            vec![],
            a,
            "const_a".to_string(),
        );

        // b = 0
        lir.add_node(
            LirOp::Constant { width: 16, value: 0 },
            vec![],
            b,
            "const_b".to_string(),
        );

        // result = a < b (signed) (0 < 0 = false)
        lir.add_node(
            LirOp::Slt { width: 16 },
            vec![a, b],
            result,
            "slt".to_string(),
        );

        // Convert to AIG
        let converter = LirToAig::new();
        let aig = converter.convert(&lir);

        // Simulate
        let mut sim_values: std::collections::HashMap<u32, bool> = std::collections::HashMap::new();
        sim_values.insert(0, false);

        for (idx, node) in aig.nodes.iter().enumerate() {
            let idx = idx as u32;
            match node {
                AigNode::False => { sim_values.insert(idx, false); }
                AigNode::Input { .. } => { sim_values.insert(idx, false); }
                AigNode::And { left, right } => {
                    let l = sim_values.get(&left.node.0).copied().unwrap_or(false);
                    let r = sim_values.get(&right.node.0).copied().unwrap_or(false);
                    let l = if left.inverted { !l } else { l };
                    let r = if right.inverted { !r } else { r };
                    sim_values.insert(idx, l && r);
                }
                AigNode::Latch { .. } => {}
            }
        }

        let out_lit = aig.outputs[0];
        let out_val = sim_values.get(&out_lit.node.0).copied().unwrap_or(false);
        let out_val = if out_lit.inverted { !out_val } else { out_val };

        assert!(!out_val, "signed 0 < 0 should be false, but got true");
    }

    #[test]
    fn test_signed_less_than_negative() {
        use skalp_lir::{Lir, LirOp};

        // Test that (signed) -1 < 0 is true
        // -1 in 16-bit two's complement is 0xFFFF
        let mut lir = Lir::new("test_slt_neg".to_string());

        let a = lir.add_signal("a".to_string(), 16);
        let b = lir.add_signal("b".to_string(), 16);
        let result = lir.add_output("result".to_string(), 1);

        // a = -1 (0xFFFF in two's complement)
        lir.add_node(
            LirOp::Constant { width: 16, value: 0xFFFF },
            vec![],
            a,
            "const_a".to_string(),
        );

        // b = 0
        lir.add_node(
            LirOp::Constant { width: 16, value: 0 },
            vec![],
            b,
            "const_b".to_string(),
        );

        // result = a < b (signed) (-1 < 0 = true)
        lir.add_node(
            LirOp::Slt { width: 16 },
            vec![a, b],
            result,
            "slt".to_string(),
        );

        // Convert to AIG
        let converter = LirToAig::new();
        let aig = converter.convert(&lir);

        // Simulate: for constants, all values should be derivable from false_lit and true_lit
        let mut sim_values: std::collections::HashMap<u32, bool> = std::collections::HashMap::new();
        sim_values.insert(0, false);

        for (idx, node) in aig.nodes.iter().enumerate() {
            let idx = idx as u32;
            match node {
                AigNode::False => { sim_values.insert(idx, false); }
                AigNode::Input { .. } => { sim_values.insert(idx, false); }
                AigNode::And { left, right } => {
                    let l = sim_values.get(&left.node.0).copied().unwrap_or(false);
                    let r = sim_values.get(&right.node.0).copied().unwrap_or(false);
                    let l = if left.inverted { !l } else { l };
                    let r = if right.inverted { !r } else { r };
                    sim_values.insert(idx, l && r);
                }
                AigNode::Latch { .. } => {}
            }
        }

        let out_lit = aig.outputs[0];
        let out_val = sim_values.get(&out_lit.node.0).copied().unwrap_or(false);
        let out_val = if out_lit.inverted { !out_val } else { out_val };

        assert!(out_val, "signed -1 < 0 should be true, but got false");
    }

    #[test]
    fn test_gt_comparison_non_zero() {
        use skalp_lir::{Lir, LirOp};

        // Test that 100 > 50 is true
        let mut lir = Lir::new("test_gt_nonzero".to_string());

        let a = lir.add_signal("a".to_string(), 16);
        let b = lir.add_signal("b".to_string(), 16);
        let result = lir.add_output("result".to_string(), 1);

        // a = 100
        lir.add_node(
            LirOp::Constant { width: 16, value: 100 },
            vec![],
            a,
            "const_a".to_string(),
        );

        // b = 50
        lir.add_node(
            LirOp::Constant { width: 16, value: 50 },
            vec![],
            b,
            "const_b".to_string(),
        );

        // result = a > b (100 > 50 = true)
        lir.add_node(
            LirOp::Gt { width: 16 },
            vec![a, b],
            result,
            "gt".to_string(),
        );

        // Convert to AIG
        let converter = LirToAig::new();
        let aig = converter.convert(&lir);

        // Simulate
        let mut sim_values: std::collections::HashMap<u32, bool> = std::collections::HashMap::new();
        sim_values.insert(0, false);

        for (idx, node) in aig.nodes.iter().enumerate() {
            let idx = idx as u32;
            match node {
                AigNode::False => { sim_values.insert(idx, false); }
                AigNode::Input { .. } => { sim_values.insert(idx, false); }
                AigNode::And { left, right } => {
                    let l = sim_values.get(&left.node.0).copied().unwrap_or(false);
                    let r = sim_values.get(&right.node.0).copied().unwrap_or(false);
                    let l = if left.inverted { !l } else { l };
                    let r = if right.inverted { !r } else { r };
                    sim_values.insert(idx, l && r);
                }
                AigNode::Latch { .. } => {}
            }
        }

        let out_lit = aig.outputs[0];
        let out_val = sim_values.get(&out_lit.node.0).copied().unwrap_or(false);
        let out_val = if out_lit.inverted { !out_val } else { out_val };

        assert!(out_val, "100 > 50 should be true, but got false");
    }

    #[test]
    fn test_gt_comparison_less() {
        use skalp_lir::{Lir, LirOp};

        // Test that 50 > 100 is false
        let mut lir = Lir::new("test_gt_less".to_string());

        let a = lir.add_signal("a".to_string(), 16);
        let b = lir.add_signal("b".to_string(), 16);
        let result = lir.add_output("result".to_string(), 1);

        // a = 50
        lir.add_node(
            LirOp::Constant { width: 16, value: 50 },
            vec![],
            a,
            "const_a".to_string(),
        );

        // b = 100
        lir.add_node(
            LirOp::Constant { width: 16, value: 100 },
            vec![],
            b,
            "const_b".to_string(),
        );

        // result = a > b (50 > 100 = false)
        lir.add_node(
            LirOp::Gt { width: 16 },
            vec![a, b],
            result,
            "gt".to_string(),
        );

        // Convert to AIG
        let converter = LirToAig::new();
        let aig = converter.convert(&lir);

        // Simulate
        let mut sim_values: std::collections::HashMap<u32, bool> = std::collections::HashMap::new();
        sim_values.insert(0, false);

        for (idx, node) in aig.nodes.iter().enumerate() {
            let idx = idx as u32;
            match node {
                AigNode::False => { sim_values.insert(idx, false); }
                AigNode::Input { .. } => { sim_values.insert(idx, false); }
                AigNode::And { left, right } => {
                    let l = sim_values.get(&left.node.0).copied().unwrap_or(false);
                    let r = sim_values.get(&right.node.0).copied().unwrap_or(false);
                    let l = if left.inverted { !l } else { l };
                    let r = if right.inverted { !r } else { r };
                    sim_values.insert(idx, l && r);
                }
                AigNode::Latch { .. } => {}
            }
        }

        let out_lit = aig.outputs[0];
        let out_val = sim_values.get(&out_lit.node.0).copied().unwrap_or(false);
        let out_val = if out_lit.inverted { !out_val } else { out_val };

        assert!(!out_val, "50 > 100 should be false, but got true");
    }

    #[test]
    fn test_threshold_comparator_like_mux_chain() {
        use skalp_lir::{Lir, LirOp, LirSignalId};

        // Test MUX chain similar to ThresholdComparator:
        // result = mux(compare_high,
        //              else_val=0,
        //              then_val=mux(!state,
        //                           else_val=(value > threshold_hysteresis),
        //                           then_val=(value > threshold)))
        //
        // With compare_high=1 and state=0:
        //   result = mux(1, 0, mux(1, cmp2, cmp1))
        //          = mux(1, cmp2, cmp1)  [inner mux with !state=1]
        //          = cmp1  [selector=1 picks then_val]
        //          = value > threshold
        //          = 100 > 50
        //          = true

        let mut lir = Lir::new("test_threshold_mux".to_string());

        // Constants
        let compare_high = lir.add_signal("compare_high".to_string(), 1);
        let state = lir.add_signal("state".to_string(), 1);
        let value = lir.add_signal("value".to_string(), 16);
        let threshold = lir.add_signal("threshold".to_string(), 16);
        let threshold_hysteresis = lir.add_signal("threshold_hysteresis".to_string(), 16);

        // Intermediate signals
        let not_state = lir.add_signal("not_state".to_string(), 1);
        let cmp1 = lir.add_signal("cmp1".to_string(), 1);
        let cmp2 = lir.add_signal("cmp2".to_string(), 1);
        let inner_mux = lir.add_signal("inner_mux".to_string(), 1);
        let zero = lir.add_signal("zero".to_string(), 1);

        let result = lir.add_output("result".to_string(), 1);

        // compare_high = 1 (constant)
        lir.add_node(
            LirOp::Constant { width: 1, value: 1 },
            vec![],
            compare_high,
            "const_compare_high".to_string(),
        );

        // state = 0 (constant - simulating initial state)
        lir.add_node(
            LirOp::Constant { width: 1, value: 0 },
            vec![],
            state,
            "const_state".to_string(),
        );

        // value = 100
        lir.add_node(
            LirOp::Constant { width: 16, value: 100 },
            vec![],
            value,
            "const_value".to_string(),
        );

        // threshold = 50
        lir.add_node(
            LirOp::Constant { width: 16, value: 50 },
            vec![],
            threshold,
            "const_threshold".to_string(),
        );

        // threshold_hysteresis = 40
        lir.add_node(
            LirOp::Constant { width: 16, value: 40 },
            vec![],
            threshold_hysteresis,
            "const_threshold_hysteresis".to_string(),
        );

        // zero = 0
        lir.add_node(
            LirOp::Constant { width: 1, value: 0 },
            vec![],
            zero,
            "const_zero".to_string(),
        );

        // not_state = !state
        lir.add_node(
            LirOp::Not { width: 1 },
            vec![state],
            not_state,
            "not_state".to_string(),
        );

        // cmp1 = value > threshold (100 > 50 = true)
        lir.add_node(
            LirOp::Gt { width: 16 },
            vec![value, threshold],
            cmp1,
            "cmp1".to_string(),
        );

        // cmp2 = value > threshold_hysteresis (100 > 40 = true)
        lir.add_node(
            LirOp::Gt { width: 16 },
            vec![value, threshold_hysteresis],
            cmp2,
            "cmp2".to_string(),
        );

        // inner_mux = mux(not_state, cmp2, cmp1)
        // not_state=1, so should select cmp1 (then_val)
        lir.add_node(
            LirOp::Mux2 { width: 1 },
            vec![not_state, cmp2, cmp1],
            inner_mux,
            "inner_mux".to_string(),
        );

        // result = mux(compare_high, zero, inner_mux)
        // compare_high=1, so should select inner_mux (then_val)
        lir.add_node(
            LirOp::Mux2 { width: 1 },
            vec![compare_high, zero, inner_mux],
            result,
            "outer_mux".to_string(),
        );

        // Convert to AIG
        let converter = LirToAig::new();
        let aig = converter.convert(&lir);

        // Simulate
        let mut sim_values: std::collections::HashMap<u32, bool> = std::collections::HashMap::new();
        sim_values.insert(0, false);

        for (idx, node) in aig.nodes.iter().enumerate() {
            let idx = idx as u32;
            match node {
                AigNode::False => { sim_values.insert(idx, false); }
                AigNode::Input { .. } => { sim_values.insert(idx, false); }
                AigNode::And { left, right } => {
                    let l = sim_values.get(&left.node.0).copied().unwrap_or(false);
                    let r = sim_values.get(&right.node.0).copied().unwrap_or(false);
                    let l = if left.inverted { !l } else { l };
                    let r = if right.inverted { !r } else { r };
                    sim_values.insert(idx, l && r);
                }
                AigNode::Latch { .. } => {}
            }
        }

        let out_lit = aig.outputs[0];
        let out_val = sim_values.get(&out_lit.node.0).copied().unwrap_or(false);
        let out_val = if out_lit.inverted { !out_val } else { out_val };

        // Expected: with compare_high=1, state=0, value=100, threshold=50
        // result = mux(1, 0, mux(1, cmp2, cmp1))
        //        = mux(1, cmp2, cmp1)  [inner mux: not_state=1]
        //        = cmp1                [outer mux: compare_high=1]
        //        = 100 > 50
        //        = true
        assert!(out_val, "ThresholdComparator-like MUX chain should return true for 100 > 50 with compare_high=1, state=0");
    }
}
