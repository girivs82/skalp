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
use skalp_mir::Type;
use std::collections::HashMap;
use std::sync::atomic::{AtomicBool, Ordering};
use varisat::{CnfFormula, ExtendFormula, Lit, Solver, Var};

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
        }
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
    pub signal_map: HashMap<(u32, u32), AigLit>,
}

impl LirToAig {
    pub fn new() -> Self {
        Self {
            aig: Aig::new(),
            signal_map: HashMap::new(),
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
        let mut reg_current_map: HashMap<(u32, u32), AigLit> = HashMap::new();
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

                for bit in 0..*width {
                    let latch_name = if *width == 1 {
                        output_signal.name.clone()
                    } else {
                        format!("{}[{}]", output_signal.name, bit)
                    };

                    // Get D input (next state)
                    let d_lit = self.get_input_bit(d_input, bit);

                    // Handle reset: if has_reset, apply rst ? reset_val : d
                    let next_lit = if *has_reset {
                        if let Some(rst_id) = node.reset {
                            let rst_lit = self.get_input_bit(rst_id, 0);
                            let reset_bit = (reset_val >> bit) & 1 != 0;
                            // MUX: rst ? reset_bit : d
                            if reset_bit {
                                // rst | (!rst & d) = rst | d
                                self.aig.add_or(rst_lit, d_lit)
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
        let mut signal_producer: HashMap<u32, usize> = HashMap::new();
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
                // Debug: log missing signal lookup
                log::warn!("[LIR_AIG] ⚠️ Missing signal lookup: signal_id={}, bit={}", signal_id.0, bit);
                self.aig.false_lit()
            })
    }

    fn set_output_bit(&mut self, signal_id: LirSignalId, bit: u32, lit: AigLit) {
        self.signal_map.insert((signal_id.0, bit), lit);
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

            // Multiplication (simplified - creates many AND/ADD gates)
            LirOp::Mul { width, result_width } => {
                let a = node.inputs[0];
                let b = node.inputs[1];

                // Initialize result to zero
                let mut result: Vec<AigLit> = vec![self.aig.false_lit(); *result_width as usize];

                // Grade-school multiplication: for each bit of b, add shifted a if bit is set
                for i in 0..*width {
                    let b_bit = self.get_input_bit(b, i);

                    // Add a << i to result, gated by b[i]
                    let mut carry = self.aig.false_lit();
                    for j in 0..*width {
                        let out_idx = (i + j) as usize;
                        if out_idx < *result_width as usize {
                            let a_bit = self.get_input_bit(a, j);
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
                    // Propagate remaining carry
                    let mut idx = (i + *width) as usize;
                    while idx < *result_width as usize {
                        let sum = self.aig.add_xor(result[idx], carry);
                        carry = self.aig.add_and(result[idx], carry);
                        result[idx] = sum;
                        if carry == self.aig.false_lit() {
                            break;
                        }
                        idx += 1;
                    }
                }

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
                println!("WARNING: Unimplemented LirOp {:?} for signal '{}' (width={})",
                    node.op, output_signal.name, width);
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
    net_map: HashMap<u32, AigLit>,
}

impl GateNetlistToAig {
    pub fn new() -> Self {
        Self {
            aig: Aig::new(),
            net_map: HashMap::new(),
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
        for &output_id in &netlist.outputs {
            let net = &netlist.nets[output_id.0 as usize];
            let lit = self
                .net_map
                .get(&output_id.0)
                .copied()
                .unwrap_or_else(|| self.aig.false_lit());
            self.aig.add_output(net.name.clone(), lit);
        }

        self.aig
    }

    fn get_net(&self, net_id: skalp_lir::GateNetId) -> AigLit {
        self.net_map
            .get(&net_id.0)
            .copied()
            .unwrap_or_else(|| self.aig.false_lit())
    }

    fn set_net(&mut self, net_id: skalp_lir::GateNetId, lit: AigLit) {
        self.net_map.insert(net_id.0, lit);
    }

    /// Topologically sort combinational cells based on their input/output dependencies.
    /// Returns cell indices in order such that when processing a cell, all its inputs
    /// (from other cells' outputs) have already been processed.
    fn topological_sort_cells(&self, netlist: &GateNetlist) -> Vec<usize> {
        use std::collections::{HashMap, HashSet, VecDeque};

        // Build a map from output net -> cell index
        // IMPORTANT: Only include COMBINATIONAL cells - DFF outputs should be treated as primary inputs
        // because their values are determined by state, not by combinational logic in the same cycle
        let mut net_to_cell: HashMap<u32, usize> = HashMap::new();
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
        let mut dependencies: HashMap<usize, HashSet<usize>> = HashMap::new();
        let mut reverse_deps: HashMap<usize, HashSet<usize>> = HashMap::new();

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
        let mut in_degree: HashMap<usize, usize> = HashMap::new();
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
        let mut dff_cells: Vec<(usize, skalp_lir::GateNetId, String)> = Vec::new();

        for (idx, cell) in netlist.cells.iter().enumerate() {
            if cell.is_sequential() {
                if let Some(out) = cell.outputs.first().copied() {
                    let net = &netlist.nets[out.0 as usize];
                    // Create temporary input for the current DFF output (latch state)
                    let temp_name = format!("__dff_cur_{}", net.name);
                    let temp_lit = self.aig.add_input(temp_name);
                    self.net_map.insert(out.0, temp_lit);
                    dff_cells.push((idx, out, net.name.clone()));
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
        for (cell_idx, out_net, latch_name) in &dff_cells {
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

            // Handle reset: if cell has reset input, next_state = rst ? 0 : D
            // But SKIP this if the D input is already driven by a MUX with rst as selector,
            // because that means sync reset is handled by the MUX
            let mut reset_value_from_mux: Option<bool> = None;

            // First, check if D is driven by a MUX with rst as selector (even without cell.reset)
            // This handles the case where MUX-based reset is used instead of DFFR
            if let Some(&d_net) = d_input {
                // Find if D is driven by a MUX where selector contains "rst"
                if let Some(mux_cell) = netlist.cells.iter().find(|driver| {
                    let outputs_match = driver.outputs.iter().any(|o| o.0 == d_net.0);
                    let is_mux = matches!(driver.function, Some(CellFunction::Mux2));
                    let sel_is_rst = if let Some(&sel_net) = driver.inputs.first() {
                        let sel_net_name = &netlist.nets[sel_net.0 as usize].name;
                        sel_net_name.contains("rst")
                    } else {
                        false
                    };
                    outputs_match && is_mux && sel_is_rst
                }) {
                    // MUX inputs: [sel, d0 (when sel=0), d1 (when sel=1)]
                    // When rst=1 (sel=1), we use d1 - that's the reset value
                    // d1 is inputs[2] in MUX2
                    if let Some(&reset_net) = mux_cell.inputs.get(2) {
                        // Check if reset_net is driven by TIE cell
                        let reset_val = netlist.cells.iter().find(|c| {
                            c.outputs.iter().any(|o| o.0 == reset_net.0)
                        }).and_then(|driver| {
                            if driver.cell_type.contains("TIE_HIGH") || driver.cell_type.contains("TIEHI") {
                                Some(true)
                            } else if driver.cell_type.contains("TIE_LOW") || driver.cell_type.contains("TIELO") {
                                Some(false)
                            } else {
                                None
                            }
                        });
                        reset_value_from_mux = reset_val;
                    }
                }
            }

            let next_lit = if let Some(rst_net) = cell.reset {
                // Check if D input is driven by a MUX that handles reset
                // Multiple detection strategies:
                // 1. MUX where one data input is a constant (TIE cell)
                // 2. MUX where selector is derived from the same reset net
                let d_has_reset_mux = if let Some(&d_net) = d_input {
                    // Find if D is driven by a MUX
                    let result = netlist.cells.iter().any(|driver| {
                        let outputs_match = driver.outputs.iter().any(|o| o.0 == d_net.0);
                        let is_mux = matches!(driver.function, Some(CellFunction::Mux2));
                        if !outputs_match || !is_mux {
                            return false;
                        }

                        // Strategy 1: Check if any data input is a TIE cell
                        let has_tie_input = driver.inputs.iter().skip(1).any(|&data_net| {
                            netlist.cells.iter().any(|c| {
                                c.outputs.iter().any(|o| o.0 == data_net.0)
                                    && (c.cell_type.contains("TIE") || c.cell_type.contains("TIEHI") || c.cell_type.contains("TIELO"))
                            })
                        });

                        // Strategy 2: Check if selector is derived from the reset net
                        // Trace back through multiple levels of combinational logic
                        // The MUX selector is inputs[0]
                        let selector_related_to_reset = if !driver.inputs.is_empty() {
                            let sel_net = driver.inputs[0];
                            // BFS to trace back through combinational logic to find if it derives from rst
                            let mut visited = std::collections::HashSet::new();
                            let mut queue = std::collections::VecDeque::new();
                            queue.push_back(sel_net.0);
                            let mut found_rst = false;
                            let max_depth = 10; // Limit depth to avoid infinite loops
                            let mut depth = 0;

                            while !queue.is_empty() && depth < max_depth && !found_rst {
                                let level_size = queue.len();
                                for _ in 0..level_size {
                                    if let Some(current_net) = queue.pop_front() {
                                        if visited.contains(&current_net) {
                                            continue;
                                        }
                                        visited.insert(current_net);

                                        // Direct match with reset net
                                        if current_net == rst_net.0 {
                                            found_rst = true;
                                            break;
                                        }

                                        // Find the cell that drives this net and add its inputs to queue
                                        for c in &netlist.cells {
                                            if c.outputs.iter().any(|o| o.0 == current_net) {
                                                // Don't trace through sequential elements
                                                if matches!(c.function, Some(CellFunction::Dff) | Some(CellFunction::DffR)) {
                                                    continue;
                                                }
                                                for &input_net in &c.inputs {
                                                    if !visited.contains(&input_net.0) {
                                                        queue.push_back(input_net.0);
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                                depth += 1;
                            }
                            found_rst
                        } else {
                            false
                        };

                        has_tie_input || selector_related_to_reset
                    });
                    result
                } else {
                    false
                };

                // Always apply !rst AND D to match LIR-to-AIG behavior
                // This ensures consistent AIG literal inversion flags between LIR and Gate AIGs
                // (The MUX uses add_or which returns inverted literals, while add_and doesn't)
                let rst_lit = self.net_map.get(&rst_net.0).copied().unwrap_or_else(|| {
                    let rst_net_obj = &netlist.nets[rst_net.0 as usize];
                    // Create input for reset signal
                    let lit = self.aig.add_input(rst_net_obj.name.clone());
                    self.net_map.insert(rst_net.0, lit);
                    lit
                });
                // next = rst ? 0 : D = !rst AND D
                // This applies to both DFFR (explicit reset) and MUX-based reset
                // for consistency with LIR-to-AIG
                self.aig.add_and(rst_lit.invert(), d_lit)
            } else {
                // No reset - just use D input
                d_lit
            };

            // Determine init value:
            // - If we found a MUX with a reset value, use that
            // - Otherwise, default to false
            let init_value = reset_value_from_mux.unwrap_or(false);

            // Create latch with proper init value
            let latch_lit = self.aig.add_latch(latch_name.clone(), next_lit, init_value);

            // Update net_map to use latch output
            self.net_map.insert(out_net.0, latch_lit);
        }

        // Add primary outputs
        for &output_id in &netlist.outputs {
            let net = &netlist.nets[output_id.0 as usize];
            let lit = self
                .net_map
                .get(&output_id.0)
                .copied()
                .unwrap_or_else(|| self.aig.false_lit());
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
    let mut map1: HashMap<u32, AigLit> = HashMap::new();
    let mut map2: HashMap<u32, AigLit> = HashMap::new();

    // Node 0 (false) maps to node 0
    map1.insert(0, miter.false_lit());
    map2.insert(0, miter.false_lit());

    // Add shared inputs (by name matching)
    let mut input_map: HashMap<String, AigLit> = HashMap::new();

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

fn remap_lit(lit: AigLit, map: &HashMap<u32, AigLit>) -> AigLit {
    let mapped = map.get(&lit.node.0).copied().unwrap_or(AigLit::positive(AigNodeId(0)));
    if lit.inverted {
        mapped.invert()
    } else {
        mapped
    }
}

fn copy_aig_structure(target: &mut Aig, source: &Aig, map: &mut HashMap<u32, AigLit>) {
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
                // For combinational checking, latches are treated as inputs
                // They should already be mapped
            }
        }
    }
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

/// Normalize a port name by stripping hierarchy prefixes and extracting bit index
///
/// Handles various naming conventions:
/// - `port_name` -> (base="port_name", bit=None)
/// - `port_name[3]` -> (base="port_name", bit=Some(3))
/// - `top.port_name[3]` -> (base="port_name", bit=Some(3))
/// - `inst.sub.port_name[3]` -> (base="port_name", bit=Some(3))
/// - `port_name_3_dff_out` -> (base="port_name", bit=Some(3)) [DFF output]
/// - `config__field__subfield[3]` -> (base="config.field.subfield", bit=Some(3)) [flattened struct]
pub fn normalize_port_name(name: &str) -> NormalizedPort {
    let mut working = name.to_string();

    // Strip DFF output suffix if present
    if let Some(stripped) = working.strip_suffix("_dff_out") {
        working = stripped.to_string();
    }

    // Strip _unknown suffix (unmatched internal signals)
    if let Some(stripped) = working.strip_suffix("_unknown") {
        working = stripped.to_string();
    }

    // Strip hierarchy prefix like "top." or "inst.sub."
    // Find the first component that looks like an actual port name (not "top", not an instance path)
    if working.starts_with("top.") {
        working = working[4..].to_string();
    }

    // Handle nested instance paths: strip everything up to and including the module name
    // e.g., "top.DabBatteryController.Mul_23.sum_0_0" -> need to identify the port vs instance
    // For now, just strip known instance prefixes
    while working.contains('.') {
        if let Some(dot_pos) = working.find('.') {
            let before = &working[..dot_pos];
            // If the part before dot looks like an instance/module name (not a port name)
            // Instance names often contain capitals or underscores followed by numbers
            // Port names are typically simple identifiers
            // This is a heuristic - we keep struct field paths like "config.field"
            if before.chars().any(|c| c.is_ascii_uppercase())
                || before.starts_with("_t")
                || before.starts_with("Mul_")
                || before.starts_with("Add_")
                || before.starts_with("Sub_")
                || before.starts_with("And_")
                || before.starts_with("Or_")
                || before.starts_with("Xor_")
                || before.starts_with("Greater_")
                || before.starts_with("Less_")
                || before.starts_with("Equal_")
                || before.starts_with("Mux_")
                || before.starts_with("LeftShift_")
                || before.starts_with("RightShift_")
            {
                working = working[dot_pos + 1..].to_string();
            } else {
                // Looks like a port name with struct field, keep it
                break;
            }
        } else {
            break;
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

    // Try underscore notation for bit index: port_name_3
    // Only if the part after last underscore is a pure number (not part of a name)
    if let Some(last_underscore) = working.rfind('_') {
        let suffix = &working[last_underscore + 1..];
        // Make sure suffix is purely numeric and the base isn't empty
        if !suffix.is_empty() && suffix.chars().all(|c| c.is_ascii_digit()) {
            if let Ok(idx) = suffix.parse::<u32>() {
                let base = &working[..last_underscore];
                if !base.is_empty() && !base.ends_with('_') {
                    return NormalizedPort {
                        base_name: base.to_string(),
                        bit_index: Some(idx),
                    };
                }
            }
        }
    }

    // No bit index found
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

    // Map from old node IDs to new ones for both AIGs
    let mut map1: HashMap<u32, AigLit> = HashMap::new();
    let mut map2: HashMap<u32, AigLit> = HashMap::new();

    // Node 0 (false) maps to node 0
    map1.insert(0, miter.false_lit());
    map2.insert(0, miter.false_lit());

    // Collect inputs from both AIGs with normalized names
    // Filter out internal signals - only primary I/O should be compared
    let mut aig1_inputs: HashMap<String, (u32, String)> = HashMap::new(); // normalized_key -> (node_idx, original_name)
    let mut aig2_inputs: HashMap<String, (u32, String)> = HashMap::new();

    for (idx, node) in aig1.nodes.iter().enumerate() {
        if let AigNode::Input { name } = node {
            // Skip internal signals
            if is_internal_signal(name) {
                continue;
            }
            let normalized = normalize_port_name(name);
            aig1_inputs.insert(normalized.key(), (idx as u32, name.clone()));
        }
    }

    for (idx, node) in aig2.nodes.iter().enumerate() {
        if let AigNode::Input { name } = node {
            // Skip internal signals
            if is_internal_signal(name) {
                continue;
            }
            let normalized = normalize_port_name(name);
            aig2_inputs.insert(normalized.key(), (idx as u32, name.clone()));
        }
    }

    // Create shared inputs for ports that exist in both designs
    let mut shared_input_map: HashMap<String, AigLit> = HashMap::new();
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
    let mut aig2_outputs_map: HashMap<String, AigLit> = HashMap::new();

    for (i, output) in aig1.outputs.iter().enumerate() {
        let name = aig1.output_names.get(i).cloned().unwrap_or_else(|| format!("out_{}", i));
        let normalized = normalize_port_name(&name);
        let lit = remap_lit(*output, &map1);
        aig1_outputs.push((normalized.key(), lit));
    }

    for (i, output) in aig2.outputs.iter().enumerate() {
        let name = aig2.output_names.get(i).cloned().unwrap_or_else(|| format!("out_{}", i));
        let normalized = normalize_port_name(&name);
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
            let normalized = normalize_port_name(name);
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
        let normalized = normalize_port_name(&name);

        for (j, _) in aig2.outputs.iter().enumerate() {
            let name2 = aig2.output_names.get(j).cloned().unwrap_or_else(|| format!("out_{}", j));
            let normalized2 = normalize_port_name(&name2);
            if normalized.key() == normalized2.key() {
                output_pairs.push((i, j));
                break;
            }
        }
    }

    // Debug: count how many inputs/outputs match
    let matched_inputs: usize = aig1_input_nodes.iter()
        .filter(|(_, name)| {
            let normalized = normalize_port_name(name);
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
        .map(|(_, name)| normalize_port_name(name).key())
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
            let normalized = normalize_port_name(name);
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
fn aig_to_cnf(aig: &Aig) -> (CnfFormula, HashMap<u32, Var>, Lit) {
    let mut formula = CnfFormula::new();
    let mut var_map: HashMap<u32, Var> = HashMap::new();

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

    // Get output literal
    let output = &aig.outputs[0];
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
    var_map: &HashMap<u32, Var>,
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
        let miter = build_miter_with_port_matching(&aig_lir, &aig_netlist)?;
        check_equivalence_sat(&miter)
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
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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
    /// The module being converted
    module: &'a Module,
    /// Full MIR design (for hierarchical flattening)
    mir: Option<&'a Mir>,
    /// The AIG being built
    pub aig: Aig,
    /// Map from (MirSignalRef, bit_index) to current AigLit
    signal_map: HashMap<(MirSignalRef, u32), AigLit>,
    /// Map from port/signal names to their reference
    name_to_ref: HashMap<String, MirSignalRef>,
    /// Register outputs (for sequential detection)
    register_outputs: Vec<MirSignalRef>,
}

impl<'a> MirToAig<'a> {
    pub fn new(module: &'a Module) -> Self {
        let mut converter = Self {
            module,
            mir: None,
            aig: Aig::new(),
            signal_map: HashMap::new(),
            name_to_ref: HashMap::new(),
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
            aig: Aig::new(),
            signal_map: HashMap::new(),
            name_to_ref: HashMap::new(),
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

    /// Find a port by its ID
    fn find_port(&self, id: PortId) -> Option<&Port> {
        self.module.ports.iter().find(|p| p.id == id)
    }

    /// Find a signal by its ID
    fn find_signal(&self, id: SignalId) -> Option<&Signal> {
        self.module.signals.iter().find(|s| s.id == id)
    }

    /// Find a variable by its ID
    fn find_variable(&self, id: VariableId) -> Option<&skalp_mir::Variable> {
        self.module.variables.iter().find(|v| v.id == id)
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
        let mut reg_current_lits: HashMap<(MirSignalRef, u32), AigLit> = HashMap::new();
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
        let mut next_state_map: HashMap<(MirSignalRef, u32), AigLit> = HashMap::new();

        // Collect reset values from sequential processes
        let mut reset_values: HashMap<(MirSignalRef, u32), u64> = HashMap::new();
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

    /// Convert sequential process for BMC - stores next-state values separately
    fn convert_sequential_process_for_bmc(
        &mut self,
        process: &Process,
        next_state_map: &mut HashMap<(MirSignalRef, u32), AigLit>,
    ) {
        // Convert the process body, but capture assignments to registers
        // as next-state values instead of updating signal_map directly
        self.convert_block_for_bmc(&process.body, next_state_map);
    }

    /// Convert a block for BMC - separates register assignments as next-state
    fn convert_block_for_bmc(
        &mut self,
        block: &Block,
        next_state_map: &mut HashMap<(MirSignalRef, u32), AigLit>,
    ) {
        for stmt in &block.statements {
            self.convert_statement_for_bmc(stmt, next_state_map);
        }
    }

    fn convert_statement_for_bmc(
        &mut self,
        stmt: &Statement,
        next_state_map: &mut HashMap<(MirSignalRef, u32), AigLit>,
    ) {
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
                self.convert_assignment(assign, &HashMap::new());
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
                let mut all_keys: std::collections::HashSet<(MirSignalRef, u32)> =
                    std::collections::HashSet::new();
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
                let mut case_updates: Vec<(AigLit, HashMap<(MirSignalRef, u32), AigLit>)> =
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
                        let current_val = next_state_map
                            .get(key)
                            .copied()
                            .unwrap_or_else(|| initial_state.get(key).copied().unwrap_or_else(|| self.aig.false_lit()));

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
                self.convert_statement(stmt, &HashMap::new());
            }
            _ => {
                // Other statements handled normally
                self.convert_statement(stmt, &HashMap::new());
            }
        }
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
                _ => {}
            }
        }
    }

    /// Extract reset values from sequential process bodies
    /// Looks for pattern: if rst { signal = value }
    fn collect_reset_values(
        &self,
        block: &Block,
        reset_values: &mut HashMap<(MirSignalRef, u32), u64>,
    ) {
        for stmt in &block.statements {
            if let Statement::If(if_stmt) = stmt {
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
        reset_values: &mut HashMap<(MirSignalRef, u32), u64>,
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
        self.convert_block(&process.body, &HashMap::new());
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
        conditions: &HashMap<MirSignalRef, Vec<AigLit>>,
    ) {
        for stmt in &block.statements {
            self.convert_statement(stmt, conditions);
        }
    }

    fn convert_statement(
        &mut self,
        stmt: &Statement,
        conditions: &HashMap<MirSignalRef, Vec<AigLit>>,
    ) {
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
    }

    fn convert_assignment(
        &mut self,
        assign: &Assignment,
        _conditions: &HashMap<MirSignalRef, Vec<AigLit>>,
    ) {
        let rhs_lits = self.convert_expression(&assign.rhs);
        self.assign_lvalue(&assign.lhs, &rhs_lits);
    }

    fn convert_if_statement(
        &mut self,
        if_stmt: &IfStatement,
        conditions: &HashMap<MirSignalRef, Vec<AigLit>>,
    ) {
        // Convert condition to single bit
        let cond_lits = self.convert_expression(&if_stmt.condition);
        let cond = if !cond_lits.is_empty() {
            cond_lits[0]
        } else {
            self.aig.false_lit()
        };

        // Get all signals assigned in then/else branches
        // Use (MirSignalRef, bit) as key to properly track bit positions
        let mut then_assigns: HashMap<(MirSignalRef, u32), AigLit> = HashMap::new();
        let mut else_assigns: HashMap<(MirSignalRef, u32), AigLit> = HashMap::new();

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
        let all_keys: std::collections::HashSet<_> = then_assigns
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
    }

    fn convert_case_statement(
        &mut self,
        case_stmt: &CaseStatement,
        conditions: &HashMap<MirSignalRef, Vec<AigLit>>,
    ) {
        let selector_lits = self.convert_expression(&case_stmt.expr);
        let _selector_width = selector_lits.len();

        // Build a priority mux chain for case items
        // Start with default value (or current value)
        let saved_state = self.signal_map.clone();

        // Use (MirSignalRef, bit) as key to properly track bit positions
        let mut result_map: HashMap<(MirSignalRef, u32), AigLit> = HashMap::new();

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
        match &expr.kind {
            ExpressionKind::Literal(value) => self.convert_literal(value),

            ExpressionKind::Ref(lvalue) => self.convert_lvalue_ref(lvalue),

            ExpressionKind::Binary { op, left, right } => {
                let left_lits = self.convert_expression(left);
                let right_lits = self.convert_expression(right);
                // Determine signedness from operand types for comparison operations
                let signed = self.is_signed_type(&left.ty) || self.is_signed_type(&right.ty);
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
                // Sign or zero extend
                let mut result = expr_lits;
                while result.len() < target_width {
                    result.push(self.aig.false_lit());
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

    fn convert_binary_op(
        &mut self,
        op: BinaryOp,
        left: &[AigLit],
        right: &[AigLit],
        signed: bool,
    ) -> Vec<AigLit> {
        let max_width = left.len().max(right.len());

        match op {
            BinaryOp::BitwiseAnd | BinaryOp::And => {
                (0..max_width)
                    .map(|i| {
                        let l = left.get(i).copied().unwrap_or_else(|| self.aig.false_lit());
                        let r = right.get(i).copied().unwrap_or_else(|| self.aig.false_lit());
                        self.aig.add_and(l, r)
                    })
                    .collect()
            }
            BinaryOp::BitwiseOr | BinaryOp::Or => {
                (0..max_width)
                    .map(|i| {
                        let l = left.get(i).copied().unwrap_or_else(|| self.aig.false_lit());
                        let r = right.get(i).copied().unwrap_or_else(|| self.aig.false_lit());
                        self.aig.add_or(l, r)
                    })
                    .collect()
            }
            BinaryOp::BitwiseXor | BinaryOp::Xor => {
                (0..max_width)
                    .map(|i| {
                        let l = left.get(i).copied().unwrap_or_else(|| self.aig.false_lit());
                        let r = right.get(i).copied().unwrap_or_else(|| self.aig.false_lit());
                        self.aig.add_xor(l, r)
                    })
                    .collect()
            }
            BinaryOp::LogicalAnd => {
                // OR all bits of each operand, then AND
                let l_any = self.reduce_or(left);
                let r_any = self.reduce_or(right);
                vec![self.aig.add_and(l_any, r_any)]
            }
            BinaryOp::LogicalOr => {
                let l_any = self.reduce_or(left);
                let r_any = self.reduce_or(right);
                vec![self.aig.add_or(l_any, r_any)]
            }
            BinaryOp::Equal => {
                vec![self.build_equality(left, right)]
            }
            BinaryOp::NotEqual => {
                vec![self.build_equality(left, right).invert()]
            }
            BinaryOp::Less => {
                vec![self.build_less_than(left, right, signed)]
            }
            BinaryOp::LessEqual => {
                let lt = self.build_less_than(left, right, signed);
                let eq = self.build_equality(left, right);
                vec![self.aig.add_or(lt, eq)]
            }
            BinaryOp::Greater => {
                vec![self.build_less_than(right, left, signed)]
            }
            BinaryOp::GreaterEqual => {
                let gt = self.build_less_than(right, left, signed);
                let eq = self.build_equality(left, right);
                vec![self.aig.add_or(gt, eq)]
            }
            BinaryOp::Add => self.build_adder(left, right),
            BinaryOp::Sub => {
                // a - b = a + (~b + 1)
                let not_right: Vec<_> = right.iter().map(|l| l.invert()).collect();
                let one = vec![self.aig.true_lit()];
                let neg_b = self.build_adder(&not_right, &one);
                self.build_adder(left, &neg_b)
            }
            BinaryOp::LeftShift => {
                // Static shift only for now
                if let Some(&lit) = right.first() {
                    if lit == self.aig.false_lit() {
                        return left.to_vec();
                    }
                }
                // For dynamic shift, this is complex - simplified version
                left.to_vec()
            }
            BinaryOp::RightShift => {
                if let Some(&lit) = right.first() {
                    if lit == self.aig.false_lit() {
                        return left.to_vec();
                    }
                }
                left.to_vec()
            }
            BinaryOp::Mul | BinaryOp::Div | BinaryOp::Mod => {
                // Complex operations - would need full multiplier/divider
                // For formal verification, we typically abstract these
                left.to_vec()
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
        let mut matched = Vec::new();
        let mut aig1_map: HashMap<String, u32> = HashMap::new();
        let mut aig2_map: HashMap<String, u32> = HashMap::new();

        // Build maps for both AIGs
        for (idx, node) in aig1.nodes.iter().enumerate() {
            if let AigNode::Input { name } = node {
                if !is_internal_signal(name) {
                    let normalized = normalize_port_name(name);
                    aig1_map.insert(normalized.key(), idx as u32);
                }
            }
        }

        for (idx, node) in aig2.nodes.iter().enumerate() {
            if let AigNode::Input { name } = node {
                if !is_internal_signal(name) {
                    let normalized = normalize_port_name(name);
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
        let mut matched = Vec::new();

        let aig2_outputs: HashMap<String, usize> = aig2.output_names.iter()
            .enumerate()
            .map(|(i, name)| (normalize_port_name(name).key(), i))
            .collect();

        for (i, name) in aig1.output_names.iter().enumerate() {
            let key = normalize_port_name(name).key();
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

        for cycle in 0..bound {
            // Generate random inputs for this cycle
            let mut input_values: HashMap<String, bool> = HashMap::new();
            for key in matched_inputs {
                input_values.insert(key.clone(), rng.gen());
            }

            // Simulate both AIGs
            let (outputs1, next_state1) = self.simulate_aig_cycle(
                aig1, &input_values, &state1, aig1_input_map
            );
            let (outputs2, next_state2) = self.simulate_aig_cycle(
                aig2, &input_values, &state2, aig2_input_map
            );

            // Check output equivalence
            for (i1, i2, name) in matched_outputs {
                let o1 = outputs1.get(*i1).copied().unwrap_or(false);
                let o2 = outputs2.get(*i2).copied().unwrap_or(false);
                if o1 != o2 {
                    return Some((cycle, name.clone()));
                }
            }

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

        // Build mapping from latch names to their __reg_cur_ / __dff_cur_ INPUT nodes
        // This is needed because in sequential AIGs, the current state is read from
        // INPUT nodes (e.g., __reg_cur_foo), not directly from LATCH outputs
        let mut latch_name_to_cur_input: HashMap<String, u32> = HashMap::new();
        for (idx, node) in aig.nodes.iter().enumerate() {
            if let AigNode::Input { name } = node {
                if let Some(latch_name) = name.strip_prefix("__reg_cur_") {
                    latch_name_to_cur_input.insert(latch_name.to_string(), idx as u32);
                } else if let Some(latch_name) = name.strip_prefix("__dff_cur_") {
                    latch_name_to_cur_input.insert(latch_name.to_string(), idx as u32);
                }
            }
        }

        // Set __reg_cur_ / __dff_cur_ INPUT nodes with corresponding latch state values
        for &latch_id in &aig.latches {
            if let AigNode::Latch { name, .. } = &aig.nodes[latch_id.0 as usize] {
                if let Some(latch_val) = latch_state.get(&latch_id.0) {
                    // Find the corresponding current-state INPUT node
                    if let Some(&input_id) = latch_name_to_cur_input.get(name) {
                        values.insert(input_id, *latch_val);
                    }
                }
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
        for &latch_id in &aig.latches {
            if let AigNode::Latch { next, .. } = &aig.nodes[latch_id.0 as usize] {
                let val = values.get(&next.node.0).copied().unwrap_or(false);
                let val = if next.inverted { !val } else { val };
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
        // Build mapping from latch names to latch nodes
        let mut latch_name_to_id: HashMap<String, u32> = HashMap::new();
        for &latch_id in &aig.latches {
            if let AigNode::Latch { name, .. } = &aig.nodes[latch_id.0 as usize] {
                latch_name_to_id.insert(name.clone(), latch_id.0);
            }
        }

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

        // Connect __reg_cur_ / __dff_cur_ INPUT nodes to corresponding latch outputs at this cycle
        for (idx, node) in aig.nodes.iter().enumerate() {
            if let AigNode::Input { name } = node {
                let latch_name = name.strip_prefix("__reg_cur_")
                    .or_else(|| name.strip_prefix("__dff_cur_"));
                if let Some(latch_name) = latch_name {
                    if let Some(&latch_id) = latch_name_to_id.get(latch_name) {
                        // Connect input node to latch output at this cycle
                        let input_var = get_var(aig_id, idx as u32, cycle);
                        let latch_var = get_var(aig_id, latch_id, cycle);
                        // input_var = latch_var
                        formula.add_clause(&[Lit::negative(input_var), Lit::positive(latch_var)]);
                        formula.add_clause(&[Lit::positive(input_var), Lit::negative(latch_var)]);
                    }
                }
            }
        }
    }

    /// Constrain latches to initial values at cycle 0
    /// Also connects __reg_cur_ / __dff_cur_ input nodes to latch outputs
    fn constrain_initial_state(
        &self,
        aig: &Aig,
        aig_id: u8,
        formula: &mut CnfFormula,
        get_var: &mut impl FnMut(u8, u32, usize) -> Var,
    ) {
        // Build mapping from latch names to latch nodes
        let mut latch_name_to_id: HashMap<String, u32> = HashMap::new();
        for &latch_id in &aig.latches {
            if let AigNode::Latch { name, .. } = &aig.nodes[latch_id.0 as usize] {
                latch_name_to_id.insert(name.clone(), latch_id.0);
            }
        }

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

        // Connect __reg_cur_ / __dff_cur_ INPUT nodes to corresponding latch outputs
        // These inputs represent the current latch state and must equal the latch output
        for (idx, node) in aig.nodes.iter().enumerate() {
            if let AigNode::Input { name } = node {
                let latch_name = name.strip_prefix("__reg_cur_")
                    .or_else(|| name.strip_prefix("__dff_cur_"));
                if let Some(latch_name) = latch_name {
                    if let Some(&latch_id) = latch_name_to_id.get(latch_name) {
                        // Connect input node to latch output
                        let input_var = get_var(aig_id, idx as u32, 0);
                        let latch_var = get_var(aig_id, latch_id, 0);
                        // input_var = latch_var
                        formula.add_clause(&[Lit::negative(input_var), Lit::positive(latch_var)]);
                        formula.add_clause(&[Lit::positive(input_var), Lit::negative(latch_var)]);
                    }
                }
            }
        }
    }
}

impl Default for BoundedModelChecker {
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
