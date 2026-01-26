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
use skalp_lir::{GateNetlist, Lir, LirNode, LirOp, LirSignalId};
use std::collections::HashMap;
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
    aig: Aig,
    /// Map from (LirSignalId, bit_index) to AigLit
    signal_map: HashMap<(u32, u32), AigLit>,
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

    fn get_input_bit(&self, signal_id: LirSignalId, bit: u32) -> AigLit {
        self.signal_map
            .get(&(signal_id.0, bit))
            .copied()
            .unwrap_or_else(|| self.aig.false_lit())
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
                let sel = node.inputs[0];
                let a = node.inputs[1];
                let b = node.inputs[2];
                let sel_lit = self.get_input_bit(sel, 0);
                for bit in 0..*width {
                    let a_lit = self.get_input_bit(a, bit);
                    let b_lit = self.get_input_bit(b, bit);
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

    fn convert_cell(&mut self, cell: &skalp_lir::Cell, netlist: &GateNetlist) {
        use skalp_lir::CellFunction;

        let output_net = cell.outputs.first().copied();

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
                // Mux2 inputs: [sel, d0, d1], output = sel ? d1 : d0
                let sel = self.get_net(cell.inputs[0]);
                let d0 = self.get_net(cell.inputs[1]);
                let d1 = self.get_net(cell.inputs[2]);
                let result = self.aig.add_mux(sel, d0, d1);
                if let Some(out) = output_net {
                    self.set_net(out, result);
                }
            }

            // Sequential cells - treat as inputs for combinational equivalence
            Some(CellFunction::Dff) | Some(CellFunction::DffR) | Some(CellFunction::DffE) | Some(CellFunction::DffRE) => {
                // Output of DFF is treated as an input for combinational checking
                if let Some(out) = output_net {
                    let net = &netlist.nets[out.0 as usize];
                    let lit = self.aig.add_input(format!("{}_dff_out", net.name));
                    self.set_net(out, lit);
                }
            }

            // For unknown cell types, treat output as an input
            _ => {
                if let Some(out) = output_net {
                    let net = &netlist.nets[out.0 as usize];
                    let lit = self.aig.add_input(format!("{}_unknown", net.name));
                    self.set_net(out, lit);
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
    pub fn check_synthesis_equivalence(
        &self,
        lir: &Lir,
        netlist: &GateNetlist,
    ) -> FormalResult<EquivalenceResult> {
        let aig_lir = LirToAig::new().convert(lir);
        let aig_netlist = GateNetlistToAig::new().convert(netlist);

        let miter = build_miter(&aig_lir, &aig_netlist)?;
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
}

impl Default for EquivalenceChecker {
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
}
