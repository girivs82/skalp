//! LIR to Synthesis AIG Converter
//!
//! Converts word-level LIR directly to the synthesis AIG representation,
//! bypassing the intermediate GateNetlist stage. This is the canonical path
//! for synthesis: all combinational and sequential logic goes through AIG
//! optimization before technology mapping.
//!
//! Adapted from the formal equivalence checker's LirToAig (in skalp-formal)
//! but targets the synthesis AIG which has safety info, clock/reset tracking,
//! and structural hashing.

use crate::lir::{Lir, LirNode, LirOp, LirSignalId};
use crate::gate_netlist::GateNetId;

use super::aig::{Aig, AigLit, AigNodeId, AigSafetyInfo};

use std::collections::{BTreeMap, HashMap, HashSet, VecDeque};

/// Converts word-level LIR to synthesis AIG.
///
/// All combinational logic (arithmetic, comparisons, shifts, MUX, reductions)
/// is decomposed into AIG AND/INV nodes. Sequential elements (Reg) become
/// AIG latches with clock/reset metadata. Memory blocks and NCL ops are
/// returned separately as "physical" nodes for special handling.
pub struct LirToSynthAig<'a> {
    lir: &'a Lir,
    aig: Aig,
    /// Map from (signal_id, bit_index) to AIG literal
    signal_map: BTreeMap<(u32, u32), AigLit>,
    /// LIR node indices that cannot be represented in AIG (MemBlock, NCL, etc.)
    physical_nodes: Vec<usize>,
}

/// Result of LIR-to-AIG conversion
pub struct LirToAigResult {
    /// The synthesized AIG
    pub aig: Aig,
    /// Indices into lir.nodes for nodes that need special handling (BRAM, NCL, DSP)
    pub physical_node_indices: Vec<usize>,
}

impl<'a> LirToSynthAig<'a> {
    pub fn new(lir: &'a Lir) -> Self {
        Self {
            lir,
            aig: Aig::new(lir.name.clone()),
            signal_map: BTreeMap::new(),
            physical_nodes: Vec::new(),
        }
    }

    /// Build the AIG from LIR
    pub fn build(mut self) -> LirToAigResult {
        // Debug: dump all signals
        for (idx, sig) in self.lir.signals.iter().enumerate() {
            if sig.width > 1 || !sig.name.is_empty() {
                eprintln!("[LIR_DEBUG] signal[{}]: name='{}' width={}", idx, sig.name, sig.width);
            }
        }
        // Debug: dump all nodes
        for (idx, node) in self.lir.nodes.iter().enumerate() {
            eprintln!("[LIR_DEBUG] node[{}]: op={:?} output=signal_{} inputs={:?}", idx, std::mem::discriminant(&node.op), node.output.0, node.inputs.iter().map(|i| i.0).collect::<Vec<_>>());
        }

        // Phase 1: Find register and physical nodes
        let mut register_indices: Vec<usize> = Vec::new();
        for (idx, node) in self.lir.nodes.iter().enumerate() {
            if matches!(node.op, LirOp::Reg { .. } | LirOp::Latch { .. }) {
                register_indices.push(idx);
            } else if self.is_physical_op(&node.op) {
                self.physical_nodes.push(idx);
            }
        }

        // Phase 2: Add primary inputs (skip clocks/resets — tracked separately)
        let clock_signals: HashSet<u32> = self.lir.clocks.iter().map(|s| s.0).collect();
        let reset_signals: HashSet<u32> = self.lir.resets.iter().map(|s| s.0).collect();

        for &input_id in &self.lir.inputs {
            let signal = &self.lir.signals[input_id.0 as usize];
            let is_clock = clock_signals.contains(&input_id.0);
            let is_reset = reset_signals.contains(&input_id.0);

            for bit in 0..signal.width {
                let name = if signal.width == 1 {
                    signal.name.clone()
                } else {
                    format!("{}[{}]", signal.name, bit)
                };
                let node_id = self.aig.add_input(name, None);
                let lit = AigLit::new(node_id);
                self.signal_map.insert((input_id.0, bit), lit);

                if is_clock {
                    self.aig.clock_inputs.insert(node_id);
                }
                if is_reset {
                    self.aig.reset_inputs.insert(node_id);
                }
            }
        }

        // Phase 3: Pre-create latch outputs (needed for feedback loops)
        // Registers' outputs must exist before combinational logic that reads them
        let mut latch_node_ids: HashMap<(u32, u32), AigNodeId> = HashMap::new();
        for &reg_idx in &register_indices {
            let node = &self.lir.nodes[reg_idx];
            if let LirOp::Reg { width, reset_value, .. } = &node.op {
                let reset_val = reset_value.unwrap_or(0);
                for bit in 0..*width {
                    let init_value = (reset_val >> bit) & 1 != 0;
                    // Pre-create with false_lit as placeholder data
                    let clock_node = self.find_clock_node(node);
                    let reset_node = self.find_reset_node(node);
                    let latch_id = self.aig.add_latch(
                        AigLit::false_lit(),
                        Some(init_value),
                        clock_node,
                        reset_node,
                    );
                    let lit = AigLit::new(latch_id);
                    self.signal_map.insert((node.output.0, bit), lit);
                    latch_node_ids.insert((node.output.0, bit), latch_id);
                }
            }
        }

        // Collect register output signals so we can skip combinational nodes
        // that would overwrite latch outputs (e.g., initial-value Buffers).
        let register_outputs: HashSet<u32> = register_indices
            .iter()
            .map(|&idx| self.lir.nodes[idx].output.0)
            .collect();

        // Phase 4: Topologically sort and process combinational nodes
        let sorted = self.topological_sort();
        for idx in sorted {
            let node = &self.lir.nodes[idx];
            if matches!(node.op, LirOp::Reg { .. } | LirOp::Latch { .. }) {
                continue; // Handled separately
            }
            if self.is_physical_op(&node.op) {
                continue; // Handled by physical partition
            }
            // Skip combinational nodes that output to a register signal —
            // these are initial-value assignments (e.g., Buffer of constant)
            // that would overwrite the latch output created in Phase 3.
            if register_outputs.contains(&node.output.0) {
                continue;
            }
            self.convert_node(node);
        }

        // Phase 5: Connect latch data inputs (now that combinational logic is built)
        for &reg_idx in &register_indices {
            let node = &self.lir.nodes[reg_idx];
            if let LirOp::Reg { width, has_reset, reset_value, has_enable, .. } = &node.op {
                let d_input = node.inputs.first().copied().unwrap_or(LirSignalId(0));
                let d_width = self.lir.signals.get(d_input.0 as usize)
                    .map(|s| s.width)
                    .unwrap_or(*width);
                let reset_val = reset_value.unwrap_or(0);

                for bit in 0..*width {
                    let latch_id = latch_node_ids[&(node.output.0, bit)];

                    // Get D input with zero-extension
                    let d_lit = if bit < d_width {
                        self.get_input_bit(d_input, bit)
                    } else {
                        AigLit::false_lit()
                    };

                    // Handle enable: en ? d : q (hold current value when disabled)
                    let d_after_enable = if *has_enable {
                        if let Some(en_id) = node.inputs.get(1).copied() {
                            let en_lit = self.get_input_bit(en_id, 0);
                            let q_lit = AigLit::new(latch_id); // Current latch output
                            // en ? d : q
                            self.aig.add_mux(en_lit, d_lit, q_lit)
                        } else {
                            d_lit
                        }
                    } else {
                        d_lit
                    };

                    // Handle synchronous reset: rst ? reset_val : d
                    let next_lit = if *has_reset {
                        if let Some(rst_id) = node.reset {
                            let rst_lit = self.get_input_bit(rst_id, 0);
                            let reset_bit = (reset_val >> bit) & 1 != 0;
                            if reset_bit {
                                // rst ? 1 : d = rst | d
                                self.aig.add_or(rst_lit, d_after_enable)
                            } else {
                                // rst ? 0 : d = !rst & d
                                self.aig.add_and(rst_lit.invert(), d_after_enable)
                            }
                        } else {
                            d_after_enable
                        }
                    } else {
                        d_after_enable
                    };

                    self.aig.update_latch_data(latch_id, next_lit);
                }
            }
        }

        // Phase 6: Add primary outputs
        for &output_id in &self.lir.outputs {
            let signal = &self.lir.signals[output_id.0 as usize];
            for bit in 0..signal.width {
                let name = if signal.width == 1 {
                    signal.name.clone()
                } else {
                    format!("{}[{}]", signal.name, bit)
                };
                let lit = self.signal_map
                    .get(&(output_id.0, bit))
                    .copied()
                    .unwrap_or(AigLit::false_lit());
                self.aig.add_output(name, lit);
            }
        }

        LirToAigResult {
            aig: self.aig,
            physical_node_indices: self.physical_nodes.clone(),
        }
    }

    /// Check if a LIR operation cannot be represented in AIG
    fn is_physical_op(&self, op: &LirOp) -> bool {
        matches!(op,
            LirOp::MemBlock { .. } |
            LirOp::MemRead { .. } |
            LirOp::MemWrite { .. } |
            LirOp::Th12 { .. } |
            LirOp::Th22 { .. } |
            LirOp::NclEncode { .. } |
            LirOp::NclDecode { .. } |
            LirOp::NclAnd { .. } |
            LirOp::NclOr { .. } |
            LirOp::NclXor { .. } |
            LirOp::NclNot { .. } |
            LirOp::NclAdd { .. } |
            LirOp::NclSub { .. } |
            LirOp::NclMul { .. } |
            LirOp::NclLt { .. } |
            LirOp::NclEq { .. } |
            LirOp::NclShl { .. } |
            LirOp::NclShr { .. } |
            LirOp::NclMux2 { .. } |
            LirOp::NclReg { .. } |
            LirOp::NclComplete { .. } |
            LirOp::NclNull { .. } |
            LirOp::Tristate { .. }
        )
    }

    /// Find the AIG clock node for a register
    fn find_clock_node(&self, node: &LirNode) -> Option<AigNodeId> {
        node.clock.and_then(|clk_id| {
            self.signal_map.get(&(clk_id.0, 0)).map(|lit| lit.node)
        })
    }

    /// Find the AIG reset node for a register
    fn find_reset_node(&self, node: &LirNode) -> Option<AigNodeId> {
        node.reset.and_then(|rst_id| {
            self.signal_map.get(&(rst_id.0, 0)).map(|lit| lit.node)
        })
    }

    fn get_input_bit(&self, signal_id: LirSignalId, bit: u32) -> AigLit {
        self.signal_map
            .get(&(signal_id.0, bit))
            .copied()
            .unwrap_or_else(|| {
                let sig_name = self.lir.signals.get(signal_id.0 as usize)
                    .map(|s| s.name.as_str()).unwrap_or("???");
                eprintln!(
                    "[LIR_SYNTH_AIG] Missing signal lookup: signal_id={} ({}), bit={}, signal_width={}",
                    signal_id.0, sig_name, bit,
                    self.lir.signals.get(signal_id.0 as usize).map(|s| s.width).unwrap_or(0)
                );
                AigLit::false_lit()
            })
    }

    fn set_output_bit(&mut self, signal_id: LirSignalId, bit: u32, lit: AigLit) {
        self.signal_map.insert((signal_id.0, bit), lit);
    }

    /// Topologically sort LIR nodes (Kahn's algorithm)
    fn topological_sort(&self) -> Vec<usize> {
        let mut signal_producer: HashMap<u32, usize> = HashMap::new();
        for (idx, node) in self.lir.nodes.iter().enumerate() {
            if matches!(node.op, LirOp::Reg { .. } | LirOp::Latch { .. }) {
                continue;
            }
            signal_producer.insert(node.output.0, idx);
        }

        let n = self.lir.nodes.len();
        let mut dependencies: Vec<HashSet<usize>> = vec![HashSet::new(); n];
        for (idx, node) in self.lir.nodes.iter().enumerate() {
            for &input_id in &node.inputs {
                if let Some(&producer_idx) = signal_producer.get(&input_id.0) {
                    if producer_idx != idx {
                        dependencies[idx].insert(producer_idx);
                    }
                }
            }
            if let Some(clk) = node.clock {
                if let Some(&p) = signal_producer.get(&clk.0) {
                    if p != idx { dependencies[idx].insert(p); }
                }
            }
            if let Some(rst) = node.reset {
                if let Some(&p) = signal_producer.get(&rst.0) {
                    if p != idx { dependencies[idx].insert(p); }
                }
            }
        }

        let mut in_degree: Vec<usize> = dependencies.iter().map(|d| d.len()).collect();
        let mut dependents: Vec<Vec<usize>> = vec![Vec::new(); n];
        for (idx, deps) in dependencies.iter().enumerate() {
            for &dep in deps {
                dependents[dep].push(idx);
            }
        }

        let mut queue: VecDeque<usize> = in_degree.iter().enumerate()
            .filter(|(_, &d)| d == 0)
            .map(|(i, _)| i)
            .collect();

        let mut sorted = Vec::with_capacity(n);
        while let Some(idx) = queue.pop_front() {
            sorted.push(idx);
            for &dep in &dependents[idx] {
                in_degree[dep] -= 1;
                if in_degree[dep] == 0 {
                    queue.push_back(dep);
                }
            }
        }

        if sorted.len() != n {
            eprintln!("[LIR_SYNTH_AIG] Cycle detected in topological sort, using original order");
            (0..n).collect()
        } else {
            sorted
        }
    }

    /// Convert a single LIR node to AIG gates
    fn convert_node(&mut self, node: &LirNode) {
        match &node.op {
            LirOp::Constant { width, value } => {
                for bit in 0..*width {
                    let lit = if (value >> bit) & 1 == 1 {
                        AigLit::true_lit()
                    } else {
                        AigLit::false_lit()
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
                let (a, b) = (node.inputs[0], node.inputs[1]);
                for bit in 0..*width {
                    let result = self.aig.add_and(
                        self.get_input_bit(a, bit),
                        self.get_input_bit(b, bit),
                    );
                    self.set_output_bit(node.output, bit, result);
                }
            }

            LirOp::Or { width } => {
                let (a, b) = (node.inputs[0], node.inputs[1]);
                for bit in 0..*width {
                    let result = self.aig.add_or(
                        self.get_input_bit(a, bit),
                        self.get_input_bit(b, bit),
                    );
                    self.set_output_bit(node.output, bit, result);
                }
            }

            LirOp::Xor { width } => {
                let (a, b) = (node.inputs[0], node.inputs[1]);
                for bit in 0..*width {
                    let result = self.aig.add_xor(
                        self.get_input_bit(a, bit),
                        self.get_input_bit(b, bit),
                    );
                    self.set_output_bit(node.output, bit, result);
                }
            }

            LirOp::Nand { width } => {
                let (a, b) = (node.inputs[0], node.inputs[1]);
                for bit in 0..*width {
                    let result = self.aig.add_and(
                        self.get_input_bit(a, bit),
                        self.get_input_bit(b, bit),
                    ).invert();
                    self.set_output_bit(node.output, bit, result);
                }
            }

            LirOp::Nor { width } => {
                let (a, b) = (node.inputs[0], node.inputs[1]);
                for bit in 0..*width {
                    let result = self.aig.add_or(
                        self.get_input_bit(a, bit),
                        self.get_input_bit(b, bit),
                    ).invert();
                    self.set_output_bit(node.output, bit, result);
                }
            }

            LirOp::Mux2 { width } => {
                // Inputs: [sel, else_value, then_value]
                let sel = node.inputs[0];
                let else_val = node.inputs[1];
                let then_val = node.inputs[2];
                let sel_lit = self.get_input_bit(sel, 0);

                for bit in 0..*width {
                    let else_lit = self.get_input_bit(else_val, bit);
                    let then_lit = self.get_input_bit(then_val, bit);
                    let result = self.aig.add_mux(sel_lit, then_lit, else_lit);
                    self.set_output_bit(node.output, bit, result);
                }
            }

            LirOp::MuxN { width, ways } => {
                // N-way MUX: inputs[0] = selector, inputs[1..] = data
                // Build as tree of 2-way MUXes
                if node.inputs.len() < 2 {
                    for bit in 0..*width {
                        self.set_output_bit(node.output, bit, AigLit::false_lit());
                    }
                    return;
                }
                let sel = node.inputs[0];
                let data_inputs: Vec<LirSignalId> = node.inputs[1..].to_vec();
                let sel_width = self.lir.signals.get(sel.0 as usize)
                    .map(|s| s.width)
                    .unwrap_or(1);

                for bit in 0..*width {
                    let mut values: Vec<AigLit> = data_inputs.iter()
                        .map(|&d| self.get_input_bit(d, bit))
                        .collect();

                    // Pad to power of 2 with false
                    while values.len() < (1 << sel_width) as usize {
                        values.push(AigLit::false_lit());
                    }

                    // Build binary MUX tree
                    for stage in 0..sel_width {
                        let sel_bit = self.get_input_bit(sel, stage);
                        let mut next = Vec::new();
                        for pair in values.chunks(2) {
                            let lo = pair[0];
                            let hi = pair.get(1).copied().unwrap_or(AigLit::false_lit());
                            next.push(self.aig.add_mux(sel_bit, hi, lo));
                        }
                        values = next;
                    }

                    self.set_output_bit(node.output, bit, values[0]);
                }
            }

            LirOp::Eq { width } => {
                let (a, b) = (node.inputs[0], node.inputs[1]);
                let mut result = AigLit::true_lit();
                for bit in 0..*width {
                    let bit_eq = self.aig.add_xor(
                        self.get_input_bit(a, bit),
                        self.get_input_bit(b, bit),
                    ).invert();
                    result = self.aig.add_and(result, bit_eq);
                }
                self.set_output_bit(node.output, 0, result);
            }

            LirOp::Ne { width } => {
                let (a, b) = (node.inputs[0], node.inputs[1]);
                let mut result = AigLit::false_lit();
                for bit in 0..*width {
                    let bit_ne = self.aig.add_xor(
                        self.get_input_bit(a, bit),
                        self.get_input_bit(b, bit),
                    );
                    result = self.aig.add_or(result, bit_ne);
                }
                self.set_output_bit(node.output, 0, result);
            }

            LirOp::Add { width, has_carry, .. } => {
                let (a, b) = (node.inputs[0], node.inputs[1]);
                let mut carry = AigLit::false_lit();

                for bit in 0..*width {
                    let a_lit = self.get_input_bit(a, bit);
                    let b_lit = self.get_input_bit(b, bit);

                    let a_xor_b = self.aig.add_xor(a_lit, b_lit);
                    let sum = self.aig.add_xor(a_xor_b, carry);
                    self.set_output_bit(node.output, bit, sum);

                    let a_and_b = self.aig.add_and(a_lit, b_lit);
                    let cin_and_xor = self.aig.add_and(carry, a_xor_b);
                    carry = self.aig.add_or(a_and_b, cin_and_xor);
                }

                if *has_carry {
                    self.set_output_bit(node.output, *width, carry);
                }
            }

            LirOp::Sub { width, has_borrow } => {
                let (a, b) = (node.inputs[0], node.inputs[1]);
                let mut borrow = AigLit::false_lit();

                for bit in 0..*width {
                    let a_lit = self.get_input_bit(a, bit);
                    let b_lit = self.get_input_bit(b, bit);

                    let a_xor_b = self.aig.add_xor(a_lit, b_lit);
                    let diff = self.aig.add_xor(a_xor_b, borrow);
                    self.set_output_bit(node.output, bit, diff);

                    let not_a_and_b = self.aig.add_and(a_lit.invert(), b_lit);
                    let borrow_and_eq = self.aig.add_and(borrow, a_xor_b.invert());
                    borrow = self.aig.add_or(not_a_and_b, borrow_and_eq);
                }

                if *has_borrow {
                    self.set_output_bit(node.output, *width, borrow);
                }
            }

            LirOp::Lt { width } => {
                let (a, b) = (node.inputs[0], node.inputs[1]);
                let borrow = self.unsigned_lt_borrow(a, b, *width);
                self.set_output_bit(node.output, 0, borrow);
            }

            LirOp::Le { width } => {
                // a <= b is !(b < a)
                let (a, b) = (node.inputs[0], node.inputs[1]);
                let borrow = self.unsigned_lt_borrow(b, a, *width);
                self.set_output_bit(node.output, 0, borrow.invert());
            }

            LirOp::Gt { width } => {
                // a > b is b < a
                let (a, b) = (node.inputs[0], node.inputs[1]);
                let borrow = self.unsigned_lt_borrow(b, a, *width);
                self.set_output_bit(node.output, 0, borrow);
            }

            LirOp::Ge { width } => {
                // a >= b is !(a < b)
                let (a, b) = (node.inputs[0], node.inputs[1]);
                let borrow = self.unsigned_lt_borrow(a, b, *width);
                self.set_output_bit(node.output, 0, borrow.invert());
            }

            LirOp::Slt { width } => {
                let (a, b) = (node.inputs[0], node.inputs[1]);
                let borrow = self.signed_lt_borrow(a, b, *width);
                self.set_output_bit(node.output, 0, borrow);
            }

            LirOp::Sle { width } => {
                let (a, b) = (node.inputs[0], node.inputs[1]);
                let borrow = self.signed_lt_borrow(b, a, *width);
                self.set_output_bit(node.output, 0, borrow.invert());
            }

            LirOp::Sgt { width } => {
                let (a, b) = (node.inputs[0], node.inputs[1]);
                let borrow = self.signed_lt_borrow(b, a, *width);
                self.set_output_bit(node.output, 0, borrow);
            }

            LirOp::Sge { width } => {
                let (a, b) = (node.inputs[0], node.inputs[1]);
                let borrow = self.signed_lt_borrow(a, b, *width);
                self.set_output_bit(node.output, 0, borrow.invert());
            }

            LirOp::Shl { width } => {
                self.build_barrel_shifter(node, *width, ShiftDir::Left, false);
            }

            LirOp::Shr { width } => {
                self.build_barrel_shifter(node, *width, ShiftDir::Right, false);
            }

            LirOp::Sar { width } => {
                self.build_barrel_shifter(node, *width, ShiftDir::Right, true);
            }

            LirOp::Mul { width, result_width, signed } => {
                let a = node.inputs[0];
                let b = node.inputs[1];

                let result = if *signed {
                    let a_w = self.lir.signals[a.0 as usize].width;
                    let b_w = self.lir.signals[b.0 as usize].width;
                    self.build_signed_mul(a, b, *width, *result_width, a_w, b_w)
                } else {
                    self.build_unsigned_mul(a, b, *width, *result_width)
                };

                for (bit, lit) in result.into_iter().enumerate() {
                    self.set_output_bit(node.output, bit as u32, lit);
                }
            }

            LirOp::RedAnd { width } => {
                let input = node.inputs[0];
                let mut result = AigLit::true_lit();
                for bit in 0..*width {
                    result = self.aig.add_and(result, self.get_input_bit(input, bit));
                }
                self.set_output_bit(node.output, 0, result);
            }

            LirOp::RedOr { width } => {
                let input = node.inputs[0];
                let mut result = AigLit::false_lit();
                for bit in 0..*width {
                    result = self.aig.add_or(result, self.get_input_bit(input, bit));
                }
                self.set_output_bit(node.output, 0, result);
            }

            LirOp::RedXor { width } => {
                let input = node.inputs[0];
                let mut result = AigLit::false_lit();
                for bit in 0..*width {
                    result = self.aig.add_xor(result, self.get_input_bit(input, bit));
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

            LirOp::BitSelect { width } => {
                // For constant index (common case), direct bit selection
                // For variable index, build MUX tree
                let data = node.inputs[0];
                let index = node.inputs[1];
                let data_width = self.lir.signals.get(data.0 as usize)
                    .map(|s| s.width)
                    .unwrap_or(1);

                // Build MUX tree for variable bit select
                let mut values: Vec<AigLit> = (0..data_width)
                    .map(|bit| self.get_input_bit(data, bit))
                    .collect();

                let sel_width = self.lir.signals.get(index.0 as usize)
                    .map(|s| s.width)
                    .unwrap_or(1);

                // Pad to power of 2
                while values.len() < (1usize << sel_width) {
                    values.push(AigLit::false_lit());
                }

                for stage in 0..sel_width {
                    let sel_bit = self.get_input_bit(index, stage);
                    let mut next = Vec::new();
                    for pair in values.chunks(2) {
                        let lo = pair[0];
                        let hi = pair.get(1).copied().unwrap_or(AigLit::false_lit());
                        next.push(self.aig.add_mux(sel_bit, hi, lo));
                    }
                    values = next;
                }

                self.set_output_bit(node.output, 0, values[0]);
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
                    self.set_output_bit(node.output, bit, AigLit::false_lit());
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

            // Reg/Latch handled in build(), skip here
            LirOp::Reg { .. } | LirOp::Latch { .. } => {}

            // Physical ops handled separately
            _ if self.is_physical_op(&node.op) => {}

            // Unimplemented ops: create placeholder false outputs
            _ => {
                let width = node.op.output_width();
                for bit in 0..width {
                    self.set_output_bit(node.output, bit, AigLit::false_lit());
                }
                eprintln!(
                    "[LIR_SYNTH_AIG] Unimplemented op {:?} at {}, using false",
                    std::mem::discriminant(&node.op),
                    node.path
                );
            }
        }
    }

    /// Compute unsigned a < b using subtraction borrow chain
    fn unsigned_lt_borrow(&mut self, a: LirSignalId, b: LirSignalId, width: u32) -> AigLit {
        let mut borrow = AigLit::false_lit();
        for bit in 0..width {
            let a_lit = self.get_input_bit(a, bit);
            let b_lit = self.get_input_bit(b, bit);
            let not_a_and_b = self.aig.add_and(a_lit.invert(), b_lit);
            let a_xor_b = self.aig.add_xor(a_lit, b_lit);
            let borrow_and_eq = self.aig.add_and(borrow, a_xor_b.invert());
            borrow = self.aig.add_or(not_a_and_b, borrow_and_eq);
        }
        borrow
    }

    /// Compute signed a < b (flip MSB, then unsigned comparison)
    fn signed_lt_borrow(&mut self, a: LirSignalId, b: LirSignalId, width: u32) -> AigLit {
        let mut borrow = AigLit::false_lit();
        for bit in 0..width {
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
        borrow
    }

    /// Build barrel shifter for shift operations
    fn build_barrel_shifter(&mut self, node: &LirNode, width: u32, dir: ShiftDir, arithmetic: bool) {
        let data = node.inputs[0];
        let amount = node.inputs[1];
        let fill_bit = if arithmetic {
            self.get_input_bit(data, width - 1)
        } else {
            AigLit::false_lit()
        };

        let mut current: Vec<AigLit> = (0..width)
            .map(|bit| self.get_input_bit(data, bit))
            .collect();

        let stages = if width <= 1 { 1 } else { (width as f32).log2().ceil() as u32 };
        for stage in 0..stages {
            let shift_bit = self.get_input_bit(amount, stage);
            let shift_amount = 1u32 << stage;
            let mut next = vec![AigLit::false_lit(); width as usize];

            for bit in 0..width {
                let orig = current[bit as usize];
                let shifted = match dir {
                    ShiftDir::Left => {
                        if bit >= shift_amount {
                            current[(bit - shift_amount) as usize]
                        } else {
                            AigLit::false_lit()
                        }
                    }
                    ShiftDir::Right => {
                        if bit + shift_amount < width {
                            current[(bit + shift_amount) as usize]
                        } else {
                            fill_bit
                        }
                    }
                };
                next[bit as usize] = self.aig.add_mux(shift_bit, shifted, orig);
            }
            current = next;
        }

        for bit in 0..width {
            self.set_output_bit(node.output, bit, current[bit as usize]);
        }
    }

    /// Grade-school unsigned multiplier
    fn build_unsigned_mul(
        &mut self,
        a: LirSignalId,
        b: LirSignalId,
        width: u32,
        result_width: u32,
    ) -> Vec<AigLit> {
        let mut result: Vec<AigLit> = vec![AigLit::false_lit(); result_width as usize];

        for i in 0..width {
            let b_bit = self.get_input_bit(b, i);
            let mut carry = AigLit::false_lit();

            for j in 0..width {
                let out_idx = (i + j) as usize;
                if out_idx >= result_width as usize { break; }

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
            let mut idx = (i + width) as usize;
            while idx < result_width as usize {
                let sum = self.aig.add_xor(result[idx], carry);
                carry = self.aig.add_and(result[idx], carry);
                result[idx] = sum;
                if carry == AigLit::false_lit() { break; }
                idx += 1;
            }
        }

        result
    }

    /// Signed multiplication using sign-magnitude approach
    fn build_signed_mul(
        &mut self,
        a: LirSignalId,
        b: LirSignalId,
        width: u32,
        result_width: u32,
        a_actual_width: u32,
        b_actual_width: u32,
    ) -> Vec<AigLit> {
        let a_sign = self.get_input_bit(a, a_actual_width - 1);
        let b_sign = self.get_input_bit(b, b_actual_width - 1);
        let result_sign = self.aig.add_xor(a_sign, b_sign);

        // Sign-extend inputs
        let a_bits: Vec<AigLit> = (0..width)
            .map(|i| if i < a_actual_width { self.get_input_bit(a, i) } else { a_sign })
            .collect();
        let b_bits: Vec<AigLit> = (0..width)
            .map(|i| if i < b_actual_width { self.get_input_bit(b, i) } else { b_sign })
            .collect();

        // Get magnitudes
        let a_neg = self.negate_vector(&a_bits);
        let a_mag = self.mux_vectors(a_sign, &a_neg, &a_bits);
        let b_neg = self.negate_vector(&b_bits);
        let b_mag = self.mux_vectors(b_sign, &b_neg, &b_bits);

        // Unsigned multiply magnitudes
        let unsigned_result = self.unsigned_mul_vectors(&a_mag, &b_mag, result_width);

        // Conditionally negate
        let neg_result = self.negate_vector(&unsigned_result);
        self.mux_vectors(result_sign, &neg_result, &unsigned_result)
    }

    fn unsigned_mul_vectors(&mut self, a: &[AigLit], b: &[AigLit], result_width: u32) -> Vec<AigLit> {
        let width = a.len() as u32;
        let mut result: Vec<AigLit> = vec![AigLit::false_lit(); result_width as usize];

        for i in 0..width {
            let b_bit = b.get(i as usize).copied().unwrap_or(AigLit::false_lit());
            let mut carry = AigLit::false_lit();

            for j in 0..width {
                let out_idx = (i + j) as usize;
                if out_idx >= result_width as usize { break; }

                let a_bit = a.get(j as usize).copied().unwrap_or(AigLit::false_lit());
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
            let mut idx = (i + width) as usize;
            while idx < result_width as usize {
                let sum = self.aig.add_xor(result[idx], carry);
                carry = self.aig.add_and(result[idx], carry);
                result[idx] = sum;
                if carry == AigLit::false_lit() { break; }
                idx += 1;
            }
        }
        result
    }

    fn negate_vector(&mut self, x: &[AigLit]) -> Vec<AigLit> {
        let mut result = Vec::with_capacity(x.len());
        let mut carry = AigLit::true_lit();
        for &bit in x {
            let inv = bit.invert();
            let sum = self.aig.add_xor(inv, carry);
            carry = self.aig.add_and(inv, carry);
            result.push(sum);
        }
        result
    }

    fn mux_vectors(&mut self, sel: AigLit, a: &[AigLit], b: &[AigLit]) -> Vec<AigLit> {
        let width = a.len().max(b.len());
        (0..width)
            .map(|i| {
                let a_bit = a.get(i).copied().unwrap_or(AigLit::false_lit());
                let b_bit = b.get(i).copied().unwrap_or(AigLit::false_lit());
                self.aig.add_mux(sel, a_bit, b_bit)
            })
            .collect()
    }
}

#[derive(Clone, Copy)]
enum ShiftDir {
    Left,
    Right,
}
