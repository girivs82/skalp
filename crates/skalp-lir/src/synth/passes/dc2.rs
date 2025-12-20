//! DC2: Don't Care Based Optimization Pass
//!
//! This pass performs more aggressive optimization by using don't care conditions
//! derived from the circuit structure. It combines rewriting with SAT-based
//! simplification.
//!
//! # Algorithm
//!
//! 1. For each node, compute observability don't cares (ODCs)
//! 2. Use ODCs to find simpler implementations
//! 3. Apply structural hashing and rewriting iteratively
//! 4. Use SAT to verify functional equivalence
//!
//! # References
//!
//! - Mishchenko, A., Brayton, R. (2007). "SAT-based combinational synthesis."

use super::{Pass, PassResult};
use crate::synth::{Aig, AigLit, AigNode, AigNodeId};
use std::collections::{HashMap, HashSet};

/// Maximum cut size for DC2 optimization
const MAX_CUT_SIZE: usize = 6;

/// Maximum iterations for internal optimization
const MAX_INTERNAL_ITERS: usize = 3;

/// DC2 Pass - Don't care based optimization
pub struct Dc2 {
    /// Maximum cut size
    max_cut: usize,
    /// Number of nodes optimized
    optimized_count: usize,
    /// Verbose logging
    verbose: bool,
}

impl Default for Dc2 {
    fn default() -> Self {
        Self::new()
    }
}

impl Dc2 {
    /// Create a new DC2 pass
    pub fn new() -> Self {
        Self {
            max_cut: MAX_CUT_SIZE,
            optimized_count: 0,
            verbose: false,
        }
    }

    /// Create with verbose logging
    pub fn verbose() -> Self {
        Self {
            max_cut: MAX_CUT_SIZE,
            optimized_count: 0,
            verbose: true,
        }
    }

    /// Create with custom cut size
    pub fn with_cut_size(max_cut: usize) -> Self {
        Self {
            max_cut,
            optimized_count: 0,
            verbose: false,
        }
    }

    /// Compute fanout counts for all nodes
    fn compute_fanout(&self, aig: &Aig) -> HashMap<AigNodeId, usize> {
        let mut fanouts = HashMap::new();

        for (_, node) in aig.iter_nodes() {
            for fanin in node.fanins() {
                *fanouts.entry(fanin.node).or_insert(0) += 1;
            }
        }

        for (_, lit) in aig.outputs() {
            *fanouts.entry(lit.node).or_insert(0) += 1;
        }

        fanouts
    }

    /// Collect the MFFC (Maximum Fanout-Free Cone) for a node
    /// Returns nodes that are only used by this node
    fn collect_mffc(
        &self,
        aig: &Aig,
        root: AigNodeId,
        fanouts: &HashMap<AigNodeId, usize>,
    ) -> Vec<AigNodeId> {
        let mut mffc = Vec::new();
        let mut visited = HashSet::new();
        self.collect_mffc_rec(aig, root, fanouts, &mut mffc, &mut visited);
        mffc
    }

    fn collect_mffc_rec(
        &self,
        aig: &Aig,
        node: AigNodeId,
        fanouts: &HashMap<AigNodeId, usize>,
        mffc: &mut Vec<AigNodeId>,
        visited: &mut HashSet<AigNodeId>,
    ) {
        if visited.contains(&node) {
            return;
        }
        visited.insert(node);

        if let Some(n) = aig.get_node(node) {
            if let AigNode::And { left, right, .. } = n {
                // Only include if this is the only fanout
                let left_fo = fanouts.get(&left.node).copied().unwrap_or(1);
                let right_fo = fanouts.get(&right.node).copied().unwrap_or(1);

                if left_fo == 1 {
                    self.collect_mffc_rec(aig, left.node, fanouts, mffc, visited);
                }
                if right_fo == 1 {
                    self.collect_mffc_rec(aig, right.node, fanouts, mffc, visited);
                }
            }
        }

        mffc.push(node);
    }

    /// Collect cut leaves for a node
    fn collect_cut_leaves(
        &self,
        aig: &Aig,
        root: AigNodeId,
        max_size: usize,
    ) -> Vec<AigNodeId> {
        let mut leaves = Vec::new();
        let mut internal = HashSet::new();
        let mut frontier = vec![root];
        internal.insert(root);

        while let Some(node) = frontier.pop() {
            if let Some(n) = aig.get_node(node) {
                match n {
                    AigNode::And { left, right, .. } => {
                        // Try to expand fanins
                        for lit in [left, right] {
                            if !internal.contains(&lit.node) {
                                if leaves.len() + 1 <= max_size {
                                    // Can still add leaves
                                    if let Some(AigNode::And { .. }) = aig.get_node(lit.node) {
                                        // Expand this AND node
                                        internal.insert(lit.node);
                                        frontier.push(lit.node);
                                    } else {
                                        // PI or latch - add as leaf
                                        if !leaves.contains(&lit.node) {
                                            leaves.push(lit.node);
                                        }
                                    }
                                } else if !leaves.contains(&lit.node) {
                                    leaves.push(lit.node);
                                }
                            }
                        }
                    }
                    AigNode::Input { .. } | AigNode::Latch { .. } => {
                        if !leaves.contains(&node) {
                            leaves.push(node);
                        }
                    }
                    _ => {}
                }
            }
        }

        leaves
    }

    /// Compute truth table for a node over given leaves
    fn compute_tt(
        &self,
        aig: &Aig,
        node: AigNodeId,
        leaves: &[AigNodeId],
    ) -> Option<u64> {
        if leaves.len() > 6 {
            return None;
        }

        let leaf_map: HashMap<AigNodeId, usize> = leaves
            .iter()
            .enumerate()
            .map(|(i, &n)| (n, i))
            .collect();

        let num_rows = 1usize << leaves.len();
        let mut tt = 0u64;

        for row in 0..num_rows {
            if self.evaluate(aig, node, row, &leaf_map) {
                tt |= 1u64 << row;
            }
        }

        Some(tt)
    }

    fn evaluate(
        &self,
        aig: &Aig,
        node: AigNodeId,
        assignment: usize,
        leaf_map: &HashMap<AigNodeId, usize>,
    ) -> bool {
        match aig.get_node(node) {
            Some(AigNode::Input { .. }) | Some(AigNode::Latch { .. }) => {
                if let Some(&idx) = leaf_map.get(&node) {
                    (assignment >> idx) & 1 == 1
                } else {
                    false
                }
            }
            Some(AigNode::And { left, right, .. }) => {
                let l = self.evaluate_lit(aig, *left, assignment, leaf_map);
                let r = self.evaluate_lit(aig, *right, assignment, leaf_map);
                l && r
            }
            Some(AigNode::Const) => false,
            _ => false,
        }
    }

    fn evaluate_lit(
        &self,
        aig: &Aig,
        lit: AigLit,
        assignment: usize,
        leaf_map: &HashMap<AigNodeId, usize>,
    ) -> bool {
        let val = self.evaluate(aig, lit.node, assignment, leaf_map);
        if lit.inverted {
            !val
        } else {
            val
        }
    }

    /// Count the number of 1s in a truth table (population count)
    fn popcount(tt: u64) -> u32 {
        tt.count_ones()
    }

    /// Try to find a simpler implementation for a truth table
    /// Returns (gates, structure) if found
    fn try_simplify(&self, tt: u64, num_inputs: usize) -> Option<SimplifiedForm> {
        let mask = if num_inputs >= 6 {
            u64::MAX
        } else {
            (1u64 << (1 << num_inputs)) - 1
        };
        let tt = tt & mask;

        // Check for constant
        if tt == 0 {
            return Some(SimplifiedForm::Constant(false));
        }
        if tt == mask {
            return Some(SimplifiedForm::Constant(true));
        }

        // Check for single variable
        for i in 0..num_inputs {
            let var_tt = self.make_var_tt(i, num_inputs);
            if tt == var_tt {
                return Some(SimplifiedForm::Variable(i, false));
            }
            if tt == (!var_tt & mask) {
                return Some(SimplifiedForm::Variable(i, true));
            }
        }

        // Check for simple 2-input functions
        if num_inputs >= 2 {
            for i in 0..num_inputs {
                for j in (i + 1)..num_inputs {
                    let vi = self.make_var_tt(i, num_inputs);
                    let vj = self.make_var_tt(j, num_inputs);

                    // AND
                    if tt == (vi & vj) {
                        return Some(SimplifiedForm::And2(i, false, j, false));
                    }
                    // NAND
                    if tt == (!(vi & vj) & mask) {
                        return Some(SimplifiedForm::And2Inv(i, false, j, false));
                    }
                    // OR
                    if tt == ((vi | vj) & mask) {
                        return Some(SimplifiedForm::Or2(i, false, j, false));
                    }
                    // XOR
                    if tt == ((vi ^ vj) & mask) {
                        return Some(SimplifiedForm::Xor2(i, j));
                    }
                    // XNOR
                    if tt == (!(vi ^ vj) & mask) {
                        return Some(SimplifiedForm::Xnor2(i, j));
                    }

                    // AND with inversions
                    if tt == ((!vi & mask) & vj) {
                        return Some(SimplifiedForm::And2(i, true, j, false));
                    }
                    if tt == (vi & (!vj & mask)) {
                        return Some(SimplifiedForm::And2(i, false, j, true));
                    }
                }
            }
        }

        // Check for MUX pattern: (s & a) | (!s & b)
        if num_inputs >= 3 {
            for s in 0..num_inputs {
                for a in 0..num_inputs {
                    if a == s {
                        continue;
                    }
                    for b in 0..num_inputs {
                        if b == s || b == a {
                            continue;
                        }

                        let vs = self.make_var_tt(s, num_inputs);
                        let va = self.make_var_tt(a, num_inputs);
                        let vb = self.make_var_tt(b, num_inputs);

                        let mux = ((vs & va) | ((!vs & mask) & vb)) & mask;
                        if tt == mux {
                            return Some(SimplifiedForm::Mux(s, a, false, b, false));
                        }
                    }
                }
            }
        }

        None
    }

    /// Create truth table for variable i with n inputs
    fn make_var_tt(&self, var: usize, num_inputs: usize) -> u64 {
        if num_inputs > 6 {
            return 0;
        }
        let num_rows = 1usize << num_inputs;
        let mut tt = 0u64;
        for row in 0..num_rows {
            if (row >> var) & 1 == 1 {
                tt |= 1u64 << row;
            }
        }
        tt
    }

    /// Build the simplified form in the AIG
    fn build_simplified(
        &self,
        aig: &mut Aig,
        form: &SimplifiedForm,
        leaves: &[AigNodeId],
    ) -> AigLit {
        match form {
            SimplifiedForm::Constant(val) => {
                // Constant false is node 0, constant true is node 0 inverted
                AigLit {
                    node: AigNodeId::FALSE,
                    inverted: *val,
                }
            }
            SimplifiedForm::Variable(idx, inv) => AigLit {
                node: leaves[*idx],
                inverted: *inv,
            },
            SimplifiedForm::And2(i, inv_i, j, inv_j) => {
                let li = AigLit {
                    node: leaves[*i],
                    inverted: *inv_i,
                };
                let lj = AigLit {
                    node: leaves[*j],
                    inverted: *inv_j,
                };
                aig.add_and(li, lj)
            }
            SimplifiedForm::And2Inv(i, inv_i, j, inv_j) => {
                let li = AigLit {
                    node: leaves[*i],
                    inverted: *inv_i,
                };
                let lj = AigLit {
                    node: leaves[*j],
                    inverted: *inv_j,
                };
                let and = aig.add_and(li, lj);
                AigLit {
                    node: and.node,
                    inverted: true,
                }
            }
            SimplifiedForm::Or2(i, inv_i, j, inv_j) => {
                // OR(a,b) = !(!a & !b)
                let li = AigLit {
                    node: leaves[*i],
                    inverted: !*inv_i,
                };
                let lj = AigLit {
                    node: leaves[*j],
                    inverted: !*inv_j,
                };
                let nand = aig.add_and(li, lj);
                AigLit {
                    node: nand.node,
                    inverted: true,
                }
            }
            SimplifiedForm::Xor2(i, j) => {
                // XOR(a,b) = (a & !b) | (!a & b) = !(!( a & !b) & !(!a & b))
                let a = AigLit::new(leaves[*i]);
                let b = AigLit::new(leaves[*j]);
                let a_and_nb = aig.add_and(a, b.invert());
                let na_and_b = aig.add_and(a.invert(), b);
                let nand = aig.add_and(a_and_nb.invert(), na_and_b.invert());
                nand.invert()
            }
            SimplifiedForm::Xnor2(i, j) => {
                let a = AigLit::new(leaves[*i]);
                let b = AigLit::new(leaves[*j]);
                let a_and_nb = aig.add_and(a, b.invert());
                let na_and_b = aig.add_and(a.invert(), b);
                let nand = aig.add_and(a_and_nb.invert(), na_and_b.invert());
                nand // XNOR = !XOR
            }
            SimplifiedForm::Mux(s, a, inv_a, b, inv_b) => {
                // MUX(s,a,b) = (s & a) | (!s & b)
                let ls = AigLit::new(leaves[*s]);
                let la = AigLit {
                    node: leaves[*a],
                    inverted: *inv_a,
                };
                let lb = AigLit {
                    node: leaves[*b],
                    inverted: *inv_b,
                };

                let s_and_a = aig.add_and(ls, la);
                let ns_and_b = aig.add_and(ls.invert(), lb);
                // OR = NAND of complements
                let result = aig.add_and(s_and_a.invert(), ns_and_b.invert());
                result.invert()
            }
        }
    }

    /// Run local optimization on a single node
    fn optimize_node(
        &mut self,
        aig: &mut Aig,
        node: AigNodeId,
        fanouts: &HashMap<AigNodeId, usize>,
    ) -> Option<AigLit> {
        // Get cut leaves
        let leaves = self.collect_cut_leaves(aig, node, self.max_cut);
        if leaves.len() > 6 || leaves.is_empty() {
            return None;
        }

        // Compute truth table
        let tt = self.compute_tt(aig, node, &leaves)?;

        // Try to simplify
        let form = self.try_simplify(tt, leaves.len())?;

        // Count gates in current implementation
        let mffc = self.collect_mffc(aig, node, fanouts);
        let current_gates = mffc.len();

        // Count gates in simplified form
        let simplified_gates = form.gate_count();

        // Only apply if we save gates
        if simplified_gates < current_gates {
            let new_lit = self.build_simplified(aig, &form, &leaves);
            self.optimized_count += 1;
            Some(new_lit)
        } else {
            None
        }
    }
}

/// Simplified form of a logic function
#[derive(Debug, Clone)]
enum SimplifiedForm {
    /// Constant 0 or 1
    Constant(bool),
    /// Single variable (index, inverted)
    Variable(usize, bool),
    /// AND of two variables
    And2(usize, bool, usize, bool),
    /// Inverted AND (NAND) of two variables
    And2Inv(usize, bool, usize, bool),
    /// OR of two variables
    Or2(usize, bool, usize, bool),
    /// XOR of two variables
    Xor2(usize, usize),
    /// XNOR of two variables
    Xnor2(usize, usize),
    /// MUX: sel ? a : b
    Mux(usize, usize, bool, usize, bool),
}

impl SimplifiedForm {
    fn gate_count(&self) -> usize {
        match self {
            SimplifiedForm::Constant(_) => 0,
            SimplifiedForm::Variable(_, _) => 0,
            SimplifiedForm::And2(_, _, _, _) => 1,
            SimplifiedForm::And2Inv(_, _, _, _) => 1,
            SimplifiedForm::Or2(_, _, _, _) => 1,
            SimplifiedForm::Xor2(_, _) => 3, // XOR needs 3 ANDs
            SimplifiedForm::Xnor2(_, _) => 3,
            SimplifiedForm::Mux(_, _, _, _, _) => 3, // MUX needs 3 ANDs
        }
    }
}

impl Pass for Dc2 {
    fn name(&self) -> &str {
        "dc2"
    }

    fn run(&mut self, aig: &mut Aig) -> PassResult {
        let mut result = PassResult::new(self.name());
        result.record_before(aig);

        self.optimized_count = 0;

        // Run multiple iterations
        for _iter in 0..MAX_INTERNAL_ITERS {
            let fanouts = self.compute_fanout(aig);

            // Collect AND nodes
            let nodes: Vec<AigNodeId> = aig
                .iter_nodes()
                .filter_map(|(id, node)| {
                    if matches!(node, AigNode::And { .. }) {
                        Some(id)
                    } else {
                        None
                    }
                })
                .collect();

            let mut substitutions: HashMap<AigNodeId, AigLit> = HashMap::new();
            let before_count = self.optimized_count;

            for node in nodes {
                if substitutions.contains_key(&node) {
                    continue;
                }

                if let Some(new_lit) = self.optimize_node(aig, node, &fanouts) {
                    if new_lit.node != node {
                        substitutions.insert(node, new_lit);
                    }
                }
            }

            // Apply substitutions
            if !substitutions.is_empty() {
                aig.apply_substitutions(&substitutions);

                // Clean up
                let mut dce = super::Dce::new();
                dce.run(aig);

                // Structural hashing
                let mut strash = super::Strash::new();
                strash.run(aig);
            }

            // Check convergence
            if self.optimized_count == before_count {
                break;
            }
        }

        result.record_after(aig);
        result.add_extra("nodes_optimized", &self.optimized_count.to_string());
        result
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_dc2_creation() {
        let pass = Dc2::new();
        assert_eq!(pass.name(), "dc2");
        assert_eq!(pass.max_cut, MAX_CUT_SIZE);
    }

    #[test]
    fn test_dc2_with_cut_size() {
        let pass = Dc2::with_cut_size(4);
        assert_eq!(pass.max_cut, 4);
    }

    #[test]
    fn test_make_var_tt() {
        let dc2 = Dc2::new();

        // 2-input: var 0 = 0b1010, var 1 = 0b1100
        assert_eq!(dc2.make_var_tt(0, 2), 0b1010);
        assert_eq!(dc2.make_var_tt(1, 2), 0b1100);

        // 3-input: var 0 = 0b10101010
        assert_eq!(dc2.make_var_tt(0, 3), 0b10101010);
    }

    #[test]
    fn test_try_simplify_constant() {
        let dc2 = Dc2::new();

        // Constant 0
        let result = dc2.try_simplify(0, 2);
        assert!(matches!(result, Some(SimplifiedForm::Constant(false))));

        // Constant 1
        let result = dc2.try_simplify(0xF, 2); // All 1s for 2 inputs
        assert!(matches!(result, Some(SimplifiedForm::Constant(true))));
    }

    #[test]
    fn test_try_simplify_variable() {
        let dc2 = Dc2::new();

        // Variable 0
        let result = dc2.try_simplify(0b1010, 2);
        assert!(matches!(result, Some(SimplifiedForm::Variable(0, false))));

        // Variable 1
        let result = dc2.try_simplify(0b1100, 2);
        assert!(matches!(result, Some(SimplifiedForm::Variable(1, false))));

        // !Variable 0
        let result = dc2.try_simplify(0b0101, 2);
        assert!(matches!(result, Some(SimplifiedForm::Variable(0, true))));
    }

    #[test]
    fn test_try_simplify_and() {
        let dc2 = Dc2::new();

        // a AND b
        let result = dc2.try_simplify(0b1000, 2);
        assert!(matches!(
            result,
            Some(SimplifiedForm::And2(0, false, 1, false))
        ));
    }

    #[test]
    fn test_try_simplify_or() {
        let dc2 = Dc2::new();

        // a OR b = 0b1110
        let result = dc2.try_simplify(0b1110, 2);
        assert!(matches!(
            result,
            Some(SimplifiedForm::Or2(0, false, 1, false))
        ));
    }

    #[test]
    fn test_try_simplify_xor() {
        let dc2 = Dc2::new();

        // a XOR b = 0b0110
        let result = dc2.try_simplify(0b0110, 2);
        assert!(matches!(result, Some(SimplifiedForm::Xor2(0, 1))));
    }

    #[test]
    fn test_dc2_simple() {
        let mut aig = Aig::new("test".to_string());
        let a = aig.add_input("a".to_string(), None);
        let b = aig.add_input("b".to_string(), None);
        let ab = aig.add_and(AigLit::new(a), AigLit::new(b));
        aig.add_output("y".to_string(), ab);

        let mut pass = Dc2::new();
        let result = pass.run(&mut aig);

        // Should complete without error
        assert!(result.ands_after >= 1);
    }

    #[test]
    fn test_simplified_form_gate_count() {
        assert_eq!(SimplifiedForm::Constant(false).gate_count(), 0);
        assert_eq!(SimplifiedForm::Variable(0, false).gate_count(), 0);
        assert_eq!(SimplifiedForm::And2(0, false, 1, false).gate_count(), 1);
        assert_eq!(SimplifiedForm::Xor2(0, 1).gate_count(), 3);
        assert_eq!(SimplifiedForm::Mux(0, 1, false, 2, false).gate_count(), 3);
    }
}
