//! K-Feasible Cut Enumeration
//!
//! This module implements cut enumeration for And-Inverter Graphs (AIGs).
//! A cut of a node is a set of nodes (leaves) such that every path from
//! any primary input to the node passes through at least one leaf.
//!
//! # Algorithm
//!
//! We use the standard bottom-up cut enumeration algorithm:
//! 1. For each input, the trivial cut is {input}
//! 2. For each AND node, cuts are computed by merging cuts of its fanins
//! 3. We keep only K-feasible cuts (size <= K) and limit the number of cuts per node
//!
//! # References
//!
//! - Cong, J., & Ding, Y. (1994). FlowMap: An optimal technology mapping algorithm for delay optimization in lookup-table based FPGA designs.
//! - Mishchenko, A., Chatterjee, S., & Brayton, R. (2006). DAG-aware AIG rewriting: A fresh look at combinational logic synthesis.

use crate::synth::{Aig, AigLit, AigNode, AigNodeId};
use std::collections::HashMap;

/// Maximum cut size (K in K-feasible cuts)
pub const DEFAULT_CUT_SIZE: usize = 4;

/// Maximum number of cuts to keep per node
pub const DEFAULT_MAX_CUTS: usize = 8;

/// A cut of a node in the AIG
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Cut {
    /// The leaves of the cut (sorted for canonical form)
    pub leaves: Vec<AigNodeId>,
    /// Truth table for cuts with <= 6 inputs (2^6 = 64 bits)
    pub truth_table: u64,
}

impl Cut {
    /// Create a new cut with the given leaves
    pub fn new(mut leaves: Vec<AigNodeId>) -> Self {
        leaves.sort();
        leaves.dedup();
        Self {
            leaves,
            truth_table: 0,
        }
    }

    /// Create a trivial cut containing just the node itself
    pub fn trivial(node: AigNodeId) -> Self {
        Self {
            leaves: vec![node],
            truth_table: 0,
        }
    }

    /// Get the size of this cut
    pub fn size(&self) -> usize {
        self.leaves.len()
    }

    /// Check if this cut is K-feasible
    pub fn is_k_feasible(&self, k: usize) -> bool {
        self.leaves.len() <= k
    }

    /// Check if this cut dominates another (is a subset)
    pub fn dominates(&self, other: &Cut) -> bool {
        if self.leaves.len() > other.leaves.len() {
            return false;
        }
        self.leaves.iter().all(|l| other.leaves.contains(l))
    }

    /// Merge two cuts by taking the union of their leaves
    pub fn merge(&self, other: &Cut) -> Cut {
        let mut leaves = self.leaves.clone();
        for leaf in &other.leaves {
            if !leaves.contains(leaf) {
                leaves.push(*leaf);
            }
        }
        leaves.sort();
        Cut {
            leaves,
            truth_table: 0,
        }
    }

    /// Check if this cut contains a specific node
    pub fn contains(&self, node: AigNodeId) -> bool {
        self.leaves.binary_search(&node).is_ok()
    }
}

/// Parameters for cut enumeration
#[derive(Debug, Clone)]
pub struct CutParams {
    /// Maximum cut size (K)
    pub k: usize,
    /// Maximum cuts per node
    pub max_cuts: usize,
    /// Whether to compute truth tables
    pub compute_truth_tables: bool,
}

impl Default for CutParams {
    fn default() -> Self {
        Self {
            k: DEFAULT_CUT_SIZE,
            max_cuts: DEFAULT_MAX_CUTS,
            compute_truth_tables: true,
        }
    }
}

/// Cut set for a single node
#[derive(Debug, Clone, Default)]
pub struct CutSet {
    /// The cuts for this node
    pub cuts: Vec<Cut>,
}

impl CutSet {
    /// Create a new empty cut set
    pub fn new() -> Self {
        Self { cuts: Vec::new() }
    }

    /// Create a cut set with just the trivial cut
    pub fn trivial(node: AigNodeId) -> Self {
        Self {
            cuts: vec![Cut::trivial(node)],
        }
    }

    /// Add a cut to this set, maintaining dominance filtering
    pub fn add(&mut self, cut: Cut, max_cuts: usize) {
        // Check if this cut is dominated by an existing cut
        for existing in &self.cuts {
            if existing.dominates(&cut) {
                return;
            }
        }

        // Remove cuts that are dominated by the new cut
        self.cuts.retain(|existing| !cut.dominates(existing));

        // Add the new cut if we have room
        if self.cuts.len() < max_cuts {
            self.cuts.push(cut);
        } else {
            // Replace the largest cut if the new one is smaller
            if let Some(largest_idx) = self
                .cuts
                .iter()
                .enumerate()
                .max_by_key(|(_, c)| c.size())
                .map(|(i, _)| i)
            {
                if cut.size() < self.cuts[largest_idx].size() {
                    self.cuts[largest_idx] = cut;
                }
            }
        }
    }

    /// Get the best cut (smallest)
    pub fn best(&self) -> Option<&Cut> {
        self.cuts.iter().min_by_key(|c| c.size())
    }

    /// Get the number of cuts
    pub fn len(&self) -> usize {
        self.cuts.len()
    }

    /// Check if the cut set is empty
    pub fn is_empty(&self) -> bool {
        self.cuts.is_empty()
    }
}

/// Result of cut enumeration for an entire AIG
pub struct CutEnumeration {
    /// Cuts for each node
    pub node_cuts: HashMap<AigNodeId, CutSet>,
    /// Parameters used
    pub params: CutParams,
}

impl CutEnumeration {
    /// Enumerate cuts for the given AIG
    pub fn enumerate(aig: &Aig, params: CutParams) -> Self {
        let mut node_cuts: HashMap<AigNodeId, CutSet> = HashMap::new();

        // Process nodes in topological order
        for (id, node) in aig.iter_nodes() {
            let cut_set = match node {
                AigNode::Const => {
                    // Constant has a trivial cut
                    CutSet::trivial(id)
                }
                AigNode::Input { .. } => {
                    // Inputs have trivial cuts
                    CutSet::trivial(id)
                }
                AigNode::Latch { .. } => {
                    // Latches are treated like inputs for combinational cuts
                    CutSet::trivial(id)
                }
                AigNode::Barrier { .. } => {
                    // Barriers are power domain boundaries - treated like inputs
                    // to prevent cross-domain optimization
                    CutSet::trivial(id)
                }
                AigNode::And { left, right } => {
                    // Compute cuts by merging fanin cuts
                    let left_cuts = node_cuts.get(&left.node).cloned().unwrap_or_default();
                    let right_cuts = node_cuts.get(&right.node).cloned().unwrap_or_default();

                    let mut new_cuts = CutSet::new();

                    // Always include the trivial cut
                    new_cuts.add(Cut::trivial(id), params.max_cuts);

                    // Merge all pairs of cuts from left and right
                    for left_cut in &left_cuts.cuts {
                        for right_cut in &right_cuts.cuts {
                            let merged = left_cut.merge(right_cut);
                            if merged.is_k_feasible(params.k) {
                                let mut cut = merged;
                                if params.compute_truth_tables {
                                    cut.truth_table = compute_and_truth_table(
                                        aig,
                                        id,
                                        &cut.leaves,
                                        left,
                                        right,
                                        &node_cuts,
                                    );
                                }
                                new_cuts.add(cut, params.max_cuts);
                            }
                        }
                    }

                    new_cuts
                }
            };

            node_cuts.insert(id, cut_set);
        }

        Self { node_cuts, params }
    }

    /// Get the cuts for a specific node
    pub fn get_cuts(&self, node: AigNodeId) -> Option<&CutSet> {
        self.node_cuts.get(&node)
    }

    /// Get the best cut for a node
    pub fn best_cut(&self, node: AigNodeId) -> Option<&Cut> {
        self.node_cuts.get(&node).and_then(|cs| cs.best())
    }

    /// Get total number of cuts across all nodes
    pub fn total_cuts(&self) -> usize {
        self.node_cuts.values().map(|cs| cs.len()).sum()
    }

    /// Get average cuts per node
    pub fn avg_cuts_per_node(&self) -> f64 {
        if self.node_cuts.is_empty() {
            0.0
        } else {
            self.total_cuts() as f64 / self.node_cuts.len() as f64
        }
    }
}

/// Compute truth table for an AND node given its cut leaves
fn compute_and_truth_table(
    aig: &Aig,
    node: AigNodeId,
    leaves: &[AigNodeId],
    left: &AigLit,
    right: &AigLit,
    node_cuts: &HashMap<AigNodeId, CutSet>,
) -> u64 {
    if leaves.len() > 6 {
        return 0; // Can't fit in 64 bits
    }

    let num_inputs = leaves.len();
    let num_rows = 1usize << num_inputs;
    let mut truth_table = 0u64;

    // Create a mapping from leaf to input index
    let leaf_to_idx: HashMap<AigNodeId, usize> =
        leaves.iter().enumerate().map(|(i, &n)| (n, i)).collect();

    // Evaluate for each input combination
    for row in 0..num_rows {
        let result = evaluate_node(aig, node, left, right, row, &leaf_to_idx, node_cuts);
        if result {
            truth_table |= 1u64 << row;
        }
    }

    truth_table
}

/// Evaluate a node for a specific input assignment
fn evaluate_node(
    aig: &Aig,
    node: AigNodeId,
    left: &AigLit,
    right: &AigLit,
    input_assignment: usize,
    leaf_to_idx: &HashMap<AigNodeId, usize>,
    node_cuts: &HashMap<AigNodeId, CutSet>,
) -> bool {
    let left_val = evaluate_lit(aig, left, input_assignment, leaf_to_idx, node_cuts);
    let right_val = evaluate_lit(aig, right, input_assignment, leaf_to_idx, node_cuts);
    left_val && right_val
}

/// Evaluate a literal for a specific input assignment
fn evaluate_lit(
    aig: &Aig,
    lit: &AigLit,
    input_assignment: usize,
    leaf_to_idx: &HashMap<AigNodeId, usize>,
    node_cuts: &HashMap<AigNodeId, CutSet>,
) -> bool {
    let base_val = if let Some(&idx) = leaf_to_idx.get(&lit.node) {
        // This is a leaf - get value from input assignment
        (input_assignment >> idx) & 1 == 1
    } else if lit.node == AigNodeId::FALSE {
        false
    } else {
        // Need to recursively evaluate
        if let Some(node) = aig.get_node(lit.node) {
            match node {
                AigNode::Const => false,
                AigNode::Input { .. } | AigNode::Latch { .. } | AigNode::Barrier { .. } => {
                    // Should have been a leaf (barriers are optimization boundaries)
                    false
                }
                AigNode::And { left, right } => evaluate_node(
                    aig,
                    lit.node,
                    left,
                    right,
                    input_assignment,
                    leaf_to_idx,
                    node_cuts,
                ),
            }
        } else {
            false
        }
    };

    if lit.inverted {
        !base_val
    } else {
        base_val
    }
}

/// Standard truth tables for common 4-input functions
pub mod standard_truth_tables {
    /// Truth table for constant 0
    pub const CONST_0: u64 = 0x0000;
    /// Truth table for constant 1
    pub const CONST_1: u64 = 0xFFFF;
    /// Truth table for input A (variable 0)
    pub const VAR_A: u64 = 0xAAAA;
    /// Truth table for input B (variable 1)
    pub const VAR_B: u64 = 0xCCCC;
    /// Truth table for input C (variable 2)
    pub const VAR_C: u64 = 0xF0F0;
    /// Truth table for input D (variable 3)
    pub const VAR_D: u64 = 0xFF00;

    /// Get the truth table for a single variable
    pub fn var(idx: usize) -> u64 {
        match idx {
            0 => VAR_A,
            1 => VAR_B,
            2 => VAR_C,
            3 => VAR_D,
            _ => 0,
        }
    }

    /// Compute NOT of a truth table
    pub fn not(tt: u64) -> u64 {
        !tt & 0xFFFF
    }

    /// Compute AND of two truth tables
    pub fn and(a: u64, b: u64) -> u64 {
        a & b
    }

    /// Compute OR of two truth tables
    pub fn or(a: u64, b: u64) -> u64 {
        a | b
    }

    /// Compute XOR of two truth tables
    pub fn xor(a: u64, b: u64) -> u64 {
        a ^ b
    }

    /// Compute MUX of three truth tables: sel ? a : b
    pub fn mux(sel: u64, a: u64, b: u64) -> u64 {
        (sel & a) | (!sel & b)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_cut_creation() {
        let cut = Cut::new(vec![AigNodeId(3), AigNodeId(1), AigNodeId(2)]);
        // Should be sorted
        assert_eq!(cut.leaves, vec![AigNodeId(1), AigNodeId(2), AigNodeId(3)]);
    }

    #[test]
    fn test_cut_trivial() {
        let cut = Cut::trivial(AigNodeId(5));
        assert_eq!(cut.leaves, vec![AigNodeId(5)]);
        assert_eq!(cut.size(), 1);
    }

    #[test]
    fn test_cut_merge() {
        let cut1 = Cut::new(vec![AigNodeId(1), AigNodeId(2)]);
        let cut2 = Cut::new(vec![AigNodeId(2), AigNodeId(3)]);
        let merged = cut1.merge(&cut2);
        assert_eq!(
            merged.leaves,
            vec![AigNodeId(1), AigNodeId(2), AigNodeId(3)]
        );
    }

    #[test]
    fn test_cut_dominates() {
        let cut1 = Cut::new(vec![AigNodeId(1), AigNodeId(2)]);
        let cut2 = Cut::new(vec![AigNodeId(1), AigNodeId(2), AigNodeId(3)]);
        assert!(cut1.dominates(&cut2));
        assert!(!cut2.dominates(&cut1));
    }

    #[test]
    fn test_cut_k_feasible() {
        let cut = Cut::new(vec![AigNodeId(1), AigNodeId(2), AigNodeId(3)]);
        assert!(cut.is_k_feasible(4));
        assert!(cut.is_k_feasible(3));
        assert!(!cut.is_k_feasible(2));
    }

    #[test]
    fn test_cut_set_add() {
        let mut cs = CutSet::new();
        cs.add(Cut::new(vec![AigNodeId(1), AigNodeId(2)]), 10);
        cs.add(Cut::new(vec![AigNodeId(1), AigNodeId(2), AigNodeId(3)]), 10);

        // The larger cut should be filtered out as dominated
        assert_eq!(cs.len(), 1);
    }

    #[test]
    fn test_cut_enumeration_simple() {
        let mut aig = Aig::new("test".to_string());
        let a = aig.add_input("a".to_string(), None);
        let b = aig.add_input("b".to_string(), None);
        let ab = aig.add_and(AigLit::new(a), AigLit::new(b));
        aig.add_output("y".to_string(), ab);

        let enum_result = CutEnumeration::enumerate(&aig, CutParams::default());

        // Input nodes should have trivial cuts
        assert_eq!(enum_result.get_cuts(a).unwrap().len(), 1);
        assert_eq!(enum_result.get_cuts(b).unwrap().len(), 1);

        // AND node should have cuts
        let and_cuts = enum_result.get_cuts(ab.node).unwrap();
        assert!(!and_cuts.is_empty());

        // Should have cut {a, b}
        let has_ab_cut = and_cuts
            .cuts
            .iter()
            .any(|cut| cut.leaves.len() == 2 && cut.leaves.contains(&a) && cut.leaves.contains(&b));
        assert!(has_ab_cut);
    }

    #[test]
    fn test_cut_enumeration_chain() {
        let mut aig = Aig::new("test".to_string());
        let a = aig.add_input("a".to_string(), None);
        let b = aig.add_input("b".to_string(), None);
        let c = aig.add_input("c".to_string(), None);

        let ab = aig.add_and(AigLit::new(a), AigLit::new(b));
        let abc = aig.add_and(ab, AigLit::new(c));
        aig.add_output("y".to_string(), abc);

        let enum_result = CutEnumeration::enumerate(&aig, CutParams::default());

        // The final node should have cuts including {a, b, c}
        let final_cuts = enum_result.get_cuts(abc.node).unwrap();
        let has_abc_cut = final_cuts.cuts.iter().any(|cut| {
            cut.leaves.len() == 3
                && cut.leaves.contains(&a)
                && cut.leaves.contains(&b)
                && cut.leaves.contains(&c)
        });
        assert!(has_abc_cut);
    }

    #[test]
    fn test_truth_table_var() {
        use standard_truth_tables::*;
        assert_eq!(var(0), VAR_A);
        assert_eq!(var(1), VAR_B);
        assert_eq!(not(VAR_A) & 0xFFFF, 0x5555);
    }

    #[test]
    fn test_truth_table_and() {
        use standard_truth_tables::*;
        // A AND B
        let result = and(VAR_A, VAR_B);
        assert_eq!(result, 0x8888);
    }
}
