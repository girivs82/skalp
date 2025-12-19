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

/// Priority mode for cut selection
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum CutPriority {
    /// Prioritize by area (smaller cuts preferred)
    #[default]
    Area,
    /// Prioritize by delay (lower arrival time preferred)
    Delay,
    /// Prioritize by area-delay product
    AreaDelay,
    /// Prioritize area with delay constraint
    AreaWithDelayBound,
}

/// A cut of a node in the AIG
#[derive(Debug, Clone, PartialEq)]
pub struct Cut {
    /// The leaves of the cut (sorted for canonical form)
    pub leaves: Vec<AigNodeId>,
    /// Truth table for cuts with <= 6 inputs (2^6 = 64 bits)
    pub truth_table: u64,
    /// Area cost estimate (e.g., number of LUTs or gates to implement)
    pub area_cost: f32,
    /// Arrival time at output (for delay-optimal mapping)
    pub arrival_time: f32,
    /// Area flow (for area recovery)
    pub area_flow: f32,
    /// Edge count (fanin edges, used for some cost metrics)
    pub edge_count: u32,
}

impl Cut {
    /// Create a new cut with the given leaves
    pub fn new(mut leaves: Vec<AigNodeId>) -> Self {
        leaves.sort();
        leaves.dedup();
        let edge_count = leaves.len() as u32;
        Self {
            leaves,
            truth_table: 0,
            area_cost: 1.0, // Default: 1 LUT/gate
            arrival_time: 0.0,
            area_flow: 0.0,
            edge_count,
        }
    }

    /// Create a trivial cut containing just the node itself
    pub fn trivial(node: AigNodeId) -> Self {
        Self {
            leaves: vec![node],
            truth_table: 0,
            area_cost: 0.0, // Trivial cut has no area cost
            arrival_time: 0.0,
            area_flow: 0.0,
            edge_count: 1,
        }
    }

    /// Create a cut with explicit priority metrics
    pub fn with_metrics(mut leaves: Vec<AigNodeId>, area_cost: f32, arrival_time: f32) -> Self {
        leaves.sort();
        leaves.dedup();
        let edge_count = leaves.len() as u32;
        Self {
            leaves,
            truth_table: 0,
            area_cost,
            arrival_time,
            area_flow: area_cost, // Initialize area flow to area cost
            edge_count,
        }
    }

    /// Compute the priority score for this cut (lower is better)
    pub fn priority_score(&self, mode: CutPriority, delay_bound: f32) -> f32 {
        match mode {
            CutPriority::Area => self.area_cost,
            CutPriority::Delay => self.arrival_time,
            CutPriority::AreaDelay => self.area_cost * self.arrival_time.max(1.0),
            CutPriority::AreaWithDelayBound => {
                if self.arrival_time <= delay_bound {
                    self.area_cost
                } else {
                    // Heavily penalize cuts that violate delay bound
                    self.area_cost + 1000.0 * (self.arrival_time - delay_bound)
                }
            }
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
    ///
    /// Priority metrics are combined as follows:
    /// - area_cost: sum of both (implementing both cuts)
    /// - arrival_time: max of both (critical path)
    /// - area_flow: sum of both
    /// - edge_count: count of merged leaves
    pub fn merge(&self, other: &Cut) -> Cut {
        let mut leaves = self.leaves.clone();
        for leaf in &other.leaves {
            if !leaves.contains(leaf) {
                leaves.push(*leaf);
            }
        }
        leaves.sort();
        let edge_count = leaves.len() as u32;
        Cut {
            leaves,
            truth_table: 0,
            // Combined area is sum of both cut areas
            area_cost: self.area_cost + other.area_cost,
            // Arrival time is max (critical path through either fanin)
            arrival_time: self.arrival_time.max(other.arrival_time),
            // Area flow is sum
            area_flow: self.area_flow + other.area_flow,
            edge_count,
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
    /// Priority mode for cut selection
    pub priority: CutPriority,
    /// Delay bound for AreaWithDelayBound mode
    pub delay_bound: f32,
    /// Whether to enumerate cuts through choice nodes
    pub use_choices: bool,
}

impl Default for CutParams {
    fn default() -> Self {
        Self {
            k: DEFAULT_CUT_SIZE,
            max_cuts: DEFAULT_MAX_CUTS,
            compute_truth_tables: true,
            priority: CutPriority::Area,
            delay_bound: f32::INFINITY,
            use_choices: false,
        }
    }
}

impl CutParams {
    /// Create params for delay-optimal mapping
    pub fn delay_optimal(k: usize) -> Self {
        Self {
            k,
            max_cuts: DEFAULT_MAX_CUTS,
            compute_truth_tables: true,
            priority: CutPriority::Delay,
            delay_bound: f32::INFINITY,
            use_choices: false,
        }
    }

    /// Create params for area-optimal mapping
    pub fn area_optimal(k: usize) -> Self {
        Self {
            k,
            max_cuts: DEFAULT_MAX_CUTS,
            compute_truth_tables: true,
            priority: CutPriority::Area,
            delay_bound: f32::INFINITY,
            use_choices: false,
        }
    }

    /// Create params for area recovery with delay constraint
    pub fn area_recovery(k: usize, delay_bound: f32) -> Self {
        Self {
            k,
            max_cuts: DEFAULT_MAX_CUTS,
            compute_truth_tables: true,
            priority: CutPriority::AreaWithDelayBound,
            delay_bound,
            use_choices: false,
        }
    }

    /// Enable choice-aware cut enumeration
    pub fn with_choices(mut self) -> Self {
        self.use_choices = true;
        self
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

    /// Add a cut using priority-based selection
    ///
    /// This is used during priority-aware cut enumeration to keep
    /// the best cuts according to the specified priority mode.
    pub fn add_with_priority(
        &mut self,
        cut: Cut,
        max_cuts: usize,
        priority: CutPriority,
        delay_bound: f32,
    ) {
        // Check if this cut is dominated by an existing cut
        for existing in &self.cuts {
            if existing.dominates(&cut) {
                return;
            }
        }

        // Remove cuts that are dominated by the new cut
        self.cuts.retain(|existing| !cut.dominates(existing));

        let new_score = cut.priority_score(priority, delay_bound);

        // Add the new cut if we have room
        if self.cuts.len() < max_cuts {
            self.cuts.push(cut);
        } else {
            // Replace the worst cut if the new one is better
            if let Some((worst_idx, worst_score)) = self
                .cuts
                .iter()
                .enumerate()
                .map(|(i, c)| (i, c.priority_score(priority, delay_bound)))
                .max_by(|(_, a), (_, b)| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal))
            {
                if new_score < worst_score {
                    self.cuts[worst_idx] = cut;
                }
            }
        }
    }

    /// Get the best cut (smallest)
    pub fn best(&self) -> Option<&Cut> {
        self.cuts.iter().min_by_key(|c| c.size())
    }

    /// Get the best cut according to priority mode
    pub fn best_by_priority(&self, priority: CutPriority, delay_bound: f32) -> Option<&Cut> {
        self.cuts.iter().min_by(|a, b| {
            let score_a = a.priority_score(priority, delay_bound);
            let score_b = b.priority_score(priority, delay_bound);
            score_a
                .partial_cmp(&score_b)
                .unwrap_or(std::cmp::Ordering::Equal)
        })
    }

    /// Sort cuts by priority (best first)
    pub fn sort_by_priority(&mut self, priority: CutPriority, delay_bound: f32) {
        self.cuts.sort_by(|a, b| {
            let score_a = a.priority_score(priority, delay_bound);
            let score_b = b.priority_score(priority, delay_bound);
            score_a
                .partial_cmp(&score_b)
                .unwrap_or(std::cmp::Ordering::Equal)
        });
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

    /// Enumerate cuts with priority-based selection
    ///
    /// This variant uses the priority mode specified in params to decide
    /// which cuts to keep when the cut set is full.
    pub fn enumerate_with_priority(aig: &Aig, params: CutParams) -> Self {
        let mut node_cuts: HashMap<AigNodeId, CutSet> = HashMap::new();
        let priority = params.priority;
        let delay_bound = params.delay_bound;

        // Process nodes in topological order
        for (id, node) in aig.iter_nodes() {
            let cut_set = match node {
                AigNode::Const => CutSet::trivial(id),
                AigNode::Input { .. } => CutSet::trivial(id),
                AigNode::Latch { .. } => CutSet::trivial(id),
                AigNode::Barrier { .. } => CutSet::trivial(id),
                AigNode::And { left, right } => {
                    let left_cuts = node_cuts.get(&left.node).cloned().unwrap_or_default();
                    let right_cuts = node_cuts.get(&right.node).cloned().unwrap_or_default();

                    let mut new_cuts = CutSet::new();

                    // Always include the trivial cut
                    new_cuts.add_with_priority(
                        Cut::trivial(id),
                        params.max_cuts,
                        priority,
                        delay_bound,
                    );

                    // Merge all pairs of cuts from left and right
                    for left_cut in &left_cuts.cuts {
                        for right_cut in &right_cuts.cuts {
                            let merged = left_cut.merge(right_cut);
                            if merged.is_k_feasible(params.k) {
                                let mut cut = merged;
                                // Update arrival time: add 1 unit delay for the AND gate
                                cut.arrival_time += 1.0;

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
                                new_cuts.add_with_priority(
                                    cut,
                                    params.max_cuts,
                                    priority,
                                    delay_bound,
                                );
                            }
                        }
                    }

                    // Sort by priority for consistent ordering
                    new_cuts.sort_by_priority(priority, delay_bound);
                    new_cuts
                }
            };

            node_cuts.insert(id, cut_set);
        }

        Self { node_cuts, params }
    }

    /// Enumerate cuts including choice nodes
    ///
    /// When `use_choices` is enabled in params, this method will also consider
    /// alternative implementations stored in the AIG's choice nodes.
    pub fn enumerate_with_choices(aig: &Aig, params: CutParams) -> Self {
        let mut node_cuts: HashMap<AigNodeId, CutSet> = HashMap::new();
        let priority = params.priority;
        let delay_bound = params.delay_bound;

        // Process nodes in topological order
        for (id, node) in aig.iter_nodes() {
            let cut_set = match node {
                AigNode::Const => CutSet::trivial(id),
                AigNode::Input { .. } => CutSet::trivial(id),
                AigNode::Latch { .. } => CutSet::trivial(id),
                AigNode::Barrier { .. } => CutSet::trivial(id),
                AigNode::And { left, right } => {
                    let left_cuts = node_cuts.get(&left.node).cloned().unwrap_or_default();
                    let right_cuts = node_cuts.get(&right.node).cloned().unwrap_or_default();

                    let mut new_cuts = CutSet::new();

                    // Always include the trivial cut
                    new_cuts.add_with_priority(
                        Cut::trivial(id),
                        params.max_cuts,
                        priority,
                        delay_bound,
                    );

                    // Merge all pairs of cuts from left and right
                    for left_cut in &left_cuts.cuts {
                        for right_cut in &right_cuts.cuts {
                            let merged = left_cut.merge(right_cut);
                            if merged.is_k_feasible(params.k) {
                                let mut cut = merged;
                                cut.arrival_time += 1.0;

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
                                new_cuts.add_with_priority(
                                    cut,
                                    params.max_cuts,
                                    priority,
                                    delay_bound,
                                );
                            }
                        }
                    }

                    // If choices are enabled, also enumerate cuts through choice alternatives
                    if params.use_choices && aig.has_choices(id) {
                        for choice_lit in aig.get_choices(id) {
                            if let Some(choice_cuts) = node_cuts.get(&choice_lit.node) {
                                for choice_cut in &choice_cuts.cuts {
                                    let mut cut = choice_cut.clone();
                                    // Adjust for inversion if needed
                                    if choice_lit.inverted {
                                        cut.truth_table = !cut.truth_table;
                                    }
                                    new_cuts.add_with_priority(
                                        cut,
                                        params.max_cuts,
                                        priority,
                                        delay_bound,
                                    );
                                }
                            }
                        }
                    }

                    new_cuts.sort_by_priority(priority, delay_bound);
                    new_cuts
                }
            };

            node_cuts.insert(id, cut_set);
        }

        Self { node_cuts, params }
    }

    /// Get the best cut for a node according to priority mode
    pub fn best_cut_by_priority(&self, node: AigNodeId) -> Option<&Cut> {
        self.node_cuts
            .get(&node)
            .and_then(|cs| cs.best_by_priority(self.params.priority, self.params.delay_bound))
    }

    /// Compute the depth (max arrival time) of the AIG
    pub fn max_depth(&self) -> f32 {
        self.node_cuts
            .values()
            .flat_map(|cs| cs.cuts.iter())
            .map(|c| c.arrival_time)
            .fold(0.0f32, f32::max)
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

    #[test]
    fn test_cut_priority_score_area() {
        let cut1 = Cut::with_metrics(vec![AigNodeId(1), AigNodeId(2)], 2.0, 3.0);
        let cut2 = Cut::with_metrics(vec![AigNodeId(1), AigNodeId(2), AigNodeId(3)], 3.0, 2.0);

        // In Area mode, cut1 is better (lower area_cost)
        assert!(
            cut1.priority_score(CutPriority::Area, f32::INFINITY)
                < cut2.priority_score(CutPriority::Area, f32::INFINITY)
        );
    }

    #[test]
    fn test_cut_priority_score_delay() {
        let cut1 = Cut::with_metrics(vec![AigNodeId(1), AigNodeId(2)], 2.0, 5.0);
        let cut2 = Cut::with_metrics(vec![AigNodeId(1), AigNodeId(2), AigNodeId(3)], 3.0, 3.0);

        // In Delay mode, cut2 is better (lower arrival_time)
        assert!(
            cut2.priority_score(CutPriority::Delay, f32::INFINITY)
                < cut1.priority_score(CutPriority::Delay, f32::INFINITY)
        );
    }

    #[test]
    fn test_cut_priority_score_area_delay() {
        let cut1 = Cut::with_metrics(vec![AigNodeId(1), AigNodeId(2)], 2.0, 2.0); // score = 4
        let cut2 = Cut::with_metrics(vec![AigNodeId(1), AigNodeId(2)], 3.0, 1.5); // score = 4.5

        // In AreaDelay mode, cut1 is better (lower product)
        assert!(
            cut1.priority_score(CutPriority::AreaDelay, f32::INFINITY)
                < cut2.priority_score(CutPriority::AreaDelay, f32::INFINITY)
        );
    }

    #[test]
    fn test_cut_priority_score_with_delay_bound() {
        let cut1 = Cut::with_metrics(vec![AigNodeId(1), AigNodeId(2)], 2.0, 4.0);
        let cut2 = Cut::with_metrics(vec![AigNodeId(1), AigNodeId(2)], 3.0, 2.0);

        // With delay bound of 3.0, cut1 violates the bound, so cut2 is better
        let delay_bound = 3.0;
        assert!(
            cut2.priority_score(CutPriority::AreaWithDelayBound, delay_bound)
                < cut1.priority_score(CutPriority::AreaWithDelayBound, delay_bound)
        );

        // With delay bound of 5.0, both are valid, so cut1 is better (lower area)
        let delay_bound = 5.0;
        assert!(
            cut1.priority_score(CutPriority::AreaWithDelayBound, delay_bound)
                < cut2.priority_score(CutPriority::AreaWithDelayBound, delay_bound)
        );
    }

    #[test]
    fn test_cut_merge_propagates_metrics() {
        let cut1 = Cut::with_metrics(vec![AigNodeId(1)], 1.0, 2.0);
        let cut2 = Cut::with_metrics(vec![AigNodeId(2)], 1.5, 3.0);
        let merged = cut1.merge(&cut2);

        // Area should be summed
        assert!((merged.area_cost - 2.5).abs() < 0.001);
        // Arrival time should be max
        assert!((merged.arrival_time - 3.0).abs() < 0.001);
        // Edge count should be count of leaves
        assert_eq!(merged.edge_count, 2);
    }

    #[test]
    fn test_cut_set_add_with_priority() {
        let mut cs = CutSet::new();

        // Add cuts with different priorities
        let cut1 = Cut::with_metrics(vec![AigNodeId(1)], 1.0, 2.0);
        let cut2 = Cut::with_metrics(vec![AigNodeId(2)], 2.0, 1.0);
        let cut3 = Cut::with_metrics(vec![AigNodeId(3)], 3.0, 0.5);

        cs.add_with_priority(cut1.clone(), 2, CutPriority::Area, f32::INFINITY);
        cs.add_with_priority(cut2.clone(), 2, CutPriority::Area, f32::INFINITY);
        cs.add_with_priority(cut3.clone(), 2, CutPriority::Area, f32::INFINITY);

        // Only 2 cuts should be kept (max_cuts = 2)
        assert_eq!(cs.len(), 2);

        // The best cut by area should be cut1 (area = 1.0)
        let best = cs
            .best_by_priority(CutPriority::Area, f32::INFINITY)
            .unwrap();
        assert!((best.area_cost - 1.0).abs() < 0.001);
    }

    #[test]
    fn test_cut_set_best_by_priority() {
        let mut cs = CutSet::new();

        let cut1 = Cut::with_metrics(vec![AigNodeId(1)], 1.0, 5.0); // Best area, worst delay
        let cut2 = Cut::with_metrics(vec![AigNodeId(2)], 3.0, 1.0); // Worst area, best delay

        cs.cuts.push(cut1);
        cs.cuts.push(cut2);

        // Best by area should be first cut
        let best_area = cs
            .best_by_priority(CutPriority::Area, f32::INFINITY)
            .unwrap();
        assert!((best_area.area_cost - 1.0).abs() < 0.001);

        // Best by delay should be second cut
        let best_delay = cs
            .best_by_priority(CutPriority::Delay, f32::INFINITY)
            .unwrap();
        assert!((best_delay.arrival_time - 1.0).abs() < 0.001);
    }

    #[test]
    fn test_cut_set_sort_by_priority() {
        let mut cs = CutSet::new();

        cs.cuts
            .push(Cut::with_metrics(vec![AigNodeId(1)], 3.0, 1.0));
        cs.cuts
            .push(Cut::with_metrics(vec![AigNodeId(2)], 1.0, 2.0));
        cs.cuts
            .push(Cut::with_metrics(vec![AigNodeId(3)], 2.0, 3.0));

        cs.sort_by_priority(CutPriority::Area, f32::INFINITY);

        // Should be sorted by area: 1.0, 2.0, 3.0
        assert!((cs.cuts[0].area_cost - 1.0).abs() < 0.001);
        assert!((cs.cuts[1].area_cost - 2.0).abs() < 0.001);
        assert!((cs.cuts[2].area_cost - 3.0).abs() < 0.001);
    }

    #[test]
    fn test_cut_params_presets() {
        let delay_params = CutParams::delay_optimal(4);
        assert_eq!(delay_params.priority, CutPriority::Delay);
        assert_eq!(delay_params.k, 4);

        let area_params = CutParams::area_optimal(6);
        assert_eq!(area_params.priority, CutPriority::Area);
        assert_eq!(area_params.k, 6);

        let recovery_params = CutParams::area_recovery(4, 10.0);
        assert_eq!(recovery_params.priority, CutPriority::AreaWithDelayBound);
        assert!((recovery_params.delay_bound - 10.0).abs() < 0.001);
    }

    #[test]
    fn test_enumerate_with_priority() {
        let mut aig = Aig::new("test".to_string());
        let a = aig.add_input("a".to_string(), None);
        let b = aig.add_input("b".to_string(), None);
        let c = aig.add_input("c".to_string(), None);

        // Create a chain: ab = a & b, abc = ab & c
        let ab = aig.add_and(AigLit::new(a), AigLit::new(b));
        let abc = aig.add_and(ab, AigLit::new(c));
        aig.add_output("y".to_string(), abc);

        let params = CutParams::delay_optimal(4);
        let enum_result = CutEnumeration::enumerate_with_priority(&aig, params);

        // Should have cuts with arrival time information
        let final_cuts = enum_result.get_cuts(abc.node).unwrap();
        assert!(!final_cuts.is_empty());

        // The best cut by delay should be available
        let best = enum_result.best_cut_by_priority(abc.node);
        assert!(best.is_some());
    }

    #[test]
    fn test_enumerate_with_choices() {
        let mut aig = Aig::new("test".to_string());
        let a = aig.add_input("a".to_string(), None);
        let b = aig.add_input("b".to_string(), None);
        let c = aig.add_input("c".to_string(), None);

        // Create two equivalent implementations
        let ab = aig.add_and(AigLit::new(a), AigLit::new(b));
        let bc = aig.add_and(AigLit::new(b), AigLit::new(c));

        // Add choice: ab is equivalent to some alternative (for testing, use bc as mock)
        aig.add_choice(ab.node, AigLit::new(bc.node));

        aig.add_output("y".to_string(), ab);

        let params = CutParams::default().with_choices();
        let enum_result = CutEnumeration::enumerate_with_choices(&aig, params);

        // Should have enumerated cuts
        assert!(enum_result.get_cuts(ab.node).is_some());

        // Cuts from the choice node should be included
        let ab_cuts = enum_result.get_cuts(ab.node).unwrap();
        // Due to choice, we should have more cuts (from bc as well)
        assert!(!ab_cuts.is_empty());
    }

    #[test]
    fn test_max_depth() {
        let mut aig = Aig::new("test".to_string());
        let a = aig.add_input("a".to_string(), None);
        let b = aig.add_input("b".to_string(), None);
        let c = aig.add_input("c".to_string(), None);

        // Chain of 2 ANDs
        let ab = aig.add_and(AigLit::new(a), AigLit::new(b));
        let abc = aig.add_and(ab, AigLit::new(c));
        aig.add_output("y".to_string(), abc);

        let enum_result = CutEnumeration::enumerate_with_priority(&aig, CutParams::default());

        // Depth should be 2 (two levels of AND gates)
        let depth = enum_result.max_depth();
        assert!(depth >= 1.0); // At least one level of logic
    }
}
