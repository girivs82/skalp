//! AIG Resubstitution Pass
//!
//! This pass performs Boolean resubstitution, which attempts to re-express
//! each node using a different set of divisors (existing nodes in the circuit).
//!
//! # Algorithm
//!
//! For each node:
//! 1. Compute the set of potential divisors (nodes in the transitive fanin)
//! 2. Compute the truth table of the node and each divisor
//! 3. Check if the node can be expressed with fewer gates using divisors
//! 4. If found, substitute with the simpler expression
//!
//! # References
//!
//! - Mishchenko, A., Brayton, R., Jiang, J.-H. R., & Jang, S. (2011).
//!   Scalable don't-care-based logic optimization and resynthesis.
//! - Lee, C., Riener, H., Mishchenko, A., Brayton, R., & De Micheli, G. (2020).
//!   Simulation-based resubstitution. IWLS'20.

use super::{Pass, PassResult};
use crate::synth::{Aig, AigLit, AigNode, AigNodeId};
use std::collections::{HashMap, HashSet};

/// Maximum number of divisors to consider
const MAX_DIVISORS: usize = 150;

/// Maximum number of inputs for truth table computation
const MAX_TT_INPUTS: usize = 6;

/// Maximum resubstitution gain (number of divisors to use)
const MAX_RESUB_DIVS: usize = 3;

/// Resubstitution pass
pub struct Resub {
    /// Maximum number of divisors
    max_divisors: usize,
    /// Maximum inputs for truth table
    max_inputs: usize,
    /// Number of nodes resubstituted
    resub_count: usize,
    /// Total nodes saved
    total_savings: i32,
    /// Zero-cost mode
    zero_cost: bool,
}

impl Resub {
    /// Create a new resubstitution pass
    pub fn new() -> Self {
        Self {
            max_divisors: MAX_DIVISORS,
            max_inputs: MAX_TT_INPUTS,
            resub_count: 0,
            total_savings: 0,
            zero_cost: false,
        }
    }

    /// Create with zero-cost mode (allows 0-gain substitutions for restructuring)
    pub fn zero_cost() -> Self {
        Self {
            max_divisors: MAX_DIVISORS,
            max_inputs: MAX_TT_INPUTS,
            resub_count: 0,
            total_savings: 0,
            zero_cost: true,
        }
    }

    /// Create with custom parameters
    pub fn with_params(max_divisors: usize, max_inputs: usize) -> Self {
        Self {
            max_divisors,
            max_inputs,
            resub_count: 0,
            total_savings: 0,
            zero_cost: false,
        }
    }

    /// Collect potential divisors for a node
    /// Divisors are nodes in the transitive fanin that could be used
    fn collect_divisors(
        &self,
        aig: &Aig,
        target: AigNodeId,
        fanout_counts: &HashMap<AigNodeId, usize>,
    ) -> Vec<AigNodeId> {
        let mut divisors = Vec::new();
        let mut visited = HashSet::new();
        let mut stack = vec![target];

        // Collect nodes in transitive fanin
        while let Some(node) = stack.pop() {
            if visited.contains(&node) || divisors.len() >= self.max_divisors {
                continue;
            }
            visited.insert(node);

            // Don't include the target itself
            if node != target {
                divisors.push(node);
            }

            if let Some(n) = aig.get_node(node) {
                for fanin in n.fanins() {
                    stack.push(fanin.node);
                }
            }
        }

        // Sort by depth (prefer nodes closer to inputs for stability)
        // Also prefer nodes with higher fanout (more useful as divisors)
        divisors.sort_by(|&a, &b| {
            let fanout_a = fanout_counts.get(&a).copied().unwrap_or(0);
            let fanout_b = fanout_counts.get(&b).copied().unwrap_or(0);
            fanout_b.cmp(&fanout_a) // Higher fanout first
        });

        divisors.truncate(self.max_divisors);
        divisors
    }

    /// Collect leaves (primary inputs and latches) for truth table computation
    fn collect_leaves(&self, aig: &Aig, nodes: &[AigNodeId]) -> Vec<AigNodeId> {
        let mut leaves = Vec::new();
        let mut visited = HashSet::new();
        let mut stack: Vec<AigNodeId> = nodes.to_vec();

        while let Some(node) = stack.pop() {
            if visited.contains(&node) {
                continue;
            }
            visited.insert(node);

            match aig.get_node(node) {
                Some(AigNode::Input { .. }) | Some(AigNode::Latch { .. }) => {
                    if !leaves.contains(&node) {
                        leaves.push(node);
                    }
                }
                Some(AigNode::And { left, right, .. }) => {
                    stack.push(left.node);
                    stack.push(right.node);
                }
                Some(AigNode::Const) => {}
                Some(AigNode::Barrier { data, .. }) => {
                    stack.push(data.node);
                }
                None => {}
            }
        }

        leaves
    }

    /// Compute truth table for a node given leaf assignments
    fn compute_truth_table(&self, aig: &Aig, node: AigNodeId, leaves: &[AigNodeId]) -> Option<u64> {
        if leaves.len() > self.max_inputs {
            return None;
        }

        let num_rows = 1usize << leaves.len();
        let leaf_to_idx: HashMap<AigNodeId, usize> =
            leaves.iter().enumerate().map(|(i, &n)| (n, i)).collect();

        let mut tt = 0u64;
        let mut cache = HashMap::new();

        for row in 0..num_rows {
            if self.evaluate_node(aig, node, row, &leaf_to_idx, &mut cache) {
                tt |= 1u64 << row;
            }
            cache.clear();
        }

        Some(tt)
    }

    /// Evaluate a node for a given input assignment
    fn evaluate_node(
        &self,
        aig: &Aig,
        node: AigNodeId,
        assignment: usize,
        leaf_to_idx: &HashMap<AigNodeId, usize>,
        cache: &mut HashMap<AigNodeId, bool>,
    ) -> bool {
        if let Some(&cached) = cache.get(&node) {
            return cached;
        }

        let result = match aig.get_node(node) {
            Some(AigNode::Input { .. }) | Some(AigNode::Latch { .. }) => {
                if let Some(&idx) = leaf_to_idx.get(&node) {
                    (assignment >> idx) & 1 == 1
                } else {
                    false
                }
            }
            Some(AigNode::And { left, right, .. }) => {
                let left_val = self.evaluate_lit(aig, *left, assignment, leaf_to_idx, cache);
                let right_val = self.evaluate_lit(aig, *right, assignment, leaf_to_idx, cache);
                left_val && right_val
            }
            Some(AigNode::Const) => false,
            _ => false,
        };

        cache.insert(node, result);
        result
    }

    /// Evaluate a literal for a given input assignment
    fn evaluate_lit(
        &self,
        aig: &Aig,
        lit: AigLit,
        assignment: usize,
        leaf_to_idx: &HashMap<AigNodeId, usize>,
        cache: &mut HashMap<AigNodeId, bool>,
    ) -> bool {
        let val = self.evaluate_node(aig, lit.node, assignment, leaf_to_idx, cache);
        if lit.inverted {
            !val
        } else {
            val
        }
    }

    /// Try to find a resubstitution for a node using divisors
    /// Returns (new_expression, number_of_gates_saved)
    fn try_resub(
        &self,
        target_tt: u64,
        divisor_tts: &[(AigNodeId, u64)],
        num_leaves: usize,
    ) -> Option<ResubResult> {
        let mask = if num_leaves >= 6 {
            u64::MAX
        } else {
            (1u64 << (1 << num_leaves)) - 1
        };
        let target = target_tt & mask;

        // Try 0-divisor resubstitution (constant or single divisor)
        for (i, &(div_node, div_tt)) in divisor_tts.iter().enumerate() {
            let div = div_tt & mask;

            // Check if target == divisor
            if target == div {
                return Some(ResubResult {
                    kind: ResubKind::Equal(div_node, false),
                    gates_saved: 1, // Save at least the target node
                });
            }

            // Check if target == !divisor
            if target == (!div & mask) {
                return Some(ResubResult {
                    kind: ResubKind::Equal(div_node, true),
                    gates_saved: 1,
                });
            }
        }

        // Try 1-divisor resubstitution (AND, OR with one divisor)
        for (i, &(div_node, div_tt)) in divisor_tts.iter().enumerate() {
            let div = div_tt & mask;

            // Check if target == d AND x or d AND !x for some other divisor x
            for (j, &(div2_node, div2_tt)) in divisor_tts.iter().enumerate() {
                if i == j {
                    continue;
                }
                let div2 = div2_tt & mask;

                // target == div AND div2
                if target == (div & div2) {
                    return Some(ResubResult {
                        kind: ResubKind::And2(div_node, false, div2_node, false),
                        gates_saved: 0, // AND of two divisors = 1 gate, might not save
                    });
                }

                // target == div AND !div2
                if target == (div & (!div2 & mask)) {
                    return Some(ResubResult {
                        kind: ResubKind::And2(div_node, false, div2_node, true),
                        gates_saved: 0,
                    });
                }

                // target == !div AND div2
                if target == ((!div & mask) & div2) {
                    return Some(ResubResult {
                        kind: ResubKind::And2(div_node, true, div2_node, false),
                        gates_saved: 0,
                    });
                }

                // target == !div AND !div2
                if target == ((!div & mask) & (!div2 & mask)) {
                    return Some(ResubResult {
                        kind: ResubKind::And2(div_node, true, div2_node, true),
                        gates_saved: 0,
                    });
                }

                // OR variants: target == div OR div2 = !(!div AND !div2)
                if target == (!(!div & mask) & (!div2 & mask)) | (div | div2) & mask {
                    // Simplified: target == div | div2
                    if target == ((div | div2) & mask) {
                        return Some(ResubResult {
                            kind: ResubKind::Or2(div_node, false, div2_node, false),
                            gates_saved: 0,
                        });
                    }
                }
            }
        }

        // Try 2-divisor AND-OR: (a AND b) OR c
        if divisor_tts.len() >= 3 {
            for (i, &(d1, tt1)) in divisor_tts.iter().enumerate() {
                for (j, &(d2, tt2)) in divisor_tts.iter().enumerate() {
                    if j <= i {
                        continue;
                    }
                    for (k, &(d3, tt3)) in divisor_tts.iter().enumerate() {
                        if k == i || k == j {
                            continue;
                        }

                        let t1 = tt1 & mask;
                        let t2 = tt2 & mask;
                        let t3 = tt3 & mask;

                        // (d1 AND d2) OR d3
                        if target == ((t1 & t2) | t3) & mask {
                            return Some(ResubResult {
                                kind: ResubKind::AndOr(d1, false, d2, false, d3, false),
                                gates_saved: 0,
                            });
                        }
                    }
                }
            }
        }

        None
    }

    /// Build the resubstitution result in the AIG
    fn build_resub(&self, aig: &mut Aig, result: &ResubResult) -> AigLit {
        match &result.kind {
            ResubKind::Equal(node, inverted) => AigLit {
                node: *node,
                inverted: *inverted,
            },
            ResubKind::And2(n1, inv1, n2, inv2) => {
                let lit1 = AigLit {
                    node: *n1,
                    inverted: *inv1,
                };
                let lit2 = AigLit {
                    node: *n2,
                    inverted: *inv2,
                };
                aig.add_and(lit1, lit2)
            }
            ResubKind::Or2(n1, inv1, n2, inv2) => {
                // OR = NAND of complements
                let lit1 = AigLit {
                    node: *n1,
                    inverted: !*inv1,
                };
                let lit2 = AigLit {
                    node: *n2,
                    inverted: !*inv2,
                };
                let and_result = aig.add_and(lit1, lit2);
                AigLit {
                    node: and_result.node,
                    inverted: true,
                }
            }
            ResubKind::AndOr(n1, inv1, n2, inv2, n3, inv3) => {
                // (n1 AND n2) OR n3 = !(!( n1 AND n2) AND !n3)
                let lit1 = AigLit {
                    node: *n1,
                    inverted: *inv1,
                };
                let lit2 = AigLit {
                    node: *n2,
                    inverted: *inv2,
                };
                let and12 = aig.add_and(lit1, lit2);

                let lit3 = AigLit {
                    node: *n3,
                    inverted: !*inv3,
                };
                let nand_result = aig.add_and(
                    AigLit {
                        node: and12.node,
                        inverted: true,
                    },
                    lit3,
                );
                AigLit {
                    node: nand_result.node,
                    inverted: true,
                }
            }
        }
    }
}

/// Result of resubstitution
#[derive(Debug, Clone)]
struct ResubResult {
    kind: ResubKind,
    gates_saved: i32,
}

/// Kind of resubstitution found
#[derive(Debug, Clone)]
enum ResubKind {
    /// Target equals a divisor (possibly inverted)
    Equal(AigNodeId, bool),
    /// Target is AND of two divisors
    And2(AigNodeId, bool, AigNodeId, bool),
    /// Target is OR of two divisors
    Or2(AigNodeId, bool, AigNodeId, bool),
    /// Target is (d1 AND d2) OR d3
    AndOr(AigNodeId, bool, AigNodeId, bool, AigNodeId, bool),
}

/// Compute fanout counts
fn compute_fanout_counts(aig: &Aig) -> HashMap<AigNodeId, usize> {
    let mut counts = HashMap::new();

    for (_, node) in aig.iter_nodes() {
        for fanin in node.fanins() {
            *counts.entry(fanin.node).or_insert(0) += 1;
        }
    }

    for (_, lit) in aig.outputs() {
        *counts.entry(lit.node).or_insert(0) += 1;
    }

    counts
}

impl Default for Resub {
    fn default() -> Self {
        Self::new()
    }
}

impl Pass for Resub {
    fn name(&self) -> &str {
        if self.zero_cost {
            "resub_z"
        } else {
            "resub"
        }
    }

    fn run(&mut self, aig: &mut Aig) -> PassResult {
        let mut result = PassResult::new(self.name());
        result.record_before(aig);

        self.resub_count = 0;
        self.total_savings = 0;

        let fanout_counts = compute_fanout_counts(aig);

        // Collect AND nodes to process
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

        for target in nodes {
            // Skip if already substituted
            if substitutions.contains_key(&target) {
                continue;
            }

            // Collect divisors
            let divisors = self.collect_divisors(aig, target, &fanout_counts);
            if divisors.is_empty() {
                continue;
            }

            // Get all nodes for leaf collection
            let mut all_nodes = divisors.clone();
            all_nodes.push(target);

            // Collect leaves
            let leaves = self.collect_leaves(aig, &all_nodes);
            if leaves.len() > self.max_inputs {
                continue;
            }

            // Compute truth tables
            let target_tt = match self.compute_truth_table(aig, target, &leaves) {
                Some(tt) => tt,
                None => continue,
            };

            let mut divisor_tts: Vec<(AigNodeId, u64)> = Vec::new();
            for &div in &divisors {
                if let Some(tt) = self.compute_truth_table(aig, div, &leaves) {
                    divisor_tts.push((div, tt));
                }
            }

            // Try resubstitution
            if let Some(resub_result) = self.try_resub(target_tt, &divisor_tts, leaves.len()) {
                // Check if we should accept this
                let accept = if self.zero_cost {
                    resub_result.gates_saved >= 0
                } else {
                    resub_result.gates_saved > 0
                };

                if accept || matches!(resub_result.kind, ResubKind::Equal(_, _)) {
                    let new_lit = self.build_resub(aig, &resub_result);
                    if new_lit.node != target {
                        substitutions.insert(target, new_lit);
                        self.resub_count += 1;
                        self.total_savings += resub_result.gates_saved;
                    }
                }
            }
        }

        // Apply substitutions
        if !substitutions.is_empty() {
            aig.apply_substitutions(&substitutions);

            // Clean up
            let mut dce = super::Dce::new();
            dce.run(aig);
        }

        result.record_after(aig);
        result.add_extra("nodes_resubstituted", &self.resub_count.to_string());
        result.add_extra("nodes_saved", &self.total_savings.to_string());
        result
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_resub_creation() {
        let pass = Resub::new();
        assert_eq!(pass.name(), "resub");

        let pass_z = Resub::zero_cost();
        assert_eq!(pass_z.name(), "resub_z");
    }

    #[test]
    fn test_resub_simple() {
        let mut aig = Aig::new("test".to_string());
        let a = aig.add_input("a".to_string(), None);
        let b = aig.add_input("b".to_string(), None);
        let ab = aig.add_and(AigLit::new(a), AigLit::new(b));
        aig.add_output("y".to_string(), ab);

        let mut pass = Resub::new();
        let result = pass.run(&mut aig);

        // Should complete without error
        assert!(result.ands_after >= 1);
    }

    #[test]
    fn test_resub_duplicate_detection() {
        // Create a circuit where two nodes compute the same function
        let mut aig = Aig::new("test".to_string());
        let a = aig.add_input("a".to_string(), None);
        let b = aig.add_input("b".to_string(), None);

        // First a AND b
        let ab1 = aig.add_and(AigLit::new(a), AigLit::new(b));

        // Duplicate: another a AND b (should be found by resub)
        let ab2 = aig.add_and(AigLit::new(a), AigLit::new(b));

        // Use both
        let result = aig.add_and(ab1, ab2);
        aig.add_output("y".to_string(), result);

        let before = aig.compute_stats().and_count;

        let mut pass = Resub::new();
        pass.run(&mut aig);

        // Strash should have already merged these, but resub should find it too
        // The result should use fewer or equal gates
        assert!(aig.compute_stats().and_count <= before);
    }

    #[test]
    fn test_collect_divisors() {
        let mut aig = Aig::new("test".to_string());
        let a = aig.add_input("a".to_string(), None);
        let b = aig.add_input("b".to_string(), None);
        let c = aig.add_input("c".to_string(), None);

        let ab = aig.add_and(AigLit::new(a), AigLit::new(b));
        let abc = aig.add_and(ab, AigLit::new(c));
        aig.add_output("y".to_string(), abc);

        let fanout_counts = compute_fanout_counts(&aig);
        let resub = Resub::new();

        let divisors = resub.collect_divisors(&aig, abc.node, &fanout_counts);

        // Should include ab, a, b, c
        assert!(!divisors.is_empty());
        assert!(divisors.contains(&ab.node));
    }

    #[test]
    fn test_with_params() {
        let pass = Resub::with_params(100, 5);
        assert_eq!(pass.max_divisors, 100);
        assert_eq!(pass.max_inputs, 5);
    }
}
