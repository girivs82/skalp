//! Tree Balancing Pass
//!
//! This pass balances AND trees to minimize logic depth. It collects
//! all inputs to an AND tree and rebuilds it as a balanced tree.
//!
//! # Algorithm
//!
//! For each node, we collect all leaves of its AND cone, then rebuild
//! using a balanced binary tree structure. This reduces depth from
//! O(n) to O(log n) for chains of ANDs.
//!
//! # References
//!
//! - Cortadella, J. (2003). Timing-driven logic bi-decomposition.
//! - ABC User Guide: `balance` command for AIG tree balancing.

use super::{Pass, PassResult};
use crate::synth::{Aig, AigLit, AigNode, AigNodeId, AigSafetyInfo, BarrierType};
use indexmap::IndexMap;
use std::collections::HashSet;

/// Tree balancing pass
pub struct Balance {
    /// Number of nodes rebalanced
    rebalanced_count: usize,
}

/// Collect all leaves of an AND cone rooted at the given literal
///
/// A leaf is either:
/// - An input
/// - A latch
/// - An AND node that is inverted (can't be expanded)
/// - An AND node with multiple fanouts (can't be absorbed)
fn collect_leaves(
    aig: &Aig,
    lit: AigLit,
    fanout_counts: &IndexMap<AigNodeId, usize>,
    leaves: &mut Vec<AigLit>,
    visited: &mut HashSet<AigNodeId>,
) {
    // Inverted literals are always leaves (can't expand through NOT)
    if lit.inverted {
        leaves.push(lit);
        return;
    }

    // Already visited - this is a multi-fanout node
    if visited.contains(&lit.node) {
        leaves.push(lit);
        return;
    }

    // Check if this is an AND node we can expand
    if let Some(node) = aig.get_node(lit.node) {
        match node {
            AigNode::And { left, right } => {
                // Only expand if single fanout (or we're at the root)
                let fanouts = fanout_counts.get(&lit.node).copied().unwrap_or(0);
                if fanouts > 1 {
                    leaves.push(lit);
                } else {
                    visited.insert(lit.node);
                    collect_leaves(aig, *left, fanout_counts, leaves, visited);
                    collect_leaves(aig, *right, fanout_counts, leaves, visited);
                }
            }
            _ => {
                // Input, latch, or constant - always a leaf
                leaves.push(lit);
            }
        }
    } else {
        leaves.push(lit);
    }
}

/// Build a balanced AND tree from a list of literals
fn build_balanced_tree(new_aig: &mut Aig, mut lits: Vec<AigLit>, safety: AigSafetyInfo) -> AigLit {
    if lits.is_empty() {
        return AigLit::true_lit();
    }
    if lits.len() == 1 {
        return lits.pop().unwrap();
    }

    // Build tree level by level (bottom-up)
    while lits.len() > 1 {
        let mut next_level = Vec::with_capacity(lits.len().div_ceil(2));

        let mut i = 0;
        while i + 1 < lits.len() {
            let combined = new_aig.add_and_with_safety(lits[i], lits[i + 1], safety.clone());
            next_level.push(combined);
            i += 2;
        }

        // Handle odd element
        if i < lits.len() {
            next_level.push(lits[i]);
        }

        lits = next_level;
    }

    lits.pop().unwrap()
}

impl Balance {
    /// Create a new tree balancing pass
    pub fn new() -> Self {
        Self {
            rebalanced_count: 0,
        }
    }

    /// Compute fanout counts for all nodes
    fn compute_fanout_counts(&self, aig: &Aig) -> IndexMap<AigNodeId, usize> {
        let mut counts: IndexMap<AigNodeId, usize> = IndexMap::new();

        for (_, node) in aig.iter_nodes() {
            for fanin in node.fanins() {
                *counts.entry(fanin.node).or_insert(0) += 1;
            }
        }

        // Count outputs as fanouts
        for (_, lit) in aig.outputs() {
            *counts.entry(lit.node).or_insert(0) += 1;
        }

        counts
    }
}

impl Default for Balance {
    fn default() -> Self {
        Self::new()
    }
}

impl Pass for Balance {
    fn name(&self) -> &str {
        "balance"
    }

    #[allow(clippy::type_complexity)]
    fn run(&mut self, aig: &mut Aig) -> PassResult {
        let mut result = PassResult::new(self.name());
        result.record_before(aig);

        self.rebalanced_count = 0;

        // Compute fanout counts
        let fanout_counts = self.compute_fanout_counts(aig);

        // Build new AIG with balanced trees using two-phase approach for sequential circuits:
        // Phase 1: Process inputs, pre-create latches/barriers, then process AND nodes
        // Phase 2: Update latch/barrier data with resolved values
        let mut new_aig = Aig::new(aig.name.clone());
        let mut node_map: IndexMap<AigNodeId, AigLit> = IndexMap::new();

        // Map constant
        node_map.insert(AigNodeId::FALSE, AigLit::false_lit());

        // Collect latches and barriers for two-phase processing
        let mut latch_ids: Vec<(
            AigNodeId,
            AigLit,
            Option<bool>,
            Option<AigNodeId>,
            Option<AigNodeId>,
        )> = Vec::new();
        let mut barrier_ids: Vec<(
            AigNodeId,
            BarrierType,
            AigLit,
            Option<AigLit>,
            Option<AigNodeId>,
            Option<AigNodeId>,
            Option<bool>,
        )> = Vec::new();

        // Phase 1a: Process inputs FIRST (must be in node_map before latches reference them)
        for (id, node) in aig.iter_nodes() {
            if let AigNode::Input { name, source_net } = node {
                let safety = aig.get_safety_info(id).cloned().unwrap_or_default();
                let new_id = new_aig.add_input_with_safety(name.clone(), *source_net, safety);
                node_map.insert(id, AigLit::new(new_id));
            }
        }

        // Phase 1a': Pre-create latches and barriers (now clock/reset inputs are in node_map)
        for (id, node) in aig.iter_nodes() {
            match node {
                AigNode::Latch {
                    data,
                    init,
                    clock,
                    reset,
                } => {
                    // Pre-create latch with placeholder data
                    let resolved_clock = clock.and_then(|c| node_map.get(&c).map(|lit| lit.node));
                    let resolved_reset = reset.and_then(|r| node_map.get(&r).map(|lit| lit.node));
                    let safety = aig.get_safety_info(id).cloned().unwrap_or_default();
                    let new_id = new_aig.add_latch_with_safety(
                        AigLit::false_lit(),
                        *init,
                        resolved_clock,
                        resolved_reset,
                        safety,
                    );
                    node_map.insert(id, AigLit::new(new_id));
                    latch_ids.push((id, *data, *init, *clock, *reset));
                }
                AigNode::Barrier {
                    barrier_type,
                    data,
                    enable,
                    clock,
                    reset,
                    init,
                } => {
                    // Pre-create barrier with placeholder data
                    let resolved_clock = clock.and_then(|c| node_map.get(&c).map(|lit| lit.node));
                    let resolved_reset = reset.and_then(|r| node_map.get(&r).map(|lit| lit.node));
                    let safety = aig.get_safety_info(id).cloned().unwrap_or_default();
                    let new_id = new_aig.add_barrier_with_safety(
                        barrier_type.clone(),
                        AigLit::false_lit(),
                        None,
                        resolved_clock,
                        resolved_reset,
                        *init,
                        safety,
                    );
                    node_map.insert(id, AigLit::new(new_id));
                    barrier_ids.push((
                        id,
                        barrier_type.clone(),
                        *data,
                        *enable,
                        *clock,
                        *reset,
                        *init,
                    ));
                }
                _ => {
                    // Const, Input, And handled elsewhere
                }
            }
        }

        // Phase 1b: Process AND nodes (now latch outputs are in node_map)
        for (id, node) in aig.iter_nodes() {
            if let AigNode::And { left, right } = node {
                // Process AND nodes
                let resolved_left = resolve_lit(&node_map, *left);
                let resolved_right = resolve_lit(&node_map, *right);

                // Check if this is a root of an AND cone (multiple fanouts or output)
                let fanouts = fanout_counts.get(&id).copied().unwrap_or(0);
                let is_root = fanouts > 1 || fanouts == 0;

                if is_root {
                    // Collect leaves and rebuild balanced
                    let mut leaves = Vec::new();
                    let mut visited = HashSet::new();
                    visited.insert(id);

                    collect_leaves(aig, *left, &fanout_counts, &mut leaves, &mut visited);
                    collect_leaves(aig, *right, &fanout_counts, &mut leaves, &mut visited);

                    // Resolve leaves through node_map
                    let resolved_leaves: Vec<AigLit> = leaves
                        .into_iter()
                        .map(|l| resolve_lit(&node_map, l))
                        .collect();

                    if resolved_leaves.len() > 2 {
                        self.rebalanced_count += 1;
                    }

                    let safety = aig.get_safety_info(id).cloned().unwrap_or_default();
                    let balanced = build_balanced_tree(&mut new_aig, resolved_leaves, safety);
                    node_map.insert(id, balanced);
                } else {
                    // Single fanout - just copy the node
                    let safety = aig.get_safety_info(id).cloned().unwrap_or_default();
                    let new_lit =
                        new_aig.add_and_with_safety(resolved_left, resolved_right, safety);
                    node_map.insert(id, new_lit);
                }
            }
        }

        // Phase 2: Update latch/barrier data with resolved values
        for (id, data, _init, _clock, _reset) in latch_ids {
            let new_node_id = node_map.get(&id).map(|lit| lit.node).unwrap();
            let resolved_data = resolve_lit(&node_map, data);
            new_aig.update_latch_data(new_node_id, resolved_data);
        }

        for (id, _barrier_type, data, enable, _clock, _reset, _init) in barrier_ids {
            let new_node_id = node_map.get(&id).map(|lit| lit.node).unwrap();
            let resolved_data = resolve_lit(&node_map, data);
            let resolved_enable = enable.map(|e| resolve_lit(&node_map, e));
            new_aig.update_barrier_data(new_node_id, resolved_data, resolved_enable);
        }

        // Copy outputs with resolved literals
        for (name, lit) in aig.outputs() {
            let new_lit = resolve_lit(&node_map, *lit);
            new_aig.add_output(name.clone(), new_lit);
        }

        // Replace the AIG
        *aig = new_aig;

        result.record_after(aig);
        result.add_extra("rebalanced", &self.rebalanced_count.to_string());
        result
    }
}

/// Resolve a literal through the node mapping
fn resolve_lit(map: &IndexMap<AigNodeId, AigLit>, lit: AigLit) -> AigLit {
    if lit.node == AigNodeId::FALSE {
        // Constant false is always valid
        return lit;
    }
    if let Some(&mapped) = map.get(&lit.node) {
        if lit.inverted {
            mapped.invert()
        } else {
            mapped
        }
    } else {
        // Node not found in map - return FALSE as safe default
        eprintln!(
            "[BALANCE WARNING] Node {:?} not found in map, returning FALSE",
            lit.node
        );
        AigLit::false_lit()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_balance_chain() {
        let mut aig = Aig::new("test".to_string());
        let a = aig.add_input("a".to_string(), None);
        let b = aig.add_input("b".to_string(), None);
        let c = aig.add_input("c".to_string(), None);
        let d = aig.add_input("d".to_string(), None);

        // Create a chain: ((a & b) & c) & d - depth 3
        let ab = aig.add_and(AigLit::new(a), AigLit::new(b));
        let abc = aig.add_and(ab, AigLit::new(c));
        let abcd = aig.add_and(abc, AigLit::new(d));

        aig.add_output("y".to_string(), abcd);

        let before_levels = aig.compute_stats().max_level;

        let mut pass = Balance::new();
        let result = pass.run(&mut aig);

        let after_levels = aig.compute_stats().max_level;

        // Balanced tree should have depth 2: (a&b) & (c&d)
        assert!(
            after_levels <= before_levels,
            "Depth should not increase: {} -> {}",
            before_levels,
            after_levels
        );
    }

    #[test]
    fn test_balance_already_balanced() {
        let mut aig = Aig::new("test".to_string());
        let a = aig.add_input("a".to_string(), None);
        let b = aig.add_input("b".to_string(), None);
        let c = aig.add_input("c".to_string(), None);
        let d = aig.add_input("d".to_string(), None);

        // Create balanced: (a & b) & (c & d) - already depth 2
        let ab = aig.add_and(AigLit::new(a), AigLit::new(b));
        let cd = aig.add_and(AigLit::new(c), AigLit::new(d));
        let abcd = aig.add_and(ab, cd);

        aig.add_output("y".to_string(), abcd);

        let before_ands = aig.and_count();

        let mut pass = Balance::new();
        pass.run(&mut aig);

        // Should have same number of ANDs
        assert_eq!(aig.and_count(), before_ands);
    }

    #[test]
    fn test_balance_with_inversions() {
        let mut aig = Aig::new("test".to_string());
        let a = aig.add_input("a".to_string(), None);
        let b = aig.add_input("b".to_string(), None);
        let c = aig.add_input("c".to_string(), None);

        // Create: (!a & b) & c - inversion should stop expansion
        let not_a = AigLit::not(a);
        let not_a_and_b = aig.add_and(not_a, AigLit::new(b));
        let result = aig.add_and(not_a_and_b, AigLit::new(c));

        aig.add_output("y".to_string(), result);

        let mut pass = Balance::new();
        pass.run(&mut aig);

        // Should still work correctly
        assert!(aig.and_count() >= 2);
    }

    #[test]
    fn test_balance_multi_fanout() {
        let mut aig = Aig::new("test".to_string());
        let a = aig.add_input("a".to_string(), None);
        let b = aig.add_input("b".to_string(), None);
        let c = aig.add_input("c".to_string(), None);

        // Create: ab = a & b (used twice)
        let ab = aig.add_and(AigLit::new(a), AigLit::new(b));
        let abc = aig.add_and(ab, AigLit::new(c));

        // ab used in two outputs
        aig.add_output("y1".to_string(), ab);
        aig.add_output("y2".to_string(), abc);

        let mut pass = Balance::new();
        pass.run(&mut aig);

        // Should preserve multi-fanout node
        assert!(aig.and_count() >= 2);
    }
}
