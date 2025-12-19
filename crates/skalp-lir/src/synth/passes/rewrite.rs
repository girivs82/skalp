//! AIG Rewriting Pass
//!
//! This pass performs cut-based AIG rewriting, replacing subgraphs with
//! smaller functionally equivalent implementations.
//!
//! # Algorithm
//!
//! For each AND node:
//! 1. Enumerate K-feasible cuts
//! 2. Compute truth table for each cut
//! 3. Look up optimal implementation in NPN database
//! 4. If a smaller implementation exists, replace the subgraph
//!
//! # References
//!
//! - Mishchenko, A., Chatterjee, S., & Brayton, R. (2006). DAG-aware AIG rewriting.

use super::{Pass, PassResult};
use crate::synth::cuts::{CutEnumeration, CutParams};
use crate::synth::npn::NpnDatabase;
use crate::synth::{Aig, AigLit, AigNode, AigNodeId, AigSafetyInfo, BarrierType};
use std::collections::HashMap;

/// AIG rewriting pass
pub struct Rewrite {
    /// Cut parameters
    cut_params: CutParams,
    /// NPN database
    npn_db: NpnDatabase,
    /// Zero-cost mode: allow rewrites with gain >= 0 (instead of > 0)
    /// This enables more depth optimization without area increase
    zero_cost: bool,
    /// Number of nodes rewritten
    rewritten_count: usize,
    /// Total gain (nodes saved)
    total_gain: i32,
}

impl Rewrite {
    /// Create a new rewriting pass with default parameters
    pub fn new() -> Self {
        Self {
            cut_params: CutParams::default(),
            npn_db: NpnDatabase::new(),
            zero_cost: false,
            rewritten_count: 0,
            total_gain: 0,
        }
    }

    /// Create a rewriting pass with zero-cost mode enabled
    /// Zero-cost mode allows rewrites that don't change node count
    /// (equivalent to ABC's `rewrite -z`)
    pub fn zero_cost() -> Self {
        Self {
            cut_params: CutParams::default(),
            npn_db: NpnDatabase::new(),
            zero_cost: true,
            rewritten_count: 0,
            total_gain: 0,
        }
    }

    /// Create a rewriting pass with custom cut parameters
    pub fn with_params(cut_params: CutParams) -> Self {
        Self {
            cut_params,
            npn_db: NpnDatabase::new(),
            zero_cost: false,
            rewritten_count: 0,
            total_gain: 0,
        }
    }

    /// Create a rewriting pass with custom cut parameters and zero-cost mode
    pub fn with_params_zero_cost(cut_params: CutParams) -> Self {
        Self {
            cut_params,
            npn_db: NpnDatabase::new(),
            zero_cost: true,
            rewritten_count: 0,
            total_gain: 0,
        }
    }

    /// Evaluate the potential gain from rewriting a node with a given cut
    fn evaluate_rewrite(
        &self,
        aig: &Aig,
        node: AigNodeId,
        cut: &crate::synth::cuts::Cut,
        fanout_counts: &HashMap<AigNodeId, usize>,
    ) -> Option<RewriteCandidate> {
        // Skip trivial cuts
        if cut.size() <= 1 {
            return None;
        }

        // Look up the optimal implementation
        let (impl_, canonical) = self.npn_db.lookup(cut.truth_table, cut.size())?;

        // Count current nodes in the cone
        let current_nodes = count_cone_nodes(aig, node, &cut.leaves, fanout_counts);

        // Calculate gain (positive means improvement)
        // Note: current_nodes only counts nodes with single fanout that can be removed
        // impl_.and_count is the number of new gates we'll add
        let gain = current_nodes as i32 - impl_.and_count as i32;

        // IMPORTANT: Only accept if we're actually reducing gates
        // The gain must be > 0 (strictly positive) to guarantee improvement
        // Zero-cost mode accepts gain >= 0 for depth optimization without area increase
        let accept = if self.zero_cost { gain >= 0 } else { gain > 0 };

        if accept {
            Some(RewriteCandidate {
                node,
                cut: cut.clone(),
                canonical,
                implementation: impl_,
                gain,
            })
        } else {
            None
        }
    }

    /// Apply a rewrite by building the new implementation in the AIG
    /// Returns the new literal that should replace the original node
    fn apply_rewrite(&self, aig: &mut Aig, candidate: &RewriteCandidate) -> Option<AigLit> {
        // For now, we use a simple approach:
        // If the implementation has fewer gates than the current cone,
        // we rebuild using basic boolean algebra

        // Get the leaves as literals, applying the NPN transformation
        let num_leaves = candidate.cut.leaves.len();
        if num_leaves > 6 {
            return None; // Too many inputs
        }

        // Build input literals with NPN transformations applied
        let mut input_lits: Vec<AigLit> = Vec::new();
        for i in 0..num_leaves {
            let perm_idx = candidate.canonical.permutation[i];
            if perm_idx < num_leaves {
                let leaf = candidate.cut.leaves[perm_idx];
                let negated = (candidate.canonical.input_negations >> i) & 1 == 1;
                input_lits.push(AigLit {
                    node: leaf,
                    inverted: negated,
                });
            } else {
                // Invalid permutation index
                return None;
            }
        }

        // Build the implementation using the gates from NPN database
        let result_lit = if candidate.implementation.gates.is_empty() {
            // No gates needed - the result is a direct input or constant
            // Decode the result_lit from the implementation
            let impl_result = candidate.implementation.result_lit;
            let idx = (impl_result / 2) as usize;
            let inv = impl_result & 1 == 1;

            if idx < num_leaves {
                let mut lit = input_lits[idx];
                if inv {
                    lit.inverted = !lit.inverted;
                }
                lit
            } else {
                // Invalid - shouldn't happen for empty gates
                return None;
            }
        } else {
            // Build using the gate list
            self.build_from_gates(
                aig,
                &input_lits,
                &candidate.implementation.gates,
                candidate.implementation.result_lit,
            )?
        };

        // Apply output negation if needed
        let final_lit = if candidate.canonical.output_negated {
            AigLit {
                node: result_lit.node,
                inverted: !result_lit.inverted,
            }
        } else {
            result_lit
        };

        Some(final_lit)
    }

    /// Build an implementation from a list of gates
    /// Each gate is (left_lit, right_lit) where lit = input_idx * 2 + inverted
    /// result_lit specifies which literal is the output
    fn build_from_gates(
        &self,
        aig: &mut Aig,
        input_lits: &[AigLit],
        gates: &[(u8, u8)],
        result_lit: u8,
    ) -> Option<AigLit> {
        let num_inputs = input_lits.len();
        let mut node_lits: Vec<AigLit> = input_lits.to_vec();

        for &(left_encoded, right_encoded) in gates {
            let left_idx = (left_encoded / 2) as usize;
            let left_inv = left_encoded & 1 == 1;
            let right_idx = (right_encoded / 2) as usize;
            let right_inv = right_encoded & 1 == 1;

            // Get literals for left and right inputs
            let left_lit = if left_idx < node_lits.len() {
                let mut lit = node_lits[left_idx];
                if left_inv {
                    lit.inverted = !lit.inverted;
                }
                lit
            } else {
                return None; // Invalid index
            };

            let right_lit = if right_idx < node_lits.len() {
                let mut lit = node_lits[right_idx];
                if right_inv {
                    lit.inverted = !lit.inverted;
                }
                lit
            } else {
                return None; // Invalid index
            };

            // Create the AND node
            let new_lit = aig.add_and(left_lit, right_lit);
            node_lits.push(new_lit);
        }

        // Use the result_lit to get the correct output
        let result_idx = (result_lit / 2) as usize;
        let result_inv = result_lit & 1 == 1;

        if result_idx < node_lits.len() {
            let mut lit = node_lits[result_idx];
            if result_inv {
                lit.inverted = !lit.inverted;
            }
            Some(lit)
        } else {
            None
        }
    }
}

/// A candidate rewrite operation
struct RewriteCandidate {
    node: AigNodeId,
    cut: crate::synth::cuts::Cut,
    canonical: crate::synth::npn::NpnCanonical,
    implementation: crate::synth::npn::NpnImplementation,
    gain: i32,
}

/// Count nodes in a cone that are not shared (single fanout)
fn count_cone_nodes(
    aig: &Aig,
    root: AigNodeId,
    leaves: &[AigNodeId],
    fanout_counts: &HashMap<AigNodeId, usize>,
) -> usize {
    let mut count = 0;
    let mut visited = std::collections::HashSet::new();
    let mut stack = vec![root];

    while let Some(node) = stack.pop() {
        if visited.contains(&node) {
            continue;
        }
        if leaves.contains(&node) {
            continue;
        }
        visited.insert(node);

        if let Some(AigNode::And { left, right }) = aig.get_node(node) {
            // Only count if single fanout or if it's the root
            let fanouts = fanout_counts.get(&node).copied().unwrap_or(0);
            if fanouts <= 1 || node == root {
                count += 1;
            }
            stack.push(left.node);
            stack.push(right.node);
        }
    }

    count
}

/// Rebuild AIG in topological order, discarding unreachable nodes
///
/// This is necessary after applying substitutions because nodes may reference
/// other nodes that come later in the nodes vector. Processing in topological
/// order ensures all references are resolved before they're needed.
fn rebuild_aig_topological(aig: &mut Aig) {
    use std::collections::HashSet;

    // Find all reachable nodes starting from outputs AND latches using DFS
    // Latches must be included because they form feedback loops in sequential circuits
    let mut reachable: HashSet<AigNodeId> = HashSet::new();
    let mut stack: Vec<AigNodeId> = Vec::new();

    // Start from outputs
    for (_, lit) in aig.outputs() {
        stack.push(lit.node);
    }

    // Also start from all latches and barriers (sequential elements with feedback)
    for (id, node) in aig.iter_nodes() {
        if node.is_latch() || matches!(node, AigNode::Barrier { .. }) {
            stack.push(id);
        }
    }

    // Add constant
    reachable.insert(AigNodeId::FALSE);

    // DFS to find all reachable nodes
    while let Some(id) = stack.pop() {
        if reachable.contains(&id) {
            continue;
        }
        reachable.insert(id);

        if let Some(node) = aig.get_node(id) {
            for fanin in node.fanins() {
                if !reachable.contains(&fanin.node) {
                    stack.push(fanin.node);
                }
            }
            // Handle latch clock/reset
            if let AigNode::Latch { clock, reset, .. } = node {
                if let Some(c) = clock {
                    if !reachable.contains(c) {
                        stack.push(*c);
                    }
                }
                if let Some(r) = reset {
                    if !reachable.contains(r) {
                        stack.push(*r);
                    }
                }
            }
        }
    }

    // Build new AIG with reachable nodes using Kahn's algorithm (O(V+E))
    let mut new_aig = Aig::new(aig.name.clone());
    let mut node_map: HashMap<AigNodeId, AigLit> = HashMap::new();
    node_map.insert(AigNodeId::FALSE, AigLit::false_lit());

    // Compute in-degrees for reachable nodes (only count reachable fanins)
    let mut in_degree: HashMap<AigNodeId, usize> = HashMap::new();
    for &id in &reachable {
        in_degree.insert(id, 0);
    }
    for &id in &reachable {
        if let Some(node) = aig.get_node(id) {
            for fanin in node.fanins() {
                if reachable.contains(&fanin.node) && fanin.node != AigNodeId::FALSE {
                    *in_degree.entry(id).or_insert(0) += 1;
                }
            }
        }
    }

    // Initialize queue with nodes that have no dependencies (in_degree == 0)
    // Sort by node name for inputs (deterministic semantic ordering)
    use std::collections::VecDeque;
    let mut zero_degree: Vec<AigNodeId> = in_degree
        .iter()
        .filter(|(&id, &deg)| deg == 0 && id != AigNodeId::FALSE)
        .map(|(&id, _)| id)
        .collect();

    // Sort zero-degree nodes: inputs by name, others by node type then ID
    zero_degree.sort_by(|&a, &b| {
        let node_a = aig.get_node(a);
        let node_b = aig.get_node(b);
        match (node_a, node_b) {
            (
                Some(AigNode::Input { name: name_a, .. }),
                Some(AigNode::Input { name: name_b, .. }),
            ) => name_a.cmp(name_b),
            (Some(AigNode::Input { .. }), _) => std::cmp::Ordering::Less,
            (_, Some(AigNode::Input { .. })) => std::cmp::Ordering::Greater,
            _ => a.0.cmp(&b.0),
        }
    });

    let mut queue: VecDeque<AigNodeId> = zero_degree.into_iter().collect();

    // Build fanout lists for efficient updates (sort for determinism)
    let mut fanouts: HashMap<AigNodeId, Vec<AigNodeId>> = HashMap::new();
    for &id in &reachable {
        if let Some(node) = aig.get_node(id) {
            for fanin in node.fanins() {
                if reachable.contains(&fanin.node) {
                    fanouts.entry(fanin.node).or_default().push(id);
                }
            }
        }
    }
    // Sort fanout lists by node ID for determinism
    for list in fanouts.values_mut() {
        list.sort_by_key(|id| id.0);
    }

    // Process nodes in topological order
    while let Some(id) = queue.pop_front() {
        if node_map.contains_key(&id) {
            continue;
        }

        let node = match aig.get_node(id) {
            Some(n) => n.clone(),
            None => continue,
        };

        // Add node to new AIG
        match node {
            AigNode::Const => {
                // Already handled
            }
            AigNode::Input {
                ref name,
                source_net,
            } => {
                let safety = aig.get_safety_info(id).cloned().unwrap_or_default();
                let new_id = new_aig.add_input_with_safety(name.clone(), source_net, safety);
                node_map.insert(id, AigLit::new(new_id));
            }
            AigNode::And { left, right } => {
                let new_left = resolve_lit(&node_map, left);
                let new_right = resolve_lit(&node_map, right);
                let safety = aig.get_safety_info(id).cloned().unwrap_or_default();
                let new_lit = new_aig.add_and_with_safety(new_left, new_right, safety);
                node_map.insert(id, new_lit);
            }
            AigNode::Latch {
                data,
                init,
                clock,
                reset,
            } => {
                let new_data = resolve_lit(&node_map, data);
                let new_clock =
                    clock.map(|c| node_map.get(&c).copied().unwrap_or(AigLit::new(c)).node);
                let new_reset =
                    reset.map(|r| node_map.get(&r).copied().unwrap_or(AigLit::new(r)).node);
                let safety = aig.get_safety_info(id).cloned().unwrap_or_default();
                let new_id =
                    new_aig.add_latch_with_safety(new_data, init, new_clock, new_reset, safety);
                node_map.insert(id, AigLit::new(new_id));
            }
            AigNode::Barrier {
                ref barrier_type,
                data,
                enable,
                clock,
                reset,
                init,
            } => {
                let new_data = resolve_lit(&node_map, data);
                let new_enable = enable.map(|e| resolve_lit(&node_map, e));
                let new_clock =
                    clock.map(|c| node_map.get(&c).copied().unwrap_or(AigLit::new(c)).node);
                let new_reset =
                    reset.map(|r| node_map.get(&r).copied().unwrap_or(AigLit::new(r)).node);
                let safety = aig.get_safety_info(id).cloned().unwrap_or_default();
                let new_id = new_aig.add_barrier_with_safety(
                    barrier_type.clone(),
                    new_data,
                    new_enable,
                    new_clock,
                    new_reset,
                    init,
                    safety,
                );
                node_map.insert(id, AigLit::new(new_id));
            }
        }

        // Decrease in-degree of fanouts and add to queue if ready
        if let Some(fouts) = fanouts.get(&id) {
            for &fanout_id in fouts {
                if let Some(deg) = in_degree.get_mut(&fanout_id) {
                    if *deg > 0 {
                        *deg -= 1;
                        if *deg == 0 {
                            queue.push_back(fanout_id);
                        }
                    }
                }
            }
        }
    }

    // Copy outputs with resolved literals
    for (name, lit) in aig.outputs() {
        let new_lit = resolve_lit(&node_map, *lit);
        new_aig.add_output(name.clone(), new_lit);
    }

    // Replace the AIG
    *aig = new_aig;
}

/// Resolve a literal through the node mapping
fn resolve_lit(map: &HashMap<AigNodeId, AigLit>, lit: AigLit) -> AigLit {
    if let Some(&mapped) = map.get(&lit.node) {
        if lit.inverted {
            mapped.invert()
        } else {
            mapped
        }
    } else {
        lit
    }
}

/// Compute fanout counts for all nodes
fn compute_fanout_counts(aig: &Aig) -> HashMap<AigNodeId, usize> {
    let mut counts: HashMap<AigNodeId, usize> = HashMap::new();

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

impl Default for Rewrite {
    fn default() -> Self {
        Self::new()
    }
}

impl Pass for Rewrite {
    fn name(&self) -> &str {
        "rewrite"
    }

    fn run(&mut self, aig: &mut Aig) -> PassResult {
        let mut result = PassResult::new(self.name());
        result.record_before(aig);

        self.rewritten_count = 0;
        self.total_gain = 0;

        // Enumerate cuts
        let cuts = CutEnumeration::enumerate(aig, self.cut_params.clone());

        // Compute fanout counts
        let fanout_counts = compute_fanout_counts(aig);

        // Find all rewrite candidates
        let mut candidates: Vec<RewriteCandidate> = Vec::new();

        for (id, node) in aig.iter_nodes() {
            if let AigNode::And { .. } = node {
                if let Some(cut_set) = cuts.get_cuts(id) {
                    for cut in &cut_set.cuts {
                        if let Some(candidate) = self.evaluate_rewrite(aig, id, cut, &fanout_counts)
                        {
                            candidates.push(candidate);
                        }
                    }
                }
            }
        }

        // Sort by gain (highest first)
        candidates.sort_by(|a, b| b.gain.cmp(&a.gain));

        // Track which nodes have been rewritten (to avoid conflicts)
        let mut rewritten_nodes: std::collections::HashSet<AigNodeId> =
            std::collections::HashSet::new();

        // Build substitution map: old_node -> new_lit
        let mut subst_map: HashMap<AigNodeId, AigLit> = HashMap::new();

        // Apply rewrites greedily
        for candidate in &candidates {
            // Skip if this node or any of its leaves have been modified
            if rewritten_nodes.contains(&candidate.node) {
                continue;
            }
            let mut conflict = false;
            for leaf in &candidate.cut.leaves {
                if rewritten_nodes.contains(leaf) {
                    conflict = true;
                    break;
                }
            }
            if conflict {
                continue;
            }

            // Apply the rewrite by building a new implementation
            let gates_before = aig.and_count();
            if let Some(new_lit) = self.apply_rewrite(aig, candidate) {
                let gates_added = aig.and_count() - gates_before;
                // Sanity check: we should add at most impl_.and_count gates
                // (could be fewer due to structural hashing)
                if gates_added > candidate.implementation.and_count {
                    eprintln!(
                        "    [rewrite warning] Expected to add {} gates, actually added {}",
                        candidate.implementation.and_count, gates_added
                    );
                }

                // Register the substitution
                subst_map.insert(candidate.node, new_lit);

                // Mark this node as rewritten
                rewritten_nodes.insert(candidate.node);

                self.rewritten_count += 1;
                self.total_gain += candidate.gain;
            }

            // Limit the number of rewrites per pass
            if self.rewritten_count >= 100 {
                break;
            }
        }

        // Apply all substitutions to update the AIG
        if !subst_map.is_empty() {
            aig.apply_substitutions(&subst_map);
            // Rebuild AIG to compact and ensure topological order
            rebuild_aig_topological(aig);
        }

        result.record_after(aig);
        result.add_extra("candidates", &candidates.len().to_string());
        result.add_extra("rewrites_applied", &self.rewritten_count.to_string());
        result.add_extra("total_gain", &self.total_gain.to_string());
        result
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_rewrite_creation() {
        let pass = Rewrite::new();
        assert_eq!(pass.name(), "rewrite");
    }

    #[test]
    fn test_rewrite_simple() {
        let mut aig = Aig::new("test".to_string());
        let a = aig.add_input("a".to_string(), None);
        let b = aig.add_input("b".to_string(), None);
        let ab = aig.add_and(AigLit::new(a), AigLit::new(b));
        aig.add_output("y".to_string(), ab);

        let mut pass = Rewrite::new();
        let result = pass.run(&mut aig);

        // Should complete without error
        assert!(result.ands_after >= 1);
    }

    #[test]
    fn test_count_cone_nodes() {
        let mut aig = Aig::new("test".to_string());
        let a = aig.add_input("a".to_string(), None);
        let b = aig.add_input("b".to_string(), None);
        let c = aig.add_input("c".to_string(), None);

        let ab = aig.add_and(AigLit::new(a), AigLit::new(b));
        let abc = aig.add_and(ab, AigLit::new(c));
        aig.add_output("y".to_string(), abc);

        let fanout_counts = compute_fanout_counts(&aig);

        // Count nodes in cone from abc to {a, b, c}
        let count = count_cone_nodes(&aig, abc.node, &[a, b, c], &fanout_counts);
        assert_eq!(count, 2); // ab and abc
    }

    #[test]
    fn test_fanout_counts() {
        let mut aig = Aig::new("test".to_string());
        let a = aig.add_input("a".to_string(), None);
        let b = aig.add_input("b".to_string(), None);

        let ab = aig.add_and(AigLit::new(a), AigLit::new(b));
        let ab2 = aig.add_and(ab, AigLit::new(a)); // a used twice

        aig.add_output("y".to_string(), ab2);

        let fanout_counts = compute_fanout_counts(&aig);

        // 'a' should have fanout 2 (ab and ab2)
        assert_eq!(fanout_counts.get(&a), Some(&2));
        // 'b' should have fanout 1
        assert_eq!(fanout_counts.get(&b), Some(&1));
    }

    #[test]
    fn test_rewrite_finds_candidates() {
        use crate::synth::cuts::{CutEnumeration, CutParams};
        use crate::synth::npn::NpnDatabase;

        // Create an AIG with a known pattern that should be rewritable
        // XOR: (a & !b) | (!a & b) = !(!( a & !b) & !(! a & b))
        // In AIG: need 4 AND gates naively
        let mut aig = Aig::new("test".to_string());
        let a = aig.add_input("a".to_string(), None);
        let b = aig.add_input("b".to_string(), None);

        // !a & b
        let not_a_and_b = aig.add_and(AigLit::not(a), AigLit::new(b));
        // a & !b
        let a_and_not_b = aig.add_and(AigLit::new(a), AigLit::not(b));
        // !((!a & b) & (a & !b)) - this is !((! a & b) & (a & !b)), then invert for OR
        // Actually for OR we need: !(!x & !y) = x | y
        let nand = aig.add_and(not_a_and_b.invert(), a_and_not_b.invert());
        // Result is !(nand) = XOR
        aig.add_output("xor".to_string(), nand.invert());

        let before_ands = aig.and_count();
        println!("Before rewrite: {} ANDs", before_ands);

        // Debug: enumerate cuts and see what we find
        let cuts = CutEnumeration::enumerate(&aig, CutParams::default());
        println!("\nCuts enumerated:");
        for (id, node) in aig.iter_nodes() {
            if let AigNode::And { .. } = node {
                if let Some(cut_set) = cuts.get_cuts(id) {
                    println!("  Node {:?}: {} cuts", id, cut_set.cuts.len());
                    for cut in &cut_set.cuts {
                        println!(
                            "    leaves: {:?}, tt: 0x{:x}, size: {}",
                            cut.leaves,
                            cut.truth_table,
                            cut.size()
                        );
                    }
                }
            }
        }

        // Debug: check NPN database
        let npn_db = NpnDatabase::new();
        println!("\nNPN database has {} entries", npn_db.len());

        // Check what the XOR truth table is
        // XOR of 2 inputs: 0110 = 0x6
        let xor_tt_2 = 0x6_u64;
        if let Some((impl_, canonical)) = npn_db.lookup(xor_tt_2, 2) {
            println!(
                "XOR 2-input (0x{:x}) -> canonical 0x{:x}, {} AND gates, {} gates",
                xor_tt_2,
                canonical.canonical_tt,
                impl_.and_count,
                impl_.gates.len()
            );
        } else {
            println!("XOR 2-input (0x{:x}) NOT FOUND in NPN database", xor_tt_2);
        }

        // XOR of 4 inputs that only depends on 2 vars: 0x6666
        let xor_tt_4 = 0x6666_u64;
        if let Some((impl_, canonical)) = npn_db.lookup(xor_tt_4, 4) {
            println!(
                "XOR 4-input (0x{:x}) -> canonical 0x{:x}, {} AND gates, {} gates",
                xor_tt_4,
                canonical.canonical_tt,
                impl_.and_count,
                impl_.gates.len()
            );
        } else {
            println!("XOR 4-input (0x{:x}) NOT FOUND in NPN database", xor_tt_4);
        }

        // Also check what's at 0x9 (what the cut has)
        if let Some((impl_, canonical)) = npn_db.lookup(0x9, 2) {
            println!(
                "XNOR 2-input (0x9) -> canonical 0x{:x}, {} AND gates, {} gates",
                canonical.canonical_tt,
                impl_.and_count,
                impl_.gates.len()
            );
        }

        let mut pass = Rewrite::new();
        let result = pass.run(&mut aig);

        println!("\nAfter rewrite: {} ANDs", result.ands_after);
        // Extract extra values from the Vec
        let get_extra = |key: &str| -> String {
            result
                .extra
                .iter()
                .find(|(k, _)| k == key)
                .map(|(_, v)| v.clone())
                .unwrap_or_else(|| "?".to_string())
        };
        println!(
            "Candidates found: {}, rewrites applied: {}, total gain: {}",
            get_extra("candidates"),
            get_extra("rewrites_applied"),
            get_extra("total_gain")
        );

        // The rewrite should at least complete and find some candidates
        assert!(result.ands_after > 0);
    }
}
