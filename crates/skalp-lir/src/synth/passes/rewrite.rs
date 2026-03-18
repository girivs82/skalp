//! AIG Rewriting Pass
//!
//! This pass performs cut-based AIG rewriting, replacing subgraphs with
//! smaller functionally equivalent implementations.
//!
//! # Algorithm
//!
//! For each AND node in topological order:
//! 1. Enumerate K-feasible cuts
//! 2. Compute truth table for each cut
//! 3. Look up optimal implementation in NPN database
//! 4. Compute MFFC gain using reference counting
//! 5. If a smaller implementation exists, apply the rewrite immediately
//!
//! Processing in topological order with incremental reference count updates
//! enables cascading optimizations: earlier rewrites reduce reference counts,
//! exposing larger MFFCs for later nodes.
//!
//! # References
//!
//! - Mishchenko, A., Chatterjee, S., & Brayton, R. (2006). DAG-aware AIG rewriting.

use super::{Pass, PassResult};
use crate::synth::cuts::{CutEnumeration, CutParams};
use crate::synth::npn::NpnDatabase;
use crate::synth::{Aig, AigLit, AigNode, AigNodeId, AigSafetyInfo, BarrierType};
use indexmap::IndexMap;

/// AIG rewriting pass
pub struct Rewrite {
    /// Cut parameters
    cut_params: CutParams,
    /// NPN database (global cached reference)
    npn_db: &'static NpnDatabase,
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
            npn_db: NpnDatabase::global(),
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
            npn_db: NpnDatabase::global(),
            zero_cost: true,
            rewritten_count: 0,
            total_gain: 0,
        }
    }

    /// Create a rewriting pass with custom cut parameters
    pub fn with_params(cut_params: CutParams) -> Self {
        Self {
            cut_params,
            npn_db: NpnDatabase::global(),
            zero_cost: false,
            rewritten_count: 0,
            total_gain: 0,
        }
    }

    /// Create a rewriting pass with custom cut parameters and zero-cost mode
    pub fn with_params_zero_cost(cut_params: CutParams) -> Self {
        Self {
            cut_params,
            npn_db: NpnDatabase::global(),
            zero_cost: true,
            rewritten_count: 0,
            total_gain: 0,
        }
    }

    /// Apply a rewrite by building the new implementation in the AIG
    /// Returns the new literal that should replace the original node
    fn apply_rewrite(&self, aig: &mut Aig, candidate: &RewriteCandidate) -> Option<AigLit> {
        let num_leaves = candidate.cut.leaves.len();
        if num_leaves > 6 {
            return None;
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
                return None;
            }
        }

        // Build the implementation using the gates from NPN database
        let result_lit = if candidate.implementation.gates.is_empty() {
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
                return None;
            }
        } else {
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

            let left_lit = if left_idx < node_lits.len() {
                let mut lit = node_lits[left_idx];
                if left_inv {
                    lit.inverted = !lit.inverted;
                }
                lit
            } else {
                return None;
            };

            let right_lit = if right_idx < node_lits.len() {
                let mut lit = node_lits[right_idx];
                if right_inv {
                    lit.inverted = !lit.inverted;
                }
                lit
            } else {
                return None;
            };

            let new_lit = aig.add_and(left_lit, right_lit);
            node_lits.push(new_lit);
        }

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

/// Count MFFC nodes by decrementing reference counts (ABC-style).
///
/// Returns the number of AND nodes in the Maximum Fanout-Free Cone that would
/// become dead if `root` were removed. Stops at `leaves` (cut boundary).
///
/// **WARNING**: This mutates `ref_counts`. Call `mffc_ref` to restore them.
fn mffc_deref(
    aig: &Aig,
    root: AigNodeId,
    leaves: &[AigNodeId],
    ref_counts: &mut IndexMap<AigNodeId, usize>,
) -> usize {
    if leaves.contains(&root) {
        return 0;
    }
    let (left, right) = match aig.get_node(root) {
        Some(AigNode::And { left, right }) => (*left, *right),
        _ => return 0,
    };

    let mut count = 1; // Count this node

    // Deref left child
    let left_rc = ref_counts.get(&left.node).copied().unwrap_or(0);
    if left_rc > 0 {
        *ref_counts.entry(left.node).or_insert(0) = left_rc - 1;
        if left_rc == 1 {
            count += mffc_deref(aig, left.node, leaves, ref_counts);
        }
    }

    // Deref right child (handle self-loop case where left.node == right.node)
    let right_rc = ref_counts.get(&right.node).copied().unwrap_or(0);
    if right_rc > 0 {
        *ref_counts.entry(right.node).or_insert(0) = right_rc - 1;
        if right_rc == 1 {
            count += mffc_deref(aig, right.node, leaves, ref_counts);
        }
    }

    count
}

/// Restore reference counts after `mffc_deref` (symmetric undo operation).
fn mffc_ref(
    aig: &Aig,
    root: AigNodeId,
    leaves: &[AigNodeId],
    ref_counts: &mut IndexMap<AigNodeId, usize>,
) -> usize {
    if leaves.contains(&root) {
        return 0;
    }
    let (left, right) = match aig.get_node(root) {
        Some(AigNode::And { left, right }) => (*left, *right),
        _ => return 0,
    };

    let mut count = 1;

    // Ref left child (must ref before incrementing so we detect the 0→1 transition)
    let left_rc = ref_counts.get(&left.node).copied().unwrap_or(0);
    if left_rc == 0 {
        count += mffc_ref(aig, left.node, leaves, ref_counts);
    }
    *ref_counts.entry(left.node).or_insert(0) = left_rc + 1;

    // Ref right child
    let right_rc = ref_counts.get(&right.node).copied().unwrap_or(0);
    if right_rc == 0 {
        count += mffc_ref(aig, right.node, leaves, ref_counts);
    }
    *ref_counts.entry(right.node).or_insert(0) = right_rc + 1;

    count
}

/// Rebuild AIG in topological order, discarding unreachable nodes
///
/// This is necessary after applying substitutions because nodes may reference
/// other nodes that come later in the nodes vector. Processing in topological
/// order ensures all references are resolved before they're needed.
///
/// **Must be called after `apply_substitutions()`** whenever new nodes were
/// added at higher IDs (e.g., by rewrite/refactor) — `apply_substitutions`
/// can create forward references that cause downstream passes (DCE, strash)
/// to encounter unresolved node IDs.
/// Compute truth table for a node given specific leaves (inputs).
/// Evaluates all 2^n input combinations.
fn compute_truth_table_for_node(aig: &Aig, node: AigNodeId, leaves: &[AigNodeId]) -> u64 {
    compute_truth_table_for_lit(aig, AigLit::new(node), leaves)
}

/// Compute truth table for a literal given specific leaves.
fn compute_truth_table_for_lit(aig: &Aig, lit: AigLit, leaves: &[AigNodeId]) -> u64 {
    let n = leaves.len();
    if n > 6 { return 0; }
    let num_rows = 1u64 << n;
    let mut tt = 0u64;

    for row in 0..num_rows {
        // Set leaf values
        let mut vals: std::collections::HashMap<AigNodeId, bool> = std::collections::HashMap::new();
        vals.insert(AigNodeId::FALSE, false);
        for (i, &leaf) in leaves.iter().enumerate() {
            vals.insert(leaf, (row >> i) & 1 == 1);
        }
        // Evaluate
        let result = eval_lit(aig, lit, &mut vals);
        if result {
            tt |= 1u64 << row;
        }
    }
    tt
}

fn eval_lit(aig: &Aig, lit: AigLit, vals: &mut std::collections::HashMap<AigNodeId, bool>) -> bool {
    let val = eval_node(aig, lit.node, vals);
    if lit.inverted { !val } else { val }
}

fn eval_node(aig: &Aig, node: AigNodeId, vals: &mut std::collections::HashMap<AigNodeId, bool>) -> bool {
    if let Some(&v) = vals.get(&node) {
        return v;
    }
    let result = match aig.get_node(node) {
        Some(AigNode::And { left, right }) => {
            let l = eval_lit(aig, *left, vals);
            let r = eval_lit(aig, *right, vals);
            l && r
        }
        Some(AigNode::Const) => false,
        _ => false, // Inputs/latches not in leaves — treat as false
    };
    vals.insert(node, result);
    result
}

pub(crate) fn rebuild_aig_topological(aig: &mut Aig) {
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

        // Only add to reachable if the node actually exists in the AIG
        // After optimization, some referenced nodes might not exist anymore
        let node = match aig.get_node(id) {
            Some(n) => n,
            None => {
                continue;
            }
        };

        reachable.insert(id);

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

    // Build new AIG with reachable nodes using Kahn's algorithm (O(V+E))
    let mut new_aig = Aig::new(aig.name.clone());
    let mut node_map: IndexMap<AigNodeId, AigLit> = IndexMap::new();
    node_map.insert(AigNodeId::FALSE, AigLit::false_lit());

    // Compute in-degrees for reachable nodes (only count reachable fanins)
    // IMPORTANT: Latches break cycles in sequential circuits. Their outputs are
    // treated as having in_degree 0 (like primary inputs), and their data inputs
    // are resolved after all combinational logic is processed.
    let mut in_degree: IndexMap<AigNodeId, usize> = IndexMap::new();
    for &id in &reachable {
        in_degree.insert(id, 0);
    }
    for &id in &reachable {
        if let Some(node) = aig.get_node(id) {
            // Skip latches and barriers - they're processed in phase 2
            if node.is_latch() || matches!(node, AigNode::Barrier { .. }) {
                continue;
            }
            for fanin in node.fanins() {
                // Don't count fanins from latch/barrier nodes - they break cycles
                // and their outputs are treated as pseudo-inputs
                let is_latch_or_barrier = aig
                    .get_node(fanin.node)
                    .map(|n| n.is_latch() || matches!(n, AigNode::Barrier { .. }))
                    .unwrap_or(false);
                if reachable.contains(&fanin.node)
                    && fanin.node != AigNodeId::FALSE
                    && !is_latch_or_barrier
                {
                    *in_degree.entry(id).or_insert(0) += 1;
                }
            }
        }
    }

    // Initialize queue with nodes that have no dependencies (in_degree == 0)
    // IMPORTANT: Exclude latches/barriers from initial queue - they must be processed LAST
    // after all combinational logic is in node_map, so their data inputs can be resolved
    use std::collections::VecDeque;
    let mut zero_degree: Vec<AigNodeId> = in_degree
        .iter()
        .filter(|(&id, &deg)| {
            if deg != 0 || id == AigNodeId::FALSE {
                return false;
            }
            // Exclude latches and barriers - process them after combinational logic
            if let Some(node) = aig.get_node(id) {
                !node.is_latch() && !matches!(node, AigNode::Barrier { .. })
            } else {
                false
            }
        })
        .map(|(&id, _)| id)
        .collect();

    // Collect latches and barriers to process after combinational logic
    let mut sequential_nodes: Vec<AigNodeId> = reachable
        .iter()
        .filter(|&&id| {
            if let Some(node) = aig.get_node(id) {
                node.is_latch() || matches!(node, AigNode::Barrier { .. })
            } else {
                false
            }
        })
        .copied()
        .collect();
    sequential_nodes.sort_by_key(|id| id.0); // Deterministic order

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

    // Phase 0: Process input nodes FIRST so clock/reset mappings are available for latches
    let mut input_ids = Vec::new();
    for &id in queue.iter() {
        if let Some(AigNode::Input { name, source_net }) = aig.get_node(id) {
            let safety = aig.get_safety_info(id).cloned().unwrap_or_default();
            let new_id = new_aig.add_input_with_safety(name.clone(), *source_net, safety);
            node_map.insert(id, AigLit::new(new_id));
            input_ids.push(id);
        }
    }
    // Propagate clock/reset input metadata to new AIG
    new_aig.copy_clock_reset_metadata(aig, &node_map);

    // Pre-create latch/barrier outputs as placeholders in node_map
    // This allows AND nodes to resolve references to latch outputs during phase 1
    // The latch data inputs will be properly connected in phase 2
    // Now clock/reset can be correctly resolved using the input node_map entries
    for &id in &sequential_nodes {
        if let Some(node) = aig.get_node(id) {
            match node {
                AigNode::Latch {
                    init, clock, reset, ..
                } => {
                    // Resolve clock/reset through node_map (now populated with inputs)
                    let new_clock = clock.and_then(|c| node_map.get(&c).map(|lit| lit.node));
                    let new_reset = reset.and_then(|r| node_map.get(&r).map(|lit| lit.node));
                    let safety = aig.get_safety_info(id).cloned().unwrap_or_default();
                    // Create latch with false_lit as placeholder data - will be updated in phase 2
                    let new_id = new_aig.add_latch_with_safety(
                        AigLit::false_lit(),
                        *init,
                        new_clock,
                        new_reset,
                        safety,
                    );
                    node_map.insert(id, AigLit::new(new_id));
                }
                AigNode::Barrier {
                    barrier_type,
                    init,
                    clock,
                    reset,
                    ..
                } => {
                    let new_clock = clock.and_then(|c| node_map.get(&c).map(|lit| lit.node));
                    let new_reset = reset.and_then(|r| node_map.get(&r).map(|lit| lit.node));
                    let safety = aig.get_safety_info(id).cloned().unwrap_or_default();
                    // Create barrier with false_lit as placeholder data - will be updated in phase 2
                    let new_id = new_aig.add_barrier_with_safety(
                        barrier_type.clone(),
                        AigLit::false_lit(),
                        None,
                        new_clock,
                        new_reset,
                        *init,
                        safety,
                    );
                    node_map.insert(id, AigLit::new(new_id));
                }
                _ => {}
            }
        }
    }

    // Build fanout lists for efficient updates (sort for determinism)
    let mut fanouts: IndexMap<AigNodeId, Vec<AigNodeId>> = IndexMap::new();
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
        // ALWAYS decrease in-degree of fanouts, even if node already processed
        // This is critical for correctness after pre-processing inputs in phase 0
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

        // Skip if already processed (inputs were pre-created in phase 0)
        if node_map.contains_key(&id) {
            continue;
        }

        let node = match aig.get_node(id) {
            Some(n) => n.clone(),
            None => continue,
        };

        // Add node to new AIG (only inputs and AND gates in phase 1)
        // Latches and barriers are handled in phase 2 after combinational logic
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
            // Latches and barriers are processed in phase 2
            AigNode::Latch { .. } | AigNode::Barrier { .. } => {}
        }
    }

    // Phase 2: Update sequential elements (latches/barriers) with their data inputs
    // The latches were pre-created with placeholder data - now update with real values
    for id in sequential_nodes {
        // Get the pre-created latch's new node ID
        let new_node_id = match node_map.get(&id) {
            Some(lit) => lit.node,
            None => continue, // Shouldn't happen since we pre-created all latches
        };

        let node = match aig.get_node(id) {
            Some(n) => n.clone(),
            None => continue,
        };

        match node {
            AigNode::Latch { data, .. } => {
                let new_data = resolve_lit(&node_map, data);
                new_aig.update_latch_data(new_node_id, new_data);
            }
            AigNode::Barrier { data, enable, .. } => {
                let new_data = resolve_lit(&node_map, data);
                let new_enable = enable.map(|e| resolve_lit(&node_map, e));
                // For barriers, we need to update data and enable
                // Using update_latch_data as a workaround (barriers use same data field)
                new_aig.update_latch_data(new_node_id, new_data);
                // TODO: Also update enable if needed
            }
            _ => {} // Inputs and ANDs already processed
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
        panic!(
            "resolve_lit: node {:?} not found in map (map has {} entries, max id {:?}) — \
             likely forward reference from apply_substitutions without topological rebuild",
            lit.node,
            map.len(),
            map.keys().max_by_key(|k| k.0),
        );
    }
}

/// Compute fanout counts for all nodes
fn compute_fanout_counts(aig: &Aig) -> IndexMap<AigNodeId, usize> {
    let mut counts: IndexMap<AigNodeId, usize> = IndexMap::new();

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

impl Rewrite {
    /// Regular rewrite: pre-compute cuts once, collect rewrites, batch apply.
    fn run_regular(&mut self, aig: &mut Aig) {
        let cuts = CutEnumeration::enumerate(aig, self.cut_params.clone());
        let mut ref_counts = compute_fanout_counts(aig);

        let topo_nodes: Vec<AigNodeId> = aig
            .iter_nodes()
            .filter_map(|(id, node)| {
                if matches!(node, AigNode::And { .. }) { Some(id) } else { None }
            })
            .collect();

        let mut subst_map: IndexMap<AigNodeId, AigLit> = IndexMap::new();

        for &node_id in &topo_nodes {
            if subst_map.contains_key(&node_id) {
                continue;
            }

            let cut_set = match cuts.get_cuts(node_id) {
                Some(cs) => cs,
                None => continue,
            };

            let mut best_gain = 1i32;
            let mut best_candidate: Option<RewriteCandidate> = None;

            for cut in &cut_set.cuts {
                if cut.size() <= 1 { continue; }
                if cut.leaves.iter().any(|l| subst_map.contains_key(l)) { continue; }

                let (impl_, canonical) = match self.npn_db.lookup(cut.truth_table, cut.size()) {
                    Some(x) => x,
                    None => continue,
                };

                let mffc_size = mffc_deref(aig, node_id, &cut.leaves, &mut ref_counts);
                mffc_ref(aig, node_id, &cut.leaves, &mut ref_counts);
                let gain = mffc_size as i32 - impl_.and_count as i32;

                if gain >= best_gain {
                    best_gain = gain;
                    best_candidate = Some(RewriteCandidate {
                        node: node_id, cut: cut.clone(), canonical, implementation: impl_, gain,
                    });
                }
            }

            if let Some(candidate) = best_candidate {
                if let Some(new_lit) = self.apply_rewrite(aig, &candidate) {
                    // Verify truth tables match before committing this rewrite.
                    // The cut truth table was computed during enumeration, but
                    // intermediate rewrites may have changed the AIG structure
                    // (new AND nodes added by prior apply_rewrite calls).
                    if new_lit.node != node_id {
                        let old_tt = compute_truth_table_for_node(aig, node_id, &candidate.cut.leaves);
                        let new_tt = compute_truth_table_for_lit(aig, new_lit, &candidate.cut.leaves);
                        if old_tt != new_tt {
                            continue; // Truth tables diverged — skip this rewrite
                        }
                    }
                    subst_map.insert(node_id, new_lit);
                    self.rewritten_count += 1;
                    self.total_gain += candidate.gain;
                }
            }
        }

        if !subst_map.is_empty() {
            aig.apply_substitutions(&subst_map);
            rebuild_aig_topological(aig);
        }
    }

    /// Zero-cost rewrite: apply each rewrite immediately and re-enumerate cuts
    /// after each substitution to keep truth tables consistent.
    fn run_zero_cost(&mut self, aig: &mut Aig) {
        let mut any_rewrites = false;

        // Iterate: each pass re-enumerates cuts on the current AIG state.
        // We do a single pass over all AND nodes.
        let cuts = CutEnumeration::enumerate(aig, self.cut_params.clone());
        let mut ref_counts = compute_fanout_counts(aig);

        let topo_nodes: Vec<AigNodeId> = aig
            .iter_nodes()
            .filter_map(|(id, node)| {
                if matches!(node, AigNode::And { .. }) { Some(id) } else { None }
            })
            .collect();

        let mut rewritten: std::collections::HashSet<AigNodeId> = std::collections::HashSet::new();
        // Collect all _z rewrites first (scan phase), then apply one at a time.
        // This ensures cuts are evaluated on a consistent AIG state.
        struct ZRewriteEntry {
            node: AigNodeId,
            cut: crate::synth::cuts::Cut,
            canonical: crate::synth::npn::NpnCanonical,
            implementation: crate::synth::npn::NpnImplementation,
            gain: i32,
        }
        let mut entries: Vec<ZRewriteEntry> = Vec::new();

        for &node_id in &topo_nodes {
            if rewritten.contains(&node_id) { continue; }

            let cut_set = match cuts.get_cuts(node_id) {
                Some(cs) => cs,
                None => continue,
            };

            let mut best_gain = 0i32;
            let mut best_candidate: Option<ZRewriteEntry> = None;

            for cut in &cut_set.cuts {
                if cut.size() <= 1 { continue; }
                if cut.leaves.iter().any(|l| rewritten.contains(l)) { continue; }

                let (impl_, canonical) = match self.npn_db.lookup(cut.truth_table, cut.size()) {
                    Some(x) => x,
                    None => continue,
                };

                let mffc_size = mffc_deref(aig, node_id, &cut.leaves, &mut ref_counts);
                mffc_ref(aig, node_id, &cut.leaves, &mut ref_counts);
                let gain = mffc_size as i32 - impl_.and_count as i32;

                if gain >= best_gain {
                    best_gain = gain;
                    best_candidate = Some(ZRewriteEntry {
                        node: node_id, cut: cut.clone(), canonical, implementation: impl_, gain,
                    });
                }
            }

            if let Some(entry) = best_candidate {
                rewritten.insert(node_id);
                entries.push(entry);
            }
        }

        // Apply each rewrite individually with strash rebuild between each.
        // Verify truth tables before applying: previous substitutions may change
        // the AIG structure, making the pre-computed truth table stale. Only apply
        // rewrites that are verified correct on the current AIG state.
        for entry in &entries {
            let candidate = RewriteCandidate {
                node: entry.node,
                cut: entry.cut.clone(),
                canonical: entry.canonical.clone(),
                implementation: entry.implementation.clone(),
                gain: entry.gain,
            };
            if let Some(new_lit) = self.apply_rewrite(aig, &candidate) {
                if new_lit.node != entry.node {
                    // Verify truth tables match on the current AIG state
                    let old_tt = compute_truth_table_for_node(aig, entry.node, &entry.cut.leaves);
                    let new_tt = compute_truth_table_for_lit(aig, new_lit, &entry.cut.leaves);
                    if old_tt != new_tt {
                        continue; // Stale cut — skip
                    }

                    let mut single = IndexMap::new();
                    single.insert(entry.node, new_lit);
                    aig.apply_substitutions(&single);
                    aig.rebuild_strash();
                    any_rewrites = true;
                    self.rewritten_count += 1;
                    self.total_gain += entry.gain;
                }
            }
        }

        if any_rewrites {
            rebuild_aig_topological(aig);
        }
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

        if self.zero_cost {
            self.run_zero_cost(aig);
        } else {
            self.run_regular(aig);
        }

        result.record_after(aig);
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
    fn test_mffc_deref_ref_roundtrip() {
        // Create a simple AIG: y = (a & b) & c
        let mut aig = Aig::new("test".to_string());
        let a = aig.add_input("a".to_string(), None);
        let b = aig.add_input("b".to_string(), None);
        let c = aig.add_input("c".to_string(), None);

        let ab = aig.add_and(AigLit::new(a), AigLit::new(b));
        let abc = aig.add_and(ab, AigLit::new(c));
        aig.add_output("y".to_string(), abc);

        let mut ref_counts = compute_fanout_counts(&aig);
        let ref_counts_before = ref_counts.clone();

        // Deref from abc with leaves {a, b, c}
        let mffc = mffc_deref(&aig, abc.node, &[a, b, c], &mut ref_counts);
        assert_eq!(mffc, 2); // ab and abc are both in the MFFC

        // Ref to restore
        mffc_ref(&aig, abc.node, &[a, b, c], &mut ref_counts);
        assert_eq!(ref_counts, ref_counts_before);
    }

    #[test]
    fn test_mffc_shared_node() {
        // Create AIG where ab is shared: y1 = (a & b) & c, y2 = (a & b) & d
        let mut aig = Aig::new("test".to_string());
        let a = aig.add_input("a".to_string(), None);
        let b = aig.add_input("b".to_string(), None);
        let c = aig.add_input("c".to_string(), None);
        let d = aig.add_input("d".to_string(), None);

        let ab = aig.add_and(AigLit::new(a), AigLit::new(b));
        let abc = aig.add_and(ab, AigLit::new(c));
        let abd = aig.add_and(ab, AigLit::new(d));
        aig.add_output("y1".to_string(), abc);
        aig.add_output("y2".to_string(), abd);

        let mut ref_counts = compute_fanout_counts(&aig);

        // MFFC of abc with leaves {a, b, c}: only abc itself (ab is shared, fanout=2)
        let mffc = mffc_deref(&aig, abc.node, &[a, b, c], &mut ref_counts);
        mffc_ref(&aig, abc.node, &[a, b, c], &mut ref_counts);
        assert_eq!(mffc, 1); // Only abc, not ab (ab has fanout 2)
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

        let mut pass = Rewrite::new();
        let result = pass.run(&mut aig);

        // The rewrite should at least complete and find some candidates
        assert!(result.ands_after > 0);
    }
}
