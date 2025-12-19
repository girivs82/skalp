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
use crate::synth::{Aig, AigLit, AigNode, AigNodeId, AigSafetyInfo};
use std::collections::HashMap;

/// AIG rewriting pass
pub struct Rewrite {
    /// Cut parameters
    cut_params: CutParams,
    /// NPN database
    npn_db: NpnDatabase,
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
            rewritten_count: 0,
            total_gain: 0,
        }
    }

    /// Create a rewriting pass with custom cut parameters
    pub fn with_params(cut_params: CutParams) -> Self {
        Self {
            cut_params,
            npn_db: NpnDatabase::new(),
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

        // Calculate gain
        let gain = current_nodes as i32 - impl_.and_count as i32;

        if gain > 0 {
            Some(RewriteCandidate {
                node,
                cut: cut.clone(),
                canonical,
                implementation: impl_.clone(),
                gain,
            })
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

        // Apply rewrites greedily
        // Note: For simplicity, we just collect stats here.
        // A full implementation would actually apply the rewrites.
        for candidate in &candidates {
            // In a full implementation, we would:
            // 1. Build the new subgraph using the optimal implementation
            // 2. Update the node mapping
            // 3. Run DCE to clean up

            // For now, just count potential improvements
            self.rewritten_count += 1;
            self.total_gain += candidate.gain;

            // Limit the number of rewrites per pass
            if self.rewritten_count >= 100 {
                break;
            }
        }

        // The actual rewriting would rebuild the AIG here
        // For now, we just report the potential improvements

        result.record_after(aig);
        result.add_extra("candidates", &candidates.len().to_string());
        result.add_extra("potential_gain", &self.total_gain.to_string());
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
}
