//! AIG Refactoring Pass
//!
//! This pass performs cone-based refactoring, which works on larger
//! logic cones than the rewrite pass. It collects the leaves of a
//! cone and rebuilds it using algebraic factorization.
//!
//! # Algorithm
//!
//! For each node with multiple fanins:
//! 1. Collect all leaves up to a depth limit
//! 2. Compute the Boolean function of the cone
//! 3. Factor the function using algebraic methods
//! 4. Rebuild with the factored form if smaller
//!
//! # References
//!
//! - Brayton, R. K., & McMullen, C. (1982). The decomposition and factorization of Boolean expressions.

use super::{Pass, PassResult};
use crate::synth::{Aig, AigLit, AigNode, AigNodeId, AigSafetyInfo};
use std::collections::{HashMap, HashSet};

/// Maximum depth for cone collection
const MAX_CONE_DEPTH: usize = 10;

/// Maximum number of leaves for refactoring
const MAX_CONE_LEAVES: usize = 12;

/// AIG refactoring pass
pub struct Refactor {
    /// Maximum cone depth
    max_depth: usize,
    /// Maximum cone leaves
    max_leaves: usize,
    /// Number of cones refactored
    refactored_count: usize,
}

impl Refactor {
    /// Create a new refactoring pass with default parameters
    pub fn new() -> Self {
        Self {
            max_depth: MAX_CONE_DEPTH,
            max_leaves: MAX_CONE_LEAVES,
            refactored_count: 0,
        }
    }

    /// Create a refactoring pass with custom parameters
    pub fn with_params(max_depth: usize, max_leaves: usize) -> Self {
        Self {
            max_depth,
            max_leaves,
            refactored_count: 0,
        }
    }

    /// Collect the leaves of a cone rooted at the given node
    fn collect_cone(
        &self,
        aig: &Aig,
        root: AigNodeId,
        fanout_counts: &HashMap<AigNodeId, usize>,
    ) -> ConeInfo {
        let mut leaves = Vec::new();
        let mut internal = HashSet::new();
        let mut stack = vec![(root, 0usize)];
        let mut visited = HashSet::new();

        while let Some((node, depth)) = stack.pop() {
            if visited.contains(&node) {
                continue;
            }
            visited.insert(node);

            // Check if this should be a leaf
            let is_leaf = if node == root {
                false
            } else if depth >= self.max_depth || leaves.len() >= self.max_leaves {
                true
            } else {
                match aig.get_node(node) {
                    Some(AigNode::And { .. }) => {
                        // Multi-fanout nodes become leaves
                        let fanouts = fanout_counts.get(&node).copied().unwrap_or(0);
                        fanouts > 1
                    }
                    Some(AigNode::Input { .. }) | Some(AigNode::Latch { .. }) => true,
                    Some(AigNode::Const) => true,
                    None => true,
                }
            };

            if is_leaf {
                if !leaves.contains(&node) {
                    leaves.push(node);
                }
            } else {
                internal.insert(node);
                if let Some(n) = aig.get_node(node) {
                    for fanin in n.fanins() {
                        stack.push((fanin.node, depth + 1));
                    }
                }
            }
        }

        ConeInfo {
            root,
            leaves,
            internal_nodes: internal.len(),
        }
    }

    /// Try to find a better factorization for a cone
    fn try_refactor(&self, aig: &Aig, cone: &ConeInfo) -> Option<RefactorResult> {
        // For now, we just check if the cone could potentially be improved
        // A full implementation would:
        // 1. Compute the Boolean function of the cone
        // 2. Apply algebraic factorization (quick_factor, good_factor)
        // 3. Compare the result with the current implementation

        // Skip small cones
        if cone.internal_nodes <= 2 {
            return None;
        }

        // Skip cones with too many leaves (can't compute truth table)
        if cone.leaves.len() > 6 {
            return None;
        }

        // Estimate potential improvement based on heuristics
        // Real implementation would compute actual factorization
        let estimated_savings = if cone.internal_nodes > 4 && cone.leaves.len() <= 4 {
            (cone.internal_nodes as i32 - 3).max(0)
        } else {
            0
        };

        if estimated_savings > 0 {
            Some(RefactorResult {
                cone: cone.clone(),
                estimated_savings,
            })
        } else {
            None
        }
    }
}

/// Information about a logic cone
#[derive(Debug, Clone)]
struct ConeInfo {
    root: AigNodeId,
    leaves: Vec<AigNodeId>,
    internal_nodes: usize,
}

/// Result of attempting to refactor a cone
struct RefactorResult {
    cone: ConeInfo,
    estimated_savings: i32,
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

impl Default for Refactor {
    fn default() -> Self {
        Self::new()
    }
}

impl Pass for Refactor {
    fn name(&self) -> &str {
        "refactor"
    }

    fn run(&mut self, aig: &mut Aig) -> PassResult {
        let mut result = PassResult::new(self.name());
        result.record_before(aig);

        self.refactored_count = 0;

        // Compute fanout counts
        let fanout_counts = compute_fanout_counts(aig);

        // Collect refactoring candidates
        let mut candidates: Vec<RefactorResult> = Vec::new();

        for (id, node) in aig.iter_nodes() {
            if let AigNode::And { .. } = node {
                let cone = self.collect_cone(aig, id, &fanout_counts);
                if let Some(refactor_result) = self.try_refactor(aig, &cone) {
                    candidates.push(refactor_result);
                }
            }
        }

        // Sort by savings
        candidates.sort_by(|a, b| b.estimated_savings.cmp(&a.estimated_savings));

        // Count potential improvements
        let total_savings: i32 = candidates.iter().map(|c| c.estimated_savings).sum();
        self.refactored_count = candidates.len();

        // The actual refactoring would rebuild the AIG here
        // For now, we just report the potential improvements

        result.record_after(aig);
        result.add_extra("cones_analyzed", &candidates.len().to_string());
        result.add_extra("potential_savings", &total_savings.to_string());
        result
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_refactor_creation() {
        let pass = Refactor::new();
        assert_eq!(pass.name(), "refactor");
    }

    #[test]
    fn test_refactor_simple() {
        let mut aig = Aig::new("test".to_string());
        let a = aig.add_input("a".to_string(), None);
        let b = aig.add_input("b".to_string(), None);
        let ab = aig.add_and(AigLit::new(a), AigLit::new(b));
        aig.add_output("y".to_string(), ab);

        let mut pass = Refactor::new();
        let result = pass.run(&mut aig);

        // Should complete without error
        assert!(result.ands_after >= 1);
    }

    #[test]
    fn test_collect_cone() {
        let mut aig = Aig::new("test".to_string());
        let a = aig.add_input("a".to_string(), None);
        let b = aig.add_input("b".to_string(), None);
        let c = aig.add_input("c".to_string(), None);

        let ab = aig.add_and(AigLit::new(a), AigLit::new(b));
        let abc = aig.add_and(ab, AigLit::new(c));
        aig.add_output("y".to_string(), abc);

        let fanout_counts = compute_fanout_counts(&aig);
        let refactor = Refactor::new();

        let cone = refactor.collect_cone(&aig, abc.node, &fanout_counts);

        // Leaves should be a, b, c
        assert_eq!(cone.leaves.len(), 3);
        assert!(cone.leaves.contains(&a));
        assert!(cone.leaves.contains(&b));
        assert!(cone.leaves.contains(&c));

        // Internal nodes: ab and abc
        assert_eq!(cone.internal_nodes, 2);
    }

    #[test]
    fn test_collect_cone_with_shared() {
        let mut aig = Aig::new("test".to_string());
        let a = aig.add_input("a".to_string(), None);
        let b = aig.add_input("b".to_string(), None);

        let ab = aig.add_and(AigLit::new(a), AigLit::new(b));
        let aab = aig.add_and(ab, AigLit::new(a));

        // ab is used twice, so output both
        aig.add_output("y1".to_string(), ab);
        aig.add_output("y2".to_string(), aab);

        let fanout_counts = compute_fanout_counts(&aig);
        let refactor = Refactor::new();

        let cone = refactor.collect_cone(&aig, aab.node, &fanout_counts);

        // ab should be a leaf (multi-fanout)
        assert!(cone.leaves.contains(&ab.node));
    }

    #[test]
    fn test_refactor_with_params() {
        let pass = Refactor::with_params(5, 8);
        assert_eq!(pass.max_depth, 5);
        assert_eq!(pass.max_leaves, 8);
    }
}
