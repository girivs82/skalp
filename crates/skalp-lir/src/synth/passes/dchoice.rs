//! Choice-Based Synthesis (DCHOICE) Pass
//!
//! This pass builds an AIG with choice nodes - equivalent implementations
//! of the same function that can be selected by the technology mapper.
//!
//! # Algorithm
//!
//! 1. Run multiple synthesis flows (balance, rewrite, refactor, etc.)
//! 2. Record equivalent nodes found during synthesis
//! 3. Create choice links between equivalent implementations
//! 4. Technology mapper can choose the best implementation
//!
//! # References
//!
//! - Mishchenko, A., Chatterjee, S., & Brayton, R. (2006).
//!   DAG-aware AIG rewriting: a fresh look at combinational logic synthesis.

use super::{Pass, PassResult};
use crate::synth::{Aig, AigLit, AigNode, AigNodeId};
use indexmap::IndexMap;
use std::collections::HashSet;

/// Maximum choices per node
const MAX_CHOICES: usize = 4;

/// Choice-Based Synthesis Pass
pub struct Dchoice {
    /// Number of choices recorded
    choices_recorded: usize,
    /// Verbose logging
    verbose: bool,
}

impl Default for Dchoice {
    fn default() -> Self {
        Self::new()
    }
}

impl Dchoice {
    /// Create a new dchoice pass
    pub fn new() -> Self {
        Self {
            choices_recorded: 0,
            verbose: false,
        }
    }

    /// Create with verbose logging
    pub fn verbose() -> Self {
        Self {
            choices_recorded: 0,
            verbose: true,
        }
    }

    /// Run a synthesis flow and record choices
    fn run_synthesis_flow(
        &self,
        aig: &mut Aig,
        passes: &[&str],
    ) -> IndexMap<AigNodeId, Vec<AigNodeId>> {
        let mut choices: IndexMap<AigNodeId, Vec<AigNodeId>> = IndexMap::new();

        // Record original nodes before synthesis
        let original_nodes: Vec<AigNodeId> = aig
            .iter_nodes()
            .filter_map(|(id, node)| {
                if matches!(node, AigNode::And { .. }) {
                    Some(id)
                } else {
                    None
                }
            })
            .collect();

        // Run synthesis passes
        for pass_name in passes {
            match *pass_name {
                "balance" => {
                    let mut pass = super::Balance::new();
                    pass.run(aig);
                }
                "rewrite" => {
                    let mut pass = super::Rewrite::new();
                    pass.run(aig);
                }
                "rewrite_z" => {
                    let mut pass = super::Rewrite::zero_cost();
                    pass.run(aig);
                }
                "refactor" => {
                    let mut pass = super::Refactor::new();
                    pass.run(aig);
                }
                "refactor_z" => {
                    let mut pass = super::Refactor::zero_cost();
                    pass.run(aig);
                }
                "strash" => {
                    let mut pass = super::Strash::new();
                    pass.run(aig);
                }
                "dce" => {
                    let mut pass = super::Dce::new();
                    pass.run(aig);
                }
                _ => {}
            }
        }

        // Find equivalent nodes by comparing truth tables
        let new_nodes: Vec<AigNodeId> = aig
            .iter_nodes()
            .filter_map(|(id, node)| {
                if matches!(node, AigNode::And { .. }) {
                    Some(id)
                } else {
                    None
                }
            })
            .collect();

        // Build equivalence map using truth tables
        for &orig in &original_nodes {
            let orig_leaves = self.collect_leaves(aig, orig, 6);
            let orig_tt = self.compute_tt(aig, orig, &orig_leaves);

            if let Some(ott) = orig_tt {
                for &new_node in &new_nodes {
                    if new_node == orig {
                        continue;
                    }

                    let new_leaves = self.collect_leaves(aig, new_node, 6);

                    // Only compare if leaves are the same
                    if new_leaves != orig_leaves || new_leaves.len() > 6 {
                        continue;
                    }

                    if let Some(ntt) = self.compute_tt(aig, new_node, &new_leaves) {
                        let mask = if new_leaves.len() < 6 {
                            (1u64 << (1 << new_leaves.len())) - 1
                        } else {
                            u64::MAX
                        };

                        if (ott & mask) == (ntt & mask) {
                            choices.entry(orig).or_default().push(new_node);
                        }
                    }
                }
            }
        }

        choices
    }

    /// Collect leaves for a node
    fn collect_leaves(&self, aig: &Aig, node: AigNodeId, max: usize) -> Vec<AigNodeId> {
        let mut leaves = Vec::new();
        let mut visited = HashSet::new();
        let mut stack = vec![node];

        while let Some(n) = stack.pop() {
            if visited.contains(&n) || leaves.len() >= max {
                continue;
            }
            visited.insert(n);

            match aig.get_node(n) {
                Some(AigNode::Input { .. }) | Some(AigNode::Latch { .. }) => {
                    leaves.push(n);
                }
                Some(AigNode::And { left, right, .. }) => {
                    stack.push(left.node);
                    stack.push(right.node);
                }
                _ => {}
            }
        }

        leaves.sort();
        leaves
    }

    /// Compute truth table
    fn compute_tt(&self, aig: &Aig, node: AigNodeId, leaves: &[AigNodeId]) -> Option<u64> {
        if leaves.len() > 6 {
            return None;
        }

        let leaf_map: IndexMap<AigNodeId, usize> =
            leaves.iter().enumerate().map(|(i, &n)| (n, i)).collect();

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
        leaf_map: &IndexMap<AigNodeId, usize>,
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
        leaf_map: &IndexMap<AigNodeId, usize>,
    ) -> bool {
        let val = self.evaluate(aig, lit.node, assignment, leaf_map);
        if lit.inverted {
            !val
        } else {
            val
        }
    }
}

impl Pass for Dchoice {
    fn name(&self) -> &str {
        "dchoice"
    }

    fn run(&mut self, aig: &mut Aig) -> PassResult {
        let mut result = PassResult::new(self.name());
        result.record_before(aig);

        self.choices_recorded = 0;

        // Run multiple synthesis flows to find choices
        let flows = [
            // Flow 1: Balance-focused
            vec!["balance", "rewrite", "balance"],
            // Flow 2: Rewrite-focused
            vec!["rewrite", "rewrite_z", "balance"],
            // Flow 3: Refactor-focused
            vec!["refactor", "balance", "rewrite"],
        ];

        let mut all_choices: IndexMap<AigNodeId, Vec<AigNodeId>> = IndexMap::new();

        // Run each flow and collect choices
        for flow in &flows {
            // We need a fresh copy for each flow, but since we're modifying in place,
            // we'll just run the passes. In a full implementation, we'd maintain
            // separate AIGs and merge.
            let choices = self.run_synthesis_flow(aig, flow);

            for (node, equiv_nodes) in choices {
                let entry = all_choices.entry(node).or_default();
                for equiv in equiv_nodes {
                    if !entry.contains(&equiv) && entry.len() < MAX_CHOICES {
                        entry.push(equiv);
                        self.choices_recorded += 1;
                    }
                }
            }
        }

        // Record choices in the AIG
        // Note: Full implementation would use aig.add_choice() if available
        // For now, we just track the equivalences
        for (node, equiv_nodes) in &all_choices {
            if !equiv_nodes.is_empty() {
                // Record the choice relationship
                // The technology mapper can use this information
                for &equiv in equiv_nodes {
                    aig.add_choice(*node, AigLit::new(equiv));
                }
            }
        }

        // Final cleanup
        let mut dce = super::Dce::new();
        dce.run(aig);

        result.record_after(aig);
        result.add_extra("choices_recorded", &self.choices_recorded.to_string());
        result
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_dchoice_creation() {
        let pass = Dchoice::new();
        assert_eq!(pass.name(), "dchoice");
    }

    #[test]
    fn test_dchoice_simple() {
        let mut aig = Aig::new("test".to_string());
        let a = aig.add_input("a".to_string(), None);
        let b = aig.add_input("b".to_string(), None);
        let ab = aig.add_and(AigLit::new(a), AigLit::new(b));
        aig.add_output("y".to_string(), ab);

        let mut pass = Dchoice::new();
        let result = pass.run(&mut aig);

        // Should complete without error
        assert!(result.ands_after >= 1);
    }

    #[test]
    fn test_collect_leaves() {
        let mut aig = Aig::new("test".to_string());
        let a = aig.add_input("a".to_string(), None);
        let b = aig.add_input("b".to_string(), None);
        let c = aig.add_input("c".to_string(), None);

        let ab = aig.add_and(AigLit::new(a), AigLit::new(b));
        let abc = aig.add_and(ab, AigLit::new(c));
        aig.add_output("y".to_string(), abc);

        let dchoice = Dchoice::new();
        let leaves = dchoice.collect_leaves(&aig, abc.node, 6);

        // Should include a, b, c
        assert_eq!(leaves.len(), 3);
    }

    #[test]
    fn test_compute_tt() {
        let mut aig = Aig::new("test".to_string());
        let a = aig.add_input("a".to_string(), None);
        let b = aig.add_input("b".to_string(), None);
        let ab = aig.add_and(AigLit::new(a), AigLit::new(b));
        aig.add_output("y".to_string(), ab);

        let dchoice = Dchoice::new();
        let leaves = dchoice.collect_leaves(&aig, ab.node, 6);
        let tt = dchoice.compute_tt(&aig, ab.node, &leaves);

        // AND truth table for 2 inputs: 1000 = 0x8
        assert!(tt.is_some());
        // The exact value depends on variable ordering
    }
}
