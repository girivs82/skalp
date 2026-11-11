//! Structural choices (ABC's `dch`).
//!
//! A cut-based mapper only sees the cuts of the one AIG structure it is
//! given, so the depth it reaches depends on how the logic happens to be
//! written. This pass gives it several structures of the same logic at once:
//!
//! 1. Copies of the AIG are optimized by different scripts (balancing for
//!    depth, rewriting and refactoring for area).
//! 2. The copies are added into the original AIG on its own inputs and
//!    latches; structural hashing shares whatever they have in common.
//! 3. FRAIG (simulation and SAT) proves which nodes compute the same
//!    function. Instead of merging them, each proven node becomes a *choice*
//!    of its class's first node, the representative.
//! 4. Every reader of a class member reads the representative instead, so
//!    the alternatives of one class are built on the classes below them and
//!    the mapper can mix structures (ABC's choice AIG).
//!
//! The mapper then takes, at a node with choices, the cuts of every
//! alternative as well (see `CutEnumeration::enumerate_with_choices`).
//! A choice is dropped when using it would make the AIG cyclic: an
//! alternative that reads its own representative, directly or through other
//! choices, cannot replace it.
//!
//! Reference: Chatterjee, Mishchenko, Brayton, Wang, Kam, "Reducing
//! structural bias in technology mapping", ICCAD 2005.

use super::{Balance, Fraig, FraigConfig, Pass, PassResult, Refactor, Rewrite};
use crate::synth::{Aig, AigLit, AigNode, AigNodeId};
use std::collections::HashMap;

/// Choice-Based Synthesis Pass
pub struct Dchoice {
    /// Number of choices recorded
    choices_recorded: usize,
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
        }
    }

    /// Choices recorded by the last run
    pub fn choices_recorded(&self) -> usize {
        self.choices_recorded
    }

    /// The copies of `aig` whose structures become choices: one balanced
    /// for depth, one rewritten and refactored for area, then balanced.
    fn snapshots(aig: &Aig) -> Vec<Aig> {
        let mut balanced = aig.clone();
        Balance::new().run(&mut balanced);

        let mut restructured = aig.clone();
        Rewrite::new().run(&mut restructured);
        Refactor::new().run(&mut restructured);
        Balance::new().run(&mut restructured);
        Rewrite::new().run(&mut restructured);

        vec![balanced, restructured]
    }

    /// Add the AND nodes of `snapshot` to `aig`, reading `aig`'s own inputs,
    /// latches and barriers. Passes rebuild an AIG keeping its inputs in
    /// order, and its latches and barriers in order, so the k-th of each
    /// kind corresponds. Returns false (adding nothing) when the snapshot's
    /// sources do not line up with `aig`'s.
    fn import(aig: &mut Aig, snapshot: &Aig) -> bool {
        let sources = |a: &Aig| {
            let mut inputs = Vec::new();
            let mut state = Vec::new();
            for (id, node) in a.iter_nodes() {
                match node {
                    AigNode::Input { .. } => inputs.push(id),
                    AigNode::Latch { .. } | AigNode::Barrier { .. } => state.push(id),
                    _ => {}
                }
            }
            (inputs, state)
        };
        let (own_inputs, own_state) = sources(aig);
        let (snap_inputs, snap_state) = sources(snapshot);
        if own_inputs.len() != snap_inputs.len() || own_state.len() != snap_state.len() {
            return false;
        }
        let mut map: HashMap<AigNodeId, AigLit> = HashMap::new();
        map.insert(AigNodeId::FALSE, AigLit::false_lit());
        for (s, o) in snap_inputs
            .iter()
            .zip(&own_inputs)
            .chain(snap_state.iter().zip(&own_state))
        {
            map.insert(*s, AigLit::new(*o));
        }
        for (id, node) in snapshot.iter_nodes() {
            if let AigNode::And { left, right } = node {
                let (Some(&l), Some(&r)) = (map.get(&left.node), map.get(&right.node)) else {
                    // An AND reading a later node: not a rebuilt AIG
                    return false;
                };
                let l = if left.inverted { l.invert() } else { l };
                let r = if right.inverted { r.invert() } else { r };
                let lit = aig.add_and(l, r);
                map.insert(id, lit);
            }
        }
        true
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

        // Choices recorded by an earlier run refer to nodes of this AIG and
        // stay valid; the new ones are added to them.
        for snapshot in Self::snapshots(aig) {
            if !Self::import(aig, &snapshot) {
                tracing::debug!("[DCH] a snapshot's inputs do not line up; skipped");
            }
        }

        let mut fraig = Fraig::with_config(FraigConfig {
            record_choices: true,
            max_sat_calls: 200_000,
            ..FraigConfig::default()
        });
        fraig.run(aig);
        let redirected = aig.redirect_to_representatives();
        let dropped = aig.prune_cyclic_choices();
        self.choices_recorded = aig.total_choice_count();

        result.record_after(aig);
        result.add_extra("choices_recorded", &self.choices_recorded.to_string());
        result.add_extra("fanins_redirected", &redirected.to_string());
        result.add_extra("cyclic_choices_dropped", &dropped.to_string());
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

    /// A chain `((((a & b) & c) & d) & ...)` balances to a tree: the chain's
    /// last node gets the tree as a choice, and every choice is acyclic.
    /// `Balance` rebuilds AND cones rooted at nodes read more than once, so
    /// the chain is read by two outputs.
    #[test]
    fn test_chain_gets_its_balanced_tree_as_a_choice() {
        let mut aig = Aig::new("test".to_string());
        let inputs: Vec<AigLit> = ["a", "b", "c", "d", "e", "f", "g", "h"]
            .iter()
            .map(|n| AigLit::new(aig.add_input(n.to_string(), None)))
            .collect();
        let mut chain = inputs[0];
        for &x in &inputs[1..] {
            chain = aig.add_and(chain, x);
        }
        aig.add_output("y".to_string(), chain);
        aig.add_output("z".to_string(), chain);
        let nodes_before = aig.iter_nodes().count();

        let mut pass = Dchoice::new();
        pass.run(&mut aig);

        assert!(aig.iter_nodes().count() > nodes_before, "snapshots added");
        assert!(aig.has_choices(chain.node), "the output node has choices");
        assert_eq!(aig.outputs()[0].1, chain, "outputs are unchanged");
        assert_eq!(aig.prune_cyclic_choices(), 0);
    }

    #[test]
    fn test_snapshot_with_other_inputs_is_not_imported() {
        let mut aig = Aig::new("a".to_string());
        let a = AigLit::new(aig.add_input("a".to_string(), None));
        let b = AigLit::new(aig.add_input("b".to_string(), None));
        let y = aig.add_and(a, b);
        aig.add_output("y".to_string(), y);
        let mut other = Aig::new("b".to_string());
        let c = AigLit::new(other.add_input("c".to_string(), None));
        other.add_output("y".to_string(), c);
        assert!(!Dchoice::import(&mut aig, &other));
    }
}
