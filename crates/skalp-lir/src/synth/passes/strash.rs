//! Structural Hashing Pass
//!
//! This pass rebuilds the AIG with structural hashing enabled, merging
//! structurally identical AND nodes.
//!
//! Structural hashing ensures that for any two AND nodes with the same
//! inputs, only one copy exists in the AIG.
//!
//! # References
//!
//! - Mishchenko, A., Chatterjee, S., & Brayton, R. (2006).
//!   DAG-aware AIG rewriting: a fresh look at combinational logic synthesis.
//! - ABC User Guide: `strash` command for structural hashing.

use super::{Pass, PassResult};
use crate::synth::{Aig, AigLit, AigNode, AigNodeId, BarrierType};
use indexmap::IndexMap;

/// Structural hashing pass
pub struct Strash {
    /// Number of nodes merged
    merged_count: usize,
}

impl Strash {
    /// Create a new structural hashing pass
    pub fn new() -> Self {
        Self { merged_count: 0 }
    }
}

impl Default for Strash {
    fn default() -> Self {
        Self::new()
    }
}

impl Pass for Strash {
    fn name(&self) -> &str {
        "strash"
    }

    #[allow(clippy::type_complexity)]
    fn run(&mut self, aig: &mut Aig) -> PassResult {
        let mut result = PassResult::new(self.name());
        result.record_before(aig);

        self.merged_count = 0;

        // Build new AIG with fresh structural hash
        // Use two-phase approach for sequential circuits:
        // Phase 1: Process inputs, pre-create latches, then process AND nodes
        // Phase 2: Update latch data with resolved values
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
                    // Pre-create latch with placeholder data, record for phase 2
                    let new_clock = clock.and_then(|c| node_map.get(&c).map(|lit| lit.node));
                    let new_reset = reset.and_then(|r| node_map.get(&r).map(|lit| lit.node));
                    let safety = aig.get_safety_info(id).cloned().unwrap_or_default();
                    let new_id = new_aig.add_latch_with_safety(
                        AigLit::false_lit(),
                        *init,
                        new_clock,
                        new_reset,
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
                    let new_clock = clock.and_then(|c| node_map.get(&c).map(|lit| lit.node));
                    let new_reset = reset.and_then(|r| node_map.get(&r).map(|lit| lit.node));
                    let safety = aig.get_safety_info(id).cloned().unwrap_or_default();
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
                // Resolve inputs through the mapping
                let new_left = resolve_lit(&node_map, *left);
                let new_right = resolve_lit(&node_map, *right);

                // The add_and method will perform structural hashing
                let old_and_count = new_aig.and_count();
                let safety = aig.get_safety_info(id).cloned().unwrap_or_default();
                let new_lit = new_aig.add_and_with_safety(new_left, new_right, safety);
                let new_and_count = new_aig.and_count();

                // Check if the node was merged (reused existing)
                if new_and_count == old_and_count {
                    self.merged_count += 1;
                }

                node_map.insert(id, new_lit);
            }
        }

        // Phase 2: Update latch/barrier data with resolved values
        for (id, data, _init, _clock, _reset) in latch_ids {
            let new_node_id = node_map.get(&id).map(|lit| lit.node).unwrap();
            let new_data = resolve_lit(&node_map, data);
            new_aig.update_latch_data(new_node_id, new_data);
        }

        for (id, _barrier_type, data, enable, _clock, _reset, _init) in barrier_ids {
            let new_node_id = node_map.get(&id).map(|lit| lit.node).unwrap();
            let new_data = resolve_lit(&node_map, data);
            let new_enable = enable.map(|e| resolve_lit(&node_map, e));
            new_aig.update_barrier_data(new_node_id, new_data, new_enable);
        }

        // Copy outputs with resolved literals
        for (name, lit) in aig.outputs() {
            let new_lit = resolve_lit(&node_map, *lit);
            new_aig.add_output(name.clone(), new_lit);
        }

        // Replace the AIG
        *aig = new_aig;

        result.record_after(aig);
        result.add_extra("merged", &self.merged_count.to_string());
        result
    }
}

/// Resolve a literal through the node mapping
fn resolve_lit(map: &IndexMap<AigNodeId, AigLit>, lit: AigLit) -> AigLit {
    if lit.node == AigNodeId::FALSE {
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
            "[STRASH WARNING] Node {:?} not found in map, returning FALSE",
            lit.node
        );
        AigLit::false_lit()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_strash_merges_identical() {
        let mut aig = Aig::new("test".to_string());
        let a = aig.add_input("a".to_string(), None);
        let b = aig.add_input("b".to_string(), None);

        // Create two identical AND gates (strash should already merge them)
        let and1 = aig.add_and(AigLit::new(a), AigLit::new(b));
        let and2 = aig.add_and(AigLit::new(a), AigLit::new(b));

        // They should already be the same due to built-in strash
        assert_eq!(and1, and2);
        assert_eq!(aig.and_count(), 1);

        // Running strash pass should not change anything
        let mut pass = Strash::new();
        let result = pass.run(&mut aig);

        assert_eq!(aig.and_count(), 1);
    }

    #[test]
    fn test_strash_after_modifications() {
        let mut aig = Aig::new("test".to_string());
        let a = aig.add_input("a".to_string(), None);
        let b = aig.add_input("b".to_string(), None);

        // Create an AND gate
        let and1 = aig.add_and(AigLit::new(a), AigLit::new(b));
        aig.add_output("y".to_string(), and1);

        // Clear strash and rebuild
        aig.clear_strash();

        let mut pass = Strash::new();
        let result = pass.run(&mut aig);

        // Should still have 1 AND
        assert_eq!(aig.and_count(), 1);
    }
}
