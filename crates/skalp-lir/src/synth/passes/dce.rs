//! Dead Code Elimination Pass
//!
//! This pass removes nodes that do not contribute to any output.
//! It performs a backward traversal from outputs and removes any
//! nodes that are not reachable.

use super::{Pass, PassResult};
use crate::synth::{Aig, AigLit, AigNode, AigNodeId, BarrierType};
use std::collections::{HashMap, HashSet};

/// Dead code elimination pass
pub struct Dce {
    /// Number of nodes removed
    removed_count: usize,
}

/// Mark a node and its fanin cone as live
fn mark_live(aig: &Aig, node_id: AigNodeId, live: &mut HashSet<AigNodeId>) {
    if live.contains(&node_id) {
        return;
    }
    live.insert(node_id);

    if let Some(node) = aig.get_node(node_id) {
        for fanin in node.fanins() {
            mark_live(aig, fanin.node, live);
        }
    }
}

impl Dce {
    /// Create a new DCE pass
    pub fn new() -> Self {
        Self { removed_count: 0 }
    }
}

impl Default for Dce {
    fn default() -> Self {
        Self::new()
    }
}

impl Pass for Dce {
    fn name(&self) -> &str {
        "dce"
    }

    fn run(&mut self, aig: &mut Aig) -> PassResult {
        let mut result = PassResult::new(self.name());
        result.record_before(aig);

        self.removed_count = 0;

        // Mark all live nodes starting from outputs
        let mut live = HashSet::new();
        live.insert(AigNodeId::FALSE); // Constant is always live

        for (_, lit) in aig.outputs() {
            mark_live(aig, lit.node, &mut live);
        }

        // Also mark latches as live (their outputs might feed back)
        for (id, node) in aig.iter_nodes() {
            if node.is_latch() {
                live.insert(id);
                for fanin in node.fanins() {
                    mark_live(aig, fanin.node, &mut live);
                }
            }
        }

        // Count dead nodes
        let total_nodes = aig.node_count();
        let live_nodes = live.len();
        self.removed_count = total_nodes.saturating_sub(live_nodes);

        // If no dead nodes, return early
        if self.removed_count == 0 {
            result.record_after(aig);
            return result;
        }

        // Build new AIG with only live nodes
        let mut new_aig = Aig::new(aig.name.clone());
        let mut node_map: HashMap<AigNodeId, AigLit> = HashMap::new();

        // Map constant
        node_map.insert(AigNodeId::FALSE, AigLit::false_lit());

        // Process nodes in order, copying only live ones
        for (id, node) in aig.iter_nodes() {
            if !live.contains(&id) {
                continue;
            }

            match node {
                AigNode::Const => {
                    // Already handled
                }
                AigNode::Input { name, source_net } => {
                    let safety = aig.get_safety_info(id).cloned().unwrap_or_default();
                    let new_id = new_aig.add_input_with_safety(name.clone(), *source_net, safety);
                    node_map.insert(id, AigLit::new(new_id));
                }
                AigNode::And { left, right } => {
                    let new_left = resolve_lit(&node_map, *left);
                    let new_right = resolve_lit(&node_map, *right);

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
                    let new_data = resolve_lit(&node_map, *data);
                    let new_clock =
                        clock.map(|c| node_map.get(&c).copied().unwrap_or(AigLit::new(c)).node);
                    let new_reset =
                        reset.map(|r| node_map.get(&r).copied().unwrap_or(AigLit::new(r)).node);

                    let safety = aig.get_safety_info(id).cloned().unwrap_or_default();
                    let new_id = new_aig
                        .add_latch_with_safety(new_data, *init, new_clock, new_reset, safety);
                    node_map.insert(id, AigLit::new(new_id));
                }
                AigNode::Barrier {
                    barrier_type,
                    data,
                    enable,
                    clock,
                    reset,
                    init,
                } => {
                    let new_data = resolve_lit(&node_map, *data);
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
                        *init,
                        safety,
                    );
                    node_map.insert(id, AigLit::new(new_id));
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

        result.record_after(aig);
        result.add_extra("removed", &self.removed_count.to_string());
        result
    }
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_dce_removes_dead_nodes() {
        let mut aig = Aig::new("test".to_string());
        let a = aig.add_input("a".to_string(), None);
        let b = aig.add_input("b".to_string(), None);
        let c = aig.add_input("c".to_string(), None);

        // Create some AND gates
        let ab = aig.add_and(AigLit::new(a), AigLit::new(b)); // Used
        let bc = aig.add_and(AigLit::new(b), AigLit::new(c)); // Dead

        // Only output ab
        aig.add_output("y".to_string(), ab);

        let before_ands = aig.and_count();
        assert_eq!(before_ands, 2);

        let mut pass = Dce::new();
        let result = pass.run(&mut aig);

        // bc should be removed
        assert_eq!(aig.and_count(), 1);
        assert!(result.changed);
    }

    #[test]
    fn test_dce_keeps_all_used() {
        let mut aig = Aig::new("test".to_string());
        let a = aig.add_input("a".to_string(), None);
        let b = aig.add_input("b".to_string(), None);

        let ab = aig.add_and(AigLit::new(a), AigLit::new(b));
        aig.add_output("y".to_string(), ab);

        let before_ands = aig.and_count();

        let mut pass = Dce::new();
        let result = pass.run(&mut aig);

        // Nothing should be removed
        assert_eq!(aig.and_count(), before_ands);
    }

    #[test]
    fn test_dce_with_chain() {
        let mut aig = Aig::new("test".to_string());
        let a = aig.add_input("a".to_string(), None);
        let b = aig.add_input("b".to_string(), None);
        let c = aig.add_input("c".to_string(), None);

        // Chain: a & b -> (a&b) & c
        let ab = aig.add_and(AigLit::new(a), AigLit::new(b));
        let abc = aig.add_and(ab, AigLit::new(c));

        // Dead chain: b & c
        let _bc = aig.add_and(AigLit::new(b), AigLit::new(c));

        aig.add_output("y".to_string(), abc);

        assert_eq!(aig.and_count(), 3);

        let mut pass = Dce::new();
        let result = pass.run(&mut aig);

        // Only ab and abc should remain
        assert_eq!(aig.and_count(), 2);
        assert!(result.changed);
    }
}
