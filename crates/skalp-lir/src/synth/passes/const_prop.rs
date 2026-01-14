//! Constant Propagation Pass
//!
//! This pass propagates constant values through the AIG, simplifying nodes
//! where one or more inputs are constant.
//!
//! # Simplifications
//!
//! - `a & 0 = 0`
//! - `a & 1 = a`
//! - `a & a = a`
//! - `a & !a = 0`

use super::{Pass, PassResult};
use crate::synth::{Aig, AigLit, AigNode, AigNodeId, AigSafetyInfo, BarrierType};
use indexmap::IndexMap;

/// Constant propagation pass
pub struct ConstProp {
    /// Mapping from old node to new literal
    replacements: IndexMap<AigNodeId, AigLit>,
    /// Number of nodes simplified
    simplified_count: usize,
}

impl ConstProp {
    /// Create a new constant propagation pass
    pub fn new() -> Self {
        Self {
            replacements: IndexMap::new(),
            simplified_count: 0,
        }
    }

    /// Resolve a literal through replacements
    fn resolve(&self, lit: AigLit) -> AigLit {
        if let Some(&replacement) = self.replacements.get(&lit.node) {
            if lit.inverted {
                replacement.invert()
            } else {
                replacement
            }
        } else {
            lit
        }
    }

    /// Try to simplify an AND node
    fn simplify_and(&self, left: AigLit, right: AigLit) -> Option<AigLit> {
        let left = self.resolve(left);
        let right = self.resolve(right);

        // a & 0 = 0
        if left.node == AigNodeId::FALSE && !left.inverted {
            return Some(AigLit::false_lit());
        }
        if right.node == AigNodeId::FALSE && !right.inverted {
            return Some(AigLit::false_lit());
        }

        // a & 1 = a
        if left.node == AigNodeId::FALSE && left.inverted {
            return Some(right);
        }
        if right.node == AigNodeId::FALSE && right.inverted {
            return Some(left);
        }

        // a & a = a
        if left == right {
            return Some(left);
        }

        // a & !a = 0
        if left.node == right.node && left.inverted != right.inverted {
            return Some(AigLit::false_lit());
        }

        None
    }
}

impl Default for ConstProp {
    fn default() -> Self {
        Self::new()
    }
}

impl Pass for ConstProp {
    fn name(&self) -> &str {
        "const_prop"
    }

    fn run(&mut self, aig: &mut Aig) -> PassResult {
        let mut result = PassResult::new(self.name());
        result.record_before(aig);

        self.replacements.clear();
        self.simplified_count = 0;

        // Build new AIG with constants propagated
        let mut new_aig = Aig::new(aig.name.clone());

        // First pass: copy inputs and find simplifications
        for (id, node) in aig.iter_nodes() {
            match node {
                AigNode::Const => {
                    // Constant node - already exists in new AIG at index 0
                }
                AigNode::Input { name, source_net } => {
                    let safety = aig.get_safety_info(id).cloned().unwrap_or_default();
                    let new_id = new_aig.add_input_with_safety(name.clone(), *source_net, safety);
                    self.replacements.insert(id, AigLit::new(new_id));
                }
                AigNode::And { left, right } => {
                    let resolved_left = self.resolve(*left);
                    let resolved_right = self.resolve(*right);

                    if let Some(simplified) = self.simplify_and(resolved_left, resolved_right) {
                        self.replacements.insert(id, simplified);
                        self.simplified_count += 1;
                    } else {
                        // Create new AND node with resolved inputs
                        let safety = aig.get_safety_info(id).cloned().unwrap_or_default();
                        let new_lit =
                            new_aig.add_and_with_safety(resolved_left, resolved_right, safety);
                        self.replacements.insert(id, new_lit);
                    }
                }
                AigNode::Latch {
                    data,
                    init,
                    clock,
                    reset,
                } => {
                    let resolved_data = self.resolve(*data);
                    let resolved_clock = clock.map(|c| self.resolve(AigLit::new(c)).node);
                    let resolved_reset = reset.map(|r| self.resolve(AigLit::new(r)).node);

                    let safety = aig.get_safety_info(id).cloned().unwrap_or_default();
                    let new_id = new_aig.add_latch_with_safety(
                        resolved_data,
                        *init,
                        resolved_clock,
                        resolved_reset,
                        safety,
                    );
                    self.replacements.insert(id, AigLit::new(new_id));
                }
                AigNode::Barrier {
                    barrier_type,
                    data,
                    enable,
                    clock,
                    reset,
                    init,
                } => {
                    // Barriers are power domain boundaries - copy with resolved inputs
                    let resolved_data = self.resolve(*data);
                    let resolved_enable = enable.map(|e| self.resolve(e));
                    let resolved_clock = clock.map(|c| self.resolve(AigLit::new(c)).node);
                    let resolved_reset = reset.map(|r| self.resolve(AigLit::new(r)).node);

                    let safety = aig.get_safety_info(id).cloned().unwrap_or_default();
                    let new_id = new_aig.add_barrier_with_safety(
                        barrier_type.clone(),
                        resolved_data,
                        resolved_enable,
                        resolved_clock,
                        resolved_reset,
                        *init,
                        safety,
                    );
                    self.replacements.insert(id, AigLit::new(new_id));
                }
            }
        }

        // Copy outputs with resolved literals
        for (name, lit) in aig.outputs() {
            let resolved = self.resolve(*lit);
            new_aig.add_output(name.clone(), resolved);
        }

        // Replace the AIG
        *aig = new_aig;

        result.record_after(aig);
        result.add_extra("simplified", &self.simplified_count.to_string());
        result
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_const_prop_and_zero() {
        let mut aig = Aig::new("test".to_string());
        let a = aig.add_input("a".to_string(), None);

        // a & 0 should become 0
        // Note: add_and already simplifies this, so the output will be false_lit directly
        let and_lit = aig.add_and(AigLit::new(a), AigLit::false_lit());
        aig.add_output("y".to_string(), and_lit);

        let mut pass = ConstProp::new();
        let _result = pass.run(&mut aig);

        // Output should be constant 0
        let (_, out_lit) = &aig.outputs()[0];
        assert_eq!(*out_lit, AigLit::false_lit());
        // Note: result.changed may be false because add_and already simplified
    }

    #[test]
    fn test_const_prop_and_one() {
        let mut aig = Aig::new("test".to_string());
        let a = aig.add_input("a".to_string(), None);

        // a & 1 should become a
        let and_lit = aig.add_and(AigLit::new(a), AigLit::true_lit());
        aig.add_output("y".to_string(), and_lit);

        let mut pass = ConstProp::new();
        let _result = pass.run(&mut aig);

        // Output should reference input a
        let (_, out_lit) = &aig.outputs()[0];
        assert!(!out_lit.inverted);
        // The node should be an input (not an AND)
        assert!(aig.get_node(out_lit.node).unwrap().is_input());
    }

    #[test]
    fn test_const_prop_and_self() {
        let mut aig = Aig::new("test".to_string());
        let a = aig.add_input("a".to_string(), None);

        // a & a should become a
        let and_lit = aig.add_and(AigLit::new(a), AigLit::new(a));
        aig.add_output("y".to_string(), and_lit);

        let mut pass = ConstProp::new();
        let _result = pass.run(&mut aig);

        // Output should reference input a
        let (_, out_lit) = &aig.outputs()[0];
        assert!(aig.get_node(out_lit.node).unwrap().is_input());
    }

    #[test]
    fn test_const_prop_and_complement() {
        let mut aig = Aig::new("test".to_string());
        let a = aig.add_input("a".to_string(), None);

        // a & !a should become 0
        // Note: add_and already simplifies this
        let and_lit = aig.add_and(AigLit::new(a), AigLit::not(a));
        aig.add_output("y".to_string(), and_lit);

        let mut pass = ConstProp::new();
        let _result = pass.run(&mut aig);

        // Output should be constant 0
        let (_, out_lit) = &aig.outputs()[0];
        assert_eq!(*out_lit, AigLit::false_lit());
        // Note: result.changed may be false because add_and already simplified
    }
}
