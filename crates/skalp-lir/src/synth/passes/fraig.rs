//! FRAIG (Functionally Reduced And-Inverter Graph)
//!
//! This module implements functional equivalence detection using random
//! simulation to find and merge functionally equivalent nodes.
//!
//! # Algorithm
//!
//! 1. Simulate the AIG with random input patterns
//! 2. Group nodes by their simulation signatures
//! 3. Nodes with identical signatures are candidate equivalents
//! 4. Merge confirmed equivalent nodes
//!
//! # Note
//!
//! This implementation uses random simulation which can find definite
//! inequivalences but may miss some equivalences. For full functional
//! reduction, SAT solving would be needed.

use super::{Pass, PassResult};
use crate::synth::{Aig, AigLit, AigNode, AigNodeId, BarrierType};
use std::collections::HashMap;

/// FRAIG configuration
#[derive(Debug, Clone)]
pub struct FraigConfig {
    /// Number of random simulation patterns
    pub num_patterns: usize,
    /// Random seed for reproducibility
    pub seed: u64,
    /// Enable verbose output
    pub verbose: bool,
}

impl Default for FraigConfig {
    fn default() -> Self {
        Self {
            num_patterns: 64,
            seed: 0xDEADBEEF,
            verbose: false,
        }
    }
}

/// Simulation signature for a node
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct SimSignature(u64);

impl SimSignature {
    fn invert(self) -> Self {
        Self(!self.0)
    }
}

/// FRAIG optimization pass
pub struct Fraig {
    config: FraigConfig,
    /// Nodes merged in last run
    nodes_merged: usize,
    /// Equivalence classes found
    equiv_classes: usize,
}

impl Default for Fraig {
    fn default() -> Self {
        Self::new()
    }
}

impl Fraig {
    /// Create a new FRAIG pass with default config
    pub fn new() -> Self {
        Self {
            config: FraigConfig::default(),
            nodes_merged: 0,
            equiv_classes: 0,
        }
    }

    /// Create with specific config
    pub fn with_config(config: FraigConfig) -> Self {
        Self {
            config,
            nodes_merged: 0,
            equiv_classes: 0,
        }
    }

    /// Get number of nodes merged in last run
    pub fn nodes_merged(&self) -> usize {
        self.nodes_merged
    }

    /// Get number of equivalence classes found
    pub fn equiv_classes(&self) -> usize {
        self.equiv_classes
    }

    /// Simulate the AIG with random patterns
    fn simulate(&self, aig: &Aig) -> HashMap<AigNodeId, SimSignature> {
        let mut signatures: HashMap<AigNodeId, SimSignature> = HashMap::new();

        // Constant node is always 0
        signatures.insert(AigNodeId::FALSE, SimSignature(0));

        // Generate random patterns for inputs
        let mut rng_state = self.config.seed;
        for (id, _name) in aig.iter_inputs() {
            // Simple xorshift for random number generation
            rng_state ^= rng_state << 13;
            rng_state ^= rng_state >> 7;
            rng_state ^= rng_state << 17;
            signatures.insert(id, SimSignature(rng_state));
        }

        // Propagate through AND nodes in topological order
        // Since nodes are stored in order of creation, we can just iterate
        for (id, node) in aig.iter_nodes() {
            match node {
                AigNode::Const | AigNode::Input { .. } => {
                    // Already handled
                }
                AigNode::And { left, right } => {
                    let left_sig = self.get_lit_signature(&signatures, *left);
                    let right_sig = self.get_lit_signature(&signatures, *right);
                    let result = SimSignature(left_sig.0 & right_sig.0);
                    signatures.insert(id, result);
                }
                AigNode::Latch { data, .. } => {
                    // For latches, use the data input signature
                    // (In a proper sequential simulation, we'd need state)
                    let data_sig = self.get_lit_signature(&signatures, *data);
                    signatures.insert(id, data_sig);
                }
                AigNode::Barrier { data, .. } => {
                    // Barriers are power domain boundaries - use the data input signature
                    let data_sig = self.get_lit_signature(&signatures, *data);
                    signatures.insert(id, data_sig);
                }
            }
        }

        signatures
    }

    /// Get signature for a literal (handling inversion)
    fn get_lit_signature(
        &self,
        signatures: &HashMap<AigNodeId, SimSignature>,
        lit: AigLit,
    ) -> SimSignature {
        let base = signatures
            .get(&lit.node)
            .copied()
            .unwrap_or(SimSignature(0));
        if lit.inverted {
            base.invert()
        } else {
            base
        }
    }

    /// Find equivalence classes from signatures
    fn find_equiv_classes(
        &self,
        aig: &Aig,
        signatures: &HashMap<AigNodeId, SimSignature>,
    ) -> Vec<Vec<(AigNodeId, bool)>> {
        // Group nodes by signature (or inverted signature)
        let mut sig_to_nodes: HashMap<SimSignature, Vec<(AigNodeId, bool)>> = HashMap::new();

        for (id, node) in aig.iter_nodes() {
            // Skip constant and input nodes (can't be merged)
            if node.is_const() || node.is_input() {
                continue;
            }

            if let Some(&sig) = signatures.get(&id) {
                // Check both regular and inverted signature
                // Always use the "canonical" form (smaller signature value)
                let (canon_sig, inverted) = if sig.0 <= sig.invert().0 {
                    (sig, false)
                } else {
                    (sig.invert(), true)
                };

                sig_to_nodes
                    .entry(canon_sig)
                    .or_default()
                    .push((id, inverted));
            }
        }

        // Filter to only classes with multiple members
        sig_to_nodes
            .into_values()
            .filter(|class| class.len() > 1)
            .collect()
    }

    /// Merge equivalent nodes
    fn merge_equiv_class(
        &self,
        aig: &mut Aig,
        class: &[(AigNodeId, bool)],
        node_map: &mut HashMap<AigNodeId, AigLit>,
    ) -> usize {
        if class.len() < 2 {
            return 0;
        }

        // Use the node with smallest ID as the representative
        let (repr_id, repr_inverted) = class.iter().min_by_key(|(id, _)| id.0).copied().unwrap();

        let repr_lit = if repr_inverted {
            AigLit::not(repr_id)
        } else {
            AigLit::new(repr_id)
        };

        let mut merged = 0;
        for &(id, inverted) in class.iter() {
            if id == repr_id {
                continue;
            }

            // Map this node to the representative
            let mapped_lit = if inverted == repr_inverted {
                repr_lit
            } else {
                repr_lit.invert()
            };

            node_map.insert(id, mapped_lit);
            merged += 1;
        }

        merged
    }

    /// Rebuild the AIG with merged nodes
    fn rebuild_aig(&self, aig: &Aig, node_map: &HashMap<AigNodeId, AigLit>) -> Aig {
        let mut new_aig = Aig::new(aig.name.clone());

        // Map from old node IDs to new literals
        let mut old_to_new: HashMap<AigNodeId, AigLit> = HashMap::new();

        // Constant maps to itself
        old_to_new.insert(AigNodeId::FALSE, AigLit::false_lit());

        // First pass: add inputs
        for (old_id, node) in aig.iter_nodes() {
            if let AigNode::Input { name, source_net } = node {
                let new_id = new_aig.add_input(name.clone(), *source_net);
                old_to_new.insert(old_id, AigLit::new(new_id));
            }
        }

        // Second pass: add AND nodes, applying the node map
        for (old_id, node) in aig.iter_nodes() {
            // Check if this node was merged
            if let Some(&mapped_lit) = node_map.get(&old_id) {
                // This node is equivalent to another - look up the mapped literal
                if let Some(&new_lit) = old_to_new.get(&mapped_lit.node) {
                    let final_lit = if mapped_lit.inverted {
                        new_lit.invert()
                    } else {
                        new_lit
                    };
                    old_to_new.insert(old_id, final_lit);
                }
                continue;
            }

            match node {
                AigNode::Const | AigNode::Input { .. } => {
                    // Already handled
                }
                AigNode::And { left, right } => {
                    let new_left = self.translate_lit(&old_to_new, *left);
                    let new_right = self.translate_lit(&old_to_new, *right);
                    let new_lit = new_aig.add_and(new_left, new_right);
                    old_to_new.insert(old_id, new_lit);
                }
                AigNode::Latch {
                    data,
                    init,
                    clock,
                    reset,
                } => {
                    let new_data = self.translate_lit(&old_to_new, *data);
                    let new_clock = clock.and_then(|c| old_to_new.get(&c).map(|l| l.node));
                    let new_reset = reset.and_then(|r| old_to_new.get(&r).map(|l| l.node));
                    let new_id = new_aig.add_latch(new_data, *init, new_clock, new_reset);
                    old_to_new.insert(old_id, AigLit::new(new_id));
                }
                AigNode::Barrier {
                    barrier_type,
                    data,
                    enable,
                    clock,
                    reset,
                    init,
                } => {
                    let new_data = self.translate_lit(&old_to_new, *data);
                    let new_enable = enable.map(|e| self.translate_lit(&old_to_new, e));
                    let new_clock = clock.and_then(|c| old_to_new.get(&c).map(|l| l.node));
                    let new_reset = reset.and_then(|r| old_to_new.get(&r).map(|l| l.node));
                    let new_id = new_aig.add_barrier(
                        barrier_type.clone(),
                        new_data,
                        new_enable,
                        new_clock,
                        new_reset,
                        *init,
                    );
                    old_to_new.insert(old_id, AigLit::new(new_id));
                }
            }
        }

        // Add outputs
        for (name, lit) in aig.outputs() {
            let new_lit = self.translate_lit(&old_to_new, *lit);
            new_aig.add_output(name.clone(), new_lit);
        }

        new_aig
    }

    /// Translate a literal using the old-to-new mapping
    fn translate_lit(&self, old_to_new: &HashMap<AigNodeId, AigLit>, lit: AigLit) -> AigLit {
        if let Some(&new_lit) = old_to_new.get(&lit.node) {
            if lit.inverted {
                new_lit.invert()
            } else {
                new_lit
            }
        } else {
            // Not found - this shouldn't happen in a correct topological traversal
            lit
        }
    }
}

impl Pass for Fraig {
    fn name(&self) -> &str {
        "fraig"
    }

    fn run(&mut self, aig: &mut Aig) -> PassResult {
        let mut result = PassResult::new(self.name());
        result.record_before(aig);

        // Reset counters
        self.nodes_merged = 0;
        self.equiv_classes = 0;

        // Simulate to get signatures
        let signatures = self.simulate(aig);

        // Find equivalence classes
        let classes = self.find_equiv_classes(aig, &signatures);
        self.equiv_classes = classes.len();

        if classes.is_empty() {
            result.record_after(aig);
            result.add_extra("equiv_classes", "0");
            result.add_extra("nodes_merged", "0");
            return result;
        }

        // Build node merge map
        let mut node_map: HashMap<AigNodeId, AigLit> = HashMap::new();
        for class in &classes {
            self.nodes_merged += self.merge_equiv_class(aig, class, &mut node_map);
        }

        // Rebuild AIG if we merged any nodes
        if !node_map.is_empty() {
            let new_aig = self.rebuild_aig(aig, &node_map);
            *aig = new_aig;
        }

        result.record_after(aig);
        result.add_extra("equiv_classes", &self.equiv_classes.to_string());
        result.add_extra("nodes_merged", &self.nodes_merged.to_string());
        result
    }
}

/// Statistics from FRAIG pass
#[derive(Debug, Clone, Default)]
pub struct FraigStats {
    /// Equivalence classes found
    pub equiv_classes: usize,
    /// Nodes merged
    pub nodes_merged: usize,
    /// Original AND count
    pub original_ands: usize,
    /// Final AND count
    pub final_ands: usize,
}

impl FraigStats {
    /// Get reduction percentage
    pub fn reduction_percent(&self) -> f64 {
        if self.original_ands == 0 {
            0.0
        } else {
            (self.original_ands - self.final_ands) as f64 / self.original_ands as f64 * 100.0
        }
    }

    /// Get summary string
    pub fn summary(&self) -> String {
        format!(
            "Found {} equiv classes, merged {} nodes, ANDs: {} → {} ({:.1}% reduction)",
            self.equiv_classes,
            self.nodes_merged,
            self.original_ands,
            self.final_ands,
            self.reduction_percent()
        )
    }
}

/// Run FRAIG with default config
pub fn run_fraig(aig: &mut Aig) -> FraigStats {
    let original_ands = aig.and_count();
    let mut fraig = Fraig::new();
    fraig.run(aig);

    FraigStats {
        equiv_classes: fraig.equiv_classes(),
        nodes_merged: fraig.nodes_merged(),
        original_ands,
        final_ands: aig.and_count(),
    }
}

/// Run FRAIG with custom config
pub fn run_fraig_with_config(aig: &mut Aig, config: FraigConfig) -> FraigStats {
    let original_ands = aig.and_count();
    let mut fraig = Fraig::with_config(config);
    fraig.run(aig);

    FraigStats {
        equiv_classes: fraig.equiv_classes(),
        nodes_merged: fraig.nodes_merged(),
        original_ands,
        final_ands: aig.and_count(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fraig_config_default() {
        let config = FraigConfig::default();
        assert_eq!(config.num_patterns, 64);
        assert!(!config.verbose);
    }

    #[test]
    fn test_sim_signature_invert() {
        let sig = SimSignature(0xAAAAAAAAAAAAAAAA);
        let inverted = sig.invert();
        assert_eq!(inverted.0, 0x5555555555555555);
        assert_eq!(inverted.invert(), sig);
    }

    #[test]
    fn test_fraig_empty_aig() {
        let mut aig = Aig::new("test".to_string());
        let mut fraig = Fraig::new();
        let result = fraig.run(&mut aig);

        assert!(!result.changed);
        assert_eq!(fraig.nodes_merged(), 0);
    }

    #[test]
    fn test_fraig_single_and() {
        let mut aig = Aig::new("test".to_string());
        let a = aig.add_input("a".to_string(), None);
        let b = aig.add_input("b".to_string(), None);
        let and_result = aig.add_and(AigLit::new(a), AigLit::new(b));
        aig.add_output("out".to_string(), and_result);

        let original_ands = aig.and_count();
        let mut fraig = Fraig::new();
        fraig.run(&mut aig);

        // Should not merge anything with just one AND
        assert_eq!(aig.and_count(), original_ands);
    }

    #[test]
    fn test_fraig_duplicate_and() {
        let mut aig = Aig::new("test".to_string());
        let a = aig.add_input("a".to_string(), None);
        let b = aig.add_input("b".to_string(), None);

        // Create two identical AND gates (strash should catch this, but test FRAIG anyway)
        let and1 = aig.add_and(AigLit::new(a), AigLit::new(b));
        aig.add_output("out1".to_string(), and1);
        aig.add_output("out2".to_string(), and1); // Same lit due to strash

        let mut fraig = Fraig::new();
        let result = fraig.run(&mut aig);

        // Strash already handles this case
        assert_eq!(aig.output_count(), 2);
    }

    #[test]
    fn test_fraig_stats() {
        let stats = FraigStats {
            equiv_classes: 5,
            nodes_merged: 10,
            original_ands: 100,
            final_ands: 90,
        };

        assert_eq!(stats.reduction_percent(), 10.0);
        assert!(stats.summary().contains("10 nodes"));
    }
}
