//! Cut-Based Technology Mapping
//!
//! This module implements the core cut-based mapping algorithm that
//! maps an AIG to technology library cells.

use super::{CellMatcher, CutMatch, MappedNode, MappingObjective, MappingStats};
use crate::synth::cuts::{CutEnumeration, CutParams};
use crate::synth::{Aig, AigLit, AigNode, AigNodeId, Cut};
use std::collections::{HashMap, HashSet};

/// Cut-based technology mapper
pub struct CutMapper {
    /// Cell matcher
    matcher: CellMatcher,
    /// Cut parameters
    cut_params: CutParams,
    /// Mapping objective
    objective: MappingObjective,
    /// Area weight (for balanced objective)
    area_weight: f64,
    /// Delay weight (for balanced objective)
    delay_weight: f64,
}

impl Default for CutMapper {
    fn default() -> Self {
        Self::new()
    }
}

impl CutMapper {
    /// Create a new mapper with default settings
    pub fn new() -> Self {
        Self {
            matcher: CellMatcher::new(),
            cut_params: CutParams::default(),
            objective: MappingObjective::Balanced,
            area_weight: 1.0,
            delay_weight: 1.0,
        }
    }

    /// Create mapper with specific objective
    pub fn with_objective(objective: MappingObjective) -> Self {
        Self {
            matcher: CellMatcher::new(),
            cut_params: CutParams::default(),
            objective,
            area_weight: 1.0,
            delay_weight: 1.0,
        }
    }

    /// Set the mapping objective
    pub fn set_objective(&mut self, objective: MappingObjective) {
        self.objective = objective;
    }

    /// Set weights for balanced mapping
    pub fn set_weights(&mut self, area_weight: f64, delay_weight: f64) {
        self.area_weight = area_weight;
        self.delay_weight = delay_weight;
    }

    /// Map an AIG to a technology netlist
    pub fn map(&self, aig: &Aig) -> MappingResult {
        let mut result = MappingResult::new();

        // Step 1: Enumerate cuts
        let cuts = CutEnumeration::enumerate(aig, self.cut_params.clone());

        // Step 2: Compute best match for each node
        let mut best_match: HashMap<AigNodeId, Option<CutMatch>> = HashMap::new();
        let mut node_delay: HashMap<AigNodeId, f64> = HashMap::new();
        let mut node_area: HashMap<AigNodeId, f64> = HashMap::new();

        // Process nodes in topological order
        let topo_order = self.topological_sort(aig);

        for &node_id in &topo_order {
            let Some(node) = aig.get_node(node_id) else {
                continue;
            };

            match node {
                AigNode::Input { .. } | AigNode::Const => {
                    // Inputs and constants have zero cost
                    node_delay.insert(node_id, 0.0);
                    node_area.insert(node_id, 0.0);
                    best_match.insert(node_id, None);
                }

                AigNode::Latch { data, .. } => {
                    // Latches are mapped to DFF
                    let dff_delay = 50.0; // Clock-to-Q
                    let dff_area = 4.0;

                    node_delay.insert(node_id, dff_delay);
                    node_area.insert(node_id, dff_area);

                    result.stats.record_cell("DFF_X1", dff_area);
                    result.mapped_nodes.insert(
                        node_id,
                        MappedNode {
                            cell_type: "DFF_X1".to_string(),
                            inputs: vec![(data.node, data.inverted)],
                            area: dff_area,
                            delay: dff_delay,
                        },
                    );
                    best_match.insert(node_id, None);
                }

                AigNode::Barrier { data, .. } => {
                    // Barriers are power domain boundaries - treat as optimization barriers
                    // with a fixed mapping (they'll be preserved in the final netlist)
                    let barrier_delay = 30.0; // Level shifter/isolation cell delay
                    let barrier_area = 2.0;

                    node_delay.insert(node_id, barrier_delay);
                    node_area.insert(node_id, barrier_area);

                    result.stats.record_cell("BARRIER_X1", barrier_area);
                    result.mapped_nodes.insert(
                        node_id,
                        MappedNode {
                            cell_type: "BARRIER_X1".to_string(),
                            inputs: vec![(data.node, data.inverted)],
                            area: barrier_area,
                            delay: barrier_delay,
                        },
                    );
                    best_match.insert(node_id, None);
                }

                AigNode::And { left, right } => {
                    // Find best cut and cell match
                    let best = self.find_best_match(aig, node_id, &cuts, &node_delay, &node_area);

                    if let Some(match_) = &best {
                        let arrival = self.compute_arrival(&match_.cut, &node_delay);
                        let total_delay = arrival + match_.delay;
                        let total_area =
                            self.compute_cut_area(&match_.cut, &node_area) + match_.area;

                        node_delay.insert(node_id, total_delay);
                        node_area.insert(node_id, total_area);

                        // Create mapped node
                        let inputs: Vec<(AigNodeId, bool)> = match_
                            .cut
                            .leaves
                            .iter()
                            .zip(match_.input_inversions.iter())
                            .map(|(&leaf, &inv)| (leaf, inv))
                            .collect();

                        result.stats.record_cell(&match_.cell_type, match_.area);
                        result.mapped_nodes.insert(
                            node_id,
                            MappedNode {
                                cell_type: match_.cell_type.clone(),
                                inputs,
                                area: match_.area,
                                delay: match_.delay,
                            },
                        );
                    } else {
                        // Fallback: map AND gate directly
                        let and_delay = 25.0;
                        let and_area = 2.0;

                        let left_delay = node_delay.get(&left.node).copied().unwrap_or(0.0);
                        let right_delay = node_delay.get(&right.node).copied().unwrap_or(0.0);
                        let arrival = left_delay.max(right_delay);

                        node_delay.insert(node_id, arrival + and_delay);
                        node_area.insert(node_id, and_area);

                        result.stats.record_cell("AND2_X1", and_area);
                        result.mapped_nodes.insert(
                            node_id,
                            MappedNode {
                                cell_type: "AND2_X1".to_string(),
                                inputs: vec![
                                    (left.node, left.inverted),
                                    (right.node, right.inverted),
                                ],
                                area: and_area,
                                delay: and_delay,
                            },
                        );
                    }

                    best_match.insert(node_id, best);
                }
            }
        }

        // Step 3: Add inverters for inverted output signals
        self.add_output_inverters(aig, &mut result, &node_delay);

        // Update critical delay
        result.stats.critical_delay = node_delay.values().copied().fold(0.0, f64::max);

        result
    }

    /// Find the best cut match for a node
    fn find_best_match(
        &self,
        aig: &Aig,
        node_id: AigNodeId,
        cuts: &CutEnumeration,
        node_delay: &HashMap<AigNodeId, f64>,
        node_area: &HashMap<AigNodeId, f64>,
    ) -> Option<CutMatch> {
        let cut_set = cuts.get_cuts(node_id)?;

        let mut best: Option<CutMatch> = None;
        let mut best_cost = f64::MAX;

        for cut in &cut_set.cuts {
            // Skip trivial cuts
            if cut.leaves.is_empty() {
                continue;
            }

            // Find matching cells
            let matches = self.matcher.find_matches(cut.truth_table, cut.leaves.len());

            for mut match_ in matches {
                // Copy cut leaves to the match
                match_.cut = cut.clone();

                // Compute cost based on objective
                let arrival = self.compute_arrival(cut, node_delay);
                let area = self.compute_cut_area(cut, node_area);

                let cost = match self.objective {
                    MappingObjective::Area => area + match_.area,
                    MappingObjective::Delay => arrival + match_.delay,
                    MappingObjective::Balanced => {
                        self.area_weight * (area + match_.area)
                            + self.delay_weight * (arrival + match_.delay)
                    }
                };

                if cost < best_cost {
                    best_cost = cost;
                    best = Some(match_);
                }
            }
        }

        best
    }

    /// Compute arrival time for a cut (max of leaf arrivals)
    fn compute_arrival(&self, cut: &Cut, node_delay: &HashMap<AigNodeId, f64>) -> f64 {
        cut.leaves
            .iter()
            .filter_map(|&leaf| node_delay.get(&leaf).copied())
            .fold(0.0, f64::max)
    }

    /// Compute area for a cut (sum of leaf areas that are not shared)
    fn compute_cut_area(&self, cut: &Cut, node_area: &HashMap<AigNodeId, f64>) -> f64 {
        // For simplicity, we don't account for sharing here
        // A more sophisticated mapper would track fanout
        0.0
    }

    /// Add inverters for inverted outputs
    fn add_output_inverters(
        &self,
        aig: &Aig,
        result: &mut MappingResult,
        node_delay: &HashMap<AigNodeId, f64>,
    ) {
        for (name, lit) in aig.outputs() {
            if lit.inverted {
                // Need to add an inverter
                let inv_area = 1.0;
                let inv_delay = 15.0;

                result.stats.record_cell("INV_X1", inv_area);
                // Note: We're tracking this for statistics but not adding to mapped_nodes
                // since the output inverter is handled in netlisting
            }
        }
    }

    /// Topological sort of AIG nodes
    fn topological_sort(&self, aig: &Aig) -> Vec<AigNodeId> {
        let mut order = Vec::new();
        let mut visited = HashSet::new();

        fn visit(
            aig: &Aig,
            node: AigNodeId,
            visited: &mut HashSet<AigNodeId>,
            order: &mut Vec<AigNodeId>,
        ) {
            if visited.contains(&node) {
                return;
            }
            visited.insert(node);

            if let Some(n) = aig.get_node(node) {
                for fanin in n.fanins() {
                    visit(aig, fanin.node, visited, order);
                }
            }

            order.push(node);
        }

        for (id, _) in aig.iter_nodes() {
            visit(aig, id, &mut visited, &mut order);
        }

        order
    }
}

/// Result of technology mapping
#[derive(Debug, Clone)]
pub struct MappingResult {
    /// Mapped nodes
    pub mapped_nodes: HashMap<AigNodeId, MappedNode>,
    /// Mapping statistics
    pub stats: MappingStats,
}

impl MappingResult {
    /// Create new empty result
    pub fn new() -> Self {
        Self {
            mapped_nodes: HashMap::new(),
            stats: MappingStats::new(),
        }
    }

    /// Get a mapped node
    pub fn get_node(&self, id: AigNodeId) -> Option<&MappedNode> {
        self.mapped_nodes.get(&id)
    }

    /// Check if mapping was successful
    pub fn is_success(&self) -> bool {
        !self.mapped_nodes.is_empty()
    }
}

impl Default for MappingResult {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_cut_mapper_simple() {
        let mut aig = Aig::new("test".to_string());
        let a = aig.add_input("a".to_string(), None);
        let b = aig.add_input("b".to_string(), None);
        let ab = aig.add_and(AigLit::new(a), AigLit::new(b));
        aig.add_output("y".to_string(), ab);

        let mapper = CutMapper::new();
        let result = mapper.map(&aig);

        assert!(result.is_success());
        assert!(result.stats.cell_count >= 1);
    }

    #[test]
    fn test_cut_mapper_chain() {
        let mut aig = Aig::new("chain".to_string());
        let a = aig.add_input("a".to_string(), None);
        let b = aig.add_input("b".to_string(), None);
        let c = aig.add_input("c".to_string(), None);

        let ab = aig.add_and(AigLit::new(a), AigLit::new(b));
        let abc = aig.add_and(ab, AigLit::new(c));
        aig.add_output("y".to_string(), abc);

        let mapper = CutMapper::new();
        let result = mapper.map(&aig);

        assert!(result.is_success());
        // Should map to either AND3 or two AND2s
        assert!(result.stats.cell_count >= 1);
    }

    #[test]
    fn test_cut_mapper_objectives() {
        let mut aig = Aig::new("test".to_string());
        let a = aig.add_input("a".to_string(), None);
        let b = aig.add_input("b".to_string(), None);
        let ab = aig.add_and(AigLit::new(a), AigLit::new(b));
        aig.add_output("y".to_string(), ab);

        // Area-focused mapping
        let mapper_area = CutMapper::with_objective(MappingObjective::Area);
        let result_area = mapper_area.map(&aig);

        // Delay-focused mapping
        let mapper_delay = CutMapper::with_objective(MappingObjective::Delay);
        let result_delay = mapper_delay.map(&aig);

        // Both should succeed
        assert!(result_area.is_success());
        assert!(result_delay.is_success());
    }

    #[test]
    fn test_mapping_result() {
        let result = MappingResult::new();
        assert!(!result.is_success());
        assert!(result.get_node(AigNodeId(0)).is_none());
    }
}
