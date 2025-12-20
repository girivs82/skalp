//! Delay-Optimal Technology Mapping
//!
//! This module implements timing-driven mapping that prioritizes
//! minimizing critical path delay.

use super::{CellMatcher, CutMatch, MappedNode, MappingStats};
use crate::synth::cuts::{CutEnumeration, CutParams};
use crate::synth::timing::{NetTiming, TimePs};
use crate::synth::{Aig, AigLit, AigNode, AigNodeId, Cut};
use std::collections::{HashMap, HashSet};

/// Configuration for delay-optimal mapping
#[derive(Debug, Clone)]
pub struct DelayMappingConfig {
    /// Target clock period (ps)
    pub target_period: TimePs,
    /// Slack threshold for area recovery
    pub slack_threshold: TimePs,
    /// Maximum cut size
    pub max_cut_size: usize,
    /// Whether to perform area recovery
    pub area_recovery: bool,
}

impl Default for DelayMappingConfig {
    fn default() -> Self {
        Self {
            target_period: 10000.0, // 10ns = 100MHz
            slack_threshold: 100.0, // 100ps
            max_cut_size: 4,
            area_recovery: true,
        }
    }
}

/// Delay-optimal technology mapper
pub struct DelayMapper {
    /// Cell matcher
    matcher: CellMatcher,
    /// Configuration
    config: DelayMappingConfig,
}

impl Default for DelayMapper {
    fn default() -> Self {
        Self::new()
    }
}

impl DelayMapper {
    /// Create a new delay mapper with default config
    pub fn new() -> Self {
        Self {
            matcher: CellMatcher::new(),
            config: DelayMappingConfig::default(),
        }
    }

    /// Create with specific configuration
    pub fn with_config(config: DelayMappingConfig) -> Self {
        Self {
            matcher: CellMatcher::new(),
            config,
        }
    }

    /// Set target clock period
    pub fn set_target_period(&mut self, period: TimePs) {
        self.config.target_period = period;
    }

    /// Map an AIG with delay optimization
    pub fn map(&self, aig: &Aig) -> DelayMappingResult {
        let mut result = DelayMappingResult::new();

        // Phase 1: Delay-optimal mapping
        let (mapping, arrival) = self.delay_optimal_mapping(aig);

        // Phase 2: Area recovery on non-critical paths (if enabled)
        let final_mapping = if self.config.area_recovery {
            self.area_recovery(aig, mapping, &arrival)
        } else {
            mapping
        };

        // Build result
        for (node_id, mapped) in final_mapping {
            result.stats.record_cell(&mapped.cell_type, mapped.area);
            result.mapped_nodes.insert(node_id, mapped);
        }

        // Compute critical delay
        result.stats.critical_delay = arrival.values().copied().fold(0.0, f64::max);

        result.timing_met = result.stats.critical_delay <= self.config.target_period;

        result
    }

    /// Perform delay-optimal mapping
    fn delay_optimal_mapping(
        &self,
        aig: &Aig,
    ) -> (HashMap<AigNodeId, MappedNode>, HashMap<AigNodeId, TimePs>) {
        let mut mapping: HashMap<AigNodeId, MappedNode> = HashMap::new();
        let mut arrival: HashMap<AigNodeId, TimePs> = HashMap::new();
        let mut best_cut: HashMap<AigNodeId, Option<Cut>> = HashMap::new();

        // Enumerate cuts
        let cut_params = CutParams {
            k: self.config.max_cut_size,
            ..Default::default()
        };
        let cuts = CutEnumeration::enumerate(aig, cut_params);

        // Process in topological order
        let topo_order = self.topological_sort(aig);

        for &node_id in &topo_order {
            let Some(node) = aig.get_node(node_id) else {
                continue;
            };

            match node {
                AigNode::Input { .. } | AigNode::Const => {
                    arrival.insert(node_id, 0.0);
                    best_cut.insert(node_id, None);
                }

                AigNode::Latch { data, .. } => {
                    // Latch resets timing path
                    let clk_to_q = 50.0;
                    arrival.insert(node_id, clk_to_q);

                    mapping.insert(
                        node_id,
                        MappedNode {
                            cell_type: "DFF_X1".to_string(),
                            inputs: vec![(data.node, data.inverted)],
                            area: 4.0,
                            delay: clk_to_q,
                            output_inverted: false,
                        },
                    );
                    best_cut.insert(node_id, None);
                }

                AigNode::Barrier { data, .. } => {
                    // Barriers are power domain boundaries - fixed delay
                    let barrier_delay = 30.0;
                    let data_arrival = arrival.get(&data.node).copied().unwrap_or(0.0);
                    arrival.insert(node_id, data_arrival + barrier_delay);

                    mapping.insert(
                        node_id,
                        MappedNode {
                            cell_type: "BARRIER_X1".to_string(),
                            inputs: vec![(data.node, data.inverted)],
                            area: 2.0,
                            delay: barrier_delay,
                            output_inverted: false,
                        },
                    );
                    best_cut.insert(node_id, None);
                }

                AigNode::And { left, right } => {
                    // Find minimum delay cut
                    let (min_delay, best, cell_match) =
                        self.find_min_delay_cut(aig, node_id, &cuts, &arrival);

                    arrival.insert(node_id, min_delay);
                    best_cut.insert(node_id, best.clone());

                    if let (Some(cut), Some(match_)) = (best, cell_match) {
                        let inputs: Vec<(AigNodeId, bool)> = cut
                            .leaves
                            .iter()
                            .zip(match_.input_inversions.iter())
                            .map(|(&leaf, &inv)| (leaf, inv))
                            .collect();

                        mapping.insert(
                            node_id,
                            MappedNode {
                                cell_type: match_.cell_type,
                                inputs,
                                area: match_.area,
                                delay: match_.delay,
                                output_inverted: match_.output_inverted,
                            },
                        );
                    } else {
                        // Fallback to AND2
                        let left_arr = arrival.get(&left.node).copied().unwrap_or(0.0);
                        let right_arr = arrival.get(&right.node).copied().unwrap_or(0.0);

                        mapping.insert(
                            node_id,
                            MappedNode {
                                cell_type: "AND2_X1".to_string(),
                                inputs: vec![
                                    (left.node, left.inverted),
                                    (right.node, right.inverted),
                                ],
                                area: 2.0,
                                delay: 25.0,
                                output_inverted: false,
                            },
                        );
                    }
                }
            }
        }

        (mapping, arrival)
    }

    /// Find the minimum delay cut for a node
    fn find_min_delay_cut(
        &self,
        aig: &Aig,
        node_id: AigNodeId,
        cuts: &CutEnumeration,
        arrival: &HashMap<AigNodeId, TimePs>,
    ) -> (TimePs, Option<Cut>, Option<CutMatch>) {
        let Some(cut_set) = cuts.get_cuts(node_id) else {
            return (f64::MAX, None, None);
        };

        let mut min_delay = f64::MAX;
        let mut best_cut: Option<Cut> = None;
        let mut best_match: Option<CutMatch> = None;

        for cut in &cut_set.cuts {
            if cut.leaves.is_empty() {
                continue;
            }

            // Compute arrival at cut leaves
            let max_leaf_arrival = cut
                .leaves
                .iter()
                .filter_map(|&leaf| arrival.get(&leaf).copied())
                .fold(0.0, f64::max);

            // Find matching cells
            let matches = self.matcher.find_matches(cut.truth_table, cut.leaves.len());

            for mut match_ in matches {
                let total_delay = max_leaf_arrival + match_.delay;

                if total_delay < min_delay {
                    min_delay = total_delay;
                    match_.cut = cut.clone();
                    best_cut = Some(cut.clone());
                    best_match = Some(match_);
                }
            }
        }

        // If no match found, use default AND gate timing
        if best_cut.is_none() {
            if let Some(AigNode::And { left, right }) = aig.get_node(node_id) {
                let left_arr = arrival.get(&left.node).copied().unwrap_or(0.0);
                let right_arr = arrival.get(&right.node).copied().unwrap_or(0.0);
                min_delay = left_arr.max(right_arr) + 25.0;
            }
        }

        (min_delay, best_cut, best_match)
    }

    /// Perform area recovery on non-critical paths
    fn area_recovery(
        &self,
        aig: &Aig,
        mut mapping: HashMap<AigNodeId, MappedNode>,
        arrival: &HashMap<AigNodeId, TimePs>,
    ) -> HashMap<AigNodeId, MappedNode> {
        // Compute required times
        let required = self.compute_required_times(aig, arrival);

        // Enumerate cuts for area-focused remapping
        let cut_params = CutParams {
            k: self.config.max_cut_size,
            ..Default::default()
        };
        let cuts = CutEnumeration::enumerate(aig, cut_params);

        // Find nodes with slack
        let slack_nodes: Vec<AigNodeId> = arrival
            .iter()
            .filter_map(|(&id, &arr)| {
                let req = required.get(&id).copied().unwrap_or(f64::MAX);
                let slack = req - arr;
                if slack > self.config.slack_threshold {
                    Some(id)
                } else {
                    None
                }
            })
            .collect();

        // Try to remap slack nodes with smaller cells
        for node_id in slack_nodes {
            let Some(current) = mapping.get(&node_id) else {
                continue;
            };
            let current_area = current.area;

            // Find a smaller cell that still meets timing
            if let Some(cut_set) = cuts.get_cuts(node_id) {
                for cut in &cut_set.cuts {
                    if cut.leaves.is_empty() {
                        continue;
                    }

                    let max_leaf_arrival = cut
                        .leaves
                        .iter()
                        .filter_map(|&leaf| arrival.get(&leaf).copied())
                        .fold(0.0, f64::max);

                    let req = required.get(&node_id).copied().unwrap_or(f64::MAX);
                    let available_delay = req - max_leaf_arrival;

                    // Find matches with smaller area
                    let matches = self.matcher.find_matches(cut.truth_table, cut.leaves.len());

                    for match_ in matches {
                        if match_.delay <= available_delay && match_.area < current_area {
                            // Found a smaller cell that meets timing
                            let inputs: Vec<(AigNodeId, bool)> = cut
                                .leaves
                                .iter()
                                .zip(match_.input_inversions.iter())
                                .map(|(&leaf, &inv)| (leaf, inv))
                                .collect();

                            mapping.insert(
                                node_id,
                                MappedNode {
                                    cell_type: match_.cell_type,
                                    inputs,
                                    area: match_.area,
                                    delay: match_.delay,
                                    output_inverted: match_.output_inverted,
                                },
                            );
                            break;
                        }
                    }
                }
            }
        }

        mapping
    }

    /// Compute required times (backward from outputs)
    fn compute_required_times(
        &self,
        aig: &Aig,
        arrival: &HashMap<AigNodeId, TimePs>,
    ) -> HashMap<AigNodeId, TimePs> {
        let mut required: HashMap<AigNodeId, TimePs> = HashMap::new();

        // Initialize outputs with target period
        for (_, lit) in aig.outputs() {
            required.insert(lit.node, self.config.target_period);
        }

        // Propagate backwards
        let topo_order = self.topological_sort(aig);
        for &node_id in topo_order.iter().rev() {
            let Some(node) = aig.get_node(node_id) else {
                continue;
            };

            let current_req = required.get(&node_id).copied().unwrap_or(f64::MAX);

            for fanin in node.fanins() {
                // Subtract gate delay
                let gate_delay = 25.0; // Conservative estimate
                let new_req = current_req - gate_delay;

                let existing = required.get(&fanin.node).copied().unwrap_or(f64::MAX);
                required.insert(fanin.node, existing.min(new_req));
            }
        }

        required
    }

    /// Topological sort
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

/// Result of delay-optimal mapping
#[derive(Debug, Clone)]
pub struct DelayMappingResult {
    /// Mapped nodes
    pub mapped_nodes: HashMap<AigNodeId, MappedNode>,
    /// Mapping statistics
    pub stats: MappingStats,
    /// Whether timing was met
    pub timing_met: bool,
}

impl DelayMappingResult {
    /// Create new empty result
    pub fn new() -> Self {
        Self {
            mapped_nodes: HashMap::new(),
            stats: MappingStats::new(),
            timing_met: false,
        }
    }

    /// Get timing slack
    pub fn slack(&self, target_period: TimePs) -> TimePs {
        target_period - self.stats.critical_delay
    }
}

impl Default for DelayMappingResult {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_delay_mapper_simple() {
        let mut aig = Aig::new("test".to_string());
        let a = aig.add_input("a".to_string(), None);
        let b = aig.add_input("b".to_string(), None);
        let ab = aig.add_and(AigLit::new(a), AigLit::new(b));
        aig.add_output("y".to_string(), ab);

        let mapper = DelayMapper::new();
        let result = mapper.map(&aig);

        assert!(result.timing_met); // With 10ns period, should easily meet timing
        assert!(!result.mapped_nodes.is_empty());
    }

    #[test]
    fn test_delay_mapper_config() {
        let config = DelayMappingConfig {
            target_period: 5000.0, // 5ns
            slack_threshold: 50.0,
            max_cut_size: 6,
            area_recovery: true,
        };

        let mapper = DelayMapper::with_config(config);
        assert_eq!(mapper.config.target_period, 5000.0);
    }

    #[test]
    fn test_delay_mapper_tight_timing() {
        let mut aig = Aig::new("tight".to_string());

        // Create a moderately long chain
        let first = aig.add_input("in".to_string(), None);
        let mut prev = AigLit::new(first);
        for i in 0..5 {
            let next = aig.add_input(format!("x{}", i), None);
            prev = aig.add_and(prev, AigLit::new(next));
        }
        aig.add_output("out".to_string(), prev);

        // Very tight timing
        let config = DelayMappingConfig {
            target_period: 20.0, // 20ps - very tight, less than a single gate
            ..Default::default()
        };
        let mapper = DelayMapper::with_config(config);
        let result = mapper.map(&aig);

        // With 20ps target and chain of gates, should violate
        assert!(!result.timing_met);
    }

    #[test]
    fn test_delay_mapping_result() {
        let mut result = DelayMappingResult::new();
        result.stats.critical_delay = 500.0;

        assert_eq!(result.slack(1000.0), 500.0);
        assert!(!result.timing_met);
    }
}
