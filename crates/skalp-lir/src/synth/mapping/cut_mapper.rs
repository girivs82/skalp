//! Cut-Based Technology Mapping
//!
//! This module implements the core cut-based mapping algorithm that
//! maps an AIG to technology library cells.
//!
//! # Features
//!
//! - Priority-based cut selection (area, delay, balanced)
//! - Two-phase mapping: delay-optimal + area recovery
//! - Choice-aware mapping for exploring equivalent implementations

use super::{CellMatcher, CutMatch, MappedNode, MappingObjective, MappingStats};
use crate::synth::cuts::{CutEnumeration, CutParams, CutPriority};
use crate::synth::{Aig, AigLit, AigNode, AigNodeId, Cut};
use crate::tech_library::TechLibrary;
use indexmap::IndexMap;
use std::collections::HashSet;

/// Configuration for the cut-based mapper
#[derive(Debug, Clone)]
pub struct CutMapperConfig {
    /// Maximum cut size (K)
    pub cut_size: usize,
    /// Maximum cuts per node
    pub max_cuts: usize,
    /// Whether to use priority-based cut enumeration
    pub use_priority_cuts: bool,
    /// Whether to enable area recovery phase
    pub area_recovery: bool,
    /// Area recovery iterations
    pub recovery_iterations: usize,
    /// Whether to use choice nodes for exploration
    pub use_choices: bool,
}

impl Default for CutMapperConfig {
    fn default() -> Self {
        Self {
            cut_size: 4,
            max_cuts: 12,
            use_priority_cuts: true,
            area_recovery: true,
            recovery_iterations: 4,
            use_choices: false,
        }
    }
}

impl CutMapperConfig {
    /// Configuration for fast mapping (minimal optimization)
    pub fn fast() -> Self {
        Self {
            cut_size: 4,
            max_cuts: 4,
            use_priority_cuts: false,
            area_recovery: false,
            recovery_iterations: 0,
            use_choices: false,
        }
    }

    /// Configuration for best quality mapping
    pub fn quality() -> Self {
        Self {
            cut_size: 6,
            max_cuts: 12,
            use_priority_cuts: true,
            area_recovery: true,
            recovery_iterations: 3,
            use_choices: true,
        }
    }

    /// Set cut size (for configuring based on library LUT size)
    pub fn with_cut_size(mut self, k: usize) -> Self {
        self.cut_size = k;
        self
    }

    /// Configuration optimized for a specific library
    ///
    /// For FPGA libraries with lut_size, uses that as the cut size.
    /// For ASIC libraries, uses a larger cut size for more optimization freedom.
    pub fn for_library(library: &crate::tech_library::TechLibrary) -> Self {
        let cut_size = library.get_lut_size();
        Self {
            cut_size,
            max_cuts: if cut_size <= 4 { 12 } else { 16 },
            use_priority_cuts: true,
            area_recovery: true,
            recovery_iterations: 5,
            use_choices: false,
        }
    }

    /// Fast configuration optimized for a specific library
    pub fn fast_for_library(library: &crate::tech_library::TechLibrary) -> Self {
        Self {
            cut_size: library.get_lut_size(),
            max_cuts: 4,
            use_priority_cuts: false,
            area_recovery: false,
            recovery_iterations: 0,
            use_choices: false,
        }
    }

    /// Quality configuration optimized for a specific library
    pub fn quality_for_library(library: &crate::tech_library::TechLibrary) -> Self {
        let cut_size = library.get_lut_size();
        Self {
            cut_size,
            max_cuts: if cut_size <= 4 { 16 } else { 20 },
            use_priority_cuts: true,
            area_recovery: true,
            recovery_iterations: 8,
            use_choices: true,
        }
    }
}

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
    /// Mapper configuration
    config: CutMapperConfig,
}

impl CutMapper {
    /// Create a mapper that uses cells from a technology library
    ///
    /// This is the only way to create a CutMapper for production use.
    /// The mapper will only use cells available in the library.
    pub fn from_library(library: &TechLibrary) -> Self {
        Self {
            matcher: CellMatcher::from_library(library),
            cut_params: CutParams::default(),
            objective: MappingObjective::Balanced,
            area_weight: 1.0,
            delay_weight: 1.0,
            config: CutMapperConfig::default(),
        }
    }

    /// Create mapper from library with specific configuration
    pub fn from_library_with_config(library: &TechLibrary, config: CutMapperConfig) -> Self {
        let cut_params = CutParams {
            k: config.cut_size,
            max_cuts: config.max_cuts,
            use_choices: config.use_choices,
            ..Default::default()
        };

        Self {
            matcher: CellMatcher::from_library(library),
            cut_params,
            objective: MappingObjective::Balanced,
            area_weight: 1.0,
            delay_weight: 1.0,
            config,
        }
    }

    /// Create mapper from library with specific objective
    pub fn from_library_with_objective(library: &TechLibrary, objective: MappingObjective) -> Self {
        Self {
            matcher: CellMatcher::from_library(library),
            cut_params: CutParams::default(),
            objective,
            area_weight: 1.0,
            delay_weight: 1.0,
            config: CutMapperConfig::default(),
        }
    }

    /// Create a new mapper with hardcoded cells for testing only
    #[cfg(test)]
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Self {
            matcher: CellMatcher::new(),
            cut_params: CutParams::default(),
            objective: MappingObjective::Balanced,
            area_weight: 1.0,
            delay_weight: 1.0,
            config: CutMapperConfig::default(),
        }
    }

    /// Create mapper with specific objective for testing only
    #[cfg(test)]
    pub fn with_objective(objective: MappingObjective) -> Self {
        Self {
            matcher: CellMatcher::new(),
            cut_params: CutParams::default(),
            objective,
            area_weight: 1.0,
            delay_weight: 1.0,
            config: CutMapperConfig::default(),
        }
    }

    /// Create mapper with specific configuration for testing only
    #[cfg(test)]
    pub fn with_config(config: CutMapperConfig) -> Self {
        let cut_params = CutParams {
            k: config.cut_size,
            max_cuts: config.max_cuts,
            use_choices: config.use_choices,
            ..Default::default()
        };

        Self {
            matcher: CellMatcher::new(),
            cut_params,
            objective: MappingObjective::Balanced,
            area_weight: 1.0,
            delay_weight: 1.0,
            config,
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

    /// Set configuration
    pub fn set_config(&mut self, config: CutMapperConfig) {
        self.config = config.clone();
        self.cut_params.k = config.cut_size;
        self.cut_params.max_cuts = config.max_cuts;
        self.cut_params.use_choices = config.use_choices;
    }

    /// Map an AIG to a technology netlist
    pub fn map(&self, aig: &Aig) -> MappingResult {
        if self.config.use_priority_cuts && self.config.area_recovery {
            self.map_with_recovery(aig)
        } else if self.config.use_priority_cuts {
            self.map_with_priority(aig)
        } else {
            self.map_basic(aig)
        }
    }

    /// Two-phase mapping: delay-optimal followed by area recovery
    fn map_with_recovery(&self, aig: &Aig) -> MappingResult {
        // Phase 1: Delay-optimal mapping
        let mut params = self.cut_params.clone();
        params.priority = CutPriority::Delay;

        let has_any_choices = aig.choice_node_count() > 0;
        let cuts = if self.config.use_choices && has_any_choices {
            CutEnumeration::enumerate_with_choices(aig, params.clone())
        } else {
            CutEnumeration::enumerate_with_priority(aig, params.clone())
        };

        let mut result = self.map_internal(aig, &cuts);
        let initial_delay = result.stats.critical_delay;

        // Phase 2: Area recovery with delay bound
        if self.config.area_recovery && self.config.recovery_iterations > 0 {
            let delay_bound = initial_delay * 1.05; // Allow 5% slack

            for _ in 0..self.config.recovery_iterations {
                params.priority = CutPriority::AreaWithDelayBound;
                params.delay_bound = delay_bound as f32;

                let recovery_cuts = if self.config.use_choices {
                    CutEnumeration::enumerate_with_choices(aig, params.clone())
                } else {
                    CutEnumeration::enumerate_with_priority(aig, params.clone())
                };

                let recovery_result = self.map_internal(aig, &recovery_cuts);

                // Accept if area improved without violating delay
                if recovery_result.stats.total_area < result.stats.total_area
                    && recovery_result.stats.critical_delay <= delay_bound
                {
                    result = recovery_result;
                }
            }
        }

        result
    }

    /// Priority-based mapping (single phase)
    fn map_with_priority(&self, aig: &Aig) -> MappingResult {
        let mut params = self.cut_params.clone();
        params.priority = match self.objective {
            MappingObjective::Area => CutPriority::Area,
            MappingObjective::Delay => CutPriority::Delay,
            MappingObjective::Balanced => CutPriority::AreaDelay,
        };

        let cuts = if self.config.use_choices {
            CutEnumeration::enumerate_with_choices(aig, params)
        } else {
            CutEnumeration::enumerate_with_priority(aig, params)
        };

        self.map_internal(aig, &cuts)
    }

    /// Basic mapping without priority cuts
    fn map_basic(&self, aig: &Aig) -> MappingResult {
        let cuts = CutEnumeration::enumerate(aig, self.cut_params.clone());
        self.map_internal(aig, &cuts)
    }

    /// Internal mapping implementation — ABC's `if` algorithm.
    ///
    /// Three phases:
    /// 1. **Forward pass**: Compute delay-optimal mapping for each node
    /// 2. **Backward covering**: Select which nodes need cells (from outputs back)
    /// 3. **Area recovery**: Re-select cuts to minimize area within delay budget
    ///
    /// Reference: Mishchenko, Chatterjee, Brayton. "Combinational and sequential
    /// mapping with priority cuts." DAC 2007.
    fn map_internal(&self, aig: &Aig, cuts: &CutEnumeration) -> MappingResult {
        let topo_order = self.topological_sort(aig);

        // ──────────────────────────────────────────────────────────────
        // Phase 1: Forward pass — compute best cut for each AND node
        // ──────────────────────────────────────────────────────────────
        // For each node, store: (best_cut, delay, area_flow)
        let mut node_delay: IndexMap<AigNodeId, f64> = IndexMap::new();
        let mut node_area_flow: IndexMap<AigNodeId, f64> = IndexMap::new();
        let mut node_best_cut: IndexMap<AigNodeId, Option<CutMatch>> = IndexMap::new();

        // Compute fanout counts for area flow
        let mut fanout_count: IndexMap<AigNodeId, usize> = IndexMap::new();
        for (id, node) in aig.iter_nodes() {
            if let AigNode::And { left, right } = node {
                *fanout_count.entry(left.node).or_insert(0) += 1;
                *fanout_count.entry(right.node).or_insert(0) += 1;
            }
            if let AigNode::Latch { data, .. } = node {
                *fanout_count.entry(data.node).or_insert(0) += 1;
            }
        }
        // Output consumers
        for (_, lit) in aig.outputs() {
            *fanout_count.entry(lit.node).or_insert(0) += 1;
        }

        for &node_id in &topo_order {
            let Some(node) = aig.get_node(node_id) else {
                continue;
            };

            match node {
                AigNode::Input { .. } | AigNode::Const => {
                    node_delay.insert(node_id, 0.0);
                    node_area_flow.insert(node_id, 0.0);
                    node_best_cut.insert(node_id, None);
                }
                AigNode::Latch { .. } | AigNode::Barrier { .. } => {
                    node_delay.insert(node_id, 0.0); // Latch output starts a new timing path
                    node_area_flow.insert(node_id, 0.0);
                    node_best_cut.insert(node_id, None);
                }
                AigNode::And { left, right } => {
                    let best =
                        self.find_best_match_if(aig, node_id, cuts, &node_delay, &node_area_flow, &fanout_count);

                    if let Some(ref match_) = best {
                        let arrival = self.compute_arrival(&match_.cut, &node_delay);
                        node_delay.insert(node_id, arrival + match_.delay);

                        // ABC-style area flow: each leaf's contribution divided by its fanout
                        let leaf_area: f64 = match_
                            .cut
                            .leaves
                            .iter()
                            .map(|l| {
                                let af = node_area_flow.get(l).copied().unwrap_or(0.0);
                                let leaf_fo = fanout_count.get(l).copied().unwrap_or(1).max(1);
                                af / leaf_fo as f64
                            })
                            .sum();
                        let fo = fanout_count.get(&node_id).copied().unwrap_or(1).max(1);
                        node_area_flow.insert(node_id, (match_.area + leaf_area) / fo as f64);
                    } else {
                        // AND2 fallback
                        let left_d = node_delay.get(&left.node).copied().unwrap_or(0.0);
                        let right_d = node_delay.get(&right.node).copied().unwrap_or(0.0);
                        node_delay.insert(node_id, left_d.max(right_d) + 25.0);
                        node_area_flow.insert(node_id, 2.0);
                    }

                    node_best_cut.insert(node_id, best);
                }
            }
        }

        let global_required = node_delay.values().copied().fold(0.0f64, f64::max);

        // ──────────────────────────────────────────────────────────────
        // Phase 2: Backward covering — determine which nodes need cells
        // ──────────────────────────────────────────────────────────────
        // Start from outputs and latch data inputs, walk backward through
        // selected cuts. Only cut roots become mapped cells.
        let mut required_nodes: HashSet<AigNodeId> = HashSet::new();
        let mut worklist: Vec<AigNodeId> = Vec::new();

        // Seed with output drivers and latch data inputs
        for (_, lit) in aig.outputs() {
            if lit.node != AigNodeId::FALSE {
                worklist.push(lit.node);
            }
        }
        for (id, node) in aig.iter_nodes() {
            match node {
                AigNode::Latch { data, clock, reset, .. } => {
                    worklist.push(data.node);
                    if let Some(c) = clock {
                        worklist.push(*c);
                    }
                    if let Some(r) = reset {
                        worklist.push(*r);
                    }
                }
                AigNode::Barrier { data, enable, clock, reset, .. } => {
                    worklist.push(data.node);
                    if let Some(e) = enable {
                        worklist.push(e.node);
                    }
                    if let Some(c) = clock {
                        worklist.push(*c);
                    }
                    if let Some(r) = reset {
                        worklist.push(*r);
                    }
                }
                _ => {}
            }
        }

        while let Some(node_id) = worklist.pop() {
            if required_nodes.contains(&node_id) {
                continue;
            }
            required_nodes.insert(node_id);

            // If this node has a cut, its leaves become the next required nodes
            if let Some(Some(match_)) = node_best_cut.get(&node_id) {
                for &leaf in &match_.cut.leaves {
                    worklist.push(leaf);
                }
            } else if let Some(AigNode::And { left, right }) = aig.get_node(node_id) {
                // No cut — AND2 fallback, both children are required
                worklist.push(left.node);
                worklist.push(right.node);
            }
        }

        // ──────────────────────────────────────────────────────────────
        // Phase 3: Area recovery iterations
        // ──────────────────────────────────────────────────────────────
        // For each required AND node, try to find a cut with less area
        // that still meets the delay bound. Update area flow and re-cover.
        let delay_bound = global_required * 1.05;
        let recovery_iters = self.config.recovery_iterations.min(8);

        for _iter in 0..recovery_iters {
            // Recompute area flow with exact local references (fanout-weighted)
            let mut ref_counts: IndexMap<AigNodeId, usize> = IndexMap::new();
            for &nid in &required_nodes {
                if let Some(Some(m)) = node_best_cut.get(&nid) {
                    for &leaf in &m.cut.leaves {
                        *ref_counts.entry(leaf).or_insert(0) += 1;
                    }
                } else if let Some(AigNode::And { left, right }) = aig.get_node(nid) {
                    *ref_counts.entry(left.node).or_insert(0) += 1;
                    *ref_counts.entry(right.node).or_insert(0) += 1;
                }
            }

            // Recompute area flow using reference counts from covering (ABC-style)
            for &node_id in &topo_order {
                if !required_nodes.contains(&node_id) {
                    continue;
                }
                if let Some(AigNode::And { .. }) = aig.get_node(node_id) {
                    if let Some(Some(match_)) = node_best_cut.get(&node_id) {
                        let leaf_area: f64 = match_
                            .cut
                            .leaves
                            .iter()
                            .map(|l| {
                                let af = node_area_flow.get(l).copied().unwrap_or(0.0);
                                let leaf_refs = ref_counts.get(l).copied().unwrap_or(1).max(1);
                                af / leaf_refs as f64
                            })
                            .sum();
                        let refs = ref_counts.get(&node_id).copied().unwrap_or(1).max(1);
                        node_area_flow
                            .insert(node_id, (match_.area + leaf_area) / refs as f64);
                    }
                }
            }

            let mut changed = false;

            // Try to find better (smaller area) cuts for each required AND node
            for &node_id in &topo_order {
                if !required_nodes.contains(&node_id) {
                    continue;
                }
                if !matches!(aig.get_node(node_id), Some(AigNode::And { .. })) {
                    continue;
                }

                let Some(cut_set) = cuts.get_cuts(node_id) else {
                    continue;
                };

                let mut best_area_flow = node_area_flow.get(&node_id).copied().unwrap_or(f64::MAX);

                for cut in &cut_set.cuts {
                    if cut.leaves.is_empty()
                        || (cut.leaves.len() == 1 && cut.leaves[0] == node_id)
                    {
                        continue;
                    }

                    // Check delay feasibility
                    let arrival = self.compute_arrival(cut, &node_delay);
                    let matches = self.matcher.find_matches(cut.truth_table, cut.leaves.len());

                    for match_ in matches {
                        let total_delay = arrival + match_.delay;
                        if total_delay > delay_bound {
                            continue;
                        }

                        // Compute area flow for this cut (ABC-style: leaf/fanout)
                        let leaf_area: f64 = cut
                            .leaves
                            .iter()
                            .map(|l| {
                                let af = node_area_flow.get(l).copied().unwrap_or(0.0);
                                let leaf_refs = ref_counts.get(l).copied().unwrap_or(1).max(1);
                                af / leaf_refs as f64
                            })
                            .sum();
                        let refs = ref_counts.get(&node_id).copied().unwrap_or(1).max(1);
                        let new_area_flow = (match_.area + leaf_area) / refs as f64;

                        // Track the BEST improvement across all cuts (not just the last)
                        if new_area_flow < best_area_flow {
                            best_area_flow = new_area_flow;
                            let mut new_match = match_.clone();
                            new_match.cut = cut.clone();
                            node_best_cut.insert(node_id, Some(new_match));
                            node_area_flow.insert(node_id, new_area_flow);
                            changed = true;
                            break; // Best match for this cut found; try next cut
                        }
                    }
                }
            }

            if !changed {
                break;
            }

            // Re-cover: recompute required_nodes with updated cuts
            required_nodes.clear();
            worklist.clear();
            for (_, lit) in aig.outputs() {
                if lit.node != AigNodeId::FALSE {
                    worklist.push(lit.node);
                }
            }
            for (_, node) in aig.iter_nodes() {
                match node {
                    AigNode::Latch { data, clock, reset, .. } => {
                        worklist.push(data.node);
                        if let Some(c) = clock {
                            worklist.push(*c);
                        }
                        if let Some(r) = reset {
                            worklist.push(*r);
                        }
                    }
                    AigNode::Barrier { data, enable, clock, reset, .. } => {
                        worklist.push(data.node);
                        if let Some(e) = enable {
                            worklist.push(e.node);
                        }
                        if let Some(c) = clock {
                            worklist.push(*c);
                        }
                        if let Some(r) = reset {
                            worklist.push(*r);
                        }
                    }
                    _ => {}
                }
            }
            while let Some(node_id) = worklist.pop() {
                if required_nodes.contains(&node_id) {
                    continue;
                }
                required_nodes.insert(node_id);
                if let Some(Some(match_)) = node_best_cut.get(&node_id) {
                    for &leaf in &match_.cut.leaves {
                        worklist.push(leaf);
                    }
                } else if let Some(AigNode::And { left, right }) = aig.get_node(node_id) {
                    worklist.push(left.node);
                    worklist.push(right.node);
                }
            }
        }

        // ──────────────────────────────────────────────────────────────
        // Phase 4: Build result — only map required AND nodes
        // ──────────────────────────────────────────────────────────────
        let mut result = MappingResult::new();

        for &node_id in &topo_order {
            if !required_nodes.contains(&node_id) {
                continue;
            }
            let Some(node) = aig.get_node(node_id) else {
                continue;
            };

            match node {
                AigNode::Latch { data, .. } => {
                    result.stats.record_cell("DFF_X1", 4.0);
                    result.mapped_nodes.insert(
                        node_id,
                        MappedNode {
                            cell_type: "DFF_X1".to_string(),
                            inputs: vec![(data.node, data.inverted)],
                            area: 4.0,
                            delay: 50.0,
                            output_inverted: false,
                            truth_table: None,
                            pin_mapping: None,
                        },
                    );
                }
                AigNode::Barrier { data, .. } => {
                    result.stats.record_cell("BARRIER_X1", 2.0);
                    result.mapped_nodes.insert(
                        node_id,
                        MappedNode {
                            cell_type: "BARRIER_X1".to_string(),
                            inputs: vec![(data.node, data.inverted)],
                            area: 2.0,
                            delay: 30.0,
                            output_inverted: false,
                            truth_table: None,
                            pin_mapping: None,
                        },
                    );
                }
                AigNode::And { left, right } => {
                    if let Some(Some(match_)) = node_best_cut.get(&node_id) {
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
                                output_inverted: match_.output_inverted,
                                truth_table: Some(match_.cut.truth_table),
                                pin_mapping: Some(match_.pin_mapping.clone()),
                            },
                        );
                    } else {
                        // AND2 fallback
                        result.stats.record_cell("AND2_X1", 2.0);
                        result.mapped_nodes.insert(
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
                                truth_table: Some(0x8),
                                pin_mapping: None,
                            },
                        );
                    }
                }
                _ => {}
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
        node_delay: &IndexMap<AigNodeId, f64>,
        node_area: &IndexMap<AigNodeId, f64>,
    ) -> Option<CutMatch> {
        let cut_set = cuts.get_cuts(node_id)?;

        let mut best: Option<CutMatch> = None;
        let mut best_cost = f64::MAX;

        for cut in &cut_set.cuts {
            // Skip trivial cuts (empty leaves or self-referencing)
            if cut.leaves.is_empty() {
                continue;
            }
            if cut.leaves.len() == 1 && cut.leaves[0] == node_id {
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

    /// Find the best cut match for a node using ABC's `if` cost function.
    ///
    /// Uses delay as primary criterion, then area flow as tie-breaker.
    /// This ensures delay-optimal mapping in the forward pass.
    fn find_best_match_if(
        &self,
        aig: &Aig,
        node_id: AigNodeId,
        cuts: &CutEnumeration,
        node_delay: &IndexMap<AigNodeId, f64>,
        node_area_flow: &IndexMap<AigNodeId, f64>,
        fanout_count: &IndexMap<AigNodeId, usize>,
    ) -> Option<CutMatch> {
        let cut_set = cuts.get_cuts(node_id)?;

        let mut best: Option<CutMatch> = None;
        let mut best_delay = f64::MAX;
        let mut best_area_flow = f64::MAX;
        let mut best_num_leaves = usize::MAX;

        for cut in &cut_set.cuts {
            if cut.leaves.is_empty() {
                continue;
            }
            if cut.leaves.len() == 1 && cut.leaves[0] == node_id {
                continue;
            }

            let matches = self.matcher.find_matches(cut.truth_table, cut.leaves.len());

            for mut match_ in matches {
                match_.cut = cut.clone();

                let arrival = self.compute_arrival(cut, node_delay);
                let total_delay = arrival + match_.delay;

                // ABC-style area flow: divide each leaf's contribution by its fanout
                // to avoid over-counting shared nodes. This properly incentivizes
                // larger cuts that cover more internal logic.
                let leaf_area: f64 = cut
                    .leaves
                    .iter()
                    .map(|l| {
                        let af = node_area_flow.get(l).copied().unwrap_or(0.0);
                        let leaf_fo = fanout_count.get(l).copied().unwrap_or(1).max(1);
                        af / leaf_fo as f64
                    })
                    .sum();
                let fo = fanout_count.get(&node_id).copied().unwrap_or(1).max(1);
                let area_flow = (match_.area + leaf_area) / fo as f64;

                // Selection: minimize delay first, then area flow within delay band
                // Within the same delay tier, accept ANY area improvement (no 5% threshold)
                // and prefer larger cuts that cover more logic when area is similar
                let is_better = total_delay < best_delay - 0.01
                    || (total_delay < best_delay + 0.01
                        && (area_flow < best_area_flow - 0.001
                            || ((area_flow - best_area_flow).abs() < 0.001
                                && cut.leaves.len() > best_num_leaves)));

                if is_better {
                    best_delay = total_delay;
                    best_area_flow = area_flow;
                    best_num_leaves = cut.leaves.len();
                    best = Some(match_);
                }
            }
        }

        best
    }

    /// Compute arrival time for a cut (max of leaf arrivals)
    fn compute_arrival(&self, cut: &Cut, node_delay: &IndexMap<AigNodeId, f64>) -> f64 {
        cut.leaves
            .iter()
            .filter_map(|&leaf| node_delay.get(&leaf).copied())
            .fold(0.0, f64::max)
    }

    /// Compute area for a cut using area flow
    /// Area flow divides the cost of each leaf by its fanout count,
    /// giving a more accurate estimate of the marginal cost of using this cut
    fn compute_cut_area(&self, cut: &Cut, node_area: &IndexMap<AigNodeId, f64>) -> f64 {
        // Use the cut's pre-computed area_flow if available
        if cut.area_flow > 0.0 {
            return cut.area_flow as f64;
        }

        // Otherwise, sum the areas of the cut leaves
        // This gives an upper bound on the area contribution
        cut.leaves
            .iter()
            .filter_map(|&leaf| node_area.get(&leaf).copied())
            .sum()
    }

    /// Add inverters for inverted outputs
    fn add_output_inverters(
        &self,
        aig: &Aig,
        result: &mut MappingResult,
        node_delay: &IndexMap<AigNodeId, f64>,
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
    pub mapped_nodes: IndexMap<AigNodeId, MappedNode>,
    /// Mapping statistics
    pub stats: MappingStats,
}

impl MappingResult {
    /// Create new empty result
    pub fn new() -> Self {
        Self {
            mapped_nodes: IndexMap::new(),
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

    #[test]
    fn test_cut_mapper_config_default() {
        let config = CutMapperConfig::default();
        assert_eq!(config.cut_size, 4);
        assert_eq!(config.max_cuts, 12);
        assert!(config.use_priority_cuts);
        assert!(config.area_recovery);
        assert_eq!(config.recovery_iterations, 4);
        assert!(!config.use_choices);
    }

    #[test]
    fn test_cut_mapper_config_fast() {
        let config = CutMapperConfig::fast();
        assert!(!config.use_priority_cuts);
        assert!(!config.area_recovery);
        assert_eq!(config.max_cuts, 4);
    }

    #[test]
    fn test_cut_mapper_config_quality() {
        let config = CutMapperConfig::quality();
        assert!(config.use_priority_cuts);
        assert!(config.area_recovery);
        assert_eq!(config.cut_size, 6);
        assert_eq!(config.max_cuts, 12);
        assert!(config.use_choices);
    }

    #[test]
    fn test_cut_mapper_with_config() {
        let mut aig = Aig::new("test".to_string());
        let a = aig.add_input("a".to_string(), None);
        let b = aig.add_input("b".to_string(), None);
        let c = aig.add_input("c".to_string(), None);

        let ab = aig.add_and(AigLit::new(a), AigLit::new(b));
        let abc = aig.add_and(ab, AigLit::new(c));
        aig.add_output("y".to_string(), abc);

        // Test with fast config
        let mapper_fast = CutMapper::with_config(CutMapperConfig::fast());
        let result_fast = mapper_fast.map(&aig);
        assert!(result_fast.is_success());

        // Test with quality config
        let mapper_quality = CutMapper::with_config(CutMapperConfig::quality());
        let result_quality = mapper_quality.map(&aig);
        assert!(result_quality.is_success());
    }

    #[test]
    fn test_cut_mapper_priority_mapping() {
        let mut aig = Aig::new("test".to_string());
        let a = aig.add_input("a".to_string(), None);
        let b = aig.add_input("b".to_string(), None);
        let c = aig.add_input("c".to_string(), None);
        let d = aig.add_input("d".to_string(), None);

        // Create a more complex circuit for priority mapping to show difference
        let ab = aig.add_and(AigLit::new(a), AigLit::new(b));
        let cd = aig.add_and(AigLit::new(c), AigLit::new(d));
        let abcd = aig.add_and(ab, cd);
        aig.add_output("y".to_string(), abcd);

        // Priority mapping should work
        let config = CutMapperConfig {
            use_priority_cuts: true,
            area_recovery: false,
            ..Default::default()
        };

        let mapper = CutMapper::with_config(config);
        let result = mapper.map(&aig);

        assert!(result.is_success());
        assert!(result.stats.cell_count >= 1);
    }

    #[test]
    fn test_cut_mapper_area_recovery() {
        let mut aig = Aig::new("test".to_string());
        let a = aig.add_input("a".to_string(), None);
        let b = aig.add_input("b".to_string(), None);
        let c = aig.add_input("c".to_string(), None);

        let ab = aig.add_and(AigLit::new(a), AigLit::new(b));
        let abc = aig.add_and(ab, AigLit::new(c));
        aig.add_output("y".to_string(), abc);

        // Enable area recovery
        let config = CutMapperConfig {
            area_recovery: true,
            recovery_iterations: 2,
            ..Default::default()
        };

        let mapper = CutMapper::with_config(config);
        let result = mapper.map(&aig);

        assert!(result.is_success());
        // Area recovery should complete without error
        assert!(result.stats.critical_delay > 0.0);
    }

    #[test]
    fn test_cut_mapper_set_config() {
        let mut mapper = CutMapper::new();

        let config = CutMapperConfig::quality();
        mapper.set_config(config);

        let mut aig = Aig::new("test".to_string());
        let a = aig.add_input("a".to_string(), None);
        let b = aig.add_input("b".to_string(), None);
        let ab = aig.add_and(AigLit::new(a), AigLit::new(b));
        aig.add_output("y".to_string(), ab);

        let result = mapper.map(&aig);
        assert!(result.is_success());
    }
}
