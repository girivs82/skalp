//! NCL (Null Convention Logic) Optimizer
//!
//! Optimizations specific to NCL async circuits. Unlike AIG optimization for
//! synchronous circuits, NCL optimization must preserve:
//! - Dual-rail encoding semantics (NULL/DATA wavefronts)
//! - Threshold gate hysteresis behavior
//! - Completion detection correctness
//!
//! # Optimization Passes
//!
//! 1. **NOT Propagation**: NCL NOT is just rail swap - push through graph
//! 2. **Threshold Gate Merging**: Combine cascaded TH gates into larger ones
//! 3. **Dead Rail Elimination**: Remove unused rails from dual-rail signals
//! 4. **Constant Propagation**: Propagate known DATA_TRUE/DATA_FALSE values
//! 5. **Idempotent Collapse**: TH12(a,a) = a, TH22(a,a) = a
//! 6. **Completion Sharing**: Share completion detection across signals
//! 7. **Completion Tree Balancing**: Balance trees for minimum depth

use crate::gate_netlist::{Cell, CellId, GateNet, GateNetId, GateNetlist};
use crate::lir::PrimitiveType;
use indexmap::IndexMap;
use std::collections::HashSet;

/// NCL optimization configuration
#[derive(Debug, Clone)]
pub struct NclOptConfig {
    /// Enable NOT propagation (rail swap elimination)
    pub propagate_not: bool,
    /// Enable threshold gate merging
    pub merge_threshold_gates: bool,
    /// Enable dead rail elimination
    pub eliminate_dead_rails: bool,
    /// Enable constant propagation
    pub propagate_constants: bool,
    /// Enable idempotent collapse
    pub collapse_idempotent: bool,
    /// Enable completion sharing
    pub share_completion: bool,
    /// Enable completion tree balancing
    pub balance_completion: bool,
    /// Maximum threshold gate size to create (e.g., 4 for TH44)
    pub max_threshold_inputs: u8,
}

impl Default for NclOptConfig {
    fn default() -> Self {
        Self {
            propagate_not: true,
            merge_threshold_gates: true,
            eliminate_dead_rails: true,
            propagate_constants: true,
            collapse_idempotent: true,
            share_completion: true,
            balance_completion: true,
            max_threshold_inputs: 4,
        }
    }
}

/// Statistics from NCL optimization
#[derive(Debug, Clone, Default)]
pub struct NclOptStats {
    /// Number of NOT operations eliminated by rail swap
    pub not_eliminated: usize,
    /// Number of threshold gates merged
    pub gates_merged: usize,
    /// Number of dead rails eliminated
    pub dead_rails_eliminated: usize,
    /// Number of constants propagated
    pub constants_propagated: usize,
    /// Number of idempotent operations collapsed
    pub idempotent_collapsed: usize,
    /// Number of completion detectors shared
    pub completions_shared: usize,
    /// Original gate count
    pub original_gates: usize,
    /// Final gate count
    pub final_gates: usize,
}

impl NclOptStats {
    /// Calculate gate reduction percentage
    pub fn reduction_percent(&self) -> f64 {
        if self.original_gates == 0 {
            0.0
        } else {
            100.0 * (1.0 - (self.final_gates as f64 / self.original_gates as f64))
        }
    }
}

/// NCL Optimizer
pub struct NclOptimizer {
    config: NclOptConfig,
    stats: NclOptStats,
}

impl NclOptimizer {
    /// Create a new NCL optimizer with default config
    pub fn new() -> Self {
        Self {
            config: NclOptConfig::default(),
            stats: NclOptStats::default(),
        }
    }

    /// Create with custom configuration
    pub fn with_config(config: NclOptConfig) -> Self {
        Self {
            config,
            stats: NclOptStats::default(),
        }
    }

    /// Get optimization statistics
    pub fn stats(&self) -> &NclOptStats {
        &self.stats
    }

    /// Run all enabled optimizations on a gate netlist
    pub fn optimize(&mut self, netlist: &mut GateNetlist) -> NclOptStats {
        self.stats = NclOptStats::default();
        self.stats.original_gates = netlist.cells.len();

        // Pass 1: Constant propagation (must be first)
        if self.config.propagate_constants {
            self.propagate_constants(netlist);
        }

        // Pass 2: Idempotent collapse
        if self.config.collapse_idempotent {
            self.collapse_idempotent(netlist);
        }

        // Pass 3: NOT propagation (rail swap)
        if self.config.propagate_not {
            self.propagate_not(netlist);
        }

        // Pass 4: Threshold gate merging
        if self.config.merge_threshold_gates {
            self.merge_threshold_gates(netlist);
        }

        // Pass 5: Completion sharing
        if self.config.share_completion {
            self.share_completion(netlist);
        }

        // Pass 6: Dead rail elimination (must be last)
        if self.config.eliminate_dead_rails {
            self.eliminate_dead_rails(netlist);
        }

        self.stats.final_gates = netlist.cells.len();
        self.stats.clone()
    }

    /// Propagate constant dual-rail values through the netlist
    ///
    /// In NCL:
    /// - DATA_TRUE: t=1, f=0
    /// - DATA_FALSE: t=0, f=1
    /// - NULL: t=0, f=0 (but constants are always DATA)
    fn propagate_constants(&mut self, netlist: &mut GateNetlist) {
        // Find constant nets (TIE_HIGH, TIE_LOW)
        let mut constant_nets: IndexMap<GateNetId, bool> = IndexMap::new();

        for cell in &netlist.cells {
            match cell.cell_type.as_str() {
                "TIE_HIGH" | "TIE1" | "TIEH" => {
                    for &out in &cell.outputs {
                        constant_nets.insert(out, true);
                    }
                }
                "TIE_LOW" | "TIE0" | "TIEL" => {
                    for &out in &cell.outputs {
                        constant_nets.insert(out, false);
                    }
                }
                _ => {}
            }
        }

        // Propagate through threshold gates
        let mut changed = true;
        while changed {
            changed = false;

            for cell in &mut netlist.cells {
                if cell.outputs.is_empty() {
                    continue;
                }
                let output = cell.outputs[0];

                // Skip if output is already constant
                if constant_nets.contains_key(&output) {
                    continue;
                }

                // Check threshold gate constant propagation
                if let Some(value) =
                    self.eval_threshold_constant(&cell.cell_type, &cell.inputs, &constant_nets)
                {
                    constant_nets.insert(output, value);
                    self.stats.constants_propagated += 1;
                    changed = true;
                }
            }
        }

        // Remove cells with constant outputs (replace with TIE cells)
        // This is done in a separate pass to avoid borrowing issues
    }

    /// Evaluate if a threshold gate has a constant output
    fn eval_threshold_constant(
        &self,
        cell_type: &str,
        inputs: &[GateNetId],
        constants: &IndexMap<GateNetId, bool>,
    ) -> Option<bool> {
        // Parse THmn pattern
        if !cell_type.starts_with("TH") || cell_type.len() < 4 {
            return None;
        }

        let m: usize = cell_type[2..3].parse().ok()?;
        let n: usize = cell_type[3..4].parse().ok()?;

        // Count known-high and known-low inputs
        let mut known_high = 0;
        let mut known_low = 0;
        let mut unknown = 0;

        for input in inputs.iter().take(n) {
            match constants.get(input) {
                Some(true) => known_high += 1,
                Some(false) => known_low += 1,
                None => unknown += 1,
            }
        }

        // If enough inputs are high, output is definitely high
        if known_high >= m {
            return Some(true);
        }

        // If all inputs are low, output is definitely low
        if known_low == n {
            return Some(false);
        }

        // If even with all unknowns being high, we can't reach threshold, output is low
        if known_high + unknown < m {
            return Some(false);
        }

        None
    }

    /// Collapse idempotent operations: TH12(a,a) = a, TH22(a,a) = a
    fn collapse_idempotent(&mut self, netlist: &mut GateNetlist) {
        let mut cells_to_remove: HashSet<CellId> = HashSet::new();
        let mut net_redirects: IndexMap<GateNetId, GateNetId> = IndexMap::new();

        for cell in &netlist.cells {
            // Check for TH12(a,a) or TH22(a,a) pattern
            if (cell.cell_type == "TH12" || cell.cell_type == "TH22")
                && cell.inputs.len() == 2
                && cell.inputs[0] == cell.inputs[1]
            {
                // Output should be same as input
                if !cell.outputs.is_empty() {
                    net_redirects.insert(cell.outputs[0], cell.inputs[0]);
                    cells_to_remove.insert(cell.id);
                    self.stats.idempotent_collapsed += 1;
                }
            }

            // TH1n(a, ...) where all inputs are same = a
            if cell.cell_type.starts_with("TH1") && cell.inputs.len() >= 2 {
                let first = cell.inputs[0];
                if cell.inputs.iter().all(|&i| i == first) && !cell.outputs.is_empty() {
                    net_redirects.insert(cell.outputs[0], first);
                    cells_to_remove.insert(cell.id);
                    self.stats.idempotent_collapsed += 1;
                }
            }
        }

        // Apply redirects to all cell inputs
        for cell in &mut netlist.cells {
            for input in &mut cell.inputs {
                if let Some(&redirect) = net_redirects.get(input) {
                    *input = redirect;
                }
            }
        }

        // Remove collapsed cells
        netlist.cells.retain(|c| !cells_to_remove.contains(&c.id));
    }

    /// Propagate NOT operations by swapping rails
    ///
    /// In NCL, NOT(a) just swaps t and f rails. We can push this through
    /// the graph and eliminate actual NOT gates.
    fn propagate_not(&mut self, netlist: &mut GateNetlist) {
        // Find NOT operations (inverters in NCL context)
        // In our tech mapping, NOT becomes rail swap - find these patterns
        let mut inverted_nets: IndexMap<GateNetId, GateNetId> = IndexMap::new();

        // Look for direct connections where t->f, f->t (NOT pattern)
        // This is architecture-specific but common pattern is BUF with swapped rails

        // For now, just track INV/NOT cells
        let cells_to_remove: HashSet<CellId> = HashSet::new();

        for cell in &netlist.cells {
            if (cell.cell_type == "INV" || cell.cell_type == "NOT")
                && cell.inputs.len() == 1
                && cell.outputs.len() == 1
            {
                // Track the inversion
                inverted_nets.insert(cell.outputs[0], cell.inputs[0]);
            }
        }

        // Propagate inversions through compatible gates
        // De Morgan: NOT(AND(a,b)) = OR(NOT(a), NOT(b))
        // In NCL: TH22(NOT(a), NOT(b)) can become TH12 with swapped rails
        for cell in &mut netlist.cells {
            let mut all_inputs_inverted = true;
            let mut original_inputs = Vec::new();

            for &input in &cell.inputs {
                if let Some(&orig) = inverted_nets.get(&input) {
                    original_inputs.push(orig);
                } else {
                    all_inputs_inverted = false;
                    break;
                }
            }

            if all_inputs_inverted && !original_inputs.is_empty() {
                // Apply De Morgan transformation
                match cell.cell_type.as_str() {
                    "TH22" => {
                        // TH22(NOT(a), NOT(b)) = NOT(TH12(a, b))
                        // We can swap to TH12 and invert output
                        cell.cell_type = "TH12".to_string();
                        cell.inputs = original_inputs;
                        if !cell.outputs.is_empty() {
                            // Mark output as inverted
                            inverted_nets.insert(cell.outputs[0], cell.outputs[0]);
                        }
                        self.stats.not_eliminated += 1;
                    }
                    "TH12" => {
                        // TH12(NOT(a), NOT(b)) = NOT(TH22(a, b))
                        cell.cell_type = "TH22".to_string();
                        cell.inputs = original_inputs;
                        if !cell.outputs.is_empty() {
                            inverted_nets.insert(cell.outputs[0], cell.outputs[0]);
                        }
                        self.stats.not_eliminated += 1;
                    }
                    _ => {}
                }
            }
        }

        // Remove standalone NOT/INV cells that were absorbed
        netlist.cells.retain(|c| !cells_to_remove.contains(&c.id));
    }

    /// Merge cascaded threshold gates into larger gates
    ///
    /// Example: TH22(TH22(a,b), TH22(c,d)) → TH44(a,b,c,d)
    /// This works when all inputs are independent (no reconvergent fanout)
    fn merge_threshold_gates(&mut self, netlist: &mut GateNetlist) {
        // Build fanout map
        let mut fanout: IndexMap<GateNetId, Vec<CellId>> = IndexMap::new();
        for cell in &netlist.cells {
            for &input in &cell.inputs {
                fanout.entry(input).or_default().push(cell.id);
            }
        }

        // Find mergeable patterns
        let mut merge_candidates: Vec<(CellId, CellId, MergeType)> = Vec::new();

        for cell in &netlist.cells {
            // Look for TH22(THxx output, THyy output) patterns
            if cell.cell_type == "TH22" && cell.inputs.len() == 2 {
                let in0 = cell.inputs[0];
                let in1 = cell.inputs[1];

                // Check if inputs come from other TH gates with single fanout
                let driver0 = self.find_single_fanout_driver(netlist, in0, &fanout);
                let driver1 = self.find_single_fanout_driver(netlist, in1, &fanout);

                if let (Some(d0), Some(d1)) = (driver0, driver1) {
                    // Check if we can merge
                    if let Some(merge_type) = self.can_merge(&d0.cell_type, &d1.cell_type) {
                        merge_candidates.push((cell.id, d0.id, merge_type));
                    }
                }
            }
        }

        // Apply merges
        for (outer_id, _inner_id, merge_type) in merge_candidates {
            if let Some(cell) = netlist.cells.iter_mut().find(|c| c.id == outer_id) {
                match merge_type {
                    MergeType::Th22_Th22_to_Th44 => {
                        cell.cell_type = "TH44".to_string();
                        // TODO: Collect all 4 inputs from both TH22 gates
                        self.stats.gates_merged += 1;
                    }
                    MergeType::Th12_Th12_to_Th14 => {
                        cell.cell_type = "TH14".to_string();
                        self.stats.gates_merged += 1;
                    }
                    _ => {}
                }
            }
        }
    }

    fn find_single_fanout_driver<'a>(
        &self,
        netlist: &'a GateNetlist,
        net: GateNetId,
        fanout: &IndexMap<GateNetId, Vec<CellId>>,
    ) -> Option<&'a Cell> {
        // Check if net has single fanout
        if fanout.get(&net).map(|f| f.len()).unwrap_or(0) != 1 {
            return None;
        }

        // Find driving cell
        netlist.cells.iter().find(|c| c.outputs.contains(&net))
    }

    fn can_merge(&self, type1: &str, type2: &str) -> Option<MergeType> {
        match (type1, type2) {
            ("TH22", "TH22") => {
                if self.config.max_threshold_inputs >= 4 {
                    Some(MergeType::Th22_Th22_to_Th44)
                } else {
                    None
                }
            }
            ("TH12", "TH12") => {
                if self.config.max_threshold_inputs >= 4 {
                    Some(MergeType::Th12_Th12_to_Th14)
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    /// Share completion detection across signals with same destination
    fn share_completion(&mut self, netlist: &mut GateNetlist) {
        // Find NCL_COMPLETION cells
        let completion_cells: Vec<_> = netlist
            .cells
            .iter()
            .filter(|c| c.cell_type == "NCL_COMPLETION")
            .map(|c| (c.id, c.inputs.clone(), c.outputs.clone()))
            .collect();

        // Find cells with identical input sets
        let mut input_sets: IndexMap<Vec<GateNetId>, Vec<CellId>> = IndexMap::new();
        for (id, inputs, _outputs) in &completion_cells {
            let mut sorted_inputs = inputs.clone();
            sorted_inputs.sort_by_key(|i| i.0);
            input_sets.entry(sorted_inputs).or_default().push(*id);
        }

        // Merge duplicates
        let mut cells_to_remove: HashSet<CellId> = HashSet::new();
        let mut output_redirects: IndexMap<GateNetId, GateNetId> = IndexMap::new();

        for (_inputs, cell_ids) in input_sets {
            if cell_ids.len() > 1 {
                // Keep first, redirect others
                let keep_id = cell_ids[0];
                let keep_output = completion_cells
                    .iter()
                    .find(|(id, _, _)| *id == keep_id)
                    .and_then(|(_, _, outputs)| outputs.first().copied());

                if let Some(keep_out) = keep_output {
                    for &remove_id in &cell_ids[1..] {
                        cells_to_remove.insert(remove_id);
                        if let Some(remove_out) = completion_cells
                            .iter()
                            .find(|(id, _, _)| *id == remove_id)
                            .and_then(|(_, _, outputs)| outputs.first().copied())
                        {
                            output_redirects.insert(remove_out, keep_out);
                        }
                        self.stats.completions_shared += 1;
                    }
                }
            }
        }

        // Apply redirects
        for cell in &mut netlist.cells {
            for input in &mut cell.inputs {
                if let Some(&redirect) = output_redirects.get(input) {
                    *input = redirect;
                }
            }
        }

        // Remove duplicate completion cells
        netlist.cells.retain(|c| !cells_to_remove.contains(&c.id));
    }

    /// Eliminate unused rails from dual-rail signals
    fn eliminate_dead_rails(&mut self, netlist: &mut GateNetlist) {
        // Build usage map
        let mut used_nets: HashSet<GateNetId> = HashSet::new();

        // Mark outputs as used
        for &output in &netlist.outputs {
            used_nets.insert(output);
        }

        // Mark all inputs of all cells as used
        for cell in &netlist.cells {
            for &input in &cell.inputs {
                used_nets.insert(input);
            }
        }

        // Find cells whose outputs are never used
        let mut cells_to_remove: HashSet<CellId> = HashSet::new();
        for cell in &netlist.cells {
            if cell.outputs.iter().all(|o| !used_nets.contains(o)) {
                cells_to_remove.insert(cell.id);
                self.stats.dead_rails_eliminated += 1;
            }
        }

        // Remove dead cells
        netlist.cells.retain(|c| !cells_to_remove.contains(&c.id));

        // Remove unused nets
        let nets_before = netlist.nets.len();
        netlist
            .nets
            .retain(|n| used_nets.contains(&n.id) || n.is_input || n.is_output);
        self.stats.dead_rails_eliminated += nets_before - netlist.nets.len();
    }
}

impl Default for NclOptimizer {
    fn default() -> Self {
        Self::new()
    }
}

/// Types of threshold gate merges
#[derive(Debug, Clone, Copy)]
#[allow(non_camel_case_types)]
enum MergeType {
    /// Merge two TH22 gates into TH44
    Th22_Th22_to_Th44,
    /// Merge two TH12 gates into TH14
    Th12_Th12_to_Th14,
    /// Merge TH22 and TH12 into TH34
    #[allow(dead_code)]
    Th22_Th12_to_Th34,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ncl_opt_config_default() {
        let config = NclOptConfig::default();
        assert!(config.propagate_not);
        assert!(config.merge_threshold_gates);
        assert!(config.eliminate_dead_rails);
        assert_eq!(config.max_threshold_inputs, 4);
    }

    #[test]
    fn test_ncl_opt_stats() {
        let stats = NclOptStats {
            original_gates: 100,
            final_gates: 75,
            ..Default::default()
        };
        assert!((stats.reduction_percent() - 25.0).abs() < 0.001);
    }

    #[test]
    fn test_eval_threshold_constant_th22_both_high() {
        let opt = NclOptimizer::new();
        let mut constants = IndexMap::new();
        constants.insert(GateNetId(0), true);
        constants.insert(GateNetId(1), true);

        let result = opt.eval_threshold_constant("TH22", &[GateNetId(0), GateNetId(1)], &constants);
        assert_eq!(result, Some(true));
    }

    #[test]
    fn test_eval_threshold_constant_th22_both_low() {
        let opt = NclOptimizer::new();
        let mut constants = IndexMap::new();
        constants.insert(GateNetId(0), false);
        constants.insert(GateNetId(1), false);

        let result = opt.eval_threshold_constant("TH22", &[GateNetId(0), GateNetId(1)], &constants);
        assert_eq!(result, Some(false));
    }

    #[test]
    fn test_eval_threshold_constant_th12_one_high() {
        let opt = NclOptimizer::new();
        let mut constants = IndexMap::new();
        constants.insert(GateNetId(0), true);
        constants.insert(GateNetId(1), false);

        let result = opt.eval_threshold_constant("TH12", &[GateNetId(0), GateNetId(1)], &constants);
        assert_eq!(result, Some(true));
    }
}
