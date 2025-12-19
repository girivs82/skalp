//! Gate-Level Netlist Optimization
//!
//! Optimization passes for `GateNetlist` to reduce gate count and improve FIT.
//!
//! # Passes
//!
//! 1. **ConstantFolding** - Evaluate cells with constant inputs
//! 2. **DeadCodeElimination** - Remove cells not driving outputs
//! 3. **BooleanSimplification** - Double negation, idempotency
//! 4. **MuxOptimization** - Constant select, identical inputs
//! 5. **BufferRemoval** - Remove unnecessary buffers
//!
//! # Example
//!
//! ```ignore
//! use skalp_lir::gate_optimizer::GateOptimizer;
//! use skalp_lir::gate_netlist::GateNetlist;
//!
//! let mut netlist = GateNetlist::new("test".into(), "generic_asic".into());
//! // ... populate netlist ...
//!
//! let mut optimizer = GateOptimizer::new();
//! let stats = optimizer.optimize(&mut netlist);
//! println!("Removed {} cells", stats.cells_removed);
//! ```

use crate::gate_netlist::{Cell, CellId, GateNet, GateNetId, GateNetlist};
use std::collections::{HashMap, HashSet};

// ============================================================================
// Optimization Statistics
// ============================================================================

/// Statistics from gate optimization
#[derive(Debug, Clone, Default)]
pub struct OptimizationStats {
    /// Cells before optimization
    pub cells_before: usize,
    /// Cells after optimization
    pub cells_after: usize,
    /// Cells removed
    pub cells_removed: usize,
    /// Nets before optimization
    pub nets_before: usize,
    /// Nets after optimization
    pub nets_after: usize,
    /// FIT before optimization
    pub fit_before: f64,
    /// FIT after optimization
    pub fit_after: f64,
    /// Per-pass statistics
    pub pass_stats: Vec<PassStats>,
}

/// Statistics for a single optimization pass
#[derive(Debug, Clone)]
pub struct PassStats {
    /// Pass name
    pub name: String,
    /// Cells removed by this pass
    pub cells_removed: usize,
    /// FIT reduced by this pass
    pub fit_reduced: f64,
}

// ============================================================================
// Gate Optimizer
// ============================================================================

/// Gate-level netlist optimizer
pub struct GateOptimizer {
    /// Enable constant folding
    pub enable_constant_folding: bool,
    /// Enable dead code elimination
    pub enable_dce: bool,
    /// Enable boolean simplification
    pub enable_boolean_simp: bool,
    /// Enable MUX optimization
    pub enable_mux_opt: bool,
    /// Enable buffer removal
    pub enable_buffer_removal: bool,
    /// Known constant net values
    constants: HashMap<GateNetId, bool>,
    /// Cells to remove
    cells_to_remove: HashSet<CellId>,
    /// Net replacements (old_net -> new_net)
    net_replacements: HashMap<GateNetId, GateNetId>,
}

impl Default for GateOptimizer {
    fn default() -> Self {
        Self::new()
    }
}

impl GateOptimizer {
    /// Create a new optimizer with all passes enabled
    pub fn new() -> Self {
        Self {
            enable_constant_folding: true,
            enable_dce: true,
            enable_boolean_simp: true,
            enable_mux_opt: true,
            enable_buffer_removal: true,
            constants: HashMap::new(),
            cells_to_remove: HashSet::new(),
            net_replacements: HashMap::new(),
        }
    }

    /// Run all enabled optimization passes
    pub fn optimize(&mut self, netlist: &mut GateNetlist) -> OptimizationStats {
        let cells_before = netlist.cells.len();
        let nets_before = netlist.nets.len();
        let fit_before = netlist.total_fit();

        let mut pass_stats = Vec::new();

        // Reset state
        self.constants.clear();
        self.cells_to_remove.clear();
        self.net_replacements.clear();

        // Pass 1: Constant folding
        if self.enable_constant_folding {
            let removed = self.constant_folding(netlist);
            if removed > 0 {
                pass_stats.push(PassStats {
                    name: "ConstantFolding".to_string(),
                    cells_removed: removed,
                    fit_reduced: 0.0, // Calculated after
                });
            }
        }

        // Pass 2: Boolean simplification
        if self.enable_boolean_simp {
            let removed = self.boolean_simplification(netlist);
            if removed > 0 {
                pass_stats.push(PassStats {
                    name: "BooleanSimplification".to_string(),
                    cells_removed: removed,
                    fit_reduced: 0.0,
                });
            }
        }

        // Pass 3: MUX optimization
        if self.enable_mux_opt {
            let removed = self.mux_optimization(netlist);
            if removed > 0 {
                pass_stats.push(PassStats {
                    name: "MuxOptimization".to_string(),
                    cells_removed: removed,
                    fit_reduced: 0.0,
                });
            }
        }

        // Pass 4: Buffer removal
        if self.enable_buffer_removal {
            let removed = self.buffer_removal(netlist);
            if removed > 0 {
                pass_stats.push(PassStats {
                    name: "BufferRemoval".to_string(),
                    cells_removed: removed,
                    fit_reduced: 0.0,
                });
            }
        }

        // Pass 5: Dead code elimination (run last)
        if self.enable_dce {
            let removed = self.dead_code_elimination(netlist);
            if removed > 0 {
                pass_stats.push(PassStats {
                    name: "DeadCodeElimination".to_string(),
                    cells_removed: removed,
                    fit_reduced: 0.0,
                });
            }
        }

        // Apply all optimizations
        self.apply_optimizations(netlist);

        let cells_after = netlist.cells.len();
        let nets_after = netlist.nets.len();
        let fit_after = netlist.total_fit();

        OptimizationStats {
            cells_before,
            cells_after,
            cells_removed: cells_before.saturating_sub(cells_after),
            nets_before,
            nets_after,
            fit_before,
            fit_after,
            pass_stats,
        }
    }

    // ========================================================================
    // Pass 1: Constant Folding
    // ========================================================================

    fn constant_folding(&mut self, netlist: &GateNetlist) -> usize {
        let mut removed = 0;

        // Initialize constants from tie-high/tie-low cells
        for cell in &netlist.cells {
            match cell.cell_type.as_str() {
                "TIE_HI" | "TIEH" | "VDD" => {
                    if let Some(&out) = cell.outputs.first() {
                        self.constants.insert(out, true);
                        self.cells_to_remove.insert(cell.id);
                        removed += 1;
                    }
                }
                "TIE_LO" | "TIEL" | "VSS" | "GND" => {
                    if let Some(&out) = cell.outputs.first() {
                        self.constants.insert(out, false);
                        self.cells_to_remove.insert(cell.id);
                        removed += 1;
                    }
                }
                _ => {}
            }
        }

        // Propagate constants through combinational cells
        let mut changed = true;
        while changed {
            changed = false;

            for cell in &netlist.cells {
                if self.cells_to_remove.contains(&cell.id) {
                    continue;
                }
                if cell.is_sequential() {
                    continue; // Don't fold through registers
                }

                if let Some(result) = self.evaluate_cell(cell) {
                    // Cell can be folded to constant
                    if let Some(&out) = cell.outputs.first() {
                        use std::collections::hash_map::Entry;
                        if let Entry::Vacant(e) = self.constants.entry(out) {
                            e.insert(result);
                            self.cells_to_remove.insert(cell.id);
                            removed += 1;
                            changed = true;
                        }
                    }
                }
            }
        }

        removed
    }

    /// Evaluate a cell if all inputs are constants
    fn evaluate_cell(&self, cell: &Cell) -> Option<bool> {
        let inputs: Vec<bool> = cell
            .inputs
            .iter()
            .map(|id| self.get_constant(*id))
            .collect::<Option<Vec<_>>>()?;

        let cell_type = cell.cell_type.to_uppercase();

        // Match common cell types
        match cell_type.as_str() {
            // Inverters
            "INV" | "INV_X1" | "NOT" => Some(!*inputs.first()?),

            // AND gates
            "AND2" | "AND2_X1" => Some(inputs.first()? & inputs.get(1)?),
            "AND3" | "AND3_X1" => Some(inputs.first()? & inputs.get(1)? & inputs.get(2)?),
            "AND4" | "AND4_X1" => {
                Some(inputs.first()? & inputs.get(1)? & inputs.get(2)? & inputs.get(3)?)
            }

            // OR gates
            "OR2" | "OR2_X1" => Some(inputs.first()? | inputs.get(1)?),
            "OR3" | "OR3_X1" => Some(inputs.first()? | inputs.get(1)? | inputs.get(2)?),
            "OR4" | "OR4_X1" => {
                Some(inputs.first()? | inputs.get(1)? | inputs.get(2)? | inputs.get(3)?)
            }

            // NAND gates
            "NAND2" | "NAND2_X1" => Some(!(inputs.first()? & inputs.get(1)?)),
            "NAND3" | "NAND3_X1" => Some(!(inputs.first()? & inputs.get(1)? & inputs.get(2)?)),
            "NAND4" | "NAND4_X1" => {
                Some(!(inputs.first()? & inputs.get(1)? & inputs.get(2)? & inputs.get(3)?))
            }

            // NOR gates
            "NOR2" | "NOR2_X1" => Some(!(inputs.first()? | inputs.get(1)?)),
            "NOR3" | "NOR3_X1" => Some(!(inputs.first()? | inputs.get(1)? | inputs.get(2)?)),
            "NOR4" | "NOR4_X1" => {
                Some(!(inputs.first()? | inputs.get(1)? | inputs.get(2)? | inputs.get(3)?))
            }

            // XOR gates
            "XOR2" | "XOR2_X1" => Some(inputs.first()? ^ inputs.get(1)?),
            "XNOR2" | "XNOR2_X1" => Some(!(inputs.first()? ^ inputs.get(1)?)),

            // MUX (sel, a, b) -> sel ? b : a
            "MUX2" | "MUX2_X1" => {
                let sel = inputs.first()?;
                let a = inputs.get(1)?;
                let b = inputs.get(2)?;
                Some(if *sel { *b } else { *a })
            }

            // Buffer
            "BUF" | "BUF_X1" | "BUFF" => inputs.first().copied(),

            _ => None, // Unknown cell type
        }
    }

    /// Get constant value for a net, following replacements
    fn get_constant(&self, mut net_id: GateNetId) -> Option<bool> {
        // Follow replacement chain
        while let Some(&replacement) = self.net_replacements.get(&net_id) {
            net_id = replacement;
        }
        self.constants.get(&net_id).copied()
    }

    // ========================================================================
    // Pass 2: Boolean Simplification
    // ========================================================================

    fn boolean_simplification(&mut self, netlist: &GateNetlist) -> usize {
        let mut removed = 0;

        // Build driver map: net_id -> cell_id that drives it
        let mut driver_map: HashMap<GateNetId, CellId> = HashMap::new();
        for cell in &netlist.cells {
            for &out in &cell.outputs {
                driver_map.insert(out, cell.id);
            }
        }

        // Find cell by ID
        let cell_by_id: HashMap<CellId, &Cell> = netlist.cells.iter().map(|c| (c.id, c)).collect();

        for cell in &netlist.cells {
            if self.cells_to_remove.contains(&cell.id) {
                continue;
            }

            let cell_type = cell.cell_type.to_uppercase();

            // Double negation: INV(INV(x)) -> x
            if matches!(cell_type.as_str(), "INV" | "INV_X1" | "NOT") {
                if let Some(&input_net) = cell.inputs.first() {
                    if let Some(&driver_id) = driver_map.get(&input_net) {
                        if let Some(driver) = cell_by_id.get(&driver_id) {
                            if matches!(
                                driver.cell_type.to_uppercase().as_str(),
                                "INV" | "INV_X1" | "NOT"
                            ) {
                                // INV(INV(x)) -> x
                                if let Some(&orig_input) = driver.inputs.first() {
                                    if let Some(&out) = cell.outputs.first() {
                                        self.net_replacements.insert(out, orig_input);
                                        self.cells_to_remove.insert(cell.id);
                                        removed += 1;
                                    }
                                }
                            }
                        }
                    }
                }
            }

            // AND with constant 0 -> 0, AND with constant 1 -> other input
            if matches!(cell_type.as_str(), "AND2" | "AND2_X1") {
                if let (Some(&a), Some(&b)) = (cell.inputs.first(), cell.inputs.get(1)) {
                    if let Some(const_a) = self.get_constant(a) {
                        if !const_a {
                            // AND(0, x) -> 0
                            if let Some(&out) = cell.outputs.first() {
                                self.constants.insert(out, false);
                                self.cells_to_remove.insert(cell.id);
                                removed += 1;
                            }
                        } else {
                            // AND(1, x) -> x
                            if let Some(&out) = cell.outputs.first() {
                                self.net_replacements.insert(out, b);
                                self.cells_to_remove.insert(cell.id);
                                removed += 1;
                            }
                        }
                    } else if let Some(const_b) = self.get_constant(b) {
                        if !const_b {
                            // AND(x, 0) -> 0
                            if let Some(&out) = cell.outputs.first() {
                                self.constants.insert(out, false);
                                self.cells_to_remove.insert(cell.id);
                                removed += 1;
                            }
                        } else {
                            // AND(x, 1) -> x
                            if let Some(&out) = cell.outputs.first() {
                                self.net_replacements.insert(out, a);
                                self.cells_to_remove.insert(cell.id);
                                removed += 1;
                            }
                        }
                    } else if a == b {
                        // AND(x, x) -> x (idempotency)
                        if let Some(&out) = cell.outputs.first() {
                            self.net_replacements.insert(out, a);
                            self.cells_to_remove.insert(cell.id);
                            removed += 1;
                        }
                    }
                }
            }

            // OR with constant 1 -> 1, OR with constant 0 -> other input
            if matches!(cell_type.as_str(), "OR2" | "OR2_X1") {
                if let (Some(&a), Some(&b)) = (cell.inputs.first(), cell.inputs.get(1)) {
                    if let Some(const_a) = self.get_constant(a) {
                        if const_a {
                            // OR(1, x) -> 1
                            if let Some(&out) = cell.outputs.first() {
                                self.constants.insert(out, true);
                                self.cells_to_remove.insert(cell.id);
                                removed += 1;
                            }
                        } else {
                            // OR(0, x) -> x
                            if let Some(&out) = cell.outputs.first() {
                                self.net_replacements.insert(out, b);
                                self.cells_to_remove.insert(cell.id);
                                removed += 1;
                            }
                        }
                    } else if let Some(const_b) = self.get_constant(b) {
                        if const_b {
                            // OR(x, 1) -> 1
                            if let Some(&out) = cell.outputs.first() {
                                self.constants.insert(out, true);
                                self.cells_to_remove.insert(cell.id);
                                removed += 1;
                            }
                        } else {
                            // OR(x, 0) -> x
                            if let Some(&out) = cell.outputs.first() {
                                self.net_replacements.insert(out, a);
                                self.cells_to_remove.insert(cell.id);
                                removed += 1;
                            }
                        }
                    } else if a == b {
                        // OR(x, x) -> x (idempotency)
                        if let Some(&out) = cell.outputs.first() {
                            self.net_replacements.insert(out, a);
                            self.cells_to_remove.insert(cell.id);
                            removed += 1;
                        }
                    }
                }
            }

            // XOR with constant 0 -> other input, XOR with constant 1 -> INV(other)
            if matches!(cell_type.as_str(), "XOR2" | "XOR2_X1") {
                if let (Some(&a), Some(&b)) = (cell.inputs.first(), cell.inputs.get(1)) {
                    if let Some(const_a) = self.get_constant(a) {
                        if !const_a {
                            // XOR(0, x) -> x
                            if let Some(&out) = cell.outputs.first() {
                                self.net_replacements.insert(out, b);
                                self.cells_to_remove.insert(cell.id);
                                removed += 1;
                            }
                        }
                        // XOR(1, x) -> INV(x) - can't simplify without adding INV
                    } else if let Some(const_b) = self.get_constant(b) {
                        if !const_b {
                            // XOR(x, 0) -> x
                            if let Some(&out) = cell.outputs.first() {
                                self.net_replacements.insert(out, a);
                                self.cells_to_remove.insert(cell.id);
                                removed += 1;
                            }
                        }
                    } else if a == b {
                        // XOR(x, x) -> 0
                        if let Some(&out) = cell.outputs.first() {
                            self.constants.insert(out, false);
                            self.cells_to_remove.insert(cell.id);
                            removed += 1;
                        }
                    }
                }
            }
        }

        removed
    }

    // ========================================================================
    // Pass 3: MUX Optimization
    // ========================================================================

    fn mux_optimization(&mut self, netlist: &GateNetlist) -> usize {
        let mut removed = 0;

        for cell in &netlist.cells {
            if self.cells_to_remove.contains(&cell.id) {
                continue;
            }

            let cell_type = cell.cell_type.to_uppercase();

            if matches!(cell_type.as_str(), "MUX2" | "MUX2_X1") {
                // MUX2 inputs: sel, a (sel=0), b (sel=1)
                if let (Some(&sel), Some(&a), Some(&b)) =
                    (cell.inputs.first(), cell.inputs.get(1), cell.inputs.get(2))
                {
                    // Constant select
                    if let Some(sel_val) = self.get_constant(sel) {
                        if let Some(&out) = cell.outputs.first() {
                            let selected = if sel_val { b } else { a };
                            self.net_replacements.insert(out, selected);
                            self.cells_to_remove.insert(cell.id);
                            removed += 1;
                        }
                    }
                    // Both inputs same
                    else if a == b {
                        if let Some(&out) = cell.outputs.first() {
                            self.net_replacements.insert(out, a);
                            self.cells_to_remove.insert(cell.id);
                            removed += 1;
                        }
                    }
                    // Both inputs are constants and equal
                    else if let (Some(const_a), Some(const_b)) =
                        (self.get_constant(a), self.get_constant(b))
                    {
                        if const_a == const_b {
                            if let Some(&out) = cell.outputs.first() {
                                self.constants.insert(out, const_a);
                                self.cells_to_remove.insert(cell.id);
                                removed += 1;
                            }
                        }
                    }
                }
            }
        }

        removed
    }

    // ========================================================================
    // Pass 4: Buffer Removal
    // ========================================================================

    fn buffer_removal(&mut self, netlist: &GateNetlist) -> usize {
        let mut removed = 0;

        for cell in &netlist.cells {
            if self.cells_to_remove.contains(&cell.id) {
                continue;
            }

            let cell_type = cell.cell_type.to_uppercase();

            if matches!(cell_type.as_str(), "BUF" | "BUF_X1" | "BUFF" | "CLKBUF") {
                if let (Some(&input), Some(&output)) = (cell.inputs.first(), cell.outputs.first()) {
                    // Replace buffer output with its input
                    self.net_replacements.insert(output, input);
                    self.cells_to_remove.insert(cell.id);
                    removed += 1;
                }
            }
        }

        removed
    }

    // ========================================================================
    // Pass 5: Dead Code Elimination
    // ========================================================================

    fn dead_code_elimination(&mut self, netlist: &GateNetlist) -> usize {
        // Mark all nets that are outputs or used by outputs
        let mut live_nets: HashSet<GateNetId> = HashSet::new();
        let mut live_cells: HashSet<CellId> = HashSet::new();

        // Start with primary outputs and detection signals
        // Follow net_replacements to find the actual nets
        for &net_id in &netlist.outputs {
            let resolved = self.resolve_net(net_id);
            live_nets.insert(resolved);
        }
        for net in &netlist.nets {
            if net.is_detection {
                let resolved = self.resolve_net(net.id);
                live_nets.insert(resolved);
            }
        }

        // Build driver map: net_id -> cell that drives it (not in cells_to_remove)
        let mut driver_map: HashMap<GateNetId, CellId> = HashMap::new();
        for cell in &netlist.cells {
            if !self.cells_to_remove.contains(&cell.id) {
                for &out in &cell.outputs {
                    driver_map.insert(out, cell.id);
                }
            }
        }

        // Build cell by id map
        let cell_by_id: HashMap<CellId, &Cell> = netlist.cells.iter().map(|c| (c.id, c)).collect();

        // Propagate liveness backwards
        let mut worklist: Vec<GateNetId> = live_nets.iter().copied().collect();

        while let Some(net_id) = worklist.pop() {
            // Find driver cell for this net (after following replacements)
            if let Some(&driver_id) = driver_map.get(&net_id) {
                if !live_cells.contains(&driver_id) {
                    live_cells.insert(driver_id);

                    // Add all inputs of this cell to worklist (following replacements)
                    if let Some(cell) = cell_by_id.get(&driver_id) {
                        for &input in &cell.inputs {
                            let resolved = self.resolve_net(input);
                            if !live_nets.contains(&resolved) {
                                live_nets.insert(resolved);
                                worklist.push(resolved);
                            }
                        }
                        if let Some(clk) = cell.clock {
                            let resolved = self.resolve_net(clk);
                            if !live_nets.contains(&resolved) {
                                live_nets.insert(resolved);
                                worklist.push(resolved);
                            }
                        }
                        if let Some(rst) = cell.reset {
                            let resolved = self.resolve_net(rst);
                            if !live_nets.contains(&resolved) {
                                live_nets.insert(resolved);
                                worklist.push(resolved);
                            }
                        }
                    }
                }
            }
        }

        // Mark all non-live cells for removal
        let mut removed = 0;
        for cell in &netlist.cells {
            if !live_cells.contains(&cell.id) && !self.cells_to_remove.contains(&cell.id) {
                self.cells_to_remove.insert(cell.id);
                removed += 1;
            }
        }

        removed
    }

    /// Resolve a net ID through the replacement chain
    fn resolve_net(&self, mut net_id: GateNetId) -> GateNetId {
        while let Some(&replacement) = self.net_replacements.get(&net_id) {
            net_id = replacement;
        }
        net_id
    }

    // ========================================================================
    // Apply Optimizations
    // ========================================================================

    fn apply_optimizations(&mut self, netlist: &mut GateNetlist) {
        // First, apply net replacements to all cell inputs
        for cell in &mut netlist.cells {
            for input in &mut cell.inputs {
                while let Some(&replacement) = self.net_replacements.get(input) {
                    *input = replacement;
                }
            }
        }

        // Update net fanout information
        for net in &mut netlist.nets {
            net.fanout
                .retain(|(cell_id, _)| !self.cells_to_remove.contains(cell_id));
        }

        // Remove marked cells
        netlist
            .cells
            .retain(|cell| !self.cells_to_remove.contains(&cell.id));

        // Update statistics
        netlist.update_stats();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_constant_folding_and_gate() {
        let mut netlist = GateNetlist::new("test".into(), "test_lib".into());

        // Create: TIE_LO -> AND2 -> output
        // Should fold to: output = 0
        let const_lo_out = netlist.add_net(GateNet::new(GateNetId(0), "const_lo".into()));
        let and_in_b = netlist.add_input("in_b".into());
        let and_out = netlist.add_output("and_out".into());

        // TIE_LO cell
        netlist.cells.push(Cell::new_comb(
            CellId(0),
            "TIE_LO".into(),
            "test_lib".into(),
            0.1,
            "tie_lo".into(),
            vec![],
            vec![const_lo_out],
        ));

        // AND2 cell
        netlist.cells.push(Cell::new_comb(
            CellId(1),
            "AND2".into(),
            "test_lib".into(),
            0.1,
            "and".into(),
            vec![const_lo_out, and_in_b],
            vec![and_out],
        ));

        let mut optimizer = GateOptimizer::new();
        let stats = optimizer.optimize(&mut netlist);

        // Both cells should be removed (TIE_LO and AND2)
        assert_eq!(stats.cells_removed, 2);
        assert_eq!(netlist.cells.len(), 0);
    }

    #[test]
    fn test_double_inverter_removal() {
        let mut netlist = GateNetlist::new("test".into(), "test_lib".into());

        // Create: input -> INV -> INV -> output
        // Should simplify to: input -> output
        let input = netlist.add_input("input".into());
        let inv1_out = netlist.add_net(GateNet::new(GateNetId(0), "inv1_out".into()));
        let output = netlist.add_output("output".into());

        // INV 1
        let inv1 = Cell::new_comb(
            CellId(0),
            "INV".into(),
            "test_lib".into(),
            0.1,
            "inv1".into(),
            vec![input],
            vec![inv1_out],
        );
        netlist.nets[inv1_out.0 as usize].driver = Some(CellId(0));
        netlist.cells.push(inv1);

        // INV 2
        let inv2 = Cell::new_comb(
            CellId(1),
            "INV".into(),
            "test_lib".into(),
            0.1,
            "inv2".into(),
            vec![inv1_out],
            vec![output],
        );
        netlist.nets[output.0 as usize].driver = Some(CellId(1));
        netlist.cells.push(inv2);

        let mut optimizer = GateOptimizer::new();
        let stats = optimizer.optimize(&mut netlist);

        // Second INV should be removed (double negation)
        // First INV becomes dead code
        assert!(stats.cells_removed >= 1);
    }

    #[test]
    fn test_mux_constant_select() {
        let mut netlist = GateNetlist::new("test".into(), "test_lib".into());

        // Create: MUX2(0, a, b) -> should select 'a'
        let sel = netlist.add_net(GateNet::new(GateNetId(0), "sel".into()));
        let a = netlist.add_input("a".into());
        let b = netlist.add_input("b".into());
        let out = netlist.add_output("out".into());

        // TIE_LO for sel=0
        netlist.cells.push(Cell::new_comb(
            CellId(0),
            "TIE_LO".into(),
            "test_lib".into(),
            0.1,
            "tie_lo".into(),
            vec![],
            vec![sel],
        ));

        // MUX2
        netlist.cells.push(Cell::new_comb(
            CellId(1),
            "MUX2".into(),
            "test_lib".into(),
            0.2,
            "mux".into(),
            vec![sel, a, b],
            vec![out],
        ));

        let mut optimizer = GateOptimizer::new();
        let stats = optimizer.optimize(&mut netlist);

        // TIE_LO and MUX2 should be removed
        assert_eq!(stats.cells_removed, 2);
    }
}
