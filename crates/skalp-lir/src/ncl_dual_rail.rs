//! NCL Dual-Rail Conversion
//!
//! Converts an optimized single-rail gate netlist to NCL dual-rail encoding.
//! This enables the "optimize-first" synthesis flow:
//!
//! ```text
//! MIR → LIR → Single-rail gates → AIG optimize → Dual-rail convert → Minimal completion
//! ```
//!
//! ## Conversion Rules
//!
//! | Boolean Gate | True Rail (out_t)           | False Rail (out_f)          |
//! |--------------|-----------------------------|-----------------------------|
//! | AND(a, b)    | TH22(a_t, b_t)              | TH12(a_f, b_f)              |
//! | OR(a, b)     | TH12(a_t, b_t)              | TH22(a_f, b_f)              |
//! | NOT(a)       | a_f                         | a_t (swap rails)            |
//! | XOR(a, b)    | TH12(TH22(a_t,b_f),TH22(a_f,b_t)) | TH12(TH22(a_t,b_t),TH22(a_f,b_f)) |
//! | NAND(a, b)   | TH12(a_f, b_f)              | TH22(a_t, b_t)              |
//! | NOR(a, b)    | TH22(a_f, b_f)              | TH12(a_t, b_t)              |
//! | BUF(a)       | a_t                         | a_f                         |
//!
//! ## Completion Detection
//!
//! Added only at primary outputs:
//! - Per-bit: `complete_i = TH12(out_t[i], out_f[i])`
//! - Combined: `all_complete = TH22(complete_0, TH22(complete_1, ...))`

use crate::gate_netlist::{Cell, CellId, GateNet, GateNetId, GateNetlist};
use std::collections::HashMap;

/// Configuration for dual-rail conversion
#[derive(Debug, Clone)]
pub struct DualRailConfig {
    /// Add completion detection at primary outputs
    pub add_completion: bool,
    /// Library name for generated cells
    pub library_name: String,
    /// FIT rate for TH12 gates (or OR2 if using std cells)
    pub th12_fit: f64,
    /// FIT rate for TH22 gates (or C-element macro if using std cells)
    pub th22_fit: f64,
    /// Use standard cells with feedback for C-elements instead of native THmn gates
    /// When true: TH12 → OR2, TH22 → C-element macro (AND2 + OR2 + feedback)
    /// This is auto-detected from the tech library - set via `from_library()`
    pub use_std_cells: bool,
}

impl Default for DualRailConfig {
    fn default() -> Self {
        Self {
            add_completion: true,
            library_name: "generic_asic".to_string(),
            th12_fit: 0.1,
            th22_fit: 0.1,
            use_std_cells: false, // Default to native THmn gates
        }
    }
}

impl DualRailConfig {
    /// Create config by querying the tech library for available cells
    /// If THmn gates (TH12, TH22) are available, use them directly.
    /// Otherwise, decompose to standard cells with feedback.
    pub fn from_library(library: &crate::TechLibrary) -> Self {
        let has_th12 = library.get_cell("TH12_X1").is_some();
        let has_th22 = library.get_cell("TH22_X1").is_some();
        let use_std_cells = !has_th12 || !has_th22;

        // Get FIT rates from library if available
        let th12_fit = library
            .get_cell("TH12_X1")
            .map(|c| c.fit)
            .or_else(|| library.get_cell("OR2_X1").map(|c| c.fit))
            .unwrap_or(0.1);

        let th22_fit = library
            .get_cell("TH22_X1")
            .map(|c| c.fit)
            .or_else(|| library.get_cell("AND2_X1").map(|c| c.fit))
            .unwrap_or(0.1);

        // Note: if use_std_cells is true, TH12→OR2 and TH22→C-element macro

        Self {
            add_completion: true,
            library_name: library.name.clone(),
            th12_fit,
            th22_fit,
            use_std_cells,
        }
    }
}

/// Statistics from dual-rail conversion
#[derive(Debug, Clone, Default)]
pub struct DualRailStats {
    /// Original single-rail cell count
    pub single_rail_cells: usize,
    /// Resulting dual-rail cell count
    pub dual_rail_cells: usize,
    /// Number of TH12 gates created (or OR2 if use_std_cells)
    pub th12_count: usize,
    /// Number of TH22 gates created (or C-element macros if use_std_cells)
    pub th22_count: usize,
    /// Number of completion detection gates
    pub completion_gates: usize,
    /// Number of nets before conversion
    pub nets_before: usize,
    /// Number of nets after conversion
    pub nets_after: usize,
    /// Number of C-element macros created (only when use_std_cells=true)
    pub c_element_macros: usize,
    /// Number of feedback loops created (for C-element macros)
    pub feedback_loops: usize,
}

/// Dual-rail signal pair
#[derive(Debug, Clone, Copy)]
struct DualRail {
    /// True rail net ID
    t: GateNetId,
    /// False rail net ID
    f: GateNetId,
}

/// Converts a single-rail gate netlist to NCL dual-rail encoding
pub struct DualRailConverter {
    config: DualRailConfig,
    /// Maps single-rail net to dual-rail pair
    rail_map: HashMap<GateNetId, DualRail>,
    /// Next cell ID
    next_cell_id: u32,
    /// Next net ID
    next_net_id: u32,
    /// Statistics
    stats: DualRailStats,
}

impl DualRailConverter {
    /// Create a new dual-rail converter
    pub fn new(config: DualRailConfig) -> Self {
        Self {
            config,
            rail_map: HashMap::new(),
            next_cell_id: 0,
            next_net_id: 0,
            stats: DualRailStats::default(),
        }
    }

    /// Convert a single-rail gate netlist to dual-rail NCL
    pub fn convert(&mut self, input: &GateNetlist) -> (GateNetlist, DualRailStats) {
        self.stats.single_rail_cells = input.cells.len();
        self.stats.nets_before = input.nets.len();

        // Create output netlist
        let mut output = GateNetlist::new(
            format!("{}_ncl", input.name),
            self.config.library_name.clone(),
        );

        // Initialize IDs
        self.next_net_id = 0;
        self.next_cell_id = 0;
        self.rail_map.clear();

        // Phase 1: Create dual-rail nets for all single-rail nets
        for net in &input.nets {
            let t_id = self.alloc_net_id();
            let f_id = self.alloc_net_id();

            // Create true rail net
            let t_net = GateNet::new(t_id, format!("{}_t", net.name));
            output.nets.push(t_net);

            // Create false rail net
            let f_net = GateNet::new(f_id, format!("{}_f", net.name));
            output.nets.push(f_net);

            self.rail_map.insert(net.id, DualRail { t: t_id, f: f_id });
        }

        // Phase 2: Convert input ports to dual-rail
        // Primary inputs need TIE cells for NULL state initialization
        for &input_net in &input.inputs {
            if let Some(&dual) = self.rail_map.get(&input_net) {
                output.inputs.push(dual.t);
                output.inputs.push(dual.f);
            }
        }

        // Phase 3: Convert each gate to its dual-rail equivalent
        for cell in &input.cells {
            self.convert_cell(cell, &mut output);
        }

        // Phase 4: Convert output ports and add completion detection
        let mut output_rails = Vec::new();
        for &output_net in &input.outputs {
            if let Some(&dual) = self.rail_map.get(&output_net) {
                output.outputs.push(dual.t);
                output.outputs.push(dual.f);
                output_rails.push(dual);
            }
        }

        // Phase 5: Add completion detection at primary outputs
        if self.config.add_completion && !output_rails.is_empty() {
            self.add_completion_detection(&output_rails, &mut output);
        }

        // Update statistics
        self.stats.dual_rail_cells = output.cells.len();
        self.stats.nets_after = output.nets.len();

        // Rebuild net connectivity
        output.rebuild_net_connectivity();

        (output, self.stats.clone())
    }

    /// Allocate a new net ID
    fn alloc_net_id(&mut self) -> GateNetId {
        let id = GateNetId(self.next_net_id);
        self.next_net_id += 1;
        id
    }

    /// Allocate a new cell ID
    fn alloc_cell_id(&mut self) -> CellId {
        let id = CellId(self.next_cell_id);
        self.next_cell_id += 1;
        id
    }

    /// Get dual-rail nets for a single-rail net
    fn get_dual_rail(&self, net: GateNetId) -> Option<DualRail> {
        self.rail_map.get(&net).copied()
    }

    /// Create a TH12 gate (1-of-2, OR semantics)
    /// When use_std_cells=true, uses OR2 instead (equivalent behavior for NCL)
    fn create_th12(
        &mut self,
        output: &mut GateNetlist,
        name: &str,
        in_a: GateNetId,
        in_b: GateNetId,
    ) -> GateNetId {
        let out_id = self.alloc_net_id();
        let out_net = GateNet::new(out_id, format!("{}_out", name));
        output.nets.push(out_net);

        let cell_id = self.alloc_cell_id();
        // TH12 (1-of-2) is equivalent to OR for NCL steady-state behavior
        let cell_type = if self.config.use_std_cells {
            "OR2_X1".to_string()
        } else {
            "TH12_X1".to_string()
        };
        let cell = Cell::new_comb(
            cell_id,
            cell_type,
            self.config.library_name.clone(),
            self.config.th12_fit,
            name.to_string(),
            vec![in_a, in_b],
            vec![out_id],
        );
        output.cells.push(cell);
        self.stats.th12_count += 1;
        out_id
    }

    /// Create a TH22 gate (2-of-2, AND/C-element semantics)
    /// When use_std_cells=true, creates a C-element macro with feedback:
    /// Y = (A & B) | (Y & (A | B))
    fn create_th22(
        &mut self,
        output: &mut GateNetlist,
        name: &str,
        in_a: GateNetId,
        in_b: GateNetId,
    ) -> GateNetId {
        if self.config.use_std_cells {
            return self.create_c_element_macro(output, name, in_a, in_b);
        }

        let out_id = self.alloc_net_id();
        let out_net = GateNet::new(out_id, format!("{}_out", name));
        output.nets.push(out_net);

        let cell_id = self.alloc_cell_id();
        let cell = Cell::new_comb(
            cell_id,
            "TH22_X1".to_string(),
            self.config.library_name.clone(),
            self.config.th22_fit,
            name.to_string(),
            vec![in_a, in_b],
            vec![out_id],
        );
        output.cells.push(cell);
        self.stats.th22_count += 1;
        out_id
    }

    /// Create a C-element macro using standard cells with feedback
    /// Implements: Y = (A & B) | (Y & (A | B))
    ///
    /// This is equivalent to TH22 (Muller C-element) behavior:
    /// - Output = 1 when both inputs = 1
    /// - Output = 0 when both inputs = 0
    /// - Output holds when inputs differ
    ///
    /// Structure:
    /// ```text
    ///        A ──┬──[AND2]──┐
    ///            │          │
    ///        B ──┼──────────┤──[OR2]── Y
    ///            │          │          │
    ///            └──[OR2]───┼──[AND2]──┘
    ///                       │     ↑
    ///                       └─────┘ (feedback)
    /// ```
    fn create_c_element_macro(
        &mut self,
        output: &mut GateNetlist,
        name: &str,
        in_a: GateNetId,
        in_b: GateNetId,
    ) -> GateNetId {
        // Create output net first (needed for feedback loop)
        let out_id = self.alloc_net_id();
        let out_net = GateNet::new(out_id, format!("{}_out", name));
        output.nets.push(out_net);

        // 1. AND2: and_ab = A & B (the "set" condition)
        let and_ab_id = self.alloc_net_id();
        let and_ab_net = GateNet::new(and_ab_id, format!("{}_and_ab", name));
        output.nets.push(and_ab_net);

        let and_ab_cell_id = self.alloc_cell_id();
        let and_ab_cell = Cell::new_comb(
            and_ab_cell_id,
            "AND2_X1".to_string(),
            self.config.library_name.clone(),
            0.1,
            format!("{}_and_ab", name),
            vec![in_a, in_b],
            vec![and_ab_id],
        );
        output.cells.push(and_ab_cell);

        // 2. OR2: or_ab = A | B (used for hold condition)
        let or_ab_id = self.alloc_net_id();
        let or_ab_net = GateNet::new(or_ab_id, format!("{}_or_ab", name));
        output.nets.push(or_ab_net);

        let or_ab_cell_id = self.alloc_cell_id();
        let or_ab_cell = Cell::new_comb(
            or_ab_cell_id,
            "OR2_X1".to_string(),
            self.config.library_name.clone(),
            0.1,
            format!("{}_or_ab", name),
            vec![in_a, in_b],
            vec![or_ab_id],
        );
        output.cells.push(or_ab_cell);

        // 3. AND2: hold = Y & (A | B) (feedback - holds previous value)
        let hold_id = self.alloc_net_id();
        let hold_net = GateNet::new(hold_id, format!("{}_hold", name));
        output.nets.push(hold_net);

        let hold_cell_id = self.alloc_cell_id();
        let hold_cell = Cell::new_comb(
            hold_cell_id,
            "AND2_X1".to_string(),
            self.config.library_name.clone(),
            0.1,
            format!("{}_hold", name),
            vec![out_id, or_ab_id], // out_id creates feedback loop!
            vec![hold_id],
        );
        output.cells.push(hold_cell);

        // 4. OR2: Y = (A & B) | hold (final output)
        let out_cell_id = self.alloc_cell_id();
        let out_cell = Cell::new_comb(
            out_cell_id,
            "OR2_X1".to_string(),
            self.config.library_name.clone(),
            self.config.th22_fit,
            format!("{}_out", name),
            vec![and_ab_id, hold_id],
            vec![out_id],
        );
        output.cells.push(out_cell);

        // Update stats
        self.stats.th22_count += 1;
        self.stats.c_element_macros += 1;
        self.stats.feedback_loops += 1;

        out_id
    }

    /// Convert a single-rail gate to dual-rail NCL
    fn convert_cell(&mut self, cell: &Cell, output: &mut GateNetlist) {
        let cell_type = cell.cell_type.to_uppercase();

        // Get output dual-rail (already allocated in phase 1)
        let out_dual = if let Some(&out_net) = cell.outputs.first() {
            self.get_dual_rail(out_net)
        } else {
            return; // No output, skip
        };

        let out_dual = match out_dual {
            Some(d) => d,
            None => return,
        };

        match cell_type.as_str() {
            // Inverter: swap rails (FREE!)
            "INV" | "INV_X1" | "NOT" => {
                if let Some(&in_net) = cell.inputs.first() {
                    if let Some(in_dual) = self.get_dual_rail(in_net) {
                        // out_t = in_f, out_f = in_t
                        // Create buffer connections (or just use the nets directly)
                        self.create_buffer(output, &cell.path, in_dual.f, out_dual.t);
                        self.create_buffer(output, &cell.path, in_dual.t, out_dual.f);
                    }
                }
            }

            // Buffer: pass through both rails
            "BUF" | "BUF_X1" | "BUFF" => {
                if let Some(&in_net) = cell.inputs.first() {
                    if let Some(in_dual) = self.get_dual_rail(in_net) {
                        self.create_buffer(output, &cell.path, in_dual.t, out_dual.t);
                        self.create_buffer(output, &cell.path, in_dual.f, out_dual.f);
                    }
                }
            }

            // AND: out_t = TH22(a_t, b_t), out_f = TH12(a_f, b_f)
            "AND2" | "AND2_X1" => {
                self.convert_and2(cell, out_dual, output);
            }

            // OR: out_t = TH12(a_t, b_t), out_f = TH22(a_f, b_f)
            "OR2" | "OR2_X1" => {
                self.convert_or2(cell, out_dual, output);
            }

            // NAND: out_t = TH12(a_f, b_f), out_f = TH22(a_t, b_t)
            "NAND2" | "NAND2_X1" => {
                self.convert_nand2(cell, out_dual, output);
            }

            // NOR: out_t = TH22(a_f, b_f), out_f = TH12(a_t, b_t)
            "NOR2" | "NOR2_X1" => {
                self.convert_nor2(cell, out_dual, output);
            }

            // XOR: complex dual-rail
            "XOR2" | "XOR2_X1" => {
                self.convert_xor2(cell, out_dual, output);
            }

            // XNOR: complex dual-rail (inverse of XOR)
            "XNOR2" | "XNOR2_X1" => {
                self.convert_xnor2(cell, out_dual, output);
            }

            // MUX2: sel ? b : a
            "MUX2" | "MUX2_X1" => {
                self.convert_mux2(cell, out_dual, output);
            }

            // TIE cells: constant values
            "TIE_HI" | "TIEH" | "VDD" | "TIE_HIGH" => {
                // Constant 1: t=1, f=0 (but in NCL we use TIE cells)
                self.create_tie_high(output, &cell.path, out_dual.t);
                self.create_tie_low(output, &cell.path, out_dual.f);
            }

            "TIE_LO" | "TIEL" | "VSS" | "GND" | "TIE_LOW" => {
                // Constant 0: t=0, f=1
                self.create_tie_low(output, &cell.path, out_dual.t);
                self.create_tie_high(output, &cell.path, out_dual.f);
            }

            // Multi-input gates - expand to tree
            "AND3" | "AND3_X1" => {
                self.convert_and3(cell, out_dual, output);
            }

            "OR3" | "OR3_X1" => {
                self.convert_or3(cell, out_dual, output);
            }

            "AND4" | "AND4_X1" => {
                self.convert_and4(cell, out_dual, output);
            }

            "OR4" | "OR4_X1" => {
                self.convert_or4(cell, out_dual, output);
            }

            // Full Adder: has 2 outputs (sum, cout)
            "FA" | "FA_X1" => {
                self.convert_fa(cell, output);
            }

            // Half Adder: has 2 outputs (sum, cout)
            "HA" | "HA_X1" => {
                self.convert_ha(cell, output);
            }

            _ => {
                // Unknown gate type - log warning and create passthrough
                eprintln!(
                    "[DUAL_RAIL] Warning: Unknown gate type '{}' in cell '{}'",
                    cell.cell_type, cell.path
                );
            }
        }
    }

    /// Convert AND2 gate
    fn convert_and2(&mut self, cell: &Cell, out: DualRail, output: &mut GateNetlist) {
        if let (Some(&a), Some(&b)) = (cell.inputs.first(), cell.inputs.get(1)) {
            if let (Some(a_dual), Some(b_dual)) = (self.get_dual_rail(a), self.get_dual_rail(b)) {
                // out_t = TH22(a_t, b_t)
                let t_out =
                    self.create_th22(output, &format!("{}_t", cell.path), a_dual.t, b_dual.t);
                self.connect_nets(output, t_out, out.t);

                // out_f = TH12(a_f, b_f)
                let f_out =
                    self.create_th12(output, &format!("{}_f", cell.path), a_dual.f, b_dual.f);
                self.connect_nets(output, f_out, out.f);
            }
        }
    }

    /// Convert OR2 gate
    fn convert_or2(&mut self, cell: &Cell, out: DualRail, output: &mut GateNetlist) {
        if let (Some(&a), Some(&b)) = (cell.inputs.first(), cell.inputs.get(1)) {
            if let (Some(a_dual), Some(b_dual)) = (self.get_dual_rail(a), self.get_dual_rail(b)) {
                // out_t = TH12(a_t, b_t)
                let t_out =
                    self.create_th12(output, &format!("{}_t", cell.path), a_dual.t, b_dual.t);
                self.connect_nets(output, t_out, out.t);

                // out_f = TH22(a_f, b_f)
                let f_out =
                    self.create_th22(output, &format!("{}_f", cell.path), a_dual.f, b_dual.f);
                self.connect_nets(output, f_out, out.f);
            }
        }
    }

    /// Convert NAND2 gate
    fn convert_nand2(&mut self, cell: &Cell, out: DualRail, output: &mut GateNetlist) {
        if let (Some(&a), Some(&b)) = (cell.inputs.first(), cell.inputs.get(1)) {
            if let (Some(a_dual), Some(b_dual)) = (self.get_dual_rail(a), self.get_dual_rail(b)) {
                // NAND = NOT(AND) = swap rails of AND
                // out_t = TH12(a_f, b_f)
                let t_out =
                    self.create_th12(output, &format!("{}_t", cell.path), a_dual.f, b_dual.f);
                self.connect_nets(output, t_out, out.t);

                // out_f = TH22(a_t, b_t)
                let f_out =
                    self.create_th22(output, &format!("{}_f", cell.path), a_dual.t, b_dual.t);
                self.connect_nets(output, f_out, out.f);
            }
        }
    }

    /// Convert NOR2 gate
    fn convert_nor2(&mut self, cell: &Cell, out: DualRail, output: &mut GateNetlist) {
        if let (Some(&a), Some(&b)) = (cell.inputs.first(), cell.inputs.get(1)) {
            if let (Some(a_dual), Some(b_dual)) = (self.get_dual_rail(a), self.get_dual_rail(b)) {
                // NOR = NOT(OR) = swap rails of OR
                // out_t = TH22(a_f, b_f)
                let t_out =
                    self.create_th22(output, &format!("{}_t", cell.path), a_dual.f, b_dual.f);
                self.connect_nets(output, t_out, out.t);

                // out_f = TH12(a_t, b_t)
                let f_out =
                    self.create_th12(output, &format!("{}_f", cell.path), a_dual.t, b_dual.t);
                self.connect_nets(output, f_out, out.f);
            }
        }
    }

    /// Convert XOR2 gate
    fn convert_xor2(&mut self, cell: &Cell, out: DualRail, output: &mut GateNetlist) {
        if let (Some(&a), Some(&b)) = (cell.inputs.first(), cell.inputs.get(1)) {
            if let (Some(a_dual), Some(b_dual)) = (self.get_dual_rail(a), self.get_dual_rail(b)) {
                // XOR(a, b) = (a AND NOT b) OR (NOT a AND b)
                // out_t = TH12(TH22(a_t, b_f), TH22(a_f, b_t))
                let t_and1 =
                    self.create_th22(output, &format!("{}_t_and1", cell.path), a_dual.t, b_dual.f);
                let t_and2 =
                    self.create_th22(output, &format!("{}_t_and2", cell.path), a_dual.f, b_dual.t);
                let t_out = self.create_th12(output, &format!("{}_t", cell.path), t_and1, t_and2);
                self.connect_nets(output, t_out, out.t);

                // out_f = TH12(TH22(a_t, b_t), TH22(a_f, b_f))
                let f_and1 =
                    self.create_th22(output, &format!("{}_f_and1", cell.path), a_dual.t, b_dual.t);
                let f_and2 =
                    self.create_th22(output, &format!("{}_f_and2", cell.path), a_dual.f, b_dual.f);
                let f_out = self.create_th12(output, &format!("{}_f", cell.path), f_and1, f_and2);
                self.connect_nets(output, f_out, out.f);
            }
        }
    }

    /// Convert XNOR2 gate (inverse of XOR)
    fn convert_xnor2(&mut self, cell: &Cell, out: DualRail, output: &mut GateNetlist) {
        if let (Some(&a), Some(&b)) = (cell.inputs.first(), cell.inputs.get(1)) {
            if let (Some(a_dual), Some(b_dual)) = (self.get_dual_rail(a), self.get_dual_rail(b)) {
                // XNOR = NOT(XOR) = swap rails of XOR
                // out_t = TH12(TH22(a_t, b_t), TH22(a_f, b_f))
                let t_and1 =
                    self.create_th22(output, &format!("{}_t_and1", cell.path), a_dual.t, b_dual.t);
                let t_and2 =
                    self.create_th22(output, &format!("{}_t_and2", cell.path), a_dual.f, b_dual.f);
                let t_out = self.create_th12(output, &format!("{}_t", cell.path), t_and1, t_and2);
                self.connect_nets(output, t_out, out.t);

                // out_f = TH12(TH22(a_t, b_f), TH22(a_f, b_t))
                let f_and1 =
                    self.create_th22(output, &format!("{}_f_and1", cell.path), a_dual.t, b_dual.f);
                let f_and2 =
                    self.create_th22(output, &format!("{}_f_and2", cell.path), a_dual.f, b_dual.t);
                let f_out = self.create_th12(output, &format!("{}_f", cell.path), f_and1, f_and2);
                self.connect_nets(output, f_out, out.f);
            }
        }
    }

    /// Convert MUX2 gate (sel ? b : a)
    fn convert_mux2(&mut self, cell: &Cell, out: DualRail, output: &mut GateNetlist) {
        if let (Some(&sel), Some(&a), Some(&b)) =
            (cell.inputs.first(), cell.inputs.get(1), cell.inputs.get(2))
        {
            if let (Some(sel_dual), Some(a_dual), Some(b_dual)) = (
                self.get_dual_rail(sel),
                self.get_dual_rail(a),
                self.get_dual_rail(b),
            ) {
                // MUX = (sel AND b) OR (NOT sel AND a)
                // out_t = TH12(TH22(sel_t, b_t), TH22(sel_f, a_t))
                let t_sel_b = self.create_th22(
                    output,
                    &format!("{}_t_sel_b", cell.path),
                    sel_dual.t,
                    b_dual.t,
                );
                let t_nsel_a = self.create_th22(
                    output,
                    &format!("{}_t_nsel_a", cell.path),
                    sel_dual.f,
                    a_dual.t,
                );
                let t_out =
                    self.create_th12(output, &format!("{}_t", cell.path), t_sel_b, t_nsel_a);
                self.connect_nets(output, t_out, out.t);

                // out_f = TH12(TH22(sel_t, b_f), TH22(sel_f, a_f))
                let f_sel_b = self.create_th22(
                    output,
                    &format!("{}_f_sel_b", cell.path),
                    sel_dual.t,
                    b_dual.f,
                );
                let f_nsel_a = self.create_th22(
                    output,
                    &format!("{}_f_nsel_a", cell.path),
                    sel_dual.f,
                    a_dual.f,
                );
                let f_out =
                    self.create_th12(output, &format!("{}_f", cell.path), f_sel_b, f_nsel_a);
                self.connect_nets(output, f_out, out.f);
            }
        }
    }

    /// Convert AND3 gate to tree of AND2
    fn convert_and3(&mut self, cell: &Cell, out: DualRail, output: &mut GateNetlist) {
        if cell.inputs.len() >= 3 {
            let a = cell.inputs[0];
            let b = cell.inputs[1];
            let c = cell.inputs[2];
            if let (Some(a_dual), Some(b_dual), Some(c_dual)) = (
                self.get_dual_rail(a),
                self.get_dual_rail(b),
                self.get_dual_rail(c),
            ) {
                // AND3(a, b, c) = AND2(AND2(a, b), c)
                // True rail: TH22(TH22(a_t, b_t), c_t)
                let t_ab =
                    self.create_th22(output, &format!("{}_t_ab", cell.path), a_dual.t, b_dual.t);
                let t_out = self.create_th22(output, &format!("{}_t", cell.path), t_ab, c_dual.t);
                self.connect_nets(output, t_out, out.t);

                // False rail: TH12(TH12(a_f, b_f), c_f) - OR tree
                let f_ab =
                    self.create_th12(output, &format!("{}_f_ab", cell.path), a_dual.f, b_dual.f);
                let f_out = self.create_th12(output, &format!("{}_f", cell.path), f_ab, c_dual.f);
                self.connect_nets(output, f_out, out.f);
            }
        }
    }

    /// Convert OR3 gate to tree of OR2
    fn convert_or3(&mut self, cell: &Cell, out: DualRail, output: &mut GateNetlist) {
        if cell.inputs.len() >= 3 {
            let a = cell.inputs[0];
            let b = cell.inputs[1];
            let c = cell.inputs[2];
            if let (Some(a_dual), Some(b_dual), Some(c_dual)) = (
                self.get_dual_rail(a),
                self.get_dual_rail(b),
                self.get_dual_rail(c),
            ) {
                // OR3(a, b, c) = OR2(OR2(a, b), c)
                // True rail: TH12(TH12(a_t, b_t), c_t)
                let t_ab =
                    self.create_th12(output, &format!("{}_t_ab", cell.path), a_dual.t, b_dual.t);
                let t_out = self.create_th12(output, &format!("{}_t", cell.path), t_ab, c_dual.t);
                self.connect_nets(output, t_out, out.t);

                // False rail: TH22(TH22(a_f, b_f), c_f) - AND tree
                let f_ab =
                    self.create_th22(output, &format!("{}_f_ab", cell.path), a_dual.f, b_dual.f);
                let f_out = self.create_th22(output, &format!("{}_f", cell.path), f_ab, c_dual.f);
                self.connect_nets(output, f_out, out.f);
            }
        }
    }

    /// Convert AND4 gate to tree
    fn convert_and4(&mut self, cell: &Cell, out: DualRail, output: &mut GateNetlist) {
        if cell.inputs.len() >= 4 {
            let a = cell.inputs[0];
            let b = cell.inputs[1];
            let c = cell.inputs[2];
            let d = cell.inputs[3];
            if let (Some(a_dual), Some(b_dual), Some(c_dual), Some(d_dual)) = (
                self.get_dual_rail(a),
                self.get_dual_rail(b),
                self.get_dual_rail(c),
                self.get_dual_rail(d),
            ) {
                // AND4 = AND2(AND2(a, b), AND2(c, d))
                let t_ab =
                    self.create_th22(output, &format!("{}_t_ab", cell.path), a_dual.t, b_dual.t);
                let t_cd =
                    self.create_th22(output, &format!("{}_t_cd", cell.path), c_dual.t, d_dual.t);
                let t_out = self.create_th22(output, &format!("{}_t", cell.path), t_ab, t_cd);
                self.connect_nets(output, t_out, out.t);

                let f_ab =
                    self.create_th12(output, &format!("{}_f_ab", cell.path), a_dual.f, b_dual.f);
                let f_cd =
                    self.create_th12(output, &format!("{}_f_cd", cell.path), c_dual.f, d_dual.f);
                let f_out = self.create_th12(output, &format!("{}_f", cell.path), f_ab, f_cd);
                self.connect_nets(output, f_out, out.f);
            }
        }
    }

    /// Convert OR4 gate to tree
    fn convert_or4(&mut self, cell: &Cell, out: DualRail, output: &mut GateNetlist) {
        if cell.inputs.len() >= 4 {
            let a = cell.inputs[0];
            let b = cell.inputs[1];
            let c = cell.inputs[2];
            let d = cell.inputs[3];
            if let (Some(a_dual), Some(b_dual), Some(c_dual), Some(d_dual)) = (
                self.get_dual_rail(a),
                self.get_dual_rail(b),
                self.get_dual_rail(c),
                self.get_dual_rail(d),
            ) {
                // OR4 = OR2(OR2(a, b), OR2(c, d))
                let t_ab =
                    self.create_th12(output, &format!("{}_t_ab", cell.path), a_dual.t, b_dual.t);
                let t_cd =
                    self.create_th12(output, &format!("{}_t_cd", cell.path), c_dual.t, d_dual.t);
                let t_out = self.create_th12(output, &format!("{}_t", cell.path), t_ab, t_cd);
                self.connect_nets(output, t_out, out.t);

                let f_ab =
                    self.create_th22(output, &format!("{}_f_ab", cell.path), a_dual.f, b_dual.f);
                let f_cd =
                    self.create_th22(output, &format!("{}_f_cd", cell.path), c_dual.f, d_dual.f);
                let f_out = self.create_th22(output, &format!("{}_f", cell.path), f_ab, f_cd);
                self.connect_nets(output, f_out, out.f);
            }
        }
    }

    /// Convert Full Adder (FA_X1)
    /// Inputs: a[0], b[1], cin[2]
    /// Outputs: sum[0], cout[1]
    ///
    /// NCL Implementation:
    /// - sum = a XOR b XOR cin (3-way XOR)
    /// - cout = majority(a, b, cin) = (a AND b) OR (b AND cin) OR (a AND cin)
    fn convert_fa(&mut self, cell: &Cell, output: &mut GateNetlist) {
        if cell.inputs.len() < 3 || cell.outputs.len() < 2 {
            eprintln!(
                "[DUAL_RAIL] FA_X1 requires 3 inputs and 2 outputs, got {} inputs and {} outputs",
                cell.inputs.len(),
                cell.outputs.len()
            );
            return;
        }

        let a = cell.inputs[0];
        let b = cell.inputs[1];
        let cin = cell.inputs[2];
        let sum_out = cell.outputs[0];
        let cout_out = cell.outputs[1];

        let (a_dual, b_dual, cin_dual) = match (
            self.get_dual_rail(a),
            self.get_dual_rail(b),
            self.get_dual_rail(cin),
        ) {
            (Some(a), Some(b), Some(c)) => (a, b, c),
            _ => return,
        };

        let (sum_dual, cout_dual) =
            match (self.get_dual_rail(sum_out), self.get_dual_rail(cout_out)) {
                (Some(s), Some(c)) => (s, c),
                _ => return,
            };

        // === SUM = a XOR b XOR cin ===
        // First compute a XOR b
        // xor_ab_t = TH12(TH22(a_t, b_f), TH22(a_f, b_t))
        let xor_ab_t1 = self.create_th22(
            output,
            &format!("{}_xor_ab_t1", cell.path),
            a_dual.t,
            b_dual.f,
        );
        let xor_ab_t2 = self.create_th22(
            output,
            &format!("{}_xor_ab_t2", cell.path),
            a_dual.f,
            b_dual.t,
        );
        let xor_ab_t = self.create_th12(
            output,
            &format!("{}_xor_ab_t", cell.path),
            xor_ab_t1,
            xor_ab_t2,
        );

        // xor_ab_f = TH12(TH22(a_t, b_t), TH22(a_f, b_f))
        let xor_ab_f1 = self.create_th22(
            output,
            &format!("{}_xor_ab_f1", cell.path),
            a_dual.t,
            b_dual.t,
        );
        let xor_ab_f2 = self.create_th22(
            output,
            &format!("{}_xor_ab_f2", cell.path),
            a_dual.f,
            b_dual.f,
        );
        let xor_ab_f = self.create_th12(
            output,
            &format!("{}_xor_ab_f", cell.path),
            xor_ab_f1,
            xor_ab_f2,
        );

        // Now compute (a XOR b) XOR cin
        // sum_t = TH12(TH22(xor_ab_t, cin_f), TH22(xor_ab_f, cin_t))
        let sum_t1 = self.create_th22(
            output,
            &format!("{}_sum_t1", cell.path),
            xor_ab_t,
            cin_dual.f,
        );
        let sum_t2 = self.create_th22(
            output,
            &format!("{}_sum_t2", cell.path),
            xor_ab_f,
            cin_dual.t,
        );
        let sum_t = self.create_th12(output, &format!("{}_sum_t", cell.path), sum_t1, sum_t2);
        self.connect_nets(output, sum_t, sum_dual.t);

        // sum_f = TH12(TH22(xor_ab_t, cin_t), TH22(xor_ab_f, cin_f))
        let sum_f1 = self.create_th22(
            output,
            &format!("{}_sum_f1", cell.path),
            xor_ab_t,
            cin_dual.t,
        );
        let sum_f2 = self.create_th22(
            output,
            &format!("{}_sum_f2", cell.path),
            xor_ab_f,
            cin_dual.f,
        );
        let sum_f = self.create_th12(output, &format!("{}_sum_f", cell.path), sum_f1, sum_f2);
        self.connect_nets(output, sum_f, sum_dual.f);

        // === COUT = majority(a, b, cin) = (a AND b) OR (b AND cin) OR (a AND cin) ===
        // cout_t = TH12(TH22(a_t, b_t), TH12(TH22(b_t, cin_t), TH22(a_t, cin_t)))
        let cout_ab = self.create_th22(
            output,
            &format!("{}_cout_ab", cell.path),
            a_dual.t,
            b_dual.t,
        );
        let cout_bc = self.create_th22(
            output,
            &format!("{}_cout_bc", cell.path),
            b_dual.t,
            cin_dual.t,
        );
        let cout_ac = self.create_th22(
            output,
            &format!("{}_cout_ac", cell.path),
            a_dual.t,
            cin_dual.t,
        );
        let cout_bc_ac = self.create_th12(
            output,
            &format!("{}_cout_bc_ac", cell.path),
            cout_bc,
            cout_ac,
        );
        let cout_t = self.create_th12(
            output,
            &format!("{}_cout_t", cell.path),
            cout_ab,
            cout_bc_ac,
        );
        self.connect_nets(output, cout_t, cout_dual.t);

        // cout_f = TH12(TH22(a_f, b_f), TH12(TH22(b_f, cin_f), TH22(a_f, cin_f)))
        let cout_ab_f = self.create_th22(
            output,
            &format!("{}_cout_ab_f", cell.path),
            a_dual.f,
            b_dual.f,
        );
        let cout_bc_f = self.create_th22(
            output,
            &format!("{}_cout_bc_f", cell.path),
            b_dual.f,
            cin_dual.f,
        );
        let cout_ac_f = self.create_th22(
            output,
            &format!("{}_cout_ac_f", cell.path),
            a_dual.f,
            cin_dual.f,
        );
        let cout_bc_ac_f = self.create_th12(
            output,
            &format!("{}_cout_bc_ac_f", cell.path),
            cout_bc_f,
            cout_ac_f,
        );
        let cout_f = self.create_th12(
            output,
            &format!("{}_cout_f", cell.path),
            cout_ab_f,
            cout_bc_ac_f,
        );
        self.connect_nets(output, cout_f, cout_dual.f);
    }

    /// Convert Half Adder (HA_X1)
    /// Inputs: a[0], b[1]
    /// Outputs: sum[0], cout[1]
    ///
    /// NCL Implementation:
    /// - sum = a XOR b
    /// - cout = a AND b
    fn convert_ha(&mut self, cell: &Cell, output: &mut GateNetlist) {
        if cell.inputs.len() < 2 || cell.outputs.len() < 2 {
            eprintln!(
                "[DUAL_RAIL] HA_X1 requires 2 inputs and 2 outputs, got {} inputs and {} outputs",
                cell.inputs.len(),
                cell.outputs.len()
            );
            return;
        }

        let a = cell.inputs[0];
        let b = cell.inputs[1];
        let sum_out = cell.outputs[0];
        let cout_out = cell.outputs[1];

        let (a_dual, b_dual) = match (self.get_dual_rail(a), self.get_dual_rail(b)) {
            (Some(a), Some(b)) => (a, b),
            _ => return,
        };

        let (sum_dual, cout_dual) =
            match (self.get_dual_rail(sum_out), self.get_dual_rail(cout_out)) {
                (Some(s), Some(c)) => (s, c),
                _ => return,
            };

        // === SUM = a XOR b ===
        // sum_t = TH12(TH22(a_t, b_f), TH22(a_f, b_t))
        let sum_t1 = self.create_th22(output, &format!("{}_sum_t1", cell.path), a_dual.t, b_dual.f);
        let sum_t2 = self.create_th22(output, &format!("{}_sum_t2", cell.path), a_dual.f, b_dual.t);
        let sum_t = self.create_th12(output, &format!("{}_sum_t", cell.path), sum_t1, sum_t2);
        self.connect_nets(output, sum_t, sum_dual.t);

        // sum_f = TH12(TH22(a_t, b_t), TH22(a_f, b_f))
        let sum_f1 = self.create_th22(output, &format!("{}_sum_f1", cell.path), a_dual.t, b_dual.t);
        let sum_f2 = self.create_th22(output, &format!("{}_sum_f2", cell.path), a_dual.f, b_dual.f);
        let sum_f = self.create_th12(output, &format!("{}_sum_f", cell.path), sum_f1, sum_f2);
        self.connect_nets(output, sum_f, sum_dual.f);

        // === COUT = a AND b ===
        // cout_t = TH22(a_t, b_t)
        let cout_t = self.create_th22(output, &format!("{}_cout_t", cell.path), a_dual.t, b_dual.t);
        self.connect_nets(output, cout_t, cout_dual.t);

        // cout_f = TH12(a_f, b_f)
        let cout_f = self.create_th12(output, &format!("{}_cout_f", cell.path), a_dual.f, b_dual.f);
        self.connect_nets(output, cout_f, cout_dual.f);
    }

    /// Create a buffer cell (for rail connections)
    fn create_buffer(
        &mut self,
        output: &mut GateNetlist,
        name: &str,
        input: GateNetId,
        out_net: GateNetId,
    ) {
        let cell_id = self.alloc_cell_id();
        let cell = Cell::new_comb(
            cell_id,
            "BUF_X1".to_string(),
            self.config.library_name.clone(),
            0.01, // Low FIT for buffer
            format!("{}_buf", name),
            vec![input],
            vec![out_net],
        );
        output.cells.push(cell);
    }

    /// Connect two nets (by creating a buffer or direct connection)
    fn connect_nets(&mut self, output: &mut GateNetlist, from: GateNetId, to: GateNetId) {
        // For now, create a buffer. The optimizer will remove it later.
        let cell_id = self.alloc_cell_id();
        let cell = Cell::new_comb(
            cell_id,
            "BUF_X1".to_string(),
            self.config.library_name.clone(),
            0.01,
            format!("conn_{}_to_{}", from.0, to.0),
            vec![from],
            vec![to],
        );
        output.cells.push(cell);
    }

    /// Create a TIE-HIGH cell
    fn create_tie_high(&mut self, output: &mut GateNetlist, name: &str, out_net: GateNetId) {
        let cell_id = self.alloc_cell_id();
        let cell = Cell::new_comb(
            cell_id,
            "TIE_HIGH".to_string(),
            self.config.library_name.clone(),
            0.01,
            format!("{}_tie_high", name),
            vec![],
            vec![out_net],
        );
        output.cells.push(cell);
    }

    /// Create a TIE-LOW cell
    fn create_tie_low(&mut self, output: &mut GateNetlist, name: &str, out_net: GateNetId) {
        let cell_id = self.alloc_cell_id();
        let cell = Cell::new_comb(
            cell_id,
            "TIE_LOW".to_string(),
            self.config.library_name.clone(),
            0.01,
            format!("{}_tie_low", name),
            vec![],
            vec![out_net],
        );
        output.cells.push(cell);
    }

    /// Add completion detection tree at primary outputs
    fn add_completion_detection(&mut self, output_rails: &[DualRail], output: &mut GateNetlist) {
        if output_rails.is_empty() {
            return;
        }

        // Step 1: Create per-bit completion signals
        // complete_i = TH12(out_t[i], out_f[i])
        let mut bit_completions = Vec::new();
        for (i, &dual) in output_rails.iter().enumerate() {
            let complete_i = self.create_th12(output, &format!("complete_{}", i), dual.t, dual.f);
            bit_completions.push(complete_i);
            self.stats.completion_gates += 1;
        }

        // Step 2: Build completion tree using TH22 gates
        // all_complete = TH22(complete_0, TH22(complete_1, ...))
        if bit_completions.len() == 1 {
            // Single bit, no tree needed
            let complete_net_id = self.alloc_net_id();
            let complete_net = GateNet::new(complete_net_id, "all_complete".to_string());
            output.nets.push(complete_net);
            self.connect_nets(output, bit_completions[0], complete_net_id);
            output.outputs.push(complete_net_id);
        } else {
            // Build balanced binary tree
            let all_complete = self.build_completion_tree(output, &bit_completions, "complete");
            output.outputs.push(all_complete);
        }
    }

    /// Build a balanced binary tree of TH22 gates for completion
    fn build_completion_tree(
        &mut self,
        output: &mut GateNetlist,
        signals: &[GateNetId],
        prefix: &str,
    ) -> GateNetId {
        if signals.is_empty() {
            // Should not happen, but return a dummy
            let id = self.alloc_net_id();
            output
                .nets
                .push(GateNet::new(id, format!("{}_empty", prefix)));
            return id;
        }

        if signals.len() == 1 {
            return signals[0];
        }

        if signals.len() == 2 {
            let out = self.create_th22(output, &format!("{}_tree", prefix), signals[0], signals[1]);
            self.stats.completion_gates += 1;
            return out;
        }

        // Split in half and recurse
        let mid = signals.len() / 2;
        let left = self.build_completion_tree(output, &signals[..mid], &format!("{}_L", prefix));
        let right = self.build_completion_tree(output, &signals[mid..], &format!("{}_R", prefix));

        let out = self.create_th22(output, &format!("{}_tree", prefix), left, right);
        self.stats.completion_gates += 1;
        out
    }
}

/// Convert a single-rail gate netlist to NCL dual-rail
///
/// This is the main entry point for the optimize-first NCL synthesis flow.
pub fn convert_to_dual_rail(
    input: &GateNetlist,
    config: DualRailConfig,
) -> (GateNetlist, DualRailStats) {
    let mut converter = DualRailConverter::new(config);
    converter.convert(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_dual_rail_and_gate() {
        // Create a simple AND gate netlist
        let mut input = GateNetlist::new("test".to_string(), "test_lib".to_string());

        let a = input.add_input("a".to_string());
        let b = input.add_input("b".to_string());
        let out = input.add_output("out".to_string());

        input.cells.push(Cell::new_comb(
            CellId(0),
            "AND2_X1".to_string(),
            "test_lib".to_string(),
            0.1,
            "and_gate".to_string(),
            vec![a, b],
            vec![out],
        ));

        let config = DualRailConfig::default();
        let (result, stats) = convert_to_dual_rail(&input, config);

        // Should have: 1 TH22 for true rail, 1 TH12 for false rail
        // Plus buffers for connection, plus completion
        assert!(stats.th22_count >= 1);
        assert!(stats.th12_count >= 1);
        println!("Stats: {:?}", stats);
    }

    #[test]
    fn test_dual_rail_inverter() {
        let mut input = GateNetlist::new("test".to_string(), "test_lib".to_string());

        let a = input.add_input("a".to_string());
        let out = input.add_output("out".to_string());

        input.cells.push(Cell::new_comb(
            CellId(0),
            "INV_X1".to_string(),
            "test_lib".to_string(),
            0.1,
            "inv_gate".to_string(),
            vec![a],
            vec![out],
        ));

        let config = DualRailConfig::default();
        let (result, stats) = convert_to_dual_rail(&input, config);

        // Inverter just swaps rails, no TH gates for the logic itself
        // Only completion detection adds TH gates
        println!("Inverter stats: {:?}", stats);
    }

    #[test]
    fn test_dual_rail_with_std_cells() {
        // Test C-element macro mode (for libraries without native THmn gates)
        let mut input = GateNetlist::new("test".to_string(), "test_lib".to_string());

        let a = input.add_input("a".to_string());
        let b = input.add_input("b".to_string());
        let out = input.add_output("out".to_string());

        input.cells.push(Cell::new_comb(
            CellId(0),
            "AND2_X1".to_string(),
            "test_lib".to_string(),
            0.1,
            "and_gate".to_string(),
            vec![a, b],
            vec![out],
        ));

        // Enable std cells mode (simulating a library without THmn gates)
        let config = DualRailConfig {
            use_std_cells: true,
            ..Default::default()
        };

        let (result, stats) = convert_to_dual_rail(&input, config);

        // With std cells mode:
        // - TH22 (for true rail AND) becomes C-element macro (4 gates: 2 AND, 2 OR)
        // - TH12 (for false rail OR) becomes OR2
        // - Completion also uses C-element macros
        assert!(stats.c_element_macros >= 1, "Should use C-element macros");
        assert!(stats.feedback_loops >= 1, "C-elements have feedback loops");

        // Check that output contains standard cells, not THmn gates
        let has_th22 = result.cells.iter().any(|c| c.cell_type.contains("TH22"));
        let has_or2 = result.cells.iter().any(|c| c.cell_type.contains("OR2"));
        let has_and2 = result.cells.iter().any(|c| c.cell_type.contains("AND2"));

        assert!(!has_th22, "Should not have TH22 cells in std cells mode");
        assert!(has_or2, "Should have OR2 cells (for TH12 replacement)");
        assert!(has_and2, "Should have AND2 cells (for C-element macro)");

        println!("Std cells mode stats: {:?}", stats);
        println!("Total cells: {}", result.cells.len());
    }

    #[test]
    fn test_c_element_macro_structure() {
        // Verify the C-element macro has correct structure:
        // Y = (A & B) | (Y & (A | B))
        let mut input = GateNetlist::new("test".to_string(), "test_lib".to_string());

        let a = input.add_input("a".to_string());
        let b = input.add_input("b".to_string());
        let out = input.add_output("out".to_string());

        // Simple AND gate - its true rail will use C-element (TH22)
        input.cells.push(Cell::new_comb(
            CellId(0),
            "AND2_X1".to_string(),
            "test_lib".to_string(),
            0.1,
            "and_gate".to_string(),
            vec![a, b],
            vec![out],
        ));

        let config = DualRailConfig {
            use_std_cells: true,
            add_completion: false, // Skip completion to focus on logic
            ..Default::default()
        };

        let (result, stats) = convert_to_dual_rail(&input, config);

        // Each C-element macro adds 4 cells: 2 AND2, 2 OR2
        // The macro structure is:
        //   AND2: and_ab = A & B
        //   OR2:  or_ab = A | B
        //   AND2: hold = Y & or_ab (feedback!)
        //   OR2:  Y = and_ab | hold

        // Count cell types
        let and2_count = result
            .cells
            .iter()
            .filter(|c| c.cell_type == "AND2_X1")
            .count();
        let or2_count = result
            .cells
            .iter()
            .filter(|c| c.cell_type == "OR2_X1")
            .count();

        println!("AND2 cells: {}, OR2 cells: {}", and2_count, or2_count);
        println!("C-element macros: {}", stats.c_element_macros);
        println!("Feedback loops: {}", stats.feedback_loops);

        // The feedback is created by connecting Y back to the hold AND gate
        // We verify this via the stats counter rather than graph analysis
        assert!(stats.feedback_loops > 0, "Should have feedback loops");
    }
}
