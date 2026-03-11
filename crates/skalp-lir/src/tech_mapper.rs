//! Technology Mapper
//!
//! Maps word-level operations (Lir) to gate-level primitives (GateNetlist)
//! using a technology library.
//!
//! # Flow
//!
//! ```text
//! Lir (word-level) → TechMapper → GateNetlist (gate-level with library cells)
//! ```
//!
//! The mapper:
//! 1. For each word-level operation, finds matching library cells
//! 2. Applies decomposition rules if no direct match
//! 3. Creates GateNetlist with FIT rates from the library

use crate::gate_netlist::{
    Cell, CellFailureMode, CellId, CellSafetyClassification, GateNet, GateNetId, GateNetlist,
};
use crate::lir::{Lir, LirNode, LirNodeId, LirOp, LirSafetyInfo, LirSignalId};
use crate::tech_library::{
    CellFunction, DecompConnectivity, IoCellInfo, LibraryCell, LibraryFailureMode, TechLibrary,
};
use indexmap::IndexMap;
use skalp_frontend::hir::PhysicalConstraints;

/// Information extracted from a library cell for tech mapping
struct LibraryCellInfo {
    name: String,
    function: CellFunction,
    fit: f64,
    failure_modes: Vec<CellFailureMode>,
}

impl LibraryCellInfo {
    /// Create from a library cell reference
    fn from_library_cell(cell: &LibraryCell) -> Self {
        Self {
            name: cell.name.clone(),
            function: cell.function.clone(),
            fit: cell.fit,
            failure_modes: cell
                .failure_modes
                .iter()
                .map(convert_failure_mode)
                .collect(),
        }
    }

    /// Apply library cell metadata to a gate cell
    ///
    /// Sets function and failure_modes from library info.
    fn apply_to_cell(&self, cell: &mut Cell) {
        cell.function = Some(self.function.clone());
        cell.failure_modes = self.failure_modes.clone();
    }
}

/// Convert a library failure mode to a cell failure mode
fn convert_failure_mode(fm: &LibraryFailureMode) -> CellFailureMode {
    CellFailureMode {
        name: fm.name.clone(),
        fit: fm.fit,
        fault_type: fm.fault_type,
    }
}

/// Result of technology mapping
#[derive(Debug)]
pub struct TechMapResult {
    /// The generated gate netlist
    pub netlist: GateNetlist,
    /// Mapping statistics
    pub stats: TechMapStats,
    /// Warnings generated during mapping
    pub warnings: Vec<String>,
}

/// Statistics from technology mapping
#[derive(Debug, Default)]
pub struct TechMapStats {
    /// Word-level nodes processed
    pub nodes_processed: usize,
    /// Cells created
    pub cells_created: usize,
    /// Nets created
    pub nets_created: usize,
    /// Direct mappings (1:1)
    pub direct_mappings: usize,
    /// Decomposed mappings (1:N)
    pub decomposed_mappings: usize,
    /// Clock buffers inserted
    pub clock_buffers_inserted: usize,
    /// IO buffers inserted
    pub io_buffers_inserted: usize,
}

/// Technology mapper
pub struct TechMapper<'a> {
    /// Technology library
    library: &'a TechLibrary,
    /// Output netlist
    netlist: GateNetlist,
    /// Mapping from LirSignalId to GateNetId (for multi-bit, maps to first bit)
    signal_to_net: IndexMap<LirSignalId, Vec<GateNetId>>,
    /// Statistics
    stats: TechMapStats,
    /// Warnings
    warnings: Vec<String>,
    /// Next cell ID
    next_cell_id: u32,
    /// Module-level safety info (from Lir)
    /// Applied to all cells during mapping
    module_safety_info: Option<LirSafetyInfo>,
    /// Counter for unique C-element naming (to avoid duplicate internal net names)
    c_elem_counter: u32,
    /// Physical constraints per port name (pin location, IO standard, drive strength, etc.)
    port_constraints: IndexMap<String, PhysicalConstraints>,
    /// Set of output port signal IDs (for buffer elimination)
    output_signals: std::collections::HashSet<LirSignalId>,
    /// Enable info for registers: maps Reg node ID → (enable_signal, data_signal, invert_enable)
    /// Populated by pre-pass that detects Mux2→Reg enable patterns.
    /// invert_enable=true when d1=Q (sel=1→hold, need ~sel for enable)
    reg_enable_info: IndexMap<LirNodeId, (LirSignalId, LirSignalId, bool)>,
    /// Mux2 nodes absorbed into DffE/DffRE — skipped during mapping
    skip_nodes: std::collections::HashSet<LirNodeId>,
}

impl<'a> TechMapper<'a> {
    /// Create a new technology mapper
    pub fn new(library: &'a TechLibrary) -> Self {
        Self {
            library,
            netlist: GateNetlist::new(String::new(), library.name.clone()),
            signal_to_net: IndexMap::new(),
            stats: TechMapStats::default(),
            warnings: Vec::new(),
            next_cell_id: 0,
            module_safety_info: None,
            c_elem_counter: 0,
            port_constraints: IndexMap::new(),
            output_signals: std::collections::HashSet::new(),
            reg_enable_info: IndexMap::new(),
            skip_nodes: std::collections::HashSet::new(),
        }
    }

    /// Get library cell info for a function
    ///
    /// # Panics
    /// Panics if the library doesn't contain a cell for the requested function.
    fn get_cell_info(&self, function: &CellFunction) -> LibraryCellInfo {
        self.library
            .find_best_cell(function)
            .map(LibraryCellInfo::from_library_cell)
            .unwrap_or_else(|| {
                panic!(
                    "Technology library '{}' does not contain cell for function {:?}. \
                     Cannot synthesize without this cell type.",
                    self.library.name, function
                )
            })
    }

    /// Map a Lir to a GateNetlist
    pub fn map(&mut self, word_lir: &Lir) -> TechMapResult {
        self.netlist.name = word_lir.name.clone();

        // Store module-level safety info for propagation to cells
        self.module_safety_info = word_lir.module_safety_info.clone();

        // BUG #237 DEBUG: Check LIR for signals that have no driver but are used as inputs
        let mut used_signals: std::collections::HashSet<crate::lir::LirSignalId> =
            std::collections::HashSet::new();
        for node in &word_lir.nodes {
            for input in &node.inputs {
                used_signals.insert(*input);
            }
        }

        let undriven_but_used: Vec<_> = word_lir
            .signals
            .iter()
            .filter(|s| s.driver.is_none() && !s.is_input && used_signals.contains(&s.id))
            .collect();
        // Phase 1: Create nets for all signals
        for signal in &word_lir.signals {
            let nets = self.create_signal_nets(signal.id, &signal.name, signal.width);
            self.signal_to_net.insert(signal.id, nets);
        }

        // Mark input/output nets
        for &input_id in &word_lir.inputs {
            if let Some(nets) = self.signal_to_net.get(&input_id) {
                for &net_id in nets {
                    if let Some(net) = self.netlist.get_net_mut(net_id) {
                        net.is_input = true;
                    }
                    self.netlist.inputs.push(net_id);
                }
            }
        }

        for &output_id in &word_lir.outputs {
            self.output_signals.insert(output_id);
            if let Some(nets) = self.signal_to_net.get(&output_id) {
                for &net_id in nets {
                    if let Some(net) = self.netlist.get_net_mut(net_id) {
                        net.is_output = true;
                    }
                    self.netlist.outputs.push(net_id);
                }
            }
        }

        // Mark clock nets
        for &clock_id in &word_lir.clocks {
            if let Some(nets) = self.signal_to_net.get(&clock_id) {
                for &net_id in nets {
                    if let Some(net) = self.netlist.get_net_mut(net_id) {
                        net.is_clock = true;
                    }
                    self.netlist.clocks.push(net_id);
                }
            }
        }

        // Mark reset nets
        for &reset_id in &word_lir.resets {
            if let Some(nets) = self.signal_to_net.get(&reset_id) {
                for &net_id in nets {
                    if let Some(net) = self.netlist.get_net_mut(net_id) {
                        net.is_reset = true;
                    }
                    self.netlist.resets.push(net_id);
                }
            }
        }

        // Mark detection signal nets (for safety analysis)
        for &detection_id in &word_lir.detection_signals {
            if let Some(nets) = self.signal_to_net.get(&detection_id) {
                for &net_id in nets {
                    if let Some(net) = self.netlist.get_net_mut(net_id) {
                        net.is_detection = true;
                    }
                }
            }
        }

        // Phase 1.5: Detect Mux2→Reg enable patterns for DffE/DffRE absorption
        // When a Reg's D-input comes from a Mux2 where one data arm feeds back
        // the Reg's own Q output, the Mux2 is an enable gate:
        //   Mux2(sel=enable, d0=Q_feedback, d1=D_new) → Reg
        // We absorb the Mux2 into the DFF hardware (DffE/DffRE) and skip mapping it.
        self.detect_enable_patterns(word_lir);

        // Phase 2: Map each node to cells
        for node in &word_lir.nodes {
            if self.skip_nodes.contains(&node.id) {
                continue;
            }
            self.map_node(node, word_lir);
            self.stats.nodes_processed += 1;
        }

        // Phase 3: Insert global clock buffers on clock nets
        self.insert_clock_buffers();

        // Phase 4: Insert IO buffers on primary input/output ports
        self.insert_io_buffers();

        // Propagate NCL flag from LIR (set by expand_to_ncl)
        self.netlist.is_ncl = word_lir.is_ncl;

        // Update statistics
        self.netlist.update_stats();

        TechMapResult {
            netlist: std::mem::replace(
                &mut self.netlist,
                GateNetlist::new(String::new(), self.library.name.clone()),
            ),
            stats: std::mem::take(&mut self.stats),
            warnings: std::mem::take(&mut self.warnings),
        }
    }

    /// Create nets for a signal (one per bit)
    fn create_signal_nets(
        &mut self,
        _signal_id: LirSignalId,
        name: &str,
        width: u32,
    ) -> Vec<GateNetId> {
        let mut nets = Vec::new();
        for bit in 0..width {
            let net_name = if width == 1 {
                name.to_string()
            } else {
                format!("{}[{}]", name, bit)
            };
            let net_id = self.alloc_net_id();
            self.netlist.add_net(GateNet::new(net_id, net_name));
            nets.push(net_id);
            self.stats.nets_created += 1;
        }
        nets
    }

    /// Check if a signal name indicates a true rail in NCL (ends with _t)
    fn is_ncl_true_rail(signal_name: &str) -> bool {
        signal_name.ends_with("_t") || signal_name.contains("_t[") || signal_name.ends_with("_t]")
    }

    /// Check if a signal name indicates a false rail in NCL (ends with _f)
    fn is_ncl_false_rail(signal_name: &str) -> bool {
        signal_name.ends_with("_f") || signal_name.contains("_f[") || signal_name.ends_with("_f]")
    }

    /// Detect Mux2→Reg enable patterns and mark nodes for DffE/DffRE absorption.
    ///
    /// Pattern: A Reg node whose D-input signal is driven by a Mux2, where one
    /// of the Mux2's data inputs is the Reg's own output (Q feedback).
    ///
    /// Mux2 inputs are [sel, d0, d1] with sel=0→d0, sel=1→d1.
    /// Two cases:
    ///   - d0 = Q_feedback → enable = sel,  D_new = d1 (sel=1 → load new data)
    ///   - d1 = Q_feedback → enable = ~sel, D_new = d0 (sel=0 → load new data)
    ///
    /// We only handle the non-inverted case (d0 = Q_feedback) to avoid creating
    /// extra inverter logic. The inverted case is rare in practice.
    fn detect_enable_patterns(&mut self, lir: &Lir) {
        // Check if the library supports DffE or DffRE
        let has_dffe = self.library.find_best_cell(&CellFunction::DffE).is_some();
        let has_dffre = self.library.find_best_cell(&CellFunction::DffRE).is_some();
        if !has_dffe && !has_dffre {
            return;
        }

        // Build signal→driver map for quick lookup
        let signal_driver: IndexMap<LirSignalId, &LirNode> = lir
            .nodes
            .iter()
            .map(|n| (n.output, n))
            .collect();

        // Build signal use-count map (how many nodes consume each signal)
        let mut signal_use_count: IndexMap<LirSignalId, usize> = IndexMap::new();
        for node in &lir.nodes {
            for input in &node.inputs {
                *signal_use_count.entry(*input).or_insert(0) += 1;
            }
        }

        for node in &lir.nodes {
            if let LirOp::Reg { has_reset, .. } = &node.op {
                // Skip if no suitable cell: need DffRE for reset registers, DffE for plain
                if *has_reset && !has_dffre {
                    continue;
                }
                if !has_reset && !has_dffe {
                    continue;
                }

                // Reg input[0] = D signal
                let d_signal = node.inputs[0];
                let q_signal = node.output;

                // Check if D is driven by a Mux2
                if let Some(mux_node) = signal_driver.get(&d_signal) {
                    if let LirOp::Mux2 { .. } = &mux_node.op {
                        let sel_signal = mux_node.inputs[0];
                        let d0_signal = mux_node.inputs[1];
                        let d1_signal = mux_node.inputs[2];

                        if d0_signal == q_signal {
                            // d0 = Q: sel=0→hold, sel=1→load
                            self.reg_enable_info
                                .insert(node.id, (sel_signal, d1_signal, false));
                            self.skip_nodes.insert(mux_node.id);
                        } else if d1_signal == q_signal {
                            // d1 = Q: sel=1→hold, sel=0→load
                            self.reg_enable_info
                                .insert(node.id, (sel_signal, d0_signal, true));
                            self.skip_nodes.insert(mux_node.id);
                        } else {
                            // Deep pattern: check if an entire Mux2 subtree is
                            // functionally equivalent to Q (all leaves == Q).
                            let d0_is_q = Self::mux_tree_all_leaves_eq(&signal_driver, d0_signal, q_signal);
                            let d1_is_q = Self::mux_tree_all_leaves_eq(&signal_driver, d1_signal, q_signal);
                            if d0_is_q && !d1_is_q {
                                // d0 subtree ≡ Q: sel=0→hold, sel=1→load
                                self.reg_enable_info
                                    .insert(node.id, (sel_signal, d1_signal, false));
                                self.skip_nodes.insert(mux_node.id);
                                Self::collect_mux_nodes(&signal_driver, d0_signal, &signal_use_count, &mut self.skip_nodes);
                            } else if d1_is_q && !d0_is_q {
                                // d1 subtree ≡ Q: sel=1→hold, sel=0→load
                                self.reg_enable_info
                                    .insert(node.id, (sel_signal, d0_signal, true));
                                self.skip_nodes.insert(mux_node.id);
                                Self::collect_mux_nodes(&signal_driver, d1_signal, &signal_use_count, &mut self.skip_nodes);
                            }
                        }
                    }
                }
            }
        }
    }

    /// Collect Mux2 node IDs in a dead subtree (all leaves ≡ Q) for skipping.
    /// Only skips nodes whose output signal has a single consumer (exclusively
    /// part of this dead subtree). Multi-use signals are kept — they'll become
    /// identity gates that the optimizer can handle.
    fn collect_mux_nodes(
        signal_driver: &IndexMap<LirSignalId, &LirNode>,
        signal: LirSignalId,
        signal_use_count: &IndexMap<LirSignalId, usize>,
        skip_nodes: &mut std::collections::HashSet<LirNodeId>,
    ) {
        if let Some(driver) = signal_driver.get(&signal) {
            if let LirOp::Mux2 { .. } = &driver.op {
                let uses = signal_use_count.get(&signal).copied().unwrap_or(0);
                if uses <= 1 {
                    skip_nodes.insert(driver.id);
                }
                let d0 = driver.inputs[1];
                let d1 = driver.inputs[2];
                Self::collect_mux_nodes(signal_driver, d0, signal_use_count, skip_nodes);
                Self::collect_mux_nodes(signal_driver, d1, signal_use_count, skip_nodes);
            }
        }
    }

    /// Check if a MUX tree's leaves are ALL equal to `target`.
    /// Walks through Mux2 nodes recursively; at leaf nodes (non-Mux2 drivers
    /// or primary inputs), checks if the signal equals `target`.
    /// Returns true iff the subtree is functionally equivalent to `target`
    /// regardless of select signals.
    fn mux_tree_all_leaves_eq(
        signal_driver: &IndexMap<LirSignalId, &LirNode>,
        signal: LirSignalId,
        target: LirSignalId,
    ) -> bool {
        if signal == target {
            return true;
        }
        if let Some(driver) = signal_driver.get(&signal) {
            if let LirOp::Mux2 { .. } = &driver.op {
                let d0 = driver.inputs[1];
                let d1 = driver.inputs[2];
                return Self::mux_tree_all_leaves_eq(signal_driver, d0, target)
                    && Self::mux_tree_all_leaves_eq(signal_driver, d1, target);
            }
        }
        false
    }

    /// Map a single node to cells
    fn map_node(&mut self, node: &LirNode, word_lir: &Lir) {
        let input_nets = self.get_input_nets(&node.inputs);
        let output_nets = self.get_output_nets(node.output);

        match &node.op {
            // Simple logic gates - decompose to per-bit operations
            // For NCL: detect dual-rail patterns and use TH22/TH12 instead
            LirOp::And { width } => {
                // Check if this is an NCL operation by looking at signal names
                let output_signal = &word_lir.signals[node.output.0 as usize];
                let is_ncl_t = Self::is_ncl_true_rail(&output_signal.name);
                let is_ncl_f = Self::is_ncl_false_rail(&output_signal.name);

                if is_ncl_t || is_ncl_f {
                    // NCL AND on dual rails → C-element (TH22)
                    // Check if library has native TH22 cells
                    if self.library.find_best_cell(&CellFunction::Th22).is_some() {
                        // Use native TH22 from library
                        self.map_bitwise_gate(
                            CellFunction::Th22,
                            *width,
                            &input_nets,
                            &output_nets,
                            &node.path,
                        );
                    } else {
                        // Synthesize C-element from standard gates
                        // C-element: Q = (a & b) | (Q & (a | b))
                        self.map_c_element(*width, &input_nets, &output_nets, &node.path);
                    }
                } else {
                    // Regular AND
                    self.map_bitwise_gate(
                        CellFunction::And2,
                        *width,
                        &input_nets,
                        &output_nets,
                        &node.path,
                    );
                }
            }
            LirOp::Or { width } => {
                // Check if this is an NCL operation by looking at signal names
                let output_signal = &word_lir.signals[node.output.0 as usize];
                let is_ncl_t = Self::is_ncl_true_rail(&output_signal.name);
                let is_ncl_f = Self::is_ncl_false_rail(&output_signal.name);

                if is_ncl_f || is_ncl_t {
                    // NCL OR on dual rails → TH12 (1-of-2)
                    // Check if library has native TH12 cells
                    if self.library.find_best_cell(&CellFunction::Th12).is_some() {
                        // Use native TH12 from library
                        self.map_bitwise_gate(
                            CellFunction::Th12,
                            *width,
                            &input_nets,
                            &output_nets,
                            &node.path,
                        );
                    } else {
                        // Fall back to standard OR
                        // In NCL data flow, TH12 behavior is approximated by OR
                        // (full hysteresis handled by C-elements in the circuit)
                        self.map_bitwise_gate(
                            CellFunction::Or2,
                            *width,
                            &input_nets,
                            &output_nets,
                            &node.path,
                        );
                    }
                } else {
                    // Regular OR
                    self.map_bitwise_gate(
                        CellFunction::Or2,
                        *width,
                        &input_nets,
                        &output_nets,
                        &node.path,
                    );
                }
            }
            LirOp::Xor { width } => {
                self.map_bitwise_gate(
                    CellFunction::Xor2,
                    *width,
                    &input_nets,
                    &output_nets,
                    &node.path,
                );
            }
            LirOp::Nand { width } => {
                self.map_bitwise_gate(
                    CellFunction::Nand2,
                    *width,
                    &input_nets,
                    &output_nets,
                    &node.path,
                );
            }
            LirOp::Nor { width } => {
                self.map_bitwise_gate(
                    CellFunction::Nor2,
                    *width,
                    &input_nets,
                    &output_nets,
                    &node.path,
                );
            }
            LirOp::Not { width } => {
                self.map_unary_gate(
                    CellFunction::Inv,
                    *width,
                    &input_nets,
                    &output_nets,
                    &node.path,
                );
            }
            LirOp::Buf { width } | LirOp::Buffer { width } => {
                self.map_unary_gate(
                    CellFunction::Buf,
                    *width,
                    &input_nets,
                    &output_nets,
                    &node.path,
                );
            }

            // NCL Threshold Gates - direct mapping to TH12/TH22 cells
            LirOp::Th12 { width } => {
                // TH12: 1-of-2 threshold gate (OR with hysteresis)
                if self.library.find_best_cell(&CellFunction::Th12).is_some() {
                    self.map_bitwise_gate(
                        CellFunction::Th12,
                        *width,
                        &input_nets,
                        &output_nets,
                        &node.path,
                    );
                } else {
                    // Fallback: synthesize from OR (will lose hysteresis, but functional)
                    self.map_bitwise_gate(
                        CellFunction::Or2,
                        *width,
                        &input_nets,
                        &output_nets,
                        &node.path,
                    );
                }
            }
            LirOp::Th22 { width } => {
                // TH22: 2-of-2 threshold gate (C-element / AND with hysteresis)
                if self.library.find_best_cell(&CellFunction::Th22).is_some() {
                    self.map_bitwise_gate(
                        CellFunction::Th22,
                        *width,
                        &input_nets,
                        &output_nets,
                        &node.path,
                    );
                } else {
                    // Synthesize C-element from standard gates
                    self.map_c_element(*width, &input_nets, &output_nets, &node.path);
                }
            }

            // Adder - ripple carry chain
            LirOp::Add { width, has_carry, const_b } => {
                self.map_adder(*width, *has_carry, *const_b, &input_nets, &output_nets, &node.path);
            }

            // Subtractor
            LirOp::Sub { width, .. } => {
                self.map_subtractor(*width, &input_nets, &output_nets, &node.path);
            }

            // Multiplier — try DSP hard block first, fall back to gate-level
            LirOp::Mul {
                width,
                result_width,
                signed,
            } => {
                if !self.try_map_dsp_multiply(
                    *width,
                    *result_width,
                    *signed,
                    &input_nets,
                    &output_nets,
                    &node.path,
                ) {
                    // No DSP available or operands too wide — gate-level fallback
                    self.map_multiplier(
                        *width,
                        *result_width,
                        *signed,
                        &input_nets,
                        &output_nets,
                        &node.path,
                    );
                }
            }

            // Multiplexer
            LirOp::Mux2 { width } => {
                self.map_mux2(*width, &input_nets, &output_nets, &node.path);
            }

            // Comparators
            LirOp::Eq { width } => {
                self.map_equality(*width, &input_nets, &output_nets, &node.path, false);
            }
            LirOp::Ne { width } => {
                self.map_equality(*width, &input_nets, &output_nets, &node.path, true);
            }
            LirOp::Lt { width } => {
                self.map_less_than(*width, &input_nets, &output_nets, &node.path);
            }
            LirOp::Gt { width } => {
                // a > b is equivalent to b < a (swap inputs)
                if input_nets.len() >= 2 {
                    let swapped_inputs = vec![input_nets[1].clone(), input_nets[0].clone()];
                    self.map_less_than(*width, &swapped_inputs, &output_nets, &node.path);
                } else {
                    self.warnings.push(format!(
                        "GreaterThan needs 2 inputs, got {}",
                        input_nets.len()
                    ));
                }
            }
            LirOp::Le { width } => {
                // a <= b is equivalent to !(a > b) = !(b < a)
                if input_nets.len() >= 2 {
                    // First compute b < a
                    let swapped_inputs = vec![input_nets[1].clone(), input_nets[0].clone()];
                    let gt_out = self.alloc_net_id();
                    self.netlist
                        .add_net(GateNet::new(gt_out, format!("{}.gt_temp", node.path)));
                    self.map_less_than(
                        *width,
                        &swapped_inputs,
                        &[gt_out],
                        &format!("{}.gt", node.path),
                    );
                    // Then invert
                    let inv_info = self.get_cell_info(&CellFunction::Inv);
                    let y = output_nets.first().copied().unwrap_or(GateNetId(0));
                    let inv_cell = Cell::new_comb(
                        CellId(0),
                        inv_info.name.clone(),
                        self.library.name.clone(),
                        inv_info.fit,
                        format!("{}.le_inv", node.path),
                        vec![gt_out],
                        vec![y],
                    );
                    self.add_cell(inv_cell);
                } else {
                    self.warnings.push(format!(
                        "LessOrEqual needs 2 inputs, got {}",
                        input_nets.len()
                    ));
                }
            }
            LirOp::Ge { width } => {
                // a >= b is equivalent to !(a < b)
                if input_nets.len() >= 2 {
                    // First compute a < b
                    let lt_out = self.alloc_net_id();
                    self.netlist
                        .add_net(GateNet::new(lt_out, format!("{}.lt_temp", node.path)));
                    self.map_less_than(
                        *width,
                        &input_nets,
                        &[lt_out],
                        &format!("{}.lt", node.path),
                    );
                    // Then invert
                    let inv_info = self.get_cell_info(&CellFunction::Inv);
                    let y = output_nets.first().copied().unwrap_or(GateNetId(0));
                    let inv_cell = Cell::new_comb(
                        CellId(0),
                        inv_info.name.clone(),
                        self.library.name.clone(),
                        inv_info.fit,
                        format!("{}.ge_inv", node.path),
                        vec![lt_out],
                        vec![y],
                    );
                    self.add_cell(inv_cell);
                } else {
                    self.warnings.push(format!(
                        "GreaterOrEqual needs 2 inputs, got {}",
                        input_nets.len()
                    ));
                }
            }
            LirOp::Slt { width } => {
                // Signed less-than: compare as signed integers
                self.map_signed_less_than(*width, &input_nets, &output_nets, &node.path);
            }
            LirOp::Sle { width } => {
                // Signed less-than-or-equal: compare as signed integers
                self.map_signed_less_equal(*width, &input_nets, &output_nets, &node.path);
            }
            LirOp::Sgt { width } => {
                // Signed greater-than: compare as signed integers
                self.map_signed_greater_than(*width, &input_nets, &output_nets, &node.path);
            }
            LirOp::Sge { width } => {
                // Signed greater-than-or-equal: compare as signed integers
                self.map_signed_greater_equal(*width, &input_nets, &output_nets, &node.path);
            }

            // Register
            LirOp::Reg {
                width,
                has_reset,
                async_reset,
                reset_value,
                ..
            } => {
                let clock_net = node
                    .clock
                    .and_then(|c| self.signal_to_net.get(&c).and_then(|n| n.first().copied()));
                let reset_net = if *has_reset {
                    node.reset
                        .and_then(|r| self.signal_to_net.get(&r).and_then(|n| n.first().copied()))
                } else {
                    None
                };

                // Check if this Reg has an absorbed enable pattern
                let enable_info = self.reg_enable_info.get(&node.id).cloned();
                if let Some((enable_sig, data_sig, invert_enable)) = enable_info {
                    let enable_nets = self
                        .signal_to_net
                        .get(&enable_sig)
                        .cloned()
                        .unwrap_or_default();
                    let data_nets = self
                        .signal_to_net
                        .get(&data_sig)
                        .cloned()
                        .unwrap_or_default();
                    self.map_register_with_enable(
                        *width,
                        &data_nets,
                        &enable_nets,
                        invert_enable,
                        &output_nets,
                        &node.path,
                        clock_net,
                        reset_net,
                        *reset_value,
                        *async_reset,
                    );
                } else {
                    self.map_register(
                        *width,
                        &input_nets,
                        &output_nets,
                        &node.path,
                        clock_net,
                        reset_net,
                        *reset_value,
                        *async_reset,
                    );
                }
            }

            // Constant
            LirOp::Constant { width, value } => {
                self.map_constant(*width, *value, &output_nets, &node.path);
            }

            // Reductions
            LirOp::RedAnd { width } => {
                self.map_reduction(
                    CellFunction::And2,
                    *width,
                    &input_nets,
                    &output_nets,
                    &node.path,
                );
            }
            LirOp::RedOr { width } => {
                self.map_reduction(
                    CellFunction::Or2,
                    *width,
                    &input_nets,
                    &output_nets,
                    &node.path,
                );
            }
            LirOp::RedXor { width } => {
                self.map_reduction(
                    CellFunction::Xor2,
                    *width,
                    &input_nets,
                    &output_nets,
                    &node.path,
                );
            }

            // Shift operations
            LirOp::Shl { width } => {
                self.map_shift(*width, &input_nets, &output_nets, &node.path, true, false);
            }
            LirOp::Shr { width } => {
                self.map_shift(*width, &input_nets, &output_nets, &node.path, false, false);
            }
            LirOp::Sar { width } => {
                // Arithmetic right shift - sign-extend (fill with MSB instead of 0)
                self.map_shift(*width, &input_nets, &output_nets, &node.path, false, true);
            }

            // Range select: extract bits [high:low] from input
            LirOp::RangeSelect { high, low, .. } => {
                self.map_range_select(*high, *low, &input_nets, &output_nets, &node.path);
            }

            // Bit select: extract single bit from input
            LirOp::BitSelect { .. } => {
                self.map_bit_select(&input_nets, &output_nets, &node.path);
            }

            // Concat: concatenate multiple inputs
            // This is a wiring operation - each output bit comes from an input bit
            LirOp::Concat { widths } => {
                self.map_concat(widths, &input_nets, &output_nets, &node.path);
            }

            // Zero extend: copy lower bits, pad upper bits with zero
            LirOp::ZeroExtend { from, to } => {
                self.map_zero_extend(*from, *to, &input_nets, &output_nets, &node.path);
            }

            // Sign extend: copy lower bits, extend sign bit to upper bits
            LirOp::SignExtend { from, to } => {
                self.map_sign_extend(*from, *to, &input_nets, &output_nets, &node.path);
            }

            // === NCL (Null Convention Logic) Operations ===
            // Dual-rail encoding where each bit is represented by two wires (t,f)
            // 00=NULL, 01=DATA_FALSE, 10=DATA_TRUE
            LirOp::NclEncode { width } => {
                self.map_ncl_encode(*width, &input_nets, &output_nets, &node.path);
            }
            LirOp::NclDecode { width } => {
                self.map_ncl_decode(*width, &input_nets, &output_nets, &node.path);
            }
            LirOp::NclAnd { width } => {
                self.map_ncl_and(*width, &input_nets, &output_nets, &node.path);
            }
            LirOp::NclOr { width } => {
                self.map_ncl_or(*width, &input_nets, &output_nets, &node.path);
            }
            LirOp::NclXor { width } => {
                self.map_ncl_xor(*width, &input_nets, &output_nets, &node.path);
            }
            LirOp::NclNot { width } => {
                self.map_ncl_not(*width, &input_nets, &output_nets, &node.path);
            }
            LirOp::NclAdd { width } => {
                self.map_ncl_add(*width, &input_nets, &output_nets, &node.path);
            }
            LirOp::NclSub { width } => {
                self.map_ncl_sub(*width, &input_nets, &output_nets, &node.path);
            }
            LirOp::NclMul {
                input_width,
                result_width,
            } => {
                self.map_ncl_mul(
                    *input_width,
                    *result_width,
                    &input_nets,
                    &output_nets,
                    &node.path,
                );
            }
            LirOp::NclLt { width } => {
                self.map_ncl_lt(*width, &input_nets, &output_nets, &node.path);
            }
            LirOp::NclEq { width } => {
                self.map_ncl_eq(*width, &input_nets, &output_nets, &node.path);
            }
            LirOp::NclShl { width } => {
                self.map_ncl_shift(*width, true, &input_nets, &output_nets, &node.path);
            }
            LirOp::NclShr { width } => {
                self.map_ncl_shift(*width, false, &input_nets, &output_nets, &node.path);
            }
            LirOp::NclMux2 { width } => {
                self.map_ncl_mux2(*width, &input_nets, &output_nets, &node.path);
            }
            LirOp::NclReg { width } => {
                self.map_ncl_reg(*width, &input_nets, &output_nets, &node.path);
            }
            LirOp::NclComplete { width } => {
                self.map_ncl_completion(*width, &input_nets, &output_nets, &node.path);
            }
            LirOp::NclNull { width } => {
                self.map_ncl_null(*width, &output_nets, &node.path);
            }

            // Memory block — vendor-agnostic mapping to RAM primitives
            LirOp::MemBlock {
                data_width,
                addr_width,
                depth,
                has_write,
                ..
            } => {
                let clock_net = node
                    .clock
                    .and_then(|c| self.signal_to_net.get(&c).and_then(|n| n.first().copied()));
                self.map_memory_block(
                    *data_width,
                    *addr_width,
                    *depth,
                    *has_write,
                    &input_nets,
                    &output_nets,
                    &node.path,
                    clock_net,
                );
            }

            _ => {
                self.warnings
                    .push(format!("Unsupported operation: {:?}", node.op));
            }
        }
    }

    /// Get input nets from input signal IDs
    ///
    /// Returns a vector of net IDs for each input signal. If a signal is not
    /// found in the mapping (which indicates a bug), an empty vector is used
    /// as a placeholder and a warning is logged.
    fn get_input_nets(&self, inputs: &[LirSignalId]) -> Vec<Vec<GateNetId>> {
        inputs
            .iter()
            .map(|id| {
                self.signal_to_net.get(id).cloned().unwrap_or_else(|| {
                    // BUG #237: Don't silently drop missing signals - this causes
                    // downstream mapping functions to receive fewer inputs than expected,
                    // leading to early returns and undriven output nets.
                    tracing::warn!(
                        "Signal {:?} not found in signal_to_net mapping - this indicates a bug",
                        id
                    );
                    Vec::new()
                })
            })
            .collect()
    }

    /// Get output nets for an output signal ID
    fn get_output_nets(&self, output: LirSignalId) -> Vec<GateNetId> {
        self.signal_to_net.get(&output).cloned().unwrap_or_else(|| {
            // BUG #237: Log when output signal is not found
            tracing::warn!(
                "Output signal {:?} not found in signal_to_net mapping - output will be undriven",
                output
            );
            Vec::new()
        })
    }

    /// Map a bitwise binary gate (AND, OR, XOR, etc.)
    fn map_bitwise_gate(
        &mut self,
        function: CellFunction,
        width: u32,
        inputs: &[Vec<GateNetId>],
        outputs: &[GateNetId],
        path: &str,
    ) {
        if inputs.len() < 2 {
            self.warnings
                .push(format!("Bitwise gate needs 2 inputs, got {}", inputs.len()));
            return;
        }

        let cell_info = self.get_cell_info(&function);
        let tie_low = self.get_tie_low();

        for bit in 0..width as usize {
            let a = inputs[0].get(bit).copied().unwrap_or(tie_low);
            let b = inputs[1].get(bit).copied().unwrap_or(tie_low);
            let y = outputs.get(bit).copied().unwrap_or(outputs[0]);

            let mut gate_cell = Cell::new_comb(
                CellId(0),
                cell_info.name.clone(),
                self.library.name.clone(),
                cell_info.fit,
                format!("{}.bit{}", path, bit),
                vec![a, b],
                vec![y],
            );
            gate_cell.source_op = Some(format!("{:?}", function));
            cell_info.apply_to_cell(&mut gate_cell);
            self.add_cell(gate_cell);
        }

        self.stats.direct_mappings += 1;
    }

    /// Map a unary gate (NOT, BUF)
    fn map_unary_gate(
        &mut self,
        function: CellFunction,
        width: u32,
        inputs: &[Vec<GateNetId>],
        outputs: &[GateNetId],
        path: &str,
    ) {
        if inputs.is_empty() {
            self.warnings.push("Unary gate needs 1 input".to_string());
            return;
        }

        let cell_info = self.get_cell_info(&function);
        let tie_low = self.get_tie_low();

        for bit in 0..width as usize {
            let a = inputs[0].get(bit).copied().unwrap_or(tie_low);
            let y = outputs.get(bit).copied().unwrap_or(outputs[0]);

            let mut gate_cell = Cell::new_comb(
                CellId(0),
                cell_info.name.clone(),
                self.library.name.clone(),
                cell_info.fit,
                format!("{}.bit{}", path, bit),
                vec![a],
                vec![y],
            );
            gate_cell.source_op = Some(format!("{:?}", function));
            cell_info.apply_to_cell(&mut gate_cell);
            self.add_cell(gate_cell);
        }

        self.stats.direct_mappings += 1;
    }

    /// Map an adder using ripple carry
    ///
    /// Uses different strategies based on available library cells:
    /// - If Carry cell exists (FPGA with carry chain): uses XOR + Carry cells
    /// - Otherwise: uses HalfAdder + FullAdder cells
    fn map_adder(
        &mut self,
        width: u32,
        _has_carry: bool,
        const_b: Option<u64>,
        inputs: &[Vec<GateNetId>],
        outputs: &[GateNetId],
        path: &str,
    ) {
        if inputs.len() < 2 {
            self.warnings
                .push(format!("Adder needs 2 inputs, got {}", inputs.len()));
            return;
        }

        // Try to find a multi-bit adder first
        if let Some(adder) = self.library.find_best_cell(&CellFunction::Adder(width)) {
            let adder_info = LibraryCellInfo::from_library_cell(adder);
            // Use the compound adder cell
            let all_inputs: Vec<GateNetId> =
                inputs[0].iter().chain(inputs[1].iter()).copied().collect();

            let mut cell = Cell::new_comb(
                CellId(0),
                adder_info.name.clone(),
                self.library.name.clone(),
                adder_info.fit,
                path.to_string(),
                all_inputs,
                outputs.to_vec(),
            );
            cell.source_op = Some("Adder".to_string());
            adder_info.apply_to_cell(&mut cell);
            self.add_cell(cell);
            self.stats.direct_mappings += 1;
            return;
        }

        // Check for FPGA carry chain support
        if let Some(carry_cell) = self.library.find_best_cell(&CellFunction::Carry) {
            // Use FPGA carry chain: XOR for sum, Carry cell for carry propagation
            self.map_adder_fpga_carry_chain(width, const_b, inputs, outputs, path, carry_cell);
            return;
        }

        // Otherwise, decompose to half adder + full adders
        let ha_info = self.get_cell_info(&CellFunction::HalfAdder);
        let fa_info = self.get_cell_info(&CellFunction::FullAdder);
        // Use TIE_LOW for bits beyond input width (zero-extension for unsigned)
        let tie_low = self.get_tie_low();

        let mut carry_net: Option<GateNetId> = None;

        for bit in 0..width as usize {
            let a = inputs[0].get(bit).copied().unwrap_or(tie_low);
            let b = inputs[1].get(bit).copied().unwrap_or(tie_low);
            let sum = outputs.get(bit).copied().unwrap_or(outputs[0]);

            // Create carry output net
            let cout = self.alloc_net_id();
            self.netlist
                .add_net(GateNet::new(cout, format!("{}.cout{}", path, bit)));
            self.stats.nets_created += 1;

            if let Some(cin) = carry_net {
                // Full adder
                let mut cell = Cell::new_comb(
                    CellId(0),
                    fa_info.name.clone(),
                    self.library.name.clone(),
                    fa_info.fit,
                    format!("{}.fa{}", path, bit),
                    vec![a, b, cin],
                    vec![sum, cout],
                );
                cell.source_op = Some("FullAdder".to_string());
                fa_info.apply_to_cell(&mut cell);
                self.add_cell(cell);
            } else {
                // Half adder for first bit
                let mut cell = Cell::new_comb(
                    CellId(0),
                    ha_info.name.clone(),
                    self.library.name.clone(),
                    ha_info.fit,
                    format!("{}.ha{}", path, bit),
                    vec![a, b],
                    vec![sum, cout],
                );
                cell.source_op = Some("HalfAdder".to_string());
                ha_info.apply_to_cell(&mut cell);
                self.add_cell(cell);
            }

            carry_net = Some(cout);
        }

        self.stats.decomposed_mappings += 1;
    }

    /// Map an adder using FPGA carry chain primitives
    ///
    /// For FPGAs like iCE40, arithmetic uses dedicated carry chain cells:
    /// - LUT computes: sum = a XOR b XOR cin
    /// - Carry cell computes: cout = (a & b) | ((a | b) & cin)
    fn map_adder_fpga_carry_chain(
        &mut self,
        width: u32,
        const_b: Option<u64>,
        inputs: &[Vec<GateNetId>],
        outputs: &[GateNetId],
        path: &str,
        carry_cell: &LibraryCell,
    ) {
        let carry_info = LibraryCellInfo::from_library_cell(carry_cell);
        let lut4_info = self.get_cell_info(&CellFunction::Lut4);
        // Use TIE_LOW for bits beyond input width (zero-extension for unsigned)
        let tie_low_fpga = self.get_tie_low();

        let mut carry_net: Option<GateNetId> = None;

        for bit in 0..width as usize {
            let a = inputs[0].get(bit).copied().unwrap_or(tie_low_fpga);
            let sum = outputs.get(bit).copied().unwrap_or(outputs[0]);

            // Extract known constant bit value if available
            let b_bit = const_b.map(|v| ((v >> bit) & 1) as u8);

            // Create carry output net
            let cout = self.alloc_net_id();
            self.netlist
                .add_net(GateNet::new(cout, format!("{}.cout{}", path, bit)));
            self.stats.nets_created += 1;

            if let Some(cin) = carry_net {
                // Full adder stage (bit > 0)
                match b_bit {
                    Some(0) => {
                        // b=0: sum = a XOR cin (2-input), carry = a AND cin
                        // LUT INIT for XOR(I0=a, I1=cin): 0x6666
                        let mut sum_cell = Cell::new_comb(
                            CellId(0),
                            lut4_info.name.clone(),
                            self.library.name.clone(),
                            lut4_info.fit,
                            format!("{}.sum_{}", path, bit),
                            vec![a, cin],
                            vec![sum],
                        );
                        sum_cell.lut_init = Some(0x6666); // XOR(a, cin)
                        sum_cell.source_op = Some("XOR2_const".to_string());
                        lut4_info.apply_to_cell(&mut sum_cell);
                        self.add_cell(sum_cell);

                        // Carry: when b=0, cout = a AND cin
                        // SB_CARRY computes: cout = (I0 & I1) | ((I0 | I1) & CI)
                        // With I1=0: cout = 0 | (I0 & CI) = a & cin — correct!
                        let mut cc = Cell::new_comb(
                            CellId(0),
                            carry_info.name.clone(),
                            self.library.name.clone(),
                            carry_info.fit,
                            format!("{}.carry{}", path, bit),
                            vec![a, tie_low_fpga, cin],
                            vec![cout],
                        );
                        cc.source_op = Some("Carry".to_string());
                        carry_info.apply_to_cell(&mut cc);
                        self.add_cell(cc);
                    }
                    Some(1) => {
                        // b=1: sum = a XOR 1 XOR cin = NOT(a) XOR cin = XNOR(a, cin)
                        // LUT INIT for XNOR(I0=a, I1=cin): 0x9999
                        let mut sum_cell = Cell::new_comb(
                            CellId(0),
                            lut4_info.name.clone(),
                            self.library.name.clone(),
                            lut4_info.fit,
                            format!("{}.sum_{}", path, bit),
                            vec![a, cin],
                            vec![sum],
                        );
                        sum_cell.lut_init = Some(0x9999); // XNOR(a, cin)
                        sum_cell.source_op = Some("XNOR2_const".to_string());
                        lut4_info.apply_to_cell(&mut sum_cell);
                        self.add_cell(sum_cell);

                        // Carry: when b=1, cout = (a & 1) | ((a | 1) & cin) = a | cin
                        // SB_CARRY with I1=1: cout = (I0 & 1) | ((I0 | 1) & CI) = I0 | CI
                        // But we need I1 to be high. Use tie_high.
                        let tie_high_fpga = self.get_tie_high();
                        let mut cc = Cell::new_comb(
                            CellId(0),
                            carry_info.name.clone(),
                            self.library.name.clone(),
                            carry_info.fit,
                            format!("{}.carry{}", path, bit),
                            vec![a, tie_high_fpga, cin],
                            vec![cout],
                        );
                        cc.source_op = Some("Carry".to_string());
                        carry_info.apply_to_cell(&mut cc);
                        self.add_cell(cc);
                    }
                    _ => {
                        // Non-constant b: standard 3-input XOR
                        let b = inputs[1].get(bit).copied().unwrap_or(tie_low_fpga);
                        let mut sum_cell = Cell::new_comb(
                            CellId(0),
                            lut4_info.name.clone(),
                            self.library.name.clone(),
                            lut4_info.fit,
                            format!("{}.sum_{}", path, bit),
                            vec![a, b, cin],
                            vec![sum],
                        );
                        sum_cell.lut_init = Some(0x9696);
                        sum_cell.source_op = Some("XOR3".to_string());
                        lut4_info.apply_to_cell(&mut sum_cell);
                        self.add_cell(sum_cell);

                        let mut cc = Cell::new_comb(
                            CellId(0),
                            carry_info.name.clone(),
                            self.library.name.clone(),
                            carry_info.fit,
                            format!("{}.carry{}", path, bit),
                            vec![a, b, cin],
                            vec![cout],
                        );
                        cc.source_op = Some("Carry".to_string());
                        carry_info.apply_to_cell(&mut cc);
                        self.add_cell(cc);
                    }
                }
            } else {
                // Half adder for first bit (bit 0, no carry in)
                match b_bit {
                    Some(0) => {
                        // a + 0: sum = a (buffer), carry = 0
                        // LUT as buffer: INIT = 0xAAAA (pass-through I0)
                        let mut sum_cell = Cell::new_comb(
                            CellId(0),
                            lut4_info.name.clone(),
                            self.library.name.clone(),
                            lut4_info.fit,
                            format!("{}.sum_{}", path, bit),
                            vec![a],
                            vec![sum],
                        );
                        sum_cell.lut_init = Some(0xAAAA);
                        sum_cell.source_op = Some("BUF_const".to_string());
                        lut4_info.apply_to_cell(&mut sum_cell);
                        self.add_cell(sum_cell);

                        // carry = a AND 0 = 0, just use tie_low for carry
                        // Connect cout to tie_low — but we need a net.
                        // Actually, map carry cell with b=0: cout = a & 0 = 0
                        let mut cc = Cell::new_comb(
                            CellId(0),
                            carry_info.name.clone(),
                            self.library.name.clone(),
                            carry_info.fit,
                            format!("{}.carry{}", path, bit),
                            vec![a, tie_low_fpga, tie_low_fpga],
                            vec![cout],
                        );
                        cc.source_op = Some("Carry".to_string());
                        carry_info.apply_to_cell(&mut cc);
                        self.add_cell(cc);
                    }
                    Some(1) => {
                        // a + 1: sum = NOT(a), carry = a
                        // LUT as inverter: INIT = 0x5555
                        let mut sum_cell = Cell::new_comb(
                            CellId(0),
                            lut4_info.name.clone(),
                            self.library.name.clone(),
                            lut4_info.fit,
                            format!("{}.sum_{}", path, bit),
                            vec![a],
                            vec![sum],
                        );
                        sum_cell.lut_init = Some(0x5555);
                        sum_cell.source_op = Some("INV_const".to_string());
                        lut4_info.apply_to_cell(&mut sum_cell);
                        self.add_cell(sum_cell);

                        // carry = a AND 1 = a
                        // SB_CARRY with I1=1, CI=0: cout = (a & 1) | ((a|1) & 0) = a
                        let tie_high_fpga = self.get_tie_high();
                        let mut cc = Cell::new_comb(
                            CellId(0),
                            carry_info.name.clone(),
                            self.library.name.clone(),
                            carry_info.fit,
                            format!("{}.carry{}", path, bit),
                            vec![a, tie_high_fpga, tie_low_fpga],
                            vec![cout],
                        );
                        cc.source_op = Some("Carry".to_string());
                        carry_info.apply_to_cell(&mut cc);
                        self.add_cell(cc);
                    }
                    _ => {
                        // Non-constant b: standard half adder
                        let b = inputs[1].get(bit).copied().unwrap_or(tie_low_fpga);
                        let xor_info = self.get_cell_info(&CellFunction::Xor2);
                        let mut xor_cell = Cell::new_comb(
                            CellId(0),
                            xor_info.name.clone(),
                            self.library.name.clone(),
                            xor_info.fit,
                            format!("{}.xor_{}", path, bit),
                            vec![a, b],
                            vec![sum],
                        );
                        xor_cell.source_op = Some("XOR".to_string());
                        xor_info.apply_to_cell(&mut xor_cell);
                        self.add_cell(xor_cell);

                        let and_info = self.get_cell_info(&CellFunction::And2);
                        let mut and_cell = Cell::new_comb(
                            CellId(0),
                            and_info.name.clone(),
                            self.library.name.clone(),
                            and_info.fit,
                            format!("{}.and_{}", path, bit),
                            vec![a, b],
                            vec![cout],
                        );
                        and_cell.source_op = Some("AND".to_string());
                        and_info.apply_to_cell(&mut and_cell);
                        self.add_cell(and_cell);
                    }
                }
            }

            carry_net = Some(cout);
        }

        self.stats.decomposed_mappings += 1;
    }

    /// Map a subtractor (a - b = a + ~b + 1)
    ///
    /// Uses FPGA carry chain when available, otherwise falls back to FullAdder cells.
    fn map_subtractor(
        &mut self,
        width: u32,
        inputs: &[Vec<GateNetId>],
        outputs: &[GateNetId],
        path: &str,
    ) {
        if inputs.len() < 2 {
            self.warnings
                .push(format!("Subtractor needs 2 inputs, got {}", inputs.len()));
            return;
        }

        // Check for FPGA carry chain support
        if let Some(carry_cell) = self.library.find_best_cell(&CellFunction::Carry).cloned() {
            self.map_subtractor_fpga_carry_chain(width, inputs, outputs, path, &carry_cell);
            return;
        }

        // Fallback: Invert b, then add with carry_in=1 using FullAdder cells
        let inv_info = self.get_cell_info(&CellFunction::Inv);
        let fa_info = self.get_cell_info(&CellFunction::FullAdder);
        let sub_tie_low = self.get_tie_low();

        // Create inverted b nets
        let mut inv_b: Vec<GateNetId> = Vec::new();
        for bit in 0..width as usize {
            let b = inputs[1].get(bit).copied().unwrap_or(sub_tie_low);
            let not_b = self.alloc_net_id();
            self.netlist
                .add_net(GateNet::new(not_b, format!("{}.not_b{}", path, bit)));
            self.stats.nets_created += 1;

            let mut cell = Cell::new_comb(
                CellId(0),
                inv_info.name.clone(),
                self.library.name.clone(),
                inv_info.fit,
                format!("{}.inv_b{}", path, bit),
                vec![b],
                vec![not_b],
            );
            cell.source_op = Some("Inverter".to_string());
            inv_info.apply_to_cell(&mut cell);
            self.add_cell(cell);
            inv_b.push(not_b);
        }

        // Create constant 1 for initial carry (carry_in = 1 for two's complement subtraction)
        let const_1 = self.alloc_net_id();
        self.netlist
            .add_net(GateNet::new(const_1, format!("{}.const_1", path)));
        self.stats.nets_created += 1;

        let mut tie_cell = Cell::new_comb(
            CellId(0),
            "TIE_HIGH".to_string(),
            self.library.name.clone(),
            0.01,
            format!("{}.tie_high", path),
            vec![],
            vec![const_1],
        );
        tie_cell.source_op = Some("Constant".to_string());
        self.add_cell(tie_cell);

        let mut carry_net = const_1;

        for (bit, &not_b) in inv_b.iter().enumerate().take(width as usize) {
            let a = inputs[0].get(bit).copied().unwrap_or(sub_tie_low);
            let diff = outputs.get(bit).copied().unwrap_or(outputs[0]);

            let cout = self.alloc_net_id();
            self.netlist
                .add_net(GateNet::new(cout, format!("{}.cout{}", path, bit)));
            self.stats.nets_created += 1;

            let mut cell = Cell::new_comb(
                CellId(0),
                fa_info.name.clone(),
                self.library.name.clone(),
                fa_info.fit,
                format!("{}.sub_fa{}", path, bit),
                vec![a, not_b, carry_net],
                vec![diff, cout],
            );
            cell.source_op = Some("FullAdder".to_string());
            fa_info.apply_to_cell(&mut cell);
            self.add_cell(cell);

            carry_net = cout;
        }

        self.stats.decomposed_mappings += 1;
    }

    /// Map a subtractor using FPGA carry chain (a - b = a + ~b + 1)
    ///
    /// Uses LUT4 for XNOR (a XOR ~b = a XNOR b) and Carry cells for borrow propagation.
    /// Carry_in = 1 (TIE_HIGH) for two's complement subtraction.
    fn map_subtractor_fpga_carry_chain(
        &mut self,
        width: u32,
        inputs: &[Vec<GateNetId>],
        outputs: &[GateNetId],
        path: &str,
        carry_cell: &LibraryCell,
    ) {
        let carry_info = LibraryCellInfo::from_library_cell(carry_cell);
        let lut4_info = self.get_cell_info(&CellFunction::Lut4);
        let sub_tie_low = self.get_tie_low();

        // Create TIE_HIGH for carry_in = 1
        let const_1 = self.alloc_net_id();
        self.netlist
            .add_net(GateNet::new(const_1, format!("{}.carry_in", path)));
        self.stats.nets_created += 1;

        let mut tie_cell = Cell::new_comb(
            CellId(0),
            "TIE_HIGH".to_string(),
            self.library.name.clone(),
            0.01,
            format!("{}.tie_high", path),
            vec![],
            vec![const_1],
        );
        tie_cell.function = Some(CellFunction::TieHigh);
        tie_cell.source_op = Some("Constant".to_string());
        self.add_cell(tie_cell);

        let mut carry_net = const_1;

        for bit in 0..width as usize {
            let a = inputs[0].get(bit).copied().unwrap_or(sub_tie_low);
            let b = inputs[1].get(bit).copied().unwrap_or(sub_tie_low);
            let diff = outputs.get(bit).copied().unwrap_or(outputs[0]);

            let cout = self.alloc_net_id();
            self.netlist
                .add_net(GateNet::new(cout, format!("{}.cout{}", path, bit)));
            self.stats.nets_created += 1;

            // Subtraction: diff = a XOR ~b XOR cin = a XNOR b XOR cin
            // For bit 0: diff = a XNOR b XOR 1 = a XOR b (since cin=1)
            //   which equals half-adder with inverted logic
            // For bits 1+: use LUT4 with INIT = 0x6969 (a XNOR b XOR cin)

            if bit == 0 {
                // First bit with carry_in=1: diff = a XOR ~b XOR 1 = a XNOR b XOR 1
                // Use LUT4: INIT = 0x6969 for (a XNOR b XOR cin) with cin=1
                let mut sum_cell = Cell::new_comb(
                    CellId(0),
                    lut4_info.name.clone(),
                    self.library.name.clone(),
                    lut4_info.fit,
                    format!("{}.diff_{}", path, bit),
                    vec![a, b, carry_net],
                    vec![diff],
                );
                sum_cell.lut_init = Some(0x6969);
                sum_cell.source_op = Some("SUB_XOR3".to_string());
                lut4_info.apply_to_cell(&mut sum_cell);
                self.add_cell(sum_cell);
            } else {
                // LUT4: diff = a XNOR b XOR cin (INIT = 0x6969)
                let mut sum_cell = Cell::new_comb(
                    CellId(0),
                    lut4_info.name.clone(),
                    self.library.name.clone(),
                    lut4_info.fit,
                    format!("{}.diff_{}", path, bit),
                    vec![a, b, carry_net],
                    vec![diff],
                );
                sum_cell.lut_init = Some(0x6969);
                sum_cell.source_op = Some("SUB_XOR3".to_string());
                lut4_info.apply_to_cell(&mut sum_cell);
                self.add_cell(sum_cell);
            }

            // Carry cell: cout = (~b & a) | ((~b | a) & cin) = (a & ~b) | ((a | ~b) & cin)
            // This is the standard carry cell with b inverted.
            // On iCE40, the SB_CARRY computes: CO = (I0 & I1) | ((I0 | I1) & CI)
            // For subtraction, we feed ~b as I0 (or I1). But we don't have ~b as a net.
            // Alternative: SB_CARRY with I0=a, I1=~b. We need to create ~b.
            // Actually, for the carry chain: we need the carry to propagate correctly.
            // a + ~b + cin: carry = (a & ~b) | ((a | ~b) & cin)
            // With SB_CARRY(I0, I1, CI) = (I0 & I1) | ((I0 | I1) & CI)
            // We need I0=a, I1=~b.

            // Create inverted b for carry cell
            let inv_info = self.get_cell_info(&CellFunction::Inv);
            let not_b = self.alloc_net_id();
            self.netlist
                .add_net(GateNet::new(not_b, format!("{}.not_b{}", path, bit)));
            self.stats.nets_created += 1;

            let mut inv_cell = Cell::new_comb(
                CellId(0),
                inv_info.name.clone(),
                self.library.name.clone(),
                inv_info.fit,
                format!("{}.inv_b{}", path, bit),
                vec![b],
                vec![not_b],
            );
            inv_cell.source_op = Some("Inverter".to_string());
            inv_info.apply_to_cell(&mut inv_cell);
            self.add_cell(inv_cell);

            let mut carry_cell = Cell::new_comb(
                CellId(0),
                carry_info.name.clone(),
                self.library.name.clone(),
                carry_info.fit,
                format!("{}.carry{}", path, bit),
                vec![a, not_b, carry_net],
                vec![cout],
            );
            carry_cell.source_op = Some("Carry".to_string());
            carry_info.apply_to_cell(&mut carry_cell);
            self.add_cell(carry_cell);

            carry_net = cout;
        }

        self.stats.decomposed_mappings += 1;
    }

    // =========================================================================
    // DSP Hard Block Mapping
    // =========================================================================

    /// Try to map a multiply operation to DSP hard blocks.
    ///
    /// Returns `true` if the multiply was mapped to DSP, `false` if no DSP
    /// is available or operands are too wide (caller should fall back to
    /// gate-level decomposition).
    fn try_map_dsp_multiply(
        &mut self,
        width: u32,
        result_width: u32,
        signed: bool,
        inputs: &[Vec<GateNetId>],
        outputs: &[GateNetId],
        path: &str,
    ) -> bool {
        // Query library for DSP capability
        let (dsp_cell_name, dsp_cell_fit, dsp_info) = match self.library.find_dsp_cell() {
            Some((cell, info)) => (cell.name.clone(), cell.fit, info.clone()),
            None => return false, // No DSP in this library
        };

        let dsp_cell_info = self
            .library
            .find_best_cell(&CellFunction::Dsp)
            .map(LibraryCellInfo::from_library_cell)
            .unwrap();

        if width <= dsp_info.a_width && width <= dsp_info.b_width {
            // Single DSP block is sufficient
            self.instantiate_dsp_block(
                &dsp_cell_name,
                dsp_cell_fit,
                &dsp_cell_info,
                &dsp_info,
                width,
                width,
                result_width,
                signed,
                inputs,
                outputs,
                path,
            );
            true
        } else if width <= dsp_info.a_width * 2 && width <= dsp_info.b_width * 2 {
            // Operands fit in 2x DSP width — use tiled decomposition
            self.map_dsp_wide_multiply(
                &dsp_cell_name,
                dsp_cell_fit,
                &dsp_cell_info,
                &dsp_info,
                width,
                result_width,
                signed,
                inputs,
                outputs,
                path,
            );
            true
        } else {
            // Too wide for DSP — fall back to gate-level
            false
        }
    }

    /// Instantiate a single DSP hard multiplier block.
    ///
    /// Wires operands (zero/sign-extended to DSP width), ties control signals,
    /// and connects product output bits.
    #[allow(clippy::too_many_arguments)]
    fn instantiate_dsp_block(
        &mut self,
        dsp_cell_name: &str,
        dsp_cell_fit: f64,
        dsp_cell_info: &LibraryCellInfo,
        dsp_info: &crate::tech_library::DspCellInfo,
        a_width: u32,
        b_width: u32,
        result_width: u32,
        signed: bool,
        inputs: &[Vec<GateNetId>],
        outputs: &[GateNetId],
        path: &str,
    ) {
        let tie_low = self.get_tie_low();
        let tie_high = self.get_tie_high();

        // Determine sign extension bit for each operand
        let a_sign = if signed {
            inputs[0]
                .get(a_width as usize - 1)
                .copied()
                .unwrap_or(tie_low)
        } else {
            tie_low
        };
        let b_sign = if signed {
            inputs[1]
                .get(b_width as usize - 1)
                .copied()
                .unwrap_or(tie_low)
        } else {
            tie_low
        };

        // Build input list matching the cell's pin order from the library
        // Order: A[0..a_width], B[0..b_width], C[0..18], SIGNEDA, SIGNEDB,
        //        SOURCEA, SOURCEB, CLK[0..3], CE[0..3], RST[0..3],
        //        SRIA[0..17], SRIB[0..17]
        let mut cell_inputs = Vec::new();

        // A input — pad/extend to DSP a_width
        for bit in 0..dsp_info.a_width as usize {
            if bit < a_width as usize {
                cell_inputs.push(inputs[0].get(bit).copied().unwrap_or(tie_low));
            } else {
                // Zero-extend (unsigned) or sign-extend (signed)
                cell_inputs.push(a_sign);
            }
        }

        // B input — pad/extend to DSP b_width
        for bit in 0..dsp_info.b_width as usize {
            if bit < b_width as usize {
                cell_inputs.push(inputs[1].get(bit).copied().unwrap_or(tie_low));
            } else {
                cell_inputs.push(b_sign);
            }
        }

        // C input — tie low (not used for simple multiply)
        for _ in 0..18 {
            cell_inputs.push(tie_low);
        }

        // SIGNEDA, SIGNEDB — tie high for signed, low for unsigned
        let signed_net = if signed { tie_high } else { tie_low };
        cell_inputs.push(signed_net); // SIGNEDA
        cell_inputs.push(signed_net); // SIGNEDB

        // SOURCEA, SOURCEB — tie low (direct input, not from cascade)
        cell_inputs.push(tie_low); // SOURCEA
        cell_inputs.push(tie_low); // SOURCEB

        // CLK[0..3] — tie low (combinational mode)
        for _ in 0..4 {
            cell_inputs.push(tie_low);
        }

        // CE[0..3] — tie low (combinational mode)
        for _ in 0..4 {
            cell_inputs.push(tie_low);
        }

        // RST[0..3] — tie low (combinational mode)
        for _ in 0..4 {
            cell_inputs.push(tie_low);
        }

        // SRIA[0..17] — tie low (shift register not used)
        for _ in 0..18 {
            cell_inputs.push(tie_low);
        }

        // SRIB[0..17] — tie low (shift register not used)
        for _ in 0..18 {
            cell_inputs.push(tie_low);
        }

        // Build output list: P[0..35], SIGNEDP, SROA[0..17], SROB[0..17],
        //                     ROA[0..17], ROB[0..17], ROC[0..17]
        let mut cell_outputs = Vec::new();

        // P output — connect result_width bits to output, rest unused
        for bit in 0..dsp_info.p_width as usize {
            if bit < result_width as usize && bit < outputs.len() {
                cell_outputs.push(outputs[bit]);
            } else {
                let unused = self
                    .netlist
                    .add_net_with_name(format!("{}.dsp_unused_p{}", path, bit));
                cell_outputs.push(unused);
            }
        }

        // SIGNEDP — unused
        let signedp_unused = self
            .netlist
            .add_net_with_name(format!("{}.dsp_unused_signedp", path));
        cell_outputs.push(signedp_unused);

        // Cascade/shift outputs — all unused
        for prefix in &["sroa", "srob", "roa", "rob", "roc"] {
            for bit in 0..18 {
                let unused = self
                    .netlist
                    .add_net_with_name(format!("{}.dsp_unused_{}_{}", path, prefix, bit));
                cell_outputs.push(unused);
            }
        }

        // Create the DSP cell
        let mut cell = Cell::new_comb(
            CellId(0),
            dsp_cell_name.to_string(),
            self.library.name.clone(),
            dsp_cell_fit,
            path.to_string(),
            cell_inputs,
            cell_outputs,
        );
        cell.source_op = Some("DspMultiply".to_string());

        // Set parameters for combinational mode
        cell.parameters
            .insert("REG_INPUTA_CLK".to_string(), "NONE".to_string());
        cell.parameters
            .insert("REG_INPUTB_CLK".to_string(), "NONE".to_string());
        cell.parameters
            .insert("REG_INPUTC_CLK".to_string(), "NONE".to_string());
        cell.parameters
            .insert("REG_PIPELINE_CLK".to_string(), "NONE".to_string());
        cell.parameters
            .insert("REG_OUTPUT_CLK".to_string(), "NONE".to_string());
        cell.parameters
            .insert("SOURCEB_MODE".to_string(), "B_INPUT".to_string());
        cell.parameters
            .insert("MULT_BYPASS".to_string(), "DISABLED".to_string());
        cell.parameters
            .insert("RESETMODE".to_string(), "SYNC".to_string());

        if signed {
            cell.parameters
                .insert("SIGNED_MODE".to_string(), "SIGNED".to_string());
        } else {
            cell.parameters
                .insert("SIGNED_MODE".to_string(), "UNSIGNED".to_string());
        }

        dsp_cell_info.apply_to_cell(&mut cell);
        self.add_cell(cell);
        self.stats.direct_mappings += 1;
    }

    /// Map a wide multiply (operands > single DSP width, ≤ 2x) using
    /// 4 DSP blocks in a tiled decomposition.
    ///
    /// For operands A and B each split into hi/lo halves:
    /// ```text
    /// A = A_hi * 2^N + A_lo
    /// B = B_hi * 2^N + B_lo
    /// P = A_lo*B_lo + (A_lo*B_hi + A_hi*B_lo) << N + A_hi*B_hi << 2N
    /// ```
    /// where N = dsp_info.a_width (e.g., 18 for MULT18X18D).
    #[allow(clippy::too_many_arguments)]
    fn map_dsp_wide_multiply(
        &mut self,
        dsp_cell_name: &str,
        dsp_cell_fit: f64,
        dsp_cell_info: &LibraryCellInfo,
        dsp_info: &crate::tech_library::DspCellInfo,
        width: u32,
        result_width: u32,
        signed: bool,
        inputs: &[Vec<GateNetId>],
        outputs: &[GateNetId],
        path: &str,
    ) {
        let n = dsp_info.a_width; // Split point (e.g., 18)
        let tie_low = self.get_tie_low();

        // Split A and B into hi/lo halves
        let a_lo: Vec<GateNetId> = (0..n as usize)
            .map(|i| inputs[0].get(i).copied().unwrap_or(tie_low))
            .collect();
        let a_hi: Vec<GateNetId> = (n as usize..width as usize)
            .map(|i| inputs[0].get(i).copied().unwrap_or(tie_low))
            .collect();
        let b_lo: Vec<GateNetId> = (0..n as usize)
            .map(|i| inputs[1].get(i).copied().unwrap_or(tie_low))
            .collect();
        let b_hi: Vec<GateNetId> = (n as usize..width as usize)
            .map(|i| inputs[1].get(i).copied().unwrap_or(tie_low))
            .collect();

        let a_hi_width = width - n;
        let b_hi_width = width - n;

        // Product widths for each partial multiply
        let pp_ll_width = dsp_info.p_width; // A_lo * B_lo (full DSP width)
        let pp_lh_width = n + b_hi_width; // A_lo * B_hi
        let pp_hl_width = a_hi_width + n; // A_hi * B_lo
        let pp_hh_width = a_hi_width + b_hi_width; // A_hi * B_hi

        // Allocate output nets for each partial product
        let mut pp_ll_outs = Vec::new();
        for i in 0..pp_ll_width {
            let net = self
                .netlist
                .add_net_with_name(format!("{}.dsp_pp_ll_{}", path, i));
            pp_ll_outs.push(net);
        }

        let mut pp_lh_outs = Vec::new();
        for i in 0..pp_lh_width {
            let net = self
                .netlist
                .add_net_with_name(format!("{}.dsp_pp_lh_{}", path, i));
            pp_lh_outs.push(net);
        }

        let mut pp_hl_outs = Vec::new();
        for i in 0..pp_hl_width {
            let net = self
                .netlist
                .add_net_with_name(format!("{}.dsp_pp_hl_{}", path, i));
            pp_hl_outs.push(net);
        }

        let mut pp_hh_outs = Vec::new();
        for i in 0..pp_hh_width {
            let net = self
                .netlist
                .add_net_with_name(format!("{}.dsp_pp_hh_{}", path, i));
            pp_hh_outs.push(net);
        }

        // Instantiate 4 DSP blocks
        // PP_LL = A_lo * B_lo (unsigned — lo halves are always positive)
        self.instantiate_dsp_block(
            dsp_cell_name,
            dsp_cell_fit,
            dsp_cell_info,
            dsp_info,
            n,
            n,
            pp_ll_width,
            false, // lo * lo is always unsigned
            &[a_lo.clone(), b_lo.clone()],
            &pp_ll_outs,
            &format!("{}.dsp_ll", path),
        );

        // PP_LH = A_lo * B_hi
        self.instantiate_dsp_block(
            dsp_cell_name,
            dsp_cell_fit,
            dsp_cell_info,
            dsp_info,
            n,
            b_hi_width,
            pp_lh_width,
            signed, // B_hi may be signed
            &[a_lo, b_hi.clone()],
            &pp_lh_outs,
            &format!("{}.dsp_lh", path),
        );

        // PP_HL = A_hi * B_lo
        self.instantiate_dsp_block(
            dsp_cell_name,
            dsp_cell_fit,
            dsp_cell_info,
            dsp_info,
            a_hi_width,
            n,
            pp_hl_width,
            signed, // A_hi may be signed
            &[a_hi.clone(), b_lo],
            &pp_hl_outs,
            &format!("{}.dsp_hl", path),
        );

        // PP_HH = A_hi * B_hi
        self.instantiate_dsp_block(
            dsp_cell_name,
            dsp_cell_fit,
            dsp_cell_info,
            dsp_info,
            a_hi_width,
            b_hi_width,
            pp_hh_width,
            signed,
            &[a_hi, b_hi],
            &pp_hh_outs,
            &format!("{}.dsp_hh", path),
        );

        // Now combine: P = PP_LL + (PP_LH + PP_HL) << N + PP_HH << 2N
        // Step 1: Add PP_LH + PP_HL (cross terms, both shifted by N)
        let cross_width = std::cmp::max(pp_lh_width, pp_hl_width) + 1;
        let mut cross_sum_outs = Vec::new();
        for i in 0..cross_width {
            let net = self
                .netlist
                .add_net_with_name(format!("{}.dsp_cross_sum_{}", path, i));
            cross_sum_outs.push(net);
        }

        // Pad shorter operand
        let pp_lh_padded: Vec<GateNetId> = (0..cross_width as usize)
            .map(|i| pp_lh_outs.get(i).copied().unwrap_or(tie_low))
            .collect();
        let pp_hl_padded: Vec<GateNetId> = (0..cross_width as usize)
            .map(|i| pp_hl_outs.get(i).copied().unwrap_or(tie_low))
            .collect();

        // Use gate-level adder for the summation
        self.map_adder(
            cross_width,
            false,
            None,
            &[pp_lh_padded, pp_hl_padded],
            &cross_sum_outs,
            &format!("{}.dsp_cross_add", path),
        );

        // Step 2: Build final result by positional accumulation
        // Result bits [0..N) come from PP_LL[0..N)
        // Result bits [N..2N) come from PP_LL[N..2N) + cross_sum[0..N) + PP_HH shifted
        // This is equivalent to a big addition with shifts

        // Build shifted terms for final addition:
        // Term A: PP_LL (no shift)
        // Term B: cross_sum << N
        // Term C: PP_HH << 2N
        // We do: (PP_LL + cross_sum << N) + PP_HH << 2N

        // First addition: PP_LL + (cross_sum << N)
        let add1_width = std::cmp::min(result_width, n + cross_width + 1);
        let mut add1_a = Vec::new();
        let mut add1_b = Vec::new();
        for i in 0..add1_width as usize {
            // Term A: PP_LL
            add1_a.push(pp_ll_outs.get(i).copied().unwrap_or(tie_low));
            // Term B: cross_sum << N
            if i < n as usize {
                add1_b.push(tie_low);
            } else {
                add1_b.push(
                    cross_sum_outs
                        .get(i - n as usize)
                        .copied()
                        .unwrap_or(tie_low),
                );
            }
        }

        let mut add1_outs = Vec::new();
        for i in 0..add1_width {
            let net = self
                .netlist
                .add_net_with_name(format!("{}.dsp_add1_{}", path, i));
            add1_outs.push(net);
        }

        self.map_adder(
            add1_width,
            false,
            None,
            &[add1_a, add1_b],
            &add1_outs,
            &format!("{}.dsp_add1", path),
        );

        // Second addition: add1_result + (PP_HH << 2N)
        let final_width = std::cmp::min(result_width, add1_width.max(2 * n + pp_hh_width) + 1);
        let mut add2_a = Vec::new();
        let mut add2_b = Vec::new();
        for i in 0..final_width as usize {
            add2_a.push(add1_outs.get(i).copied().unwrap_or(tie_low));
            if i < (2 * n) as usize {
                add2_b.push(tie_low);
            } else {
                add2_b.push(
                    pp_hh_outs
                        .get(i - (2 * n) as usize)
                        .copied()
                        .unwrap_or(tie_low),
                );
            }
        }

        // Final addition outputs directly to the result
        let mut final_outs = Vec::new();
        for i in 0..final_width as usize {
            if i < result_width as usize && i < outputs.len() {
                final_outs.push(outputs[i]);
            } else {
                let net = self
                    .netlist
                    .add_net_with_name(format!("{}.dsp_final_unused_{}", path, i));
                final_outs.push(net);
            }
        }

        self.map_adder(
            final_width,
            false,
            None,
            &[add2_a, add2_b],
            &final_outs,
            &format!("{}.dsp_final_add", path),
        );

        // Connect any remaining output bits that weren't covered
        // (e.g., if result_width > final_width, those bits are zero)
        // This shouldn't happen in practice since final_width >= result_width

        self.stats.decomposed_mappings += 1;
    }

    /// Map a multiplier using shift-and-add algorithm
    /// result = a * b, where both inputs have 'width' bits and result has 'result_width' bits
    /// For signed multiplication, uses sign-extension to ensure correct results.
    fn map_multiplier(
        &mut self,
        width: u32,
        result_width: u32,
        signed: bool,
        inputs: &[Vec<GateNetId>],
        outputs: &[GateNetId],
        path: &str,
    ) {
        if inputs.len() < 2 {
            self.warnings
                .push(format!("Multiplier needs 2 inputs, got {}", inputs.len()));
            return;
        }

        // For signed multiplication, use sign-magnitude approach:
        // 1. XOR sign bits to get result sign
        // 2. Conditionally negate inputs to get magnitudes
        // 3. Unsigned multiply magnitudes
        // 4. Conditionally negate result based on sign
        if signed {
            self.map_signed_multiplier(width, result_width, inputs, outputs, path);
            return;
        }

        let and_info = self.get_cell_info(&CellFunction::And2);
        let ha_info = self.get_cell_info(&CellFunction::HalfAdder);
        let fa_info = self.get_cell_info(&CellFunction::FullAdder);
        let tie_low = self.get_tie_low();

        // Create partial products: pp[i][j] = a[j] & b[i]
        // This creates a grid of AND gates
        let mut partial_products: Vec<Vec<GateNetId>> = Vec::new();
        let n = width as usize;

        for i in 0..n {
            let mut row: Vec<GateNetId> = Vec::new();
            let b_i = inputs[1].get(i).copied().unwrap_or(tie_low);

            for j in 0..n {
                let a_j = inputs[0].get(j).copied().unwrap_or(tie_low);
                let pp = self.alloc_net_id();
                self.netlist
                    .add_net(GateNet::new(pp, format!("{}.pp_{}_{}", path, i, j)));
                self.stats.nets_created += 1;

                let mut cell = Cell::new_comb(
                    CellId(0),
                    and_info.name.clone(),
                    self.library.name.clone(),
                    and_info.fit,
                    format!("{}.pp_and_{}_{}", path, i, j),
                    vec![a_j, b_i],
                    vec![pp],
                );
                cell.source_op = Some("PartialProduct".to_string());
                and_info.apply_to_cell(&mut cell);
                self.add_cell(cell);
                row.push(pp);
            }
            partial_products.push(row);
        }

        // Now we need to add up all partial products with proper shifting
        // pp[0] contributes to bits 0..width
        // pp[1] contributes to bits 1..width+1
        // etc.

        // Create a result accumulator that we'll build up
        // We'll use a ripple-carry array multiplier approach

        // Initialize result with partial product 0 (no shift)
        let mut current_sum: Vec<GateNetId> = Vec::new();

        // Extend pp[0] to result_width with zeros (TIE_LOW)
        // Need j for both indexing and cell naming
        #[allow(clippy::needless_range_loop)]
        for j in 0..result_width as usize {
            if j < n {
                current_sum.push(partial_products[0][j]);
            } else {
                // Need a tie-low for zero extension
                let zero = self.alloc_net_id();
                self.netlist
                    .add_net(GateNet::new(zero, format!("{}.zero_{}", path, j)));
                self.stats.nets_created += 1;

                let mut tie_cell = Cell::new_comb(
                    CellId(0),
                    "TIE_LOW".to_string(),
                    self.library.name.clone(),
                    0.01,
                    format!("{}.tie_low_{}", path, j),
                    vec![],
                    vec![zero],
                );
                tie_cell.source_op = Some("Constant".to_string());
                self.add_cell(tie_cell);
                current_sum.push(zero);
            }
        }

        // Add each subsequent partial product (shifted by its index)
        // We need both the index i (for shift calculations) and partial_products[i]
        #[allow(clippy::needless_range_loop)]
        for i in 1..n {
            let mut next_sum: Vec<GateNetId> = Vec::new();
            let mut carry: Option<GateNetId> = None;

            for j in 0..result_width as usize {
                // Get the partial product bit (shifted by i)
                let pp_bit = if j >= i && j - i < n {
                    partial_products[i][j - i]
                } else {
                    // Zero for bits outside the partial product range
                    let zero = self.alloc_net_id();
                    self.netlist
                        .add_net(GateNet::new(zero, format!("{}.zero_{}_{}", path, i, j)));
                    self.stats.nets_created += 1;

                    let mut tie_cell = Cell::new_comb(
                        CellId(0),
                        "TIE_LOW".to_string(),
                        self.library.name.clone(),
                        0.01,
                        format!("{}.tie_low_{}_{}", path, i, j),
                        vec![],
                        vec![zero],
                    );
                    tie_cell.source_op = Some("Constant".to_string());
                    self.add_cell(tie_cell);
                    zero
                };

                let sum_bit = current_sum.get(j).copied().unwrap_or(current_sum[0]);

                // Create sum and carry output nets
                let new_sum = self.alloc_net_id();
                self.netlist
                    .add_net(GateNet::new(new_sum, format!("{}.sum_{}_{}", path, i, j)));
                self.stats.nets_created += 1;

                let new_carry = self.alloc_net_id();
                self.netlist.add_net(GateNet::new(
                    new_carry,
                    format!("{}.carry_{}_{}", path, i, j),
                ));
                self.stats.nets_created += 1;

                if let Some(cin) = carry {
                    // Full adder: sum_bit + pp_bit + carry_in
                    let mut cell = Cell::new_comb(
                        CellId(0),
                        fa_info.name.clone(),
                        self.library.name.clone(),
                        fa_info.fit,
                        format!("{}.fa_{}_{}", path, i, j),
                        vec![sum_bit, pp_bit, cin],
                        vec![new_sum, new_carry],
                    );
                    cell.source_op = Some("FullAdder".to_string());
                    fa_info.apply_to_cell(&mut cell);
                    self.add_cell(cell);
                } else {
                    // Half adder for first bit (no carry in)
                    let mut cell = Cell::new_comb(
                        CellId(0),
                        ha_info.name.clone(),
                        self.library.name.clone(),
                        ha_info.fit,
                        format!("{}.ha_{}_{}", path, i, j),
                        vec![sum_bit, pp_bit],
                        vec![new_sum, new_carry],
                    );
                    cell.source_op = Some("HalfAdder".to_string());
                    ha_info.apply_to_cell(&mut cell);
                    self.add_cell(cell);
                }

                next_sum.push(new_sum);
                carry = Some(new_carry);
            }

            current_sum = next_sum;
        }

        // Connect final sum to outputs
        for (j, &out) in outputs.iter().enumerate().take(result_width as usize) {
            if j < current_sum.len() {
                // Create a buffer to connect internal sum to output
                let buf_info = self.get_cell_info(&CellFunction::Buf);
                let mut cell = Cell::new_comb(
                    CellId(0),
                    buf_info.name.clone(),
                    self.library.name.clone(),
                    buf_info.fit,
                    format!("{}.out_buf_{}", path, j),
                    vec![current_sum[j]],
                    vec![out],
                );
                cell.source_op = Some("Buffer".to_string());
                self.add_cell(cell);
            }
        }

        self.stats.decomposed_mappings += 1;
    }

    /// Map a signed multiplier using sign-magnitude approach
    ///
    /// Algorithm:
    /// 1. Compute result sign = a_sign XOR b_sign
    /// 2. Conditionally negate a to get |a| (two's complement if negative)
    /// 3. Conditionally negate b to get |b|
    /// 4. Unsigned multiply: unsigned_product = |a| * |b|
    /// 5. Conditionally negate result if result_sign is set
    #[allow(clippy::needless_range_loop)]
    fn map_signed_multiplier(
        &mut self,
        width: u32,
        result_width: u32,
        inputs: &[Vec<GateNetId>],
        outputs: &[GateNetId],
        path: &str,
    ) {
        if inputs.len() < 2 {
            self.warnings.push(format!(
                "Signed multiplier needs 2 inputs, got {}",
                inputs.len()
            ));
            return;
        }

        let n = width as usize;
        let xor_info = self.get_cell_info(&CellFunction::Xor2);
        let inv_info = self.get_cell_info(&CellFunction::Inv);
        let ha_info = self.get_cell_info(&CellFunction::HalfAdder);
        let fa_info = self.get_cell_info(&CellFunction::FullAdder);
        let mux_info = self.get_cell_info(&CellFunction::Mux2);

        let a_raw = &inputs[0];
        let b_raw = &inputs[1];
        let tie_low = self.get_tie_low();

        // BUG FIX #247: For asymmetric input widths (e.g., 16×32), we must:
        // 1. Get the sign bit from the actual MSB of each input (not from bit n-1)
        // 2. Sign-extend shorter inputs to width n before processing
        let a_actual_width = a_raw.len();
        let b_actual_width = b_raw.len();

        // Get sign bits from actual MSB of each input
        let a_sign = a_raw.last().copied().unwrap_or(a_raw[0]);
        let b_sign = b_raw.last().copied().unwrap_or(b_raw[0]);

        // Sign-extend inputs to width n if needed
        let a: Vec<GateNetId> = if a_actual_width < n {
            let mut extended = a_raw.clone();
            // Fill remaining bits with the sign bit
            for _ in a_actual_width..n {
                extended.push(a_sign);
            }
            extended
        } else {
            a_raw.clone()
        };

        let b: Vec<GateNetId> = if b_actual_width < n {
            let mut extended = b_raw.clone();
            for _ in b_actual_width..n {
                extended.push(b_sign);
            }
            extended
        } else {
            b_raw.clone()
        };

        let result_sign = self.alloc_net_id();
        self.netlist
            .add_net(GateNet::new(result_sign, format!("{}.result_sign", path)));
        self.stats.nets_created += 1;

        let mut xor_cell = Cell::new_comb(
            CellId(0),
            xor_info.name.clone(),
            self.library.name.clone(),
            xor_info.fit,
            format!("{}.sign_xor", path),
            vec![a_sign, b_sign],
            vec![result_sign],
        );
        xor_cell.source_op = Some("SignXor".to_string());
        xor_info.apply_to_cell(&mut xor_cell);
        self.add_cell(xor_cell);

        // Step 2: Conditionally negate a to get |a|
        // Two's complement: -a = ~a + 1
        // |a| = a_sign ? (~a + 1) : a
        // We compute ~a first, then use conditional increment

        // First, compute ~a
        let mut a_inv: Vec<GateNetId> = Vec::with_capacity(n);
        for i in 0..n {
            let a_bit = a.get(i).copied().unwrap_or(tie_low);
            let inv_net = self.alloc_net_id();
            self.netlist
                .add_net(GateNet::new(inv_net, format!("{}.a_inv_{}", path, i)));
            self.stats.nets_created += 1;

            let mut inv_cell = Cell::new_comb(
                CellId(0),
                inv_info.name.clone(),
                self.library.name.clone(),
                inv_info.fit,
                format!("{}.a_inv_{}", path, i),
                vec![a_bit],
                vec![inv_net],
            );
            inv_cell.source_op = Some("Invert".to_string());
            inv_info.apply_to_cell(&mut inv_cell);
            self.add_cell(inv_cell);
            a_inv.push(inv_net);
        }

        // Compute ~a + 1 using half/full adders with carry=1
        let mut a_neg: Vec<GateNetId> = Vec::with_capacity(n);
        let mut carry = self.get_tie_high(); // Initial carry = 1 for +1

        for i in 0..n {
            let sum_net = self.alloc_net_id();
            self.netlist
                .add_net(GateNet::new(sum_net, format!("{}.a_neg_{}", path, i)));
            self.stats.nets_created += 1;

            let carry_out = self.alloc_net_id();
            self.netlist.add_net(GateNet::new(
                carry_out,
                format!("{}.a_neg_carry_{}", path, i),
            ));
            self.stats.nets_created += 1;

            // Half adder: a_inv[i] + carry
            let mut ha_cell = Cell::new_comb(
                CellId(0),
                ha_info.name.clone(),
                self.library.name.clone(),
                ha_info.fit,
                format!("{}.a_neg_ha_{}", path, i),
                vec![a_inv[i], carry],
                vec![sum_net, carry_out],
            );
            ha_cell.source_op = Some("TwosComplement".to_string());
            ha_info.apply_to_cell(&mut ha_cell);
            self.add_cell(ha_cell);

            a_neg.push(sum_net);
            carry = carry_out;
        }

        // Select |a| = a_sign ? a_neg : a
        let mut a_mag: Vec<GateNetId> = Vec::with_capacity(n);
        for i in 0..n {
            let a_bit = a.get(i).copied().unwrap_or(tie_low);
            let mux_out = self.alloc_net_id();
            self.netlist
                .add_net(GateNet::new(mux_out, format!("{}.a_mag_{}", path, i)));
            self.stats.nets_created += 1;

            // MUX: sel=a_sign, d0=a (when a_sign=0), d1=a_neg (when a_sign=1)
            let mut mux_cell = Cell::new_comb(
                CellId(0),
                mux_info.name.clone(),
                self.library.name.clone(),
                mux_info.fit,
                format!("{}.a_mag_mux_{}", path, i),
                vec![a_sign, a_bit, a_neg[i]],
                vec![mux_out],
            );
            mux_cell.source_op = Some("Magnitude".to_string());
            mux_info.apply_to_cell(&mut mux_cell);
            self.add_cell(mux_cell);
            a_mag.push(mux_out);
        }

        // Step 3: Same for b
        let mut b_inv: Vec<GateNetId> = Vec::with_capacity(n);
        for i in 0..n {
            let b_bit = b.get(i).copied().unwrap_or(tie_low);
            let inv_net = self.alloc_net_id();
            self.netlist
                .add_net(GateNet::new(inv_net, format!("{}.b_inv_{}", path, i)));
            self.stats.nets_created += 1;

            let mut inv_cell = Cell::new_comb(
                CellId(0),
                inv_info.name.clone(),
                self.library.name.clone(),
                inv_info.fit,
                format!("{}.b_inv_{}", path, i),
                vec![b_bit],
                vec![inv_net],
            );
            inv_cell.source_op = Some("Invert".to_string());
            inv_info.apply_to_cell(&mut inv_cell);
            self.add_cell(inv_cell);
            b_inv.push(inv_net);
        }

        let mut b_neg: Vec<GateNetId> = Vec::with_capacity(n);
        let mut carry = self.get_tie_high();

        for i in 0..n {
            let sum_net = self.alloc_net_id();
            self.netlist
                .add_net(GateNet::new(sum_net, format!("{}.b_neg_{}", path, i)));
            self.stats.nets_created += 1;

            let carry_out = self.alloc_net_id();
            self.netlist.add_net(GateNet::new(
                carry_out,
                format!("{}.b_neg_carry_{}", path, i),
            ));
            self.stats.nets_created += 1;

            let mut ha_cell = Cell::new_comb(
                CellId(0),
                ha_info.name.clone(),
                self.library.name.clone(),
                ha_info.fit,
                format!("{}.b_neg_ha_{}", path, i),
                vec![b_inv[i], carry],
                vec![sum_net, carry_out],
            );
            ha_cell.source_op = Some("TwosComplement".to_string());
            ha_info.apply_to_cell(&mut ha_cell);
            self.add_cell(ha_cell);

            b_neg.push(sum_net);
            carry = carry_out;
        }

        let mut b_mag: Vec<GateNetId> = Vec::with_capacity(n);
        for i in 0..n {
            let b_bit = b.get(i).copied().unwrap_or(tie_low);
            let mux_out = self.alloc_net_id();
            self.netlist
                .add_net(GateNet::new(mux_out, format!("{}.b_mag_{}", path, i)));
            self.stats.nets_created += 1;

            let mut mux_cell = Cell::new_comb(
                CellId(0),
                mux_info.name.clone(),
                self.library.name.clone(),
                mux_info.fit,
                format!("{}.b_mag_mux_{}", path, i),
                vec![b_sign, b_bit, b_neg[i]],
                vec![mux_out],
            );
            mux_cell.source_op = Some("Magnitude".to_string());
            mux_info.apply_to_cell(&mut mux_cell);
            self.add_cell(mux_cell);
            b_mag.push(mux_out);
        }

        // Step 4: Unsigned multiply |a| * |b|
        let mut unsigned_product: Vec<GateNetId> = Vec::with_capacity(result_width as usize);
        for i in 0..result_width as usize {
            let net = self.alloc_net_id();
            self.netlist
                .add_net(GateNet::new(net, format!("{}.unsigned_prod_{}", path, i)));
            self.stats.nets_created += 1;
            unsigned_product.push(net);
        }

        self.map_multiplier(
            width,
            result_width,
            false, // Unsigned multiplication of magnitudes
            &[a_mag, b_mag],
            &unsigned_product,
            &format!("{}.unsigned_mul", path),
        );

        // Step 5: Conditionally negate result
        // If result_sign = 1, output = ~unsigned_product + 1
        // Else, output = unsigned_product

        // Compute ~unsigned_product
        let mut prod_inv: Vec<GateNetId> = Vec::with_capacity(result_width as usize);
        for i in 0..result_width as usize {
            let inv_net = self.alloc_net_id();
            self.netlist
                .add_net(GateNet::new(inv_net, format!("{}.prod_inv_{}", path, i)));
            self.stats.nets_created += 1;

            let mut inv_cell = Cell::new_comb(
                CellId(0),
                inv_info.name.clone(),
                self.library.name.clone(),
                inv_info.fit,
                format!("{}.prod_inv_{}", path, i),
                vec![unsigned_product[i]],
                vec![inv_net],
            );
            inv_cell.source_op = Some("Invert".to_string());
            inv_info.apply_to_cell(&mut inv_cell);
            self.add_cell(inv_cell);
            prod_inv.push(inv_net);
        }

        // Compute ~unsigned_product + 1
        let mut prod_neg: Vec<GateNetId> = Vec::with_capacity(result_width as usize);
        let mut carry = self.get_tie_high();

        for i in 0..result_width as usize {
            let sum_net = self.alloc_net_id();
            self.netlist
                .add_net(GateNet::new(sum_net, format!("{}.prod_neg_{}", path, i)));
            self.stats.nets_created += 1;

            let carry_out = self.alloc_net_id();
            self.netlist.add_net(GateNet::new(
                carry_out,
                format!("{}.prod_neg_carry_{}", path, i),
            ));
            self.stats.nets_created += 1;

            let mut ha_cell = Cell::new_comb(
                CellId(0),
                ha_info.name.clone(),
                self.library.name.clone(),
                ha_info.fit,
                format!("{}.prod_neg_ha_{}", path, i),
                vec![prod_inv[i], carry],
                vec![sum_net, carry_out],
            );
            ha_cell.source_op = Some("TwosComplement".to_string());
            ha_info.apply_to_cell(&mut ha_cell);
            self.add_cell(ha_cell);

            prod_neg.push(sum_net);
            carry = carry_out;
        }

        // Final output: result_sign ? prod_neg : unsigned_product
        for i in 0..result_width as usize {
            if i >= outputs.len() {
                break;
            }
            let out = outputs[i];

            let mut mux_cell = Cell::new_comb(
                CellId(0),
                mux_info.name.clone(),
                self.library.name.clone(),
                mux_info.fit,
                format!("{}.final_mux_{}", path, i),
                vec![result_sign, unsigned_product[i], prod_neg[i]],
                vec![out],
            );
            mux_cell.source_op = Some("SignedResult".to_string());
            mux_info.apply_to_cell(&mut mux_cell);
            self.add_cell(mux_cell);
        }

        self.stats.decomposed_mappings += 1;
    }

    /// Map a 2:1 multiplexer
    fn map_mux2(
        &mut self,
        width: u32,
        inputs: &[Vec<GateNetId>],
        outputs: &[GateNetId],
        path: &str,
    ) {
        if inputs.len() < 3 {
            self.warnings.push(format!(
                "Mux2 needs 3 inputs (sel, d0, d1), got {}",
                inputs.len()
            ));
            return;
        }

        let mux_info = self.get_cell_info(&CellFunction::Mux2);
        let tie_low = self.get_tie_low();

        let sel = inputs[0].first().copied().unwrap_or(GateNetId(0));

        for bit in 0..width as usize {
            let d0 = inputs[1].get(bit).copied().unwrap_or(tie_low);
            let d1 = inputs[2].get(bit).copied().unwrap_or(tie_low);
            let y = outputs.get(bit).copied().unwrap_or(outputs[0]);

            let mut cell = Cell::new_comb(
                CellId(0),
                mux_info.name.clone(),
                self.library.name.clone(),
                mux_info.fit,
                format!("{}.bit{}", path, bit),
                vec![sel, d0, d1],
                vec![y],
            );
            cell.source_op = Some("Mux2".to_string());
            mux_info.apply_to_cell(&mut cell);
            self.add_cell(cell);
        }

        self.stats.direct_mappings += 1;
    }

    /// Map a barrel shifter (left or right shift)
    ///
    /// Uses cascaded MUX layers for logarithmic shift implementation.
    /// For an N-bit data with log2(N) shift bits:
    /// - Layer 0: shift by 0 or 1 (controlled by shift_amt[0])
    /// - Layer 1: shift by 0 or 2 (controlled by shift_amt[1])
    /// - Layer 2: shift by 0 or 4 (controlled by shift_amt[2])
    /// - etc.
    fn map_shift(
        &mut self,
        width: u32,
        inputs: &[Vec<GateNetId>],
        outputs: &[GateNetId],
        path: &str,
        left: bool,
        sign_extend: bool,
    ) {
        if inputs.len() < 2 {
            self.warnings.push(format!(
                "Shift needs 2 inputs (data, amount), got {}",
                inputs.len()
            ));
            return;
        }

        let data = &inputs[0];
        let shift_amt = &inputs[1];

        if data.is_empty() || shift_amt.is_empty() {
            self.warnings.push("Shift: empty input vectors".to_string());
            return;
        }

        let mux_info = self.get_cell_info(&CellFunction::Mux2);

        // Calculate number of shift stages (log2 of width)
        let shift_bits = (32 - (width - 1).leading_zeros()) as usize;
        let shift_bits = shift_bits.min(shift_amt.len()); // Limit to available shift bits

        // Use tie-low net for filling in shifted bits (properly driven by TIE_LOW cell)
        let const_0 = self.get_tie_low();

        // For arithmetic right shift, use sign bit (MSB) as fill value instead of 0
        let fill_value = if sign_extend && !left {
            // Get the sign bit (MSB) of the input data
            data.get(width as usize - 1).copied().unwrap_or(data[0])
        } else {
            const_0
        };

        // Current stage values (start with input data)
        let mut current: Vec<GateNetId> = (0..width as usize)
            .map(|i| data.get(i).copied().unwrap_or(data[0]))
            .collect();

        // Build barrel shifter with cascaded MUX layers
        for stage in 0..shift_bits {
            let shift_amount = 1usize << stage;
            let sel = shift_amt.get(stage).copied().unwrap_or(shift_amt[0]);

            let mut next_stage: Vec<GateNetId> = Vec::with_capacity(width as usize);

            for bit in 0..width as usize {
                // Allocate output net for this MUX
                let mux_out = if stage == shift_bits - 1 {
                    // Last stage - use final output nets
                    outputs.get(bit).copied().unwrap_or(outputs[0])
                } else {
                    // Intermediate stage - create new net
                    let net = self.alloc_net_id();
                    self.netlist.add_net(GateNet::new(
                        net,
                        format!("{}.stage{}_bit{}", path, stage, bit),
                    ));
                    self.stats.nets_created += 1;
                    net
                };

                // Calculate source bit for shifted value
                let (d0, d1) = if left {
                    // Left shift: when sel=1, bit[i] gets bit[i-shift_amount] (or 0)
                    let unshifted = current[bit];
                    let shifted = if bit >= shift_amount {
                        current[bit - shift_amount]
                    } else {
                        const_0
                    };
                    (unshifted, shifted)
                } else {
                    // Right shift: when sel=1, bit[i] gets bit[i+shift_amount] (or fill_value)
                    // For logical shift (Shr): fill_value = 0
                    // For arithmetic shift (Sar): fill_value = sign bit (MSB)
                    let unshifted = current[bit];
                    let shifted = if bit + shift_amount < width as usize {
                        current[bit + shift_amount]
                    } else {
                        fill_value
                    };
                    (unshifted, shifted)
                };

                // Create MUX: sel=0 -> d0 (unshifted), sel=1 -> d1 (shifted)
                let mut cell = Cell::new_comb(
                    CellId(0),
                    mux_info.name.clone(),
                    self.library.name.clone(),
                    mux_info.fit,
                    format!("{}.mux_s{}_b{}", path, stage, bit),
                    vec![sel, d0, d1],
                    vec![mux_out],
                );
                cell.source_op = Some(if left { "Shl" } else { "Shr" }.to_string());
                mux_info.apply_to_cell(&mut cell);
                self.add_cell(cell);

                next_stage.push(mux_out);
            }

            current = next_stage;
        }

        // If no shift stages were needed (width=1), just connect input to output
        if shift_bits == 0 {
            for bit in 0..width as usize {
                let src = current.get(bit).copied().unwrap_or(current[0]);
                let dst = outputs.get(bit).copied().unwrap_or(outputs[0]);

                // Use buffer to connect
                let buf_info = self.get_cell_info(&CellFunction::Buf);
                let mut cell = Cell::new_comb(
                    CellId(0),
                    buf_info.name.clone(),
                    self.library.name.clone(),
                    buf_info.fit,
                    format!("{}.buf_bit{}", path, bit),
                    vec![src],
                    vec![dst],
                );
                cell.source_op = Some("Buffer".to_string());
                buf_info.apply_to_cell(&mut cell);
                self.add_cell(cell);
            }
        }

        self.stats.decomposed_mappings += 1;
    }

    /// Map range select (bit extraction): output = input[high:low]
    ///
    /// This is pure wiring - we just connect the specified bits from input to output.
    /// Uses buffers to make the connections explicit in the netlist.
    fn map_range_select(
        &mut self,
        high: u32,
        low: u32,
        inputs: &[Vec<GateNetId>],
        outputs: &[GateNetId],
        path: &str,
    ) {
        if inputs.is_empty() || inputs[0].is_empty() {
            self.warnings.push("RangeSelect: empty input".to_string());
            return;
        }

        let data = &inputs[0];
        let out_width = (high - low + 1) as usize;

        // Get buffer cell info for explicit wiring
        let buf_info = self.get_cell_info(&CellFunction::Buf);

        for bit in 0..out_width {
            let src_bit = low as usize + bit;
            let src = data.get(src_bit).copied().unwrap_or_else(|| {
                // If the source bit doesn't exist, warn but continue
                self.warnings.push(format!(
                    "RangeSelect: bit {} out of range for input width {}",
                    src_bit,
                    data.len()
                ));
                data.first().copied().unwrap_or(GateNetId(0))
            });
            let dst = outputs.get(bit).copied().unwrap_or(outputs[0]);

            // Use buffer for explicit connection
            let mut cell = Cell::new_comb(
                CellId(0),
                buf_info.name.clone(),
                self.library.name.clone(),
                buf_info.fit,
                format!("{}.sel_bit{}", path, bit),
                vec![src],
                vec![dst],
            );
            cell.source_op = Some("RangeSelect".to_string());
            buf_info.apply_to_cell(&mut cell);
            self.add_cell(cell);
        }

        self.stats.direct_mappings += 1;
    }

    /// Map concat: concatenate multiple inputs into output
    ///
    /// The widths vector specifies the width of each input.
    /// In Verilog/SKALP, {a, b} means a is in MSB position and b is in LSB position.
    /// So for {a[2:0], b[1:0]} producing output[4:0]:
    ///   - b[1:0] (last input) goes to output[1:0] (LSB)
    ///   - a[2:0] (first input) goes to output[4:2] (MSB)
    fn map_concat(
        &mut self,
        widths: &[u32],
        inputs: &[Vec<GateNetId>],
        outputs: &[GateNetId],
        path: &str,
    ) {
        if inputs.is_empty() {
            self.warnings.push("Concat: no inputs".to_string());
            return;
        }

        // Get buffer cell info for explicit wiring
        let buf_info = self.get_cell_info(&CellFunction::Buf);

        // Process inputs in REVERSE order to match Verilog convention:
        // Last input goes to low bits, first input goes to high bits
        let mut out_bit = 0;
        for (i, (&width, input)) in widths.iter().zip(inputs.iter()).enumerate().rev() {
            for bit in 0..width as usize {
                let src = input.get(bit).copied().unwrap_or_else(|| {
                    self.warnings.push(format!(
                        "Concat: input {} bit {} out of range (width {})",
                        i,
                        bit,
                        input.len()
                    ));
                    input.first().copied().unwrap_or(GateNetId(0))
                });
                let dst = outputs.get(out_bit).copied().unwrap_or_else(|| {
                    self.warnings.push(format!(
                        "Concat: output bit {} out of range (width {})",
                        out_bit,
                        outputs.len()
                    ));
                    outputs.first().copied().unwrap_or(GateNetId(0))
                });

                // Use buffer for explicit connection
                let mut cell = Cell::new_comb(
                    CellId(0),
                    buf_info.name.clone(),
                    self.library.name.clone(),
                    buf_info.fit,
                    format!("{}.concat_bit{}", path, out_bit),
                    vec![src],
                    vec![dst],
                );
                cell.source_op = Some("Concat".to_string());
                buf_info.apply_to_cell(&mut cell);
                self.add_cell(cell);

                out_bit += 1;
            }
        }

        self.stats.direct_mappings += 1;
    }

    /// Map zero extend: copy lower bits, pad upper bits with zero
    fn map_zero_extend(
        &mut self,
        from: u32,
        to: u32,
        inputs: &[Vec<GateNetId>],
        outputs: &[GateNetId],
        path: &str,
    ) {
        if inputs.is_empty() {
            self.warnings.push("ZeroExtend: no inputs".to_string());
            return;
        }

        let input = &inputs[0];
        let buf_info = self.get_cell_info(&CellFunction::Buf);

        // Copy lower bits using buffers
        for bit in 0..from as usize {
            let src = input.get(bit).copied().unwrap_or_else(|| {
                self.warnings.push(format!(
                    "ZeroExtend: input bit {} out of range (from={})",
                    bit, from
                ));
                input.first().copied().unwrap_or(GateNetId(0))
            });
            let dst = outputs.get(bit).copied().unwrap_or_else(|| {
                self.warnings.push(format!(
                    "ZeroExtend: output bit {} out of range (to={})",
                    bit, to
                ));
                outputs.first().copied().unwrap_or(GateNetId(0))
            });

            let mut cell = Cell::new_comb(
                CellId(0),
                buf_info.name.clone(),
                self.library.name.clone(),
                buf_info.fit,
                format!("{}.zext_bit{}", path, bit),
                vec![src],
                vec![dst],
            );
            cell.source_op = Some("ZeroExtend".to_string());
            buf_info.apply_to_cell(&mut cell);
            self.add_cell(cell);
        }

        // Pad upper bits with TIE_LOW
        for bit in from as usize..to as usize {
            let dst = outputs.get(bit).copied().unwrap_or_else(|| {
                self.warnings.push(format!(
                    "ZeroExtend: output bit {} out of range (to={})",
                    bit, to
                ));
                outputs.first().copied().unwrap_or(GateNetId(0))
            });

            let mut cell = Cell::new_comb(
                CellId(0),
                "TIE_LOW".to_string(),
                self.library.name.clone(),
                0.01,
                format!("{}.zext_zero{}", path, bit),
                vec![],
                vec![dst],
            );
            cell.source_op = Some("ZeroExtend".to_string());
            self.add_cell(cell);
        }

        self.stats.direct_mappings += 1;
    }

    /// Map sign extend: copy lower bits, extend sign bit to upper bits
    fn map_sign_extend(
        &mut self,
        from: u32,
        to: u32,
        inputs: &[Vec<GateNetId>],
        outputs: &[GateNetId],
        path: &str,
    ) {
        if inputs.is_empty() || from == 0 {
            self.warnings
                .push("SignExtend: no inputs or from=0".to_string());
            return;
        }

        let input = &inputs[0];
        let buf_info = self.get_cell_info(&CellFunction::Buf);

        // Get the sign bit (highest bit of input)
        let sign_bit = input.get((from - 1) as usize).copied().unwrap_or_else(|| {
            self.warnings.push(format!(
                "SignExtend: sign bit {} out of range (from={})",
                from - 1,
                from
            ));
            input.first().copied().unwrap_or(GateNetId(0))
        });

        // Copy lower bits using buffers
        for bit in 0..from as usize {
            let src = input.get(bit).copied().unwrap_or_else(|| {
                self.warnings.push(format!(
                    "SignExtend: input bit {} out of range (from={})",
                    bit, from
                ));
                input.first().copied().unwrap_or(GateNetId(0))
            });
            let dst = outputs.get(bit).copied().unwrap_or_else(|| {
                self.warnings.push(format!(
                    "SignExtend: output bit {} out of range (to={})",
                    bit, to
                ));
                outputs.first().copied().unwrap_or(GateNetId(0))
            });

            let mut cell = Cell::new_comb(
                CellId(0),
                buf_info.name.clone(),
                self.library.name.clone(),
                buf_info.fit,
                format!("{}.sext_bit{}", path, bit),
                vec![src],
                vec![dst],
            );
            cell.source_op = Some("SignExtend".to_string());
            buf_info.apply_to_cell(&mut cell);
            self.add_cell(cell);
        }

        // Extend sign bit to upper bits using buffers
        for bit in from as usize..to as usize {
            let dst = outputs.get(bit).copied().unwrap_or_else(|| {
                self.warnings.push(format!(
                    "SignExtend: output bit {} out of range (to={})",
                    bit, to
                ));
                outputs.first().copied().unwrap_or(GateNetId(0))
            });

            let mut cell = Cell::new_comb(
                CellId(0),
                buf_info.name.clone(),
                self.library.name.clone(),
                buf_info.fit,
                format!("{}.sext_sign{}", path, bit),
                vec![sign_bit],
                vec![dst],
            );
            cell.source_op = Some("SignExtend".to_string());
            buf_info.apply_to_cell(&mut cell);
            self.add_cell(cell);
        }

        self.stats.direct_mappings += 1;
    }

    /// Map a Muller C-element (TH22) using standard gates
    ///
    /// The C-element is the fundamental building block of NCL async circuits.
    /// It implements: Q = (a AND b) OR (Q AND (a OR b))
    ///
    /// This creates a state-holding element where:
    /// - Output goes HIGH when both inputs are HIGH
    /// - Output goes LOW when both inputs are LOW
    /// - Output HOLDS previous value otherwise (hysteresis)
    ///
    /// Implementation uses 4 standard gates:
    /// ```text
    ///     a ──┬──[AND2]──┐
    ///         │          │
    ///     b ──┘          ├──[OR2]─── Q
    ///                    │           │
    ///     a ──[OR2]──[AND2]──────────│
    ///         │                      │
    ///     b ──┘                      │
    ///         ┌──────────────────────┘ (feedback)
    ///         v
    /// ```
    fn map_c_element(
        &mut self,
        width: u32,
        inputs: &[Vec<GateNetId>],
        outputs: &[GateNetId],
        path: &str,
    ) {
        if inputs.len() < 2 {
            self.warnings.push("C-element needs 2 inputs".to_string());
            return;
        }

        let and_info = self.get_cell_info(&CellFunction::And2);
        let or_info = self.get_cell_info(&CellFunction::Or2);

        // Use a unique counter to ensure C-element internal nets have unique names
        // This is needed because multiple NCL operations can have the same path
        let c_elem_id = self.c_elem_counter;
        self.c_elem_counter += 1;
        let tie_low = self.get_tie_low();

        for bit in 0..width as usize {
            let a = inputs[0].get(bit).copied().unwrap_or(tie_low);
            let b = inputs[1].get(bit).copied().unwrap_or(tie_low);
            let q = outputs.get(bit).copied().unwrap_or(GateNetId(0));

            // Create intermediate nets with unique naming: path.c_elem_{id}_{bit}
            let ab_and_net = self.alloc_net_id();
            self.netlist.add_net(GateNet::new(
                ab_and_net,
                format!("{}.c_elem{}_{}.ab_and", path, c_elem_id, bit),
            ));

            let ab_or_net = self.alloc_net_id();
            self.netlist.add_net(GateNet::new(
                ab_or_net,
                format!("{}.c_elem{}_{}.ab_or", path, c_elem_id, bit),
            ));

            let q_and_or_net = self.alloc_net_id();
            self.netlist.add_net(GateNet::new(
                q_and_or_net,
                format!("{}.c_elem{}_{}.q_and_or", path, c_elem_id, bit),
            ));

            // Gate 1: ab_and = a AND b
            let mut cell1 = Cell::new_comb(
                CellId(0),
                and_info.name.clone(),
                self.library.name.clone(),
                and_info.fit,
                format!("{}.c_elem{}_{}.and1", path, c_elem_id, bit),
                vec![a, b],
                vec![ab_and_net],
            );
            cell1.source_op = Some("C-element_AND_AB".to_string());
            self.add_cell(cell1);

            // Gate 2: ab_or = a OR b
            let mut cell2 = Cell::new_comb(
                CellId(0),
                or_info.name.clone(),
                self.library.name.clone(),
                or_info.fit,
                format!("{}.c_elem{}_{}.or1", path, c_elem_id, bit),
                vec![a, b],
                vec![ab_or_net],
            );
            cell2.source_op = Some("C-element_OR_AB".to_string());
            self.add_cell(cell2);

            // Gate 3: q_and_or = Q AND (a OR b) - feedback from output
            // Note: Q is the output net, creating a combinational feedback loop
            let mut cell3 = Cell::new_comb(
                CellId(0),
                and_info.name.clone(),
                self.library.name.clone(),
                and_info.fit,
                format!("{}.c_elem{}_{}.and2", path, c_elem_id, bit),
                vec![q, ab_or_net],
                vec![q_and_or_net],
            );
            cell3.source_op = Some("C-element_AND_Q_OR".to_string());
            self.add_cell(cell3);

            // Gate 4: Q = (a AND b) OR (Q AND (a OR b))
            let mut cell4 = Cell::new_comb(
                CellId(0),
                or_info.name.clone(),
                self.library.name.clone(),
                or_info.fit,
                format!("{}.c_elem{}_{}.or2", path, c_elem_id, bit),
                vec![ab_and_net, q_and_or_net],
                vec![q],
            );
            cell4.source_op = Some("C-element_OUTPUT".to_string());
            self.add_cell(cell4);

            self.stats.cells_created += 4;
        }

        self.stats.decomposed_mappings += 1;
    }

    /// Map bit select (single bit extraction): output = input[index]
    ///
    /// Index is the second input signal (dynamic bit select).
    /// For simplicity, we implement this as a MUX tree.
    fn map_bit_select(&mut self, inputs: &[Vec<GateNetId>], outputs: &[GateNetId], path: &str) {
        if inputs.len() < 2 {
            self.warnings
                .push("BitSelect needs 2 inputs (data, index)".to_string());
            return;
        }

        let data = &inputs[0];
        let index = &inputs[1];
        let output = outputs.first().copied().unwrap_or(GateNetId(0));

        if data.is_empty() || index.is_empty() {
            self.warnings
                .push("BitSelect: empty input vectors".to_string());
            return;
        }

        // Build a MUX tree to select one bit based on index
        // For small inputs, we can use a cascade of MUXes
        let mux_info = self.get_cell_info(&CellFunction::Mux2);

        // Number of MUX stages = ceil(log2(data.len()))
        let num_bits = data.len();
        let index_bits = (32 - (num_bits.saturating_sub(1) as u32).leading_zeros()) as usize;
        let index_bits = index_bits.max(1).min(index.len());

        // Start with all data bits padded to power of 2
        let padded_size = 1usize << index_bits;
        let mut current: Vec<GateNetId> = (0..padded_size)
            .map(|i| data.get(i).copied().unwrap_or(data[0]))
            .collect();

        // Build MUX tree from leaves to root
        for stage in 0..index_bits {
            let sel = index.get(stage).copied().unwrap_or(index[0]);
            let stage_size = current.len() / 2;
            let mut next_stage = Vec::with_capacity(stage_size);

            for i in 0..stage_size {
                let d0 = current[i * 2];
                let d1 = current[i * 2 + 1];

                let mux_out = if stage == index_bits - 1 && i == 0 {
                    output
                } else {
                    let net = self.alloc_net_id();
                    self.netlist.add_net(GateNet::new(
                        net,
                        format!("{}.bitsel_s{}_i{}", path, stage, i),
                    ));
                    self.stats.nets_created += 1;
                    net
                };

                let mut cell = Cell::new_comb(
                    CellId(0),
                    mux_info.name.clone(),
                    self.library.name.clone(),
                    mux_info.fit,
                    format!("{}.bitsel_mux_s{}_i{}", path, stage, i),
                    vec![sel, d0, d1],
                    vec![mux_out],
                );
                cell.source_op = Some("BitSelect".to_string());
                mux_info.apply_to_cell(&mut cell);
                self.add_cell(cell);

                next_stage.push(mux_out);
            }

            current = next_stage;
        }

        self.stats.decomposed_mappings += 1;
    }

    /// Map equality comparator
    fn map_equality(
        &mut self,
        width: u32,
        inputs: &[Vec<GateNetId>],
        outputs: &[GateNetId],
        path: &str,
        invert: bool,
    ) {
        if inputs.len() < 2 {
            self.warnings
                .push(format!("Equality needs 2 inputs, got {}", inputs.len()));
            return;
        }

        let xnor_info = self.get_cell_info(&CellFunction::Xnor2);
        let and_info = self.get_cell_info(&CellFunction::And2);
        let tie_low = self.get_tie_low();

        // XNOR each bit pair
        let mut xnor_outs: Vec<GateNetId> = Vec::new();
        for bit in 0..width as usize {
            let a = inputs[0].get(bit).copied().unwrap_or(tie_low);
            let b = inputs[1].get(bit).copied().unwrap_or(tie_low);

            // Use add_net's return value - it assigns the correct ID
            let xnor_out = self
                .netlist
                .add_net(GateNet::new(GateNetId(0), format!("{}.xnor{}", path, bit)));
            self.stats.nets_created += 1;

            let mut cell = Cell::new_comb(
                CellId(0),
                xnor_info.name.clone(),
                self.library.name.clone(),
                xnor_info.fit,
                format!("{}.xnor{}", path, bit),
                vec![a, b],
                vec![xnor_out],
            );
            cell.source_op = Some("XNOR2".to_string());
            xnor_info.apply_to_cell(&mut cell);
            self.add_cell(cell);
            xnor_outs.push(xnor_out);
        }

        // AND tree to combine
        let eq_out = self.reduce_tree(&xnor_outs, &and_info, path, "eq_and");

        // Optionally invert for NotEqual
        if invert {
            let inv_info = self.get_cell_info(&CellFunction::Inv);

            let y = outputs.first().copied().unwrap_or(GateNetId(0));
            let mut cell = Cell::new_comb(
                CellId(0),
                inv_info.name.clone(),
                self.library.name.clone(),
                inv_info.fit,
                format!("{}.ne_inv", path),
                vec![eq_out],
                vec![y],
            );
            cell.source_op = Some("Inverter".to_string());
            inv_info.apply_to_cell(&mut cell);
            self.add_cell(cell);
        } else {
            // Connect eq_out to output (add buffer if needed)
            let y = outputs.first().copied().unwrap_or(GateNetId(0));
            if eq_out != y {
                let buf_info = self.get_cell_info(&CellFunction::Buf);

                let mut cell = Cell::new_comb(
                    CellId(0),
                    buf_info.name.clone(),
                    self.library.name.clone(),
                    buf_info.fit,
                    format!("{}.eq_buf", path),
                    vec![eq_out],
                    vec![y],
                );
                cell.source_op = Some("Buffer".to_string());
                buf_info.apply_to_cell(&mut cell);
                self.add_cell(cell);
            }
        }

        self.stats.decomposed_mappings += 1;
    }

    /// Map less-than comparator
    /// a < b is implemented as: starting from MSB, first bit where a[i] != b[i] determines result
    /// If a[i]=0 and b[i]=1, then a < b (result = 1)
    /// If a[i]=1 and b[i]=0, then a >= b (result = 0)
    /// Otherwise check next bit
    fn map_less_than(
        &mut self,
        width: u32,
        inputs: &[Vec<GateNetId>],
        outputs: &[GateNetId],
        path: &str,
    ) {
        if inputs.len() < 2 {
            self.warnings
                .push(format!("LessThan needs 2 inputs, got {}", inputs.len()));
            return;
        }

        let y = outputs.first().copied().unwrap_or(GateNetId(0));
        let a = &inputs[0];
        let b = &inputs[1];

        // Build a bit-serial comparator from MSB to LSB
        // For each bit position i:
        //   lt_i = (!a[i] & b[i]) | (a[i] == b[i]) & lt_{i-1}
        // Where lt at bit -1 (after LSB) is 0 (not less if all bits equal)

        // Get cell info
        let and_info = self.get_cell_info(&CellFunction::And2);
        let or_info = self.get_cell_info(&CellFunction::Or2);
        let inv_info = self.get_cell_info(&CellFunction::Inv);
        let xnor_info = self.get_cell_info(&CellFunction::Xnor2);
        let tie_low = self.get_tie_low();

        // Start from LSB and work up
        // prev_lt carries whether a[i-1:0] < b[i-1:0] based on lower bits
        // Formula: lt_i = lt_bit_i | (eq_i & prev_lt)
        // where lt_bit_i = ~a[i] & b[i] (a < b at this bit position)
        let mut prev_lt = None; // None means 0 (not less than so far)

        for i in 0..width as usize {
            let a_bit = a.get(i).copied().unwrap_or(tie_low);
            let b_bit = b.get(i).copied().unwrap_or(tie_low);

            // Compute a[i] == b[i] using XNOR
            let eq_net = self
                .netlist
                .add_net(GateNet::new(GateNetId(0), format!("{}.eq_{}", path, i)));
            let eq_cell = Cell::new_comb(
                CellId(0),
                xnor_info.name.clone(),
                self.library.name.clone(),
                xnor_info.fit,
                format!("{}.eq_{}", path, i),
                vec![a_bit, b_bit],
                vec![eq_net],
            );
            self.add_cell(eq_cell);

            // Compute !a[i] & b[i] (a[i] < b[i] at this bit)
            let not_a_net = self
                .netlist
                .add_net(GateNet::new(GateNetId(0), format!("{}.not_a_{}", path, i)));
            let inv_cell = Cell::new_comb(
                CellId(0),
                inv_info.name.clone(),
                self.library.name.clone(),
                inv_info.fit,
                format!("{}.inv_a_{}", path, i),
                vec![a_bit],
                vec![not_a_net],
            );
            self.add_cell(inv_cell);

            let lt_bit_net = self
                .netlist
                .add_net(GateNet::new(GateNetId(0), format!("{}.lt_bit_{}", path, i)));
            let and_cell = Cell::new_comb(
                CellId(0),
                and_info.name.clone(),
                self.library.name.clone(),
                and_info.fit,
                format!("{}.lt_bit_{}", path, i),
                vec![not_a_net, b_bit],
                vec![lt_bit_net],
            );
            self.add_cell(and_cell);

            // Combine with previous result:
            // lt_i = lt_bit_i | (eq_i & prev_lt)
            // At LSB (i==0), prev_lt is 0 so lt_0 = lt_bit_0
            let lt_net = if i == 0 {
                // LSB: lt = lt_bit (no previous result)
                lt_bit_net
            } else if let Some(prev_lt_net) = prev_lt {
                // Combine: lt = lt_bit | (eq & prev_lt)
                let eq_and_prev = self.netlist.add_net(GateNet::new(
                    GateNetId(0),
                    format!("{}.eq_and_prev_{}", path, i),
                ));
                let and_cell = Cell::new_comb(
                    CellId(0),
                    and_info.name.clone(),
                    self.library.name.clone(),
                    and_info.fit,
                    format!("{}.eq_and_prev_{}", path, i),
                    vec![eq_net, prev_lt_net],
                    vec![eq_and_prev],
                );
                self.add_cell(and_cell);

                let combined = if i == width as usize - 1 {
                    y // Use output net for final result at MSB
                } else {
                    self.netlist.add_net(GateNet::new(
                        GateNetId(0),
                        format!("{}.lt_combined_{}", path, i),
                    ))
                };
                let or_cell = Cell::new_comb(
                    CellId(0),
                    or_info.name.clone(),
                    self.library.name.clone(),
                    or_info.fit,
                    format!("{}.lt_{}", path, i),
                    vec![lt_bit_net, eq_and_prev],
                    vec![combined],
                );
                self.add_cell(or_cell);
                combined
            } else {
                // prev_lt is 0, so lt = lt_bit
                lt_bit_net
            };

            prev_lt = Some(lt_net);
        }

        // Final result should be connected to output y
        // If prev_lt is already y, we're done. Otherwise need to buffer.
        if prev_lt != Some(y) {
            if let Some(lt_result) = prev_lt {
                // Create a buffer to connect to output
                let buf_info = self.get_cell_info(&CellFunction::Buf);
                let buf_cell = Cell::new_comb(
                    CellId(0),
                    buf_info.name.clone(),
                    self.library.name.clone(),
                    buf_info.fit,
                    format!("{}.out_buf", path),
                    vec![lt_result],
                    vec![y],
                );
                self.add_cell(buf_cell);
            }
        }

        self.stats.decomposed_mappings += 1;
    }

    /// Map a signed less-than comparison: result = (a < b) for signed integers
    /// Implementation: XOR the MSB of both operands to flip the sign bit,
    /// then use unsigned comparison. This correctly maps the signed ordering.
    fn map_signed_less_than(
        &mut self,
        width: u32,
        inputs: &[Vec<GateNetId>],
        outputs: &[GateNetId],
        path: &str,
    ) {
        if inputs.len() < 2 {
            self.warnings.push(format!(
                "SignedLessThan needs 2 inputs, got {}",
                inputs.len()
            ));
            return;
        }

        let y = outputs.first().copied().unwrap_or(GateNetId(0));
        let a = &inputs[0];
        let b = &inputs[1];

        // For signed comparison, we flip the MSB of both operands
        // This converts signed ordering to unsigned ordering:
        //   -128 (0b10000000) -> 0 (0b00000000) [smallest]
        //   -1   (0b11111111) -> 127 (0b01111111)
        //   0    (0b00000000) -> 128 (0b10000000)
        //   127  (0b01111111) -> 255 (0b11111111) [largest]

        let xor_info = self.get_cell_info(&CellFunction::Xor2);
        let const1_info = self.get_cell_info(&CellFunction::TieHigh);

        // Create constant 1 for XOR with MSB
        let const1_net = self
            .netlist
            .add_net(GateNet::new(GateNetId(0), format!("{}.const1", path)));
        let const1_cell = Cell::new_comb(
            CellId(0),
            const1_info.name.clone(),
            self.library.name.clone(),
            const1_info.fit,
            format!("{}.const1", path),
            vec![],
            vec![const1_net],
        );
        self.add_cell(const1_cell);

        // Create modified inputs with flipped MSB
        // If input is narrower than width, use its actual MSB (sign bit) for sign extension
        let msb_idx = width as usize - 1;
        let a_msb = a
            .get(msb_idx)
            .or_else(|| a.last())
            .copied()
            .unwrap_or(GateNetId(0));
        let b_msb = b
            .get(msb_idx)
            .or_else(|| b.last())
            .copied()
            .unwrap_or(GateNetId(0));

        // Flip a's MSB
        let a_msb_flipped = self
            .netlist
            .add_net(GateNet::new(GateNetId(0), format!("{}.a_msb_flip", path)));
        let xor_a_cell = Cell::new_comb(
            CellId(0),
            xor_info.name.clone(),
            self.library.name.clone(),
            xor_info.fit,
            format!("{}.a_msb_xor", path),
            vec![a_msb, const1_net],
            vec![a_msb_flipped],
        );
        self.add_cell(xor_a_cell);

        // Flip b's MSB
        let b_msb_flipped = self
            .netlist
            .add_net(GateNet::new(GateNetId(0), format!("{}.b_msb_flip", path)));
        let xor_b_cell = Cell::new_comb(
            CellId(0),
            xor_info.name.clone(),
            self.library.name.clone(),
            xor_info.fit,
            format!("{}.b_msb_xor", path),
            vec![b_msb, const1_net],
            vec![b_msb_flipped],
        );
        self.add_cell(xor_b_cell);

        // Build modified input vectors
        let mut a_modified: Vec<GateNetId> = a.iter().take(msb_idx).copied().collect();
        a_modified.push(a_msb_flipped);

        let mut b_modified: Vec<GateNetId> = b.iter().take(msb_idx).copied().collect();
        b_modified.push(b_msb_flipped);

        // Now use unsigned less-than on the modified inputs
        self.map_less_than(
            width,
            &[a_modified, b_modified],
            outputs,
            &format!("{}.unsigned_cmp", path),
        );

        self.stats.decomposed_mappings += 1;
    }

    /// Map a signed less-than-or-equal comparison: result = (a <= b) for signed integers
    fn map_signed_less_equal(
        &mut self,
        width: u32,
        inputs: &[Vec<GateNetId>],
        outputs: &[GateNetId],
        path: &str,
    ) {
        // a <= b is equivalent to !(a > b) which is !(b < a)
        // So we compute b < a (signed) and invert the result
        if inputs.len() < 2 {
            self.warnings.push(format!(
                "SignedLessEqual needs 2 inputs, got {}",
                inputs.len()
            ));
            return;
        }

        let y = outputs.first().copied().unwrap_or(GateNetId(0));

        // First compute b < a (swap inputs for signed less than)
        let gt_result = self
            .netlist
            .add_net(GateNet::new(GateNetId(0), format!("{}.gt_result", path)));
        self.map_signed_less_than(
            width,
            &[inputs[1].clone(), inputs[0].clone()],
            &[gt_result],
            path,
        );

        // Invert to get a <= b
        let inv_info = self.get_cell_info(&CellFunction::Inv);
        let inv_cell = Cell::new_comb(
            CellId(0),
            inv_info.name.clone(),
            self.library.name.clone(),
            inv_info.fit,
            format!("{}.inv", path),
            vec![gt_result],
            vec![y],
        );
        self.add_cell(inv_cell);
    }

    /// Map a signed greater-than comparison: result = (a > b) for signed integers
    fn map_signed_greater_than(
        &mut self,
        width: u32,
        inputs: &[Vec<GateNetId>],
        outputs: &[GateNetId],
        path: &str,
    ) {
        // a > b is equivalent to b < a
        if inputs.len() < 2 {
            self.warnings.push(format!(
                "SignedGreaterThan needs 2 inputs, got {}",
                inputs.len()
            ));
            return;
        }

        // Swap inputs and use signed less than
        self.map_signed_less_than(
            width,
            &[inputs[1].clone(), inputs[0].clone()],
            outputs,
            path,
        );
    }

    /// Map a signed greater-than-or-equal comparison: result = (a >= b) for signed integers
    fn map_signed_greater_equal(
        &mut self,
        width: u32,
        inputs: &[Vec<GateNetId>],
        outputs: &[GateNetId],
        path: &str,
    ) {
        // a >= b is equivalent to !(a < b)
        if inputs.len() < 2 {
            self.warnings.push(format!(
                "SignedGreaterEqual needs 2 inputs, got {}",
                inputs.len()
            ));
            return;
        }

        let y = outputs.first().copied().unwrap_or(GateNetId(0));

        // First compute a < b (signed)
        let lt_result = self
            .netlist
            .add_net(GateNet::new(GateNetId(0), format!("{}.lt_result", path)));
        self.map_signed_less_than(width, inputs, &[lt_result], path);

        // Invert to get a >= b
        let inv_info = self.get_cell_info(&CellFunction::Inv);
        let inv_cell = Cell::new_comb(
            CellId(0),
            inv_info.name.clone(),
            self.library.name.clone(),
            inv_info.fit,
            format!("{}.inv", path),
            vec![lt_result],
            vec![y],
        );
        self.add_cell(inv_cell);
    }

    /// Map a register
    #[allow(clippy::too_many_arguments)]
    fn map_register(
        &mut self,
        width: u32,
        inputs: &[Vec<GateNetId>],
        outputs: &[GateNetId],
        path: &str,
        clock: Option<GateNetId>,
        reset: Option<GateNetId>,
        reset_value: Option<u64>,
        async_reset: bool,
    ) {
        if inputs.is_empty() {
            self.warnings
                .push("Register needs at least 1 input (d)".to_string());
            return;
        }

        let reset_val = reset_value.unwrap_or(0);
        let clk = clock.unwrap_or(GateNetId(0));
        let tie_low = self.get_tie_low();

        if async_reset && reset.is_some() {
            // Async reset: DFF with dedicated async reset pin, no MUX on D-input.
            // Reset takes effect immediately, independent of clock.
            let dff_func = CellFunction::DffR;
            let dff_info = self.get_cell_info(&dff_func);

            for bit in 0..width as usize {
                let d_orig = inputs[0].get(bit).copied().unwrap_or(tie_low);
                let q = outputs.get(bit).copied().unwrap_or(outputs[0]);
                let reset_bit_val = (reset_val >> bit) & 1 != 0;

                if reset_bit_val {
                    // Reset-to-1: invert D before DFF, use DffR (clears to 0), invert Q output
                    // At reset: stored=0, output=INV(0)=1. During operation: stored=~D, output=INV(~D)=D.
                    let inv_d_net = self
                        .netlist
                        .add_net_with_name(format!("{}.inv_d_bit{}", path, bit));
                    let inv_info = self.get_cell_info(&CellFunction::Inv);
                    let mut inv_d_cell = Cell::new_comb(
                        CellId(0),
                        inv_info.name.clone(),
                        self.library.name.clone(),
                        inv_info.fit,
                        format!("{}.inv_d{}", path, bit),
                        vec![d_orig],
                        vec![inv_d_net],
                    );
                    inv_d_cell.source_op = Some("AsyncResetInvD".to_string());
                    inv_info.apply_to_cell(&mut inv_d_cell);
                    self.add_cell(inv_d_cell);

                    let dff_q_net = self
                        .netlist
                        .add_net_with_name(format!("{}.dff_q_bit{}", path, bit));
                    let mut cell = Cell::new_seq(
                        CellId(0),
                        dff_info.name.clone(),
                        self.library.name.clone(),
                        dff_info.fit,
                        format!("{}.bit{}", path, bit),
                        vec![inv_d_net],
                        vec![dff_q_net],
                        clk,
                        reset,
                    );
                    cell.source_op = Some("AsyncResetRegister".to_string());
                    dff_info.apply_to_cell(&mut cell);
                    self.add_cell(cell);

                    let inv_q_info = self.get_cell_info(&CellFunction::Inv);
                    let mut inv_q_cell = Cell::new_comb(
                        CellId(0),
                        inv_q_info.name.clone(),
                        self.library.name.clone(),
                        inv_q_info.fit,
                        format!("{}.inv_q{}", path, bit),
                        vec![dff_q_net],
                        vec![q],
                    );
                    inv_q_cell.source_op = Some("AsyncResetInvQ".to_string());
                    inv_q_info.apply_to_cell(&mut inv_q_cell);
                    self.add_cell(inv_q_cell);
                } else {
                    // Reset-to-0: standard DffR cell with async clear
                    let mut cell = Cell::new_seq(
                        CellId(0),
                        dff_info.name.clone(),
                        self.library.name.clone(),
                        dff_info.fit,
                        format!("{}.bit{}", path, bit),
                        vec![d_orig],
                        vec![q],
                        clk,
                        reset,
                    );
                    cell.source_op = Some("AsyncResetRegister".to_string());
                    dff_info.apply_to_cell(&mut cell);
                    self.add_cell(cell);
                }
            }
        } else {
            // Sync reset: use DffR (reset-to-0) when possible, otherwise MUX + Dff
            let has_reset = reset.is_some();

            // Check if all reset bits are 0 — can use DffR directly (no MUX needed)
            let all_reset_zero = reset_val == 0;
            let use_dffr = has_reset && all_reset_zero;

            let dff_func = if use_dffr {
                CellFunction::DffR
            } else {
                CellFunction::Dff
            };
            let dff_info = self.get_cell_info(&dff_func);

            for bit in 0..width as usize {
                let d_orig = inputs[0].get(bit).copied().unwrap_or(tie_low);
                let q = outputs.get(bit).copied().unwrap_or(outputs[0]);

                let d = if has_reset && !use_dffr {
                    // Non-zero reset value: need MUX to select between reset_value and D
                    let reset_bit_val = (reset_val >> bit) & 1 != 0;

                    let reset_bit_net = self
                        .netlist
                        .add_net_with_name(format!("{}.rst_val_bit{}", path, bit));
                    let tie_cell_name = if reset_bit_val { "TIE_HIGH" } else { "TIE_LOW" };
                    let mut tie_cell = Cell::new_comb(
                        CellId(0),
                        tie_cell_name.to_string(),
                        self.library.name.clone(),
                        0.01,
                        format!("{}.rst_tie{}", path, bit),
                        vec![],
                        vec![reset_bit_net],
                    );
                    tie_cell.source_op = Some("ResetValue".to_string());
                    self.add_cell(tie_cell);

                    let mux_out = self
                        .netlist
                        .add_net_with_name(format!("{}.rst_mux_bit{}", path, bit));
                    let mux_info = self.get_cell_info(&CellFunction::Mux2);
                    let mut mux_cell = Cell::new_comb(
                        CellId(0),
                        mux_info.name.clone(),
                        self.library.name.clone(),
                        mux_info.fit,
                        format!("{}.rst_mux{}", path, bit),
                        vec![reset.unwrap(), d_orig, reset_bit_net],
                        vec![mux_out],
                    );
                    mux_cell.source_op = Some("ResetMux".to_string());
                    mux_info.apply_to_cell(&mut mux_cell);
                    self.add_cell(mux_cell);

                    mux_out
                } else {
                    d_orig
                };

                let dff_inputs = vec![d];
                let dff_reset = if use_dffr { reset } else { None };

                let mut cell = Cell::new_seq(
                    CellId(0),
                    dff_info.name.clone(),
                    self.library.name.clone(),
                    dff_info.fit,
                    format!("{}.bit{}", path, bit),
                    dff_inputs,
                    vec![q],
                    clk,
                    dff_reset,
                );
                cell.source_op = Some("Register".to_string());
                dff_info.apply_to_cell(&mut cell);
                self.add_cell(cell);
            }
        }

        self.stats.direct_mappings += 1;
    }

    /// Map a register with an absorbed enable MUX, using DffE or DffRE cells.
    ///
    /// Instead of Mux2 + Dff, we use the DFF's built-in enable pin:
    /// - DffE: D flip-flop with enable (no reset)
    /// - DffRE: D flip-flop with reset and enable
    #[allow(clippy::too_many_arguments)]
    fn map_register_with_enable(
        &mut self,
        width: u32,
        data_nets: &[GateNetId],
        enable_nets: &[GateNetId],
        invert_enable: bool,
        outputs: &[GateNetId],
        path: &str,
        clock: Option<GateNetId>,
        reset: Option<GateNetId>,
        reset_value: Option<u64>,
        async_reset: bool,
    ) {
        let reset_val = reset_value.unwrap_or(0);
        let clk = clock.unwrap_or(GateNetId(0));
        let tie_low = self.get_tie_low();

        // Get enable net (single bit — it's the Mux2 select)
        let raw_enable = enable_nets.first().copied().unwrap_or(tie_low);

        // Invert enable if needed (d1=Q case: sel=1 means hold, so enable = ~sel)
        let enable = if invert_enable {
            let inv_en_net = self
                .netlist
                .add_net_with_name(format!("{}.inv_en", path));
            let inv_info = self.get_cell_info(&CellFunction::Inv);
            let mut inv_cell = Cell::new_comb(
                CellId(0),
                inv_info.name.clone(),
                self.library.name.clone(),
                inv_info.fit,
                format!("{}.inv_en", path),
                vec![raw_enable],
                vec![inv_en_net],
            );
            inv_cell.source_op = Some("EnableInvert".to_string());
            inv_info.apply_to_cell(&mut inv_cell);
            self.add_cell(inv_cell);
            inv_en_net
        } else {
            raw_enable
        };

        let has_reset = reset.is_some();

        if async_reset && has_reset {
            // Async reset with enable — need DffRE equivalent
            // For now, fall back to DffR + external enable MUX if no async reset+enable cell
            // iCE40 has SB_DFFER (async reset + enable)
            let dff_func = CellFunction::DffRE;
            let dff_info = self.get_cell_info(&dff_func);

            for bit in 0..width as usize {
                let d = data_nets.get(bit).copied().unwrap_or(tie_low);
                let q = outputs.get(bit).copied().unwrap_or(outputs[0]);
                let reset_bit_val = (reset_val >> bit) & 1 != 0;

                if reset_bit_val {
                    // Reset-to-1: invert D and Q around a reset-to-0 DffRE
                    let inv_d_net = self
                        .netlist
                        .add_net_with_name(format!("{}.inv_d_bit{}", path, bit));
                    let inv_info = self.get_cell_info(&CellFunction::Inv);
                    let mut inv_d_cell = Cell::new_comb(
                        CellId(0),
                        inv_info.name.clone(),
                        self.library.name.clone(),
                        inv_info.fit,
                        format!("{}.inv_d{}", path, bit),
                        vec![d],
                        vec![inv_d_net],
                    );
                    inv_d_cell.source_op = Some("AsyncResetInvD".to_string());
                    inv_info.apply_to_cell(&mut inv_d_cell);
                    self.add_cell(inv_d_cell);

                    let dff_q_net = self
                        .netlist
                        .add_net_with_name(format!("{}.dff_q_bit{}", path, bit));
                    let mut cell = Cell::new_seq_with_enable(
                        CellId(0),
                        dff_info.name.clone(),
                        self.library.name.clone(),
                        dff_info.fit,
                        format!("{}.bit{}", path, bit),
                        inv_d_net,
                        enable,
                        dff_q_net,
                        clk,
                        reset,
                    );
                    cell.source_op = Some("RegisterWithEnable".to_string());
                    dff_info.apply_to_cell(&mut cell);
                    self.add_cell(cell);

                    let inv_q_info = self.get_cell_info(&CellFunction::Inv);
                    let mut inv_q_cell = Cell::new_comb(
                        CellId(0),
                        inv_q_info.name.clone(),
                        self.library.name.clone(),
                        inv_q_info.fit,
                        format!("{}.inv_q{}", path, bit),
                        vec![dff_q_net],
                        vec![q],
                    );
                    inv_q_cell.source_op = Some("AsyncResetInvQ".to_string());
                    inv_q_info.apply_to_cell(&mut inv_q_cell);
                    self.add_cell(inv_q_cell);
                } else {
                    let mut cell = Cell::new_seq_with_enable(
                        CellId(0),
                        dff_info.name.clone(),
                        self.library.name.clone(),
                        dff_info.fit,
                        format!("{}.bit{}", path, bit),
                        d,
                        enable,
                        q,
                        clk,
                        reset,
                    );
                    cell.source_op = Some("RegisterWithEnable".to_string());
                    dff_info.apply_to_cell(&mut cell);
                    self.add_cell(cell);
                }
            }
        } else {
            // Sync reset or no reset — use DffRE (reset+enable) or DffE (enable only)
            let all_reset_zero = reset_val == 0;
            let use_dffre = has_reset && all_reset_zero;

            let dff_func = if use_dffre {
                CellFunction::DffRE
            } else if has_reset {
                // Non-zero reset: need MUX for reset value + DffE
                CellFunction::DffE
            } else {
                CellFunction::DffE
            };
            let dff_info = self.get_cell_info(&dff_func);

            for bit in 0..width as usize {
                let d_orig = data_nets.get(bit).copied().unwrap_or(tie_low);
                let q = outputs.get(bit).copied().unwrap_or(outputs[0]);

                let d = if has_reset && !use_dffre {
                    // Non-zero reset value: need MUX to select between reset_value and D
                    let reset_bit_val = (reset_val >> bit) & 1 != 0;
                    let reset_bit_net = self
                        .netlist
                        .add_net_with_name(format!("{}.rst_val_bit{}", path, bit));
                    let tie_cell_name = if reset_bit_val { "TIE_HIGH" } else { "TIE_LOW" };
                    let mut tie_cell = Cell::new_comb(
                        CellId(0),
                        tie_cell_name.to_string(),
                        self.library.name.clone(),
                        0.01,
                        format!("{}.rst_tie{}", path, bit),
                        vec![],
                        vec![reset_bit_net],
                    );
                    tie_cell.source_op = Some("ResetValue".to_string());
                    self.add_cell(tie_cell);

                    let mux_out = self
                        .netlist
                        .add_net_with_name(format!("{}.rst_mux_bit{}", path, bit));
                    let mux_info = self.get_cell_info(&CellFunction::Mux2);
                    let mut mux_cell = Cell::new_comb(
                        CellId(0),
                        mux_info.name.clone(),
                        self.library.name.clone(),
                        mux_info.fit,
                        format!("{}.rst_mux{}", path, bit),
                        vec![reset.unwrap(), d_orig, reset_bit_net],
                        vec![mux_out],
                    );
                    mux_cell.source_op = Some("ResetMux".to_string());
                    mux_info.apply_to_cell(&mut mux_cell);
                    self.add_cell(mux_cell);
                    mux_out
                } else {
                    d_orig
                };

                let dff_reset = if use_dffre { reset } else { None };

                let mut cell = Cell::new_seq_with_enable(
                    CellId(0),
                    dff_info.name.clone(),
                    self.library.name.clone(),
                    dff_info.fit,
                    format!("{}.bit{}", path, bit),
                    d,
                    enable,
                    q,
                    clk,
                    dff_reset,
                );
                cell.source_op = Some("RegisterWithEnable".to_string());
                dff_info.apply_to_cell(&mut cell);
                self.add_cell(cell);
            }
        }

        self.stats.direct_mappings += 1;
    }

    /// Map a constant
    fn map_constant(&mut self, width: u32, value: u64, outputs: &[GateNetId], path: &str) {
        // Constants are implemented with tie-high/tie-low cells
        for bit in 0..width as usize {
            // For bits >= 64, they're always 0 since value is u64
            let bit_val = if bit < 64 {
                (value >> bit) & 1 != 0
            } else {
                false
            };
            let y = outputs.get(bit).copied().unwrap_or(GateNetId(0));

            let cell_name = if bit_val { "TIE_HIGH" } else { "TIE_LOW" };
            let fit = 0.01; // Very low FIT for constant drivers

            let mut cell = Cell::new_comb(
                CellId(0),
                cell_name.to_string(),
                self.library.name.clone(),
                fit,
                format!("{}.const{}", path, bit),
                vec![],
                vec![y],
            );
            cell.source_op = Some("Constant".to_string());
            self.add_cell(cell);
        }

        self.stats.direct_mappings += 1;
    }

    /// Map a reduction operation (AND, OR, XOR tree)
    fn map_reduction(
        &mut self,
        function: CellFunction,
        width: u32,
        inputs: &[Vec<GateNetId>],
        outputs: &[GateNetId],
        path: &str,
    ) {
        if inputs.is_empty() {
            self.warnings
                .push("Reduction needs at least 1 input".to_string());
            return;
        }

        let cell_info = self.get_cell_info(&function);

        // Get all input bits
        let input_bits: Vec<GateNetId> = inputs[0].iter().take(width as usize).copied().collect();

        // Build reduction tree
        let result = self.reduce_tree(&input_bits, &cell_info, path, "reduce");

        // Connect to output
        let y = outputs.first().copied().unwrap_or(GateNetId(0));
        if result != y {
            let buf_info = self.get_cell_info(&CellFunction::Buf);

            let mut cell = Cell::new_comb(
                CellId(0),
                buf_info.name.clone(),
                self.library.name.clone(),
                buf_info.fit,
                format!("{}.reduce_buf", path),
                vec![result],
                vec![y],
            );
            cell.source_op = Some("Buffer".to_string());
            buf_info.apply_to_cell(&mut cell);
            self.add_cell(cell);
        }

        self.stats.decomposed_mappings += 1;
    }

    /// Build a binary tree reduction
    fn reduce_tree(
        &mut self,
        inputs: &[GateNetId],
        cell_info: &LibraryCellInfo,
        path: &str,
        prefix: &str,
    ) -> GateNetId {
        if inputs.is_empty() {
            return GateNetId(0);
        }
        if inputs.len() == 1 {
            return inputs[0];
        }

        let mut current_level = inputs.to_vec();
        let mut level = 0;

        while current_level.len() > 1 {
            let mut next_level = Vec::new();

            for (pair_idx, chunk) in current_level.chunks(2).enumerate() {
                if chunk.len() == 2 {
                    let out = self.alloc_net_id();
                    self.netlist.add_net(GateNet::new(
                        out,
                        format!("{}.{}_l{}_{}", path, prefix, level, pair_idx),
                    ));
                    self.stats.nets_created += 1;

                    let mut cell = Cell::new_comb(
                        CellId(0),
                        cell_info.name.clone(),
                        self.library.name.clone(),
                        cell_info.fit,
                        format!("{}.{}_l{}_{}", path, prefix, level, pair_idx),
                        vec![chunk[0], chunk[1]],
                        vec![out],
                    );
                    cell.source_op = Some(cell_info.name.clone());
                    cell_info.apply_to_cell(&mut cell);
                    self.add_cell(cell);
                    next_level.push(out);
                } else {
                    // Odd element passes through
                    next_level.push(chunk[0]);
                }
            }

            current_level = next_level;
            level += 1;
        }

        current_level[0]
    }

    /// Add a cell to the netlist, applying module-level safety classification
    fn add_cell(&mut self, mut cell: Cell) {
        // Apply safety classification from module-level safety info
        if let Some(ref safety_info) = self.module_safety_info {
            cell.safety_classification =
                CellSafetyClassification::from_lir_safety_info(safety_info);
        }
        self.netlist.add_cell(cell);
        self.stats.cells_created += 1;
    }

    /// Allocate a new net ID
    /// NOTE: This returns the NEXT ID that will be assigned by add_net.
    /// The caller MUST call add_net immediately after this to keep IDs in sync.
    fn alloc_net_id(&mut self) -> GateNetId {
        // Use nets.len() to stay in sync with add_net which assigns IDs from nets.len()
        GateNetId(self.netlist.nets.len() as u32)
    }

    // =========================================================================
    // Clock Buffer Insertion
    // =========================================================================

    /// Insert global clock buffers on all clock nets.
    ///
    /// For each clock net, creates a ClkBuf cell (e.g., SB_GB on iCE40, DCCA on ECP5)
    /// and rewires all clock consumers to use the buffered output. This ensures clocks
    /// are routed through dedicated global clock routing fabric for timing closure.
    fn insert_clock_buffers(&mut self) {
        // Check if the library has a clock buffer cell
        let (clk_buf_cell, clk_buf_info) = match self.library.find_clk_buf_cell() {
            Some(pair) => pair,
            None => return, // No clock buffer in library (e.g., generic_asic)
        };

        let cell_name = clk_buf_cell.name.clone();
        let cell_fit = clk_buf_cell.fit;
        let cell_info = LibraryCellInfo::from_library_cell(clk_buf_cell);
        let input_pin = clk_buf_info.input.clone();
        let output_pin = clk_buf_info.output.clone();
        let has_enable = clk_buf_info.has_enable;
        let enable_pin = clk_buf_info.enable.clone();

        // Clone clock net IDs to avoid borrow conflict
        let clock_nets: Vec<GateNetId> = self.netlist.clocks.clone();

        for clock_net_id in clock_nets {
            let clock_name = self
                .netlist
                .nets
                .get(clock_net_id.0 as usize)
                .map(|n| n.name.clone())
                .unwrap_or_else(|| format!("clk_{}", clock_net_id.0));

            // Create buffered output net
            let buf_out_id = self.alloc_net_id();
            let mut buf_out_net = GateNet::new(buf_out_id, format!("{}_gbuf", clock_name));
            buf_out_net.is_clock = true;
            self.netlist.add_net(buf_out_net);

            // Build input list: clock input, plus tie-high for enable if needed
            let mut cell_inputs = vec![clock_net_id];
            if has_enable {
                let tie_high = self.get_tie_high();
                cell_inputs.push(tie_high);
            }

            // Create clock buffer cell
            let mut cell = Cell::new_comb(
                CellId(0),
                cell_name.clone(),
                self.library.name.clone(),
                cell_fit,
                format!("{}_gbuf", clock_name),
                cell_inputs,
                vec![buf_out_id],
            );
            cell.source_op = Some("ClockBuffer".to_string());
            cell_info.apply_to_cell(&mut cell);
            self.add_cell(cell);

            // Rewire all clock consumers to use the buffered net
            self.rewire_clock_consumers(clock_net_id, buf_out_id);

            self.stats.clock_buffers_inserted += 1;
        }
    }

    /// Rewire all clock consumers from `old_net` to `new_net`.
    ///
    /// Updates cell clock connections and input pin connections, skipping the
    /// clock buffer cell itself (which drives `new_net` from `old_net`).
    fn rewire_clock_consumers(&mut self, old_net: GateNetId, new_net: GateNetId) {
        for cell_idx in 0..self.netlist.cells.len() {
            let cell = &self.netlist.cells[cell_idx];

            // Skip the clock buffer cell itself (its input is old_net and output is new_net)
            if cell.inputs.contains(&old_net) && cell.outputs.contains(&new_net) {
                continue;
            }

            // Rewire clock connection
            if self.netlist.cells[cell_idx].clock == Some(old_net) {
                self.netlist.cells[cell_idx].clock = Some(new_net);
            }

            // Rewire input pins
            for pin_idx in 0..self.netlist.cells[cell_idx].inputs.len() {
                if self.netlist.cells[cell_idx].inputs[pin_idx] == old_net {
                    self.netlist.cells[cell_idx].inputs[pin_idx] = new_net;
                }
            }
        }

        // Note: netlist.clocks is NOT updated — it must keep pointing to the
        // original primary input clock nets for simulation clock edge detection.
        // The SIR builder uses netlist.clocks to identify the clock signal for
        // sequential blocks. The physical routing through clock buffers is captured
        // by the cell input/output connections.
    }

    // =========================================================================
    // IO Buffer Insertion
    // =========================================================================

    /// Insert IO buffers on primary input and output ports.
    ///
    /// For each primary input (excluding clocks), inserts an input pad cell.
    /// For each primary output, inserts an output pad cell.
    /// Constraints from `self.port_constraints` are propagated to cell parameters.
    fn insert_io_buffers(&mut self) {
        // Check if the library has any IO cells (any cell with io_info)
        let has_io_cells = self
            .library
            .iter_cells()
            .any(|(_, cell)| cell.io_info.is_some());
        if !has_io_cells {
            return;
        }

        // Clone port lists to avoid borrow conflicts
        let input_nets: Vec<GateNetId> = self.netlist.inputs.clone();
        let output_nets: Vec<GateNetId> = self.netlist.outputs.clone();
        let clock_nets: std::collections::HashSet<GateNetId> =
            self.netlist.clocks.iter().copied().collect();

        // Insert input IO buffers
        for &input_net_id in &input_nets {
            // Skip clock nets (already handled by clock buffers)
            if clock_nets.contains(&input_net_id) {
                continue;
            }

            let (input_cell, input_io_info) = match self.library.find_input_pad() {
                Some(pair) => pair,
                None => continue,
            };

            let port_name = self
                .netlist
                .nets
                .get(input_net_id.0 as usize)
                .map(|n| n.name.clone())
                .unwrap_or_else(|| format!("in_{}", input_net_id.0));

            // Create internal buffered net
            let buf_out_id = self.alloc_net_id();
            let buf_out_net = GateNet::new(buf_out_id, format!("{}_io", port_name));
            self.netlist.add_net(buf_out_net);

            let cell_name = input_cell.name.clone();
            let cell_fit = input_cell.fit;
            let cell_info = LibraryCellInfo::from_library_cell(input_cell);

            // Build IO buffer cell: input from pad net, output to internal net
            let mut cell = Cell::new_comb(
                CellId(0),
                cell_name,
                self.library.name.clone(),
                cell_fit,
                format!("{}_ibuf", port_name),
                vec![input_net_id],
                vec![buf_out_id],
            );
            cell.source_op = Some("IOBuffer".to_string());
            cell_info.apply_to_cell(&mut cell);

            // Apply physical constraints (pin location, IO standard, etc.)
            self.apply_io_constraints(&mut cell, &port_name);

            self.add_cell(cell);

            // Rewire all consumers of the original input to use the buffered net
            self.rewire_io_consumers(input_net_id, buf_out_id);

            self.stats.io_buffers_inserted += 1;
        }

        // Insert output IO buffers
        for &output_net_id in &output_nets {
            let (output_cell, output_io_info) = match self.library.find_output_pad() {
                Some(pair) => pair,
                None => continue,
            };

            let port_name = self
                .netlist
                .nets
                .get(output_net_id.0 as usize)
                .map(|n| n.name.clone())
                .unwrap_or_else(|| format!("out_{}", output_net_id.0));

            // Create pad output net
            let pad_out_id = self.alloc_net_id();
            let pad_out_net = GateNet::new(pad_out_id, format!("{}_pad", port_name));
            self.netlist.add_net(pad_out_net);

            let cell_name = output_cell.name.clone();
            let cell_fit = output_cell.fit;
            let cell_info = LibraryCellInfo::from_library_cell(output_cell);
            let supports_tristate = output_io_info.supports_tristate;

            // Build inputs: signal + optional tie-high for output enable
            let mut cell_inputs = vec![output_net_id];
            if supports_tristate {
                let tie_high = self.get_tie_high();
                cell_inputs.push(tie_high);
            }

            let mut cell = Cell::new_comb(
                CellId(0),
                cell_name,
                self.library.name.clone(),
                cell_fit,
                format!("{}_obuf", port_name),
                cell_inputs,
                vec![pad_out_id],
            );
            cell.source_op = Some("IOBuffer".to_string());
            cell_info.apply_to_cell(&mut cell);

            // Apply physical constraints
            self.apply_io_constraints(&mut cell, &port_name);

            self.add_cell(cell);

            self.stats.io_buffers_inserted += 1;
        }
    }

    /// Apply physical constraints from port annotations to IO cell parameters.
    ///
    /// Maps `PhysicalConstraints` fields to cell `parameters` entries that the
    /// native PnR reads directly. External flows (nextpnr) can still use
    /// `constraint_gen.rs` for PCF/XDC file generation.
    fn apply_io_constraints(&self, cell: &mut Cell, port_name: &str) {
        let constraints = match self.port_constraints.get(port_name) {
            Some(c) => c,
            None => return,
        };

        if let Some(ref pin_loc) = constraints.pin_location {
            use skalp_frontend::hir::PinLocation;
            match pin_loc {
                PinLocation::Single(pin) => {
                    cell.parameters.insert("LOC".into(), pin.clone());
                }
                PinLocation::Differential { positive, negative } => {
                    cell.parameters.insert("LOC".into(), positive.clone());
                    cell.parameters.insert("LOC_N".into(), negative.clone());
                }
                _ => {} // Multi-bit handled per-bit by caller
            }
        }
        if let Some(ref std) = constraints.io_standard {
            cell.parameters.insert("IO_STANDARD".into(), std.clone());
        }
        if let Some(ref drive) = constraints.drive_strength {
            cell.parameters
                .insert("DRIVE_STRENGTH".into(), format!("{:?}", drive));
        }
        if let Some(ref slew) = constraints.slew_rate {
            cell.parameters
                .insert("SLEW_RATE".into(), format!("{:?}", slew));
        }
        if let Some(ref term) = constraints.termination {
            cell.parameters
                .insert("TERMINATION".into(), format!("{:?}", term));
        }
        if let Some(schmitt) = constraints.schmitt_trigger {
            cell.parameters
                .insert("SCHMITT_TRIGGER".into(), schmitt.to_string());
        }
    }

    /// Rewire all consumers of `old_net` to use `new_net`, skipping IO buffer cells.
    fn rewire_io_consumers(&mut self, old_net: GateNetId, new_net: GateNetId) {
        for cell_idx in 0..self.netlist.cells.len() {
            let cell = &self.netlist.cells[cell_idx];

            // Skip the IO buffer cell itself (its input is old_net and output is new_net)
            if cell.inputs.contains(&old_net) && cell.outputs.contains(&new_net) {
                continue;
            }

            // Rewire input pins
            for pin_idx in 0..self.netlist.cells[cell_idx].inputs.len() {
                if self.netlist.cells[cell_idx].inputs[pin_idx] == old_net {
                    self.netlist.cells[cell_idx].inputs[pin_idx] = new_net;
                }
            }

            // Rewire clock connection (in case a non-clock input was routed through clock)
            if self.netlist.cells[cell_idx].clock == Some(old_net) {
                self.netlist.cells[cell_idx].clock = Some(new_net);
            }
        }
    }

    // =========================================================================
    // NCL (Null Convention Logic) Mapping Functions
    // =========================================================================
    //
    // NCL uses dual-rail encoding where each logical bit is represented by two wires:
    // - (t, f) = (0, 0) => NULL (spacer)
    // - (t, f) = (0, 1) => DATA_FALSE (logical 0)
    // - (t, f) = (1, 0) => DATA_TRUE (logical 1)
    // - (t, f) = (1, 1) => Invalid (should never occur)
    //
    // THmn gates are m-of-n threshold gates with hysteresis:
    // - TH12: 1-of-2 (OR with hold)
    // - TH22: 2-of-2 (C-element, Muller gate)
    // - TH23: 2-of-3 threshold, etc.

    /// Map NCL encode: single-rail → t-rail (buffer only)
    /// The f-rail is handled by a separate LirOp::Not node in the LIR
    fn map_ncl_encode(
        &mut self,
        width: u32,
        inputs: &[Vec<GateNetId>],
        outputs: &[GateNetId],
        path: &str,
    ) {
        if inputs.is_empty() {
            self.warnings.push("NCL encode needs input".to_string());
            return;
        }

        let buf_info = self.get_cell_info(&CellFunction::Buf);

        for i in 0..width as usize {
            let input_bit = inputs[0].get(i).copied().unwrap_or(GateNetId(0));
            // NclEncode only outputs the t-rail; f-rail is a separate NOT node
            let out_t = outputs.get(i).copied().unwrap_or(GateNetId(0));

            // True rail is the input directly (via buffer)
            let mut buf_cell = Cell::new_comb(
                CellId(0),
                buf_info.name.clone(),
                self.library.name.clone(),
                buf_info.fit,
                format!("{}.enc_t{}", path, i),
                vec![input_bit],
                vec![out_t],
            );
            buf_cell.source_op = Some("NclEncode_T".to_string());
            self.add_cell(buf_cell);
        }

        self.stats.decomposed_mappings += 1;
    }

    /// Map NCL decode: dual-rail → single-rail
    /// Output is just the true rail (ignores false rail)
    fn map_ncl_decode(
        &mut self,
        width: u32,
        inputs: &[Vec<GateNetId>],
        outputs: &[GateNetId],
        path: &str,
    ) {
        if inputs.is_empty() {
            self.warnings.push("NCL decode needs input".to_string());
            return;
        }

        let buf_info = self.get_cell_info(&CellFunction::Buf);

        for i in 0..width as usize {
            // inputs[0] = t-rail signal (all t bits), inputs[1] = f-rail signal (all f bits)
            // We only use the t-rail for decode (single-rail = just the true rail)
            let in_t = inputs[0].get(i).copied().unwrap_or(GateNetId(0));
            let out = outputs.get(i).copied().unwrap_or(GateNetId(0));

            // Decode just takes the true rail
            let mut cell = Cell::new_comb(
                CellId(0),
                buf_info.name.clone(),
                self.library.name.clone(),
                buf_info.fit,
                format!("{}.dec{}", path, i),
                vec![in_t],
                vec![out],
            );
            cell.source_op = Some("NclDecode".to_string());
            self.add_cell(cell);
        }

        self.stats.decomposed_mappings += 1;
    }

    /// Map NCL AND: For each bit position:
    /// - out_t = TH22(a_t, b_t)  -- AND with hysteresis
    /// - out_f = TH12(a_f, b_f)  -- OR with hysteresis
    fn map_ncl_and(
        &mut self,
        width: u32,
        inputs: &[Vec<GateNetId>],
        outputs: &[GateNetId],
        path: &str,
    ) {
        if inputs.len() < 2 {
            self.warnings
                .push("NCL AND needs 2 dual-rail inputs".to_string());
            return;
        }

        let th22_info = self.get_ncl_cell_info(&CellFunction::Th22);
        let th12_info = self.get_ncl_cell_info(&CellFunction::Th12);

        for i in 0..width as usize {
            // Dual-rail inputs: [a_t, a_f, ...] and [b_t, b_f, ...]
            let a_t = inputs[0].get(i * 2).copied().unwrap_or(GateNetId(0));
            let a_f = inputs[0].get(i * 2 + 1).copied().unwrap_or(GateNetId(0));
            let b_t = inputs[1].get(i * 2).copied().unwrap_or(GateNetId(0));
            let b_f = inputs[1].get(i * 2 + 1).copied().unwrap_or(GateNetId(0));

            let out_t = outputs.get(i * 2).copied().unwrap_or(GateNetId(0));
            let out_f = outputs.get(i * 2 + 1).copied().unwrap_or(GateNetId(0));

            // True rail: TH22(a_t, b_t) - both true rails must be high
            let mut cell_t = Cell::new_comb(
                CellId(0),
                th22_info.name.clone(),
                self.library.name.clone(),
                th22_info.fit,
                format!("{}.and_t{}", path, i),
                vec![a_t, b_t],
                vec![out_t],
            );
            cell_t.source_op = Some("NclAnd_T".to_string());
            self.add_cell(cell_t);

            // False rail: TH12(a_f, b_f) - either false rail high
            let mut cell_f = Cell::new_comb(
                CellId(0),
                th12_info.name.clone(),
                self.library.name.clone(),
                th12_info.fit,
                format!("{}.and_f{}", path, i),
                vec![a_f, b_f],
                vec![out_f],
            );
            cell_f.source_op = Some("NclAnd_F".to_string());
            self.add_cell(cell_f);
        }

        self.stats.decomposed_mappings += 1;
    }

    /// Map NCL OR: For each bit position:
    /// - out_t = TH12(a_t, b_t)  -- OR with hysteresis
    /// - out_f = TH22(a_f, b_f)  -- AND with hysteresis
    fn map_ncl_or(
        &mut self,
        width: u32,
        inputs: &[Vec<GateNetId>],
        outputs: &[GateNetId],
        path: &str,
    ) {
        if inputs.len() < 2 {
            self.warnings
                .push("NCL OR needs 2 dual-rail inputs".to_string());
            return;
        }

        let th12_info = self.get_ncl_cell_info(&CellFunction::Th12);
        let th22_info = self.get_ncl_cell_info(&CellFunction::Th22);

        for i in 0..width as usize {
            let a_t = inputs[0].get(i * 2).copied().unwrap_or(GateNetId(0));
            let a_f = inputs[0].get(i * 2 + 1).copied().unwrap_or(GateNetId(0));
            let b_t = inputs[1].get(i * 2).copied().unwrap_or(GateNetId(0));
            let b_f = inputs[1].get(i * 2 + 1).copied().unwrap_or(GateNetId(0));

            let out_t = outputs.get(i * 2).copied().unwrap_or(GateNetId(0));
            let out_f = outputs.get(i * 2 + 1).copied().unwrap_or(GateNetId(0));

            // True rail: TH12(a_t, b_t) - either true rail high
            let mut cell_t = Cell::new_comb(
                CellId(0),
                th12_info.name.clone(),
                self.library.name.clone(),
                th12_info.fit,
                format!("{}.or_t{}", path, i),
                vec![a_t, b_t],
                vec![out_t],
            );
            cell_t.source_op = Some("NclOr_T".to_string());
            self.add_cell(cell_t);

            // False rail: TH22(a_f, b_f) - both false rails high
            let mut cell_f = Cell::new_comb(
                CellId(0),
                th22_info.name.clone(),
                self.library.name.clone(),
                th22_info.fit,
                format!("{}.or_f{}", path, i),
                vec![a_f, b_f],
                vec![out_f],
            );
            cell_f.source_op = Some("NclOr_F".to_string());
            self.add_cell(cell_f);
        }

        self.stats.decomposed_mappings += 1;
    }

    /// Map NCL XOR: More complex using TH22 and TH12 combinations
    /// XOR(a,b)_t = TH22(TH12(a_t, b_f), TH12(a_f, b_t))
    /// XOR(a,b)_f = TH22(TH12(a_t, b_t), TH12(a_f, b_f))
    fn map_ncl_xor(
        &mut self,
        width: u32,
        inputs: &[Vec<GateNetId>],
        outputs: &[GateNetId],
        path: &str,
    ) {
        if inputs.len() < 2 {
            self.warnings
                .push("NCL XOR needs 2 dual-rail inputs".to_string());
            return;
        }

        let th12_info = self.get_ncl_cell_info(&CellFunction::Th12);
        let th22_info = self.get_ncl_cell_info(&CellFunction::Th22);

        for i in 0..width as usize {
            let a_t = inputs[0].get(i * 2).copied().unwrap_or(GateNetId(0));
            let a_f = inputs[0].get(i * 2 + 1).copied().unwrap_or(GateNetId(0));
            let b_t = inputs[1].get(i * 2).copied().unwrap_or(GateNetId(0));
            let b_f = inputs[1].get(i * 2 + 1).copied().unwrap_or(GateNetId(0));

            let out_t = outputs.get(i * 2).copied().unwrap_or(GateNetId(0));
            let out_f = outputs.get(i * 2 + 1).copied().unwrap_or(GateNetId(0));

            // Intermediate signals for XOR
            let at_bf = self.alloc_net_id();
            self.netlist
                .add_net(GateNet::new(at_bf, format!("{}.xor_at_bf{}", path, i)));

            let af_bt = self.alloc_net_id();
            self.netlist
                .add_net(GateNet::new(af_bt, format!("{}.xor_af_bt{}", path, i)));

            let at_bt = self.alloc_net_id();
            self.netlist
                .add_net(GateNet::new(at_bt, format!("{}.xor_at_bt{}", path, i)));

            let af_bf = self.alloc_net_id();
            self.netlist
                .add_net(GateNet::new(af_bf, format!("{}.xor_af_bf{}", path, i)));

            // TH12(a_t, b_f) -> at_bf
            let mut cell1 = Cell::new_comb(
                CellId(0),
                th12_info.name.clone(),
                self.library.name.clone(),
                th12_info.fit,
                format!("{}.xor_th12_1_{}", path, i),
                vec![a_t, b_f],
                vec![at_bf],
            );
            cell1.source_op = Some("NclXor_TH12".to_string());
            self.add_cell(cell1);

            // TH12(a_f, b_t) -> af_bt
            let mut cell2 = Cell::new_comb(
                CellId(0),
                th12_info.name.clone(),
                self.library.name.clone(),
                th12_info.fit,
                format!("{}.xor_th12_2_{}", path, i),
                vec![a_f, b_t],
                vec![af_bt],
            );
            cell2.source_op = Some("NclXor_TH12".to_string());
            self.add_cell(cell2);

            // TH22(at_bf, af_bt) -> out_t
            let mut cell3 = Cell::new_comb(
                CellId(0),
                th22_info.name.clone(),
                self.library.name.clone(),
                th22_info.fit,
                format!("{}.xor_t{}", path, i),
                vec![at_bf, af_bt],
                vec![out_t],
            );
            cell3.source_op = Some("NclXor_T".to_string());
            self.add_cell(cell3);

            // TH12(a_t, b_t) -> at_bt
            let mut cell4 = Cell::new_comb(
                CellId(0),
                th12_info.name.clone(),
                self.library.name.clone(),
                th12_info.fit,
                format!("{}.xor_th12_3_{}", path, i),
                vec![a_t, b_t],
                vec![at_bt],
            );
            cell4.source_op = Some("NclXor_TH12".to_string());
            self.add_cell(cell4);

            // TH12(a_f, b_f) -> af_bf
            let mut cell5 = Cell::new_comb(
                CellId(0),
                th12_info.name.clone(),
                self.library.name.clone(),
                th12_info.fit,
                format!("{}.xor_th12_4_{}", path, i),
                vec![a_f, b_f],
                vec![af_bf],
            );
            cell5.source_op = Some("NclXor_TH12".to_string());
            self.add_cell(cell5);

            // TH22(at_bt, af_bf) -> out_f
            let mut cell6 = Cell::new_comb(
                CellId(0),
                th22_info.name.clone(),
                self.library.name.clone(),
                th22_info.fit,
                format!("{}.xor_f{}", path, i),
                vec![at_bt, af_bf],
                vec![out_f],
            );
            cell6.source_op = Some("NclXor_F".to_string());
            self.add_cell(cell6);
        }

        self.stats.decomposed_mappings += 1;
    }

    /// Map NCL NOT: Simply swap true and false rails
    fn map_ncl_not(
        &mut self,
        width: u32,
        inputs: &[Vec<GateNetId>],
        outputs: &[GateNetId],
        path: &str,
    ) {
        if inputs.is_empty() {
            self.warnings.push("NCL NOT needs input".to_string());
            return;
        }

        let buf_info = self.get_cell_info(&CellFunction::Buf);

        for i in 0..width as usize {
            let in_t = inputs[0].get(i * 2).copied().unwrap_or(GateNetId(0));
            let in_f = inputs[0].get(i * 2 + 1).copied().unwrap_or(GateNetId(0));

            let out_t = outputs.get(i * 2).copied().unwrap_or(GateNetId(0));
            let out_f = outputs.get(i * 2 + 1).copied().unwrap_or(GateNetId(0));

            // NOT just swaps: out_t = in_f, out_f = in_t
            let mut cell_t = Cell::new_comb(
                CellId(0),
                buf_info.name.clone(),
                self.library.name.clone(),
                buf_info.fit,
                format!("{}.not_t{}", path, i),
                vec![in_f],
                vec![out_t],
            );
            cell_t.source_op = Some("NclNot_T".to_string());
            self.add_cell(cell_t);

            let mut cell_f = Cell::new_comb(
                CellId(0),
                buf_info.name.clone(),
                self.library.name.clone(),
                buf_info.fit,
                format!("{}.not_f{}", path, i),
                vec![in_t],
                vec![out_f],
            );
            cell_f.source_op = Some("NclNot_F".to_string());
            self.add_cell(cell_f);
        }

        self.stats.decomposed_mappings += 1;
    }

    /// Map NCL adder using NCL full-adder cells
    /// Accepts 4 inputs: a_t, a_f, b_t, b_f (separate dual-rail signals)
    fn map_ncl_add(
        &mut self,
        width: u32,
        inputs: &[Vec<GateNetId>],
        outputs: &[GateNetId],
        path: &str,
    ) {
        if inputs.len() < 4 {
            self.warnings
                .push("NCL Add needs 4 inputs (a_t, a_f, b_t, b_f)".to_string());
            return;
        }

        // NCL adder: ripple-carry chain using NCL half/full-adders
        // For simplicity, we build from NCL AND, OR, XOR gates

        let th12_info = self.get_ncl_cell_info(&CellFunction::Th12);
        let th22_info = self.get_ncl_cell_info(&CellFunction::Th22);
        let th23_info = self.get_ncl_cell_info(&CellFunction::Th23);
        let _ = th23_info; // Suppress unused warning

        // Initialize carry to 0 (NULL initially, but DATA_FALSE after encoding)
        // For first bit, carry_in is 0 = (t=0, f=1)
        let tie_low = self.get_tie_low();
        let tie_high = self.get_tie_high();

        // Carry chain (dual-rail): start with carry = 0 (t=low, f=high)
        let mut carry_t = tie_low;
        let mut carry_f = tie_high;

        for i in 0..width as usize {
            // inputs[0] = a_t (all true rails)
            // inputs[1] = a_f (all false rails)
            // inputs[2] = b_t (all true rails)
            // inputs[3] = b_f (all false rails)
            //
            // For NCL zero-extension: bits beyond input width are logical 0
            // In dual-rail encoding: 0 = (t=0, f=1) = (TIE_LOW, TIE_HIGH)
            let a_t = inputs[0].get(i).copied().unwrap_or(tie_low);
            let a_f = inputs[1].get(i).copied().unwrap_or(tie_high);
            let b_t = inputs[2].get(i).copied().unwrap_or(tie_low);
            let b_f = inputs[3].get(i).copied().unwrap_or(tie_high);

            // outputs are interleaved: [sum_t[0], sum_f[0], sum_t[1], sum_f[1], ...]
            let sum_t = outputs.get(i * 2).copied().unwrap_or(GateNetId(0));
            let sum_f = outputs.get(i * 2 + 1).copied().unwrap_or(GateNetId(0));

            // NCL full adder:
            // sum = a XOR b XOR cin
            // cout = (a AND b) OR (a AND cin) OR (b AND cin) -- majority

            // Create intermediate nets for XOR stages
            let xor_ab_t = self.alloc_net_id();
            self.netlist.add_net(GateNet::new(
                xor_ab_t,
                format!("{}.add_xor_ab_t{}", path, i),
            ));
            let xor_ab_f = self.alloc_net_id();
            self.netlist.add_net(GateNet::new(
                xor_ab_f,
                format!("{}.add_xor_ab_f{}", path, i),
            ));

            // XOR(a, b) for sum partial
            // Using simplified: build NCL XOR inline
            let at_bf = self.alloc_net_id();
            self.netlist
                .add_net(GateNet::new(at_bf, format!("{}.add_at_bf{}", path, i)));
            let af_bt = self.alloc_net_id();
            self.netlist
                .add_net(GateNet::new(af_bt, format!("{}.add_af_bt{}", path, i)));
            let at_bt = self.alloc_net_id();
            self.netlist
                .add_net(GateNet::new(at_bt, format!("{}.add_at_bt{}", path, i)));
            let af_bf = self.alloc_net_id();
            self.netlist
                .add_net(GateNet::new(af_bf, format!("{}.add_af_bf{}", path, i)));

            // Build XOR(a,b)
            self.add_cell(Cell::new_comb(
                CellId(0),
                th12_info.name.clone(),
                self.library.name.clone(),
                th12_info.fit,
                format!("{}.add_xor1_{}", path, i),
                vec![a_t, b_f],
                vec![at_bf],
            ));
            self.add_cell(Cell::new_comb(
                CellId(0),
                th12_info.name.clone(),
                self.library.name.clone(),
                th12_info.fit,
                format!("{}.add_xor2_{}", path, i),
                vec![a_f, b_t],
                vec![af_bt],
            ));
            self.add_cell(Cell::new_comb(
                CellId(0),
                th22_info.name.clone(),
                self.library.name.clone(),
                th22_info.fit,
                format!("{}.add_xorab_t{}", path, i),
                vec![at_bf, af_bt],
                vec![xor_ab_t],
            ));
            self.add_cell(Cell::new_comb(
                CellId(0),
                th12_info.name.clone(),
                self.library.name.clone(),
                th12_info.fit,
                format!("{}.add_xor3_{}", path, i),
                vec![a_t, b_t],
                vec![at_bt],
            ));
            self.add_cell(Cell::new_comb(
                CellId(0),
                th12_info.name.clone(),
                self.library.name.clone(),
                th12_info.fit,
                format!("{}.add_xor4_{}", path, i),
                vec![a_f, b_f],
                vec![af_bf],
            ));
            self.add_cell(Cell::new_comb(
                CellId(0),
                th22_info.name.clone(),
                self.library.name.clone(),
                th22_info.fit,
                format!("{}.add_xorab_f{}", path, i),
                vec![at_bt, af_bf],
                vec![xor_ab_f],
            ));

            // XOR(xor_ab, cin) for sum
            let xab_t_cf = self.alloc_net_id();
            self.netlist.add_net(GateNet::new(
                xab_t_cf,
                format!("{}.add_xab_t_cf{}", path, i),
            ));
            let xab_f_ct = self.alloc_net_id();
            self.netlist.add_net(GateNet::new(
                xab_f_ct,
                format!("{}.add_xab_f_ct{}", path, i),
            ));
            let xab_t_ct = self.alloc_net_id();
            self.netlist.add_net(GateNet::new(
                xab_t_ct,
                format!("{}.add_xab_t_ct{}", path, i),
            ));
            let xab_f_cf = self.alloc_net_id();
            self.netlist.add_net(GateNet::new(
                xab_f_cf,
                format!("{}.add_xab_f_cf{}", path, i),
            ));

            self.add_cell(Cell::new_comb(
                CellId(0),
                th12_info.name.clone(),
                self.library.name.clone(),
                th12_info.fit,
                format!("{}.add_sumxor1_{}", path, i),
                vec![xor_ab_t, carry_f],
                vec![xab_t_cf],
            ));
            self.add_cell(Cell::new_comb(
                CellId(0),
                th12_info.name.clone(),
                self.library.name.clone(),
                th12_info.fit,
                format!("{}.add_sumxor2_{}", path, i),
                vec![xor_ab_f, carry_t],
                vec![xab_f_ct],
            ));
            self.add_cell(Cell::new_comb(
                CellId(0),
                th22_info.name.clone(),
                self.library.name.clone(),
                th22_info.fit,
                format!("{}.add_sum_t{}", path, i),
                vec![xab_t_cf, xab_f_ct],
                vec![sum_t],
            ));
            self.add_cell(Cell::new_comb(
                CellId(0),
                th12_info.name.clone(),
                self.library.name.clone(),
                th12_info.fit,
                format!("{}.add_sumxor3_{}", path, i),
                vec![xor_ab_t, carry_t],
                vec![xab_t_ct],
            ));
            self.add_cell(Cell::new_comb(
                CellId(0),
                th12_info.name.clone(),
                self.library.name.clone(),
                th12_info.fit,
                format!("{}.add_sumxor4_{}", path, i),
                vec![xor_ab_f, carry_f],
                vec![xab_f_cf],
            ));
            self.add_cell(Cell::new_comb(
                CellId(0),
                th22_info.name.clone(),
                self.library.name.clone(),
                th22_info.fit,
                format!("{}.add_sum_f{}", path, i),
                vec![xab_t_ct, xab_f_cf],
                vec![sum_f],
            ));

            // Carry out: majority(a, b, cin)
            // cout_t = TH23(a_t, b_t, cin_t)
            // cout_f = TH23(a_f, b_f, cin_f)
            if i < (width as usize - 1) {
                let new_carry_t = self.alloc_net_id();
                self.netlist.add_net(GateNet::new(
                    new_carry_t,
                    format!("{}.add_cout_t{}", path, i),
                ));
                let new_carry_f = self.alloc_net_id();
                self.netlist.add_net(GateNet::new(
                    new_carry_f,
                    format!("{}.add_cout_f{}", path, i),
                ));

                self.add_cell(Cell::new_comb(
                    CellId(0),
                    th23_info.name.clone(),
                    self.library.name.clone(),
                    th23_info.fit,
                    format!("{}.add_cout_t{}", path, i),
                    vec![a_t, b_t, carry_t],
                    vec![new_carry_t],
                ));
                self.add_cell(Cell::new_comb(
                    CellId(0),
                    th23_info.name.clone(),
                    self.library.name.clone(),
                    th23_info.fit,
                    format!("{}.add_cout_f{}", path, i),
                    vec![a_f, b_f, carry_f],
                    vec![new_carry_f],
                ));

                carry_t = new_carry_t;
                carry_f = new_carry_f;
            }
        }

        self.stats.decomposed_mappings += 1;
    }

    /// Map NCL subtractor: a - b = a + (~b) + 1
    /// Map NCL subtractor using ripple-borrow chain
    /// Accepts 4 inputs: a_t, a_f, b_t, b_f (b is already inverted by caller)
    /// Uses carry_in = 1 for two's complement subtraction
    fn map_ncl_sub(
        &mut self,
        width: u32,
        inputs: &[Vec<GateNetId>],
        outputs: &[GateNetId],
        path: &str,
    ) {
        if inputs.len() < 4 {
            self.warnings
                .push("NCL Sub needs 4 inputs (a_t, a_f, b_inverted_t, b_inverted_f)".to_string());
            return;
        }

        // NCL subtractor: a - b = a + (~b) + 1
        // The caller (ncl_expand) swaps b's rails, so inputs are:
        // inputs[0] = a_t, inputs[1] = a_f, inputs[2] = ~b_t (= b_f), inputs[3] = ~b_f (= b_t)

        let th12_info = self.get_ncl_cell_info(&CellFunction::Th12);
        let th22_info = self.get_ncl_cell_info(&CellFunction::Th22);

        // Initialize carry to 1 (for two's complement): t=high, f=low
        let tie_low = self.get_tie_low();
        let tie_high = self.get_tie_high();

        let mut carry_t = tie_high; // Start with carry = 1
        let mut carry_f = tie_low;

        for i in 0..width as usize {
            // For NCL zero-extension: bits beyond input width are logical 0
            // In dual-rail encoding: 0 = (t=0, f=1) = (TIE_LOW, TIE_HIGH)
            let a_t = inputs[0].get(i).copied().unwrap_or(tie_low);
            let a_f = inputs[1].get(i).copied().unwrap_or(tie_high);
            // For b: inputs[2] = ~b_t = b_f, inputs[3] = ~b_f = b_t (already inverted)
            // When original b beyond width is 0: b_t=0, b_f=1
            // So: ~b_t = b_f = 1 (TIE_HIGH), ~b_f = b_t = 0 (TIE_LOW)
            let b_t = inputs[2].get(i).copied().unwrap_or(tie_high); // ~b_t = 1 for zero
            let b_f = inputs[3].get(i).copied().unwrap_or(tie_low); // ~b_f = 0 for zero

            let diff_t = outputs.get(i * 2).copied().unwrap_or(GateNetId(0));
            let diff_f = outputs.get(i * 2 + 1).copied().unwrap_or(GateNetId(0));

            // Build XOR(a, b) intermediate nets
            let xor_ab_t = self.alloc_net_id();
            self.netlist.add_net(GateNet::new(
                xor_ab_t,
                format!("{}.sub_xor_ab_t{}", path, i),
            ));
            let xor_ab_f = self.alloc_net_id();
            self.netlist.add_net(GateNet::new(
                xor_ab_f,
                format!("{}.sub_xor_ab_f{}", path, i),
            ));

            let at_bf = self.alloc_net_id();
            self.netlist
                .add_net(GateNet::new(at_bf, format!("{}.sub_at_bf{}", path, i)));
            let af_bt = self.alloc_net_id();
            self.netlist
                .add_net(GateNet::new(af_bt, format!("{}.sub_af_bt{}", path, i)));
            let at_bt = self.alloc_net_id();
            self.netlist
                .add_net(GateNet::new(at_bt, format!("{}.sub_at_bt{}", path, i)));
            let af_bf = self.alloc_net_id();
            self.netlist
                .add_net(GateNet::new(af_bf, format!("{}.sub_af_bf{}", path, i)));

            // Build XOR(a, b) = t: TH12(TH22(a_t,b_f), TH22(a_f,b_t))
            self.add_cell(Cell::new_comb(
                CellId(0),
                th22_info.name.clone(),
                self.library.name.clone(),
                th22_info.fit,
                format!("{}.sub_xor1_{}", path, i),
                vec![a_t, b_f],
                vec![at_bf],
            ));
            self.add_cell(Cell::new_comb(
                CellId(0),
                th22_info.name.clone(),
                self.library.name.clone(),
                th22_info.fit,
                format!("{}.sub_xor2_{}", path, i),
                vec![a_f, b_t],
                vec![af_bt],
            ));
            self.add_cell(Cell::new_comb(
                CellId(0),
                th12_info.name.clone(),
                self.library.name.clone(),
                th12_info.fit,
                format!("{}.sub_xor_t_{}", path, i),
                vec![at_bf, af_bt],
                vec![xor_ab_t],
            ));

            // XOR false rail
            self.add_cell(Cell::new_comb(
                CellId(0),
                th22_info.name.clone(),
                self.library.name.clone(),
                th22_info.fit,
                format!("{}.sub_xor3_{}", path, i),
                vec![a_t, b_t],
                vec![at_bt],
            ));
            self.add_cell(Cell::new_comb(
                CellId(0),
                th22_info.name.clone(),
                self.library.name.clone(),
                th22_info.fit,
                format!("{}.sub_xor4_{}", path, i),
                vec![a_f, b_f],
                vec![af_bf],
            ));
            self.add_cell(Cell::new_comb(
                CellId(0),
                th12_info.name.clone(),
                self.library.name.clone(),
                th12_info.fit,
                format!("{}.sub_xor_f_{}", path, i),
                vec![at_bt, af_bf],
                vec![xor_ab_f],
            ));

            // Build XOR(xor_ab, carry) for diff
            let xor_ct = self.alloc_net_id();
            self.netlist
                .add_net(GateNet::new(xor_ct, format!("{}.sub_xor_ct{}", path, i)));
            let xor_cf = self.alloc_net_id();
            self.netlist
                .add_net(GateNet::new(xor_cf, format!("{}.sub_xor_cf{}", path, i)));

            self.add_cell(Cell::new_comb(
                CellId(0),
                th22_info.name.clone(),
                self.library.name.clone(),
                th22_info.fit,
                format!("{}.sub_sum1_{}", path, i),
                vec![xor_ab_t, carry_f],
                vec![xor_ct],
            ));
            self.add_cell(Cell::new_comb(
                CellId(0),
                th22_info.name.clone(),
                self.library.name.clone(),
                th22_info.fit,
                format!("{}.sub_sum2_{}", path, i),
                vec![xor_ab_f, carry_t],
                vec![xor_cf],
            ));
            self.add_cell(Cell::new_comb(
                CellId(0),
                th12_info.name.clone(),
                self.library.name.clone(),
                th12_info.fit,
                format!("{}.sub_diff_t_{}", path, i),
                vec![xor_ct, xor_cf],
                vec![diff_t],
            ));

            // diff_f
            let xor_dt = self.alloc_net_id();
            self.netlist
                .add_net(GateNet::new(xor_dt, format!("{}.sub_xor_dt{}", path, i)));
            let xor_df = self.alloc_net_id();
            self.netlist
                .add_net(GateNet::new(xor_df, format!("{}.sub_xor_df{}", path, i)));

            self.add_cell(Cell::new_comb(
                CellId(0),
                th22_info.name.clone(),
                self.library.name.clone(),
                th22_info.fit,
                format!("{}.sub_sum3_{}", path, i),
                vec![xor_ab_t, carry_t],
                vec![xor_dt],
            ));
            self.add_cell(Cell::new_comb(
                CellId(0),
                th22_info.name.clone(),
                self.library.name.clone(),
                th22_info.fit,
                format!("{}.sub_sum4_{}", path, i),
                vec![xor_ab_f, carry_f],
                vec![xor_df],
            ));
            self.add_cell(Cell::new_comb(
                CellId(0),
                th12_info.name.clone(),
                self.library.name.clone(),
                th12_info.fit,
                format!("{}.sub_diff_f_{}", path, i),
                vec![xor_dt, xor_df],
                vec![diff_f],
            ));

            // Carry out = MAJ(a, b, cin) for next bit
            let and_ab_t = self.alloc_net_id();
            self.netlist.add_net(GateNet::new(
                and_ab_t,
                format!("{}.sub_and_ab_t{}", path, i),
            ));
            let and_ab_f = self.alloc_net_id();
            self.netlist.add_net(GateNet::new(
                and_ab_f,
                format!("{}.sub_and_ab_f{}", path, i),
            ));
            let and_bc_t = self.alloc_net_id();
            self.netlist.add_net(GateNet::new(
                and_bc_t,
                format!("{}.sub_and_bc_t{}", path, i),
            ));
            let and_bc_f = self.alloc_net_id();
            self.netlist.add_net(GateNet::new(
                and_bc_f,
                format!("{}.sub_and_bc_f{}", path, i),
            ));
            let and_ac_t = self.alloc_net_id();
            self.netlist.add_net(GateNet::new(
                and_ac_t,
                format!("{}.sub_and_ac_t{}", path, i),
            ));
            let and_ac_f = self.alloc_net_id();
            self.netlist.add_net(GateNet::new(
                and_ac_f,
                format!("{}.sub_and_ac_f{}", path, i),
            ));

            // AND(a, b)
            self.add_cell(Cell::new_comb(
                CellId(0),
                th22_info.name.clone(),
                self.library.name.clone(),
                th22_info.fit,
                format!("{}.sub_and_ab_t_{}", path, i),
                vec![a_t, b_t],
                vec![and_ab_t],
            ));
            self.add_cell(Cell::new_comb(
                CellId(0),
                th12_info.name.clone(),
                self.library.name.clone(),
                th12_info.fit,
                format!("{}.sub_and_ab_f_{}", path, i),
                vec![a_f, b_f],
                vec![and_ab_f],
            ));

            // AND(b, cin)
            self.add_cell(Cell::new_comb(
                CellId(0),
                th22_info.name.clone(),
                self.library.name.clone(),
                th22_info.fit,
                format!("{}.sub_and_bc_t_{}", path, i),
                vec![b_t, carry_t],
                vec![and_bc_t],
            ));
            self.add_cell(Cell::new_comb(
                CellId(0),
                th12_info.name.clone(),
                self.library.name.clone(),
                th12_info.fit,
                format!("{}.sub_and_bc_f_{}", path, i),
                vec![b_f, carry_f],
                vec![and_bc_f],
            ));

            // AND(a, cin)
            self.add_cell(Cell::new_comb(
                CellId(0),
                th22_info.name.clone(),
                self.library.name.clone(),
                th22_info.fit,
                format!("{}.sub_and_ac_t_{}", path, i),
                vec![a_t, carry_t],
                vec![and_ac_t],
            ));
            self.add_cell(Cell::new_comb(
                CellId(0),
                th12_info.name.clone(),
                self.library.name.clone(),
                th12_info.fit,
                format!("{}.sub_and_ac_f_{}", path, i),
                vec![a_f, carry_f],
                vec![and_ac_f],
            ));

            // OR of all three ANDs for carry
            let or_ab_bc_t = self.alloc_net_id();
            self.netlist.add_net(GateNet::new(
                or_ab_bc_t,
                format!("{}.sub_or_ab_bc_t{}", path, i),
            ));
            let or_ab_bc_f = self.alloc_net_id();
            self.netlist.add_net(GateNet::new(
                or_ab_bc_f,
                format!("{}.sub_or_ab_bc_f{}", path, i),
            ));

            self.add_cell(Cell::new_comb(
                CellId(0),
                th12_info.name.clone(),
                self.library.name.clone(),
                th12_info.fit,
                format!("{}.sub_or1_{}", path, i),
                vec![and_ab_t, and_bc_t],
                vec![or_ab_bc_t],
            ));
            self.add_cell(Cell::new_comb(
                CellId(0),
                th22_info.name.clone(),
                self.library.name.clone(),
                th22_info.fit,
                format!("{}.sub_or1f_{}", path, i),
                vec![and_ab_f, and_bc_f],
                vec![or_ab_bc_f],
            ));

            // New carry
            let new_carry_t = self.alloc_net_id();
            self.netlist.add_net(GateNet::new(
                new_carry_t,
                format!("{}.sub_cout_t{}", path, i),
            ));
            let new_carry_f = self.alloc_net_id();
            self.netlist.add_net(GateNet::new(
                new_carry_f,
                format!("{}.sub_cout_f{}", path, i),
            ));

            self.add_cell(Cell::new_comb(
                CellId(0),
                th12_info.name.clone(),
                self.library.name.clone(),
                th12_info.fit,
                format!("{}.sub_cout_t_{}", path, i),
                vec![or_ab_bc_t, and_ac_t],
                vec![new_carry_t],
            ));
            self.add_cell(Cell::new_comb(
                CellId(0),
                th22_info.name.clone(),
                self.library.name.clone(),
                th22_info.fit,
                format!("{}.sub_cout_f_{}", path, i),
                vec![or_ab_bc_f, and_ac_f],
                vec![new_carry_f],
            ));

            carry_t = new_carry_t;
            carry_f = new_carry_f;
        }

        self.stats.decomposed_mappings += 1;
    }

    /// Map NCL multiplier with proper NULL/DATA handling
    ///
    /// NCL Mul expects 4 inputs: a_t, a_f, b_t, b_f (each width bits)
    /// Output is 2*width bits interleaved: [t0, f0, t1, f1, ...]
    ///
    /// Key NCL property: during NULL phase (all inputs t=f=0), output must also be NULL.
    /// During DATA phase, output encodes the product value.
    ///
    /// Implementation approach:
    /// 1. Compute product_value = a_t * b_t (correct in DATA phase since true rail = value)
    /// 2. Compute input completion = all input bits have exactly one of (t,f) set
    /// 3. Output: out_t[i] = product[i] AND completion, out_f[i] = NOT(product[i]) AND completion
    ///
    /// This ensures NULL propagation: when inputs are NULL, completion=0, so output is NULL.
    fn map_ncl_mul(
        &mut self,
        input_width: u32,
        result_width: u32,
        inputs: &[Vec<GateNetId>],
        outputs: &[GateNetId],
        path: &str,
    ) {
        if inputs.len() < 4 {
            self.warnings.push(format!(
                "NCL Mul at {} needs 4 inputs (a_t, a_f, b_t, b_f), got {}",
                path,
                inputs.len()
            ));
            return;
        }

        let a_t = &inputs[0];
        let a_f = &inputs[1];
        let b_t = &inputs[2];
        let b_f = &inputs[3];

        let and_info = self.get_cell_info(&CellFunction::And2);
        let inv_info = self.get_cell_info(&CellFunction::Inv);
        let xor_info = self.get_cell_info(&CellFunction::Xor2);

        // Step 1: Compute multiplication on true rails (gives correct value in DATA phase)
        // Create intermediate nets for product result (result_width bits)
        let mut product_nets = Vec::with_capacity(result_width as usize);
        for i in 0..result_width as usize {
            let net_id = self.alloc_net_id();
            self.netlist
                .add_net(GateNet::new(net_id, format!("{}.mul_prod{}", path, i)));
            product_nets.push(net_id);
        }

        // Use existing map_multiplier to compute a_t * b_t
        // Need to collect the true rails as input vectors (input_width bits each)
        let mul_input_a: Vec<GateNetId> = (0..input_width as usize)
            .map(|i| a_t.get(i).copied().unwrap_or(GateNetId(0)))
            .collect();
        let mul_input_b: Vec<GateNetId> = (0..input_width as usize)
            .map(|i| b_t.get(i).copied().unwrap_or(GateNetId(0)))
            .collect();

        // Call existing multiplier implementation
        // input_width x input_width -> result_width output
        // Note: NCL multiplication uses unsigned arithmetic on true rails
        self.map_multiplier(
            input_width,
            result_width,
            false, // NCL multiplication is unsigned
            &[mul_input_a, mul_input_b],
            &product_nets,
            &format!("{}.mul_core", path),
        );

        // Step 2: Compute completion detection for all input bits
        // A bit is complete when exactly one of (t, f) is 1: completion[i] = XOR(t[i], f[i])
        // Overall completion = AND of all per-bit completions
        let mut completion_bits_a = Vec::with_capacity(input_width as usize);
        let mut completion_bits_b = Vec::with_capacity(input_width as usize);

        for i in 0..input_width as usize {
            // a completion bit
            let a_comp = self.alloc_net_id();
            self.netlist
                .add_net(GateNet::new(a_comp, format!("{}.mul_a_comp{}", path, i)));
            self.add_cell(Cell::new_comb(
                CellId(0),
                xor_info.name.clone(),
                self.library.name.clone(),
                xor_info.fit,
                format!("{}.mul_a_comp_xor{}", path, i),
                vec![
                    a_t.get(i).copied().unwrap_or(GateNetId(0)),
                    a_f.get(i).copied().unwrap_or(GateNetId(0)),
                ],
                vec![a_comp],
            ));
            completion_bits_a.push(a_comp);

            // b completion bit
            let b_comp = self.alloc_net_id();
            self.netlist
                .add_net(GateNet::new(b_comp, format!("{}.mul_b_comp{}", path, i)));
            self.add_cell(Cell::new_comb(
                CellId(0),
                xor_info.name.clone(),
                self.library.name.clone(),
                xor_info.fit,
                format!("{}.mul_b_comp_xor{}", path, i),
                vec![
                    b_t.get(i).copied().unwrap_or(GateNetId(0)),
                    b_f.get(i).copied().unwrap_or(GateNetId(0)),
                ],
                vec![b_comp],
            ));
            completion_bits_b.push(b_comp);
        }

        // AND tree for overall completion
        // Combine all completion bits into one signal
        let mut all_comp_bits: Vec<GateNetId> = completion_bits_a;
        all_comp_bits.extend(completion_bits_b);

        // Build AND tree
        let input_completion = self.build_and_tree(&all_comp_bits, &and_info, path, "mul_comp");

        // Step 3: Add delay chain to completion signal
        // The false rail should not go high until the product has stabilized.
        // Add buffers to delay completion by the multiplier depth (~2*result_width).
        // This prevents glitches where out_f is high before product is computed.
        let buf_info = self.get_cell_info(&CellFunction::Buf);
        let delay_depth = (result_width * 2).max(8) as usize; // At least 8, typically 2*result_width

        let mut delayed_completion = input_completion;
        for d in 0..delay_depth {
            let delay_net = self.alloc_net_id();
            self.netlist.add_net(GateNet::new(
                delay_net,
                format!("{}.mul_comp_delay{}", path, d),
            ));
            self.add_cell(Cell::new_comb(
                CellId(0),
                buf_info.name.clone(),
                self.library.name.clone(),
                buf_info.fit,
                format!("{}.mul_comp_buf{}", path, d),
                vec![delayed_completion],
                vec![delay_net],
            ));
            delayed_completion = delay_net;
        }

        // Use delayed completion for gating outputs
        let completion = delayed_completion;

        // Step 4: Gate outputs with completion
        // out_t[i] = product[i] AND completion
        // out_f[i] = NOT(product[i]) AND completion
        for i in 0..result_width as usize {
            let out_t = outputs.get(i * 2).copied().unwrap_or(GateNetId(0));
            let out_f = outputs.get(i * 2 + 1).copied().unwrap_or(GateNetId(0));
            let prod_bit = product_nets.get(i).copied().unwrap_or(GateNetId(0));

            if out_t.0 != 0 {
                // out_t = prod AND completion
                self.add_cell(Cell::new_comb(
                    CellId(0),
                    and_info.name.clone(),
                    self.library.name.clone(),
                    and_info.fit,
                    format!("{}.mul_out_t{}", path, i),
                    vec![prod_bit, completion],
                    vec![out_t],
                ));
            }

            if out_f.0 != 0 {
                // out_f = NOT(prod) AND completion
                let not_prod = self.alloc_net_id();
                self.netlist.add_net(GateNet::new(
                    not_prod,
                    format!("{}.mul_not_prod{}", path, i),
                ));
                self.add_cell(Cell::new_comb(
                    CellId(0),
                    inv_info.name.clone(),
                    self.library.name.clone(),
                    inv_info.fit,
                    format!("{}.mul_not{}", path, i),
                    vec![prod_bit],
                    vec![not_prod],
                ));
                self.add_cell(Cell::new_comb(
                    CellId(0),
                    and_info.name.clone(),
                    self.library.name.clone(),
                    and_info.fit,
                    format!("{}.mul_out_f{}", path, i),
                    vec![not_prod, completion],
                    vec![out_f],
                ));
            }
        }

        self.stats.decomposed_mappings += 1;
    }

    /// Build an AND tree from a list of input nets, returning the final AND result
    fn build_and_tree(
        &mut self,
        inputs: &[GateNetId],
        and_info: &LibraryCellInfo,
        path: &str,
        prefix: &str,
    ) -> GateNetId {
        if inputs.is_empty() {
            return self.get_tie_high();
        }
        if inputs.len() == 1 {
            return inputs[0];
        }

        let mut current_level = inputs.to_vec();
        let mut level = 0;

        while current_level.len() > 1 {
            let mut next_level = Vec::new();
            let mut i = 0;

            while i < current_level.len() {
                if i + 1 < current_level.len() {
                    // Pair two inputs
                    let and_out = self.alloc_net_id();
                    self.netlist.add_net(GateNet::new(
                        and_out,
                        format!("{}.{}_{}_and{}", path, prefix, level, i / 2),
                    ));
                    self.add_cell(Cell::new_comb(
                        CellId(0),
                        and_info.name.clone(),
                        self.library.name.clone(),
                        and_info.fit,
                        format!("{}.{}_{}_and{}", path, prefix, level, i / 2),
                        vec![current_level[i], current_level[i + 1]],
                        vec![and_out],
                    ));
                    next_level.push(and_out);
                    i += 2;
                } else {
                    // Odd one out, pass through
                    next_level.push(current_level[i]);
                    i += 1;
                }
            }

            current_level = next_level;
            level += 1;
        }

        current_level[0]
    }

    /// Map NCL less-than comparator
    fn map_ncl_lt(
        &mut self,
        width: u32,
        inputs: &[Vec<GateNetId>],
        outputs: &[GateNetId],
        path: &str,
    ) {
        // NCL Lt expects 4 inputs: a_t, a_f, b_t, b_f (each width bits)
        // Output is 2 bits: [lt_t, lt_f] interleaved
        if inputs.len() < 4 {
            self.warnings.push(format!(
                "NCL Lt at {} needs 4 inputs (a_t, a_f, b_t, b_f), got {}",
                path,
                inputs.len()
            ));
            return;
        }

        let a_t = &inputs[0]; // True rail of a
        let a_f = &inputs[1]; // False rail of a
        let b_t = &inputs[2]; // True rail of b
        let b_f = &inputs[3]; // False rail of b

        let out_t = outputs.first().copied().unwrap_or(GateNetId(0));
        let out_f = outputs.get(1).copied().unwrap_or(GateNetId(0));

        let th22_info = self.get_ncl_cell_info(&CellFunction::Th22);
        let th12_info = self.get_ncl_cell_info(&CellFunction::Th12);

        // NCL magnitude comparator: iterative from MSB to LSB
        // For each bit position i:
        //   lt_i = (a[i] < b[i]) OR ((a[i] == b[i]) AND lt_prev)
        //   ge_i = (a[i] > b[i]) OR ((a[i] == b[i]) AND ge_prev)
        //
        // In NCL dual-rail:
        //   a[i] < b[i]  means a_f[i] AND b_t[i]
        //   a[i] > b[i]  means a_t[i] AND b_f[i]
        //   a[i] == b[i] means (a_t[i] AND b_t[i]) OR (a_f[i] AND b_f[i])

        // Initialize: start from MSB
        // lt_accum_t = 0 (not less than yet)
        // lt_accum_f = 0 (not determined yet - both rails 0 is NULL, but we're in DATA)
        // Actually in NCL, we need completion detection, but for now use simpler approach:
        // Just compute lt on true rails and ge on false rails

        // For simplicity, use the true rails to compute regular Lt, then
        // compute completion to gate the output.

        // Step 1: Compute completion detection for inputs (all bits have valid data)
        // A bit is "complete" when exactly one of (t, f) is 1
        // For now, assume inputs are already valid DATA.

        // Step 2: Compute less-than using iterative magnitude comparison
        // Build the chain from MSB (index width-1) to LSB (index 0)

        let tie_low = self.get_tie_low();
        let tie_high = self.get_tie_high();

        // Magnitude comparator from MSB to LSB using equality chain approach:
        // - eq_chain: 1 while all bits so far are equal, 0 once difference found
        // - lt: 1 if (first differing bit had a < b), latches forever once set
        // - ge: 1 if (first differing bit had a > b), latches forever once set
        //
        // Recurrence:
        //   eq_chain_new = eq_chain_prev AND a_eq_b
        //   lt_new = lt_prev OR (eq_chain_prev AND a_lt_b)
        //   ge_new = ge_prev OR (eq_chain_prev AND a_gt_b)

        let mut eq_chain_prev = tie_high; // Start with "all equal so far" = true
        let mut lt_t_prev = tie_low; // Not less than yet
        let mut ge_t_prev = tie_low; // Not greater than yet

        for i in (0..width as usize).rev() {
            // Get dual-rail bits for this position
            let ai_t = a_t.get(i).copied().unwrap_or(tie_low);
            let ai_f = a_f.get(i).copied().unwrap_or(tie_low);
            let bi_t = b_t.get(i).copied().unwrap_or(tie_low);
            let bi_f = b_f.get(i).copied().unwrap_or(tie_low);

            // a[i] < b[i] in NCL: a_f AND b_t (a is 0, b is 1)
            let a_lt_b = self.alloc_net_id();
            self.netlist
                .add_net(GateNet::new(a_lt_b, format!("{}.lt_bit{}_altb", path, i)));
            self.add_cell(Cell::new_comb(
                CellId(0),
                th22_info.name.clone(),
                self.library.name.clone(),
                th22_info.fit,
                format!("{}.lt_bit{}_th22_altb", path, i),
                vec![ai_f, bi_t],
                vec![a_lt_b],
            ));

            // a[i] > b[i] in NCL: a_t AND b_f (a is 1, b is 0)
            let a_gt_b = self.alloc_net_id();
            self.netlist
                .add_net(GateNet::new(a_gt_b, format!("{}.lt_bit{}_agtb", path, i)));
            self.add_cell(Cell::new_comb(
                CellId(0),
                th22_info.name.clone(),
                self.library.name.clone(),
                th22_info.fit,
                format!("{}.lt_bit{}_th22_agtb", path, i),
                vec![ai_t, bi_f],
                vec![a_gt_b],
            ));

            // a[i] == b[i] in NCL: (a_t AND b_t) OR (a_f AND b_f)
            let both_true = self.alloc_net_id();
            self.netlist.add_net(GateNet::new(
                both_true,
                format!("{}.lt_bit{}_both_t", path, i),
            ));
            let both_false = self.alloc_net_id();
            self.netlist.add_net(GateNet::new(
                both_false,
                format!("{}.lt_bit{}_both_f", path, i),
            ));
            self.add_cell(Cell::new_comb(
                CellId(0),
                th22_info.name.clone(),
                self.library.name.clone(),
                th22_info.fit,
                format!("{}.lt_bit{}_th22_both_t", path, i),
                vec![ai_t, bi_t],
                vec![both_true],
            ));
            self.add_cell(Cell::new_comb(
                CellId(0),
                th22_info.name.clone(),
                self.library.name.clone(),
                th22_info.fit,
                format!("{}.lt_bit{}_th22_both_f", path, i),
                vec![ai_f, bi_f],
                vec![both_false],
            ));
            let a_eq_b = self.alloc_net_id();
            self.netlist
                .add_net(GateNet::new(a_eq_b, format!("{}.lt_bit{}_aeqb", path, i)));
            self.add_cell(Cell::new_comb(
                CellId(0),
                th12_info.name.clone(),
                self.library.name.clone(),
                th12_info.fit,
                format!("{}.lt_bit{}_th12_aeqb", path, i),
                vec![both_true, both_false],
                vec![a_eq_b],
            ));

            // eq_chain_new = eq_chain_prev AND a_eq_b
            let eq_chain_new = self.alloc_net_id();
            self.netlist.add_net(GateNet::new(
                eq_chain_new,
                format!("{}.lt_bit{}_eq_chain", path, i),
            ));
            self.add_cell(Cell::new_comb(
                CellId(0),
                th22_info.name.clone(),
                self.library.name.clone(),
                th22_info.fit,
                format!("{}.lt_bit{}_th22_eq_chain", path, i),
                vec![eq_chain_prev, a_eq_b],
                vec![eq_chain_new],
            ));

            // lt_new = lt_prev OR (eq_chain_prev AND a_lt_b)
            let eq_and_lt = self.alloc_net_id();
            self.netlist.add_net(GateNet::new(
                eq_and_lt,
                format!("{}.lt_bit{}_eq_and_lt", path, i),
            ));
            self.add_cell(Cell::new_comb(
                CellId(0),
                th22_info.name.clone(),
                self.library.name.clone(),
                th22_info.fit,
                format!("{}.lt_bit{}_th22_eq_lt", path, i),
                vec![eq_chain_prev, a_lt_b],
                vec![eq_and_lt],
            ));
            let lt_new = self.alloc_net_id();
            self.netlist
                .add_net(GateNet::new(lt_new, format!("{}.lt_bit{}_lt_new", path, i)));
            self.add_cell(Cell::new_comb(
                CellId(0),
                th12_info.name.clone(),
                self.library.name.clone(),
                th12_info.fit,
                format!("{}.lt_bit{}_th12_lt_new", path, i),
                vec![lt_t_prev, eq_and_lt],
                vec![lt_new],
            ));

            // ge_new = ge_prev OR (eq_chain_prev AND a_gt_b)
            let eq_and_ge = self.alloc_net_id();
            self.netlist.add_net(GateNet::new(
                eq_and_ge,
                format!("{}.lt_bit{}_eq_and_ge", path, i),
            ));
            self.add_cell(Cell::new_comb(
                CellId(0),
                th22_info.name.clone(),
                self.library.name.clone(),
                th22_info.fit,
                format!("{}.lt_bit{}_th22_eq_ge", path, i),
                vec![eq_chain_prev, a_gt_b],
                vec![eq_and_ge],
            ));
            let ge_new = self.alloc_net_id();
            self.netlist
                .add_net(GateNet::new(ge_new, format!("{}.lt_bit{}_ge_new", path, i)));
            self.add_cell(Cell::new_comb(
                CellId(0),
                th12_info.name.clone(),
                self.library.name.clone(),
                th12_info.fit,
                format!("{}.lt_bit{}_th12_ge_new", path, i),
                vec![ge_t_prev, eq_and_ge],
                vec![ge_new],
            ));

            eq_chain_prev = eq_chain_new;
            lt_t_prev = lt_new;
            ge_t_prev = ge_new;
        }

        // Handle the case where all bits are equal: a == b means NOT (a < b)
        // So if eq_chain is still 1 (all equal), ge should be 1 (not less than)
        // Final: ge_final = ge_t_prev OR eq_chain_prev
        let ge_final = self.alloc_net_id();
        self.netlist
            .add_net(GateNet::new(ge_final, format!("{}.lt_ge_final", path)));
        self.add_cell(Cell::new_comb(
            CellId(0),
            th12_info.name.clone(),
            self.library.name.clone(),
            th12_info.fit,
            format!("{}.lt_th12_ge_final", path),
            vec![ge_t_prev, eq_chain_prev],
            vec![ge_final],
        ));
        let ge_t_prev = ge_final;

        // Final output: lt_t = final lt accumulator, lt_f = final ge accumulator
        let buf_info = self.get_cell_info(&CellFunction::Buf);
        if out_t.0 != 0 {
            self.add_cell(Cell::new_comb(
                CellId(0),
                buf_info.name.clone(),
                self.library.name.clone(),
                buf_info.fit,
                format!("{}.lt_out_t", path),
                vec![lt_t_prev],
                vec![out_t],
            ));
        }
        if out_f.0 != 0 {
            self.add_cell(Cell::new_comb(
                CellId(0),
                buf_info.name.clone(),
                self.library.name.clone(),
                buf_info.fit,
                format!("{}.lt_out_f", path),
                vec![ge_t_prev],
                vec![out_f],
            ));
        }

        self.stats.decomposed_mappings += 1;
    }

    /// Map NCL equality comparator
    fn map_ncl_eq(
        &mut self,
        width: u32,
        inputs: &[Vec<GateNetId>],
        outputs: &[GateNetId],
        path: &str,
    ) {
        if inputs.len() < 2 {
            self.warnings.push("NCL Eq needs 2 inputs".to_string());
            return;
        }

        // NCL equality: XNOR all bit pairs, then AND-reduce
        // XNOR = NOT(XOR) = swap XOR rails

        let th22_info = self.get_ncl_cell_info(&CellFunction::Th22);

        // Build per-bit XNOR (equality) signals
        let mut eq_bits_t = Vec::new();
        let mut eq_bits_f = Vec::new();

        for i in 0..width as usize {
            let a_t = inputs[0].get(i * 2).copied().unwrap_or(GateNetId(0));
            let a_f = inputs[0].get(i * 2 + 1).copied().unwrap_or(GateNetId(0));
            let b_t = inputs[1].get(i * 2).copied().unwrap_or(GateNetId(0));
            let b_f = inputs[1].get(i * 2 + 1).copied().unwrap_or(GateNetId(0));

            // XNOR: equal when both true or both false
            // eq_t = TH22(a_t, b_t) OR TH22(a_f, b_f)
            // eq_f = TH22(a_t, b_f) OR TH22(a_f, b_t)

            let both_t = self.alloc_net_id();
            self.netlist
                .add_net(GateNet::new(both_t, format!("{}.eq_both_t{}", path, i)));
            let both_f = self.alloc_net_id();
            self.netlist
                .add_net(GateNet::new(both_f, format!("{}.eq_both_f{}", path, i)));
            let diff_tf = self.alloc_net_id();
            self.netlist
                .add_net(GateNet::new(diff_tf, format!("{}.eq_diff_tf{}", path, i)));
            let diff_ft = self.alloc_net_id();
            self.netlist
                .add_net(GateNet::new(diff_ft, format!("{}.eq_diff_ft{}", path, i)));

            // TH22(a_t, b_t) -> both_t (both are 1)
            self.add_cell(Cell::new_comb(
                CellId(0),
                th22_info.name.clone(),
                self.library.name.clone(),
                th22_info.fit,
                format!("{}.eq_tt{}", path, i),
                vec![a_t, b_t],
                vec![both_t],
            ));

            // TH22(a_f, b_f) -> both_f (both are 0)
            self.add_cell(Cell::new_comb(
                CellId(0),
                th22_info.name.clone(),
                self.library.name.clone(),
                th22_info.fit,
                format!("{}.eq_ff{}", path, i),
                vec![a_f, b_f],
                vec![both_f],
            ));

            // TH22(a_t, b_f) -> diff_tf (a=1, b=0)
            self.add_cell(Cell::new_comb(
                CellId(0),
                th22_info.name.clone(),
                self.library.name.clone(),
                th22_info.fit,
                format!("{}.eq_tf{}", path, i),
                vec![a_t, b_f],
                vec![diff_tf],
            ));

            // TH22(a_f, b_t) -> diff_ft (a=0, b=1)
            self.add_cell(Cell::new_comb(
                CellId(0),
                th22_info.name.clone(),
                self.library.name.clone(),
                th22_info.fit,
                format!("{}.eq_ft{}", path, i),
                vec![a_f, b_t],
                vec![diff_ft],
            ));

            // eq_bit_t = TH12(both_t, both_f) - equal (both same)
            let eq_bit_t = self.alloc_net_id();
            self.netlist
                .add_net(GateNet::new(eq_bit_t, format!("{}.eq_bit_t{}", path, i)));
            let th12_info = self.get_ncl_cell_info(&CellFunction::Th12);
            self.add_cell(Cell::new_comb(
                CellId(0),
                th12_info.name.clone(),
                self.library.name.clone(),
                th12_info.fit,
                format!("{}.eq_or_t{}", path, i),
                vec![both_t, both_f],
                vec![eq_bit_t],
            ));

            // eq_bit_f = TH12(diff_tf, diff_ft) - not equal (different)
            let eq_bit_f = self.alloc_net_id();
            self.netlist
                .add_net(GateNet::new(eq_bit_f, format!("{}.eq_bit_f{}", path, i)));
            self.add_cell(Cell::new_comb(
                CellId(0),
                th12_info.name.clone(),
                self.library.name.clone(),
                th12_info.fit,
                format!("{}.eq_or_f{}", path, i),
                vec![diff_tf, diff_ft],
                vec![eq_bit_f],
            ));

            eq_bits_t.push(eq_bit_t);
            eq_bits_f.push(eq_bit_f);
        }

        // AND-reduce all eq_bits for final result
        // Result is true iff ALL bits are equal
        let out_t = outputs.first().copied().unwrap_or(GateNetId(0));
        let out_f = outputs.get(1).copied().unwrap_or(GateNetId(0));

        if eq_bits_t.len() == 1 {
            // Single bit: direct connection
            let buf_info = self.get_cell_info(&CellFunction::Buf);
            self.add_cell(Cell::new_comb(
                CellId(0),
                buf_info.name.clone(),
                self.library.name.clone(),
                buf_info.fit,
                format!("{}.eq_out_t", path),
                vec![eq_bits_t[0]],
                vec![out_t],
            ));
            self.add_cell(Cell::new_comb(
                CellId(0),
                buf_info.name.clone(),
                self.library.name.clone(),
                buf_info.fit,
                format!("{}.eq_out_f", path),
                vec![eq_bits_f[0]],
                vec![out_f],
            ));
        } else {
            // Multi-bit: AND tree for true, OR tree for false
            // Use THnn for n-input AND
            let and_result = self.ncl_reduce_and(&eq_bits_t, path, "eq_and");
            let or_result = self.ncl_reduce_or(&eq_bits_f, path, "eq_or");

            let buf_info = self.get_cell_info(&CellFunction::Buf);
            self.add_cell(Cell::new_comb(
                CellId(0),
                buf_info.name.clone(),
                self.library.name.clone(),
                buf_info.fit,
                format!("{}.eq_final_t", path),
                vec![and_result],
                vec![out_t],
            ));
            self.add_cell(Cell::new_comb(
                CellId(0),
                buf_info.name.clone(),
                self.library.name.clone(),
                buf_info.fit,
                format!("{}.eq_final_f", path),
                vec![or_result],
                vec![out_f],
            ));
        }

        self.stats.decomposed_mappings += 1;
    }

    /// Map NCL shift operation
    fn map_ncl_shift(
        &mut self,
        width: u32,
        _left: bool,
        inputs: &[Vec<GateNetId>],
        outputs: &[GateNetId],
        path: &str,
    ) {
        // NCL shifter: barrel shifter using NCL mux tree
        // Simplified implementation: just buffer through
        self.warnings.push(format!(
            "NCL Shift at {} - using simplified implementation",
            path
        ));

        let buf_info = self.get_cell_info(&CellFunction::Buf);
        for i in 0..(width * 2) as usize {
            let in_bit = inputs
                .first()
                .and_then(|v| v.get(i))
                .copied()
                .unwrap_or(GateNetId(0));
            let out_bit = outputs.get(i).copied().unwrap_or(GateNetId(0));
            if out_bit.0 != 0 && in_bit.0 != 0 {
                self.add_cell(Cell::new_comb(
                    CellId(0),
                    buf_info.name.clone(),
                    self.library.name.clone(),
                    buf_info.fit,
                    format!("{}.shift_buf{}", path, i),
                    vec![in_bit],
                    vec![out_bit],
                ));
            }
        }

        self.stats.decomposed_mappings += 1;
    }

    /// Map NCL 2-to-1 multiplexer
    fn map_ncl_mux2(
        &mut self,
        width: u32,
        inputs: &[Vec<GateNetId>],
        outputs: &[GateNetId],
        path: &str,
    ) {
        if inputs.len() < 3 {
            self.warnings
                .push("NCL Mux2 needs selector and 2 data inputs".to_string());
            return;
        }

        // NCL Mux: y = (sel AND d1) OR (NOT sel AND d0)
        // In NCL: y_t = TH22(sel_t, d1_t) OR TH22(sel_f, d0_t)
        //         y_f = TH22(sel_t, d1_f) OR TH22(sel_f, d0_f)

        let th12_info = self.get_ncl_cell_info(&CellFunction::Th12);
        let th22_info = self.get_ncl_cell_info(&CellFunction::Th22);

        // Selector is single-bit dual-rail
        let sel_t = inputs[0].first().copied().unwrap_or(GateNetId(0));
        let sel_f = inputs[0].get(1).copied().unwrap_or(GateNetId(0));

        for i in 0..width as usize {
            let d0_t = inputs[1].get(i * 2).copied().unwrap_or(GateNetId(0));
            let d0_f = inputs[1].get(i * 2 + 1).copied().unwrap_or(GateNetId(0));
            let d1_t = inputs[2].get(i * 2).copied().unwrap_or(GateNetId(0));
            let d1_f = inputs[2].get(i * 2 + 1).copied().unwrap_or(GateNetId(0));

            let out_t = outputs.get(i * 2).copied().unwrap_or(GateNetId(0));
            let out_f = outputs.get(i * 2 + 1).copied().unwrap_or(GateNetId(0));

            // sel_t AND d1_t
            let sel_d1_t = self.alloc_net_id();
            self.netlist.add_net(GateNet::new(
                sel_d1_t,
                format!("{}.mux_sel_d1_t{}", path, i),
            ));
            self.add_cell(Cell::new_comb(
                CellId(0),
                th22_info.name.clone(),
                self.library.name.clone(),
                th22_info.fit,
                format!("{}.mux_and1_t{}", path, i),
                vec![sel_t, d1_t],
                vec![sel_d1_t],
            ));

            // sel_f AND d0_t (NOT sel AND d0)
            let nsel_d0_t = self.alloc_net_id();
            self.netlist.add_net(GateNet::new(
                nsel_d0_t,
                format!("{}.mux_nsel_d0_t{}", path, i),
            ));
            self.add_cell(Cell::new_comb(
                CellId(0),
                th22_info.name.clone(),
                self.library.name.clone(),
                th22_info.fit,
                format!("{}.mux_and2_t{}", path, i),
                vec![sel_f, d0_t],
                vec![nsel_d0_t],
            ));

            // out_t = OR(sel_d1_t, nsel_d0_t)
            self.add_cell(Cell::new_comb(
                CellId(0),
                th12_info.name.clone(),
                self.library.name.clone(),
                th12_info.fit,
                format!("{}.mux_or_t{}", path, i),
                vec![sel_d1_t, nsel_d0_t],
                vec![out_t],
            ));

            // Similar for false rail
            let sel_d1_f = self.alloc_net_id();
            self.netlist.add_net(GateNet::new(
                sel_d1_f,
                format!("{}.mux_sel_d1_f{}", path, i),
            ));
            self.add_cell(Cell::new_comb(
                CellId(0),
                th22_info.name.clone(),
                self.library.name.clone(),
                th22_info.fit,
                format!("{}.mux_and1_f{}", path, i),
                vec![sel_t, d1_f],
                vec![sel_d1_f],
            ));

            let nsel_d0_f = self.alloc_net_id();
            self.netlist.add_net(GateNet::new(
                nsel_d0_f,
                format!("{}.mux_nsel_d0_f{}", path, i),
            ));
            self.add_cell(Cell::new_comb(
                CellId(0),
                th22_info.name.clone(),
                self.library.name.clone(),
                th22_info.fit,
                format!("{}.mux_and2_f{}", path, i),
                vec![sel_f, d0_f],
                vec![nsel_d0_f],
            ));

            self.add_cell(Cell::new_comb(
                CellId(0),
                th12_info.name.clone(),
                self.library.name.clone(),
                th12_info.fit,
                format!("{}.mux_or_f{}", path, i),
                vec![sel_d1_f, nsel_d0_f],
                vec![out_f],
            ));
        }

        self.stats.decomposed_mappings += 1;
    }

    /// Map NCL register (NULL/DATA latch with completion)
    fn map_ncl_reg(
        &mut self,
        width: u32,
        inputs: &[Vec<GateNetId>],
        outputs: &[GateNetId],
        path: &str,
    ) {
        if inputs.is_empty() {
            self.warnings.push("NCL Reg needs input".to_string());
            return;
        }

        // NCL register: TH22-based latch for each dual-rail bit
        // The register holds value when input goes to NULL

        let th22_info = self.get_ncl_cell_info(&CellFunction::Th22);

        for i in 0..width as usize {
            let in_t = inputs[0].get(i * 2).copied().unwrap_or(GateNetId(0));
            let in_f = inputs[0].get(i * 2 + 1).copied().unwrap_or(GateNetId(0));

            let out_t = outputs.get(i * 2).copied().unwrap_or(GateNetId(0));
            let out_f = outputs.get(i * 2 + 1).copied().unwrap_or(GateNetId(0));

            // NCL latch: uses TH22 feedback for state-holding behavior
            // Note: NCL registers are clockless - statefulness comes from feedback
            // true rail latch
            let mut cell_t = Cell::new_comb(
                CellId(0),
                th22_info.name.clone(),
                self.library.name.clone(),
                th22_info.fit,
                format!("{}.reg_t{}", path, i),
                vec![in_t, out_t], // Feedback from output makes this stateful
                vec![out_t],
            );
            cell_t.source_op = Some("NclReg_T".to_string());
            self.add_cell(cell_t);

            // false rail latch
            let mut cell_f = Cell::new_comb(
                CellId(0),
                th22_info.name.clone(),
                self.library.name.clone(),
                th22_info.fit,
                format!("{}.reg_f{}", path, i),
                vec![in_f, out_f], // Feedback from output makes this stateful
                vec![out_f],
            );
            cell_f.source_op = Some("NclReg_F".to_string());
            self.add_cell(cell_f);
        }

        self.stats.decomposed_mappings += 1;
    }

    /// Map NCL completion detection
    fn map_ncl_completion(
        &mut self,
        width: u32,
        inputs: &[Vec<GateNetId>],
        outputs: &[GateNetId],
        path: &str,
    ) {
        if inputs.is_empty() {
            self.warnings.push("NCL Completion needs input".to_string());
            return;
        }

        // Completion detection: all bits must be either DATA_TRUE or DATA_FALSE
        // For each bit: complete_i = t_i OR f_i
        // Total: complete = AND(complete_0, complete_1, ...)

        // Check if TH12 is available in the library, if not use OR2 as fallback
        // TH12 is functionally equivalent to OR for completion detection
        let th12_cells = self.library.find_cells_by_function(&CellFunction::Th12);
        let use_th12 = !th12_cells.is_empty();

        let gate_info = if use_th12 {
            self.get_ncl_cell_info(&CellFunction::Th12)
        } else {
            // Fall back to OR2 gate for completion detection
            self.get_cell_info(&CellFunction::Or2)
        };

        let mut bit_completes = Vec::new();

        for i in 0..width as usize {
            // inputs[i*2] contains the t rail nets, inputs[i*2+1] contains the f rail nets
            // Each input is a separate signal from the NCL expansion
            let in_t = inputs
                .get(i * 2)
                .and_then(|v| v.first())
                .copied()
                .unwrap_or(GateNetId(0));
            let in_f = inputs
                .get(i * 2 + 1)
                .and_then(|v| v.first())
                .copied()
                .unwrap_or(GateNetId(0));

            let bit_complete = self.alloc_net_id();
            self.netlist.add_net(GateNet::new(
                bit_complete,
                format!("{}.complete_bit{}", path, i),
            ));

            // TH12(t, f) or OR2(t, f) - complete when either rail is high
            self.add_cell(Cell::new_comb(
                CellId(0),
                gate_info.name.clone(),
                self.library.name.clone(),
                gate_info.fit,
                format!("{}.complete_or{}", path, i),
                vec![in_t, in_f],
                vec![bit_complete],
            ));

            bit_completes.push(bit_complete);
        }

        // AND all bit completions together
        let complete_out = outputs.first().copied().unwrap_or(GateNetId(0));

        if bit_completes.len() == 1 {
            let buf_info = self.get_cell_info(&CellFunction::Buf);
            self.add_cell(Cell::new_comb(
                CellId(0),
                buf_info.name.clone(),
                self.library.name.clone(),
                buf_info.fit,
                format!("{}.complete_buf", path),
                vec![bit_completes[0]],
                vec![complete_out],
            ));
        } else {
            let and_result = self.ncl_reduce_and(&bit_completes, path, "complete");

            let buf_info = self.get_cell_info(&CellFunction::Buf);
            self.add_cell(Cell::new_comb(
                CellId(0),
                buf_info.name.clone(),
                self.library.name.clone(),
                buf_info.fit,
                format!("{}.complete_final", path),
                vec![and_result],
                vec![complete_out],
            ));
        }

        self.stats.decomposed_mappings += 1;
    }

    /// Map NCL NULL generator (outputs all zeros = NULL state)
    fn map_ncl_null(&mut self, width: u32, outputs: &[GateNetId], path: &str) {
        let tie_low = self.get_tie_low();
        let buf_info = self.get_cell_info(&CellFunction::Buf);

        for i in 0..(width * 2) as usize {
            let out = outputs.get(i).copied().unwrap_or(GateNetId(0));
            if out.0 != 0 {
                self.add_cell(Cell::new_comb(
                    CellId(0),
                    buf_info.name.clone(),
                    self.library.name.clone(),
                    buf_info.fit,
                    format!("{}.null{}", path, i),
                    vec![tie_low],
                    vec![out],
                ));
            }
        }

        self.stats.decomposed_mappings += 1;
    }

    /// Get NCL cell info, falling back to generic if not in library
    fn get_ncl_cell_info(&self, function: &CellFunction) -> LibraryCellInfo {
        // Try to find in library first
        let cells = self.library.find_cells_by_function(function);
        if let Some(cell) = cells.first() {
            return LibraryCellInfo::from_library_cell(cell);
        }

        // Fallback: create synthetic NCL cell info
        let (name, fit) = match function {
            CellFunction::Th12 => ("TH12", 0.5),
            CellFunction::Th22 => ("TH22", 0.6),
            CellFunction::Th13 => ("TH13", 0.6),
            CellFunction::Th23 => ("TH23", 0.7),
            CellFunction::Th33 => ("TH33", 0.8),
            CellFunction::Th14 => ("TH14", 0.7),
            CellFunction::Th24 => ("TH24", 0.8),
            CellFunction::Th34 => ("TH34", 0.9),
            CellFunction::Th44 => ("TH44", 1.0),
            CellFunction::Thmn { m, n } => {
                return LibraryCellInfo {
                    name: format!("TH{}{}", m, n),
                    function: function.clone(),
                    fit: 0.5 + 0.1 * (*n as f64),
                    failure_modes: vec![],
                };
            }
            CellFunction::NclCompletion { width } => {
                return LibraryCellInfo {
                    name: format!("NCL_COMPLETE{}", width),
                    function: function.clone(),
                    fit: 0.1 * (*width as f64),
                    failure_modes: vec![],
                };
            }
            _ => ("NCL_GENERIC", 0.5),
        };

        LibraryCellInfo {
            name: name.to_string(),
            function: function.clone(),
            fit,
            failure_modes: vec![],
        }
    }

    // ========================================================================
    // Memory Block Mapping (vendor-agnostic)
    // ========================================================================

    /// Map a MemBlock node to RAM primitive cells from the technology library.
    ///
    /// Queries `RamCellInfo` from the library for capabilities (aspect ratios,
    /// pin names). Never hardcodes cell names — works with any tech library
    /// that defines a RAM cell with `ram_info`.
    #[allow(clippy::too_many_arguments)]
    fn map_memory_block(
        &mut self,
        data_width: u32,
        addr_width: u32,
        depth: u32,
        has_write: bool,
        inputs: &[Vec<GateNetId>],
        outputs: &[GateNetId],
        path: &str,
        clock: Option<GateNetId>,
    ) {
        // 1. Query library for RAM cell
        let (ram_cell, ram_info) = match self.library.find_ram_cell() {
            Some(info) => info,
            None => {
                self.warnings.push(format!(
                    "No RAM cell in library '{}' — cannot map MemBlock at {}",
                    self.library.name, path
                ));
                return;
            }
        };
        let ram_cell_name = ram_cell.name.clone();
        let ram_cell_fit = ram_cell.fit;
        let ram_cell_info = LibraryCellInfo::from_library_cell(ram_cell);

        // 2. Select best aspect ratio
        let (block_depth, block_width) =
            Self::select_best_aspect_ratio(&ram_info.aspect_ratios, data_width, depth);

        // 3. Compute tiling
        let width_blocks = data_width.div_ceil(block_width);
        let depth_blocks = depth.div_ceil(block_depth);
        let block_addr_width = Self::clog2(block_depth);

        let tie_low = self.get_tie_low();
        let tie_high = self.get_tie_high();
        let clk = clock.unwrap_or(tie_low);

        // Extract input nets
        // Inputs: [raddr, waddr, wdata, we] for read-write, [raddr] for read-only
        let raddr_nets = &inputs[0];
        let waddr_nets = if has_write && inputs.len() > 1 {
            &inputs[1]
        } else {
            &inputs[0] // read-only — won't be used
        };
        let wdata_nets = if has_write && inputs.len() > 2 {
            &inputs[2]
        } else {
            &Vec::new()
        };
        let we_net = if has_write && inputs.len() > 3 {
            inputs[3].first().copied().unwrap_or(tie_low)
        } else {
            tie_low
        };

        if depth_blocks == 1 && width_blocks == 1 {
            // Simple case: single RAM block
            self.instantiate_ram_block(
                &ram_cell_name,
                &ram_cell_info,
                ram_cell_fit,
                block_depth,
                block_width,
                block_addr_width,
                data_width,
                addr_width,
                raddr_nets,
                waddr_nets,
                wdata_nets,
                we_net,
                outputs,
                clk,
                tie_low,
                tie_high,
                has_write,
                path,
                ram_info,
            );
        } else if depth_blocks == 1 {
            // Width tiling only: multiple blocks in parallel, same address
            self.map_memory_width_tiling(
                &ram_cell_name,
                &ram_cell_info,
                ram_cell_fit,
                block_depth,
                block_width,
                block_addr_width,
                data_width,
                addr_width,
                width_blocks,
                raddr_nets,
                waddr_nets,
                wdata_nets,
                we_net,
                outputs,
                clk,
                tie_low,
                tie_high,
                has_write,
                path,
                ram_info,
            );
        } else {
            // Depth tiling (possibly with width tiling too)
            self.map_memory_depth_tiling(
                &ram_cell_name,
                &ram_cell_info,
                ram_cell_fit,
                block_depth,
                block_width,
                block_addr_width,
                data_width,
                addr_width,
                depth,
                width_blocks,
                depth_blocks,
                raddr_nets,
                waddr_nets,
                wdata_nets,
                we_net,
                outputs,
                clk,
                tie_low,
                tie_high,
                has_write,
                path,
                ram_info,
            );
        }

        self.stats.direct_mappings += 1;
    }

    /// Select the best aspect ratio from available options.
    ///
    /// Prefers single-block fit. If no single-block fit, minimizes total block count.
    fn select_best_aspect_ratio(
        aspect_ratios: &[(u32, u32)],
        data_width: u32,
        depth: u32,
    ) -> (u32, u32) {
        // Try to find a single-block fit
        let mut best_single: Option<(u32, u32, u32)> = None; // (depth, width, waste)
        for &(bd, bw) in aspect_ratios {
            if bw >= data_width && bd >= depth {
                let waste = (bw - data_width) * bd + (bd - depth) * bw;
                if best_single.is_none() || waste < best_single.unwrap().2 {
                    best_single = Some((bd, bw, waste));
                }
            }
        }
        if let Some((bd, bw, _)) = best_single {
            return (bd, bw);
        }

        // No single-block fit — minimize total block count
        let mut best: (u32, u32) = aspect_ratios[0];
        let mut best_count = u32::MAX;
        for &(bd, bw) in aspect_ratios {
            let w_blocks = data_width.div_ceil(bw);
            let d_blocks = depth.div_ceil(bd);
            let total = w_blocks * d_blocks;
            if total < best_count {
                best_count = total;
                best = (bd, bw);
            }
        }
        best
    }

    /// Ceiling log2
    fn clog2(n: u32) -> u32 {
        if n <= 1 {
            return 0;
        }
        32 - (n - 1).leading_zeros()
    }

    /// Instantiate a single RAM block with the given port connections.
    #[allow(clippy::too_many_arguments)]
    fn instantiate_ram_block(
        &mut self,
        ram_cell_name: &str,
        ram_cell_info: &LibraryCellInfo,
        ram_cell_fit: f64,
        block_depth: u32,
        block_width: u32,
        block_addr_width: u32,
        data_width: u32,
        addr_width: u32,
        raddr_nets: &[GateNetId],
        waddr_nets: &[GateNetId],
        wdata_nets: &[GateNetId],
        we_net: GateNetId,
        output_nets: &[GateNetId],
        clk: GateNetId,
        tie_low: GateNetId,
        tie_high: GateNetId,
        has_write: bool,
        path: &str,
        ram_info: &crate::tech_library::RamCellInfo,
    ) {
        // Build input net list: raddr (padded), re, rclke, wdata (padded), waddr (padded), we, wclke
        // Order matches the cell's pin list from the library
        let mut cell_inputs = Vec::new();

        // Read address — pad to block_addr_width
        for bit in 0..block_addr_width as usize {
            cell_inputs.push(raddr_nets.get(bit).copied().unwrap_or(tie_low));
        }
        // RCLK
        cell_inputs.push(clk);
        // RCLKE — tie high (always enabled)
        cell_inputs.push(tie_high);
        // RE — tie high (always enabled)
        cell_inputs.push(tie_high);

        if has_write {
            // Write data — pad to block_width
            for bit in 0..block_width as usize {
                cell_inputs.push(wdata_nets.get(bit).copied().unwrap_or(tie_low));
            }
            // Write address — pad to block_addr_width
            for bit in 0..block_addr_width as usize {
                cell_inputs.push(waddr_nets.get(bit).copied().unwrap_or(tie_low));
            }
            // WCLK
            cell_inputs.push(clk);
            // WCLKE — tie high
            cell_inputs.push(tie_high);
            // WE
            cell_inputs.push(we_net);
        } else {
            // ROM mode: tie write inputs low
            for _ in 0..block_width as usize {
                cell_inputs.push(tie_low);
            }
            for _ in 0..block_addr_width as usize {
                cell_inputs.push(tie_low);
            }
            cell_inputs.push(clk);
            cell_inputs.push(tie_low); // WCLKE
            cell_inputs.push(tie_low); // WE
        }

        // MASK — tie all high (no masking)
        if ram_info.has_write_mask {
            for _ in 0..block_width as usize {
                cell_inputs.push(tie_high);
            }
        }

        // Output nets: only data_width bits are used, rest are unconnected
        let mut cell_outputs = Vec::new();
        for bit in 0..block_width as usize {
            if bit < data_width as usize {
                cell_outputs.push(output_nets.get(bit).copied().unwrap_or(output_nets[0]));
            } else {
                // Unused output bit — create a dangling net
                let unused = self
                    .netlist
                    .add_net_with_name(format!("{}.unused_rdata{}", path, bit));
                cell_outputs.push(unused);
            }
        }

        let mut cell = Cell::new_seq(
            CellId(0),
            ram_cell_name.to_string(),
            self.library.name.clone(),
            ram_cell_fit,
            path.to_string(),
            cell_inputs,
            cell_outputs,
            clk,
            None,
        );
        cell.source_op = Some("MemBlock".to_string());
        cell.parameters
            .insert("READ_MODE".to_string(), block_width.to_string());
        cell.parameters
            .insert("WRITE_MODE".to_string(), block_width.to_string());
        ram_cell_info.apply_to_cell(&mut cell);
        self.add_cell(cell);
    }

    /// Width tiling: multiple RAM blocks in parallel, each handling a data slice.
    #[allow(clippy::too_many_arguments)]
    fn map_memory_width_tiling(
        &mut self,
        ram_cell_name: &str,
        ram_cell_info: &LibraryCellInfo,
        ram_cell_fit: f64,
        block_depth: u32,
        block_width: u32,
        block_addr_width: u32,
        data_width: u32,
        addr_width: u32,
        width_blocks: u32,
        raddr_nets: &[GateNetId],
        waddr_nets: &[GateNetId],
        wdata_nets: &[GateNetId],
        we_net: GateNetId,
        output_nets: &[GateNetId],
        clk: GateNetId,
        tie_low: GateNetId,
        tie_high: GateNetId,
        has_write: bool,
        path: &str,
        ram_info: &crate::tech_library::RamCellInfo,
    ) {
        for wb in 0..width_blocks {
            let bit_lo = (wb * block_width) as usize;
            let bit_hi = std::cmp::min(((wb + 1) * block_width) as usize, data_width as usize);
            let slice_width = bit_hi - bit_lo;

            // Slice the data/output nets for this width block
            let wdata_slice: Vec<GateNetId> = (0..block_width as usize)
                .map(|b| {
                    if b < slice_width {
                        wdata_nets.get(bit_lo + b).copied().unwrap_or(tie_low)
                    } else {
                        tie_low
                    }
                })
                .collect();

            let mut output_slice: Vec<GateNetId> = Vec::new();
            for b in 0..block_width as usize {
                if b < slice_width {
                    output_slice.push(
                        output_nets
                            .get(bit_lo + b)
                            .copied()
                            .unwrap_or(output_nets[0]),
                    );
                } else {
                    let unused = self
                        .netlist
                        .add_net_with_name(format!("{}.wb{}.unused_rdata{}", path, wb, b));
                    output_slice.push(unused);
                }
            }

            self.instantiate_ram_block(
                ram_cell_name,
                ram_cell_info,
                ram_cell_fit,
                block_depth,
                block_width,
                block_addr_width,
                block_width, // each block uses full block_width
                addr_width,
                raddr_nets,
                waddr_nets,
                &wdata_slice,
                we_net,
                &output_slice,
                clk,
                tie_low,
                tie_high,
                has_write,
                &format!("{}.wb{}", path, wb),
                ram_info,
            );
        }
    }

    /// Depth tiling: multiple RAM blocks with address decode + output mux.
    ///
    /// Upper address bits select which block is active. Output mux routes
    /// the selected block's data to the final output. Decode logic and mux
    /// are built from standard cells.
    #[allow(clippy::too_many_arguments)]
    fn map_memory_depth_tiling(
        &mut self,
        ram_cell_name: &str,
        ram_cell_info: &LibraryCellInfo,
        ram_cell_fit: f64,
        block_depth: u32,
        block_width: u32,
        block_addr_width: u32,
        data_width: u32,
        addr_width: u32,
        _depth: u32,
        width_blocks: u32,
        depth_blocks: u32,
        raddr_nets: &[GateNetId],
        waddr_nets: &[GateNetId],
        wdata_nets: &[GateNetId],
        we_net: GateNetId,
        output_nets: &[GateNetId],
        clk: GateNetId,
        tie_low: GateNetId,
        tie_high: GateNetId,
        has_write: bool,
        path: &str,
        ram_info: &crate::tech_library::RamCellInfo,
    ) {
        let upper_bits_count = Self::clog2(depth_blocks);
        // Upper address bits for depth decode
        let upper_raddr: Vec<GateNetId> = (0..upper_bits_count)
            .map(|i| {
                raddr_nets
                    .get((block_addr_width + i) as usize)
                    .copied()
                    .unwrap_or(tie_low)
            })
            .collect();
        let upper_waddr: Vec<GateNetId> = (0..upper_bits_count)
            .map(|i| {
                waddr_nets
                    .get((block_addr_width + i) as usize)
                    .copied()
                    .unwrap_or(tie_low)
            })
            .collect();

        // Per-depth-block output nets and write-enable nets
        let mut block_outputs: Vec<Vec<GateNetId>> = Vec::new();
        let mut block_we_nets: Vec<GateNetId> = Vec::new();

        for db in 0..depth_blocks {
            // Decode write-enable: we_block = we & (upper_addr == db)
            let we_block = if depth_blocks > 1 && has_write {
                self.build_addr_decode(
                    &upper_waddr,
                    db,
                    upper_bits_count,
                    we_net,
                    tie_low,
                    tie_high,
                    &format!("{}.db{}.we_decode", path, db),
                )
            } else {
                we_net
            };
            block_we_nets.push(we_block);

            // Create output nets for this depth block
            let mut db_outputs = Vec::new();
            for bit in 0..data_width {
                let net = self
                    .netlist
                    .add_net_with_name(format!("{}.db{}.rdata{}", path, db, bit));
                db_outputs.push(net);
            }
            block_outputs.push(db_outputs.clone());

            // Instantiate RAM block(s) for this depth slice
            if width_blocks == 1 {
                self.instantiate_ram_block(
                    ram_cell_name,
                    ram_cell_info,
                    ram_cell_fit,
                    block_depth,
                    block_width,
                    block_addr_width,
                    data_width,
                    addr_width,
                    raddr_nets,
                    waddr_nets,
                    wdata_nets,
                    we_block,
                    &db_outputs,
                    clk,
                    tie_low,
                    tie_high,
                    has_write,
                    &format!("{}.db{}", path, db),
                    ram_info,
                );
            } else {
                // Width + depth tiling
                self.map_memory_width_tiling(
                    ram_cell_name,
                    ram_cell_info,
                    ram_cell_fit,
                    block_depth,
                    block_width,
                    block_addr_width,
                    data_width,
                    addr_width,
                    width_blocks,
                    raddr_nets,
                    waddr_nets,
                    wdata_nets,
                    we_block,
                    &db_outputs,
                    clk,
                    tie_low,
                    tie_high,
                    has_write,
                    &format!("{}.db{}", path, db),
                    ram_info,
                );
            }
        }

        // Build output mux: select among depth blocks using upper read address bits
        self.build_depth_output_mux(
            &block_outputs,
            &upper_raddr,
            output_nets,
            data_width,
            depth_blocks,
            tie_low,
            &format!("{}.depth_mux", path),
        );
    }

    /// Build address decode logic: output = enable & (addr_bits == target_value)
    #[allow(clippy::too_many_arguments)]
    fn build_addr_decode(
        &mut self,
        addr_bits: &[GateNetId],
        target_value: u32,
        num_bits: u32,
        enable: GateNetId,
        tie_low: GateNetId,
        _tie_high: GateNetId,
        path: &str,
    ) -> GateNetId {
        // Build AND tree: match each bit against the target value
        let mut match_net = enable;

        for bit in 0..num_bits as usize {
            let addr_bit = addr_bits.get(bit).copied().unwrap_or(tie_low);
            let target_bit = (target_value >> bit) & 1;

            let bit_match = if target_bit == 0 {
                // Need inverted bit
                let inv_net = self
                    .netlist
                    .add_net_with_name(format!("{}.inv_bit{}", path, bit));
                let inv_info = self.get_cell_info(&CellFunction::Inv);
                let mut inv_cell = Cell::new_comb(
                    CellId(0),
                    inv_info.name.clone(),
                    self.library.name.clone(),
                    inv_info.fit,
                    format!("{}.inv{}", path, bit),
                    vec![addr_bit],
                    vec![inv_net],
                );
                inv_info.apply_to_cell(&mut inv_cell);
                self.add_cell(inv_cell);
                inv_net
            } else {
                addr_bit
            };

            // AND with running result
            let and_out = self
                .netlist
                .add_net_with_name(format!("{}.and{}", path, bit));
            let and_info = self.get_cell_info(&CellFunction::And2);
            let mut and_cell = Cell::new_comb(
                CellId(0),
                and_info.name.clone(),
                self.library.name.clone(),
                and_info.fit,
                format!("{}.and{}", path, bit),
                vec![match_net, bit_match],
                vec![and_out],
            );
            and_info.apply_to_cell(&mut and_cell);
            self.add_cell(and_cell);

            match_net = and_out;
        }

        match_net
    }

    /// Build output mux for depth tiling using cascaded 2:1 muxes.
    ///
    /// Each select bit picks between pairs of block outputs.
    #[allow(clippy::too_many_arguments)]
    fn build_depth_output_mux(
        &mut self,
        block_outputs: &[Vec<GateNetId>],
        select_bits: &[GateNetId],
        final_outputs: &[GateNetId],
        data_width: u32,
        depth_blocks: u32,
        tie_low: GateNetId,
        path: &str,
    ) {
        if depth_blocks == 1 {
            // No mux needed — but we should never get here (caller handles single block)
            return;
        }

        // For 2 depth blocks: simple 2:1 mux
        if depth_blocks == 2 {
            let sel = select_bits.first().copied().unwrap_or(tie_low);
            let mux_info = self.get_cell_info(&CellFunction::Mux2);

            for bit in 0..data_width as usize {
                let d0 = block_outputs[0].get(bit).copied().unwrap_or(tie_low);
                let d1 = block_outputs[1].get(bit).copied().unwrap_or(tie_low);
                let y = final_outputs.get(bit).copied().unwrap_or(final_outputs[0]);

                let mut cell = Cell::new_comb(
                    CellId(0),
                    mux_info.name.clone(),
                    self.library.name.clone(),
                    mux_info.fit,
                    format!("{}.bit{}", path, bit),
                    vec![sel, d0, d1],
                    vec![y],
                );
                cell.source_op = Some("DepthMux".to_string());
                mux_info.apply_to_cell(&mut cell);
                self.add_cell(cell);
            }
            return;
        }

        // For >2 depth blocks: cascaded mux tree
        // Layer-by-layer: pair up blocks, mux with select_bits[layer]
        let mut current_level = block_outputs.to_vec();

        for layer in 0..select_bits.len() {
            let sel = select_bits[layer];
            let mut next_level = Vec::new();
            let mux_info = self.get_cell_info(&CellFunction::Mux2);

            let pairs = current_level.len() / 2;
            for p in 0..pairs {
                let mut mux_outputs = Vec::new();
                for bit in 0..data_width as usize {
                    let d0 = current_level[p * 2].get(bit).copied().unwrap_or(tie_low);
                    let d1 = current_level[p * 2 + 1]
                        .get(bit)
                        .copied()
                        .unwrap_or(tie_low);

                    let is_final =
                        pairs == 1 && current_level.len() <= 2 && layer == select_bits.len() - 1;
                    let y = if is_final {
                        final_outputs.get(bit).copied().unwrap_or(final_outputs[0])
                    } else {
                        self.netlist
                            .add_net_with_name(format!("{}.l{}.p{}.bit{}", path, layer, p, bit))
                    };

                    let mut cell = Cell::new_comb(
                        CellId(0),
                        mux_info.name.clone(),
                        self.library.name.clone(),
                        mux_info.fit,
                        format!("{}.l{}.p{}.bit{}", path, layer, p, bit),
                        vec![sel, d0, d1],
                        vec![y],
                    );
                    cell.source_op = Some("DepthMux".to_string());
                    mux_info.apply_to_cell(&mut cell);
                    self.add_cell(cell);
                    mux_outputs.push(y);
                }
                next_level.push(mux_outputs);
            }
            // If odd number, pass through the last one
            if current_level.len() % 2 == 1 {
                next_level.push(current_level.last().unwrap().clone());
            }
            current_level = next_level;
        }

        // Connect final level to outputs (if not already done in the last mux layer)
        if current_level.len() == 1 {
            // The final mux should have output to final_outputs already
            // This handles the case where it didn't (e.g., odd depth_blocks)
            for bit in 0..data_width as usize {
                let src = current_level[0].get(bit).copied().unwrap_or(tie_low);
                let dst = final_outputs.get(bit).copied().unwrap_or(final_outputs[0]);
                if src != dst {
                    // Buffer to connect
                    let buf_info = self.get_cell_info(&CellFunction::Buf);
                    let mut cell = Cell::new_comb(
                        CellId(0),
                        buf_info.name.clone(),
                        self.library.name.clone(),
                        buf_info.fit,
                        format!("{}.final_buf.bit{}", path, bit),
                        vec![src],
                        vec![dst],
                    );
                    buf_info.apply_to_cell(&mut cell);
                    self.add_cell(cell);
                }
            }
        }
    }

    /// Get or create a tie-low net
    fn get_tie_low(&mut self) -> GateNetId {
        // Look for existing tie-low
        for net in &self.netlist.nets {
            if net.name.contains("tie_low") || net.name == "gnd" {
                return net.id;
            }
        }

        // Create new tie-low
        let id = self.alloc_net_id();
        self.netlist
            .add_net(GateNet::new(id, "tie_low".to_string()));

        // Add tie cell
        let tie_info = self.get_cell_info(&CellFunction::TieLow);
        self.add_cell(Cell::new_comb(
            CellId(0),
            tie_info.name,
            self.library.name.clone(),
            tie_info.fit,
            "tie_low".to_string(),
            vec![],
            vec![id],
        ));

        id
    }

    /// Get or create a tie-high net
    fn get_tie_high(&mut self) -> GateNetId {
        // Look for existing tie-high
        for net in &self.netlist.nets {
            if net.name.contains("tie_high") || net.name == "vdd" {
                return net.id;
            }
        }

        // Create new tie-high
        let id = self.alloc_net_id();
        self.netlist
            .add_net(GateNet::new(id, "tie_high".to_string()));

        // Add tie cell
        let tie_info = self.get_cell_info(&CellFunction::TieHigh);
        self.add_cell(Cell::new_comb(
            CellId(0),
            tie_info.name,
            self.library.name.clone(),
            tie_info.fit,
            "tie_high".to_string(),
            vec![],
            vec![id],
        ));

        id
    }

    /// NCL AND reduction tree
    fn ncl_reduce_and(&mut self, inputs: &[GateNetId], path: &str, prefix: &str) -> GateNetId {
        if inputs.is_empty() {
            return self.get_tie_high();
        }
        if inputs.len() == 1 {
            return inputs[0];
        }

        // Check if TH22 is available in the library, if not use AND2 as fallback
        // TH22 is functionally equivalent to AND for completion detection
        let th22_cells = self.library.find_cells_by_function(&CellFunction::Th22);
        let gate_info = if !th22_cells.is_empty() {
            self.get_ncl_cell_info(&CellFunction::Th22)
        } else {
            // Fall back to AND2 gate
            self.get_cell_info(&CellFunction::And2)
        };

        let mut current = inputs.to_vec();
        let mut level = 0;

        while current.len() > 1 {
            let mut next = Vec::new();
            for (i, chunk) in current.chunks(2).enumerate() {
                if chunk.len() == 2 {
                    let out = self.alloc_net_id();
                    self.netlist.add_net(GateNet::new(
                        out,
                        format!("{}_{}_l{}_{}", path, prefix, level, i),
                    ));
                    self.add_cell(Cell::new_comb(
                        CellId(0),
                        gate_info.name.clone(),
                        self.library.name.clone(),
                        gate_info.fit,
                        format!("{}_{}_l{}_{}", path, prefix, level, i),
                        vec![chunk[0], chunk[1]],
                        vec![out],
                    ));
                    next.push(out);
                } else {
                    next.push(chunk[0]);
                }
            }
            current = next;
            level += 1;
        }

        current[0]
    }

    /// NCL OR reduction tree
    fn ncl_reduce_or(&mut self, inputs: &[GateNetId], path: &str, prefix: &str) -> GateNetId {
        if inputs.is_empty() {
            return self.get_tie_low();
        }
        if inputs.len() == 1 {
            return inputs[0];
        }

        // Check if TH12 is available in the library, if not use OR2 as fallback
        // TH12 is functionally equivalent to OR
        let th12_cells = self.library.find_cells_by_function(&CellFunction::Th12);
        let gate_info = if !th12_cells.is_empty() {
            self.get_ncl_cell_info(&CellFunction::Th12)
        } else {
            // Fall back to OR2 gate
            self.get_cell_info(&CellFunction::Or2)
        };

        let mut current = inputs.to_vec();
        let mut level = 0;

        while current.len() > 1 {
            let mut next = Vec::new();
            for (i, chunk) in current.chunks(2).enumerate() {
                if chunk.len() == 2 {
                    let out = self.alloc_net_id();
                    self.netlist.add_net(GateNet::new(
                        out,
                        format!("{}_{}_l{}_{}", path, prefix, level, i),
                    ));
                    self.add_cell(Cell::new_comb(
                        CellId(0),
                        gate_info.name.clone(),
                        self.library.name.clone(),
                        gate_info.fit,
                        format!("{}_{}_l{}_{}", path, prefix, level, i),
                        vec![chunk[0], chunk[1]],
                        vec![out],
                    ));
                    next.push(out);
                } else {
                    next.push(chunk[0]);
                }
            }
            current = next;
            level += 1;
        }

        current[0]
    }
}

/// Map a Lir to GateNetlist using the given library
pub fn map_lir_to_gates(lir: &Lir, library: &TechLibrary) -> TechMapResult {
    let mut mapper = TechMapper::new(library);
    mapper.map(lir)
}

/// Map a Lir to GateNetlist with physical constraints for IO buffer insertion.
///
/// Constraints from source-level annotations (e.g., `@ { pin: "A1", io_standard: "LVCMOS33" }`)
/// are propagated directly to IO cell parameters. The native PnR reads `cell.parameters["LOC"]`
/// and `cell.parameters["IO_STANDARD"]` directly — no separate PCF/XDC needed.
pub fn map_lir_to_gates_with_constraints(
    lir: &Lir,
    library: &TechLibrary,
    port_constraints: IndexMap<String, PhysicalConstraints>,
) -> TechMapResult {
    let mut mapper = TechMapper::new(library);
    mapper.port_constraints = port_constraints;
    mapper.map(lir)
}

/// Map a Lir to GateNetlist with constraints and gate-level optimizations
pub fn map_lir_to_gates_with_constraints_optimized(
    lir: &Lir,
    library: &TechLibrary,
    port_constraints: IndexMap<String, PhysicalConstraints>,
) -> TechMapResult {
    use crate::gate_optimizer::GateOptimizer;

    let mut mapper = TechMapper::new(library);
    mapper.port_constraints = port_constraints;
    let mut result = mapper.map(lir);

    let mut optimizer = GateOptimizer::new();
    let opt_stats = optimizer.optimize(&mut result.netlist);

    result.stats.cells_created = result.netlist.cells.len();
    result.stats.nets_created = result.netlist.nets.len();

    if opt_stats.cells_removed > 0 {
        result.warnings.push(format!(
            "Optimization: removed {} cells ({} → {}), FIT {} → {}",
            opt_stats.cells_removed,
            opt_stats.cells_before,
            opt_stats.cells_after,
            opt_stats.fit_before,
            opt_stats.fit_after
        ));
    }

    result
}

/// Map a Lir to GateNetlist and run gate-level optimizations
pub fn map_lir_to_gates_optimized(lir: &Lir, library: &TechLibrary) -> TechMapResult {
    use crate::gate_optimizer::GateOptimizer;

    let mut mapper = TechMapper::new(library);
    let mut result = mapper.map(lir);

    // Run gate-level optimizations
    let mut optimizer = GateOptimizer::new();
    let opt_stats = optimizer.optimize(&mut result.netlist);

    // LUT post-mapping optimization for FPGA targets
    if library.is_fpga() {
        crate::gate_lut_opt::optimize_luts(&mut result.netlist);
    }

    // Update stats with optimization info
    result.stats.cells_created = result.netlist.cells.len();
    result.stats.nets_created = result.netlist.nets.len();

    // Add optimization summary to warnings (as info)
    if opt_stats.cells_removed > 0 {
        result.warnings.push(format!(
            "Optimization: removed {} cells ({} → {}), FIT {} → {}",
            opt_stats.cells_removed,
            opt_stats.cells_before,
            opt_stats.cells_after,
            opt_stats.fit_before,
            opt_stats.fit_after
        ));
    }

    result
}

/// Map a Lir to GateNetlist with configurable gate-level optimization
///
/// opt_level controls the optimization passes:
/// - 0: No optimization (raw tech mapping output)
/// - 1: Basic optimization (constant folding, buffer removal, DCE)
/// - 2+: Full optimization (all passes)
pub fn map_lir_to_gates_with_opt_level(
    lir: &Lir,
    library: &TechLibrary,
    opt_level: u8,
) -> TechMapResult {
    use crate::gate_optimizer::GateOptimizer;

    let mut mapper = TechMapper::new(library);
    let mut result = mapper.map(lir);

    if opt_level == 0 {
        // No optimization - return raw tech mapping output
        return result;
    }

    // Run gate-level optimizations
    let mut optimizer = GateOptimizer::new();

    if opt_level == 1 {
        // Basic optimization only
        optimizer.set_enable_constant_folding(true);
        optimizer.set_enable_buffer_removal(true);
        optimizer.set_enable_dce(true);
        optimizer.set_enable_double_inverter_removal(true);
        optimizer.set_enable_identity_removal(true);
    }
    // opt_level >= 2 uses all optimizations (default)

    let opt_stats = optimizer.optimize(&mut result.netlist);

    // Update stats with optimization info
    result.stats.cells_created = result.netlist.cells.len();
    result.stats.nets_created = result.netlist.nets.len();

    // Add optimization summary to warnings (as info)
    if opt_stats.cells_removed > 0 {
        result.warnings.push(format!(
            "Optimization (level {}): removed {} cells ({} → {}), FIT {} → {}",
            opt_level,
            opt_stats.cells_removed,
            opt_stats.cells_before,
            opt_stats.cells_after,
            opt_stats.fit_before,
            opt_stats.fit_after
        ));
    }

    result
}

/// Backward-compatible alias
pub fn map_word_lir_to_gates(word_lir: &Lir, library: &TechLibrary) -> TechMapResult {
    map_lir_to_gates(word_lir, library)
}

/// Full synthesis: LIR → tech mapping → AIG optimization → technology mapping
///
/// This function provides a complete synthesis flow that includes:
/// 1. Initial technology mapping (LIR to gate netlist)
/// 2. AIG-based optimization passes (rewrite, refactor, balance, FRAIG, etc.)
/// 3. Library-aware technology mapping with cut enumeration
///
/// The optimization level is automatically configured based on the library:
/// - FPGA libraries: uses lut_size for K-feasible cuts
/// - ASIC libraries: uses default cut size for more optimization freedom
///
/// # Arguments
/// * `lir` - The LIR (Low-level IR) to synthesize
/// * `library` - Target technology library
/// * `preset` - Synthesis preset controlling optimization effort
///
/// # Example
/// ```ignore
/// use skalp_lir::{synthesize, SynthPreset};
///
/// let result = synthesize(&lir, &library, SynthPreset::Balanced);
/// println!("Cells after optimization: {}", result.netlist.cells.len());
/// ```
pub fn synthesize(
    lir: &Lir,
    library: &TechLibrary,
    preset: crate::synth::SynthPreset,
) -> crate::synth::SynthResult {
    use crate::synth::SynthEngine;

    // Step 1: Initial tech mapping
    let initial_result = map_lir_to_gates(lir, library);

    // Note: Pre-AIG buffer removal was removed because it breaks the AIG partition
    // merge. The reverse net replacement rewires cell outputs to use output port net
    // IDs, but the AIG round-trip creates new nets and relies on name-based matching.
    // The output port nets end up disconnected in combinational designs (e.g., ALU).

    // Step 2: Run synthesis engine with AIG optimization
    let mut engine = SynthEngine::with_preset(preset);
    let mut result = engine.optimize(&initial_result.netlist, library);

    // Step 3: LUT post-mapping optimization for FPGA targets
    if library.is_fpga() {
        crate::gate_lut_opt::optimize_luts(&mut result.netlist);
    }

    // Step 4: Buffer removal (remove buffer cells that just pass signals through)
    {
        let mut gate_opt = crate::gate_optimizer::GateOptimizer::new();
        gate_opt.set_enable_constant_folding(false);
        gate_opt.set_enable_dce(false);
        gate_opt.set_enable_boolean_simp(false);
        gate_opt.set_enable_mux_opt(false);
        gate_opt.set_enable_buffer_removal(true);
        gate_opt.optimize(&mut result.netlist);
    }

    result
}

/// Full synthesis with default balanced preset
///
/// Convenience function that uses `SynthPreset::Balanced` for a good
/// trade-off between optimization quality and runtime.
pub fn synthesize_balanced(lir: &Lir, library: &TechLibrary) -> crate::synth::SynthResult {
    // FPGA targets benefit from Auto (parallel Resyn2 + Compress2) since
    // the extra runtime is small relative to place-and-route
    let preset = if library.is_fpga() {
        crate::synth::SynthPreset::Auto
    } else {
        crate::synth::SynthPreset::Balanced
    };
    synthesize(lir, library, preset)
}

/// Full synthesis with area-focused optimization
///
/// Uses `SynthPreset::Area` for aggressive area minimization.
/// Useful for area-constrained designs.
pub fn synthesize_for_area(lir: &Lir, library: &TechLibrary) -> crate::synth::SynthResult {
    synthesize(lir, library, crate::synth::SynthPreset::Area)
}

/// Full synthesis with timing-focused optimization
///
/// Uses `SynthPreset::Timing` for timing-driven optimization.
/// Useful for timing-critical designs.
pub fn synthesize_for_timing(lir: &Lir, library: &TechLibrary) -> crate::synth::SynthResult {
    synthesize(lir, library, crate::synth::SynthPreset::Timing)
}

/// Map hierarchical MIR result to HierarchicalNetlist
///
/// Takes the result of `lower_mir_hierarchical` and maps each instance's LIR
/// to a GateNetlist, preserving hierarchy and connection context.
pub fn map_hierarchical_to_gates(
    hier_lir: &crate::mir_to_lir::HierarchicalMirToLirResult,
    library: &TechLibrary,
) -> crate::hierarchical_netlist::HierarchicalNetlist {
    use crate::compiled_ip::CompiledIp;
    use crate::hierarchical_netlist::{HierarchicalNetlist, InstanceNetlist, PortConnection};
    use crate::mir_to_lir::PortConnectionInfo;

    let mut result = HierarchicalNetlist::new(hier_lir.top_module.clone(), library.name.clone());

    // Map each instance's LIR to a GateNetlist
    // Sort instance paths for deterministic iteration order (HashMap is non-deterministic)
    let mut sorted_paths: Vec<_> = hier_lir.instances.keys().collect();
    sorted_paths.sort();

    for path in sorted_paths {
        let inst_lir = hier_lir.instances.get(path).unwrap();
        // Check if this is a compiled IP (pre-compiled netlist)
        let tech_result = if let Some(ref compiled_ip_path) = inst_lir.lir_result.compiled_ip_path {
            // Load the pre-compiled netlist directly
            match CompiledIp::read_from_file(std::path::Path::new(compiled_ip_path), None) {
                Ok(compiled_ip) => TechMapResult {
                    netlist: compiled_ip.netlist.clone(),
                    stats: TechMapStats {
                        nodes_processed: 0,
                        cells_created: compiled_ip.netlist.cells.len(),
                        nets_created: compiled_ip.netlist.nets.len(),
                        direct_mappings: 0,
                        decomposed_mappings: 0,
                        clock_buffers_inserted: 0,
                        io_buffers_inserted: 0,
                    },
                    warnings: vec![format!(
                        "Loaded pre-compiled netlist from '{}'",
                        compiled_ip_path
                    )],
                },
                Err(_e) => map_lir_to_gates_optimized(&inst_lir.lir_result.lir, library),
            }
        } else if let Some(ref blackbox_info) = inst_lir.lir_result.blackbox_info {
            // This is a blackbox/vendor IP - create a netlist with a single blackbox cell
            create_blackbox_netlist(blackbox_info, &inst_lir.module_name)
        } else {
            // Normal synthesis path - use non-optimized mapping for now
            // The gate optimizer can break NCL circuits by removing cells that
            // appear "dead" but are actually essential for dual-rail signaling
            // TODO: Add NCL-aware optimization that understands dual-rail semantics
            map_lir_to_gates(&inst_lir.lir_result.lir, library)
        };

        let mut inst_netlist =
            InstanceNetlist::new(inst_lir.module_name.clone(), tech_result.netlist);

        // Record connection context
        // Sort port names for deterministic ordering (HashMap is non-deterministic)
        let mut sorted_port_names: Vec<_> = inst_lir.port_connections.keys().collect();
        sorted_port_names.sort();

        for port_name in sorted_port_names {
            let conn_info = inst_lir.port_connections.get(port_name).unwrap();
            let port_conn = match conn_info {
                PortConnectionInfo::Signal(signal_name) => {
                    PortConnection::ParentNet(signal_name.clone())
                }
                PortConnectionInfo::Constant(value) => {
                    inst_netlist.record_constant_input(port_name, *value);
                    PortConnection::Constant(*value)
                }
                PortConnectionInfo::InstancePort(inst_path, inst_port) => {
                    PortConnection::ChildPort(inst_path.clone(), inst_port.clone())
                }
                PortConnectionInfo::Range(signal_name, high, low) => {
                    PortConnection::ParentRange(signal_name.clone(), *high, *low)
                }
                PortConnectionInfo::BitSelect(signal_name, bit_idx) => {
                    PortConnection::ParentBit(signal_name.clone(), *bit_idx)
                }
            };
            inst_netlist.add_port_connection(port_name.clone(), port_conn);
        }

        // Add child instance references
        for child in &inst_lir.children {
            inst_netlist.add_child(child.clone());
        }

        result.add_instance(path.clone(), inst_netlist);
    }

    result
}

/// Create a GateNetlist containing a single blackbox cell
///
/// This is used for vendor IP modules that should not be synthesized.
/// The blackbox cell preserves the port interface and can be instantiated
/// in the final output.
fn create_blackbox_netlist(
    blackbox_info: &crate::mir_to_lir::BlackboxInfo,
    module_name: &str,
) -> TechMapResult {
    let library_name = "blackbox".to_string();
    let mut netlist = GateNetlist::new(module_name.to_string(), library_name.clone());

    // Create nets for all ports
    let mut input_nets = Vec::new();
    let mut output_nets = Vec::new();

    // Create input port nets
    for input_name in &blackbox_info.inputs {
        let width = blackbox_info
            .port_widths
            .get(input_name)
            .copied()
            .unwrap_or(1);
        for bit in 0..width {
            let net_name = if width == 1 {
                input_name.clone()
            } else {
                format!("{}[{}]", input_name, bit)
            };
            let net = GateNet {
                id: GateNetId(netlist.nets.len() as u32),
                name: net_name,
                driver: None,
                driver_pin: None,
                fanout: Vec::new(),
                is_input: true,
                is_output: false,
                is_clock: input_name.contains("clk") || input_name.contains("clock"),
                is_reset: input_name.contains("rst") || input_name.contains("reset"),
                is_detection: false,
                detection_config: None,
                alias_of: None,
            };
            input_nets.push(net.id);
            netlist.inputs.push(net.id);
            netlist.nets.push(net);
        }
    }

    // Create output port nets
    for output_name in &blackbox_info.outputs {
        let width = blackbox_info
            .port_widths
            .get(output_name)
            .copied()
            .unwrap_or(1);
        for bit in 0..width {
            let net_name = if width == 1 {
                output_name.clone()
            } else {
                format!("{}[{}]", output_name, bit)
            };
            let net = GateNet {
                id: GateNetId(netlist.nets.len() as u32),
                name: net_name,
                driver: None,
                driver_pin: None,
                fanout: Vec::new(),
                is_input: false,
                is_output: true,
                is_clock: false,
                is_reset: false,
                is_detection: false,
                detection_config: None,
                alias_of: None,
            };
            output_nets.push(net.id);
            netlist.outputs.push(net.id);
            netlist.nets.push(net);
        }
    }

    // Create inout port nets (both input and output)
    for inout_name in &blackbox_info.inouts {
        let width = blackbox_info
            .port_widths
            .get(inout_name)
            .copied()
            .unwrap_or(1);
        for bit in 0..width {
            let net_name = if width == 1 {
                inout_name.clone()
            } else {
                format!("{}[{}]", inout_name, bit)
            };
            let net = GateNet {
                id: GateNetId(netlist.nets.len() as u32),
                name: net_name,
                driver: None,
                driver_pin: None,
                fanout: Vec::new(),
                is_input: true,
                is_output: true,
                is_clock: false,
                is_reset: false,
                is_detection: false,
                detection_config: None,
                alias_of: None,
            };
            input_nets.push(net.id);
            output_nets.push(net.id);
            netlist.inputs.push(net.id);
            netlist.outputs.push(net.id);
            netlist.nets.push(net);
        }
    }

    // Create the blackbox cell using the Blackbox CellFunction
    let blackbox_cell = Cell {
        id: CellId(0),
        cell_type: format!("BLACKBOX_{}", blackbox_info.cell_name),
        library: library_name,
        function: None, // Unknown function for blackbox
        fit: 0.0,       // Unknown FIT for blackbox
        failure_modes: Vec::new(),
        inputs: input_nets.clone(),
        outputs: output_nets.clone(),
        path: format!("{}.{}", module_name, blackbox_info.cell_name),
        clock: None,
        reset: None,
        source_op: Some(format!("blackbox:{}", blackbox_info.cell_name)),
        safety_classification: CellSafetyClassification::Functional,
        lut_init: None,
        parameters: IndexMap::new(),
    };
    netlist.cells.push(blackbox_cell);

    // Update net drivers to point to the blackbox cell
    for (pin_idx, &output_id) in output_nets.iter().enumerate() {
        if let Some(net) = netlist.get_net_mut(output_id) {
            net.driver = Some(CellId(0));
            net.driver_pin = Some(pin_idx);
        }
    }

    // Update fanout for input nets
    for (pin_idx, &input_id) in input_nets.iter().enumerate() {
        if let Some(net) = netlist.get_net_mut(input_id) {
            net.fanout.push((CellId(0), pin_idx));
        }
    }

    let total_nets = netlist.nets.len();

    TechMapResult {
        netlist,
        stats: TechMapStats {
            nodes_processed: 0,
            cells_created: 1,
            nets_created: total_nets,
            direct_mappings: 1,
            decomposed_mappings: 0,
            clock_buffers_inserted: 0,
            io_buffers_inserted: 0,
        },
        warnings: vec![format!(
            "Created blackbox cell '{}' for vendor IP",
            blackbox_info.cell_name
        )],
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lir::Lir;
    use crate::tech_library::{LibraryCell, TechLibrary};

    fn make_test_library() -> TechLibrary {
        let mut lib = TechLibrary::new("test_lib");
        lib.add_cell(LibraryCell::new_comb("INV_X1", CellFunction::Inv, 0.05));
        lib.add_cell(LibraryCell::new_comb("NAND2_X1", CellFunction::Nand2, 0.1));
        lib.add_cell(LibraryCell::new_comb("AND2_X1", CellFunction::And2, 0.1));
        lib.add_cell(LibraryCell::new_comb("OR2_X1", CellFunction::Or2, 0.1));
        lib.add_cell(LibraryCell::new_comb("XOR2_X1", CellFunction::Xor2, 0.1));
        lib.add_cell(LibraryCell::new_comb("XNOR2_X1", CellFunction::Xnor2, 0.1));
        lib.add_cell(LibraryCell::new_comb("BUF_X1", CellFunction::Buf, 0.05));
        lib.add_cell(LibraryCell::new_comb("MUX2_X1", CellFunction::Mux2, 0.15));
        lib.add_cell(LibraryCell::new_comb("HA_X1", CellFunction::HalfAdder, 0.2));
        lib.add_cell(LibraryCell::new_comb("FA_X1", CellFunction::FullAdder, 0.3));
        lib.add_cell(LibraryCell::new_comb("DFF_X1", CellFunction::Dff, 0.2));
        lib.add_cell(LibraryCell::new_comb("DFFR_X1", CellFunction::DffR, 0.25));
        lib.add_cell(LibraryCell::new_comb("TIEL_X1", CellFunction::TieLow, 0.01));
        lib
    }

    #[test]
    fn test_map_and_gate() {
        let lib = make_test_library();
        let mut word_lir = Lir::new("test".to_string());

        let a = word_lir.add_input("a".to_string(), 8);
        let b = word_lir.add_input("b".to_string(), 8);
        let y = word_lir.add_output("y".to_string(), 8);

        word_lir.add_node(
            LirOp::And { width: 8 },
            vec![a, b],
            y,
            "test.and".to_string(),
        );

        let result = map_word_lir_to_gates(&word_lir, &lib);

        // Should create 8 AND gates (one per bit) + 1 TieLow cell
        assert_eq!(result.stats.cells_created, 9);
        assert!(result.warnings.is_empty());
    }

    #[test]
    fn test_map_adder() {
        let lib = make_test_library();
        let mut word_lir = Lir::new("test".to_string());

        let a = word_lir.add_input("a".to_string(), 4);
        let b = word_lir.add_input("b".to_string(), 4);
        let sum = word_lir.add_output("sum".to_string(), 4);

        word_lir.add_node(
            LirOp::Add {
                width: 4,
                has_carry: false,
                const_b: None,
            },
            vec![a, b],
            sum,
            "test.add".to_string(),
        );

        let result = map_word_lir_to_gates(&word_lir, &lib);

        // Should create 1 half adder + 3 full adders + 1 TieLow cell
        assert_eq!(result.stats.cells_created, 5);
    }

    #[test]
    fn test_map_mux() {
        let lib = make_test_library();
        let mut word_lir = Lir::new("test".to_string());

        let sel = word_lir.add_input("sel".to_string(), 1);
        let d0 = word_lir.add_input("d0".to_string(), 16);
        let d1 = word_lir.add_input("d1".to_string(), 16);
        let y = word_lir.add_output("y".to_string(), 16);

        word_lir.add_node(
            LirOp::Mux2 { width: 16 },
            vec![sel, d0, d1],
            y,
            "test.mux".to_string(),
        );

        let result = map_word_lir_to_gates(&word_lir, &lib);

        // Should create 16 MUX2 cells + 1 TieLow cell
        assert_eq!(result.stats.cells_created, 17);
    }

    #[test]
    fn test_map_register() {
        let lib = make_test_library();
        let mut word_lir = Lir::new("test".to_string());

        let clk = word_lir.add_input("clk".to_string(), 1);
        word_lir.clocks.push(clk);
        let d = word_lir.add_input("d".to_string(), 8);
        let q = word_lir.add_output("q".to_string(), 8);

        word_lir.add_seq_node(
            LirOp::Reg {
                width: 8,
                has_enable: false,
                has_reset: false,
                async_reset: false,
                reset_value: None,
            },
            vec![d],
            q,
            "test.reg".to_string(),
            clk,
            None,
        );

        let result = map_word_lir_to_gates(&word_lir, &lib);

        // Should create 8 DFF cells + 1 TieLow cell
        assert_eq!(result.stats.cells_created, 9);

        // 8 should be sequential (TieLow is combinational)
        let seq_count = result
            .netlist
            .cells
            .iter()
            .filter(|c| c.is_sequential())
            .count();
        assert_eq!(seq_count, 8);
    }

    #[test]
    fn test_total_fit() {
        let lib = make_test_library();
        let mut word_lir = Lir::new("test".to_string());

        let a = word_lir.add_input("a".to_string(), 4);
        let y = word_lir.add_output("y".to_string(), 4);

        word_lir.add_node(LirOp::Not { width: 4 }, vec![a], y, "test.not".to_string());

        let result = map_word_lir_to_gates(&word_lir, &lib);

        // 4 inverters at 0.05 FIT each + 1 TieLow at 0.01 FIT = 0.21 total
        assert!((result.netlist.total_fit() - 0.21).abs() < 0.001);
    }

    #[test]
    fn test_failure_modes_copied() {
        use crate::gate_netlist::FaultType;
        use crate::tech_library::LibraryFailureMode;

        // Create library with cells that have failure modes
        let mut lib = TechLibrary::new("test_lib_with_fm");
        let mut and_cell = LibraryCell::new_comb("AND2_FM", CellFunction::And2, 0.1);
        and_cell.failure_modes = vec![
            LibraryFailureMode::new("stuck_at_0", 0.03, FaultType::StuckAt0),
            LibraryFailureMode::new("stuck_at_1", 0.03, FaultType::StuckAt1),
            LibraryFailureMode::new("transient", 0.015, FaultType::Transient),
        ];
        lib.add_cell(and_cell);
        lib.add_cell(LibraryCell::new_comb("TIEL_FM", CellFunction::TieLow, 0.01));

        // Map an AND gate
        let mut word_lir = Lir::new("test".to_string());
        let a = word_lir.add_input("a".to_string(), 1);
        let b = word_lir.add_input("b".to_string(), 1);
        let y = word_lir.add_output("y".to_string(), 1);
        word_lir.add_node(
            LirOp::And { width: 1 },
            vec![a, b],
            y,
            "test.and".to_string(),
        );

        let result = map_word_lir_to_gates(&word_lir, &lib);

        // Verify cells were created (1 AND + 1 TieLow)
        assert_eq!(result.netlist.cells.len(), 2);
        // Find the AND cell (not the TieLow)
        let cell = result
            .netlist
            .cells
            .iter()
            .find(|c| c.cell_type == "AND2_FM")
            .expect("Should have an AND2_FM cell");

        // Verify failure modes were copied
        assert_eq!(cell.failure_modes.len(), 3);
        assert_eq!(cell.failure_modes[0].name, "stuck_at_0");
        assert_eq!(cell.failure_modes[0].fault_type, FaultType::StuckAt0);
        assert!((cell.failure_modes[0].fit - 0.03).abs() < 0.001);

        assert_eq!(cell.failure_modes[1].name, "stuck_at_1");
        assert_eq!(cell.failure_modes[2].name, "transient");
        assert_eq!(cell.failure_modes[2].fault_type, FaultType::Transient);
    }

    #[test]
    fn test_ice40_carry_chain_mapping() {
        // Load the ice40 library
        let lib =
            crate::tech_library::get_stdlib_library("ice40").expect("Failed to load ice40 library");

        // Create a simple adder
        let mut word_lir = Lir::new("test_ice40_adder".to_string());
        let a = word_lir.add_input("a".to_string(), 4);
        let b = word_lir.add_input("b".to_string(), 4);
        let sum = word_lir.add_output("sum".to_string(), 4);

        word_lir.add_node(
            LirOp::Add {
                width: 4,
                has_carry: false,
                const_b: None,
            },
            vec![a, b],
            sum,
            "test.add".to_string(),
        );

        let result = map_word_lir_to_gates(&word_lir, &lib);

        // Verify no warnings
        assert!(
            result.warnings.is_empty(),
            "Should have no warnings, got: {:?}",
            result.warnings
        );

        // Verify cells were created
        assert!(result.stats.cells_created > 0, "Should have created cells");

        // Count Carry cells (should have 3 for bits 1-3, first bit uses AND)
        let carry_cells: Vec<_> = result
            .netlist
            .cells
            .iter()
            .filter(|c| c.cell_type.contains("SB_CARRY"))
            .collect();
        assert_eq!(
            carry_cells.len(),
            3,
            "Should have 3 Carry cells for 4-bit adder (bits 1-3)"
        );

        // Count XOR cells (should have 1 for first bit half-adder)
        let xor_cells: Vec<_> = result
            .netlist
            .cells
            .iter()
            .filter(|c| c.cell_type.contains("SB_LUT4_XOR2"))
            .collect();
        assert_eq!(
            xor_cells.len(),
            1,
            "Should have 1 XOR cell for first bit half-adder"
        );

        // Count LUT4 cells for sum (bits 1-3 use single LUT4 with INIT=0x9696)
        let sum_lut4_cells: Vec<_> = result
            .netlist
            .cells
            .iter()
            .filter(|c| c.cell_type == "SB_LUT4" && c.lut_init == Some(0x9696))
            .collect();
        assert_eq!(
            sum_lut4_cells.len(),
            3,
            "Should have 3 LUT4 sum cells for bits 1-3"
        );

        // Count AND cells (should have 1 for first bit carry)
        let and_cells: Vec<_> = result
            .netlist
            .cells
            .iter()
            .filter(|c| c.cell_type.contains("SB_LUT4_AND2"))
            .collect();
        assert_eq!(and_cells.len(), 1, "Should have 1 AND cell for first bit");

        // Core adder cells = 1 SB_GND + 1 XOR + 1 AND (bit 0) + 3 * (1 LUT4 + 1 Carry) (bits 1-3) = 9
        // IO buffers = 8 SB_IO (4 inputs a + 4 inputs b) + 4 SB_IO (4 outputs sum) + 1 SB_VCC = 13
        // Total = 22
        let logic_cells = result
            .netlist
            .cells
            .iter()
            .filter(|c| c.cell_type != "SB_IO" && c.cell_type != "SB_VCC")
            .count();
        assert_eq!(
            logic_cells, 9,
            "Should have 9 logic cells for 4-bit ice40 adder"
        );
        assert_eq!(
            result.stats.cells_created, 22,
            "Should have 22 cells total (9 logic + 12 SB_IO + 1 SB_VCC)"
        );
    }
}
