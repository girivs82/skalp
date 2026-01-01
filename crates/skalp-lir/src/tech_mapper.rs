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
use crate::lir::{Lir, LirNode, LirOp, LirSafetyInfo, LirSignalId};
use crate::tech_library::{
    CellFunction, DecompConnectivity, LibraryCell, LibraryFailureMode, TechLibrary,
};
use std::collections::HashMap;

/// Information extracted from a library cell for tech mapping
struct LibraryCellInfo {
    name: String,
    fit: f64,
    failure_modes: Vec<CellFailureMode>,
}

impl LibraryCellInfo {
    /// Create from a library cell reference
    fn from_library_cell(cell: &LibraryCell) -> Self {
        Self {
            name: cell.name.clone(),
            fit: cell.fit,
            failure_modes: cell
                .failure_modes
                .iter()
                .map(convert_failure_mode)
                .collect(),
        }
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
}

/// Technology mapper
pub struct TechMapper<'a> {
    /// Technology library
    library: &'a TechLibrary,
    /// Output netlist
    netlist: GateNetlist,
    /// Mapping from LirSignalId to GateNetId (for multi-bit, maps to first bit)
    signal_to_net: HashMap<LirSignalId, Vec<GateNetId>>,
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
}

impl<'a> TechMapper<'a> {
    /// Create a new technology mapper
    pub fn new(library: &'a TechLibrary) -> Self {
        Self {
            library,
            netlist: GateNetlist::new(String::new(), library.name.clone()),
            signal_to_net: HashMap::new(),
            stats: TechMapStats::default(),
            warnings: Vec::new(),
            next_cell_id: 0,
            module_safety_info: None,
            c_elem_counter: 0,
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

        // Phase 2: Map each node to cells
        for node in &word_lir.nodes {
            self.map_node(node, word_lir);
            self.stats.nodes_processed += 1;
        }

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
            LirOp::Add { width, has_carry } => {
                self.map_adder(*width, *has_carry, &input_nets, &output_nets, &node.path);
            }

            // Subtractor
            LirOp::Sub { width, .. } => {
                self.map_subtractor(*width, &input_nets, &output_nets, &node.path);
            }

            // Multiplier
            LirOp::Mul {
                width,
                result_width,
            } => {
                self.map_multiplier(*width, *result_width, &input_nets, &output_nets, &node.path);
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
                // For now, treat same as unsigned - TODO: implement proper signed comparison
                self.map_less_than(*width, &input_nets, &output_nets, &node.path);
                self.warnings
                    .push("Signed LessThan treated as unsigned - may be incorrect".to_string());
            }

            // Register
            LirOp::Reg {
                width, has_reset, ..
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
                self.map_register(
                    *width,
                    &input_nets,
                    &output_nets,
                    &node.path,
                    clock_net,
                    reset_net,
                );
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
                self.map_shift(*width, &input_nets, &output_nets, &node.path, true);
            }
            LirOp::Shr { width } => {
                self.map_shift(*width, &input_nets, &output_nets, &node.path, false);
            }
            LirOp::Sar { width } => {
                // Arithmetic right shift - same as logical for now, but should sign-extend
                self.map_shift(*width, &input_nets, &output_nets, &node.path, false);
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

            // Floating-Point operations (soft macros)
            LirOp::FpAdd { width } => {
                self.map_fp_operation(
                    CellFunction::FpAdd32,
                    *width,
                    &input_nets,
                    &output_nets,
                    &node.path,
                );
            }
            LirOp::FpSub { width } => {
                self.map_fp_operation(
                    CellFunction::FpSub32,
                    *width,
                    &input_nets,
                    &output_nets,
                    &node.path,
                );
            }
            LirOp::FpMul { width } => {
                self.map_fp_operation(
                    CellFunction::FpMul32,
                    *width,
                    &input_nets,
                    &output_nets,
                    &node.path,
                );
            }
            LirOp::FpDiv { width } => {
                self.map_fp_operation(
                    CellFunction::FpDiv32,
                    *width,
                    &input_nets,
                    &output_nets,
                    &node.path,
                );
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

            _ => {
                self.warnings
                    .push(format!("Unsupported operation: {:?}", node.op));
            }
        }
    }

    /// Get input nets from input signal IDs
    fn get_input_nets(&self, inputs: &[LirSignalId]) -> Vec<Vec<GateNetId>> {
        inputs
            .iter()
            .filter_map(|id| self.signal_to_net.get(id).cloned())
            .collect()
    }

    /// Get output nets for an output signal ID
    fn get_output_nets(&self, output: LirSignalId) -> Vec<GateNetId> {
        self.signal_to_net.get(&output).cloned().unwrap_or_default()
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

        for bit in 0..width as usize {
            let a = inputs[0].get(bit).copied().unwrap_or(inputs[0][0]);
            let b = inputs[1].get(bit).copied().unwrap_or(inputs[1][0]);
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
            gate_cell.failure_modes = cell_info.failure_modes.clone();
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

        for bit in 0..width as usize {
            let a = inputs[0].get(bit).copied().unwrap_or(inputs[0][0]);
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
            gate_cell.failure_modes = cell_info.failure_modes.clone();
            self.add_cell(gate_cell);
        }

        self.stats.direct_mappings += 1;
    }

    /// Map an adder using ripple carry
    fn map_adder(
        &mut self,
        width: u32,
        _has_carry: bool,
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
                adder_info.name,
                self.library.name.clone(),
                adder_info.fit,
                path.to_string(),
                all_inputs,
                outputs.to_vec(),
            );
            cell.source_op = Some("Adder".to_string());
            cell.failure_modes = adder_info.failure_modes;
            self.add_cell(cell);
            self.stats.direct_mappings += 1;
            return;
        }

        // Otherwise, decompose to half adder + full adders
        let ha_info = self.get_cell_info(&CellFunction::HalfAdder);
        let fa_info = self.get_cell_info(&CellFunction::FullAdder);

        let mut carry_net: Option<GateNetId> = None;

        for bit in 0..width as usize {
            let a = inputs[0].get(bit).copied().unwrap_or(inputs[0][0]);
            let b = inputs[1].get(bit).copied().unwrap_or(inputs[1][0]);
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
                cell.failure_modes = fa_info.failure_modes.clone();
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
                cell.failure_modes = ha_info.failure_modes.clone();
                self.add_cell(cell);
            }

            carry_net = Some(cout);
        }

        self.stats.decomposed_mappings += 1;
    }

    /// Map a subtractor (a - b = a + ~b + 1)
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

        // Invert b, then add with carry_in=1
        let inv_info = self.get_cell_info(&CellFunction::Inv);
        let fa_info = self.get_cell_info(&CellFunction::FullAdder);

        // Create inverted b nets
        let mut inv_b: Vec<GateNetId> = Vec::new();
        for bit in 0..width as usize {
            let b = inputs[1].get(bit).copied().unwrap_or(inputs[1][0]);
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
            cell.failure_modes = inv_info.failure_modes.clone();
            self.add_cell(cell);
            inv_b.push(not_b);
        }

        // Create constant 1 for initial carry (carry_in = 1 for two's complement subtraction)
        let const_1 = self.alloc_net_id();
        self.netlist
            .add_net(GateNet::new(const_1, format!("{}.const_1", path)));
        self.stats.nets_created += 1;

        // Add TIE_HIGH cell to drive the constant 1 net
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
            let a = inputs[0].get(bit).copied().unwrap_or(inputs[0][0]);
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
            cell.failure_modes = fa_info.failure_modes.clone();
            self.add_cell(cell);

            carry_net = cout;
        }

        self.stats.decomposed_mappings += 1;
    }

    /// Map a multiplier using shift-and-add algorithm
    /// result = a * b, where both inputs have 'width' bits and result has 'result_width' bits
    fn map_multiplier(
        &mut self,
        width: u32,
        result_width: u32,
        inputs: &[Vec<GateNetId>],
        outputs: &[GateNetId],
        path: &str,
    ) {
        if inputs.len() < 2 {
            self.warnings
                .push(format!("Multiplier needs 2 inputs, got {}", inputs.len()));
            return;
        }

        let and_info = self.get_cell_info(&CellFunction::And2);
        let ha_info = self.get_cell_info(&CellFunction::HalfAdder);
        let fa_info = self.get_cell_info(&CellFunction::FullAdder);

        // Create partial products: pp[i][j] = a[j] & b[i]
        // This creates a grid of AND gates
        let mut partial_products: Vec<Vec<GateNetId>> = Vec::new();

        for i in 0..width as usize {
            let mut row: Vec<GateNetId> = Vec::new();
            let b_i = inputs[1].get(i).copied().unwrap_or(inputs[1][0]);

            for j in 0..width as usize {
                let a_j = inputs[0].get(j).copied().unwrap_or(inputs[0][0]);
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
            if j < width as usize {
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
        for i in 1..width as usize {
            let mut next_sum: Vec<GateNetId> = Vec::new();
            let mut carry: Option<GateNetId> = None;

            for j in 0..result_width as usize {
                // Get the partial product bit (shifted by i)
                let pp_bit = if j >= i && j - i < width as usize {
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
                    cell.failure_modes = fa_info.failure_modes.clone();
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
                    cell.failure_modes = ha_info.failure_modes.clone();
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

        let sel = inputs[0].first().copied().unwrap_or(GateNetId(0));

        for bit in 0..width as usize {
            let d0 = inputs[1].get(bit).copied().unwrap_or(inputs[1][0]);
            let d1 = inputs[2].get(bit).copied().unwrap_or(inputs[2][0]);
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
            cell.failure_modes = mux_info.failure_modes.clone();
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

        // Create a constant 0 net for filling in shifted bits
        let const_0 = self.alloc_net_id();
        self.netlist
            .add_net(GateNet::new(const_0, format!("{}.const_0", path)));
        self.stats.nets_created += 1;

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
                    // Right shift: when sel=1, bit[i] gets bit[i+shift_amount] (or 0)
                    let unshifted = current[bit];
                    let shifted = if bit + shift_amount < width as usize {
                        current[bit + shift_amount]
                    } else {
                        const_0
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
                cell.failure_modes = mux_info.failure_modes.clone();
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
                cell.failure_modes = buf_info.failure_modes.clone();
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
            cell.failure_modes = buf_info.failure_modes.clone();
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
                cell.failure_modes = buf_info.failure_modes.clone();
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
            cell.failure_modes = buf_info.failure_modes.clone();
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
            cell.failure_modes = buf_info.failure_modes.clone();
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
            cell.failure_modes = buf_info.failure_modes.clone();
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

        for bit in 0..width as usize {
            let a = inputs[0].get(bit).copied().unwrap_or(inputs[0][0]);
            let b = inputs[1].get(bit).copied().unwrap_or(inputs[1][0]);
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

    /// Map a floating-point operation as a soft-macro cell
    ///
    /// FP operations are mapped as single cells with all 32 input bits for A,
    /// all 32 input bits for B, and all 32 output bits. These cells represent
    /// IEEE 754 floating-point operations that can be:
    /// - Simulated at gate level using software FP computation
    /// - Replaced with actual FP IP during physical implementation
    fn map_fp_operation(
        &mut self,
        function: CellFunction,
        width: u32,
        inputs: &[Vec<GateNetId>],
        outputs: &[GateNetId],
        path: &str,
    ) {
        if inputs.len() < 2 {
            self.warnings
                .push(format!("FP operation needs 2 inputs, got {}", inputs.len()));
            return;
        }

        // Collect all input nets (A and B operands)
        let mut all_inputs: Vec<GateNetId> = Vec::new();

        // Add all bits of operand A
        for bit in 0..width as usize {
            let net = inputs[0].get(bit).copied().unwrap_or(inputs[0][0]);
            all_inputs.push(net);
        }

        // Add all bits of operand B
        for bit in 0..width as usize {
            let net = inputs[1].get(bit).copied().unwrap_or(inputs[1][0]);
            all_inputs.push(net);
        }

        // Collect all output nets
        let mut all_outputs: Vec<GateNetId> = Vec::new();
        for bit in 0..width as usize {
            let net = outputs.get(bit).copied().unwrap_or(outputs[0]);
            all_outputs.push(net);
        }

        // Create the soft-macro cell
        let cell_name = match function {
            CellFunction::FpAdd32 => "FP32_ADD",
            CellFunction::FpSub32 => "FP32_SUB",
            CellFunction::FpMul32 => "FP32_MUL",
            CellFunction::FpDiv32 => "FP32_DIV",
            _ => "FP32_UNKNOWN",
        };

        let mut cell = Cell::new_comb(
            CellId(0),
            cell_name.to_string(),
            self.library.name.clone(),
            100.0, // Higher FIT for complex FP unit
            format!("{}.fp_op", path),
            all_inputs,
            all_outputs,
        );
        cell.source_op = Some(format!("{:?}", function));
        self.add_cell(cell);

        self.stats.direct_mappings += 1;
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
                cell.failure_modes = mux_info.failure_modes.clone();
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

        // XNOR each bit pair
        let mut xnor_outs: Vec<GateNetId> = Vec::new();
        for bit in 0..width as usize {
            let a = inputs[0].get(bit).copied().unwrap_or(inputs[0][0]);
            let b = inputs[1].get(bit).copied().unwrap_or(inputs[1][0]);

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
            cell.failure_modes = xnor_info.failure_modes.clone();
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
                inv_info.name,
                self.library.name.clone(),
                inv_info.fit,
                format!("{}.ne_inv", path),
                vec![eq_out],
                vec![y],
            );
            cell.source_op = Some("Inverter".to_string());
            cell.failure_modes = inv_info.failure_modes;
            self.add_cell(cell);
        } else {
            // Connect eq_out to output (add buffer if needed)
            let y = outputs.first().copied().unwrap_or(GateNetId(0));
            if eq_out != y {
                let buf_info = self.get_cell_info(&CellFunction::Buf);

                let mut cell = Cell::new_comb(
                    CellId(0),
                    buf_info.name,
                    self.library.name.clone(),
                    buf_info.fit,
                    format!("{}.eq_buf", path),
                    vec![eq_out],
                    vec![y],
                );
                cell.source_op = Some("Buffer".to_string());
                cell.failure_modes = buf_info.failure_modes;
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

        // Start from LSB and work up
        // prev_lt carries whether a[i-1:0] < b[i-1:0] based on lower bits
        // Formula: lt_i = lt_bit_i | (eq_i & prev_lt)
        // where lt_bit_i = ~a[i] & b[i] (a < b at this bit position)
        let mut prev_lt = None; // None means 0 (not less than so far)

        for i in 0..width as usize {
            let a_bit = a.get(i).copied().unwrap_or(a[0]);
            let b_bit = b.get(i).copied().unwrap_or(b[0]);

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

    /// Map a register
    fn map_register(
        &mut self,
        width: u32,
        inputs: &[Vec<GateNetId>],
        outputs: &[GateNetId],
        path: &str,
        clock: Option<GateNetId>,
        reset: Option<GateNetId>,
    ) {
        if inputs.is_empty() {
            self.warnings
                .push("Register needs at least 1 input (d)".to_string());
            return;
        }

        let dff_func = if reset.is_some() {
            CellFunction::DffR
        } else {
            CellFunction::Dff
        };

        let dff_info = self.get_cell_info(&dff_func);

        let clk = clock.unwrap_or(GateNetId(0));

        for bit in 0..width as usize {
            let d = inputs[0].get(bit).copied().unwrap_or(inputs[0][0]);
            let q = outputs.get(bit).copied().unwrap_or(outputs[0]);

            // D input only - reset is passed separately to Cell::new_seq
            // and rendered as .RST in Verilog output
            let dff_inputs = vec![d];

            let mut cell = Cell::new_seq(
                CellId(0),
                dff_info.name.clone(),
                self.library.name.clone(),
                dff_info.fit,
                format!("{}.bit{}", path, bit),
                dff_inputs,
                vec![q],
                clk,
                reset,
            );
            cell.source_op = Some("Register".to_string());
            cell.failure_modes = dff_info.failure_modes.clone();
            self.add_cell(cell);
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
                buf_info.name,
                self.library.name.clone(),
                buf_info.fit,
                format!("{}.reduce_buf", path),
                vec![result],
                vec![y],
            );
            cell.source_op = Some("Buffer".to_string());
            cell.failure_modes = buf_info.failure_modes;
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
                    cell.failure_modes = cell_info.failure_modes.clone();
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

    /// Map NCL encode: single-rail → dual-rail
    /// For each input bit, generates (t, f) where t=input, f=!input
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

        let inv_info = self.get_cell_info(&CellFunction::Inv);

        for i in 0..width as usize {
            let input_bit = inputs[0].get(i).copied().unwrap_or(GateNetId(0));
            let out_t = outputs.get(i * 2).copied().unwrap_or(GateNetId(0));
            let out_f = outputs.get(i * 2 + 1).copied().unwrap_or(GateNetId(0));

            // True rail is the input directly (via buffer)
            let buf_info = self.get_cell_info(&CellFunction::Buf);
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

            // False rail is inverted input
            let mut inv_cell = Cell::new_comb(
                CellId(0),
                inv_info.name.clone(),
                self.library.name.clone(),
                inv_info.fit,
                format!("{}.enc_f{}", path, i),
                vec![input_bit],
                vec![out_f],
            );
            inv_cell.source_op = Some("NclEncode_F".to_string());
            self.add_cell(inv_cell);
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
            // Input is dual-rail: [t0, f0, t1, f1, ...]
            let in_t = inputs[0].get(i * 2).copied().unwrap_or(GateNetId(0));
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
        self.map_multiplier(
            input_width,
            result_width,
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

        let th12_info = self.get_ncl_cell_info(&CellFunction::Th12);
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

            // TH12(t, f) - complete when either rail is high
            self.add_cell(Cell::new_comb(
                CellId(0),
                th12_info.name.clone(),
                self.library.name.clone(),
                th12_info.fit,
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
                    fit: 0.5 + 0.1 * (*n as f64),
                    failure_modes: vec![],
                };
            }
            CellFunction::NclCompletion { width } => {
                return LibraryCellInfo {
                    name: format!("NCL_COMPLETE{}", width),
                    fit: 0.1 * (*width as f64),
                    failure_modes: vec![],
                };
            }
            _ => ("NCL_GENERIC", 0.5),
        };

        LibraryCellInfo {
            name: name.to_string(),
            fit,
            failure_modes: vec![],
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

        let th22_info = self.get_ncl_cell_info(&CellFunction::Th22);
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
                        th22_info.name.clone(),
                        self.library.name.clone(),
                        th22_info.fit,
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

        let th12_info = self.get_ncl_cell_info(&CellFunction::Th12);
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
                        th12_info.name.clone(),
                        self.library.name.clone(),
                        th12_info.fit,
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

/// Map a Lir to GateNetlist and run gate-level optimizations
pub fn map_lir_to_gates_optimized(lir: &Lir, library: &TechLibrary) -> TechMapResult {
    use crate::gate_optimizer::GateOptimizer;

    let mut mapper = TechMapper::new(library);
    let mut result = mapper.map(lir);

    // Run gate-level optimizations
    let mut optimizer = GateOptimizer::new();
    let opt_stats = optimizer.optimize(&mut result.netlist);

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
    for (path, inst_lir) in &hier_lir.instances {
        // Check if this is a compiled IP (pre-compiled netlist)
        let tech_result = if let Some(ref compiled_ip_path) = inst_lir.lir_result.compiled_ip_path {
            // Load the pre-compiled netlist directly
            match CompiledIp::read_from_file(std::path::Path::new(compiled_ip_path), None) {
                Ok(compiled_ip) => {
                    eprintln!(
                        "📦 COMPILED_IP: Using pre-compiled netlist for '{}' from '{}'",
                        inst_lir.module_name, compiled_ip_path
                    );
                    TechMapResult {
                        netlist: compiled_ip.netlist.clone(),
                        stats: TechMapStats {
                            nodes_processed: 0,
                            cells_created: compiled_ip.netlist.cells.len(),
                            nets_created: compiled_ip.netlist.nets.len(),
                            direct_mappings: 0,
                            decomposed_mappings: 0,
                        },
                        warnings: vec![format!(
                            "Loaded pre-compiled netlist from '{}'",
                            compiled_ip_path
                        )],
                    }
                }
                Err(e) => {
                    eprintln!(
                        "⚠️ COMPILED_IP: Failed to load '{}', falling back to synthesis: {}",
                        compiled_ip_path, e
                    );
                    map_lir_to_gates_optimized(&inst_lir.lir_result.lir, library)
                }
            }
        } else if let Some(ref blackbox_info) = inst_lir.lir_result.blackbox_info {
            // This is a blackbox/vendor IP - create a netlist with a single blackbox cell
            eprintln!(
                "🔌 BLACKBOX: Creating blackbox cell '{}' for instance '{}'",
                blackbox_info.cell_name, path
            );
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
        for (port_name, conn_info) in &inst_lir.port_connections {
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
        fit: 0.0, // Unknown FIT for blackbox
        failure_modes: Vec::new(),
        inputs: input_nets.clone(),
        outputs: output_nets.clone(),
        path: format!("{}.{}", module_name, blackbox_info.cell_name),
        clock: None,
        reset: None,
        source_op: Some(format!("blackbox:{}", blackbox_info.cell_name)),
        safety_classification: CellSafetyClassification::Functional,
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

        // Should create 8 AND gates (one per bit)
        assert_eq!(result.stats.cells_created, 8);
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
            },
            vec![a, b],
            sum,
            "test.add".to_string(),
        );

        let result = map_word_lir_to_gates(&word_lir, &lib);

        // Should create 1 half adder + 3 full adders
        assert_eq!(result.stats.cells_created, 4);
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

        // Should create 16 MUX2 cells
        assert_eq!(result.stats.cells_created, 16);
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
                reset_value: None,
            },
            vec![d],
            q,
            "test.reg".to_string(),
            clk,
            None,
        );

        let result = map_word_lir_to_gates(&word_lir, &lib);

        // Should create 8 DFF cells
        assert_eq!(result.stats.cells_created, 8);

        // All should be sequential
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

        // 4 inverters at 0.05 FIT each = 0.2 total
        assert!((result.netlist.total_fit() - 0.2).abs() < 0.001);
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

        // Verify cell was created
        assert_eq!(result.netlist.cells.len(), 1);
        let cell = &result.netlist.cells[0];

        // Verify failure modes were copied
        assert_eq!(cell.failure_modes.len(), 3);
        assert_eq!(cell.failure_modes[0].name, "stuck_at_0");
        assert_eq!(cell.failure_modes[0].fault_type, FaultType::StuckAt0);
        assert!((cell.failure_modes[0].fit - 0.03).abs() < 0.001);

        assert_eq!(cell.failure_modes[1].name, "stuck_at_1");
        assert_eq!(cell.failure_modes[2].name, "transient");
        assert_eq!(cell.failure_modes[2].fault_type, FaultType::Transient);
    }
}
