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

    /// Create a default fallback when no library cell is found
    fn fallback(name: String, fit: f64) -> Self {
        Self {
            name,
            fit,
            failure_modes: Vec::new(),
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
    /// Next net ID
    next_net_id: u32,
    /// Module-level safety info (from Lir)
    /// Applied to all cells during mapping
    module_safety_info: Option<LirSafetyInfo>,
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
            next_net_id: 0,
            module_safety_info: None,
        }
    }

    /// Get library cell info for a function, with fallback
    fn get_cell_info(&self, function: &CellFunction, default_fit: f64) -> LibraryCellInfo {
        self.library
            .find_best_cell(function)
            .map(LibraryCellInfo::from_library_cell)
            .unwrap_or_else(|| LibraryCellInfo::fallback(format!("{:?}", function), default_fit))
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

    /// Map a single node to cells
    fn map_node(&mut self, node: &LirNode, word_lir: &Lir) {
        let input_nets = self.get_input_nets(&node.inputs);
        let output_nets = self.get_output_nets(node.output);

        match &node.op {
            // Simple logic gates - decompose to per-bit operations
            LirOp::And { width } => {
                self.map_bitwise_gate(
                    CellFunction::And2,
                    *width,
                    &input_nets,
                    &output_nets,
                    &node.path,
                );
            }
            LirOp::Or { width } => {
                self.map_bitwise_gate(
                    CellFunction::Or2,
                    *width,
                    &input_nets,
                    &output_nets,
                    &node.path,
                );
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

            // Adder - ripple carry chain
            LirOp::Add { width, has_carry } => {
                self.map_adder(*width, *has_carry, &input_nets, &output_nets, &node.path);
            }

            // Subtractor
            LirOp::Sub { width, .. } => {
                self.map_subtractor(*width, &input_nets, &output_nets, &node.path);
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
                    let inv_info = self.get_cell_info(&CellFunction::Inv, 0.05);
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
                    let inv_info = self.get_cell_info(&CellFunction::Inv, 0.05);
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

            // Buffer
            LirOp::Buffer { width } => {
                self.map_unary_gate(
                    CellFunction::Buf,
                    *width,
                    &input_nets,
                    &output_nets,
                    &node.path,
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

        let cell_info = self.get_cell_info(&function, 0.1);

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

        let cell_info = self.get_cell_info(&function, 0.05);

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
        let ha_info = self.get_cell_info(&CellFunction::HalfAdder, 0.2);
        let fa_info = self.get_cell_info(&CellFunction::FullAdder, 0.3);

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
        let inv_info = self.get_cell_info(&CellFunction::Inv, 0.05);
        let fa_info = self.get_cell_info(&CellFunction::FullAdder, 0.3);

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

        // Create constant 1 for initial carry
        let const_1 = self.alloc_net_id();
        self.netlist
            .add_net(GateNet::new(const_1, format!("{}.const_1", path)));
        self.stats.nets_created += 1;
        // Note: We'd need a tie-high cell here, but we'll just use it as a signal

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

        let mux_info = self.get_cell_info(&CellFunction::Mux2, 0.15);

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

        let mux_info = self.get_cell_info(&CellFunction::Mux2, 0.15);

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
                let buf_info = self.get_cell_info(&CellFunction::Buf, 0.05);
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
        let buf_info = self.get_cell_info(&CellFunction::Buf, 0.05);

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
        let mux_info = self.get_cell_info(&CellFunction::Mux2, 0.15);

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

        let xnor_info = self.get_cell_info(&CellFunction::Xnor2, 0.1);
        let and_info = self.get_cell_info(&CellFunction::And2, 0.1);

        // XNOR each bit pair
        let mut xnor_outs: Vec<GateNetId> = Vec::new();
        for bit in 0..width as usize {
            let a = inputs[0].get(bit).copied().unwrap_or(inputs[0][0]);
            let b = inputs[1].get(bit).copied().unwrap_or(inputs[1][0]);

            let xnor_out = self.alloc_net_id();
            self.netlist
                .add_net(GateNet::new(xnor_out, format!("{}.xnor{}", path, bit)));
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
            let inv_info = self.get_cell_info(&CellFunction::Inv, 0.05);

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
                let buf_info = self.get_cell_info(&CellFunction::Buf, 0.05);

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
        let and_info = self.get_cell_info(&CellFunction::And2, 0.1);
        let or_info = self.get_cell_info(&CellFunction::Or2, 0.1);
        let inv_info = self.get_cell_info(&CellFunction::Inv, 0.05);
        let xnor_info = self.get_cell_info(&CellFunction::Xnor2, 0.1);

        // Start from MSB and work down
        // prev_lt carries whether a < b based on higher bits
        let mut prev_lt = None; // None means 0 (not less than so far)

        for i in (0..width as usize).rev() {
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
            // But if this is the MSB, prev_lt is 0 so lt_i = lt_bit_i
            let lt_net = if i == width as usize - 1 {
                // MSB: lt = lt_bit
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

                let combined = if i == 0 {
                    y // Use output net for final result
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
                let buf_info = self.get_cell_info(&CellFunction::Buf, 0.05);
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

        let dff_info = self.get_cell_info(&dff_func, 0.2);

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
        // Constants are typically implemented with tie-high/tie-low cells
        // For simplicity, we just mark the nets as driven by constants
        for bit in 0..width as usize {
            let bit_val = (value >> bit) & 1 != 0;
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

        let cell_info = self.get_cell_info(&function, 0.1);

        // Get all input bits
        let input_bits: Vec<GateNetId> = inputs[0].iter().take(width as usize).copied().collect();

        // Build reduction tree
        let result = self.reduce_tree(&input_bits, &cell_info, path, "reduce");

        // Connect to output
        let y = outputs.first().copied().unwrap_or(GateNetId(0));
        if result != y {
            let buf_info = self.get_cell_info(&CellFunction::Buf, 0.05);

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
    fn alloc_net_id(&mut self) -> GateNetId {
        let id = GateNetId(self.next_net_id);
        self.next_net_id += 1;
        id
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
    use crate::hierarchical_netlist::{HierarchicalNetlist, InstanceNetlist, PortConnection};
    use crate::mir_to_lir::PortConnectionInfo;

    let mut result = HierarchicalNetlist::new(hier_lir.top_module.clone(), library.name.clone());

    // Map each instance's LIR to a GateNetlist
    for (path, inst_lir) in &hier_lir.instances {
        let tech_result = map_lir_to_gates_optimized(&inst_lir.lir_result.lir, library);
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
