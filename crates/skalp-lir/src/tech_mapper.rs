//! Technology Mapper
//!
//! Maps word-level operations (WordLir) to gate-level primitives (GateNetlist)
//! using a technology library.
//!
//! # Flow
//!
//! ```text
//! WordLir (word-level) → TechMapper → GateNetlist (gate-level with library cells)
//! ```
//!
//! The mapper:
//! 1. For each word-level operation, finds matching library cells
//! 2. Applies decomposition rules if no direct match
//! 3. Creates GateNetlist with FIT rates from the library

use crate::gate_netlist::{Cell, CellFailureMode, CellId, GateNet, GateNetId, GateNetlist};
use crate::tech_library::{CellFunction, DecompConnectivity, TechLibrary};
use crate::word_lir::{WordLir, WordNode, WordOp, WordSignalId};
use std::collections::HashMap;

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
    /// Mapping from WordSignalId to GateNetId (for multi-bit, maps to first bit)
    signal_to_net: HashMap<WordSignalId, Vec<GateNetId>>,
    /// Statistics
    stats: TechMapStats,
    /// Warnings
    warnings: Vec<String>,
    /// Next cell ID
    next_cell_id: u32,
    /// Next net ID
    next_net_id: u32,
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
        }
    }

    /// Map a WordLir to a GateNetlist
    pub fn map(&mut self, word_lir: &WordLir) -> TechMapResult {
        self.netlist.name = word_lir.name.clone();

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
        _signal_id: WordSignalId,
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
    fn map_node(&mut self, node: &WordNode, word_lir: &WordLir) {
        let input_nets = self.get_input_nets(&node.inputs);
        let output_nets = self.get_output_nets(node.output);

        match &node.op {
            // Simple logic gates - decompose to per-bit operations
            WordOp::And { width } => {
                self.map_bitwise_gate(
                    CellFunction::And2,
                    *width,
                    &input_nets,
                    &output_nets,
                    &node.path,
                );
            }
            WordOp::Or { width } => {
                self.map_bitwise_gate(
                    CellFunction::Or2,
                    *width,
                    &input_nets,
                    &output_nets,
                    &node.path,
                );
            }
            WordOp::Xor { width } => {
                self.map_bitwise_gate(
                    CellFunction::Xor2,
                    *width,
                    &input_nets,
                    &output_nets,
                    &node.path,
                );
            }
            WordOp::Nand { width } => {
                self.map_bitwise_gate(
                    CellFunction::Nand2,
                    *width,
                    &input_nets,
                    &output_nets,
                    &node.path,
                );
            }
            WordOp::Nor { width } => {
                self.map_bitwise_gate(
                    CellFunction::Nor2,
                    *width,
                    &input_nets,
                    &output_nets,
                    &node.path,
                );
            }
            WordOp::Not { width } => {
                self.map_unary_gate(
                    CellFunction::Inv,
                    *width,
                    &input_nets,
                    &output_nets,
                    &node.path,
                );
            }

            // Adder - ripple carry chain
            WordOp::Add { width, has_carry } => {
                self.map_adder(*width, *has_carry, &input_nets, &output_nets, &node.path);
            }

            // Subtractor
            WordOp::Sub { width, .. } => {
                self.map_subtractor(*width, &input_nets, &output_nets, &node.path);
            }

            // Multiplexer
            WordOp::Mux2 { width } => {
                self.map_mux2(*width, &input_nets, &output_nets, &node.path);
            }

            // Comparators
            WordOp::Eq { width } => {
                self.map_equality(*width, &input_nets, &output_nets, &node.path, false);
            }
            WordOp::Ne { width } => {
                self.map_equality(*width, &input_nets, &output_nets, &node.path, true);
            }
            WordOp::Lt { width } => {
                self.map_less_than(*width, &input_nets, &output_nets, &node.path);
            }

            // Register
            WordOp::Reg {
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
            WordOp::Buffer { width } => {
                self.map_unary_gate(
                    CellFunction::Buf,
                    *width,
                    &input_nets,
                    &output_nets,
                    &node.path,
                );
            }

            // Constant
            WordOp::Constant { width, value } => {
                self.map_constant(*width, *value, &output_nets, &node.path);
            }

            // Reductions
            WordOp::RedAnd { width } => {
                self.map_reduction(
                    CellFunction::And2,
                    *width,
                    &input_nets,
                    &output_nets,
                    &node.path,
                );
            }
            WordOp::RedOr { width } => {
                self.map_reduction(
                    CellFunction::Or2,
                    *width,
                    &input_nets,
                    &output_nets,
                    &node.path,
                );
            }
            WordOp::RedXor { width } => {
                self.map_reduction(
                    CellFunction::Xor2,
                    *width,
                    &input_nets,
                    &output_nets,
                    &node.path,
                );
            }

            _ => {
                self.warnings
                    .push(format!("Unsupported operation: {:?}", node.op));
            }
        }
    }

    /// Get input nets from input signal IDs
    fn get_input_nets(&self, inputs: &[WordSignalId]) -> Vec<Vec<GateNetId>> {
        inputs
            .iter()
            .filter_map(|id| self.signal_to_net.get(id).cloned())
            .collect()
    }

    /// Get output nets for an output signal ID
    fn get_output_nets(&self, output: WordSignalId) -> Vec<GateNetId> {
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

        let cell = self.library.find_best_cell(&function);
        let (cell_name, fit) = cell
            .map(|c| (c.name.clone(), c.fit))
            .unwrap_or_else(|| (format!("{:?}", function), 0.1));

        for bit in 0..width as usize {
            let a = inputs[0].get(bit).copied().unwrap_or(inputs[0][0]);
            let b = inputs[1].get(bit).copied().unwrap_or(inputs[1][0]);
            let y = outputs.get(bit).copied().unwrap_or(outputs[0]);

            let mut gate_cell = Cell::new_comb(
                CellId(0),
                cell_name.clone(),
                self.library.name.clone(),
                fit,
                format!("{}.bit{}", path, bit),
                vec![a, b],
                vec![y],
            );
            gate_cell.source_op = Some(format!("{:?}", function));
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

        let cell = self.library.find_best_cell(&function);
        let (cell_name, fit) = cell
            .map(|c| (c.name.clone(), c.fit))
            .unwrap_or_else(|| (format!("{:?}", function), 0.05));

        for bit in 0..width as usize {
            let a = inputs[0].get(bit).copied().unwrap_or(inputs[0][0]);
            let y = outputs.get(bit).copied().unwrap_or(outputs[0]);

            let mut gate_cell = Cell::new_comb(
                CellId(0),
                cell_name.clone(),
                self.library.name.clone(),
                fit,
                format!("{}.bit{}", path, bit),
                vec![a],
                vec![y],
            );
            gate_cell.source_op = Some(format!("{:?}", function));
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
            // Use the compound adder cell
            let all_inputs: Vec<GateNetId> =
                inputs[0].iter().chain(inputs[1].iter()).copied().collect();

            let mut cell = Cell::new_comb(
                CellId(0),
                adder.name.clone(),
                self.library.name.clone(),
                adder.fit,
                path.to_string(),
                all_inputs,
                outputs.to_vec(),
            );
            cell.source_op = Some("Adder".to_string());
            self.add_cell(cell);
            self.stats.direct_mappings += 1;
            return;
        }

        // Otherwise, decompose to half adder + full adders
        let ha_cell = self.library.find_best_cell(&CellFunction::HalfAdder);
        let fa_cell = self.library.find_best_cell(&CellFunction::FullAdder);

        let (ha_name, ha_fit) = ha_cell
            .map(|c| (c.name.clone(), c.fit))
            .unwrap_or_else(|| ("HalfAdder".to_string(), 0.2));
        let (fa_name, fa_fit) = fa_cell
            .map(|c| (c.name.clone(), c.fit))
            .unwrap_or_else(|| ("FullAdder".to_string(), 0.3));

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
                    fa_name.clone(),
                    self.library.name.clone(),
                    fa_fit,
                    format!("{}.fa{}", path, bit),
                    vec![a, b, cin],
                    vec![sum, cout],
                );
                cell.source_op = Some("FullAdder".to_string());
                self.add_cell(cell);
            } else {
                // Half adder for first bit
                let mut cell = Cell::new_comb(
                    CellId(0),
                    ha_name.clone(),
                    self.library.name.clone(),
                    ha_fit,
                    format!("{}.ha{}", path, bit),
                    vec![a, b],
                    vec![sum, cout],
                );
                cell.source_op = Some("HalfAdder".to_string());
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
        let inv_cell = self.library.find_best_cell(&CellFunction::Inv);
        let (inv_name, inv_fit) = inv_cell
            .map(|c| (c.name.clone(), c.fit))
            .unwrap_or_else(|| ("INV".to_string(), 0.05));

        let fa_cell = self.library.find_best_cell(&CellFunction::FullAdder);
        let (fa_name, fa_fit) = fa_cell
            .map(|c| (c.name.clone(), c.fit))
            .unwrap_or_else(|| ("FullAdder".to_string(), 0.3));

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
                inv_name.clone(),
                self.library.name.clone(),
                inv_fit,
                format!("{}.inv_b{}", path, bit),
                vec![b],
                vec![not_b],
            );
            cell.source_op = Some("Inverter".to_string());
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
                fa_name.clone(),
                self.library.name.clone(),
                fa_fit,
                format!("{}.sub_fa{}", path, bit),
                vec![a, not_b, carry_net],
                vec![diff, cout],
            );
            cell.source_op = Some("FullAdder".to_string());
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

        let mux_cell = self.library.find_best_cell(&CellFunction::Mux2);
        let (mux_name, mux_fit) = mux_cell
            .map(|c| (c.name.clone(), c.fit))
            .unwrap_or_else(|| ("MUX2".to_string(), 0.15));

        let sel = inputs[0].first().copied().unwrap_or(GateNetId(0));

        for bit in 0..width as usize {
            let d0 = inputs[1].get(bit).copied().unwrap_or(inputs[1][0]);
            let d1 = inputs[2].get(bit).copied().unwrap_or(inputs[2][0]);
            let y = outputs.get(bit).copied().unwrap_or(outputs[0]);

            let mut cell = Cell::new_comb(
                CellId(0),
                mux_name.clone(),
                self.library.name.clone(),
                mux_fit,
                format!("{}.bit{}", path, bit),
                vec![sel, d0, d1],
                vec![y],
            );
            cell.source_op = Some("Mux2".to_string());
            self.add_cell(cell);
        }

        self.stats.direct_mappings += 1;
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

        let xnor_cell = self.library.find_best_cell(&CellFunction::Xnor2);
        let (xnor_name, xnor_fit) = xnor_cell
            .map(|c| (c.name.clone(), c.fit))
            .unwrap_or_else(|| ("XNOR2".to_string(), 0.1));

        let and_cell = self.library.find_best_cell(&CellFunction::And2);
        let (and_name, and_fit) = and_cell
            .map(|c| (c.name.clone(), c.fit))
            .unwrap_or_else(|| ("AND2".to_string(), 0.1));

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
                xnor_name.clone(),
                self.library.name.clone(),
                xnor_fit,
                format!("{}.xnor{}", path, bit),
                vec![a, b],
                vec![xnor_out],
            );
            cell.source_op = Some("XNOR2".to_string());
            self.add_cell(cell);
            xnor_outs.push(xnor_out);
        }

        // AND tree to combine
        let eq_out = self.reduce_tree(&xnor_outs, &and_name, and_fit, path, "eq_and");

        // Optionally invert for NotEqual
        if invert {
            let inv_cell = self.library.find_best_cell(&CellFunction::Inv);
            let (inv_name, inv_fit) = inv_cell
                .map(|c| (c.name.clone(), c.fit))
                .unwrap_or_else(|| ("INV".to_string(), 0.05));

            let y = outputs.first().copied().unwrap_or(GateNetId(0));
            let mut cell = Cell::new_comb(
                CellId(0),
                inv_name,
                self.library.name.clone(),
                inv_fit,
                format!("{}.ne_inv", path),
                vec![eq_out],
                vec![y],
            );
            cell.source_op = Some("Inverter".to_string());
            self.add_cell(cell);
        } else {
            // Connect eq_out to output (add buffer if needed)
            let y = outputs.first().copied().unwrap_or(GateNetId(0));
            if eq_out != y {
                let buf_cell = self.library.find_best_cell(&CellFunction::Buf);
                let (buf_name, buf_fit) = buf_cell
                    .map(|c| (c.name.clone(), c.fit))
                    .unwrap_or_else(|| ("BUF".to_string(), 0.05));

                let mut cell = Cell::new_comb(
                    CellId(0),
                    buf_name,
                    self.library.name.clone(),
                    buf_fit,
                    format!("{}.eq_buf", path),
                    vec![eq_out],
                    vec![y],
                );
                cell.source_op = Some("Buffer".to_string());
                self.add_cell(cell);
            }
        }

        self.stats.decomposed_mappings += 1;
    }

    /// Map less-than comparator
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

        // Use subtractor and check borrow
        // For now, simplified: just emit a warning and create placeholder
        self.warnings
            .push("LessThan comparator implementation is simplified".to_string());

        // Create a placeholder cell
        let y = outputs.first().copied().unwrap_or(GateNetId(0));
        let all_inputs: Vec<GateNetId> = inputs.iter().flatten().copied().collect();

        let mut cell = Cell::new_comb(
            CellId(0),
            "LT_COMPARATOR".to_string(),
            self.library.name.clone(),
            0.5 * width as f64, // Estimate FIT
            path.to_string(),
            all_inputs,
            vec![y],
        );
        cell.source_op = Some("LessThan".to_string());
        self.add_cell(cell);

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

        let dff_cell = self.library.find_best_cell(&dff_func);
        let (dff_name, dff_fit) = dff_cell
            .map(|c| (c.name.clone(), c.fit))
            .unwrap_or_else(|| (format!("{:?}", dff_func), 0.2));

        let clk = clock.unwrap_or(GateNetId(0));

        for bit in 0..width as usize {
            let d = inputs[0].get(bit).copied().unwrap_or(inputs[0][0]);
            let q = outputs.get(bit).copied().unwrap_or(outputs[0]);

            let mut dff_inputs = vec![d];
            if let Some(rst) = reset {
                dff_inputs.push(rst);
            }

            let mut cell = Cell::new_seq(
                CellId(0),
                dff_name.clone(),
                self.library.name.clone(),
                dff_fit,
                format!("{}.bit{}", path, bit),
                dff_inputs,
                vec![q],
                clk,
                reset,
            );
            cell.source_op = Some("Register".to_string());
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

        let cell = self.library.find_best_cell(&function);
        let (cell_name, fit) = cell
            .map(|c| (c.name.clone(), c.fit))
            .unwrap_or_else(|| (format!("{:?}", function), 0.1));

        // Get all input bits
        let input_bits: Vec<GateNetId> = inputs[0].iter().take(width as usize).copied().collect();

        // Build reduction tree
        let result = self.reduce_tree(&input_bits, &cell_name, fit, path, "reduce");

        // Connect to output
        let y = outputs.first().copied().unwrap_or(GateNetId(0));
        if result != y {
            let buf_cell = self.library.find_best_cell(&CellFunction::Buf);
            let (buf_name, buf_fit) = buf_cell
                .map(|c| (c.name.clone(), c.fit))
                .unwrap_or_else(|| ("BUF".to_string(), 0.05));

            let mut cell = Cell::new_comb(
                CellId(0),
                buf_name,
                self.library.name.clone(),
                buf_fit,
                format!("{}.reduce_buf", path),
                vec![result],
                vec![y],
            );
            cell.source_op = Some("Buffer".to_string());
            self.add_cell(cell);
        }

        self.stats.decomposed_mappings += 1;
    }

    /// Build a binary tree reduction
    fn reduce_tree(
        &mut self,
        inputs: &[GateNetId],
        cell_name: &str,
        fit: f64,
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
                        cell_name.to_string(),
                        self.library.name.clone(),
                        fit,
                        format!("{}.{}_l{}_{}", path, prefix, level, pair_idx),
                        vec![chunk[0], chunk[1]],
                        vec![out],
                    );
                    cell.source_op = Some(cell_name.to_string());
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

    /// Add a cell to the netlist
    fn add_cell(&mut self, cell: Cell) {
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

/// Map a WordLir to GateNetlist using the given library
pub fn map_word_lir_to_gates(word_lir: &WordLir, library: &TechLibrary) -> TechMapResult {
    let mut mapper = TechMapper::new(library);
    mapper.map(word_lir)
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tech_library::{LibraryCell, TechLibrary};
    use crate::word_lir::WordLir;

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
        let mut word_lir = WordLir::new("test".to_string());

        let a = word_lir.add_input("a".to_string(), 8);
        let b = word_lir.add_input("b".to_string(), 8);
        let y = word_lir.add_output("y".to_string(), 8);

        word_lir.add_node(
            WordOp::And { width: 8 },
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
        let mut word_lir = WordLir::new("test".to_string());

        let a = word_lir.add_input("a".to_string(), 4);
        let b = word_lir.add_input("b".to_string(), 4);
        let sum = word_lir.add_output("sum".to_string(), 4);

        word_lir.add_node(
            WordOp::Add {
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
        let mut word_lir = WordLir::new("test".to_string());

        let sel = word_lir.add_input("sel".to_string(), 1);
        let d0 = word_lir.add_input("d0".to_string(), 16);
        let d1 = word_lir.add_input("d1".to_string(), 16);
        let y = word_lir.add_output("y".to_string(), 16);

        word_lir.add_node(
            WordOp::Mux2 { width: 16 },
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
        let mut word_lir = WordLir::new("test".to_string());

        let clk = word_lir.add_input("clk".to_string(), 1);
        word_lir.clocks.push(clk);
        let d = word_lir.add_input("d".to_string(), 8);
        let q = word_lir.add_output("q".to_string(), 8);

        word_lir.add_seq_node(
            WordOp::Reg {
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
        let mut word_lir = WordLir::new("test".to_string());

        let a = word_lir.add_input("a".to_string(), 4);
        let y = word_lir.add_output("y".to_string(), 4);

        word_lir.add_node(WordOp::Not { width: 4 }, vec![a], y, "test.not".to_string());

        let result = map_word_lir_to_gates(&word_lir, &lib);

        // 4 inverters at 0.05 FIT each = 0.2 total
        assert!((result.netlist.total_fit() - 0.2).abs() < 0.001);
    }
}
