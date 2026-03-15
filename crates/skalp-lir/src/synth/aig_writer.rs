//! AIG Writer - Converts AIG back to GateNetlist
//!
//! This module handles the conversion from AIG (And-Inverter Graph) back to
//! a gate-level netlist using library cells.

use crate::gate_netlist::{
    Cell, CellId, CellSafetyClassification, GateNet, GateNetId, GateNetlist,
};
use crate::tech_library::{CellFunction, TechLibrary};

use super::aig::{Aig, AigLit, AigNode, AigNodeId, BarrierType};
use super::dff_decompose::LatchDecomp;
use super::mapping::{MappedNode, MappingResult};

use indexmap::IndexMap;
use std::collections::{HashMap, HashSet};

/// Writer for converting AIG to GateNetlist
pub struct AigWriter<'a> {
    /// Target technology library
    library: &'a TechLibrary,
    /// Optional mapping result for technology-mapped output
    mapping_result: Option<&'a MappingResult>,
    /// Cofactor-based latch decompositions (enable/reset extraction)
    latch_decomps: HashMap<AigNodeId, LatchDecomp>,
}

impl<'a> AigWriter<'a> {
    /// Create a new writer
    pub fn new(library: &'a TechLibrary) -> Self {
        Self {
            library,
            mapping_result: None,
            latch_decomps: HashMap::new(),
        }
    }

    /// Create a writer with technology mapping results
    ///
    /// When mapping results are provided, the writer will use the mapped
    /// cell types (NAND2, NOR2, XOR2, MUX2, etc.) instead of just AND2/INV.
    pub fn with_mapping(library: &'a TechLibrary, mapping: &'a MappingResult) -> Self {
        Self {
            library,
            mapping_result: Some(mapping),
            latch_decomps: HashMap::new(),
        }
    }

    /// Set latch decomposition results from cofactor analysis
    pub fn set_latch_decompositions(&mut self, decomps: HashMap<AigNodeId, LatchDecomp>) {
        self.latch_decomps = decomps;
    }

    /// Write AIG to gate netlist
    pub fn write(&self, aig: &Aig) -> GateNetlist {
        // Pre-compute AND nodes that need cells even when the technology mapper
        // considers them interior to a cut (unmapped). An unmapped AND node needs
        // a cell if it's used as:
        // - A cut leaf of any mapped node
        // - A latch data/enable/reset input
        // - A primary output
        let mut required_unmapped = HashSet::new();
        if let Some(mapping) = self.mapping_result {
            // Collect all cut leaves from mapped nodes
            for mapped in mapping.mapped_nodes.values() {
                for &(leaf_id, _inverted) in &mapped.inputs {
                    if matches!(aig.get_node(leaf_id), Some(AigNode::And { .. }))
                        && !mapping.mapped_nodes.contains_key(&leaf_id)
                    {
                        required_unmapped.insert(leaf_id);
                    }
                }
            }

            // Also mark AND nodes referenced by latch data inputs
            for (_id, node) in aig.iter_nodes() {
                if let AigNode::Latch { data, .. } = node {
                    if matches!(aig.get_node(data.node), Some(AigNode::And { .. }))
                        && !mapping.mapped_nodes.contains_key(&data.node)
                    {
                        required_unmapped.insert(data.node);
                    }
                }
            }

            // Mark AND nodes in latch decomposition fanin cones
            fn mark_and_fanin(aig: &Aig, node_id: AigNodeId, nodes: &mut HashSet<AigNodeId>) {
                if nodes.contains(&node_id) || node_id == AigNodeId::FALSE {
                    return;
                }
                if let Some(AigNode::And { left, right }) = aig.get_node(node_id) {
                    nodes.insert(node_id);
                    mark_and_fanin(aig, left.node, nodes);
                    mark_and_fanin(aig, right.node, nodes);
                }
            }
            for decomp in self.latch_decomps.values() {
                mark_and_fanin(aig, decomp.data.node, &mut required_unmapped);
                if let Some(e) = decomp.enable {
                    mark_and_fanin(aig, e.node, &mut required_unmapped);
                }
                if let Some(r) = decomp.sync_reset {
                    mark_and_fanin(aig, r.node, &mut required_unmapped);
                }
            }

            // Remove nodes that ARE mapped — those get proper cells from emit_mapped_cell
            for id in mapping.mapped_nodes.keys() {
                required_unmapped.remove(id);
            }
        }
        let latch_decomp_nodes = required_unmapped;

        let mut state = AigWriterState {
            library: self.library,
            mapping_result: self.mapping_result,
            netlist: GateNetlist::new(aig.name.clone(), self.library.name.clone()),
            node_to_net: IndexMap::new(),
            lit_to_net: IndexMap::new(),
            next_cell_id: 0,
            latch_decomps: &self.latch_decomps,
            latch_decomp_nodes,
        };

        // Phase 1: Create nets for inputs
        state.create_input_nets(aig);

        // Phase 2: Create constant nets
        state.create_const_nets(aig);

        // Phase 3: Pre-create latch output nets (needed for sequential circuits with feedback)
        state.pre_create_latch_nets(aig);

        // Phase 4: Process nodes in topological order
        state.process_nodes(aig);



        // Phase 5: Create outputs
        state.create_outputs(aig);

        // Debug: count cells by type before DCE
        // Phase 6: Remove dead cells (cells whose outputs have no fanout)
        // Iterate until fixed point — backward covering may leave chains of
        // unmapped intermediate cells that cascade when removed.
        loop {
            let removed = state.netlist.remove_dead_cells();
            if removed == 0 {
                break;
            }
        }

        // Phase 7: Compute lut_init for FPGA LUT cells that don't have it set.
        // The AIG writer creates cells via Cell::new_comb for inversions and
        // unmapped AND nodes — these don't get lut_init from the mapping pass.
        if self.library.is_fpga() {
            // Collect init values first to avoid borrow conflict
            let inits: Vec<(usize, u64)> = state
                .netlist
                .cells
                .iter()
                .enumerate()
                .filter(|(_, c)| c.lut_init.is_none() && c.cell_type.contains("LUT4"))
                .filter_map(|(i, c)| {
                    state
                        .netlist
                        .compute_lut4_init(&c.cell_type)
                        .map(|v| (i, v as u64))
                })
                .collect();
            for (i, init) in inits {
                state.netlist.cells[i].lut_init = Some(init);
            }
        }

        state.netlist.update_stats();
        state.netlist
    }
}

/// Internal state for AIG writing
struct AigWriterState<'a> {
    /// Target technology library
    library: &'a TechLibrary,

    /// Optional mapping result for technology-mapped output
    mapping_result: Option<&'a MappingResult>,

    /// The netlist being built
    netlist: GateNetlist,

    /// Mapping from AIG node to output net ID
    node_to_net: IndexMap<AigNodeId, GateNetId>,

    /// Mapping from (node, inverted) to net ID
    lit_to_net: IndexMap<(AigNodeId, bool), GateNetId>,

    /// Next cell ID
    next_cell_id: u32,

    /// Cofactor-based latch decompositions
    latch_decomps: &'a HashMap<AigNodeId, LatchDecomp>,

    /// AND nodes needed by latch decompositions (their transitive fan-in)
    /// When mapping is active, these nodes need AND2 cells even if not in mapped_nodes.
    latch_decomp_nodes: HashSet<AigNodeId>,
}

impl AigWriterState<'_> {
    /// Create nets for primary inputs
    fn create_input_nets(&mut self, aig: &Aig) {
        for (id, node) in aig.iter_nodes() {
            if let AigNode::Input { name, source_net } = node {
                // Use AIG-level clock/reset flags (populated from GateNet.is_clock/is_reset
                // during AIG building). Fall back to name matching with a warning.
                let is_clock = if aig.clock_inputs.contains(&id) {
                    true
                } else if name.contains("clk") || name.contains("clock") {
                    eprintln!(
                        "warning: clock input '{}' detected by name only (no is_clock flag)",
                        name
                    );
                    true
                } else {
                    false
                };

                let is_reset = if aig.reset_inputs.contains(&id) {
                    true
                } else if !is_clock && (name.contains("rst") || name.contains("reset")) {
                    eprintln!(
                        "warning: reset input '{}' detected by name only (no is_reset flag)",
                        name
                    );
                    true
                } else {
                    false
                };

                let net_id = if is_clock {
                    self.netlist.add_clock(name.clone())
                } else if is_reset {
                    self.netlist.add_reset(name.clone())
                } else {
                    self.netlist.add_input(name.clone())
                };

                self.node_to_net.insert(id, net_id);
                self.lit_to_net.insert((id, false), net_id);
            }
        }
    }

    /// Create nets for constants, using TIE cells from the library when available
    fn create_const_nets(&mut self, _aig: &Aig) {
        // Create constant 0 net
        let const0 = self
            .netlist
            .add_net(GateNet::new(GateNetId(0), "const_0".to_string()));
        self.node_to_net.insert(AigNodeId::FALSE, const0);
        self.lit_to_net.insert((AigNodeId::FALSE, false), const0);

        // If the library has a TIE_LOW cell, instantiate it to drive const_0
        if let Some((tie_low_name, tie_low_fit)) = self.try_find_tie_low_cell() {
            let cell = Cell::new_comb(
                CellId(self.next_cell_id),
                tie_low_name,
                self.library.name.clone(),
                tie_low_fit,
                "tie_low_inst".to_string(),
                vec![], // No inputs
                vec![const0],
            );
            self.next_cell_id += 1;
            self.netlist.add_cell(cell);
        }

        // Create constant 1 net and TIE_HIGH cell if available
        let const1 = self
            .netlist
            .add_net(GateNet::new(GateNetId(0), "const_1".to_string()));
        self.lit_to_net.insert((AigNodeId::FALSE, true), const1);

        if let Some((tie_high_name, tie_high_fit)) = self.try_find_tie_high_cell() {
            let cell = Cell::new_comb(
                CellId(self.next_cell_id),
                tie_high_name,
                self.library.name.clone(),
                tie_high_fit,
                "tie_high_inst".to_string(),
                vec![], // No inputs
                vec![const1],
            );
            self.next_cell_id += 1;
            self.netlist.add_cell(cell);
        }
    }

    /// Pre-create latch output nets before processing nodes
    ///
    /// This is needed for sequential circuits with feedback loops where
    /// AND nodes may reference latch outputs before the latches are processed.
    fn pre_create_latch_nets(&mut self, aig: &Aig) {
        for (id, node) in aig.iter_nodes() {
            if let AigNode::Latch { .. } = node {
                // Create output net for this latch
                let output_net = self
                    .netlist
                    .add_net(GateNet::new(GateNetId(0), format!("q{}", id.0)));
                self.node_to_net.insert(id, output_net);
                self.lit_to_net.insert((id, false), output_net);
            }
        }
    }

    /// Process all nodes
    fn process_nodes(&mut self, aig: &Aig) {
        // Get nodes in topological order
        let order = self.topological_order(aig);

        for id in order {
            let node = aig.get_node(id).unwrap().clone();
            match &node {
                AigNode::Const | AigNode::Input { .. } => {
                    // Already handled
                }
                AigNode::And { left, right } => {
                    self.process_and_node(aig, id, *left, *right);
                }
                AigNode::Latch {
                    data,
                    init,
                    clock,
                    reset,
                } => {
                    self.process_latch_node(aig, id, *data, *init, *clock, *reset);
                }
                AigNode::Barrier {
                    barrier_type,
                    data,
                    enable,
                    clock,
                    reset,
                    init,
                } => {
                    self.process_barrier_node(
                        aig,
                        id,
                        barrier_type.clone(),
                        *data,
                        *enable,
                        *clock,
                        *reset,
                        *init,
                    );
                }
            }
        }
    }

    /// Get topological order of nodes
    ///
    /// For latches with decompositions, also visits the decomposed data/enable
    /// nodes before the latch so they're processed first.
    fn topological_order(&self, aig: &Aig) -> Vec<AigNodeId> {
        let mut result = Vec::new();
        let mut visited = vec![false; aig.node_count()];

        fn visit(aig: &Aig, id: AigNodeId, visited: &mut [bool], result: &mut Vec<AigNodeId>) {
            if id.0 as usize >= visited.len() || visited[id.0 as usize] {
                return;
            }
            visited[id.0 as usize] = true;

            if let Some(node) = aig.get_node(id) {
                for fanin in node.fanins() {
                    visit(aig, fanin.node, visited, result);
                }
            }

            result.push(id);
        }

        // First pass: visit all nodes normally
        for (id, node) in aig.iter_nodes() {
            // For latches with decompositions, visit decomp nodes first
            if let AigNode::Latch { .. } = node {
                if let Some(decomp) = self.latch_decomps.get(&id) {
                    visit(aig, decomp.data.node, &mut visited, &mut result);
                    if let Some(e) = decomp.enable {
                        visit(aig, e.node, &mut visited, &mut result);
                    }
                    if let Some(r) = decomp.sync_reset {
                        visit(aig, r.node, &mut visited, &mut result);
                    }
                }
            }
            visit(aig, id, &mut visited, &mut result);
        }

        result
    }

    /// Process an AND node
    fn process_and_node(&mut self, aig: &Aig, id: AigNodeId, left: AigLit, right: AigLit) {
        // Check if we have a technology-mapped cell for this node
        if let Some(mapping) = self.mapping_result {
            if let Some(mapped) = mapping.mapped_nodes.get(&id) {
                self.emit_mapped_cell(aig, id, mapped);
                return;
            }
            // Unmapped AND node — might be interior to a cut, or might be needed
            // as a cut leaf by another mapped node. Create an AND2 cell for it;
            // DCE will remove truly unused cells afterward.
            // (Previously we skipped cell creation, but that left undriven nets
            // when this node was used as a cut leaf by another mapped cell.)
        }

        // Select the best cell based on input inversions and library availability.
        // Strategy: Try specialized cells first (ANDNOT, NOR2), fall back to AND2+INV.
        // This is library-agnostic - we query what cells are available.
        let (cell_type, cell_fit, input_a, input_b): (String, f64, AigLit, AigLit) =
            self.select_best_and_cell(left, right);

        // Get input nets (now using the adjusted inputs without redundant inversions)
        let left_net = self.get_or_create_lit_net(aig, input_a);
        let right_net = self.get_or_create_lit_net(aig, input_b);

        // Create output net
        let output_net = self
            .netlist
            .add_net(GateNet::new(GateNetId(0), format!("n{}", id.0)));

        // Get safety info
        let safety = aig
            .get_safety_info(id)
            .map(|s| {
                s.classification
                    .clone()
                    .unwrap_or(CellSafetyClassification::Functional)
            })
            .unwrap_or(CellSafetyClassification::Functional);

        // Create cell
        let cell = Cell::new_comb(
            CellId(self.next_cell_id),
            cell_type,
            self.library.name.clone(),
            cell_fit,
            format!("aig.n{}", id.0),
            vec![left_net, right_net],
            vec![output_net],
        )
        .with_safety_classification(safety);

        self.next_cell_id += 1;
        self.netlist.add_cell(cell);

        // Store mapping
        self.node_to_net.insert(id, output_net);
        self.lit_to_net.insert((id, false), output_net);
    }

    /// Emit a technology-mapped cell (NAND2, XOR2, MUX2, etc.)
    fn emit_mapped_cell(&mut self, aig: &Aig, id: AigNodeId, mapped: &MappedNode) {
        // Create output net
        let output_net = self
            .netlist
            .add_net(GateNet::new(GateNetId(0), format!("n{}", id.0)));

        // Get safety info
        let safety = aig
            .get_safety_info(id)
            .map(|s| {
                s.classification
                    .clone()
                    .unwrap_or(CellSafetyClassification::Functional)
            })
            .unwrap_or(CellSafetyClassification::Functional);

        // For FPGA LUT targets with truth tables, absorb input and output inversions
        // into the truth table instead of creating physical inverter cells.
        // A LUT4 can implement ANY 4-input boolean function, so inversions are free.
        let is_fpga_lut = mapped.truth_table.is_some() && self.library.is_fpga();

        // Get input nets — for FPGA LUTs, use non-inverted base nets
        let input_nets: Vec<GateNetId> = mapped
            .inputs
            .iter()
            .map(|(node, inverted)| {
                let lit = AigLit {
                    node: *node,
                    inverted: if is_fpga_lut { false } else { *inverted },
                };
                self.get_or_create_lit_net(aig, lit)
            })
            .collect();

        // Create cell — use LUT cell when truth table is available (FPGA mapping)
        let cell = if let Some(tt) = mapped.truth_table {
            if self.library.is_fpga() {
                let num_inputs = mapped.inputs.len();

                // Absorb input inversions into truth table
                let mut adjusted_tt = tt;
                for (i, &(_node, inverted)) in mapped.inputs.iter().enumerate() {
                    if inverted {
                        adjusted_tt = complement_tt_input(adjusted_tt, i, num_inputs);
                    }
                }

                // For FPGA LUTs, do NOT apply output inversion — the LUT can implement
                // any function directly via its INIT value. Applying output inversion
                // would flip the polarity (LUT computes !f) while lit_to_net stores
                // it as (id, false), causing every consumer to see wrong polarity
                // and create extra INV cells.
                // (Output inversion is only needed for ASIC cells with fixed functions.)

                let lut_init = expand_truth_table_to_lut4(adjusted_tt, num_inputs);

                // Pad inputs to 4 for LUT4 (unused inputs connected to first input)
                let mut padded_inputs = input_nets.clone();
                while padded_inputs.len() < 4 {
                    padded_inputs.push(padded_inputs[0]);
                }

                Cell::new_lut(
                    CellId(self.next_cell_id),
                    mapped.cell_type.clone(),
                    self.library.name.clone(),
                    mapped.area,
                    format!("aig.n{}", id.0),
                    padded_inputs,
                    vec![output_net],
                    lut_init,
                )
                .with_safety_classification(safety)
            } else {
                Cell::new_comb(
                    CellId(self.next_cell_id),
                    mapped.cell_type.clone(),
                    self.library.name.clone(),
                    mapped.area,
                    format!("aig.n{}", id.0),
                    input_nets,
                    vec![output_net],
                )
                .with_safety_classification(safety)
            }
        } else {
            Cell::new_comb(
                CellId(self.next_cell_id),
                mapped.cell_type.clone(),
                self.library.name.clone(),
                mapped.area,
                format!("aig.n{}", id.0),
                input_nets,
                vec![output_net],
            )
            .with_safety_classification(safety)
        };

        self.next_cell_id += 1;
        self.netlist.add_cell(cell);

        // Store mapping based on output polarity
        if is_fpga_lut {
            // For FPGA LUTs, output inversion was absorbed into the truth table
            self.node_to_net.insert(id, output_net);
            self.lit_to_net.insert((id, false), output_net);
        } else if mapped.output_inverted {
            // Cell output is inverted relative to the AIG node
            // (id, true) -> output_net means requesting inverted output gets the cell directly
            self.node_to_net.insert(id, output_net);
            self.lit_to_net.insert((id, true), output_net);
            // Note: (id, false) will create an inverter if needed
        } else {
            // Normal case: cell output matches AIG node
            self.node_to_net.insert(id, output_net);
            self.lit_to_net.insert((id, false), output_net);
        }
    }

    /// Process a latch node
    fn process_latch_node(
        &mut self,
        aig: &Aig,
        id: AigNodeId,
        data: AigLit,
        _init: Option<bool>,
        clock: Option<AigNodeId>,
        reset: Option<AigNodeId>,
    ) {
        // Get pre-created output net (created in pre_create_latch_nets phase)
        let output_net = self.node_to_net.get(&id).copied().unwrap_or_else(|| {
            // Fallback: create if not pre-created (shouldn't happen normally)
            self.netlist
                .add_net(GateNet::new(GateNetId(0), format!("q{}", id.0)))
        });

        // Get clock and reset nets
        let clock_net = clock.and_then(|c| self.node_to_net.get(&c).copied());
        let reset_net = reset.and_then(|r| self.node_to_net.get(&r).copied());

        // Get safety info
        let safety = aig
            .get_safety_info(id)
            .map(|s| {
                s.classification
                    .clone()
                    .unwrap_or(CellSafetyClassification::Functional)
            })
            .unwrap_or(CellSafetyClassification::Functional);

        // Use cofactor-based decomposition for enable detection (functional, depth-independent)
        // When technology mapping is active, only use the decomposition if all referenced
        // nodes have cells or nets (the decomposition creates AND nodes that the mapper's
        // backward covering may not have reached).
        let enable_pattern = self
            .latch_decomps
            .get(&id)
            .and_then(|decomp| {
                let enable = decomp.enable?;
                // Check that the decomposed nodes have nets (mapped or pre-created)
                let data_ok = decomp.data.node == AigNodeId::FALSE
                    || self.node_to_net.contains_key(&decomp.data.node);
                let enable_ok = enable.node == AigNodeId::FALSE
                    || self.node_to_net.contains_key(&enable.node);
                if data_ok && enable_ok {
                    Some((enable, decomp.data))
                } else {
                    None // Decomposed nodes not available, fall back to raw data
                }
            });

        if let Some((enable_lit, new_data_lit)) = enable_pattern {
            let enable_net = self.get_or_create_lit_net(aig, enable_lit);
            let new_data_net = self.get_or_create_lit_net(aig, new_data_lit);

            // Use DffRE (enable + reset) when reset is present, DffE (enable only) otherwise
            let (cell_type, cell_fit) = if reset_net.is_some() {
                self.find_dffre_cell_or_fallback()
            } else {
                self.find_sdffe_cell()
            };

            // Create SDFFE cell with enable input
            // Inputs: [D, E] where D is the new data and E is the enable
            let cell = Cell::new_seq_with_enable(
                CellId(self.next_cell_id),
                cell_type,
                self.library.name.clone(),
                cell_fit,
                format!("aig.latch{}", id.0),
                new_data_net,
                enable_net,
                output_net,
                clock_net.unwrap_or(GateNetId(0)),
                reset_net,
            )
            .with_safety_classification(safety);

            self.next_cell_id += 1;
            self.netlist.add_cell(cell);
        } else {
            // No enable pattern found, use regular DFFR
            let data_net = self.get_or_create_lit_net(aig, data);

            // Find appropriate cell from library
            let (cell_type, cell_fit) = if reset_net.is_some() {
                self.find_dffr_cell()
            } else {
                self.find_dff_cell()
            };

            // Create cell
            let cell = Cell::new_seq(
                CellId(self.next_cell_id),
                cell_type,
                self.library.name.clone(),
                cell_fit,
                format!("aig.latch{}", id.0),
                vec![data_net],
                vec![output_net],
                clock_net.unwrap_or(GateNetId(0)),
                reset_net,
            )
            .with_safety_classification(safety);

            self.next_cell_id += 1;
            self.netlist.add_cell(cell);
        }

        // Note: node_to_net and lit_to_net are already set in pre_create_latch_nets
    }

    /// Get or create a net for a literal (handling inversion)
    fn get_or_create_lit_net(&mut self, aig: &Aig, lit: AigLit) -> GateNetId {
        // Check if we already have this exact literal
        if let Some(&net) = self.lit_to_net.get(&(lit.node, lit.inverted)) {
            return net;
        }

        // If not inverted, try to get the base net
        if !lit.inverted {
            if let Some(&net) = self.node_to_net.get(&lit.node) {
                self.lit_to_net.insert((lit.node, false), net);
                return net;
            }
        }

        // Handle constant
        if lit.node == AigNodeId::FALSE {
            if lit.inverted {
                // Constant 1 - use pre-created net from create_const_nets
                if let Some(&net) = self.lit_to_net.get(&(AigNodeId::FALSE, true)) {
                    return net;
                }
                // Fallback: create if not exists (shouldn't happen if create_const_nets was called)
                let const1 = self
                    .netlist
                    .add_net(GateNet::new(GateNetId(0), "const_1".to_string()));
                self.lit_to_net.insert((AigNodeId::FALSE, true), const1);
                return const1;
            } else {
                // Constant 0
                return *self.node_to_net.get(&AigNodeId::FALSE).unwrap();
            }
        }

        // Need to create an inverter (or the base net is missing)
        let base_net = self.node_to_net.get(&lit.node).copied().unwrap_or_else(|| {
            // Create a placeholder net — this node has no cell driving it
            let node_info = aig.get_node(lit.node).map(|n| match n {
                AigNode::Const => "Const",
                AigNode::Input { .. } => "Input",
                AigNode::And { .. } => "And",
                AigNode::Latch { .. } => "Latch",
                AigNode::Barrier { .. } => "Barrier",
            }).unwrap_or("MISSING");
            // Placeholder net for unmapped AIG node — happens when mapping
            // doesn't cover all transitive fanin nodes
            self.netlist
                .add_net(GateNet::new(GateNetId(0), format!("n{}", lit.node.0)))
        });

        if lit.inverted {
            // Create inverter
            let inv_net = self
                .netlist
                .add_net(GateNet::new(GateNetId(0), format!("n{}_inv", lit.node.0)));

            let (cell_type, cell_fit) = self.find_inv_cell();

            // Get safety from source node
            let safety = aig
                .get_safety_info(lit.node)
                .map(|s| {
                    s.classification
                        .clone()
                        .unwrap_or(CellSafetyClassification::Functional)
                })
                .unwrap_or(CellSafetyClassification::Functional);

            let cell = Cell::new_comb(
                CellId(self.next_cell_id),
                cell_type,
                self.library.name.clone(),
                cell_fit,
                format!("aig.inv{}", lit.node.0),
                vec![base_net],
                vec![inv_net],
            )
            .with_safety_classification(safety);

            self.next_cell_id += 1;
            self.netlist.add_cell(cell);

            self.lit_to_net.insert((lit.node, true), inv_net);
            inv_net
        } else {
            self.lit_to_net.insert((lit.node, false), base_net);
            base_net
        }
    }

    /// Create output nets
    fn create_outputs(&mut self, aig: &Aig) {
        for (name, lit) in aig.outputs() {
            // For constant outputs, create a dedicated net with a tie cell
            // This ensures each output has its own net even if multiple outputs
            // are tied to the same constant
            if lit.node == AigNodeId::FALSE {
                let is_one = lit.inverted;
                // Create a dedicated net for this output
                let output_net = self
                    .netlist
                    .add_net(GateNet::new(GateNetId(0), name.clone()));

                // Add tie cell to drive the net
                self.netlist.add_tie_cell(name, if is_one { 1 } else { 0 });

                // Mark as output
                if let Some(gate_net) = self.netlist.get_net_mut(output_net) {
                    gate_net.is_output = true;
                }
                self.netlist.outputs.push(output_net);
            } else {
                // Non-constant output - use the existing logic
                let net = self.get_or_create_lit_net(aig, *lit);

                // Mark this net as output
                if let Some(gate_net) = self.netlist.get_net_mut(net) {
                    gate_net.is_output = true;
                }

                // Add to outputs list if not already there
                if !self.netlist.outputs.contains(&net) {
                    self.netlist.outputs.push(net);
                }

                // Rename net to match output name
                if let Some(gate_net) = self.netlist.get_net_mut(net) {
                    gate_net.name = name.clone();
                }
            }
        }
    }

    /// Find an AND2 cell in the library
    ///
    /// Panics if the library doesn't contain an AND2 or NAND2 cell.
    fn find_and2_cell(&self) -> (String, f64) {
        // First try to find an AND2
        let and2_cells = self.library.find_cells_by_function(&CellFunction::And2);
        if let Some(cell) = and2_cells.first() {
            return (cell.name.clone(), cell.fit);
        }

        // Fall back to NAND2 + INV (will be handled by optimizer)
        let nand2_cells = self.library.find_cells_by_function(&CellFunction::Nand2);
        if let Some(cell) = nand2_cells.first() {
            // Return NAND2 - the output inversion is handled by AIG lit
            return (cell.name.clone(), cell.fit);
        }

        panic!(
            "Technology library '{}' does not contain AND2 or NAND2 cells. \
             Cannot synthesize combinational logic without these primitives.",
            self.library.name
        )
    }

    /// Try to find an ANDNOT cell in the library (computes A & ~B)
    /// Returns None if the library doesn't have this cell type
    fn try_find_andnot_cell(&self) -> Option<(String, f64)> {
        self.library
            .find_best_cell(&CellFunction::AndNot)
            .map(|cell| (cell.name.clone(), cell.fit))
    }

    /// Try to find a NOR2 cell in the library
    /// Returns None if the library doesn't have this cell type
    fn try_find_nor2_cell(&self) -> Option<(String, f64)> {
        self.library
            .find_best_cell(&CellFunction::Nor2)
            .map(|cell| (cell.name.clone(), cell.fit))
    }

    /// Try to find an ORNOT cell in the library (computes A | ~B)
    /// Returns None if the library doesn't have this cell type
    fn try_find_ornot_cell(&self) -> Option<(String, f64)> {
        self.library
            .find_best_cell(&CellFunction::OrNot)
            .map(|cell| (cell.name.clone(), cell.fit))
    }

    /// Try to find a NAND2 cell in the library
    /// Returns None if the library doesn't have this cell type
    fn try_find_nand2_cell(&self) -> Option<(String, f64)> {
        self.library
            .find_best_cell(&CellFunction::Nand2)
            .map(|cell| (cell.name.clone(), cell.fit))
    }

    /// Select the best cell for an AND operation based on input inversions
    /// and what cells are available in the technology library.
    ///
    /// This is the core library-agnostic optimization: we query what primitives
    /// are available and pick the best option for each pattern.
    fn select_best_and_cell(&self, left: AigLit, right: AigLit) -> (String, f64, AigLit, AigLit) {
        let (and2_name, and2_fit) = self.find_and2_cell();
        let (inv_name, inv_fit) = self.find_inv_cell();
        let _ = inv_name; // Will be used for cost calculation

        match (left.inverted, right.inverted) {
            (false, false) => {
                // AND(a, b) → AND2 (no alternatives needed)
                (and2_name, and2_fit, left, right)
            }

            (true, false) | (false, true) => {
                // AND(~a, b) or AND(a, ~b) → Try ANDNOT, else AND2 + INV
                let (inverted_input, non_inverted_input) = if left.inverted {
                    (left, right)
                } else {
                    (right, left)
                };

                // Option 1: Use ANDNOT if available
                // ANDNOT computes A & ~B, so A=non_inverted, B=inverted.node
                if let Some((andnot_name, andnot_fit)) = self.try_find_andnot_cell() {
                    let uninverted = AigLit {
                        node: inverted_input.node,
                        inverted: false,
                    };
                    // Cost comparison: ANDNOT vs AND2 + INV
                    if andnot_fit <= and2_fit + inv_fit {
                        return (andnot_name, andnot_fit, non_inverted_input, uninverted);
                    }
                }

                // Option 2: Use NAND + output_inversion pattern
                // ~(a & b) with inverted output = a & b
                // For AND(~x, y): we could use ~NAND(x, ~y) but that's more complex

                // Fallback: Use AND2 with inverted input (inverter will be created)
                (and2_name, and2_fit, left, right)
            }

            (true, true) => {
                // AND(~a, ~b) = ~(a | b) = NOR2(a, b) by DeMorgan
                let uninverted_left = AigLit {
                    node: left.node,
                    inverted: false,
                };
                let uninverted_right = AigLit {
                    node: right.node,
                    inverted: false,
                };

                // Option 1: Use NOR2 if available (best option, single cell)
                if let Some((nor2_name, nor2_fit)) = self.try_find_nor2_cell() {
                    // Cost: NOR2 vs AND2 + 2*INV
                    if nor2_fit <= and2_fit + 2.0 * inv_fit {
                        return (nor2_name, nor2_fit, uninverted_left, uninverted_right);
                    }
                }

                // Option 2: Use NAND with inverted output
                // AND(~a, ~b) = ~NAND(~a, ~b) - but NAND(~a,~b) = ~(~a & ~b) = a | b
                // So ~NAND(~a,~b) = ~(a | b) = NOR - same as NOR2
                // This doesn't help unless we can share the output inversion

                // Fallback: Use AND2 with both inputs inverted
                (and2_name, and2_fit, left, right)
            }
        }
    }

    /// Find an inverter cell in the library
    ///
    /// Panics if the library doesn't contain an INV cell.
    fn find_inv_cell(&self) -> (String, f64) {
        let inv_cells = self.library.find_cells_by_function(&CellFunction::Inv);
        if let Some(cell) = inv_cells.first() {
            return (cell.name.clone(), cell.fit);
        }
        panic!(
            "Technology library '{}' does not contain INV cell. \
             Cannot synthesize without an inverter primitive.",
            self.library.name
        )
    }

    /// Find a DFF cell in the library
    ///
    /// Panics if the library doesn't contain a DFF cell.
    fn find_dff_cell(&self) -> (String, f64) {
        let dff_cells = self.library.find_cells_by_function(&CellFunction::Dff);
        if let Some(cell) = dff_cells.first() {
            return (cell.name.clone(), cell.fit);
        }
        panic!(
            "Technology library '{}' does not contain DFF cell. \
             Cannot synthesize sequential logic without a flip-flop primitive.",
            self.library.name
        )
    }

    /// Find a DFFR cell in the library
    ///
    /// Panics if the library doesn't contain a DFFR cell.
    fn find_dffr_cell(&self) -> (String, f64) {
        let dffr_cells = self.library.find_cells_by_function(&CellFunction::DffR);
        if let Some(cell) = dffr_cells.first() {
            return (cell.name.clone(), cell.fit);
        }
        panic!(
            "Technology library '{}' does not contain DFFR (DFF with reset) cell. \
             Cannot synthesize latches with reset without this primitive.",
            self.library.name
        )
    }

    /// Find a SDFFE cell (DFF with synchronous enable) in the library
    ///
    /// Panics if the library doesn't contain a DFFE cell.
    fn find_sdffe_cell(&self) -> (String, f64) {
        // Try to find DFFE in library (DFF with synchronous enable)
        let sdffe_cells = self.library.find_cells_by_function(&CellFunction::DffE);
        if let Some(cell) = sdffe_cells.first() {
            return (cell.name.clone(), cell.fit);
        }
        panic!(
            "Technology library '{}' does not contain DFFE (DFF with enable) cell. \
             Cannot synthesize latches with enable pattern without this primitive.",
            self.library.name
        )
    }

    /// Find a DffRE cell (DFF with enable + reset) in the library.
    /// Falls back to DffE if DffRE is not available (reset handled externally).
    fn find_dffre_cell_or_fallback(&self) -> (String, f64) {
        // Try DffRE first (combined enable + reset)
        let dffre_cells = self.library.find_cells_by_function(&CellFunction::DffRE);
        if let Some(cell) = dffre_cells.first() {
            return (cell.name.clone(), cell.fit);
        }
        // Fall back to DffE (enable only, reset handled as external MUX)
        self.find_sdffe_cell()
    }

    /// Try to find a TIE_HIGH cell in the library (constant 1 driver)
    /// Returns None if the library doesn't have this cell type
    fn try_find_tie_high_cell(&self) -> Option<(String, f64)> {
        self.library
            .find_best_cell(&CellFunction::TieHigh)
            .map(|cell| (cell.name.clone(), cell.fit))
    }

    /// Try to find a TIE_LOW cell in the library (constant 0 driver)
    /// Returns None if the library doesn't have this cell type
    fn try_find_tie_low_cell(&self) -> Option<(String, f64)> {
        self.library
            .find_best_cell(&CellFunction::TieLow)
            .map(|cell| (cell.name.clone(), cell.fit))
    }

    /// Process a barrier node (power domain boundary)
    #[allow(clippy::too_many_arguments)]
    fn process_barrier_node(
        &mut self,
        aig: &Aig,
        id: AigNodeId,
        barrier_type: BarrierType,
        data: AigLit,
        enable: Option<AigLit>,
        clock: Option<AigNodeId>,
        reset: Option<AigNodeId>,
        _init: Option<bool>,
    ) {
        // Get input nets
        let data_net = self.get_or_create_lit_net(aig, data);

        // Create output net
        let output_net = self
            .netlist
            .add_net(GateNet::new(GateNetId(0), format!("barrier{}", id.0)));

        // Get safety info
        let safety = aig
            .get_safety_info(id)
            .map(|s| {
                s.classification
                    .clone()
                    .unwrap_or(CellSafetyClassification::Functional)
            })
            .unwrap_or(CellSafetyClassification::Functional);

        // Find appropriate cell and create it
        let (cell_type, cell_fit, inputs, is_seq) = match barrier_type {
            BarrierType::LevelShifterLH => {
                let (name, fit) = self.find_level_shifter_lh_cell();
                (name, fit, vec![data_net], false)
            }
            BarrierType::LevelShifterHL => {
                let (name, fit) = self.find_level_shifter_hl_cell();
                (name, fit, vec![data_net], false)
            }
            BarrierType::AlwaysOnBuf => {
                let (name, fit) = self.find_always_on_buf_cell();
                (name, fit, vec![data_net], false)
            }
            BarrierType::IsolationAnd => {
                let enable_net = enable
                    .map(|e| self.get_or_create_lit_net(aig, e))
                    .unwrap_or_else(|| {
                        // Use pre-created constant 1 net if no enable
                        *self
                            .lit_to_net
                            .get(&(AigNodeId::FALSE, true))
                            .expect("const_1 net should be pre-created")
                    });
                let (name, fit) = self.find_isolation_and_cell();
                (name, fit, vec![data_net, enable_net], false)
            }
            BarrierType::IsolationOr => {
                let enable_net = enable
                    .map(|e| self.get_or_create_lit_net(aig, e))
                    .unwrap_or_else(|| {
                        // Create a constant 0 net if no enable
                        *self.node_to_net.get(&AigNodeId::FALSE).unwrap()
                    });
                let (name, fit) = self.find_isolation_or_cell();
                (name, fit, vec![data_net, enable_net], false)
            }
            BarrierType::IsolationLatch => {
                let (name, fit) = self.find_isolation_latch_cell();
                (name, fit, vec![data_net], true)
            }
            BarrierType::RetentionDff => {
                let (name, fit) = self.find_retention_dff_cell();
                (name, fit, vec![data_net], true)
            }
            BarrierType::RetentionDffR => {
                let (name, fit) = self.find_retention_dffr_cell();
                (name, fit, vec![data_net], true)
            }
            BarrierType::PowerSwitchHeader => {
                let (name, fit) = self.find_power_switch_header_cell();
                (name, fit, vec![data_net], false)
            }
            BarrierType::PowerSwitchFooter => {
                let (name, fit) = self.find_power_switch_footer_cell();
                (name, fit, vec![data_net], false)
            }
            // I/O Pad barriers
            BarrierType::InputPad => {
                let (name, fit) = self.find_input_pad_cell();
                (name, fit, vec![data_net], false)
            }
            BarrierType::OutputPad => {
                let enable_net = enable
                    .map(|e| self.get_or_create_lit_net(aig, e))
                    .unwrap_or_else(|| {
                        // Use pre-created constant 1 (always enabled) if no OE
                        *self
                            .lit_to_net
                            .get(&(AigNodeId::FALSE, true))
                            .expect("const_1 net should be pre-created")
                    });
                let (name, fit) = self.find_output_pad_cell();
                (name, fit, vec![data_net, enable_net], false)
            }
            BarrierType::BidirPad => {
                let enable_net = enable
                    .map(|e| self.get_or_create_lit_net(aig, e))
                    .unwrap_or_else(|| {
                        // Use pre-created constant 1 (output enabled) if no OE
                        *self
                            .lit_to_net
                            .get(&(AigNodeId::FALSE, true))
                            .expect("const_1 net should be pre-created")
                    });
                let (name, fit) = self.find_bidir_pad_cell();
                (name, fit, vec![data_net, enable_net], false)
            }
            BarrierType::ClockPad => {
                let (name, fit) = self.find_clock_pad_cell();
                (name, fit, vec![data_net], false)
            }
            BarrierType::AnalogPad => {
                let (name, fit) = self.find_analog_pad_cell();
                (name, fit, vec![data_net], false)
            }
        };

        // Create the cell
        let cell = if is_seq {
            let clock_net = clock.and_then(|c| self.node_to_net.get(&c).copied());
            let reset_net = reset.and_then(|r| self.node_to_net.get(&r).copied());

            Cell::new_seq(
                CellId(self.next_cell_id),
                cell_type,
                self.library.name.clone(),
                cell_fit,
                format!("aig.barrier{}", id.0),
                inputs,
                vec![output_net],
                clock_net.unwrap_or(GateNetId(0)),
                reset_net,
            )
            .with_safety_classification(safety)
        } else {
            Cell::new_comb(
                CellId(self.next_cell_id),
                cell_type,
                self.library.name.clone(),
                cell_fit,
                format!("aig.barrier{}", id.0),
                inputs,
                vec![output_net],
            )
            .with_safety_classification(safety)
        };

        self.next_cell_id += 1;
        self.netlist.add_cell(cell);

        // Store mapping
        self.node_to_net.insert(id, output_net);
        self.lit_to_net.insert((id, false), output_net);
    }

    /// Find a level shifter LH cell in the library
    ///
    /// Panics if the library doesn't contain a level shifter LH cell.
    fn find_level_shifter_lh_cell(&self) -> (String, f64) {
        let cells = self
            .library
            .find_cells_by_function(&CellFunction::LevelShifterLH);
        if let Some(cell) = cells.first() {
            return (cell.name.clone(), cell.fit);
        }
        panic!(
            "Technology library '{}' does not contain LevelShifterLH cell. \
             Cannot synthesize level shifter barriers without this primitive.",
            self.library.name
        )
    }

    /// Find a level shifter HL cell in the library
    ///
    /// Panics if the library doesn't contain a level shifter HL cell.
    fn find_level_shifter_hl_cell(&self) -> (String, f64) {
        let cells = self
            .library
            .find_cells_by_function(&CellFunction::LevelShifterHL);
        if let Some(cell) = cells.first() {
            return (cell.name.clone(), cell.fit);
        }
        panic!(
            "Technology library '{}' does not contain LevelShifterHL cell. \
             Cannot synthesize level shifter barriers without this primitive.",
            self.library.name
        )
    }

    /// Find an always-on buffer cell in the library
    ///
    /// Panics if the library doesn't contain an always-on buffer cell.
    fn find_always_on_buf_cell(&self) -> (String, f64) {
        let cells = self
            .library
            .find_cells_by_function(&CellFunction::AlwaysOnBuf);
        if let Some(cell) = cells.first() {
            return (cell.name.clone(), cell.fit);
        }
        panic!(
            "Technology library '{}' does not contain AlwaysOnBuf cell. \
             Cannot synthesize always-on buffers without this primitive.",
            self.library.name
        )
    }

    /// Find an isolation AND cell in the library
    ///
    /// Panics if the library doesn't contain an isolation AND cell.
    fn find_isolation_and_cell(&self) -> (String, f64) {
        let cells = self
            .library
            .find_cells_by_function(&CellFunction::IsolationAnd);
        if let Some(cell) = cells.first() {
            return (cell.name.clone(), cell.fit);
        }
        panic!(
            "Technology library '{}' does not contain IsolationAnd cell. \
             Cannot synthesize isolation barriers without this primitive.",
            self.library.name
        )
    }

    /// Find an isolation OR cell in the library
    ///
    /// Panics if the library doesn't contain an isolation OR cell.
    fn find_isolation_or_cell(&self) -> (String, f64) {
        let cells = self
            .library
            .find_cells_by_function(&CellFunction::IsolationOr);
        if let Some(cell) = cells.first() {
            return (cell.name.clone(), cell.fit);
        }
        panic!(
            "Technology library '{}' does not contain IsolationOr cell. \
             Cannot synthesize isolation barriers without this primitive.",
            self.library.name
        )
    }

    /// Find an isolation latch cell in the library
    ///
    /// Panics if the library doesn't contain an isolation latch cell.
    fn find_isolation_latch_cell(&self) -> (String, f64) {
        let cells = self
            .library
            .find_cells_by_function(&CellFunction::IsolationLatch);
        if let Some(cell) = cells.first() {
            return (cell.name.clone(), cell.fit);
        }
        panic!(
            "Technology library '{}' does not contain IsolationLatch cell. \
             Cannot synthesize isolation latches without this primitive.",
            self.library.name
        )
    }

    /// Find a retention DFF cell in the library
    ///
    /// Panics if the library doesn't contain a retention DFF cell.
    fn find_retention_dff_cell(&self) -> (String, f64) {
        let cells = self
            .library
            .find_cells_by_function(&CellFunction::RetentionDff);
        if let Some(cell) = cells.first() {
            return (cell.name.clone(), cell.fit);
        }
        panic!(
            "Technology library '{}' does not contain RetentionDff cell. \
             Cannot synthesize retention registers without this primitive.",
            self.library.name
        )
    }

    /// Find a retention DFFR cell in the library
    ///
    /// Panics if the library doesn't contain a retention DFFR cell.
    fn find_retention_dffr_cell(&self) -> (String, f64) {
        let cells = self
            .library
            .find_cells_by_function(&CellFunction::RetentionDffR);
        if let Some(cell) = cells.first() {
            return (cell.name.clone(), cell.fit);
        }
        panic!(
            "Technology library '{}' does not contain RetentionDffR cell. \
             Cannot synthesize retention registers with reset without this primitive.",
            self.library.name
        )
    }

    /// Find a power switch header cell in the library
    ///
    /// Panics if the library doesn't contain a power switch header cell.
    fn find_power_switch_header_cell(&self) -> (String, f64) {
        let cells = self
            .library
            .find_cells_by_function(&CellFunction::PowerSwitchHeader);
        if let Some(cell) = cells.first() {
            return (cell.name.clone(), cell.fit);
        }
        panic!(
            "Technology library '{}' does not contain PowerSwitchHeader cell. \
             Cannot synthesize power switch barriers without this primitive.",
            self.library.name
        )
    }

    /// Find a power switch footer cell in the library
    ///
    /// Panics if the library doesn't contain a power switch footer cell.
    fn find_power_switch_footer_cell(&self) -> (String, f64) {
        let cells = self
            .library
            .find_cells_by_function(&CellFunction::PowerSwitchFooter);
        if let Some(cell) = cells.first() {
            return (cell.name.clone(), cell.fit);
        }
        panic!(
            "Technology library '{}' does not contain PowerSwitchFooter cell. \
             Cannot synthesize power switch barriers without this primitive.",
            self.library.name
        )
    }

    // ========================================================================
    // I/O Pad Cell Finders
    // ========================================================================

    /// Find an input pad cell in the library
    ///
    /// Panics if the library doesn't contain an input pad cell.
    fn find_input_pad_cell(&self) -> (String, f64) {
        let cells = self.library.find_cells_by_function(&CellFunction::InputPad);
        if let Some(cell) = cells.first() {
            return (cell.name.clone(), cell.fit);
        }
        panic!(
            "Technology library '{}' does not contain InputPad cell. \
             Cannot synthesize input pads without this primitive.",
            self.library.name
        )
    }

    /// Find an output pad cell in the library
    ///
    /// Panics if the library doesn't contain an output pad cell.
    fn find_output_pad_cell(&self) -> (String, f64) {
        let cells = self
            .library
            .find_cells_by_function(&CellFunction::OutputPad);
        if let Some(cell) = cells.first() {
            return (cell.name.clone(), cell.fit);
        }
        panic!(
            "Technology library '{}' does not contain OutputPad cell. \
             Cannot synthesize output pads without this primitive.",
            self.library.name
        )
    }

    /// Find a bidirectional pad cell in the library
    ///
    /// Panics if the library doesn't contain a bidirectional pad cell.
    fn find_bidir_pad_cell(&self) -> (String, f64) {
        let cells = self.library.find_cells_by_function(&CellFunction::BidirPad);
        if let Some(cell) = cells.first() {
            return (cell.name.clone(), cell.fit);
        }
        panic!(
            "Technology library '{}' does not contain BidirPad cell. \
             Cannot synthesize bidirectional pads without this primitive.",
            self.library.name
        )
    }

    /// Find a clock pad cell in the library
    ///
    /// Panics if the library doesn't contain a clock pad cell.
    fn find_clock_pad_cell(&self) -> (String, f64) {
        let cells = self.library.find_cells_by_function(&CellFunction::ClockPad);
        if let Some(cell) = cells.first() {
            return (cell.name.clone(), cell.fit);
        }
        panic!(
            "Technology library '{}' does not contain ClockPad cell. \
             Cannot synthesize clock pads without this primitive.",
            self.library.name
        )
    }

    /// Find an analog pad cell in the library
    ///
    /// Panics if the library doesn't contain an analog pad cell.
    fn find_analog_pad_cell(&self) -> (String, f64) {
        let cells = self
            .library
            .find_cells_by_function(&CellFunction::AnalogPad);
        if let Some(cell) = cells.first() {
            return (cell.name.clone(), cell.fit);
        }
        panic!(
            "Technology library '{}' does not contain AnalogPad cell. \
             Cannot synthesize analog pads without this primitive.",
            self.library.name
        )
    }
}

/// Simplified writer that maps AND nodes directly to NAND2 gates
/// This is the basic mapping; the optimizer can improve it later.
pub fn write_aig_to_gates(aig: &Aig, library: &TechLibrary) -> GateNetlist {
    let writer = AigWriter::new(library);
    writer.write(aig)
}

/// Expand a K-input truth table to a 16-bit LUT4 INIT value.
///
/// For K<4 inputs, the K-bit pattern is replicated to fill 16 bits.
/// This ensures that unused higher-order inputs don't affect the output.
/// Example: 2-input AND (tt=0x8, K=2) → 0x8888 (replicated 4×)
fn expand_truth_table_to_lut4(tt: u64, num_inputs: usize) -> u64 {
    let k_bits = 1usize << num_inputs; // number of entries in the K-input truth table
    let k_mask = (1u64 << k_bits) - 1;
    let base = tt & k_mask;

    match num_inputs {
        0 => {
            // Constant: replicate bit 0 across all 16 bits
            if base & 1 != 0 { 0xFFFF } else { 0x0000 }
        }
        1 => {
            // 2-entry → replicate 8 times to fill 16 bits
            let mut init = 0u64;
            for i in 0..8 {
                init |= base << (i * 2);
            }
            init & 0xFFFF
        }
        2 => {
            // 4-entry → replicate 4 times
            let mut init = 0u64;
            for i in 0..4 {
                init |= base << (i * 4);
            }
            init & 0xFFFF
        }
        3 => {
            // 8-entry → replicate 2 times
            (base | (base << 8)) & 0xFFFF
        }
        4 => {
            // Already 16 entries, just mask
            base & 0xFFFF
        }
        _ => {
            // For >4 inputs, truncate (shouldn't happen for LUT4)
            base & 0xFFFF
        }
    }
}

/// Complement a truth table variable (absorb input inversion).
///
/// If input `k` is inverted, the LUT sees `!xk` instead of `xk`.
/// We transform the truth table so that `tt'[...xk...] = tt[...!xk...]`,
/// i.e., swap all pairs of bit positions that differ only in bit `k`.
fn complement_tt_input(tt: u64, input: usize, num_inputs: usize) -> u64 {
    let num_entries = 1usize << num_inputs;
    let mut result = 0u64;
    for i in 0..num_entries {
        // For each truth table entry, read from the position with bit `input` flipped
        let j = i ^ (1 << input);
        if (tt >> j) & 1 == 1 {
            result |= 1u64 << i;
        }
    }
    result
}

/// Post-processing pass: absorb INV cells into downstream LUT truth tables.
///
/// On FPGA, every INV cell wastes a full LUT4. This pass finds INV cells
/// whose output feeds only into LUT4 cells, and absorbs the inversion by
/// complementing the appropriate truth table variable. The INV cell is then
/// removed and the LUT input is connected directly to the INV's source.
pub fn absorb_inverters_into_luts(netlist: &mut GateNetlist) -> usize {
    // Step 1: Build consumer map: output_net → [(cell_index, input_pin_index)]
    let mut consumers: HashMap<GateNetId, Vec<(usize, usize)>> = HashMap::new();
    for (cell_idx, cell) in netlist.cells.iter().enumerate() {
        for (pin_idx, &net_id) in cell.inputs.iter().enumerate() {
            consumers.entry(net_id).or_default().push((cell_idx, pin_idx));
        }
        // Also check clock nets — these should NOT absorb inversions
        if let Some(clk_net) = cell.clock {
            consumers
                .entry(clk_net)
                .or_default()
                .push((cell_idx, usize::MAX)); // sentinel: non-absorbable
        }
    }

    // Also track output port nets — can't remove INVs driving output ports
    let output_nets: HashSet<GateNetId> = netlist.outputs.iter().copied().collect();

    // Step 2: Find INV cells whose output feeds only LUT4 cells
    let mut inv_removals: Vec<(usize, GateNetId, GateNetId)> = Vec::new(); // (cell_idx, inv_input_net, inv_output_net)

    for (cell_idx, cell) in netlist.cells.iter().enumerate() {
        // Detect INV cells: function is Inv, or cell_type contains "INV" and has 1 input, 1 output
        let is_inv = cell
            .function
            .as_ref()
            .is_some_and(|f| matches!(f, CellFunction::Inv))
            || (cell.cell_type.contains("INV")
                && cell.inputs.len() == 1
                && cell.outputs.len() == 1);

        if !is_inv || cell.inputs.is_empty() || cell.outputs.is_empty() {
            continue;
        }

        let inv_input = cell.inputs[0];
        let inv_output = cell.outputs[0];

        // Skip if the INV output is a primary output
        if output_nets.contains(&inv_output) {
            continue;
        }

        // Check all consumers of the INV output
        let consumer_list = match consumers.get(&inv_output) {
            Some(list) if !list.is_empty() => list,
            _ => continue, // No consumers or dead — skip (DCE will handle dead)
        };

        // ALL consumers must be LUT cells (have lut_init)
        let all_consumers_are_luts = consumer_list.iter().all(|&(ci, pin)| {
            pin != usize::MAX && netlist.cells[ci].lut_init.is_some()
        });

        if all_consumers_are_luts {
            inv_removals.push((cell_idx, inv_input, inv_output));
        }
    }

    let removed = inv_removals.len();

    // Step 3: Apply absorption — complement LUT truth tables and rewire
    let inv_cell_indices: HashSet<usize> = inv_removals.iter().map(|&(idx, _, _)| idx).collect();

    for &(_cell_idx, inv_input, inv_output) in &inv_removals {
        if let Some(consumer_list) = consumers.get(&inv_output) {
            for &(ci, pin_idx) in consumer_list {
                if pin_idx == usize::MAX {
                    continue;
                }
                // Complement the truth table at this input position
                if let Some(init) = netlist.cells[ci].lut_init {
                    let new_init = complement_tt_input(init, pin_idx, 4);
                    netlist.cells[ci].lut_init = Some(new_init);
                }
                // Rewire: connect the LUT input directly to the INV's source
                netlist.cells[ci].inputs[pin_idx] = inv_input;
            }
        }
    }

    // Step 4: Remove INV cells (in reverse order to preserve indices)
    let mut indices: Vec<usize> = inv_cell_indices.into_iter().collect();
    indices.sort_unstable();
    for &idx in indices.iter().rev() {
        netlist.cells.remove(idx);
    }

    removed
}

/// Post-processing pass: push INV cells backward into producing LUT truth tables.
///
/// Complements `absorb_inverters_into_luts` which absorbs INV→LUT (forward).
/// This pass handles INV cells that feed non-LUT consumers (DFFs, outputs) by
/// flipping the producing LUT's INIT value instead, eliminating the INV cell.
///
/// Only safe when ALL consumers of the producing LUT's output go through INV cells
/// (i.e., nobody needs the non-inverted value directly).
pub fn push_inverters_into_producing_luts(netlist: &mut GateNetlist) -> usize {
    // Build maps: net → driver cell, net → consumer cells
    let mut net_driver: HashMap<GateNetId, usize> = HashMap::new();
    let mut net_consumers: HashMap<GateNetId, Vec<(usize, usize)>> = HashMap::new(); // net → [(cell_idx, pin_idx)]

    for (cell_idx, cell) in netlist.cells.iter().enumerate() {
        for &out_net in &cell.outputs {
            net_driver.insert(out_net, cell_idx);
        }
        for (pin_idx, &in_net) in cell.inputs.iter().enumerate() {
            net_consumers
                .entry(in_net)
                .or_default()
                .push((cell_idx, pin_idx));
        }
        if let Some(clk) = cell.clock {
            net_consumers
                .entry(clk)
                .or_default()
                .push((cell_idx, usize::MAX));
        }
    }

    // Track output port nets
    let output_nets: HashSet<GateNetId> = netlist.outputs.iter().copied().collect();

    // Find INV cells whose producing LUT can absorb the inversion
    let mut inv_to_remove: Vec<usize> = Vec::new();
    // Track which LUTs to flip: producer_idx → [(inv_idx, inv_output_net, non-INV LUT consumers)]
    let mut luts_to_flip: HashMap<usize, Vec<(usize, GateNetId, Vec<(usize, usize)>)>> =
        HashMap::new();

    for (cell_idx, cell) in netlist.cells.iter().enumerate() {
        let is_inv = cell
            .function
            .as_ref()
            .is_some_and(|f| matches!(f, CellFunction::Inv))
            || (cell.cell_type.contains("INV")
                && cell.inputs.len() == 1
                && cell.outputs.len() == 1);

        if !is_inv || cell.inputs.is_empty() || cell.outputs.is_empty() {
            continue;
        }

        let inv_input_net = cell.inputs[0];
        let inv_output_net = cell.outputs[0];

        // Find the producing cell
        let Some(&producer_idx) = net_driver.get(&inv_input_net) else {
            continue;
        };

        // Producer must be a LUT4
        if netlist.cells[producer_idx].lut_init.is_none() {
            continue;
        }

        // Skip if the producer's output net is a primary output
        if output_nets.contains(&inv_input_net) {
            continue;
        }

        // Classify consumers of the producer's output:
        // - INV cells: will be removed if we flip
        // - LUT cells: can absorb polarity change via truth table complement (free)
        // - Other cells (DFF, output): would need a new INV cell
        let Some(consumers) = net_consumers.get(&inv_input_net) else {
            continue;
        };

        let mut inv_consumers = Vec::new(); // (cell_idx, inv_output_net)
        let mut lut_consumers = Vec::new(); // (cell_idx, pin_idx) — non-INV LUT consumers
        let mut other_count = 0usize; // non-INV, non-LUT consumers

        for &(ci, pin) in consumers {
            let c = &netlist.cells[ci];
            let is_inv = c
                .function
                .as_ref()
                .is_some_and(|f| matches!(f, CellFunction::Inv))
                || (c.cell_type.contains("INV")
                    && c.inputs.len() == 1
                    && c.outputs.len() == 1);

            if is_inv {
                inv_consumers.push(ci);
            } else if pin != usize::MAX && c.lut_init.is_some() {
                lut_consumers.push((ci, pin));
            } else {
                other_count += 1;
            }
        }

        // Also check if the producer's output is a primary output
        if output_nets.contains(&inv_input_net) {
            other_count += 1;
        }

        // Flip if: removing INV cells saves more than adding INVs for 'other' consumers
        // When other_count == 0, all non-INV consumers are LUTs that absorb for free
        if !inv_consumers.is_empty() && inv_consumers.len() > other_count {
            luts_to_flip
                .entry(producer_idx)
                .or_default()
                .push((cell_idx, inv_output_net, lut_consumers.clone()));
        }
    }

    // Apply: flip LUT INITs, remove INV cells, complement LUT consumers' truth tables
    let mut total_removed = 0;
    for (producer_idx, flip_info) in &luts_to_flip {
        // Flip the LUT's INIT
        if let Some(init) = netlist.cells[*producer_idx].lut_init {
            netlist.cells[*producer_idx].lut_init = Some(!init & 0xFFFF);
        }

        let producer_output = netlist.cells[*producer_idx].outputs[0];

        // Complement non-INV LUT consumers' truth tables at the affected input
        // (they were consuming non-inverted; now the LUT produces inverted)
        let mut complemented_luts: HashSet<(usize, usize)> = HashSet::new();
        for &(_, _, ref lut_consumers) in flip_info {
            for &(ci, pin_idx) in lut_consumers {
                if complemented_luts.insert((ci, pin_idx)) {
                    if let Some(init) = netlist.cells[ci].lut_init {
                        netlist.cells[ci].lut_init =
                            Some(complement_tt_input(init, pin_idx, 4));
                    }
                }
            }
        }

        // Rewire: each INV cell's output consumers now connect to the (flipped) LUT output
        for &(inv_idx, inv_output_net, _) in flip_info {
            if let Some(consumers) = net_consumers.get(&inv_output_net) {
                for &(ci, pin_idx) in consumers {
                    if pin_idx == usize::MAX {
                        netlist.cells[ci].clock = Some(producer_output);
                    } else if pin_idx < netlist.cells[ci].inputs.len() {
                        netlist.cells[ci].inputs[pin_idx] = producer_output;
                    }
                }
            }

            // Also rewire output ports
            for out in &mut netlist.outputs {
                if *out == inv_output_net {
                    *out = producer_output;
                }
            }

            inv_to_remove.push(inv_idx);
            total_removed += 1;
        }
    }

    // Remove INV cells (reverse order to preserve indices)
    inv_to_remove.sort_unstable();
    inv_to_remove.dedup();
    for &idx in inv_to_remove.iter().rev() {
        netlist.cells.remove(idx);
    }

    total_removed
}

/// Post-mapping LUT merging pass: combine adjacent small LUTs into one.
///
/// When a producer LUT has a single fanout into a consumer LUT, and the
/// combined input count fits in a LUT4 (≤4 unique inputs), compose the truth
/// tables and merge into a single LUT cell. This eliminates under-utilized
/// LUTs (e.g., INV→AND2 → single LUT4, AND2→AND2 → single LUT4).
///
/// Returns the number of LUT cells removed.
pub fn merge_adjacent_luts(netlist: &mut GateNetlist) -> usize {
    let max_lut_inputs = 4usize;
    let mut total_merged = 0;

    // Iterate until no more merges are found (merging can create new opportunities)
    loop {
        // Build net→driver and net→consumer maps
        let mut net_driver: HashMap<GateNetId, usize> = HashMap::new();
        let mut net_consumers: HashMap<GateNetId, Vec<(usize, usize)>> = HashMap::new();

        for (cell_idx, cell) in netlist.cells.iter().enumerate() {
            for &out_net in &cell.outputs {
                net_driver.insert(out_net, cell_idx);
            }
            for (pin_idx, &in_net) in cell.inputs.iter().enumerate() {
                net_consumers
                    .entry(in_net)
                    .or_default()
                    .push((cell_idx, pin_idx));
            }
            if let Some(clk) = cell.clock {
                net_consumers
                    .entry(clk)
                    .or_default()
                    .push((cell_idx, usize::MAX));
            }
        }

        // Track output port nets — can't remove LUTs driving primary outputs
        let output_nets: HashSet<GateNetId> = netlist.outputs.iter().copied().collect();

        // Find merge candidates: producer LUT → consumer LUT (single fanout)
        let mut merges: Vec<(usize, usize, usize)> = Vec::new(); // (producer_idx, consumer_idx, pin_in_consumer)
        let mut consumed_producers: HashSet<usize> = HashSet::new();
        let mut consumed_consumers: HashSet<usize> = HashSet::new();

        for (prod_idx, cell) in netlist.cells.iter().enumerate() {
            // Must be a LUT cell
            if cell.lut_init.is_none() || cell.outputs.is_empty() {
                continue;
            }

            let prod_output = cell.outputs[0];

            // Skip if output is a primary output
            if output_nets.contains(&prod_output) {
                continue;
            }

            // Must have exactly one consumer (single fanout)
            let consumers = match net_consumers.get(&prod_output) {
                Some(list) => list,
                None => continue,
            };
            if consumers.len() != 1 {
                continue;
            }

            let (cons_idx, pin_idx) = consumers[0];
            if pin_idx == usize::MAX {
                continue; // Clock pin, not data
            }

            let consumer = &netlist.cells[cons_idx];
            if consumer.lut_init.is_none() {
                continue; // Consumer must also be a LUT
            }

            // Count unique combined inputs
            let prod_inputs = &cell.inputs;
            let cons_inputs = &consumer.inputs;

            // Collect unique nets: consumer inputs (except the merged pin) + producer inputs
            let mut combined: Vec<GateNetId> = Vec::new();
            for (i, &net) in cons_inputs.iter().enumerate() {
                if i == pin_idx {
                    continue; // Skip the internal wire
                }
                if !combined.contains(&net) {
                    combined.push(net);
                }
            }
            for &net in prod_inputs {
                if !combined.contains(&net) {
                    combined.push(net);
                }
            }

            if combined.len() <= max_lut_inputs
                && !consumed_producers.contains(&prod_idx)
                && !consumed_consumers.contains(&cons_idx)
                && prod_idx != cons_idx
            {
                merges.push((prod_idx, cons_idx, pin_idx));
                consumed_producers.insert(prod_idx);
                consumed_consumers.insert(cons_idx);
                // Also prevent consumer from being used as a producer in the same round
                consumed_producers.insert(cons_idx);
            }
        }

        if merges.is_empty() {
            break;
        }

        // Apply merges
        let mut cells_to_remove: HashSet<usize> = HashSet::new();

        for &(prod_idx, cons_idx, pin_idx) in &merges {
            let prod_init = netlist.cells[prod_idx].lut_init.unwrap();
            let cons_init = netlist.cells[cons_idx].lut_init.unwrap();
            let prod_inputs = netlist.cells[prod_idx].inputs.clone();
            let cons_inputs = netlist.cells[cons_idx].inputs.clone();

            // Build the combined input list and mapping
            let mut combined_inputs: Vec<GateNetId> = Vec::new();

            // Map: for each original consumer input, what index in combined_inputs?
            let mut cons_input_map: Vec<usize> = Vec::new();
            for (i, &net) in cons_inputs.iter().enumerate() {
                if i == pin_idx {
                    cons_input_map.push(usize::MAX); // placeholder for producer output
                    continue;
                }
                let pos = combined_inputs.iter().position(|&n| n == net).unwrap_or_else(|| {
                    combined_inputs.push(net);
                    combined_inputs.len() - 1
                });
                cons_input_map.push(pos);
            }

            // Map: for each producer input, what index in combined_inputs?
            let mut prod_input_map: Vec<usize> = Vec::new();
            for &net in &prod_inputs {
                let pos = combined_inputs.iter().position(|&n| n == net).unwrap_or_else(|| {
                    combined_inputs.push(net);
                    combined_inputs.len() - 1
                });
                prod_input_map.push(pos);
            }

            let num_combined = combined_inputs.len();
            if num_combined > max_lut_inputs {
                continue; // Safety check (shouldn't happen due to earlier check)
            }

            // Compute composed truth table
            let num_prod_inputs = prod_inputs.len();
            let num_cons_inputs = cons_inputs.len();
            let mut new_tt = 0u64;

            for row in 0..(1u64 << num_combined) {
                // Evaluate producer: extract producer input bits from the combined row
                let mut prod_row = 0usize;
                for (pi, &ci) in prod_input_map.iter().enumerate() {
                    if (row >> ci) & 1 == 1 {
                        prod_row |= 1 << pi;
                    }
                }
                let prod_out = (prod_init >> prod_row) & 1;

                // Evaluate consumer: extract consumer input bits, substituting producer output
                let mut cons_row = 0usize;
                for (ci_pin, &mapped) in cons_input_map.iter().enumerate() {
                    if ci_pin == pin_idx {
                        // This is the pin fed by the producer
                        if prod_out == 1 {
                            cons_row |= 1 << ci_pin;
                        }
                    } else {
                        if (row >> mapped) & 1 == 1 {
                            cons_row |= 1 << ci_pin;
                        }
                    }
                }
                let cons_out = (cons_init >> cons_row) & 1;

                if cons_out == 1 {
                    new_tt |= 1u64 << row;
                }
            }

            // Expand to LUT4 (pad if fewer than 4 inputs)
            let lut4_init = expand_truth_table_to_lut4(new_tt, num_combined);

            // Pad inputs to 4
            let mut padded = combined_inputs.clone();
            while padded.len() < 4 {
                if padded.is_empty() {
                    // Constant — use any net (won't matter)
                    if let Some(&net) = netlist.inputs.first() {
                        padded.push(net);
                    } else {
                        break;
                    }
                } else {
                    padded.push(padded[0]);
                }
            }

            // Update consumer cell in-place with merged LUT
            netlist.cells[cons_idx].inputs = padded;
            netlist.cells[cons_idx].lut_init = Some(lut4_init);

            // Mark producer for removal
            cells_to_remove.insert(prod_idx);
        }

        // Remove merged producer cells (reverse order to preserve indices)
        let mut indices: Vec<usize> = cells_to_remove.into_iter().collect();
        indices.sort_unstable();
        for &idx in indices.iter().rev() {
            netlist.cells.remove(idx);
        }

        total_merged += merges.len();
    }

    total_merged
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::gate_netlist::{Cell, CellId, GateNet, GateNetId, GateNetlist};
    use crate::get_stdlib_library;
    use crate::synth::AigBuilder;

    fn create_simple_netlist() -> GateNetlist {
        let mut netlist = GateNetlist::new("test".to_string(), "generic_asic".to_string());

        let a = netlist.add_input("a".to_string());
        let b = netlist.add_input("b".to_string());
        let and_out = netlist.add_net(GateNet::new(GateNetId(0), "and_out".to_string()));
        let y = netlist.add_output("y".to_string());

        let and_cell = Cell::new_comb(
            CellId(0),
            "AND2_X1".to_string(),
            "generic_asic".to_string(),
            0.1,
            "top.and".to_string(),
            vec![a, b],
            vec![and_out],
        );
        netlist.add_cell(and_cell);

        let buf_cell = Cell::new_comb(
            CellId(0),
            "BUF_X1".to_string(),
            "generic_asic".to_string(),
            0.05,
            "top.buf".to_string(),
            vec![and_out],
            vec![y],
        );
        netlist.add_cell(buf_cell);

        netlist
    }

    #[test]
    fn test_round_trip_simple() {
        let library = get_stdlib_library("generic_asic").expect("Failed to load library");
        let netlist = create_simple_netlist();

        // Build AIG
        let builder = AigBuilder::new(&netlist);
        let aig = builder.build();

        assert_eq!(aig.input_count(), 2);
        assert_eq!(aig.output_count(), 1);

        // Write back to netlist
        let result = write_aig_to_gates(&aig, &library);

        // Should have same I/O
        assert_eq!(result.inputs.len(), 2);
        assert_eq!(result.outputs.len(), 1);
    }

    #[test]
    fn test_inversion_handling() {
        let library = get_stdlib_library("generic_asic").expect("Failed to load library");
        let mut netlist = GateNetlist::new("test".to_string(), "generic_asic".to_string());

        let a = netlist.add_input("a".to_string());
        let y = netlist.add_output("y".to_string());

        // Create inverter
        let inv_cell = Cell::new_comb(
            CellId(0),
            "INV_X1".to_string(),
            "generic_asic".to_string(),
            0.05,
            "top.inv".to_string(),
            vec![a],
            vec![y],
        );
        netlist.add_cell(inv_cell);

        // Build AIG
        let builder = AigBuilder::new(&netlist);
        let aig = builder.build();

        // Output should be inverted input
        let (_, out_lit) = &aig.outputs()[0];
        assert!(out_lit.inverted);

        // Write back
        let result = write_aig_to_gates(&aig, &library);

        assert_eq!(result.inputs.len(), 1);
        assert_eq!(result.outputs.len(), 1);
    }
}
