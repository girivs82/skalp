//! AIG Writer - Converts AIG back to GateNetlist
//!
//! This module handles the conversion from AIG (And-Inverter Graph) back to
//! a gate-level netlist using library cells.

use crate::gate_netlist::{
    Cell, CellId, CellSafetyClassification, GateNet, GateNetId, GateNetlist,
};
use crate::tech_library::{CellFunction, TechLibrary};

use super::aig::{Aig, AigLit, AigNode, AigNodeId, BarrierType};
use super::mapping::{MappedNode, MappingResult};

use std::collections::HashMap;

/// Writer for converting AIG to GateNetlist
pub struct AigWriter<'a> {
    /// Target technology library
    library: &'a TechLibrary,
    /// Optional mapping result for technology-mapped output
    mapping_result: Option<&'a MappingResult>,
}

impl<'a> AigWriter<'a> {
    /// Create a new writer
    pub fn new(library: &'a TechLibrary) -> Self {
        Self {
            library,
            mapping_result: None,
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
        }
    }

    /// Write AIG to gate netlist
    pub fn write(&self, aig: &Aig) -> GateNetlist {
        let mut state = AigWriterState {
            library: self.library,
            mapping_result: self.mapping_result,
            netlist: GateNetlist::new(aig.name.clone(), self.library.name.clone()),
            node_to_net: HashMap::new(),
            lit_to_net: HashMap::new(),
            next_cell_id: 0,
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

        // Phase 6: Remove dead cells (cells whose outputs have no fanout)
        let removed = state.netlist.remove_dead_cells();
        if removed > 0 {
            eprintln!("[AIG_WRITER] Removed {} dead cells", removed);
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
    node_to_net: HashMap<AigNodeId, GateNetId>,

    /// Mapping from (node, inverted) to net ID
    lit_to_net: HashMap<(AigNodeId, bool), GateNetId>,

    /// Next cell ID
    next_cell_id: u32,
}

impl AigWriterState<'_> {
    /// Create nets for primary inputs
    fn create_input_nets(&mut self, aig: &Aig) {
        for (id, node) in aig.iter_nodes() {
            if let AigNode::Input { name, source_net } = node {
                // Check if this is a special input (clock, reset)
                let is_clock = name.contains("clk") || name.contains("clock");
                let is_reset = name.contains("rst") || name.contains("reset");

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

    /// Create nets for constants
    fn create_const_nets(&mut self, _aig: &Aig) {
        // Create constant 0 net
        let const0 = self
            .netlist
            .add_net(GateNet::new(GateNetId(0), "const_0".to_string()));
        self.node_to_net.insert(AigNodeId::FALSE, const0);
        self.lit_to_net.insert((AigNodeId::FALSE, false), const0);

        // We'll create constant 1 (inverted const 0) lazily when needed
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

        eprintln!(
            "[AIG_WRITER] Processing {} nodes in topological order",
            order.len()
        );
        let mut and_count = 0;
        let mut latch_count = 0;
        for (id, node) in aig.iter_nodes() {
            match node {
                AigNode::And { .. } => and_count += 1,
                AigNode::Latch { .. } => latch_count += 1,
                _ => {}
            }
        }
        eprintln!(
            "[AIG_WRITER] AIG has {} AND nodes, {} latches",
            and_count, latch_count
        );

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
    fn topological_order(&self, aig: &Aig) -> Vec<AigNodeId> {
        let mut result = Vec::new();
        let mut visited = vec![false; aig.node_count()];

        fn visit(aig: &Aig, id: AigNodeId, visited: &mut [bool], result: &mut Vec<AigNodeId>) {
            if visited[id.0 as usize] {
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

        for (id, _) in aig.iter_nodes() {
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
        }

        // Fall back to basic AND2 mapping
        // Get input nets
        let left_net = self.get_or_create_lit_net(aig, left);
        let right_net = self.get_or_create_lit_net(aig, right);

        // Create output net
        let output_net = self
            .netlist
            .add_net(GateNet::new(GateNetId(0), format!("n{}", id.0)));

        // Find appropriate cell from library
        let (cell_type, cell_fit) = self.find_and2_cell();

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

        // Get input nets from the mapped cell's inputs
        // Each input is (AigNodeId, inverted) - we need to get or create the net
        let input_nets: Vec<GateNetId> = mapped
            .inputs
            .iter()
            .map(|(node, inverted)| {
                let lit = AigLit {
                    node: *node,
                    inverted: *inverted,
                };
                self.get_or_create_lit_net(aig, lit)
            })
            .collect();

        // Get safety info
        let safety = aig
            .get_safety_info(id)
            .map(|s| {
                s.classification
                    .clone()
                    .unwrap_or(CellSafetyClassification::Functional)
            })
            .unwrap_or(CellSafetyClassification::Functional);

        // Create cell with the mapped cell type
        let cell = Cell::new_comb(
            CellId(self.next_cell_id),
            mapped.cell_type.clone(),
            self.library.name.clone(),
            mapped.area, // Use mapped area (which includes FIT estimate)
            format!("aig.n{}", id.0),
            input_nets,
            vec![output_net],
        )
        .with_safety_classification(safety);

        self.next_cell_id += 1;
        self.netlist.add_cell(cell);

        // Store mapping based on output polarity
        // If output_inverted is true, the cell computes the inverse of the AIG function,
        // so the cell's output represents the inverted literal
        if mapped.output_inverted {
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

        // Try to detect enable pattern: D = (enable & new_value) | (~enable & Q)
        // If found, use SDFFE instead of DFFR to save combinational logic
        if let Some((enable_lit, new_data_lit)) = self.detect_enable_pattern(aig, data, id) {
            let enable_net = self.get_or_create_lit_net(aig, enable_lit);
            let new_data_net = self.get_or_create_lit_net(aig, new_data_lit);

            // Find SDFFE cell (with enable and reset)
            let (cell_type, cell_fit) = self.find_sdffe_cell();

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
                // Constant 1 - create if not exists
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

        // Need to create an inverter
        let base_net = self.node_to_net.get(&lit.node).copied().unwrap_or_else(|| {
            // Create a placeholder net
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

    /// Find an AND2 cell in the library
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

        // Default
        ("AND2_X1".to_string(), 0.1)
    }

    /// Find an inverter cell in the library
    fn find_inv_cell(&self) -> (String, f64) {
        let inv_cells = self.library.find_cells_by_function(&CellFunction::Inv);
        if let Some(cell) = inv_cells.first() {
            return (cell.name.clone(), cell.fit);
        }
        ("INV_X1".to_string(), 0.05)
    }

    /// Find a DFF cell in the library
    fn find_dff_cell(&self) -> (String, f64) {
        let dff_cells = self.library.find_cells_by_function(&CellFunction::Dff);
        if let Some(cell) = dff_cells.first() {
            return (cell.name.clone(), cell.fit);
        }
        ("DFF_X1".to_string(), 0.2)
    }

    /// Find a DFFR cell in the library
    fn find_dffr_cell(&self) -> (String, f64) {
        let dffr_cells = self.library.find_cells_by_function(&CellFunction::DffR);
        if let Some(cell) = dffr_cells.first() {
            return (cell.name.clone(), cell.fit);
        }
        ("DFFR_X1".to_string(), 0.25)
    }

    /// Find a SDFFE cell (DFF with synchronous enable and reset) in the library
    fn find_sdffe_cell(&self) -> (String, f64) {
        // Try to find SDFFE in library (DFF with synchronous enable and reset)
        let sdffe_cells = self.library.find_cells_by_function(&CellFunction::DffE);
        if let Some(cell) = sdffe_cells.first() {
            return (cell.name.clone(), cell.fit);
        }
        // Fallback: use SDFFE_PP0P name format (positive edge, positive enable, reset to 0)
        ("SDFFE_PP0P_X1".to_string(), 0.28)
    }

    /// Detect MUX-style enable pattern in latch data input
    ///
    /// Pattern: D = (enable & new_value) | (~enable & Q)
    /// In AIG: D = NOT(AND(NOT(AND(enable, new_value)), NOT(AND(NOT(enable), Q))))
    ///
    /// Returns Some((enable_lit, data_lit)) if pattern is detected
    fn detect_enable_pattern(
        &self,
        aig: &Aig,
        data: AigLit,
        latch_id: AigNodeId,
    ) -> Option<(AigLit, AigLit)> {
        // Pattern: D = NOT(AND(X, Y)) where D.inverted = true
        if !data.inverted {
            return None;
        }

        // Get the AND node for the inverted MUX output
        let mux_and = match aig.get_node(data.node) {
            Some(AigNode::And { left, right }) => (*left, *right),
            _ => return None,
        };

        // Both inputs should be inverted (they are the NORed halves of the MUX)
        // X = NOT(AND(enable, new_value))
        // Y = NOT(AND(NOT(enable), Q))
        if !mux_and.0.inverted || !mux_and.1.inverted {
            return None;
        }

        // Try both orderings
        for (x, y) in [(mux_and.0, mux_and.1), (mux_and.1, mux_and.0)] {
            if let Some(result) = self.try_extract_enable_pattern(aig, x, y, latch_id) {
                return Some(result);
            }
        }

        None
    }

    /// Try to extract enable pattern from two AND nodes
    /// x = NOT(AND(enable, new_value))
    /// y = NOT(AND(NOT(enable), Q))
    fn try_extract_enable_pattern(
        &self,
        aig: &Aig,
        x: AigLit,
        y: AigLit,
        latch_id: AigNodeId,
    ) -> Option<(AigLit, AigLit)> {
        // x.node should be AND(enable, new_value)
        let (x_left, x_right) = match aig.get_node(x.node) {
            Some(AigNode::And { left, right }) => (*left, *right),
            _ => return None,
        };

        // y.node should be AND(NOT(enable), Q)
        let (y_left, y_right) = match aig.get_node(y.node) {
            Some(AigNode::And { left, right }) => (*left, *right),
            _ => return None,
        };

        // One of y's inputs should be the latch output (Q)
        // The other should be the inverted enable
        let (enable_inv, q_input) = if y_left.node == latch_id {
            (y_right, y_left)
        } else if y_right.node == latch_id {
            (y_left, y_right)
        } else {
            return None;
        };

        // Q should not be inverted in the feedback path
        if q_input.inverted {
            return None;
        }

        // enable_inv should be inverted (it's ~enable in the pattern)
        if !enable_inv.inverted {
            return None;
        }

        let enable_node = enable_inv.node;

        // Check if one of x's inputs matches the enable
        let (potential_enable, new_value) = if x_left.node == enable_node && !x_left.inverted {
            (x_left, x_right)
        } else if x_right.node == enable_node && !x_right.inverted {
            (x_right, x_left)
        } else {
            return None;
        };

        // Found the pattern!
        Some((potential_enable, new_value))
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
                        // Create a constant 1 net if no enable
                        self.netlist
                            .add_net(GateNet::new(GateNetId(0), "const_1".to_string()))
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
    fn find_level_shifter_lh_cell(&self) -> (String, f64) {
        let cells = self
            .library
            .find_cells_by_function(&CellFunction::LevelShifterLH);
        if let Some(cell) = cells.first() {
            return (cell.name.clone(), cell.fit);
        }
        ("LVLSHIFT_LH_X1".to_string(), 0.15)
    }

    /// Find a level shifter HL cell in the library
    fn find_level_shifter_hl_cell(&self) -> (String, f64) {
        let cells = self
            .library
            .find_cells_by_function(&CellFunction::LevelShifterHL);
        if let Some(cell) = cells.first() {
            return (cell.name.clone(), cell.fit);
        }
        ("LVLSHIFT_HL_X1".to_string(), 0.15)
    }

    /// Find an always-on buffer cell in the library
    fn find_always_on_buf_cell(&self) -> (String, f64) {
        let cells = self
            .library
            .find_cells_by_function(&CellFunction::AlwaysOnBuf);
        if let Some(cell) = cells.first() {
            return (cell.name.clone(), cell.fit);
        }
        ("AON_BUF_X1".to_string(), 0.1)
    }

    /// Find an isolation AND cell in the library
    fn find_isolation_and_cell(&self) -> (String, f64) {
        let cells = self
            .library
            .find_cells_by_function(&CellFunction::IsolationAnd);
        if let Some(cell) = cells.first() {
            return (cell.name.clone(), cell.fit);
        }
        ("ISO_AND_X1".to_string(), 0.12)
    }

    /// Find an isolation OR cell in the library
    fn find_isolation_or_cell(&self) -> (String, f64) {
        let cells = self
            .library
            .find_cells_by_function(&CellFunction::IsolationOr);
        if let Some(cell) = cells.first() {
            return (cell.name.clone(), cell.fit);
        }
        ("ISO_OR_X1".to_string(), 0.12)
    }

    /// Find an isolation latch cell in the library
    fn find_isolation_latch_cell(&self) -> (String, f64) {
        let cells = self
            .library
            .find_cells_by_function(&CellFunction::IsolationLatch);
        if let Some(cell) = cells.first() {
            return (cell.name.clone(), cell.fit);
        }
        ("ISO_LATCH_X1".to_string(), 0.2)
    }

    /// Find a retention DFF cell in the library
    fn find_retention_dff_cell(&self) -> (String, f64) {
        let cells = self
            .library
            .find_cells_by_function(&CellFunction::RetentionDff);
        if let Some(cell) = cells.first() {
            return (cell.name.clone(), cell.fit);
        }
        ("RTNDFF_X1".to_string(), 0.25)
    }

    /// Find a retention DFFR cell in the library
    fn find_retention_dffr_cell(&self) -> (String, f64) {
        let cells = self
            .library
            .find_cells_by_function(&CellFunction::RetentionDffR);
        if let Some(cell) = cells.first() {
            return (cell.name.clone(), cell.fit);
        }
        ("RTNDFFR_X1".to_string(), 0.28)
    }

    /// Find a power switch header cell in the library
    fn find_power_switch_header_cell(&self) -> (String, f64) {
        let cells = self
            .library
            .find_cells_by_function(&CellFunction::PowerSwitchHeader);
        if let Some(cell) = cells.first() {
            return (cell.name.clone(), cell.fit);
        }
        ("PSW_HEADER_X1".to_string(), 0.1)
    }

    /// Find a power switch footer cell in the library
    fn find_power_switch_footer_cell(&self) -> (String, f64) {
        let cells = self
            .library
            .find_cells_by_function(&CellFunction::PowerSwitchFooter);
        if let Some(cell) = cells.first() {
            return (cell.name.clone(), cell.fit);
        }
        ("PSW_FOOTER_X1".to_string(), 0.1)
    }
}

/// Simplified writer that maps AND nodes directly to NAND2 gates
/// This is the basic mapping; the optimizer can improve it later.
pub fn write_aig_to_gates(aig: &Aig, library: &TechLibrary) -> GateNetlist {
    let writer = AigWriter::new(library);
    writer.write(aig)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::builtin_libraries::builtin_generic_asic;
    use crate::gate_netlist::{Cell, CellId, GateNet, GateNetId, GateNetlist};
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
        let library = builtin_generic_asic();
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
        let library = builtin_generic_asic();
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
