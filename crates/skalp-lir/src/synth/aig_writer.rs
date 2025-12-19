//! AIG Writer - Converts AIG back to GateNetlist
//!
//! This module handles the conversion from AIG (And-Inverter Graph) back to
//! a gate-level netlist using library cells.

use crate::gate_netlist::{
    Cell, CellId, CellSafetyClassification, GateNet, GateNetId, GateNetlist,
};
use crate::tech_library::{CellFunction, TechLibrary};

use super::aig::{Aig, AigLit, AigNode, AigNodeId};

use std::collections::HashMap;

/// Writer for converting AIG to GateNetlist
pub struct AigWriter<'a> {
    /// Target technology library
    library: &'a TechLibrary,
}

impl<'a> AigWriter<'a> {
    /// Create a new writer
    pub fn new(library: &'a TechLibrary) -> Self {
        Self { library }
    }

    /// Write AIG to gate netlist
    pub fn write(&self, aig: &Aig) -> GateNetlist {
        let mut state = AigWriterState {
            library: self.library,
            netlist: GateNetlist::new(aig.name.clone(), self.library.name.clone()),
            node_to_net: HashMap::new(),
            lit_to_net: HashMap::new(),
            next_cell_id: 0,
        };

        // Phase 1: Create nets for inputs
        state.create_input_nets(aig);

        // Phase 2: Create constant nets
        state.create_const_nets(aig);

        // Phase 3: Process nodes in topological order
        state.process_nodes(aig);

        // Phase 4: Create outputs
        state.create_outputs(aig);

        state.netlist.update_stats();
        state.netlist
    }
}

/// Internal state for AIG writing
struct AigWriterState<'a> {
    /// Target technology library
    library: &'a TechLibrary,

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

    /// Process all nodes
    fn process_nodes(&mut self, aig: &Aig) {
        // Get nodes in topological order
        let order = self.topological_order(aig);

        for id in order {
            let node = aig.get_node(id).unwrap();
            match node {
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
        // Get input net
        let data_net = self.get_or_create_lit_net(aig, data);

        // Create output net
        let output_net = self
            .netlist
            .add_net(GateNet::new(GateNetId(0), format!("q{}", id.0)));

        // Get clock and reset nets
        let clock_net = clock.and_then(|c| self.node_to_net.get(&c).copied());
        let reset_net = reset.and_then(|r| self.node_to_net.get(&r).copied());

        // Find appropriate cell from library
        let (cell_type, cell_fit) = if reset_net.is_some() {
            self.find_dffr_cell()
        } else {
            self.find_dff_cell()
        };

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

        // Store mapping
        self.node_to_net.insert(id, output_net);
        self.lit_to_net.insert((id, false), output_net);
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
