//! AIG Builder - Converts GateNetlist to AIG
//!
//! This module handles the conversion from gate-level netlist (with library cells
//! like NAND2, XOR2, DFF, etc.) to the AIG (And-Inverter Graph) representation.

use crate::gate_netlist::{Cell, CellId, GateNet, GateNetId, GateNetlist};
use crate::tech_library::CellFunction;

use super::aig::{Aig, AigLit, AigNodeId, AigSafetyInfo, BarrierType};

/// Builder for converting GateNetlist to AIG
pub struct AigBuilder<'a> {
    /// Source gate netlist
    netlist: &'a GateNetlist,

    /// The AIG being built
    aig: Aig,

    /// Mapping from GateNetId to AigLit
    net_map: Vec<Option<AigLit>>,

    /// Clock input node IDs
    clock_nodes: Vec<AigNodeId>,

    /// Reset input node IDs
    reset_nodes: Vec<AigNodeId>,
}

impl<'a> AigBuilder<'a> {
    /// Create a new builder from a gate netlist
    pub fn new(netlist: &'a GateNetlist) -> Self {
        Self {
            netlist,
            aig: Aig::new(netlist.name.clone()),
            net_map: vec![None; netlist.nets.len()],
            clock_nodes: Vec::new(),
            reset_nodes: Vec::new(),
        }
    }

    /// Build the AIG from the gate netlist
    pub fn build(mut self) -> Aig {
        // Phase 1: Create input nodes for primary inputs
        self.build_inputs();

        // Phase 2: Pre-create latch nodes for sequential feedback
        // This handles cycles where combinational cells depend on latch outputs
        self.pre_create_latches();

        // Phase 3: Process cells in topological order
        self.process_cells();

        // Phase 4: Create outputs
        self.build_outputs();

        self.aig
    }

    /// Pre-create latch nodes for all DFF cells to handle feedback loops
    /// This ensures latch output nets are mapped before combinational cells try to read them
    fn pre_create_latches(&mut self) {
        let mut latch_count = 0;
        for cell in &self.netlist.cells {
            let cell_type = cell.cell_type.to_uppercase();
            let base = cell_type.split('_').next().unwrap_or(&cell_type);

            // Check if this is a DFF/latch cell
            if matches!(
                base,
                "DFF" | "DFFR" | "DFFE" | "DFFRE" | "DFFRQ" | "DFFQ" | "LATCH"
            ) {
                // Pre-create latch node with placeholder input
                // The real input will be connected when the cell is processed
                let clock = cell
                    .clock
                    .and_then(|c| self.net_map[c.0 as usize])
                    .map(|l| l.node);
                let reset = cell
                    .reset
                    .and_then(|r| self.net_map[r.0 as usize])
                    .map(|l| l.node);

                eprintln!("[AIG_BUILDER] Pre-creating latch for cell {} (type={}), clock={:?}, reset={:?}",
                    cell.id.0, cell.cell_type, clock, reset);

                // Create latch with false_lit as placeholder - will be updated later
                let latch_id = self.aig.add_latch(AigLit::false_lit(), None, clock, reset);
                let latch_lit = AigLit::new(latch_id);

                // Map all output nets to this latch
                for &output_net in &cell.outputs {
                    let net_name = &self.netlist.nets[output_net.0 as usize].name;
                    eprintln!(
                        "[AIG_BUILDER]   Mapping output net {} ({}) to latch {:?}",
                        output_net.0, net_name, latch_id
                    );
                    self.net_map[output_net.0 as usize] = Some(latch_lit);
                    self.aig.register_net(output_net, latch_lit);
                }
                latch_count += 1;
            }
        }
        eprintln!("[AIG_BUILDER] Pre-created {} latches", latch_count);
    }

    /// Create AIG inputs for primary inputs
    fn build_inputs(&mut self) {
        for &net_id in &self.netlist.inputs {
            let net = &self.netlist.nets[net_id.0 as usize];

            // Create AIG input node
            let aig_id = self.aig.add_input(net.name.clone(), Some(net_id));

            let lit = AigLit::new(aig_id);
            self.net_map[net_id.0 as usize] = Some(lit);
            self.aig.register_net(net_id, lit);

            // Track clock and reset inputs
            if net.is_clock {
                self.clock_nodes.push(aig_id);
            }
            if net.is_reset {
                self.reset_nodes.push(aig_id);
            }
        }
    }

    /// Process all cells and convert to AIG nodes
    fn process_cells(&mut self) {
        // Build a lookup map from CellId to Cell reference
        // since CellIds may not be contiguous indices
        let cells_by_id: std::collections::HashMap<CellId, &Cell> =
            self.netlist.cells.iter().map(|c| (c.id, c)).collect();

        // Topological sort cells by dependency
        let cell_order = self.topological_order();

        for cell_id in cell_order {
            if let Some(cell) = cells_by_id.get(&cell_id) {
                self.process_cell(cell);
            }
        }
    }

    /// Get cells in topological order
    fn topological_order(&self) -> Vec<CellId> {
        use std::collections::{HashMap, HashSet, VecDeque};

        let mut result = Vec::with_capacity(self.netlist.cells.len());
        let mut ready: VecDeque<CellId> = VecDeque::new();
        let mut processed: HashSet<CellId> = HashSet::new();

        // Build dependency map: cell -> cells it depends on
        // Use HashMap since CellIds may not be contiguous
        let mut deps: HashMap<CellId, HashSet<CellId>> = HashMap::new();

        for cell in &self.netlist.cells {
            let cell_deps = deps.entry(cell.id).or_default();
            for &input_net in &cell.inputs {
                if (input_net.0 as usize) < self.netlist.nets.len() {
                    let net = &self.netlist.nets[input_net.0 as usize];
                    if let Some(driver_id) = net.driver {
                        cell_deps.insert(driver_id);
                    }
                }
            }
        }

        // Find cells with no dependencies (or only primary inputs)
        for cell in &self.netlist.cells {
            if deps.get(&cell.id).map(|d| d.is_empty()).unwrap_or(true) {
                ready.push_back(cell.id);
            }
        }

        // Process in topological order
        while let Some(cell_id) = ready.pop_front() {
            if processed.contains(&cell_id) {
                continue;
            }

            result.push(cell_id);
            processed.insert(cell_id);

            // Find cells that depend on this one and may now be ready
            for cell in &self.netlist.cells {
                if !processed.contains(&cell.id) {
                    if let Some(cell_deps) = deps.get_mut(&cell.id) {
                        cell_deps.remove(&cell_id);
                        if cell_deps.is_empty() || cell_deps.iter().all(|d| processed.contains(d)) {
                            ready.push_back(cell.id);
                        }
                    }
                }
            }
        }

        // Handle any remaining cells (cycles - shouldn't happen in valid netlists)
        for cell in &self.netlist.cells {
            if !processed.contains(&cell.id) {
                result.push(cell.id);
            }
        }

        result
    }

    /// Process a single cell
    fn process_cell(&mut self, cell: &Cell) {
        // Get input literals
        let inputs: Vec<AigLit> = cell
            .inputs
            .iter()
            .map(|&net_id| self.get_or_create_net_lit(net_id))
            .collect();

        // Parse cell function from name or type
        let function = self.parse_cell_function(&cell.cell_type);

        // Get net names for debugging
        let input_net_names: Vec<_> = cell
            .inputs
            .iter()
            .map(|&net_id| {
                let net_name = &self.netlist.nets[net_id.0 as usize].name;
                format!("{}({})", net_id.0, net_name)
            })
            .collect();
        eprintln!(
            "[AIG_BUILDER] Processing cell {} (type={}, fn={:?})",
            cell.id.0, cell.cell_type, function
        );
        eprintln!("              input_nets={:?}", input_net_names);
        eprintln!("              input_lits={:?}", inputs);
        eprintln!("              outputs={:?}", cell.outputs);

        // Build AIG for this cell
        let safety =
            AigSafetyInfo::from_cell(cell.id, cell.fit, cell.safety_classification.clone());

        let output_lit = match function {
            // Inverter
            CellFunction::Inv => {
                let a = inputs.first().copied().unwrap_or(AigLit::false_lit());
                a.invert()
            }

            // Buffer
            CellFunction::Buf => inputs.first().copied().unwrap_or(AigLit::false_lit()),

            // Tie cells (constant outputs)
            CellFunction::TieHigh => AigLit::true_lit(),
            CellFunction::TieLow => AigLit::false_lit(),

            // AND gates
            CellFunction::And2 => {
                let a = inputs.first().copied().unwrap_or(AigLit::false_lit());
                let b = inputs.get(1).copied().unwrap_or(AigLit::false_lit());
                self.aig.add_and_with_safety(a, b, safety.clone())
            }
            CellFunction::And3 => {
                let a = inputs.first().copied().unwrap_or(AigLit::false_lit());
                let b = inputs.get(1).copied().unwrap_or(AigLit::false_lit());
                let c = inputs.get(2).copied().unwrap_or(AigLit::false_lit());
                let ab = self.aig.add_and(a, b);
                self.aig.add_and_with_safety(ab, c, safety.clone())
            }
            CellFunction::And4 => {
                let a = inputs.first().copied().unwrap_or(AigLit::false_lit());
                let b = inputs.get(1).copied().unwrap_or(AigLit::false_lit());
                let c = inputs.get(2).copied().unwrap_or(AigLit::false_lit());
                let d = inputs.get(3).copied().unwrap_or(AigLit::false_lit());
                let ab = self.aig.add_and(a, b);
                let cd = self.aig.add_and(c, d);
                self.aig.add_and_with_safety(ab, cd, safety.clone())
            }

            // NAND gates
            CellFunction::Nand2 => {
                let a = inputs.first().copied().unwrap_or(AigLit::false_lit());
                let b = inputs.get(1).copied().unwrap_or(AigLit::false_lit());
                self.aig.add_and_with_safety(a, b, safety.clone()).invert()
            }
            CellFunction::Nand3 => {
                let a = inputs.first().copied().unwrap_or(AigLit::false_lit());
                let b = inputs.get(1).copied().unwrap_or(AigLit::false_lit());
                let c = inputs.get(2).copied().unwrap_or(AigLit::false_lit());
                let ab = self.aig.add_and(a, b);
                self.aig.add_and_with_safety(ab, c, safety.clone()).invert()
            }
            CellFunction::Nand4 => {
                let a = inputs.first().copied().unwrap_or(AigLit::false_lit());
                let b = inputs.get(1).copied().unwrap_or(AigLit::false_lit());
                let c = inputs.get(2).copied().unwrap_or(AigLit::false_lit());
                let d = inputs.get(3).copied().unwrap_or(AigLit::false_lit());
                let ab = self.aig.add_and(a, b);
                let cd = self.aig.add_and(c, d);
                self.aig
                    .add_and_with_safety(ab, cd, safety.clone())
                    .invert()
            }

            // OR gates: a | b = !(!a & !b)
            CellFunction::Or2 => {
                let a = inputs.first().copied().unwrap_or(AigLit::false_lit());
                let b = inputs.get(1).copied().unwrap_or(AigLit::false_lit());
                // OR is NAND of inverted inputs - assign safety to the AND node
                let nand = self
                    .aig
                    .add_and_with_safety(a.invert(), b.invert(), safety.clone());
                nand.invert()
            }
            CellFunction::Or3 => {
                let a = inputs.first().copied().unwrap_or(AigLit::false_lit());
                let b = inputs.get(1).copied().unwrap_or(AigLit::false_lit());
                let c = inputs.get(2).copied().unwrap_or(AigLit::false_lit());
                let ab = self.aig.add_or(a, b);
                let nand = self
                    .aig
                    .add_and_with_safety(ab.invert(), c.invert(), safety.clone());
                nand.invert()
            }
            CellFunction::Or4 => {
                let a = inputs.first().copied().unwrap_or(AigLit::false_lit());
                let b = inputs.get(1).copied().unwrap_or(AigLit::false_lit());
                let c = inputs.get(2).copied().unwrap_or(AigLit::false_lit());
                let d = inputs.get(3).copied().unwrap_or(AigLit::false_lit());
                let ab = self.aig.add_or(a, b);
                let cd = self.aig.add_or(c, d);
                let nand = self
                    .aig
                    .add_and_with_safety(ab.invert(), cd.invert(), safety.clone());
                nand.invert()
            }

            // NOR gates: !(a | b) = !a & !b
            CellFunction::Nor2 => {
                let a = inputs.first().copied().unwrap_or(AigLit::false_lit());
                let b = inputs.get(1).copied().unwrap_or(AigLit::false_lit());
                self.aig
                    .add_and_with_safety(a.invert(), b.invert(), safety.clone())
            }

            // ANDNOT: a & ~b
            CellFunction::AndNot => {
                let a = inputs.first().copied().unwrap_or(AigLit::false_lit());
                let b = inputs.get(1).copied().unwrap_or(AigLit::false_lit());
                self.aig.add_and_with_safety(a, b.invert(), safety.clone())
            }

            // ORNOT: a | ~b = ~(~a & b)
            CellFunction::OrNot => {
                let a = inputs.first().copied().unwrap_or(AigLit::false_lit());
                let b = inputs.get(1).copied().unwrap_or(AigLit::false_lit());
                self.aig
                    .add_and_with_safety(a.invert(), b, safety.clone())
                    .invert()
            }

            CellFunction::Nor3 => {
                let a = inputs.first().copied().unwrap_or(AigLit::false_lit());
                let b = inputs.get(1).copied().unwrap_or(AigLit::false_lit());
                let c = inputs.get(2).copied().unwrap_or(AigLit::false_lit());
                let ab = self.aig.add_and(a.invert(), b.invert());
                self.aig.add_and_with_safety(ab, c.invert(), safety.clone())
            }
            CellFunction::Nor4 => {
                let a = inputs.first().copied().unwrap_or(AigLit::false_lit());
                let b = inputs.get(1).copied().unwrap_or(AigLit::false_lit());
                let c = inputs.get(2).copied().unwrap_or(AigLit::false_lit());
                let d = inputs.get(3).copied().unwrap_or(AigLit::false_lit());
                let ab = self.aig.add_and(a.invert(), b.invert());
                let cd = self.aig.add_and(c.invert(), d.invert());
                self.aig.add_and_with_safety(ab, cd, safety.clone())
            }

            // XOR gate: a ^ b = (a & !b) | (!a & b)
            CellFunction::Xor2 => {
                let a = inputs.first().copied().unwrap_or(AigLit::false_lit());
                let b = inputs.get(1).copied().unwrap_or(AigLit::false_lit());
                self.aig.add_xor(a, b)
            }

            // XNOR gate: !(a ^ b) = (a & b) | (!a & !b)
            CellFunction::Xnor2 => {
                let a = inputs.first().copied().unwrap_or(AigLit::false_lit());
                let b = inputs.get(1).copied().unwrap_or(AigLit::false_lit());
                self.aig.add_xor(a, b).invert()
            }

            // AOI21: !((a & b) | c)
            CellFunction::Aoi21 => {
                let a = inputs.first().copied().unwrap_or(AigLit::false_lit());
                let b = inputs.get(1).copied().unwrap_or(AigLit::false_lit());
                let c = inputs.get(2).copied().unwrap_or(AigLit::false_lit());
                let ab = self.aig.add_and(a, b);
                self.aig.add_or(ab, c).invert()
            }

            // AOI22: !((a & b) | (c & d))
            CellFunction::Aoi22 => {
                let a = inputs.first().copied().unwrap_or(AigLit::false_lit());
                let b = inputs.get(1).copied().unwrap_or(AigLit::false_lit());
                let c = inputs.get(2).copied().unwrap_or(AigLit::false_lit());
                let d = inputs.get(3).copied().unwrap_or(AigLit::false_lit());
                let ab = self.aig.add_and(a, b);
                let cd = self.aig.add_and(c, d);
                self.aig.add_or(ab, cd).invert()
            }

            // OAI21: !((a | b) & c)
            CellFunction::Oai21 => {
                let a = inputs.first().copied().unwrap_or(AigLit::false_lit());
                let b = inputs.get(1).copied().unwrap_or(AigLit::false_lit());
                let c = inputs.get(2).copied().unwrap_or(AigLit::false_lit());
                let ab = self.aig.add_or(a, b);
                self.aig.add_and(ab, c).invert()
            }

            // OAI22: !((a | b) & (c | d))
            CellFunction::Oai22 => {
                let a = inputs.first().copied().unwrap_or(AigLit::false_lit());
                let b = inputs.get(1).copied().unwrap_or(AigLit::false_lit());
                let c = inputs.get(2).copied().unwrap_or(AigLit::false_lit());
                let d = inputs.get(3).copied().unwrap_or(AigLit::false_lit());
                let ab = self.aig.add_or(a, b);
                let cd = self.aig.add_or(c, d);
                self.aig.add_and(ab, cd).invert()
            }

            // MUX2: sel ? d1 : d0
            CellFunction::Mux2 => {
                // Inputs are typically: sel, d0, d1
                let sel = inputs.first().copied().unwrap_or(AigLit::false_lit());
                let d0 = inputs.get(1).copied().unwrap_or(AigLit::false_lit());
                let d1 = inputs.get(2).copied().unwrap_or(AigLit::false_lit());
                self.aig.add_mux(sel, d1, d0)
            }

            // Half adder: sum = a ^ b, cout = a & b
            // This is a multi-output cell - handle specially
            CellFunction::HalfAdder => {
                let a = inputs.first().copied().unwrap_or(AigLit::false_lit());
                let b = inputs.get(1).copied().unwrap_or(AigLit::false_lit());

                // Compute both outputs
                let sum_lit = self.aig.add_xor(a, b);
                let carry_lit = self.aig.add_and(a, b);

                // Map outputs: first is sum, second is carry
                if let Some(&sum_net) = cell.outputs.first() {
                    self.net_map[sum_net.0 as usize] = Some(sum_lit);
                    self.aig.register_net(sum_net, sum_lit);
                }
                if let Some(&carry_net) = cell.outputs.get(1) {
                    self.net_map[carry_net.0 as usize] = Some(carry_lit);
                    self.aig.register_net(carry_net, carry_lit);
                }

                // Return early - we've already mapped outputs
                return;
            }

            // Full adder: sum = a ^ b ^ cin, cout = (a & b) | (cin & (a ^ b))
            // This is a multi-output cell - handle specially
            CellFunction::FullAdder => {
                let a = inputs.first().copied().unwrap_or(AigLit::false_lit());
                let b = inputs.get(1).copied().unwrap_or(AigLit::false_lit());
                let cin = inputs.get(2).copied().unwrap_or(AigLit::false_lit());

                // Compute both outputs
                let ab_xor = self.aig.add_xor(a, b);
                let sum_lit = self.aig.add_xor(ab_xor, cin);

                // Carry = (a & b) | (cin & (a ^ b)) = majority function
                let ab_and = self.aig.add_and(a, b);
                let cin_ab_xor = self.aig.add_and(cin, ab_xor);
                let carry_lit = self.aig.add_or(ab_and, cin_ab_xor);

                // Map outputs: first is sum, second is carry
                if let Some(&sum_net) = cell.outputs.first() {
                    self.net_map[sum_net.0 as usize] = Some(sum_lit);
                    self.aig.register_net(sum_net, sum_lit);
                }
                if let Some(&cout_net) = cell.outputs.get(1) {
                    self.net_map[cout_net.0 as usize] = Some(carry_lit);
                    self.aig.register_net(cout_net, carry_lit);
                }

                // Return early - we've already mapped outputs
                return;
            }

            // Sequential elements
            // Note: Latches are pre-created in pre_create_latches() to handle feedback loops
            // Here we just update their data input with the computed value
            CellFunction::Dff | CellFunction::DffR | CellFunction::DffE | CellFunction::DffRE => {
                let d = inputs.first().copied().unwrap_or(AigLit::false_lit());
                eprintln!(
                    "[AIG_BUILDER] Processing DFF cell {}, d input = {:?}",
                    cell.id.0, d
                );

                // Find the pre-created latch for this cell's output
                if let Some(&output_net) = cell.outputs.first() {
                    if let Some(existing_lit) = self.net_map[output_net.0 as usize] {
                        // Update the latch with the real data input
                        eprintln!(
                            "[AIG_BUILDER]   Updating pre-created latch {:?} with d={:?}",
                            existing_lit.node, d
                        );
                        self.aig.update_latch_data(existing_lit.node, d);
                        return; // Already mapped, don't create new one
                    }
                }

                // Fallback: create new latch if not pre-created
                let clock = cell
                    .clock
                    .and_then(|c| self.net_map[c.0 as usize])
                    .map(|l| l.node);
                let reset = cell
                    .reset
                    .and_then(|r| self.net_map[r.0 as usize])
                    .map(|l| l.node);

                let latch_id =
                    self.aig
                        .add_latch_with_safety(d, None, clock, reset, safety.clone());
                AigLit::new(latch_id)
            }

            CellFunction::Latch => {
                let d = inputs.first().copied().unwrap_or(AigLit::false_lit());

                // Find the pre-created latch for this cell's output
                if let Some(&output_net) = cell.outputs.first() {
                    if let Some(existing_lit) = self.net_map[output_net.0 as usize] {
                        // Update the latch with the real data input
                        self.aig.update_latch_data(existing_lit.node, d);
                        return; // Already mapped, don't create new one
                    }
                }

                // Fallback: create new latch if not pre-created
                let clock = cell
                    .clock
                    .and_then(|c| self.net_map[c.0 as usize])
                    .map(|l| l.node);

                let latch_id = self
                    .aig
                    .add_latch_with_safety(d, None, clock, None, safety.clone());
                AigLit::new(latch_id)
            }

            // Tristate (model as buffer for now - full tristate needs Z support)
            CellFunction::Tristate => {
                let a = inputs.first().copied().unwrap_or(AigLit::false_lit());
                let en = inputs.get(1).copied().unwrap_or(AigLit::true_lit());
                // Model as: en ? a : 0 (simplified)
                self.aig.add_and(en, a)
            }

            // Power infrastructure cells - create BARRIER nodes to prevent cross-domain optimization
            CellFunction::LevelShifterLH => {
                let data = inputs.first().copied().unwrap_or(AigLit::false_lit());
                let barrier_id = self.aig.add_barrier_with_safety(
                    BarrierType::LevelShifterLH,
                    data,
                    None,
                    None,
                    None,
                    None,
                    safety.clone(),
                );
                AigLit::new(barrier_id)
            }

            CellFunction::LevelShifterHL => {
                let data = inputs.first().copied().unwrap_or(AigLit::false_lit());
                let barrier_id = self.aig.add_barrier_with_safety(
                    BarrierType::LevelShifterHL,
                    data,
                    None,
                    None,
                    None,
                    None,
                    safety.clone(),
                );
                AigLit::new(barrier_id)
            }

            CellFunction::AlwaysOnBuf => {
                let data = inputs.first().copied().unwrap_or(AigLit::false_lit());
                let barrier_id = self.aig.add_barrier_with_safety(
                    BarrierType::AlwaysOnBuf,
                    data,
                    None,
                    None,
                    None,
                    None,
                    safety.clone(),
                );
                AigLit::new(barrier_id)
            }

            CellFunction::IsolationAnd => {
                let data = inputs.first().copied().unwrap_or(AigLit::false_lit());
                let enable = inputs.get(1).copied();
                let barrier_id = self.aig.add_barrier_with_safety(
                    BarrierType::IsolationAnd,
                    data,
                    enable,
                    None,
                    None,
                    None,
                    safety.clone(),
                );
                AigLit::new(barrier_id)
            }

            CellFunction::IsolationOr => {
                let data = inputs.first().copied().unwrap_or(AigLit::false_lit());
                let enable = inputs.get(1).copied();
                let barrier_id = self.aig.add_barrier_with_safety(
                    BarrierType::IsolationOr,
                    data,
                    enable,
                    None,
                    None,
                    None,
                    safety.clone(),
                );
                AigLit::new(barrier_id)
            }

            CellFunction::IsolationLatch => {
                let d = inputs.first().copied().unwrap_or(AigLit::false_lit());
                let clock = cell
                    .clock
                    .and_then(|c| self.net_map[c.0 as usize])
                    .map(|l| l.node);

                let barrier_id = self.aig.add_barrier_with_safety(
                    BarrierType::IsolationLatch,
                    d,
                    None,
                    clock,
                    None,
                    None,
                    safety.clone(),
                );
                AigLit::new(barrier_id)
            }

            CellFunction::RetentionDff => {
                let d = inputs.first().copied().unwrap_or(AigLit::false_lit());
                let clock = cell
                    .clock
                    .and_then(|c| self.net_map[c.0 as usize])
                    .map(|l| l.node);

                let barrier_id = self.aig.add_barrier_with_safety(
                    BarrierType::RetentionDff,
                    d,
                    None,
                    clock,
                    None,
                    None,
                    safety.clone(),
                );
                AigLit::new(barrier_id)
            }

            CellFunction::RetentionDffR => {
                let d = inputs.first().copied().unwrap_or(AigLit::false_lit());
                let clock = cell
                    .clock
                    .and_then(|c| self.net_map[c.0 as usize])
                    .map(|l| l.node);
                let reset = cell
                    .reset
                    .and_then(|r| self.net_map[r.0 as usize])
                    .map(|l| l.node);

                let barrier_id = self.aig.add_barrier_with_safety(
                    BarrierType::RetentionDffR,
                    d,
                    None,
                    clock,
                    reset,
                    None,
                    safety.clone(),
                );
                AigLit::new(barrier_id)
            }

            CellFunction::PowerSwitchHeader => {
                let data = inputs.first().copied().unwrap_or(AigLit::true_lit());
                let barrier_id = self.aig.add_barrier_with_safety(
                    BarrierType::PowerSwitchHeader,
                    data,
                    None,
                    None,
                    None,
                    None,
                    safety.clone(),
                );
                AigLit::new(barrier_id)
            }

            CellFunction::PowerSwitchFooter => {
                let data = inputs.first().copied().unwrap_or(AigLit::true_lit());
                let barrier_id = self.aig.add_barrier_with_safety(
                    BarrierType::PowerSwitchFooter,
                    data,
                    None,
                    None,
                    None,
                    None,
                    safety.clone(),
                );
                AigLit::new(barrier_id)
            }

            // N-bit adders and MUX4 - complex cells decomposed elsewhere
            CellFunction::Adder(_) | CellFunction::Mux4 => {
                // These should be decomposed before reaching AIG builder
                // For now, just pass through first input
                inputs.first().copied().unwrap_or(AigLit::false_lit())
            }

            CellFunction::Custom(_) => {
                // Unknown cell - just pass through first input
                inputs.first().copied().unwrap_or(AigLit::false_lit())
            }
        };

        // Map cell outputs to AIG literals
        for &output_net in &cell.outputs {
            self.net_map[output_net.0 as usize] = Some(output_lit);
            self.aig.register_net(output_net, output_lit);
        }
    }

    /// Get or create a literal for a net
    fn get_or_create_net_lit(&mut self, net_id: GateNetId) -> AigLit {
        if let Some(lit) = self.net_map[net_id.0 as usize] {
            return lit;
        }

        // Check if this is a constant net
        let net = &self.netlist.nets[net_id.0 as usize];

        // Handle special constant nets
        if net.name.contains("const_0") || net.name.contains("gnd") || net.name.contains("vss") {
            let lit = AigLit::false_lit();
            self.net_map[net_id.0 as usize] = Some(lit);
            return lit;
        }
        if net.name.contains("const_1") || net.name.contains("vdd") || net.name.contains("vcc") {
            let lit = AigLit::true_lit();
            self.net_map[net_id.0 as usize] = Some(lit);
            return lit;
        }

        // Unknown net - treat as primary input
        // This can happen for nets that weren't explicitly added as inputs
        // but are referenced by cells (e.g., internal nets without drivers)
        let id = self.aig.add_input(net.name.clone(), Some(net_id));
        let lit = AigLit::new(id);
        self.net_map[net_id.0 as usize] = Some(lit);
        self.aig.register_net(net_id, lit);
        lit
    }

    /// Create AIG outputs
    fn build_outputs(&mut self) {
        for &net_id in &self.netlist.outputs {
            let net = &self.netlist.nets[net_id.0 as usize];
            let lit = self.get_or_create_net_lit(net_id);
            self.aig.add_output(net.name.clone(), lit);
        }
    }

    /// Parse cell function from cell type name
    fn parse_cell_function(&self, cell_type: &str) -> CellFunction {
        // First check for power domain cells (which have multi-part names like LVLSHIFT_LH_X1)
        let upper = cell_type.to_uppercase();

        // Check power domain cells first (need to check before stripping suffix)
        if upper.starts_with("LVLSHIFT_LH") || upper.starts_with("LVLSHIFTLH") {
            return CellFunction::LevelShifterLH;
        }
        if upper.starts_with("LVLSHIFT_HL") || upper.starts_with("LVLSHIFTHL") {
            return CellFunction::LevelShifterHL;
        }
        if upper.starts_with("ISO_AND") || upper.starts_with("ISOAND") {
            return CellFunction::IsolationAnd;
        }
        if upper.starts_with("ISO_OR") || upper.starts_with("ISOOR") {
            return CellFunction::IsolationOr;
        }
        if upper.starts_with("ISO_LATCH") || upper.starts_with("ISOLATCH") {
            return CellFunction::IsolationLatch;
        }
        if upper.starts_with("AON_BUF") || upper.starts_with("AONBUF") {
            return CellFunction::AlwaysOnBuf;
        }
        if upper.starts_with("RET_DFF_R") || upper.starts_with("RETDFFR") {
            return CellFunction::RetentionDffR;
        }
        if upper.starts_with("RET_DFF") || upper.starts_with("RETDFF") {
            return CellFunction::RetentionDff;
        }
        if upper.starts_with("PSW_HDR") || upper.starts_with("PSWHDR") {
            return CellFunction::PowerSwitchHeader;
        }
        if upper.starts_with("PSW_FTR") || upper.starts_with("PSWFTR") {
            return CellFunction::PowerSwitchFooter;
        }

        // Check tie cells (TIE_HIGH, TIE_LOW have underscore in name)
        if upper.starts_with("TIE_HIGH")
            || upper.starts_with("TIEHIGH")
            || upper.starts_with("TIEHI")
        {
            return CellFunction::TieHigh;
        }
        if upper.starts_with("TIE_LOW") || upper.starts_with("TIELOW") || upper.starts_with("TIELO")
        {
            return CellFunction::TieLow;
        }

        // Normalize name (remove drive strength suffix like _X1, _X2)
        let base_name = cell_type
            .split('_')
            .next()
            .unwrap_or(cell_type)
            .to_uppercase();

        match base_name.as_str() {
            "INV" | "NOT" => CellFunction::Inv,
            "BUF" | "BUFF" | "BUFFER" => CellFunction::Buf,
            "NAND2" => CellFunction::Nand2,
            "NAND3" => CellFunction::Nand3,
            "NAND4" => CellFunction::Nand4,
            "NOR2" => CellFunction::Nor2,
            "NOR3" => CellFunction::Nor3,
            "NOR4" => CellFunction::Nor4,
            "AND2" => CellFunction::And2,
            "AND3" => CellFunction::And3,
            "AND4" => CellFunction::And4,
            "OR2" => CellFunction::Or2,
            "OR3" => CellFunction::Or3,
            "OR4" => CellFunction::Or4,
            "XOR2" | "XOR" => CellFunction::Xor2,
            "XNOR2" | "XNOR" => CellFunction::Xnor2,
            "AOI21" => CellFunction::Aoi21,
            "AOI22" => CellFunction::Aoi22,
            "OAI21" => CellFunction::Oai21,
            "OAI22" => CellFunction::Oai22,
            "MUX2" | "MUX" => CellFunction::Mux2,
            "MUX4" => CellFunction::Mux4,
            "HA" | "HALFADDER" => CellFunction::HalfAdder,
            "FA" | "FULLADDER" => CellFunction::FullAdder,
            "DFF" | "DFFQ" => CellFunction::Dff,
            "DFFR" | "DFFRQ" => CellFunction::DffR,
            "DFFE" | "DFFEQ" => CellFunction::DffE,
            "DFFRE" | "DFFREQ" => CellFunction::DffRE,
            "LATCH" | "LAT" => CellFunction::Latch,
            "TRI" | "TRISTATE" | "TRIBUF" => CellFunction::Tristate,
            // Tie cells for constant outputs
            "TIE_HIGH" | "TIEHIGH" | "TIEHI" | "VDD" => CellFunction::TieHigh,
            "TIE_LOW" | "TIELOW" | "TIELO" | "VSS" | "GND" => CellFunction::TieLow,
            _ => CellFunction::Custom(cell_type.to_string()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::gate_netlist::{Cell, GateNet, GateNetlist};

    fn create_simple_netlist() -> GateNetlist {
        let mut netlist = GateNetlist::new("test".to_string(), "generic".to_string());

        // Add inputs
        let a = netlist.add_input("a".to_string());
        let b = netlist.add_input("b".to_string());

        // Add internal net
        let and_out = netlist.add_net(GateNet::new(GateNetId(0), "and_out".to_string()));

        // Add output
        let y = netlist.add_output("y".to_string());

        // Add AND gate
        let and_cell = Cell::new_comb(
            CellId(0),
            "AND2_X1".to_string(),
            "generic".to_string(),
            0.1,
            "top.and".to_string(),
            vec![a, b],
            vec![and_out],
        );
        netlist.add_cell(and_cell);

        // Add buffer to output
        let buf_cell = Cell::new_comb(
            CellId(0),
            "BUF_X1".to_string(),
            "generic".to_string(),
            0.05,
            "top.buf".to_string(),
            vec![and_out],
            vec![y],
        );
        netlist.add_cell(buf_cell);

        netlist
    }

    #[test]
    fn test_builder_basic() {
        let netlist = create_simple_netlist();
        let builder = AigBuilder::new(&netlist);
        let aig = builder.build();

        assert_eq!(aig.input_count(), 2);
        assert_eq!(aig.output_count(), 1);
        // Should have 1 AND node (buffer is pass-through)
        assert_eq!(aig.and_count(), 1);
    }

    #[test]
    fn test_builder_nand() {
        let mut netlist = GateNetlist::new("test".to_string(), "generic".to_string());

        let a = netlist.add_input("a".to_string());
        let b = netlist.add_input("b".to_string());
        let y = netlist.add_output("y".to_string());

        let nand_cell = Cell::new_comb(
            CellId(0),
            "NAND2_X1".to_string(),
            "generic".to_string(),
            0.1,
            "top.nand".to_string(),
            vec![a, b],
            vec![y],
        );
        netlist.add_cell(nand_cell);

        let builder = AigBuilder::new(&netlist);
        let aig = builder.build();

        assert_eq!(aig.and_count(), 1);
        // Output should be inverted
        let (_, out_lit) = &aig.outputs()[0];
        assert!(out_lit.inverted);
    }

    #[test]
    fn test_builder_xor() {
        let mut netlist = GateNetlist::new("test".to_string(), "generic".to_string());

        let a = netlist.add_input("a".to_string());
        let b = netlist.add_input("b".to_string());
        let y = netlist.add_output("y".to_string());

        let xor_cell = Cell::new_comb(
            CellId(0),
            "XOR2_X1".to_string(),
            "generic".to_string(),
            0.2,
            "top.xor".to_string(),
            vec![a, b],
            vec![y],
        );
        netlist.add_cell(xor_cell);

        let builder = AigBuilder::new(&netlist);
        let aig = builder.build();

        // XOR uses 3 AND gates
        assert_eq!(aig.and_count(), 3);
    }

    #[test]
    fn test_builder_with_dff() {
        let mut netlist = GateNetlist::new("test".to_string(), "generic".to_string());

        let clk = netlist.add_clock("clk".to_string());
        let d = netlist.add_input("d".to_string());
        let q = netlist.add_output("q".to_string());

        let dff_cell = Cell::new_seq(
            CellId(0),
            "DFF_X1".to_string(),
            "generic".to_string(),
            0.2,
            "top.dff".to_string(),
            vec![d],
            vec![q],
            clk,
            None,
        );
        netlist.add_cell(dff_cell);

        let builder = AigBuilder::new(&netlist);
        let aig = builder.build();

        assert_eq!(aig.latch_count(), 1);
        assert_eq!(aig.input_count(), 2); // clk and d
    }

    #[test]
    fn test_half_adder_both_outputs() {
        // HalfAdder has 2 outputs: sum = a ^ b, carry = a & b
        // This test verifies that both outputs are computed correctly
        // and are DIFFERENT (the bug was that all outputs got the same value)
        let mut netlist = GateNetlist::new("test".to_string(), "generic".to_string());

        let a = netlist.add_input("a".to_string());
        let b = netlist.add_input("b".to_string());
        let sum = netlist.add_output("sum".to_string());
        let carry = netlist.add_output("carry".to_string());

        let ha_cell = Cell::new_comb(
            CellId(0),
            "HA_X1".to_string(),
            "generic".to_string(),
            0.2,
            "top.ha".to_string(),
            vec![a, b],
            vec![sum, carry],
        );
        netlist.add_cell(ha_cell);

        let builder = AigBuilder::new(&netlist);
        let aig = builder.build();

        // Get the output literals
        let outputs = aig.outputs();
        assert_eq!(outputs.len(), 2, "HalfAdder should have 2 outputs");

        let (sum_name, sum_lit) = &outputs[0];
        let (carry_name, carry_lit) = &outputs[1];

        // Verify output names
        assert_eq!(sum_name, "sum");
        assert_eq!(carry_name, "carry");

        // CRITICAL: sum and carry must be DIFFERENT
        // sum = a ^ b (XOR), carry = a & b (AND)
        // These are different functions, so their AIG literals must differ
        assert_ne!(
            sum_lit, carry_lit,
            "BUG: HalfAdder sum and carry must be different! sum={:?}, carry={:?}",
            sum_lit, carry_lit
        );
    }

    #[test]
    fn test_full_adder_both_outputs() {
        // FullAdder has 2 outputs: sum = a ^ b ^ cin, carry = (a & b) | (cin & (a ^ b))
        let mut netlist = GateNetlist::new("test".to_string(), "generic".to_string());

        let a = netlist.add_input("a".to_string());
        let b = netlist.add_input("b".to_string());
        let cin = netlist.add_input("cin".to_string());
        let sum = netlist.add_output("sum".to_string());
        let cout = netlist.add_output("cout".to_string());

        let fa_cell = Cell::new_comb(
            CellId(0),
            "FA_X1".to_string(),
            "generic".to_string(),
            0.3,
            "top.fa".to_string(),
            vec![a, b, cin],
            vec![sum, cout],
        );
        netlist.add_cell(fa_cell);

        let builder = AigBuilder::new(&netlist);
        let aig = builder.build();

        let outputs = aig.outputs();
        assert_eq!(outputs.len(), 2, "FullAdder should have 2 outputs");

        let (sum_name, sum_lit) = &outputs[0];
        let (cout_name, cout_lit) = &outputs[1];

        assert_eq!(sum_name, "sum");
        assert_eq!(cout_name, "cout");

        // CRITICAL: sum and cout must be DIFFERENT
        assert_ne!(
            sum_lit, cout_lit,
            "BUG: FullAdder sum and cout must be different! sum={:?}, cout={:?}",
            sum_lit, cout_lit
        );
    }

    #[test]
    fn test_adder_chain_functional() {
        // Test a chain of half adder + full adder (2-bit adder)
        // This simulates what happens when adding counter + 1
        let mut netlist = GateNetlist::new("test".to_string(), "generic".to_string());

        // Inputs: a[0], a[1], b[0], b[1]
        let a0 = netlist.add_input("a0".to_string());
        let a1 = netlist.add_input("a1".to_string());
        let b0 = netlist.add_input("b0".to_string());
        let b1 = netlist.add_input("b1".to_string());

        // Intermediate carry
        let c0 = netlist.add_net(GateNet::new(GateNetId(0), "c0".to_string()));

        // Outputs
        let s0 = netlist.add_output("s0".to_string());
        let s1 = netlist.add_output("s1".to_string());
        let cout = netlist.add_output("cout".to_string());

        // Half adder for bit 0
        let ha_cell = Cell::new_comb(
            CellId(0),
            "HA_X1".to_string(),
            "generic".to_string(),
            0.2,
            "add.ha0".to_string(),
            vec![a0, b0],
            vec![s0, c0],
        );
        netlist.add_cell(ha_cell);

        // Full adder for bit 1
        let fa_cell = Cell::new_comb(
            CellId(1),
            "FA_X1".to_string(),
            "generic".to_string(),
            0.3,
            "add.fa1".to_string(),
            vec![a1, b1, c0],
            vec![s1, cout],
        );
        netlist.add_cell(fa_cell);

        let builder = AigBuilder::new(&netlist);
        let aig = builder.build();

        let outputs = aig.outputs();
        assert_eq!(
            outputs.len(),
            3,
            "2-bit adder should have 3 outputs (s0, s1, cout)"
        );

        // All outputs should be different
        let lits: Vec<_> = outputs.iter().map(|(_, lit)| lit).collect();
        assert_ne!(lits[0], lits[1], "s0 and s1 must be different");
        assert_ne!(lits[0], lits[2], "s0 and cout must be different");
        assert_ne!(lits[1], lits[2], "s1 and cout must be different");
    }
}
