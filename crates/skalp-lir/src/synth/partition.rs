//! Netlist Partitioning for AIG Optimization
//!
//! Extracts AIG-incompatible cells (RAM, DSP, PLL, ClkBuf, ClkDiv, Blackbox,
//! NCL threshold gates, FP macros) before building the AIG, then merges them
//! back after optimization. This mirrors the Yosys/ABC approach of partitioning
//! the netlist into combinational cones bounded by physical cells.

use crate::gate_netlist::{Cell, CellId, GateNet, GateNetId, GateNetlist};
use std::collections::{HashMap, HashSet};

/// Result of partitioning a netlist for AIG optimization.
pub struct NetlistPartition {
    /// The optimizable (AIG-compatible) sub-netlist.
    pub optimizable: GateNetlist,
    /// Extracted physical/incompatible cells.
    pub physical_cells: Vec<Cell>,
    /// Maps original net IDs → optimizable netlist net IDs.
    pub orig_to_opt: HashMap<GateNetId, GateNetId>,
    /// Maps optimizable netlist net IDs → original net IDs.
    pub opt_to_orig: HashMap<GateNetId, GateNetId>,
    /// Nets in the original netlist driven by physical cells (boundary inputs to AIG).
    pub boundary_inputs: HashSet<GateNetId>,
    /// Nets in the original netlist consumed by physical cells (boundary outputs from AIG).
    pub boundary_outputs: HashSet<GateNetId>,
    /// Original netlist metadata preserved for merge.
    pub orig_name: String,
    pub orig_library: String,
    pub orig_inputs: Vec<GateNetId>,
    pub orig_outputs: Vec<GateNetId>,
    pub orig_clocks: Vec<GateNetId>,
    pub orig_resets: Vec<GateNetId>,
    pub orig_is_ncl: bool,
}

/// Partition a netlist into AIG-compatible and physical sub-netlists.
///
/// Returns `None` if all cells are AIG-compatible (no partitioning needed).
pub fn partition_for_aig(netlist: &GateNetlist) -> Option<NetlistPartition> {
    // Identify incompatible cells
    let physical_cell_ids: HashSet<CellId> = netlist
        .cells
        .iter()
        .filter(|c| {
            // Cells with a known incompatible function
            if c.function.as_ref().is_some_and(|f| !f.is_aig_compatible()) {
                return true;
            }
            // Cells with no function that are infrastructure (SB_GND, SB_VCC, SB_IO, etc.)
            // The AIG builder can't handle these — they have no CellFunction mapping
            if c.function.is_none() {
                let upper = c.cell_type.to_uppercase();
                if upper.starts_with("SB_GND")
                    || upper.starts_with("SB_VCC")
                    || upper.starts_with("SB_IO")
                    || upper.starts_with("SB_GB")
                    || upper.starts_with("TIE")
                {
                    return true;
                }
            }
            false
        })
        .map(|c| c.id)
        .collect();

    if physical_cell_ids.is_empty() {
        return None;
    }

    // Collect nets touched by physical cells
    let mut boundary_inputs: HashSet<GateNetId> = HashSet::new(); // driven by physical, consumed by logic
    let mut boundary_outputs: HashSet<GateNetId> = HashSet::new(); // driven by logic, consumed by physical

    // Nets driven by physical cells
    let mut phys_driven_nets: HashSet<GateNetId> = HashSet::new();
    // Nets consumed by physical cells
    let mut phys_consumed_nets: HashSet<GateNetId> = HashSet::new();

    for cell in &netlist.cells {
        if physical_cell_ids.contains(&cell.id) {
            for &net_id in &cell.outputs {
                phys_driven_nets.insert(net_id);
            }
            for &net_id in &cell.inputs {
                phys_consumed_nets.insert(net_id);
            }
            if let Some(clk) = cell.clock {
                phys_consumed_nets.insert(clk);
            }
            if let Some(rst) = cell.reset {
                phys_consumed_nets.insert(rst);
            }
        }
    }

    // A net driven by a physical cell that is consumed by any logic cell is a boundary input
    // A net driven by a logic cell that is consumed by a physical cell is a boundary output
    for &net_id in &phys_driven_nets {
        let net = &netlist.nets[net_id.0 as usize];
        // Check if any fanout cell is a logic cell
        let has_logic_consumer = net
            .fanout
            .iter()
            .any(|(cid, _)| !physical_cell_ids.contains(cid));
        // Also check if it's a primary output
        if has_logic_consumer || net.is_output {
            boundary_inputs.insert(net_id);
        }
    }

    for &net_id in &phys_consumed_nets {
        let net = &netlist.nets[net_id.0 as usize];
        // If driven by a logic cell (or is a primary input), it's a boundary output
        if let Some(driver) = net.driver {
            if !physical_cell_ids.contains(&driver) {
                boundary_outputs.insert(net_id);
            }
        } else if net.is_input {
            // Primary input consumed by physical cell — it will be in the optimizable
            // netlist as a primary input anyway, so just mark as boundary output
            boundary_outputs.insert(net_id);
        }
    }

    // Build the optimizable sub-netlist
    let mut opt = GateNetlist::new(netlist.name.clone(), netlist.library_name.clone());
    let mut orig_to_opt: HashMap<GateNetId, GateNetId> = HashMap::new();
    let mut opt_to_orig: HashMap<GateNetId, GateNetId> = HashMap::new();

    // Determine which nets are needed by logic cells
    let mut needed_nets: HashSet<GateNetId> = HashSet::new();
    for cell in &netlist.cells {
        if physical_cell_ids.contains(&cell.id) {
            continue;
        }
        for &net_id in &cell.inputs {
            needed_nets.insert(net_id);
        }
        for &net_id in &cell.outputs {
            needed_nets.insert(net_id);
        }
        if let Some(clk) = cell.clock {
            needed_nets.insert(clk);
        }
        if let Some(rst) = cell.reset {
            needed_nets.insert(rst);
        }
    }
    // Also include primary inputs/outputs
    for &net_id in &netlist.inputs {
        needed_nets.insert(net_id);
    }
    for &net_id in &netlist.outputs {
        needed_nets.insert(net_id);
    }
    // And boundary nets
    for &net_id in &boundary_inputs {
        needed_nets.insert(net_id);
    }
    for &net_id in &boundary_outputs {
        needed_nets.insert(net_id);
    }

    // Copy needed nets to optimizable netlist
    for &net_id in &needed_nets {
        let orig_net = &netlist.nets[net_id.0 as usize];
        let mut new_net = GateNet::new(GateNetId(0), orig_net.name.clone());
        new_net.is_clock = orig_net.is_clock;
        new_net.is_reset = orig_net.is_reset;
        new_net.is_detection = orig_net.is_detection;
        new_net.detection_config = orig_net.detection_config.clone();

        // Nets driven by physical cells become pseudo-primary-inputs in the opt netlist
        if boundary_inputs.contains(&net_id) {
            new_net.is_input = true;
        } else {
            new_net.is_input = orig_net.is_input;
        }

        // Nets consumed by physical cells become pseudo-primary-outputs
        if boundary_outputs.contains(&net_id) && !orig_net.is_input {
            new_net.is_output = true;
        } else {
            new_net.is_output = orig_net.is_output;
        }

        let opt_id = opt.add_net(new_net);
        orig_to_opt.insert(net_id, opt_id);
        opt_to_orig.insert(opt_id, net_id);
    }

    // Set up primary inputs/outputs in opt netlist
    for &net_id in &netlist.inputs {
        if let Some(&opt_id) = orig_to_opt.get(&net_id) {
            if !opt.inputs.contains(&opt_id) {
                opt.inputs.push(opt_id);
            }
        }
    }
    // Add boundary inputs as primary inputs
    for &net_id in &boundary_inputs {
        if let Some(&opt_id) = orig_to_opt.get(&net_id) {
            if !opt.inputs.contains(&opt_id) {
                opt.inputs.push(opt_id);
            }
        }
    }
    for &net_id in &netlist.outputs {
        if let Some(&opt_id) = orig_to_opt.get(&net_id) {
            if !opt.outputs.contains(&opt_id) {
                opt.outputs.push(opt_id);
            }
        }
    }
    // Add boundary outputs as primary outputs
    for &net_id in &boundary_outputs {
        if let Some(&opt_id) = orig_to_opt.get(&net_id) {
            if !opt.outputs.contains(&opt_id) && !opt.inputs.contains(&opt_id) {
                opt.outputs.push(opt_id);
            }
        }
    }

    // Copy clocks and resets
    for &net_id in &netlist.clocks {
        if let Some(&opt_id) = orig_to_opt.get(&net_id) {
            if !opt.clocks.contains(&opt_id) {
                opt.clocks.push(opt_id);
            }
        }
    }
    for &net_id in &netlist.resets {
        if let Some(&opt_id) = orig_to_opt.get(&net_id) {
            if !opt.resets.contains(&opt_id) {
                opt.resets.push(opt_id);
            }
        }
    }

    // Copy logic cells with remapped net IDs
    for cell in &netlist.cells {
        if physical_cell_ids.contains(&cell.id) {
            continue;
        }
        let new_inputs: Vec<GateNetId> = cell
            .inputs
            .iter()
            .map(|&id| *orig_to_opt.get(&id).unwrap_or(&GateNetId(0)))
            .collect();
        let new_outputs: Vec<GateNetId> = cell
            .outputs
            .iter()
            .map(|&id| *orig_to_opt.get(&id).unwrap_or(&GateNetId(0)))
            .collect();
        let new_clock = cell.clock.and_then(|id| orig_to_opt.get(&id).copied());
        let new_reset = cell.reset.and_then(|id| orig_to_opt.get(&id).copied());

        let new_cell = Cell {
            id: CellId(0), // will be reassigned by add_cell
            cell_type: cell.cell_type.clone(),
            library: cell.library.clone(),
            function: cell.function.clone(),
            fit: cell.fit,
            failure_modes: cell.failure_modes.clone(),
            inputs: new_inputs,
            outputs: new_outputs,
            path: cell.path.clone(),
            clock: new_clock,
            reset: new_reset,
            source_op: cell.source_op.clone(),
            safety_classification: cell.safety_classification.clone(),
            lut_init: cell.lut_init,
            parameters: cell.parameters.clone(),
        };
        opt.add_cell(new_cell);
    }

    // Extract physical cells (keep original net IDs — will be remapped during merge)
    let physical_cells: Vec<Cell> = netlist
        .cells
        .iter()
        .filter(|c| physical_cell_ids.contains(&c.id))
        .cloned()
        .collect();

    Some(NetlistPartition {
        optimizable: opt,
        physical_cells,
        orig_to_opt,
        opt_to_orig,
        boundary_inputs,
        boundary_outputs,
        orig_name: netlist.name.clone(),
        orig_library: netlist.library_name.clone(),
        orig_inputs: netlist.inputs.clone(),
        orig_outputs: netlist.outputs.clone(),
        orig_clocks: netlist.clocks.clone(),
        orig_resets: netlist.resets.clone(),
        orig_is_ncl: netlist.is_ncl,
    })
}

/// Merge physical cells back into an optimized netlist.
///
/// The optimized netlist came from AIG optimization of the `optimizable` partition.
/// The AIG round-trip creates a new netlist with independent net IDs, but preserves
/// net **names** for inputs and outputs. We use name-based matching to bridge the
/// optimized netlist's ID space back to the original.
pub fn merge_after_aig(optimized: GateNetlist, partition: &NetlistPartition) -> GateNetlist {
    let mut merged = GateNetlist::new(partition.orig_name.clone(), partition.orig_library.clone());

    // Step 1: Build name-based bridge: optimizable net name → original net ID.
    // The AIG round-trip preserves names for I/O nets, so matching by name lets us
    // connect the optimized netlist back to the original net IDs.
    let mut opt_name_to_orig: HashMap<String, GateNetId> = HashMap::new();
    for net in &partition.optimizable.nets {
        if let Some(&orig_id) = partition.opt_to_orig.get(&net.id) {
            opt_name_to_orig.insert(net.name.clone(), orig_id);
        }
    }

    // Step 2: Copy all optimized nets into merged.
    // Clear I/O flags — we restore the correct ones from orig_inputs/orig_outputs.
    let mut opt_to_merged: HashMap<GateNetId, GateNetId> = HashMap::new();
    let mut orig_to_merged: HashMap<GateNetId, GateNetId> = HashMap::new();

    for net in &optimized.nets {
        let mut new_net = GateNet::new(GateNetId(0), net.name.clone());
        new_net.is_clock = net.is_clock;
        new_net.is_reset = net.is_reset;
        new_net.is_detection = net.is_detection;
        new_net.detection_config = net.detection_config.clone();
        // is_input / is_output left false — set from orig_inputs/orig_outputs later

        let merged_id = merged.add_net(new_net);
        opt_to_merged.insert(net.id, merged_id);

        // Name-based bridge: if this net name matches an optimizable net that maps
        // to an original net, record the original→merged mapping.
        if let Some(&orig_id) = opt_name_to_orig.get(&net.name) {
            orig_to_merged.insert(orig_id, merged_id);
        }
    }

    // Step 3: Create nets for physical cell connections not yet in merged.
    // These are nets used only by physical cells (not in the optimizable partition).
    for cell in &partition.physical_cells {
        let all_nets = cell
            .inputs
            .iter()
            .chain(cell.outputs.iter())
            .chain(cell.clock.iter())
            .chain(cell.reset.iter());

        for &net_id in all_nets {
            if orig_to_merged.contains_key(&net_id) {
                continue;
            }
            let net_name = format!("__phys_net_{}", net_id.0);
            let new_net = GateNet::new(GateNetId(0), net_name);
            let merged_id = merged.add_net(new_net);
            orig_to_merged.insert(net_id, merged_id);
        }
    }

    // Step 4: Copy optimized cells (remap via opt_to_merged).
    for cell in &optimized.cells {
        let new_cell = Cell {
            id: CellId(0),
            cell_type: cell.cell_type.clone(),
            library: cell.library.clone(),
            function: cell.function.clone(),
            fit: cell.fit,
            failure_modes: cell.failure_modes.clone(),
            inputs: cell.inputs.iter().map(|&id| opt_to_merged[&id]).collect(),
            outputs: cell.outputs.iter().map(|&id| opt_to_merged[&id]).collect(),
            path: cell.path.clone(),
            clock: cell.clock.map(|id| opt_to_merged[&id]),
            reset: cell.reset.map(|id| opt_to_merged[&id]),
            source_op: cell.source_op.clone(),
            safety_classification: cell.safety_classification.clone(),
            lut_init: cell.lut_init,
            parameters: cell.parameters.clone(),
        };
        merged.add_cell(new_cell);
    }

    // Step 5: Insert physical cells (remap via orig_to_merged).
    for cell in &partition.physical_cells {
        let new_cell = Cell {
            id: CellId(0),
            cell_type: cell.cell_type.clone(),
            library: cell.library.clone(),
            function: cell.function.clone(),
            fit: cell.fit,
            failure_modes: cell.failure_modes.clone(),
            inputs: cell.inputs.iter().map(|&id| orig_to_merged[&id]).collect(),
            outputs: cell.outputs.iter().map(|&id| orig_to_merged[&id]).collect(),
            path: cell.path.clone(),
            clock: cell.clock.map(|id| orig_to_merged[&id]),
            reset: cell.reset.map(|id| orig_to_merged[&id]),
            source_op: cell.source_op.clone(),
            safety_classification: cell.safety_classification.clone(),
            lut_init: cell.lut_init,
            parameters: cell.parameters.clone(),
        };
        merged.add_cell(new_cell);
    }

    // Step 6: Set primary I/O from original netlist (mapped to merged IDs).
    for &orig_id in &partition.orig_inputs {
        if let Some(&merged_id) = orig_to_merged.get(&orig_id) {
            merged.nets[merged_id.0 as usize].is_input = true;
            if !merged.inputs.contains(&merged_id) {
                merged.inputs.push(merged_id);
            }
        }
    }
    for &orig_id in &partition.orig_outputs {
        if let Some(&merged_id) = orig_to_merged.get(&orig_id) {
            merged.nets[merged_id.0 as usize].is_output = true;
            if !merged.outputs.contains(&merged_id) {
                merged.outputs.push(merged_id);
            }
        }
    }

    // Copy clocks and resets from original
    for &orig_clk in &partition.orig_clocks {
        if let Some(&merged_id) = orig_to_merged.get(&orig_clk) {
            if !merged.clocks.contains(&merged_id) {
                merged.clocks.push(merged_id);
            }
        }
    }
    for &orig_rst in &partition.orig_resets {
        if let Some(&merged_id) = orig_to_merged.get(&orig_rst) {
            if !merged.resets.contains(&merged_id) {
                merged.resets.push(merged_id);
            }
        }
    }

    merged.is_ncl = partition.orig_is_ncl;

    // Rebuild connectivity for the merged netlist
    merged.rebuild_net_connectivity();

    merged
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::gate_netlist::{Cell, CellId, GateNetId};
    use crate::tech_library::CellFunction;

    /// Test that a purely logic netlist returns None (no partitioning needed)
    #[test]
    fn test_no_partition_needed() {
        let mut netlist = GateNetlist::new("test".to_string(), "generic".to_string());
        let a = netlist.add_input("a".to_string());
        let b = netlist.add_input("b".to_string());
        let y_net = netlist.add_output("y".to_string());

        let mut cell = Cell::new_comb(
            CellId(0),
            "AND2".to_string(),
            "generic".to_string(),
            0.1,
            "top.and".to_string(),
            vec![a, b],
            vec![y_net],
        );
        cell.function = Some(CellFunction::And2);
        netlist.add_cell(cell);

        assert!(partition_for_aig(&netlist).is_none());
    }

    /// Test that a netlist with a RAM cell gets partitioned
    #[test]
    fn test_partition_with_ram() {
        let mut netlist = GateNetlist::new("test".to_string(), "generic".to_string());
        let a = netlist.add_input("a".to_string());
        let b = netlist.add_input("b".to_string());
        let ram_out = netlist.add_net(GateNet::new(GateNetId(0), "ram_out".to_string()));
        let y_net = netlist.add_output("y".to_string());

        // Logic cell: AND2
        let mut and_cell = Cell::new_comb(
            CellId(0),
            "AND2".to_string(),
            "generic".to_string(),
            0.1,
            "top.and".to_string(),
            vec![a, ram_out],
            vec![y_net],
        );
        and_cell.function = Some(CellFunction::And2);
        netlist.add_cell(and_cell);

        // Physical cell: RAM
        let mut ram_cell = Cell::new_comb(
            CellId(0),
            "RAM".to_string(),
            "generic".to_string(),
            0.5,
            "top.ram".to_string(),
            vec![b],
            vec![ram_out],
        );
        ram_cell.function = Some(CellFunction::Ram);
        netlist.add_cell(ram_cell);

        let partition = partition_for_aig(&netlist).expect("Should partition");

        // Physical cells should be extracted
        assert_eq!(partition.physical_cells.len(), 1);
        assert_eq!(
            partition.physical_cells[0].function,
            Some(CellFunction::Ram)
        );

        // Optimizable netlist should have only the AND cell
        assert_eq!(partition.optimizable.cells.len(), 1);

        // ram_out should be a boundary input (driven by RAM, consumed by logic)
        assert!(partition.boundary_inputs.contains(&ram_out));
    }

    /// Helper: build a test netlist with logic + RAM cells.
    /// Returns (netlist, primary_input_a, primary_output_y).
    fn make_logic_plus_ram() -> (GateNetlist, GateNetId, GateNetId) {
        let mut netlist = GateNetlist::new("test".to_string(), "generic".to_string());
        let a = netlist.add_input("a".to_string());
        let clk = netlist.add_input("clk".to_string());
        netlist.clocks.push(clk);
        let ram_out = netlist.add_net(GateNet::new(GateNetId(0), "ram_out".to_string()));
        let and_out = netlist.add_net(GateNet::new(GateNetId(0), "and_out".to_string()));
        let y_net = netlist.add_output("y".to_string());

        // AND2: a & ram_out → and_out
        let mut and_cell = Cell::new_comb(
            CellId(0),
            "AND2".to_string(),
            "generic".to_string(),
            0.1,
            "top.and".to_string(),
            vec![a, ram_out],
            vec![and_out],
        );
        and_cell.function = Some(CellFunction::And2);
        netlist.add_cell(and_cell);

        // BUF: and_out → y
        let mut buf_cell = Cell::new_comb(
            CellId(0),
            "BUF".to_string(),
            "generic".to_string(),
            0.05,
            "top.buf".to_string(),
            vec![and_out],
            vec![y_net],
        );
        buf_cell.function = Some(CellFunction::Buf);
        netlist.add_cell(buf_cell);

        // RAM: clk → ram_out
        let mut ram_cell = Cell::new_comb(
            CellId(0),
            "RAM".to_string(),
            "generic".to_string(),
            0.5,
            "top.ram".to_string(),
            vec![clk],
            vec![ram_out],
        );
        ram_cell.function = Some(CellFunction::Ram);
        netlist.add_cell(ram_cell);

        (netlist, a, y_net)
    }

    /// Test round-trip: partition → merge preserves cell count (identity transform)
    #[test]
    fn test_partition_merge_roundtrip() {
        let (netlist, _, _) = make_logic_plus_ram();

        let partition = partition_for_aig(&netlist).expect("Should partition");
        assert_eq!(partition.optimizable.cells.len(), 2); // AND + BUF
        assert_eq!(partition.physical_cells.len(), 1); // RAM

        // Simulate AIG optimization (identity — just return optimizable as-is)
        let merged = merge_after_aig(partition.optimizable.clone(), &partition);

        // Should have all 3 cells back
        assert_eq!(merged.cells.len(), 3);
        // Primary I/O should match original
        assert_eq!(merged.inputs.len(), netlist.inputs.len());
        assert_eq!(merged.outputs.len(), netlist.outputs.len());
        assert_eq!(merged.clocks.len(), netlist.clocks.len());
    }

    /// Test full AIG round-trip: partition → AIG build → AIG write → merge.
    /// This exercises the name-based bridging across independent ID spaces.
    #[test]
    fn test_partition_merge_aig_roundtrip() {
        use crate::synth::AigBuilder;
        use crate::synth::AigWriter;
        use crate::tech_library::{LibraryCell, TechLibrary};

        let (netlist, _, _) = make_logic_plus_ram();

        let partition = partition_for_aig(&netlist).expect("Should partition");
        assert_eq!(partition.optimizable.cells.len(), 2); // AND + BUF
        assert_eq!(partition.physical_cells.len(), 1); // RAM

        // Run the actual AIG round-trip (new ID space after this)
        let builder = AigBuilder::new(&partition.optimizable);
        let aig = builder.build();

        let mut lib = TechLibrary::new("generic");
        lib.add_cell(LibraryCell::new_comb("AND2", CellFunction::And2, 0.1));
        lib.add_cell(LibraryCell::new_comb("BUF", CellFunction::Buf, 0.05));
        lib.add_cell(LibraryCell::new_comb("INV", CellFunction::Inv, 0.05));

        let writer = AigWriter::new(&lib);
        let optimized = writer.write(&aig);

        // The optimized netlist has completely different net IDs — merge must bridge them
        let merged = merge_after_aig(optimized, &partition);

        // All cells must be present (logic cells from AIG + physical RAM)
        assert!(
            merged.cells.len() >= 2,
            "merged should have at least 2 cells (logic + RAM), got {}",
            merged.cells.len()
        );

        // The RAM cell must survive
        let ram_cells: Vec<_> = merged
            .cells
            .iter()
            .filter(|c| c.function == Some(CellFunction::Ram))
            .collect();
        assert_eq!(ram_cells.len(), 1, "RAM cell must be preserved in merge");

        // Primary I/O must match original
        assert_eq!(
            merged.inputs.len(),
            netlist.inputs.len(),
            "merged inputs count must match original"
        );
        assert_eq!(
            merged.outputs.len(),
            netlist.outputs.len(),
            "merged outputs count must match original"
        );
        assert_eq!(
            merged.clocks.len(),
            netlist.clocks.len(),
            "merged clocks count must match original"
        );

        // All nets referenced by cells must be valid
        for cell in &merged.cells {
            for &net_id in cell
                .inputs
                .iter()
                .chain(cell.outputs.iter())
                .chain(cell.clock.iter())
                .chain(cell.reset.iter())
            {
                assert!(
                    (net_id.0 as usize) < merged.nets.len(),
                    "cell {} references invalid net {}",
                    cell.cell_type,
                    net_id.0
                );
            }
        }

        // Verify I/O flags on nets are consistent with the lists
        for &input_id in &merged.inputs {
            assert!(
                merged.nets[input_id.0 as usize].is_input,
                "input net {} should have is_input=true",
                merged.nets[input_id.0 as usize].name
            );
        }
        for &output_id in &merged.outputs {
            assert!(
                merged.nets[output_id.0 as usize].is_output,
                "output net {} should have is_output=true",
                merged.nets[output_id.0 as usize].name
            );
        }

        // Verify connectivity was rebuilt (RAM should drive something)
        let ram_cell = &ram_cells[0];
        let ram_out_id = ram_cell.outputs[0];
        let ram_out_net = &merged.nets[ram_out_id.0 as usize];
        assert_eq!(
            ram_out_net.driver,
            Some(ram_cell.id),
            "RAM output net must be driven by RAM cell"
        );
    }
}
