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
            if c.function
                .as_ref()
                .is_some_and(|f| !f.is_aig_compatible())
            {
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
        orig_clocks: netlist.clocks.clone(),
        orig_resets: netlist.resets.clone(),
        orig_is_ncl: netlist.is_ncl,
    })
}

/// Merge physical cells back into an optimized netlist.
///
/// The optimized netlist came from AIG optimization of the `optimizable` partition.
/// We need to:
/// 1. Map optimized net names back to original IDs
/// 2. Insert physical cells with their original connectivity
/// 3. Reconnect boundary nets
pub fn merge_after_aig(
    optimized: GateNetlist,
    partition: &NetlistPartition,
) -> GateNetlist {
    let mut merged = GateNetlist::new(partition.orig_name.clone(), partition.orig_library.clone());

    // Build name→id map from optimized netlist for reconnection
    let mut opt_name_to_id: HashMap<String, GateNetId> = HashMap::new();
    for net in &optimized.nets {
        opt_name_to_id.insert(net.name.clone(), net.id);
    }

    // Step 1: Copy all nets from optimized netlist into merged
    let mut opt_to_merged: HashMap<GateNetId, GateNetId> = HashMap::new();
    for net in &optimized.nets {
        let mut new_net = GateNet::new(GateNetId(0), net.name.clone());
        new_net.is_input = net.is_input;
        new_net.is_output = net.is_output;
        new_net.is_clock = net.is_clock;
        new_net.is_reset = net.is_reset;
        new_net.is_detection = net.is_detection;
        new_net.detection_config = net.detection_config.clone();
        let merged_id = merged.add_net(new_net);
        opt_to_merged.insert(net.id, merged_id);
    }

    // Step 2: Add nets from physical cells that aren't already in merged
    // Build a map from original net ID → merged net ID
    let mut orig_to_merged: HashMap<GateNetId, GateNetId> = HashMap::new();

    // First, map via opt_to_orig → opt_to_merged
    for (&opt_id, &orig_id) in &partition.opt_to_orig {
        if let Some(&merged_id) = opt_to_merged.get(&opt_id) {
            orig_to_merged.insert(orig_id, merged_id);
        }
    }

    // For physical cell nets not already in merged, try matching by name or create new
    let orig_netlist_nets = &partition.optimizable; // we'll use the original partition info
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

            // If the net was in orig_to_opt, it's already in merged via opt_to_merged.
            if let Some(&opt_id) = partition.orig_to_opt.get(&net_id) {
                if let Some(&merged_id) = opt_to_merged.get(&opt_id) {
                    orig_to_merged.insert(net_id, merged_id);
                    continue;
                }
            }

            // Net not in optimizable partition — create a new net
            let net_name = format!("__phys_net_{}", net_id.0);
            let new_net = GateNet::new(GateNetId(0), net_name);
            let merged_id = merged.add_net(new_net);
            orig_to_merged.insert(net_id, merged_id);
        }
    }

    // Step 3: Copy all cells from optimized netlist
    for cell in &optimized.cells {
        let new_inputs: Vec<GateNetId> = cell
            .inputs
            .iter()
            .map(|&id| *opt_to_merged.get(&id).unwrap_or(&id))
            .collect();
        let new_outputs: Vec<GateNetId> = cell
            .outputs
            .iter()
            .map(|&id| *opt_to_merged.get(&id).unwrap_or(&id))
            .collect();
        let new_clock = cell
            .clock
            .map(|id| *opt_to_merged.get(&id).unwrap_or(&id));
        let new_reset = cell
            .reset
            .map(|id| *opt_to_merged.get(&id).unwrap_or(&id));

        let new_cell = Cell {
            id: CellId(0),
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
        merged.add_cell(new_cell);
    }

    // Step 4: Insert physical cells with remapped net IDs
    for cell in &partition.physical_cells {
        let new_inputs: Vec<GateNetId> = cell
            .inputs
            .iter()
            .map(|&id| *orig_to_merged.get(&id).unwrap_or(&id))
            .collect();
        let new_outputs: Vec<GateNetId> = cell
            .outputs
            .iter()
            .map(|&id| *orig_to_merged.get(&id).unwrap_or(&id))
            .collect();
        let new_clock = cell
            .clock
            .map(|id| *orig_to_merged.get(&id).unwrap_or(&id));
        let new_reset = cell
            .reset
            .map(|id| *orig_to_merged.get(&id).unwrap_or(&id));

        let new_cell = Cell {
            id: CellId(0),
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
        merged.add_cell(new_cell);
    }

    // Step 5: Set up primary inputs/outputs from the original + optimized
    // Use the optimized netlist's inputs (minus pseudo-inputs) + original primary inputs
    for &opt_id in &optimized.inputs {
        if let Some(&merged_id) = opt_to_merged.get(&opt_id) {
            // Skip pseudo-primary-inputs that were boundary inputs (driven by physical cells)
            let is_boundary = partition.boundary_inputs.iter().any(|&orig_id| {
                partition
                    .orig_to_opt
                    .get(&orig_id)
                    .is_some_and(|&oid| oid == opt_id)
            });
            if !is_boundary && !merged.inputs.contains(&merged_id) {
                merged.inputs.push(merged_id);
            }
        }
    }
    // Re-add boundary inputs only if they're actual primary inputs in the original
    for &opt_id in &optimized.inputs {
        if let Some(&merged_id) = opt_to_merged.get(&opt_id) {
            if !merged.inputs.contains(&merged_id) {
                // Check if this was an original primary input
                let is_orig_input = partition.opt_to_orig.get(&opt_id).is_some_and(|&orig_id| {
                    let orig_net = &partition.optimizable.nets[0]; // dummy
                    // Check original netlist inputs
                    // We stored original inputs through orig_to_opt
                    // A net is an original primary input if it was in the original inputs list
                    // We can check by looking at whether it's NOT a boundary_input
                    !partition.boundary_inputs.contains(&orig_id)
                });
                // If not already added and it's not a pseudo-input, skip
            }
        }
    }

    // Outputs: use original primary outputs
    for &opt_id in &optimized.outputs {
        if let Some(&merged_id) = opt_to_merged.get(&opt_id) {
            // Skip pseudo-primary-outputs that were boundary outputs
            let is_boundary = partition.boundary_outputs.iter().any(|&orig_id| {
                partition
                    .orig_to_opt
                    .get(&orig_id)
                    .is_some_and(|&oid| oid == opt_id)
            });
            if !is_boundary && !merged.outputs.contains(&merged_id) {
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

    /// Test round-trip: partition → merge preserves cell count
    #[test]
    fn test_partition_merge_roundtrip() {
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

        let partition = partition_for_aig(&netlist).expect("Should partition");
        assert_eq!(partition.optimizable.cells.len(), 2); // AND + BUF
        assert_eq!(partition.physical_cells.len(), 1); // RAM

        // Simulate AIG optimization (identity — just return optimizable as-is)
        let merged = merge_after_aig(partition.optimizable.clone(), &partition);

        // Should have all 3 cells back
        assert_eq!(merged.cells.len(), 3);
    }
}
