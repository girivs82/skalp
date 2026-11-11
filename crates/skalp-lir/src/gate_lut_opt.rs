//! LUT4 Post-Mapping Optimizer
//!
//! Three optimization passes operating directly on `GateNetlist` LUT cells:
//!
//! 1. **Buffer LUT elimination** — LUTs with identity truth tables replaced by wires
//! 2. **Constant input projection** — LUTs with constant (TIE) inputs projected to smaller truth tables
//! 3. **LUT combining** — Pairs of LUTs merged into single LUTs when total inputs fit
//!
//! These optimizations complement AIG synthesis by exploiting FPGA-specific properties
//! that the AIG framework doesn't model.

use crate::gate_netlist::{CellId, GateNetId, GateNetlist};
use std::collections::{HashMap, HashSet};

/// Statistics from LUT optimization passes.
#[derive(Debug, Default, Clone)]
pub struct LutOptStats {
    /// Buffer LUTs eliminated
    pub buffers_eliminated: usize,
    /// LUTs simplified by constant input projection
    pub constants_projected: usize,
    /// LUT pairs combined into single LUTs
    pub luts_combined: usize,
    /// LUTs eliminated entirely (became constant or trivial)
    pub luts_eliminated: usize,
}

impl std::ops::AddAssign for LutOptStats {
    fn add_assign(&mut self, rhs: Self) {
        self.buffers_eliminated += rhs.buffers_eliminated;
        self.constants_projected += rhs.constants_projected;
        self.luts_combined += rhs.luts_combined;
        self.luts_eliminated += rhs.luts_eliminated;
    }
}

/// Run all LUT optimization passes on a gate netlist.
pub fn optimize_luts(netlist: &mut GateNetlist) -> LutOptStats {
    let mut stats = LutOptStats::default();
    stats += eliminate_buffer_luts(netlist);
    stats += project_constant_inputs(netlist);

    // Iterative LUT combining: after merging A→B, the merged LUT may feed
    // another LUT C with total inputs ≤ 4, enabling further combining.
    for _ in 0..10 {
        let iter_stats = combine_lut_pairs(netlist);
        if iter_stats.luts_combined == 0 {
            break;
        }
        stats += iter_stats;
        netlist.rebuild_net_connectivity();
    }

    // Rebuild connectivity before DCE so fanout data is current.
    // Without this, remove_dead_cells() sees stale fanout=0 on nets
    // that are actually consumed (e.g. SB_IO output nets used by LUTs)
    // and incorrectly removes those cells.
    netlist.rebuild_net_connectivity();
    netlist.remove_dead_cells();
    netlist.rebuild_net_connectivity();
    stats
}

// ============================================================================
// Identity truth table patterns for LUT4 (16-bit)
// ============================================================================

/// LUT4 identity truth tables: output = input[i]
const LUT4_IDENTITY: [u64; 4] = [
    0xAAAA, // I3 (MSB) — bit pattern: 1010101010101010
    0xCCCC, // I2         bit pattern: 1100110011001100
    0xF0F0, // I1         bit pattern: 1111000011110000
    0xFF00, // I0 (LSB) — bit pattern: 1111111100000000
];

/// LUT4 inverted identity truth tables: output = !input[i]
const LUT4_INV_IDENTITY: [u64; 4] = [
    0x5555, // !I3
    0x3333, // !I2
    0x0F0F, // !I1
    0x00FF, // !I0
];

// ============================================================================
// Pass 1: Buffer LUT Elimination
// ============================================================================

/// Detect and eliminate buffer LUTs (identity truth tables).
///
/// A LUT is a buffer if its truth table is the identity function of one of its
/// inputs. Such LUTs can be replaced by directly wiring the passthrough input
/// to all the buffer's fanout.
fn eliminate_buffer_luts(netlist: &mut GateNetlist) -> LutOptStats {
    let mut stats = LutOptStats::default();

    // Find buffer LUTs: cell_id → (passthrough_input_net, is_inverted)
    let mut buffer_rewires: Vec<(CellId, GateNetId, GateNetId, bool)> = Vec::new();

    // A buffer driving a primary output from a primary input (or from another
    // primary output) must stay: eliminating it would move the output's name
    // onto that port net, so a passthrough `y = a` would rename input `a` to
    // `y` and the input port would disappear from the netlist.
    let primary_outputs: HashSet<GateNetId> = netlist.outputs.iter().copied().collect();
    let is_port = |net: GateNetId| {
        primary_outputs.contains(&net)
            || netlist
                .nets
                .get(net.0 as usize)
                .is_some_and(|n| n.is_input || n.is_output)
    };

    for cell in &netlist.cells {
        let init = match cell.lut_init {
            Some(init) if cell.inputs.len() <= 4 => init & 0xFFFF, // LUT4
            _ => continue,
        };

        // Check identity patterns
        for (i, &pattern) in LUT4_IDENTITY.iter().enumerate() {
            if init == pattern && i < cell.inputs.len() {
                // Buffer: output = input[i]
                let passthrough = cell.inputs[i];
                let output = cell.outputs[0];
                if primary_outputs.contains(&output) && is_port(passthrough) {
                    break;
                }
                buffer_rewires.push((cell.id, output, passthrough, false));
                break;
            }
        }
        // Check inverted identity — we can't eliminate these without adding an inverter,
        // so skip for now (could be enhanced later)
    }

    // Apply rewires: redirect all fanout of the buffer's output net to the passthrough net
    for (cell_id, output_net, passthrough_net, _inverted) in &buffer_rewires {
        // Find all cells that consume the buffer's output, change their input to passthrough
        for cell in &mut netlist.cells {
            if cell.id == *cell_id {
                continue; // skip the buffer itself
            }
            for input in &mut cell.inputs {
                if *input == *output_net {
                    *input = *passthrough_net;
                }
            }
        }
        // Also update primary outputs
        let mut is_primary_output = false;
        for output in &mut netlist.outputs {
            if *output == *output_net {
                *output = *passthrough_net;
                is_primary_output = true;
            }
        }
        // Transfer is_output flag and output name from old net to passthrough net
        if is_primary_output {
            let output_name = netlist
                .nets
                .get(output_net.0 as usize)
                .map(|n| n.name.clone())
                .unwrap_or_default();
            if let Some(old_net) = netlist.nets.get_mut(output_net.0 as usize) {
                old_net.is_output = false;
            }
            if let Some(new_net) = netlist.nets.get_mut(passthrough_net.0 as usize) {
                new_net.is_output = true;
                if !output_name.is_empty() {
                    new_net.name = output_name;
                }
            }
        }
        stats.buffers_eliminated += 1;
    }

    // Mark buffer cells for removal by clearing their outputs
    let buffer_ids: HashSet<CellId> = buffer_rewires.iter().map(|(id, _, _, _)| *id).collect();
    netlist.cells.retain(|c| !buffer_ids.contains(&c.id));
    netlist.rebuild_net_connectivity();

    stats
}

// ============================================================================
// Pass 2: Constant Input Projection
// ============================================================================

/// When a LUT input is driven by VCC (TIE1) or GND (TIE0), project the truth table.
///
/// For a 4-LUT with input `i` constant at value `c`:
/// - If c=0: new_tt = tt & mask_i (keep rows where input i is 0)
///   → take the even half of truth table wrt input i
/// - If c=1: new_tt = (tt >> stride_i) & mask
///   → take the odd half of truth table wrt input i
///
/// After projection, if the truth table is all-0 or all-1, eliminate the LUT.
/// If it's an identity of remaining inputs, mark for buffer elimination.
fn project_constant_inputs(netlist: &mut GateNetlist) -> LutOptStats {
    let mut stats = LutOptStats::default();

    // Build constant net map: net_id → constant value (0 or 1)
    let constant_nets = netlist.get_constant_nets();

    // Process each LUT cell
    for cell in &mut netlist.cells {
        let init = match cell.lut_init {
            Some(init) if cell.inputs.len() <= 4 => init & 0xFFFF,
            _ => continue,
        };

        // Find which inputs are constant
        let mut const_inputs: Vec<(usize, bool)> = Vec::new(); // (input_index, value)
        for (i, &net_id) in cell.inputs.iter().enumerate() {
            if let Some(&val) = constant_nets.get(&net_id) {
                const_inputs.push((i, val));
            }
        }

        if const_inputs.is_empty() {
            continue;
        }

        // Project truth table by eliminating constant inputs
        let mut projected = init;
        // Process from highest input index to lowest so indices stay valid
        let mut remaining_inputs = cell.inputs.clone();
        let mut sorted_consts = const_inputs.clone();
        sorted_consts.sort_by_key(|b| std::cmp::Reverse(b.0)); // descending

        for (input_idx, const_val) in &sorted_consts {
            projected = project_truth_table(projected, *input_idx, *const_val);
            remaining_inputs.remove(*input_idx);
        }

        // Mask to remaining input count
        let remaining_count = remaining_inputs.len();
        let mask = if remaining_count >= 4 {
            0xFFFF
        } else {
            (1u64 << (1 << remaining_count)) - 1
        };
        projected &= mask;

        // Check if the LUT became trivial
        if projected == 0 {
            // Output is always 0 — find or create a TIE0 net
            // For simplicity, just leave the LUT with updated truth table
            // The dead cell elimination will handle it if output has no fanout
            cell.lut_init = Some(0);
            cell.inputs = remaining_inputs;
            stats.constants_projected += 1;
            stats.luts_eliminated += 1;
            continue;
        }
        if projected == mask {
            // Output is always 1
            cell.lut_init = Some(mask);
            cell.inputs = remaining_inputs;
            stats.constants_projected += 1;
            stats.luts_eliminated += 1;
            continue;
        }

        // Update the cell with projected truth table and fewer inputs
        cell.lut_init = Some(projected);
        cell.inputs = remaining_inputs;
        stats.constants_projected += 1;
    }

    stats
}

/// Project a truth table by fixing one input to a constant value.
///
/// For a truth table with inputs [I0, I1, I2, I3] (I3=MSB):
/// - Input i has stride 2^i in the truth table
/// - To fix input i to `val`:
///   - Extract the bits where input i matches val
///   - Compact them into the lower half
fn project_truth_table(tt: u64, input_idx: usize, const_val: bool) -> u64 {
    let stride = 1usize << input_idx;
    let mut result = 0u64;
    let mut out_bit = 0;

    // Walk through truth table entries, keeping only those where input_idx matches const_val
    for entry in 0..16u64 {
        let input_bit = (entry >> input_idx) & 1;
        if (input_bit == 1) == const_val {
            if (tt >> entry) & 1 == 1 {
                result |= 1 << out_bit;
            }
            out_bit += 1;
        }
    }

    result
}

// ============================================================================
// Pass 3: LUT Combining (Two-LUT Merge)
// ============================================================================

/// Combine pairs of LUTs where one feeds directly into the other.
///
/// If LUT-A's single fanout is an input of LUT-B, and the combined LUT would
/// use ≤4 total unique inputs, compose the truth tables into one LUT4.
fn combine_lut_pairs(netlist: &mut GateNetlist) -> LutOptStats {
    let mut stats = LutOptStats::default();

    // Build fanout map: net_id → list of (cell_id, input_pin_index)
    let mut net_fanout: HashMap<GateNetId, Vec<(CellId, usize)>> = HashMap::new();
    for cell in &netlist.cells {
        for (pin, &net_id) in cell.inputs.iter().enumerate() {
            net_fanout.entry(net_id).or_default().push((cell.id, pin));
        }
    }

    // Build cell lookup: cell_id → index in cells vec
    let cell_index: HashMap<CellId, usize> = netlist
        .cells
        .iter()
        .enumerate()
        .map(|(i, c)| (c.id, i))
        .collect();

    // Find merge candidates
    let mut merges: Vec<(CellId, CellId, usize)> = Vec::new(); // (lut_a, lut_b, input_pin_in_b)
    let mut already_merged: HashSet<CellId> = HashSet::new();

    for cell_a in &netlist.cells {
        if cell_a.lut_init.is_none() || cell_a.outputs.is_empty() || cell_a.inputs.len() > 4 {
            continue;
        }
        if already_merged.contains(&cell_a.id) {
            continue;
        }

        let output_net = cell_a.outputs[0];

        // Check: does this output go to exactly one LUT input?
        let fanout = match net_fanout.get(&output_net) {
            Some(f) if f.len() == 1 => &f[0],
            _ => continue,
        };

        // Also ensure the net is not a primary output
        if netlist.outputs.contains(&output_net) {
            continue;
        }

        let (cell_b_id, input_pin) = *fanout;
        if already_merged.contains(&cell_b_id) {
            continue;
        }

        let cell_b_idx = match cell_index.get(&cell_b_id) {
            Some(&idx) => idx,
            None => continue,
        };
        let cell_b = &netlist.cells[cell_b_idx];

        if cell_b.lut_init.is_none() || cell_b.inputs.len() > 4 {
            continue;
        }

        // Check: can we fit all unique inputs into 4?
        let a_inputs: HashSet<GateNetId> = cell_a.inputs.iter().copied().collect();
        let b_other_inputs: HashSet<GateNetId> = cell_b
            .inputs
            .iter()
            .enumerate()
            .filter(|(i, _)| *i != input_pin)
            .map(|(_, &id)| id)
            .collect();

        let mut all_inputs: HashSet<GateNetId> = a_inputs;
        all_inputs.extend(b_other_inputs);

        if all_inputs.len() > 4 {
            continue;
        }

        merges.push((cell_a.id, cell_b_id, input_pin));
        already_merged.insert(cell_a.id);
        already_merged.insert(cell_b_id);
    }

    // Apply merges
    let mut cells_to_remove: HashSet<CellId> = HashSet::new();

    for (cell_a_id, cell_b_id, input_pin_in_b) in &merges {
        let cell_a_idx = match cell_index.get(cell_a_id) {
            Some(&idx) => idx,
            None => continue,
        };
        let cell_b_idx = match cell_index.get(cell_b_id) {
            Some(&idx) => idx,
            None => continue,
        };

        let a_init = netlist.cells[cell_a_idx].lut_init.unwrap() & 0xFFFF;
        let b_init = netlist.cells[cell_b_idx].lut_init.unwrap() & 0xFFFF;
        let a_inputs = netlist.cells[cell_a_idx].inputs.clone();
        let b_inputs = netlist.cells[cell_b_idx].inputs.clone();
        let b_outputs = netlist.cells[cell_b_idx].outputs.clone();

        // Build combined input list: all unique inputs from A + B (excluding B's connection to A)
        let mut combined_inputs: Vec<GateNetId> = Vec::new();
        let mut input_map_a: Vec<usize> = Vec::new(); // a_input[i] → combined_inputs index
        let mut input_map_b: Vec<usize> = Vec::new(); // b_input[i] → combined_inputs index (or special)

        for &net_id in &a_inputs {
            if let Some(pos) = combined_inputs.iter().position(|&n| n == net_id) {
                input_map_a.push(pos);
            } else {
                input_map_a.push(combined_inputs.len());
                combined_inputs.push(net_id);
            }
        }

        for (i, &net_id) in b_inputs.iter().enumerate() {
            if i == *input_pin_in_b {
                // This is the connection from A→B, will be composed
                input_map_b.push(usize::MAX); // sentinel
                continue;
            }
            if let Some(pos) = combined_inputs.iter().position(|&n| n == net_id) {
                input_map_b.push(pos);
            } else {
                input_map_b.push(combined_inputs.len());
                combined_inputs.push(net_id);
            }
        }

        if combined_inputs.len() > 4 || combined_inputs.is_empty() {
            // Over four inputs does not fit; none means A is a constant
            // feeding B alone, which constant projection handles instead.
            continue;
        }

        // Compose truth tables
        // For each combination of combined inputs, evaluate:
        //   a_out = a_init[a_entry] where a_entry is built from combined input bits
        //   b_out = b_init[b_entry] where b_entry uses a_out for the connected pin
        let num_combined = combined_inputs.len();
        let mut composed_tt: u64 = 0;

        for entry in 0..(1u64 << num_combined) {
            // Evaluate LUT A
            let mut a_entry: u64 = 0;
            for (a_idx, &combined_idx) in input_map_a.iter().enumerate() {
                let bit = (entry >> combined_idx) & 1;
                a_entry |= bit << a_idx;
            }
            let a_out = (a_init >> a_entry) & 1;

            // Evaluate LUT B with A's output substituted
            let mut b_entry: u64 = 0;
            for (b_idx, &combined_idx) in input_map_b.iter().enumerate() {
                let bit = if combined_idx == usize::MAX {
                    a_out // this is the connection from A
                } else {
                    (entry >> combined_idx) & 1
                };
                b_entry |= bit << b_idx;
            }
            let b_out = (b_init >> b_entry) & 1;

            if b_out == 1 {
                composed_tt |= 1 << entry;
            }
        }

        // Pad combined_inputs to 4 with the first input. The padded pins carry a live
        // signal, so the table must ignore them: repeat the num_combined-input table
        // across all 16 entries. (Leaving the upper entries 0 made e.g. a 3-input AND
        // padded with its first input read 0 whenever that input was 1.)
        while combined_inputs.len() < 4 {
            combined_inputs.push(combined_inputs[0]);
        }
        let mask = (1u64 << num_combined) - 1;
        let mut padded_tt: u64 = 0;
        for entry in 0..16u64 {
            if (composed_tt >> (entry & mask)) & 1 == 1 {
                padded_tt |= 1 << entry;
            }
        }
        let composed_tt = padded_tt;

        // Update cell B to be the combined LUT
        netlist.cells[cell_b_idx].lut_init = Some(composed_tt & 0xFFFF);
        netlist.cells[cell_b_idx].inputs = combined_inputs;
        // Outputs remain the same (B's output)

        // Mark cell A for removal
        cells_to_remove.insert(*cell_a_id);
        stats.luts_combined += 1;
    }

    netlist.cells.retain(|c| !cells_to_remove.contains(&c.id));
    netlist.rebuild_net_connectivity();

    stats
}

// ============================================================================
// Flattened-design cleanup
// ============================================================================

/// LUT clean-up on a flattened FPGA netlist, across the module boundaries
/// that per-module synthesis could not see through.
///
/// Each module is mapped on its own, so a signal crossing a boundary can
/// pick up a buffer (a child output forwarded to another child) or an
/// inverter (a complemented module output), and two LUTs on either side of
/// a boundary are never merged. Each of those is a LUT level and a routing
/// hop on the path. This pass removes the buffers, folds inverters into the
/// LUTs they feed, and merges single-fanout LUT pairs that fit one LUT.
///
/// Nets that are outputs of the design keep their drivers. NCL netlists are
/// left alone: their dual-rail encoding relies on the cells as written.
pub fn optimize_flattened_luts(netlist: &mut GateNetlist) -> LutOptStats {
    let mut stats = LutOptStats::default();
    if netlist.is_ncl {
        return stats;
    }
    stats.buffers_eliminated += remove_flat_buffers(netlist);
    stats.luts_eliminated += absorb_flat_inverters(netlist);
    netlist.rebuild_net_connectivity();
    for _ in 0..10 {
        let iter_stats = combine_lut_pairs(netlist);
        if iter_stats.luts_combined == 0 {
            break;
        }
        stats += iter_stats;
        netlist.rebuild_net_connectivity();
    }
    // Cell ids index the cell list everywhere downstream; close the gaps.
    for (index, cell) in netlist.cells.iter_mut().enumerate() {
        cell.id = CellId(index as u32);
    }
    netlist.rebuild_net_connectivity();
    netlist.update_stats();
    stats
}

fn is_single_io(cell: &crate::gate_netlist::Cell) -> bool {
    cell.inputs.len() == 1 && cell.outputs.len() == 1 && cell.clock.is_none()
}

fn is_buffer_cell(cell: &crate::gate_netlist::Cell) -> bool {
    use crate::tech_library::CellFunction;
    is_single_io(cell)
        && (cell.function == Some(CellFunction::Buf)
            || (cell.function.is_none() && cell.lut_init.map(|t| t & 0x3) == Some(0x2)))
}

fn is_inverter_cell(cell: &crate::gate_netlist::Cell) -> bool {
    use crate::tech_library::CellFunction;
    is_single_io(cell)
        && (cell.function == Some(CellFunction::Inv)
            || (cell.function.is_none() && cell.lut_init.map(|t| t & 0x3) == Some(0x1)))
}

/// Whether `cell` is a single-output LUT whose truth table can take a
/// rewritten input.
fn is_rewritable_lut(cell: &crate::gate_netlist::Cell) -> bool {
    cell.lut_variables().is_some() && cell.outputs.len() == 1 && cell.clock.is_none()
}

/// Remove buffer cells that do not drive a design output, wiring their
/// consumers (data, clock and reset pins) to the buffer's input. The removed
/// net becomes an alias of the input so lookups by name still resolve.
pub(crate) fn remove_flat_buffers(netlist: &mut GateNetlist) -> usize {
    let outputs: HashSet<GateNetId> = netlist.outputs.iter().copied().collect();
    let mut replace: HashMap<GateNetId, GateNetId> = HashMap::new();
    let mut removed: HashSet<CellId> = HashSet::new();
    for cell in &netlist.cells {
        if !is_buffer_cell(cell) {
            continue;
        }
        let (input, output) = (cell.inputs[0], cell.outputs[0]);
        if outputs.contains(&output) || input == output {
            continue;
        }
        replace.insert(output, input);
        removed.insert(cell.id);
    }
    if removed.is_empty() {
        return 0;
    }
    let resolve = |mut net: GateNetId| {
        let mut steps = 0;
        while let Some(&next) = replace.get(&net) {
            if next == net || steps > replace.len() {
                break;
            }
            net = next;
            steps += 1;
        }
        net
    };
    netlist.cells.retain(|c| !removed.contains(&c.id));
    for cell in &mut netlist.cells {
        for input in &mut cell.inputs {
            *input = resolve(*input);
        }
        cell.clock = cell.clock.map(resolve);
        cell.reset = cell.reset.map(resolve);
    }
    for &old in replace.keys() {
        let target = resolve(old);
        if let Some(net) = netlist.nets.get_mut(old.0 as usize) {
            net.alias_of = Some(target);
        }
    }
    removed.len()
}

/// Fold each inverter into the LUTs it feeds by complementing the matching
/// truth-table input. An inverter left with no consumers is removed; one
/// that still feeds a register, RAM or other non-LUT input stays for those.
fn absorb_flat_inverters(netlist: &mut GateNetlist) -> usize {
    let outputs: HashSet<GateNetId> = netlist.outputs.iter().copied().collect();
    let inverters: HashMap<GateNetId, (CellId, GateNetId)> = netlist
        .cells
        .iter()
        .filter(|c| is_inverter_cell(c))
        .map(|c| (c.outputs[0], (c.id, c.inputs[0])))
        .collect();
    if inverters.is_empty() {
        return 0;
    }
    for cell in &mut netlist.cells {
        if !is_rewritable_lut(cell) || inverters.contains_key(&cell.outputs[0]) {
            continue;
        }
        for pin in 0..cell.inputs.len() {
            if let Some(&(_, source)) = inverters.get(&cell.inputs[pin]) {
                let init = cell.lut_init.unwrap_or(0);
                let vars = cell.lut_variables().unwrap_or(4);
                cell.lut_init = Some(complement_input(init, pin, vars));
                cell.inputs[pin] = source;
            }
        }
    }
    let mut used: HashSet<GateNetId> = outputs;
    for cell in &netlist.cells {
        used.extend(cell.inputs.iter().copied());
        used.extend(cell.clock);
        used.extend(cell.reset);
    }
    let dead: HashSet<CellId> = inverters
        .iter()
        .filter(|(out, _)| !used.contains(out))
        .map(|(_, &(id, _))| id)
        .collect();
    netlist.cells.retain(|c| !dead.contains(&c.id));
    dead.len()
}

/// `tt`, a truth table over `vars` inputs, with input `pin` complemented.
fn complement_input(tt: u64, pin: usize, vars: usize) -> u64 {
    (0..1usize << vars)
        .filter(|&i| (tt >> (i ^ (1 << pin))) & 1 == 1)
        .fold(0u64, |acc, i| acc | (1 << i))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::gate_netlist::{Cell, CellId, GateNet, GateNetId, GateNetlist};
    use crate::tech_library::CellFunction;

    fn make_test_netlist() -> GateNetlist {
        GateNetlist::new("test".to_string(), "fpga".to_string())
    }

    #[test]
    fn test_passthrough_buffer_keeps_input_port() {
        // y = a through an identity LUT: eliminating it would rename input
        // `a` to `y`, so the buffer must stay and both ports keep their nets.
        let mut netlist = make_test_netlist();
        let a = netlist.add_input("a".to_string());
        let y = netlist.add_output("y".to_string());
        netlist.add_cell(Cell::new_lut(
            CellId(0),
            "SB_LUT4".to_string(),
            "fpga".to_string(),
            0.1,
            "buf".to_string(),
            vec![a, a, a, a],
            vec![y],
            0xAAAA,
        ));

        let stats = eliminate_buffer_luts(&mut netlist);
        assert_eq!(stats.buffers_eliminated, 0);
        assert_eq!(netlist.nets[a.0 as usize].name, "a");
        assert_eq!(netlist.nets[y.0 as usize].name, "y");
        assert_eq!(netlist.outputs, vec![y]);
        assert_eq!(netlist.cells.len(), 1);
    }

    #[test]
    fn test_buffer_lut_elimination() {
        let mut netlist = make_test_netlist();
        let a = netlist.add_input("a".to_string());
        let b = netlist.add_input("b".to_string());
        let c = netlist.add_input("c".to_string());
        let d = netlist.add_input("d".to_string());
        let buf_out = netlist.add_net(GateNet::new(GateNetId(0), "buf_out".to_string()));
        let y = netlist.add_output("y".to_string());

        // Buffer LUT: output = I3 (truth table 0xAAAA)
        let buf_cell = Cell::new_lut(
            CellId(0),
            "SB_LUT4".to_string(),
            "fpga".to_string(),
            0.1,
            "buf".to_string(),
            vec![a, b, c, d],
            vec![buf_out],
            0xAAAA,
        );
        netlist.add_cell(buf_cell);

        // Consumer LUT: uses buf_out
        let consumer = Cell::new_lut(
            CellId(0),
            "SB_LUT4".to_string(),
            "fpga".to_string(),
            0.1,
            "consumer".to_string(),
            vec![buf_out, b, c, d],
            vec![y],
            0x8000, // AND4
        );
        netlist.add_cell(consumer);

        let stats = eliminate_buffer_luts(&mut netlist);
        assert_eq!(stats.buffers_eliminated, 1);
        // Buffer cell should be removed
        assert_eq!(netlist.cells.len(), 1);
        // Consumer should now use 'a' (I3 of the buffer) instead of buf_out
        assert_eq!(netlist.cells[0].inputs[0], a); // was buf_out, now 'a'
    }

    #[test]
    fn test_identity_patterns() {
        // Verify identity truth tables
        assert_eq!(LUT4_IDENTITY[0], 0xAAAA); // I3
        assert_eq!(LUT4_IDENTITY[1], 0xCCCC); // I2
        assert_eq!(LUT4_IDENTITY[2], 0xF0F0); // I1
        assert_eq!(LUT4_IDENTITY[3], 0xFF00); // I0
    }

    #[test]
    fn test_project_truth_table() {
        // AND2 truth table (I1 & I0): 1000 = 0x8
        // Fix I0=1 (input_idx=0, val=true): should give I1's identity = 0b10 = 0x2
        let tt = 0x8u64; // 2-input AND: entries [00=0, 01=0, 10=0, 11=1]
        let projected = project_truth_table(tt, 0, true);
        // When I0=1, entries are: I1=0→entry[01]=0, I1=1→entry[11]=1
        // Result: 0b10 = 2
        assert_eq!(projected, 0x2);

        // Fix I0=0: should give constant 0
        let projected_0 = project_truth_table(tt, 0, false);
        // When I0=0, entries are: I1=0→entry[00]=0, I1=1→entry[10]=0
        assert_eq!(projected_0, 0x0);
    }

    #[test]
    fn test_constant_input_projection() {
        let mut netlist = make_test_netlist();
        let a = netlist.add_input("a".to_string());
        let b = netlist.add_input("b".to_string());
        let y = netlist.add_output("y".to_string());

        // Create a TIE1 cell driving a constant net
        let const1_net = netlist.add_net(GateNet::new(GateNetId(0), "const1".to_string()));
        let tie_cell = Cell::new_comb(
            CellId(0),
            "TIE1_X1".to_string(),
            "fpga".to_string(),
            0.0,
            "tie1".to_string(),
            vec![],
            vec![const1_net],
        );
        netlist.add_cell(tie_cell);

        // Unused input for padding
        let unused = netlist.add_input("unused".to_string());

        // LUT4: AND of I3 and I2, with I1=const1, I0=unused
        // AND(I3, I2) = 0x8888 (for 4-input, but we only care about I3&I2)
        // Actually: AND4 all inputs = 0x8000
        // Let's use: I3 AND I2 = truth table where bits 12,13,14,15 are copies of "I3&I2" pattern
        // For I3 & I2: tt = 0x8 repeated = 0x8888
        // With I1=1 (const), we project: keep entries where I1=1
        let lut = Cell::new_lut(
            CellId(0),
            "SB_LUT4".to_string(),
            "fpga".to_string(),
            0.1,
            "lut".to_string(),
            vec![a, b, const1_net, unused],
            vec![y],
            0x8888, // I3 AND I2 (ignoring I1, I0)
        );
        netlist.add_cell(lut);

        let stats = project_constant_inputs(&mut netlist);
        assert_eq!(stats.constants_projected, 1);
        // The LUT should now have fewer inputs
        let lut_cell = netlist.cells.iter().find(|c| c.lut_init.is_some()).unwrap();
        assert!(lut_cell.inputs.len() < 4);
    }

    #[test]
    fn test_lut_combining() {
        let mut netlist = make_test_netlist();
        let a = netlist.add_input("a".to_string());
        let b = netlist.add_input("b".to_string());
        let c = netlist.add_input("c".to_string());
        let mid = netlist.add_net(GateNet::new(GateNetId(0), "mid".to_string()));
        let y = netlist.add_output("y".to_string());

        // LUT A: AND(I3, I2) = a AND b → mid
        // 2-input truth table for AND: 0x8
        // In LUT4 encoding with 2 inputs at I3, I2: 0x8888 but let's use proper 2-input:
        let lut_a = Cell::new_lut(
            CellId(0),
            "SB_LUT4".to_string(),
            "fpga".to_string(),
            0.1,
            "lut_a".to_string(),
            vec![a, b],
            vec![mid],
            0x8, // 2-input AND
        );
        netlist.add_cell(lut_a);

        // LUT B: OR(mid, c) → y
        // 2-input OR truth table: 0xE
        let lut_b = Cell::new_lut(
            CellId(0),
            "SB_LUT4".to_string(),
            "fpga".to_string(),
            0.1,
            "lut_b".to_string(),
            vec![mid, c],
            vec![y],
            0xE, // 2-input OR
        );
        netlist.add_cell(lut_b);

        netlist.rebuild_net_connectivity();
        let stats = combine_lut_pairs(&mut netlist);
        assert_eq!(stats.luts_combined, 1);
        // Should have 1 cell left (LUT B with composed truth table)
        assert_eq!(netlist.cells.len(), 1);
        // Combined: (a AND b) OR c — 3-input truth table
        // a=I0, b=I1, c=I2 (or similar ordering)
        // Should produce correct composed result
        let combined = netlist.cells[0].lut_init.unwrap();
        // Verify: for all 8 combinations of (a,b,c), (a&b)|c should match
        for entry in 0..8u64 {
            let a_val = entry & 1;
            let b_val = (entry >> 1) & 1;
            let c_val = (entry >> 2) & 1;
            let expected = (a_val & b_val) | c_val;
            let actual = (combined >> entry) & 1;
            assert_eq!(
                actual, expected,
                "Mismatch at entry {}: a={}, b={}, c={}, expected={}, got={}",
                entry, a_val, b_val, c_val, expected, actual
            );
        }
    }

    #[test]
    fn test_optimize_luts_full() {
        // An identity LUT between internal logic and an output port goes away;
        // the output name moves onto the logic's net. (A port-to-port
        // passthrough stays: see test_passthrough_buffer_keeps_input_port.)
        let mut netlist = make_test_netlist();
        let a = netlist.add_input("a".to_string());
        let b = netlist.add_input("b".to_string());
        let n = netlist.add_net(GateNet::new(GateNetId(0), "n".to_string()));
        let y = netlist.add_output("y".to_string());

        netlist.add_cell(Cell::new_lut(
            CellId(0),
            "SB_LUT4".to_string(),
            "fpga".to_string(),
            0.1,
            "and".to_string(),
            vec![a, b, a, a],
            vec![n],
            0x8888, // I0 & I1
        ));
        netlist.add_cell(Cell::new_lut(
            CellId(0),
            "SB_LUT4".to_string(),
            "fpga".to_string(),
            0.1,
            "buf".to_string(),
            vec![n, n, n, n],
            vec![y],
            0xAAAA, // identity of I0
        ));

        let stats = optimize_luts(&mut netlist);
        assert_eq!(stats.buffers_eliminated, 1);
        assert_eq!(netlist.cells.len(), 1);
        assert_eq!(netlist.outputs, vec![n]);
        assert_eq!(netlist.nets[n.0 as usize].name, "y");
        assert_eq!(netlist.nets[a.0 as usize].name, "a");
    }

    fn ff(netlist: &mut GateNetlist, clk: GateNetId, d: GateNetId, q: GateNetId) {
        let mut cell = Cell::new_comb(
            CellId(0),
            "FF".to_string(),
            "fpga".to_string(),
            0.1,
            "ff".to_string(),
            vec![d],
            vec![q],
        );
        cell.clock = Some(clk);
        netlist.add_cell(cell);
    }

    fn gate(netlist: &mut GateNetlist, function: CellFunction, a: GateNetId, y: GateNetId) {
        netlist.add_cell(
            Cell::new_comb(
                CellId(0),
                format!("{:?}", function),
                "fpga".to_string(),
                0.1,
                "gate".to_string(),
                vec![a],
                vec![y],
            )
            .with_function(function),
        );
    }

    fn eval(netlist: &GateNetlist, values: &HashMap<GateNetId, bool>, net: GateNetId) -> bool {
        if let Some(&v) = values.get(&net) {
            return v;
        }
        let cell = netlist
            .cells
            .iter()
            .find(|c| c.outputs.contains(&net))
            .expect("driven net");
        let bits: Vec<bool> = cell
            .inputs
            .iter()
            .map(|&i| eval(netlist, values, i))
            .collect();
        match (cell.lut_init, cell.function.as_ref()) {
            (Some(init), _) => {
                let index = bits
                    .iter()
                    .enumerate()
                    .fold(0, |acc, (i, &b)| acc | ((b as usize) << i));
                (init >> index) & 1 == 1
            }
            (None, Some(CellFunction::Inv)) => !bits[0],
            (None, Some(CellFunction::Buf)) => bits[0],
            _ => panic!("cannot evaluate {}", cell.cell_type),
        }
    }

    /// q0 -> buf -> (module boundary) -> inv -> lut(and with q1) -> y, and
    /// the inverter also feeds a register.
    #[test]
    fn flattened_buffers_go_and_inverters_fold_into_luts() {
        let mut netlist = make_test_netlist();
        let clk = netlist.add_input("clk".to_string());
        let q0 = netlist.add_net(GateNet::new(GateNetId(0), "q0".to_string()));
        let q1 = netlist.add_net(GateNet::new(GateNetId(0), "q1".to_string()));
        let fwd = netlist.add_net(GateNet::new(GateNetId(0), "fwd".to_string()));
        let inv = netlist.add_net(GateNet::new(GateNetId(0), "inv".to_string()));
        let r = netlist.add_net(GateNet::new(GateNetId(0), "r".to_string()));
        let y = netlist.add_output("y".to_string());
        let d0 = netlist.add_input("d0".to_string());
        let d1 = netlist.add_input("d1".to_string());
        ff(&mut netlist, clk, d0, q0);
        ff(&mut netlist, clk, d1, q1);
        gate(&mut netlist, CellFunction::Buf, q0, fwd);
        gate(&mut netlist, CellFunction::Inv, fwd, inv);
        ff(&mut netlist, clk, inv, r);
        netlist.add_cell(Cell::new_lut(
            CellId(0),
            "LUT4".to_string(),
            "fpga".to_string(),
            0.1,
            "and".to_string(),
            vec![inv, q1, inv, inv],
            vec![y],
            0x8888, // I0 & I1, padded with I0
        ));
        let before = netlist.clone();

        let stats = optimize_flattened_luts(&mut netlist);
        assert_eq!(stats.buffers_eliminated, 1);
        assert_eq!(
            stats.luts_eliminated, 0,
            "the register still needs the inverter"
        );
        let lut = netlist.cells.iter().find(|c| c.path == "and").unwrap();
        assert_eq!(lut.inputs, vec![q0, q1, q0, q0]);
        assert_eq!(netlist.nets[fwd.0 as usize].alias_of, Some(q0));
        let inverter = netlist
            .cells
            .iter()
            .find(|c| c.function == Some(CellFunction::Inv))
            .expect("inverter kept for the register");
        assert_eq!(inverter.inputs, vec![q0]);
        assert!(
            netlist
                .cells
                .iter()
                .enumerate()
                .all(|(i, c)| c.id.0 as usize == i),
            "cell ids index the cell list"
        );

        for bits in 0..4u8 {
            let values: HashMap<GateNetId, bool> =
                HashMap::from_iter([(q0, bits & 1 == 1), (q1, bits & 2 == 2)]);
            for net in [y, inv] {
                assert_eq!(
                    eval(&netlist, &values, net),
                    eval(&before, &values, net),
                    "net {:?} with q0={} q1={}",
                    net,
                    bits & 1,
                    bits >> 1
                );
            }
        }
    }

    #[test]
    fn flattened_inverter_feeding_only_luts_is_removed() {
        let mut netlist = make_test_netlist();
        let a = netlist.add_input("a".to_string());
        let b = netlist.add_input("b".to_string());
        let na = netlist.add_net(GateNet::new(GateNetId(0), "na".to_string()));
        let y = netlist.add_output("y".to_string());
        let z = netlist.add_output("z".to_string());
        gate(&mut netlist, CellFunction::Inv, a, na);
        for (name, out, init) in [("xor", y, 0x6666u64), ("or", z, 0xEEEE)] {
            netlist.add_cell(Cell::new_lut(
                CellId(0),
                "LUT4".to_string(),
                "fpga".to_string(),
                0.1,
                name.to_string(),
                vec![na, b, na, na],
                vec![out],
                init,
            ));
        }
        let before = netlist.clone();

        let stats = optimize_flattened_luts(&mut netlist);
        assert_eq!(stats.luts_eliminated, 1);
        assert!(netlist
            .cells
            .iter()
            .all(|c| c.function != Some(CellFunction::Inv)));
        for bits in 0..4u8 {
            let values = HashMap::from_iter([(a, bits & 1 == 1), (b, bits & 2 == 2)]);
            for net in [y, z] {
                assert_eq!(eval(&netlist, &values, net), eval(&before, &values, net));
            }
        }
    }

    #[test]
    fn flattened_buffer_driving_a_design_output_stays() {
        let mut netlist = make_test_netlist();
        let clk = netlist.add_input("clk".to_string());
        let d = netlist.add_input("d".to_string());
        let q = netlist.add_net(GateNet::new(GateNetId(0), "q".to_string()));
        let y = netlist.add_output("y".to_string());
        ff(&mut netlist, clk, d, q);
        gate(&mut netlist, CellFunction::Buf, q, y);

        let stats = optimize_flattened_luts(&mut netlist);
        assert_eq!(stats.buffers_eliminated, 0);
        assert_eq!(netlist.cells.len(), 2);
    }
}
