//! Synthesis
//!
//! Synthesizes word-level operations (Lir) to gate-level primitives (GateNetlist)
//! using AIG-based optimization and technology mapping.
//!
//! # Flow
//!
//! ```text
//! Lir → AIG → SynthEngine (optimize) → TechMap → GateNetlist
//! ```
//!
//! Post-processing: insert_clock_buffers, insert_io_buffers, BRAM/DSP inference.

use crate::gate_netlist::{
    Cell, CellFailureMode, CellId, CellSafetyClassification, GateNet, GateNetId, GateNetlist,
    DSP_A_BITS_PARAM, DSP_B_BITS_PARAM, DSP_C_BITS, DSP_P_BITS_PARAM, RAM_ADDR_BITS_PARAM,
    RAM_DATA_BITS_PARAM,
};
use crate::lir::{Lir, LirNodeId, LirOp, LirSignalId};
use crate::tech_library::{CellFunction, LibraryCell, LibraryFailureMode, TechLibrary};
use indexmap::IndexMap;
use skalp_frontend::hir::PhysicalConstraints;

/// Information extracted from a library cell for tech mapping
struct LibraryCellInfo {
    name: String,
    function: CellFunction,
    fit: f64,
    failure_modes: Vec<CellFailureMode>,
    /// LUT initialization value from the library (technology-specific encoding).
    /// Set when the library cell has a `lut_init` field — the mapper uses this
    /// instead of hardcoding INIT values.
    lut_init: Option<u64>,
}

impl LibraryCellInfo {
    /// Create from a library cell reference
    fn from_library_cell(cell: &LibraryCell) -> Self {
        Self {
            name: cell.name.clone(),
            function: cell.function.clone(),
            fit: cell.fit,
            failure_modes: cell
                .failure_modes
                .iter()
                .map(convert_failure_mode)
                .collect(),
            lut_init: cell.lut_init,
        }
    }

    /// Apply library cell metadata to a gate cell.
    ///
    /// Sets function, failure_modes, and lut_init from library info.
    fn apply_to_cell(&self, cell: &mut Cell) {
        cell.function = Some(self.function.clone());
        cell.failure_modes = self.failure_modes.clone();
        // Propagate lut_init from library — only if the cell doesn't already have one
        // (explicit overrides from the mapper take precedence)
        if cell.lut_init.is_none() {
            cell.lut_init = self.lut_init;
        }
    }
}

/// Input order for a MUX2 cell computing `sel ? d1 : d0`. A generic MUX2 takes
/// `[sel, d0, d1]`; an FPGA MUX2 is a LUT whose library INIT (0xCACA) reads
/// `[d0, d1, sel]`, so passing the generic order to it computes a different function.
fn mux2_inputs(
    ci: &LibraryCellInfo,
    sel: GateNetId,
    d0: GateNetId,
    d1: GateNetId,
) -> Vec<GateNetId> {
    if ci.lut_init.is_some() {
        vec![d0, d1, sel]
    } else {
        vec![sel, d0, d1]
    }
}

/// Look up a library cell by function, falling back to a default if not found.
fn lookup_cell(
    library: &TechLibrary,
    func: CellFunction,
    fallback_name: &str,
    fallback_fit: f64,
) -> LibraryCellInfo {
    match library.find_best_cell(&func) {
        Some(cell) => LibraryCellInfo::from_library_cell(cell),
        None => LibraryCellInfo {
            name: fallback_name.to_string(),
            function: func,
            fit: fallback_fit,
            failure_modes: vec![],
            lut_init: None,
        },
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

/// Ceiling log2
fn clog2(n: u32) -> u32 {
    if n <= 1 {
        return 0;
    }
    32 - (n - 1).leading_zeros()
}

/// Select the best BRAM aspect ratio from available options.
///
/// Prefers single-block fit. If no single-block fit, minimizes total block count.
fn select_best_aspect_ratio(
    aspect_ratios: &[(u32, u32)],
    data_width: u32,
    depth: u32,
) -> (u32, u32) {
    // Try to find a single-block fit
    let mut best_single: Option<(u32, u32, u32)> = None; // (depth, width, waste)
    for &(bd, bw) in aspect_ratios {
        if bw >= data_width && bd >= depth {
            let waste = (bw - data_width) * bd + (bd - depth) * bw;
            if best_single.is_none() || waste < best_single.unwrap().2 {
                best_single = Some((bd, bw, waste));
            }
        }
    }
    if let Some((bd, bw, _)) = best_single {
        return (bd, bw);
    }

    // No single-block fit — minimize total block count
    let mut best: (u32, u32) = aspect_ratios[0];
    let mut best_count = u32::MAX;
    for &(bd, bw) in aspect_ratios {
        let w_blocks = data_width.div_ceil(bw);
        let d_blocks = depth.div_ceil(bd);
        let total = w_blocks * d_blocks;
        if total < best_count {
            best_count = total;
            best = (bd, bw);
        }
    }
    best
}

/// Full synthesis: LIR → tech mapping → AIG optimization → technology mapping
///
/// This function provides a complete synthesis flow that includes:
/// 1. Initial technology mapping (LIR to gate netlist)
/// 2. AIG-based optimization passes (rewrite, refactor, balance, FRAIG, etc.)
/// 3. Library-aware technology mapping with cut enumeration
///
/// The optimization level is automatically configured based on the library:
/// - FPGA libraries: uses lut_size for K-feasible cuts
/// - ASIC libraries: uses default cut size for more optimization freedom
///
/// # Arguments
/// * `lir` - The LIR (Low-level IR) to synthesize
/// * `library` - Target technology library
/// * `preset` - Synthesis preset controlling optimization effort
///
/// # Example
/// ```ignore
/// use skalp_lir::{synthesize, SynthPreset};
///
/// let result = synthesize(&lir, &library, SynthPreset::Balanced);
/// println!("Cells after optimization: {}", result.netlist.cells.len());
/// ```
pub fn synthesize(
    lir: &Lir,
    library: &TechLibrary,
    preset: crate::synth::SynthPreset,
) -> crate::synth::SynthResult {
    use crate::synth::{LirToSynthAig, SynthEngine};

    let absorbed = absorb_block_ram_read_registers(lir, library);
    let lir = absorbed.as_ref().unwrap_or(lir);
    let banked = split_distram_banks(lir, library);
    let lir = banked.as_ref().unwrap_or(lir);

    // Step 1: Convert LIR directly to AIG (bypasses intermediate GateNetlist).
    // Physical ops (NCL, BRAM, DSP) become pseudo-inputs in the AIG so that
    // combinational logic referencing their outputs is preserved.
    let dsp_max_width = library
        .find_dsp_cell()
        .map(|(_, info)| info.a_width.min(info.b_width))
        .unwrap_or(0);
    // Adders, subtractors and comparators go on the carry chain when the
    // target has one (Lattice CCU2); otherwise they are AIG logic.
    let carry_chain_min_width = library
        .find_best_cell(&CellFunction::Ccu2)
        .map(|_| crate::carry_chain::CARRY_CHAIN_MIN_WIDTH);
    let converter = LirToSynthAig::new(lir)
        .with_dsp_max_width(dsp_max_width)
        .with_carry_chain_min_width(carry_chain_min_width);
    let lir_to_aig_result = converter.build();

    // Step 2: Run synthesis engine on the AIG
    let mut engine = SynthEngine::with_preset(preset);
    let mut result = engine.optimize_from_aig(lir_to_aig_result.aig, library);

    // Step 3: Merge physical LIR nodes (NCL, BRAM) into the AIG-derived netlist.
    // Must happen BEFORE buffer removal so that __phys_* pseudo-input nets are
    // properly driven by physical cells before the optimizer merges nets.
    // Rebuild net_map first: the AIG writer renames output nets in create_outputs()
    // without updating net_map, leaving stale name→ID mappings.
    result.netlist.rebuild_cache();
    if !lir_to_aig_result.physical_node_indices.is_empty() {
        merge_physical_nodes_into_netlist(
            &mut result.netlist,
            lir,
            &lir_to_aig_result.physical_node_indices,
            library,
        );
    }

    // Step 3.5: Ensure all LIR output signals have output nets in the netlist.
    // Physical inputs now use actual names (is_physical flag replaces __phys_ prefix),
    // so we just check if the output net exists and create it if not.
    if lir.is_ncl {
        for &output_id in &lir.outputs {
            let signal = &lir.signals[output_id.0 as usize];
            for bit in 0..signal.width {
                let name = if signal.width == 1 {
                    signal.name.clone()
                } else {
                    format!("{}[{}]", signal.name, bit)
                };
                if let Some(net_id) = result.netlist.get_net_id(&name) {
                    // Net exists — ensure it's marked as output
                    if let Some(net) = result.netlist.get_net_mut(net_id) {
                        if !net.is_output {
                            net.is_output = true;
                            result.netlist.outputs.push(net_id);
                        }
                    }
                } else {
                    // Create as output
                    let net_id = result
                        .netlist
                        .add_net(crate::gate_netlist::GateNet::new(GateNetId(0), name));
                    if let Some(net) = result.netlist.get_net_mut(net_id) {
                        net.is_output = true;
                    }
                    result.netlist.outputs.push(net_id);
                }
            }
        }
        result.netlist.rebuild_cache();
    }

    // Step 3.7 (AUDIT-2 safety): apply the MODULE-level safety
    // classification to cells. #[safety_mechanism]-annotated entities carry
    // LirSafetyInfo on the LIR, but the AIG synthesis path never consumed
    // it — every cell stayed Functional and FMEA/PMHF analyses saw no
    // safety-mechanism hardware at all. Cells that already carry a more
    // specific classification (from per-signal annotations) are preserved.
    if let Some(ref info) = lir.module_safety_info {
        if info.mechanism_name.is_some() || info.is_sm_of_sm {
            let mechanism_name = info
                .mechanism_name
                .clone()
                .unwrap_or_else(|| "unnamed".to_string());
            let goal_name = info
                .goal_name
                .clone()
                .unwrap_or_else(|| "unassigned".to_string());
            let classification = if info.is_sm_of_sm {
                crate::gate_netlist::CellSafetyClassification::SafetyMechanismOfSm {
                    protected_sm_name: info
                        .protected_sm_name
                        .clone()
                        .unwrap_or_else(|| "unknown".to_string()),
                    goal_name,
                    mechanism_name,
                }
            } else {
                crate::gate_netlist::CellSafetyClassification::SafetyMechanism {
                    goal_name,
                    mechanism_name,
                }
            };
            for cell in &mut result.netlist.cells {
                if matches!(
                    cell.safety_classification,
                    crate::gate_netlist::CellSafetyClassification::Functional
                ) {
                    cell.safety_classification = classification.clone();
                }
            }
        }
    }

    // Step 4: LUT post-mapping optimization for FPGA targets
    if library.is_fpga() {
        crate::gate_lut_opt::optimize_luts(&mut result.netlist);
    }

    // Step 5: Buffer removal and cleanup
    {
        let mut gate_opt = crate::gate_optimizer::GateOptimizer::new();
        gate_opt.set_enable_constant_folding(false);
        gate_opt.set_enable_dce(false);
        gate_opt.set_enable_boolean_simp(false);
        gate_opt.set_enable_mux_opt(false);
        gate_opt.set_enable_buffer_removal(true);
        gate_opt.optimize(&mut result.netlist);
    }

    // Preserve NCL flag
    result.netlist.is_ncl = lir.is_ncl;

    // Step 5.5: Preserve input AND output port names for hierarchical stitching.
    // The AIG optimizer may inline input signals into LUT truth tables or
    // merge output names away. Re-create them so the flattener can stitch
    // parent↔child connections by port name.
    //
    // Direction matters: output-port nets must be preserved AS OUTPUTS. The old
    // code marked every preserved port net is_input, which dual-flagged all
    // driven outputs; downstream the gate→SIR converter classifies is_input
    // first, so every output port surfaced as an *input* and equivalence
    // checking found no outputs to compare.
    for (&port_id, port_is_output) in lir
        .inputs
        .iter()
        .map(|p| (p, false))
        .chain(lir.outputs.iter().map(|p| (p, true)))
    {
        let signal = &lir.signals[port_id.0 as usize];
        for bit in 0..signal.width {
            let name = if signal.width == 1 {
                signal.name.clone()
            } else {
                format!("{}[{}]", signal.name, bit)
            };
            let net_id = match result.netlist.get_net_id(&name) {
                Some(id) => id,
                None => result.netlist.add_net_with_name(name),
            };
            if let Some(net) = result.netlist.nets.get_mut(net_id.0 as usize) {
                if port_is_output {
                    if !net.is_output {
                        net.is_output = true;
                        result.netlist.outputs.push(net_id);
                    }
                } else if !net.is_input {
                    net.is_input = true;
                    result.netlist.inputs.push(net_id);
                }
            }
        }
    }

    // Step 5.6: Stamp #[detection_signal] metadata onto gate nets. The AIG
    // path creates fresh nets from AIG outputs, so LIR-level detection flags
    // (ports via add_detection_output, internals via mark_as_detection) never
    // reach the netlist — safety analyses were falling back to the deprecated
    // name heuristic. Mark by signal name, per bit, following aliases.
    for signal in lir.signals.iter().filter(|s| s.is_detection) {
        for bit in 0..signal.width {
            let name = if signal.width == 1 {
                signal.name.clone()
            } else {
                format!("{}[{}]", signal.name, bit)
            };
            if let Some(net_id) = result.netlist.get_net_id(&name) {
                let canonical = result.netlist.resolve_alias(net_id);
                for id in [net_id, canonical] {
                    if let Some(net) = result.netlist.nets.get_mut(id.0 as usize) {
                        net.is_detection = true;
                    }
                }
            }
        }
    }
    result.netlist.rebuild_cache();

    // The AIG exported physical-node inputs as outputs only to keep their
    // logic alive; now that the physical cells read them they are ordinary
    // internal nets, not module ports. This runs last, after the cleanup
    // passes above, which treat output nets as fixed points: dropping the
    // exports before buffer removal broke NCL gate-level simulation (an
    // async FP32 adder stopped converging) (left as outputs, every carry-chain
    // operand bit would become a top-level output and an I/O cell in PnR).
    if !lir_to_aig_result.internal_exports.is_empty() {
        let internal: std::collections::HashSet<&str> = lir_to_aig_result
            .internal_exports
            .iter()
            .map(|s| s.as_str())
            .collect();
        let nets = &result.netlist.nets;
        result.netlist.outputs.retain(|id| {
            nets.get(id.0 as usize)
                .is_none_or(|n| !internal.contains(n.name.as_str()))
        });
        // The writer gave an export that shares its literal with an output
        // (or another export) its own net through a buffer. With the export
        // no longer an output, that buffer is only a LUT level and a routing
        // hop between a register and its loads.
        if !result.netlist.is_ncl {
            crate::gate_lut_opt::remove_flat_buffers(&mut result.netlist);
            result.netlist.rebuild_net_connectivity();
        }
    }

    // Sync is_output flags with netlist.outputs — post-processing passes may
    // have moved output references without updating the per-net flag.
    result.netlist.sync_output_flags();

    // For NCL designs: buffer removal may have merged __phys_* pseudo-input nets
    // with output nets, leaving nets marked as both input and output. Remove
    // output nets from the input list — they're driven by physical cells, not external.
    if lir.is_ncl {
        // Remove output nets from input list — physical cells now drive them.
        let output_set: std::collections::HashSet<GateNetId> =
            result.netlist.outputs.iter().copied().collect();
        result.netlist.inputs.retain(|id| !output_set.contains(id));
        // Clear is_input on output nets that were removed from inputs
        for net in &mut result.netlist.nets {
            if net.is_output && net.is_input {
                net.is_input = false;
            }
        }
    }

    // Step 6: Propagate NCL metadata from LIR signals to gate nets.
    // Build a map from net name patterns to NclSignalKind, then stamp matching nets.
    // This replaces fragile name-suffix conventions with structured metadata.
    if lir.is_ncl {
        use crate::gate_netlist::{GateNetNclInfo, GateNetNclKind};
        // Build map: for each LIR signal with ncl_info, map all its bit-indexed
        // net names to the corresponding GateNetNclInfo.
        let mut ncl_net_map: std::collections::HashMap<String, GateNetNclInfo> =
            std::collections::HashMap::new();
        for signal in &lir.signals {
            if let Some(ref ncl_kind) = signal.ncl_info {
                let (gate_kind, origin_port) = match ncl_kind {
                    crate::lir::NclSignalKind::TrueRail { origin_port } => {
                        (GateNetNclKind::TrueRail, origin_port.clone())
                    }
                    crate::lir::NclSignalKind::FalseRail { origin_port } => {
                        (GateNetNclKind::FalseRail, origin_port.clone())
                    }
                    crate::lir::NclSignalKind::Decoded { origin_port } => {
                        (GateNetNclKind::Decoded, origin_port.clone())
                    }
                    crate::lir::NclSignalKind::SingleRailSource { origin_port } => {
                        (GateNetNclKind::SingleRailSource, origin_port.clone())
                    }
                };
                for bit in 0..signal.width {
                    let net_name = if signal.width == 1 {
                        signal.name.clone()
                    } else {
                        format!("{}[{}]", signal.name, bit)
                    };
                    ncl_net_map.insert(
                        net_name,
                        GateNetNclInfo {
                            kind: gate_kind,
                            origin_port: origin_port.clone(),
                            bit_index: bit as usize,
                        },
                    );
                }
            }
        }
        // Stamp matching nets
        for net in &mut result.netlist.nets {
            if let Some(info) = ncl_net_map.get(&net.name) {
                net.ncl_info = Some(info.clone());
            }
        }
    }

    result
}

/// Insert clock buffers into a synthesized netlist.
///
/// For each clock net, inserts a clock buffer cell (e.g., SB_GB on iCE40, DCCA on ECP5).
/// If the library has no clock buffer cell, this is a no-op.
pub fn insert_clock_buffers(netlist: &mut GateNetlist, library: &TechLibrary) {
    let (clk_buf_cell, clk_buf_info) = match library.find_clk_buf_cell() {
        Some(pair) => pair,
        None => return,
    };

    let cell_name = clk_buf_cell.name.clone();
    let cell_fit = clk_buf_cell.fit;
    let cell_info = LibraryCellInfo::from_library_cell(clk_buf_cell);
    let has_enable = clk_buf_info.has_enable;

    let clock_nets: Vec<GateNetId> = netlist.clocks.clone();

    for clock_net_id in clock_nets {
        let clock_name = netlist
            .nets
            .get(clock_net_id.0 as usize)
            .map(|n| n.name.clone())
            .unwrap_or_else(|| format!("clk_{}", clock_net_id.0));

        let buf_out_id = GateNetId(netlist.nets.len() as u32);
        let mut buf_out_net = GateNet::new(buf_out_id, format!("{}_gbuf", clock_name));
        buf_out_net.is_clock = true;
        netlist.add_net(buf_out_net);

        let mut cell_inputs = vec![clock_net_id];
        if has_enable {
            // Create or find tie-high net
            let tie_high = get_or_create_tie_high(netlist, library);
            cell_inputs.push(tie_high);
        }

        let mut cell = Cell::new_comb(
            CellId(0),
            cell_name.clone(),
            library.name.clone(),
            cell_fit,
            format!("{}_gbuf", clock_name),
            cell_inputs,
            vec![buf_out_id],
        );
        cell.source_op = Some("ClockBuffer".to_string());
        cell_info.apply_to_cell(&mut cell);
        netlist.add_cell(cell);

        // Rewire all clock consumers
        rewire_consumers(netlist, clock_net_id, buf_out_id);
    }
}

/// Insert IO buffers into a synthesized netlist.
///
/// For each primary input (excluding clocks), inserts an input pad cell.
/// For each primary output, inserts an output pad cell.
/// If the library has no IO cells, this is a no-op.
pub fn insert_io_buffers(
    netlist: &mut GateNetlist,
    library: &TechLibrary,
    port_constraints: &IndexMap<String, PhysicalConstraints>,
) {
    let has_io_cells = library.iter_cells().any(|(_, cell)| cell.io_info.is_some());
    if !has_io_cells {
        return;
    }

    let input_nets: Vec<GateNetId> = netlist.inputs.clone();
    let output_nets: Vec<GateNetId> = netlist.outputs.clone();
    let clock_nets: std::collections::HashSet<GateNetId> = netlist.clocks.iter().copied().collect();

    // Insert input IO buffers
    for &input_net_id in &input_nets {
        // A clock gets a pad only when the design assigns it to a pin.
        let has_pin = netlist
            .nets
            .get(input_net_id.0 as usize)
            .and_then(|n| port_constraint(port_constraints, &n.name))
            .is_some_and(|(c, _)| c.pin_location.is_some());
        if clock_nets.contains(&input_net_id) && !has_pin {
            continue;
        }

        let (input_cell, _input_io_info) = match library.find_input_pad() {
            Some(pair) => pair,
            None => continue,
        };

        let port_name = netlist
            .nets
            .get(input_net_id.0 as usize)
            .map(|n| n.name.clone())
            .unwrap_or_else(|| format!("in_{}", input_net_id.0));

        let buf_out_id = GateNetId(netlist.nets.len() as u32);
        netlist.add_net(GateNet::new(buf_out_id, format!("{}_io", port_name)));

        let cell_info = LibraryCellInfo::from_library_cell(input_cell);
        let mut cell = Cell::new_comb(
            CellId(0),
            input_cell.name.clone(),
            library.name.clone(),
            input_cell.fit,
            format!("{}_ibuf", port_name),
            vec![input_net_id],
            vec![buf_out_id],
        );
        cell.source_op = Some("IOBuffer".to_string());
        cell_info.apply_to_cell(&mut cell);
        apply_io_constraints_standalone(&mut cell, &port_name, port_constraints);
        netlist.add_cell(cell);

        rewire_consumers(netlist, input_net_id, buf_out_id);
    }

    // Insert output IO buffers
    for &output_net_id in &output_nets {
        let (output_cell, output_io_info) = match library.find_output_pad() {
            Some(pair) => pair,
            None => continue,
        };

        let port_name = netlist
            .nets
            .get(output_net_id.0 as usize)
            .map(|n| n.name.clone())
            .unwrap_or_else(|| format!("out_{}", output_net_id.0));

        let pad_out_id = GateNetId(netlist.nets.len() as u32);
        netlist.add_net(GateNet::new(pad_out_id, format!("{}_pad", port_name)));

        let cell_info = LibraryCellInfo::from_library_cell(output_cell);
        let supports_tristate = output_io_info.supports_tristate;

        let mut cell_inputs = vec![output_net_id];
        if supports_tristate {
            let tie_high = get_or_create_tie_high(netlist, library);
            cell_inputs.push(tie_high);
        }

        let mut cell = Cell::new_comb(
            CellId(0),
            output_cell.name.clone(),
            library.name.clone(),
            output_cell.fit,
            format!("{}_obuf", port_name),
            cell_inputs,
            vec![pad_out_id],
        );
        cell.source_op = Some("IOBuffer".to_string());
        cell_info.apply_to_cell(&mut cell);
        apply_io_constraints_standalone(&mut cell, &port_name, port_constraints);
        netlist.add_cell(cell);
    }
}

/// Get or create a tie-high net in the netlist.
fn get_or_create_tie_high(netlist: &mut GateNetlist, library: &TechLibrary) -> GateNetId {
    // Look for existing tie-high
    for net in &netlist.nets {
        if net.name.contains("tie_high") || net.name == "vdd" {
            return net.id;
        }
    }

    // Create new tie-high
    let id = GateNetId(netlist.nets.len() as u32);
    netlist.add_net(GateNet::new(id, "tie_high".to_string()));

    // Add tie cell if library has one
    let tie_cells = library.find_cells_by_function(&CellFunction::TieHigh);
    if let Some(lib_cell) = tie_cells.first() {
        let cell_info = LibraryCellInfo::from_library_cell(lib_cell);
        let mut cell = Cell::new_comb(
            CellId(0),
            lib_cell.name.clone(),
            library.name.clone(),
            lib_cell.fit,
            "tie_high".to_string(),
            vec![],
            vec![id],
        );
        cell_info.apply_to_cell(&mut cell);
        netlist.add_cell(cell);
    }

    id
}

/// Rewire all consumers of old_net to use new_net, skipping the buffer cell itself.
fn rewire_consumers(netlist: &mut GateNetlist, old_net: GateNetId, new_net: GateNetId) {
    for cell_idx in 0..netlist.cells.len() {
        let cell = &netlist.cells[cell_idx];
        if cell.inputs.contains(&old_net) && cell.outputs.contains(&new_net) {
            continue;
        }

        if netlist.cells[cell_idx].clock == Some(old_net) {
            netlist.cells[cell_idx].clock = Some(new_net);
        }
        for pin_idx in 0..netlist.cells[cell_idx].inputs.len() {
            if netlist.cells[cell_idx].inputs[pin_idx] == old_net {
                netlist.cells[cell_idx].inputs[pin_idx] = new_net;
            }
        }
    }
}

/// The constraints of the port a pad's net belongs to, with the net's bit
/// index when it is one bit of a multi-bit port: the netlist has a net per
/// bit (`top.led[2]`), while constraints are written per port (`top.led`).
fn port_constraint<'a>(
    port_constraints: &'a IndexMap<String, PhysicalConstraints>,
    net_name: &str,
) -> Option<(&'a PhysicalConstraints, Option<usize>)> {
    if let Some(c) = port_constraints.get(net_name) {
        return Some((c, None));
    }
    let (port, rest) = net_name.rsplit_once('[')?;
    let bit = rest.strip_suffix(']')?.parse().ok()?;
    port_constraints.get(port).map(|c| (c, Some(bit)))
}

/// Apply physical constraints to an IO cell.
fn apply_io_constraints_standalone(
    cell: &mut Cell,
    port_name: &str,
    port_constraints: &IndexMap<String, PhysicalConstraints>,
) {
    let Some((constraints, bit)) = port_constraint(port_constraints, port_name) else {
        return;
    };

    if let Some(ref pin_loc) = constraints.pin_location {
        use skalp_frontend::hir::PinLocation;
        match (pin_loc, bit) {
            (PinLocation::Single(pin), None | Some(0)) => {
                cell.parameters.insert("LOC".into(), pin.clone());
            }
            (PinLocation::Differential { positive, negative }, None | Some(0)) => {
                cell.parameters.insert("LOC".into(), positive.clone());
                cell.parameters.insert("LOC_N".into(), negative.clone());
            }
            // `pins: [...]` lists one pin per bit, bit 0 first; a one-bit
            // port's net carries no index
            (PinLocation::Multiple(pins), bit) => {
                if let Some(pin) = pins.get(bit.unwrap_or(0)) {
                    cell.parameters.insert("LOC".into(), pin.clone());
                }
            }
            _ => {}
        }
    }
    if let Some(ref std) = constraints.io_standard {
        cell.parameters.insert("IO_STANDARD".into(), std.clone());
    }
    if let Some(ref reason) = constraints.drive_override {
        cell.parameters.insert(
            crate::gate_netlist::DRIVE_OVERRIDE_PARAM.into(),
            reason.clone(),
        );
    }
    if let Some(ref drive) = constraints.drive_strength {
        cell.parameters
            .insert("DRIVE_STRENGTH".into(), format!("{:?}", drive));
    }
    if let Some(ref slew) = constraints.slew_rate {
        cell.parameters
            .insert("SLEW_RATE".into(), format!("{:?}", slew));
    }
    if let Some(ref term) = constraints.termination {
        cell.parameters
            .insert("TERMINATION".into(), format!("{:?}", term));
    }
    if let Some(schmitt) = constraints.schmitt_trigger {
        cell.parameters
            .insert("SCHMITT_TRIGGER".into(), schmitt.to_string());
    }
}

/// Map a LIR Mul node to DSP hard multiplier blocks (standalone version).
///
/// Queries `DspCellInfo` from the library. Handles single-block multiply
/// (operands ≤ DSP width) and tiled wide multiply (operands ≤ 2× DSP width).
/// Falls through silently if no DSP cell is available (AIG gate-level fallback).
#[allow(clippy::too_many_arguments)]
fn map_dsp_standalone(
    netlist: &mut GateNetlist,
    library: &TechLibrary,
    width: u32,
    result_width: u32,
    signed: bool,
    inputs: &[Vec<GateNetId>],
    outputs: &[GateNetId],
    path: &str,
) {
    let (dsp_cell, dsp_info) = match library.find_dsp_cell() {
        Some(pair) => pair,
        None => return,
    };
    let dsp_cell_name = dsp_cell.name.clone();
    let dsp_cell_fit = dsp_cell.fit;
    let dsp_cell_info = LibraryCellInfo::from_library_cell(dsp_cell);
    let dsp_info = dsp_info.clone();

    // Get tie nets
    let tie_low = {
        if let Some(id) = netlist
            .get_net_id("tie_low")
            .or_else(|| netlist.get_net_id("gnd"))
        {
            id
        } else {
            let id = netlist.add_net(GateNet::new(GateNetId(0), "tie_low".to_string()));
            let tie_info = lookup_cell(library, CellFunction::TieLow, "TIE_LOW", 0.01);
            let mut cell = Cell::new_comb(
                CellId(0),
                tie_info.name,
                library.name.clone(),
                tie_info.fit,
                "tie_low".to_string(),
                vec![],
                vec![id],
            );
            cell.function = Some(CellFunction::TieLow);
            netlist.add_cell(cell);
            id
        }
    };
    let tie_high = get_or_create_tie_high(netlist, library);

    let signed_net = if signed { tie_high } else { tie_low };

    // Determine sign extension bits
    let a_sign = if signed {
        inputs[0]
            .get(width as usize - 1)
            .copied()
            .unwrap_or(tie_low)
    } else {
        tie_low
    };
    let b_sign = if signed {
        inputs[1]
            .get(width as usize - 1)
            .copied()
            .unwrap_or(tie_low)
    } else {
        tie_low
    };

    // Helper: instantiate a single DSP block
    let instantiate_dsp = |netlist: &mut GateNetlist,
                           a_nets: &[GateNetId],
                           b_nets: &[GateNetId],
                           a_w: u32,
                           b_w: u32,
                           rw: u32,
                           out: &[GateNetId],
                           a_ext: GateNetId,
                           b_ext: GateNetId,
                           a_signed: GateNetId,
                           b_signed: GateNetId,
                           suffix: &str| {
        let mut cell_inputs = Vec::new();

        // A input — pad/extend to DSP a_width
        for bit in 0..dsp_info.a_width as usize {
            if bit < a_w as usize {
                cell_inputs.push(a_nets.get(bit).copied().unwrap_or(tie_low));
            } else {
                cell_inputs.push(a_ext);
            }
        }
        // B input
        for bit in 0..dsp_info.b_width as usize {
            if bit < b_w as usize {
                cell_inputs.push(b_nets.get(bit).copied().unwrap_or(tie_low));
            } else {
                cell_inputs.push(b_ext);
            }
        }
        // C input — tie low
        for _ in 0..DSP_C_BITS {
            cell_inputs.push(tie_low);
        }
        // SIGNEDA, SIGNEDB
        cell_inputs.push(a_signed);
        cell_inputs.push(b_signed);
        // SOURCEA, SOURCEB — direct input
        cell_inputs.push(tie_low);
        cell_inputs.push(tie_low);
        // CLK[0..3], CE[0..3], RST[0..3] — combinational mode
        for _ in 0..12 {
            cell_inputs.push(tie_low);
        }
        // SRIA[0..17], SRIB[0..17] — unused
        for _ in 0..36 {
            cell_inputs.push(tie_low);
        }

        // Outputs: P[0..p_width], SIGNEDP, cascade/shift outputs
        let mut cell_outputs = Vec::new();
        for bit in 0..dsp_info.p_width as usize {
            if bit < rw as usize && bit < out.len() {
                cell_outputs.push(out[bit]);
            } else {
                let unused = netlist.add_net(GateNet::new(
                    GateNetId(0),
                    format!("{}{}.dsp_unused_p{}", path, suffix, bit),
                ));
                cell_outputs.push(unused);
            }
        }
        // SIGNEDP
        let signedp = netlist.add_net(GateNet::new(
            GateNetId(0),
            format!("{}{}.dsp_unused_signedp", path, suffix),
        ));
        cell_outputs.push(signedp);
        // Cascade outputs: sroa/srob/roa/rob/roc × 18
        for prefix in &["sroa", "srob", "roa", "rob", "roc"] {
            for bit in 0..18 {
                let unused = netlist.add_net(GateNet::new(
                    GateNetId(0),
                    format!("{}{}.dsp_unused_{}_{}", path, suffix, prefix, bit),
                ));
                cell_outputs.push(unused);
            }
        }

        let cell_path = if suffix.is_empty() {
            path.to_string()
        } else {
            format!("{}{}", path, suffix)
        };
        let mut cell = Cell::new_comb(
            CellId(0),
            dsp_cell_name.clone(),
            library.name.clone(),
            dsp_cell_fit,
            cell_path,
            cell_inputs,
            cell_outputs,
        );
        cell.source_op = Some("DspMultiply".to_string());
        cell.parameters
            .insert(DSP_A_BITS_PARAM.to_string(), dsp_info.a_width.to_string());
        cell.parameters
            .insert(DSP_B_BITS_PARAM.to_string(), dsp_info.b_width.to_string());
        cell.parameters
            .insert(DSP_P_BITS_PARAM.to_string(), dsp_info.p_width.to_string());
        cell.parameters
            .insert("REG_INPUTA_CLK".to_string(), "NONE".to_string());
        cell.parameters
            .insert("REG_INPUTB_CLK".to_string(), "NONE".to_string());
        cell.parameters
            .insert("REG_INPUTC_CLK".to_string(), "NONE".to_string());
        cell.parameters
            .insert("REG_PIPELINE_CLK".to_string(), "NONE".to_string());
        cell.parameters
            .insert("REG_OUTPUT_CLK".to_string(), "NONE".to_string());
        cell.parameters
            .insert("SOURCEB_MODE".to_string(), "B_INPUT".to_string());
        cell.parameters
            .insert("MULT_BYPASS".to_string(), "DISABLED".to_string());
        cell.parameters
            .insert("RESETMODE".to_string(), "SYNC".to_string());
        cell.parameters.insert(
            "SIGNED_MODE".to_string(),
            if signed { "SIGNED" } else { "UNSIGNED" }.to_string(),
        );
        dsp_cell_info.apply_to_cell(&mut cell);
        netlist.add_cell(cell);
    };

    if width <= dsp_info.a_width && width <= dsp_info.b_width {
        // Single DSP block
        instantiate_dsp(
            netlist,
            &inputs[0],
            &inputs[1],
            width,
            width,
            result_width,
            outputs,
            a_sign,
            b_sign,
            signed_net,
            signed_net,
            "",
        );
    } else if width <= dsp_info.a_width * 2 && width <= dsp_info.b_width * 2 {
        // Tiled: split operands into hi/lo halves, 4 DSP blocks
        let half = dsp_info.a_width;
        let a_lo: Vec<GateNetId> = (0..half as usize)
            .map(|i| inputs[0].get(i).copied().unwrap_or(tie_low))
            .collect();
        let a_hi: Vec<GateNetId> = (half as usize..width as usize)
            .map(|i| inputs[0].get(i).copied().unwrap_or(a_sign))
            .collect();
        let b_lo: Vec<GateNetId> = (0..half as usize)
            .map(|i| inputs[1].get(i).copied().unwrap_or(tie_low))
            .collect();
        let b_hi: Vec<GateNetId> = (half as usize..width as usize)
            .map(|i| inputs[1].get(i).copied().unwrap_or(b_sign))
            .collect();

        let a_hi_w = width - half;
        let b_hi_w = width - half;

        let product_nets = |netlist: &mut GateNetlist, width: u32, tag: &str| -> Vec<GateNetId> {
            (0..width as usize)
                .map(|i| netlist.add_net(GateNet::new(GateNetId(0), format!("{path}.{tag}_p{i}"))))
                .collect()
        };

        // The low halves are unsigned; a signed multiply has signed high halves.
        let ll_width = (2 * half).min(dsp_info.p_width);
        let ll_out = product_nets(netlist, ll_width, "ll");
        instantiate_dsp(
            netlist, &a_lo, &b_lo, half, half, ll_width, &ll_out, tie_low, tie_low, tie_low,
            tie_low, "_ll",
        );
        let lh_width = (half + b_hi_w).min(dsp_info.p_width);
        let lh_out = product_nets(netlist, lh_width, "lh");
        instantiate_dsp(
            netlist, &a_lo, &b_hi, half, b_hi_w, lh_width, &lh_out, tie_low, b_sign, tie_low,
            signed_net, "_lh",
        );
        let hl_width = (a_hi_w + half).min(dsp_info.p_width);
        let hl_out = product_nets(netlist, hl_width, "hl");
        instantiate_dsp(
            netlist, &a_hi, &b_lo, a_hi_w, half, hl_width, &hl_out, a_sign, tie_low, signed_net,
            tie_low, "_hl",
        );
        let hh_width = (a_hi_w + b_hi_w).min(dsp_info.p_width);
        let hh_out = product_nets(netlist, hh_width, "hh");
        instantiate_dsp(
            netlist, &a_hi, &b_hi, a_hi_w, b_hi_w, hh_width, &hh_out, a_sign, b_sign, signed_net,
            signed_net, "_hh",
        );

        // Line each partial product up at its weight, extended to the full result.
        let total = result_width as usize;
        let shifted = |product: &[GateNetId], shift: usize, sign: GateNetId| -> Vec<GateNetId> {
            (0..total)
                .map(|bit| {
                    if bit < shift {
                        tie_low
                    } else {
                        product.get(bit - shift).copied().unwrap_or(sign)
                    }
                })
                .collect()
        };
        let top_bit = |product: &[GateNetId]| {
            if signed {
                product.last().copied().unwrap_or(tie_low)
            } else {
                tie_low
            }
        };
        let h = half as usize;
        let ll = shifted(&ll_out, 0, tie_low);
        let lh = shifted(&lh_out, h, top_bit(&lh_out));
        let hl = shifted(&hl_out, h, top_bit(&hl_out));
        let hh = shifted(&hh_out, 2 * h, top_bit(&hh_out));

        let sum = add_nets(netlist, library, &ll, &lh, tie_low, &format!("{path}.sum0"));
        let sum = add_nets(
            netlist,
            library,
            &sum,
            &hl,
            tie_low,
            &format!("{path}.sum1"),
        );
        let sum = add_nets(
            netlist,
            library,
            &sum,
            &hh,
            tie_low,
            &format!("{path}.sum2"),
        );
        let buffer = lookup_cell(library, CellFunction::Buf, "BUF", 0.1);
        for (bit, &output) in outputs.iter().enumerate().take(total) {
            let mut cell = Cell::new_comb(
                CellId(0),
                buffer.name.clone(),
                library.name.clone(),
                buffer.fit,
                format!("{path}.product_buf{bit}"),
                vec![sum[bit]],
                vec![output],
            );
            buffer.apply_to_cell(&mut cell);
            netlist.add_cell(cell);
        }
    } else {
        // Too wide for DSP — should not happen since the AIG path would handle it,
        // but just in case, warn
        eprintln!(
            "warning: Mul too wide for DSP at {} (width={}), falling through",
            path, width
        );
    }
}

/// Add two equal-width buses, on a carry chain when the library has one.
fn add_nets(
    netlist: &mut GateNetlist,
    library: &TechLibrary,
    a: &[GateNetId],
    b: &[GateNetId],
    tie_low: GateNetId,
    name: &str,
) -> Vec<GateNetId> {
    let width = a.len();
    let outputs: Vec<GateNetId> = (0..width)
        .map(|bit| netlist.add_net(GateNet::new(GateNetId(0), format!("{name}_{bit}"))))
        .collect();
    let operand = |nets: &[GateNetId]| crate::carry_chain::Operand {
        nets: nets.to_vec(),
        constant: None,
    };
    let add = LirOp::Add {
        width: width as u32,
        has_carry: false,
        const_b: None,
    };
    if crate::carry_chain::map_carry_chain(
        netlist,
        library,
        &add,
        &[operand(a), operand(b)],
        &outputs,
        name,
    ) {
        return outputs;
    }

    // No carry chain in this library: a ripple adder from basic gates.
    let xor = lookup_cell(library, CellFunction::Xor2, "XOR2", 0.2);
    let and = lookup_cell(library, CellFunction::And2, "AND2", 0.2);
    let or = lookup_cell(library, CellFunction::Or2, "OR2", 0.2);
    let gate = |netlist: &mut GateNetlist,
                info: &LibraryCellInfo,
                inputs: Vec<GateNetId>,
                output: GateNetId,
                tag: String| {
        let mut cell = Cell::new_comb(
            CellId(0),
            info.name.clone(),
            library.name.clone(),
            info.fit,
            tag,
            inputs,
            vec![output],
        );
        info.apply_to_cell(&mut cell);
        netlist.add_cell(cell);
    };
    let mut carry = tie_low;
    for bit in 0..width {
        let net = |netlist: &mut GateNetlist, tag: &str| {
            netlist.add_net(GateNet::new(GateNetId(0), format!("{name}_{tag}{bit}")))
        };
        let half_sum = net(netlist, "x");
        let generate = net(netlist, "g");
        let propagate = net(netlist, "p");
        let carry_out = net(netlist, "c");
        gate(
            netlist,
            &xor,
            vec![a[bit], b[bit]],
            half_sum,
            format!("{name}.x{bit}"),
        );
        gate(
            netlist,
            &xor,
            vec![half_sum, carry],
            outputs[bit],
            format!("{name}.s{bit}"),
        );
        gate(
            netlist,
            &and,
            vec![a[bit], b[bit]],
            generate,
            format!("{name}.g{bit}"),
        );
        gate(
            netlist,
            &and,
            vec![half_sum, carry],
            propagate,
            format!("{name}.p{bit}"),
        );
        gate(
            netlist,
            &or,
            vec![generate, propagate],
            carry_out,
            format!("{name}.c{bit}"),
        );
        carry = carry_out;
    }
    outputs
}

/// Smallest memory, in bits, worth a block RAM.
const BRAM_MIN_BITS: u32 = 256;

/// How a read register merged into a block RAM takes in new data.
enum ReadCapture {
    /// Every clock
    Always,
    /// Only while `enable` is true; otherwise it holds
    Hold { enable: EnableSignal },
    /// Every clock, but reads as `value` when `clear` is true
    Clear { clear: EnableSignal, value: u64 },
}

/// A control signal and whether it is active low.
#[derive(Clone, Copy)]
struct EnableSignal {
    signal: LirSignalId,
    active_low: bool,
}

/// A register merged into a block RAM.
struct ReadRegister {
    register: usize,
    d_mux: Option<usize>,
    capture: ReadCapture,
}

/// The register a memory's read data goes straight into, if it goes nowhere else.
fn find_read_register(
    lir: &Lir,
    readers: &IndexMap<LirSignalId, Vec<usize>>,
    rdata: LirSignalId,
    clock: Option<LirSignalId>,
) -> Option<ReadRegister> {
    let is_output =
        |s: LirSignalId| lir.signals[s.0 as usize].is_output || lir.outputs.contains(&s);
    let only_reader = |s: LirSignalId| match readers.get(&s).map(Vec::as_slice) {
        Some([reader]) if !is_output(s) => Some(*reader),
        _ => None,
    };
    let is_register = |index: usize, d: LirSignalId| {
        let node = &lir.nodes[index];
        matches!(
            node.op,
            LirOp::Reg {
                has_enable: false,
                ..
            }
        ) && node.clock == clock
            && node.inputs.first() == Some(&d)
    };
    let constant = |s: LirSignalId| {
        let driver = lir.signals[s.0 as usize].driver?;
        match lir.nodes[driver.0 as usize].op {
            LirOp::Constant { value, .. } => Some(value),
            _ => None,
        }
    };

    let reader = only_reader(rdata)?;
    if is_register(reader, rdata) {
        return Some(ReadRegister {
            register: reader,
            d_mux: None,
            capture: ReadCapture::Always,
        });
    }
    let mux = &lir.nodes[reader];
    let LirOp::Mux2 { .. } = mux.op else {
        return None;
    };
    let &[select, d0, d1] = mux.inputs.as_slice() else {
        return None;
    };
    let register = only_reader(mux.output)?;
    if !is_register(register, mux.output) {
        return None;
    }
    let q = lir.nodes[register].output;
    // Which select value picks the read data, and what the other input is.
    let (other, rdata_when_low) = match (d0 == rdata, d1 == rdata) {
        (false, true) => (d0, false),
        (true, false) => (d1, true),
        _ => return None,
    };
    let capture = if other == q {
        ReadCapture::Hold {
            enable: EnableSignal {
                signal: select,
                active_low: rdata_when_low,
            },
        }
    } else {
        ReadCapture::Clear {
            clear: EnableSignal {
                signal: select,
                active_low: !rdata_when_low,
            },
            value: constant(other)?,
        }
    };
    Some(ReadRegister {
        register,
        d_mux: Some(reader),
        capture,
    })
}

/// Emit a control signal as active high.
fn active_high(lir: &mut Lir, control: EnableSignal, name: &str) -> LirSignalId {
    if !control.active_low {
        return control.signal;
    }
    let inverted = lir.add_signal(name.to_string(), 1);
    lir.add_node(
        LirOp::Not { width: 1 },
        vec![control.signal],
        inverted,
        format!("{name}.not"),
    );
    inverted
}

/// A register recording that the last clock loaded the read register from the RAM.
fn add_loaded_flag(
    lir: &mut Lir,
    next: LirSignalId,
    flag: LirSignalId,
    register: &crate::lir::LirNode,
    with_reset: bool,
    name: &str,
) {
    let async_reset = matches!(
        register.op,
        LirOp::Reg {
            async_reset: true,
            ..
        }
    );
    lir.add_seq_node(
        LirOp::Reg {
            width: 1,
            has_enable: false,
            has_reset: with_reset && register.reset.is_some(),
            async_reset,
            reset_value: Some(0),
        },
        vec![next],
        flag,
        name.to_string(),
        register.clock.expect("a read register has a clock"),
        register.reset.filter(|_| with_reset),
    );
}

/// `output = loaded ? data : value`.
fn add_fixed_value_mux(
    lir: &mut Lir,
    loaded: LirSignalId,
    data: LirSignalId,
    value: u64,
    output: LirSignalId,
    name: &str,
) {
    let width = lir.signals[data.0 as usize].width;
    let fixed = lir.add_signal(format!("{name}_fixed"), width);
    lir.add_node(
        LirOp::Constant { width, value },
        Vec::new(),
        fixed,
        format!("{name}.fixed"),
    );
    lir.add_node(
        LirOp::Mux2 { width },
        vec![loaded, fixed, data],
        output,
        format!("{name}.value"),
    );
}

/// Make a merged read register's reset and clear values appear on its output.
///
/// A block RAM's output has no general set or clear, so a flag records whether
/// the last clock loaded the register from the RAM, and the output is the fixed
/// value until it did. `q` then behaves just as the register would have.
fn add_read_value_overrides(
    lir: &mut Lir,
    rdata: LirSignalId,
    q: LirSignalId,
    capture: &ReadCapture,
    register: &crate::lir::LirNode,
    reset_value: Option<u64>,
    path: &str,
) {
    let width = lir.signals[rdata.0 as usize].width;
    let mut data = rdata;

    if let ReadCapture::Clear { clear, value } = capture {
        let name = format!("{path}_read_clear");
        let flag = lir.add_signal(format!("{name}_loaded"), 1);
        let not_clear = active_high(
            lir,
            EnableSignal {
                signal: clear.signal,
                active_low: !clear.active_low,
            },
            &format!("{name}_inactive"),
        );
        add_loaded_flag(
            lir,
            not_clear,
            flag,
            register,
            false,
            &format!("{name}.loaded"),
        );
        let output = if reset_value.is_some() {
            lir.add_signal(format!("{name}_data"), width)
        } else {
            q
        };
        add_fixed_value_mux(lir, flag, data, *value, output, &name);
        data = output;
    }

    if let Some(value) = reset_value {
        let name = format!("{path}_read_reset");
        let flag = lir.add_signal(format!("{name}_loaded"), 1);
        let one = lir.add_signal(format!("{name}_one"), 1);
        lir.add_node(
            LirOp::Constant { width: 1, value: 1 },
            Vec::new(),
            one,
            format!("{name}.one"),
        );
        let next = match capture {
            ReadCapture::Hold { enable } => {
                let enable = active_high(lir, *enable, &format!("{name}_enable"));
                let held = lir.add_signal(format!("{name}_loaded_d"), 1);
                lir.add_node(
                    LirOp::Mux2 { width: 1 },
                    vec![enable, flag, one],
                    held,
                    format!("{name}.loaded_d"),
                );
                held
            }
            _ => one,
        };
        add_loaded_flag(lir, next, flag, register, true, &format!("{name}.loaded"));
        add_fixed_value_mux(lir, flag, data, value, q, &name);
    }
}

/// Merge each block RAM's read register into the RAM.
///
/// The LIR describes a read as a combinational memory output followed by the
/// design's register, but a block RAM's output is itself registered: keeping
/// both would delay reads by an extra cycle. A memory whose read data also feeds
/// logic directly cannot use a block RAM at all and is marked `async_read`.
fn absorb_block_ram_read_registers(lir: &Lir, library: &TechLibrary) -> Option<Lir> {
    library.find_ram_cell()?;
    let candidates: Vec<usize> = lir
        .nodes
        .iter()
        .enumerate()
        .filter(|(_, node)| {
            matches!(
                node.op,
                LirOp::MemBlock {
                    data_width,
                    depth,
                    async_read: false,
                    registered_read: false,
                    ..
                } if data_width * depth >= BRAM_MIN_BITS
            )
        })
        .map(|(index, _)| index)
        .collect();
    if candidates.is_empty() {
        return None;
    }

    let mut readers: IndexMap<LirSignalId, Vec<usize>> = IndexMap::new();
    for (index, node) in lir.nodes.iter().enumerate() {
        for &input in &node.inputs {
            readers.entry(input).or_default().push(index);
        }
    }

    let mut result = lir.clone();
    let mut removed = std::collections::HashSet::new();
    for memory in candidates {
        let (rdata, clock, path) = {
            let node = &lir.nodes[memory];
            (node.output, node.clock, node.path.clone())
        };
        let Some(found) = find_read_register(lir, &readers, rdata, clock) else {
            eprintln!(
                "warning: memory read at '{path}' feeds logic directly, so it uses LUT RAM or registers instead of block RAM"
            );
            if let LirOp::MemBlock { async_read, .. } = &mut result.nodes[memory].op {
                *async_read = true;
            }
            continue;
        };
        let register = lir.nodes[found.register].clone();
        let q = register.output;

        // With no reset, the fixed value is what the register powers up holding.
        let reset_value = match register.op {
            LirOp::Reg {
                has_reset: true,
                reset_value,
                ..
            } => Some(reset_value.unwrap_or(0)),
            _ => lir.signals[q.0 as usize].initial.filter(|&v| v != 0),
        };
        let has_override =
            reset_value.is_some() || matches!(found.capture, ReadCapture::Clear { .. });

        // A Hold enable goes to the RAM's clock enable. A clear does not stop the
        // RAM capturing, since the design's register would load either way.
        let enable = match &found.capture {
            ReadCapture::Hold { enable } => Some(active_high(
                &mut result,
                *enable,
                &format!("{path}_read_enable_ram"),
            )),
            _ => None,
        };
        if has_override {
            add_read_value_overrides(
                &mut result,
                rdata,
                q,
                &found.capture,
                &register,
                reset_value,
                &path,
            );
        }

        let node = &mut result.nodes[memory];
        let LirOp::MemBlock {
            registered_read,
            read_enable,
            ..
        } = &mut node.op
        else {
            continue;
        };
        *registered_read = true;
        *read_enable = enable.is_some();
        if !has_override {
            node.output = q;
        }
        node.inputs.extend(enable);
        removed.insert(found.register);
        removed.extend(found.d_mux);
    }

    let mut index = 0;
    result.nodes.retain(|_| {
        index += 1;
        !removed.contains(&(index - 1))
    });
    for signal in &mut result.signals {
        signal.driver = None;
    }
    for (index, node) in result.nodes.iter_mut().enumerate() {
        node.id = LirNodeId(index as u32);
        result.signals[node.output.0 as usize].driver = Some(node.id);
    }
    Some(result)
}

/// Add a flip-flop for `d`, optionally holding its value when not enabled.
fn register_bit(
    netlist: &mut GateNetlist,
    library: &TechLibrary,
    d: GateNetId,
    enable: Option<GateNetId>,
    clk: GateNetId,
    name: &str,
) -> GateNetId {
    let q = netlist.add_net(GateNet::new(GateNetId(0), format!("{name}_q")));
    let data = match enable {
        None => d,
        Some(enable) => {
            let mux_ci = lookup_cell(library, CellFunction::Mux2, "MUX2", 0.3);
            let held = netlist.add_net(GateNet::new(GateNetId(0), format!("{name}_d")));
            let mut mux = Cell::new_comb(
                CellId(0),
                mux_ci.name.clone(),
                library.name.clone(),
                mux_ci.fit,
                format!("{name}_hold"),
                mux2_inputs(&mux_ci, enable, q, d),
                vec![held],
            );
            mux_ci.apply_to_cell(&mut mux);
            netlist.add_cell(mux);
            held
        }
    };
    let dff_ci = lookup_cell(library, CellFunction::Dff, "DFF", 0.5);
    let mut dff = Cell::new_seq(
        CellId(0),
        dff_ci.name.clone(),
        library.name.clone(),
        dff_ci.fit,
        format!("{name}_dff"),
        vec![data],
        vec![q],
        clk,
        None,
    );
    dff_ci.apply_to_cell(&mut dff);
    netlist.add_cell(dff);
    q
}

/// Output high when the upper address bits select block RAM bank `bank`.
fn decode_ram_bank(
    netlist: &mut GateNetlist,
    library: &TechLibrary,
    address_bits: &[GateNetId],
    bank: u32,
    tie_high: GateNetId,
    name: &str,
) -> GateNetId {
    let mut selected = tie_high;
    for (bit, &address_bit) in address_bits.iter().enumerate() {
        let wanted_high = (bank >> bit) & 1 == 1;
        let (function, fallback) = if wanted_high {
            (CellFunction::Buf, "BUF")
        } else {
            (CellFunction::Inv, "INV")
        };
        let ci = lookup_cell(library, function, fallback, 0.1);
        let matched = netlist.add_net(GateNet::new(GateNetId(0), format!("{name}_match{bit}")));
        let mut cell = Cell::new_comb(
            CellId(0),
            ci.name.clone(),
            library.name.clone(),
            ci.fit,
            format!("{name}_addr{bit}"),
            vec![address_bit],
            vec![matched],
        );
        ci.apply_to_cell(&mut cell);
        netlist.add_cell(cell);

        let and_ci = lookup_cell(library, CellFunction::And2, "AND2", 0.2);
        let next = netlist.add_net(GateNet::new(GateNetId(0), format!("{name}_sel{bit}")));
        let mut cell = Cell::new_comb(
            CellId(0),
            and_ci.name.clone(),
            library.name.clone(),
            and_ci.fit,
            format!("{name}_and{bit}"),
            vec![selected, matched],
            vec![next],
        );
        and_ci.apply_to_cell(&mut cell);
        netlist.add_cell(cell);
        selected = next;
    }
    selected
}

/// Map a LIR MemBlock node to RAM primitive cells (standalone version).
///
/// Queries `RamCellInfo` from the library for capabilities (aspect ratios,
/// pin names). Falls back to DFF decomposition if no RAM cell available.
/// Handles width tiling, depth tiling, and combined tiling.
#[allow(clippy::too_many_arguments)]
fn map_memblock_standalone(
    netlist: &mut GateNetlist,
    library: &TechLibrary,
    data_width: u32,
    addr_width: u32,
    depth: u32,
    has_write: bool,
    async_read: bool,
    read_enable: Option<GateNetId>,
    inputs: &[Vec<GateNetId>],
    outputs: &[GateNetId],
    path: &str,
    clk: GateNetId,
    init: Option<&[u64]>,
) {
    // AUDIT-2 #7: small memories must NOT burn a block-RAM tile — the
    // documented Auto policy keeps memories under 256 bits in registers
    // even when the target has BRAM (an iCE40 EBR is 4096 bits; spending
    // one on 32 bits wastes a scarce resource). The LIR MemBlock is the
    // canonical memory form; THIS is where BRAM-vs-DFF is decided.
    let total_bits = data_width * depth;

    // Query library for RAM cell
    // Block RAM reads are synchronous; a memory read combinationally (a register
    // file read in the same cycle) would see its data a cycle late, so it is built
    // from registers instead.
    let ram_lookup = if total_bits < BRAM_MIN_BITS || async_read {
        None
    } else {
        library.find_ram_cell()
    };
    // A memory read combinationally goes into distributed RAM when the target
    // has it and that is cheaper than registers (it nearly always is beyond a
    // couple of words).
    if ram_lookup.is_none() && goes_to_distram(library, data_width, depth, has_write, async_read) {
        map_memblock_to_distram(
            netlist, library, data_width, addr_width, depth, inputs, outputs, path, clk, init,
        );
        return;
    }
    let (ram_cell, ram_info) = match ram_lookup {
        Some(info) => info,
        None => {
            // No block RAM in this library (e.g. generic ASIC), or the
            // memory is below the BRAM threshold: decompose the memory
            // into DFFs + write-select muxes + a read mux tree. This
            // keeps the memory verifiable — gate-level simulation and the SAT
            // equivalence AIG both understand plain DFF/MUX cells.
            decompose_memblock_to_dffs(
                netlist, library, data_width, addr_width, depth, has_write, inputs, outputs, path,
                clk, init,
            );
            return;
        }
    };
    let ram_cell_name = ram_cell.name.clone();
    let ram_cell_fit = ram_cell.fit;
    let ram_cell_info = LibraryCellInfo::from_library_cell(ram_cell);
    let ram_info = ram_info.clone();

    // Select best aspect ratio
    let (block_depth, block_width) =
        select_best_aspect_ratio(&ram_info.aspect_ratios, data_width, depth);

    // Compute tiling
    let width_blocks = data_width.div_ceil(block_width);
    let depth_blocks = depth.div_ceil(block_depth);
    let block_addr_width = clog2(block_depth);

    // Get tie nets
    let tie_low = {
        if let Some(id) = netlist
            .get_net_id("tie_low")
            .or_else(|| netlist.get_net_id("gnd"))
        {
            id
        } else {
            let id = netlist.add_net(GateNet::new(GateNetId(0), "tie_low".to_string()));
            let tie_info = lookup_cell(library, CellFunction::TieLow, "TIE_LOW", 0.01);
            let mut cell = Cell::new_comb(
                CellId(0),
                tie_info.name.clone(),
                library.name.clone(),
                tie_info.fit,
                "tie_low".to_string(),
                vec![],
                vec![id],
            );
            tie_info.apply_to_cell(&mut cell);
            netlist.add_cell(cell);
            id
        }
    };
    let tie_high = get_or_create_tie_high(netlist, library);

    // Extract input nets
    let raddr_nets = &inputs[0];
    let waddr_nets = if has_write && inputs.len() > 1 {
        &inputs[1]
    } else {
        &inputs[0]
    };
    let empty_vec = Vec::new();
    let wdata_nets = if has_write && inputs.len() > 2 {
        &inputs[2]
    } else {
        &empty_vec
    };
    let we_net = if has_write && inputs.len() > 3 {
        inputs[3].first().copied().unwrap_or(tie_low)
    } else {
        tie_low
    };

    // Helper to instantiate a single RAM block
    let instantiate_single = |netlist: &mut GateNetlist,
                              raddr: &[GateNetId],
                              waddr: &[GateNetId],
                              wdata: &[GateNetId],
                              we: GateNetId,
                              out: &[GateNetId],
                              dw: u32,
                              aw: u32,
                              suffix: &str,
                              contents: Option<Vec<u64>>| {
        let mut cell_inputs = Vec::new();

        // Read address — pad to block_addr_width
        for bit in 0..block_addr_width as usize {
            cell_inputs.push(raddr.get(bit).copied().unwrap_or(tie_low));
        }
        cell_inputs.push(clk); // RCLK
        cell_inputs.push(read_enable.unwrap_or(tie_high)); // RCLKE
        cell_inputs.push(tie_high); // RE

        if has_write {
            for bit in 0..block_width as usize {
                cell_inputs.push(wdata.get(bit).copied().unwrap_or(tie_low));
            }
            for bit in 0..block_addr_width as usize {
                cell_inputs.push(waddr.get(bit).copied().unwrap_or(tie_low));
            }
            cell_inputs.push(clk); // WCLK
            cell_inputs.push(tie_high); // WCLKE
            cell_inputs.push(we); // WE
        } else {
            for _ in 0..block_width as usize {
                cell_inputs.push(tie_low);
            }
            for _ in 0..block_addr_width as usize {
                cell_inputs.push(tie_low);
            }
            cell_inputs.push(clk);
            cell_inputs.push(tie_low); // WCLKE
            cell_inputs.push(tie_low); // WE
        }

        // MASK — tie all high (no masking)
        if ram_info.has_write_mask {
            for _ in 0..block_width as usize {
                cell_inputs.push(tie_high);
            }
        }

        // Outputs
        let mut cell_outputs = Vec::new();
        for bit in 0..block_width as usize {
            if bit < dw as usize {
                cell_outputs.push(out.get(bit).copied().unwrap_or(out[0]));
            } else {
                let unused = netlist.add_net(GateNet::new(
                    GateNetId(0),
                    format!("{}{}.unused_rdata{}", path, suffix, bit),
                ));
                cell_outputs.push(unused);
            }
        }

        let cell_path = if suffix.is_empty() {
            path.to_string()
        } else {
            format!("{}{}", path, suffix)
        };
        let mut cell = Cell::new_seq(
            CellId(0),
            ram_cell_name.clone(),
            library.name.clone(),
            ram_cell_fit,
            cell_path,
            cell_inputs,
            cell_outputs,
            clk,
            None,
        );
        cell.source_op = Some("MemBlock".to_string());
        // The width and depth; each backend derives its own mode settings.
        cell.parameters.insert(
            RAM_ADDR_BITS_PARAM.to_string(),
            block_addr_width.to_string(),
        );
        cell.parameters
            .insert(RAM_DATA_BITS_PARAM.to_string(), block_width.to_string());
        if let Some(words) = contents {
            cell.set_memory_init(&words);
        }
        ram_cell_info.apply_to_cell(&mut cell);
        netlist.add_cell(cell);
    };
    // The words of the block holding addresses from `depth_block` *
    // block_depth, bits from `bit_lo`, `width` bits wide.
    let block_contents = |depth_block: u32, bit_lo: u32, width: u32| -> Option<Vec<u64>> {
        let words = init?;
        let start = (depth_block * block_depth) as usize;
        let mask = if width >= 64 {
            u64::MAX
        } else {
            (1u64 << width) - 1
        };
        let mut slice: Vec<u64> = words
            .iter()
            .skip(start)
            .take(block_depth as usize)
            .map(|&w| (w >> bit_lo) & mask)
            .collect();
        while slice.last() == Some(&0) {
            slice.pop();
        }
        Some(slice)
    };

    if depth_blocks == 1 && width_blocks == 1 {
        // Single block
        instantiate_single(
            netlist,
            raddr_nets,
            waddr_nets,
            wdata_nets,
            we_net,
            outputs,
            data_width,
            addr_width,
            "",
            block_contents(0, 0, data_width),
        );
    } else if depth_blocks == 1 {
        // Width tiling: split data across multiple blocks
        for wb in 0..width_blocks {
            let bit_lo = (wb * block_width) as usize;
            let bit_hi = ((wb + 1) * block_width).min(data_width) as usize;
            let slice_width = (bit_hi - bit_lo) as u32;

            let slice_wdata: Vec<GateNetId> = (bit_lo..bit_hi)
                .map(|i| wdata_nets.get(i).copied().unwrap_or(tie_low))
                .collect();
            let slice_out: Vec<GateNetId> = (bit_lo..bit_hi)
                .map(|i| outputs.get(i).copied().unwrap_or(outputs[0]))
                .collect();

            instantiate_single(
                netlist,
                raddr_nets,
                waddr_nets,
                &slice_wdata,
                we_net,
                &slice_out,
                slice_width,
                addr_width,
                &format!("_w{}", wb),
                block_contents(0, bit_lo as u32, slice_width),
            );
        }
    } else {
        // Depth tiling (with possible width tiling)
        // Need address decode for write enable and output MUX for read
        let upper_addr_bits = clog2(depth_blocks);

        // A block RAM's output follows the address by a cycle, so the output mux
        // must select on the address of that cycle.
        let read_bank_bits: Vec<GateNetId> = (0..upper_addr_bits as usize)
            .map(|bit| {
                let addr_bit = raddr_nets
                    .get(block_addr_width as usize + bit)
                    .copied()
                    .unwrap_or(tie_low);
                let name = format!("{}_rbank{}", path, bit);
                register_bit(netlist, library, addr_bit, read_enable, clk, &name)
            })
            .collect();

        // The latest read mux output for each bit of each width slice.
        let mut read_results: IndexMap<(u32, usize), GateNetId> = IndexMap::new();
        for db in 0..depth_blocks {
            let write_bank_bits: Vec<GateNetId> = (0..upper_addr_bits as usize)
                .map(|bit| {
                    waddr_nets
                        .get(block_addr_width as usize + bit)
                        .copied()
                        .unwrap_or(tie_low)
                })
                .collect();
            let write_sel = decode_ram_bank(
                netlist,
                library,
                &write_bank_bits,
                db,
                tie_high,
                &format!("{}_d{}_w", path, db),
            );
            let read_sel = decode_ram_bank(
                netlist,
                library,
                &read_bank_bits,
                db,
                tie_high,
                &format!("{}_d{}_r", path, db),
            );

            // Bank-qualified write enable
            let bank_we = netlist.add_net(GateNet::new(
                GateNetId(0),
                format!("{}_d{}_bank_we", path, db),
            ));
            let we_and_ci = lookup_cell(library, CellFunction::And2, "AND2", 0.2);
            let mut cell = Cell::new_comb(
                CellId(0),
                we_and_ci.name.clone(),
                library.name.clone(),
                we_and_ci.fit,
                format!("{}_d{}_we_and", path, db),
                vec![we_net, write_sel],
                vec![bank_we],
            );
            we_and_ci.apply_to_cell(&mut cell);
            netlist.add_cell(cell);

            // For each width block in this depth slice
            for wb in 0..width_blocks {
                let bit_lo = (wb * block_width) as usize;
                let bit_hi = ((wb + 1) * block_width).min(data_width) as usize;
                let slice_width = (bit_hi - bit_lo) as u32;

                let slice_wdata: Vec<GateNetId> = (bit_lo..bit_hi)
                    .map(|i| wdata_nets.get(i).copied().unwrap_or(tie_low))
                    .collect();

                // Block read outputs go to intermediate nets (will be MUXed)
                let block_out: Vec<GateNetId> = (bit_lo..bit_hi)
                    .map(|i| {
                        netlist.add_net(GateNet::new(
                            GateNetId(0),
                            format!("{}_d{}_w{}_rdata{}", path, db, wb, i - bit_lo),
                        ))
                    })
                    .collect();

                instantiate_single(
                    netlist,
                    raddr_nets,
                    waddr_nets,
                    &slice_wdata,
                    bank_we,
                    &block_out,
                    slice_width,
                    addr_width,
                    &format!("_d{}_w{}", db, wb),
                    block_contents(db, bit_lo as u32, slice_width),
                );

                // MUX read data: if bank_sel, use this block's output, else keep previous
                // For first depth block, output is block_out directly; for subsequent,
                // MUX with previous result
                let mux_ci = lookup_cell(library, CellFunction::Mux2, "MUX2", 0.3);
                if db == 0 {
                    // First bank: wire block outputs to intermediate "result" nets
                    for i in bit_lo..bit_hi {
                        let result_net = netlist.add_net(GateNet::new(
                            GateNetId(0),
                            format!("{}_w{}_result{}", path, wb, i - bit_lo),
                        ));
                        read_results.insert((wb, i - bit_lo), result_net);
                        // MUX: bank_sel ? block_out : tie_low
                        let mut cell = Cell::new_comb(
                            CellId(0),
                            mux_ci.name.clone(),
                            library.name.clone(),
                            mux_ci.fit,
                            format!("{}_d0_w{}_rmux{}", path, wb, i - bit_lo),
                            mux2_inputs(&mux_ci, read_sel, tie_low, block_out[i - bit_lo]),
                            vec![result_net],
                        );
                        cell.source_op = Some("MemBlock_ReadMux".to_string());
                        mux_ci.apply_to_cell(&mut cell);
                        netlist.add_cell(cell);
                    }
                } else if db == depth_blocks - 1 {
                    // Last bank: MUX to final output
                    for i in bit_lo..bit_hi {
                        let prev_result = read_results[&(wb, i - bit_lo)];
                        let final_out = outputs.get(i).copied().unwrap_or(outputs[0]);
                        // MUX: bank_sel ? block_out : prev_result → final output
                        let mut cell = Cell::new_comb(
                            CellId(0),
                            mux_ci.name.clone(),
                            library.name.clone(),
                            mux_ci.fit,
                            format!("{}_d{}_w{}_rmux{}", path, db, wb, i - bit_lo),
                            mux2_inputs(&mux_ci, read_sel, prev_result, block_out[i - bit_lo]),
                            vec![final_out],
                        );
                        cell.source_op = Some("MemBlock_ReadMux".to_string());
                        mux_ci.apply_to_cell(&mut cell);
                        netlist.add_cell(cell);
                    }
                } else {
                    // Middle bank: MUX with previous, store to new result net
                    for i in bit_lo..bit_hi {
                        let prev_result = read_results[&(wb, i - bit_lo)];
                        let new_result = netlist.add_net(GateNet::new(
                            GateNetId(0),
                            format!("{}_d{}_w{}_result{}", path, db, wb, i - bit_lo),
                        ));
                        let mut cell = Cell::new_comb(
                            CellId(0),
                            mux_ci.name.clone(),
                            library.name.clone(),
                            mux_ci.fit,
                            format!("{}_d{}_w{}_rmux{}", path, db, wb, i - bit_lo),
                            mux2_inputs(&mux_ci, read_sel, prev_result, block_out[i - bit_lo]),
                            vec![new_result],
                        );
                        cell.source_op = Some("MemBlock_ReadMux".to_string());
                        mux_ci.apply_to_cell(&mut cell);
                        netlist.add_cell(cell);
                        read_results.insert((wb, i - bit_lo), new_result);
                    }
                }
            }
        }
    }
}

/// Whether a memory is built from 16×4 distributed RAM cells: it is read
/// combinationally (so it cannot be a block RAM), written, the target has
/// distributed RAM, and that is cheaper than registers.
fn goes_to_distram(
    library: &TechLibrary,
    data_width: u32,
    depth: u32,
    has_write: bool,
    async_read: bool,
) -> bool {
    async_read
        && has_write
        && library.find_best_cell(&CellFunction::DistRam16x4).is_some()
        && distram_is_cheaper(data_width, depth)
}

/// Words in one distributed RAM cell, and its address bits.
const DISTRAM_WORDS: u32 = 16;
const DISTRAM_ADDR_BITS: u32 = 4;

/// Split each distributed-RAM memory deeper than one cell into memories of
/// one cell's depth (banks), with the bank write enables and the read mux
/// over the banks as ordinary logic. That logic then goes through the AIG
/// with the rest of the design, so the mapper can pack the read mux into the
/// LUTs that use the data, instead of leaving two or more mux levels in
/// front of them. Returns `None` when there is nothing to split.
fn split_distram_banks(lir: &Lir, library: &TechLibrary) -> Option<Lir> {
    let candidates: Vec<usize> = lir
        .nodes
        .iter()
        .enumerate()
        .filter(|(_, node)| match &node.op {
            LirOp::MemBlock {
                data_width,
                addr_width,
                depth,
                has_write,
                async_read,
                registered_read: false,
                read_enable: false,
                ..
            } => {
                *depth > DISTRAM_WORDS
                    && *addr_width > DISTRAM_ADDR_BITS
                    && node.inputs.len() == 4
                    && goes_to_distram(library, *data_width, *depth, *has_write, *async_read)
            }
            _ => false,
        })
        .map(|(index, _)| index)
        .collect();
    if candidates.is_empty() {
        return None;
    }

    let mut result = lir.clone();
    for memory in candidates {
        let node = lir.nodes[memory].clone();
        let LirOp::MemBlock {
            data_width: dw,
            addr_width: aw,
            depth,
            init,
            ..
        } = node.op
        else {
            unreachable!()
        };
        let (raddr, waddr, wdata, we) = (
            node.inputs[0],
            node.inputs[1],
            node.inputs[2],
            node.inputs[3],
        );
        let path = node.path.clone();
        let rdata_name = lir.signals[node.output.0 as usize].name.clone();
        let upper = aw - DISTRAM_ADDR_BITS;
        let banks = depth.div_ceil(DISTRAM_WORDS);

        let select =
            |result: &mut Lir, from: LirSignalId, width: u32, high: u32, low: u32, name: String| {
                let out = result.add_signal(name.clone(), high - low + 1);
                result.add_node(
                    LirOp::RangeSelect { width, high, low },
                    vec![from],
                    out,
                    name,
                );
                out
            };
        let raddr_low = select(
            &mut result,
            raddr,
            aw,
            DISTRAM_ADDR_BITS - 1,
            0,
            format!("{path}.raddr_low"),
        );
        let waddr_low = select(
            &mut result,
            waddr,
            aw,
            DISTRAM_ADDR_BITS - 1,
            0,
            format!("{path}.waddr_low"),
        );
        let waddr_high = select(
            &mut result,
            waddr,
            aw,
            aw - 1,
            DISTRAM_ADDR_BITS,
            format!("{path}.waddr_high"),
        );

        // Bank b: written when we & waddr[high] == b; its read data is rdata_b
        let mut bank_data = Vec::with_capacity(banks as usize);
        for b in 0..banks {
            let index = result.add_signal(format!("{path}.bank{b}_index"), upper);
            result.add_node(
                LirOp::Constant {
                    width: upper,
                    value: b as u64,
                },
                vec![],
                index,
                format!("{path}.bank{b}_index"),
            );
            let hit = result.add_signal(format!("{path}.bank{b}_hit"), 1);
            result.add_node(
                LirOp::Eq { width: upper },
                vec![waddr_high, index],
                hit,
                format!("{path}.bank{b}_hit"),
            );
            let bank_we = result.add_signal(format!("{path}.bank{b}_we"), 1);
            result.add_node(
                LirOp::And { width: 1 },
                vec![we, hit],
                bank_we,
                format!("{path}.bank{b}_we"),
            );
            let words = (depth - b * DISTRAM_WORDS).min(DISTRAM_WORDS);
            let bank_init = init.as_ref().map(|words_init| {
                words_init
                    .iter()
                    .skip((b * DISTRAM_WORDS) as usize)
                    .take(words as usize)
                    .copied()
                    .collect::<Vec<u64>>()
            });
            let out = result.add_signal(format!("{rdata_name}_bank{b}"), dw);
            let bank_path = format!("{path}.bank{b}");
            let op = LirOp::MemBlock {
                data_width: dw,
                addr_width: DISTRAM_ADDR_BITS,
                depth: words,
                has_write: true,
                read_only: false,
                async_read: true,
                registered_read: false,
                read_enable: false,
                init: bank_init,
            };
            let inputs = vec![raddr_low, waddr_low, wdata, bank_we];
            match node.clock {
                Some(clock) => {
                    result.add_seq_node(op, inputs, out, bank_path, clock, node.reset);
                }
                None => {
                    result.add_node(op, inputs, out, bank_path);
                }
            }
            bank_data.push(out);
        }

        // Read mux tree: level u halves the banks using raddr[4 + u]. A
        // missing partner (past the memory's depth) reads as 0.
        let zero = result.add_signal(format!("{path}.bank_none"), dw);
        result.add_node(
            LirOp::Constant {
                width: dw,
                value: 0,
            },
            vec![],
            zero,
            format!("{path}.bank_none"),
        );
        let mut level = bank_data;
        let mut u = 0;
        while level.len() > 1 {
            let sel = select(
                &mut result,
                raddr,
                aw,
                DISTRAM_ADDR_BITS + u,
                DISTRAM_ADDR_BITS + u,
                format!("{path}.raddr{}", DISTRAM_ADDR_BITS + u),
            );
            let last = level.len() <= 2;
            let pairs: Vec<(LirSignalId, LirSignalId)> = level
                .chunks(2)
                .map(|pair| (pair[0], pair.get(1).copied().unwrap_or(zero)))
                .collect();
            level = pairs
                .into_iter()
                .enumerate()
                .map(|(k, (d0, d1))| {
                    let mux_path = format!("{path}.rmux{u}_{k}");
                    if last {
                        // The memory's own node becomes the last mux, so its
                        // output signal keeps its one driver
                        let root = &mut result.nodes[memory];
                        root.op = LirOp::Mux2 { width: dw };
                        root.inputs = vec![sel, d0, d1];
                        root.path = mux_path;
                        root.clock = None;
                        root.reset = None;
                        node.output
                    } else {
                        let out = result.add_signal(mux_path.clone(), dw);
                        result.add_node(
                            LirOp::Mux2 { width: dw },
                            vec![sel, d0, d1],
                            out,
                            mux_path,
                        );
                        out
                    }
                })
                .collect();
            u += 1;
        }
    }
    Some(result)
}

/// LUT-equivalent cost of a `DistRam16x4` cell: four RAM LUTs plus the write
/// port slice that drives them.
const DISTRAM_CELL_COST: u32 = 6;

/// Whether a `data_width` × `depth` memory costs less as 16×4 distributed RAM
/// than as registers. Registers: one FF per bit, a 2:1 mux per bit and word
/// for the read tree, and a write mux per bit. Distributed RAM: the cells,
/// plus a read mux tree across 16-word banks.
fn distram_is_cheaper(data_width: u32, depth: u32) -> bool {
    let (dw, depth) = (data_width.max(1), depth.max(1));
    let register_cost = dw * depth + dw * (depth - 1) + dw * depth;
    let banks = depth.div_ceil(16);
    let distram_cost = banks * dw.div_ceil(4) * DISTRAM_CELL_COST + dw * (banks - 1);
    distram_cost < register_cost
}

/// Map an asynchronous-read MemBlock onto 16×4 distributed RAM cells
/// (`CellFunction::DistRam16x4`: Lattice DPR16X4 / DPR16X4C).
///
/// The memory is tiled into 16-word banks of 4-bit slices. Each bank's write
/// enable is `we` qualified by the upper write-address bits; the read data is
/// a mux tree over the banks selected by the upper read-address bits. Each
/// read port of a memory is its own MemBlock (see mir_to_lir), so a register
/// file with two read ports becomes two copies sharing the write port.
#[allow(clippy::too_many_arguments)]
fn map_memblock_to_distram(
    netlist: &mut GateNetlist,
    library: &TechLibrary,
    data_width: u32,
    addr_width: u32,
    depth: u32,
    inputs: &[Vec<GateNetId>],
    outputs: &[GateNetId],
    path: &str,
    clk: GateNetId,
    init: Option<&[u64]>,
) {
    const WORDS: usize = 16;
    const BITS: usize = 4;
    const CELL_ADDR_BITS: usize = 4;

    let ram_ci = lookup_cell(library, CellFunction::DistRam16x4, "DPR16X4", 0.16);
    let inv_ci = lookup_cell(library, CellFunction::Inv, "INV", 0.1);
    let and_ci = lookup_cell(library, CellFunction::And2, "AND2", 0.2);
    let mux_ci = lookup_cell(library, CellFunction::Mux2, "MUX2", 0.3);
    let tie_low = crate::carry_chain::get_or_create_tie(netlist, false);

    let dw = data_width as usize;
    let aw = addr_width as usize;
    let banks = (depth as usize).div_ceil(WORDS);
    let empty = Vec::new();
    let raddr = inputs.first().unwrap_or(&empty);
    let waddr = inputs.get(1).unwrap_or(&empty);
    let wdata = inputs.get(2).unwrap_or(&empty);
    let we = inputs
        .get(3)
        .and_then(|v| v.first().copied())
        .unwrap_or(tie_low);
    let bit =
        |nets: &Vec<GateNetId>, i: usize| -> GateNetId { nets.get(i).copied().unwrap_or(tie_low) };

    let add_comb = |netlist: &mut GateNetlist,
                    ci: &LibraryCellInfo,
                    cell_path: String,
                    ins: Vec<GateNetId>,
                    out_name: String|
     -> GateNetId {
        let out = netlist.add_net(GateNet::new(GateNetId(0), out_name));
        let mut cell = Cell::new_comb(
            CellId(0),
            ci.name.clone(),
            library.name.clone(),
            ci.fit,
            cell_path,
            ins,
            vec![out],
        );
        cell.source_op = Some("MemBlock_DistRam".to_string());
        ci.apply_to_cell(&mut cell);
        netlist.add_cell(cell);
        out
    };

    // Per-bank write enable: we & (waddr[upper] == bank)
    let upper_bits = aw.saturating_sub(CELL_ADDR_BITS);
    let mut inverted_waddr: Vec<Option<GateNetId>> = vec![None; upper_bits];
    let mut bank_we = Vec::with_capacity(banks);
    for b in 0..banks {
        let mut sel = we;
        for (u, inverted) in inverted_waddr.iter_mut().enumerate() {
            let a = bit(waddr, CELL_ADDR_BITS + u);
            let term = if (b >> u) & 1 == 1 {
                a
            } else {
                *inverted.get_or_insert_with(|| {
                    add_comb(
                        netlist,
                        &inv_ci,
                        format!("{}.waddr_inv{}", path, u),
                        vec![a],
                        format!("{}.waddr_n{}", path, u),
                    )
                })
            };
            sel = add_comb(
                netlist,
                &and_ci,
                format!("{}.bank{}_we_and{}", path, b, u),
                vec![sel, term],
                format!("{}.bank{}_we{}", path, b, u),
            );
        }
        bank_we.push(sel);
    }

    // RAM cells. bank_data[b][i] = bank b's read data bit i.
    let mut bank_data: Vec<Vec<GateNetId>> = Vec::with_capacity(banks);
    for (b, &wre) in bank_we.iter().enumerate() {
        let mut data = Vec::with_capacity(dw);
        for s in 0..dw.div_ceil(BITS) {
            let mut ins = Vec::with_capacity(2 * BITS + 1 + CELL_ADDR_BITS);
            for i in 0..BITS {
                let d = s * BITS + i;
                ins.push(if d < dw { bit(wdata, d) } else { tie_low });
            }
            for i in 0..CELL_ADDR_BITS {
                ins.push(if i < aw { bit(waddr, i) } else { tie_low });
            }
            ins.push(wre);
            for i in 0..CELL_ADDR_BITS {
                ins.push(if i < aw { bit(raddr, i) } else { tie_low });
            }
            let outs: Vec<GateNetId> = (0..BITS)
                .map(|i| {
                    let d = s * BITS + i;
                    // A single bank drives the memory's outputs directly
                    if banks == 1 && d < dw {
                        outputs.get(d).copied().unwrap_or_else(|| {
                            netlist.add_net(GateNet::new(
                                GateNetId(0),
                                format!("{}.b{}s{}.do{}", path, b, s, i),
                            ))
                        })
                    } else {
                        netlist.add_net(GateNet::new(
                            GateNetId(0),
                            format!("{}.b{}s{}.do{}", path, b, s, i),
                        ))
                    }
                })
                .collect();
            data.extend(outs.iter().take(dw.saturating_sub(s * BITS).min(BITS)));
            let mut cell = Cell::new_seq(
                CellId(0),
                ram_ci.name.clone(),
                library.name.clone(),
                ram_ci.fit,
                format!("{}.b{}s{}", path, b, s),
                ins,
                outs,
                clk,
                None,
            );
            cell.source_op = Some("MemBlock_DistRam".to_string());
            // INITVAL bit 4 * entry + i is bit i of the cell's word `entry`.
            let initval = (0..WORDS).fold(0u64, |acc, entry| {
                let word = init
                    .and_then(|w| w.get(b * WORDS + entry))
                    .copied()
                    .unwrap_or(0);
                let nibble = if s * BITS < 64 {
                    (word >> (s * BITS)) & 0xF
                } else {
                    0
                };
                acc | (nibble << (BITS * entry))
            });
            cell.parameters
                .insert("INITVAL".to_string(), format!("0x{initval:016X}"));
            ram_ci.apply_to_cell(&mut cell);
            netlist.add_cell(cell);
        }
        bank_data.push(data);
    }
    if banks == 1 {
        return;
    }

    // Read mux tree over the banks: level u halves the candidates using
    // raddr[4 + u]. Banks past the memory's depth read as 0.
    for i in 0..dw {
        let mut level: Vec<GateNetId> = bank_data.iter().map(|d| d[i]).collect();
        let mut u = 0;
        while level.len() > 1 {
            let sel = bit(raddr, CELL_ADDR_BITS + u);
            let last_level = level.len() <= 2;
            level = level
                .chunks(2)
                .enumerate()
                .map(|(k, pair)| {
                    let d0 = pair[0];
                    let d1 = pair.get(1).copied().unwrap_or(tie_low);
                    let out = if last_level {
                        outputs[i]
                    } else {
                        netlist.add_net(GateNet::new(
                            GateNetId(0),
                            format!("{}.rmux{}_{}_{}", path, i, u, k),
                        ))
                    };
                    let mut cell = Cell::new_comb(
                        CellId(0),
                        mux_ci.name.clone(),
                        library.name.clone(),
                        mux_ci.fit,
                        format!("{}.rmux{}_{}_{}", path, i, u, k),
                        mux2_inputs(&mux_ci, sel, d0, d1),
                        vec![out],
                    );
                    cell.source_op = Some("MemBlock_DistRam".to_string());
                    mux_ci.apply_to_cell(&mut cell);
                    netlist.add_cell(cell);
                    out
                })
                .collect();
            u += 1;
        }
    }
}

/// Decompose a MemBlock into DFFs + write-select muxes + a read mux chain.
///
/// Used when the library has no block-RAM cell (e.g. the generic ASIC library
/// EC synthesizes with). Semantics match the RamBlock primitive and MIR:
/// write on the clock edge when WE is high, transparent combinational read.
///
/// Per word w (write side):   word_we_w = WE & (waddr == w)
///                            din[b]    = word_we_w ? wdata[b] : q_w[b]
///                            q_w[b]    = DFF(din[b])  (no reset — memories
///                                        start at zero like RamBlock state)
/// Read side (per bit b):     rdata[b]  = priority chain of
///                            (raddr == w) ? q_w[b] : previous
#[allow(clippy::too_many_arguments)]
fn decompose_memblock_to_dffs(
    netlist: &mut GateNetlist,
    library: &TechLibrary,
    data_width: u32,
    addr_width: u32,
    depth: u32,
    has_write: bool,
    inputs: &[Vec<GateNetId>],
    outputs: &[GateNetId],
    path: &str,
    clk: GateNetId,
    init: Option<&[u64]>,
) {
    let inv_ci = lookup_cell(library, CellFunction::Inv, "INV", 0.1);
    let and_ci = lookup_cell(library, CellFunction::And2, "AND2", 0.2);
    let mux_ci = lookup_cell(library, CellFunction::Mux2, "MUX2", 0.3);
    let buf_ci = lookup_cell(library, CellFunction::Buf, "BUF", 0.1);
    let dff_ci = lookup_cell(library, CellFunction::Dff, "DFF", 0.5);

    let tie_low = {
        if let Some(id) = netlist
            .get_net_id("tie_low")
            .or_else(|| netlist.get_net_id("gnd"))
        {
            id
        } else {
            let id = netlist.add_net(GateNet::new(GateNetId(0), "tie_low".to_string()));
            let tie_info = lookup_cell(library, CellFunction::TieLow, "TIE_LOW", 0.01);
            let mut cell = Cell::new_comb(
                CellId(0),
                tie_info.name.clone(),
                library.name.clone(),
                tie_info.fit,
                "tie_low".to_string(),
                vec![],
                vec![id],
            );
            tie_info.apply_to_cell(&mut cell);
            netlist.add_cell(cell);
            id
        }
    };

    let aw = addr_width as usize;
    let dw = data_width as usize;
    let empty = Vec::new();
    let raddr_nets = inputs.first().unwrap_or(&empty);
    let waddr_nets = if has_write && inputs.len() > 1 {
        &inputs[1]
    } else {
        &empty
    };
    let wdata_nets = if has_write && inputs.len() > 2 {
        &inputs[2]
    } else {
        &empty
    };
    let we_net = if has_write && inputs.len() > 3 {
        inputs[3].first().copied().unwrap_or(tie_low)
    } else {
        tie_low
    };

    // Small builders for one-output comb cells
    let mk_cell = |netlist: &mut GateNetlist,
                   ci: &LibraryCellInfo,
                   cell_inputs: Vec<GateNetId>,
                   out: GateNetId,
                   cell_path: String| {
        let mut cell = Cell::new_comb(
            CellId(0),
            ci.name.clone(),
            library.name.clone(),
            ci.fit,
            cell_path,
            cell_inputs,
            vec![out],
        );
        cell.source_op = Some("MemBlock_DffFallback".to_string());
        ci.apply_to_cell(&mut cell);
        netlist.add_cell(cell);
    };

    // Inverted address bits (shared across word decoders)
    let invert_bits =
        |netlist: &mut GateNetlist, bits: &[GateNetId], tag: &str| -> Vec<GateNetId> {
            (0..aw)
                .map(|i| {
                    let src = bits.get(i).copied().unwrap_or(tie_low);
                    let out = netlist.add_net(GateNet::new(
                        GateNetId(0),
                        format!("{}_{}inv{}", path, tag, i),
                    ));
                    mk_cell(
                        netlist,
                        &inv_ci,
                        vec![src],
                        out,
                        format!("{}_{}inv{}", path, tag, i),
                    );
                    out
                })
                .collect()
        };
    let waddr_inv = if has_write {
        invert_bits(netlist, waddr_nets, "wa")
    } else {
        Vec::new()
    };
    let raddr_inv = invert_bits(netlist, raddr_nets, "ra");

    // Decode `addr == word` as an AND chain seeded with `seed`
    let mk_decode = |netlist: &mut GateNetlist,
                     word: usize,
                     bits: &[GateNetId],
                     inv_bits: &[GateNetId],
                     seed: Option<GateNetId>,
                     tag: &str|
     -> GateNetId {
        let mut acc = seed;
        for i in 0..aw {
            let term = if (word >> i) & 1 == 1 {
                bits.get(i).copied().unwrap_or(tie_low)
            } else {
                inv_bits.get(i).copied().unwrap_or(tie_low)
            };
            acc = Some(match acc {
                None => term,
                Some(prev) => {
                    let out = netlist.add_net(GateNet::new(
                        GateNetId(0),
                        format!("{}_w{}_{}sel{}", path, word, tag, i),
                    ));
                    mk_cell(
                        netlist,
                        &and_ci,
                        vec![prev, term],
                        out,
                        format!("{}_w{}_{}and{}", path, word, tag, i),
                    );
                    out
                }
            });
        }
        acc.unwrap_or(tie_low)
    };

    // A read-port replica (`<memory>_rd<n>`, see mir_to_lir's handle_memory_read)
    // shares the write port of its primary block, so in registers it can share the
    // primary's storage and add only a read mux.
    let shared_storage: Option<Vec<Vec<GateNetId>>> = path
        .rsplit_once("_rd")
        .filter(|(_, n)| !n.is_empty() && n.bytes().all(|b| b.is_ascii_digit()))
        .and_then(|(primary, _)| {
            (0..depth as usize)
                .map(|word| {
                    (0..dw)
                        .map(|b| netlist.get_net_id(&format!("{}_w{}_q{}", primary, word, b)))
                        .collect::<Option<Vec<_>>>()
                })
                .collect::<Option<Vec<_>>>()
        });

    // Storage: depth × data_width DFFs with write-select muxes
    let mut q_nets: Vec<Vec<GateNetId>> = Vec::with_capacity(depth as usize);
    if let Some(shared) = shared_storage {
        q_nets = shared;
    }
    for word in 0..depth as usize {
        if q_nets.len() == depth as usize {
            break;
        }
        let word_we = if has_write {
            mk_decode(netlist, word, waddr_nets, &waddr_inv, Some(we_net), "w")
        } else {
            tie_low
        };
        let mut word_q = Vec::with_capacity(dw);
        for b in 0..dw {
            let starts_high = init
                .and_then(|words| words.get(word))
                .is_some_and(|&w| b < 64 && (w >> b) & 1 == 1);
            // A memory that is never written holds its initial contents: they
            // are constants, which synthesis folds into the read logic.
            if !has_write && init.is_some() {
                word_q.push(if starts_high {
                    get_or_create_tie_high(netlist, library)
                } else {
                    tie_low
                });
                continue;
            }
            let q = netlist.add_net(GateNet::new(
                GateNetId(0),
                format!("{}_w{}_q{}", path, word, b),
            ));
            let din = if has_write {
                let wbit = wdata_nets.get(b).copied().unwrap_or(tie_low);
                let din = netlist.add_net(GateNet::new(
                    GateNetId(0),
                    format!("{}_w{}_din{}", path, word, b),
                ));
                // MUX2 [sel, d0, d1]: word_we ? wdata : q (hold)
                mk_cell(
                    netlist,
                    &mux_ci,
                    mux2_inputs(&mux_ci, word_we, q, wbit),
                    din,
                    format!("{}_w{}_wmux{}", path, word, b),
                );
                din
            } else {
                q
            };
            let mut dff = Cell::new_seq(
                CellId(0),
                dff_ci.name.clone(),
                library.name.clone(),
                dff_ci.fit,
                format!("{}_w{}_dff{}", path, word, b),
                vec![din],
                vec![q],
                clk,
                None,
            );
            dff.source_op = Some("MemBlock_DffFallback".to_string());
            if starts_high {
                dff.parameters.insert(
                    crate::gate_netlist::FF_INIT_PARAM.to_string(),
                    "1".to_string(),
                );
            }
            dff_ci.apply_to_cell(&mut dff);
            netlist.add_cell(dff);
            word_q.push(q);
        }
        q_nets.push(word_q);
    }

    // Read side: decode each word's read select once, then build a priority
    // mux chain per output bit. The final stage drives the existing rdata
    // output net directly.
    let read_sel: Vec<GateNetId> = (1..depth as usize)
        .map(|word| mk_decode(netlist, word, raddr_nets, &raddr_inv, None, "r"))
        .collect();
    for (b, &out_net) in outputs.iter().enumerate().take(dw) {
        if depth == 1 {
            mk_cell(
                netlist,
                &buf_ci,
                vec![q_nets[0][b]],
                out_net,
                format!("{}_rbuf{}", path, b),
            );
            continue;
        }
        let mut acc = q_nets[0][b];
        for word in 1..depth as usize {
            let stage_out = if word == depth as usize - 1 {
                out_net
            } else {
                netlist.add_net(GateNet::new(
                    GateNetId(0),
                    format!("{}_r{}_b{}", path, word, b),
                ))
            };
            // MUX2 [sel, d0, d1]: (raddr == word) ? q_word : acc
            mk_cell(
                netlist,
                &mux_ci,
                mux2_inputs(&mux_ci, read_sel[word - 1], acc, q_nets[word][b]),
                stage_out,
                format!("{}_rmux_w{}_b{}", path, word, b),
            );
            acc = stage_out;
        }
    }
}

/// Merge physical LIR nodes (NCL, BRAM) into an AIG-derived GateNetlist.
///
/// Physical ops were represented as pseudo-inputs (`__phys_*`) during
/// LIR→AIG conversion. This function creates the actual gate cells for
/// those ops and wires them into the existing netlist:
///
/// 1. Physical cell outputs drive `__phys_*` nets (removing them from inputs)
/// 2. Physical cell inputs connect to existing LIR primary input nets or
///    other physical node output nets
/// 3. For NCL ops without native THmn gates, C-element macros are used
fn merge_physical_nodes_into_netlist(
    netlist: &mut GateNetlist,
    lir: &Lir,
    physical_indices: &[usize],
    library: &TechLibrary,
) {
    use std::collections::HashSet;

    // Build set of signal IDs that are physical node outputs
    let phys_output_signals: HashSet<u32> = physical_indices
        .iter()
        .map(|&idx| lir.nodes[idx].output.0)
        .collect();

    // Build set of LIR input signal IDs
    let lir_input_signals: HashSet<u32> = lir.inputs.iter().map(|s| s.0).collect();

    // Values of constant-driven signals (folded into carry-chain truth tables)
    let constant_signals: std::collections::HashMap<u32, u64> = lir
        .nodes
        .iter()
        .filter_map(|n| match n.op {
            LirOp::Constant { value, .. } => Some((n.output.0, value)),
            _ => None,
        })
        .collect();

    // Resolve a LIR signal to a vector of GateNetIds in the netlist.
    // Creates missing input nets on the fly (module inputs consumed only by
    // physical nodes aren't in the AIG-derived netlist).
    let resolve_signal = |netlist: &mut GateNetlist, sig_id: LirSignalId| -> Vec<GateNetId> {
        let sig = &lir.signals[sig_id.0 as usize];
        let is_lir_input = lir_input_signals.contains(&sig_id.0);
        (0..sig.width)
            .map(|bit| {
                // Physical nodes no longer use __phys_ prefix; use actual signal name.
                // The is_physical flag on AIG nodes replaces the prefix convention.
                let name = if sig.width == 1 {
                    sig.name.clone()
                } else {
                    format!("{}[{}]", sig.name, bit)
                };
                // AUDIT-2 #1 FIX: resolve to the FIRST net with this name,
                // not net_map's last-registered one. When a physical net is
                // also a module output, aig_writer creates a SECOND net with
                // the same name plus an `aig.phys_buf_*` buffer between them
                // (pseudo-input → output). The AIG logic cone reads the
                // FIRST (pseudo-input) net; resolving physical drivers to
                // the last-registered duplicate left the cone's net undriven
                // — in the non-flattened CLI path every NCL output read NULL
                // (the hierarchical flatten masked this by merging
                // same-named nets, turning the buf into a self-loop).
                // Driving the FIRST net lets phys_buf carry the value to
                // the output copy: pseudo-input → buf → output.
                if let Some(id) = netlist.nets.iter().find(|n| n.name == name).map(|n| n.id) {
                    id
                } else if is_lir_input {
                    // Module input not in AIG netlist (only consumed by physical nodes).
                    // Create it as a primary input.
                    netlist.add_input(name)
                } else {
                    // Internal signal — create as internal net
                    netlist.add_net(GateNet::new(GateNetId(0), name))
                }
            })
            .collect()
    };

    // Look up library cells
    let has_th22 = library.find_best_cell(&CellFunction::Th22).is_some();
    let has_th12 = library.find_best_cell(&CellFunction::Th12).is_some();

    let buf_cell = lookup_cell(library, CellFunction::Buf, "BUF", 0.1);
    let and2_cell = lookup_cell(library, CellFunction::And2, "AND2", 0.2);
    let or2_cell = lookup_cell(library, CellFunction::Or2, "OR2", 0.2);
    let th22_cell = lookup_cell(library, CellFunction::Th22, "TH22", 0.6);
    let th12_cell = lookup_cell(library, CellFunction::Th12, "TH12", 0.5);

    // Helper: get or create tie-low net
    let tie_low_info = lookup_cell(library, CellFunction::TieLow, "TIE_LOW", 0.01);
    let tie_high_info = lookup_cell(library, CellFunction::TieHigh, "TIE_HIGH", 0.01);

    let get_tie_low = |netlist: &mut GateNetlist| -> GateNetId {
        if let Some(id) = netlist.get_net_id("tie_low") {
            return id;
        }
        if let Some(id) = netlist.get_net_id("gnd") {
            return id;
        }
        let id = netlist.add_net(GateNet::new(GateNetId(0), "tie_low".to_string()));
        let mut cell = Cell::new_comb(
            CellId(0),
            tie_low_info.name.clone(),
            library.name.clone(),
            tie_low_info.fit,
            "tie_low".to_string(),
            vec![],
            vec![id],
        );
        tie_low_info.apply_to_cell(&mut cell);
        netlist.add_cell(cell);
        id
    };

    let get_tie_high = |netlist: &mut GateNetlist| -> GateNetId {
        if let Some(id) = netlist.get_net_id("tie_high") {
            return id;
        }
        if let Some(id) = netlist.get_net_id("vdd") {
            return id;
        }
        let id = netlist.add_net(GateNet::new(GateNetId(0), "tie_high".to_string()));
        let mut cell = Cell::new_comb(
            CellId(0),
            tie_high_info.name.clone(),
            library.name.clone(),
            tie_high_info.fit,
            "tie_high".to_string(),
            vec![],
            vec![id],
        );
        tie_high_info.apply_to_cell(&mut cell);
        netlist.add_cell(cell);
        id
    };

    // Helper: create a TH22 or C-element macro
    // C-element: Q = (A & B) | (Q & (A | B))
    let make_th22_or_celement =
        |netlist: &mut GateNetlist, a: GateNetId, b: GateNetId, q: GateNetId, path: &str| {
            if has_th22 {
                let mut cell = Cell::new_comb(
                    CellId(0),
                    th22_cell.name.clone(),
                    library.name.clone(),
                    th22_cell.fit,
                    path.to_string(),
                    vec![a, b],
                    vec![q],
                );
                th22_cell.apply_to_cell(&mut cell);
                netlist.add_cell(cell);
            } else {
                // C-element macro: 2 AND2 + 2 OR2
                let ab_and =
                    netlist.add_net(GateNet::new(GateNetId(0), format!("{}.ab_and", path)));
                let ab_or = netlist.add_net(GateNet::new(GateNetId(0), format!("{}.ab_or", path)));
                let q_and_or =
                    netlist.add_net(GateNet::new(GateNetId(0), format!("{}.q_and_or", path)));

                // ab_and = A & B
                let mut c1 = Cell::new_comb(
                    CellId(0),
                    and2_cell.name.clone(),
                    library.name.clone(),
                    and2_cell.fit,
                    format!("{}.and1", path),
                    vec![a, b],
                    vec![ab_and],
                );
                c1.source_op = Some("C-element_AND_AB".to_string());
                and2_cell.apply_to_cell(&mut c1);
                netlist.add_cell(c1);

                // ab_or = A | B
                let mut c2 = Cell::new_comb(
                    CellId(0),
                    or2_cell.name.clone(),
                    library.name.clone(),
                    or2_cell.fit,
                    format!("{}.or1", path),
                    vec![a, b],
                    vec![ab_or],
                );
                c2.source_op = Some("C-element_OR_AB".to_string());
                or2_cell.apply_to_cell(&mut c2);
                netlist.add_cell(c2);

                // q_and_or = Q & (A | B) — feedback from output
                let mut c3 = Cell::new_comb(
                    CellId(0),
                    and2_cell.name.clone(),
                    library.name.clone(),
                    and2_cell.fit,
                    format!("{}.and2", path),
                    vec![q, ab_or],
                    vec![q_and_or],
                );
                c3.source_op = Some("C-element_AND_Q_OR".to_string());
                and2_cell.apply_to_cell(&mut c3);
                netlist.add_cell(c3);

                // Q = (A & B) | (Q & (A | B))
                let mut c4 = Cell::new_comb(
                    CellId(0),
                    or2_cell.name.clone(),
                    library.name.clone(),
                    or2_cell.fit,
                    format!("{}.or2", path),
                    vec![ab_and, q_and_or],
                    vec![q],
                );
                c4.source_op = Some("C-element_OUTPUT".to_string());
                or2_cell.apply_to_cell(&mut c4);
                netlist.add_cell(c4);
            }
        };

    // Helper: create a TH12 or OR2 (TH12 = 1-of-2 threshold = OR in combinational)
    let make_th12_or_or2 =
        |netlist: &mut GateNetlist, a: GateNetId, b: GateNetId, q: GateNetId, path: &str| {
            if has_th12 {
                let mut cell = Cell::new_comb(
                    CellId(0),
                    th12_cell.name.clone(),
                    library.name.clone(),
                    th12_cell.fit,
                    path.to_string(),
                    vec![a, b],
                    vec![q],
                );
                th12_cell.apply_to_cell(&mut cell);
                netlist.add_cell(cell);
            } else {
                let mut cell = Cell::new_comb(
                    CellId(0),
                    or2_cell.name.clone(),
                    library.name.clone(),
                    or2_cell.fit,
                    path.to_string(),
                    vec![a, b],
                    vec![q],
                );
                or2_cell.apply_to_cell(&mut cell);
                netlist.add_cell(cell);
            }
        };

    // Process each physical node
    for &idx in physical_indices {
        let node = &lir.nodes[idx];
        let path = &node.path;

        // Resolve input signals to net ID vectors. A carry chain folds
        // constant operands into its truth tables, so those need no nets.
        let folds_constants = crate::carry_chain::is_carry_chain_op(&node.op, 0);
        let input_nets: Vec<Vec<GateNetId>> = node
            .inputs
            .iter()
            .map(|&sig_id| {
                if folds_constants && constant_signals.contains_key(&sig_id.0) {
                    Vec::new()
                } else {
                    resolve_signal(netlist, sig_id)
                }
            })
            .collect();

        // Resolve output signal to net ID vector (__phys_* nets)
        let output_nets = resolve_signal(netlist, node.output);

        match &node.op {
            LirOp::NclDecode { width } => {
                // Dual-rail → single-rail: take t-rail (first half of input)
                for i in 0..*width as usize {
                    let in_t = input_nets
                        .first()
                        .and_then(|v| v.get(i))
                        .copied()
                        .unwrap_or(GateNetId(0));
                    let out = output_nets.get(i).copied().unwrap_or(GateNetId(0));
                    let mut cell = Cell::new_comb(
                        CellId(0),
                        buf_cell.name.clone(),
                        library.name.clone(),
                        buf_cell.fit,
                        format!("{}.dec{}", path, i),
                        vec![in_t],
                        vec![out],
                    );
                    cell.source_op = Some("NclDecode".to_string());
                    buf_cell.apply_to_cell(&mut cell);
                    netlist.add_cell(cell);
                }
            }

            LirOp::NclEncode { width } => {
                // Boundary NCL: t-rail = value, f-rail = NOT(value).
                // The AIG already produces y_f[i] = NOT(value) as a module output.
                // We derive the t-rail as: y_t[i] = INV(y_f[i]).
                let out_sig = &lir.signals[node.output.0 as usize];
                let f_signal_base = if out_sig.name.ends_with("_t") {
                    format!("{}_f", &out_sig.name[..out_sig.name.len() - 2])
                } else {
                    format!("{}_f", out_sig.name)
                };

                let inv_cell = lookup_cell(library, CellFunction::Inv, "INV_X1", 0.1);

                for i in 0..*width as usize {
                    let out_t = output_nets.get(i).copied().unwrap_or(GateNetId(0));
                    if out_t.0 == 0 {
                        continue;
                    }

                    let f_net_name = if *width == 1 {
                        f_signal_base.clone()
                    } else {
                        format!("{}[{}]", f_signal_base, i)
                    };

                    if let Some(f_net_id) = netlist.get_net_id(&f_net_name) {
                        // y_t[i] = INV(y_f[i])
                        let mut cell = Cell::new_comb(
                            CellId(0),
                            inv_cell.name.clone(),
                            library.name.clone(),
                            inv_cell.fit,
                            format!("{}.enc_t{}", path, i),
                            vec![f_net_id],
                            vec![out_t],
                        );
                        cell.source_op = Some("NclEncode_T".to_string());
                        inv_cell.apply_to_cell(&mut cell);
                        netlist.add_cell(cell);
                    }
                }
            }

            LirOp::NclAnd { width } => {
                // inputs[0], inputs[1] are dual-rail: [t0,f0,t1,f1,...] each
                for i in 0..*width as usize {
                    let a_t = input_nets
                        .first()
                        .and_then(|v| v.get(i * 2))
                        .copied()
                        .unwrap_or(GateNetId(0));
                    let a_f = input_nets
                        .first()
                        .and_then(|v| v.get(i * 2 + 1))
                        .copied()
                        .unwrap_or(GateNetId(0));
                    let b_t = input_nets
                        .get(1)
                        .and_then(|v| v.get(i * 2))
                        .copied()
                        .unwrap_or(GateNetId(0));
                    let b_f = input_nets
                        .get(1)
                        .and_then(|v| v.get(i * 2 + 1))
                        .copied()
                        .unwrap_or(GateNetId(0));
                    let out_t = output_nets.get(i * 2).copied().unwrap_or(GateNetId(0));
                    let out_f = output_nets.get(i * 2 + 1).copied().unwrap_or(GateNetId(0));

                    // True rail: TH22(a_t, b_t)
                    make_th22_or_celement(
                        netlist,
                        a_t,
                        b_t,
                        out_t,
                        &format!("{}.and_t{}", path, i),
                    );
                    // False rail: TH12(a_f, b_f)
                    make_th12_or_or2(netlist, a_f, b_f, out_f, &format!("{}.and_f{}", path, i));
                }
            }

            LirOp::NclOr { width } => {
                for i in 0..*width as usize {
                    let a_t = input_nets
                        .first()
                        .and_then(|v| v.get(i * 2))
                        .copied()
                        .unwrap_or(GateNetId(0));
                    let a_f = input_nets
                        .first()
                        .and_then(|v| v.get(i * 2 + 1))
                        .copied()
                        .unwrap_or(GateNetId(0));
                    let b_t = input_nets
                        .get(1)
                        .and_then(|v| v.get(i * 2))
                        .copied()
                        .unwrap_or(GateNetId(0));
                    let b_f = input_nets
                        .get(1)
                        .and_then(|v| v.get(i * 2 + 1))
                        .copied()
                        .unwrap_or(GateNetId(0));
                    let out_t = output_nets.get(i * 2).copied().unwrap_or(GateNetId(0));
                    let out_f = output_nets.get(i * 2 + 1).copied().unwrap_or(GateNetId(0));

                    // True rail: TH12(a_t, b_t) — either true rail
                    make_th12_or_or2(netlist, a_t, b_t, out_t, &format!("{}.or_t{}", path, i));
                    // False rail: TH22(a_f, b_f) — both false rails
                    make_th22_or_celement(netlist, a_f, b_f, out_f, &format!("{}.or_f{}", path, i));
                }
            }

            LirOp::NclXor { width } => {
                // XOR(a,b)_t = TH22(TH12(a_t, b_f), TH12(a_f, b_t))
                // XOR(a,b)_f = TH22(TH12(a_t, b_t), TH12(a_f, b_f))
                for i in 0..*width as usize {
                    let a_t = input_nets
                        .first()
                        .and_then(|v| v.get(i * 2))
                        .copied()
                        .unwrap_or(GateNetId(0));
                    let a_f = input_nets
                        .first()
                        .and_then(|v| v.get(i * 2 + 1))
                        .copied()
                        .unwrap_or(GateNetId(0));
                    let b_t = input_nets
                        .get(1)
                        .and_then(|v| v.get(i * 2))
                        .copied()
                        .unwrap_or(GateNetId(0));
                    let b_f = input_nets
                        .get(1)
                        .and_then(|v| v.get(i * 2 + 1))
                        .copied()
                        .unwrap_or(GateNetId(0));
                    let out_t = output_nets.get(i * 2).copied().unwrap_or(GateNetId(0));
                    let out_f = output_nets.get(i * 2 + 1).copied().unwrap_or(GateNetId(0));

                    // Intermediate nets
                    let at_bf = netlist.add_net(GateNet::new(
                        GateNetId(0),
                        format!("{}.xor_at_bf{}", path, i),
                    ));
                    let af_bt = netlist.add_net(GateNet::new(
                        GateNetId(0),
                        format!("{}.xor_af_bt{}", path, i),
                    ));
                    let at_bt = netlist.add_net(GateNet::new(
                        GateNetId(0),
                        format!("{}.xor_at_bt{}", path, i),
                    ));
                    let af_bf = netlist.add_net(GateNet::new(
                        GateNetId(0),
                        format!("{}.xor_af_bf{}", path, i),
                    ));

                    // TH12(a_t, b_f) → at_bf
                    make_th12_or_or2(
                        netlist,
                        a_t,
                        b_f,
                        at_bf,
                        &format!("{}.xor_th12_1_{}", path, i),
                    );
                    // TH12(a_f, b_t) → af_bt
                    make_th12_or_or2(
                        netlist,
                        a_f,
                        b_t,
                        af_bt,
                        &format!("{}.xor_th12_2_{}", path, i),
                    );
                    // TH22(at_bf, af_bt) → out_t
                    make_th22_or_celement(
                        netlist,
                        at_bf,
                        af_bt,
                        out_t,
                        &format!("{}.xor_t{}", path, i),
                    );

                    // TH12(a_t, b_t) → at_bt
                    make_th12_or_or2(
                        netlist,
                        a_t,
                        b_t,
                        at_bt,
                        &format!("{}.xor_th12_3_{}", path, i),
                    );
                    // TH12(a_f, b_f) → af_bf
                    make_th12_or_or2(
                        netlist,
                        a_f,
                        b_f,
                        af_bf,
                        &format!("{}.xor_th12_4_{}", path, i),
                    );
                    // TH22(at_bt, af_bf) → out_f
                    make_th22_or_celement(
                        netlist,
                        at_bt,
                        af_bf,
                        out_f,
                        &format!("{}.xor_f{}", path, i),
                    );
                }
            }

            LirOp::NclNot { width } => {
                // NOT swaps t and f rails
                for i in 0..*width as usize {
                    let in_t = input_nets
                        .first()
                        .and_then(|v| v.get(i * 2))
                        .copied()
                        .unwrap_or(GateNetId(0));
                    let in_f = input_nets
                        .first()
                        .and_then(|v| v.get(i * 2 + 1))
                        .copied()
                        .unwrap_or(GateNetId(0));
                    let out_t = output_nets.get(i * 2).copied().unwrap_or(GateNetId(0));
                    let out_f = output_nets.get(i * 2 + 1).copied().unwrap_or(GateNetId(0));

                    // out_t = in_f, out_f = in_t (swap rails)
                    let mut ct = Cell::new_comb(
                        CellId(0),
                        buf_cell.name.clone(),
                        library.name.clone(),
                        buf_cell.fit,
                        format!("{}.not_t{}", path, i),
                        vec![in_f],
                        vec![out_t],
                    );
                    ct.source_op = Some("NclNot_T".to_string());
                    buf_cell.apply_to_cell(&mut ct);
                    netlist.add_cell(ct);

                    let mut cf = Cell::new_comb(
                        CellId(0),
                        buf_cell.name.clone(),
                        library.name.clone(),
                        buf_cell.fit,
                        format!("{}.not_f{}", path, i),
                        vec![in_t],
                        vec![out_f],
                    );
                    cf.source_op = Some("NclNot_F".to_string());
                    buf_cell.apply_to_cell(&mut cf);
                    netlist.add_cell(cf);
                }
            }

            LirOp::NclAdd { width } => {
                // NCL ripple-carry adder
                // inputs: [a_t, a_f, b_t, b_f] as 4 separate signals each of `width` bits
                let tie_low = get_tie_low(netlist);
                let tie_high = get_tie_high(netlist);
                let mut carry_t = tie_low;
                let mut carry_f = tie_high;

                for i in 0..*width as usize {
                    let a_t = input_nets
                        .first()
                        .and_then(|v| v.get(i))
                        .copied()
                        .unwrap_or(tie_low);
                    let a_f = input_nets
                        .get(1)
                        .and_then(|v| v.get(i))
                        .copied()
                        .unwrap_or(tie_high);
                    let b_t = input_nets
                        .get(2)
                        .and_then(|v| v.get(i))
                        .copied()
                        .unwrap_or(tie_low);
                    let b_f = input_nets
                        .get(3)
                        .and_then(|v| v.get(i))
                        .copied()
                        .unwrap_or(tie_high);

                    let sum_t = output_nets.get(i * 2).copied().unwrap_or(GateNetId(0));
                    let sum_f = output_nets.get(i * 2 + 1).copied().unwrap_or(GateNetId(0));

                    // XOR(a, b) — intermediate dual-rail
                    let xor_ab_t = netlist.add_net(GateNet::new(
                        GateNetId(0),
                        format!("{}.add_xor_ab_t{}", path, i),
                    ));
                    let xor_ab_f = netlist.add_net(GateNet::new(
                        GateNetId(0),
                        format!("{}.add_xor_ab_f{}", path, i),
                    ));

                    // Build XOR(a,b) inline
                    let at_bf = netlist.add_net(GateNet::new(
                        GateNetId(0),
                        format!("{}.add_at_bf{}", path, i),
                    ));
                    let af_bt = netlist.add_net(GateNet::new(
                        GateNetId(0),
                        format!("{}.add_af_bt{}", path, i),
                    ));
                    let at_bt = netlist.add_net(GateNet::new(
                        GateNetId(0),
                        format!("{}.add_at_bt{}", path, i),
                    ));
                    let af_bf = netlist.add_net(GateNet::new(
                        GateNetId(0),
                        format!("{}.add_af_bf{}", path, i),
                    ));

                    make_th12_or_or2(
                        netlist,
                        a_t,
                        b_f,
                        at_bf,
                        &format!("{}.add_xor1_{}", path, i),
                    );
                    make_th12_or_or2(
                        netlist,
                        a_f,
                        b_t,
                        af_bt,
                        &format!("{}.add_xor2_{}", path, i),
                    );
                    make_th22_or_celement(
                        netlist,
                        at_bf,
                        af_bt,
                        xor_ab_t,
                        &format!("{}.add_xorab_t{}", path, i),
                    );
                    make_th12_or_or2(
                        netlist,
                        a_t,
                        b_t,
                        at_bt,
                        &format!("{}.add_xor3_{}", path, i),
                    );
                    make_th12_or_or2(
                        netlist,
                        a_f,
                        b_f,
                        af_bf,
                        &format!("{}.add_xor4_{}", path, i),
                    );
                    make_th22_or_celement(
                        netlist,
                        at_bt,
                        af_bf,
                        xor_ab_f,
                        &format!("{}.add_xorab_f{}", path, i),
                    );

                    // XOR(xor_ab, carry) → sum
                    let xc_t_bf = netlist.add_net(GateNet::new(
                        GateNetId(0),
                        format!("{}.add_xc_t_bf{}", path, i),
                    ));
                    let xc_f_bt = netlist.add_net(GateNet::new(
                        GateNetId(0),
                        format!("{}.add_xc_f_bt{}", path, i),
                    ));
                    let xc_t_bt = netlist.add_net(GateNet::new(
                        GateNetId(0),
                        format!("{}.add_xc_t_bt{}", path, i),
                    ));
                    let xc_f_bf = netlist.add_net(GateNet::new(
                        GateNetId(0),
                        format!("{}.add_xc_f_bf{}", path, i),
                    ));

                    make_th12_or_or2(
                        netlist,
                        xor_ab_t,
                        carry_f,
                        xc_t_bf,
                        &format!("{}.add_sxor1_{}", path, i),
                    );
                    make_th12_or_or2(
                        netlist,
                        xor_ab_f,
                        carry_t,
                        xc_f_bt,
                        &format!("{}.add_sxor2_{}", path, i),
                    );
                    make_th22_or_celement(
                        netlist,
                        xc_t_bf,
                        xc_f_bt,
                        sum_t,
                        &format!("{}.add_sum_t{}", path, i),
                    );
                    make_th12_or_or2(
                        netlist,
                        xor_ab_t,
                        carry_t,
                        xc_t_bt,
                        &format!("{}.add_sxor3_{}", path, i),
                    );
                    make_th12_or_or2(
                        netlist,
                        xor_ab_f,
                        carry_f,
                        xc_f_bf,
                        &format!("{}.add_sxor4_{}", path, i),
                    );
                    make_th22_or_celement(
                        netlist,
                        xc_t_bt,
                        xc_f_bf,
                        sum_f,
                        &format!("{}.add_sum_f{}", path, i),
                    );

                    // Carry: majority(a, b, cin) in dual-rail
                    // cout_t = TH22(a_t,b_t) | TH22(a_t,cin_t) | TH22(b_t,cin_t)
                    // cout_f = TH22(a_f,b_f) | TH22(a_f,cin_f) | TH22(b_f,cin_f)
                    if i < (*width as usize) - 1 {
                        let new_carry_t = netlist.add_net(GateNet::new(
                            GateNetId(0),
                            format!("{}.add_carry_t{}", path, i),
                        ));
                        let new_carry_f = netlist.add_net(GateNet::new(
                            GateNetId(0),
                            format!("{}.add_carry_f{}", path, i),
                        ));

                        let ab_t = netlist.add_net(GateNet::new(
                            GateNetId(0),
                            format!("{}.add_maj_ab_t{}", path, i),
                        ));
                        let ac_t = netlist.add_net(GateNet::new(
                            GateNetId(0),
                            format!("{}.add_maj_ac_t{}", path, i),
                        ));
                        let bc_t = netlist.add_net(GateNet::new(
                            GateNetId(0),
                            format!("{}.add_maj_bc_t{}", path, i),
                        ));
                        make_th22_or_celement(
                            netlist,
                            a_t,
                            b_t,
                            ab_t,
                            &format!("{}.add_maj_ab_t{}", path, i),
                        );
                        make_th22_or_celement(
                            netlist,
                            a_t,
                            carry_t,
                            ac_t,
                            &format!("{}.add_maj_ac_t{}", path, i),
                        );
                        make_th22_or_celement(
                            netlist,
                            b_t,
                            carry_t,
                            bc_t,
                            &format!("{}.add_maj_bc_t{}", path, i),
                        );
                        // OR3 via two OR2: (ab | ac) | bc
                        let ab_ac_t = netlist.add_net(GateNet::new(
                            GateNetId(0),
                            format!("{}.add_maj_abac_t{}", path, i),
                        ));
                        make_th12_or_or2(
                            netlist,
                            ab_t,
                            ac_t,
                            ab_ac_t,
                            &format!("{}.add_maj_or1_t{}", path, i),
                        );
                        make_th12_or_or2(
                            netlist,
                            ab_ac_t,
                            bc_t,
                            new_carry_t,
                            &format!("{}.add_maj_or2_t{}", path, i),
                        );

                        let ab_f = netlist.add_net(GateNet::new(
                            GateNetId(0),
                            format!("{}.add_maj_ab_f{}", path, i),
                        ));
                        let ac_f = netlist.add_net(GateNet::new(
                            GateNetId(0),
                            format!("{}.add_maj_ac_f{}", path, i),
                        ));
                        let bc_f = netlist.add_net(GateNet::new(
                            GateNetId(0),
                            format!("{}.add_maj_bc_f{}", path, i),
                        ));
                        make_th22_or_celement(
                            netlist,
                            a_f,
                            b_f,
                            ab_f,
                            &format!("{}.add_maj_ab_f{}", path, i),
                        );
                        make_th22_or_celement(
                            netlist,
                            a_f,
                            carry_f,
                            ac_f,
                            &format!("{}.add_maj_ac_f{}", path, i),
                        );
                        make_th22_or_celement(
                            netlist,
                            b_f,
                            carry_f,
                            bc_f,
                            &format!("{}.add_maj_bc_f{}", path, i),
                        );
                        let ab_ac_f = netlist.add_net(GateNet::new(
                            GateNetId(0),
                            format!("{}.add_maj_abac_f{}", path, i),
                        ));
                        make_th12_or_or2(
                            netlist,
                            ab_f,
                            ac_f,
                            ab_ac_f,
                            &format!("{}.add_maj_or1_f{}", path, i),
                        );
                        make_th12_or_or2(
                            netlist,
                            ab_ac_f,
                            bc_f,
                            new_carry_f,
                            &format!("{}.add_maj_or2_f{}", path, i),
                        );

                        carry_t = new_carry_t;
                        carry_f = new_carry_f;
                    }
                }
            }

            LirOp::NclComplete { width } => {
                // Completion detection: all bit positions must be non-NULL
                // For each bit: OR(t, f) → bit_valid. Then AND all bit_valids.
                //
                // Input structure: alternating t and f signals [t0, f0, t1, f1, ...]
                // Each signal may be multi-bit, so we iterate signal pairs and their bits.
                let mut valid_nets: Vec<GateNetId> = Vec::new();
                let num_pairs = input_nets.len() / 2;
                let mut bit_idx = 0usize;
                for pair_idx in 0..num_pairs {
                    let t_nets = &input_nets[pair_idx * 2];
                    let f_nets = &input_nets[pair_idx * 2 + 1];
                    let pair_width = t_nets.len();
                    for (j, &in_t) in t_nets.iter().enumerate().take(pair_width) {
                        let in_f = f_nets.get(j).copied().unwrap_or(GateNetId(0));
                        let valid = netlist.add_net(GateNet::new(
                            GateNetId(0),
                            format!("{}.valid{}", path, bit_idx),
                        ));
                        let mut vc = Cell::new_comb(
                            CellId(0),
                            or2_cell.name.clone(),
                            library.name.clone(),
                            or2_cell.fit,
                            format!("{}.or_valid{}", path, bit_idx),
                            vec![in_t, in_f],
                            vec![valid],
                        );
                        or2_cell.apply_to_cell(&mut vc);
                        netlist.add_cell(vc);
                        valid_nets.push(valid);
                        bit_idx += 1;
                    }
                }
                debug_assert_eq!(bit_idx, *width as usize, "NclComplete: bit count mismatch");
                // AND-reduce: tree reduction.
                // AUDIT-2 #1 FIX: net/cell names must be unique across tree
                // LEVELS — `and_tree_{i}` repeated per level, and the
                // hierarchical flatten merges nets BY NAME, so level-2's
                // and_tree_0 collapsed onto level-1's net: two AND2 cells
                // drove one net with different inputs and the completion
                // signal oscillated forever (every NCL sim reported
                // stable=false with correct data). Include the level index.
                let out = output_nets.first().copied().unwrap_or(GateNetId(0));
                let mut current = valid_nets;
                let mut level = 0usize;
                while current.len() > 1 {
                    let mut next = Vec::new();
                    for pair in current.chunks(2) {
                        if pair.len() == 2 {
                            let intermediate = if next.is_empty() && current.len() == 2 {
                                out
                            } else {
                                netlist.add_net(GateNet::new(
                                    GateNetId(0),
                                    format!("{}.and_tree_l{}_{}", path, level, next.len()),
                                ))
                            };
                            let mut ac = Cell::new_comb(
                                CellId(0),
                                and2_cell.name.clone(),
                                library.name.clone(),
                                and2_cell.fit,
                                format!("{}.and_tree_l{}_{}", path, level, next.len()),
                                vec![pair[0], pair[1]],
                                vec![intermediate],
                            );
                            and2_cell.apply_to_cell(&mut ac);
                            netlist.add_cell(ac);
                            next.push(intermediate);
                        } else {
                            next.push(pair[0]);
                        }
                    }
                    current = next;
                    level += 1;
                }
                if current.len() == 1 && current[0] != out {
                    // Single valid bit → buffer to output
                    let mut bc = Cell::new_comb(
                        CellId(0),
                        buf_cell.name.clone(),
                        library.name.clone(),
                        buf_cell.fit,
                        format!("{}.buf_out", path),
                        vec![current[0]],
                        vec![out],
                    );
                    buf_cell.apply_to_cell(&mut bc);
                    netlist.add_cell(bc);
                }
            }

            LirOp::NclNull { width } => {
                // NULL generator: output all-zeros (both rails low)
                let tie_low = get_tie_low(netlist);
                for i in 0..(*width * 2) as usize {
                    let out = output_nets.get(i).copied().unwrap_or(GateNetId(0));
                    let mut nc = Cell::new_comb(
                        CellId(0),
                        buf_cell.name.clone(),
                        library.name.clone(),
                        buf_cell.fit,
                        format!("{}.null{}", path, i),
                        vec![tie_low],
                        vec![out],
                    );
                    buf_cell.apply_to_cell(&mut nc);
                    netlist.add_cell(nc);
                }
            }

            LirOp::Th12 { .. } => {
                let a = input_nets
                    .first()
                    .and_then(|v| v.first())
                    .copied()
                    .unwrap_or(GateNetId(0));
                let b = input_nets
                    .get(1)
                    .and_then(|v| v.first())
                    .copied()
                    .unwrap_or(GateNetId(0));
                let q = output_nets.first().copied().unwrap_or(GateNetId(0));
                make_th12_or_or2(netlist, a, b, q, &format!("{}.th12", path));
            }

            LirOp::Th22 { .. } => {
                let a = input_nets
                    .first()
                    .and_then(|v| v.first())
                    .copied()
                    .unwrap_or(GateNetId(0));
                let b = input_nets
                    .get(1)
                    .and_then(|v| v.first())
                    .copied()
                    .unwrap_or(GateNetId(0));
                let q = output_nets.first().copied().unwrap_or(GateNetId(0));
                make_th22_or_celement(netlist, a, b, q, &format!("{}.th22", path));
            }

            LirOp::NclSub { width } => {
                // NCL subtractor: a - b = a + (~b) + 1
                // For now, fall back to old TechMapper for complex ops
                eprintln!(
                    "warning: NclSub not yet supported in new synth path, {} bits at {}",
                    width, path
                );
            }

            LirOp::NclMul {
                input_width,
                result_width,
            } => {
                eprintln!(
                    "warning: NclMul not yet supported in new synth path, {}→{} bits at {}",
                    input_width, result_width, path
                );
            }

            LirOp::NclLt { width } => {
                eprintln!(
                    "warning: NclLt not yet supported in new synth path, {} bits at {}",
                    width, path
                );
            }

            LirOp::NclEq { width } => {
                eprintln!(
                    "warning: NclEq not yet supported in new synth path, {} bits at {}",
                    width, path
                );
            }

            LirOp::NclShl { width } | LirOp::NclShr { width } => {
                eprintln!(
                    "warning: NclShift not yet supported in new synth path, {} bits at {}",
                    width, path
                );
            }

            LirOp::NclMux2 { width } => {
                eprintln!(
                    "warning: NclMux2 not yet supported in new synth path, {} bits at {}",
                    width, path
                );
            }

            LirOp::NclReg { width } => {
                eprintln!(
                    "warning: NclReg not yet supported in new synth path, {} bits at {}",
                    width, path
                );
            }

            LirOp::MemBlock {
                data_width,
                addr_width,
                depth,
                has_write,
                async_read,
                read_enable,
                init,
                ..
            } => {
                // TRIAGE #35: the clock input net may not EXIST yet — when
                // the memory is the design's only sequential element, no comb
                // logic references clk and the input-net sweep drops it, so
                // get_net_id misses and the fallback silently clocked every
                // memory DFF with TIE_LOW (a memory that never writes; the
                // gate SIM exposed it, SAT couldn't — GateNetlistToAig treats
                // DFF clocking implicitly). Get-or-CREATE the clock input net.
                let clk_net = node
                    .clock
                    .map(|clk_sig| {
                        let clk_name = lir.signals[clk_sig.0 as usize].name.clone();
                        let id = netlist
                            .get_net_id(&clk_name)
                            .unwrap_or_else(|| netlist.add_clock(clk_name.clone()));
                        // The SIR conversion derives the sequential block's
                        // clock from netlist.clocks — an unregistered clock
                        // silently falls back to SirSignalId(0) and the
                        // memory DFFs never see an edge.
                        if !netlist.clocks.contains(&id) {
                            netlist.clocks.push(id);
                        }
                        id
                    })
                    .unwrap_or_else(|| get_tie_low(netlist));
                map_memblock_standalone(
                    netlist,
                    library,
                    *data_width,
                    *addr_width,
                    *depth,
                    *has_write,
                    *async_read,
                    read_enable
                        .then(|| input_nets.last()?.first().copied())
                        .flatten(),
                    &input_nets,
                    &output_nets,
                    path,
                    clk_net,
                    init.as_deref(),
                );
            }
            LirOp::MemRead { .. } | LirOp::MemWrite { .. } => {
                // MemRead/MemWrite are part of MemBlock — handled above
            }

            op if crate::carry_chain::is_carry_chain_op(op, 0) => {
                let operands: Vec<crate::carry_chain::Operand> = node
                    .inputs
                    .iter()
                    .zip(input_nets.iter())
                    .map(|(sig, nets)| crate::carry_chain::Operand {
                        nets: nets.clone(),
                        constant: constant_signals.get(&sig.0).copied(),
                    })
                    .collect();
                if !crate::carry_chain::map_carry_chain(
                    netlist,
                    library,
                    op,
                    &operands,
                    &output_nets,
                    path,
                ) {
                    eprintln!(
                        "warning: no carry-chain cell in library '{}' for {:?} at {}",
                        library.name, op, path
                    );
                }
            }

            LirOp::Mul {
                width,
                result_width,
                signed,
            } => {
                map_dsp_standalone(
                    netlist,
                    library,
                    *width,
                    *result_width,
                    *signed,
                    &input_nets,
                    &output_nets,
                    path,
                );
            }

            _ => {
                eprintln!("warning: unexpected physical op {:?} at {}", node.op, path);
            }
        }
    }

    // Remove physical pseudo-input nets from the primary input list.
    // These nets are now driven by the physical cells we just created.
    // Identify them by checking if the net now has a driver (was input, now driven).
    netlist.rebuild_net_connectivity();
    netlist.inputs.retain(|&net_id| {
        if let Some(net) = netlist.nets.get(net_id.0 as usize) {
            // If the net now has a driver, it's no longer a primary input
            net.driver.is_none()
        } else {
            true
        }
    });
    // Clear is_input on nets that were removed from the input list
    for net in &mut netlist.nets {
        if net.driver.is_some() && net.is_input {
            net.is_input = false;
        }
    }
}

/// Full synthesis with default balanced preset
///
/// Convenience function that uses `SynthPreset::Balanced` for a good
/// trade-off between optimization quality and runtime.
pub fn synthesize_balanced(lir: &Lir, library: &TechLibrary) -> crate::synth::SynthResult {
    // FPGA targets benefit from Auto (parallel Resyn2 + Compress2) since
    // the extra runtime is small relative to place-and-route
    let preset = if library.is_fpga() {
        crate::synth::SynthPreset::Auto
    } else {
        crate::synth::SynthPreset::Balanced
    };
    synthesize(lir, library, preset)
}

/// Full synthesis with area-focused optimization
///
/// Uses `SynthPreset::Area` for aggressive area minimization.
/// Useful for area-constrained designs.
pub fn synthesize_for_area(lir: &Lir, library: &TechLibrary) -> crate::synth::SynthResult {
    synthesize(lir, library, crate::synth::SynthPreset::Area)
}

/// Full synthesis with timing-focused optimization
///
/// Uses `SynthPreset::Timing` for timing-driven optimization.
/// Useful for timing-critical designs.
pub fn synthesize_for_timing(lir: &Lir, library: &TechLibrary) -> crate::synth::SynthResult {
    synthesize(lir, library, crate::synth::SynthPreset::Timing)
}

/// Synthesize a hierarchical design using the new AIG-based engine.
///
/// Like `map_hierarchical_to_gates` but uses `synthesize()` per instance
/// instead of the old TechMapper. Compiled IPs and blackboxes are handled
/// the same way (loaded directly / stub netlist).
///
/// This replaces the two-step `map_hierarchical_to_gates` + `engine.optimize_hierarchical()`
/// pattern — each instance is fully optimized in a single pass.
pub fn synthesize_hierarchical(
    hier_lir: &crate::mir_to_lir::HierarchicalMirToLirResult,
    library: &TechLibrary,
    preset: crate::synth::SynthPreset,
) -> crate::hierarchical_netlist::HierarchicalNetlist {
    use crate::compiled_ip::CompiledIp;
    use crate::hierarchical_netlist::{HierarchicalNetlist, InstanceNetlist, PortConnection};
    use crate::mir_to_lir::PortConnectionInfo;

    let mut result = HierarchicalNetlist::new(hier_lir.top_module.clone(), library.name.clone());

    // Bug B fix: Build map of internal signals that children reference in their
    // port connections. These must be promoted to outputs before synthesis so the
    // AIG optimizer doesn't inline them into LUT truth tables.
    let mut signals_needed_by_children: IndexMap<String, std::collections::HashSet<String>> =
        IndexMap::new();
    for (path, inst) in &hier_lir.instances {
        // Get the parent path for this instance
        if let Some(parent_dot) = path.rfind('.') {
            let parent_path = &path[..parent_dot];
            for (_port_name, conn) in &inst.port_connections {
                let signal_name = match conn {
                    PortConnectionInfo::Signal(name) => Some(name.clone()),
                    PortConnectionInfo::Range(name, _, _) => Some(name.clone()),
                    PortConnectionInfo::BitSelect(name, _) => Some(name.clone()),
                    PortConnectionInfo::InstancePort(inst_name, port_name) => {
                        // Instance output port → parent signal "{inst}_{port}"
                        Some(format!("{}_{}", inst_name, port_name))
                    }
                    _ => None,
                };
                if let Some(name) = signal_name {
                    // Check if this signal is internal (not an input/output of the parent)
                    if let Some(parent_inst) = hier_lir.instances.get(parent_path) {
                        let lir = &parent_inst.lir_result.lir;
                        let is_port = lir.inputs.iter().chain(lir.outputs.iter()).any(|&id| {
                            lir.signals
                                .get(id.0 as usize)
                                .map(|s| s.name == name)
                                .unwrap_or(false)
                        });
                        if !is_port {
                            signals_needed_by_children
                                .entry(parent_path.to_string())
                                .or_default()
                                .insert(name);
                        }
                    }
                }
            }
        }
    }

    let mut sorted_paths: Vec<_> = hier_lir.instances.keys().collect();
    sorted_paths.sort();

    for path in sorted_paths {
        let inst_lir = hier_lir.instances.get(path).unwrap();

        // Check if this instance has internal signals referenced by children
        let needs_promotion = signals_needed_by_children.get(path.as_str());
        let mut internal_outputs = std::collections::HashSet::new();

        let netlist = if let Some(ref compiled_ip_path) = inst_lir.lir_result.compiled_ip_path {
            match CompiledIp::read_from_file(std::path::Path::new(compiled_ip_path), None) {
                Ok(compiled_ip) => compiled_ip.netlist.clone(),
                Err(_e) => synthesize(&inst_lir.lir_result.lir, library, preset).netlist,
            }
        } else if let Some(ref blackbox_info) = inst_lir.lir_result.blackbox_info {
            create_blackbox_netlist(blackbox_info, &inst_lir.module_name)
        } else if let Some(signal_names) = needs_promotion {
            // Promote internal signals so synthesis preserves them.
            // Signals driven by children (instance outputs like "core_y")
            // must be inputs of the parent — otherwise the synthesizer sees
            // them as undriven and replaces with constant 0.
            // Signals consumed by children (internal computed values like "t0")
            // must be outputs.
            let mut lir = inst_lir.lir_result.lir.clone();
            for sig_name in signal_names {
                if let Some(sig_id) = lir.signals.iter().position(|s| s.name == *sig_name) {
                    let sid = crate::lir::LirSignalId(sig_id as u32);
                    let sig = &lir.signals[sig_id];
                    let has_driver = sig.driver.is_some();
                    if has_driver {
                        // Signal is driven by parent logic → promote to output
                        // (consumed by a child instance)
                        if !lir.outputs.contains(&sid) {
                            tracing::trace!(
                                "[SYNTH_HIER] Promoting driven signal '{}' to output for '{}'",
                                sig_name,
                                path
                            );
                            lir.outputs.push(sid);
                            internal_outputs.insert(sig_name.clone());
                        }
                    } else {
                        // Signal has no driver in parent → it's driven by a child instance.
                        // Promote to input so the synthesizer preserves it as a port.
                        if !lir.inputs.contains(&sid) {
                            tracing::trace!(
                                "[SYNTH_HIER] Promoting undriven signal '{}' to input for '{}'",
                                sig_name,
                                path
                            );
                            lir.inputs.push(sid);
                        }
                    }
                }
            }
            synthesize(&lir, library, preset).netlist
        } else {
            synthesize(&inst_lir.lir_result.lir, library, preset).netlist
        };

        let mut inst_netlist = InstanceNetlist::new(inst_lir.module_name.clone(), netlist);
        inst_netlist.internal_outputs = internal_outputs;

        let mut sorted_port_names: Vec<_> = inst_lir.port_connections.keys().collect();
        sorted_port_names.sort();

        for port_name in sorted_port_names {
            let conn_info = inst_lir.port_connections.get(port_name).unwrap();
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

        for child in &inst_lir.children {
            inst_netlist.add_child(child.clone());
        }

        result.add_instance(path.clone(), inst_netlist);
    }

    result
}

/// A blackbox cell's parameters: the instance's own, unquoted, and the
/// names of its input and output pins in cell pin order
/// ([`crate::gate_netlist::BLACKBOX_INPUTS_PARAM`]).
fn blackbox_parameters(info: &crate::mir_to_lir::BlackboxInfo) -> IndexMap<String, String> {
    let pins = |ports: &[String]| -> String {
        ports
            .iter()
            .flat_map(|port| {
                let width = info.port_widths.get(port).copied().unwrap_or(1);
                (0..width).map(move |bit| {
                    if width == 1 {
                        port.clone()
                    } else {
                        format!("{port}[{bit}]")
                    }
                })
            })
            .collect::<Vec<_>>()
            .join(",")
    };
    let mut parameters: IndexMap<String, String> = info
        .parameters
        .iter()
        .map(|(k, v)| (k.clone(), v.trim_matches('"').to_string()))
        .collect();
    let inputs: Vec<String> = info.inputs.iter().chain(&info.inouts).cloned().collect();
    let outputs: Vec<String> = info.outputs.iter().chain(&info.inouts).cloned().collect();
    parameters.insert(
        crate::gate_netlist::BLACKBOX_INPUTS_PARAM.to_string(),
        pins(&inputs),
    );
    parameters.insert(
        crate::gate_netlist::BLACKBOX_OUTPUTS_PARAM.to_string(),
        pins(&outputs),
    );
    parameters
}

/// Create a GateNetlist containing a single blackbox cell
///
/// This is used for vendor IP modules that should not be synthesized.
/// The blackbox cell preserves the port interface and can be instantiated
/// in the final output.
fn create_blackbox_netlist(
    blackbox_info: &crate::mir_to_lir::BlackboxInfo,
    module_name: &str,
) -> GateNetlist {
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
                alias_of: None,
                ncl_info: None,
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
                alias_of: None,
                ncl_info: None,
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
                alias_of: None,
                ncl_info: None,
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
        function: None, // Unknown function for blackbox
        fit: 0.0,       // Unknown FIT for blackbox
        failure_modes: Vec::new(),
        inputs: input_nets.clone(),
        outputs: output_nets.clone(),
        path: format!("{}.{}", module_name, blackbox_info.cell_name),
        clock: None,
        reset: None,
        source_op: Some(format!("blackbox:{}", blackbox_info.cell_name)),
        safety_classification: CellSafetyClassification::Functional,
        lut_init: None,
        parameters: blackbox_parameters(blackbox_info),
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

    netlist
}

// ============================================================================
// Tests — verify synthesize() path produces valid netlists
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lir::Lir;

    /// A 40-word combinational-read memory on a target with 16-word
    /// distributed RAM becomes three one-cell memories (the last 8 words
    /// deep) with the read mux over them as logic, keeping its output
    /// signal, and is mapped to distributed RAM cells with no mux cells of
    /// its own.
    #[test]
    fn deep_distributed_ram_is_split_into_banks() {
        let lib = crate::tech_library::get_stdlib_library("nexus").unwrap();
        let mut lir = Lir::new("ram".to_string());
        let clk = lir.add_input("clk".to_string(), 1);
        lir.clocks.push(clk);
        let raddr = lir.add_input("ra".to_string(), 6);
        let waddr = lir.add_input("wa".to_string(), 6);
        let wdata = lir.add_input("wd".to_string(), 8);
        let we = lir.add_input("we".to_string(), 1);
        let rdata = lir.add_output("rd".to_string(), 8);
        let init: Vec<u64> = (0..40).collect();
        lir.add_seq_node(
            LirOp::MemBlock {
                data_width: 8,
                addr_width: 6,
                depth: 40,
                has_write: true,
                read_only: false,
                async_read: true,
                registered_read: false,
                read_enable: false,
                init: Some(init),
            },
            vec![raddr, waddr, wdata, we],
            rdata,
            "ram.m".to_string(),
            clk,
            None,
        );

        let split = split_distram_banks(&lir, &lib).expect("the memory is split");
        let banks: Vec<(u32, u32, Vec<u64>)> = split
            .nodes
            .iter()
            .filter_map(|node| match &node.op {
                LirOp::MemBlock {
                    addr_width,
                    depth,
                    init,
                    ..
                } => {
                    assert_eq!(node.clock, Some(clk));
                    Some((*addr_width, *depth, init.clone().unwrap()))
                }
                _ => None,
            })
            .collect();
        assert_eq!(banks.len(), 3);
        assert_eq!((banks[0].0, banks[0].1), (4, 16));
        assert_eq!(banks[1].2, (16..32).collect::<Vec<u64>>());
        assert_eq!((banks[2].1, banks[2].2.len()), (8, 8));
        let driver = split.signals[rdata.0 as usize].driver.unwrap();
        assert!(matches!(
            split.nodes[driver.0 as usize].op,
            LirOp::Mux2 { width: 8 }
        ));
        // Nothing to split once each memory is one cell deep
        assert!(split_distram_banks(&split, &lib).is_none());

        let netlist = synthesize(&lir, &lib, crate::synth::SynthPreset::Full).netlist;
        let rams = netlist
            .cells
            .iter()
            .filter(|c| c.function == Some(CellFunction::DistRam16x4))
            .count();
        assert_eq!(rams, 3 * 2, "two 4-bit slices per bank");
        assert!(
            netlist
                .cells
                .iter()
                .all(|c| c.source_op.as_deref() != Some("MemBlock_DistRam")
                    || c.function == Some(CellFunction::DistRam16x4)),
            "the bank logic is mapped with the rest of the design"
        );
    }

    /// A register reset by a signal computed in the same module (a
    /// power-on reset from a counter) keeps its reset: the flip-flop gets
    /// the reset net on its reset pin.
    #[test]
    fn register_reset_by_logic_keeps_its_reset() {
        let lib = crate::tech_library::get_stdlib_library("nexus").unwrap();
        let mut lir = Lir::new("por".to_string());
        let clk = lir.add_input("clk".to_string(), 1);
        lir.clocks.push(clk);
        let a = lir.add_input("a".to_string(), 2);
        let d = lir.add_input("d".to_string(), 8);
        let q = lir.add_output("q".to_string(), 8);
        let three = lir.add_signal("three".to_string(), 2);
        lir.add_node(
            LirOp::Constant { width: 2, value: 3 },
            vec![],
            three,
            "por.three".to_string(),
        );
        let rst = lir.add_signal("rst".to_string(), 1);
        lir.add_node(
            LirOp::Ne { width: 2 },
            vec![a, three],
            rst,
            "por.rst".to_string(),
        );
        lir.add_seq_node(
            LirOp::Reg {
                width: 8,
                has_enable: false,
                has_reset: true,
                async_reset: true,
                reset_value: Some(0),
            },
            vec![d],
            q,
            "por.q".to_string(),
            clk,
            Some(rst),
        );

        let netlist = synthesize(&lir, &lib, crate::synth::SynthPreset::Full).netlist;
        let ffs: Vec<_> = netlist.cells.iter().filter(|c| c.is_sequential()).collect();
        assert_eq!(ffs.len(), 8);
        for ff in ffs {
            assert!(ff.reset.is_some(), "{} lost its reset", ff.path);
        }
        assert!(
            netlist.undriven_nets_with_fanout().is_empty(),
            "the reset logic must drive the reset pins"
        );
    }

    /// A register read by a carry chain under two names (an output and a
    /// renamed copy) is exported twice from the AIG, and the writer gives
    /// the second name its own net through a buffer. Once the exports are
    /// plain internal nets again the buffer goes: it was only a LUT level
    /// and a routing hop between the register and the adder.
    #[test]
    fn exported_carry_chain_operand_needs_no_buffer() {
        let lib = crate::tech_library::get_stdlib_library("nexus").unwrap();
        let mut lir = Lir::new("acc".to_string());
        let clk = lir.add_input("clk".to_string(), 1);
        lir.clocks.push(clk);
        let d = lir.add_input("d".to_string(), 8);
        let b = lir.add_input("b".to_string(), 8);
        let q = lir.add_output("q".to_string(), 8);
        let s_out = lir.add_output("s".to_string(), 8);
        let r = lir.add_signal("r".to_string(), 8);
        lir.add_seq_node(
            LirOp::Reg {
                width: 8,
                has_enable: false,
                has_reset: false,
                async_reset: false,
                reset_value: None,
            },
            vec![d],
            r,
            "acc.r".to_string(),
            clk,
            None,
        );
        let a = lir.add_signal("a".to_string(), 8);
        lir.add_node(LirOp::Buffer { width: 8 }, vec![r], a, "acc.a".to_string());
        let a2 = lir.add_signal("a2".to_string(), 8);
        lir.add_node(
            LirOp::Buffer { width: 8 },
            vec![r],
            a2,
            "acc.a2".to_string(),
        );
        lir.add_node(LirOp::Buffer { width: 8 }, vec![r], q, "acc.q".to_string());
        let sum = lir.add_signal("sum".to_string(), 8);
        lir.add_node(
            LirOp::Add {
                width: 8,
                has_carry: false,
                const_b: None,
            },
            vec![a, b],
            sum,
            "acc.add".to_string(),
        );
        lir.add_node(
            LirOp::Add {
                width: 8,
                has_carry: false,
                const_b: None,
            },
            vec![a2, sum],
            s_out,
            "acc.add2".to_string(),
        );

        let netlist = synthesize(&lir, &lib, crate::synth::SynthPreset::Full).netlist;
        assert!(
            netlist.cells.iter().any(|c| c.cell_type == "CCU2"),
            "the adders go on the carry chain"
        );
        let buffers: Vec<_> = netlist
            .cells
            .iter()
            .filter(|c| {
                c.function == Some(CellFunction::Buf) && !netlist.outputs.contains(&c.outputs[0])
            })
            .map(|c| c.path.clone())
            .collect();
        assert!(buffers.is_empty(), "internal buffers left: {buffers:?}");
        assert!(netlist.undriven_nets_with_fanout().is_empty());
    }

    #[test]
    fn test_synthesize_and_gate() {
        let lib = crate::tech_library::get_stdlib_library("generic_asic")
            .expect("Failed to load generic_asic");
        let mut lir = Lir::new("test".to_string());
        let a = lir.add_input("a".to_string(), 8);
        let b = lir.add_input("b".to_string(), 8);
        let y = lir.add_output("y".to_string(), 8);
        lir.add_node(
            LirOp::And { width: 8 },
            vec![a, b],
            y,
            "test.and".to_string(),
        );

        let result = synthesize(&lir, &lib, crate::synth::SynthPreset::Quick);
        assert!(!result.netlist.cells.is_empty(), "Should produce cells");
    }

    #[test]
    fn test_synthesize_adder() {
        let lib = crate::tech_library::get_stdlib_library("generic_asic")
            .expect("Failed to load generic_asic");
        let mut lir = Lir::new("test".to_string());
        let a = lir.add_input("a".to_string(), 4);
        let b = lir.add_input("b".to_string(), 4);
        let sum = lir.add_output("sum".to_string(), 4);
        lir.add_node(
            LirOp::Add {
                width: 4,
                has_carry: false,
                const_b: None,
            },
            vec![a, b],
            sum,
            "test.add".to_string(),
        );

        let result = synthesize(&lir, &lib, crate::synth::SynthPreset::Quick);
        assert!(!result.netlist.cells.is_empty(), "Should produce cells");
    }

    #[test]
    fn test_synthesize_mux() {
        let lib = crate::tech_library::get_stdlib_library("generic_asic")
            .expect("Failed to load generic_asic");
        let mut lir = Lir::new("test".to_string());
        let sel = lir.add_input("sel".to_string(), 1);
        let d0 = lir.add_input("d0".to_string(), 16);
        let d1 = lir.add_input("d1".to_string(), 16);
        let y = lir.add_output("y".to_string(), 16);
        lir.add_node(
            LirOp::Mux2 { width: 16 },
            vec![sel, d0, d1],
            y,
            "test.mux".to_string(),
        );

        let result = synthesize(&lir, &lib, crate::synth::SynthPreset::Quick);
        assert!(!result.netlist.cells.is_empty(), "Should produce cells");
    }

    #[test]
    fn test_synthesize_register() {
        let lib = crate::tech_library::get_stdlib_library("generic_asic")
            .expect("Failed to load generic_asic");
        let mut lir = Lir::new("test".to_string());
        let clk = lir.add_input("clk".to_string(), 1);
        lir.clocks.push(clk);
        let d = lir.add_input("d".to_string(), 8);
        let q = lir.add_output("q".to_string(), 8);
        lir.add_seq_node(
            LirOp::Reg {
                width: 8,
                has_enable: false,
                has_reset: false,
                async_reset: false,
                reset_value: None,
            },
            vec![d],
            q,
            "test.reg".to_string(),
            clk,
            None,
        );

        let result = synthesize(&lir, &lib, crate::synth::SynthPreset::Quick);
        let seq_count = result
            .netlist
            .cells
            .iter()
            .filter(|c| c.is_sequential())
            .count();
        assert!(
            seq_count >= 8,
            "Should have at least 8 sequential cells, got {}",
            seq_count
        );
    }

    #[test]
    fn test_synthesize_ice40_adder() {
        let lib = crate::tech_library::get_stdlib_library("ice40").expect("Failed to load ice40");
        let mut lir = Lir::new("test".to_string());
        let a = lir.add_input("a".to_string(), 4);
        let b = lir.add_input("b".to_string(), 4);
        let sum = lir.add_output("sum".to_string(), 4);
        lir.add_node(
            LirOp::Add {
                width: 4,
                has_carry: false,
                const_b: None,
            },
            vec![a, b],
            sum,
            "test.add".to_string(),
        );

        let result = synthesize(&lir, &lib, crate::synth::SynthPreset::Quick);
        assert!(!result.netlist.cells.is_empty(), "Should produce cells");
        let logic_cells = result
            .netlist
            .cells
            .iter()
            .filter(|c| {
                !matches!(
                    c.cell_type.as_str(),
                    "SB_IO" | "SB_VCC" | "SB_GND" | "SB_GB"
                )
            })
            .count();
        assert!(
            logic_cells <= 45,
            "4-bit ice40 adder should have ≤45 logic cells, got {}",
            logic_cells
        );
    }
}
