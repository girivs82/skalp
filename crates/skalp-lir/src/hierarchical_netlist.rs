//! Hierarchical Gate Netlist
//!
//! This module provides data structures for representing hierarchical gate-level
//! netlists where each module instance can be optimized independently with
//! context-aware specialization.

use crate::gate_netlist::{Cell, CellId, GateNet, GateNetId, GateNetlist};
use indexmap::IndexMap;
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};
use tracing::trace;

/// Path identifying a unique instance in the hierarchy (e.g., "top.cpu.alu")
pub type InstancePath = String;

/// A hierarchical gate-level netlist with per-instance specialization
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HierarchicalNetlist {
    /// Name of the top module
    pub top_module: String,
    /// Each instance gets its own specialized netlist
    pub instances: IndexMap<InstancePath, InstanceNetlist>,
    /// Technology library name
    pub library: String,
}

/// A specialized instance with its netlist and port connections
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InstanceNetlist {
    /// Original module name (before specialization)
    pub module_name: String,
    /// The gate netlist for this instance
    pub netlist: GateNetlist,
    /// Port connections to parent instance
    pub port_connections: IndexMap<String, PortConnection>,
    /// Which outputs are unused (for optimization)
    pub unused_outputs: HashSet<String>,
    /// Constant inputs that were propagated (for documentation)
    pub constant_inputs: IndexMap<String, u64>,
    /// Child instances within this instance
    pub children: Vec<String>,
}

/// A connection to a port
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum PortConnection {
    /// Connected to a net in parent instance
    ParentNet(String),
    /// Tied to a constant value
    Constant(u64),
    /// Connected to another instance's port
    ChildPort(InstancePath, String),
    /// Connected to a range of a parent net (base_signal, high_bit, low_bit)
    ParentRange(String, usize, usize),
    /// Connected to a single bit of a parent net (base_signal, bit_index)
    ParentBit(String, usize),
}

/// Result of hierarchical synthesis
#[derive(Debug, Clone)]
pub struct HierarchicalSynthResult {
    /// The optimized hierarchical netlist
    pub netlist: HierarchicalNetlist,
    /// Per-instance synthesis results
    pub instance_results: IndexMap<InstancePath, crate::synth::SynthResult>,
    /// Total synthesis time in milliseconds
    pub total_time_ms: u64,
}

impl HierarchicalNetlist {
    /// Create a new empty hierarchical netlist
    pub fn new(top_module: String, library: String) -> Self {
        Self {
            top_module,
            instances: IndexMap::new(),
            library,
        }
    }

    /// Add an instance to the hierarchy
    pub fn add_instance(&mut self, path: InstancePath, instance: InstanceNetlist) {
        self.instances.insert(path, instance);
    }

    /// Get the total number of instances
    pub fn instance_count(&self) -> usize {
        self.instances.len()
    }

    /// Get the total cell count across all instances
    pub fn total_cell_count(&self) -> usize {
        self.instances
            .values()
            .map(|i| i.netlist.cell_count())
            .sum()
    }

    /// Flatten the hierarchical netlist into a single GateNetlist
    /// with proper port connections between modules
    pub fn flatten(&self) -> GateNetlist {
        let mut result = GateNetlist::new(self.top_module.clone(), self.library.clone());

        // Propagate NCL flag: if any instance is NCL, the flattened netlist is NCL
        let ncl_instances: Vec<_> = self
            .instances
            .iter()
            .filter(|(_, inst)| inst.netlist.is_ncl)
            .map(|(path, _)| path.clone())
            .collect();
        result.is_ncl = !ncl_instances.is_empty();
        if result.is_ncl {
            trace!(
                "[FLATTEN] NCL detected in {} instances, setting is_ncl=true",
                ncl_instances.len()
            );
        }

        // Track net ID remapping for each instance
        let mut net_id_maps: IndexMap<&InstancePath, IndexMap<GateNetId, GateNetId>> =
            IndexMap::new();

        // Phase 1: Add all cells from all instances with hierarchical path prefix
        // IMPORTANT: Sort instance paths to ensure deterministic ordering.
        // HashMap iteration order is non-deterministic, which causes flaky tests
        // when net IDs are assigned differently between runs.
        let mut sorted_paths: Vec<_> = self.instances.keys().collect();
        sorted_paths.sort();

        for path in sorted_paths {
            let inst = self.instances.get(path).unwrap();
            let mut net_map = IndexMap::new();

            // First, create remapped nets for this instance
            for (idx, net) in inst.netlist.nets.iter().enumerate() {
                let new_net_name = format!("{}.{}", path, net.name);
                let new_net_id = result.add_net_with_name(new_net_name.clone());

                // Copy net properties (is_clock, is_reset)
                if let Some(new_net) = result.nets.get_mut(new_net_id.0 as usize) {
                    new_net.is_clock = net.is_clock;
                    new_net.is_reset = net.is_reset;
                }

                net_map.insert(GateNetId(idx as u32), new_net_id);
            }

            // Then add cells with remapped connections
            for cell in &inst.netlist.cells {
                let mut new_cell = cell.clone();
                new_cell.path = format!("{}.{}", path, cell.path);
                new_cell.id = CellId(result.cells.len() as u32);

                // Remap input and output net IDs
                new_cell.inputs = cell
                    .inputs
                    .iter()
                    .map(|&net_id| *net_map.get(&net_id).unwrap_or(&net_id))
                    .collect();
                new_cell.outputs = cell
                    .outputs
                    .iter()
                    .map(|&net_id| *net_map.get(&net_id).unwrap_or(&net_id))
                    .collect();

                // Remap clock and reset
                new_cell.clock = cell.clock.and_then(|c| net_map.get(&c).copied());
                new_cell.reset = cell.reset.and_then(|r| net_map.get(&r).copied());

                result.cells.push(new_cell);
            }

            net_id_maps.insert(path, net_map);
        }

        // Phase 2: Stitch port connections between instances
        self.stitch_all_ports(&mut result, &net_id_maps);

        // Phase 2.5: Rebuild net connectivity after stitching
        // Stitching changes cell connections, so we need to rebuild driver/fanout info
        result.rebuild_net_connectivity();

        // Phase 3: Set up top-level inputs and outputs
        // Find the "top" instance and export its I/O
        // Note: After stitching, some "inputs" may actually be driven by child modules,
        // so we only mark nets as inputs if they have no driver.
        // IMPORTANT: We must look up nets by NAME after stitching, because
        // merge_nets_by_name updates net_map to point to the merged net.
        if let Some(top_inst) = self.instances.get("top") {
            // Mark top-level inputs (only if no driver after stitching)
            for &input_net in &top_inst.netlist.inputs {
                // Get the original net name from the instance
                if let Some(net) = top_inst.netlist.nets.get(input_net.0 as usize) {
                    let full_name = format!("top.{}", net.name);
                    // Look up by name to get the FINAL net ID after stitching
                    if let Some(final_net_id) = result.get_net_id(&full_name) {
                        // Check if this net has a driver
                        if let Some(final_net) = result.nets.get_mut(final_net_id.0 as usize) {
                            if final_net.driver.is_none() {
                                final_net.is_input = true;
                                result.inputs.push(final_net_id);
                            }
                        }
                    }
                }
            }
            // Mark top-level outputs
            for &output_net in &top_inst.netlist.outputs {
                if let Some(net) = top_inst.netlist.nets.get(output_net.0 as usize) {
                    let full_name = format!("top.{}", net.name);
                    if let Some(final_net_id) = result.get_net_id(&full_name) {
                        if let Some(final_net) = result.nets.get_mut(final_net_id.0 as usize) {
                            final_net.is_output = true;
                            result.outputs.push(final_net_id);
                        }
                    }
                }
            }
        }

        // Phase 4: Cross-boundary cleanup
        // DISABLED for NCL: propagate_constants and remove_dead_cells can break
        // NCL circuits by removing cells that appear "dead" but are essential
        // for dual-rail signaling. NCL uses both true and false rails - removing
        // one breaks the circuit.
        // TODO: Add NCL-aware cleanup that understands dual-rail semantics
        // result.propagate_constants();
        // let removed = result.remove_dead_cells();
        // if removed > 0 {
        //     trace!("[FLATTEN] Removed {} dead cells after stitching", removed);
        // }

        // Phase 5: Post-flatten buffer removal
        // Buffer removal reduces cell count by removing unnecessary BUF cells created
        // during RangeSelect/Concat operations. The optimizer properly transfers
        // meaningful net names from removed buffer outputs to their inputs.
        {
            use crate::gate_optimizer::GateOptimizer;
            let cells_before = result.cells.len();
            let mut optimizer = GateOptimizer::new();
            optimizer.set_enable_constant_folding(false);
            optimizer.set_enable_dce(false);
            optimizer.set_enable_boolean_simp(false);
            optimizer.set_enable_mux_opt(false);
            optimizer.set_enable_buffer_removal(true);
            let opt_stats = optimizer.optimize(&mut result);
            if opt_stats.cells_removed > 0 {
                trace!(
                    "[FLATTEN] Post-flatten buffer removal: {} cells removed ({} → {})",
                    opt_stats.cells_removed,
                    cells_before,
                    result.cells.len()
                );
            }
        }

        // Phase 6: Final rebuild of net connectivity after all optimizations
        // This ensures fanout and driver info is accurate after buffer removal
        result.rebuild_net_connectivity();

        result.update_stats();
        result
    }

    /// Stitch all port connections between instances
    fn stitch_all_ports(
        &self,
        result: &mut GateNetlist,
        _net_maps: &IndexMap<&InstancePath, IndexMap<GateNetId, GateNetId>>,
    ) {
        trace!(
            "[STITCH] Starting port stitching for {} instances",
            self.instances.len()
        );

        // Collect all merge pairs for batched processing
        // Each pair is (survivor_name, merged_name) - survivor's name is preserved
        let mut merge_pairs: Vec<(String, String)> = Vec::new();

        // Sort instance paths for deterministic ordering
        let mut sorted_paths: Vec<_> = self.instances.keys().collect();
        sorted_paths.sort();

        for path in sorted_paths {
            let inst = self.instances.get(path).unwrap();
            // Get parent path (everything before the last '.')
            let parent_path = path.rfind('.').map(|pos| &path[..pos]).unwrap_or("");

            if !inst.port_connections.is_empty() {
                trace!(
                    "[STITCH] Instance '{}' has {} port connections",
                    path,
                    inst.port_connections.len()
                );
            }

            // Sort port names for deterministic ordering
            let mut sorted_port_names: Vec<_> = inst.port_connections.keys().collect();
            sorted_port_names.sort();

            for port_name in sorted_port_names {
                let conn = inst.port_connections.get(port_name).unwrap();
                match conn {
                    PortConnection::ParentNet(parent_signal) => {
                        // Merge child port net with parent net
                        // Child net is {instance_path}.{port_name}
                        // Parent net is {parent_path}.{signal_name}
                        let child_net_name = format!("{}.{}", path, port_name);
                        let parent_net_name = if parent_path.is_empty() {
                            parent_signal.clone()
                        } else {
                            format!("{}.{}", parent_path, parent_signal)
                        };

                        // Check if direct merge will work
                        let child_exists = result.get_net(&child_net_name).is_some();
                        let parent_exists = result.get_net(&parent_net_name).is_some();

                        if child_exists && parent_exists {
                            // Direct single-net merge - parent first so its name survives
                            trace!("[STITCH]   ✓ {} <-> {}", child_net_name, parent_net_name);
                            merge_pairs.push((parent_net_name, child_net_name));
                        } else {
                            // Try bit-level stitching for multi-bit ports
                            let child_bits = result.find_bit_indexed_nets(&child_net_name);
                            let parent_bits = result.find_bit_indexed_nets(&parent_net_name);

                            if !child_bits.is_empty() && !parent_bits.is_empty() {
                                // Both have bit-indexed nets - stitch bit by bit
                                // Convert parent_bits to HashMap for O(1) lookup
                                let parent_map: HashMap<usize, &String> =
                                    parent_bits.iter().map(|(idx, name)| (*idx, name)).collect();
                                // Parent first so its name survives
                                let mut stitched = 0;
                                for (idx, child_bit_net) in &child_bits {
                                    // Find matching parent bit - O(1) lookup
                                    if let Some(parent_bit_net) = parent_map.get(idx) {
                                        merge_pairs.push((
                                            (*parent_bit_net).clone(),
                                            child_bit_net.clone(),
                                        ));
                                        stitched += 1;
                                    }
                                }
                                trace!(
                                    "[STITCH]   ✓ {} <-> {} (bit-level: {} bits)",
                                    child_net_name,
                                    parent_net_name,
                                    stitched
                                );
                            } else if !child_bits.is_empty() || !parent_bits.is_empty() {
                                // Mismatch: one is bit-indexed, the other isn't
                                // Try NCL dual-rail stitching
                                let (child_t, child_f) =
                                    result.find_ncl_bit_indexed_nets(&child_net_name);
                                let (parent_t, parent_f) =
                                    result.find_ncl_bit_indexed_nets(&parent_net_name);

                                if !child_t.is_empty() && !parent_t.is_empty() {
                                    // NCL dual-rail stitching with HashMap for O(1) lookup
                                    let parent_t_map: HashMap<usize, &String> =
                                        parent_t.iter().map(|(idx, name)| (*idx, name)).collect();
                                    let parent_f_map: HashMap<usize, &String> =
                                        parent_f.iter().map(|(idx, name)| (*idx, name)).collect();
                                    let mut stitched = 0;

                                    // Stitch true rails
                                    for (idx, child_bit_net) in &child_t {
                                        if let Some(parent_bit_net) = parent_t_map.get(idx) {
                                            merge_pairs.push((
                                                (*parent_bit_net).clone(),
                                                child_bit_net.clone(),
                                            ));
                                            stitched += 1;
                                        }
                                    }

                                    // Stitch false rails
                                    for (idx, child_bit_net) in &child_f {
                                        if let Some(parent_bit_net) = parent_f_map.get(idx) {
                                            merge_pairs.push((
                                                (*parent_bit_net).clone(),
                                                child_bit_net.clone(),
                                            ));
                                            stitched += 1;
                                        }
                                    }

                                    trace!(
                                        "[STITCH]   ✓ {} <-> {} (NCL dual-rail: {} nets)",
                                        child_net_name,
                                        parent_net_name,
                                        stitched
                                    );
                                } else {
                                    trace!(
                                        "[STITCH]   ~ {} -> {} (child_bits={}, parent_bits={}, ncl_t={}/{})",
                                        child_net_name,
                                        parent_net_name,
                                        child_bits.len(),
                                        parent_bits.len(),
                                        child_t.len(),
                                        parent_t.len()
                                    );
                                }
                            } else {
                                // Try NCL dual-rail stitching even when no standard bit-indexed nets
                                let (child_t, child_f) =
                                    result.find_ncl_bit_indexed_nets(&child_net_name);
                                let (parent_t, parent_f) =
                                    result.find_ncl_bit_indexed_nets(&parent_net_name);

                                if !child_t.is_empty() && !parent_t.is_empty() {
                                    // NCL dual-rail stitching with HashMap for O(1) lookup
                                    let parent_t_map: HashMap<usize, &String> =
                                        parent_t.iter().map(|(idx, name)| (*idx, name)).collect();
                                    let parent_f_map: HashMap<usize, &String> =
                                        parent_f.iter().map(|(idx, name)| (*idx, name)).collect();
                                    let mut stitched = 0;

                                    // Stitch true rails
                                    for (idx, child_bit_net) in &child_t {
                                        if let Some(parent_bit_net) = parent_t_map.get(idx) {
                                            merge_pairs.push((
                                                (*parent_bit_net).clone(),
                                                child_bit_net.clone(),
                                            ));
                                            stitched += 1;
                                        }
                                    }

                                    // Stitch false rails
                                    for (idx, child_bit_net) in &child_f {
                                        if let Some(parent_bit_net) = parent_f_map.get(idx) {
                                            merge_pairs.push((
                                                (*parent_bit_net).clone(),
                                                child_bit_net.clone(),
                                            ));
                                            stitched += 1;
                                        }
                                    }

                                    trace!(
                                        "[STITCH]   ✓ {} <-> {} (NCL dual-rail: {} nets)",
                                        child_net_name,
                                        parent_net_name,
                                        stitched
                                    );
                                } else if !child_t.is_empty() || !parent_t.is_empty() {
                                    // One side has NCL nets, the other doesn't
                                    trace!(
                                        "[STITCH]   ~ {} -> {} (NCL mismatch: child_t={}, parent_t={})",
                                        child_net_name,
                                        parent_net_name,
                                        child_t.len(),
                                        parent_t.len()
                                    );
                                } else {
                                    // Neither exists at all - might be unused connection
                                    trace!(
                                        "[STITCH]   ✗ {} -> {} (neither exists)",
                                        child_net_name,
                                        parent_net_name
                                    );
                                }
                            }
                        }
                    }
                    PortConnection::Constant(value) => {
                        // Create tie cell(s) for constant
                        // BUG #168 FIX: Check if child port has bit-indexed nets
                        let net_name = format!("{}.{}", path, port_name);
                        let child_bits = result.find_bit_indexed_nets(&net_name);

                        trace!(
                            "[STITCH]   Looking for '{}' - found {} bit-indexed nets",
                            net_name,
                            child_bits.len()
                        );

                        if !child_bits.is_empty() {
                            // Multi-bit port: create a tie cell for each bit
                            trace!(
                                "[STITCH]   Constant 0x{:X} -> {} ({} bits)",
                                value,
                                net_name,
                                child_bits.len()
                            );
                            for (bit_idx, bit_net_name) in &child_bits {
                                let bit_value = (value >> bit_idx) & 1;
                                result.add_tie_cell(bit_net_name, bit_value);
                            }
                        } else if result.get_net(&net_name).is_some() {
                            // Single net exists - use simple tie cell
                            result.add_tie_cell(&net_name, *value);
                        } else {
                            // Net doesn't exist - this might be an unused connection
                            trace!(
                                "[STITCH]   ✗ Constant 0x{:X} -> {} (net not found)",
                                value,
                                net_name
                            );
                        }
                    }
                    PortConnection::ChildPort(child_path, child_port) => {
                        // Direct connection between instances
                        let net1 = format!("{}.{}", path, port_name);
                        let net2 = format!("{}.{}", child_path, child_port);

                        // Try direct merge first
                        let net1_exists = result.get_net(&net1).is_some();
                        let net2_exists = result.get_net(&net2).is_some();

                        if net1_exists && net2_exists {
                            merge_pairs.push((net1, net2));
                        } else {
                            // Try bit-level stitching
                            let bits1 = result.find_bit_indexed_nets(&net1);
                            let bits2 = result.find_bit_indexed_nets(&net2);

                            if !bits1.is_empty() && !bits2.is_empty() {
                                for (idx, bit1_net) in &bits1 {
                                    if let Some((_, bit2_net)) =
                                        bits2.iter().find(|(idx2, _)| idx2 == idx)
                                    {
                                        merge_pairs.push((bit1_net.clone(), bit2_net.clone()));
                                    }
                                }
                            }
                        }
                    }
                    PortConnection::ParentRange(parent_signal, high, low) => {
                        // Range connection: child port connects to parent[high:low]
                        // Child bit i connects to parent bit (low + i)
                        let child_net_name = format!("{}.{}", path, port_name);
                        let parent_base = if parent_path.is_empty() {
                            parent_signal.clone()
                        } else {
                            format!("{}.{}", parent_path, parent_signal)
                        };

                        // Get child bits
                        let child_bits = result.find_bit_indexed_nets(&child_net_name);

                        if !child_bits.is_empty() {
                            let mut stitched = 0;
                            for (child_idx, child_bit_net) in &child_bits {
                                // Child bit i maps to parent bit (low + i)
                                let parent_bit_idx = low + child_idx;
                                if parent_bit_idx <= *high {
                                    let parent_bit_net =
                                        format!("{}[{}]", parent_base, parent_bit_idx);
                                    if result.get_net(&parent_bit_net).is_some() {
                                        merge_pairs.push((parent_bit_net, child_bit_net.clone()));
                                        stitched += 1;
                                    }
                                }
                            }
                            trace!(
                                "[STITCH]   ✓ {} <-> {}[{}:{}] (range: {} bits)",
                                child_net_name,
                                parent_base,
                                high,
                                low,
                                stitched
                            );
                        } else {
                            // Try NCL dual-rail range stitching
                            // For NCL, we need to stitch {child}_t[N] <-> {parent}_t[low+N]
                            // and {child}_f[N] <-> {parent}_f[low+N]
                            let (child_t, child_f) =
                                result.find_ncl_bit_indexed_nets(&child_net_name);

                            if !child_t.is_empty() {
                                let mut stitched = 0;

                                // Stitch true rails with offset
                                for (child_idx, child_bit_net) in &child_t {
                                    let parent_bit_idx = low + child_idx;
                                    if parent_bit_idx <= *high {
                                        let parent_bit_net =
                                            format!("{}_t[{}]", parent_base, parent_bit_idx);
                                        if result.get_net(&parent_bit_net).is_some() {
                                            merge_pairs
                                                .push((parent_bit_net, child_bit_net.clone()));
                                            stitched += 1;
                                        } else if stitched == 0 && *child_idx == 0 {
                                            // BUG #200 DEBUG: Log first missing parent net
                                            trace!(
                                                "[STITCH]     DEBUG: Parent net '{}' NOT FOUND for {}",
                                                parent_bit_net, parent_base
                                            );
                                        }
                                    }
                                }

                                // Stitch false rails with offset
                                for (child_idx, child_bit_net) in &child_f {
                                    let parent_bit_idx = low + child_idx;
                                    if parent_bit_idx <= *high {
                                        let parent_bit_net =
                                            format!("{}_f[{}]", parent_base, parent_bit_idx);
                                        if result.get_net(&parent_bit_net).is_some() {
                                            merge_pairs
                                                .push((parent_bit_net, child_bit_net.clone()));
                                            stitched += 1;
                                        }
                                    }
                                }

                                trace!(
                                    "[STITCH]   ✓ {} <-> {}[{}:{}] (NCL range: {} nets)",
                                    child_net_name,
                                    parent_base,
                                    high,
                                    low,
                                    stitched
                                );
                            } else {
                                trace!(
                                    "[STITCH]   ✗ {} -> {}[{}:{}] (no child bits found)",
                                    child_net_name,
                                    parent_base,
                                    high,
                                    low
                                );
                            }
                        }
                    }
                    PortConnection::ParentBit(parent_signal, bit_idx) => {
                        // Single bit connection: child port connects to parent[bit_idx]
                        let child_net_name = format!("{}.{}", path, port_name);
                        let parent_base = if parent_path.is_empty() {
                            parent_signal.clone()
                        } else {
                            format!("{}.{}", parent_path, parent_signal)
                        };
                        let parent_bit_net = format!("{}[{}]", parent_base, bit_idx);

                        // Try direct merge first (child might be single-bit)
                        if result.get_net(&child_net_name).is_some()
                            && result.get_net(&parent_bit_net).is_some()
                        {
                            merge_pairs.push((parent_bit_net.clone(), child_net_name.clone()));
                            trace!("[STITCH]   ✓ {} <-> {}", child_net_name, parent_bit_net);
                        } else {
                            // Try child bit 0 -> parent bit
                            let child_bit0 = format!("{}[0]", child_net_name);
                            if result.get_net(&child_bit0).is_some()
                                && result.get_net(&parent_bit_net).is_some()
                            {
                                merge_pairs.push((parent_bit_net.clone(), child_bit0.clone()));
                                trace!("[STITCH]   ✓ {} <-> {}", child_bit0, parent_bit_net);
                            } else {
                                trace!(
                                    "[STITCH]   ✗ {} -> {} (nets not found)",
                                    child_net_name,
                                    parent_bit_net
                                );
                            }
                        }
                    }
                }
            }
        }

        // Apply all merges in a single batched pass
        if !merge_pairs.is_empty() {
            trace!(
                "[STITCH] Applying {} merge pairs in batched mode",
                merge_pairs.len()
            );
            let pairs_refs: Vec<(&str, &str)> = merge_pairs
                .iter()
                .map(|(s, m)| (s.as_str(), m.as_str()))
                .collect();
            result.merge_nets_batched(&pairs_refs);
        }
    }

    /// Get a summary string
    pub fn summary(&self) -> String {
        format!(
            "HierarchicalNetlist: {} instances, {} total cells",
            self.instance_count(),
            self.total_cell_count()
        )
    }
}

impl InstanceNetlist {
    /// Create a new instance netlist
    pub fn new(module_name: String, netlist: GateNetlist) -> Self {
        Self {
            module_name,
            netlist,
            port_connections: IndexMap::new(),
            unused_outputs: HashSet::new(),
            constant_inputs: IndexMap::new(),
            children: Vec::new(),
        }
    }

    /// Mark an output as unused (for DCE optimization)
    pub fn mark_output_unused(&mut self, output_name: &str) {
        self.unused_outputs.insert(output_name.to_string());
    }

    /// Record a constant input (for documentation)
    pub fn record_constant_input(&mut self, input_name: &str, value: u64) {
        self.constant_inputs.insert(input_name.to_string(), value);
    }

    /// Add a port connection
    pub fn add_port_connection(&mut self, port_name: String, connection: PortConnection) {
        self.port_connections.insert(port_name, connection);
    }

    /// Add a child instance reference
    pub fn add_child(&mut self, child_path: String) {
        self.children.push(child_path);
    }
}

impl HierarchicalSynthResult {
    /// Get total cell count after optimization
    pub fn total_cells(&self) -> usize {
        self.netlist.total_cell_count()
    }

    /// Get a summary of per-instance results
    pub fn instance_summary(&self) -> String {
        let mut lines = Vec::new();
        for (path, result) in &self.instance_results {
            lines.push(format!(
                "  {}: {} -> {} cells ({:.1}% reduction)",
                path,
                result.initial_and_count,
                result.netlist.cell_count(),
                result.gate_reduction() * 100.0
            ));
        }
        lines.join("\n")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_hierarchical_netlist_creation() {
        let hier = HierarchicalNetlist::new("top".to_string(), "generic".to_string());
        assert_eq!(hier.instance_count(), 0);
        assert_eq!(hier.total_cell_count(), 0);
    }

    #[test]
    fn test_instance_netlist_creation() {
        let netlist = GateNetlist::new("test".to_string(), "generic".to_string());
        let mut inst = InstanceNetlist::new("TestModule".to_string(), netlist);

        inst.mark_output_unused("unused_out");
        assert!(inst.unused_outputs.contains("unused_out"));

        inst.record_constant_input("const_in", 42);
        assert_eq!(inst.constant_inputs.get("const_in"), Some(&42));
    }

    #[test]
    fn test_port_connection_types() {
        let conn1 = PortConnection::ParentNet("parent_net".to_string());
        let conn2 = PortConnection::Constant(0);
        let conn3 = PortConnection::ChildPort("child.path".to_string(), "port".to_string());

        // Just verify they can be created and matched
        match conn1 {
            PortConnection::ParentNet(name) => assert_eq!(name, "parent_net"),
            _ => panic!("Wrong variant"),
        }
        match conn2 {
            PortConnection::Constant(v) => assert_eq!(v, 0),
            _ => panic!("Wrong variant"),
        }
        match conn3 {
            PortConnection::ChildPort(path, port) => {
                assert_eq!(path, "child.path");
                assert_eq!(port, "port");
            }
            _ => panic!("Wrong variant"),
        }
    }
}
