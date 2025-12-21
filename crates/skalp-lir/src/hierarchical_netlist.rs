//! Hierarchical Gate Netlist
//!
//! This module provides data structures for representing hierarchical gate-level
//! netlists where each module instance can be optimized independently with
//! context-aware specialization.

use crate::gate_netlist::{Cell, CellId, GateNet, GateNetId, GateNetlist};
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};

/// Path identifying a unique instance in the hierarchy (e.g., "top.cpu.alu")
pub type InstancePath = String;

/// A hierarchical gate-level netlist with per-instance specialization
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HierarchicalNetlist {
    /// Name of the top module
    pub top_module: String,
    /// Each instance gets its own specialized netlist
    pub instances: HashMap<InstancePath, InstanceNetlist>,
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
    pub port_connections: HashMap<String, PortConnection>,
    /// Which outputs are unused (for optimization)
    pub unused_outputs: HashSet<String>,
    /// Constant inputs that were propagated (for documentation)
    pub constant_inputs: HashMap<String, u64>,
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
}

/// Result of hierarchical synthesis
#[derive(Debug, Clone)]
pub struct HierarchicalSynthResult {
    /// The optimized hierarchical netlist
    pub netlist: HierarchicalNetlist,
    /// Per-instance synthesis results
    pub instance_results: HashMap<InstancePath, crate::synth::SynthResult>,
    /// Total synthesis time in milliseconds
    pub total_time_ms: u64,
}

impl HierarchicalNetlist {
    /// Create a new empty hierarchical netlist
    pub fn new(top_module: String, library: String) -> Self {
        Self {
            top_module,
            instances: HashMap::new(),
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

        // Track net ID remapping for each instance
        let mut net_id_maps: HashMap<&InstancePath, HashMap<GateNetId, GateNetId>> = HashMap::new();

        // Phase 1: Add all cells from all instances with hierarchical path prefix
        for (path, inst) in &self.instances {
            let mut net_map = HashMap::new();

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

        // Phase 3: Set up top-level inputs and outputs
        // Find the "top" instance and export its I/O
        if let Some(top_inst) = self.instances.get("top") {
            if let Some(top_net_map) = net_id_maps.get(&"top".to_string()) {
                // Mark top-level inputs
                for &input_net in &top_inst.netlist.inputs {
                    if let Some(&remapped) = top_net_map.get(&input_net) {
                        result.inputs.push(remapped);
                    }
                }
                // Mark top-level outputs
                for &output_net in &top_inst.netlist.outputs {
                    if let Some(&remapped) = top_net_map.get(&output_net) {
                        result.outputs.push(remapped);
                    }
                }
            }
        }

        // Phase 4: Cross-boundary cleanup
        result.propagate_constants();
        // NOTE: DCE disabled because port stitching doesn't properly connect nets yet.
        // See stitch_all_ports() for details on the limitation.
        // result.remove_dead_cells();

        result.update_stats();
        result
    }

    /// Stitch all port connections between instances
    ///
    /// NOTE: Port stitching is currently limited because MIR uses internal signal IDs
    /// (like `signal_3`, `port_14`) while GateNetlist uses actual signal names
    /// (like `cmp_lt`, `a[0]`). This mapping is lost during elaboration.
    /// TODO: Carry signal name information through elaboration for proper stitching.
    fn stitch_all_ports(
        &self,
        result: &mut GateNetlist,
        _net_maps: &HashMap<&InstancePath, HashMap<GateNetId, GateNetId>>,
    ) {
        for (path, inst) in &self.instances {
            // Get parent path (everything before the last '.')
            let parent_path = path.rfind('.').map(|pos| &path[..pos]).unwrap_or("");

            for (port_name, conn) in &inst.port_connections {
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
                        result.merge_nets_by_name(&child_net_name, &parent_net_name);
                    }
                    PortConnection::Constant(value) => {
                        // Create tie cell for constant
                        let net_name = format!("{}.{}", path, port_name);
                        result.add_tie_cell(&net_name, *value);
                    }
                    PortConnection::ChildPort(child_path, child_port) => {
                        // Direct connection between instances
                        let net1 = format!("{}.{}", path, port_name);
                        let net2 = format!("{}.{}", child_path, child_port);
                        result.merge_nets_by_name(&net1, &net2);
                    }
                }
            }
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
            port_connections: HashMap::new(),
            unused_outputs: HashSet::new(),
            constant_inputs: HashMap::new(),
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
