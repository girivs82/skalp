//! Signal Trace Module
//!
//! Provides functionality to trace signals through a gate-level netlist,
//! showing the complete driver chain from output to source (backward trace)
//! or fanout from source to sinks (forward trace).
//!
//! This is useful for debugging equivalence check mismatches by showing
//! exactly how a signal is computed through the gate network.

use crate::gate_netlist::{Cell, CellId, GateNet, GateNetId, GateNetlist};
use indexmap::IndexMap;
use std::collections::HashSet;

/// Direction of trace
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TraceDirection {
    /// Trace backward from output to sources (driver chain)
    Backward,
    /// Trace forward from source to sinks (fanout)
    Forward,
}

/// A node in the signal trace
#[derive(Debug, Clone)]
pub struct TraceNode {
    /// The net being traced
    pub net_name: String,
    /// The net ID
    pub net_id: GateNetId,
    /// Whether this is a primary input
    pub is_input: bool,
    /// Whether this is a primary output
    pub is_output: bool,
    /// The cell driving this net (None for primary inputs)
    pub driver_cell: Option<TraceCellInfo>,
    /// Depth in the trace (0 = starting signal)
    pub depth: usize,
}

/// Information about a cell in the trace
#[derive(Debug, Clone)]
pub struct TraceCellInfo {
    /// Cell ID
    pub cell_id: CellId,
    /// Cell type (e.g., "NAND2_X1", "DFF_X1", "MUX2_X1")
    pub cell_type: String,
    /// Hierarchical path
    pub path: String,
    /// Source operation (if available)
    pub source_op: Option<String>,
    /// Input nets with their names
    pub inputs: Vec<(String, GateNetId)>,
    /// Output net names
    pub outputs: Vec<String>,
    /// Clock net (for sequential cells)
    pub clock: Option<String>,
    /// Reset net (for sequential cells)
    pub reset: Option<String>,
    /// Is this a sequential element (DFF, latch)?
    pub is_sequential: bool,
}

/// Result of a signal trace
#[derive(Debug, Clone)]
pub struct SignalTrace {
    /// The starting signal name
    pub start_signal: String,
    /// Direction of trace
    pub direction: TraceDirection,
    /// Nodes in the trace (in traversal order)
    pub nodes: Vec<TraceNode>,
    /// Maximum depth reached
    pub max_depth: usize,
    /// Whether the trace was truncated due to depth limit
    pub truncated: bool,
}

impl SignalTrace {
    /// Print the trace in a human-readable format
    pub fn print(&self) {
        let dir_str = match self.direction {
            TraceDirection::Backward => "DRIVER TRACE (backward)",
            TraceDirection::Forward => "FANOUT TRACE (forward)",
        };

        println!("Signal: {}", self.start_signal);
        println!("Direction: {}", dir_str);
        println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
        println!();

        for node in &self.nodes {
            let indent = "    ".repeat(node.depth);
            let prefix = if node.depth == 0 { "▶" } else { "↑" };

            // Net info
            let mut net_info = String::new();
            if node.is_input {
                net_info.push_str(" [INPUT]");
            }
            if node.is_output {
                net_info.push_str(" [OUTPUT]");
            }

            println!("{}[NET] {}{}", indent, node.net_name, net_info);

            // Driver cell info
            if let Some(ref cell) = node.driver_cell {
                let cell_prefix = if cell.is_sequential { "🔲" } else { "⬡" };
                println!("{}  {} {}", indent, prefix, cell_prefix);
                println!("{}[{}] {}", indent, cell.cell_type, cell.path);

                if let Some(ref op) = cell.source_op {
                    println!("{}    source_op: {}", indent, op);
                }

                // Show inputs
                if !cell.inputs.is_empty() {
                    println!("{}    inputs:", indent);
                    for (name, _id) in &cell.inputs {
                        println!("{}      ↑ {}", indent, name);
                    }
                }

                // Show clock/reset for sequential
                if let Some(ref clk) = cell.clock {
                    println!("{}    clk: {}", indent, clk);
                }
                if let Some(ref rst) = cell.reset {
                    println!("{}    rst: {}", indent, rst);
                }

                println!();
            }
        }

        if self.truncated {
            println!();
            println!(
                "⚠️  Trace truncated at depth {} (use --depth to increase)",
                self.max_depth
            );
        }
    }
}

/// Error type for signal tracing
#[derive(Debug, Clone)]
pub enum TraceError {
    /// Signal not found in netlist
    SignalNotFound(String),
    /// Multiple signals match the pattern
    AmbiguousSignal(String, Vec<String>),
    /// Internal error
    Internal(String),
}

impl std::fmt::Display for TraceError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TraceError::SignalNotFound(s) => write!(f, "Signal '{}' not found in netlist", s),
            TraceError::AmbiguousSignal(s, matches) => {
                write!(f, "Ambiguous signal '{}'. Matches: {:?}", s, matches)
            }
            TraceError::Internal(s) => write!(f, "Internal error: {}", s),
        }
    }
}

impl std::error::Error for TraceError {}

/// Signal tracer for gate-level netlists
pub struct SignalTracer<'a> {
    netlist: &'a GateNetlist,
    /// Net name to ID mapping (cached)
    net_by_name: IndexMap<String, GateNetId>,
    /// Cell ID to cell mapping (cached)
    cell_by_id: IndexMap<u32, &'a Cell>,
    /// Net ID to driver cell mapping (cached)
    driver_map: IndexMap<u32, &'a Cell>,
}

impl<'a> SignalTracer<'a> {
    /// Create a new signal tracer for the given netlist
    pub fn new(netlist: &'a GateNetlist) -> Self {
        // Build name -> ID map
        let mut net_by_name = IndexMap::new();
        for net in &netlist.nets {
            net_by_name.insert(net.name.clone(), net.id);
        }

        // Build cell ID -> cell map
        let mut cell_by_id = IndexMap::new();
        for cell in &netlist.cells {
            cell_by_id.insert(cell.id.0, cell);
        }

        // Build net ID -> driver cell map (for forward tracing fanout)
        let mut driver_map: IndexMap<u32, &Cell> = IndexMap::new();
        for cell in &netlist.cells {
            for output in &cell.outputs {
                driver_map.insert(output.0, cell);
            }
        }

        Self {
            netlist,
            net_by_name,
            cell_by_id,
            driver_map,
        }
    }

    /// Find signals matching a pattern (for suggestions)
    pub fn find_matching_signals(&self, pattern: &str, limit: usize) -> Vec<String> {
        let pattern_lower = pattern.to_lowercase();
        let mut matches: Vec<_> = self
            .net_by_name
            .keys()
            .filter(|name| name.to_lowercase().contains(&pattern_lower))
            .cloned()
            .collect();

        // Sort by relevance (exact match first, then by length)
        matches.sort_by(|a, b| {
            let a_exact = a.to_lowercase() == pattern_lower;
            let b_exact = b.to_lowercase() == pattern_lower;
            if a_exact != b_exact {
                return b_exact.cmp(&a_exact);
            }
            a.len().cmp(&b.len())
        });

        matches.truncate(limit);
        matches
    }

    /// Resolve a signal name to a net ID
    fn resolve_signal(&self, name: &str) -> Result<GateNetId, TraceError> {
        // Try exact match first
        if let Some(&id) = self.net_by_name.get(name) {
            return Ok(id);
        }

        // Try with common variations
        // struct.field -> struct__field
        let underscore_name = name.replace('.', "__");
        if let Some(&id) = self.net_by_name.get(&underscore_name) {
            return Ok(id);
        }

        // Try adding [0] suffix for single-bit signals
        let with_bit = format!("{}[0]", name);
        if let Some(&id) = self.net_by_name.get(&with_bit) {
            return Ok(id);
        }

        // Try with top. prefix
        let with_top = format!("top.{}", name);
        if let Some(&id) = self.net_by_name.get(&with_top) {
            return Ok(id);
        }

        // Search for partial matches
        let matches = self.find_matching_signals(name, 5);
        if matches.len() == 1 {
            // Single match - use it
            return Ok(*self.net_by_name.get(&matches[0]).unwrap());
        } else if !matches.is_empty() {
            return Err(TraceError::AmbiguousSignal(name.to_string(), matches));
        }

        Err(TraceError::SignalNotFound(name.to_string()))
    }

    /// Trace a signal through the netlist
    pub fn trace(
        &self,
        signal: &str,
        direction: TraceDirection,
        max_depth: usize,
    ) -> Result<SignalTrace, TraceError> {
        let net_id = self.resolve_signal(signal)?;

        match direction {
            TraceDirection::Backward => self.trace_backward(signal, net_id, max_depth),
            TraceDirection::Forward => self.trace_forward(signal, net_id, max_depth),
        }
    }

    /// Trace backward from a signal to its sources
    fn trace_backward(
        &self,
        start_name: &str,
        start_id: GateNetId,
        max_depth: usize,
    ) -> Result<SignalTrace, TraceError> {
        let mut nodes = Vec::new();
        let mut visited = HashSet::new();
        let mut truncated = false;

        self.trace_backward_recursive(
            start_id,
            0,
            max_depth,
            &mut nodes,
            &mut visited,
            &mut truncated,
        );

        Ok(SignalTrace {
            start_signal: start_name.to_string(),
            direction: TraceDirection::Backward,
            nodes,
            max_depth,
            truncated,
        })
    }

    fn trace_backward_recursive(
        &self,
        net_id: GateNetId,
        depth: usize,
        max_depth: usize,
        nodes: &mut Vec<TraceNode>,
        visited: &mut HashSet<u32>,
        truncated: &mut bool,
    ) {
        // Prevent infinite loops
        if visited.contains(&net_id.0) {
            return;
        }
        visited.insert(net_id.0);

        // Check depth limit
        if depth > max_depth {
            *truncated = true;
            return;
        }

        let net = &self.netlist.nets[net_id.0 as usize];

        // Build driver cell info
        // BUG #247 FIX: net.driver is a CellId, not a net ID!
        // We must use cell_by_id (keyed by CellId) not driver_map (keyed by net ID)
        let driver_cell = if let Some(driver_id) = net.driver {
            self.cell_by_id.get(&driver_id.0).map(|cell| {
                let inputs: Vec<_> = cell
                    .inputs
                    .iter()
                    .map(|&input_id| {
                        let input_net = &self.netlist.nets[input_id.0 as usize];
                        (input_net.name.clone(), input_id)
                    })
                    .collect();

                let outputs: Vec<_> = cell
                    .outputs
                    .iter()
                    .map(|&output_id| self.netlist.nets[output_id.0 as usize].name.clone())
                    .collect();

                let clock = cell
                    .clock
                    .map(|clk_id| self.netlist.nets[clk_id.0 as usize].name.clone());

                let reset = cell
                    .reset
                    .map(|rst_id| self.netlist.nets[rst_id.0 as usize].name.clone());

                let is_sequential = cell.cell_type.contains("DFF")
                    || cell.cell_type.contains("LATCH")
                    || cell.cell_type.contains("FF")
                    || cell.clock.is_some();

                TraceCellInfo {
                    cell_id: cell.id,
                    cell_type: cell.cell_type.clone(),
                    path: cell.path.clone(),
                    source_op: cell.source_op.clone(),
                    inputs,
                    outputs,
                    clock,
                    reset,
                    is_sequential,
                }
            })
        } else {
            None
        };

        let trace_node = TraceNode {
            net_name: net.name.clone(),
            net_id,
            is_input: net.is_input,
            is_output: net.is_output,
            driver_cell: driver_cell.clone(),
            depth,
        };

        nodes.push(trace_node);

        // Continue tracing through driver's inputs (but stop at sequential elements for readability)
        if let Some(ref cell_info) = driver_cell {
            // For sequential elements, just show inputs but don't recurse further by default
            // This keeps the trace focused on the combinational cone
            if !cell_info.is_sequential {
                for (_, input_id) in &cell_info.inputs {
                    self.trace_backward_recursive(
                        *input_id,
                        depth + 1,
                        max_depth,
                        nodes,
                        visited,
                        truncated,
                    );
                }
            }
        }
    }

    /// Trace forward from a signal to its fanout
    fn trace_forward(
        &self,
        start_name: &str,
        start_id: GateNetId,
        max_depth: usize,
    ) -> Result<SignalTrace, TraceError> {
        let mut nodes = Vec::new();
        let mut visited = HashSet::new();
        let mut truncated = false;

        self.trace_forward_recursive(
            start_id,
            0,
            max_depth,
            &mut nodes,
            &mut visited,
            &mut truncated,
        );

        Ok(SignalTrace {
            start_signal: start_name.to_string(),
            direction: TraceDirection::Forward,
            nodes,
            max_depth,
            truncated,
        })
    }

    fn trace_forward_recursive(
        &self,
        net_id: GateNetId,
        depth: usize,
        max_depth: usize,
        nodes: &mut Vec<TraceNode>,
        visited: &mut HashSet<u32>,
        truncated: &mut bool,
    ) {
        if visited.contains(&net_id.0) {
            return;
        }
        visited.insert(net_id.0);

        if depth > max_depth {
            *truncated = true;
            return;
        }

        let net = &self.netlist.nets[net_id.0 as usize];

        // For forward trace, we show the fanout cells
        let trace_node = TraceNode {
            net_name: net.name.clone(),
            net_id,
            is_input: net.is_input,
            is_output: net.is_output,
            driver_cell: None, // We'll show fanout instead
            depth,
        };

        nodes.push(trace_node);

        // Trace through fanout
        for (cell_id, _pin_idx) in &net.fanout {
            if let Some(cell) = self.cell_by_id.get(&cell_id.0) {
                // Trace to each output of this cell
                for &output_id in &cell.outputs {
                    self.trace_forward_recursive(
                        output_id,
                        depth + 1,
                        max_depth,
                        nodes,
                        visited,
                        truncated,
                    );
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_trace_direction() {
        assert_eq!(TraceDirection::Backward, TraceDirection::Backward);
        assert_ne!(TraceDirection::Backward, TraceDirection::Forward);
    }
}
