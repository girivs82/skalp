//! Gate-Level Netlist - Output of Technology Mapping
//!
//! This module provides a gate-level representation of hardware designs,
//! where each cell is a library primitive with associated FIT rates.
//!
//! # Flow
//!
//! ```text
//! WordLir (word-level) → TechMapper → GateNetlist (gate-level with library cells)
//!                                          ↓
//!                              Fault Simulation / FMEDA
//! ```
//!
//! Unlike `Lir` (technology-independent), `GateNetlist` uses specific library
//! cells with known FIT rates and failure modes from the technology library.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;

// ============================================================================
// Cell Types
// ============================================================================

/// Unique identifier for a cell in the netlist
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct CellId(pub u32);

/// Unique identifier for a net (wire)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct GateNetId(pub u32);

/// A cell instance in the gate netlist
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Cell {
    /// Unique identifier
    pub id: CellId,
    /// Library cell type name (e.g., "NAND2_X1", "FA_X1", "DFF_X1")
    pub cell_type: String,
    /// Library name this cell comes from
    pub library: String,
    /// FIT rate for this cell (from library)
    pub fit: f64,
    /// Failure modes with their FIT contributions
    pub failure_modes: Vec<CellFailureMode>,
    /// Input net IDs
    pub inputs: Vec<GateNetId>,
    /// Output net IDs
    pub outputs: Vec<GateNetId>,
    /// Hierarchical path for traceability (e.g., "top.counter.adder.bit3")
    pub path: String,
    /// Clock net (for sequential cells)
    pub clock: Option<GateNetId>,
    /// Reset net (for sequential cells with reset)
    pub reset: Option<GateNetId>,
    /// Source WordLir operation this came from (for debug/traceability)
    pub source_op: Option<String>,
}

impl Cell {
    /// Create a new combinational cell
    pub fn new_comb(
        id: CellId,
        cell_type: String,
        library: String,
        fit: f64,
        path: String,
        inputs: Vec<GateNetId>,
        outputs: Vec<GateNetId>,
    ) -> Self {
        Self {
            id,
            cell_type,
            library,
            fit,
            failure_modes: Vec::new(),
            inputs,
            outputs,
            path,
            clock: None,
            reset: None,
            source_op: None,
        }
    }

    /// Create a new sequential cell
    #[allow(clippy::too_many_arguments)]
    pub fn new_seq(
        id: CellId,
        cell_type: String,
        library: String,
        fit: f64,
        path: String,
        inputs: Vec<GateNetId>,
        outputs: Vec<GateNetId>,
        clock: GateNetId,
        reset: Option<GateNetId>,
    ) -> Self {
        Self {
            id,
            cell_type,
            library,
            fit,
            failure_modes: Vec::new(),
            inputs,
            outputs,
            path,
            clock: Some(clock),
            reset,
            source_op: None,
        }
    }

    /// Check if this is a sequential cell
    pub fn is_sequential(&self) -> bool {
        self.clock.is_some()
    }
}

/// Failure mode for a cell
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CellFailureMode {
    /// Name of the failure mode (e.g., "stuck_at_0", "stuck_at_1", "transient")
    pub name: String,
    /// FIT contribution for this failure mode
    pub fit: f64,
    /// Fault type for simulation
    pub fault_type: FaultType,
}

/// Types of faults for simulation
///
/// These fault types align with ISO 26262 failure mode categories for FMEDA analysis.
/// Each fault type has different implications for safety:
/// - Permanent faults (StuckAt, Open, Bridge) require hardware safety mechanisms
/// - Transient faults (Transient, Delay) may be masked by temporal redundancy
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum FaultType {
    /// Output stuck at 0 (permanent)
    StuckAt0,
    /// Output stuck at 1 (permanent)
    StuckAt1,
    /// Single-cycle transient glitch (soft error)
    Transient,
    /// Timing violation (setup/hold failure)
    Timing,
    /// Bridge fault (short between nets, permanent)
    Bridge,
    /// Open circuit fault (high impedance, permanent)
    Open,
    /// Delay fault (signal arrives late but correct value)
    Delay,
    /// Data retention failure (sequential cells)
    DataRetention,
    /// Clock path failure (sequential cells)
    ClockPath,
    /// Reset path failure (sequential cells with reset)
    ResetPath,
}

// ============================================================================
// Net Types
// ============================================================================

/// A net (wire) in the gate netlist
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GateNet {
    /// Unique identifier
    pub id: GateNetId,
    /// Net name
    pub name: String,
    /// Driver cell (None for primary inputs)
    pub driver: Option<CellId>,
    /// Driver output pin index
    pub driver_pin: Option<usize>,
    /// Fan-out cells (list of (cell_id, input_pin_index))
    pub fanout: Vec<(CellId, usize)>,
    /// Is this a primary input?
    pub is_input: bool,
    /// Is this a primary output?
    pub is_output: bool,
    /// Is this a clock net?
    pub is_clock: bool,
    /// Is this a reset net?
    pub is_reset: bool,
}

impl GateNet {
    /// Create a new internal net
    pub fn new(id: GateNetId, name: String) -> Self {
        Self {
            id,
            name,
            driver: None,
            driver_pin: None,
            fanout: Vec::new(),
            is_input: false,
            is_output: false,
            is_clock: false,
            is_reset: false,
        }
    }

    /// Create a new primary input net
    pub fn new_input(id: GateNetId, name: String) -> Self {
        Self {
            id,
            name,
            driver: None,
            driver_pin: None,
            fanout: Vec::new(),
            is_input: true,
            is_output: false,
            is_clock: false,
            is_reset: false,
        }
    }

    /// Create a new primary output net
    pub fn new_output(id: GateNetId, name: String) -> Self {
        Self {
            id,
            name,
            driver: None,
            driver_pin: None,
            fanout: Vec::new(),
            is_input: false,
            is_output: true,
            is_clock: false,
            is_reset: false,
        }
    }
}

// ============================================================================
// Gate Netlist
// ============================================================================

/// Gate-level netlist with library cells
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GateNetlist {
    /// Design name
    pub name: String,
    /// Technology library used
    pub library_name: String,
    /// All cells
    pub cells: Vec<Cell>,
    /// All nets
    pub nets: Vec<GateNet>,
    /// Primary input net IDs
    pub inputs: Vec<GateNetId>,
    /// Primary output net IDs
    pub outputs: Vec<GateNetId>,
    /// Clock net IDs
    pub clocks: Vec<GateNetId>,
    /// Reset net IDs
    pub resets: Vec<GateNetId>,
    /// Net name to ID mapping
    net_map: HashMap<String, GateNetId>,
    /// Statistics
    pub stats: GateNetlistStats,
}

impl GateNetlist {
    /// Create a new empty gate netlist
    pub fn new(name: String, library_name: String) -> Self {
        Self {
            name,
            library_name,
            cells: Vec::new(),
            nets: Vec::new(),
            inputs: Vec::new(),
            outputs: Vec::new(),
            clocks: Vec::new(),
            resets: Vec::new(),
            net_map: HashMap::new(),
            stats: GateNetlistStats::default(),
        }
    }

    /// Add a net and return its ID
    pub fn add_net(&mut self, mut net: GateNet) -> GateNetId {
        let id = GateNetId(self.nets.len() as u32);
        net.id = id;
        self.net_map.insert(net.name.clone(), id);
        self.nets.push(net);
        id
    }

    /// Add an input net
    pub fn add_input(&mut self, name: String) -> GateNetId {
        let id = self.add_net(GateNet::new_input(GateNetId(0), name));
        self.inputs.push(id);
        id
    }

    /// Add an output net
    pub fn add_output(&mut self, name: String) -> GateNetId {
        let id = self.add_net(GateNet::new_output(GateNetId(0), name));
        self.outputs.push(id);
        id
    }

    /// Add a clock net
    pub fn add_clock(&mut self, name: String) -> GateNetId {
        let mut net = GateNet::new_input(GateNetId(0), name);
        net.is_clock = true;
        let id = self.add_net(net);
        self.inputs.push(id);
        self.clocks.push(id);
        id
    }

    /// Add a reset net
    pub fn add_reset(&mut self, name: String) -> GateNetId {
        let mut net = GateNet::new_input(GateNetId(0), name);
        net.is_reset = true;
        let id = self.add_net(net);
        self.inputs.push(id);
        self.resets.push(id);
        id
    }

    /// Add a cell and return its ID
    pub fn add_cell(&mut self, mut cell: Cell) -> CellId {
        let id = CellId(self.cells.len() as u32);
        cell.id = id;

        // Update net connectivity
        for (pin, &net_id) in cell.outputs.iter().enumerate() {
            if let Some(net) = self.nets.get_mut(net_id.0 as usize) {
                net.driver = Some(id);
                net.driver_pin = Some(pin);
            }
        }
        for (pin, &net_id) in cell.inputs.iter().enumerate() {
            if let Some(net) = self.nets.get_mut(net_id.0 as usize) {
                net.fanout.push((id, pin));
            }
        }

        self.cells.push(cell);
        id
    }

    /// Get a net by name
    pub fn get_net(&self, name: &str) -> Option<&GateNet> {
        self.net_map.get(name).map(|id| &self.nets[id.0 as usize])
    }

    /// Get a mutable net by ID
    pub fn get_net_mut(&mut self, id: GateNetId) -> Option<&mut GateNet> {
        self.nets.get_mut(id.0 as usize)
    }

    /// Get a cell by ID
    pub fn get_cell(&self, id: CellId) -> Option<&Cell> {
        self.cells.get(id.0 as usize)
    }

    /// Update statistics
    pub fn update_stats(&mut self) {
        self.stats = GateNetlistStats::from_netlist(self);
    }

    /// Total cell count
    pub fn cell_count(&self) -> usize {
        self.cells.len()
    }

    /// Total net count
    pub fn net_count(&self) -> usize {
        self.nets.len()
    }

    /// Total FIT
    pub fn total_fit(&self) -> f64 {
        self.cells.iter().map(|c| c.fit).sum()
    }
}

// ============================================================================
// Statistics
// ============================================================================

/// Statistics for a gate netlist
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct GateNetlistStats {
    /// Total cells
    pub total_cells: usize,
    /// Total nets
    pub total_nets: usize,
    /// Total FIT
    pub total_fit: f64,
    /// Sequential cells
    pub sequential_cells: usize,
    /// Combinational cells
    pub combinational_cells: usize,
    /// Cell type distribution
    pub cell_types: HashMap<String, usize>,
    /// Library distribution
    pub libraries: HashMap<String, usize>,
}

impl GateNetlistStats {
    /// Calculate statistics from a gate netlist
    pub fn from_netlist(netlist: &GateNetlist) -> Self {
        let mut stats = Self {
            total_cells: netlist.cells.len(),
            total_nets: netlist.nets.len(),
            total_fit: netlist.total_fit(),
            ..Default::default()
        };

        for cell in &netlist.cells {
            if cell.is_sequential() {
                stats.sequential_cells += 1;
            } else {
                stats.combinational_cells += 1;
            }

            *stats.cell_types.entry(cell.cell_type.clone()).or_insert(0) += 1;
            *stats.libraries.entry(cell.library.clone()).or_insert(0) += 1;
        }

        stats
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_gate_netlist_creation() {
        let mut netlist = GateNetlist::new("test".to_string(), "generic_asic".to_string());

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
            "NAND2_X1".to_string(),
            "generic_asic".to_string(),
            0.1,
            "top.and".to_string(),
            vec![a, b],
            vec![and_out],
        );
        netlist.add_cell(and_cell);

        // Add buffer
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

        assert_eq!(netlist.cell_count(), 2);
        assert_eq!(netlist.net_count(), 4);
        assert_eq!(netlist.inputs.len(), 2);
        assert_eq!(netlist.outputs.len(), 1);
    }

    #[test]
    fn test_gate_netlist_fit() {
        let mut netlist = GateNetlist::new("test".to_string(), "generic_asic".to_string());

        let a = netlist.add_input("a".to_string());
        let y = netlist.add_output("y".to_string());

        let cell = Cell::new_comb(
            CellId(0),
            "INV_X1".to_string(),
            "generic_asic".to_string(),
            0.05,
            "top.inv".to_string(),
            vec![a],
            vec![y],
        );
        netlist.add_cell(cell);

        assert!((netlist.total_fit() - 0.05).abs() < 0.001);
    }

    #[test]
    fn test_sequential_cell() {
        let mut netlist = GateNetlist::new("test".to_string(), "generic_asic".to_string());

        let clk = netlist.add_clock("clk".to_string());
        let d = netlist.add_input("d".to_string());
        let q = netlist.add_output("q".to_string());

        let dff = Cell::new_seq(
            CellId(0),
            "DFF_X1".to_string(),
            "generic_asic".to_string(),
            0.2,
            "top.dff".to_string(),
            vec![d],
            vec![q],
            clk,
            None,
        );
        netlist.add_cell(dff);

        assert!(netlist.cells[0].is_sequential());
        assert_eq!(netlist.clocks.len(), 1);
    }

    #[test]
    fn test_netlist_stats() {
        let mut netlist = GateNetlist::new("test".to_string(), "generic_asic".to_string());

        let a = netlist.add_input("a".to_string());
        let b = netlist.add_input("b".to_string());
        let out = netlist.add_output("out".to_string());

        let cell1 = Cell::new_comb(
            CellId(0),
            "NAND2_X1".to_string(),
            "generic_asic".to_string(),
            0.1,
            "top.nand".to_string(),
            vec![a, b],
            vec![out],
        );
        netlist.add_cell(cell1);

        netlist.update_stats();

        assert_eq!(netlist.stats.total_cells, 1);
        assert_eq!(netlist.stats.combinational_cells, 1);
        assert_eq!(netlist.stats.sequential_cells, 0);
        assert_eq!(netlist.stats.cell_types.get("NAND2_X1"), Some(&1));
    }

    #[test]
    fn test_failure_modes() {
        let mut cell = Cell::new_comb(
            CellId(0),
            "NAND2_X1".to_string(),
            "generic_asic".to_string(),
            0.1,
            "top.nand".to_string(),
            vec![GateNetId(0), GateNetId(1)],
            vec![GateNetId(2)],
        );

        cell.failure_modes.push(CellFailureMode {
            name: "stuck_at_0".to_string(),
            fit: 0.05,
            fault_type: FaultType::StuckAt0,
        });

        cell.failure_modes.push(CellFailureMode {
            name: "stuck_at_1".to_string(),
            fit: 0.05,
            fault_type: FaultType::StuckAt1,
        });

        assert_eq!(cell.failure_modes.len(), 2);
        let total_failure_fit: f64 = cell.failure_modes.iter().map(|f| f.fit).sum();
        assert!((total_failure_fit - 0.1).abs() < 0.001);
    }
}
