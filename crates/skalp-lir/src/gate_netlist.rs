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

use crate::lir::LirSafetyInfo;
use serde::{Deserialize, Serialize};
use skalp_frontend::hir::DetectionConfig;
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
    /// Safety classification: Functional vs SafetyMechanism
    /// Used for ISO 26262 FMEDA to track λSM (safety mechanism failure rate)
    pub safety_classification: CellSafetyClassification,
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
            safety_classification: CellSafetyClassification::default(),
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
            safety_classification: CellSafetyClassification::default(),
        }
    }

    /// Create a new sequential cell with enable input (e.g., SDFFE)
    ///
    /// The inputs are ordered as [data, enable] to match standard cell conventions.
    #[allow(clippy::too_many_arguments)]
    pub fn new_seq_with_enable(
        id: CellId,
        cell_type: String,
        library: String,
        fit: f64,
        path: String,
        data: GateNetId,
        enable: GateNetId,
        output: GateNetId,
        clock: GateNetId,
        reset: Option<GateNetId>,
    ) -> Self {
        Self {
            id,
            cell_type,
            library,
            fit,
            failure_modes: Vec::new(),
            inputs: vec![data, enable], // [D, E] ordering
            outputs: vec![output],
            path,
            clock: Some(clock),
            reset,
            source_op: None,
            safety_classification: CellSafetyClassification::default(),
        }
    }

    /// Set the safety classification for this cell
    pub fn with_safety_classification(mut self, classification: CellSafetyClassification) -> Self {
        self.safety_classification = classification;
        self
    }

    /// Check if this cell is part of a safety mechanism
    pub fn is_safety_mechanism(&self) -> bool {
        self.safety_classification.is_safety_mechanism()
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
// Safety Classification
// ============================================================================

/// Classification of a cell's role in the safety architecture
///
/// Used for ISO 26262 FMEDA to distinguish between:
/// - Functional logic cells (contribute to λSPF or λRF)
/// - Safety mechanism cells (contribute to λSM)
///
/// Safety mechanism failures must be tracked separately because:
/// 1. SM hardware can fail (voter gate stuck, watchdog FF fails)
/// 2. SM failure eliminates protection for covered functional logic
/// 3. SM failure combined with functional fault = hazard (dependent failure)
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Default)]
pub enum CellSafetyClassification {
    /// Functional logic cell (not part of any safety mechanism)
    /// Failures contribute to λSPF (if unprotected) or λRF (if protected)
    #[default]
    Functional,

    /// Safety mechanism cell
    /// Failures contribute to λSM in PMHF calculation
    SafetyMechanism {
        /// Safety goal this mechanism implements (e.g., "BrakingSafety")
        goal_name: String,
        /// Mechanism name within the goal (e.g., "TmrVoting")
        mechanism_name: String,
    },

    /// Safety mechanism protecting another safety mechanism
    /// Used for hierarchical SM structures (e.g., watchdog monitoring a TMR voter)
    SafetyMechanismOfSm {
        /// The SM being protected
        protected_sm_name: String,
        /// Safety goal of the protecting mechanism
        goal_name: String,
        /// Mechanism name of the protector
        mechanism_name: String,
    },
}

impl CellSafetyClassification {
    /// Check if this cell is part of a safety mechanism
    pub fn is_safety_mechanism(&self) -> bool {
        !matches!(self, CellSafetyClassification::Functional)
    }

    /// Get the mechanism name if this is an SM cell
    pub fn mechanism_name(&self) -> Option<&str> {
        match self {
            CellSafetyClassification::SafetyMechanism { mechanism_name, .. } => {
                Some(mechanism_name)
            }
            CellSafetyClassification::SafetyMechanismOfSm { mechanism_name, .. } => {
                Some(mechanism_name)
            }
            CellSafetyClassification::Functional => None,
        }
    }

    /// Get the goal name if this is an SM cell
    pub fn goal_name(&self) -> Option<&str> {
        match self {
            CellSafetyClassification::SafetyMechanism { goal_name, .. } => Some(goal_name),
            CellSafetyClassification::SafetyMechanismOfSm { goal_name, .. } => Some(goal_name),
            CellSafetyClassification::Functional => None,
        }
    }

    /// Create CellSafetyClassification from LirSafetyInfo
    /// Used during technology mapping to propagate safety annotations from LIR to gate-level cells
    pub fn from_lir_safety_info(info: &LirSafetyInfo) -> Self {
        // If this is a safety mechanism of another SM
        if info.is_sm_of_sm {
            if let (Some(goal), Some(mechanism), Some(protected)) = (
                info.goal_name.as_ref(),
                info.mechanism_name.as_ref(),
                info.protected_sm_name.as_ref(),
            ) {
                return CellSafetyClassification::SafetyMechanismOfSm {
                    protected_sm_name: protected.clone(),
                    goal_name: goal.clone(),
                    mechanism_name: mechanism.clone(),
                };
            }
        }

        // If this has a mechanism name, it's a safety mechanism
        // Note: goal_name may be None for standalone safety mechanisms not yet associated with a goal
        if let Some(mechanism) = info.mechanism_name.as_ref() {
            return CellSafetyClassification::SafetyMechanism {
                // Use goal_name if provided, otherwise use a placeholder for standalone SMs
                goal_name: info
                    .goal_name
                    .clone()
                    .unwrap_or_else(|| "unassigned".to_string()),
                mechanism_name: mechanism.clone(),
            };
        }

        // If this has a goal_name but no mechanism, it's still a safety mechanism
        // (e.g., signals marked with #[implements(GoalName)])
        if let Some(goal) = info.goal_name.as_ref() {
            return CellSafetyClassification::SafetyMechanism {
                goal_name: goal.clone(),
                mechanism_name: "unspecified".to_string(),
            };
        }

        // Default to Functional if no safety information
        CellSafetyClassification::Functional
    }
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
    /// Is this a detection signal (for safety analysis)?
    /// Set via #[detection_signal] attribute on port
    pub is_detection: bool,
    /// Detection signal configuration (temporal mode for safety analysis)
    /// Includes mode (continuous/boot/periodic) and optional interval
    /// Note: Not using skip_serializing_if because bincode uses positional encoding
    pub detection_config: Option<DetectionConfig>,
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
            is_detection: false,
            detection_config: None,
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
            is_detection: false,
            detection_config: None,
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
            is_detection: false,
            detection_config: None,
        }
    }

    /// Create a new detection signal output net
    pub fn new_detection_output(id: GateNetId, name: String) -> Self {
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
            is_detection: true,
            detection_config: Some(DetectionConfig::default()), // Default to continuous mode
        }
    }

    /// Create a new detection signal output net with specific config
    pub fn new_detection_output_with_config(
        id: GateNetId,
        name: String,
        config: DetectionConfig,
    ) -> Self {
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
            is_detection: true,
            detection_config: Some(config),
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
    /// True if this is an NCL (Null Convention Logic) asynchronous circuit.
    /// Set by tech_mapper when dual-rail signals are detected.
    #[serde(default)]
    pub is_ncl: bool,
    /// Net name to ID mapping (skipped during serialization - rebuilt from nets)
    #[serde(skip, default)]
    net_map: HashMap<String, GateNetId>,
    /// Statistics (skipped during serialization - rebuilt from cells/nets)
    #[serde(skip, default)]
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
            is_ncl: false,
            net_map: HashMap::new(),
            stats: GateNetlistStats::default(),
        }
    }

    /// Rebuild the net_map and stats after deserialization.
    /// Must be called after deserializing a GateNetlist since these fields
    /// are skipped during serialization for deterministic checksums.
    pub fn rebuild_cache(&mut self) {
        // Rebuild net_map from nets
        self.net_map.clear();
        for net in &self.nets {
            self.net_map.insert(net.name.clone(), net.id);
        }

        // Rebuild stats
        self.stats = GateNetlistStats::from_netlist(self);
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

    /// Get a net ID by name
    pub fn get_net_id(&self, name: &str) -> Option<GateNetId> {
        self.net_map.get(name).copied()
    }

    /// Rebuild driver and fanout information from cells
    /// Useful after net merging to ensure connectivity is accurate
    pub fn rebuild_net_connectivity(&mut self) {
        // Count tie cells before rebuild
        let tie_cells: Vec<_> = self
            .cells
            .iter()
            .filter(|c| c.cell_type.starts_with("TIE"))
            .map(|c| (c.id.0, &c.cell_type, c.outputs.first().cloned()))
            .collect();
        eprintln!(
            "[REBUILD_CONN] Found {} tie cells before rebuild",
            tie_cells.len()
        );

        // Clear all existing driver/fanout info
        for net in &mut self.nets {
            net.driver = None;
            net.driver_pin = None;
            net.fanout.clear();
        }

        // Rebuild from cells
        for cell in &self.cells {
            // Update drivers
            for (pin, &net_id) in cell.outputs.iter().enumerate() {
                if let Some(net) = self.nets.get_mut(net_id.0 as usize) {
                    net.driver = Some(cell.id);
                    net.driver_pin = Some(pin);
                }
            }
            // Update fanout
            for (pin, &net_id) in cell.inputs.iter().enumerate() {
                if let Some(net) = self.nets.get_mut(net_id.0 as usize) {
                    net.fanout.push((cell.id, pin));
                }
            }
        }

        eprintln!(
            "[REBUILD_CONN] Rebuild complete, {} total cells",
            self.cells.len()
        );
    }

    /// Get a mutable net by ID
    pub fn get_net_mut(&mut self, id: GateNetId) -> Option<&mut GateNet> {
        self.nets.get_mut(id.0 as usize)
    }

    /// Get a cell by ID
    pub fn get_cell(&self, id: CellId) -> Option<&Cell> {
        self.cells.get(id.0 as usize)
    }

    /// Remove dead cells whose outputs have no fanout and aren't primary outputs
    ///
    /// This is useful for cleaning up redundant cells that were created during
    /// technology mapping but whose outputs are never used.
    ///
    /// Note: Blackbox cells and sequential cells are never considered dead.
    pub fn remove_dead_cells(&mut self) -> usize {
        use std::collections::HashSet;

        // Identify output nets that must be preserved
        let output_nets: HashSet<GateNetId> = self.outputs.iter().copied().collect();

        // Identify cells that are dead (all outputs have no fanout and aren't primary outputs)
        let dead_cells: HashSet<CellId> = self
            .cells
            .iter()
            .filter(|cell| {
                // A cell is dead if all its outputs have no fanout and aren't primary outputs
                // Sequential cells (latches/FFs) are never considered dead
                if cell.clock.is_some() {
                    return false;
                }

                // Blackbox cells (vendor IP) are never considered dead
                // They represent external IP that must be preserved
                if cell.cell_type.starts_with("BLACKBOX_")
                    || cell
                        .source_op
                        .as_ref()
                        .is_some_and(|op| op.starts_with("blackbox:"))
                {
                    return false;
                }

                // TIE cells (constant generators) must be preserved
                // They drive constant values to child module ports
                if cell.cell_type.starts_with("TIE") {
                    return false;
                }

                cell.outputs.iter().all(|&out_net| {
                    // Check if this output net is a primary output
                    if output_nets.contains(&out_net) {
                        return false;
                    }
                    // Check if this output net has any fanout
                    if let Some(net) = self.nets.get(out_net.0 as usize) {
                        net.fanout.is_empty()
                    } else {
                        true // Net doesn't exist, treat as dead
                    }
                })
            })
            .map(|cell| cell.id)
            .collect();

        if dead_cells.is_empty() {
            return 0;
        }

        let removed_count = dead_cells.len();

        // Remove dead cells by filtering
        self.cells.retain(|cell| !dead_cells.contains(&cell.id));

        // Re-assign cell IDs and update net references
        let mut old_to_new: HashMap<CellId, CellId> = HashMap::new();
        for (new_idx, cell) in self.cells.iter_mut().enumerate() {
            old_to_new.insert(cell.id, CellId(new_idx as u32));
            cell.id = CellId(new_idx as u32);
        }

        // Update net driver references
        for net in &mut self.nets {
            if let Some(old_driver) = net.driver {
                if let Some(&new_driver) = old_to_new.get(&old_driver) {
                    net.driver = Some(new_driver);
                } else {
                    // Driver was removed
                    net.driver = None;
                    net.driver_pin = None;
                }
            }
            // Update fanout references
            net.fanout
                .retain(|(cell_id, _)| old_to_new.contains_key(cell_id));
            for (cell_id, _) in &mut net.fanout {
                if let Some(&new_id) = old_to_new.get(cell_id) {
                    *cell_id = new_id;
                }
            }
        }

        removed_count
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

    /// Generate structural Verilog from this gate netlist
    pub fn to_verilog(&self) -> String {
        let mut output = String::new();

        // Module header
        output.push_str(&format!(
            "// Generated by SKALP - Gate-level netlist\n// Library: {}\n\n",
            self.library_name
        ));

        // Collect port names
        let input_names: Vec<_> = self
            .inputs
            .iter()
            .map(|id| self.nets[id.0 as usize].name.clone())
            .collect();
        let output_names: Vec<_> = self
            .outputs
            .iter()
            .map(|id| self.nets[id.0 as usize].name.clone())
            .collect();

        // Module declaration
        output.push_str(&format!("module {} (\n", self.name));

        // Input ports
        for (i, name) in input_names.iter().enumerate() {
            let comma = if i < input_names.len() - 1 || !output_names.is_empty() {
                ","
            } else {
                ""
            };
            output.push_str(&format!("    input wire {}{}\n", name, comma));
        }

        // Output ports
        for (i, name) in output_names.iter().enumerate() {
            let comma = if i < output_names.len() - 1 { "," } else { "" };
            output.push_str(&format!("    output wire {}{}\n", name, comma));
        }

        output.push_str(");\n\n");

        // Internal wires
        output.push_str("    // Internal wires\n");
        for net in &self.nets {
            // Skip ports
            if net.is_input || net.is_output {
                continue;
            }
            output.push_str(&format!("    wire {};\n", net.name));
        }
        output.push('\n');

        // Cell instantiations
        output.push_str("    // Cell instances\n");
        for cell in &self.cells {
            // For blackbox cells, use the original IP name without BLACKBOX_ prefix
            let cell_type_display = if cell.cell_type.starts_with("BLACKBOX_") {
                // Strip the BLACKBOX_ prefix for cleaner output
                &cell.cell_type["BLACKBOX_".len()..]
            } else {
                &cell.cell_type
            };
            output.push_str(&format!("    {} U{} (\n", cell_type_display, cell.id.0));

            // Collect all port connections
            let mut connections = Vec::new();

            // Add input connections
            for (i, input_id) in cell.inputs.iter().enumerate() {
                let net_name = &self.nets[input_id.0 as usize].name;
                // For blackbox cells, derive pin name from net name
                let pin_name = get_input_pin_name_with_net(&cell.cell_type, i, Some(net_name));
                connections.push(format!("        .{}({})", pin_name, net_name));
            }

            // Add output connections
            for (i, output_id) in cell.outputs.iter().enumerate() {
                let net_name = &self.nets[output_id.0 as usize].name;
                // For blackbox cells, derive pin name from net name
                let pin_name = get_output_pin_name_with_net(&cell.cell_type, i, Some(net_name));
                connections.push(format!("        .{}({})", pin_name, net_name));
            }

            // Add clock if sequential
            if let Some(clk_id) = cell.clock {
                let clk_name = &self.nets[clk_id.0 as usize].name;
                connections.push(format!("        .CLK({})", clk_name));
            }

            // Add reset if present
            if let Some(rst_id) = cell.reset {
                let rst_name = &self.nets[rst_id.0 as usize].name;
                connections.push(format!("        .RST({})", rst_name));
            }

            // Write connections
            for (i, conn) in connections.iter().enumerate() {
                let comma = if i < connections.len() - 1 { "," } else { "" };
                output.push_str(&format!("{}{}\n", conn, comma));
            }

            output.push_str("    );\n\n");
        }

        output.push_str("endmodule\n");

        // Generate NCL cell library definitions if NCL cells are used
        let ncl_lib = self.generate_ncl_library();
        if !ncl_lib.is_empty() {
            output.push_str("\n// ============================================\n");
            output.push_str("// NCL (Null Convention Logic) Cell Library\n");
            output.push_str("// ============================================\n\n");
            output.push_str(&ncl_lib);
        }

        output
    }

    /// Generate NCL cell library Verilog definitions for THmn gates used in this netlist
    fn generate_ncl_library(&self) -> String {
        use std::collections::HashSet;

        let mut output = String::new();
        let mut generated: HashSet<String> = HashSet::new();

        for cell in &self.cells {
            let cell_type = &cell.cell_type;

            // Check for THmn gates (e.g., TH12, TH22, TH23, TH33, TH44)
            if cell_type.starts_with("TH")
                && cell_type.len() >= 4
                && cell_type[2..].chars().all(|c| c.is_ascii_digit())
            {
                if generated.contains(cell_type) {
                    continue;
                }
                generated.insert(cell_type.clone());

                // Parse m and n from THmn
                let m = cell_type[2..3].parse::<u8>().unwrap_or(1);
                let n = cell_type[3..4].parse::<u8>().unwrap_or(2);

                output.push_str(&generate_thmn_module(m, n));
                output.push('\n');
            }

            // NCL completion detection
            if cell_type == "NCL_COMPLETION" && !generated.contains(cell_type) {
                generated.insert(cell_type.clone());
                output.push_str(&generate_ncl_completion_module(cell.inputs.len()));
                output.push('\n');
            }

            // NCL register
            if cell_type == "NCL_REG" && !generated.contains(cell_type) {
                generated.insert(cell_type.clone());
                output.push_str(&generate_ncl_reg_module());
                output.push('\n');
            }
        }

        output
    }

    // =========================================================================
    // Hierarchical Synthesis Support
    // =========================================================================

    /// Add a net with a specific name and return its ID
    pub fn add_net_with_name(&mut self, name: String) -> GateNetId {
        // Check if net already exists
        if let Some(&existing_id) = self.net_map.get(&name) {
            return existing_id;
        }

        let id = GateNetId(self.nets.len() as u32);
        let net = GateNet {
            id,
            name: name.clone(),
            driver: None,
            driver_pin: None,
            fanout: Vec::new(),
            is_input: false,
            is_output: false,
            is_clock: false,
            is_reset: false,
            is_detection: false,
            detection_config: None,
        };
        self.net_map.insert(name, id);
        self.nets.push(net);
        id
    }

    /// Merge two nets by name (for stitching hierarchical boundaries)
    /// All connections to net2 are redirected to net1
    pub fn merge_nets_by_name(&mut self, net1_name: &str, net2_name: &str) {
        let net1_id = match self.net_map.get(net1_name) {
            Some(&id) => id,
            None => return, // Net doesn't exist
        };
        let net2_id = match self.net_map.get(net2_name) {
            Some(&id) => id,
            None => return, // Net doesn't exist
        };

        if net1_id == net2_id {
            return; // Same net, nothing to do
        }

        // Update all cells that reference net2 to use net1
        for cell in &mut self.cells {
            for input in &mut cell.inputs {
                if *input == net2_id {
                    *input = net1_id;
                }
            }
            for output in &mut cell.outputs {
                if *output == net2_id {
                    *output = net1_id;
                }
            }
            if cell.clock == Some(net2_id) {
                cell.clock = Some(net1_id);
            }
            if cell.reset == Some(net2_id) {
                cell.reset = Some(net1_id);
            }
        }

        // Merge fanout from net2 into net1
        let net2_fanout = self.nets[net2_id.0 as usize].fanout.clone();
        self.nets[net1_id.0 as usize].fanout.extend(net2_fanout);

        // If net2 has a driver, transfer it to net1 if net1 doesn't have one
        if self.nets[net1_id.0 as usize].driver.is_none() {
            self.nets[net1_id.0 as usize].driver = self.nets[net2_id.0 as usize].driver;
            self.nets[net1_id.0 as usize].driver_pin = self.nets[net2_id.0 as usize].driver_pin;
        }

        // Mark net2 as merged (clear its connections)
        self.nets[net2_id.0 as usize].fanout.clear();
        self.nets[net2_id.0 as usize].driver = None;

        // Update net_map so lookups for net2_name now resolve to net1
        // This is important for correct I/O determination after stitching
        self.net_map.insert(net2_name.to_string(), net1_id);
    }

    /// Add a tie cell for a constant value
    pub fn add_tie_cell(&mut self, net_name: &str, value: u64) {
        let net_id = match self.net_map.get(net_name) {
            Some(&id) => id,
            None => self.add_net_with_name(net_name.to_string()),
        };

        let cell_type = if value == 0 {
            "TIE0_X1".to_string()
        } else {
            "TIE1_X1".to_string()
        };

        let cell_id = CellId(self.cells.len() as u32);
        let cell = Cell::new_comb(
            cell_id,
            cell_type,
            self.library_name.clone(),
            0.0, // Tie cells have negligible FIT
            format!("tie_{}", net_name),
            vec![],
            vec![net_id],
        );
        self.cells.push(cell);

        // Update net driver
        self.nets[net_id.0 as usize].driver = Some(cell_id);
        self.nets[net_id.0 as usize].driver_pin = Some(0);
    }

    /// Find all bit-indexed nets matching a prefix (e.g., "signal" matches "signal[0]", "signal[1]", etc.)
    /// Returns a sorted list of (bit_index, net_name) pairs
    pub fn find_bit_indexed_nets(&self, prefix: &str) -> Vec<(usize, String)> {
        let mut result = Vec::new();

        // Pattern: prefix[N] where N is a non-negative integer
        for name in self.net_map.keys() {
            if let Some(rest) = name.strip_prefix(prefix) {
                if let Some(inner) = rest.strip_prefix('[') {
                    if let Some(idx_str) = inner.strip_suffix(']') {
                        if let Ok(idx) = idx_str.parse::<usize>() {
                            result.push((idx, name.clone()));
                        }
                    }
                }
            }
        }

        // Sort by bit index
        result.sort_by_key(|(idx, _)| *idx);
        result
    }

    /// Find all NCL dual-rail bit-indexed nets matching a prefix
    ///
    /// For NCL signals, the naming convention is:
    /// - `signal_t[N]` for true rail of bit N
    /// - `signal_f[N]` for false rail of bit N
    ///
    /// Returns two sorted lists: (true_rail_nets, false_rail_nets)
    /// Each list contains (bit_index, net_name) pairs.
    /// For 1-bit signals, the net name is just {prefix}_t without [0], so we check for exact match too.
    #[allow(clippy::type_complexity)]
    pub fn find_ncl_bit_indexed_nets(
        &self,
        prefix: &str,
    ) -> (Vec<(usize, String)>, Vec<(usize, String)>) {
        let mut true_rail = Vec::new();
        let mut false_rail = Vec::new();

        let prefix_t = format!("{}_t", prefix);
        let prefix_f = format!("{}_f", prefix);

        for name in self.net_map.keys() {
            // Check for true rail: prefix_t[N] or exact match prefix_t (for 1-bit signals)
            if name == &prefix_t {
                // Exact match - 1-bit signal without bit index
                true_rail.push((0, name.clone()));
            } else if let Some(rest) = name.strip_prefix(&prefix_t) {
                if let Some(inner) = rest.strip_prefix('[') {
                    if let Some(idx_str) = inner.strip_suffix(']') {
                        if let Ok(idx) = idx_str.parse::<usize>() {
                            true_rail.push((idx, name.clone()));
                        }
                    }
                }
            }
            // Check for false rail: prefix_f[N] or exact match prefix_f (for 1-bit signals)
            if name == &prefix_f {
                // Exact match - 1-bit signal without bit index
                false_rail.push((0, name.clone()));
            } else if let Some(rest) = name.strip_prefix(&prefix_f) {
                if let Some(inner) = rest.strip_prefix('[') {
                    if let Some(idx_str) = inner.strip_suffix(']') {
                        if let Ok(idx) = idx_str.parse::<usize>() {
                            false_rail.push((idx, name.clone()));
                        }
                    }
                }
            }
        }

        // Sort by bit index
        true_rail.sort_by_key(|(idx, _)| *idx);
        false_rail.sort_by_key(|(idx, _)| *idx);

        (true_rail, false_rail)
    }

    /// Propagate constants through the netlist (lightweight optimization)
    pub fn propagate_constants(&mut self) {
        // Find tie cells and their driven nets
        let mut constant_nets: HashMap<GateNetId, bool> = HashMap::new();

        for cell in &self.cells {
            if cell.cell_type.starts_with("TIE0") {
                for &output in &cell.outputs {
                    constant_nets.insert(output, false);
                }
            } else if cell.cell_type.starts_with("TIE1") {
                for &output in &cell.outputs {
                    constant_nets.insert(output, true);
                }
            }
        }

        // For now, just record constants - full propagation would require
        // evaluating each cell type's truth table
        // This is a placeholder for more sophisticated constant propagation
        let _ = constant_nets;
    }
}

/// Get input pin name for a cell type
fn get_input_pin_name(cell_type: &str, index: usize) -> String {
    get_input_pin_name_with_net(cell_type, index, None)
}

/// Get input pin name, optionally using the net name for blackbox cells
fn get_input_pin_name_with_net(cell_type: &str, index: usize, net_name: Option<&str>) -> String {
    // For BLACKBOX cells, extract the port name from the net name
    if cell_type.starts_with("BLACKBOX_") {
        if let Some(name) = net_name {
            // Net names are like "top.pll.clk_ref" - extract the last component
            if let Some(port_name) = name.rsplit('.').next() {
                // Strip any bit index like [0]
                let port_name = port_name.split('[').next().unwrap_or(port_name);
                return port_name.to_string();
            }
        }
        // Fallback: use generic names
        return format!("I{}", index);
    }

    // Common naming conventions for standard cells
    let base = cell_type.split('_').next().unwrap_or(cell_type);

    match base {
        "INV" | "BUF" => "A".to_string(),
        "AND2" | "OR2" | "NAND2" | "NOR2" | "XOR2" | "XNOR2" => {
            if index == 0 {
                "A".to_string()
            } else {
                "B".to_string()
            }
        }
        "AND3" | "OR3" | "NAND3" | "NOR3" => match index {
            0 => "A".to_string(),
            1 => "B".to_string(),
            _ => "C".to_string(),
        },
        "AND4" | "OR4" | "NAND4" | "NOR4" => match index {
            0 => "A".to_string(),
            1 => "B".to_string(),
            2 => "C".to_string(),
            _ => "D".to_string(),
        },
        // MUX2: Y = (S ? B : A), inputs are [sel, d0, d1]
        // so index 0 is sel -> S, index 1 is d0 -> A, index 2 is d1 -> B
        "MUX2" => match index {
            0 => "S".to_string(),
            1 => "A".to_string(),
            _ => "B".to_string(),
        },
        "DFF" | "DFFR" | "DFFS" => "D".to_string(),
        // SDFFE and DFFE have two inputs: D (data) and E (enable)
        // Also handle Yosys-style names like SDFFE_PP0P
        s if s.starts_with("SDFFE") || s.starts_with("DFFE") => match index {
            0 => "D".to_string(),
            _ => "E".to_string(),
        },
        "AOI21" | "OAI21" => match index {
            0 => "A1".to_string(),
            1 => "A2".to_string(),
            _ => "B".to_string(),
        },
        // NCL threshold gates (TH12, TH22, TH23, etc.)
        s if s.starts_with("TH") && s.len() >= 4 && s[2..].chars().all(|c| c.is_ascii_digit()) => {
            // THmn gates use A, B, C, D naming for inputs
            match index {
                0 => "A".to_string(),
                1 => "B".to_string(),
                2 => "C".to_string(),
                3 => "D".to_string(),
                _ => format!("I{}", index),
            }
        }
        // NCL completion detection
        "NCL_COMPLETION" => format!("I{}", index),
        // NCL register (DATA/NULL latch)
        "NCL_REG" => match index {
            0 => "D".to_string(),  // Data input
            _ => "Ki".to_string(), // Acknowledge input
        },
        _ => format!("I{}", index),
    }
}

/// Get output pin name for a cell type
fn get_output_pin_name(cell_type: &str, index: usize) -> String {
    get_output_pin_name_with_net(cell_type, index, None)
}

/// Get output pin name, optionally using the net name for blackbox cells
fn get_output_pin_name_with_net(cell_type: &str, index: usize, net_name: Option<&str>) -> String {
    // For BLACKBOX cells, extract the port name from the net name
    if cell_type.starts_with("BLACKBOX_") {
        if let Some(name) = net_name {
            // Net names are like "top.pll.clk_out" - extract the last component
            if let Some(port_name) = name.rsplit('.').next() {
                // Strip any bit index like [0]
                let port_name = port_name.split('[').next().unwrap_or(port_name);
                return port_name.to_string();
            }
        }
        // Fallback: use generic names
        if index == 0 {
            return "Y".to_string();
        } else {
            return format!("Y{}", index);
        }
    }

    let base = cell_type.split('_').next().unwrap_or(cell_type);

    match base {
        "DFF" | "DFFR" | "DFFS" => {
            if index == 0 {
                "Q".to_string()
            } else {
                "QN".to_string()
            }
        }
        // NCL register (DATA/NULL latch)
        "NCL_REG" => match index {
            0 => "Q".to_string(),  // Data output
            _ => "Ko".to_string(), // Acknowledge output
        },
        // NCL completion detection
        "NCL_COMPLETION" => "Y".to_string(),
        // NCL threshold gates use Y for output
        _ => {
            if index == 0 {
                "Y".to_string()
            } else {
                format!("Y{}", index)
            }
        }
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
    // ===== Safety Mechanism Statistics (ISO 26262) =====
    /// Number of cells classified as safety mechanisms
    pub sm_cell_count: usize,
    /// Number of cells classified as functional logic
    pub functional_cell_count: usize,
    /// Total FIT for safety mechanism cells (contributes to λSM)
    pub sm_fit: f64,
    /// Total FIT for functional cells (contributes to λSPF or λRF)
    pub functional_fit: f64,
    /// Breakdown of SM cells by mechanism name
    pub sm_by_mechanism: HashMap<String, usize>,
    /// FIT breakdown by mechanism name
    pub sm_fit_by_mechanism: HashMap<String, f64>,
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
            // Sequential vs combinational
            if cell.is_sequential() {
                stats.sequential_cells += 1;
            } else {
                stats.combinational_cells += 1;
            }

            // Cell type and library distribution
            *stats.cell_types.entry(cell.cell_type.clone()).or_insert(0) += 1;
            *stats.libraries.entry(cell.library.clone()).or_insert(0) += 1;

            // Safety mechanism vs functional classification
            if cell.safety_classification.is_safety_mechanism() {
                stats.sm_cell_count += 1;
                stats.sm_fit += cell.fit;

                // Track per-mechanism breakdown
                if let Some(mech_name) = cell.safety_classification.mechanism_name() {
                    *stats
                        .sm_by_mechanism
                        .entry(mech_name.to_string())
                        .or_insert(0) += 1;
                    *stats
                        .sm_fit_by_mechanism
                        .entry(mech_name.to_string())
                        .or_insert(0.0) += cell.fit;
                }
            } else {
                stats.functional_cell_count += 1;
                stats.functional_fit += cell.fit;
            }
        }

        stats
    }
}

// ============================================================================
// NCL Cell Library Generation
// ============================================================================

/// Generate Verilog module for a THmn (m-of-n threshold) gate
/// These are state-holding gates with hysteresis behavior:
/// - Output goes HIGH when at least m of n inputs are HIGH
/// - Output goes LOW when all inputs are LOW
/// - Output holds previous value otherwise (hysteresis)
fn generate_thmn_module(m: u8, n: u8) -> String {
    let inputs: Vec<String> = (0..n)
        .map(|i| {
            let c = (b'A' + i) as char;
            c.to_string()
        })
        .collect();
    let input_list = inputs.join(", ");

    let mut verilog = String::new();

    // Module header
    verilog.push_str(&format!(
        "// TH{}{} - {}-of-{} threshold gate (NCL)\n",
        m, n, m, n
    ));
    verilog.push_str(&format!(
        "// Output HIGH when >= {} inputs HIGH, LOW when all inputs LOW, else hold\n",
        m
    ));
    verilog.push_str(&format!(
        "module TH{}{} (input {}, output reg Y);\n",
        m, n, input_list
    ));

    // Generate threshold logic
    verilog.push_str(&format!("    wire [{n}:0] sum = "));
    let sum_terms: Vec<String> = inputs
        .iter()
        .map(|s| format!("{{{}'b0, {}}}", n, s))
        .collect();
    verilog.push_str(&sum_terms.join(" + "));
    verilog.push_str(";\n");

    // All-low detection
    let all_low = inputs
        .iter()
        .map(|s| format!("~{}", s))
        .collect::<Vec<_>>()
        .join(" & ");

    verilog.push_str(&format!("    always @({}) begin\n", input_list));
    verilog.push_str(&format!(
        "        if (sum >= {}'d{}) Y <= 1'b1;\n",
        n + 1,
        m
    ));
    verilog.push_str(&format!("        else if ({}) Y <= 1'b0;\n", all_low));
    verilog.push_str("        // else hold previous value (hysteresis)\n");
    verilog.push_str("    end\n");
    verilog.push_str("endmodule\n");

    verilog
}

/// Generate Verilog module for NCL completion detection
/// Monitors dual-rail signals to detect when all are either DATA or all are NULL
fn generate_ncl_completion_module(width: usize) -> String {
    let mut verilog = String::new();

    verilog.push_str("// NCL_COMPLETION - Completion detection for dual-rail signals\n");
    verilog
        .push_str("// Output HIGH when all inputs are DATA (01 or 10), LOW when all NULL (00)\n");
    verilog.push_str(&format!(
        "module NCL_COMPLETION #(parameter WIDTH = {}) (\n",
        width
    ));
    verilog.push_str("    input [WIDTH-1:0] I,\n");
    verilog.push_str("    output reg Y\n");
    verilog.push_str(");\n");
    verilog.push_str(
        "    // For dual-rail: bit pairs should be (01) or (10) for DATA, (00) for NULL\n",
    );
    verilog.push_str("    // This detects when all pairs are valid DATA\n");
    verilog.push_str("    wire all_null = (I == {WIDTH{1'b0}});\n");
    verilog.push_str("    // Check each bit pair is not 11 (invalid) and not 00 (null)\n");
    verilog.push_str("    integer i;\n");
    verilog.push_str("    reg all_data;\n");
    verilog.push_str("    always @(*) begin\n");
    verilog.push_str("        all_data = 1'b1;\n");
    verilog.push_str("        for (i = 0; i < WIDTH/2; i = i + 1) begin\n");
    verilog.push_str("            if (I[2*i +: 2] == 2'b00 || I[2*i +: 2] == 2'b11)\n");
    verilog.push_str("                all_data = 1'b0;\n");
    verilog.push_str("        end\n");
    verilog.push_str("        if (all_data) Y = 1'b1;\n");
    verilog.push_str("        else if (all_null) Y = 1'b0;\n");
    verilog.push_str("        // else hold\n");
    verilog.push_str("    end\n");
    verilog.push_str("endmodule\n");

    verilog
}

/// Generate Verilog module for NCL register (DATA/NULL latch)
/// A transparent latch that passes DATA when acknowledged, and resets to NULL
fn generate_ncl_reg_module() -> String {
    let mut verilog = String::new();

    verilog.push_str("// NCL_REG - NCL register (DATA/NULL latch) with handshaking\n");
    verilog.push_str("// Captures DATA when Ki (acknowledge input) is high\n");
    verilog.push_str("// Outputs NULL when Ki is low (requesting next DATA)\n");
    verilog.push_str("module NCL_REG (\n");
    verilog.push_str("    input D,     // Dual-rail data input\n");
    verilog.push_str("    input Ki,    // Acknowledge input from downstream\n");
    verilog.push_str("    output reg Q, // Dual-rail data output\n");
    verilog.push_str("    output Ko    // Acknowledge output to upstream\n");
    verilog.push_str(");\n");
    verilog.push_str("    // NCL latch behavior:\n");
    verilog.push_str("    // When Ki=1 (downstream ready), pass DATA through\n");
    verilog.push_str("    // When Ki=0 (downstream not ready), output NULL\n");
    verilog.push_str("    always @(*) begin\n");
    verilog.push_str("        if (Ki)\n");
    verilog.push_str("            Q = D;  // Pass data\n");
    verilog.push_str("        else\n");
    verilog.push_str("            Q = 1'b0; // Output NULL\n");
    verilog.push_str("    end\n");
    verilog.push_str("    // Acknowledge propagates when we have valid output\n");
    verilog.push_str("    assign Ko = Q;\n");
    verilog.push_str("endmodule\n");

    verilog
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

    #[test]
    fn test_safety_classification_default() {
        let cell = Cell::new_comb(
            CellId(0),
            "NAND2_X1".to_string(),
            "generic_asic".to_string(),
            0.1,
            "top.nand".to_string(),
            vec![GateNetId(0)],
            vec![GateNetId(1)],
        );

        // Default classification should be Functional
        assert_eq!(
            cell.safety_classification,
            CellSafetyClassification::Functional
        );
        assert!(!cell.is_safety_mechanism());
    }

    #[test]
    fn test_safety_classification_sm() {
        let cell = Cell::new_comb(
            CellId(0),
            "NAND2_X1".to_string(),
            "generic_asic".to_string(),
            0.1,
            "top.tmr_voter.nand".to_string(),
            vec![GateNetId(0)],
            vec![GateNetId(1)],
        )
        .with_safety_classification(CellSafetyClassification::SafetyMechanism {
            goal_name: "BrakingSafety".to_string(),
            mechanism_name: "TmrVoting".to_string(),
        });

        assert!(cell.is_safety_mechanism());
        assert_eq!(
            cell.safety_classification.mechanism_name(),
            Some("TmrVoting")
        );
        assert_eq!(
            cell.safety_classification.goal_name(),
            Some("BrakingSafety")
        );
    }

    #[test]
    fn test_safety_classification_sm_of_sm() {
        let cell = Cell::new_comb(
            CellId(0),
            "NAND2_X1".to_string(),
            "generic_asic".to_string(),
            0.1,
            "top.watchdog.nand".to_string(),
            vec![GateNetId(0)],
            vec![GateNetId(1)],
        )
        .with_safety_classification(CellSafetyClassification::SafetyMechanismOfSm {
            protected_sm_name: "TmrVoting".to_string(),
            goal_name: "BrakingSafety".to_string(),
            mechanism_name: "Watchdog".to_string(),
        });

        assert!(cell.is_safety_mechanism());
        assert_eq!(
            cell.safety_classification.mechanism_name(),
            Some("Watchdog")
        );
    }

    #[test]
    fn test_netlist_stats_with_sm() {
        let mut netlist = GateNetlist::new("test".to_string(), "generic_asic".to_string());

        let a = netlist.add_input("a".to_string());
        let b = netlist.add_input("b".to_string());
        let c = netlist.add_input("c".to_string());
        let out = netlist.add_output("out".to_string());

        // Add functional cell (FIT = 0.1)
        let functional_cell = Cell::new_comb(
            CellId(0),
            "NAND2_X1".to_string(),
            "generic_asic".to_string(),
            0.1,
            "top.functional.nand".to_string(),
            vec![a, b],
            vec![GateNetId(10)],
        );
        netlist.add_cell(functional_cell);

        // Add SM cell (FIT = 0.2)
        let sm_cell = Cell::new_comb(
            CellId(0),
            "NAND3_X1".to_string(),
            "generic_asic".to_string(),
            0.2,
            "top.tmr.nand".to_string(),
            vec![GateNetId(10), c],
            vec![out],
        )
        .with_safety_classification(CellSafetyClassification::SafetyMechanism {
            goal_name: "BrakingSafety".to_string(),
            mechanism_name: "TmrVoting".to_string(),
        });
        netlist.add_cell(sm_cell);

        netlist.update_stats();

        // Check SM statistics
        assert_eq!(netlist.stats.total_cells, 2);
        assert_eq!(netlist.stats.functional_cell_count, 1);
        assert_eq!(netlist.stats.sm_cell_count, 1);
        assert!((netlist.stats.functional_fit - 0.1).abs() < 0.001);
        assert!((netlist.stats.sm_fit - 0.2).abs() < 0.001);
        assert_eq!(netlist.stats.sm_by_mechanism.get("TmrVoting"), Some(&1));
        assert!((netlist.stats.sm_fit_by_mechanism.get("TmrVoting").unwrap() - 0.2).abs() < 0.001);
    }
}
