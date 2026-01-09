//! NCL (Null Convention Logic) Wavefront Simulation
//!
//! Provides accurate simulation of NCL async circuits including:
//! - Stateful THmn threshold gate evaluation with hysteresis
//! - NULL/DATA wavefront propagation
//! - Completion detection and phase tracking
//!
//! # NCL Basics
//!
//! NCL uses dual-rail encoding where each logical bit has two wires (t, f):
//! - `(0, 0)` = NULL (spacer phase)
//! - `(0, 1)` = DATA_FALSE (logical 0)
//! - `(1, 0)` = DATA_TRUE (logical 1)
//! - `(1, 1)` = Invalid (should never occur in correct NCL circuit)
//!
//! THmn gates are m-of-n threshold gates with hysteresis:
//! - Output goes HIGH when at least m of n inputs are HIGH
//! - Output goes LOW only when ALL inputs are LOW
//! - Otherwise, output HOLDS its previous value (hysteresis)
//!
//! # Wavefront Simulation
//!
//! NCL circuits operate without clocks using alternating NULL and DATA wavefronts:
//! 1. DATA wavefront: Valid data propagates through the circuit
//! 2. NULL wavefront: Spacer values reset the circuit for next computation
//!
//! The simulation iterates until stability (no signal changes).
//!
//! # Example
//!
//! ```ignore
//! use skalp_sim::ncl_sim::{NclSimulator, NclSimConfig};
//!
//! let mut sim = NclSimulator::new(gate_netlist, NclSimConfig::default());
//!
//! // Apply DATA inputs (dual-rail encoded)
//! sim.set_dual_rail("a", 0, true);  // a = DATA_TRUE (1)
//! sim.set_dual_rail("b", 0, false); // b = DATA_FALSE (0)
//!
//! // Run until stable
//! let iterations = sim.run_until_stable(1000);
//!
//! // Check completion
//! if sim.is_complete() {
//!     let result = sim.get_dual_rail("y", 0);
//! }
//! ```

use skalp_lir::gate_netlist::{CellId, GateNetId, GateNetlist};
use skalp_lir::PrimitiveType;
use std::collections::HashMap;

/// NCL signal phase
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum NclPhase {
    /// NULL phase: all dual-rail signals are (0, 0)
    #[default]
    Null,
    /// DATA phase: all dual-rail signals are valid (0,1) or (1,0)
    Data,
    /// Transitioning between phases (some NULL, some DATA)
    Transitioning,
}

/// Value of a dual-rail NCL signal
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NclValue {
    /// NULL state: (t=0, f=0)
    Null,
    /// DATA_FALSE: (t=0, f=1) = logical 0
    DataFalse,
    /// DATA_TRUE: (t=1, f=0) = logical 1
    DataTrue,
    /// Invalid: (t=1, f=1) - should not occur
    Invalid,
}

impl NclValue {
    /// Create from true/false rails
    pub fn from_rails(t: bool, f: bool) -> Self {
        match (t, f) {
            (false, false) => NclValue::Null,
            (false, true) => NclValue::DataFalse,
            (true, false) => NclValue::DataTrue,
            (true, true) => NclValue::Invalid,
        }
    }

    /// Get the true rail value
    pub fn true_rail(&self) -> bool {
        matches!(self, NclValue::DataTrue | NclValue::Invalid)
    }

    /// Get the false rail value
    pub fn false_rail(&self) -> bool {
        matches!(self, NclValue::DataFalse | NclValue::Invalid)
    }

    /// Check if this is a valid data value
    pub fn is_data(&self) -> bool {
        matches!(self, NclValue::DataTrue | NclValue::DataFalse)
    }

    /// Check if this is NULL
    pub fn is_null(&self) -> bool {
        matches!(self, NclValue::Null)
    }

    /// Convert to logical boolean (only valid for DATA states)
    pub fn to_bool(&self) -> Option<bool> {
        match self {
            NclValue::DataTrue => Some(true),
            NclValue::DataFalse => Some(false),
            _ => None,
        }
    }
}

/// Configuration for NCL simulation
#[derive(Debug, Clone)]
pub struct NclSimConfig {
    /// Maximum iterations per step before declaring oscillation
    pub max_iterations: u32,
    /// Enable verbose debug output
    pub debug: bool,
    /// Track completion per stage (for pipelined designs)
    pub track_stages: bool,
}

impl Default for NclSimConfig {
    fn default() -> Self {
        Self {
            max_iterations: 10000,
            debug: false,
            track_stages: false,
        }
    }
}

/// State of NCL threshold gates (for hysteresis behavior)
#[derive(Debug, Clone)]
pub struct NclGateState {
    /// Current output state for each THmn cell
    /// Key is CellId, value is output state
    th_states: HashMap<CellId, bool>,
}

impl NclGateState {
    pub fn new() -> Self {
        Self {
            th_states: HashMap::new(),
        }
    }

    /// Get the current state of a threshold gate (defaults to false)
    pub fn get(&self, cell_id: CellId) -> bool {
        self.th_states.get(&cell_id).copied().unwrap_or(false)
    }

    /// Set the state of a threshold gate
    pub fn set(&mut self, cell_id: CellId, state: bool) {
        self.th_states.insert(cell_id, state);
    }
}

impl Default for NclGateState {
    fn default() -> Self {
        Self::new()
    }
}

/// Statistics from NCL simulation
#[derive(Debug, Clone, Default)]
pub struct NclSimStats {
    /// Total iterations run
    pub iterations: u64,
    /// Number of wavefronts completed
    pub wavefronts: u64,
    /// Number of NULL→DATA transitions
    pub data_wavefronts: u64,
    /// Number of DATA→NULL transitions
    pub null_wavefronts: u64,
    /// Signals that changed in last iteration
    pub last_changes: u32,
    /// Whether simulation is stable
    pub is_stable: bool,
    /// Current phase
    pub phase: NclPhase,
}

/// NCL wavefront simulator
pub struct NclSimulator {
    /// The gate netlist being simulated
    netlist: GateNetlist,
    /// Configuration
    config: NclSimConfig,
    /// Current net values (single-bit per net)
    net_values: Vec<bool>,
    /// Previous net values (for change detection)
    prev_values: Vec<bool>,
    /// THmn gate states (for hysteresis)
    gate_state: NclGateState,
    /// Simulation statistics
    stats: NclSimStats,
    /// Input net IDs (for setting inputs)
    input_nets: HashMap<String, Vec<GateNetId>>,
    /// Output net IDs (for reading outputs)
    output_nets: HashMap<String, Vec<GateNetId>>,
}

impl NclSimulator {
    /// Create a new NCL simulator for the given netlist
    pub fn new(netlist: GateNetlist, config: NclSimConfig) -> Self {
        let net_count = netlist.nets.len();

        // Collect input and output nets by name
        let mut input_nets: HashMap<String, Vec<GateNetId>> = HashMap::new();
        let mut output_nets: HashMap<String, Vec<GateNetId>> = HashMap::new();

        for net in &netlist.nets {
            if net.is_input {
                // Group by base name (strip bit index)
                let base_name = strip_bit_suffix(&net.name);
                input_nets.entry(base_name).or_default().push(net.id);
            }
            if net.is_output {
                let base_name = strip_bit_suffix(&net.name);
                output_nets.entry(base_name).or_default().push(net.id);
            }
        }

        // Build a lookup map for net names (do this once, not per signal group)
        let net_names: std::collections::HashMap<u32, &str> = netlist
            .nets
            .iter()
            .map(|n| (n.id.0, n.name.as_str()))
            .collect();

        // Sort nets in each signal group to ensure proper dual-rail ordering:
        // [t0, t1, ..., tN-1, f0, f1, ..., fN-1]
        for (_base_name, nets) in input_nets.iter_mut() {
            sort_dual_rail_nets(nets, &net_names);
        }
        for (_base_name, nets) in output_nets.iter_mut() {
            sort_dual_rail_nets(nets, &net_names);
        }

        Self {
            netlist,
            config,
            net_values: vec![false; net_count],
            prev_values: vec![false; net_count],
            gate_state: NclGateState::new(),
            stats: NclSimStats::default(),
            input_nets,
            output_nets,
        }
    }

    /// Set a single-bit net value
    ///
    /// If the net is an alias (e.g., after buffer removal), resolves to the canonical net.
    pub fn set_net(&mut self, net_id: GateNetId, value: bool) {
        // Resolve alias chain first (handles nets orphaned by buffer removal)
        let resolved_id = self.netlist.resolve_alias(net_id);

        if (resolved_id.0 as usize) < self.net_values.len() {
            self.net_values[resolved_id.0 as usize] = value;
        }
    }

    /// Get a single-bit net value
    ///
    /// If the net is an alias (e.g., after buffer removal), resolves to the canonical net.
    pub fn get_net(&self, net_id: GateNetId) -> bool {
        // Resolve alias chain first (handles nets orphaned by buffer removal)
        let resolved_id = self.netlist.resolve_alias(net_id);

        self.net_values
            .get(resolved_id.0 as usize)
            .copied()
            .unwrap_or(false)
    }

    /// Set a dual-rail NCL signal by name and bit index
    /// For a signal "x" bit 0: x_t[0] (true rail) and x_f[0] (false rail)
    ///
    /// Storage layout: all t rails first, then all f rails
    /// For width N: indices 0..N are t rails, indices N..2N are f rails
    pub fn set_dual_rail(&mut self, name: &str, bit: usize, value: NclValue) {
        // Debug: always print for bit 0
        if bit == 0 {
            eprintln!(
                "[NCL_SIM DEBUG] set_dual_rail called: name='{}' bit={}",
                name, bit
            );
        }
        // Extract net IDs first to avoid borrow conflict
        let (t_net, f_net, width) = {
            if let Some(nets) = self.input_nets.get(name) {
                // Layout: [t0, t1, ..., tN-1, f0, f1, ..., fN-1]
                // Width is half the total nets
                let width = nets.len() / 2;
                let t_idx = bit;
                let f_idx = width + bit;
                (nets.get(t_idx).copied(), nets.get(f_idx).copied(), width)
            } else {
                if bit == 0 {
                    eprintln!(
                        "[NCL_SIM] set_dual_rail: input '{}' not found. Available: {:?}",
                        name,
                        self.input_nets.keys().collect::<Vec<_>>()
                    );
                }
                (None, None, 0)
            }
        };

        if let Some(t_net) = t_net {
            self.set_net(t_net, value.true_rail());
            if bit == 0 {
                eprintln!(
                    "[NCL_SIM] set_dual_rail: '{}' bit {} t_net={:?} f_net={:?} width={} value={:?}",
                    name, bit, t_net, f_net, width, value
                );
            }
        }
        if let Some(f_net) = f_net {
            self.set_net(f_net, value.false_rail());
        }
    }

    /// Set a dual-rail NCL input from a boolean value
    pub fn set_dual_rail_bool(&mut self, name: &str, bit: usize, value: bool) {
        let ncl_value = if value {
            NclValue::DataTrue
        } else {
            NclValue::DataFalse
        };
        self.set_dual_rail(name, bit, ncl_value);
    }

    /// Set all bits of a dual-rail signal to NULL
    pub fn set_null(&mut self, name: &str, width: usize) {
        for bit in 0..width {
            self.set_dual_rail(name, bit, NclValue::Null);
        }
    }

    /// Set all bits of a dual-rail signal from an integer value
    pub fn set_dual_rail_value(&mut self, name: &str, value: u64, width: usize) {
        for bit in 0..width {
            // For bits >= 64, they're always 0 since value is u64
            let bit_value = if bit < 64 {
                (value >> bit) & 1 != 0
            } else {
                false
            };
            self.set_dual_rail_bool(name, bit, bit_value);
        }
    }

    /// Set all bits of a dual-rail signal from a u128 value (for signals up to 128 bits)
    pub fn set_dual_rail_value_u128(&mut self, name: &str, value: u128, width: usize) {
        for bit in 0..width {
            // For bits >= 128, they're always 0 since value is u128
            let bit_value = if bit < 128 {
                (value >> bit) & 1 != 0
            } else {
                false
            };
            self.set_dual_rail_bool(name, bit, bit_value);
        }
    }

    /// Set all bits of a dual-rail signal from two u128 values (for signals up to 256 bits)
    /// low = bits [127:0], high = bits [255:128]
    pub fn set_dual_rail_value_u256(&mut self, name: &str, low: u128, high: u128, width: usize) {
        for bit in 0..width {
            let bit_value = if bit < 128 {
                (low >> bit) & 1 != 0
            } else if bit < 256 {
                (high >> (bit - 128)) & 1 != 0
            } else {
                false
            };
            self.set_dual_rail_bool(name, bit, bit_value);
        }
    }

    /// Get a dual-rail NCL signal value
    ///
    /// Storage layout: all t rails first, then all f rails
    /// For width N: indices 0..N are t rails, indices N..2N are f rails
    pub fn get_dual_rail(&self, name: &str, bit: usize) -> NclValue {
        if let Some(nets) = self.output_nets.get(name) {
            // Layout: [t0, t1, ..., tN-1, f0, f1, ..., fN-1]
            // Width is half the total nets
            let width = nets.len() / 2;
            let t_idx = bit;
            let f_idx = width + bit;

            let t = nets.get(t_idx).map(|&n| self.get_net(n)).unwrap_or(false);
            let f = nets.get(f_idx).map(|&n| self.get_net(n)).unwrap_or(false);

            NclValue::from_rails(t, f)
        } else {
            NclValue::Null
        }
    }

    /// Get a multi-bit dual-rail value as integer (returns None if any bit is NULL/Invalid)
    /// Note: Only the lower 64 bits are returned in the u64 result
    pub fn get_dual_rail_value(&self, name: &str, width: usize) -> Option<u64> {
        let mut result = 0u64;
        for bit in 0..width {
            match self.get_dual_rail(name, bit).to_bool() {
                Some(true) => {
                    // Only set bits 0-63, bits >= 64 don't fit in u64
                    if bit < 64 {
                        result |= 1 << bit;
                    }
                }
                Some(false) => {}
                None => return None, // Not valid data
            }
        }
        Some(result)
    }

    /// Check if all outputs are complete (all bits are valid DATA)
    pub fn is_complete(&self) -> bool {
        for (name, nets) in &self.output_nets {
            let width = nets.len() / 2;
            for bit in 0..width {
                if !self.get_dual_rail(name, bit).is_data() {
                    return false;
                }
            }
        }
        true
    }

    /// Check if all outputs are NULL
    pub fn is_null(&self) -> bool {
        for (name, nets) in &self.output_nets {
            let width = nets.len() / 2;
            for bit in 0..width {
                if !self.get_dual_rail(name, bit).is_null() {
                    return false;
                }
            }
        }
        true
    }

    /// Get list of input signal names (for debugging)
    pub fn input_names(&self) -> Vec<&String> {
        self.input_nets.keys().collect()
    }

    /// Get list of output signal names (for debugging)
    pub fn output_names(&self) -> Vec<&String> {
        self.output_nets.keys().collect()
    }

    /// Get the current phase based on output state
    pub fn get_phase(&self) -> NclPhase {
        if self.is_complete() {
            NclPhase::Data
        } else if self.is_null() {
            NclPhase::Null
        } else {
            NclPhase::Transitioning
        }
    }

    /// Run one iteration of combinational propagation
    /// Returns the number of signals that changed
    pub fn iterate(&mut self) -> u32 {
        // Save previous values for change detection
        self.prev_values.clone_from(&self.net_values);

        // First pass: collect all cell info and inputs (avoid borrow conflicts)
        let cell_evals: Vec<_> = self
            .netlist
            .cells
            .iter()
            .map(|cell| {
                let inputs: Vec<bool> = cell
                    .inputs
                    .iter()
                    .map(|&net_id| {
                        self.net_values
                            .get(net_id.0 as usize)
                            .copied()
                            .unwrap_or(false)
                    })
                    .collect();
                let outputs: Vec<GateNetId> = cell.outputs.clone();
                (cell.id, cell.cell_type.clone(), inputs, outputs)
            })
            .collect();

        let mut changes = 0u32;

        // Second pass: evaluate cells and apply outputs
        for (cell_id, cell_type, inputs, output_nets) in cell_evals {
            let outputs = self.evaluate_cell_stateful(cell_id, &cell_type, &inputs);

            for (i, output_net) in output_nets.iter().enumerate() {
                if let Some(&new_value) = outputs.get(i) {
                    let old_value = self.get_net(*output_net);
                    if new_value != old_value {
                        self.set_net(*output_net, new_value);
                        changes += 1;
                    }
                }
            }
        }

        self.stats.iterations += 1;
        self.stats.last_changes = changes;
        changes
    }

    /// Run simulation until stable (no signal changes) or max iterations
    /// Returns number of iterations taken
    pub fn run_until_stable(&mut self, max_iterations: u32) -> u32 {
        let max = max_iterations.min(self.config.max_iterations);
        let mut iterations = 0;

        loop {
            let changes = self.iterate();
            iterations += 1;

            if changes == 0 {
                self.stats.is_stable = true;
                break;
            }

            if iterations >= max {
                if self.config.debug {
                    eprintln!(
                        "[NCL_SIM] Warning: Max iterations ({}) reached with {} changes",
                        max, changes
                    );
                }
                self.stats.is_stable = false;
                break;
            }
        }

        // Update phase
        self.stats.phase = self.get_phase();
        iterations
    }

    /// Advance one complete wavefront (DATA or NULL)
    /// Returns true if wavefront completed, false if stuck
    pub fn advance_wavefront(&mut self) -> bool {
        let start_phase = self.get_phase();

        let iterations = self.run_until_stable(self.config.max_iterations);

        if self.stats.is_stable {
            let end_phase = self.get_phase();

            if start_phase != end_phase {
                self.stats.wavefronts += 1;
                match end_phase {
                    NclPhase::Data => self.stats.data_wavefronts += 1,
                    NclPhase::Null => self.stats.null_wavefronts += 1,
                    _ => {}
                }
            }

            if self.config.debug {
                eprintln!(
                    "[NCL_SIM] Wavefront complete: {:?} -> {:?} in {} iterations",
                    start_phase, end_phase, iterations
                );
            }

            true
        } else {
            false
        }
    }

    /// Reset all states to NULL/false
    pub fn reset(&mut self) {
        self.net_values.fill(false);
        self.prev_values.fill(false);
        self.gate_state = NclGateState::new();
        self.stats = NclSimStats::default();
    }

    /// Get simulation statistics
    pub fn stats(&self) -> &NclSimStats {
        &self.stats
    }

    /// Evaluate a cell with stateful THmn handling
    fn evaluate_cell_stateful(
        &mut self,
        cell_id: CellId,
        cell_type: &str,
        inputs: &[bool],
    ) -> Vec<bool> {
        // Parse cell type to determine primitive
        let ptype = self.parse_cell_type(cell_type);

        match &ptype {
            // THmn gates with hysteresis
            PrimitiveType::Th12 => {
                vec![self.evaluate_thmn(cell_id, 1, 2, inputs)]
            }
            PrimitiveType::Th22 => {
                vec![self.evaluate_thmn(cell_id, 2, 2, inputs)]
            }
            PrimitiveType::Th13 => {
                vec![self.evaluate_thmn(cell_id, 1, 3, inputs)]
            }
            PrimitiveType::Th23 => {
                vec![self.evaluate_thmn(cell_id, 2, 3, inputs)]
            }
            PrimitiveType::Th33 => {
                vec![self.evaluate_thmn(cell_id, 3, 3, inputs)]
            }
            PrimitiveType::Th14 => {
                vec![self.evaluate_thmn(cell_id, 1, 4, inputs)]
            }
            PrimitiveType::Th24 => {
                vec![self.evaluate_thmn(cell_id, 2, 4, inputs)]
            }
            PrimitiveType::Th34 => {
                vec![self.evaluate_thmn(cell_id, 3, 4, inputs)]
            }
            PrimitiveType::Th44 => {
                vec![self.evaluate_thmn(cell_id, 4, 4, inputs)]
            }
            PrimitiveType::Thmn { m, n } => {
                vec![self.evaluate_thmn(cell_id, *m as usize, *n as usize, inputs)]
            }
            PrimitiveType::ThmnW { m, n, weights } => {
                vec![self.evaluate_thmn_weighted(cell_id, *m, *n, inputs, weights)]
            }
            PrimitiveType::NclCompletion { width } => {
                vec![self.evaluate_completion(*width as usize, inputs)]
            }
            // Non-THmn gates: use standard stateless evaluation
            _ => crate::gate_eval::evaluate_primitive(&ptype, inputs),
        }
    }

    /// Evaluate THmn threshold gate with hysteresis
    ///
    /// THmn behavior:
    /// - Output = 1 when count(inputs) >= m
    /// - Output = 0 when all inputs are 0
    /// - Output = previous state otherwise (hysteresis/hold)
    fn evaluate_thmn(&mut self, cell_id: CellId, m: usize, n: usize, inputs: &[bool]) -> bool {
        let count: usize = inputs.iter().take(n).filter(|&&x| x).count();
        let all_low = count == 0;

        let prev_state = self.gate_state.get(cell_id);

        let new_state = if count >= m {
            // Threshold met: output HIGH
            true
        } else if all_low {
            // All inputs LOW: output LOW
            false
        } else {
            // Hold previous state (hysteresis)
            prev_state
        };

        self.gate_state.set(cell_id, new_state);
        new_state
    }

    /// Evaluate weighted THmn threshold gate with hysteresis
    fn evaluate_thmn_weighted(
        &mut self,
        cell_id: CellId,
        m: u8,
        n: u8,
        inputs: &[bool],
        weights: &[u8],
    ) -> bool {
        let weighted_sum: u32 = inputs
            .iter()
            .take(n as usize)
            .zip(weights.iter())
            .map(|(&input, &weight)| if input { weight as u32 } else { 0 })
            .sum();

        let all_low = !inputs.iter().take(n as usize).any(|&x| x);
        let prev_state = self.gate_state.get(cell_id);

        let new_state = if weighted_sum >= m as u32 {
            true
        } else if all_low {
            false
        } else {
            prev_state
        };

        self.gate_state.set(cell_id, new_state);
        new_state
    }

    /// Evaluate NCL completion detector
    fn evaluate_completion(&self, width: usize, inputs: &[bool]) -> bool {
        let mut all_data = true;
        let mut all_null = true;

        for i in 0..width {
            let t = inputs.get(i * 2).copied().unwrap_or(false);
            let f = inputs.get(i * 2 + 1).copied().unwrap_or(false);

            let is_data = t ^ f; // Exactly one rail high
            let is_null = !t && !f; // Both rails low

            all_data = all_data && is_data;
            all_null = all_null && is_null;
        }

        all_data || all_null
    }

    /// Parse cell type string to PrimitiveType
    fn parse_cell_type(&self, cell_type: &str) -> PrimitiveType {
        let upper = cell_type.to_uppercase();

        // Strip common library suffixes like "_X1", "_X2", "_M" etc.
        let base_type = if let Some(pos) = upper.rfind("_X") {
            &upper[..pos]
        } else if let Some(pos) = upper.rfind("_M") {
            &upper[..pos]
        } else {
            &upper
        };

        // Check for THmn patterns (use base_type which has library suffix stripped)
        if base_type.starts_with("TH") {
            if let Some(digits) = base_type.strip_prefix("TH") {
                // Parse the first two chars as m and n digits
                let m = digits.chars().next().and_then(|c| c.to_digit(10));
                let n = digits.chars().nth(1).and_then(|c| c.to_digit(10));
                if let (Some(m), Some(n)) = (m, n) {
                    return match (m, n) {
                        (1, 2) => PrimitiveType::Th12,
                        (2, 2) => PrimitiveType::Th22,
                        (1, 3) => PrimitiveType::Th13,
                        (2, 3) => PrimitiveType::Th23,
                        (3, 3) => PrimitiveType::Th33,
                        (1, 4) => PrimitiveType::Th14,
                        (2, 4) => PrimitiveType::Th24,
                        (3, 4) => PrimitiveType::Th34,
                        (4, 4) => PrimitiveType::Th44,
                        _ => PrimitiveType::Thmn {
                            m: m as u8,
                            n: n as u8,
                        },
                    };
                }
            }
        }

        // Check for NCL completion
        if upper.starts_with("NCL_COMPLETE") {
            if let Some(width_str) = upper.strip_prefix("NCL_COMPLETE") {
                if let Ok(width) = width_str.parse::<u32>() {
                    return PrimitiveType::NclCompletion { width };
                }
            }
        }

        // FP32 soft macros - need explicit handling for NCL simulation
        // Tech library generates fp_add32 (uppercase: FP_ADD32), also match FP32_ADD variants
        if base_type == "FP32_ADD" || upper.contains("FP32_ADD") || upper.starts_with("FP_ADD32") {
            return PrimitiveType::Fp32Add;
        }
        if base_type == "FP32_SUB" || upper.contains("FP32_SUB") || upper.starts_with("FP_SUB32") {
            return PrimitiveType::Fp32Sub;
        }
        if base_type == "FP32_MUL" || upper.contains("FP32_MUL") || upper.starts_with("FP_MUL32") {
            return PrimitiveType::Fp32Mul;
        }
        if base_type == "FP32_DIV" || upper.contains("FP32_DIV") || upper.starts_with("FP_DIV32") {
            return PrimitiveType::Fp32Div;
        }

        // FP32 comparisons (BUG #191 FIX)
        if base_type == "FP32_LT" || upper.contains("FP32_LT") || upper.starts_with("FP_LT32") {
            return PrimitiveType::Fp32Lt;
        }
        if base_type == "FP32_GT" || upper.contains("FP32_GT") || upper.starts_with("FP_GT32") {
            return PrimitiveType::Fp32Gt;
        }
        if base_type == "FP32_LE" || upper.contains("FP32_LE") || upper.starts_with("FP_LE32") {
            return PrimitiveType::Fp32Le;
        }
        if base_type == "FP32_GE" || upper.contains("FP32_GE") || upper.starts_with("FP_GE32") {
            return PrimitiveType::Fp32Ge;
        }

        // Common gates - use base_type which has library suffixes stripped
        match base_type {
            "INV" | "NOT" => PrimitiveType::Inv,
            "BUF" | "BUFFER" => PrimitiveType::Buf,
            "AND2" => PrimitiveType::And { inputs: 2 },
            "AND3" => PrimitiveType::And { inputs: 3 },
            "AND4" => PrimitiveType::And { inputs: 4 },
            "OR2" => PrimitiveType::Or { inputs: 2 },
            "OR3" => PrimitiveType::Or { inputs: 3 },
            "OR4" => PrimitiveType::Or { inputs: 4 },
            "NAND2" => PrimitiveType::Nand { inputs: 2 },
            "NAND3" => PrimitiveType::Nand { inputs: 3 },
            "NAND4" => PrimitiveType::Nand { inputs: 4 },
            "NOR2" => PrimitiveType::Nor { inputs: 2 },
            "NOR3" => PrimitiveType::Nor { inputs: 3 },
            "NOR4" => PrimitiveType::Nor { inputs: 4 },
            "XOR2" | "XOR" => PrimitiveType::Xor,
            "XNOR2" | "XNOR" => PrimitiveType::Xnor,
            "MUX2" => PrimitiveType::Mux2,
            "MUX4" => PrimitiveType::Mux4,
            "HA" | "HALFADDER" | "HALF_ADDER" => PrimitiveType::HalfAdder,
            "FA" | "FULLADDER" | "FULL_ADDER" => PrimitiveType::FullAdder,
            "TIE_HIGH" | "TIEH" | "VDD" => PrimitiveType::Constant { value: true },
            "TIE_LOW" | "TIEL" | "GND" | "VSS" => PrimitiveType::Constant { value: false },
            _ => {
                // Default to buffer for unknown types
                if self.config.debug {
                    eprintln!(
                        "[NCL_SIM] Unknown cell type: {} (base: {})",
                        cell_type, base_type
                    );
                }
                PrimitiveType::Buf
            }
        }
    }
}

/// Strip bit suffix from signal name
/// For multi-bit dual-rail signals like "a_t[0]", strips BOTH suffixes to get "a"
/// Examples:
/// - "data[3]" -> "data"
/// - "data_t0" -> "data"
/// - "a_t[0]" -> "a" (strips [0] first, then _t)
/// - "a_f[7]" -> "a" (strips [7] first, then _f)
fn strip_bit_suffix(name: &str) -> String {
    let mut result = name.to_string();

    // Step 1: Strip [N] suffix if present
    if let Some(bracket_pos) = result.find('[') {
        result = result[..bracket_pos].to_string();
    }

    // Step 2: Strip _t or _f suffix for dual-rail signals
    // Handles both "_t" (without digit) and "_t0", "_t1", etc.
    if let Some(underscore_pos) = result.rfind('_') {
        let suffix = &result[underscore_pos + 1..];
        // Check for "_t" or "_f" optionally followed by digits
        if (suffix == "t" || suffix == "f")
            || ((suffix.starts_with('t') || suffix.starts_with('f'))
                && suffix[1..].chars().all(|c| c.is_ascii_digit()))
        {
            result = result[..underscore_pos].to_string();
        }
    }

    result
}

/// Sort nets for proper dual-rail ordering: [t0, t1, ..., tN-1, f0, f1, ..., fN-1]
///
/// This ensures that `set_dual_rail_value` and `get_dual_rail_value` can correctly
/// map bit positions to the right rails.
fn sort_dual_rail_nets(nets: &mut [GateNetId], net_names: &std::collections::HashMap<u32, &str>) {
    // Parse each net to extract (is_false_rail, bit_index)
    // Returns (false, bit_index) for true rail, (true, bit_index) for false rail
    // This way sorting naturally groups t rails first, then f rails, each sorted by bit index
    let get_sort_key = |net_id: &GateNetId| -> (bool, usize) {
        if let Some(&name) = net_names.get(&net_id.0) {
            // Check for _t or _f suffix patterns
            if let Some(underscore_pos) = name.rfind('_') {
                let suffix = &name[underscore_pos + 1..];

                // Handle _t (1-bit true rail) or _f (1-bit false rail)
                if suffix == "t" {
                    return (false, 0); // true rail, bit 0
                }
                if suffix == "f" {
                    return (true, 0); // false rail, bit 0
                }

                // Handle _tN or _fN patterns
                if let Some(rest) = suffix.strip_prefix('t') {
                    if let Ok(bit) = rest.parse::<usize>() {
                        return (false, bit); // true rail
                    }
                }
                if let Some(rest) = suffix.strip_prefix('f') {
                    if let Ok(bit) = rest.parse::<usize>() {
                        return (true, bit); // false rail
                    }
                }
            }

            // Check for [N] suffix with _t or _f before it
            // e.g., "signal_t[5]" or "signal_f[5]"
            if let Some(bracket_pos) = name.find('[') {
                let before_bracket = &name[..bracket_pos];
                let bit_str = &name[bracket_pos + 1..name.len().saturating_sub(1)];
                let bit = bit_str.parse::<usize>().unwrap_or(0);

                if before_bracket.ends_with("_t") {
                    return (false, bit); // true rail
                }
                if before_bracket.ends_with("_f") {
                    return (true, bit); // false rail
                }
            }
        }
        // Default: treat as true rail bit 0 (shouldn't happen for valid NCL nets)
        (false, 0)
    };

    nets.sort_by_key(get_sort_key);
}

/// Evaluate a THmn gate with explicit previous state (for use in gate_eval.rs)
pub fn evaluate_thmn_stateful(m: usize, n: usize, inputs: &[bool], prev_state: bool) -> bool {
    let count: usize = inputs.iter().take(n).filter(|&&x| x).count();
    let all_low = count == 0;

    if count >= m {
        true
    } else if all_low {
        false
    } else {
        prev_state
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ncl_value() {
        assert_eq!(NclValue::from_rails(false, false), NclValue::Null);
        assert_eq!(NclValue::from_rails(false, true), NclValue::DataFalse);
        assert_eq!(NclValue::from_rails(true, false), NclValue::DataTrue);
        assert_eq!(NclValue::from_rails(true, true), NclValue::Invalid);

        assert!(NclValue::Null.is_null());
        assert!(NclValue::DataTrue.is_data());
        assert!(NclValue::DataFalse.is_data());
        assert!(!NclValue::Invalid.is_data());

        assert_eq!(NclValue::DataTrue.to_bool(), Some(true));
        assert_eq!(NclValue::DataFalse.to_bool(), Some(false));
        assert_eq!(NclValue::Null.to_bool(), None);
    }

    #[test]
    fn test_th22_hysteresis() {
        // TH22 (C-element) behavior:
        // - Output HIGH when both inputs HIGH
        // - Output LOW when both inputs LOW
        // - Hold otherwise

        let prev = false;

        // Both high -> output high
        assert!(evaluate_thmn_stateful(2, 2, &[true, true], prev));

        // Both low -> output low
        assert!(!evaluate_thmn_stateful(2, 2, &[false, false], prev));

        // One high, one low -> hold previous
        assert!(!evaluate_thmn_stateful(2, 2, &[true, false], false));
        assert!(evaluate_thmn_stateful(2, 2, &[true, false], true));
    }

    #[test]
    fn test_th12_hysteresis() {
        // TH12 (OR with hysteresis):
        // - Output HIGH when at least 1 input HIGH
        // - Output LOW when all inputs LOW
        // - Hold otherwise (but for TH12, either threshold or all-low)

        // At least one high -> output high
        assert!(evaluate_thmn_stateful(1, 2, &[true, false], false));
        assert!(evaluate_thmn_stateful(1, 2, &[false, true], false));
        assert!(evaluate_thmn_stateful(1, 2, &[true, true], false));

        // Both low -> output low
        assert!(!evaluate_thmn_stateful(1, 2, &[false, false], true));
    }

    #[test]
    fn test_strip_bit_suffix() {
        assert_eq!(strip_bit_suffix("data[3]"), "data");
        assert_eq!(strip_bit_suffix("data_t0"), "data");
        assert_eq!(strip_bit_suffix("data_f5"), "data");
        assert_eq!(strip_bit_suffix("simple"), "simple");
        assert_eq!(strip_bit_suffix("with_underscore"), "with_underscore");
    }
}
