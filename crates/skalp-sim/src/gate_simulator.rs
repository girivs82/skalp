//! Gate-Level Simulator
//!
//! A dedicated simulator for gate-level (structural) simulation with
//! first-class support for fault injection and safety analysis.
//!
//! # Features
//!
//! - **Cycle-accurate simulation** of primitive-based netlists
//! - **Fault injection** at any primitive (SA0, SA1, BitFlip, Transient, Bridge)
//! - **FIT tracking** for reliability analysis
//! - **Waveform capture** compatible with VCD viewers
//! - **Batch fault campaigns** for ISO 26262 DC measurement
//!
//! # Example
//!
//! ```ignore
//! use skalp_sim::gate_simulator::GateLevelSimulator;
//! use skalp_sim::sir::FaultInjectionConfig;
//!
//! // Create simulator from SIR
//! let mut sim = GateLevelSimulator::new(&sir);
//!
//! // Run normal simulation
//! sim.set_input("clk", &[0]);
//! sim.set_input("a", &[1]);
//! sim.set_input("b", &[1]);
//! sim.step();
//! let result = sim.get_output("y");
//!
//! // Run with fault injection
//! let fault = FaultInjectionConfig::stuck_at_0(PrimitiveId(42), 0);
//! sim.inject_fault(fault);
//! sim.step();
//! let faulty_result = sim.get_output("y");
//! ```

use crate::gate_eval::{evaluate_primitive, evaluate_primitive_with_fault};
use crate::sir::{
    CombinationalBlock, EdgeType, FaultInjectionConfig, PrimitiveId, PrimitiveType, Sir,
    SirOperation, SirPortDirection, SirSignalId, SirSignalType,
};
use std::collections::HashMap;

/// Gate-level simulation state
#[derive(Debug, Clone)]
pub struct GateSimulationState {
    /// Current simulation cycle
    pub cycle: u64,
    /// Signal values (signal_id -> value as bool vector)
    pub signals: HashMap<u32, Vec<bool>>,
    /// Previous clock values for edge detection
    pub prev_clocks: HashMap<u32, bool>,
}

impl GateSimulationState {
    /// Create a new empty state
    pub fn new() -> Self {
        Self {
            cycle: 0,
            signals: HashMap::new(),
            prev_clocks: HashMap::new(),
        }
    }
}

impl Default for GateSimulationState {
    fn default() -> Self {
        Self::new()
    }
}

/// Result from a fault injection simulation
#[derive(Debug, Clone)]
pub struct FaultSimResult {
    /// The fault that was injected
    pub fault: FaultInjectionConfig,
    /// Whether the fault was detected by safety mechanisms
    pub detected: bool,
    /// Output signal differences (signal_name -> (normal, faulty))
    pub output_diffs: HashMap<String, (Vec<bool>, Vec<bool>)>,
    /// Cycle at which detection occurred (if detected)
    pub detection_cycle: Option<u64>,
    /// Detection mode of the signal that detected the fault (if detected)
    /// Used to calculate separate runtime_dc (Continuous) vs boot_dc (Boot/Periodic/OnDemand)
    pub detection_mode: Option<crate::sir::SirDetectionMode>,
}

/// Configuration for fault campaign
#[derive(Debug, Clone)]
pub struct FaultCampaignConfig {
    /// Number of cycles to simulate per fault
    pub cycles_per_fault: u64,
    /// Clock signal name
    pub clock_name: String,
    /// Fault types to test
    pub fault_types: Vec<crate::sir::FaultType>,
    /// Maximum faults to test (0 = all)
    pub max_faults: usize,
}

impl Default for FaultCampaignConfig {
    fn default() -> Self {
        Self {
            cycles_per_fault: 100,
            clock_name: "clk".to_string(),
            fault_types: vec![
                crate::sir::FaultType::StuckAt0,
                crate::sir::FaultType::StuckAt1,
            ],
            max_faults: 0,
        }
    }
}

/// Results from a fault campaign
#[derive(Debug, Clone)]
pub struct FaultCampaignResults {
    /// Total faults simulated
    pub total_faults: usize,
    /// Faults that were detected by safety mechanisms
    pub detected_faults: usize,
    /// Faults that caused output corruption (dangerous faults)
    pub corruption_faults: usize,
    /// Faults that caused no output change (safe faults - masked by logic)
    pub safe_faults: usize,
    /// Diagnostic coverage (detected / corruption) as percentage
    pub diagnostic_coverage: f64,
    /// Safe fault percentage (safe / total) as percentage
    pub safe_fault_percentage: f64,
    /// Individual fault results
    pub fault_results: Vec<FaultSimResult>,
}

/// Gate-level simulator with fault injection support
pub struct GateLevelSimulator {
    /// The SIR being simulated
    sir: Sir,
    /// Current simulation state
    state: GateSimulationState,
    /// Signal name to ID mapping
    signal_name_to_id: HashMap<String, SirSignalId>,
    /// Signal ID to name mapping
    signal_id_to_name: HashMap<u32, String>,
    /// Signal widths
    signal_widths: HashMap<u32, usize>,
    /// Input port IDs
    input_ports: Vec<SirSignalId>,
    /// Output port IDs
    output_ports: Vec<SirSignalId>,
    /// Clock signal IDs
    clock_signals: Vec<SirSignalId>,
    /// Active fault injection (if any)
    active_fault: Option<FaultInjectionConfig>,
    /// Detection signals (output signals that indicate fault detection)
    detection_signals: Vec<SirSignalId>,
    /// Total FIT of the design
    total_fit: f64,
}

impl GateLevelSimulator {
    /// Create a new gate-level simulator from SIR
    pub fn new(sir: &Sir) -> Self {
        let mut sim = Self {
            sir: sir.clone(),
            state: GateSimulationState::new(),
            signal_name_to_id: HashMap::new(),
            signal_id_to_name: HashMap::new(),
            signal_widths: HashMap::new(),
            input_ports: Vec::new(),
            output_ports: Vec::new(),
            clock_signals: Vec::new(),
            active_fault: None,
            detection_signals: Vec::new(),
            total_fit: 0.0,
        };
        sim.initialize();
        sim
    }

    /// Initialize the simulator from SIR
    fn initialize(&mut self) {
        let module = &self.sir.top_module;

        // Build signal mappings
        for signal in &module.signals {
            self.signal_name_to_id
                .insert(signal.name.clone(), signal.id);
            self.signal_id_to_name
                .insert(signal.id.0, signal.name.clone());
            self.signal_widths.insert(signal.id.0, signal.width);

            // Initialize signal values to 0
            self.state
                .signals
                .insert(signal.id.0, vec![false; signal.width]);

            // Categorize signals
            match &signal.signal_type {
                SirSignalType::Port { direction } => match direction {
                    SirPortDirection::Input => {
                        self.input_ports.push(signal.id);
                        // Check if it's a clock
                        if signal.name.contains("clk") || signal.name.contains("clock") {
                            self.clock_signals.push(signal.id);
                        }
                    }
                    SirPortDirection::Output => {
                        self.output_ports.push(signal.id);
                        // Check if it's a detection signal (explicit annotation preferred)
                        if signal.is_detection {
                            // Explicit #[detection_signal] annotation
                            self.detection_signals.push(signal.id);
                        } else if signal.name.contains("fault")
                            || signal.name.contains("error")
                            || signal.name.contains("detect")
                        {
                            // Fallback: name-based heuristic (deprecated, for backwards compatibility)
                            self.detection_signals.push(signal.id);
                        }
                    }
                    SirPortDirection::InOut => {
                        self.input_ports.push(signal.id);
                        self.output_ports.push(signal.id);
                    }
                },
                SirSignalType::Register { clock, .. } => {
                    // Track clock for edge detection
                    self.state.prev_clocks.insert(clock.0, false);
                }
                SirSignalType::Wire => {}
            }
        }

        // Calculate total FIT from primitives
        self.total_fit = self.calculate_total_fit();
    }

    /// Calculate total FIT from all primitives
    fn calculate_total_fit(&self) -> f64 {
        let mut fit = 0.0;
        for block in &self.sir.top_module.comb_blocks {
            for op in &block.operations {
                if let SirOperation::Primitive { ptype, .. } = op {
                    fit += ptype.base_fit();
                }
            }
        }
        for block in &self.sir.top_module.seq_blocks {
            for op in &block.operations {
                if let SirOperation::Primitive { ptype, .. } = op {
                    fit += ptype.base_fit();
                }
            }
        }
        fit
    }

    /// Get total FIT of the design
    pub fn total_fit(&self) -> f64 {
        self.total_fit
    }

    /// Set an input signal value
    pub fn set_input(&mut self, name: &str, value: &[bool]) {
        if let Some(id) = self.signal_name_to_id.get(name) {
            let width = self.signal_widths.get(&id.0).copied().unwrap_or(1);
            let mut padded = value.to_vec();
            padded.resize(width, false);
            self.state.signals.insert(id.0, padded);
        }
    }

    /// Set an input signal value from u64
    ///
    /// This handles both single signals (e.g., "a") and bit-indexed signals (e.g., "a[0]", "a[1]").
    /// For multi-bit ports that were decomposed to bit-level signals, this will set all
    /// bit signals from "name[0]" to "name[N-1]".
    pub fn set_input_u64(&mut self, name: &str, value: u64) {
        // First, try direct lookup (single-bit signal or non-decomposed)
        if let Some(id) = self.signal_name_to_id.get(name) {
            let width = self.signal_widths.get(&id.0).copied().unwrap_or(1);
            let bits: Vec<bool> = (0..width).map(|i| (value >> i) & 1 == 1).collect();
            self.state.signals.insert(id.0, bits);
            return;
        }

        // Otherwise, look for bit-indexed signals: name[0], name[1], ...
        // Count how many bit signals exist for this base name
        let mut bit_idx = 0;
        while let Some(id) = self
            .signal_name_to_id
            .get(&format!("{}[{}]", name, bit_idx))
        {
            let bit_value = (value >> bit_idx) & 1 == 1;
            self.state.signals.insert(id.0, vec![bit_value]);
            bit_idx += 1;
        }
    }

    /// Get an output signal value
    pub fn get_output(&self, name: &str) -> Option<Vec<bool>> {
        self.signal_name_to_id
            .get(name)
            .and_then(|id| self.state.signals.get(&id.0).cloned())
    }

    /// Get any signal value by name (for debugging)
    pub fn get_signal(&self, name: &str) -> Option<Vec<bool>> {
        self.signal_name_to_id
            .get(name)
            .and_then(|id| self.state.signals.get(&id.0).cloned())
    }

    /// Get all signal values (for debugging)
    pub fn dump_signals(&self) -> Vec<(String, Vec<bool>)> {
        let mut result: Vec<_> = self
            .signal_name_to_id
            .iter()
            .filter_map(|(name, id)| {
                self.state
                    .signals
                    .get(&id.0)
                    .map(|v| (name.clone(), v.clone()))
            })
            .collect();
        result.sort_by(|a, b| a.0.cmp(&b.0));
        result
    }

    /// Get an output signal value as u64
    ///
    /// This handles both single signals (e.g., "sum") and bit-indexed signals (e.g., "sum[0]", "sum[1]").
    /// For multi-bit outputs that were decomposed to bit-level signals, this will collect all
    /// bit signals from "name[0]" to "name[N-1]" and combine them into a u64.
    pub fn get_output_u64(&self, name: &str) -> Option<u64> {
        // First, try direct lookup
        if let Some(bits) = self.get_output(name) {
            return Some(
                bits.iter()
                    .enumerate()
                    .fold(0u64, |acc, (i, &b)| acc | ((b as u64) << i)),
            );
        }

        // Otherwise, collect bit-indexed signals: name[0], name[1], ...
        let mut result = 0u64;
        let mut bit_idx = 0;
        let mut found_any = false;

        while let Some(id) = self
            .signal_name_to_id
            .get(&format!("{}[{}]", name, bit_idx))
        {
            found_any = true;
            if let Some(bits) = self.state.signals.get(&id.0) {
                if bits.first().copied().unwrap_or(false) {
                    result |= 1u64 << bit_idx;
                }
            }
            bit_idx += 1;
        }

        if found_any {
            Some(result)
        } else {
            None
        }
    }

    /// Get the current cycle
    pub fn cycle(&self) -> u64 {
        self.state.cycle
    }

    /// Inject a fault (active until cleared)
    pub fn inject_fault(&mut self, fault: FaultInjectionConfig) {
        self.active_fault = Some(fault);
    }

    /// Clear any active fault
    pub fn clear_fault(&mut self) {
        self.active_fault = None;
    }

    /// Reset the simulator
    pub fn reset(&mut self) {
        self.state = GateSimulationState::new();
        self.active_fault = None;

        // Re-initialize signal values
        for signal in &self.sir.top_module.signals {
            self.state
                .signals
                .insert(signal.id.0, vec![false; signal.width]);
        }
    }

    /// Step simulation by one cycle
    pub fn step(&mut self) -> GateSimulationState {
        // Phase 1: Detect clock edges
        let clock_edges = self.detect_clock_edges();

        // Phase 2: Evaluate combinational logic
        self.evaluate_combinational();

        // Phase 3: Update sequential logic on clock edges
        for (clock_id, edge) in &clock_edges {
            self.update_sequential(*clock_id, edge.clone());
        }

        // Phase 4: Re-evaluate combinational with new register values
        self.evaluate_combinational();

        // Update cycle counter
        self.state.cycle += 1;

        // Update previous clock values
        for clock_id in &self.clock_signals {
            if let Some(value) = self.state.signals.get(&clock_id.0) {
                let clk_value = value.first().copied().unwrap_or(false);
                self.state.prev_clocks.insert(clock_id.0, clk_value);
            }
        }

        self.state.clone()
    }

    /// Detect clock edges
    fn detect_clock_edges(&self) -> Vec<(SirSignalId, EdgeType)> {
        let mut edges = Vec::new();

        for clock_id in &self.clock_signals {
            let prev = self
                .state
                .prev_clocks
                .get(&clock_id.0)
                .copied()
                .unwrap_or(false);
            let curr = self
                .state
                .signals
                .get(&clock_id.0)
                .and_then(|v| v.first().copied())
                .unwrap_or(false);

            if !prev && curr {
                edges.push((*clock_id, EdgeType::Rising));
            } else if prev && !curr {
                edges.push((*clock_id, EdgeType::Falling));
            }
        }

        edges
    }

    /// Evaluate all combinational logic
    fn evaluate_combinational(&mut self) {
        for block in &self.sir.top_module.comb_blocks.clone() {
            self.evaluate_comb_block(block);
        }
    }

    /// Evaluate a single combinational block
    fn evaluate_comb_block(&mut self, block: &CombinationalBlock) {
        // If block has structural info with eval_order, use it
        if let Some(info) = &block.structural_info {
            if let Some(order) = &info.eval_order {
                for &idx in order {
                    if idx < block.operations.len() {
                        self.evaluate_operation(&block.operations[idx]);
                    }
                }
                return;
            }
        }

        // Otherwise evaluate in order
        for op in &block.operations {
            self.evaluate_operation(op);
        }
    }

    /// Evaluate a single operation
    fn evaluate_operation(&mut self, op: &SirOperation) {
        match op {
            SirOperation::Primitive {
                id,
                ptype,
                inputs,
                outputs,
                path,
            } => {
                // Gather input values
                let input_values: Vec<bool> = inputs
                    .iter()
                    .filter_map(|sig_id| {
                        self.state
                            .signals
                            .get(&sig_id.0)
                            .and_then(|v| v.first().copied())
                    })
                    .collect();

                // Evaluate primitive with potential fault injection
                let output_values = if let Some(fault) = &self.active_fault {
                    if fault.target_primitive == *id {
                        evaluate_primitive_with_fault(
                            ptype,
                            &input_values,
                            Some(fault),
                            self.state.cycle,
                        )
                    } else {
                        evaluate_primitive(ptype, &input_values)
                    }
                } else {
                    evaluate_primitive(ptype, &input_values)
                };

                // Store output values
                for (i, out_id) in outputs.iter().enumerate() {
                    if let Some(&value) = output_values.get(i) {
                        let width = self.signal_widths.get(&out_id.0).copied().unwrap_or(1);
                        let mut bits = vec![false; width];
                        bits[0] = value;
                        self.state.signals.insert(out_id.0, bits);
                    }
                }
            }
            SirOperation::Assign { target, source } => {
                // For behavioral operations (shouldn't happen in structural mode)
                // but handle for completeness
                let value = self.evaluate_expression(source);
                self.state.signals.insert(target.0, value);
            }
            _ => {
                // Other behavioral operations - skip in structural mode
            }
        }
    }

    /// Evaluate a behavioral expression (fallback)
    fn evaluate_expression(&self, expr: &crate::sir::SirExpression) -> Vec<bool> {
        use crate::sir::SirExpression;
        match expr {
            SirExpression::Signal(id) => self.state.signals.get(&id.0).cloned().unwrap_or_default(),
            SirExpression::Constant(bv) => bv.iter().by_vals().collect(),
            _ => vec![false], // Simplified - full expression eval not needed for structural
        }
    }

    /// Update sequential logic on clock edge
    fn update_sequential(&mut self, clock_id: SirSignalId, edge: EdgeType) {
        for block in &self.sir.top_module.seq_blocks.clone() {
            // Check if this block uses this clock with matching edge
            if block.clock == clock_id {
                let matches_edge = match (&block.clock_edge, &edge) {
                    (EdgeType::Rising, EdgeType::Rising) => true,
                    (EdgeType::Falling, EdgeType::Falling) => true,
                    (EdgeType::Both, _) => true,
                    _ => false,
                };

                if matches_edge {
                    // Evaluate sequential operations (DFFs)
                    for op in &block.operations {
                        self.evaluate_operation(op);
                    }
                }
            }
        }
    }

    /// Run simulation for N cycles
    pub fn run(&mut self, cycles: u64) -> Vec<GateSimulationState> {
        let mut states = Vec::with_capacity(cycles as usize);
        for _ in 0..cycles {
            states.push(self.step());
        }
        states
    }

    /// Run simulation with a clock pattern (toggles clock each step)
    pub fn run_clocked(&mut self, cycles: u64, clock_name: &str) -> Vec<GateSimulationState> {
        let mut states = Vec::with_capacity((cycles * 2) as usize);
        for _ in 0..cycles {
            // Low phase
            self.set_input(clock_name, &[false]);
            states.push(self.step());
            // High phase
            self.set_input(clock_name, &[true]);
            states.push(self.step());
        }
        states
    }

    /// Check if any detection signal is asserted
    pub fn is_fault_detected(&self) -> bool {
        for det_id in &self.detection_signals {
            if let Some(value) = self.state.signals.get(&det_id.0) {
                if value.first().copied().unwrap_or(false) {
                    return true;
                }
            }
        }
        false
    }

    /// Run a single fault injection simulation
    pub fn run_fault_sim(
        &mut self,
        fault: FaultInjectionConfig,
        cycles: u64,
        clock_name: &str,
    ) -> FaultSimResult {
        // First run normal simulation to get golden outputs
        self.reset();
        let _normal_states = self.run_clocked(cycles, clock_name);
        let normal_outputs: HashMap<String, Vec<bool>> = self
            .output_ports
            .iter()
            .filter_map(|id| {
                let name = self.signal_id_to_name.get(&id.0)?;
                let value = self.state.signals.get(&id.0)?;
                Some((name.clone(), value.clone()))
            })
            .collect();

        // Now run with fault injection
        self.reset();
        self.inject_fault(fault.clone());
        let mut detection_cycle = None;

        for cycle in 0..(cycles * 2) {
            // Toggle clock
            let clk_val = cycle % 2 == 1;
            self.set_input(clock_name, &[clk_val]);
            self.step();

            // Check for detection
            if detection_cycle.is_none() && self.is_fault_detected() {
                detection_cycle = Some(self.state.cycle);
            }
        }

        // Compare outputs
        let mut output_diffs = HashMap::new();
        let mut any_diff = false;

        for id in &self.output_ports {
            if let Some(name) = self.signal_id_to_name.get(&id.0) {
                if let Some(faulty) = self.state.signals.get(&id.0) {
                    if let Some(normal) = normal_outputs.get(name) {
                        if normal != faulty {
                            output_diffs.insert(name.clone(), (normal.clone(), faulty.clone()));
                            any_diff = true;
                        }
                    }
                }
            }
        }

        self.clear_fault();

        FaultSimResult {
            fault,
            detected: detection_cycle.is_some(),
            output_diffs,
            detection_cycle,
            detection_mode: None, // Gate-level simulator doesn't track detection mode
        }
    }

    /// Run a fault campaign over all primitives
    pub fn run_fault_campaign(
        &mut self,
        cycles_per_fault: u64,
        clock_name: &str,
    ) -> FaultCampaignResults {
        let mut results = Vec::new();
        let mut total_faults = 0;
        let mut detected_faults = 0;
        let mut corruption_faults = 0;

        // Collect all primitive IDs
        let mut primitives: Vec<(PrimitiveId, PrimitiveType)> = Vec::new();
        for block in &self.sir.top_module.comb_blocks {
            for op in &block.operations {
                if let SirOperation::Primitive { id, ptype, .. } = op {
                    primitives.push((*id, ptype.clone()));
                }
            }
        }
        for block in &self.sir.top_module.seq_blocks {
            for op in &block.operations {
                if let SirOperation::Primitive { id, ptype, .. } = op {
                    primitives.push((*id, ptype.clone()));
                }
            }
        }

        // Test SA0 and SA1 for each primitive
        for (prim_id, _ptype) in &primitives {
            // Stuck-at-0
            let fault_sa0 = FaultInjectionConfig::stuck_at_0(*prim_id, 0);
            let result = self.run_fault_sim(fault_sa0, cycles_per_fault, clock_name);
            total_faults += 1;
            if result.detected {
                detected_faults += 1;
            }
            if !result.output_diffs.is_empty() {
                corruption_faults += 1;
            }
            results.push(result);

            // Stuck-at-1
            let fault_sa1 = FaultInjectionConfig::stuck_at_1(*prim_id, 0);
            let result = self.run_fault_sim(fault_sa1, cycles_per_fault, clock_name);
            total_faults += 1;
            if result.detected {
                detected_faults += 1;
            }
            if !result.output_diffs.is_empty() {
                corruption_faults += 1;
            }
            results.push(result);
        }

        // Safe faults = faults that caused no output change (masked by logic)
        let safe_faults = total_faults - corruption_faults;

        let dc = if corruption_faults > 0 {
            (detected_faults as f64) / (corruption_faults as f64) * 100.0
        } else {
            100.0 // No corruption = no dangerous faults
        };

        let safe_pct = if total_faults > 0 {
            (safe_faults as f64) / (total_faults as f64) * 100.0
        } else {
            0.0
        };

        FaultCampaignResults {
            total_faults,
            detected_faults,
            corruption_faults,
            safe_faults,
            diagnostic_coverage: dc,
            safe_fault_percentage: safe_pct,
            fault_results: results,
        }
    }

    /// Run a fault campaign with configuration
    pub fn run_fault_campaign_with_config(
        &mut self,
        config: &FaultCampaignConfig,
    ) -> FaultCampaignResults {
        use crate::sir::FaultType;

        let mut results = Vec::new();
        let mut total_faults = 0;
        let mut detected_faults = 0;
        let mut corruption_faults = 0;

        // Collect all primitive IDs
        let mut primitives: Vec<(PrimitiveId, PrimitiveType)> = Vec::new();
        for block in &self.sir.top_module.comb_blocks {
            for op in &block.operations {
                if let SirOperation::Primitive { id, ptype, .. } = op {
                    primitives.push((*id, ptype.clone()));
                }
            }
        }
        for block in &self.sir.top_module.seq_blocks {
            for op in &block.operations {
                if let SirOperation::Primitive { id, ptype, .. } = op {
                    primitives.push((*id, ptype.clone()));
                }
            }
        }

        // Test each configured fault type for each primitive
        for (prim_id, _ptype) in &primitives {
            for fault_type in &config.fault_types {
                if config.max_faults > 0 && total_faults >= config.max_faults {
                    break;
                }

                let fault = match fault_type {
                    FaultType::StuckAt0 => FaultInjectionConfig::stuck_at_0(*prim_id, 0),
                    FaultType::StuckAt1 => FaultInjectionConfig::stuck_at_1(*prim_id, 0),
                    FaultType::BitFlip => FaultInjectionConfig::bit_flip(*prim_id, 0, Some(1)),
                    FaultType::Transient => FaultInjectionConfig::transient(*prim_id, 0),
                    _ => continue,
                };

                let result = self.run_fault_sim(fault, config.cycles_per_fault, &config.clock_name);
                total_faults += 1;
                let caused_corruption = !result.output_diffs.is_empty();
                if caused_corruption {
                    corruption_faults += 1;
                    // DC counts detected faults among corruption faults only
                    if result.detected {
                        detected_faults += 1;
                    }
                }
                results.push(result);
            }
            if config.max_faults > 0 && total_faults >= config.max_faults {
                break;
            }
        }

        // Safe faults = faults that caused no output change (masked by logic)
        let safe_faults = total_faults - corruption_faults;

        let dc = if corruption_faults > 0 {
            (detected_faults as f64) / (corruption_faults as f64) * 100.0
        } else {
            100.0
        };

        let safe_pct = if total_faults > 0 {
            (safe_faults as f64) / (total_faults as f64) * 100.0
        } else {
            0.0
        };

        FaultCampaignResults {
            total_faults,
            detected_faults,
            corruption_faults,
            safe_faults,
            diagnostic_coverage: dc,
            safe_fault_percentage: safe_pct,
            fault_results: results,
        }
    }

    /// Get primitive count
    pub fn primitive_count(&self) -> usize {
        let mut count = 0;
        for block in &self.sir.top_module.comb_blocks {
            for op in &block.operations {
                if matches!(op, SirOperation::Primitive { .. }) {
                    count += 1;
                }
            }
        }
        for block in &self.sir.top_module.seq_blocks {
            for op in &block.operations {
                if matches!(op, SirOperation::Primitive { .. }) {
                    count += 1;
                }
            }
        }
        count
    }

    /// Get primitive breakdown by type
    pub fn primitive_breakdown(&self) -> HashMap<String, usize> {
        let mut breakdown = HashMap::new();
        for block in &self.sir.top_module.comb_blocks {
            for op in &block.operations {
                if let SirOperation::Primitive { ptype, .. } = op {
                    *breakdown.entry(ptype.short_name().to_string()).or_insert(0) += 1;
                }
            }
        }
        for block in &self.sir.top_module.seq_blocks {
            for op in &block.operations {
                if let SirOperation::Primitive { ptype, .. } = op {
                    *breakdown.entry(ptype.short_name().to_string()).or_insert(0) += 1;
                }
            }
        }
        breakdown
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lir_to_sir::convert_lir_to_sir;
    use skalp_lir::lir::{Lir, LirNet, NetId, Primitive};

    fn make_and_gate_lir() -> Lir {
        let mut lir = Lir::new("and_gate".to_string());

        // Create nets: a, b, y
        let net_a = lir.add_net(LirNet {
            id: NetId(0),
            name: "a".to_string(),
            driver: None,
            loads: vec![],
            is_primary_input: true,
            is_primary_output: false,
            is_state_output: false,
            is_detection: false,
            detection_config: None,
            width: 1,
        });

        let net_b = lir.add_net(LirNet {
            id: NetId(1),
            name: "b".to_string(),
            driver: None,
            loads: vec![],
            is_primary_input: true,
            is_primary_output: false,
            is_state_output: false,
            is_detection: false,
            detection_config: None,
            width: 1,
        });

        let net_y = lir.add_net(LirNet {
            id: NetId(2),
            name: "y".to_string(),
            driver: None,
            loads: vec![],
            is_primary_input: false,
            is_primary_output: true,
            is_state_output: false,
            is_detection: false,
            detection_config: None,
            width: 1,
        });

        // Create AND gate
        lir.add_primitive(Primitive {
            id: PrimitiveId(0),
            ptype: PrimitiveType::And { inputs: 2 },
            path: "and_0".to_string(),
            inputs: vec![net_a, net_b],
            outputs: vec![net_y],
            clock: None,
            reset: None,
            enable: None,
            bit_index: None,
            safety_info: None,
            power_domain: None,
        });

        lir
    }

    #[test]
    fn test_gate_simulator_creation() {
        let lir = make_and_gate_lir();
        let sir_result = convert_lir_to_sir(&lir);
        let sim = GateLevelSimulator::new(&sir_result.sir);

        assert!(sim.primitive_count() >= 1);
        assert!(sim.total_fit() > 0.0);
    }

    #[test]
    fn test_and_gate_simulation() {
        let lir = make_and_gate_lir();
        let sir_result = convert_lir_to_sir(&lir);
        let mut sim = GateLevelSimulator::new(&sir_result.sir);

        // Test AND(1,1) = 1
        sim.set_input("a", &[true]);
        sim.set_input("b", &[true]);
        sim.step();
        assert_eq!(sim.get_output("y"), Some(vec![true]));

        // Test AND(1,0) = 0
        sim.set_input("a", &[true]);
        sim.set_input("b", &[false]);
        sim.step();
        assert_eq!(sim.get_output("y"), Some(vec![false]));

        // Test AND(0,1) = 0
        sim.set_input("a", &[false]);
        sim.set_input("b", &[true]);
        sim.step();
        assert_eq!(sim.get_output("y"), Some(vec![false]));

        // Test AND(0,0) = 0
        sim.set_input("a", &[false]);
        sim.set_input("b", &[false]);
        sim.step();
        assert_eq!(sim.get_output("y"), Some(vec![false]));
    }

    #[test]
    fn test_fault_injection() {
        let lir = make_and_gate_lir();
        let sir_result = convert_lir_to_sir(&lir);
        let mut sim = GateLevelSimulator::new(&sir_result.sir);

        // Normal: AND(1,1) = 1
        sim.set_input("a", &[true]);
        sim.set_input("b", &[true]);
        sim.step();
        assert_eq!(sim.get_output("y"), Some(vec![true]));

        // With SA0 fault: AND(1,1) = 0
        sim.inject_fault(FaultInjectionConfig::stuck_at_0(PrimitiveId(0), 0));
        sim.step();
        assert_eq!(sim.get_output("y"), Some(vec![false]));

        // Clear fault
        sim.clear_fault();
        sim.step();
        assert_eq!(sim.get_output("y"), Some(vec![true]));
    }

    #[test]
    fn test_primitive_breakdown() {
        let lir = make_and_gate_lir();
        let sir_result = convert_lir_to_sir(&lir);
        let sim = GateLevelSimulator::new(&sir_result.sir);

        let breakdown = sim.primitive_breakdown();
        assert!(breakdown.contains_key("AND") || breakdown.contains_key("And { inputs: 2 }"));
    }
}
