//! Unified Simulation Runtime
//!
//! This module provides a unified interface for both behavioral and gate-level simulation,
//! allowing the same testbench to run on either mode with a simple flag.
//!
//! # Architecture
//!
//! ```text
//!                     ┌─────────────────────┐
//!                     │   Same .sk source   │
//!                     └──────────┬──────────┘
//!                                │
//!                                ▼
//!                     ┌─────────────────────┐
//!                     │  UnifiedSimulator   │
//!                     │  (this module)      │
//!                     └──────────┬──────────┘
//!                                │
//!          ┌─────────────────────┴─────────────────────┐
//!          │                                           │
//!          ▼                                           ▼
//!   ┌─────────────────┐                     ┌─────────────────┐
//!   │   Behavioral    │                     │   Gate-Level    │
//!   │   (MIR → SIR)   │                     │ (MIR → LIR →    │
//!   │                 │                     │      SIR)       │
//!   └─────────────────┘                     └─────────────────┘
//! ```
//!
//! # Example
//!
//! ```ignore
//! // Create unified simulator
//! let mut sim = UnifiedSimulator::from_source(source, SimLevel::Behavioral)?;
//! // Or for gate-level:
//! let mut sim = UnifiedSimulator::from_source(source, SimLevel::GateLevel)?;
//!
//! // Same testbench API for both!
//! sim.set_input("clk", 1)?;
//! sim.step()?;
//! let result = sim.get_output("result")?;
//! ```

use crate::gate_runtime::{GateLevelRuntime, SimulationMode};
use crate::simulator::{SimulationError, SimulationResult, SimulationState};
use std::collections::HashMap;

/// Simulation abstraction level
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum SimLevel {
    /// Behavioral simulation (MIR → behavioral SIR)
    /// Faster, but no gate-level detail
    #[default]
    Behavioral,
    /// Gate-level simulation (MIR → LIR → structural SIR)
    /// Slower, but enables fault injection and gate-level analysis
    GateLevel,
}

/// Hardware acceleration mode
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum HwAccel {
    /// CPU-only simulation
    Cpu,
    /// GPU-accelerated simulation (Metal on macOS)
    Gpu,
    /// Auto-select (prefer GPU if available)
    #[default]
    Auto,
}

/// Configuration for unified simulation
#[derive(Debug, Clone)]
pub struct UnifiedSimConfig {
    /// Simulation abstraction level
    pub level: SimLevel,
    /// Hardware acceleration mode
    pub hw_accel: HwAccel,
    /// Maximum cycles before timeout
    pub max_cycles: u64,
    /// Whether to capture waveforms
    pub capture_waveforms: bool,
}

impl Default for UnifiedSimConfig {
    fn default() -> Self {
        Self {
            level: SimLevel::Behavioral,
            hw_accel: HwAccel::Auto,
            max_cycles: 1_000_000,
            capture_waveforms: false,
        }
    }
}

impl UnifiedSimConfig {
    /// Create config for behavioral simulation
    pub fn behavioral() -> Self {
        Self {
            level: SimLevel::Behavioral,
            ..Default::default()
        }
    }

    /// Create config for gate-level simulation
    pub fn gate_level() -> Self {
        Self {
            level: SimLevel::GateLevel,
            ..Default::default()
        }
    }

    /// Set hardware acceleration mode
    pub fn with_hw_accel(mut self, accel: HwAccel) -> Self {
        self.hw_accel = accel;
        self
    }

    /// Enable waveform capture
    pub fn with_waveforms(mut self) -> Self {
        self.capture_waveforms = true;
        self
    }
}

/// Backend selector for unified runtime
enum Backend {
    /// Behavioral simulation via CpuRuntime
    Behavioral(BehavioralBackend),
    /// Gate-level simulation via GateLevelRuntime
    GateLevel(GateLevelRuntime),
}

/// Behavioral backend wrapping the async CpuRuntime in a sync interface
struct BehavioralBackend {
    /// The SIR module (behavioral)
    module: skalp_sir::SirModule,
    /// Input values
    inputs: HashMap<String, Vec<u8>>,
    /// Output values
    outputs: HashMap<String, Vec<u8>>,
    /// State values
    state: HashMap<String, Vec<u8>>,
    /// Signal values
    signals: HashMap<String, Vec<u8>>,
    /// Signal widths
    signal_widths: HashMap<String, usize>,
    /// Previous clock values
    prev_clocks: HashMap<String, u8>,
    /// Current cycle
    current_cycle: u64,
}

impl BehavioralBackend {
    fn new(module: skalp_sir::SirModule) -> Self {
        let mut backend = Self {
            module,
            inputs: HashMap::new(),
            outputs: HashMap::new(),
            state: HashMap::new(),
            signals: HashMap::new(),
            signal_widths: HashMap::new(),
            prev_clocks: HashMap::new(),
            current_cycle: 0,
        };
        backend.initialize_signals();
        backend
    }

    fn initialize_signals(&mut self) {
        // Initialize inputs
        for input in &self.module.inputs {
            let byte_size = input.width.div_ceil(8);
            self.inputs.insert(input.name.clone(), vec![0u8; byte_size]);
            self.signal_widths.insert(input.name.clone(), input.width);
        }

        // Initialize outputs
        for output in &self.module.outputs {
            let byte_size = output.width.div_ceil(8);
            self.outputs.insert(output.name.clone(), vec![0u8; byte_size]);
            self.signal_widths.insert(output.name.clone(), output.width);
        }

        // Initialize signals
        for signal in &self.module.signals {
            let byte_size = signal.width.div_ceil(8);
            self.signals.insert(signal.name.clone(), vec![0u8; byte_size]);
            self.signal_widths.insert(signal.name.clone(), signal.width);
        }

        // Initialize state elements
        for (name, element) in &self.module.state_elements {
            let byte_size = element.width.div_ceil(8);
            let initial = if let Some(reset_val) = element.reset_value {
                let mut bytes = vec![0u8; byte_size];
                for (i, byte) in bytes.iter_mut().enumerate() {
                    *byte = ((reset_val >> (i * 8)) & 0xFF) as u8;
                }
                bytes
            } else {
                vec![0u8; byte_size]
            };
            self.state.insert(name.clone(), initial);
            self.signal_widths.insert(name.clone(), element.width);
        }
    }

    fn set_input(&mut self, name: &str, value: u64) -> SimulationResult<()> {
        if let Some(width) = self.signal_widths.get(name) {
            let byte_size = width.div_ceil(8);
            let mut bytes = vec![0u8; byte_size];
            for (i, byte) in bytes.iter_mut().enumerate() {
                *byte = ((value >> (i * 8)) & 0xFF) as u8;
            }
            self.inputs.insert(name.to_string(), bytes);
            Ok(())
        } else {
            Err(SimulationError::InvalidInput(format!(
                "Input '{}' not found",
                name
            )))
        }
    }

    fn get_output(&self, name: &str) -> SimulationResult<u64> {
        self.outputs
            .get(name)
            .map(|bytes| {
                let mut val = 0u64;
                for (i, &byte) in bytes.iter().enumerate().take(8) {
                    val |= (byte as u64) << (i * 8);
                }
                val
            })
            .ok_or_else(|| {
                SimulationError::InvalidInput(format!("Output '{}' not found", name))
            })
    }

    fn step(&mut self) -> SimulationResult<SimulationState> {
        // Copy state to signals
        for (name, value) in &self.state {
            self.signals.insert(name.clone(), value.clone());
        }

        // Evaluate combinational logic
        self.evaluate_combinational()?;

        // Evaluate sequential logic
        self.evaluate_sequential()?;

        // Re-evaluate combinational with new state
        for (name, value) in &self.state {
            self.signals.insert(name.clone(), value.clone());
        }
        self.evaluate_combinational()?;

        self.current_cycle += 1;

        Ok(SimulationState {
            cycle: self.current_cycle,
            signals: self.outputs.clone(),
            registers: self.state.clone(),
        })
    }

    fn evaluate_combinational(&mut self) -> SimulationResult<()> {
        use skalp_sir::SirNodeKind;

        // Copy inputs to signals for evaluation
        for (name, value) in &self.inputs {
            self.signals.insert(name.clone(), value.clone());
        }

        // Build node map
        let node_map: HashMap<usize, &skalp_sir::SirNode> = self
            .module
            .combinational_nodes
            .iter()
            .map(|n| (n.id, n))
            .collect();

        // Evaluate in topological order
        for node_id in &self.module.sorted_combinational_node_ids {
            if let Some(node) = node_map.get(node_id) {
                // Get input values
                let input_values: Vec<Vec<u8>> = node
                    .inputs
                    .iter()
                    .map(|input| {
                        self.signals
                            .get(&input.signal_id)
                            .or_else(|| self.inputs.get(&input.signal_id))
                            .or_else(|| self.state.get(&input.signal_id))
                            .cloned()
                            .unwrap_or_else(|| vec![0u8])
                    })
                    .collect();

                // Evaluate node
                let output_value = match &node.kind {
                    SirNodeKind::Constant { value, width } => {
                        let byte_size = width.div_ceil(8);
                        let mut bytes = vec![0u8; byte_size];
                        for (i, byte) in bytes.iter_mut().enumerate() {
                            *byte = ((value >> (i * 8)) & 0xFF) as u8;
                        }
                        bytes
                    }
                    SirNodeKind::SignalRef { signal } => {
                        self.signals
                            .get(signal)
                            .or_else(|| self.inputs.get(signal))
                            .or_else(|| self.state.get(signal))
                            .cloned()
                            .unwrap_or_else(|| vec![0u8])
                    }
                    SirNodeKind::BinaryOp(op) => {
                        if input_values.len() >= 2 {
                            self.eval_binary_op(op, &input_values[0], &input_values[1])
                        } else {
                            vec![0u8]
                        }
                    }
                    SirNodeKind::UnaryOp(op) => {
                        if !input_values.is_empty() {
                            self.eval_unary_op(op, &input_values[0])
                        } else {
                            vec![0u8]
                        }
                    }
                    SirNodeKind::Mux => {
                        if input_values.len() >= 3 {
                            let cond = input_values[0].first().copied().unwrap_or(0) != 0;
                            if cond {
                                input_values[1].clone()
                            } else {
                                input_values[2].clone()
                            }
                        } else {
                            vec![0u8]
                        }
                    }
                    _ => vec![0u8],
                };

                // Store outputs
                for output in &node.outputs {
                    self.signals.insert(output.signal_id.clone(), output_value.clone());
                }
            }
        }

        // Update outputs from signals
        for output in &self.module.outputs {
            if let Some(value) = self.signals.get(&output.name) {
                self.outputs.insert(output.name.clone(), value.clone());
            }
        }

        Ok(())
    }

    fn evaluate_sequential(&mut self) -> SimulationResult<()> {
        use skalp_sir::ClockEdge;

        let mut next_state = HashMap::new();

        for node in &self.module.sequential_nodes {
            if let skalp_sir::SirNodeKind::FlipFlop { clock_edge } = &node.kind {
                if node.inputs.len() < 2 {
                    continue;
                }

                let clock_signal = &node.inputs[0].signal_id;
                let d_input = &node.inputs[1].signal_id;

                // Get current clock value
                let current_clock = self
                    .signals
                    .get(clock_signal)
                    .or_else(|| self.inputs.get(clock_signal))
                    .and_then(|v| v.first())
                    .copied()
                    .unwrap_or(0);

                let prev_clock = self.prev_clocks.get(clock_signal).copied().unwrap_or(0);

                // Check for edge
                let edge_detected = match clock_edge {
                    ClockEdge::Rising => prev_clock == 0 && current_clock != 0,
                    ClockEdge::Falling => prev_clock != 0 && current_clock == 0,
                    ClockEdge::Both => {
                        (prev_clock == 0 && current_clock != 0)
                            || (prev_clock != 0 && current_clock == 0)
                    }
                };

                self.prev_clocks.insert(clock_signal.clone(), current_clock);

                if edge_detected {
                    let d_value = self
                        .signals
                        .get(d_input)
                        .or_else(|| self.inputs.get(d_input))
                        .or_else(|| self.state.get(d_input))
                        .cloned()
                        .unwrap_or_else(|| vec![0u8]);

                    if let Some(output) = node.outputs.first() {
                        next_state.insert(output.signal_id.clone(), d_value);
                    }
                }
            }
        }

        // Update state
        for (name, value) in next_state {
            self.state.insert(name, value);
        }

        Ok(())
    }

    fn eval_binary_op(
        &self,
        op: &skalp_sir::BinaryOperation,
        left: &[u8],
        right: &[u8],
    ) -> Vec<u8> {
        use skalp_sir::BinaryOperation;

        let left_val = bytes_to_u64(left);
        let right_val = bytes_to_u64(right);

        let result = match op {
            BinaryOperation::Add => left_val.wrapping_add(right_val),
            BinaryOperation::Sub => left_val.wrapping_sub(right_val),
            BinaryOperation::Mul => left_val.wrapping_mul(right_val),
            BinaryOperation::Div => {
                if right_val == 0 { 0 } else { left_val / right_val }
            }
            BinaryOperation::Mod => {
                if right_val == 0 { 0 } else { left_val % right_val }
            }
            BinaryOperation::And => left_val & right_val,
            BinaryOperation::Or => left_val | right_val,
            BinaryOperation::Xor => left_val ^ right_val,
            BinaryOperation::Eq => if left_val == right_val { 1 } else { 0 },
            BinaryOperation::Neq => if left_val != right_val { 1 } else { 0 },
            BinaryOperation::Lt => if left_val < right_val { 1 } else { 0 },
            BinaryOperation::Lte => if left_val <= right_val { 1 } else { 0 },
            BinaryOperation::Gt => if left_val > right_val { 1 } else { 0 },
            BinaryOperation::Gte => if left_val >= right_val { 1 } else { 0 },
            BinaryOperation::Shl => left_val << (right_val & 0x3F),
            BinaryOperation::Shr => left_val >> (right_val & 0x3F),
            _ => 0, // FP ops not implemented in simplified version
        };

        let size = left.len().max(right.len());
        u64_to_bytes(result, size)
    }

    fn eval_unary_op(&self, op: &skalp_sir::UnaryOperation, operand: &[u8]) -> Vec<u8> {
        use skalp_sir::UnaryOperation;

        let val = bytes_to_u64(operand);

        let result = match op {
            UnaryOperation::Not => !val,
            UnaryOperation::Neg => val.wrapping_neg(),
            UnaryOperation::RedAnd => {
                let mask = (1u64 << (operand.len() * 8)) - 1;
                if (val & mask) == mask { 1 } else { 0 }
            }
            UnaryOperation::RedOr => if val != 0 { 1 } else { 0 },
            UnaryOperation::RedXor => {
                let mut result = 0;
                let mut v = val;
                while v != 0 {
                    result ^= v & 1;
                    v >>= 1;
                }
                result
            }
            _ => 0, // FP ops not implemented
        };

        u64_to_bytes(result, operand.len())
    }

    fn reset(&mut self) {
        self.current_cycle = 0;
        self.prev_clocks.clear();

        // Reset state elements
        for (name, element) in &self.module.state_elements {
            let byte_size = element.width.div_ceil(8);
            let reset_value = if let Some(val) = element.reset_value {
                let mut bytes = vec![0u8; byte_size];
                for (i, byte) in bytes.iter_mut().enumerate() {
                    *byte = ((val >> (i * 8)) & 0xFF) as u8;
                }
                bytes
            } else {
                vec![0u8; byte_size]
            };
            self.state.insert(name.clone(), reset_value);
        }

        // Clear signals
        for value in self.signals.values_mut() {
            value.fill(0);
        }
    }
}

/// Unified simulator providing the same API for both behavioral and gate-level simulation
pub struct UnifiedSimulator {
    backend: Backend,
    config: UnifiedSimConfig,
    current_cycle: u64,
    waveform_history: Vec<SimulationState>,
}

impl UnifiedSimulator {
    /// Create a unified simulator from a behavioral SIR module
    pub fn from_behavioral_sir(
        module: skalp_sir::SirModule,
        config: UnifiedSimConfig,
    ) -> SimulationResult<Self> {
        let backend = Backend::Behavioral(BehavioralBackend::new(module));

        Ok(Self {
            backend,
            config,
            current_cycle: 0,
            waveform_history: Vec::new(),
        })
    }

    /// Create a unified simulator from a gate-level netlist (LIR)
    pub fn from_lir(
        lir: &skalp_lir::lir::Lir,
        config: UnifiedSimConfig,
    ) -> SimulationResult<Self> {
        let mode = match config.hw_accel {
            HwAccel::Cpu => SimulationMode::Cpu,
            HwAccel::Gpu => SimulationMode::Gpu,
            HwAccel::Auto => SimulationMode::Auto,
        };

        let gate_runtime = GateLevelRuntime::from_lir_with_mode(lir, mode)?;

        Ok(Self {
            backend: Backend::GateLevel(gate_runtime),
            config,
            current_cycle: 0,
            waveform_history: Vec::new(),
        })
    }

    /// Get the simulation level
    pub fn level(&self) -> SimLevel {
        match &self.backend {
            Backend::Behavioral(_) => SimLevel::Behavioral,
            Backend::GateLevel(_) => SimLevel::GateLevel,
        }
    }

    /// Check if GPU is being used
    pub fn is_using_gpu(&self) -> bool {
        match &self.backend {
            Backend::Behavioral(_) => false,
            Backend::GateLevel(rt) => rt.is_using_gpu(),
        }
    }

    /// Get device info string
    pub fn device_info(&self) -> String {
        match &self.backend {
            Backend::Behavioral(_) => "Behavioral CPU".to_string(),
            Backend::GateLevel(rt) => rt.device_info(),
        }
    }

    /// Set an input signal value as u64
    pub fn set_input(&mut self, name: &str, value: u64) -> SimulationResult<()> {
        match &mut self.backend {
            Backend::Behavioral(be) => be.set_input(name, value),
            Backend::GateLevel(rt) => rt.set_input_u64(name, value),
        }
    }

    /// Get an output signal value as u64
    pub fn get_output(&self, name: &str) -> SimulationResult<u64> {
        match &self.backend {
            Backend::Behavioral(be) => be.get_output(name),
            Backend::GateLevel(rt) => rt.get_output_u64(name),
        }
    }

    /// Step simulation by one cycle
    pub fn step(&mut self) -> SimulationResult<SimulationState> {
        let state = match &mut self.backend {
            Backend::Behavioral(be) => be.step()?,
            Backend::GateLevel(rt) => rt.step()?,
        };

        self.current_cycle += 1;

        if self.config.capture_waveforms {
            self.waveform_history.push(state.clone());
        }

        Ok(state)
    }

    /// Run simulation for multiple cycles
    pub fn run(&mut self, cycles: u64) -> SimulationResult<Vec<SimulationState>> {
        let mut states = Vec::with_capacity(cycles as usize);
        for _ in 0..cycles {
            states.push(self.step()?);
        }
        Ok(states)
    }

    /// Reset simulation state
    pub fn reset(&mut self) -> SimulationResult<()> {
        match &mut self.backend {
            Backend::Behavioral(be) => {
                be.reset();
            }
            Backend::GateLevel(rt) => {
                rt.reset()?;
            }
        }
        self.current_cycle = 0;
        self.waveform_history.clear();
        Ok(())
    }

    /// Get current cycle count
    pub fn cycle(&self) -> u64 {
        self.current_cycle
    }

    /// Get waveform history (if capture_waveforms enabled)
    pub fn waveform_history(&self) -> &[SimulationState] {
        &self.waveform_history
    }
}

// Helper functions
fn bytes_to_u64(bytes: &[u8]) -> u64 {
    let mut result = 0u64;
    for (i, &byte) in bytes.iter().enumerate().take(8) {
        result |= (byte as u64) << (i * 8);
    }
    result
}

fn u64_to_bytes(value: u64, byte_count: usize) -> Vec<u8> {
    let mut bytes = vec![0u8; byte_count];
    for (i, byte) in bytes.iter_mut().enumerate().take(8.min(byte_count)) {
        *byte = ((value >> (i * 8)) & 0xFF) as u8;
    }
    bytes
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_unified_sim_config_defaults() {
        let config = UnifiedSimConfig::default();
        assert_eq!(config.level, SimLevel::Behavioral);
        assert_eq!(config.hw_accel, HwAccel::Auto);
    }

    #[test]
    fn test_unified_sim_config_builders() {
        let behavioral = UnifiedSimConfig::behavioral();
        assert_eq!(behavioral.level, SimLevel::Behavioral);

        let gate_level = UnifiedSimConfig::gate_level();
        assert_eq!(gate_level.level, SimLevel::GateLevel);

        let with_gpu = UnifiedSimConfig::gate_level().with_hw_accel(HwAccel::Gpu);
        assert_eq!(with_gpu.hw_accel, HwAccel::Gpu);
    }

    #[test]
    fn test_bytes_conversion() {
        assert_eq!(bytes_to_u64(&[0x12, 0x34]), 0x3412);
        assert_eq!(u64_to_bytes(0x3412, 2), vec![0x12, 0x34]);
    }
}
