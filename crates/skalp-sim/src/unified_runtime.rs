//! Unified Simulation Runtime
//!
//! Provides a single interface for both behavioral and gate-level simulation,
//! with automatic GPU acceleration when available.
//!
//! # Simulation Levels
//!
//! - **Behavioral**: Uses `skalp_sir::SirModule` (higher-level representation)
//! - **GateLevel**: Uses `skalp_sim::sir::Sir` (primitive-based gate netlist)
//!
//! # Hardware Acceleration
//!
//! - **CPU**: Always available, uses `GateLevelSimulator` for gate-level
//! - **GPU**: Uses Metal on macOS via `GpuGateRuntime`
//!
//! # Pipeline Annotations
//!
//! When synthesis performs register retiming, the behavioral model may have
//! different latency than the gate-level implementation. Pipeline annotations
//! can be loaded to adjust behavioral simulation to match gate-level timing.
//!
//! ```ignore
//! let mut sim = UnifiedSimulator::new(config)?;
//! sim.load_pipeline_annotations("build/pipeline_annotations.toml")?;
//! ```
//!
//! # Example
//!
//! ```ignore
//! use skalp_sim::{UnifiedSimulator, UnifiedSimConfig, SimLevel, HwAccel};
//!
//! // Create simulator for gate-level with GPU acceleration
//! let config = UnifiedSimConfig {
//!     level: SimLevel::GateLevel,
//!     hw_accel: HwAccel::Auto,
//!     max_cycles: 1000,
//!     capture_waveforms: true,
//! };
//!
//! let mut sim = UnifiedSimulator::new(config)?;
//! sim.load_gate_level(&sir)?;
//! sim.set_input("clk", 0);
//! sim.set_input("a", 42);
//! sim.run(100);
//! let result = sim.get_output("y");
//! ```

use skalp_lir::synth::PipelineAnnotations;
use std::collections::{HashMap, VecDeque};
use std::path::Path;

/// Simulation abstraction level
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum SimLevel {
    /// Behavioral simulation (MIR → behavioral SIR)
    #[default]
    Behavioral,
    /// Gate-level simulation (MIR → WordLir → GateNetlist → SIR)
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
#[derive(Debug, Clone, Default)]
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

/// Simulation state snapshot for waveform capture
#[derive(Debug, Clone)]
pub struct SimulationSnapshot {
    /// Cycle number
    pub cycle: u64,
    /// Signal values (name -> value as u64)
    pub signals: HashMap<String, u64>,
}

/// Result from unified simulation
#[derive(Debug, Clone)]
pub struct UnifiedSimResult {
    /// Number of cycles simulated
    pub cycles: u64,
    /// Final output values
    pub outputs: HashMap<String, u64>,
    /// Waveform snapshots (if capture_waveforms was enabled)
    pub waveforms: Vec<SimulationSnapshot>,
    /// Whether GPU was used
    pub used_gpu: bool,
}

/// Unified simulator supporting both behavioral and gate-level simulation
pub struct UnifiedSimulator {
    config: UnifiedSimConfig,
    backend: SimulatorBackend,
    waveforms: Vec<SimulationSnapshot>,
    current_cycle: u64,
    /// Pipeline annotations loaded from synthesis
    pipeline_annotations: Option<PipelineAnnotations>,
    /// Output delay buffers for latency adjustment
    /// Each output has a VecDeque that acts as a delay line
    output_delay_buffers: HashMap<String, VecDeque<u64>>,
    /// Latency adjustment per output (in cycles)
    latency_adjustments: HashMap<String, u32>,
}

/// Internal backend enum to hold the actual simulator
enum SimulatorBackend {
    /// Not yet loaded
    Uninitialized,
    /// Gate-level CPU simulation
    GateLevelCpu(crate::gate_simulator::GateLevelSimulator),
    /// Gate-level GPU simulation (macOS only)
    #[cfg(target_os = "macos")]
    GateLevelGpu(crate::gpu_gate_runtime::GpuGateRuntime),
}

impl UnifiedSimulator {
    /// Create a new unified simulator with the given configuration
    pub fn new(config: UnifiedSimConfig) -> Result<Self, String> {
        Ok(Self {
            config,
            backend: SimulatorBackend::Uninitialized,
            waveforms: Vec::new(),
            current_cycle: 0,
            pipeline_annotations: None,
            output_delay_buffers: HashMap::new(),
            latency_adjustments: HashMap::new(),
        })
    }

    /// Load pipeline annotations from a TOML file
    ///
    /// Pipeline annotations describe latency adjustments needed to match
    /// behavioral simulation with gate-level timing after register retiming.
    ///
    /// # Arguments
    /// * `path` - Path to the `pipeline_annotations.toml` file
    ///
    /// # Example
    /// ```ignore
    /// sim.load_pipeline_annotations("build/pipeline_annotations.toml")?;
    /// ```
    pub fn load_pipeline_annotations<P: AsRef<Path>>(&mut self, path: P) -> Result<(), String> {
        let annotations = PipelineAnnotations::read_toml(path.as_ref())
            .map_err(|e| format!("Failed to read pipeline annotations: {}", e))?;

        // Extract total latency adjustment per module
        for module in &annotations.modules {
            let adjustment = module
                .final_latency_cycles
                .saturating_sub(module.original_latency_cycles);

            if adjustment > 0 {
                // Apply the latency adjustment to all outputs of this module
                // For now, we use the module name as a prefix for outputs
                self.latency_adjustments
                    .insert(module.name.clone(), adjustment);

                eprintln!(
                    "[SIM] Pipeline latency adjustment: {} = +{} cycles",
                    module.name, adjustment
                );
            }
        }

        if annotations.has_retiming() {
            eprintln!(
                "[SIM] Loaded pipeline annotations: {}",
                annotations.summary()
            );
        }

        self.pipeline_annotations = Some(annotations);
        Ok(())
    }

    /// Set latency adjustment for a specific output
    ///
    /// This allows manual override of latency adjustments for testing
    /// or when annotations are not available.
    pub fn set_output_latency(&mut self, output_name: &str, cycles: u32) {
        if cycles > 0 {
            self.latency_adjustments
                .insert(output_name.to_string(), cycles);
            // Initialize the delay buffer with zeros
            let mut buffer = VecDeque::with_capacity(cycles as usize + 1);
            for _ in 0..cycles {
                buffer.push_back(0);
            }
            self.output_delay_buffers
                .insert(output_name.to_string(), buffer);
        } else {
            self.latency_adjustments.remove(output_name);
            self.output_delay_buffers.remove(output_name);
        }
    }

    /// Get total latency adjustment applied
    pub fn total_latency_adjustment(&self) -> u32 {
        self.latency_adjustments.values().sum()
    }

    /// Check if pipeline annotations are loaded
    pub fn has_pipeline_annotations(&self) -> bool {
        self.pipeline_annotations.is_some()
    }

    /// Load a gate-level SIR for simulation
    pub fn load_gate_level(&mut self, sir: &crate::sir::Sir) -> Result<(), String> {
        let use_gpu = match self.config.hw_accel {
            HwAccel::Cpu => false,
            HwAccel::Gpu => true,
            HwAccel::Auto => cfg!(target_os = "macos"),
        };

        if use_gpu {
            #[cfg(target_os = "macos")]
            {
                match crate::gpu_gate_runtime::GpuGateRuntime::new(sir) {
                    Ok(runtime) => {
                        self.backend = SimulatorBackend::GateLevelGpu(runtime);
                        return Ok(());
                    }
                    Err(e) => {
                        eprintln!("GPU initialization failed, falling back to CPU: {}", e);
                        // Fall through to CPU
                    }
                }
            }
            #[cfg(not(target_os = "macos"))]
            {
                eprintln!("GPU requested but not available on this platform, using CPU");
            }
        }

        // CPU fallback
        self.backend =
            SimulatorBackend::GateLevelCpu(crate::gate_simulator::GateLevelSimulator::new(sir));
        Ok(())
    }

    /// Check if GPU is being used
    pub fn is_using_gpu(&self) -> bool {
        #[cfg(target_os = "macos")]
        {
            matches!(self.backend, SimulatorBackend::GateLevelGpu(_))
        }
        #[cfg(not(target_os = "macos"))]
        {
            false
        }
    }

    /// Get device info string
    pub fn device_info(&self) -> String {
        match &self.backend {
            SimulatorBackend::Uninitialized => "Not initialized".to_string(),
            SimulatorBackend::GateLevelCpu(_) => "CPU".to_string(),
            #[cfg(target_os = "macos")]
            SimulatorBackend::GateLevelGpu(gpu) => format!("GPU (Metal): {}", gpu.device_name()),
        }
    }

    /// Set an input value (as u64)
    pub fn set_input(&mut self, name: &str, value: u64) {
        match &mut self.backend {
            SimulatorBackend::Uninitialized => {
                eprintln!("Warning: set_input called before loading design");
            }
            SimulatorBackend::GateLevelCpu(sim) => {
                // Convert u64 to bool vector
                let bits: Vec<bool> = (0..64).map(|i| (value >> i) & 1 == 1).collect();
                sim.set_input(name, &bits);
            }
            #[cfg(target_os = "macos")]
            SimulatorBackend::GateLevelGpu(runtime) => {
                runtime.set_input_u64(name, value);
            }
        }
    }

    /// Get an output value (as u64), with latency adjustment applied
    ///
    /// If pipeline annotations have been loaded and this output has a
    /// latency adjustment, the delayed value is returned. Otherwise,
    /// returns the current (raw) value.
    pub fn get_output(&self, name: &str) -> Option<u64> {
        // Check if this output has a delay buffer with values
        if let Some(buffer) = self.output_delay_buffers.get(name) {
            // Return the oldest value in the buffer (front)
            return buffer.front().copied();
        }

        // No latency adjustment, return raw value
        self.get_output_raw(name)
    }

    /// Get the raw (non-delayed) output value
    ///
    /// This bypasses any latency adjustment and returns the actual
    /// current output value from the simulation backend.
    pub fn get_output_raw(&self, name: &str) -> Option<u64> {
        match &self.backend {
            SimulatorBackend::Uninitialized => None,
            SimulatorBackend::GateLevelCpu(sim) => sim.get_output(name).map(|bits| {
                bits.iter()
                    .enumerate()
                    .filter(|(_, &b)| b)
                    .map(|(i, _)| 1u64 << i)
                    .sum()
            }),
            #[cfg(target_os = "macos")]
            SimulatorBackend::GateLevelGpu(runtime) => runtime.get_output_u64(name),
        }
    }

    /// Get all output values
    pub fn get_all_outputs(&self) -> HashMap<String, u64> {
        let mut outputs = HashMap::new();

        let output_names: Vec<String> = match &self.backend {
            SimulatorBackend::Uninitialized => vec![],
            SimulatorBackend::GateLevelCpu(sim) => sim.get_output_names(),
            #[cfg(target_os = "macos")]
            SimulatorBackend::GateLevelGpu(runtime) => runtime.get_output_names(),
        };

        for name in output_names {
            if let Some(value) = self.get_output(&name) {
                outputs.insert(name, value);
            }
        }

        outputs
    }

    /// Step simulation by one cycle
    pub fn step(&mut self) {
        match &mut self.backend {
            SimulatorBackend::Uninitialized => {
                eprintln!("Warning: step called before loading design");
            }
            SimulatorBackend::GateLevelCpu(sim) => {
                sim.step();
            }
            #[cfg(target_os = "macos")]
            SimulatorBackend::GateLevelGpu(runtime) => {
                runtime.step();
            }
        }

        self.current_cycle += 1;

        // Update delay buffers for latency adjustment
        // Push current raw values, pop oldest values
        for (output_name, delay) in &self.latency_adjustments {
            if let Some(raw_value) = self.get_output_raw(output_name) {
                let buffer = self
                    .output_delay_buffers
                    .entry(output_name.clone())
                    .or_insert_with(|| {
                        // Initialize buffer with zeros for the delay depth
                        let mut buf = VecDeque::with_capacity(*delay as usize + 1);
                        for _ in 0..*delay {
                            buf.push_back(0);
                        }
                        buf
                    });

                // Push new value at back
                buffer.push_back(raw_value);

                // Pop oldest value from front (maintains delay)
                if buffer.len() > *delay as usize {
                    buffer.pop_front();
                }
            }
        }

        // Capture waveform if enabled
        if self.config.capture_waveforms {
            let snapshot = SimulationSnapshot {
                cycle: self.current_cycle,
                signals: self.get_all_outputs(),
            };
            self.waveforms.push(snapshot);
        }
    }

    /// Run simulation with clock toggling for a given number of cycles
    pub fn run_clocked(&mut self, cycles: u64, clock_name: &str) -> UnifiedSimResult {
        for _ in 0..cycles {
            // Low phase
            self.set_input(clock_name, 0);
            self.step();
            // High phase
            self.set_input(clock_name, 1);
            self.step();
        }

        UnifiedSimResult {
            cycles: self.current_cycle,
            outputs: self.get_all_outputs(),
            waveforms: self.waveforms.clone(),
            used_gpu: self.is_using_gpu(),
        }
    }

    /// Run simulation for a given number of steps (without clock toggling)
    pub fn run(&mut self, steps: u64) -> UnifiedSimResult {
        for _ in 0..steps {
            self.step();
        }

        UnifiedSimResult {
            cycles: self.current_cycle,
            outputs: self.get_all_outputs(),
            waveforms: self.waveforms.clone(),
            used_gpu: self.is_using_gpu(),
        }
    }

    /// Reset simulation state
    pub fn reset(&mut self) {
        match &mut self.backend {
            SimulatorBackend::Uninitialized => {}
            SimulatorBackend::GateLevelCpu(sim) => {
                sim.reset();
            }
            #[cfg(target_os = "macos")]
            SimulatorBackend::GateLevelGpu(runtime) => {
                runtime.reset();
            }
        }
        self.current_cycle = 0;
        self.waveforms.clear();

        // Reset delay buffers to zeros (maintaining the same depth)
        for (output_name, delay) in &self.latency_adjustments {
            let buffer = self
                .output_delay_buffers
                .entry(output_name.clone())
                .or_default();
            buffer.clear();
            for _ in 0..*delay {
                buffer.push_back(0);
            }
        }
    }

    /// Get current cycle count
    pub fn current_cycle(&self) -> u64 {
        self.current_cycle
    }

    /// Get captured waveforms
    pub fn get_waveforms(&self) -> &[SimulationSnapshot] {
        &self.waveforms
    }

    /// Get input port names
    pub fn get_input_names(&self) -> Vec<String> {
        match &self.backend {
            SimulatorBackend::Uninitialized => vec![],
            SimulatorBackend::GateLevelCpu(sim) => sim.get_input_names(),
            #[cfg(target_os = "macos")]
            SimulatorBackend::GateLevelGpu(runtime) => runtime.get_input_names(),
        }
    }

    /// Get output port names
    pub fn get_output_names(&self) -> Vec<String> {
        match &self.backend {
            SimulatorBackend::Uninitialized => vec![],
            SimulatorBackend::GateLevelCpu(sim) => sim.get_output_names(),
            #[cfg(target_os = "macos")]
            SimulatorBackend::GateLevelGpu(runtime) => runtime.get_output_names(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_config_defaults() {
        let config = UnifiedSimConfig::default();
        assert_eq!(config.level, SimLevel::Behavioral);
        assert_eq!(config.hw_accel, HwAccel::Auto);
        assert_eq!(config.max_cycles, 0);
        assert!(!config.capture_waveforms);
    }

    #[test]
    fn test_simulator_creation() {
        let config = UnifiedSimConfig {
            level: SimLevel::GateLevel,
            hw_accel: HwAccel::Cpu,
            max_cycles: 100,
            capture_waveforms: true,
        };
        let sim = UnifiedSimulator::new(config);
        assert!(sim.is_ok());
    }

    #[test]
    fn test_latency_adjustment() {
        let config = UnifiedSimConfig::default();
        let mut sim = UnifiedSimulator::new(config).unwrap();

        // Set a 2-cycle latency adjustment for output "count"
        sim.set_output_latency("count", 2);

        // Verify the adjustment is stored
        assert_eq!(sim.total_latency_adjustment(), 2);
        assert!(!sim.has_pipeline_annotations()); // No file loaded

        // Verify the delay buffer is initialized
        assert!(sim.output_delay_buffers.contains_key("count"));
        assert_eq!(sim.output_delay_buffers.get("count").unwrap().len(), 2);

        // Remove the adjustment
        sim.set_output_latency("count", 0);
        assert_eq!(sim.total_latency_adjustment(), 0);
    }

    #[test]
    fn test_delay_buffer_behavior() {
        let config = UnifiedSimConfig::default();
        let mut sim = UnifiedSimulator::new(config).unwrap();

        // Set a 2-cycle latency for "out1"
        sim.set_output_latency("out1", 2);

        // Initially, buffer should be [0, 0]
        let buffer = sim.output_delay_buffers.get("out1").unwrap();
        assert_eq!(buffer.len(), 2);
        assert_eq!(*buffer.front().unwrap(), 0);

        // Reset should reinitialize the buffers
        sim.reset();
        let buffer = sim.output_delay_buffers.get("out1").unwrap();
        assert_eq!(buffer.len(), 2);
    }
}
