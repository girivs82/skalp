//! Unified Simulation Runtime
//!
//! Provides a single interface for both behavioral and gate-level simulation,
//! with automatic GPU acceleration when available. Supports both synchronous
//! (clocked) and asynchronous (NCL) circuit simulation.
//!
//! # Simulation Levels
//!
//! - **Behavioral**: Uses `skalp_sir::SirModule` (higher-level representation)
//! - **GateLevel**: Uses `skalp_sim::sir::Sir` (primitive-based gate netlist)
//!
//! # Circuit Modes
//!
//! - **Sync** (default): Clock-driven simulation with DFFs
//! - **Async/NCL**: Null Convention Logic with wavefront propagation and THmn gates
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
//! # Example (Synchronous)
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
//!     ..Default::default()
//! };
//!
//! let mut sim = UnifiedSimulator::new(config)?;
//! sim.load_gate_level(&sir)?;
//! sim.set_input("clk", 0);
//! sim.set_input("a", 42);
//! sim.run(100);
//! let result = sim.get_output("y");
//! ```
//!
//! # Example (NCL/Async)
//!
//! ```ignore
//! use skalp_sim::{UnifiedSimulator, UnifiedSimConfig, SimLevel, CircuitMode};
//!
//! let config = UnifiedSimConfig {
//!     level: SimLevel::GateLevel,
//!     circuit_mode: CircuitMode::Ncl,
//!     max_iterations: 10000,
//!     ..Default::default()
//! };
//!
//! let mut sim = UnifiedSimulator::new(config)?;
//! sim.load_ncl_gate_level(&gate_netlist)?;
//! sim.set_ncl_input("a", 5, 8);  // a = 5, 8-bit dual-rail
//! sim.run_until_stable();        // Iterate until no changes
//! let result = sim.get_ncl_output("y", 8);
//! ```

use crate::ncl_sim::{NclSimConfig, NclSimStats, NclSimulator};
use skalp_lir::gate_netlist::GateNetlist;
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

/// Circuit timing mode
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum CircuitMode {
    /// Synchronous (clocked) circuits - uses clock edges and DFFs
    #[default]
    Sync,
    /// Asynchronous NCL (Null Convention Logic) - uses wavefront propagation
    /// with THmn threshold gates and dual-rail encoding
    Ncl,
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
    /// Circuit timing mode (sync vs async/NCL)
    pub circuit_mode: CircuitMode,
    /// Hardware acceleration mode
    pub hw_accel: HwAccel,
    /// Maximum cycles before timeout (for sync mode)
    pub max_cycles: u64,
    /// Maximum iterations per wavefront (for NCL mode)
    pub max_iterations: u32,
    /// Whether to capture waveforms
    pub capture_waveforms: bool,
    /// Enable debug output for NCL simulation
    pub ncl_debug: bool,
}

impl Default for UnifiedSimConfig {
    fn default() -> Self {
        Self {
            level: SimLevel::default(),
            circuit_mode: CircuitMode::default(),
            hw_accel: HwAccel::default(),
            max_cycles: 0,
            max_iterations: 10000,
            capture_waveforms: false,
            ncl_debug: false,
        }
    }
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
    /// Number of cycles simulated (sync mode)
    pub cycles: u64,
    /// Number of iterations run (NCL mode)
    pub iterations: u64,
    /// Number of wavefronts completed (NCL mode)
    pub wavefronts: u64,
    /// Final output values
    pub outputs: HashMap<String, u64>,
    /// Waveform snapshots (if capture_waveforms was enabled)
    pub waveforms: Vec<SimulationSnapshot>,
    /// Whether GPU was used
    pub used_gpu: bool,
    /// Whether simulation is stable (NCL mode)
    pub is_stable: bool,
    /// Circuit mode used
    pub circuit_mode: CircuitMode,
}

impl Default for UnifiedSimResult {
    fn default() -> Self {
        Self {
            cycles: 0,
            iterations: 0,
            wavefronts: 0,
            outputs: HashMap::new(),
            waveforms: Vec::new(),
            used_gpu: false,
            is_stable: true,
            circuit_mode: CircuitMode::Sync,
        }
    }
}

/// Unified simulator supporting both behavioral and gate-level simulation
pub struct UnifiedSimulator {
    config: UnifiedSimConfig,
    backend: SimulatorBackend,
    waveforms: Vec<SimulationSnapshot>,
    current_cycle: u64,
    /// Total iterations for NCL mode
    total_iterations: u64,
    /// Total wavefronts completed for NCL mode
    total_wavefronts: u64,
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
    /// Gate-level CPU simulation (synchronous)
    GateLevelCpu(crate::gate_simulator::GateLevelSimulator),
    /// Gate-level GPU simulation (macOS only, synchronous)
    #[cfg(target_os = "macos")]
    GateLevelGpu(crate::gpu_gate_runtime::GpuGateRuntime),
    /// NCL gate-level CPU simulation (asynchronous)
    NclCpu(NclSimulator),
    /// NCL gate-level GPU simulation (macOS only, asynchronous)
    #[cfg(target_os = "macos")]
    NclGpu(crate::gpu_ncl_runtime::GpuNclRuntime),
}

impl UnifiedSimulator {
    /// Create a new unified simulator with the given configuration
    pub fn new(config: UnifiedSimConfig) -> Result<Self, String> {
        Ok(Self {
            config,
            backend: SimulatorBackend::Uninitialized,
            waveforms: Vec::new(),
            current_cycle: 0,
            total_iterations: 0,
            total_wavefronts: 0,
            pipeline_annotations: None,
            output_delay_buffers: HashMap::new(),
            latency_adjustments: HashMap::new(),
        })
    }

    /// Check if this simulator is in NCL (async) mode
    pub fn is_ncl_mode(&self) -> bool {
        self.config.circuit_mode == CircuitMode::Ncl
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

    /// Load a gate-level SIR for synchronous simulation
    pub fn load_gate_level(&mut self, sir: &crate::sir::Sir) -> Result<(), String> {
        if self.is_ncl_mode() {
            return Err("Cannot load SIR for NCL mode. Use load_ncl_gate_level() instead.".into());
        }

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

    /// Load a gate-level GateNetlist for NCL (async) simulation
    ///
    /// This uses the NclSimulator (CPU) or GpuNclRuntime (GPU) which properly
    /// handles THmn threshold gates with hysteresis and wavefront propagation.
    pub fn load_ncl_gate_level(&mut self, netlist: GateNetlist) -> Result<(), String> {
        if !self.is_ncl_mode() {
            return Err(
                "Cannot load GateNetlist for sync mode. Use load_gate_level() instead.".into(),
            );
        }

        let use_gpu = match self.config.hw_accel {
            HwAccel::Cpu => false,
            HwAccel::Gpu => true,
            HwAccel::Auto => cfg!(target_os = "macos"),
        };

        if use_gpu {
            #[cfg(target_os = "macos")]
            {
                match crate::gpu_ncl_runtime::GpuNclRuntime::new(netlist.clone()) {
                    Ok(runtime) => {
                        eprintln!(
                            "[SIM] NCL GPU runtime initialized: {}",
                            if runtime.is_using_gpu() {
                                "GPU"
                            } else {
                                "CPU fallback"
                            }
                        );
                        self.backend = SimulatorBackend::NclGpu(runtime);
                        return Ok(());
                    }
                    Err(e) => {
                        eprintln!("NCL GPU initialization failed, falling back to CPU: {}", e);
                        // Fall through to CPU
                    }
                }
            }
            #[cfg(not(target_os = "macos"))]
            {
                eprintln!("GPU requested but not available on this platform, using CPU for NCL");
            }
        }

        // CPU fallback
        let ncl_config = NclSimConfig {
            max_iterations: self.config.max_iterations,
            debug: self.config.ncl_debug,
            track_stages: true,
        };
        self.backend = SimulatorBackend::NclCpu(NclSimulator::new(netlist, ncl_config));
        Ok(())
    }

    /// Check if GPU is being used
    pub fn is_using_gpu(&self) -> bool {
        #[cfg(target_os = "macos")]
        {
            match &self.backend {
                SimulatorBackend::GateLevelGpu(_) => true,
                SimulatorBackend::NclGpu(runtime) => runtime.is_using_gpu(),
                _ => false,
            }
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
            SimulatorBackend::GateLevelCpu(_) => "CPU (Sync)".to_string(),
            #[cfg(target_os = "macos")]
            SimulatorBackend::GateLevelGpu(gpu) => format!("GPU (Metal): {}", gpu.device_name()),
            SimulatorBackend::NclCpu(_) => "CPU (NCL/Async)".to_string(),
            #[cfg(target_os = "macos")]
            SimulatorBackend::NclGpu(gpu) => {
                format!("GPU (Metal, NCL/Async): {}", gpu.device_name())
            }
        }
    }

    /// Set an input value (as u64)
    ///
    /// For NCL mode, this sets the dual-rail encoded value (use set_ncl_input for more control)
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
            SimulatorBackend::NclCpu(ncl_sim) => {
                // For NCL, infer width from value (up to 64 bits)
                let width = if value == 0 {
                    1
                } else {
                    64 - value.leading_zeros() as usize
                };
                ncl_sim.set_dual_rail_value(name, value, width.max(1));
            }
            #[cfg(target_os = "macos")]
            SimulatorBackend::NclGpu(runtime) => {
                // For NCL, infer width from value (up to 64 bits)
                let width = if value == 0 {
                    1
                } else {
                    64 - value.leading_zeros() as usize
                };
                runtime.set_dual_rail_value(name, value, width.max(1));
            }
        }
    }

    /// Check if an NCL signal exists (for debugging)
    pub fn has_ncl_signal(&self, name: &str) -> bool {
        match &self.backend {
            #[cfg(target_os = "macos")]
            SimulatorBackend::NclGpu(runtime) => runtime.has_signal(name),
            _ => false,
        }
    }

    /// Get list of NCL signal names (for debugging)
    pub fn ncl_signal_names(&self) -> Vec<String> {
        match &self.backend {
            #[cfg(target_os = "macos")]
            SimulatorBackend::NclGpu(runtime) => {
                runtime.signal_names().into_iter().cloned().collect()
            }
            _ => Vec::new(),
        }
    }

    /// Get list of all NCL net names (for debugging)
    pub fn ncl_all_net_names(&self) -> Vec<String> {
        match &self.backend {
            #[cfg(target_os = "macos")]
            SimulatorBackend::NclGpu(runtime) => runtime.all_net_names(),
            _ => Vec::new(),
        }
    }

    /// Get the nets for a specific NCL signal (for debugging)
    pub fn ncl_signal_nets(&self, name: &str) -> Option<Vec<skalp_lir::gate_netlist::GateNetId>> {
        match &self.backend {
            #[cfg(target_os = "macos")]
            SimulatorBackend::NclGpu(runtime) => runtime.signal_nets(name),
            _ => None,
        }
    }

    /// Get all net values (for debugging)
    pub fn ncl_get_all_net_values(&self) -> Vec<bool> {
        match &self.backend {
            #[cfg(target_os = "macos")]
            SimulatorBackend::NclGpu(runtime) => runtime.get_all_net_values(),
            _ => Vec::new(),
        }
    }

    /// Set an NCL dual-rail input with explicit width
    ///
    /// This is the preferred way to set inputs in NCL mode as it gives
    /// explicit control over the bit width.
    pub fn set_ncl_input(&mut self, name: &str, value: u64, width: usize) {
        eprintln!(
            "[UNIFIED] set_ncl_input: name='{}' value=0x{:X} width={}",
            name, value, width
        );
        match &mut self.backend {
            SimulatorBackend::NclCpu(ncl_sim) => {
                eprintln!("[UNIFIED] Calling ncl_sim.set_dual_rail_value");
                ncl_sim.set_dual_rail_value(name, value, width);
            }
            #[cfg(target_os = "macos")]
            SimulatorBackend::NclGpu(runtime) => {
                runtime.set_dual_rail_value(name, value, width);
            }
            _ => {
                eprintln!("Warning: set_ncl_input called but not in NCL mode");
            }
        }
    }

    /// Set NCL dual-rail input from u128 (for signals up to 128 bits)
    ///
    /// Use this for wide inputs like 96-bit vector data.
    pub fn set_ncl_input_u128(&mut self, name: &str, value: u128, width: usize) {
        match &mut self.backend {
            SimulatorBackend::NclCpu(ncl_sim) => {
                ncl_sim.set_dual_rail_value_u128(name, value, width);
            }
            #[cfg(target_os = "macos")]
            SimulatorBackend::NclGpu(runtime) => {
                runtime.set_dual_rail_value_u128(name, value, width);
            }
            _ => {
                eprintln!("Warning: set_ncl_input_u128 called but not in NCL mode");
            }
        }
    }

    /// Set NCL dual-rail input from two u128 values (for signals up to 256 bits)
    ///
    /// Use this for full 256-bit data words.
    /// low = bits [127:0], high = bits [255:128]
    pub fn set_ncl_input_u256(&mut self, name: &str, low: u128, high: u128, width: usize) {
        match &mut self.backend {
            SimulatorBackend::NclCpu(ncl_sim) => {
                ncl_sim.set_dual_rail_value_u256(name, low, high, width);
            }
            #[cfg(target_os = "macos")]
            SimulatorBackend::NclGpu(runtime) => {
                runtime.set_dual_rail_value_u256(name, low, high, width);
            }
            _ => {
                eprintln!("Warning: set_ncl_input_u256 called but not in NCL mode");
            }
        }
    }

    /// Set all NCL inputs to NULL (spacer phase)
    pub fn set_ncl_null(&mut self, name: &str, width: usize) {
        match &mut self.backend {
            SimulatorBackend::NclCpu(ncl_sim) => {
                ncl_sim.set_null(name, width);
            }
            #[cfg(target_os = "macos")]
            SimulatorBackend::NclGpu(runtime) => {
                runtime.set_null(name, width);
            }
            _ => {
                eprintln!("Warning: set_ncl_null called but not in NCL mode");
            }
        }
    }

    /// Get an NCL dual-rail output value
    ///
    /// Returns None if any bit is NULL or Invalid (not yet stable)
    pub fn get_ncl_output(&self, name: &str, width: usize) -> Option<u64> {
        match &self.backend {
            SimulatorBackend::NclCpu(ncl_sim) => ncl_sim.get_dual_rail_value(name, width),
            #[cfg(target_os = "macos")]
            SimulatorBackend::NclGpu(runtime) => runtime.get_dual_rail_value(name, width),
            _ => None,
        }
    }

    /// Check if NCL outputs are complete (all bits have valid DATA values)
    pub fn is_ncl_complete(&self) -> bool {
        match &self.backend {
            SimulatorBackend::NclCpu(ncl_sim) => ncl_sim.is_complete(),
            _ => false,
        }
    }

    /// Check if NCL outputs are all NULL
    pub fn is_ncl_null(&self) -> bool {
        match &self.backend {
            SimulatorBackend::NclCpu(ncl_sim) => ncl_sim.is_null(),
            _ => false,
        }
    }

    /// Get NCL simulation statistics
    pub fn get_ncl_stats(&self) -> Option<NclSimStats> {
        match &self.backend {
            SimulatorBackend::NclCpu(ncl_sim) => Some(ncl_sim.stats().clone()),
            _ => None,
        }
    }

    /// List all available signal names (for debugging NCL simulation)
    pub fn list_signal_names(&self) -> Vec<String> {
        match &self.backend {
            #[cfg(target_os = "macos")]
            SimulatorBackend::NclGpu(runtime) => runtime
                .signal_names()
                .iter()
                .map(|s| (*s).clone())
                .collect(),
            _ => Vec::new(),
        }
    }

    /// List all net names (for debugging NCL simulation)
    pub fn list_all_net_names(&self) -> Vec<String> {
        match &self.backend {
            #[cfg(target_os = "macos")]
            SimulatorBackend::NclGpu(runtime) => runtime.all_net_names(),
            _ => Vec::new(),
        }
    }

    /// Get raw net value by name (for debugging NCL simulation)
    pub fn get_net_value(&self, name: &str) -> Option<bool> {
        match &self.backend {
            #[cfg(target_os = "macos")]
            SimulatorBackend::NclGpu(runtime) => runtime.get_net_by_name(name),
            _ => None,
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
    ///
    /// For NCL mode, this returns the value if all bits are valid DATA,
    /// otherwise returns None.
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
            SimulatorBackend::NclCpu(ncl_sim) => {
                // For NCL, we need to determine width somehow
                // Try common widths and return first valid result
                for width in [64, 32, 16, 8, 4, 2, 1] {
                    if let Some(value) = ncl_sim.get_dual_rail_value(name, width) {
                        return Some(value);
                    }
                }
                None
            }
            #[cfg(target_os = "macos")]
            SimulatorBackend::NclGpu(runtime) => {
                // For NCL, try common widths and return first valid result
                for width in [64, 32, 16, 8, 4, 2, 1] {
                    if let Some(value) = runtime.get_dual_rail_value(name, width) {
                        return Some(value);
                    }
                }
                None
            }
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
            SimulatorBackend::NclCpu(_) => {
                // NCL outputs are named differently; return empty for now
                // Users should use get_ncl_output with explicit width
                vec![]
            }
            #[cfg(target_os = "macos")]
            SimulatorBackend::NclGpu(_) => {
                // NCL outputs are named differently; return empty for now
                // Users should use get_ncl_output with explicit width
                vec![]
            }
        };

        for name in output_names {
            if let Some(value) = self.get_output(&name) {
                outputs.insert(name, value);
            }
        }

        outputs
    }

    /// Step simulation by one cycle (sync mode) or one iteration (NCL mode)
    ///
    /// For NCL mode, this runs a single combinational propagation iteration.
    /// Use `run_until_stable()` for full wavefront propagation.
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
            SimulatorBackend::NclCpu(ncl_sim) => {
                ncl_sim.iterate();
                self.total_iterations += 1;
            }
            #[cfg(target_os = "macos")]
            SimulatorBackend::NclGpu(runtime) => {
                runtime.iterate();
                self.total_iterations += 1;
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

    /// Run simulation with clock toggling for a given number of cycles (sync mode only)
    pub fn run_clocked(&mut self, cycles: u64, clock_name: &str) -> UnifiedSimResult {
        if self.is_ncl_mode() {
            eprintln!("Warning: run_clocked called in NCL mode. NCL circuits don't use clocks.");
            return self.build_result();
        }

        for _ in 0..cycles {
            // Low phase
            self.set_input(clock_name, 0);
            self.step();
            // High phase
            self.set_input(clock_name, 1);
            self.step();
        }

        self.build_result()
    }

    /// Run simulation for a given number of steps (without clock toggling)
    ///
    /// For NCL mode, this runs a fixed number of iterations (not wavefronts).
    /// Use `run_until_stable()` for proper NCL wavefront propagation.
    pub fn run(&mut self, steps: u64) -> UnifiedSimResult {
        for _ in 0..steps {
            self.step();
        }

        self.build_result()
    }

    /// Run NCL simulation until stable (no signal changes)
    ///
    /// This is the primary simulation method for NCL/async circuits.
    /// It iterates until the circuit reaches a stable state (either all DATA or all NULL).
    ///
    /// Returns the result including number of iterations taken.
    pub fn run_until_stable(&mut self) -> UnifiedSimResult {
        match &mut self.backend {
            SimulatorBackend::NclCpu(ncl_sim) => {
                let iterations = ncl_sim.run_until_stable(self.config.max_iterations);
                self.total_iterations += iterations as u64;

                let stats = ncl_sim.stats();
                if stats.is_stable {
                    self.total_wavefronts += 1;
                }

                self.build_result()
            }
            #[cfg(target_os = "macos")]
            SimulatorBackend::NclGpu(runtime) => {
                let iterations = runtime.run_until_stable(self.config.max_iterations);
                self.total_iterations += iterations as u64;
                self.total_wavefronts += 1;
                self.build_result()
            }
            _ => {
                eprintln!("Warning: run_until_stable called but not in NCL mode");
                self.build_result()
            }
        }
    }

    /// Advance one complete NCL wavefront (DATA or NULL)
    ///
    /// For pipelined NCL designs, this advances computation through one stage.
    /// Call this to complete a NULL→DATA or DATA→NULL transition.
    pub fn advance_wavefront(&mut self) -> bool {
        match &mut self.backend {
            SimulatorBackend::NclCpu(ncl_sim) => {
                let success = ncl_sim.advance_wavefront();
                if success {
                    self.total_wavefronts += 1;
                }
                let stats = ncl_sim.stats();
                self.total_iterations = stats.iterations;
                success
            }
            #[cfg(target_os = "macos")]
            SimulatorBackend::NclGpu(runtime) => {
                // GPU NCL uses run_until_stable for wavefront advancement
                let iterations = runtime.run_until_stable(self.config.max_iterations);
                self.total_iterations += iterations as u64;
                let success = iterations < self.config.max_iterations;
                if success {
                    self.total_wavefronts += 1;
                }
                success
            }
            _ => {
                eprintln!("Warning: advance_wavefront called but not in NCL mode");
                false
            }
        }
    }

    /// Run a complete NCL computation cycle (DATA wavefront followed by NULL wavefront)
    ///
    /// This simulates one complete async computation:
    /// 1. Apply DATA inputs
    /// 2. Run until DATA wavefront completes
    /// 3. Apply NULL inputs (spacer)
    /// 4. Run until NULL wavefront completes
    ///
    /// Returns true if both wavefronts completed successfully.
    pub fn run_ncl_cycle(&mut self) -> bool {
        if !self.is_ncl_mode() {
            eprintln!("Warning: run_ncl_cycle called but not in NCL mode");
            return false;
        }

        // DATA wavefront should already be set by caller
        let data_complete = self.advance_wavefront();
        if !data_complete {
            return false;
        }

        // Now caller would set NULL inputs, then call advance_wavefront again
        // This method just advances one wavefront at a time
        data_complete
    }

    /// Build the result struct
    fn build_result(&self) -> UnifiedSimResult {
        let is_stable = match &self.backend {
            SimulatorBackend::NclCpu(ncl_sim) => ncl_sim.stats().is_stable,
            #[cfg(target_os = "macos")]
            SimulatorBackend::NclGpu(runtime) => runtime.is_stable(),
            _ => true,
        };

        UnifiedSimResult {
            cycles: self.current_cycle,
            iterations: self.total_iterations,
            wavefronts: self.total_wavefronts,
            outputs: self.get_all_outputs(),
            waveforms: self.waveforms.clone(),
            used_gpu: self.is_using_gpu(),
            is_stable,
            circuit_mode: self.config.circuit_mode,
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
            SimulatorBackend::NclCpu(ncl_sim) => {
                ncl_sim.reset();
            }
            #[cfg(target_os = "macos")]
            SimulatorBackend::NclGpu(runtime) => {
                runtime.reset();
            }
        }
        self.current_cycle = 0;
        self.total_iterations = 0;
        self.total_wavefronts = 0;
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

    /// Get total iterations (NCL mode)
    pub fn total_iterations(&self) -> u64 {
        self.total_iterations
    }

    /// Get total wavefronts completed (NCL mode)
    pub fn total_wavefronts(&self) -> u64 {
        self.total_wavefronts
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
            SimulatorBackend::NclCpu(ncl_sim) => {
                ncl_sim.input_names().into_iter().cloned().collect()
            }
            #[cfg(target_os = "macos")]
            SimulatorBackend::NclGpu(runtime) => runtime.get_input_names(),
        }
    }

    /// Get output port names
    pub fn get_output_names(&self) -> Vec<String> {
        match &self.backend {
            SimulatorBackend::Uninitialized => vec![],
            SimulatorBackend::GateLevelCpu(sim) => sim.get_output_names(),
            SimulatorBackend::NclCpu(ncl_sim) => {
                ncl_sim.output_names().into_iter().cloned().collect()
            }
            #[cfg(target_os = "macos")]
            SimulatorBackend::GateLevelGpu(runtime) => runtime.get_output_names(),
            #[cfg(target_os = "macos")]
            SimulatorBackend::NclGpu(runtime) => runtime.get_output_names(),
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
        assert_eq!(config.circuit_mode, CircuitMode::Sync);
        assert_eq!(config.hw_accel, HwAccel::Auto);
        assert_eq!(config.max_cycles, 0);
        assert_eq!(config.max_iterations, 10000);
        assert!(!config.capture_waveforms);
        assert!(!config.ncl_debug);
    }

    #[test]
    fn test_simulator_creation() {
        let config = UnifiedSimConfig {
            level: SimLevel::GateLevel,
            hw_accel: HwAccel::Cpu,
            max_cycles: 100,
            capture_waveforms: true,
            ..Default::default()
        };
        let sim = UnifiedSimulator::new(config);
        assert!(sim.is_ok());
    }

    #[test]
    fn test_ncl_mode_simulator() {
        let config = UnifiedSimConfig {
            level: SimLevel::GateLevel,
            circuit_mode: CircuitMode::Ncl,
            max_iterations: 5000,
            ..Default::default()
        };
        let sim = UnifiedSimulator::new(config).unwrap();
        assert!(sim.is_ncl_mode());
        assert_eq!(sim.device_info(), "Not initialized");
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
