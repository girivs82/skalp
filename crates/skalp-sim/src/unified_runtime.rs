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

use crate::compiled_cpu_runtime::CompiledCpuRuntime;
use crate::ncl_sim::{NclSimConfig, NclSimStats, NclSimulator};
use crate::simulator::SimulationRuntime;
use indexmap::IndexMap;
use skalp_lir::gate_netlist::GateNetlist;
use skalp_lir::synth::PipelineAnnotations;
use skalp_mir::name_registry::NameRegistry;
use skalp_sir::SirModule;
use std::collections::VecDeque;
use std::path::Path;

#[cfg(target_os = "macos")]
use crate::gpu_runtime::GpuRuntime;

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
    pub signals: IndexMap<String, u64>,
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
    pub outputs: IndexMap<String, u64>,
    /// Waveform snapshots (if capture_waveforms was enabled)
    pub waveforms: Vec<SimulationSnapshot>,
    /// Signal widths (display_name -> bit width) for waveform export
    pub signal_widths: IndexMap<String, usize>,
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
            outputs: IndexMap::new(),
            waveforms: Vec::new(),
            signal_widths: IndexMap::new(),
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
    output_delay_buffers: IndexMap<String, VecDeque<u64>>,
    /// Latency adjustment per output (in cycles)
    latency_adjustments: IndexMap<String, u32>,
    /// Behavioral input port names (stored when loading behavioral module)
    behavioral_input_names: Vec<String>,
    /// Behavioral output port names (stored when loading behavioral module)
    behavioral_output_names: Vec<String>,
    /// Name registry for resolving user-facing paths to internal names
    /// Users can use hierarchical paths (e.g., "bms.connected") which get
    /// resolved to collision-proof internal names (e.g., "_s0")
    name_registry: NameRegistry,
    /// Current input values (internal_name -> value) for waveform capture.
    /// Updated on every set_input() call so clock/reset/all inputs appear in waveforms.
    current_input_values: IndexMap<String, u64>,
}

/// Internal backend enum to hold the actual simulator
enum SimulatorBackend {
    /// Not yet loaded
    Uninitialized,
    /// Compiled CPU simulation (uses SirModule compiled to native C++)
    CompiledCpu(CompiledCpuRuntime),
    /// Behavioral GPU simulation (macOS only, uses SirModule with Metal)
    #[cfg(target_os = "macos")]
    BehavioralGpu(GpuRuntime),
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
            output_delay_buffers: IndexMap::new(),
            latency_adjustments: IndexMap::new(),
            behavioral_input_names: Vec::new(),
            behavioral_output_names: Vec::new(),
            name_registry: NameRegistry::new(),
            current_input_values: IndexMap::new(),
        })
    }

    /// Check if this simulator is in NCL (async) mode
    pub fn is_ncl_mode(&self) -> bool {
        self.config.circuit_mode == CircuitMode::Ncl
    }

    /// Resolve a user-facing signal path to its internal name
    ///
    /// Users can use hierarchical paths like "bms.connected" which get
    /// resolved to collision-proof internal names like "_s0".
    /// If no mapping exists, the original name is returned (for backward compatibility).
    ///
    /// # Arguments
    /// * `path` - User-facing signal path (e.g., "bms.connected" or "state_reg")
    ///
    /// # Returns
    /// The internal signal name to use for actual lookups
    fn resolve_path(&self, path: &str) -> String {
        // Try exact match first
        if let Some(internal) = self.name_registry.resolve(path) {
            return internal.to_string();
        }

        // Try converting underscore notation to dot notation
        // This handles testbench paths like "vertex_x" → "vertex.x"
        // which is how struct fields are registered in the name registry
        if path.contains('_') && !path.contains('.') {
            // Try replacing single underscores with dots for struct field access
            // We need to be careful here - only try reasonable conversions
            // For "vertex_x", try "vertex.x"
            // For "foo_bar_x", try "foo.bar.x", "foo.bar_x", "foo_bar.x"

            // Simple case: one underscore
            let parts: Vec<&str> = path.splitn(2, '_').collect();
            if parts.len() == 2 {
                let dot_path = format!("{}.{}", parts[0], parts[1]);
                if let Some(internal) = self.name_registry.resolve(&dot_path) {
                    return internal.to_string();
                }
            }

            // Try replacing the last underscore with a dot
            if let Some(last_underscore) = path.rfind('_') {
                let (base, field) = path.split_at(last_underscore);
                let dot_path = format!("{}.{}", base, &field[1..]); // Skip the underscore
                if let Some(internal) = self.name_registry.resolve(&dot_path) {
                    return internal.to_string();
                }
            }
        }

        // Fallback: use path as-is (for internal names or direct signal access)
        path.to_string()
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

            }
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
            self.latency_adjustments.shift_remove(output_name);
            self.output_delay_buffers.shift_remove(output_name);
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

        // Build name registry from SIR signal names
        // This creates user-friendly aliases for struct field signals
        // e.g., "bms__connected" -> user can reference as "bms.connected"
        self.build_gate_level_name_registry(sir);

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

    /// Build name registry from gate-level SIR
    ///
    /// BUG #237 FIX: Use the SIR's name_registry which contains proper mappings
    /// from user-facing paths (e.g., "rst", "state") to internal names (e.g., "_s1", "_s74")
    ///
    /// The SIR's name_registry is populated during MIR-to-SIR conversion and contains
    /// all the correct mappings. Previously this function was creating a new registry
    /// and guessing mappings based on `__` replacement, which doesn't work for
    /// collision-proof internal names like `_s0`, `_s1`, etc.
    fn build_gate_level_name_registry(&mut self, sir: &crate::sir::Sir) {
        // BUG #237 FIX: Clone the name registry from SIR instead of building our own
        // This ensures gate-level simulation uses the same signal name mappings
        // as behavioral simulation
        self.name_registry = sir.name_registry.clone();

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
                        self.backend = SimulatorBackend::NclGpu(runtime);
                        return Ok(());
                    }
                    Err(e) => {
                        // Fall through to CPU
                    }
                }
            }
            #[cfg(not(target_os = "macos"))]
            {
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

    /// Load a behavioral SirModule for simulation
    ///
    /// This is the high-level simulation mode using the original SIR representation
    /// before technology mapping. It's faster but less accurate than gate-level.
    ///
    /// # Arguments
    /// * `module` - The SirModule from MIR compilation
    ///
    /// # Example
    /// ```ignore
    /// let sir_module = compile_to_sir(&source)?;
    /// sim.load_behavioral(&sir_module).await?;
    /// ```
    pub async fn load_behavioral(&mut self, module: &SirModule) -> Result<(), String> {
        if self.config.level != SimLevel::Behavioral {
            return Err(
                "Config level is not Behavioral. Set level to SimLevel::Behavioral.".into(),
            );
        }

        // Store port names from the SirModule for later introspection
        self.behavioral_input_names = module.inputs.iter().map(|p| p.name.clone()).collect();
        self.behavioral_output_names = module.outputs.iter().map(|p| p.name.clone()).collect();

        // Store name registry for path resolution
        self.name_registry = module.name_registry.clone();

        let use_gpu = match self.config.hw_accel {
            HwAccel::Cpu => false,
            HwAccel::Gpu => true,
            HwAccel::Auto => cfg!(target_os = "macos"),
        };


        if use_gpu {
            #[cfg(target_os = "macos")]
            {
                match GpuRuntime::new().await {
                    Ok(mut runtime) => {
                        match runtime.initialize(module).await {
                            Err(_e) => {
                                // Fall through to CPU
                            }
                            Ok(()) => {
                                self.backend = SimulatorBackend::BehavioralGpu(runtime);
                                return Ok(());
                            }
                        }
                    }
                    Err(e) => {
                        eprintln!("GPU runtime creation failed, falling back to CPU: {}", e);
                        // Fall through to CPU
                    }
                }
            }
            #[cfg(not(target_os = "macos"))]
            {
                eprintln!("GPU requested but not available on this platform, using CPU");
            }
        }

        // Use compiled CPU runtime
        let mut runtime = CompiledCpuRuntime::new(module)
            .map_err(|e| format!("{}", e))?;
        runtime
            .initialize(module)
            .await
            .map_err(|e| format!("Compiled CPU init failed: {}", e))?;
        self.backend = SimulatorBackend::CompiledCpu(runtime);
        Ok(())
    }

    /// Check if GPU is being used
    pub fn is_using_gpu(&self) -> bool {
        #[cfg(target_os = "macos")]
        {
            match &self.backend {
                SimulatorBackend::BehavioralGpu(_) => true,
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
            SimulatorBackend::CompiledCpu(_) => "CPU (Compiled C++)".to_string(),
            #[cfg(target_os = "macos")]
            SimulatorBackend::BehavioralGpu(_) => "GPU (Metal, Behavioral)".to_string(),
            SimulatorBackend::GateLevelCpu(_) => "CPU (Gate-level, Sync)".to_string(),
            #[cfg(target_os = "macos")]
            SimulatorBackend::GateLevelGpu(gpu) => {
                format!("GPU (Metal, Gate-level): {}", gpu.device_name())
            }
            SimulatorBackend::NclCpu(_) => "CPU (NCL/Async)".to_string(),
            #[cfg(target_os = "macos")]
            SimulatorBackend::NclGpu(gpu) => {
                format!("GPU (Metal, NCL/Async): {}", gpu.device_name())
            }
        }
    }

    /// Set an input value (as u64)
    ///
    /// Accepts user-facing hierarchical paths (e.g., "bms.connected") which are
    /// automatically resolved to internal signal names.
    ///
    /// For NCL mode, this sets the dual-rail encoded value (use set_ncl_input for more control)
    pub async fn set_input(&mut self, name: &str, value: u64) {
        // Resolve user-facing path to internal name (for behavioral simulation)
        let internal_name = self.resolve_path(name);

        // Track current input value for waveform capture
        self.current_input_values.insert(internal_name.clone(), value);

        match &mut self.backend {
            SimulatorBackend::Uninitialized => {
                eprintln!("Warning: set_input called before loading design");
            }
            SimulatorBackend::CompiledCpu(runtime) => {
                // Convert u64 to bytes (little-endian)
                let bytes = value.to_le_bytes().to_vec();
                let _ = runtime.set_input(&internal_name, &bytes).await;
            }
            #[cfg(target_os = "macos")]
            SimulatorBackend::BehavioralGpu(runtime) => {
                // Convert u64 to bytes (little-endian)
                let bytes = value.to_le_bytes().to_vec();
                let _ = runtime.set_input(&internal_name, &bytes).await;
            }
            SimulatorBackend::GateLevelCpu(sim) => {
                // Gate-level netlists use user-facing names, not internal _s names
                // Convert u64 to bool vector
                let bits: Vec<bool> = (0..64).map(|i| (value >> i) & 1 == 1).collect();
                sim.set_input(name, &bits);
            }
            #[cfg(target_os = "macos")]
            SimulatorBackend::GateLevelGpu(runtime) => {
                // Gate-level netlists use user-facing names, not internal _s names
                runtime.set_input_u64(name, value);
            }
            SimulatorBackend::NclCpu(ncl_sim) => {
                // For NCL, infer width from value (up to 64 bits)
                let width = if value == 0 {
                    1
                } else {
                    64 - value.leading_zeros() as usize
                };
                ncl_sim.set_dual_rail_value(&internal_name, value, width.max(1));
            }
            #[cfg(target_os = "macos")]
            SimulatorBackend::NclGpu(runtime) => {
                // For NCL, infer width from value (up to 64 bits)
                let width = if value == 0 {
                    1
                } else {
                    64 - value.leading_zeros() as usize
                };
                runtime.set_dual_rail_value(&internal_name, value, width.max(1));
            }
        }
    }

    /// Check if an NCL signal exists (for debugging)
    pub fn has_ncl_signal(&self, name: &str) -> bool {
        let internal_name = self.resolve_path(name);
        match &self.backend {
            #[cfg(target_os = "macos")]
            SimulatorBackend::NclGpu(runtime) => runtime.has_signal(&internal_name),
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
        match &mut self.backend {
            SimulatorBackend::NclCpu(ncl_sim) => {
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
        let result = match &self.backend {
            SimulatorBackend::NclCpu(ncl_sim) => ncl_sim.get_dual_rail_value(name, width),
            #[cfg(target_os = "macos")]
            SimulatorBackend::NclGpu(runtime) => runtime.get_dual_rail_value(name, width),
            _ => None,
        };
        result
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
            SimulatorBackend::NclCpu(ncl_sim) => ncl_sim.get_net_value_by_name(name),
            #[cfg(target_os = "macos")]
            SimulatorBackend::NclGpu(runtime) => runtime.get_net_by_name(name),
            _ => None,
        }
    }

    /// Dump all nets matching a pattern (for debugging NCL simulation)
    pub fn dump_nets_matching(&self, pattern: &str) {
        match &self.backend {
            SimulatorBackend::NclCpu(ncl_sim) => ncl_sim.dump_nets_matching(pattern),
            _ => println!("  dump_nets_matching: not in NclCpu mode"),
        }
    }

    /// Count undriven-but-used nets (for debugging NCL simulation)
    pub fn count_undriven_nets(&self) -> (usize, Vec<String>) {
        match &self.backend {
            SimulatorBackend::NclCpu(ncl_sim) => ncl_sim.count_undriven_nets(),
            _ => (0, Vec::new()),
        }
    }

    /// Dump all gate-level signals (for debugging)
    /// Returns a vector of (signal_name, value) pairs sorted by name
    pub fn dump_gate_signals(&self) -> Vec<(String, Vec<bool>)> {
        match &self.backend {
            SimulatorBackend::GateLevelCpu(gate_sim) => gate_sim.dump_signals(),
            #[cfg(target_os = "macos")]
            SimulatorBackend::GateLevelGpu(runtime) => {
                // GPU runtime doesn't have dump_signals, return empty
                Vec::new()
            }
            _ => Vec::new(),
        }
    }

    /// Get a gate-level signal by name (for debugging)
    pub fn get_gate_signal(&self, name: &str) -> Option<Vec<bool>> {
        match &self.backend {
            SimulatorBackend::GateLevelCpu(gate_sim) => gate_sim.get_signal(name),
            #[cfg(target_os = "macos")]
            SimulatorBackend::GateLevelGpu(_runtime) => None, // GPU doesn't expose this
            _ => None,
        }
    }

    /// Get an output value (as u64), with latency adjustment applied
    ///
    /// If pipeline annotations have been loaded and this output has a
    /// latency adjustment, the delayed value is returned. Otherwise,
    /// returns the current (raw) value.
    pub async fn get_output(&self, name: &str) -> Option<u64> {
        // Check if this output has a delay buffer with values
        if let Some(buffer) = self.output_delay_buffers.get(name) {
            // Return the oldest value in the buffer (front)
            return buffer.front().copied();
        }

        // No latency adjustment, return raw value
        self.get_output_raw(name).await
    }

    /// Alias for get_output - returns the output value as u64
    ///
    /// This is a convenience method that matches the naming convention
    /// used in many test files.
    pub async fn get_output_u64(&self, name: &str) -> Option<u64> {
        self.get_output(name).await
    }

    /// Get the raw (non-delayed) output value
    ///
    /// This bypasses any latency adjustment and returns the actual
    /// current output value from the simulation backend.
    /// Accepts user-facing hierarchical paths which are resolved to internal names.
    ///
    /// For NCL mode, this returns the value if all bits are valid DATA,
    /// otherwise returns None.
    pub async fn get_output_raw(&self, name: &str) -> Option<u64> {
        // Resolve user-facing path to internal name
        let internal_name = self.resolve_path(name);

        match &self.backend {
            SimulatorBackend::Uninitialized => None,
            SimulatorBackend::CompiledCpu(runtime) => {
                // Get bytes from behavioral runtime and convert to u64
                runtime.get_output(&internal_name).await.ok().map(|bytes| {
                    let mut value = 0u64;
                    for (i, &byte) in bytes.iter().take(8).enumerate() {
                        value |= (byte as u64) << (i * 8);
                    }
                    value
                })
            }
            #[cfg(target_os = "macos")]
            SimulatorBackend::BehavioralGpu(runtime) => {
                // Get bytes from behavioral runtime and convert to u64
                runtime.get_output(&internal_name).await.ok().map(|bytes| {
                    let mut value = 0u64;
                    for (i, &byte) in bytes.iter().take(8).enumerate() {
                        value |= (byte as u64) << (i * 8);
                    }
                    value
                })
            }
            SimulatorBackend::GateLevelCpu(sim) => {
                // Gate-level netlists use user-facing names, not internal _s names
                sim.get_output(name).map(|bits| {
                    bits.iter()
                        .enumerate()
                        .filter(|(_, &b)| b)
                        .map(|(i, _)| 1u64 << i)
                        .sum()
                })
            }
            #[cfg(target_os = "macos")]
            SimulatorBackend::GateLevelGpu(runtime) => {
                // Gate-level netlists use user-facing names, not internal _s names
                runtime.get_output_u64(name)
            }
            SimulatorBackend::NclCpu(ncl_sim) => {
                // For NCL, we need to determine width somehow
                // Try common widths and return first valid result
                for width in [64, 32, 16, 8, 4, 2, 1] {
                    if let Some(value) = ncl_sim.get_dual_rail_value(&internal_name, width) {
                        return Some(value);
                    }
                }
                None
            }
            #[cfg(target_os = "macos")]
            SimulatorBackend::NclGpu(runtime) => {
                // For NCL, try common widths and return first valid result
                for width in [64, 32, 16, 8, 4, 2, 1] {
                    if let Some(value) = runtime.get_dual_rail_value(&internal_name, width) {
                        return Some(value);
                    }
                }
                None
            }
        }
    }

    /// Get all output values
    pub async fn get_all_outputs(&self) -> IndexMap<String, u64> {
        let mut outputs = IndexMap::new();

        // Include tracked input values (clk, rst, and all user inputs) for waveform capture
        for (internal_name, value) in &self.current_input_values {
            let display = self.name_registry.reverse_resolve(internal_name)
                .unwrap_or(internal_name)
                .to_string();
            outputs.insert(display, *value);
        }

        match &self.backend {
            SimulatorBackend::CompiledCpu(runtime) => {
                // Use structured signal list with user-facing names
                for (field_name, display_name, _width) in runtime.get_waveform_signals() {
                    if let Some(value) = self.get_output(&field_name).await {
                        outputs.insert(display_name, value);
                    }
                }
            }
            _ => {
                // Gate-level and other backends: use output names directly
                let output_names: Vec<String> = match &self.backend {
                    SimulatorBackend::Uninitialized => vec![],
                    SimulatorBackend::CompiledCpu(_) => unreachable!(),
                    #[cfg(target_os = "macos")]
                    SimulatorBackend::BehavioralGpu(_) => {
                        let mut names = self.behavioral_input_names.clone();
                        names.extend(self.behavioral_output_names.clone());
                        names
                    }
                    SimulatorBackend::GateLevelCpu(sim) => sim.get_output_names(),
                    #[cfg(target_os = "macos")]
                    SimulatorBackend::GateLevelGpu(runtime) => runtime.get_output_names(),
                    SimulatorBackend::NclCpu(_) => vec![],
                    #[cfg(target_os = "macos")]
                    SimulatorBackend::NclGpu(_) => vec![],
                };
                for name in output_names {
                    if let Some(value) = self.get_output(&name).await {
                        let display = self.name_registry.reverse_resolve(&name)
                            .unwrap_or(&name)
                            .to_string();
                        outputs.insert(display, value);
                    }
                }
            }
        }

        outputs
    }

    /// Step simulation by one cycle (sync mode) or one iteration (NCL mode)
    ///
    /// For NCL mode, this runs a single combinational propagation iteration.
    /// Use `run_until_stable()` for full wavefront propagation.
    pub async fn step(&mut self) {
        match &mut self.backend {
            SimulatorBackend::Uninitialized => {
                eprintln!("Warning: step called before loading design");
            }
            SimulatorBackend::CompiledCpu(runtime) => {
                let _ = runtime.step().await;
            }
            #[cfg(target_os = "macos")]
            SimulatorBackend::BehavioralGpu(runtime) => {
                let _ = runtime.step().await;
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
            if let Some(raw_value) = self.get_output_raw(output_name).await {
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
                signals: self.get_all_outputs().await,
            };
            self.waveforms.push(snapshot);
        }
    }

    /// Step simulation and return a signal snapshot for coverage tracking.
    ///
    /// For behavioral CPU: returns `SimulationState.signals` from the step.
    /// For gate-level CPU: returns signal snapshot as byte-encoded values.
    /// For other backends: steps normally and returns None.
    pub async fn step_with_snapshot(
        &mut self,
    ) -> Option<crate::simulator::SimulationState> {
        let result = match &mut self.backend {
            SimulatorBackend::CompiledCpu(runtime) => {
                // step() returns SimulationState with signals and registers
                runtime.step().await.ok()
            }
            #[cfg(target_os = "macos")]
            SimulatorBackend::BehavioralGpu(runtime) => {
                // GPU step() also returns SimulationState (UMA allows efficient readback)
                runtime.step().await.ok()
            }
            SimulatorBackend::GateLevelCpu(sim) => {
                sim.step();
                // Build a SimulationState from gate snapshot
                let gate_signals = sim.snapshot_signals();
                // Convert bool vecs to byte vecs for uniform representation
                let signals: IndexMap<String, Vec<u8>> = gate_signals
                    .into_iter()
                    .map(|(name, bits)| {
                        let num_bytes = (bits.len() + 7) / 8;
                        let mut bytes = vec![0u8; num_bytes];
                        for (i, &b) in bits.iter().enumerate() {
                            if b {
                                bytes[i / 8] |= 1 << (i % 8);
                            }
                        }
                        (name, bytes)
                    })
                    .collect();
                Some(crate::simulator::SimulationState {
                    cycle: self.current_cycle,
                    signals,
                    registers: IndexMap::new(),
                })
            }
            #[cfg(target_os = "macos")]
            SimulatorBackend::GateLevelGpu(runtime) => {
                runtime.step();
                // Build a SimulationState from GPU gate snapshot (dump_signals returns Vec<(name, bits)>)
                let gate_signals = runtime.dump_signals();
                let signals: IndexMap<String, Vec<u8>> = gate_signals
                    .into_iter()
                    .map(|(name, bits)| {
                        let num_bytes = (bits.len() + 7) / 8;
                        let mut bytes = vec![0u8; num_bytes];
                        for (i, &b) in bits.iter().enumerate() {
                            if b {
                                bytes[i / 8] |= 1 << (i % 8);
                            }
                        }
                        (name, bytes)
                    })
                    .collect();
                Some(crate::simulator::SimulationState {
                    cycle: self.current_cycle,
                    signals,
                    registers: IndexMap::new(),
                })
            }
            _ => {
                // Other backends (NCL): just step normally
                self.step().await;
                return None;
            }
        };

        self.current_cycle += 1;

        // Update delay buffers for latency adjustment
        for (output_name, delay) in &self.latency_adjustments {
            if let Some(raw_value) = self.get_output_raw(output_name).await {
                let buffer = self
                    .output_delay_buffers
                    .entry(output_name.clone())
                    .or_insert_with(|| {
                        let mut buf = VecDeque::with_capacity(*delay as usize + 1);
                        for _ in 0..*delay {
                            buf.push_back(0);
                        }
                        buf
                    });
                buffer.push_back(raw_value);
                if buffer.len() > *delay as usize {
                    buffer.pop_front();
                }
            }
        }

        // Capture waveform if enabled
        if self.config.capture_waveforms {
            let snapshot = SimulationSnapshot {
                cycle: self.current_cycle,
                signals: self.get_all_outputs().await,
            };
            self.waveforms.push(snapshot);
        }

        result
    }

    /// Run simulation with clock toggling for a given number of cycles (sync mode only)
    pub async fn run_clocked(&mut self, cycles: u64, clock_name: &str) -> UnifiedSimResult {
        if self.is_ncl_mode() {
            eprintln!("Warning: run_clocked called in NCL mode. NCL circuits don't use clocks.");
            return self.build_result().await;
        }

        for _ in 0..cycles {
            // Low phase
            self.set_input(clock_name, 0).await;
            self.step().await;
            // High phase
            self.set_input(clock_name, 1).await;
            self.step().await;
        }

        self.build_result().await
    }

    /// Run simulation for a given number of steps (without clock toggling)
    ///
    /// For NCL mode, this runs a fixed number of iterations (not wavefronts).
    /// Use `run_until_stable()` for proper NCL wavefront propagation.
    pub async fn run(&mut self, steps: u64) -> UnifiedSimResult {
        for _ in 0..steps {
            self.step().await;
        }

        self.build_result().await
    }

    /// PERF: Run multiple cycles in a single GPU dispatch (GPU behavioral only)
    ///
    /// This provides massive speedup (100-1000x) for large cycle counts by
    /// eliminating per-cycle CPU<->GPU synchronization overhead.
    ///
    /// Falls back to regular step-by-step execution for non-GPU backends.
    pub async fn run_batched(&mut self, cycles: u64) -> UnifiedSimResult {
        match &mut self.backend {
            #[cfg(target_os = "macos")]
            SimulatorBackend::BehavioralGpu(gpu_runtime) => {
                // Use GPU batched kernel for massive speedup
                let _state = gpu_runtime.run_batched(cycles).await;
                self.current_cycle += cycles;
                self.build_result().await
            }
            _ => {
                // Fallback to step-by-step for non-GPU backends
                // BUG FIX: Must toggle clock like run_clocked does
                // (testbench expects run_batched to handle clock toggling internally)
                self.run_clocked(cycles, "clk").await
            }
        }
    }

    /// Run NCL simulation until stable (no signal changes)
    ///
    /// This is the primary simulation method for NCL/async circuits.
    /// It iterates until the circuit reaches a stable state (either all DATA or all NULL).
    ///
    /// Returns the result including number of iterations taken.
    pub async fn run_until_stable(&mut self) -> UnifiedSimResult {
        match &mut self.backend {
            SimulatorBackend::NclCpu(ncl_sim) => {
                let iterations = ncl_sim.run_until_stable(self.config.max_iterations);
                self.total_iterations += iterations as u64;

                let stats = ncl_sim.stats();
                if stats.is_stable {
                    self.total_wavefronts += 1;
                }

                self.build_result().await
            }
            #[cfg(target_os = "macos")]
            SimulatorBackend::NclGpu(runtime) => {
                let iterations = runtime.run_until_stable(self.config.max_iterations);
                self.total_iterations += iterations as u64;
                self.total_wavefronts += 1;
                self.build_result().await
            }
            _ => {
                eprintln!("Warning: run_until_stable called but not in NCL mode");
                self.build_result().await
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
    async fn build_result(&self) -> UnifiedSimResult {
        let is_stable = match &self.backend {
            SimulatorBackend::NclCpu(ncl_sim) => ncl_sim.stats().is_stable,
            #[cfg(target_os = "macos")]
            SimulatorBackend::NclGpu(runtime) => runtime.is_stable(),
            _ => true,
        };

        let signal_widths = match &self.backend {
            SimulatorBackend::CompiledCpu(runtime) => {
                runtime.get_waveform_signals().into_iter()
                    .map(|(_, display, width)| (display, width))
                    .collect()
            }
            _ => IndexMap::new(),
        };

        UnifiedSimResult {
            cycles: self.current_cycle,
            iterations: self.total_iterations,
            wavefronts: self.total_wavefronts,
            outputs: self.get_all_outputs().await,
            waveforms: self.waveforms.clone(),
            signal_widths,
            used_gpu: self.is_using_gpu(),
            is_stable,
            circuit_mode: self.config.circuit_mode,
        }
    }

    /// Reset simulation state
    pub fn reset(&mut self) {
        match &mut self.backend {
            SimulatorBackend::Uninitialized => {}
            SimulatorBackend::CompiledCpu(_) => {
                // Behavioral CPU runtime state is managed externally
                // No direct reset needed - state resets when inputs change
            }
            #[cfg(target_os = "macos")]
            SimulatorBackend::BehavioralGpu(_) => {
                // Behavioral GPU runtime state is managed externally
                // No direct reset needed - state resets when inputs change
            }
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

    /// Get signal widths (display_name -> bit width) for waveform export
    pub fn get_signal_widths(&self) -> IndexMap<String, usize> {
        match &self.backend {
            SimulatorBackend::CompiledCpu(runtime) => {
                runtime.get_waveform_signals().into_iter()
                    .map(|(_, display, width)| (display, width))
                    .collect()
            }
            _ => {
                // Use name registry to get widths for all known signals
                let mut widths = IndexMap::new();
                for internal_name in self.behavioral_input_names.iter()
                    .chain(self.behavioral_output_names.iter())
                {
                    if let Some(entry) = self.name_registry.get_entry_by_internal(internal_name) {
                        widths.insert(entry.hierarchical_path.clone(), entry.width);
                    }
                }
                widths
            }
        }
    }

    /// Get input port names
    pub fn get_input_names(&self) -> Vec<String> {
        match &self.backend {
            SimulatorBackend::Uninitialized => vec![],
            SimulatorBackend::CompiledCpu(_) => self.behavioral_input_names.clone(),
            #[cfg(target_os = "macos")]
            SimulatorBackend::BehavioralGpu(_) => self.behavioral_input_names.clone(),
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

    /// Set an input value as raw bytes (for behavioral simulation)
    ///
    /// This is useful for behavioral simulation where inputs may be wider than 64 bits.
    /// For gate-level simulation, the bytes are converted to u64 (truncated to 8 bytes).
    ///
    /// # Arguments
    /// * `name` - The input port name
    /// * `value` - The value as a byte slice (little-endian)
    ///
    /// # Example
    /// ```ignore
    /// // Set a 128-bit input
    /// let wide_value: [u8; 16] = [0x01, 0x02, ...];
    /// sim.set_input_bytes("wide_data", &wide_value).await;
    /// ```
    pub async fn set_input_bytes(&mut self, name: &str, value: &[u8]) {
        match &mut self.backend {
            SimulatorBackend::Uninitialized => {
                eprintln!("Warning: set_input_bytes called before loading design");
            }
            SimulatorBackend::CompiledCpu(runtime) => {
                let _ = runtime.set_input(name, value).await;
            }
            #[cfg(target_os = "macos")]
            SimulatorBackend::BehavioralGpu(runtime) => {
                let _ = runtime.set_input(name, value).await;
            }
            // For non-behavioral backends, convert bytes to u64 and use set_input
            _ => {
                // Convert bytes to u64 (little-endian, truncate to 8 bytes)
                let mut u64_value = 0u64;
                for (i, &byte) in value.iter().take(8).enumerate() {
                    u64_value |= (byte as u64) << (i * 8);
                }
                self.set_input(name, u64_value).await;
            }
        }
    }

    /// Get an output value as raw bytes (for behavioral simulation)
    ///
    /// This is useful for behavioral simulation where outputs may be wider than 64 bits.
    /// For gate-level simulation, the u64 value is converted to 8 bytes (little-endian).
    /// Accepts user-facing hierarchical paths which are resolved to internal names.
    ///
    /// # Arguments
    /// * `name` - The output port name (can be hierarchical path like "bms.connected")
    ///
    /// # Returns
    /// The output value as bytes (little-endian), or None if the output doesn't exist
    ///
    /// # Example
    /// ```ignore
    /// // Get a wide output
    /// if let Some(bytes) = sim.get_output_bytes("wide_result").await {
    ///     // bytes contains the raw output value
    /// }
    /// ```
    pub async fn get_output_bytes(&self, name: &str) -> Option<Vec<u8>> {
        // Resolve user-facing path to internal name
        let internal_name = self.resolve_path(name);

        match &self.backend {
            SimulatorBackend::Uninitialized => None,
            SimulatorBackend::CompiledCpu(runtime) => runtime.get_output(&internal_name).await.ok(),
            #[cfg(target_os = "macos")]
            SimulatorBackend::BehavioralGpu(runtime) => runtime.get_output(&internal_name).await.ok(),
            // For non-behavioral backends, get u64 and convert to bytes
            _ => self
                .get_output_raw(name)
                .await
                .map(|v| v.to_le_bytes().to_vec()),
        }
    }

    /// Get output port names
    pub fn get_output_names(&self) -> Vec<String> {
        match &self.backend {
            SimulatorBackend::Uninitialized => vec![],
            SimulatorBackend::CompiledCpu(_) => self.behavioral_output_names.clone(),
            #[cfg(target_os = "macos")]
            SimulatorBackend::BehavioralGpu(_) => self.behavioral_output_names.clone(),
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
