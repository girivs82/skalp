//! Ergonomic testbench API for hardware verification
//!
//! This module provides a high-level, declarative API for writing testbenches
//! that is much more concise than manually calling set_input/step/get_output.
//!
//! The `Testbench` struct uses `UnifiedSimulator` as its backend and supports:
//! - Behavioral simulation (fast functional verification)
//! - Gate-level synchronous simulation (accurate timing)
//! - NCL asynchronous simulation (dual-rail encoded circuits)
//!
//! # Example
//! ```rust,no_run
//! use skalp_testing::testbench::*;
//!
//! #[tokio::test]
//! async fn test_alu() {
//!     let mut tb = Testbench::new("examples/alu.sk").await.unwrap();
//!
//!     // Test ADD operation
//!     tb.set("a", 5u32).set("b", 3u32).set("op", 0b000u8);
//!     tb.clock(2).await;
//!     tb.expect("result", 8u32).await;
//!
//!     // Test SUB operation
//!     tb.set("op", 0b001u8);
//!     tb.clock(2).await;
//!     tb.expect("result", 2u32).await;
//! }
//! ```

use crate::cache::{collect_dependencies, CompilationCache};
use anyhow::Result;
use skalp_lir::gate_netlist::GateNetlist;
use skalp_mir::MirCompiler;
use skalp_sim::{
    convert_gate_netlist_to_sir, CircuitMode, HwAccel, SimLevel, UnifiedSimConfig,
    UnifiedSimResult, UnifiedSimulator,
};
use skalp_sir::convert_mir_to_sir_with_hierarchy;
use std::collections::HashMap;
use std::path::Path;

/// Simulation mode for the testbench
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TestbenchMode {
    /// Behavioral simulation - fast functional verification
    Behavioral,
    /// Gate-level synchronous simulation - accurate timing with clock
    GateLevel,
    /// NCL asynchronous simulation - dual-rail encoded, clockless
    Ncl,
}

/// High-level testbench builder for ergonomic verification
///
/// Uses `UnifiedSimulator` as the backend to support all simulation modes.
pub struct Testbench {
    sim: UnifiedSimulator,
    mode: TestbenchMode,
    pending_inputs: HashMap<String, u64>,
    cycle_count: u64,
    /// Available input port names
    input_names: Vec<String>,
    /// Available output port names
    output_names: Vec<String>,
}

impl Testbench {
    // ========================================================================
    // Constructors
    // ========================================================================

    /// Create a new behavioral testbench from a SKALP source file
    ///
    /// This is the default mode for fast functional verification.
    pub async fn new(source_path: &str) -> Result<Self> {
        Self::behavioral(source_path).await
    }

    /// Create a behavioral simulation testbench
    ///
    /// Behavioral simulation is the fastest mode, using high-level
    /// constructs without gate-level detail.
    pub async fn behavioral(source_path: &str) -> Result<Self> {
        let use_gpu = std::env::var("SKALP_SIM_MODE")
            .map(|v| v.to_lowercase() != "cpu")
            .unwrap_or(true);

        let config = UnifiedSimConfig {
            level: SimLevel::Behavioral,
            circuit_mode: CircuitMode::Sync,
            hw_accel: if use_gpu { HwAccel::Auto } else { HwAccel::Cpu },
            max_cycles: 1_000_000,
            capture_waveforms: true,
            ..Default::default()
        };

        Self::from_source_behavioral(source_path, config).await
    }

    /// Create a gate-level synchronous testbench
    ///
    /// Gate-level simulation provides accurate timing using primitive gates.
    pub async fn gate_level(source_path: &str) -> Result<Self> {
        let use_gpu = std::env::var("SKALP_SIM_MODE")
            .map(|v| v.to_lowercase() != "cpu")
            .unwrap_or(true);

        let config = UnifiedSimConfig {
            level: SimLevel::GateLevel,
            circuit_mode: CircuitMode::Sync,
            hw_accel: if use_gpu { HwAccel::Auto } else { HwAccel::Cpu },
            max_cycles: 1_000_000,
            capture_waveforms: true,
            ..Default::default()
        };

        Self::from_source_gate_level(source_path, config).await
    }

    /// Create an NCL (asynchronous) testbench from source
    ///
    /// NCL simulation uses dual-rail encoding and wavefront propagation
    /// instead of clocks.
    pub async fn ncl(source_path: &str) -> Result<Self> {
        let use_gpu = std::env::var("SKALP_SIM_MODE")
            .map(|v| v.to_lowercase() != "cpu")
            .unwrap_or(true);

        let config = UnifiedSimConfig {
            level: SimLevel::GateLevel,
            circuit_mode: CircuitMode::Ncl,
            hw_accel: if use_gpu { HwAccel::Auto } else { HwAccel::Cpu },
            max_iterations: 100000,
            ncl_debug: false,
            ..Default::default()
        };

        Self::from_source_ncl_with_top(source_path, config, None).await
    }

    /// Create an NCL testbench with explicit top module name
    ///
    /// Same as `ncl()` but allows specifying which module to use as top.
    pub async fn ncl_with_top_module(source_path: &str, top_module: &str) -> Result<Self> {
        let use_gpu = std::env::var("SKALP_SIM_MODE")
            .map(|v| v.to_lowercase() != "cpu")
            .unwrap_or(true);

        let config = UnifiedSimConfig {
            level: SimLevel::GateLevel,
            circuit_mode: CircuitMode::Ncl,
            hw_accel: if use_gpu { HwAccel::Auto } else { HwAccel::Cpu },
            max_iterations: 100000,
            ncl_debug: false,
            ..Default::default()
        };

        Self::from_source_ncl_with_top(source_path, config, Some(top_module)).await
    }

    /// Create an NCL testbench from a pre-compiled GateNetlist
    ///
    /// This is useful when you want to compile once and run multiple tests.
    ///
    /// # Example
    /// ```rust,no_run
    /// use skalp_testing::testbench::*;
    /// use skalp_lir::gate_netlist::GateNetlist;
    /// use std::sync::OnceLock;
    ///
    /// static NETLIST: OnceLock<GateNetlist> = OnceLock::new();
    ///
    /// #[tokio::test]
    /// async fn test_op1() {
    ///     let netlist = NETLIST.get_or_init(|| compile_design());
    ///     let mut tb = Testbench::from_netlist_ncl(netlist.clone()).unwrap();
    ///     // ... test code
    /// }
    /// ```
    pub fn from_netlist_ncl(netlist: GateNetlist) -> Result<Self> {
        let use_gpu = std::env::var("SKALP_SIM_MODE")
            .map(|v| v.to_lowercase() != "cpu")
            .unwrap_or(true);

        let config = UnifiedSimConfig {
            level: SimLevel::GateLevel,
            circuit_mode: CircuitMode::Ncl,
            hw_accel: if use_gpu { HwAccel::Auto } else { HwAccel::Cpu },
            max_iterations: 100000,
            ncl_debug: false,
            ..Default::default()
        };

        Self::from_netlist_ncl_with_config(netlist, config)
    }

    /// Create an NCL testbench from netlist with custom config
    pub fn from_netlist_ncl_with_config(
        netlist: GateNetlist,
        config: UnifiedSimConfig,
    ) -> Result<Self> {
        use std::time::Instant;
        let start = Instant::now();

        let mut sim = UnifiedSimulator::new(config)
            .map_err(|e| anyhow::anyhow!("Failed to create NCL simulator: {}", e))?;

        sim.load_ncl_gate_level(netlist)
            .map_err(|e| anyhow::anyhow!("Failed to load NCL netlist: {}", e))?;

        eprintln!(
            "⏱️  [TESTBENCH] NCL testbench created in {:?} (device: {})",
            start.elapsed(),
            sim.device_info()
        );

        Ok(Self {
            sim,
            mode: TestbenchMode::Ncl,
            pending_inputs: HashMap::new(),
            cycle_count: 0,
            input_names: vec![],
            output_names: vec![],
        })
    }

    /// Create a testbench with explicit top module name
    pub async fn with_top_module(source_path: &str, top_module: &str) -> Result<Self> {
        Self::behavioral_with_top(source_path, top_module).await
    }

    /// Create a behavioral testbench with explicit top module
    pub async fn behavioral_with_top(source_path: &str, top_module: &str) -> Result<Self> {
        let use_gpu = std::env::var("SKALP_SIM_MODE")
            .map(|v| v.to_lowercase() != "cpu")
            .unwrap_or(true);

        let config = UnifiedSimConfig {
            level: SimLevel::Behavioral,
            circuit_mode: CircuitMode::Sync,
            hw_accel: if use_gpu { HwAccel::Auto } else { HwAccel::Cpu },
            max_cycles: 1_000_000,
            capture_waveforms: true,
            ..Default::default()
        };

        Self::from_source_behavioral_with_top(source_path, config, Some(top_module)).await
    }

    // ========================================================================
    // Internal constructors
    // ========================================================================

    async fn from_source_behavioral(source_path: &str, config: UnifiedSimConfig) -> Result<Self> {
        Self::from_source_behavioral_with_top(source_path, config, None).await
    }

    async fn from_source_behavioral_with_top(
        source_path: &str,
        config: UnifiedSimConfig,
        top_module: Option<&str>,
    ) -> Result<Self> {
        use std::time::Instant;
        let start_total = Instant::now();
        eprintln!(
            "⏱️  [TESTBENCH] Starting behavioral compilation of '{}'",
            source_path
        );

        let path = Path::new(source_path);

        // Initialize cache
        let cache = CompilationCache::new();
        let dependencies = collect_dependencies(path).unwrap_or_default();
        let cache_key = cache.compute_cache_key(path, &dependencies).ok();

        // Compile to SIR
        let sir = if top_module.is_some() {
            Self::compile_to_sir_with_top(path, &cache, cache_key.as_ref(), top_module)?
        } else if let Some(ref key) = cache_key {
            if let Ok(Some(cached_sir)) = cache.load(key) {
                eprintln!("⏱️  [TESTBENCH] Using cached SIR");
                cached_sir
            } else {
                Self::compile_to_sir_with_top(path, &cache, Some(key), None)?
            }
        } else {
            Self::compile_to_sir_with_top(path, &cache, None, None)?
        };

        // Extract port names
        let input_names: Vec<String> = sir.inputs.iter().map(|i| i.name.clone()).collect();
        let output_names: Vec<String> = sir.outputs.iter().map(|o| o.name.clone()).collect();

        // Create simulator
        let start_sim = Instant::now();
        let mut sim = UnifiedSimulator::new(config)
            .map_err(|e| anyhow::anyhow!("Failed to create simulator: {}", e))?;

        sim.load_behavioral(&sir)
            .await
            .map_err(|e| anyhow::anyhow!("Failed to load behavioral design: {}", e))?;

        eprintln!(
            "⏱️  [TESTBENCH] Simulator initialized in {:?} (device: {})",
            start_sim.elapsed(),
            sim.device_info()
        );
        eprintln!(
            "⏱️  [TESTBENCH] ✅ Total creation time: {:?}",
            start_total.elapsed()
        );

        Ok(Self {
            sim,
            mode: TestbenchMode::Behavioral,
            pending_inputs: HashMap::new(),
            cycle_count: 0,
            input_names,
            output_names,
        })
    }

    async fn from_source_gate_level(source_path: &str, config: UnifiedSimConfig) -> Result<Self> {
        use skalp_frontend::parse_and_build_hir_from_file;
        use skalp_lir::{
            get_stdlib_library, lower_mir_hierarchical, lower_mir_module_to_lir,
            map_hierarchical_to_gates, map_lir_to_gates_optimized,
        };
        use std::time::Instant;

        let start_total = Instant::now();
        eprintln!(
            "⏱️  [TESTBENCH] Starting gate-level compilation of '{}'",
            source_path
        );

        let path = Path::new(source_path);

        // Parse HIR
        let hir = parse_and_build_hir_from_file(path)?;

        // Compile to MIR
        let compiler = MirCompiler::new();
        let mir = compiler
            .compile_to_mir(&hir)
            .map_err(|e| anyhow::anyhow!("MIR compilation failed: {}", e))?;

        // Load technology library
        let library = get_stdlib_library("generic_asic")
            .map_err(|e| anyhow::anyhow!("Failed to load technology library: {:?}", e))?;

        // Lower to GateNetlist
        let has_hierarchy =
            mir.modules.len() > 1 || mir.modules.iter().any(|m| !m.instances.is_empty());

        let netlist = if has_hierarchy {
            let hier_lir = lower_mir_hierarchical(&mir);
            let hier_netlist = map_hierarchical_to_gates(&hier_lir, &library);
            hier_netlist.flatten()
        } else {
            let top_module = mir
                .modules
                .first()
                .ok_or_else(|| anyhow::anyhow!("No modules found"))?;
            let lir_result = lower_mir_module_to_lir(top_module);
            let tech_result = map_lir_to_gates_optimized(&lir_result.lir, &library);
            tech_result.netlist
        };

        eprintln!(
            "⏱️  [TESTBENCH] Gate netlist: {} cells, {} nets",
            netlist.cells.len(),
            netlist.nets.len()
        );

        // Convert to SIR and load
        let sir_result = convert_gate_netlist_to_sir(&netlist);

        let mut sim = UnifiedSimulator::new(config)
            .map_err(|e| anyhow::anyhow!("Failed to create simulator: {}", e))?;

        sim.load_gate_level(&sir_result.sir)
            .map_err(|e| anyhow::anyhow!("Failed to load design: {}", e))?;

        // Extract port names before moving sim
        let input_names = sim.get_input_names();
        let output_names = sim.get_output_names();

        eprintln!(
            "⏱️  [TESTBENCH] ✅ Gate-level testbench created in {:?}",
            start_total.elapsed()
        );

        Ok(Self {
            sim,
            mode: TestbenchMode::GateLevel,
            pending_inputs: HashMap::new(),
            cycle_count: 0,
            input_names,
            output_names,
        })
    }

    async fn from_source_ncl_with_top(
        source_path: &str,
        config: UnifiedSimConfig,
        top_module: Option<&str>,
    ) -> Result<Self> {
        use skalp_lir::{get_stdlib_library, lower_mir_hierarchical, map_hierarchical_to_gates};
        use std::time::Instant;

        let start_total = Instant::now();
        eprintln!(
            "⏱️  [TESTBENCH] Starting NCL compilation of '{}' (top: {:?})",
            source_path, top_module
        );

        let path = Path::new(source_path);

        // Parse with compilation context (supports module resolution)
        let context = skalp_frontend::parse_and_build_compilation_context(path)?;

        // Compile to MIR with modules
        let compiler = MirCompiler::new();
        let mir = compiler
            .compile_to_mir_with_modules(&context.main_hir, &context.module_hirs)
            .map_err(|e| anyhow::anyhow!("MIR compilation failed: {}", e))?;

        // If explicit top module specified, verify it exists
        if let Some(top_name) = top_module {
            if !mir.modules.iter().any(|m| m.name == top_name) {
                anyhow::bail!(
                    "Top module '{}' not found. Available modules: {:?}",
                    top_name,
                    mir.modules.iter().map(|m| &m.name).collect::<Vec<_>>()
                );
            }
            eprintln!("⏱️  [TESTBENCH] Using explicit top module: {}", top_name);
        }

        // Load technology library and synthesize
        let library = get_stdlib_library("generic_asic")
            .map_err(|e| anyhow::anyhow!("Failed to load technology library: {:?}", e))?;

        let hier_lir = lower_mir_hierarchical(&mir);
        let hier_netlist = map_hierarchical_to_gates(&hier_lir, &library);
        let netlist = hier_netlist.flatten();

        eprintln!(
            "⏱️  [TESTBENCH] NCL netlist: {} cells, {} nets",
            netlist.cells.len(),
            netlist.nets.len()
        );

        // Load as NCL
        let mut sim = UnifiedSimulator::new(config)
            .map_err(|e| anyhow::anyhow!("Failed to create NCL simulator: {}", e))?;

        sim.load_ncl_gate_level(netlist)
            .map_err(|e| anyhow::anyhow!("Failed to load NCL netlist: {}", e))?;

        eprintln!(
            "⏱️  [TESTBENCH] ✅ NCL testbench created in {:?}",
            start_total.elapsed()
        );

        Ok(Self {
            sim,
            mode: TestbenchMode::Ncl,
            pending_inputs: HashMap::new(),
            cycle_count: 0,
            input_names: vec![],
            output_names: vec![],
        })
    }

    /// Compile source to SIR (HIR → MIR → SIR)
    fn compile_to_sir_with_top(
        path: &Path,
        cache: &CompilationCache,
        cache_key: Option<&String>,
        explicit_top: Option<&str>,
    ) -> Result<skalp_sir::SirModule> {
        use std::time::Instant;

        let start_hir = Instant::now();
        let context = skalp_frontend::parse_and_build_compilation_context(path)?;
        eprintln!(
            "⏱️  [TESTBENCH] HIR parsing completed in {:?}",
            start_hir.elapsed()
        );

        let start_mir = Instant::now();
        let compiler = MirCompiler::new();
        let mir = compiler
            .compile_to_mir_with_modules(&context.main_hir, &context.module_hirs)
            .map_err(|e| anyhow::anyhow!("{}", e))?;
        eprintln!(
            "⏱️  [TESTBENCH] MIR compilation completed in {:?}",
            start_mir.elapsed()
        );

        let start_sir = Instant::now();
        if mir.modules.is_empty() {
            anyhow::bail!("No modules found in design");
        }

        // Find top module
        let mut instantiated_modules = std::collections::HashSet::new();
        for module in &mir.modules {
            for instance in &module.instances {
                instantiated_modules.insert(instance.module);
            }
        }

        let uninstantiated: Vec<_> = mir
            .modules
            .iter()
            .filter(|m| !instantiated_modules.contains(&m.id))
            .collect();

        if uninstantiated.is_empty() {
            anyhow::bail!("Could not find top-level module");
        }

        let source_basename = path
            .file_stem()
            .and_then(|s| s.to_str())
            .map(|s| s.to_string());

        let top_module = if let Some(explicit_name) = explicit_top {
            mir.modules
                .iter()
                .find(|m| m.name == explicit_name)
                .ok_or_else(|| anyhow::anyhow!("Top module '{}' not found", explicit_name))?
        } else if let Some(ref basename) = source_basename {
            let pascal_basename: String = basename
                .split('_')
                .map(|s| {
                    let mut chars: Vec<char> = s.chars().collect();
                    if !chars.is_empty() {
                        chars[0] = chars[0].to_ascii_uppercase();
                    }
                    chars.into_iter().collect::<String>()
                })
                .collect();

            if let Some(m) = mir.modules.iter().find(|m| m.name == pascal_basename) {
                m
            } else {
                *uninstantiated
                    .iter()
                    .max_by_key(|m| m.instances.len())
                    .unwrap()
            }
        } else {
            *uninstantiated
                .iter()
                .max_by_key(|m| m.instances.len())
                .unwrap()
        };

        eprintln!("✅ Selected top module: '{}'", top_module.name);

        let sir = convert_mir_to_sir_with_hierarchy(&mir, top_module);
        eprintln!(
            "⏱️  [TESTBENCH] SIR conversion completed in {:?}",
            start_sir.elapsed()
        );

        if let Some(key) = cache_key {
            let _ = cache.store(key, &sir);
        }

        Ok(sir)
    }

    // ========================================================================
    // Common methods (work for all modes)
    // ========================================================================

    /// Set an input signal value (chainable)
    pub fn set(&mut self, signal: &str, value: impl IntoSignalValue) -> &mut Self {
        let bytes = value.into_bytes();
        let val = bytes_to_u64(&bytes);
        self.pending_inputs.insert(signal.to_string(), val);
        self
    }

    /// Apply pending inputs and run for N clock cycles
    pub async fn clock(&mut self, cycles: usize) -> &mut Self {
        self.clock_signal("clk", cycles).await
    }

    /// Apply pending inputs and run for N cycles on a specific clock signal
    pub async fn clock_signal(&mut self, clock_name: &str, cycles: usize) -> &mut Self {
        // Apply all pending inputs
        for (signal, value) in self.pending_inputs.drain() {
            self.sim.set_input(&signal, value).await;
        }

        // Run clock cycles
        for _ in 0..cycles {
            self.sim.set_input(clock_name, 0).await;
            self.sim.step().await;
            self.sim.set_input(clock_name, 1).await;
            self.sim.step().await;
            self.cycle_count += 1;
        }

        self
    }

    /// Apply pending inputs and run multiple clock signals for different cycle counts
    ///
    /// This is useful for multi-clock domain designs where different clocks
    /// need to run for different numbers of cycles.
    ///
    /// # Example
    /// ```rust,ignore
    /// // Run wr_clk for 2 cycles and rd_clk for 4 cycles
    /// tb.clock_multi(&[("wr_clk", 2), ("rd_clk", 4)]).await;
    /// ```
    pub async fn clock_multi(&mut self, clocks: &[(&str, usize)]) -> &mut Self {
        // Apply all pending inputs
        for (signal, value) in self.pending_inputs.drain() {
            self.sim.set_input(&signal, value).await;
        }

        // Find the maximum number of cycles needed
        let max_cycles = clocks.iter().map(|(_, c)| *c).max().unwrap_or(0);

        // Run cycles, toggling each clock appropriately
        for cycle in 0..max_cycles {
            // Clock low phase
            for (clock_name, num_cycles) in clocks {
                if cycle < *num_cycles {
                    self.sim.set_input(clock_name, 0).await;
                }
            }
            self.sim.step().await;

            // Clock high phase
            for (clock_name, num_cycles) in clocks {
                if cycle < *num_cycles {
                    self.sim.set_input(clock_name, 1).await;
                }
            }
            self.sim.step().await;
            self.cycle_count += 1;
        }

        self
    }

    /// Apply pending inputs and step the simulation once
    pub async fn step(&mut self) -> &mut Self {
        for (signal, value) in self.pending_inputs.drain() {
            self.sim.set_input(&signal, value).await;
        }
        self.sim.step().await;
        self
    }

    /// Get an output value as u32
    pub async fn get_u32(&mut self, signal: &str) -> u32 {
        if !self.pending_inputs.is_empty() {
            self.step().await;
        }

        if let Some(val) = self.sim.get_output(signal).await {
            return val as u32;
        }

        self.get_bitblasted_value(signal, 32).await as u32
    }

    /// Get an output value as u64
    pub async fn get_u64(&mut self, signal: &str) -> u64 {
        if !self.pending_inputs.is_empty() {
            self.step().await;
        }

        if let Some(val) = self.sim.get_output(signal).await {
            return val;
        }

        self.get_bitblasted_value(signal, 64).await
    }

    /// Get an output value as a specific type
    ///
    /// This is a convenience method that converts the output value to the requested type.
    /// For behavioral simulation, it uses byte-based I/O for larger types.
    ///
    /// # Example
    /// ```rust,ignore
    /// let result: u32 = tb.get_as("output").await;
    /// let data: Vec<u8> = tb.get_as("large_output").await;
    /// ```
    pub async fn get_as<T: FromSignalValue>(&mut self, signal: &str) -> T {
        if !self.pending_inputs.is_empty() {
            self.step().await;
        }

        // Try byte-based output first (works for behavioral with large types)
        if let Some(bytes) = self.sim.get_output_bytes(signal).await {
            return T::from_bytes(&bytes);
        }

        // Fall back to u64-based output
        if let Some(val) = self.sim.get_output(signal).await {
            let bytes = val.to_le_bytes().to_vec();
            return T::from_bytes(&bytes);
        }

        // Try bit-blasted value
        let val = self.get_bitblasted_value(signal, 64).await;
        let bytes = val.to_le_bytes().to_vec();
        T::from_bytes(&bytes)
    }

    /// Get a bit-blasted signal value
    async fn get_bitblasted_value(&self, signal: &str, max_bits: usize) -> u64 {
        let mut result = 0u64;

        for bit in 0..max_bits {
            let bit_signal = format!("{}[{}]", signal, bit);
            if let Some(val) = self.sim.get_output(&bit_signal).await {
                if val != 0 {
                    result |= 1u64 << bit;
                }
            } else if bit > 0 {
                break;
            }
        }

        result
    }

    /// Expect an output signal to have a specific value
    pub async fn expect(&mut self, signal: &str, expected: impl IntoSignalValue) -> &mut Self {
        let expected_bytes = expected.into_bytes();
        let expected_u64 = bytes_to_u64(&expected_bytes);
        let actual = self.sim.get_output(signal).await.unwrap_or(0);

        assert_eq!(
            actual, expected_u64,
            "Signal '{}' mismatch at cycle {}: expected 0x{:x}, got 0x{:x}",
            signal, self.cycle_count, expected_u64, actual
        );

        self
    }

    /// Expect an fp32 output with tolerance
    pub async fn expect_fp32(&mut self, signal: &str, expected: f32, tolerance: f32) -> &mut Self {
        let actual_bits = self.sim.get_output(signal).await.unwrap_or(0) as u32;
        let actual = f32::from_bits(actual_bits);
        let diff = (actual - expected).abs();

        assert!(
            diff <= tolerance,
            "Signal '{}' fp32 mismatch at cycle {}: expected {}, got {} (diff: {}, tolerance: {})",
            signal,
            self.cycle_count,
            expected,
            actual,
            diff,
            tolerance
        );

        self
    }

    /// Apply reset for N cycles
    pub async fn reset(&mut self, cycles: usize) -> &mut Self {
        self.set("rst", 1u8);
        self.clock(cycles).await;
        self.set("rst", 0u8);
        self.clock(1).await;
        self
    }

    /// Get the current cycle count
    pub fn cycles(&self) -> u64 {
        self.cycle_count
    }

    /// Get device info (CPU or GPU)
    pub fn device_info(&self) -> String {
        self.sim.device_info()
    }

    /// Get the simulation mode
    pub fn mode(&self) -> TestbenchMode {
        self.mode
    }

    /// Get input port names
    pub fn get_input_names(&self) -> Vec<String> {
        if self.input_names.is_empty() {
            self.sim.get_input_names()
        } else {
            self.input_names.clone()
        }
    }

    /// Get output port names
    pub fn get_output_names(&self) -> Vec<String> {
        if self.output_names.is_empty() {
            self.sim.get_output_names()
        } else {
            self.output_names.clone()
        }
    }

    // ========================================================================
    // NCL-specific methods
    // ========================================================================

    /// Set an NCL dual-rail input with explicit width
    ///
    /// For NCL circuits, inputs are encoded as dual-rail signals.
    pub fn set_ncl_input(&mut self, name: &str, value: u64, width: usize) -> &mut Self {
        self.sim.set_ncl_input(name, value, width);
        self
    }

    /// Set NCL input from u128 (for signals up to 128 bits)
    pub fn set_ncl_input_u128(&mut self, name: &str, value: u128, width: usize) -> &mut Self {
        self.sim.set_ncl_input_u128(name, value, width);
        self
    }

    /// Set all NCL inputs to NULL (spacer phase)
    pub fn set_ncl_null(&mut self, name: &str, width: usize) -> &mut Self {
        self.sim.set_ncl_null(name, width);
        self
    }

    /// Run NCL simulation until stable (no signal changes)
    pub async fn run_until_stable(&mut self) -> UnifiedSimResult {
        self.sim.run_until_stable().await
    }

    /// Get an NCL dual-rail output value
    pub fn get_ncl_output(&self, name: &str, width: usize) -> Option<u64> {
        self.sim.get_ncl_output(name, width)
    }

    /// Check if NCL simulation is complete
    pub fn is_ncl_complete(&self) -> bool {
        self.sim.is_ncl_complete()
    }

    /// Check if NCL outputs are all NULL
    pub fn is_ncl_null(&self) -> bool {
        self.sim.is_ncl_null()
    }

    /// Get NCL signal names (for debugging)
    pub fn ncl_signal_names(&self) -> Vec<String> {
        self.sim.ncl_signal_names()
    }

    /// Check if an NCL signal exists
    pub fn has_ncl_signal(&self, name: &str) -> bool {
        self.sim.has_ncl_signal(name)
    }

    /// Reset NCL simulation state
    pub fn reset_ncl(&mut self) {
        self.sim.reset();
    }
}

// ============================================================================
// Signal value conversion traits
// ============================================================================

/// Trait for converting Rust types to signal byte values
pub trait IntoSignalValue {
    fn into_bytes(self) -> Vec<u8>;
}

impl IntoSignalValue for u8 {
    fn into_bytes(self) -> Vec<u8> {
        vec![self]
    }
}

impl IntoSignalValue for u16 {
    fn into_bytes(self) -> Vec<u8> {
        self.to_le_bytes().to_vec()
    }
}

impl IntoSignalValue for u32 {
    fn into_bytes(self) -> Vec<u8> {
        self.to_le_bytes().to_vec()
    }
}

impl IntoSignalValue for u64 {
    fn into_bytes(self) -> Vec<u8> {
        self.to_le_bytes().to_vec()
    }
}

impl IntoSignalValue for Vec<u8> {
    fn into_bytes(self) -> Vec<u8> {
        self
    }
}

impl IntoSignalValue for &[u8] {
    fn into_bytes(self) -> Vec<u8> {
        self.to_vec()
    }
}

impl IntoSignalValue for f32 {
    fn into_bytes(self) -> Vec<u8> {
        self.to_le_bytes().to_vec()
    }
}

impl IntoSignalValue for f64 {
    fn into_bytes(self) -> Vec<u8> {
        self.to_le_bytes().to_vec()
    }
}

/// Trait for converting signal byte values to Rust types
pub trait FromSignalValue {
    fn from_bytes(bytes: &[u8]) -> Self;
}

impl FromSignalValue for u8 {
    fn from_bytes(bytes: &[u8]) -> Self {
        bytes.first().copied().unwrap_or(0)
    }
}

impl FromSignalValue for u16 {
    fn from_bytes(bytes: &[u8]) -> Self {
        let mut arr = [0u8; 2];
        for (i, &b) in bytes.iter().take(2).enumerate() {
            arr[i] = b;
        }
        u16::from_le_bytes(arr)
    }
}

impl FromSignalValue for u32 {
    fn from_bytes(bytes: &[u8]) -> Self {
        let mut arr = [0u8; 4];
        for (i, &b) in bytes.iter().take(4).enumerate() {
            arr[i] = b;
        }
        u32::from_le_bytes(arr)
    }
}

impl FromSignalValue for u64 {
    fn from_bytes(bytes: &[u8]) -> Self {
        let mut arr = [0u8; 8];
        for (i, &b) in bytes.iter().take(8).enumerate() {
            arr[i] = b;
        }
        u64::from_le_bytes(arr)
    }
}

impl FromSignalValue for Vec<u8> {
    fn from_bytes(bytes: &[u8]) -> Self {
        bytes.to_vec()
    }
}

impl FromSignalValue for f32 {
    fn from_bytes(bytes: &[u8]) -> Self {
        let mut arr = [0u8; 4];
        for (i, &b) in bytes.iter().take(4).enumerate() {
            arr[i] = b;
        }
        f32::from_le_bytes(arr)
    }
}

impl FromSignalValue for f64 {
    fn from_bytes(bytes: &[u8]) -> Self {
        let mut arr = [0u8; 8];
        for (i, &b) in bytes.iter().take(8).enumerate() {
            arr[i] = b;
        }
        f64::from_le_bytes(arr)
    }
}

// ============================================================================
// Helper functions
// ============================================================================

/// Convert bytes to u64 (little-endian)
fn bytes_to_u64(bytes: &[u8]) -> u64 {
    let mut result = 0u64;
    for (i, &byte) in bytes.iter().take(8).enumerate() {
        result |= (byte as u64) << (i * 8);
    }
    result
}

// ============================================================================
// Deprecated: GateLevelTestbench (use Testbench::gate_level() instead)
// ============================================================================

/// **DEPRECATED**: Use `Testbench::gate_level()` or `Testbench::ncl()` instead.
///
/// This type alias is kept for backwards compatibility.
#[deprecated(
    since = "0.2.0",
    note = "Use Testbench::gate_level() or Testbench::ncl() instead"
)]
pub type GateLevelTestbench = Testbench;
