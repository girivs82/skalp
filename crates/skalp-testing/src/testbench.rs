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
use indexmap::IndexMap;
use skalp_lir::gate_netlist::GateNetlist;
use skalp_mir::MirCompiler;
use skalp_sim::{
    convert_gate_netlist_to_sir, CircuitMode, CoverageMetrics, CoverageReport, CoverageVectorGen,
    HwAccel, SimCoverageDb, SimLevel, UnifiedSimConfig, UnifiedSimResult, UnifiedSimulator,
};
use skalp_sir::{convert_mir_to_sir_with_hierarchy, SirModule};
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
    pending_inputs: IndexMap<String, u64>,
    cycle_count: u64,
    /// Available input port names
    input_names: Vec<String>,
    /// Available output port names
    output_names: Vec<String>,
    /// Retained SIR module for coverage update_nodes (behavioral mode)
    sir_module: Option<SirModule>,
    /// Coverage database (toggle + mux + comparison tracking)
    coverage_db: Option<SimCoverageDb>,
    /// Whether coverage tracking is enabled
    coverage_enabled: bool,
    /// Input info (name, width) for CoverageVectorGen
    coverage_input_info: Vec<(String, usize)>,
    /// Coverage sampling rate: sample every N cycles (default 1 = every cycle)
    /// Higher values allow GPU batched execution between samples
    coverage_sample_rate: usize,
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
        Self::gate_level_with_library(source_path, "generic_asic").await
    }

    /// Create a gate-level synchronous testbench with a specific technology library
    ///
    /// This allows testing designs synthesized with different technology libraries
    /// like "ice40", "fpga_lut4", "fpga_lut6", etc.
    ///
    /// # Example
    /// ```rust,ignore
    /// let mut tb = Testbench::gate_level_with_library("design.sk", "ice40").await.unwrap();
    /// ```
    pub async fn gate_level_with_library(source_path: &str, library_name: &str) -> Result<Self> {
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

        Self::from_source_gate_level_with_library(source_path, config, library_name, None).await
    }

    /// Create a gate-level synchronous testbench with explicit top module
    ///
    /// Same as `gate_level()` but allows specifying which module to use as top.
    pub async fn gate_level_with_top(source_path: &str, top_module: &str) -> Result<Self> {
        Self::gate_level_with_top_and_library(source_path, top_module, "generic_asic").await
    }

    /// Create a gate-level synchronous testbench with explicit top module and library
    pub async fn gate_level_with_top_and_library(
        source_path: &str,
        top_module: &str,
        library_name: &str,
    ) -> Result<Self> {
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

        Self::from_source_gate_level_with_library(
            source_path,
            config,
            library_name,
            Some(top_module),
        )
        .await
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
        let mut sim = UnifiedSimulator::new(config)
            .map_err(|e| anyhow::anyhow!("Failed to create NCL simulator: {}", e))?;

        sim.load_ncl_gate_level(netlist)
            .map_err(|e| anyhow::anyhow!("Failed to load NCL netlist: {}", e))?;

        Ok(Self {
            sim,
            mode: TestbenchMode::Ncl,
            pending_inputs: IndexMap::new(),
            cycle_count: 0,
            input_names: vec![],
            output_names: vec![],
            sir_module: None,
            coverage_db: None,
            coverage_enabled: false,
            coverage_input_info: vec![],
            coverage_sample_rate: 1,
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
    // Coverage-enabled constructors
    // ========================================================================

    /// Create a behavioral testbench with coverage tracking enabled
    ///
    /// Coverage tracking records toggle, mux arm, and comparison coverage
    /// per simulation cycle. The SIR module is retained for node-level
    /// coverage updates and name resolution.
    ///
    /// Note: coverage mode disables batched execution (each cycle steps
    /// individually to capture signal snapshots).
    pub async fn behavioral_with_coverage(source_path: &str) -> Result<Self> {
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

        Self::from_source_behavioral_with_coverage(source_path, config, None).await
    }

    /// Create a gate-level testbench with coverage tracking enabled
    ///
    /// Gate-level coverage tracks toggle coverage only (no mux/comparison
    /// nodes at the gate level).
    pub async fn gate_level_with_coverage(source_path: &str) -> Result<Self> {
        Self::gate_level_with_coverage_and_library(source_path, "generic_asic").await
    }

    /// Create a gate-level testbench with coverage and specific library
    pub async fn gate_level_with_coverage_and_library(
        source_path: &str,
        library_name: &str,
    ) -> Result<Self> {
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

        Self::from_source_gate_level_with_coverage(source_path, config, library_name, None).await
    }

    /// Internal: behavioral testbench with coverage
    async fn from_source_behavioral_with_coverage(
        source_path: &str,
        config: UnifiedSimConfig,
        top_module: Option<&str>,
    ) -> Result<Self> {
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

        // Build coverage database from SIR - only track user-visible signals
        // This gives meaningful coverage for SKALP source-level testing
        let coverage_db = SimCoverageDb::from_sir_module_user_visible(&sir);

        // Collect input info for coverage vector generation
        let coverage_input_info: Vec<(String, usize)> = sir
            .inputs
            .iter()
            .map(|p| (p.name.clone(), p.width))
            .collect();

        // Create simulator
        let mut sim = UnifiedSimulator::new(config)
            .map_err(|e| anyhow::anyhow!("Failed to create simulator: {}", e))?;

        sim.load_behavioral(&sir)
            .await
            .map_err(|e| anyhow::anyhow!("Failed to load behavioral design: {}", e))?;

        Ok(Self {
            sim,
            mode: TestbenchMode::Behavioral,
            pending_inputs: IndexMap::new(),
            cycle_count: 0,
            input_names,
            output_names,
            sir_module: Some(sir),
            coverage_db: Some(coverage_db),
            coverage_enabled: true,
            coverage_input_info,
            coverage_sample_rate: 10, // Sample every 10 cycles for GPU efficiency
        })
    }

    /// Internal: gate-level testbench with coverage
    async fn from_source_gate_level_with_coverage(
        source_path: &str,
        config: UnifiedSimConfig,
        library_name: &str,
        top_module_name: Option<&str>,
    ) -> Result<Self> {
        use skalp_frontend::parse_and_build_hir_from_file;
        use skalp_lir::{
            get_stdlib_library, lower_mir_hierarchical_with_top, lower_mir_module_to_lir,
            map_hierarchical_to_gates, map_lir_to_gates_optimized,
        };
        let path = Path::new(source_path);

        // Parse HIR
        let hir = parse_and_build_hir_from_file(path)?;

        // Compile to MIR
        let compiler = MirCompiler::new();
        let mir = compiler
            .compile_to_mir(&hir)
            .map_err(|e| anyhow::anyhow!("MIR compilation failed: {}", e))?;

        // Load technology library
        let library = get_stdlib_library(library_name).map_err(|e| {
            anyhow::anyhow!(
                "Failed to load technology library '{}': {:?}",
                library_name,
                e
            )
        })?;

        // Lower to GateNetlist
        let has_hierarchy =
            mir.modules.len() > 1 || mir.modules.iter().any(|m| !m.instances.is_empty());

        let netlist = if has_hierarchy || top_module_name.is_some() {
            let hier_lir = lower_mir_hierarchical_with_top(&mir, top_module_name);
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

        // Convert to SIR
        let mut sir_result = convert_gate_netlist_to_sir(&netlist);

        // Copy name_registry from behavioral SIR and retain behavioral SIR for name resolution
        let top_mir_module = if let Some(top_name) = top_module_name {
            mir.modules.iter().find(|m| m.name == top_name)
        } else {
            mir.modules.first()
        };
        let behavioral_sir =
            top_mir_module.map(|top_module| convert_mir_to_sir_with_hierarchy(&mir, top_module));
        if let Some(ref beh_sir) = behavioral_sir {
            sir_result.sir.name_registry = beh_sir.name_registry.clone();
        }

        // Extract gate signal info for coverage (toggle only)
        let gate_signal_info: Vec<(String, usize)> = sir_result
            .sir
            .top_module
            .signals
            .iter()
            .map(|s| (s.name.clone(), s.width))
            .collect();

        let coverage_db = SimCoverageDb::from_gate_netlist(&gate_signal_info);

        // Collect input info from behavioral SIR ports (gate SIR signals don't have port info)
        let coverage_input_info: Vec<(String, usize)> = behavioral_sir
            .as_ref()
            .map(|sir| {
                sir.inputs
                    .iter()
                    .map(|p| (p.name.clone(), p.width))
                    .collect()
            })
            .unwrap_or_default();

        let mut sim = UnifiedSimulator::new(config)
            .map_err(|e| anyhow::anyhow!("Failed to create simulator: {}", e))?;

        sim.load_gate_level(&sir_result.sir)
            .map_err(|e| anyhow::anyhow!("Failed to load design: {}", e))?;

        let input_names = sim.get_input_names();
        let output_names = sim.get_output_names();

        Ok(Self {
            sim,
            mode: TestbenchMode::GateLevel,
            pending_inputs: IndexMap::new(),
            cycle_count: 0,
            input_names,
            output_names,
            sir_module: behavioral_sir,
            coverage_db: Some(coverage_db),
            coverage_enabled: true,
            coverage_input_info,
            coverage_sample_rate: 10, // Sample every 10 cycles for GPU efficiency
        })
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
        let mut sim = UnifiedSimulator::new(config)
            .map_err(|e| anyhow::anyhow!("Failed to create simulator: {}", e))?;

        sim.load_behavioral(&sir)
            .await
            .map_err(|e| anyhow::anyhow!("Failed to load behavioral design: {}", e))?;

        Ok(Self {
            sim,
            mode: TestbenchMode::Behavioral,
            pending_inputs: IndexMap::new(),
            cycle_count: 0,
            input_names,
            output_names,
            sir_module: None,
            coverage_db: None,
            coverage_enabled: false,
            coverage_input_info: vec![],
            coverage_sample_rate: 1,
        })
    }

    #[allow(dead_code)]
    async fn from_source_gate_level(source_path: &str, config: UnifiedSimConfig) -> Result<Self> {
        Self::from_source_gate_level_with_library(source_path, config, "generic_asic", None).await
    }

    async fn from_source_gate_level_with_library(
        source_path: &str,
        config: UnifiedSimConfig,
        library_name: &str,
        top_module_name: Option<&str>,
    ) -> Result<Self> {
        use skalp_frontend::parse_and_build_hir_from_file;
        use skalp_lir::{
            get_stdlib_library, lower_mir_hierarchical_with_top, lower_mir_module_to_lir,
            map_hierarchical_to_gates, map_lir_to_gates_optimized,
        };

        let path = Path::new(source_path);

        // Parse HIR
        let hir = parse_and_build_hir_from_file(path)?;

        // Compile to MIR
        let compiler = MirCompiler::new();
        let mir = compiler
            .compile_to_mir(&hir)
            .map_err(|e| anyhow::anyhow!("MIR compilation failed: {}", e))?;

        // Load technology library
        let library = get_stdlib_library(library_name).map_err(|e| {
            anyhow::anyhow!(
                "Failed to load technology library '{}': {:?}",
                library_name,
                e
            )
        })?;

        // Lower to GateNetlist
        let has_hierarchy =
            mir.modules.len() > 1 || mir.modules.iter().any(|m| !m.instances.is_empty());

        let netlist = if has_hierarchy || top_module_name.is_some() {
            let hier_lir = lower_mir_hierarchical_with_top(&mir, top_module_name);
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

        // Convert to SIR and load
        let mut sir_result = convert_gate_netlist_to_sir(&netlist);

        // BUG #237 FIX: Also compile to behavioral SIR to get name_registry
        // The behavioral SIR has the correct mappings from user-facing names (e.g., "rst")
        // to internal signal names (e.g., "_s1")
        let top_mir_module = if let Some(top_name) = top_module_name {
            mir.modules.iter().find(|m| m.name == top_name)
        } else {
            mir.modules.first()
        };
        if let Some(top_module) = top_mir_module {
            let behavioral_sir = skalp_sir::convert_mir_to_sir_with_hierarchy(&mir, top_module);
            sir_result.sir.name_registry = behavioral_sir.name_registry.clone();
        }

        let mut sim = UnifiedSimulator::new(config)
            .map_err(|e| anyhow::anyhow!("Failed to create simulator: {}", e))?;

        sim.load_gate_level(&sir_result.sir)
            .map_err(|e| anyhow::anyhow!("Failed to load design: {}", e))?;

        // Extract port names before moving sim
        let input_names = sim.get_input_names();
        let output_names = sim.get_output_names();

        Ok(Self {
            sim,
            mode: TestbenchMode::GateLevel,
            pending_inputs: IndexMap::new(),
            cycle_count: 0,
            input_names,
            output_names,
            sir_module: None,
            coverage_db: None,
            coverage_enabled: false,
            coverage_input_info: vec![],
            coverage_sample_rate: 1,
        })
    }

    async fn from_source_ncl_with_top(
        source_path: &str,
        config: UnifiedSimConfig,
        top_module: Option<&str>,
    ) -> Result<Self> {
        use skalp_lir::{get_stdlib_library, lower_mir_hierarchical, map_hierarchical_to_gates};

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
        }

        // Load technology library and synthesize
        let library = get_stdlib_library("generic_asic")
            .map_err(|e| anyhow::anyhow!("Failed to load technology library: {:?}", e))?;

        let hier_lir = lower_mir_hierarchical(&mir);
        let hier_netlist = map_hierarchical_to_gates(&hier_lir, &library);
        let netlist = hier_netlist.flatten();

        // Load as NCL
        let mut sim = UnifiedSimulator::new(config)
            .map_err(|e| anyhow::anyhow!("Failed to create NCL simulator: {}", e))?;

        sim.load_ncl_gate_level(netlist)
            .map_err(|e| anyhow::anyhow!("Failed to load NCL netlist: {}", e))?;

        Ok(Self {
            sim,
            mode: TestbenchMode::Ncl,
            pending_inputs: IndexMap::new(),
            cycle_count: 0,
            input_names: vec![],
            output_names: vec![],
            sir_module: None,
            coverage_db: None,
            coverage_enabled: false,
            coverage_input_info: vec![],
            coverage_sample_rate: 1,
        })
    }

    /// Compile source to SIR (HIR → MIR → SIR)
    fn compile_to_sir_with_top(
        path: &Path,
        cache: &CompilationCache,
        cache_key: Option<&String>,
        explicit_top: Option<&str>,
    ) -> Result<skalp_sir::SirModule> {
        let context = skalp_frontend::parse_and_build_compilation_context(path)?;

        let compiler = MirCompiler::new();
        let mir = compiler
            .compile_to_mir_with_modules(&context.main_hir, &context.module_hirs)
            .map_err(|e| anyhow::anyhow!("{}", e))?;

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
            // BUG #230 + #232 FIX: When looking for a top module by name, also consider
            // specialized versions (e.g., "TriangularCarrier" should match "TriangularCarrier_1000").
            // Collect both exact matches and specialized versions.
            let prefix = format!("{}_", explicit_name);
            let candidates: Vec<_> = mir
                .modules
                .iter()
                .filter(|m| m.name == explicit_name || m.name.starts_with(&prefix))
                .collect();

            if candidates.is_empty() {
                anyhow::bail!("Top module '{}' not found", explicit_name);
            }

            // BUG #232: Prefer specialized versions (with suffix) over generic templates (exact match).
            // Specialized versions have concrete const values, while generic templates don't.
            // Among specialized versions, prefer the one with most signals.
            let specialized: Vec<_> = candidates
                .iter()
                .filter(|m| m.name.starts_with(&prefix))
                .collect();

            if !specialized.is_empty() {
                // Use the specialized version with most signals
                **specialized
                    .iter()
                    .max_by_key(|m| m.signals.len() + m.processes.len())
                    .unwrap()
            } else {
                // No specialized version, use the exact match with most signals
                *candidates
                    .iter()
                    .max_by_key(|m| m.signals.len() + m.processes.len())
                    .unwrap()
            }
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

        let sir = convert_mir_to_sir_with_hierarchy(&mir, top_module);

        if let Some(key) = cache_key {
            let _ = cache.store(key, &sir);
        }

        Ok(sir)
    }

    // ========================================================================
    // Coverage helpers
    // ========================================================================

    /// Step the simulation and update coverage tracking from the snapshot.
    ///
    /// Uses field-level borrows to avoid borrow checker conflicts:
    /// `self.sim`, `self.coverage_db`, and `self.sir_module` are accessed
    /// as separate fields.
    async fn step_with_coverage_update(&mut self) {
        let snapshot = self.sim.step_with_snapshot().await;
        if let (Some(state), Some(ref mut cov_db)) = (snapshot, &mut self.coverage_db) {
            // Update toggle coverage from both signals and registers
            // State elements are in registers (from on(clk.rise) blocks)
            cov_db.update_toggle(&state.signals);
            cov_db.update_toggle(&state.registers);
            if let Some(ref sir) = self.sir_module {
                cov_db.update_nodes(&state.signals, sir);
            }
        }
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
        for (signal, value) in self.pending_inputs.drain(..) {
            self.sim.set_input(&signal, value).await;
        }

        // When coverage is enabled, use batched execution with periodic sampling
        // This provides GPU efficiency while still tracking coverage
        if self.coverage_enabled {
            let sample_rate = self.coverage_sample_rate;
            let mut remaining = cycles;

            while remaining > 0 {
                // Run (sample_rate - 1) cycles in batched mode (no snapshot)
                let batch_cycles = (sample_rate - 1).min(remaining);
                if batch_cycles > 0 {
                    self.sim.set_input(clock_name, 1).await;
                    self.sim.run_batched(batch_cycles as u64).await;
                    self.cycle_count += batch_cycles as u64;
                    remaining -= batch_cycles;
                }

                // Then do one cycle with coverage snapshot
                if remaining > 0 {
                    self.sim.set_input(clock_name, 0).await;
                    self.step_with_coverage_update().await;
                    self.sim.set_input(clock_name, 1).await;
                    self.step_with_coverage_update().await;
                    if let Some(ref mut db) = self.coverage_db {
                        db.record_vector();
                    }
                    self.cycle_count += 1;
                    remaining -= 1;
                }
            }
            return self;
        }

        // PERF: Use batched GPU execution for cycle counts >= BATCH_MIN
        // This provides speedup by eliminating per-cycle CPU<->GPU sync
        // Large counts are chunked to avoid GPU kernel timeouts
        const BATCH_MIN: usize = 100;
        const BATCH_CHUNK: usize = 100_000; // Process in 100K cycle chunks

        if cycles >= BATCH_MIN {
            // Set clock high initially (batched kernel handles toggling internally)
            self.sim.set_input(clock_name, 1).await;

            // Process in chunks to avoid GPU timeouts while maintaining batch efficiency
            let mut remaining = cycles;
            while remaining > 0 {
                let chunk_size = remaining.min(BATCH_CHUNK);
                self.sim.run_batched(chunk_size as u64).await;
                self.cycle_count += chunk_size as u64;
                remaining -= chunk_size;
            }
        } else {
            // Run clock cycles step-by-step for small counts
            for _ in 0..cycles {
                self.sim.set_input(clock_name, 0).await;
                self.sim.step().await;
                self.sim.set_input(clock_name, 1).await;
                self.sim.step().await;
                self.cycle_count += 1;
            }
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
        for (signal, value) in self.pending_inputs.drain(..) {
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
            if self.coverage_enabled {
                self.step_with_coverage_update().await;
            } else {
                self.sim.step().await;
            }

            // Clock high phase
            for (clock_name, num_cycles) in clocks {
                if cycle < *num_cycles {
                    self.sim.set_input(clock_name, 1).await;
                }
            }
            if self.coverage_enabled {
                self.step_with_coverage_update().await;
                if let Some(ref mut db) = self.coverage_db {
                    db.record_vector();
                }
            } else {
                self.sim.step().await;
            }
            self.cycle_count += 1;
        }

        self
    }

    /// Apply pending inputs and step the simulation once
    pub async fn step(&mut self) -> &mut Self {
        for (signal, value) in self.pending_inputs.drain(..) {
            self.sim.set_input(&signal, value).await;
        }
        if self.coverage_enabled {
            self.step_with_coverage_update().await;
        } else {
            self.sim.step().await;
        }
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
    // Coverage query methods
    // ========================================================================

    /// Check if coverage tracking is enabled
    pub fn coverage_enabled(&self) -> bool {
        self.coverage_enabled
    }

    /// Set the coverage sampling rate (sample every N cycles).
    ///
    /// Higher values allow GPU batched execution between samples for better performance.
    /// Default is 10 for coverage-enabled testbenches.
    /// Use 1 for maximum coverage accuracy (samples every cycle, slower).
    ///
    /// # Example
    /// ```rust,ignore
    /// // Sample every 100 cycles for fast execution on long tests
    /// tb.set_coverage_sample_rate(100);
    /// ```
    pub fn set_coverage_sample_rate(&mut self, rate: usize) -> &mut Self {
        self.coverage_sample_rate = rate.max(1);
        self
    }

    /// Get the current coverage sampling rate
    pub fn coverage_sample_rate(&self) -> usize {
        self.coverage_sample_rate
    }

    /// Get current coverage metrics
    pub fn coverage_metrics(&self) -> Option<CoverageMetrics> {
        self.coverage_db.as_ref().map(|db| db.metrics())
    }

    /// Get a reference to the coverage database
    pub fn coverage_db(&self) -> Option<&SimCoverageDb> {
        self.coverage_db.as_ref()
    }

    /// Build a coverage report from the current coverage state
    pub fn coverage_report(&self) -> Option<CoverageReport> {
        self.coverage_db
            .as_ref()
            .map(|db| CoverageReport::from_coverage_dbs(db, None, true, self.cycle_count))
    }

    /// Print a basic coverage summary to stdout
    pub fn print_coverage_summary(&self) {
        if let Some(metrics) = self.coverage_metrics() {
            println!();
            println!("Coverage Summary (after {} cycles):", self.cycle_count);
            println!(
                "  Toggle:     {:6.1}%  ({}/{} bits)",
                metrics.toggle_pct, metrics.toggle_covered, metrics.toggle_total
            );
            println!(
                "  Mux arms:   {:6.1}%  ({}/{} arms)",
                metrics.mux_pct, metrics.mux_arms_covered, metrics.mux_arms_total
            );
            println!(
                "  Comparison: {:6.1}%  ({}/{} outcomes)",
                metrics.comparison_pct, metrics.cmp_covered, metrics.cmp_total
            );
            println!("  Overall:    {:6.1}%", metrics.overall_pct);
            println!("  Vectors:    {}", metrics.vectors_applied);
            println!();
        }
    }

    /// Print actionable coverage guidance with user-facing signal names.
    ///
    /// Uses `name_registry` to resolve internal signal names (e.g., `_s5`)
    /// to hierarchical user-facing paths (e.g., `controller.duty_cycle`).
    /// Groups uncovered items by signal and provides suggestions for
    /// closing coverage gaps.
    pub fn print_coverage_guidance(&self) {
        let cov_db = match &self.coverage_db {
            Some(db) => db,
            None => {
                println!("Coverage tracking is not enabled.");
                return;
            }
        };

        let metrics = cov_db.metrics();
        let name_registry = self.sir_module.as_ref().map(|sir| &sir.name_registry);

        println!();
        println!(
            "Coverage Guidance (after {} cycles, {:.1}% toggle coverage):",
            self.cycle_count, metrics.toggle_pct
        );
        println!("{}", "\u{2501}".repeat(60));
        println!();

        // --- Untoggled Signals ---
        let uncovered_toggles = cov_db.uncovered_toggles();
        if !uncovered_toggles.is_empty() {
            // Group by signal name (preserve insertion order via Vec)
            let mut by_signal: Vec<(String, Vec<(usize, &str)>)> = Vec::new();
            for (sig_name, bit, direction) in &uncovered_toggles {
                let display_name = Self::resolve_signal_name(sig_name, name_registry);
                if let Some(entry) = by_signal.iter_mut().find(|(n, _)| n == &display_name) {
                    entry.1.push((*bit, *direction));
                } else {
                    by_signal.push((display_name, vec![(*bit, *direction)]));
                }
            }

            println!("Untoggled Signals:");
            for (display_name, bits) in &by_signal {
                let total_width = bits.iter().map(|(b, _)| b + 1).max().unwrap_or(1);
                let never_toggled = bits.iter().filter(|(_, d)| *d == "neither").count();

                if never_toggled == bits.len() && bits.len() == total_width {
                    // Entire signal never toggled
                    println!("  {} ({}-bit): never toggled", display_name, total_width);
                    if total_width == 1 {
                        println!(
                            "    \u{2192} Set {} to both 0 and 1 in separate test phases",
                            display_name
                        );
                    } else {
                        println!(
                            "    \u{2192} Exercise {} with varying values (try 0x0, 0xFF, 0x55AA)",
                            display_name
                        );
                    }
                } else {
                    // Partial toggle coverage
                    let uncovered_bits: Vec<usize> = bits.iter().map(|(b, _)| *b).collect();
                    let bit_range = Self::format_bit_ranges(&uncovered_bits);
                    println!(
                        "  {} ({}-bit): {}/{} bits untoggled",
                        display_name,
                        total_width,
                        bits.len(),
                        total_width
                    );
                    println!(
                        "    \u{2192} bits {} need exercise with different values",
                        bit_range
                    );
                }
            }
            println!();
        }

        // --- Untaken Mux Arms ---
        let uncovered_mux = cov_db.uncovered_mux_arms();
        if !uncovered_mux.is_empty() {
            println!("Untaken Mux Arms:");
            for (node_name, arms) in &uncovered_mux {
                // Try to resolve the mux output signal name
                let display_name = if let Some(ref sir) = self.sir_module {
                    // Find the node and its output signal
                    sir.combinational_nodes
                        .iter()
                        .find(|n| format!("node_{}", n.id) == *node_name)
                        .and_then(|n| n.outputs.first())
                        .map(|out_ref| Self::resolve_signal_name(&out_ref.signal_id, name_registry))
                        .unwrap_or_else(|| node_name.to_string())
                } else {
                    node_name.to_string()
                };

                let total_arms = self
                    .sir_module
                    .as_ref()
                    .and_then(|sir| {
                        sir.combinational_nodes
                            .iter()
                            .find(|n| format!("node_{}", n.id) == *node_name)
                    })
                    .map(|n| match &n.kind {
                        skalp_sir::SirNodeKind::Mux => 2,
                        skalp_sir::SirNodeKind::ParallelMux { num_cases, .. } => *num_cases,
                        _ => 2,
                    })
                    .unwrap_or(2);

                for &arm in arms {
                    println!(
                        "  {} arm {} (of {}): not taken",
                        display_name, arm, total_arms
                    );
                    if total_arms == 2 {
                        let path_desc = if arm == 0 { "false/else" } else { "true/then" };
                        println!(
                            "    \u{2192} Exercise the '{}' path of {}",
                            path_desc, display_name
                        );
                    } else {
                        println!("    \u{2192} Drive selector to match case {}", arm);
                    }
                }
            }
            println!();
        }

        // --- Comparison Outcomes ---
        let uncovered_cmps = cov_db.uncovered_comparisons();
        if !uncovered_cmps.is_empty() {
            println!("Comparison Outcomes:");
            for (node_name, op, seen_true, seen_false) in &uncovered_cmps {
                let display_name = if let Some(ref sir) = self.sir_module {
                    sir.combinational_nodes
                        .iter()
                        .find(|n| format!("node_{}", n.id) == *node_name)
                        .and_then(|n| n.outputs.first())
                        .map(|out_ref| Self::resolve_signal_name(&out_ref.signal_id, name_registry))
                        .unwrap_or_else(|| node_name.to_string())
                } else {
                    node_name.to_string()
                };

                let missing = if !seen_true && !seen_false {
                    "both true and false"
                } else if !seen_true {
                    "true outcome"
                } else {
                    "false outcome"
                };
                println!("  {} ({}): missing {}", display_name, op, missing);
                println!(
                    "    \u{2192} Choose inputs that make this comparison {}",
                    if !*seen_true { "true" } else { "false" }
                );
            }
            println!();
        }

        if uncovered_toggles.is_empty() && uncovered_mux.is_empty() && uncovered_cmps.is_empty() {
            println!("  (all covered)");
            println!();
        }

        // Summary line
        let total_gaps = uncovered_toggles.len()
            + uncovered_mux.iter().map(|(_, a)| a.len()).sum::<usize>()
            + uncovered_cmps.len();
        if total_gaps > 0 {
            println!(
                "Summary: {} coverage gaps remaining ({} toggle bits, {} mux arms, {} comparisons)",
                total_gaps,
                uncovered_toggles.len(),
                uncovered_mux.iter().map(|(_, a)| a.len()).sum::<usize>(),
                uncovered_cmps.len()
            );
        } else {
            println!("Summary: all coverage goals met");
        }
        println!();
    }

    /// Resolve an internal signal name to a user-facing name via the name registry.
    fn resolve_signal_name(
        internal_name: &str,
        name_registry: Option<&skalp_mir::NameRegistry>,
    ) -> String {
        if let Some(registry) = name_registry {
            if let Some(path) = registry.reverse_resolve(internal_name) {
                return path.to_string();
            }
        }
        internal_name.to_string()
    }

    /// Auto-generate test vectors to close coverage gaps.
    ///
    /// Creates a `CoverageVectorGen` from the stored input info, then loops:
    /// generate vector → apply inputs → clock cycle with snapshots → check goal.
    ///
    /// Returns the final `CoverageMetrics` after the closure run.
    ///
    /// # Arguments
    /// * `clock_name` - Name of the clock signal to toggle
    /// * `goal` - Target coverage percentage (e.g., 90.0)
    /// * `max_vectors` - Maximum number of vectors to try before stopping
    pub async fn run_coverage_closure(
        &mut self,
        clock_name: &str,
        goal: f64,
        max_vectors: usize,
    ) -> Result<CoverageMetrics> {
        if !self.coverage_enabled {
            anyhow::bail!("Coverage tracking is not enabled. Use behavioral_with_coverage() or gate_level_with_coverage().");
        }

        let mut vecgen = CoverageVectorGen::from_input_info(&self.coverage_input_info, 42)
            .with_coverage_goal(goal)
            .with_lfsr_budget(max_vectors.min(1000))
            .with_bias_budget(max_vectors.saturating_sub(1000));

        let mut vectors_applied = 0usize;

        loop {
            // Check if we've reached the goal
            if let Some(ref db) = self.coverage_db {
                let m = db.metrics();
                if m.overall_pct >= goal {
                    return Ok(m);
                }
            }

            if vectors_applied >= max_vectors {
                break;
            }

            // Generate next vector
            let vector = {
                let cov_ref = self.coverage_db.as_ref();
                vecgen.next(cov_ref)
            };

            let vector = match vector {
                Some(v) => v,
                None => break,
            };

            // Apply the vector inputs
            for (name, value) in &vector.values {
                self.sim.set_input(name, *value).await;
            }

            // Run one clock cycle with coverage snapshots
            self.sim.set_input(clock_name, 0).await;
            self.step_with_coverage_update().await;
            self.sim.set_input(clock_name, 1).await;
            self.step_with_coverage_update().await;
            if let Some(ref mut db) = self.coverage_db {
                db.record_vector();
            }
            self.cycle_count += 1;
            vectors_applied += 1;
        }

        let metrics = self
            .coverage_db
            .as_ref()
            .map(|db| db.metrics())
            .unwrap_or(CoverageMetrics {
                toggle_pct: 0.0,
                toggle_covered: 0,
                toggle_total: 0,
                mux_pct: 0.0,
                mux_arms_covered: 0,
                mux_arms_total: 0,
                comparison_pct: 0.0,
                cmp_covered: 0,
                cmp_total: 0,
                overall_pct: 0.0,
                vectors_applied: 0,
            });

        Ok(metrics)
    }

    /// Format a list of bit indices into compact range notation (e.g., "[0..5], [8]")
    fn format_bit_ranges(bits: &[usize]) -> String {
        if bits.is_empty() {
            return String::new();
        }
        let mut sorted = bits.to_vec();
        sorted.sort_unstable();
        sorted.dedup();

        let mut ranges = Vec::new();
        let mut start = sorted[0];
        let mut end = sorted[0];

        for &b in &sorted[1..] {
            if b == end + 1 {
                end = b;
            } else {
                ranges.push((start, end));
                start = b;
                end = b;
            }
        }
        ranges.push((start, end));

        ranges
            .iter()
            .map(|(s, e)| {
                if s == e {
                    format!("[{}]", s)
                } else {
                    format!("[{}..{}]", s, e)
                }
            })
            .collect::<Vec<_>>()
            .join(", ")
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

    // ========================================================================
    // Waveform export
    // ========================================================================

    /// Export captured waveform data to an `.skw.gz` file.
    ///
    /// The testbench must have been created with `capture_waveforms: true`
    /// (which is the default for all constructors). Call this after running
    /// your test stimulus to save the waveform for viewing in the VS Code
    /// waveform viewer.
    ///
    /// # Example
    /// ```rust,ignore
    /// let mut tb = Testbench::behavioral("design.sk").await.unwrap();
    /// tb.reset(5).await;
    /// tb.set("input", 42u32);
    /// tb.clock(100).await;
    /// tb.export_waveform("build/test_output.skw.gz").unwrap();
    /// ```
    pub fn export_waveform(&self, path: impl AsRef<Path>) -> Result<()> {
        use skalp_sim::Waveform;

        let snapshots = self.sim.get_waveforms();
        if snapshots.is_empty() {
            anyhow::bail!("No waveform data captured. Ensure capture_waveforms is enabled.");
        }

        let signal_widths = self.sim.get_signal_widths();
        let mut waveform = Waveform::new();

        // Initialize signals from first snapshot with real widths
        if let Some(first) = snapshots.first() {
            for name in first.signals.keys() {
                let width = signal_widths.get(name).copied().unwrap_or(64);
                waveform.add_signal(name.clone(), width);
            }
        }

        // Add values from all snapshots
        for snapshot in snapshots {
            for (name, value) in &snapshot.signals {
                waveform.add_value(name, snapshot.cycle, value.to_le_bytes().to_vec());
            }
        }

        // Ensure parent directory exists
        let path = path.as_ref();
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent)?;
        }

        let design_name = path
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("testbench")
            .to_string();

        waveform
            .export_skw_compressed(path, &design_name)
            .map_err(|e| anyhow::anyhow!("Failed to write waveform: {}", e))?;

        eprintln!(
            "Waveform exported to {:?} ({} cycles, {} signals)",
            path,
            self.cycle_count,
            waveform.signals.len()
        );

        Ok(())
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

impl IntoSignalValue for i8 {
    fn into_bytes(self) -> Vec<u8> {
        self.to_le_bytes().to_vec()
    }
}

impl IntoSignalValue for i16 {
    fn into_bytes(self) -> Vec<u8> {
        self.to_le_bytes().to_vec()
    }
}

impl IntoSignalValue for i32 {
    fn into_bytes(self) -> Vec<u8> {
        self.to_le_bytes().to_vec()
    }
}

impl IntoSignalValue for i64 {
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

impl FromSignalValue for i8 {
    fn from_bytes(bytes: &[u8]) -> Self {
        bytes.first().map(|&b| b as i8).unwrap_or(0)
    }
}

impl FromSignalValue for i16 {
    fn from_bytes(bytes: &[u8]) -> Self {
        let mut arr = [0u8; 2];
        for (i, &b) in bytes.iter().take(2).enumerate() {
            arr[i] = b;
        }
        i16::from_le_bytes(arr)
    }
}

impl FromSignalValue for i32 {
    fn from_bytes(bytes: &[u8]) -> Self {
        let mut arr = [0u8; 4];
        for (i, &b) in bytes.iter().take(4).enumerate() {
            arr[i] = b;
        }
        i32::from_le_bytes(arr)
    }
}

impl FromSignalValue for i64 {
    fn from_bytes(bytes: &[u8]) -> Self {
        let mut arr = [0u8; 8];
        for (i, &b) in bytes.iter().take(8).enumerate() {
            arr[i] = b;
        }
        i64::from_le_bytes(arr)
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
