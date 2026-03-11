//! Synthesis Engine
//!
//! This module provides the main API for running logic synthesis optimization
//! on gate-level netlists. It orchestrates the various optimization passes
//! and provides presets for common use cases.
//!
//! # Example
//!
//! ```ignore
//! use skalp_lir::synth::{SynthEngine, SynthPreset};
//!
//! let mut engine = SynthEngine::with_preset(SynthPreset::Balanced);
//! let optimized = engine.optimize(&gate_netlist, &tech_library);
//! ```

use super::datapath::{AdderArchitecture, AdderConfig, AdderOptimizer, DatapathConfig};
use super::liberty::LibertyLibrary;
use super::mapping::{
    CutMapper, CutMapperConfig, DelayMapper, DelayMappingConfig, MappingObjective, MappingResult,
};
use super::partition::NetlistPartition;
use super::passes::{
    Balance, BufferConfig, BufferInsertion, ConstProp, Dc2, Dce, Dchoice, Fraig, FraigConfig, Pass,
    PassResult, Refactor, Resub, Retiming, RetimingConfig, Rewrite, Scorr, Strash,
};
use super::sta::{Sta, StaResult};
use super::timing::{TimePs, TimingConstraints};
use super::{Aig, AigBuilder, AigWriter};
use crate::gate_netlist::{Cell, CellId, GateNet, GateNetId, GateNetlist};
use crate::pipeline_annotations::{ModuleAnnotations, PipelineAnnotations};
use crate::tech_library::{CellFunction, TechLibrary};
use indexmap::IndexMap;
use rayon::prelude::*;
use std::collections::{HashMap, HashSet};
use std::time::Instant;

/// Synthesis preset configurations
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum SynthPreset {
    /// Quick optimization - minimal passes for fast turnaround
    Quick,
    /// Balanced optimization - good trade-off between quality and runtime
    #[default]
    Balanced,
    /// Full optimization - maximum effort for best QoR
    Full,
    /// Timing-focused - prioritize meeting timing constraints
    Timing,
    /// Area-focused - prioritize minimizing gate count
    Area,
    /// Resyn2 - ABC's proven pass sequence for high-quality optimization
    /// balance; rewrite; refactor; balance; rewrite; rewrite -z; balance; refactor -z; rewrite -z; balance
    Resyn2,
    /// Compress2 - ABC's area-focused script with resubstitution
    /// balance; resub; rewrite; resub; refactor; resub; balance; ...
    Compress2,
    /// Auto - run multiple presets in parallel and pick the best result
    Auto,
}

/// Synthesis configuration
#[derive(Debug, Clone)]
pub struct SynthConfig {
    /// Optimization preset
    pub preset: SynthPreset,
    /// Target clock period (ps) - for timing-driven optimization
    pub target_period: Option<TimePs>,
    /// Maximum number of optimization iterations
    pub max_iterations: usize,
    /// Enable verbose logging
    pub verbose: bool,
    /// Run timing analysis after optimization
    pub run_timing_analysis: bool,
    /// Custom pass sequence (if None, uses preset)
    pub custom_passes: Option<Vec<String>>,
}

impl Default for SynthConfig {
    fn default() -> Self {
        Self {
            preset: SynthPreset::default(),
            target_period: None,
            max_iterations: 3,
            verbose: false,
            run_timing_analysis: false,
            custom_passes: None,
        }
    }
}

impl SynthConfig {
    /// Create config for quick optimization
    pub fn quick() -> Self {
        Self {
            preset: SynthPreset::Quick,
            max_iterations: 1,
            ..Default::default()
        }
    }

    /// Create config for full optimization
    pub fn full() -> Self {
        Self {
            preset: SynthPreset::Full,
            max_iterations: 5,
            run_timing_analysis: true,
            ..Default::default()
        }
    }

    /// Create config for timing-driven optimization
    pub fn timing() -> Self {
        Self {
            preset: SynthPreset::Timing,
            target_period: Some(10000.0), // 10ns default
            max_iterations: 5,
            run_timing_analysis: true,
            ..Default::default()
        }
    }

    /// Create config for timing-driven optimization with specific period
    pub fn timing_with_period(target_period: TimePs) -> Self {
        Self {
            preset: SynthPreset::Timing,
            target_period: Some(target_period),
            max_iterations: 5,
            run_timing_analysis: true,
            ..Default::default()
        }
    }

    /// Create config for area-focused optimization
    pub fn area() -> Self {
        Self {
            preset: SynthPreset::Area,
            max_iterations: 3,
            run_timing_analysis: false,
            ..Default::default()
        }
    }
}

/// Synthesis engine for logic optimization
#[derive(Debug)]
pub struct SynthEngine {
    /// Configuration
    config: SynthConfig,
    /// Timing constraints
    constraints: Option<TimingConstraints>,
    /// Pass results from last run
    pass_results: Vec<PassResult>,
    /// Timing analysis result
    timing_result: Option<StaResult>,
    /// Mapping result
    mapping_result: Option<MappingResult>,
    /// Pipeline annotations from retiming passes
    pipeline_annotations: PipelineAnnotations,
    /// Total optimization time
    total_time_ms: u64,
}

impl Default for SynthEngine {
    fn default() -> Self {
        Self::new()
    }
}

impl SynthEngine {
    /// Create a new synthesis engine with default configuration
    pub fn new() -> Self {
        Self {
            config: SynthConfig::default(),
            constraints: None,
            pass_results: Vec::new(),
            timing_result: None,
            mapping_result: None,
            pipeline_annotations: PipelineAnnotations::new(),
            total_time_ms: 0,
        }
    }

    /// Create an engine with specific preset
    pub fn with_preset(preset: SynthPreset) -> Self {
        let config = match preset {
            SynthPreset::Quick => SynthConfig::quick(),
            SynthPreset::Balanced => SynthConfig::default(),
            SynthPreset::Full => SynthConfig::full(),
            SynthPreset::Timing => SynthConfig {
                preset: SynthPreset::Timing,
                run_timing_analysis: true,
                max_iterations: 4,
                ..Default::default()
            },
            SynthPreset::Area => SynthConfig {
                preset: SynthPreset::Area,
                max_iterations: 4,
                ..Default::default()
            },
            // Resyn2 runs the full sequence once (no iterations - the sequence itself has repetition)
            SynthPreset::Resyn2 => SynthConfig {
                preset: SynthPreset::Resyn2,
                max_iterations: 3, // Run the resyn2 sequence 3 times for convergence
                run_timing_analysis: true,
                ..Default::default()
            },
            // Compress2 - aggressive area optimization with resubstitution
            SynthPreset::Compress2 => SynthConfig {
                preset: SynthPreset::Compress2,
                max_iterations: 3,
                ..Default::default()
            },
            // Auto - will run multiple presets in parallel
            SynthPreset::Auto => SynthConfig {
                preset: SynthPreset::Auto,
                max_iterations: 3,
                ..Default::default()
            },
        };

        Self {
            config,
            ..Self::new()
        }
    }

    /// Create an engine with specific configuration
    pub fn with_config(config: SynthConfig) -> Self {
        Self {
            config,
            ..Self::new()
        }
    }

    /// Set timing constraints
    pub fn set_constraints(&mut self, constraints: TimingConstraints) {
        self.constraints = Some(constraints);
    }

    /// Set target clock period
    pub fn set_target_period(&mut self, period: TimePs) {
        self.config.target_period = Some(period);
        self.config.run_timing_analysis = true;
    }

    /// Enable verbose logging
    pub fn set_verbose(&mut self, verbose: bool) {
        self.config.verbose = verbose;
    }

    /// Optimize a GateNetlist
    pub fn optimize(&mut self, netlist: &GateNetlist, library: &TechLibrary) -> SynthResult {
        // Handle Auto preset - run multiple presets in parallel and pick best
        if self.config.preset == SynthPreset::Auto {
            return self.optimize_auto(netlist, library);
        }

        let start = Instant::now();
        self.pass_results.clear();

        // Phase 0: Partition — extract AIG-incompatible cells (RAM, DSP, PLL, arithmetic, etc.)
        let mut partition = super::partition::partition_for_aig(netlist);

        // Phase 0.5: Datapath optimization — rebuild adder chains if a better architecture exists
        self.run_datapath_optimization(&mut partition, library);

        let aig_input = partition
            .as_ref()
            .map(|p| &p.optimizable)
            .unwrap_or(netlist);

        // Phase 1: Build AIG (only from AIG-compatible cells)
        let builder = AigBuilder::new(aig_input);
        let mut aig = builder.build();

        let initial_stats = aig.compute_stats();

        // Phase 2: Run optimization passes
        self.run_optimization_passes(&mut aig);

        let final_stats = aig.compute_stats();

        // Phase 3: Run timing analysis if requested
        if self.config.run_timing_analysis {
            self.run_timing_analysis(&aig);
        }

        // Phase 3.5: DFF functional decomposition via cofactoring
        // Extract enable/reset signals from latch data cones AFTER ABC optimization.
        let latch_decomps = super::dff_decompose::decompose_latches(&mut aig);

        // Phase 4: Map to library cells using available primitives
        self.run_technology_mapping(&aig, library);

        // Phase 5: Convert back to GateNetlist using technology mapping results
        let writer = if let Some(ref mapping) = self.mapping_result {
            let mut w = AigWriter::with_mapping(library, mapping);
            w.set_latch_decompositions(latch_decomps);
            w
        } else {
            let mut w = AigWriter::new(library);
            w.set_latch_decompositions(latch_decomps);
            w
        };
        let mut optimized = writer.write(&aig);

        // Phase 5b: Merge physical cells back if we partitioned
        if let Some(ref part) = partition {
            optimized = super::partition::merge_after_aig(optimized, part);
        }

        // Preserve NCL flag from input netlist - needed for async-STA
        optimized.is_ncl = netlist.is_ncl;

        self.total_time_ms = start.elapsed().as_millis() as u64;

        // Build pipeline annotations with metadata
        let annotations = if self.pipeline_annotations.has_retiming() {
            let mut annotations = std::mem::take(&mut self.pipeline_annotations);
            if let Some(period) = self.config.target_period {
                annotations.metadata.target_frequency_mhz = 1_000_000.0 / period;
                // ps to MHz
            }
            if let Some(ref timing) = self.timing_result {
                // Use wns (worst negative slack) - more negative means longer critical path
                annotations.metadata.original_critical_path_ps = -timing.wns;
                annotations.metadata.timing_met = timing.is_timing_met();
            }
            Some(annotations)
        } else {
            None
        };

        SynthResult {
            netlist: optimized,
            initial_and_count: initial_stats.and_count,
            final_and_count: final_stats.and_count,
            initial_levels: initial_stats.max_level as usize,
            final_levels: final_stats.max_level as usize,
            pass_results: self.pass_results.clone(),
            timing_result: self.timing_result.clone(),
            mapping_result: self.mapping_result.clone(),
            pipeline_annotations: annotations,
            total_time_ms: self.total_time_ms,
        }
    }

    /// Run multiple optimization presets in parallel and return the best result
    fn optimize_auto(&mut self, netlist: &GateNetlist, library: &TechLibrary) -> SynthResult {
        let start = Instant::now();

        // Presets to try in parallel
        let presets = [SynthPreset::Resyn2, SynthPreset::Compress2];

        // Clone netlist and library for parallel use
        let netlist_clone = netlist.clone();
        let library_clone = library.clone();

        // Run presets in parallel
        let results: Vec<(SynthPreset, SynthResult)> = presets
            .par_iter()
            .map(|&preset| {
                let mut engine = SynthEngine::with_preset(preset);
                let result = engine.optimize(&netlist_clone, &library_clone);
                (preset, result)
            })
            .collect();

        // Find the best result (fewest gates in final netlist)
        let (best_preset, mut best_result) = results
            .into_iter()
            .min_by_key(|(_, result)| result.netlist.cell_count())
            .expect("At least one preset should run");

        let total_time = start.elapsed().as_millis() as u64;

        // Update timing to reflect total parallel time
        best_result.total_time_ms = total_time;

        // Copy results to self for consistency
        self.pass_results = best_result.pass_results.clone();
        self.timing_result = best_result.timing_result.clone();
        self.mapping_result = best_result.mapping_result.clone();
        if let Some(ref annotations) = best_result.pipeline_annotations {
            self.pipeline_annotations = annotations.clone();
        }
        self.total_time_ms = total_time;

        best_result
    }

    /// Optimize a hierarchical netlist with per-instance parallel synthesis
    ///
    /// Uses module-type caching: identical module types (same name + cell count)
    /// are optimized only once, and the result is reused for all instances.
    /// This dramatically speeds up designs with many identical FP/arithmetic units.
    pub fn optimize_hierarchical(
        &mut self,
        hier: &crate::hierarchical_netlist::HierarchicalNetlist,
        library: &TechLibrary,
    ) -> crate::hierarchical_netlist::HierarchicalSynthResult {
        use crate::hierarchical_netlist::{HierarchicalNetlist, HierarchicalSynthResult};
        use std::sync::Mutex;

        let start = Instant::now();

        // Group instances by module signature (name + cell count + output count)
        // This allows caching optimization results for identical modules
        let mut module_groups: IndexMap<String, Vec<String>> = IndexMap::new();

        // Sort instance paths for deterministic iteration order (HashMap is non-deterministic)
        let mut sorted_instance_paths: Vec<_> = hier.instances.keys().collect();
        sorted_instance_paths.sort();

        for path in sorted_instance_paths {
            let inst = &hier.instances[path];
            // Create signature from module name + cell count + output count
            let signature = format!(
                "{}_{}_{}",
                inst.module_name,
                inst.netlist.cell_count(),
                inst.netlist.outputs.len()
            );
            module_groups
                .entry(signature)
                .or_default()
                .push(path.clone());
        }

        let unique_modules = module_groups.len();
        let total_instances = hier.instances.len();
        let cache_hits = total_instances - unique_modules;

        // Optimize each unique module type in parallel
        // Use the first instance of each type as the representative
        let cache: Mutex<IndexMap<String, SynthResult>> = Mutex::new(IndexMap::new());

        // Sort signature keys for deterministic processing order
        let mut unique_sigs: Vec<_> = module_groups.keys().cloned().collect();
        unique_sigs.sort();

        unique_sigs.par_iter().for_each(|signature| {
            let paths = &module_groups[signature];
            let first_path = &paths[0];
            let inst = &hier.instances[first_path];

            let mut engine = SynthEngine::with_preset(SynthPreset::Auto);
            let result = engine.optimize(&inst.netlist, library);

            cache.lock().unwrap().insert(signature.clone(), result);
        });

        let cached_results = cache.into_inner().unwrap();

        // Build result map by applying cached results to all instances
        let mut optimized: IndexMap<String, SynthResult> = IndexMap::new();

        // Sort signature keys for deterministic iteration
        let mut sorted_sigs: Vec<_> = module_groups.keys().collect();
        sorted_sigs.sort();

        for signature in sorted_sigs {
            let paths = &module_groups[signature];
            let result = &cached_results[signature];
            for path in paths {
                optimized.insert(path.clone(), result.clone());
            }
        }

        // Build optimized hierarchical netlist
        let mut opt_hier = hier.clone();

        // Sort paths for deterministic iteration
        let mut sorted_opt_paths: Vec<_> = optimized.keys().collect();
        sorted_opt_paths.sort();

        for path in sorted_opt_paths {
            let result = &optimized[path];
            if let Some(inst) = opt_hier.instances.get_mut(path) {
                inst.netlist = result.netlist.clone();
            }
        }

        let total_time = start.elapsed().as_millis() as u64;

        // Copy results to self for consistency
        self.total_time_ms = total_time;

        HierarchicalSynthResult {
            netlist: opt_hier,
            instance_results: optimized,
            total_time_ms: total_time,
        }
    }

    /// Optimize an AIG directly with optional library-aware mapping
    ///
    /// If a library is provided, technology mapping will use the library's
    /// cells. Otherwise, default cell definitions are used.
    pub fn optimize_aig(
        &mut self,
        aig: &mut Aig,
        library: Option<&TechLibrary>,
    ) -> Vec<PassResult> {
        self.pass_results.clear();
        self.run_optimization_passes(aig);

        if self.config.run_timing_analysis {
            self.run_timing_analysis(aig);
        }

        // Technology mapping requires a library
        if let Some(lib) = library {
            self.run_technology_mapping(aig, lib);
        } else {
            // No library provided - skip mapping (caller must handle this)
            // The mapping will be done at a higher level with the appropriate library
        }

        self.pass_results.clone()
    }

    /// Run the optimization pass sequence with convergence detection.
    ///
    /// Terminates when:
    /// - Exact convergence: and_count and max_level unchanged
    /// - Epsilon convergence: < 1% improvement for 2 consecutive iterations
    /// - Max iteration cap reached (10, regardless of preset)
    fn run_optimization_passes(&mut self, aig: &mut Aig) {
        // Skip optimization for trivial designs — not worth the pass overhead
        if aig.and_count() <= 2 {
            return;
        }

        let passes = self.get_pass_sequence();
        let max_iters = self.config.max_iterations.max(10); // Raise cap; convergence does real termination
        let mut consecutive_small_improvement = 0u32;

        for _iteration in 0..max_iters {
            let before = aig.compute_stats();
            let before_and = before.and_count;

            for pass_name in &passes {
                if let Some(result) = self.run_pass(aig, pass_name) {
                    self.pass_results.push(result);
                }
            }

            let after = aig.compute_stats();

            // Exact convergence: nothing changed
            if before.and_count == after.and_count && before.max_level == after.max_level {
                break;
            }

            // Epsilon convergence: < 1% improvement for 2 consecutive iterations
            let improvement = if before_and > 0 {
                (before_and as f64 - after.and_count as f64) / before_and as f64
            } else {
                0.0
            };

            if improvement.abs() < 0.01 {
                consecutive_small_improvement += 1;
                if consecutive_small_improvement >= 2 {
                    break;
                }
            } else {
                consecutive_small_improvement = 0;
            }
        }
    }

    /// Get the pass sequence for the current preset
    fn get_pass_sequence(&self) -> Vec<String> {
        if let Some(ref custom) = self.config.custom_passes {
            return custom.clone();
        }

        match self.config.preset {
            SynthPreset::Quick => vec!["strash".to_string(), "dce".to_string()],
            // Balanced: Use Resyn2-strength sequence for better QoR
            SynthPreset::Balanced => vec![
                "strash".to_string(),
                "balance".to_string(),
                "rewrite".to_string(),
                "refactor".to_string(),
                "balance".to_string(),
                "rewrite".to_string(),
                "rewrite_z".to_string(),
                "balance".to_string(),
                "refactor_z".to_string(),
                "rewrite_z".to_string(),
                "balance".to_string(),
                "dce".to_string(),
            ],
            SynthPreset::Full => vec![
                "strash".to_string(),
                "const_prop".to_string(),
                "balance".to_string(),
                "rewrite".to_string(),
                "refactor".to_string(),
                "balance".to_string(),
                "rewrite".to_string(),
                "fraig".to_string(), // SAT-based equivalence checking
                "dce".to_string(),
            ],
            SynthPreset::Timing => vec![
                "strash".to_string(),
                "balance".to_string(),
                "rewrite".to_string(),
                "balance".to_string(),
                "retiming".to_string(), // Register retiming for timing optimization
                "buffer".to_string(),   // Buffer insertion for fanout management
                "dce".to_string(),
            ],
            SynthPreset::Area => vec![
                "strash".to_string(),
                "const_prop".to_string(),
                "rewrite".to_string(),
                "resub".to_string(),
                "refactor".to_string(),
                "rewrite".to_string(),
                "resub".to_string(),
                "dce".to_string(),
            ],
            // ABC's resyn2: strash; balance; rewrite; refactor; balance; rewrite; rewrite -z; balance; refactor -z; rewrite -z; balance
            SynthPreset::Resyn2 => vec![
                "strash".to_string(),
                "balance".to_string(),
                "rewrite".to_string(),
                "refactor".to_string(),
                "balance".to_string(),
                "rewrite".to_string(),
                "rewrite_z".to_string(), // zero-cost rewrite
                "balance".to_string(),
                "refactor_z".to_string(), // zero-cost refactor
                "rewrite_z".to_string(),  // zero-cost rewrite
                "balance".to_string(),
                "dce".to_string(),
            ],
            // ABC's compress2-style: aggressive area optimization with resubstitution
            // Key: run refactor AFTER balance to find better factorizations
            SynthPreset::Compress2 => vec![
                "strash".to_string(),
                "balance".to_string(),
                "resub".to_string(),
                "rewrite".to_string(),
                "resub".to_string(),
                "refactor".to_string(),
                "resub".to_string(),
                "balance".to_string(),
                "refactor_z".to_string(),
                "resub_z".to_string(),
                "rewrite_z".to_string(),
                "balance".to_string(),
                "refactor_z".to_string(),
                "resub_z".to_string(),
                "fraig".to_string(),
                "dce".to_string(),
            ],
            // Auto is handled at optimize() level, shouldn't reach here
            SynthPreset::Auto => unreachable!("Auto preset handled in optimize_auto()"),
        }
    }

    /// Run a single pass by name
    fn run_pass(&mut self, aig: &mut Aig, pass_name: &str) -> Option<PassResult> {
        match pass_name {
            "strash" => {
                let mut pass = Strash::new();
                Some(pass.run(aig))
            }
            "const_prop" | "constprop" => {
                let mut pass = ConstProp::new();
                Some(pass.run(aig))
            }
            "balance" => {
                let mut pass = Balance::new();
                Some(pass.run(aig))
            }
            "rewrite" => {
                let mut pass = Rewrite::new();
                Some(pass.run(aig))
            }
            // Zero-cost rewrite (equivalent to ABC's `rewrite -z`)
            "rewrite_z" | "rewrite-z" => {
                let mut pass = Rewrite::zero_cost();
                Some(pass.run(aig))
            }
            "refactor" => {
                let mut pass = Refactor::new();
                Some(pass.run(aig))
            }
            // Zero-cost refactor (equivalent to ABC's `refactor -z`)
            "refactor_z" | "refactor-z" => {
                let mut pass = Refactor::zero_cost();
                Some(pass.run(aig))
            }
            "dce" => {
                let mut pass = Dce::new();
                Some(pass.run(aig))
            }
            "fraig" => {
                // FRAIG: SAT-based functional equivalence checking
                let config = FraigConfig::default();
                let mut pass = Fraig::with_config(config);
                Some(pass.run(aig))
            }
            "fraig_choices" => {
                // FRAIG with choice recording for mapper exploration
                let config = FraigConfig {
                    record_choices: true,
                    ..Default::default()
                };
                let mut pass = Fraig::with_config(config);
                Some(pass.run(aig))
            }
            "buffer" | "buffer_insert" => {
                // Buffer insertion for fanout management
                let config = BufferConfig::default();
                let mut pass = BufferInsertion::with_config(config);
                Some(pass.run(aig))
            }
            "buffer_perf" => {
                // High-performance buffer insertion (more aggressive)
                let config = BufferConfig::high_performance();
                let mut pass = BufferInsertion::with_config(config);
                Some(pass.run(aig))
            }
            "retiming" | "retime" => {
                // Register retiming for timing optimization
                let mut config = RetimingConfig::default();
                if let Some(period) = self.config.target_period {
                    config.target_period = period;
                }
                let mut pass = Retiming::with_config(config);
                let result = pass.run(aig);
                // Extract annotations from retiming pass
                let annotations = pass.take_annotations();
                if !annotations.pipeline_stages.is_empty() {
                    self.pipeline_annotations.add_module(annotations);
                }
                Some(result)
            }
            "retiming_hf" => {
                // High-frequency retiming (aggressive)
                let config = RetimingConfig::high_frequency();
                let mut pass = Retiming::with_config(config);
                let result = pass.run(aig);
                // Extract annotations from retiming pass
                let annotations = pass.take_annotations();
                if !annotations.pipeline_stages.is_empty() {
                    self.pipeline_annotations.add_module(annotations);
                }
                Some(result)
            }
            "resub" => {
                // Resubstitution: re-express nodes using existing divisors
                let mut pass = Resub::new();
                Some(pass.run(aig))
            }
            "resub_z" | "resub-z" => {
                // Zero-cost resubstitution
                let mut pass = Resub::zero_cost();
                Some(pass.run(aig))
            }
            "dc2" => {
                // DC2: Don't care based optimization
                let mut pass = Dc2::new();
                Some(pass.run(aig))
            }
            "scorr" => {
                // Sequential SAT sweeping - find equivalent signals
                let mut pass = Scorr::new();
                Some(pass.run(aig))
            }
            "dchoice" | "choice" => {
                // Choice-based synthesis - record equivalent implementations
                let mut pass = Dchoice::new();
                Some(pass.run(aig))
            }
            _ => None,
        }
    }

    /// Run timing analysis
    fn run_timing_analysis(&mut self, aig: &Aig) {
        let mut sta = Sta::new();

        if let Some(ref constraints) = self.constraints {
            sta.set_constraints(constraints.clone());
        } else if let Some(period) = self.config.target_period {
            let mut constraints = TimingConstraints::new();
            constraints.add_clock("clk", period);
            sta.set_constraints(constraints);
        }

        self.timing_result = Some(sta.analyze(aig));
    }

    /// Run technology mapping using cells from the library
    fn run_technology_mapping(&mut self, aig: &Aig, library: &TechLibrary) {
        let result = match self.config.preset {
            SynthPreset::Timing => {
                let config = DelayMappingConfig {
                    target_period: self.config.target_period.unwrap_or(10000.0),
                    area_recovery: true,
                    ..Default::default()
                };
                let mapper = DelayMapper::from_library_with_config(library, config);
                let delay_result = mapper.map(aig);
                MappingResult {
                    mapped_nodes: delay_result.mapped_nodes,
                    stats: delay_result.stats,
                }
            }
            SynthPreset::Quick => {
                // Fast mapping with minimal optimization, using library cells
                // Cut size auto-configured from library's lut_size
                let config = CutMapperConfig::fast_for_library(library);
                let mapper = CutMapper::from_library_with_config(library, config);
                mapper.map(aig)
            }
            SynthPreset::Full | SynthPreset::Resyn2 => {
                // Quality mapping with priority cuts and area recovery
                // Cut size auto-configured from library's lut_size
                let mut config = CutMapperConfig::quality_for_library(library);
                // Enable choice-aware mapping if AIG has choice nodes
                config.use_choices = aig.choice_node_count() > 0;
                let mapper = CutMapper::from_library_with_config(library, config);
                mapper.map(aig)
            }
            SynthPreset::Area | SynthPreset::Compress2 => {
                // Area-focused mapping with aggressive area recovery
                // Cut size auto-configured from library's lut_size
                let config = CutMapperConfig::for_library(library);
                let config = CutMapperConfig {
                    use_priority_cuts: true,
                    area_recovery: true,
                    recovery_iterations: 3,
                    ..config
                };
                let mut mapper = CutMapper::from_library_with_config(library, config);
                mapper.set_objective(MappingObjective::Area);
                mapper.map(aig)
            }
            SynthPreset::Balanced => {
                // Quality mapping using library cells
                // Cut size auto-configured from library's lut_size
                let config = CutMapperConfig::for_library(library);
                let mapper = CutMapper::from_library_with_config(library, config);
                mapper.map(aig)
            }
            // Auto is handled at optimize() level, shouldn't reach here
            SynthPreset::Auto => unreachable!("Auto preset handled in optimize_auto()"),
        };

        self.mapping_result = Some(result);
    }

    /// Get the pass results from the last run
    pub fn pass_results(&self) -> &[PassResult] {
        &self.pass_results
    }

    /// Get the timing result from the last run
    pub fn timing_result(&self) -> Option<&StaResult> {
        self.timing_result.as_ref()
    }

    /// Get the mapping result from the last run
    pub fn mapping_result(&self) -> Option<&MappingResult> {
        self.mapping_result.as_ref()
    }

    /// Get total optimization time in milliseconds
    pub fn total_time_ms(&self) -> u64 {
        self.total_time_ms
    }

    /// Run datapath optimization on partitioned-out arithmetic cells.
    ///
    /// For FPGA targets with carry chain support, adder cells are already optimal
    /// (ripple carry via XOR + Carry primitives) and are simply preserved.
    /// For ASIC targets or wide adders, the optimizer may select a better architecture
    /// (CLA, Kogge-Stone, Brent-Kung) and rebuild the chain.
    fn run_datapath_optimization(
        &self,
        partition: &mut Option<NetlistPartition>,
        library: &TechLibrary,
    ) {
        let part = match partition.as_mut() {
            Some(p) => p,
            None => return,
        };

        let chains = detect_adder_chains(&part.physical_cells);
        if chains.is_empty() {
            return;
        }

        // FPGA with carry chains: ripple carry is already optimal, skip rebuild
        if library.is_fpga() && library.has_function(&CellFunction::Carry) {
            return;
        }

        let dp_config = if let Some(period) = self.config.target_period {
            DatapathConfig::with_timing(period)
        } else {
            DatapathConfig::default()
        };
        let optimizer = AdderOptimizer::with_config(dp_config);

        for chain in &chains {
            let arch = optimizer.select_architecture(chain.width, None);

            // Only rebuild if a better architecture than ripple carry is available
            if arch != AdderArchitecture::RippleCarry {
                rebuild_adder_chain(part, chain, &optimizer, arch, library);
            }
        }
    }
}

/// Detected adder chain in the physical partition
struct AdderChain {
    width: usize,
    cells: Vec<CellId>,
    input_a_nets: Vec<GateNetId>,
    input_b_nets: Vec<GateNetId>,
    carry_in: Option<GateNetId>,
    sum_nets: Vec<GateNetId>,
    carry_out: Option<GateNetId>,
}

/// Detect adder chains among partitioned-out physical cells.
///
/// Groups cells by path prefix (e.g., "top.add0.carry1" → prefix "top.add0"),
/// then within each group finds Carry/FullAdder/HalfAdder chains and traces
/// their I/O nets.
fn detect_adder_chains(physical_cells: &[Cell]) -> Vec<AdderChain> {
    // Group arithmetic cells by path prefix
    let mut groups: HashMap<String, Vec<&Cell>> = HashMap::new();

    for cell in physical_cells {
        let func = match &cell.function {
            Some(CellFunction::Carry)
            | Some(CellFunction::FullAdder)
            | Some(CellFunction::HalfAdder) => cell.function.as_ref().unwrap(),
            _ => continue,
        };

        // Extract prefix: "top.add0.carry1" → "top.add0"
        // The last segment contains the cell type + bit index
        let prefix = match cell.path.rfind('.') {
            Some(pos) => &cell.path[..pos],
            None => continue,
        };

        groups.entry(prefix.to_string()).or_default().push(cell);
    }

    let mut chains = Vec::new();

    for cells in groups.values() {
        // Try to build a carry chain from Carry cells (FPGA path)
        let carry_cells: Vec<&&Cell> = cells
            .iter()
            .filter(|c| matches!(c.function, Some(CellFunction::Carry)))
            .collect();

        if !carry_cells.is_empty() {
            if let Some(chain) = build_carry_chain(&carry_cells) {
                chains.push(chain);
                continue;
            }
        }

        // Try FullAdder/HalfAdder chain (ASIC path)
        let adder_cells: Vec<&&Cell> = cells
            .iter()
            .filter(|c| {
                matches!(
                    c.function,
                    Some(CellFunction::FullAdder | CellFunction::HalfAdder)
                )
            })
            .collect();

        if !adder_cells.is_empty() {
            if let Some(chain) = build_adder_chain(&adder_cells) {
                chains.push(chain);
            }
        }
    }

    chains
}

/// Build an AdderChain from a set of Carry cells (FPGA carry chain path).
///
/// Carry cells have 3 inputs (a, b, cin) and 1 output (cout).
/// They are connected in a chain where each cell's cout feeds the next cell's cin.
fn build_carry_chain(cells: &[&&Cell]) -> Option<AdderChain> {
    if cells.is_empty() {
        return None;
    }

    // Sort by bit index extracted from path suffix
    let mut indexed: Vec<(usize, &Cell)> = cells
        .iter()
        .filter_map(|c| {
            let suffix = c.path.rsplit('.').next()?;
            // Extract digit from suffix like "carry1" → 1
            let idx: usize = suffix
                .chars()
                .filter(|c| c.is_ascii_digit())
                .collect::<String>()
                .parse()
                .ok()?;
            Some((idx, **c))
        })
        .collect();
    indexed.sort_by_key(|(idx, _)| *idx);

    if indexed.is_empty() {
        return None;
    }

    let width = indexed.len();
    let mut cell_ids = Vec::with_capacity(width);
    let mut input_a_nets = Vec::with_capacity(width);
    let mut input_b_nets = Vec::with_capacity(width);
    let mut carry_in = None;
    let mut carry_out = None;

    for (i, (_idx, cell)) in indexed.iter().enumerate() {
        cell_ids.push(cell.id);

        // Carry cell inputs: [a, b, cin]
        if cell.inputs.len() >= 2 {
            input_a_nets.push(cell.inputs[0]);
            input_b_nets.push(cell.inputs[1]);
        }
        if i == 0 && cell.inputs.len() >= 3 {
            carry_in = Some(cell.inputs[2]);
        }

        // Last cell's output is the chain carry-out
        if i == width - 1 {
            if let Some(&cout) = cell.outputs.first() {
                carry_out = Some(cout);
            }
        }
    }

    // For FPGA carry chains, sum nets come from the XOR cells (not the Carry cells).
    // We don't have them here since XOR cells are AIG-compatible and stayed in the
    // optimizable partition. The sum nets are handled by the AIG path.
    // We provide empty sum_nets since we won't rebuild FPGA carry chains.
    let sum_nets = vec![GateNetId(0); width];

    Some(AdderChain {
        width,
        cells: cell_ids,
        input_a_nets,
        input_b_nets,
        carry_in,
        sum_nets,
        carry_out,
    })
}

/// Build an AdderChain from FullAdder/HalfAdder cells (ASIC path).
///
/// HalfAdder: inputs [a, b], outputs [sum, carry]
/// FullAdder: inputs [a, b, cin], outputs [sum, carry]
fn build_adder_chain(cells: &[&&Cell]) -> Option<AdderChain> {
    if cells.is_empty() {
        return None;
    }

    // Sort by bit index from path
    let mut indexed: Vec<(usize, &Cell)> = cells
        .iter()
        .filter_map(|c| {
            let suffix = c.path.rsplit('.').next()?;
            let idx: usize = suffix
                .chars()
                .filter(|c| c.is_ascii_digit())
                .collect::<String>()
                .parse()
                .ok()?;
            Some((idx, **c))
        })
        .collect();
    indexed.sort_by_key(|(idx, _)| *idx);

    if indexed.is_empty() {
        return None;
    }

    let width = indexed.len();
    let mut cell_ids = Vec::with_capacity(width);
    let mut input_a_nets = Vec::with_capacity(width);
    let mut input_b_nets = Vec::with_capacity(width);
    let mut sum_nets = Vec::with_capacity(width);
    let mut carry_in = None;
    let mut carry_out = None;

    for (i, (_idx, cell)) in indexed.iter().enumerate() {
        cell_ids.push(cell.id);

        // Both HA and FA have a, b as first two inputs
        if cell.inputs.len() >= 2 {
            input_a_nets.push(cell.inputs[0]);
            input_b_nets.push(cell.inputs[1]);
        }

        // First cell: if FA, carry_in is input[2]; if HA, no carry_in
        if i == 0
            && matches!(cell.function, Some(CellFunction::FullAdder))
            && cell.inputs.len() >= 3
        {
            carry_in = Some(cell.inputs[2]);
        }

        // Both HA and FA have [sum, carry] as outputs
        if !cell.outputs.is_empty() {
            sum_nets.push(cell.outputs[0]);
        }

        // Last cell's carry output
        if i == width - 1 && cell.outputs.len() >= 2 {
            carry_out = Some(cell.outputs[1]);
        }
    }

    Some(AdderChain {
        width,
        cells: cell_ids,
        input_a_nets,
        input_b_nets,
        carry_in,
        sum_nets,
        carry_out,
    })
}

/// Rebuild an adder chain using a better architecture.
///
/// Generates the new adder in a temporary AIG, converts it to a gate netlist,
/// and replaces the old chain cells in the partition.
fn rebuild_adder_chain(
    partition: &mut NetlistPartition,
    chain: &AdderChain,
    optimizer: &AdderOptimizer,
    arch: AdderArchitecture,
    library: &TechLibrary,
) {
    // 1. Generate adder in a temporary AIG
    let mut config = AdderConfig::new(chain.width).with_architecture(arch);
    if chain.carry_in.is_some() {
        config = config.with_carry_in();
    }
    if chain.carry_out.is_some() {
        config = config.with_carry_out();
    }

    let mut temp_aig = Aig::new("adder_rebuild".to_string());
    let result = optimizer.generate(&mut temp_aig, &config);

    // 2. Register outputs in temp AIG
    for (i, &sum_lit) in result.sum.iter().enumerate() {
        temp_aig.add_output(format!("sum{}", i), sum_lit);
    }
    if let Some(cout) = result.carry_out {
        temp_aig.add_output("cout".to_string(), cout);
    }

    // 3. Convert to gate netlist via AigWriter
    let writer = AigWriter::new(library);
    let rebuilt = writer.write(&temp_aig);

    // 4. Build I/O name → original net ID mapping
    //    AdderOptimizer creates inputs "a0".."aN", "b0".."bN", "cin"
    //    and we registered outputs "sum0".."sumN", "cout"
    let mut io_map: HashMap<String, GateNetId> = HashMap::new();
    for i in 0..chain.width {
        io_map.insert(format!("a{}", i), chain.input_a_nets[i]);
        io_map.insert(format!("b{}", i), chain.input_b_nets[i]);
        io_map.insert(format!("sum{}", i), chain.sum_nets[i]);
    }
    if let Some(cin) = chain.carry_in {
        io_map.insert("cin".into(), cin);
    }
    if let Some(cout) = chain.carry_out {
        io_map.insert("cout".into(), cout);
    }

    // 5. Map temp net IDs → original net IDs
    //    I/O nets: use io_map. Internal nets: allocate new IDs.
    // Compute actual max net ID across the entire partition to avoid collisions
    let max_orig_id = partition
        .physical_cells
        .iter()
        .flat_map(|c| {
            c.inputs
                .iter()
                .chain(c.outputs.iter())
                .chain(c.clock.iter())
                .chain(c.reset.iter())
        })
        .map(|id| id.0)
        .max()
        .unwrap_or(0);
    let mut next_id = max_orig_id + 1;
    let mut temp_to_orig: HashMap<GateNetId, GateNetId> = HashMap::new();

    for net in &rebuilt.nets {
        if let Some(&orig_id) = io_map.get(&net.name) {
            temp_to_orig.insert(net.id, orig_id);
        } else {
            temp_to_orig.insert(net.id, GateNetId(next_id));
            next_id += 1;
        }
    }

    // 6. Remove old chain cells, add rebuilt cells with remapped nets
    let chain_ids: HashSet<CellId> = chain.cells.iter().copied().collect();
    partition
        .physical_cells
        .retain(|c| !chain_ids.contains(&c.id));

    let remap = |id: &GateNetId| -> GateNetId { temp_to_orig.get(id).copied().unwrap_or(*id) };

    for cell in &rebuilt.cells {
        let new_cell = Cell {
            id: cell.id,
            cell_type: cell.cell_type.clone(),
            library: cell.library.clone(),
            function: cell.function.clone(),
            fit: cell.fit,
            failure_modes: cell.failure_modes.clone(),
            inputs: cell.inputs.iter().map(&remap).collect(),
            outputs: cell.outputs.iter().map(&remap).collect(),
            path: cell.path.clone(),
            clock: cell.clock.map(|id| remap(&id)),
            reset: cell.reset.map(|id| remap(&id)),
            source_op: Some("DatapathOpt".to_string()),
            safety_classification: cell.safety_classification.clone(),
            lut_init: cell.lut_init,
            parameters: cell.parameters.clone(),
        };
        partition.physical_cells.push(new_cell);
    }
}

/// Result of synthesis optimization
#[derive(Debug, Clone)]
pub struct SynthResult {
    /// Optimized netlist
    pub netlist: GateNetlist,
    /// Initial AND gate count
    pub initial_and_count: usize,
    /// Final AND gate count
    pub final_and_count: usize,
    /// Initial logic levels
    pub initial_levels: usize,
    /// Final logic levels
    pub final_levels: usize,
    /// Results from each pass
    pub pass_results: Vec<PassResult>,
    /// Timing analysis result
    pub timing_result: Option<StaResult>,
    /// Technology mapping result
    pub mapping_result: Option<MappingResult>,
    /// Pipeline annotations from retiming passes
    pub pipeline_annotations: Option<PipelineAnnotations>,
    /// Total time in milliseconds
    pub total_time_ms: u64,
}

impl SynthResult {
    /// Get the gate reduction ratio
    pub fn gate_reduction(&self) -> f64 {
        if self.initial_and_count == 0 {
            0.0
        } else {
            1.0 - (self.final_and_count as f64 / self.initial_and_count as f64)
        }
    }

    /// Get the level reduction ratio
    pub fn level_reduction(&self) -> f64 {
        if self.initial_levels == 0 {
            0.0
        } else {
            1.0 - (self.final_levels as f64 / self.initial_levels as f64)
        }
    }

    /// Check if timing was met
    pub fn timing_met(&self) -> bool {
        self.timing_result
            .as_ref()
            .map(|r| r.is_timing_met())
            .unwrap_or(true)
    }

    /// Get a summary string
    pub fn summary(&self) -> String {
        format!(
            "Gates: {} -> {} ({:.1}% reduction), Levels: {} -> {} ({:.1}% reduction), Time: {}ms",
            self.initial_and_count,
            self.final_and_count,
            self.gate_reduction() * 100.0,
            self.initial_levels,
            self.final_levels,
            self.level_reduction() * 100.0,
            self.total_time_ms
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_synth_engine_creation() {
        let engine = SynthEngine::new();
        assert_eq!(engine.config.preset, SynthPreset::Balanced);
    }

    #[test]
    fn test_synth_preset() {
        let quick = SynthEngine::with_preset(SynthPreset::Quick);
        assert_eq!(quick.config.max_iterations, 1);

        let full = SynthEngine::with_preset(SynthPreset::Full);
        assert_eq!(full.config.max_iterations, 5);
    }

    #[test]
    fn test_synth_config() {
        let config = SynthConfig::timing_with_period(5000.0);
        assert_eq!(config.target_period, Some(5000.0));
        assert!(config.run_timing_analysis);

        let config_default = SynthConfig::timing();
        assert_eq!(config_default.target_period, Some(10000.0)); // default 10ns
        assert!(config_default.run_timing_analysis);
    }

    #[test]
    fn test_get_pass_sequence() {
        let engine = SynthEngine::with_preset(SynthPreset::Quick);
        let passes = engine.get_pass_sequence();
        assert!(passes.contains(&"strash".to_string()));
        assert!(passes.contains(&"dce".to_string()));
    }

    #[test]
    fn test_optimize_aig_simple() {
        use crate::synth::{Aig, AigLit};

        // Build a non-trivial AIG (>2 AND nodes) so optimization passes run
        let mut aig = Aig::new("test".to_string());
        let a = aig.add_input("a".to_string(), None);
        let b = aig.add_input("b".to_string(), None);
        let c = aig.add_input("c".to_string(), None);
        let ab = aig.add_and(AigLit::new(a), AigLit::new(b));
        let abc = aig.add_and(ab, AigLit::new(c));
        let not_ab = aig.add_and(AigLit::not(a), AigLit::not(b));
        let result = aig.add_and(abc, not_ab.invert());
        aig.add_output("y".to_string(), result);

        let mut engine = SynthEngine::with_preset(SynthPreset::Quick);
        let results = engine.optimize_aig(&mut aig, None);

        // Should have run some passes
        assert!(!results.is_empty());
    }

    #[test]
    fn test_optimize_aig_trivial_skipped() {
        use crate::synth::{Aig, AigLit};

        // Trivial AIG (≤2 AND nodes) — optimization passes should be skipped
        let mut aig = Aig::new("test".to_string());
        let a = aig.add_input("a".to_string(), None);
        let b = aig.add_input("b".to_string(), None);
        let ab = aig.add_and(AigLit::new(a), AigLit::new(b));
        aig.add_output("y".to_string(), ab);

        let mut engine = SynthEngine::with_preset(SynthPreset::Quick);
        let results = engine.optimize_aig(&mut aig, None);

        // Trivial designs skip optimization — no passes run
        assert!(results.is_empty());
    }

    #[test]
    fn test_synth_result_metrics() {
        let result = SynthResult {
            netlist: GateNetlist::new("test".to_string(), "default".to_string()),
            initial_and_count: 100,
            final_and_count: 60,
            initial_levels: 10,
            final_levels: 7,
            pass_results: Vec::new(),
            timing_result: None,
            mapping_result: None,
            pipeline_annotations: None,
            total_time_ms: 50,
        };

        assert!((result.gate_reduction() - 0.4).abs() < 0.001);
        assert!((result.level_reduction() - 0.3).abs() < 0.001);
        assert!(result.timing_met()); // No timing constraint
    }

    /// Regression test: AIG optimization must not collapse an 8-bit adder to constant.
    ///
    /// Before the fix, `resolve_lit` returned `AigLit::false_lit()` when a node
    /// wasn't found in the rebuild map, silently collapsing live logic to 0.
    /// This happened when `apply_substitutions` created forward references and
    /// a downstream pass (DCE) iterated in index order without a topological
    /// rebuild first.
    #[test]
    fn test_balanced_optimization_preserves_adder_logic() {
        use crate::synth::{Aig, AigLit};

        // Build an 8-bit ripple-carry adder: sum[i] = a[i] ^ b[i] ^ carry[i]
        let mut aig = Aig::new("adder8".to_string());

        let mut a_inputs = Vec::new();
        let mut b_inputs = Vec::new();
        for i in 0..8 {
            a_inputs.push(aig.add_input(format!("a{}", i), None));
            b_inputs.push(aig.add_input(format!("b{}", i), None));
        }
        let cin = aig.add_input("cin".to_string(), None);

        let mut carry = AigLit::new(cin);
        for i in 0..8 {
            let a = AigLit::new(a_inputs[i]);
            let b = AigLit::new(b_inputs[i]);

            // sum = a ^ b ^ carry
            let ab_xor = aig.add_xor(a, b);
            let sum = aig.add_xor(ab_xor, carry);
            aig.add_output(format!("sum{}", i), sum);

            // carry_out = (a & b) | (carry & (a ^ b))
            let ab_and = aig.add_and(a, b);
            let carry_ab_xor = aig.add_and(carry, ab_xor);
            carry = aig.add_or(ab_and, carry_ab_xor);
        }
        aig.add_output("cout".to_string(), carry);

        let ands_before = aig.and_count();
        assert!(
            ands_before > 20,
            "adder should have significant AND count before optimization, got {}",
            ands_before
        );

        // Run Balanced preset optimization (the preset that triggered the bug)
        let mut engine = SynthEngine::with_preset(SynthPreset::Balanced);
        engine.optimize_aig(&mut aig, None);

        let ands_after = aig.and_count();

        // The adder MUST retain meaningful logic. Before the fix, this collapsed
        // to ~0 AND nodes. A correct optimization should leave at least 10 ANDs
        // for an 8-bit adder (ABC typically gets ~24 for ripple-carry).
        assert!(
            ands_after >= 10,
            "8-bit adder collapsed to {} AND nodes after Balanced optimization — \
             resolve_lit may be silently returning FALSE for unresolved nodes",
            ands_after
        );
    }

    #[test]
    fn test_ice40_adder_preserves_carry_chain() {
        use crate::lir::{Lir, LirOp};
        use crate::tech_mapper::map_word_lir_to_gates;

        // Build 8-bit adder LIR
        let mut lir = Lir::new("test_adder8".to_string());
        let a = lir.add_input("a".to_string(), 8);
        let b = lir.add_input("b".to_string(), 8);
        let sum = lir.add_output("sum".to_string(), 8);

        lir.add_node(
            LirOp::Add {
                width: 8,
                has_carry: false,
                const_b: None,
            },
            vec![a, b],
            sum,
            "test.add".to_string(),
        );

        // Tech-map to ice40
        let lib =
            crate::tech_library::get_stdlib_library("ice40").expect("Failed to load ice40 library");
        let mapped = map_word_lir_to_gates(&lir, &lib);

        // Count carry cells before synthesis
        let carry_before = mapped
            .netlist
            .cells
            .iter()
            .filter(|c| matches!(c.function, Some(CellFunction::Carry)))
            .count();
        assert!(
            carry_before > 0,
            "Tech mapper should produce Carry cells for ice40 adder"
        );

        // Run synthesis
        let mut engine = SynthEngine::with_preset(SynthPreset::Balanced);
        let result = engine.optimize(&mapped.netlist, &lib);

        // Carry cells must be preserved (not destroyed by AIG optimization)
        let carry_after = result
            .netlist
            .cells
            .iter()
            .filter(|c| matches!(c.function, Some(CellFunction::Carry)))
            .count();
        assert_eq!(
            carry_after, carry_before,
            "Carry cells must be preserved through synthesis: before={}, after={}",
            carry_before, carry_after
        );

        // Total cell count should be better than before (was 57 LUTs without carry preservation).
        // With carry cells preserved, the AIG only processes XOR logic.
        // The carry chain (7 cells) is kept intact; XOR cells get some AIG overhead.
        let logic_cells = result
            .netlist
            .cells
            .iter()
            .filter(|c| {
                !matches!(
                    c.cell_type.as_str(),
                    "SB_IO" | "SB_VCC" | "SB_GND" | "SB_GB"
                )
            })
            .count();
        assert!(
            logic_cells <= 45,
            "8-bit ice40 adder should have fewer than 45 logic cells (was 57 before), got {}",
            logic_cells
        );
    }
}
