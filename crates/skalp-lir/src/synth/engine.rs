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

use super::liberty::LibertyLibrary;
use super::mapping::{
    CutMapper, CutMapperConfig, DelayMapper, DelayMappingConfig, MappingObjective, MappingResult,
};
use super::passes::{
    Balance, BufferConfig, BufferInsertion, ConstProp, Dc2, Dce, Dchoice, Fraig, FraigConfig, Pass,
    PassResult, Refactor, Resub, Retiming, RetimingConfig, Rewrite, Scorr, Strash,
};
use super::sta::{Sta, StaResult};
use super::timing::{TimePs, TimingConstraints};
use super::{Aig, AigBuilder, AigWriter};
use crate::gate_netlist::GateNetlist;
use crate::pipeline_annotations::{ModuleAnnotations, PipelineAnnotations};
use crate::tech_library::TechLibrary;
use rayon::prelude::*;
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

        // Phase 1: Build AIG
        if self.config.verbose {
            eprintln!("Building AIG from GateNetlist...");
        }
        let builder = AigBuilder::new(netlist);
        let mut aig = builder.build();

        let initial_stats = aig.compute_stats();
        eprintln!(
            "[ENGINE] Initial AIG: {} ANDs, {} levels, {} latches, {} inputs, {} outputs",
            initial_stats.and_count,
            initial_stats.max_level,
            initial_stats.latch_count,
            initial_stats.input_count,
            initial_stats.output_count
        );
        if self.config.verbose {
            eprintln!(
                "Initial: {} ANDs, {} levels",
                initial_stats.and_count, initial_stats.max_level
            );
        }

        // Phase 2: Run optimization passes
        self.run_optimization_passes(&mut aig);

        let final_stats = aig.compute_stats();
        eprintln!(
            "[ENGINE] After optimization: {} ANDs, {} levels, {} latches",
            final_stats.and_count, final_stats.max_level, final_stats.latch_count
        );
        if self.config.verbose {
            eprintln!(
                "After optimization: {} ANDs, {} levels",
                final_stats.and_count, final_stats.max_level
            );
        }

        // Phase 3: Run timing analysis if requested
        if self.config.run_timing_analysis {
            self.run_timing_analysis(&aig);
        }

        // Phase 4: Map to library cells using available primitives
        self.run_technology_mapping(&aig, library);

        // Phase 5: Convert back to GateNetlist using technology mapping results
        if self.config.verbose {
            eprintln!("Converting AIG back to GateNetlist...");
        }
        let writer = if let Some(ref mapping) = self.mapping_result {
            AigWriter::with_mapping(library, mapping)
        } else {
            AigWriter::new(library)
        };
        let mut optimized = writer.write(&aig);

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

        // DEBUG: eprintln!("[AUTO] Running multiple optimization strategies in parallel...");

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

        eprintln!(
            "[AUTO] Best result: {:?} with {} cells (tested {} presets in {}ms)",
            best_preset,
            best_result.netlist.cell_count(),
            presets.len(),
            total_time
        );

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
        let mut module_groups: std::collections::HashMap<String, Vec<String>> =
            std::collections::HashMap::new();

        for (path, inst) in &hier.instances {
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

        eprintln!(
            "[HIER] Optimizing {} unique module types ({} instances, {} cache hits)",
            unique_modules, total_instances, cache_hits
        );

        // Optimize each unique module type in parallel
        // Use the first instance of each type as the representative
        let cache: Mutex<std::collections::HashMap<String, SynthResult>> =
            Mutex::new(std::collections::HashMap::new());

        let unique_sigs: Vec<_> = module_groups.keys().cloned().collect();

        unique_sigs.par_iter().for_each(|signature| {
            let paths = &module_groups[signature];
            let first_path = &paths[0];
            let inst = &hier.instances[first_path];

            let original_cells = inst.netlist.cell_count();
            eprintln!(
                "[HIER] {} ({}) -> optimizing... ({} cells, {} outputs) [{}x instances]",
                first_path,
                inst.module_name,
                original_cells,
                inst.netlist.outputs.len(),
                paths.len()
            );

            let mut engine = SynthEngine::with_preset(SynthPreset::Auto);
            let result = engine.optimize(&inst.netlist, library);

            let optimized_cells = result.netlist.cell_count();
            eprintln!(
                "[HIER] {} -> {} cells (was {})",
                first_path, optimized_cells, original_cells
            );

            cache.lock().unwrap().insert(signature.clone(), result);
        });

        let cached_results = cache.into_inner().unwrap();

        // Build result map by applying cached results to all instances
        let mut optimized: std::collections::HashMap<String, SynthResult> =
            std::collections::HashMap::new();

        for (signature, paths) in &module_groups {
            let result = &cached_results[signature];
            for path in paths {
                optimized.insert(path.clone(), result.clone());
            }
        }

        // Build optimized hierarchical netlist
        let mut opt_hier = hier.clone();
        for (path, result) in &optimized {
            if let Some(inst) = opt_hier.instances.get_mut(path) {
                inst.netlist = result.netlist.clone();
            }
        }

        let total_time = start.elapsed().as_millis() as u64;

        eprintln!(
            "[HIER] Hierarchical synthesis complete: {} instances ({} unique) in {}ms",
            hier.instances.len(),
            unique_modules,
            total_time
        );

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

    /// Run the optimization pass sequence
    fn run_optimization_passes(&mut self, aig: &mut Aig) {
        let passes = self.get_pass_sequence();

        for iteration in 0..self.config.max_iterations {
            if self.config.verbose {
                eprintln!("=== Iteration {} ===", iteration + 1);
            }

            let before = aig.compute_stats();

            for pass_name in &passes {
                if let Some(result) = self.run_pass(aig, pass_name) {
                    let stats = aig.compute_stats();
                    eprintln!(
                        "[PASS] {}: {} -> {} ANDs, {} latches, {} inputs, {} outputs",
                        result.pass_name,
                        result.ands_before,
                        result.ands_after,
                        stats.latch_count,
                        stats.input_count,
                        stats.output_count
                    );
                    self.pass_results.push(result);
                }
            }

            let after = aig.compute_stats();

            // Check for convergence
            if before.and_count == after.and_count && before.max_level == after.max_level {
                if self.config.verbose {
                    eprintln!("Converged after {} iterations", iteration + 1);
                }
                break;
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
            // Balanced: Use Full-style sequence which preserves SDFFE patterns better
            SynthPreset::Balanced => vec![
                "strash".to_string(),
                "const_prop".to_string(),
                "balance".to_string(),
                "rewrite".to_string(),
                "refactor".to_string(),
                "balance".to_string(),
                "rewrite".to_string(),
                "fraig".to_string(), // SAT-based equivalence - don't add more passes after
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
            // ABC's resyn2: balance; rewrite; refactor; balance; rewrite; rewrite -z; balance; refactor -z; rewrite -z; balance
            SynthPreset::Resyn2 => vec![
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
            _ => {
                if self.config.verbose {
                    eprintln!("Unknown pass: {}", pass_name);
                }
                None
            }
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

        if self.config.verbose {
            if let Some(ref result) = self.timing_result {
                eprintln!("Timing: {}", result.summary());
            }
        }
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
                let mapper = CutMapper::from_library_with_config(library, CutMapperConfig::fast());
                mapper.map(aig)
            }
            SynthPreset::Full | SynthPreset::Resyn2 => {
                // Quality mapping with priority cuts and area recovery
                let mut config = CutMapperConfig::quality();
                // Enable choice-aware mapping if AIG has choice nodes
                config.use_choices = aig.choice_node_count() > 0;
                let mapper = CutMapper::from_library_with_config(library, config);
                mapper.map(aig)
            }
            SynthPreset::Area | SynthPreset::Compress2 => {
                // Area-focused mapping with aggressive area recovery
                let config = CutMapperConfig {
                    use_priority_cuts: true,
                    area_recovery: true,
                    recovery_iterations: 3,
                    ..Default::default()
                };
                let mut mapper = CutMapper::from_library_with_config(library, config);
                mapper.set_objective(MappingObjective::Area);
                mapper.map(aig)
            }
            SynthPreset::Balanced => {
                // Quality mapping using library cells
                let config = CutMapperConfig::quality();
                let mapper = CutMapper::from_library_with_config(library, config);
                mapper.map(aig)
            }
            // Auto is handled at optimize() level, shouldn't reach here
            SynthPreset::Auto => unreachable!("Auto preset handled in optimize_auto()"),
        };

        if self.config.verbose {
            eprintln!("Mapping: {}", result.stats.summary());
        }

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

        let mut aig = Aig::new("test".to_string());
        let a = aig.add_input("a".to_string(), None);
        let b = aig.add_input("b".to_string(), None);
        let ab = aig.add_and(AigLit::new(a), AigLit::new(b));
        aig.add_output("y".to_string(), ab);

        let mut engine = SynthEngine::with_preset(SynthPreset::Quick);
        let results = engine.optimize_aig(&mut aig, None);

        // Should have run some passes
        assert!(!results.is_empty());
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
}
