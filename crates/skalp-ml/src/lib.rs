//! ML-Guided Synthesis Optimization
//!
//! This crate provides machine learning integration for logic synthesis
//! optimization in SKALP. It enables learned decision-making for:
//!
//! - **Pass Ordering**: Which optimization passes to run and in what order
//! - **Cut Selection**: Which cuts to choose during technology mapping
//! - **Architecture Selection**: Which datapath architecture to use
//!
//! # Architecture
//!
//! The ML layer sits on top of the classical synthesis engine:
//!
//! ```text
//! ┌─────────────────────────────────────────────────────────────┐
//! │                    ML Decision Layer                         │
//! │  ┌──────────────┐ ┌──────────────┐ ┌──────────────────────┐ │
//! │  │ Pass Ordering│ │ Cut Selector │ │ Architecture Advisor │ │
//! │  │    (RL)      │ │    (GNN)     │ │       (MLP)          │ │
//! │  └──────┬───────┘ └──────┬───────┘ └──────────┬───────────┘ │
//! └─────────┼────────────────┼────────────────────┼─────────────┘
//!           │                │                    │
//!           v                v                    v
//! ┌─────────────────────────────────────────────────────────────┐
//! │              Classical Synthesis Engine                      │
//! │  ┌──────────┐ ┌──────────┐ ┌──────────┐ ┌──────────┐       │
//! │  │   AIG    │ │  Passes  │ │ Mapping  │ │ Datapath │       │
//! │  └──────────┘ └──────────┘ └──────────┘ └──────────┘       │
//! └─────────────────────────────────────────────────────────────┘
//! ```
//!
//! # Features
//!
//! - `onnx`: Enable ONNX Runtime for model inference
//! - `training`: Enable training utilities
//!
//! # Example
//!
//! ```ignore
//! use skalp_ml::{MlSynthEngine, MlConfig};
//! use skalp_lir::synth::Aig;
//!
//! // Create ML-guided engine
//! let mut engine = MlSynthEngine::new(MlConfig::default());
//!
//! // Optimize with learned decisions
//! engine.optimize(&mut aig);
//! ```

pub mod arch_select;
pub mod cut_gnn;
pub mod features;
pub mod pass_advisor;
pub mod policy;
pub mod trainer;
pub mod training_data;

pub use arch_select::{ArchAdvisor, ArchAdvisorConfig, DatapathArchitecture};
pub use cut_gnn::{CutScorer, CutScorerConfig, GnnCutSelector};
pub use features::{extract_features, AigFeatures, FeatureExtractor};
pub use pass_advisor::{MlPassAdvisor, PassAction, PassAdvisorConfig};
pub use policy::{PolicyNetwork, SimplePolicy};
pub use trainer::{PolicyTrainer, TrainerConfig, TrainingMode, TrainingStats};
pub use training_data::{
    CollectorConfig, QualityOfResult, SynthesisEpisode, TrainingDataCollector, TrainingDataset,
};

use skalp_lir::synth::{Aig, AigBuilder, AigWriter, PassResult};
use skalp_lir::{GateNetlist, TechLibrary};
use thiserror::Error;

/// Errors that can occur in ML operations
#[derive(Error, Debug)]
pub enum MlError {
    #[error("Model not loaded: {0}")]
    ModelNotLoaded(String),

    #[error("Feature extraction failed: {0}")]
    FeatureExtractionFailed(String),

    #[error("Inference failed: {0}")]
    InferenceFailed(String),

    #[error("Invalid configuration: {0}")]
    InvalidConfig(String),
}

/// Result type for ML operations
pub type MlResult<T> = Result<T, MlError>;

/// Configuration for ML-guided synthesis
#[derive(Debug, Clone)]
pub struct MlConfig {
    /// Enable ML-guided pass ordering
    pub use_pass_advisor: bool,
    /// Enable ML-guided cut selection
    pub use_cut_selector: bool,
    /// Enable ML-guided architecture selection
    pub use_arch_advisor: bool,
    /// Maximum passes to run
    pub max_passes: usize,
    /// Fallback to heuristics if ML fails
    pub fallback_on_error: bool,
    /// Path to policy model (ONNX format)
    pub policy_model_path: Option<String>,
    /// Exploration rate for training (epsilon-greedy)
    pub exploration_rate: f64,
}

impl Default for MlConfig {
    fn default() -> Self {
        Self {
            use_pass_advisor: true,
            use_cut_selector: false, // Requires GNN, disabled by default
            use_arch_advisor: false, // Requires MLP, disabled by default
            max_passes: 20,
            fallback_on_error: true,
            policy_model_path: None,
            exploration_rate: 0.0, // No exploration during inference
        }
    }
}

impl MlConfig {
    /// Create config for training mode
    pub fn training() -> Self {
        Self {
            exploration_rate: 0.2, // 20% random exploration - mostly on-policy data
            max_passes: 40,        // Run more passes to gather more decisions
            ..Default::default()
        }
    }

    /// Create config with all ML features enabled
    pub fn full() -> Self {
        Self {
            use_pass_advisor: true,
            use_cut_selector: true,
            use_arch_advisor: true,
            ..Default::default()
        }
    }
}

/// Result of ML-guided netlist optimization
#[derive(Debug, Clone)]
pub struct MlOptimizeResult {
    /// The optimized netlist
    pub netlist: GateNetlist,
    /// Initial AND gate count
    pub initial_and_count: usize,
    /// Final AND gate count
    pub final_and_count: usize,
    /// Initial logic levels
    pub initial_levels: u32,
    /// Final logic levels
    pub final_levels: u32,
    /// Number of passes executed
    pub passes_executed: usize,
    /// Sequence of passes applied
    pub pass_sequence: Vec<String>,
    /// ML decisions made
    pub ml_decisions: usize,
    /// Improvement achieved (0.0-1.0)
    pub improvement: f64,
}

/// ML-guided synthesis engine
///
/// Wraps the classical synthesis engine with ML-based decision making.
pub struct MlSynthEngine {
    config: MlConfig,
    pass_advisor: MlPassAdvisor,
    feature_extractor: FeatureExtractor,
    /// Statistics from ML decisions
    stats: MlSynthStats,
    /// Optional training data collector for recording decisions
    collector: Option<TrainingDataCollector>,
}

/// Statistics from ML-guided synthesis
#[derive(Debug, Clone, Default, serde::Serialize)]
pub struct MlSynthStats {
    /// Total passes executed
    pub passes_executed: usize,
    /// ML decisions made
    pub ml_decisions: usize,
    /// Fallback decisions (when ML unavailable)
    pub fallback_decisions: usize,
    /// Pass sequence chosen
    pub pass_sequence: Vec<String>,
    /// Improvement achieved
    pub improvement: f64,
}

impl MlSynthEngine {
    /// Create a new ML-guided synthesis engine
    pub fn new(config: MlConfig) -> Self {
        let advisor_config = PassAdvisorConfig {
            exploration_rate: config.exploration_rate,
            max_no_improvement: 5, // Allow more passes before giving up
            ..Default::default()
        };
        Self {
            pass_advisor: MlPassAdvisor::new(advisor_config),
            feature_extractor: FeatureExtractor::new(),
            config,
            stats: MlSynthStats::default(),
            collector: None,
        }
    }

    /// Create an engine with training data collection enabled
    pub fn with_training(config: MlConfig, run_id: &str) -> Self {
        let advisor_config = PassAdvisorConfig {
            exploration_rate: config.exploration_rate,
            max_no_improvement: 10, // More lenient for training
            ..Default::default()
        };
        Self {
            pass_advisor: MlPassAdvisor::new(advisor_config),
            feature_extractor: FeatureExtractor::new(),
            config,
            stats: MlSynthStats::default(),
            collector: Some(TrainingDataCollector::new(run_id)),
        }
    }

    /// Enable training data collection
    pub fn enable_training(&mut self, run_id: &str) {
        self.collector = Some(TrainingDataCollector::new(run_id));
    }

    /// Get the training data collector (for exporting data)
    pub fn collector(&self) -> Option<&TrainingDataCollector> {
        self.collector.as_ref()
    }

    /// Get mutable access to the training data collector
    pub fn collector_mut(&mut self) -> Option<&mut TrainingDataCollector> {
        self.collector.as_mut()
    }

    /// Export training data to a directory
    pub fn export_training_data(&self, output_dir: &str) -> std::io::Result<()> {
        if let Some(ref collector) = self.collector {
            collector.export(output_dir)
        } else {
            Ok(())
        }
    }

    /// Get statistics from last run
    pub fn stats(&self) -> &MlSynthStats {
        &self.stats
    }

    /// Optimize an AIG using ML-guided pass selection
    pub fn optimize(&mut self, aig: &mut Aig) -> MlResult<Vec<PassResult>> {
        self.optimize_with_design_name(aig, "unknown", "default")
    }

    /// Optimize an AIG with design name for training data collection
    pub fn optimize_with_design_name(
        &mut self,
        aig: &mut Aig,
        design_name: &str,
        library_name: &str,
    ) -> MlResult<Vec<PassResult>> {
        self.stats = MlSynthStats::default();
        let mut results = Vec::new();

        // Extract initial features
        let initial_features = self.feature_extractor.extract(aig);
        let initial_cost = initial_features.estimated_cost();

        // Start training episode if collector is enabled
        if let Some(ref mut collector) = self.collector {
            collector.start_episode(design_name, library_name, &initial_features);
        }

        // Run optimization loop
        for _iter in 0..self.config.max_passes {
            // Extract current features
            let features_before = self.feature_extractor.extract(aig);

            // Get next action from policy
            let action = if self.config.use_pass_advisor {
                self.stats.ml_decisions += 1;
                self.pass_advisor.suggest_action(&features_before)
            } else {
                self.stats.fallback_decisions += 1;
                PassAction::default_sequence()
            };

            // Check for termination
            if matches!(action, PassAction::Terminate) {
                break;
            }

            // Skip if ML selected DCE (we'll run it automatically anyway)
            if matches!(action, PassAction::Dce) {
                continue;
            }

            // Execute the suggested pass
            if let Some(result) = self.execute_action(aig, &action) {
                // Extract features after the pass
                let features_after = self.feature_extractor.extract(aig);

                // Record decision for training
                if let Some(ref mut collector) = self.collector {
                    collector.record_pass_decision(
                        &features_before,
                        action,
                        None, // action_probs not available from simple advisor
                        &result,
                        &features_after,
                    );
                }

                self.stats.pass_sequence.push(result.pass_name.clone());
                self.stats.passes_executed += 1;
                results.push(result);

                // Auto-run DCE after every pass (guaranteed cleanup)
                if let Some(dce_result) = self.execute_action(aig, &PassAction::Dce) {
                    self.stats.pass_sequence.push(dce_result.pass_name.clone());
                    self.stats.passes_executed += 1;
                    results.push(dce_result);
                }
            }
        }

        // Compute improvement
        let final_features = self.feature_extractor.extract(aig);
        let final_cost = final_features.estimated_cost();
        self.stats.improvement = if initial_cost > 0.0 {
            (initial_cost - final_cost) / initial_cost
        } else {
            0.0
        };

        // End training episode with QoR
        if let Some(ref mut collector) = self.collector {
            let qor = QualityOfResult {
                cell_count: final_features.node_count,
                and_equivalent: final_features.and_count,
                critical_delay: final_features.max_level as f64 * 10.0, // Rough estimate
                total_area: final_features.estimated_cost(),
                inverter_count: 0,
                buffer_count: 0,
                cell_distribution: std::collections::HashMap::new(),
                improvement: self.stats.improvement,
            };
            collector.end_episode(&final_features, qor);
        }

        Ok(results)
    }

    /// Optimize a GateNetlist using ML-guided pass selection
    ///
    /// This is the high-level API that takes a gate netlist, converts to AIG,
    /// runs ML-guided optimization, and converts back to a gate netlist.
    pub fn optimize_netlist(
        &mut self,
        netlist: &GateNetlist,
        library: &TechLibrary,
    ) -> MlResult<MlOptimizeResult> {
        // Phase 1: Build AIG from netlist
        let builder = AigBuilder::new(netlist);
        let mut aig = builder.build();

        let initial_stats = aig.compute_stats();

        // Phase 2: Run ML-guided optimization
        let pass_results = self.optimize(&mut aig)?;

        let final_stats = aig.compute_stats();

        // Phase 3: Write back to netlist
        let writer = AigWriter::new(library);
        let optimized_netlist = writer.write(&aig);

        Ok(MlOptimizeResult {
            netlist: optimized_netlist,
            initial_and_count: initial_stats.and_count,
            final_and_count: final_stats.and_count,
            initial_levels: initial_stats.max_level,
            final_levels: final_stats.max_level,
            passes_executed: pass_results.len(),
            pass_sequence: self.stats.pass_sequence.clone(),
            ml_decisions: self.stats.ml_decisions,
            improvement: self.stats.improvement,
        })
    }

    /// Execute a pass action
    fn execute_action(&mut self, aig: &mut Aig, action: &PassAction) -> Option<PassResult> {
        use skalp_lir::synth::*;

        match action {
            PassAction::Terminate => None,
            PassAction::Rewrite => {
                let mut pass = Rewrite::new();
                Some(pass.run(aig))
            }
            PassAction::Refactor => {
                let mut pass = Refactor::new();
                Some(pass.run(aig))
            }
            PassAction::Balance => {
                let mut pass = Balance::new();
                Some(pass.run(aig))
            }
            PassAction::Strash => {
                let mut pass = Strash::new();
                Some(pass.run(aig))
            }
            PassAction::Fraig => {
                let mut pass = Fraig::new();
                Some(pass.run(aig))
            }
            PassAction::Dce => {
                let mut pass = Dce::new();
                Some(pass.run(aig))
            }
            PassAction::ConstProp => {
                let mut pass = ConstProp::new();
                Some(pass.run(aig))
            }
        }
    }

    /// Load a policy model from file (ONNX format)
    #[cfg(feature = "onnx")]
    pub fn load_policy(&mut self, path: &str) -> MlResult<()> {
        self.pass_advisor.load_model(path)
    }

    /// Load a trained policy from a JSON file
    pub fn load_json_policy(&mut self, path: &str) -> std::io::Result<()> {
        self.pass_advisor.load_policy_from_file(path)
    }

    /// Save training data for offline learning
    pub fn save_training_data(&self, path: &str) -> MlResult<()> {
        // Serialize pass sequence and outcomes for training
        let data = serde_json::to_string_pretty(&self.stats)
            .map_err(|e| MlError::InvalidConfig(e.to_string()))?;
        std::fs::write(path, data).map_err(|e| MlError::InvalidConfig(e.to_string()))?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ml_config_default() {
        let config = MlConfig::default();
        assert!(config.use_pass_advisor);
        assert!(!config.use_cut_selector);
        assert!(config.fallback_on_error);
    }

    #[test]
    fn test_ml_config_training() {
        let config = MlConfig::training();
        assert!(config.exploration_rate > 0.0);
    }

    #[test]
    fn test_ml_synth_engine_creation() {
        let engine = MlSynthEngine::new(MlConfig::default());
        assert_eq!(engine.stats().passes_executed, 0);
    }

    #[test]
    fn test_ml_synth_empty_aig() {
        let mut aig = Aig::new("test".to_string());
        let mut engine = MlSynthEngine::new(MlConfig::default());

        let results = engine.optimize(&mut aig).unwrap();
        // Empty AIG should terminate quickly
        assert!(results.len() <= 20);
    }

    #[test]
    fn test_training_data_collection() {
        use skalp_lir::synth::AigLit;

        // Create an engine with training enabled
        let mut engine = MlSynthEngine::with_training(MlConfig::default(), "test_run");

        // Create a simple AIG with some logic
        let mut aig = Aig::new("counter".to_string());
        let a = aig.add_input("a".to_string(), None);
        let b = aig.add_input("b".to_string(), None);
        let and_node = aig.add_and(AigLit::new(a), AigLit::new(b));
        aig.add_output("out".to_string(), and_node);

        // Run optimization
        let _results = engine
            .optimize_with_design_name(&mut aig, "counter", "7nm")
            .unwrap();

        // Verify training data was collected
        let collector = engine.collector().expect("Collector should exist");
        assert_eq!(collector.episode_count(), 1);

        // Check stats
        let stats = collector.stats();
        // Pass decisions may be 0 if no passes were beneficial (usize always >= 0)
        assert_eq!(stats.total_episodes, 1);
    }

    #[test]
    fn test_training_data_export() {
        use skalp_lir::synth::AigLit;

        // Create an engine with training enabled
        let mut engine = MlSynthEngine::with_training(MlConfig::default(), "export_test");

        // Create a simple AIG
        let mut aig = Aig::new("simple".to_string());
        let a = aig.add_input("a".to_string(), None);
        let b = aig.add_input("b".to_string(), None);
        let and_node = aig.add_and(AigLit::new(a), AigLit::new(b));
        aig.add_output("out".to_string(), and_node);

        // Run optimization
        let _results = engine
            .optimize_with_design_name(&mut aig, "simple", "default")
            .unwrap();

        // Export to temp directory
        let temp_dir = std::env::temp_dir().join("skalp_ml_test");
        engine
            .export_training_data(temp_dir.to_str().unwrap())
            .unwrap();

        // Verify files were created
        assert!(temp_dir.join("dataset.json").exists());
        assert!(temp_dir.join("stats.json").exists());

        // Clean up
        let _ = std::fs::remove_dir_all(&temp_dir);
    }
}
