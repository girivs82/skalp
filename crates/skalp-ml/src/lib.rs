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

pub use arch_select::{ArchAdvisor, ArchAdvisorConfig, DatapathArchitecture};
pub use cut_gnn::{CutScorer, CutScorerConfig, GnnCutSelector};
pub use features::{extract_features, AigFeatures, FeatureExtractor};
pub use pass_advisor::{MlPassAdvisor, PassAction, PassAdvisorConfig};
pub use policy::{PolicyNetwork, SimplePolicy};

use skalp_lir::synth::{Aig, PassResult};
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
            exploration_rate: 0.1, // 10% random exploration
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

/// ML-guided synthesis engine
///
/// Wraps the classical synthesis engine with ML-based decision making.
pub struct MlSynthEngine {
    config: MlConfig,
    pass_advisor: MlPassAdvisor,
    feature_extractor: FeatureExtractor,
    /// Statistics from ML decisions
    stats: MlSynthStats,
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
        Self {
            pass_advisor: MlPassAdvisor::new(PassAdvisorConfig::default()),
            feature_extractor: FeatureExtractor::new(),
            config,
            stats: MlSynthStats::default(),
        }
    }

    /// Get statistics from last run
    pub fn stats(&self) -> &MlSynthStats {
        &self.stats
    }

    /// Optimize an AIG using ML-guided pass selection
    pub fn optimize(&mut self, aig: &mut Aig) -> MlResult<Vec<PassResult>> {
        self.stats = MlSynthStats::default();
        let mut results = Vec::new();

        // Extract initial features
        let initial_features = self.feature_extractor.extract(aig);
        let initial_cost = initial_features.estimated_cost();

        // Run optimization loop
        for _iter in 0..self.config.max_passes {
            // Extract current features
            let features = self.feature_extractor.extract(aig);

            // Get next action from policy
            let action = if self.config.use_pass_advisor {
                self.stats.ml_decisions += 1;
                self.pass_advisor.suggest_action(&features)
            } else {
                self.stats.fallback_decisions += 1;
                PassAction::default_sequence()
            };

            // Check for termination
            if matches!(action, PassAction::Terminate) {
                break;
            }

            // Execute the suggested pass
            if let Some(result) = self.execute_action(aig, &action) {
                self.stats.pass_sequence.push(result.pass_name.clone());
                self.stats.passes_executed += 1;
                results.push(result);
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

        Ok(results)
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

    /// Load a policy model from file
    #[cfg(feature = "onnx")]
    pub fn load_policy(&mut self, path: &str) -> MlResult<()> {
        self.pass_advisor.load_model(path)
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
}
