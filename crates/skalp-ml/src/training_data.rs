//! Training Data Collection for ML-Guided Synthesis
//!
//! This module provides infrastructure for collecting high-quality training data
//! from synthesis runs. The data is structured to support training:
//!
//! - **Pass ordering policy**: Which optimization pass to run next
//! - **Cut selection GNN**: Which cut to choose during technology mapping
//! - **Cell selection model**: Which library cell to use for a given function
//!
//! # Data Collection Strategy
//!
//! We collect data at three granularities:
//!
//! 1. **Episode level**: Full synthesis trajectory with pass sequence and QoR
//! 2. **Node level**: Per-node decisions during technology mapping
//! 3. **Outcome level**: Final metrics to compute rewards
//!
//! # Usage
//!
//! ```ignore
//! let mut collector = TrainingDataCollector::new("run_001");
//! collector.start_episode("counter", &library);
//!
//! // During synthesis...
//! collector.record_pass_decision(&features, PassAction::Rewrite, &result);
//! collector.record_cut_decision(node_id, &cuts, chosen_idx, &context);
//! collector.record_cell_decision(truth_table, &context, "AOI21_X1");
//!
//! collector.end_episode(&final_features, &final_qor);
//! collector.export("training_data/")?;
//! ```

use crate::features::AigFeatures;
use crate::pass_advisor::PassAction;
use serde::{Deserialize, Serialize};
use skalp_lir::synth::{AigNodeId, Cut, PassResult};
use std::collections::HashMap;
use std::path::Path;

/// Complete training dataset from multiple synthesis runs
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TrainingDataset {
    /// Dataset metadata
    pub metadata: DatasetMetadata,
    /// All collected episodes
    pub episodes: Vec<SynthesisEpisode>,
    /// Aggregated statistics
    pub stats: DatasetStats,
}

/// Metadata about the training dataset
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DatasetMetadata {
    /// Dataset version
    pub version: String,
    /// Creation timestamp
    pub created_at: String,
    /// Number of designs processed
    pub num_designs: usize,
    /// Technology libraries used
    pub libraries: Vec<String>,
    /// Collection configuration
    pub config: CollectorConfig,
}

/// Statistics about the collected data
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct DatasetStats {
    /// Total episodes collected
    pub total_episodes: usize,
    /// Total pass decisions recorded
    pub total_pass_decisions: usize,
    /// Total cut decisions recorded
    pub total_cut_decisions: usize,
    /// Total cell decisions recorded
    pub total_cell_decisions: usize,
    /// Average improvement per episode
    pub avg_improvement: f64,
    /// Best improvement seen
    pub best_improvement: f64,
    /// Distribution of pass actions
    pub pass_action_counts: HashMap<String, usize>,
    /// Distribution of cell types chosen
    pub cell_type_counts: HashMap<String, usize>,
}

/// A single synthesis episode (one design, one run)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SynthesisEpisode {
    /// Unique episode identifier
    pub episode_id: String,
    /// Design name
    pub design_name: String,
    /// Technology library used
    pub library_name: String,
    /// Initial AIG features
    pub initial_features: AigFeatures,
    /// Final AIG features
    pub final_features: AigFeatures,
    /// Sequence of pass decisions
    pub pass_decisions: Vec<PassDecision>,
    /// Cut selection decisions (during tech mapping)
    pub cut_decisions: Vec<CutDecision>,
    /// Cell selection decisions
    pub cell_decisions: Vec<CellDecision>,
    /// Final quality of result
    pub final_qor: QualityOfResult,
    /// Total episode reward
    pub total_reward: f64,
    /// Episode duration in milliseconds
    pub duration_ms: u64,
}

/// A single pass selection decision
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PassDecision {
    /// Step number in the episode
    pub step: usize,
    /// AIG features before the pass
    pub features_before: AigFeatures,
    /// Action taken
    pub action: PassAction,
    /// Action probabilities from policy (if available)
    pub action_probs: Option<[f64; 8]>,
    /// Result of the pass
    pub pass_result: PassResultSummary,
    /// Immediate reward (change in cost)
    pub reward: f64,
    /// Features after the pass
    pub features_after: AigFeatures,
}

/// Summary of a pass result (serializable)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PassResultSummary {
    /// Pass name
    pub pass_name: String,
    /// AND count before
    pub ands_before: usize,
    /// AND count after
    pub ands_after: usize,
    /// Levels before
    pub levels_before: u32,
    /// Levels after
    pub levels_after: u32,
    /// Pass duration in microseconds
    pub duration_us: u64,
}

impl From<&PassResult> for PassResultSummary {
    fn from(result: &PassResult) -> Self {
        Self {
            pass_name: result.pass_name.clone(),
            ands_before: result.ands_before,
            ands_after: result.ands_after,
            levels_before: result.levels_before,
            levels_after: result.levels_after,
            duration_us: 0, // Duration not tracked in PassResult
        }
    }
}

/// A single cut selection decision
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CutDecision {
    /// Node being mapped
    pub node_id: u32,
    /// Node features (local context)
    pub node_features: NodeContext,
    /// All candidate cuts considered
    pub candidate_cuts: Vec<CutCandidate>,
    /// Index of chosen cut
    pub chosen_index: usize,
    /// Score of chosen cut (if scored)
    pub chosen_score: Option<f64>,
    /// Was this choice optimal? (computed post-hoc)
    pub was_optimal: Option<bool>,
    /// Regret (difference from best possible)
    pub regret: Option<f64>,
}

/// Context features for a node being mapped
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NodeContext {
    /// Node level (depth from inputs)
    pub level: u32,
    /// Fanout count
    pub fanout: u32,
    /// Is on critical path?
    pub is_critical: bool,
    /// Slack available (if timing info available)
    pub slack: Option<f64>,
    /// Number of reconvergent paths through this node
    pub reconvergence: u32,
    /// Distance to nearest output
    pub distance_to_output: u32,
}

/// A candidate cut during technology mapping
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CutCandidate {
    /// Number of leaves
    pub num_leaves: usize,
    /// Truth table (for matching)
    pub truth_table: u64,
    /// Estimated area cost
    pub area_cost: f32,
    /// Estimated delay
    pub arrival_time: f32,
    /// Area flow (for area recovery)
    pub area_flow: f32,
    /// Matched cell type (if any)
    pub matched_cell: Option<String>,
}

impl From<&Cut> for CutCandidate {
    fn from(cut: &Cut) -> Self {
        Self {
            num_leaves: cut.leaves.len(),
            truth_table: cut.truth_table,
            area_cost: cut.area_cost,
            arrival_time: cut.arrival_time,
            area_flow: cut.area_flow,
            matched_cell: None, // Filled in during collection
        }
    }
}

/// A single cell selection decision
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CellDecision {
    /// Truth table of the function
    pub truth_table: u64,
    /// Number of inputs
    pub num_inputs: usize,
    /// Context for this decision
    pub context: CellContext,
    /// Available cell options
    pub cell_options: Vec<CellOption>,
    /// Index of chosen cell
    pub chosen_index: usize,
    /// Why this cell was chosen
    pub selection_reason: SelectionReason,
}

/// Context for cell selection
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CellContext {
    /// Fanout of the node
    pub fanout: u32,
    /// Available timing slack
    pub slack: Option<f64>,
    /// Is this on the critical path?
    pub is_critical: bool,
    /// Input inversions pattern
    pub input_inversions: Vec<bool>,
    /// Output inversion needed
    pub output_inverted: bool,
}

/// A cell option during selection
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CellOption {
    /// Cell name
    pub cell_name: String,
    /// Cell area
    pub area: f64,
    /// Cell delay
    pub delay: f64,
    /// Combined cost (area + delay weighted)
    pub cost: f64,
}

/// Reason for cell selection
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum SelectionReason {
    /// Lowest cost
    LowestCost,
    /// Best delay (timing critical)
    BestDelay,
    /// Only option available
    OnlyOption,
    /// Pattern match (e.g., ANDNOT for a&~b)
    PatternMatch(String),
    /// ML model decision
    MlDecision { confidence: f64 },
}

/// Final quality of result metrics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct QualityOfResult {
    /// Final cell count
    pub cell_count: usize,
    /// Final AND equivalent count
    pub and_equivalent: usize,
    /// Critical path delay (ps)
    pub critical_delay: f64,
    /// Total area
    pub total_area: f64,
    /// Number of inverters
    pub inverter_count: usize,
    /// Number of buffers
    pub buffer_count: usize,
    /// Cell type distribution
    pub cell_distribution: HashMap<String, usize>,
    /// Improvement from initial (0-1)
    pub improvement: f64,
}

/// Configuration for the data collector
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CollectorConfig {
    /// Collect pass decisions
    pub collect_passes: bool,
    /// Collect cut decisions
    pub collect_cuts: bool,
    /// Collect cell decisions
    pub collect_cells: bool,
    /// Maximum cuts to record per node
    pub max_cuts_per_node: usize,
    /// Record action probabilities
    pub record_action_probs: bool,
    /// Compute optimal labels (slower but more informative)
    pub compute_optimal_labels: bool,
}

impl Default for CollectorConfig {
    fn default() -> Self {
        Self {
            collect_passes: true,
            collect_cuts: true,
            collect_cells: true,
            max_cuts_per_node: 10,
            record_action_probs: true,
            compute_optimal_labels: false, // Expensive, enable for final data collection
        }
    }
}

/// Training data collector
///
/// Collects training data during synthesis for later ML model training.
pub struct TrainingDataCollector {
    /// Collector configuration
    config: CollectorConfig,
    /// Current run identifier
    run_id: String,
    /// Current episode being recorded
    current_episode: Option<SynthesisEpisode>,
    /// All completed episodes
    completed_episodes: Vec<SynthesisEpisode>,
    /// Running statistics
    stats: DatasetStats,
    /// Start time of current episode
    episode_start: Option<std::time::Instant>,
    /// Current step in episode
    current_step: usize,
}

impl TrainingDataCollector {
    /// Create a new training data collector
    pub fn new(run_id: &str) -> Self {
        Self {
            config: CollectorConfig::default(),
            run_id: run_id.to_string(),
            current_episode: None,
            completed_episodes: Vec::new(),
            stats: DatasetStats::default(),
            episode_start: None,
            current_step: 0,
        }
    }

    /// Create with custom configuration
    pub fn with_config(run_id: &str, config: CollectorConfig) -> Self {
        Self {
            config,
            run_id: run_id.to_string(),
            current_episode: None,
            completed_episodes: Vec::new(),
            stats: DatasetStats::default(),
            episode_start: None,
            current_step: 0,
        }
    }

    /// Start recording a new episode
    pub fn start_episode(&mut self, design_name: &str, library_name: &str, features: &AigFeatures) {
        let episode_id = format!("{}_{}", self.run_id, self.completed_episodes.len());

        self.current_episode = Some(SynthesisEpisode {
            episode_id,
            design_name: design_name.to_string(),
            library_name: library_name.to_string(),
            initial_features: features.clone(),
            final_features: AigFeatures::default(),
            pass_decisions: Vec::new(),
            cut_decisions: Vec::new(),
            cell_decisions: Vec::new(),
            final_qor: QualityOfResult {
                cell_count: 0,
                and_equivalent: 0,
                critical_delay: 0.0,
                total_area: 0.0,
                inverter_count: 0,
                buffer_count: 0,
                cell_distribution: HashMap::new(),
                improvement: 0.0,
            },
            total_reward: 0.0,
            duration_ms: 0,
        });

        self.episode_start = Some(std::time::Instant::now());
        self.current_step = 0;
    }

    /// Record a pass decision
    pub fn record_pass_decision(
        &mut self,
        features_before: &AigFeatures,
        action: PassAction,
        action_probs: Option<[f64; 8]>,
        result: &PassResult,
        features_after: &AigFeatures,
    ) {
        if !self.config.collect_passes {
            return;
        }

        let Some(episode) = &mut self.current_episode else {
            return;
        };

        // Compute immediate reward: reduction in estimated cost
        let cost_before = features_before.estimated_cost();
        let cost_after = features_after.estimated_cost();
        let reward = (cost_before - cost_after) / cost_before.max(1.0);

        let decision = PassDecision {
            step: self.current_step,
            features_before: features_before.clone(),
            action,
            action_probs,
            pass_result: result.into(),
            reward,
            features_after: features_after.clone(),
        };

        episode.pass_decisions.push(decision);
        episode.total_reward += reward;
        self.current_step += 1;

        // Update stats
        self.stats.total_pass_decisions += 1;
        *self
            .stats
            .pass_action_counts
            .entry(format!("{:?}", action))
            .or_insert(0) += 1;
    }

    /// Record a cut selection decision
    pub fn record_cut_decision(
        &mut self,
        node_id: AigNodeId,
        node_context: NodeContext,
        candidates: &[Cut],
        chosen_index: usize,
        chosen_score: Option<f64>,
    ) {
        if !self.config.collect_cuts {
            return;
        }

        let Some(episode) = &mut self.current_episode else {
            return;
        };

        // Convert cuts to candidates (limit to max)
        let candidate_cuts: Vec<CutCandidate> = candidates
            .iter()
            .take(self.config.max_cuts_per_node)
            .map(|c| c.into())
            .collect();

        let decision = CutDecision {
            node_id: node_id.0,
            node_features: node_context,
            candidate_cuts,
            chosen_index,
            chosen_score,
            was_optimal: None, // Computed post-hoc if enabled
            regret: None,
        };

        episode.cut_decisions.push(decision);
        self.stats.total_cut_decisions += 1;
    }

    /// Record a cell selection decision
    pub fn record_cell_decision(
        &mut self,
        truth_table: u64,
        num_inputs: usize,
        context: CellContext,
        options: Vec<CellOption>,
        chosen_index: usize,
        reason: SelectionReason,
    ) {
        if !self.config.collect_cells {
            return;
        }

        let Some(episode) = &mut self.current_episode else {
            return;
        };

        // Update stats with chosen cell
        if let Some(chosen) = options.get(chosen_index) {
            *self
                .stats
                .cell_type_counts
                .entry(chosen.cell_name.clone())
                .or_insert(0) += 1;
        }

        let decision = CellDecision {
            truth_table,
            num_inputs,
            context,
            cell_options: options,
            chosen_index,
            selection_reason: reason,
        };

        episode.cell_decisions.push(decision);
        self.stats.total_cell_decisions += 1;
    }

    /// End the current episode
    pub fn end_episode(&mut self, final_features: &AigFeatures, qor: QualityOfResult) {
        let Some(mut episode) = self.current_episode.take() else {
            return;
        };

        episode.final_features = final_features.clone();
        episode.final_qor = qor;

        if let Some(start) = self.episode_start.take() {
            episode.duration_ms = start.elapsed().as_millis() as u64;
        }

        // Compute improvement
        let initial_cost = episode.initial_features.estimated_cost();
        let final_cost = final_features.estimated_cost();
        let improvement = if initial_cost > 0.0 {
            (initial_cost - final_cost) / initial_cost
        } else {
            0.0
        };
        episode.final_qor.improvement = improvement;

        // Update stats
        self.stats.total_episodes += 1;
        self.stats.avg_improvement =
            (self.stats.avg_improvement * (self.stats.total_episodes - 1) as f64 + improvement)
                / self.stats.total_episodes as f64;
        self.stats.best_improvement = self.stats.best_improvement.max(improvement);

        self.completed_episodes.push(episode);
    }

    /// Export collected data to a directory
    pub fn export(&self, output_dir: &str) -> std::io::Result<()> {
        let path = Path::new(output_dir);
        std::fs::create_dir_all(path)?;

        // Create dataset
        let dataset = TrainingDataset {
            metadata: DatasetMetadata {
                version: "1.0".to_string(),
                created_at: chrono::Utc::now().to_rfc3339(),
                num_designs: self.completed_episodes.len(),
                libraries: self
                    .completed_episodes
                    .iter()
                    .map(|e| e.library_name.clone())
                    .collect::<std::collections::HashSet<_>>()
                    .into_iter()
                    .collect(),
                config: self.config.clone(),
            },
            episodes: self.completed_episodes.clone(),
            stats: self.stats.clone(),
        };

        // Write full dataset
        let dataset_path = path.join("dataset.json");
        let json = serde_json::to_string_pretty(&dataset)?;
        std::fs::write(&dataset_path, json)?;

        // Write summary stats
        let stats_path = path.join("stats.json");
        let stats_json = serde_json::to_string_pretty(&self.stats)?;
        std::fs::write(&stats_path, stats_json)?;

        // Write individual episode files for large datasets
        if self.completed_episodes.len() > 10 {
            let episodes_dir = path.join("episodes");
            std::fs::create_dir_all(&episodes_dir)?;

            for episode in &self.completed_episodes {
                let episode_path = episodes_dir.join(format!("{}.json", episode.episode_id));
                let episode_json = serde_json::to_string_pretty(&episode)?;
                std::fs::write(&episode_path, episode_json)?;
            }
        }

        // Write pass decisions as flat CSV for easy analysis
        self.export_pass_decisions_csv(path)?;

        // Write cell decisions as flat CSV
        self.export_cell_decisions_csv(path)?;

        Ok(())
    }

    /// Export pass decisions as CSV for analysis
    fn export_pass_decisions_csv(&self, path: &Path) -> std::io::Result<()> {
        let csv_path = path.join("pass_decisions.csv");
        let mut writer = std::fs::File::create(&csv_path)?;

        use std::io::Write;
        writeln!(
            writer,
            "episode_id,step,action,ands_before,ands_after,levels_before,levels_after,reward"
        )?;

        for episode in &self.completed_episodes {
            for decision in &episode.pass_decisions {
                writeln!(
                    writer,
                    "{},{},{:?},{},{},{},{},{:.6}",
                    episode.episode_id,
                    decision.step,
                    decision.action,
                    decision.pass_result.ands_before,
                    decision.pass_result.ands_after,
                    decision.pass_result.levels_before,
                    decision.pass_result.levels_after,
                    decision.reward
                )?;
            }
        }

        Ok(())
    }

    /// Export cell decisions as CSV for analysis
    fn export_cell_decisions_csv(&self, path: &Path) -> std::io::Result<()> {
        let csv_path = path.join("cell_decisions.csv");
        let mut writer = std::fs::File::create(&csv_path)?;

        use std::io::Write;
        writeln!(
            writer,
            "episode_id,truth_table,num_inputs,num_options,chosen_cell,fanout,is_critical"
        )?;

        for episode in &self.completed_episodes {
            for decision in &episode.cell_decisions {
                let chosen_cell = decision
                    .cell_options
                    .get(decision.chosen_index)
                    .map(|c| c.cell_name.as_str())
                    .unwrap_or("unknown");

                writeln!(
                    writer,
                    "{},0x{:x},{},{},{},{},{}",
                    episode.episode_id,
                    decision.truth_table,
                    decision.num_inputs,
                    decision.cell_options.len(),
                    chosen_cell,
                    decision.context.fanout,
                    decision.context.is_critical
                )?;
            }
        }

        Ok(())
    }

    /// Get number of completed episodes
    pub fn episode_count(&self) -> usize {
        self.completed_episodes.len()
    }

    /// Get current statistics
    pub fn stats(&self) -> &DatasetStats {
        &self.stats
    }

    /// Clear all collected data
    pub fn clear(&mut self) {
        self.completed_episodes.clear();
        self.current_episode = None;
        self.stats = DatasetStats::default();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_collector_creation() {
        let collector = TrainingDataCollector::new("test_run");
        assert_eq!(collector.episode_count(), 0);
    }

    #[test]
    fn test_episode_lifecycle() {
        let mut collector = TrainingDataCollector::new("test_run");

        let features = AigFeatures::default();
        collector.start_episode("counter", "7nm", &features);

        assert!(collector.current_episode.is_some());

        let qor = QualityOfResult {
            cell_count: 10,
            and_equivalent: 20,
            critical_delay: 100.0,
            total_area: 50.0,
            inverter_count: 2,
            buffer_count: 1,
            cell_distribution: HashMap::new(),
            improvement: 0.1,
        };

        collector.end_episode(&features, qor);

        assert_eq!(collector.episode_count(), 1);
        assert!(collector.current_episode.is_none());
    }

    #[test]
    fn test_cell_decision_recording() {
        let mut collector = TrainingDataCollector::new("test_run");

        let features = AigFeatures::default();
        collector.start_episode("test", "7nm", &features);

        let context = CellContext {
            fanout: 2,
            slack: Some(10.0),
            is_critical: false,
            input_inversions: vec![false, false],
            output_inverted: false,
        };

        let options = vec![
            CellOption {
                cell_name: "AND2_X1".to_string(),
                area: 2.0,
                delay: 25.0,
                cost: 27.0,
            },
            CellOption {
                cell_name: "NAND2_X1".to_string(),
                area: 1.5,
                delay: 18.0,
                cost: 19.5,
            },
        ];

        collector.record_cell_decision(0x8, 2, context, options, 0, SelectionReason::LowestCost);

        assert_eq!(collector.stats.total_cell_decisions, 1);
    }
}
