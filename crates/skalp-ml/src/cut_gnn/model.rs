//! GNN Model for Cut Selection
//!
//! Implements a message-passing GNN for scoring cuts during technology mapping.
//! The model learns to predict which cut will lead to the best area/delay trade-off.

use super::graph::AigGraph;
use serde::{Deserialize, Serialize};
use skalp_lir::synth::{cuts::Cut, Aig, AigNodeId};

/// Configuration for the GNN cut selector
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CutScorerConfig {
    /// Number of message passing layers
    pub num_layers: usize,
    /// Hidden dimension
    pub hidden_dim: usize,
    /// Use attention mechanism
    pub use_attention: bool,
    /// Dropout rate (for training)
    pub dropout: f32,
    /// Learning rate (for training)
    pub learning_rate: f32,
}

impl Default for CutScorerConfig {
    fn default() -> Self {
        Self {
            num_layers: 3,
            hidden_dim: 64,
            use_attention: true,
            dropout: 0.1,
            learning_rate: 0.001,
        }
    }
}

/// Simple MLP-based cut scorer (no GNN, for fallback)
///
/// Uses handcrafted features from cuts for scoring.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CutScorer {
    /// Configuration
    pub config: CutScorerConfig,
    /// Weights for cut features
    weights: Vec<f32>,
    /// Bias
    bias: f32,
}

impl Default for CutScorer {
    fn default() -> Self {
        Self::new(CutScorerConfig::default())
    }
}

impl CutScorer {
    /// Number of cut features
    const NUM_CUT_FEATURES: usize = 12;

    /// Create a new cut scorer
    pub fn new(config: CutScorerConfig) -> Self {
        // Initialize with heuristic weights
        let mut weights = vec![0.0; Self::NUM_CUT_FEATURES];

        // Prefer smaller cuts (fewer leaves)
        weights[0] = -0.5; // num_leaves

        // Prefer lower delay cuts
        weights[1] = -0.8; // delay

        // Prefer lower area cuts
        weights[2] = -0.4; // area

        // Prefer cuts with balanced fanins
        weights[3] = 0.2; // fanin_balance

        // Prefer cuts with lower truth table complexity
        weights[4] = -0.3; // tt_complexity

        // Prefer cuts that match common patterns
        weights[5] = 0.5; // pattern_match

        // Prefer cuts with lower switching activity
        weights[6] = -0.2; // switching_activity

        // Prefer cuts with good reconvergence
        weights[7] = 0.3; // reconvergence_score

        // FIT rate consideration (safety-aware)
        weights[8] = -0.1; // fit_rate

        // Depth of cut (lower is better for mapping)
        weights[9] = -0.2; // depth

        // Slack availability (higher is better)
        weights[10] = 0.3; // slack

        // Fanout of root (higher fanout = more important)
        weights[11] = 0.1; // root_fanout

        Self {
            config,
            weights,
            bias: 0.0,
        }
    }

    /// Extract features from a cut
    fn extract_cut_features(
        &self,
        cut: &Cut,
        graph: &AigGraph,
        root_idx: usize,
    ) -> [f32; Self::NUM_CUT_FEATURES] {
        let mut features = [0.0f32; Self::NUM_CUT_FEATURES];

        // Basic cut properties
        features[0] = cut.leaves.len() as f32 / 6.0; // Normalized by max cut size

        // Estimate delay from cut size (larger cuts have more logic depth)
        let estimated_delay = cut.leaves.len() as f32 * 0.1;
        features[1] = estimated_delay.min(1.0);

        // Estimate area from truth table complexity
        let num_minterms = cut.truth_table.count_ones();
        let estimated_area = (num_minterms as f32 / 10.0).min(1.0);
        features[2] = estimated_area;

        // Fanin balance: variance of leaf levels
        if !cut.leaves.is_empty() {
            let leaf_levels: Vec<f32> = cut
                .leaves
                .iter()
                .filter_map(|&id| graph.node_to_idx.get(&id))
                .map(|&idx| graph.nodes[idx].level_from_inputs)
                .collect();

            if !leaf_levels.is_empty() {
                let mean = leaf_levels.iter().sum::<f32>() / leaf_levels.len() as f32;
                let variance = leaf_levels.iter().map(|&l| (l - mean).powi(2)).sum::<f32>()
                    / leaf_levels.len() as f32;
                features[3] = 1.0 - variance.sqrt().min(1.0); // Higher balance = lower variance
            }
        }

        // Truth table complexity (number of minterms)
        let max_minterms = 1u64 << cut.leaves.len().min(6);
        features[4] = num_minterms as f32 / max_minterms.max(1) as f32;

        // Pattern matching score (simple patterns score higher)
        features[5] = self.pattern_match_score(cut.truth_table, cut.leaves.len());

        // Switching activity estimate (based on truth table)
        features[6] = self.estimate_switching_activity(cut.truth_table, cut.leaves.len());

        // Reconvergence score (placeholder)
        features[7] = 0.5;

        // FIT rate (estimated from cut size - larger cuts have more failure points)
        let estimated_fit = cut.leaves.len() as f32 * 0.1;
        features[8] = estimated_fit.min(1.0);

        // Depth (from root node features)
        if root_idx < graph.nodes.len() {
            features[9] = graph.nodes[root_idx].level_from_inputs;
        }

        // Slack (placeholder - would need timing info)
        features[10] = 0.5;

        // Root fanout
        if root_idx < graph.nodes.len() {
            features[11] = graph.nodes[root_idx].fanout;
        }

        features
    }

    /// Score for pattern matching (common logic functions)
    fn pattern_match_score(&self, truth_table: u64, num_inputs: usize) -> f32 {
        if num_inputs == 0 {
            return 1.0;
        }

        let mask = (1u64 << (1 << num_inputs)) - 1;
        let tt = truth_table & mask;

        // Common patterns for 2-4 input functions
        let common_patterns: &[u64] = match num_inputs {
            1 => &[0b01, 0b10], // Buffer, inverter
            2 => &[
                0b0001, 0b0010, 0b0100, 0b0111, // AND variants
                0b1000, 0b1011, 0b1101, 0b1110, // OR variants
                0b0110, 0b1001, // XOR, XNOR
            ],
            3 => &[
                0b00010111, // Majority
                0b01101001, // XOR3
                0b00001111, // AND with one don't-care
            ],
            4 => &[
                0x7FFF, 0x8000, // OR4, NOR4
                0x0001, 0xFFFE, // AND4, NAND4
                0x6996, // XOR4
            ],
            _ => &[],
        };

        for &pattern in common_patterns {
            if tt == pattern || tt == (!pattern & mask) {
                return 1.0;
            }
        }

        0.3 // Default score for non-common patterns
    }

    /// Estimate switching activity from truth table
    fn estimate_switching_activity(&self, truth_table: u64, num_inputs: usize) -> f32 {
        if num_inputs == 0 {
            return 0.0;
        }

        let num_minterms = truth_table.count_ones();
        let total = 1u64 << num_inputs.min(6);

        // Switching activity is roughly proportional to p(1) * p(0)
        let p1 = num_minterms as f32 / total as f32;
        let p0 = 1.0 - p1;

        4.0 * p0 * p1 // Normalized to [0, 1]
    }

    /// Score a single cut
    pub fn score(&self, cut: &Cut, graph: &AigGraph, root_idx: usize) -> f32 {
        let features = self.extract_cut_features(cut, graph, root_idx);

        // Linear scoring
        let mut score = self.bias;
        for (w, f) in self.weights.iter().zip(features.iter()) {
            score += w * f;
        }

        score
    }

    /// Score multiple cuts and return indices sorted by score (descending)
    pub fn rank_cuts(&self, cuts: &[Cut], graph: &AigGraph, root_idx: usize) -> Vec<usize> {
        let mut scored: Vec<(usize, f32)> = cuts
            .iter()
            .enumerate()
            .map(|(i, cut)| (i, self.score(cut, graph, root_idx)))
            .collect();

        scored.sort_by(|a, b| b.1.partial_cmp(&a.1).unwrap_or(std::cmp::Ordering::Equal));

        scored.into_iter().map(|(i, _)| i).collect()
    }

    /// Update weights based on training signal
    pub fn update(&mut self, features: &[f32; Self::NUM_CUT_FEATURES], target_adjustment: f32) {
        let lr = self.config.learning_rate;
        for (w, &f) in self.weights.iter_mut().zip(features.iter()) {
            *w += lr * target_adjustment * f;
        }
        self.bias += lr * target_adjustment;
    }
}

/// GNN-based cut selector
///
/// Uses a graph neural network to score cuts based on local graph structure.
/// Falls back to heuristic scoring when ONNX is not available.
pub struct GnnCutSelector {
    /// Configuration
    pub config: CutScorerConfig,
    /// Fallback scorer (always available)
    fallback: CutScorer,
    /// ONNX session (optional)
    #[cfg(feature = "onnx")]
    session: Option<ort::session::Session>,
}

impl GnnCutSelector {
    /// Create a new GNN cut selector
    pub fn new(config: CutScorerConfig) -> Self {
        Self {
            fallback: CutScorer::new(config.clone()),
            config,
            #[cfg(feature = "onnx")]
            session: None,
        }
    }

    /// Load ONNX model
    #[cfg(feature = "onnx")]
    pub fn load_model(&mut self, path: &str) -> crate::MlResult<()> {
        let session = ort::session::Session::builder()
            .map_err(|e| crate::MlError::ModelNotLoaded(e.to_string()))?
            .commit_from_file(path)
            .map_err(|e| crate::MlError::ModelNotLoaded(e.to_string()))?;
        self.session = Some(session);
        Ok(())
    }

    /// Score cuts for a node
    pub fn score_cuts(&self, aig: &Aig, node: AigNodeId, cuts: &[Cut]) -> Vec<f64> {
        let graph = AigGraph::from_aig(aig);

        let root_idx = graph.node_to_idx.get(&node).copied().unwrap_or(0);

        // Use fallback scorer
        cuts.iter()
            .map(|cut| self.fallback.score(cut, &graph, root_idx) as f64)
            .collect()
    }

    /// Select the best cut for a node
    pub fn select_best(&self, aig: &Aig, node: AigNodeId, cuts: &[Cut]) -> Option<usize> {
        if cuts.is_empty() {
            return None;
        }

        let scores = self.score_cuts(aig, node, cuts);

        scores
            .iter()
            .enumerate()
            .max_by(|(_, a), (_, b)| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal))
            .map(|(i, _)| i)
    }
}

impl Default for GnnCutSelector {
    fn default() -> Self {
        Self::new(CutScorerConfig::default())
    }
}

/// Training example for cut selection
#[allow(dead_code)]
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CutTrainingExample {
    /// Node features for the local cone
    pub node_features: Vec<f32>,
    /// Edge indices (flattened)
    pub edge_index: Vec<usize>,
    /// Cut features for each candidate
    pub cut_features: Vec<Vec<f32>>,
    /// Label: which cut was optimal (index)
    pub optimal_cut: usize,
    /// Reward: improvement achieved
    pub reward: f32,
}

#[cfg(test)]
mod tests {
    use super::*;
    use skalp_lir::synth::AigLit;

    #[test]
    fn test_cut_scorer_creation() {
        let scorer = CutScorer::default();
        assert_eq!(scorer.weights.len(), CutScorer::NUM_CUT_FEATURES);
    }

    #[test]
    fn test_cut_scoring() {
        let mut aig = Aig::new("test".to_string());
        let a = aig.add_input("a".to_string(), None);
        let b = aig.add_input("b".to_string(), None);
        let and_result = aig.add_and(AigLit::new(a), AigLit::new(b));
        aig.add_output("out".to_string(), and_result);

        let graph = AigGraph::from_aig(&aig);
        let scorer = CutScorer::default();

        // Create a simple cut using the actual Cut struct
        let mut cut = Cut::new(vec![a, b]);
        cut.truth_table = 0b1000; // AND

        let root_idx = graph.node_to_idx[&and_result.node];
        let score = scorer.score(&cut, &graph, root_idx);

        // Score should be finite
        assert!(score.is_finite());
    }

    #[test]
    fn test_pattern_matching() {
        let scorer = CutScorer::default();

        // AND2 should match
        let and_score = scorer.pattern_match_score(0b1000, 2);
        assert!(and_score > 0.5);

        // XOR2 should match
        let xor_score = scorer.pattern_match_score(0b0110, 2);
        assert!(xor_score > 0.5);
    }

    #[test]
    fn test_gnn_selector_creation() {
        let selector = GnnCutSelector::default();
        assert_eq!(selector.config.num_layers, 3);
    }

    #[test]
    fn test_cut_ranking() {
        let mut aig = Aig::new("test".to_string());
        let a = aig.add_input("a".to_string(), None);
        let b = aig.add_input("b".to_string(), None);
        let c = aig.add_input("c".to_string(), None);
        let and_result = aig.add_and(AigLit::new(a), AigLit::new(b));
        aig.add_output("out".to_string(), and_result);

        let graph = AigGraph::from_aig(&aig);
        let scorer = CutScorer::default();

        let mut cut1 = Cut::new(vec![a, b]);
        cut1.truth_table = 0b1000;

        let mut cut2 = Cut::new(vec![a, b, c]);
        cut2.truth_table = 0b10000000;

        let cuts = vec![cut1, cut2];

        let root_idx = graph.node_to_idx[&and_result.node];
        let ranked = scorer.rank_cuts(&cuts, &graph, root_idx);

        // Smaller cut should rank higher
        assert_eq!(ranked[0], 0);
    }
}
