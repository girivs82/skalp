//! Policy Gradient Training Module
//!
//! Trains the pass ordering policy using collected training data.
//! Uses a simple policy gradient approach with baseline subtraction.

use crate::features::{AigFeatures, NUM_FEATURES};
use crate::pass_advisor::PassAction;
use crate::policy::{PolicyNetwork, SimplePolicy};
use crate::training_data::{PassDecision, TrainingDataset};
use serde::{Deserialize, Serialize};
use std::path::Path;

/// Training configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TrainerConfig {
    /// Learning rate for policy updates
    pub learning_rate: f64,
    /// Discount factor for rewards (gamma)
    pub discount_factor: f64,
    /// Number of training epochs
    pub epochs: usize,
    /// Batch size for mini-batch training
    pub batch_size: usize,
    /// Entropy bonus coefficient (encourages exploration)
    pub entropy_coef: f64,
    /// Gradient clipping threshold
    pub grad_clip: f64,
    /// Weight decay for regularization
    pub weight_decay: f64,
    /// Validation split ratio
    pub validation_split: f64,
    /// Early stopping patience (epochs without improvement)
    pub early_stopping_patience: usize,
}

impl Default for TrainerConfig {
    fn default() -> Self {
        Self {
            learning_rate: 0.001,
            discount_factor: 0.99,
            epochs: 100,
            batch_size: 32,
            entropy_coef: 0.01,
            grad_clip: 1.0,
            weight_decay: 0.0001,
            validation_split: 0.1,
            early_stopping_patience: 10, // Stop if no improvement for 10 epochs
        }
    }
}

/// Training statistics
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct TrainingStats {
    /// Loss per epoch
    pub losses: Vec<f64>,
    /// Validation accuracy per epoch
    pub val_accuracies: Vec<f64>,
    /// Best validation accuracy
    pub best_val_accuracy: f64,
    /// Epoch with best accuracy
    pub best_epoch: usize,
    /// Total training time in seconds
    pub training_time_secs: f64,
}

/// Experience tuple for training
#[derive(Debug, Clone)]
pub struct Experience {
    /// Features at decision time
    pub features: AigFeatures,
    /// Action taken
    pub action: PassAction,
    /// Reward received
    pub reward: f64,
    /// Discounted return (computed during training)
    pub return_value: f64,
    /// Advantage (return - baseline)
    pub advantage: f64,
}

impl From<&PassDecision> for Experience {
    fn from(decision: &PassDecision) -> Self {
        Self {
            features: decision.features_before.clone(),
            action: decision.action,
            reward: decision.reward,
            return_value: decision.reward, // Will be recomputed
            advantage: 0.0,                // Will be computed during training
        }
    }
}

/// Training mode for different data preprocessing strategies
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TrainingMode {
    /// Use discounted returns (standard policy gradient)
    Standard,
    /// Only use decisions with positive immediate rewards
    PositiveOnly,
    /// Use episode-level reward for all decisions
    EpisodeWeighted,
    /// Only train on best episodes (top 20%)
    BestEpisodes,
}

/// Policy gradient trainer
pub struct PolicyTrainer {
    config: TrainerConfig,
    policy: SimplePolicy,
    best_policy: Option<SimplePolicy>,
    stats: TrainingStats,
}

impl PolicyTrainer {
    /// Create a new trainer with default policy
    pub fn new(config: TrainerConfig) -> Self {
        Self {
            config,
            policy: SimplePolicy::new(),
            best_policy: None,
            stats: TrainingStats::default(),
        }
    }

    /// Create a trainer with an existing policy
    pub fn with_policy(config: TrainerConfig, policy: SimplePolicy) -> Self {
        Self {
            config,
            policy,
            best_policy: None,
            stats: TrainingStats::default(),
        }
    }

    /// Load training data from a directory
    pub fn load_data(path: &Path) -> std::io::Result<TrainingDataset> {
        let dataset_path = path.join("merged_dataset.json");
        let json = std::fs::read_to_string(&dataset_path)?;
        let dataset: TrainingDataset =
            serde_json::from_str(&json).map_err(|e| std::io::Error::other(e.to_string()))?;
        Ok(dataset)
    }

    /// Convert dataset to experiences
    fn dataset_to_experiences(dataset: &TrainingDataset) -> Vec<Experience> {
        let mut experiences = Vec::new();

        for episode in &dataset.episodes {
            let mut episode_experiences: Vec<Experience> = episode
                .pass_decisions
                .iter()
                .map(Experience::from)
                .collect();

            // Compute discounted returns (reverse traversal)
            let gamma = 0.99;
            let mut running_return = 0.0;
            for exp in episode_experiences.iter_mut().rev() {
                running_return = exp.reward + gamma * running_return;
                exp.return_value = running_return;
            }

            experiences.extend(episode_experiences);
        }

        experiences
    }

    /// Convert dataset to experiences, filtering for positive rewards only
    fn dataset_to_positive_experiences(dataset: &TrainingDataset) -> Vec<Experience> {
        let mut experiences = Vec::new();

        for episode in &dataset.episodes {
            for decision in &episode.pass_decisions {
                // Only include decisions that led to improvement
                if decision.reward > 0.0 {
                    let exp = Experience::from(decision);
                    experiences.push(exp);
                }
            }
        }

        // If we have very few positive examples, include neutral ones too
        if experiences.len() < 100 {
            for episode in &dataset.episodes {
                for decision in &episode.pass_decisions {
                    if decision.reward >= 0.0 {
                        let exp = Experience::from(decision);
                        experiences.push(exp);
                    }
                }
            }
        }

        experiences
    }

    /// Convert dataset using episode-level reward assignment
    /// All decisions in an episode share the episode's total reward
    /// This gives credit to passes that set up later successful passes
    fn dataset_to_episode_weighted_experiences(dataset: &TrainingDataset) -> Vec<Experience> {
        let mut experiences = Vec::new();

        for episode in &dataset.episodes {
            if episode.pass_decisions.is_empty() {
                continue;
            }

            // Compute episode total reward
            let episode_reward: f64 = episode.pass_decisions.iter().map(|d| d.reward).sum();

            // Assign episode reward to all decisions
            for decision in &episode.pass_decisions {
                let mut exp = Experience::from(decision);
                exp.return_value = episode_reward;
                experiences.push(exp);
            }
        }

        experiences
    }

    /// Convert dataset using best-episode supervised learning
    /// Only include decisions from episodes with top N% episode rewards
    fn dataset_to_best_episode_experiences(
        dataset: &TrainingDataset,
        top_percentile: f64,
    ) -> Vec<Experience> {
        // Compute episode rewards
        let mut episode_rewards: Vec<(usize, f64)> = dataset
            .episodes
            .iter()
            .enumerate()
            .map(|(i, ep)| {
                let reward: f64 = ep.pass_decisions.iter().map(|d| d.reward).sum();
                (i, reward)
            })
            .collect();

        // Sort by reward descending
        episode_rewards.sort_by(|a, b| b.1.partial_cmp(&a.1).unwrap());

        // Take top percentile
        let take_count = ((dataset.episodes.len() as f64) * top_percentile).ceil() as usize;
        let take_count = take_count.max(1);

        let mut experiences = Vec::new();
        for (idx, reward) in episode_rewards.into_iter().take(take_count) {
            let episode = &dataset.episodes[idx];
            for decision in &episode.pass_decisions {
                let mut exp = Experience::from(decision);
                exp.return_value = reward; // Use episode-level reward
                experiences.push(exp);
            }
        }

        experiences
    }

    /// Compute baseline (average return) for advantage estimation
    fn compute_baseline(experiences: &[Experience]) -> f64 {
        if experiences.is_empty() {
            return 0.0;
        }
        experiences.iter().map(|e| e.return_value).sum::<f64>() / experiences.len() as f64
    }

    /// Compute advantages using baseline subtraction
    fn compute_advantages(experiences: &mut [Experience], baseline: f64) {
        for exp in experiences.iter_mut() {
            exp.advantage = exp.return_value - baseline;
        }
    }

    /// Train the policy on the given dataset
    pub fn train(&mut self, dataset: &TrainingDataset) -> TrainingStats {
        self.train_with_mode(dataset, TrainingMode::Standard)
    }

    /// Train the policy only on positive reward examples
    pub fn train_positive_only(&mut self, dataset: &TrainingDataset) -> TrainingStats {
        self.train_with_mode(dataset, TrainingMode::PositiveOnly)
    }

    /// Train the policy using episode-level rewards
    pub fn train_episode_weighted(&mut self, dataset: &TrainingDataset) -> TrainingStats {
        self.train_with_mode(dataset, TrainingMode::EpisodeWeighted)
    }

    /// Train the policy only on best episodes
    pub fn train_best_episodes(&mut self, dataset: &TrainingDataset) -> TrainingStats {
        self.train_with_mode(dataset, TrainingMode::BestEpisodes)
    }

    /// Train the policy with the specified mode
    pub fn train_with_mode(
        &mut self,
        dataset: &TrainingDataset,
        mode: TrainingMode,
    ) -> TrainingStats {
        let start_time = std::time::Instant::now();

        // Convert to experiences based on mode
        let mut experiences = match mode {
            TrainingMode::Standard => Self::dataset_to_experiences(dataset),
            TrainingMode::PositiveOnly => Self::dataset_to_positive_experiences(dataset),
            TrainingMode::EpisodeWeighted => Self::dataset_to_episode_weighted_experiences(dataset),
            TrainingMode::BestEpisodes => Self::dataset_to_best_episode_experiences(dataset, 0.20),
        };

        if experiences.is_empty() {
            return self.stats.clone();
        }

        // Compute baseline and advantages
        let baseline = Self::compute_baseline(&experiences);
        Self::compute_advantages(&mut experiences, baseline);

        // Split into train/validation
        let val_size = (experiences.len() as f64 * self.config.validation_split) as usize;
        let train_size = experiences.len() - val_size;

        // Shuffle experiences
        use rand::seq::SliceRandom;
        let mut rng = rand::thread_rng();
        experiences.shuffle(&mut rng);

        let (train_exp, val_exp) = experiences.split_at(train_size);

        // Training loop with early stopping
        let mut epochs_without_improvement = 0;

        for epoch in 0..self.config.epochs {
            let loss = self.train_epoch(train_exp);
            self.stats.losses.push(loss);

            // Validation
            let val_accuracy = self.evaluate(val_exp);
            self.stats.val_accuracies.push(val_accuracy);

            if val_accuracy > self.stats.best_val_accuracy {
                self.stats.best_val_accuracy = val_accuracy;
                self.stats.best_epoch = epoch;
                // Save best policy
                self.best_policy = Some(self.policy.clone());
                epochs_without_improvement = 0;
            } else {
                epochs_without_improvement += 1;
            }

            // Early stopping check
            if epochs_without_improvement >= self.config.early_stopping_patience {
                break;
            }
        }

        // Restore best policy
        if let Some(best) = self.best_policy.take() {
            self.policy = best;
        }

        self.stats.training_time_secs = start_time.elapsed().as_secs_f64();

        self.stats.clone()
    }

    /// Train a single epoch
    fn train_epoch(&mut self, experiences: &[Experience]) -> f64 {
        let mut total_loss = 0.0;
        let mut batch_count = 0;

        // Process in mini-batches
        for batch in experiences.chunks(self.config.batch_size) {
            let batch_loss = self.train_batch(batch);
            total_loss += batch_loss;
            batch_count += 1;
        }

        if batch_count > 0 {
            total_loss / batch_count as f64
        } else {
            0.0
        }
    }

    /// Train on a single batch using policy gradient
    fn train_batch(&mut self, batch: &[Experience]) -> f64 {
        let mut total_loss = 0.0;

        // Compute policy gradient for each experience
        let mut weight_gradients = vec![[0.0; PassAction::NUM_ACTIONS]; NUM_FEATURES];
        let mut bias_gradients = [0.0; PassAction::NUM_ACTIONS];

        for exp in batch {
            // Get action probabilities
            let probs = self.policy.action_probs(&exp.features);
            let action_idx = exp.action.to_index();

            // Policy gradient: grad = advantage * grad(log(pi(a|s)))
            // For softmax: grad(log(pi(a))) = 1(a) - pi(a) for the action taken
            let features = exp.features.to_vector();

            for (feat_idx, &feat_val) in features.iter().enumerate() {
                for (act_idx, (&prob, grad_slot)) in probs
                    .iter()
                    .zip(weight_gradients[feat_idx].iter_mut())
                    .enumerate()
                {
                    let indicator = if act_idx == action_idx { 1.0 } else { 0.0 };
                    let grad = (indicator - prob) * feat_val;
                    *grad_slot += exp.advantage * grad;
                }
            }

            for (act_idx, (&prob, grad_slot)) in
                probs.iter().zip(bias_gradients.iter_mut()).enumerate()
            {
                let indicator = if act_idx == action_idx { 1.0 } else { 0.0 };
                let grad = indicator - prob;
                *grad_slot += exp.advantage * grad;
            }

            // Compute loss: -log(pi(a|s)) * advantage
            let log_prob = probs[action_idx].max(1e-10).ln();
            total_loss -= log_prob * exp.advantage;

            // Entropy bonus: sum(pi * log(pi))
            let entropy: f64 = probs.iter().map(|&p| -p * p.max(1e-10).ln()).sum();
            total_loss -= self.config.entropy_coef * entropy;
        }

        let batch_size = batch.len() as f64;
        let lr = self.config.learning_rate;

        // Apply gradients with clipping
        for (feat_idx, feat_grads) in weight_gradients.iter().enumerate() {
            for (act_idx, &grad_val) in feat_grads.iter().enumerate() {
                let grad = grad_val / batch_size;
                let clipped_grad = grad.clamp(-self.config.grad_clip, self.config.grad_clip);

                // Get current weight
                let current = self.policy.weights()[feat_idx][act_idx];
                // Apply gradient update with weight decay
                let new_weight =
                    current + lr * clipped_grad - lr * self.config.weight_decay * current;
                self.policy.set_weight(feat_idx, act_idx, new_weight);
            }
        }

        for (act_idx, &grad_val) in bias_gradients.iter().enumerate() {
            let grad = grad_val / batch_size;
            let clipped_grad = grad.clamp(-self.config.grad_clip, self.config.grad_clip);

            let current = self.policy.biases()[act_idx];
            let new_bias = current + lr * clipped_grad;
            self.policy.set_bias(act_idx, new_bias);
        }

        total_loss / batch_size
    }

    /// Evaluate policy on validation set
    fn evaluate(&self, experiences: &[Experience]) -> f64 {
        if experiences.is_empty() {
            return 0.0;
        }

        let mut correct = 0;
        let mut total = 0;

        for exp in experiences {
            let predicted = self.policy.predict(&exp.features);
            if predicted == exp.action {
                correct += 1;
            }
            total += 1;
        }

        correct as f64 / total as f64
    }

    /// Get the trained policy
    pub fn policy(&self) -> &SimplePolicy {
        &self.policy
    }

    /// Take ownership of the trained policy
    pub fn into_policy(self) -> SimplePolicy {
        self.policy
    }

    /// Get training statistics
    pub fn stats(&self) -> &TrainingStats {
        &self.stats
    }

    /// Save trained policy to file
    pub fn save_policy(&self, path: &str) -> std::io::Result<()> {
        self.policy.save(path)
    }

    /// Save training statistics
    pub fn save_stats(&self, path: &str) -> std::io::Result<()> {
        let json = serde_json::to_string_pretty(&self.stats)?;
        std::fs::write(path, json)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::training_data::{
        CollectorConfig, DatasetMetadata, DatasetStats, PassResultSummary, QualityOfResult,
        SynthesisEpisode,
    };

    fn create_test_dataset() -> TrainingDataset {
        // Create a simple episode with some decisions
        let mut episode = SynthesisEpisode {
            episode_id: "test_ep_1".to_string(),
            design_name: "test_design".to_string(),
            library_name: "7nm".to_string(),
            initial_features: AigFeatures::default(),
            final_features: AigFeatures::default(),
            pass_decisions: Vec::new(),
            cut_decisions: Vec::new(),
            cell_decisions: Vec::new(),
            final_qor: QualityOfResult {
                cell_count: 100,
                and_equivalent: 50,
                critical_delay: 10.0,
                total_area: 1000.0,
                inverter_count: 10,
                buffer_count: 5,
                cell_distribution: std::collections::HashMap::new(),
                improvement: 0.0,
            },
            total_reward: 0.0,
            duration_ms: 100,
        };

        // Add some pass decisions
        for i in 0..10 {
            let action = if i % 2 == 0 {
                PassAction::Rewrite
            } else {
                PassAction::Balance
            };
            let decision = PassDecision {
                step: i,
                features_before: AigFeatures::default(),
                action,
                action_probs: None,
                pass_result: PassResultSummary {
                    pass_name: format!("{:?}", action),
                    ands_before: 100,
                    ands_after: 95,
                    levels_before: 10,
                    levels_after: 10,
                    duration_us: 100,
                },
                reward: if i % 2 == 0 { 0.5 } else { -0.1 },
                features_after: AigFeatures::default(),
            };
            episode.pass_decisions.push(decision);
        }

        TrainingDataset {
            metadata: DatasetMetadata {
                version: "1.0".to_string(),
                created_at: "2024-01-01T00:00:00Z".to_string(),
                num_designs: 1,
                libraries: vec!["7nm".to_string()],
                config: CollectorConfig::default(),
            },
            episodes: vec![episode],
            stats: DatasetStats::default(),
        }
    }

    #[test]
    fn test_trainer_creation() {
        let config = TrainerConfig::default();
        let trainer = PolicyTrainer::new(config);
        assert_eq!(trainer.stats().losses.len(), 0);
    }

    #[test]
    fn test_dataset_to_experiences() {
        let dataset = create_test_dataset();
        let experiences = PolicyTrainer::dataset_to_experiences(&dataset);

        assert_eq!(experiences.len(), 10);
        assert!(experiences.iter().all(|e| e.return_value.is_finite()));
    }

    #[test]
    fn test_compute_baseline() {
        let dataset = create_test_dataset();
        let experiences = PolicyTrainer::dataset_to_experiences(&dataset);
        let baseline = PolicyTrainer::compute_baseline(&experiences);

        assert!(baseline.is_finite());
    }

    #[test]
    fn test_training_loop() {
        let config = TrainerConfig {
            epochs: 5,
            batch_size: 4,
            ..Default::default()
        };
        let mut trainer = PolicyTrainer::new(config);
        let dataset = create_test_dataset();

        let stats = trainer.train(&dataset);

        assert_eq!(stats.losses.len(), 5);
        assert!(stats.losses.iter().all(|&l| l.is_finite()));
    }
}
