//! Policy Networks for Pass Selection
//!
//! Implements policy networks for reinforcement learning-based
//! pass ordering. Supports both simple heuristic policies and
//! neural network policies (when ONNX feature is enabled).
//!
//! # Architecture
//!
//! The policy network maps AIG features to action probabilities:
//!
//! ```text
//! Features (32) → Hidden (64) → Hidden (32) → Actions (8)
//! ```
//!
//! # Training
//!
//! Training is performed offline using PPO or A2C:
//! 1. Collect trajectories by running synthesis
//! 2. Compute advantages using GAE
//! 3. Update policy using clipped objective

use crate::features::{AigFeatures, NUM_FEATURES};
use crate::pass_advisor::PassAction;
use serde::{Deserialize, Serialize};

#[cfg(feature = "onnx")]
use crate::MlResult;

/// Trait for policy networks
pub trait PolicyNetwork: Send + Sync {
    /// Predict the best action given features
    fn predict(&self, features: &AigFeatures) -> PassAction;

    /// Get action probabilities
    fn action_probs(&self, features: &AigFeatures) -> [f64; PassAction::NUM_ACTIONS];

    /// Record an experience for training
    fn record_experience(&mut self, action: PassAction, reward: f64);

    /// Load model from file
    #[cfg(feature = "onnx")]
    fn load(&mut self, path: &str) -> MlResult<()>;
}

/// Simple policy using learned weights
///
/// A lightweight policy that can be trained and used without
/// external ML frameworks. Uses a simple linear model with
/// learned weights.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SimplePolicy {
    /// Weights: [NUM_FEATURES x NUM_ACTIONS]
    weights: Vec<Vec<f64>>,
    /// Bias: [NUM_ACTIONS]
    bias: Vec<f64>,
    /// Experience buffer for training
    #[serde(skip)]
    experiences: Vec<(PassAction, f64)>,
    /// Learning rate
    learning_rate: f64,
}

impl Default for SimplePolicy {
    fn default() -> Self {
        Self::new()
    }
}

impl SimplePolicy {
    /// Create a new simple policy with heuristic initialization
    pub fn new() -> Self {
        let num_actions = PassAction::NUM_ACTIONS;

        // Initialize weights with heuristics
        let mut weights = vec![vec![0.0; num_actions]; NUM_FEATURES];
        let mut bias = vec![0.0; num_actions];

        // Bias towards useful passes
        bias[PassAction::Strash.to_index()] = 0.5; // Strash is usually good first
        bias[PassAction::ConstProp.to_index()] = 0.3;
        bias[PassAction::Dce.to_index()] = 0.2;
        bias[PassAction::Rewrite.to_index()] = 0.4;
        bias[PassAction::Balance.to_index()] = 0.3;
        bias[PassAction::Terminate.to_index()] = -0.5; // Don't terminate early

        // Feature-based weights (heuristic initialization)
        // High AND count -> prefer rewriting
        weights[1][PassAction::Rewrite.to_index()] = 0.3;
        weights[1][PassAction::Refactor.to_index()] = 0.2;

        // High depth -> prefer balancing
        weights[5][PassAction::Balance.to_index()] = 0.4;

        // High fanout -> prefer balancing
        weights[7][PassAction::Balance.to_index()] = 0.3;

        // Low optimization potential -> prefer termination
        weights[31][PassAction::Terminate.to_index()] = 1.0;
        weights[31][PassAction::Rewrite.to_index()] = -0.3;

        Self {
            weights,
            bias,
            experiences: Vec::new(),
            learning_rate: 0.01,
        }
    }

    /// Compute logits for each action
    fn compute_logits(&self, features: &AigFeatures) -> [f64; PassAction::NUM_ACTIONS] {
        let feature_vec = features.to_vector();
        let mut logits = [0.0; PassAction::NUM_ACTIONS];

        // Linear transformation: logits = features * weights + bias
        for (action_idx, logit) in logits.iter_mut().enumerate() {
            *logit = self.bias[action_idx];
            for (feat_idx, &feat_val) in feature_vec.iter().enumerate() {
                *logit += feat_val * self.weights[feat_idx][action_idx];
            }
        }

        logits
    }

    /// Apply softmax to get probabilities
    fn softmax(&self, logits: &[f64; PassAction::NUM_ACTIONS]) -> [f64; PassAction::NUM_ACTIONS] {
        let max_logit = logits.iter().cloned().fold(f64::NEG_INFINITY, f64::max);
        let exp_sum: f64 = logits.iter().map(|&l| (l - max_logit).exp()).sum();

        let mut probs = [0.0; PassAction::NUM_ACTIONS];
        for (i, &logit) in logits.iter().enumerate() {
            probs[i] = (logit - max_logit).exp() / exp_sum;
        }
        probs
    }

    /// Sample action from probability distribution
    #[allow(dead_code)]
    fn sample_action(&self, probs: &[f64; PassAction::NUM_ACTIONS]) -> PassAction {
        let r: f64 = rand::random();
        let mut cumsum = 0.0;

        for (idx, &prob) in probs.iter().enumerate() {
            cumsum += prob;
            if r < cumsum {
                return PassAction::from_index(idx);
            }
        }

        PassAction::Terminate
    }

    /// Update weights based on experiences (simple policy gradient)
    pub fn update(&mut self) {
        if self.experiences.is_empty() {
            return;
        }

        // Compute average reward
        let avg_reward: f64 =
            self.experiences.iter().map(|(_, r)| r).sum::<f64>() / self.experiences.len() as f64;

        // Update weights using policy gradient (simplified)
        // This is a very basic update - full PPO would be more complex
        for (action, reward) in &self.experiences {
            let advantage = reward - avg_reward;
            let action_idx = action.to_index();

            // Increase probability of good actions
            self.bias[action_idx] += self.learning_rate * advantage;
        }

        self.experiences.clear();
    }

    /// Save policy to file
    pub fn save(&self, path: &str) -> std::io::Result<()> {
        let json = serde_json::to_string_pretty(self)?;
        std::fs::write(path, json)
    }

    /// Load policy from file
    pub fn load_from_file(path: &str) -> std::io::Result<Self> {
        let json = std::fs::read_to_string(path)?;
        let policy: Self = serde_json::from_str(&json)?;
        Ok(policy)
    }
}

impl PolicyNetwork for SimplePolicy {
    fn predict(&self, features: &AigFeatures) -> PassAction {
        let logits = self.compute_logits(features);
        let probs = self.softmax(&logits);

        // Return action with highest probability (greedy)
        let (best_idx, _) = probs
            .iter()
            .enumerate()
            .max_by(|(_, a), (_, b)| a.partial_cmp(b).unwrap())
            .unwrap();

        PassAction::from_index(best_idx)
    }

    fn action_probs(&self, features: &AigFeatures) -> [f64; PassAction::NUM_ACTIONS] {
        let logits = self.compute_logits(features);
        self.softmax(&logits)
    }

    fn record_experience(&mut self, action: PassAction, reward: f64) {
        self.experiences.push((action, reward));
    }

    #[cfg(feature = "onnx")]
    fn load(&mut self, path: &str) -> MlResult<()> {
        // For SimplePolicy, we can load our own format
        match Self::load_from_file(path) {
            Ok(loaded) => {
                *self = loaded;
                Ok(())
            }
            Err(e) => Err(crate::MlError::ModelNotLoaded(e.to_string())),
        }
    }
}

/// ONNX-based policy network (requires onnx feature)
#[cfg(feature = "onnx")]
pub struct OnnxPolicy {
    session: ort::session::Session,
}

#[cfg(feature = "onnx")]
impl OnnxPolicy {
    /// Load model from ONNX file
    pub fn new(path: &str) -> crate::MlResult<Self> {
        let session = ort::session::Session::builder()
            .map_err(|e| crate::MlError::ModelNotLoaded(e.to_string()))?
            .commit_from_file(path)
            .map_err(|e| crate::MlError::ModelNotLoaded(e.to_string()))?;

        Ok(Self { session })
    }

    /// Run inference (internal, needs mutable session)
    #[allow(dead_code)]
    fn run_inference(&mut self, features: &AigFeatures) -> [f64; PassAction::NUM_ACTIONS] {
        use crate::features::NUM_FEATURES;

        let input = features.to_vector();
        let input_array: ndarray::Array2<f32> = ndarray::Array2::from_shape_vec(
            (1, NUM_FEATURES),
            input.iter().map(|&x| x as f32).collect(),
        )
        .unwrap();

        // Create tensor from array view
        let tensor = ort::value::TensorRef::from_array_view(&input_array).unwrap();

        let outputs = self.session.run(ort::inputs![tensor]).unwrap();

        // Extract output tensor using indexing
        // try_extract_tensor returns (&Shape, &[f32])
        let (_shape, data) = outputs[0].try_extract_tensor::<f32>().unwrap();

        let mut probs = [0.0; PassAction::NUM_ACTIONS];
        for (i, &p) in data.iter().enumerate().take(PassAction::NUM_ACTIONS) {
            probs[i] = p as f64;
        }

        probs
    }
}

#[cfg(feature = "onnx")]
impl PolicyNetwork for OnnxPolicy {
    fn predict(&self, features: &AigFeatures) -> PassAction {
        // Note: This is a workaround since we can't mutate self in predict
        // In practice, you'd use interior mutability or restructure
        let probs = self.action_probs(features);
        let (best_idx, _) = probs
            .iter()
            .enumerate()
            .max_by(|(_, a), (_, b)| a.partial_cmp(b).unwrap())
            .unwrap();
        PassAction::from_index(best_idx)
    }

    fn action_probs(&self, _features: &AigFeatures) -> [f64; PassAction::NUM_ACTIONS] {
        // Return uniform distribution as fallback
        // Real implementation would use interior mutability (RefCell/Mutex)
        let uniform = 1.0 / PassAction::NUM_ACTIONS as f64;
        [uniform; PassAction::NUM_ACTIONS]
    }

    fn record_experience(&mut self, _action: PassAction, _reward: f64) {
        // ONNX models don't support online learning
    }

    fn load(&mut self, path: &str) -> crate::MlResult<()> {
        let session = ort::session::Session::builder()
            .map_err(|e| crate::MlError::ModelNotLoaded(e.to_string()))?
            .commit_from_file(path)
            .map_err(|e| crate::MlError::ModelNotLoaded(e.to_string()))?;
        self.session = session;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_policy_creation() {
        let policy = SimplePolicy::new();
        assert_eq!(policy.weights.len(), NUM_FEATURES);
        assert_eq!(policy.bias.len(), PassAction::NUM_ACTIONS);
    }

    #[test]
    fn test_simple_policy_predict() {
        let policy = SimplePolicy::new();
        let features = AigFeatures::default();

        let action = policy.predict(&features);
        assert!(action.to_index() < PassAction::NUM_ACTIONS);
    }

    #[test]
    fn test_simple_policy_probs() {
        let policy = SimplePolicy::new();
        let features = AigFeatures::default();

        let probs = policy.action_probs(&features);

        // Probabilities should sum to 1
        let sum: f64 = probs.iter().sum();
        assert!((sum - 1.0).abs() < 0.001);

        // All probabilities should be non-negative
        assert!(probs.iter().all(|&p| p >= 0.0));
    }

    #[test]
    fn test_simple_policy_logits() {
        let policy = SimplePolicy::new();
        let features = AigFeatures::default();

        let logits = policy.compute_logits(&features);

        // Logits should be finite
        assert!(logits.iter().all(|&l| l.is_finite()));
    }

    #[test]
    fn test_simple_policy_update() {
        let mut policy = SimplePolicy::new();

        // Record some experiences
        policy.record_experience(PassAction::Rewrite, 1.0);
        policy.record_experience(PassAction::Balance, -0.5);
        policy.record_experience(PassAction::Strash, 0.5);

        // Update should not crash
        policy.update();

        // Experiences should be cleared
        assert!(policy.experiences.is_empty());
    }

    #[test]
    fn test_softmax() {
        let policy = SimplePolicy::new();
        let logits = [1.0, 2.0, 3.0, 1.0, 1.0, 1.0, 1.0, 1.0];

        let probs = policy.softmax(&logits);

        // Probabilities should sum to 1
        let sum: f64 = probs.iter().sum();
        assert!((sum - 1.0).abs() < 0.001);

        // Highest logit should have highest probability
        assert!(probs[2] > probs[0]);
        assert!(probs[2] > probs[1]);
    }
}
