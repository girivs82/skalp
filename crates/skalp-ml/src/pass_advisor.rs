//! Pass Ordering Advisor
//!
//! Provides ML-guided selection of optimization passes based on
//! the current state of the AIG.
//!
//! # Approach
//!
//! Uses a policy network (or heuristic fallback) to select the next
//! optimization pass based on extracted features. The policy can be:
//!
//! 1. **Learned**: Neural network trained with PPO/A2C
//! 2. **Heuristic**: Rule-based selection as fallback
//!
//! # Training
//!
//! The policy is trained offline using reinforcement learning:
//! - State: AIG features (32-dimensional vector)
//! - Action: Which pass to run next (8 discrete actions)
//! - Reward: Improvement in area/delay cost

use crate::features::AigFeatures;
use crate::policy::{PolicyNetwork, SimplePolicy};
use serde::{Deserialize, Serialize};

#[cfg(feature = "onnx")]
use crate::MlResult;

/// Available pass actions
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum PassAction {
    /// Terminate optimization
    Terminate,
    /// Run AIG rewriting
    Rewrite,
    /// Run cone refactoring
    Refactor,
    /// Run tree balancing
    Balance,
    /// Run structural hashing
    Strash,
    /// Run functional reduction (FRAIG)
    Fraig,
    /// Run dead code elimination
    Dce,
    /// Run constant propagation
    ConstProp,
}

impl PassAction {
    /// Number of possible actions
    pub const NUM_ACTIONS: usize = 8;

    /// Convert to action index
    pub fn to_index(&self) -> usize {
        match self {
            PassAction::Terminate => 0,
            PassAction::Rewrite => 1,
            PassAction::Refactor => 2,
            PassAction::Balance => 3,
            PassAction::Strash => 4,
            PassAction::Fraig => 5,
            PassAction::Dce => 6,
            PassAction::ConstProp => 7,
        }
    }

    /// Convert from action index
    pub fn from_index(idx: usize) -> Self {
        match idx {
            0 => PassAction::Terminate,
            1 => PassAction::Rewrite,
            2 => PassAction::Refactor,
            3 => PassAction::Balance,
            4 => PassAction::Strash,
            5 => PassAction::Fraig,
            6 => PassAction::Dce,
            7 => PassAction::ConstProp,
            _ => PassAction::Terminate,
        }
    }

    /// Get all non-terminate actions
    pub fn all_passes() -> Vec<Self> {
        vec![
            PassAction::Rewrite,
            PassAction::Refactor,
            PassAction::Balance,
            PassAction::Strash,
            PassAction::Fraig,
            PassAction::Dce,
            PassAction::ConstProp,
        ]
    }

    /// Default optimization sequence (heuristic)
    pub fn default_sequence() -> Self {
        // Start with structural cleanup
        PassAction::Strash
    }

    /// Get pass name
    pub fn name(&self) -> &'static str {
        match self {
            PassAction::Terminate => "terminate",
            PassAction::Rewrite => "rewrite",
            PassAction::Refactor => "refactor",
            PassAction::Balance => "balance",
            PassAction::Strash => "strash",
            PassAction::Fraig => "fraig",
            PassAction::Dce => "dce",
            PassAction::ConstProp => "const_prop",
        }
    }
}

/// Configuration for pass advisor
#[derive(Debug, Clone)]
pub struct PassAdvisorConfig {
    /// Use learned policy (vs heuristic)
    pub use_learned_policy: bool,
    /// Exploration rate (epsilon for epsilon-greedy)
    pub exploration_rate: f64,
    /// Maximum consecutive no-improvement passes before termination
    pub max_no_improvement: usize,
    /// Minimum improvement threshold to continue
    pub min_improvement: f64,
}

impl Default for PassAdvisorConfig {
    fn default() -> Self {
        Self {
            use_learned_policy: true,
            exploration_rate: 0.0,
            max_no_improvement: 3,
            min_improvement: 0.001, // 0.1% improvement threshold
        }
    }
}

/// ML-guided pass ordering advisor
pub struct MlPassAdvisor {
    config: PassAdvisorConfig,
    policy: SimplePolicy,
    /// History of recent improvements
    improvement_history: Vec<f64>,
    /// Previous cost for improvement tracking
    prev_cost: Option<f64>,
    /// Count of passes with no improvement
    no_improvement_count: usize,
}

impl MlPassAdvisor {
    /// Create a new pass advisor
    pub fn new(config: PassAdvisorConfig) -> Self {
        Self {
            policy: SimplePolicy::new(),
            config,
            improvement_history: Vec::new(),
            prev_cost: None,
            no_improvement_count: 0,
        }
    }

    /// Suggest the next action based on current features
    pub fn suggest_action(&mut self, features: &AigFeatures) -> PassAction {
        let current_cost = features.estimated_cost();

        // Check for improvement
        if let Some(prev) = self.prev_cost {
            let improvement = (prev - current_cost) / prev.max(1.0);
            self.improvement_history.push(improvement);

            if improvement < self.config.min_improvement {
                self.no_improvement_count += 1;
            } else {
                self.no_improvement_count = 0;
            }

            // Terminate if no improvement for too long
            if self.no_improvement_count >= self.config.max_no_improvement {
                return PassAction::Terminate;
            }
        }
        self.prev_cost = Some(current_cost);

        // Exploration (for training)
        if self.config.exploration_rate > 0.0
            && rand::random::<f64>() < self.config.exploration_rate
        {
            return self.random_action();
        }

        // Get action from policy
        if self.config.use_learned_policy {
            self.policy.predict(features)
        } else {
            self.heuristic_action(features)
        }
    }

    /// Random action for exploration
    fn random_action(&self) -> PassAction {
        let actions = PassAction::all_passes();
        let idx = rand::random::<usize>() % actions.len();
        actions[idx]
    }

    /// Heuristic action selection (fallback)
    fn heuristic_action(&self, features: &AigFeatures) -> PassAction {
        // Simple rule-based policy

        // If there's high optimization potential, try aggressive passes
        if features.optimization_potential() > 0.3 {
            // High fanout -> balance/buffer
            if features.max_fanout > 20 {
                return PassAction::Balance;
            }

            // Deep logic -> balance
            if features.max_level > 30 {
                return PassAction::Balance;
            }

            // Many nodes -> try rewriting
            if features.and_count > 100 {
                return PassAction::Rewrite;
            }
        }

        // Medium potential -> standard cleanup
        if features.optimization_potential() > 0.1 {
            // Structural cleanup first
            if self.improvement_history.len() < 2 {
                return PassAction::Strash;
            }

            // Then constant propagation
            if self.improvement_history.len() < 4 {
                return PassAction::ConstProp;
            }

            // Then DCE
            if self.improvement_history.len() < 6 {
                return PassAction::Dce;
            }

            // Try rewriting
            return PassAction::Rewrite;
        }

        // Low potential -> try FRAIG for functional reduction
        if features.and_count > 50 {
            return PassAction::Fraig;
        }

        // Otherwise terminate
        PassAction::Terminate
    }

    /// Load a policy model from file
    #[cfg(feature = "onnx")]
    pub fn load_model(&mut self, path: &str) -> MlResult<()> {
        self.policy.load(path)
    }

    /// Reset state for new optimization run
    pub fn reset(&mut self) {
        self.improvement_history.clear();
        self.prev_cost = None;
        self.no_improvement_count = 0;
    }

    /// Get improvement history
    pub fn improvement_history(&self) -> &[f64] {
        &self.improvement_history
    }

    /// Record a reward for training
    pub fn record_reward(&mut self, action: PassAction, reward: f64) {
        self.policy.record_experience(action, reward);
    }
}

/// Training data for offline learning
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TrainingExample {
    /// Features before action
    pub state: Vec<f64>,
    /// Action taken
    pub action: usize,
    /// Reward received
    pub reward: f64,
    /// Features after action
    pub next_state: Vec<f64>,
    /// Whether this was a terminal state
    pub done: bool,
}

impl TrainingExample {
    /// Create a new training example
    pub fn new(
        state: &AigFeatures,
        action: PassAction,
        reward: f64,
        next_state: &AigFeatures,
        done: bool,
    ) -> Self {
        Self {
            state: state.to_vector().to_vec(),
            action: action.to_index(),
            reward,
            next_state: next_state.to_vector().to_vec(),
            done,
        }
    }
}

/// Training data collector
#[derive(Debug, Clone, Default)]
pub struct TrainingCollector {
    examples: Vec<TrainingExample>,
}

impl TrainingCollector {
    /// Create a new collector
    pub fn new() -> Self {
        Self {
            examples: Vec::new(),
        }
    }

    /// Add a training example
    pub fn add(&mut self, example: TrainingExample) {
        self.examples.push(example);
    }

    /// Get all examples
    pub fn examples(&self) -> &[TrainingExample] {
        &self.examples
    }

    /// Save to file
    pub fn save(&self, path: &str) -> std::io::Result<()> {
        let json = serde_json::to_string_pretty(&self.examples)?;
        std::fs::write(path, json)
    }

    /// Load from file
    pub fn load(path: &str) -> std::io::Result<Self> {
        let json = std::fs::read_to_string(path)?;
        let examples: Vec<TrainingExample> = serde_json::from_str(&json)?;
        Ok(Self { examples })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_pass_action_conversion() {
        for i in 0..PassAction::NUM_ACTIONS {
            let action = PassAction::from_index(i);
            assert_eq!(action.to_index(), i);
        }
    }

    #[test]
    fn test_pass_advisor_creation() {
        let advisor = MlPassAdvisor::new(PassAdvisorConfig::default());
        assert!(advisor.improvement_history().is_empty());
    }

    #[test]
    fn test_pass_advisor_suggest() {
        let mut advisor = MlPassAdvisor::new(PassAdvisorConfig::default());
        let features = AigFeatures::default();

        let action = advisor.suggest_action(&features);
        // Should not crash and return a valid action
        assert!(action.to_index() < PassAction::NUM_ACTIONS);
    }

    #[test]
    fn test_pass_advisor_termination() {
        let config = PassAdvisorConfig {
            max_no_improvement: 2,
            min_improvement: 0.1,
            ..Default::default()
        };
        let mut advisor = MlPassAdvisor::new(config);

        // Simulate no improvement
        let features = AigFeatures::default();
        advisor.suggest_action(&features);
        advisor.suggest_action(&features);
        let action = advisor.suggest_action(&features);

        // Should terminate after max_no_improvement
        assert_eq!(action, PassAction::Terminate);
    }

    #[test]
    fn test_training_example() {
        let state = AigFeatures::default();
        let next_state = AigFeatures::default();

        let example = TrainingExample::new(&state, PassAction::Rewrite, 0.5, &next_state, false);

        assert_eq!(example.action, PassAction::Rewrite.to_index());
        assert_eq!(example.reward, 0.5);
        assert!(!example.done);
    }
}
