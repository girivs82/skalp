//! Architecture Classifier
//!
//! MLP-based classifier for selecting datapath architectures.

use serde::{Deserialize, Serialize};

/// Type of datapath operation
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum OperationType {
    /// Addition/subtraction
    Add,
    /// Multiplication
    Multiply,
    /// Division
    Divide,
    /// Shift (left/right)
    Shift,
    /// Comparison
    Compare,
}

/// Available datapath architectures
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum DatapathArchitecture {
    // Adder architectures
    /// Ripple-carry adder (simple, slow)
    RippleCarry,
    /// Carry-lookahead adder (medium complexity)
    CarryLookahead,
    /// Kogge-Stone parallel prefix (fast, large area)
    KoggeStone,
    /// Brent-Kung parallel prefix (balanced)
    BrentKung,
    /// Sklansky parallel prefix (compromise)
    Sklansky,

    // Multiplier architectures
    /// Array multiplier (simple)
    ArrayMult,
    /// Wallace tree multiplier (fast)
    WallaceTree,
    /// Booth-encoded multiplier (efficient for signed)
    BoothMult,
    /// Dadda tree multiplier (variant of Wallace)
    DaddaTree,

    // Shifter architectures
    /// Barrel shifter (fast, parallel)
    BarrelShifter,
    /// Logarithmic shifter (area efficient)
    LogShifter,

    // Comparator architectures
    /// Ripple comparator
    RippleCompare,
    /// Tree comparator
    TreeCompare,
}

impl DatapathArchitecture {
    /// Get all adder architectures
    pub fn adder_architectures() -> Vec<Self> {
        vec![
            Self::RippleCarry,
            Self::CarryLookahead,
            Self::KoggeStone,
            Self::BrentKung,
            Self::Sklansky,
        ]
    }

    /// Get all multiplier architectures
    pub fn multiplier_architectures() -> Vec<Self> {
        vec![
            Self::ArrayMult,
            Self::WallaceTree,
            Self::BoothMult,
            Self::DaddaTree,
        ]
    }

    /// Get all shifter architectures
    pub fn shifter_architectures() -> Vec<Self> {
        vec![Self::BarrelShifter, Self::LogShifter]
    }

    /// Get estimated area factor (relative to RippleCarry)
    pub fn area_factor(&self) -> f32 {
        match self {
            // Adders
            Self::RippleCarry => 1.0,
            Self::CarryLookahead => 1.5,
            Self::KoggeStone => 2.5,
            Self::BrentKung => 2.0,
            Self::Sklansky => 2.2,

            // Multipliers
            Self::ArrayMult => 1.0,
            Self::WallaceTree => 1.3,
            Self::BoothMult => 1.4,
            Self::DaddaTree => 1.25,

            // Shifters
            Self::BarrelShifter => 1.5,
            Self::LogShifter => 1.0,

            // Comparators
            Self::RippleCompare => 1.0,
            Self::TreeCompare => 1.3,
        }
    }

    /// Get estimated delay factor (relative to RippleCarry, lower is faster)
    pub fn delay_factor(&self) -> f32 {
        match self {
            // Adders
            Self::RippleCarry => 1.0,
            Self::CarryLookahead => 0.5,
            Self::KoggeStone => 0.3,
            Self::BrentKung => 0.35,
            Self::Sklansky => 0.32,

            // Multipliers
            Self::ArrayMult => 1.0,
            Self::WallaceTree => 0.6,
            Self::BoothMult => 0.7,
            Self::DaddaTree => 0.65,

            // Shifters
            Self::BarrelShifter => 0.3,
            Self::LogShifter => 0.5,

            // Comparators
            Self::RippleCompare => 1.0,
            Self::TreeCompare => 0.5,
        }
    }
}

/// Configuration for architecture advisor
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ArchAdvisorConfig {
    /// Weight for area in cost function (0-1)
    pub area_weight: f32,
    /// Weight for delay in cost function (0-1)
    pub delay_weight: f32,
    /// Use ML model for selection
    pub use_ml_model: bool,
    /// Path to ONNX model
    pub model_path: Option<String>,
}

impl Default for ArchAdvisorConfig {
    fn default() -> Self {
        Self {
            area_weight: 0.5,
            delay_weight: 0.5,
            use_ml_model: false,
            model_path: None,
        }
    }
}

impl ArchAdvisorConfig {
    /// Create config optimized for area
    pub fn area_optimized() -> Self {
        Self {
            area_weight: 0.8,
            delay_weight: 0.2,
            ..Default::default()
        }
    }

    /// Create config optimized for delay
    pub fn delay_optimized() -> Self {
        Self {
            area_weight: 0.2,
            delay_weight: 0.8,
            ..Default::default()
        }
    }
}

/// Features for architecture selection
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ArchFeatures {
    /// Bitwidth of operation
    pub bitwidth: u32,
    /// Is operation signed
    pub is_signed: bool,
    /// Available timing slack (normalized)
    pub slack: f32,
    /// Area budget (normalized)
    pub area_budget: f32,
    /// Power budget (normalized)
    pub power_budget: f32,
    /// Number of such operations in design
    pub operation_count: usize,
    /// Is on critical path
    pub on_critical_path: bool,
    /// Frequency constraint (normalized)
    pub frequency: f32,
}

impl Default for ArchFeatures {
    fn default() -> Self {
        Self {
            bitwidth: 32,
            is_signed: true,
            slack: 0.5,
            area_budget: 0.5,
            power_budget: 0.5,
            operation_count: 1,
            on_critical_path: false,
            frequency: 0.5,
        }
    }
}

impl ArchFeatures {
    /// Number of features
    pub const NUM_FEATURES: usize = 8;

    /// Convert to feature vector
    pub fn to_vector(&self) -> [f32; Self::NUM_FEATURES] {
        [
            (self.bitwidth as f32 / 64.0).min(1.0), // Normalized bitwidth
            if self.is_signed { 1.0 } else { 0.0 },
            self.slack,
            self.area_budget,
            self.power_budget,
            (self.operation_count as f32 / 100.0).min(1.0),
            if self.on_critical_path { 1.0 } else { 0.0 },
            self.frequency,
        ]
    }
}

/// Architecture advisor using MLP classifier
#[derive(Debug, Clone)]
pub struct ArchAdvisor {
    /// Configuration
    pub config: ArchAdvisorConfig,
    /// Weights for each architecture (learned)
    weights: Vec<Vec<f32>>,
    /// Bias for each architecture
    bias: Vec<f32>,
}

impl Default for ArchAdvisor {
    fn default() -> Self {
        Self::new(ArchAdvisorConfig::default())
    }
}

impl ArchAdvisor {
    /// Create a new architecture advisor
    pub fn new(config: ArchAdvisorConfig) -> Self {
        // Initialize with heuristic weights
        let num_archs = 5; // For adders (main use case)
        let num_features = ArchFeatures::NUM_FEATURES;

        let mut weights = vec![vec![0.0; num_features]; num_archs];
        let mut bias = vec![0.0; num_archs];

        // RippleCarry: prefer for small bitwidth, high area budget
        weights[0][0] = -0.5; // Negative for bitwidth (prefer small)
        weights[0][2] = -0.3; // Negative for slack (works with little slack)
        weights[0][3] = 0.5; // Positive for area budget
        bias[0] = 0.3;

        // CarryLookahead: balanced
        weights[1][0] = 0.2;
        weights[1][2] = 0.3;
        weights[1][3] = 0.2;
        bias[1] = 0.2;

        // KoggeStone: prefer for timing-critical, large bitwidth
        weights[2][0] = 0.5; // Positive for bitwidth
        weights[2][2] = 0.5; // Positive for slack (use when timing is tight)
        weights[2][6] = 0.5; // Positive for critical path
        bias[2] = -0.2;

        // BrentKung: balanced performance
        weights[3][0] = 0.3;
        weights[3][2] = 0.4;
        weights[3][3] = 0.3;
        weights[3][6] = 0.3;
        bias[3] = 0.1;

        // Sklansky: compromise
        weights[4][0] = 0.35;
        weights[4][2] = 0.35;
        weights[4][3] = 0.25;
        bias[4] = 0.0;

        Self {
            config,
            weights,
            bias,
        }
    }

    /// Select architecture for an adder
    pub fn select_adder(&self, features: &ArchFeatures) -> DatapathArchitecture {
        let architectures = DatapathArchitecture::adder_architectures();
        let scores = self.score_architectures(features, &architectures);

        let (best_idx, _) = scores
            .iter()
            .enumerate()
            .max_by(|(_, a), (_, b)| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal))
            .unwrap_or((0, &0.0));

        architectures[best_idx]
    }

    /// Select architecture for a multiplier
    pub fn select_multiplier(&self, features: &ArchFeatures) -> DatapathArchitecture {
        // Use heuristic selection for multipliers
        if features.bitwidth <= 8 {
            DatapathArchitecture::ArrayMult
        } else if features.on_critical_path || features.slack > 0.7 {
            DatapathArchitecture::WallaceTree
        } else if features.is_signed && features.bitwidth >= 16 {
            DatapathArchitecture::BoothMult
        } else {
            DatapathArchitecture::DaddaTree
        }
    }

    /// Select architecture for a shifter
    pub fn select_shifter(&self, features: &ArchFeatures) -> DatapathArchitecture {
        if features.on_critical_path || features.bitwidth >= 32 {
            DatapathArchitecture::BarrelShifter
        } else {
            DatapathArchitecture::LogShifter
        }
    }

    /// Score architectures using the learned model
    fn score_architectures(
        &self,
        features: &ArchFeatures,
        architectures: &[DatapathArchitecture],
    ) -> Vec<f32> {
        let feature_vec = features.to_vector();
        let mut scores = Vec::with_capacity(architectures.len());

        for (i, arch) in architectures.iter().enumerate() {
            let mut score = if i < self.bias.len() {
                self.bias[i]
            } else {
                0.0
            };

            // Linear scoring from learned weights
            if i < self.weights.len() {
                for (j, &f) in feature_vec.iter().enumerate() {
                    if j < self.weights[i].len() {
                        score += self.weights[i][j] * f;
                    }
                }
            }

            // Add cost-based component from config
            let area_cost = arch.area_factor() * self.config.area_weight;
            let delay_cost = arch.delay_factor() * self.config.delay_weight;
            score -= area_cost + delay_cost;

            scores.push(score);
        }

        scores
    }

    /// Get recommended architecture for an operation
    pub fn recommend(
        &self,
        op_type: OperationType,
        features: &ArchFeatures,
    ) -> DatapathArchitecture {
        match op_type {
            OperationType::Add => self.select_adder(features),
            OperationType::Multiply => self.select_multiplier(features),
            OperationType::Shift => self.select_shifter(features),
            OperationType::Compare => {
                if features.on_critical_path {
                    DatapathArchitecture::TreeCompare
                } else {
                    DatapathArchitecture::RippleCompare
                }
            }
            OperationType::Divide => {
                // Division typically uses array-style implementation
                DatapathArchitecture::ArrayMult
            }
        }
    }

    /// Update weights based on training signal
    pub fn update(&mut self, arch_idx: usize, features: &ArchFeatures, reward: f32) {
        if arch_idx >= self.weights.len() {
            return;
        }

        let feature_vec = features.to_vector();
        let lr = 0.01; // Learning rate

        for (j, &f) in feature_vec.iter().enumerate() {
            if j < self.weights[arch_idx].len() {
                self.weights[arch_idx][j] += lr * reward * f;
            }
        }
        self.bias[arch_idx] += lr * reward;
    }
}

/// Training example for architecture selection
#[allow(dead_code)]
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ArchTrainingExample {
    /// Operation type
    pub op_type: OperationType,
    /// Features
    pub features: ArchFeatures,
    /// Selected architecture (index)
    pub selected_arch: usize,
    /// Resulting area
    pub area: f32,
    /// Resulting delay
    pub delay: f32,
    /// Reward (negative cost)
    pub reward: f32,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_arch_advisor_creation() {
        let advisor = ArchAdvisor::default();
        assert_eq!(advisor.config.area_weight, 0.5);
    }

    #[test]
    fn test_adder_selection_small() {
        let advisor = ArchAdvisor::new(ArchAdvisorConfig::area_optimized());
        let features = ArchFeatures {
            bitwidth: 8,
            slack: 0.8,
            area_budget: 0.2, // Low area budget
            ..Default::default()
        };

        let arch = advisor.select_adder(&features);
        // Should prefer simpler architecture for small bitwidth + low area
        assert!(matches!(
            arch,
            DatapathArchitecture::RippleCarry | DatapathArchitecture::CarryLookahead
        ));
    }

    #[test]
    fn test_adder_selection_timing_critical() {
        let advisor = ArchAdvisor::new(ArchAdvisorConfig::delay_optimized());
        let features = ArchFeatures {
            bitwidth: 64,
            on_critical_path: true,
            slack: 0.1, // Tight timing
            ..Default::default()
        };

        let arch = advisor.select_adder(&features);
        // Should prefer fast architecture for timing-critical
        assert!(matches!(
            arch,
            DatapathArchitecture::KoggeStone
                | DatapathArchitecture::BrentKung
                | DatapathArchitecture::Sklansky
        ));
    }

    #[test]
    fn test_multiplier_selection() {
        let advisor = ArchAdvisor::default();

        // Small multiplier
        let small_features = ArchFeatures {
            bitwidth: 4,
            ..Default::default()
        };
        let arch = advisor.select_multiplier(&small_features);
        assert_eq!(arch, DatapathArchitecture::ArrayMult);

        // Large timing-critical multiplier
        let critical_features = ArchFeatures {
            bitwidth: 32,
            on_critical_path: true,
            ..Default::default()
        };
        let arch = advisor.select_multiplier(&critical_features);
        assert_eq!(arch, DatapathArchitecture::WallaceTree);
    }

    #[test]
    fn test_shifter_selection() {
        let advisor = ArchAdvisor::default();

        let critical_features = ArchFeatures {
            bitwidth: 32,
            on_critical_path: true,
            ..Default::default()
        };
        let arch = advisor.select_shifter(&critical_features);
        assert_eq!(arch, DatapathArchitecture::BarrelShifter);
    }

    #[test]
    fn test_area_delay_factors() {
        // KoggeStone should be faster but larger than RippleCarry
        assert!(
            DatapathArchitecture::KoggeStone.delay_factor()
                < DatapathArchitecture::RippleCarry.delay_factor()
        );
        assert!(
            DatapathArchitecture::KoggeStone.area_factor()
                > DatapathArchitecture::RippleCarry.area_factor()
        );
    }

    #[test]
    fn test_recommend() {
        let advisor = ArchAdvisor::default();
        let features = ArchFeatures::default();

        let add_arch = advisor.recommend(OperationType::Add, &features);
        assert!(DatapathArchitecture::adder_architectures().contains(&add_arch));

        let mult_arch = advisor.recommend(OperationType::Multiply, &features);
        assert!(DatapathArchitecture::multiplier_architectures().contains(&mult_arch));
    }
}
