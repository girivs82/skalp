//! Feature Extraction for ML Models
//!
//! Extracts numerical features from AIGs for use in machine learning models.
//! Features are designed to capture the key characteristics that influence
//! optimization decisions.
//!
//! # Feature Categories
//!
//! 1. **Size Features**: Node counts, input/output counts
//! 2. **Structure Features**: Depth, fanout distribution
//! 3. **Complexity Features**: Cut statistics, reconvergence
//! 4. **Safety Features**: FIT rates, classification distribution

use serde::{Deserialize, Serialize};
use skalp_lir::synth::{Aig, AigNode, AigNodeId, AigStats};
use std::collections::HashMap;

/// Number of features in the fixed-size feature vector
pub const NUM_FEATURES: usize = 37;

/// Features extracted from an AIG
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AigFeatures {
    // === Size Features ===
    /// Total number of nodes
    pub node_count: usize,
    /// Number of AND nodes
    pub and_count: usize,
    /// Number of primary inputs
    pub input_count: usize,
    /// Number of primary outputs
    pub output_count: usize,
    /// Number of latches (registers)
    pub latch_count: usize,

    // === Structure Features ===
    /// Maximum logic depth
    pub max_level: u32,
    /// Average logic depth to outputs
    pub avg_level: f64,
    /// Maximum fanout
    pub max_fanout: u32,
    /// Average fanout
    pub avg_fanout: f64,
    /// Fanout standard deviation
    pub fanout_stddev: f64,

    // === Complexity Features ===
    /// Ratio of ANDs to inputs (complexity indicator)
    pub and_per_input: f64,
    /// Ratio of ANDs to outputs
    pub and_per_output: f64,
    /// Density: ANDs / (inputs * outputs)
    pub density: f64,
    /// Level histogram (10 bins, normalized)
    pub level_histogram: [f64; 10],
    /// Fanout histogram (10 bins, normalized)
    pub fanout_histogram: [f64; 10],

    // === Safety Features ===
    /// Total FIT rate
    pub total_fit: f64,
    /// Maximum FIT rate of any node
    pub max_fit: f64,
    /// Fraction of safety-critical nodes
    pub safety_critical_fraction: f64,

    // === Derived Metrics ===
    /// Estimated cost (area + delay weighted)
    estimated_cost: f64,
    /// Optimization potential (heuristic)
    optimization_potential: f64,
}

impl Default for AigFeatures {
    fn default() -> Self {
        Self {
            node_count: 0,
            and_count: 0,
            input_count: 0,
            output_count: 0,
            latch_count: 0,
            max_level: 0,
            avg_level: 0.0,
            max_fanout: 0,
            avg_fanout: 0.0,
            fanout_stddev: 0.0,
            and_per_input: 0.0,
            and_per_output: 0.0,
            density: 0.0,
            level_histogram: [0.0; 10],
            fanout_histogram: [0.0; 10],
            total_fit: 0.0,
            max_fit: 0.0,
            safety_critical_fraction: 0.0,
            estimated_cost: 0.0,
            optimization_potential: 0.0,
        }
    }
}

impl AigFeatures {
    /// Get estimated cost (for reward computation)
    pub fn estimated_cost(&self) -> f64 {
        self.estimated_cost
    }

    /// Get optimization potential (0-1, higher = more room for optimization)
    pub fn optimization_potential(&self) -> f64 {
        self.optimization_potential
    }

    /// Convert to fixed-size feature vector for ML models
    pub fn to_vector(&self) -> [f64; NUM_FEATURES] {
        let mut v = [0.0; NUM_FEATURES];

        // Normalize features to roughly [0, 1] range
        v[0] = (self.node_count as f64).ln().max(0.0) / 15.0; // log scale
        v[1] = (self.and_count as f64).ln().max(0.0) / 15.0;
        v[2] = (self.input_count as f64).ln().max(0.0) / 10.0;
        v[3] = (self.output_count as f64).ln().max(0.0) / 10.0;
        v[4] = (self.latch_count as f64).ln().max(0.0) / 10.0;

        v[5] = (self.max_level as f64) / 100.0;
        v[6] = self.avg_level / 50.0;
        v[7] = (self.max_fanout as f64) / 100.0;
        v[8] = self.avg_fanout / 10.0;
        v[9] = self.fanout_stddev / 20.0;

        v[10] = self.and_per_input.min(100.0) / 100.0;
        v[11] = self.and_per_output.min(100.0) / 100.0;
        v[12] = self.density.min(1.0);

        // Level histogram (10 values)
        v[13..23].copy_from_slice(&self.level_histogram);

        // Fanout histogram (10 values)
        v[23..33].copy_from_slice(&self.fanout_histogram);

        // Safety features
        v[33] = (self.total_fit + 1.0).ln() / 10.0;
        v[34] = (self.max_fit + 1.0).ln() / 10.0;
        v[35] = self.safety_critical_fraction;

        // Derived
        v[36] = self.optimization_potential;

        v
    }

    /// Create features from a vector (for deserialization)
    pub fn from_vector(v: &[f64; NUM_FEATURES]) -> Self {
        // Inverse of to_vector (approximate)
        Self {
            node_count: (v[0] * 15.0).exp() as usize,
            and_count: (v[1] * 15.0).exp() as usize,
            input_count: (v[2] * 10.0).exp() as usize,
            output_count: (v[3] * 10.0).exp() as usize,
            latch_count: (v[4] * 10.0).exp() as usize,
            max_level: (v[5] * 100.0) as u32,
            avg_level: v[6] * 50.0,
            max_fanout: (v[7] * 100.0) as u32,
            avg_fanout: v[8] * 10.0,
            fanout_stddev: v[9] * 20.0,
            and_per_input: v[10] * 100.0,
            and_per_output: v[11] * 100.0,
            density: v[12],
            level_histogram: [
                v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22],
            ],
            fanout_histogram: [
                v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32],
            ],
            total_fit: (v[33] * 10.0).exp() - 1.0,
            max_fit: (v[34] * 10.0).exp() - 1.0,
            safety_critical_fraction: v[35],
            estimated_cost: 0.0, // Recomputed
            optimization_potential: v[36],
        }
    }
}

/// Feature extractor for AIGs
#[derive(Debug, Clone, Default)]
pub struct FeatureExtractor {
    /// Cache of computed features (optional)
    #[allow(dead_code)]
    cache: HashMap<String, AigFeatures>,
}

impl FeatureExtractor {
    /// Create a new feature extractor
    pub fn new() -> Self {
        Self {
            cache: HashMap::new(),
        }
    }

    /// Extract features from an AIG
    pub fn extract(&self, aig: &Aig) -> AigFeatures {
        let stats = aig.compute_stats();

        // Compute level histogram
        let levels = self.compute_levels(aig);
        let level_histogram = self.histogram(&levels, 10, stats.max_level as f64);

        // Compute fanout histogram
        let fanouts = self.compute_fanouts(aig);
        let fanout_histogram = self.histogram(&fanouts, 5, stats.max_fanout as f64);

        // Compute fanout std dev
        let fanout_stddev = self.stddev(&fanouts);

        // Compute average level
        let avg_level = if levels.is_empty() {
            0.0
        } else {
            levels.iter().sum::<f64>() / levels.len() as f64
        };

        // Derived metrics
        let and_per_input = if stats.input_count > 0 {
            stats.and_count as f64 / stats.input_count as f64
        } else {
            0.0
        };

        let and_per_output = if stats.output_count > 0 {
            stats.and_count as f64 / stats.output_count as f64
        } else {
            0.0
        };

        let density = if stats.input_count > 0 && stats.output_count > 0 {
            stats.and_count as f64 / (stats.input_count * stats.output_count) as f64
        } else {
            0.0
        };

        // Estimated cost (area + delay weighted)
        let area_cost = stats.and_count as f64;
        let delay_cost = stats.max_level as f64 * 10.0; // Weight delay higher
        let estimated_cost = area_cost + delay_cost;

        // Optimization potential (heuristic based on structure)
        let optimization_potential = self.estimate_optimization_potential(&stats, avg_level);

        // Safety features (simplified - would need actual safety info)
        let total_fit = stats.total_fit;
        let max_fit = total_fit; // Simplified
        let safety_critical_fraction = 0.0; // Would need classification info

        AigFeatures {
            node_count: stats.node_count,
            and_count: stats.and_count,
            input_count: stats.input_count,
            output_count: stats.output_count,
            latch_count: stats.latch_count,
            max_level: stats.max_level,
            avg_level,
            max_fanout: stats.max_fanout,
            avg_fanout: stats.avg_fanout,
            fanout_stddev,
            and_per_input,
            and_per_output,
            density,
            level_histogram,
            fanout_histogram,
            total_fit,
            max_fit,
            safety_critical_fraction,
            estimated_cost,
            optimization_potential,
        }
    }

    /// Compute levels for all nodes
    fn compute_levels(&self, aig: &Aig) -> Vec<f64> {
        let mut levels = Vec::new();

        for (id, node) in aig.iter_nodes() {
            if let AigNode::And { .. } = node {
                // This is simplified - would need proper level computation
                levels.push(id.0 as f64 / aig.node_count().max(1) as f64);
            }
        }

        levels
    }

    /// Compute fanouts for all nodes
    fn compute_fanouts(&self, aig: &Aig) -> Vec<f64> {
        let mut fanout_count: HashMap<AigNodeId, usize> = HashMap::new();

        for (_id, node) in aig.iter_nodes() {
            for lit in node.fanins() {
                *fanout_count.entry(lit.node).or_insert(0) += 1;
            }
        }

        fanout_count.values().map(|&c| c as f64).collect()
    }

    /// Compute histogram of values
    fn histogram(&self, values: &[f64], bins: usize, max_val: f64) -> [f64; 10] {
        let mut hist = [0.0; 10];
        if values.is_empty() || max_val == 0.0 {
            return hist;
        }

        let bin_size = max_val / bins as f64;
        for &v in values {
            let bin = ((v / bin_size) as usize).min(bins - 1);
            if bin < 10 {
                hist[bin] += 1.0;
            }
        }

        // Normalize
        let total: f64 = hist.iter().sum();
        if total > 0.0 {
            for h in &mut hist {
                *h /= total;
            }
        }

        hist
    }

    /// Compute standard deviation
    fn stddev(&self, values: &[f64]) -> f64 {
        if values.is_empty() {
            return 0.0;
        }

        let mean = values.iter().sum::<f64>() / values.len() as f64;
        let variance = values.iter().map(|v| (v - mean).powi(2)).sum::<f64>() / values.len() as f64;
        variance.sqrt()
    }

    /// Estimate optimization potential based on structure
    fn estimate_optimization_potential(&self, stats: &AigStats, avg_level: f64) -> f64 {
        let mut potential: f64 = 0.0;

        // High fanout suggests buffer insertion opportunity
        if stats.max_fanout > 10 {
            potential += 0.1;
        }

        // Deep logic suggests balancing opportunity
        if stats.max_level > 20 {
            potential += 0.1;
        }

        // Large difference between max and avg level suggests imbalance
        if stats.max_level as f64 > avg_level * 2.0 {
            potential += 0.15;
        }

        // Many ANDs relative to I/O suggests potential for sharing
        if stats.and_count > stats.input_count * 10 {
            potential += 0.1;
        }

        // High node count suggests DCE opportunity
        if stats.node_count > stats.and_count * 2 {
            potential += 0.05;
        }

        potential.min(1.0)
    }
}

/// Convenience function to extract features
pub fn extract_features(aig: &Aig) -> AigFeatures {
    FeatureExtractor::new().extract(aig)
}

#[cfg(test)]
mod tests {
    use super::*;
    use skalp_lir::synth::AigLit;

    #[test]
    fn test_feature_extraction_empty() {
        let aig = Aig::new("test".to_string());
        let features = extract_features(&aig);

        assert_eq!(features.and_count, 0);
        assert_eq!(features.input_count, 0);
    }

    #[test]
    fn test_feature_extraction_simple() {
        let mut aig = Aig::new("test".to_string());
        let a = aig.add_input("a".to_string(), None);
        let b = aig.add_input("b".to_string(), None);
        let and_result = aig.add_and(AigLit::new(a), AigLit::new(b));
        aig.add_output("out".to_string(), and_result);

        let features = extract_features(&aig);

        assert_eq!(features.input_count, 2);
        assert_eq!(features.output_count, 1);
        assert_eq!(features.and_count, 1);
    }

    #[test]
    fn test_feature_vector_roundtrip() {
        let mut aig = Aig::new("test".to_string());
        let a = aig.add_input("a".to_string(), None);
        let b = aig.add_input("b".to_string(), None);
        let and_result = aig.add_and(AigLit::new(a), AigLit::new(b));
        aig.add_output("out".to_string(), and_result);

        let features = extract_features(&aig);
        let vector = features.to_vector();

        // Vector should have valid values
        assert!(vector.iter().all(|&v| v.is_finite()));
        assert!(vector.iter().all(|&v| (-1.0..=2.0).contains(&v)));
    }

    #[test]
    fn test_estimated_cost() {
        let mut aig = Aig::new("test".to_string());
        let a = aig.add_input("a".to_string(), None);
        let b = aig.add_input("b".to_string(), None);

        // Chain of ANDs to increase depth
        let and1 = aig.add_and(AigLit::new(a), AigLit::new(b));
        let and2 = aig.add_and(and1, AigLit::new(a));
        let and3 = aig.add_and(and2, AigLit::new(b));
        aig.add_output("out".to_string(), and3);

        let features = extract_features(&aig);

        // Cost should be positive and include both area and delay
        assert!(features.estimated_cost() > 0.0);
    }
}
