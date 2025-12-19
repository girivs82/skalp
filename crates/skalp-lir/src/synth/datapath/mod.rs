//! Datapath Optimization
//!
//! This module provides optimizations for arithmetic datapath operations,
//! including adder architecture selection and multiplier optimization.
//!
//! # Adder Architectures
//!
//! Different adder architectures trade off area vs delay:
//!
//! | Architecture | Delay | Area | Best For |
//! |--------------|-------|------|----------|
//! | Ripple Carry | O(n) | O(n) | Small widths, area-constrained |
//! | Carry Lookahead | O(log n) | O(n log n) | Medium widths |
//! | Kogge-Stone | O(log n) | O(n log n) | Speed-critical |
//! | Brent-Kung | O(log n) | O(n) | Balanced |
//!
//! # References
//!
//! - Kogge, P. M., & Stone, H. S. (1973). A parallel algorithm for the efficient solution of a general class of recurrence equations.
//! - Brent, R. P., & Kung, H. T. (1982). A regular layout for parallel adders.

mod adder_opt;

pub use adder_opt::{
    generate_carry_lookahead, generate_kogge_stone, generate_ripple_carry, AdderArchitecture,
    AdderConfig, AdderOptimizer, AdderStats,
};

use super::timing::TimePs;

/// Datapath operation type
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DatapathOp {
    /// Addition
    Add,
    /// Subtraction
    Sub,
    /// Increment
    Inc,
    /// Decrement
    Dec,
    /// Comparison
    Compare,
    /// Multiplication
    Multiply,
}

/// Datapath optimization configuration
#[derive(Debug, Clone)]
pub struct DatapathConfig {
    /// Target timing constraint (ps)
    pub timing_target: Option<TimePs>,
    /// Area budget (relative units)
    pub area_budget: Option<f64>,
    /// Prefer parallel architectures
    pub prefer_parallel: bool,
    /// Maximum fanout before buffering
    pub max_fanout: usize,
}

impl Default for DatapathConfig {
    fn default() -> Self {
        Self {
            timing_target: None,
            area_budget: None,
            prefer_parallel: false,
            max_fanout: 4,
        }
    }
}

impl DatapathConfig {
    /// Create config optimized for speed
    pub fn speed() -> Self {
        Self {
            prefer_parallel: true,
            ..Default::default()
        }
    }

    /// Create config optimized for area
    pub fn area() -> Self {
        Self {
            prefer_parallel: false,
            ..Default::default()
        }
    }

    /// Create config with timing target
    pub fn with_timing(target: TimePs) -> Self {
        Self {
            timing_target: Some(target),
            ..Default::default()
        }
    }
}

/// Statistics for datapath operations
#[derive(Debug, Clone, Default)]
pub struct DatapathStats {
    /// Number of adders optimized
    pub adders_optimized: usize,
    /// Total area savings
    pub area_savings: f64,
    /// Total delay improvement (ps)
    pub delay_improvement: TimePs,
    /// Architectures used
    pub architectures_used: std::collections::HashMap<String, usize>,
}

impl DatapathStats {
    /// Create new empty stats
    pub fn new() -> Self {
        Self::default()
    }

    /// Record an optimization
    pub fn record(&mut self, arch: &str, area_saved: f64, delay_improved: TimePs) {
        self.adders_optimized += 1;
        self.area_savings += area_saved;
        self.delay_improvement += delay_improved;
        *self.architectures_used.entry(arch.to_string()).or_insert(0) += 1;
    }

    /// Get summary string
    pub fn summary(&self) -> String {
        format!(
            "Optimized {} adders, Area: {:.1}, Delay: {:.1}ps",
            self.adders_optimized, self.area_savings, self.delay_improvement
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_datapath_config() {
        let config = DatapathConfig::speed();
        assert!(config.prefer_parallel);

        let config = DatapathConfig::area();
        assert!(!config.prefer_parallel);

        let config = DatapathConfig::with_timing(5000.0);
        assert_eq!(config.timing_target, Some(5000.0));
    }

    #[test]
    fn test_datapath_stats() {
        let mut stats = DatapathStats::new();
        stats.record("kogge_stone", 10.0, 50.0);
        stats.record("ripple_carry", -5.0, -20.0);

        assert_eq!(stats.adders_optimized, 2);
        assert_eq!(stats.area_savings, 5.0);
        assert_eq!(stats.delay_improvement, 30.0);
    }
}
