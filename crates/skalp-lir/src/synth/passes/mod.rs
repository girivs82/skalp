//! AIG Optimization Passes
//!
//! This module provides optimization passes that transform AIGs to reduce
//! area, depth, or improve other metrics while preserving functionality.
//!
//! # Pass Categories
//!
//! - **Structural**: `strash`, `dce` - Clean up the AIG structure
//! - **Simplification**: `const_prop` - Simplify using constants
//! - **Balancing**: `balance` - Reduce logic depth
//! - **Rewriting**: `rewrite`, `refactor` - Replace subgraphs with smaller equivalents
//!
//! # Example
//!
//! ```ignore
//! use skalp_lir::synth::passes::{Pass, ConstProp, Dce, Balance};
//!
//! let mut aig = build_aig(&netlist);
//!
//! // Run optimization passes
//! ConstProp::new().run(&mut aig);
//! Dce::new().run(&mut aig);
//! Balance::new().run(&mut aig);
//! ```

mod balance;
mod buffer_opt;
mod const_prop;
mod dc2;
mod dce;
mod fraig;
mod refactor;
mod resub;
mod retiming;
mod rewrite;
mod scorr;
mod strash;

pub use balance::Balance;
pub use buffer_opt::{analyze_fanout, BufferConfig, BufferInsertion, BufferStats, FanoutAnalysis};
pub use const_prop::ConstProp;
pub use dc2::Dc2;
pub use dce::Dce;
pub use fraig::{run_fraig, run_fraig_with_config, Fraig, FraigConfig, FraigStats};
pub use refactor::Refactor;
pub use resub::Resub;
pub use retiming::{
    run_retiming, run_retiming_with_config, Retiming, RetimingConfig, RetimingStats,
};
pub use rewrite::Rewrite;
pub use scorr::Scorr;
pub use strash::Strash;

use super::{Aig, AigStats};

/// Result of running an optimization pass
#[derive(Debug, Clone, Default)]
pub struct PassResult {
    /// Name of the pass
    pub pass_name: String,
    /// Number of nodes before the pass
    pub nodes_before: usize,
    /// Number of nodes after the pass
    pub nodes_after: usize,
    /// Number of AND nodes before
    pub ands_before: usize,
    /// Number of AND nodes after
    pub ands_after: usize,
    /// Maximum level before
    pub levels_before: u32,
    /// Maximum level after
    pub levels_after: u32,
    /// Whether the pass made any changes
    pub changed: bool,
    /// Additional statistics
    pub extra: Vec<(String, String)>,
}

impl PassResult {
    /// Create a new pass result
    pub fn new(pass_name: &str) -> Self {
        Self {
            pass_name: pass_name.to_string(),
            ..Default::default()
        }
    }

    /// Record the "before" stats
    pub fn record_before(&mut self, aig: &Aig) {
        let stats = aig.compute_stats();
        self.nodes_before = stats.node_count;
        self.ands_before = stats.and_count;
        self.levels_before = stats.max_level;
    }

    /// Record the "after" stats
    pub fn record_after(&mut self, aig: &Aig) {
        let stats = aig.compute_stats();
        self.nodes_after = stats.node_count;
        self.ands_after = stats.and_count;
        self.levels_after = stats.max_level;
        self.changed = self.nodes_before != self.nodes_after
            || self.ands_before != self.ands_after
            || self.levels_before != self.levels_after;
    }

    /// Add extra information
    pub fn add_extra(&mut self, key: &str, value: &str) {
        self.extra.push((key.to_string(), value.to_string()));
    }

    /// Get reduction in AND nodes
    pub fn and_reduction(&self) -> i32 {
        self.ands_before as i32 - self.ands_after as i32
    }

    /// Get reduction in levels
    pub fn level_reduction(&self) -> i32 {
        self.levels_before as i32 - self.levels_after as i32
    }
}

impl std::fmt::Display for PassResult {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}: ANDs {} → {} ({}), Levels {} → {} ({})",
            self.pass_name,
            self.ands_before,
            self.ands_after,
            if self.and_reduction() >= 0 {
                format!("-{}", self.and_reduction())
            } else {
                format!("+{}", -self.and_reduction())
            },
            self.levels_before,
            self.levels_after,
            if self.level_reduction() >= 0 {
                format!("-{}", self.level_reduction())
            } else {
                format!("+{}", -self.level_reduction())
            }
        )
    }
}

/// Trait for AIG optimization passes
pub trait Pass {
    /// Get the name of this pass
    fn name(&self) -> &str;

    /// Run the pass on an AIG
    fn run(&mut self, aig: &mut Aig) -> PassResult;
}

/// Run a sequence of passes on an AIG
pub fn run_passes(aig: &mut Aig, passes: &mut [Box<dyn Pass>]) -> Vec<PassResult> {
    let mut results = Vec::new();
    for pass in passes.iter_mut() {
        let result = pass.run(aig);
        results.push(result);
    }
    results
}

/// Default optimization sequence
pub fn default_optimization_sequence() -> Vec<Box<dyn Pass>> {
    vec![
        Box::new(ConstProp::new()),
        Box::new(Strash::new()),
        Box::new(Dce::new()),
        Box::new(Balance::new()),
    ]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_pass_result() {
        let mut result = PassResult::new("test");
        result.nodes_before = 100;
        result.ands_before = 50;
        result.levels_before = 10;
        result.nodes_after = 80;
        result.ands_after = 40;
        result.levels_after = 8;
        result.changed = true;

        assert_eq!(result.and_reduction(), 10);
        assert_eq!(result.level_reduction(), 2);
    }
}
