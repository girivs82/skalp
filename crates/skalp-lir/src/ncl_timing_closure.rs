//! Iterative NCL Timing Closure
//!
//! Implements the STA → PnR → STA feedback loop for NCL async circuits.
//!
//! # Problem
//!
//! Pre-PnR async STA uses estimated wire delays (flat per-fanout guess).
//! After PnR, actual routing delays may differ significantly, making the
//! initial buffer insertion under- or over-compensating.
//!
//! # Solution: Iterative Closure
//!
//! ```text
//! ┌─────────────────────────────────────────────────────┐
//! │  1. Pre-PnR STA (estimated delays)                  │
//! │     → Insert conservative buffers                   │
//! │     → Generate initial constraints                  │
//! │                                                     │
//! │  2. Run PnR (placement + routing)                   │
//! │     → Extract actual per-net wire delays             │
//! │                                                     │
//! │  3. Post-PnR STA (actual wire delays)               │
//! │     → Check: all forks within skew budget?          │
//! │     │                                               │
//! │     ├─ YES → Done (timing closed)                   │
//! │     │                                               │
//! │     └─ NO  → Adjust buffers, update constraints     │
//! │              → Go to step 2 (re-route)              │
//! │                                                     │
//! │  Max iterations: 3-5 (typically converges in 2)     │
//! └─────────────────────────────────────────────────────┘
//! ```
//!
//! # Usage
//!
//! ```ignore
//! let mut closure = NclTimingClosure::new(netlist, library, config);
//!
//! // Iteration 1: pre-PnR
//! let pre_result = closure.pre_pnr_analysis();
//! let constraints = pre_result.constraints;
//! // ... run PnR with constraints ...
//! let wire_delays = extract_wire_delays(&routing_result);
//!
//! // Iteration 2: post-PnR with actual delays
//! let post_result = closure.post_pnr_iteration(wire_delays);
//! if post_result.converged {
//!     println!("Timing closed in {} iterations", post_result.iteration);
//! }
//! ```

use crate::async_sta::{
    analyze_async_timing, AsyncStaConfig, AsyncStaResult, ViolationSeverity,
};
use crate::async_sta_fix::{fix_fork_violations, AsyncStaFixConfig, AsyncStaFixResult, FixStrategy};
use crate::gate_netlist::{GateNetId, GateNetlist};
use crate::ncl_constraints::{
    generate_ncl_constraints, NclConstraintConfig, NclRoutingConstraints,
};
use crate::tech_library::TechLibrary;
use indexmap::IndexMap;

/// Configuration for iterative timing closure
#[derive(Debug, Clone)]
pub struct TimingClosureConfig {
    /// Maximum iterations before giving up
    pub max_iterations: usize,
    /// Async STA configuration
    pub sta_config: AsyncStaConfig,
    /// Buffer insertion configuration
    pub fix_config: AsyncStaFixConfig,
    /// Constraint generation configuration
    pub constraint_config: NclConstraintConfig,
    /// Convergence threshold: if max skew improvement between iterations
    /// is less than this (ps), consider converged even with remaining violations
    pub convergence_threshold_ps: f64,
}

impl Default for TimingClosureConfig {
    fn default() -> Self {
        Self {
            max_iterations: 4,
            sta_config: AsyncStaConfig::default(),
            fix_config: AsyncStaFixConfig {
                strategy: FixStrategy::DelayReadySignal,
                ready_delay_margin_ps: 10.0,
                ..Default::default()
            },
            constraint_config: NclConstraintConfig::default(),
            convergence_threshold_ps: 5.0,
        }
    }
}

/// Result of a single timing closure iteration
#[derive(Debug, Clone)]
pub struct ClosureIteration {
    /// Which iteration (0 = pre-PnR, 1+ = post-PnR)
    pub iteration: usize,
    /// STA results for this iteration
    pub sta_result: AsyncStaResult,
    /// Fix results (buffers inserted, if any)
    pub fix_result: Option<AsyncStaFixResult>,
    /// Updated routing constraints
    pub constraints: NclRoutingConstraints,
    /// Whether actual wire delays were used (false = estimated)
    pub used_actual_delays: bool,
    /// Max skew from this iteration
    pub max_skew_ps: f64,
    /// Number of fork violations
    pub violation_count: usize,
}

/// Result of the complete timing closure process
#[derive(Debug, Clone)]
pub struct TimingClosureResult {
    /// Whether timing converged (all forks within budget)
    pub converged: bool,
    /// Number of iterations performed
    pub iterations_performed: usize,
    /// History of each iteration's results
    pub history: Vec<ClosureIteration>,
    /// Final routing constraints
    pub final_constraints: NclRoutingConstraints,
    /// Total buffers inserted across all iterations
    pub total_buffers_inserted: usize,
    /// Max skew in final iteration
    pub final_max_skew_ps: f64,
    /// Convergence reason
    pub reason: ConvergenceReason,
}

/// Why timing closure stopped
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ConvergenceReason {
    /// All forks within skew budget
    AllForksClean,
    /// No more violations above error threshold (only warnings remain)
    OnlyWarningsRemain,
    /// Skew stopped improving between iterations
    SkewConverged,
    /// Hit maximum iteration limit
    MaxIterations,
}

impl TimingClosureResult {
    pub fn summary(&self) -> String {
        let mut s = String::new();
        s.push_str("=== NCL Timing Closure Report ===\n");
        s.push_str(&format!(
            "  Converged: {} ({:?})\n",
            self.converged, self.reason
        ));
        s.push_str(&format!("  Iterations: {}\n", self.iterations_performed));
        s.push_str(&format!(
            "  Total buffers inserted: {}\n",
            self.total_buffers_inserted
        ));
        s.push_str(&format!(
            "  Final max skew: {:.1}ps\n",
            self.final_max_skew_ps
        ));

        if self.history.len() > 1 {
            s.push_str("  Skew progression:");
            for iter in &self.history {
                let tag = if iter.used_actual_delays {
                    "post-PnR"
                } else {
                    "estimated"
                };
                s.push_str(&format!(
                    " {:.1}ps({})",
                    iter.max_skew_ps, tag
                ));
                if iter.iteration < self.history.len() - 1 {
                    s.push_str(" →");
                }
            }
            s.push('\n');
        }

        s
    }
}

/// Run pre-PnR timing analysis (iteration 0)
///
/// Uses estimated wire delays. Returns initial constraints for PnR.
/// After running PnR with these constraints, call `post_pnr_iteration`
/// with the actual wire delays from routing.
pub fn pre_pnr_analysis(
    netlist: &mut GateNetlist,
    library: Option<&TechLibrary>,
    config: &TimingClosureConfig,
) -> ClosureIteration {
    // Run STA with estimated delays
    let sta_result = analyze_async_timing(netlist, library, &config.sta_config);

    // Apply initial fix (conservative — uses estimated delays)
    let fix_result = if sta_result.has_violations() {
        let fr = fix_fork_violations(netlist, &sta_result, &config.fix_config);
        Some(fr)
    } else {
        None
    };

    // Generate initial constraints
    let constraints = generate_ncl_constraints(netlist, &sta_result, &config.constraint_config);

    ClosureIteration {
        iteration: 0,
        max_skew_ps: sta_result.stats.max_skew_ps,
        violation_count: sta_result.stats.fork_violations,
        sta_result,
        fix_result,
        constraints,
        used_actual_delays: false,
    }
}

/// Run post-PnR timing analysis with actual wire delays
///
/// This is the core of the iterative loop. Call this after each PnR run
/// with the actual per-net wire delays extracted from routing results.
///
/// Returns updated constraints. If `converged` is false in the result,
/// the caller should re-run PnR with the updated constraints and call
/// this again.
pub fn post_pnr_iteration(
    netlist: &mut GateNetlist,
    library: Option<&TechLibrary>,
    wire_delays: IndexMap<GateNetId, f64>,
    iteration: usize,
    prev_max_skew: f64,
    config: &TimingClosureConfig,
) -> ClosureIteration {
    // Run STA with actual wire delays from PnR
    let mut sta = crate::async_sta::AsyncSta::new(netlist, config.sta_config.clone());
    if let Some(lib) = library {
        sta = sta.with_library(lib);
    }
    sta = sta.with_wire_delays(wire_delays);
    let sta_result = sta.analyze();

    // Apply fix if still violating
    let fix_result = if sta_result.has_violations() && sta_result.error_count() > 0 {
        let fr = fix_fork_violations(netlist, &sta_result, &config.fix_config);
        Some(fr)
    } else {
        None
    };

    // Generate updated constraints
    let constraints = generate_ncl_constraints(netlist, &sta_result, &config.constraint_config);

    ClosureIteration {
        iteration,
        max_skew_ps: sta_result.stats.max_skew_ps,
        violation_count: sta_result.stats.fork_violations,
        sta_result,
        fix_result,
        constraints,
        used_actual_delays: true,
    }
}

/// Run the full iterative timing closure loop (for use when PnR is in-process)
///
/// This drives the complete loop when the PnR engine is available as a callback.
/// The `run_pnr` callback takes a netlist + constraints and returns per-net wire delays.
///
/// For external PnR (nextpnr), use `pre_pnr_analysis` and `post_pnr_iteration`
/// manually in a script loop.
pub fn run_timing_closure<F>(
    netlist: &mut GateNetlist,
    library: Option<&TechLibrary>,
    config: &TimingClosureConfig,
    mut run_pnr: F,
) -> TimingClosureResult
where
    F: FnMut(&GateNetlist, &NclRoutingConstraints) -> IndexMap<GateNetId, f64>,
{
    let mut history = Vec::new();
    let mut total_buffers = 0;

    // Iteration 0: pre-PnR with estimated delays
    let iter0 = pre_pnr_analysis(netlist, library, config);
    if let Some(ref fix) = iter0.fix_result {
        total_buffers += fix.buffers_inserted;
    }
    let mut prev_max_skew = iter0.max_skew_ps;
    let mut last_constraints = iter0.constraints.clone();
    history.push(iter0);

    // Check if already clean
    if history[0].violation_count == 0 {
        return TimingClosureResult {
            converged: true,
            iterations_performed: 1,
            final_constraints: last_constraints,
            total_buffers_inserted: total_buffers,
            final_max_skew_ps: prev_max_skew,
            reason: ConvergenceReason::AllForksClean,
            history,
        };
    }

    // Iterative loop
    for i in 1..=config.max_iterations {
        // Run PnR with current constraints
        let wire_delays = run_pnr(netlist, &last_constraints);

        // Post-PnR STA with actual delays
        let iter = post_pnr_iteration(
            netlist,
            library,
            wire_delays,
            i,
            prev_max_skew,
            config,
        );

        if let Some(ref fix) = iter.fix_result {
            total_buffers += fix.buffers_inserted;
        }

        let current_skew = iter.max_skew_ps;
        let current_violations = iter.violation_count;
        last_constraints = iter.constraints.clone();
        history.push(iter);

        // Check convergence conditions

        // 1. All clean
        if current_violations == 0 {
            return TimingClosureResult {
                converged: true,
                iterations_performed: i + 1,
                final_constraints: last_constraints,
                total_buffers_inserted: total_buffers,
                final_max_skew_ps: current_skew,
                reason: ConvergenceReason::AllForksClean,
                history,
            };
        }

        // 2. Only warnings remain (no errors/critical)
        let has_errors = history.last().unwrap().sta_result.fork_violations.iter().any(|v| {
            v.severity == ViolationSeverity::Error || v.severity == ViolationSeverity::Critical
        });
        if !has_errors {
            return TimingClosureResult {
                converged: true,
                iterations_performed: i + 1,
                final_constraints: last_constraints,
                total_buffers_inserted: total_buffers,
                final_max_skew_ps: current_skew,
                reason: ConvergenceReason::OnlyWarningsRemain,
                history,
            };
        }

        // 3. Skew stopped improving
        let improvement = prev_max_skew - current_skew;
        if improvement.abs() < config.convergence_threshold_ps && i > 1 {
            return TimingClosureResult {
                converged: false,
                iterations_performed: i + 1,
                final_constraints: last_constraints,
                total_buffers_inserted: total_buffers,
                final_max_skew_ps: current_skew,
                reason: ConvergenceReason::SkewConverged,
                history,
            };
        }

        prev_max_skew = current_skew;
    }

    // Hit max iterations
    let final_skew = history.last().map(|h| h.max_skew_ps).unwrap_or(0.0);
    TimingClosureResult {
        converged: false,
        iterations_performed: config.max_iterations + 1,
        final_constraints: last_constraints,
        total_buffers_inserted: total_buffers,
        final_max_skew_ps: final_skew,
        reason: ConvergenceReason::MaxIterations,
        history,
    }
}

/// Extract per-net wire delays from a PnR routing result.
///
/// This is a helper for converting PnR output into the format needed by
/// `post_pnr_iteration`. The wire_delays map is: net_id → delay_ps.
///
/// For skalp-place-route, the Route struct already has `delay: u32` (ps).
/// For nextpnr, parse the timing report to get per-net delays.
pub fn wire_delays_from_route_map(
    route_delays: &std::collections::HashMap<GateNetId, u32>,
) -> IndexMap<GateNetId, f64> {
    route_delays
        .iter()
        .map(|(&net_id, &delay_ps)| (net_id, delay_ps as f64))
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::async_sta::{AsyncStaConfig, AsyncStaResult, AsyncStaStats, ForkViolation};

    fn make_ncl_netlist() -> GateNetlist {
        let mut netlist = GateNetlist::new("test_ncl".to_string(), "test_lib".to_string());
        netlist.is_ncl = true;
        netlist
    }

    #[test]
    fn test_pre_pnr_analysis_empty() {
        let mut netlist = make_ncl_netlist();
        let config = TimingClosureConfig::default();
        let result = pre_pnr_analysis(&mut netlist, None, &config);

        assert_eq!(result.iteration, 0);
        assert!(!result.used_actual_delays);
        assert_eq!(result.violation_count, 0);
    }

    #[test]
    fn test_timing_closure_converges_immediately() {
        let mut netlist = make_ncl_netlist();
        let config = TimingClosureConfig::default();

        // PnR callback that returns empty delays (no nets to route)
        let result = run_timing_closure(&mut netlist, None, &config, |_, _| IndexMap::new());

        assert!(result.converged);
        assert_eq!(result.reason, ConvergenceReason::AllForksClean);
        assert_eq!(result.iterations_performed, 1); // Only pre-PnR needed
    }

    #[test]
    fn test_wire_delays_from_route_map() {
        let mut routes = std::collections::HashMap::new();
        routes.insert(GateNetId(0), 150u32);
        routes.insert(GateNetId(5), 300u32);

        let delays = wire_delays_from_route_map(&routes);
        assert_eq!(delays.len(), 2);
        assert_eq!(*delays.get(&GateNetId(0)).unwrap(), 150.0);
        assert_eq!(*delays.get(&GateNetId(5)).unwrap(), 300.0);
    }

    #[test]
    fn test_closure_result_summary() {
        let result = TimingClosureResult {
            converged: true,
            iterations_performed: 2,
            history: vec![],
            final_constraints: NclRoutingConstraints::default(),
            total_buffers_inserted: 3,
            final_max_skew_ps: 12.5,
            reason: ConvergenceReason::OnlyWarningsRemain,
        };

        let summary = result.summary();
        assert!(summary.contains("Converged: true"));
        assert!(summary.contains("Iterations: 2"));
        assert!(summary.contains("12.5ps"));
    }
}
