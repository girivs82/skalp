//! Iterative NCL Timing Closure (FPGA-correct)
//!
//! Implements the STA → PnR → STA feedback loop for NCL async circuits.
//!
//! # Ready-Signal-Delay Approach
//!
//! Instead of balancing individual data fork branches (expensive, often impossible),
//! we delay the completion/ready signal by the worst-case skew. This ensures all
//! data paths settle before the handshake completes.
//!
//! Two mechanisms:
//!
//! 1. **Placement proximity** — zero area cost, co-locate fork destinations
//! 2. **Ready signal delay** — LUT buffer chain on completion/detection nets only
//!
//! # 2-Pass Flow
//!
//! ```text
//! ┌──────────────────────────────────────────────────────┐
//! │  1. Pre-PnR STA (estimated delays)                   │
//! │     → Generate placement constraints (proximity)      │
//! │     → NO buffer insertion at this stage               │
//! │                                                       │
//! │  2. Run PnR (placement + routing) unconstrained       │
//! │     → Extract actual per-net wire delays               │
//! │                                                       │
//! │  3. Post-PnR STA (actual wire delays)                 │
//! │     → Compute worst-case fork skew                    │
//! │     │                                                 │
//! │     ├─ No violations → Done (timing closed)           │
//! │     │                                                 │
//! │     └─ Violations → Insert ready-signal delay buffer  │
//! │        chain on completion/detection nets              │
//! │                                                       │
//! │  4. Re-PnR with buffer-modified netlist               │
//! │     → Buffers are placed and routed normally           │
//! │                                                       │
//! │  Typically 2 passes (1 without buffers, 1 with)       │
//! └──────────────────────────────────────────────────────┘
//! ```
//!
//! # Usage
//!
//! ```ignore
//! // Manual iteration (for external PnR like nextpnr)
//! let pre = pre_pnr_analysis(&mut netlist, None, &config);
//! let constraints = &pre.constraints;
//! // ... run PnR with constraints, extract wire delays ...
//! let post = post_pnr_iteration(&mut netlist, None, wire_delays, 1, pre.max_skew_ps, &config);
//!
//! // Automatic loop (for in-process skalp PnR)
//! let result = run_timing_closure(&mut netlist, None, &config, |n, c| run_pnr(n, c));
//! ```

use crate::async_sta::{analyze_async_timing, AsyncStaConfig, AsyncStaResult, ViolationSeverity};
use crate::async_sta_fix::{fix_fork_violations, AsyncStaFixConfig, AsyncStaFixResult};
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
    /// Buffer insertion configuration (used only as last resort)
    pub fix_config: AsyncStaFixConfig,
    /// Constraint generation configuration
    pub constraint_config: NclConstraintConfig,
    /// Convergence threshold: if max skew improvement between iterations
    /// is less than this (ps), consider converged even with remaining violations
    pub convergence_threshold_ps: f64,
    /// How many constraint-only iterations to try before allowing LUT buffers.
    /// Default: 2 — try routing constraints twice before resorting to buffers.
    pub constraint_only_iterations: usize,
    /// Factor to tighten constraints by on each re-route iteration.
    /// E.g., 0.8 means each iteration requests 80% of the previous max_skew target.
    pub constraint_tightening_factor: f64,
}

impl Default for TimingClosureConfig {
    fn default() -> Self {
        Self {
            max_iterations: 4,
            sta_config: AsyncStaConfig::default(),
            fix_config: AsyncStaFixConfig::default(),
            constraint_config: NclConstraintConfig::default(),
            convergence_threshold_ps: 5.0,
            constraint_only_iterations: 2,
            constraint_tightening_factor: 0.8,
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
    /// Fix results (LUT buffers inserted — only on later iterations as last resort)
    pub fix_result: Option<AsyncStaFixResult>,
    /// Updated routing constraints
    pub constraints: NclRoutingConstraints,
    /// Whether actual wire delays were used (false = estimated)
    pub used_actual_delays: bool,
    /// Max skew from this iteration
    pub max_skew_ps: f64,
    /// Number of fork violations
    pub violation_count: usize,
    /// What action was taken
    pub action: ClosureAction,
}

/// What action the closure iteration took
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ClosureAction {
    /// Generated initial constraints from estimated delays (pre-PnR)
    InitialConstraints,
    /// Tightened existing constraints based on actual delays
    TightenedConstraints,
    /// Inserted LUT buffer chains as last resort
    InsertedBuffers,
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
    /// Total LUT buffers inserted (ideally 0)
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
            "  LUT buffers inserted: {}\n",
            self.total_buffers_inserted
        ));
        s.push_str(&format!(
            "  Final max skew: {:.1}ps\n",
            self.final_max_skew_ps
        ));

        if self.history.len() > 1 {
            s.push_str("  Skew progression:");
            for iter in &self.history {
                let tag = match (&iter.action, iter.used_actual_delays) {
                    (ClosureAction::InitialConstraints, _) => "estimated",
                    (ClosureAction::TightenedConstraints, true) => "post-PnR",
                    (ClosureAction::InsertedBuffers, _) => "post-PnR+buf",
                    _ => "unknown",
                };
                s.push_str(&format!(" {:.1}ps({})", iter.max_skew_ps, tag));
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
/// **No buffer insertion** — only routing and placement constraints.
/// Constraints are also attached to `netlist.ncl_constraints` so the PnR
/// engine can read them directly without a separate file.
///
/// After running PnR with these constraints, call `post_pnr_iteration`
/// with the actual wire delays from routing.
pub fn pre_pnr_analysis(
    netlist: &mut GateNetlist,
    library: Option<&TechLibrary>,
    config: &TimingClosureConfig,
) -> ClosureIteration {
    // Run STA with estimated delays
    let sta_result = analyze_async_timing(netlist, library, &config.sta_config);

    // Generate initial constraints — NO buffer insertion
    let constraints = generate_ncl_constraints(netlist, &sta_result, &config.constraint_config);

    // Attach constraints to netlist for PnR to consume
    netlist.ncl_constraints = Some(constraints.clone());

    ClosureIteration {
        iteration: 0,
        max_skew_ps: sta_result.stats.max_skew_ps,
        violation_count: sta_result.stats.fork_violations,
        sta_result,
        fix_result: None,
        constraints,
        used_actual_delays: false,
        action: ClosureAction::InitialConstraints,
    }
}

/// Run post-PnR timing analysis with actual wire delays
///
/// This is the core of the iterative loop. Call this after each PnR run
/// with the actual per-net wire delays extracted from routing results.
///
/// Wire delays can be passed explicitly via `wire_delays`, or if `None`,
/// they are read from `netlist.ncl_wire_delays` (populated by PnR).
///
/// The `iteration` parameter controls whether LUT buffers are allowed:
/// - `iteration <= config.constraint_only_iterations`: tighten constraints only
/// - `iteration > config.constraint_only_iterations`: allow LUT buffer insertion
///
/// Updated constraints are attached to `netlist.ncl_constraints`.
pub fn post_pnr_iteration(
    netlist: &mut GateNetlist,
    library: Option<&TechLibrary>,
    wire_delays: IndexMap<GateNetId, f64>,
    iteration: usize,
    prev_max_skew: f64,
    config: &TimingClosureConfig,
) -> ClosureIteration {
    // Use provided delays, or fall back to netlist-attached delays from PnR
    let delays = if wire_delays.is_empty() {
        netlist.ncl_wire_delays.clone().unwrap_or_default()
    } else {
        wire_delays
    };

    // Run STA with actual wire delays from PnR
    let mut sta = crate::async_sta::AsyncSta::new(netlist, config.sta_config.clone());
    if let Some(lib) = library {
        sta = sta.with_library(lib);
    }
    sta = sta.with_wire_delays(delays);
    let sta_result = sta.analyze();

    let has_errors = sta_result.fork_violations.iter().any(|v| {
        v.severity == ViolationSeverity::Error || v.severity == ViolationSeverity::Critical
    });

    // Decide action: tighten constraints or insert buffers as last resort
    let allow_buffers = iteration > config.constraint_only_iterations && has_errors;

    let (fix_result, action) = if allow_buffers {
        // Last resort: insert LUT buffers on specific fast paths
        let fr = fix_fork_violations(netlist, &sta_result, &config.fix_config);
        (Some(fr), ClosureAction::InsertedBuffers)
    } else {
        (None, ClosureAction::TightenedConstraints)
    };

    // Generate updated constraints (tighter if routing couldn't meet previous)
    let mut tightened_config = config.constraint_config.clone();
    if iteration > 1 {
        // Tighten the skew margin on each re-route iteration
        tightened_config.skew_margin_ps += config.constraint_config.skew_margin_ps
            * (1.0 - config.constraint_tightening_factor)
            * iteration as f64;
    }
    let constraints = generate_ncl_constraints(netlist, &sta_result, &tightened_config);

    // Attach updated constraints to netlist for next PnR iteration
    netlist.ncl_constraints = Some(constraints.clone());

    ClosureIteration {
        iteration,
        max_skew_ps: sta_result.stats.max_skew_ps,
        violation_count: sta_result.stats.fork_violations,
        sta_result,
        fix_result,
        constraints,
        used_actual_delays: true,
        action,
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

    // Iteration 0: pre-PnR with estimated delays — constraints only
    let iter0 = pre_pnr_analysis(netlist, library, config);
    let mut prev_max_skew = iter0.max_skew_ps;
    let mut last_constraints = iter0.constraints.clone();
    history.push(iter0);

    // Check if already clean (no violations from estimation)
    if history[0].violation_count == 0 {
        return TimingClosureResult {
            converged: true,
            iterations_performed: 1,
            final_constraints: last_constraints,
            total_buffers_inserted: 0,
            final_max_skew_ps: prev_max_skew,
            reason: ConvergenceReason::AllForksClean,
            history,
        };
    }

    // Iterative loop
    for i in 1..=config.max_iterations {
        // Run PnR with current constraints — routing is always fresh from placement
        let wire_delays = run_pnr(netlist, &last_constraints);

        // Post-PnR STA with actual delays
        let iter = post_pnr_iteration(netlist, library, wire_delays, i, prev_max_skew, config);

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
        let has_errors = history
            .last()
            .unwrap()
            .sta_result
            .fork_violations
            .iter()
            .any(|v| {
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

        // 3. Skew stopped improving (only check after first post-PnR iteration)
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
        assert_eq!(result.action, ClosureAction::InitialConstraints);
        // No buffers should be inserted in pre-PnR
        assert!(result.fix_result.is_none());
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
        assert_eq!(result.total_buffers_inserted, 0);
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
            total_buffers_inserted: 0,
            final_max_skew_ps: 12.5,
            reason: ConvergenceReason::OnlyWarningsRemain,
        };

        let summary = result.summary();
        assert!(summary.contains("Converged: true"));
        assert!(summary.contains("Iterations: 2"));
        assert!(summary.contains("12.5ps"));
        assert!(summary.contains("LUT buffers inserted: 0"));
    }

    #[test]
    fn test_constraint_only_iterations_config() {
        let config = TimingClosureConfig::default();
        // Default: 2 constraint-only iterations before allowing buffers
        assert_eq!(config.constraint_only_iterations, 2);
        assert_eq!(config.max_iterations, 4);
        // So: iter 0 = pre-PnR (constraints), iter 1-2 = post-PnR (constraints only),
        // iter 3-4 = post-PnR (constraints + buffers if needed)
    }

    #[test]
    fn test_closure_action_progression() {
        // Verify the action types are what we expect
        assert_eq!(
            ClosureAction::InitialConstraints,
            ClosureAction::InitialConstraints
        );
        assert_ne!(
            ClosureAction::InitialConstraints,
            ClosureAction::TightenedConstraints
        );
        assert_ne!(
            ClosureAction::TightenedConstraints,
            ClosureAction::InsertedBuffers
        );
    }
}
