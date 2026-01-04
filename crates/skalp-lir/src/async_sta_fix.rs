//! Async STA Fix: Buffer insertion to fix isochronic fork violations
//!
//! This module implements automatic fixing of fork timing violations detected
//! by async STA. The primary strategy is buffer insertion on the fast path
//! to balance fork branch delays.
//!
//! # Algorithm
//!
//! For each fork violation:
//! 1. Identify the fast branch (minimum delay)
//! 2. Calculate delay deficit: `target_delay - fast_delay`
//! 3. Insert buffers on the fast path: `num_buffers = ceil(deficit / buffer_delay)`
//! 4. Update netlist connectivity
//!
//! # Usage
//!
//! ```ignore
//! let result = analyze_async_timing(&netlist, None, &config);
//! if result.has_violations() {
//!     let fixed = fix_fork_violations(&mut netlist, &result, &fix_config);
//!     println!("Inserted {} buffers", fixed.buffers_inserted);
//! }
//! ```

use crate::async_sta::{AsyncStaConfig, AsyncStaResult, ForkViolation, ViolationSeverity};
use crate::gate_netlist::{
    Cell, CellId, CellSafetyClassification, GateNet, GateNetId, GateNetlist,
};
use std::collections::HashMap;

/// Configuration for async STA fix pass
#[derive(Debug, Clone)]
pub struct AsyncStaFixConfig {
    /// Buffer cell type to use (e.g., "BUF_X1")
    pub buffer_cell_type: String,
    /// Buffer delay in ps (from tech library)
    pub buffer_delay_ps: f64,
    /// Library name for inserted buffers
    pub library_name: String,
    /// Only fix violations at or above this severity
    pub min_severity: ViolationSeverity,
    /// Maximum buffers to insert per violation
    pub max_buffers_per_violation: usize,
    /// Maximum total buffers to insert
    pub max_total_buffers: usize,
}

impl Default for AsyncStaFixConfig {
    fn default() -> Self {
        Self {
            buffer_cell_type: "BUF_X1".to_string(),
            buffer_delay_ps: 20.0,
            library_name: "generic_asic".to_string(),
            min_severity: ViolationSeverity::Warning,
            max_buffers_per_violation: 10,
            max_total_buffers: 1000,
        }
    }
}

/// Result of the fix pass
#[derive(Debug, Clone)]
pub struct AsyncStaFixResult {
    /// Number of violations fixed
    pub violations_fixed: usize,
    /// Number of violations skipped (below severity threshold or at max buffers)
    pub violations_skipped: usize,
    /// Total buffers inserted
    pub buffers_inserted: usize,
    /// Details of each fix applied
    pub fixes: Vec<ForkFix>,
}

/// Details of a single fork fix
#[derive(Debug, Clone)]
pub struct ForkFix {
    /// The fork net that was fixed
    pub fork_net_id: GateNetId,
    /// Fork net name
    pub fork_net_name: String,
    /// The fast branch cell that received buffers
    pub fast_branch_cell: CellId,
    /// Number of buffers inserted
    pub buffers_inserted: usize,
    /// Original skew before fix
    pub original_skew_ps: f64,
    /// Estimated skew after fix
    pub estimated_skew_ps: f64,
}

/// Fix fork violations by inserting buffers on fast paths
///
/// This modifies the netlist in place, inserting buffer cells on the fast
/// branches of fork violations to balance the delays.
pub fn fix_fork_violations(
    netlist: &mut GateNetlist,
    sta_result: &AsyncStaResult,
    config: &AsyncStaFixConfig,
) -> AsyncStaFixResult {
    let mut result = AsyncStaFixResult {
        violations_fixed: 0,
        violations_skipped: 0,
        buffers_inserted: 0,
        fixes: Vec::new(),
    };

    // Track next available IDs
    let mut next_cell_id = netlist.cells.iter().map(|c| c.id.0).max().unwrap_or(0) + 1;
    let mut next_net_id = netlist.nets.iter().map(|n| n.id.0).max().unwrap_or(0) + 1;

    for violation in &sta_result.fork_violations {
        // Check severity threshold
        if !should_fix_violation(violation, config) {
            result.violations_skipped += 1;
            continue;
        }

        // Check if we've hit the total buffer limit
        if result.buffers_inserted >= config.max_total_buffers {
            result.violations_skipped += 1;
            continue;
        }

        // Fix this violation
        if let Some(fix) = fix_single_violation(
            netlist,
            violation,
            config,
            &mut next_cell_id,
            &mut next_net_id,
            config.max_total_buffers - result.buffers_inserted,
        ) {
            result.buffers_inserted += fix.buffers_inserted;
            result.violations_fixed += 1;
            result.fixes.push(fix);
        } else {
            result.violations_skipped += 1;
        }
    }

    result
}

/// Check if a violation should be fixed based on config
fn should_fix_violation(violation: &ForkViolation, config: &AsyncStaFixConfig) -> bool {
    match config.min_severity {
        ViolationSeverity::Warning => true, // Fix all
        ViolationSeverity::Error => {
            violation.severity == ViolationSeverity::Error
                || violation.severity == ViolationSeverity::Critical
        }
        ViolationSeverity::Critical => violation.severity == ViolationSeverity::Critical,
    }
}

/// Fix a single fork violation by inserting buffers on the fast path
fn fix_single_violation(
    netlist: &mut GateNetlist,
    violation: &ForkViolation,
    config: &AsyncStaFixConfig,
    next_cell_id: &mut u32,
    next_net_id: &mut u32,
    remaining_buffer_budget: usize,
) -> Option<ForkFix> {
    // Find the fast branch (minimum delay) and slow branch (maximum delay)
    let (fast_cell_id, _, min_delay) = violation
        .branch_delays
        .iter()
        .min_by(|a, b| a.2.partial_cmp(&b.2).unwrap())?;

    let max_delay = violation
        .branch_delays
        .iter()
        .map(|(_, _, d)| *d)
        .fold(f64::NEG_INFINITY, f64::max);

    // Calculate how many buffers we need
    let delay_deficit = max_delay - min_delay;
    let buffers_needed = (delay_deficit / config.buffer_delay_ps).ceil() as usize;
    let buffers_to_insert = buffers_needed
        .min(config.max_buffers_per_violation)
        .min(remaining_buffer_budget);

    if buffers_to_insert == 0 {
        return None;
    }

    // Find the fork net
    let fork_net_idx = netlist
        .nets
        .iter()
        .position(|n| n.id == violation.fork_net)?;

    // Find the fast branch cell's input pin on the fork net
    let fast_cell_idx = netlist.cells.iter().position(|c| c.id == *fast_cell_id)?;
    let fast_cell_input_pin = netlist.nets[fork_net_idx]
        .fanout
        .iter()
        .find(|(cell_id, _)| *cell_id == *fast_cell_id)
        .map(|(_, pin)| *pin)?;

    // Insert buffer chain
    let mut current_input_net = violation.fork_net;

    for i in 0..buffers_to_insert {
        // Create new output net for this buffer
        let buffer_output_net_id = GateNetId(*next_net_id);
        *next_net_id += 1;

        let buffer_output_net = GateNet {
            id: buffer_output_net_id,
            name: format!("{}_buf{}_out", violation.fork_net_name, i),
            driver: Some(CellId(*next_cell_id)),
            driver_pin: Some(0),
            fanout: Vec::new(), // Will be updated below
            is_input: false,
            is_output: false,
            is_clock: false,
            is_reset: false,
            is_detection: false,
            detection_config: None,
            alias_of: None,
        };

        // Create the buffer cell
        let buffer_cell = Cell {
            id: CellId(*next_cell_id),
            cell_type: config.buffer_cell_type.clone(),
            library: config.library_name.clone(),
            fit: 0.0, // Buffer FIT is typically negligible
            failure_modes: Vec::new(),
            inputs: vec![current_input_net],
            outputs: vec![buffer_output_net_id],
            path: format!("{}.timing_fix_buf{}", violation.fork_net_name, i),
            clock: None,
            reset: None,
            source_op: Some("async_sta_fix".to_string()),
            safety_classification: CellSafetyClassification::default(),
        };

        let buffer_cell_id = buffer_cell.id;
        *next_cell_id += 1;

        // Add buffer to netlist
        netlist.cells.push(buffer_cell);
        netlist.nets.push(buffer_output_net);

        // Update current input net's fanout to include this buffer
        if let Some(input_net) = netlist.nets.iter_mut().find(|n| n.id == current_input_net) {
            // For the first buffer, we're connecting to the fork net
            // We need to add this buffer as a fanout
            if i == 0 {
                // The buffer takes input from the fork net
                input_net.fanout.push((buffer_cell_id, 0));
            } else {
                // Subsequent buffers take input from previous buffer's output
                input_net.fanout.push((buffer_cell_id, 0));
            }
        }

        // The next buffer (or final destination) will use this buffer's output
        current_input_net = buffer_output_net_id;
    }

    // Update the fast branch cell to use the last buffer's output instead of fork net
    // 1. Remove fast cell from fork net's fanout
    if let Some(fork_net) = netlist.nets.iter_mut().find(|n| n.id == violation.fork_net) {
        fork_net
            .fanout
            .retain(|(cell_id, _)| *cell_id != *fast_cell_id);
    }

    // 2. Add fast cell to the last buffer's output fanout
    if let Some(last_buffer_net) = netlist.nets.iter_mut().find(|n| n.id == current_input_net) {
        last_buffer_net
            .fanout
            .push((*fast_cell_id, fast_cell_input_pin));
    }

    // 3. Update fast cell's input to use the last buffer's output
    if let Some(fast_cell) = netlist.cells.iter_mut().find(|c| c.id == *fast_cell_id) {
        if let Some(input) = fast_cell.inputs.get_mut(fast_cell_input_pin) {
            *input = current_input_net;
        }
    }

    // Calculate estimated new skew
    let added_delay = buffers_to_insert as f64 * config.buffer_delay_ps;
    let new_fast_delay = min_delay + added_delay;
    let estimated_skew = (max_delay - new_fast_delay).abs();

    Some(ForkFix {
        fork_net_id: violation.fork_net,
        fork_net_name: violation.fork_net_name.clone(),
        fast_branch_cell: *fast_cell_id,
        buffers_inserted: buffers_to_insert,
        original_skew_ps: violation.skew_ps,
        estimated_skew_ps: estimated_skew,
    })
}

/// Run iterative async STA and fix flow
///
/// This runs the full flow:
/// 1. Run async STA
/// 2. If violations found, fix them with buffer insertion
/// 3. Repeat until clean or max iterations reached
///
/// Note: This does NOT re-run simulation between iterations. For oscillation-aware
/// timing, you should run simulation once before calling this, then the fixes
/// are applied based on the initial oscillation data.
pub fn run_iterative_sta_fix(
    netlist: &mut GateNetlist,
    sta_config: &AsyncStaConfig,
    fix_config: &AsyncStaFixConfig,
    max_iterations: usize,
) -> IterativeStaResult {
    use crate::async_sta::analyze_async_timing;

    let mut result = IterativeStaResult {
        iterations: 0,
        total_buffers_inserted: 0,
        final_violations: 0,
        converged: false,
        iteration_results: Vec::new(),
    };

    for iteration in 0..max_iterations {
        result.iterations = iteration + 1;

        // Run async STA
        let sta_result = analyze_async_timing(netlist, None, sta_config);

        let violations_count = sta_result.fork_violations.len();
        let error_count = sta_result.error_count();

        // Check if we're done
        if error_count == 0 {
            result.converged = true;
            result.final_violations = violations_count;
            result.iteration_results.push(IterationResult {
                iteration: iteration + 1,
                violations_before: violations_count,
                violations_after: violations_count,
                buffers_inserted: 0,
            });
            break;
        }

        // Fix violations
        let fix_result = fix_fork_violations(netlist, &sta_result, fix_config);

        result.total_buffers_inserted += fix_result.buffers_inserted;
        result.iteration_results.push(IterationResult {
            iteration: iteration + 1,
            violations_before: violations_count,
            violations_after: violations_count - fix_result.violations_fixed,
            buffers_inserted: fix_result.buffers_inserted,
        });

        // If we couldn't fix any violations, stop
        if fix_result.violations_fixed == 0 {
            result.final_violations = violations_count;
            break;
        }
    }

    // Final check
    if !result.converged {
        let final_sta = analyze_async_timing(netlist, None, sta_config);
        result.final_violations = final_sta.error_count();
        result.converged = result.final_violations == 0;
    }

    result
}

/// Result of iterative STA fix flow
#[derive(Debug, Clone)]
pub struct IterativeStaResult {
    /// Number of iterations run
    pub iterations: usize,
    /// Total buffers inserted across all iterations
    pub total_buffers_inserted: usize,
    /// Number of violations remaining after all iterations
    pub final_violations: usize,
    /// Whether the flow converged (no more errors)
    pub converged: bool,
    /// Per-iteration results
    pub iteration_results: Vec<IterationResult>,
}

impl IterativeStaResult {
    /// Generate a summary report
    pub fn summary(&self) -> String {
        let mut s = String::new();
        s.push_str("=== Iterative Async STA Fix Report ===\n\n");

        for iter_result in &self.iteration_results {
            s.push_str(&format!(
                "Iteration {}: {} violations -> {} violations ({} buffers inserted)\n",
                iter_result.iteration,
                iter_result.violations_before,
                iter_result.violations_after,
                iter_result.buffers_inserted
            ));
        }

        s.push_str(&format!("\nTotal iterations: {}\n", self.iterations));
        s.push_str(&format!(
            "Total buffers inserted: {}\n",
            self.total_buffers_inserted
        ));
        s.push_str(&format!("Final violations: {}\n", self.final_violations));
        s.push_str(&format!(
            "Converged: {}\n",
            if self.converged { "Yes" } else { "No" }
        ));

        s
    }
}

/// Result of a single iteration
#[derive(Debug, Clone)]
pub struct IterationResult {
    /// Iteration number (1-based)
    pub iteration: usize,
    /// Violations before this iteration's fixes
    pub violations_before: usize,
    /// Violations after this iteration's fixes
    pub violations_after: usize,
    /// Buffers inserted in this iteration
    pub buffers_inserted: usize,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::gate_netlist::GateNet;

    fn make_fork_netlist() -> GateNetlist {
        let mut netlist = GateNetlist::new("test".to_string(), "test".to_string());

        // Create a fork: input_net -> cell0 (INV) and cell1 (AND2)
        let mut input_net = GateNet::new_input(GateNetId(0), "fork_net".to_string());
        input_net.fanout = vec![(CellId(0), 0), (CellId(1), 0)];
        netlist.nets.push(input_net);

        // INV cell (fast path: 15ps)
        let inv_cell = Cell {
            id: CellId(0),
            cell_type: "INV_X1".to_string(),
            library: "test".to_string(),
            fit: 0.0,
            failure_modes: vec![],
            inputs: vec![GateNetId(0)],
            outputs: vec![GateNetId(1)],
            path: "inv".to_string(),
            clock: None,
            reset: None,
            source_op: None,
            safety_classification: Default::default(),
        };

        // AND2 cell (slow path: 25ps)
        let and_cell = Cell {
            id: CellId(1),
            cell_type: "AND2_X1".to_string(),
            library: "test".to_string(),
            fit: 0.0,
            failure_modes: vec![],
            inputs: vec![GateNetId(0)],
            outputs: vec![GateNetId(2)],
            path: "and".to_string(),
            clock: None,
            reset: None,
            source_op: None,
            safety_classification: Default::default(),
        };

        netlist.cells.push(inv_cell);
        netlist.cells.push(and_cell);

        // Output nets
        let mut out1 = GateNet::new_output(GateNetId(1), "out1".to_string());
        out1.driver = Some(CellId(0));
        let mut out2 = GateNet::new_output(GateNetId(2), "out2".to_string());
        out2.driver = Some(CellId(1));
        netlist.nets.push(out1);
        netlist.nets.push(out2);

        netlist
    }

    #[test]
    fn test_fix_single_violation() {
        let mut netlist = make_fork_netlist();

        // Create a fake violation
        let violation = ForkViolation {
            fork_net: GateNetId(0),
            fork_net_name: "fork_net".to_string(),
            branch_delays: vec![
                (CellId(0), "INV_X1".to_string(), 25.0), // Fast: 15ps + 10ps wire
                (CellId(1), "AND2_X1".to_string(), 85.0), // Slow: 25ps + 10ps wire + extra
            ],
            skew_ps: 60.0,
            threshold_ps: 50.0,
            severity: ViolationSeverity::Error,
        };

        let config = AsyncStaFixConfig {
            buffer_delay_ps: 20.0,
            ..Default::default()
        };

        let mut next_cell_id = 2;
        let mut next_net_id = 3;

        let fix = fix_single_violation(
            &mut netlist,
            &violation,
            &config,
            &mut next_cell_id,
            &mut next_net_id,
            100,
        );

        assert!(fix.is_some());
        let fix = fix.unwrap();

        // Should insert 3 buffers: ceil(60 / 20) = 3
        assert_eq!(fix.buffers_inserted, 3);
        assert_eq!(fix.fast_branch_cell, CellId(0));

        // Check that buffers were added to netlist
        let buffer_count = netlist
            .cells
            .iter()
            .filter(|c| c.cell_type == "BUF_X1")
            .count();
        assert_eq!(buffer_count, 3);

        // Check that INV now takes input from the last buffer's output
        let inv_cell = netlist.cells.iter().find(|c| c.id == CellId(0)).unwrap();
        assert_ne!(inv_cell.inputs[0], GateNetId(0)); // No longer directly connected to fork

        // Check that fork net no longer has INV in its fanout
        let fork_net = netlist.nets.iter().find(|n| n.id == GateNetId(0)).unwrap();
        assert!(!fork_net.fanout.iter().any(|(id, _)| *id == CellId(0)));
    }

    #[test]
    fn test_buffer_chain_connectivity() {
        let mut netlist = make_fork_netlist();

        let violation = ForkViolation {
            fork_net: GateNetId(0),
            fork_net_name: "fork_net".to_string(),
            branch_delays: vec![
                (CellId(0), "INV_X1".to_string(), 25.0),
                (CellId(1), "AND2_X1".to_string(), 65.0),
            ],
            skew_ps: 40.0,
            threshold_ps: 50.0,
            severity: ViolationSeverity::Warning,
        };

        let config = AsyncStaFixConfig {
            buffer_delay_ps: 20.0,
            ..Default::default()
        };

        let mut next_cell_id = 2;
        let mut next_net_id = 3;

        fix_single_violation(
            &mut netlist,
            &violation,
            &config,
            &mut next_cell_id,
            &mut next_net_id,
            100,
        );

        // Verify buffer chain: fork_net -> buf0 -> buf0_out -> buf1 -> buf1_out -> INV
        // With 2 buffers: ceil(40/20) = 2

        // Find the buffer cells
        let buffers: Vec<_> = netlist
            .cells
            .iter()
            .filter(|c| c.cell_type == "BUF_X1")
            .collect();
        assert_eq!(buffers.len(), 2);

        // First buffer should take input from fork_net
        assert_eq!(buffers[0].inputs[0], GateNetId(0));

        // Second buffer should take input from first buffer's output
        let buf0_output = buffers[0].outputs[0];
        assert_eq!(buffers[1].inputs[0], buf0_output);

        // INV should take input from second buffer's output
        let buf1_output = buffers[1].outputs[0];
        let inv_cell = netlist.cells.iter().find(|c| c.id == CellId(0)).unwrap();
        assert_eq!(inv_cell.inputs[0], buf1_output);
    }
}
