//! Async Static Timing Analysis for NCL Circuits
//!
//! This module implements timing analysis specific to NCL (Null Convention Logic)
//! asynchronous circuits. Unlike synchronous STA which checks setup/hold times
//! relative to clock edges, async STA verifies:
//!
//! 1. **Isochronic Fork Analysis**: All branches of a signal fork have bounded skew
//! 2. **Completion Timing**: Completion detection signals arrive after data settles
//!
//! # Isochronic Fork Assumption
//!
//! NCL circuits rely on the "isochronic fork" assumption: when a signal fans out
//! to multiple destinations, all branches must have approximately equal delays.
//! Violation of this assumption can cause:
//! - Premature completion detection
//! - NULL wavefront overtaking DATA wavefront
//! - Incorrect circuit behavior
//!
//! # Usage
//!
//! ```ignore
//! let config = AsyncStaConfig::default();
//! let result = analyze_async_timing(&netlist, &library, &config);
//! if !result.fork_violations.is_empty() {
//!     println!("{}", result.summary());
//! }
//! ```

use crate::gate_netlist::{Cell, CellId, GateNetId, GateNetlist};
use crate::tech_library::TechLibrary;
use std::collections::{HashMap, HashSet};

/// Configuration for async timing analysis
#[derive(Debug, Clone)]
pub struct AsyncStaConfig {
    /// Maximum allowed skew for isochronic forks (in ps)
    pub max_fork_skew_ps: f64,
    /// Completion timing margin (in ps)
    pub completion_margin_ps: f64,
    /// Default gate delay if not in library (in ps)
    pub default_gate_delay_ps: f64,
    /// Wire delay estimate per fanout (in ps)
    pub wire_delay_per_fanout_ps: f64,
    /// Maximum path depth to trace (prevents infinite loops)
    pub max_trace_depth: usize,
}

impl Default for AsyncStaConfig {
    fn default() -> Self {
        Self {
            max_fork_skew_ps: 50.0,         // Conservative threshold
            completion_margin_ps: 20.0,     // 20ps margin for completion
            default_gate_delay_ps: 25.0,    // Fallback if not in library
            wire_delay_per_fanout_ps: 10.0, // Estimate wire delay
            max_trace_depth: 100,           // Prevent infinite recursion
        }
    }
}

/// Severity level of a timing violation
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ViolationSeverity {
    /// Skew > 50% of threshold - marginal
    Warning,
    /// Skew > threshold - likely issue
    Error,
    /// Skew > 2x threshold - almost certain failure
    Critical,
}

impl ViolationSeverity {
    fn from_skew(skew: f64, threshold: f64) -> Self {
        if skew > threshold * 2.0 {
            ViolationSeverity::Critical
        } else if skew > threshold {
            ViolationSeverity::Error
        } else {
            ViolationSeverity::Warning
        }
    }

    fn as_str(&self) -> &'static str {
        match self {
            ViolationSeverity::Warning => "WARNING",
            ViolationSeverity::Error => "ERROR",
            ViolationSeverity::Critical => "CRITICAL",
        }
    }
}

/// An isochronic fork violation
#[derive(Debug, Clone)]
pub struct ForkViolation {
    /// The forking net ID
    pub fork_net: GateNetId,
    /// The forking net name
    pub fork_net_name: String,
    /// Delays to each destination (cell_id, cell_type, delay_ps)
    pub branch_delays: Vec<(CellId, String, f64)>,
    /// Measured skew (max - min delay)
    pub skew_ps: f64,
    /// Allowed threshold
    pub threshold_ps: f64,
    /// Severity level
    pub severity: ViolationSeverity,
}

impl ForkViolation {
    /// Format violation for display
    pub fn format(&self) -> String {
        let mut s = format!(
            "[{}] Fork at net '{}':\n",
            self.severity.as_str(),
            self.fork_net_name
        );
        for (cell_id, cell_type, delay) in &self.branch_delays {
            s.push_str(&format!(
                "    Branch to {} (cell_{}): {:.1}ps\n",
                cell_type, cell_id.0, delay
            ));
        }
        s.push_str(&format!(
            "    Skew: {:.1}ps (threshold: {:.1}ps)\n",
            self.skew_ps, self.threshold_ps
        ));
        s
    }
}

/// Completion timing violation
#[derive(Debug, Clone)]
pub struct CompletionViolation {
    /// The completion detection cell
    pub completion_cell: CellId,
    /// Cell type name
    pub cell_type: String,
    /// Maximum data path delay to this completion
    pub max_data_delay_ps: f64,
    /// Completion signal arrival time
    pub completion_delay_ps: f64,
    /// Margin (negative = violation)
    pub margin_ps: f64,
}

/// Statistics from async timing analysis
#[derive(Debug, Clone, Default)]
pub struct AsyncStaStats {
    /// Total number of nets analyzed
    pub total_nets: usize,
    /// Number of fork points (fanout > 1)
    pub total_forks: usize,
    /// Number of fork violations
    pub fork_violations: usize,
    /// Number of completion cells analyzed
    pub completion_cells: usize,
    /// Number of completion violations
    pub completion_violations: usize,
    /// Maximum skew found (ps)
    pub max_skew_ps: f64,
    /// Average skew across all forks (ps)
    pub avg_skew_ps: f64,
}

/// Result of async timing analysis
#[derive(Debug, Clone)]
pub struct AsyncStaResult {
    /// Fork violations found
    pub fork_violations: Vec<ForkViolation>,
    /// Completion timing issues
    pub completion_violations: Vec<CompletionViolation>,
    /// Summary statistics
    pub stats: AsyncStaStats,
}

impl AsyncStaResult {
    /// Check if timing is clean (no errors or critical issues)
    pub fn is_clean(&self) -> bool {
        self.fork_violations
            .iter()
            .all(|v| v.severity == ViolationSeverity::Warning)
            && self.completion_violations.is_empty()
    }

    /// Check if there are any violations
    pub fn has_violations(&self) -> bool {
        !self.fork_violations.is_empty() || !self.completion_violations.is_empty()
    }

    /// Count errors and critical violations
    pub fn error_count(&self) -> usize {
        self.fork_violations
            .iter()
            .filter(|v| v.severity != ViolationSeverity::Warning)
            .count()
            + self.completion_violations.len()
    }

    /// Generate summary report
    pub fn summary(&self) -> String {
        let mut s = String::new();
        s.push_str("=== Async STA Report ===\n\n");

        // Fork analysis
        s.push_str("Fork Analysis:\n");
        s.push_str(&format!("  Analyzed {} forks\n", self.stats.total_forks));
        s.push_str(&format!("  Violations: {}\n", self.stats.fork_violations));

        if !self.fork_violations.is_empty() {
            s.push('\n');
            for violation in &self.fork_violations {
                s.push_str(&format!("  {}", violation.format()));
            }
        }

        // Completion timing
        s.push_str("\nCompletion Timing:\n");
        s.push_str(&format!(
            "  Analyzed {} completion detectors\n",
            self.stats.completion_cells
        ));
        if self.completion_violations.is_empty() {
            s.push_str("  All margins positive (OK)\n");
        } else {
            s.push_str(&format!(
                "  Violations: {}\n",
                self.stats.completion_violations
            ));
        }

        // Summary
        s.push_str("\nSummary:\n");
        let errors = self
            .fork_violations
            .iter()
            .filter(|v| v.severity == ViolationSeverity::Error)
            .count();
        let critical = self
            .fork_violations
            .iter()
            .filter(|v| v.severity == ViolationSeverity::Critical)
            .count();
        let warnings = self
            .fork_violations
            .iter()
            .filter(|v| v.severity == ViolationSeverity::Warning)
            .count();

        s.push_str(&format!("  Critical: {}\n", critical));
        s.push_str(&format!("  Errors: {}\n", errors));
        s.push_str(&format!("  Warnings: {}\n", warnings));
        s.push_str(&format!("  Max skew: {:.1}ps\n", self.stats.max_skew_ps));

        s
    }
}

/// Async Static Timing Analyzer
pub struct AsyncSta<'a> {
    config: AsyncStaConfig,
    netlist: &'a GateNetlist,
    library: Option<&'a TechLibrary>,
    /// Cached cell delays
    cell_delays: HashMap<CellId, f64>,
}

impl<'a> AsyncSta<'a> {
    /// Create a new async STA analyzer
    pub fn new(netlist: &'a GateNetlist, config: AsyncStaConfig) -> Self {
        Self {
            config,
            netlist,
            library: None,
            cell_delays: HashMap::new(),
        }
    }

    /// Set the technology library for delay lookup
    pub fn with_library(mut self, library: &'a TechLibrary) -> Self {
        self.library = Some(library);
        self
    }

    /// Run the analysis
    pub fn analyze(&mut self) -> AsyncStaResult {
        // Build delay map
        self.build_delay_map();

        // Find and analyze forks
        let forks = self.find_forks();
        let mut fork_violations = Vec::new();
        let mut max_skew = 0.0f64;
        let mut total_skew = 0.0f64;

        for fork_net_id in &forks {
            if let Some(violation) = self.analyze_fork(*fork_net_id) {
                max_skew = max_skew.max(violation.skew_ps);
                total_skew += violation.skew_ps;
                fork_violations.push(violation);
            }
        }

        // Analyze completion timing
        let completion_violations = self.analyze_completion_timing();

        // Build stats
        let stats = AsyncStaStats {
            total_nets: self.netlist.nets.len(),
            total_forks: forks.len(),
            fork_violations: fork_violations.len(),
            completion_cells: self.count_completion_cells(),
            completion_violations: completion_violations.len(),
            max_skew_ps: max_skew,
            avg_skew_ps: if !forks.is_empty() {
                total_skew / forks.len() as f64
            } else {
                0.0
            },
        };

        AsyncStaResult {
            fork_violations,
            completion_violations,
            stats,
        }
    }

    /// Build cell delay map from library or defaults
    fn build_delay_map(&mut self) {
        for cell in &self.netlist.cells {
            let delay = self.get_cell_delay_from_type(&cell.cell_type, cell);
            self.cell_delays.insert(cell.id, delay);
        }
    }

    /// Get cell delay from library or use default
    fn get_cell_delay_from_type(&self, cell_type: &str, cell: &Cell) -> f64 {
        // Try library lookup first
        if let Some(lib) = self.library {
            if let Some(lib_cell) = lib.get_cell(cell_type) {
                if let Some(timing) = &lib_cell.timing {
                    if let Some((_, arc)) = timing.arcs.iter().next() {
                        // Estimate load from fanout
                        let fanout = self.estimate_fanout(cell);
                        let load = fanout as f64 * 5.0; // 5fF per fanout estimate
                        return arc.avg_delay_ps(load);
                    }
                }
            }
        }

        // Use default delays based on cell type
        self.default_delay_for_type(cell_type)
    }

    /// Get default delay based on cell type pattern
    fn default_delay_for_type(&self, cell_type: &str) -> f64 {
        let upper = cell_type.to_uppercase();

        // Strip suffix like _X1, _X2
        let base = upper.split('_').next().unwrap_or(&upper);

        match base {
            "INV" | "NOT" => 15.0,
            "BUF" | "BUFF" | "BUFFER" => 20.0,
            "AND2" | "NAND2" => 25.0,
            "AND3" | "NAND3" => 30.0,
            "AND4" | "NAND4" => 35.0,
            "OR2" | "NOR2" => 25.0,
            "OR3" | "NOR3" => 30.0,
            "OR4" | "NOR4" => 35.0,
            "XOR" | "XOR2" | "XNOR" | "XNOR2" => 35.0,
            "MUX2" | "MUX" => 35.0,
            "TH12" => 30.0,
            "TH22" => 40.0,
            "TH13" | "TH23" | "TH33" => 50.0,
            "TH14" | "TH24" | "TH34" | "TH44" => 60.0,
            _ if upper.starts_with("FP32") => 100.0,
            _ if upper.contains("COMPLETION") || upper.contains("NCL") => 50.0,
            _ => self.config.default_gate_delay_ps,
        }
    }

    /// Estimate fanout count for a cell
    fn estimate_fanout(&self, cell: &Cell) -> usize {
        let mut fanout = 0;
        for output_net_id in &cell.outputs {
            if let Some(net) = self.netlist.nets.iter().find(|n| n.id == *output_net_id) {
                fanout += net.fanout.len();
            }
        }
        fanout.max(1)
    }

    /// Find all nets that are fork points (fanout > 1)
    fn find_forks(&self) -> Vec<GateNetId> {
        self.netlist
            .nets
            .iter()
            .filter(|net| net.fanout.len() > 1)
            .map(|net| net.id)
            .collect()
    }

    /// Analyze a single fork for skew violations
    fn analyze_fork(&self, net_id: GateNetId) -> Option<ForkViolation> {
        let net = self.netlist.nets.iter().find(|n| n.id == net_id)?;

        if net.fanout.len() <= 1 {
            return None;
        }

        // Calculate delay to each fanout destination
        let mut branch_delays: Vec<(CellId, String, f64)> = Vec::new();

        for (dest_cell_id, _pin) in &net.fanout {
            let mut visited = HashSet::new();
            let delay = self.trace_path_delay(*dest_cell_id, &mut visited, 0);

            // Add wire delay estimate
            let wire_delay = self.config.wire_delay_per_fanout_ps;
            let total_delay = delay + wire_delay;

            let cell_type = self
                .netlist
                .cells
                .iter()
                .find(|c| c.id == *dest_cell_id)
                .map(|c| c.cell_type.clone())
                .unwrap_or_else(|| "UNKNOWN".to_string());

            branch_delays.push((*dest_cell_id, cell_type, total_delay));
        }

        if branch_delays.is_empty() {
            return None;
        }

        // Calculate skew
        let min_delay = branch_delays
            .iter()
            .map(|(_, _, d)| *d)
            .fold(f64::INFINITY, f64::min);
        let max_delay = branch_delays
            .iter()
            .map(|(_, _, d)| *d)
            .fold(f64::NEG_INFINITY, f64::max);
        let skew = max_delay - min_delay;

        // Check if violation threshold exceeded (or warning at 50%)
        let threshold = self.config.max_fork_skew_ps;
        if skew > threshold * 0.5 {
            let severity = ViolationSeverity::from_skew(skew, threshold);
            Some(ForkViolation {
                fork_net: net_id,
                fork_net_name: net.name.clone(),
                branch_delays,
                skew_ps: skew,
                threshold_ps: threshold,
                severity,
            })
        } else {
            None
        }
    }

    /// Trace path delay from a cell to the next synchronization point
    fn trace_path_delay(
        &self,
        cell_id: CellId,
        visited: &mut HashSet<CellId>,
        depth: usize,
    ) -> f64 {
        // Prevent infinite loops and excessive depth
        if visited.contains(&cell_id) || depth > self.config.max_trace_depth {
            return 0.0;
        }
        visited.insert(cell_id);

        // Get this cell's delay
        let cell_delay = self.cell_delays.get(&cell_id).copied().unwrap_or(0.0);

        // Check if this is a synchronization point (stop tracing)
        if self.is_sync_point(cell_id) {
            return cell_delay;
        }

        // Get the cell
        let Some(cell) = self.netlist.cells.iter().find(|c| c.id == cell_id) else {
            return cell_delay;
        };

        // Trace through outputs to find max downstream delay
        let mut max_downstream = 0.0f64;

        for output_net_id in &cell.outputs {
            if let Some(net) = self.netlist.nets.iter().find(|n| n.id == *output_net_id) {
                for (next_cell_id, _) in &net.fanout {
                    let downstream = self.trace_path_delay(*next_cell_id, visited, depth + 1);
                    max_downstream = max_downstream.max(downstream);
                }
            }
        }

        cell_delay + max_downstream
    }

    /// Check if a cell is a synchronization point (where paths reconverge)
    fn is_sync_point(&self, cell_id: CellId) -> bool {
        let Some(cell) = self.netlist.cells.iter().find(|c| c.id == cell_id) else {
            return false;
        };

        let upper = cell.cell_type.to_uppercase();

        // THmn gates are synchronization points (C-elements wait for all inputs)
        if upper.starts_with("TH") {
            // TH12, TH22, TH23, etc. are all sync points
            if upper.len() >= 4 {
                let chars: Vec<char> = upper.chars().collect();
                if chars.len() >= 4 && chars[2].is_ascii_digit() && chars[3].is_ascii_digit() {
                    return true;
                }
            }
        }

        // Completion detection is a sync point
        if upper.contains("COMPLETION") || upper.contains("NCL_COMPLETE") {
            return true;
        }

        // Primary outputs are sync points (check if output net is primary output)
        for output_net_id in &cell.outputs {
            if let Some(net) = self.netlist.nets.iter().find(|n| n.id == *output_net_id) {
                if net.is_output {
                    return true;
                }
            }
        }

        false
    }

    /// Count completion detection cells
    fn count_completion_cells(&self) -> usize {
        self.netlist
            .cells
            .iter()
            .filter(|c| {
                let upper = c.cell_type.to_uppercase();
                upper.contains("COMPLETION") || upper.contains("NCL_COMPLETE")
            })
            .count()
    }

    /// Analyze completion timing
    fn analyze_completion_timing(&self) -> Vec<CompletionViolation> {
        let mut violations = Vec::new();

        for cell in &self.netlist.cells {
            let upper = cell.cell_type.to_uppercase();
            if !upper.contains("COMPLETION") && !upper.contains("NCL_COMPLETE") {
                continue;
            }

            // For each completion cell, find max data path delay to it
            let mut max_data_delay = 0.0f64;

            for input_net_id in &cell.inputs {
                if let Some(net) = self.netlist.nets.iter().find(|n| n.id == *input_net_id) {
                    if let Some(driver_id) = net.driver {
                        let mut visited = HashSet::new();
                        let delay = self.trace_backward_delay(driver_id, &mut visited, 0);
                        max_data_delay = max_data_delay.max(delay);
                    }
                }
            }

            // Completion delay is just this cell's delay
            let completion_delay = self.cell_delays.get(&cell.id).copied().unwrap_or(0.0);

            // Check margin
            let margin = completion_delay - max_data_delay - self.config.completion_margin_ps;

            if margin < 0.0 {
                violations.push(CompletionViolation {
                    completion_cell: cell.id,
                    cell_type: cell.cell_type.clone(),
                    max_data_delay_ps: max_data_delay,
                    completion_delay_ps: completion_delay,
                    margin_ps: margin,
                });
            }
        }

        violations
    }

    /// Trace backward from a cell to find path delay (for completion timing)
    fn trace_backward_delay(
        &self,
        cell_id: CellId,
        visited: &mut HashSet<CellId>,
        depth: usize,
    ) -> f64 {
        if visited.contains(&cell_id) || depth > self.config.max_trace_depth {
            return 0.0;
        }
        visited.insert(cell_id);

        let cell_delay = self.cell_delays.get(&cell_id).copied().unwrap_or(0.0);

        let Some(cell) = self.netlist.cells.iter().find(|c| c.id == cell_id) else {
            return cell_delay;
        };

        // Check if we've reached a primary input (stop tracing)
        for input_net_id in &cell.inputs {
            if let Some(net) = self.netlist.nets.iter().find(|n| n.id == *input_net_id) {
                if net.is_input {
                    return cell_delay; // Reached input, stop
                }
            }
        }

        // Trace backward through inputs
        let mut max_upstream = 0.0f64;

        for input_net_id in &cell.inputs {
            if let Some(net) = self.netlist.nets.iter().find(|n| n.id == *input_net_id) {
                if let Some(driver_id) = net.driver {
                    let upstream = self.trace_backward_delay(driver_id, visited, depth + 1);
                    max_upstream = max_upstream.max(upstream);
                }
            }
        }

        cell_delay + max_upstream
    }
}

/// Convenience function to analyze async timing
pub fn analyze_async_timing(
    netlist: &GateNetlist,
    library: Option<&TechLibrary>,
    config: &AsyncStaConfig,
) -> AsyncStaResult {
    let mut sta = AsyncSta::new(netlist, config.clone());
    if let Some(lib) = library {
        sta = sta.with_library(lib);
    }
    sta.analyze()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::gate_netlist::GateNet;

    fn make_test_netlist() -> GateNetlist {
        let mut netlist = GateNetlist::new("test".to_string(), "test".to_string());

        // Create input net
        let input_net = GateNet::new_input(GateNetId(0), "input".to_string());
        netlist.nets.push(input_net);

        // Create two cells
        let cell1 = Cell {
            id: CellId(0),
            cell_type: "INV_X1".to_string(),
            library: "test".to_string(),
            fit: 0.0,
            failure_modes: vec![],
            inputs: vec![GateNetId(0)],
            outputs: vec![GateNetId(1)],
            path: String::new(),
            clock: None,
            reset: None,
            source_op: None,
            safety_classification: Default::default(),
        };

        let cell2 = Cell {
            id: CellId(1),
            cell_type: "AND2_X1".to_string(),
            library: "test".to_string(),
            fit: 0.0,
            failure_modes: vec![],
            inputs: vec![GateNetId(0)],
            outputs: vec![GateNetId(2)],
            path: String::new(),
            clock: None,
            reset: None,
            source_op: None,
            safety_classification: Default::default(),
        };

        netlist.cells.push(cell1);
        netlist.cells.push(cell2);

        // Create output nets
        let mut out1 = GateNet::new_output(GateNetId(1), "out1".to_string());
        out1.driver = Some(CellId(0));

        let mut out2 = GateNet::new_output(GateNetId(2), "out2".to_string());
        out2.driver = Some(CellId(1));

        netlist.nets.push(out1);
        netlist.nets.push(out2);

        // Set up fanout on input net
        netlist.nets[0].fanout = vec![(CellId(0), 0), (CellId(1), 0)];

        netlist
    }

    #[test]
    fn test_fork_detection() {
        let netlist = make_test_netlist();
        let sta = AsyncSta::new(&netlist, AsyncStaConfig::default());
        let forks = sta.find_forks();

        // Input net has fanout to 2 cells
        assert_eq!(forks.len(), 1);
        assert_eq!(forks[0], GateNetId(0));
    }

    #[test]
    fn test_default_delays() {
        let netlist = make_test_netlist();
        let sta = AsyncSta::new(&netlist, AsyncStaConfig::default());

        assert_eq!(sta.default_delay_for_type("INV_X1"), 15.0);
        assert_eq!(sta.default_delay_for_type("AND2_X1"), 25.0);
        assert_eq!(sta.default_delay_for_type("TH22_X1"), 40.0);
        assert_eq!(sta.default_delay_for_type("FP32_ADD"), 100.0);
    }

    #[test]
    fn test_sync_point_detection() {
        let mut netlist = GateNetlist::new("test".to_string(), "test".to_string());

        // Add TH22 cell
        let cell = Cell {
            id: CellId(0),
            cell_type: "TH22_X1".to_string(),
            library: "test".to_string(),
            fit: 0.0,
            failure_modes: vec![],
            inputs: vec![],
            outputs: vec![],
            path: String::new(),
            clock: None,
            reset: None,
            source_op: None,
            safety_classification: Default::default(),
        };
        netlist.cells.push(cell);

        let sta = AsyncSta::new(&netlist, AsyncStaConfig::default());
        assert!(sta.is_sync_point(CellId(0)));
    }

    #[test]
    fn test_analyze_produces_result() {
        let netlist = make_test_netlist();
        let mut sta = AsyncSta::new(&netlist, AsyncStaConfig::default());
        let result = sta.analyze();

        // Should have analyzed at least one fork
        assert!(result.stats.total_forks >= 1);

        // Summary should be non-empty
        let summary = result.summary();
        assert!(summary.contains("Async STA Report"));
    }

    #[test]
    fn test_skew_violation_detection() {
        // Create a netlist with known unbalanced paths
        let mut netlist = GateNetlist::new("test".to_string(), "test".to_string());

        // Input net that forks
        let mut input_net = GateNet::new_input(GateNetId(0), "fork_point".to_string());

        // Short path: just an inverter (15ps)
        let inv_cell = Cell {
            id: CellId(0),
            cell_type: "INV_X1".to_string(),
            library: "test".to_string(),
            fit: 0.0,
            failure_modes: vec![],
            inputs: vec![GateNetId(0)],
            outputs: vec![GateNetId(1)],
            path: String::new(),
            clock: None,
            reset: None,
            source_op: None,
            safety_classification: Default::default(),
        };

        // Long path: chain of 4 AND gates (4 * 25 = 100ps)
        let and1 = Cell {
            id: CellId(1),
            cell_type: "AND2_X1".to_string(),
            library: "test".to_string(),
            fit: 0.0,
            failure_modes: vec![],
            inputs: vec![GateNetId(0)],
            outputs: vec![GateNetId(2)],
            path: String::new(),
            clock: None,
            reset: None,
            source_op: None,
            safety_classification: Default::default(),
        };

        input_net.fanout = vec![(CellId(0), 0), (CellId(1), 0)];
        netlist.nets.push(input_net);

        // Output nets
        let mut out1 = GateNet::new_output(GateNetId(1), "out_short".to_string());
        out1.driver = Some(CellId(0));

        let mut out2 = GateNet::new_output(GateNetId(2), "out_long".to_string());
        out2.driver = Some(CellId(1));

        netlist.nets.push(out1);
        netlist.nets.push(out2);
        netlist.cells.push(inv_cell);
        netlist.cells.push(and1);

        // Analyze with tight threshold
        let config = AsyncStaConfig {
            max_fork_skew_ps: 5.0, // Very tight - should trigger violation
            ..Default::default()
        };

        let mut sta = AsyncSta::new(&netlist, config);
        let result = sta.analyze();

        // Should detect a violation (15ps INV vs 25ps AND = 10ps skew > 5ps threshold)
        assert!(
            !result.fork_violations.is_empty(),
            "Should detect fork violation"
        );
    }
}
