//! NCL Routing Constraints for Isochronic Fork Enforcement
//!
//! Generates placement and routing constraints from async STA analysis results
//! to enforce the isochronic fork assumption in NCL circuits. Produces constraints
//! for both skalp's native PnR engine and nextpnr.
//!
//! # Isochronic Fork Constraints
//!
//! When a signal fans out to multiple NCL gates, all branches must have bounded
//! skew. This module translates STA fork analysis into concrete PnR constraints:
//!
//! - **Matched-delay groups**: nets that must be routed with similar wire lengths
//! - **Max skew bounds**: per-group skew tolerance in picoseconds
//! - **Proximity groups**: cells that should be placed close together
//! - **Completion guard margins**: minimum delay on completion detection paths
//!
//! # Dual Output
//!
//! Constraints are emitted in two formats:
//! 1. **Skalp PnR native**: `NclRoutingConstraints` struct consumed directly by
//!    skalp-place-route's placer and router
//! 2. **nextpnr PDC/JSON**: Physical Design Constraints for Lattice/nextpnr flow

use crate::async_sta::{AsyncStaResult, ForkViolation, ViolationSeverity};
use crate::gate_netlist::{CellId, GateNetId, GateNetlist};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

// ─── Constraint IR (technology-independent) ─────────────────────────────────

/// A group of net branches that must have matched routing delay
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MatchedDelayGroup {
    /// Human-readable group name (e.g., "fork_at_net42")
    pub name: String,
    /// The source (forking) net ID
    pub fork_net: GateNetId,
    /// The source net name
    pub fork_net_name: String,
    /// Destination cell IDs (the fanout targets)
    pub dest_cells: Vec<CellId>,
    /// Destination cell names
    pub dest_cell_names: Vec<String>,
    /// Maximum allowed skew between any two branches (ps)
    pub max_skew_ps: f64,
    /// Priority: higher = more critical to satisfy
    pub priority: u32,
    /// Per-destination cell/logic delay in ps (parallel to `dest_cells`).
    /// The router uses these to compute total skew = cell_delay + wire_delay
    /// per branch, then equalises the total across branches.
    /// Empty when cell delays are unavailable (router falls back to wire-only skew).
    #[serde(default)]
    pub branch_cell_delays_ps: Vec<f64>,
}

/// A group of cells that should be placed in proximity
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProximityGroup {
    /// Group name
    pub name: String,
    /// Cell IDs that should be placed close
    pub cells: Vec<CellId>,
    /// Cell names
    pub cell_names: Vec<String>,
    /// Maximum Manhattan distance between any two cells (in tiles)
    pub max_distance_tiles: u32,
}

/// Guard margin on completion detection paths
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CompletionGuard {
    /// Completion detection cell ID
    pub completion_cell: CellId,
    /// Completion cell name
    pub completion_cell_name: String,
    /// Minimum extra delay to add on completion path (ps)
    /// This ensures completion fires after all data paths have settled
    pub min_guard_delay_ps: f64,
}

/// Complete set of NCL routing constraints
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct NclRoutingConstraints {
    /// Matched-delay net groups (isochronic fork enforcement)
    pub matched_delay_groups: Vec<MatchedDelayGroup>,
    /// Proximity placement groups
    pub proximity_groups: Vec<ProximityGroup>,
    /// Completion detection guard margins
    pub completion_guards: Vec<CompletionGuard>,
    /// Global max skew target (ps) — applies to all unspecified forks
    pub global_max_skew_ps: f64,
}

/// Configuration for constraint generation
#[derive(Debug, Clone)]
pub struct NclConstraintConfig {
    /// Only generate constraints for violations at or above this severity
    pub min_severity: ViolationSeverity,
    /// Skew margin to subtract from threshold to create tighter constraint (ps)
    /// Constraint skew = threshold - margin
    pub skew_margin_ps: f64,
    /// Maximum tile distance for proximity groups
    pub proximity_max_tiles: u32,
    /// Guard margin for completion detection (ps)
    pub completion_guard_margin_ps: f64,
    /// Also generate constraints for non-violating forks with high fanout
    pub constrain_high_fanout: bool,
    /// Fanout threshold above which to constrain even without violation
    pub high_fanout_threshold: usize,
}

impl Default for NclConstraintConfig {
    fn default() -> Self {
        Self {
            min_severity: ViolationSeverity::Warning,
            skew_margin_ps: 10.0,
            proximity_max_tiles: 3,
            completion_guard_margin_ps: 20.0,
            constrain_high_fanout: true,
            high_fanout_threshold: 4,
        }
    }
}

// ─── Constraint Generation ──────────────────────────────────────────────────

/// Generate NCL routing constraints from async STA results
pub fn generate_ncl_constraints(
    netlist: &GateNetlist,
    sta_result: &AsyncStaResult,
    config: &NclConstraintConfig,
) -> NclRoutingConstraints {
    let mut constraints = NclRoutingConstraints {
        global_max_skew_ps: 50.0, // default from AsyncStaConfig
        ..Default::default()
    };

    // Generate matched-delay groups from fork violations
    for (i, violation) in sta_result.fork_violations.iter().enumerate() {
        if !should_constrain(violation, config) {
            continue;
        }

        let dest_cells: Vec<CellId> = violation
            .branch_delays
            .iter()
            .map(|(id, _, _)| *id)
            .collect();
        let dest_cell_names: Vec<String> = violation
            .branch_delays
            .iter()
            .map(|(_, name, _)| name.clone())
            .collect();

        // Constraint is tighter than violation threshold
        let constraint_skew = (violation.threshold_ps - config.skew_margin_ps).max(5.0);

        let priority = match violation.severity {
            ViolationSeverity::Critical => 3,
            ViolationSeverity::Error => 2,
            ViolationSeverity::Warning => 1,
        };

        // Extract per-branch cell/logic delays for the router to compute total skew
        let branch_cell_delays_ps: Vec<f64> = violation
            .branch_delays
            .iter()
            .map(|(_, _, delay)| *delay)
            .collect();

        constraints.matched_delay_groups.push(MatchedDelayGroup {
            name: format!("fork_{}", i),
            fork_net: violation.fork_net,
            fork_net_name: violation.fork_net_name.clone(),
            dest_cells: dest_cells.clone(),
            dest_cell_names: dest_cell_names.clone(),
            max_skew_ps: constraint_skew,
            priority,
            branch_cell_delays_ps,
        });

        // Also create proximity group for the same cells
        constraints.proximity_groups.push(ProximityGroup {
            name: format!("prox_fork_{}", i),
            cells: dest_cells,
            cell_names: dest_cell_names,
            max_distance_tiles: config.proximity_max_tiles,
        });
    }

    // Generate completion guards from completion violations
    for cv in &sta_result.completion_violations {
        let cell_name = find_cell_name(netlist, cv.completion_cell);
        constraints.completion_guards.push(CompletionGuard {
            completion_cell: cv.completion_cell,
            completion_cell_name: cell_name,
            min_guard_delay_ps: cv.margin_ps.abs() + config.completion_guard_margin_ps,
        });
    }

    // Optionally constrain high-fanout NCL nets even without violations
    if config.constrain_high_fanout {
        add_high_fanout_constraints(netlist, &mut constraints, config);
    }

    constraints
}

/// Check if a violation warrants constraint generation
fn should_constrain(violation: &ForkViolation, config: &NclConstraintConfig) -> bool {
    match config.min_severity {
        ViolationSeverity::Warning => true, // constrain all
        ViolationSeverity::Error => violation.severity != ViolationSeverity::Warning,
        ViolationSeverity::Critical => violation.severity == ViolationSeverity::Critical,
    }
}

/// Find cell name by ID in netlist
fn find_cell_name(netlist: &GateNetlist, cell_id: CellId) -> String {
    netlist
        .cells
        .iter()
        .find(|c| c.id == cell_id)
        .map(|c| c.path.clone())
        .unwrap_or_else(|| format!("cell_{}", cell_id.0))
}

/// Add constraints for high-fanout NCL nets that didn't produce violations
fn add_high_fanout_constraints(
    netlist: &GateNetlist,
    constraints: &mut NclRoutingConstraints,
    config: &NclConstraintConfig,
) {
    // Track which nets already have constraints
    let constrained_nets: std::collections::HashSet<GateNetId> = constraints
        .matched_delay_groups
        .iter()
        .map(|g| g.fork_net)
        .collect();

    for net in &netlist.nets {
        if net.fanout.len() < config.high_fanout_threshold {
            continue;
        }
        if constrained_nets.contains(&net.id) {
            continue;
        }

        // Only constrain nets in NCL regions (check if any destination is an NCL gate)
        let has_ncl_dest = net.fanout.iter().any(|(cell_id, _)| {
            netlist
                .cells
                .iter()
                .find(|c| c.id == *cell_id)
                .map(|c| is_ncl_cell_type(&c.cell_type))
                .unwrap_or(false)
        });

        if !has_ncl_dest {
            continue;
        }

        let dest_cells: Vec<CellId> = net.fanout.iter().map(|(id, _)| *id).collect();
        let dest_cell_names: Vec<String> = dest_cells
            .iter()
            .map(|id| find_cell_name(netlist, *id))
            .collect();

        let idx = constraints.matched_delay_groups.len();
        constraints.matched_delay_groups.push(MatchedDelayGroup {
            name: format!("hifan_{}", idx),
            fork_net: net.id,
            fork_net_name: net.name.clone(),
            dest_cells: dest_cells.clone(),
            dest_cell_names: dest_cell_names.clone(),
            max_skew_ps: constraints.global_max_skew_ps,
            priority: 0, // low priority — preventive, not reactive
            branch_cell_delays_ps: Vec::new(), // no STA data for high-fanout preventive groups
        });
    }
}

/// Check if a cell type is an NCL gate
fn is_ncl_cell_type(cell_type: &str) -> bool {
    let upper = cell_type.to_uppercase();
    upper.starts_with("TH")
        || upper.contains("NCL")
        || upper.contains("CELEMENT")
        || upper.contains("COMPLETION")
}

// ─── Skalp PnR Export ───────────────────────────────────────────────────────

impl NclRoutingConstraints {
    /// Export as skalp PnR constraint JSON (consumed by skalp-place-route)
    ///
    /// Format:
    /// ```json
    /// {
    ///   "ncl_constraints": {
    ///     "matched_delay_groups": [...],
    ///     "proximity_groups": [...],
    ///     "completion_guards": [...],
    ///     "global_max_skew_ps": 50.0
    ///   }
    /// }
    /// ```
    pub fn to_skalp_json(&self) -> String {
        serde_json::to_string_pretty(self).unwrap_or_else(|_| "{}".to_string())
    }

    /// Export as skalp PnR constraint file (.skcf format)
    ///
    /// Human-readable format that skalp-place-route parses directly.
    pub fn to_skalp_constraints(&self) -> String {
        let mut out = String::new();
        out.push_str("# NCL Routing Constraints (generated by skalp async STA)\n");
        out.push_str("# These constraints enforce the isochronic fork assumption.\n\n");

        out.push_str(&format!(
            "global_max_skew {:.1}ps\n\n",
            self.global_max_skew_ps
        ));

        // Matched-delay groups
        if !self.matched_delay_groups.is_empty() {
            out.push_str("# === Matched-Delay Groups (isochronic forks) ===\n");
            for group in &self.matched_delay_groups {
                out.push_str(&format!(
                    "# Fork at net '{}' (priority {})\n",
                    group.fork_net_name, group.priority
                ));
                out.push_str(&format!("matched_delay_group {} {{\n", group.name));
                out.push_str(&format!("    max_skew {:.1}ps\n", group.max_skew_ps));
                out.push_str(&format!("    source_net {}\n", group.fork_net_name));
                for name in &group.dest_cell_names {
                    out.push_str(&format!("    dest {}\n", name));
                }
                out.push_str("}\n\n");
            }
        }

        // Proximity groups
        if !self.proximity_groups.is_empty() {
            out.push_str("# === Proximity Groups (co-locate fork destinations) ===\n");
            for group in &self.proximity_groups {
                out.push_str(&format!("proximity_group {} {{\n", group.name));
                out.push_str(&format!(
                    "    max_distance {}tiles\n",
                    group.max_distance_tiles
                ));
                for name in &group.cell_names {
                    out.push_str(&format!("    cell {}\n", name));
                }
                out.push_str("}\n\n");
            }
        }

        // Completion guards
        if !self.completion_guards.is_empty() {
            out.push_str("# === Completion Detection Guards ===\n");
            for guard in &self.completion_guards {
                out.push_str(&format!(
                    "completion_guard {} min_delay {:.1}ps\n",
                    guard.completion_cell_name, guard.min_guard_delay_ps
                ));
            }
            out.push('\n');
        }

        out
    }

    /// Check if there are any constraints
    pub fn is_empty(&self) -> bool {
        self.matched_delay_groups.is_empty()
            && self.proximity_groups.is_empty()
            && self.completion_guards.is_empty()
    }

    /// Total number of constraint groups
    pub fn total_groups(&self) -> usize {
        self.matched_delay_groups.len()
            + self.proximity_groups.len()
            + self.completion_guards.len()
    }
}

// ─── nextpnr Export ─────────────────────────────────────────────────────────

impl NclRoutingConstraints {
    /// Export as nextpnr-compatible PDC (Physical Design Constraints)
    ///
    /// nextpnr supports cell attributes and net constraints via JSON.
    /// For Lattice targets, uses LPF (Logical Preference File) syntax.
    pub fn to_nextpnr_pdc(&self) -> String {
        let mut out = String::new();
        out.push_str("// NCL Isochronic Fork Constraints for nextpnr\n");
        out.push_str("// Generated by skalp async STA\n\n");

        // nextpnr uses BLOCK ROUTE / BLOCK PATH style for Lattice
        // and cell attributes for generic targets

        // Matched-delay groups as MAXSKEW constraints
        for group in &self.matched_delay_groups {
            out.push_str(&format!(
                "// Fork group '{}': max skew {:.1}ps\n",
                group.name, group.max_skew_ps
            ));

            // nextpnr PDC: MAXSKEW constraint on nets
            // Lattice LPF format: MAXSKEW NET "name" value ns;
            let skew_ns = group.max_skew_ps / 1000.0;
            out.push_str(&format!(
                "MAXSKEW NET \"{}\" {:.4} ns;\n",
                group.fork_net_name, skew_ns
            ));
        }

        if !self.matched_delay_groups.is_empty() {
            out.push('\n');
        }

        // Proximity groups as LOCATE COMP / REGION constraints
        for group in &self.proximity_groups {
            out.push_str(&format!(
                "// Proximity group '{}': max {}tiles apart\n",
                group.name, group.max_distance_tiles
            ));
            // Use UGROUP to group cells for co-placement
            out.push_str(&format!("UGROUP \"{}\" BBOX {} {} ", group.name, group.max_distance_tiles, group.max_distance_tiles));
            let names: Vec<String> = group.cell_names.iter().map(|n| format!("\"{}\"", n)).collect();
            out.push_str(&names.join(" "));
            out.push_str(";\n");
        }

        if !self.proximity_groups.is_empty() {
            out.push('\n');
        }

        // Completion guards as MINDELAY constraints
        for guard in &self.completion_guards {
            let delay_ns = guard.min_guard_delay_ps / 1000.0;
            out.push_str(&format!(
                "// Completion guard: min {:.1}ps delay\n",
                guard.min_guard_delay_ps
            ));
            out.push_str(&format!(
                "MINDELAY CELL \"{}\" {:.4} ns;\n",
                guard.completion_cell_name, delay_ns
            ));
        }

        out
    }

    /// Export as nextpnr JSON cell attributes
    ///
    /// These are merged into the nextpnr JSON netlist to annotate cells
    /// with routing constraints. nextpnr reads `ROUTE_GROUP` and `MAX_SKEW`
    /// attributes during routing.
    pub fn to_nextpnr_json_attrs(&self) -> HashMap<String, HashMap<String, String>> {
        let mut attrs: HashMap<String, HashMap<String, String>> = HashMap::new();

        for group in &self.matched_delay_groups {
            for name in &group.dest_cell_names {
                let cell_attrs = attrs.entry(name.clone()).or_default();
                cell_attrs.insert("ROUTE_GROUP".to_string(), group.name.clone());
                cell_attrs.insert(
                    "MAX_SKEW_PS".to_string(),
                    format!("{:.0}", group.max_skew_ps),
                );
            }
        }

        for group in &self.proximity_groups {
            for name in &group.cell_names {
                let cell_attrs = attrs.entry(name.clone()).or_default();
                cell_attrs.insert("PLACE_GROUP".to_string(), group.name.clone());
                cell_attrs.insert(
                    "MAX_DISTANCE".to_string(),
                    group.max_distance_tiles.to_string(),
                );
            }
        }

        for guard in &self.completion_guards {
            let cell_attrs = attrs
                .entry(guard.completion_cell_name.clone())
                .or_default();
            cell_attrs.insert(
                "MIN_DELAY_PS".to_string(),
                format!("{:.0}", guard.min_guard_delay_ps),
            );
        }

        attrs
    }

    /// Summary statistics
    pub fn summary(&self) -> String {
        let mut s = String::new();
        s.push_str("=== NCL Routing Constraints ===\n");
        s.push_str(&format!(
            "  Matched-delay groups: {}\n",
            self.matched_delay_groups.len()
        ));
        s.push_str(&format!(
            "  Proximity groups: {}\n",
            self.proximity_groups.len()
        ));
        s.push_str(&format!(
            "  Completion guards: {}\n",
            self.completion_guards.len()
        ));
        s.push_str(&format!(
            "  Global max skew: {:.1}ps\n",
            self.global_max_skew_ps
        ));

        if !self.matched_delay_groups.is_empty() {
            let max_priority = self.matched_delay_groups.iter().map(|g| g.priority).max().unwrap_or(0);
            let critical = self.matched_delay_groups.iter().filter(|g| g.priority == 3).count();
            s.push_str(&format!(
                "  Critical forks (priority 3): {}\n",
                critical
            ));
            s.push_str(&format!("  Max priority: {}\n", max_priority));
        }

        s
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::async_sta::{AsyncStaResult, AsyncStaStats, CompletionViolation, ForkViolation};

    fn make_fork_violation(net_id: u32, skew: f64, severity: ViolationSeverity) -> ForkViolation {
        ForkViolation {
            fork_net: GateNetId(net_id),
            fork_net_name: format!("net_{}", net_id),
            branch_delays: vec![
                (CellId(10), "TH22_a".to_string(), 30.0),
                (CellId(11), "TH12_b".to_string(), 30.0 + skew),
            ],
            skew_ps: skew,
            threshold_ps: 50.0,
            severity,
            corner: None,
        }
    }

    fn make_sta_result(violations: Vec<ForkViolation>) -> AsyncStaResult {
        AsyncStaResult {
            fork_violations: violations,
            completion_violations: vec![],
            stats: AsyncStaStats {
                total_nets: 100,
                total_forks: 10,
                fork_violations: 2,
                completion_cells: 1,
                completion_violations: 0,
                max_skew_ps: 80.0,
                avg_skew_ps: 40.0,
            },
        }
    }

    fn make_empty_netlist() -> GateNetlist {
        GateNetlist::new("test".to_string(), "test_lib".to_string())
    }

    #[test]
    fn test_generate_from_violations() {
        let violations = vec![
            make_fork_violation(1, 60.0, ViolationSeverity::Error),
            make_fork_violation(2, 80.0, ViolationSeverity::Critical),
        ];
        let sta = make_sta_result(violations);
        let netlist = make_empty_netlist();
        let config = NclConstraintConfig {
            constrain_high_fanout: false,
            ..Default::default()
        };

        let constraints = generate_ncl_constraints(&netlist, &sta, &config);

        assert_eq!(constraints.matched_delay_groups.len(), 2);
        assert_eq!(constraints.proximity_groups.len(), 2);

        // Higher severity → higher priority
        assert_eq!(constraints.matched_delay_groups[0].priority, 2); // Error
        assert_eq!(constraints.matched_delay_groups[1].priority, 3); // Critical

        // Constraint skew is tighter than threshold
        assert!(constraints.matched_delay_groups[0].max_skew_ps < 50.0);
    }

    #[test]
    fn test_severity_filtering() {
        let violations = vec![
            make_fork_violation(1, 30.0, ViolationSeverity::Warning),
            make_fork_violation(2, 60.0, ViolationSeverity::Error),
        ];
        let sta = make_sta_result(violations);
        let netlist = make_empty_netlist();
        let config = NclConstraintConfig {
            min_severity: ViolationSeverity::Error,
            constrain_high_fanout: false,
            ..Default::default()
        };

        let constraints = generate_ncl_constraints(&netlist, &sta, &config);
        // Only Error and above should be constrained
        assert_eq!(constraints.matched_delay_groups.len(), 1);
        assert_eq!(
            constraints.matched_delay_groups[0].fork_net_name,
            "net_2"
        );
    }

    #[test]
    fn test_skalp_constraint_output() {
        let violations = vec![make_fork_violation(5, 70.0, ViolationSeverity::Error)];
        let sta = make_sta_result(violations);
        let netlist = make_empty_netlist();
        let config = NclConstraintConfig {
            constrain_high_fanout: false,
            ..Default::default()
        };

        let constraints = generate_ncl_constraints(&netlist, &sta, &config);
        let skcf = constraints.to_skalp_constraints();

        assert!(skcf.contains("matched_delay_group fork_0"));
        assert!(skcf.contains("max_skew"));
        assert!(skcf.contains("net_5"));
        assert!(skcf.contains("proximity_group"));
    }

    #[test]
    fn test_nextpnr_pdc_output() {
        let violations = vec![make_fork_violation(3, 55.0, ViolationSeverity::Error)];
        let sta = make_sta_result(violations);
        let netlist = make_empty_netlist();
        let config = NclConstraintConfig {
            constrain_high_fanout: false,
            ..Default::default()
        };

        let constraints = generate_ncl_constraints(&netlist, &sta, &config);
        let pdc = constraints.to_nextpnr_pdc();

        assert!(pdc.contains("MAXSKEW NET"));
        assert!(pdc.contains("net_3"));
        assert!(pdc.contains("UGROUP"));
    }

    #[test]
    fn test_nextpnr_json_attrs() {
        let violations = vec![make_fork_violation(1, 60.0, ViolationSeverity::Error)];
        let sta = make_sta_result(violations);
        let netlist = make_empty_netlist();
        let config = NclConstraintConfig {
            constrain_high_fanout: false,
            ..Default::default()
        };

        let constraints = generate_ncl_constraints(&netlist, &sta, &config);
        let attrs = constraints.to_nextpnr_json_attrs();

        // Both destination cells should have ROUTE_GROUP attributes
        assert!(attrs.contains_key("TH22_a"));
        assert!(attrs.contains_key("TH12_b"));
        assert_eq!(
            attrs["TH22_a"].get("ROUTE_GROUP").unwrap(),
            "fork_0"
        );
    }

    #[test]
    fn test_completion_guard_generation() {
        let sta = AsyncStaResult {
            fork_violations: vec![],
            completion_violations: vec![CompletionViolation {
                completion_cell: CellId(20),
                cell_type: "NCL_COMPLETE".to_string(),
                max_data_delay_ps: 100.0,
                completion_delay_ps: 80.0,
                margin_ps: -20.0, // negative = violation
            }],
            stats: AsyncStaStats::default(),
        };

        let mut netlist = make_empty_netlist();
        // Add a cell so find_cell_name works
        netlist.cells.push(crate::gate_netlist::Cell::new_comb(
            CellId(20),
            "NCL_COMPLETE".to_string(),
            "test".to_string(),
            0.1,
            "completion_det".to_string(),
            vec![],
            vec![],
        ));

        let config = NclConstraintConfig {
            constrain_high_fanout: false,
            ..Default::default()
        };
        let constraints = generate_ncl_constraints(&netlist, &sta, &config);

        assert_eq!(constraints.completion_guards.len(), 1);
        // Guard delay = |margin| + config margin = 20 + 20 = 40ps
        assert!((constraints.completion_guards[0].min_guard_delay_ps - 40.0).abs() < 0.1);
    }

    #[test]
    fn test_skalp_json_roundtrip() {
        let violations = vec![make_fork_violation(1, 60.0, ViolationSeverity::Error)];
        let sta = make_sta_result(violations);
        let netlist = make_empty_netlist();
        let config = NclConstraintConfig {
            constrain_high_fanout: false,
            ..Default::default()
        };

        let constraints = generate_ncl_constraints(&netlist, &sta, &config);
        let json = constraints.to_skalp_json();

        // Should be valid JSON
        let parsed: NclRoutingConstraints = serde_json::from_str(&json).unwrap();
        assert_eq!(parsed.matched_delay_groups.len(), 1);
        assert_eq!(parsed.proximity_groups.len(), 1);
    }

    #[test]
    fn test_empty_constraints() {
        let sta = make_sta_result(vec![]);
        let netlist = make_empty_netlist();
        let config = NclConstraintConfig {
            constrain_high_fanout: false,
            ..Default::default()
        };

        let constraints = generate_ncl_constraints(&netlist, &sta, &config);
        assert!(constraints.is_empty());
        assert_eq!(constraints.total_groups(), 0);
    }
}
