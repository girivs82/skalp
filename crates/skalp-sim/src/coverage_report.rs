//! Coverage report formatting and output
//!
//! Generates human-readable coverage reports for both spec simulation
//! and equivalence checking modes.

use crate::sim_coverage::{CoverageMetrics, SimCoverageDb};
use std::collections::HashMap;
use std::io;
use std::path::Path;

/// Status of an uncovered mux arm after cross-referencing with gate netlist
#[derive(Debug, Clone)]
pub enum MuxArmStatus {
    /// Mux output still present in gate netlist — true coverage gap
    CoverageGap,
    /// Mux output not found in gate netlist — optimized away by synthesis
    OptimizedAway,
    /// Cross-reference not available (e.g., no gate model)
    Unknown,
}

/// An uncovered coverage item for detailed reporting
#[derive(Debug, Clone)]
pub enum UncoveredItem {
    Toggle {
        signal: String,
        bit: usize,
        direction: String,
        status: MuxArmStatus,
    },
    MuxArm {
        node: String,
        arm: usize,
        status: MuxArmStatus,
    },
    Comparison {
        node: String,
        op: String,
        missing: String,
    },
}

/// Complete coverage report
#[derive(Debug, Clone)]
pub struct CoverageReport {
    pub behavioral: CoverageMetrics,
    pub gate: Option<CoverageMetrics>,
    pub uncovered: Vec<UncoveredItem>,
    pub equivalence_ok: bool,
    pub cycles: u64,
    /// Total toggle bits in signals optimized away by synthesis
    pub toggle_optimized_bits: usize,
    /// Covered toggle bits in signals optimized away (for adjusted metric)
    pub toggle_optimized_covered: usize,
}

impl CoverageReport {
    /// Build a coverage report from coverage databases
    pub fn from_coverage_dbs(
        behavioral_db: &SimCoverageDb,
        gate_db: Option<&SimCoverageDb>,
        equivalence_ok: bool,
        cycles: u64,
    ) -> Self {
        let behavioral = behavioral_db.metrics();
        let gate = gate_db.map(|g| g.metrics());

        // Collect uncovered items (limit to most interesting ones)
        let mut uncovered = Vec::new();

        // Toggle uncovered items (limit to 50)
        for (signal, bit, direction) in behavioral_db.uncovered_toggles().into_iter().take(50) {
            uncovered.push(UncoveredItem::Toggle {
                signal,
                bit,
                direction: direction.to_string(),
                status: MuxArmStatus::Unknown,
            });
        }

        // Mux arm uncovered items
        for (node, arms) in behavioral_db.uncovered_mux_arms() {
            for arm in arms {
                uncovered.push(UncoveredItem::MuxArm {
                    node: node.to_string(),
                    arm,
                    status: MuxArmStatus::Unknown,
                });
            }
        }

        // Comparison uncovered items
        for (node, op, seen_true, seen_false) in behavioral_db.uncovered_comparisons() {
            let missing = if !seen_true && !seen_false {
                "both true and false"
            } else if !seen_true {
                "true outcome"
            } else {
                "false outcome"
            };
            uncovered.push(UncoveredItem::Comparison {
                node: node.to_string(),
                op: op.to_string(),
                missing: missing.to_string(),
            });
        }

        CoverageReport {
            behavioral,
            gate,
            uncovered,
            equivalence_ok,
            cycles,
            toggle_optimized_bits: 0,
            toggle_optimized_covered: 0,
        }
    }

    /// Build a coverage report with mux and toggle cross-reference data.
    /// `mux_xref` maps node_name -> MuxArmStatus from gate netlist cross-referencing.
    /// `toggle_xref` maps signal_name -> MuxArmStatus for toggle signals.
    pub fn from_coverage_dbs_with_xref(
        behavioral_db: &SimCoverageDb,
        gate_db: Option<&SimCoverageDb>,
        mux_xref: &HashMap<String, MuxArmStatus>,
        toggle_xref: &HashMap<String, MuxArmStatus>,
        equivalence_ok: bool,
        cycles: u64,
    ) -> Self {
        let behavioral = behavioral_db.metrics();
        let gate = gate_db.map(|g| g.metrics());

        let mut uncovered = Vec::new();

        // Toggle uncovered items with cross-reference status (limit to 50)
        for (signal, bit, direction) in behavioral_db.uncovered_toggles().into_iter().take(50) {
            let status = toggle_xref
                .get(&signal)
                .cloned()
                .unwrap_or(MuxArmStatus::Unknown);
            uncovered.push(UncoveredItem::Toggle {
                signal,
                bit,
                direction: direction.to_string(),
                status,
            });
        }

        // Mux arm uncovered items with cross-reference status
        for (node, arms) in behavioral_db.uncovered_mux_arms() {
            let status = mux_xref
                .get(node)
                .cloned()
                .unwrap_or(MuxArmStatus::Unknown);
            for arm in arms {
                uncovered.push(UncoveredItem::MuxArm {
                    node: node.to_string(),
                    arm,
                    status: status.clone(),
                });
            }
        }

        // Comparison uncovered items
        for (node, op, seen_true, seen_false) in behavioral_db.uncovered_comparisons() {
            let missing = if !seen_true && !seen_false {
                "both true and false"
            } else if !seen_true {
                "true outcome"
            } else {
                "false outcome"
            };
            uncovered.push(UncoveredItem::Comparison {
                node: node.to_string(),
                op: op.to_string(),
                missing: missing.to_string(),
            });
        }

        // Compute toggle bits in optimized-away signals
        let mut toggle_optimized_bits = 0usize;
        let mut toggle_optimized_covered = 0usize;
        let tracked = behavioral_db.tracked_toggle_signals();
        for (sig_name, status) in toggle_xref {
            if matches!(status, MuxArmStatus::OptimizedAway) {
                if let Some(&width) = tracked.get(sig_name.as_str()) {
                    toggle_optimized_bits += width;
                    toggle_optimized_covered +=
                        behavioral_db.toggle_covered_for_signal(sig_name);
                }
            }
        }

        CoverageReport {
            behavioral,
            gate,
            uncovered,
            equivalence_ok,
            cycles,
            toggle_optimized_bits,
            toggle_optimized_covered,
        }
    }

    /// Print a summary to stdout
    pub fn print_summary(&self) {
        println!();
        println!("Coverage Report");
        println!("{}", "=".repeat(60));
        println!();

        // Behavioral coverage
        println!("Behavioral Coverage:");
        println!(
            "  Toggle:     {:6.1}%  ({}/{} bits)",
            self.behavioral.toggle_pct,
            self.behavioral.toggle_covered,
            self.behavioral.toggle_total
        );
        if self.toggle_optimized_bits > 0 {
            let adj_total = self.behavioral.toggle_total - self.toggle_optimized_bits;
            let adj_covered = self.behavioral.toggle_covered - self.toggle_optimized_covered;
            let adj_pct = if adj_total > 0 {
                (adj_covered as f64 / adj_total as f64) * 100.0
            } else {
                100.0
            };
            println!(
                "  Toggle adj: {:6.1}%  ({}/{} bits, {} optimized away)",
                adj_pct, adj_covered, adj_total, self.toggle_optimized_bits
            );
        }
        println!(
            "  Mux arms:   {:6.1}%  ({}/{} arms)",
            self.behavioral.mux_pct,
            self.behavioral.mux_arms_covered,
            self.behavioral.mux_arms_total
        );
        println!(
            "  Comparison: {:6.1}%  ({}/{} outcomes)",
            self.behavioral.comparison_pct,
            self.behavioral.cmp_covered,
            self.behavioral.cmp_total
        );
        println!(
            "  Overall:    {:6.1}%",
            self.behavioral.overall_pct
        );
        println!(
            "  Vectors:    {}",
            self.behavioral.vectors_applied
        );

        // Gate coverage (if present)
        if let Some(gate) = &self.gate {
            println!();
            println!("Gate-Level Coverage:");
            println!(
                "  Toggle:     {:6.1}%  ({}/{} bits)",
                gate.toggle_pct, gate.toggle_covered, gate.toggle_total
            );
            println!(
                "  Vectors:    {}",
                gate.vectors_applied
            );
        }

        // Equivalence result
        println!();
        if self.equivalence_ok {
            println!("Equivalence: PASS ({} cycles)", self.cycles);
        } else {
            println!("Equivalence: FAIL (mismatch at or before cycle {})", self.cycles);
        }

        // Uncovered items summary
        if !self.uncovered.is_empty() {
            println!();
            let toggle_count = self
                .uncovered
                .iter()
                .filter(|u| matches!(u, UncoveredItem::Toggle { .. }))
                .count();
            let mux_count = self
                .uncovered
                .iter()
                .filter(|u| matches!(u, UncoveredItem::MuxArm { .. }))
                .count();
            let mux_optimized = self
                .uncovered
                .iter()
                .filter(|u| matches!(u, UncoveredItem::MuxArm { status: MuxArmStatus::OptimizedAway, .. }))
                .count();
            let mux_gaps = self
                .uncovered
                .iter()
                .filter(|u| matches!(u, UncoveredItem::MuxArm { status: MuxArmStatus::CoverageGap, .. }))
                .count();
            let cmp_count = self
                .uncovered
                .iter()
                .filter(|u| matches!(u, UncoveredItem::Comparison { .. }))
                .count();

            let toggle_optimized = self
                .uncovered
                .iter()
                .filter(|u| matches!(u, UncoveredItem::Toggle { status: MuxArmStatus::OptimizedAway, .. }))
                .count();
            let toggle_gaps = self
                .uncovered
                .iter()
                .filter(|u| matches!(u, UncoveredItem::Toggle { status: MuxArmStatus::CoverageGap, .. }))
                .count();

            println!(
                "Uncovered items: {} toggle, {} mux arms, {} comparisons",
                toggle_count, mux_count, cmp_count
            );
            if toggle_count > 0 && (toggle_optimized > 0 || toggle_gaps > 0) {
                println!(
                    "  Toggle breakdown: {} optimized away, {} coverage gaps, {} unknown",
                    toggle_optimized, toggle_gaps, toggle_count - toggle_optimized - toggle_gaps
                );
            }
            if mux_count > 0 && (mux_optimized > 0 || mux_gaps > 0) {
                println!(
                    "  Mux arm breakdown: {} optimized away, {} coverage gaps, {} unknown",
                    mux_optimized, mux_gaps, mux_count - mux_optimized - mux_gaps
                );
            }

            // Show first few uncovered items
            let show_limit = 10;
            if self.uncovered.len() > 0 {
                println!();
                println!("Top uncovered items:");
                for item in self.uncovered.iter().take(show_limit) {
                    match item {
                        UncoveredItem::Toggle {
                            signal,
                            bit,
                            direction,
                            status,
                        } => {
                            let status_str = match status {
                                MuxArmStatus::CoverageGap => " (coverage gap)",
                                MuxArmStatus::OptimizedAway => " (optimized away)",
                                MuxArmStatus::Unknown => "",
                            };
                            println!("  - Toggle: {}[{}] missing {}{}", signal, bit, direction, status_str);
                        }
                        UncoveredItem::MuxArm { node, arm, status } => {
                            let status_str = match status {
                                MuxArmStatus::CoverageGap => " (coverage gap)",
                                MuxArmStatus::OptimizedAway => " (optimized away)",
                                MuxArmStatus::Unknown => "",
                            };
                            println!("  - Mux: {} arm {} not taken{}", node, arm, status_str);
                        }
                        UncoveredItem::Comparison { node, op, missing } => {
                            println!("  - Comparison: {} ({}) missing {}", node, op, missing);
                        }
                    }
                }
                if self.uncovered.len() > show_limit {
                    println!(
                        "  ... and {} more",
                        self.uncovered.len() - show_limit
                    );
                }
            }
        }

        println!();
        println!("{}", "=".repeat(60));
    }

    /// Write a text report to a file
    pub fn write_text(&self, path: &Path) -> io::Result<()> {
        use std::fmt::Write as FmtWrite;
        use std::fs;

        let mut report = String::new();

        writeln!(report, "Coverage Report").unwrap();
        writeln!(report, "{}", "=".repeat(60)).unwrap();
        writeln!(report).unwrap();

        writeln!(report, "Behavioral Coverage:").unwrap();
        writeln!(
            report,
            "  Toggle:     {:6.1}%  ({}/{} bits)",
            self.behavioral.toggle_pct,
            self.behavioral.toggle_covered,
            self.behavioral.toggle_total
        )
        .unwrap();
        if self.toggle_optimized_bits > 0 {
            let adj_total = self.behavioral.toggle_total - self.toggle_optimized_bits;
            let adj_covered = self.behavioral.toggle_covered - self.toggle_optimized_covered;
            let adj_pct = if adj_total > 0 {
                (adj_covered as f64 / adj_total as f64) * 100.0
            } else {
                100.0
            };
            writeln!(
                report,
                "  Toggle adj: {:6.1}%  ({}/{} bits, {} optimized away)",
                adj_pct, adj_covered, adj_total, self.toggle_optimized_bits
            )
            .unwrap();
        }
        writeln!(
            report,
            "  Mux arms:   {:6.1}%  ({}/{} arms)",
            self.behavioral.mux_pct,
            self.behavioral.mux_arms_covered,
            self.behavioral.mux_arms_total
        )
        .unwrap();
        writeln!(
            report,
            "  Comparison: {:6.1}%  ({}/{} outcomes)",
            self.behavioral.comparison_pct,
            self.behavioral.cmp_covered,
            self.behavioral.cmp_total
        )
        .unwrap();
        writeln!(
            report,
            "  Overall:    {:6.1}%",
            self.behavioral.overall_pct
        )
        .unwrap();
        writeln!(
            report,
            "  Vectors:    {}",
            self.behavioral.vectors_applied
        )
        .unwrap();

        if let Some(gate) = &self.gate {
            writeln!(report).unwrap();
            writeln!(report, "Gate-Level Coverage:").unwrap();
            writeln!(
                report,
                "  Toggle:     {:6.1}%  ({}/{} bits)",
                gate.toggle_pct, gate.toggle_covered, gate.toggle_total
            )
            .unwrap();
            writeln!(report, "  Vectors:    {}", gate.vectors_applied).unwrap();
        }

        writeln!(report).unwrap();
        if self.equivalence_ok {
            writeln!(report, "Equivalence: PASS ({} cycles)", self.cycles).unwrap();
        } else {
            writeln!(
                report,
                "Equivalence: FAIL (mismatch at or before cycle {})",
                self.cycles
            )
            .unwrap();
        }

        // All uncovered items
        if !self.uncovered.is_empty() {
            writeln!(report).unwrap();
            writeln!(report, "Uncovered Items:").unwrap();
            for item in &self.uncovered {
                match item {
                    UncoveredItem::Toggle {
                        signal,
                        bit,
                        direction,
                        status,
                    } => {
                        let status_str = match status {
                            MuxArmStatus::CoverageGap => " (coverage gap)",
                            MuxArmStatus::OptimizedAway => " (optimized away)",
                            MuxArmStatus::Unknown => "",
                        };
                        writeln!(report, "  Toggle: {}[{}] missing {}{}", signal, bit, direction, status_str)
                            .unwrap();
                    }
                    UncoveredItem::MuxArm { node, arm, status } => {
                        let status_str = match status {
                            MuxArmStatus::CoverageGap => " (coverage gap)",
                            MuxArmStatus::OptimizedAway => " (optimized away)",
                            MuxArmStatus::Unknown => "",
                        };
                        writeln!(report, "  Mux: {} arm {} not taken{}", node, arm, status_str).unwrap();
                    }
                    UncoveredItem::Comparison { node, op, missing } => {
                        writeln!(
                            report,
                            "  Comparison: {} ({}) missing {}",
                            node, op, missing
                        )
                        .unwrap();
                    }
                }
            }
        }

        fs::write(path, report)
    }
}
