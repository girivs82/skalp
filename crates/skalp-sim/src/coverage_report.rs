//! Coverage report formatting and output
//!
//! Generates human-readable coverage reports for both spec simulation
//! and equivalence checking modes.

use crate::sim_coverage::{CoverageMetrics, SimCoverageDb};
use std::io;
use std::path::Path;

/// An uncovered coverage item for detailed reporting
#[derive(Debug, Clone)]
pub enum UncoveredItem {
    Toggle {
        signal: String,
        bit: usize,
        direction: String,
    },
    MuxArm {
        node: String,
        arm: usize,
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
            });
        }

        // Mux arm uncovered items
        for (node, arms) in behavioral_db.uncovered_mux_arms() {
            for arm in arms {
                uncovered.push(UncoveredItem::MuxArm {
                    node: node.to_string(),
                    arm,
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
            let cmp_count = self
                .uncovered
                .iter()
                .filter(|u| matches!(u, UncoveredItem::Comparison { .. }))
                .count();

            println!(
                "Uncovered items: {} toggle, {} mux arms, {} comparisons",
                toggle_count, mux_count, cmp_count
            );

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
                        } => {
                            println!("  - Toggle: {}[{}] missing {}", signal, bit, direction);
                        }
                        UncoveredItem::MuxArm { node, arm } => {
                            println!("  - Mux: {} arm {} not taken", node, arm);
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
                    } => {
                        writeln!(report, "  Toggle: {}[{}] missing {}", signal, bit, direction)
                            .unwrap();
                    }
                    UncoveredItem::MuxArm { node, arm } => {
                        writeln!(report, "  Mux: {} arm {} not taken", node, arm).unwrap();
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
