//! Timing analysis and constraints
//!
//! Provides timing analysis capabilities and constraint handling for both FPGA and ASIC flows.

use crate::{BackendResult, TimingConstraint, TimingResults, TimingSlack};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Timing analysis engine
#[derive(Debug)]
pub struct TimingAnalyzer {
    /// Clock definitions
    clocks: HashMap<String, ClockDefinition>,
    /// Path groups for analysis
    path_groups: Vec<PathGroup>,
    /// Analysis configuration
    config: TimingAnalysisConfig,
}

/// Clock definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ClockDefinition {
    /// Clock name
    pub name: String,
    /// Period in nanoseconds
    pub period_ns: f64,
    /// Duty cycle (0.0-1.0)
    pub duty_cycle: f64,
    /// Clock uncertainty in ns
    pub uncertainty_ns: f64,
    /// Clock source latency in ns
    pub source_latency_ns: f64,
}

/// Path group for timing analysis
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PathGroup {
    /// Group name
    pub name: String,
    /// Group type
    pub group_type: PathGroupType,
    /// Weight for optimization
    pub weight: f64,
}

/// Path group types
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum PathGroupType {
    /// Register to register paths
    RegToReg,
    /// Input to register paths
    InputToReg,
    /// Register to output paths
    RegToOutput,
    /// Input to output paths (combinational)
    InputToOutput,
    /// Clock paths
    Clock,
}

/// Timing analysis configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TimingAnalysisConfig {
    /// Operating conditions
    pub operating_conditions: OperatingConditions,
    /// Analysis corners
    pub corners: Vec<AnalysisCorner>,
    /// Report configuration
    pub report_config: ReportConfig,
}

/// Operating conditions
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OperatingConditions {
    /// Temperature in Celsius
    pub temperature_c: f64,
    /// Supply voltage in volts
    pub voltage_v: f64,
    /// Process corner
    pub process_corner: ProcessCorner,
}

/// Process corners
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ProcessCorner {
    /// Typical process
    Typical,
    /// Fast process
    Fast,
    /// Slow process
    Slow,
    /// Fast NMOS, Slow PMOS
    FastSlow,
    /// Slow NMOS, Fast PMOS
    SlowFast,
}

/// Analysis corner
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AnalysisCorner {
    /// Corner name
    pub name: String,
    /// Operating conditions
    pub conditions: OperatingConditions,
    /// Analysis type
    pub analysis_type: AnalysisType,
}

/// Analysis types
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum AnalysisType {
    /// Setup time analysis
    Setup,
    /// Hold time analysis
    Hold,
    /// Maximum delay analysis
    MaxDelay,
    /// Minimum delay analysis
    MinDelay,
}

/// Report configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ReportConfig {
    /// Number of paths to report
    pub max_paths: usize,
    /// Report unconstrained paths
    pub report_unconstrained: bool,
    /// Report path details
    pub include_path_details: bool,
    /// Slack threshold for reporting
    pub slack_threshold_ns: f64,
}

impl Default for TimingAnalysisConfig {
    fn default() -> Self {
        Self {
            operating_conditions: OperatingConditions {
                temperature_c: 25.0,
                voltage_v: 1.2,
                process_corner: ProcessCorner::Typical,
            },
            corners: vec![
                AnalysisCorner {
                    name: "setup_slow".to_string(),
                    conditions: OperatingConditions {
                        temperature_c: 125.0,
                        voltage_v: 1.08,
                        process_corner: ProcessCorner::Slow,
                    },
                    analysis_type: AnalysisType::Setup,
                },
                AnalysisCorner {
                    name: "hold_fast".to_string(),
                    conditions: OperatingConditions {
                        temperature_c: -40.0,
                        voltage_v: 1.32,
                        process_corner: ProcessCorner::Fast,
                    },
                    analysis_type: AnalysisType::Hold,
                },
            ],
            report_config: ReportConfig {
                max_paths: 100,
                report_unconstrained: true,
                include_path_details: true,
                slack_threshold_ns: -0.1, // Report violations worse than -100ps
            },
        }
    }
}

impl TimingAnalyzer {
    /// Create new timing analyzer
    pub fn new(config: TimingAnalysisConfig) -> Self {
        Self {
            clocks: HashMap::new(),
            path_groups: Vec::new(),
            config,
        }
    }

    /// Add clock definition
    pub fn add_clock(&mut self, clock: ClockDefinition) {
        self.clocks.insert(clock.name.clone(), clock);
    }

    /// Apply timing constraints
    pub fn apply_constraints(&mut self, constraints: &[TimingConstraint]) -> BackendResult<()> {
        for constraint in constraints {
            match constraint {
                TimingConstraint::ClockPeriod { clock_name, period_ns } => {
                    let clock = ClockDefinition {
                        name: clock_name.clone(),
                        period_ns: *period_ns,
                        duty_cycle: 0.5,
                        uncertainty_ns: period_ns * 0.05, // 5% uncertainty
                        source_latency_ns: 0.0,
                    };
                    self.add_clock(clock);
                }
                TimingConstraint::InputDelay { port_name, delay_ns, clock_name } => {
                    // Would create input delay constraint
                    println!("Input delay constraint: {} = {} ns relative to {}",
                            port_name, delay_ns, clock_name);
                }
                TimingConstraint::OutputDelay { port_name, delay_ns, clock_name } => {
                    // Would create output delay constraint
                    println!("Output delay constraint: {} = {} ns relative to {}",
                            port_name, delay_ns, clock_name);
                }
                TimingConstraint::FalsePath { from, to } => {
                    // Would create false path constraint
                    println!("False path: {} to {}", from, to);
                }
                TimingConstraint::MulticyclePath { from, to, cycles } => {
                    // Would create multicycle path constraint
                    println!("Multicycle path: {} to {} ({} cycles)", from, to, cycles);
                }
            }
        }
        Ok(())
    }

    /// Run timing analysis
    pub fn analyze_timing(&self, netlist_path: &str) -> BackendResult<TimingResults> {
        // Mock timing analysis - in real implementation would:
        // 1. Parse netlist and extract timing paths
        // 2. Calculate delays for each path
        // 3. Check setup and hold constraints
        // 4. Generate timing reports

        let max_frequency = self.calculate_max_frequency();
        let critical_path_delay = 1000.0 / max_frequency; // Convert to ns

        let timing_slack = TimingSlack {
            worst_negative_slack_ns: 0.25, // Positive slack
            total_negative_slack_ns: 0.0,
            failing_endpoints: 0,
        };

        Ok(TimingResults {
            max_frequency_mhz: max_frequency,
            critical_path_delay_ns: critical_path_delay,
            setup_violations: vec![],
            hold_violations: vec![],
            timing_slack,
        })
    }

    /// Calculate maximum achievable frequency
    fn calculate_max_frequency(&self) -> f64 {
        // Find the most restrictive clock constraint
        let min_period = self.clocks.values()
            .map(|clock| clock.period_ns)
            .min_by(|a, b| a.partial_cmp(b).unwrap())
            .unwrap_or(10.0); // Default 100 MHz

        1000.0 / min_period // Convert to MHz
    }

    /// Generate timing report
    pub fn generate_report(&self, results: &TimingResults) -> String {
        let mut report = String::new();

        report.push_str("===============================================\n");
        report.push_str("           TIMING ANALYSIS REPORT\n");
        report.push_str("===============================================\n\n");

        // Clock summary
        report.push_str("Clock Summary:\n");
        report.push_str("--------------\n");
        for clock in self.clocks.values() {
            report.push_str(&format!("  {}: {:.3} ns ({:.1} MHz)\n",
                                   clock.name, clock.period_ns, 1000.0 / clock.period_ns));
        }
        report.push_str("\n");

        // Timing summary
        report.push_str("Timing Summary:\n");
        report.push_str("---------------\n");
        report.push_str(&format!("  Maximum Frequency: {:.1} MHz\n", results.max_frequency_mhz));
        report.push_str(&format!("  Critical Path Delay: {:.3} ns\n", results.critical_path_delay_ns));
        report.push_str(&format!("  Worst Negative Slack: {:.3} ns\n", results.timing_slack.worst_negative_slack_ns));
        report.push_str(&format!("  Setup Violations: {}\n", results.setup_violations.len()));
        report.push_str(&format!("  Hold Violations: {}\n", results.hold_violations.len()));
        report.push_str("\n");

        // Violations
        if !results.setup_violations.is_empty() {
            report.push_str("Setup Violations:\n");
            report.push_str("-----------------\n");
            for violation in &results.setup_violations {
                report.push_str(&format!("  {} -> {}: slack = {:.3} ns\n",
                                       violation.from, violation.to, violation.slack_ns));
            }
            report.push_str("\n");
        }

        if !results.hold_violations.is_empty() {
            report.push_str("Hold Violations:\n");
            report.push_str("----------------\n");
            for violation in &results.hold_violations {
                report.push_str(&format!("  {} -> {}: slack = {:.3} ns\n",
                                       violation.from, violation.to, violation.slack_ns));
            }
            report.push_str("\n");
        }

        report.push_str("===============================================\n");
        report
    }
}

/// Create timing analyzer with default configuration
pub fn create_timing_analyzer() -> TimingAnalyzer {
    TimingAnalyzer::new(TimingAnalysisConfig::default())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_timing_analyzer_creation() {
        let analyzer = create_timing_analyzer();
        assert_eq!(analyzer.clocks.len(), 0);
        assert_eq!(analyzer.config.operating_conditions.temperature_c, 25.0);
    }

    #[test]
    fn test_clock_addition() {
        let mut analyzer = create_timing_analyzer();
        let clock = ClockDefinition {
            name: "clk".to_string(),
            period_ns: 10.0,
            duty_cycle: 0.5,
            uncertainty_ns: 0.5,
            source_latency_ns: 0.0,
        };

        analyzer.add_clock(clock);
        assert_eq!(analyzer.clocks.len(), 1);
        assert!(analyzer.clocks.contains_key("clk"));
    }

    #[test]
    fn test_constraint_application() {
        let mut analyzer = create_timing_analyzer();
        let constraints = vec![
            TimingConstraint::ClockPeriod {
                clock_name: "clk".to_string(),
                period_ns: 8.0,
            },
            TimingConstraint::InputDelay {
                port_name: "data_in".to_string(),
                delay_ns: 2.0,
                clock_name: "clk".to_string(),
            },
        ];

        let result = analyzer.apply_constraints(&constraints);
        assert!(result.is_ok());
        assert_eq!(analyzer.clocks.len(), 1);
    }

    #[test]
    fn test_timing_analysis() {
        let mut analyzer = create_timing_analyzer();
        let clock = ClockDefinition {
            name: "clk".to_string(),
            period_ns: 5.0, // 200 MHz
            duty_cycle: 0.5,
            uncertainty_ns: 0.25,
            source_latency_ns: 0.0,
        };
        analyzer.add_clock(clock);

        let results = analyzer.analyze_timing("mock_netlist.v").unwrap();
        assert_eq!(results.max_frequency_mhz, 200.0);
        assert_eq!(results.critical_path_delay_ns, 5.0);
    }

    #[test]
    fn test_report_generation() {
        let analyzer = create_timing_analyzer();
        let results = TimingResults {
            max_frequency_mhz: 150.0,
            critical_path_delay_ns: 6.67,
            setup_violations: vec![],
            hold_violations: vec![],
            timing_slack: TimingSlack {
                worst_negative_slack_ns: 1.0,
                total_negative_slack_ns: 0.0,
                failing_endpoints: 0,
            },
        };

        let report = analyzer.generate_report(&results);
        assert!(report.contains("TIMING ANALYSIS REPORT"));
        assert!(report.contains("150.0 MHz"));
        assert!(report.contains("6.670 ns"));
    }
}