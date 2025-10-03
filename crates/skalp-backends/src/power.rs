//! Power analysis and optimization
//!
//! Provides power analysis capabilities for both FPGA and ASIC flows.

use crate::{BackendResult, PowerConstraints, PowerResults};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Power analysis engine
#[derive(Debug)]
pub struct PowerAnalyzer {
    /// Power models for different technologies
    models: HashMap<String, PowerModel>,
    /// Analysis configuration
    config: PowerAnalysisConfig,
}

/// Power model for a specific technology
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PowerModel {
    /// Technology name
    pub name: String,
    /// Process node in nm
    pub process_node_nm: u32,
    /// Base parameters
    pub base_params: BasePowerParams,
    /// Cell-specific parameters
    pub cell_params: HashMap<String, CellPowerParams>,
}

/// Base power parameters
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BasePowerParams {
    /// Supply voltage in volts
    pub vdd_v: f64,
    /// Temperature in Celsius
    pub temperature_c: f64,
    /// Base leakage current per um^2 in nA
    pub leakage_per_um2_na: f64,
    /// Switching activity factor (0.0-1.0)
    pub switching_activity: f64,
}

/// Cell-specific power parameters
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CellPowerParams {
    /// Cell name
    pub name: String,
    /// Static power in nW
    pub static_power_nw: f64,
    /// Dynamic power per MHz in nW
    pub dynamic_power_per_mhz_nw: f64,
    /// Input capacitance in fF
    pub input_cap_ff: f64,
    /// Area in um^2
    pub area_um2: f64,
}

/// Power analysis configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PowerAnalysisConfig {
    /// Operating conditions
    pub operating_conditions: Vec<OperatingCondition>,
    /// Analysis modes
    pub analysis_modes: Vec<AnalysisMode>,
    /// Report configuration
    pub report_config: PowerReportConfig,
}

/// Operating condition for power analysis
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OperatingCondition {
    /// Condition name
    pub name: String,
    /// Supply voltage in volts
    pub voltage_v: f64,
    /// Temperature in Celsius
    pub temperature_c: f64,
    /// Frequency in MHz
    pub frequency_mhz: f64,
}

/// Analysis mode
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AnalysisMode {
    /// Mode name
    pub name: String,
    /// Mode type
    pub mode_type: AnalysisModeType,
    /// Switching activity override
    pub switching_activity: Option<f64>,
}

/// Analysis mode types
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum AnalysisModeType {
    /// Active mode (normal operation)
    Active,
    /// Standby mode (clock gated)
    Standby,
    /// Sleep mode (power gated)
    Sleep,
    /// Test mode
    Test,
}

/// Power report configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PowerReportConfig {
    /// Include per-instance breakdown
    pub per_instance_breakdown: bool,
    /// Include per-net breakdown
    pub per_net_breakdown: bool,
    /// Include hierarchy breakdown
    pub hierarchy_breakdown: bool,
    /// Power threshold for reporting in mW
    pub power_threshold_mw: f64,
}

impl Default for PowerAnalysisConfig {
    fn default() -> Self {
        Self {
            operating_conditions: vec![
                OperatingCondition {
                    name: "typical".to_string(),
                    voltage_v: 1.2,
                    temperature_c: 25.0,
                    frequency_mhz: 100.0,
                },
                OperatingCondition {
                    name: "worst_case".to_string(),
                    voltage_v: 1.08,
                    temperature_c: 125.0,
                    frequency_mhz: 100.0,
                },
            ],
            analysis_modes: vec![
                AnalysisMode {
                    name: "active".to_string(),
                    mode_type: AnalysisModeType::Active,
                    switching_activity: Some(0.15),
                },
                AnalysisMode {
                    name: "standby".to_string(),
                    mode_type: AnalysisModeType::Standby,
                    switching_activity: Some(0.01),
                },
            ],
            report_config: PowerReportConfig {
                per_instance_breakdown: true,
                per_net_breakdown: false,
                hierarchy_breakdown: true,
                power_threshold_mw: 0.1,
            },
        }
    }
}

impl PowerAnalyzer {
    /// Create new power analyzer
    pub fn new(config: PowerAnalysisConfig) -> Self {
        let mut analyzer = Self {
            models: HashMap::new(),
            config,
        };

        // Add default power models
        analyzer.add_default_models();
        analyzer
    }

    /// Add default power models for common technologies
    fn add_default_models(&mut self) {
        // Add iCE40 FPGA model
        let ice40_model = PowerModel {
            name: "iCE40".to_string(),
            process_node_nm: 40,
            base_params: BasePowerParams {
                vdd_v: 1.2,
                temperature_c: 25.0,
                leakage_per_um2_na: 0.1,
                switching_activity: 0.15,
            },
            cell_params: HashMap::from([
                (
                    "SB_LUT4".to_string(),
                    CellPowerParams {
                        name: "SB_LUT4".to_string(),
                        static_power_nw: 5.0,
                        dynamic_power_per_mhz_nw: 0.1,
                        input_cap_ff: 2.0,
                        area_um2: 25.0,
                    },
                ),
                (
                    "SB_DFF".to_string(),
                    CellPowerParams {
                        name: "SB_DFF".to_string(),
                        static_power_nw: 8.0,
                        dynamic_power_per_mhz_nw: 0.15,
                        input_cap_ff: 1.5,
                        area_um2: 20.0,
                    },
                ),
            ]),
        };
        self.models.insert("iCE40".to_string(), ice40_model);

        // Add generic ASIC model (45nm)
        let asic_45nm_model = PowerModel {
            name: "ASIC_45nm".to_string(),
            process_node_nm: 45,
            base_params: BasePowerParams {
                vdd_v: 1.1,
                temperature_c: 25.0,
                leakage_per_um2_na: 1.0,
                switching_activity: 0.15,
            },
            cell_params: HashMap::from([
                (
                    "AND2_X1".to_string(),
                    CellPowerParams {
                        name: "AND2_X1".to_string(),
                        static_power_nw: 2.0,
                        dynamic_power_per_mhz_nw: 0.05,
                        input_cap_ff: 1.0,
                        area_um2: 2.5,
                    },
                ),
                (
                    "DFF_X1".to_string(),
                    CellPowerParams {
                        name: "DFF_X1".to_string(),
                        static_power_nw: 15.0,
                        dynamic_power_per_mhz_nw: 0.2,
                        input_cap_ff: 1.2,
                        area_um2: 8.0,
                    },
                ),
            ]),
        };
        self.models.insert("ASIC_45nm".to_string(), asic_45nm_model);
    }

    /// Add custom power model
    pub fn add_model(&mut self, model: PowerModel) {
        self.models.insert(model.name.clone(), model);
    }

    /// Analyze power consumption
    pub fn analyze_power(
        &self,
        technology: &str,
        cell_counts: &HashMap<String, u32>,
        frequency_mhz: f64,
        area_um2: Option<f64>,
    ) -> BackendResult<PowerResults> {
        let model = self.models.get(technology).ok_or_else(|| {
            crate::BackendError::PowerError(format!(
                "Power model not found for technology: {}",
                technology
            ))
        })?;

        let mut total_static_power = 0.0;
        let mut total_dynamic_power = 0.0;
        let mut power_breakdown = HashMap::new();

        // Calculate power for each cell type
        for (cell_type, count) in cell_counts {
            if let Some(cell_params) = model.cell_params.get(cell_type) {
                let static_power = (cell_params.static_power_nw * *count as f64) / 1_000_000.0; // Convert to mW
                let dynamic_power =
                    (cell_params.dynamic_power_per_mhz_nw * *count as f64 * frequency_mhz)
                        / 1_000_000.0; // Convert to mW

                total_static_power += static_power;
                total_dynamic_power += dynamic_power;

                power_breakdown.insert(cell_type.clone(), static_power + dynamic_power);
            }
        }

        // Add base leakage if area is provided
        if let Some(area) = area_um2 {
            let base_leakage = (model.base_params.leakage_per_um2_na * area) / 1_000_000.0; // Convert to mW
            total_static_power += base_leakage;
            power_breakdown.insert("base_leakage".to_string(), base_leakage);
        }

        // Apply temperature scaling
        let temp_scaling = self.calculate_temperature_scaling(
            model.base_params.temperature_c,
            25.0, // Reference temperature
        );
        total_static_power *= temp_scaling;

        // Apply voltage scaling for dynamic power (P ∝ V²)
        let voltage_scaling = (model.base_params.vdd_v / 1.2).powi(2); // Reference voltage 1.2V
        total_dynamic_power *= voltage_scaling;

        let total_power = total_static_power + total_dynamic_power;

        Ok(PowerResults {
            total_power_mw: total_power,
            dynamic_power_mw: total_dynamic_power,
            static_power_mw: total_static_power,
            power_breakdown,
        })
    }

    /// Calculate temperature scaling factor for leakage power
    fn calculate_temperature_scaling(&self, actual_temp_c: f64, ref_temp_c: f64) -> f64 {
        // Simplified exponential temperature dependence
        // Leakage roughly doubles every 10°C
        let temp_diff = actual_temp_c - ref_temp_c;
        2.0_f64.powf(temp_diff / 10.0)
    }

    /// Check power constraints
    pub fn check_constraints(
        &self,
        results: &PowerResults,
        constraints: &Option<PowerConstraints>,
    ) -> Vec<String> {
        let mut violations = Vec::new();

        if let Some(constraints) = constraints {
            if let Some(max_dynamic) = constraints.max_dynamic_power {
                if results.dynamic_power_mw > max_dynamic {
                    violations.push(format!(
                        "Dynamic power violation: {:.2} mW > {:.2} mW",
                        results.dynamic_power_mw, max_dynamic
                    ));
                }
            }

            if let Some(max_static) = constraints.max_static_power {
                if results.static_power_mw > max_static {
                    violations.push(format!(
                        "Static power violation: {:.2} mW > {:.2} mW",
                        results.static_power_mw, max_static
                    ));
                }
            }
        }

        violations
    }

    /// Generate power report
    pub fn generate_report(&self, results: &PowerResults) -> String {
        let mut report = String::new();

        report.push_str("===============================================\n");
        report.push_str("            POWER ANALYSIS REPORT\n");
        report.push_str("===============================================\n\n");

        // Power summary
        report.push_str("Power Summary:\n");
        report.push_str("--------------\n");
        report.push_str(&format!(
            "  Total Power: {:.3} mW\n",
            results.total_power_mw
        ));
        report.push_str(&format!(
            "  Dynamic Power: {:.3} mW ({:.1}%)\n",
            results.dynamic_power_mw,
            (results.dynamic_power_mw / results.total_power_mw) * 100.0
        ));
        report.push_str(&format!(
            "  Static Power: {:.3} mW ({:.1}%)\n",
            results.static_power_mw,
            (results.static_power_mw / results.total_power_mw) * 100.0
        ));
        report.push('\n');

        // Power breakdown
        if !results.power_breakdown.is_empty() {
            report.push_str("Power Breakdown by Component:\n");
            report.push_str("------------------------------\n");

            let mut breakdown_vec: Vec<_> = results.power_breakdown.iter().collect();
            breakdown_vec.sort_by(|a, b| b.1.partial_cmp(a.1).unwrap());

            for (component, power) in breakdown_vec {
                let percentage = (power / results.total_power_mw) * 100.0;
                report.push_str(&format!(
                    "  {:20}: {:8.3} mW ({:5.1}%)\n",
                    component, power, percentage
                ));
            }
            report.push('\n');
        }

        report.push_str("===============================================\n");
        report
    }
}

/// Create power analyzer with default configuration
pub fn create_power_analyzer() -> PowerAnalyzer {
    PowerAnalyzer::new(PowerAnalysisConfig::default())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_power_analyzer_creation() {
        let analyzer = create_power_analyzer();
        assert!(!analyzer.models.is_empty());
        assert!(analyzer.models.contains_key("iCE40"));
        assert!(analyzer.models.contains_key("ASIC_45nm"));
    }

    #[test]
    fn test_temperature_scaling() {
        let analyzer = create_power_analyzer();

        // At reference temperature, scaling should be 1.0
        let scaling = analyzer.calculate_temperature_scaling(25.0, 25.0);
        assert!((scaling - 1.0).abs() < 0.01);

        // At 35°C (10°C higher), scaling should be ~2.0
        let scaling = analyzer.calculate_temperature_scaling(35.0, 25.0);
        assert!((scaling - 2.0).abs() < 0.1);
    }

    #[test]
    fn test_power_analysis() {
        let analyzer = create_power_analyzer();

        let mut cell_counts = HashMap::new();
        cell_counts.insert("AND2_X1".to_string(), 100);
        cell_counts.insert("DFF_X1".to_string(), 50);

        let results = analyzer
            .analyze_power(
                "ASIC_45nm",
                &cell_counts,
                100.0,        // 100 MHz
                Some(1000.0), // 1000 um^2
            )
            .unwrap();

        assert!(results.total_power_mw > 0.0);
        assert!(results.dynamic_power_mw > 0.0);
        assert!(results.static_power_mw > 0.0);
        assert!(!results.power_breakdown.is_empty());
    }

    #[test]
    fn test_power_constraints() {
        let analyzer = create_power_analyzer();

        let results = PowerResults {
            total_power_mw: 150.0,
            dynamic_power_mw: 100.0,
            static_power_mw: 50.0,
            power_breakdown: HashMap::new(),
        };

        let constraints = PowerConstraints {
            max_dynamic_power: Some(80.0), // Violation
            max_static_power: Some(60.0),  // OK
            operating_voltage: 1.2,
            operating_temperature: 25.0,
        };

        let violations = analyzer.check_constraints(&results, &Some(constraints));
        assert_eq!(violations.len(), 1);
        assert!(violations[0].contains("Dynamic power violation"));
    }

    #[test]
    fn test_report_generation() {
        let analyzer = create_power_analyzer();

        let mut power_breakdown = HashMap::new();
        power_breakdown.insert("logic".to_string(), 80.0);
        power_breakdown.insert("memory".to_string(), 40.0);
        power_breakdown.insert("clock".to_string(), 30.0);

        let results = PowerResults {
            total_power_mw: 150.0,
            dynamic_power_mw: 100.0,
            static_power_mw: 50.0,
            power_breakdown,
        };

        let report = analyzer.generate_report(&results);
        assert!(report.contains("POWER ANALYSIS REPORT"));
        assert!(report.contains("150.000 mW"));
        assert!(report.contains("logic"));
        assert!(report.contains("memory"));
    }
}
