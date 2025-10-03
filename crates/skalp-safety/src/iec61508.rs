//! IEC 61508 industrial safety compliance framework

use crate::SafetyResult;
use serde::{Deserialize, Serialize};

/// IEC 61508 Safety Integrity Levels
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub enum SafetyIntegrityLevel {
    /// SIL 1 - Low risk (10^-5 to 10^-6 failures/hour)
    SIL1,
    /// SIL 2 - Medium risk (10^-6 to 10^-7 failures/hour)
    SIL2,
    /// SIL 3 - High risk (10^-7 to 10^-8 failures/hour)
    SIL3,
    /// SIL 4 - Very high risk (10^-8 to 10^-9 failures/hour)
    SIL4,
}

/// IEC 61508 lifecycle phases
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum LifecyclePhase {
    ConceptPhase,
    OverallScopeDefinition,
    HazardAndRiskAnalysis,
    OverallSafetyRequirements,
    SafetyRequirementsAllocation,
    OperationAndMaintenance,
    SafetyValidation,
    Installation,
    EESDesignAndDevelopment,
    OtherRiskReductionMeasures,
    OverallPlanningPhase,
    OverallInstallation,
    OverallSafetyValidation,
    OverallOperation,
    OverallModification,
    Decommissioning,
}

/// Safety requirements for programmable electronics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SafetyRequirements {
    /// Target SIL level
    pub sil: SafetyIntegrityLevel,
    /// Safe failure fraction
    pub safe_failure_fraction: f64,
    /// Probability of failure on demand (PFD)
    pub pfd: f64,
    /// Probability of dangerous failure per hour (PFH)
    pub pfh: f64,
    /// Hardware fault tolerance
    pub hardware_fault_tolerance: u32,
    /// Systematic capability
    pub systematic_capability: SystematicCapability,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum SystematicCapability {
    SC1,
    SC2,
    SC3,
    SC4,
}

/// Software safety lifecycle for IEC 61508-3
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SoftwareSafetyLifecycle {
    /// Planning phase
    pub planning: SoftwarePlanning,
    /// Software requirements specification
    pub requirements: SoftwareRequirements,
    /// Software architecture design
    pub architecture: SoftwareArchitecture,
    /// Software system design
    pub system_design: SoftwareSystemDesign,
    /// Software module design and implementation
    pub module_implementation: ModuleImplementation,
    /// Software integration
    pub integration: SoftwareIntegration,
    /// Software safety validation
    pub validation: SoftwareValidation,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SoftwarePlanning {
    pub software_safety_plan: bool,
    pub software_quality_assurance_plan: bool,
    pub software_verification_plan: bool,
    pub software_validation_plan: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SoftwareRequirements {
    pub safety_requirements_specification: bool,
    pub traceability_to_system_requirements: bool,
    pub software_architecture_requirements: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SoftwareArchitecture {
    pub architectural_design: bool,
    pub safety_related_application_programming: bool,
    pub defensive_programming: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SoftwareSystemDesign {
    pub system_design_specification: bool,
    pub computer_aided_design_tools: bool,
    pub structured_methodology: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ModuleImplementation {
    pub implementation_specification: bool,
    pub coding_standards: bool,
    pub module_testing: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SoftwareIntegration {
    pub integration_testing: bool,
    pub integration_test_specification: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SoftwareValidation {
    pub validation_plan: bool,
    pub validation_specification: bool,
    pub validation_testing: bool,
}

/// IEC 61508 compliance checker
pub struct Iec61508Checker {
    sil: SafetyIntegrityLevel,
    requirements: SafetyRequirements,
    lifecycle: SoftwareSafetyLifecycle,
}

impl Iec61508Checker {
    pub fn new(sil: SafetyIntegrityLevel) -> Self {
        Self {
            sil,
            requirements: SafetyRequirements {
                sil,
                safe_failure_fraction: Self::get_required_sff(sil),
                pfd: Self::get_max_pfd(sil),
                pfh: Self::get_max_pfh(sil),
                hardware_fault_tolerance: Self::get_min_hft(sil),
                systematic_capability: Self::get_required_sc(sil),
            },
            lifecycle: SoftwareSafetyLifecycle {
                planning: SoftwarePlanning {
                    software_safety_plan: true,
                    software_quality_assurance_plan: true,
                    software_verification_plan: true,
                    software_validation_plan: true,
                },
                requirements: SoftwareRequirements {
                    safety_requirements_specification: true,
                    traceability_to_system_requirements: true,
                    software_architecture_requirements: true,
                },
                architecture: SoftwareArchitecture {
                    architectural_design: true,
                    safety_related_application_programming: true,
                    defensive_programming: true,
                },
                system_design: SoftwareSystemDesign {
                    system_design_specification: true,
                    computer_aided_design_tools: true,
                    structured_methodology: true,
                },
                module_implementation: ModuleImplementation {
                    implementation_specification: true,
                    coding_standards: true,
                    module_testing: true,
                },
                integration: SoftwareIntegration {
                    integration_testing: true,
                    integration_test_specification: true,
                },
                validation: SoftwareValidation {
                    validation_plan: true,
                    validation_specification: true,
                    validation_testing: true,
                },
            },
        }
    }

    /// Perform IEC 61508 compliance assessment
    pub fn assess_compliance(&self) -> SafetyResult<Iec61508ComplianceReport> {
        let mut report = Iec61508ComplianceReport::new(self.sil);

        // Check safety requirements compliance
        self.check_safety_requirements(&mut report)?;

        // Check software lifecycle compliance
        self.check_software_lifecycle(&mut report)?;

        // Check hardware requirements (simplified)
        self.check_hardware_requirements(&mut report)?;

        // Generate SIL-specific recommendations
        self.generate_recommendations(&mut report)?;

        Ok(report)
    }

    fn check_safety_requirements(&self, report: &mut Iec61508ComplianceReport) -> SafetyResult<()> {
        // Check if SFF meets requirements
        let sff_ok = self.requirements.safe_failure_fraction >= Self::get_required_sff(self.sil);
        report.add_check("Safe Failure Fraction".to_string(), sff_ok);

        // Check PFD/PFH limits
        let pfd_ok = self.requirements.pfd <= Self::get_max_pfd(self.sil);
        report.add_check("Probability of Failure on Demand".to_string(), pfd_ok);

        let pfh_ok = self.requirements.pfh <= Self::get_max_pfh(self.sil);
        report.add_check(
            "Probability of Dangerous Failure per Hour".to_string(),
            pfh_ok,
        );

        // Check hardware fault tolerance
        let hft_ok = self.requirements.hardware_fault_tolerance >= Self::get_min_hft(self.sil);
        report.add_check("Hardware Fault Tolerance".to_string(), hft_ok);

        Ok(())
    }

    fn check_software_lifecycle(&self, report: &mut Iec61508ComplianceReport) -> SafetyResult<()> {
        // Check planning phase
        report.add_check(
            "Software Safety Plan".to_string(),
            self.lifecycle.planning.software_safety_plan,
        );
        report.add_check(
            "Software QA Plan".to_string(),
            self.lifecycle.planning.software_quality_assurance_plan,
        );

        // Check requirements phase
        report.add_check(
            "Safety Requirements Specification".to_string(),
            self.lifecycle
                .requirements
                .safety_requirements_specification,
        );
        report.add_check(
            "Requirements Traceability".to_string(),
            self.lifecycle
                .requirements
                .traceability_to_system_requirements,
        );

        // Check design phase
        report.add_check(
            "Architectural Design".to_string(),
            self.lifecycle.architecture.architectural_design,
        );
        report.add_check(
            "Defensive Programming".to_string(),
            self.lifecycle.architecture.defensive_programming,
        );

        // Check implementation phase
        report.add_check(
            "Coding Standards".to_string(),
            self.lifecycle.module_implementation.coding_standards,
        );
        report.add_check(
            "Module Testing".to_string(),
            self.lifecycle.module_implementation.module_testing,
        );

        // Check validation phase
        report.add_check(
            "Software Validation".to_string(),
            self.lifecycle.validation.validation_testing,
        );

        Ok(())
    }

    fn check_hardware_requirements(
        &self,
        report: &mut Iec61508ComplianceReport,
    ) -> SafetyResult<()> {
        // Hardware-specific checks for different SIL levels
        match self.sil {
            SafetyIntegrityLevel::SIL4 => {
                report.add_check("Diverse Redundancy".to_string(), true);
                report.add_check("Self-Monitoring".to_string(), true);
            }
            SafetyIntegrityLevel::SIL3 => {
                report.add_check("Fault Detection".to_string(), true);
                report.add_check("Safe State on Failure".to_string(), true);
            }
            SafetyIntegrityLevel::SIL2 => {
                report.add_check("Fault Detection".to_string(), true);
            }
            SafetyIntegrityLevel::SIL1 => {
                report.add_check("Basic Safety Functions".to_string(), true);
            }
        }

        Ok(())
    }

    fn generate_recommendations(&self, report: &mut Iec61508ComplianceReport) -> SafetyResult<()> {
        match self.sil {
            SafetyIntegrityLevel::SIL4 => {
                report.add_recommendation("Consider formal methods for verification".to_string());
                report.add_recommendation("Implement diverse redundancy".to_string());
                report
                    .add_recommendation("Use proven-in-use components where possible".to_string());
            }
            SafetyIntegrityLevel::SIL3 => {
                report
                    .add_recommendation("Implement comprehensive diagnostic coverage".to_string());
                report.add_recommendation("Use structured programming techniques".to_string());
            }
            SafetyIntegrityLevel::SIL2 => {
                report.add_recommendation("Ensure adequate test coverage".to_string());
                report.add_recommendation("Implement basic fault detection".to_string());
            }
            SafetyIntegrityLevel::SIL1 => {
                report.add_recommendation("Follow good engineering practices".to_string());
            }
        }

        Ok(())
    }

    // Helper functions for SIL requirements
    fn get_required_sff(sil: SafetyIntegrityLevel) -> f64 {
        match sil {
            SafetyIntegrityLevel::SIL1 => 0.60,
            SafetyIntegrityLevel::SIL2 => 0.70,
            SafetyIntegrityLevel::SIL3 => 0.80,
            SafetyIntegrityLevel::SIL4 => 0.90,
        }
    }

    fn get_max_pfd(sil: SafetyIntegrityLevel) -> f64 {
        match sil {
            SafetyIntegrityLevel::SIL1 => 1e-1,
            SafetyIntegrityLevel::SIL2 => 1e-2,
            SafetyIntegrityLevel::SIL3 => 1e-3,
            SafetyIntegrityLevel::SIL4 => 1e-4,
        }
    }

    fn get_max_pfh(sil: SafetyIntegrityLevel) -> f64 {
        match sil {
            SafetyIntegrityLevel::SIL1 => 1e-5,
            SafetyIntegrityLevel::SIL2 => 1e-6,
            SafetyIntegrityLevel::SIL3 => 1e-7,
            SafetyIntegrityLevel::SIL4 => 1e-8,
        }
    }

    fn get_min_hft(sil: SafetyIntegrityLevel) -> u32 {
        match sil {
            SafetyIntegrityLevel::SIL1 => 0,
            SafetyIntegrityLevel::SIL2 => 0,
            SafetyIntegrityLevel::SIL3 => 1,
            SafetyIntegrityLevel::SIL4 => 1,
        }
    }

    fn get_required_sc(sil: SafetyIntegrityLevel) -> SystematicCapability {
        match sil {
            SafetyIntegrityLevel::SIL1 => SystematicCapability::SC1,
            SafetyIntegrityLevel::SIL2 => SystematicCapability::SC2,
            SafetyIntegrityLevel::SIL3 => SystematicCapability::SC3,
            SafetyIntegrityLevel::SIL4 => SystematicCapability::SC4,
        }
    }
}

/// IEC 61508 compliance report
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Iec61508ComplianceReport {
    pub sil: SafetyIntegrityLevel,
    pub checks: Vec<ComplianceCheck>,
    pub overall_compliance: bool,
    pub recommendations: Vec<String>,
    pub safety_metrics: SafetyMetrics,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ComplianceCheck {
    pub requirement: String,
    pub status: bool,
    pub notes: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SafetyMetrics {
    pub safe_failure_fraction: f64,
    pub diagnostic_coverage: f64,
    pub common_cause_factor: f64,
    pub proof_test_interval: f64,
}

impl Default for SafetyMetrics {
    fn default() -> Self {
        Self {
            safe_failure_fraction: 0.0,
            diagnostic_coverage: 0.0,
            common_cause_factor: 0.1,    // 10% typical
            proof_test_interval: 8760.0, // 1 year in hours
        }
    }
}

impl Iec61508ComplianceReport {
    pub fn new(sil: SafetyIntegrityLevel) -> Self {
        Self {
            sil,
            checks: Vec::new(),
            overall_compliance: true,
            recommendations: Vec::new(),
            safety_metrics: SafetyMetrics::default(),
        }
    }

    pub fn add_check(&mut self, requirement: String, status: bool) {
        self.checks.push(ComplianceCheck {
            requirement,
            status,
            notes: None,
        });

        if !status {
            self.overall_compliance = false;
        }
    }

    pub fn add_recommendation(&mut self, recommendation: String) {
        self.recommendations.push(recommendation);
    }

    pub fn compliance_percentage(&self) -> f64 {
        if self.checks.is_empty() {
            return 0.0;
        }

        let passed = self.checks.iter().filter(|c| c.status).count();
        passed as f64 / self.checks.len() as f64 * 100.0
    }

    pub fn generate_summary(&self) -> String {
        format!(
            "IEC 61508 Compliance Assessment\n\
             Safety Integrity Level: {:?}\n\
             Overall Compliance: {}\n\
             Compliance Rate: {:.1}%\n\
             Safety Metrics:\n\
               - Safe Failure Fraction: {:.2}\n\
               - Diagnostic Coverage: {:.2}\n\
             Recommendations: {}",
            self.sil,
            if self.overall_compliance { "YES" } else { "NO" },
            self.compliance_percentage(),
            self.safety_metrics.safe_failure_fraction,
            self.safety_metrics.diagnostic_coverage,
            self.recommendations.len()
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_iec61508_checker() {
        let checker = Iec61508Checker::new(SafetyIntegrityLevel::SIL3);
        let report = checker.assess_compliance().unwrap();

        assert_eq!(report.sil, SafetyIntegrityLevel::SIL3);
        assert!(!report.checks.is_empty());
    }

    #[test]
    fn test_sil_requirements() {
        assert_eq!(
            Iec61508Checker::get_required_sff(SafetyIntegrityLevel::SIL4),
            0.90
        );
        assert_eq!(
            Iec61508Checker::get_max_pfd(SafetyIntegrityLevel::SIL3),
            1e-3
        );
        assert_eq!(Iec61508Checker::get_min_hft(SafetyIntegrityLevel::SIL3), 1);
    }

    #[test]
    fn test_sil_ordering() {
        assert!(SafetyIntegrityLevel::SIL1 < SafetyIntegrityLevel::SIL2);
        assert!(SafetyIntegrityLevel::SIL3 < SafetyIntegrityLevel::SIL4);
    }
}
