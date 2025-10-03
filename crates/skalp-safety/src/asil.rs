//! ASIL (Automotive Safety Integrity Level) definitions and requirements
//!
//! Implements ISO 26262 ASIL levels A through D with associated requirements
//! for hardware design verification and validation.

use serde::{Deserialize, Serialize};
use std::fmt;

/// ASIL levels according to ISO 26262
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub enum AsilLevel {
    /// Quality Management (QM) - No safety requirements
    QM,
    /// ASIL A - Lowest safety integrity level
    A,
    /// ASIL B - Low safety integrity level
    B,
    /// ASIL C - Medium safety integrity level
    C,
    /// ASIL D - Highest safety integrity level
    D,
}

impl fmt::Display for AsilLevel {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AsilLevel::A => write!(f, "ASIL A"),
            AsilLevel::B => write!(f, "ASIL B"),
            AsilLevel::C => write!(f, "ASIL C"),
            AsilLevel::D => write!(f, "ASIL D"),
            AsilLevel::QM => write!(f, "QM"),
        }
    }
}

/// Hardware development requirements for each ASIL level
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AsilRequirements {
    /// ASIL level
    pub level: AsilLevel,
    /// Hardware architectural metrics requirements
    pub spfm_target: Option<f64>, // Single Point Fault Metric
    pub lf_target: Option<f64>, // Latent Fault Metric
    /// Verification requirements
    pub verification_methods: Vec<VerificationMethod>,
    /// Safety analysis requirements
    pub safety_analyses: Vec<SafetyAnalysis>,
    /// Tool qualification requirements
    pub tool_confidence_level: ToolConfidenceLevel,
}

/// Verification methods required for different ASIL levels
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum VerificationMethod {
    /// Inspection of design
    Inspection,
    /// Walkthrough of design
    Walkthrough,
    /// Review of design
    Review,
    /// Simulation testing
    Simulation,
    /// Formal verification
    FormalVerification,
    /// Hardware-in-the-loop testing
    HilTesting,
    /// Fault injection testing
    FaultInjection,
}

/// Safety analyses required for different ASIL levels
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum SafetyAnalysis {
    /// Failure Mode and Effects Analysis
    Fmea,
    /// Failure Mode, Effects and Diagnostic Analysis
    Fmeda,
    /// Fault Tree Analysis
    Fta,
    /// Dependent Failure Analysis
    Dfa,
    /// Common Cause Analysis
    Cca,
}

/// Tool confidence levels according to ISO 26262
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ToolConfidenceLevel {
    /// TCL1 - No specific requirements
    Tcl1,
    /// TCL2 - Validation and verification required
    Tcl2,
    /// TCL3 - Qualification required
    Tcl3,
}

impl AsilLevel {
    /// Get the hardware development requirements for this ASIL level
    pub fn requirements(&self) -> AsilRequirements {
        match self {
            AsilLevel::QM => AsilRequirements {
                level: *self,
                spfm_target: None,
                lf_target: None,
                verification_methods: vec![
                    VerificationMethod::Inspection,
                    VerificationMethod::Simulation,
                ],
                safety_analyses: vec![],
                tool_confidence_level: ToolConfidenceLevel::Tcl1,
            },
            AsilLevel::A => AsilRequirements {
                level: *self,
                spfm_target: Some(90.0),
                lf_target: Some(60.0),
                verification_methods: vec![
                    VerificationMethod::Inspection,
                    VerificationMethod::Walkthrough,
                    VerificationMethod::Simulation,
                ],
                safety_analyses: vec![SafetyAnalysis::Fmea],
                tool_confidence_level: ToolConfidenceLevel::Tcl1,
            },
            AsilLevel::B => AsilRequirements {
                level: *self,
                spfm_target: Some(90.0),
                lf_target: Some(80.0),
                verification_methods: vec![
                    VerificationMethod::Inspection,
                    VerificationMethod::Walkthrough,
                    VerificationMethod::Review,
                    VerificationMethod::Simulation,
                ],
                safety_analyses: vec![SafetyAnalysis::Fmea, SafetyAnalysis::Fmeda],
                tool_confidence_level: ToolConfidenceLevel::Tcl2,
            },
            AsilLevel::C => AsilRequirements {
                level: *self,
                spfm_target: Some(97.0),
                lf_target: Some(80.0),
                verification_methods: vec![
                    VerificationMethod::Inspection,
                    VerificationMethod::Walkthrough,
                    VerificationMethod::Review,
                    VerificationMethod::Simulation,
                    VerificationMethod::FormalVerification,
                ],
                safety_analyses: vec![
                    SafetyAnalysis::Fmea,
                    SafetyAnalysis::Fmeda,
                    SafetyAnalysis::Fta,
                ],
                tool_confidence_level: ToolConfidenceLevel::Tcl2,
            },
            AsilLevel::D => AsilRequirements {
                level: *self,
                spfm_target: Some(99.0),
                lf_target: Some(90.0),
                verification_methods: vec![
                    VerificationMethod::Inspection,
                    VerificationMethod::Walkthrough,
                    VerificationMethod::Review,
                    VerificationMethod::Simulation,
                    VerificationMethod::FormalVerification,
                    VerificationMethod::HilTesting,
                    VerificationMethod::FaultInjection,
                ],
                safety_analyses: vec![
                    SafetyAnalysis::Fmea,
                    SafetyAnalysis::Fmeda,
                    SafetyAnalysis::Fta,
                    SafetyAnalysis::Dfa,
                    SafetyAnalysis::Cca,
                ],
                tool_confidence_level: ToolConfidenceLevel::Tcl3,
            },
        }
    }

    /// Check if this ASIL level is compatible with another (can be decomposed to)
    pub fn is_compatible_with(&self, other: &AsilLevel) -> bool {
        // Higher ASIL levels can satisfy lower requirements
        self >= other
    }

    /// Decompose this ASIL level into multiple lower levels
    pub fn decompose(&self) -> Vec<(AsilLevel, AsilLevel)> {
        match self {
            AsilLevel::D => vec![(AsilLevel::B, AsilLevel::B), (AsilLevel::C, AsilLevel::A)],
            AsilLevel::C => vec![(AsilLevel::A, AsilLevel::A), (AsilLevel::B, AsilLevel::QM)],
            AsilLevel::B => vec![(AsilLevel::A, AsilLevel::QM)],
            AsilLevel::A | AsilLevel::QM => vec![],
        }
    }
}

/// Hardware architectural metrics for safety evaluation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HardwareMetrics {
    /// Single Point Fault Metric (percentage)
    pub spfm: f64,
    /// Latent Fault Metric (percentage)
    pub lf: f64,
    /// Probabilistic Metric for Hardware Failures (FIT)
    pub pmhf: f64,
}

impl HardwareMetrics {
    /// Check if metrics meet ASIL requirements
    pub fn meets_asil_requirements(&self, asil: &AsilLevel) -> bool {
        let requirements = asil.requirements();

        if let Some(spfm_target) = requirements.spfm_target {
            if self.spfm < spfm_target {
                return false;
            }
        }

        if let Some(lf_target) = requirements.lf_target {
            if self.lf < lf_target {
                return false;
            }
        }

        // PMHF requirements vary by ASIL level
        let pmhf_target = match asil {
            AsilLevel::A => 1000.0, // 10^3 FIT
            AsilLevel::B => 100.0,  // 10^2 FIT
            AsilLevel::C => 100.0,  // 10^2 FIT
            AsilLevel::D => 10.0,   // 10^1 FIT
            AsilLevel::QM => f64::INFINITY,
        };

        self.pmhf <= pmhf_target
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_asil_ordering() {
        assert!(AsilLevel::D > AsilLevel::C);
        assert!(AsilLevel::C > AsilLevel::B);
        assert!(AsilLevel::B > AsilLevel::A);
        assert!(AsilLevel::A > AsilLevel::QM);
    }

    #[test]
    fn test_asil_compatibility() {
        assert!(AsilLevel::D.is_compatible_with(&AsilLevel::A));
        assert!(AsilLevel::C.is_compatible_with(&AsilLevel::B));
        assert!(!AsilLevel::A.is_compatible_with(&AsilLevel::D));
    }

    #[test]
    fn test_asil_requirements() {
        let asil_d = AsilLevel::D.requirements();
        assert_eq!(asil_d.spfm_target, Some(99.0));
        assert_eq!(asil_d.lf_target, Some(90.0));
        assert!(asil_d
            .verification_methods
            .contains(&VerificationMethod::FormalVerification));
        assert!(asil_d.safety_analyses.contains(&SafetyAnalysis::Fmeda));
    }

    #[test]
    fn test_asil_decomposition() {
        let decompositions = AsilLevel::D.decompose();
        assert!(!decompositions.is_empty());
        assert!(decompositions.contains(&(AsilLevel::B, AsilLevel::B)));
    }

    #[test]
    fn test_hardware_metrics() {
        let metrics = HardwareMetrics {
            spfm: 95.0,
            lf: 85.0,
            pmhf: 50.0,
        };

        assert!(metrics.meets_asil_requirements(&AsilLevel::B));
        assert!(!metrics.meets_asil_requirements(&AsilLevel::D));
    }
}
