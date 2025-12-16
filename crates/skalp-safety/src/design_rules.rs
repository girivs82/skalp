//! Design Rules Module for ISO 26262 Systematic Failure Prevention
//!
//! Implements design rule checking for ASIL C/D compliance.
//! These rules help prevent systematic failures by enforcing:
//! - No single point of failure (SPF)
//! - Required redundancy levels (DMR, TMR)
//! - Diversity between redundant channels
//! - Physical/logical separation
//!
//! # ISO 26262 Context
//!
//! Per ISO 26262-5:7.4.2 and 7.4.3, hardware design must address:
//! - Avoidance of systematic faults through design guidelines
//! - Detection of systematic faults through verification
//!
//! Higher ASIL levels require stricter design rules.

use serde::{Deserialize, Serialize};

// ============================================================================
// Design Rule Types
// ============================================================================

/// A design rule that must be satisfied for safety compliance
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum SafetyDesignRule {
    /// No single cell failure can violate the safety goal
    /// ISO 26262-5:7.4.2 - Evaluation of safety goal violations
    NoSinglePointOfFailure {
        /// Safety goal being protected
        goal: String,
        /// Coverage patterns (glob) for signals to check
        signal_patterns: Vec<String>,
    },

    /// Redundant implementation required (DMR/TMR)
    /// ISO 26262-5:7.4.4 - Fault tolerant mechanisms
    RedundancyRequired {
        /// Safety goal requiring redundancy
        goal: String,
        /// Minimum number of redundant copies
        min_copies: u32,
        /// Redundancy type
        redundancy_type: RedundancyType,
    },

    /// Diversity between redundant channels
    /// ISO 26262-9:7 - Analysis of dependent failures
    DiversityRequired {
        /// Channels that must be diverse
        channels: Vec<String>,
        /// Required diversity aspects
        aspects: Vec<DiversityAspect>,
    },

    /// Physical/logical separation between elements
    /// ISO 26262-5:7.4.4.6 - Independence between elements
    IndependenceRequired {
        /// Elements that must be independent
        elements: Vec<String>,
        /// Type of separation required
        separation: SeparationType,
    },

    /// Watchdog timer required for safety goal
    WatchdogRequired {
        /// Safety goal needing watchdog protection
        goal: String,
        /// Maximum timeout in milliseconds
        max_timeout_ms: u32,
    },

    /// Voter required for redundant channels
    VoterRequired {
        /// Safety goal needing voting
        goal: String,
        /// Minimum number of voters
        min_voters: u32,
        /// Voting algorithm type
        voting_type: VotingType,
    },

    /// Error detection mechanism required
    ErrorDetectionRequired {
        /// Safety goal needing error detection
        goal: String,
        /// Type of error detection
        detection_type: ErrorDetectionType,
        /// Target diagnostic coverage percentage
        target_dc: f64,
    },

    /// Custom rule with description
    Custom {
        /// Rule name
        name: String,
        /// Rule description
        description: String,
        /// ISO reference (e.g., "ISO 26262-5:7.4.2")
        iso_reference: Option<String>,
    },
}

/// Type of redundancy
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum RedundancyType {
    /// Dual Modular Redundancy (2 copies)
    Dmr,
    /// Triple Modular Redundancy (3 copies)
    Tmr,
    /// N-Modular Redundancy (N copies)
    Nmr(u32),
    /// Lockstep execution
    Lockstep,
}

/// Diversity aspects for dependent failure prevention
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum DiversityAspect {
    /// Different algorithms for same function
    DifferentAlgorithm,
    /// Same algorithm, different implementation
    DifferentImplementation,
    /// Different technology (ASIC vs FPGA)
    DifferentTechnology,
    /// Different design team
    DifferentDesignTeam,
    /// Different toolchain/compiler
    DifferentToolchain,
    /// Different timing (temporal diversity)
    TemporalDiversity {
        /// Minimum time offset in nanoseconds
        min_offset_ns: u64,
    },
}

/// Type of separation/independence
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum SeparationType {
    /// Logical separation (different modules)
    LogicalSeparation,
    /// Temporal separation (different time slots)
    TemporalSeparation {
        /// Minimum time separation in nanoseconds
        min_separation_ns: u64,
    },
    /// Physical separation (different die areas)
    PhysicalSeparation {
        /// Minimum physical distance in micrometers
        min_distance_um: Option<f64>,
    },
    /// Power domain separation
    PowerDomainSeparation,
    /// Clock domain separation
    ClockDomainSeparation,
}

/// Type of voting algorithm
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum VotingType {
    /// Majority voting (2-of-3, 3-of-5, etc.)
    Majority,
    /// All-agree voting (requires all inputs to match)
    Unanimous,
    /// Weighted voting
    Weighted,
    /// Best-of-N with error correction
    BestOfN,
}

/// Type of error detection mechanism
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ErrorDetectionType {
    /// Parity checking
    Parity,
    /// CRC checking
    Crc {
        /// CRC polynomial width
        width: u8,
    },
    /// ECC (Error Correcting Code)
    Ecc {
        /// SECDED, DECTED, etc.
        mode: String,
    },
    /// Lockstep comparison
    LockstepCompare,
    /// Range/plausibility check
    RangeCheck,
    /// Protocol check (sequence, timing)
    ProtocolCheck,
}

// ============================================================================
// Design Rule Checker
// ============================================================================

/// Design rule checker for safety verification
#[derive(Debug, Clone)]
pub struct DesignRuleChecker {
    /// Rules to check
    pub rules: Vec<SafetyDesignRule>,
    /// Target ASIL level (determines strictness)
    pub target_asil: String,
}

impl DesignRuleChecker {
    /// Create a new design rule checker
    pub fn new(target_asil: &str) -> Self {
        Self {
            rules: Vec::new(),
            target_asil: target_asil.to_string(),
        }
    }

    /// Add a design rule
    pub fn add_rule(&mut self, rule: SafetyDesignRule) {
        self.rules.push(rule);
    }

    /// Get default rules for an ASIL level
    pub fn with_default_rules_for_asil(mut self, asil: &str) -> Self {
        self.rules.extend(default_rules_for_asil(asil));
        self
    }
}

/// Rule check result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum RuleCheckStatus {
    /// All rules passed
    Passed,
    /// Some rules failed
    Failed,
    /// Check could not be completed (insufficient information)
    Incomplete,
}

/// Result of rule checking
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DesignRuleReport {
    /// Rules that passed
    pub passed: Vec<RuleResult>,
    /// Rules that failed (violations)
    pub failed: Vec<RuleViolation>,
    /// Warnings (not violations but noteworthy)
    pub warnings: Vec<RuleWarning>,
    /// Overall check status
    pub overall_status: RuleCheckStatus,
}

impl DesignRuleReport {
    /// Create an empty report
    pub fn new() -> Self {
        Self {
            passed: Vec::new(),
            failed: Vec::new(),
            warnings: Vec::new(),
            overall_status: RuleCheckStatus::Passed,
        }
    }

    /// Add a passed result
    pub fn add_passed(&mut self, result: RuleResult) {
        self.passed.push(result);
    }

    /// Add a violation
    pub fn add_violation(&mut self, violation: RuleViolation) {
        self.failed.push(violation);
        self.overall_status = RuleCheckStatus::Failed;
    }

    /// Add a warning
    pub fn add_warning(&mut self, warning: RuleWarning) {
        self.warnings.push(warning);
    }

    /// Check if there are any violations
    pub fn has_violations(&self) -> bool {
        !self.failed.is_empty()
    }
}

impl Default for DesignRuleReport {
    fn default() -> Self {
        Self::new()
    }
}

/// Result of a successful rule check
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RuleResult {
    /// Rule that was checked
    pub rule_name: String,
    /// Elements that satisfied the rule
    pub satisfied_by: Vec<String>,
    /// Coverage achieved (if applicable)
    pub coverage: Option<f64>,
}

/// A rule violation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RuleViolation {
    /// The rule that was violated
    pub rule: String,
    /// Elements that violate the rule
    pub affected_elements: Vec<String>,
    /// Detailed description of the violation
    pub description: String,
    /// ISO 26262 reference for this rule
    pub iso_reference: String,
    /// Suggested remediation
    pub remediation: String,
    /// Severity (1-5, where 5 is most severe)
    pub severity: u8,
}

/// A rule warning (not a violation)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RuleWarning {
    /// Warning category
    pub category: String,
    /// Affected elements
    pub elements: Vec<String>,
    /// Warning message
    pub message: String,
    /// Recommendation
    pub recommendation: Option<String>,
}

// ============================================================================
// Default Rules
// ============================================================================

/// Get default design rules for an ASIL level
pub fn default_rules_for_asil(asil: &str) -> Vec<SafetyDesignRule> {
    match asil.to_uppercase().as_str() {
        "D" => default_asil_d_rules(),
        "C" => default_asil_c_rules(),
        "B" => default_asil_b_rules(),
        "A" => default_asil_a_rules(),
        _ => Vec::new(), // QM has no required rules
    }
}

/// Default rules for ASIL D (most stringent)
fn default_asil_d_rules() -> Vec<SafetyDesignRule> {
    let mut rules = default_asil_c_rules();

    // Additional ASIL D rules
    rules.push(SafetyDesignRule::RedundancyRequired {
        goal: "*".to_string(),
        min_copies: 3,
        redundancy_type: RedundancyType::Tmr,
    });

    rules.push(SafetyDesignRule::DiversityRequired {
        channels: vec!["*".to_string()],
        aspects: vec![
            DiversityAspect::DifferentImplementation,
            DiversityAspect::TemporalDiversity { min_offset_ns: 100 },
        ],
    });

    rules.push(SafetyDesignRule::ErrorDetectionRequired {
        goal: "*".to_string(),
        detection_type: ErrorDetectionType::Ecc {
            mode: "SECDED".to_string(),
        },
        target_dc: 99.0,
    });

    rules
}

/// Default rules for ASIL C
fn default_asil_c_rules() -> Vec<SafetyDesignRule> {
    let mut rules = default_asil_b_rules();

    // Additional ASIL C rules
    rules.push(SafetyDesignRule::RedundancyRequired {
        goal: "*".to_string(),
        min_copies: 2,
        redundancy_type: RedundancyType::Dmr,
    });

    rules.push(SafetyDesignRule::VoterRequired {
        goal: "*".to_string(),
        min_voters: 1,
        voting_type: VotingType::Majority,
    });

    rules.push(SafetyDesignRule::ErrorDetectionRequired {
        goal: "*".to_string(),
        detection_type: ErrorDetectionType::Crc { width: 16 },
        target_dc: 90.0,
    });

    rules
}

/// Default rules for ASIL B
fn default_asil_b_rules() -> Vec<SafetyDesignRule> {
    let mut rules = default_asil_a_rules();

    // Additional ASIL B rules
    rules.push(SafetyDesignRule::WatchdogRequired {
        goal: "*".to_string(),
        max_timeout_ms: 100,
    });

    rules.push(SafetyDesignRule::IndependenceRequired {
        elements: vec!["functional".to_string(), "safety_mechanism".to_string()],
        separation: SeparationType::LogicalSeparation,
    });

    rules
}

/// Default rules for ASIL A
fn default_asil_a_rules() -> Vec<SafetyDesignRule> {
    vec![
        SafetyDesignRule::NoSinglePointOfFailure {
            goal: "*".to_string(),
            signal_patterns: vec!["*".to_string()],
        },
        SafetyDesignRule::ErrorDetectionRequired {
            goal: "*".to_string(),
            detection_type: ErrorDetectionType::Parity,
            target_dc: 60.0,
        },
    ]
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_design_rule_checker_creation() {
        let checker = DesignRuleChecker::new("D").with_default_rules_for_asil("D");
        assert!(!checker.rules.is_empty());
    }

    #[test]
    fn test_asil_d_rules_are_strictest() {
        let asil_d = default_rules_for_asil("D");
        let asil_a = default_rules_for_asil("A");

        assert!(asil_d.len() > asil_a.len());
    }

    #[test]
    fn test_report_creation() {
        let mut report = DesignRuleReport::new();
        assert!(matches!(report.overall_status, RuleCheckStatus::Passed));

        report.add_violation(RuleViolation {
            rule: "TestRule".to_string(),
            affected_elements: vec!["elem1".to_string()],
            description: "Test violation".to_string(),
            iso_reference: "ISO 26262-5:7.4.2".to_string(),
            remediation: "Add redundancy".to_string(),
            severity: 3,
        });

        assert!(report.has_violations());
        assert!(matches!(report.overall_status, RuleCheckStatus::Failed));
    }

    #[test]
    fn test_redundancy_types() {
        let dmr = RedundancyType::Dmr;
        let tmr = RedundancyType::Tmr;
        let nmr = RedundancyType::Nmr(5);

        assert_ne!(dmr, tmr);
        assert!(matches!(nmr, RedundancyType::Nmr(5)));
    }
}
