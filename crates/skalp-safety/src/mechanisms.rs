//! Safety mechanisms implementation for ISO 26262 compliance
//!
//! Provides Primary Safety Mechanisms (PSM) and Latent Safety Mechanisms (LSM)
//! for achieving required safety integrity levels.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use chrono::{DateTime, Utc};
use crate::asil::AsilLevel;
use skalp_mir::mir::Expression;

/// Safety mechanism types according to ISO 26262
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum MechanismType {
    /// Primary Safety Mechanism - prevents single point failures
    Primary,
    /// Latent Safety Mechanism - detects latent faults
    Latent,
    /// Dual safety mechanism (both primary and latent)
    Dual,
}

/// Categories of safety mechanisms
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum MechanismCategory {
    /// Error Detection and Correction (ECC)
    ErrorDetection,
    /// Redundancy mechanisms
    Redundancy,
    /// Diversity mechanisms
    Diversity,
    /// Monitoring mechanisms
    Monitoring,
    /// Isolation mechanisms
    Isolation,
    /// Graceful degradation
    GracefulDegradation,
    /// Fail-safe mechanisms
    FailSafe,
    /// Built-in self-test
    BuiltInSelfTest,
}

/// Implementation approach for safety mechanisms
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ImplementationApproach {
    /// Hardware-based implementation
    Hardware,
    /// Software-based implementation
    Software,
    /// Mixed hardware/software implementation
    Mixed,
    /// External component
    External,
}

/// Safety mechanism definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SafetyMechanism {
    /// Unique identifier
    pub id: String,
    /// Human-readable name
    pub name: String,
    /// Description of the mechanism
    pub description: String,
    /// Type of mechanism (PSM/LSM)
    pub mechanism_type: MechanismType,
    /// Category of mechanism
    pub category: MechanismCategory,
    /// Implementation approach
    pub implementation: ImplementationApproach,
    /// Target ASIL level
    pub target_asil: AsilLevel,
    /// Fault coverage percentage
    pub fault_coverage: f64,
    /// Diagnostic coverage percentage
    pub diagnostic_coverage: f64,
    /// Failure mode addressed
    pub failure_modes: Vec<String>,
    /// Associated safety requirements
    pub safety_requirements: Vec<String>,
    /// Implementation status
    pub status: MechanismStatus,
    /// Verification methods
    pub verification_methods: Vec<String>,
    /// Hardware implementation details
    pub hardware_implementation: Option<HardwareImplementation>,
    /// Creation timestamp
    pub created_at: DateTime<Utc>,
    /// Last modified timestamp
    pub modified_at: DateTime<Utc>,
}

/// Status of safety mechanism implementation
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum MechanismStatus {
    /// Mechanism specified but not implemented
    Specified,
    /// Implementation in progress
    InProgress,
    /// Implementation completed
    Implemented,
    /// Verification in progress
    Verifying,
    /// Mechanism verified
    Verified,
    /// Mechanism validated
    Validated,
    /// Mechanism disabled/inactive
    Disabled,
}

/// Hardware implementation details for safety mechanisms
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HardwareImplementation {
    /// Logic equations for the mechanism
    pub logic_equations: Vec<LogicEquation>,
    /// Resource utilization
    pub resource_utilization: ResourceUtilization,
    /// Timing constraints
    pub timing_constraints: TimingConstraints,
    /// Power consumption
    pub power_consumption: PowerConsumption,
}

/// Logic equation for hardware implementation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LogicEquation {
    /// Output signal name
    pub output: String,
    /// Logic expression
    pub expression: Expression,
    /// Description of the equation
    pub description: String,
}

/// Resource utilization metrics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResourceUtilization {
    /// Logic gates used
    pub logic_gates: u32,
    /// Flip-flops used
    pub flip_flops: u32,
    /// Memory bits used
    pub memory_bits: u32,
    /// Area overhead percentage
    pub area_overhead: f64,
}

/// Timing constraints for safety mechanisms
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TimingConstraints {
    /// Maximum detection time (nanoseconds)
    pub max_detection_time: u64,
    /// Maximum response time (nanoseconds)
    pub max_response_time: u64,
    /// Clock domain requirements
    pub clock_domains: Vec<String>,
}

/// Power consumption metrics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PowerConsumption {
    /// Static power (mW)
    pub static_power: f64,
    /// Dynamic power (mW)
    pub dynamic_power: f64,
    /// Power overhead percentage
    pub power_overhead: f64,
}

/// Safety mechanism manager
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SafetyMechanismManager {
    /// All safety mechanisms
    mechanisms: HashMap<String, SafetyMechanism>,
    /// Mechanism assignments to design elements
    assignments: HashMap<String, Vec<String>>, // design_element -> mechanisms
    /// Fault coverage matrix
    fault_coverage: FaultCoverageMatrix,
}

/// Fault coverage analysis matrix
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FaultCoverageMatrix {
    /// Fault modes covered by mechanisms
    fault_to_mechanisms: HashMap<String, Vec<String>>,
    /// Mechanisms covering each fault
    mechanism_to_faults: HashMap<String, Vec<String>>,
    /// Coverage percentages
    coverage_percentages: HashMap<String, f64>,
}

impl SafetyMechanism {
    /// Create a new safety mechanism
    pub fn new(
        id: String,
        name: String,
        description: String,
        mechanism_type: MechanismType,
        category: MechanismCategory,
    ) -> Self {
        let now = Utc::now();
        Self {
            id,
            name,
            description,
            mechanism_type,
            category,
            implementation: ImplementationApproach::Hardware,
            target_asil: AsilLevel::A,
            fault_coverage: 0.0,
            diagnostic_coverage: 0.0,
            failure_modes: vec![],
            safety_requirements: vec![],
            status: MechanismStatus::Specified,
            verification_methods: vec![],
            hardware_implementation: None,
            created_at: now,
            modified_at: now,
        }
    }

    /// Set fault coverage percentage
    pub fn set_fault_coverage(&mut self, coverage: f64) {
        self.fault_coverage = coverage.clamp(0.0, 100.0);
        self.modified_at = Utc::now();
    }

    /// Set diagnostic coverage percentage
    pub fn set_diagnostic_coverage(&mut self, coverage: f64) {
        self.diagnostic_coverage = coverage.clamp(0.0, 100.0);
        self.modified_at = Utc::now();
    }

    /// Add a failure mode
    pub fn add_failure_mode(&mut self, failure_mode: String) {
        if !self.failure_modes.contains(&failure_mode) {
            self.failure_modes.push(failure_mode);
            self.modified_at = Utc::now();
        }
    }

    /// Add safety requirement
    pub fn add_safety_requirement(&mut self, requirement_id: String) {
        if !self.safety_requirements.contains(&requirement_id) {
            self.safety_requirements.push(requirement_id);
            self.modified_at = Utc::now();
        }
    }

    /// Update mechanism status
    pub fn update_status(&mut self, status: MechanismStatus) {
        self.status = status;
        self.modified_at = Utc::now();
    }

    /// Set hardware implementation
    pub fn set_hardware_implementation(&mut self, implementation: HardwareImplementation) {
        self.hardware_implementation = Some(implementation);
        self.modified_at = Utc::now();
    }

    /// Check if mechanism is active
    pub fn is_active(&self) -> bool {
        !matches!(self.status, MechanismStatus::Disabled)
    }

    /// Check if mechanism is verified
    pub fn is_verified(&self) -> bool {
        matches!(self.status, MechanismStatus::Verified | MechanismStatus::Validated)
    }

    /// Calculate effectiveness based on coverage
    pub fn calculate_effectiveness(&self) -> f64 {
        match self.mechanism_type {
            MechanismType::Primary => self.fault_coverage,
            MechanismType::Latent => self.diagnostic_coverage,
            MechanismType::Dual => {
                // Combined effectiveness for dual mechanisms
                1.0 - (1.0 - self.fault_coverage / 100.0) * (1.0 - self.diagnostic_coverage / 100.0)
            }
        }
    }
}

impl SafetyMechanismManager {
    /// Create a new mechanism manager
    pub fn new() -> Self {
        Self {
            mechanisms: HashMap::new(),
            assignments: HashMap::new(),
            fault_coverage: FaultCoverageMatrix {
                fault_to_mechanisms: HashMap::new(),
                mechanism_to_faults: HashMap::new(),
                coverage_percentages: HashMap::new(),
            },
        }
    }

    /// Add a safety mechanism
    pub fn add_mechanism(&mut self, mechanism: SafetyMechanism) {
        let id = mechanism.id.clone();

        // Update fault coverage matrix
        for fault_mode in &mechanism.failure_modes {
            self.fault_coverage
                .fault_to_mechanisms
                .entry(fault_mode.clone())
                .or_insert_with(Vec::new)
                .push(id.clone());

            self.fault_coverage
                .mechanism_to_faults
                .entry(id.clone())
                .or_insert_with(Vec::new)
                .push(fault_mode.clone());
        }

        self.mechanisms.insert(id, mechanism);
    }

    /// Get a mechanism by ID
    pub fn get_mechanism(&self, id: &str) -> Option<&SafetyMechanism> {
        self.mechanisms.get(id)
    }

    /// Get mutable reference to a mechanism
    pub fn get_mechanism_mut(&mut self, id: &str) -> Option<&mut SafetyMechanism> {
        self.mechanisms.get_mut(id)
    }

    /// Get mechanisms by type
    pub fn get_mechanisms_by_type(&self, mechanism_type: MechanismType) -> Vec<&SafetyMechanism> {
        self.mechanisms
            .values()
            .filter(|m| m.mechanism_type == mechanism_type)
            .collect()
    }

    /// Get mechanisms by category
    pub fn get_mechanisms_by_category(&self, category: MechanismCategory) -> Vec<&SafetyMechanism> {
        self.mechanisms
            .values()
            .filter(|m| m.category == category)
            .collect()
    }

    /// Assign mechanism to design element
    pub fn assign_mechanism(&mut self, design_element: String, mechanism_id: String) {
        self.assignments
            .entry(design_element)
            .or_insert_with(Vec::new)
            .push(mechanism_id);
    }

    /// Get mechanisms assigned to design element
    pub fn get_assigned_mechanisms(&self, design_element: &str) -> Vec<&SafetyMechanism> {
        self.assignments
            .get(design_element)
            .map(|mechanism_ids| {
                mechanism_ids
                    .iter()
                    .filter_map(|id| self.mechanisms.get(id))
                    .collect()
            })
            .unwrap_or_default()
    }

    /// Calculate overall fault coverage for design element
    pub fn calculate_fault_coverage(&self, design_element: &str) -> f64 {
        let mechanisms = self.get_assigned_mechanisms(design_element);
        if mechanisms.is_empty() {
            return 0.0;
        }

        // Calculate combined coverage using probabilistic model
        let mut combined_coverage = 0.0;
        for mechanism in mechanisms {
            let individual_coverage = mechanism.fault_coverage / 100.0;
            combined_coverage = combined_coverage + individual_coverage - (combined_coverage * individual_coverage);
        }

        combined_coverage * 100.0
    }

    /// Calculate diagnostic coverage for design element
    pub fn calculate_diagnostic_coverage(&self, design_element: &str) -> f64 {
        let mechanisms = self.get_assigned_mechanisms(design_element);
        if mechanisms.is_empty() {
            return 0.0;
        }

        // Calculate combined diagnostic coverage
        let mut combined_coverage = 0.0;
        for mechanism in mechanisms {
            let individual_coverage = mechanism.diagnostic_coverage / 100.0;
            combined_coverage = combined_coverage + individual_coverage - (combined_coverage * individual_coverage);
        }

        combined_coverage * 100.0
    }

    /// Generate safety mechanism report
    pub fn generate_report(&self) -> MechanismReport {
        let total_mechanisms = self.mechanisms.len();
        let verified_mechanisms = self.mechanisms
            .values()
            .filter(|m| m.is_verified())
            .count();

        let mut type_breakdown = HashMap::new();
        let mut category_breakdown = HashMap::new();

        for mechanism in self.mechanisms.values() {
            *type_breakdown.entry(mechanism.mechanism_type.clone()).or_insert(0) += 1;
            *category_breakdown.entry(mechanism.category.clone()).or_insert(0) += 1;
        }

        let avg_fault_coverage = if total_mechanisms > 0 {
            self.mechanisms
                .values()
                .map(|m| m.fault_coverage)
                .sum::<f64>() / total_mechanisms as f64
        } else {
            0.0
        };

        let avg_diagnostic_coverage = if total_mechanisms > 0 {
            self.mechanisms
                .values()
                .map(|m| m.diagnostic_coverage)
                .sum::<f64>() / total_mechanisms as f64
        } else {
            0.0
        };

        MechanismReport {
            total_mechanisms,
            verified_mechanisms,
            verification_percentage: if total_mechanisms > 0 {
                (verified_mechanisms as f64 / total_mechanisms as f64) * 100.0
            } else {
                0.0
            },
            type_breakdown,
            category_breakdown,
            avg_fault_coverage,
            avg_diagnostic_coverage,
        }
    }

    /// Get all mechanisms (for metrics calculation)
    pub fn get_all_mechanisms(&self) -> Vec<SafetyMechanism> {
        self.mechanisms.values().cloned().collect()
    }

    /// Validate mechanism assignments
    pub fn validate_assignments(&self) -> Vec<ValidationError> {
        let mut errors = vec![];

        for (design_element, mechanism_ids) in &self.assignments {
            for mechanism_id in mechanism_ids {
                if !self.mechanisms.contains_key(mechanism_id) {
                    errors.push(ValidationError::MissingMechanism {
                        design_element: design_element.clone(),
                        mechanism_id: mechanism_id.clone(),
                    });
                }
            }
        }

        errors
    }
}

/// Safety mechanism report
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MechanismReport {
    /// Total number of mechanisms
    pub total_mechanisms: usize,
    /// Number of verified mechanisms
    pub verified_mechanisms: usize,
    /// Verification percentage
    pub verification_percentage: f64,
    /// Breakdown by mechanism type
    pub type_breakdown: HashMap<MechanismType, usize>,
    /// Breakdown by category
    pub category_breakdown: HashMap<MechanismCategory, usize>,
    /// Average fault coverage
    pub avg_fault_coverage: f64,
    /// Average diagnostic coverage
    pub avg_diagnostic_coverage: f64,
}

/// Validation errors for mechanism assignments
#[derive(Debug, Clone, PartialEq)]
pub enum ValidationError {
    /// Assigned mechanism doesn't exist
    MissingMechanism {
        design_element: String,
        mechanism_id: String,
    },
}

impl Default for SafetyMechanismManager {
    fn default() -> Self {
        Self::new()
    }
}

/// Predefined safety mechanisms commonly used in automotive applications
pub mod predefined {
    use super::*;

    /// Create ECC (Error Correcting Code) mechanism
    pub fn create_ecc_mechanism() -> SafetyMechanism {
        let mut mechanism = SafetyMechanism::new(
            "ECC-001".to_string(),
            "Error Correcting Code".to_string(),
            "Single-error correction and double-error detection".to_string(),
            MechanismType::Primary,
            MechanismCategory::ErrorDetection,
        );

        mechanism.set_fault_coverage(99.9);
        mechanism.set_diagnostic_coverage(99.0);
        mechanism.add_failure_mode("Single-bit memory error".to_string());
        mechanism.add_failure_mode("Double-bit memory error".to_string());

        mechanism
    }

    /// Create dual-core lockstep mechanism
    pub fn create_lockstep_mechanism() -> SafetyMechanism {
        let mut mechanism = SafetyMechanism::new(
            "LOCKSTEP-001".to_string(),
            "Dual-Core Lockstep".to_string(),
            "Two cores executing same instructions with comparison".to_string(),
            MechanismType::Dual,
            MechanismCategory::Redundancy,
        );

        mechanism.set_fault_coverage(95.0);
        mechanism.set_diagnostic_coverage(98.0);
        mechanism.add_failure_mode("Processor core failure".to_string());
        mechanism.add_failure_mode("Instruction execution error".to_string());

        mechanism
    }

    /// Create watchdog timer mechanism
    pub fn create_watchdog_mechanism() -> SafetyMechanism {
        let mut mechanism = SafetyMechanism::new(
            "WATCHDOG-001".to_string(),
            "Watchdog Timer".to_string(),
            "Monitors system liveliness and triggers reset on timeout".to_string(),
            MechanismType::Latent,
            MechanismCategory::Monitoring,
        );

        mechanism.set_fault_coverage(0.0); // Doesn't prevent faults
        mechanism.set_diagnostic_coverage(90.0);
        mechanism.add_failure_mode("Software hang".to_string());
        mechanism.add_failure_mode("Infinite loop".to_string());

        mechanism
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use super::predefined::*;

    #[test]
    fn test_safety_mechanism_creation() {
        let mechanism = SafetyMechanism::new(
            "TEST-001".to_string(),
            "Test Mechanism".to_string(),
            "Test description".to_string(),
            MechanismType::Primary,
            MechanismCategory::ErrorDetection,
        );

        assert_eq!(mechanism.id, "TEST-001");
        assert_eq!(mechanism.mechanism_type, MechanismType::Primary);
        assert_eq!(mechanism.status, MechanismStatus::Specified);
    }

    #[test]
    fn test_mechanism_effectiveness() {
        let mut mechanism = SafetyMechanism::new(
            "TEST-002".to_string(),
            "Test Dual Mechanism".to_string(),
            "Test description".to_string(),
            MechanismType::Dual,
            MechanismCategory::Redundancy,
        );

        mechanism.set_fault_coverage(90.0);
        mechanism.set_diagnostic_coverage(95.0);

        let effectiveness = mechanism.calculate_effectiveness();
        assert!(effectiveness > 0.99); // Combined effectiveness should be high
    }

    #[test]
    fn test_mechanism_manager() {
        let mut manager = SafetyMechanismManager::new();
        let mechanism = create_ecc_mechanism();

        manager.add_mechanism(mechanism);

        let retrieved = manager.get_mechanism("ECC-001").unwrap();
        assert_eq!(retrieved.name, "Error Correcting Code");
    }

    #[test]
    fn test_fault_coverage_calculation() {
        let mut manager = SafetyMechanismManager::new();

        let mut mech1 = create_ecc_mechanism();
        mech1.set_fault_coverage(90.0);

        let mut mech2 = create_watchdog_mechanism();
        mech2.set_fault_coverage(80.0);

        manager.add_mechanism(mech1);
        manager.add_mechanism(mech2);

        manager.assign_mechanism("memory_controller".to_string(), "ECC-001".to_string());
        manager.assign_mechanism("memory_controller".to_string(), "WATCHDOG-001".to_string());

        let coverage = manager.calculate_fault_coverage("memory_controller");
        assert!(coverage > 90.0); // Combined coverage should be higher
    }

    #[test]
    fn test_predefined_mechanisms() {
        let ecc = create_ecc_mechanism();
        assert_eq!(ecc.category, MechanismCategory::ErrorDetection);
        assert!(ecc.fault_coverage > 99.0);

        let lockstep = create_lockstep_mechanism();
        assert_eq!(lockstep.mechanism_type, MechanismType::Dual);
        assert_eq!(lockstep.category, MechanismCategory::Redundancy);

        let watchdog = create_watchdog_mechanism();
        assert_eq!(watchdog.mechanism_type, MechanismType::Latent);
        assert_eq!(watchdog.category, MechanismCategory::Monitoring);
    }

    #[test]
    fn test_mechanism_report() {
        let mut manager = SafetyMechanismManager::new();

        let mut mech1 = create_ecc_mechanism();
        mech1.update_status(MechanismStatus::Verified);

        let mech2 = create_watchdog_mechanism();

        manager.add_mechanism(mech1);
        manager.add_mechanism(mech2);

        let report = manager.generate_report();
        assert_eq!(report.total_mechanisms, 2);
        assert_eq!(report.verified_mechanisms, 1);
        assert_eq!(report.verification_percentage, 50.0);
    }
}