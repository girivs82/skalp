//! Safety Hierarchy System for ISO 26262 Functional Safety
//!
//! Implements a three-layer architecture for functional safety:
//! 1. Safety Goal (Layer 1) - Abstract requirements from hazard analysis
//! 2. Design (Layer 2) - RTL with `#[implements(...)]` annotations
//! 3. Safety Entity (Layer 3) - Binding layer with HSI, FMEA, overrides
//!
//! Key insight: **Hierarchical nesting IS traceability** - no explicit links needed.
//! The parent-child relationship automatically provides full traceability.

use crate::asil::AsilLevel;
use indexmap::IndexMap;
use petgraph::graph::NodeIndex;
use petgraph::Graph;
use serde::{Deserialize, Serialize};
use std::fmt;
use std::time::Duration;

// ============================================================================
// Instance Path and Design Reference Types
// ============================================================================

/// Instance path: "top.subsys.instance"
/// Represents hierarchical path through design instances
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct InstancePath {
    /// Path segments: ["top", "subsys", "instance"]
    pub segments: Vec<String>,
}

impl InstancePath {
    /// Create a new instance path from segments
    pub fn new(segments: Vec<String>) -> Self {
        Self { segments }
    }

    /// Parse from dot-notation string: "top.subsys.instance"
    pub fn parse(path: &str) -> Self {
        Self {
            segments: path.split('.').map(|s| s.to_string()).collect(),
        }
    }
}

impl fmt::Display for InstancePath {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.segments.join("."))
    }
}

impl InstancePath {
    /// Check if this path matches a glob pattern (e.g., "top.sram_*")
    pub fn matches_pattern(&self, pattern: &str) -> bool {
        let pattern_segments: Vec<&str> = pattern.split('.').collect();

        if pattern_segments.len() != self.segments.len() {
            return false;
        }

        for (seg, pat) in self.segments.iter().zip(pattern_segments.iter()) {
            if *pat == "*" {
                continue;
            }
            if pat.contains('*') {
                // Simple glob matching (e.g., "sram_*")
                let prefix = pat.trim_end_matches('*');
                if !seg.starts_with(prefix) {
                    return false;
                }
            } else if seg != *pat {
                return false;
            }
        }
        true
    }

    /// Get parent path (all segments except last)
    pub fn parent(&self) -> Option<Self> {
        if self.segments.len() > 1 {
            Some(Self {
                segments: self.segments[..self.segments.len() - 1].to_vec(),
            })
        } else {
            None
        }
    }

    /// Get leaf name (last segment)
    pub fn leaf(&self) -> Option<&str> {
        self.segments.last().map(|s| s.as_str())
    }
}

/// Design reference: instance path + optional signal
/// Examples:
/// - `top.brake_main` - instance only
/// - `top.brake_main::pressure_a` - instance + signal
/// - `top.brake_main::state[3:0]` - instance + signal + bit slice
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct DesignRef {
    /// Instance path (dot notation)
    pub instance: InstancePath,
    /// Optional signal within instance (after ::)
    pub signal: Option<String>,
    /// Optional bit range slice
    pub bit_range: Option<(u32, u32)>,
}

impl DesignRef {
    /// Create a reference to an instance only
    pub fn instance(path: InstancePath) -> Self {
        Self {
            instance: path,
            signal: None,
            bit_range: None,
        }
    }

    /// Create a reference to a signal within an instance
    pub fn signal(path: InstancePath, signal: String) -> Self {
        Self {
            instance: path,
            signal: Some(signal),
            bit_range: None,
        }
    }

    /// Create a reference with bit slice
    pub fn signal_slice(path: InstancePath, signal: String, high: u32, low: u32) -> Self {
        Self {
            instance: path,
            signal: Some(signal),
            bit_range: Some((high, low)),
        }
    }

    /// Parse from string: "top.brake_main::pressure_a[11:0]"
    pub fn parse(s: &str) -> Self {
        // Split on :: for instance::signal
        let parts: Vec<&str> = s.split("::").collect();
        let instance = InstancePath::parse(parts[0]);

        if parts.len() == 1 {
            return Self::instance(instance);
        }

        let signal_part = parts[1];

        // Check for bit slice [high:low]
        if let Some(bracket_pos) = signal_part.find('[') {
            let signal = signal_part[..bracket_pos].to_string();
            let range_str = &signal_part[bracket_pos + 1..signal_part.len() - 1];
            let range_parts: Vec<&str> = range_str.split(':').collect();
            if range_parts.len() == 2 {
                let high: u32 = range_parts[0].parse().unwrap_or(0);
                let low: u32 = range_parts[1].parse().unwrap_or(0);
                return Self::signal_slice(instance, signal, high, low);
            }
        }

        Self::signal(instance, signal_part.to_string())
    }
}

impl fmt::Display for DesignRef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.instance)?;
        if let Some(ref sig) = self.signal {
            write!(f, "::{}", sig)?;
            if let Some((high, low)) = self.bit_range {
                write!(f, "[{}:{}]", high, low)?;
            }
        }
        Ok(())
    }
}

/// Pattern for matching design references (supports wildcards)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DesignPattern {
    /// Instance pattern (may contain wildcards)
    pub instance_pattern: String,
    /// Signal pattern (may contain wildcards)
    pub signal_pattern: Option<String>,
}

impl DesignPattern {
    /// Create a pattern that matches instances
    pub fn instances(pattern: &str) -> Self {
        Self {
            instance_pattern: pattern.to_string(),
            signal_pattern: None,
        }
    }

    /// Create a pattern that matches signals
    pub fn signals(instance_pattern: &str, signal_pattern: &str) -> Self {
        Self {
            instance_pattern: instance_pattern.to_string(),
            signal_pattern: Some(signal_pattern.to_string()),
        }
    }

    /// Check if a design reference matches this pattern
    pub fn matches(&self, design_ref: &DesignRef) -> bool {
        if !design_ref.instance.matches_pattern(&self.instance_pattern) {
            return false;
        }

        match (&self.signal_pattern, &design_ref.signal) {
            (Some(pat), Some(sig)) => {
                if pat == "*" {
                    true
                } else if pat.contains('*') {
                    let prefix = pat.trim_end_matches('*');
                    sig.starts_with(prefix)
                } else {
                    pat == sig
                }
            }
            (Some(_), None) => false, // Pattern expects signal but ref has none
            (None, _) => true,        // Pattern doesn't care about signal
        }
    }
}

// ============================================================================
// Safety Goal (Layer 1) - ISO 26262 Part 3
// ============================================================================

/// Unique identifier for a safety goal
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct SafetyGoalId(pub u32);

/// Safety Goal - top of hierarchy (ISO 26262 Part 3)
/// Defines WHAT is required without referencing design
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SafetyGoal {
    /// Internal identifier
    pub id: SafetyGoalId,
    /// External identifier (e.g., "SG-001")
    pub external_id: String,
    /// Goal name (e.g., "BrakingSafety")
    pub name: String,
    /// Description of the safety goal
    pub description: String,
    /// Required ASIL level
    pub asil: AsilLevel,
    /// Fault Tolerant Time Interval (FTTI)
    pub ftti: Option<Duration>,
    /// External traceability links (only for external systems like DOORS)
    pub traces_to: Vec<String>,

    /// Target SPFM (Single Point Fault Metric)
    pub target_spfm: Option<f64>,
    /// Target LFM (Latent Fault Metric)
    pub target_lfm: Option<f64>,
    /// Target PMHF (Probabilistic Metric for Hardware Failures) in FIT
    pub target_pmhf: Option<f64>,

    /// Hardware Safety Requirements - children (traceability = nesting)
    pub hsrs: Vec<HardwareSafetyRequirement>,

    /// Goal-level Latent Safety Mechanisms (not tied to specific HSR)
    pub lsms: Vec<LatentSafetyMechanism>,

    /// Trait usages for composition
    pub trait_usages: Vec<TraitUsage>,
}

impl SafetyGoal {
    /// Create a new safety goal
    pub fn new(
        id: SafetyGoalId,
        external_id: String,
        name: String,
        description: String,
        asil: AsilLevel,
    ) -> Self {
        Self {
            id,
            external_id,
            name,
            description,
            asil,
            ftti: None,
            traces_to: Vec::new(),
            target_spfm: None,
            target_lfm: None,
            target_pmhf: None,
            hsrs: Vec::new(),
            lsms: Vec::new(),
            trait_usages: Vec::new(),
        }
    }

    /// Set target metrics based on ASIL level
    pub fn with_default_targets(mut self) -> Self {
        let requirements = self.asil.requirements();
        self.target_spfm = requirements.spfm_target;
        self.target_lfm = requirements.lf_target;
        // PMHF targets from ISO 26262
        self.target_pmhf = Some(match self.asil {
            AsilLevel::QM => f64::INFINITY,
            AsilLevel::A => 1000.0,
            AsilLevel::B => 100.0,
            AsilLevel::C => 100.0,
            AsilLevel::D => 10.0,
        });
        self
    }
}

// ============================================================================
// Hardware Safety Requirement (HSR) - Contains PSM
// ============================================================================

/// Hardware Safety Requirement
/// Contains its PSM as a child - nesting = traceability
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HardwareSafetyRequirement {
    /// Identifier (e.g., "HSR_001")
    pub id: String,
    /// Requirement text
    pub requirement: String,
    /// Verification methods
    pub verification: Vec<VerificationMethod>,

    /// Child PSM - no "satisfied_by" needed, it's a child
    pub psm: Option<PrimarySafetyMechanism>,
}

impl HardwareSafetyRequirement {
    /// Create a new HSR
    pub fn new(id: String, requirement: String) -> Self {
        Self {
            id,
            requirement,
            verification: Vec::new(),
            psm: None,
        }
    }

    /// Add a verification method
    pub fn with_verification(mut self, method: VerificationMethod) -> Self {
        self.verification.push(method);
        self
    }

    /// Set the PSM for this HSR
    pub fn with_psm(mut self, psm: PrimarySafetyMechanism) -> Self {
        self.psm = Some(psm);
        self
    }
}

/// Verification methods for safety requirements
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum VerificationMethod {
    /// Formal proof
    FormalProve,
    /// Model checking
    ModelCheck,
    /// Fault injection testing
    FaultInjection,
    /// Simulation with coverage
    SimulationCoverage { target_percentage: u32 },
    /// Hardware-in-the-loop testing
    HilTest,
    /// Code review
    Review,
    /// Static analysis
    StaticAnalysis,
    /// Test case
    TestCase { test_id: String },
    /// Custom verification
    Custom { name: String, description: String },
}

// ============================================================================
// Primary Safety Mechanism (PSM) - Contains DHSR
// ============================================================================

/// Primary Safety Mechanism
/// Contains its DHSR as a child - nesting = traceability
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PrimarySafetyMechanism {
    /// Mechanism name (e.g., "SensorVoting")
    pub name: String,
    /// Target diagnostic coverage (percentage)
    pub dc_target: f64,
    /// Mechanism type for categorization
    pub mechanism_type: MechanismType,

    /// Child DHSR - no "diagnoses" field needed, it's a child
    pub dhsr: Option<DiagnosticHardwareSafetyRequirement>,

    /// Design bindings (auto-discovered from #[implements(...)])
    pub implementations: Vec<DesignRef>,

    /// FMEA failure modes detected by this PSM (by nesting in FMEA)
    pub failure_modes: Vec<FailureModeBinding>,
}

impl PrimarySafetyMechanism {
    /// Create a new PSM
    pub fn new(name: String, dc_target: f64) -> Self {
        Self {
            name,
            dc_target,
            mechanism_type: MechanismType::Custom,
            dhsr: None,
            implementations: Vec::new(),
            failure_modes: Vec::new(),
        }
    }

    /// Set mechanism type
    pub fn with_type(mut self, mechanism_type: MechanismType) -> Self {
        self.mechanism_type = mechanism_type;
        self
    }

    /// Set the DHSR for this PSM
    pub fn with_dhsr(mut self, dhsr: DiagnosticHardwareSafetyRequirement) -> Self {
        self.dhsr = Some(dhsr);
        self
    }

    /// Add a design implementation
    pub fn with_implementation(mut self, design_ref: DesignRef) -> Self {
        self.implementations.push(design_ref);
        self
    }
}

/// Types of safety mechanisms
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum MechanismType {
    /// CRC/Checksum for data integrity
    Crc,
    /// ECC for memory protection
    Ecc,
    /// Triple Modular Redundancy
    Tmr,
    /// Dual lockstep comparison
    Lockstep,
    /// Watchdog timer
    Watchdog,
    /// Deadline monitoring
    DeadlineMonitor,
    /// Sequence checking
    SequenceCheck,
    /// Voltage monitoring
    VoltageMonitor,
    /// Built-in self-test
    Bist,
    /// Custom mechanism
    Custom,
}

// ============================================================================
// Diagnostic Hardware Safety Requirement (DHSR)
// ============================================================================

/// Diagnostic Hardware Safety Requirement
/// Child of PSM - specifies diagnostic requirements
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DiagnosticHardwareSafetyRequirement {
    /// Identifier (e.g., "DHSR_001")
    pub id: String,
    /// Requirement text
    pub requirement: String,
    /// Maximum detection time
    pub detection_time: Option<Duration>,
}

impl DiagnosticHardwareSafetyRequirement {
    /// Create a new DHSR
    pub fn new(id: String, requirement: String) -> Self {
        Self {
            id,
            requirement,
            detection_time: None,
        }
    }

    /// Set detection time
    pub fn with_detection_time(mut self, duration: Duration) -> Self {
        self.detection_time = Some(duration);
        self
    }
}

// ============================================================================
// Latent Safety Mechanism (LSM)
// ============================================================================

/// Latent Safety Mechanism for latent fault detection
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LatentSafetyMechanism {
    /// Mechanism name
    pub name: String,
    /// Target latent fault coverage (percentage)
    pub lc_target: f64,
    /// Test interval
    pub interval: Option<Duration>,

    /// Design bindings
    pub implementations: Vec<DesignRef>,

    /// FMEA failure modes detected by this LSM
    pub failure_modes: Vec<FailureModeBinding>,
}

impl LatentSafetyMechanism {
    /// Create a new LSM
    pub fn new(name: String, lc_target: f64) -> Self {
        Self {
            name,
            lc_target,
            interval: None,
            implementations: Vec::new(),
            failure_modes: Vec::new(),
        }
    }

    /// Set test interval
    pub fn with_interval(mut self, interval: Duration) -> Self {
        self.interval = Some(interval);
        self
    }
}

// ============================================================================
// FMEA Types (Hierarchical)
// ============================================================================

/// FMEA component with failure modes grouped by detector
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FmeaComponent {
    /// Design reference (e.g., top.brake_main::pressure_a)
    pub design_ref: DesignRef,
    /// FMEDA library name
    pub library: String,
    /// Part identifier in library
    pub part: String,

    /// Failure modes grouped by detecting PSM (psm_name -> modes)
    pub psm_detected: IndexMap<String, Vec<FailureMode>>,
    /// Failure modes grouped by detecting LSM (lsm_name -> modes)
    pub lsm_detected: IndexMap<String, Vec<FailureMode>>,
    /// Safe failure modes (no detection needed)
    pub safe_modes: Vec<FailureMode>,
}

impl FmeaComponent {
    /// Create a new FMEA component
    pub fn new(design_ref: DesignRef, library: String, part: String) -> Self {
        Self {
            design_ref,
            library,
            part,
            psm_detected: IndexMap::new(),
            lsm_detected: IndexMap::new(),
            safe_modes: Vec::new(),
        }
    }

    /// Add a failure mode detected by PSM
    pub fn add_psm_failure_mode(&mut self, psm_name: &str, mode: FailureMode) {
        self.psm_detected
            .entry(psm_name.to_string())
            .or_default()
            .push(mode);
    }

    /// Add a failure mode detected by LSM
    pub fn add_lsm_failure_mode(&mut self, lsm_name: &str, mode: FailureMode) {
        self.lsm_detected
            .entry(lsm_name.to_string())
            .or_default()
            .push(mode);
    }

    /// Add a safe failure mode
    pub fn add_safe_mode(&mut self, mode: FailureMode) {
        self.safe_modes.push(mode);
    }

    /// Calculate the total FIT rate for this component
    /// Considers multi-contributor weights for failure modes with contributors
    pub fn calculate_total_fit(&self) -> f64 {
        let mut total_fit = 0.0;

        // Sum PSM-detected failure modes
        for modes in self.psm_detected.values() {
            for mode in modes {
                if let Some(fit) = mode.get_fit_contribution(&self.design_ref) {
                    total_fit += fit;
                } else if let Some(base_fit) = mode.base_fit {
                    // If no contributor info matches, use base FIT (backward compat)
                    if mode.contributors.is_none() {
                        total_fit += base_fit;
                    }
                }
            }
        }

        // Sum LSM-detected failure modes
        for modes in self.lsm_detected.values() {
            for mode in modes {
                if let Some(fit) = mode.get_fit_contribution(&self.design_ref) {
                    total_fit += fit;
                } else if let Some(base_fit) = mode.base_fit {
                    if mode.contributors.is_none() {
                        total_fit += base_fit;
                    }
                }
            }
        }

        // Sum safe failure modes
        for mode in &self.safe_modes {
            if let Some(fit) = mode.get_fit_contribution(&self.design_ref) {
                total_fit += fit;
            } else if let Some(base_fit) = mode.base_fit {
                if mode.contributors.is_none() {
                    total_fit += base_fit;
                }
            }
        }

        total_fit
    }
}

/// Cross-component failure mode that spans multiple design entities
/// Used for failure modes where no single component "owns" the failure
/// Examples:
/// - Timing failure: 50% clock tree, 50% interconnect
/// - Data corruption: 40% sender, 40% receiver, 20% bus
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CrossComponentFailureMode {
    /// Unique identifier for this failure mode
    pub id: String,
    /// The underlying failure mode with multi-contributor info
    pub failure_mode: FailureMode,
    /// Detecting mechanism (PSM, LSM, or Safe)
    pub detector: DetectorRef,
    /// Description of the cross-component nature
    pub description: String,
}

impl CrossComponentFailureMode {
    /// Create a new cross-component failure mode
    pub fn new(
        id: String,
        name: String,
        severity: Severity,
        class: FailureClass,
        contributors: Vec<FailureContributor>,
        detector: DetectorRef,
    ) -> Self {
        Self {
            id,
            failure_mode: FailureMode::new(name, severity, class).with_contributors(contributors),
            detector,
            description: String::new(),
        }
    }

    /// Set description
    pub fn with_description(mut self, desc: String) -> Self {
        self.description = desc;
        self
    }

    /// Set base FIT rate (total, before splitting by contributors)
    pub fn with_fit(mut self, fit: f64) -> Self {
        self.failure_mode.base_fit = Some(fit);
        self
    }

    /// Set coverage
    pub fn with_coverage(mut self, coverage: f64) -> Self {
        self.failure_mode.coverage = coverage;
        self
    }

    /// Get FIT contribution for a specific design entity
    pub fn get_fit_contribution(&self, design_ref: &DesignRef) -> Option<f64> {
        self.failure_mode.get_fit_contribution(design_ref)
    }

    /// Validate that contributors are properly configured
    pub fn validate(&self) -> Result<(), String> {
        if self.failure_mode.contributors.is_none() {
            return Err("Cross-component failure mode must have contributors".to_string());
        }
        self.failure_mode.validate_contributors()
    }
}

/// Container for all FMEA data including cross-component failure modes
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct FmeaData {
    /// Component-specific failure modes
    pub components: Vec<FmeaComponent>,
    /// Cross-component failure modes (spanning multiple entities)
    pub cross_component_modes: Vec<CrossComponentFailureMode>,
}

impl FmeaData {
    /// Create a new FMEA data container
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a component
    pub fn add_component(&mut self, component: FmeaComponent) {
        self.components.push(component);
    }

    /// Add a cross-component failure mode
    pub fn add_cross_component_mode(&mut self, mode: CrossComponentFailureMode) {
        self.cross_component_modes.push(mode);
    }

    /// Calculate total FIT rate for a specific design entity
    /// Includes contributions from both component-specific and cross-component modes
    pub fn calculate_entity_fit(&self, design_ref: &DesignRef) -> f64 {
        let mut total_fit = 0.0;

        // Component-specific FIT
        for component in &self.components {
            if &component.design_ref == design_ref {
                total_fit += component.calculate_total_fit();
            }
        }

        // Cross-component FIT contributions
        for cross_mode in &self.cross_component_modes {
            if let Some(fit) = cross_mode.get_fit_contribution(design_ref) {
                total_fit += fit;
            }
        }

        total_fit
    }

    /// Calculate total FIT rate for all entities
    pub fn calculate_total_fit(&self) -> f64 {
        let mut total_fit = 0.0;

        // Sum all component FIT rates
        for component in &self.components {
            total_fit += component.calculate_total_fit();
        }

        // Add cross-component FIT rates (already include contributor weights)
        for cross_mode in &self.cross_component_modes {
            if let Some(fit) = cross_mode.failure_mode.base_fit {
                total_fit += fit;
            }
        }

        total_fit
    }

    /// Get all failure modes affecting a specific design entity
    pub fn get_failure_modes_for_entity(&self, design_ref: &DesignRef) -> Vec<&FailureMode> {
        let mut modes = Vec::new();

        // Component-specific modes
        for component in &self.components {
            if &component.design_ref == design_ref {
                for mode_list in component.psm_detected.values() {
                    modes.extend(mode_list.iter());
                }
                for mode_list in component.lsm_detected.values() {
                    modes.extend(mode_list.iter());
                }
                modes.extend(component.safe_modes.iter());
            }
        }

        // Cross-component modes where this entity contributes
        for cross_mode in &self.cross_component_modes {
            if let Some(contributors) = &cross_mode.failure_mode.contributors {
                if contributors.iter().any(|c| &c.design_ref == design_ref) {
                    modes.push(&cross_mode.failure_mode);
                }
            }
        }

        modes
    }

    /// Validate all failure modes
    pub fn validate(&self) -> Result<(), Vec<String>> {
        let mut errors = Vec::new();

        // Validate cross-component modes
        for cross_mode in &self.cross_component_modes {
            if let Err(e) = cross_mode.validate() {
                errors.push(format!("Cross-component mode '{}': {}", cross_mode.id, e));
            }
        }

        // Validate multi-contributor modes in components
        for component in &self.components {
            for (psm, modes) in &component.psm_detected {
                for mode in modes {
                    if mode.is_multi_contributor() {
                        if let Err(e) = mode.validate_contributors() {
                            errors.push(format!(
                                "Component '{}', PSM '{}', mode '{}': {}",
                                component.design_ref, psm, mode.name, e
                            ));
                        }
                    }
                }
            }
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }
}

/// Individual failure mode
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FailureMode {
    /// Failure mode name
    pub name: String,
    /// Effect description
    pub effect: String,
    /// Severity classification
    pub severity: Severity,
    /// Diagnostic/latent coverage (percentage)
    pub coverage: f64,
    /// Failure mode class
    pub class: FailureClass,
    /// Base FIT rate (from library)
    pub base_fit: Option<f64>,
    /// Multi-contributor weights (for failure modes spanning multiple entities)
    /// If None, 100% contribution from parent component
    /// Sum of weights should equal 1.0 (100%)
    pub contributors: Option<Vec<FailureContributor>>,
}

/// Contributor to a multi-entity failure mode
/// Used when a failure mode has contributions from multiple design entities
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FailureContributor {
    /// Design reference to the contributing entity
    pub design_ref: DesignRef,
    /// Weight/contribution (0.0 to 1.0, representing 0% to 100%)
    pub weight: f64,
    /// Optional description of how this entity contributes
    pub contribution_description: Option<String>,
}

impl FailureContributor {
    /// Create a new failure contributor
    pub fn new(design_ref: DesignRef, weight: f64) -> Self {
        Self {
            design_ref,
            weight,
            contribution_description: None,
        }
    }

    /// Add a description of the contribution
    pub fn with_description(mut self, desc: String) -> Self {
        self.contribution_description = Some(desc);
        self
    }
}

impl FailureMode {
    /// Create a new failure mode
    pub fn new(name: String, severity: Severity, class: FailureClass) -> Self {
        Self {
            name,
            effect: String::new(),
            severity,
            coverage: 0.0,
            class,
            base_fit: None,
            contributors: None,
        }
    }

    /// Set effect description
    pub fn with_effect(mut self, effect: String) -> Self {
        self.effect = effect;
        self
    }

    /// Set coverage
    pub fn with_coverage(mut self, coverage: f64) -> Self {
        self.coverage = coverage;
        self
    }

    /// Set base FIT rate
    pub fn with_fit(mut self, fit: f64) -> Self {
        self.base_fit = Some(fit);
        self
    }

    /// Set contributors for multi-entity failure mode
    /// Contributors should have weights summing to 1.0
    pub fn with_contributors(mut self, contributors: Vec<FailureContributor>) -> Self {
        self.contributors = Some(contributors);
        self
    }

    /// Add a single contributor to this failure mode
    /// Converts to multi-contributor mode if not already
    pub fn add_contributor(&mut self, contributor: FailureContributor) {
        match &mut self.contributors {
            Some(contributors) => contributors.push(contributor),
            None => self.contributors = Some(vec![contributor]),
        }
    }

    /// Check if this is a multi-contributor failure mode
    pub fn is_multi_contributor(&self) -> bool {
        self.contributors
            .as_ref()
            .map(|c| c.len() > 1)
            .unwrap_or(false)
    }

    /// Validate that contributor weights sum to approximately 1.0
    pub fn validate_contributors(&self) -> Result<(), String> {
        if let Some(contributors) = &self.contributors {
            let total_weight: f64 = contributors.iter().map(|c| c.weight).sum();
            // Allow 1% tolerance for floating point
            if (total_weight - 1.0).abs() > 0.01 {
                return Err(format!(
                    "Contributor weights sum to {:.2}, expected 1.0",
                    total_weight
                ));
            }
            // Check for negative weights
            for c in contributors {
                if c.weight < 0.0 {
                    return Err(format!(
                        "Negative weight {} for contributor {:?}",
                        c.weight,
                        c.design_ref.to_string()
                    ));
                }
            }
        }
        Ok(())
    }

    /// Get the effective FIT rate contribution from a specific design entity
    /// Returns None if this entity doesn't contribute to this failure mode
    pub fn get_fit_contribution(&self, design_ref: &DesignRef) -> Option<f64> {
        let base_fit = self.base_fit?;

        match &self.contributors {
            Some(contributors) => {
                // Find the contributor matching this design ref
                contributors
                    .iter()
                    .find(|c| &c.design_ref == design_ref)
                    .map(|c| base_fit * c.weight)
            }
            None => {
                // Single contributor mode - return full FIT if no multi-contributor
                Some(base_fit)
            }
        }
    }
}

/// Severity classification (S1-S3)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Severity {
    /// S1 - Light or moderate injuries
    S1,
    /// S2 - Severe injuries
    S2,
    /// S3 - Life-threatening or fatal injuries
    S3,
}

/// Failure mode classification
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum FailureClass {
    /// Safe failure - doesn't affect safety function
    Safe,
    /// Single point fault - can cause hazard alone
    SinglePointFault,
    /// Residual fault - remaining after mechanism
    Residual,
    /// Latent fault - undetected during operation
    Latent,
    /// Multi-point fault - requires multiple faults
    MultiPoint,
}

/// Binding from FMEA to mechanism (by nesting)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FailureModeBinding {
    /// Component reference
    pub component: DesignRef,
    /// Failure mode name
    pub mode: String,
    /// Coverage achieved
    pub coverage: f64,
}

// ============================================================================
// Hardware-Software Interface (HSI)
// ============================================================================

/// Hardware-Software Interface specification
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct HardwareSoftwareInterface {
    /// Included signal patterns
    pub includes: Vec<DesignPattern>,
    /// Excluded signal patterns
    pub excludes: Vec<DesignPattern>,
    /// Timing constraints
    pub timing: HsiTiming,
    /// Signal-specific ASIL overrides
    pub asil_overrides: IndexMap<String, AsilLevel>,
}

impl HardwareSoftwareInterface {
    /// Add an include pattern
    pub fn include(&mut self, pattern: DesignPattern) {
        self.includes.push(pattern);
    }

    /// Add an exclude pattern
    pub fn exclude(&mut self, pattern: DesignPattern) {
        self.excludes.push(pattern);
    }

    /// Check if a design reference is included in HSI
    pub fn is_included(&self, design_ref: &DesignRef) -> bool {
        // Check if excluded first
        for exclude in &self.excludes {
            if exclude.matches(design_ref) {
                return false;
            }
        }
        // Check if included
        for include in &self.includes {
            if include.matches(design_ref) {
                return true;
            }
        }
        false
    }
}

/// HSI timing constraints
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct HsiTiming {
    /// Global FTTI
    pub ftti: Option<Duration>,
    /// Signal-specific latency constraints
    pub signal_latencies: IndexMap<String, Duration>,
}

// ============================================================================
// Safety Entity (Layer 3) - Binding Layer
// ============================================================================

/// Unique identifier for a safety entity
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct SafetyEntityId(pub u32);

/// Safety entity binds goal to design instances
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SafetyEntity {
    /// Internal identifier
    pub id: SafetyEntityId,
    /// Entity name
    pub name: String,
    /// Goal being implemented
    pub implements: String,
    /// Design instances covered
    pub covers: Vec<DesignPattern>,

    /// PSM DC overrides (measured values)
    pub psm_overrides: IndexMap<String, f64>,
    /// LSM LC overrides (measured values)
    pub lsm_overrides: IndexMap<String, f64>,

    /// Hardware-Software Interface
    pub hsi: HardwareSoftwareInterface,
    /// FMEA components
    pub fmea: Vec<FmeaComponent>,

    /// Hierarchical child entities
    pub instances: Vec<SafetyEntityInstance>,

    /// ASIL decomposition (if this is a decomposed entity)
    pub decomposition: Option<AsilDecomposition>,

    /// Trait usages for composition
    pub trait_usages: Vec<TraitUsage>,
}

impl SafetyEntity {
    /// Create a new safety entity
    pub fn new(id: SafetyEntityId, name: String, implements: String) -> Self {
        Self {
            id,
            name,
            implements,
            covers: Vec::new(),
            psm_overrides: IndexMap::new(),
            lsm_overrides: IndexMap::new(),
            hsi: HardwareSoftwareInterface::default(),
            fmea: Vec::new(),
            instances: Vec::new(),
            decomposition: None,
            trait_usages: Vec::new(),
        }
    }

    /// Add a design pattern to cover
    pub fn covers(&mut self, pattern: DesignPattern) {
        self.covers.push(pattern);
    }

    /// Override PSM diagnostic coverage
    pub fn override_psm_dc(&mut self, psm_name: &str, dc: f64) {
        self.psm_overrides.insert(psm_name.to_string(), dc);
    }

    /// Override LSM latent coverage
    pub fn override_lsm_lc(&mut self, lsm_name: &str, lc: f64) {
        self.lsm_overrides.insert(lsm_name.to_string(), lc);
    }
}

/// Child safety entity instance
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SafetyEntityInstance {
    /// Instance name
    pub name: String,
    /// Entity type being instantiated
    pub entity_type: String,
    /// Instance-specific covers override
    pub covers_override: Option<Vec<DesignPattern>>,
    /// Instance-specific ASIL override
    pub asil_override: Option<AsilLevel>,
}

/// ASIL decomposition specification
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AsilDecomposition {
    /// Original ASIL being decomposed
    pub original_asil: AsilLevel,
    /// Decomposed parts (e.g., ASIL_B + ASIL_B)
    pub parts: Vec<AsilLevel>,
    /// Goal being decomposed
    pub decomposes: String,
}

impl AsilDecomposition {
    /// Check if decomposition is valid per ISO 26262
    pub fn is_valid(&self) -> bool {
        self.original_asil.decompose().contains(&(
            *self.parts.first().unwrap_or(&AsilLevel::QM),
            *self.parts.get(1).unwrap_or(&AsilLevel::QM),
        ))
    }
}

// ============================================================================
// Trait System (Composition)
// ============================================================================

/// Parameterized safety trait (reusable fragment)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SafetyTrait {
    /// Trait name
    pub name: String,
    /// Type parameters with defaults
    pub parameters: Vec<TraitParameter>,
    /// HSR definitions
    pub hsrs: Vec<HardwareSafetyRequirement>,
    /// LSM definitions
    pub lsms: Vec<LatentSafetyMechanism>,
}

/// Trait parameter definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TraitParameter {
    /// Parameter name
    pub name: String,
    /// Parameter type
    pub param_type: ParamType,
    /// Default value
    pub default: Option<ParamValue>,
}

/// Parameter types
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum ParamType {
    /// Floating point (for DC, LC percentages)
    Float,
    /// Duration (for FTTI, intervals)
    Duration,
    /// Integer
    Integer,
    /// String
    String,
}

/// Parameter values
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ParamValue {
    Float(f64),
    Duration(Duration),
    Integer(i64),
    String(String),
}

/// Trait usage with overrides
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TraitUsage {
    /// Trait being used
    pub trait_name: String,
    /// Parameter overrides (positional)
    pub param_overrides: Vec<ParamValue>,
    /// Member overrides
    pub member_overrides: Vec<MemberOverride>,
    /// Additional members
    pub additions: Vec<Addition>,
    /// Members to remove
    pub removals: Vec<String>,
}

/// Member override specification
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum MemberOverride {
    /// Override PSM diagnostic coverage
    Psm { name: String, dc: f64 },
    /// Override LSM latent coverage
    Lsm { name: String, lc: f64 },
    /// Override failure mode coverage
    FailureMode {
        component: String,
        mode: String,
        coverage: f64,
    },
}

/// Additional member to add to trait
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Addition {
    /// Add HSR
    Hsr(HardwareSafetyRequirement),
    /// Add LSM
    Lsm(LatentSafetyMechanism),
}

/// FMEA trait with wildcard patterns
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FmeaTrait {
    /// Trait name
    pub name: String,
    /// Component patterns (with wildcards)
    pub component_patterns: Vec<FmeaComponentPattern>,
}

/// FMEA component pattern for matching
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FmeaComponentPattern {
    /// Pattern (e.g., "*::sda" matches any instance's sda signal)
    pub pattern: String,
    /// Library name
    pub library: String,
    /// Part identifier
    pub part: String,
    /// Failure mode groups by mechanism
    pub failure_mode_groups: Vec<FailureModeGroup>,
}

/// Failure modes grouped by detecting mechanism
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FailureModeGroup {
    /// Detecting mechanism type
    pub detector: DetectorRef,
    /// Failure modes
    pub modes: Vec<FailureMode>,
}

/// Reference to a detecting mechanism
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum DetectorRef {
    /// PSM detector
    Psm(String),
    /// LSM detector
    Lsm(String),
    /// Safe (no detector needed)
    Safe,
}

/// HSI trait for reusable interface patterns
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HsiTrait {
    /// Trait name
    pub name: String,
    /// Signal patterns to include
    pub signal_patterns: Vec<String>,
    /// Timing patterns
    pub timing_patterns: Vec<HsiTimingPattern>,
}

/// HSI timing pattern
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HsiTimingPattern {
    /// Signal pattern
    pub signal_pattern: String,
    /// Maximum latency
    pub max_latency: Duration,
}

// ============================================================================
// Safety Hierarchy (Complete Structure)
// ============================================================================

/// Relationship types between safety entities
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum SafetyRelationType {
    /// Parent-child relationship
    ParentChild,
    /// Implements relationship (entity implements goal)
    Implements,
    /// Decomposes relationship (ASIL decomposition)
    Decomposes,
    /// Depends on relationship
    DependsOn,
}

/// Complete safety hierarchy
#[derive(Debug, Clone)]
pub struct SafetyHierarchy {
    /// Safety goals by name
    pub goals: IndexMap<String, SafetyGoal>,
    /// Safety entities by name
    pub entities: IndexMap<String, SafetyEntity>,
    /// Safety traits by name
    pub traits: IndexMap<String, SafetyTrait>,
    /// FMEA traits by name
    pub fmea_traits: IndexMap<String, FmeaTrait>,
    /// HSI traits by name
    pub hsi_traits: IndexMap<String, HsiTrait>,
    /// Relationship graph
    pub relationship_graph: Graph<String, SafetyRelationType>,
    /// Node indices for graph
    pub node_indices: IndexMap<String, NodeIndex>,
}

impl SafetyHierarchy {
    /// Create a new empty hierarchy
    pub fn new() -> Self {
        Self {
            goals: IndexMap::new(),
            entities: IndexMap::new(),
            traits: IndexMap::new(),
            fmea_traits: IndexMap::new(),
            hsi_traits: IndexMap::new(),
            relationship_graph: Graph::new(),
            node_indices: IndexMap::new(),
        }
    }

    /// Add a safety goal
    pub fn add_goal(&mut self, goal: SafetyGoal) {
        let name = goal.name.clone();
        let node = self.relationship_graph.add_node(name.clone());
        self.node_indices.insert(name.clone(), node);
        self.goals.insert(name, goal);
    }

    /// Add a safety entity
    pub fn add_entity(&mut self, entity: SafetyEntity) {
        let name = entity.name.clone();
        let implements = entity.implements.clone();

        let node = self.relationship_graph.add_node(name.clone());
        self.node_indices.insert(name.clone(), node);

        // Add implements relationship
        if let Some(&goal_node) = self.node_indices.get(&implements) {
            self.relationship_graph
                .add_edge(node, goal_node, SafetyRelationType::Implements);
        }

        self.entities.insert(name, entity);
    }

    /// Add a safety trait
    pub fn add_trait(&mut self, trait_def: SafetyTrait) {
        self.traits.insert(trait_def.name.clone(), trait_def);
    }

    /// Add an FMEA trait
    pub fn add_fmea_trait(&mut self, trait_def: FmeaTrait) {
        self.fmea_traits.insert(trait_def.name.clone(), trait_def);
    }

    /// Add an HSI trait
    pub fn add_hsi_trait(&mut self, trait_def: HsiTrait) {
        self.hsi_traits.insert(trait_def.name.clone(), trait_def);
    }

    /// Expand all trait usages into concrete members
    pub fn expand_traits(&mut self) -> Result<(), TraitExpansionError> {
        // Expand traits in goals
        for goal in self.goals.values_mut() {
            for usage in &goal.trait_usages {
                if let Some(trait_def) = self.traits.get(&usage.trait_name) {
                    // Apply parameter substitution and merge HSRs/LSMs
                    for hsr in &trait_def.hsrs {
                        let mut hsr_clone = hsr.clone();
                        // Apply member overrides
                        for override_spec in &usage.member_overrides {
                            if let MemberOverride::Psm { name, dc } = override_spec {
                                if let Some(ref mut psm) = hsr_clone.psm {
                                    if psm.name == *name {
                                        psm.dc_target = *dc;
                                    }
                                }
                            }
                        }
                        goal.hsrs.push(hsr_clone);
                    }
                    for lsm in &trait_def.lsms {
                        goal.lsms.push(lsm.clone());
                    }
                } else {
                    return Err(TraitExpansionError::TraitNotFound(usage.trait_name.clone()));
                }
            }
        }
        Ok(())
    }

    /// Generate traceability matrix from hierarchical structure
    pub fn generate_traceability(&self) -> TraceabilityMatrix {
        let mut entries = Vec::new();

        for goal in self.goals.values() {
            // Goal level entry
            entries.push(TraceabilityEntry {
                level: TraceLevel::SafetyGoal,
                id: goal.external_id.clone(),
                description: goal.description.clone(),
                parent: None,
                children: goal.hsrs.iter().map(|h| h.id.clone()).collect(),
                design_refs: Vec::new(),
                verification: Vec::new(),
            });

            // HSR entries
            for hsr in &goal.hsrs {
                let mut hsr_entry = TraceabilityEntry {
                    level: TraceLevel::Hsr,
                    id: hsr.id.clone(),
                    description: hsr.requirement.clone(),
                    parent: Some(goal.external_id.clone()),
                    children: Vec::new(),
                    design_refs: Vec::new(),
                    verification: hsr.verification.clone(),
                };

                // PSM entries (if present)
                if let Some(ref psm) = hsr.psm {
                    hsr_entry.children.push(psm.name.clone());

                    let mut psm_entry = TraceabilityEntry {
                        level: TraceLevel::Psm,
                        id: psm.name.clone(),
                        description: format!("DC >= {}%", psm.dc_target),
                        parent: Some(hsr.id.clone()),
                        children: Vec::new(),
                        design_refs: psm.implementations.iter().map(|r| r.to_string()).collect(),
                        verification: Vec::new(),
                    };

                    // DHSR entries (if present)
                    if let Some(ref dhsr) = psm.dhsr {
                        psm_entry.children.push(dhsr.id.clone());

                        entries.push(TraceabilityEntry {
                            level: TraceLevel::Dhsr,
                            id: dhsr.id.clone(),
                            description: dhsr.requirement.clone(),
                            parent: Some(psm.name.clone()),
                            children: Vec::new(),
                            design_refs: Vec::new(),
                            verification: Vec::new(),
                        });
                    }

                    entries.push(psm_entry);
                }

                entries.push(hsr_entry);
            }

            // LSM entries
            for lsm in &goal.lsms {
                entries.push(TraceabilityEntry {
                    level: TraceLevel::Lsm,
                    id: lsm.name.clone(),
                    description: format!("LC >= {}%, interval: {:?}", lsm.lc_target, lsm.interval),
                    parent: Some(goal.external_id.clone()),
                    children: Vec::new(),
                    design_refs: lsm.implementations.iter().map(|r| r.to_string()).collect(),
                    verification: Vec::new(),
                });
            }
        }

        TraceabilityMatrix { entries }
    }
}

impl Default for SafetyHierarchy {
    fn default() -> Self {
        Self::new()
    }
}

/// Traceability matrix generated from hierarchy
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TraceabilityMatrix {
    /// Traceability entries
    pub entries: Vec<TraceabilityEntry>,
}

/// Single entry in traceability matrix
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TraceabilityEntry {
    /// Level in hierarchy
    pub level: TraceLevel,
    /// Identifier
    pub id: String,
    /// Description
    pub description: String,
    /// Parent entry ID
    pub parent: Option<String>,
    /// Child entry IDs
    pub children: Vec<String>,
    /// Design references
    pub design_refs: Vec<String>,
    /// Verification methods
    pub verification: Vec<VerificationMethod>,
}

/// Traceability levels
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum TraceLevel {
    SafetyGoal,
    Hsr,
    Psm,
    Dhsr,
    Lsm,
    Fmea,
    Design,
}

/// Errors during trait expansion
#[derive(Debug, thiserror::Error)]
pub enum TraitExpansionError {
    #[error("Trait not found: {0}")]
    TraitNotFound(String),
    #[error("Conflicting definition: {0}")]
    ConflictingDefinition(String),
    #[error("Invalid parameter: {0}")]
    InvalidParameter(String),
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_instance_path() {
        let path = InstancePath::parse("top.brake_main.crc");
        assert_eq!(path.segments, vec!["top", "brake_main", "crc"]);
        assert_eq!(path.to_string(), "top.brake_main.crc");
        assert_eq!(path.leaf(), Some("crc"));
        assert_eq!(path.parent().unwrap().to_string(), "top.brake_main");
    }

    #[test]
    fn test_instance_path_glob() {
        let path = InstancePath::parse("top.sram_0");
        assert!(path.matches_pattern("top.sram_*"));
        assert!(path.matches_pattern("top.*"));
        assert!(!path.matches_pattern("top.cpu"));
    }

    #[test]
    fn test_design_ref() {
        let ref1 = DesignRef::parse("top.brake_main::pressure_a");
        assert_eq!(ref1.instance.to_string(), "top.brake_main");
        assert_eq!(ref1.signal, Some("pressure_a".to_string()));
        assert_eq!(ref1.bit_range, None);

        let ref2 = DesignRef::parse("top.cpu::state[7:0]");
        assert_eq!(ref2.signal, Some("state".to_string()));
        assert_eq!(ref2.bit_range, Some((7, 0)));
    }

    #[test]
    fn test_design_pattern() {
        let pattern = DesignPattern::signals("top.brake_*", "*");
        let ref1 = DesignRef::parse("top.brake_main::pressure_a");
        let ref2 = DesignRef::parse("top.cpu::state");

        assert!(pattern.matches(&ref1));
        assert!(!pattern.matches(&ref2));
    }

    #[test]
    fn test_safety_goal_creation() {
        let goal = SafetyGoal::new(
            SafetyGoalId(1),
            "SG-001".to_string(),
            "BrakingSafety".to_string(),
            "Prevent unintended braking".to_string(),
            AsilLevel::D,
        )
        .with_default_targets();

        assert_eq!(goal.target_spfm, Some(99.0));
        assert_eq!(goal.target_lfm, Some(90.0));
        assert_eq!(goal.target_pmhf, Some(10.0));
    }

    #[test]
    fn test_hsr_psm_dhsr_hierarchy() {
        let dhsr = DiagnosticHardwareSafetyRequirement::new(
            "DHSR_001".to_string(),
            "Voting error detected within 1 cycle".to_string(),
        )
        .with_detection_time(Duration::from_micros(100));

        let psm = PrimarySafetyMechanism::new("SensorVoting".to_string(), 99.0)
            .with_type(MechanismType::Tmr)
            .with_dhsr(dhsr);

        let hsr = HardwareSafetyRequirement::new(
            "HSR_001".to_string(),
            "Detect sensor faults within FTTI".to_string(),
        )
        .with_verification(VerificationMethod::FormalProve)
        .with_psm(psm);

        assert!(hsr.psm.is_some());
        assert!(hsr.psm.as_ref().unwrap().dhsr.is_some());
    }

    #[test]
    fn test_fmea_component() {
        let mut component = FmeaComponent::new(
            DesignRef::parse("top.brake_main::pressure_a"),
            "automotive_sensors".to_string(),
            "PRESSURE_SENSOR".to_string(),
        );

        component.add_psm_failure_mode(
            "SensorVoting",
            FailureMode::new("stuck".to_string(), Severity::S3, FailureClass::Residual)
                .with_coverage(99.5),
        );

        component.add_safe_mode(FailureMode::new(
            "open_circuit".to_string(),
            Severity::S1,
            FailureClass::Safe,
        ));

        assert_eq!(component.psm_detected.len(), 1);
        assert_eq!(component.safe_modes.len(), 1);
    }

    #[test]
    fn test_safety_hierarchy() {
        let mut hierarchy = SafetyHierarchy::new();

        let goal = SafetyGoal::new(
            SafetyGoalId(1),
            "SG-001".to_string(),
            "BrakingSafety".to_string(),
            "Prevent unintended braking".to_string(),
            AsilLevel::D,
        );

        let entity = SafetyEntity::new(
            SafetyEntityId(1),
            "BrakingControl".to_string(),
            "BrakingSafety".to_string(),
        );

        hierarchy.add_goal(goal);
        hierarchy.add_entity(entity);

        assert_eq!(hierarchy.goals.len(), 1);
        assert_eq!(hierarchy.entities.len(), 1);
        assert!(hierarchy.node_indices.contains_key("BrakingSafety"));
        assert!(hierarchy.node_indices.contains_key("BrakingControl"));
    }

    #[test]
    fn test_traceability_generation() {
        let mut hierarchy = SafetyHierarchy::new();

        let psm = PrimarySafetyMechanism::new("SensorVoting".to_string(), 99.0);
        let hsr = HardwareSafetyRequirement::new(
            "HSR_001".to_string(),
            "Detect sensor faults".to_string(),
        )
        .with_psm(psm);

        let mut goal = SafetyGoal::new(
            SafetyGoalId(1),
            "SG-001".to_string(),
            "BrakingSafety".to_string(),
            "Prevent unintended braking".to_string(),
            AsilLevel::D,
        );
        goal.hsrs.push(hsr);

        hierarchy.add_goal(goal);

        let matrix = hierarchy.generate_traceability();
        assert!(!matrix.entries.is_empty());

        // Check we have entries at different levels
        let levels: Vec<_> = matrix.entries.iter().map(|e| e.level).collect();
        assert!(levels.contains(&TraceLevel::SafetyGoal));
        assert!(levels.contains(&TraceLevel::Hsr));
        assert!(levels.contains(&TraceLevel::Psm));
    }

    #[test]
    fn test_asil_decomposition() {
        let decomp = AsilDecomposition {
            original_asil: AsilLevel::D,
            parts: vec![AsilLevel::B, AsilLevel::B],
            decomposes: "BrakingSafety".to_string(),
        };
        assert!(decomp.is_valid());

        let invalid_decomp = AsilDecomposition {
            original_asil: AsilLevel::D,
            parts: vec![AsilLevel::C, AsilLevel::C],
            decomposes: "BrakingSafety".to_string(),
        };
        assert!(!invalid_decomp.is_valid());
    }

    // =========================================================================
    // Multi-Contributor Failure Mode Tests
    // =========================================================================

    #[test]
    fn test_failure_contributor_creation() {
        let design_ref = DesignRef::parse("top.clock_tree::clk_out");
        let contributor = FailureContributor::new(design_ref.clone(), 0.5)
            .with_description("Clock distribution delay".to_string());

        assert_eq!(contributor.weight, 0.5);
        assert_eq!(
            contributor.contribution_description,
            Some("Clock distribution delay".to_string())
        );
    }

    #[test]
    fn test_multi_contributor_failure_mode() {
        // Create a timing failure that's 50% clock tree, 50% interconnect
        let clock_ref = DesignRef::parse("top.clock_tree::clk_out");
        let interconnect_ref = DesignRef::parse("top.interconnect::data_bus");

        let mode = FailureMode::new(
            "timing_violation".to_string(),
            Severity::S2,
            FailureClass::SinglePointFault,
        )
        .with_fit(100.0) // Total 100 FIT
        .with_coverage(95.0)
        .with_contributors(vec![
            FailureContributor::new(clock_ref.clone(), 0.5),
            FailureContributor::new(interconnect_ref.clone(), 0.5),
        ]);

        assert!(mode.is_multi_contributor());
        assert!(mode.validate_contributors().is_ok());

        // Check FIT contributions
        assert_eq!(mode.get_fit_contribution(&clock_ref), Some(50.0));
        assert_eq!(mode.get_fit_contribution(&interconnect_ref), Some(50.0));

        // Unknown ref should return None
        let other_ref = DesignRef::parse("top.cpu::core");
        assert_eq!(mode.get_fit_contribution(&other_ref), None);
    }

    #[test]
    fn test_multi_contributor_unequal_weights() {
        // Data corruption: 40% sender, 40% receiver, 20% bus
        let sender_ref = DesignRef::parse("top.sender::tx");
        let receiver_ref = DesignRef::parse("top.receiver::rx");
        let bus_ref = DesignRef::parse("top.bus::data");

        let mode = FailureMode::new(
            "data_corruption".to_string(),
            Severity::S3,
            FailureClass::SinglePointFault,
        )
        .with_fit(200.0) // Total 200 FIT
        .with_contributors(vec![
            FailureContributor::new(sender_ref.clone(), 0.4),
            FailureContributor::new(receiver_ref.clone(), 0.4),
            FailureContributor::new(bus_ref.clone(), 0.2),
        ]);

        assert!(mode.validate_contributors().is_ok());
        assert_eq!(mode.get_fit_contribution(&sender_ref), Some(80.0));
        assert_eq!(mode.get_fit_contribution(&receiver_ref), Some(80.0));
        assert_eq!(mode.get_fit_contribution(&bus_ref), Some(40.0));
    }

    #[test]
    fn test_contributor_validation_sum_error() {
        let ref1 = DesignRef::parse("top.a");
        let ref2 = DesignRef::parse("top.b");

        // Weights sum to 0.8, not 1.0
        let mode = FailureMode::new("bad_mode".to_string(), Severity::S1, FailureClass::Safe)
            .with_contributors(vec![
                FailureContributor::new(ref1, 0.5),
                FailureContributor::new(ref2, 0.3),
            ]);

        let result = mode.validate_contributors();
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("sum to 0.80"));
    }

    #[test]
    fn test_contributor_validation_negative_weight() {
        let ref1 = DesignRef::parse("top.a");
        let ref2 = DesignRef::parse("top.b");

        let mode = FailureMode::new("bad_mode".to_string(), Severity::S1, FailureClass::Safe)
            .with_contributors(vec![
                FailureContributor::new(ref1, 1.5),
                FailureContributor::new(ref2, -0.5), // Negative!
            ]);

        let result = mode.validate_contributors();
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("Negative weight"));
    }

    #[test]
    fn test_cross_component_failure_mode() {
        let clock_ref = DesignRef::parse("top.pll::clk");
        let dist_ref = DesignRef::parse("top.clock_dist::out");

        let cross_mode = CrossComponentFailureMode::new(
            "CCF-001".to_string(),
            "clock_jitter".to_string(),
            Severity::S2,
            FailureClass::SinglePointFault,
            vec![
                FailureContributor::new(clock_ref.clone(), 0.6)
                    .with_description("PLL phase noise".to_string()),
                FailureContributor::new(dist_ref.clone(), 0.4)
                    .with_description("Distribution skew".to_string()),
            ],
            DetectorRef::Psm("ClockMonitor".to_string()),
        )
        .with_fit(50.0)
        .with_coverage(90.0)
        .with_description("Jitter exceeds spec due to PLL and distribution".to_string());

        assert!(cross_mode.validate().is_ok());
        assert_eq!(cross_mode.get_fit_contribution(&clock_ref), Some(30.0));
        assert_eq!(cross_mode.get_fit_contribution(&dist_ref), Some(20.0));
    }

    #[test]
    fn test_cross_component_validation_no_contributors() {
        // A cross-component mode must have contributors
        let mode = CrossComponentFailureMode {
            id: "CCF-BAD".to_string(),
            failure_mode: FailureMode::new("bad".to_string(), Severity::S1, FailureClass::Safe),
            detector: DetectorRef::Safe,
            description: String::new(),
        };

        assert!(mode.validate().is_err());
    }

    #[test]
    fn test_fmea_data_with_cross_component() {
        let mut fmea_data = FmeaData::new();

        // Component-specific failure mode
        let sensor_ref = DesignRef::parse("top.sensor::out");
        let mut sensor_component = FmeaComponent::new(
            sensor_ref.clone(),
            "sensors".to_string(),
            "TEMP_SENSOR".to_string(),
        );
        sensor_component.add_psm_failure_mode(
            "TempMonitor",
            FailureMode::new(
                "stuck_high".to_string(),
                Severity::S2,
                FailureClass::Residual,
            )
            .with_fit(25.0),
        );
        fmea_data.add_component(sensor_component);

        // Cross-component failure mode
        let adc_ref = DesignRef::parse("top.adc::data");
        let cross_mode = CrossComponentFailureMode::new(
            "CCF-002".to_string(),
            "adc_coupling_noise".to_string(),
            Severity::S2,
            FailureClass::SinglePointFault,
            vec![
                FailureContributor::new(sensor_ref.clone(), 0.3),
                FailureContributor::new(adc_ref.clone(), 0.7),
            ],
            DetectorRef::Psm("NoiseFilter".to_string()),
        )
        .with_fit(100.0);
        fmea_data.add_cross_component_mode(cross_mode);

        // Validate
        assert!(fmea_data.validate().is_ok());

        // Calculate FIT for sensor (25 from component + 30 from cross-component)
        let sensor_fit = fmea_data.calculate_entity_fit(&sensor_ref);
        assert_eq!(sensor_fit, 55.0);

        // Calculate FIT for ADC (only from cross-component)
        let adc_fit = fmea_data.calculate_entity_fit(&adc_ref);
        assert_eq!(adc_fit, 70.0);

        // Total FIT: 25 (sensor component) + 100 (cross-component total)
        let total_fit = fmea_data.calculate_total_fit();
        assert_eq!(total_fit, 125.0);
    }

    #[test]
    fn test_fmea_data_get_failure_modes_for_entity() {
        let mut fmea_data = FmeaData::new();

        let ref_a = DesignRef::parse("top.a");
        let ref_b = DesignRef::parse("top.b");

        // Component with one mode
        let mut comp_a = FmeaComponent::new(ref_a.clone(), "lib".to_string(), "PART_A".to_string());
        comp_a.add_safe_mode(FailureMode::new(
            "mode_a".to_string(),
            Severity::S1,
            FailureClass::Safe,
        ));
        fmea_data.add_component(comp_a);

        // Cross-component mode affecting both
        fmea_data.add_cross_component_mode(CrossComponentFailureMode::new(
            "CCF-X".to_string(),
            "shared_mode".to_string(),
            Severity::S2,
            FailureClass::MultiPoint,
            vec![
                FailureContributor::new(ref_a.clone(), 0.5),
                FailureContributor::new(ref_b.clone(), 0.5),
            ],
            DetectorRef::Safe,
        ));

        // Entity A should see 2 modes (1 component + 1 cross-component)
        let modes_a = fmea_data.get_failure_modes_for_entity(&ref_a);
        assert_eq!(modes_a.len(), 2);

        // Entity B should see 1 mode (just cross-component)
        let modes_b = fmea_data.get_failure_modes_for_entity(&ref_b);
        assert_eq!(modes_b.len(), 1);
    }

    #[test]
    fn test_backward_compatibility_single_contributor() {
        // Failure modes without contributors should still work
        let mode = FailureMode::new(
            "legacy_mode".to_string(),
            Severity::S2,
            FailureClass::Residual,
        )
        .with_fit(50.0);

        assert!(!mode.is_multi_contributor());
        assert!(mode.validate_contributors().is_ok()); // No contributors = valid

        // get_fit_contribution returns full FIT for single contributor mode
        let any_ref = DesignRef::parse("top.any::signal");
        assert_eq!(mode.get_fit_contribution(&any_ref), Some(50.0));
    }

    #[test]
    fn test_add_contributor_incrementally() {
        let mut mode =
            FailureMode::new("incremental".to_string(), Severity::S1, FailureClass::Safe);

        let ref1 = DesignRef::parse("top.a");
        let ref2 = DesignRef::parse("top.b");

        mode.add_contributor(FailureContributor::new(ref1, 0.6));
        mode.add_contributor(FailureContributor::new(ref2, 0.4));

        assert!(mode.is_multi_contributor());
        assert!(mode.validate_contributors().is_ok());
    }
}
