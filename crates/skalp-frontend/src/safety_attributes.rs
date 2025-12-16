//! Safety Attributes for ISO 26262 Compliance
//!
//! This module defines the top-level safety constructs that can be declared
//! at module level in SKALP source files:
//! - `SafetyGoalDef` - Safety goals with ASIL levels and HSRs
//! - `SafetyMechanismDef` - Reusable safety mechanisms (independent, shareable)
//! - `HsiDef` - Hardware-Software Interface definitions
//!
//! These are distinct from instance-level annotations (`#[implements(...)]`)
//! which link components to these definitions.

use serde::{Deserialize, Serialize};

// Re-export ASIL level from skalp-safety if available, or define locally
/// ASIL Level per ISO 26262
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize)]
pub enum AsilLevel {
    /// Quality Management (no safety requirements)
    #[default]
    Qm,
    /// ASIL A - lowest automotive safety integrity level
    A,
    /// ASIL B
    B,
    /// ASIL C
    C,
    /// ASIL D - highest automotive safety integrity level
    D,
}

impl std::fmt::Display for AsilLevel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AsilLevel::Qm => write!(f, "QM"),
            AsilLevel::A => write!(f, "ASIL-A"),
            AsilLevel::B => write!(f, "ASIL-B"),
            AsilLevel::C => write!(f, "ASIL-C"),
            AsilLevel::D => write!(f, "ASIL-D"),
        }
    }
}

impl std::str::FromStr for AsilLevel {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_uppercase().as_str() {
            "QM" => Ok(AsilLevel::Qm),
            "A" | "ASIL-A" | "ASIL_A" => Ok(AsilLevel::A),
            "B" | "ASIL-B" | "ASIL_B" => Ok(AsilLevel::B),
            "C" | "ASIL-C" | "ASIL_C" => Ok(AsilLevel::C),
            "D" | "ASIL-D" | "ASIL_D" => Ok(AsilLevel::D),
            _ => Err(format!("Invalid ASIL level: {}", s)),
        }
    }
}

// ============================================================================
// Top-Level Safety Goal Definition
// ============================================================================

/// Top-level safety goal definition (independent, module-level)
///
/// Safety goals define what must not happen (hazards) and the required
/// ASIL level for protection. They contain HSRs (Hardware Safety Requirements)
/// that are derived from the goal.
///
/// Example in SKALP:
/// ```skalp
/// #[safety_goal(
///     id = "SG-001",
///     asil = "D",
///     description = "Prevent unintended motor activation",
///     ftti = 10_000_000,  // 10ms in nanoseconds
///     mechanisms = ["TmrVoting", "Watchdog"],
///     hsi = ["MotorControlHsi"]
/// )]
/// ```
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct SafetyGoalDef {
    /// Unique identifier for the safety goal (e.g., "SG-001")
    pub id: String,

    /// Human-readable description of the safety goal
    pub description: String,

    /// Required ASIL level for this goal
    pub asil: AsilLevel,

    /// Fault Tolerant Time Interval in nanoseconds
    /// Time available to reach safe state after fault detection
    pub ftti_ns: Option<u64>,

    /// References to SafetyMechanismDef names that protect this goal
    pub mechanism_refs: Vec<String>,

    /// References to HsiDef names associated with this goal
    pub hsi_refs: Vec<String>,

    /// Hardware Safety Requirements derived from this goal
    pub hsrs: Vec<HsrDef>,
}

impl SafetyGoalDef {
    /// Create a new safety goal with the given ID
    pub fn new(id: impl Into<String>) -> Self {
        Self {
            id: id.into(),
            ..Default::default()
        }
    }

    /// Builder: set ASIL level
    pub fn with_asil(mut self, asil: AsilLevel) -> Self {
        self.asil = asil;
        self
    }

    /// Builder: set description
    pub fn with_description(mut self, desc: impl Into<String>) -> Self {
        self.description = desc.into();
        self
    }

    /// Builder: set FTTI
    pub fn with_ftti(mut self, ftti_ns: u64) -> Self {
        self.ftti_ns = Some(ftti_ns);
        self
    }

    /// Builder: add mechanism reference
    pub fn with_mechanism(mut self, mechanism: impl Into<String>) -> Self {
        self.mechanism_refs.push(mechanism.into());
        self
    }

    /// Builder: add HSI reference
    pub fn with_hsi(mut self, hsi: impl Into<String>) -> Self {
        self.hsi_refs.push(hsi.into());
        self
    }

    /// Builder: add HSR
    pub fn with_hsr(mut self, hsr: HsrDef) -> Self {
        self.hsrs.push(hsr);
        self
    }
}

// ============================================================================
// Hardware Safety Requirement (HSR) Definition
// ============================================================================

/// Hardware Safety Requirement - derived from a SafetyGoal
///
/// HSRs live inside SafetyGoalDef because they are specific to
/// and derived from their parent goal.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct HsrDef {
    /// Unique identifier (e.g., "HSR-001-001")
    pub id: String,

    /// Description of the requirement
    pub description: String,

    /// How this requirement should be verified
    pub verification_methods: Vec<VerificationMethod>,

    /// Whether allocated to HW, SW, or both
    pub allocation: HsrAllocation,

    /// Traceability to parent safety goal
    pub parent_goal_id: Option<String>,
}

impl HsrDef {
    /// Create a new HSR with the given ID
    pub fn new(id: impl Into<String>) -> Self {
        Self {
            id: id.into(),
            ..Default::default()
        }
    }

    /// Builder: set description
    pub fn with_description(mut self, desc: impl Into<String>) -> Self {
        self.description = desc.into();
        self
    }

    /// Builder: add verification method
    pub fn with_verification(mut self, method: VerificationMethod) -> Self {
        self.verification_methods.push(method);
        self
    }

    /// Builder: set allocation
    pub fn with_allocation(mut self, allocation: HsrAllocation) -> Self {
        self.allocation = allocation;
        self
    }
}

/// Verification method for requirements
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum VerificationMethod {
    /// Verification by review/inspection
    Review,
    /// Verification by analysis/calculation
    Analysis,
    /// Verification by simulation
    Simulation,
    /// Verification by formal methods
    FormalVerification,
    /// Verification by hardware testing
    HardwareTest,
    /// Verification by fault injection
    FaultInjection,
    /// Custom verification method
    Custom(String),
}

/// Allocation of requirement to HW/SW
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub enum HsrAllocation {
    /// Requirement allocated to hardware only
    #[default]
    Hardware,
    /// Requirement allocated to software only
    Software,
    /// Requirement allocated to both HW and SW
    Both,
}

// ============================================================================
// Top-Level Safety Mechanism Definition
// ============================================================================

/// Safety mechanism type classification
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum MechanismType {
    /// Triple Modular Redundancy
    Tmr,
    /// Dual Modular Redundancy
    Dmr,
    /// Error Correcting Code (SECDED, etc.)
    Ecc,
    /// Cyclic Redundancy Check
    Crc,
    /// Lockstep execution comparison
    Lockstep,
    /// Watchdog timer
    Watchdog,
    /// Comparator (for redundant channels)
    Comparator,
    /// Parity check
    Parity,
    /// Memory BIST
    MemoryBist,
    /// Logic BIST
    LogicBist,
    /// Custom mechanism type
    Custom(String),
}

impl Default for MechanismType {
    fn default() -> Self {
        MechanismType::Custom("unknown".to_string())
    }
}

impl std::fmt::Display for MechanismType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MechanismType::Tmr => write!(f, "TMR"),
            MechanismType::Dmr => write!(f, "DMR"),
            MechanismType::Ecc => write!(f, "ECC"),
            MechanismType::Crc => write!(f, "CRC"),
            MechanismType::Lockstep => write!(f, "Lockstep"),
            MechanismType::Watchdog => write!(f, "Watchdog"),
            MechanismType::Comparator => write!(f, "Comparator"),
            MechanismType::Parity => write!(f, "Parity"),
            MechanismType::MemoryBist => write!(f, "MemoryBIST"),
            MechanismType::LogicBist => write!(f, "LogicBIST"),
            MechanismType::Custom(name) => write!(f, "{}", name),
        }
    }
}

impl std::str::FromStr for MechanismType {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "tmr" | "triple_modular_redundancy" => Ok(MechanismType::Tmr),
            "dmr" | "dual_modular_redundancy" => Ok(MechanismType::Dmr),
            "ecc" | "error_correcting_code" => Ok(MechanismType::Ecc),
            "crc" | "cyclic_redundancy_check" => Ok(MechanismType::Crc),
            "lockstep" => Ok(MechanismType::Lockstep),
            "watchdog" => Ok(MechanismType::Watchdog),
            "comparator" => Ok(MechanismType::Comparator),
            "parity" => Ok(MechanismType::Parity),
            "memory_bist" | "mbist" => Ok(MechanismType::MemoryBist),
            "logic_bist" | "lbist" => Ok(MechanismType::LogicBist),
            other => Ok(MechanismType::Custom(other.to_string())),
        }
    }
}

/// Top-level safety mechanism definition (independent, shareable)
///
/// Safety mechanisms are defined independently and can be shared
/// across multiple safety goals. They specify detection/coverage
/// capabilities and what they protect.
///
/// Example in SKALP:
/// ```skalp
/// #[safety_mechanism(
///     name = "TmrVoting",
///     type = "tmr",
///     dc = 99.0,
///     lc = 90.0,
///     coverage = ["alu_*", "reg_*"],
///     implementations = ["voter"]
/// )]
/// ```
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct SafetyMechanismDef {
    /// Unique name for this mechanism
    pub name: String,

    /// Type of safety mechanism
    pub mechanism_type: MechanismType,

    /// Target diagnostic coverage (percentage 0-100)
    pub dc_target: f64,

    /// Target latent coverage (percentage 0-100)
    pub lc_target: Option<f64>,

    /// Glob patterns for cells this mechanism covers
    /// e.g., ["alu_*", "reg_file_*"]
    pub coverage_patterns: Vec<String>,

    /// Component/instance paths that implement this mechanism
    pub implementations: Vec<String>,

    /// Human-readable description
    pub description: Option<String>,

    /// Fault Detection Time Interval in clock cycles
    pub fdti_cycles: Option<u32>,

    /// Fault Reaction Time Interval in clock cycles
    pub frti_cycles: Option<u32>,
}

impl SafetyMechanismDef {
    /// Create a new safety mechanism with the given name
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            ..Default::default()
        }
    }

    /// Builder: set mechanism type
    pub fn with_type(mut self, mtype: MechanismType) -> Self {
        self.mechanism_type = mtype;
        self
    }

    /// Builder: set DC target
    pub fn with_dc(mut self, dc: f64) -> Self {
        self.dc_target = dc;
        self
    }

    /// Builder: set LC target
    pub fn with_lc(mut self, lc: f64) -> Self {
        self.lc_target = Some(lc);
        self
    }

    /// Builder: add coverage pattern
    pub fn with_coverage(mut self, pattern: impl Into<String>) -> Self {
        self.coverage_patterns.push(pattern.into());
        self
    }

    /// Builder: add implementation
    pub fn with_implementation(mut self, impl_path: impl Into<String>) -> Self {
        self.implementations.push(impl_path.into());
        self
    }

    /// Builder: set description
    pub fn with_description(mut self, desc: impl Into<String>) -> Self {
        self.description = Some(desc.into());
        self
    }
}

// ============================================================================
// Hardware-Software Interface (HSI) Definition
// ============================================================================

/// Direction of HSI signal from hardware perspective
#[derive(Debug, Clone, Copy, Default, Serialize, Deserialize)]
pub enum HsiDirection {
    /// Hardware output to software (register read by SW)
    #[default]
    HwToSw,
    /// Software input to hardware (register written by SW)
    SwToHw,
    /// Bidirectional communication
    Bidirectional,
}

/// Safety classification for HSI signals
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub enum HsiSafetyClass {
    /// Safety-relevant signal with specific ASIL
    SafetyRelevant { asil: AsilLevel },
    /// Not safety-relevant (QM)
    #[default]
    NotSafetyRelevant,
}

/// Access type for software interface
#[derive(Debug, Clone, Copy, Default, Serialize, Deserialize)]
pub enum AccessType {
    /// Read-only from SW perspective
    #[default]
    Read,
    /// Write-only from SW perspective
    Write,
    /// Read-write from SW perspective
    ReadWrite,
}

/// End-to-end protection type
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum E2eProtection {
    /// CRC protection with specified polynomial width
    Crc { width: u8 },
    /// Rolling counter with specified width
    Counter { width: u8 },
    /// Both CRC and counter
    CrcAndCounter { crc_width: u8, counter_width: u8 },
    /// Custom E2E protection
    Custom(String),
}

/// Software interface specification for an HSI signal
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct SwInterfaceSpec {
    /// Register address (if memory-mapped)
    pub register_address: Option<u32>,

    /// Access type from SW perspective
    pub access_type: AccessType,

    /// Required refresh rate in milliseconds
    pub refresh_rate_ms: Option<u32>,

    /// E2E protection type
    pub e2e_protection: Option<E2eProtection>,
}

/// Single HSI signal definition
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct HsiSignalDef {
    /// Signal name
    pub name: String,

    /// Direction from HW perspective
    pub direction: HsiDirection,

    /// Signal width in bits
    pub width: u32,

    /// Port name pattern to match in netlist (e.g., "motor_cmd_*")
    pub port_pattern: String,

    /// Safety classification
    pub safety_class: HsiSafetyClass,

    /// Software interface specification
    pub sw_interface: SwInterfaceSpec,
}

impl HsiSignalDef {
    /// Create a new HSI signal definition
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            ..Default::default()
        }
    }

    /// Builder: set direction
    pub fn with_direction(mut self, dir: HsiDirection) -> Self {
        self.direction = dir;
        self
    }

    /// Builder: set width
    pub fn with_width(mut self, width: u32) -> Self {
        self.width = width;
        self
    }

    /// Builder: set port pattern
    pub fn with_port_pattern(mut self, pattern: impl Into<String>) -> Self {
        self.port_pattern = pattern.into();
        self
    }

    /// Builder: set safety class
    pub fn with_safety_class(mut self, class: HsiSafetyClass) -> Self {
        self.safety_class = class;
        self
    }
}

/// HSI timing requirement
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct HsiTimingReq {
    /// Signal pattern this timing applies to
    pub signal_pattern: String,

    /// Maximum latency in nanoseconds
    pub max_latency_ns: u64,

    /// Minimum update rate in Hz
    pub min_update_rate_hz: Option<f64>,
}

/// Top-level Hardware-Software Interface definition (independent, shareable)
///
/// HSI definitions describe the boundary between hardware and software,
/// including signal definitions, timing requirements, and safety classification.
///
/// Example in SKALP:
/// ```skalp
/// #[hsi(
///     name = "MotorControlHsi",
///     signals = [
///         { name = "motor_cmd", direction = "sw_to_hw", width = 16, pattern = "motor_cmd_*" },
///         { name = "motor_status", direction = "hw_to_sw", width = 8, pattern = "motor_status" }
///     ]
/// )]
/// ```
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct HsiDef {
    /// Unique name for this HSI
    pub name: String,

    /// Signal definitions
    pub signals: Vec<HsiSignalDef>,

    /// Timing requirements
    pub timing_requirements: Vec<HsiTimingReq>,

    /// Safety goals that use this HSI
    pub used_by_goals: Vec<String>,

    /// Human-readable description
    pub description: Option<String>,
}

impl HsiDef {
    /// Create a new HSI definition
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            ..Default::default()
        }
    }

    /// Builder: add signal
    pub fn with_signal(mut self, signal: HsiSignalDef) -> Self {
        self.signals.push(signal);
        self
    }

    /// Builder: add timing requirement
    pub fn with_timing(mut self, timing: HsiTimingReq) -> Self {
        self.timing_requirements.push(timing);
        self
    }

    /// Builder: set description
    pub fn with_description(mut self, desc: impl Into<String>) -> Self {
        self.description = Some(desc.into());
        self
    }
}

// ============================================================================
// Instance-Level Safety Annotation (extends existing SafetyConfig)
// ============================================================================

/// Component-level safety annotation (minimal, on instances/signals)
///
/// This is used with `#[implements("SafetyGoal")]` on components to link
/// them to the top-level safety definitions.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum SafetyAnnotation {
    /// This component implements a specific safety goal
    /// e.g., `#[implements("SG-001")]`
    ImplementsGoal { goal_id: String },

    /// This component is a safety mechanism implementation
    /// e.g., `#[is_safety_mechanism("TmrVoting")]`
    IsSafetyMechanism { mechanism_name: String },

    /// This component is safety-critical for the given reason
    /// e.g., `#[safety_critical(reason = "Primary control path")]`
    SafetyCritical { reason: String },
}

// ============================================================================
// Module-Level Safety Definitions Container
// ============================================================================

/// Container for all top-level safety definitions in a module
///
/// This is added to the HIR to hold module-level safety definitions.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct ModuleSafetyDefinitions {
    /// Safety goal definitions declared in this module
    pub safety_goals: Vec<SafetyGoalDef>,

    /// Safety mechanism definitions declared in this module
    pub safety_mechanisms: Vec<SafetyMechanismDef>,

    /// HSI definitions declared in this module
    pub hsi_definitions: Vec<HsiDef>,
}

impl ModuleSafetyDefinitions {
    /// Create empty definitions
    pub fn new() -> Self {
        Self::default()
    }

    /// Check if any safety definitions exist
    pub fn is_empty(&self) -> bool {
        self.safety_goals.is_empty()
            && self.safety_mechanisms.is_empty()
            && self.hsi_definitions.is_empty()
    }

    /// Add a safety goal
    pub fn add_safety_goal(&mut self, goal: SafetyGoalDef) {
        self.safety_goals.push(goal);
    }

    /// Add a safety mechanism
    pub fn add_safety_mechanism(&mut self, mechanism: SafetyMechanismDef) {
        self.safety_mechanisms.push(mechanism);
    }

    /// Add an HSI definition
    pub fn add_hsi(&mut self, hsi: HsiDef) {
        self.hsi_definitions.push(hsi);
    }

    /// Find a safety goal by ID
    pub fn find_goal(&self, id: &str) -> Option<&SafetyGoalDef> {
        self.safety_goals.iter().find(|g| g.id == id)
    }

    /// Find a safety mechanism by name
    pub fn find_mechanism(&self, name: &str) -> Option<&SafetyMechanismDef> {
        self.safety_mechanisms.iter().find(|m| m.name == name)
    }

    /// Find an HSI by name
    pub fn find_hsi(&self, name: &str) -> Option<&HsiDef> {
        self.hsi_definitions.iter().find(|h| h.name == name)
    }

    /// Validate all references (mechanisms in goals exist, etc.)
    pub fn validate_references(&self) -> Result<(), Vec<String>> {
        let mut errors = Vec::new();

        for goal in &self.safety_goals {
            // Check mechanism references
            for mech_ref in &goal.mechanism_refs {
                if self.find_mechanism(mech_ref).is_none() {
                    errors.push(format!(
                        "Safety goal '{}' references unknown mechanism '{}'",
                        goal.id, mech_ref
                    ));
                }
            }

            // Check HSI references
            for hsi_ref in &goal.hsi_refs {
                if self.find_hsi(hsi_ref).is_none() {
                    errors.push(format!(
                        "Safety goal '{}' references unknown HSI '{}'",
                        goal.id, hsi_ref
                    ));
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_asil_level_parsing() {
        assert_eq!("A".parse::<AsilLevel>().unwrap(), AsilLevel::A);
        assert_eq!("ASIL-D".parse::<AsilLevel>().unwrap(), AsilLevel::D);
        assert_eq!("qm".parse::<AsilLevel>().unwrap(), AsilLevel::Qm);
    }

    #[test]
    fn test_mechanism_type_parsing() {
        assert!(matches!(
            "tmr".parse::<MechanismType>().unwrap(),
            MechanismType::Tmr
        ));
        assert!(matches!(
            "ecc".parse::<MechanismType>().unwrap(),
            MechanismType::Ecc
        ));
        assert!(matches!(
            "my_custom".parse::<MechanismType>().unwrap(),
            MechanismType::Custom(s) if s == "my_custom"
        ));
    }

    #[test]
    fn test_safety_goal_builder() {
        let goal = SafetyGoalDef::new("SG-001")
            .with_asil(AsilLevel::D)
            .with_description("Prevent unintended motor activation")
            .with_ftti(10_000_000)
            .with_mechanism("TmrVoting")
            .with_hsi("MotorControlHsi");

        assert_eq!(goal.id, "SG-001");
        assert_eq!(goal.asil, AsilLevel::D);
        assert_eq!(goal.ftti_ns, Some(10_000_000));
        assert_eq!(goal.mechanism_refs.len(), 1);
        assert_eq!(goal.hsi_refs.len(), 1);
    }

    #[test]
    fn test_safety_mechanism_builder() {
        let mech = SafetyMechanismDef::new("TmrVoting")
            .with_type(MechanismType::Tmr)
            .with_dc(99.0)
            .with_coverage("alu_*")
            .with_implementation("voter");

        assert_eq!(mech.name, "TmrVoting");
        assert!(matches!(mech.mechanism_type, MechanismType::Tmr));
        assert_eq!(mech.dc_target, 99.0);
        assert_eq!(mech.coverage_patterns.len(), 1);
    }

    #[test]
    fn test_hsi_builder() {
        let signal = HsiSignalDef::new("motor_cmd")
            .with_direction(HsiDirection::SwToHw)
            .with_width(16)
            .with_port_pattern("motor_cmd_*");

        let hsi = HsiDef::new("MotorControlHsi")
            .with_signal(signal)
            .with_description("Motor control interface");

        assert_eq!(hsi.name, "MotorControlHsi");
        assert_eq!(hsi.signals.len(), 1);
        assert_eq!(hsi.signals[0].name, "motor_cmd");
    }

    #[test]
    fn test_module_definitions_validation() {
        let mut defs = ModuleSafetyDefinitions::new();

        // Add a goal that references non-existent mechanism
        let goal = SafetyGoalDef::new("SG-001").with_mechanism("NonExistent");
        defs.add_safety_goal(goal);

        // Validation should fail
        let result = defs.validate_references();
        assert!(result.is_err());
        assert!(result.unwrap_err()[0].contains("NonExistent"));
    }

    #[test]
    fn test_module_definitions_valid() {
        let mut defs = ModuleSafetyDefinitions::new();

        // Add mechanism first
        let mech = SafetyMechanismDef::new("TmrVoting").with_type(MechanismType::Tmr);
        defs.add_safety_mechanism(mech);

        // Add HSI
        let hsi = HsiDef::new("MotorControlHsi");
        defs.add_hsi(hsi);

        // Add goal that references them
        let goal = SafetyGoalDef::new("SG-001")
            .with_mechanism("TmrVoting")
            .with_hsi("MotorControlHsi");
        defs.add_safety_goal(goal);

        // Validation should pass
        assert!(defs.validate_references().is_ok());
    }
}
