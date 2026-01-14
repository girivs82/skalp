//! Hardware-Software Interface (HSI) Module
//!
//! Defines and analyzes the hardware-software interface boundary for ISO 26262 compliance.
//! The HSI specifies how hardware exposes its functionality to software and is critical
//! for deriving Hardware Safety Requirements (HSRs) from Software Safety Requirements.
//!
//! # ISO 26262 Context
//!
//! Per ISO 26262-4:7, the HSI includes:
//! - Description of hardware parts controlled by software
//! - Dependencies between hardware and software
//! - Resources consumed (timing, memory, etc.)
//! - Interaction under nominal and error conditions

use indexmap::IndexMap;
use serde::{Deserialize, Serialize};

// Re-use ASIL level from the asil module
use crate::asil::AsilLevel;

// ============================================================================
// HSI Data Structures
// ============================================================================

/// Top-level HSI definition (independent, can be shared across safety goals)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HardwareSoftwareInterface {
    /// Unique HSI name (e.g., "MotorControlHsi")
    pub name: String,
    /// Description of this HSI
    pub description: String,
    /// Signals exposed at this interface
    pub signals: Vec<HsiSignal>,
    /// Timing requirements for this interface
    pub timing_requirements: Vec<HsiTimingRequirement>,
    /// Safety goals that reference this HSI
    pub used_by_goals: Vec<String>,
    /// Version for configuration management
    pub version: String,
}

impl HardwareSoftwareInterface {
    /// Create a new HSI with the given name
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            description: String::new(),
            signals: Vec::new(),
            timing_requirements: Vec::new(),
            used_by_goals: Vec::new(),
            version: "1.0".to_string(),
        }
    }

    /// Add a signal to this HSI
    pub fn add_signal(&mut self, signal: HsiSignal) {
        self.signals.push(signal);
    }

    /// Add a timing requirement
    pub fn add_timing_requirement(&mut self, req: HsiTimingRequirement) {
        self.timing_requirements.push(req);
    }

    /// Get all safety-relevant signals
    pub fn safety_relevant_signals(&self) -> Vec<&HsiSignal> {
        self.signals
            .iter()
            .filter(|s| {
                matches!(
                    s.safety_classification,
                    HsiSafetyClass::SafetyRelevant { .. }
                )
            })
            .collect()
    }
}

/// Direction of HSI signal from hardware perspective
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum HsiDirection {
    /// Output from hardware, input to software (e.g., status registers)
    HwToSw,
    /// Input to hardware, output from software (e.g., command registers)
    SwToHw,
    /// Bidirectional (e.g., shared memory)
    Bidirectional,
}

/// Safety classification for HSI signals
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum HsiSafetyClass {
    /// Signal is safety-relevant with specified ASIL
    SafetyRelevant { asil: AsilLevel },
    /// Signal is not safety-relevant (QM)
    NotSafetyRelevant,
}

/// Software interface specification for HSI signals
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SwInterfaceSpec {
    /// Register address (if memory-mapped)
    pub register_address: Option<u32>,
    /// Register offset within base address
    pub register_offset: Option<u32>,
    /// Access type
    pub access_type: AccessType,
    /// Required refresh rate in milliseconds
    pub refresh_rate_ms: Option<u32>,
    /// End-to-end protection method
    pub e2e_protection: Option<E2eProtection>,
    /// Bit field within register (if applicable)
    pub bit_field: Option<BitField>,
}

impl Default for SwInterfaceSpec {
    fn default() -> Self {
        Self {
            register_address: None,
            register_offset: None,
            access_type: AccessType::ReadWrite,
            refresh_rate_ms: None,
            e2e_protection: None,
            bit_field: None,
        }
    }
}

/// Register access type
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum AccessType {
    /// Read-only access
    Read,
    /// Write-only access
    Write,
    /// Read and write access
    ReadWrite,
    /// Clear on read
    ClearOnRead,
    /// Write-1-to-clear
    Write1ToClear,
}

/// End-to-end protection method per AUTOSAR E2E
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct E2eProtection {
    /// E2E profile (e.g., "Profile1", "Profile4")
    pub profile: String,
    /// Counter length in bits
    pub counter_bits: u8,
    /// CRC polynomial (if applicable)
    pub crc_polynomial: Option<u32>,
    /// Data ID for protection
    pub data_id: u16,
}

/// Bit field specification
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BitField {
    /// Starting bit position
    pub start_bit: u8,
    /// Width in bits
    pub width: u8,
}

/// Signal in the HSI
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HsiSignal {
    /// Signal name
    pub name: String,
    /// Direction from hardware perspective
    pub direction: HsiDirection,
    /// Width in bits
    pub width: u32,
    /// Glob pattern to match RTL ports (e.g., "motor_cmd_*")
    pub port_pattern: String,
    /// Safety classification
    pub safety_classification: HsiSafetyClass,
    /// Software interface specification
    pub sw_interface: SwInterfaceSpec,
    /// Description
    pub description: String,
    /// Valid value range (if applicable)
    pub valid_range: Option<ValueRange>,
}

impl HsiSignal {
    /// Create a new HSI signal
    pub fn new(name: &str, direction: HsiDirection, width: u32) -> Self {
        Self {
            name: name.to_string(),
            direction,
            width,
            port_pattern: name.to_string(),
            safety_classification: HsiSafetyClass::NotSafetyRelevant,
            sw_interface: SwInterfaceSpec::default(),
            description: String::new(),
            valid_range: None,
        }
    }

    /// Set the safety classification
    pub fn with_asil(mut self, asil: AsilLevel) -> Self {
        self.safety_classification = HsiSafetyClass::SafetyRelevant { asil };
        self
    }

    /// Set the port pattern for RTL matching
    pub fn with_pattern(mut self, pattern: &str) -> Self {
        self.port_pattern = pattern.to_string();
        self
    }
}

/// Value range for HSI signals
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValueRange {
    /// Minimum valid value
    pub min: i64,
    /// Maximum valid value
    pub max: i64,
    /// Resolution (smallest increment)
    pub resolution: Option<f64>,
    /// Unit (e.g., "rpm", "degrees")
    pub unit: Option<String>,
}

/// Timing requirement for HSI signals
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HsiTimingRequirement {
    /// Signal pattern this applies to (glob)
    pub signal_pattern: String,
    /// Maximum latency in nanoseconds
    pub max_latency_ns: u64,
    /// Minimum update rate in Hz
    pub min_update_rate_hz: Option<f64>,
    /// Jitter tolerance in nanoseconds
    pub max_jitter_ns: Option<u64>,
    /// Description
    pub description: String,
}

// ============================================================================
// HSI FMEA
// ============================================================================

/// HSI failure mode types
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum HsiFailureMode {
    /// Signal stuck at logic 0
    StuckAt0,
    /// Signal stuck at logic 1
    StuckAt1,
    /// Data corruption (bit flip, wrong value)
    DataCorruption,
    /// Timing violation - late arrival
    LateArrival,
    /// Timing violation - early arrival
    EarlyArrival,
    /// Signal missing entirely
    SignalMissing,
    /// Sequence error (for multi-word transfers)
    SequenceError,
    /// Address error (wrong register accessed)
    AddressError,
}

/// FMEA entry for HSI failure
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HsiFmeaEntry {
    /// Signal name
    pub signal: String,
    /// Failure mode
    pub failure_mode: HsiFailureMode,
    /// Effect on hardware
    pub hw_effect: String,
    /// Effect on software
    pub sw_effect: String,
    /// Detection method (if any)
    pub detection_method: Option<String>,
    /// Mitigation measure (if any)
    pub mitigation: Option<String>,
    /// Severity (1-10 FMEA scale)
    pub severity: u8,
    /// Occurrence (1-10 FMEA scale)
    pub occurrence: u8,
    /// Detection rating (1-10 FMEA scale)
    pub detection: u8,
}

impl HsiFmeaEntry {
    /// Calculate Risk Priority Number (RPN)
    pub fn rpn(&self) -> u32 {
        self.severity as u32 * self.occurrence as u32 * self.detection as u32
    }
}

/// Generate FMEA entries for all HSI failure modes
pub fn generate_hsi_fmea(hsi: &HardwareSoftwareInterface) -> Vec<HsiFmeaEntry> {
    let mut entries = Vec::new();

    for signal in &hsi.signals {
        // Only generate FMEA for safety-relevant signals
        if !matches!(
            signal.safety_classification,
            HsiSafetyClass::SafetyRelevant { .. }
        ) {
            continue;
        }

        // Stuck-at-0 failure
        entries.push(HsiFmeaEntry {
            signal: signal.name.clone(),
            failure_mode: HsiFailureMode::StuckAt0,
            hw_effect: format!("Signal {} stuck at 0", signal.name),
            sw_effect: "Software receives constant 0 value".to_string(),
            detection_method: Some("Value monitoring, E2E protection".to_string()),
            mitigation: Some("Plausibility check, redundant channel".to_string()),
            severity: 8,
            occurrence: 3,
            detection: 4,
        });

        // Stuck-at-1 failure
        entries.push(HsiFmeaEntry {
            signal: signal.name.clone(),
            failure_mode: HsiFailureMode::StuckAt1,
            hw_effect: format!("Signal {} stuck at 1", signal.name),
            sw_effect: "Software receives constant max value".to_string(),
            detection_method: Some("Value monitoring, E2E protection".to_string()),
            mitigation: Some("Plausibility check, redundant channel".to_string()),
            severity: 8,
            occurrence: 3,
            detection: 4,
        });

        // Data corruption
        entries.push(HsiFmeaEntry {
            signal: signal.name.clone(),
            failure_mode: HsiFailureMode::DataCorruption,
            hw_effect: format!("Signal {} data corrupted", signal.name),
            sw_effect: "Software receives incorrect value".to_string(),
            detection_method: Some("CRC, parity, E2E protection".to_string()),
            mitigation: Some("Error correction, redundancy".to_string()),
            severity: 7,
            occurrence: 4,
            detection: 3,
        });

        // Timing violation for time-critical signals
        if signal.sw_interface.refresh_rate_ms.is_some() {
            entries.push(HsiFmeaEntry {
                signal: signal.name.clone(),
                failure_mode: HsiFailureMode::LateArrival,
                hw_effect: format!("Signal {} arrives late", signal.name),
                sw_effect: "Software uses stale data".to_string(),
                detection_method: Some("Timestamp, alive counter".to_string()),
                mitigation: Some("Timeout detection, safe state".to_string()),
                severity: 6,
                occurrence: 4,
                detection: 5,
            });
        }
    }

    entries
}

// ============================================================================
// HSI Port Linking
// ============================================================================

/// Result of linking HSI signals to actual RTL ports
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HsiPortMapping {
    /// Mapping from HSI signal name to matched RTL port names
    pub signal_to_ports: IndexMap<String, Vec<String>>,
    /// Patterns that didn't match any ports
    pub unmatched_patterns: Vec<String>,
    /// Coverage percentage (matched signals / total signals)
    pub coverage_percentage: f64,
}

impl HsiPortMapping {
    /// Check if all signals are matched
    pub fn is_complete(&self) -> bool {
        self.unmatched_patterns.is_empty()
    }
}

/// Link HSI signal patterns to actual port names from a netlist
///
/// # Arguments
/// * `hsi` - The HSI definition to link
/// * `port_names` - List of actual port names from the design
///
/// # Returns
/// Mapping result with matched and unmatched patterns
pub fn link_hsi_to_ports(hsi: &HardwareSoftwareInterface, port_names: &[String]) -> HsiPortMapping {
    let mut signal_to_ports: IndexMap<String, Vec<String>> = IndexMap::new();
    let mut unmatched_patterns = Vec::new();
    let mut total_signals = 0;
    let mut matched_signals = 0;

    for signal in &hsi.signals {
        total_signals += 1;
        let pattern = &signal.port_pattern;

        // Match port names using glob pattern
        let matches: Vec<String> = port_names
            .iter()
            .filter(|p| matches_glob_pattern(p, pattern))
            .cloned()
            .collect();

        if matches.is_empty() {
            unmatched_patterns.push(pattern.clone());
        } else {
            matched_signals += 1;
            signal_to_ports.insert(signal.name.clone(), matches);
        }
    }

    let coverage_percentage = if total_signals > 0 {
        (matched_signals as f64 / total_signals as f64) * 100.0
    } else {
        100.0
    };

    HsiPortMapping {
        signal_to_ports,
        unmatched_patterns,
        coverage_percentage,
    }
}

/// Check if a string matches a glob pattern (supports * and ? wildcards)
fn matches_glob_pattern(s: &str, pattern: &str) -> bool {
    glob_match(
        pattern.chars().collect::<Vec<_>>().as_slice(),
        s.chars().collect::<Vec<_>>().as_slice(),
    )
}

/// Recursive glob matching implementation
fn glob_match(pattern: &[char], text: &[char]) -> bool {
    match (pattern.first(), text.first()) {
        // Empty pattern matches empty text
        (None, None) => true,
        // Pattern exhausted but text remaining
        (None, Some(_)) => false,
        // Wildcard * matches zero or more characters
        (Some('*'), _) => {
            // Try matching zero chars (skip the *)
            if glob_match(&pattern[1..], text) {
                return true;
            }
            // Try matching one or more chars (consume text char, keep *)
            if !text.is_empty() && glob_match(pattern, &text[1..]) {
                return true;
            }
            false
        }
        // Single char wildcard ? matches any single character
        (Some('?'), Some(_)) => glob_match(&pattern[1..], &text[1..]),
        // No text left but pattern remaining (not *)
        (Some(_), None) => pattern.iter().all(|&c| c == '*'),
        // Exact character match
        (Some(&pc), Some(&tc)) if pc == tc => glob_match(&pattern[1..], &text[1..]),
        // No match
        _ => false,
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_hsi_creation() {
        let mut hsi = HardwareSoftwareInterface::new("MotorControlHsi");
        hsi.description = "Motor control interface".to_string();

        let signal = HsiSignal::new("motor_cmd", HsiDirection::SwToHw, 16)
            .with_asil(AsilLevel::D)
            .with_pattern("motor_cmd_*");

        hsi.add_signal(signal);

        assert_eq!(hsi.signals.len(), 1);
        assert_eq!(hsi.safety_relevant_signals().len(), 1);
    }

    #[test]
    fn test_hsi_fmea_generation() {
        let mut hsi = HardwareSoftwareInterface::new("TestHsi");

        let signal = HsiSignal::new("test_signal", HsiDirection::HwToSw, 8).with_asil(AsilLevel::B);

        hsi.add_signal(signal);

        let fmea = generate_hsi_fmea(&hsi);
        assert!(!fmea.is_empty());

        // Should have entries for stuck-at-0, stuck-at-1, and data corruption
        assert!(fmea.len() >= 3);
    }

    #[test]
    fn test_hsi_port_linking() {
        let mut hsi = HardwareSoftwareInterface::new("TestHsi");

        let signal = HsiSignal::new("data", HsiDirection::HwToSw, 8).with_pattern("data_*");

        hsi.add_signal(signal);

        let ports = vec![
            "data_in".to_string(),
            "data_out".to_string(),
            "clock".to_string(),
        ];

        let mapping = link_hsi_to_ports(&hsi, &ports);

        assert!(mapping.unmatched_patterns.is_empty());
        assert_eq!(mapping.signal_to_ports.get("data").unwrap().len(), 2);
    }

    #[test]
    fn test_rpn_calculation() {
        let entry = HsiFmeaEntry {
            signal: "test".to_string(),
            failure_mode: HsiFailureMode::StuckAt0,
            hw_effect: "".to_string(),
            sw_effect: "".to_string(),
            detection_method: None,
            mitigation: None,
            severity: 8,
            occurrence: 4,
            detection: 5,
        };

        assert_eq!(entry.rpn(), 160); // 8 * 4 * 5
    }

    #[test]
    fn test_glob_pattern_matching() {
        // Test wildcard *
        assert!(matches_glob_pattern("motor_cmd", "motor_*"));
        assert!(matches_glob_pattern("motor_status", "motor_*"));
        assert!(matches_glob_pattern("motor_", "motor_*"));
        assert!(!matches_glob_pattern("other_cmd", "motor_*"));

        // Test wildcard ?
        assert!(matches_glob_pattern("data_a", "data_?"));
        assert!(matches_glob_pattern("data_x", "data_?"));
        assert!(!matches_glob_pattern("data_ab", "data_?"));

        // Test exact match
        assert!(matches_glob_pattern("test.signal", "test.signal"));
        assert!(!matches_glob_pattern("test_signal", "test.signal"));

        // Test combined patterns
        assert!(matches_glob_pattern("reg_01_data", "reg_*_data"));
        assert!(!matches_glob_pattern("reg_01_status", "reg_*_data"));
    }
}
