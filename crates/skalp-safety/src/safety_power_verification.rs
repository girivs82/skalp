//! Safety-Power Domain Verification
//!
//! Verifies correct interaction between safety mechanisms and power domains:
//! - Detection signals with `requires_always_on` must be in always-on domains
//! - Safety signals crossing power domains must have proper isolation
//! - Power domain fault coverage for safety-critical functions
//!
//! # ISO 26262 Context
//!
//! Per ISO 26262-5, safety mechanisms must be available when needed. For mechanisms
//! that must survive power domain failures (e.g., watchdogs, voltage monitors),
//! they should be placed in always-on domains.
//!
//! The `requires_always_on` attribute is opt-in because not all detection signals
//! need to be always-on - only those that must detect faults across power states.

use serde::{Deserialize, Serialize};
use skalp_lir::lir::{Lir, NetId, PrimitiveId, PrimitiveType};
use std::collections::HashMap;

// ============================================================================
// Power Domain Info (Type-Safe Tracking)
// ============================================================================

/// Resolved power domain information with type-safe ID
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResolvedPowerDomain {
    /// Unique ID for this domain
    pub id: PowerDomainRef,
    /// Domain name (e.g., "vdd_core", "always_on")
    pub name: String,
    /// Nominal voltage in millivolts
    pub voltage_mv: u16,
    /// Is this an always-on domain?
    pub is_always_on: bool,
    /// Is this domain switchable/power-gated?
    pub is_switchable: bool,
    /// Has retention capability?
    pub has_retention: bool,
}

/// Type-safe reference to a power domain
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct PowerDomainRef(pub u32);

impl Default for ResolvedPowerDomain {
    fn default() -> Self {
        Self {
            id: PowerDomainRef(0),
            name: "vdd_default".to_string(),
            voltage_mv: 1000,
            is_always_on: false,
            is_switchable: false,
            has_retention: false,
        }
    }
}

// ============================================================================
// Verification Results
// ============================================================================

/// Result of safety-power domain verification
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct SafetyPowerVerificationResult {
    /// Violations found during verification
    pub violations: Vec<SafetyPowerViolation>,
    /// Warnings (non-blocking issues)
    pub warnings: Vec<SafetyPowerWarning>,
    /// Statistics about what was verified
    pub stats: VerificationStats,
    /// Power domain map built during analysis
    pub domain_map: HashMap<String, ResolvedPowerDomain>,
}

impl SafetyPowerVerificationResult {
    /// Check if verification passed (no errors)
    pub fn passed(&self) -> bool {
        self.violations.is_empty()
    }

    /// Count of errors
    pub fn error_count(&self) -> usize {
        self.violations.len()
    }

    /// Count of warnings
    pub fn warning_count(&self) -> usize {
        self.warnings.len()
    }
}

/// A violation that must be fixed
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SafetyPowerViolation {
    /// Type of violation
    pub violation_type: ViolationType,
    /// Signal/net name involved
    pub signal_name: String,
    /// Net ID if applicable
    pub net_id: Option<NetId>,
    /// Power domain of the signal
    pub signal_domain: String,
    /// Expected domain (if applicable)
    pub expected_domain: Option<String>,
    /// Human-readable description
    pub description: String,
    /// Suggested fix
    pub suggestion: String,
}

/// Types of safety-power violations
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum ViolationType {
    /// Detection signal with requires_always_on is in switchable domain
    DetectionNotInAlwaysOn,
    /// Safety signal crosses power domain without isolation
    MissingIsolation,
    /// Safety signal crosses voltage domain without level shifter
    MissingLevelShifter,
    /// Redundant safety mechanisms in same power domain (CCF risk)
    RedundancyInSameDomain,
}

/// A warning (non-blocking but should be reviewed)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SafetyPowerWarning {
    /// Type of warning
    pub warning_type: WarningType,
    /// Signal/net name involved
    pub signal_name: String,
    /// Human-readable description
    pub description: String,
}

/// Types of warnings
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum WarningType {
    /// Detection signal without requires_always_on in switchable domain
    DetectionInSwitchableDomain,
    /// Safety signal crossing domain with isolation (info only)
    SafetyCrossingWithIsolation,
    /// Unknown power domain (using defaults)
    UnknownPowerDomain,
}

/// Statistics from verification
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct VerificationStats {
    /// Number of detection signals analyzed
    pub detection_signals_count: usize,
    /// Number of detection signals requiring always-on
    pub requires_always_on_count: usize,
    /// Number of safety signal domain crossings
    pub safety_crossings_count: usize,
    /// Number of power domains found
    pub power_domains_count: usize,
    /// Number of isolation cells found
    pub isolation_cells_count: usize,
    /// Number of level shifters found
    pub level_shifters_count: usize,
}

// ============================================================================
// Main Verification Function
// ============================================================================

/// Verify safety-power domain requirements
///
/// This function performs three main checks:
/// 1. Detection signals with `requires_always_on = true` are in always-on domains
/// 2. Safety signals crossing power domains have proper isolation
/// 3. No CCF risk from redundant mechanisms in same domain
///
/// # Example
/// ```ignore
/// use skalp_safety::safety_power_verification::verify_safety_power_domains;
///
/// let lir = compile_to_lir(source);
/// let result = verify_safety_power_domains(&lir);
///
/// if !result.passed() {
///     for violation in &result.violations {
///         eprintln!("ERROR: {}", violation.description);
///     }
/// }
/// ```
pub fn verify_safety_power_domains(lir: &Lir) -> SafetyPowerVerificationResult {
    let mut result = SafetyPowerVerificationResult::default();

    // Build power domain map from primitives
    let domain_map = build_power_domain_map(lir);
    result.stats.power_domains_count = domain_map.len();
    result.domain_map = domain_map.clone();

    // Build primitive-to-domain mapping
    let prim_domain_map = build_primitive_domain_map(lir, &domain_map);

    // Count isolation cells and level shifters
    for prim in &lir.primitives {
        match &prim.ptype {
            PrimitiveType::IsolationCell { .. } => result.stats.isolation_cells_count += 1,
            PrimitiveType::LevelShifter { .. } => result.stats.level_shifters_count += 1,
            _ => {}
        }
    }

    // Check 1: Detection signals with requires_always_on
    verify_detection_always_on(lir, &prim_domain_map, &domain_map, &mut result);

    // Check 2: Safety signal domain crossings need isolation
    verify_safety_signal_isolation(lir, &prim_domain_map, &domain_map, &mut result);

    // Check 3: Redundant mechanisms in same domain (CCF risk)
    verify_redundancy_separation(lir, &prim_domain_map, &mut result);

    result
}

// ============================================================================
// Power Domain Map Building
// ============================================================================

/// Build a map of power domain names to resolved domain info
fn build_power_domain_map(lir: &Lir) -> HashMap<String, ResolvedPowerDomain> {
    let mut domains = HashMap::new();
    let mut next_id = 0u32;

    // Collect unique domain names from primitives
    for prim in &lir.primitives {
        if let Some(domain_name) = &prim.power_domain {
            if !domains.contains_key(domain_name) {
                let resolved = resolve_domain_from_name(domain_name, PowerDomainRef(next_id));
                domains.insert(domain_name.clone(), resolved);
                next_id += 1;
            }
        }
    }

    // Always add a default domain
    if !domains.contains_key("vdd_default") {
        domains.insert(
            "vdd_default".to_string(),
            ResolvedPowerDomain {
                id: PowerDomainRef(next_id),
                name: "vdd_default".to_string(),
                voltage_mv: 1000,
                is_always_on: false,
                is_switchable: false,
                has_retention: false,
            },
        );
    }

    domains
}

/// Resolve domain properties from name (using naming conventions)
fn resolve_domain_from_name(name: &str, id: PowerDomainRef) -> ResolvedPowerDomain {
    let name_lower = name.to_lowercase();

    // Determine if always-on from naming conventions
    let is_always_on = name_lower.contains("always_on")
        || name_lower.contains("aon")
        || name_lower.contains("vdd_aon")
        || name_lower == "always_on";

    // Determine if switchable/power-gated
    let is_switchable = name_lower.contains("_pg")
        || name_lower.contains("switchable")
        || name_lower.contains("gated");

    // Determine if retention domain
    let has_retention = name_lower.contains("ret") || name_lower.contains("retention");

    // Infer voltage from name
    let voltage_mv = infer_voltage_from_name(&name_lower);

    ResolvedPowerDomain {
        id,
        name: name.to_string(),
        voltage_mv,
        is_always_on,
        is_switchable,
        has_retention,
    }
}

/// Infer voltage from domain name
fn infer_voltage_from_name(name: &str) -> u16 {
    // Check for explicit voltage in name
    if name.contains("0v9") || name.contains("0p9") {
        return 900;
    }
    if name.contains("1v0") || name.contains("1p0") {
        return 1000;
    }
    if name.contains("1v2") || name.contains("1p2") {
        return 1200;
    }
    if name.contains("1v8") || name.contains("1p8") {
        return 1800;
    }
    if name.contains("2v5") || name.contains("2p5") {
        return 2500;
    }
    if name.contains("3v3") || name.contains("3p3") {
        return 3300;
    }
    if name.contains("5v") || name.contains("5p0") {
        return 5000;
    }

    // Domain type heuristics
    if name.contains("core") {
        return 1000;
    }
    if name.contains("io") {
        return 3300;
    }
    if name.contains("analog") {
        return 5000;
    }
    if name.contains("mem") || name.contains("sram") {
        return 1100;
    }

    // Default
    1000
}

/// Build a map of primitive ID to its power domain
fn build_primitive_domain_map(
    lir: &Lir,
    domain_map: &HashMap<String, ResolvedPowerDomain>,
) -> HashMap<PrimitiveId, ResolvedPowerDomain> {
    let mut prim_domains = HashMap::new();

    let default_domain = domain_map.get("vdd_default").cloned().unwrap_or_default();

    for prim in &lir.primitives {
        let domain = if let Some(domain_name) = &prim.power_domain {
            domain_map
                .get(domain_name)
                .cloned()
                .unwrap_or_else(|| default_domain.clone())
        } else {
            default_domain.clone()
        };
        prim_domains.insert(prim.id, domain);
    }

    prim_domains
}

// ============================================================================
// Verification Check 1: Detection Signals in Always-On Domains
// ============================================================================

/// Verify detection signals with requires_always_on are in always-on domains
fn verify_detection_always_on(
    lir: &Lir,
    prim_domain_map: &HashMap<PrimitiveId, ResolvedPowerDomain>,
    domain_map: &HashMap<String, ResolvedPowerDomain>,
    result: &mut SafetyPowerVerificationResult,
) {
    for net in &lir.nets {
        // Check if this is a detection signal
        if !net.is_detection {
            continue;
        }
        result.stats.detection_signals_count += 1;

        // Check if requires_always_on is set
        let requires_always_on = net
            .detection_config
            .as_ref()
            .map(|c| c.requires_always_on)
            .unwrap_or(false);

        if requires_always_on {
            result.stats.requires_always_on_count += 1;
        }

        // Get the domain of the net's driver
        let net_domain = if let Some((driver_id, _)) = net.driver {
            prim_domain_map.get(&driver_id).cloned()
        } else {
            // Primary input - check if there's a domain annotation
            // For now, assume primary inputs without domain are in default domain
            domain_map.get("vdd_default").cloned()
        };

        let domain = net_domain.unwrap_or_default();

        if requires_always_on {
            // ERROR: requires_always_on but not in always-on domain
            if !domain.is_always_on {
                result.violations.push(SafetyPowerViolation {
                    violation_type: ViolationType::DetectionNotInAlwaysOn,
                    signal_name: net.name.clone(),
                    net_id: Some(net.id),
                    signal_domain: domain.name.clone(),
                    expected_domain: Some("always_on".to_string()),
                    description: format!(
                        "Detection signal '{}' has requires_always_on=true but is in \
                         domain '{}' which is not always-on",
                        net.name, domain.name
                    ),
                    suggestion: format!(
                        "Move signal '{}' to an always-on power domain, or remove \
                         requires_always_on if the signal doesn't need to survive \
                         power domain failures",
                        net.name
                    ),
                });
            }
        } else if domain.is_switchable && !requires_always_on {
            // WARNING: detection signal in switchable domain without explicit opt-in
            result.warnings.push(SafetyPowerWarning {
                warning_type: WarningType::DetectionInSwitchableDomain,
                signal_name: net.name.clone(),
                description: format!(
                    "Detection signal '{}' is in switchable domain '{}'. If this signal \
                     must detect faults when the domain is powered down, add \
                     requires_always_on=true to the attribute",
                    net.name, domain.name
                ),
            });
        }
    }
}

// ============================================================================
// Verification Check 2: Safety Signal Isolation at Domain Crossings
// ============================================================================

/// Verify safety signals crossing power domains have proper isolation
fn verify_safety_signal_isolation(
    lir: &Lir,
    prim_domain_map: &HashMap<PrimitiveId, ResolvedPowerDomain>,
    domain_map: &HashMap<String, ResolvedPowerDomain>,
    result: &mut SafetyPowerVerificationResult,
) {
    // Build set of nets that are isolated (driven through isolation cells)
    let mut isolated_nets: HashMap<NetId, PrimitiveId> = HashMap::new();
    let mut level_shifted_nets: HashMap<NetId, PrimitiveId> = HashMap::new();

    for prim in &lir.primitives {
        match &prim.ptype {
            PrimitiveType::IsolationCell { .. } => {
                // Output is first in outputs vec
                if let Some(&output) = prim.outputs.first() {
                    isolated_nets.insert(output, prim.id);
                }
            }
            PrimitiveType::LevelShifter { .. } => {
                // Output is first in outputs vec
                if let Some(&output) = prim.outputs.first() {
                    level_shifted_nets.insert(output, prim.id);
                }
            }
            _ => {}
        }
    }

    // Check each detection signal for domain crossings
    for net in &lir.nets {
        if !net.is_detection {
            continue;
        }

        // Get source domain (driver)
        let source_domain = if let Some((driver_id, _)) = net.driver {
            prim_domain_map.get(&driver_id).cloned()
        } else {
            domain_map.get("vdd_default").cloned()
        };
        let source = source_domain.unwrap_or_default();

        // Check each load for domain crossing
        for (load_id, _) in &net.loads {
            let dest_domain = prim_domain_map.get(load_id).cloned().unwrap_or_default();

            // Skip if same domain
            if source.name == dest_domain.name {
                continue;
            }

            result.stats.safety_crossings_count += 1;

            // Check if crossing from switchable to always-on needs isolation
            if source.is_switchable && dest_domain.is_always_on {
                // Need isolation cell
                if !isolated_nets.contains_key(&net.id) {
                    result.violations.push(SafetyPowerViolation {
                        violation_type: ViolationType::MissingIsolation,
                        signal_name: net.name.clone(),
                        net_id: Some(net.id),
                        signal_domain: source.name.clone(),
                        expected_domain: Some(dest_domain.name.clone()),
                        description: format!(
                            "Safety signal '{}' crosses from switchable domain '{}' to \
                             always-on domain '{}' without isolation cell. When '{}' \
                             powers down, the signal will float causing undefined behavior",
                            net.name, source.name, dest_domain.name, source.name
                        ),
                        suggestion: format!(
                            "Add isolation cell for '{}' with appropriate clamp value, or \
                             annotate with #[isolation(clamp = low)]",
                            net.name
                        ),
                    });
                } else {
                    result.warnings.push(SafetyPowerWarning {
                        warning_type: WarningType::SafetyCrossingWithIsolation,
                        signal_name: net.name.clone(),
                        description: format!(
                            "Safety signal '{}' crosses from '{}' to '{}' with isolation (OK)",
                            net.name, source.name, dest_domain.name
                        ),
                    });
                }
            }

            // Check if voltage domain crossing needs level shifter
            if source.voltage_mv != dest_domain.voltage_mv
                && !level_shifted_nets.contains_key(&net.id)
            {
                result.violations.push(SafetyPowerViolation {
                    violation_type: ViolationType::MissingLevelShifter,
                    signal_name: net.name.clone(),
                    net_id: Some(net.id),
                    signal_domain: source.name.clone(),
                    expected_domain: Some(dest_domain.name.clone()),
                    description: format!(
                        "Safety signal '{}' crosses voltage domains ({}mV → {}mV) \
                             without level shifter. This can cause signal integrity issues \
                             and potential safety mechanism failures",
                        net.name, source.voltage_mv, dest_domain.voltage_mv
                    ),
                    suggestion: format!(
                        "Add level shifter for '{}' or annotate with #[level_shift]",
                        net.name
                    ),
                });
            }
        }
    }
}

// ============================================================================
// Verification Check 3: Redundancy Separation (CCF Risk)
// ============================================================================

/// Verify redundant safety mechanisms are in separate domains (CCF risk)
fn verify_redundancy_separation(
    lir: &Lir,
    prim_domain_map: &HashMap<PrimitiveId, ResolvedPowerDomain>,
    result: &mut SafetyPowerVerificationResult,
) {
    // Group detection signals by domain
    let mut detection_by_domain: HashMap<String, Vec<String>> = HashMap::new();

    for net in &lir.nets {
        if !net.is_detection {
            continue;
        }

        let domain = if let Some((driver_id, _)) = net.driver {
            prim_domain_map
                .get(&driver_id)
                .map(|d| d.name.clone())
                .unwrap_or_else(|| "vdd_default".to_string())
        } else {
            "vdd_default".to_string()
        };

        detection_by_domain
            .entry(domain)
            .or_default()
            .push(net.name.clone());
    }

    // Warn if multiple detection signals are in the same switchable domain
    for (domain_name, signals) in &detection_by_domain {
        if signals.len() > 1 {
            // Check if domain is switchable (CCF risk)
            let is_switchable = domain_name.contains("_pg")
                || domain_name.contains("switchable")
                || (!domain_name.contains("always_on") && !domain_name.contains("aon"));

            if is_switchable {
                result.violations.push(SafetyPowerViolation {
                    violation_type: ViolationType::RedundancyInSameDomain,
                    signal_name: signals.join(", "),
                    net_id: None,
                    signal_domain: domain_name.clone(),
                    expected_domain: None,
                    description: format!(
                        "Multiple detection signals ({}) are in the same power domain '{}'. \
                         A power failure in this domain would disable all these mechanisms \
                         simultaneously (Common Cause Failure risk per ISO 26262-9)",
                        signals.join(", "),
                        domain_name
                    ),
                    suggestion:
                        "Consider placing redundant detection mechanisms in separate power \
                         domains or in an always-on domain to improve independence"
                            .to_string(),
                });
            }
        }
    }
}

// ============================================================================
// Report Generation
// ============================================================================

/// Format verification results as a human-readable report
pub fn format_verification_report(result: &SafetyPowerVerificationResult) -> String {
    let mut report = String::new();

    report.push_str("## Safety-Power Domain Verification Report\n\n");

    // Summary
    let status = if result.passed() { "PASSED" } else { "FAILED" };
    report.push_str(&format!("**Status**: {}\n\n", status));

    report.push_str("### Statistics\n\n");
    report.push_str("| Metric | Count |\n|--------|-------|\n");
    report.push_str(&format!(
        "| Detection signals | {} |\n",
        result.stats.detection_signals_count
    ));
    report.push_str(&format!(
        "| Requires always-on | {} |\n",
        result.stats.requires_always_on_count
    ));
    report.push_str(&format!(
        "| Safety domain crossings | {} |\n",
        result.stats.safety_crossings_count
    ));
    report.push_str(&format!(
        "| Power domains | {} |\n",
        result.stats.power_domains_count
    ));
    report.push_str(&format!(
        "| Isolation cells | {} |\n",
        result.stats.isolation_cells_count
    ));
    report.push_str(&format!(
        "| Level shifters | {} |\n\n",
        result.stats.level_shifters_count
    ));

    // Violations
    if !result.violations.is_empty() {
        report.push_str("### Violations\n\n");
        for (i, v) in result.violations.iter().enumerate() {
            report.push_str(&format!(
                "{}. **{:?}**: {}\n",
                i + 1,
                v.violation_type,
                v.signal_name
            ));
            report.push_str(&format!("   - Domain: {}\n", v.signal_domain));
            report.push_str(&format!("   - Issue: {}\n", v.description));
            report.push_str(&format!("   - Fix: {}\n\n", v.suggestion));
        }
    }

    // Warnings
    if !result.warnings.is_empty() {
        report.push_str("### Warnings\n\n");
        for w in &result.warnings {
            report.push_str(&format!("- **{}**: {}\n", w.signal_name, w.description));
        }
        report.push('\n');
    }

    // Power domains found
    if !result.domain_map.is_empty() {
        report.push_str("### Power Domains\n\n");
        report.push_str("| Domain | Voltage | Always-On | Switchable | Retention |\n");
        report.push_str("|--------|---------|-----------|------------|----------|\n");
        for domain in result.domain_map.values() {
            report.push_str(&format!(
                "| {} | {}mV | {} | {} | {} |\n",
                domain.name,
                domain.voltage_mv,
                if domain.is_always_on { "Yes" } else { "No" },
                if domain.is_switchable { "Yes" } else { "No" },
                if domain.has_retention { "Yes" } else { "No" },
            ));
        }
    }

    report
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_resolve_domain_always_on() {
        let domain = resolve_domain_from_name("vdd_always_on", PowerDomainRef(0));
        assert!(domain.is_always_on);
        assert!(!domain.is_switchable);
    }

    #[test]
    fn test_resolve_domain_switchable() {
        let domain = resolve_domain_from_name("vdd_core_pg", PowerDomainRef(0));
        assert!(!domain.is_always_on);
        assert!(domain.is_switchable);
    }

    #[test]
    fn test_resolve_domain_voltage() {
        assert_eq!(
            resolve_domain_from_name("vdd_1v8", PowerDomainRef(0)).voltage_mv,
            1800
        );
        assert_eq!(
            resolve_domain_from_name("vdd_3v3", PowerDomainRef(0)).voltage_mv,
            3300
        );
        assert_eq!(
            resolve_domain_from_name("vdd_io", PowerDomainRef(0)).voltage_mv,
            3300
        );
        assert_eq!(
            resolve_domain_from_name("vdd_core", PowerDomainRef(0)).voltage_mv,
            1000
        );
    }

    #[test]
    fn test_empty_lir_verification() {
        let lir = Lir::new("test".to_string());
        let result = verify_safety_power_domains(&lir);
        assert!(result.passed());
        assert_eq!(result.stats.detection_signals_count, 0);
    }

    #[test]
    fn test_infer_voltage() {
        assert_eq!(infer_voltage_from_name("vdd_0v9"), 900);
        assert_eq!(infer_voltage_from_name("vdd_1v0"), 1000);
        assert_eq!(infer_voltage_from_name("vdd_1v8"), 1800);
        assert_eq!(infer_voltage_from_name("vdd_3v3"), 3300);
        assert_eq!(infer_voltage_from_name("vdd_5v"), 5000);
    }
}
