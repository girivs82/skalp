//! Trait Expansion System for ISO 26262 Safety Composition
//!
//! Implements parameterized safety traits that enable DRY reuse of safety
//! requirements, FMEA patterns, and HSI specifications. The trait system
//! supports:
//!
//! - Parameter substitution (e.g., `use PecProtection<99.5>`)
//! - Member overrides (e.g., `use Trait { psm X { dc: 99.6 } }`)
//! - Additions and removals (`+ hsr NEW { ... }`, `- hsr HSR_PEC`)
//! - Wildcard pattern matching for FMEA traits (`component_pattern *::sda`)
//! - Conflict detection for duplicate definitions

use crate::hierarchy::{
    Addition, DesignPattern, DesignRef, DiagnosticHardwareSafetyRequirement, FailureMode,
    FailureModeBinding, FailureModeGroup, FmeaComponent, FmeaComponentPattern, FmeaTrait,
    HardwareSafetyRequirement, HardwareSoftwareInterface, HsiTimingPattern, HsiTrait,
    LatentSafetyMechanism, MemberOverride, ParamType, ParamValue, PrimarySafetyMechanism,
    SafetyEntity, SafetyGoal, SafetyHierarchy, SafetyTrait, TraitExpansionError, TraitParameter,
    TraitUsage,
};
use indexmap::IndexMap;
use std::collections::HashSet;
use std::time::Duration;

// ============================================================================
// Trait Expansion Engine
// ============================================================================

/// Engine for expanding trait usages into concrete safety members
pub struct TraitExpander {
    /// Named parameter bindings during expansion
    param_bindings: IndexMap<String, ParamValue>,
}

impl TraitExpander {
    /// Create a new trait expander
    pub fn new() -> Self {
        Self {
            param_bindings: IndexMap::new(),
        }
    }

    /// Expand a safety trait usage into HSRs and LSMs
    pub fn expand_safety_trait(
        &mut self,
        trait_def: &SafetyTrait,
        usage: &TraitUsage,
    ) -> Result<ExpandedSafetyTrait, TraitExpansionError> {
        // Bind parameters
        self.bind_parameters(&trait_def.parameters, &usage.param_overrides)?;

        // Clone and substitute HSRs
        let mut hsrs: Vec<HardwareSafetyRequirement> = trait_def
            .hsrs
            .iter()
            .map(|hsr| self.substitute_hsr(hsr))
            .collect();

        // Clone and substitute LSMs
        let mut lsms: Vec<LatentSafetyMechanism> = trait_def
            .lsms
            .iter()
            .map(|lsm| self.substitute_lsm(lsm))
            .collect();

        // Apply member overrides
        for override_spec in &usage.member_overrides {
            self.apply_member_override(&mut hsrs, &mut lsms, override_spec);
        }

        // Apply additions
        for addition in &usage.additions {
            match addition {
                Addition::Hsr(hsr) => hsrs.push(hsr.clone()),
                Addition::Lsm(lsm) => lsms.push(lsm.clone()),
            }
        }

        // Apply removals
        for removal in &usage.removals {
            hsrs.retain(|hsr| hsr.id != *removal);
            lsms.retain(|lsm| lsm.name != *removal);
        }

        Ok(ExpandedSafetyTrait { hsrs, lsms })
    }

    /// Expand an FMEA trait into concrete component patterns
    pub fn expand_fmea_trait(
        &self,
        trait_def: &FmeaTrait,
        covered_refs: &[DesignRef],
    ) -> Vec<FmeaComponent> {
        let mut components = Vec::new();

        for pattern in &trait_def.component_patterns {
            // Match against covered design references
            for design_ref in covered_refs {
                if self.matches_fmea_pattern(&pattern.pattern, design_ref) {
                    let component = self.create_fmea_component(pattern, design_ref.clone());
                    components.push(component);
                }
            }
        }

        components
    }

    /// Expand an HSI trait into concrete interface patterns
    pub fn expand_hsi_trait(
        &self,
        trait_def: &HsiTrait,
        covered_refs: &[DesignRef],
    ) -> HardwareSoftwareInterface {
        let mut hsi = HardwareSoftwareInterface::default();

        // Add signal patterns as includes
        for pattern in &trait_def.signal_patterns {
            hsi.includes.push(DesignPattern::signals(
                "*", // Match any instance
                pattern,
            ));
        }

        // Add timing constraints
        for timing_pattern in &trait_def.timing_patterns {
            hsi.timing.signal_latencies.insert(
                timing_pattern.signal_pattern.clone(),
                timing_pattern.max_latency,
            );
        }

        hsi
    }

    /// Bind parameters from usage to trait definition
    fn bind_parameters(
        &mut self,
        params: &[TraitParameter],
        overrides: &[ParamValue],
    ) -> Result<(), TraitExpansionError> {
        self.param_bindings.clear();

        for (i, param) in params.iter().enumerate() {
            let value = if i < overrides.len() {
                // Use provided override
                overrides[i].clone()
            } else if let Some(ref default) = param.default {
                // Use default value
                default.clone()
            } else {
                return Err(TraitExpansionError::InvalidParameter(format!(
                    "No value provided for parameter '{}' and no default",
                    param.name
                )));
            };

            // Type check
            if !self.type_matches(&value, &param.param_type) {
                return Err(TraitExpansionError::InvalidParameter(format!(
                    "Type mismatch for parameter '{}': expected {:?}",
                    param.name, param.param_type
                )));
            }

            self.param_bindings.insert(param.name.clone(), value);
        }

        Ok(())
    }

    /// Check if value matches expected type
    fn type_matches(&self, value: &ParamValue, expected: &ParamType) -> bool {
        matches!(
            (value, expected),
            (ParamValue::Float(_), ParamType::Float)
                | (ParamValue::Duration(_), ParamType::Duration)
                | (ParamValue::Integer(_), ParamType::Integer)
                | (ParamValue::String(_), ParamType::String)
        )
    }

    /// Substitute parameters in an HSR
    fn substitute_hsr(&self, hsr: &HardwareSafetyRequirement) -> HardwareSafetyRequirement {
        let mut result = hsr.clone();

        // Substitute in PSM if present
        if let Some(ref psm) = hsr.psm {
            result.psm = Some(self.substitute_psm(psm));
        }

        result
    }

    /// Substitute parameters in a PSM
    fn substitute_psm(&self, psm: &PrimarySafetyMechanism) -> PrimarySafetyMechanism {
        let mut result = psm.clone();

        // Substitute DC target if it references a parameter
        // Look for parameter named like "DC" or the PSM name + "_dc"
        let dc_param_name = format!("{}_dc", psm.name.to_lowercase());
        if let Some(ParamValue::Float(dc)) = self.param_bindings.get("DC") {
            result.dc_target = *dc;
        } else if let Some(ParamValue::Float(dc)) = self.param_bindings.get(&dc_param_name) {
            result.dc_target = *dc;
        }

        result
    }

    /// Substitute parameters in an LSM
    fn substitute_lsm(&self, lsm: &LatentSafetyMechanism) -> LatentSafetyMechanism {
        let mut result = lsm.clone();

        // Substitute LC target if it references a parameter
        let lc_param_name = format!("{}_lc", lsm.name.to_lowercase());
        if let Some(ParamValue::Float(lc)) = self.param_bindings.get("LC") {
            result.lc_target = *lc;
        } else if let Some(ParamValue::Float(lc)) = self.param_bindings.get(&lc_param_name) {
            result.lc_target = *lc;
        }

        // Substitute interval
        let interval_param_name = format!("{}_interval", lsm.name.to_lowercase());
        if let Some(ParamValue::Duration(interval)) = self.param_bindings.get("INTERVAL") {
            result.interval = Some(*interval);
        } else if let Some(ParamValue::Duration(interval)) =
            self.param_bindings.get(&interval_param_name)
        {
            result.interval = Some(*interval);
        }

        result
    }

    /// Apply a member override to HSRs and LSMs
    fn apply_member_override(
        &self,
        hsrs: &mut [HardwareSafetyRequirement],
        lsms: &mut [LatentSafetyMechanism],
        override_spec: &MemberOverride,
    ) {
        match override_spec {
            MemberOverride::Psm { name, dc } => {
                for hsr in hsrs.iter_mut() {
                    if let Some(ref mut psm) = hsr.psm {
                        if psm.name == *name {
                            psm.dc_target = *dc;
                        }
                    }
                }
            }
            MemberOverride::Lsm { name, lc } => {
                for lsm in lsms.iter_mut() {
                    if lsm.name == *name {
                        lsm.lc_target = *lc;
                    }
                }
            }
            MemberOverride::FailureMode {
                component: _,
                mode: _,
                coverage: _,
            } => {
                // Failure mode overrides are applied during FMEA expansion
            }
        }
    }

    /// Check if a design reference matches an FMEA pattern
    fn matches_fmea_pattern(&self, pattern: &str, design_ref: &DesignRef) -> bool {
        // Patterns can be:
        // - "*::sda" - any instance with sda signal
        // - "top.i2c_*::sda" - instances matching glob with sda signal
        // - "top.i2c_0" - specific instance

        let parts: Vec<&str> = pattern.split("::").collect();
        let instance_pattern = parts[0];
        let signal_pattern = parts.get(1).copied();

        // Check instance match
        if !self.matches_glob(instance_pattern, &design_ref.instance.to_string()) {
            return false;
        }

        // Check signal match if pattern specifies signal
        if let Some(sig_pat) = signal_pattern {
            if let Some(ref signal) = design_ref.signal {
                return self.matches_glob(sig_pat, signal);
            }
            return false; // Pattern expects signal but ref has none
        }

        true
    }

    /// Simple glob matching (* as wildcard)
    fn matches_glob(&self, pattern: &str, text: &str) -> bool {
        if pattern == "*" {
            return true;
        }

        if pattern.contains('*') {
            // Support patterns like "i2c_*" or "*_sda"
            if let Some(suffix) = pattern.strip_prefix('*') {
                return text.ends_with(suffix);
            }
            if let Some(prefix) = pattern.strip_suffix('*') {
                return text.starts_with(prefix);
            }
            // Pattern with * in middle: "prefix*suffix"
            if let Some(star_pos) = pattern.find('*') {
                let prefix = &pattern[..star_pos];
                let suffix = &pattern[star_pos + 1..];
                return text.starts_with(prefix) && text.ends_with(suffix);
            }
        }

        pattern == text
    }

    /// Create an FMEA component from a pattern and design reference
    fn create_fmea_component(
        &self,
        pattern: &FmeaComponentPattern,
        design_ref: DesignRef,
    ) -> FmeaComponent {
        let mut component =
            FmeaComponent::new(design_ref, pattern.library.clone(), pattern.part.clone());

        // Add failure modes grouped by detector
        for group in &pattern.failure_mode_groups {
            match &group.detector {
                crate::hierarchy::DetectorRef::Psm(psm_name) => {
                    for mode in &group.modes {
                        component.add_psm_failure_mode(psm_name, mode.clone());
                    }
                }
                crate::hierarchy::DetectorRef::Lsm(lsm_name) => {
                    for mode in &group.modes {
                        component.add_lsm_failure_mode(lsm_name, mode.clone());
                    }
                }
                crate::hierarchy::DetectorRef::Safe => {
                    for mode in &group.modes {
                        component.add_safe_mode(mode.clone());
                    }
                }
            }
        }

        component
    }
}

impl Default for TraitExpander {
    fn default() -> Self {
        Self::new()
    }
}

/// Result of expanding a safety trait
pub struct ExpandedSafetyTrait {
    /// Expanded HSRs
    pub hsrs: Vec<HardwareSafetyRequirement>,
    /// Expanded LSMs
    pub lsms: Vec<LatentSafetyMechanism>,
}

// ============================================================================
// Conflict Detection
// ============================================================================

/// Conflict detector for safety definitions
pub struct ConflictDetector;

impl ConflictDetector {
    /// Check for conflicts when merging trait members into a goal
    pub fn check_goal_conflicts(
        goal: &SafetyGoal,
        new_hsrs: &[HardwareSafetyRequirement],
        new_lsms: &[LatentSafetyMechanism],
    ) -> Vec<ConflictError> {
        let mut errors = Vec::new();

        // Check for duplicate HSR IDs
        let existing_hsr_ids: HashSet<_> = goal.hsrs.iter().map(|h| &h.id).collect();
        for hsr in new_hsrs {
            if existing_hsr_ids.contains(&hsr.id) {
                errors.push(ConflictError::DuplicateHsr(hsr.id.clone()));
            }
        }

        // Check for duplicate PSM names
        let existing_psm_names: HashSet<_> = goal
            .hsrs
            .iter()
            .filter_map(|h| h.psm.as_ref().map(|p| &p.name))
            .collect();
        for hsr in new_hsrs {
            if let Some(ref psm) = hsr.psm {
                if existing_psm_names.contains(&psm.name) {
                    errors.push(ConflictError::DuplicatePsm(psm.name.clone()));
                }
            }
        }

        // Check for duplicate LSM names
        let existing_lsm_names: HashSet<_> = goal.lsms.iter().map(|l| &l.name).collect();
        for lsm in new_lsms {
            if existing_lsm_names.contains(&lsm.name) {
                errors.push(ConflictError::DuplicateLsm(lsm.name.clone()));
            }
        }

        errors
    }

    /// Check for conflicts when merging trait members into an entity
    pub fn check_entity_conflicts(
        entity: &SafetyEntity,
        new_fmea: &[FmeaComponent],
    ) -> Vec<ConflictError> {
        let mut errors = Vec::new();

        // Check for duplicate FMEA components (same design ref)
        let existing_refs: HashSet<_> = entity
            .fmea
            .iter()
            .map(|c| c.design_ref.to_string())
            .collect();
        for component in new_fmea {
            let ref_str = component.design_ref.to_string();
            if existing_refs.contains(&ref_str) {
                errors.push(ConflictError::DuplicateFmeaComponent(ref_str));
            }
        }

        errors
    }

    /// Check for PSM definition conflicts with different DC values
    pub fn check_psm_dc_conflicts(hsrs: &[HardwareSafetyRequirement]) -> Vec<(String, f64, f64)> {
        let mut psm_dcs: IndexMap<String, f64> = IndexMap::new();
        let mut conflicts = Vec::new();

        for hsr in hsrs {
            if let Some(ref psm) = hsr.psm {
                if let Some(&existing_dc) = psm_dcs.get(&psm.name) {
                    if (existing_dc - psm.dc_target).abs() > 0.001 {
                        conflicts.push((psm.name.clone(), existing_dc, psm.dc_target));
                    }
                } else {
                    psm_dcs.insert(psm.name.clone(), psm.dc_target);
                }
            }
        }

        conflicts
    }
}

/// Conflict errors during trait expansion
#[derive(Debug, Clone)]
pub enum ConflictError {
    /// Duplicate HSR ID
    DuplicateHsr(String),
    /// Duplicate PSM name
    DuplicatePsm(String),
    /// Duplicate LSM name
    DuplicateLsm(String),
    /// Duplicate FMEA component
    DuplicateFmeaComponent(String),
    /// Conflicting PSM DC values
    ConflictingPsmDc { name: String, dc1: f64, dc2: f64 },
}

impl std::fmt::Display for ConflictError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ConflictError::DuplicateHsr(id) => write!(f, "duplicate HSR: {}", id),
            ConflictError::DuplicatePsm(name) => write!(f, "duplicate PSM: {}", name),
            ConflictError::DuplicateLsm(name) => write!(f, "duplicate LSM: {}", name),
            ConflictError::DuplicateFmeaComponent(ref_str) => {
                write!(f, "duplicate FMEA component: {}", ref_str)
            }
            ConflictError::ConflictingPsmDc { name, dc1, dc2 } => {
                write!(f, "conflicting DC for PSM {}: {} vs {}", name, dc1, dc2)
            }
        }
    }
}

// ============================================================================
// Hierarchy Integration
// ============================================================================

/// Expand all traits in a safety hierarchy
pub fn expand_all_traits(hierarchy: &mut SafetyHierarchy) -> Result<(), TraitExpansionError> {
    let mut expander = TraitExpander::new();

    // Clone trait definitions to avoid borrow conflicts
    let traits = hierarchy.traits.clone();
    let fmea_traits = hierarchy.fmea_traits.clone();
    let hsi_traits = hierarchy.hsi_traits.clone();

    // Expand traits in goals
    for goal in hierarchy.goals.values_mut() {
        let usages = std::mem::take(&mut goal.trait_usages);

        for usage in usages {
            if let Some(trait_def) = traits.get(&usage.trait_name) {
                let expanded = expander.expand_safety_trait(trait_def, &usage)?;

                // Check for conflicts
                let conflicts =
                    ConflictDetector::check_goal_conflicts(goal, &expanded.hsrs, &expanded.lsms);
                if !conflicts.is_empty() {
                    return Err(TraitExpansionError::ConflictingDefinition(
                        conflicts[0].to_string(),
                    ));
                }

                // Merge expanded members
                goal.hsrs.extend(expanded.hsrs);
                goal.lsms.extend(expanded.lsms);
            } else {
                return Err(TraitExpansionError::TraitNotFound(usage.trait_name.clone()));
            }
        }
    }

    // Expand traits in entities
    for entity in hierarchy.entities.values_mut() {
        let usages = std::mem::take(&mut entity.trait_usages);

        for usage in usages {
            // Expand FMEA traits
            if let Some(fmea_trait) = fmea_traits.get(&usage.trait_name) {
                // Collect covered design refs
                let covered_refs: Vec<DesignRef> =
                    entity.fmea.iter().map(|c| c.design_ref.clone()).collect();

                let components = expander.expand_fmea_trait(fmea_trait, &covered_refs);

                // Check for conflicts
                let conflicts = ConflictDetector::check_entity_conflicts(entity, &components);
                if !conflicts.is_empty() {
                    return Err(TraitExpansionError::ConflictingDefinition(
                        conflicts[0].to_string(),
                    ));
                }

                entity.fmea.extend(components);
            }

            // Expand HSI traits
            if let Some(hsi_trait) = hsi_traits.get(&usage.trait_name) {
                let covered_refs: Vec<DesignRef> =
                    entity.fmea.iter().map(|c| c.design_ref.clone()).collect();

                let expanded_hsi = expander.expand_hsi_trait(hsi_trait, &covered_refs);

                // Merge into entity's HSI
                entity.hsi.includes.extend(expanded_hsi.includes);
                entity.hsi.excludes.extend(expanded_hsi.excludes);
                entity
                    .hsi
                    .timing
                    .signal_latencies
                    .extend(expanded_hsi.timing.signal_latencies);
            }
        }
    }

    Ok(())
}

// ============================================================================
// Trait Builder Helpers
// ============================================================================

/// Builder for creating safety traits
pub struct SafetyTraitBuilder {
    name: String,
    parameters: Vec<TraitParameter>,
    hsrs: Vec<HardwareSafetyRequirement>,
    lsms: Vec<LatentSafetyMechanism>,
}

impl SafetyTraitBuilder {
    /// Create a new trait builder
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            parameters: Vec::new(),
            hsrs: Vec::new(),
            lsms: Vec::new(),
        }
    }

    /// Add a float parameter with default
    pub fn param_float(mut self, name: &str, default: f64) -> Self {
        self.parameters.push(TraitParameter {
            name: name.to_string(),
            param_type: ParamType::Float,
            default: Some(ParamValue::Float(default)),
        });
        self
    }

    /// Add a duration parameter with default
    pub fn param_duration(mut self, name: &str, default: Duration) -> Self {
        self.parameters.push(TraitParameter {
            name: name.to_string(),
            param_type: ParamType::Duration,
            default: Some(ParamValue::Duration(default)),
        });
        self
    }

    /// Add a float parameter without default (required)
    pub fn param_float_required(mut self, name: &str) -> Self {
        self.parameters.push(TraitParameter {
            name: name.to_string(),
            param_type: ParamType::Float,
            default: None,
        });
        self
    }

    /// Add an HSR
    pub fn hsr(mut self, hsr: HardwareSafetyRequirement) -> Self {
        self.hsrs.push(hsr);
        self
    }

    /// Add an LSM
    pub fn lsm(mut self, lsm: LatentSafetyMechanism) -> Self {
        self.lsms.push(lsm);
        self
    }

    /// Build the trait
    pub fn build(self) -> SafetyTrait {
        SafetyTrait {
            name: self.name,
            parameters: self.parameters,
            hsrs: self.hsrs,
            lsms: self.lsms,
        }
    }
}

/// Builder for trait usages
pub struct TraitUsageBuilder {
    trait_name: String,
    param_overrides: Vec<ParamValue>,
    member_overrides: Vec<MemberOverride>,
    additions: Vec<Addition>,
    removals: Vec<String>,
}

impl TraitUsageBuilder {
    /// Create a new usage builder
    pub fn new(trait_name: &str) -> Self {
        Self {
            trait_name: trait_name.to_string(),
            param_overrides: Vec::new(),
            member_overrides: Vec::new(),
            additions: Vec::new(),
            removals: Vec::new(),
        }
    }

    /// Override a float parameter
    pub fn override_float(mut self, value: f64) -> Self {
        self.param_overrides.push(ParamValue::Float(value));
        self
    }

    /// Override a duration parameter
    pub fn override_duration(mut self, value: Duration) -> Self {
        self.param_overrides.push(ParamValue::Duration(value));
        self
    }

    /// Override PSM diagnostic coverage
    pub fn override_psm_dc(mut self, psm_name: &str, dc: f64) -> Self {
        self.member_overrides.push(MemberOverride::Psm {
            name: psm_name.to_string(),
            dc,
        });
        self
    }

    /// Override LSM latent coverage
    pub fn override_lsm_lc(mut self, lsm_name: &str, lc: f64) -> Self {
        self.member_overrides.push(MemberOverride::Lsm {
            name: lsm_name.to_string(),
            lc,
        });
        self
    }

    /// Add an HSR
    pub fn add_hsr(mut self, hsr: HardwareSafetyRequirement) -> Self {
        self.additions.push(Addition::Hsr(hsr));
        self
    }

    /// Add an LSM
    pub fn add_lsm(mut self, lsm: LatentSafetyMechanism) -> Self {
        self.additions.push(Addition::Lsm(lsm));
        self
    }

    /// Remove a member by ID/name
    pub fn remove(mut self, name: &str) -> Self {
        self.removals.push(name.to_string());
        self
    }

    /// Build the usage
    pub fn build(self) -> TraitUsage {
        TraitUsage {
            trait_name: self.trait_name,
            param_overrides: self.param_overrides,
            member_overrides: self.member_overrides,
            additions: self.additions,
            removals: self.removals,
        }
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hierarchy::{DetectorRef, FailureClass, Severity};

    #[test]
    fn test_trait_expansion_basic() {
        let psm = PrimarySafetyMechanism::new("CommandIntegrity".to_string(), 99.0);
        let hsr = HardwareSafetyRequirement::new(
            "HSR_PEC".to_string(),
            "Commands protected with PEC".to_string(),
        )
        .with_psm(psm);

        let trait_def = SafetyTraitBuilder::new("PecProtection")
            .param_float("DC", 99.0)
            .hsr(hsr)
            .build();

        let usage = TraitUsageBuilder::new("PecProtection").build();

        let mut expander = TraitExpander::new();
        let expanded = expander.expand_safety_trait(&trait_def, &usage).unwrap();

        assert_eq!(expanded.hsrs.len(), 1);
        assert_eq!(expanded.hsrs[0].id, "HSR_PEC");
    }

    #[test]
    fn test_trait_expansion_with_override() {
        let psm = PrimarySafetyMechanism::new("CommandIntegrity".to_string(), 99.0);
        let hsr = HardwareSafetyRequirement::new(
            "HSR_PEC".to_string(),
            "Commands protected with PEC".to_string(),
        )
        .with_psm(psm);

        let trait_def = SafetyTraitBuilder::new("PecProtection")
            .param_float("DC", 99.0)
            .hsr(hsr)
            .build();

        let usage = TraitUsageBuilder::new("PecProtection")
            .override_psm_dc("CommandIntegrity", 99.5)
            .build();

        let mut expander = TraitExpander::new();
        let expanded = expander.expand_safety_trait(&trait_def, &usage).unwrap();

        assert_eq!(expanded.hsrs[0].psm.as_ref().unwrap().dc_target, 99.5);
    }

    #[test]
    fn test_trait_expansion_with_removal() {
        let hsr1 =
            HardwareSafetyRequirement::new("HSR_001".to_string(), "Requirement 1".to_string());
        let hsr2 =
            HardwareSafetyRequirement::new("HSR_002".to_string(), "Requirement 2".to_string());

        let trait_def = SafetyTraitBuilder::new("MultiRequirement")
            .hsr(hsr1)
            .hsr(hsr2)
            .build();

        let usage = TraitUsageBuilder::new("MultiRequirement")
            .remove("HSR_001")
            .build();

        let mut expander = TraitExpander::new();
        let expanded = expander.expand_safety_trait(&trait_def, &usage).unwrap();

        assert_eq!(expanded.hsrs.len(), 1);
        assert_eq!(expanded.hsrs[0].id, "HSR_002");
    }

    #[test]
    fn test_trait_expansion_with_addition() {
        let hsr1 =
            HardwareSafetyRequirement::new("HSR_001".to_string(), "Requirement 1".to_string());

        let trait_def = SafetyTraitBuilder::new("BaseRequirement").hsr(hsr1).build();

        let new_hsr =
            HardwareSafetyRequirement::new("HSR_NEW".to_string(), "New requirement".to_string());

        let usage = TraitUsageBuilder::new("BaseRequirement")
            .add_hsr(new_hsr)
            .build();

        let mut expander = TraitExpander::new();
        let expanded = expander.expand_safety_trait(&trait_def, &usage).unwrap();

        assert_eq!(expanded.hsrs.len(), 2);
    }

    #[test]
    fn test_glob_matching() {
        let expander = TraitExpander::new();

        assert!(expander.matches_glob("*", "anything"));
        assert!(expander.matches_glob("i2c_*", "i2c_0"));
        assert!(expander.matches_glob("i2c_*", "i2c_master"));
        assert!(!expander.matches_glob("i2c_*", "spi_0"));
        assert!(expander.matches_glob("*_sda", "i2c_sda"));
        assert!(expander.matches_glob("prefix*suffix", "prefixMIDDLEsuffix"));
    }

    #[test]
    fn test_fmea_pattern_matching() {
        let expander = TraitExpander::new();

        let design_ref = DesignRef::parse("top.i2c_0::sda");

        assert!(expander.matches_fmea_pattern("*::sda", &design_ref));
        assert!(expander.matches_fmea_pattern("top.i2c_*::sda", &design_ref));
        assert!(expander.matches_fmea_pattern("top.i2c_0::sda", &design_ref));
        assert!(!expander.matches_fmea_pattern("*::scl", &design_ref));
        assert!(!expander.matches_fmea_pattern("top.spi_*::sda", &design_ref));
    }

    #[test]
    fn test_conflict_detection() {
        use crate::asil::AsilLevel;
        use crate::hierarchy::SafetyGoalId;

        let psm = PrimarySafetyMechanism::new("Voting".to_string(), 99.0);
        let hsr = HardwareSafetyRequirement::new("HSR_001".to_string(), "Requirement".to_string())
            .with_psm(psm);

        let goal = SafetyGoal {
            id: SafetyGoalId(1),
            external_id: "SG-001".to_string(),
            name: "TestGoal".to_string(),
            description: "Test".to_string(),
            asil: AsilLevel::D,
            ftti: None,
            traces_to: Vec::new(),
            target_spfm: None,
            target_lfm: None,
            target_pmhf: None,
            hsrs: vec![hsr.clone()],
            lsms: Vec::new(),
            trait_usages: Vec::new(),
        };

        // Should detect duplicate HSR
        let conflicts = ConflictDetector::check_goal_conflicts(&goal, &[hsr], &[]);
        assert!(conflicts
            .iter()
            .any(|c| matches!(c, ConflictError::DuplicateHsr(_))));
    }

    #[test]
    fn test_parameter_missing_error() {
        let trait_def = SafetyTraitBuilder::new("RequiredParam")
            .param_float_required("DC")
            .build();

        let usage = TraitUsageBuilder::new("RequiredParam").build();

        let mut expander = TraitExpander::new();
        let result = expander.expand_safety_trait(&trait_def, &usage);

        assert!(result.is_err());
        assert!(matches!(
            result,
            Err(TraitExpansionError::InvalidParameter(_))
        ));
    }

    #[test]
    fn test_fmea_trait_expansion() {
        let failure_mode =
            FailureMode::new("bit_flip".to_string(), Severity::S3, FailureClass::Residual)
                .with_coverage(99.0);

        let group = FailureModeGroup {
            detector: DetectorRef::Psm("CommandIntegrity".to_string()),
            modes: vec![failure_mode],
        };

        let pattern = FmeaComponentPattern {
            pattern: "*::sda".to_string(),
            library: "automotive_io".to_string(),
            part: "I2C_PHY".to_string(),
            failure_mode_groups: vec![group],
        };

        let fmea_trait = FmeaTrait {
            name: "I2cPhyFailures".to_string(),
            component_patterns: vec![pattern],
        };

        let expander = TraitExpander::new();
        let covered_refs = vec![
            DesignRef::parse("top.i2c_0::sda"),
            DesignRef::parse("top.i2c_1::sda"),
            DesignRef::parse("top.spi_0::mosi"), // Should not match
        ];

        let components = expander.expand_fmea_trait(&fmea_trait, &covered_refs);

        assert_eq!(components.len(), 2); // Only i2c_* with sda
        assert!(components
            .iter()
            .all(|c| c.design_ref.signal == Some("sda".to_string())));
    }

    #[test]
    fn test_hierarchy_trait_expansion() {
        use crate::asil::AsilLevel;
        use crate::hierarchy::SafetyGoalId;

        let mut hierarchy = SafetyHierarchy::new();

        // Add a trait
        let psm = PrimarySafetyMechanism::new("Integrity".to_string(), 99.0);
        let hsr = HardwareSafetyRequirement::new("HSR_TRAIT".to_string(), "From trait".to_string())
            .with_psm(psm);

        let trait_def = SafetyTraitBuilder::new("IntegrityProtection")
            .hsr(hsr)
            .build();

        hierarchy.add_trait(trait_def);

        // Add a goal that uses the trait
        let mut goal = SafetyGoal::new(
            SafetyGoalId(1),
            "SG-001".to_string(),
            "TestGoal".to_string(),
            "Test".to_string(),
            AsilLevel::D,
        );
        goal.trait_usages
            .push(TraitUsageBuilder::new("IntegrityProtection").build());

        hierarchy.add_goal(goal);

        // Expand traits
        expand_all_traits(&mut hierarchy).unwrap();

        // Check that HSR was added to goal
        let goal = hierarchy.goals.get("TestGoal").unwrap();
        assert_eq!(goal.hsrs.len(), 1);
        assert_eq!(goal.hsrs[0].id, "HSR_TRAIT");
    }
}
