//! Design Path Resolution for ISO 26262 Safety Analysis
//!
//! Resolves instance paths (e.g., "top.brake_main::pressure_a") to HIR/MIR entities.
//! Supports glob patterns for matching multiple design elements.
//!
//! Used by safety_entity to verify that referenced design elements exist and
//! to auto-discover #[implements(...)] annotations from design files.

use crate::hierarchy::{DesignPattern, DesignRef, InstancePath};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

// ============================================================================
// Design Index - Built from HIR
// ============================================================================

/// Index of design hierarchy for path resolution
#[derive(Debug, Clone, Default)]
pub struct DesignIndex {
    /// All instances in design hierarchy
    instances: HashMap<String, DesignInstance>,
    /// All signals in design hierarchy (fully qualified: "top.brake_main::pressure_a")
    signals: HashMap<String, DesignSignal>,
    /// Top-level module name
    top_module: Option<String>,
    /// Instance hierarchy (parent -> children)
    hierarchy: HashMap<String, Vec<String>>,
    /// Safety annotations found in design (#[implements(...)])
    annotations: Vec<SafetyAnnotation>,
}

impl DesignIndex {
    /// Create a new empty design index
    pub fn new() -> Self {
        Self::default()
    }

    /// Set the top-level module
    pub fn set_top(&mut self, name: &str) {
        self.top_module = Some(name.to_string());
    }

    /// Add an instance to the index
    pub fn add_instance(&mut self, instance: DesignInstance) {
        let path = instance.path.to_string();

        // Update hierarchy
        if let Some(parent) = instance.path.parent() {
            self.hierarchy
                .entry(parent.to_string())
                .or_default()
                .push(path.clone());
        }

        self.instances.insert(path, instance);
    }

    /// Add a signal to the index
    pub fn add_signal(&mut self, signal: DesignSignal) {
        let key = format!("{}::{}", signal.instance_path.to_string(), signal.name);
        self.signals.insert(key, signal);
    }

    /// Add a safety annotation found in design
    pub fn add_annotation(&mut self, annotation: SafetyAnnotation) {
        self.annotations.push(annotation);
    }

    /// Get an instance by path
    pub fn get_instance(&self, path: &str) -> Option<&DesignInstance> {
        self.instances.get(path)
    }

    /// Get a signal by full path (instance::signal)
    pub fn get_signal(&self, path: &str) -> Option<&DesignSignal> {
        self.signals.get(path)
    }

    /// Get all children of an instance
    pub fn get_children(&self, path: &str) -> Option<&Vec<String>> {
        self.hierarchy.get(path)
    }

    /// Get all safety annotations
    pub fn get_annotations(&self) -> &[SafetyAnnotation] {
        &self.annotations
    }

    /// Get annotations for a specific goal
    pub fn get_annotations_for_goal(&self, goal: &str) -> Vec<&SafetyAnnotation> {
        self.annotations
            .iter()
            .filter(|a| a.goal_name == goal)
            .collect()
    }

    /// Get annotations for a specific mechanism
    pub fn get_annotations_for_mechanism(&self, goal: &str, mechanism: &str) -> Vec<&SafetyAnnotation> {
        self.annotations
            .iter()
            .filter(|a| a.goal_name == goal && a.mechanism_name == mechanism)
            .collect()
    }
}

/// Design instance information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DesignInstance {
    /// Instance path
    pub path: InstancePath,
    /// Entity type name
    pub entity_type: String,
    /// Port count
    pub port_count: usize,
    /// Signal count
    pub signal_count: usize,
    /// Module instance depth
    pub depth: usize,
}

impl DesignInstance {
    /// Create a new design instance
    pub fn new(path: InstancePath, entity_type: String) -> Self {
        Self {
            path,
            entity_type,
            port_count: 0,
            signal_count: 0,
            depth: 0,
        }
    }
}

/// Design signal information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DesignSignal {
    /// Instance path containing this signal
    pub instance_path: InstancePath,
    /// Signal name
    pub name: String,
    /// Signal width in bits
    pub width: u32,
    /// Is this a register?
    pub is_register: bool,
    /// Direction (for ports)
    pub direction: Option<SignalDirection>,
}

impl DesignSignal {
    /// Create a new design signal
    pub fn new(instance_path: InstancePath, name: String, width: u32) -> Self {
        Self {
            instance_path,
            name,
            width,
            is_register: false,
            direction: None,
        }
    }

    /// Get full path including signal
    pub fn full_path(&self) -> String {
        format!("{}::{}", self.instance_path.to_string(), self.name)
    }
}

/// Signal direction
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum SignalDirection {
    Input,
    Output,
    InOut,
}

/// Safety annotation from design (#[implements(...)])
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SafetyAnnotation {
    /// Location in design (instance or signal path)
    pub design_ref: DesignRef,
    /// Safety goal name
    pub goal_name: String,
    /// Mechanism name within goal
    pub mechanism_name: String,
    /// Is this an entity-level or signal-level annotation?
    pub level: AnnotationLevel,
}

impl SafetyAnnotation {
    /// Create a new safety annotation
    pub fn new(design_ref: DesignRef, goal_name: String, mechanism_name: String) -> Self {
        Self {
            design_ref,
            goal_name,
            mechanism_name,
            level: AnnotationLevel::Signal,
        }
    }

    /// Set annotation level
    pub fn with_level(mut self, level: AnnotationLevel) -> Self {
        self.level = level;
        self
    }
}

/// Level of safety annotation
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum AnnotationLevel {
    /// Entity-level annotation (#[implements(...)] on entity)
    Entity,
    /// Instance-level annotation (#[implements(...)] on inst)
    Instance,
    /// Signal-level annotation (#[implements(...)] on signal)
    Signal,
}

// ============================================================================
// Design Path Resolver
// ============================================================================

/// Resolver for mapping design paths to HIR/MIR entities
pub struct DesignPathResolver {
    /// Design index
    index: DesignIndex,
}

impl DesignPathResolver {
    /// Create a new resolver with the given index
    pub fn new(index: DesignIndex) -> Self {
        Self { index }
    }

    /// Get the underlying index
    pub fn index(&self) -> &DesignIndex {
        &self.index
    }

    /// Resolve a design reference
    pub fn resolve(&self, design_ref: &DesignRef) -> ResolutionResult {
        let instance_path = design_ref.instance.to_string();

        // Check if instance exists
        if self.index.get_instance(&instance_path).is_none() {
            return ResolutionResult::NotFound(format!(
                "Instance '{}' not found in design",
                instance_path
            ));
        }

        // If signal is specified, check if it exists
        if let Some(ref signal) = design_ref.signal {
            let full_path = format!("{}::{}", instance_path, signal);
            if let Some(sig) = self.index.get_signal(&full_path) {
                // Check bit range if specified
                if let Some((high, low)) = design_ref.bit_range {
                    if high >= sig.width || low > high {
                        return ResolutionResult::Error(format!(
                            "Bit range [{}:{}] invalid for {}-bit signal '{}'",
                            high, low, sig.width, signal
                        ));
                    }
                }
                ResolutionResult::Signal(sig.clone())
            } else {
                ResolutionResult::NotFound(format!(
                    "Signal '{}' not found in instance '{}'",
                    signal, instance_path
                ))
            }
        } else {
            let instance = self.index.get_instance(&instance_path).unwrap().clone();
            ResolutionResult::Instance(instance)
        }
    }

    /// Resolve a design pattern (may return multiple results)
    pub fn resolve_pattern(&self, pattern: &DesignPattern) -> Vec<ResolvedDesignRef> {
        let mut results = Vec::new();

        // Match instances
        for (path, instance) in &self.index.instances {
            if instance.path.matches_pattern(&pattern.instance_pattern) {
                if let Some(ref sig_pattern) = pattern.signal_pattern {
                    // Match signals within instance
                    let prefix = format!("{}::", path);
                    for (sig_path, signal) in &self.index.signals {
                        if sig_path.starts_with(&prefix) {
                            let sig_name = &sig_path[prefix.len()..];
                            if matches_signal_pattern(sig_name, sig_pattern) {
                                results.push(ResolvedDesignRef {
                                    design_ref: DesignRef::signal(
                                        instance.path.clone(),
                                        sig_name.to_string(),
                                    ),
                                    instance: Some(instance.clone()),
                                    signal: Some(signal.clone()),
                                });
                            }
                        }
                    }
                } else {
                    // Instance only
                    results.push(ResolvedDesignRef {
                        design_ref: DesignRef::instance(instance.path.clone()),
                        instance: Some(instance.clone()),
                        signal: None,
                    });
                }
            }
        }

        results
    }

    /// Validate that all design references in a list exist
    pub fn validate_refs(&self, refs: &[DesignRef]) -> ValidationResult {
        let mut errors = Vec::new();

        for design_ref in refs {
            match self.resolve(design_ref) {
                ResolutionResult::NotFound(msg) => {
                    errors.push(ValidationError {
                        design_ref: design_ref.clone(),
                        message: msg,
                    });
                }
                ResolutionResult::Error(msg) => {
                    errors.push(ValidationError {
                        design_ref: design_ref.clone(),
                        message: msg,
                    });
                }
                _ => {}
            }
        }

        if errors.is_empty() {
            ValidationResult::Valid
        } else {
            ValidationResult::Invalid(errors)
        }
    }

    /// Validate a design pattern
    pub fn validate_pattern(&self, pattern: &DesignPattern) -> PatternValidationResult {
        let matches = self.resolve_pattern(pattern);

        if matches.is_empty() {
            PatternValidationResult::NoMatches {
                pattern: pattern.clone(),
                suggestion: suggest_pattern_fix(pattern, &self.index),
            }
        } else {
            PatternValidationResult::Matches(matches)
        }
    }

    /// Find annotations implementing a specific mechanism
    pub fn find_implementations(&self, goal: &str, mechanism: &str) -> Vec<&SafetyAnnotation> {
        self.index.get_annotations_for_mechanism(goal, mechanism)
    }

    /// Get all annotations for auto-discovery
    pub fn discover_annotations(&self) -> &[SafetyAnnotation] {
        self.index.get_annotations()
    }
}

/// Result of resolving a design reference
#[derive(Debug, Clone)]
pub enum ResolutionResult {
    /// Resolved to an instance
    Instance(DesignInstance),
    /// Resolved to a signal
    Signal(DesignSignal),
    /// Not found in design
    NotFound(String),
    /// Error during resolution
    Error(String),
}

/// Resolved design reference with full information
#[derive(Debug, Clone)]
pub struct ResolvedDesignRef {
    /// Original design reference
    pub design_ref: DesignRef,
    /// Resolved instance (if applicable)
    pub instance: Option<DesignInstance>,
    /// Resolved signal (if applicable)
    pub signal: Option<DesignSignal>,
}

/// Validation result
#[derive(Debug, Clone)]
pub enum ValidationResult {
    /// All references valid
    Valid,
    /// Some references invalid
    Invalid(Vec<ValidationError>),
}

/// Validation error
#[derive(Debug, Clone)]
pub struct ValidationError {
    /// Design reference that failed
    pub design_ref: DesignRef,
    /// Error message
    pub message: String,
}

/// Pattern validation result
#[derive(Debug, Clone)]
pub enum PatternValidationResult {
    /// Pattern has matches
    Matches(Vec<ResolvedDesignRef>),
    /// No matches found
    NoMatches {
        pattern: DesignPattern,
        suggestion: Option<String>,
    },
}

// ============================================================================
// Helper Functions
// ============================================================================

/// Check if a signal name matches a pattern
fn matches_signal_pattern(name: &str, pattern: &str) -> bool {
    if pattern == "*" {
        return true;
    }
    if pattern.ends_with('*') {
        let prefix = &pattern[..pattern.len() - 1];
        return name.starts_with(prefix);
    }
    if pattern.starts_with('*') {
        let suffix = &pattern[1..];
        return name.ends_with(suffix);
    }
    name == pattern
}

/// Suggest a fix for a pattern that doesn't match
fn suggest_pattern_fix(pattern: &DesignPattern, index: &DesignIndex) -> Option<String> {
    // Try to find similar instance paths
    let pattern_segments: Vec<&str> = pattern.instance_pattern.split('.').collect();

    if pattern_segments.is_empty() {
        return None;
    }

    // Find instances that match the first segment
    let first = pattern_segments[0];
    let matching_prefixes: Vec<_> = index
        .instances
        .keys()
        .filter(|p| p.starts_with(first) || first.trim_end_matches('*').is_empty())
        .take(3)
        .collect();

    if matching_prefixes.is_empty() {
        // Suggest available top-level instances
        let tops: Vec<_> = index
            .instances
            .values()
            .filter(|i| i.depth == 0)
            .map(|i| i.path.to_string())
            .take(3)
            .collect();

        if !tops.is_empty() {
            return Some(format!("Did you mean one of: {}?", tops.join(", ")));
        }
    } else {
        return Some(format!("Did you mean: {}?", matching_prefixes[0]));
    }

    None
}

// ============================================================================
// Builder from HIR (placeholder - real implementation in Phase 8)
// ============================================================================

/// Builder for constructing DesignIndex from HIR
pub struct DesignIndexBuilder {
    index: DesignIndex,
    current_path: Vec<String>,
}

impl DesignIndexBuilder {
    /// Create a new builder
    pub fn new() -> Self {
        Self {
            index: DesignIndex::new(),
            current_path: Vec::new(),
        }
    }

    /// Set top module
    pub fn set_top(mut self, name: &str) -> Self {
        self.index.set_top(name);
        self.current_path.push(name.to_string());

        // Add top as an instance
        let path = InstancePath::new(vec![name.to_string()]);
        let instance = DesignInstance::new(path, name.to_string());
        self.index.add_instance(instance);

        self
    }

    /// Add an instance at current path
    pub fn add_instance(&mut self, name: &str, entity_type: &str) {
        let mut path_segments = self.current_path.clone();
        path_segments.push(name.to_string());

        let path = InstancePath::new(path_segments);
        let mut instance = DesignInstance::new(path, entity_type.to_string());
        instance.depth = self.current_path.len();

        self.index.add_instance(instance);
    }

    /// Add a signal at current path
    pub fn add_signal(&mut self, name: &str, width: u32, is_register: bool) {
        let path = InstancePath::new(self.current_path.clone());
        let mut signal = DesignSignal::new(path, name.to_string(), width);
        signal.is_register = is_register;
        self.index.add_signal(signal);
    }

    /// Add a signal with direction (for ports)
    pub fn add_port(&mut self, name: &str, width: u32, direction: SignalDirection) {
        let path = InstancePath::new(self.current_path.clone());
        let mut signal = DesignSignal::new(path, name.to_string(), width);
        signal.direction = Some(direction);
        self.index.add_signal(signal);
    }

    /// Add a safety annotation
    pub fn add_annotation(&mut self, signal: Option<&str>, goal: &str, mechanism: &str) {
        let path = InstancePath::new(self.current_path.clone());
        let design_ref = if let Some(sig) = signal {
            DesignRef::signal(path, sig.to_string())
        } else {
            DesignRef::instance(path)
        };

        let level = if signal.is_some() {
            AnnotationLevel::Signal
        } else {
            AnnotationLevel::Instance
        };

        let annotation = SafetyAnnotation::new(design_ref, goal.to_string(), mechanism.to_string())
            .with_level(level);

        self.index.add_annotation(annotation);
    }

    /// Enter a sub-instance
    pub fn enter(&mut self, name: &str) {
        self.current_path.push(name.to_string());
    }

    /// Exit current sub-instance
    pub fn exit(&mut self) {
        self.current_path.pop();
    }

    /// Build the final index
    pub fn build(self) -> DesignIndex {
        self.index
    }
}

impl Default for DesignIndexBuilder {
    fn default() -> Self {
        Self::new()
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    fn create_test_index() -> DesignIndex {
        let mut builder = DesignIndexBuilder::new().set_top("top");

        // Add top-level instance
        builder.add_instance("brake_main", "BrakeController");
        builder.add_signal("state", 4, true);

        // Enter brake_main
        builder.enter("brake_main");
        builder.add_signal("pressure_a", 12, false);
        builder.add_signal("pressure_b", 12, false);
        builder.add_signal("voted_pressure", 12, false);
        builder.add_signal("fault", 1, true);
        builder.add_annotation(Some("voted_pressure"), "BrakingSafety", "SensorVoting");

        // Add CRC sub-instance
        builder.add_instance("crc", "Crc8Checker");
        builder.enter("crc");
        builder.add_signal("error", 1, true);
        builder.exit();

        // Add another brake instance
        builder.exit();
        builder.add_instance("brake_aux", "BrakeController");

        builder.build()
    }

    #[test]
    fn test_design_index_creation() {
        let index = create_test_index();

        assert!(index.get_instance("top").is_some());
        assert!(index.get_instance("top.brake_main").is_some());
        assert!(index.get_instance("top.brake_main.crc").is_some());
        assert!(index.get_instance("top.brake_aux").is_some());
    }

    #[test]
    fn test_signal_resolution() {
        let index = create_test_index();
        let resolver = DesignPathResolver::new(index);

        let ref1 = DesignRef::from_str("top.brake_main::pressure_a");
        match resolver.resolve(&ref1) {
            ResolutionResult::Signal(sig) => {
                assert_eq!(sig.name, "pressure_a");
                assert_eq!(sig.width, 12);
            }
            _ => panic!("Expected signal resolution"),
        }
    }

    #[test]
    fn test_instance_resolution() {
        let index = create_test_index();
        let resolver = DesignPathResolver::new(index);

        let ref1 = DesignRef::from_str("top.brake_main.crc");
        match resolver.resolve(&ref1) {
            ResolutionResult::Instance(inst) => {
                assert_eq!(inst.entity_type, "Crc8Checker");
            }
            _ => panic!("Expected instance resolution"),
        }
    }

    #[test]
    fn test_not_found() {
        let index = create_test_index();
        let resolver = DesignPathResolver::new(index);

        let ref1 = DesignRef::from_str("top.nonexistent");
        match resolver.resolve(&ref1) {
            ResolutionResult::NotFound(_) => {}
            _ => panic!("Expected not found"),
        }

        let ref2 = DesignRef::from_str("top.brake_main::nonexistent_signal");
        match resolver.resolve(&ref2) {
            ResolutionResult::NotFound(_) => {}
            _ => panic!("Expected not found"),
        }
    }

    #[test]
    fn test_pattern_matching() {
        let index = create_test_index();
        let resolver = DesignPathResolver::new(index);

        // Match all brake_* instances
        let pattern = DesignPattern::instances("top.brake_*");
        let matches = resolver.resolve_pattern(&pattern);
        assert_eq!(matches.len(), 2); // brake_main and brake_aux

        // Match all pressure_* signals in brake_main
        let pattern = DesignPattern::signals("top.brake_main", "pressure_*");
        let matches = resolver.resolve_pattern(&pattern);
        assert_eq!(matches.len(), 2); // pressure_a and pressure_b
    }

    #[test]
    fn test_wildcard_all_signals() {
        let index = create_test_index();
        let resolver = DesignPathResolver::new(index);

        let pattern = DesignPattern::signals("top.brake_main", "*");
        let matches = resolver.resolve_pattern(&pattern);
        assert!(matches.len() >= 4); // pressure_a, pressure_b, voted_pressure, fault
    }

    #[test]
    fn test_validation() {
        let index = create_test_index();
        let resolver = DesignPathResolver::new(index);

        let refs = vec![
            DesignRef::from_str("top.brake_main::pressure_a"),
            DesignRef::from_str("top.brake_main.crc"),
        ];

        match resolver.validate_refs(&refs) {
            ValidationResult::Valid => {}
            _ => panic!("Expected valid"),
        }

        let invalid_refs = vec![
            DesignRef::from_str("top.brake_main::pressure_a"),
            DesignRef::from_str("top.nonexistent::signal"),
        ];

        match resolver.validate_refs(&invalid_refs) {
            ValidationResult::Invalid(errors) => {
                assert_eq!(errors.len(), 1);
            }
            _ => panic!("Expected invalid"),
        }
    }

    #[test]
    fn test_annotation_discovery() {
        let index = create_test_index();
        let resolver = DesignPathResolver::new(index);

        let annotations = resolver.discover_annotations();
        assert!(!annotations.is_empty());

        let voting_impls = resolver.find_implementations("BrakingSafety", "SensorVoting");
        assert_eq!(voting_impls.len(), 1);
        assert_eq!(
            voting_impls[0].design_ref.signal,
            Some("voted_pressure".to_string())
        );
    }

    #[test]
    fn test_bit_range_validation() {
        let index = create_test_index();
        let resolver = DesignPathResolver::new(index);

        // Valid bit range
        let ref1 = DesignRef::from_str("top.brake_main::pressure_a[11:0]");
        match resolver.resolve(&ref1) {
            ResolutionResult::Signal(_) => {}
            _ => panic!("Expected valid signal with bit range"),
        }

        // Invalid bit range (exceeds width)
        let ref2 = DesignRef::signal_slice(
            InstancePath::from_str("top.brake_main"),
            "pressure_a".to_string(),
            15,
            0,
        );
        match resolver.resolve(&ref2) {
            ResolutionResult::Error(msg) => {
                assert!(msg.contains("invalid"));
            }
            _ => panic!("Expected error for invalid bit range"),
        }
    }

    #[test]
    fn test_hierarchy_traversal() {
        let index = create_test_index();

        let children = index.get_children("top");
        assert!(children.is_some());
        let children = children.unwrap();
        assert!(children.contains(&"top.brake_main".to_string()));
        assert!(children.contains(&"top.brake_aux".to_string()));

        let sub_children = index.get_children("top.brake_main");
        assert!(sub_children.is_some());
        assert!(sub_children.unwrap().contains(&"top.brake_main.crc".to_string()));
    }

    #[test]
    fn test_signal_pattern_matching() {
        assert!(matches_signal_pattern("pressure_a", "*"));
        assert!(matches_signal_pattern("pressure_a", "pressure_*"));
        assert!(matches_signal_pattern("pressure_a", "pressure_a"));
        assert!(!matches_signal_pattern("pressure_a", "pressure_b"));
        assert!(matches_signal_pattern("debug_signal", "*_signal"));
        assert!(!matches_signal_pattern("debug_value", "*_signal"));
    }
}
