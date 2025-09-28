//! Safety requirements management for ISO 26262 compliance
//!
//! Provides structured safety requirements with ASIL allocation,
//! traceability, and verification status tracking.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use chrono::{DateTime, Utc};
use crate::asil::{AsilLevel, VerificationMethod};

/// Safety requirement with ISO 26262 compliance information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SafetyRequirement {
    /// Unique identifier
    pub id: String,
    /// Human-readable description
    pub description: String,
    /// Allocated ASIL level
    pub asil: AsilLevel,
    /// Requirement category
    pub category: RequirementCategory,
    /// Current status
    pub status: RequirementStatus,
    /// Parent requirement (for decomposition)
    pub parent: Option<String>,
    /// Child requirements
    pub children: Vec<String>,
    /// Verification methods used
    pub verification_methods: Vec<VerificationMethod>,
    /// Associated safety mechanisms
    pub safety_mechanisms: Vec<String>,
    /// Rationale for the requirement
    pub rationale: String,
    /// Creation timestamp
    pub created_at: DateTime<Utc>,
    /// Last modified timestamp
    pub modified_at: DateTime<Utc>,
    /// Notes and comments
    pub notes: Vec<String>,
}

/// Categories of safety requirements
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum RequirementCategory {
    /// Functional safety requirement
    Functional,
    /// Technical safety requirement
    Technical,
    /// Hardware safety requirement
    Hardware,
    /// Software safety requirement
    Software,
    /// System safety requirement
    System,
    /// Verification requirement
    Verification,
    /// Validation requirement
    Validation,
}

/// Status of safety requirement implementation
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum RequirementStatus {
    /// Requirement identified but not implemented
    Identified,
    /// Implementation in progress
    InProgress,
    /// Implementation completed
    Implemented,
    /// Verification in progress
    Verifying,
    /// Requirement verified
    Verified,
    /// Requirement validated
    Validated,
    /// Requirement rejected or obsolete
    Rejected,
}

/// Safety requirement manager
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SafetyRequirementManager {
    /// All safety requirements
    requirements: HashMap<String, SafetyRequirement>,
    /// Requirement hierarchy graph
    hierarchy: RequirementHierarchy,
    /// Traceability matrix
    traceability: TraceabilityMatrix,
}

/// Hierarchical structure of requirements
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RequirementHierarchy {
    /// Root requirements (no parents)
    roots: Vec<String>,
    /// Parent-child relationships
    relationships: HashMap<String, Vec<String>>,
}

/// Traceability matrix for requirements
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TraceabilityMatrix {
    /// Requirements to design elements
    req_to_design: HashMap<String, Vec<String>>,
    /// Requirements to verification activities
    req_to_verification: HashMap<String, Vec<String>>,
    /// Requirements to safety mechanisms
    req_to_mechanisms: HashMap<String, Vec<String>>,
}

impl SafetyRequirement {
    /// Create a new safety requirement
    pub fn new(
        id: String,
        description: String,
        asil: AsilLevel,
        category: RequirementCategory,
    ) -> Self {
        let now = Utc::now();
        Self {
            id,
            description,
            asil,
            category,
            status: RequirementStatus::Identified,
            parent: None,
            children: vec![],
            verification_methods: vec![],
            safety_mechanisms: vec![],
            rationale: String::new(),
            created_at: now,
            modified_at: now,
            notes: vec![],
        }
    }

    /// Add a child requirement
    pub fn add_child(&mut self, child_id: String) {
        if !self.children.contains(&child_id) {
            self.children.push(child_id);
            self.modified_at = Utc::now();
        }
    }

    /// Add a verification method
    pub fn add_verification_method(&mut self, method: VerificationMethod) {
        if !self.verification_methods.contains(&method) {
            self.verification_methods.push(method);
            self.modified_at = Utc::now();
        }
    }

    /// Add a safety mechanism
    pub fn add_safety_mechanism(&mut self, mechanism: String) {
        if !self.safety_mechanisms.contains(&mechanism) {
            self.safety_mechanisms.push(mechanism);
            self.modified_at = Utc::now();
        }
    }

    /// Update requirement status
    pub fn update_status(&mut self, status: RequirementStatus) {
        self.status = status;
        self.modified_at = Utc::now();
    }

    /// Add a note
    pub fn add_note(&mut self, note: String) {
        self.notes.push(note);
        self.modified_at = Utc::now();
    }

    /// Check if requirement is complete (verified or validated)
    pub fn is_complete(&self) -> bool {
        matches!(self.status, RequirementStatus::Verified | RequirementStatus::Validated)
    }

    /// Check if requirement meets ASIL verification requirements
    pub fn meets_asil_verification(&self) -> bool {
        let required_methods = self.asil.requirements().verification_methods;
        required_methods.iter().all(|method| self.verification_methods.contains(method))
    }
}

impl SafetyRequirementManager {
    /// Create a new requirement manager
    pub fn new() -> Self {
        Self {
            requirements: HashMap::new(),
            hierarchy: RequirementHierarchy {
                roots: vec![],
                relationships: HashMap::new(),
            },
            traceability: TraceabilityMatrix {
                req_to_design: HashMap::new(),
                req_to_verification: HashMap::new(),
                req_to_mechanisms: HashMap::new(),
            },
        }
    }

    /// Add a safety requirement
    pub fn add_requirement(&mut self, requirement: SafetyRequirement) {
        let id = requirement.id.clone();

        // Update hierarchy
        if let Some(parent_id) = &requirement.parent {
            self.hierarchy.relationships
                .entry(parent_id.clone())
                .or_insert_with(Vec::new)
                .push(id.clone());
        } else {
            self.hierarchy.roots.push(id.clone());
        }

        self.requirements.insert(id, requirement);
    }

    /// Get a requirement by ID
    pub fn get_requirement(&self, id: &str) -> Option<&SafetyRequirement> {
        self.requirements.get(id)
    }

    /// Get a mutable reference to a requirement
    pub fn get_requirement_mut(&mut self, id: &str) -> Option<&mut SafetyRequirement> {
        self.requirements.get_mut(id)
    }

    /// Get all requirements for a specific ASIL level
    pub fn get_requirements_by_asil(&self, asil: AsilLevel) -> Vec<&SafetyRequirement> {
        self.requirements
            .values()
            .filter(|req| req.asil == asil)
            .collect()
    }

    /// Get all requirements by category
    pub fn get_requirements_by_category(&self, category: RequirementCategory) -> Vec<&SafetyRequirement> {
        self.requirements
            .values()
            .filter(|req| req.category == category)
            .collect()
    }

    /// Get child requirements
    pub fn get_children(&self, parent_id: &str) -> Vec<&SafetyRequirement> {
        self.hierarchy
            .relationships
            .get(parent_id)
            .map(|children| {
                children
                    .iter()
                    .filter_map(|id| self.requirements.get(id))
                    .collect()
            })
            .unwrap_or_default()
    }

    /// Get parent requirement
    pub fn get_parent(&self, child_id: &str) -> Option<&SafetyRequirement> {
        self.requirements
            .get(child_id)
            .and_then(|req| req.parent.as_ref())
            .and_then(|parent_id| self.requirements.get(parent_id))
    }

    /// Add traceability link to design element
    pub fn add_design_trace(&mut self, req_id: String, design_element: String) {
        self.traceability
            .req_to_design
            .entry(req_id)
            .or_insert_with(Vec::new)
            .push(design_element);
    }

    /// Add traceability link to verification activity
    pub fn add_verification_trace(&mut self, req_id: String, verification_activity: String) {
        self.traceability
            .req_to_verification
            .entry(req_id)
            .or_insert_with(Vec::new)
            .push(verification_activity);
    }

    /// Add traceability link to safety mechanism
    pub fn add_mechanism_trace(&mut self, req_id: String, mechanism: String) {
        self.traceability
            .req_to_mechanisms
            .entry(req_id)
            .or_insert_with(Vec::new)
            .push(mechanism);
    }

    /// Generate requirement coverage report
    pub fn generate_coverage_report(&self) -> CoverageReport {
        let total = self.requirements.len();
        let verified = self.requirements
            .values()
            .filter(|req| req.is_complete())
            .count();

        let asil_breakdown = {
            let mut breakdown = HashMap::new();
            for req in self.requirements.values() {
                let entry = breakdown.entry(req.asil).or_insert((0, 0));
                entry.0 += 1;
                if req.is_complete() {
                    entry.1 += 1;
                }
            }
            breakdown
        };

        CoverageReport {
            total_requirements: total,
            verified_requirements: verified,
            coverage_percentage: if total > 0 {
                (verified as f64 / total as f64) * 100.0
            } else {
                0.0
            },
            asil_breakdown,
        }
    }

    /// Validate requirement hierarchy
    pub fn validate_hierarchy(&self) -> Vec<ValidationError> {
        let mut errors = vec![];

        // Check for orphaned requirements
        for req in self.requirements.values() {
            if let Some(parent_id) = &req.parent {
                if !self.requirements.contains_key(parent_id) {
                    errors.push(ValidationError::OrphanedRequirement(req.id.clone()));
                }
            }
        }

        // Check for circular dependencies
        for req in self.requirements.values() {
            if self.has_circular_dependency(&req.id) {
                errors.push(ValidationError::CircularDependency(req.id.clone()));
            }
        }

        // Check ASIL consistency in hierarchy
        for req in self.requirements.values() {
            if let Some(parent) = self.get_parent(&req.id) {
                if req.asil > parent.asil {
                    errors.push(ValidationError::AsilInconsistency {
                        child: req.id.clone(),
                        parent: parent.id.clone(),
                    });
                }
            }
        }

        errors
    }

    /// Check for circular dependency
    fn has_circular_dependency(&self, req_id: &str) -> bool {
        let mut visited = std::collections::HashSet::new();
        self.has_circular_dependency_helper(req_id, &mut visited)
    }

    fn has_circular_dependency_helper(
        &self,
        req_id: &str,
        visited: &mut std::collections::HashSet<String>,
    ) -> bool {
        if visited.contains(req_id) {
            return true;
        }

        visited.insert(req_id.to_string());

        if let Some(req) = self.requirements.get(req_id) {
            for child_id in &req.children {
                if self.has_circular_dependency_helper(child_id, visited) {
                    return true;
                }
            }
        }

        visited.remove(req_id);
        false
    }
}

/// Coverage report for safety requirements
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CoverageReport {
    /// Total number of requirements
    pub total_requirements: usize,
    /// Number of verified requirements
    pub verified_requirements: usize,
    /// Coverage percentage
    pub coverage_percentage: f64,
    /// Breakdown by ASIL level (total, verified)
    pub asil_breakdown: HashMap<AsilLevel, (usize, usize)>,
}

/// Validation errors for requirement hierarchy
#[derive(Debug, Clone, PartialEq)]
pub enum ValidationError {
    /// Requirement references non-existent parent
    OrphanedRequirement(String),
    /// Circular dependency detected
    CircularDependency(String),
    /// ASIL level inconsistency
    AsilInconsistency { child: String, parent: String },
}

impl Default for SafetyRequirementManager {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_safety_requirement_creation() {
        let req = SafetyRequirement::new(
            "REQ-001".to_string(),
            "System shall detect hardware failures".to_string(),
            AsilLevel::D,
            RequirementCategory::Functional,
        );

        assert_eq!(req.id, "REQ-001");
        assert_eq!(req.asil, AsilLevel::D);
        assert_eq!(req.status, RequirementStatus::Identified);
    }

    #[test]
    fn test_requirement_manager() {
        let mut manager = SafetyRequirementManager::new();

        let req = SafetyRequirement::new(
            "REQ-001".to_string(),
            "Test requirement".to_string(),
            AsilLevel::B,
            RequirementCategory::Functional,
        );

        manager.add_requirement(req);

        let retrieved = manager.get_requirement("REQ-001").unwrap();
        assert_eq!(retrieved.id, "REQ-001");
        assert_eq!(retrieved.asil, AsilLevel::B);
    }

    #[test]
    fn test_requirement_hierarchy() {
        let mut manager = SafetyRequirementManager::new();

        let mut parent = SafetyRequirement::new(
            "REQ-100".to_string(),
            "Parent requirement".to_string(),
            AsilLevel::C,
            RequirementCategory::System,
        );

        let mut child = SafetyRequirement::new(
            "REQ-101".to_string(),
            "Child requirement".to_string(),
            AsilLevel::B,
            RequirementCategory::Hardware,
        );

        child.parent = Some("REQ-100".to_string());
        parent.add_child("REQ-101".to_string());

        manager.add_requirement(parent);
        manager.add_requirement(child);

        let children = manager.get_children("REQ-100");
        assert_eq!(children.len(), 1);
        assert_eq!(children[0].id, "REQ-101");

        let parent_ref = manager.get_parent("REQ-101").unwrap();
        assert_eq!(parent_ref.id, "REQ-100");
    }

    #[test]
    fn test_coverage_report() {
        let mut manager = SafetyRequirementManager::new();

        for i in 0..5 {
            let mut req = SafetyRequirement::new(
                format!("REQ-{:03}", i),
                format!("Requirement {}", i),
                AsilLevel::A,
                RequirementCategory::Functional,
            );

            if i < 3 {
                req.update_status(RequirementStatus::Verified);
            }

            manager.add_requirement(req);
        }

        let report = manager.generate_coverage_report();
        assert_eq!(report.total_requirements, 5);
        assert_eq!(report.verified_requirements, 3);
        assert_eq!(report.coverage_percentage, 60.0);
    }

    #[test]
    fn test_hierarchy_validation() {
        let mut manager = SafetyRequirementManager::new();

        // Create requirement with non-existent parent
        let mut orphan = SafetyRequirement::new(
            "REQ-ORPHAN".to_string(),
            "Orphaned requirement".to_string(),
            AsilLevel::A,
            RequirementCategory::Functional,
        );
        orphan.parent = Some("NON-EXISTENT".to_string());

        manager.add_requirement(orphan);

        let errors = manager.validate_hierarchy();
        assert_eq!(errors.len(), 1);
        assert!(matches!(errors[0], ValidationError::OrphanedRequirement(_)));
    }
}