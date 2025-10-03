//! Requirements tracking and verification

use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};
use std::fmt;

/// Requirement definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Requirement {
    /// Unique requirement ID
    pub id: String,

    /// Requirement description
    pub description: String,

    /// Priority level
    pub priority: Priority,

    /// Requirement type
    pub req_type: RequirementType,

    /// Parent requirement ID (for traceability)
    pub parent: Option<String>,

    /// Child requirements
    pub children: Vec<String>,

    /// Verification criteria
    pub criteria: Vec<VerificationCriterion>,

    /// Associated properties
    pub properties: Vec<String>,

    /// Associated tests
    pub tests: Vec<String>,

    /// Status
    pub status: RequirementStatus,

    /// Notes and comments
    pub notes: Vec<String>,
}

/// Requirement priority
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Priority {
    Critical,
    High,
    Medium,
    Low,
}

/// Requirement type
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum RequirementType {
    /// Functional requirement
    Functional,

    /// Performance requirement
    Performance,

    /// Power requirement
    Power,

    /// Area/resource requirement
    Area,

    /// Interface requirement
    Interface,

    /// Safety requirement
    Safety,

    /// Security requirement
    Security,
}

/// Requirement status
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum RequirementStatus {
    /// Not yet implemented
    NotImplemented,

    /// Implementation in progress
    InProgress,

    /// Implemented but not verified
    Implemented,

    /// Verified through testing
    Verified,

    /// Formally proven
    Proven,

    /// Waived or not applicable
    Waived,
}

/// Verification criterion
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VerificationCriterion {
    /// Criterion description
    pub description: String,

    /// Method of verification
    pub method: VerificationMethod,

    /// Pass criteria
    pub pass_criteria: String,

    /// Status
    pub status: CriterionStatus,
}

/// Verification method
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum VerificationMethod {
    /// Simulation-based testing
    Simulation,

    /// Formal verification
    Formal,

    /// Code review
    Review,

    /// Static analysis
    StaticAnalysis,

    /// Hardware testing
    HardwareTest,

    /// Performance analysis
    Performance,
}

/// Criterion status
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum CriterionStatus {
    NotTested,
    InProgress,
    Passed,
    Failed,
    Partial,
}

/// Requirements tracker
pub struct RequirementTracker {
    /// All requirements
    requirements: HashMap<String, Requirement>,

    /// Requirement hierarchy
    hierarchy: RequirementHierarchy,

    /// Coverage matrix
    coverage_matrix: CoverageMatrix,

    /// Traceability links
    traceability: TraceabilityMatrix,
}

/// Requirement hierarchy
struct RequirementHierarchy {
    /// Root requirements
    roots: Vec<String>,

    /// Parent-child relationships
    children: HashMap<String, Vec<String>>,
}

/// Coverage matrix
struct CoverageMatrix {
    /// Requirement to properties mapping
    req_to_properties: HashMap<String, HashSet<String>>,

    /// Requirement to tests mapping
    req_to_tests: HashMap<String, HashSet<String>>,

    /// Property to requirements mapping
    property_to_reqs: HashMap<String, HashSet<String>>,

    /// Test to requirements mapping
    test_to_reqs: HashMap<String, HashSet<String>>,
}

/// Traceability matrix
struct TraceabilityMatrix {
    /// Forward traceability (parent to children)
    forward: HashMap<String, HashSet<String>>,

    /// Backward traceability (child to parents)
    backward: HashMap<String, HashSet<String>>,

    /// Derived from relationships
    derived_from: HashMap<String, HashSet<String>>,

    /// Refines relationships
    refines: HashMap<String, HashSet<String>>,
}

impl RequirementTracker {
    /// Create a new requirement tracker
    pub fn new() -> Self {
        Self {
            requirements: HashMap::new(),
            hierarchy: RequirementHierarchy {
                roots: Vec::new(),
                children: HashMap::new(),
            },
            coverage_matrix: CoverageMatrix {
                req_to_properties: HashMap::new(),
                req_to_tests: HashMap::new(),
                property_to_reqs: HashMap::new(),
                test_to_reqs: HashMap::new(),
            },
            traceability: TraceabilityMatrix {
                forward: HashMap::new(),
                backward: HashMap::new(),
                derived_from: HashMap::new(),
                refines: HashMap::new(),
            },
        }
    }

    /// Add a requirement
    pub fn add_requirement(&mut self, req: Requirement) {
        let id = req.id.clone();

        // Update hierarchy
        if req.parent.is_none() {
            self.hierarchy.roots.push(id.clone());
        } else if let Some(parent) = &req.parent {
            self.hierarchy
                .children
                .entry(parent.clone())
                .or_default()
                .push(id.clone());
        }

        // Update coverage matrix
        self.coverage_matrix
            .req_to_properties
            .insert(id.clone(), req.properties.iter().cloned().collect());
        self.coverage_matrix
            .req_to_tests
            .insert(id.clone(), req.tests.iter().cloned().collect());

        for prop in &req.properties {
            self.coverage_matrix
                .property_to_reqs
                .entry(prop.clone())
                .or_default()
                .insert(id.clone());
        }

        for test in &req.tests {
            self.coverage_matrix
                .test_to_reqs
                .entry(test.clone())
                .or_default()
                .insert(id.clone());
        }

        // Update traceability
        if let Some(parent) = &req.parent {
            self.traceability
                .forward
                .entry(parent.clone())
                .or_default()
                .insert(id.clone());
            self.traceability
                .backward
                .entry(id.clone())
                .or_default()
                .insert(parent.clone());
        }

        self.requirements.insert(id, req);
    }

    /// Link requirement to property
    pub fn link_property(&mut self, req_id: &str, property_id: &str) {
        if let Some(req) = self.requirements.get_mut(req_id) {
            req.properties.push(property_id.to_string());
            self.coverage_matrix
                .req_to_properties
                .entry(req_id.to_string())
                .or_default()
                .insert(property_id.to_string());
            self.coverage_matrix
                .property_to_reqs
                .entry(property_id.to_string())
                .or_default()
                .insert(req_id.to_string());
        }
    }

    /// Link requirement to test
    pub fn link_test(&mut self, req_id: &str, test_id: &str) {
        if let Some(req) = self.requirements.get_mut(req_id) {
            req.tests.push(test_id.to_string());
            self.coverage_matrix
                .req_to_tests
                .entry(req_id.to_string())
                .or_default()
                .insert(test_id.to_string());
            self.coverage_matrix
                .test_to_reqs
                .entry(test_id.to_string())
                .or_default()
                .insert(req_id.to_string());
        }
    }

    /// Update requirement status
    pub fn update_status(&mut self, req_id: &str, status: RequirementStatus) {
        if let Some(req) = self.requirements.get_mut(req_id) {
            req.status = status;
        }
    }

    /// Get requirement by ID
    pub fn get_requirement(&self, id: &str) -> Option<&Requirement> {
        self.requirements.get(id)
    }

    /// Get all child requirements
    pub fn get_children(&self, id: &str) -> Vec<&Requirement> {
        self.hierarchy
            .children
            .get(id)
            .map(|children| {
                children
                    .iter()
                    .filter_map(|child_id| self.requirements.get(child_id))
                    .collect()
            })
            .unwrap_or_default()
    }

    /// Get requirements by property
    pub fn get_requirements_by_property(&self, property_id: &str) -> Vec<&Requirement> {
        self.coverage_matrix
            .property_to_reqs
            .get(property_id)
            .map(|req_ids| {
                req_ids
                    .iter()
                    .filter_map(|id| self.requirements.get(id))
                    .collect()
            })
            .unwrap_or_default()
    }

    /// Get requirements by test
    pub fn get_requirements_by_test(&self, test_id: &str) -> Vec<&Requirement> {
        self.coverage_matrix
            .test_to_reqs
            .get(test_id)
            .map(|req_ids| {
                req_ids
                    .iter()
                    .filter_map(|id| self.requirements.get(id))
                    .collect()
            })
            .unwrap_or_default()
    }

    /// Check requirement coverage
    pub fn check_coverage(&self, req_id: &str) -> RequirementCoverage {
        if let Some(req) = self.requirements.get(req_id) {
            let total_criteria = req.criteria.len();
            let passed_criteria = req
                .criteria
                .iter()
                .filter(|c| matches!(c.status, CriterionStatus::Passed))
                .count();

            let properties_covered = !req.properties.is_empty();
            let tests_covered = !req.tests.is_empty();

            RequirementCoverage {
                requirement_id: req_id.to_string(),
                total_criteria,
                passed_criteria,
                properties_covered,
                tests_covered,
                coverage_percentage: if total_criteria > 0 {
                    100.0 * passed_criteria as f64 / total_criteria as f64
                } else {
                    0.0
                },
            }
        } else {
            RequirementCoverage {
                requirement_id: req_id.to_string(),
                total_criteria: 0,
                passed_criteria: 0,
                properties_covered: false,
                tests_covered: false,
                coverage_percentage: 0.0,
            }
        }
    }

    /// Generate verification report
    pub fn generate_report(&self) -> VerificationReport {
        let total = self.requirements.len();
        let mut by_status = HashMap::new();
        let mut by_priority = HashMap::new();
        let mut by_type = HashMap::new();

        for req in self.requirements.values() {
            *by_status.entry(req.status).or_insert(0) += 1;
            *by_priority.entry(req.priority).or_insert(0) += 1;
            *by_type.entry(req.req_type).or_insert(0) += 1;
        }

        let verified = *by_status.get(&RequirementStatus::Verified).unwrap_or(&0)
            + *by_status.get(&RequirementStatus::Proven).unwrap_or(&0);

        let coverage = if total > 0 {
            100.0 * verified as f64 / total as f64
        } else {
            0.0
        };

        VerificationReport {
            total_requirements: total,
            by_status,
            by_priority,
            by_type,
            coverage_percentage: coverage,
            critical_unverified: self.get_critical_unverified(),
        }
    }

    /// Get critical unverified requirements
    fn get_critical_unverified(&self) -> Vec<String> {
        self.requirements
            .values()
            .filter(|req| {
                matches!(req.priority, Priority::Critical)
                    && !matches!(
                        req.status,
                        RequirementStatus::Verified | RequirementStatus::Proven
                    )
            })
            .map(|req| req.id.clone())
            .collect()
    }

    /// Generate traceability report
    pub fn generate_traceability_report(&self) -> TraceabilityReport {
        let mut orphan_requirements = Vec::new();
        let mut untested_requirements = Vec::new();
        let unlinked_properties = HashSet::new();
        let unlinked_tests = HashSet::new();

        for req in self.requirements.values() {
            if req.parent.is_none() && req.children.is_empty() {
                orphan_requirements.push(req.id.clone());
            }
            if req.tests.is_empty() {
                untested_requirements.push(req.id.clone());
            }
        }

        TraceabilityReport {
            orphan_requirements,
            untested_requirements,
            unlinked_properties: unlinked_properties.into_iter().collect(),
            unlinked_tests: unlinked_tests.into_iter().collect(),
        }
    }
}

/// Requirement coverage information
#[derive(Debug, Clone)]
pub struct RequirementCoverage {
    /// Requirement ID
    pub requirement_id: String,

    /// Total verification criteria
    pub total_criteria: usize,

    /// Passed criteria
    pub passed_criteria: usize,

    /// Has property coverage
    pub properties_covered: bool,

    /// Has test coverage
    pub tests_covered: bool,

    /// Coverage percentage
    pub coverage_percentage: f64,
}

/// Verification report
#[derive(Debug)]
pub struct VerificationReport {
    /// Total requirements
    pub total_requirements: usize,

    /// Requirements by status
    pub by_status: HashMap<RequirementStatus, usize>,

    /// Requirements by priority
    pub by_priority: HashMap<Priority, usize>,

    /// Requirements by type
    pub by_type: HashMap<RequirementType, usize>,

    /// Overall coverage percentage
    pub coverage_percentage: f64,

    /// Critical unverified requirements
    pub critical_unverified: Vec<String>,
}

/// Traceability report
#[derive(Debug)]
pub struct TraceabilityReport {
    /// Orphan requirements (no parent or children)
    pub orphan_requirements: Vec<String>,

    /// Requirements without tests
    pub untested_requirements: Vec<String>,

    /// Properties not linked to requirements
    pub unlinked_properties: Vec<String>,

    /// Tests not linked to requirements
    pub unlinked_tests: Vec<String>,
}

impl fmt::Display for VerificationReport {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "=== Verification Report ===")?;
        writeln!(f, "Total Requirements: {}", self.total_requirements)?;
        writeln!(f, "Coverage: {:.1}%", self.coverage_percentage)?;
        writeln!(f)?;

        writeln!(f, "By Status:")?;
        for (status, count) in &self.by_status {
            writeln!(f, "  {:?}: {}", status, count)?;
        }
        writeln!(f)?;

        writeln!(f, "By Priority:")?;
        for (priority, count) in &self.by_priority {
            writeln!(f, "  {:?}: {}", priority, count)?;
        }

        if !self.critical_unverified.is_empty() {
            writeln!(f)?;
            writeln!(f, "Critical Unverified Requirements:")?;
            for id in &self.critical_unverified {
                writeln!(f, "  - {}", id)?;
            }
        }

        Ok(())
    }
}

impl fmt::Display for TraceabilityReport {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "=== Traceability Report ===")?;

        if !self.orphan_requirements.is_empty() {
            writeln!(f, "Orphan Requirements:")?;
            for id in &self.orphan_requirements {
                writeln!(f, "  - {}", id)?;
            }
        }

        if !self.untested_requirements.is_empty() {
            writeln!(f, "Untested Requirements:")?;
            for id in &self.untested_requirements {
                writeln!(f, "  - {}", id)?;
            }
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_requirement_tracker_new() {
        let tracker = RequirementTracker::new();
        assert_eq!(tracker.requirements.len(), 0);
        assert_eq!(tracker.hierarchy.roots.len(), 0);
    }

    #[test]
    fn test_add_requirement() {
        let mut tracker = RequirementTracker::new();

        let req = Requirement {
            id: "REQ-001".to_string(),
            description: "Test requirement".to_string(),
            priority: Priority::High,
            req_type: RequirementType::Functional,
            parent: None,
            children: vec![],
            criteria: vec![],
            properties: vec!["prop1".to_string()],
            tests: vec!["test1".to_string()],
            status: RequirementStatus::NotImplemented,
            notes: vec![],
        };

        tracker.add_requirement(req);

        assert_eq!(tracker.requirements.len(), 1);
        assert_eq!(tracker.hierarchy.roots.len(), 1);
        assert!(tracker.requirements.contains_key("REQ-001"));
    }

    #[test]
    fn test_requirement_hierarchy() {
        let mut tracker = RequirementTracker::new();

        let parent = Requirement {
            id: "REQ-100".to_string(),
            description: "Parent requirement".to_string(),
            priority: Priority::Critical,
            req_type: RequirementType::Functional,
            parent: None,
            children: vec!["REQ-101".to_string()],
            criteria: vec![],
            properties: vec![],
            tests: vec![],
            status: RequirementStatus::InProgress,
            notes: vec![],
        };

        let child = Requirement {
            id: "REQ-101".to_string(),
            description: "Child requirement".to_string(),
            priority: Priority::Medium,
            req_type: RequirementType::Functional,
            parent: Some("REQ-100".to_string()),
            children: vec![],
            criteria: vec![],
            properties: vec![],
            tests: vec![],
            status: RequirementStatus::Verified,
            notes: vec![],
        };

        tracker.add_requirement(parent);
        tracker.add_requirement(child);

        // Check hierarchy
        let children = tracker.get_children("REQ-100");
        assert_eq!(children.len(), 1);
        assert_eq!(children[0].id, "REQ-101");

        // Root should only contain parent
        assert_eq!(tracker.hierarchy.roots.len(), 1);
        assert_eq!(tracker.hierarchy.roots[0], "REQ-100");
    }

    #[test]
    fn test_link_property() {
        let mut tracker = RequirementTracker::new();

        let req = Requirement {
            id: "REQ-001".to_string(),
            description: "Test requirement".to_string(),
            priority: Priority::High,
            req_type: RequirementType::Functional,
            parent: None,
            children: vec![],
            criteria: vec![],
            properties: vec![],
            tests: vec![],
            status: RequirementStatus::NotImplemented,
            notes: vec![],
        };

        tracker.add_requirement(req);
        tracker.link_property("REQ-001", "safety_prop");

        let req = tracker.get_requirement("REQ-001").unwrap();
        assert!(req.properties.contains(&"safety_prop".to_string()));

        let reqs = tracker.get_requirements_by_property("safety_prop");
        assert_eq!(reqs.len(), 1);
        assert_eq!(reqs[0].id, "REQ-001");
    }

    #[test]
    fn test_requirement_coverage() {
        let mut tracker = RequirementTracker::new();

        let req = Requirement {
            id: "REQ-001".to_string(),
            description: "Test requirement".to_string(),
            priority: Priority::High,
            req_type: RequirementType::Functional,
            parent: None,
            children: vec![],
            criteria: vec![
                VerificationCriterion {
                    description: "Criterion 1".to_string(),
                    method: VerificationMethod::Simulation,
                    pass_criteria: "Test passes".to_string(),
                    status: CriterionStatus::Passed,
                },
                VerificationCriterion {
                    description: "Criterion 2".to_string(),
                    method: VerificationMethod::Formal,
                    pass_criteria: "Property proven".to_string(),
                    status: CriterionStatus::NotTested,
                },
            ],
            properties: vec!["prop1".to_string()],
            tests: vec!["test1".to_string()],
            status: RequirementStatus::NotImplemented,
            notes: vec![],
        };

        tracker.add_requirement(req);

        let coverage = tracker.check_coverage("REQ-001");
        assert_eq!(coverage.total_criteria, 2);
        assert_eq!(coverage.passed_criteria, 1);
        assert_eq!(coverage.coverage_percentage, 50.0);
        assert!(coverage.properties_covered);
        assert!(coverage.tests_covered);
    }

    #[test]
    fn test_verification_report() {
        let mut tracker = RequirementTracker::new();

        // Add requirements with different statuses
        for i in 0..3 {
            let req = Requirement {
                id: format!("REQ-{:03}", i),
                description: format!("Requirement {}", i),
                priority: if i == 0 {
                    Priority::Critical
                } else {
                    Priority::Medium
                },
                req_type: RequirementType::Functional,
                parent: None,
                children: vec![],
                criteria: vec![],
                properties: vec![],
                tests: vec![],
                status: if i < 2 {
                    RequirementStatus::Verified
                } else {
                    RequirementStatus::NotImplemented
                },
                notes: vec![],
            };
            tracker.add_requirement(req);
        }

        let report = tracker.generate_report();
        assert_eq!(report.total_requirements, 3);
        assert_eq!(report.coverage_percentage, 66.66666666666667); // 2 out of 3 verified
        assert_eq!(
            *report.by_status.get(&RequirementStatus::Verified).unwrap(),
            2
        );
        assert_eq!(
            *report
                .by_status
                .get(&RequirementStatus::NotImplemented)
                .unwrap(),
            1
        );
    }

    #[test]
    fn test_update_status() {
        let mut tracker = RequirementTracker::new();

        let req = Requirement {
            id: "REQ-001".to_string(),
            description: "Test requirement".to_string(),
            priority: Priority::High,
            req_type: RequirementType::Functional,
            parent: None,
            children: vec![],
            criteria: vec![],
            properties: vec![],
            tests: vec![],
            status: RequirementStatus::NotImplemented,
            notes: vec![],
        };

        tracker.add_requirement(req);
        tracker.update_status("REQ-001", RequirementStatus::Verified);

        let req = tracker.get_requirement("REQ-001").unwrap();
        assert_eq!(req.status, RequirementStatus::Verified);
    }

    #[test]
    fn test_traceability_report() {
        let mut tracker = RequirementTracker::new();

        // Add orphan requirement (no parent or children)
        let orphan = Requirement {
            id: "REQ-ORPHAN".to_string(),
            description: "Orphan requirement".to_string(),
            priority: Priority::Low,
            req_type: RequirementType::Functional,
            parent: None,
            children: vec![],
            criteria: vec![],
            properties: vec![],
            tests: vec![], // No tests
            status: RequirementStatus::NotImplemented,
            notes: vec![],
        };

        tracker.add_requirement(orphan);

        let traceability = tracker.generate_traceability_report();
        assert_eq!(traceability.orphan_requirements.len(), 1);
        assert_eq!(traceability.untested_requirements.len(), 1);
        assert_eq!(traceability.orphan_requirements[0], "REQ-ORPHAN");
        assert_eq!(traceability.untested_requirements[0], "REQ-ORPHAN");
    }
}
