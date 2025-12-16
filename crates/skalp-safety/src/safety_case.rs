//! Safety Case Generation using Goal Structuring Notation (GSN)
//!
//! This module generates safety cases following GSN notation as defined in
//! the GSN Community Standard v3. Safety cases provide structured arguments
//! showing that a system is acceptably safe.
//!
//! # GSN Elements
//!
//! - **Goal** - A claim about the system to be supported
//! - **Strategy** - The approach used to break down a goal
//! - **Solution** - A reference to evidence supporting a goal
//! - **Context** - Background information or scope
//! - **Assumption** - A statement taken to be true
//! - **Justification** - Rationale for the argument approach
//!
//! # Example
//!
//! ```ignore
//! use skalp_safety::safety_case::{GsnDiagram, GsnElement, generate_safety_case};
//!
//! let gsn = generate_safety_case(&hierarchy, &metrics, &fta);
//! let yaml = gsn.to_yaml();
//! let dot = gsn.to_graphviz();
//! ```

use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

use crate::asil::AsilLevel;
use crate::fta::CutSetAnalysis;
use crate::hierarchy::SafetyHierarchy;
use crate::metrics::HardwareArchitecturalMetrics;

// ============================================================================
// GSN Element Types
// ============================================================================

/// GSN element identifier
pub type GsnId = String;

/// GSN element types per GSN Community Standard v3
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum GsnElement {
    /// Goal - A claim about the system to be supported
    Goal {
        /// Unique identifier (e.g., "G1", "G2.1")
        id: GsnId,
        /// Goal description
        description: String,
        /// Whether this goal is undeveloped
        undeveloped: bool,
        /// ASIL level if applicable
        asil: Option<AsilLevel>,
    },
    /// Strategy - The approach used to break down a goal
    Strategy {
        /// Unique identifier (e.g., "S1", "S2")
        id: GsnId,
        /// Strategy description
        description: String,
    },
    /// Solution - Reference to evidence supporting a goal
    Solution {
        /// Unique identifier (e.g., "Sn1", "Sn2")
        id: GsnId,
        /// Evidence reference
        evidence: String,
        /// Document/artifact reference
        artifact: Option<String>,
    },
    /// Context - Background information or scope
    Context {
        /// Unique identifier (e.g., "C1", "C2")
        id: GsnId,
        /// Context description
        description: String,
    },
    /// Assumption - A statement taken to be true
    Assumption {
        /// Unique identifier (e.g., "A1", "A2")
        id: GsnId,
        /// Assumption description
        description: String,
        /// Whether this assumption has been validated
        validated: bool,
    },
    /// Justification - Rationale for the argument approach
    Justification {
        /// Unique identifier (e.g., "J1", "J2")
        id: GsnId,
        /// Justification description
        description: String,
    },
}

impl GsnElement {
    /// Get the element's ID
    pub fn id(&self) -> &str {
        match self {
            GsnElement::Goal { id, .. } => id,
            GsnElement::Strategy { id, .. } => id,
            GsnElement::Solution { id, .. } => id,
            GsnElement::Context { id, .. } => id,
            GsnElement::Assumption { id, .. } => id,
            GsnElement::Justification { id, .. } => id,
        }
    }

    /// Get the element type name
    pub fn type_name(&self) -> &'static str {
        match self {
            GsnElement::Goal { .. } => "Goal",
            GsnElement::Strategy { .. } => "Strategy",
            GsnElement::Solution { .. } => "Solution",
            GsnElement::Context { .. } => "Context",
            GsnElement::Assumption { .. } => "Assumption",
            GsnElement::Justification { .. } => "Justification",
        }
    }

    /// Create a new goal element
    pub fn goal(id: &str, description: &str) -> Self {
        GsnElement::Goal {
            id: id.to_string(),
            description: description.to_string(),
            undeveloped: false,
            asil: None,
        }
    }

    /// Create a new undeveloped goal
    pub fn undeveloped_goal(id: &str, description: &str) -> Self {
        GsnElement::Goal {
            id: id.to_string(),
            description: description.to_string(),
            undeveloped: true,
            asil: None,
        }
    }

    /// Create a new strategy element
    pub fn strategy(id: &str, description: &str) -> Self {
        GsnElement::Strategy {
            id: id.to_string(),
            description: description.to_string(),
        }
    }

    /// Create a new solution element
    pub fn solution(id: &str, evidence: &str) -> Self {
        GsnElement::Solution {
            id: id.to_string(),
            evidence: evidence.to_string(),
            artifact: None,
        }
    }

    /// Create a new context element
    pub fn context(id: &str, description: &str) -> Self {
        GsnElement::Context {
            id: id.to_string(),
            description: description.to_string(),
        }
    }

    /// Create a new assumption element
    pub fn assumption(id: &str, description: &str) -> Self {
        GsnElement::Assumption {
            id: id.to_string(),
            description: description.to_string(),
            validated: false,
        }
    }

    /// Create a new justification element
    pub fn justification(id: &str, description: &str) -> Self {
        GsnElement::Justification {
            id: id.to_string(),
            description: description.to_string(),
        }
    }
}

/// Link types between GSN elements
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum GsnLinkType {
    /// "Supported by" relationship (goal to strategy/goal/solution)
    SupportedBy,
    /// "In context of" relationship (element to context/assumption)
    InContextOf,
}

/// Link between two GSN elements
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GsnLink {
    /// Source element ID
    pub from: GsnId,
    /// Target element ID
    pub to: GsnId,
    /// Link type
    pub link_type: GsnLinkType,
}

impl GsnLink {
    /// Create a "supported by" link
    pub fn supported_by(from: &str, to: &str) -> Self {
        Self {
            from: from.to_string(),
            to: to.to_string(),
            link_type: GsnLinkType::SupportedBy,
        }
    }

    /// Create an "in context of" link
    pub fn in_context_of(from: &str, to: &str) -> Self {
        Self {
            from: from.to_string(),
            to: to.to_string(),
            link_type: GsnLinkType::InContextOf,
        }
    }
}

// ============================================================================
// GSN Diagram
// ============================================================================

/// Complete GSN diagram containing all elements and links
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GsnDiagram {
    /// Diagram title
    pub title: String,
    /// Diagram description
    pub description: String,
    /// Design/system being analyzed
    pub system_name: String,
    /// Target ASIL level
    pub target_asil: AsilLevel,
    /// All GSN elements indexed by ID
    pub elements: HashMap<GsnId, GsnElement>,
    /// Links between elements
    pub links: Vec<GsnLink>,
    /// Top-level goal ID
    pub top_goal: Option<GsnId>,
    /// Metadata
    pub metadata: GsnMetadata,
}

/// Diagram metadata
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GsnMetadata {
    /// Author
    pub author: String,
    /// Creation date
    pub created_at: DateTime<Utc>,
    /// Version
    pub version: String,
    /// Status
    pub status: GsnStatus,
}

impl Default for GsnMetadata {
    fn default() -> Self {
        Self {
            author: "SKALP Safety Analysis".to_string(),
            created_at: Utc::now(),
            version: "1.0.0".to_string(),
            status: GsnStatus::Draft,
        }
    }
}

/// Diagram status
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum GsnStatus {
    /// Draft - under development
    Draft,
    /// Review - ready for review
    Review,
    /// Approved - accepted
    Approved,
    /// Released - final
    Released,
}

impl GsnDiagram {
    /// Create a new empty GSN diagram
    pub fn new(title: &str, system_name: &str, target_asil: AsilLevel) -> Self {
        Self {
            title: title.to_string(),
            description: String::new(),
            system_name: system_name.to_string(),
            target_asil,
            elements: HashMap::new(),
            links: Vec::new(),
            top_goal: None,
            metadata: GsnMetadata::default(),
        }
    }

    /// Add an element to the diagram
    pub fn add_element(&mut self, element: GsnElement) {
        let id = element.id().to_string();
        self.elements.insert(id, element);
    }

    /// Add a link between elements
    pub fn add_link(&mut self, link: GsnLink) {
        self.links.push(link);
    }

    /// Set the top-level goal
    pub fn set_top_goal(&mut self, id: &str) {
        self.top_goal = Some(id.to_string());
    }

    /// Get an element by ID
    pub fn get_element(&self, id: &str) -> Option<&GsnElement> {
        self.elements.get(id)
    }

    /// Get all goals
    pub fn goals(&self) -> Vec<&GsnElement> {
        self.elements
            .values()
            .filter(|e| matches!(e, GsnElement::Goal { .. }))
            .collect()
    }

    /// Get all strategies
    pub fn strategies(&self) -> Vec<&GsnElement> {
        self.elements
            .values()
            .filter(|e| matches!(e, GsnElement::Strategy { .. }))
            .collect()
    }

    /// Get all solutions
    pub fn solutions(&self) -> Vec<&GsnElement> {
        self.elements
            .values()
            .filter(|e| matches!(e, GsnElement::Solution { .. }))
            .collect()
    }

    /// Check if the diagram is complete (no undeveloped goals)
    pub fn is_complete(&self) -> bool {
        !self.elements.values().any(|e| {
            matches!(
                e,
                GsnElement::Goal {
                    undeveloped: true,
                    ..
                }
            )
        })
    }

    /// Get undeveloped goals
    pub fn undeveloped_goals(&self) -> Vec<&GsnElement> {
        self.elements
            .values()
            .filter(|e| {
                matches!(
                    e,
                    GsnElement::Goal {
                        undeveloped: true,
                        ..
                    }
                )
            })
            .collect()
    }

    /// Export to YAML format
    pub fn to_yaml(&self) -> String {
        let mut output = String::new();
        output.push_str("---\n");
        output.push_str(&format!("title: \"{}\"\n", self.title));
        output.push_str(&format!("system_name: \"{}\"\n", self.system_name));
        output.push_str(&format!("target_asil: {:?}\n", self.target_asil));
        output.push_str(&format!("status: {:?}\n", self.metadata.status));
        output.push_str(&format!("version: \"{}\"\n", self.metadata.version));
        output.push_str("\nelements:\n");
        for (id, element) in &self.elements {
            output.push_str(&format!("  {}:\n", id));
            output.push_str(&format!("    type: {}\n", element.type_name()));
            match element {
                GsnElement::Goal {
                    description,
                    undeveloped,
                    asil,
                    ..
                } => {
                    output.push_str(&format!("    description: \"{}\"\n", description));
                    output.push_str(&format!("    undeveloped: {}\n", undeveloped));
                    if let Some(a) = asil {
                        output.push_str(&format!("    asil: {:?}\n", a));
                    }
                }
                GsnElement::Strategy { description, .. } => {
                    output.push_str(&format!("    description: \"{}\"\n", description));
                }
                GsnElement::Solution {
                    evidence, artifact, ..
                } => {
                    output.push_str(&format!("    evidence: \"{}\"\n", evidence));
                    if let Some(a) = artifact {
                        output.push_str(&format!("    artifact: \"{}\"\n", a));
                    }
                }
                GsnElement::Context { description, .. } => {
                    output.push_str(&format!("    description: \"{}\"\n", description));
                }
                GsnElement::Assumption {
                    description,
                    validated,
                    ..
                } => {
                    output.push_str(&format!("    description: \"{}\"\n", description));
                    output.push_str(&format!("    validated: {}\n", validated));
                }
                GsnElement::Justification { description, .. } => {
                    output.push_str(&format!("    description: \"{}\"\n", description));
                }
            }
        }
        output.push_str("\nlinks:\n");
        for link in &self.links {
            output.push_str(&format!("  - from: \"{}\"\n", link.from));
            output.push_str(&format!("    to: \"{}\"\n", link.to));
            output.push_str(&format!("    type: {:?}\n", link.link_type));
        }
        output
    }

    /// Export to JSON format
    pub fn to_json(&self) -> String {
        let mut output = String::new();
        output.push_str("{\n");
        output.push_str(&format!("  \"title\": \"{}\",\n", self.title));
        output.push_str(&format!("  \"system_name\": \"{}\",\n", self.system_name));
        output.push_str(&format!("  \"target_asil\": \"{:?}\",\n", self.target_asil));
        output.push_str(&format!("  \"status\": \"{:?}\",\n", self.metadata.status));
        output.push_str(&format!("  \"version\": \"{}\",\n", self.metadata.version));
        output.push_str("  \"elements\": [\n");
        let elements: Vec<_> = self.elements.values().collect();
        for (i, element) in elements.iter().enumerate() {
            output.push_str("    {\n");
            output.push_str(&format!("      \"id\": \"{}\",\n", element.id()));
            output.push_str(&format!("      \"type\": \"{}\"\n", element.type_name()));
            output.push_str("    }");
            if i < elements.len() - 1 {
                output.push(',');
            }
            output.push('\n');
        }
        output.push_str("  ],\n");
        output.push_str(&format!("  \"link_count\": {}\n", self.links.len()));
        output.push_str("}\n");
        output
    }

    /// Export to Graphviz DOT format for visualization
    pub fn to_graphviz(&self) -> String {
        let mut output = String::new();

        output.push_str("digraph GSN {\n");
        output.push_str("  rankdir=TB;\n");
        output.push_str("  node [fontname=\"Helvetica\"];\n");
        output.push_str("  edge [fontname=\"Helvetica\"];\n\n");

        // Title
        output.push_str(&format!(
            "  labelloc=\"t\";\n  label=\"{}\\n{} - ASIL {:?}\";\n\n",
            self.title, self.system_name, self.target_asil
        ));

        // Define node shapes and styles for each element type
        for element in self.elements.values() {
            let (shape, style, fillcolor, label) = match element {
                GsnElement::Goal {
                    id,
                    description,
                    undeveloped,
                    asil,
                } => {
                    let style = if *undeveloped {
                        "filled,dashed"
                    } else {
                        "filled"
                    };
                    let asil_str = asil.map(|a| format!(" [{:?}]", a)).unwrap_or_default();
                    (
                        "rectangle",
                        style,
                        "#B0E0E6",
                        format!("{}{}\\n{}", id, asil_str, truncate_text(description, 40)),
                    )
                }
                GsnElement::Strategy { id, description } => (
                    "parallelogram",
                    "filled",
                    "#90EE90",
                    format!("{}\\n{}", id, truncate_text(description, 40)),
                ),
                GsnElement::Solution { id, evidence, .. } => (
                    "circle",
                    "filled",
                    "#FFD700",
                    format!("{}\\n{}", id, truncate_text(evidence, 30)),
                ),
                GsnElement::Context { id, description } => (
                    "rectangle",
                    "filled,rounded",
                    "#FFA07A",
                    format!("{}\\n{}", id, truncate_text(description, 30)),
                ),
                GsnElement::Assumption {
                    id, description, ..
                } => (
                    "ellipse",
                    "filled",
                    "#DDA0DD",
                    format!("{}\\nAsm: {}", id, truncate_text(description, 30)),
                ),
                GsnElement::Justification { id, description } => (
                    "ellipse",
                    "filled",
                    "#87CEEB",
                    format!("{}\\nJust: {}", id, truncate_text(description, 30)),
                ),
            };

            output.push_str(&format!(
                "  \"{}\" [shape={}, style=\"{}\", fillcolor=\"{}\", label=\"{}\"];\n",
                element.id(),
                shape,
                style,
                fillcolor,
                label
            ));
        }

        output.push('\n');

        // Define edges
        for link in &self.links {
            let style = match link.link_type {
                GsnLinkType::SupportedBy => "solid",
                GsnLinkType::InContextOf => "dashed",
            };
            let arrowhead = match link.link_type {
                GsnLinkType::SupportedBy => "normal",
                GsnLinkType::InContextOf => "empty",
            };
            output.push_str(&format!(
                "  \"{}\" -> \"{}\" [style={}, arrowhead={}];\n",
                link.from, link.to, style, arrowhead
            ));
        }

        output.push_str("}\n");
        output
    }

    /// Export to Markdown format
    pub fn to_markdown(&self) -> String {
        let mut output = String::new();

        output.push_str(&format!("# {}\n\n", self.title));
        output.push_str(&format!(
            "**System:** {} | **ASIL:** {:?} | **Status:** {:?}\n\n",
            self.system_name, self.target_asil, self.metadata.status
        ));

        if !self.description.is_empty() {
            output.push_str(&format!("{}\n\n", self.description));
        }

        // Goals
        output.push_str("## Goals\n\n");
        for element in self.goals() {
            if let GsnElement::Goal {
                id,
                description,
                undeveloped,
                asil,
            } = element
            {
                let status = if *undeveloped {
                    "🔴 Undeveloped"
                } else {
                    "✅"
                };
                let asil_str = asil.map(|a| format!(" [ASIL {:?}]", a)).unwrap_or_default();
                output.push_str(&format!(
                    "- **{}**{}: {} {}\n",
                    id, asil_str, description, status
                ));
            }
        }
        output.push('\n');

        // Strategies
        if !self.strategies().is_empty() {
            output.push_str("## Strategies\n\n");
            for element in self.strategies() {
                if let GsnElement::Strategy { id, description } = element {
                    output.push_str(&format!("- **{}**: {}\n", id, description));
                }
            }
            output.push('\n');
        }

        // Solutions/Evidence
        if !self.solutions().is_empty() {
            output.push_str("## Evidence\n\n");
            for element in self.solutions() {
                if let GsnElement::Solution {
                    id,
                    evidence,
                    artifact,
                } = element
                {
                    let artifact_str = artifact
                        .as_ref()
                        .map(|a| format!(" → {}", a))
                        .unwrap_or_default();
                    output.push_str(&format!("- **{}**: {}{}\n", id, evidence, artifact_str));
                }
            }
            output.push('\n');
        }

        // Diagram completeness
        output.push_str("## Status\n\n");
        if self.is_complete() {
            output.push_str("✅ All goals are developed and supported by evidence.\n");
        } else {
            output.push_str("⚠️ The following goals require development:\n");
            for goal in self.undeveloped_goals() {
                output.push_str(&format!("- {}\n", goal.id()));
            }
        }

        output
    }
}

/// Helper to truncate text for diagram display
fn truncate_text(text: &str, max_len: usize) -> String {
    if text.len() <= max_len {
        text.to_string()
    } else {
        format!("{}...", &text[..max_len - 3])
    }
}

// ============================================================================
// Safety Case Generation
// ============================================================================

/// Generate a safety case from analysis results
///
/// Creates a GSN diagram arguing that the design meets ISO 26262
/// hardware safety requirements for the target ASIL level.
pub fn generate_safety_case(
    hierarchy: &SafetyHierarchy,
    metrics: &HardwareArchitecturalMetrics,
    fta: Option<&CutSetAnalysis>,
) -> GsnDiagram {
    let target_asil = hierarchy
        .goals
        .values()
        .map(|sg| sg.asil)
        .max()
        .unwrap_or(AsilLevel::QM);

    let design_name = hierarchy
        .goals
        .values()
        .next()
        .map(|sg| sg.name.as_str())
        .unwrap_or("Design")
        .to_string();

    let mut gsn = GsnDiagram::new("Hardware Safety Case", &design_name, target_asil);

    gsn.description = format!(
        "Safety case for {} demonstrating compliance with ISO 26262 ASIL {:?} requirements.",
        design_name, target_asil
    );

    // Top-level goal: System is acceptably safe
    let g1 = GsnElement::Goal {
        id: "G1".to_string(),
        description: format!(
            "{} hardware is acceptably safe for ASIL {:?} application",
            design_name, target_asil
        ),
        undeveloped: false,
        asil: Some(target_asil),
    };
    gsn.add_element(g1);
    gsn.set_top_goal("G1");

    // Context: ISO 26262 standard
    gsn.add_element(GsnElement::context(
        "C1",
        "ISO 26262:2018 Part 5 - Hardware Development",
    ));
    gsn.add_link(GsnLink::in_context_of("G1", "C1"));

    // Context: Target ASIL
    gsn.add_element(GsnElement::context(
        "C2",
        &format!("Target integrity level: ASIL {:?}", target_asil),
    ));
    gsn.add_link(GsnLink::in_context_of("G1", "C2"));

    // Strategy: Argue over ISO 26262 Part 5 requirements
    gsn.add_element(GsnElement::strategy(
        "S1",
        "Argument over ISO 26262-5 hardware architectural metrics and safety mechanisms",
    ));
    gsn.add_link(GsnLink::supported_by("G1", "S1"));

    // Sub-goal: SPFM met
    let spfm_met = metrics.spfm >= get_spfm_target(target_asil);
    let g2 = GsnElement::Goal {
        id: "G2".to_string(),
        description: format!(
            "Single Point Fault Metric (SPFM) ≥ {:.0}% achieved: {:.2}%",
            get_spfm_target(target_asil),
            metrics.spfm
        ),
        undeveloped: !spfm_met,
        asil: Some(target_asil),
    };
    gsn.add_element(g2);
    gsn.add_link(GsnLink::supported_by("S1", "G2"));

    if spfm_met {
        gsn.add_element(GsnElement::solution(
            "Sn1",
            &format!("FMEA analysis shows SPFM = {:.2}%", metrics.spfm),
        ));
        gsn.add_link(GsnLink::supported_by("G2", "Sn1"));
    }

    // Sub-goal: LFM met
    let lfm_met = metrics.lf >= get_lfm_target(target_asil);
    let g3 = GsnElement::Goal {
        id: "G3".to_string(),
        description: format!(
            "Latent Fault Metric (LFM) ≥ {:.0}% achieved: {:.2}%",
            get_lfm_target(target_asil),
            metrics.lf
        ),
        undeveloped: !lfm_met,
        asil: Some(target_asil),
    };
    gsn.add_element(g3);
    gsn.add_link(GsnLink::supported_by("S1", "G3"));

    if lfm_met {
        gsn.add_element(GsnElement::solution(
            "Sn2",
            &format!("FMEA analysis shows LFM = {:.2}%", metrics.lf),
        ));
        gsn.add_link(GsnLink::supported_by("G3", "Sn2"));
    }

    // Sub-goal: PMHF met
    let pmhf_target = get_pmhf_target(target_asil);
    let pmhf_met = metrics.pmhf <= pmhf_target;
    let g4 = GsnElement::Goal {
        id: "G4".to_string(),
        description: format!(
            "PMHF < {:.0} FIT achieved: {:.4} FIT",
            pmhf_target, metrics.pmhf
        ),
        undeveloped: !pmhf_met,
        asil: Some(target_asil),
    };
    gsn.add_element(g4);
    gsn.add_link(GsnLink::supported_by("S1", "G4"));

    if pmhf_met {
        gsn.add_element(GsnElement::solution(
            "Sn3",
            &format!(
                "PMHF calculation: {:.4} FIT (target: < {:.0} FIT)",
                metrics.pmhf, pmhf_target
            ),
        ));
        gsn.add_link(GsnLink::supported_by("G4", "Sn3"));
    }

    // Sub-goal: Safety mechanisms effective
    gsn.add_element(GsnElement::goal(
        "G5",
        "Safety mechanisms are effective for fault detection and mitigation",
    ));
    gsn.add_link(GsnLink::supported_by("S1", "G5"));

    // Strategy for safety mechanisms
    gsn.add_element(GsnElement::strategy(
        "S2",
        "Argument over individual safety mechanism coverage",
    ));
    gsn.add_link(GsnLink::supported_by("G5", "S2"));

    // Add goals for each safety mechanism from goals' HSRs' PSMs
    let mut sm_idx = 1;
    for (goal_id, goal) in &hierarchy.goals {
        for hsr in &goal.hsrs {
            if let Some(psm) = &hsr.psm {
                let sm_goal_id = format!("G5.{}", sm_idx);
                let dc = psm.dc_target; // Target diagnostic coverage
                let sm_goal = GsnElement::Goal {
                    id: sm_goal_id.clone(),
                    description: format!(
                        "{} targets {:.0}% diagnostic coverage for {}",
                        psm.name, dc, hsr.id
                    ),
                    undeveloped: dc < 60.0,
                    asil: None,
                };
                gsn.add_element(sm_goal);
                gsn.add_link(GsnLink::supported_by("S2", &sm_goal_id));

                if dc >= 60.0 {
                    let sn_id = format!("Sn5.{}", sm_idx);
                    gsn.add_element(GsnElement::solution(
                        &sn_id,
                        &format!(
                            "SM verification report for {} ({}/{})",
                            psm.name, goal_id, hsr.id
                        ),
                    ));
                    gsn.add_link(GsnLink::supported_by(&sm_goal_id, &sn_id));
                }

                sm_idx += 1;
            }
        }
    }

    // FTA results if available
    if let Some(fta_results) = fta {
        gsn.add_element(GsnElement::goal(
            "G6",
            "Fault Tree Analysis confirms minimal cut set safety",
        ));
        gsn.add_link(GsnLink::supported_by("S1", "G6"));

        let single_order = fta_results
            .cut_sets
            .iter()
            .filter(|cs| cs.order == 1)
            .count();
        let is_safe = single_order == 0 || fta_results.top_event_probability < 1e-7;

        if is_safe {
            gsn.add_element(GsnElement::solution(
                "Sn6",
                &format!(
                    "FTA shows {} minimal cut sets, top event probability: {:.2e}",
                    fta_results.cut_sets.len(),
                    fta_results.top_event_probability
                ),
            ));
            gsn.add_link(GsnLink::supported_by("G6", "Sn6"));
        } else {
            // Mark G6 as undeveloped if FTA shows issues
            if let Some(GsnElement::Goal { undeveloped, .. }) = gsn.elements.get_mut("G6") {
                *undeveloped = true;
            }
        }
    }

    // Assumptions
    gsn.add_element(GsnElement::assumption(
        "A1",
        "Hardware is operated within specified environmental conditions",
    ));
    gsn.add_link(GsnLink::in_context_of("G1", "A1"));

    gsn.add_element(GsnElement::assumption(
        "A2",
        "Systematic faults are addressed through development process",
    ));
    gsn.add_link(GsnLink::in_context_of("G1", "A2"));

    // Justification for approach
    gsn.add_element(GsnElement::justification(
        "J1",
        "Hardware architectural metrics per ISO 26262-5 provide quantitative safety evidence",
    ));
    gsn.add_link(GsnLink::in_context_of("S1", "J1"));

    gsn
}

/// Get SPFM target for ASIL level
fn get_spfm_target(asil: AsilLevel) -> f64 {
    match asil {
        AsilLevel::D => 99.0,
        AsilLevel::C => 97.0,
        AsilLevel::B => 90.0,
        AsilLevel::A => 0.0,
        AsilLevel::QM => 0.0,
    }
}

/// Get LFM target for ASIL level
fn get_lfm_target(asil: AsilLevel) -> f64 {
    match asil {
        AsilLevel::D => 90.0,
        AsilLevel::C => 80.0,
        AsilLevel::B => 60.0,
        AsilLevel::A => 0.0,
        AsilLevel::QM => 0.0,
    }
}

/// Get PMHF target for ASIL level
fn get_pmhf_target(asil: AsilLevel) -> f64 {
    match asil {
        AsilLevel::D => 10.0,
        AsilLevel::C => 100.0,
        AsilLevel::B => 100.0,
        AsilLevel::A => 1000.0,
        AsilLevel::QM => 10000.0,
    }
}

/// Validate a GSN diagram for completeness
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GsnValidationResult {
    /// Is the diagram valid?
    pub valid: bool,
    /// Undeveloped goals
    pub undeveloped_goals: Vec<GsnId>,
    /// Orphan elements (not linked)
    pub orphan_elements: Vec<GsnId>,
    /// Missing links (goals without support)
    pub goals_without_support: Vec<GsnId>,
    /// Warnings
    pub warnings: Vec<String>,
}

/// Validate a GSN diagram
pub fn validate_gsn(gsn: &GsnDiagram) -> GsnValidationResult {
    let mut result = GsnValidationResult {
        valid: true,
        undeveloped_goals: Vec::new(),
        orphan_elements: Vec::new(),
        goals_without_support: Vec::new(),
        warnings: Vec::new(),
    };

    // Check for undeveloped goals
    for element in gsn.elements.values() {
        if let GsnElement::Goal {
            id, undeveloped, ..
        } = element
        {
            if *undeveloped {
                result.undeveloped_goals.push(id.clone());
                result.valid = false;
            }
        }
    }

    // Check for goals without support
    let supported_ids: std::collections::HashSet<_> = gsn
        .links
        .iter()
        .filter(|l| l.link_type == GsnLinkType::SupportedBy)
        .map(|l| l.to.clone())
        .collect();

    for element in gsn.elements.values() {
        if let GsnElement::Goal { id, .. } = element {
            // Top goal doesn't need to support anything
            if Some(id.clone()) != gsn.top_goal && !supported_ids.contains(id) {
                // This goal isn't supported by anything
                // Check if it supports something (leaf goal)
                let supports_something = gsn.links.iter().any(|l| l.from == *id);
                if !supports_something {
                    result.goals_without_support.push(id.clone());
                }
            }
        }
    }

    // Check for orphan elements (not linked at all)
    let linked_ids: std::collections::HashSet<_> = gsn
        .links
        .iter()
        .flat_map(|l| vec![l.from.clone(), l.to.clone()])
        .collect();

    for id in gsn.elements.keys() {
        if !linked_ids.contains(id) && Some(id.clone()) != gsn.top_goal {
            result.orphan_elements.push(id.clone());
            result
                .warnings
                .push(format!("Element {} is not connected to the argument", id));
        }
    }

    result
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_gsn_element_creation() {
        let goal = GsnElement::goal("G1", "System is safe");
        assert_eq!(goal.id(), "G1");
        assert_eq!(goal.type_name(), "Goal");

        let strategy = GsnElement::strategy("S1", "Argue over metrics");
        assert_eq!(strategy.id(), "S1");
        assert_eq!(strategy.type_name(), "Strategy");

        let solution = GsnElement::solution("Sn1", "FMEA report");
        assert_eq!(solution.id(), "Sn1");
        assert_eq!(solution.type_name(), "Solution");
    }

    #[test]
    fn test_gsn_diagram_creation() {
        let mut gsn = GsnDiagram::new("Test Case", "TestDesign", AsilLevel::C);

        gsn.add_element(GsnElement::goal("G1", "Top goal"));
        gsn.add_element(GsnElement::strategy("S1", "Strategy"));
        gsn.add_element(GsnElement::solution("Sn1", "Evidence"));

        gsn.add_link(GsnLink::supported_by("G1", "S1"));
        gsn.add_link(GsnLink::supported_by("S1", "Sn1"));

        gsn.set_top_goal("G1");

        assert_eq!(gsn.elements.len(), 3);
        assert_eq!(gsn.links.len(), 2);
        assert_eq!(gsn.top_goal, Some("G1".to_string()));
    }

    #[test]
    fn test_gsn_diagram_completeness() {
        let mut gsn = GsnDiagram::new("Test", "Design", AsilLevel::B);

        gsn.add_element(GsnElement::goal("G1", "Developed goal"));
        assert!(gsn.is_complete());

        gsn.add_element(GsnElement::undeveloped_goal("G2", "Undeveloped"));
        assert!(!gsn.is_complete());
        assert_eq!(gsn.undeveloped_goals().len(), 1);
    }

    #[test]
    fn test_gsn_to_graphviz() {
        let mut gsn = GsnDiagram::new("Test", "Design", AsilLevel::D);
        gsn.add_element(GsnElement::goal("G1", "Top goal"));
        gsn.add_element(GsnElement::solution("Sn1", "Evidence"));
        gsn.add_link(GsnLink::supported_by("G1", "Sn1"));

        let dot = gsn.to_graphviz();
        assert!(dot.contains("digraph GSN"));
        assert!(dot.contains("G1"));
        assert!(dot.contains("Sn1"));
    }

    #[test]
    fn test_gsn_to_markdown() {
        let mut gsn = GsnDiagram::new("Test Safety Case", "TestDesign", AsilLevel::C);
        gsn.add_element(GsnElement::goal("G1", "Top goal"));
        gsn.add_element(GsnElement::strategy("S1", "Strategy"));

        let md = gsn.to_markdown();
        assert!(md.contains("Test Safety Case"));
        assert!(md.contains("Goals"));
        assert!(md.contains("G1"));
    }

    #[test]
    fn test_gsn_validation() {
        let mut gsn = GsnDiagram::new("Test", "Design", AsilLevel::B);
        gsn.add_element(GsnElement::goal("G1", "Top"));
        gsn.add_element(GsnElement::undeveloped_goal("G2", "Undeveloped"));
        gsn.add_element(GsnElement::context("C1", "Orphan context"));

        gsn.set_top_goal("G1");
        gsn.add_link(GsnLink::supported_by("G1", "G2"));

        let result = validate_gsn(&gsn);
        assert!(!result.valid); // Undeveloped goal
        assert!(!result.undeveloped_goals.is_empty());
        assert!(!result.orphan_elements.is_empty()); // C1 is orphan
    }

    #[test]
    fn test_asil_targets() {
        assert!((get_spfm_target(AsilLevel::D) - 99.0).abs() < 0.01);
        assert!((get_spfm_target(AsilLevel::C) - 97.0).abs() < 0.01);
        assert!((get_lfm_target(AsilLevel::D) - 90.0).abs() < 0.01);
        assert!((get_pmhf_target(AsilLevel::D) - 10.0).abs() < 0.01);
    }

    #[test]
    fn test_gsn_link_types() {
        let supported = GsnLink::supported_by("G1", "S1");
        assert_eq!(supported.link_type, GsnLinkType::SupportedBy);

        let context = GsnLink::in_context_of("G1", "C1");
        assert_eq!(context.link_type, GsnLinkType::InContextOf);
    }
}
