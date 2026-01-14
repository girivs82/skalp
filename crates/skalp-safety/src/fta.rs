//! Fault Tree Analysis (FTA) for ISO 26262 compliance
//!
//! This module provides Fault Tree Analysis capabilities for safety-critical hardware designs.
//! FTA is a top-down deductive analysis method required for ASIL C/D certification.
//!
//! Key features:
//! - Fault tree construction from safety goals and gate netlist
//! - BDD-based minimal cut set calculation for efficient analysis
//! - Probability propagation and importance measures
//! - Cut set analysis for PMHF contribution

use crate::asil::AsilLevel;
use crate::hierarchy::SafetyGoal;
use indexmap::IndexMap;
use serde::{Deserialize, Serialize};
use std::collections::HashSet;

/// Unique identifier for fault tree nodes
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct FtaNodeId(pub usize);

impl FtaNodeId {
    /// Create a new node ID
    pub fn new(id: usize) -> Self {
        Self(id)
    }
}

/// FTA gate types for combining fault events
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum GateType {
    /// AND gate: All inputs must fail for output to fail
    And,
    /// OR gate: Any input failing causes output to fail
    Or,
    /// K-of-N voting: At least K of N inputs must fail
    KofN { k: u32, n: u32 },
    /// Priority AND: Inputs must fail in sequence (order matters)
    /// The first input must fail before the second, and so on.
    Pand,
    /// NOT gate: Output fails when input doesn't fail
    Not,
    /// XOR gate: Exactly one input must fail
    Xor,

    // ===== Dynamic Fault Tree Gates =====
    /// SPARE gate: Models standby redundancy with cold/warm/hot spares
    ///
    /// The primary input is normally active. When it fails, the next spare
    /// takes over if the switching mechanism succeeds. The gate output fails
    /// when all spares are exhausted or switching fails.
    ///
    /// Parameters:
    /// - `primary`: The main component (always first input)
    /// - `switching_probability`: P(successful switchover), typically 0.9-0.999
    /// - `dormancy_factor`: Failure rate multiplier for dormant spares (0=cold, 1=hot)
    Spare {
        /// Probability of successful switchover (0.0 to 1.0)
        switching_probability: f64,
        /// Dormancy factor: 0.0 = cold spare (no failures while dormant)
        /// 0.5 = warm spare (reduced failure rate), 1.0 = hot spare (same failure rate)
        dormancy_factor: f64,
    },

    /// SEQ gate: Sequence enforcing gate
    ///
    /// Forces events to occur in a specific order. The gate output is true
    /// only if all inputs fail and they fail in the specified order (left to right).
    /// This is stricter than PAND as it prevents any out-of-order failures.
    Seq,

    /// FDEP gate: Functional Dependency
    ///
    /// Models a trigger event that causes all dependent events to fail.
    /// When the trigger fires, all dependents are immediately considered failed.
    /// The FDEP gate itself doesn't have an output - it modifies the behavior
    /// of dependent basic events.
    ///
    /// Parameters:
    /// - First input is the trigger
    /// - Remaining inputs are dependent events
    Fdep,

    /// Inhibit gate: AND with a conditioning event
    ///
    /// Output fails only if the basic event fails AND the conditioning event
    /// (probability) is satisfied. Useful for modeling partial coverage or
    /// conditional failures.
    Inhibit {
        /// Probability that the inhibit condition allows fault propagation
        condition_probability: f64,
    },
}

// Manual implementation of Hash for GateType since f64 doesn't implement Hash
impl std::hash::Hash for GateType {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::mem::discriminant(self).hash(state);
        match self {
            GateType::KofN { k, n } => {
                k.hash(state);
                n.hash(state);
            }
            GateType::Spare {
                switching_probability,
                dormancy_factor,
            } => {
                switching_probability.to_bits().hash(state);
                dormancy_factor.to_bits().hash(state);
            }
            GateType::Inhibit {
                condition_probability,
            } => {
                condition_probability.to_bits().hash(state);
            }
            _ => {}
        }
    }
}

impl Eq for GateType {}

/// Type of fault tree node
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum FtaNodeType {
    /// Top event (root of fault tree) - the undesired hazardous event
    TopEvent {
        description: String,
        safety_goal_id: Option<String>,
    },
    /// Intermediate gate combining lower events
    Gate {
        gate_type: GateType,
        inputs: Vec<FtaNodeId>,
    },
    /// Basic event - elementary fault with known failure rate
    BasicEvent {
        /// Failure rate in FIT (failures per 10^9 hours)
        failure_rate: f64,
        /// Exposure time in hours
        exposure_time: f64,
        /// Component or cell this event represents
        component: String,
    },
    /// Undeveloped event - not analyzed further
    UndevelopedEvent { description: String },
    /// House event - event that is always true or false
    HouseEvent { state: bool },
    /// Transfer in - reference to another fault tree
    TransferIn {
        target_tree: String,
        target_node: FtaNodeId,
    },
}

/// A node in the fault tree
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FtaNode {
    /// Unique node identifier
    pub id: FtaNodeId,
    /// Human-readable name
    pub name: String,
    /// Type and properties of the node
    pub node_type: FtaNodeType,
    /// Calculated probability (filled during analysis)
    pub probability: Option<f64>,
    /// Optional description
    pub description: Option<String>,
}

impl FtaNode {
    /// Create a new fault tree node
    pub fn new(id: FtaNodeId, name: &str, node_type: FtaNodeType) -> Self {
        Self {
            id,
            name: name.to_string(),
            node_type,
            probability: None,
            description: None,
        }
    }

    /// Check if this node is a basic event
    pub fn is_basic_event(&self) -> bool {
        matches!(self.node_type, FtaNodeType::BasicEvent { .. })
    }

    /// Check if this node is a gate
    pub fn is_gate(&self) -> bool {
        matches!(self.node_type, FtaNodeType::Gate { .. })
    }
}

/// Fault tree metadata
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FtaMetadata {
    /// Unique identifier
    pub id: String,
    /// Analysis name
    pub name: String,
    /// Design being analyzed
    pub design_name: String,
    /// Target ASIL level
    pub target_asil: AsilLevel,
    /// Analysis date
    pub analysis_date: chrono::DateTime<chrono::Utc>,
    /// Analyst name
    pub analyst: String,
}

/// Complete fault tree structure
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FaultTree {
    /// Metadata about the analysis
    pub metadata: FtaMetadata,
    /// Safety goal this tree analyzes
    pub safety_goal: String,
    /// Root node (top event)
    pub top_event: FtaNodeId,
    /// All nodes in the tree
    pub nodes: Vec<FtaNode>,
    /// Node lookup by ID for efficient access
    #[serde(skip)]
    node_index: IndexMap<FtaNodeId, usize>,
}

impl FaultTree {
    /// Create a new fault tree
    pub fn new(metadata: FtaMetadata, safety_goal: &str) -> Self {
        Self {
            metadata,
            safety_goal: safety_goal.to_string(),
            top_event: FtaNodeId(0),
            nodes: Vec::new(),
            node_index: IndexMap::new(),
        }
    }

    /// Add a node to the tree and return its ID
    pub fn add_node(&mut self, name: &str, node_type: FtaNodeType) -> FtaNodeId {
        let id = FtaNodeId(self.nodes.len());
        let node = FtaNode::new(id, name, node_type);
        self.node_index.insert(id, self.nodes.len());
        self.nodes.push(node);
        id
    }

    /// Set the top event
    pub fn set_top_event(&mut self, id: FtaNodeId) {
        self.top_event = id;
    }

    /// Get a node by ID
    pub fn get_node(&self, id: FtaNodeId) -> Option<&FtaNode> {
        self.node_index
            .get(&id)
            .and_then(|&idx| self.nodes.get(idx))
    }

    /// Get a mutable node by ID
    pub fn get_node_mut(&mut self, id: FtaNodeId) -> Option<&mut FtaNode> {
        self.node_index
            .get(&id)
            .and_then(|&idx| self.nodes.get_mut(idx))
    }

    /// Get all basic events
    pub fn basic_events(&self) -> Vec<&FtaNode> {
        self.nodes.iter().filter(|n| n.is_basic_event()).collect()
    }

    /// Rebuild the node index (call after deserialization)
    pub fn rebuild_index(&mut self) {
        self.node_index.clear();
        for (idx, node) in self.nodes.iter().enumerate() {
            self.node_index.insert(node.id, idx);
        }
    }

    /// Count total nodes
    pub fn node_count(&self) -> usize {
        self.nodes.len()
    }

    /// Count basic events
    pub fn basic_event_count(&self) -> usize {
        self.nodes.iter().filter(|n| n.is_basic_event()).count()
    }

    /// Count gates
    pub fn gate_count(&self) -> usize {
        self.nodes.iter().filter(|n| n.is_gate()).count()
    }

    // ===== Convenience Methods for Creating Gates =====

    /// Add a SPARE gate for standby redundancy
    ///
    /// # Arguments
    /// * `name` - Gate name
    /// * `primary` - Primary component (must fail first)
    /// * `spares` - Spare components that take over on primary failure
    /// * `switching_probability` - Probability of successful switchover (0.0-1.0)
    /// * `dormancy_factor` - 0.0 = cold spare, 0.5 = warm spare, 1.0 = hot spare
    pub fn add_spare_gate(
        &mut self,
        name: &str,
        primary: FtaNodeId,
        spares: &[FtaNodeId],
        switching_probability: f64,
        dormancy_factor: f64,
    ) -> FtaNodeId {
        let mut inputs = vec![primary];
        inputs.extend_from_slice(spares);
        self.add_node(
            name,
            FtaNodeType::Gate {
                gate_type: GateType::Spare {
                    switching_probability,
                    dormancy_factor,
                },
                inputs,
            },
        )
    }

    /// Add a SEQ (sequence) gate - events must fail in order
    pub fn add_seq_gate(&mut self, name: &str, events_in_order: &[FtaNodeId]) -> FtaNodeId {
        self.add_node(
            name,
            FtaNodeType::Gate {
                gate_type: GateType::Seq,
                inputs: events_in_order.to_vec(),
            },
        )
    }

    /// Add an FDEP (functional dependency) gate
    ///
    /// # Arguments
    /// * `name` - Gate name
    /// * `trigger` - The trigger event that causes all dependents to fail
    /// * `dependents` - Events that fail when trigger fires
    pub fn add_fdep_gate(
        &mut self,
        name: &str,
        trigger: FtaNodeId,
        dependents: &[FtaNodeId],
    ) -> FtaNodeId {
        let mut inputs = vec![trigger];
        inputs.extend_from_slice(dependents);
        self.add_node(
            name,
            FtaNodeType::Gate {
                gate_type: GateType::Fdep,
                inputs,
            },
        )
    }

    /// Add an INHIBIT gate - AND with conditioning probability
    pub fn add_inhibit_gate(
        &mut self,
        name: &str,
        inputs: &[FtaNodeId],
        condition_probability: f64,
    ) -> FtaNodeId {
        self.add_node(
            name,
            FtaNodeType::Gate {
                gate_type: GateType::Inhibit {
                    condition_probability,
                },
                inputs: inputs.to_vec(),
            },
        )
    }

    /// Add a basic AND gate
    pub fn add_and_gate(&mut self, name: &str, inputs: &[FtaNodeId]) -> FtaNodeId {
        self.add_node(
            name,
            FtaNodeType::Gate {
                gate_type: GateType::And,
                inputs: inputs.to_vec(),
            },
        )
    }

    /// Add a basic OR gate
    pub fn add_or_gate(&mut self, name: &str, inputs: &[FtaNodeId]) -> FtaNodeId {
        self.add_node(
            name,
            FtaNodeType::Gate {
                gate_type: GateType::Or,
                inputs: inputs.to_vec(),
            },
        )
    }

    /// Add a basic event (elementary fault)
    pub fn add_basic_event(
        &mut self,
        name: &str,
        failure_rate: f64,
        exposure_time: f64,
        component: &str,
    ) -> FtaNodeId {
        self.add_node(
            name,
            FtaNodeType::BasicEvent {
                failure_rate,
                exposure_time,
                component: component.to_string(),
            },
        )
    }
}

/// A minimal cut set - smallest combination of basic events causing top event
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MinimalCutSet {
    /// IDs of basic events in this cut set
    pub basic_events: Vec<FtaNodeId>,
    /// Names of basic events (for convenience)
    pub event_names: Vec<String>,
    /// Order of the cut set (number of events)
    pub order: usize,
    /// Calculated probability (product of event probabilities)
    pub probability: f64,
    /// Contribution to top event probability
    pub contribution_percentage: f64,
}

impl MinimalCutSet {
    /// Create a new minimal cut set
    pub fn new(basic_events: Vec<FtaNodeId>, event_names: Vec<String>, probability: f64) -> Self {
        let order = basic_events.len();
        Self {
            basic_events,
            event_names,
            order,
            probability,
            contribution_percentage: 0.0,
        }
    }

    /// Check if this cut set is a single-point failure (order 1)
    pub fn is_single_point(&self) -> bool {
        self.order == 1
    }

    /// Check if this cut set contains a specific event
    pub fn contains_event(&self, event_id: FtaNodeId) -> bool {
        self.basic_events.contains(&event_id)
    }
}

/// Importance measures for a basic event
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ImportanceMeasures {
    /// Event ID
    pub event_id: FtaNodeId,
    /// Event name
    pub event_name: String,
    /// Fussell-Vesely importance: contribution to top event probability
    pub fussell_vesely: f64,
    /// Birnbaum importance: sensitivity of top event to this event
    pub birnbaum: f64,
    /// Risk Achievement Worth: increase in risk if event always fails
    pub rac: f64,
    /// Risk Decrease Worth: decrease in risk if event never fails
    pub rdc: f64,
    /// Criticality importance
    pub criticality: f64,
}

/// Cut set analysis results
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CutSetAnalysis {
    /// All minimal cut sets found
    pub cut_sets: Vec<MinimalCutSet>,
    /// Top event probability
    pub top_event_probability: f64,
    /// Number of cut sets by order
    pub cut_sets_by_order: IndexMap<usize, usize>,
    /// Importance measures for each basic event
    pub importance_measures: IndexMap<FtaNodeId, ImportanceMeasures>,
    /// Single point failures (order-1 cut sets)
    pub single_point_failures: Vec<MinimalCutSet>,
    /// Dominant cut sets (contributing > 1% to top event)
    pub dominant_cut_sets: Vec<MinimalCutSet>,
}

impl CutSetAnalysis {
    /// Create empty analysis results
    pub fn new() -> Self {
        Self {
            cut_sets: Vec::new(),
            top_event_probability: 0.0,
            cut_sets_by_order: IndexMap::new(),
            importance_measures: IndexMap::new(),
            single_point_failures: Vec::new(),
            dominant_cut_sets: Vec::new(),
        }
    }

    /// Get maximum cut set order
    pub fn max_order(&self) -> usize {
        self.cut_sets.iter().map(|cs| cs.order).max().unwrap_or(0)
    }

    /// Get total number of cut sets
    pub fn total_cut_sets(&self) -> usize {
        self.cut_sets.len()
    }
}

impl Default for CutSetAnalysis {
    fn default() -> Self {
        Self::new()
    }
}

// ============================================================================
// BDD (Binary Decision Diagram) Implementation
// ============================================================================

/// BDD node ID
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BddNodeId(usize);

impl BddNodeId {
    /// Terminal false node
    pub const FALSE: BddNodeId = BddNodeId(0);
    /// Terminal true node
    pub const TRUE: BddNodeId = BddNodeId(1);

    fn new(id: usize) -> Self {
        Self(id)
    }

    fn is_terminal(&self) -> bool {
        self.0 <= 1
    }
}

/// A node in the BDD
#[derive(Debug, Clone)]
struct BddNode {
    /// Variable index (corresponds to basic event)
    var: usize,
    /// Low child (variable = 0)
    low: BddNodeId,
    /// High child (variable = 1)
    high: BddNodeId,
}

/// Binary Decision Diagram for efficient cut set calculation
///
/// BDDs provide a canonical, compact representation of boolean functions,
/// enabling efficient minimal cut set extraction.
pub struct Bdd {
    /// All BDD nodes (index 0 = false terminal, index 1 = true terminal)
    nodes: Vec<Option<BddNode>>,
    /// Variable ordering (maps variable index to FtaNodeId)
    var_order: Vec<FtaNodeId>,
    /// Reverse mapping: FtaNodeId -> variable index
    var_index: IndexMap<FtaNodeId, usize>,
    /// Computed table for memoization
    computed: IndexMap<(BddNodeId, BddNodeId, char), BddNodeId>,
    /// Unique table for hash-consing
    unique: IndexMap<(usize, BddNodeId, BddNodeId), BddNodeId>,
}

impl Bdd {
    /// Create a new BDD with the given variable ordering
    pub fn new(var_order: Vec<FtaNodeId>) -> Self {
        let mut var_index = IndexMap::new();
        for (idx, &id) in var_order.iter().enumerate() {
            var_index.insert(id, idx);
        }

        let mut bdd = Self {
            nodes: vec![None, None], // Reserve 0 and 1 for terminals
            var_order,
            var_index,
            computed: IndexMap::new(),
            unique: IndexMap::new(),
        };

        // Initialize terminal nodes
        bdd.nodes[0] = None; // False terminal
        bdd.nodes[1] = None; // True terminal

        bdd
    }

    /// Build BDD from a fault tree
    pub fn from_fault_tree(tree: &FaultTree) -> Self {
        // Get variable ordering from basic events
        let var_order: Vec<FtaNodeId> = tree.basic_events().iter().map(|n| n.id).collect();
        let mut bdd = Self::new(var_order);

        // Build BDD recursively from top event
        let _root = bdd.build_from_node(tree, tree.top_event);

        bdd
    }

    /// Build BDD for a fault tree node
    fn build_from_node(&mut self, tree: &FaultTree, node_id: FtaNodeId) -> BddNodeId {
        let node = match tree.get_node(node_id) {
            Some(n) => n,
            None => return BddNodeId::FALSE,
        };

        match &node.node_type {
            FtaNodeType::BasicEvent { .. } => {
                // Create a variable node for this basic event
                if let Some(&var_idx) = self.var_index.get(&node_id) {
                    self.make_node(var_idx, BddNodeId::FALSE, BddNodeId::TRUE)
                } else {
                    BddNodeId::FALSE
                }
            }
            FtaNodeType::Gate { gate_type, inputs } => {
                if inputs.is_empty() {
                    return BddNodeId::FALSE;
                }

                // Build BDD for each input
                let input_bdds: Vec<BddNodeId> = inputs
                    .iter()
                    .map(|&id| self.build_from_node(tree, id))
                    .collect();

                // Combine based on gate type
                match gate_type {
                    GateType::Or => {
                        let mut result = input_bdds[0];
                        for &input in &input_bdds[1..] {
                            result = self.apply_or(result, input);
                        }
                        result
                    }
                    GateType::And => {
                        let mut result = input_bdds[0];
                        for &input in &input_bdds[1..] {
                            result = self.apply_and(result, input);
                        }
                        result
                    }
                    GateType::Not => {
                        if !input_bdds.is_empty() {
                            self.apply_not(input_bdds[0])
                        } else {
                            BddNodeId::FALSE
                        }
                    }
                    GateType::Xor => {
                        let mut result = input_bdds[0];
                        for &input in &input_bdds[1..] {
                            result = self.apply_xor(result, input);
                        }
                        result
                    }
                    GateType::KofN { k, n } => {
                        // K-of-N: at least k of n inputs must be true
                        self.build_kofn(*k as usize, &input_bdds)
                    }
                    GateType::Pand => {
                        // Priority AND: treat as AND for cut set analysis
                        // (temporal ordering doesn't affect cut sets)
                        let mut result = input_bdds[0];
                        for &input in &input_bdds[1..] {
                            result = self.apply_and(result, input);
                        }
                        result
                    }
                    GateType::Seq => {
                        // SEQ gate: all inputs must fail in sequence
                        // For cut set analysis, this is equivalent to AND
                        // (all must fail for output to fail)
                        let mut result = input_bdds[0];
                        for &input in &input_bdds[1..] {
                            result = self.apply_and(result, input);
                        }
                        result
                    }
                    GateType::Spare { .. } => {
                        // SPARE gate: for cut set analysis, all spares AND primary must fail
                        // (conservative analysis - actual probability depends on temporal behavior)
                        let mut result = input_bdds[0];
                        for &input in &input_bdds[1..] {
                            result = self.apply_and(result, input);
                        }
                        result
                    }
                    GateType::Fdep => {
                        // FDEP: Functional Dependency
                        // First input is trigger, rest are dependents
                        // Output is true if trigger fires OR all dependents fail independently
                        if input_bdds.is_empty() {
                            BddNodeId::FALSE
                        } else if input_bdds.len() == 1 {
                            input_bdds[0] // Just the trigger
                        } else {
                            // Trigger causes all dependents to fail, so
                            // output = trigger OR (all dependents fail independently)
                            let trigger = input_bdds[0];
                            let mut dependents_fail = input_bdds[1];
                            for &input in &input_bdds[2..] {
                                dependents_fail = self.apply_and(dependents_fail, input);
                            }
                            self.apply_or(trigger, dependents_fail)
                        }
                    }
                    GateType::Inhibit { .. } => {
                        // INHIBIT gate: AND with conditioning probability
                        // For cut set analysis, treat as AND (condition is modeled separately)
                        let mut result = input_bdds[0];
                        for &input in &input_bdds[1..] {
                            result = self.apply_and(result, input);
                        }
                        result
                    }
                }
            }
            FtaNodeType::TopEvent { .. } => {
                // Top event should have inputs via a gate
                // If it doesn't, return FALSE
                BddNodeId::FALSE
            }
            FtaNodeType::UndevelopedEvent { .. } => {
                // Treat undeveloped as FALSE (conservative)
                BddNodeId::FALSE
            }
            FtaNodeType::HouseEvent { state } => {
                if *state {
                    BddNodeId::TRUE
                } else {
                    BddNodeId::FALSE
                }
            }
            FtaNodeType::TransferIn { .. } => {
                // Transfer requires resolving external tree
                BddNodeId::FALSE
            }
        }
    }

    /// Make a BDD node (with hash-consing)
    fn make_node(&mut self, var: usize, low: BddNodeId, high: BddNodeId) -> BddNodeId {
        // Reduction rule: if both children are the same, return the child
        if low == high {
            return low;
        }

        // Check unique table
        let key = (var, low, high);
        if let Some(&id) = self.unique.get(&key) {
            return id;
        }

        // Create new node
        let id = BddNodeId::new(self.nodes.len());
        self.nodes.push(Some(BddNode { var, low, high }));
        self.unique.insert(key, id);
        id
    }

    /// Apply OR operation
    fn apply_or(&mut self, f: BddNodeId, g: BddNodeId) -> BddNodeId {
        self.apply(f, g, '|')
    }

    /// Apply AND operation
    fn apply_and(&mut self, f: BddNodeId, g: BddNodeId) -> BddNodeId {
        self.apply(f, g, '&')
    }

    /// Apply NOT operation
    fn apply_not(&mut self, f: BddNodeId) -> BddNodeId {
        if f == BddNodeId::FALSE {
            BddNodeId::TRUE
        } else if f == BddNodeId::TRUE {
            BddNodeId::FALSE
        } else if let Some(node) = &self.nodes[f.0].clone() {
            let low = self.apply_not(node.low);
            let high = self.apply_not(node.high);
            self.make_node(node.var, low, high)
        } else {
            BddNodeId::FALSE
        }
    }

    /// Apply XOR operation
    fn apply_xor(&mut self, f: BddNodeId, g: BddNodeId) -> BddNodeId {
        self.apply(f, g, '^')
    }

    /// Generic apply operation with memoization
    fn apply(&mut self, f: BddNodeId, g: BddNodeId, op: char) -> BddNodeId {
        // Terminal cases
        match op {
            '|' => {
                if f == BddNodeId::TRUE || g == BddNodeId::TRUE {
                    return BddNodeId::TRUE;
                }
                if f == BddNodeId::FALSE {
                    return g;
                }
                if g == BddNodeId::FALSE {
                    return f;
                }
            }
            '&' => {
                if f == BddNodeId::FALSE || g == BddNodeId::FALSE {
                    return BddNodeId::FALSE;
                }
                if f == BddNodeId::TRUE {
                    return g;
                }
                if g == BddNodeId::TRUE {
                    return f;
                }
            }
            '^' => {
                if f == BddNodeId::FALSE {
                    return g;
                }
                if g == BddNodeId::FALSE {
                    return f;
                }
                if f == BddNodeId::TRUE {
                    return self.apply_not(g);
                }
                if g == BddNodeId::TRUE {
                    return self.apply_not(f);
                }
            }
            _ => {}
        }

        // Check computed table
        let key = (f, g, op);
        if let Some(&result) = self.computed.get(&key) {
            return result;
        }

        // Get node information
        let f_node = self.nodes.get(f.0).and_then(|n| n.as_ref()).cloned();
        let g_node = self.nodes.get(g.0).and_then(|n| n.as_ref()).cloned();

        let result = match (&f_node, &g_node) {
            (Some(fn_), Some(gn)) => {
                use std::cmp::Ordering;
                let (var, f_low, f_high, g_low, g_high) = match fn_.var.cmp(&gn.var) {
                    Ordering::Less => (fn_.var, fn_.low, fn_.high, g, g),
                    Ordering::Greater => (gn.var, f, f, gn.low, gn.high),
                    Ordering::Equal => (fn_.var, fn_.low, fn_.high, gn.low, gn.high),
                };

                let low = self.apply(f_low, g_low, op);
                let high = self.apply(f_high, g_high, op);
                self.make_node(var, low, high)
            }
            (Some(fn_), None) => {
                let low = self.apply(fn_.low, g, op);
                let high = self.apply(fn_.high, g, op);
                self.make_node(fn_.var, low, high)
            }
            (None, Some(gn)) => {
                let low = self.apply(f, gn.low, op);
                let high = self.apply(f, gn.high, op);
                self.make_node(gn.var, low, high)
            }
            (None, None) => BddNodeId::FALSE,
        };

        self.computed.insert(key, result);
        result
    }

    /// Build K-of-N gate
    fn build_kofn(&mut self, k: usize, inputs: &[BddNodeId]) -> BddNodeId {
        let n = inputs.len();
        if k == 0 {
            return BddNodeId::TRUE;
        }
        if k > n {
            return BddNodeId::FALSE;
        }
        if k == n {
            // All must be true
            let mut result = inputs[0];
            for &input in &inputs[1..] {
                result = self.apply_and(result, input);
            }
            return result;
        }
        if k == 1 {
            // At least one must be true (OR)
            let mut result = inputs[0];
            for &input in &inputs[1..] {
                result = self.apply_or(result, input);
            }
            return result;
        }

        // General case: recursive definition
        // K-of-N with first input true + (K-1)-of-(N-1) with remaining
        // OR K-of-(N-1) with remaining (first input false)
        let first = inputs[0];
        let rest = &inputs[1..];

        let with_first = {
            let rest_kofn = self.build_kofn(k - 1, rest);
            self.apply_and(first, rest_kofn)
        };

        let without_first = self.build_kofn(k, rest);

        self.apply_or(with_first, without_first)
    }

    /// Extract minimal cut sets from the BDD
    pub fn extract_minimal_cut_sets(&self, tree: &FaultTree) -> Vec<MinimalCutSet> {
        let mut cut_sets = Vec::new();
        let mut current_path: Vec<FtaNodeId> = Vec::new();

        // Start from root (index 2 if non-trivial, or terminal)
        if self.nodes.len() > 2 {
            self.extract_paths(
                BddNodeId::new(self.nodes.len() - 1),
                &mut current_path,
                &mut cut_sets,
                tree,
            );
        }

        // Remove non-minimal cut sets
        self.minimize_cut_sets(&mut cut_sets);

        // Calculate probabilities
        self.calculate_cut_set_probabilities(&mut cut_sets, tree);

        cut_sets
    }

    /// Recursively extract paths to TRUE terminal
    fn extract_paths(
        &self,
        node_id: BddNodeId,
        current_path: &mut Vec<FtaNodeId>,
        cut_sets: &mut Vec<MinimalCutSet>,
        tree: &FaultTree,
    ) {
        if node_id == BddNodeId::TRUE {
            // Found a path to TRUE - this is a cut set
            if !current_path.is_empty() {
                let event_names: Vec<String> = current_path
                    .iter()
                    .filter_map(|&id| tree.get_node(id).map(|n| n.name.clone()))
                    .collect();
                cut_sets.push(MinimalCutSet::new(current_path.clone(), event_names, 0.0));
            }
            return;
        }

        if node_id == BddNodeId::FALSE {
            // Dead end
            return;
        }

        if let Some(Some(node)) = self.nodes.get(node_id.0) {
            // Get the FtaNodeId for this variable
            if let Some(&fta_id) = self.var_order.get(node.var) {
                // Try high branch (variable is TRUE - event occurs)
                current_path.push(fta_id);
                self.extract_paths(node.high, current_path, cut_sets, tree);
                current_path.pop();

                // Try low branch (variable is FALSE - event doesn't occur)
                self.extract_paths(node.low, current_path, cut_sets, tree);
            }
        }
    }

    /// Remove non-minimal cut sets
    fn minimize_cut_sets(&self, cut_sets: &mut Vec<MinimalCutSet>) {
        // Sort by order (smallest first)
        cut_sets.sort_by_key(|cs| cs.order);

        let mut minimal: Vec<MinimalCutSet> = Vec::new();

        for cs in cut_sets.drain(..) {
            let cs_set: HashSet<_> = cs.basic_events.iter().copied().collect();

            // Check if any existing minimal cut set is a subset
            let is_superset = minimal.iter().any(|m| {
                let m_set: HashSet<_> = m.basic_events.iter().copied().collect();
                m_set.is_subset(&cs_set)
            });

            if !is_superset {
                // Remove any existing that are supersets of this one
                minimal.retain(|m| {
                    let m_set: HashSet<_> = m.basic_events.iter().copied().collect();
                    !cs_set.is_subset(&m_set)
                });
                minimal.push(cs);
            }
        }

        *cut_sets = minimal;
    }

    /// Calculate probabilities for cut sets
    fn calculate_cut_set_probabilities(&self, cut_sets: &mut [MinimalCutSet], tree: &FaultTree) {
        for cs in cut_sets.iter_mut() {
            let mut prob = 1.0;
            for &event_id in &cs.basic_events {
                if let Some(node) = tree.get_node(event_id) {
                    if let FtaNodeType::BasicEvent {
                        failure_rate,
                        exposure_time,
                        ..
                    } = &node.node_type
                    {
                        // Probability = λ × t (for small values)
                        // FIT is per 10^9 hours, so convert
                        let lambda = failure_rate * 1e-9;
                        prob *= lambda * exposure_time;
                    }
                }
            }
            cs.probability = prob;
        }
    }

    /// Calculate top event probability using inclusion-exclusion
    pub fn calculate_top_probability(&self, cut_sets: &[MinimalCutSet]) -> f64 {
        if cut_sets.is_empty() {
            return 0.0;
        }

        // For small number of cut sets, use inclusion-exclusion
        // For larger numbers, use upper bound (sum of probabilities)
        if cut_sets.len() <= 10 {
            // Inclusion-exclusion (simplified for small n)
            let mut prob = 0.0;
            for cs in cut_sets {
                prob += cs.probability;
            }
            // Subtract pairwise intersections (approximation)
            for i in 0..cut_sets.len() {
                for j in (i + 1)..cut_sets.len() {
                    prob -= cut_sets[i].probability * cut_sets[j].probability;
                }
            }
            prob.clamp(0.0, 1.0)
        } else {
            // Upper bound (rare event approximation)
            let sum: f64 = cut_sets.iter().map(|cs| cs.probability).sum();
            sum.min(1.0)
        }
    }
}

// ============================================================================
// Analysis Functions
// ============================================================================

/// Analyze a fault tree and compute cut sets and probabilities
pub fn analyze_fault_tree(tree: &FaultTree) -> CutSetAnalysis {
    let bdd = Bdd::from_fault_tree(tree);
    let mut cut_sets = bdd.extract_minimal_cut_sets(tree);

    // Calculate top event probability
    let top_probability = bdd.calculate_top_probability(&cut_sets);

    // Calculate contribution percentages
    if top_probability > 0.0 {
        for cs in &mut cut_sets {
            cs.contribution_percentage = (cs.probability / top_probability) * 100.0;
        }
    }

    // Build results
    let mut analysis = CutSetAnalysis::new();
    analysis.top_event_probability = top_probability;

    // Count cut sets by order
    for cs in &cut_sets {
        *analysis.cut_sets_by_order.entry(cs.order).or_insert(0) += 1;
    }

    // Identify single point failures
    analysis.single_point_failures = cut_sets
        .iter()
        .filter(|cs| cs.order == 1)
        .cloned()
        .collect();

    // Identify dominant cut sets (>1% contribution)
    analysis.dominant_cut_sets = cut_sets
        .iter()
        .filter(|cs| cs.contribution_percentage > 1.0)
        .cloned()
        .collect();

    // Calculate importance measures
    analysis.importance_measures = calculate_importance_measures(&cut_sets, top_probability, tree);

    analysis.cut_sets = cut_sets;

    analysis
}

/// Calculate importance measures for all basic events
fn calculate_importance_measures(
    cut_sets: &[MinimalCutSet],
    top_probability: f64,
    tree: &FaultTree,
) -> IndexMap<FtaNodeId, ImportanceMeasures> {
    let mut measures = IndexMap::new();

    for node in tree.basic_events() {
        let event_id = node.id;

        // Fussell-Vesely: sum of probabilities of cut sets containing this event / top probability
        let fv_sum: f64 = cut_sets
            .iter()
            .filter(|cs| cs.contains_event(event_id))
            .map(|cs| cs.probability)
            .sum();
        let fussell_vesely = if top_probability > 0.0 {
            fv_sum / top_probability
        } else {
            0.0
        };

        // Get event probability
        let event_prob = if let FtaNodeType::BasicEvent {
            failure_rate,
            exposure_time,
            ..
        } = &node.node_type
        {
            failure_rate * 1e-9 * exposure_time
        } else {
            0.0
        };

        // Birnbaum: ∂Q/∂q_i (approximated)
        // For rare events: sum of probabilities of cut sets containing event / event probability
        let birnbaum = if event_prob > 0.0 {
            fv_sum / event_prob
        } else {
            0.0
        };

        // RAC (Risk Achievement Worth): Q(q_i=1) / Q
        // Approximated by assuming event always fails
        let rac = if top_probability > 0.0 {
            (top_probability + fv_sum).min(1.0) / top_probability
        } else {
            1.0
        };

        // RDC (Risk Decrease Worth): Q / Q(q_i=0)
        // Approximated by removing event's contribution
        let q_without = top_probability - fv_sum;
        let rdc = if q_without > 0.0 {
            top_probability / q_without
        } else {
            f64::INFINITY
        };

        // Criticality: FV * q_i / Q
        let criticality = fussell_vesely * event_prob;

        measures.insert(
            event_id,
            ImportanceMeasures {
                event_id,
                event_name: node.name.clone(),
                fussell_vesely,
                birnbaum,
                rac,
                rdc,
                criticality,
            },
        );
    }

    measures
}

/// FTA compliance result against ASIL requirements
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FtaComplianceResult {
    /// Target ASIL level
    pub target_asil: AsilLevel,
    /// PMHF target (FIT)
    pub pmhf_target: f64,
    /// Calculated PMHF from FTA (FIT)
    pub calculated_pmhf: f64,
    /// Whether PMHF target is met
    pub pmhf_compliant: bool,
    /// Number of single point failures (should be 0 for ASIL D)
    pub single_point_count: usize,
    /// Single point failure compliant
    pub spf_compliant: bool,
    /// Overall compliance
    pub overall_compliant: bool,
    /// Recommendations for improvement
    pub recommendations: Vec<String>,
}

/// Verify FTA results against ASIL requirements
pub fn verify_fta_compliance(
    analysis: &CutSetAnalysis,
    target_asil: AsilLevel,
) -> FtaComplianceResult {
    // PMHF targets by ASIL (FIT)
    let pmhf_target = match target_asil {
        AsilLevel::QM => f64::INFINITY,
        AsilLevel::A => 1000.0,
        AsilLevel::B => 100.0,
        AsilLevel::C => 100.0,
        AsilLevel::D => 10.0,
    };

    // Convert probability to FIT (assume 1000 hour exposure)
    let calculated_pmhf = analysis.top_event_probability * 1e9 / 1000.0;
    let pmhf_compliant = calculated_pmhf <= pmhf_target;

    let single_point_count = analysis.single_point_failures.len();
    // ASIL D should have no single point failures
    let spf_compliant = match target_asil {
        AsilLevel::D => single_point_count == 0,
        AsilLevel::C => single_point_count <= 1,
        _ => true,
    };

    let overall_compliant = pmhf_compliant && spf_compliant;

    let mut recommendations = Vec::new();
    if !pmhf_compliant {
        recommendations.push(format!(
            "PMHF ({:.2e} FIT) exceeds target ({:.0} FIT). Add safety mechanisms to reduce failure rate.",
            calculated_pmhf, pmhf_target
        ));
    }
    if !spf_compliant {
        recommendations.push(format!(
            "Found {} single point failures. Add redundancy or error detection for SPF mitigation.",
            single_point_count
        ));
        for spf in &analysis.single_point_failures {
            recommendations.push(format!("  - SPF: {}", spf.event_names.join(", ")));
        }
    }

    FtaComplianceResult {
        target_asil,
        pmhf_target,
        calculated_pmhf,
        pmhf_compliant,
        single_point_count,
        spf_compliant,
        overall_compliant,
        recommendations,
    }
}

/// Format FTA analysis report
pub fn format_fta_report(tree: &FaultTree, analysis: &CutSetAnalysis) -> String {
    let mut output = String::new();

    output.push_str("=== Fault Tree Analysis Report ===\n\n");
    output.push_str(&format!("Design: {}\n", tree.metadata.design_name));
    output.push_str(&format!("Safety Goal: {}\n", tree.safety_goal));
    output.push_str(&format!("Target ASIL: {:?}\n", tree.metadata.target_asil));
    output.push_str(&format!(
        "Analysis Date: {}\n\n",
        tree.metadata.analysis_date
    ));

    output.push_str("--- Fault Tree Structure ---\n");
    output.push_str(&format!("Total nodes: {}\n", tree.node_count()));
    output.push_str(&format!("Basic events: {}\n", tree.basic_event_count()));
    output.push_str(&format!("Gates: {}\n\n", tree.gate_count()));

    output.push_str("--- Cut Set Analysis ---\n");
    output.push_str(&format!(
        "Top event probability: {:.2e}\n",
        analysis.top_event_probability
    ));
    output.push_str(&format!(
        "Total minimal cut sets: {}\n",
        analysis.total_cut_sets()
    ));
    output.push_str(&format!(
        "Maximum cut set order: {}\n",
        analysis.max_order()
    ));

    output.push_str("\nCut sets by order:\n");
    let mut orders: Vec<_> = analysis.cut_sets_by_order.iter().collect();
    orders.sort_by_key(|&(k, _)| k);
    for (order, count) in orders {
        output.push_str(&format!("  Order {}: {} cut sets\n", order, count));
    }

    if !analysis.single_point_failures.is_empty() {
        output.push_str(&format!(
            "\n*** SINGLE POINT FAILURES: {} ***\n",
            analysis.single_point_failures.len()
        ));
        for spf in &analysis.single_point_failures {
            output.push_str(&format!(
                "  - {} (P={:.2e})\n",
                spf.event_names.join(", "),
                spf.probability
            ));
        }
    }

    if !analysis.dominant_cut_sets.is_empty() {
        output.push_str("\nDominant cut sets (>1% contribution):\n");
        for cs in &analysis.dominant_cut_sets {
            output.push_str(&format!(
                "  - {} ({:.1}%)\n",
                cs.event_names.join(" AND "),
                cs.contribution_percentage
            ));
        }
    }

    output.push_str("\n--- Importance Measures ---\n");
    let mut importance: Vec<_> = analysis.importance_measures.values().collect();
    importance.sort_by(|a, b| b.fussell_vesely.partial_cmp(&a.fussell_vesely).unwrap());

    output.push_str(&format!(
        "{:<30} {:>10} {:>10} {:>10}\n",
        "Event", "F-V", "Birnbaum", "Criticality"
    ));
    output.push_str(&format!("{}\n", "-".repeat(62)));
    for im in importance.iter().take(10) {
        output.push_str(&format!(
            "{:<30} {:>10.4} {:>10.4} {:>10.2e}\n",
            im.event_name, im.fussell_vesely, im.birnbaum, im.criticality
        ));
    }

    output
}

// ============================================================================
// Export Formats for Standard FTA Tools
// ============================================================================

/// OpenFTA XML export format
///
/// OpenFTA is an open-source FTA tool. This export generates XML compatible
/// with OpenFTA's import format.
pub fn export_openfta_xml(tree: &FaultTree) -> String {
    let mut xml = String::new();

    xml.push_str("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
    xml.push_str("<openfta-model>\n");
    xml.push_str(&format!(
        "  <model name=\"{}\">\n",
        escape_xml(&tree.metadata.name)
    ));
    xml.push_str(&format!(
        "    <description>{}</description>\n",
        escape_xml(&tree.safety_goal)
    ));

    // Primary events (basic events)
    xml.push_str("    <primary-events>\n");
    for node in &tree.nodes {
        if let FtaNodeType::BasicEvent {
            failure_rate,
            exposure_time,
            component,
        } = &node.node_type
        {
            xml.push_str(&format!(
                "      <primary-event id=\"{}\" name=\"{}\">\n",
                node.id.0,
                escape_xml(&node.name)
            ));
            xml.push_str(&format!(
                "        <probability>{:.6e}</probability>\n",
                failure_rate * exposure_time
            ));
            xml.push_str(&format!(
                "        <failure-rate>{:.6e}</failure-rate>\n",
                failure_rate
            ));
            xml.push_str(&format!(
                "        <exposure-time>{}</exposure-time>\n",
                exposure_time
            ));
            if let Some(desc) = &node.description {
                xml.push_str(&format!(
                    "        <description>{}</description>\n",
                    escape_xml(desc)
                ));
            }
            xml.push_str(&format!(
                "        <component>{}</component>\n",
                escape_xml(component)
            ));
            xml.push_str("      </primary-event>\n");
        }
    }
    xml.push_str("    </primary-events>\n");

    // Gates
    xml.push_str("    <gates>\n");
    for node in &tree.nodes {
        if let FtaNodeType::Gate { gate_type, inputs } = &node.node_type {
            let gate_str = match gate_type {
                GateType::And => "AND",
                GateType::Or => "OR",
                GateType::KofN { k, n } => &format!("{}-of-{}", k, n),
                GateType::Pand => "PAND",
                GateType::Not => "NOT",
                GateType::Xor => "XOR",
                GateType::Spare { .. } => "SPARE",
                GateType::Seq => "SEQ",
                GateType::Fdep => "FDEP",
                GateType::Inhibit { .. } => "INHIBIT",
            };

            xml.push_str(&format!(
                "      <gate id=\"{}\" name=\"{}\" type=\"{}\">\n",
                node.id.0,
                escape_xml(&node.name),
                gate_str
            ));

            // Add gate-specific parameters
            match gate_type {
                GateType::KofN { k, n } => {
                    xml.push_str(&format!("        <k>{}</k>\n", k));
                    xml.push_str(&format!("        <n>{}</n>\n", n));
                }
                GateType::Spare {
                    switching_probability,
                    dormancy_factor,
                } => {
                    xml.push_str(&format!(
                        "        <switching-probability>{}</switching-probability>\n",
                        switching_probability
                    ));
                    xml.push_str(&format!(
                        "        <dormancy-factor>{}</dormancy-factor>\n",
                        dormancy_factor
                    ));
                }
                GateType::Inhibit {
                    condition_probability,
                } => {
                    xml.push_str(&format!(
                        "        <condition-probability>{}</condition-probability>\n",
                        condition_probability
                    ));
                }
                _ => {}
            }

            xml.push_str("        <inputs>\n");
            for input in inputs {
                xml.push_str(&format!("          <input ref=\"{}\"/>\n", input.0));
            }
            xml.push_str("        </inputs>\n");

            if let Some(desc) = &node.description {
                xml.push_str(&format!(
                    "        <description>{}</description>\n",
                    escape_xml(desc)
                ));
            }
            xml.push_str("      </gate>\n");
        }
    }
    xml.push_str("    </gates>\n");

    // Top event reference
    xml.push_str(&format!("    <top-event ref=\"{}\"/>\n", tree.top_event.0));

    xml.push_str("  </model>\n");
    xml.push_str("</openfta-model>\n");

    xml
}

/// Isograph FTA export format (simplified XML)
///
/// Isograph Reliability Workbench uses a proprietary format.
/// This export provides a compatible subset.
pub fn export_isograph_xml(tree: &FaultTree) -> String {
    let mut xml = String::new();

    xml.push_str("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
    xml.push_str("<fault-tree xmlns=\"http://www.isograph.com/fta\">\n");
    xml.push_str(&format!(
        "  <name>{}</name>\n",
        escape_xml(&tree.metadata.name)
    ));
    xml.push_str(&format!(
        "  <description>{}</description>\n",
        escape_xml(&tree.safety_goal)
    ));
    xml.push_str(&format!(
        "  <design>{}</design>\n",
        escape_xml(&tree.metadata.design_name)
    ));
    xml.push_str(&format!(
        "  <analysis-date>{}</analysis-date>\n",
        tree.metadata.analysis_date.format("%Y-%m-%d")
    ));

    xml.push_str("  <events>\n");
    for node in &tree.nodes {
        match &node.node_type {
            FtaNodeType::BasicEvent {
                failure_rate,
                exposure_time,
                component,
            } => {
                xml.push_str(&format!(
                    "    <basic-event id=\"BE{}\" name=\"{}\">\n",
                    node.id.0,
                    escape_xml(&node.name)
                ));
                xml.push_str(&format!(
                    "      <failure-rate unit=\"per-hour\">{:.6e}</failure-rate>\n",
                    failure_rate
                ));
                xml.push_str(&format!(
                    "      <mission-time unit=\"hours\">{}</mission-time>\n",
                    exposure_time
                ));
                xml.push_str(&format!(
                    "      <component>{}</component>\n",
                    escape_xml(component)
                ));
                xml.push_str("    </basic-event>\n");
            }
            FtaNodeType::UndevelopedEvent { description } => {
                xml.push_str(&format!(
                    "    <undeveloped-event id=\"UE{}\" name=\"{}\">\n",
                    node.id.0,
                    escape_xml(&node.name)
                ));
                xml.push_str(&format!(
                    "      <description>{}</description>\n",
                    escape_xml(description)
                ));
                xml.push_str("    </undeveloped-event>\n");
            }
            FtaNodeType::HouseEvent { state } => {
                xml.push_str(&format!(
                    "    <house-event id=\"HE{}\" name=\"{}\" state=\"{}\"/>\n",
                    node.id.0,
                    escape_xml(&node.name),
                    if *state { "true" } else { "false" }
                ));
            }
            _ => {}
        }
    }
    xml.push_str("  </events>\n");

    xml.push_str("  <gates>\n");
    for node in &tree.nodes {
        if let FtaNodeType::Gate { gate_type, inputs } = &node.node_type {
            let (gate_str, attrs) = match gate_type {
                GateType::And => ("and-gate", String::new()),
                GateType::Or => ("or-gate", String::new()),
                GateType::KofN { k, n } => ("voting-gate", format!(" k=\"{}\" n=\"{}\"", k, n)),
                GateType::Pand => ("pand-gate", String::new()),
                GateType::Not => ("not-gate", String::new()),
                GateType::Xor => ("xor-gate", String::new()),
                GateType::Spare {
                    switching_probability,
                    dormancy_factor,
                } => (
                    "spare-gate",
                    format!(
                        " switch-prob=\"{}\" dormancy=\"{}\"",
                        switching_probability, dormancy_factor
                    ),
                ),
                GateType::Seq => ("seq-gate", String::new()),
                GateType::Fdep => ("fdep-gate", String::new()),
                GateType::Inhibit {
                    condition_probability,
                } => (
                    "inhibit-gate",
                    format!(" condition=\"{}\"", condition_probability),
                ),
            };

            xml.push_str(&format!(
                "    <{} id=\"G{}\" name=\"{}\"{}>\n",
                gate_str,
                node.id.0,
                escape_xml(&node.name),
                attrs
            ));
            xml.push_str("      <inputs>\n");
            for input in inputs {
                let input_node = tree.get_node(*input);
                let prefix = if input_node.is_some_and(|n| n.is_basic_event()) {
                    "BE"
                } else {
                    "G"
                };
                xml.push_str(&format!("        <input ref=\"{}{}\"/>\n", prefix, input.0));
            }
            xml.push_str("      </inputs>\n");
            xml.push_str(&format!("    </{}>\n", gate_str));
        }
    }
    xml.push_str("  </gates>\n");

    let top_prefix = if tree
        .get_node(tree.top_event)
        .is_some_and(|n| n.is_basic_event())
    {
        "BE"
    } else {
        "G"
    };
    xml.push_str(&format!(
        "  <top-event ref=\"{}{}\"/>\n",
        top_prefix, tree.top_event.0
    ));

    xml.push_str("</fault-tree>\n");

    xml
}

/// Generic JSON interchange format for FTA
///
/// This is a tool-agnostic JSON format that preserves all FTA information
/// and can be easily parsed by any tool or script.
pub fn export_json(tree: &FaultTree, analysis: Option<&CutSetAnalysis>) -> String {
    let mut json_tree = serde_json::json!({
        "format": "skalp-fta-v1",
        "metadata": {
            "id": tree.metadata.id,
            "name": tree.metadata.name,
            "design": tree.metadata.design_name,
            "safety_goal": tree.safety_goal,
            "target_asil": format!("{:?}", tree.metadata.target_asil),
            "analysis_date": tree.metadata.analysis_date.to_rfc3339(),
            "analyst": tree.metadata.analyst,
        },
        "top_event_id": tree.top_event.0,
        "nodes": tree.nodes.iter().map(|n| {
            let mut node = serde_json::json!({
                "id": n.id.0,
                "name": n.name,
            });

            if let Some(desc) = &n.description {
                node["description"] = serde_json::json!(desc);
            }

            if let Some(prob) = n.probability {
                node["probability"] = serde_json::json!(prob);
            }

            match &n.node_type {
                FtaNodeType::BasicEvent { failure_rate, exposure_time, component } => {
                    node["type"] = serde_json::json!("basic_event");
                    node["failure_rate"] = serde_json::json!(failure_rate);
                    node["exposure_time"] = serde_json::json!(exposure_time);
                    node["component"] = serde_json::json!(component);
                }
                FtaNodeType::Gate { gate_type, inputs } => {
                    node["type"] = serde_json::json!("gate");
                    node["gate_type"] = serde_json::json!(format!("{:?}", gate_type));
                    node["inputs"] = serde_json::json!(inputs.iter().map(|i| i.0).collect::<Vec<_>>());
                }
                FtaNodeType::UndevelopedEvent { description } => {
                    node["type"] = serde_json::json!("undeveloped");
                    node["event_description"] = serde_json::json!(description);
                }
                FtaNodeType::HouseEvent { state } => {
                    node["type"] = serde_json::json!("house");
                    node["state"] = serde_json::json!(state);
                }
                FtaNodeType::TransferIn { target_tree, target_node } => {
                    node["type"] = serde_json::json!("transfer");
                    node["target_tree"] = serde_json::json!(target_tree);
                    node["target_node"] = serde_json::json!(target_node.0);
                }
                FtaNodeType::TopEvent { description, safety_goal_id } => {
                    node["type"] = serde_json::json!("top_event");
                    node["event_description"] = serde_json::json!(description);
                    if let Some(sg_id) = safety_goal_id {
                        node["safety_goal_id"] = serde_json::json!(sg_id);
                    }
                }
            }

            node
        }).collect::<Vec<_>>(),
    });

    if let Some(analysis) = analysis {
        json_tree["analysis"] = serde_json::json!({
            "top_event_probability": analysis.top_event_probability,
            "total_cut_sets": analysis.total_cut_sets(),
            "max_order": analysis.max_order(),
            "cut_sets_by_order": analysis.cut_sets_by_order,
            "single_point_failures": analysis.single_point_failures.iter().map(|cs| {
                serde_json::json!({
                    "events": cs.event_names,
                    "probability": cs.probability,
                    "contribution_percentage": cs.contribution_percentage,
                })
            }).collect::<Vec<_>>(),
            "dominant_cut_sets": analysis.dominant_cut_sets.iter().map(|cs| {
                serde_json::json!({
                    "events": cs.event_names,
                    "order": cs.order,
                    "probability": cs.probability,
                    "contribution_percentage": cs.contribution_percentage,
                })
            }).collect::<Vec<_>>(),
            "importance_measures": analysis.importance_measures.iter().map(|(name, im)| {
                serde_json::json!({
                    "event": name,
                    "fussell_vesely": im.fussell_vesely,
                    "birnbaum": im.birnbaum,
                    "criticality": im.criticality,
                })
            }).collect::<Vec<_>>(),
        });
    }

    serde_json::to_string_pretty(&json_tree).unwrap_or_else(|_| "{}".to_string())
}

/// Galileo FTA format (text-based)
///
/// Galileo is a format used by several academic FTA tools.
/// It uses a simple textual representation.
pub fn export_galileo(tree: &FaultTree) -> String {
    let mut output = String::new();

    output.push_str(&format!("// Galileo FTA export: {}\n", tree.metadata.name));
    output.push_str(&format!("// Safety Goal: {}\n", tree.safety_goal));
    output.push_str(&format!("// Design: {}\n\n", tree.metadata.design_name));

    // Top level declaration
    let top_node = tree.get_node(tree.top_event);
    if let Some(top) = top_node {
        output.push_str(&format!("toplevel \"{}\";\n\n", top.name));
    }

    // Basic events
    output.push_str("// Basic Events\n");
    for node in &tree.nodes {
        if let FtaNodeType::BasicEvent {
            failure_rate,
            exposure_time,
            ..
        } = &node.node_type
        {
            let prob = failure_rate * exposure_time;
            output.push_str(&format!("basic \"{}\" prob={:.6e};\n", node.name, prob));
        }
    }

    output.push('\n');

    // Gates
    output.push_str("// Gates\n");
    for node in &tree.nodes {
        if let FtaNodeType::Gate { gate_type, inputs } = &node.node_type {
            let gate_str = match gate_type {
                GateType::And => "and",
                GateType::Or => "or",
                GateType::KofN { k, .. } => {
                    // Galileo uses atleast for voting gates
                    output.push_str(&format!("\"{}\" atleast {} ", node.name, k));
                    let input_names: Vec<String> = inputs
                        .iter()
                        .filter_map(|id| tree.get_node(*id).map(|n| format!("\"{}\"", n.name)))
                        .collect();
                    output.push_str(&input_names.join(" "));
                    output.push_str(";\n");
                    continue;
                }
                GateType::Pand => "pand",
                GateType::Not => "not",
                GateType::Xor => "xor",
                GateType::Spare { .. } => "wsp", // warm spare
                GateType::Seq => "seq",
                GateType::Fdep => "fdep",
                GateType::Inhibit { .. } => "inhibit",
            };

            let input_names: Vec<String> = inputs
                .iter()
                .filter_map(|id| tree.get_node(*id).map(|n| format!("\"{}\"", n.name)))
                .collect();

            output.push_str(&format!(
                "\"{}\" {} {};\n",
                node.name,
                gate_str,
                input_names.join(" ")
            ));
        }
    }

    output
}

/// Helper function to escape XML special characters
fn escape_xml(s: &str) -> String {
    s.replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
        .replace('"', "&quot;")
        .replace('\'', "&apos;")
}

/// Export format enum for convenience
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FtaExportFormat {
    /// OpenFTA XML format
    OpenFtaXml,
    /// Isograph XML format
    IsographXml,
    /// Generic JSON interchange
    Json,
    /// Galileo text format
    Galileo,
    /// Human-readable report
    Report,
}

impl std::str::FromStr for FtaExportFormat {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "openfta" | "openfta-xml" | "openfta_xml" => Ok(FtaExportFormat::OpenFtaXml),
            "isograph" | "isograph-xml" | "isograph_xml" => Ok(FtaExportFormat::IsographXml),
            "json" => Ok(FtaExportFormat::Json),
            "galileo" | "gal" => Ok(FtaExportFormat::Galileo),
            "report" | "txt" | "text" => Ok(FtaExportFormat::Report),
            _ => Err(format!("Unknown FTA export format: {}", s)),
        }
    }
}

/// Export fault tree in the specified format
pub fn export_fta(
    tree: &FaultTree,
    format: FtaExportFormat,
    analysis: Option<&CutSetAnalysis>,
) -> String {
    match format {
        FtaExportFormat::OpenFtaXml => export_openfta_xml(tree),
        FtaExportFormat::IsographXml => export_isograph_xml(tree),
        FtaExportFormat::Json => export_json(tree, analysis),
        FtaExportFormat::Galileo => export_galileo(tree),
        FtaExportFormat::Report => {
            if let Some(analysis) = analysis {
                format_fta_report(tree, analysis)
            } else {
                // Basic report without analysis
                format!(
                    "=== Fault Tree: {} ===\nSafety Goal: {}\nNodes: {}\nBasic Events: {}\nGates: {}\n",
                    tree.metadata.name,
                    tree.safety_goal,
                    tree.node_count(),
                    tree.basic_event_count(),
                    tree.gate_count()
                )
            }
        }
    }
}

/// Get the file extension for an export format
pub fn export_format_extension(format: FtaExportFormat) -> &'static str {
    match format {
        FtaExportFormat::OpenFtaXml => "xml",
        FtaExportFormat::IsographXml => "xml",
        FtaExportFormat::Json => "json",
        FtaExportFormat::Galileo => "gal",
        FtaExportFormat::Report => "txt",
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use chrono::Utc;

    fn create_test_tree() -> FaultTree {
        let metadata = FtaMetadata {
            id: "FTA-001".to_string(),
            name: "Test FTA".to_string(),
            design_name: "test_design".to_string(),
            target_asil: AsilLevel::B,
            analysis_date: Utc::now(),
            analyst: "test".to_string(),
        };

        let mut tree = FaultTree::new(metadata, "Prevent unintended acceleration");

        // Create basic events
        let e1 = tree.add_node(
            "Throttle sensor fault",
            FtaNodeType::BasicEvent {
                failure_rate: 100.0, // 100 FIT
                exposure_time: 1000.0,
                component: "throttle_sensor".to_string(),
            },
        );

        let e2 = tree.add_node(
            "ECU fault",
            FtaNodeType::BasicEvent {
                failure_rate: 50.0, // 50 FIT
                exposure_time: 1000.0,
                component: "ecu".to_string(),
            },
        );

        let e3 = tree.add_node(
            "Actuator fault",
            FtaNodeType::BasicEvent {
                failure_rate: 75.0, // 75 FIT
                exposure_time: 1000.0,
                component: "actuator".to_string(),
            },
        );

        // Create OR gate
        let or_gate = tree.add_node(
            "System failure",
            FtaNodeType::Gate {
                gate_type: GateType::Or,
                inputs: vec![e1, e2, e3],
            },
        );

        // Create top event
        let top = tree.add_node(
            "Unintended acceleration",
            FtaNodeType::TopEvent {
                description: "Vehicle accelerates without driver input".to_string(),
                safety_goal_id: Some("SG-001".to_string()),
            },
        );

        tree.set_top_event(or_gate);
        tree.rebuild_index();

        tree
    }

    #[test]
    fn test_fault_tree_creation() {
        let tree = create_test_tree();

        assert_eq!(tree.node_count(), 5);
        assert_eq!(tree.basic_event_count(), 3);
        assert_eq!(tree.gate_count(), 1);
    }

    #[test]
    fn test_bdd_or_gate() {
        let tree = create_test_tree();
        let bdd = Bdd::from_fault_tree(&tree);
        let cut_sets = bdd.extract_minimal_cut_sets(&tree);

        // OR gate should produce 3 single-event cut sets
        assert_eq!(cut_sets.len(), 3);
        assert!(cut_sets.iter().all(|cs| cs.order == 1));
    }

    #[test]
    fn test_and_gate() {
        let metadata = FtaMetadata {
            id: "FTA-002".to_string(),
            name: "AND Gate Test".to_string(),
            design_name: "test".to_string(),
            target_asil: AsilLevel::B,
            analysis_date: Utc::now(),
            analyst: "test".to_string(),
        };

        let mut tree = FaultTree::new(metadata, "Test AND gate");

        let e1 = tree.add_node(
            "Event A",
            FtaNodeType::BasicEvent {
                failure_rate: 100.0,
                exposure_time: 1000.0,
                component: "a".to_string(),
            },
        );

        let e2 = tree.add_node(
            "Event B",
            FtaNodeType::BasicEvent {
                failure_rate: 100.0,
                exposure_time: 1000.0,
                component: "b".to_string(),
            },
        );

        let and_gate = tree.add_node(
            "Both fail",
            FtaNodeType::Gate {
                gate_type: GateType::And,
                inputs: vec![e1, e2],
            },
        );

        tree.set_top_event(and_gate);
        tree.rebuild_index();

        let bdd = Bdd::from_fault_tree(&tree);
        let cut_sets = bdd.extract_minimal_cut_sets(&tree);

        // AND gate should produce 1 two-event cut set
        assert_eq!(cut_sets.len(), 1);
        assert_eq!(cut_sets[0].order, 2);
    }

    #[test]
    fn test_cut_set_analysis() {
        let tree = create_test_tree();
        let analysis = analyze_fault_tree(&tree);

        assert_eq!(analysis.total_cut_sets(), 3);
        assert_eq!(analysis.single_point_failures.len(), 3);
        assert!(analysis.top_event_probability > 0.0);
    }

    #[test]
    fn test_fta_compliance() {
        let tree = create_test_tree();
        let analysis = analyze_fault_tree(&tree);
        let compliance = verify_fta_compliance(&analysis, AsilLevel::B);

        // With 3 SPFs at 100, 50, 75 FIT, PMHF should be ~225 FIT
        // ASIL B target is 100 FIT, so should not be compliant
        assert!(!compliance.spf_compliant || !compliance.pmhf_compliant);
        assert_eq!(compliance.single_point_count, 3);
    }

    #[test]
    fn test_importance_measures() {
        let tree = create_test_tree();
        let analysis = analyze_fault_tree(&tree);

        assert_eq!(analysis.importance_measures.len(), 3);

        // All events should have some importance
        for im in analysis.importance_measures.values() {
            assert!(im.fussell_vesely >= 0.0);
            assert!(im.fussell_vesely <= 1.0);
        }
    }

    #[test]
    fn test_format_report() {
        let tree = create_test_tree();
        let analysis = analyze_fault_tree(&tree);
        let report = format_fta_report(&tree, &analysis);

        assert!(report.contains("Fault Tree Analysis Report"));
        assert!(report.contains("test_design"));
        assert!(report.contains("SINGLE POINT FAILURES"));
    }

    #[test]
    fn test_kofn_gate() {
        let metadata = FtaMetadata {
            id: "FTA-003".to_string(),
            name: "2-of-3 Test".to_string(),
            design_name: "test".to_string(),
            target_asil: AsilLevel::C,
            analysis_date: Utc::now(),
            analyst: "test".to_string(),
        };

        let mut tree = FaultTree::new(metadata, "Test 2-of-3");

        let e1 = tree.add_node(
            "Channel A",
            FtaNodeType::BasicEvent {
                failure_rate: 100.0,
                exposure_time: 1000.0,
                component: "ch_a".to_string(),
            },
        );

        let e2 = tree.add_node(
            "Channel B",
            FtaNodeType::BasicEvent {
                failure_rate: 100.0,
                exposure_time: 1000.0,
                component: "ch_b".to_string(),
            },
        );

        let e3 = tree.add_node(
            "Channel C",
            FtaNodeType::BasicEvent {
                failure_rate: 100.0,
                exposure_time: 1000.0,
                component: "ch_c".to_string(),
            },
        );

        let kofn_gate = tree.add_node(
            "2-of-3 fail",
            FtaNodeType::Gate {
                gate_type: GateType::KofN { k: 2, n: 3 },
                inputs: vec![e1, e2, e3],
            },
        );

        tree.set_top_event(kofn_gate);
        tree.rebuild_index();

        let bdd = Bdd::from_fault_tree(&tree);
        let cut_sets = bdd.extract_minimal_cut_sets(&tree);

        // 2-of-3 should produce 3 two-event cut sets: (A,B), (A,C), (B,C)
        assert_eq!(cut_sets.len(), 3);
        assert!(cut_sets.iter().all(|cs| cs.order == 2));
    }

    // ===== Dynamic Gate Tests =====

    #[test]
    fn test_spare_gate() {
        let metadata = FtaMetadata {
            id: "FTA-SPARE".to_string(),
            name: "SPARE Gate Test".to_string(),
            design_name: "test".to_string(),
            target_asil: AsilLevel::D,
            analysis_date: Utc::now(),
            analyst: "test".to_string(),
        };

        let mut tree = FaultTree::new(metadata, "Test SPARE gate");

        // Primary and spare components
        let primary = tree.add_basic_event("Primary Unit", 100.0, 1000.0, "primary");
        let spare1 = tree.add_basic_event("Spare Unit 1", 100.0, 1000.0, "spare1");
        let spare2 = tree.add_basic_event("Spare Unit 2", 100.0, 1000.0, "spare2");

        // SPARE gate: system fails when primary and all spares fail
        let spare_gate = tree.add_spare_gate(
            "Redundant System",
            primary,
            &[spare1, spare2],
            0.99, // 99% switching probability
            0.0,  // Cold spares
        );

        tree.set_top_event(spare_gate);
        tree.rebuild_index();

        let bdd = Bdd::from_fault_tree(&tree);
        let cut_sets = bdd.extract_minimal_cut_sets(&tree);

        // SPARE gate for cut set analysis: all must fail
        // Should produce 1 three-event cut set
        assert_eq!(cut_sets.len(), 1);
        assert_eq!(cut_sets[0].order, 3);
    }

    #[test]
    fn test_seq_gate() {
        let metadata = FtaMetadata {
            id: "FTA-SEQ".to_string(),
            name: "SEQ Gate Test".to_string(),
            design_name: "test".to_string(),
            target_asil: AsilLevel::C,
            analysis_date: Utc::now(),
            analyst: "test".to_string(),
        };

        let mut tree = FaultTree::new(metadata, "Test SEQ gate");

        let event1 = tree.add_basic_event("First Failure", 100.0, 1000.0, "first");
        let event2 = tree.add_basic_event("Second Failure", 100.0, 1000.0, "second");
        let event3 = tree.add_basic_event("Third Failure", 100.0, 1000.0, "third");

        // SEQ gate: events must occur in order
        let seq_gate = tree.add_seq_gate("Cascading Failure", &[event1, event2, event3]);

        tree.set_top_event(seq_gate);
        tree.rebuild_index();

        let bdd = Bdd::from_fault_tree(&tree);
        let cut_sets = bdd.extract_minimal_cut_sets(&tree);

        // SEQ gate is AND for cut set analysis
        assert_eq!(cut_sets.len(), 1);
        assert_eq!(cut_sets[0].order, 3);
    }

    #[test]
    fn test_fdep_gate() {
        let metadata = FtaMetadata {
            id: "FTA-FDEP".to_string(),
            name: "FDEP Gate Test".to_string(),
            design_name: "test".to_string(),
            target_asil: AsilLevel::B,
            analysis_date: Utc::now(),
            analyst: "test".to_string(),
        };

        let mut tree = FaultTree::new(metadata, "Test FDEP gate");

        // Trigger and dependent events
        let trigger = tree.add_basic_event("Power Supply Failure", 50.0, 1000.0, "psu");
        let dep1 = tree.add_basic_event("Component A", 100.0, 1000.0, "comp_a");
        let dep2 = tree.add_basic_event("Component B", 100.0, 1000.0, "comp_b");

        // FDEP: power supply failure causes A and B to fail
        let fdep_gate = tree.add_fdep_gate("Power Dependency", trigger, &[dep1, dep2]);

        tree.set_top_event(fdep_gate);
        tree.rebuild_index();

        let bdd = Bdd::from_fault_tree(&tree);
        let cut_sets = bdd.extract_minimal_cut_sets(&tree);

        // FDEP: trigger OR (all dependents)
        // Cut sets: {trigger} or {dep1, dep2}
        assert_eq!(cut_sets.len(), 2);
        // One should be order 1 (trigger alone)
        assert!(cut_sets.iter().any(|cs| cs.order == 1));
        // One should be order 2 (both dependents)
        assert!(cut_sets.iter().any(|cs| cs.order == 2));
    }

    #[test]
    fn test_inhibit_gate() {
        let metadata = FtaMetadata {
            id: "FTA-INHIBIT".to_string(),
            name: "INHIBIT Gate Test".to_string(),
            design_name: "test".to_string(),
            target_asil: AsilLevel::A,
            analysis_date: Utc::now(),
            analyst: "test".to_string(),
        };

        let mut tree = FaultTree::new(metadata, "Test INHIBIT gate");

        let event = tree.add_basic_event("Fault", 100.0, 1000.0, "fault");
        let condition = tree.add_basic_event("Condition", 50.0, 1000.0, "condition");

        // INHIBIT: fault occurs AND condition is satisfied (probability 0.5)
        let inhibit_gate = tree.add_inhibit_gate("Conditional Fault", &[event, condition], 0.5);

        tree.set_top_event(inhibit_gate);
        tree.rebuild_index();

        let bdd = Bdd::from_fault_tree(&tree);
        let cut_sets = bdd.extract_minimal_cut_sets(&tree);

        // INHIBIT is AND for cut sets
        assert_eq!(cut_sets.len(), 1);
        assert_eq!(cut_sets[0].order, 2);
    }

    #[test]
    fn test_convenience_methods() {
        let metadata = FtaMetadata {
            id: "FTA-CONV".to_string(),
            name: "Convenience Test".to_string(),
            design_name: "test".to_string(),
            target_asil: AsilLevel::B,
            analysis_date: Utc::now(),
            analyst: "test".to_string(),
        };

        let mut tree = FaultTree::new(metadata, "Test convenience methods");

        let e1 = tree.add_basic_event("Event 1", 100.0, 1000.0, "e1");
        let e2 = tree.add_basic_event("Event 2", 100.0, 1000.0, "e2");
        let e3 = tree.add_basic_event("Event 3", 100.0, 1000.0, "e3");

        let and_gate = tree.add_and_gate("AND Gate", &[e1, e2]);
        let or_gate = tree.add_or_gate("OR Gate", &[and_gate, e3]);

        tree.set_top_event(or_gate);
        tree.rebuild_index();

        assert_eq!(tree.node_count(), 5); // 3 events + 2 gates
        assert_eq!(tree.basic_event_count(), 3);
        assert_eq!(tree.gate_count(), 2);

        let bdd = Bdd::from_fault_tree(&tree);
        let cut_sets = bdd.extract_minimal_cut_sets(&tree);

        // Should have {e1, e2} and {e3}
        assert_eq!(cut_sets.len(), 2);
    }

    // ========== Export Format Tests ==========

    #[test]
    fn test_export_openfta_xml() {
        let tree = create_test_tree();
        let xml = export_openfta_xml(&tree);

        // Check basic XML structure
        assert!(xml.contains("<?xml version=\"1.0\" encoding=\"UTF-8\"?>"));
        assert!(xml.contains("<openfta-model>"));
        assert!(xml.contains("<primary-events>"));
        assert!(xml.contains("<gates>"));
        assert!(xml.contains("<primary-event"));
        assert!(xml.contains("<gate"));
        assert!(xml.contains("</openfta-model>"));

        // Check content
        assert!(xml.contains("Throttle sensor fault"));
        assert!(xml.contains("ECU fault"));
        assert!(xml.contains("Actuator fault"));
    }

    #[test]
    fn test_export_isograph_xml() {
        let tree = create_test_tree();
        let xml = export_isograph_xml(&tree);

        // Check Isograph XML structure
        assert!(xml.contains("<?xml version=\"1.0\""));
        assert!(xml.contains("<fault-tree"));
        assert!(xml.contains("<events>"));
        assert!(xml.contains("<gates>"));
        assert!(xml.contains("<basic-event"));
        assert!(xml.contains("<or-gate"));
        assert!(xml.contains("</fault-tree>"));

        // Check that failure rates are included
        assert!(xml.contains("<failure-rate"));
    }

    #[test]
    fn test_export_json() {
        let tree = create_test_tree();
        let json = export_json(&tree, None);

        // Parse the JSON to verify structure
        let parsed: serde_json::Value = serde_json::from_str(&json).expect("Valid JSON");

        assert_eq!(parsed["format"], "skalp-fta-v1");
        assert!(parsed["metadata"]["id"].is_string());
        assert!(parsed["metadata"]["name"].is_string());
        assert!(parsed["nodes"].is_array());
        assert!(parsed["top_event_id"].is_number());

        // Check nodes array has content
        let nodes = parsed["nodes"].as_array().unwrap();
        assert!(!nodes.is_empty());
    }

    #[test]
    fn test_export_json_with_analysis() {
        let tree = create_test_tree();
        let analysis = analyze_fault_tree(&tree);

        let json = export_json(&tree, Some(&analysis));
        let parsed: serde_json::Value = serde_json::from_str(&json).expect("Valid JSON");

        // Should have analysis section
        assert!(parsed["analysis"].is_object());
        assert!(parsed["analysis"]["top_event_probability"].is_number());
        assert!(parsed["analysis"]["total_cut_sets"].is_number());
    }

    #[test]
    fn test_export_galileo() {
        let tree = create_test_tree();
        let output = export_galileo(&tree);

        // Check Galileo format structure
        assert!(output.contains("toplevel"));
        assert!(output.contains("basic"));
        assert!(output.contains("prob="));
        assert!(output.contains("or"));

        // Check event names are quoted
        assert!(output.contains("\"Throttle sensor fault\""));
        assert!(output.contains("\"ECU fault\""));
    }

    #[test]
    fn test_export_format_enum() {
        // Test FromStr
        assert_eq!(
            "openfta".parse::<FtaExportFormat>().unwrap(),
            FtaExportFormat::OpenFtaXml
        );
        assert_eq!(
            "isograph-xml".parse::<FtaExportFormat>().unwrap(),
            FtaExportFormat::IsographXml
        );
        assert_eq!(
            "json".parse::<FtaExportFormat>().unwrap(),
            FtaExportFormat::Json
        );
        assert_eq!(
            "galileo".parse::<FtaExportFormat>().unwrap(),
            FtaExportFormat::Galileo
        );
        assert_eq!(
            "report".parse::<FtaExportFormat>().unwrap(),
            FtaExportFormat::Report
        );

        // Test file extensions
        assert_eq!(export_format_extension(FtaExportFormat::OpenFtaXml), "xml");
        assert_eq!(export_format_extension(FtaExportFormat::Json), "json");
        assert_eq!(export_format_extension(FtaExportFormat::Galileo), "gal");
    }

    #[test]
    fn test_export_fta_unified() {
        let tree = create_test_tree();

        // Test each format through unified interface
        let xml = export_fta(&tree, FtaExportFormat::OpenFtaXml, None);
        assert!(xml.contains("<openfta-model>"));

        let isograph = export_fta(&tree, FtaExportFormat::IsographXml, None);
        assert!(isograph.contains("<fault-tree"));

        let json = export_fta(&tree, FtaExportFormat::Json, None);
        assert!(json.contains("\"format\": \"skalp-fta-v1\""));

        let galileo = export_fta(&tree, FtaExportFormat::Galileo, None);
        assert!(galileo.contains("toplevel"));

        let report = export_fta(&tree, FtaExportFormat::Report, None);
        assert!(report.contains("Fault Tree:"));
    }

    #[test]
    fn test_xml_escape() {
        // Test XML special character escaping
        let escaped = escape_xml("a < b & c > d \"quote\" 'apos'");
        assert_eq!(
            escaped,
            "a &lt; b &amp; c &gt; d &quot;quote&quot; &apos;apos&apos;"
        );
    }
}
