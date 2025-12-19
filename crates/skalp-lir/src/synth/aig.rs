//! And-Inverter Graph (AIG) Data Structure
//!
//! AIGs represent Boolean functions using only AND gates and inverters.
//! This canonical form enables efficient optimization algorithms like
//! structural hashing and cut-based rewriting.

use crate::gate_netlist::{CellId, CellSafetyClassification, GateNetId};
use std::collections::HashMap;

/// Unique identifier for an AIG node
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct AigNodeId(pub u32);

impl AigNodeId {
    /// The constant false node
    pub const FALSE: AigNodeId = AigNodeId(0);

    /// The constant true node (inverted false)
    pub const TRUE: AigNodeId = AigNodeId(0);
}

/// A literal in the AIG (node reference with optional inversion)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct AigLit {
    /// The referenced node
    pub node: AigNodeId,
    /// Whether this reference is inverted
    pub inverted: bool,
}

impl AigLit {
    /// Create a new positive literal
    pub fn new(node: AigNodeId) -> Self {
        Self {
            node,
            inverted: false,
        }
    }

    /// Create a new negative (inverted) literal
    pub fn not(node: AigNodeId) -> Self {
        Self {
            node,
            inverted: true,
        }
    }

    /// Create the constant false literal
    pub fn false_lit() -> Self {
        Self {
            node: AigNodeId::FALSE,
            inverted: false,
        }
    }

    /// Create the constant true literal
    pub fn true_lit() -> Self {
        Self {
            node: AigNodeId::TRUE,
            inverted: true,
        }
    }

    /// Invert this literal
    pub fn invert(self) -> Self {
        Self {
            node: self.node,
            inverted: !self.inverted,
        }
    }

    /// Check if this is a constant
    pub fn is_const(&self) -> bool {
        self.node == AigNodeId::FALSE
    }

    /// Get constant value if this is a constant literal
    pub fn const_value(&self) -> Option<bool> {
        if self.node == AigNodeId::FALSE {
            Some(self.inverted)
        } else {
            None
        }
    }
}

/// An AIG node
#[derive(Debug, Clone)]
pub enum AigNode {
    /// Constant false (node 0)
    Const,

    /// Primary input
    Input {
        /// Input name
        name: String,
        /// Source net ID from gate netlist (for traceability)
        source_net: Option<GateNetId>,
    },

    /// AND gate with two inputs
    And {
        /// Left input
        left: AigLit,
        /// Right input
        right: AigLit,
    },

    /// Latch (sequential element)
    Latch {
        /// Data input
        data: AigLit,
        /// Initial value (if known)
        init: Option<bool>,
        /// Clock input node
        clock: Option<AigNodeId>,
        /// Reset input node
        reset: Option<AigNodeId>,
    },
}

impl AigNode {
    /// Check if this is an AND node
    pub fn is_and(&self) -> bool {
        matches!(self, AigNode::And { .. })
    }

    /// Check if this is an input node
    pub fn is_input(&self) -> bool {
        matches!(self, AigNode::Input { .. })
    }

    /// Check if this is a latch
    pub fn is_latch(&self) -> bool {
        matches!(self, AigNode::Latch { .. })
    }

    /// Check if this is the constant node
    pub fn is_const(&self) -> bool {
        matches!(self, AigNode::Const)
    }

    /// Get the fanin literals for this node
    pub fn fanins(&self) -> Vec<AigLit> {
        match self {
            AigNode::Const | AigNode::Input { .. } => vec![],
            AigNode::And { left, right } => vec![*left, *right],
            AigNode::Latch { data, .. } => vec![*data],
        }
    }
}

/// Safety information preserved through AIG transformations
#[derive(Debug, Clone, Default)]
pub struct AigSafetyInfo {
    /// FIT rate (maximum from merged nodes for conservative estimate)
    pub fit: f64,
    /// Safety classification
    pub classification: Option<CellSafetyClassification>,
    /// Source cells from gate netlist (for traceability)
    pub source_cells: Vec<CellId>,
}

impl AigSafetyInfo {
    /// Create empty safety info
    pub fn empty() -> Self {
        Self::default()
    }

    /// Create safety info from a single cell
    pub fn from_cell(cell_id: CellId, fit: f64, classification: CellSafetyClassification) -> Self {
        Self {
            fit,
            classification: Some(classification),
            source_cells: vec![cell_id],
        }
    }

    /// Merge two safety infos (conservative: max FIT, union of sources)
    pub fn merge(&self, other: &AigSafetyInfo) -> Self {
        let merged_classification = match (&self.classification, &other.classification) {
            (None, None) => None,
            (Some(c), None) | (None, Some(c)) => Some(c.clone()),
            (Some(a), Some(b)) => {
                // If either is a safety mechanism, the merged result is SM (conservative)
                if a.is_safety_mechanism() || b.is_safety_mechanism() {
                    if a.is_safety_mechanism() {
                        Some(a.clone())
                    } else {
                        Some(b.clone())
                    }
                } else {
                    Some(a.clone())
                }
            }
        };

        let mut sources = self.source_cells.clone();
        for cell in &other.source_cells {
            if !sources.contains(cell) {
                sources.push(*cell);
            }
        }

        Self {
            fit: self.fit.max(other.fit),
            classification: merged_classification,
            source_cells: sources,
        }
    }
}

/// And-Inverter Graph
#[derive(Debug, Clone)]
pub struct Aig {
    /// Design name
    pub name: String,

    /// All nodes (index 0 is always the constant false node)
    nodes: Vec<AigNode>,

    /// Primary outputs as (name, literal) pairs
    outputs: Vec<(String, AigLit)>,

    /// Safety info per node (parallel to nodes vec)
    safety_info: Vec<AigSafetyInfo>,

    /// Mapping from gate net IDs to AIG literals (for building)
    net_to_lit: HashMap<GateNetId, AigLit>,

    /// Structural hash for AND nodes: (left, right) -> node_id
    strash_map: HashMap<(AigLit, AigLit), AigNodeId>,
}

impl Aig {
    /// Create a new empty AIG
    pub fn new(name: String) -> Self {
        let mut aig = Self {
            name,
            nodes: Vec::new(),
            outputs: Vec::new(),
            safety_info: Vec::new(),
            net_to_lit: HashMap::new(),
            strash_map: HashMap::new(),
        };

        // Node 0 is always constant false
        aig.nodes.push(AigNode::Const);
        aig.safety_info.push(AigSafetyInfo::empty());

        aig
    }

    /// Get the number of nodes
    pub fn node_count(&self) -> usize {
        self.nodes.len()
    }

    /// Get the number of AND nodes
    pub fn and_count(&self) -> usize {
        self.nodes.iter().filter(|n| n.is_and()).count()
    }

    /// Get the number of input nodes
    pub fn input_count(&self) -> usize {
        self.nodes.iter().filter(|n| n.is_input()).count()
    }

    /// Get the number of latch nodes
    pub fn latch_count(&self) -> usize {
        self.nodes.iter().filter(|n| n.is_latch()).count()
    }

    /// Get the number of outputs
    pub fn output_count(&self) -> usize {
        self.outputs.len()
    }

    /// Get a node by ID
    pub fn get_node(&self, id: AigNodeId) -> Option<&AigNode> {
        self.nodes.get(id.0 as usize)
    }

    /// Get safety info for a node
    pub fn get_safety_info(&self, id: AigNodeId) -> Option<&AigSafetyInfo> {
        self.safety_info.get(id.0 as usize)
    }

    /// Get all outputs
    pub fn outputs(&self) -> &[(String, AigLit)] {
        &self.outputs
    }

    /// Iterate over all nodes
    pub fn iter_nodes(&self) -> impl Iterator<Item = (AigNodeId, &AigNode)> {
        self.nodes
            .iter()
            .enumerate()
            .map(|(i, n)| (AigNodeId(i as u32), n))
    }

    /// Iterate over all AND nodes
    pub fn iter_and_nodes(&self) -> impl Iterator<Item = (AigNodeId, AigLit, AigLit)> + '_ {
        self.nodes.iter().enumerate().filter_map(|(i, n)| match n {
            AigNode::And { left, right } => Some((AigNodeId(i as u32), *left, *right)),
            _ => None,
        })
    }

    /// Iterate over all input nodes
    pub fn iter_inputs(&self) -> impl Iterator<Item = (AigNodeId, &str)> + '_ {
        self.nodes.iter().enumerate().filter_map(|(i, n)| match n {
            AigNode::Input { name, .. } => Some((AigNodeId(i as u32), name.as_str())),
            _ => None,
        })
    }

    /// Iterate over all latch nodes
    pub fn iter_latches(&self) -> impl Iterator<Item = (AigNodeId, &AigNode)> + '_ {
        self.nodes.iter().enumerate().filter_map(|(i, n)| match n {
            AigNode::Latch { .. } => Some((AigNodeId(i as u32), n)),
            _ => None,
        })
    }

    /// Add a primary input
    pub fn add_input(&mut self, name: String, source_net: Option<GateNetId>) -> AigNodeId {
        let id = AigNodeId(self.nodes.len() as u32);
        self.nodes.push(AigNode::Input { name, source_net });
        self.safety_info.push(AigSafetyInfo::empty());
        id
    }

    /// Add a primary input with safety info
    pub fn add_input_with_safety(
        &mut self,
        name: String,
        source_net: Option<GateNetId>,
        safety: AigSafetyInfo,
    ) -> AigNodeId {
        let id = AigNodeId(self.nodes.len() as u32);
        self.nodes.push(AigNode::Input { name, source_net });
        self.safety_info.push(safety);
        id
    }

    /// Add an AND gate with structural hashing
    pub fn add_and(&mut self, mut left: AigLit, mut right: AigLit) -> AigLit {
        // Normalize: smaller node ID first (for structural hashing)
        if left.node.0 > right.node.0 {
            std::mem::swap(&mut left, &mut right);
        }

        // Trivial cases
        // a & 0 = 0
        if left.is_const() && !left.inverted {
            return AigLit::false_lit();
        }
        // a & 1 = a
        if left.is_const() && left.inverted {
            return right;
        }
        // a & a = a
        if left == right {
            return left;
        }
        // a & !a = 0
        if left.node == right.node && left.inverted != right.inverted {
            return AigLit::false_lit();
        }

        // Check strash map
        let key = (left, right);
        if let Some(&existing) = self.strash_map.get(&key) {
            return AigLit::new(existing);
        }

        // Create new AND node
        let id = AigNodeId(self.nodes.len() as u32);
        self.nodes.push(AigNode::And { left, right });

        // Merge safety info from inputs
        let left_safety = self.safety_info.get(left.node.0 as usize).cloned();
        let right_safety = self.safety_info.get(right.node.0 as usize).cloned();
        let merged = match (left_safety, right_safety) {
            (Some(a), Some(b)) => a.merge(&b),
            (Some(a), None) | (None, Some(a)) => a,
            (None, None) => AigSafetyInfo::empty(),
        };
        self.safety_info.push(merged);

        self.strash_map.insert(key, id);
        AigLit::new(id)
    }

    /// Add an AND gate with explicit safety info
    pub fn add_and_with_safety(
        &mut self,
        left: AigLit,
        right: AigLit,
        safety: AigSafetyInfo,
    ) -> AigLit {
        let lit = self.add_and(left, right);
        // Override the auto-merged safety info
        if let Some(info) = self.safety_info.get_mut(lit.node.0 as usize) {
            *info = safety;
        }
        lit
    }

    /// Add a latch (sequential element)
    pub fn add_latch(
        &mut self,
        data: AigLit,
        init: Option<bool>,
        clock: Option<AigNodeId>,
        reset: Option<AigNodeId>,
    ) -> AigNodeId {
        let id = AigNodeId(self.nodes.len() as u32);
        self.nodes.push(AigNode::Latch {
            data,
            init,
            clock,
            reset,
        });

        // Copy safety from data input
        let data_safety = self
            .safety_info
            .get(data.node.0 as usize)
            .cloned()
            .unwrap_or_default();
        self.safety_info.push(data_safety);

        id
    }

    /// Add a latch with explicit safety info
    pub fn add_latch_with_safety(
        &mut self,
        data: AigLit,
        init: Option<bool>,
        clock: Option<AigNodeId>,
        reset: Option<AigNodeId>,
        safety: AigSafetyInfo,
    ) -> AigNodeId {
        let id = AigNodeId(self.nodes.len() as u32);
        self.nodes.push(AigNode::Latch {
            data,
            init,
            clock,
            reset,
        });
        self.safety_info.push(safety);
        id
    }

    /// Add a primary output
    pub fn add_output(&mut self, name: String, lit: AigLit) {
        self.outputs.push((name, lit));
    }

    /// Register a net mapping (used during building)
    pub fn register_net(&mut self, net_id: GateNetId, lit: AigLit) {
        self.net_to_lit.insert(net_id, lit);
    }

    /// Look up a net mapping
    pub fn get_net_lit(&self, net_id: GateNetId) -> Option<AigLit> {
        self.net_to_lit.get(&net_id).copied()
    }

    /// Build an OR gate: a | b = !(!a & !b)
    pub fn add_or(&mut self, left: AigLit, right: AigLit) -> AigLit {
        let nand = self.add_and(left.invert(), right.invert());
        nand.invert()
    }

    /// Build an XOR gate: a ^ b = (a & !b) | (!a & b)
    pub fn add_xor(&mut self, left: AigLit, right: AigLit) -> AigLit {
        let and1 = self.add_and(left, right.invert());
        let and2 = self.add_and(left.invert(), right);
        self.add_or(and1, and2)
    }

    /// Build a MUX: sel ? then : else = (sel & then) | (!sel & else)
    pub fn add_mux(&mut self, sel: AigLit, then_lit: AigLit, else_lit: AigLit) -> AigLit {
        let sel_then = self.add_and(sel, then_lit);
        let not_sel_else = self.add_and(sel.invert(), else_lit);
        self.add_or(sel_then, not_sel_else)
    }

    /// Compute AIG statistics
    pub fn compute_stats(&self) -> AigStats {
        let and_count = self.and_count();
        let input_count = self.input_count();
        let latch_count = self.latch_count();
        let output_count = self.output_count();

        // Compute levels (depth) using topological order
        let mut levels = vec![0u32; self.nodes.len()];
        for (id, node) in self.iter_nodes() {
            if let AigNode::And { left, right } = node {
                let left_level = levels[left.node.0 as usize];
                let right_level = levels[right.node.0 as usize];
                levels[id.0 as usize] = left_level.max(right_level) + 1;
            }
        }
        let max_level = *levels.iter().max().unwrap_or(&0);

        // Compute fanout
        let mut fanout = vec![0u32; self.nodes.len()];
        for node in &self.nodes {
            for lit in node.fanins() {
                fanout[lit.node.0 as usize] += 1;
            }
        }
        for (_, lit) in &self.outputs {
            fanout[lit.node.0 as usize] += 1;
        }
        let max_fanout = *fanout.iter().max().unwrap_or(&0);
        let avg_fanout = if self.nodes.len() > 1 {
            fanout.iter().sum::<u32>() as f64 / (self.nodes.len() - 1) as f64
        } else {
            0.0
        };

        // Total FIT
        let total_fit: f64 = self.safety_info.iter().map(|s| s.fit).sum();

        AigStats {
            node_count: self.nodes.len(),
            and_count,
            input_count,
            output_count,
            latch_count,
            max_level,
            max_fanout,
            avg_fanout,
            total_fit,
        }
    }

    /// Clear the structural hash (for rebuilding)
    pub fn clear_strash(&mut self) {
        self.strash_map.clear();
    }

    /// Rebuild structural hash from current nodes
    pub fn rebuild_strash(&mut self) {
        self.strash_map.clear();
        for (id, node) in self.nodes.iter().enumerate() {
            if let AigNode::And { left, right } = node {
                let mut l = *left;
                let mut r = *right;
                if l.node.0 > r.node.0 {
                    std::mem::swap(&mut l, &mut r);
                }
                self.strash_map.insert((l, r), AigNodeId(id as u32));
            }
        }
    }
}

/// Statistics for an AIG
#[derive(Debug, Clone, Default)]
pub struct AigStats {
    /// Total nodes (including const)
    pub node_count: usize,
    /// Number of AND nodes
    pub and_count: usize,
    /// Number of input nodes
    pub input_count: usize,
    /// Number of output nodes
    pub output_count: usize,
    /// Number of latch nodes
    pub latch_count: usize,
    /// Maximum logic level (depth)
    pub max_level: u32,
    /// Maximum fanout
    pub max_fanout: u32,
    /// Average fanout
    pub avg_fanout: f64,
    /// Total FIT rate
    pub total_fit: f64,
}

impl std::fmt::Display for AigStats {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "AIG Statistics:")?;
        writeln!(f, "  Nodes: {} ({} AND)", self.node_count, self.and_count)?;
        writeln!(f, "  Inputs: {}", self.input_count)?;
        writeln!(f, "  Outputs: {}", self.output_count)?;
        writeln!(f, "  Latches: {}", self.latch_count)?;
        writeln!(f, "  Levels: {}", self.max_level)?;
        writeln!(f, "  Max fanout: {}", self.max_fanout)?;
        writeln!(f, "  Avg fanout: {:.2}", self.avg_fanout)?;
        writeln!(f, "  Total FIT: {:.4}", self.total_fit)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_aig_creation() {
        let aig = Aig::new("test".to_string());
        assert_eq!(aig.node_count(), 1); // Just const node
        assert_eq!(aig.and_count(), 0);
    }

    #[test]
    fn test_aig_input() {
        let mut aig = Aig::new("test".to_string());
        let a = aig.add_input("a".to_string(), None);
        let b = aig.add_input("b".to_string(), None);

        assert_eq!(aig.input_count(), 2);
        assert!(aig.get_node(a).unwrap().is_input());
        assert!(aig.get_node(b).unwrap().is_input());
    }

    #[test]
    fn test_aig_and() {
        let mut aig = Aig::new("test".to_string());
        let a = aig.add_input("a".to_string(), None);
        let b = aig.add_input("b".to_string(), None);

        let and_lit = aig.add_and(AigLit::new(a), AigLit::new(b));

        assert_eq!(aig.and_count(), 1);
        assert!(!and_lit.inverted);
    }

    #[test]
    fn test_aig_strash() {
        let mut aig = Aig::new("test".to_string());
        let a = aig.add_input("a".to_string(), None);
        let b = aig.add_input("b".to_string(), None);

        // Add same AND twice - should return same node
        let and1 = aig.add_and(AigLit::new(a), AigLit::new(b));
        let and2 = aig.add_and(AigLit::new(a), AigLit::new(b));
        let and3 = aig.add_and(AigLit::new(b), AigLit::new(a)); // Order swapped

        assert_eq!(and1, and2);
        assert_eq!(and1, and3);
        assert_eq!(aig.and_count(), 1);
    }

    #[test]
    fn test_aig_trivial_cases() {
        let mut aig = Aig::new("test".to_string());
        let a = aig.add_input("a".to_string(), None);

        // a & 0 = 0
        let and_zero = aig.add_and(AigLit::new(a), AigLit::false_lit());
        assert_eq!(and_zero, AigLit::false_lit());

        // a & 1 = a
        let and_one = aig.add_and(AigLit::new(a), AigLit::true_lit());
        assert_eq!(and_one, AigLit::new(a));

        // a & a = a
        let and_same = aig.add_and(AigLit::new(a), AigLit::new(a));
        assert_eq!(and_same, AigLit::new(a));

        // a & !a = 0
        let and_comp = aig.add_and(AigLit::new(a), AigLit::not(a));
        assert_eq!(and_comp, AigLit::false_lit());
    }

    #[test]
    fn test_aig_or() {
        let mut aig = Aig::new("test".to_string());
        let a = aig.add_input("a".to_string(), None);
        let b = aig.add_input("b".to_string(), None);

        let or_lit = aig.add_or(AigLit::new(a), AigLit::new(b));

        // OR is implemented as inverted NAND
        assert!(or_lit.inverted);
        assert_eq!(aig.and_count(), 1);
    }

    #[test]
    fn test_aig_xor() {
        let mut aig = Aig::new("test".to_string());
        let a = aig.add_input("a".to_string(), None);
        let b = aig.add_input("b".to_string(), None);

        let xor_lit = aig.add_xor(AigLit::new(a), AigLit::new(b));

        // XOR uses 3 AND nodes
        assert_eq!(aig.and_count(), 3);
        assert!(!xor_lit.is_const());
    }

    #[test]
    fn test_aig_mux() {
        let mut aig = Aig::new("test".to_string());
        let s = aig.add_input("s".to_string(), None);
        let a = aig.add_input("a".to_string(), None);
        let b = aig.add_input("b".to_string(), None);

        let mux_lit = aig.add_mux(AigLit::new(s), AigLit::new(a), AigLit::new(b));

        // MUX uses 3 AND nodes
        assert_eq!(aig.and_count(), 3);
        assert!(!mux_lit.is_const());
    }

    #[test]
    fn test_aig_latch() {
        let mut aig = Aig::new("test".to_string());
        let clk = aig.add_input("clk".to_string(), None);
        let d = aig.add_input("d".to_string(), None);

        let q = aig.add_latch(AigLit::new(d), Some(false), Some(clk), None);

        assert_eq!(aig.latch_count(), 1);
        assert!(aig.get_node(q).unwrap().is_latch());
    }

    #[test]
    fn test_aig_output() {
        let mut aig = Aig::new("test".to_string());
        let a = aig.add_input("a".to_string(), None);
        let b = aig.add_input("b".to_string(), None);
        let and_lit = aig.add_and(AigLit::new(a), AigLit::new(b));

        aig.add_output("y".to_string(), and_lit);

        assert_eq!(aig.output_count(), 1);
        assert_eq!(aig.outputs()[0].0, "y");
    }

    #[test]
    fn test_aig_stats() {
        let mut aig = Aig::new("test".to_string());
        let a = aig.add_input("a".to_string(), None);
        let b = aig.add_input("b".to_string(), None);
        let c = aig.add_input("c".to_string(), None);

        let ab = aig.add_and(AigLit::new(a), AigLit::new(b));
        let abc = aig.add_and(ab, AigLit::new(c));
        aig.add_output("y".to_string(), abc);

        let stats = aig.compute_stats();
        assert_eq!(stats.input_count, 3);
        assert_eq!(stats.and_count, 2);
        assert_eq!(stats.output_count, 1);
        assert_eq!(stats.max_level, 2);
    }

    #[test]
    fn test_safety_info_merge() {
        let a = AigSafetyInfo {
            fit: 0.1,
            classification: Some(CellSafetyClassification::Functional),
            source_cells: vec![CellId(0)],
        };
        let b = AigSafetyInfo {
            fit: 0.2,
            classification: Some(CellSafetyClassification::SafetyMechanism {
                goal_name: "Test".to_string(),
                mechanism_name: "SM1".to_string(),
            }),
            source_cells: vec![CellId(1)],
        };

        let merged = a.merge(&b);

        // Max FIT
        assert!((merged.fit - 0.2).abs() < 0.001);
        // SM takes precedence
        assert!(merged
            .classification
            .as_ref()
            .unwrap()
            .is_safety_mechanism());
        // All sources included
        assert_eq!(merged.source_cells.len(), 2);
    }
}
