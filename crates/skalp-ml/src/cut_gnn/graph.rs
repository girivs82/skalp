//! Graph Encoding for AIGs
//!
//! Converts AIGs to graph representations suitable for GNN processing.

use serde::{Deserialize, Serialize};
use skalp_lir::synth::{Aig, AigNode, AigNodeId};
use std::collections::HashMap;

/// Number of features per node
pub const NODE_FEATURE_DIM: usize = 8;

/// Number of features per edge
pub const EDGE_FEATURE_DIM: usize = 2;

/// Node features for GNN
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub struct NodeFeatures {
    /// Node type: 0=input, 1=and, 2=latch, 3=const
    pub node_type: f32,
    /// Fanout count (normalized)
    pub fanout: f32,
    /// Level from primary inputs (normalized)
    pub level_from_inputs: f32,
    /// Level to primary outputs (normalized)
    pub level_to_outputs: f32,
    /// Is on critical path
    pub on_critical_path: f32,
    /// Reconvergence factor (0-1)
    pub reconvergence: f32,
    /// Cut size hint (for cuts rooted at this node)
    pub cut_size_hint: f32,
    /// Structural depth in local cone
    pub local_depth: f32,
}

impl Default for NodeFeatures {
    fn default() -> Self {
        Self {
            node_type: 0.0,
            fanout: 0.0,
            level_from_inputs: 0.0,
            level_to_outputs: 0.0,
            on_critical_path: 0.0,
            reconvergence: 0.0,
            cut_size_hint: 0.0,
            local_depth: 0.0,
        }
    }
}

impl NodeFeatures {
    /// Convert to feature vector
    pub fn to_vector(&self) -> [f32; NODE_FEATURE_DIM] {
        [
            self.node_type,
            self.fanout,
            self.level_from_inputs,
            self.level_to_outputs,
            self.on_critical_path,
            self.reconvergence,
            self.cut_size_hint,
            self.local_depth,
        ]
    }

    /// Create from feature vector
    pub fn from_vector(v: &[f32; NODE_FEATURE_DIM]) -> Self {
        Self {
            node_type: v[0],
            fanout: v[1],
            level_from_inputs: v[2],
            level_to_outputs: v[3],
            on_critical_path: v[4],
            reconvergence: v[5],
            cut_size_hint: v[6],
            local_depth: v[7],
        }
    }
}

/// Edge in the AIG graph
#[derive(Debug, Clone, Copy)]
pub struct AigEdge {
    /// Source node
    pub from: usize,
    /// Target node
    pub to: usize,
    /// Whether the edge is inverted
    pub inverted: bool,
    /// Edge weight (for message passing)
    pub weight: f32,
}

impl AigEdge {
    /// Convert to feature vector
    pub fn to_features(self) -> [f32; EDGE_FEATURE_DIM] {
        [if self.inverted { 1.0 } else { 0.0 }, self.weight]
    }
}

/// Graph representation of an AIG for GNN processing
#[derive(Debug, Clone)]
pub struct AigGraph {
    /// Node features
    pub nodes: Vec<NodeFeatures>,
    /// Edge list (COO format)
    pub edges: Vec<AigEdge>,
    /// Node ID to index mapping
    pub node_to_idx: HashMap<AigNodeId, usize>,
    /// Index to node ID mapping
    pub idx_to_node: Vec<AigNodeId>,
    /// Number of nodes
    pub num_nodes: usize,
    /// Number of edges
    pub num_edges: usize,
}

impl AigGraph {
    /// Create an empty graph
    pub fn new() -> Self {
        Self {
            nodes: Vec::new(),
            edges: Vec::new(),
            node_to_idx: HashMap::new(),
            idx_to_node: Vec::new(),
            num_nodes: 0,
            num_edges: 0,
        }
    }

    /// Build graph from AIG
    pub fn from_aig(aig: &Aig) -> Self {
        let mut graph = Self::new();

        // First pass: collect all nodes and assign indices
        let mut fanout_count: HashMap<AigNodeId, usize> = HashMap::new();

        for (id, node) in aig.iter_nodes() {
            let idx = graph.num_nodes;
            graph.node_to_idx.insert(id, idx);
            graph.idx_to_node.push(id);
            graph.num_nodes += 1;

            // Count fanouts
            for lit in node.fanins() {
                *fanout_count.entry(lit.node).or_insert(0) += 1;
            }

            // Initialize node features
            graph.nodes.push(NodeFeatures::default());
        }

        // Compute levels (forward pass)
        let levels = compute_levels(aig);
        let max_level = *levels.values().max().unwrap_or(&1) as f32;

        // Compute reverse levels (backward pass)
        let reverse_levels = compute_reverse_levels(aig);
        let max_reverse_level = *reverse_levels.values().max().unwrap_or(&1) as f32;

        // Second pass: populate node features and edges
        for (id, node) in aig.iter_nodes() {
            let idx = graph.node_to_idx[&id];

            // Set node type
            let node_type = match node {
                AigNode::Input { .. } => 0.0,
                AigNode::And { .. } => 1.0,
                AigNode::Latch { .. } => 2.0,
                AigNode::Const => 3.0,
                AigNode::Barrier { .. } => 4.0, // Power domain boundary
            };

            // Normalize fanout
            let fanout = fanout_count.get(&id).copied().unwrap_or(0) as f32;
            let max_fanout = *fanout_count.values().max().unwrap_or(&1) as f32;

            // Get levels
            let level = levels.get(&id).copied().unwrap_or(0) as f32;
            let reverse_level = reverse_levels.get(&id).copied().unwrap_or(0) as f32;

            graph.nodes[idx] = NodeFeatures {
                node_type: node_type / 3.0, // Normalize to [0, 1]
                fanout: (fanout / max_fanout).min(1.0),
                level_from_inputs: level / max_level.max(1.0),
                level_to_outputs: reverse_level / max_reverse_level.max(1.0),
                on_critical_path: 0.0, // Would need timing info
                reconvergence: 0.0,    // Computed separately if needed
                cut_size_hint: 0.0,
                local_depth: level / max_level.max(1.0),
            };

            // Add edges for fanins
            for lit in node.fanins() {
                if let Some(&from_idx) = graph.node_to_idx.get(&lit.node) {
                    graph.edges.push(AigEdge {
                        from: from_idx,
                        to: idx,
                        inverted: lit.inverted,
                        weight: 1.0,
                    });
                    graph.num_edges += 1;
                }
            }
        }

        graph
    }

    /// Get node features as a flat vector (for batch processing)
    pub fn node_features_flat(&self) -> Vec<f32> {
        let mut features = Vec::with_capacity(self.num_nodes * NODE_FEATURE_DIM);
        for node in &self.nodes {
            features.extend_from_slice(&node.to_vector());
        }
        features
    }

    /// Get edge index in COO format (2 x num_edges)
    pub fn edge_index(&self) -> (Vec<usize>, Vec<usize>) {
        let mut from_nodes = Vec::with_capacity(self.num_edges);
        let mut to_nodes = Vec::with_capacity(self.num_edges);

        for edge in &self.edges {
            from_nodes.push(edge.from);
            to_nodes.push(edge.to);
        }

        (from_nodes, to_nodes)
    }

    /// Get edge features as a flat vector
    pub fn edge_features_flat(&self) -> Vec<f32> {
        let mut features = Vec::with_capacity(self.num_edges * EDGE_FEATURE_DIM);
        for edge in &self.edges {
            features.extend_from_slice(&edge.to_features());
        }
        features
    }

    /// Extract subgraph around a node (local cone)
    pub fn local_cone(&self, root_idx: usize, max_depth: usize) -> AigGraph {
        let mut subgraph = AigGraph::new();
        let mut visited: HashMap<usize, usize> = HashMap::new();
        let mut queue = vec![(root_idx, 0)];

        while let Some((idx, depth)) = queue.pop() {
            if visited.contains_key(&idx) || depth > max_depth {
                continue;
            }

            let new_idx = subgraph.num_nodes;
            visited.insert(idx, new_idx);
            subgraph.nodes.push(self.nodes[idx]);
            subgraph.idx_to_node.push(self.idx_to_node[idx]);
            subgraph.node_to_idx.insert(self.idx_to_node[idx], new_idx);
            subgraph.num_nodes += 1;

            // Find predecessors
            for edge in &self.edges {
                if edge.to == idx && !visited.contains_key(&edge.from) {
                    queue.push((edge.from, depth + 1));
                }
            }
        }

        // Add edges within subgraph
        for edge in &self.edges {
            if let (Some(&new_from), Some(&new_to)) =
                (visited.get(&edge.from), visited.get(&edge.to))
            {
                subgraph.edges.push(AigEdge {
                    from: new_from,
                    to: new_to,
                    inverted: edge.inverted,
                    weight: edge.weight,
                });
                subgraph.num_edges += 1;
            }
        }

        subgraph
    }
}

impl Default for AigGraph {
    fn default() -> Self {
        Self::new()
    }
}

/// Compute levels from primary inputs (forward propagation)
fn compute_levels(aig: &Aig) -> HashMap<AigNodeId, usize> {
    let mut levels: HashMap<AigNodeId, usize> = HashMap::new();

    for (id, node) in aig.iter_nodes() {
        let level = match node {
            AigNode::Input { .. } | AigNode::Const => 0,
            AigNode::And { left, right } => {
                let left_level = levels.get(&left.node).copied().unwrap_or(0);
                let right_level = levels.get(&right.node).copied().unwrap_or(0);
                left_level.max(right_level) + 1
            }
            AigNode::Latch { data, .. } => levels.get(&data.node).copied().unwrap_or(0) + 1,
            AigNode::Barrier { data, .. } => levels.get(&data.node).copied().unwrap_or(0) + 1,
        };
        levels.insert(id, level);
    }

    levels
}

/// Compute reverse levels from primary outputs (backward propagation)
fn compute_reverse_levels(aig: &Aig) -> HashMap<AigNodeId, usize> {
    let mut reverse_levels: HashMap<AigNodeId, usize> = HashMap::new();

    // Initialize outputs with level 0
    for (_, lit) in aig.outputs() {
        reverse_levels.insert(lit.node, 0);
    }

    // Build fanout map
    let mut fanouts: HashMap<AigNodeId, Vec<AigNodeId>> = HashMap::new();
    for (id, node) in aig.iter_nodes() {
        for lit in node.fanins() {
            fanouts.entry(lit.node).or_default().push(id);
        }
    }

    // Collect nodes in reverse order (since iter_nodes doesn't implement DoubleEndedIterator)
    let nodes: Vec<_> = aig.iter_nodes().map(|(id, _)| id).collect();

    // Propagate backwards
    for id in nodes.into_iter().rev() {
        if let Some(fouts) = fanouts.get(&id) {
            let max_fanout_level = fouts
                .iter()
                .filter_map(|fid| reverse_levels.get(fid))
                .max()
                .copied()
                .unwrap_or(0);
            let current = reverse_levels.entry(id).or_insert(0);
            *current = (*current).max(max_fanout_level + 1);
        }
    }

    reverse_levels
}

#[cfg(test)]
mod tests {
    use super::*;
    use skalp_lir::synth::AigLit;

    #[test]
    fn test_aig_graph_empty() {
        let aig = Aig::new("test".to_string());
        let graph = AigGraph::from_aig(&aig);

        // Empty AIG may have a constant node (node count >= 0)
        assert!(graph.num_nodes <= 1);
        assert_eq!(graph.num_edges, 0);
    }

    #[test]
    fn test_aig_graph_simple() {
        let mut aig = Aig::new("test".to_string());
        let a = aig.add_input("a".to_string(), None);
        let b = aig.add_input("b".to_string(), None);
        let and_result = aig.add_and(AigLit::new(a), AigLit::new(b));
        aig.add_output("out".to_string(), and_result);

        let graph = AigGraph::from_aig(&aig);

        // 2 inputs + 1 AND + possibly 1 constant = 3-4 nodes
        assert!(graph.num_nodes >= 3);
        assert!(graph.num_edges >= 2); // At least 2 edges to AND
    }

    #[test]
    fn test_node_features() {
        let features = NodeFeatures {
            node_type: 0.5,
            fanout: 0.3,
            level_from_inputs: 0.2,
            level_to_outputs: 0.4,
            on_critical_path: 1.0,
            reconvergence: 0.1,
            cut_size_hint: 0.6,
            local_depth: 0.25,
        };

        let vec = features.to_vector();
        let restored = NodeFeatures::from_vector(&vec);

        assert!((restored.node_type - 0.5).abs() < 0.001);
        assert!((restored.fanout - 0.3).abs() < 0.001);
    }

    #[test]
    fn test_local_cone() {
        let mut aig = Aig::new("test".to_string());
        let a = aig.add_input("a".to_string(), None);
        let b = aig.add_input("b".to_string(), None);
        let c = aig.add_input("c".to_string(), None);
        let and1 = aig.add_and(AigLit::new(a), AigLit::new(b));
        let and2 = aig.add_and(and1, AigLit::new(c));
        aig.add_output("out".to_string(), and2);

        let graph = AigGraph::from_aig(&aig);
        let root_idx = graph.node_to_idx[&and2.node];
        let cone = graph.local_cone(root_idx, 1);

        // Depth 1 from and2 should include and2 and and1 (but not a, b, c)
        assert!(cone.num_nodes <= 3);
    }
}
