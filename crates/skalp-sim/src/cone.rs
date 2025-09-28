//! Combinational cone extraction for GPU parallelization
//!
//! Identifies groups of combinational logic that can be executed in parallel
//! on GPU compute shaders. This analysis is critical for GPU simulation performance.

use crate::sir::{SirModule, CombinationalBlock, SirSignalId, CombBlockId};
use std::collections::{HashMap, HashSet};
use petgraph::Graph;

/// A combinational cone - a group of logic blocks that can execute together
#[derive(Debug, Clone)]
pub struct CombinationalCone {
    /// Unique cone ID
    pub id: ConeId,
    /// Blocks in this cone (execution order within cone)
    pub blocks: Vec<CombBlockId>,
    /// Input signals (from other cones or sequential logic)
    pub inputs: Vec<SirSignalId>,
    /// Output signals (to other cones or sequential logic)
    pub outputs: Vec<SirSignalId>,
    /// Estimated GPU workgroup size for this cone
    pub workgroup_size: u32,
    /// Maximum depth of logic in this cone
    pub logic_depth: u32,
}

/// Cone identifier
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ConeId(pub u32);

/// Result of cone extraction analysis
#[derive(Debug)]
pub struct ConeExtractionResult {
    /// All extracted cones
    pub cones: Vec<CombinationalCone>,
    /// Execution order of cones (topological order)
    pub execution_order: Vec<ConeId>,
    /// Estimated parallelism factor
    pub parallelism_factor: f32,
}

/// Combinational cone extractor
pub struct ConeExtractor {
    /// Next cone ID
    next_cone_id: u32,
}

impl ConeExtractor {
    /// Create a new cone extractor
    pub fn new() -> Self {
        Self {
            next_cone_id: 0,
        }
    }

    /// Extract combinational cones from SIR module
    pub fn extract_cones(&mut self, module: &SirModule) -> ConeExtractionResult {
        // Build dependency graph for combinational blocks
        let dep_graph = self.build_dependency_graph(module);

        // Find strongly connected components (cycles) - these must be in same cone
        let sccs = self.find_strongly_connected_components(&dep_graph);

        // Extract cones based on SCCs and optimization heuristics
        let mut cones = self.extract_cones_from_sccs(module, &dep_graph, &sccs);

        // Optimize cone boundaries for GPU execution
        self.optimize_cone_boundaries(module, &mut cones);

        // Determine execution order
        let execution_order = self.determine_execution_order(&cones);

        // Calculate parallelism metrics
        let parallelism_factor = self.calculate_parallelism_factor(&cones);

        ConeExtractionResult {
            cones,
            execution_order,
            parallelism_factor,
        }
    }

    /// Build dependency graph between combinational blocks
    fn build_dependency_graph(&self, module: &SirModule) -> Graph<CombBlockId, ()> {
        let mut graph = Graph::new();
        let mut block_nodes = HashMap::new();

        // Add all combinational blocks as nodes
        for block in &module.comb_blocks {
            let node = graph.add_node(block.id);
            block_nodes.insert(block.id, node);
        }

        // Add edges based on signal dependencies
        for block in &module.comb_blocks {
            for input_signal in &block.inputs {
                // Find which block drives this input
                for other_block in &module.comb_blocks {
                    if other_block.id != block.id && other_block.outputs.contains(input_signal) {
                        // Add dependency edge: other_block -> block
                        if let (Some(&source_node), Some(&target_node)) =
                            (block_nodes.get(&other_block.id), block_nodes.get(&block.id)) {
                            graph.add_edge(source_node, target_node, ());
                        }
                    }
                }
            }
        }

        graph
    }

    /// Find strongly connected components in the dependency graph
    fn find_strongly_connected_components(&self, graph: &Graph<CombBlockId, ()>) -> Vec<Vec<CombBlockId>> {
        use petgraph::algo::kosaraju_scc;

        let sccs = kosaraju_scc(graph);
        sccs.into_iter()
            .map(|scc| scc.into_iter().map(|node| graph[node]).collect())
            .collect()
    }

    /// Extract cones from strongly connected components
    fn extract_cones_from_sccs(
        &mut self,
        module: &SirModule,
        graph: &Graph<CombBlockId, ()>,
        sccs: &[Vec<CombBlockId>],
    ) -> Vec<CombinationalCone> {
        let mut cones = Vec::new();
        let block_map: HashMap<CombBlockId, &CombinationalBlock> =
            module.comb_blocks.iter().map(|b| (b.id, b)).collect();

        for scc in sccs {
            if scc.is_empty() {
                continue;
            }

            // For now, each SCC becomes one cone
            // Future optimization: split large SCCs if beneficial
            let cone_id = ConeId(self.next_cone_id);
            self.next_cone_id += 1;

            let mut cone_inputs = HashSet::new();
            let mut cone_outputs = HashSet::new();
            let mut total_workgroup_hint = 0;

            // Collect inputs and outputs for this cone
            for &block_id in scc {
                if let Some(block) = block_map.get(&block_id) {
                    // Add external inputs (not driven by blocks in this cone)
                    for &input in &block.inputs {
                        let is_external = !scc.iter().any(|&other_block_id| {
                            if let Some(other_block) = block_map.get(&other_block_id) {
                                other_block.outputs.contains(&input)
                            } else {
                                false
                            }
                        });
                        if is_external {
                            cone_inputs.insert(input);
                        }
                    }

                    // Add outputs that go outside this cone
                    for &output in &block.outputs {
                        cone_outputs.insert(output);
                    }

                    // Accumulate workgroup hints
                    if let Some(hint) = block.workgroup_size_hint {
                        total_workgroup_hint += hint;
                    }
                }
            }

            // Estimate optimal workgroup size
            let workgroup_size = if total_workgroup_hint > 0 {
                (total_workgroup_hint / scc.len() as u32).max(32).min(1024)
            } else {
                64 // Default workgroup size
            };

            // Calculate logic depth (longest path through cone)
            let logic_depth = self.calculate_cone_depth(scc, &block_map, graph);

            let cone = CombinationalCone {
                id: cone_id,
                blocks: scc.clone(),
                inputs: cone_inputs.into_iter().collect(),
                outputs: cone_outputs.into_iter().collect(),
                workgroup_size,
                logic_depth,
            };

            cones.push(cone);
        }

        cones
    }

    /// Calculate the logic depth of a cone
    fn calculate_cone_depth(
        &self,
        scc: &[CombBlockId],
        block_map: &HashMap<CombBlockId, &CombinationalBlock>,
        graph: &Graph<CombBlockId, ()>,
    ) -> u32 {
        // For now, use a simple heuristic based on number of blocks
        // Future: implement proper critical path analysis
        scc.len() as u32
    }

    /// Optimize cone boundaries for better GPU utilization
    fn optimize_cone_boundaries(&self, module: &SirModule, cones: &mut Vec<CombinationalCone>) {
        // Optimization strategies:
        // 1. Merge small cones to reduce GPU kernel launch overhead
        // 2. Split large cones if they exceed optimal workgroup size
        // 3. Balance cones for better load distribution

        let min_cone_size = 16; // Minimum blocks per cone
        let max_cone_size = 256; // Maximum blocks per cone

        // Simple optimization: merge very small cones
        let mut i = 0;
        while i < cones.len() {
            if cones[i].blocks.len() < min_cone_size && i + 1 < cones.len() {
                // Try to merge with next cone if compatible
                if self.can_merge_cones(&cones[i], &cones[i + 1]) {
                    let next_cone = cones.remove(i + 1);
                    self.merge_cones(&mut cones[i], next_cone);
                }
            }
            i += 1;
        }

        // TODO: Implement cone splitting for oversized cones
        // TODO: Implement load balancing optimization
    }

    /// Check if two cones can be merged
    fn can_merge_cones(&self, cone1: &CombinationalCone, cone2: &CombinationalCone) -> bool {
        // Check if cone2's inputs overlap with cone1's outputs
        // This indicates cone2 depends on cone1 and they can potentially be merged
        cone2.inputs.iter().any(|input| cone1.outputs.contains(input))
    }

    /// Merge two cones
    fn merge_cones(&self, target: &mut CombinationalCone, source: CombinationalCone) {
        target.blocks.extend(source.blocks);

        // Merge inputs (remove internal connections)
        for input in source.inputs {
            if !target.outputs.contains(&input) {
                target.inputs.push(input);
            }
        }
        target.outputs.extend(source.outputs);

        // Recalculate metrics
        target.workgroup_size = ((target.workgroup_size + source.workgroup_size) / 2).max(32).min(1024);
        target.logic_depth = target.logic_depth.max(source.logic_depth);
    }

    /// Determine execution order of cones
    fn determine_execution_order(&self, cones: &[CombinationalCone]) -> Vec<ConeId> {
        // Build cone dependency graph
        let mut cone_graph = Graph::new();
        let mut cone_nodes = HashMap::new();

        // Add cone nodes
        for cone in cones {
            let node = cone_graph.add_node(cone.id);
            cone_nodes.insert(cone.id, node);
        }

        // Add dependency edges between cones
        for i in 0..cones.len() {
            for j in 0..cones.len() {
                if i != j {
                    // Check if cone j depends on cone i
                    let depends = cones[j].inputs.iter().any(|input| cones[i].outputs.contains(input));
                    if depends {
                        if let (Some(&source), Some(&target)) =
                            (cone_nodes.get(&cones[i].id), cone_nodes.get(&cones[j].id)) {
                            cone_graph.add_edge(source, target, ());
                        }
                    }
                }
            }
        }

        // Topological sort to get execution order
        match petgraph::algo::toposort(&cone_graph, None) {
            Ok(sorted_nodes) => {
                sorted_nodes.into_iter()
                    .map(|node| cone_graph[node])
                    .collect()
            }
            Err(_) => {
                // Fallback: original order if cycle detected
                cones.iter().map(|cone| cone.id).collect()
            }
        }
    }

    /// Calculate parallelism factor
    fn calculate_parallelism_factor(&self, cones: &[CombinationalCone]) -> f32 {
        if cones.is_empty() {
            return 0.0;
        }

        let total_blocks: u32 = cones.iter().map(|cone| cone.blocks.len() as u32).sum();
        let max_depth: u32 = cones.iter().map(|cone| cone.logic_depth).max().unwrap_or(1);

        // Parallelism factor = average blocks that can execute in parallel
        total_blocks as f32 / max_depth as f32
    }
}

impl Default for ConeExtractor {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::sir::*;

    #[test]
    fn test_cone_extraction_simple() {
        // Create a simple module with two independent combinational blocks
        let mut module = SirModule::new("test".to_string());

        // Add some signals
        module.signals = vec![
            SirSignal {
                id: SirSignalId(0),
                name: "a".to_string(),
                width: 4,
                signal_type: SirSignalType::Wire,
                initial_value: None,
            },
            SirSignal {
                id: SirSignalId(1),
                name: "b".to_string(),
                width: 4,
                signal_type: SirSignalType::Wire,
                initial_value: None,
            },
            SirSignal {
                id: SirSignalId(2),
                name: "sum".to_string(),
                width: 5,
                signal_type: SirSignalType::Wire,
                initial_value: None,
            },
        ];

        // Add a combinational block
        module.comb_blocks = vec![
            CombinationalBlock {
                id: CombBlockId(0),
                inputs: vec![SirSignalId(0), SirSignalId(1)],
                outputs: vec![SirSignalId(2)],
                operations: Vec::new(),
                workgroup_size_hint: Some(64),
            }
        ];

        let mut extractor = ConeExtractor::new();
        let result = extractor.extract_cones(&module);

        assert_eq!(result.cones.len(), 1);
        assert_eq!(result.execution_order.len(), 1);
        assert!(result.parallelism_factor > 0.0);

        println!("Extracted {} cones with parallelism factor {:.2}",
                result.cones.len(), result.parallelism_factor);
    }
}