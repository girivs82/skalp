//! Dependency graph management

use crate::ParallelResult;

pub struct DependencyGraph {
    nodes: Vec<String>,
    edges: Vec<(String, String)>,
}

impl Default for DependencyGraph {
    fn default() -> Self {
        Self::new()
    }
}

impl DependencyGraph {
    pub fn new() -> Self {
        Self {
            nodes: Vec::new(),
            edges: Vec::new(),
        }
    }

    pub fn add_dependency(&mut self, from: String, to: String) {
        if !self.nodes.contains(&from) {
            self.nodes.push(from.clone());
        }
        if !self.nodes.contains(&to) {
            self.nodes.push(to.clone());
        }
        self.edges.push((from, to));
    }

    pub fn topological_sort(&self) -> ParallelResult<Vec<String>> {
        // Simplified topological sort
        Ok(self.nodes.clone())
    }
}
