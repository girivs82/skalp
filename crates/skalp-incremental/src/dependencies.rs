//! Dependency tracking for incremental builds

use crate::IncrementalResult;

pub struct DependencyTracker {
    dependencies: std::collections::HashMap<String, Vec<String>>,
}

impl DependencyTracker {
    pub fn new() -> Self {
        Self {
            dependencies: std::collections::HashMap::new(),
        }
    }

    pub fn get_dependency_graph(&self, target: &str) -> IncrementalResult<Vec<String>> {
        Ok(self.dependencies.get(target).cloned().unwrap_or_default())
    }

    pub fn get_dependents(&self, _target: &str) -> IncrementalResult<Vec<String>> {
        Ok(Vec::new())
    }

    pub fn topological_sort(&self, targets: &[String]) -> IncrementalResult<Vec<String>> {
        Ok(targets.to_vec())
    }
}