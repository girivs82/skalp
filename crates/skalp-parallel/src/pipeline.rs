//! Compilation pipeline management

use crate::{ParallelResult, scheduler::TaskGraph};

#[derive(Debug, Default)]
pub struct CompilationResults {
    pub success: bool,
    pub duration: std::time::Duration,
    pub artifacts: Vec<String>,
}

pub struct CompilationPipeline {
    design_path: String,
}

impl CompilationPipeline {
    pub fn new(design_path: String) -> Self {
        Self { design_path }
    }

    pub fn build_task_graph(&self) -> ParallelResult<TaskGraph> {
        Ok(TaskGraph {
            tasks: Vec::new(),
        })
    }
}