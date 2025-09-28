//! Task definitions and execution

use crate::{TaskContext, ParallelResult};

pub trait Task {
    fn execute(&self, context: &TaskContext) -> ParallelResult<()>;
}

pub struct CompileTask {
    pub source_path: String,
}

impl Task for CompileTask {
    fn execute(&self, _context: &TaskContext) -> ParallelResult<()> {
        // Compile implementation
        Ok(())
    }
}

pub struct SynthesisTask {
    pub design_path: String,
}

impl Task for SynthesisTask {
    fn execute(&self, _context: &TaskContext) -> ParallelResult<()> {
        // Synthesis implementation
        Ok(())
    }
}