//! Task scheduler for parallel compilation

use crate::{ParallelResult, TaskContext};
use tokio::sync::mpsc;

pub struct TaskScheduler {
    max_queue_size: usize,
    completed_count: std::sync::atomic::AtomicU64,
    failed_count: std::sync::atomic::AtomicU64,
}

impl TaskScheduler {
    pub fn new(max_queue_size: usize) -> Self {
        Self {
            max_queue_size,
            completed_count: std::sync::atomic::AtomicU64::new(0),
            failed_count: std::sync::atomic::AtomicU64::new(0),
        }
    }

    pub async fn start(&self) -> ParallelResult<()> {
        Ok(())
    }

    pub async fn stop(&self) -> ParallelResult<()> {
        Ok(())
    }

    pub fn queue_size(&self) -> usize {
        0
    }

    pub fn completed_count(&self) -> u64 {
        self.completed_count
            .load(std::sync::atomic::Ordering::Relaxed)
    }

    pub fn failed_count(&self) -> u64 {
        self.failed_count.load(std::sync::atomic::Ordering::Relaxed)
    }

    pub async fn submit_graph(&self, _graph: TaskGraph) -> ParallelResult<CompletionHandle> {
        Ok(CompletionHandle {})
    }
}

pub struct TaskGraph {
    pub tasks: Vec<TaskContext>,
}

pub struct CompletionHandle {}

impl CompletionHandle {
    pub async fn wait(self) -> ParallelResult<crate::pipeline::CompilationResults> {
        Ok(crate::pipeline::CompilationResults::default())
    }
}
