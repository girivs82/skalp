//! Worker thread pool management

use crate::{ParallelResult, scheduler::TaskScheduler};

pub struct WorkerPool {
    worker_count: usize,
}

impl WorkerPool {
    pub fn new(worker_count: usize) -> Self {
        Self { worker_count }
    }

    pub async fn start(&self, _scheduler: &TaskScheduler) -> ParallelResult<()> {
        Ok(())
    }

    pub async fn stop(&self) -> ParallelResult<()> {
        Ok(())
    }

    pub fn active_count(&self) -> usize {
        self.worker_count
    }

    pub fn total_memory_usage(&self) -> usize {
        1024 // MB
    }
}