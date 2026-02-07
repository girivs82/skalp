//! Simulation Runtime Types
//!
//! This module defines the core types and traits for simulation runtimes.
//! The actual implementation is in `CompiledCpuRuntime` (CPU) and `GpuRuntime` (GPU).

use async_trait::async_trait;
use indexmap::IndexMap;
use skalp_sir::SirModule;
use thiserror::Error;

/// Errors that can occur during simulation
#[derive(Debug, Error)]
pub enum SimulationError {
    #[error("GPU runtime error: {0}")]
    GpuError(String),
    #[error("CPU runtime error: {0}")]
    CpuError(String),
    #[error("Invalid input: {0}")]
    InvalidInput(String),
    #[error("Simulation timeout")]
    Timeout,
    #[error("Breakpoint hit: {0}")]
    BreakpointHit(String),
}

/// Result type for simulation operations
pub type SimulationResult<T> = Result<T, SimulationError>;

/// Configuration for simulation
#[derive(Debug, Clone)]
pub struct SimulationConfig {
    pub use_gpu: bool,
    pub max_cycles: u64,
    pub timeout_ms: u64,
    pub capture_waveforms: bool,
    pub parallel_threads: usize,
}

impl Default for SimulationConfig {
    fn default() -> Self {
        SimulationConfig {
            use_gpu: true,
            max_cycles: 1_000_000,
            timeout_ms: 60_000,
            capture_waveforms: true,
            parallel_threads: 4,
        }
    }
}

/// Snapshot of simulation state at a given cycle
#[derive(Debug, Clone, PartialEq)]
pub struct SimulationState {
    pub cycle: u64,
    pub signals: IndexMap<String, Vec<u8>>,
    pub registers: IndexMap<String, Vec<u8>>,
}

/// Trait for simulation runtime implementations
///
/// Implemented by `CompiledCpuRuntime` and `GpuRuntime`.
#[async_trait]
pub trait SimulationRuntime: Send + Sync {
    /// Initialize the runtime with a SIR module
    async fn initialize(&mut self, module: &SirModule) -> SimulationResult<()>;

    /// Execute one simulation cycle
    async fn step(&mut self) -> SimulationResult<SimulationState>;

    /// Execute multiple simulation cycles
    async fn run(&mut self, cycles: u64) -> SimulationResult<Vec<SimulationState>>;

    /// Reset the simulation state
    async fn reset(&mut self) -> SimulationResult<()>;

    /// Set an input value
    async fn set_input(&mut self, name: &str, value: &[u8]) -> SimulationResult<()>;

    /// Get an output value
    async fn get_output(&self, name: &str) -> SimulationResult<Vec<u8>>;
}
