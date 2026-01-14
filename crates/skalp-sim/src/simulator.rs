use async_trait::async_trait;
use indexmap::IndexMap;
use skalp_sir::SirModule;
use std::sync::Arc;
use thiserror::Error;
use tokio::sync::{mpsc, RwLock};

use crate::breakpoint::{BreakpointAction, BreakpointHit, BreakpointManager};

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

pub type SimulationResult<T> = Result<T, SimulationError>;

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

#[derive(Debug, Clone, PartialEq)]
pub struct SimulationState {
    pub cycle: u64,
    pub signals: IndexMap<String, Vec<u8>>,
    pub registers: IndexMap<String, Vec<u8>>,
}

#[async_trait]
pub trait SimulationRuntime: Send + Sync {
    async fn initialize(&mut self, module: &SirModule) -> SimulationResult<()>;
    async fn step(&mut self) -> SimulationResult<SimulationState>;
    async fn run(&mut self, cycles: u64) -> SimulationResult<Vec<SimulationState>>;
    async fn reset(&mut self) -> SimulationResult<()>;
    async fn set_input(&mut self, name: &str, value: &[u8]) -> SimulationResult<()>;
    async fn get_output(&self, name: &str) -> SimulationResult<Vec<u8>>;
}

pub struct Simulator {
    config: SimulationConfig,
    runtime: Box<dyn SimulationRuntime>,
    state_history: Arc<RwLock<Vec<SimulationState>>>,
    control_tx: mpsc::Sender<SimulatorCommand>,
    control_rx: mpsc::Receiver<SimulatorCommand>,
    /// Breakpoint manager for simulation debugging
    breakpoint_manager: BreakpointManager,
    /// Whether simulation is paused (e.g., at a breakpoint)
    paused: bool,
    /// Last breakpoint hits (for inspection)
    last_breakpoint_hits: Vec<BreakpointHit>,
}

#[allow(dead_code)]
#[derive(Debug)]
enum SimulatorCommand {
    Start,
    Stop,
    Pause,
    Step,
    Reset,
    SetInput { name: String, value: Vec<u8> },
}

impl Simulator {
    pub async fn new(config: SimulationConfig) -> SimulationResult<Self> {
        println!("🔧 Simulator::new called with use_gpu = {}", config.use_gpu);
        println!("🔧 target_os = macos: {}", cfg!(target_os = "macos"));

        let runtime: Box<dyn SimulationRuntime> = if config.use_gpu {
            println!("🔧 Attempting to create GPU runtime...");
            #[cfg(target_os = "macos")]
            {
                println!("🔧 Creating GpuRuntime (macOS)...");
                match crate::gpu_runtime::GpuRuntime::new().await {
                    Ok(rt) => {
                        println!("✅ GpuRuntime created successfully!");
                        Box::new(rt)
                    }
                    Err(e) => {
                        println!("❌ GpuRuntime creation FAILED: {:?}", e);
                        return Err(e);
                    }
                }
            }
            #[cfg(not(target_os = "macos"))]
            {
                println!("❌ Not on macOS, GPU not available");
                return Err(SimulationError::GpuError(
                    "GPU simulation only available on macOS".into(),
                ));
            }
        } else {
            println!("🔧 Creating CpuRuntime (use_gpu = false)...");
            Box::new(crate::cpu_runtime::CpuRuntime::new())
        };

        let (control_tx, control_rx) = mpsc::channel(100);

        Ok(Simulator {
            config,
            runtime,
            state_history: Arc::new(RwLock::new(Vec::new())),
            control_tx,
            control_rx,
            breakpoint_manager: BreakpointManager::new(),
            paused: false,
            last_breakpoint_hits: Vec::new(),
        })
    }

    pub async fn load_module(&mut self, module: &SirModule) -> SimulationResult<()> {
        eprintln!("🔧 Simulator::load_module calling runtime.initialize()...");
        let result = self.runtime.initialize(module).await;
        eprintln!("🔧 Simulator::load_module result: {:?}", result.is_ok());
        result
    }

    pub async fn run_simulation(&mut self) -> SimulationResult<()> {
        let max_cycles = self.config.max_cycles;
        let capture_waveforms = self.config.capture_waveforms;

        let mut runtime = Box::new(crate::cpu_runtime::CpuRuntime::new());
        let state_history = self.state_history.clone();
        let _control_rx = self.control_rx.try_recv();

        // Start simulation loop
        tokio::spawn(async move {
            let mut current_cycle = 0u64;

            while current_cycle < max_cycles {
                // Execute simulation step
                match runtime.step().await {
                    Ok(state) => {
                        if capture_waveforms {
                            let mut history = state_history.write().await;
                            history.push(state);
                        }
                        current_cycle += 1;
                    }
                    Err(e) => {
                        eprintln!("Simulation error at cycle {}: {:?}", current_cycle, e);
                        break;
                    }
                }

                // Yield to other tasks periodically
                if current_cycle % 1000 == 0 {
                    tokio::task::yield_now().await;
                }
            }
        });

        Ok(())
    }

    pub async fn step_simulation(&mut self) -> SimulationResult<SimulationState> {
        // Don't step if paused at a breakpoint
        if self.paused {
            return Err(SimulationError::BreakpointHit(
                "Simulation is paused at a breakpoint. Call resume() to continue.".into(),
            ));
        }

        let state = self.runtime.step().await?;

        if self.config.capture_waveforms {
            let mut history = self.state_history.write().await;
            history.push(state.clone());
        }

        // Check breakpoints after step completes
        let hits = self
            .breakpoint_manager
            .check_cycle(state.cycle, &state.signals);

        if !hits.is_empty() {
            self.last_breakpoint_hits = hits.clone();

            // Check if any hit requires stopping or pausing
            for hit in &hits {
                // Log the breakpoint hit
                if hit.is_error {
                    eprintln!(
                        "🛑 BREAKPOINT ERROR [{}] at cycle {}: {}",
                        hit.name,
                        hit.cycle,
                        hit.message.as_deref().unwrap_or("breakpoint triggered")
                    );
                } else {
                    eprintln!(
                        "🔴 BREAKPOINT [{}] at cycle {}: {}",
                        hit.name,
                        hit.cycle,
                        hit.message.as_deref().unwrap_or("breakpoint triggered")
                    );
                }

                match hit.action {
                    BreakpointAction::Stop => {
                        return Err(SimulationError::BreakpointHit(format!(
                            "Error breakpoint '{}' triggered at cycle {}",
                            hit.name, hit.cycle
                        )));
                    }
                    BreakpointAction::Pause => {
                        self.paused = true;
                    }
                    BreakpointAction::Log => {
                        // Already logged above, continue simulation
                    }
                }
            }
        }

        Ok(state)
    }

    /// Step simulation ignoring breakpoints (for debugging)
    pub async fn step_simulation_no_breakpoints(&mut self) -> SimulationResult<SimulationState> {
        let state = self.runtime.step().await?;

        if self.config.capture_waveforms {
            let mut history = self.state_history.write().await;
            history.push(state.clone());
        }

        Ok(state)
    }

    pub async fn reset(&mut self) -> SimulationResult<()> {
        self.runtime.reset().await?;
        let mut history = self.state_history.write().await;
        history.clear();
        Ok(())
    }

    pub async fn set_input(&mut self, name: &str, value: Vec<u8>) -> SimulationResult<()> {
        self.runtime.set_input(name, &value).await
    }

    pub async fn get_output(&self, name: &str) -> SimulationResult<Vec<u8>> {
        self.runtime.get_output(name).await
    }

    pub async fn get_waveforms(&self) -> Vec<SimulationState> {
        let history = self.state_history.read().await;
        history.clone()
    }

    pub async fn pause(&self) -> SimulationResult<()> {
        self.control_tx
            .send(SimulatorCommand::Pause)
            .await
            .map_err(|_| SimulationError::CpuError("Failed to send pause command".into()))
    }

    pub async fn resume(&mut self) -> SimulationResult<()> {
        self.paused = false;
        self.last_breakpoint_hits.clear();
        self.control_tx
            .send(SimulatorCommand::Start)
            .await
            .map_err(|_| SimulationError::CpuError("Failed to send start command".into()))
    }

    pub async fn stop(&self) -> SimulationResult<()> {
        self.control_tx
            .send(SimulatorCommand::Stop)
            .await
            .map_err(|_| SimulationError::CpuError("Failed to send stop command".into()))
    }

    // ============================================================
    // Breakpoint Management API
    // ============================================================

    /// Register a breakpoint from a BreakpointConfig (from HIR/MIR)
    pub fn register_breakpoint_from_config(
        &mut self,
        signal_name: &str,
        config: &skalp_frontend::hir::BreakpointConfig,
    ) -> u32 {
        self.breakpoint_manager
            .register_from_config(signal_name, config)
    }

    /// Register a simple breakpoint that triggers when signal is non-zero
    pub fn register_breakpoint(&mut self, signal_name: &str) -> u32 {
        self.breakpoint_manager.register_simple(signal_name)
    }

    /// Enable or disable all breakpoints
    pub fn set_breakpoints_enabled(&mut self, enabled: bool) {
        self.breakpoint_manager.set_enabled(enabled);
    }

    /// Enable or disable a specific breakpoint by ID
    pub fn set_breakpoint_enabled(&mut self, id: u32, enabled: bool) -> bool {
        self.breakpoint_manager.set_breakpoint_enabled(id, enabled)
    }

    /// Remove a breakpoint by ID
    pub fn remove_breakpoint(&mut self, id: u32) -> bool {
        self.breakpoint_manager.remove(id)
    }

    /// Clear all breakpoints
    pub fn clear_breakpoints(&mut self) {
        self.breakpoint_manager.clear();
    }

    /// Get number of registered breakpoints
    pub fn breakpoint_count(&self) -> usize {
        self.breakpoint_manager.count()
    }

    /// Check if simulation is paused at a breakpoint
    pub fn is_paused(&self) -> bool {
        self.paused
    }

    /// Get the breakpoint hits from the last step
    pub fn last_breakpoint_hits(&self) -> &[BreakpointHit] {
        &self.last_breakpoint_hits
    }

    /// Get mutable access to the breakpoint manager for advanced configuration
    pub fn breakpoint_manager_mut(&mut self) -> &mut BreakpointManager {
        &mut self.breakpoint_manager
    }
}
