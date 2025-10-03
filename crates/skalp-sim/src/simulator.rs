use async_trait::async_trait;
use skalp_sir::SirModule;
use std::collections::HashMap;
use std::sync::Arc;
use thiserror::Error;
use tokio::sync::{mpsc, RwLock};

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
    pub signals: HashMap<String, Vec<u8>>,
    pub registers: HashMap<String, Vec<u8>>,
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
}

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
        let runtime: Box<dyn SimulationRuntime> = if config.use_gpu {
            #[cfg(target_os = "macos")]
            {
                Box::new(crate::gpu_runtime::GpuRuntime::new().await?)
            }
            #[cfg(not(target_os = "macos"))]
            {
                return Err(SimulationError::GpuError(
                    "GPU simulation only available on macOS".into(),
                ));
            }
        } else {
            Box::new(crate::cpu_runtime::CpuRuntime::new())
        };

        let (control_tx, control_rx) = mpsc::channel(100);

        Ok(Simulator {
            config,
            runtime,
            state_history: Arc::new(RwLock::new(Vec::new())),
            control_tx,
            control_rx,
        })
    }

    pub async fn load_module(&mut self, module: &SirModule) -> SimulationResult<()> {
        self.runtime.initialize(module).await
    }

    pub async fn run_simulation(&mut self) -> SimulationResult<()> {
        let max_cycles = self.config.max_cycles;
        let capture_waveforms = self.config.capture_waveforms;

        let mut runtime = Box::new(crate::cpu_runtime::CpuRuntime::new());
        let state_history = self.state_history.clone();
        let control_rx = self.control_rx.try_recv();

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

    pub async fn resume(&self) -> SimulationResult<()> {
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
}
