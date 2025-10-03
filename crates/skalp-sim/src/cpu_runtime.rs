use crate::simulator::{SimulationError, SimulationResult, SimulationRuntime, SimulationState};
use async_trait::async_trait;
use skalp_sir::SirModule;
use std::collections::HashMap;

pub struct CpuRuntime {
    module: Option<SirModule>,
    state: HashMap<String, Vec<u8>>,
    next_state: HashMap<String, Vec<u8>>,
    signals: HashMap<String, Vec<u8>>,
    current_cycle: u64,
}

impl Default for CpuRuntime {
    fn default() -> Self {
        Self::new()
    }
}

impl CpuRuntime {
    pub fn new() -> Self {
        CpuRuntime {
            module: None,
            state: HashMap::new(),
            next_state: HashMap::new(),
            signals: HashMap::new(),
            current_cycle: 0,
        }
    }

    fn evaluate_combinational(&mut self) -> Result<(), SimulationError> {
        if let Some(module) = &self.module {
            // Extract combinational cones
            let cones = module.extract_combinational_cones();

            // Evaluate each cone in parallel (simulated)
            for cone in cones {
                self.evaluate_cone(&cone)?;
            }
        }

        Ok(())
    }

    fn evaluate_cone(
        &mut self,
        _cone: &skalp_sir::CombinationalCone,
    ) -> Result<(), SimulationError> {
        // TODO: Implement actual cone evaluation
        // For now, just a placeholder
        Ok(())
    }

    fn evaluate_sequential(&mut self) -> Result<(), SimulationError> {
        if let Some(module) = &self.module {
            for node in &module.sequential_nodes {
                if let skalp_sir::SirNodeKind::FlipFlop { clock_edge: _ } = &node.kind {
                    // Update flip-flop state on clock edge
                    for output in &node.outputs {
                        let signal_name = &output.signal_id;
                        if let Some(current_val) = self.signals.get(signal_name) {
                            self.next_state
                                .insert(signal_name.clone(), current_val.clone());
                        }
                    }
                }
            }

            // Swap state buffers
            std::mem::swap(&mut self.state, &mut self.next_state);
            self.next_state.clear();
        }

        Ok(())
    }

    fn initialize_signals(&mut self, module: &SirModule) {
        // Initialize all signals to zero
        for signal in &module.signals {
            let byte_size = (signal.width + 7) / 8;
            self.signals
                .insert(signal.name.clone(), vec![0u8; byte_size]);
        }

        // Initialize input ports
        for input in &module.inputs {
            let byte_size = (input.width + 7) / 8;
            self.state.insert(input.name.clone(), vec![0u8; byte_size]);
        }

        // Initialize output ports
        for output in &module.outputs {
            let byte_size = (output.width + 7) / 8;
            self.state.insert(output.name.clone(), vec![0u8; byte_size]);
        }

        // Initialize state elements
        for (name, element) in &module.state_elements {
            let byte_size = (element.width + 7) / 8;
            let initial_value = if let Some(reset_val) = element.reset_value {
                let mut bytes = vec![0u8; byte_size];
                // Convert reset value to bytes
                for (i, byte) in bytes.iter_mut().enumerate().take(byte_size) {
                    *byte = ((reset_val >> (i * 8)) & 0xFF) as u8;
                }
                bytes
            } else {
                vec![0u8; byte_size]
            };
            self.state.insert(name.clone(), initial_value);
        }
    }
}

#[async_trait]
impl SimulationRuntime for CpuRuntime {
    async fn initialize(&mut self, module: &SirModule) -> SimulationResult<()> {
        self.module = Some(module.clone());
        self.initialize_signals(module);
        Ok(())
    }

    async fn step(&mut self) -> SimulationResult<SimulationState> {
        // Evaluate combinational logic
        self.evaluate_combinational()?;

        // Evaluate sequential logic
        self.evaluate_sequential()?;

        self.current_cycle += 1;

        // Create simulation state snapshot
        Ok(SimulationState {
            cycle: self.current_cycle,
            signals: self.signals.clone(),
            registers: self.state.clone(),
        })
    }

    async fn run(&mut self, cycles: u64) -> SimulationResult<Vec<SimulationState>> {
        let mut states = Vec::new();

        for _ in 0..cycles {
            let state = self.step().await?;
            states.push(state);

            // Yield occasionally to prevent blocking
            if states.len() % 100 == 0 {
                tokio::task::yield_now().await;
            }
        }

        Ok(states)
    }

    async fn reset(&mut self) -> SimulationResult<()> {
        self.current_cycle = 0;

        // Reset all state elements to initial values
        if let Some(module) = &self.module {
            for (name, element) in &module.state_elements {
                let byte_size = (element.width + 7) / 8;
                let reset_value = if let Some(val) = element.reset_value {
                    let mut bytes = vec![0u8; byte_size];
                    for (i, byte) in bytes.iter_mut().enumerate().take(byte_size) {
                        *byte = ((val >> (i * 8)) & 0xFF) as u8;
                    }
                    bytes
                } else {
                    vec![0u8; byte_size]
                };
                self.state.insert(name.clone(), reset_value);
            }
        }

        // Clear signals
        for (_, signal_data) in self.signals.iter_mut() {
            signal_data.fill(0);
        }

        Ok(())
    }

    async fn set_input(&mut self, name: &str, value: &[u8]) -> SimulationResult<()> {
        if self.state.contains_key(name) {
            self.state.insert(name.to_string(), value.to_vec());
            Ok(())
        } else {
            Err(SimulationError::InvalidInput(format!(
                "Input {} not found",
                name
            )))
        }
    }

    async fn get_output(&self, name: &str) -> SimulationResult<Vec<u8>> {
        self.state
            .get(name)
            .cloned()
            .ok_or_else(|| SimulationError::InvalidInput(format!("Output {} not found", name)))
    }
}
