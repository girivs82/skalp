use crate::clock_manager::ClockManager;
use crate::simulator::{SimulationError, SimulationResult, SimulationRuntime, SimulationState};
use async_trait::async_trait;
use metal::{Buffer, CommandQueue, ComputePipelineState, Device, MTLResourceOptions};
use skalp_sir::{generate_metal_shader, SirModule};
use std::collections::HashMap;

pub struct GpuDevice {
    device: Device,
    command_queue: CommandQueue,
}

impl GpuDevice {
    pub fn new() -> Result<Self, SimulationError> {
        let device = Device::system_default()
            .ok_or_else(|| SimulationError::GpuError("No Metal device found".into()))?;

        let command_queue = device.new_command_queue();

        Ok(GpuDevice {
            device,
            command_queue,
        })
    }
}

pub struct GpuRuntime {
    device: GpuDevice,
    module: Option<SirModule>,
    pipelines: HashMap<String, ComputePipelineState>,
    input_buffer: Option<Buffer>,
    register_buffer: Option<Buffer>,
    signal_buffer: Option<Buffer>,
    current_cycle: u64,
    clock_manager: ClockManager,
}

impl GpuRuntime {
    pub async fn new() -> SimulationResult<Self> {
        let device = GpuDevice::new()?;

        Ok(GpuRuntime {
            device,
            module: None,
            pipelines: HashMap::new(),
            input_buffer: None,
            register_buffer: None,
            signal_buffer: None,
            current_cycle: 0,
            clock_manager: ClockManager::new(),
        })
    }

    fn compile_shader(
        &mut self,
        shader_source: &str,
        function_name: &str,
    ) -> Result<ComputePipelineState, SimulationError> {
        let library = self
            .device
            .device
            .new_library_with_source(shader_source, &metal::CompileOptions::new())
            .map_err(|e| {
                SimulationError::GpuError(format!("Shader compilation failed: {:?}", e))
            })?;

        let function = library.get_function(function_name, None).map_err(|e| {
            SimulationError::GpuError(format!("Function '{}' not found: {}", function_name, e))
        })?;

        let pipeline = self
            .device
            .device
            .new_compute_pipeline_state_with_function(&function)
            .map_err(|e| SimulationError::GpuError(format!("Pipeline creation failed: {:?}", e)))?;

        Ok(pipeline)
    }

    fn allocate_buffers(&mut self, module: &SirModule) -> Result<(), SimulationError> {
        // Calculate buffer sizes
        let input_size = self.calculate_input_size(module);
        let register_size = self.calculate_register_size(module);
        let signal_size = self.calculate_signal_size(module);

        // Allocate input buffer
        self.input_buffer = Some(
            self.device
                .device
                .new_buffer(input_size.max(16), MTLResourceOptions::StorageModeShared),
        );

        // Allocate register buffer (for flip-flop states)
        self.register_buffer = Some(
            self.device
                .device
                .new_buffer(register_size.max(16), MTLResourceOptions::StorageModeShared),
        );

        // Allocate signal buffer (for all computed values)
        self.signal_buffer = Some(
            self.device
                .device
                .new_buffer(signal_size.max(1024), MTLResourceOptions::StorageModeShared),
        );

        // Initialize registers to zero
        if let Some(reg_buffer) = &self.register_buffer {
            let ptr = reg_buffer.contents();
            unsafe {
                std::ptr::write_bytes(ptr, 0, register_size as usize);
            }
        }

        Ok(())
    }

    fn calculate_input_size(&self, module: &SirModule) -> u64 {
        let mut size = 0u64;
        for input in &module.inputs {
            size += self.get_metal_type_size(input.width) as u64;
        }
        size
    }

    fn calculate_register_size(&self, module: &SirModule) -> u64 {
        let mut size = 0u64;
        for state in module.state_elements.values() {
            size += self.get_metal_type_size(state.width) as u64;
        }
        size
    }

    fn calculate_signal_size(&self, module: &SirModule) -> u64 {
        let mut size = 0u64;

        // Outputs go in signals
        for output in &module.outputs {
            size += self.get_metal_type_size(output.width) as u64;
        }

        // All non-state signals
        for signal in &module.signals {
            if !signal.is_state {
                size += self.get_metal_type_size(signal.width) as u64;
            }
        }

        size
    }

    fn get_metal_type_size(&self, width: usize) -> usize {
        match width {
            1..=32 => 4,  // uint (32-bit)
            33..=64 => 8, // uint2 (64-bit)
            _ => 16,      // uint4 (128-bit)
        }
    }

    async fn execute_combinational(&mut self) -> Result<(), SimulationError> {
        // Get the number of combinational cones from the module
        let cone_count = if let Some(module) = &self.module {
            let cones = module.extract_combinational_cones();
            if cones.is_empty() {
                1
            } else {
                cones.len()
            } // Execute empty kernel if no cones
        } else {
            0
        };

        // Execute each combinational cone kernel
        for i in 0..cone_count {
            let pipeline_name = format!("combinational_{}", i);
            if let Some(pipeline) = self.pipelines.get(&pipeline_name) {
                let command_buffer = self.device.command_queue.new_command_buffer();
                let encoder = command_buffer.new_compute_command_encoder();

                encoder.set_compute_pipeline_state(pipeline);

                // Set buffers: inputs, registers, signals
                if let Some(input_buffer) = &self.input_buffer {
                    encoder.set_buffer(0, Some(input_buffer), 0);
                }

                if let Some(register_buffer) = &self.register_buffer {
                    encoder.set_buffer(1, Some(register_buffer), 0);
                }

                if let Some(signal_buffer) = &self.signal_buffer {
                    encoder.set_buffer(2, Some(signal_buffer), 0);
                }

                let thread_groups = metal::MTLSize {
                    width: 1,
                    height: 1,
                    depth: 1,
                };
                let threads_per_group = metal::MTLSize {
                    width: 64,
                    height: 1,
                    depth: 1,
                };

                encoder.dispatch_thread_groups(thread_groups, threads_per_group);
                encoder.end_encoding();

                command_buffer.commit();
                command_buffer.wait_until_completed();
            }
        }

        Ok(())
    }

    async fn execute_sequential(&mut self) -> Result<(), SimulationError> {
        // Check if we have any clock edges by looking at current vs prev clock values
        let mut has_edge = false;
        if let Some(module) = &self.module {
            for input in &module.inputs {
                if input.name.contains("clk") || input.name.contains("clock") {
                    // Check if this clock has changed
                    if let Some(input_buffer) = &self.input_buffer {
                        let ptr = input_buffer.contents() as *const u32;
                        let current_value = unsafe { *ptr } != 0;
                        let prev_value = self
                            .clock_manager
                            .clocks
                            .get(&input.name)
                            .map(|c| c.previous_value)
                            .unwrap_or(false);

                        if current_value && !prev_value {
                            // Rising edge detected
                            has_edge = true;
                        }
                    }
                }
            }
        }

        if !has_edge {
            return Ok(());
        }

        if let Some(pipeline) = self.pipelines.get("sequential") {
            let command_buffer = self.device.command_queue.new_command_buffer();
            let encoder = command_buffer.new_compute_command_encoder();

            encoder.set_compute_pipeline_state(pipeline);

            // Set buffers: inputs, registers (in/out), signals
            if let Some(input_buffer) = &self.input_buffer {
                encoder.set_buffer(0, Some(input_buffer), 0);
            }

            if let Some(register_buffer) = &self.register_buffer {
                encoder.set_buffer(1, Some(register_buffer), 0);
            }

            if let Some(signal_buffer) = &self.signal_buffer {
                encoder.set_buffer(2, Some(signal_buffer), 0);
            }

            let thread_groups = metal::MTLSize {
                width: 1,
                height: 1,
                depth: 1,
            };
            let threads_per_group = metal::MTLSize {
                width: 64,
                height: 1,
                depth: 1,
            };

            encoder.dispatch_thread_groups(thread_groups, threads_per_group);
            encoder.end_encoding();

            command_buffer.commit();
            command_buffer.wait_until_completed();
        }

        Ok(())
    }

    fn extract_state(&self) -> SimulationState {
        let mut signals = HashMap::new();
        let mut registers = HashMap::new();

        // Read data from GPU buffers
        if let Some(module) = &self.module {
            // Read outputs from signal buffer
            if let Some(signal_buffer) = &self.signal_buffer {
                let signal_ptr = signal_buffer.contents() as *const u8;
                let mut offset = 0usize;

                // Read outputs first
                for output in &module.outputs {
                    let metal_size = self.get_metal_type_size(output.width);
                    let bytes_needed = output.width.div_ceil(8);
                    let mut value = vec![0u8; bytes_needed];
                    unsafe {
                        std::ptr::copy_nonoverlapping(
                            signal_ptr.add(offset),
                            value.as_mut_ptr(),
                            bytes_needed.min(metal_size),
                        );
                    }
                    signals.insert(output.name.clone(), value);
                    offset += metal_size;
                }

                // Read intermediate signals
                for signal in &module.signals {
                    if !signal.is_state {
                        let metal_size = self.get_metal_type_size(signal.width);
                        let bytes_needed = signal.width.div_ceil(8);
                        let mut value = vec![0u8; bytes_needed];
                        unsafe {
                            std::ptr::copy_nonoverlapping(
                                signal_ptr.add(offset),
                                value.as_mut_ptr(),
                                bytes_needed.min(metal_size),
                            );
                        }
                        signals.insert(signal.name.clone(), value);
                        offset += metal_size;
                    }
                }
            }

            // Read state elements from register buffer
            if let Some(register_buffer) = &self.register_buffer {
                let register_ptr = register_buffer.contents() as *const u8;
                let mut offset = 0usize;

                for (name, state_elem) in &module.state_elements {
                    let metal_size = self.get_metal_type_size(state_elem.width);
                    let bytes_needed = state_elem.width.div_ceil(8);
                    let mut value = vec![0u8; bytes_needed];
                    unsafe {
                        std::ptr::copy_nonoverlapping(
                            register_ptr.add(offset),
                            value.as_mut_ptr(),
                            bytes_needed.min(metal_size),
                        );
                    }
                    registers.insert(name.clone(), value);
                    offset += metal_size;
                }
            }
        }

        SimulationState {
            cycle: self.current_cycle,
            signals,
            registers,
        }
    }
}

#[async_trait]
impl SimulationRuntime for GpuRuntime {
    async fn initialize(&mut self, module: &SirModule) -> SimulationResult<()> {
        self.module = Some(module.clone());

        // Register clocks from the module
        for name in module.clock_domains.keys() {
            // Default to 10ns period (100MHz)
            self.clock_manager.add_clock(name.clone(), 10_000);
        }

        // Also check for clock inputs
        for input in &module.inputs {
            if input.name.contains("clk") || input.name.contains("clock") {
                // Just add it, the clock manager will handle duplicates
                self.clock_manager.add_clock(input.name.clone(), 10_000);
            }
        }

        // Generate Metal shader from SIR
        let shader_source = generate_metal_shader(module);

        // Get the number of combinational cones
        let cones = module.extract_combinational_cones();

        // Compile combinational cone kernels
        if cones.is_empty() {
            // If no cones, compile the empty kernel
            let pipeline = self.compile_shader(&shader_source, "combinational_cone_0")?;
            self.pipelines
                .insert("combinational_0".to_string(), pipeline);
        } else {
            for (i, _cone) in cones.iter().enumerate() {
                let function_name = format!("combinational_cone_{}", i);
                let pipeline = self.compile_shader(&shader_source, &function_name)?;
                self.pipelines
                    .insert(format!("combinational_{}", i), pipeline);
            }
        }

        // Compile sequential kernel if there are sequential nodes
        if !module.sequential_nodes.is_empty() {
            let pipeline = self.compile_shader(&shader_source, "sequential_update")?;
            self.pipelines.insert("sequential".to_string(), pipeline);
        }

        // Allocate GPU buffers
        self.allocate_buffers(module)?;

        Ok(())
    }

    async fn step(&mut self) -> SimulationResult<SimulationState> {
        // CRITICAL TWO-PHASE EXECUTION for correct non-blocking semantics:

        // Phase 1: Combinational logic computes values that will be sampled by flip-flops
        // This ensures that when we read from state elements in sequential assignments,
        // we get the OLD values (before the clock edge updates them)
        self.execute_combinational().await?;

        // Phase 2: Sequential logic updates registers on clock edge
        // Flip-flops sample their data inputs (computed in phase 1) and update register values
        self.execute_sequential().await?;

        // Phase 3: Re-execute combinational logic to update outputs based on new register state
        // This ensures that outputs (which are often just wires from registers) reflect
        // the CURRENT register state, not the old state
        self.execute_combinational().await?;

        self.current_cycle += 1;

        // Debug: Print register values when they have meaningful data
        if self.current_cycle >= 1 && self.current_cycle <= 35 {
            if let Some(register_buffer) = &self.register_buffer {
                let register_ptr = register_buffer.contents() as *const u32;
                let reg0 = unsafe { *register_ptr.offset(0) };
                let reg1 = unsafe { *register_ptr.offset(1) };
                let reg2 = unsafe { *register_ptr.offset(2) };
                let reg3 = unsafe { *register_ptr.offset(3) };
                let reg4 = unsafe { *register_ptr.offset(4) };
                let reg5 = unsafe { *register_ptr.offset(5) };

                // Check input values
                if let Some(input_buffer) = &self.input_buffer {
                    let input_ptr = input_buffer.contents() as *const u32;
                    let rst = unsafe { *input_ptr.offset(1) };
                    let instruction = unsafe { *input_ptr.offset(2) };
                    let data_in = unsafe { *input_ptr.offset(3) };

                    // Also check signal buffer
                    if let Some(signal_buffer) = &self.signal_buffer {
                        let signal_ptr = signal_buffer.contents() as *const u32;
                        let result_signal = unsafe { *signal_ptr.offset(0) };
                        let valid_signal = unsafe { *signal_ptr.offset(1) };

                        eprintln!("DEBUG Cycle {}: rst={}, instr=0x{:04X}, data={}, regs=[{},{},{},{},{},{}], signals=[result={}, valid={}]",
                            self.current_cycle, rst, instruction, data_in, reg0, reg1, reg2, reg3, reg4, reg5, result_signal, valid_signal);
                    } else {
                        eprintln!("DEBUG Cycle {}: rst={}, instr=0x{:04X}, data={}, regs=[{},{},{},{},{},{}]",
                            self.current_cycle, rst, instruction, data_in, reg0, reg1, reg2, reg3, reg4, reg5);
                    }
                }
            }
        }

        // Extract and return current state
        Ok(self.extract_state())
    }

    async fn run(&mut self, cycles: u64) -> SimulationResult<Vec<SimulationState>> {
        let mut states = Vec::new();

        for _ in 0..cycles {
            let state = self.step().await?;
            states.push(state);
        }

        Ok(states)
    }

    async fn reset(&mut self) -> SimulationResult<()> {
        self.current_cycle = 0;

        // Clear GPU buffers
        // TODO: Implement proper reset logic

        Ok(())
    }

    async fn set_input(&mut self, name: &str, value: &[u8]) -> SimulationResult<()> {
        // Update clock manager for clock signals
        if name.contains("clk") || name.contains("clock") {
            let clock_value = value[0] != 0;
            self.clock_manager.set_clock(name, clock_value);
        }

        // Write to input buffer
        if let Some(module) = &self.module {
            if let Some(input_buffer) = &self.input_buffer {
                let input_ptr = input_buffer.contents() as *mut u8;
                let mut offset = 0usize;

                // Find the input and write to its location
                for input in &module.inputs {
                    let metal_size = self.get_metal_type_size(input.width);
                    let bytes_needed = input.width.div_ceil(8);
                    if input.name == name {
                        if value.len() != bytes_needed {
                            return Err(SimulationError::GpuError(format!(
                                "Input {} expects {} bytes, got {}",
                                name,
                                bytes_needed,
                                value.len()
                            )));
                        }
                        unsafe {
                            // Clear the Metal-sized buffer first
                            std::ptr::write_bytes(input_ptr.add(offset), 0, metal_size);
                            // Then copy our data
                            std::ptr::copy_nonoverlapping(
                                value.as_ptr(),
                                input_ptr.add(offset),
                                bytes_needed.min(metal_size),
                            );
                        }
                        return Ok(());
                    }
                    offset += metal_size;
                }
            }
        }
        Err(SimulationError::GpuError(format!(
            "Input {} not found",
            name
        )))
    }

    async fn get_output(&self, name: &str) -> SimulationResult<Vec<u8>> {
        if let Some(module) = &self.module {
            if let Some(signal_buffer) = &self.signal_buffer {
                let signal_ptr = signal_buffer.contents() as *const u8;
                let mut offset = 0usize;

                // Find the output in the signals buffer
                for output in &module.outputs {
                    let metal_size = self.get_metal_type_size(output.width);
                    let bytes_needed = output.width.div_ceil(8);
                    if output.name == name {
                        let mut value = vec![0u8; bytes_needed];
                        unsafe {
                            std::ptr::copy_nonoverlapping(
                                signal_ptr.add(offset),
                                value.as_mut_ptr(),
                                bytes_needed.min(metal_size),
                            );
                        }
                        return Ok(value);
                    }
                    offset += metal_size;
                }
            }
        }
        Err(SimulationError::GpuError(format!(
            "Output {} not found",
            name
        )))
    }
}
