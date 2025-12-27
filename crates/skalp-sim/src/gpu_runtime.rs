use crate::clock_manager::ClockManager;
use crate::simulator::{SimulationError, SimulationResult, SimulationRuntime, SimulationState};
use async_trait::async_trait;
use metal::{Buffer, CommandQueue, ComputePipelineState, Device, MTLResourceOptions};
use skalp_sir::{generate_metal_shader, SirModule};
use std::collections::HashMap;
use std::fs;

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
    output_buffer: Option<Buffer>, // Stores outputs from BEFORE sequential update
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
            output_buffer: None,
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

        // Allocate output buffer (for storing outputs before sequential update)
        let output_size = self.calculate_output_size(module);
        self.output_buffer = Some(
            self.device
                .device
                .new_buffer(output_size.max(16), MTLResourceOptions::StorageModeShared),
        );

        // Initialize registers to zero
        if let Some(reg_buffer) = &self.register_buffer {
            let ptr = reg_buffer.contents();
            unsafe {
                std::ptr::write_bytes(ptr, 0, register_size as usize);
            }
        }

        // Initialize inputs to zero to prevent undefined behavior
        if let Some(input_buffer) = &self.input_buffer {
            let ptr = input_buffer.contents();
            unsafe {
                std::ptr::write_bytes(ptr, 0, input_size as usize);
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
        // Sort state elements by name for consistent ordering with Metal shader
        let mut sorted_states: Vec<_> = module.state_elements.values().collect();
        sorted_states.sort_by_key(|state| &state.name);
        for state in sorted_states {
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

        // Build set of input port names to exclude
        let input_names: std::collections::HashSet<&str> =
            module.inputs.iter().map(|i| i.name.as_str()).collect();

        // All non-state signals (excluding input ports)
        // Input ports are in the Inputs struct, not the Signals struct
        for signal in &module.signals {
            if !signal.is_state && !input_names.contains(signal.name.as_str()) {
                size += self.get_metal_type_size(signal.width) as u64;
            }
        }

        size
    }

    fn calculate_output_size(&self, module: &SirModule) -> u64 {
        let mut size = 0u64;
        for output in &module.outputs {
            size += self.get_metal_type_size(output.width) as u64;
        }
        size
    }

    fn get_metal_type_size(&self, width: usize) -> usize {
        match width {
            1..=32 => 4,     // uint (32-bit)
            33..=64 => 8,    // uint2 (64-bit)
            65..=128 => 16,  // uint4 (128-bit)
            129..=256 => 32, // uint[8] (256-bit array)
            _ => {
                // BUG FIX #71e: Handle decomposed signals (>256 bits)
                // Calculate sum of all parts' sizes
                let num_parts = width.div_ceil(256);
                let mut total_size = 0;
                for part_idx in 0..num_parts {
                    let part_width = if part_idx == num_parts - 1 {
                        width - (num_parts - 1) * 256
                    } else {
                        256
                    };
                    total_size += self.get_metal_type_size(part_width);
                }
                total_size
            }
        }
    }

    fn capture_outputs(&mut self) -> Result<(), SimulationError> {
        // Copy output values from signal buffer to output buffer
        // This preserves the output values from BEFORE the sequential update
        if let (Some(signal_buffer), Some(output_buffer), Some(module)) =
            (&self.signal_buffer, &self.output_buffer, &self.module)
        {
            let signal_ptr = signal_buffer.contents() as *const u8;
            let output_ptr = output_buffer.contents() as *mut u8;
            let mut signal_offset = 0usize;
            let mut output_offset = 0usize;

            let debug = std::env::var("SKALP_DEBUG_OUTPUT").is_ok();

            // Unconditional debug to verify capture_outputs is being called
            // Outputs are at the beginning of the signal buffer
            for output in &module.outputs {
                let metal_size = self.get_metal_type_size(output.width);

                unsafe {
                    // BUG #181 FIX: Apply bit width masking before copying
                    // Metal shader NOT operations (~x) produce 32-bit NOT, but for
                    // narrower signals we need to mask to the proper bit width
                    if output.width < 32 {
                        let raw_value = *(signal_ptr.add(signal_offset) as *const u32);
                        let mask = (1u32 << output.width) - 1;
                        let masked_value = raw_value & mask;
                        *(output_ptr.add(output_offset) as *mut u32) = masked_value;
                    } else {
                        std::ptr::copy_nonoverlapping(
                            signal_ptr.add(signal_offset),
                            output_ptr.add(output_offset),
                            metal_size,
                        );
                    }
                }
                signal_offset += metal_size;
                output_offset += metal_size;
            }
        }
        Ok(())
    }

    /// BUG #180 FIX: Update clock manager's previous_value to match current input buffer values
    /// This ensures that on the next step(), if the clock hasn't changed, no edge is detected.
    fn update_clock_previous_values(&mut self) {
        if let Some(module) = &self.module {
            if let Some(input_buffer) = &self.input_buffer {
                let ptr = input_buffer.contents() as *const u32;
                let mut input_offset = 0usize;

                for input in &module.inputs {
                    if input.name.contains("clk") || input.name.contains("clock") {
                        // Read current clock value from input buffer
                        let current_value = unsafe { *ptr.add(input_offset) } != 0;
                        // Update clock manager so previous_value = current_value
                        // This is done by calling set_clock with the SAME value,
                        // which will shift current_value to previous_value
                        self.clock_manager.set_clock(&input.name, current_value);
                    }
                    // Advance offset by input width (in u32 words)
                    input_offset += (input.width + 31) / 32;
                }
            }
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

        // No debug here - moved to step() function

        Ok(())
    }

    /// Execute sequential kernel with edge detection
    async fn execute_sequential(&mut self) -> Result<(), SimulationError> {
        self.execute_sequential_with_edges(false).await
    }

    /// Execute sequential kernel, optionally forcing execution even without detected edges
    async fn execute_sequential_with_edges(&mut self, force: bool) -> Result<(), SimulationError> {
        // Check if we have any clock edges by looking at current vs prev clock values
        let mut has_edge = false;
        if let Some(module) = &self.module {
            // BUG #179 FIX: Track input offset to read from correct buffer position
            // Previously this always read from offset 0, causing only the first clock to be detected
            let mut input_offset = 0usize;
            for input in &module.inputs {
                if input.name.contains("clk") || input.name.contains("clock") {
                    // Check if this clock has changed
                    if let Some(input_buffer) = &self.input_buffer {
                        let ptr = input_buffer.contents() as *const u32;
                        // BUG #179 FIX: Read from correct offset for this input
                        let current_value = unsafe { *ptr.add(input_offset) } != 0;
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
                // Advance offset by input width (in u32 words)
                input_offset += (input.width + 31) / 32;
            }
        }

        // Skip if no edges detected and not forcing execution
        if !has_edge && !force {
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
            // Build set of output names to avoid reading them twice
            let output_names: std::collections::HashSet<_> =
                module.outputs.iter().map(|o| &o.name).collect();

            // Read outputs from output_buffer (buffered values from BEFORE sequential update)
            if let Some(output_buffer) = &self.output_buffer {
                let output_ptr = output_buffer.contents() as *const u8;
                let mut offset = 0usize;

                for output in &module.outputs {
                    let metal_size = self.get_metal_type_size(output.width);
                    let bytes_needed = output.width.div_ceil(8);
                    let mut value = vec![0u8; bytes_needed];
                    unsafe {
                        std::ptr::copy_nonoverlapping(
                            output_ptr.add(offset),
                            value.as_mut_ptr(),
                            bytes_needed.min(metal_size),
                        );
                    }
                    signals.insert(output.name.clone(), value);
                    offset += metal_size;
                }
            }

            // Read intermediate signals from signal buffer (skipping outputs and input ports)
            if let Some(signal_buffer) = &self.signal_buffer {
                let signal_ptr = signal_buffer.contents() as *const u8;
                // Skip past outputs in signal buffer to get to intermediate signals
                let mut offset = module
                    .outputs
                    .iter()
                    .map(|o| self.get_metal_type_size(o.width))
                    .sum::<usize>();

                // Build set of input port names to exclude (they're in Inputs struct, not Signals)
                let input_names: std::collections::HashSet<_> =
                    module.inputs.iter().map(|i| i.name.as_str()).collect();

                // Read intermediate signals (skipping outputs, input ports, and state elements)
                for signal in &module.signals {
                    if !signal.is_state
                        && !output_names.contains(&signal.name)
                        && !input_names.contains(signal.name.as_str())
                    {
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

                // Sort state elements by name for consistent ordering with Metal shader
                let mut sorted_states: Vec<_> = module.state_elements.iter().collect();
                sorted_states.sort_by_key(|(name, _)| *name);

                for (name, state_elem) in sorted_states {
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

        // DEBUG: Write Metal shader source to file for inspection
        if let Err(e) = fs::write("/tmp/skalp_metal_shader.metal", &shader_source) {
            eprintln!(
                "Warning: Could not write Metal shader to /tmp/skalp_metal_shader.metal: {}",
                e
            );
        } else {
            eprintln!("✅ Metal shader written to /tmp/skalp_metal_shader.metal");
        }

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
        // THREE-PHASE EXECUTION for correct FWFT (First-Word-Fall-Through) FIFO semantics:
        //
        // Phase 1: Combinational logic computes values using current register state
        // Phase 2: Sequential logic updates registers on clock edge
        // Phase 3: Combinational logic recomputes outputs with new register values
        //
        // KNOWN LIMITATION (BUG #182): Complex designs with state machines that control
        // FIFO writes in the same clock cycle may not simulate correctly. This is because
        // the GPU evaluates all signals atomically, while CPU simulation evaluates them
        // sequentially within each clock cycle. The simpler AsyncFifo tests pass, but
        // the multi-clock graphics pipeline test may fail on GPU.
        // Workaround: Use SKALP_SIM_MODE=cpu for complex multi-domain designs.

        // Phase 1: Combinational logic computes values using current register state
        self.execute_combinational().await?;

        // Phase 2: Sequential logic updates registers on clock edge
        self.execute_sequential().await?;

        // Phase 3: Re-execute combinational logic with new register values (FWFT)
        self.execute_combinational().await?;

        // Capture outputs after all phases complete
        self.capture_outputs()?;

        self.current_cycle += 1;

        // BUG #180 FIX: Update clock manager's previous_value to current value
        // This ensures that on the next step(), if the clock hasn't changed,
        // we won't incorrectly detect a rising edge again.
        // Without this fix, the clock_manager's previous_value only gets updated
        // when set_input() is called, so if the testbench doesn't call set_input()
        // between steps, we'd incorrectly detect edges on every step.
        self.update_clock_previous_values();

        // Debug: Print register values when they have meaningful data
        // Disabled debug output - enable by changing false to true
        #[allow(clippy::overly_complex_bool_expr)]
        if false {
            if let Some(register_buffer) = &self.register_buffer {
                let register_ptr = register_buffer.contents() as *const u32;
                // AsyncFifo registers (alphabetically sorted in Metal struct):
                // reg0-7: fifo_mem_0-7_value
                // reg8: fifo_rd_ptr
                // reg9: fifo_rd_ptr_gray
                // reg10: fifo_rd_ptr_gray_sync1
                // reg11: fifo_rd_ptr_gray_sync2
                // reg12: fifo_wr_ptr
                // reg13: fifo_wr_ptr_gray
                // reg14: fifo_wr_ptr_gray_sync1
                // reg15: fifo_wr_ptr_gray_sync2
                let reg0 = unsafe { *register_ptr.offset(0) };
                let reg1 = unsafe { *register_ptr.offset(1) };
                let reg2 = unsafe { *register_ptr.offset(2) };
                let reg8 = unsafe { *register_ptr.offset(8) };
                let reg9 = unsafe { *register_ptr.offset(9) };
                let reg10 = unsafe { *register_ptr.offset(10) };
                let reg11 = unsafe { *register_ptr.offset(11) };
                let reg12 = unsafe { *register_ptr.offset(12) };
                let reg13 = unsafe { *register_ptr.offset(13) };
                let reg14 = unsafe { *register_ptr.offset(14) };
                let reg15 = unsafe { *register_ptr.offset(15) };

                // Check input values - print ALL inputs
                if let Some(input_buffer) = &self.input_buffer {
                    let input_ptr = input_buffer.contents() as *const u32;
                    // Print first 8 input words
                    let in0 = unsafe { *input_ptr.offset(0) };
                    let in1 = unsafe { *input_ptr.offset(1) };
                    let in2 = unsafe { *input_ptr.offset(2) };
                    let in3 = unsafe { *input_ptr.offset(3) };
                    let in4 = unsafe { *input_ptr.offset(4) };
                    let in5 = unsafe { *input_ptr.offset(5) };
                    let in6 = unsafe { *input_ptr.offset(6) };

                    // Also check signal buffer for intermediate values
                    if let Some(signal_buffer) = &self.signal_buffer {
                        let _signal_ptr = signal_buffer.contents() as *const u32;

                        // Only print detailed debug on key cycles
                        if self.current_cycle == 7 || self.current_cycle == 8 {
                            eprintln!("DEBUG Cycle {}: inputs=[wr_clk:{}, wr_rst:{}, rd_clk:{}, rd_rst:{}, wr_data:0x{:08X}, wr_en:{}, rd_en:{}]",
                                self.current_cycle, in0, in1, in2, in3, in4, in5, in6);
                            eprintln!("  regs=[mem[0-2]:{},{},{}, rd_ptr:{}, rd_ptr_gray:{}, rd_ptr_gray_sync1:{}, rd_ptr_gray_sync2:{}, wr_ptr:{}, wr_ptr_gray:{}, wr_ptr_gray_sync1:{}, wr_ptr_gray_sync2:{}]",
                                reg0, reg1, reg2, reg8, reg9, reg10, reg11, reg12, reg13, reg14, reg15);
                        }
                    } else {
                        eprintln!("DEBUG Cycle {}: inputs=[wr_clk:{}, wr_rst:{}, rd_clk:{}, rd_rst:{}, wr_data:0x{:08X}, wr_en:{}, rd_en:{}], regs=[mem[0-2]:{},{},{}, rd_ptr:{}, rd_ptr_gray:{}, rd_ptr_gray_sync1:{}, rd_ptr_gray_sync2:{}, wr_ptr:{}, wr_ptr_gray:{}, wr_ptr_gray_sync1:{}, wr_ptr_gray_sync2:{}]",
                            self.current_cycle, in0, in1, in2, in3, in4, in5, in6, reg0, reg1, reg2, reg8, reg9, reg10, reg11, reg12, reg13, reg14, reg15);
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
            // Read from output_buffer which contains outputs from BEFORE sequential update
            if let Some(output_buffer) = &self.output_buffer {
                let output_ptr = output_buffer.contents() as *const u8;
                let mut offset = 0usize;

                // Find the output in the output buffer
                for output in &module.outputs {
                    let metal_size = self.get_metal_type_size(output.width);
                    let bytes_needed = output.width.div_ceil(8);
                    if output.name == name {
                        let mut value = vec![0u8; bytes_needed];
                        unsafe {
                            std::ptr::copy_nonoverlapping(
                                output_ptr.add(offset),
                                value.as_mut_ptr(),
                                bytes_needed.min(metal_size),
                            );
                        }
                        // DEBUG: Print what we read for multi-clock debugging
                        if std::env::var("SKALP_DEBUG_OUTPUT").is_ok() {
                            eprintln!(
                                "[DEBUG get_output] name='{}' width={} metal_size={} bytes_needed={} offset={} value={:?}",
                                name, output.width, metal_size, bytes_needed, offset, value
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
