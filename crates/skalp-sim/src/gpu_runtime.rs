use crate::clock_manager::ClockManager;
use crate::simulator::{SimulationError, SimulationResult, SimulationRuntime, SimulationState};
use async_trait::async_trait;
use indexmap::IndexMap;
use metal::{Buffer, CommandQueue, ComputePipelineState, Device, MTLResourceOptions};
use skalp_sir::{MetalBackend, SirModule};

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
    pipelines: IndexMap<String, ComputePipelineState>,
    input_buffer: Option<Buffer>,
    register_buffer: Option<Buffer>,
    // BUG #182 FIX: Shadow register buffer for double-buffered sequential updates
    // Sequential kernel reads from register_buffer, writes to shadow_register_buffer
    // After sequential, we swap the buffers (copy shadow to main)
    shadow_register_buffer: Option<Buffer>,
    signal_buffer: Option<Buffer>,
    output_buffer: Option<Buffer>, // Stores outputs from BEFORE sequential update
    // PERF: Params buffer for batched simulation kernel
    params_buffer: Option<Buffer>,
    current_cycle: u64,
    clock_manager: ClockManager,
    // PERF: Cache cone count to avoid expensive extract_combinational_cones() every step
    cached_cone_count: usize,
    // PERF: Clock signal offset in input buffer for batched kernel
    clock_signal_offset: usize,
}

impl GpuRuntime {
    pub async fn new() -> SimulationResult<Self> {
        let device = GpuDevice::new()?;

        Ok(GpuRuntime {
            device,
            module: None,
            pipelines: IndexMap::new(),
            input_buffer: None,
            register_buffer: None,
            shadow_register_buffer: None,
            signal_buffer: None,
            output_buffer: None,
            params_buffer: None,
            current_cycle: 0,
            clock_manager: ClockManager::new(),
            cached_cone_count: 0,
            clock_signal_offset: 0,
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

        // BUG #182 FIX: Allocate shadow register buffer for double-buffered updates
        self.shadow_register_buffer = Some(
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
        // BUG FIX #182: Account for Metal struct alignment padding
        let mut offset = 0u64;
        for input in &module.inputs {
            let metal_size = self.get_metal_type_size(input.width) as u64;
            let metal_align = self.get_metal_type_alignment(input.width) as u64;

            // Align offset to the required alignment boundary
            let remainder = offset % metal_align;
            if remainder != 0 {
                offset += metal_align - remainder;
            }
            offset += metal_size;
        }
        offset
    }

    fn calculate_register_size(&self, module: &SirModule) -> u64 {
        // BUG FIX #182: Account for Metal struct alignment padding
        let mut offset = 0u64;
        // Sort state elements by name for consistent ordering with Metal shader
        let mut sorted_states: Vec<_> = module.state_elements.values().collect();
        sorted_states.sort_by_key(|state| &state.name);
        for state in sorted_states {
            let metal_size = self.get_metal_type_size(state.width) as u64;
            let metal_align = self.get_metal_type_alignment(state.width) as u64;

            // Align offset to the required alignment boundary
            let remainder = offset % metal_align;
            if remainder != 0 {
                offset += metal_align - remainder;
            }
            offset += metal_size;
        }
        offset
    }

    fn calculate_signal_size(&self, module: &SirModule) -> u64 {
        // BUG FIX #182: Account for Metal struct alignment padding
        let mut offset = 0u64;

        // Outputs go in signals
        for output in &module.outputs {
            let metal_size = self.get_metal_type_size(output.width) as u64;
            let metal_align = self.get_metal_type_alignment(output.width) as u64;

            let remainder = offset % metal_align;
            if remainder != 0 {
                offset += metal_align - remainder;
            }
            offset += metal_size;
        }

        // Build set of input port names to exclude
        let input_names: std::collections::HashSet<&str> =
            module.inputs.iter().map(|i| i.name.as_str()).collect();

        // All non-state signals (excluding input ports)
        // Input ports are in the Inputs struct, not the Signals struct
        for signal in &module.signals {
            if !signal.is_state && !input_names.contains(signal.name.as_str()) {
                let metal_size = self.get_metal_type_size(signal.width) as u64;
                let metal_align = self.get_metal_type_alignment(signal.width) as u64;

                let remainder = offset % metal_align;
                if remainder != 0 {
                    offset += metal_align - remainder;
                }
                offset += metal_size;
            }
        }

        offset
    }

    fn calculate_output_size(&self, module: &SirModule) -> u64 {
        // BUG FIX #182: Account for Metal struct alignment padding
        let mut offset = 0u64;
        for output in &module.outputs {
            let metal_size = self.get_metal_type_size(output.width) as u64;
            let metal_align = self.get_metal_type_alignment(output.width) as u64;

            // Align offset to the required alignment boundary
            let remainder = offset % metal_align;
            if remainder != 0 {
                offset += metal_align - remainder;
            }
            offset += metal_size;
        }
        offset
    }

    /// Get the alignment requirement for a Metal type based on bit width
    /// Metal requires types to be naturally aligned:
    /// - uint (32-bit) -> 4-byte alignment
    /// - uint2 (64-bit) -> 8-byte alignment
    /// - uint4 (128-bit) -> 16-byte alignment
    fn get_metal_type_alignment(&self, width: usize) -> usize {
        match width {
            1..=32 => 4,    // uint aligns to 4 bytes
            33..=64 => 8,   // uint2 aligns to 8 bytes
            65..=128 => 16, // uint4 aligns to 16 bytes
            _ => 4,         // Arrays of uint align to 4 bytes
        }
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

            // Outputs are at the beginning of the signal buffer
            // BUG FIX: Skip state element outputs - they're NOT in the Signals struct
            // (Metal shader generator skips them too, see codegen/shared.rs)
            // State element outputs are read from register_buffer in get_output() fallback
            for output in &module.outputs {
                // Skip state element outputs - they're in Registers, not Signals
                // Don't advance output_offset either - get_output() also skips them
                if module.state_elements.contains_key(&output.name) {
                    continue;
                }

                let metal_size = self.get_metal_type_size(output.width);
                let metal_align = self.get_metal_type_alignment(output.width);

                // BUG #185 FIX: Apply same alignment to signal buffer reads
                let sig_remainder = signal_offset % metal_align;
                if sig_remainder != 0 {
                    signal_offset += metal_align - sig_remainder;
                }
                let out_remainder = output_offset % metal_align;
                if out_remainder != 0 {
                    output_offset += metal_align - out_remainder;
                }

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
                    // BUG FIX #117r: Check against registered clocks, not name matching
                    let is_clock = self.clock_manager.clocks.contains_key(&input.name);
                    if is_clock {
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

    /// BUG #182 FIX: Create a frozen snapshot of current registers at the start of step()
    /// This snapshot is used as the "pre-edge" values that sequential logic reads from.
    /// The working register buffer receives updates, but sequential always reads from snapshot.
    fn create_register_snapshot(&mut self) {
        if let (Some(main_buffer), Some(shadow_buffer)) =
            (&self.register_buffer, &self.shadow_register_buffer)
        {
            let size = main_buffer.length() as usize;
            let main_ptr = main_buffer.contents() as *const u8;
            let shadow_ptr = shadow_buffer.contents() as *mut u8;

            // Copy main -> shadow (shadow becomes the frozen snapshot)
            unsafe {
                std::ptr::copy_nonoverlapping(main_ptr, shadow_ptr, size);
            }
        }
    }

    async fn execute_combinational(&mut self) -> Result<(), SimulationError> {
        // PERF: Use cached cone count instead of expensive extract_combinational_cones()
        let cone_count = self.cached_cone_count;
        if cone_count == 0 {
            return Ok(());
        }

        // PERF: Batch all cones into a single command buffer to reduce GPU overhead
        // Previously each cone had its own command buffer with synchronous wait,
        // causing ~100 GPU round-trips per step (50 cones × 2 combinational passes)
        let command_buffer = self.device.command_queue.new_command_buffer();

        for i in 0..cone_count {
            let pipeline_name = format!("combinational_{}", i);
            if let Some(pipeline) = self.pipelines.get(&pipeline_name) {
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
            }
        }

        // PERF: Single wait for all cones instead of one per cone
        command_buffer.commit();
        command_buffer.wait_until_completed();

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
                // BUG FIX #117r: Check if this input is a registered clock (not just name matching)
                let is_clock = self.clock_manager.clocks.contains_key(&input.name);
                if is_clock {
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

            // BUG #182 FIX: Double-buffered register updates for idempotent sequential execution
            // Buffer 0: inputs (read-only)
            // Buffer 1: shadow_register_buffer - FROZEN snapshot from start of step() (read-only)
            // Buffer 2: signals (read-only)
            // Buffer 3: register_buffer - working buffer that receives updates (write-only)
            //
            // By reading from the frozen snapshot, all iterations of sequential logic
            // see the same pre-edge register values, making the operation idempotent.
            if let Some(input_buffer) = &self.input_buffer {
                encoder.set_buffer(0, Some(input_buffer), 0);
            }

            // Buffer 1: Frozen snapshot (created at start of step)
            if let Some(shadow_buffer) = &self.shadow_register_buffer {
                encoder.set_buffer(1, Some(shadow_buffer), 0);
            }

            if let Some(signal_buffer) = &self.signal_buffer {
                encoder.set_buffer(2, Some(signal_buffer), 0);
            }

            // Buffer 3: Working register buffer (receives updates)
            if let Some(register_buffer) = &self.register_buffer {
                encoder.set_buffer(3, Some(register_buffer), 0);
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

            // No swap needed - working register buffer accumulates updates directly
            // The frozen snapshot remains unchanged for subsequent iterations
        }

        Ok(())
    }

    fn extract_state(&self) -> SimulationState {
        let mut signals = IndexMap::new();
        let mut registers = IndexMap::new();

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

        // Register clocks from the module's clock_domains (uses internal names via name registry)
        for mir_name in module.clock_domains.keys() {
            // Translate MIR name to internal SIR name using name registry
            let internal_name = module
                .name_registry
                .resolve(mir_name)
                .map(|s| s.to_string())
                .unwrap_or_else(|| mir_name.clone());
            // Default to 10ns period (100MHz)
            self.clock_manager.add_clock(internal_name, 10_000);
        }

        // Also identify clocks from sequential nodes' clock inputs (metadata-based)
        // This catches any clocks not in clock_domains (e.g., from flip-flop synthesis)
        for node in &module.sequential_nodes {
            if let skalp_sir::SirNodeKind::FlipFlop { .. } = &node.kind {
                if let Some(clock_input) = node.inputs.first() {
                    let clock_name = &clock_input.signal_id;
                    if !self.clock_manager.clocks.contains_key(clock_name) {
                        self.clock_manager.add_clock(clock_name.clone(), 10_000);
                    }
                }
            }
        }

        // Generate Metal shader from SIR
        let shader_source = MetalBackend::generate(module);

        // The Metal backend generates a single combined `combinational_cone_0` kernel
        // that evaluates all combinational logic in topological order.
        // We always use this single kernel regardless of how many cones exist.
        self.cached_cone_count = 1;

        // Compile the single combinational kernel
        let pipeline = self.compile_shader(&shader_source, "combinational_cone_0")?;
        self.pipelines
            .insert("combinational_0".to_string(), pipeline);

        // Compile sequential kernel if there are sequential nodes
        if !module.sequential_nodes.is_empty() {
            let pipeline = self.compile_shader(&shader_source, "sequential_update")?;
            self.pipelines.insert("sequential".to_string(), pipeline);
        }

        // PERF: Compile batched simulation kernel for multi-cycle execution
        match self.compile_shader(&shader_source, "batched_simulation") {
            Ok(pipeline) => {
                self.pipelines.insert("batched".to_string(), pipeline);
            }
            Err(_) => {
                // Fall back to step-by-step GPU execution
            }
        }

        // Find clock signal offset in input buffer
        // Look for the first clock signal (registered in clock_manager)
        let mut clock_offset = 0usize;
        for input in &module.inputs {
            if self.clock_manager.clocks.contains_key(&input.name) {
                self.clock_signal_offset = clock_offset;
                break;
            }
            // Advance offset by input width (in u32 words)
            clock_offset += (input.width + 31) / 32;
        }

        // Allocate GPU buffers
        self.allocate_buffers(module)?;

        // Allocate params buffer for batched kernel (2 u32: num_cycles, clock_offset)
        self.params_buffer = Some(
            self.device
                .device
                .new_buffer(8, MTLResourceOptions::StorageModeShared),
        );

        Ok(())
    }

    async fn step(&mut self) -> SimulationResult<SimulationState> {
        // BUG #182 FIX: SINGLE SEQUENTIAL UPDATE WITH POST-UPDATE COMBINATIONAL PASS
        //
        // For correct simulation semantics:
        // 1. Combinational computes signals using OLD register values
        // 2. Sequential updates ALL registers based on those signals (runs ONCE per edge)
        // 3. Final Combinational recomputes outputs using NEW register values
        //
        // The key insight is that state machines should only transition ONCE per clock
        // edge. Running sequential twice would cause the state machine to see its own
        // updated value and potentially transition again, which is incorrect.
        //
        // For FWFT (First Word Fall Through) semantics like FIFOs, we need the final
        // combinational pass to reflect the new register values in the outputs.

        // Create snapshot of current registers for the sequential kernel to read from
        // This ensures sequential reads consistent pre-edge values while writing new values
        self.create_register_snapshot();

        // === PHASE 1: Initial update pass ===
        // Combinational: compute signals from working registers (still have old values)
        self.execute_combinational().await?;

        // Sequential: state machines update based on old signals
        // (reads frozen snapshot, writes to working buffer)
        self.execute_sequential().await?;

        // === FINAL COMBINATIONAL: Output update pass ===
        // Recompute combinational outputs to reflect NEW register values
        // This provides FWFT (First Word Fall Through) semantics
        self.execute_combinational().await?;

        // Capture outputs after convergence
        self.capture_outputs()?;

        self.current_cycle += 1;

        // BUG #180 FIX: Update clock manager's previous_value to current value
        // This ensures that on the next step(), if the clock hasn't changed,
        // we won't incorrectly detect a rising edge again.
        // Without this fix, the clock_manager's previous_value only gets updated
        // when set_input() is called, so if the testbench doesn't call set_input()
        // between steps, we'd incorrectly detect edges on every step.
        self.update_clock_previous_values();

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
        // BUG FIX #117r: Update clock manager for clock signals
        // Check against registered clocks (not string matching) since internal names like "_s0" don't contain "clk"
        if self.clock_manager.clocks.contains_key(name) {
            let clock_value = value[0] != 0;
            self.clock_manager.set_clock(name, clock_value);
        }

        // Write to input buffer
        if let Some(module) = &self.module {
            if let Some(input_buffer) = &self.input_buffer {
                let input_ptr = input_buffer.contents() as *mut u8;
                let mut offset = 0usize;

                // Find the input and write to its location
                // BUG FIX #182: Account for Metal struct alignment padding
                // Metal naturally aligns struct fields: uint2 to 8-byte, uint4 to 16-byte
                for input in &module.inputs {
                    let metal_size = self.get_metal_type_size(input.width);
                    let metal_align = self.get_metal_type_alignment(input.width);

                    // Align offset to the required alignment boundary
                    let remainder = offset % metal_align;
                    if remainder != 0 {
                        offset += metal_align - remainder;
                    }

                    let bytes_needed = input.width.div_ceil(8);
                    if input.name == name {
                        // Auto-truncate or pad value to match expected size
                        // This allows users to pass u64 for smaller signals without explicit casting
                        let effective_value: Vec<u8> = if value.len() > bytes_needed {
                            // Truncate to needed bytes (take LSBs for little-endian)
                            value[..bytes_needed].to_vec()
                        } else if value.len() < bytes_needed {
                            // Pad with zeros
                            let mut padded = value.to_vec();
                            padded.resize(bytes_needed, 0);
                            padded
                        } else {
                            value.to_vec()
                        };
                        unsafe {
                            // Clear the Metal-sized buffer first
                            std::ptr::write_bytes(input_ptr.add(offset), 0, metal_size);
                            // Then copy our data
                            std::ptr::copy_nonoverlapping(
                                effective_value.as_ptr(),
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
            // BUG FIX: For state element outputs, skip output_buffer and read from register_buffer
            // State element outputs are NOT in the Signals struct (Metal shader skips them)
            // so output_buffer contains garbage for these outputs
            let is_state_element_output = module.state_elements.contains_key(name);

            // First try to read from output_buffer (only for non-state-element outputs)
            if !is_state_element_output {
                if let Some(output_buffer) = &self.output_buffer {
                    let output_ptr = output_buffer.contents() as *const u8;
                    let mut offset = 0usize;

                    // Find the output in the output buffer
                    // BUG FIX #182: Account for Metal struct alignment padding
                    for output in &module.outputs {
                        // Skip state element outputs in offset calculation
                        if module.state_elements.contains_key(&output.name) {
                            continue;
                        }

                        let metal_size = self.get_metal_type_size(output.width);
                        let metal_align = self.get_metal_type_alignment(output.width);

                        // Align offset to the required alignment boundary
                        let remainder = offset % metal_align;
                        if remainder != 0 {
                            offset += metal_align - remainder;
                        }

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
                            return Ok(value);
                        }
                        offset += metal_size;
                    }
                }
            }

            // Not found in outputs, try to read from register buffer
            // This allows reading internal state (registers/flip-flops) for debugging/testing
            if let Some(register_buffer) = &self.register_buffer {
                // Registers are sorted alphabetically in the Metal struct
                let mut sorted_states: Vec<_> = module.state_elements.iter().collect();
                sorted_states.sort_by_key(|(n, _)| *n);

                let register_ptr = register_buffer.contents() as *const u8;
                let mut offset = 0usize;

                for (reg_name, state_elem) in sorted_states.iter() {
                    let width = state_elem.width;
                    let metal_size = self.get_metal_type_size(width);
                    let metal_align = self.get_metal_type_alignment(width);

                    // Align offset to the required alignment boundary
                    let remainder = offset % metal_align;
                    if remainder != 0 {
                        offset += metal_align - remainder;
                    }

                    let bytes_needed = width.div_ceil(8);
                    if *reg_name == name {
                        let mut value = vec![0u8; bytes_needed];
                        unsafe {
                            std::ptr::copy_nonoverlapping(
                                register_ptr.add(offset),
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
            "Output/register {} not found",
            name
        )))
    }
}

// PERF: Batched simulation methods (not part of SimulationRuntime trait)
impl GpuRuntime {
    /// Run multiple cycles in a single GPU dispatch for massive speedup
    /// This bypasses per-cycle CPU<->GPU synchronization overhead
    pub async fn run_batched(&mut self, cycles: u64) -> SimulationResult<SimulationState> {
        if let Some(pipeline) = self.pipelines.get("batched") {
            // Set up params buffer with cycle count and clock offset
            if let Some(params_buffer) = &self.params_buffer {
                let params_ptr = params_buffer.contents() as *mut u32;
                unsafe {
                    *params_ptr = cycles as u32;
                    *params_ptr.add(1) = self.clock_signal_offset as u32;
                }
            }

            // Set clock high in input buffer (batched kernel handles toggling internally)
            if let Some(input_buffer) = &self.input_buffer {
                let input_ptr = input_buffer.contents() as *mut u32;
                unsafe {
                    *input_ptr.add(self.clock_signal_offset) = 1;
                }
            }

            // Single GPU dispatch for all cycles
            let command_buffer = self.device.command_queue.new_command_buffer();
            let encoder = command_buffer.new_compute_command_encoder();

            encoder.set_compute_pipeline_state(pipeline);

            // Set buffers
            if let Some(input_buffer) = &self.input_buffer {
                encoder.set_buffer(0, Some(input_buffer), 0);
            }
            if let Some(register_buffer) = &self.register_buffer {
                encoder.set_buffer(1, Some(register_buffer), 0);
            }
            if let Some(signal_buffer) = &self.signal_buffer {
                encoder.set_buffer(2, Some(signal_buffer), 0);
            }
            if let Some(params_buffer) = &self.params_buffer {
                encoder.set_buffer(3, Some(params_buffer), 0);
            }

            // Single thread does all the work
            let thread_groups = metal::MTLSize {
                width: 1,
                height: 1,
                depth: 1,
            };
            let threads_per_group = metal::MTLSize {
                width: 1,
                height: 1,
                depth: 1,
            };

            encoder.dispatch_thread_groups(thread_groups, threads_per_group);
            encoder.end_encoding();

            command_buffer.commit();
            command_buffer.wait_until_completed();

            // Update cycle counter
            self.current_cycle += cycles;

            // Capture outputs
            self.capture_outputs()?;
        } else {
            // No batched pipeline - fall back to step mode
            // This shouldn't happen in normal operation
        }

        Ok(self.extract_state())
    }
}
