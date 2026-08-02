//! Compiled CPU Runtime
//!
//! This module provides a high-performance CPU simulation runtime by compiling
//! the SIR module to native C++ code and loading it as a dynamic library.
//!
//! # Architecture
//!
//! ```text
//! SirModule → CppBackend → compile_cpp_kernel → .dylib/.so → dlopen → SimulationRuntime
//! ```
//!
//! # Performance
//!
//! The compiled runtime is significantly faster than the interpreted runtime because:
//! - All node evaluation is native code (no interpretation overhead)
//! - Memory layout matches the generated struct layout
//! - Batched simulation runs entirely in native code
//! - Compiler optimizations (O2) are applied

use crate::cpp_compiler::{compile_cpp_kernel, CompileError};
use crate::simulator::{SimulationError, SimulationResult, SimulationRuntime, SimulationState};
use async_trait::async_trait;
use indexmap::IndexMap;
use libloading::{Library, Symbol};
use skalp_sir::{CppBackend, SirModule, SirNodeKind, SirType};
use std::collections::HashMap;
use std::ffi::c_void;

/// Function pointer types for the compiled kernel
type CombinationalEvalFn = unsafe extern "C" fn(*const c_void, *const c_void, *mut c_void);
type SequentialUpdateFn =
    unsafe extern "C" fn(*const c_void, *const c_void, *const c_void, *mut c_void);
type BatchedSimulationFn = unsafe extern "C" fn(*mut c_void, *mut c_void, *mut c_void, u32);

/// A single field entry from the C++ offset table (must match C++ SkalpFieldEntry)
#[repr(C)]
struct SkalpFieldEntry {
    name: *const std::os::raw::c_char,
    offset: usize,
    size: usize,
}

/// The kernel export table structure (must match C++ layout)
#[repr(C)]
struct SkalpKernel {
    combinational_eval: CombinationalEvalFn,
    sequential_update: SequentialUpdateFn,
    batched_simulation: BatchedSimulationFn,
    inputs_size: usize,
    registers_size: usize,
    signals_size: usize,
    inputs_fields: *const SkalpFieldEntry,
    inputs_field_count: usize,
    registers_fields: *const SkalpFieldEntry,
    registers_field_count: usize,
    signals_fields: *const SkalpFieldEntry,
    signals_field_count: usize,
}

/// Compiled CPU runtime for high-performance simulation
pub struct CompiledCpuRuntime {
    /// Loaded library (must keep alive for function pointers to remain valid)
    _library: Library,
    /// Kernel function pointers
    kernel: *const SkalpKernel,
    /// Input buffer (aligned)
    inputs: AlignedBuffer,
    /// Register buffer (aligned)
    registers: AlignedBuffer,
    /// Signal buffer (aligned)
    signals: AlignedBuffer,
    /// Shadow register buffer for double-buffering
    shadow_registers: AlignedBuffer,
    /// Field offset maps for reading/writing values
    input_fields: FieldMap,
    register_fields: FieldMap,
    signal_fields: FieldMap,
    /// Output port mappings (output name -> signal field name)
    output_mappings: HashMap<String, String>,
    /// Current simulation cycle
    current_cycle: u64,
    /// Reference to the SIR module for metadata
    module: SirModule,
    /// Clock signal names for edge detection (all clock domains)
    clock_names: Vec<String>,
    /// Previous clock values for edge detection
    prev_clocks: Vec<u64>,
}

/// A mapping from field names to their offset and size in the buffer
#[derive(Debug, Default)]
struct FieldMap {
    fields: HashMap<String, FieldInfo>,
}

#[derive(Debug, Clone)]
struct FieldInfo {
    offset: usize,
    size: usize,
    width_bits: usize,
}

impl FieldMap {
    fn new() -> Self {
        Self {
            fields: HashMap::new(),
        }
    }

    fn add(&mut self, name: String, offset: usize, size: usize, width_bits: usize) {
        self.fields.insert(
            name,
            FieldInfo {
                offset,
                size,
                width_bits,
            },
        );
    }

    fn get(&self, name: &str) -> Option<&FieldInfo> {
        self.fields.get(name)
    }
}

/// Aligned memory buffer for struct data
struct AlignedBuffer {
    data: Vec<u8>,
}

impl AlignedBuffer {
    fn new(size: usize) -> Self {
        // Align to 16 bytes for SIMD compatibility
        let aligned_size = (size + 15) & !15;
        Self {
            data: vec![0u8; aligned_size.max(16)],
        }
    }

    fn as_ptr(&self) -> *const c_void {
        self.data.as_ptr() as *const c_void
    }

    fn as_mut_ptr(&mut self) -> *mut c_void {
        self.data.as_mut_ptr() as *mut c_void
    }

    fn read(&self, offset: usize, size: usize) -> &[u8] {
        &self.data[offset..offset + size]
    }

    fn write(&mut self, offset: usize, data: &[u8]) {
        let end = offset + data.len();
        if end <= self.data.len() {
            self.data[offset..end].copy_from_slice(data);
        }
    }

    fn clear(&mut self) {
        self.data.fill(0);
    }

    fn copy_from(&mut self, other: &AlignedBuffer) {
        let len = self.data.len().min(other.data.len());
        self.data[..len].copy_from_slice(&other.data[..len]);
    }
}

impl Clone for AlignedBuffer {
    fn clone(&self) -> Self {
        Self {
            data: self.data.clone(),
        }
    }
}

// Safety: The buffer contains only raw bytes and is not shared across threads
unsafe impl Send for CompiledCpuRuntime {}
unsafe impl Sync for CompiledCpuRuntime {}

impl CompiledCpuRuntime {
    /// Create a new compiled CPU runtime for the given SIR module
    pub fn new(module: &SirModule) -> Result<Self, CompileError> {
        // Generate C++ source code
        let source = CppBackend::generate(module);

        // Debug: dump C++ if env var set
        if std::env::var("SKALP_DUMP_CPP").is_ok() {
            let dump_path = std::env::var("SKALP_DUMP_CPP")
                .unwrap_or_else(|_| "/tmp/skalp_generated.cpp".to_string());
            std::fs::write(&dump_path, &source).ok();
            eprintln!("[SKALP] Dumped C++ to {}", dump_path);
        }

        // Compile to dynamic library
        let lib_path = compile_cpp_kernel(&source)?;

        // Load library
        let library = unsafe { Library::new(&lib_path) }.map_err(|e| {
            CompileError::CompilationFailed(format!("Failed to load library: {}", e))
        })?;

        // Get kernel export table
        let kernel: *const SkalpKernel = unsafe {
            let sym: Symbol<*const SkalpKernel> = library.get(b"SKALP_KERNEL").map_err(|e| {
                CompileError::CompilationFailed(format!("Failed to find SKALP_KERNEL: {}", e))
            })?;
            *sym
        };

        // Get buffer sizes
        let (inputs_size, registers_size, signals_size) = unsafe {
            (
                (*kernel).inputs_size,
                (*kernel).registers_size,
                (*kernel).signals_size,
            )
        };

        // Create buffers
        let inputs = AlignedBuffer::new(inputs_size);
        let registers = AlignedBuffer::new(registers_size);
        let signals = AlignedBuffer::new(signals_size);
        let shadow_registers = AlignedBuffer::new(registers_size);

        // Build field maps from the C++ offset tables (authoritative layout)
        let (input_fields, register_fields, signal_fields, output_mappings) =
            unsafe { Self::build_field_maps(module, kernel) };

        // Collect ALL clock signal names for multi-clock edge detection
        // (matching the GPU runtime pattern in gpu_runtime.rs:652-675)
        let mut clock_names: Vec<String> = module.clock_domains.keys().cloned().collect();

        // Also check sequential nodes for clocks not in clock_domains
        for node in &module.sequential_nodes {
            if let SirNodeKind::FlipFlop { .. } = &node.kind {
                if let Some(clock_input) = node.inputs.first() {
                    if !clock_names.contains(&clock_input.signal_id) {
                        clock_names.push(clock_input.signal_id.clone());
                    }
                }
            }
        }

        // Fallback for simple designs with no explicit clock domains.
        // Only apply if there are sequential elements — purely combinational circuits
        // should keep clock_names empty so eval_sequential always runs.
        if clock_names.is_empty() && !module.sequential_nodes.is_empty() {
            // Try name_registry lookup for inputs containing clk/clock
            if let Some(clk) = module.inputs.iter().find(|input| {
                if let Some(entry) = module.name_registry.get_entry_by_internal(&input.name) {
                    let path = &entry.hierarchical_path;
                    path.contains("clk") || path.contains("clock")
                } else {
                    false
                }
            }) {
                clock_names.push(clk.name.clone());
            } else if let Some(clk) = module.inputs.iter().find(|p| p.sir_type.width() == 1) {
                // Last resort: first input with width 1 (often clock in simple designs)
                clock_names.push(clk.name.clone());
            }
        }

        let prev_clocks = vec![0u64; clock_names.len()];

        Ok(Self {
            _library: library,
            kernel,
            inputs,
            registers,
            signals,
            shadow_registers,
            input_fields,
            register_fields,
            signal_fields,
            output_mappings,
            current_cycle: 0,
            module: module.clone(),
            clock_names,
            prev_clocks,
        })
    }

    /// Build field offset maps by reading the C++ offset tables emitted via offsetof()/sizeof().
    /// This is the authoritative source of struct layout — no runtime offset computation needed.
    unsafe fn build_field_maps(
        module: &SirModule,
        kernel: *const SkalpKernel,
    ) -> (FieldMap, FieldMap, FieldMap, HashMap<String, String>) {
        let mut input_fields = FieldMap::new();
        let mut register_fields = FieldMap::new();
        let mut signal_fields = FieldMap::new();
        let mut output_mappings = HashMap::new();

        let k = &*kernel;

        // Read input field table
        let input_entries = std::slice::from_raw_parts(k.inputs_fields, k.inputs_field_count);
        for entry in input_entries {
            let name = std::ffi::CStr::from_ptr(entry.name)
                .to_str()
                .unwrap_or("")
                .to_string();
            // width_bits is not critical for correctness — use size*8 as approximation
            input_fields.add(name, entry.offset, entry.size, entry.size * 8);
        }

        // Read register field table
        let register_entries =
            std::slice::from_raw_parts(k.registers_fields, k.registers_field_count);
        for entry in register_entries {
            let name = std::ffi::CStr::from_ptr(entry.name)
                .to_str()
                .unwrap_or("")
                .to_string();
            register_fields.add(name, entry.offset, entry.size, entry.size * 8);
        }

        // Read signal field table
        let signal_entries = std::slice::from_raw_parts(k.signals_fields, k.signals_field_count);
        for entry in signal_entries {
            let name = std::ffi::CStr::from_ptr(entry.name)
                .to_str()
                .unwrap_or("")
                .to_string();
            signal_fields.add(name.clone(), entry.offset, entry.size, entry.size * 8);
        }

        // Build output mappings (output name -> sanitized signal field name)
        for output in &module.outputs {
            if module.state_elements.contains_key(&output.name) {
                continue;
            }
            let sanitized = output.name.replace('.', "_");
            if signal_fields.get(&sanitized).is_some() {
                output_mappings.insert(output.name.clone(), sanitized);
            }
        }

        (
            input_fields,
            register_fields,
            signal_fields,
            output_mappings,
        )
    }

    /// Get byte size for a bit width (scalar types only)
    fn type_byte_size(width: usize) -> usize {
        match width {
            0 => 4,
            1..=32 => 4,
            33..=64 => 8,
            _ => width.div_ceil(32) * 4,
        }
    }

    /// Get byte size for a SIR type, correctly handling arrays.
    ///
    /// The C++ codegen aligns each array element to its own uint32_t/uint64_t
    /// slot (e.g., `[nat[28]; 4096]` becomes `uint32_t[4096]` = 16384 bytes).
    /// This function matches that layout so runtime field offsets are correct.
    fn sir_type_byte_size(sir_type: &SirType) -> usize {
        match sir_type {
            SirType::Array(elem_type, count) => Self::sir_type_byte_size(elem_type) * count,
            _ => Self::type_byte_size(sir_type.width()),
        }
    }

    /// Run combinational evaluation
    pub(crate) fn eval_combinational(&mut self) {
        unsafe {
            let kernel = &*self.kernel;
            (kernel.combinational_eval)(
                self.inputs.as_ptr(),
                self.registers.as_ptr(),
                self.signals.as_mut_ptr(),
            );
        }
    }

    /// Run sequential update
    pub(crate) fn eval_sequential(&mut self) {
        unsafe {
            let kernel = &*self.kernel;
            // Copy current registers to shadow
            self.shadow_registers.copy_from(&self.registers);
            // Update: shadow -> registers
            (kernel.sequential_update)(
                self.inputs.as_ptr(),
                self.shadow_registers.as_ptr(),
                self.signals.as_ptr(),
                self.registers.as_mut_ptr(),
            );
        }
    }

    /// Clear all registers to zero
    pub(crate) fn reset_registers(&mut self) {
        self.registers.clear();
        self.shadow_registers.clear();
    }

    /// Set an input value (synchronous, non-async wrapper)
    pub(crate) fn set_input_sync(
        &mut self,
        name: &str,
        value: &[u8],
    ) -> Result<(), crate::simulator::SimulationError> {
        let sanitized = name.replace('.', "_");
        if let Some(info) = self.input_fields.get(&sanitized) {
            let mut padded = vec![0u8; info.size];
            let copy_len = value.len().min(info.size);
            padded[..copy_len].copy_from_slice(&value[..copy_len]);
            self.inputs.write(info.offset, &padded);
            Ok(())
        } else {
            Err(crate::simulator::SimulationError::InvalidInput(
                name.to_string(),
            ))
        }
    }

    /// Get an output value (synchronous, non-async wrapper)
    pub(crate) fn get_output_sync(
        &self,
        name: &str,
    ) -> Result<Vec<u8>, crate::simulator::SimulationError> {
        let sanitized = name.replace('.', "_");

        // Check mapped outputs
        if let Some(signal_name) = self.output_mappings.get(name) {
            if let Some(info) = self.signal_fields.get(signal_name) {
                return Ok(self.signals.read(info.offset, info.size).to_vec());
            }
        }

        // Try direct signal lookup
        if let Some(info) = self.signal_fields.get(&sanitized) {
            return Ok(self.signals.read(info.offset, info.size).to_vec());
        }

        // Try register lookup
        if let Some(info) = self.register_fields.get(&sanitized) {
            return Ok(self.registers.read(info.offset, info.size).to_vec());
        }

        Err(crate::simulator::SimulationError::InvalidInput(
            name.to_string(),
        ))
    }

    /// Get user-facing signal and register names for waveform capture.
    /// Returns (sanitized_field_name, display_name) pairs.
    /// Uses the name_registry to reverse-resolve internal names to user-facing paths.
    /// Signals without a user-facing name are skipped.
    pub fn get_waveform_signals(&self) -> Vec<(String, String, usize)> {
        let mut result: Vec<(String, String, usize)> = Vec::new();
        let mut seen = std::collections::HashSet::new();

        // Input ports — use name_registry to get user-facing names
        for input in &self.module.inputs {
            let sanitized = input.name.replace('.', "_");
            if self.input_fields.get(&sanitized).is_some() {
                let display = self
                    .module
                    .name_registry
                    .reverse_resolve(&input.name)
                    .unwrap_or(&input.name)
                    .to_string();
                if seen.insert(display.clone()) {
                    result.push((sanitized, display, input.width));
                }
            }
        }

        // Output ports
        for output in &self.module.outputs {
            let sanitized = output.name.replace('.', "_");
            if self.signal_fields.get(&sanitized).is_some() {
                let display = self
                    .module
                    .name_registry
                    .reverse_resolve(&output.name)
                    .unwrap_or(&output.name)
                    .to_string();
                if seen.insert(display.clone()) {
                    result.push((sanitized, display, output.width));
                }
            }
        }

        // State elements (registers)
        for (name, elem) in &self.module.state_elements {
            let sanitized = name.replace('.', "_");
            if self.register_fields.get(&sanitized).is_some() {
                let display = self
                    .module
                    .name_registry
                    .reverse_resolve(name)
                    .map(|s| s.to_string())
                    .unwrap_or_else(|| name.clone());
                // Skip internal names that didn't resolve
                if display.starts_with("_s") || display.starts_with("_t") {
                    continue;
                }
                if seen.insert(display.clone()) {
                    result.push((sanitized, display, elem.width));
                }
            }
        }

        // Other signals — only include those with user-facing names
        for signal in &self.module.signals {
            let sanitized = signal.name.replace('.', "_");
            // Try signal_fields first, then register_fields
            let in_fields = self.signal_fields.get(&sanitized).is_some()
                || self.register_fields.get(&sanitized).is_some();
            if !in_fields {
                continue;
            }

            if let Some(display) = self.module.name_registry.reverse_resolve(&signal.name) {
                let display = display.to_string();
                if seen.insert(display.clone()) {
                    result.push((sanitized, display, signal.width));
                }
            }
        }

        result
    }

    /// Extract current state as SimulationState
    fn extract_state(&self) -> SimulationState {
        let mut signals = IndexMap::new();
        let mut registers = IndexMap::new();

        // Extract signals
        for (name, info) in &self.signal_fields.fields {
            let value = self.signals.read(info.offset, info.size).to_vec();
            signals.insert(name.clone(), value);
        }

        // Extract registers
        for (name, info) in &self.register_fields.fields {
            let value = self.registers.read(info.offset, info.size).to_vec();
            registers.insert(name.clone(), value);
        }

        SimulationState {
            cycle: self.current_cycle,
            signals,
            registers,
        }
    }
}

#[async_trait]
impl SimulationRuntime for CompiledCpuRuntime {
    async fn initialize(&mut self, _module: &SirModule) -> SimulationResult<()> {
        // Already initialized in constructor
        self.inputs.clear();
        self.registers.clear();
        self.signals.clear();
        self.current_cycle = 0;
        self.prev_clocks.iter_mut().for_each(|c| *c = 0);
        Ok(())
    }

    async fn step(&mut self) -> SimulationResult<SimulationState> {
        // Phase 1: Combinational evaluation
        self.eval_combinational();

        // Phase 2: Sequential update - only on rising edge of any clock
        let mut has_rising_edge = false;
        for (i, clock_name) in self.clock_names.iter().enumerate() {
            let sanitized = clock_name.replace('.', "_");
            let curr = if let Some(info) = self.input_fields.get(&sanitized) {
                let bytes = self.inputs.read(info.offset, info.size);
                if !bytes.is_empty() {
                    bytes[0] as u64 & 1
                } else {
                    0
                }
            } else {
                0
            };
            if self.prev_clocks[i] == 0 && curr == 1 {
                has_rising_edge = true;
            }
            self.prev_clocks[i] = curr;
        }

        if has_rising_edge || self.clock_names.is_empty() {
            self.eval_sequential();
        }

        // Phase 3: Combinational evaluation again (to propagate new register values)
        self.eval_combinational();

        self.current_cycle += 1;
        Ok(self.extract_state())
    }

    async fn run(&mut self, cycles: u64) -> SimulationResult<Vec<SimulationState>> {
        let mut states = Vec::with_capacity(cycles as usize);

        // Use batched simulation for efficiency if more than a few cycles
        if cycles > 10 {
            unsafe {
                let kernel = &*self.kernel;
                (kernel.batched_simulation)(
                    self.inputs.as_mut_ptr(),
                    self.registers.as_mut_ptr(),
                    self.signals.as_mut_ptr(),
                    cycles as u32,
                );
            }
            self.current_cycle += cycles;
            states.push(self.extract_state());
        } else {
            for _ in 0..cycles {
                states.push(self.step().await?);
            }
        }

        Ok(states)
    }

    async fn reset(&mut self) -> SimulationResult<()> {
        self.registers.clear();
        self.signals.clear();
        self.current_cycle = 0;
        self.prev_clocks.iter_mut().for_each(|c| *c = 0);

        // Run one combinational eval to propagate reset values
        self.eval_combinational();

        Ok(())
    }

    async fn set_input(&mut self, name: &str, value: &[u8]) -> SimulationResult<()> {
        let sanitized = name.replace('.', "_");
        if let Some(info) = self.input_fields.get(&sanitized) {
            // Truncate or zero-extend value to match expected size
            let mut padded = vec![0u8; info.size];
            let copy_len = value.len().min(info.size);
            padded[..copy_len].copy_from_slice(&value[..copy_len]);
            self.inputs.write(info.offset, &padded);
            Ok(())
        } else {
            Err(SimulationError::InvalidInput(name.to_string()))
        }
    }

    async fn get_output(&self, name: &str) -> SimulationResult<Vec<u8>> {
        let sanitized = name.replace('.', "_");

        // Check if it's a mapped output
        if let Some(signal_name) = self.output_mappings.get(name) {
            if let Some(info) = self.signal_fields.get(signal_name) {
                return Ok(self.signals.read(info.offset, info.size).to_vec());
            }
        }

        // Try direct signal lookup
        if let Some(info) = self.signal_fields.get(&sanitized) {
            return Ok(self.signals.read(info.offset, info.size).to_vec());
        }

        // Try register lookup (for state element outputs)
        if let Some(info) = self.register_fields.get(&sanitized) {
            return Ok(self.registers.read(info.offset, info.size).to_vec());
        }

        Err(SimulationError::InvalidInput(name.to_string()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_aligned_buffer() {
        let mut buf = AlignedBuffer::new(100);
        assert!(buf.data.len() >= 100);

        buf.write(0, &[1, 2, 3, 4]);
        assert_eq!(buf.read(0, 4), &[1, 2, 3, 4]);

        buf.clear();
        assert_eq!(buf.read(0, 4), &[0, 0, 0, 0]);
    }

    #[test]
    fn test_field_map() {
        let mut map = FieldMap::new();
        map.add("counter".to_string(), 0, 4, 32);
        map.add("data".to_string(), 4, 8, 64);

        assert!(map.get("counter").is_some());
        assert!(map.get("data").is_some());
        assert!(map.get("missing").is_none());
    }

    #[test]
    fn test_type_byte_size() {
        assert_eq!(CompiledCpuRuntime::type_byte_size(1), 4);
        assert_eq!(CompiledCpuRuntime::type_byte_size(32), 4);
        assert_eq!(CompiledCpuRuntime::type_byte_size(33), 8);
        assert_eq!(CompiledCpuRuntime::type_byte_size(64), 8);
        assert_eq!(CompiledCpuRuntime::type_byte_size(65), 12);
        assert_eq!(CompiledCpuRuntime::type_byte_size(128), 16);
    }
}
