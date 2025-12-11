//! Gate-level runtime providing same API as behavioral Simulator
//!
//! This module provides a `GateLevelRuntime` that wraps either `GateLevelSimulator` (CPU)
//! or `GpuGateRuntime` (GPU) and provides the same high-level API (set_input, get_output, step)
//! as the behavioral Simulator, allowing transparent switching between simulation modes.
//!
//! # Usage
//!
//! For gate-level simulation, the flow is:
//! 1. Parse and compile to MIR (same as behavioral)
//! 2. Convert MIR to LIR using `lower_mir_module_to_lir`
//! 3. Create `GateLevelRuntime::from_lir(&lir)`
//! 4. Use same API: `set_input()`, `step()`, `get_output()`
//!
//! # GPU Acceleration
//!
//! On macOS, the runtime automatically tries to use GPU acceleration. Use
//! `from_lir_with_mode()` to explicitly select CPU or GPU.
//!
//! # Example
//! ```ignore
//! // Convert MIR to LIR
//! let result = lower_mir_module_to_lir(&mir_module);
//! let lir = result.lir;
//!
//! // Create gate-level runtime (auto-selects GPU if available)
//! let mut sim = GateLevelRuntime::from_lir(&lir)?;
//!
//! // Same API as behavioral simulator
//! sim.set_input("clk", &[1])?;
//! sim.step()?;
//! let output = sim.get_output("result")?;
//! ```

use crate::gate_simulator::GateLevelSimulator;
use crate::lir_to_sir::convert_lir_to_sir;
use crate::simulator::{SimulationError, SimulationResult, SimulationState};
use crate::sir::{Sir, SirSignalType, SirPortDirection};
use skalp_lir::lir::Lir;
use std::collections::HashMap;

#[cfg(target_os = "macos")]
use crate::gpu_gate_runtime::GpuGateRuntime;

/// Simulation backend mode
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SimulationMode {
    /// CPU-based simulation
    Cpu,
    /// GPU-accelerated simulation (Metal on macOS)
    Gpu,
    /// Auto-select (prefer GPU if available)
    Auto,
}

impl Default for SimulationMode {
    fn default() -> Self {
        SimulationMode::Auto
    }
}

/// Backend implementation selector
enum Backend {
    Cpu(GateLevelSimulator),
    #[cfg(target_os = "macos")]
    Gpu(GpuGateRuntime),
}

/// Gate-level runtime providing same API as behavioral Simulator
///
/// This allows using gate-level simulation with the same interface as
/// behavioral simulation, enabling transparent switching between modes.
///
/// Supports both CPU and GPU (on macOS) backends with automatic selection.
pub struct GateLevelRuntime {
    /// The underlying backend
    backend: Backend,
    /// Input port names for validation
    input_names: Vec<String>,
    /// Output port names for validation
    output_names: Vec<String>,
    /// Current cycle count
    current_cycle: u64,
    /// Whether GPU is being used
    using_gpu: bool,
}

impl GateLevelRuntime {
    /// Create a gate-level runtime from a pre-built Lir
    ///
    /// This is the primary entry point for gate-level simulation.
    /// Automatically uses GPU if available on macOS.
    pub fn from_lir(netlist: &Lir) -> SimulationResult<Self> {
        Self::from_lir_with_mode(netlist, SimulationMode::Auto)
    }

    /// Create a gate-level runtime with explicit mode selection
    pub fn from_lir_with_mode(
        netlist: &Lir,
        mode: SimulationMode,
    ) -> SimulationResult<Self> {
        // Convert Lir to SIR
        let sir_result = convert_lir_to_sir(netlist);
        let sir = sir_result.sir;

        Self::from_sir_with_mode(&sir, mode)
    }

    /// Create from a pre-built Sir (for advanced use cases)
    pub fn from_sir(sir: &Sir) -> Self {
        Self::from_sir_with_mode(sir, SimulationMode::Auto)
            .expect("CPU backend should always succeed")
    }

    /// Create from Sir with explicit mode selection
    pub fn from_sir_with_mode(sir: &Sir, mode: SimulationMode) -> SimulationResult<Self> {
        // Extract port names from SIR
        let mut input_names = Vec::new();
        let mut output_names = Vec::new();

        for signal in &sir.top_module.signals {
            if let SirSignalType::Port { direction } = &signal.signal_type {
                match direction {
                    SirPortDirection::Input => {
                        input_names.push(signal.name.clone());
                    }
                    SirPortDirection::Output => {
                        output_names.push(signal.name.clone());
                    }
                    SirPortDirection::InOut => {
                        // InOut ports are both input and output
                        input_names.push(signal.name.clone());
                        output_names.push(signal.name.clone());
                    }
                }
            }
        }

        // Select backend based on mode
        let (backend, using_gpu) = match mode {
            SimulationMode::Cpu => {
                (Backend::Cpu(GateLevelSimulator::new(sir)), false)
            }
            #[cfg(target_os = "macos")]
            SimulationMode::Gpu => {
                match GpuGateRuntime::new(sir) {
                    Ok(gpu_rt) => {
                        let is_using = gpu_rt.is_using_gpu();
                        (Backend::Gpu(gpu_rt), is_using)
                    }
                    Err(e) => {
                        return Err(SimulationError::GpuError(e));
                    }
                }
            }
            #[cfg(not(target_os = "macos"))]
            SimulationMode::Gpu => {
                return Err(SimulationError::GpuError(
                    "GPU simulation requires macOS".to_string(),
                ));
            }
            #[cfg(target_os = "macos")]
            SimulationMode::Auto => {
                // Try GPU first, fall back to CPU
                match GpuGateRuntime::new(sir) {
                    Ok(gpu_rt) => {
                        let is_using = gpu_rt.is_using_gpu();
                        (Backend::Gpu(gpu_rt), is_using)
                    }
                    Err(_) => {
                        (Backend::Cpu(GateLevelSimulator::new(sir)), false)
                    }
                }
            }
            #[cfg(not(target_os = "macos"))]
            SimulationMode::Auto => {
                (Backend::Cpu(GateLevelSimulator::new(sir)), false)
            }
        };

        Ok(GateLevelRuntime {
            backend,
            input_names,
            output_names,
            current_cycle: 0,
            using_gpu,
        })
    }

    /// Check if GPU acceleration is being used
    pub fn is_using_gpu(&self) -> bool {
        self.using_gpu
    }

    /// Get device info string
    pub fn device_info(&self) -> String {
        match &self.backend {
            Backend::Cpu(_) => "CPU backend".to_string(),
            #[cfg(target_os = "macos")]
            Backend::Gpu(gpu_rt) => gpu_rt.device_info(),
        }
    }

    /// Set an input port value (bytes, LSB first)
    pub fn set_input(&mut self, name: &str, value: &[u8]) -> SimulationResult<()> {
        match &mut self.backend {
            Backend::Cpu(sim) => {
                // Get the width for this signal
                let width = sim.get_signal(name).map(|b| b.len()).unwrap_or_else(|| {
                    // Try to find bit-indexed signals
                    let mut w = 0;
                    while sim.get_signal(&format!("{}[{}]", name, w)).is_some() {
                        w += 1;
                    }
                    if w > 0 { w } else { value.len() * 8 }
                });

                // Convert bytes to bits
                let bits = Self::bytes_to_bits(value, width);
                sim.set_input(name, &bits);
            }
            #[cfg(target_os = "macos")]
            Backend::Gpu(gpu_rt) => {
                // GPU backend handles conversion internally
                let bits = Self::bytes_to_bits(value, value.len() * 8);
                gpu_rt.set_input(name, &bits);
            }
        }
        Ok(())
    }

    /// Set an input port value as u64
    pub fn set_input_u64(&mut self, name: &str, value: u64) -> SimulationResult<()> {
        match &mut self.backend {
            Backend::Cpu(sim) => {
                sim.set_input_u64(name, value);
            }
            #[cfg(target_os = "macos")]
            Backend::Gpu(gpu_rt) => {
                gpu_rt.set_input_u64(name, value);
            }
        }
        Ok(())
    }

    /// Step simulation by one cycle
    pub fn step(&mut self) -> SimulationResult<SimulationState> {
        match &mut self.backend {
            Backend::Cpu(sim) => {
                sim.step();
            }
            #[cfg(target_os = "macos")]
            Backend::Gpu(gpu_rt) => {
                gpu_rt.step();
            }
        }
        self.current_cycle += 1;
        Ok(self.extract_state())
    }

    /// Run simulation for multiple cycles
    pub fn run(&mut self, cycles: u64) -> SimulationResult<Vec<SimulationState>> {
        let mut states = Vec::with_capacity(cycles as usize);
        for _ in 0..cycles {
            states.push(self.step()?);
        }
        Ok(states)
    }

    /// Reset simulation state
    pub fn reset(&mut self) -> SimulationResult<()> {
        match &mut self.backend {
            Backend::Cpu(sim) => {
                sim.reset();
            }
            #[cfg(target_os = "macos")]
            Backend::Gpu(gpu_rt) => {
                gpu_rt.reset();
            }
        }
        self.current_cycle = 0;
        Ok(())
    }

    /// Get an output port value (bytes, LSB first)
    pub fn get_output(&self, name: &str) -> SimulationResult<Vec<u8>> {
        match &self.backend {
            Backend::Cpu(sim) => {
                // Try direct lookup first
                if let Some(bits) = sim.get_output(name) {
                    return Ok(Self::bits_to_bytes(&bits));
                }

                // Try u64 lookup (handles bit-indexed signals)
                if let Some(value) = sim.get_output_u64(name) {
                    // Determine byte count based on actual signal width
                    let mut byte_count = 8; // Default to 64 bits
                    for i in 0..64 {
                        if sim.get_signal(&format!("{}[{}]", name, i)).is_none() {
                            byte_count = (i + 7) / 8;
                            if byte_count == 0 {
                                byte_count = 1;
                            }
                            break;
                        }
                    }
                    let mut bytes = vec![0u8; byte_count];
                    for (i, byte) in bytes.iter_mut().enumerate() {
                        *byte = ((value >> (i * 8)) & 0xFF) as u8;
                    }
                    return Ok(bytes);
                }

                Err(SimulationError::InvalidInput(format!(
                    "Output '{}' not found",
                    name
                )))
            }
            #[cfg(target_os = "macos")]
            Backend::Gpu(gpu_rt) => {
                if let Some(bits) = gpu_rt.get_output(name) {
                    return Ok(Self::bits_to_bytes(&bits));
                }
                if let Some(value) = gpu_rt.get_output_u64(name) {
                    let mut bytes = vec![0u8; 8];
                    for (i, byte) in bytes.iter_mut().enumerate() {
                        *byte = ((value >> (i * 8)) & 0xFF) as u8;
                    }
                    return Ok(bytes);
                }
                Err(SimulationError::InvalidInput(format!(
                    "Output '{}' not found",
                    name
                )))
            }
        }
    }

    /// Get an output port value as u64
    pub fn get_output_u64(&self, name: &str) -> SimulationResult<u64> {
        match &self.backend {
            Backend::Cpu(sim) => {
                sim.get_output_u64(name).ok_or_else(|| {
                    SimulationError::InvalidInput(format!("Output '{}' not found", name))
                })
            }
            #[cfg(target_os = "macos")]
            Backend::Gpu(gpu_rt) => {
                gpu_rt.get_output_u64(name).ok_or_else(|| {
                    SimulationError::InvalidInput(format!("Output '{}' not found", name))
                })
            }
        }
    }

    /// Get any signal value (for debugging)
    pub fn get_signal(&self, name: &str) -> Option<Vec<bool>> {
        match &self.backend {
            Backend::Cpu(sim) => sim.get_signal(name),
            #[cfg(target_os = "macos")]
            Backend::Gpu(gpu_rt) => gpu_rt.get_signal(name),
        }
    }

    /// Get current cycle count
    pub fn cycle(&self) -> u64 {
        self.current_cycle
    }

    /// Get list of input port names
    pub fn input_names(&self) -> &[String] {
        &self.input_names
    }

    /// Get list of output port names
    pub fn output_names(&self) -> &[String] {
        &self.output_names
    }

    /// Dump all signals (for debugging)
    pub fn dump_signals(&self) -> Vec<(String, Vec<bool>)> {
        match &self.backend {
            Backend::Cpu(sim) => sim.dump_signals(),
            #[cfg(target_os = "macos")]
            Backend::Gpu(gpu_rt) => gpu_rt.dump_signals(),
        }
    }

    /// Convert Vec<u8> (bytes, LSB first) to Vec<bool> (bits, LSB first)
    fn bytes_to_bits(bytes: &[u8], width: usize) -> Vec<bool> {
        let mut bits = Vec::with_capacity(width);
        for i in 0..width {
            let byte_idx = i / 8;
            let bit_idx = i % 8;
            let bit = if byte_idx < bytes.len() {
                (bytes[byte_idx] >> bit_idx) & 1 == 1
            } else {
                false
            };
            bits.push(bit);
        }
        bits
    }

    /// Convert Vec<bool> (bits, LSB first) to Vec<u8> (bytes, LSB first)
    fn bits_to_bytes(bits: &[bool]) -> Vec<u8> {
        let byte_count = bits.len().div_ceil(8);
        let mut bytes = vec![0u8; byte_count];
        for (i, &bit) in bits.iter().enumerate() {
            if bit {
                let byte_idx = i / 8;
                let bit_idx = i % 8;
                bytes[byte_idx] |= 1 << bit_idx;
            }
        }
        bytes
    }

    /// Extract current state as SimulationState
    fn extract_state(&self) -> SimulationState {
        let mut signals = HashMap::new();
        let registers = HashMap::new();

        // Get all output signal values
        for name in &self.output_names {
            match &self.backend {
                Backend::Cpu(sim) => {
                    if let Some(bits) = sim.get_signal(name) {
                        signals.insert(name.clone(), Self::bits_to_bytes(&bits));
                    } else if let Some(value) = sim.get_output_u64(name) {
                        // Handle bit-indexed signals
                        let byte_count = 8; // Assume 64-bit max
                        let mut bytes = vec![0u8; byte_count];
                        for (i, byte) in bytes.iter_mut().enumerate() {
                            *byte = ((value >> (i * 8)) & 0xFF) as u8;
                        }
                        signals.insert(name.clone(), bytes);
                    }
                }
                #[cfg(target_os = "macos")]
                Backend::Gpu(gpu_rt) => {
                    if let Some(bits) = gpu_rt.get_output(name) {
                        signals.insert(name.clone(), Self::bits_to_bytes(&bits));
                    } else if let Some(value) = gpu_rt.get_output_u64(name) {
                        let mut bytes = vec![0u8; 8];
                        for (i, byte) in bytes.iter_mut().enumerate() {
                            *byte = ((value >> (i * 8)) & 0xFF) as u8;
                        }
                        signals.insert(name.clone(), bytes);
                    }
                }
            }
        }

        // Also include inputs in state
        for name in &self.input_names {
            match &self.backend {
                Backend::Cpu(sim) => {
                    if let Some(bits) = sim.get_signal(name) {
                        signals.insert(name.clone(), Self::bits_to_bytes(&bits));
                    }
                }
                #[cfg(target_os = "macos")]
                Backend::Gpu(gpu_rt) => {
                    if let Some(bits) = gpu_rt.get_signal(name) {
                        signals.insert(name.clone(), Self::bits_to_bytes(&bits));
                    }
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_bytes_to_bits() {
        // Single byte
        let bytes = vec![0b10101010];
        let bits = GateLevelRuntime::bytes_to_bits(&bytes, 8);
        assert_eq!(
            bits,
            vec![false, true, false, true, false, true, false, true]
        );

        // Two bytes, 12 bits
        let bytes = vec![0xFF, 0x0F];
        let bits = GateLevelRuntime::bytes_to_bits(&bytes, 12);
        assert_eq!(
            bits,
            vec![true, true, true, true, true, true, true, true, true, true, true, true]
        );
    }

    #[test]
    fn test_bits_to_bytes() {
        // 8 bits
        let bits = vec![false, true, false, true, false, true, false, true];
        let bytes = GateLevelRuntime::bits_to_bytes(&bits);
        assert_eq!(bytes, vec![0b10101010]);

        // 12 bits -> 2 bytes
        let bits = vec![
            true, true, true, true, true, true, true, true, true, true, true, true,
        ];
        let bytes = GateLevelRuntime::bits_to_bytes(&bits);
        assert_eq!(bytes, vec![0xFF, 0x0F]);
    }

    #[test]
    fn test_roundtrip_conversion() {
        let original = vec![0x12, 0x34, 0x56];
        let bits = GateLevelRuntime::bytes_to_bits(&original, 24);
        let bytes = GateLevelRuntime::bits_to_bytes(&bits);
        assert_eq!(bytes, original);
    }
}
