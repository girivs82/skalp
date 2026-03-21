//! NCL Behavioral Simulation Runtime
//!
//! Phase-level behavioral simulation for NCL (Null Convention Logic) circuits.
//! Models DATA/NULL phase alternation at word level using the compiled C++ kernel,
//! providing NCL-correct semantics at behavioral simulation speed.
//!
//! # Architecture
//!
//! Wraps `CompiledCpuRuntime` (SIR → C++ → native) but drives it with NCL phase
//! semantics instead of clock edges:
//!
//! ```text
//! DATA phase:  apply inputs → combinational eval → register update → eval outputs
//! NULL phase:  clear inputs → combinational eval → registers hold → outputs go NULL
//! ```
//!
//! # Why not just use sync behavioral?
//!
//! - NCL circuits have no clock — registers update on completion detection
//! - NULL phases between DATA phases are part of the protocol
//! - Downstream modules expect to see phase alternation
//! - Behavioral NCL catches protocol bugs that sync sim would miss
//!
//! # Performance
//!
//! Same as `CompiledCpuRuntime` — native compiled C++ evaluation.
//! Each NCL cycle is ~2 sync steps (DATA eval + NULL eval).
//!
//! # Example
//!
//! ```ignore
//! let mut sim = NclBehavioralRuntime::new(&sir_module)?;
//!
//! // Set inputs for DATA phase
//! sim.set_input("enable", 1, 1);
//!
//! // Run one complete DATA→NULL cycle
//! let outputs = sim.step_cycle();
//! assert_eq!(outputs["count"], vec![1, 0, 0, 0]);
//!
//! // Run 10 more cycles
//! let results = sim.run_cycles(10);
//! ```

use crate::compiled_cpu_runtime::CompiledCpuRuntime;
use crate::cpp_compiler::CompileError;
use indexmap::IndexMap;
use skalp_sir::SirModule;
use std::collections::HashMap;

/// NCL phase state
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NclBehavioralPhase {
    /// NULL phase: all inputs/outputs are NULL (zero)
    Null,
    /// DATA phase: valid data is being processed
    Data,
}

/// Statistics for NCL behavioral simulation
#[derive(Debug, Clone, Default)]
pub struct NclBehavioralStats {
    /// Number of complete DATA/NULL cycles
    pub cycles: u64,
    /// Number of DATA phases evaluated
    pub data_phases: u64,
    /// Number of NULL phases evaluated
    pub null_phases: u64,
}

/// NCL behavioral simulation runtime
///
/// Provides phase-accurate NCL simulation at word level by wrapping
/// the compiled C++ behavioral kernel with NCL phase sequencing.
pub struct NclBehavioralRuntime {
    /// Underlying compiled runtime (handles eval_combinational + eval_sequential)
    inner: CompiledCpuRuntime,
    /// Current phase
    phase: NclBehavioralPhase,
    /// Simulation statistics
    stats: NclBehavioralStats,
    /// Cached input values for DATA phase (name -> value bytes)
    data_inputs: IndexMap<String, Vec<u8>>,
    /// User-facing output names (for API results)
    output_names: Vec<String>,
    /// Internal input names (for NULL clearing via set_input_sync)
    input_names: Vec<String>,
    /// User-facing name → internal SIR name (for inputs)
    user_to_internal_input: HashMap<String, String>,
    /// User-facing name → internal SIR name (for outputs)
    user_to_internal_output: HashMap<String, String>,
}

impl NclBehavioralRuntime {
    /// Create a new NCL behavioral runtime from a SIR module
    pub fn new(module: &SirModule) -> Result<Self, CompileError> {
        let inner = CompiledCpuRuntime::new(module)?;

        // Internal names for direct set_input_sync/get_output_sync calls
        let input_names: Vec<String> = module.inputs.iter().map(|i| i.name.clone()).collect();

        // Build user-facing → internal name maps via name_registry
        let mut user_to_internal_input = HashMap::new();
        for input in &module.inputs {
            if let Some(user_name) = module.name_registry.reverse_resolve(&input.name) {
                user_to_internal_input.insert(user_name.to_string(), input.name.clone());
            }
        }

        let mut user_to_internal_output = HashMap::new();
        let mut output_names = Vec::new();
        for output in &module.outputs {
            if let Some(user_name) = module.name_registry.reverse_resolve(&output.name) {
                user_to_internal_output.insert(user_name.to_string(), output.name.clone());
                output_names.push(user_name.to_string());
            } else {
                // Fallback: use internal name directly
                output_names.push(output.name.clone());
            }
        }

        Ok(Self {
            inner,
            phase: NclBehavioralPhase::Null,
            stats: NclBehavioralStats::default(),
            data_inputs: IndexMap::new(),
            output_names,
            input_names,
            user_to_internal_input,
            user_to_internal_output,
        })
    }

    /// Set an input value for the next DATA phase.
    /// Values are buffered and applied when `step_data()` is called.
    pub fn set_input(&mut self, name: &str, value: u64, width: usize) {
        let byte_width = ((width + 7) / 8).max(1);
        let bytes = value.to_le_bytes();
        // Resolve user-facing name to internal SIR name
        let internal = self.resolve_input_name(name);
        self.data_inputs
            .insert(internal, bytes[..byte_width].to_vec());
    }

    /// Set an input value from raw bytes
    pub fn set_input_bytes(&mut self, name: &str, value: &[u8]) {
        let internal = self.resolve_input_name(name);
        self.data_inputs.insert(internal, value.to_vec());
    }

    /// Resolve a user-facing input name to its internal SIR name
    fn resolve_input_name(&self, name: &str) -> String {
        self.user_to_internal_input
            .get(name)
            .cloned()
            .unwrap_or_else(|| name.to_string())
    }

    /// Resolve a user-facing output name to its internal SIR name
    fn resolve_output_name(&self, name: &str) -> String {
        self.user_to_internal_output
            .get(name)
            .cloned()
            .unwrap_or_else(|| name.to_string())
    }

    /// Execute one DATA phase:
    /// 1. Apply buffered input values
    /// 2. Evaluate combinational logic
    /// 3. Update registers (completion implicitly true — all inputs valid)
    /// 4. Re-evaluate combinational (propagate new register values to outputs)
    ///
    /// Returns output values as (name, value_bytes) pairs.
    pub fn step_data(&mut self) -> IndexMap<String, Vec<u8>> {
        // Apply all buffered inputs
        for (name, value) in &self.data_inputs {
            let _ = self.inner.set_input_sync(name, value);
        }

        // Evaluate: combinational → sequential → combinational
        // At behavioral level, completion is implicit (all inputs are valid DATA)
        self.inner.eval_combinational();
        self.inner.eval_sequential();
        self.inner.eval_combinational();

        self.phase = NclBehavioralPhase::Data;
        self.stats.data_phases += 1;

        self.read_outputs()
    }

    /// Execute one NULL phase:
    /// 1. Clear all inputs to zero (NULL encoding)
    /// 2. Evaluate combinational logic (outputs go NULL)
    /// 3. Registers HOLD their values (no sequential update during NULL)
    pub fn step_null(&mut self) {
        // Clear all inputs to zero (NULL)
        let zero = vec![0u8; 8];
        for name in &self.input_names {
            let _ = self.inner.set_input_sync(name, &zero);
        }

        // Combinational only — registers hold through NULL phase
        self.inner.eval_combinational();

        self.phase = NclBehavioralPhase::Null;
        self.stats.null_phases += 1;
    }

    /// Execute one complete NCL cycle (DATA → NULL).
    /// Returns the output values from the DATA phase.
    pub fn step_cycle(&mut self) -> IndexMap<String, Vec<u8>> {
        let outputs = self.step_data();
        self.step_null();
        self.stats.cycles += 1;
        outputs
    }

    /// Run multiple NCL cycles, returning outputs from each DATA phase.
    pub fn run_cycles(&mut self, count: u64) -> Vec<IndexMap<String, Vec<u8>>> {
        let mut results = Vec::with_capacity(count as usize);
        for _ in 0..count {
            results.push(self.step_cycle());
        }
        results
    }

    /// Get the current output values (keyed by user-facing names)
    pub fn read_outputs(&self) -> IndexMap<String, Vec<u8>> {
        let mut outputs = IndexMap::new();
        for user_name in &self.output_names {
            let internal = self.resolve_output_name(user_name);
            if let Ok(value) = self.inner.get_output_sync(&internal) {
                outputs.insert(user_name.clone(), value);
            }
        }
        outputs
    }

    /// Get a single output value as u64
    pub fn get_output(&self, name: &str) -> Option<u64> {
        let internal = self.resolve_output_name(name);
        self.inner
            .get_output_sync(&internal)
            .ok()
            .map(|bytes| bytes_to_u64(&bytes))
    }

    /// Get a single output value as raw bytes
    pub fn get_output_bytes(&self, name: &str) -> Option<Vec<u8>> {
        let internal = self.resolve_output_name(name);
        self.inner.get_output_sync(&internal).ok()
    }

    /// Get current phase
    pub fn phase(&self) -> NclBehavioralPhase {
        self.phase
    }

    /// Get simulation statistics
    pub fn stats(&self) -> &NclBehavioralStats {
        &self.stats
    }

    /// Reset the simulation — clear all registers and return to NULL phase
    pub fn reset(&mut self) {
        let zero = vec![0u8; 8];
        for name in &self.input_names {
            let _ = self.inner.set_input_sync(name, &zero);
        }
        self.inner.reset_registers();
        self.inner.eval_combinational();
        self.phase = NclBehavioralPhase::Null;
        self.stats = NclBehavioralStats::default();
        self.data_inputs.clear();
    }
}

/// Convert bytes (little-endian) to u64
fn bytes_to_u64(bytes: &[u8]) -> u64 {
    let mut buf = [0u8; 8];
    let len = bytes.len().min(8);
    buf[..len].copy_from_slice(&bytes[..len]);
    u64::from_le_bytes(buf)
}
