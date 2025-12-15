// Also allow unknown_lints because this lint doesn't exist in stable Rust
#![allow(unknown_lints)]
#![allow(clippy::manual_is_multiple_of)]
// Allow unused variables because debug output is disabled (eprintln macro is no-op)
#![allow(unused_variables)]
#![allow(unused_assignments)]
#![allow(clippy::only_used_in_recursion)]
#![allow(clippy::explicit_counter_loop)]
#![allow(clippy::if_same_then_else)]
#![allow(clippy::comparison_chain)]
#![allow(clippy::needless_range_loop)]
#![allow(clippy::manual_div_ceil)]
#![allow(clippy::match_like_matches_macro)]
#![allow(dead_code)]

// Allow manual_is_multiple_of lint because is_multiple_of() is not yet stable

// GLOBAL: Disable all debug output for performance
macro_rules! eprintln {
    ($($arg:tt)*) => {{}};
}

pub mod breakpoint;
pub mod clock_manager;
pub mod cpu_runtime;
pub mod gate_eval;
pub mod gate_netlist_to_sir;
pub mod gate_runtime;
pub mod gate_simulator;
#[cfg(target_os = "macos")]
pub mod gpu_fault_simulator;
#[cfg(target_os = "macos")]
pub mod gpu_gate_runtime;
#[cfg(target_os = "macos")]
pub mod gpu_runtime;
pub mod lir_to_sir;
pub mod simulator;
pub mod sir;
pub mod testbench;
pub mod unified_runtime;
pub mod waveform;

pub use breakpoint::{
    BreakpointAction, BreakpointCondition, BreakpointHit, BreakpointManager, SimBreakpoint,
};
pub use clock_manager::{ClockEdge, ClockInfo, ClockManager};
pub use cpu_runtime::CpuRuntime;
pub use gate_eval::{
    bits_to_value, evaluate_primitive, evaluate_primitive_with_fault, value_to_bits,
};
pub use gate_netlist_to_sir::{
    convert_gate_netlist_to_sir, ConversionStats as GateNetlistConversionStats,
    GateNetlistToSirConverter, GateNetlistToSirResult,
};
pub use gate_runtime::{GateLevelRuntime, SimulationMode};
pub use gate_simulator::{
    FaultCampaignConfig, FaultCampaignResults, FaultSimResult, GateLevelSimulator,
    GateSimulationState,
};
#[cfg(target_os = "macos")]
pub use gpu_fault_simulator::{GpuFaultCampaignConfig, GpuFaultSimulator};
#[cfg(target_os = "macos")]
pub use gpu_gate_runtime::GpuGateRuntime;
#[cfg(target_os = "macos")]
pub use gpu_runtime::{GpuDevice, GpuRuntime};
pub use lir_to_sir::{convert_lir_to_sir, ConversionStats, LirToSirConverter, LirToSirResult};
pub use simulator::{SimulationConfig, SimulationResult, Simulator};
pub use testbench::{TestResult, TestVector, Testbench};

// Backward-compatible alias - convert_lir_to_sir renamed from convert_gate_netlist_to_sir
// Note: The new `convert_gate_netlist_to_sir` function from gate_netlist_to_sir module
// is the preferred way to convert GateNetlist to SIR for gate-level simulation.
pub use unified_runtime::{HwAccel, SimLevel, UnifiedSimConfig, UnifiedSimulator};
pub use waveform::{Signal as WaveformSignal, Waveform};
