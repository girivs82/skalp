// Also allow unknown_lints because this lint doesn't exist in stable Rust
#![allow(unknown_lints)]
#![allow(clippy::manual_is_multiple_of)]

// Allow manual_is_multiple_of lint because is_multiple_of() is not yet stable

// GLOBAL: Disable all debug output for performance
macro_rules! eprintln {
    ($($arg:tt)*) => {{}};
}

pub mod breakpoint;
pub mod clock_manager;
pub mod cpu_runtime;
pub mod gate_eval;
pub mod gate_simulator;
#[cfg(target_os = "macos")]
pub mod gpu_fault_simulator;
#[cfg(target_os = "macos")]
pub mod gpu_runtime;
pub mod lir_to_sir;
pub mod simulator;
pub mod sir;
pub mod testbench;
pub mod waveform;

pub use breakpoint::{BreakpointAction, BreakpointCondition, BreakpointHit, BreakpointManager, SimBreakpoint};
pub use clock_manager::{ClockEdge, ClockInfo, ClockManager};
pub use cpu_runtime::CpuRuntime;
#[cfg(target_os = "macos")]
pub use gpu_runtime::{GpuDevice, GpuRuntime};
pub use simulator::{SimulationConfig, SimulationResult, Simulator};
pub use testbench::{TestResult, TestVector, Testbench};
pub use gate_eval::{bits_to_value, evaluate_primitive, evaluate_primitive_with_fault, value_to_bits};
pub use gate_simulator::{FaultCampaignConfig, FaultCampaignResults, FaultSimResult, GateLevelSimulator, GateSimulationState};
#[cfg(target_os = "macos")]
pub use gpu_fault_simulator::{GpuFaultSimulator, GpuFaultCampaignConfig};
pub use lir_to_sir::{convert_gate_netlist_to_sir, ConversionStats, LirToSirConverter, LirToSirResult};
pub use waveform::{Signal as WaveformSignal, Waveform};
