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

pub mod breakpoint;
pub mod clock_manager;
pub mod debug_server;
pub mod compiled_cpu_runtime;
pub mod coverage_report;
pub mod coverage_vecgen;
pub mod cpp_compiler;
pub mod gate_eval;
pub mod gate_netlist_to_sir;
pub mod gate_runtime;
pub mod gate_simulator;
pub mod gpu_aig_cone_sim;
#[cfg(target_os = "macos")]
pub mod gpu_fault_simulator;
#[cfg(target_os = "macos")]
pub mod gpu_gate_runtime;
#[cfg(target_os = "macos")]
pub mod gpu_ncl_runtime;
#[cfg(target_os = "macos")]
pub mod gpu_runtime;
pub mod ncl_sim;
pub mod sim_coverage;
pub mod simulator;
pub mod sir;
pub mod unified_runtime;
pub mod waveform;

pub use breakpoint::{
    BreakpointAction, BreakpointCondition, BreakpointHit, BreakpointManager, SimBreakpoint,
};
pub use clock_manager::{ClockEdge, ClockInfo, ClockManager};
pub use compiled_cpu_runtime::CompiledCpuRuntime;
pub use cpp_compiler::{compile_cpp_kernel, clear_cache as clear_cpp_cache, cache_stats as cpp_cache_stats, CacheStats, CompileError};
pub use gate_eval::{
    bits_to_value, evaluate_primitive, evaluate_primitive_with_fault, value_to_bits,
};
pub use gate_netlist_to_sir::{
    convert_gate_netlist_to_sir, ConversionStats as GateNetlistConversionStats,
    GateNetlistToSirConverter, GateNetlistToSirResult,
};
// NOTE: GateLevelRuntime needs to be reimplemented for GateNetlist
pub use gate_simulator::{
    FaultCampaignConfig, FaultCampaignResults, FaultSimResult, GateLevelSimulator,
    GateSimulationState,
};
pub use gpu_aig_cone_sim::AigCone;
#[cfg(target_os = "macos")]
pub use gpu_aig_cone_sim::GpuAigConeSim;
#[cfg(target_os = "macos")]
pub use gpu_fault_simulator::{GpuFaultCampaignConfig, GpuFaultSimulator};
#[cfg(target_os = "macos")]
pub use gpu_gate_runtime::GpuGateRuntime;
#[cfg(target_os = "macos")]
pub use gpu_ncl_runtime::GpuNclRuntime;
#[cfg(target_os = "macos")]
pub use gpu_runtime::{GpuDevice, GpuRuntime};

pub use ncl_sim::{
    evaluate_thmn_stateful, NclGateState, NclPhase, NclSimConfig, NclSimStats, NclSimulator,
    NclValue,
};
pub use unified_runtime::{
    CircuitMode, HwAccel, SimLevel, UnifiedSimConfig, UnifiedSimResult, UnifiedSimulator,
};
pub use waveform::{Signal as WaveformSignal, Waveform};

pub use coverage_report::{CoverageReport, MuxArmStatus};
pub use coverage_vecgen::{CoverageVectorGen, InputVector};
pub use sim_coverage::{CoverageMetrics, SimCoverageDb};
