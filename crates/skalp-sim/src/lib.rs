// Also allow unknown_lints because this lint doesn't exist in stable Rust
#![allow(unknown_lints)]
#![allow(clippy::manual_is_multiple_of)]

// Allow manual_is_multiple_of lint because is_multiple_of() is not yet stable

// GLOBAL: Disable all debug output for performance
macro_rules! eprintln {
    ($($arg:tt)*) => {{}};
}

pub mod clock_manager;
pub mod cpu_runtime;
#[cfg(target_os = "macos")]
pub mod gpu_runtime;
pub mod simulator;
pub mod testbench;
pub mod waveform;

pub use clock_manager::{ClockEdge, ClockInfo, ClockManager};
pub use cpu_runtime::CpuRuntime;
#[cfg(target_os = "macos")]
pub use gpu_runtime::{GpuDevice, GpuRuntime};
pub use simulator::{SimulationConfig, SimulationResult, Simulator};
pub use testbench::{TestResult, TestVector, Testbench};
pub use waveform::{Signal as WaveformSignal, Waveform};
