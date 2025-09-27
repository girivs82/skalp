//! SKALP Simulation Engine
//!
//! GPU-accelerated simulation using Metal on macOS.
//! Single unified simulation path for simplicity.

pub mod simulator;
pub mod event;
pub mod state;

#[cfg(target_os = "macos")]
pub mod metal;

pub use simulator::Simulator;