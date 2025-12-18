//! Unified Simulation Runtime - to be reimplemented for GateNetlist
//!
//! This module needs to be updated to use GateNetlist instead of the legacy Lir type.
//! The new flow is: MIR → WordLir → TechMapper → GateNetlist → gate_netlist_to_sir → SIR

/// Simulation abstraction level
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum SimLevel {
    /// Behavioral simulation (MIR → behavioral SIR)
    #[default]
    Behavioral,
    /// Gate-level simulation (MIR → WordLir → GateNetlist → SIR)
    GateLevel,
}

/// Hardware acceleration mode
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum HwAccel {
    /// CPU-only simulation
    Cpu,
    /// GPU-accelerated simulation (Metal on macOS)
    Gpu,
    /// Auto-select (prefer GPU if available)
    #[default]
    Auto,
}

/// Configuration for unified simulation
#[derive(Debug, Clone, Default)]
pub struct UnifiedSimConfig {
    /// Simulation abstraction level
    pub level: SimLevel,
    /// Hardware acceleration mode
    pub hw_accel: HwAccel,
    /// Maximum cycles before timeout
    pub max_cycles: u64,
    /// Whether to capture waveforms
    pub capture_waveforms: bool,
}

/// Placeholder for UnifiedSimulator - to be reimplemented
pub struct UnifiedSimulator;
