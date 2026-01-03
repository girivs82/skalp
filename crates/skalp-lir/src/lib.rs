#![allow(dead_code, unused_variables, unused_imports)]
//! SKALP LIR - Low-level Intermediate Representation
//!
//! Word-level and gate-level representations for hardware designs. The compilation flow is:
//!
//! ```text
//! HIR → MIR → Lir → TechMapper → GateNetlist → SIR (for simulation)
//! ```
//!
//! Key types:
//! - [`Lir`] - Word-level IR preserving multi-bit operations
//! - [`LirOp`] - Word-level operations (Add, Mux2, Reg, etc.)
//! - [`GateNetlist`] - Gate-level netlist from technology mapping
//! - [`PrimitiveType`] - Type of gate primitive (AND, OR, DFF, MUX2, etc.)
//! - [`CompiledIp`] - Precompiled IP binary format for distribution
//!
//! # Example
//!
//! ```ignore
//! use skalp_lir::{lower_mir_module_to_lir, map_lir_to_gates, get_stdlib_library};
//!
//! let mir = compile_to_mir(source)?;
//! let lir_result = lower_mir_module_to_lir(&mir.modules[0]);
//! let library = get_stdlib_library("generic_asic")?;
//! let gate_netlist = map_lir_to_gates(&lir_result.lir, &library)?;
//! println!("Cells: {}", gate_netlist.netlist.cells.len());
//! ```

pub mod async_sta;
pub mod compiled_ip;
pub mod gate_netlist;
pub mod gate_optimizer;
pub mod hierarchical_netlist;
pub mod lir;
pub mod mir_to_lir;
pub mod ncl_dual_rail;
pub mod ncl_expand;
pub mod ncl_optimizer;
pub mod netlist;
pub mod pattern_detector;
pub mod pipeline_annotations;
pub mod primitives;
pub mod synth;
pub mod tech_library;
pub mod tech_mapper;
pub mod technology;

// Core primitive types (used by SIR for simulation)
pub use lir::{FitOverrides, HierarchyNode, LirSafetyInfo, NetId, PrimitiveId, PrimitiveType};

// LIR types (word-level, input to technology mapping)
pub use lir::{Lir, LirNode, LirNodeId, LirOp, LirSignal, LirSignalId, LirStats};

// Backward-compatible type aliases
pub use lir::{WordLir, WordLirStats, WordNode, WordNodeId, WordOp, WordSignal, WordSignalId};

// MIR to LIR transformation
pub use mir_to_lir::{lower_mir_module_to_lir, MirToLirResult};

// Backward-compatible aliases for MIR to LIR
pub use mir_to_lir::{lower_mir_module_to_word_lir, MirToWordLirResult};

// Hierarchical MIR to LIR transformation
pub use mir_to_lir::{
    lower_mir_hierarchical, HierarchicalMirToLirResult, InstanceLirResult, PortConnectionInfo,
};

// Gate-level netlist (output of technology mapping)
pub use gate_netlist::{
    Cell, CellFailureMode, CellId, CellSafetyClassification, FaultType, GateNet, GateNetId,
    GateNetlist, GateNetlistStats,
};

// Technology library
pub use tech_library::{
    arrhenius_acceleration_factor, get_stdlib_library, list_stdlib_libraries,
    process_corner_factor, voltage_acceleration_factor, CellFunction, DecompConnectivity,
    DecompSource, DecompositionRule, DeratingFactors, DeratingPreset, LibraryCell,
    LibraryDeratingSummary, LibraryFailureMode, LibraryLoadError, OperatingConditions,
    ProcessCorner, TechLibrary,
};

// Technology mapper
pub use tech_mapper::{
    map_hierarchical_to_gates, map_lir_to_gates, map_lir_to_gates_optimized,
    map_lir_to_gates_with_opt_level, map_word_lir_to_gates, TechMapResult, TechMapStats,
    TechMapper,
};

// Structural pattern detection for safety mechanisms
pub use pattern_detector::{
    DetectedPatterns, DmrPattern, PatternDetector, TmrPattern, VoterPattern, VoterType,
    WatchdogPattern,
};

// Gate optimizer
pub use gate_optimizer::{GateOptimizer, OptimizationStats, PassStats};

// NCL optimizer
pub use ncl_optimizer::{NclOptConfig, NclOptStats, NclOptimizer};

// NCL dual-rail conversion (optimize-first flow)
pub use ncl_dual_rail::{convert_to_dual_rail, DualRailConfig, DualRailConverter, DualRailStats};

// Hierarchical netlist (for per-entity synthesis)
pub use hierarchical_netlist::{
    HierarchicalNetlist, HierarchicalSynthResult, InstanceNetlist, InstancePath, PortConnection,
};

// Synthesis engine (AIG-based optimization)
pub use synth::{Aig, AigBuilder, AigLit, AigNode, AigNodeId, AigSafetyInfo, AigStats, AigWriter};

// Other exports
pub use netlist::Netlist;

// Compiled IP format
pub use compiled_ip::{
    generate_header, CompiledIp, CompiledIpHeader, CompiledPortDirection, CompiledPortInfo,
    GenericValue, SKB_MAGIC, SKB_VERSION,
};

// Async STA for NCL circuits
pub use async_sta::{
    analyze_async_timing, AsyncSta, AsyncStaConfig, AsyncStaResult, AsyncStaStats,
    CompletionViolation, ForkViolation, ViolationSeverity,
};
