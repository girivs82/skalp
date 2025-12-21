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
//!
//! # Example
//!
//! ```ignore
//! use skalp_lir::{lower_mir_module_to_lir, map_lir_to_gates, builtin_generic_asic};
//!
//! let mir = compile_to_mir(source)?;
//! let lir_result = lower_mir_module_to_lir(&mir.modules[0]);
//! let library = builtin_generic_asic();
//! let gate_netlist = map_lir_to_gates(&lir_result.lir, &library)?;
//! println!("Cells: {}", gate_netlist.netlist.cells.len());
//! ```

pub mod builtin_libraries;
pub mod gate_netlist;
pub mod gate_optimizer;
pub mod hierarchical_netlist;
pub mod lir;
pub mod mir_to_lir;
pub mod netlist;
pub mod pattern_detector;
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
    arrhenius_acceleration_factor, process_corner_factor, voltage_acceleration_factor,
    CellFunction, DecompConnectivity, DecompSource, DecompositionRule, DeratingFactors,
    DeratingPreset, LibraryCell, LibraryDeratingSummary, LibraryFailureMode, OperatingConditions,
    ProcessCorner, TechLibrary,
};

// Technology mapper
pub use tech_mapper::{
    map_hierarchical_to_gates, map_lir_to_gates, map_lir_to_gates_optimized, map_word_lir_to_gates,
    TechMapResult, TechMapStats, TechMapper,
};

// Built-in technology libraries
pub use builtin_libraries::{
    builtin_asic_28nm, builtin_asic_7nm, builtin_fpga_lut4, builtin_fpga_lut6,
    builtin_generic_asic, get_builtin_library, list_builtin_libraries,
};

// Structural pattern detection for safety mechanisms
pub use pattern_detector::{
    DetectedPatterns, DmrPattern, PatternDetector, TmrPattern, VoterPattern, VoterType,
    WatchdogPattern,
};

// Gate optimizer
pub use gate_optimizer::{GateOptimizer, OptimizationStats, PassStats};

// Hierarchical netlist (for per-entity synthesis)
pub use hierarchical_netlist::{
    HierarchicalNetlist, HierarchicalSynthResult, InstanceNetlist, InstancePath, PortConnection,
};

// Synthesis engine (AIG-based optimization)
pub use synth::{Aig, AigBuilder, AigLit, AigNode, AigNodeId, AigSafetyInfo, AigStats, AigWriter};

// Other exports
pub use netlist::Netlist;
