#![allow(dead_code, unused_variables, unused_imports)]
//! SKALP LIR - Low-level Intermediate Representation
//!
//! Gate-level representation for hardware designs. The compilation flow is:
//!
//! ```text
//! HIR → MIR → LIR → SIR (for simulation)
//! ```
//!
//! Key types:
//! - [`Lir`] - Technology-independent gate-level netlist (primitives + nets)
//! - [`Primitive`] - Individual gate/flip-flop/mux with hierarchy path
//! - [`PrimitiveType`] - Type of primitive (AND, OR, DFF, MUX2, etc.)
//! - [`LirNet`] - Wire connecting primitives
//!
//! # Example
//!
//! ```ignore
//! use skalp_lir::lower_to_lir;
//!
//! let mir = compile_to_mir(source)?;
//! let lir_results = lower_to_lir(&mir)?;
//! for result in lir_results {
//!     println!("Module: {}", result.lir.name);
//!     println!("Primitives: {}", result.lir.primitives.len());
//!     println!("Total FIT: {}", result.lir.stats.total_fit);
//! }
//! ```

pub mod builtin_libraries;
pub mod gate_netlist;
pub mod gate_optimization;
pub mod lir;
pub mod mir_to_gate_netlist;
pub mod mir_to_word_lir;
pub mod netlist;
pub mod pattern_detector;
pub mod primitives;
pub mod tech_library;
pub mod tech_mapper;
pub mod technology;
pub mod word_lir;

// Primary LIR types
pub use lir::{
    FitOverrides, HierarchyNode, Lir, LirNet, LirSafetyInfo, NetId, NetlistStats, Primitive,
    PrimitiveId, PrimitiveType,
};

// MIR to LIR transformation
pub use mir_to_gate_netlist::{
    lower_mir_module_to_lir, MirToLirResult, MirToLirTransform, TransformStats,
};

// Gate optimization passes (operate on Lir type)
pub use gate_optimization::{
    GateBooleanSimplification, GateBufferRemoval, GateCSE, GateConstantFolding,
    GateDeadCodeElimination, GateFanoutOptimization, GateMuxOptimization, GateOptConfig,
    GateOptimizationPipeline, GateOptimizationResult, LirOptimizationPass, OptTarget,
};

// Other exports
pub use netlist::Netlist;

// Word-level LIR (for technology mapping)
pub use word_lir::{WordLir, WordLirStats, WordNode, WordNodeId, WordOp, WordSignal, WordSignalId};

// MIR to Word-level LIR transformation
pub use mir_to_word_lir::{lower_mir_module_to_word_lir, MirToWordLirResult};

// Gate-level netlist (output of technology mapping)
pub use gate_netlist::{
    Cell, CellFailureMode, CellId, CellSafetyClassification, FaultType, GateNet, GateNetId,
    GateNetlist, GateNetlistStats,
};

// Technology library
pub use tech_library::{
    arrhenius_acceleration_factor,
    process_corner_factor,
    voltage_acceleration_factor,
    CellFunction,
    DecompConnectivity,
    DecompSource,
    DecompositionRule,
    // Derating and operating conditions
    DeratingFactors,
    DeratingPreset,
    LibraryCell,
    LibraryDeratingSummary,
    LibraryFailureMode,
    OperatingConditions,
    ProcessCorner,
    TechLibrary,
};

// Technology mapper
pub use tech_mapper::{map_word_lir_to_gates, TechMapResult, TechMapStats, TechMapper};

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

use anyhow::Result;
use skalp_mir::Mir;

/// Lower MIR to LIR (gate-level netlist)
///
/// This produces a technology-independent gate-level representation with:
/// - Full primitive decomposition (gates, flip-flops, muxes, adders)
/// - Per-bit net representation for multi-bit signals
/// - FIT estimation for each primitive (for ISO 26262 safety analysis)
/// - Hierarchy traceability via `Primitive.path`
///
/// # Example
///
/// ```ignore
/// use skalp_mir::Mir;
/// use skalp_lir::lower_to_lir;
///
/// let mir = compile_to_mir(source)?;
/// let lir_results = lower_to_lir(&mir)?;
/// for result in lir_results {
///     println!("Module: {}", result.lir.name);
///     println!("Primitives: {}", result.lir.primitives.len());
///     println!("Total FIT: {}", result.lir.stats.total_fit);
/// }
/// ```
pub fn lower_to_lir(mir: &Mir) -> Result<Vec<MirToLirResult>> {
    let mut results = Vec::new();

    for module in &mir.modules {
        let result = lower_mir_module_to_lir(module);
        results.push(result);
    }

    Ok(results)
}
