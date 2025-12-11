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

pub mod gate_optimization;
pub mod lir;
pub mod mir_to_gate_netlist;
pub mod netlist;
pub mod primitives;
pub mod technology;

// Primary LIR types
pub use lir::{
    FitOverrides, HierarchyNode, Lir, LirNet, NetId, NetlistStats, Primitive, PrimitiveId,
    PrimitiveType,
};

// MIR to LIR transformation
pub use mir_to_gate_netlist::{
    lower_mir_module_to_lir, MirToLirResult, MirToLirTransform, TransformStats,
};

// Gate optimization passes (operate on Lir type)
pub use gate_optimization::{
    GateBooleanSimplification, GateBufferRemoval, GateCSE, GateConstantFolding,
    GateDeadCodeElimination, GateFanoutOptimization, GateMuxOptimization,
    LirOptimizationPass, GateOptConfig, GateOptimizationPipeline, GateOptimizationResult,
    OptTarget,
};

// Other exports
pub use netlist::Netlist;

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
