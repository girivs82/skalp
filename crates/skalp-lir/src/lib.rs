#![allow(dead_code, unused_variables, unused_imports)]
//! SKALP LIR - Low-level Intermediate Representation
//!
//! This crate handles:
//! - Gate-level representation
//! - Technology mapping
//! - Netlist generation
//! - Physical primitives
//! - MIR to LIR transformation
//! - Optimization passes
//! - Timing analysis

pub mod lir;
pub mod mir_to_lir;
pub mod netlist;
pub mod optimization;
pub mod primitives;
pub mod tech_mapping;
pub mod technology;
pub mod timing;

pub use lir::{Gate, GateType, Lir, Net, LirDesign, LirModule, LirSignal};
pub use mir_to_lir::transform_mir_to_lir;
pub use optimization::{OptimizationPipeline, OptimizationResult};
pub use netlist::Netlist;

use skalp_mir::Mir;
use anyhow::Result;

/// Lower MIR to LIR
pub fn lower_to_lir(mir: &Mir) -> Result<LirDesign> {
    // Simplified lowering - would use full transformer in production
    let mut lir_modules = Vec::new();

    for module in &mir.modules {
        let lir = transform_mir_to_lir(module);
        // Convert Lir to LirModule
        lir_modules.push(LirModule {
            name: module.name.clone(),
            signals: Vec::new(),
            gates: lir.gates.clone(),
            nets: lir.nets.clone(),
        });
    }

    // If no modules, create a default one
    if lir_modules.is_empty() {
        lir_modules.push(LirModule {
            name: mir.name.clone(),
            signals: Vec::new(),
            gates: Vec::new(),
            nets: Vec::new(),
        });
    }

    Ok(LirDesign {
        name: mir.name.clone(),
        modules: lir_modules,
    })
}