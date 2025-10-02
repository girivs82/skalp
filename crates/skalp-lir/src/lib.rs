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
pub mod technology_mapping;
pub mod timing;

pub use lir::{Gate, GateType, Lir, Net, LirDesign, LirModule, LirSignal};
pub use mir_to_lir::transform_mir_to_lir;
pub use optimization::{OptimizationPipeline, OptimizationResult};
pub use netlist::Netlist;
pub use technology_mapping::{TechnologyMapper, TechnologyTarget, TechnologyMappingResult, ResourceUsage};

use skalp_mir::Mir;
use anyhow::Result;

/// Lower MIR to LIR
pub fn lower_to_lir(mir: &Mir) -> Result<LirDesign> {
    let mut lir_modules = Vec::new();

    for module in &mir.modules {
        // Transform MIR module to LIR
        let lir = transform_mir_to_lir(module);

        // Create LirSignals from MIR ports and signals
        let mut signals = Vec::new();

        // Convert ports to signals
        for port in &module.ports {
            let signal = LirSignal {
                name: port.name.clone(),
                signal_type: format!("{:?}", port.port_type), // Convert type to string
                is_input: matches!(port.direction, skalp_mir::PortDirection::Input),
                is_register: false, // Ports are not registers
                is_output: matches!(port.direction, skalp_mir::PortDirection::Output),
            };
            signals.push(signal);
        }

        // Convert internal signals
        for signal in &module.signals {
            let lir_signal = LirSignal {
                name: signal.name.clone(),
                signal_type: format!("{:?}", signal.signal_type),
                is_input: false,
                is_output: false, // Internal signals are not ports
                is_register: signal.initial.is_some(), // Signals with initial values are registers
            };
            signals.push(lir_signal);
        }

        // Create LirModule with populated signals
        lir_modules.push(LirModule {
            name: module.name.clone(),
            signals,
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

    if !lir_modules.is_empty() {
    }

    Ok(LirDesign {
        name: mir.name.clone(),
        modules: lir_modules,
    })
}