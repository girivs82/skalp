//! Logic Synthesis Engine
//!
//! This module provides production-quality logic synthesis optimization inspired by
//! Yosys and ABC, but with safety-aware optimizations preserving FIT rates and
//! ISO 26262 traceability.
//!
//! # Architecture
//!
//! ```text
//! GateNetlist → AIG Builder → AIG → [Optimization Passes] → AIG Writer → GateNetlist
//! ```
//!
//! # Key Components
//!
//! - [`Aig`] - And-Inverter Graph representation
//! - [`AigBuilder`] - Converts GateNetlist to AIG
//! - [`AigWriter`] - Converts AIG back to GateNetlist
//!
//! # Example
//!
//! ```ignore
//! use skalp_lir::synth::{Aig, AigBuilder, AigWriter};
//!
//! // Convert to AIG for optimization
//! let mut builder = AigBuilder::new(&gate_netlist);
//! let mut aig = builder.build();
//!
//! // Run optimization passes
//! aig.strash();
//!
//! // Convert back to gate netlist
//! let writer = AigWriter::new(&library);
//! let optimized_netlist = writer.write(&aig);
//! ```

mod aig;
mod aig_builder;
mod aig_writer;
pub mod cuts;
pub mod datapath;
mod engine;
pub mod liberty;
pub mod mapping;
pub mod npn;
pub mod passes;
pub mod sta;
pub mod timing;

pub use aig::{Aig, AigLit, AigNode, AigNodeId, AigSafetyInfo, AigStats};
pub use aig_builder::AigBuilder;
pub use aig_writer::AigWriter;
pub use cuts::{Cut, CutEnumeration, CutParams, CutSet};
pub use datapath::{
    generate_carry_lookahead, generate_kogge_stone, generate_ripple_carry, AdderArchitecture,
    AdderConfig, AdderOptimizer, AdderStats, DatapathConfig, DatapathOp, DatapathStats,
};
pub use engine::{SynthConfig, SynthEngine, SynthPreset, SynthResult};
pub use liberty::{create_basic_library, LibertyLibrary};
pub use mapping::{
    size_cells, size_cells_for_area, size_cells_for_timing, CellMatcher, CellSizer,
    CellSizingConfig, CellSizingStats, CutMapper, CutMatch, DelayMapper, DelayMappingConfig,
    DriveStrength, MappedNode, MappingObjective, MappingResult, MappingStats,
};
pub use npn::{are_npn_equivalent, npn_canonicalize, NpnCanonical, NpnDatabase};
pub use passes::{
    analyze_fanout, Balance, BufferConfig, BufferInsertion, BufferStats, ConstProp, Dce,
    FanoutAnalysis, Pass, PassResult, Refactor, Rewrite, Strash,
};
pub use sta::{Sta, StaResult, TimingOptHints, TimingPath};
pub use timing::{
    CellTiming, ClockDefinition, NetTiming, NldmTable, OperatingConditions, PinTiming, TimePs,
    TimingArc, TimingConstraints, TimingSense, TimingType,
};

#[cfg(test)]
mod tests;
