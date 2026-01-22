//! Native FPGA Place and Route for SKALP
//!
//! This crate provides native Rust place and route for iCE40 FPGAs,
//! eliminating the dependency on external tools like nextpnr while
//! maintaining IceStorm bitstream compatibility.
//!
//! # Overview
//!
//! The place and route flow consists of:
//! 1. **Device Database**: Architecture description for iCE40 FPGAs
//! 2. **Placement**: Analytical placement with simulated annealing refinement
//! 3. **Routing**: PathFinder negotiated congestion routing with A*
//! 4. **Bitstream**: IceStorm-compatible ASCII and binary formats
//! 5. **Timing**: Static timing analysis with iCE40 delay models
//!
//! # Example
//!
//! ```ignore
//! use skalp_place_route::{place_and_route, PnrConfig, Ice40Variant};
//! use skalp_lir::gate_netlist::GateNetlist;
//!
//! // Get netlist from technology mapping
//! let netlist: GateNetlist = /* from skalp-lir */;
//!
//! // Run place and route
//! let result = place_and_route(&netlist, Ice40Variant::Hx8k, PnrConfig::default())?;
//!
//! // Write bitstream
//! result.bitstream.write_to_file(Path::new("output.bin"))?;
//! ```
//!
//! # Supported Devices
//!
//! - iCE40 HX1K, HX4K, HX8K
//! - iCE40 LP1K, LP4K, LP8K
//! - iCE40 UP5K
//!
//! # Algorithms
//!
//! ## Placement
//! - Analytical placement using quadratic wirelength minimization
//! - Legalization to valid BEL sites
//! - Simulated annealing refinement with HPWL cost
//!
//! ## Routing
//! - A* shortest path finding
//! - PathFinder negotiated congestion routing
//! - Global clock/reset routing through dedicated resources
//!
//! ## Timing
//! - Static timing analysis with iCE40 delay models
//! - Setup/hold checking
//! - Critical path reporting

pub mod bitstream;
pub mod device;
pub mod error;
pub mod placer;
pub mod pnr;
pub mod router;
pub mod timing;

// Re-export main types for convenience
pub use bitstream::{Bitstream, BitstreamConfig, BitstreamFormat, BitstreamGenerator};
pub use device::ice40::{Ice40ChipDb, Ice40Device, Ice40Variant};
pub use device::{
    Bel, BelId, BelType, Device, DeviceFamily, DeviceStats, PackagePins, Pip, PipId, Tile,
    TileType, Wire, WireId, WireType,
};
pub use error::{PlaceRouteError, Result};
pub use placer::{
    IoConstraints, PinConstraint, PlacementAlgorithm, PlacementLoc, PlacementResult, Placer,
    PlacerConfig, PullType,
};
pub use pnr::{
    place_and_route, place_and_route_hx1k, place_and_route_hx8k, place_and_route_up5k, PnrConfig,
    PnrResult,
};
pub use router::{Route, Router, RouterConfig, RoutingAlgorithm, RoutingResult};
pub use timing::{DelayModel, TimingAnalyzer, TimingConfig, TimingDrivenPlacer, TimingReport};
