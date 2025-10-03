//! Native ASIC Implementation for SKALP
//!
//! Complete native ASIC flow without external dependencies:
//! - Standard cell library mapping
//! - Native placement and routing
//! - Clock tree synthesis
//! - GDSII generation
//! - Design rule checking

pub mod cts;
pub mod cts_manual;
pub mod drc;
pub mod floorplan;
pub mod gdsii;
pub mod incremental;
pub mod interactive;
pub mod placement;
pub mod routing;
pub mod routing_guide;
pub mod sdc;
pub mod sky130;
pub mod timing;
pub mod visualization;
pub mod web_ui;

use thiserror::Error;

/// Error type for ASIC operations
#[derive(Debug, Error)]
pub enum AsicError {
    #[error("Placement error: {0}")]
    PlacementError(String),

    #[error("Routing error: {0}")]
    RoutingError(String),

    #[error("CTS error: {0}")]
    CtsError(String),

    #[error("Technology error: {0}")]
    TechnologyError(String),

    #[error("GDSII error: {0}")]
    GdsError(String),

    #[error("DRC error: {0}")]
    DrcError(String),

    #[error("IO error: {0}")]
    IoError(#[from] std::io::Error),
}

/// Native ASIC compiler
pub struct AsicCompiler {
    /// Target technology
    pub technology: Technology,
    /// Design rules
    pub design_rules: DesignRules,
}

/// Supported ASIC technologies
#[derive(Debug, Clone)]
pub enum Technology {
    /// SkyWater 130nm CMOS
    Sky130,
    /// GlobalFoundries 180nm MCU
    Gf180,
    /// IHP 130nm BiCMOS
    Ihp130,
}

/// Design rules for a technology
#[derive(Debug, Clone)]
pub struct DesignRules {
    /// Minimum feature size (nm)
    pub min_feature: f64,
    /// Metal layers
    pub metal_layers: usize,
    /// Minimum wire width (λ units)
    pub min_width: f64,
    /// Minimum wire spacing (λ units)
    pub min_spacing: f64,
    /// Via rules
    pub via_rules: ViaRules,
}

/// Via connection rules
#[derive(Debug, Clone)]
pub struct ViaRules {
    /// Minimum via size
    pub min_size: f64,
    /// Via enclosure
    pub enclosure: f64,
    /// Via spacing
    pub spacing: f64,
}
