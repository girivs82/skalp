//! Technology mapping and library information for SKALP LIR

use serde::{Deserialize, Serialize};
use crate::primitives::{Primitive, PrimitiveInfo};

/// Technology definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Technology {
    /// Technology name
    pub name: String,
    /// Technology kind
    pub kind: TechnologyKind,
    /// Process node size in nanometers
    pub process_nm: u32,
}

/// Kind of technology target
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum TechnologyKind {
    /// FPGA target
    FPGA,
    /// ASIC standard cell
    ASIC,
    /// Generic technology
    Generic,
}

/// Technology library definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TechnologyLibrary {
    /// Library name
    pub name: String,
    /// Technology node (e.g., "7nm", "28nm")
    pub node: String,
    /// Available primitives in this technology
    pub primitives: Vec<PrimitiveInfo>,
    /// Library characteristics
    pub characteristics: LibraryCharacteristics,
}

/// Technology library characteristics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LibraryCharacteristics {
    /// Supply voltage (volts)
    pub vdd: f64,
    /// Operating temperature range (Celsius)
    pub temperature_range: (i32, i32),
    /// Process corners
    pub corners: Vec<ProcessCorner>,
    /// Metal layers available
    pub metal_layers: u8,
}

/// Process corner definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProcessCorner {
    /// Corner name (e.g., "FF", "SS", "TT")
    pub name: String,
    /// Process variation (Fast/Slow)
    pub process: ProcessVariation,
    /// Voltage variation
    pub voltage: VoltageVariation,
    /// Temperature (Celsius)
    pub temperature: i32,
}

/// Process variation types
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ProcessVariation {
    Fast,
    Typical,
    Slow,
}

/// Voltage variation types
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum VoltageVariation {
    High,
    Nominal,
    Low,
}

/// Technology mapping result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MappingResult {
    /// Original primitive
    pub source: Primitive,
    /// Mapped technology primitive
    pub target: String,
    /// Mapping quality score
    pub quality: f64,
    /// Estimated characteristics after mapping
    pub estimated_characteristics: PrimitiveInfo,
}

/// Technology mapper
pub struct TechnologyMapper {
    /// Available technology libraries
    pub libraries: Vec<TechnologyLibrary>,
}

impl TechnologyMapper {
    /// Create a new technology mapper
    pub fn new() -> Self {
        Self {
            libraries: Vec::new(),
        }
    }

    /// Add a technology library
    pub fn add_library(&mut self, library: TechnologyLibrary) {
        self.libraries.push(library);
    }

    /// Map a primitive to target technology
    pub fn map_primitive(&self, primitive: &Primitive, target_library: &str) -> Option<MappingResult> {
        // Stub implementation - will be expanded in Phase 3
        None
    }
}

impl Default for TechnologyMapper {
    fn default() -> Self {
        Self::new()
    }
}