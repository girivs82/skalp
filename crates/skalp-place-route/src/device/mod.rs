//! Device database module for FPGA architectures
//!
//! This module provides abstractions for describing FPGA device architectures,
//! including tiles, basic elements (BELs), wires, and programmable interconnect points (PIPs).

pub mod ice40;

use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Device family enumeration
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum DeviceFamily {
    /// Lattice iCE40 FPGAs
    Ice40,
    /// Lattice ECP5 FPGAs
    Ecp5,
    /// Generic VTR academic architecture
    Vtr,
    /// OpenFPGA custom architecture
    OpenFpga,
}

/// Unique identifier for a wire
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct WireId(pub u32);

/// Unique identifier for a PIP (Programmable Interconnect Point)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct PipId(pub u32);

/// Unique identifier for a BEL (Basic Element of Logic)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct BelId(pub u32);

/// Switch pattern for routing architecture
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum SwitchPattern {
    /// Full crossbar (all-to-all)
    Universal,
    /// Wilton switch pattern
    Wilton,
    /// Subset switch pattern
    Subset,
    /// iCE40 specific pattern
    Ice40,
}

/// Device statistics
#[derive(Debug, Clone, Default)]
pub struct DeviceStats {
    /// Total number of LUTs
    pub total_luts: usize,
    /// Total number of flip-flops
    pub total_ffs: usize,
    /// Total number of I/O pins
    pub total_ios: usize,
    /// Total number of block RAMs
    pub total_brams: usize,
    /// Total number of DSP blocks
    pub total_dsps: usize,
    /// Total number of global clock buffers
    pub total_gclks: usize,
}

/// Wire segment description for routing architecture
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WireSegment {
    /// Segment length (in tiles)
    pub length: u8,
    /// Number of such segments per tile
    pub count: u8,
    /// Direction (horizontal or vertical)
    pub direction: WireDirection,
}

/// Wire direction
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum WireDirection {
    Horizontal,
    Vertical,
    Bidirectional,
}

/// Routing architecture description
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RoutingArchitecture {
    /// Routing channel width (tracks per direction)
    pub channels: (u32, u32),
    /// Switch pattern used
    pub switch_pattern: SwitchPattern,
    /// Wire segments available
    pub wire_segments: Vec<WireSegment>,
}

impl Default for RoutingArchitecture {
    fn default() -> Self {
        Self {
            channels: (20, 20),
            switch_pattern: SwitchPattern::Ice40,
            wire_segments: vec![
                WireSegment {
                    length: 1,
                    count: 8,
                    direction: WireDirection::Bidirectional,
                },
                WireSegment {
                    length: 4,
                    count: 4,
                    direction: WireDirection::Horizontal,
                },
                WireSegment {
                    length: 4,
                    count: 4,
                    direction: WireDirection::Vertical,
                },
            ],
        }
    }
}

/// Clock resources description
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ClockResources {
    /// Number of global clock networks
    pub global_clocks: u8,
    /// Number of PLLs
    pub plls: u8,
    /// Number of DLLs (delay-locked loops)
    pub dlls: u8,
    /// Clock domains with frequency limits
    pub clock_domains: Vec<ClockDomain>,
}

impl Default for ClockResources {
    fn default() -> Self {
        Self {
            global_clocks: 8,
            plls: 1,
            dlls: 0,
            clock_domains: vec![ClockDomain {
                name: "GCLK".to_string(),
                max_frequency: 275.0e6,
            }],
        }
    }
}

/// Clock domain description
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ClockDomain {
    /// Domain name
    pub name: String,
    /// Maximum frequency in Hz
    pub max_frequency: f64,
}

/// I/O tile description
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IoTile {
    /// Tile coordinates
    pub x: u32,
    pub y: u32,
    /// Number of I/O cells in this tile
    pub io_count: u8,
    /// Side of chip (for corner/edge tiles)
    pub side: IoSide,
    /// Supported I/O standards
    pub io_standards: Vec<String>,
    /// Supported drive strengths (mA)
    pub drive_strengths: Vec<u8>,
    /// Supports differential pairs
    pub diff_pairs: bool,
}

/// I/O tile side
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum IoSide {
    Top,
    Bottom,
    Left,
    Right,
}

/// Logic tile description
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LogicTile {
    /// Tile coordinates
    pub x: u32,
    pub y: u32,
    /// Number of LUTs in this tile
    pub lut_count: u8,
    /// Number of flip-flops in this tile
    pub ff_count: u8,
    /// Has carry chain
    pub has_carry: bool,
}

/// Memory block description
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MemoryBlock {
    /// Tile coordinates
    pub x: u32,
    pub y: u32,
    /// Block size in bits
    pub size_bits: u32,
    /// Supported widths
    pub widths: Vec<u8>,
}

/// Package pin mapping
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PackagePins {
    /// Package name
    pub name: String,
    /// Pin mapping: pin name -> (tile_x, tile_y, bel_idx)
    pub pins: HashMap<String, (u32, u32, u8)>,
}

/// Device trait that all FPGA devices must implement
pub trait Device: Send + Sync {
    /// Get the device family
    fn family(&self) -> DeviceFamily;

    /// Get the device name
    fn name(&self) -> &str;

    /// Get grid dimensions (width, height)
    fn grid_size(&self) -> (u32, u32);

    /// Get device statistics
    fn stats(&self) -> DeviceStats;

    /// Get a tile at the given coordinates
    fn tile_at(&self, x: u32, y: u32) -> Option<&dyn Tile>;

    /// Get a wire by ID
    fn wire(&self, id: WireId) -> Option<&Wire>;

    /// Get a PIP by ID
    fn pip(&self, id: PipId) -> Option<&Pip>;

    /// Get wire ID by name
    fn wire_by_name(&self, name: &str) -> Option<WireId>;

    /// Get all wires in a tile
    fn tile_wires(&self, x: u32, y: u32) -> Vec<WireId>;

    /// Get all PIPs that can drive a wire (wire is destination)
    fn wire_pips(&self, wire_id: WireId) -> Vec<PipId>;

    /// Get all PIPs driven by a wire (wire is source) - for forward routing
    fn wire_src_pips(&self, wire_id: WireId) -> Vec<PipId>;

    /// Get package pin mappings
    fn packages(&self) -> &HashMap<String, PackagePins>;

    /// Get the routing architecture
    fn routing(&self) -> &RoutingArchitecture;

    /// Get clock resources
    fn clock_resources(&self) -> &ClockResources;

    /// Check if a cell type can be placed on a BEL type
    fn can_place(&self, cell_type: &str, bel_type: BelType) -> bool;

    // BEL pin wire methods (optional, for proper routing endpoints)

    /// Get wire ID for a LUT output pin
    /// lc_idx is 0-7 for the 8 logic cells in a tile
    fn lut_output_wire(&self, _tile_x: u32, _tile_y: u32, _lc_idx: usize) -> Option<WireId> {
        None
    }

    /// Get wire ID for a LUT input pin
    /// lc_idx is 0-7, input_idx is 0-3 for the 4 inputs
    fn lut_input_wire(
        &self,
        _tile_x: u32,
        _tile_y: u32,
        _lc_idx: usize,
        _input_idx: usize,
    ) -> Option<WireId> {
        None
    }

    /// Get wire ID for the global clock signal in a logic tile
    fn clock_wire(&self, _tile_x: u32, _tile_y: u32) -> Option<WireId> {
        None
    }

    /// Get wire ID for I/O data output
    /// iob_idx is 0 or 1 for the two I/O blocks in an I/O tile
    fn io_output_wire(&self, _tile_x: u32, _tile_y: u32, _iob_idx: usize) -> Option<WireId> {
        None
    }

    /// Get wire ID for I/O data input (signal coming from pad into fabric)
    fn io_input_wire(&self, _tile_x: u32, _tile_y: u32, _iob_idx: usize) -> Option<WireId> {
        None
    }
}

/// Tile trait
pub trait Tile: Send + Sync {
    /// Get tile type
    fn tile_type(&self) -> TileType;

    /// Get tile coordinates
    fn coords(&self) -> (u32, u32);

    /// Get BELs in this tile
    fn bels(&self) -> &[Bel];

    /// Get BEL by index
    fn bel(&self, idx: usize) -> Option<&Bel>;

    /// Get number of available BELs of a given type
    fn available_bels(&self, bel_type: BelType) -> usize;
}

/// Tile type enumeration
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum TileType {
    /// Logic tile with LUTs and FFs
    Logic,
    /// I/O tile (top)
    IoTop,
    /// I/O tile (bottom)
    IoBottom,
    /// I/O tile (left)
    IoLeft,
    /// I/O tile (right)
    IoRight,
    /// Block RAM tile (top half)
    RamTop,
    /// Block RAM tile (bottom half)
    RamBottom,
    /// DSP tile
    Dsp,
    /// PLL tile
    Pll,
    /// Global buffer tile
    GlobalBuf,
    /// IP connection tile
    IpCon,
    /// Empty/unused tile
    Empty,
}

/// BEL type enumeration
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum BelType {
    /// 4-input LUT
    Lut4,
    /// 6-input LUT (for larger FPGAs)
    Lut6,
    /// D flip-flop
    Dff,
    /// D flip-flop with enable
    DffE,
    /// D flip-flop with sync reset
    DffSr,
    /// D flip-flop with enable and sync reset
    DffSrE,
    /// Carry chain cell
    Carry,
    /// Block RAM slice
    RamSlice,
    /// I/O cell
    IoCell,
    /// Global clock buffer
    GlobalBuf,
    /// PLL
    Pll,
    /// DSP slice
    DspSlice,
}

impl BelType {
    /// Check if this BEL type can implement a DFF
    pub fn is_ff(&self) -> bool {
        matches!(
            self,
            BelType::Dff | BelType::DffE | BelType::DffSr | BelType::DffSrE
        )
    }

    /// Check if this BEL type is a LUT
    pub fn is_lut(&self) -> bool {
        matches!(self, BelType::Lut4 | BelType::Lut6)
    }
}

/// Basic Element of Logic (BEL)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Bel {
    /// BEL ID
    pub id: BelId,
    /// BEL type
    pub bel_type: BelType,
    /// BEL name (e.g., "LUT_0", "FF_3")
    pub name: String,
    /// BEL pins
    pub pins: Vec<BelPin>,
}

impl Bel {
    /// Get a pin by name
    pub fn pin(&self, name: &str) -> Option<&BelPin> {
        self.pins.iter().find(|p| p.name == name)
    }

    /// Get input pins
    pub fn input_pins(&self) -> impl Iterator<Item = &BelPin> {
        self.pins
            .iter()
            .filter(|p| p.direction == PinDirection::Input)
    }

    /// Get output pins
    pub fn output_pins(&self) -> impl Iterator<Item = &BelPin> {
        self.pins
            .iter()
            .filter(|p| p.direction == PinDirection::Output)
    }
}

/// BEL pin
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BelPin {
    /// Pin name
    pub name: String,
    /// Pin direction
    pub direction: PinDirection,
    /// Connected wire (in the tile's local routing)
    pub wire: Option<WireId>,
}

/// Pin direction
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum PinDirection {
    Input,
    Output,
    Inout,
}

/// Wire description
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Wire {
    /// Wire ID
    pub id: WireId,
    /// Wire name
    pub name: String,
    /// Wire type
    pub wire_type: WireType,
    /// Tile coordinates
    pub tile_x: u32,
    pub tile_y: u32,
    /// Intrinsic delay (ps)
    pub delay: u32,
}

/// Wire type
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum WireType {
    /// Local wire within a tile
    Local(u8),
    /// Horizontal span-4 wire
    Span4H(u8),
    /// Vertical span-4 wire
    Span4V(u8),
    /// Horizontal span-12 wire (longer distance)
    Span12H(u8),
    /// Vertical span-12 wire (longer distance)
    Span12V(u8),
    /// Global clock wire
    Global(u8),
    /// Neighbor wire (to adjacent tile)
    Neighbour,
    /// Carry chain wire
    CarryChain,
    /// BEL pin wire
    BelPin,
}

/// Programmable Interconnect Point (PIP)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Pip {
    /// PIP ID
    pub id: PipId,
    /// Source wire
    pub src_wire: WireId,
    /// Destination wire
    pub dst_wire: WireId,
    /// PIP delay (ps)
    pub delay: u32,
    /// Is this a configurable PIP (vs hardwired)?
    pub configurable: bool,
    /// Tile coordinates
    pub tile_x: u32,
    pub tile_y: u32,
}

impl Pip {
    /// Get the cost of using this PIP (for routing)
    pub fn cost(&self) -> u32 {
        // Base cost plus delay-proportional cost
        1 + self.delay / 100
    }
}
