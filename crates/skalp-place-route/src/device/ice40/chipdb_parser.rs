//! IceStorm Chipdb Parser
//!
//! Parses the text-based chipdb format from Project IceStorm.
//! This provides real device data for iCE40 FPGAs including:
//! - Tile grid layout
//! - Wire definitions
//! - PIP (routing) connections with configuration bits
//! - Package pin mappings
//!
//! License: The chipdb data from Project IceStorm is licensed under ISC license.
//! See: https://github.com/YosysHQ/icestorm

use super::{Ice40Variant, TileType};
use crate::device::{Bel, BelId, BelType, Pip, PipId, Wire, WireId, WireType};
use std::collections::HashMap;

/// Parsed chipdb data
#[derive(Debug, Clone)]
pub struct ChipDb {
    /// Device name (e.g., "1k", "8k")
    pub device_name: String,
    /// Grid width
    pub width: u32,
    /// Grid height
    pub height: u32,
    /// Number of wires
    pub num_wires: u32,
    /// Tile grid: (x, y) -> tile type
    pub tiles: HashMap<(u32, u32), TileType>,
    /// Wire definitions: wire_id -> (segments)
    pub wires: HashMap<u32, WireInfo>,
    /// PIPs: list of routing connections
    pub pips: Vec<PipInfo>,
    /// Package pins: package_name -> pin_mappings
    pub packages: HashMap<String, Vec<PinMapping>>,
    /// Tile configuration bits
    pub tile_bits: HashMap<TileType, Vec<ConfigBit>>,
    /// Global buffer inputs
    pub gbufin: Vec<GbufIn>,
    /// Tile bit dimensions by type
    pub tile_dimensions: HashMap<TileType, TileBitDimensions>,
    /// LC bit mappings (for logic tiles)
    pub lc_mappings: Vec<LcBitMapping>,
}

/// Wire information
#[derive(Debug, Clone)]
pub struct WireInfo {
    /// Wire ID
    pub id: u32,
    /// Wire segments: (x, y, name)
    pub segments: Vec<(u32, u32, String)>,
    /// Wire type (derived from name)
    pub wire_type: WireType,
}

/// PIP (programmable interconnect point) information
#[derive(Debug, Clone)]
pub struct PipInfo {
    /// Tile X
    pub tile_x: u32,
    /// Tile Y
    pub tile_y: u32,
    /// Source wire ID
    pub src_wire: u32,
    /// Destination wire ID
    pub dst_wire: u32,
    /// Configuration bits to enable this PIP
    pub config_bits: Vec<ConfigBitRef>,
    /// Is this a buffer (vs routing mux)?
    pub is_buffer: bool,
    /// Delay in picoseconds
    pub delay_ps: u32,
}

/// Reference to a configuration bit
#[derive(Debug, Clone)]
pub struct ConfigBitRef {
    /// Bit row (B0-B15)
    pub row: u8,
    /// Bit column within row
    pub col: u8,
    /// Inverted (prefixed with !)
    pub inverted: bool,
}

/// Configuration bit definition
#[derive(Debug, Clone)]
pub struct ConfigBit {
    /// Bit name
    pub name: String,
    /// Bit row
    pub row: u8,
    /// Bit column
    pub col: u8,
}

/// Tile bit dimensions
#[derive(Debug, Clone, Copy)]
pub struct TileBitDimensions {
    /// Number of columns
    pub columns: u8,
    /// Number of rows
    pub rows: u8,
}

/// LUT bit mapping (for LC_0 through LC_7)
#[derive(Debug, Clone)]
pub struct LcBitMapping {
    /// LC index (0-7)
    pub lc_idx: u8,
    /// Bit positions as (row, col) pairs for the 16-bit LUT init
    /// First 10 bits from one row pair, next 10 from another
    pub bit_positions: Vec<(u8, u8)>,
}

/// Package pin mapping
#[derive(Debug, Clone)]
pub struct PinMapping {
    /// Pin name (e.g., "A1", "B2")
    pub pin_name: String,
    /// Tile X coordinate
    pub tile_x: u32,
    /// Tile Y coordinate
    pub tile_y: u32,
    /// BEL index within tile
    pub bel_idx: u8,
}

/// Global buffer input
#[derive(Debug, Clone)]
pub struct GbufIn {
    /// Tile X
    pub tile_x: u32,
    /// Tile Y
    pub tile_y: u32,
    /// Global network index (0-7)
    pub glb_num: u8,
}

/// Current buffer/routing context for multi-line parsing
#[derive(Debug, Clone)]
struct BufferContext {
    tile_x: u32,
    tile_y: u32,
    dst_wire: u32,
    config_bits: Vec<String>,
    is_buffer: bool,
}

impl ChipDb {
    /// Parse chipdb from text content
    pub fn parse(content: &str) -> Result<Self, String> {
        let mut chipdb = ChipDb {
            device_name: String::new(),
            width: 0,
            height: 0,
            num_wires: 0,
            tiles: HashMap::new(),
            wires: HashMap::new(),
            pips: Vec::new(),
            packages: HashMap::new(),
            tile_bits: HashMap::new(),
            gbufin: Vec::new(),
            tile_dimensions: HashMap::new(),
            lc_mappings: Vec::new(),
        };

        let mut current_section = Section::None;
        let mut current_wire_id: Option<u32> = None;
        let mut current_package: Option<String> = None;
        let mut current_tile_type: Option<TileType> = None;
        let mut current_buffer: Option<BufferContext> = None;

        for line in content.lines() {
            let line = line.trim();
            if line.is_empty() || line.starts_with('#') {
                continue;
            }

            // Check for section markers
            if line.starts_with('.') {
                // Finish any pending buffer section
                current_buffer = None;

                let parts: Vec<&str> = line.split_whitespace().collect();
                match parts[0] {
                    ".device" => {
                        if parts.len() >= 4 {
                            chipdb.device_name = parts[1].to_string();
                            chipdb.width = parts[2].parse().unwrap_or(0);
                            chipdb.height = parts[3].parse().unwrap_or(0);
                            if parts.len() >= 5 {
                                chipdb.num_wires = parts[4].parse().unwrap_or(0);
                            }
                        }
                        current_section = Section::None;
                    }
                    ".logic_tile" | ".io_tile" | ".ramb_tile" | ".ramt_tile" | ".dsp0_tile"
                    | ".dsp1_tile" | ".dsp2_tile" | ".dsp3_tile" | ".ipcon_tile" => {
                        if parts.len() >= 3 {
                            let x: u32 = parts[1].parse().unwrap_or(0);
                            let y: u32 = parts[2].parse().unwrap_or(0);
                            let tile_type = match parts[0] {
                                ".logic_tile" => TileType::Logic,
                                ".io_tile" => {
                                    // Determine IO position based on coordinates
                                    if y == 0 {
                                        TileType::IoBottom
                                    } else if y == chipdb.height - 1 {
                                        TileType::IoTop
                                    } else if x == 0 {
                                        TileType::IoLeft
                                    } else {
                                        TileType::IoRight
                                    }
                                }
                                ".ramb_tile" => TileType::RamBottom,
                                ".ramt_tile" => TileType::RamTop,
                                ".dsp0_tile" | ".dsp1_tile" | ".dsp2_tile" | ".dsp3_tile" => {
                                    TileType::Dsp
                                }
                                ".ipcon_tile" => TileType::Pll,
                                _ => TileType::Empty,
                            };
                            chipdb.tiles.insert((x, y), tile_type);
                        }
                        current_section = Section::None;
                    }
                    ".net" => {
                        if parts.len() >= 2 {
                            let wire_id: u32 = parts[1].parse().unwrap_or(0);
                            current_wire_id = Some(wire_id);
                            chipdb.wires.insert(
                                wire_id,
                                WireInfo {
                                    id: wire_id,
                                    segments: Vec::new(),
                                    wire_type: WireType::Local(0),
                                },
                            );
                            current_section = Section::Net;
                        }
                    }
                    ".buffer" | ".routing" => {
                        // Format: .buffer X Y DST_NET CONFIG_BIT_NAMES...
                        // Followed by lines: CONFIG_BIT_VALUES SRC_NET
                        if parts.len() >= 4 {
                            let tile_x: u32 = parts[1].parse().unwrap_or(0);
                            let tile_y: u32 = parts[2].parse().unwrap_or(0);
                            let dst_wire: u32 = parts[3].parse().unwrap_or(0);
                            let is_buffer = parts[0] == ".buffer";

                            // Collect config bit names
                            let config_bits: Vec<String> =
                                parts[4..].iter().map(|s| s.to_string()).collect();

                            current_buffer = Some(BufferContext {
                                tile_x,
                                tile_y,
                                dst_wire,
                                config_bits,
                                is_buffer,
                            });
                            current_section = Section::Buffer;
                        }
                    }
                    ".pins" => {
                        if parts.len() >= 2 {
                            current_package = Some(parts[1].to_string());
                            chipdb.packages.insert(parts[1].to_string(), Vec::new());
                            current_section = Section::Pins;
                        }
                    }
                    ".gbufin" => {
                        current_section = Section::GbufIn;
                    }
                    ".logic_tile_bits" => {
                        current_tile_type = Some(TileType::Logic);
                        current_section = Section::TileBits;
                        // Parse dimensions: .logic_tile_bits COLS ROWS
                        if parts.len() >= 3 {
                            let cols: u8 = parts[1].parse().unwrap_or(54);
                            let rows: u8 = parts[2].parse().unwrap_or(16);
                            chipdb.tile_dimensions.insert(
                                TileType::Logic,
                                TileBitDimensions {
                                    columns: cols,
                                    rows,
                                },
                            );
                        }
                    }
                    ".io_tile_bits" => {
                        current_tile_type = Some(TileType::IoTop);
                        current_section = Section::TileBits;
                        if parts.len() >= 3 {
                            let cols: u8 = parts[1].parse().unwrap_or(18);
                            let rows: u8 = parts[2].parse().unwrap_or(16);
                            chipdb.tile_dimensions.insert(
                                TileType::IoTop,
                                TileBitDimensions {
                                    columns: cols,
                                    rows,
                                },
                            );
                            chipdb.tile_dimensions.insert(
                                TileType::IoBottom,
                                TileBitDimensions {
                                    columns: cols,
                                    rows,
                                },
                            );
                            chipdb.tile_dimensions.insert(
                                TileType::IoLeft,
                                TileBitDimensions {
                                    columns: cols,
                                    rows,
                                },
                            );
                            chipdb.tile_dimensions.insert(
                                TileType::IoRight,
                                TileBitDimensions {
                                    columns: cols,
                                    rows,
                                },
                            );
                        }
                    }
                    ".ramb_tile_bits" => {
                        current_tile_type = Some(TileType::RamBottom);
                        current_section = Section::TileBits;
                        if parts.len() >= 3 {
                            let cols: u8 = parts[1].parse().unwrap_or(42);
                            let rows: u8 = parts[2].parse().unwrap_or(16);
                            chipdb.tile_dimensions.insert(
                                TileType::RamBottom,
                                TileBitDimensions {
                                    columns: cols,
                                    rows,
                                },
                            );
                        }
                    }
                    ".ramt_tile_bits" => {
                        current_tile_type = Some(TileType::RamTop);
                        current_section = Section::TileBits;
                        if parts.len() >= 3 {
                            let cols: u8 = parts[1].parse().unwrap_or(42);
                            let rows: u8 = parts[2].parse().unwrap_or(16);
                            chipdb.tile_dimensions.insert(
                                TileType::RamTop,
                                TileBitDimensions {
                                    columns: cols,
                                    rows,
                                },
                            );
                        }
                    }
                    _ => {
                        current_section = Section::None;
                    }
                }
                continue;
            }

            // Process data lines based on current section
            match current_section {
                Section::Net => {
                    if let Some(wire_id) = current_wire_id {
                        let parts: Vec<&str> = line.split_whitespace().collect();
                        if parts.len() >= 3 {
                            let x: u32 = parts[0].parse().unwrap_or(0);
                            let y: u32 = parts[1].parse().unwrap_or(0);
                            let name = parts[2].to_string();

                            if let Some(wire) = chipdb.wires.get_mut(&wire_id) {
                                wire.wire_type = wire_type_from_name(&name);
                                wire.segments.push((x, y, name));
                            }
                        }
                    }
                }
                Section::Buffer => {
                    if let Some(ref ctx) = current_buffer {
                        // Format: CONFIG_BIT_VALUES SRC_NET
                        let parts: Vec<&str> = line.split_whitespace().collect();
                        if parts.len() >= 2 {
                            let bit_values = parts[0];
                            let src_wire: u32 = parts[1].parse().unwrap_or(0);

                            // Parse config bits for this MUX option
                            let config_bits: Vec<ConfigBitRef> = ctx
                                .config_bits
                                .iter()
                                .zip(bit_values.chars())
                                .filter_map(|(name, value)| {
                                    let bit_ref = parse_config_bit_ref(name)?;
                                    // Only include bits that are set (value='1')
                                    if value == '1' {
                                        Some(bit_ref)
                                    } else {
                                        // For inverted bits (value='0'), create inverted ref
                                        Some(ConfigBitRef {
                                            row: bit_ref.row,
                                            col: bit_ref.col,
                                            inverted: value == '0',
                                        })
                                    }
                                })
                                .collect();

                            chipdb.pips.push(PipInfo {
                                tile_x: ctx.tile_x,
                                tile_y: ctx.tile_y,
                                src_wire,
                                dst_wire: ctx.dst_wire,
                                config_bits,
                                is_buffer: ctx.is_buffer,
                                delay_ps: if ctx.is_buffer { 50 } else { 100 },
                            });
                        }
                    }
                }
                Section::Pins => {
                    if let Some(ref pkg) = current_package {
                        let parts: Vec<&str> = line.split_whitespace().collect();
                        if parts.len() >= 4 {
                            let pin_name = parts[0].to_string();
                            let tile_x: u32 = parts[1].parse().unwrap_or(0);
                            let tile_y: u32 = parts[2].parse().unwrap_or(0);
                            let bel_idx: u8 = parts[3].parse().unwrap_or(0);

                            if let Some(pins) = chipdb.packages.get_mut(pkg) {
                                pins.push(PinMapping {
                                    pin_name,
                                    tile_x,
                                    tile_y,
                                    bel_idx,
                                });
                            }
                        }
                    }
                }
                Section::GbufIn => {
                    let parts: Vec<&str> = line.split_whitespace().collect();
                    if parts.len() >= 3 {
                        let tile_x: u32 = parts[0].parse().unwrap_or(0);
                        let tile_y: u32 = parts[1].parse().unwrap_or(0);
                        let glb_num: u8 = parts[2].parse().unwrap_or(0);
                        chipdb.gbufin.push(GbufIn {
                            tile_x,
                            tile_y,
                            glb_num,
                        });
                    }
                }
                Section::TileBits => {
                    if let Some(tile_type) = current_tile_type {
                        let parts: Vec<&str> = line.split_whitespace().collect();
                        if parts.len() >= 2 {
                            let name = parts[0].to_string();

                            // Check if this is an LC_* line (has many bits)
                            if name.starts_with("LC_") && parts.len() > 2 {
                                // Parse LC index
                                if let Ok(lc_idx) = name[3..].parse::<u8>() {
                                    // Parse all bit positions
                                    let bit_positions: Vec<(u8, u8)> = parts[1..]
                                        .iter()
                                        .filter_map(|s| {
                                            let bit_ref = parse_config_bit_ref(s)?;
                                            Some((bit_ref.row, bit_ref.col))
                                        })
                                        .collect();

                                    chipdb.lc_mappings.push(LcBitMapping {
                                        lc_idx,
                                        bit_positions,
                                    });
                                }
                            } else if let Some(bit_ref) = parse_config_bit_ref(parts[1]) {
                                let bits = chipdb.tile_bits.entry(tile_type).or_default();
                                bits.push(ConfigBit {
                                    name,
                                    row: bit_ref.row,
                                    col: bit_ref.col,
                                });
                            }
                        }
                    }
                }
                Section::None => {}
            }
        }

        Ok(chipdb)
    }

    /// Load embedded chipdb for a variant
    pub fn load_embedded(variant: Ice40Variant) -> Result<Self, String> {
        let content = match variant {
            Ice40Variant::Hx1k | Ice40Variant::Lp1k => include_str!("chipdb/chipdb-1k.txt"),
            Ice40Variant::Hx4k | Ice40Variant::Lp4k => include_str!("chipdb/chipdb-5k.txt"),
            Ice40Variant::Hx8k | Ice40Variant::Lp8k => include_str!("chipdb/chipdb-8k.txt"),
            Ice40Variant::Up5k => include_str!("chipdb/chipdb-5k.txt"),
        };
        Self::parse(content)
    }

    /// Build wires for the device
    pub fn build_wires(&self) -> Vec<Wire> {
        self.wires
            .values()
            .map(|w| {
                let name = w
                    .segments
                    .first()
                    .map(|(_, _, n)| n.clone())
                    .unwrap_or_else(|| format!("wire_{}", w.id));
                // Estimate delay based on wire type
                let delay = match w.wire_type {
                    WireType::Local(_) => 50,
                    WireType::Span4H(_) | WireType::Span4V(_) => 200,
                    WireType::Span12H(_) | WireType::Span12V(_) => 400,
                    WireType::Global(_) => 100,
                    WireType::Neighbour => 100,
                    WireType::CarryChain => 30,
                    WireType::BelPin => 10,
                };
                Wire {
                    id: WireId(w.id),
                    name,
                    wire_type: w.wire_type,
                    tile_x: w.segments.first().map(|(x, _, _)| *x).unwrap_or(0),
                    tile_y: w.segments.first().map(|(_, y, _)| *y).unwrap_or(0),
                    delay,
                }
            })
            .collect()
    }

    /// Build PIPs for the device
    pub fn build_pips(&self) -> Vec<Pip> {
        self.pips
            .iter()
            .enumerate()
            .map(|(idx, p)| Pip {
                id: PipId(idx as u32),
                src_wire: WireId(p.src_wire),
                dst_wire: WireId(p.dst_wire),
                tile_x: p.tile_x,
                tile_y: p.tile_y,
                delay: p.delay_ps,
                configurable: true,
            })
            .collect()
    }

    /// Build BELs for a tile
    pub fn build_bels_for_tile(&self, x: u32, y: u32) -> Vec<Bel> {
        let tile_type = self.tiles.get(&(x, y)).copied().unwrap_or(TileType::Empty);
        let mut bels = Vec::new();
        let mut bel_id = x * 1000 + y * 10; // Unique ID based on position

        match tile_type {
            TileType::Logic => {
                // 8 logic cells per logic tile (each with LUT4 + DFF)
                for i in 0..8 {
                    // LUT4
                    bels.push(Bel {
                        id: BelId(bel_id),
                        name: format!("LC_{}", i),
                        bel_type: BelType::Lut4,
                        pins: vec![],
                    });
                    bel_id += 1;

                    // DFF
                    bels.push(Bel {
                        id: BelId(bel_id),
                        name: format!("DFF_{}", i),
                        bel_type: BelType::Dff,
                        pins: vec![],
                    });
                    bel_id += 1;
                }

                // Carry chain
                bels.push(Bel {
                    id: BelId(bel_id),
                    name: "CARRY".to_string(),
                    bel_type: BelType::Carry,
                    pins: vec![],
                });
            }
            TileType::IoTop | TileType::IoBottom | TileType::IoLeft | TileType::IoRight => {
                // 2 I/O cells per I/O tile
                for i in 0..2 {
                    bels.push(Bel {
                        id: BelId(bel_id),
                        name: format!("IO_{}", i),
                        bel_type: BelType::IoCell,
                        pins: vec![],
                    });
                    bel_id += 1;
                }
            }
            TileType::RamTop | TileType::RamBottom => {
                bels.push(Bel {
                    id: BelId(bel_id),
                    name: "RAM".to_string(),
                    bel_type: BelType::RamSlice,
                    pins: vec![],
                });
            }
            TileType::Pll => {
                bels.push(Bel {
                    id: BelId(bel_id),
                    name: "PLL".to_string(),
                    bel_type: BelType::Pll,
                    pins: vec![],
                });
            }
            _ => {}
        }

        bels
    }
}

/// Current parsing section
#[derive(Debug, Clone, Copy, PartialEq)]
enum Section {
    None,
    Net,
    Buffer,
    Pins,
    GbufIn,
    TileBits,
}

/// Parse a config bit reference like "B1[5]" or "!B0[3]"
fn parse_config_bit_ref(s: &str) -> Option<ConfigBitRef> {
    let inverted = s.starts_with('!');
    let s = s.trim_start_matches('!');

    if !s.starts_with('B') {
        return None;
    }

    let s = &s[1..]; // Skip 'B'

    // Find '[' and ']'
    let bracket_start = s.find('[')?;
    let bracket_end = s.find(']')?;

    let row: u8 = s[..bracket_start].parse().ok()?;
    let col: u8 = s[bracket_start + 1..bracket_end].parse().ok()?;

    Some(ConfigBitRef { row, col, inverted })
}

/// Derive wire type from wire name
fn wire_type_from_name(name: &str) -> WireType {
    if name.starts_with("sp4_h") || name.starts_with("span4_horz") {
        WireType::Span4H(0)
    } else if name.starts_with("sp4_v") || name.starts_with("span4_vert") {
        WireType::Span4V(0)
    } else if name.starts_with("sp12_h") || name.starts_with("span12_horz") {
        WireType::Span12H(0)
    } else if name.starts_with("sp12_v") || name.starts_with("span12_vert") {
        WireType::Span12V(0)
    } else if name.starts_with("glb_netwk") || name.starts_with("global") {
        let num = name
            .chars()
            .last()
            .and_then(|c| c.to_digit(10))
            .unwrap_or(0) as u8;
        WireType::Global(num)
    } else if name.starts_with("neigh") || name.starts_with("logic_op") {
        WireType::Neighbour
    } else if name.starts_with("carry") || name.starts_with("cout") {
        WireType::CarryChain
    } else {
        // Default to local wire (including lutff, lc_, local prefixes)
        WireType::Local(0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_config_bit_ref() {
        let bit = parse_config_bit_ref("B1[5]").unwrap();
        assert_eq!(bit.row, 1);
        assert_eq!(bit.col, 5);
        assert!(!bit.inverted);

        let bit = parse_config_bit_ref("!B0[12]").unwrap();
        assert_eq!(bit.row, 0);
        assert_eq!(bit.col, 12);
        assert!(bit.inverted);
    }

    #[test]
    fn test_wire_type_from_name() {
        assert!(matches!(
            wire_type_from_name("span4_horz_3"),
            WireType::Span4H(_)
        ));
        assert!(matches!(
            wire_type_from_name("glb_netwk_0"),
            WireType::Global(_)
        ));
        assert!(matches!(
            wire_type_from_name("carry_in"),
            WireType::CarryChain
        ));
    }

    #[test]
    fn test_load_embedded_1k() {
        let chipdb = ChipDb::load_embedded(Ice40Variant::Hx1k).unwrap();
        assert_eq!(chipdb.device_name, "1k");
        assert_eq!(chipdb.width, 14);
        assert_eq!(chipdb.height, 18);
        assert!(chipdb.num_wires > 0);
        assert!(!chipdb.tiles.is_empty());
        assert!(!chipdb.wires.is_empty());
        assert!(!chipdb.pips.is_empty());
        assert!(!chipdb.packages.is_empty());

        // Check that we have logic tiles
        let logic_tiles: Vec<_> = chipdb
            .tiles
            .iter()
            .filter(|(_, t)| **t == TileType::Logic)
            .collect();
        assert!(!logic_tiles.is_empty(), "Should have logic tiles");

        // Check that we can build wires
        let wires = chipdb.build_wires();
        assert!(!wires.is_empty(), "Should have wires");

        // Check that we can build PIPs
        let pips = chipdb.build_pips();
        assert!(!pips.is_empty(), "Should have PIPs");
    }

    #[test]
    fn test_load_embedded_8k() {
        let chipdb = ChipDb::load_embedded(Ice40Variant::Hx8k).unwrap();
        assert_eq!(chipdb.device_name, "8k");
        // 8k is larger than 1k
        assert!(chipdb.num_wires > 27000);
    }
}
