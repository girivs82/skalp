//! iCE40 FPGA device database
//!
//! This module provides the device database for Lattice iCE40 FPGAs,
//! supporting HX1K, HX4K, HX8K, LP1K, LP8K, and UP5K variants.

mod chipdb;
pub mod chipdb_parser;
mod tiles;

pub use chipdb::Ice40ChipDb;
pub use chipdb_parser::ChipDb;
pub use tiles::Ice40Tile;

use super::{
    Bel, BelId, BelPin, BelType, ClockResources, Device, DeviceFamily, DeviceStats, IoSide, IoTile,
    LogicTile, MemoryBlock, PackagePins, PinDirection, Pip, PipId, RoutingArchitecture,
    SwitchPattern, Tile, TileType, Wire, WireDirection, WireId, WireSegment, WireType,
};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// iCE40 device variants
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Ice40Variant {
    /// HX1K - 1280 LUTs
    Hx1k,
    /// HX4K - 3520 LUTs
    Hx4k,
    /// HX8K - 7680 LUTs
    Hx8k,
    /// LP1K - 1280 LUTs (low power)
    Lp1k,
    /// LP4K - 3520 LUTs (low power)
    Lp4k,
    /// LP8K - 7680 LUTs (low power)
    Lp8k,
    /// UP5K - 5280 LUTs (ultra low power, has DSP)
    Up5k,
}

impl Ice40Variant {
    /// Get grid dimensions for this variant
    pub fn grid_size(&self) -> (u32, u32) {
        match self {
            Ice40Variant::Hx1k | Ice40Variant::Lp1k => (13, 17),
            Ice40Variant::Hx4k | Ice40Variant::Lp4k => (17, 17),
            Ice40Variant::Hx8k | Ice40Variant::Lp8k => (33, 33),
            Ice40Variant::Up5k => (25, 21),
        }
    }

    /// Get the device name as a string
    pub fn name(&self) -> &'static str {
        match self {
            Ice40Variant::Hx1k => "ice40hx1k",
            Ice40Variant::Hx4k => "ice40hx4k",
            Ice40Variant::Hx8k => "ice40hx8k",
            Ice40Variant::Lp1k => "ice40lp1k",
            Ice40Variant::Lp4k => "ice40lp4k",
            Ice40Variant::Lp8k => "ice40lp8k",
            Ice40Variant::Up5k => "ice40up5k",
        }
    }

    /// Get number of LUTs for this variant
    pub fn lut_count(&self) -> usize {
        match self {
            Ice40Variant::Hx1k | Ice40Variant::Lp1k => 1280,
            Ice40Variant::Hx4k | Ice40Variant::Lp4k => 3520,
            Ice40Variant::Hx8k | Ice40Variant::Lp8k => 7680,
            Ice40Variant::Up5k => 5280,
        }
    }

    /// Get number of block RAMs for this variant
    pub fn bram_count(&self) -> usize {
        match self {
            Ice40Variant::Hx1k | Ice40Variant::Lp1k => 16,
            Ice40Variant::Hx4k | Ice40Variant::Lp4k => 20,
            Ice40Variant::Hx8k | Ice40Variant::Lp8k => 32,
            Ice40Variant::Up5k => 30,
        }
    }

    /// Check if this variant has DSP blocks
    pub fn has_dsp(&self) -> bool {
        matches!(self, Ice40Variant::Up5k)
    }
}

impl std::fmt::Display for Ice40Variant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name())
    }
}

/// iCE40 device implementation
#[derive(Debug, Clone)]
pub struct Ice40Device {
    /// Device variant
    pub variant: Ice40Variant,
    /// Grid dimensions
    grid_size: (u32, u32),
    /// Tiles indexed by (x, y)
    tiles: Vec<Vec<Option<Ice40Tile>>>,
    /// All wires
    wires: Vec<Wire>,
    /// Wire name to ID mapping
    wire_names: HashMap<String, WireId>,
    /// All PIPs
    pips: Vec<Pip>,
    /// Wire to PIP mapping (PIPs that drive this wire)
    wire_to_pips: HashMap<WireId, Vec<PipId>>,
    /// Tile to wire mapping
    tile_wires: HashMap<(u32, u32), Vec<WireId>>,
    /// Package pin mappings
    packages: HashMap<String, PackagePins>,
    /// Routing architecture
    routing: RoutingArchitecture,
    /// Clock resources
    clock_resources: ClockResources,
    /// Logic tiles
    pub logic_tiles: Vec<LogicTile>,
    /// I/O tiles
    pub io_tiles: Vec<IoTile>,
    /// Memory blocks
    pub memory_blocks: Vec<MemoryBlock>,
}

impl Ice40Device {
    /// Create a new iCE40 device with default architecture
    pub fn new(variant: Ice40Variant) -> Self {
        // Try to load from real chipdb first
        if let Ok(device) = Self::from_chipdb(variant) {
            return device;
        }
        // Fall back to synthetic architecture
        Self::new_synthetic(variant)
    }

    /// Create from real IceStorm chipdb data
    pub fn from_chipdb(variant: Ice40Variant) -> Result<Self, String> {
        let chipdb = chipdb_parser::ChipDb::load_embedded(variant)?;

        let grid_size = (chipdb.width, chipdb.height);

        let mut device = Self {
            variant,
            grid_size,
            tiles: Vec::new(),
            wires: Vec::new(),
            wire_names: HashMap::new(),
            pips: Vec::new(),
            wire_to_pips: HashMap::new(),
            tile_wires: HashMap::new(),
            packages: HashMap::new(),
            routing: Self::default_routing(),
            clock_resources: Self::default_clock_resources(),
            logic_tiles: Vec::new(),
            io_tiles: Vec::new(),
            memory_blocks: Vec::new(),
        };

        // Build wires from chipdb
        for wire in chipdb.build_wires() {
            device.wire_names.insert(wire.name.clone(), wire.id);
            // Track tile wires
            device
                .tile_wires
                .entry((wire.tile_x, wire.tile_y))
                .or_default()
                .push(wire.id);
            device.wires.push(wire);
        }

        // Build PIPs from chipdb
        for pip in chipdb.build_pips() {
            device
                .wire_to_pips
                .entry(pip.dst_wire)
                .or_default()
                .push(pip.id);
            device.pips.push(pip);
        }

        // Build tiles from chipdb
        let (width, height) = grid_size;
        device.tiles = vec![vec![None; width as usize]; height as usize];

        let mut bel_id = 0u32;
        for (&(x, y), &tile_type) in &chipdb.tiles {
            let bels = chipdb.build_bels_for_tile(x, y);
            bel_id += bels.len() as u32;
            let tile = Ice40Tile::new(tile_type, x, y, bels);

            // Track tile types
            match tile_type {
                TileType::Logic => {
                    device.logic_tiles.push(LogicTile {
                        x,
                        y,
                        lut_count: 8,
                        ff_count: 8,
                        has_carry: true,
                    });
                }
                TileType::IoTop | TileType::IoBottom | TileType::IoLeft | TileType::IoRight => {
                    let side = match tile_type {
                        TileType::IoTop => IoSide::Top,
                        TileType::IoBottom => IoSide::Bottom,
                        TileType::IoLeft => IoSide::Left,
                        TileType::IoRight => IoSide::Right,
                        _ => unreachable!(),
                    };
                    device.io_tiles.push(IoTile {
                        x,
                        y,
                        io_count: 2,
                        side,
                        io_standards: vec![
                            "LVCMOS33".to_string(),
                            "LVCMOS25".to_string(),
                            "LVCMOS18".to_string(),
                        ],
                        drive_strengths: vec![4, 8, 12],
                        diff_pairs: false,
                    });
                }
                TileType::RamTop => {
                    device.memory_blocks.push(MemoryBlock {
                        x,
                        y,
                        size_bits: 4096,
                        widths: vec![1, 2, 4, 8, 16],
                    });
                }
                _ => {}
            }

            if (y as usize) < device.tiles.len() && (x as usize) < device.tiles[y as usize].len() {
                device.tiles[y as usize][x as usize] = Some(tile);
            }
        }

        // Load package pins from chipdb
        for (pkg_name, pins) in &chipdb.packages {
            let mut pin_map = HashMap::new();
            for pin in pins {
                pin_map.insert(pin.pin_name.clone(), (pin.tile_x, pin.tile_y, pin.bel_idx));
            }
            device.packages.insert(
                pkg_name.clone(),
                PackagePins {
                    name: pkg_name.clone(),
                    pins: pin_map,
                },
            );
        }

        // Suppress unused variable warning
        let _ = bel_id;

        Ok(device)
    }

    /// Create a new iCE40 device with synthetic architecture (fallback)
    fn new_synthetic(variant: Ice40Variant) -> Self {
        let grid_size = variant.grid_size();
        let mut device = Self {
            variant,
            grid_size,
            tiles: Vec::new(),
            wires: Vec::new(),
            wire_names: HashMap::new(),
            pips: Vec::new(),
            wire_to_pips: HashMap::new(),
            tile_wires: HashMap::new(),
            packages: HashMap::new(),
            routing: Self::default_routing(),
            clock_resources: Self::default_clock_resources(),
            logic_tiles: Vec::new(),
            io_tiles: Vec::new(),
            memory_blocks: Vec::new(),
        };
        device.build_architecture();
        device
    }

    /// Create HX1K device
    pub fn hx1k() -> Self {
        Self::new(Ice40Variant::Hx1k)
    }

    /// Create HX8K device
    pub fn hx8k() -> Self {
        Self::new(Ice40Variant::Hx8k)
    }

    /// Create UP5K device
    pub fn up5k() -> Self {
        Self::new(Ice40Variant::Up5k)
    }

    /// Default routing architecture for iCE40
    fn default_routing() -> RoutingArchitecture {
        RoutingArchitecture {
            channels: (20, 20),
            switch_pattern: SwitchPattern::Ice40,
            wire_segments: vec![
                // Local tracks (within tile)
                WireSegment {
                    length: 1,
                    count: 8,
                    direction: WireDirection::Bidirectional,
                },
                // Span-4 horizontal
                WireSegment {
                    length: 4,
                    count: 4,
                    direction: WireDirection::Horizontal,
                },
                // Span-4 vertical
                WireSegment {
                    length: 4,
                    count: 4,
                    direction: WireDirection::Vertical,
                },
                // Span-12 horizontal (long lines)
                WireSegment {
                    length: 12,
                    count: 2,
                    direction: WireDirection::Horizontal,
                },
                // Span-12 vertical (long lines)
                WireSegment {
                    length: 12,
                    count: 2,
                    direction: WireDirection::Vertical,
                },
            ],
        }
    }

    /// Default clock resources for iCE40
    fn default_clock_resources() -> ClockResources {
        ClockResources {
            global_clocks: 8,
            plls: 1,
            dlls: 0,
            clock_domains: vec![super::ClockDomain {
                name: "GCLK".to_string(),
                max_frequency: 275.0e6,
            }],
        }
    }

    /// Build the device architecture
    fn build_architecture(&mut self) {
        let (width, height) = self.grid_size;

        // Initialize tile grid
        self.tiles = vec![vec![None; width as usize]; height as usize];

        let mut wire_id = 0u32;
        let mut bel_id = 0u32;

        // Create tiles
        for y in 0..height {
            for x in 0..width {
                let tile_type = self.determine_tile_type(x, y);

                if tile_type != TileType::Empty {
                    let bels = self.create_tile_bels(tile_type, x, y, &mut bel_id);
                    let tile = Ice40Tile::new(tile_type, x, y, bels);

                    // Track tile types
                    match tile_type {
                        TileType::Logic => {
                            self.logic_tiles.push(LogicTile {
                                x,
                                y,
                                lut_count: 8,
                                ff_count: 8,
                                has_carry: true,
                            });
                        }
                        TileType::IoTop
                        | TileType::IoBottom
                        | TileType::IoLeft
                        | TileType::IoRight => {
                            let side = match tile_type {
                                TileType::IoTop => IoSide::Top,
                                TileType::IoBottom => IoSide::Bottom,
                                TileType::IoLeft => IoSide::Left,
                                TileType::IoRight => IoSide::Right,
                                _ => unreachable!(),
                            };
                            self.io_tiles.push(IoTile {
                                x,
                                y,
                                io_count: 2,
                                side,
                                io_standards: vec![
                                    "LVCMOS33".to_string(),
                                    "LVCMOS25".to_string(),
                                    "LVCMOS18".to_string(),
                                ],
                                drive_strengths: vec![4, 8, 12],
                                diff_pairs: false,
                            });
                        }
                        TileType::RamTop | TileType::RamBottom => {
                            if tile_type == TileType::RamTop {
                                self.memory_blocks.push(MemoryBlock {
                                    x,
                                    y,
                                    size_bits: 4096,
                                    widths: vec![1, 2, 4, 8, 16],
                                });
                            }
                        }
                        _ => {}
                    }

                    // Create wires for this tile
                    self.create_tile_wires(x, y, tile_type, &mut wire_id);

                    self.tiles[y as usize][x as usize] = Some(tile);
                }
            }
        }

        // Create PIPs (interconnect)
        self.create_pips();

        // Add package pin mappings
        self.add_package_pins();
    }

    /// Determine tile type based on coordinates
    fn determine_tile_type(&self, x: u32, y: u32) -> TileType {
        let (width, height) = self.grid_size;

        // Corner tiles are empty
        if (x == 0 || x == width - 1) && (y == 0 || y == height - 1) {
            return TileType::Empty;
        }

        // I/O tiles on edges
        if y == 0 {
            return TileType::IoBottom;
        }
        if y == height - 1 {
            return TileType::IoTop;
        }
        if x == 0 {
            return TileType::IoLeft;
        }
        if x == width - 1 {
            return TileType::IoRight;
        }

        // RAM columns (typically at specific x positions)
        let ram_column = match self.variant {
            Ice40Variant::Hx1k | Ice40Variant::Lp1k => x == 3 || x == 9,
            Ice40Variant::Hx4k | Ice40Variant::Lp4k => x == 4 || x == 12,
            Ice40Variant::Hx8k | Ice40Variant::Lp8k => x == 8 || x == 16 || x == 24,
            Ice40Variant::Up5k => x == 5 || x == 12 || x == 19,
        };

        if ram_column {
            if y % 2 == 1 {
                return TileType::RamTop;
            } else {
                return TileType::RamBottom;
            }
        }

        // Everything else is logic
        TileType::Logic
    }

    /// Create BELs for a tile
    fn create_tile_bels(
        &self,
        tile_type: TileType,
        _x: u32,
        _y: u32,
        bel_id: &mut u32,
    ) -> Vec<Bel> {
        let mut bels = Vec::new();

        match tile_type {
            TileType::Logic => {
                // iCE40 logic tiles have 8 LUTs and 8 FFs
                for i in 0..8 {
                    // LUT4
                    let lut_bel = Bel {
                        id: BelId(*bel_id),
                        bel_type: BelType::Lut4,
                        name: format!("LUT4_{}", i),
                        pins: vec![
                            BelPin {
                                name: "I0".to_string(),
                                direction: PinDirection::Input,
                                wire: None,
                            },
                            BelPin {
                                name: "I1".to_string(),
                                direction: PinDirection::Input,
                                wire: None,
                            },
                            BelPin {
                                name: "I2".to_string(),
                                direction: PinDirection::Input,
                                wire: None,
                            },
                            BelPin {
                                name: "I3".to_string(),
                                direction: PinDirection::Input,
                                wire: None,
                            },
                            BelPin {
                                name: "O".to_string(),
                                direction: PinDirection::Output,
                                wire: None,
                            },
                        ],
                    };
                    *bel_id += 1;
                    bels.push(lut_bel);

                    // DFF
                    let ff_bel = Bel {
                        id: BelId(*bel_id),
                        bel_type: BelType::Dff,
                        name: format!("DFF_{}", i),
                        pins: vec![
                            BelPin {
                                name: "D".to_string(),
                                direction: PinDirection::Input,
                                wire: None,
                            },
                            BelPin {
                                name: "CLK".to_string(),
                                direction: PinDirection::Input,
                                wire: None,
                            },
                            BelPin {
                                name: "Q".to_string(),
                                direction: PinDirection::Output,
                                wire: None,
                            },
                        ],
                    };
                    *bel_id += 1;
                    bels.push(ff_bel);
                }

                // Carry chain
                let carry_bel = Bel {
                    id: BelId(*bel_id),
                    bel_type: BelType::Carry,
                    name: "CARRY".to_string(),
                    pins: vec![
                        BelPin {
                            name: "CI".to_string(),
                            direction: PinDirection::Input,
                            wire: None,
                        },
                        BelPin {
                            name: "I0".to_string(),
                            direction: PinDirection::Input,
                            wire: None,
                        },
                        BelPin {
                            name: "I1".to_string(),
                            direction: PinDirection::Input,
                            wire: None,
                        },
                        BelPin {
                            name: "CO".to_string(),
                            direction: PinDirection::Output,
                            wire: None,
                        },
                    ],
                };
                *bel_id += 1;
                bels.push(carry_bel);
            }
            TileType::IoTop | TileType::IoBottom | TileType::IoLeft | TileType::IoRight => {
                // I/O tiles have 2 I/O cells
                for i in 0..2 {
                    let io_bel = Bel {
                        id: BelId(*bel_id),
                        bel_type: BelType::IoCell,
                        name: format!("IO_{}", i),
                        pins: vec![
                            BelPin {
                                name: "D_IN_0".to_string(),
                                direction: PinDirection::Output,
                                wire: None,
                            },
                            BelPin {
                                name: "D_OUT_0".to_string(),
                                direction: PinDirection::Input,
                                wire: None,
                            },
                            BelPin {
                                name: "OUT_EN".to_string(),
                                direction: PinDirection::Input,
                                wire: None,
                            },
                            BelPin {
                                name: "PACKAGE_PIN".to_string(),
                                direction: PinDirection::Inout,
                                wire: None,
                            },
                        ],
                    };
                    *bel_id += 1;
                    bels.push(io_bel);
                }
            }
            TileType::RamTop | TileType::RamBottom => {
                // RAM tiles have RAM primitives
                let ram_bel = Bel {
                    id: BelId(*bel_id),
                    bel_type: BelType::RamSlice,
                    name: "RAM".to_string(),
                    pins: vec![
                        BelPin {
                            name: "RDATA".to_string(),
                            direction: PinDirection::Output,
                            wire: None,
                        },
                        BelPin {
                            name: "RADDR".to_string(),
                            direction: PinDirection::Input,
                            wire: None,
                        },
                        BelPin {
                            name: "WDATA".to_string(),
                            direction: PinDirection::Input,
                            wire: None,
                        },
                        BelPin {
                            name: "WADDR".to_string(),
                            direction: PinDirection::Input,
                            wire: None,
                        },
                        BelPin {
                            name: "WE".to_string(),
                            direction: PinDirection::Input,
                            wire: None,
                        },
                        BelPin {
                            name: "WCLK".to_string(),
                            direction: PinDirection::Input,
                            wire: None,
                        },
                        BelPin {
                            name: "RCLK".to_string(),
                            direction: PinDirection::Input,
                            wire: None,
                        },
                    ],
                };
                *bel_id += 1;
                bels.push(ram_bel);
            }
            _ => {}
        }

        bels
    }

    /// Create wires for a tile
    fn create_tile_wires(&mut self, x: u32, y: u32, tile_type: TileType, wire_id: &mut u32) {
        let mut tile_wire_ids = Vec::new();

        // Create local wires
        for i in 0..8 {
            let wire = Wire {
                id: WireId(*wire_id),
                name: format!("local_g{}_{}_{}", i, x, y),
                wire_type: WireType::Local(i),
                tile_x: x,
                tile_y: y,
                delay: 50, // 50ps local wire delay
            };
            self.wire_names.insert(wire.name.clone(), wire.id);
            tile_wire_ids.push(wire.id);
            self.wires.push(wire);
            *wire_id += 1;
        }

        // Create span-4 wires (only for non-edge tiles)
        if tile_type == TileType::Logic {
            // Horizontal span-4
            for i in 0..4 {
                let wire = Wire {
                    id: WireId(*wire_id),
                    name: format!("sp4_h_r_{}_{}_{}", i, x, y),
                    wire_type: WireType::Span4H(i),
                    tile_x: x,
                    tile_y: y,
                    delay: 200, // 200ps span-4 wire delay
                };
                self.wire_names.insert(wire.name.clone(), wire.id);
                tile_wire_ids.push(wire.id);
                self.wires.push(wire);
                *wire_id += 1;
            }

            // Vertical span-4
            for i in 0..4 {
                let wire = Wire {
                    id: WireId(*wire_id),
                    name: format!("sp4_v_b_{}_{}_{}", i, x, y),
                    wire_type: WireType::Span4V(i),
                    tile_x: x,
                    tile_y: y,
                    delay: 200,
                };
                self.wire_names.insert(wire.name.clone(), wire.id);
                tile_wire_ids.push(wire.id);
                self.wires.push(wire);
                *wire_id += 1;
            }

            // Span-12 wires (long lines)
            for i in 0..2 {
                let wire_h = Wire {
                    id: WireId(*wire_id),
                    name: format!("sp12_h_r_{}_{}_{}", i, x, y),
                    wire_type: WireType::Span12H(i),
                    tile_x: x,
                    tile_y: y,
                    delay: 400, // 400ps span-12 wire delay
                };
                self.wire_names.insert(wire_h.name.clone(), wire_h.id);
                tile_wire_ids.push(wire_h.id);
                self.wires.push(wire_h);
                *wire_id += 1;

                let wire_v = Wire {
                    id: WireId(*wire_id),
                    name: format!("sp12_v_b_{}_{}_{}", i, x, y),
                    wire_type: WireType::Span12V(i),
                    tile_x: x,
                    tile_y: y,
                    delay: 400,
                };
                self.wire_names.insert(wire_v.name.clone(), wire_v.id);
                tile_wire_ids.push(wire_v.id);
                self.wires.push(wire_v);
                *wire_id += 1;
            }

            // Neighbor wires
            let wire = Wire {
                id: WireId(*wire_id),
                name: format!("neigh_op_{}_{}", x, y),
                wire_type: WireType::Neighbour,
                tile_x: x,
                tile_y: y,
                delay: 100,
            };
            self.wire_names.insert(wire.name.clone(), wire.id);
            tile_wire_ids.push(wire.id);
            self.wires.push(wire);
            *wire_id += 1;

            // Carry chain wire
            let wire = Wire {
                id: WireId(*wire_id),
                name: format!("carry_in_{}_{}", x, y),
                wire_type: WireType::CarryChain,
                tile_x: x,
                tile_y: y,
                delay: 30, // Fast carry chain
            };
            self.wire_names.insert(wire.name.clone(), wire.id);
            tile_wire_ids.push(wire.id);
            self.wires.push(wire);
            *wire_id += 1;
        }

        // Global clock wires (for all tiles)
        for i in 0..8 {
            let wire = Wire {
                id: WireId(*wire_id),
                name: format!("glb{}_{}_{}", i, x, y),
                wire_type: WireType::Global(i),
                tile_x: x,
                tile_y: y,
                delay: 100, // Global clock delay
            };
            self.wire_names.insert(wire.name.clone(), wire.id);
            tile_wire_ids.push(wire.id);
            self.wires.push(wire);
            *wire_id += 1;
        }

        self.tile_wires.insert((x, y), tile_wire_ids);
    }

    /// Create PIPs (Programmable Interconnect Points)
    fn create_pips(&mut self) {
        let mut pip_id = 0u32;

        // For each tile, create PIPs connecting wires
        for y in 0..self.grid_size.1 {
            for x in 0..self.grid_size.0 {
                if let Some(tile_wires) = self.tile_wires.get(&(x, y)).cloned() {
                    // Create PIPs within the tile (local routing)
                    self.create_tile_pips(x, y, &tile_wires, &mut pip_id);

                    // Create PIPs to adjacent tiles
                    self.create_neighbor_pips(x, y, &tile_wires, &mut pip_id);
                }
            }
        }
    }

    /// Create PIPs within a tile
    fn create_tile_pips(&mut self, x: u32, y: u32, tile_wires: &[WireId], pip_id: &mut u32) {
        // Create PIPs between local wires and span wires
        let local_wires: Vec<_> = tile_wires
            .iter()
            .filter(|&&w| matches!(self.wires[w.0 as usize].wire_type, WireType::Local(_)))
            .copied()
            .collect();

        let span_wires: Vec<_> = tile_wires
            .iter()
            .filter(|&&w| {
                matches!(
                    self.wires[w.0 as usize].wire_type,
                    WireType::Span4H(_)
                        | WireType::Span4V(_)
                        | WireType::Span12H(_)
                        | WireType::Span12V(_)
                )
            })
            .copied()
            .collect();

        // Local to span PIPs
        for &local in &local_wires {
            for &span in &span_wires {
                let pip = Pip {
                    id: PipId(*pip_id),
                    src_wire: local,
                    dst_wire: span,
                    delay: 100, // PIP delay
                    configurable: true,
                    tile_x: x,
                    tile_y: y,
                };
                self.wire_to_pips.entry(span).or_default().push(pip.id);
                self.pips.push(pip);
                *pip_id += 1;
            }
        }

        // Span to local PIPs
        for &span in &span_wires {
            for &local in &local_wires {
                let pip = Pip {
                    id: PipId(*pip_id),
                    src_wire: span,
                    dst_wire: local,
                    delay: 100,
                    configurable: true,
                    tile_x: x,
                    tile_y: y,
                };
                self.wire_to_pips.entry(local).or_default().push(pip.id);
                self.pips.push(pip);
                *pip_id += 1;
            }
        }

        // Local to local PIPs (switchbox)
        for (i, &src) in local_wires.iter().enumerate() {
            for (j, &dst) in local_wires.iter().enumerate() {
                if i != j {
                    let pip = Pip {
                        id: PipId(*pip_id),
                        src_wire: src,
                        dst_wire: dst,
                        delay: 50, // Local PIP delay
                        configurable: true,
                        tile_x: x,
                        tile_y: y,
                    };
                    self.wire_to_pips.entry(dst).or_default().push(pip.id);
                    self.pips.push(pip);
                    *pip_id += 1;
                }
            }
        }
    }

    /// Create PIPs to adjacent tiles
    fn create_neighbor_pips(&mut self, x: u32, y: u32, tile_wires: &[WireId], pip_id: &mut u32) {
        // Get span wires that connect to neighbors
        let span4_h: Vec<_> = tile_wires
            .iter()
            .filter(|&&w| matches!(self.wires[w.0 as usize].wire_type, WireType::Span4H(_)))
            .copied()
            .collect();

        let span4_v: Vec<_> = tile_wires
            .iter()
            .filter(|&&w| matches!(self.wires[w.0 as usize].wire_type, WireType::Span4V(_)))
            .copied()
            .collect();

        // Connect span-4 horizontal to right neighbor
        if x + 1 < self.grid_size.0 {
            if let Some(neighbor_wires) = self.tile_wires.get(&(x + 1, y)).cloned() {
                let neighbor_span4_h: Vec<_> = neighbor_wires
                    .iter()
                    .filter(|&&w| matches!(self.wires[w.0 as usize].wire_type, WireType::Span4H(_)))
                    .copied()
                    .collect();

                for (i, &src) in span4_h.iter().enumerate() {
                    if let Some(&dst) = neighbor_span4_h.get(i) {
                        let pip = Pip {
                            id: PipId(*pip_id),
                            src_wire: src,
                            dst_wire: dst,
                            delay: 50,           // Neighbor PIP delay
                            configurable: false, // Hardwired connection
                            tile_x: x,
                            tile_y: y,
                        };
                        self.wire_to_pips.entry(dst).or_default().push(pip.id);
                        self.pips.push(pip);
                        *pip_id += 1;
                    }
                }
            }
        }

        // Connect span-4 vertical to top neighbor
        if y + 1 < self.grid_size.1 {
            if let Some(neighbor_wires) = self.tile_wires.get(&(x, y + 1)).cloned() {
                let neighbor_span4_v: Vec<_> = neighbor_wires
                    .iter()
                    .filter(|&&w| matches!(self.wires[w.0 as usize].wire_type, WireType::Span4V(_)))
                    .copied()
                    .collect();

                for (i, &src) in span4_v.iter().enumerate() {
                    if let Some(&dst) = neighbor_span4_v.get(i) {
                        let pip = Pip {
                            id: PipId(*pip_id),
                            src_wire: src,
                            dst_wire: dst,
                            delay: 50,
                            configurable: false,
                            tile_x: x,
                            tile_y: y,
                        };
                        self.wire_to_pips.entry(dst).or_default().push(pip.id);
                        self.pips.push(pip);
                        *pip_id += 1;
                    }
                }
            }
        }
    }

    /// Add package pin mappings
    fn add_package_pins(&mut self) {
        // Add common packages for each variant
        match self.variant {
            Ice40Variant::Hx1k | Ice40Variant::Lp1k => {
                self.add_tq144_package();
                self.add_vq100_package();
            }
            Ice40Variant::Hx8k | Ice40Variant::Lp8k => {
                self.add_ct256_package();
                self.add_bg121_package();
            }
            Ice40Variant::Up5k => {
                self.add_sg48_package();
            }
            _ => {
                // Add basic package for other variants
                self.add_basic_package();
            }
        }
    }

    fn add_tq144_package(&mut self) {
        let mut pins = HashMap::new();
        // Sample pin mappings for TQ144 package
        pins.insert("1".to_string(), (0, 1, 0));
        pins.insert("2".to_string(), (0, 2, 0));
        pins.insert("3".to_string(), (0, 3, 0));
        pins.insert("4".to_string(), (0, 4, 0));
        // ... more pins would be added here

        self.packages.insert(
            "tq144".to_string(),
            PackagePins {
                name: "tq144".to_string(),
                pins,
            },
        );
    }

    fn add_vq100_package(&mut self) {
        let mut pins = HashMap::new();
        pins.insert("1".to_string(), (0, 1, 0));
        pins.insert("2".to_string(), (0, 2, 0));

        self.packages.insert(
            "vq100".to_string(),
            PackagePins {
                name: "vq100".to_string(),
                pins,
            },
        );
    }

    fn add_ct256_package(&mut self) {
        let mut pins = HashMap::new();
        pins.insert("A1".to_string(), (0, 1, 0));
        pins.insert("A2".to_string(), (0, 2, 0));
        pins.insert("B1".to_string(), (1, 1, 0));

        self.packages.insert(
            "ct256".to_string(),
            PackagePins {
                name: "ct256".to_string(),
                pins,
            },
        );
    }

    fn add_bg121_package(&mut self) {
        let mut pins = HashMap::new();
        pins.insert("A1".to_string(), (0, 1, 0));
        pins.insert("A2".to_string(), (0, 2, 0));

        self.packages.insert(
            "bg121".to_string(),
            PackagePins {
                name: "bg121".to_string(),
                pins,
            },
        );
    }

    fn add_sg48_package(&mut self) {
        let mut pins = HashMap::new();
        pins.insert("1".to_string(), (0, 1, 0));
        pins.insert("2".to_string(), (0, 2, 0));
        pins.insert("3".to_string(), (0, 3, 0));

        self.packages.insert(
            "sg48".to_string(),
            PackagePins {
                name: "sg48".to_string(),
                pins,
            },
        );
    }

    fn add_basic_package(&mut self) {
        let pins = HashMap::new();
        self.packages.insert(
            "basic".to_string(),
            PackagePins {
                name: "basic".to_string(),
                pins,
            },
        );
    }
}

impl Device for Ice40Device {
    fn family(&self) -> DeviceFamily {
        DeviceFamily::Ice40
    }

    fn name(&self) -> &str {
        self.variant.name()
    }

    fn grid_size(&self) -> (u32, u32) {
        self.grid_size
    }

    fn stats(&self) -> DeviceStats {
        DeviceStats {
            total_luts: self.logic_tiles.len() * 8,
            total_ffs: self.logic_tiles.len() * 8,
            total_ios: self.io_tiles.len() * 2,
            total_brams: self.memory_blocks.len(),
            total_dsps: if self.variant.has_dsp() { 8 } else { 0 },
            total_gclks: 8,
        }
    }

    fn tile_at(&self, x: u32, y: u32) -> Option<&dyn Tile> {
        self.tiles
            .get(y as usize)?
            .get(x as usize)?
            .as_ref()
            .map(|t| t as &dyn Tile)
    }

    fn wire(&self, id: WireId) -> Option<&Wire> {
        self.wires.get(id.0 as usize)
    }

    fn pip(&self, id: PipId) -> Option<&Pip> {
        self.pips.get(id.0 as usize)
    }

    fn wire_by_name(&self, name: &str) -> Option<WireId> {
        self.wire_names.get(name).copied()
    }

    fn tile_wires(&self, x: u32, y: u32) -> Vec<WireId> {
        self.tile_wires.get(&(x, y)).cloned().unwrap_or_default()
    }

    fn wire_pips(&self, wire_id: WireId) -> Vec<PipId> {
        self.wire_to_pips.get(&wire_id).cloned().unwrap_or_default()
    }

    fn packages(&self) -> &HashMap<String, PackagePins> {
        &self.packages
    }

    fn routing(&self) -> &RoutingArchitecture {
        &self.routing
    }

    fn clock_resources(&self) -> &ClockResources {
        &self.clock_resources
    }

    fn can_place(&self, cell_type: &str, bel_type: BelType) -> bool {
        match bel_type {
            BelType::Lut4 => {
                cell_type.starts_with("SB_LUT4") || cell_type == "LUT4" || cell_type.contains("LUT")
            }
            BelType::Dff | BelType::DffE | BelType::DffSr | BelType::DffSrE => {
                cell_type.starts_with("SB_DFF") || cell_type == "DFF" || cell_type.contains("DFF")
            }
            BelType::Carry => cell_type == "SB_CARRY" || cell_type == "CARRY",
            BelType::IoCell => {
                cell_type.starts_with("SB_IO") || cell_type == "IO" || cell_type.contains("IO")
            }
            BelType::RamSlice => cell_type.starts_with("SB_RAM") || cell_type.contains("RAM"),
            BelType::GlobalBuf => cell_type == "SB_GB" || cell_type.contains("GBUF"),
            _ => false,
        }
    }
}

/// Convenience functions for creating devices
impl Ice40Device {
    /// Create an iCE40 HX1K device (for tests)
    pub fn ice40_hx1k() -> Self {
        Self::hx1k()
    }

    /// Create an iCE40 HX8K device (for tests)
    pub fn ice40_hx8k() -> Self {
        Self::hx8k()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ice40_hx1k_from_chipdb() {
        let device = Ice40Device::hx1k();

        // Verify grid size matches chipdb (14x18 for 1k)
        let (w, h) = device.grid_size();
        assert_eq!(w, 14, "Width should be 14");
        assert_eq!(h, 18, "Height should be 18");

        // Verify we have wires from chipdb
        assert!(
            device.wires.len() > 1000,
            "Should have many wires from chipdb, got {}",
            device.wires.len()
        );

        // Verify we have PIPs from chipdb
        assert!(
            device.pips.len() > 10000,
            "Should have many PIPs from chipdb, got {}",
            device.pips.len()
        );

        // Verify we have logic tiles
        assert!(!device.logic_tiles.is_empty(), "Should have logic tiles");

        // Verify we have package pins
        assert!(
            !device.packages.is_empty(),
            "Should have package pin mappings"
        );

        // Verify stats
        let stats = device.stats();
        assert!(stats.total_luts > 0, "Should have LUTs");
        assert!(stats.total_ios > 0, "Should have I/Os");
    }

    #[test]
    fn test_ice40_hx8k_from_chipdb() {
        let device = Ice40Device::hx8k();

        // 8k should have more resources than 1k
        let stats = device.stats();
        assert!(stats.total_luts > 5000, "8k should have many LUTs");

        // Verify we have PIPs
        assert!(
            device.pips.len() > 50000,
            "8k should have many PIPs, got {}",
            device.pips.len()
        );
    }
}
