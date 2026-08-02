//! Lattice ECP5 FPGA device database
//!
//! Provides the device database for Lattice ECP5 FPGAs. The ECP5 family has
//! complete open-source toolchain support via Project Trellis (prjtrellis),
//! including SERDES, DDR3, and all hard IP blocks.
//!
//! Device parameters, grid sizes, resource counts, IDCODEs, bitstream geometry,
//! and timing data are derived from the Project Trellis database and the official
//! Lattice ECP5 Family Data Sheet (DS1044).
//!
//! Copyright (C) 2018 The Project Trellis Authors. All rights reserved.
//! Licensed under ISC License. See: <https://github.com/YosysHQ/prjtrellis>
//!
//! # Supported Variants
//!
//! - LFE5U-12F (12K LUT4, 32 EBR, 28 MULT18, 2 PLL)
//! - LFE5U-25F (24K LUT4, 56 EBR, 28 MULT18, 2 PLL)
//! - LFE5U-45F (44K LUT4, 108 EBR, 72 MULT18, 4 PLL)
//! - LFE5U-85F (84K LUT4, 208 EBR, 156 MULT18, 4 PLL)
//! - LFE5UM variants add SERDES (up to 3.125 Gbps)
//! - LFE5UM5G variants add 5G SERDES (up to 5 Gbps)

pub mod data;
mod tiles;

pub use tiles::Ecp5Tile;

use super::{
    Bel, BelId, BelPin, BelType, ClockDomain, ClockResources, Device, DeviceFamily, DeviceStats,
    DspTile, IoSide, IoTile, LogicTile, MemoryBlock, PackagePins, PinDirection, Pip, PipId,
    RoutingArchitecture, SwitchPattern, Tile, TileType, Wire, WireDirection, WireId, WireSegment,
    WireType,
};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// ECP5 device variants
///
/// Resource counts from prjtrellis devices.json and Lattice DS1044 Table 1-1.
/// 12F and 25F share the same die (72×50 grid); 12F is fuse-limited.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Ecp5Variant {
    /// LFE5U-12F — 12K LUT4, 32 EBR, 28 MULT18, 2 PLL, 197 I/O (72×50 grid)
    Lfe5u12f,
    /// LFE5U-25F — 24K LUT4, 56 EBR, 28 MULT18, 2 PLL, 197 I/O (72×50 grid)
    Lfe5u25f,
    /// LFE5U-45F — 44K LUT4, 108 EBR, 72 MULT18, 4 PLL, 365 I/O (90×71 grid)
    Lfe5u45f,
    /// LFE5U-85F — 84K LUT4, 208 EBR, 156 MULT18, 4 PLL, 365 I/O (126×95 grid)
    Lfe5u85f,
    /// LFE5UM-25F — 25F + SERDES (1 DCU, 2 channels, up to 3.125 Gbps)
    Lfe5um25f,
    /// LFE5UM-45F — 45F + SERDES (2 DCU, 4 channels, up to 3.125 Gbps)
    Lfe5um45f,
    /// LFE5UM-85F — 85F + SERDES (2 DCU, 4 channels, up to 3.125 Gbps)
    Lfe5um85f,
    /// LFE5UM5G-25F — 25F + 5G SERDES (1 DCU, 2 channels, up to 5 Gbps)
    Lfe5um5g25f,
    /// LFE5UM5G-45F — 45F + 5G SERDES (2 DCU, 4 channels, up to 5 Gbps)
    Lfe5um5g45f,
    /// LFE5UM5G-85F — 85F + 5G SERDES (2 DCU, 4 channels, up to 5 Gbps)
    Lfe5um5g85f,
}

impl Ecp5Variant {
    /// Get the die data record for this variant's base die size
    pub fn die_data(&self) -> &'static data::Ecp5DieData {
        match self.base_size() {
            Ecp5Size::F12 => &data::DIE_12F,
            Ecp5Size::F25 => &data::DIE_25F,
            Ecp5Size::F45 => &data::DIE_45F,
            Ecp5Size::F85 => &data::DIE_85F,
        }
    }

    /// Grid dimensions from prjtrellis devices.json
    pub fn grid_size(&self) -> (u32, u32) {
        self.die_data().grid
    }

    pub fn name(&self) -> &'static str {
        match self {
            Ecp5Variant::Lfe5u12f => "LFE5U-12F",
            Ecp5Variant::Lfe5u25f => "LFE5U-25F",
            Ecp5Variant::Lfe5u45f => "LFE5U-45F",
            Ecp5Variant::Lfe5u85f => "LFE5U-85F",
            Ecp5Variant::Lfe5um25f => "LFE5UM-25F",
            Ecp5Variant::Lfe5um45f => "LFE5UM-45F",
            Ecp5Variant::Lfe5um85f => "LFE5UM-85F",
            Ecp5Variant::Lfe5um5g25f => "LFE5UM5G-25F",
            Ecp5Variant::Lfe5um5g45f => "LFE5UM5G-45F",
            Ecp5Variant::Lfe5um5g85f => "LFE5UM5G-85F",
        }
    }

    pub fn lut_count(&self) -> usize {
        self.die_data().lut4s
    }
    #[allow(dead_code)]
    pub fn plc2_count(&self) -> usize {
        self.die_data().plc2_tiles
    }
    pub fn ebr_count(&self) -> usize {
        self.die_data().ebr_blocks
    }
    pub fn io_count(&self) -> usize {
        self.die_data().max_ios
    }
    pub fn dsp_count(&self) -> usize {
        self.die_data().mult18s
    }
    pub fn pll_count(&self) -> u8 {
        self.die_data().plls
    }
    #[allow(dead_code)]
    pub fn dll_count(&self) -> u8 {
        self.die_data().dlls
    }
    #[allow(dead_code)]
    pub fn dist_ram_kbits(&self) -> usize {
        self.die_data().dist_ram_kbits
    }

    /// IDCODE from prjtrellis — depends on U/UM/UM5G family
    #[allow(dead_code)]
    pub fn idcode(&self) -> u32 {
        let die = self.die_data();
        match self.family_type() {
            Ecp5Family::U => die.idcode_u,
            Ecp5Family::Um => die.idcode_um,
            Ecp5Family::Um5g => die.idcode_um5g,
        }
    }

    /// Bitstream geometry: (frames, bits_per_frame)
    #[allow(dead_code)]
    pub fn bitstream_geometry(&self) -> (u32, u32) {
        let die = self.die_data();
        (die.bitstream_frames, die.bits_per_frame)
    }

    /// Available packages
    #[allow(dead_code)]
    pub fn packages(&self) -> &'static [&'static str] {
        self.die_data().packages
    }

    /// Has SERDES transceivers (DCU blocks)
    pub fn has_serdes(&self) -> bool {
        !matches!(self.family_type(), Ecp5Family::U)
    }

    /// Number of SERDES channels (2 per DCU)
    pub fn serdes_channels(&self) -> u8 {
        if self.has_serdes() {
            self.die_data().serdes_channels
        } else {
            0
        }
    }

    /// SERDES line rate (Gbps)
    pub fn serdes_rate(&self) -> f64 {
        match self.family_type() {
            Ecp5Family::Um => data::SERDES_RATE_UM,
            Ecp5Family::Um5g => data::SERDES_RATE_UM5G,
            Ecp5Family::U => 0.0,
        }
    }

    fn family_type(&self) -> Ecp5Family {
        match self {
            Ecp5Variant::Lfe5u12f
            | Ecp5Variant::Lfe5u25f
            | Ecp5Variant::Lfe5u45f
            | Ecp5Variant::Lfe5u85f => Ecp5Family::U,
            Ecp5Variant::Lfe5um25f | Ecp5Variant::Lfe5um45f | Ecp5Variant::Lfe5um85f => {
                Ecp5Family::Um
            }
            Ecp5Variant::Lfe5um5g25f | Ecp5Variant::Lfe5um5g45f | Ecp5Variant::Lfe5um5g85f => {
                Ecp5Family::Um5g
            }
        }
    }

    fn base_size(&self) -> Ecp5Size {
        match self {
            Ecp5Variant::Lfe5u12f => Ecp5Size::F12,
            Ecp5Variant::Lfe5u25f | Ecp5Variant::Lfe5um25f | Ecp5Variant::Lfe5um5g25f => {
                Ecp5Size::F25
            }
            Ecp5Variant::Lfe5u45f | Ecp5Variant::Lfe5um45f | Ecp5Variant::Lfe5um5g45f => {
                Ecp5Size::F45
            }
            Ecp5Variant::Lfe5u85f | Ecp5Variant::Lfe5um85f | Ecp5Variant::Lfe5um5g85f => {
                Ecp5Size::F85
            }
        }
    }
}

impl std::fmt::Display for Ecp5Variant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name())
    }
}

#[derive(Debug, Clone, Copy)]
enum Ecp5Size {
    F12,
    F25,
    F45,
    F85,
}

#[derive(Debug, Clone, Copy)]
enum Ecp5Family {
    U,
    Um,
    Um5g,
}

/// Lattice ECP5 device
#[derive(Debug, Clone)]
pub struct Ecp5Device {
    pub variant: Ecp5Variant,
    grid_size: (u32, u32),
    tiles: Vec<Vec<Option<Ecp5Tile>>>,
    wires: Vec<Wire>,
    wire_names: HashMap<String, WireId>,
    pips: Vec<Pip>,
    wire_to_pips: HashMap<WireId, Vec<PipId>>,
    wire_src_pips: HashMap<WireId, Vec<PipId>>,
    tile_wires: HashMap<(u32, u32), Vec<WireId>>,
    bel_wires: HashMap<(u32, u32, String), WireId>,
    packages: HashMap<String, PackagePins>,
    routing: RoutingArchitecture,
    clock_resources: ClockResources,
    pub logic_tiles: Vec<LogicTile>,
    pub io_tiles: Vec<IoTile>,
    pub memory_blocks: Vec<MemoryBlock>,
    pub dsp_tiles: Vec<DspTile>,
}

impl Ecp5Device {
    /// Create a new ECP5 device with synthetic architecture
    pub fn new(variant: Ecp5Variant) -> Self {
        Self::new_synthetic(variant)
    }

    fn new_synthetic(variant: Ecp5Variant) -> Self {
        let grid_size = variant.grid_size();
        let (width, height) = grid_size;

        let mut device = Self {
            variant,
            grid_size,
            tiles: vec![vec![None; width as usize]; height as usize],
            wires: Vec::new(),
            wire_names: HashMap::new(),
            pips: Vec::new(),
            wire_to_pips: HashMap::new(),
            wire_src_pips: HashMap::new(),
            tile_wires: HashMap::new(),
            bel_wires: HashMap::new(),
            packages: HashMap::new(),
            routing: Self::default_routing(),
            clock_resources: Self::default_clock_resources(variant),
            logic_tiles: Vec::new(),
            io_tiles: Vec::new(),
            memory_blocks: Vec::new(),
            dsp_tiles: Vec::new(),
        };

        device.build_synthetic_tiles();
        device.build_synthetic_wires_and_pips();
        device.build_synthetic_packages();

        device
    }

    fn build_synthetic_tiles(&mut self) {
        let (width, height) = self.grid_size;
        let mut bel_id = 0u32;

        // ECP5 tile layout from prjtrellis:
        // - Row 0, height-1: I/O (PIC tiles)
        // - Col 0, width-1: I/O (PIC tiles)
        // - BRAM columns at regular intervals
        // - DSP columns at regular intervals (paired with BRAM)
        let bram_cols: Vec<u32> = (data::BRAM_COLUMN_SPACING..width - 1)
            .step_by(data::BRAM_COLUMN_SPACING as usize)
            .collect();
        let dsp_cols: Vec<u32> = if self.variant.dsp_count() > 0 {
            (data::DSP_COLUMN_SPACING..width - 1)
                .step_by(data::DSP_COLUMN_SPACING as usize)
                .collect()
        } else {
            Vec::new()
        };

        for y in 0..height {
            for x in 0..width {
                let is_top = y == height - 1;
                let is_bottom = y == 0;
                let is_left = x == 0;
                let is_right = x == width - 1;
                let is_edge = is_top || is_bottom || is_left || is_right;
                let is_bram_col = bram_cols.contains(&x);
                let is_dsp_col = dsp_cols.contains(&x);

                if is_edge {
                    if (is_top || is_bottom) && (is_left || is_right) {
                        self.tiles[y as usize][x as usize] =
                            Some(Ecp5Tile::new(TileType::Empty, x, y, Vec::new()));
                        continue;
                    }

                    let side = if is_bottom {
                        TileType::IoBottom
                    } else if is_top {
                        TileType::IoTop
                    } else if is_left {
                        TileType::IoLeft
                    } else {
                        TileType::IoRight
                    };

                    let io_bels = Self::make_io_bels(&mut bel_id);
                    let io_side = match side {
                        TileType::IoTop => IoSide::Top,
                        TileType::IoBottom => IoSide::Bottom,
                        TileType::IoLeft => IoSide::Left,
                        _ => IoSide::Right,
                    };
                    self.io_tiles.push(IoTile {
                        x,
                        y,
                        io_count: data::IO_CELLS_PER_TILE,
                        side: io_side,
                        io_standards: data::IO_STANDARDS.iter().map(|s| s.to_string()).collect(),
                        drive_strengths: data::IO_DRIVE_STRENGTHS.to_vec(),
                        diff_pairs: true,
                    });
                    self.tiles[y as usize][x as usize] = Some(Ecp5Tile::new(side, x, y, io_bels));
                } else if is_dsp_col {
                    let dsp_bels = Self::make_dsp_bels(&mut bel_id);
                    self.dsp_tiles.push(DspTile {
                        x,
                        y,
                        mac_count: data::DSP_MULTS_PER_TILE,
                    });
                    self.tiles[y as usize][x as usize] =
                        Some(Ecp5Tile::new(TileType::Dsp, x, y, dsp_bels));
                } else if is_bram_col {
                    let bram_bels = Self::make_bram_bels(&mut bel_id);
                    self.memory_blocks.push(MemoryBlock {
                        x,
                        y,
                        size_bits: data::EBR_SIZE_BITS,
                        widths: data::EBR_WIDTHS.to_vec(),
                    });
                    self.tiles[y as usize][x as usize] =
                        Some(Ecp5Tile::new(TileType::RamTop, x, y, bram_bels));
                } else {
                    // Logic tile: ECP5 PLC2 — 4 slices × 2 LUT4+FF = 8 LUT4 per tile
                    let logic_bels = Self::make_logic_bels(&mut bel_id);
                    self.logic_tiles.push(LogicTile {
                        x,
                        y,
                        lut_count: data::LOGIC_LUTS_PER_TILE,
                        ff_count: data::LOGIC_FFS_PER_TILE,
                        has_carry: true,
                    });
                    self.tiles[y as usize][x as usize] =
                        Some(Ecp5Tile::new(TileType::Logic, x, y, logic_bels));
                }
            }
        }
    }

    fn make_logic_bels(bel_id: &mut u32) -> Vec<Bel> {
        let mut bels = Vec::with_capacity(17);

        for i in 0..8 {
            let id = BelId(*bel_id);
            *bel_id += 1;
            bels.push(Bel {
                id,
                bel_type: BelType::Lut4,
                name: format!("LUT4_{}", i),
                pins: vec![
                    BelPin {
                        name: "A".into(),
                        direction: PinDirection::Input,
                        wire: None,
                    },
                    BelPin {
                        name: "B".into(),
                        direction: PinDirection::Input,
                        wire: None,
                    },
                    BelPin {
                        name: "C".into(),
                        direction: PinDirection::Input,
                        wire: None,
                    },
                    BelPin {
                        name: "D".into(),
                        direction: PinDirection::Input,
                        wire: None,
                    },
                    BelPin {
                        name: "Z".into(),
                        direction: PinDirection::Output,
                        wire: None,
                    },
                ],
            });
        }

        for i in 0..8 {
            let id = BelId(*bel_id);
            *bel_id += 1;
            bels.push(Bel {
                id,
                bel_type: BelType::DffSrE,
                name: format!("FF_{}", i),
                pins: vec![
                    BelPin {
                        name: "D".into(),
                        direction: PinDirection::Input,
                        wire: None,
                    },
                    BelPin {
                        name: "CLK".into(),
                        direction: PinDirection::Input,
                        wire: None,
                    },
                    BelPin {
                        name: "CE".into(),
                        direction: PinDirection::Input,
                        wire: None,
                    },
                    BelPin {
                        name: "LSR".into(),
                        direction: PinDirection::Input,
                        wire: None,
                    },
                    BelPin {
                        name: "Q".into(),
                        direction: PinDirection::Output,
                        wire: None,
                    },
                ],
            });
        }

        let id = BelId(*bel_id);
        *bel_id += 1;
        bels.push(Bel {
            id,
            bel_type: BelType::Carry,
            name: "CCU2C".into(),
            pins: vec![
                BelPin {
                    name: "CIN".into(),
                    direction: PinDirection::Input,
                    wire: None,
                },
                BelPin {
                    name: "COUT".into(),
                    direction: PinDirection::Output,
                    wire: None,
                },
            ],
        });

        bels
    }

    fn make_io_bels(bel_id: &mut u32) -> Vec<Bel> {
        let mut bels = Vec::with_capacity(2);
        for i in 0..2 {
            let id = BelId(*bel_id);
            *bel_id += 1;
            bels.push(Bel {
                id,
                bel_type: BelType::IoCell,
                name: format!("PIO_{}", i),
                pins: vec![
                    BelPin {
                        name: "PAD".into(),
                        direction: PinDirection::Inout,
                        wire: None,
                    },
                    BelPin {
                        name: "I".into(),
                        direction: PinDirection::Output,
                        wire: None,
                    },
                    BelPin {
                        name: "O".into(),
                        direction: PinDirection::Input,
                        wire: None,
                    },
                    BelPin {
                        name: "T".into(),
                        direction: PinDirection::Input,
                        wire: None,
                    },
                ],
            });
        }
        bels
    }

    fn make_bram_bels(bel_id: &mut u32) -> Vec<Bel> {
        let id = BelId(*bel_id);
        *bel_id += 1;
        vec![Bel {
            id,
            bel_type: BelType::RamSlice,
            name: "DP16KD".into(),
            pins: vec![
                BelPin {
                    name: "ADDRA".into(),
                    direction: PinDirection::Input,
                    wire: None,
                },
                BelPin {
                    name: "ADDRB".into(),
                    direction: PinDirection::Input,
                    wire: None,
                },
                BelPin {
                    name: "DIA".into(),
                    direction: PinDirection::Input,
                    wire: None,
                },
                BelPin {
                    name: "DIB".into(),
                    direction: PinDirection::Input,
                    wire: None,
                },
                BelPin {
                    name: "DOA".into(),
                    direction: PinDirection::Output,
                    wire: None,
                },
                BelPin {
                    name: "DOB".into(),
                    direction: PinDirection::Output,
                    wire: None,
                },
                BelPin {
                    name: "CLKA".into(),
                    direction: PinDirection::Input,
                    wire: None,
                },
                BelPin {
                    name: "CLKB".into(),
                    direction: PinDirection::Input,
                    wire: None,
                },
                BelPin {
                    name: "WEA".into(),
                    direction: PinDirection::Input,
                    wire: None,
                },
                BelPin {
                    name: "WEB".into(),
                    direction: PinDirection::Input,
                    wire: None,
                },
                BelPin {
                    name: "CEA".into(),
                    direction: PinDirection::Input,
                    wire: None,
                },
                BelPin {
                    name: "CEB".into(),
                    direction: PinDirection::Input,
                    wire: None,
                },
            ],
        }]
    }

    fn make_dsp_bels(bel_id: &mut u32) -> Vec<Bel> {
        let mut bels = Vec::with_capacity(2);
        for i in 0..2 {
            let id = BelId(*bel_id);
            *bel_id += 1;
            bels.push(Bel {
                id,
                bel_type: BelType::DspSlice,
                name: format!("MULT18X18D_{}", i),
                pins: vec![
                    BelPin {
                        name: "A".into(),
                        direction: PinDirection::Input,
                        wire: None,
                    },
                    BelPin {
                        name: "B".into(),
                        direction: PinDirection::Input,
                        wire: None,
                    },
                    BelPin {
                        name: "P".into(),
                        direction: PinDirection::Output,
                        wire: None,
                    },
                    BelPin {
                        name: "CLK".into(),
                        direction: PinDirection::Input,
                        wire: None,
                    },
                    BelPin {
                        name: "CE".into(),
                        direction: PinDirection::Input,
                        wire: None,
                    },
                    BelPin {
                        name: "RST".into(),
                        direction: PinDirection::Input,
                        wire: None,
                    },
                ],
            });
        }
        bels
    }

    fn build_synthetic_wires_and_pips(&mut self) {
        let (width, height) = self.grid_size;
        let mut wire_id = 0u32;
        let mut pip_id = 0u32;

        for y in 0..height {
            for x in 0..width {
                if self.tiles[y as usize][x as usize].is_none() {
                    continue;
                }

                let tile_type = self.tiles[y as usize][x as usize]
                    .as_ref()
                    .unwrap()
                    .tile_type();

                // Local wires (ECP5 has ~20 local wires per tile)
                for i in 0..20u8 {
                    let w = Wire {
                        id: WireId(wire_id),
                        name: format!("R{}C{}_J{}", y, x, i),
                        wire_type: WireType::Local(i),
                        tile_x: x,
                        tile_y: y,
                        delay: 40, // 40ps — ECP5 45nm
                    };
                    self.wire_names.insert(w.name.clone(), w.id);
                    self.tile_wires.entry((x, y)).or_default().push(w.id);
                    self.wires.push(w);
                    wire_id += 1;
                }

                if tile_type == TileType::Logic {
                    for lc in 0..8 {
                        // LUT output
                        let out_wire = Wire {
                            id: WireId(wire_id),
                            name: format!("R{}C{}_LUT{}_F", y, x, lc),
                            wire_type: WireType::BelPin,
                            tile_x: x,
                            tile_y: y,
                            delay: 0,
                        };
                        self.wire_names.insert(out_wire.name.clone(), out_wire.id);
                        self.bel_wires
                            .insert((x, y, format!("LUT{}_F", lc)), out_wire.id);
                        self.tile_wires.entry((x, y)).or_default().push(out_wire.id);
                        self.wires.push(out_wire);
                        wire_id += 1;

                        // LUT inputs
                        for inp in 0..4 {
                            let pin = ["A", "B", "C", "D"][inp];
                            let in_wire = Wire {
                                id: WireId(wire_id),
                                name: format!("R{}C{}_LUT{}_{}", y, x, lc, pin),
                                wire_type: WireType::BelPin,
                                tile_x: x,
                                tile_y: y,
                                delay: 0,
                            };
                            self.wire_names.insert(in_wire.name.clone(), in_wire.id);
                            self.bel_wires
                                .insert((x, y, format!("LUT{}_{}", lc, pin)), in_wire.id);
                            self.tile_wires.entry((x, y)).or_default().push(in_wire.id);
                            self.wires.push(in_wire);
                            wire_id += 1;
                        }

                        // FF output
                        let ff_wire = Wire {
                            id: WireId(wire_id),
                            name: format!("R{}C{}_FF{}_Q", y, x, lc),
                            wire_type: WireType::BelPin,
                            tile_x: x,
                            tile_y: y,
                            delay: 0,
                        };
                        self.wire_names.insert(ff_wire.name.clone(), ff_wire.id);
                        self.bel_wires
                            .insert((x, y, format!("FF{}_Q", lc)), ff_wire.id);
                        self.tile_wires.entry((x, y)).or_default().push(ff_wire.id);
                        self.wires.push(ff_wire);
                        wire_id += 1;

                        // LUT output → local PIP
                        let lut_out_id = WireId(wire_id - 7);
                        let local_idx = (lc * 2) % 20;
                        if let Some(locals) = self.tile_wires.get(&(x, y)) {
                            if let Some(&local_wire) = locals.get(local_idx) {
                                let p = Pip {
                                    id: PipId(pip_id),
                                    src_wire: lut_out_id,
                                    dst_wire: local_wire,
                                    delay: 25,
                                    configurable: true,
                                    tile_x: x,
                                    tile_y: y,
                                };
                                self.wire_to_pips.entry(p.dst_wire).or_default().push(p.id);
                                self.wire_src_pips.entry(p.src_wire).or_default().push(p.id);
                                self.pips.push(p);
                                pip_id += 1;
                            }
                        }

                        // Local → LUT input PIPs
                        for inp in 0..4 {
                            let pin = ["A", "B", "C", "D"][inp];
                            if let Some(&lut_in_id) =
                                self.bel_wires.get(&(x, y, format!("LUT{}_{}", lc, pin)))
                            {
                                for lo in 0..3u8 {
                                    let li = (lc * 2 + inp + lo as usize) % 20;
                                    if let Some(locals) = self.tile_wires.get(&(x, y)) {
                                        if let Some(&lw) = locals.get(li) {
                                            let p = Pip {
                                                id: PipId(pip_id),
                                                src_wire: lw,
                                                dst_wire: lut_in_id,
                                                delay: 20,
                                                configurable: true,
                                                tile_x: x,
                                                tile_y: y,
                                            };
                                            self.wire_to_pips
                                                .entry(p.dst_wire)
                                                .or_default()
                                                .push(p.id);
                                            self.wire_src_pips
                                                .entry(p.src_wire)
                                                .or_default()
                                                .push(p.id);
                                            self.pips.push(p);
                                            pip_id += 1;
                                        }
                                    }
                                }
                            }
                        }
                    }

                    // Clock wire
                    let clk_wire = Wire {
                        id: WireId(wire_id),
                        name: format!("R{}C{}_CLK", y, x),
                        wire_type: WireType::Global(0),
                        tile_x: x,
                        tile_y: y,
                        delay: 60,
                    };
                    self.wire_names.insert(clk_wire.name.clone(), clk_wire.id);
                    self.bel_wires
                        .insert((x, y, "CLK".to_string()), clk_wire.id);
                    self.tile_wires.entry((x, y)).or_default().push(clk_wire.id);
                    self.wires.push(clk_wire);
                    wire_id += 1;
                } else if matches!(
                    tile_type,
                    TileType::IoTop | TileType::IoBottom | TileType::IoLeft | TileType::IoRight
                ) {
                    for iob in 0..2 {
                        for (suffix, dir) in [("I", WireType::BelPin), ("O", WireType::BelPin)] {
                            let w = Wire {
                                id: WireId(wire_id),
                                name: format!("R{}C{}_PIO{}_{}", y, x, iob, suffix),
                                wire_type: dir,
                                tile_x: x,
                                tile_y: y,
                                delay: 0,
                            };
                            self.wire_names.insert(w.name.clone(), w.id);
                            self.bel_wires
                                .insert((x, y, format!("PIO{}_{}", iob, suffix)), w.id);
                            self.tile_wires.entry((x, y)).or_default().push(w.id);
                            self.wires.push(w);
                            wire_id += 1;
                        }
                    }
                }
            }
        }

        // Inter-tile routing
        for y in 1..height - 1 {
            for x in 1..width - 1 {
                if self.tiles[y as usize][x as usize].is_none() {
                    continue;
                }

                // ECP5 span wires: H01/V01 (span-1), H02/V02 (span-2), H06/V06 (span-6)
                // Span-2
                for i in 0..6u8 {
                    let h2 = Wire {
                        id: WireId(wire_id),
                        name: format!("R{}C{}_H02_{}", y, x, i),
                        wire_type: WireType::Span4H(i),
                        tile_x: x,
                        tile_y: y,
                        delay: 120,
                    };
                    self.wire_names.insert(h2.name.clone(), h2.id);
                    self.tile_wires.entry((x, y)).or_default().push(h2.id);
                    let h2_id = h2.id;
                    self.wires.push(h2);
                    wire_id += 1;

                    // local → H02
                    let li = (i as usize * 3) % 20;
                    if let Some(locals) = self.tile_wires.get(&(x, y)) {
                        if let Some(&lw) = locals.get(li) {
                            let p = Pip {
                                id: PipId(pip_id),
                                src_wire: lw,
                                dst_wire: h2_id,
                                delay: 80,
                                configurable: true,
                                tile_x: x,
                                tile_y: y,
                            };
                            self.wire_to_pips.entry(p.dst_wire).or_default().push(p.id);
                            self.wire_src_pips.entry(p.src_wire).or_default().push(p.id);
                            self.pips.push(p);
                            pip_id += 1;
                        }
                    }

                    // H02 → local at x+2
                    let dx = x + 2;
                    if dx < width - 1 {
                        let dli = (i as usize * 3 + 1) % 20;
                        if let Some(dl) = self.tile_wires.get(&(dx, y)) {
                            if let Some(&dlw) = dl.get(dli) {
                                let p = Pip {
                                    id: PipId(pip_id),
                                    src_wire: h2_id,
                                    dst_wire: dlw,
                                    delay: 50,
                                    configurable: true,
                                    tile_x: dx,
                                    tile_y: y,
                                };
                                self.wire_to_pips.entry(p.dst_wire).or_default().push(p.id);
                                self.wire_src_pips.entry(p.src_wire).or_default().push(p.id);
                                self.pips.push(p);
                                pip_id += 1;
                            }
                        }
                    }

                    // Vertical span-2
                    let v2 = Wire {
                        id: WireId(wire_id),
                        name: format!("R{}C{}_V02_{}", y, x, i),
                        wire_type: WireType::Span4V(i),
                        tile_x: x,
                        tile_y: y,
                        delay: 120,
                    };
                    self.wire_names.insert(v2.name.clone(), v2.id);
                    self.tile_wires.entry((x, y)).or_default().push(v2.id);
                    let v2_id = v2.id;
                    self.wires.push(v2);
                    wire_id += 1;

                    let li2 = (i as usize * 3 + 2) % 20;
                    if let Some(locals) = self.tile_wires.get(&(x, y)) {
                        if let Some(&lw) = locals.get(li2) {
                            let p = Pip {
                                id: PipId(pip_id),
                                src_wire: lw,
                                dst_wire: v2_id,
                                delay: 80,
                                configurable: true,
                                tile_x: x,
                                tile_y: y,
                            };
                            self.wire_to_pips.entry(p.dst_wire).or_default().push(p.id);
                            self.wire_src_pips.entry(p.src_wire).or_default().push(p.id);
                            self.pips.push(p);
                            pip_id += 1;
                        }
                    }

                    let dy = y + 2;
                    if dy < height - 1 {
                        let dli2 = (i as usize * 3 + 1) % 20;
                        if let Some(dl) = self.tile_wires.get(&(x, dy)) {
                            if let Some(&dlw) = dl.get(dli2) {
                                let p = Pip {
                                    id: PipId(pip_id),
                                    src_wire: v2_id,
                                    dst_wire: dlw,
                                    delay: 50,
                                    configurable: true,
                                    tile_x: x,
                                    tile_y: dy,
                                };
                                self.wire_to_pips.entry(p.dst_wire).or_default().push(p.id);
                                self.wire_src_pips.entry(p.src_wire).or_default().push(p.id);
                                self.pips.push(p);
                                pip_id += 1;
                            }
                        }
                    }
                }

                // Span-6 wires (3 per direction)
                for i in 0..3u8 {
                    for (dir, span, wire_type) in [
                        ("H06", 6i32, WireType::Span12H(i)),
                        ("V06", 6i32, WireType::Span12V(i)),
                    ] {
                        let w = Wire {
                            id: WireId(wire_id),
                            name: format!("R{}C{}_{}_{}", y, x, dir, i),
                            wire_type,
                            tile_x: x,
                            tile_y: y,
                            delay: 250,
                        };
                        self.wire_names.insert(w.name.clone(), w.id);
                        self.tile_wires.entry((x, y)).or_default().push(w.id);
                        let span_id = w.id;
                        self.wires.push(w);
                        wire_id += 1;

                        // local → span
                        let li = (i as usize * 7) % 20;
                        if let Some(locals) = self.tile_wires.get(&(x, y)) {
                            if let Some(&lw) = locals.get(li) {
                                let p = Pip {
                                    id: PipId(pip_id),
                                    src_wire: lw,
                                    dst_wire: span_id,
                                    delay: 100,
                                    configurable: true,
                                    tile_x: x,
                                    tile_y: y,
                                };
                                self.wire_to_pips.entry(p.dst_wire).or_default().push(p.id);
                                self.wire_src_pips.entry(p.src_wire).or_default().push(p.id);
                                self.pips.push(p);
                                pip_id += 1;
                            }
                        }

                        // span → local at destination
                        let (dest_x, dest_y) = if dir == "H06" {
                            ((x as i32 + span).min(width as i32 - 2) as u32, y)
                        } else {
                            (x, (y as i32 + span).min(height as i32 - 2) as u32)
                        };
                        if dest_x > 0 && dest_y > 0 {
                            let dli = (i as usize * 7 + 3) % 20;
                            if let Some(dl) = self.tile_wires.get(&(dest_x, dest_y)) {
                                if let Some(&dlw) = dl.get(dli) {
                                    let p = Pip {
                                        id: PipId(pip_id),
                                        src_wire: span_id,
                                        dst_wire: dlw,
                                        delay: 60,
                                        configurable: true,
                                        tile_x: dest_x,
                                        tile_y: dest_y,
                                    };
                                    self.wire_to_pips.entry(p.dst_wire).or_default().push(p.id);
                                    self.wire_src_pips.entry(p.src_wire).or_default().push(p.id);
                                    self.pips.push(p);
                                    pip_id += 1;
                                }
                            }
                        }
                    }
                }

                // Neighbour wires (4 directions)
                for (dx, dy, dir) in [(1i32, 0i32, "E"), (-1, 0, "W"), (0, 1, "N"), (0, -1, "S")] {
                    let nx = x as i32 + dx;
                    let ny = y as i32 + dy;
                    if nx < 0 || ny < 0 || nx >= width as i32 || ny >= height as i32 {
                        continue;
                    }
                    let (nx, ny) = (nx as u32, ny as u32);

                    let w = Wire {
                        id: WireId(wire_id),
                        name: format!("R{}C{}_N_{}", y, x, dir),
                        wire_type: WireType::Neighbour,
                        tile_x: x,
                        tile_y: y,
                        delay: 50,
                    };
                    self.wire_names.insert(w.name.clone(), w.id);
                    self.tile_wires.entry((x, y)).or_default().push(w.id);
                    let neigh_id = w.id;
                    self.wires.push(w);
                    wire_id += 1;

                    let src_li = match dir {
                        "E" => 0,
                        "W" => 5,
                        "N" => 10,
                        _ => 15,
                    };
                    if let Some(locals) = self.tile_wires.get(&(x, y)) {
                        if let Some(&lw) = locals.get(src_li) {
                            let p = Pip {
                                id: PipId(pip_id),
                                src_wire: lw,
                                dst_wire: neigh_id,
                                delay: 30,
                                configurable: true,
                                tile_x: x,
                                tile_y: y,
                            };
                            self.wire_to_pips.entry(p.dst_wire).or_default().push(p.id);
                            self.wire_src_pips.entry(p.src_wire).or_default().push(p.id);
                            self.pips.push(p);
                            pip_id += 1;
                        }
                    }

                    let dst_li = match dir {
                        "E" => 2,
                        "W" => 7,
                        "N" => 12,
                        _ => 17,
                    };
                    if let Some(dl) = self.tile_wires.get(&(nx, ny)) {
                        if let Some(&dlw) = dl.get(dst_li) {
                            let p = Pip {
                                id: PipId(pip_id),
                                src_wire: neigh_id,
                                dst_wire: dlw,
                                delay: 30,
                                configurable: true,
                                tile_x: nx,
                                tile_y: ny,
                            };
                            self.wire_to_pips.entry(p.dst_wire).or_default().push(p.id);
                            self.wire_src_pips.entry(p.src_wire).or_default().push(p.id);
                            self.pips.push(p);
                            pip_id += 1;
                        }
                    }
                }
            }
        }

        // Global clocks
        for gclk in 0..8u8 {
            let w = Wire {
                id: WireId(wire_id),
                name: format!("ECLK_{}", gclk),
                wire_type: WireType::Global(gclk),
                tile_x: 0,
                tile_y: 0,
                delay: 60,
            };
            self.wire_names.insert(w.name.clone(), w.id);
            self.wires.push(w);
            wire_id += 1;
        }
    }

    fn build_synthetic_packages(&mut self) {
        // Generate synthetic pin mappings for all real packages per variant
        // Real pin-to-pad mappings are in prjtrellis iodb.json
        let pkg_names = self.variant.packages();

        for &pkg_name in pkg_names {
            let mut pins = HashMap::new();
            let mut idx = 0u32;

            for io_tile in &self.io_tiles {
                for iob in 0..io_tile.io_count {
                    pins.insert(format!("P{}", idx), (io_tile.x, io_tile.y, iob));
                    idx += 1;
                }
            }

            self.packages.insert(
                pkg_name.to_string(),
                PackagePins {
                    name: pkg_name.to_string(),
                    pins,
                },
            );
        }
    }

    /// Routing architecture from prjtrellis/nextpnr wire type classification.
    fn default_routing() -> RoutingArchitecture {
        RoutingArchitecture {
            channels: data::ROUTING_CHANNELS,
            switch_pattern: SwitchPattern::Wilton,
            wire_segments: vec![
                // H00/V00: local wires
                WireSegment {
                    length: 1,
                    count: data::WIRE_LOCAL_COUNT,
                    direction: WireDirection::Bidirectional,
                },
                // H01/V01: span-1 short wires
                WireSegment {
                    length: 1,
                    count: data::WIRE_SPAN1_COUNT,
                    direction: WireDirection::Horizontal,
                },
                WireSegment {
                    length: 1,
                    count: data::WIRE_SPAN1_COUNT,
                    direction: WireDirection::Vertical,
                },
                // H02/V02: span-2 medium wires
                WireSegment {
                    length: 2,
                    count: data::WIRE_SPAN2_COUNT,
                    direction: WireDirection::Horizontal,
                },
                WireSegment {
                    length: 2,
                    count: data::WIRE_SPAN2_COUNT,
                    direction: WireDirection::Vertical,
                },
                // H06/V06: span-6 long wires
                WireSegment {
                    length: 6,
                    count: data::WIRE_SPAN6_COUNT,
                    direction: WireDirection::Horizontal,
                },
                WireSegment {
                    length: 6,
                    count: data::WIRE_SPAN6_COUNT,
                    direction: WireDirection::Vertical,
                },
            ],
        }
    }

    /// Clock resources from prjtrellis globals.json.
    fn default_clock_resources(variant: Ecp5Variant) -> ClockResources {
        ClockResources {
            global_clocks: data::GLOBAL_CLOCKS,
            plls: variant.pll_count(),
            dlls: variant.dll_count(),
            clock_domains: vec![
                ClockDomain {
                    name: "PRIMARY".to_string(),
                    max_frequency: data::MAX_FABRIC_FREQ,
                },
                ClockDomain {
                    name: "SECONDARY".to_string(),
                    max_frequency: data::MAX_FABRIC_FREQ,
                },
                ClockDomain {
                    name: "ECLK".to_string(),
                    max_frequency: if variant.has_serdes() {
                        variant.serdes_rate() * 1.0e9
                    } else {
                        data::MAX_FABRIC_FREQ
                    },
                },
            ],
        }
    }
}

impl Device for Ecp5Device {
    fn family(&self) -> DeviceFamily {
        DeviceFamily::Ecp5
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
            total_dsps: self.dsp_tiles.len() * 2,
            total_gclks: 8,
        }
    }

    fn tile_at(&self, x: u32, y: u32) -> Option<&dyn Tile> {
        self.tiles
            .get(y as usize)
            .and_then(|row| row.get(x as usize))
            .and_then(|t| t.as_ref())
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

    fn wire_src_pips(&self, wire_id: WireId) -> Vec<PipId> {
        self.wire_src_pips
            .get(&wire_id)
            .cloned()
            .unwrap_or_default()
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
        match cell_type {
            "LUT4" | "TRELLIS_SLICE" | "PFUMX" | "L6MUX21" => bel_type == BelType::Lut4,
            "DFF" | "TRELLIS_FF" | "FD1P3IX" | "FD1P3DX" | "FD1S3BX" | "FD1S3AX" => {
                bel_type.is_ff()
            }
            "CCU2C" | "CARRY" => bel_type == BelType::Carry,
            "IB" | "OB" | "BB" | "TRELLIS_IO" | "PIO" => bel_type == BelType::IoCell,
            "DP16KD" | "PDPW16KD" | "SP16KD" => bel_type == BelType::RamSlice,
            "MULT18X18D" | "ALU54B" => bel_type == BelType::DspSlice,
            "EHXPLLL" | "PLL" => bel_type == BelType::Pll,
            _ => false,
        }
    }

    fn wire_count(&self) -> usize {
        self.wires.len()
    }

    fn lut_output_wire(&self, tile_x: u32, tile_y: u32, lc_idx: usize) -> Option<WireId> {
        self.bel_wires
            .get(&(tile_x, tile_y, format!("LUT{}_F", lc_idx)))
            .copied()
    }

    fn lut_input_wire(
        &self,
        tile_x: u32,
        tile_y: u32,
        lc_idx: usize,
        input_idx: usize,
    ) -> Option<WireId> {
        let pin = ["A", "B", "C", "D"][input_idx];
        self.bel_wires
            .get(&(tile_x, tile_y, format!("LUT{}_{}", lc_idx, pin)))
            .copied()
    }

    fn clock_wire(&self, tile_x: u32, tile_y: u32) -> Option<WireId> {
        self.bel_wires
            .get(&(tile_x, tile_y, "CLK".to_string()))
            .copied()
    }

    fn io_output_wire(&self, tile_x: u32, tile_y: u32, iob_idx: usize) -> Option<WireId> {
        self.bel_wires
            .get(&(tile_x, tile_y, format!("PIO{}_O", iob_idx)))
            .copied()
    }

    fn io_input_wire(&self, tile_x: u32, tile_y: u32, iob_idx: usize) -> Option<WireId> {
        self.bel_wires
            .get(&(tile_x, tile_y, format!("PIO{}_I", iob_idx)))
            .copied()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ecp5_device_12f() {
        let device = Ecp5Device::new(Ecp5Variant::Lfe5u12f);
        assert_eq!(device.family(), DeviceFamily::Ecp5);
        assert_eq!(device.name(), "LFE5U-12F");
        assert_eq!(device.grid_size(), (72, 50)); // same die as 25F
        let stats = device.stats();
        assert!(stats.total_luts > 500, "got {} LUTs", stats.total_luts);
        assert!(stats.total_ios > 30, "got {} IOs", stats.total_ios);
        assert!(
            stats.total_dsps > 0,
            "12F has 28 MULT18, got {} DSPs",
            stats.total_dsps
        );
        // Check IDCODE
        assert_eq!(Ecp5Variant::Lfe5u12f.idcode(), 0x2111_1043);
    }

    #[test]
    fn test_ecp5_device_85f() {
        let device = Ecp5Device::new(Ecp5Variant::Lfe5u85f);
        assert_eq!(device.name(), "LFE5U-85F");
        assert_eq!(device.grid_size(), (126, 95));
        let stats = device.stats();
        assert!(stats.total_luts > 5000, "got {} LUTs", stats.total_luts);
        assert!(stats.total_brams > 10, "got {} BRAMs", stats.total_brams);
        assert!(stats.total_dsps > 0, "got {} DSPs", stats.total_dsps);
        // Check bitstream geometry
        assert_eq!(Ecp5Variant::Lfe5u85f.bitstream_geometry(), (13294, 1136));
    }

    #[test]
    fn test_ecp5_um5g_serdes() {
        let v = Ecp5Variant::Lfe5um5g85f;
        assert!(v.has_serdes());
        assert_eq!(v.serdes_rate(), 5.0);
        assert_eq!(v.serdes_channels(), 4);

        let v_base = Ecp5Variant::Lfe5u85f;
        assert!(!v_base.has_serdes());
        assert_eq!(v_base.serdes_channels(), 0);
    }

    #[test]
    fn test_ecp5_tile_access() {
        let device = Ecp5Device::new(Ecp5Variant::Lfe5u25f);
        let (w, h) = device.grid_size();
        let mid_x = w / 2;
        let mid_y = h / 2;

        if let Some(tile) = device.tile_at(mid_x, mid_y) {
            let tt = tile.tile_type();
            // Should be logic, BRAM, or DSP (not I/O since mid is interior)
            assert!(
                !matches!(
                    tt,
                    TileType::IoTop | TileType::IoBottom | TileType::IoLeft | TileType::IoRight
                ),
                "interior tile should not be I/O, got {:?}",
                tt
            );
        }
    }

    #[test]
    fn test_ecp5_routing_exists() {
        let device = Ecp5Device::new(Ecp5Variant::Lfe5u25f);
        assert!(device.wire_count() > 0);
        assert!(!device.pips.is_empty());
    }

    #[test]
    fn test_ecp5_can_place() {
        let device = Ecp5Device::new(Ecp5Variant::Lfe5u85f);
        assert!(device.can_place("LUT4", BelType::Lut4));
        assert!(device.can_place("TRELLIS_SLICE", BelType::Lut4));
        assert!(device.can_place("TRELLIS_FF", BelType::DffSrE));
        assert!(device.can_place("TRELLIS_IO", BelType::IoCell));
        assert!(device.can_place("DP16KD", BelType::RamSlice));
        assert!(device.can_place("MULT18X18D", BelType::DspSlice));
        assert!(!device.can_place("LUT4", BelType::IoCell));
    }
}
