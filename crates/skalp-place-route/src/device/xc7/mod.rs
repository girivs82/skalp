//! Xilinx 7-series FPGA device database
//!
//! Provides the device database for Xilinx 7-series FPGAs (Artix-7, Kintex-7,
//! Spartan-7). These devices have open-source toolchain support via Project
//! X-Ray (prjxray) and the F4PGA (formerly SymbiFlow) flow.
//!
//! Device parameters, grid sizes, resource counts, IDCODEs, and timing data
//! are derived from the Project X-Ray database and Xilinx datasheets (DS180,
//! DS181, DS182, UG474, UG473, UG479).
//!
//! Project X-Ray database is dedicated to the public domain under CC0 1.0.
//! See: <https://github.com/f4pga/prjxray> and <https://github.com/f4pga/prjxray-db>
//!
//! # Architecture Notes
//!
//! Xilinx 7-series uses 6-input LUTs (LUT6), unlike Lattice's 4-input LUTs.
//! Each slice contains 4 LUT6 + 8 FFs (A/B/C/D positions), and each CLB
//! contains 2 slices (SLICEL + SLICEL/SLICEM). Routing uses INT_L/INT_R
//! interconnect tiles with single/double/quad/long span wires.
//!
//! # Supported Variants
//!
//! - XC7A35T (20.8K LUT6, 50 BRAM36, 90 DSP48E1)
//! - XC7A50T (32.6K LUT6, 75 BRAM36, 120 DSP48E1)
//! - XC7A100T (63.4K LUT6, 135 BRAM36, 240 DSP48E1)
//! - XC7A200T (134.6K LUT6, 365 BRAM36, 740 DSP48E1, 16 GTP, PCIe)
//! - XC7K70T (41K LUT6, 135 BRAM36, 240 DSP48E1, 8 GTX, PCIe)
//! - XC7K160T (101.4K LUT6, 325 BRAM36, 600 DSP48E1, 8 GTX, PCIe)
//! - XC7K325T (203.8K LUT6, 445 BRAM36, 840 DSP48E1, 16 GTX, PCIe)
//! - XC7S50 (32.6K LUT6, 75 BRAM36, 120 DSP48E1)

pub mod data;
mod tiles;

pub use tiles::Xc7Tile;

use super::{
    Bel, BelId, BelPin, BelType, ClockDomain, ClockResources, Device, DeviceFamily, DeviceStats,
    DspTile, IoSide, IoTile, LogicTile, MemoryBlock, PackagePins, PinDirection, Pip, PipId,
    RoutingArchitecture, SwitchPattern, Tile, TileType, Wire, WireDirection, WireId, WireSegment,
    WireType,
};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Xilinx 7-series device variants
///
/// Resource counts from prjxray-db and Xilinx DS180 Table 1.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Xc7Variant {
    /// XC7A35T — 20.8K LUT6, 50 BRAM36, 90 DSP48E1, 5 CMT (131×106 grid)
    Xc7a35t,
    /// XC7A50T — 32.6K LUT6, 75 BRAM36, 120 DSP48E1, 5 CMT (131×106 grid)
    Xc7a50t,
    /// XC7A100T — 63.4K LUT6, 135 BRAM36, 240 DSP48E1, 6 CMT (185×178 grid)
    Xc7a100t,
    /// XC7A200T — 134.6K LUT6, 365 BRAM36, 740 DSP48E1, 16 GTP, PCIe (221×260 grid)
    Xc7a200t,
    /// XC7K70T — 41K LUT6, 135 BRAM36, 240 DSP48E1, 8 GTX, PCIe (165×150 grid)
    Xc7k70t,
    /// XC7K160T — 101.4K LUT6, 325 BRAM36, 600 DSP48E1, 8 GTX, PCIe (203×222 grid)
    Xc7k160t,
    /// XC7K325T — 203.8K LUT6, 445 BRAM36, 840 DSP48E1, 16 GTX, PCIe (237×365 grid)
    Xc7k325t,
    /// XC7S50 — 32.6K LUT6, 75 BRAM36, 120 DSP48E1, 5 CMT (131×106 grid)
    Xc7s50,
}

impl Xc7Variant {
    /// Get the die data record for this variant
    pub fn die_data(&self) -> &'static data::Xc7DieData {
        match self {
            Xc7Variant::Xc7a35t => &data::XC7A35T,
            Xc7Variant::Xc7a50t => &data::XC7A50T,
            Xc7Variant::Xc7a100t => &data::XC7A100T,
            Xc7Variant::Xc7a200t => &data::XC7A200T,
            Xc7Variant::Xc7k70t => &data::XC7K70T,
            Xc7Variant::Xc7k160t => &data::XC7K160T,
            Xc7Variant::Xc7k325t => &data::XC7K325T,
            Xc7Variant::Xc7s50 => &data::XC7S50,
        }
    }

    /// Grid dimensions from prjxray-db tilegrid.json
    pub fn grid_size(&self) -> (u32, u32) { self.die_data().grid }
    pub fn name(&self) -> &'static str { self.die_data().name }
    pub fn lut_count(&self) -> usize { self.die_data().lut6s }
    pub fn slice_count(&self) -> usize { self.die_data().slices }
    pub fn bram_count(&self) -> usize { self.die_data().bram36s }
    pub fn dsp_count(&self) -> usize { self.die_data().dsp48e1s }
    pub fn io_count(&self) -> usize { self.die_data().max_ios }
    pub fn mmcm_count(&self) -> u8 { self.die_data().mmcms }
    pub fn pll_count(&self) -> u8 { self.die_data().plls }

    #[allow(dead_code)]
    pub fn idcode(&self) -> u32 { self.die_data().idcode }

    #[allow(dead_code)]
    pub fn packages(&self) -> &'static [&'static str] { self.die_data().packages }

    /// Has GTX/GTP transceivers
    pub fn has_transceivers(&self) -> bool {
        let die = self.die_data();
        die.gtps > 0 || die.gtxs > 0
    }

    /// Has hard PCIe block
    pub fn has_pcie(&self) -> bool { self.die_data().pcie_blocks > 0 }

    /// Speed family for timing selection
    pub fn speed_family(&self) -> data::Xc7SpeedFamily { self.die_data().speed_family }

    /// Frame geometry for bitstream generation
    pub fn frame_geometry(&self) -> &'static data::Xc7FrameGeometry {
        match self {
            Xc7Variant::Xc7a35t => &data::FRAMES_XC7A35T,
            Xc7Variant::Xc7a50t => &data::FRAMES_XC7A50T,
            Xc7Variant::Xc7a100t => &data::FRAMES_XC7A100T,
            Xc7Variant::Xc7a200t => &data::FRAMES_XC7A200T,
            Xc7Variant::Xc7k70t => &data::FRAMES_XC7K70T,
            Xc7Variant::Xc7k160t => &data::FRAMES_XC7K160T,
            Xc7Variant::Xc7k325t => &data::FRAMES_XC7K325T,
            Xc7Variant::Xc7s50 => &data::FRAMES_XC7S50,
        }
    }
}

impl std::fmt::Display for Xc7Variant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name())
    }
}

/// Xilinx 7-series device
#[derive(Debug, Clone)]
pub struct Xc7Device {
    pub variant: Xc7Variant,
    grid_size: (u32, u32),
    tiles: Vec<Vec<Option<Xc7Tile>>>,
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

impl Xc7Device {
    /// Create a new Xilinx 7-series device with synthetic architecture
    pub fn new(variant: Xc7Variant) -> Self {
        Self::new_synthetic(variant)
    }

    fn new_synthetic(variant: Xc7Variant) -> Self {
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

        // 7-series tile layout (simplified from prjxray tilegrid):
        // - Row 0, height-1: I/O (IOB tiles)
        // - Col 0, width-1: I/O (IOB tiles)
        // - BRAM columns at regular intervals
        // - DSP columns at regular intervals
        // - Logic elsewhere (CLB tiles with SLICEL/SLICEM)
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
                            Some(Xc7Tile::new(TileType::Empty, x, y, Vec::new()));
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
                        diff_pairs: data::IO_DIFF_PAIRS,
                    });
                    self.tiles[y as usize][x as usize] = Some(Xc7Tile::new(side, x, y, io_bels));
                } else if is_dsp_col {
                    let dsp_bels = Self::make_dsp_bels(&mut bel_id);
                    self.dsp_tiles.push(DspTile { x, y, mac_count: 1 });
                    self.tiles[y as usize][x as usize] =
                        Some(Xc7Tile::new(TileType::Dsp, x, y, dsp_bels));
                } else if is_bram_col {
                    let bram_bels = Self::make_bram_bels(&mut bel_id);
                    self.memory_blocks.push(MemoryBlock {
                        x,
                        y,
                        size_bits: data::BRAM36_SIZE_BITS,
                        widths: data::BRAM_WIDTHS.to_vec(),
                    });
                    self.tiles[y as usize][x as usize] =
                        Some(Xc7Tile::new(TileType::RamTop, x, y, bram_bels));
                } else {
                    // Logic tile: 7-series CLB — 2 slices × 4 LUT6 = 8 LUT6 per tile
                    let logic_bels = Self::make_logic_bels(&mut bel_id);
                    self.logic_tiles.push(LogicTile {
                        x,
                        y,
                        lut_count: data::LOGIC_LUTS_PER_TILE,
                        ff_count: data::LOGIC_FFS_PER_TILE,
                        has_carry: true,
                    });
                    self.tiles[y as usize][x as usize] =
                        Some(Xc7Tile::new(TileType::Logic, x, y, logic_bels));
                }
            }
        }
    }

    /// Create BELs for a CLB tile: 2 slices × (4 LUT6 + 8 FF + 1 CARRY4)
    fn make_logic_bels(bel_id: &mut u32) -> Vec<Bel> {
        let mut bels = Vec::with_capacity(26); // 8 LUT6 + 16 FF + 2 CARRY4
        let positions = ["A", "B", "C", "D"];

        // Two slices per CLB
        for slice in 0..2 {
            let prefix = if slice == 0 { "L" } else { "M" };

            // 4 LUT6 per slice (positions A/B/C/D)
            for (_i, pos) in positions.iter().enumerate() {
                let id = BelId(*bel_id);
                *bel_id += 1;
                bels.push(Bel {
                    id,
                    bel_type: BelType::Lut6,
                    name: format!("SLICE{}_{}{}_LUT", prefix, pos, "6"),
                    pins: vec![
                        BelPin { name: "A1".into(), direction: PinDirection::Input, wire: None },
                        BelPin { name: "A2".into(), direction: PinDirection::Input, wire: None },
                        BelPin { name: "A3".into(), direction: PinDirection::Input, wire: None },
                        BelPin { name: "A4".into(), direction: PinDirection::Input, wire: None },
                        BelPin { name: "A5".into(), direction: PinDirection::Input, wire: None },
                        BelPin { name: "A6".into(), direction: PinDirection::Input, wire: None },
                        BelPin { name: "O6".into(), direction: PinDirection::Output, wire: None },
                        BelPin { name: "O5".into(), direction: PinDirection::Output, wire: None },
                    ],
                });

                // 2 FFs per position (primary + secondary in UG474 terms)
                for ff_idx in 0..2 {
                    let id = BelId(*bel_id);
                    *bel_id += 1;
                    let ff_name = if ff_idx == 0 {
                        format!("SLICE{}_{}_FF", prefix, pos)
                    } else {
                        format!("SLICE{}_{}_FF2", prefix, pos)
                    };
                    bels.push(Bel {
                        id,
                        bel_type: BelType::DffSrE,
                        name: ff_name,
                        pins: vec![
                            BelPin { name: "D".into(), direction: PinDirection::Input, wire: None },
                            BelPin { name: "C".into(), direction: PinDirection::Input, wire: None },
                            BelPin { name: "CE".into(), direction: PinDirection::Input, wire: None },
                            BelPin { name: "R".into(), direction: PinDirection::Input, wire: None },
                            BelPin { name: "Q".into(), direction: PinDirection::Output, wire: None },
                        ],
                    });
                }
            }

            // 1 CARRY4 per slice
            let id = BelId(*bel_id);
            *bel_id += 1;
            bels.push(Bel {
                id,
                bel_type: BelType::Carry,
                name: format!("SLICE{}_CARRY4", prefix),
                pins: vec![
                    BelPin { name: "CI".into(), direction: PinDirection::Input, wire: None },
                    BelPin { name: "CO3".into(), direction: PinDirection::Output, wire: None },
                    BelPin { name: "CO2".into(), direction: PinDirection::Output, wire: None },
                    BelPin { name: "CO1".into(), direction: PinDirection::Output, wire: None },
                    BelPin { name: "CO0".into(), direction: PinDirection::Output, wire: None },
                ],
            });
        }

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
                name: format!("IOB_{}", i),
                pins: vec![
                    BelPin { name: "PAD".into(), direction: PinDirection::Inout, wire: None },
                    BelPin { name: "I".into(), direction: PinDirection::Output, wire: None },
                    BelPin { name: "O".into(), direction: PinDirection::Input, wire: None },
                    BelPin { name: "T".into(), direction: PinDirection::Input, wire: None },
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
            name: "RAMB36E1".into(),
            pins: vec![
                BelPin { name: "ADDRA".into(), direction: PinDirection::Input, wire: None },
                BelPin { name: "ADDRB".into(), direction: PinDirection::Input, wire: None },
                BelPin { name: "DIA".into(), direction: PinDirection::Input, wire: None },
                BelPin { name: "DIB".into(), direction: PinDirection::Input, wire: None },
                BelPin { name: "DOA".into(), direction: PinDirection::Output, wire: None },
                BelPin { name: "DOB".into(), direction: PinDirection::Output, wire: None },
                BelPin { name: "CLKA".into(), direction: PinDirection::Input, wire: None },
                BelPin { name: "CLKB".into(), direction: PinDirection::Input, wire: None },
                BelPin { name: "WEA".into(), direction: PinDirection::Input, wire: None },
                BelPin { name: "WEB".into(), direction: PinDirection::Input, wire: None },
                BelPin { name: "ENA".into(), direction: PinDirection::Input, wire: None },
                BelPin { name: "ENB".into(), direction: PinDirection::Input, wire: None },
            ],
        }]
    }

    fn make_dsp_bels(bel_id: &mut u32) -> Vec<Bel> {
        let id = BelId(*bel_id);
        *bel_id += 1;
        vec![Bel {
            id,
            bel_type: BelType::DspSlice,
            name: "DSP48E1".into(),
            pins: vec![
                BelPin { name: "A".into(), direction: PinDirection::Input, wire: None },
                BelPin { name: "B".into(), direction: PinDirection::Input, wire: None },
                BelPin { name: "C".into(), direction: PinDirection::Input, wire: None },
                BelPin { name: "D".into(), direction: PinDirection::Input, wire: None },
                BelPin { name: "P".into(), direction: PinDirection::Output, wire: None },
                BelPin { name: "CLK".into(), direction: PinDirection::Input, wire: None },
                BelPin { name: "CEA".into(), direction: PinDirection::Input, wire: None },
                BelPin { name: "CEB".into(), direction: PinDirection::Input, wire: None },
                BelPin { name: "RST".into(), direction: PinDirection::Input, wire: None },
            ],
        }]
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

                // Local wires (INT tile has ~24 IMUX/bounce/bypass wires)
                for i in 0..24u8 {
                    let w = Wire {
                        id: WireId(wire_id),
                        name: format!("R{}C{}_INT_{}", y, x, i),
                        wire_type: WireType::Local(i),
                        tile_x: x,
                        tile_y: y,
                        delay: 30, // 30ps — 7-series 28nm
                    };
                    self.wire_names.insert(w.name.clone(), w.id);
                    self.tile_wires.entry((x, y)).or_default().push(w.id);
                    self.wires.push(w);
                    wire_id += 1;
                }

                if tile_type == TileType::Logic {
                    // 8 LUT6 outputs + 8 LUT6 input sets + 16 FF outputs + clock
                    for lc in 0..8 {
                        // LUT output (O6)
                        let out_wire = Wire {
                            id: WireId(wire_id),
                            name: format!("R{}C{}_LUT{}_O6", y, x, lc),
                            wire_type: WireType::BelPin,
                            tile_x: x,
                            tile_y: y,
                            delay: 0,
                        };
                        self.wire_names.insert(out_wire.name.clone(), out_wire.id);
                        self.bel_wires
                            .insert((x, y, format!("LUT{}_O6", lc)), out_wire.id);
                        self.tile_wires.entry((x, y)).or_default().push(out_wire.id);
                        self.wires.push(out_wire);
                        wire_id += 1;

                        // LUT inputs (A1-A6)
                        for inp in 0..6 {
                            let in_wire = Wire {
                                id: WireId(wire_id),
                                name: format!("R{}C{}_LUT{}_A{}", y, x, lc, inp + 1),
                                wire_type: WireType::BelPin,
                                tile_x: x,
                                tile_y: y,
                                delay: 0,
                            };
                            self.wire_names.insert(in_wire.name.clone(), in_wire.id);
                            self.bel_wires
                                .insert((x, y, format!("LUT{}_A{}", lc, inp + 1)), in_wire.id);
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
                        let lut_out_id = WireId(wire_id - 8); // 6 inputs + 1 FF back
                        let local_idx = (lc * 3) % 24;
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

                        // Local → LUT input PIPs (connect 3 locals to each of 6 inputs)
                        for inp in 0..6 {
                            if let Some(&lut_in_id) = self
                                .bel_wires
                                .get(&(x, y, format!("LUT{}_A{}", lc, inp + 1)))
                            {
                                for lo in 0..3u8 {
                                    let li = ((lc * 3 + inp + lo as usize) % 24) as usize;
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
                        delay: 50,
                    };
                    self.wire_names.insert(clk_wire.name.clone(), clk_wire.id);
                    self.bel_wires.insert((x, y, "CLK".to_string()), clk_wire.id);
                    self.tile_wires.entry((x, y)).or_default().push(clk_wire.id);
                    self.wires.push(clk_wire);
                    wire_id += 1;
                } else if matches!(
                    tile_type,
                    TileType::IoTop | TileType::IoBottom | TileType::IoLeft | TileType::IoRight
                ) {
                    for iob in 0..2 {
                        for suffix in ["I", "O"] {
                            let w = Wire {
                                id: WireId(wire_id),
                                name: format!("R{}C{}_IOB{}_{}", y, x, iob, suffix),
                                wire_type: WireType::BelPin,
                                tile_x: x,
                                tile_y: y,
                                delay: 0,
                            };
                            self.wire_names.insert(w.name.clone(), w.id);
                            self.bel_wires
                                .insert((x, y, format!("IOB{}_{}", iob, suffix)), w.id);
                            self.tile_wires.entry((x, y)).or_default().push(w.id);
                            self.wires.push(w);
                            wire_id += 1;
                        }
                    }
                }
            }
        }

        // Inter-tile routing: single/double/quad/long span wires
        for y in 1..height - 1 {
            for x in 1..width - 1 {
                if self.tiles[y as usize][x as usize].is_none() {
                    continue;
                }

                // Single wires (span 1) — NN1/SS1/EE1/WW1
                for i in 0..data::WIRE_SINGLE_COUNT {
                    let sh = Wire {
                        id: WireId(wire_id),
                        name: format!("R{}C{}_EE1_{}", y, x, i),
                        wire_type: WireType::Span4H(i), // reuse Span4H for single/double
                        tile_x: x,
                        tile_y: y,
                        delay: 90,
                    };
                    self.wire_names.insert(sh.name.clone(), sh.id);
                    self.tile_wires.entry((x, y)).or_default().push(sh.id);
                    let sh_id = sh.id;
                    self.wires.push(sh);
                    wire_id += 1;

                    // local → single
                    let li = (i as usize * 3) % 24;
                    if let Some(locals) = self.tile_wires.get(&(x, y)) {
                        if let Some(&lw) = locals.get(li) {
                            let p = Pip {
                                id: PipId(pip_id),
                                src_wire: lw,
                                dst_wire: sh_id,
                                delay: 60,
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

                    // single → local at x+1
                    let dx = x + 1;
                    if dx < width - 1 {
                        let dli = (i as usize * 3 + 1) % 24;
                        if let Some(dl) = self.tile_wires.get(&(dx, y)) {
                            if let Some(&dlw) = dl.get(dli) {
                                let p = Pip {
                                    id: PipId(pip_id),
                                    src_wire: sh_id,
                                    dst_wire: dlw,
                                    delay: 40,
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

                    // Vertical single
                    let sv = Wire {
                        id: WireId(wire_id),
                        name: format!("R{}C{}_NN1_{}", y, x, i),
                        wire_type: WireType::Span4V(i),
                        tile_x: x,
                        tile_y: y,
                        delay: 90,
                    };
                    self.wire_names.insert(sv.name.clone(), sv.id);
                    self.tile_wires.entry((x, y)).or_default().push(sv.id);
                    let sv_id = sv.id;
                    self.wires.push(sv);
                    wire_id += 1;

                    let li2 = (i as usize * 3 + 2) % 24;
                    if let Some(locals) = self.tile_wires.get(&(x, y)) {
                        if let Some(&lw) = locals.get(li2) {
                            let p = Pip {
                                id: PipId(pip_id),
                                src_wire: lw,
                                dst_wire: sv_id,
                                delay: 60,
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

                    let dy = y + 1;
                    if dy < height - 1 {
                        let dli2 = (i as usize * 3 + 1) % 24;
                        if let Some(dl) = self.tile_wires.get(&(x, dy)) {
                            if let Some(&dlw) = dl.get(dli2) {
                                let p = Pip {
                                    id: PipId(pip_id),
                                    src_wire: sv_id,
                                    dst_wire: dlw,
                                    delay: 40,
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

                // Long wires (span 12) — LH/LV
                for i in 0..data::WIRE_LONG_COUNT {
                    for (dir, span, wire_type) in [
                        ("LH", 12i32, WireType::Span12H(i)),
                        ("LV", 12i32, WireType::Span12V(i)),
                    ] {
                        let w = Wire {
                            id: WireId(wire_id),
                            name: format!("R{}C{}_{}_{}", y, x, dir, i),
                            wire_type,
                            tile_x: x,
                            tile_y: y,
                            delay: 300,
                        };
                        self.wire_names.insert(w.name.clone(), w.id);
                        self.tile_wires.entry((x, y)).or_default().push(w.id);
                        let span_id = w.id;
                        self.wires.push(w);
                        wire_id += 1;

                        // local → long
                        let li = (i as usize * 6) % 24;
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

                        // long → local at destination
                        let (dest_x, dest_y) = if dir == "LH" {
                            ((x as i32 + span).min(width as i32 - 2) as u32, y)
                        } else {
                            (x, (y as i32 + span).min(height as i32 - 2) as u32)
                        };
                        if dest_x > 0 && dest_y > 0 {
                            let dli = (i as usize * 6 + 3) % 24;
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

                // Neighbour wires (4 directions)
                for (dx, dy, dir) in
                    [(1i32, 0i32, "E"), (-1, 0, "W"), (0, 1, "N"), (0, -1, "S")]
                {
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
                        delay: 40,
                    };
                    self.wire_names.insert(w.name.clone(), w.id);
                    self.tile_wires.entry((x, y)).or_default().push(w.id);
                    let neigh_id = w.id;
                    self.wires.push(w);
                    wire_id += 1;

                    let src_li = match dir {
                        "E" => 0,
                        "W" => 6,
                        "N" => 12,
                        _ => 18,
                    };
                    if let Some(locals) = self.tile_wires.get(&(x, y)) {
                        if let Some(&lw) = locals.get(src_li) {
                            let p = Pip {
                                id: PipId(pip_id),
                                src_wire: lw,
                                dst_wire: neigh_id,
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

                    let dst_li = match dir {
                        "E" => 3,
                        "W" => 9,
                        "N" => 15,
                        _ => 21,
                    };
                    if let Some(dl) = self.tile_wires.get(&(nx, ny)) {
                        if let Some(&dlw) = dl.get(dst_li) {
                            let p = Pip {
                                id: PipId(pip_id),
                                src_wire: neigh_id,
                                dst_wire: dlw,
                                delay: 25,
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

        // Global clocks (BUFG)
        for gclk in 0..data::BUFG_COUNT.min(16) {
            let w = Wire {
                id: WireId(wire_id),
                name: format!("GCLK_{}", gclk),
                wire_type: WireType::Global(gclk as u8),
                tile_x: 0,
                tile_y: 0,
                delay: 50,
            };
            self.wire_names.insert(w.name.clone(), w.id);
            self.wires.push(w);
            wire_id += 1;
        }
    }

    fn build_synthetic_packages(&mut self) {
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

    /// Routing architecture from prjxray-db INT tile wire classification.
    fn default_routing() -> RoutingArchitecture {
        RoutingArchitecture {
            channels: data::ROUTING_CHANNELS,
            switch_pattern: SwitchPattern::Wilton,
            wire_segments: vec![
                // Local wires (IMUX, bounce, bypass)
                WireSegment {
                    length: 1,
                    count: data::WIRE_LOCAL_COUNT,
                    direction: WireDirection::Bidirectional,
                },
                // Single wires (NN1/SS1/EE1/WW1 — span 1)
                WireSegment {
                    length: 1,
                    count: data::WIRE_SINGLE_COUNT,
                    direction: WireDirection::Horizontal,
                },
                WireSegment {
                    length: 1,
                    count: data::WIRE_SINGLE_COUNT,
                    direction: WireDirection::Vertical,
                },
                // Double wires (NN2/SS2/EE2/WW2 — span 2)
                WireSegment {
                    length: 2,
                    count: data::WIRE_DOUBLE_COUNT,
                    direction: WireDirection::Horizontal,
                },
                WireSegment {
                    length: 2,
                    count: data::WIRE_DOUBLE_COUNT,
                    direction: WireDirection::Vertical,
                },
                // Quad wires (NN4/SS4/EE4/WW4 — span 4-6)
                WireSegment {
                    length: 4,
                    count: data::WIRE_QUAD_COUNT,
                    direction: WireDirection::Horizontal,
                },
                WireSegment {
                    length: 4,
                    count: data::WIRE_QUAD_COUNT,
                    direction: WireDirection::Vertical,
                },
                // Long wires (LH/LV — span 12)
                WireSegment {
                    length: 12,
                    count: data::WIRE_LONG_COUNT,
                    direction: WireDirection::Horizontal,
                },
                WireSegment {
                    length: 12,
                    count: data::WIRE_LONG_COUNT,
                    direction: WireDirection::Vertical,
                },
            ],
        }
    }

    /// Clock resources from prjxray-db and UG472.
    fn default_clock_resources(variant: Xc7Variant) -> ClockResources {
        let die = variant.die_data();
        let max_freq = match variant.speed_family() {
            data::Xc7SpeedFamily::Artix | data::Xc7SpeedFamily::Spartan => {
                data::MAX_GCLK_FREQ_ARTIX
            }
            data::Xc7SpeedFamily::Kintex => data::MAX_GCLK_FREQ_KINTEX,
        };

        ClockResources {
            global_clocks: data::BUFG_COUNT.min(16),
            plls: die.plls,
            dlls: 0, // 7-series uses MMCM, not DLL
            clock_domains: vec![
                ClockDomain {
                    name: "GCLK".to_string(),
                    max_frequency: max_freq,
                },
                ClockDomain {
                    name: "MMCM".to_string(),
                    max_frequency: max_freq * 2.0, // VCO can be higher
                },
            ],
        }
    }
}

impl Device for Xc7Device {
    fn family(&self) -> DeviceFamily {
        DeviceFamily::Xc7
    }

    fn name(&self) -> &str {
        self.variant.name()
    }

    fn grid_size(&self) -> (u32, u32) {
        self.grid_size
    }

    fn stats(&self) -> DeviceStats {
        DeviceStats {
            total_luts: self.logic_tiles.len() * data::LOGIC_LUTS_PER_TILE as usize,
            total_ffs: self.logic_tiles.len() * data::LOGIC_FFS_PER_TILE as usize,
            total_ios: self.io_tiles.len() * data::IO_CELLS_PER_TILE as usize,
            total_brams: self.memory_blocks.len(),
            total_dsps: self.dsp_tiles.len(),
            total_gclks: data::BUFG_COUNT.min(16) as usize,
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
            // 7-series LUT primitives
            "LUT1" | "LUT2" | "LUT3" | "LUT4" | "LUT5" | "LUT6" | "LUT6_2" => {
                bel_type == BelType::Lut6
            }
            // Also accept Lut4 mapping for smaller LUTs
            t if t.starts_with("LUT") => bel_type == BelType::Lut6 || bel_type == BelType::Lut4,
            // FF primitives
            "FDRE" | "FDSE" | "FDCE" | "FDPE" | "FD" | "FDE" | "FDR" | "FDS" | "LDCE"
            | "LDPE" => bel_type.is_ff(),
            // Carry
            "CARRY4" => bel_type == BelType::Carry,
            // I/O
            "IBUF" | "IBUFDS" | "OBUF" | "OBUFDS" | "IOBUF" | "OBUFT" => {
                bel_type == BelType::IoCell
            }
            // BRAM
            "RAMB36E1" | "RAMB18E1" | "FIFO36E1" | "FIFO18E1" => bel_type == BelType::RamSlice,
            // DSP
            "DSP48E1" => bel_type == BelType::DspSlice,
            // Clock
            "BUFG" | "BUFGCTRL" | "BUFR" | "BUFMR" => bel_type == BelType::GlobalBuf,
            "MMCME2_ADV" | "MMCME2_BASE" | "PLLE2_ADV" | "PLLE2_BASE" => {
                bel_type == BelType::Pll
            }
            _ => false,
        }
    }

    fn wire_count(&self) -> usize {
        self.wires.len()
    }

    fn lut_output_wire(&self, tile_x: u32, tile_y: u32, lc_idx: usize) -> Option<WireId> {
        self.bel_wires
            .get(&(tile_x, tile_y, format!("LUT{}_O6", lc_idx)))
            .copied()
    }

    fn lut_input_wire(
        &self,
        tile_x: u32,
        tile_y: u32,
        lc_idx: usize,
        input_idx: usize,
    ) -> Option<WireId> {
        // 7-series has 6 inputs (A1-A6) vs ice40's 4
        self.bel_wires
            .get(&(tile_x, tile_y, format!("LUT{}_A{}", lc_idx, input_idx + 1)))
            .copied()
    }

    fn clock_wire(&self, tile_x: u32, tile_y: u32) -> Option<WireId> {
        self.bel_wires
            .get(&(tile_x, tile_y, "CLK".to_string()))
            .copied()
    }

    fn io_output_wire(&self, tile_x: u32, tile_y: u32, iob_idx: usize) -> Option<WireId> {
        self.bel_wires
            .get(&(tile_x, tile_y, format!("IOB{}_O", iob_idx)))
            .copied()
    }

    fn io_input_wire(&self, tile_x: u32, tile_y: u32, iob_idx: usize) -> Option<WireId> {
        self.bel_wires
            .get(&(tile_x, tile_y, format!("IOB{}_I", iob_idx)))
            .copied()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_xc7_device_creation_a35t() {
        let device = Xc7Device::new(Xc7Variant::Xc7a35t);
        assert_eq!(device.family(), DeviceFamily::Xc7);
        assert_eq!(device.name(), "xc7a35t");
        assert_eq!(device.grid_size(), (131, 106));
        let stats = device.stats();
        assert!(stats.total_luts > 500, "got {} LUTs", stats.total_luts);
        assert!(stats.total_ffs > 500, "got {} FFs", stats.total_ffs);
        assert!(stats.total_ios > 30, "got {} IOs", stats.total_ios);
        assert!(stats.total_brams > 0, "got {} BRAMs", stats.total_brams);
        assert!(stats.total_dsps > 0, "got {} DSPs", stats.total_dsps);
    }

    #[test]
    fn test_xc7_device_creation_k325t() {
        let device = Xc7Device::new(Xc7Variant::Xc7k325t);
        assert_eq!(device.family(), DeviceFamily::Xc7);
        assert_eq!(device.name(), "xc7k325t");
        assert_eq!(device.grid_size(), (237, 365));
        let stats = device.stats();
        assert!(stats.total_luts > 10_000, "got {} LUTs", stats.total_luts);
        assert!(stats.total_brams > 0, "K325T has 445 BRAM36, got {} BRAMs", stats.total_brams);
        assert!(stats.total_dsps > 0, "K325T has 840 DSP48E1, got {} DSPs", stats.total_dsps);
        // Check IDCODE
        assert_eq!(Xc7Variant::Xc7k325t.idcode(), 0x0365_1093);
    }

    #[test]
    fn test_xc7_variant_accessors() {
        assert_eq!(Xc7Variant::Xc7a100t.lut_count(), 63_400);
        assert_eq!(Xc7Variant::Xc7a100t.bram_count(), 135);
        assert_eq!(Xc7Variant::Xc7a100t.dsp_count(), 240);
        assert!(!Xc7Variant::Xc7a100t.has_pcie());
        assert!(Xc7Variant::Xc7k325t.has_pcie());
        assert!(Xc7Variant::Xc7k325t.has_transceivers());
    }

    #[test]
    fn test_xc7_can_place() {
        let device = Xc7Device::new(Xc7Variant::Xc7a35t);
        assert!(device.can_place("LUT6", BelType::Lut6));
        assert!(device.can_place("LUT4", BelType::Lut6)); // LUT4 fits in LUT6
        assert!(device.can_place("FDRE", BelType::DffSrE));
        assert!(device.can_place("CARRY4", BelType::Carry));
        assert!(device.can_place("IBUF", BelType::IoCell));
        assert!(device.can_place("RAMB36E1", BelType::RamSlice));
        assert!(device.can_place("DSP48E1", BelType::DspSlice));
        assert!(!device.can_place("LUT6", BelType::Carry));
    }

    #[test]
    fn test_xc7_wire_connectivity() {
        let device = Xc7Device::new(Xc7Variant::Xc7a35t);
        // Check that logic tiles have BEL wires
        if let Some(logic_tile) = device.logic_tiles.first() {
            let (x, y) = (logic_tile.x, logic_tile.y);
            assert!(device.lut_output_wire(x, y, 0).is_some());
            assert!(device.lut_input_wire(x, y, 0, 0).is_some());
            assert!(device.clock_wire(x, y).is_some());
        }
    }

    #[test]
    fn test_xc7_all_variants() {
        for variant in [
            Xc7Variant::Xc7a35t,
            Xc7Variant::Xc7a50t,
            Xc7Variant::Xc7a100t,
            Xc7Variant::Xc7a200t,
            Xc7Variant::Xc7k70t,
            Xc7Variant::Xc7k160t,
            Xc7Variant::Xc7k325t,
            Xc7Variant::Xc7s50,
        ] {
            let device = Xc7Device::new(variant);
            let stats = device.stats();
            assert!(stats.total_luts > 0, "{} has no LUTs", variant.name());
            assert!(stats.total_ios > 0, "{} has no IOs", variant.name());
        }
    }
}
