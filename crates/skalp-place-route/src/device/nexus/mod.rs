//! Lattice Nexus (CertusPro-NX, CrossLink-NX) FPGA device database
//!
//! Device parameters derived from Project Oxide (prjoxide) reverse-engineering database.
//! Copyright (C) 2020-21 gatecat <gatecat@ds0.me>
//! Licensed under ISC License — see COPYING in prjoxide repository.
//!
//! Timing data from prjoxide LIFCL speed grade 10 (fast corner).
//! LFCPNX uses the same Nexus fabric; timing is a close approximation.
//!
//! # Supported Variants
//!
//! - LIFCL-40 — 32,256 LUT4, 88x57 grid, packages QFN72/csfBGA289/caBGA400
//! - LFCPNX-100 — 79,872 LUT4, 160x75 grid, KarythraGPU target (LFCPNX-VERSA-EVN)

pub mod data;
pub mod prjoxide_bels;
pub mod prjoxide_graph;
pub mod prjoxide_load;
pub mod prjoxide_pack;
mod tiles;

pub use prjoxide_graph::{resolve_wire, NodeKey};
pub use prjoxide_load::{load_tilegrid, PrjoxideTile};
pub use tiles::NexusTile;

use super::{
    Bel, BelId, BelPin, BelType, ClockDomain, ClockResources, Device, DeviceFamily, DeviceStats,
    DspTile, IoSide, IoTile, LogicTile, MemoryBlock, PackagePins, PinDirection, Pip, PipId,
    RoutingArchitecture, SwitchPattern, Tile, TileType, Wire, WireDirection, WireId, WireSegment,
    WireType,
};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Nexus device variants
///
/// Resource counts from prjoxide tile database (actual tile counts, not datasheet marketing).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum NexusVariant {
    /// LIFCL-40 (CrossLink-NX)
    /// 4,032 PLC tiles = 32,256 LUT4 + 32,256 FF
    /// 21 EBR (18Kb), 2 LRAM (32Kb), ~14 DSP blocks, 3 PLLs
    /// Packages: QFN72, csfBGA289, caBGA400
    Lifcl40,
    /// LFCPNX-100 (CertusPro-NX) — KarythraGPU target
    /// 9,984 PLC tiles = 79,872 LUT4 + 79,872 FF
    /// 52 EBR (18Kb), 7 LRAM (32Kb), ~39 DSP blocks, 4 PLLs
    /// Hard PCIe (1x PCIE_LL), 8x PCS (SerDes)
    /// Packages: ASG256, CBG256, BBG484, BFG484, LFG672
    Lfcpnx100,
}

impl NexusVariant {
    /// Get the data record for this variant
    pub fn die_data(&self) -> &'static data::NexusDieData {
        match self {
            NexusVariant::Lifcl40 => &data::LIFCL_40,
            NexusVariant::Lfcpnx100 => &data::LFCPNX_100,
        }
    }

    pub fn grid_size(&self) -> (u32, u32) {
        self.die_data().grid
    }
    pub fn name(&self) -> &'static str {
        self.die_data().name
    }
    pub fn idcode(&self) -> u32 {
        self.die_data().idcode
    }
    pub fn bitstream_frames(&self) -> u32 {
        self.die_data().bitstream_frames
    }
    pub fn bits_per_frame(&self) -> u32 {
        self.die_data().bits_per_frame
    }
    pub fn plc_count(&self) -> usize {
        self.die_data().plc_tiles
    }
    pub fn lut_count(&self) -> usize {
        self.die_data().plc_tiles * 8
    }
    pub fn ff_count(&self) -> usize {
        self.lut_count()
    }
    pub fn ebr_count(&self) -> usize {
        self.die_data().ebr_blocks
    }
    pub fn lram_count(&self) -> usize {
        self.die_data().lram_blocks
    }
    pub fn dsp_count(&self) -> usize {
        self.die_data().dsp_blocks
    }
    pub fn io_count(&self) -> usize {
        self.die_data().io_tiles
    }
    pub fn pcs_count(&self) -> u8 {
        self.die_data().pcs_channels
    }
    pub fn pll_count(&self) -> u8 {
        self.die_data().plls
    }
    pub fn global_clocks(&self) -> u8 {
        self.die_data().global_clocks
    }
    pub fn has_pcie(&self) -> bool {
        self.die_data().has_pcie
    }
    pub fn has_lpddr4(&self) -> bool {
        self.die_data().has_lpddr4
    }
    pub fn packages(&self) -> &'static [&'static str] {
        self.die_data().packages
    }
}

impl std::fmt::Display for NexusVariant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name())
    }
}

/// Lattice Nexus (CertusPro-NX) device
#[derive(Debug, Clone)]
pub struct NexusDevice {
    /// Device variant
    pub variant: NexusVariant,
    /// Grid dimensions
    grid_size: (u32, u32),
    /// Tiles indexed by [y][x]
    tiles: Vec<Vec<Option<NexusTile>>>,
    /// All wires
    wires: Vec<Wire>,
    /// Wire name to ID mapping
    wire_names: HashMap<String, WireId>,
    /// All PIPs
    pips: Vec<Pip>,
    /// Wire to PIP mapping (PIPs that drive this wire)
    wire_to_pips: HashMap<WireId, Vec<PipId>>,
    /// Wire to PIP mapping (PIPs driven by this wire)
    wire_src_pips: HashMap<WireId, Vec<PipId>>,
    /// Tile to wire mapping
    tile_wires: HashMap<(u32, u32), Vec<WireId>>,
    /// BEL pin to wire mapping
    bel_wires: HashMap<(u32, u32, String), WireId>,
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
    /// Memory blocks (EBR)
    pub memory_blocks: Vec<MemoryBlock>,
    /// DSP tiles
    pub dsp_tiles: Vec<DspTile>,
}

/// Placement priority when multiple prjoxide tiles share a grid cell: the most
/// placeable wins the single `tiles[y][x]` slot (routing-only tiles rank lowest).
fn placement_rank(t: &TileType) -> u8 {
    match t {
        TileType::Logic => 6,
        TileType::RamTop | TileType::RamBottom => 5,
        TileType::Dsp => 4,
        TileType::IoTop | TileType::IoBottom | TileType::IoLeft | TileType::IoRight => 3,
        TileType::Pll => 2,
        TileType::GlobalBuf | TileType::IpCon => 1,
        TileType::Empty => 0,
    }
}

impl NexusDevice {
    /// Create a new Nexus device with synthetic architecture
    pub fn new(variant: NexusVariant) -> Self {
        Self::new_synthetic(variant)
    }

    /// Build a Nexus device from the real prjoxide database, restricted to the
    /// inclusive grid window `bbox = (x0, y0, x1, y1)`.
    ///
    /// This is the *real-silicon* constructor (M1): the tile grid, routing graph,
    /// and BEL/wire bindings all come from prjoxide, so placement and routing run
    /// on the actual LFCPNX-100 fabric. A bounded window keeps construction cheap
    /// for bring-up; the full fabric is ~28M pips (see [`prjoxide_graph`]).
    ///
    /// `db_root` is the prjoxide database root (see
    /// [`prjoxide_load::find_database`]). Returns an error if the database can't
    /// be read.
    pub fn from_prjoxide_bbox(
        variant: NexusVariant,
        db_root: &std::path::Path,
        bbox: (u32, u32, u32, u32),
    ) -> Result<Self, String> {
        use prjoxide_graph::{build_routing_graph, node_name, resolve_wire};

        let (width, height) = variant.grid_size();
        let family = variant.prjoxide_family();

        // M1a: real tile grid. M1b: real routing graph for the window.
        let all_tiles = prjoxide_load::load_tilegrid(db_root, variant)?;
        let graph = build_routing_graph(db_root, family, &all_tiles, Some(bbox))?;

        let mut tiles: Vec<Vec<Option<NexusTile>>> =
            vec![vec![None; width as usize]; height as usize];
        let mut bel_wires: HashMap<(u32, u32, String), WireId> = HashMap::new();
        let mut logic_tiles: Vec<LogicTile> = Vec::new();
        let mut bel_id_counter = 0u32;

        // Pick the primary placeable tile per cell (PLC wins; routing tiles are
        // skipped here but still contribute to the graph). Within the bbox.
        let (x0, y0, x1, y1) = bbox;
        // Group prjoxide tiles by cell, preferring PLC.
        let mut primary: HashMap<(u32, u32), &prjoxide_load::PrjoxideTile> = HashMap::new();
        for t in &all_tiles {
            if t.x < x0 || t.x > x1 || t.y < y0 || t.y > y1 {
                continue;
            }
            let better = match primary.get(&(t.x, t.y)) {
                None => true,
                Some(cur) => placement_rank(&t.skalp_type) > placement_rank(&cur.skalp_type),
            };
            if better {
                primary.insert((t.x, t.y), t);
            }
        }

        for (&(x, y), t) in &primary {
            let bels = if t.tiletype == "PLC" {
                let mut bels = Vec::with_capacity(16);
                for pb in prjoxide_bels::plc_bels() {
                    let mut pins = Vec::with_capacity(pb.pins.len());
                    for (conv_key, site_wire, dir) in &pb.pins {
                        let wire = resolve_wire(x, y, site_wire)
                            .and_then(|n| graph.wire_names.get(&node_name(&n)).copied());
                        if let Some(w) = wire {
                            bel_wires.insert((x, y, conv_key.clone()), w);
                        }
                        // Pin name = conventional key's suffix after the BEL name.
                        let pin_name = conv_key
                            .rsplit_once('_')
                            .map(|(_, p)| p.to_string())
                            .unwrap_or_else(|| conv_key.clone());
                        pins.push(BelPin {
                            name: pin_name,
                            direction: *dir,
                            wire,
                        });
                    }
                    bels.push(Bel {
                        id: BelId(bel_id_counter),
                        bel_type: pb.bel_type,
                        name: pb.name,
                        pins,
                    });
                    bel_id_counter += 1;
                }
                // Coarse per-tile clock wire for Device::clock_wire.
                if let Some(w) = resolve_wire(x, y, prjoxide_bels::plc_clock_site_wire())
                    .and_then(|n| graph.wire_names.get(&node_name(&n)).copied())
                {
                    bel_wires.insert((x, y, "CLK".to_string()), w);
                }
                logic_tiles.push(LogicTile {
                    x,
                    y,
                    lut_count: data::LOGIC_LUTS_PER_TILE,
                    ff_count: data::LOGIC_FFS_PER_TILE,
                    has_carry: true,
                });
                bels
            } else {
                // Non-logic primary tiles: correct type, BELs deferred (M3+).
                Vec::new()
            };
            tiles[y as usize][x as usize] = Some(NexusTile::new(t.skalp_type, x, y, bels));
        }

        Ok(Self {
            variant,
            grid_size: (width, height),
            tiles,
            wires: graph.wires,
            wire_names: graph.wire_names,
            pips: graph.pips,
            wire_to_pips: graph.wire_to_pips,
            wire_src_pips: graph.wire_src_pips,
            tile_wires: graph.tile_wires,
            bel_wires,
            packages: HashMap::new(), // M3: package pins from prjoxide iodb
            routing: Self::default_routing(variant),
            clock_resources: Self::default_clock_resources(variant),
            logic_tiles,
            io_tiles: Vec::new(),
            memory_blocks: Vec::new(),
            dsp_tiles: Vec::new(),
        })
    }

    /// Create a synthetic Nexus device from datasheet parameters
    fn new_synthetic(variant: NexusVariant) -> Self {
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
            routing: Self::default_routing(variant),
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

    /// Build tile grid from synthetic architecture
    fn build_synthetic_tiles(&mut self) {
        let (width, height) = self.grid_size;
        let mut bel_id_counter = 0u32;

        // Nexus tile layout:
        // - Row 0, Row height-1: I/O rows (top/bottom)
        // - Col 0, Col width-1: I/O columns (left/right)
        // - Interior: logic tiles with periodic BRAM/DSP columns
        // - BRAM columns at every ~10th column
        // - DSP columns at every ~20th column (LFCPNX-100 only)

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
                    // I/O tile
                    let side = if is_bottom {
                        TileType::IoBottom
                    } else if is_top {
                        TileType::IoTop
                    } else if is_left {
                        TileType::IoLeft
                    } else {
                        TileType::IoRight
                    };

                    // Skip corners — they're empty
                    if (is_top || is_bottom) && (is_left || is_right) {
                        self.tiles[y as usize][x as usize] =
                            Some(NexusTile::new(TileType::Empty, x, y, Vec::new()));
                        continue;
                    }

                    let io_bels = Self::make_io_bels(&mut bel_id_counter, x, y);
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
                    self.tiles[y as usize][x as usize] = Some(NexusTile::new(side, x, y, io_bels));
                } else if is_bram_col && !is_dsp_col {
                    // EBR (Embedded Block RAM) — 18Kb
                    let bram_bels = Self::make_bram_bels(&mut bel_id_counter, x, y);
                    self.memory_blocks.push(MemoryBlock {
                        x,
                        y,
                        size_bits: data::EBR_SIZE_BITS,
                        widths: data::EBR_WIDTHS.to_vec(),
                    });
                    // Use RamTop as the tile type (Nexus EBR occupies a single tile)
                    self.tiles[y as usize][x as usize] =
                        Some(NexusTile::new(TileType::RamTop, x, y, bram_bels));
                } else if is_dsp_col {
                    // DSP block (18x18 MAC)
                    let dsp_bels = Self::make_dsp_bels(&mut bel_id_counter, x, y);
                    self.dsp_tiles.push(DspTile {
                        x,
                        y,
                        mac_count: data::DSP_MACS_PER_TILE,
                    });
                    self.tiles[y as usize][x as usize] =
                        Some(NexusTile::new(TileType::Dsp, x, y, dsp_bels));
                } else {
                    // Logic tile — Nexus CLC: 2 slices × 4 LUT4+FF = 8 LUT4 per tile
                    let logic_bels = Self::make_logic_bels(&mut bel_id_counter, x, y);
                    self.logic_tiles.push(LogicTile {
                        x,
                        y,
                        lut_count: data::LOGIC_LUTS_PER_TILE,
                        ff_count: data::LOGIC_FFS_PER_TILE,
                        has_carry: true,
                    });
                    self.tiles[y as usize][x as usize] =
                        Some(NexusTile::new(TileType::Logic, x, y, logic_bels));
                }
            }
        }
    }

    /// Create BELs for a logic tile (8 LUT4 + 8 DFF + carry chain)
    fn make_logic_bels(bel_id: &mut u32, _x: u32, _y: u32) -> Vec<Bel> {
        let mut bels = Vec::with_capacity(17);

        // 8 LUT4s (2 slices × 4 LUTs)
        for i in 0..8 {
            let id = BelId(*bel_id);
            *bel_id += 1;
            bels.push(Bel {
                id,
                bel_type: BelType::Lut4,
                name: format!("LUT4_{}", i),
                pins: vec![
                    BelPin {
                        name: "A".to_string(),
                        direction: PinDirection::Input,
                        wire: None,
                    },
                    BelPin {
                        name: "B".to_string(),
                        direction: PinDirection::Input,
                        wire: None,
                    },
                    BelPin {
                        name: "C".to_string(),
                        direction: PinDirection::Input,
                        wire: None,
                    },
                    BelPin {
                        name: "D".to_string(),
                        direction: PinDirection::Input,
                        wire: None,
                    },
                    BelPin {
                        name: "Z".to_string(),
                        direction: PinDirection::Output,
                        wire: None,
                    },
                ],
            });
        }

        // 8 DFFs (one per LUT)
        for i in 0..8 {
            let id = BelId(*bel_id);
            *bel_id += 1;
            bels.push(Bel {
                id,
                bel_type: BelType::DffSrE,
                name: format!("FF_{}", i),
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
                        name: "CE".to_string(),
                        direction: PinDirection::Input,
                        wire: None,
                    },
                    BelPin {
                        name: "LSR".to_string(),
                        direction: PinDirection::Input,
                        wire: None,
                    },
                    BelPin {
                        name: "Q".to_string(),
                        direction: PinDirection::Output,
                        wire: None,
                    },
                ],
            });
        }

        // 1 carry chain
        let id = BelId(*bel_id);
        *bel_id += 1;
        bels.push(Bel {
            id,
            bel_type: BelType::Carry,
            name: "CCU2C".to_string(),
            pins: vec![
                BelPin {
                    name: "CIN".to_string(),
                    direction: PinDirection::Input,
                    wire: None,
                },
                BelPin {
                    name: "COUT".to_string(),
                    direction: PinDirection::Output,
                    wire: None,
                },
            ],
        });

        bels
    }

    /// Create BELs for an I/O tile
    fn make_io_bels(bel_id: &mut u32, _x: u32, _y: u32) -> Vec<Bel> {
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
                        name: "PAD".to_string(),
                        direction: PinDirection::Inout,
                        wire: None,
                    },
                    BelPin {
                        name: "I".to_string(),
                        direction: PinDirection::Output,
                        wire: None,
                    },
                    BelPin {
                        name: "O".to_string(),
                        direction: PinDirection::Input,
                        wire: None,
                    },
                    BelPin {
                        name: "T".to_string(),
                        direction: PinDirection::Input,
                        wire: None,
                    },
                ],
            });
        }
        bels
    }

    /// Create BELs for an EBR tile
    fn make_bram_bels(bel_id: &mut u32, _x: u32, _y: u32) -> Vec<Bel> {
        let id = BelId(*bel_id);
        *bel_id += 1;
        vec![Bel {
            id,
            bel_type: BelType::RamSlice,
            name: "EBR".to_string(),
            pins: vec![
                BelPin {
                    name: "ADDRA".to_string(),
                    direction: PinDirection::Input,
                    wire: None,
                },
                BelPin {
                    name: "ADDRB".to_string(),
                    direction: PinDirection::Input,
                    wire: None,
                },
                BelPin {
                    name: "DIA".to_string(),
                    direction: PinDirection::Input,
                    wire: None,
                },
                BelPin {
                    name: "DIB".to_string(),
                    direction: PinDirection::Input,
                    wire: None,
                },
                BelPin {
                    name: "DOA".to_string(),
                    direction: PinDirection::Output,
                    wire: None,
                },
                BelPin {
                    name: "DOB".to_string(),
                    direction: PinDirection::Output,
                    wire: None,
                },
                BelPin {
                    name: "CLKA".to_string(),
                    direction: PinDirection::Input,
                    wire: None,
                },
                BelPin {
                    name: "CLKB".to_string(),
                    direction: PinDirection::Input,
                    wire: None,
                },
                BelPin {
                    name: "WEA".to_string(),
                    direction: PinDirection::Input,
                    wire: None,
                },
                BelPin {
                    name: "WEB".to_string(),
                    direction: PinDirection::Input,
                    wire: None,
                },
                BelPin {
                    name: "CEA".to_string(),
                    direction: PinDirection::Input,
                    wire: None,
                },
                BelPin {
                    name: "CEB".to_string(),
                    direction: PinDirection::Input,
                    wire: None,
                },
            ],
        }]
    }

    /// Create BELs for a DSP tile
    fn make_dsp_bels(bel_id: &mut u32, _x: u32, _y: u32) -> Vec<Bel> {
        let mut bels = Vec::with_capacity(2);
        for i in 0..2 {
            let id = BelId(*bel_id);
            *bel_id += 1;
            bels.push(Bel {
                id,
                bel_type: BelType::DspSlice,
                name: format!("MULT18X18_{}", i),
                pins: vec![
                    BelPin {
                        name: "A".to_string(),
                        direction: PinDirection::Input,
                        wire: None,
                    },
                    BelPin {
                        name: "B".to_string(),
                        direction: PinDirection::Input,
                        wire: None,
                    },
                    BelPin {
                        name: "P".to_string(),
                        direction: PinDirection::Output,
                        wire: None,
                    },
                    BelPin {
                        name: "CLK".to_string(),
                        direction: PinDirection::Input,
                        wire: None,
                    },
                    BelPin {
                        name: "CE".to_string(),
                        direction: PinDirection::Input,
                        wire: None,
                    },
                    BelPin {
                        name: "RST".to_string(),
                        direction: PinDirection::Input,
                        wire: None,
                    },
                ],
            });
        }
        bels
    }

    /// Build synthetic wires and PIPs for routing
    fn build_synthetic_wires_and_pips(&mut self) {
        let (width, height) = self.grid_size;
        let mut wire_id = 0u32;
        let mut pip_id = 0u32;

        // For each tile, create local wires and BEL pin wires
        for y in 0..height {
            for x in 0..width {
                if self.tiles[y as usize][x as usize].is_none() {
                    continue;
                }

                let tile_type = self.tiles[y as usize][x as usize]
                    .as_ref()
                    .unwrap()
                    .tile_type();

                // Local routing wires (Nexus has ~24 local wires per tile)
                for i in 0..24u8 {
                    let w = Wire {
                        id: WireId(wire_id),
                        name: format!("R{}C{}_LOCAL{}", y, x, i),
                        wire_type: WireType::Local(i),
                        tile_x: x,
                        tile_y: y,
                        delay: 30, // 30ps — Nexus 28nm is faster than iCE40 40nm
                    };
                    self.wire_names.insert(w.name.clone(), w.id);
                    self.tile_wires.entry((x, y)).or_default().push(w.id);
                    self.wires.push(w);
                    wire_id += 1;
                }

                // BEL pin wires for logic tiles
                if tile_type == TileType::Logic {
                    for lc in 0..8 {
                        // LUT output wire
                        let out_wire = Wire {
                            id: WireId(wire_id),
                            name: format!("R{}C{}_LUT{}_Z", y, x, lc),
                            wire_type: WireType::BelPin,
                            tile_x: x,
                            tile_y: y,
                            delay: 0,
                        };
                        self.wire_names.insert(out_wire.name.clone(), out_wire.id);
                        self.bel_wires
                            .insert((x, y, format!("LUT{}_Z", lc)), out_wire.id);
                        self.tile_wires.entry((x, y)).or_default().push(out_wire.id);
                        self.wires.push(out_wire);
                        wire_id += 1;

                        // LUT input wires (4 inputs)
                        for inp in 0..4 {
                            let pin_name = ["A", "B", "C", "D"][inp];
                            let in_wire = Wire {
                                id: WireId(wire_id),
                                name: format!("R{}C{}_LUT{}_{}", y, x, lc, pin_name),
                                wire_type: WireType::BelPin,
                                tile_x: x,
                                tile_y: y,
                                delay: 0,
                            };
                            self.wire_names.insert(in_wire.name.clone(), in_wire.id);
                            self.bel_wires
                                .insert((x, y, format!("LUT{}_{}", lc, pin_name)), in_wire.id);
                            self.tile_wires.entry((x, y)).or_default().push(in_wire.id);
                            self.wires.push(in_wire);
                            wire_id += 1;
                        }

                        // FF output wire
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

                        // Connect LUT output → local wire (BEL pin PIP)
                        let lut_out_id = WireId(wire_id - 7); // LUT_Z wire
                        let local_idx = (lc * 3) % 24;
                        if let Some(local_wires) = self.tile_wires.get(&(x, y)) {
                            if let Some(&local_wire) = local_wires.get(local_idx) {
                                let p = Pip {
                                    id: PipId(pip_id),
                                    src_wire: lut_out_id,
                                    dst_wire: local_wire,
                                    delay: 20,
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

                        // Connect local wires → LUT inputs (BEL pin PIPs)
                        for inp in 0..4 {
                            let pin_name = ["A", "B", "C", "D"][inp];
                            if let Some(&lut_in_id) =
                                self.bel_wires
                                    .get(&(x, y, format!("LUT{}_{}", lc, pin_name)))
                            {
                                // Connect from a few local wires
                                for local_offset in 0..4u8 {
                                    let local_idx = (lc * 3 + inp + local_offset as usize) % 24;
                                    if let Some(local_wires) = self.tile_wires.get(&(x, y)) {
                                        if let Some(&local_wire) = local_wires.get(local_idx) {
                                            let p = Pip {
                                                id: PipId(pip_id),
                                                src_wire: local_wire,
                                                dst_wire: lut_in_id,
                                                delay: 15,
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

                    // Clock wire for the tile
                    let clk_wire = Wire {
                        id: WireId(wire_id),
                        name: format!("R{}C{}_CLK", y, x),
                        wire_type: WireType::Global(0),
                        tile_x: x,
                        tile_y: y,
                        delay: 50,
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
                    // I/O BEL pin wires
                    for iob in 0..2 {
                        // Output from pad (input to fabric)
                        let i_wire = Wire {
                            id: WireId(wire_id),
                            name: format!("R{}C{}_PIO{}_I", y, x, iob),
                            wire_type: WireType::BelPin,
                            tile_x: x,
                            tile_y: y,
                            delay: 0,
                        };
                        self.wire_names.insert(i_wire.name.clone(), i_wire.id);
                        self.bel_wires
                            .insert((x, y, format!("PIO{}_I", iob)), i_wire.id);
                        self.tile_wires.entry((x, y)).or_default().push(i_wire.id);
                        self.wires.push(i_wire);
                        wire_id += 1;

                        // Input to pad (output from fabric)
                        let o_wire = Wire {
                            id: WireId(wire_id),
                            name: format!("R{}C{}_PIO{}_O", y, x, iob),
                            wire_type: WireType::BelPin,
                            tile_x: x,
                            tile_y: y,
                            delay: 0,
                        };
                        self.wire_names.insert(o_wire.name.clone(), o_wire.id);
                        self.bel_wires
                            .insert((x, y, format!("PIO{}_O", iob)), o_wire.id);
                        self.tile_wires.entry((x, y)).or_default().push(o_wire.id);
                        self.wires.push(o_wire);
                        wire_id += 1;
                    }
                }
            }
        }

        // Inter-tile routing: horizontal and vertical span wires
        // Nexus has H2/V2 (span-2), H6/V6 (span-6), and H12/V12 (span-12) wires
        for y in 1..height - 1 {
            for x in 1..width - 1 {
                if self.tiles[y as usize][x as usize].is_none() {
                    continue;
                }

                // Span-2 horizontal wires (8 per tile)
                for i in 0..8u8 {
                    let w = Wire {
                        id: WireId(wire_id),
                        name: format!("R{}C{}_H2_{}", y, x, i),
                        wire_type: WireType::Span4H(i), // reuse Span4H for H2
                        tile_x: x,
                        tile_y: y,
                        delay: 80,
                    };
                    self.wire_names.insert(w.name.clone(), w.id);
                    self.tile_wires.entry((x, y)).or_default().push(w.id);
                    let h2_id = w.id;
                    self.wires.push(w);
                    wire_id += 1;

                    // PIP: local → H2 (drive span wire)
                    let local_idx = (i as usize * 3) % 24;
                    if let Some(local_wires) = self.tile_wires.get(&(x, y)) {
                        if let Some(&local_wire) = local_wires.get(local_idx) {
                            let p = Pip {
                                id: PipId(pip_id),
                                src_wire: local_wire,
                                dst_wire: h2_id,
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

                    // PIP: H2 → local in destination tile (x+2)
                    let dest_x = x + 2;
                    if dest_x < width - 1 {
                        let dest_local_idx = (i as usize * 3 + 1) % 24;
                        if let Some(dest_locals) = self.tile_wires.get(&(dest_x, y)) {
                            if let Some(&dest_local) = dest_locals.get(dest_local_idx) {
                                let p = Pip {
                                    id: PipId(pip_id),
                                    src_wire: h2_id,
                                    dst_wire: dest_local,
                                    delay: 40,
                                    configurable: true,
                                    tile_x: dest_x,
                                    tile_y: y,
                                };
                                self.wire_to_pips.entry(p.dst_wire).or_default().push(p.id);
                                self.wire_src_pips.entry(p.src_wire).or_default().push(p.id);
                                self.pips.push(p);
                                pip_id += 1;
                            }
                        }
                    }
                }

                // Span-2 vertical wires (8 per tile)
                for i in 0..8u8 {
                    let w = Wire {
                        id: WireId(wire_id),
                        name: format!("R{}C{}_V2_{}", y, x, i),
                        wire_type: WireType::Span4V(i), // reuse Span4V for V2
                        tile_x: x,
                        tile_y: y,
                        delay: 80,
                    };
                    self.wire_names.insert(w.name.clone(), w.id);
                    self.tile_wires.entry((x, y)).or_default().push(w.id);
                    let v2_id = w.id;
                    self.wires.push(w);
                    wire_id += 1;

                    // PIP: local → V2
                    let local_idx = (i as usize * 3 + 2) % 24;
                    if let Some(local_wires) = self.tile_wires.get(&(x, y)) {
                        if let Some(&local_wire) = local_wires.get(local_idx) {
                            let p = Pip {
                                id: PipId(pip_id),
                                src_wire: local_wire,
                                dst_wire: v2_id,
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

                    // PIP: V2 → local in destination tile (y+2)
                    let dest_y = y + 2;
                    if dest_y < height - 1 {
                        let dest_local_idx = (i as usize * 3 + 1) % 24;
                        if let Some(dest_locals) = self.tile_wires.get(&(x, dest_y)) {
                            if let Some(&dest_local) = dest_locals.get(dest_local_idx) {
                                let p = Pip {
                                    id: PipId(pip_id),
                                    src_wire: v2_id,
                                    dst_wire: dest_local,
                                    delay: 40,
                                    configurable: true,
                                    tile_x: x,
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

                // Span-12 wires (4 per direction per tile) for long-distance routing
                for i in 0..4u8 {
                    // Horizontal span-12
                    let w = Wire {
                        id: WireId(wire_id),
                        name: format!("R{}C{}_H12_{}", y, x, i),
                        wire_type: WireType::Span12H(i),
                        tile_x: x,
                        tile_y: y,
                        delay: 200,
                    };
                    self.wire_names.insert(w.name.clone(), w.id);
                    self.tile_wires.entry((x, y)).or_default().push(w.id);
                    let h12_id = w.id;
                    self.wires.push(w);
                    wire_id += 1;

                    // PIP: local → H12
                    let local_idx = (i as usize * 6) % 24;
                    if let Some(local_wires) = self.tile_wires.get(&(x, y)) {
                        if let Some(&local_wire) = local_wires.get(local_idx) {
                            let p = Pip {
                                id: PipId(pip_id),
                                src_wire: local_wire,
                                dst_wire: h12_id,
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

                    // PIP: H12 → local at x+12
                    let dest_x = x + 12;
                    if dest_x < width - 1 {
                        let dest_local_idx = (i as usize * 6 + 3) % 24;
                        if let Some(dest_locals) = self.tile_wires.get(&(dest_x, y)) {
                            if let Some(&dest_local) = dest_locals.get(dest_local_idx) {
                                let p = Pip {
                                    id: PipId(pip_id),
                                    src_wire: h12_id,
                                    dst_wire: dest_local,
                                    delay: 50,
                                    configurable: true,
                                    tile_x: dest_x,
                                    tile_y: y,
                                };
                                self.wire_to_pips.entry(p.dst_wire).or_default().push(p.id);
                                self.wire_src_pips.entry(p.src_wire).or_default().push(p.id);
                                self.pips.push(p);
                                pip_id += 1;
                            }
                        }
                    }

                    // Vertical span-12
                    let w = Wire {
                        id: WireId(wire_id),
                        name: format!("R{}C{}_V12_{}", y, x, i),
                        wire_type: WireType::Span12V(i),
                        tile_x: x,
                        tile_y: y,
                        delay: 200,
                    };
                    self.wire_names.insert(w.name.clone(), w.id);
                    self.tile_wires.entry((x, y)).or_default().push(w.id);
                    let v12_id = w.id;
                    self.wires.push(w);
                    wire_id += 1;

                    // PIP: local → V12
                    let local_idx = (i as usize * 6 + 1) % 24;
                    if let Some(local_wires) = self.tile_wires.get(&(x, y)) {
                        if let Some(&local_wire) = local_wires.get(local_idx) {
                            let p = Pip {
                                id: PipId(pip_id),
                                src_wire: local_wire,
                                dst_wire: v12_id,
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

                    // PIP: V12 → local at y+12
                    let dest_y = y + 12;
                    if dest_y < height - 1 {
                        let dest_local_idx = (i as usize * 6 + 4) % 24;
                        if let Some(dest_locals) = self.tile_wires.get(&(x, dest_y)) {
                            if let Some(&dest_local) = dest_locals.get(dest_local_idx) {
                                let p = Pip {
                                    id: PipId(pip_id),
                                    src_wire: v12_id,
                                    dst_wire: dest_local,
                                    delay: 50,
                                    configurable: true,
                                    tile_x: x,
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

                // Neighbour wires (to adjacent tiles — 4 directions)
                for (dx, dy, dir_name) in
                    [(1i32, 0i32, "E"), (-1, 0, "W"), (0, 1, "N"), (0, -1, "S")]
                {
                    let nx = x as i32 + dx;
                    let ny = y as i32 + dy;
                    if nx < 0 || ny < 0 || nx >= width as i32 || ny >= height as i32 {
                        continue;
                    }
                    let nx = nx as u32;
                    let ny = ny as u32;

                    // Neighbour wire
                    let w = Wire {
                        id: WireId(wire_id),
                        name: format!("R{}C{}_NEIGH_{}", y, x, dir_name),
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

                    // PIP: local → neighbour
                    let local_idx = match dir_name {
                        "E" => 0,
                        "W" => 6,
                        "N" => 12,
                        _ => 18,
                    };
                    if let Some(local_wires) = self.tile_wires.get(&(x, y)) {
                        if let Some(&local_wire) = local_wires.get(local_idx) {
                            let p = Pip {
                                id: PipId(pip_id),
                                src_wire: local_wire,
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

                    // PIP: neighbour → local in target tile
                    let dest_local_idx = match dir_name {
                        "E" => 3,
                        "W" => 9,
                        "N" => 15,
                        _ => 21,
                    };
                    if let Some(dest_locals) = self.tile_wires.get(&(nx, ny)) {
                        if let Some(&dest_local) = dest_locals.get(dest_local_idx) {
                            let p = Pip {
                                id: PipId(pip_id),
                                src_wire: neigh_id,
                                dst_wire: dest_local,
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

        // Global clock wires (connect clock wire in each logic tile)
        for gclk in 0..self.variant.global_clocks() {
            let w = Wire {
                id: WireId(wire_id),
                name: format!("GCLK_{}", gclk),
                wire_type: WireType::Global(gclk),
                tile_x: 0,
                tile_y: 0,
                delay: 50, // Nexus global clock network is very low-skew
            };
            self.wire_names.insert(w.name.clone(), w.id);
            self.wires.push(w);
            wire_id += 1;
        }
    }

    /// Build synthetic package pin mappings
    fn build_synthetic_packages(&mut self) {
        let mut pins = HashMap::new();
        let mut pin_idx = 0u32;

        // Map I/O tiles to package pins
        for io_tile in &self.io_tiles {
            for iob in 0..io_tile.io_count {
                let pin_name = match self.variant {
                    NexusVariant::Lfcpnx100 => format!("P{}", pin_idx),
                    NexusVariant::Lifcl40 => format!("P{}", pin_idx),
                };
                pins.insert(pin_name, (io_tile.x, io_tile.y, iob));
                pin_idx += 1;
            }
        }

        // Use first package from prjoxide devices.json
        let pkg_name = self.variant.packages()[0];

        self.packages.insert(
            pkg_name.to_string(),
            PackagePins {
                name: pkg_name.to_string(),
                pins,
            },
        );
    }

    /// Routing architecture from prjoxide wire types
    ///
    /// Nexus interconnect (from prjoxide database):
    /// - H01/V01: span-1 (neighbour), local routing within CIB
    /// - H02/V02: span-2, short-distance
    /// - H06/V06: span-6, medium-distance
    /// - HPBX: horizontal branch (global clock distribution)
    /// - VPSX: vertical spine (global clock distribution)
    /// - CIB mux: configurable interconnect block routing
    fn default_routing(_variant: NexusVariant) -> RoutingArchitecture {
        RoutingArchitecture {
            channels: data::ROUTING_CHANNELS,
            switch_pattern: SwitchPattern::Wilton,
            wire_segments: vec![
                WireSegment {
                    length: 1,
                    count: data::WIRE_LOCAL_COUNT,
                    direction: WireDirection::Bidirectional,
                },
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

    fn default_clock_resources(variant: NexusVariant) -> ClockResources {
        ClockResources {
            global_clocks: variant.global_clocks(),
            plls: variant.pll_count(),
            dlls: data::DLLS,
            clock_domains: vec![
                ClockDomain {
                    name: "ECLK".to_string(),
                    max_frequency: data::MAX_ECLK_FREQ,
                },
                ClockDomain {
                    name: "PCLK".to_string(),
                    max_frequency: data::MAX_PCLK_FREQ,
                },
            ],
        }
    }
}

impl Device for NexusDevice {
    fn family(&self) -> DeviceFamily {
        DeviceFamily::Nexus
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
            total_dsps: self.dsp_tiles.len() * data::DSP_MACS_PER_TILE as usize,
            total_gclks: self.variant.global_clocks() as usize,
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
            "LUT4" | "OXIDE_COMB" => bel_type == BelType::Lut4,
            "DFF" | "OXIDE_FF" | "FD1P3IX" | "FD1P3DX" | "FD1S3BX" => bel_type.is_ff(),
            "CCU2C" | "CARRY" => bel_type == BelType::Carry,
            "IB" | "OB" | "BB" | "SEIO18" | "SEIO33" | "DIFFIO18" => bel_type == BelType::IoCell,
            "DP16KD" | "PDPSC16K" | "SP16KD" | "EBR" => bel_type == BelType::RamSlice,
            "MULT18X18D" | "MULT9X9D" | "DSP" => bel_type == BelType::DspSlice,
            "EHXPLLL" | "PLL" => bel_type == BelType::Pll,
            _ => false,
        }
    }

    fn wire_count(&self) -> usize {
        self.wires.len()
    }

    fn lut_output_wire(&self, tile_x: u32, tile_y: u32, lc_idx: usize) -> Option<WireId> {
        self.bel_wires
            .get(&(tile_x, tile_y, format!("LUT{}_Z", lc_idx)))
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
    fn test_nexus_device_creation_lifcl40() {
        let device = NexusDevice::new(NexusVariant::Lifcl40);
        assert_eq!(device.family(), DeviceFamily::Nexus);
        assert_eq!(device.name(), "LIFCL-40");
        let stats = device.stats();
        assert!(
            stats.total_luts > 1000,
            "expected >1000 LUTs, got {}",
            stats.total_luts
        );
        assert!(
            stats.total_ios > 50,
            "expected >50 IOs, got {}",
            stats.total_ios
        );
        assert!(stats.total_brams > 0, "expected BRAM blocks");
        assert!(stats.total_dsps > 0, "LIFCL-40 should have DSPs");
    }

    #[test]
    fn test_nexus_device_creation_lfcpnx100() {
        let device = NexusDevice::new(NexusVariant::Lfcpnx100);
        assert_eq!(device.name(), "LFCPNX-100");
        let stats = device.stats();
        assert!(
            stats.total_luts > 5000,
            "expected >5000 LUTs, got {}",
            stats.total_luts
        );
        assert!(
            stats.total_ios > 100,
            "expected >100 IOs, got {}",
            stats.total_ios
        );
        assert!(
            stats.total_brams > 10,
            "expected >10 BRAM blocks, got {}",
            stats.total_brams
        );
        assert!(stats.total_dsps > 0, "LFCPNX-100 should have DSPs");
    }

    #[test]
    fn test_nexus_tile_access() {
        let device = NexusDevice::new(NexusVariant::Lifcl40);
        let (w, h) = device.grid_size();

        // Interior tile should be logic
        let mid_x = w / 2;
        let mid_y = h / 2;
        if let Some(tile) = device.tile_at(mid_x, mid_y) {
            assert_eq!(tile.tile_type(), TileType::Logic);
            assert_eq!(tile.available_bels(BelType::Lut4), 8);
        }

        // Bottom edge should be I/O
        if let Some(tile) = device.tile_at(mid_x, 0) {
            assert!(
                matches!(tile.tile_type(), TileType::IoBottom),
                "bottom edge should be IoBottom, got {:?}",
                tile.tile_type()
            );
        }
    }

    #[test]
    fn test_nexus_routing_exists() {
        let device = NexusDevice::new(NexusVariant::Lifcl40);
        assert!(device.wire_count() > 0, "should have wires");
        assert!(!device.pips.is_empty(), "should have PIPs");

        // Check that interior tiles have wires
        let (w, h) = device.grid_size();
        let mid_x = w / 2;
        let mid_y = h / 2;
        let wires = device.tile_wires(mid_x, mid_y);
        assert!(!wires.is_empty(), "mid tile should have wires");
    }

    #[test]
    fn test_nexus_can_place() {
        let device = NexusDevice::new(NexusVariant::Lfcpnx100);
        assert!(device.can_place("LUT4", BelType::Lut4));
        assert!(device.can_place("OXIDE_COMB", BelType::Lut4));
        assert!(device.can_place("OXIDE_FF", BelType::DffSrE));
        assert!(device.can_place("IB", BelType::IoCell));
        assert!(device.can_place("DP16KD", BelType::RamSlice));
        assert!(device.can_place("MULT18X18D", BelType::DspSlice));
        assert!(!device.can_place("LUT4", BelType::IoCell));
    }

    /// M1c-part2: a device built from the real prjoxide database exposes, through
    /// the exact `Device` trait the Placer/Router use, real BELs whose pins bind to
    /// real routing nodes that are reachable through the fabric. This is the first
    /// end-to-end exercise of the silicon-grounded device model.
    #[test]
    fn from_prjoxide_device_is_placeable_and_routable() {
        let Some(db) = prjoxide_load::find_database() else {
            eprintln!("PRJOXIDE_DB not found — skipping");
            return;
        };
        let variant = NexusVariant::Lfcpnx100;
        // A window with interior PLC tiles plus neighbours for routing context.
        let device =
            NexusDevice::from_prjoxide_bbox(variant, &db, (4, 4, 12, 12)).expect("build device");

        // Locate a PLC (Logic) cell in the window.
        let mut plc = None;
        for y in 4..=12 {
            for x in 4..=12 {
                if let Some(t) = device.tile_at(x, y) {
                    if t.tile_type() == TileType::Logic {
                        plc = Some((x, y));
                    }
                }
            }
        }
        let (px, py) = plc.expect("a Logic tile in window");

        // The tile holds the full PLC complement: 8 LUT4 + 8 FF.
        let tile = device.tile_at(px, py).unwrap();
        assert_eq!(tile.available_bels(BelType::Lut4), 8, "8 LUT4 BELs in PLC");
        assert_eq!(tile.available_bels(BelType::Dff), 8, "8 FF BELs in PLC");

        // BEL pins bind to real wires via the conventional Device helpers.
        let lut0_out = device.lut_output_wire(px, py, 0).expect("LUT0 output wire");
        let lut0_in_a = device
            .lut_input_wire(px, py, 0, 0)
            .expect("LUT0 A input wire");
        let clk = device.clock_wire(px, py).expect("clock wire");
        assert!(device.wire(lut0_out).is_some());
        assert!(device.wire(lut0_in_a).is_some());
        assert!(device.wire(clk).is_some());

        // Routability: the LUT output drives PIPs into the fabric, and the LUT
        // input is driven by PIPs from the fabric. Without this, nothing routes.
        assert!(
            !device.wire_src_pips(lut0_out).is_empty(),
            "LUT output must drive fabric pips"
        );
        assert!(
            !device.wire_pips(lut0_in_a).is_empty(),
            "LUT input must be reachable via fabric pips"
        );

        // can_place still wires the standard cell types onto these BELs.
        assert!(device.can_place("OXIDE_COMB", BelType::Lut4));
        assert!(device.can_place("OXIDE_FF", BelType::Dff));

        eprintln!(
            "M1c-part2 OK: from_prjoxide device — PLC@({px},{py}) has 8 LUT4 + 8 FF, \
             LUT0.out drives {} pips, LUT0.A reached by {} pips; {} wires / {} pips total",
            device.wire_src_pips(lut0_out).len(),
            device.wire_pips(lut0_in_a).len(),
            device.wire_count(),
            device.pips.len(),
        );
    }
}
