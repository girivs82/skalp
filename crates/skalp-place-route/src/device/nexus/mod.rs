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
//! - LFCPNX-100 — 79,872 LUT4, 160x75 grid (LFCPNX-VERSA-EVN board)

pub mod data;
pub mod dsp;
pub mod ebr;
pub mod emit;
pub mod pll;
pub mod prjoxide_bels;
pub mod prjoxide_graph;
pub mod prjoxide_load;
pub mod prjoxide_pack;
pub mod sites;
mod tiles;
#[cfg(feature = "nexus-bitstream")]
pub mod timing;

pub use prjoxide_graph::{resolve_wire, NodeKey};
pub use prjoxide_load::{load_tilegrid, PrjoxideTile};
pub use sites::OSC_PIN;
pub use tiles::NexusTile;

use super::{
    Bel, BelId, BelPin, BelType, CellPin, ClockDomain, ClockResources, Device, DeviceFamily,
    DeviceStats, DspTile, IoSide, IoTile, LogicTile, MemoryBlock, PackagePins, PinDirection, Pip,
    PipId, RoutingArchitecture, SwitchPattern, Tile, TileType, Wire, WireDirection, WireId,
    WireSegment, WireType,
};

/// Local wires per tile in the synthetic routing graph: the input-mux and
/// switching wires of a logic tile, which every pin connection and every
/// route through the tile uses one of.
const LOCAL_WIRES: usize = 48;

/// Local wires each BEL input pin can select from, `INPUT_MUX_STRIDE` apart
/// (coprime with [`LOCAL_WIRES`]). With a few adjacent locals per pin, the
/// pins of a fully used tile shared too few locals to all be fed at once.
const INPUT_MUX_WIDTH: usize = 8;
const INPUT_MUX_STRIDE: usize = 11;
use crate::collections::{HashMap, HashMapExt};
use serde::{Deserialize, Serialize};
use std::sync::Arc;

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
    /// LFCPNX-100 (CertusPro-NX)
    /// 9,984 PLC tiles = 79,872 LUT4 + 79,872 FF
    /// 52 EBR (18Kb), 7 LRAM (32Kb), ~39 DSP blocks, 4 PLLs
    /// Hard PCIe (1x PCIE_LL), 8x PCS (SerDes)
    /// Packages: ASG256, CBG256, BBG484, BFG484, LFG672
    Lfcpnx100,
}

impl NexusVariant {
    /// Get the data record for this variant
    /// The package whose pin names designs use, on boards SKALP knows.
    pub fn default_package(&self) -> Option<&'static str> {
        match self {
            NexusVariant::Lfcpnx100 => Some("LFG672"),
            _ => None,
        }
    }

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
    tiles: Arc<Vec<Vec<Option<NexusTile>>>>,
    /// All wires
    wires: Arc<Vec<Wire>>,
    /// Wire name to ID mapping
    wire_names: Arc<HashMap<String, WireId>>,
    /// All PIPs
    pips: Arc<Vec<Pip>>,
    /// Wire to PIP mapping (PIPs that drive this wire)
    wire_to_pips: Arc<HashMap<WireId, Vec<PipId>>>,
    /// Wire to PIP mapping (PIPs driven by this wire)
    wire_src_pips: Arc<HashMap<WireId, Vec<PipId>>>,
    /// Replacement `wire_to_pips` lists for wires touched by [`Self::without_wires`]
    uphill_override: Arc<HashMap<WireId, Vec<PipId>>>,
    /// Replacement `wire_src_pips` lists for wires touched by [`Self::without_wires`]
    downhill_override: Arc<HashMap<WireId, Vec<PipId>>>,
    /// Tile to wire mapping
    tile_wires: Arc<HashMap<(u32, u32), Vec<WireId>>>,
    /// BEL pin to wire mapping
    bel_wires: Arc<HashMap<(u32, u32, String), WireId>>,
    /// Per-PIP configuration bits, indexed by `PipId` (real-silicon model only)
    pip_bits: Arc<Vec<Vec<prjoxide_graph::GlobalBit>>>,
    /// prjoxide tiles of the grid (real-silicon model only)
    prjoxide_tiles: Arc<Vec<PrjoxideTile>>,
    /// Least routing cost per tile distance along one axis
    /// ([`Device::route_cost_estimate`]); empty when not modelled
    hop_costs: Vec<f64>,
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
        Self::from_prjoxide_region(variant, db_root, bbox, None)
    }

    /// [`Self::from_prjoxide_bbox`], with logic placement limited to `logic_rows`.
    pub fn from_prjoxide_region(
        variant: NexusVariant,
        db_root: &std::path::Path,
        bbox: (u32, u32, u32, u32),
        logic_rows: Option<(u32, u32)>,
    ) -> Result<Self, String> {
        use prjoxide_graph::{node_name, resolve_wire};

        let (width, height) = variant.grid_size();
        let family = variant.prjoxide_family();

        // M1a: real tile grid. M1b: real routing graph for the window.
        let all_tiles = prjoxide_load::load_tilegrid(db_root, variant)?;
        let clocks =
            prjoxide_graph::ClockRegions::load(db_root, family, variant.prjoxide_device())?;
        let all_tiles_for_pins = all_tiles.clone();
        let graph = prjoxide_graph::build_routing_graph_with_clocks(
            db_root,
            family,
            &all_tiles,
            Some(bbox),
            Some(&clocks),
        )?;

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

        let dsp_sites: HashMap<(u32, u32), usize> = all_tiles
            .iter()
            .filter(|t| t.x >= x0 && t.x <= x1 && t.y >= y0 && t.y <= y1)
            .filter_map(|t| Some(((t.x, t.y), dsp::slot_of_tile(&t.tiletype)?)))
            .collect();
        let ram_sites: HashMap<(u32, u32), usize> = all_tiles
            .iter()
            .filter(|t| t.x >= x0 && t.x <= x1 && t.y >= y0 && t.y <= y1)
            .filter_map(|t| Some(((t.x, t.y), ebr::site_of_tile(&t.tiletype)?)))
            .collect();

        // PLL sites: the GPLL tile of each corner in the window
        let pll_sites: HashMap<(u32, u32), String> = all_tiles
            .iter()
            .filter(|t| t.x >= x0 && t.x <= x1 && t.y >= y0 && t.y <= y1)
            .filter_map(|t| Some(((t.x, t.y), pll::corner_of_tile(&t.tiletype)?.to_string())))
            .collect();

        // Tile order sets BEL ids and placement order, so keep it stable.
        let mut ordered: Vec<_> = primary.iter().collect();
        ordered.sort_unstable_by_key(|(&(x, y), _)| (y, x));
        for (&(x, y), t) in ordered {
            // A logic tile on a side of the window that is not the device's
            // edge gets its clock (and some routing) through tiles outside the
            // window, which the routing graph leaves out: placing a cell there
            // would cut it off from the dedicated clock network.
            let inside_columns = (x > x0 || x0 == 0) && (x < x1 || x1 + 1 >= width);
            let placeable =
                inside_columns && logic_rows.is_none_or(|(from, to)| y >= from && y <= to);
            let bels = if t.tiletype == "PLC" && !placeable {
                Vec::new()
            } else if t.tiletype == "PLC" {
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
                for (key, site_wire) in prjoxide_bels::dist_ram_wires()
                    .into_iter()
                    .chain(prjoxide_bels::ff_lut_input_wires())
                {
                    if let Some(w) = resolve_wire(x, y, &site_wire)
                        .and_then(|n| graph.wire_names.get(&node_name(&n)).copied())
                    {
                        bel_wires.insert((x, y, key), w);
                    }
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
            } else if t.tiletype.starts_with("SYSIO_B") {
                let mut bels = Vec::with_capacity(2);
                for (pio, side) in ["A", "B"].iter().enumerate() {
                    let mut wire_of = |key: String, site: String| {
                        let wire = resolve_wire(x, y, &site)
                            .and_then(|n| graph.wire_names.get(&node_name(&n)).copied());
                        if let Some(w) = wire {
                            bel_wires.insert((x, y, key), w);
                        }
                    };
                    // A high-performance bank's A pad is its differential site;
                    // CertusPro-NX suffixes these core names with A.
                    let candidates: &[&str] =
                        match (sites::is_high_performance_io(&t.tiletype), *side) {
                            (false, _) => &["SEIO33"],
                            (true, "A") => &["DIFFIO18A", "DIFFIO18"],
                            (true, _) => &["SEIO18A", "SEIO18"],
                        };
                    let core = candidates
                        .iter()
                        .find(|core| {
                            resolve_wire(x, y, &format!("JPADDO_{core}_CORE_IO{side}"))
                                .is_some_and(|n| graph.wire_names.contains_key(&node_name(&n)))
                        })
                        .copied()
                        .unwrap_or(candidates[0]);
                    wire_of(
                        format!("PIO{pio}_O"),
                        format!("JPADDO_{core}_CORE_IO{side}"),
                    );
                    wire_of(
                        format!("PIO{pio}_I"),
                        format!("JPADDI_{core}_CORE_IO{side}"),
                    );
                    bels.push(Bel {
                        id: BelId(bel_id_counter),
                        bel_type: BelType::IoCell,
                        name: format!("PIO{side}"),
                        pins: Vec::new(),
                    });
                    bel_id_counter += 1;
                }
                bels
            } else if t.tiletype == "EFB_1_OSC" {
                // The on-chip oscillator is modelled as an input pad: its
                // output drives the design like a clock pin.
                if let Some(w) = resolve_wire(x, y, "S1:JHFCLKOUT_OSC_CORE")
                    .and_then(|n| graph.wire_names.get(&node_name(&n)).copied())
                {
                    bel_wires.insert((x, y, "PIO0_I".to_string()), w);
                }
                let bel = Bel {
                    id: BelId(bel_id_counter),
                    bel_type: BelType::IoCell,
                    name: "OSC".to_string(),
                    pins: Vec::new(),
                };
                bel_id_counter += 1;
                vec![bel]
            } else {
                // Other non-logic primary tiles: correct type, BELs deferred.
                Vec::new()
            };
            let mut bels = bels;
            if let Some(&slot) = dsp_sites.get(&(x, y)).filter(|_| placeable) {
                for pin in (0..18)
                    .flat_map(|bit| [format!("A{bit}"), format!("B{bit}")])
                    .chain(["ASIGNED".to_string(), "BSIGNED".to_string()])
                    .chain((0..dsp::SHIFT_CONTROL_PINS).map(|bit| format!("SFTCTRL{bit}")))
                    .chain(dsp::low_driven_pins())
                {
                    let wire = dsp::input_wire(slot, &pin)
                        .and_then(|site| resolve_wire(x, y, &site))
                        .and_then(|n| graph.wire_names.get(&node_name(&n)).copied());
                    if let Some(w) = wire {
                        bel_wires.insert((x, y, format!("DSP_{pin}")), w);
                    }
                }
                for bit in 0..dsp::PRODUCT_BITS {
                    let wire = resolve_wire(
                        dsp::register_column(slot, x),
                        y,
                        &dsp::product_wire(slot, bit),
                    )
                    .and_then(|n| graph.wire_names.get(&node_name(&n)).copied());
                    if let Some(w) = wire {
                        bel_wires.insert((x, y, format!("DSP_Z{bit}")), w);
                    }
                }
                bels.push(Bel {
                    id: BelId(bel_id_counter),
                    bel_type: BelType::DspSlice,
                    name: format!("MULT18_{slot}"),
                    pins: Vec::new(),
                });
                bel_id_counter += 1;
            }
            // A block RAM site shares its grid cell with a logic tile.
            if let Some(&site) = ram_sites.get(&(x, y)).filter(|_| placeable) {
                let mut wire_of = |pin: &str| {
                    let wire = resolve_wire(x, y, &ebr::site_wire(pin))
                        .and_then(|n| graph.wire_names.get(&node_name(&n)).copied());
                    if let Some(w) = wire {
                        bel_wires.insert((x, y, format!("RAM_{pin}")), w);
                    }
                };
                for pin in ebr::input_pins() {
                    wire_of(pin);
                }
                wire_of("CLKA");
                wire_of("CLKB");
                for bit in 0..36 {
                    wire_of(&ebr::output_pin(bit));
                }
                bels.push(Bel {
                    id: BelId(bel_id_counter),
                    bel_type: BelType::RamSlice,
                    name: format!("EBR{site}"),
                    pins: Vec::new(),
                });
                bel_id_counter += 1;
            }
            if let Some(corner) = pll_sites.get(&(x, y)) {
                let wire_of = |step: &str, pin: &str| {
                    resolve_wire(x, y, &pll::site_wire(step, pin))
                        .and_then(|n| graph.wire_names.get(&node_name(&n)).copied())
                };
                // The pins sit one tile into the fabric from the PLL's tile,
                // beside it or diagonally (CertusPro-NX's lower left PLL sits
                // one row below its tile)
                let step = ["E1", "W1", "S1", "N1", "S1E1", "N1E1", "S1W1", "N1W1"]
                    .into_iter()
                    .find(|step| wire_of(step, "REFCK").is_some());
                if let Some(step) = step {
                    for pin in pll::input_pins().iter().chain(pll::output_pins()) {
                        if let Some(w) = wire_of(step, pin) {
                            bel_wires.insert((x, y, format!("PLL_{pin}")), w);
                        }
                    }
                    bels.push(Bel {
                        id: BelId(bel_id_counter),
                        bel_type: BelType::Pll,
                        name: format!("PLL_{corner}"),
                        pins: Vec::new(),
                    });
                    bel_id_counter += 1;
                }
            }
            tiles[y as usize][x as usize] = Some(NexusTile::new(t.skalp_type, x, y, bels));
        }

        Ok(Self {
            variant,
            grid_size: (width, height),
            tiles: Arc::new(tiles),
            wires: Arc::new(graph.wires),
            wire_names: Arc::new(graph.wire_names),
            pips: Arc::new(graph.pips),
            wire_to_pips: Arc::new(graph.wire_to_pips),
            wire_src_pips: Arc::new(graph.wire_src_pips),
            uphill_override: Arc::new(HashMap::new()),
            downhill_override: Arc::new(HashMap::new()),
            tile_wires: Arc::new(graph.tile_wires),
            bel_wires: Arc::new(bel_wires),
            pip_bits: Arc::new(graph.pip_bits),
            prjoxide_tiles: Arc::new(all_tiles),
            hop_costs: Vec::new(),
            packages: sites::package_pins(db_root, variant, &all_tiles_for_pins)?,
            routing: Self::default_routing(variant),
            clock_resources: Self::default_clock_resources(variant),
            logic_tiles,
            io_tiles: Vec::new(),
            memory_blocks: Vec::new(),
            dsp_tiles: Vec::new(),
        })
    }

    /// Remove wires from routing, leaving them to whatever claimed them.
    /// Give every PIP the delay `db` has for its class, so the router's
    /// costs and timing terms see the delays timing analysis will report
    /// (the routing graph is built with one nominal delay for every PIP).
    #[cfg(feature = "nexus-bitstream")]
    pub fn set_pip_delays(&mut self, db: &timing::TimingDb) {
        let wires = &self.wires;
        let name = |w: WireId| wires.get(w.0 as usize).map(|wire| wire.name.as_str());
        let delays: Vec<Option<u32>> = self
            .pips
            .iter()
            .map(|pip| {
                let (src, dst) = (name(pip.src_wire)?, name(pip.dst_wire)?);
                Some(db.pip_delay_ps(src, dst).max(0) as u32)
            })
            .collect();
        let pips = Arc::make_mut(&mut self.pips);
        for (pip, delay) in pips.iter_mut().zip(delays) {
            if let Some(delay) = delay {
                pip.delay = delay;
            }
        }
    }

    pub fn without_wires(&self, claimed: &crate::collections::HashSet<WireId>) -> Self {
        // The routing graph is shared between copies. Only the adjacency
        // lists that touch a claimed wire are rebuilt, into small override
        // maps, instead of copying the whole (multi-gigabyte) graph.
        let usable = |pip: &PipId| {
            self.pips
                .get(pip.0 as usize)
                .is_some_and(|p| !claimed.contains(&p.src_wire) && !claimed.contains(&p.dst_wire))
        };
        // Each wire next to a claimed one is rebuilt once, however many
        // claimed wires it touches: a clock spine wire next to thousands of
        // claimed branch wires made rebuilding it per claimed wire take over
        // a minute on a design with 12,800 flip-flops.
        let mut uphill_touched: crate::collections::HashSet<WireId> = claimed.clone();
        let mut downhill_touched: crate::collections::HashSet<WireId> = claimed.clone();
        for &wire in claimed {
            for pip in self.pips_driven_by(wire) {
                if let Some(p) = self.pips.get(pip.0 as usize) {
                    uphill_touched.insert(p.dst_wire);
                }
            }
            for pip in self.pips_driving(wire) {
                if let Some(p) = self.pips.get(pip.0 as usize) {
                    downhill_touched.insert(p.src_wire);
                }
            }
        }
        let mut uphill = (*self.uphill_override).clone();
        let mut downhill = (*self.downhill_override).clone();
        for w in uphill_touched {
            let kept = self
                .pips_driving(w)
                .iter()
                .copied()
                .filter(usable)
                .collect();
            uphill.insert(w, kept);
        }
        for w in downhill_touched {
            let kept = self
                .pips_driven_by(w)
                .iter()
                .copied()
                .filter(usable)
                .collect();
            downhill.insert(w, kept);
        }
        let mut device = self.clone();
        device.uphill_override = Arc::new(uphill);
        device.downhill_override = Arc::new(downhill);
        device
    }

    /// PIPs that drive `wire`, after any wires removed by [`Self::without_wires`].
    fn pips_driving(&self, wire: WireId) -> &[PipId] {
        self.uphill_override
            .get(&wire)
            .or_else(|| self.wire_to_pips.get(&wire))
            .map(Vec::as_slice)
            .unwrap_or(&[])
    }

    /// PIPs that `wire` drives, after any wires removed by [`Self::without_wires`].
    fn pips_driven_by(&self, wire: WireId) -> &[PipId] {
        self.downhill_override
            .get(&wire)
            .or_else(|| self.wire_src_pips.get(&wire))
            .map(Vec::as_slice)
            .unwrap_or(&[])
    }

    /// A BEL pin's wire, by its conventional key.
    pub fn bel_wire(&self, x: u32, y: u32, key: &str) -> Option<WireId> {
        self.bel_wires.get(&(x, y, key.to_string())).copied()
    }

    /// Configuration bits of a PIP (real-silicon model only).
    pub fn pip_config_bits(&self, pip: PipId) -> &[prjoxide_graph::GlobalBit] {
        self.pip_bits
            .get(pip.0 as usize)
            .map(Vec::as_slice)
            .unwrap_or(&[])
    }

    /// The first prjoxide tile of a type (real-silicon model only).
    pub fn prjoxide_tile_of_type(&self, tiletype: &str) -> Option<&PrjoxideTile> {
        self.prjoxide_tiles.iter().find(|t| t.tiletype == tiletype)
    }

    /// Every prjoxide tile of the device (real-silicon model only).
    /// A site wire recorded for tile (x, y) under `key` (such as `PLL_REFCK`).
    pub fn bel_pin_wire_by_name(&self, x: u32, y: u32, key: &str) -> Option<WireId> {
        self.bel_wires.get(&(x, y, key.to_string())).copied()
    }

    pub fn prjoxide_tiles(&self) -> &[PrjoxideTile] {
        &self.prjoxide_tiles
    }

    /// The prjoxide tile of a type at a grid location (real-silicon model only).
    pub fn prjoxide_tile(
        &self,
        x: u32,
        y: u32,
        tiletype: impl Fn(&str) -> bool,
    ) -> Option<&PrjoxideTile> {
        self.prjoxide_tiles
            .iter()
            .find(|t| t.x == x && t.y == y && tiletype(&t.tiletype))
    }

    /// Create a synthetic Nexus device from datasheet parameters
    fn new_synthetic(variant: NexusVariant) -> Self {
        let grid_size = variant.grid_size();
        let (width, height) = grid_size;

        let mut device = Self {
            variant,
            grid_size,
            tiles: Arc::new(vec![vec![None; width as usize]; height as usize]),
            wires: Arc::default(),
            wire_names: Arc::new(HashMap::new()),
            pips: Arc::default(),
            wire_to_pips: Arc::new(HashMap::new()),
            wire_src_pips: Arc::new(HashMap::new()),
            uphill_override: Arc::new(HashMap::new()),
            downhill_override: Arc::new(HashMap::new()),
            tile_wires: Arc::new(HashMap::new()),
            bel_wires: Arc::new(HashMap::new()),
            pip_bits: Arc::default(),
            prjoxide_tiles: Arc::default(),
            hop_costs: Vec::new(),
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
                        Arc::make_mut(&mut self.tiles)[y as usize][x as usize] =
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
                    Arc::make_mut(&mut self.tiles)[y as usize][x as usize] =
                        Some(NexusTile::new(side, x, y, io_bels));
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
                    Arc::make_mut(&mut self.tiles)[y as usize][x as usize] =
                        Some(NexusTile::new(TileType::RamTop, x, y, bram_bels));
                } else if is_dsp_col {
                    // DSP block (18x18 MAC)
                    let dsp_bels = Self::make_dsp_bels(&mut bel_id_counter, x, y);
                    self.dsp_tiles.push(DspTile {
                        x,
                        y,
                        mac_count: data::DSP_MACS_PER_TILE,
                    });
                    Arc::make_mut(&mut self.tiles)[y as usize][x as usize] =
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
                    Arc::make_mut(&mut self.tiles)[y as usize][x as usize] =
                        Some(NexusTile::new(TileType::Logic, x, y, logic_bels));
                }
            }
        }
    }

    /// Create BELs for a logic tile (8 LUT4 + 8 DFF + 4 CCU2 carry cells + 4
    /// wide LUTs + 1 distributed RAM)
    fn make_logic_bels(bel_id: &mut u32, _x: u32, _y: u32) -> Vec<Bel> {
        let mut bels = Vec::with_capacity(20);

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

        // 4 carry cells, one per slice; carry k uses LUTs 2k and 2k+1
        for slice in ['A', 'B', 'C', 'D'] {
            let id = BelId(*bel_id);
            *bel_id += 1;
            bels.push(Bel {
                id,
                bel_type: BelType::Carry,
                name: format!("CCU2_{}", slice),
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
        }

        // 4 wide LUTs, one per slice: the slice's two LUTs on the same four
        // inputs and the mux between them, selected by a fifth input
        for (n, slice) in ['A', 'B', 'C', 'D'].into_iter().enumerate() {
            let id = BelId(*bel_id);
            *bel_id += 1;
            bels.push(Bel {
                id,
                bel_type: BelType::WideLut,
                name: format!("WIDEFN_{}", slice),
                pins: vec![
                    BelPin {
                        name: format!("WIDE{n}_SEL"),
                        direction: PinDirection::Input,
                        wire: None,
                    },
                    BelPin {
                        name: format!("WIDE{n}_Z"),
                        direction: PinDirection::Output,
                        wire: None,
                    },
                ],
            });
        }

        // One distributed RAM site: a 16 x 4 RAM (DPR16X4) built from the
        // LUTs of slices A and B plus slice C as its write port (RAMW), so it
        // takes LUTs 0..5 (see bel_footprint)
        let id = BelId(*bel_id);
        *bel_id += 1;
        bels.push(Bel {
            id,
            bel_type: BelType::DistRam,
            name: "DPRAM".to_string(),
            pins: Vec::new(),
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

                // Local routing wires (48 per tile: the input-mux and switching wires of a PLC)
                for i in 0..LOCAL_WIRES as u8 {
                    let w = Wire {
                        id: WireId(wire_id),
                        name: format!("R{}C{}_LOCAL{}", y, x, i),
                        wire_type: WireType::Local(i),
                        tile_x: x,
                        tile_y: y,
                        delay: 30, // 30ps — Nexus 28nm is faster than iCE40 40nm
                    };
                    Arc::make_mut(&mut self.wire_names).insert(w.name.clone(), w.id);
                    Arc::make_mut(&mut self.tile_wires)
                        .entry((x, y))
                        .or_default()
                        .push(w.id);
                    Arc::make_mut(&mut self.wires).push(w);
                    wire_id += 1;
                }

                // Switch box: each local wire drives two others (+1 and +7
                // around the tile), so a signal landing on any local wire
                // can reach every other local wire of the tile, and through
                // them every outgoing span and every BEL input. Without it a
                // route could only continue from the few locals that happen
                // to drive an outgoing span, and most of the fabric was
                // unreachable.
                let locals: Vec<WireId> = self.tile_wires[&(x, y)][..LOCAL_WIRES].to_vec();
                for (k, &src_wire) in locals.iter().enumerate() {
                    for step in [1, 7] {
                        let p = Pip {
                            id: PipId(pip_id),
                            src_wire,
                            dst_wire: locals[(k + step) % LOCAL_WIRES],
                            delay: 20,
                            configurable: true,
                            tile_x: x,
                            tile_y: y,
                        };
                        Arc::make_mut(&mut self.wire_to_pips)
                            .entry(p.dst_wire)
                            .or_default()
                            .push(p.id);
                        Arc::make_mut(&mut self.wire_src_pips)
                            .entry(p.src_wire)
                            .or_default()
                            .push(p.id);
                        Arc::make_mut(&mut self.pips).push(p);
                        pip_id += 1;
                    }
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
                        Arc::make_mut(&mut self.wire_names)
                            .insert(out_wire.name.clone(), out_wire.id);
                        Arc::make_mut(&mut self.bel_wires)
                            .insert((x, y, format!("LUT{}_Z", lc)), out_wire.id);
                        Arc::make_mut(&mut self.tile_wires)
                            .entry((x, y))
                            .or_default()
                            .push(out_wire.id);
                        let lut_out_id = out_wire.id;
                        Arc::make_mut(&mut self.wires).push(out_wire);
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
                            Arc::make_mut(&mut self.wire_names)
                                .insert(in_wire.name.clone(), in_wire.id);
                            Arc::make_mut(&mut self.bel_wires)
                                .insert((x, y, format!("LUT{}_{}", lc, pin_name)), in_wire.id);
                            Arc::make_mut(&mut self.tile_wires)
                                .entry((x, y))
                                .or_default()
                                .push(in_wire.id);
                            Arc::make_mut(&mut self.wires).push(in_wire);
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
                        Arc::make_mut(&mut self.wire_names)
                            .insert(ff_wire.name.clone(), ff_wire.id);
                        Arc::make_mut(&mut self.bel_wires)
                            .insert((x, y, format!("FF{}_Q", lc)), ff_wire.id);
                        Arc::make_mut(&mut self.tile_wires)
                            .entry((x, y))
                            .or_default()
                            .push(ff_wire.id);
                        Arc::make_mut(&mut self.wires).push(ff_wire);
                        wire_id += 1;

                        // FF data input; the FF output and input reach the
                        // tile's local wires like the LUT's (one local out,
                        // a few locals in)
                        let ff_d_wire = Wire {
                            id: WireId(wire_id),
                            name: format!("R{}C{}_FF{}_D", y, x, lc),
                            wire_type: WireType::BelPin,
                            tile_x: x,
                            tile_y: y,
                            delay: 0,
                        };
                        Arc::make_mut(&mut self.wire_names)
                            .insert(ff_d_wire.name.clone(), ff_d_wire.id);
                        Arc::make_mut(&mut self.bel_wires)
                            .insert((x, y, format!("FF{}_D", lc)), ff_d_wire.id);
                        Arc::make_mut(&mut self.tile_wires)
                            .entry((x, y))
                            .or_default()
                            .push(ff_d_wire.id);
                        let (ff_q_id, ff_d_id) = (WireId(wire_id - 1), ff_d_wire.id);
                        Arc::make_mut(&mut self.wires).push(ff_d_wire);
                        wire_id += 1;
                        let locals = self.tile_wires.get(&(x, y)).cloned().unwrap_or_default();
                        let mut ff_pips = Vec::new();
                        if let Some(&lw) = locals.get((lc * 6 + 1) % LOCAL_WIRES) {
                            ff_pips.push((ff_q_id, lw, 20));
                        }
                        for j in 0..INPUT_MUX_WIDTH {
                            if let Some(&lw) =
                                locals.get((lc * 6 + 5 + j * INPUT_MUX_STRIDE) % LOCAL_WIRES)
                            {
                                ff_pips.push((lw, ff_d_id, 15));
                            }
                        }
                        for (src_wire, dst_wire, delay) in ff_pips {
                            let p = Pip {
                                id: PipId(pip_id),
                                src_wire,
                                dst_wire,
                                delay,
                                configurable: true,
                                tile_x: x,
                                tile_y: y,
                            };
                            Arc::make_mut(&mut self.wire_to_pips)
                                .entry(p.dst_wire)
                                .or_default()
                                .push(p.id);
                            Arc::make_mut(&mut self.wire_src_pips)
                                .entry(p.src_wire)
                                .or_default()
                                .push(p.id);
                            Arc::make_mut(&mut self.pips).push(p);
                            pip_id += 1;
                        }

                        // Connect LUT output → local wire (BEL pin PIP)
                        let local_idx = (lc * 6) % LOCAL_WIRES;
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
                                Arc::make_mut(&mut self.wire_to_pips)
                                    .entry(p.dst_wire)
                                    .or_default()
                                    .push(p.id);
                                Arc::make_mut(&mut self.wire_src_pips)
                                    .entry(p.src_wire)
                                    .or_default()
                                    .push(p.id);
                                Arc::make_mut(&mut self.pips).push(p);
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
                                // Input mux: INPUT_MUX_WIDTH local wires spread
                                // over the tile, so a full tile can still feed
                                // every pin from a different local
                                for j in 0..INPUT_MUX_WIDTH {
                                    let local_idx =
                                        (lc * 6 + inp + 2 + j * INPUT_MUX_STRIDE) % LOCAL_WIRES;
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
                                            Arc::make_mut(&mut self.wire_to_pips)
                                                .entry(p.dst_wire)
                                                .or_default()
                                                .push(p.id);
                                            Arc::make_mut(&mut self.wire_src_pips)
                                                .entry(p.src_wire)
                                                .or_default()
                                                .push(p.id);
                                            Arc::make_mut(&mut self.pips).push(p);
                                            pip_id += 1;
                                        }
                                    }
                                }
                            }
                        }
                    }

                    // Wide LUT select and output, one pair per slice. They use
                    // the locals of the slice's first flip-flop, which the
                    // wide LUT takes (see bel_footprint).
                    for slice in 0..4usize {
                        let lc = 2 * slice;
                        let locals = self.tile_wires.get(&(x, y)).cloned().unwrap_or_default();
                        let mut pin_wire = |name: String| {
                            let w = Wire {
                                id: WireId(wire_id),
                                name: format!("R{}C{}_{}", y, x, name),
                                wire_type: WireType::BelPin,
                                tile_x: x,
                                tile_y: y,
                                delay: 0,
                            };
                            Arc::make_mut(&mut self.wire_names).insert(w.name.clone(), w.id);
                            Arc::make_mut(&mut self.bel_wires).insert((x, y, name), w.id);
                            Arc::make_mut(&mut self.tile_wires)
                                .entry((x, y))
                                .or_default()
                                .push(w.id);
                            let id = w.id;
                            Arc::make_mut(&mut self.wires).push(w);
                            wire_id += 1;
                            id
                        };
                        let sel = pin_wire(format!("WIDE{slice}_SEL"));
                        let out = pin_wire(format!("WIDE{slice}_Z"));
                        let mut wide_pips = Vec::new();
                        if let Some(&lw) = locals.get((lc * 6 + 1) % LOCAL_WIRES) {
                            wide_pips.push((out, lw, 20));
                        }
                        for j in 0..INPUT_MUX_WIDTH {
                            if let Some(&lw) =
                                locals.get((lc * 6 + 5 + j * INPUT_MUX_STRIDE) % LOCAL_WIRES)
                            {
                                wide_pips.push((lw, sel, 15));
                            }
                        }
                        for (src_wire, dst_wire, delay) in wide_pips {
                            let p = Pip {
                                id: PipId(pip_id),
                                src_wire,
                                dst_wire,
                                delay,
                                configurable: true,
                                tile_x: x,
                                tile_y: y,
                            };
                            Arc::make_mut(&mut self.wire_to_pips)
                                .entry(p.dst_wire)
                                .or_default()
                                .push(p.id);
                            Arc::make_mut(&mut self.wire_src_pips)
                                .entry(p.src_wire)
                                .or_default()
                                .push(p.id);
                            Arc::make_mut(&mut self.pips).push(p);
                            pip_id += 1;
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
                    Arc::make_mut(&mut self.wire_names).insert(clk_wire.name.clone(), clk_wire.id);
                    Arc::make_mut(&mut self.bel_wires)
                        .insert((x, y, "CLK".to_string()), clk_wire.id);
                    Arc::make_mut(&mut self.tile_wires)
                        .entry((x, y))
                        .or_default()
                        .push(clk_wire.id);
                    Arc::make_mut(&mut self.wires).push(clk_wire);
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
                        Arc::make_mut(&mut self.wire_names).insert(i_wire.name.clone(), i_wire.id);
                        Arc::make_mut(&mut self.bel_wires)
                            .insert((x, y, format!("PIO{}_I", iob)), i_wire.id);
                        Arc::make_mut(&mut self.tile_wires)
                            .entry((x, y))
                            .or_default()
                            .push(i_wire.id);
                        Arc::make_mut(&mut self.wires).push(i_wire);
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
                        Arc::make_mut(&mut self.wire_names).insert(o_wire.name.clone(), o_wire.id);
                        Arc::make_mut(&mut self.bel_wires)
                            .insert((x, y, format!("PIO{}_O", iob)), o_wire.id);
                        Arc::make_mut(&mut self.tile_wires)
                            .entry((x, y))
                            .or_default()
                            .push(o_wire.id);
                        Arc::make_mut(&mut self.wires).push(o_wire);
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
                    Arc::make_mut(&mut self.wire_names).insert(w.name.clone(), w.id);
                    Arc::make_mut(&mut self.tile_wires)
                        .entry((x, y))
                        .or_default()
                        .push(w.id);
                    let h2_id = w.id;
                    Arc::make_mut(&mut self.wires).push(w);
                    wire_id += 1;

                    // PIP: local → H2 (drive span wire)
                    let local_idx = (i as usize * 6) % LOCAL_WIRES;
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
                            Arc::make_mut(&mut self.wire_to_pips)
                                .entry(p.dst_wire)
                                .or_default()
                                .push(p.id);
                            Arc::make_mut(&mut self.wire_src_pips)
                                .entry(p.src_wire)
                                .or_default()
                                .push(p.id);
                            Arc::make_mut(&mut self.pips).push(p);
                            pip_id += 1;
                        }
                    }

                    // PIP: H2 → local in destination tile (x+2)
                    let dest_x = x + 2;
                    if dest_x < width - 1 {
                        let dest_local_idx = (i as usize * 6 + 1) % LOCAL_WIRES;
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
                                Arc::make_mut(&mut self.wire_to_pips)
                                    .entry(p.dst_wire)
                                    .or_default()
                                    .push(p.id);
                                Arc::make_mut(&mut self.wire_src_pips)
                                    .entry(p.src_wire)
                                    .or_default()
                                    .push(p.id);
                                Arc::make_mut(&mut self.pips).push(p);
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
                    Arc::make_mut(&mut self.wire_names).insert(w.name.clone(), w.id);
                    Arc::make_mut(&mut self.tile_wires)
                        .entry((x, y))
                        .or_default()
                        .push(w.id);
                    let v2_id = w.id;
                    Arc::make_mut(&mut self.wires).push(w);
                    wire_id += 1;

                    // PIP: local → V2
                    let local_idx = (i as usize * 6 + 2) % LOCAL_WIRES;
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
                            Arc::make_mut(&mut self.wire_to_pips)
                                .entry(p.dst_wire)
                                .or_default()
                                .push(p.id);
                            Arc::make_mut(&mut self.wire_src_pips)
                                .entry(p.src_wire)
                                .or_default()
                                .push(p.id);
                            Arc::make_mut(&mut self.pips).push(p);
                            pip_id += 1;
                        }
                    }

                    // PIP: V2 → local in destination tile (y+2)
                    let dest_y = y + 2;
                    if dest_y < height - 1 {
                        let dest_local_idx = (i as usize * 6 + 1) % LOCAL_WIRES;
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
                                Arc::make_mut(&mut self.wire_to_pips)
                                    .entry(p.dst_wire)
                                    .or_default()
                                    .push(p.id);
                                Arc::make_mut(&mut self.wire_src_pips)
                                    .entry(p.src_wire)
                                    .or_default()
                                    .push(p.id);
                                Arc::make_mut(&mut self.pips).push(p);
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
                    Arc::make_mut(&mut self.wire_names).insert(w.name.clone(), w.id);
                    Arc::make_mut(&mut self.tile_wires)
                        .entry((x, y))
                        .or_default()
                        .push(w.id);
                    let h12_id = w.id;
                    Arc::make_mut(&mut self.wires).push(w);
                    wire_id += 1;

                    // PIP: local → H12
                    let local_idx = (i as usize * 12) % LOCAL_WIRES;
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
                            Arc::make_mut(&mut self.wire_to_pips)
                                .entry(p.dst_wire)
                                .or_default()
                                .push(p.id);
                            Arc::make_mut(&mut self.wire_src_pips)
                                .entry(p.src_wire)
                                .or_default()
                                .push(p.id);
                            Arc::make_mut(&mut self.pips).push(p);
                            pip_id += 1;
                        }
                    }

                    // PIP: H12 → local at x+12
                    let dest_x = x + 12;
                    if dest_x < width - 1 {
                        let dest_local_idx = (i as usize * 12 + 3) % LOCAL_WIRES;
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
                                Arc::make_mut(&mut self.wire_to_pips)
                                    .entry(p.dst_wire)
                                    .or_default()
                                    .push(p.id);
                                Arc::make_mut(&mut self.wire_src_pips)
                                    .entry(p.src_wire)
                                    .or_default()
                                    .push(p.id);
                                Arc::make_mut(&mut self.pips).push(p);
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
                    Arc::make_mut(&mut self.wire_names).insert(w.name.clone(), w.id);
                    Arc::make_mut(&mut self.tile_wires)
                        .entry((x, y))
                        .or_default()
                        .push(w.id);
                    let v12_id = w.id;
                    Arc::make_mut(&mut self.wires).push(w);
                    wire_id += 1;

                    // PIP: local → V12
                    let local_idx = (i as usize * 12 + 1) % LOCAL_WIRES;
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
                            Arc::make_mut(&mut self.wire_to_pips)
                                .entry(p.dst_wire)
                                .or_default()
                                .push(p.id);
                            Arc::make_mut(&mut self.wire_src_pips)
                                .entry(p.src_wire)
                                .or_default()
                                .push(p.id);
                            Arc::make_mut(&mut self.pips).push(p);
                            pip_id += 1;
                        }
                    }

                    // PIP: V12 → local at y+12
                    let dest_y = y + 12;
                    if dest_y < height - 1 {
                        let dest_local_idx = (i as usize * 12 + 4) % LOCAL_WIRES;
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
                                Arc::make_mut(&mut self.wire_to_pips)
                                    .entry(p.dst_wire)
                                    .or_default()
                                    .push(p.id);
                                Arc::make_mut(&mut self.wire_src_pips)
                                    .entry(p.src_wire)
                                    .or_default()
                                    .push(p.id);
                                Arc::make_mut(&mut self.pips).push(p);
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
                    Arc::make_mut(&mut self.wire_names).insert(w.name.clone(), w.id);
                    Arc::make_mut(&mut self.tile_wires)
                        .entry((x, y))
                        .or_default()
                        .push(w.id);
                    let neigh_id = w.id;
                    Arc::make_mut(&mut self.wires).push(w);
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
                            Arc::make_mut(&mut self.wire_to_pips)
                                .entry(p.dst_wire)
                                .or_default()
                                .push(p.id);
                            Arc::make_mut(&mut self.wire_src_pips)
                                .entry(p.src_wire)
                                .or_default()
                                .push(p.id);
                            Arc::make_mut(&mut self.pips).push(p);
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
                            Arc::make_mut(&mut self.wire_to_pips)
                                .entry(p.dst_wire)
                                .or_default()
                                .push(p.id);
                            Arc::make_mut(&mut self.wire_src_pips)
                                .entry(p.src_wire)
                                .or_default()
                                .push(p.id);
                            Arc::make_mut(&mut self.pips).push(p);
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
            Arc::make_mut(&mut self.wire_names).insert(w.name.clone(), w.id);
            Arc::make_mut(&mut self.wires).push(w);
            wire_id += 1;
        }

        // Switch boxes (see synthetic_switch_box_pips)
        debug_assert!(self
            .wires
            .iter()
            .enumerate()
            .all(|(i, w)| w.id.0 as usize == i));
        debug_assert!(self
            .pips
            .iter()
            .enumerate()
            .all(|(i, p)| p.id.0 as usize == i));
        for (src_wire, dst_wire, delay, tile_x, tile_y) in
            crate::device::synthetic_switch_box_pips(&self.wires, &self.pips, LOCAL_WIRES, 6)
        {
            let p = Pip {
                id: PipId(self.pips.len() as u32),
                src_wire,
                dst_wire,
                delay,
                configurable: true,
                tile_x,
                tile_y,
            };
            Arc::make_mut(&mut self.wire_to_pips)
                .entry(p.dst_wire)
                .or_default()
                .push(p.id);
            Arc::make_mut(&mut self.wire_src_pips)
                .entry(p.src_wire)
                .or_default()
                .push(p.id);
            Arc::make_mut(&mut self.pips).push(p);
        }
        // Inter-tile wires (neighbour, spans) and the cost of the PIP into
        // each, as built above
        let (width, height) = self.grid_size();
        self.hop_costs = crate::device::span_hop_costs(
            width.max(height) as usize,
            &[(1, 1.0), (2, 1.0), (12, 1.0)],
        );
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

/// Whether a wire name is general fabric routing: `H0x` or `V0x` after its
/// `R<row>C<col>_` location prefix.
fn is_general_wire_name(name: &str) -> bool {
    let base = name
        .strip_prefix('R')
        .and_then(|rest| rest.split_once('_'))
        .map_or(name, |(_, rest)| rest);
    let mut chars = base.chars();
    matches!(chars.next(), Some('H') | Some('V')) && chars.next() == Some('0')
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
        // The real-device model only places what its tiles' BELs offer.
        let real_sites = |kind: BelType| {
            self.tiles
                .iter()
                .flatten()
                .flatten()
                .flat_map(|tile| tile.bels())
                .filter(|bel| bel.bel_type == kind)
                .count()
        };
        let (total_brams, total_dsps) = if self.prjoxide_tiles.is_empty() {
            (
                self.memory_blocks.len(),
                self.dsp_tiles.len() * data::DSP_MACS_PER_TILE as usize,
            )
        } else {
            (real_sites(BelType::RamSlice), real_sites(BelType::DspSlice))
        };
        DeviceStats {
            total_luts: self.logic_tiles.len() * data::LOGIC_LUTS_PER_TILE as usize,
            total_ffs: self.logic_tiles.len() * data::LOGIC_FFS_PER_TILE as usize,
            total_ios: self.io_tiles.len() * data::IO_CELLS_PER_TILE as usize,
            total_brams,
            total_dsps,
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
        self.pips_driving(wire_id).to_vec()
    }

    fn wire_src_pips(&self, wire_id: WireId) -> &[PipId] {
        self.pips_driven_by(wire_id)
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
            "OXIDE_LUT5" => bel_type == BelType::WideLut,
            "DFF" | "OXIDE_FF" | "FD1P3IX" | "FD1P3DX" | "FD1S3BX" => bel_type.is_ff(),
            "CCU2" | "CCU2C" | "CARRY" => bel_type == BelType::Carry,
            "DPR16X4" | "DPR16X4C" => bel_type == BelType::DistRam,
            "IB" | "OB" | "BB" | "SEIO18" | "SEIO33" | "DIFFIO18" => bel_type == BelType::IoCell,
            "DP16K" | "DP16KD" | "PDPSC16K" | "SP16KD" | "EBR" => bel_type == BelType::RamSlice,
            "MULT18X18" | "MULT18X18D" | "MULT9X9D" | "DSP" => bel_type == BelType::DspSlice,
            "EHXPLLL" | "PLL" => bel_type == BelType::Pll,
            _ => false,
        }
    }

    fn carry_chain_layout(&self) -> Option<crate::device::CarryChainLayout> {
        // Carry chains run left to right: slice A..D of a tile, then slice A
        // of the tile to the right.
        let (width, height) = self.grid_size();
        (0..height)
            .flat_map(|y| (0..width).map(move |x| (x, y)))
            .filter_map(|(x, y)| self.tile_at(x, y))
            .find(|t| t.tile_type() == TileType::Logic && !t.bels().is_empty())
            .and_then(|t| crate::device::CarryChainLayout::paired_luts(t.bels(), (1, 0)))
    }

    fn ff_control_group(&self, x: u32, y: u32, bel_index: usize) -> Option<usize> {
        // Slices A and B take their CLK, CE and LSR from one set of tile
        // wires (JCLK0, JCE0, JLSR0), and slices C and D from the other
        super::ff_slice_group(self, x, y, bel_index, 4)
    }

    fn bel_footprint(&self, x: u32, y: u32, bel_index: usize) -> Vec<usize> {
        // What each BEL uses of its tile, as LUT and flip-flop numbers; two
        // BELs that use the same one exclude each other, so each is in the
        // other's footprint.
        //
        // - The distributed RAM site uses LUTs 0..5 (slices A, B and the
        //   RAMW slice C). On real silicon slices C and D share clock and
        //   set/reset settings, and those carry the RAM's write port, so both
        //   slices' flip-flops are taken too.
        // - A wide LUT uses its slice's two LUTs and the slice's first
        //   flip-flop, whose M0 input carries the mux select.
        //
        // The footprint lists LUTs first, then flip-flops, then other BELs,
        // each in BEL order: a RAM's pins are its footprint LUTs in order.
        const RAM_LUTS: std::ops::Range<usize> = 0..6;
        const RAMW_FFS: std::ops::Range<usize> = 4..8;
        let Some(tile) = self.tile_at(x, y) else {
            return Vec::new();
        };
        let bels = tile.bels();
        let ordinal = |b: usize| {
            bels[..b]
                .iter()
                .filter(|o| {
                    o.bel_type == bels[b].bel_type
                        || (o.bel_type.is_ff() && bels[b].bel_type.is_ff())
                })
                .count()
        };
        let real = !self.prjoxide_tiles.is_empty();
        // (LUTs, flip-flops) a BEL uses
        let uses = |b: usize| -> (Vec<usize>, Vec<usize>) {
            match bels[b].bel_type {
                BelType::Lut4 => (vec![ordinal(b)], Vec::new()),
                t if t.is_ff() => (Vec::new(), vec![ordinal(b)]),
                BelType::WideLut => {
                    let s = ordinal(b);
                    (vec![2 * s, 2 * s + 1], vec![2 * s])
                }
                BelType::DistRam => (
                    RAM_LUTS.collect(),
                    if real { RAMW_FFS.collect() } else { Vec::new() },
                ),
                _ => (Vec::new(), Vec::new()),
            }
        };
        if bel_index >= bels.len() {
            return Vec::new();
        }
        let (luts, ffs) = uses(bel_index);
        if luts.is_empty() && ffs.is_empty() {
            return Vec::new();
        }
        let mut overlapping: Vec<(u8, usize)> = (0..bels.len())
            .filter(|&b| b != bel_index)
            .filter_map(|b| {
                let (l, f) = uses(b);
                let shares =
                    l.iter().any(|x| luts.contains(x)) || f.iter().any(|x| ffs.contains(x));
                let rank = match bels[b].bel_type {
                    BelType::Lut4 => 0,
                    t if t.is_ff() => 1,
                    _ => 2,
                };
                shares.then_some((rank, b))
            })
            .collect();
        overlapping.sort_unstable();
        overlapping.into_iter().map(|(_, b)| b).collect()
    }

    fn wire_count(&self) -> usize {
        self.wires.len()
    }

    fn route_cost_estimate(&self, dx: u32, dy: u32) -> Option<f64> {
        let last = self.hop_costs.len().checked_sub(1)?;
        Some(self.hop_costs[(dx as usize).min(last)] + self.hop_costs[(dy as usize).min(last)])
    }

    fn is_general_routing_wire(&self, wire: WireId) -> bool {
        self.wire(wire)
            .is_some_and(|w| is_general_wire_name(&w.name))
    }

    fn default_package(&self) -> Option<&str> {
        self.variant.default_package()
    }

    fn pll_input_pin(&self, cell: &skalp_lir::gate_netlist::Cell, input: usize) -> Option<usize> {
        pll::input_router_pin(cell, input).filter(|_| pll::is_pll(cell))
    }

    fn pll_output_pin(&self, cell: &skalp_lir::gate_netlist::Cell, output: usize) -> Option<usize> {
        pll::output_router_pin(cell, output).filter(|_| pll::is_pll(cell))
    }

    fn dsp_input_pin(&self, cell: &skalp_lir::gate_netlist::Cell, input: usize) -> Option<usize> {
        dsp::router_pin(&dsp::input_pin(cell, input)?).filter(|_| dsp::is_multiplier(cell))
    }

    fn dsp_input_needs_low_driver(
        &self,
        cell: &skalp_lir::gate_netlist::Cell,
        input: usize,
    ) -> bool {
        dsp::is_multiplier(cell) && dsp::input_pin(cell, input).is_some()
    }

    fn dsp_low_driven_input_count(&self, cell: &skalp_lir::gate_netlist::Cell) -> usize {
        if dsp::is_multiplier(cell) {
            dsp::low_driven_pins().len()
        } else {
            0
        }
    }

    fn ram_input_pin(
        &self,
        ports: &skalp_lir::gate_netlist::RamPorts,
        input: usize,
    ) -> Option<usize> {
        ebr::router_pin(&ebr::input_pin(ports, input)?)
    }

    fn ram_input_needs_low_driver(
        &self,
        ports: &skalp_lir::gate_netlist::RamPorts,
        input: usize,
    ) -> bool {
        ebr::input_pin(ports, input).is_some_and(|pin| !ebr::has_inverter(&pin))
    }

    fn bel_pin_wire(&self, x: u32, y: u32, bel_index: usize, pin: CellPin) -> Option<WireId> {
        let is_io = self
            .tile_at(x, y)
            .and_then(|t| t.bels().get(bel_index))
            .is_some_and(|b| b.bel_type == BelType::IoCell);
        if is_io {
            let suffix = match pin {
                CellPin::Input(0) => "O",
                CellPin::Output(0) => "I",
                _ => return None,
            };
            return self
                .bel_wires
                .get(&(x, y, format!("PIO{bel_index}_{suffix}")))
                .copied();
        }
        let is_multiplier = !self.prjoxide_tiles.is_empty()
            && self
                .tile_at(x, y)
                .and_then(|t| t.bels().get(bel_index))
                .is_some_and(|b| b.bel_type == BelType::DspSlice);
        if is_multiplier {
            let name = match pin {
                CellPin::Output(bit) => format!("Z{bit}"),
                CellPin::Input(n) => dsp::pin_of_router_pin(n)?,
            };
            return self.bel_wires.get(&(x, y, format!("DSP_{name}"))).copied();
        }
        let is_block_ram = !self.prjoxide_tiles.is_empty()
            && self
                .tile_at(x, y)
                .and_then(|t| t.bels().get(bel_index))
                .is_some_and(|b| b.bel_type == BelType::RamSlice);
        if is_block_ram {
            let name = match pin {
                CellPin::Output(bit) => ebr::output_pin(bit),
                CellPin::Input(n) => ebr::pin_of_router_pin(n)?.to_string(),
            };
            return self.bel_wires.get(&(x, y, format!("RAM_{name}"))).copied();
        }
        let is_pll = !self.prjoxide_tiles.is_empty()
            && self
                .tile_at(x, y)
                .and_then(|t| t.bels().get(bel_index))
                .is_some_and(|b| b.bel_type == BelType::Pll);
        if is_pll {
            let name = match pin {
                CellPin::Output(n) => pll::output_pins().get(n)?,
                CellPin::Input(n) => pll::input_pins().get(n)?,
            };
            return self.bel_wires.get(&(x, y, format!("PLL_{name}"))).copied();
        }
        let is_real_dist_ram = !self.prjoxide_tiles.is_empty()
            && self
                .tile_at(x, y)
                .and_then(|t| t.bels().get(bel_index))
                .is_some_and(|b| b.bel_type == BelType::DistRam);
        if is_real_dist_ram {
            let key = match pin {
                CellPin::Output(o) => format!("DPR_DO{o}"),
                CellPin::Input(i @ 0..=3) => format!("DPR_DI{i}"),
                CellPin::Input(i @ 4..=7) => format!("DPR_WAD{}", i - 4),
                CellPin::Input(8) => "DPR_WRE".to_string(),
                CellPin::Input(crate::device::FF_PIN_CLK) => "DPR_WCK".to_string(),
                CellPin::Input(i) if i >= crate::device::DIST_RAM_PIN_RAD => {
                    let n = i - crate::device::DIST_RAM_PIN_RAD;
                    format!("DPR_RAD{}_{}", n / 4, n % 4)
                }
                CellPin::Input(_) => return None,
            };
            return self.bel_wires.get(&(x, y, key)).copied();
        }
        crate::device::lattice_logic_pin_wire(self, x, y, bel_index, pin, "Z", |name| {
            self.bel_wires.get(&(x, y, name)).copied()
        })
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
    fn test_synthetic_routing_reaches_every_tile() {
        crate::device::check_synthetic_routing(&NexusDevice::new(NexusVariant::Lifcl40));
    }

    #[test]
    fn test_logic_tile_pin_wires() {
        crate::device::check_lattice_logic_pin_wires(&NexusDevice::new(NexusVariant::Lifcl40));
    }

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
    fn general_wires_are_recognized() {
        assert!(is_general_wire_name("R2C97_H01W0300"));
        assert!(is_general_wire_name("R2C97_V02S0601"));
        assert!(is_general_wire_name("R2C97_H00L0100"));
        assert!(!is_general_wire_name("R2C97_BRANCH:HPBX0000"));
        assert!(!is_general_wire_name("R19C109_SPINE:VPSX0000"));
        assert!(!is_general_wire_name("R36C76_JCLK0"));
        assert!(!is_general_wire_name("G:JOSCHF_TMID_CORE_TMIDMUX"));
    }

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

        // Locate a PLC (Logic) cell in the window. Those on the window's
        // inner edges hold no BELs: their clock comes from outside it.
        let mut plc = None;
        for y in 4..=12 {
            for x in 4..=12 {
                if let Some(t) = device.tile_at(x, y) {
                    if t.tile_type() == TileType::Logic && !t.bels().is_empty() {
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
