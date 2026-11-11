//! Device database module for FPGA architectures
//!
//! This module provides abstractions for describing FPGA device architectures,
//! including tiles, basic elements (BELs), wires, and programmable interconnect points (PIPs).

pub mod ecp5;
pub mod ice40;
pub mod nexus;
pub mod xc7;

use crate::collections::{HashMap, HashMapExt};
use serde::{Deserialize, Serialize};

/// Device family enumeration
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum DeviceFamily {
    /// Lattice iCE40 FPGAs
    Ice40,
    /// Lattice ECP5 FPGAs
    Ecp5,
    /// Lattice Nexus (CertusPro-NX, CrossLink-NX) FPGAs
    Nexus,
    /// Xilinx 7-series (Artix-7, Kintex-7, Spartan-7) FPGAs
    Xc7,
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

/// DSP block description
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DspTile {
    /// Tile coordinates
    pub x: u32,
    pub y: u32,
    /// Number of MAC16 units at this location
    pub mac_count: u8,
}

/// Package pin mapping
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PackagePins {
    /// Package name
    pub name: String,
    /// Pin mapping: pin name -> (tile_x, tile_y, bel_idx)
    pub pins: HashMap<String, (u32, u32, u8)>,
}

/// One carry position in a logic tile.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CarrySlot {
    /// Index of the carry BEL in the tile's BEL list
    pub carry_bel: usize,
    /// LUT BELs the carry cell occupies (a `CCU2` uses its slice's two LUTs)
    pub lut_bels: Vec<usize>,
}

/// A pin of a placed cell, by its position in the cell's input or output
/// list.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CellPin {
    /// `cell.inputs[i]`
    Input(usize),
    /// `cell.outputs[i]`
    Output(usize),
}

/// Router pin number of a flip-flop's clock (data is pin 0).
pub const FF_PIN_CLK: usize = 100;
/// Router pin number of a flip-flop's clock enable.
pub const FF_PIN_CE: usize = 101;
/// Router pin number of a flip-flop's set/reset.
pub const FF_PIN_LSR: usize = 102;
/// First router pin number of a distributed RAM's read address connections:
/// read-address bit `r` reaches LUT `j` of the RAM at `base + 4 * r + j`.
pub const DIST_RAM_PIN_RAD: usize = 200;
/// Router pin number of a wide LUT's select input. Its other inputs `i`
/// (0..4) reach the first LUT at pin `i` and the second at pin `4 + i`.
pub const WIDE_LUT_PIN_SEL: usize = 8;
/// Router pin number of a block RAM's read clock. Its write clock is
/// [`FF_PIN_CLK`].
pub const RAM_PIN_READ_CLK: usize = 103;

/// Cell parameter holding where a multiplier's added low-driven inputs start.
pub const DSP_EXTRA_INPUTS_PARAM: &str = "DSP_EXTRA_INPUTS_FROM";

/// [`Device::ff_control_group`] for a tile whose flip-flops share their
/// control signals in runs of `group_size`, in BEL order (two per ECP5
/// slice; four on Nexus, whose slices A and B, and C and D, share one
/// clock, clock enable and set/reset line).
pub(crate) fn ff_slice_group<D: Device + ?Sized>(
    device: &D,
    x: u32,
    y: u32,
    bel_index: usize,
    group_size: usize,
) -> Option<usize> {
    let tile = device.tile_at(x, y)?;
    let bels = tile.bels();
    if !bels.get(bel_index)?.bel_type.is_ff() {
        return None;
    }
    let ordinal = bels[..bel_index]
        .iter()
        .filter(|b| b.bel_type.is_ff())
        .count();
    Some(ordinal / group_size)
}

/// Whether a router pin number is a clock pin, routed on the clock network.
pub fn is_clock_pin(pin: usize) -> bool {
    pin == FF_PIN_CLK || pin == RAM_PIN_READ_CLK
}

/// [`Device::bel_pin_wire`] for a Lattice logic tile (Nexus, ECP5) whose
/// BEL pin wires are named by LUT and FF number: `LUT{n}_A..D`,
/// `LUT{n}_{lut_out}`, `FF{n}_D`, `FF{n}_Q`. LUTs and FFs are numbered in BEL
/// order whatever the tile's BEL layout.
///
/// - A carry cell (`CCU2`: inputs A0 B0 C0 D0 A1 B1 C1 D1 CIN, outputs S0
///   S1 COUT) uses the pins of the two LUTs of its slice; CIN and COUT are
///   the dedicated carry path, not general routing.
/// - A distributed RAM (`DPR16X4`: inputs DI0..3 WAD0..3 WRE RAD0..3,
///   outputs DO0..3) uses the pins of its footprint LUTs: DO`i` leaves on
///   read LUT `i`, RAD on read LUT 0, WRE on read LUT 1, DI and WAD on the
///   write-port LUTs (footprint LUTs 4 and 5).
/// - A wide LUT (`WideLut`, a 5-input LUT cell) uses the pins of its two
///   footprint LUTs: input `i` < 4 on the first at router pin `i` and on the
///   second at `4 + i`, the select ([`WIDE_LUT_PIN_SEL`]) on the BEL's own
///   `WIDE{n}_SEL` wire, the output on `WIDE{n}_Z`.
/// - FF control pins (CE, LSR) are not modelled here.
pub(crate) fn lattice_logic_pin_wire<D: Device + ?Sized>(
    device: &D,
    x: u32,
    y: u32,
    bel_index: usize,
    pin: CellPin,
    lut_out: &str,
    wire: impl Fn(String) -> Option<WireId>,
) -> Option<WireId> {
    let tile = device.tile_at(x, y)?;
    let bels = tile.bels();
    let bel_type = bels.get(bel_index)?.bel_type;
    let lut_number = |b: usize| -> Option<usize> {
        (bels.get(b)?.bel_type == BelType::Lut4).then(|| {
            bels[..b]
                .iter()
                .filter(|o| o.bel_type == BelType::Lut4)
                .count()
        })
    };
    let lut_pin = |b: usize, pin: CellPin| -> Option<WireId> {
        let n = lut_number(b)?;
        match pin {
            CellPin::Input(i) => wire(format!("LUT{}_{}", n, ["A", "B", "C", "D"][i.min(3)])),
            CellPin::Output(_) => wire(format!("LUT{}_{}", n, lut_out)),
        }
    };
    match bel_type {
        BelType::Lut4 => lut_pin(bel_index, pin),
        t if t.is_ff() => {
            let n = bels[..bel_index]
                .iter()
                .filter(|o| o.bel_type.is_ff())
                .count();
            match pin {
                CellPin::Input(0) => wire(format!("FF{}_D", n)),
                CellPin::Input(FF_PIN_CLK) => wire(format!("FF{}_CLK", n)),
                CellPin::Input(FF_PIN_CE) => wire(format!("FF{}_CE", n)),
                CellPin::Input(FF_PIN_LSR) => wire(format!("FF{}_LSR", n)),
                CellPin::Input(_) => None,
                CellPin::Output(_) => wire(format!("FF{}_Q", n)),
            }
        }
        BelType::Carry => {
            let layout = device.carry_chain_layout()?;
            let slot = layout.slots.iter().find(|s| s.carry_bel == bel_index)?;
            match pin {
                CellPin::Input(i) if i < 8 => {
                    lut_pin(*slot.lut_bels.get(i / 4)?, CellPin::Input(i % 4))
                }
                CellPin::Output(o) if o < 2 => lut_pin(*slot.lut_bels.get(o)?, CellPin::Output(0)),
                _ => None,
            }
        }
        BelType::WideLut => {
            let n = bels[..bel_index]
                .iter()
                .filter(|o| o.bel_type == BelType::WideLut)
                .count();
            let luts: Vec<usize> = device
                .bel_footprint(x, y, bel_index)
                .into_iter()
                .filter(|&b| bels.get(b).is_some_and(|b| b.bel_type == BelType::Lut4))
                .collect();
            match pin {
                CellPin::Input(WIDE_LUT_PIN_SEL) => wire(format!("WIDE{}_SEL", n)),
                CellPin::Input(i) if i < 8 => lut_pin(*luts.get(i / 4)?, CellPin::Input(i % 4)),
                CellPin::Input(_) => None,
                CellPin::Output(_) => wire(format!("WIDE{}_Z", n)),
            }
        }
        BelType::DistRam => {
            let luts = device.bel_footprint(x, y, bel_index);
            let (lut, lut_input) = match pin {
                CellPin::Output(o) if o < 4 => return lut_pin(*luts.get(o)?, CellPin::Output(0)),
                CellPin::Output(_) => return None,
                CellPin::Input(i @ 0..=3) => (4, i), // DI0..3
                CellPin::Input(i @ 4..=7) => (5, i - 4), // WAD0..3
                CellPin::Input(8) => (1, 0),         // WRE
                CellPin::Input(i @ 9..=12) => (0, i - 9), // RAD0..3
                CellPin::Input(_) => return None,
            };
            lut_pin(*luts.get(lut)?, CellPin::Input(lut_input))
        }
        _ => None,
    }
}

/// Extra switch-box PIPs for a synthetic routing graph, which otherwise only
/// connects wires through a tile's local wires:
///
/// - every outgoing inter-tile wire (span or neighbour wire driven from a
///   local wire of its own tile) is also driven from `fanin - 1` more local
///   wires of that tile, spread evenly around it;
/// - every incoming inter-tile wire (one that lands on a local wire of the
///   tile it reaches) drives the tile's outgoing inter-tile wires of a
///   matching index (`index % 4`), so a route can continue straight or turn
///   without spending a local wire per tile;
/// - every incoming inter-tile wire also lands on `fanin - 1` more local
///   wires of the tile, spread evenly around it, so the signals entering a
///   busy tile do not all queue for the one local wire each lands on.
///
/// Without them a signal could leave a tile only on the one or two wires its
/// local wire drives, so long routes hopped around each tile's locals (the
/// scarcest wires) and ran them out, and the router searched far wider than
/// its distance estimate assumed.
///
/// Returns (src, dst, delay, tile_x, tile_y) for each new PIP, copying the
/// delay of the existing PIP into `dst`.
pub(crate) fn synthetic_switch_box_pips(
    wires: &[Wire],
    pips: &[Pip],
    locals_per_tile: usize,
    fanin: usize,
) -> Vec<(WireId, WireId, u32, u32, u32)> {
    use crate::collections::HashSet;
    let wire = |w: WireId| wires.get(w.0 as usize);
    let is_inter_tile = |t: WireType| {
        matches!(
            t,
            WireType::Span4H(_)
                | WireType::Span4V(_)
                | WireType::Span12H(_)
                | WireType::Span12V(_)
                | WireType::Neighbour
        )
    };
    let index = |t: WireType| match t {
        WireType::Span4H(i) | WireType::Span4V(i) | WireType::Span12H(i) | WireType::Span12V(i) => {
            i as usize
        }
        _ => 0,
    };
    let mut locals: HashMap<(u32, u32, usize), WireId> = HashMap::new();
    for w in wires {
        if let WireType::Local(i) = w.wire_type {
            locals.insert((w.tile_x, w.tile_y, i as usize), w.id);
        }
    }
    let mut existing: HashSet<(WireId, WireId)> =
        pips.iter().map(|p| (p.src_wire, p.dst_wire)).collect();
    let mut extra = Vec::new();

    // Each inter-tile wire is driven from a local wire at one end and lands
    // on a local wire at the other. Let it also be driven from the far end
    // and land at the near end (as on a real device, where spans run in
    // both directions): the graph had spans going east and north only, so
    // every route west or south took one neighbour wire per tile.
    let mut drive: HashMap<WireId, (usize, u32)> = HashMap::new();
    let mut land: HashMap<WireId, (usize, u32)> = HashMap::new();
    for p in pips {
        let (Some(src), Some(dst)) = (wire(p.src_wire), wire(p.dst_wire)) else {
            continue;
        };
        let same_tile = (src.tile_x, src.tile_y) == (dst.tile_x, dst.tile_y);
        match (src.wire_type, dst.wire_type) {
            (WireType::Local(i), t) if is_inter_tile(t) && same_tile => {
                drive.entry(p.dst_wire).or_insert((i as usize, p.delay));
            }
            (t, WireType::Local(j)) if is_inter_tile(t) && !same_tile => {
                land.entry(p.src_wire).or_insert((j as usize, p.delay));
            }
            _ => {}
        }
    }
    let mut far_end: HashMap<WireId, (u32, u32)> = HashMap::new();
    for p in pips {
        if let (Some(src), Some(dst)) = (wire(p.src_wire), wire(p.dst_wire)) {
            if land.contains_key(&p.src_wire)
                && matches!(dst.wire_type, WireType::Local(_))
                && (src.tile_x, src.tile_y) != (dst.tile_x, dst.tile_y)
            {
                far_end
                    .entry(p.src_wire)
                    .or_insert((dst.tile_x, dst.tile_y));
            }
        }
    }
    let mut reverse: Vec<Pip> = Vec::new();
    let mut span_wires: Vec<WireId> = drive.keys().copied().collect();
    span_wires.sort_unstable_by_key(|w| w.0);
    for w in span_wires {
        let (Some(&(i, d_out)), Some(&(j, d_in)), Some(&(fx, fy)), Some(near)) =
            (drive.get(&w), land.get(&w), far_end.get(&w), wire(w))
        else {
            continue;
        };
        let (nx, ny) = (near.tile_x, near.tile_y);
        if let Some(&l) = locals.get(&(fx, fy, i)) {
            if existing.insert((l, w)) {
                extra.push((l, w, d_out, fx, fy));
                reverse.push(Pip {
                    id: PipId(0),
                    src_wire: l,
                    dst_wire: w,
                    delay: d_out,
                    configurable: true,
                    tile_x: fx,
                    tile_y: fy,
                });
            }
        }
        if let Some(&l) = locals.get(&(nx, ny, j)) {
            if existing.insert((w, l)) {
                extra.push((w, l, d_in, nx, ny));
                reverse.push(Pip {
                    id: PipId(0),
                    src_wire: w,
                    dst_wire: l,
                    delay: d_in,
                    configurable: true,
                    tile_x: nx,
                    tile_y: ny,
                });
            }
        }
    }

    /// Per tile: (wire, local index, PIP delay)
    type TileEnds = HashMap<(u32, u32), Vec<(WireId, usize, u32)>>;
    // Outgoing wires and the local driving each; incoming wires and the
    // local each lands on
    let mut outgoing: TileEnds = HashMap::new();
    let mut incoming: TileEnds = HashMap::new();
    for p in pips.iter().chain(&reverse) {
        let (Some(src), Some(dst)) = (wire(p.src_wire), wire(p.dst_wire)) else {
            continue;
        };
        // Keyed by the local wire's tile: after the reversal above a wire is
        // outgoing and incoming at both of its ends
        match (src.wire_type, dst.wire_type) {
            (WireType::Local(i), t) if is_inter_tile(t) => {
                outgoing
                    .entry((src.tile_x, src.tile_y))
                    .or_default()
                    .push((p.dst_wire, i as usize, p.delay));
            }
            (t, WireType::Local(j)) if is_inter_tile(t) => {
                incoming
                    .entry((dst.tile_x, dst.tile_y))
                    .or_default()
                    .push((p.src_wire, j as usize, p.delay));
            }
            _ => {}
        }
    }
    let mut tiles: Vec<(u32, u32)> = outgoing.keys().copied().collect();
    tiles.sort_unstable();
    for (x, y) in tiles {
        let outs = &outgoing[&(x, y)];
        for &(dst, i, delay) in outs {
            for m in 1..fanin {
                let k = (i + m * locals_per_tile / fanin) % locals_per_tile;
                if let Some(&src) = locals.get(&(x, y, k)) {
                    if existing.insert((src, dst)) {
                        extra.push((src, dst, delay, x, y));
                    }
                }
            }
        }
        for &(src, j, land_delay) in incoming.get(&(x, y)).into_iter().flatten() {
            for m in 1..fanin {
                let k = (j + m * locals_per_tile / fanin) % locals_per_tile;
                if let Some(&dst) = locals.get(&(x, y, k)) {
                    if existing.insert((src, dst)) {
                        extra.push((src, dst, land_delay, x, y));
                    }
                }
            }
            let src_index = wire(src).map(|w| index(w.wire_type) % 4).unwrap_or(0);
            for &(dst, _, delay) in outs {
                let same_wire = src == dst;
                let dst_index = wire(dst).map(|w| index(w.wire_type) % 4).unwrap_or(0);
                if !same_wire && dst_index == src_index && existing.insert((src, dst)) {
                    extra.push((src, dst, delay, x, y));
                }
            }
        }
    }
    extra
}

/// Least routing cost to move `d` tiles along one axis, for `d` in
/// `0..=max_d`, on a graph whose inter-tile wires each span `span` tiles in
/// either direction at a PIP cost of `cost` (see [`Pip::cost`]). An exact
/// lower bound for [`Device::route_cost_estimate`]: turns cost nothing extra
/// (a route turns by taking a wire of the other axis).
pub(crate) fn span_hop_costs(max_d: usize, spans: &[(usize, f64)]) -> Vec<f64> {
    let reach = max_d + spans.iter().map(|s| s.0).max().unwrap_or(1);
    let mut dist = vec![f64::INFINITY; reach + 1];
    let mut done = vec![false; reach + 1];
    dist[0] = 0.0;
    // Dijkstra on positions 0..=reach (moves of ±span)
    for _ in 0..=reach {
        let Some(u) = (0..=reach)
            .filter(|&i| !done[i])
            .min_by(|&a, &b| dist[a].total_cmp(&dist[b]))
        else {
            break;
        };
        if dist[u].is_infinite() {
            break;
        }
        done[u] = true;
        for &(span, cost) in spans {
            for v in [u.checked_add(span), u.checked_sub(span)]
                .into_iter()
                .flatten()
            {
                if v <= reach && dist[u] + cost < dist[v] {
                    dist[v] = dist[u] + cost;
                }
            }
        }
    }
    dist.truncate(max_d + 1);
    dist
}

/// How carry chains run through logic tiles.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CarryChainLayout {
    /// Carry positions of a logic tile, in chain order
    pub slots: Vec<CarrySlot>,
    /// Offset from a tile to the next tile along the chain
    pub step: (i32, i32),
}

impl CarryChainLayout {
    /// Layout of a tile whose carry cells each take two LUTs, in BEL order:
    /// carry `k` occupies the `2k`-th and `2k+1`-th LUT BELs (one slice).
    /// `None` unless the tile has exactly two LUT BELs per carry BEL.
    pub fn paired_luts(bels: &[Bel], step: (i32, i32)) -> Option<Self> {
        let carries: Vec<usize> = bels
            .iter()
            .enumerate()
            .filter(|(_, b)| b.bel_type == BelType::Carry)
            .map(|(i, _)| i)
            .collect();
        let luts: Vec<usize> = bels
            .iter()
            .enumerate()
            .filter(|(_, b)| b.bel_type == BelType::Lut4)
            .map(|(i, _)| i)
            .collect();
        if carries.is_empty() || luts.len() != 2 * carries.len() {
            return None;
        }
        Some(Self {
            slots: carries
                .iter()
                .enumerate()
                .map(|(k, &carry_bel)| CarrySlot {
                    carry_bel,
                    lut_bels: vec![luts[2 * k], luts[2 * k + 1]],
                })
                .collect(),
            step,
        })
    }
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

    /// Get all PIPs driven by a wire (wire is source) - for forward routing.
    /// Borrowed: the router's search calls this for every wire it expands.
    fn wire_src_pips(&self, wire_id: WireId) -> &[PipId];

    /// Get package pin mappings
    fn packages(&self) -> &HashMap<String, PackagePins>;

    /// Get the routing architecture
    fn routing(&self) -> &RoutingArchitecture;

    /// Get clock resources
    fn clock_resources(&self) -> &ClockResources;

    /// Check if a cell type can be placed on a BEL type
    fn can_place(&self, cell_type: &str, bel_type: BelType) -> bool;

    /// Total number of wires in the device (for Vec-indexed data structures)
    fn wire_count(&self) -> usize {
        0
    }

    /// Carry-chain geometry for chain-aware placement of two-bit carry cells
    /// (Lattice `CCU2`). `None`: the device has no such layout, and carry
    /// cells (e.g. iCE40 `SB_CARRY`) use the logic-cell chain placement.
    fn carry_chain_layout(&self) -> Option<CarryChainLayout> {
        None
    }

    /// Other BELs of the same tile that a cell on BEL `bel_index` of tile
    /// (x, y) uses up: a distributed RAM site is built from the tile's LUTs,
    /// so placing a RAM there takes those LUTs, and a LUT in use blocks the
    /// RAM site. Empty for an ordinary BEL.
    fn bel_footprint(&self, _x: u32, _y: u32, _bel_index: usize) -> Vec<usize> {
        Vec::new()
    }

    /// The group of flip-flop BELs of tile (x, y) that BEL `bel_index`
    /// belongs to, whose flip-flops share one clock, clock enable and
    /// set/reset (a Lattice slice; a whole iCE40 logic tile). Flip-flops in
    /// one group must have the same control signals. `None`: the BEL is not a
    /// flip-flop, or its controls are its own.
    fn ff_control_group(&self, _x: u32, _y: u32, _bel_index: usize) -> Option<usize> {
        None
    }

    /// A lower bound on the routing cost (sum of [`Pip::cost`]) of a path
    /// between tiles `dx` and `dy` apart, for the router's A* search. `None`:
    /// the router uses its generic estimate.
    fn route_cost_estimate(&self, _dx: u32, _dy: u32) -> Option<f64> {
        None
    }

    // BEL pin wire methods (optional, for proper routing endpoints)

    /// Whether a wire is general fabric routing, which a clock must not use.
    fn is_general_routing_wire(&self, _wire: WireId) -> bool {
        false
    }

    /// The package whose pin names a design's constraints use, if the device has several.
    fn default_package(&self) -> Option<&str> {
        None
    }

    /// Router pin a multiplier input connects to, or `None` if it has no pin.
    /// Router pin number of a PLL cell's `input`-th input, where the device
    /// names PLL pins (`None`: the input's index).
    fn pll_input_pin(&self, _cell: &skalp_lir::gate_netlist::Cell, _input: usize) -> Option<usize> {
        None
    }

    /// Router pin number of a PLL cell's `output`-th output, as
    /// [`Self::pll_input_pin`].
    fn pll_output_pin(
        &self,
        _cell: &skalp_lir::gate_netlist::Cell,
        _output: usize,
    ) -> Option<usize> {
        None
    }

    fn dsp_input_pin(&self, _cell: &skalp_lir::gate_netlist::Cell, _input: usize) -> Option<usize> {
        None
    }

    /// Extra multiplier inputs the device needs held low with a real signal.
    fn dsp_low_driven_input_count(&self, _cell: &skalp_lir::gate_netlist::Cell) -> usize {
        0
    }

    /// Whether a multiplier input tied low must be driven by a real signal.
    fn dsp_input_needs_low_driver(
        &self,
        _cell: &skalp_lir::gate_netlist::Cell,
        _input: usize,
    ) -> bool {
        false
    }

    /// Router pin a block RAM input connects to, or `None` if it has no pin.
    fn ram_input_pin(
        &self,
        _ports: &skalp_lir::gate_netlist::RamPorts,
        input: usize,
    ) -> Option<usize> {
        Some(input)
    }

    /// Router output pin a block RAM output comes from.
    fn ram_output_pin(&self, _ports: &skalp_lir::gate_netlist::RamPorts, output: usize) -> usize {
        output
    }

    /// Whether a block RAM input tied low must be driven by a real signal
    /// because configuration alone cannot hold it low.
    fn ram_input_needs_low_driver(
        &self,
        _ports: &skalp_lir::gate_netlist::RamPorts,
        _input: usize,
    ) -> bool {
        false
    }

    /// The wire that `pin` of a cell placed on BEL `bel_index` of tile
    /// (x, y) connects to. `None`: the device does not model it, and the
    /// router falls back to the iCE40 logic-cell layout (a LUT/DFF pair per
    /// logic cell, BEL `2k`/`2k + 1`, through [`Self::lut_output_wire`] and
    /// [`Self::lut_input_wire`]).
    fn bel_pin_wire(&self, _x: u32, _y: u32, _bel_index: usize, _pin: CellPin) -> Option<WireId> {
        None
    }

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

    /// Check if an IO pad is GBUF-capable, return the global network number
    fn gbufpin_for_pad(&self, _tile_x: u32, _tile_y: u32, _pio_num: u8) -> Option<u8> {
        None
    }

    /// Get the padin_glb_netwk extra bit for a global network
    /// Returns (bank, addr_x, addr_y)
    fn padin_extra_bit(&self, _glb_num: u8) -> Option<(u32, u32, u32)> {
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
    /// Distributed (LUT) RAM site: a 16 × 4 RAM built from a logic tile's
    /// LUTs (Lattice DPR16X4). Its LUTs are the site's footprint
    /// ([`Device::bel_footprint`]).
    DistRam,
    /// A wide LUT: a slice's two LUT4s and the mux between them, taking a
    /// 5-input LUT (Lattice PFUMX / Nexus WIDEFN9). Its LUTs and the
    /// flip-flop whose input carries the mux select are its footprint
    /// ([`Device::bel_footprint`]).
    WideLut,
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

/// Checks for [`Device::bel_pin_wire`] on a Lattice logic tile: every pin of
/// every LUT, FF, carry and RAM BEL gets its own wire, outputs drive the
/// fabric and inputs are driven from it.
#[cfg(test)]
pub(crate) fn check_lattice_logic_pin_wires<D: Device>(device: &D) {
    use crate::collections::{HashSet, HashSetExt};
    let (w, h) = device.grid_size();
    let (x, y) = (0..h)
        .flat_map(|y| (0..w).map(move |x| (x, y)))
        .find(|&(x, y)| {
            device
                .tile_at(x, y)
                .is_some_and(|t| t.bels().iter().any(|b| b.bel_type == BelType::DistRam))
        })
        .expect("a logic tile with a RAM site");
    let bels = device.tile_at(x, y).unwrap().bels().to_vec();
    let pin = |b: usize, p: CellPin| device.bel_pin_wire(x, y, b, p);
    let drives = |wire: WireId| !device.wire_src_pips(wire).is_empty();
    let driven = |wire: WireId| !device.wire_pips(wire).is_empty();

    // LUTs and FFs: distinct output and data-input wires, all routable
    let mut outputs = HashSet::new();
    let mut inputs = HashSet::new();
    for (b, bel) in bels.iter().enumerate() {
        let n_inputs = match bel.bel_type {
            BelType::Lut4 => 4,
            t if t.is_ff() => 1,
            _ => continue,
        };
        let out = pin(b, CellPin::Output(0)).expect("output wire");
        assert!(outputs.insert(out), "BEL {b}: output wire shared");
        assert!(drives(out), "BEL {b}: output drives no PIP");
        for i in 0..n_inputs {
            let inp = pin(b, CellPin::Input(i)).expect("input wire");
            assert!(inputs.insert(inp), "BEL {b} input {i}: wire shared");
            assert!(driven(inp), "BEL {b} input {i}: no PIP drives it");
        }
    }

    // Carry cells: A0..D1 and S0/S1 on the slice's LUT pins; CIN and COUT
    // are the dedicated chain
    let layout = device.carry_chain_layout().expect("carry layout");
    for slot in &layout.slots {
        let c = slot.carry_bel;
        for i in 0..8 {
            let lut = slot.lut_bels[i / 4];
            assert_eq!(
                pin(c, CellPin::Input(i)),
                pin(lut, CellPin::Input(i % 4)),
                "carry {c} input {i}"
            );
        }
        for (o, &lut) in slot.lut_bels.iter().enumerate() {
            assert_eq!(pin(c, CellPin::Output(o)), pin(lut, CellPin::Output(0)));
        }
        assert_eq!(pin(c, CellPin::Input(8)), None, "CIN");
        assert_eq!(pin(c, CellPin::Output(2)), None, "COUT");
    }

    // RAM site: 13 distinct inputs, 4 distinct outputs, on its footprint LUTs
    let ram = bels
        .iter()
        .position(|b| b.bel_type == BelType::DistRam)
        .unwrap();
    let footprint = device.bel_footprint(x, y, ram);
    let footprint_wires: HashSet<WireId> = footprint
        .iter()
        .flat_map(|&l| {
            (0..4)
                .map(CellPin::Input)
                .chain([CellPin::Output(0)])
                .map(move |p| (l, p))
        })
        .filter_map(|(l, p)| pin(l, p))
        .collect();
    let ram_in: HashSet<WireId> = (0..13)
        .map(|i| pin(ram, CellPin::Input(i)).expect("RAM input wire"))
        .collect();
    let ram_out: HashSet<WireId> = (0..4)
        .map(|o| pin(ram, CellPin::Output(o)).expect("RAM output wire"))
        .collect();
    assert_eq!((ram_in.len(), ram_out.len()), (13, 4), "distinct RAM pins");
    assert!(ram_in
        .iter()
        .chain(&ram_out)
        .all(|w| footprint_wires.contains(w)));
    assert_eq!(pin(ram, CellPin::Input(13)), None);
    assert_eq!(pin(ram, CellPin::Output(4)), None);
}

/// Checks for a synthetic routing graph: from a LUT output in the middle of
/// the device, every logic tile's LUT inputs are reachable (in every
/// direction), and [`Device::route_cost_estimate`] never exceeds the true
/// least cost, so the router's A* search stays exact.
#[cfg(test)]
pub(crate) fn check_synthetic_routing<D: Device>(device: &D) {
    use std::cmp::Reverse;
    use std::collections::BinaryHeap;
    let (w, h) = device.grid_size();
    let logic: Vec<(u32, u32)> = (0..h)
        .flat_map(|y| (0..w).map(move |x| (x, y)))
        .filter(|&(x, y)| device.lut_output_wire(x, y, 0).is_some())
        .collect();
    let (sx, sy) = *logic
        .iter()
        .min_by_key(|&&(x, y)| (x as i64 - w as i64 / 2).abs() + (y as i64 - h as i64 / 2).abs())
        .unwrap();
    let source = device.lut_output_wire(sx, sy, 0).unwrap();
    // Dijkstra over PIP costs (integer, so exact)
    let mut dist = vec![u32::MAX; device.wire_count()];
    let mut heap = BinaryHeap::new();
    dist[source.0 as usize] = 0;
    heap.push(Reverse((0u32, source.0)));
    while let Some(Reverse((d, u))) = heap.pop() {
        if d > dist[u as usize] {
            continue;
        }
        for &p in device.wire_src_pips(WireId(u)) {
            let pip = device.pip(p).unwrap();
            let v = pip.dst_wire.0 as usize;
            let nd = d + pip.cost();
            if nd < dist[v] {
                dist[v] = nd;
                heap.push(Reverse((nd, v as u32)));
            }
        }
    }
    for &(x, y) in &logic {
        let input = device.lut_input_wire(x, y, 0, 0).unwrap();
        let d = dist[input.0 as usize];
        assert!(
            d != u32::MAX,
            "LUT input at ({x}, {y}) unreachable from ({sx}, {sy})"
        );
        let (dx, dy) = (x.abs_diff(sx), y.abs_diff(sy));
        let estimate = device.route_cost_estimate(dx, dy).expect("an estimate");
        assert!(
            estimate <= d as f64,
            "estimate {estimate} > cost {d} from ({sx}, {sy}) to ({x}, {y})"
        );
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn span_hop_costs_take_the_cheapest_mix_of_spans() {
        let c = span_hop_costs(30, &[(1, 1.0), (2, 1.0), (12, 1.0)]);
        assert_eq!(c[0], 0.0);
        assert_eq!(c[1], 1.0);
        assert_eq!(c[2], 1.0);
        assert_eq!(c[3], 2.0); // 2 + 1
        assert_eq!(c[11], 2.0); // 12 - 1
        assert_eq!(c[12], 1.0);
        assert_eq!(c[24], 2.0);
        assert_eq!(c[18], 4.0); // 12 + 2 + 2 + 2, or 24 - 2 - 2 - 2
                                // A dearer long span is used only when it pays
        let c = span_hop_costs(12, &[(1, 1.0), (2, 1.0), (6, 2.0)]);
        assert_eq!(c[6], 2.0);
        assert_eq!(c[12], 4.0);
        assert_eq!(c[5], 3.0); // 2 + 2 + 1, or 6 - 1
    }
}
