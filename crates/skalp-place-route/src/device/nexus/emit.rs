//! Turn a placed and routed design into Nexus configuration bits.
//!
//! The router connects wires; this module sets everything else: LUT truth
//! tables, flip-flop modes, carry mode, IO pads, the oscillator, and the
//! dedicated connections the router does not handle (carry chains, constants).
//!
//! The settings mirror what nextpnr writes for the same cells.

use super::prjoxide_graph::{
    load_iptype, load_tiletype, node_name, resolve_wire, ConfigBit, GlobalBit, TileBitsDb,
};
use super::{dsp, ebr, pll};
use super::{NexusDevice, NexusVariant, PrjoxideTile};
use crate::bitstream::config::{ConfigGenerator, ConfigSpace, DesignBit, PlacedDesign};
use crate::bitstream::prepare::{constant_value, is_pad};
use crate::collections::{HashMap, HashMapExt, HashSet, HashSetExt};
use crate::device::{BelType, Device};
use crate::error::{PlaceRouteError, Result};
use crate::placer::control_sets::ControlSet;
use crate::router::RoutingResult;
use skalp_lir::gate_netlist::{Cell, CellId};
use skalp_lir::tech_library::CellFunction;
use std::path::Path;

/// Slices per logic tile, in LUT/FF numbering order.
const SLICES: [char; 4] = ['A', 'B', 'C', 'D'];

/// Oscillator divider: 450 MHz / (24 + 1) = 18 MHz.
const OSC_HF_CLK_DIV: u64 = 24;
/// Clock frequency the oscillator produces with this divider, in MHz.
pub const OSC_FREQUENCY_MHZ: f64 = 450.0 / (OSC_HF_CLK_DIV as f64 + 1.0);
/// Divider for the configuration engine's clock.
const OSC_SED_SEC_DIV: u64 = 1;
/// Number given to a design's first block RAM; 0 and 1 are reserved.
const FIRST_RAM_ID: u64 = 2;

fn emit_err(message: String) -> PlaceRouteError {
    PlaceRouteError::BitstreamFailed(message)
}

/// Tiletype settings, loaded once per type.
struct ConfigDb<'a> {
    device: &'a NexusDevice,
    variant: NexusVariant,
    db_root: &'a Path,
    family: &'static str,
    cache: HashMap<String, Option<TileBitsDb>>,
    bits: Vec<GlobalBit>,
    /// Hard-block register bits on the configuration bus: (byte address,
    /// bit, value)
    bus_bits: Vec<(u32, u32, bool)>,
}

impl ConfigDb<'_> {
    fn tile(&self, x: u32, y: u32, tiletype: &str) -> Result<PrjoxideTile> {
        self.device
            .prjoxide_tile(x, y, |t| t == tiletype)
            .cloned()
            .ok_or_else(|| emit_err(format!("No {tiletype} tile at column {x}, row {y}")))
    }

    fn db(&mut self, tiletype: &str) -> Result<&TileBitsDb> {
        if !self.cache.contains_key(tiletype) {
            let loaded = load_tiletype(self.db_root, self.family, tiletype).map_err(emit_err)?;
            self.cache.insert(tiletype.to_string(), loaded);
        }
        self.cache
            .get(tiletype)
            .and_then(Option::as_ref)
            .ok_or_else(|| emit_err(format!("No configuration data for tile type {tiletype}")))
    }

    fn add(&mut self, tile: &PrjoxideTile, cell: &ConfigBit, value: bool) {
        self.bits.push(GlobalBit {
            frame: tile.start_frame + cell.frame as u32,
            bit: tile.start_bit + cell.bit as u32,
            invert: !value,
        });
    }

    fn set_enum(&mut self, tile: &PrjoxideTile, name: &str, option: &str) -> Result<()> {
        let bits = self
            .db(&tile.tiletype)?
            .enum_option(name, option)
            .ok_or_else(|| emit_err(format!("{}: no setting {name}.{option}", tile.tiletype)))?
            .to_vec();
        for bit in &bits {
            self.add(tile, bit, !bit.invert);
        }
        Ok(())
    }

    fn set_word(&mut self, tile: &PrjoxideTile, name: &str, value: u64) -> Result<()> {
        let bits = self
            .db(&tile.tiletype)?
            .word(name)
            .ok_or_else(|| emit_err(format!("{}: no word {name}", tile.tiletype)))?
            .to_vec();
        for (index, group) in bits.iter().enumerate() {
            let on = (value >> index) & 1 == 1;
            for bit in group {
                self.add(tile, bit, on ^ bit.invert);
            }
        }
        Ok(())
    }

    fn set_pip(&mut self, tile: &PrjoxideTile, dst: &str, src: &str) -> Result<()> {
        let bits = self
            .db(&tile.tiletype)?
            .pip(dst, src)
            .ok_or_else(|| emit_err(format!("{}: no pip {src} -> {dst}", tile.tiletype)))?
            .to_vec();
        for bit in &bits {
            self.add(tile, bit, !bit.invert);
        }
        Ok(())
    }
}

/// Set the bits each tile type requires regardless of the design.
fn set_always_on_bits(config: &mut ConfigDb<'_>) -> Result<()> {
    for tile in config.device.prjoxide_tiles().to_vec() {
        let bits = match config.db(&tile.tiletype) {
            Ok(db) => db.always_on().to_vec(),
            Err(_) => continue, // Tile types with no data have nothing to set.
        };
        for bit in &bits {
            config.add(&tile, bit, true);
        }
    }
    Ok(())
}

/// Make a site pin read high by selecting an unused input on its CIB mux.
///
/// A CIB input left unconnected floats high only when its mux has an input
/// selected that no net drives; the mux is otherwise undefined or follows
/// whatever signal happens to reach it.
fn float_high(
    config: &mut ConfigDb<'_>,
    pin: crate::device::WireId,
    used_wires: &HashSet<crate::device::WireId>,
) -> Result<()> {
    let device = config.device;
    let name = || {
        device
            .wire(pin)
            .map_or("?", |w| w.name.as_str())
            .to_string()
    };
    // A pin wired straight from a tile's set/reset line needs nothing: an
    // unconnected set/reset line reads high, as slices rely on
    let lsr = device.wire_pips(pin).into_iter().find_map(|p| {
        let src = device.pip(p)?.src_wire;
        let wire = device.wire(src)?;
        let line = wire.name.rsplit('_').next()?;
        let index = line.strip_prefix("JLSR")?;
        (!index.is_empty() && index.chars().all(|c| c.is_ascii_digit())).then_some(src)
    });
    if let Some(lsr) = lsr {
        if used_wires.contains(&lsr) {
            return Err(emit_err(format!(
                "The set/reset line feeding {} is used by a routed net, so the pin cannot float",
                name()
            )));
        }
        return Ok(());
    }
    let mux = find_cib_mux(device, pin)
        .ok_or_else(|| emit_err(format!("No CIB mux feeds {}", name())))?;
    if used_wires.contains(&mux) {
        return Err(emit_err(format!(
            "The CIB mux for {} is used by a routed net, so the pin cannot float",
            name()
        )));
    }
    let pip = device
        .wire_pips(mux)
        .into_iter()
        .find(|&p| {
            device
                .pip(p)
                .is_some_and(|i| !used_wires.contains(&i.src_wire))
        })
        .ok_or_else(|| emit_err(format!("No free input on the CIB mux for {}", name())))?;
    config
        .bits
        .extend(device.pip_config_bits(pip).iter().cloned());
    Ok(())
}

/// Hold the clock enable high on every clock buffer a route passes through.
///
/// A clock left without it never leaves the buffer.
fn enable_clock_buffers(
    config: &mut ConfigDb<'_>,
    routing: &RoutingResult,
    used_wires: &HashSet<crate::device::WireId>,
) -> Result<()> {
    let device = config.device;
    let mut enable_wires = HashSet::new();
    for route in routing.routes.values() {
        for &pip_id in &route.pips {
            let Some(pip) = device.pip(pip_id) else {
                continue;
            };
            let Some(name) = device.wire(pip.dst_wire).map(|w| w.name.as_str()) else {
                continue;
            };
            if name.contains("_JCLKO_DCC_") {
                enable_wires.insert(name.replace("_JCLKO_DCC_", "_JCE_DCC_"));
            }
        }
    }

    for enable in &enable_wires {
        let wire = device
            .wire_by_name(enable)
            .ok_or_else(|| emit_err(format!("No clock buffer enable wire {enable}")))?;
        float_high(config, wire, used_wires)?;
    }
    Ok(())
}

/// Walk upstream from a site pin to the CIB input mux that feeds it.
fn find_cib_mux(
    device: &NexusDevice,
    start: crate::device::WireId,
) -> Option<crate::device::WireId> {
    let mut cursor = start;
    for _ in 0..8 {
        let upstream = device.wire_pips(cursor);
        let sources: Vec<_> = upstream
            .iter()
            .filter_map(|&p| device.pip(p).map(|pip| pip.src_wire))
            .collect();
        if let Some(&mux) = sources
            .iter()
            .find(|&&w| device.wire(w).is_some_and(|w| w.name.contains("CIBMUXOUT")))
        {
            return Some(mux);
        }
        cursor = *sources.first()?;
    }
    None
}

/// Truth table of a named Nexus logic cell.
fn lut_init(cell: &Cell) -> Result<u64> {
    if let Some(init) = cell.lut_init {
        return Ok(init);
    }
    let init = match cell.function {
        Some(CellFunction::Inv) => 0x5555,
        Some(CellFunction::Buf) => 0xAAAA,
        _ => {
            return Err(emit_err(format!(
                "No truth table for cell '{}' of type {}",
                cell.path, cell.cell_type
            )))
        }
    };
    Ok(init)
}

/// Rewrite a LUT's truth table so inputs tied low can be left unconnected.
///
/// An unconnected LUT input reads 1, so every entry must take the value the
/// table held with those inputs at 0.
fn fold_tied_low(init: u64, tied_low: &[usize]) -> u64 {
    let mask = tied_low.iter().fold(0u64, |m, &pin| m | (1 << pin));
    (0..16u64)
        .filter(|&index| (init >> (index & !mask)) & 1 == 1)
        .fold(0, |folded, index| folded | (1 << index))
}

fn slice_of(index: usize) -> char {
    SLICES[index / 2]
}

/// Position of a BEL among the same-kind BELs of its tile.
fn nth_of_kind(device: &NexusDevice, loc: &crate::placer::PlacementLoc) -> Option<usize> {
    let tile = device.tile_at(loc.tile_x, loc.tile_y)?;
    let bels = tile.bels();
    let kind = bels.get(loc.bel_index)?.bel_type;
    let same = |t: BelType| {
        if kind.is_ff() {
            t.is_ff()
        } else {
            t == kind
        }
    };
    Some(
        bels[..loc.bel_index]
            .iter()
            .filter(|b| same(b.bel_type))
            .count(),
    )
}

/// Generates configuration bits for a Nexus device.
pub struct NexusConfig<'a> {
    /// Real device model the design was placed on
    pub device: &'a NexusDevice,
    /// Device variant
    pub variant: NexusVariant,
    /// prjoxide database root
    pub db_root: &'a Path,
}

impl ConfigGenerator for NexusConfig<'_> {
    fn device_name(&self) -> &str {
        self.device.name()
    }

    fn config_bits(&self, design: &PlacedDesign<'_>) -> Result<Vec<DesignBit>> {
        let bits = emit_bits(
            self.device,
            self.variant,
            design.netlist,
            design.placement,
            design.routing,
            design.tied,
            self.db_root,
        )?;
        let frames = bits.frames.into_iter().map(|b| DesignBit {
            frame: b.frame,
            bit: b.bit,
            value: !b.invert,
            space: ConfigSpace::Frames,
        });
        let bus = bits.bus.into_iter().map(|(frame, bit, value)| DesignBit {
            frame,
            bit,
            value,
            space: ConfigSpace::Bus,
        });
        Ok(frames.chain(bus).collect())
    }
}

/// Collect every configuration bit the design needs.
fn emit_bits(
    device: &NexusDevice,
    variant: NexusVariant,
    netlist: &skalp_lir::gate_netlist::GateNetlist,
    placement: &crate::placer::PlacementResult,
    routing: &RoutingResult,
    tied: &crate::bitstream::prepare::TiedInputs,
    db_root: &Path,
) -> Result<EmittedBits> {
    let mut config = ConfigDb {
        device,
        variant,
        db_root,
        family: variant.prjoxide_family(),
        cache: HashMap::new(),
        bits: Vec::new(),
        bus_bits: Vec::new(),
    };

    set_always_on_bits(&mut config)?;
    for route in routing.routes.values() {
        for &pip in &route.pips {
            config
                .bits
                .extend(device.pip_config_bits(pip).iter().cloned());
        }
    }

    let used_wires: HashSet<crate::device::WireId> = routing
        .routes
        .values()
        .flat_map(|r| r.wires.iter().copied())
        .collect();
    enable_clock_buffers(&mut config, routing, &used_wires)?;

    let is_tied_high = |cell: CellId, index: usize| tied.value(cell, index) == Some(true);
    let tied_low = |cell: CellId, first: usize, count: usize| -> Vec<usize> {
        (0..count)
            .filter(|&i| tied.value(cell, first + i) == Some(false))
            .collect()
    };
    // Each slice's control set, and a flip-flop holding it
    let control_sets = crate::placer::control_sets::control_sets(netlist);
    let mut slices_with_ff: HashMap<(u32, u32, char), (ControlSet, &str)> = HashMap::new();
    let mut pairs_with_ff: HashMap<(u32, u32, &str), (ControlSet, &str)> = HashMap::new();
    // Tiles whose slice C is a distributed RAM's write port
    let mut ram_write_tiles: HashSet<(u32, u32)> = HashSet::new();
    let mut next_ram_id = FIRST_RAM_ID;
    let mut used_multipliers: HashSet<(u32, u32, usize)> = HashSet::new();

    for cell in &netlist.cells {
        if constant_value(cell).is_some() {
            continue;
        }
        let loc = placement
            .placements
            .get(&cell.id)
            .ok_or_else(|| emit_err(format!("Cell '{}' was not placed", cell.path)))?;
        let (x, y) = (loc.tile_x, loc.tile_y);

        if is_pad(cell) {
            emit_pad(&mut config, cell, x, y, loc.bel_index, &used_wires)?;
            continue;
        }
        if loc.bel_type == BelType::DspSlice {
            emit_multiplier(&mut config, cell, x, y, tied, &used_wires)?;
            if let Some(tile) = device.prjoxide_tile(x, y, |t| dsp::slot_of_tile(t).is_some()) {
                used_multipliers.insert((
                    y,
                    dsp::row_base_column(&tile.tiletype, x).unwrap_or(x),
                    dsp::slot_of_tile(&tile.tiletype).unwrap_or(0),
                ));
            }
            continue;
        }
        if loc.bel_type == BelType::Pll {
            emit_pll(&mut config, cell, x, y, &used_wires)?;
            continue;
        }
        if loc.bel_type == BelType::RamSlice {
            let ram = PlacedRam {
                cell,
                site_x: x,
                site_y: y,
                id: next_ram_id,
            };
            emit_block_ram(&mut config, &ram, tied, &used_wires)?;
            next_ram_id += 1;
            continue;
        }

        let tile = config.tile(x, y, "PLC")?;
        let number = nth_of_kind(device, loc)
            .ok_or_else(|| emit_err(format!("Cell '{}' is on an unknown BEL", cell.path)))?;

        match loc.bel_type {
            BelType::Carry => {
                let slice = SLICES[number];
                let inits = skalp_lir::carry_chain::ccu2_inits(cell).ok_or_else(|| {
                    emit_err(format!("Carry cell '{}' has no INIT values", cell.path))
                })?;
                config.set_enum(&tile, &format!("SLICE{slice}.MODE"), "CCU2")?;
                config.set_enum(&tile, &format!("SLICE{slice}.CCU2.INJECT"), "NO")?;
                let init0 = fold_tied_low(inits.0 as u64, &tied_low(cell.id, 0, 4));
                let init1 = fold_tied_low(inits.1 as u64, &tied_low(cell.id, 4, 4));
                config.set_word(&tile, &format!("SLICE{slice}.K0.INIT"), init0)?;
                config.set_word(&tile, &format!("SLICE{slice}.K1.INIT"), init1)?;
                for index in 0..8 {
                    if is_tied_high(cell.id, index) {
                        let site = number * 2 + index / 4;
                        let pin = ['A', 'B', 'C', 'D'][index % 4];
                        config.set_pip(&tile, &format!("J{pin}{site}"), "G:VCC")?;
                    }
                }
            }
            t if t.is_ff() => {
                if cell
                    .inputs
                    .first()
                    .is_some_and(|_| tied.value(cell.id, 0).is_some())
                {
                    return Err(emit_err(format!(
                        "Flip-flop '{}' has a constant data input, which is not supported yet",
                        cell.path
                    )));
                }
                let slice = slice_of(number);
                let reg = number % 2;
                let prefix = format!("SLICE{slice}.REG{reg}");
                config.set_enum(&tile, &format!("{prefix}.USED"), "YES")?;
                let powers_up_high = cell
                    .parameters
                    .get(skalp_lir::gate_netlist::FF_INIT_PARAM)
                    .is_some_and(|v| v == "1");
                let regset = if powers_up_high { "SET" } else { "RESET" };
                config.set_enum(&tile, &format!("{prefix}.REGSET"), regset)?;
                config.set_enum(&tile, &format!("{prefix}.LSRMODE"), "LSR")?;
                config.set_enum(&tile, &format!("{prefix}.SEL"), "DF")?;
                config.set_enum(&tile, &format!("SLICE{slice}.CLKMUX"), "CLK")?;
                let set = control_sets
                    .get(&cell.id)
                    .copied()
                    .ok_or_else(|| emit_err(format!("Flip-flop '{}' has no clock", cell.path)))?;
                // Slices A and B share one clock, enable and set/reset
                // line of the tile, and C and D the other
                let pair = if slice <= 'B' { "A and B" } else { "C and D" };
                match pairs_with_ff.get(&(x, y, pair)) {
                    Some((held, other)) if *held != set => {
                        return Err(emit_err(format!(
                            "Flip-flops '{other}' and '{}' share the control lines of \
                             slices {pair} of tile ({x}, {y}) but not their clock, reset \
                             and enable",
                            cell.path
                        )));
                    }
                    Some(_) => {}
                    None => {
                        pairs_with_ff.insert((x, y, pair), (set, cell.path.as_str()));
                    }
                }
                slices_with_ff
                    .entry((x, y, slice))
                    .or_insert((set, cell.path.as_str()));
            }
            BelType::DistRam => {
                if (0..cell.inputs.len()).any(|i| tied.value(cell.id, i).is_some()) {
                    return Err(emit_err(format!(
                        "Distributed RAM '{}' has a constant input, which is not supported yet",
                        cell.path
                    )));
                }
                emit_dist_ram(&mut config, &tile, cell)?;
                ram_write_tiles.insert((x, y));
            }
            BelType::Lut4 => {
                let slice = slice_of(number);
                let lut = number % 2;
                // A truth table covers the connected inputs with the rest at 0,
                // but an unconnected input reads 1 on this silicon.
                let mut low = tied_low(cell.id, 0, 4);
                low.extend(cell.inputs.len()..4);
                let init = fold_tied_low(lut_init(cell)?, &low);
                config.set_word(&tile, &format!("SLICE{slice}.K{lut}.INIT"), init)?;
            }
            BelType::WideLut => {
                // Both LUTs of the slice see inputs 0..3; the slice's wide
                // mux picks LUT1 (the table's upper half) when input 4 is high.
                let slice = SLICES[number];
                let init = lut_init(cell)?;
                let mut halves = [init & 0xFFFF, (init >> 16) & 0xFFFF];
                match tied.value(cell.id, 4) {
                    Some(true) => halves[0] = halves[1],
                    Some(false) => halves[1] = halves[0],
                    None if cell.inputs.len() <= 4 => halves[1] = halves[0],
                    None => {}
                }
                let mut low = tied_low(cell.id, 0, 4);
                low.extend(cell.inputs.len().min(4)..4);
                for (lut, half) in halves.into_iter().enumerate() {
                    let folded = fold_tied_low(half, &low);
                    config.set_word(&tile, &format!("SLICE{slice}.K{lut}.INIT"), folded)?;
                }
            }
            other => {
                return Err(emit_err(format!(
                    "Cell '{}' needs a {other:?} site, which the backend does not support yet",
                    cell.path
                )))
            }
        }
    }

    set_unused_multiplier_defaults(&mut config, &used_multipliers)?;

    for (&(x, y, slice), &(set, path)) in &slices_with_ff {
        if matches!(slice, 'C' | 'D') && ram_write_tiles.contains(&(x, y)) {
            // The RAM's write clock and write enable drive slice C's clock
            // and set/reset inputs, which slice D shares
            return Err(emit_err(format!(
                "Flip-flop '{path}' is in slice {slice} of tile ({x}, {y}), which shares \
                 the write port of a distributed RAM"
            )));
        }
        if set.reset.is_none() {
            // An unconnected set/reset input reads high; this holds it inactive.
            let tile = config.tile(x, y, "PLC")?;
            config.set_enum(&tile, &format!("SLICE{slice}.LSRMUX"), "INV")?;
        }
    }

    Ok(EmittedBits {
        frames: config.bits,
        bus: config.bus_bits,
    })
}

/// A design's configuration: frame bits, and hard-block register bits on
/// the configuration bus as (byte address, bit, value).
struct EmittedBits {
    frames: Vec<GlobalBit>,
    bus: Vec<(u32, u32, bool)>,
}

/// Configure a logic tile's slices A and B as RAM storage and C as its write port.
fn emit_dist_ram(config: &mut ConfigDb<'_>, tile: &PrjoxideTile, cell: &Cell) -> Result<()> {
    let init = cell
        .parameters
        .get("INITVAL")
        .map(|v| {
            u64::from_str_radix(v.trim_start_matches("0x"), 16)
                .map_err(|_| emit_err(format!("Bad INITVAL on '{}': {v}", cell.path)))
        })
        .transpose()?
        .unwrap_or(0);

    for (index, (slice, lut)) in [('A', 0), ('A', 1), ('B', 0), ('B', 1)].iter().enumerate() {
        // INITVAL holds 4-bit words; each LUT stores one bit of every word.
        let contents = (0..16).fold(0u64, |word, entry| {
            word | (((init >> (4 * entry + index)) & 1) << entry)
        });
        config.set_enum(tile, &format!("SLICE{slice}.MODE"), "DPRAM")?;
        config.set_word(tile, &format!("SLICE{slice}.K{lut}.INIT"), contents)?;
    }
    config.set_enum(tile, "SLICEC.MODE", "RAMW")?;
    config.set_enum(tile, "SLICEC.CLKMUX", "CLK")?;
    config.set_enum(tile, "SLICEC.LSRMUX", "LSR")
}

/// Settings of the `PLL_CORE` primitive that are not registers of the
/// block (simulation and generator bookkeeping), ignored when given.
const PLL_PRIMITIVE_ONLY: &[&str] = &[
    "SIM_FLOAT_PRECISION",
    "PMU_WAITFORLOCK",
    "REF_OSC_CTRL",
    "EXTERNAL_DIVIDE_FACTOR",
    "INTFBKDEL_SEL",
    "PHASE_SEL_DEL",
    "PHASE_SEL_DEL_P1",
    "SEL_FBK",
    "LOC",
];

/// Configure a PLL: its tile's feedback and LMMI muxes, the device-wide bits
/// a PLL needs, and its registers on the configuration bus.
fn emit_pll(
    config: &mut ConfigDb<'_>,
    cell: &Cell,
    x: u32,
    y: u32,
    used_wires: &HashSet<crate::device::WireId>,
) -> Result<()> {
    let device = config.device;
    let tile = device
        .prjoxide_tile(x, y, |t| pll::corner_of_tile(t).is_some())
        .cloned()
        .ok_or_else(|| emit_err(format!("PLL '{}' is not on a PLL site", cell.path)))?;
    let corner = pll::corner_of_tile(&tile.tiletype)
        .unwrap_or_default()
        .to_string();
    let value = |name: &str| {
        pll::setting(cell, name)
            .ok_or_else(|| emit_err(format!("PLL '{}' has no setting {name}", cell.path)))
    };

    // The tile: feedback source and LMMI clock/reset inversion, and (on
    // CrossLink-NX) the mode bit
    let db = config.db(&tile.tiletype)?.clone();
    for &(name, default) in pll::TILE_SETTINGS {
        let option = value(name)?;
        let full = format!("PLL_{corner}.{name}");
        if db.enum_option(&full, option).is_some() {
            config.set_enum(&tile, &full, option)?;
        } else if option != default {
            return Err(emit_err(format!(
                "PLL '{}': {name}={option} is not available in tile {}",
                cell.path, tile.tiletype
            )));
        }
    }
    let mode = format!("PLL_{corner}.MODE");
    if db.enum_option(&mode, "PLL_CORE").is_some() {
        config.set_enum(&tile, &mode, "PLL_CORE")?;
    }
    for &(tiletype, frame, bit) in pll::device_enable_bits(config.variant) {
        let owner = device
            .prjoxide_tiles()
            .iter()
            .find(|t| t.tiletype == tiletype)
            .cloned()
            .ok_or_else(|| emit_err(format!("No {tiletype} tile for the PLL enable bits")))?;
        config.bits.push(GlobalBit {
            frame: owner.start_frame + frame,
            bit: owner.start_bit + bit,
            invert: false,
        });
    }

    // The registers: every bit of every setting, so none keeps a reset value
    let region = format!("PLL_{corner}");
    let base =
        super::prjoxide_load::hard_block_base_address(config.db_root, config.variant, &region)
            .ok_or_else(|| {
                emit_err(format!(
                    "The device database has no {region} address in {}/baseaddr.json",
                    config.variant.prjoxide_device()
                ))
            })?;
    let registers = load_iptype(config.db_root, config.family, pll::IPTYPE)
        .map_err(emit_err)?
        .ok_or_else(|| emit_err("No PLL register data in the device database".to_string()))?;
    let mut known: HashSet<String> = HashSet::new();
    let reserved =
        |name: &str| pll::RESERVED_SETTINGS.contains(&name) && !cell.parameters.contains_key(name);
    for name in registers.word_names() {
        known.insert(name.to_string());
        if reserved(name) {
            for bit in registers.word(name).unwrap_or_default().iter().flatten() {
                config
                    .bus_bits
                    .push((base + bit.frame as u32, bit.bit as u32, bit.invert));
            }
            continue;
        }
        let number = pll::parse_number(value(name)?).ok_or_else(|| {
            emit_err(format!(
                "PLL '{}': {name}={} is not a number",
                cell.path,
                value(name).unwrap_or_default()
            ))
        })?;
        let groups = registers.word(name).unwrap_or_default();
        if groups.len() < 64 && number >> groups.len() != 0 {
            return Err(emit_err(format!(
                "PLL '{}': {name}={number} does not fit in {} bits",
                cell.path,
                groups.len()
            )));
        }
        for (index, group) in groups.iter().enumerate() {
            let on = (number >> index) & 1 == 1;
            for bit in group {
                config
                    .bus_bits
                    .push((base + bit.frame as u32, bit.bit as u32, on ^ bit.invert));
            }
        }
    }
    for (name, options) in registers.enums() {
        known.insert(name.to_string());
        if reserved(name) {
            let bits: HashSet<(usize, usize)> = options
                .values()
                .flatten()
                .map(|bit| (bit.frame, bit.bit))
                .collect();
            for (frame, bit) in bits {
                config
                    .bus_bits
                    .push((base + frame as u32, bit as u32, false));
            }
            continue;
        }
        let option = value(name)?;
        let chosen = options.get(option).ok_or_else(|| {
            emit_err(format!(
                "PLL '{}': {name}={option} is not one of {}",
                cell.path,
                options.keys().cloned().collect::<Vec<_>>().join(", ")
            ))
        })?;
        let mut written: HashSet<(usize, usize)> = HashSet::new();
        for bit in chosen {
            written.insert((bit.frame, bit.bit));
            config
                .bus_bits
                .push((base + bit.frame as u32, bit.bit as u32, !bit.invert));
        }
        // A bit only other options use takes the value they do not set
        for bit in options.values().flatten() {
            if written.insert((bit.frame, bit.bit)) {
                config
                    .bus_bits
                    .push((base + bit.frame as u32, bit.bit as u32, bit.invert));
            }
        }
    }
    // prjoxide sets the first register bit of every PLL it configures
    config.bus_bits.push((base, 0, true));

    for name in cell.parameters.keys() {
        let tile_setting = pll::TILE_SETTINGS.iter().any(|(n, _)| n == name);
        let pins = name == skalp_lir::gate_netlist::BLACKBOX_INPUTS_PARAM
            || name == skalp_lir::gate_netlist::BLACKBOX_OUTPUTS_PARAM;
        if !known.contains(name)
            && !tile_setting
            && !pins
            && !PLL_PRIMITIVE_ONLY.contains(&name.as_str())
        {
            return Err(emit_err(format!(
                "PLL '{}' has a setting {name} that this device's PLL does not have",
                cell.path
            )));
        }
    }

    // Unconnected inputs: the enables and power-down read high; a reset left
    // unconnected would read high too, and hold the PLL in reset
    let connected =
        |pin: &str| (0..cell.inputs.len()).any(|i| pll::input_pin_name(cell, i) == Some(pin));
    if value("PLLRESET_ENA")? == "ENABLED" && !connected("PLLRESET") {
        return Err(emit_err(format!(
            "PLL '{}' has PLLRESET_ENA=ENABLED but no RST input; connect RST (0 to run)",
            cell.path
        )));
    }
    for pin in pll::FLOAT_HIGH_PINS {
        if connected(pin) {
            continue;
        }
        let wire = device.bel_pin_wire_by_name(x, y, &format!("PLL_{pin}"));
        if let Some(wire) = wire {
            float_high(config, wire, used_wires)?;
        }
    }
    Ok(())
}

/// A block RAM cell and the site it was placed on.
struct PlacedRam<'a> {
    cell: &'a Cell,
    site_x: u32,
    site_y: u32,
    /// Unique block number, used to address the memory after configuration
    id: u64,
}

/// What a block RAM pin is connected to.
#[derive(Clone, Copy, PartialEq, Eq)]
enum RamPin {
    Signal,
    High,
    Low,
}

/// Set a named option in whichever of a site's tiles define it.
///
/// Block RAM, multiplier and IO sites all spread their settings over tiles.
fn set_ram_enum(
    config: &mut ConfigDb<'_>,
    tiles: &[PrjoxideTile],
    name: &str,
    option: &str,
) -> Result<()> {
    let mut found = false;
    for tile in tiles {
        if config
            .db(&tile.tiletype)?
            .enum_option(name, option)
            .is_some()
        {
            config.set_enum(tile, name, option)?;
            found = true;
        }
    }
    found
        .then_some(())
        .ok_or_else(|| emit_err(format!("No site setting {name}.{option}")))
}

/// Set a word in whichever of a site's tiles defines it.
fn set_ram_word(
    config: &mut ConfigDb<'_>,
    tiles: &[PrjoxideTile],
    name: &str,
    value: u64,
) -> Result<()> {
    let mut found = false;
    for tile in tiles {
        if config.db(&tile.tiletype)?.word(name).is_some() {
            config.set_word(tile, name, value)?;
            found = true;
        }
    }
    found
        .then_some(())
        .ok_or_else(|| emit_err(format!("No site setting {name}")))
}

/// Write a block RAM's initial contents to its window on the configuration
/// bus (chosen by its `WID`, see [`super::prjoxide_load::block_ram_init_address`]).
fn write_block_ram_init(
    config: &mut ConfigDb<'_>,
    cell: &Cell,
    wid: u64,
    width: usize,
    words: &[u64],
) -> Result<()> {
    let image = ebr::init_image(width, words).ok_or_else(|| {
        emit_err(format!(
            "Block RAM '{}' has {} initial words, more than its {width}-bit mode holds",
            cell.path,
            words.len()
        ))
    })?;
    let (base, unconfirmed) =
        super::prjoxide_load::block_ram_init_address(config.db_root, config.variant, wid as u32)
            .map_err(emit_err)?;
    if unconfirmed {
        log::warn!(
            "{}: block RAM contents placed where other Nexus devices keep them; \
             neither prjoxide nor a silicon check has located them on this device",
            config.variant.prjoxide_device()
        );
    }
    let db = load_iptype(config.db_root, config.family, ebr::INIT_IPTYPE)
        .map_err(emit_err)?
        .ok_or_else(|| {
            emit_err("No block RAM initialisation data in the device database".to_string())
        })?;
    for (index, row) in image.iter().enumerate() {
        let name = ebr::init_word_name(index);
        let bits = db
            .word(&name)
            .ok_or_else(|| emit_err(format!("No block RAM setting {name}")))?;
        for (i, group) in bits.iter().enumerate() {
            let on = row.get(i).copied().unwrap_or(false);
            for bit in group {
                config
                    .bus_bits
                    .push((base + bit.frame as u32, bit.bit as u32, on ^ bit.invert));
            }
        }
    }
    Ok(())
}

/// Configure a block RAM site and everything its unrouted pins must read.
fn emit_block_ram(
    config: &mut ConfigDb<'_>,
    ram: &PlacedRam<'_>,
    tied: &crate::bitstream::prepare::TiedInputs,
    used_wires: &HashSet<crate::device::WireId>,
) -> Result<()> {
    let PlacedRam {
        cell,
        site_x: x,
        site_y: y,
        id,
    } = *ram;
    let ports = cell
        .ram_ports()
        .ok_or_else(|| emit_err(format!("'{}' has no block RAM port layout", cell.path)))?;
    let device = config.device;
    let first = device
        .prjoxide_tile(x, y, |t| ebr::site_of_tile(t).is_some())
        .cloned()
        .ok_or_else(|| emit_err(format!("No block RAM tile at column {x}, row {y}")))?;
    let site = ebr::site_of_tile(&first.tiletype).expect("selected by this test");
    let suffix = ebr::second_tile_suffix(site);
    let second = device
        .prjoxide_tile(x + 1, y, |t| t.ends_with(suffix))
        .cloned()
        .ok_or_else(|| emit_err(format!("No second block RAM tile at column {}", x + 1)))?;
    let tiles = [first, second];

    let width = ports.data_width();
    let prefix = format!("EBR{site}");
    let mode = if cell.inputs[ports.read_clk] == cell.inputs[ports.write_clk] {
        "PDPSC16K_MODE"
    } else {
        "PDP16K_MODE"
    };
    set_ram_enum(config, &tiles, &format!("{prefix}.MODE"), mode)?;
    set_ram_enum(config, &tiles, &format!("{prefix}.INIT_DATA"), "STATIC")?;
    set_ram_enum(config, &tiles, &format!("{prefix}.GSR"), "DISABLED")?;
    set_ram_word(config, &tiles, &format!("{prefix}.WID"), id)?;
    if let Some(words) = cell.memory_init() {
        write_block_ram_init(config, cell, id, width, &words)?;
    }
    let in_mode = format!("{prefix}.{mode}");
    set_ram_word(config, &tiles, &format!("{in_mode}.CSDECODE_W"), 0)?;
    set_ram_word(config, &tiles, &format!("{in_mode}.CSDECODE_R"), 0)?;
    for (setting, value) in [
        ("ASYNC_RST_RELEASE", "ASYNC".to_string()),
        ("DATA_WIDTH_W", format!("X{width}")),
        ("DATA_WIDTH_R", format!("X{width}")),
        ("OUTREG", "BYPASSED".to_string()),
        ("RESETMODE", "ASYNC".to_string()),
    ] {
        set_ram_enum(config, &tiles, &format!("{in_mode}.{setting}"), &value)?;
    }

    // Everything the design connects, by site pin.
    let mut pins: HashMap<String, RamPin> = HashMap::new();
    for input in 0..cell.inputs.len() {
        let state = match tied.value(cell.id, input) {
            Some(true) => RamPin::High,
            Some(false) => RamPin::Low,
            None => RamPin::Signal,
        };
        match ebr::input_pin(&ports, input) {
            Some(pin) => {
                pins.insert(pin, state);
            }
            None if ports.write_mask.contains(&input) && state == RamPin::High => {}
            None if input == ports.write_clk_en && state != RamPin::Signal => {}
            None => {
                return Err(emit_err(format!(
                    "Block RAM '{}' drives input {input}, which the backend does not support yet",
                    cell.path
                )))
            }
        }
    }

    // Muxes are named from the true dual-port view in every mode.
    let mux = |pin: &str| format!("{prefix}.DP16K_MODE.{pin}MUX");
    for pin in ["RSTA", "RSTB", "WEB"] {
        set_ram_enum(config, &tiles, &mux(pin), "INV")?;
    }
    for pin in ["CLKA", "CLKB"] {
        if pins.get(pin) != Some(&RamPin::Signal) {
            return Err(emit_err(format!(
                "Block RAM '{}' has no {pin} clock",
                cell.path
            )));
        }
        set_ram_enum(config, &tiles, &mux(pin), pin)?;
    }
    for pin in ebr::input_pins().iter().map(String::as_str) {
        match pins.get(pin).copied() {
            Some(RamPin::Signal) => {
                if ebr::has_inverter(pin) {
                    set_ram_enum(config, &tiles, &mux(pin), pin)?;
                }
            }
            Some(RamPin::Low) if ebr::has_inverter(pin) => {
                set_ram_enum(config, &tiles, &mux(pin), "INV")?;
                float_ram_pin(config, x, y, pin, used_wires)?;
            }
            Some(RamPin::Low) => {
                return Err(emit_err(format!(
                    "Block RAM '{}' pin {pin} is tied low with no driver",
                    cell.path
                )))
            }
            Some(RamPin::High) | None => float_ram_pin(config, x, y, pin, used_wires)?,
        }
    }
    Ok(())
}

/// Let a block RAM pin read high, if it has a CIB mux to set.
fn float_ram_pin(
    config: &mut ConfigDb<'_>,
    x: u32,
    y: u32,
    pin: &str,
    used_wires: &HashSet<crate::device::WireId>,
) -> Result<()> {
    if !ebr::floats_through_cib(pin) {
        return Ok(());
    }
    let device = config.device;
    let wire = device
        .bel_wire(x, y, &format!("RAM_{pin}"))
        .ok_or_else(|| emit_err(format!("No wire for block RAM pin {pin} at ({x}, {y})")))?;
    float_high(config, wire, used_wires)
}

/// Give every multiplier block the design does not use its idle configuration.
fn set_unused_multiplier_defaults(
    config: &mut ConfigDb<'_>,
    used: &HashSet<(u32, u32, usize)>,
) -> Result<()> {
    let mut rows: HashMap<(u32, u32), Vec<PrjoxideTile>> = HashMap::new();
    for tile in config.device.prjoxide_tiles() {
        if let Some(base) = dsp::row_base_column(&tile.tiletype, tile.x) {
            rows.entry((tile.y, base)).or_default().push(tile.clone());
        }
    }
    for ((y, base), tiles) in rows {
        // Both output registers of a pair serve one slot; accumulators are shared.
        for slot in 0..dsp::SLOTS {
            if used.contains(&(y, base, slot)) {
                continue;
            }
            let mut idle = |bel: String, settings: &[(&str, &str)]| -> Result<()> {
                for (name, option) in settings {
                    set_ram_enum(config, &tiles, &format!("{bel}.{name}"), option)?;
                }
                Ok(())
            };
            for half in 0..2 {
                idle(dsp::preadd(slot, half), dsp::UNUSED_PREADD_SETTINGS)?;
                idle(dsp::mult9(slot, half), dsp::UNUSED_MULT9_SETTINGS)?;
                idle(dsp::reg18(slot, half), dsp::UNUSED_REG18_SETTINGS)?;
            }
        }
        for slot in [0, 2] {
            for (name, option) in dsp::ACC54_SETTINGS {
                set_ram_enum(
                    config,
                    &tiles,
                    &format!("{}.{name}", dsp::acc54(slot)),
                    option,
                )?;
            }
        }
    }
    Ok(())
}

/// Configure the hard blocks of one multiplier and hold its unused inputs.
fn emit_multiplier(
    config: &mut ConfigDb<'_>,
    cell: &Cell,
    x: u32,
    y: u32,
    tied: &crate::bitstream::prepare::TiedInputs,
    used_wires: &HashSet<crate::device::WireId>,
) -> Result<()> {
    let device = config.device;
    let first = device
        .prjoxide_tile(x, y, |t| dsp::slot_of_tile(t).is_some())
        .cloned()
        .ok_or_else(|| emit_err(format!("No multiplier tile at column {x}, row {y}")))?;
    let slot = dsp::slot_of_tile(&first.tiletype).expect("selected by this test");
    let base = dsp::row_base_column(&first.tiletype, x)
        .ok_or_else(|| emit_err(format!("Unrecognized multiplier tile {}", first.tiletype)))?;
    let tiles: Vec<PrjoxideTile> = device
        .prjoxide_tiles()
        .iter()
        .filter(|t| t.y == y && dsp::tile_in_row(&t.tiletype, t.x, base))
        .cloned()
        .collect();

    let mut configure = |bel: String, settings: &[(&str, &str)]| -> Result<()> {
        for (name, option) in settings {
            set_ram_enum(config, &tiles, &format!("{bel}.{name}"), option)?;
        }
        Ok(())
    };
    for half in 0..2 {
        configure(dsp::preadd(slot, half), dsp::PREADD_SETTINGS)?;
        configure(dsp::mult9(slot, half), dsp::MULT9_SETTINGS)?;
        configure(dsp::reg18(slot, half), dsp::REG18_SETTINGS)?;
    }
    configure(dsp::mult18(slot), dsp::MULT18_SETTINGS)?;
    configure(dsp::acc54(slot), dsp::ACC54_SETTINGS)?;

    let float = |config: &mut ConfigDb<'_>, pin: &str| -> Result<()> {
        let wire = device
            .bel_wire(x, y, &format!("DSP_{pin}"))
            .ok_or_else(|| emit_err(format!("No wire for multiplier pin {pin} at ({x}, {y})")))?;
        float_high(config, wire, used_wires)
    };
    for input in 0..cell.inputs.len() {
        let Some(pin) = dsp::input_pin(cell, input) else {
            continue;
        };
        match tied.value(cell.id, input) {
            Some(true) => float(config, &pin)?,
            Some(false) => {
                return Err(emit_err(format!(
                    "Multiplier '{}' pin {pin} is tied low with no driver",
                    cell.path
                )))
            }
            None => {}
        }
    }
    for bit in 0..dsp::SHIFT_CONTROL_PINS {
        float(config, &format!("SFTCTRL{bit}"))?;
    }
    Ok(())
}

/// Bank voltage a standard runs at.
fn bank_voltage(standard: &str) -> Result<&'static str> {
    let voltage = standard.trim_matches(|c: char| c.is_alphabetic());
    match voltage {
        "33" => Ok("3V3"),
        "25" => Ok("2V5"),
        "18" => Ok("1V8"),
        "15" => Ok("1V5"),
        "12" => Ok("1V2"),
        "10" => Ok("1V0"),
        _ => Err(emit_err(format!("Unknown IO standard '{standard}'"))),
    }
}

fn emit_pad(
    config: &mut ConfigDb<'_>,
    cell: &Cell,
    x: u32,
    y: u32,
    bel_index: usize,
    used_wires: &HashSet<crate::device::WireId>,
) -> Result<()> {
    if cell.parameters.get("LOC").map(String::as_str) == Some(super::OSC_PIN) {
        return emit_oscillator(config, x, y, used_wires);
    }

    let tile = config
        .device
        .prjoxide_tile(x, y, |t| t.starts_with("SYSIO_B"))
        .cloned()
        .ok_or_else(|| emit_err(format!("Pad '{}' has no IO tile", cell.path)))?;
    let bank = tile
        .tiletype
        .strip_prefix("SYSIO_B")
        .and_then(|rest| rest.chars().next())
        .ok_or_else(|| emit_err(format!("Unrecognized IO tile {}", tile.tiletype)))?;
    let side = ['A', 'B'][bel_index];
    let is_output = cell.function == Some(CellFunction::OutputPad);
    let high_performance = super::sites::is_high_performance_io(&tile.tiletype);
    let standard = cell
        .parameters
        .get("IO_STANDARD")
        .map(String::as_str)
        .unwrap_or(if high_performance {
            "LVCMOS18H"
        } else {
            "LVCMOS33"
        });
    let direction = if is_output { "OUTPUT" } else { "INPUT" };
    let voltage = bank_voltage(standard)?;
    // In a high-performance bank the B pad's settings sit in the next tile.
    let (settings_tile, prefix) = if high_performance {
        let column = if side == 'A' { x } else { x + 1 };
        let tile = config
            .device
            .prjoxide_tile(column, y, |t| t.starts_with("SYSIO_B"))
            .cloned()
            .ok_or_else(|| emit_err(format!("Pad '{}' has no settings tile", cell.path)))?;
        (tile, format!("PIO{side}.SEIO18"))
    } else {
        (tile, format!("PIO{side}"))
    };
    // A corner site (`_C`) keeps the rest of its settings in the tile below.
    let mut settings_tiles = vec![settings_tile.clone()];
    if settings_tile.tiletype.ends_with("_C") {
        let rest = settings_tile.tiletype.replace("_C", "_REM");
        let below = config
            .device
            .prjoxide_tile(x, y + 1, |t| t == rest)
            .cloned()
            .ok_or_else(|| emit_err(format!("Pad '{}' has no {rest} tile", cell.path)))?;
        settings_tiles.push(below);
    }
    let tiles = settings_tiles.as_slice();
    set_ram_enum(
        config,
        tiles,
        &format!("{prefix}.BASE_TYPE"),
        &format!("{direction}_{standard}"),
    )
    .map_err(|_| {
        let bank = if high_performance {
            "a high-performance (1.8 V at most) I/O"
        } else {
            "a wide-range I/O"
        };
        emit_err(format!(
            "Pad '{}' is on {bank}, which has no {} {standard}",
            cell.path,
            direction.to_lowercase()
        ))
    })?;
    set_ram_enum(config, tiles, &format!("{prefix}.PULLMODE"), "NONE")?;
    if !high_performance {
        set_ram_enum(config, tiles, &format!("{prefix}.GLITCHFILTER"), "OFF")?;
    }
    if is_output {
        set_ram_enum(config, tiles, &format!("{prefix}.DRIVE_{voltage}"), "8")?;
        set_ram_enum(config, tiles, &format!("{prefix}.SLEWRATE"), "MED")?;
        set_ram_enum(config, tiles, &format!("PIO{side}.TMUX"), "INV")?;
    }

    // High-performance banks have no VCC setting to make.
    if !high_performance {
        let bank_ref = format!("BANKREF{bank}");
        let bank_tile = config
            .device
            .prjoxide_tiles()
            .iter()
            .find(|t| t.tiletype.starts_with(&bank_ref))
            .cloned()
            .ok_or_else(|| emit_err(format!("No {bank_ref} tile on this device")))?;
        config.set_enum(&bank_tile, &format!("BANK{bank}.VCC"), voltage)?;
    }
    Ok(())
}

fn emit_oscillator(
    config: &mut ConfigDb<'_>,
    x: u32,
    y: u32,
    used_wires: &HashSet<crate::device::WireId>,
) -> Result<()> {
    let tile = config.tile(x, y, "EFB_1_OSC")?;
    config.set_enum(&tile, "OSC_CORE.HF_OSC_EN", "ENABLED")?;
    config.set_enum(&tile, "OSC_CORE.HFDIV_FABRIC_EN", "ENABLED")?;
    config.set_enum(&tile, "OSC_CORE.DTR_EN", "ENABLED")?;
    config.set_enum(&tile, "OSC_CORE.DEBUG_N", "DISABLED")?;
    config.set_word(&tile, "OSC_CORE.HF_CLK_DIV", OSC_HF_CLK_DIV)?;
    config.set_word(&tile, "OSC_CORE.HF_SED_SEC_DIV", OSC_SED_SEC_DIV)?;

    // HFOUTEN must read high: selecting an input on its CIB mux holds it so.
    let device = config.device;
    let enable = resolve_wire(x, y, "S1:JHFOUTEN_OSC_CORE")
        .and_then(|n| device.wire_by_name(&node_name(&n)))
        .ok_or_else(|| emit_err("No oscillator enable wire".to_string()))?;
    float_high(config, enable, used_wires)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn folding_reads_tied_low_inputs_as_zero() {
        // F = A | C with C tied low must become F = A, even where C reads 1.
        let a_or_c = 0xFAFAu64;
        assert_eq!(fold_tied_low(a_or_c, &[2]), 0xAAAA);
    }

    #[test]
    fn folding_keeps_the_carry_generate_term() {
        // The low byte also feeds the carry logic through inputs A, B and C.
        // With C tied low and floating high, entries 4..=7 must repeat 0..=3.
        let folded = fold_tied_low(0x555A, &[2]);
        assert_eq!(folded & 0xFF, 0xAA);
    }

    #[test]
    fn folding_nothing_changes_nothing() {
        assert_eq!(fold_tied_low(0x1234, &[]), 0x1234);
    }

    #[test]
    fn slices_pair_up_luts() {
        assert_eq!(slice_of(0), 'A');
        assert_eq!(slice_of(1), 'A');
        assert_eq!(slice_of(7), 'D');
    }
}
