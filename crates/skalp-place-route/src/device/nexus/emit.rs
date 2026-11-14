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

/// Turn an output pad's driver on: hold its tristate input high (the pad's
/// TMUX inverts it, so the driver sees enable).
///
/// The tristate input comes from a CIB mux, as data does. Left without a
/// selection, that mux's choice is not documented, and on some pads it
/// follows the pad's data, so the pin would float whenever the data is high.
/// Vendor tools select one of the mux's CIB inputs that nothing drives: an
/// undriven CIB input reads its default constant, high. Do the same,
/// preferring the D inputs as they do, and never the mux's default input.
fn drive_output_enable(
    config: &mut ConfigDb<'_>,
    enable: crate::device::WireId,
    used_wires: &HashSet<crate::device::WireId>,
) -> Result<()> {
    let device = config.device;
    let mux = find_cib_mux(device, enable)
        .ok_or_else(|| emit_err("no CIB mux feeds its output enable".to_string()))?;
    let mux_name = device
        .wire(mux)
        .map_or("?", |w| w.name.as_str())
        .to_string();
    if used_wires.contains(&mux) {
        return Err(emit_err(format!(
            "its output enable mux {mux_name} carries a routed net"
        )));
    }
    let wire_name =
        |w: crate::device::WireId| device.wire(w).map_or(String::new(), |w| w.name.clone());
    // A CIB input is free when neither it nor anything that drives it
    // carries a net: then it reads its default, high.
    let free = |input: crate::device::WireId| {
        !used_wires.contains(&input)
            && device.wire_pips(input).into_iter().all(|p| {
                device
                    .pip(p)
                    .is_some_and(|pip| !used_wires.contains(&pip.src_wire))
            })
    };
    let candidates: Vec<(crate::device::PipId, crate::device::WireId)> = device
        .wire_pips(mux)
        .into_iter()
        .filter_map(|p| device.pip(p).map(|pip| (p, pip.src_wire)))
        .filter(|&(_, src)| wire_name(src).contains("CIBMUXIN") && free(src))
        .collect();
    // D first, then C and B: each is an explicit selection. A is the
    // mux's zero-bit default, the choice not to rely on.
    let (pip, _) = ["CIBMUXIND", "CIBMUXINC", "CIBMUXINB", "CIBMUXINA"]
        .iter()
        .find_map(|input| {
            candidates
                .iter()
                .find(|&&(_, src)| wire_name(src).contains(input))
        })
        .copied()
        .ok_or_else(|| {
            emit_err(format!(
                "no free CIB input on its output enable mux {mux_name}"
            ))
        })?;
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
    let mut banks = BankUse::default();

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
            emit_pad(
                &mut config,
                cell,
                x,
                y,
                loc.bel_index,
                &used_wires,
                &mut banks,
            )?;
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
                // Data from the LUT beside the register when its route took
                // the in-slice path (`DL`), else from routing via the bypass
                // pin (`DF`)
                let from_lut = device
                    .bel_pin_wire_by_name(x, y, &format!("FF{number}_DI"))
                    .is_some_and(|wire| used_wires.contains(&wire));
                let select = if from_lut { "DL" } else { "DF" };
                config.set_enum(&tile, &format!("{prefix}.SEL"), select)?;
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
    emit_vref_drivers(&mut config, netlist, placement, &banks.vref)?;
    release_unused_pads(&mut config, netlist, placement, &banks.vref)?;
    release_unused_banks(&mut config, &banks.vcc)?;

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
    // A pin the device data gives no connection to the fabric (LFCPNX's
    // dynamic width selects, DWS0-4, which nothing drives) has no CIB mux
    // to set: the block holds it at its own default.
    let Some(wire) = device.bel_wire(x, y, &format!("RAM_{pin}")) else {
        log::debug!("block RAM pin {pin} at ({x}, {y}) has no fabric connection");
        return Ok(());
    };
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

/// Pad bias from the port's `pull` constraint (none when it has none).
fn pull_mode(cell: &Cell) -> Result<&'static str> {
    match cell.parameters.get("TERMINATION").map(String::as_str) {
        None | Some("None") => Ok("NONE"),
        Some("PullUp") => Ok("UP"),
        Some("PullDown") => Ok("DOWN"),
        Some("Keeper") => Ok("KEEPER"),
        Some(other) => Err(emit_err(format!(
            "Pad '{}' asks for pull '{other}', which Nexus pads do not have",
            cell.path
        ))),
    }
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

/// The input reference level, in mV, of a standard that compares against
/// its bank's reference voltage; `None` for one that does not.
pub(crate) fn vref_level(standard: &str) -> Option<u32> {
    match standard {
        "SSTL15_I" | "SSTL15_II" | "HSTL15_I" => Some(750),
        "SSTL135_I" | "SSTL135_II" => Some(675),
        "HSUL12" => Some(600),
        _ => None,
    }
}

/// Base type of a bank's reference voltage pad: Radiant's bits for it,
/// which are the same whatever standard the bank's inputs use.
const VREF_DRIVER_BASE_TYPE: &str = "INPUT_SSTL15_II";

/// A bank whose inputs compare against its first reference voltage.
#[derive(Debug, Clone)]
struct VrefBank {
    standard: String,
    level: u32,
    /// A pad that uses it, for messages
    user: String,
}

/// What the design's pads have set in each bank.
#[derive(Debug, Default)]
struct BankUse {
    /// Banks whose inputs compare against their first reference voltage
    vref: HashMap<u32, VrefBank>,
    /// Banks whose voltage a pad has set
    vcc: HashMap<u32, BankVcc>,
}

/// The voltage a bank's pads have set it to.
#[derive(Debug, Clone)]
struct BankVcc {
    voltage: &'static str,
    standard: String,
    /// A pad that uses it, for messages
    user: String,
}

/// Where a pad's settings are: the tiles holding them, the prefix of their
/// names, the pad's bank, and whether the bank is high-performance.
struct PadSettings {
    tiles: Vec<PrjoxideTile>,
    prefix: String,
    bank: u32,
    high_performance: bool,
}

fn pad_settings(
    config: &ConfigDb<'_>,
    x: u32,
    y: u32,
    side: char,
    path: &str,
) -> Result<PadSettings> {
    let tile = config
        .device
        .prjoxide_tile(x, y, |t| t.starts_with("SYSIO_B"))
        .cloned()
        .ok_or_else(|| emit_err(format!("Pad '{path}' has no IO tile")))?;
    let bank = tile
        .tiletype
        .strip_prefix("SYSIO_B")
        .and_then(|rest| rest.chars().next())
        .and_then(|c| c.to_digit(10))
        .ok_or_else(|| emit_err(format!("Unrecognized IO tile {}", tile.tiletype)))?;
    let high_performance = super::sites::is_high_performance_io(&tile.tiletype);
    // In a high-performance bank the B pad's settings sit in the next tile.
    let (settings_tile, prefix) = if high_performance {
        let column = if side == 'A' { x } else { x + 1 };
        let tile = config
            .device
            .prjoxide_tile(column, y, |t| t.starts_with("SYSIO_B"))
            .cloned()
            .ok_or_else(|| emit_err(format!("Pad '{path}' has no settings tile")))?;
        (tile, format!("PIO{side}.SEIO18"))
    } else {
        (tile, format!("PIO{side}"))
    };
    // A corner site (`_C`) keeps the rest of its settings in the tile below.
    let mut tiles = vec![settings_tile.clone()];
    if settings_tile.tiletype.ends_with("_C") {
        let rest = settings_tile.tiletype.replace("_C", "_REM");
        let below = config
            .device
            .prjoxide_tile(x, y + 1, |t| t == rest)
            .cloned()
            .ok_or_else(|| emit_err(format!("Pad '{path}' has no {rest} tile")))?;
        tiles.push(below);
    }
    Ok(PadSettings {
        tiles,
        prefix,
        bank,
        high_performance,
    })
}

fn emit_pad(
    config: &mut ConfigDb<'_>,
    cell: &Cell,
    x: u32,
    y: u32,
    bel_index: usize,
    used_wires: &HashSet<crate::device::WireId>,
    banks: &mut BankUse,
) -> Result<()> {
    if cell.parameters.get("LOC").map(String::as_str) == Some(super::OSC_PIN) {
        return emit_oscillator(config, x, y, used_wires);
    }

    let side = ['A', 'B'][bel_index];
    let PadSettings {
        tiles: settings_tiles,
        prefix,
        bank,
        high_performance,
    } = pad_settings(config, x, y, side, &cell.path)?;
    let is_output = cell.function == Some(CellFunction::OutputPad);
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
    let vref = vref_level(standard);
    if vref.is_some() {
        if is_output {
            return Err(emit_err(format!(
                "Pad '{}' is an {standard} output; only {standard} inputs are supported so far",
                cell.path
            )));
        }
        if pull_mode(cell)? != "NONE" {
            return Err(emit_err(format!(
                "Pad '{}' is an {standard} input, which compares against its bank's \
                 reference voltage and cannot have a pull",
                cell.path
            )));
        }
        if !high_performance {
            return Err(emit_err(format!(
                "Pad '{}' is on a wide-range I/O, which has no {standard}; use a \
                 high-performance bank",
                cell.path
            )));
        }
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
    set_ram_enum(
        config,
        tiles,
        &format!("{prefix}.PULLMODE"),
        pull_mode(cell)?,
    )?;
    if let Some(level) = vref {
        set_ram_enum(config, tiles, &format!("{prefix}.VREF"), "VREF1_LOAD")?;
        enable_vref_receiver(config, x, y, side, &cell.path)?;
        let user = VrefBank {
            standard: standard.to_string(),
            level,
            user: cell.path.clone(),
        };
        let first = banks.vref.entry(bank).or_insert_with(|| user.clone());
        if first.level != level {
            return Err(emit_err(format!(
                "Pads '{}' ({}) and '{}' ({standard}) need different reference \
                 voltages, but bank {bank} has one",
                first.user, first.standard, cell.path
            )));
        }
        return Ok(());
    }

    let voltage = bank_voltage(standard)?;
    if !high_performance {
        set_ram_enum(config, tiles, &format!("{prefix}.GLITCHFILTER"), "OFF")?;
    }
    if is_output {
        set_ram_enum(config, tiles, &format!("{prefix}.DRIVE_{voltage}"), "8")?;
        set_ram_enum(config, tiles, &format!("{prefix}.SLEWRATE"), "MED")?;
        set_ram_enum(config, tiles, &format!("PIO{side}.TMUX"), "INV")?;
        let enable = config
            .device
            .bel_wire(x, y, &format!("PIO{bel_index}_T"))
            .ok_or_else(|| emit_err(format!("Pad '{}' has no output enable wire", cell.path)))?;
        drive_output_enable(config, enable, used_wires)
            .map_err(|e| emit_err(format!("Pad '{}': {e}", cell.path)))?;
    }

    // High-performance banks have no VCC setting to make.
    if !high_performance {
        let user = BankVcc {
            voltage,
            standard: standard.to_string(),
            user: cell.path.clone(),
        };
        if let Some(first) = banks.vcc.get(&bank) {
            if first.voltage != voltage {
                return Err(emit_err(format!(
                    "Pads '{}' ({}) and '{}' ({standard}) need different bank \
                     voltages, but bank {bank} has one",
                    first.user, first.standard, cell.path
                )));
            }
            return Ok(());
        }
        let bank_tile = bank_reference_tile(config.device, bank)
            .ok_or_else(|| emit_err(format!("No BANKREF{bank} tile on this device")))?;
        config.set_enum(&bank_tile, &format!("BANK{bank}.VCC"), voltage)?;
        banks.vcc.insert(bank, user);
    }
    Ok(())
}

/// The bank a bank reference tile (`BANKREF<n>`, with any suffix such as
/// `BANKREF3_DDR42`) belongs to.
fn bank_of_reference_tile(tiletype: &str) -> Option<u32> {
    let rest = tiletype.strip_prefix("BANKREF")?;
    let digits = rest.len() - rest.trim_start_matches(|c: char| c.is_ascii_digit()).len();
    if digits == 0 || !(rest.len() == digits || rest[digits..].starts_with('_')) {
        return None;
    }
    rest[..digits].parse().ok()
}

/// A bank's reference tile, which holds its settings such as its voltage.
fn bank_reference_tile(device: &NexusDevice, bank: u32) -> Option<PrjoxideTile> {
    device
        .prjoxide_tiles()
        .iter()
        .find(|t| bank_of_reference_tile(&t.tiletype) == Some(bank))
        .cloned()
}

/// Set every bank no pad uses to `VCC = NONE`, as Radiant does, where the
/// device's data has that setting (banks with a selectable voltage).
fn release_unused_banks(
    config: &mut ConfigDb<'_>,
    vcc_banks: &HashMap<u32, BankVcc>,
) -> Result<()> {
    let mut tiles: Vec<(u32, PrjoxideTile)> = config
        .device
        .prjoxide_tiles()
        .iter()
        .filter_map(|t| Some((bank_of_reference_tile(&t.tiletype)?, t.clone())))
        .filter(|(bank, _)| !vcc_banks.contains_key(bank))
        .collect();
    tiles.sort_by_key(|(bank, _)| *bank);
    tiles.dedup_by_key(|(bank, _)| *bank);
    for (bank, tile) in tiles {
        let name = format!("BANK{bank}.VCC");
        let has_none = config
            .db(&tile.tiletype)?
            .enum_option(&name, "NONE")
            .is_some();
        if has_none {
            config.set_enum(&tile, &name, "NONE")?;
        }
    }
    Ok(())
}

/// Set every bonded pad the design leaves unused to `BASE_TYPE = NONE`, as
/// Radiant does: in most tiles that is a bit of its own, not the all-zero
/// state. Pads whose tile has no such setting are left as they are.
fn release_unused_pads(
    config: &mut ConfigDb<'_>,
    netlist: &skalp_lir::gate_netlist::GateNetlist,
    placement: &crate::placer::PlacementResult,
    vref_banks: &HashMap<u32, VrefBank>,
) -> Result<()> {
    let device = config.device;
    let packages = device.packages();
    let package = device
        .default_package()
        .and_then(|name| packages.get(name))
        .or_else(|| {
            let first = packages.keys().min()?;
            packages.get(first)
        });
    let Some(package) = package else {
        return Ok(());
    };
    let mut used: HashSet<(u32, u32, usize)> = netlist
        .cells
        .iter()
        .filter(|c| is_pad(c))
        .filter_map(|c| placement.placements.get(&c.id))
        .map(|loc| (loc.tile_x, loc.tile_y, loc.bel_index))
        .collect();
    for bank in vref_banks.keys() {
        if let Some(driver) = device.vref_driver(*bank) {
            let (x, y, pio) = driver.site;
            used.insert((x, y, usize::from(pio)));
        }
    }
    let mut unused: Vec<(&String, (u32, u32, u8))> = package
        .pins
        .iter()
        .filter(|(pin, _)| pin.as_str() != super::OSC_PIN)
        .map(|(pin, &site)| (pin, site))
        .filter(|(_, (x, y, pio))| !used.contains(&(*x, *y, usize::from(*pio))))
        .collect();
    unused.sort_by_key(|&(pin, site)| (site, pin.clone()));
    unused.dedup_by_key(|(_, site)| *site);
    for (pin, (x, y, pio)) in unused {
        if device
            .prjoxide_tile(x, y, |t| t.starts_with("SYSIO_B"))
            .is_none()
        {
            continue;
        }
        let side = ['A', 'B'][usize::from(pio)];
        let settings = pad_settings(config, x, y, side, pin)?;
        let name = format!("{}.BASE_TYPE", settings.prefix);
        let mut defined = false;
        for tile in &settings.tiles {
            defined |= config
                .db(&tile.tiletype)?
                .enum_option(&name, "NONE")
                .is_some();
        }
        if defined {
            set_ram_enum(config, &settings.tiles, &name, "NONE")?;
        }
    }
    Ok(())
}

/// Turn on the comparator a reference-voltage input reads through: the
/// pad pair's differential receiver.
///
/// Pad A's sits in the pair's first tile and pad B's in its second, both
/// under the name `PIOA.DIFFIO18.BASE_TYPE`; the receiver settings Radiant
/// writes decode as a SUBLVDS input in the first and an SSTL15D_II input in
/// the second.
fn enable_vref_receiver(
    config: &mut ConfigDb<'_>,
    x: u32,
    y: u32,
    side: char,
    path: &str,
) -> Result<()> {
    let (column, option) = match side {
        'A' => (x, "INPUT_SUBLVDS"),
        _ => (x + 1, "INPUT_SSTL15D_II"),
    };
    let tile = config
        .device
        .prjoxide_tile(column, y, |t| t.starts_with("SYSIO_B"))
        .cloned()
        .ok_or_else(|| emit_err(format!("Pad '{path}' has no receiver tile")))?;
    set_ram_enum(
        config,
        std::slice::from_ref(&tile),
        "PIOA.DIFFIO18.BASE_TYPE",
        option,
    )
}

/// Give each bank whose inputs use a reference voltage the pad that drives
/// it. The pad is the bank's, so no design signal may sit on it.
fn emit_vref_drivers(
    config: &mut ConfigDb<'_>,
    netlist: &skalp_lir::gate_netlist::GateNetlist,
    placement: &crate::placer::PlacementResult,
    vref_banks: &HashMap<u32, VrefBank>,
) -> Result<()> {
    let mut banks: Vec<_> = vref_banks.iter().collect();
    banks.sort_by_key(|(bank, _)| **bank);
    for (&bank, vref) in banks {
        let driver = config.device.vref_driver(bank).cloned().ok_or_else(|| {
            emit_err(format!(
                "Pad '{}' ({}) needs bank {bank}'s reference voltage, which has no \
                 pad on this device",
                vref.user, vref.standard
            ))
        })?;
        let (x, y, pio) = driver.site;
        if let Some(cell) = netlist.cells.iter().find(|c| {
            placement.placements.get(&c.id).is_some_and(|loc| {
                (loc.tile_x, loc.tile_y, loc.bel_index) == (x, y, usize::from(pio)) && is_pad(c)
            })
        }) {
            return Err(emit_err(format!(
                "Pad '{}' is on {}, which drives bank {bank}'s reference voltage for \
                 '{}' ({}); move it to another pin",
                cell.path, driver.pad, vref.user, vref.standard
            )));
        }
        if pio != 0 {
            return Err(emit_err(format!(
                "Bank {bank}'s reference voltage pad {} is a B pad, which is not supported yet",
                driver.pad
            )));
        }
        // The driver's settings are the same whatever standard its loads use
        let settings = pad_settings(config, x, y, 'A', &driver.pad)?;
        let tiles = settings.tiles.as_slice();
        let prefix = &settings.prefix;
        set_ram_enum(
            config,
            tiles,
            &format!("{prefix}.BASE_TYPE"),
            VREF_DRIVER_BASE_TYPE,
        )?;
        set_ram_enum(config, tiles, &format!("{prefix}.PULLMODE"), "NONE")?;
        // Drive the bank's VREF1 rail from this pad (prjoxide models this as
        // its own setting, apart from the VREF load selection, as on LIFCL)
        set_ram_enum(config, tiles, &format!("{prefix}.VREF1_DRIVER"), "ON")?;
        // The bank's VREF1 generator, enabled from the pair's second tile by
        // the setting prjoxide names `PIOA.SEIO18.VREF.VREF1_LOAD` there
        let generator = config
            .device
            .prjoxide_tile(x + 1, y, |t| t.starts_with("SYSIO_B"))
            .cloned()
            .ok_or_else(|| {
                emit_err(format!(
                    "Bank {bank}'s reference voltage pad {} has no partner tile",
                    driver.pad
                ))
            })?;
        set_ram_enum(
            config,
            std::slice::from_ref(&generator),
            "PIOA.SEIO18.VREF",
            "VREF1_LOAD",
        )?;
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
    fn bank_reference_tiles_name_their_bank() {
        assert_eq!(bank_of_reference_tile("BANKREF0"), Some(0));
        assert_eq!(bank_of_reference_tile("BANKREF3_DDR42"), Some(3));
        assert_eq!(bank_of_reference_tile("BANKREF7_RBB_2"), Some(7));
        assert_eq!(bank_of_reference_tile("BANKREF12"), Some(12));
        assert_eq!(bank_of_reference_tile("BANKREF"), None);
        assert_eq!(bank_of_reference_tile("BANKREFX"), None);
        assert_eq!(bank_of_reference_tile("BANKREF4X"), None);
        assert_eq!(bank_of_reference_tile("SYSIO_B0_0"), None);
    }

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

    fn pad_with_termination(termination: Option<&str>) -> Cell {
        let mut cell = Cell::new_comb(
            skalp_lir::gate_netlist::CellId(0),
            "OXIDE_IB".to_string(),
            "nexus".to_string(),
            0.0,
            "top.pin_ibuf".to_string(),
            vec![],
            vec![],
        );
        if let Some(t) = termination {
            cell.parameters.insert("TERMINATION".into(), t.into());
        }
        cell
    }

    #[test]
    fn a_pull_constraint_sets_the_pad_bias() {
        let mode = |t| pull_mode(&pad_with_termination(t)).unwrap();
        assert_eq!(mode(None), "NONE");
        assert_eq!(mode(Some("None")), "NONE");
        assert_eq!(mode(Some("PullUp")), "UP");
        assert_eq!(mode(Some("PullDown")), "DOWN");
        assert_eq!(mode(Some("Keeper")), "KEEPER");
        assert!(pull_mode(&pad_with_termination(Some("Bus"))).is_err());
    }

    #[test]
    fn slices_pair_up_luts() {
        assert_eq!(slice_of(0), 'A');
        assert_eq!(slice_of(1), 'A');
        assert_eq!(slice_of(7), 'D');
    }
}

#[cfg(all(test, feature = "nexus-bitstream"))]
mod device_tests {
    use super::find_cib_mux;
    use crate::device::nexus::prjoxide_load::{find_database, load_tilegrid};
    use crate::device::nexus::{NexusDevice, NexusVariant};
    use crate::device::Device;

    /// Every output pad of the CertusPro-NX, in every bank, can have its
    /// driver turned on: its tristate input comes from a CIB mux with an
    /// explicit (configured) input to select, as `drive_output_enable` needs.
    #[test]
    fn every_certuspro_pad_can_enable_its_driver() {
        let Some(db) = find_database() else { return };
        let variant = NexusVariant::Lfcpnx100;
        let tiles = load_tilegrid(&db, variant).unwrap();
        let width = tiles.iter().map(|t| t.x).max().unwrap();
        let height = tiles.iter().map(|t| t.y).max().unwrap();
        // One strip per device edge, deep enough for the CIB beside each pad
        let strips = [
            (0, 0, width, 3),
            (0, height - 3, width, height),
            (0, 0, 3, height),
            (width - 3, 0, width, height),
        ];
        let mut checked = 0;
        for bbox in strips {
            let device = NexusDevice::from_prjoxide_bbox(variant, &db, bbox).unwrap();
            let name = |w| device.wire(w).map(|x| x.name.clone()).unwrap_or_default();
            for tile in tiles.iter().filter(|t| {
                t.tiletype.starts_with("SYSIO_B")
                    && t.x >= bbox.0
                    && t.x <= bbox.2
                    && t.y >= bbox.1
                    && t.y <= bbox.3
            }) {
                for pad in 0..2 {
                    if device
                        .bel_wire(tile.x, tile.y, &format!("PIO{pad}_O"))
                        .is_none()
                    {
                        continue;
                    }
                    let at = format!("{} pad {pad}", tile.tiletype);
                    let enable = device
                        .bel_wire(tile.x, tile.y, &format!("PIO{pad}_T"))
                        .unwrap_or_else(|| panic!("{at}: no output enable wire"));
                    let mux = find_cib_mux(&device, enable)
                        .unwrap_or_else(|| panic!("{at}: no CIB mux feeds the enable"));
                    let explicit = device.wire_pips(mux).into_iter().any(|p| {
                        let pip = device.pip(p).unwrap();
                        name(pip.src_wire).contains("CIBMUXIN")
                            && !device.pip_config_bits(p).is_empty()
                    });
                    assert!(explicit, "{at}: {} has no configured input", name(mux));
                    checked += 1;
                }
            }
        }
        assert!(checked > 300, "only {checked} pads checked");
    }
}
