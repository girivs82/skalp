//! Turn a placed and routed design into Nexus configuration bits.
//!
//! The router connects wires; this module sets everything else: LUT truth
//! tables, flip-flop modes, carry mode, IO pads, the oscillator, and the
//! dedicated connections the router does not handle (carry chains, constants).
//!
//! The settings mirror what nextpnr writes for the same cells.

use super::prjoxide_graph::{load_tiletype, ConfigBit, GlobalBit, TileBitsDb};
use super::{NexusDevice, NexusVariant, PrjoxideTile};
use crate::device::{BelType, Device, FF_PIN_CE, FF_PIN_CLK, FF_PIN_LSR};
use crate::error::{PlaceRouteError, Result};
use crate::placer::PlacementResult;
use crate::router::RoutingResult;
use skalp_lir::gate_netlist::{Cell, CellId, GateNet, GateNetId, GateNetlist};
use skalp_lir::tech_library::CellFunction;
use std::collections::{HashMap, HashSet};
use std::path::Path;

/// Slices per logic tile, in LUT/FF numbering order.
const SLICES: [char; 4] = ['A', 'B', 'C', 'D'];

/// Oscillator divider: 450 MHz / (24 + 1) = 18 MHz.
const OSC_HF_CLK_DIV: u64 = 24;
/// Divider for the configuration engine's clock.
const OSC_SED_SEC_DIV: u64 = 1;

/// Constant inputs found while preparing a netlist for routing.
#[derive(Debug, Default)]
pub struct TiedInputs {
    /// (cell, input index) → the constant it reads
    values: HashMap<(CellId, usize), bool>,
}

fn emit_err(message: String) -> PlaceRouteError {
    PlaceRouteError::BitstreamFailed(message)
}

fn constant_value(cell: &Cell) -> Option<bool> {
    match cell.function {
        Some(CellFunction::TieHigh) => Some(true),
        Some(CellFunction::TieLow) => Some(false),
        _ if crate::placer::is_constant_cell(&cell.cell_type) => {
            Some(cell.cell_type.contains('1') || cell.cell_type.contains("VCC"))
        }
        _ => None,
    }
}

fn is_carry(cell: &Cell) -> bool {
    cell.function == Some(CellFunction::Ccu2)
}

fn is_pad(cell: &Cell) -> bool {
    matches!(
        cell.function,
        Some(CellFunction::InputPad)
            | Some(CellFunction::OutputPad)
            | Some(CellFunction::BidirPad)
            | Some(CellFunction::ClockPad)
    )
}

fn is_flip_flop(cell: &Cell) -> bool {
    cell.clock.is_some()
}

/// Router pin number of a flip-flop's `index`-th input.
fn ff_pin(cell: &Cell, index: usize) -> usize {
    match (cell.cell_type.as_str(), index) {
        ("OXIDE_FF_EN", 1) | ("OXIDE_FF_RSTE", 2) => FF_PIN_CE,
        ("OXIDE_FF_RST", 1) | ("OXIDE_FF_RSTE", 1) => FF_PIN_LSR,
        _ => index,
    }
}

/// Which net drives each constant cell's output.
fn constant_nets(netlist: &GateNetlist) -> HashMap<GateNetId, bool> {
    netlist
        .cells
        .iter()
        .filter_map(|cell| {
            let value = constant_value(cell)?;
            Some((*cell.outputs.first()?, value))
        })
        .collect()
}

/// Rewrite a netlist's connectivity into what the router should route.
///
/// Rebuilds every net's fanout from the cells, because clock pins are absent
/// from it. Leaves out connections with no routing: constants (set with
/// [`emit_bits`]) and the dedicated carry path from one `CCU2` to the next.
pub fn prepare_for_routing(netlist: &GateNetlist) -> (GateNetlist, TiedInputs) {
    let constants = constant_nets(netlist);
    let mut prepared = netlist.clone();
    let mut tied = TiedInputs::default();
    for net in &mut prepared.nets {
        net.fanout.clear();
        // Routed as ordinary signals: LFCPNX clock tree data is incomplete.
        net.is_clock = false;
        net.is_reset = false;
    }

    let connect = |nets: &mut Vec<GateNet>, net: GateNetId, cell: CellId, pin: usize| {
        nets[net.0 as usize].fanout.push((cell, pin));
    };
    for cell in &netlist.cells {
        if constant_value(cell).is_some() {
            continue;
        }
        for (index, &net) in cell.inputs.iter().enumerate() {
            if let Some(&value) = constants.get(&net) {
                tied.values.insert((cell.id, index), value);
                continue;
            }
            if is_carry(cell) && index == 8 {
                continue;
            }
            let pin = if is_flip_flop(cell) {
                ff_pin(cell, index)
            } else {
                index
            };
            connect(&mut prepared.nets, net, cell.id, pin);
        }
        if let Some(clock) = cell.clock {
            connect(&mut prepared.nets, clock, cell.id, FF_PIN_CLK);
        }
        if let Some(reset) = cell.reset {
            connect(&mut prepared.nets, reset, cell.id, FF_PIN_LSR);
        }
    }
    (prepared, tied)
}

/// Copy a netlist for the placer, hiding pads so only logic gets placed.
pub fn logic_only_netlist(netlist: &GateNetlist) -> GateNetlist {
    let mut logic = netlist.clone();
    for cell in &mut logic.cells {
        if is_pad(cell) {
            cell.cell_type = "TIE_PAD".to_string();
            cell.function = None;
            cell.parameters.clear();
        }
    }
    logic
}

/// Fix each pad on the site its `LOC` names.
pub fn place_pads(
    device: &NexusDevice,
    netlist: &GateNetlist,
    placement: &mut PlacementResult,
) -> Result<()> {
    let packages = device.packages();
    let pins = packages
        .values()
        .next()
        .map(|p| &p.pins)
        .ok_or_else(|| emit_err("No package pin data for this device".to_string()))?;

    for cell in netlist.cells.iter().filter(|c| is_pad(c)) {
        let Some(loc) = cell.parameters.get("LOC") else {
            return Err(emit_err(format!(
                "Pad '{}' has no pin assignment. Annotate the port with @ {{ pin: \"...\" }}",
                cell.path
            )));
        };
        let &(x, y, bel) = pins.get(loc).ok_or_else(|| {
            let mut known: Vec<&str> = pins.keys().map(String::as_str).collect();
            known.sort_unstable();
            emit_err(format!(
                "Pin '{}' on '{}' has no known location. Known pins: {}",
                loc,
                cell.path,
                known.join(", ")
            ))
        })?;
        placement.placements.insert(
            cell.id,
            crate::placer::PlacementLoc::new(x, y, bel as usize, BelType::IoCell),
        );
    }
    Ok(())
}

/// Tiletype settings, loaded once per type.
struct ConfigDb<'a> {
    device: &'a NexusDevice,
    db_root: &'a Path,
    family: &'static str,
    cache: HashMap<String, Option<TileBitsDb>>,
    bits: Vec<GlobalBit>,
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

/// Hold the clock enable high on every clock buffer a route passes through.
///
/// A buffer's enable comes from a CIB input mux; selecting any input on that
/// mux reads as high, and a clock left without it never leaves the buffer.
fn enable_clock_buffers(config: &mut ConfigDb<'_>, routing: &RoutingResult) -> Result<()> {
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
        let cib_mux = find_cib_mux(device, wire)
            .ok_or_else(|| emit_err(format!("No CIB mux feeds {enable}")))?;
        let pip = device
            .wire_pips(cib_mux)
            .into_iter()
            .next()
            .ok_or_else(|| emit_err(format!("Nothing drives the CIB mux for {enable}")))?;
        config
            .bits
            .extend(device.pip_config_bits(pip).iter().cloned());
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

/// Collect every configuration bit the design needs.
pub fn emit_bits(
    device: &NexusDevice,
    variant: NexusVariant,
    netlist: &GateNetlist,
    placement: &PlacementResult,
    routing: &RoutingResult,
    tied: &TiedInputs,
    db_root: &Path,
) -> Result<Vec<GlobalBit>> {
    let mut config = ConfigDb {
        device,
        db_root,
        family: variant.prjoxide_family(),
        cache: HashMap::new(),
        bits: Vec::new(),
    };

    set_always_on_bits(&mut config)?;
    for route in routing.routes.values() {
        for &pip in &route.pips {
            config
                .bits
                .extend(device.pip_config_bits(pip).iter().cloned());
        }
    }

    enable_clock_buffers(&mut config, routing)?;

    let is_tied_high = |cell: CellId, index: usize| tied.values.get(&(cell, index)) == Some(&true);
    let tied_low = |cell: CellId, first: usize, count: usize| -> Vec<usize> {
        (0..count)
            .filter(|&i| tied.values.get(&(cell, first + i)) == Some(&false))
            .collect()
    };
    let mut slices_with_ff: HashSet<(u32, u32, char, bool)> = HashSet::new();

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
            emit_pad(&mut config, cell, x, y, loc.bel_index)?;
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
                    .is_some_and(|_| tied.values.contains_key(&(cell.id, 0)))
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
                config.set_enum(&tile, &format!("{prefix}.REGSET"), "RESET")?;
                config.set_enum(&tile, &format!("{prefix}.LSRMODE"), "LSR")?;
                config.set_enum(&tile, &format!("{prefix}.SEL"), "DF")?;
                config.set_enum(&tile, &format!("SLICE{slice}.CLKMUX"), "CLK")?;
                let uses_reset = cell.reset.is_some() || cell.cell_type.contains("RST");
                slices_with_ff.insert((x, y, slice, uses_reset));
            }
            BelType::Lut4 => {
                let slice = slice_of(number);
                let lut = number % 2;
                let init = fold_tied_low(lut_init(cell)?, &tied_low(cell.id, 0, 4));
                config.set_word(&tile, &format!("SLICE{slice}.K{lut}.INIT"), init)?;
            }
            other => {
                return Err(emit_err(format!(
                    "Cell '{}' needs a {other:?} site, which the backend does not support yet",
                    cell.path
                )))
            }
        }
    }

    for &(x, y, slice, uses_reset) in &slices_with_ff {
        if !uses_reset {
            // An unconnected set/reset input reads high; this holds it inactive.
            let tile = config.tile(x, y, "PLC")?;
            config.set_enum(&tile, &format!("SLICE{slice}.LSRMUX"), "INV")?;
        }
    }

    Ok(config.bits)
}

fn emit_pad(
    config: &mut ConfigDb<'_>,
    cell: &Cell,
    x: u32,
    y: u32,
    bel_index: usize,
) -> Result<()> {
    if cell.parameters.get("LOC").map(String::as_str) == Some("OSC") {
        return emit_oscillator(config, x, y);
    }

    let tile = config
        .device
        .prjoxide_tile(x, y, |t| t.starts_with("SYSIO_B1_0_"))
        .cloned()
        .ok_or_else(|| {
            emit_err(format!(
                "Pad '{}' is at column {x}, row {y}, outside bank 1, the only bank supported so far",
                cell.path
            ))
        })?;
    let side = ['A', 'B'][bel_index];
    let is_output = cell.function == Some(CellFunction::OutputPad);
    let standard = cell
        .parameters
        .get("IO_STANDARD")
        .map(String::as_str)
        .unwrap_or("LVCMOS33");
    let direction = if is_output { "OUTPUT" } else { "INPUT" };
    let prefix = format!("PIO{side}");
    config.set_enum(
        &tile,
        &format!("{prefix}.BASE_TYPE"),
        &format!("{direction}_{standard}"),
    )?;
    config.set_enum(&tile, &format!("{prefix}.PULLMODE"), "NONE")?;
    config.set_enum(&tile, &format!("{prefix}.GLITCHFILTER"), "OFF")?;
    if is_output {
        config.set_enum(&tile, &format!("{prefix}.DRIVE_3V3"), "8")?;
        config.set_enum(&tile, &format!("{prefix}.SLEWRATE"), "MED")?;
        config.set_enum(&tile, &format!("{prefix}.TMUX"), "INV")?;
    }

    let bank = config.tile(159, 14, "BANKREF1")?;
    config.set_enum(&bank, "BANK1.VCC", "3V3")
}

fn emit_oscillator(config: &mut ConfigDb<'_>, x: u32, y: u32) -> Result<()> {
    let tile = config.tile(x, y, "EFB_1_OSC")?;
    config.set_enum(&tile, "OSC_CORE.HF_OSC_EN", "ENABLED")?;
    config.set_enum(&tile, "OSC_CORE.HFDIV_FABRIC_EN", "ENABLED")?;
    config.set_enum(&tile, "OSC_CORE.DTR_EN", "ENABLED")?;
    config.set_enum(&tile, "OSC_CORE.DEBUG_N", "DISABLED")?;
    config.set_word(&tile, "OSC_CORE.HF_CLK_DIV", OSC_HF_CLK_DIV)?;
    config.set_word(&tile, "OSC_CORE.HF_SED_SEC_DIV", OSC_SED_SEC_DIV)?;
    // HFOUTEN must read high. This CIB mux feeds it from its default input.
    let cib = config.tile(134, 1, "CIB_T")?;
    config.set_pip(&cib, "JCIBMUXOUTD3", "JCIBMUXINA3")
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
