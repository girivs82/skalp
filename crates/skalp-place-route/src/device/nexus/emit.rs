//! Turn a placed and routed design into Nexus configuration bits.
//!
//! The router connects wires; this module sets everything else: LUT truth
//! tables, flip-flop modes, carry mode, IO pads, the oscillator, and the
//! dedicated connections the router does not handle (carry chains, constants).
//!
//! The settings mirror what nextpnr writes for the same cells.

use super::prjoxide_graph::{
    load_tiletype, node_name, resolve_wire, ConfigBit, GlobalBit, TileBitsDb,
};
use super::{NexusDevice, NexusVariant, PrjoxideTile};
use crate::bitstream::config::{ConfigGenerator, DesignBit, PlacedDesign};
use crate::bitstream::prepare::{constant_value, is_pad};
use crate::device::{BelType, Device};
use crate::error::{PlaceRouteError, Result};
use crate::router::RoutingResult;
use skalp_lir::gate_netlist::{Cell, CellId};
use skalp_lir::tech_library::CellFunction;
use std::collections::{HashMap, HashSet};
use std::path::Path;

/// Slices per logic tile, in LUT/FF numbering order.
const SLICES: [char; 4] = ['A', 'B', 'C', 'D'];

/// Oscillator divider: 450 MHz / (24 + 1) = 18 MHz.
const OSC_HF_CLK_DIV: u64 = 24;
/// Clock frequency the oscillator produces with this divider, in MHz.
pub const OSC_FREQUENCY_MHZ: f64 = 450.0 / (OSC_HF_CLK_DIV as f64 + 1.0);
/// Divider for the configuration engine's clock.
const OSC_SED_SEC_DIV: u64 = 1;

fn emit_err(message: String) -> PlaceRouteError {
    PlaceRouteError::BitstreamFailed(message)
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
        Ok(bits
            .into_iter()
            .map(|b| DesignBit {
                frame: b.frame,
                bit: b.bit,
                value: !b.invert,
            })
            .collect())
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

    let is_tied_high = |cell: CellId, index: usize| tied.value(cell, index) == Some(true);
    let tied_low = |cell: CellId, first: usize, count: usize| -> Vec<usize> {
        (0..count)
            .filter(|&i| tied.value(cell, first + i) == Some(false))
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
                config.set_enum(&tile, &format!("{prefix}.REGSET"), "RESET")?;
                config.set_enum(&tile, &format!("{prefix}.LSRMODE"), "LSR")?;
                config.set_enum(&tile, &format!("{prefix}.SEL"), "DF")?;
                config.set_enum(&tile, &format!("SLICE{slice}.CLKMUX"), "CLK")?;
                let uses_reset = cell.reset.is_some() || cell.cell_type.contains("RST");
                slices_with_ff.insert((x, y, slice, uses_reset));
            }
            BelType::DistRam => {
                if (0..cell.inputs.len()).any(|i| tied.value(cell.id, i).is_some()) {
                    return Err(emit_err(format!(
                        "Distributed RAM '{}' has a constant input, which is not supported yet",
                        cell.path
                    )));
                }
                emit_dist_ram(&mut config, &tile, cell)?;
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

/// Bank voltage a standard runs at.
fn bank_voltage(standard: &str) -> Result<&'static str> {
    let voltage = standard.trim_start_matches(|c: char| c.is_alphabetic());
    match voltage {
        "33" => Ok("3V3"),
        "25" => Ok("2V5"),
        "18" => Ok("1V8"),
        "15" => Ok("1V5"),
        "12" => Ok("1V2"),
        _ => Err(emit_err(format!("Unknown IO standard '{standard}'"))),
    }
}

fn emit_pad(
    config: &mut ConfigDb<'_>,
    cell: &Cell,
    x: u32,
    y: u32,
    bel_index: usize,
) -> Result<()> {
    if cell.parameters.get("LOC").map(String::as_str) == Some(super::OSC_PIN) {
        return emit_oscillator(config, x, y);
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
        let voltage = bank_voltage(standard)?;
        config.set_enum(&tile, &format!("{prefix}.DRIVE_{voltage}"), "8")?;
        config.set_enum(&tile, &format!("{prefix}.SLEWRATE"), "MED")?;
        config.set_enum(&tile, &format!("{prefix}.TMUX"), "INV")?;
    }

    let bank_ref = format!("BANKREF{bank}");
    let bank_tile = config
        .device
        .prjoxide_tile_of_type(&bank_ref)
        .cloned()
        .ok_or_else(|| emit_err(format!("No {bank_ref} tile on this device")))?;
    config.set_enum(
        &bank_tile,
        &format!("BANK{bank}.VCC"),
        bank_voltage(standard)?,
    )
}

fn emit_oscillator(config: &mut ConfigDb<'_>, x: u32, y: u32) -> Result<()> {
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
    let cib_mux = find_cib_mux(device, enable)
        .ok_or_else(|| emit_err("No CIB mux feeds the oscillator enable".to_string()))?;
    let pip = device
        .wire_pips(cib_mux)
        .into_iter()
        .next()
        .ok_or_else(|| emit_err("Nothing drives the oscillator enable mux".to_string()))?;
    config
        .bits
        .extend(device.pip_config_bits(pip).iter().cloned());
    Ok(())
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
