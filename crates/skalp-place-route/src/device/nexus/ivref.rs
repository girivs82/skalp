//! Input reference generators (`IVREF_CORE`) of a Nexus device's
//! high-performance banks.
//!
//! An LVSTL input compares its pin against an internal reference
//! (`VREF_LVSTL = VREF_INT`), but that setting only selects the reference:
//! its level comes from the bank's reference generator, a block of its own
//! with a 7-bit level input `IVREFI[6:0]` driven from the fabric. Without a
//! generator in use, the bank's LVSTL inputs read 1 whatever the pin does.
//! Lattice's tools reach the generator through the `DQSBUF_IVREF`
//! primitive (a DQS buffer with the extra level input); its level is a
//! code from 0 to 127 that read training sweeps (49 is the usual start).
//!
//! A bank has at most one generator. The device data gives each a
//! `BANK<n>.IVREF` setting in the tile that holds its level pins
//! (`JIVREFI<k>_IVREF_CORE`, fed by the fabric through a CIB mux next to
//! that tile).
//!
//! The device data says what the codes mean (`<family>/input_reference.json`,
//! [`LevelScale`]): the reference is `offset_mv + code * step_mv`.
//!
//! A design asks for a generator in one of three ways:
//! - a fixed level on the pads that use it, beside the I/O standard: a
//!   measurable one (`vref: "22%"` of the bank's I/O supply, or
//!   `vref: "245mV"`), converted to the nearest code by the device's
//!   scale, or the device's own code (`vref_code: 49`);
//! - SKALP's vendor-neutral `INPUT_REFERENCE` primitive, whose `LEVEL`
//!   input the design drives (to train it), placed by its `PIN` (a pin of
//!   the bank) or `LOC` (`BANK<n>`);
//! - Lattice's own `DQSBUF_IVREF` (`IVREFI` input), for designs written
//!   against the vendor library.

use super::prjoxide_load::PrjoxideTile;
use crate::collections::{HashMap, HashMapExt};
use skalp_frontend::hir::VrefSetting;
use skalp_lir::gate_netlist::{
    Cell, CellId, GateNet, GateNetId, GateNetlist, BLACKBOX_INPUTS_PARAM, BLACKBOX_OUTPUTS_PARAM,
    VREF_LEVEL_PARAM,
};
use skalp_lir::tech_library::CellFunction;
use std::path::Path;

/// Cell types that place a bank's reference generator: SKALP's
/// vendor-neutral primitive, Lattice's `DQSBUF_IVREF` (only its level
/// input so far) and the bare block, as blackboxes or not.
const CELL_TYPES: [&str; 3] = [NEUTRAL_CELL_TYPE, "DQSBUF_IVREF", "IVREF_CORE"];

/// SKALP's vendor-neutral input reference primitive: input `LEVEL`, the
/// level code, and a `PIN` (any pin of the bank) or `LOC` (`BANK<n>`).
pub const NEUTRAL_CELL_TYPE: &str = "INPUT_REFERENCE";

/// Names the primitives give the level input: the vendor-neutral one, then
/// Lattice's.
const LEVEL_PORTS: [&str; 2] = ["LEVEL", "IVREFI"];

/// The generator's level pins as the device names them, in router pin
/// order (`IVREFI0` is the least significant bit).
const INPUT_PINS: [&str; 7] = [
    "IVREFI0", "IVREFI1", "IVREFI2", "IVREFI3", "IVREFI4", "IVREFI5", "IVREFI6",
];

/// Whether a cell is a bank reference generator.
pub fn is_vref_generator(cell: &Cell) -> bool {
    let name = cell
        .cell_type
        .strip_prefix("BLACKBOX_")
        .unwrap_or(&cell.cell_type);
    CELL_TYPES.contains(&name)
}

/// The device pin a primitive port names: a bit of the level bus
/// (`LEVEL[3]`, `IVREFI[3]`, any case) or a pin itself (`IVREFI3`).
fn device_pin(port: &str) -> Option<&'static str> {
    let upper = port.to_ascii_uppercase();
    let rest = LEVEL_PORTS
        .iter()
        .find_map(|name| upper.strip_prefix(name))?;
    let bit = match rest {
        "" => "0",
        rest => rest
            .strip_prefix('[')
            .and_then(|r| r.strip_suffix(']'))
            .unwrap_or(rest),
    };
    let index: usize = bit.parse().ok()?;
    INPUT_PINS.get(index).copied()
}

/// Router pin number of a generator's `index`-th input.
pub fn input_router_pin(cell: &Cell, index: usize) -> Option<usize> {
    let pin = device_pin(cell.blackbox_pin(false, index)?)?;
    INPUT_PINS.iter().position(|&p| p == pin)
}

/// Level pins by router pin number.
pub fn input_pins() -> &'static [&'static str] {
    &INPUT_PINS
}

/// Name of a level pin's wire in the generator's tile.
pub fn site_wire(pin: &str) -> String {
    format!("J{pin}_IVREF_CORE")
}

/// What is wrong with a generator cell's ports, for an error: ports the
/// device's generator does not have (or not yet), and level bits the cell
/// leaves unconnected.
pub fn port_problems(cell: &Cell) -> Option<String> {
    let mut unknown = Vec::new();
    let mut index = 0;
    while let Some(port) = cell.blackbox_pin(true, index) {
        unknown.push(port.to_string());
        index += 1;
    }
    let mut connected = [false; INPUT_PINS.len()];
    index = 0;
    while let Some(port) = cell.blackbox_pin(false, index) {
        match input_router_pin(cell, index) {
            Some(pin) => connected[pin] = true,
            None => unknown.push(port.to_string()),
        }
        index += 1;
    }
    if !unknown.is_empty() {
        return Some(format!(
            "has ports the bank reference generator does not (only its {}-bit level \
             input, LEVEL or IVREFI, is supported so far): {}",
            INPUT_PINS.len(),
            unknown.join(", ")
        ));
    }
    let missing: Vec<&str> = INPUT_PINS
        .iter()
        .zip(connected)
        .filter(|(_, c)| !c)
        .map(|(p, _)| *p)
        .collect();
    (!missing.is_empty()).then(|| {
        format!(
            "leaves level bits {} unconnected; drive all {} bits of its level (49 is the \
             usual starting level)",
            missing.join(", "),
            INPUT_PINS.len()
        )
    })
}

/// A bank's reference generator: its bank and the tile holding its level
/// pins and its `BANK<n>.IVREF` setting.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Site {
    pub bank: u32,
    pub x: u32,
    pub y: u32,
    pub tiletype: String,
}

/// The bank whose generator a tile type's settings name (`BANK<n>.IVREF`),
/// from the tile type's database text.
fn bank_in_settings(text: &str) -> Option<u32> {
    text.match_indices("\"BANK").find_map(|(at, _)| {
        let rest = &text[at + 5..];
        let digits = rest.len() - rest.trim_start_matches(|c: char| c.is_ascii_digit()).len();
        (digits > 0 && rest[digits..].starts_with(".IVREF\""))
            .then(|| rest[..digits].parse().ok())
            .flatten()
    })
}

/// Every bank reference generator of a device whose tiles are `tiles`.
pub fn sites(db_root: &Path, family: &str, tiles: &[PrjoxideTile]) -> Vec<Site> {
    let mut bank_of_type: HashMap<&str, Option<u32>> = HashMap::new();
    let mut sites = Vec::new();
    for tile in tiles {
        let bank = *bank_of_type.entry(&tile.tiletype).or_insert_with(|| {
            let path = db_root
                .join(family)
                .join("tiletypes")
                .join(format!("{}.ron", tile.tiletype));
            std::fs::read_to_string(path)
                .ok()
                .and_then(|text| bank_in_settings(&text))
        });
        if let Some(bank) = bank {
            sites.push(Site {
                bank,
                x: tile.x,
                y: tile.y,
                tiletype: tile.tiletype.clone(),
            });
        }
    }
    sites.sort_by_key(|s| (s.bank, s.y, s.x));
    sites.dedup_by_key(|s| s.bank);
    sites
}

/// Whether an I/O standard's inputs compare against the bank reference
/// generator's level (the LVSTL standards, single-ended and differential).
pub fn uses_generator(standard: &str) -> bool {
    matches!(standard, "LVSTL_I" | "LVSTL_II" | "LVSTLD_I" | "LVSTLD_II")
}

/// Whether a pad cell reads its pin through an input that compares against
/// the bank reference generator.
pub fn is_referenced_input(cell: &Cell) -> bool {
    matches!(
        cell.function,
        Some(CellFunction::InputPad | CellFunction::BidirPad | CellFunction::ClockPad)
    ) && cell
        .parameters
        .get("IO_STANDARD")
        .is_some_and(|s| uses_generator(s))
}

/// The bank a generator's `LOC` names: `BANK3` or `3`.
fn located_bank(loc: &str) -> Option<u32> {
    loc.strip_prefix("BANK").unwrap_or(loc).parse().ok()
}

/// The bank a pad cell is in, from its pin (`LOC`).
fn pad_bank(cell: &Cell, pin_bank: &dyn Fn(&str) -> Option<u32>) -> Option<u32> {
    pin_bank(cell.parameters.get("LOC")?)
}

/// The bank a generator cell names: its `LOC` (`BANK<n>`) or its `PIN` (a
/// pin of the bank); `None` if it names neither.
fn named_bank(cell: &Cell, pin_bank: &dyn Fn(&str) -> Option<u32>) -> Result<Option<u32>, String> {
    if let Some(loc) = cell.parameters.get("LOC") {
        return located_bank(loc).map(Some).ok_or_else(|| {
            format!(
                "Reference generator '{}': LOC '{loc}' names no bank (use BANK<n>)",
                cell.path
            )
        });
    }
    if let Some(pin) = cell.parameters.get("PIN") {
        return pin_bank(pin).map(Some).ok_or_else(|| {
            format!(
                "Reference generator '{}': pin '{pin}' is not a pin of an I/O bank",
                cell.path
            )
        });
    }
    Ok(None)
}

/// The site of every reference generator of a design: the bank its `LOC`
/// or `PIN` names, or else a bank whose inputs need one and that has none
/// yet, in bank order. `pin_bank` gives the bank of a package pin.
pub fn choose_sites(
    sites: &[Site],
    netlist: &GateNetlist,
    pin_bank: &dyn Fn(&str) -> Option<u32>,
) -> Result<HashMap<CellId, Site>, String> {
    let mut needing: Vec<u32> = netlist
        .cells
        .iter()
        .filter(|c| is_referenced_input(c))
        .filter_map(|c| pad_bank(c, pin_bank))
        .collect();
    needing.sort_unstable();
    needing.dedup();
    let mut generators: Vec<(&Cell, Option<u32>)> = Vec::new();
    for cell in netlist.cells.iter().filter(|c| is_vref_generator(c)) {
        if let Some(problem) = port_problems(cell) {
            return Err(format!("Reference generator '{}' {problem}", cell.path));
        }
        generators.push((cell, named_bank(cell, pin_bank)?));
    }
    // Generators with a bank named take theirs first
    generators.sort_by_key(|(c, bank)| (bank.is_none(), c.id.0));
    let mut chosen = HashMap::new();
    let mut taken: Vec<u32> = Vec::new();
    for (cell, named) in generators {
        let bank = match named {
            Some(bank) => bank,
            None => *needing.iter().find(|b| !taken.contains(b)).ok_or_else(|| {
                format!(
                    "Reference generator '{}': no bank with LVSTL inputs is left \
                     for it; give it a PIN (a pin of its bank) or a LOC (BANK<n>)",
                    cell.path
                )
            })?,
        };
        if taken.contains(&bank) {
            return Err(format!(
                "Reference generator '{}': bank {bank} already has one, and a bank \
                 has only one",
                cell.path
            ));
        }
        let site = sites.iter().find(|s| s.bank == bank).ok_or_else(|| {
            let banks: Vec<String> = sites.iter().map(|s| s.bank.to_string()).collect();
            format!(
                "Reference generator '{}': bank {bank} has none; this device has them \
                 in banks {}",
                cell.path,
                if banks.is_empty() {
                    "none".to_string()
                } else {
                    banks.join(", ")
                }
            )
        })?;
        taken.push(bank);
        chosen.insert(cell.id, site.clone());
    }
    Ok(chosen)
}

/// What a family's generator codes mean: the reference is
/// `offset_mv + code * step_mv`. Read from the device data
/// (`<family>/input_reference.json`).
#[derive(Debug, Clone, PartialEq, serde::Deserialize)]
pub struct LevelScale {
    /// Reference at code 0, in mV
    pub offset_mv: f64,
    /// Rise per code, in mV
    pub step_mv: f64,
    /// Whether the scale was measured on silicon; until it is, levels set
    /// by voltage or share of the supply are only as good as its sources
    #[serde(default)]
    pub verified: bool,
    /// Where the numbers come from
    #[serde(default)]
    pub source: String,
}

impl LevelScale {
    /// The family's scale, if its device data has one.
    pub fn load(db_root: &Path, family: &str) -> Result<Option<Self>, String> {
        let path = db_root.join(family).join("input_reference.json");
        let text = match std::fs::read_to_string(&path) {
            Ok(text) => text,
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => return Ok(None),
            Err(e) => return Err(format!("{}: {e}", path.display())),
        };
        let scale: Self =
            serde_json::from_str(&text).map_err(|e| format!("{}: {e}", path.display()))?;
        if !(scale.step_mv.is_finite() && scale.step_mv > 0.0 && scale.offset_mv.is_finite()) {
            return Err(format!(
                "{}: the step must be a positive number of mV",
                path.display()
            ));
        }
        Ok(Some(scale))
    }

    /// The reference, in mV, a code gives.
    pub fn millivolts(&self, code: u32) -> f64 {
        self.offset_mv + f64::from(code) * self.step_mv
    }

    /// The code nearest a reference in mV, if the generator reaches it
    /// (within half a step of its range).
    pub fn code_for(&self, millivolts: f64) -> Option<u32> {
        let code = ((millivolts - self.offset_mv) / self.step_mv).round();
        (0.0..=f64::from(MAX_CODE))
            .contains(&code)
            .then_some(code as u32)
    }
}

/// The largest generator code.
const MAX_CODE: u32 = (1 << INPUT_PINS.len()) - 1;

/// The generator code a pad's reference setting asks for, with the
/// reference it gives in mV when the family's scale is known.
fn level_code(
    setting: VrefSetting,
    standard: &str,
    scale: Option<&LevelScale>,
    pad: &str,
) -> Result<(u32, Option<f64>), String> {
    let target_mv = match setting {
        VrefSetting::DeviceCode(code) => {
            if code > MAX_CODE {
                return Err(format!(
                    "Pad '{pad}': reference generator code {code} is not a code from 0 \
                     to {MAX_CODE}"
                ));
            }
            return Ok((code, scale.map(|s| s.millivolts(code))));
        }
        VrefSetting::Millivolts(mv) => f64::from(mv),
        VrefSetting::SupplyHundredthsPercent(hundredths) => {
            let supply = super::emit::supply_mv(standard)
                .ok_or_else(|| format!("Pad '{pad}': no known I/O supply for {standard}"))?;
            f64::from(supply) * f64::from(hundredths) / 10_000.0
        }
    };
    let scale = scale.ok_or_else(|| {
        format!(
            "Pad '{pad}' sets its reference to {setting}, but the device data has no scale \
             for this family's reference generator (input_reference.json), so it cannot be \
             converted; give the device's own generator code with `vref_code`"
        )
    })?;
    let code = scale.code_for(target_mv).ok_or_else(|| {
        format!(
            "Pad '{pad}' sets its reference to {setting} ({target_mv:.0} mV), outside the \
             generator's {:.0} to {:.0} mV",
            scale.millivolts(0),
            scale.millivolts(MAX_CODE)
        )
    })?;
    Ok((code, Some(scale.millivolts(code))))
}

/// Add a generator for each bank whose pads set a fixed reference level
/// (`vref` / `vref_code`, [`VREF_LEVEL_PARAM`]): a vendor-neutral
/// [`NEUTRAL_CELL_TYPE`] cell on the bank, its level driven by constant
/// LUTs. A measured level becomes the nearest code of the family's
/// `scale`. The pads of a bank must agree on the code; a bank whose level a
/// generator instance already sets cannot also take one from its pads.
pub fn add_pad_level_generators(
    netlist: &GateNetlist,
    pin_bank: &dyn Fn(&str) -> Option<u32>,
    scale: Option<&LevelScale>,
) -> Result<Option<GateNetlist>, String> {
    // bank -> (code, a pad that sets it, its setting)
    let mut levels: std::collections::BTreeMap<u32, (u32, &str, VrefSetting)> =
        std::collections::BTreeMap::new();
    for cell in &netlist.cells {
        let Some(setting) = cell.parameters.get(VREF_LEVEL_PARAM) else {
            continue;
        };
        let setting =
            VrefSetting::from_param(setting).map_err(|e| format!("Pad '{}': {e}", cell.path))?;
        let standard = cell
            .parameters
            .get("IO_STANDARD")
            .map(String::as_str)
            .unwrap_or("its default");
        if !uses_generator(standard) {
            return Err(format!(
                "Pad '{}' sets a reference level (vref), but {standard} does not compare \
                 against an internal reference",
                cell.path
            ));
        }
        let (level, millivolts) = level_code(setting, standard, scale, &cell.path)?;
        let bank = pad_bank(cell, pin_bank).ok_or_else(|| {
            format!(
                "Pad '{}' sets a reference level (vref) but has no pin in an I/O bank",
                cell.path
            )
        })?;
        let (first, user, first_setting) =
            *levels.entry(bank).or_insert((level, &cell.path, setting));
        if first != level {
            return Err(format!(
                "Pads '{user}' ({first_setting}, code {first}) and '{}' ({setting}, code \
                 {level}) set different reference levels, but bank {bank} has one",
                cell.path
            ));
        }
        if user == cell.path {
            match (millivolts, scale) {
                (Some(mv), Some(scale)) => {
                    if matches!(setting, VrefSetting::DeviceCode(_)) {
                        log::info!(
                            "Bank {bank} input reference: {setting} ({mv:.0} mV on the \
                             device data's scale)"
                        );
                    } else {
                        log::info!(
                            "Bank {bank} input reference: {setting} -> generator code {level} \
                             ({mv:.0} mV)"
                        );
                    }
                    if !scale.verified && !matches!(setting, VrefSetting::DeviceCode(_)) {
                        log::warn!(
                            "Bank {bank}: the reference generator's scale is not verified \
                             on silicon ({}); {setting} may not be the level you get",
                            scale.source
                        );
                    }
                }
                _ => log::info!("Bank {bank} input reference: {setting}"),
            }
        }
    }
    if levels.is_empty() {
        return Ok(None);
    }
    for cell in netlist.cells.iter().filter(|c| is_vref_generator(c)) {
        if let Some(bank) = named_bank(cell, pin_bank)? {
            if let Some((_, pad, _)) = levels.get(&bank) {
                return Err(format!(
                    "Bank {bank} takes its reference level both from pad '{pad}' (vref) \
                     and from generator '{}'; use one",
                    cell.path
                ));
            }
        }
    }
    let mut netlist = netlist.clone();
    let constant = |netlist: &mut GateNetlist, value: bool| {
        let name = format!("vref_level_constant_{}", u8::from(value));
        if let Some(net) = netlist.nets.iter().find(|n| n.name == name) {
            return net.id;
        }
        let net = netlist.add_net(GateNet::new(GateNetId(0), name.clone()));
        let mut driver = Cell::new_comb(
            CellId(0),
            "LUT4".to_string(),
            "nexus".to_string(),
            0.0,
            format!("{name}_driver"),
            Vec::new(),
            vec![net],
        );
        driver.function = Some(CellFunction::Lut4);
        driver.lut_init = Some(if value { 0xFFFF } else { 0 });
        netlist.add_cell(driver);
        net
    };
    for (bank, (level, _, _)) in levels {
        let inputs: Vec<GateNetId> = (0..INPUT_PINS.len())
            .map(|bit| constant(&mut netlist, (level >> bit) & 1 == 1))
            .collect();
        let mut generator = Cell::new_comb(
            CellId(0),
            NEUTRAL_CELL_TYPE.to_string(),
            "blackbox".to_string(),
            0.0,
            format!("top.vref_bank{bank}"),
            inputs,
            Vec::new(),
        );
        let ports: Vec<String> = (0..INPUT_PINS.len())
            .map(|bit| format!("LEVEL[{bit}]"))
            .collect();
        generator
            .parameters
            .insert(BLACKBOX_INPUTS_PARAM.into(), ports.join(","));
        generator
            .parameters
            .insert(BLACKBOX_OUTPUTS_PARAM.into(), String::new());
        generator
            .parameters
            .insert("LOC".into(), format!("BANK{bank}"));
        netlist.add_cell(generator);
    }
    Ok(Some(netlist))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn generator(inputs: &str, loc: Option<&str>) -> Cell {
        let mut cell = Cell::new_comb(
            CellId(0),
            "BLACKBOX_DQSBUF_IVREF".into(),
            "blackbox".into(),
            0.0,
            "top.vref".into(),
            vec![],
            vec![],
        );
        cell.parameters
            .insert(BLACKBOX_INPUTS_PARAM.into(), inputs.into());
        cell.parameters
            .insert(BLACKBOX_OUTPUTS_PARAM.into(), String::new());
        if let Some(loc) = loc {
            cell.parameters.insert("LOC".into(), loc.into());
        }
        cell
    }

    const BUS: &str = "IVREFI[0],IVREFI[1],IVREFI[2],IVREFI[3],IVREFI[4],IVREFI[5],IVREFI[6]";

    #[test]
    fn level_bits_map_to_device_pins() {
        let cell = generator(BUS, None);
        assert!(is_vref_generator(&cell));
        assert_eq!(input_router_pin(&cell, 0), Some(0));
        assert_eq!(input_router_pin(&cell, 6), Some(6));
        assert_eq!(port_problems(&cell), None);
        let named = generator(
            "IVREFI6,IVREFI0,IVREFI1,IVREFI2,IVREFI3,IVREFI4,IVREFI5",
            None,
        );
        assert_eq!(input_router_pin(&named, 0), Some(6));
        assert_eq!(port_problems(&named), None);
    }

    #[test]
    fn missing_and_unknown_ports_are_reported() {
        let short = generator("IVREFI[0],IVREFI[1]", None);
        assert!(port_problems(&short).unwrap().contains("IVREFI2"));
        let dqs = generator(&format!("{BUS},DQS"), None);
        assert!(port_problems(&dqs).unwrap().contains("DQS"));
    }

    #[test]
    fn the_bank_comes_from_the_tile_settings() {
        let text = r#"enums: { "BANK3.DIFF_IO": (), "BANK3.IVREF": ( options: {} ) }"#;
        assert_eq!(bank_in_settings(text), Some(3));
        assert_eq!(bank_in_settings(r#""BANK3.VCC": ()"#), None);
        assert_eq!(bank_in_settings(r#""BANK12.IVREF": ()"#), Some(12));
    }

    #[test]
    fn the_neutral_primitive_names_its_bank_by_a_pin() {
        let mut cell = generator(
            "LEVEL[0],LEVEL[1],LEVEL[2],LEVEL[3],LEVEL[4],LEVEL[5],LEVEL[6]",
            None,
        );
        cell.cell_type = NEUTRAL_CELL_TYPE.into();
        cell.parameters.insert("PIN".into(), "AA6".into());
        assert!(is_vref_generator(&cell));
        assert_eq!(port_problems(&cell), None);
        let bank = |pin: &str| (pin == "AA6").then_some(3);
        assert_eq!(named_bank(&cell, &bank), Ok(Some(3)));
        cell.parameters.insert("PIN".into(), "P24".into());
        assert!(named_bank(&cell, &bank).is_err());
    }

    fn lvstl_input(path: &str, level: Option<&str>) -> Cell {
        let mut pad = Cell::new_comb(
            CellId(0),
            "IB".into(),
            "nexus".into(),
            0.0,
            path.into(),
            vec![],
            vec![],
        );
        pad.function = Some(CellFunction::InputPad);
        pad.parameters
            .insert("IO_STANDARD".into(), "LVSTL_I".into());
        pad.parameters.insert("LOC".into(), "AA6".into());
        if let Some(level) = level {
            pad.parameters.insert(VREF_LEVEL_PARAM.into(), level.into());
        }
        pad
    }

    /// The documented LFCPNX step on an assumed 0 mV base.
    const SCALE: LevelScale = LevelScale {
        offset_mv: 0.0,
        step_mv: 5.0,
        verified: false,
        source: String::new(),
    };

    /// The code a generator's level inputs are tied to.
    fn tied_code(netlist: &GateNetlist, generator: &Cell) -> u32 {
        generator
            .inputs
            .iter()
            .enumerate()
            .map(|(bit, &net)| {
                let driver = netlist
                    .cells
                    .iter()
                    .find(|c| c.outputs.contains(&net))
                    .expect("each level bit has a driver");
                u32::from(driver.lut_init == Some(0xFFFF)) << bit
            })
            .sum()
    }

    #[test]
    fn measured_levels_become_the_nearest_code() {
        assert_eq!(SCALE.code_for(245.0), Some(49));
        assert_eq!(SCALE.code_for(247.0), Some(49));
        assert_eq!(SCALE.code_for(636.0), Some(127));
        assert_eq!(SCALE.code_for(640.0), None);
        // 22.27% of LVSTL's 1.1 V supply is 245 mV
        let pct = VrefSetting::parse_measured("22.27%").unwrap();
        assert_eq!(level_code(pct, "LVSTL_I", Some(&SCALE), "p").unwrap().0, 49);
        let mv = VrefSetting::parse_measured("0.245V").unwrap();
        assert_eq!(level_code(mv, "LVSTL_I", Some(&SCALE), "p").unwrap().0, 49);
        // A measured level needs the family's scale; a device code does not
        let err = level_code(pct, "LVSTL_I", None, "p").unwrap_err();
        assert!(err.contains("vref_code"), "{err}");
        let code = VrefSetting::DeviceCode(49);
        assert_eq!(level_code(code, "LVSTL_I", None, "p").unwrap(), (49, None));
        assert!(level_code(VrefSetting::Millivolts(700), "LVSTL_I", Some(&SCALE), "p").is_err());
    }

    #[test]
    fn the_scale_comes_from_the_device_data() {
        let dir = std::env::temp_dir().join(format!("ivref_scale_{}", std::process::id()));
        std::fs::create_dir_all(dir.join("FAM")).unwrap();
        assert_eq!(LevelScale::load(&dir, "FAM"), Ok(None));
        std::fs::write(
            dir.join("FAM/input_reference.json"),
            r#"{"offset_mv": 10, "step_mv": 5, "source": "test"}"#,
        )
        .unwrap();
        let scale = LevelScale::load(&dir, "FAM").unwrap().unwrap();
        assert_eq!(scale.millivolts(2), 20.0);
        assert!(!scale.verified);
        std::fs::write(
            dir.join("FAM/input_reference.json"),
            r#"{"offset_mv": 10, "step_mv": 0}"#,
        )
        .unwrap();
        assert!(LevelScale::load(&dir, "FAM").is_err());
        std::fs::remove_dir_all(&dir).unwrap();
    }

    #[test]
    fn a_pad_level_becomes_a_generator_with_that_level() {
        let bank = |_: &str| Some(3);
        let mut netlist = GateNetlist::new("top".into(), "nexus".into());
        netlist.add_cell(lvstl_input("top.dq0", Some("code 49")));
        netlist.add_cell(lvstl_input("top.dq1", Some("245mV")));
        netlist.add_cell(lvstl_input("top.dq2", None));
        let added = add_pad_level_generators(&netlist, &bank, Some(&SCALE))
            .unwrap()
            .unwrap();
        let generators: Vec<&Cell> = added
            .cells
            .iter()
            .filter(|c| is_vref_generator(c))
            .collect();
        assert_eq!(generators.len(), 1, "one per bank");
        let generator = generators[0];
        assert_eq!(generator.parameters["LOC"], "BANK3");
        assert_eq!(port_problems(generator), None);
        assert_eq!(tied_code(&added, generator), 49);

        // No pad level, nothing added
        let plain = GateNetlist::new("top".into(), "nexus".into());
        assert!(add_pad_level_generators(&plain, &bank, Some(&SCALE))
            .unwrap()
            .is_none());
    }

    #[test]
    fn pad_levels_must_agree_and_fit() {
        let bank = |_: &str| Some(3);
        let mut netlist = GateNetlist::new("top".into(), "nexus".into());
        netlist.add_cell(lvstl_input("top.dq0", Some("code 49")));
        netlist.add_cell(lvstl_input("top.dq1", Some("250mV")));
        let err = add_pad_level_generators(&netlist, &bank, Some(&SCALE)).unwrap_err();
        assert!(err.contains("different reference levels"), "{err}");

        let mut netlist = GateNetlist::new("top".into(), "nexus".into());
        netlist.add_cell(lvstl_input("top.dq0", Some("code 128")));
        let err = add_pad_level_generators(&netlist, &bank, Some(&SCALE)).unwrap_err();
        assert!(err.contains("0 to 127"), "{err}");

        let mut netlist = GateNetlist::new("top".into(), "nexus".into());
        let mut cmos = lvstl_input("top.dq0", Some("code 49"));
        cmos.parameters
            .insert("IO_STANDARD".into(), "LVCMOS12H".into());
        netlist.add_cell(cmos);
        let err = add_pad_level_generators(&netlist, &bank, Some(&SCALE)).unwrap_err();
        assert!(err.contains("does not compare"), "{err}");

        // A generator instance on the same bank as a pad level is refused
        let mut netlist = GateNetlist::new("top".into(), "nexus".into());
        netlist.add_cell(lvstl_input("top.dq0", Some("code 49")));
        netlist.add_cell(generator(BUS, Some("BANK3")));
        let err = add_pad_level_generators(&netlist, &bank, Some(&SCALE)).unwrap_err();
        assert!(err.contains("use one"), "{err}");
    }

    #[test]
    fn generators_go_to_the_banks_that_need_them() {
        let sites = vec![
            Site {
                bank: 3,
                x: 159,
                y: 73,
                tiletype: "BANKREF3_DDR42".into(),
            },
            Site {
                bank: 4,
                x: 77,
                y: 74,
                tiletype: "BANKREF4_DLY42_DDR41".into(),
            },
        ];
        let mut netlist = GateNetlist::new("top".into(), "nexus".into());
        let mut pad = Cell::new_comb(
            CellId(0),
            "IB".into(),
            "nexus".into(),
            0.0,
            "top.dq".into(),
            vec![],
            vec![],
        );
        pad.function = Some(CellFunction::InputPad);
        pad.parameters
            .insert("IO_STANDARD".into(), "LVSTL_I".into());
        pad.parameters.insert("LOC".into(), "AA6".into());
        netlist.add_cell(pad);
        let id = netlist.add_cell(generator(BUS, None));
        let bank = |_: &str| Some(4);
        let chosen = choose_sites(&sites, &netlist, &bank).unwrap();
        assert_eq!(chosen[&id].bank, 4);

        // A named bank wins; a bank without a generator is refused
        netlist.cells[1]
            .parameters
            .insert("LOC".into(), "BANK3".into());
        assert_eq!(choose_sites(&sites, &netlist, &bank).unwrap()[&id].bank, 3);
        netlist.cells[1]
            .parameters
            .insert("LOC".into(), "BANK5".into());
        let err = choose_sites(&sites, &netlist, &bank).unwrap_err();
        assert!(err.contains("banks 3, 4"), "{err}");

        // With no LVSTL inputs and no LOC there is no bank to choose
        netlist.cells[1].parameters.shift_remove("LOC");
        netlist.cells[0]
            .parameters
            .insert("IO_STANDARD".into(), "LVCMOS12H".into());
        assert!(choose_sites(&sites, &netlist, &bank).is_err());
    }
}
