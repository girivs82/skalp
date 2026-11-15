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

use super::prjoxide_load::PrjoxideTile;
use crate::collections::{HashMap, HashMapExt};
use skalp_lir::gate_netlist::{Cell, CellId, GateNetlist};
use skalp_lir::tech_library::CellFunction;
use std::path::Path;

/// Cell types that place a bank's reference generator: Lattice's
/// `DQSBUF_IVREF` primitive (only its level input so far) and the bare
/// block, as blackboxes or not.
const CELL_TYPES: [&str; 2] = ["DQSBUF_IVREF", "IVREF_CORE"];

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

/// The device pin a primitive port names: a bit of the `IVREFI` bus
/// (`IVREFI[3]`) or the pin itself (`IVREFI3`).
fn device_pin(port: &str) -> Option<&'static str> {
    let bit = match port.strip_prefix("IVREFI")? {
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
            "has ports the bank reference generator does not (only IVREFI[6:0], its \
             level, is supported so far): {}",
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
            "leaves level bits {} unconnected; drive all of IVREFI[6:0] (49 is the usual \
             starting level)",
            missing.join(", ")
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

/// The site of every reference generator of a design: the bank its `LOC`
/// names, or else a bank whose inputs need one and that has none yet, in
/// bank order. `pad_bank` gives the bank of a pad cell.
pub fn choose_sites(
    sites: &[Site],
    netlist: &GateNetlist,
    pad_bank: &dyn Fn(&Cell) -> Option<u32>,
) -> Result<HashMap<CellId, Site>, String> {
    let mut needing: Vec<u32> = netlist
        .cells
        .iter()
        .filter(|c| is_referenced_input(c))
        .filter_map(pad_bank)
        .collect();
    needing.sort_unstable();
    needing.dedup();
    let mut generators: Vec<&Cell> = netlist
        .cells
        .iter()
        .filter(|c| is_vref_generator(c))
        .collect();
    // Generators with a bank named take theirs first
    generators.sort_by_key(|c| (c.parameters.get("LOC").is_none(), c.id.0));
    let mut chosen = HashMap::new();
    let mut taken: Vec<u32> = Vec::new();
    for cell in generators {
        if let Some(problem) = port_problems(cell) {
            return Err(format!("Reference generator '{}' {problem}", cell.path));
        }
        let bank = match cell.parameters.get("LOC") {
            Some(loc) => located_bank(loc).ok_or_else(|| {
                format!(
                    "Reference generator '{}': LOC '{loc}' names no bank (use BANK<n>)",
                    cell.path
                )
            })?,
            None => *needing.iter().find(|b| !taken.contains(b)).ok_or_else(|| {
                format!(
                    "Reference generator '{}': no bank with LVSTL inputs is left \
                         for it; give it a LOC (BANK<n>)",
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

#[cfg(test)]
mod tests {
    use super::*;
    use skalp_lir::gate_netlist::{BLACKBOX_INPUTS_PARAM, BLACKBOX_OUTPUTS_PARAM};

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
        netlist.add_cell(pad);
        let id = netlist.add_cell(generator(BUS, None));
        let bank = |_: &Cell| Some(4);
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
