//! What a development board connects to its FPGA's pins.
//!
//! Driving a pin that something else on the board also drives can damage both
//! ends, so a pin is only allowed as an output if its board lists it as safe.
//! Everything else, DRAM, PCIe, SFP, flash, other chips, switches and clock
//! sources included, is refused unless the design gives a reason to override.

use crate::device::nexus::NexusVariant;
use skalp_lir::gate_netlist::{GateNetlist, DRIVE_OVERRIDE_PARAM};
use skalp_lir::tech_library::CellFunction;

/// A board's pins, by the ball names of its FPGA package.
pub struct BoardPins {
    /// Display name
    pub name: &'static str,
    /// Pins safe to drive, and what each one is
    pub safe_outputs: &'static [(&'static str, &'static str)],
    /// Other pins worth naming in an error
    pub connected: &'static [(&'static str, &'static str)],
}

/// CertusPro-NX Versa board, from its user guide (FPGA-EB-02053).
pub const CERTUSPRO_VERSA: BoardPins = BoardPins {
    name: "CertusPro-NX Versa",
    safe_outputs: &[
        ("R5", "LED_0"),
        ("R4", "LED_1"),
        ("R8", "LED_2"),
        ("R9", "LED_3"),
        ("U8", "LED_4"),
        ("R7", "LED_5"),
        ("R6", "LED_6"),
        ("P8", "LED_7"),
        ("M4", "SEG_A"),
        ("M3", "SEG_B"),
        ("M2", "SEG_C"),
        ("M1", "SEG_D"),
        ("N1", "SEG_E"),
        ("N2", "SEG_F"),
        ("N3", "SEG_G"),
        ("N4", "SEG_DP"),
        ("AC21", "K_DIG1"),
        ("W22", "K_DIG2"),
        ("AE26", "K_DIG3"),
        ("M9", "UART transmit, to the FTDI chip's RXD"),
        ("T1", "PMOD1_1"),
        ("U2", "PMOD1_2"),
        ("U3", "PMOD1_3"),
        ("V1", "PMOD1_4"),
        ("V3", "PMOD1_7"),
        ("W2", "PMOD1_8"),
        ("Y1", "PMOD1_9"),
        ("AA1", "PMOD1_10"),
        ("T3", "PMOD0_1"),
        ("T2", "PMOD0_2"),
        ("U1", "PMOD0_3"),
        ("U6", "PMOD0_4"),
        ("V2", "PMOD0_7"),
        ("W1", "PMOD0_8"),
        ("W3", "PMOD0_9"),
        ("AB1", "PMOD0_10"),
        ("AD23", "HP_GPIO1"),
        ("AC24", "HP_GPIO2"),
        ("AC25", "HP_GPIO3"),
        ("AC26", "HP_GPIO4"),
        ("AB26", "HP_GPIO5"),
        ("AB23", "HP_GPIO6"),
        ("AB24", "HP_GPIO7"),
        ("AD26", "HP_GPIO8"),
    ],
    connected: &[
        ("AA23", "DIP switch 1, shorted to ground when closed"),
        ("AB22", "DIP switch 2, shorted to ground when closed"),
        ("AC22", "DIP switch 3, shorted to ground when closed"),
        ("AA22", "DIP switch 4, shorted to ground when closed"),
        ("W21", "DIP switch 5, shorted to ground when closed"),
        ("G4", "PROGRAMN button SW2"),
        ("N9", "FPGA_RESET button SW3"),
        ("H21", "USB reset button SW7"),
        ("P24", "the 125 MHz oscillator"),
        ("L8", "the FTDI chip's UART transmit line"),
    ],
};

/// The board a Nexus device is fitted to, where SKALP knows one.
pub fn for_nexus_variant(variant: NexusVariant) -> Option<&'static BoardPins> {
    match variant {
        NexusVariant::Lfcpnx100 => Some(&CERTUSPRO_VERSA),
        _ => None,
    }
}

/// Check every pad a design drives against what its board allows.
pub fn check_outputs(netlist: &GateNetlist, board: &BoardPins) -> Result<(), String> {
    let refusals: Vec<String> = netlist
        .cells
        .iter()
        .filter(|cell| {
            matches!(
                cell.function,
                Some(CellFunction::OutputPad) | Some(CellFunction::BidirPad)
            ) && !cell.parameters.contains_key(DRIVE_OVERRIDE_PARAM)
        })
        .filter_map(|cell| {
            let pin = cell.parameters.get("LOC")?;
            if board.safe_outputs.iter().any(|(safe, _)| safe == pin) {
                return None;
            }
            let what = board
                .connected
                .iter()
                .find(|(known, _)| known == pin)
                .map_or("not listed as safe to drive", |(_, what)| *what);
            Some(format!("  {pin} ('{}'): {what}", cell.path))
        })
        .collect();
    if refusals.is_empty() {
        return Ok(());
    }
    Err(format!(
        "These pins are not safe to drive on the {}, so the design was refused:\n{}\nIf you are sure, add drive_override: \"reason\" to that pin's constraints.",
        board.name,
        refusals.join("\n")
    ))
}

#[cfg(test)]
mod tests {
    use super::*;
    use skalp_lir::gate_netlist::{Cell, CellId, GateNetId};

    fn pad(pin: &str, function: CellFunction) -> Cell {
        let mut cell = Cell::new_comb(
            CellId(0),
            "OB".to_string(),
            "nexus".to_string(),
            0.0,
            format!("top.{pin}"),
            vec![GateNetId(0)],
            Vec::new(),
        );
        cell.function = Some(function);
        cell.parameters.insert("LOC".to_string(), pin.to_string());
        cell
    }

    fn design(cells: Vec<Cell>) -> GateNetlist {
        let mut netlist = GateNetlist::new("top".to_string(), "nexus".to_string());
        for cell in cells {
            netlist.add_cell(cell);
        }
        netlist
    }

    #[test]
    fn an_led_may_be_driven() {
        let netlist = design(vec![pad("R8", CellFunction::OutputPad)]);
        assert!(check_outputs(&netlist, &CERTUSPRO_VERSA).is_ok());
    }

    #[test]
    fn a_switch_is_refused_and_named() {
        let netlist = design(vec![pad("AA23", CellFunction::OutputPad)]);
        let error = check_outputs(&netlist, &CERTUSPRO_VERSA).unwrap_err();
        assert!(error.contains("DIP switch 1"), "{error}");
    }

    #[test]
    fn an_unlisted_pin_is_refused() {
        let netlist = design(vec![pad("AF12", CellFunction::OutputPad)]);
        let error = check_outputs(&netlist, &CERTUSPRO_VERSA).unwrap_err();
        assert!(error.contains("not listed as safe"), "{error}");
    }

    #[test]
    fn the_same_pin_may_be_an_input() {
        let netlist = design(vec![pad("AA23", CellFunction::InputPad)]);
        assert!(check_outputs(&netlist, &CERTUSPRO_VERSA).is_ok());
    }

    #[test]
    fn a_stated_reason_overrides_the_check() {
        let mut cell = pad("AA23", CellFunction::OutputPad);
        cell.parameters.insert(
            DRIVE_OVERRIDE_PARAM.to_string(),
            "switch is open".to_string(),
        );
        assert!(check_outputs(&design(vec![cell]), &CERTUSPRO_VERSA).is_ok());
    }
}
