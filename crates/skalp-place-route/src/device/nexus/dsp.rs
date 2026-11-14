//! Multiplier sites of a Nexus device.
//!
//! One `MULT18X18` is built from six hard blocks: two 9-bit pre-adders, two
//! 9-bit multipliers, the 18-bit combiner and two output registers. The blocks
//! connect to each other through wires with no configuration, so only their
//! settings are written; the fabric reaches the A and B inputs, the sign inputs
//! and the Z outputs. Input and output registers are bypassed.

use skalp_lir::gate_netlist::Cell;

/// Tile number along a DSP row holding each multiplier slot.
const SLOT_TILES: [u32; 4] = [1, 2, 5, 6];
/// Output register group for each slot, and the tile number holding it.
const SLOT_REGISTERS: [(&str, u32); 4] = [("L0", 3), ("L1", 3), ("H0", 7), ("H1", 7)];

/// Number of inputs on each operand of the multiplier.
pub const OPERAND_BITS: usize = 18;
/// Number of output bits.
pub const PRODUCT_BITS: usize = 36;
/// First router pin number of a multiplier's inputs.
const DSP_PIN_BASE: usize = 400;
/// Router pin number of the A sign input, after the A and B operand pins.
const DSP_PIN_ASIGNED: usize = DSP_PIN_BASE + 2 * OPERAND_BITS;
/// Router pin number of the B sign input.
const DSP_PIN_BSIGNED: usize = DSP_PIN_ASIGNED + 1;

use crate::device::EXTRA_INPUTS_PARAM;

/// Inputs of the hard blocks the design does not use. Left alone they would
/// float high, which makes the low halves signed and disturbs the product.
pub fn low_driven_pins() -> Vec<String> {
    let mut pins = vec!["LOWASIGNED".to_string(), "LOWBSIGNED".to_string()];
    pins.extend((0..20).map(|n| format!("C{n}")));
    pins.push("ROUNDEN".to_string());
    pins
}

/// Shift control inputs of the combiner, which must read high when unused.
pub const SHIFT_CONTROL_PINS: usize = 4;

/// Settings of each pre-adder, in bypass.
pub const PREADD_SETTINGS: &[(&str, &str)] = &[
    ("MODE", "PREADD9_CORE"),
    ("SUBSTRACT_EN", "SUBTRACTION"),
    ("SR_18BITSHIFT_EN", "DISABLED"),
    ("SHIFTBR", "REGISTER"),
    ("SHIFTBL", "BYPASS"),
    ("RESET", "SYNC"),
    ("REGBYPSBR1", "BYPASS"),
    ("REGBYPSBR0", "BYPASS"),
    ("REGBYPSBL", "REGISTER"),
    ("PREADDCAS_EN", "DISABLED"),
    ("OPC", "INPUT_B_AS_PREADDER_OPERAND"),
    ("GSR", "DISABLED"),
    ("CSIGNED", "DISABLED"),
    ("BYPASS_PREADD9", "BYPASS"),
    ("SIGNEDSTATIC_EN", "DISABLED"),
    ("RSTCLMUX", "INV"),
    ("RSTBMUX", "INV"),
    ("CLKMUX", "CLK"),
];

/// Settings of each 9-bit multiplier, with its registers bypassed.
pub const MULT9_SETTINGS: &[(&str, &str)] = &[
    ("MODE", "MULT9_CORE"),
    ("SR_18BITSHIFT_EN", "DISABLED"),
    ("SIGNEDSTATIC_EN", "DISABLED"),
    ("SHIFTA", "DISABLED"),
    ("RESET", "SYNC"),
    ("REGBYPSB", "BYPASS"),
    ("REGBYPSA2", "BYPASS"),
    ("REGBYPSA1", "BYPASS"),
    ("GSR", "DISABLED"),
    ("BYPASS_MULT9", "USED"),
    ("ASIGNED_OPERAND_EN", "DISABLED"),
    ("RSTPMUX", "INV"),
    ("RSTAMUX", "INV"),
    ("CLKMUX", "CLK"),
];

/// Settings of the 18-bit combiner.
pub const MULT18_SETTINGS: &[(&str, &str)] = &[
    ("SFTEN", "DISABLED"),
    ("ROUNDRTZI", "ROUND_TO_ZERO"),
    ("ROUNDHALFUP", "DISABLED"),
    ("MULT18X18", "ENABLED"),
];

/// Settings of each output register, in bypass.
pub const REG18_SETTINGS: &[(&str, &str)] = &[
    ("MODE", "REG18_CORE"),
    ("RESET", "SYNC"),
    ("REGBYPS", "BYPASS"),
    ("GSR", "DISABLED"),
    ("RSTPMUX", "INV"),
    ("CLKMUX", "CLK"),
];

/// Settings every unused pre-adder must hold.
pub const UNUSED_PREADD_SETTINGS: &[(&str, &str)] = &[
    ("GSR", "ENABLED"),
    ("MODE", "NONE"),
    ("RSTBMUX", "RSTB"),
    ("RSTCLMUX", "RSTCL"),
];
/// Settings every unused 9-bit multiplier must hold.
pub const UNUSED_MULT9_SETTINGS: &[(&str, &str)] = &[
    ("GSR", "ENABLED"),
    ("MODE", "NONE"),
    ("RSTAMUX", "RSTA"),
    ("RSTPMUX", "RSTP"),
];
/// Settings every unused output register must hold.
pub const UNUSED_REG18_SETTINGS: &[(&str, &str)] =
    &[("GSR", "ENABLED"), ("MODE", "NONE"), ("RSTPMUX", "RSTP")];

/// Number of multiplier slots in a DSP row.
pub const SLOTS: usize = 4;

/// Settings of the accumulator a slot's product passes through to the fabric.
pub const ACC54_SETTINGS: &[(&str, &str)] = &[("ACCUBYPS", "BYPASS"), ("MODE", "NONE")];

/// Name of the accumulator a slot's product passes through.
pub fn acc54(slot: usize) -> String {
    format!("ACC54_{}", usize::from(slot >= 2))
}

/// The slot a tile holds, from its type, such as `DSP_R_6`.
pub fn slot_of_tile(tiletype: &str) -> Option<usize> {
    let number: u32 = tiletype
        .strip_prefix("DSP_L_")
        .or_else(|| tiletype.strip_prefix("DSP_R_"))?
        .parse()
        .ok()?;
    SLOT_TILES.iter().position(|&tile| tile == number)
}

/// The tiles of a slot's DSP row all sit to either side of its first tile.
pub fn row_base_column(tiletype: &str, column: u32) -> Option<u32> {
    let number: u32 = tiletype
        .strip_prefix("DSP_L_")
        .or_else(|| tiletype.strip_prefix("DSP_R_"))?
        .parse()
        .ok()?;
    column.checked_sub(number)
}

/// Whether a DSP tile belongs to a slot's row, given the row's base column.
pub fn tile_in_row(tiletype: &str, column: u32, base: u32) -> bool {
    row_base_column(tiletype, column) == Some(base)
}

/// Column of a slot's output registers, from the column of its own tile.
pub fn register_column(slot: usize, slot_column: u32) -> u32 {
    slot_column + SLOT_REGISTERS[slot].1 - SLOT_TILES[slot]
}

/// Name of one of a slot's two pre-adders (`half` 0 is the low nine bits).
pub fn preadd(slot: usize, half: usize) -> String {
    format!("PREADD9_{}{slot}", ['L', 'H'][half])
}

/// Name of one of a slot's two 9-bit multipliers.
pub fn mult9(slot: usize, half: usize) -> String {
    format!("MULT9_{}{slot}", ['L', 'H'][half])
}

/// Name of a slot's 18-bit combiner.
pub fn mult18(slot: usize) -> String {
    format!("MULT18_{slot}")
}

/// Name of one of a slot's two output registers.
pub fn reg18(slot: usize, index: usize) -> String {
    format!("REG18_{}_{index}", SLOT_REGISTERS[slot].0)
}

/// Site wire of an input pin, from the slot's own tile.
pub fn input_wire(slot: usize, pin: &str) -> Option<String> {
    let wire = |core: &str, bel: String, pin: String| format!("N1:J{pin}_{core}_{bel}");
    let bus = |name: &str| pin.strip_prefix(name)?.parse::<usize>().ok();
    if let Some(bit) = bus("A") {
        return Some(wire(
            "MULT9_CORE",
            mult9(slot, bit / 9),
            format!("A{}", bit % 9),
        ));
    }
    if let Some(bit) = bus("B") {
        return Some(wire(
            "PREADD9_CORE",
            preadd(slot, bit / 9),
            format!("B{}", bit % 9),
        ));
    }
    if let Some(bit) = bus("C") {
        return Some(wire(
            "PREADD9_CORE",
            preadd(slot, bit / 10),
            format!("C{}", bit % 10),
        ));
    }
    match pin {
        "ASIGNED" => Some(wire("MULT9_CORE", mult9(slot, 1), "ASIGNED".to_string())),
        "BSIGNED" => Some(wire("PREADD9_CORE", preadd(slot, 1), "BSIGNED".to_string())),
        "LOWASIGNED" => Some(wire("MULT9_CORE", mult9(slot, 0), "ASIGNED".to_string())),
        "LOWBSIGNED" => Some(wire("PREADD9_CORE", preadd(slot, 0), "BSIGNED".to_string())),
        "ROUNDEN" => Some(wire("MULT18_CORE", mult18(slot), "ROUNDEN".to_string())),
        _ => {
            let bit = pin.strip_prefix("SFTCTRL")?;
            Some(wire("MULT18_CORE", mult18(slot), format!("SFTCTRL{bit}")))
        }
    }
}

/// Site wire of a product bit, from the column of the slot's output registers.
pub fn product_wire(slot: usize, bit: usize) -> String {
    format!(
        "N1:JPP{}_REG18_CORE_{}",
        bit % OPERAND_BITS,
        reg18(slot, bit / OPERAND_BITS)
    )
}

/// Whether a cell is a multiplier this backend can build.
pub fn is_multiplier(cell: &Cell) -> bool {
    cell.dsp_ports()
        .is_some_and(|p| p.a.len() == OPERAND_BITS && p.b.len() == OPERAND_BITS)
}

/// The site pin an input of a multiplier connects to, if any.
pub fn input_pin(cell: &Cell, input: usize) -> Option<String> {
    if let Some(first) = cell
        .parameters
        .get(EXTRA_INPUTS_PARAM)
        .and_then(|v| v.parse::<usize>().ok())
    {
        if input >= first {
            return low_driven_pins().get(input - first).cloned();
        }
    }
    let ports = cell.dsp_ports()?;
    if ports.a.contains(&input) {
        Some(format!("A{}", input - ports.a.start))
    } else if ports.b.contains(&input) {
        Some(format!("B{}", input - ports.b.start))
    } else if input == ports.signed_a {
        Some("ASIGNED".to_string())
    } else if input == ports.signed_b {
        Some("BSIGNED".to_string())
    } else {
        None
    }
}

/// Router pin number of a multiplier site pin.
pub fn router_pin(pin: &str) -> Option<usize> {
    let bus = |name: &str| pin.strip_prefix(name)?.parse::<usize>().ok();
    if let Some(bit) = bus("A") {
        return Some(DSP_PIN_BASE + bit);
    }
    if let Some(bit) = bus("B") {
        return Some(DSP_PIN_BASE + OPERAND_BITS + bit);
    }
    match pin {
        "ASIGNED" => Some(DSP_PIN_ASIGNED),
        "BSIGNED" => Some(DSP_PIN_BSIGNED),
        _ => low_driven_pins()
            .iter()
            .position(|p| p == pin)
            .map(|i| DSP_PIN_BSIGNED + 1 + i),
    }
}

/// Site pin of a router pin number.
pub fn pin_of_router_pin(pin: usize) -> Option<String> {
    let offset = pin.checked_sub(DSP_PIN_BASE)?;
    match offset {
        n if n < OPERAND_BITS => Some(format!("A{n}")),
        n if n < 2 * OPERAND_BITS => Some(format!("B{}", n - OPERAND_BITS)),
        _ if pin == DSP_PIN_ASIGNED => Some("ASIGNED".to_string()),
        _ if pin == DSP_PIN_BSIGNED => Some("BSIGNED".to_string()),
        _ => low_driven_pins().get(pin - DSP_PIN_BSIGNED - 1).cloned(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn slots_come_from_tile_numbers() {
        assert_eq!(slot_of_tile("DSP_R_6"), Some(3));
        assert_eq!(slot_of_tile("DSP_L_1"), Some(0));
        assert_eq!(slot_of_tile("DSP_R_3"), None);
        assert_eq!(slot_of_tile("PLC"), None);
    }

    #[test]
    fn operands_split_into_nine_bit_halves() {
        assert_eq!(input_wire(3, "A9").unwrap(), "N1:JA0_MULT9_CORE_MULT9_H3");
        assert_eq!(
            input_wire(3, "B17").unwrap(),
            "N1:JB8_PREADD9_CORE_PREADD9_H3"
        );
        assert_eq!(product_wire(3, 18), "N1:JPP0_REG18_CORE_REG18_H1_1");
    }

    #[test]
    fn registers_sit_in_their_own_column() {
        assert_eq!(register_column(3, 152), 153);
        assert_eq!(register_column(0, 100), 102);
    }

    #[test]
    fn router_pins_round_trip() {
        for pin in [
            "A0", "A17", "B0", "B17", "ASIGNED", "BSIGNED", "C7", "ROUNDEN",
        ] {
            assert_eq!(pin_of_router_pin(router_pin(pin).unwrap()).unwrap(), pin);
        }
    }

    #[test]
    fn inputs_map_to_pins_and_the_rest_are_unused() {
        let mut cell = Cell::new_comb(
            skalp_lir::gate_netlist::CellId(0),
            "MULT18X18".to_string(),
            "nexus".to_string(),
            0.0,
            "m".to_string(),
            vec![skalp_lir::gate_netlist::GateNetId(0); 130],
            Vec::new(),
        );
        for (name, value) in [
            (EXTRA_INPUTS_PARAM, "126"),
            (skalp_lir::gate_netlist::DSP_A_BITS_PARAM, "18"),
            (skalp_lir::gate_netlist::DSP_B_BITS_PARAM, "18"),
            (skalp_lir::gate_netlist::DSP_P_BITS_PARAM, "36"),
        ] {
            cell.parameters.insert(name.to_string(), value.to_string());
        }
        assert_eq!(input_pin(&cell, 17).unwrap(), "A17");
        assert_eq!(input_pin(&cell, 18).unwrap(), "B0");
        assert_eq!(input_pin(&cell, 54).unwrap(), "ASIGNED");
        assert_eq!(input_pin(&cell, 36), None);
        assert_eq!(input_pin(&cell, 126).unwrap(), "LOWASIGNED");
        assert_eq!(input_pin(&cell, 128).unwrap(), "C0");
    }
}
