//! The analog-to-digital converter (`ADC_CORE`) of a Nexus device.
//!
//! The block holds two 12-bit SAR converters (A and B), each with its own
//! channel select, plus three comparators. It sits in a tile of type `ADC`;
//! its fabric pins are wires named `J<pin>_ADC_CORE_ADC0` one tile west of
//! that tile. Its settings are enums and one word (`ADC.CLK_DIV`) in the
//! tile.
//!
//! Two clocks drive it: `CLKDCLK`, the converters' clock, which only the
//! `CLKOS4` output of one corner's PLL reaches (a dedicated connection in
//! the device data), and `CLKFAB`, from the fabric, which times the control
//! and status pins.
//!
//! The analog inputs are not fabric pins. A converter's channel select picks
//! one of the device's ADC input pads (or an internal source) at run time;
//! a pad reaches the converters only when its own ADC input switch is on.
//! A design names the pads it measures in the instance's `ADC_PINS`
//! parameter (comma-separated pins), and the backend turns their switches
//! on.
//!
//! The block also has fabric inputs that Lattice's primitive does not
//! expose (on the LFCPNX, `RSVDH` and `RSVDL`). Lattice's tools tie them to
//! a constant; left unconnected, a fabric input reads 1, and with these high
//! the converters read mid-scale whatever they measure. The device data
//! lists them with their values (`<family>/adc_tied_inputs.json`), and the
//! backend adds the inputs that must be low to the cell, driven from a
//! constant-zero LUT ([`tied_low_pins`]).

use crate::device::FF_PIN_CLK;
use skalp_lir::gate_netlist::{Cell, CellId, GateNetlist};

/// Cell types that place the ADC: Lattice's primitive name and the block's.
const CELL_TYPES: [&str; 2] = ["ADC", "ADC_CORE"];

/// The ADC's fabric input pins as the device names them, in router pin
/// order.
const INPUT_PINS: &[&str] = &[
    "ADCEN", "CAL", "CHAEN", "CHASEL0", "CHASEL1", "CHASEL2", "CHASEL3", "CHBEN", "CHBSEL0",
    "CHBSEL1", "CHBSEL2", "CHBSEL3", "CLKDCLK", "CLKFAB", "CONVSTOP", "RESETN", "RSTN", "SOC",
    "RSVDH", "RSVDL",
];

/// The ADC's fabric output pins, in router pin order.
const OUTPUT_PINS: &[&str] = &[
    "CALRDY", "COG", "EOC", "COMP1O", "COMP2O", "COMP3O", "COMP1OL", "COMP2OL", "COMP3OL", "DA0",
    "DA1", "DA2", "DA3", "DA4", "DA5", "DA6", "DA7", "DA8", "DA9", "DA10", "DA11", "DB0", "DB1",
    "DB2", "DB3", "DB4", "DB5", "DB6", "DB7", "DB8", "DB9", "DB10", "DB11",
];

/// Ports of Lattice's primitive that are analog (dedicated pads, the
/// comparators' inputs, the GPIO channels): not fabric pins.
const ANALOG_PORTS: &[&str] = &[
    "DP0", "DN0", "DP1", "DN1", "COMP1IN", "COMP1IP", "COMP2IN", "COMP2IP", "COMP3IN", "COMP3IP",
    "GPIOP", "GPION",
];

/// The converter clock pin, which only a PLL's `CLKOS4` reaches.
pub const CONVERTER_CLOCK_PIN: &str = "CLKDCLK";

/// The parameter naming the pads a design measures.
pub const PINS_PARAM: &str = "ADC_PINS";

/// The tile type that holds the ADC and its settings.
pub const TILETYPE: &str = "ADC";

/// The ADC's enum settings with their defaults (Lattice's primitive
/// defaults) and the values each takes.
pub const ENUM_SETTINGS: &[(&str, &str, &[&str])] = &[
    ("ADC_ENP", "ENABLED", &["ENABLED", "DISABLED"]),
    ("EN_COMP1", "ENABLED", &["ENABLED", "DISABLED"]),
    ("EN_COMP2", "ENABLED", &["ENABLED", "DISABLED"]),
    ("EN_COMP3", "ENABLED", &["ENABLED", "DISABLED"]),
    ("REFBUFAEN", "ENABLED", &["ENABLED", "DISABLED"]),
    ("REFBUFBEN", "ENABLED", &["ENABLED", "DISABLED"]),
    ("VREFASEL", "INTERNAL", &["INTERNAL", "EXTERNAL"]),
    ("VREFBSEL", "INTERNAL", &["INTERNAL", "EXTERNAL"]),
    ("CTLCOMPSW1", "DISABLED", &["DISABLED", "ENABLED"]),
    ("CTLCOMPSW2", "DISABLED", &["DISABLED", "ENABLED"]),
    ("CTLCOMPSW3", "DISABLED", &["DISABLED", "ENABLED"]),
    (
        "DF",
        "STRAIGHT_BINARY",
        &["STRAIGHT_BINARY", "TWOS_COMPLEMENT"],
    ),
    ("OMA", "BIPOLAR", &["BIPOLAR", "UNIPOLAR"]),
    ("OMB", "BIPOLAR", &["BIPOLAR", "UNIPOLAR"]),
    ("SLEEP", "DISABLED", &["DISABLED", "ENABLED"]),
    (
        "VREFACFG",
        "1P0_TO_1P2",
        &["1P0_TO_1P2", "1P2_TO_1P4", "1P4_TO_1P6", "1P6_TO_1P8"],
    ),
    (
        "VREFBCFG",
        "1P0_TO_1P2",
        &["1P0_TO_1P2", "1P2_TO_1P4", "1P4_TO_1P6", "1P6_TO_1P8"],
    ),
];

/// The converter clock divider: its setting name, default and range. The
/// converters run at `CLKDCLK / CLK_DIV`.
pub const CLK_DIV: (&str, u32, std::ops::RangeInclusive<u32>) = ("CLK_DIV", 2, 0..=255);

/// Whether a cell is the ADC.
pub fn is_adc(cell: &Cell) -> bool {
    let name = cell
        .cell_type
        .strip_prefix("BLACKBOX_")
        .unwrap_or(&cell.cell_type);
    CELL_TYPES.contains(&name)
}

/// The device pin a primitive port names: a bus bit (`CHASEL[2]`, any
/// case) or a pin itself (`CHASEL2`).
fn device_pin(port: &str) -> String {
    let upper = port.to_ascii_uppercase();
    match upper.split_once('[') {
        Some((name, rest)) => format!("{name}{}", rest.trim_end_matches(']')),
        None => upper,
    }
}

/// Whether a port is one of the primitive's analog ports (any bit of one).
fn is_analog_port(port: &str) -> bool {
    let upper = port.to_ascii_uppercase();
    let name = upper.split('[').next().unwrap_or(&upper);
    ANALOG_PORTS.contains(&name)
}

/// The fabric clock pin, which takes its clock from the clock network (a
/// clock branch into the neighbouring tile's `JCLK0`), so it is routed as a
/// clock.
pub const FABRIC_CLOCK_PIN: &str = "CLKFAB";

/// Router pin number of the ADC's `index`-th input. The fabric clock is
/// [`FF_PIN_CLK`], which the clock router routes on the clock network.
pub fn input_router_pin(cell: &Cell, index: usize) -> Option<usize> {
    let pin = device_pin(cell.blackbox_pin(false, index)?);
    if pin == FABRIC_CLOCK_PIN {
        return Some(FF_PIN_CLK);
    }
    INPUT_PINS.iter().position(|&p| p == pin)
}

/// The device name of an input router pin.
pub fn input_pin_of_router_pin(pin: usize) -> Option<&'static str> {
    if pin == FF_PIN_CLK {
        return Some(FABRIC_CLOCK_PIN);
    }
    INPUT_PINS.get(pin).copied()
}

/// Router pin number of the ADC's `index`-th output.
pub fn output_router_pin(cell: &Cell, index: usize) -> Option<usize> {
    let pin = device_pin(cell.blackbox_pin(true, index)?);
    OUTPUT_PINS.iter().position(|&p| p == pin)
}

/// The device name of the input pin a cell input names, if it is an ADC
/// pin.
pub fn input_pin_name(cell: &Cell, index: usize) -> Option<&'static str> {
    input_router_pin(cell, index).and_then(input_pin_of_router_pin)
}

/// Input pins by router pin number.
pub fn input_pins() -> &'static [&'static str] {
    INPUT_PINS
}

/// Output pins by router pin number.
pub fn output_pins() -> &'static [&'static str] {
    OUTPUT_PINS
}

/// Name of a pin's wire from the ADC tile.
pub fn site_wire(pin: &str) -> String {
    format!("W1:J{pin}_ADC_CORE_ADC0")
}

/// The pads an ADC instance measures (`ADC_PINS`).
pub fn measured_pins(cell: &Cell) -> Vec<String> {
    cell.parameters
        .get(PINS_PARAM)
        .map(|pins| {
            pins.split(',')
                .map(|p| p.trim().to_string())
                .filter(|p| !p.is_empty())
                .collect()
        })
        .unwrap_or_default()
}

/// What is wrong with an ADC cell's ports and settings, for an error.
pub fn problems(cell: &Cell) -> Vec<String> {
    let mut problems = Vec::new();
    let mut unknown = Vec::new();
    let mut analog = Vec::new();
    for output in [false, true] {
        let known = if output { OUTPUT_PINS } else { INPUT_PINS };
        let mut index = 0;
        while let Some(port) = cell.blackbox_pin(output, index) {
            if is_analog_port(port) {
                analog.push(port.to_string());
            } else if !known.contains(&device_pin(port).as_str()) {
                unknown.push(port.to_string());
            }
            index += 1;
        }
    }
    if !unknown.is_empty() {
        problems.push(format!(
            "has ports the device's ADC does not: {}",
            unknown.join(", ")
        ));
    }
    if !analog.is_empty() {
        problems.push(format!(
            "connects analog ports ({}) as logic; name the pads to measure in its {PINS_PARAM} \
             parameter instead",
            analog.join(", ")
        ));
    }
    for &(name, _, values) in ENUM_SETTINGS {
        if let Some(value) = setting(cell, name) {
            if !values.contains(&value.as_str()) {
                problems.push(format!(
                    "{name} = {value} is not one of {}",
                    values.join(", ")
                ));
            }
        }
    }
    if let Some(value) = setting(cell, CLK_DIV.0) {
        match value.parse::<u32>() {
            Ok(v) if CLK_DIV.2.contains(&v) && v >= 2 && v % 2 == 0 => {}
            _ => problems.push(format!(
                "{} = {value} is not an even divider from 2 to {}",
                CLK_DIV.0,
                CLK_DIV.2.end()
            )),
        }
    }
    problems
}

/// A setting an instance gives, without quotes and in upper case.
pub fn setting(cell: &Cell, name: &str) -> Option<String> {
    cell.parameters
        .get(name)
        .map(|v| v.trim().trim_matches('"').to_ascii_uppercase())
}

/// The value an instance runs a setting at: its own, else the default.
pub fn enum_value(cell: &Cell, name: &str) -> Option<String> {
    let &(_, default, _) = ENUM_SETTINGS.iter().find(|(n, _, _)| *n == name)?;
    Some(setting(cell, name).unwrap_or_else(|| default.to_string()))
}

/// The converter clock divider an instance runs at.
pub fn clock_divider(cell: &Cell) -> u32 {
    setting(cell, CLK_DIV.0)
        .and_then(|v| v.parse().ok())
        .unwrap_or(CLK_DIV.1)
}

/// One fabric input of the ADC block that Lattice's primitive does not
/// expose, as the device data describes it.
#[derive(serde::Deserialize)]
struct TiedInput {
    /// The constant Lattice's tools tie it to
    value: u8,
}

/// The ADC's unexposed fabric inputs that must be driven low, from the
/// device data (`<family>/adc_tied_inputs.json`), in name order. Inputs
/// tied high need nothing: an unconnected fabric input reads 1. A family
/// without the file has none.
pub fn tied_low_pins(db_root: &std::path::Path, family: &str) -> Result<Vec<String>, String> {
    let path = db_root.join(family).join("adc_tied_inputs.json");
    let text = match std::fs::read_to_string(&path) {
        Ok(text) => text,
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => return Ok(Vec::new()),
        Err(e) => return Err(format!("{}: {e}", path.display())),
    };
    let file: std::collections::BTreeMap<String, serde_json::Value> =
        serde_json::from_str(&text).map_err(|e| format!("{}: {e}", path.display()))?;
    let Some(block) = file.get("ADC_CORE") else {
        return Ok(Vec::new());
    };
    let inputs: std::collections::BTreeMap<String, TiedInput> =
        serde_json::from_value(block.clone()).map_err(|e| format!("{}: {e}", path.display()))?;
    let mut low = Vec::new();
    for (pin, input) in inputs {
        if !INPUT_PINS.contains(&pin.as_str()) {
            return Err(format!(
                "{}: ADC input {pin} is not a pin this backend knows",
                path.display()
            ));
        }
        match input.value {
            0 => low.push(pin),
            1 => {}
            v => {
                return Err(format!(
                    "{}: ADC input {pin} is tied to {v}, not 0 or 1",
                    path.display()
                ))
            }
        }
    }
    Ok(low)
}

/// Whether a cell connects an input to a device pin itself.
pub fn connects(cell: &Cell, pin: &str) -> bool {
    let mut index = 0;
    while let Some(port) = cell.blackbox_pin(false, index) {
        if device_pin(port) == pin {
            return true;
        }
        index += 1;
    }
    false
}

/// The ADC cell of a design, checked: at most one, with known ports and
/// settings. A device has one ADC block (with both converters).
pub fn the_adc(netlist: &GateNetlist) -> Result<Option<CellId>, String> {
    let mut adcs = netlist.cells.iter().filter(|c| is_adc(c));
    let Some(adc) = adcs.next() else {
        return Ok(None);
    };
    if let Some(other) = adcs.next() {
        return Err(format!(
            "ADCs '{}' and '{}': the device has one ADC block (it holds both converters)",
            adc.path, other.path
        ));
    }
    let problems = problems(adc);
    if !problems.is_empty() {
        return Err(format!("ADC '{}' {}", adc.path, problems.join("; ")));
    }
    Ok(Some(adc.id))
}

#[cfg(test)]
mod tests {
    use super::*;
    use skalp_lir::gate_netlist::{BLACKBOX_INPUTS_PARAM, BLACKBOX_OUTPUTS_PARAM};

    fn adc(inputs: &str, outputs: &str) -> Cell {
        let n_in = inputs.split(',').filter(|s| !s.is_empty()).count();
        let n_out = outputs.split(',').filter(|s| !s.is_empty()).count();
        let mut cell = Cell::new_comb(
            CellId(0),
            "BLACKBOX_ADC".into(),
            "blackbox".into(),
            0.0,
            "top.adc".into(),
            (0..n_in)
                .map(|i| skalp_lir::gate_netlist::GateNetId(i as u32))
                .collect(),
            (0..n_out)
                .map(|i| skalp_lir::gate_netlist::GateNetId(100 + i as u32))
                .collect(),
        );
        cell.parameters
            .insert(BLACKBOX_INPUTS_PARAM.into(), inputs.into());
        cell.parameters
            .insert(BLACKBOX_OUTPUTS_PARAM.into(), outputs.into());
        cell
    }

    #[test]
    fn tied_inputs_come_from_the_device_data() {
        let dir = std::env::temp_dir().join(format!("skalp_adc_tied_{}", std::process::id()));
        std::fs::create_dir_all(dir.join("FAM")).unwrap();
        std::fs::write(
            dir.join("FAM/adc_tied_inputs.json"),
            r#"{"_comment": "x", "ADC_CORE": {"RSVDL": {"value": 0, "pin_wire": "w"},
                "RSVDH": {"value": 0}}}"#,
        )
        .unwrap();
        assert_eq!(tied_low_pins(&dir, "FAM").unwrap(), ["RSVDH", "RSVDL"]);
        assert!(tied_low_pins(&dir, "OTHER").unwrap().is_empty());
        std::fs::write(
            dir.join("FAM/adc_tied_inputs.json"),
            r#"{"ADC_CORE": {"RSVDH": {"value": 1}, "RSVDL": {"value": 0}}}"#,
        )
        .unwrap();
        assert_eq!(tied_low_pins(&dir, "FAM").unwrap(), ["RSVDL"]);
        std::fs::write(
            dir.join("FAM/adc_tied_inputs.json"),
            r#"{"ADC_CORE": {"NOPE": {"value": 0}}}"#,
        )
        .unwrap();
        assert!(tied_low_pins(&dir, "FAM").is_err());
        std::fs::remove_dir_all(&dir).unwrap();
    }

    #[test]
    fn a_design_may_connect_a_tied_input_itself() {
        let cell = adc("SOC,RSVDH", "EOC");
        assert!(connects(&cell, "RSVDH"));
        assert!(!connects(&cell, "RSVDL"));
        assert!(problems(&cell).is_empty(), "{:?}", problems(&cell));
    }

    #[test]
    fn bus_ports_map_to_device_pins() {
        let cell = adc("CHBSEL[3],SOC,clkdclk,CLKFAB", "DB[11],EOC");
        assert!(is_adc(&cell));
        assert_eq!(input_pin_name(&cell, 0), Some("CHBSEL3"));
        assert_eq!(input_pin_name(&cell, 1), Some("SOC"));
        assert_eq!(input_pin_name(&cell, 2), Some("CLKDCLK"));
        // The fabric clock is routed as a clock
        assert_eq!(input_router_pin(&cell, 3), Some(FF_PIN_CLK));
        assert_eq!(input_pin_name(&cell, 3), Some("CLKFAB"));
        assert_eq!(output_router_pin(&cell, 0), Some(OUTPUT_PINS.len() - 1));
        assert_eq!(output_router_pin(&cell, 1), Some(2));
        assert!(problems(&cell).is_empty());
        assert_eq!(site_wire("SOC"), "W1:JSOC_ADC_CORE_ADC0");
    }

    #[test]
    fn analog_ports_unknown_ports_and_bad_settings_are_reported() {
        let mut cell = adc("GPIOP[14],SOC,FOO", "EOC");
        cell.parameters.insert("OMB".into(), "\"SIDEWAYS\"".into());
        cell.parameters.insert("CLK_DIV".into(), "3".into());
        let problems = problems(&cell).join("; ");
        assert!(problems.contains("FOO"), "{problems}");
        assert!(problems.contains("GPIOP[14]"), "{problems}");
        assert!(problems.contains("SIDEWAYS"), "{problems}");
        assert!(problems.contains("even divider"), "{problems}");
    }

    #[test]
    fn settings_default_to_the_primitives() {
        let mut cell = adc("SOC", "EOC");
        assert_eq!(enum_value(&cell, "OMB").as_deref(), Some("BIPOLAR"));
        assert_eq!(clock_divider(&cell), 2);
        cell.parameters.insert("OMB".into(), "\"unipolar\"".into());
        cell.parameters.insert("CLK_DIV".into(), "8".into());
        cell.parameters.insert(PINS_PARAM.into(), "Y6, AA6".into());
        assert_eq!(enum_value(&cell, "OMB").as_deref(), Some("UNIPOLAR"));
        assert_eq!(clock_divider(&cell), 8);
        assert_eq!(measured_pins(&cell), ["Y6", "AA6"]);
    }

    #[test]
    fn a_design_has_at_most_one_adc() {
        let mut netlist = GateNetlist::new("top".into(), "nexus".into());
        assert_eq!(the_adc(&netlist), Ok(None));
        let id = netlist.add_cell(adc("SOC", "EOC"));
        assert_eq!(the_adc(&netlist), Ok(Some(id)));
        netlist.add_cell(adc("SOC", "EOC"));
        assert!(the_adc(&netlist).unwrap_err().contains("one ADC block"));
    }
}
