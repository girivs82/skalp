//! Block RAM (EBR) sites of a Nexus device.
//!
//! Each site occupies two configuration tiles in a row and exposes its pins
//! through wires named `J<pin>_EBR_CORE`, one tile up and to the left of the
//! first tile. The RAM runs in pseudo dual-port mode, the only one offering
//! every aspect ratio: writes through port A, reads through port B, and the
//! write enable on port A's clock enable, as in Lattice's `PDP16K` primitive.

use crate::device::{FF_PIN_CLK, RAM_PIN_READ_CLK};
use skalp_lir::gate_netlist::RamPorts;
use std::sync::LazyLock;

/// First address bit a port uses at each data width; the bits below it select
/// bytes or are ignored.
const ADDRESS_LSB: [(usize, usize); 6] = [(1, 0), (2, 1), (4, 2), (9, 3), (18, 4), (36, 5)];
/// First router pin number of a site's non-clock inputs.
const RAM_PIN_BASE: usize = 300;
/// Data bits on one port; the widest mode uses both ports for a single word.
const PORT_DATA_BITS: usize = 18;

/// Tile type holding each site's pins, in site order.
const SITE_TILES: [&str; 4] = ["EBR_1", "EBR_4", "EBR_7", "EBR_9"];
/// Tile type suffix of the second tile of each site.
const SECOND_TILES: [&str; 4] = ["EBR_2", "EBR_5", "EBR_8", "EBR_10"];

/// A site's input pins other than the clocks, in router pin order.
static INPUT_PINS: LazyLock<Vec<String>> = LazyLock::new(|| {
    fn bus(name: &'static str, count: usize) -> impl Iterator<Item = String> {
        (0..count).map(move |i| format!("{name}{i}"))
    }
    ["CEA", "CEB", "WEA", "WEB", "RSTA", "RSTB"]
        .into_iter()
        .map(String::from)
        .chain(bus("CSA", 3))
        .chain(bus("CSB", 3))
        .chain(bus("DWS", 5))
        .chain(bus("ADA", 14))
        .chain(bus("ADB", 14))
        .chain(bus("DIA", PORT_DATA_BITS))
        .chain(bus("DIB", PORT_DATA_BITS))
        .collect()
});

/// The site number of a tile that holds a block RAM's pins.
pub fn site_of_tile(tiletype: &str) -> Option<usize> {
    SITE_TILES.iter().position(|&t| t == tiletype)
}

/// Tile type suffix of a site's second tile.
pub fn second_tile_suffix(site: usize) -> &'static str {
    SECOND_TILES[site]
}

/// Every input pin of a site, without its clocks.
pub fn input_pins() -> &'static [String] {
    &INPUT_PINS
}

/// Name of the site wire for a pin, from the tile holding the site's pins.
pub fn site_wire(pin: &str) -> String {
    format!("N1W1:J{pin}_EBR_CORE")
}

/// Names of a site's data outputs, indexed by output bit.
pub fn output_pin(bit: usize) -> String {
    if bit < PORT_DATA_BITS {
        format!("DOB{bit}")
    } else {
        format!("DOA{}", bit - PORT_DATA_BITS)
    }
}

/// First address bit used at a data width, or `None` for an unsupported width.
fn address_lsb(width: usize) -> Option<usize> {
    ADDRESS_LSB
        .iter()
        .find(|&&(w, _)| w == width)
        .map(|&(_, lsb)| lsb)
}

/// The site pin a cell input connects to, or `None` if it has none.
pub fn input_pin(ports: &RamPorts, input: usize) -> Option<String> {
    let lsb = address_lsb(ports.data_width())?;
    let bus_bit =
        |range: &std::ops::Range<usize>| range.contains(&input).then(|| input - range.start);
    if let Some(bit) = bus_bit(&ports.read_addr) {
        return Some(format!("ADB{}", bit + lsb));
    }
    if let Some(bit) = bus_bit(&ports.write_addr) {
        return Some(format!("ADA{}", bit + lsb));
    }
    if let Some(bit) = bus_bit(&ports.write_data) {
        return Some(if bit < PORT_DATA_BITS {
            format!("DIA{bit}")
        } else {
            format!("DIB{}", bit - PORT_DATA_BITS)
        });
    }
    let name = if input == ports.read_clk {
        "CLKB"
    } else if input == ports.write_clk {
        "CLKA"
    } else if input == ports.read_clk_en {
        "CEB"
    } else if input == ports.read_en {
        "CSB0"
    } else if input == ports.write_en {
        "CEA"
    } else {
        return None;
    };
    Some(name.to_string())
}

/// Router pin number of a site pin.
pub fn router_pin(pin: &str) -> Option<usize> {
    match pin {
        "CLKA" => Some(FF_PIN_CLK),
        "CLKB" => Some(RAM_PIN_READ_CLK),
        _ => INPUT_PINS
            .iter()
            .position(|p| p == pin)
            .map(|i| i + RAM_PIN_BASE),
    }
}

/// Site pin of a router pin number.
pub fn pin_of_router_pin(pin: usize) -> Option<&'static str> {
    match pin {
        FF_PIN_CLK => Some("CLKA"),
        RAM_PIN_READ_CLK => Some("CLKB"),
        _ => INPUT_PINS
            .get(pin.checked_sub(RAM_PIN_BASE)?)
            .map(String::as_str),
    }
}

/// Whether a pin has a configurable inverter, which can hold it low.
pub fn has_inverter(pin: &str) -> bool {
    match pin {
        "CEA" | "CEB" | "WEA" | "WEB" | "RSTA" | "RSTB" => true,
        _ => {
            let bit = |prefix: &str| pin.strip_prefix(prefix)?.parse::<usize>().ok();
            bit("ADA").is_some_and(|n| n < 4) || bit("ADB").is_some_and(|n| n < 2)
        }
    }
}

/// Whether a pin floats high only once its CIB input mux is set.
pub fn floats_through_cib(pin: &str) -> bool {
    !matches!(pin, "CEA" | "CEB" | "WEA" | "WEB" | "RSTA" | "RSTB") && !pin.starts_with("CS")
}

#[cfg(test)]
mod tests {
    use super::*;

    fn ports(addr: usize, data: usize) -> RamPorts {
        let read_clk = addr;
        let write_data = read_clk + 3..read_clk + 3 + data;
        let write_addr = write_data.end..write_data.end + addr;
        let write_clk = write_addr.end;
        RamPorts {
            read_addr: 0..addr,
            read_clk,
            read_clk_en: read_clk + 1,
            read_en: read_clk + 2,
            write_data,
            write_addr,
            write_clk,
            write_clk_en: write_clk + 1,
            write_en: write_clk + 2,
            write_mask: 0..0,
        }
    }

    #[test]
    fn addresses_start_above_the_ignored_low_bits() {
        let wide = ports(10, 18);
        assert_eq!(input_pin(&wide, 0).unwrap(), "ADB4");
        assert_eq!(input_pin(&wide, wide.write_addr.start).unwrap(), "ADA4");
        let narrow = ports(11, 9);
        assert_eq!(input_pin(&narrow, 0).unwrap(), "ADB3");
    }

    #[test]
    fn the_widest_mode_spans_both_ports() {
        let wide = ports(9, 36);
        assert_eq!(input_pin(&wide, wide.write_data.start).unwrap(), "DIA0");
        assert_eq!(
            input_pin(&wide, wide.write_data.start + 18).unwrap(),
            "DIB0"
        );
        assert_eq!(output_pin(18), "DOA0");
    }

    #[test]
    fn write_enable_is_the_write_port_clock_enable() {
        let p = ports(10, 18);
        assert_eq!(input_pin(&p, p.write_en).unwrap(), "CEA");
        assert_eq!(input_pin(&p, p.write_clk_en), None);
    }

    #[test]
    fn router_pins_round_trip() {
        for pin in input_pins()
            .iter()
            .map(String::as_str)
            .chain(["CLKA", "CLKB"])
        {
            assert_eq!(pin_of_router_pin(router_pin(pin).unwrap()), Some(pin));
        }
    }

    #[test]
    fn only_low_address_bits_have_inverters() {
        assert!(has_inverter("ADA3"));
        assert!(!has_inverter("ADA4"));
        assert!(has_inverter("ADB1"));
        assert!(!has_inverter("ADB2"));
    }
}
