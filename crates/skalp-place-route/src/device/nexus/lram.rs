//! Large RAM (LRAM) sites of a Nexus device.
//!
//! An LRAM is a 512 Kbit block: 16,384 words of 32 bits, one clock, two
//! ports. Each site is one configuration tile (`LRAM_<n>`, or `LRAM_<n>_15K`
//! on the smallest CrossLink-NX) whose pins are wires named
//! `J<pin>_LRAM_CORE`, one step away from the tile; the step differs between
//! sites and devices, so it is found from the device data.
//!
//! The RAM runs as Lattice's `PDPSC512K`: writes through port A, reads
//! through port B, both on the one clock. The site's contents are written on
//! the configuration bus at the region the device data names after the
//! site's pin location, `LRAM_CORE_R<row>C<column>`.

use crate::device::FF_PIN_CLK;
use skalp_lir::gate_netlist::{Cell, RamPorts};
use std::sync::LazyLock;

/// Library cell types that are large RAMs.
pub const CELL_TYPES: [&str; 4] = ["PDPSC512K", "SP512K", "DPSC512K", "LRAM_CORE"];
/// Data bits on one port.
pub const DATA_BITS: usize = 32;
/// Address bits on one port.
pub const ADDRESS_BITS: usize = 14;
/// First router pin number of a site's non-clock inputs.
const PIN_BASE: usize = 300;

/// Steps from a site's tile to the location of its pins, tried in order.
pub const PIN_STEPS: [&str; 12] = [
    "W1", "E1", "N1", "S1", "N1W1", "S1W1", "N1E1", "S1E1", "N5W1", "N4W1", "N3W1", "N2W1",
];

/// Inputs that must read low and read high when left floating, so they are
/// driven low by a constant from the fabric: the byte enables (active low)
/// of both ports, and deep power save (active high), which Radiant drives
/// from a constant-0 net in every design.
const LOW_PINS: [&str; 9] = [
    "BENA_N0", "BENA_N1", "BENA_N2", "BENA_N3", "BENB_N0", "BENB_N1", "BENB_N2", "BENB_N3", "DPS",
];

/// Inputs that must read high, which they do when their CIB input mux is
/// left floating. Radiant ties these to VCC in every design (standby,
/// init, ignore and the test pins among them).
const HIGH_PINS: [&str; 9] = [
    "CSA", "CSB", "OCEA", "OCEB", "IGN", "INITN", "STDBYN", "TBISTN", "SCANRST",
];

/// A site's input pins other than its clock, in router pin order.
static INPUT_PINS: LazyLock<Vec<String>> = LazyLock::new(|| {
    fn bus(name: &'static str, count: usize) -> impl Iterator<Item = String> {
        (0..count).map(move |i| format!("{name}{i}"))
    }
    ["CEA", "CEB", "WEA", "WEB", "RSTA", "RSTB"]
        .into_iter()
        .chain(HIGH_PINS)
        .map(String::from)
        .chain(bus("ADA", ADDRESS_BITS))
        .chain(bus("ADB", ADDRESS_BITS))
        .chain(bus("DIA", DATA_BITS))
        .chain(bus("DIB", DATA_BITS))
        .chain(LOW_PINS.into_iter().map(String::from))
        .collect()
});

/// Whether a cell is a large RAM.
pub fn is_large_ram(cell: &Cell) -> bool {
    cell.ram_ports().is_some() && CELL_TYPES.contains(&cell.cell_type.as_str())
}

/// The site number of a large RAM tile (`LRAM_3`, `LRAM_0_15K`).
pub fn site_of_tile(tiletype: &str) -> Option<usize> {
    let rest = tiletype.strip_prefix("LRAM_")?;
    let number = rest.split('_').next()?;
    if number.is_empty() || !number.bytes().all(|b| b.is_ascii_digit()) {
        return None;
    }
    let tail = &rest[number.len()..];
    (tail.is_empty() || tail.starts_with('_'))
        .then(|| number.parse().ok())
        .flatten()
}

/// Name of the BEL of site `site` in its tile's settings (`LRAM3`).
pub fn bel_name(site: usize) -> String {
    format!("LRAM{site}")
}

/// Name of the wire for a pin, from the site's tile through `step`.
pub fn site_wire(step: &str, pin: &str) -> String {
    format!("{step}:J{pin}_LRAM_CORE")
}

/// Column and row offsets of a step name (`S1E1` is one row down, one column
/// right).
pub fn step_offset(step: &str) -> (i64, i64) {
    let mut dx = 0;
    let mut dy = 0;
    let bytes = step.as_bytes();
    let mut i = 0;
    while i < bytes.len() {
        let dir = bytes[i];
        let start = i + 1;
        let mut end = start;
        while end < bytes.len() && bytes[end].is_ascii_digit() {
            end += 1;
        }
        let n: i64 = step[start..end].parse().unwrap_or(0);
        match dir {
            b'N' => dy -= n,
            b'S' => dy += n,
            b'E' => dx += n,
            b'W' => dx -= n,
            _ => {}
        }
        i = end;
    }
    (dx, dy)
}

/// Name of the configuration-bus region holding the contents of the site
/// whose pins are at column `x`, row `y`.
pub fn init_region(x: u32, y: u32) -> String {
    format!("LRAM_CORE_R{y}C{x}")
}

/// Every input pin of a site, without its clock.
pub fn input_pins() -> &'static [String] {
    &INPUT_PINS
}

/// Pins driven low by a constant from the fabric, in the order a cell's
/// added inputs take them.
pub fn low_pins() -> &'static [&'static str] {
    &LOW_PINS
}

/// The low-driven pins a cell uses, of those a device has (`available`, in
/// [`low_pins`] order): a single-port RAM leaves port B's alone. Port A's
/// byte enables gate reads too, so even a RAM that never writes needs them.
pub fn low_pins_of(single_port: bool, available: &[String]) -> Vec<&String> {
    available
        .iter()
        .filter(|pin| !(single_port && is_port_b_pin(pin)))
        .collect()
}

/// Whether a pin belongs to port B, which a single-port RAM leaves at its
/// defaults.
pub fn is_port_b_pin(pin: &str) -> bool {
    pin.starts_with("ADB")
        || pin.starts_with("DIB")
        || pin.starts_with("BENB_N")
        || matches!(pin, "CEB" | "CSB" | "WEB" | "RSTB" | "OCEB")
}

/// Whether a pin must read high and does so when left floating.
pub fn floats_high(pin: &str) -> bool {
    HIGH_PINS.contains(&pin)
}

/// Whether a large RAM runs single-port, through port A alone: a memory the
/// design never writes, as Radiant builds a ROM (`EBR_SP_EN`, port B left
/// at its defaults). Other memories write through port A and read through
/// port B.
pub fn is_single_port(cell: &Cell) -> bool {
    cell.parameters
        .get(skalp_lir::gate_netlist::RAM_READ_ONLY_PARAM)
        .is_some_and(|v| v == "1")
}

/// Router output pin of a cell's read data bit: port B's `DOB<bit>` is pin
/// `bit`, port A's `DOA<bit>` pin `DATA_BITS + bit`.
pub fn router_output_pin(single_port: bool, bit: usize) -> usize {
    if single_port {
        DATA_BITS + bit
    } else {
        bit
    }
}

/// Name of the site output behind a router output pin.
pub fn output_pin(router_pin: usize) -> String {
    if router_pin < DATA_BITS {
        format!("DOB{router_pin}")
    } else {
        format!("DOA{}", router_pin - DATA_BITS)
    }
}

/// Every data output of a site, both ports.
pub fn output_pins() -> impl Iterator<Item = String> {
    (0..2 * DATA_BITS).map(output_pin)
}

/// The site pin a cell input connects to, or `None` if it has none.
///
/// The write clock is the read clock (the block has one clock; the
/// configuration checks they are the same net), and the write port's clock
/// enable stays high: the write enable alone gates writes. A single-port
/// RAM ([`is_single_port`]) reads through port A and leaves its tied-off
/// write inputs unconnected. `extra` lists the pins of the inputs the
/// backend added after the cell's own, from index `extra_from`.
pub fn input_pin(
    ports: &RamPorts,
    input: usize,
    single_port: bool,
    extra_from: Option<usize>,
    extra: &[String],
) -> Option<String> {
    if let Some(first) = extra_from.filter(|&first| input >= first) {
        return extra.get(input - first).cloned();
    }
    let bus_bit =
        |range: &std::ops::Range<usize>| range.contains(&input).then(|| input - range.start);
    let (read_port, read_enable, read_select) = if single_port {
        ("ADA", "CEA", "CSA")
    } else {
        ("ADB", "CEB", "CSB")
    };
    if let Some(bit) = bus_bit(&ports.read_addr) {
        return Some(format!("{read_port}{bit}"));
    }
    if single_port {
        return if input == ports.read_clk {
            Some("CLK".to_string())
        } else if input == ports.read_clk_en {
            Some(read_enable.to_string())
        } else if input == ports.read_en {
            Some(read_select.to_string())
        } else if input == ports.write_en {
            Some("WEA".to_string())
        } else {
            None
        };
    }
    if let Some(bit) = bus_bit(&ports.write_addr) {
        return Some(format!("ADA{bit}"));
    }
    if let Some(bit) = bus_bit(&ports.write_data) {
        return Some(format!("DIA{bit}"));
    }
    let name = if input == ports.read_clk {
        "CLK"
    } else if input == ports.read_clk_en {
        read_enable
    } else if input == ports.read_en {
        read_select
    } else if input == ports.write_en {
        "WEA"
    } else {
        return None;
    };
    Some(name.to_string())
}

/// Router pin number of a site pin.
pub fn router_pin(pin: &str) -> Option<usize> {
    match pin {
        "CLK" => Some(FF_PIN_CLK),
        _ => INPUT_PINS
            .iter()
            .position(|p| p == pin)
            .map(|i| i + PIN_BASE),
    }
}

/// Site pin of a router pin number.
pub fn pin_of_router_pin(pin: usize) -> Option<&'static str> {
    match pin {
        FF_PIN_CLK => Some("CLK"),
        _ => INPUT_PINS
            .get(pin.checked_sub(PIN_BASE)?)
            .map(String::as_str),
    }
}

/// Whether a pin has a configurable inverter, which can hold it low.
pub fn has_inverter(pin: &str) -> bool {
    matches!(pin, "CLK" | "CSA" | "CSB" | "RSTA" | "RSTB" | "WEA" | "WEB")
}

/// prjoxide's register map of a large RAM's contents.
pub const INIT_IPTYPE: &str = "LRAM_INIT";
/// Rows of a large RAM's contents (`INITVAL_00` .. `INITVAL_7F`).
pub const INIT_ROWS: usize = 128;
/// Words in a row.
const WORDS_PER_ROW: usize = 128;
/// Bits each word takes in a row; the 8 above its 32 data bits are unused.
const WORD_STRIDE: usize = 40;
/// Bits a row of contents spans.
pub const ROW_BITS: usize = WORDS_PER_ROW * WORD_STRIDE;
/// Configuration-bus bytes between rows.
pub const ROW_STRIDE: u32 = 0x280;
/// Configuration-bus bytes of a whole site's contents, every one of which is
/// written (prjoxide writes them all, the unused ones zero).
pub const INIT_BYTES: u32 = ROW_STRIDE * INIT_ROWS as u32;

/// Name of row `index` of a large RAM's contents.
pub fn init_word_name(index: usize) -> String {
    format!("INITVAL_{index:02X}")
}

/// A large RAM's contents as its 128 rows of 5,120 bits, from `words` (one
/// per address, from 0); `None` if they don't fit.
///
/// Row `r` holds words `128 r` to `128 r + 127`, word `i` of the row in bits
/// `40 i` to `40 i + 31` (Yosys's `lrams_map_nexus.v` layout).
pub fn init_image(words: &[u64]) -> Option<Vec<Vec<bool>>> {
    if words.len() > INIT_ROWS * WORDS_PER_ROW {
        return None;
    }
    let mut rows = vec![vec![false; ROW_BITS]; INIT_ROWS];
    for (address, &word) in words.iter().enumerate() {
        let row = &mut rows[address / WORDS_PER_ROW];
        let base = (address % WORDS_PER_ROW) * WORD_STRIDE;
        for bit in 0..DATA_BITS {
            row[base + bit] = (word >> bit) & 1 == 1;
        }
    }
    Some(rows)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn ports() -> RamPorts {
        let addr = ADDRESS_BITS;
        let read_clk = addr;
        let write_data = read_clk + 3..read_clk + 3 + DATA_BITS;
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
    fn sites_come_from_tile_types() {
        assert_eq!(site_of_tile("LRAM_0"), Some(0));
        assert_eq!(site_of_tile("LRAM_6"), Some(6));
        assert_eq!(site_of_tile("LRAM_2_15K"), Some(2));
        assert_eq!(site_of_tile("LRAMX"), None);
        assert_eq!(site_of_tile("LRAM_"), None);
        assert_eq!(site_of_tile("EBR_1"), None);
    }

    #[test]
    fn steps_give_offsets() {
        assert_eq!(step_offset("W1"), (-1, 0));
        assert_eq!(step_offset("S1E1"), (1, 1));
        assert_eq!(step_offset("N5W1"), (-1, -5));
    }

    #[test]
    fn ports_map_to_pseudo_dual_port_pins() {
        let p = ports();
        assert_eq!(input_pin(&p, 0, false, None, &[]).unwrap(), "ADB0");
        assert_eq!(input_pin(&p, 13, false, None, &[]).unwrap(), "ADB13");
        assert_eq!(input_pin(&p, p.read_clk, false, None, &[]).unwrap(), "CLK");
        assert_eq!(input_pin(&p, p.write_clk, false, None, &[]), None);
        assert_eq!(input_pin(&p, p.write_clk_en, false, None, &[]), None);
        assert_eq!(input_pin(&p, p.write_en, false, None, &[]).unwrap(), "WEA");
        assert_eq!(
            input_pin(&p, p.write_data.start + 31, false, None, &[]).unwrap(),
            "DIA31"
        );
        assert_eq!(
            input_pin(&p, p.write_addr.start, false, None, &[]).unwrap(),
            "ADA0"
        );
        let extra = vec!["BENB_N0".to_string(), "BENA_N0".to_string()];
        let first = p.write_en + 1;
        assert_eq!(
            input_pin(&p, first + 1, false, Some(first), &extra).unwrap(),
            "BENA_N0"
        );
    }

    #[test]
    fn a_read_only_ram_reads_through_port_a() {
        let p = ports();
        assert_eq!(input_pin(&p, 3, true, None, &[]).unwrap(), "ADA3");
        assert_eq!(
            input_pin(&p, p.read_clk_en, true, None, &[]).unwrap(),
            "CEA"
        );
        assert_eq!(input_pin(&p, p.read_en, true, None, &[]).unwrap(), "CSA");
        assert_eq!(input_pin(&p, p.write_addr.start, true, None, &[]), None);
        assert_eq!(input_pin(&p, p.write_data.start, true, None, &[]), None);
        assert_eq!(output_pin(router_output_pin(true, 5)), "DOA5");
        assert_eq!(output_pin(router_output_pin(false, 5)), "DOB5");
    }

    #[test]
    fn byte_enables_and_power_save_are_driven_low_in_both_modes() {
        let all: Vec<String> = low_pins().iter().map(|p| p.to_string()).collect();
        let single: Vec<&str> = low_pins_of(true, &all).iter().map(|p| p.as_str()).collect();
        assert_eq!(
            single,
            ["BENA_N0", "BENA_N1", "BENA_N2", "BENA_N3", "DPS"],
            "a read-only RAM still needs port A's byte enables and power save low"
        );
        let dual = low_pins_of(false, &all);
        assert_eq!(dual.len(), 9);
        assert!(dual.iter().any(|p| *p == "BENB_N3") && dual.iter().any(|p| *p == "DPS"));
        assert!(!floats_high("DPS"), "deep power save must not float high");
    }

    #[test]
    fn router_pins_round_trip() {
        for pin in input_pins().iter().map(String::as_str).chain(["CLK"]) {
            assert_eq!(pin_of_router_pin(router_pin(pin).unwrap()), Some(pin));
        }
    }

    #[test]
    fn contents_take_forty_bits_a_word() {
        let mut words = vec![0u64; 129];
        words[0] = 0x8000_0001;
        words[1] = 0xFFFF_FFFF;
        words[128] = 1;
        let image = init_image(&words).unwrap();
        assert_eq!(image.len(), 128);
        assert!(image.iter().all(|row| row.len() == 5120));
        assert!(image[0][0] && image[0][31] && !image[0][1]);
        assert!((40..72).all(|b| image[0][b]));
        assert!(!image[0][72] && !image[0][39]);
        assert!(image[1][0]);
        assert!(init_image(&vec![0; 16384]).is_some());
        assert!(init_image(&vec![0; 16385]).is_none());
    }
}
