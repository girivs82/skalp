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

/// prjoxide's register map of a block RAM's contents.
pub const INIT_IPTYPE: &str = "EBR_INIT";
/// Rows of a block RAM's contents (`INITVAL_00` .. `INITVAL_3F`).
const INIT_ROWS: usize = 64;
/// Nine-bit bytes per row; each takes ten bits of the row, the tenth unused.
const BYTES_PER_ROW: usize = 32;
/// Bits a row of contents spans.
const ROW_BITS: usize = BYTES_PER_ROW * 10;

/// Name of row `index` of a block RAM's contents.
pub fn init_word_name(index: usize) -> String {
    format!("INITVAL_{index:02X}")
}

/// A block RAM's contents as its 64 rows of 320 bits, from `words` (one per
/// address, from 0) in `width`-bit mode; `None` if they don't fit.
///
/// The RAM holds 2,048 nine-bit bytes, 32 to a row with each byte in ten bits
/// (Lattice's `INITVAL` layout, as Yosys writes it). Modes 9 bits and wider
/// take whole bytes per word, low byte first. Narrower modes address the eight
/// data bits of each byte and leave the ninth (parity) bit unused, as the
/// address pins do: in 1-, 2- and 4-bit modes the address counts data bits
/// in units of the width.
pub fn init_image(width: usize, words: &[u64]) -> Option<Vec<Vec<bool>>> {
    let mut bytes = vec![0u16; INIT_ROWS * BYTES_PER_ROW];
    let fits = |count: usize| words.len() <= count;
    match width {
        9 | 18 | 36 => {
            let per_word = width / 9;
            if !fits(bytes.len() / per_word) {
                return None;
            }
            for (address, &word) in words.iter().enumerate() {
                for k in 0..per_word {
                    bytes[address * per_word + k] = ((word >> (9 * k)) & 0x1FF) as u16;
                }
            }
        }
        1 | 2 | 4 => {
            if !fits(bytes.len() * 8 / width) {
                return None;
            }
            for (address, &word) in words.iter().enumerate() {
                for j in 0..width {
                    let bit = address * width + j;
                    if (word >> j) & 1 == 1 {
                        bytes[bit / 8] |= 1 << (bit % 8);
                    }
                }
            }
        }
        _ => return None,
    }
    Some(
        bytes
            .chunks(BYTES_PER_ROW)
            .map(|row| {
                let mut bits = vec![false; ROW_BITS];
                for (i, &byte) in row.iter().enumerate() {
                    for b in 0..9 {
                        bits[i * 10 + b] = (byte >> b) & 1 == 1;
                    }
                }
                bits
            })
            .collect(),
    )
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

    #[test]
    fn contents_fill_nine_bit_bytes_ten_bits_apart() {
        // 18-bit mode: word 0 takes bytes 0 and 1; word 17 starts row 1
        let mut words = vec![0u64; 18];
        words[0] = 0x3_FE01; // low byte 0x001, high byte 0x1FF
        words[17] = 0x1FF;
        let image = init_image(18, &words).unwrap();
        assert_eq!(image.len(), 64);
        assert!(image.iter().all(|row| row.len() == 320));
        assert!(image[0][0] && !image[0][1]);
        assert!((10..19).all(|b| image[0][b]));
        assert!(!image[0][9] && !image[0][19]);
        // byte 34 = row 1, byte 2
        assert!((20..29).all(|b| image[1][b]));
    }

    #[test]
    fn narrow_modes_skip_the_parity_bit() {
        // 1-bit mode: address 8 is the first data bit of byte 1
        let mut words = vec![0u64; 9];
        words[8] = 1;
        let image = init_image(1, &words).unwrap();
        assert!(image[0][10]);
        assert_eq!(image[0].iter().filter(|&&b| b).count(), 1);
        // 4-bit mode: address 3 is the high nibble of byte 1
        let image = init_image(4, &[0, 0, 0, 0xA]).unwrap();
        assert!(image[0][15] && image[0][17] && !image[0][14] && !image[0][16]);
    }

    #[test]
    fn contents_must_fit_the_mode() {
        assert!(init_image(9, &vec![0; 2048]).is_some());
        assert!(init_image(9, &vec![0; 2049]).is_none());
        assert!(init_image(36, &vec![0; 513]).is_none());
        assert!(init_image(1, &vec![0; 16384]).is_some());
        assert!(init_image(7, &[0]).is_none());
    }
}
