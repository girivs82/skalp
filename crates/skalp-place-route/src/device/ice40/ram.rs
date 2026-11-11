//! The pins and modes of the iCE40 block RAM (`SB_RAM40_4K`).
//!
//! The block is 256 rows of 16 bits. Its narrow modes keep that physical
//! width and use a subset of the data pins, spread out across the word: a
//! 512x8 block uses the even pins, 1024x4 pins 1, 5, 9 and 13, and 2048x2
//! pins 3 and 11 (Lattice's `SB_RAM40_4K` model in yosys `cells_sim.v`).
//! A memory cell from the tech mapper lists its data bits least significant
//! first, so its bit `d` goes to [`data_pin`] of the block's mode.

use skalp_lir::gate_netlist::{Cell, RamPorts, RAM_DATA_BITS_PARAM};

/// Address bits of the block (`RADDR`/`WADDR` are 11 bits in every mode).
pub const ADDR_BITS: usize = 11;
/// Data bits of the block.
pub const DATA_BITS: usize = 16;

/// The mode code of a block `width` bits wide: 0 = 256x16, 1 = 512x8,
/// 2 = 1024x4, 3 = 2048x2.
pub fn mode_for_width(width: usize) -> Option<u8> {
    match width {
        16 => Some(0),
        8 => Some(1),
        4 => Some(2),
        2 => Some(3),
        _ => None,
    }
}

/// The data pin (`RDATA`/`WDATA` index) carrying bit `bit` in `mode`.
pub fn data_pin(mode: u8, bit: usize) -> usize {
    match mode {
        1 => 2 * bit,
        2 => 4 * bit + 1,
        3 => 8 * bit + 3,
        _ => bit,
    }
}

/// The read and write mode codes of a RAM cell.
///
/// A cell from SKALP's tech mapper carries its width ([`RAM_DATA_BITS_PARAM`]);
/// the mode follows from it. A cell without one (written by hand, or by another
/// tool) gives its modes as `READ_MODE`/`WRITE_MODE`, as `SB_RAM40_4K` does.
pub fn cell_modes(cell: &Cell) -> (u8, u8) {
    let width = cell
        .parameters
        .get(RAM_DATA_BITS_PARAM)
        .and_then(|w| w.parse::<usize>().ok());
    if let Some(mode) = width.and_then(mode_for_width) {
        return (mode, mode);
    }
    let code = |name: &str| {
        cell.parameters
            .get(name)
            .and_then(|v| v.parse::<u8>().ok())
            .map_or(0, |v| v & 3)
    };
    (code("READ_MODE"), code("WRITE_MODE"))
}

/// A physical input pin of the block.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InputPin {
    ReadAddr(usize),
    ReadClk,
    ReadClkEn,
    ReadEn,
    WriteAddr(usize),
    WriteClk,
    WriteClkEn,
    WriteEn,
    WriteData(usize),
    Mask(usize),
}

const READ_ADDR: usize = 0;
const READ_CLK: usize = READ_ADDR + ADDR_BITS;
const READ_CLK_EN: usize = READ_CLK + 1;
const READ_EN: usize = READ_CLK + 2;
const WRITE_ADDR: usize = READ_EN + 1;
const WRITE_CLK: usize = WRITE_ADDR + ADDR_BITS;
const WRITE_CLK_EN: usize = WRITE_CLK + 1;
const WRITE_EN: usize = WRITE_CLK + 2;
const WRITE_DATA: usize = WRITE_EN + 1;
const MASK: usize = WRITE_DATA + DATA_BITS;
const INPUT_PINS: usize = MASK + DATA_BITS;

impl InputPin {
    /// The pin's index as a router input pin.
    pub fn router_index(self) -> usize {
        match self {
            Self::ReadAddr(i) => READ_ADDR + i,
            Self::ReadClk => READ_CLK,
            Self::ReadClkEn => READ_CLK_EN,
            Self::ReadEn => READ_EN,
            Self::WriteAddr(i) => WRITE_ADDR + i,
            Self::WriteClk => WRITE_CLK,
            Self::WriteClkEn => WRITE_CLK_EN,
            Self::WriteEn => WRITE_EN,
            Self::WriteData(i) => WRITE_DATA + i,
            Self::Mask(i) => MASK + i,
        }
    }

    /// The pin with router index `index`.
    pub fn from_router_index(index: usize) -> Option<Self> {
        Some(match index {
            i if i < READ_CLK => Self::ReadAddr(i - READ_ADDR),
            READ_CLK => Self::ReadClk,
            READ_CLK_EN => Self::ReadClkEn,
            READ_EN => Self::ReadEn,
            i if i < WRITE_CLK => Self::WriteAddr(i - WRITE_ADDR),
            WRITE_CLK => Self::WriteClk,
            WRITE_CLK_EN => Self::WriteClkEn,
            WRITE_EN => Self::WriteEn,
            i if i < MASK => Self::WriteData(i - WRITE_DATA),
            i if i < INPUT_PINS => Self::Mask(i - MASK),
            _ => return None,
        })
    }

    /// The pin's name in the chip database (`ram/<name>`).
    pub fn name(self) -> String {
        match self {
            Self::ReadAddr(i) => format!("RADDR_{i}"),
            Self::ReadClk => "RCLK".to_string(),
            Self::ReadClkEn => "RCLKE".to_string(),
            Self::ReadEn => "RE".to_string(),
            Self::WriteAddr(i) => format!("WADDR_{i}"),
            Self::WriteClk => "WCLK".to_string(),
            Self::WriteClkEn => "WCLKE".to_string(),
            Self::WriteEn => "WE".to_string(),
            Self::WriteData(i) => format!("WDATA_{i}"),
            Self::Mask(i) => format!("MASK_{i}"),
        }
    }

    /// Whether the pin is a clock or clock enable, which a constant must
    /// still drive; any other input left unconnected reads as 0.
    pub fn is_clock(self) -> bool {
        matches!(
            self,
            Self::ReadClk | Self::ReadClkEn | Self::WriteClk | Self::WriteClkEn
        )
    }
}

/// The physical pin a RAM cell's input `input` connects to.
///
/// `None` for the write mask: the cell's mask is active high and tied high
/// (write every bit), while the block's `MASK` is active low and unused in the
/// narrow modes, so it is left unconnected, which reads as 0 (write every bit).
pub fn input_pin(ports: &RamPorts, input: usize) -> Option<InputPin> {
    let mode = mode_for_width(ports.data_width()).unwrap_or(0);
    let offset =
        |range: &std::ops::Range<usize>| range.contains(&input).then(|| input - range.start);
    let pin = if let Some(bit) = offset(&ports.read_addr) {
        InputPin::ReadAddr(bit)
    } else if input == ports.read_clk {
        InputPin::ReadClk
    } else if input == ports.read_clk_en {
        InputPin::ReadClkEn
    } else if input == ports.read_en {
        InputPin::ReadEn
    } else if let Some(bit) = offset(&ports.write_data) {
        InputPin::WriteData(data_pin(mode, bit))
    } else if let Some(bit) = offset(&ports.write_addr) {
        InputPin::WriteAddr(bit)
    } else if input == ports.write_clk {
        InputPin::WriteClk
    } else if input == ports.write_clk_en {
        InputPin::WriteClkEn
    } else if input == ports.write_en {
        InputPin::WriteEn
    } else {
        return None;
    };
    let in_range = match pin {
        InputPin::ReadAddr(i) | InputPin::WriteAddr(i) => i < ADDR_BITS,
        InputPin::WriteData(i) => i < DATA_BITS,
        _ => true,
    };
    in_range.then_some(pin)
}

/// The `RDATA` pin a RAM cell's output `output` comes from.
pub fn output_pin(ports: &RamPorts, output: usize) -> usize {
    data_pin(mode_for_width(ports.data_width()).unwrap_or(0), output)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn ports(addr: usize, data: usize) -> RamPorts {
        let write_data = addr + 3..addr + 3 + data;
        let write_addr = write_data.end..write_data.end + addr;
        let write_clk = write_addr.end;
        RamPorts {
            read_addr: 0..addr,
            read_clk: addr,
            read_clk_en: addr + 1,
            read_en: addr + 2,
            write_data,
            write_addr,
            write_clk,
            write_clk_en: write_clk + 1,
            write_en: write_clk + 2,
            write_mask: write_clk + 3..write_clk + 3 + data,
        }
    }

    #[test]
    fn narrow_modes_use_the_spread_out_data_pins() {
        // yosys cells_sim.v: WDATA_I / RDATA by mode.
        let pins = |addr, data| -> Vec<usize> {
            let p = ports(addr, data);
            p.write_data
                .clone()
                .map(|i| match input_pin(&p, i) {
                    Some(InputPin::WriteData(pin)) => pin,
                    other => panic!("write data {i} -> {other:?}"),
                })
                .collect()
        };
        assert_eq!(pins(8, 16), (0..16).collect::<Vec<_>>());
        assert_eq!(pins(9, 8), vec![0, 2, 4, 6, 8, 10, 12, 14]);
        assert_eq!(pins(10, 4), vec![1, 5, 9, 13]);
        assert_eq!(pins(11, 2), vec![3, 11]);
        let p = ports(11, 2);
        assert_eq!(
            (0..2).map(|d| output_pin(&p, d)).collect::<Vec<_>>(),
            vec![3, 11]
        );
        let p = ports(10, 4);
        assert_eq!(
            (0..4).map(|d| output_pin(&p, d)).collect::<Vec<_>>(),
            vec![1, 5, 9, 13]
        );
    }

    #[test]
    fn control_and_address_pins_keep_their_order_and_the_mask_is_left_out() {
        let p = ports(9, 8);
        assert_eq!(input_pin(&p, 0), Some(InputPin::ReadAddr(0)));
        assert_eq!(input_pin(&p, 8), Some(InputPin::ReadAddr(8)));
        assert_eq!(input_pin(&p, p.read_clk), Some(InputPin::ReadClk));
        assert_eq!(input_pin(&p, p.read_en), Some(InputPin::ReadEn));
        assert_eq!(
            input_pin(&p, p.write_addr.start + 8),
            Some(InputPin::WriteAddr(8))
        );
        assert_eq!(input_pin(&p, p.write_en), Some(InputPin::WriteEn));
        for mask in p.write_mask.clone() {
            assert_eq!(input_pin(&p, mask), None);
        }
    }

    #[test]
    fn router_indices_round_trip() {
        let all = (0..ADDR_BITS)
            .map(InputPin::ReadAddr)
            .chain((0..ADDR_BITS).map(InputPin::WriteAddr))
            .chain((0..DATA_BITS).map(InputPin::WriteData))
            .chain((0..DATA_BITS).map(InputPin::Mask))
            .chain([
                InputPin::ReadClk,
                InputPin::ReadClkEn,
                InputPin::ReadEn,
                InputPin::WriteClk,
                InputPin::WriteClkEn,
                InputPin::WriteEn,
            ]);
        let mut seen = std::collections::BTreeSet::new();
        for pin in all {
            let index = pin.router_index();
            assert!(seen.insert(index), "{pin:?} shares index {index}");
            assert_eq!(InputPin::from_router_index(index), Some(pin));
        }
        assert_eq!(seen.len(), INPUT_PINS);
        assert_eq!(InputPin::from_router_index(INPUT_PINS), None);
    }

    #[test]
    fn modes_come_from_the_width_the_mapper_records() {
        let mut cell = Cell::new_comb(
            skalp_lir::gate_netlist::CellId(0),
            "SB_RAM40_4K".into(),
            "ice40".into(),
            0.0,
            "ram".into(),
            vec![],
            vec![],
        );
        cell.parameters
            .insert(RAM_DATA_BITS_PARAM.to_string(), "4".to_string());
        // An older netlist put the width in READ_MODE; the width wins.
        cell.parameters
            .insert("READ_MODE".to_string(), "4".to_string());
        assert_eq!(cell_modes(&cell), (2, 2));
        cell.parameters.shift_remove(RAM_DATA_BITS_PARAM);
        cell.parameters
            .insert("READ_MODE".to_string(), "1".to_string());
        cell.parameters
            .insert("WRITE_MODE".to_string(), "3".to_string());
        assert_eq!(cell_modes(&cell), (1, 3));
    }
}
