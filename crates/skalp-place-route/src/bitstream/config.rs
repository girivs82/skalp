//! The layers shared by every FPGA family's bitstream generation.
//!
//! Generating a bitstream is two independent jobs:
//!
//! - [`ConfigGenerator`], per family: turn a placed and routed design into the
//!   configuration bits it needs.
//! - [`FormatWriter`], per output format: write those bits to a file format.
//!
//! [`generate_bitstream`] joins any pair of them.

use super::prepare::TiedInputs;
use super::{Bitstream, BitstreamFormat};
use crate::error::Result;
use crate::placer::PlacementResult;
use crate::router::RoutingResult;
use skalp_lir::gate_netlist::GateNetlist;

/// Which configuration memory a [`DesignBit`] belongs to.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub enum ConfigSpace {
    /// The fabric's configuration frames: `frame` is the frame, `bit` the
    /// bit within it
    #[default]
    Frames,
    /// A hard block's registers, written over the configuration bus (block
    /// RAM contents, PLL settings): `frame` is the byte address, `bit` the
    /// bit within the byte
    Bus,
}

/// One bit of a device's configuration memory.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct DesignBit {
    /// Configuration frame, or byte address on the configuration bus
    pub frame: u32,
    /// Bit within the frame or byte
    pub bit: u32,
    /// Value to store
    pub value: bool,
    /// Memory the bit is in
    pub space: ConfigSpace,
}

/// A design after placement and routing.
#[derive(Clone, Copy)]
pub struct PlacedDesign<'a> {
    /// The design's netlist
    pub netlist: &'a GateNetlist,
    /// Where each cell went
    pub placement: &'a PlacementResult,
    /// How each net was routed
    pub routing: &'a RoutingResult,
    /// Inputs tied to constants, which the router leaves unconnected
    pub tied: &'a TiedInputs,
}

/// Turns a placed design into one family's configuration bits.
pub trait ConfigGenerator {
    /// Target device name.
    fn device_name(&self) -> &str;

    /// Every configuration bit the design needs.
    fn config_bits(&self, design: &PlacedDesign<'_>) -> Result<Vec<DesignBit>>;
}

/// Writes configuration bits to one file format.
pub trait FormatWriter {
    /// The format this writer produces.
    fn format(&self) -> BitstreamFormat;

    /// Serialize the configuration bits.
    fn write(&self, bits: &[DesignBit]) -> Result<Vec<u8>>;
}

/// Generate a bitstream with a family's generator and a format's writer.
pub fn generate_bitstream(
    generator: &dyn ConfigGenerator,
    writer: &dyn FormatWriter,
    design: &PlacedDesign<'_>,
) -> Result<Bitstream> {
    let bits = generator.config_bits(design)?;
    let mut bitstream = Bitstream::new(generator.device_name().to_string(), writer.format());
    bitstream.data = writer.write(&bits)?;
    Ok(bitstream)
}

/// Writes configuration bits as sorted `frame bit value` text lines, for
/// comparing and debugging designs. Any family can use it.
#[derive(Debug, Default, Clone, Copy)]
pub struct ConfigTextWriter;

impl FormatWriter for ConfigTextWriter {
    fn format(&self) -> BitstreamFormat {
        BitstreamFormat::ConfigBitsText
    }

    fn write(&self, bits: &[DesignBit]) -> Result<Vec<u8>> {
        // The last write to a bit wins, as it does in the configuration memory.
        let mut memory = std::collections::BTreeMap::new();
        for bit in bits {
            memory.insert((bit.space, bit.frame, bit.bit), bit.value);
        }
        let mut text = String::new();
        for ((space, frame, bit), value) in memory {
            match space {
                ConfigSpace::Frames => text.push_str(&format!("{frame} {bit} {}\n", value as u8)),
                ConfigSpace::Bus => {
                    text.push_str(&format!("bus 0x{frame:08x} {bit} {}\n", value as u8))
                }
            }
        }
        Ok(text.into_bytes())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn text_writer_keeps_the_last_value_of_each_bit() {
        let bits = [
            DesignBit {
                frame: 2,
                bit: 7,
                value: true,
                space: ConfigSpace::Frames,
            },
            DesignBit {
                frame: 1,
                bit: 3,
                value: true,
                space: ConfigSpace::Frames,
            },
            DesignBit {
                frame: 2,
                bit: 7,
                value: false,
                space: ConfigSpace::Frames,
            },
        ];
        let text = String::from_utf8(ConfigTextWriter.write(&bits).unwrap()).unwrap();
        assert_eq!(text, "1 3 1\n2 7 0\n");

        let bus = DesignBit {
            frame: 0x2000_0010,
            bit: 3,
            value: true,
            space: ConfigSpace::Bus,
        };
        let text = String::from_utf8(ConfigTextWriter.write(&[bits[0], bus]).unwrap()).unwrap();
        assert_eq!(text, "2 7 1\nbus 0x20000010 3 1\n");
    }
}
