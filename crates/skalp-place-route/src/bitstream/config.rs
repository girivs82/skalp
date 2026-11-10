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

/// One bit of a device's configuration memory.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct DesignBit {
    /// Configuration frame
    pub frame: u32,
    /// Bit within the frame
    pub bit: u32,
    /// Value to store
    pub value: bool,
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
            memory.insert((bit.frame, bit.bit), bit.value);
        }
        let mut text = String::new();
        for ((frame, bit), value) in memory {
            text.push_str(&format!("{frame} {bit} {}\n", value as u8));
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
            },
            DesignBit {
                frame: 1,
                bit: 3,
                value: true,
            },
            DesignBit {
                frame: 2,
                bit: 7,
                value: false,
            },
        ];
        let text = String::from_utf8(ConfigTextWriter.write(&bits).unwrap()).unwrap();
        assert_eq!(text, "1 3 1\n2 7 0\n");
    }
}
