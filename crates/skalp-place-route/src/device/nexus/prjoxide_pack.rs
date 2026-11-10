//! M2: real Nexus bitstream packing.
//!
//! SKALP's place-and-route decides *which* configuration bits are set (each used
//! PIP contributes global `(frame, bit)` coordinates — see [`super::prjoxide_graph`]
//! — plus LUT INIT / FF config bits). The *frame container* around those bits —
//! preamble, command sequence, compression, per-frame **CRC16** (poly `0x8005`) and
//! **ECC14** (poly `0x202D`), IO/tap frames, postamble — is intricate and safety-
//! critical: one wrong bit and the device's config engine rejects the stream.
//!
//! Rather than re-derive that container (and risk silent corruption), we hand the
//! set bits to `libprjoxide`, which already implements it exactly. The seam is its
//! raw `Chip::cram` config-RAM matrix: we `set(frame, bit, val)` the bits we want,
//! then `serialise_chip` emits a programmable `.bit`. SKALP keeps full ownership of
//! the NCL-aware P&R; libprjoxide is used *only* as the byte-accurate packer.
//!
//! Gated behind the `nexus-bitstream` feature (pulls in the `prjoxide` crate).

#[cfg(feature = "nexus-bitstream")]
use super::prjoxide_graph::GlobalBit;
#[cfg(feature = "nexus-bitstream")]
use std::path::Path;

/// Pack a set of configuration bits into a programmable Nexus bitstream.
///
/// `db_root` is the prjoxide database root (with `devices.json`); `idcode`
/// selects the device (e.g. `0x010F_4043` for LFCPNX-100). `bits` are the global
/// config bits to assert — typically gathered from used PIPs' `pip_bits` and BEL
/// config words. A `ConfigBit` with `invert = true` is stored as `0` when asserted
/// (prjoxide convention: the cram value is `!invert`).
///
/// Returns the `.bit` byte stream, ready for `openFPGALoader`.
#[cfg(feature = "nexus-bitstream")]
pub fn pack_bitstream(
    db_root: &Path,
    idcode: u32,
    bits: impl IntoIterator<Item = GlobalBit>,
) -> Result<Vec<u8>, String> {
    pack_bitstream_with_bus(db_root, idcode, bits, std::iter::empty())
}

/// [`pack_bitstream`], plus hard-block register bits written over the
/// configuration bus after the frames (block RAM contents): each is
/// `(byte address, bit, value)`, and libprjoxide groups them into bus writes.
#[cfg(feature = "nexus-bitstream")]
pub fn pack_bitstream_with_bus(
    db_root: &Path,
    idcode: u32,
    bits: impl IntoIterator<Item = GlobalBit>,
    bus_bits: impl IntoIterator<Item = (u32, u32, bool)>,
) -> Result<Vec<u8>, String> {
    use prjoxide::bitstream::BitstreamParser;
    use prjoxide::chip::Chip;
    use prjoxide::database::Database;

    let root = db_root
        .to_str()
        .ok_or_else(|| "db_root is not valid UTF-8".to_string())?;
    let mut db = Database::new(root);
    let mut chip = Chip::from_idcode(&mut db, idcode);

    for b in bits {
        chip.cram.set(b.frame as usize, b.bit as usize, !b.invert);
    }
    for (address, bit, value) in bus_bits {
        chip.set_ip_bit(address, 0, bit, value);
    }

    Ok(BitstreamParser::serialise_chip(&chip))
}

/// The bytes a Nexus bitstream writes over the configuration bus (block RAM
/// contents and other hard-block registers), by byte address in order.
#[cfg(feature = "nexus-bitstream")]
pub fn read_bus_config(db_root: &Path, bitstream: &[u8]) -> Result<Vec<(u32, u8)>, String> {
    use prjoxide::bitstream::BitstreamParser;
    use prjoxide::database::Database;

    let root = db_root
        .to_str()
        .ok_or_else(|| "db_root is not valid UTF-8".to_string())?;
    let mut db = Database::new(root);
    let chip = BitstreamParser::new(bitstream).parse(&mut db)?;
    Ok(chip.ipconfig.iter().map(|(&a, &v)| (a, v)).collect())
}

/// Writes Lattice `.bit` files for Nexus devices.
#[cfg(feature = "nexus-bitstream")]
pub struct NexusBitWriter<'a> {
    /// prjoxide database root
    pub db_root: &'a Path,
    /// IDCODE of the target device
    pub idcode: u32,
}

#[cfg(feature = "nexus-bitstream")]
impl crate::bitstream::config::FormatWriter for NexusBitWriter<'_> {
    fn format(&self) -> crate::bitstream::BitstreamFormat {
        crate::bitstream::BitstreamFormat::OxideBitstream
    }

    fn write(&self, bits: &[crate::bitstream::config::DesignBit]) -> crate::error::Result<Vec<u8>> {
        use crate::bitstream::config::ConfigSpace;
        let frames = bits
            .iter()
            .filter(|b| b.space == ConfigSpace::Frames)
            .map(|b| GlobalBit {
                frame: b.frame,
                bit: b.bit,
                invert: !b.value,
            });
        let bus = bits
            .iter()
            .filter(|b| b.space == ConfigSpace::Bus)
            .map(|b| (b.frame, b.bit, b.value));
        pack_bitstream_with_bus(self.db_root, self.idcode, frames, bus)
            .map_err(crate::error::PlaceRouteError::BitstreamFailed)
    }
}

#[cfg(all(test, feature = "nexus-bitstream"))]
mod tests {
    use super::super::prjoxide_load::find_database;
    use super::super::NexusVariant;
    use super::*;

    /// Pack a handful of bits and parse the result back through libprjoxide's own
    /// parser: the produced stream must be a valid Nexus bitstream whose decoded
    /// cram contains exactly the bits we set. This validates the full container
    /// (preamble/commands/CRC16/ECC14/frames) without needing hardware.
    #[test]
    fn pack_and_reparse_roundtrip() {
        let Some(db_root) = find_database() else {
            eprintln!("PRJOXIDE_DB not found — skipping");
            return;
        };
        let idcode = NexusVariant::Lfcpnx100.idcode();

        // A few interior bits (well within the LFCPNX-100 frame geometry:
        // 16822 frames × 878 bits).
        let set = vec![
            GlobalBit {
                frame: 604,
                bit: 67,
                invert: false,
            },
            GlobalBit {
                frame: 605,
                bit: 68,
                invert: false,
            },
            GlobalBit {
                frame: 1000,
                bit: 100,
                invert: false,
            },
        ];

        let bytes = pack_bitstream(&db_root, idcode, set.clone()).expect("pack");
        assert!(bytes.len() > 1000, "non-trivial bitstream produced");

        // Re-parse through libprjoxide and confirm our bits survived the round trip.
        use prjoxide::bitstream::BitstreamParser;
        use prjoxide::database::Database;
        let mut db = Database::new(db_root.to_str().unwrap());
        let chip = BitstreamParser::new(&bytes)
            .parse(&mut db)
            .expect("reparse valid bitstream");
        for b in &set {
            assert!(
                chip.cram.get(b.frame as usize, b.bit as usize),
                "bit ({},{}) survived pack→parse",
                b.frame,
                b.bit
            );
        }
        eprintln!(
            "M2 packing OK: {} bytes, {} bits round-tripped through libprjoxide parser",
            bytes.len(),
            set.len()
        );
    }

    /// Bus bits (block RAM contents) come back from libprjoxide's parser at
    /// the addresses they were written to.
    #[test]
    fn bus_bits_survive_pack_and_reparse() {
        let Some(db_root) = find_database() else {
            eprintln!("PRJOXIDE_DB not found — skipping");
            return;
        };
        let idcode = NexusVariant::Lfcpnx100.idcode();
        let base = 0x2004_0000u32; // block RAM window 2
        let bus = vec![(base, 0, true), (base, 3, true), (base + 1, 7, true)];
        let bytes =
            pack_bitstream_with_bus(&db_root, idcode, std::iter::empty(), bus).expect("pack");

        use prjoxide::bitstream::BitstreamParser;
        use prjoxide::database::Database;
        let mut db = Database::new(db_root.to_str().unwrap());
        let chip = BitstreamParser::new(&bytes)
            .parse(&mut db)
            .expect("reparse valid bitstream");
        assert_eq!(chip.ipconfig.get(&base), Some(&0b1001));
        assert_eq!(chip.ipconfig.get(&(base + 1)), Some(&0x80));
    }
}
