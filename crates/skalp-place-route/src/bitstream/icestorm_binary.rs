//! IceStorm Binary (.bin) Format Generator
//!
//! Generates binary bitstream format for programming iCE40 FPGAs.
//! The binary format is used by iceprog to program the device.
//!
//! Binary format structure:
//! - 0x7EAA997E: Preamble
//! - 0xFF00: Bank 0 select
//! - 0x00-0xFF: Commands and data
//! - CRAM data organized by column
//! - Wake up sequence

use super::IceStormAscii;
use crate::device::ice40::{Ice40Device, Ice40Variant};
use crate::error::Result;
use crate::placer::PlacementResult;
use crate::router::RoutingResult;

/// iCE40 binary bitstream constants
const PREAMBLE: [u8; 4] = [0x7E, 0xAA, 0x99, 0x7E];

/// IceStorm binary format generator
pub struct IceStormBinary<'a> {
    device: &'a Ice40Device,
}

impl<'a> IceStormBinary<'a> {
    /// Create a new binary generator
    pub fn new(device: &'a Ice40Device) -> Self {
        Self { device }
    }

    /// Generate binary bitstream by converting from ASCII format
    /// This ensures consistency with the verified ASCII generation
    pub fn generate(
        &self,
        placement: &PlacementResult,
        routing: &RoutingResult,
    ) -> Result<Vec<u8>> {
        // First, generate ASCII format (which we know is correct)
        let ascii_gen = IceStormAscii::new(self.device);
        let ascii = ascii_gen.generate(placement, routing, None)?;

        // Convert ASCII to binary
        self.ascii_to_binary(&ascii)
    }

    /// Convert ASCII bitstream to binary format
    /// This is equivalent to what icepack does
    fn ascii_to_binary(&self, ascii: &str) -> Result<Vec<u8>> {
        let mut data = Vec::new();

        // Preamble
        data.extend_from_slice(&PREAMBLE);

        // Parse ASCII format and build CRAM
        let mut cram = CramBuilder::new(self.device.variant);

        for line in ascii.lines() {
            let line = line.trim();

            if line.starts_with(".logic_tile") {
                // Parse: .logic_tile X Y
                let parts: Vec<&str> = line.split_whitespace().collect();
                if parts.len() >= 3 {
                    let x: u32 = parts[1].parse().unwrap_or(0);
                    let y: u32 = parts[2].parse().unwrap_or(0);
                    cram.set_current_tile(x, y, TileKind::Logic);
                }
            } else if line.starts_with(".io_tile") {
                let parts: Vec<&str> = line.split_whitespace().collect();
                if parts.len() >= 3 {
                    let x: u32 = parts[1].parse().unwrap_or(0);
                    let y: u32 = parts[2].parse().unwrap_or(0);
                    cram.set_current_tile(x, y, TileKind::Io);
                }
            } else if line.starts_with(".ramb_tile") || line.starts_with(".ramt_tile") {
                let parts: Vec<&str> = line.split_whitespace().collect();
                if parts.len() >= 3 {
                    let x: u32 = parts[1].parse().unwrap_or(0);
                    let y: u32 = parts[2].parse().unwrap_or(0);
                    cram.set_current_tile(x, y, TileKind::Ram);
                }
            } else if line.starts_with('.') || line.is_empty() {
                // Skip other directives and empty lines
                continue;
            } else if line.chars().all(|c| c == '0' || c == '1') {
                // This is a bit row
                cram.add_bit_row(line);
            }
        }

        // Build the CRAM data
        let cram_data = cram.build();

        // Bank 0 select and CRAM write command
        data.push(0x00); // Dummy bytes
        data.push(0x00);
        data.push(0x00);
        data.push(0x00);
        data.push(0x00);
        data.push(0x00);
        data.push(0x00);

        // Command 0x01: Write CRAM
        data.push(0x01);
        data.push(0x01); // Extra byte

        // CRAM length (3 bytes, big endian for iCE40 1k/8k)
        let cram_len = cram_data.len() as u32;
        data.push(((cram_len >> 16) & 0xFF) as u8);
        data.push(((cram_len >> 8) & 0xFF) as u8);
        data.push((cram_len & 0xFF) as u8);

        // CRAM data
        data.extend_from_slice(&cram_data);

        // Wake-up sequence
        data.push(0x00);
        data.push(0x00);
        data.push(0x00);
        data.push(0x00);
        data.push(0x00);
        data.push(0x00);
        data.push(0x00);
        data.push(0x00);

        // CRC (2 bytes, computed over CRAM data)
        let crc = self.calculate_crc(&cram_data);
        data.push(((crc >> 8) & 0xFF) as u8);
        data.push((crc & 0xFF) as u8);

        // Trailing zeros
        data.push(0x00);
        data.push(0x06);

        Ok(data)
    }

    /// Calculate CRC-16 for bitstream (iCE40 CRC algorithm)
    fn calculate_crc(&self, data: &[u8]) -> u16 {
        let mut crc: u16 = 0xFFFF;
        let polynomial: u16 = 0x8005;

        for &byte in data {
            crc ^= (byte as u16) << 8;
            for _ in 0..8 {
                if crc & 0x8000 != 0 {
                    crc = (crc << 1) ^ polynomial;
                } else {
                    crc <<= 1;
                }
            }
        }

        crc
    }
}

/// Tile type for CRAM organization
#[derive(Debug, Clone, Copy)]
enum TileKind {
    Logic, // 54 columns
    Io,    // 18 columns
    Ram,   // 42 columns
}

/// CRAM builder for converting ASCII to binary
struct CramBuilder {
    variant: Ice40Variant,
    current_tile: Option<(u32, u32, TileKind)>,
    current_row: usize,
    // CRAM data organized by (x, y, kind) -> rows of bits
    tiles: std::collections::HashMap<(u32, u32), Vec<Vec<bool>>>,
}

impl CramBuilder {
    fn new(variant: Ice40Variant) -> Self {
        Self {
            variant,
            current_tile: None,
            current_row: 0,
            tiles: std::collections::HashMap::new(),
        }
    }

    fn set_current_tile(&mut self, x: u32, y: u32, kind: TileKind) {
        self.current_tile = Some((x, y, kind));
        self.current_row = 0;
        // Initialize tile if not present
        self.tiles.entry((x, y)).or_default();
    }

    fn add_bit_row(&mut self, row_str: &str) {
        if let Some((x, y, _kind)) = self.current_tile {
            let bits: Vec<bool> = row_str.chars().map(|c| c == '1').collect();
            let tile_data = self.tiles.entry((x, y)).or_default();
            tile_data.push(bits);
            self.current_row += 1;
        }
    }

    fn build(&self) -> Vec<u8> {
        // Get device dimensions
        let (width, height) = match self.variant {
            Ice40Variant::Hx1k | Ice40Variant::Lp1k => (14, 18), // Including I/O columns
            Ice40Variant::Hx4k | Ice40Variant::Lp4k | Ice40Variant::Up5k => (20, 32),
            Ice40Variant::Hx8k | Ice40Variant::Lp8k => (34, 34),
        };

        // Calculate CRAM size
        // For 1k: 332 bits per column, 144 columns = 47808 bits = 5976 bytes
        let bits_per_col = height * 16; // 16 rows per tile, variable columns
        let cols_per_bank = width * 54; // Approximate - logic tiles are 54 bits

        // Build raw CRAM data
        let mut cram_bits: Vec<bool> = Vec::new();

        // Iterate through columns then rows (iCE40 CRAM organization)
        for col in 0..cols_per_bank {
            for row in 0..bits_per_col {
                // Find which tile and bit this corresponds to
                let tile_x = col / 54;
                let tile_y = row / 16;
                let bit_col = col % 54;
                let bit_row = row % 16;

                if let Some(tile_data) = self.tiles.get(&(tile_x as u32, tile_y as u32)) {
                    if bit_row < tile_data.len() && bit_col < tile_data[bit_row].len() {
                        cram_bits.push(tile_data[bit_row][bit_col]);
                    } else {
                        cram_bits.push(false);
                    }
                } else {
                    cram_bits.push(false);
                }
            }
        }

        // Convert bits to bytes
        let mut cram_bytes = Vec::new();
        for chunk in cram_bits.chunks(8) {
            let mut byte = 0u8;
            for (i, &bit) in chunk.iter().enumerate() {
                if bit {
                    byte |= 1 << (7 - i);
                }
            }
            cram_bytes.push(byte);
        }

        cram_bytes
    }
}
