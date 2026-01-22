//! IceStorm Binary (.bin) Format Generator
//!
//! Generates binary bitstream format for programming iCE40 FPGAs.

use crate::device::ice40::Ice40Device;
use crate::device::Device;
use crate::error::Result;
use crate::placer::PlacementResult;
use crate::router::RoutingResult;

/// iCE40 binary bitstream constants
const SYNC_WORD: [u8; 4] = [0xFF, 0x00, 0x00, 0xFF];
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

    /// Generate binary bitstream
    pub fn generate(
        &self,
        placement: &PlacementResult,
        routing: &RoutingResult,
    ) -> Result<Vec<u8>> {
        let mut data = Vec::new();

        // Sync pattern
        data.extend_from_slice(&SYNC_WORD);

        // Preamble
        data.extend_from_slice(&PREAMBLE);

        // Device-specific bitstream
        let (width, height) = self.device.grid_size();

        // Bitstream header command
        data.push(0x01); // Command: write CRAM
        data.push(0x00); // Bank select

        // Calculate CRAM size
        let cram_bits_per_column: usize = 54 * 8; // 54 config bits * 8 LCs
        let cram_bytes_per_column = cram_bits_per_column.div_ceil(8);
        let total_cram_bytes = (width as usize) * (height as usize) * cram_bytes_per_column;

        // Write CRAM length (big endian)
        let cram_len = total_cram_bytes as u32;
        data.push((cram_len >> 24) as u8);
        data.push((cram_len >> 16) as u8);
        data.push((cram_len >> 8) as u8);
        data.push(cram_len as u8);

        // Generate CRAM data
        let cram_data = self.generate_cram(placement, routing);
        data.extend_from_slice(&cram_data);

        // Wake-up command
        data.push(0x11); // Command: wake up

        // Padding to byte boundary
        while data.len() % 4 != 0 {
            data.push(0x00);
        }

        // CRC (simplified - actual iCE40 uses polynomial CRC)
        let crc = self.calculate_crc(&data);
        data.push((crc >> 8) as u8);
        data.push(crc as u8);

        Ok(data)
    }

    /// Generate CRAM data from placement and routing
    fn generate_cram(&self, placement: &PlacementResult, routing: &RoutingResult) -> Vec<u8> {
        let (width, height) = self.device.grid_size();
        let cram_bits_per_tile: usize = 54 * 8;
        let cram_bytes_per_tile = cram_bits_per_tile.div_ceil(8);

        let mut cram = vec![0u8; (width as usize) * (height as usize) * cram_bytes_per_tile];

        // Populate CRAM from placement
        for loc in placement.placements.values() {
            let tile_offset = ((loc.tile_y as usize) * (width as usize) + (loc.tile_x as usize))
                * cram_bytes_per_tile;

            // Set bits based on BEL type and configuration
            match loc.bel_type {
                crate::device::BelType::Lut4 => {
                    // LUT configuration starts at bit offset for this LC
                    let lc_offset = loc.bel_index * 54 / 8;
                    if tile_offset + lc_offset < cram.len() {
                        // Set LUT enable bit
                        cram[tile_offset + lc_offset] |= 0x01;
                    }
                }
                crate::device::BelType::Dff
                | crate::device::BelType::DffE
                | crate::device::BelType::DffSr
                | crate::device::BelType::DffSrE => {
                    // FF configuration
                    let lc_offset = loc.bel_index * 54 / 8;
                    if tile_offset + lc_offset + 2 < cram.len() {
                        // Set FF enable bit
                        cram[tile_offset + lc_offset + 2] |= 0x01;
                    }
                }
                crate::device::BelType::Carry => {
                    // Carry chain enable
                    let lc_offset = loc.bel_index * 54 / 8;
                    if tile_offset + lc_offset + 3 < cram.len() {
                        cram[tile_offset + lc_offset + 3] |= 0x80;
                    }
                }
                _ => {}
            }
        }

        // Populate CRAM from routing
        for route in routing.routes.values() {
            for &pip_id in &route.pips {
                if let Some(pip) = self.device.pip(pip_id) {
                    if pip.configurable {
                        let tile_offset = ((pip.tile_y as usize) * (width as usize)
                            + (pip.tile_x as usize))
                            * cram_bytes_per_tile;

                        // Set PIP enable bit
                        // PIP bits are typically in the routing section of the tile
                        let pip_byte = (pip.src_wire.0 as usize) % cram_bytes_per_tile;
                        let pip_bit = (pip.dst_wire.0 as usize) % 8;

                        if tile_offset + pip_byte < cram.len() {
                            cram[tile_offset + pip_byte] |= 1 << pip_bit;
                        }
                    }
                }
            }
        }

        cram
    }

    /// Calculate CRC-16 for bitstream
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
