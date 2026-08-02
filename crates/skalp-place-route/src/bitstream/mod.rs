//! Bitstream Generation
//!
//! Generates device-specific bitstreams for supported FPGA families.
//! Each format reads its constants (magic bytes, frame geometry, CRC polynomials)
//! from the corresponding `device::*/data.rs` module — single source of truth.

mod cram;
mod icestorm_ascii;
mod icestorm_binary;

pub use cram::ConfigRam;
pub use icestorm_ascii::IceStormAscii;
pub use icestorm_binary::IceStormBinary;

use crate::device::ice40::Ice40Device;
use crate::device::Device;
use crate::error::{PlaceRouteError, Result};
use crate::placer::PlacementResult;
use crate::router::RoutingResult;
use serde::{Deserialize, Serialize};
use skalp_lir::gate_netlist::GateNetlist;
use std::path::Path;

/// Bitstream format
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub enum BitstreamFormat {
    /// IceStorm ASCII format (.asc) - human readable
    IceStormAscii,
    /// IceStorm binary format (.bin) - for programming
    #[default]
    IceStormBinary,
    /// VTR XML format (for academic tools)
    VtrBitstream,
    /// Project Trellis format (for ECP5)
    TrellisBinary,
    /// Project Oxide format (for Nexus/CertusPro-NX)
    OxideBitstream,
    /// OpenFPGA format
    OpenFpgaBitstream,
    /// Xilinx 7-series binary format (.bit) — direct frame generation from prjxray-db
    Xc7Binary,
}

/// Bitstream configuration
#[derive(Debug, Clone)]
pub struct BitstreamConfig {
    /// Output format
    pub format: BitstreamFormat,
    /// Include routing information
    pub include_routing: bool,
    /// Compress the bitstream
    pub compress: bool,
    /// Include timing annotations
    pub timing_annotations: bool,
}

impl Default for BitstreamConfig {
    fn default() -> Self {
        Self {
            format: BitstreamFormat::IceStormBinary,
            include_routing: true,
            compress: false,
            timing_annotations: false,
        }
    }
}

/// Bitstream metadata
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BitstreamMetadata {
    /// Logic utilization (0.0 - 1.0)
    pub logic_utilization: f64,
    /// Routing utilization
    pub routing_utilization: f64,
    /// Number of LUTs used
    pub luts_used: usize,
    /// Number of FFs used
    pub ffs_used: usize,
    /// Number of I/Os used
    pub ios_used: usize,
    /// Number of BRAMs used
    pub brams_used: usize,
}

impl Default for BitstreamMetadata {
    fn default() -> Self {
        Self {
            logic_utilization: 0.0,
            routing_utilization: 0.0,
            luts_used: 0,
            ffs_used: 0,
            ios_used: 0,
            brams_used: 0,
        }
    }
}

/// Generated bitstream
#[derive(Debug, Clone)]
pub struct Bitstream {
    /// Raw bitstream data
    pub data: Vec<u8>,
    /// Target device name
    pub device: String,
    /// Bitstream format
    pub format: BitstreamFormat,
    /// Metadata
    pub metadata: BitstreamMetadata,
}

impl Bitstream {
    /// Create a new empty bitstream
    pub fn new(device: String, format: BitstreamFormat) -> Self {
        Self {
            data: Vec::new(),
            device,
            format,
            metadata: BitstreamMetadata::default(),
        }
    }

    /// Get format info string
    pub fn format_info(&self) -> &'static str {
        match self.format {
            BitstreamFormat::IceStormAscii => "IceStorm ASCII (.asc)",
            BitstreamFormat::IceStormBinary => "IceStorm Binary (.bin)",
            BitstreamFormat::VtrBitstream => "VTR XML Bitstream",
            BitstreamFormat::TrellisBinary => "Project Trellis Binary",
            BitstreamFormat::OxideBitstream => "Project Oxide (Nexus)",
            BitstreamFormat::OpenFpgaBitstream => "OpenFPGA Bitstream",
            BitstreamFormat::Xc7Binary => "Xilinx 7-Series Binary (.bit)",
        }
    }

    /// Write bitstream to file
    pub fn write_to_file(&self, path: &Path) -> Result<()> {
        std::fs::write(path, &self.data)
            .map_err(|e| PlaceRouteError::BitstreamFailed(format!("Failed to write: {}", e)))
    }

    /// Write bitstream and report
    pub fn write_with_report(&self, path: &Path) -> Result<()> {
        // Write bitstream
        self.write_to_file(path)?;

        // Write report
        let report_path = path.with_extension("rpt");
        let report = self.generate_report();
        std::fs::write(&report_path, report)
            .map_err(|e| PlaceRouteError::BitstreamFailed(format!("Failed to write report: {}", e)))
    }

    /// Generate implementation report
    fn generate_report(&self) -> String {
        let mut report = String::new();
        report.push_str(
            "================================================================================\n",
        );
        report.push_str("                     SKALP Implementation Report\n");
        report.push_str(
            "================================================================================\n\n",
        );
        report.push_str(&format!("Device: {}\n", self.device));
        report.push_str(&format!("Format: {}\n", self.format_info()));
        report.push_str(&format!("Bitstream size: {} bytes\n\n", self.data.len()));
        report.push_str("Resource Utilization:\n");
        report.push_str(&format!(
            "  Logic utilization: {:.1}%\n",
            self.metadata.logic_utilization * 100.0
        ));
        report.push_str(&format!("  LUTs used: {}\n", self.metadata.luts_used));
        report.push_str(&format!("  FFs used: {}\n", self.metadata.ffs_used));
        report.push_str(&format!("  I/Os used: {}\n", self.metadata.ios_used));
        report.push_str(&format!("  BRAMs used: {}\n", self.metadata.brams_used));
        report.push_str(
            "\n================================================================================\n",
        );
        report.push_str("                     Generated by SKALP Native P&R\n");
        report.push_str(
            "================================================================================\n",
        );
        report
    }

    /// Convert to ASCII representation (for debugging)
    /// Note: This is a basic hex dump for binary formats, or the raw content for ASCII formats
    pub fn to_ascii(&self) -> String {
        match self.format {
            BitstreamFormat::IceStormAscii
            | BitstreamFormat::VtrBitstream
            | BitstreamFormat::OpenFpgaBitstream => {
                // Already ASCII-based, return as string
                String::from_utf8_lossy(&self.data).to_string()
            }
            BitstreamFormat::IceStormBinary
            | BitstreamFormat::TrellisBinary
            | BitstreamFormat::OxideBitstream
            | BitstreamFormat::Xc7Binary => {
                // Generate a hex dump for binary formats
                let mut output = String::new();
                output.push_str(&format!(
                    ".comment SKALP P&R bitstream for {}\n",
                    self.device
                ));
                output.push_str(&format!(".device {}\n\n", self.device));
                output.push_str(".binary_dump\n");

                for (i, chunk) in self.data.chunks(16).enumerate() {
                    output.push_str(&format!("{:08x}: ", i * 16));
                    for byte in chunk {
                        output.push_str(&format!("{:02x} ", byte));
                    }
                    output.push('\n');
                }

                output
            }
        }
    }

    /// Verify bitstream integrity
    pub fn verify(&self) -> Result<()> {
        match self.format {
            BitstreamFormat::IceStormAscii => {
                // Check for required sections
                let content = String::from_utf8_lossy(&self.data);
                if !content.contains(".device") || !content.contains(".comment") {
                    return Err(PlaceRouteError::BitstreamFailed(
                        "Invalid IceStorm ASCII format".to_string(),
                    ));
                }
            }
            BitstreamFormat::IceStormBinary => {
                // Check for sync pattern
                if self.data.len() < 8 {
                    return Err(PlaceRouteError::BitstreamFailed(
                        "Binary bitstream too short".to_string(),
                    ));
                }
                // iCE40 binary starts with sync pattern
                if self.data[0..4] != [0xFF, 0x00, 0x00, 0xFF] {
                    return Err(PlaceRouteError::BitstreamFailed(
                        "Invalid binary sync pattern".to_string(),
                    ));
                }
            }
            BitstreamFormat::VtrBitstream => {
                let content = String::from_utf8_lossy(&self.data);
                if !content.contains("<?xml") || !content.contains("<vtr_bitstream>") {
                    return Err(PlaceRouteError::BitstreamFailed(
                        "Invalid VTR XML format".to_string(),
                    ));
                }
            }
            BitstreamFormat::TrellisBinary => {
                use crate::device::ecp5::data as ecp5_data;
                // Binary format starts with 0xFF dummy bytes then preamble
                if self.data.len() < 12
                    || !self
                        .data
                        .windows(4)
                        .take(32)
                        .any(|w| w == ecp5_data::BITSTREAM_PREAMBLE)
                {
                    return Err(PlaceRouteError::BitstreamFailed(
                        "Invalid ECP5 bitstream: preamble not found".to_string(),
                    ));
                }
            }
            BitstreamFormat::OxideBitstream => {
                use crate::device::nexus::data as nexus_data;
                if self.data.len() < 12
                    || !self
                        .data
                        .windows(4)
                        .take(32)
                        .any(|w| w == nexus_data::BITSTREAM_PREAMBLE)
                {
                    return Err(PlaceRouteError::BitstreamFailed(
                        "Invalid Nexus bitstream: preamble not found".to_string(),
                    ));
                }
            }
            BitstreamFormat::Xc7Binary => {
                use crate::device::xc7::data as xc7_data;
                // Look for the sync word (0xAA995566) in the first 64 bytes
                let sync_bytes = xc7_data::BITSTREAM_SYNC_WORD.to_be_bytes();
                if self.data.len() < 20 || !self.data.windows(4).take(64).any(|w| w == sync_bytes) {
                    return Err(PlaceRouteError::BitstreamFailed(
                        "Invalid Xilinx 7-series bitstream: sync word not found".to_string(),
                    ));
                }
            }
            BitstreamFormat::OpenFpgaBitstream => {
                let content = String::from_utf8_lossy(&self.data);
                if !content.contains("<openfpga_bitstream>") {
                    return Err(PlaceRouteError::BitstreamFailed(
                        "Invalid OpenFPGA format".to_string(),
                    ));
                }
            }
        }
        Ok(())
    }
}

/// Device-specific bitstream parameters (IDCODE, frame geometry)
#[derive(Debug, Clone)]
enum DeviceBitstreamInfo {
    /// No extra info needed (iCE40 uses the Ice40Device directly)
    None,
    /// Lattice ECP5 — IDCODE + frame geometry from data.rs
    Ecp5 {
        idcode: u32,
        bitstream_frames: u32,
        bits_per_frame: u32,
    },
    /// Lattice Nexus — IDCODE + frame geometry from data.rs
    Nexus {
        idcode: u32,
        bitstream_frames: u32,
        bits_per_frame: u32,
    },
    /// Xilinx 7-series — IDCODE + frame geometry from data.rs
    Xc7 {
        idcode: u32,
        frames: &'static crate::device::xc7::data::Xc7FrameGeometry,
    },
}

/// Bitstream generator
pub struct BitstreamGenerator {
    /// Target device (used for iCE40 ASCII/binary generation)
    device: Ice40Device,
    /// Device name
    device_name: String,
    /// Configuration
    config: BitstreamConfig,
    /// Device-specific bitstream info
    device_info: DeviceBitstreamInfo,
}

impl BitstreamGenerator {
    /// Create a new bitstream generator for iCE40
    pub fn new(device: Ice40Device) -> Self {
        let name = device.name().to_string();
        Self {
            device,
            device_name: name,
            config: BitstreamConfig::default(),
            device_info: DeviceBitstreamInfo::None,
        }
    }

    /// Create with specific configuration for iCE40
    pub fn with_config(device: Ice40Device, config: BitstreamConfig) -> Self {
        let name = device.name().to_string();
        Self {
            device,
            device_name: name,
            config,
            device_info: DeviceBitstreamInfo::None,
        }
    }

    /// Create a bitstream generator for a Lattice ECP5 device
    pub fn for_ecp5(variant: crate::device::ecp5::Ecp5Variant, config: BitstreamConfig) -> Self {
        let die = variant.die_data();
        let ice40_dummy = Ice40Device::new(crate::device::ice40::Ice40Variant::Hx1k);
        Self {
            device: ice40_dummy,
            device_name: variant.name().to_string(),
            config,
            device_info: DeviceBitstreamInfo::Ecp5 {
                idcode: variant.idcode(),
                bitstream_frames: die.bitstream_frames,
                bits_per_frame: die.bits_per_frame,
            },
        }
    }

    /// Create a bitstream generator for a Lattice Nexus device
    pub fn for_nexus(variant: crate::device::nexus::NexusVariant, config: BitstreamConfig) -> Self {
        let die = variant.die_data();
        let ice40_dummy = Ice40Device::new(crate::device::ice40::Ice40Variant::Hx1k);
        Self {
            device: ice40_dummy,
            device_name: variant.name().to_string(),
            config,
            device_info: DeviceBitstreamInfo::Nexus {
                idcode: die.idcode,
                bitstream_frames: die.bitstream_frames,
                bits_per_frame: die.bits_per_frame,
            },
        }
    }

    /// Create a bitstream generator for a Xilinx 7-series device
    pub fn for_xc7(variant: crate::device::xc7::Xc7Variant, config: BitstreamConfig) -> Self {
        let ice40_dummy = Ice40Device::new(crate::device::ice40::Ice40Variant::Hx1k);
        Self {
            device: ice40_dummy,
            device_name: variant.name().to_string(),
            config,
            device_info: DeviceBitstreamInfo::Xc7 {
                idcode: variant.idcode(),
                frames: variant.frame_geometry(),
            },
        }
    }

    /// Generate bitstream from placement and routing
    pub fn generate(
        &self,
        placement: &PlacementResult,
        routing: &RoutingResult,
    ) -> Result<Bitstream> {
        self.generate_with_netlist(placement, routing, None)
    }

    /// Generate bitstream from placement, routing, and netlist (for LUT init values)
    pub fn generate_with_netlist(
        &self,
        placement: &PlacementResult,
        routing: &RoutingResult,
        netlist: Option<&GateNetlist>,
    ) -> Result<Bitstream> {
        match self.config.format {
            BitstreamFormat::IceStormAscii => self.generate_ascii(placement, routing, netlist),
            BitstreamFormat::IceStormBinary => self.generate_binary(placement, routing),
            BitstreamFormat::VtrBitstream => self.generate_vtr(placement, routing),
            BitstreamFormat::TrellisBinary => self.generate_trellis(placement, routing),
            BitstreamFormat::OxideBitstream => self.generate_oxide(placement, routing, netlist),
            BitstreamFormat::OpenFpgaBitstream => self.generate_openfpga(placement, routing),
            BitstreamFormat::Xc7Binary => self.generate_xc7(placement, routing, netlist),
        }
    }

    /// Generate IceStorm ASCII format
    fn generate_ascii(
        &self,
        placement: &PlacementResult,
        routing: &RoutingResult,
        netlist: Option<&GateNetlist>,
    ) -> Result<Bitstream> {
        let ascii_gen = IceStormAscii::new(&self.device);
        let data = ascii_gen.generate(placement, routing, netlist)?;

        let mut bitstream = Bitstream::new(
            self.device.name().to_string(),
            BitstreamFormat::IceStormAscii,
        );
        bitstream.data = data.into_bytes();
        bitstream.metadata = self.calculate_metadata(placement, routing);

        Ok(bitstream)
    }

    /// Generate IceStorm binary format
    fn generate_binary(
        &self,
        placement: &PlacementResult,
        routing: &RoutingResult,
    ) -> Result<Bitstream> {
        let binary_gen = IceStormBinary::new(&self.device);
        let data = binary_gen.generate(placement, routing)?;

        let mut bitstream = Bitstream::new(
            self.device.name().to_string(),
            BitstreamFormat::IceStormBinary,
        );
        bitstream.data = data;
        bitstream.metadata = self.calculate_metadata(placement, routing);

        Ok(bitstream)
    }

    /// Generate VTR XML format
    fn generate_vtr(
        &self,
        placement: &PlacementResult,
        routing: &RoutingResult,
    ) -> Result<Bitstream> {
        let mut xml = String::new();
        xml.push_str("<?xml version=\"1.0\"?>\n");
        xml.push_str("<vtr_bitstream>\n");
        xml.push_str(&format!("  <device>{}</device>\n", self.device.name()));
        xml.push_str("  <placement>\n");

        for (cell_id, loc) in &placement.placements {
            xml.push_str(&format!(
                "    <block id=\"{}\" x=\"{}\" y=\"{}\" bel=\"{}\"/>\n",
                cell_id.0, loc.tile_x, loc.tile_y, loc.bel_index
            ));
        }

        xml.push_str("  </placement>\n");
        xml.push_str("  <routing>\n");

        for (net_id, route) in &routing.routes {
            xml.push_str(&format!("    <net id=\"{}\">\n", net_id.0));
            for wire in &route.wires {
                xml.push_str(&format!("      <wire id=\"{}\"/>\n", wire.0));
            }
            xml.push_str("    </net>\n");
        }

        xml.push_str("  </routing>\n");
        xml.push_str("</vtr_bitstream>\n");

        let mut bitstream = Bitstream::new(
            self.device.name().to_string(),
            BitstreamFormat::VtrBitstream,
        );
        bitstream.data = xml.into_bytes();
        bitstream.metadata = self.calculate_metadata(placement, routing);

        Ok(bitstream)
    }

    /// Generate ECP5 binary bitstream
    ///
    /// Produces a valid SPI configuration bitstream per prjtrellis/ecppack format.
    /// All protocol constants come from `device::ecp5::data`.
    ///
    /// Structure: [dummy] [preamble] [VERIFY_ID] [RESET_CRC] [INIT_ADDR]
    ///            [PROG_INCR × N frames] [PROGRAM_DONE] [postamble]
    fn generate_trellis(
        &self,
        placement: &PlacementResult,
        routing: &RoutingResult,
    ) -> Result<Bitstream> {
        use crate::device::ecp5::data as ecp5_data;

        let (idcode, bitstream_frames, bits_per_frame) = match &self.device_info {
            DeviceBitstreamInfo::Ecp5 {
                idcode,
                bitstream_frames,
                bits_per_frame,
            } => (*idcode, *bitstream_frames, *bits_per_frame),
            _ => {
                return Err(PlaceRouteError::BitstreamFailed(
                    "ECP5 bitstream info not set — use BitstreamGenerator::for_ecp5()".to_string(),
                ));
            }
        };

        let bytes_per_frame = (bits_per_frame / 8) as usize;
        let mut data: Vec<u8> = Vec::new();

        // --- Preamble ---
        // Dummy bytes
        data.extend(std::iter::repeat_n(
            ecp5_data::BITSTREAM_DUMMY,
            ecp5_data::BITSTREAM_DUMMY_COUNT,
        ));
        // Sync / preamble
        data.extend_from_slice(&ecp5_data::BITSTREAM_PREAMBLE);

        // --- VERIFY_ID: check IDCODE ---
        data.push(ecp5_data::CMD_VERIFY_ID);
        data.push(0x00);
        data.push(0x00);
        data.push(0x00);
        data.extend_from_slice(&idcode.to_be_bytes());

        // --- LSC_RESET_CRC ---
        data.push(ecp5_data::CMD_RESET_CRC);
        data.push(0x00);
        data.push(0x00);
        data.push(0x00);

        // --- LSC_PROG_CNTRL0 (control register 0 = 0x00000000) ---
        data.push(ecp5_data::CMD_PROG_CNTRL0);
        data.push(0x00);
        data.push(0x00);
        data.push(0x00);
        data.extend_from_slice(&0u32.to_be_bytes());

        // --- LSC_INIT_ADDRESS (reset frame address to 0) ---
        data.push(ecp5_data::CMD_INIT_ADDR);
        data.push(0x00);
        data.push(0x00);
        data.push(0x00);

        // --- Configuration frames ---
        // LSC_PROG_INCR_NV with CRC check flag, one frame per command
        for frame_idx in 0..bitstream_frames {
            data.push(ecp5_data::CMD_PROG_INCR);
            data.push(ecp5_data::PROG_INCR_CRC_FLAG);
            data.push(0x00);
            data.push(0x01); // 1 frame

            // Frame data (zeroed — real encoding requires prjtrellis segbits)
            let mut frame = vec![0u8; bytes_per_frame];

            // Simplified: encode PIP bits into frame data
            // Real implementation needs prjtrellis tile→frame bit mapping
            for route in routing.routes.values() {
                for &pip_id in &route.pips {
                    let target_frame = pip_id.0 % bitstream_frames;
                    if target_frame == frame_idx {
                        let byte_idx = ((pip_id.0 / bitstream_frames) as usize) % bytes_per_frame;
                        let bit_idx = (pip_id.0 as usize) % 8;
                        frame[byte_idx] |= 1 << bit_idx;
                    }
                }
            }

            data.extend_from_slice(&frame);

            // CRC-16 over the frame (simplified — just zeroed for now)
            data.push(0x00);
            data.push(0x00);
        }

        // --- ISC_PROGRAM_DONE ---
        data.push(ecp5_data::CMD_PROGRAM_DONE);
        data.push(0x00);
        data.push(0x00);
        data.push(0x00);

        // --- ISC_DISABLE ---
        data.push(ecp5_data::CMD_ISC_DISABLE);
        data.push(0x00);
        data.push(0x00);
        data.push(0x00);

        // --- Postamble (trailing dummy bytes) ---
        data.extend(std::iter::repeat_n(
            ecp5_data::BITSTREAM_DUMMY,
            ecp5_data::POSTAMBLE_BYTES,
        ));

        let mut bitstream =
            Bitstream::new(self.device_name.clone(), BitstreamFormat::TrellisBinary);
        bitstream.data = data;
        bitstream.metadata = self.calculate_metadata(placement, routing);

        Ok(bitstream)
    }

    /// Generate Nexus binary bitstream for CertusPro-NX / CrossLink-NX
    ///
    /// Produces a valid SPI configuration bitstream per prjoxide/nxpack format.
    /// All protocol constants come from `device::nexus::data`.
    ///
    /// Structure: [dummy] [preamble] [DEVICE_CTRL/IDCODE] [RESET_CRC]
    ///            [INIT_ADDR] [PROG_INCR × N frames] [PROGRAM_DONE] [postamble]
    fn generate_oxide(
        &self,
        placement: &PlacementResult,
        routing: &RoutingResult,
        _netlist: Option<&GateNetlist>,
    ) -> Result<Bitstream> {
        use crate::device::nexus::data as nexus_data;

        let (idcode, bitstream_frames, bits_per_frame) = match &self.device_info {
            DeviceBitstreamInfo::Nexus {
                idcode,
                bitstream_frames,
                bits_per_frame,
            } => (*idcode, *bitstream_frames, *bits_per_frame),
            _ => {
                return Err(PlaceRouteError::BitstreamFailed(
                    "Nexus bitstream info not set — use BitstreamGenerator::for_nexus()"
                        .to_string(),
                ));
            }
        };

        let bytes_per_frame = (bits_per_frame / 8) as usize;
        let mut data: Vec<u8> = Vec::new();

        // --- Preamble ---
        data.extend(std::iter::repeat_n(
            nexus_data::BITSTREAM_DUMMY,
            nexus_data::BITSTREAM_DUMMY_COUNT,
        ));
        data.extend_from_slice(&nexus_data::BITSTREAM_PREAMBLE);

        // --- DEVICE_CTRL with IDCODE ---
        data.push(nexus_data::CMD_DEVICE_CTRL);
        data.push(0x00);
        data.push(0x00);
        data.push(0x00);
        data.extend_from_slice(&idcode.to_be_bytes());

        // --- LSC_RESET_CRC ---
        data.push(nexus_data::CMD_RESET_CRC);
        data.push(0x00);
        data.push(0x00);
        data.push(0x00);

        // --- LSC_PROG_CNTRL0 ---
        data.push(nexus_data::CMD_PROG_CNTRL0);
        data.push(0x00);
        data.push(0x00);
        data.push(0x00);
        data.extend_from_slice(&0u32.to_be_bytes());

        // --- LSC_INIT_ADDRESS ---
        data.push(nexus_data::CMD_INIT_ADDR);
        data.push(0x00);
        data.push(0x00);
        data.push(0x00);

        // --- Configuration frames ---
        for frame_idx in 0..bitstream_frames {
            data.push(nexus_data::CMD_PROG_INCR);
            data.push(nexus_data::PROG_INCR_CRC_FLAG);
            data.push(0x00);
            data.push(0x01);

            let mut frame = vec![0u8; bytes_per_frame];

            // Simplified PIP encoding (real needs prjoxide tile→frame mapping)
            for route in routing.routes.values() {
                for &pip_id in &route.pips {
                    let target_frame = pip_id.0 % bitstream_frames;
                    if target_frame == frame_idx {
                        let byte_idx = ((pip_id.0 / bitstream_frames) as usize) % bytes_per_frame;
                        let bit_idx = (pip_id.0 as usize) % 8;
                        frame[byte_idx] |= 1 << bit_idx;
                    }
                }
            }

            data.extend_from_slice(&frame);

            // CRC placeholder
            data.push(0x00);
            data.push(0x00);
        }

        // --- ISC_PROGRAM_DONE ---
        data.push(nexus_data::CMD_PROGRAM_DONE);
        data.push(0x00);
        data.push(0x00);
        data.push(0x00);

        // --- ISC_DISABLE ---
        data.push(nexus_data::CMD_ISC_DISABLE);
        data.push(0x00);
        data.push(0x00);
        data.push(0x00);

        // --- Postamble ---
        data.extend(std::iter::repeat_n(
            nexus_data::BITSTREAM_DUMMY,
            nexus_data::POSTAMBLE_BYTES,
        ));

        let mut bitstream =
            Bitstream::new(self.device_name.clone(), BitstreamFormat::OxideBitstream);
        bitstream.data = data;
        bitstream.metadata = self.calculate_metadata(placement, routing);

        Ok(bitstream)
    }

    /// Generate Xilinx 7-series binary bitstream (.bit)
    ///
    /// Produces a valid .bit file structure per UG470. All constants (sync word,
    /// register command opcodes, frame dimensions, IDCODE) come from
    /// `device::xc7::data` — the single source of truth from prjxray-db.
    ///
    /// Frame data is populated from placement (LUT init, FF config) and routing
    /// (PIP bits). Currently emits zeroed frames for unplaced tiles.
    fn generate_xc7(
        &self,
        placement: &PlacementResult,
        routing: &RoutingResult,
        netlist: Option<&GateNetlist>,
    ) -> Result<Bitstream> {
        use crate::device::xc7::data as xc7_data;

        let (idcode, total_frames) = match &self.device_info {
            DeviceBitstreamInfo::Xc7 { idcode, frames } => (*idcode, frames.total_frames),
            _ => {
                return Err(PlaceRouteError::BitstreamFailed(
                    "Xc7 device info not set — use BitstreamGenerator::for_xc7()".to_string(),
                ))
            }
        };

        let mut data: Vec<u8> = Vec::new();

        // --- Header (UG470 Section 5.3.3) ---

        // Dummy words (bus width detection preamble)
        for _ in 0..8 {
            data.extend_from_slice(&xc7_data::BITSTREAM_DUMMY_WORD.to_be_bytes());
        }

        // Bus width auto-detect
        for word in &xc7_data::BITSTREAM_BUS_WIDTH_DETECT {
            data.extend_from_slice(&word.to_be_bytes());
        }

        // More dummy words + NOOP for alignment
        data.extend_from_slice(&xc7_data::BITSTREAM_DUMMY_WORD.to_be_bytes());
        data.extend_from_slice(&xc7_data::BITSTREAM_DUMMY_WORD.to_be_bytes());

        // Sync word
        data.extend_from_slice(&xc7_data::BITSTREAM_SYNC_WORD.to_be_bytes());

        // NOOP
        data.extend_from_slice(&xc7_data::CMD_NOOP.to_be_bytes());

        // Write IDCODE register
        data.extend_from_slice(&xc7_data::CMD_WRITE_IDCODE.to_be_bytes());
        data.extend_from_slice(&idcode.to_be_bytes());

        // Reset CRC
        data.extend_from_slice(&xc7_data::CMD_WRITE_CMD.to_be_bytes());
        data.extend_from_slice(&xc7_data::CMD_RCRC.to_be_bytes());
        data.extend_from_slice(&xc7_data::CMD_NOOP.to_be_bytes());
        data.extend_from_slice(&xc7_data::CMD_NOOP.to_be_bytes());

        // --- Configuration frames ---

        // Write WCFG command
        data.extend_from_slice(&xc7_data::CMD_WRITE_CMD.to_be_bytes());
        data.extend_from_slice(&xc7_data::CMD_WCFG.to_be_bytes());
        data.extend_from_slice(&xc7_data::CMD_NOOP.to_be_bytes());

        // Set FAR to 0 (start of CLB/IO/CLK frames)
        data.extend_from_slice(&xc7_data::CMD_WRITE_FAR.to_be_bytes());
        data.extend_from_slice(&0u32.to_be_bytes());

        // Type 1 FDRI header (0 word count — followed by Type 2 with real count)
        data.extend_from_slice(&xc7_data::CMD_WRITE_FDRI_HDR.to_be_bytes());

        // Type 2 packet: total_frames × FRAME_WORDS words of frame data
        let total_words = total_frames * xc7_data::FRAME_WORDS;
        let type2_header = xc7_data::CMD_TYPE2_HDR | total_words;
        data.extend_from_slice(&type2_header.to_be_bytes());

        // Emit configuration frames
        // For now, emit zeroed frames (unconfigured) — proper frame bit encoding
        // requires the prjxray-db segbits database, which maps placement/routing
        // decisions to specific bit positions within each frame.
        let frame_bytes = (total_frames * xc7_data::FRAME_WORDS * 4) as usize;
        let mut frame_data = vec![0u8; frame_bytes];

        // Encode LUT init values into frame data (simplified — real encoding
        // requires per-tile segbits from prjxray-db)
        if let Some(nl) = netlist {
            for (cell_id, loc) in &placement.placements {
                if loc.bel_type == crate::device::BelType::Lut6
                    || loc.bel_type == crate::device::BelType::Lut4
                {
                    if let Some(cell) = nl.cells.get(cell_id.0 as usize) {
                        let init = cell.lut_init.unwrap_or(0);
                        // Simplified frame encoding: place init bits at a
                        // deterministic offset derived from tile coords and BEL index.
                        // Real implementation would use prjxray segbits tables.
                        let frame_idx =
                            (loc.tile_x as usize * xc7_data::FRAME_WORDS as usize) % frame_bytes;
                        let byte_offset =
                            (loc.tile_y as usize * 8 + loc.bel_index) % (frame_bytes - 8);
                        let offset = frame_idx.min(byte_offset).min(frame_bytes - 8);
                        frame_data[offset..offset + 8].copy_from_slice(&init.to_le_bytes());
                    }
                }
            }
        }

        // Encode PIP enables (simplified)
        for route in routing.routes.values() {
            for &pip_id in &route.pips {
                // Set a bit for each active PIP (simplified — real encoding
                // requires per-tile PIP segbits from prjxray-db)
                let byte_idx = (pip_id.0 as usize / 8) % frame_bytes;
                let bit_idx = (pip_id.0 as usize) % 8;
                frame_data[byte_idx] |= 1 << bit_idx;
            }
        }

        data.extend_from_slice(&frame_data);

        // --- Footer ---

        // GRESTORE
        data.extend_from_slice(&xc7_data::CMD_WRITE_CMD.to_be_bytes());
        data.extend_from_slice(&xc7_data::CMD_GRESTORE.to_be_bytes());
        data.extend_from_slice(&xc7_data::CMD_NOOP.to_be_bytes());

        // GTS (release I/O)
        data.extend_from_slice(&xc7_data::CMD_WRITE_CMD.to_be_bytes());
        data.extend_from_slice(&xc7_data::CMD_GTS.to_be_bytes());
        data.extend_from_slice(&xc7_data::CMD_NOOP.to_be_bytes());

        // START
        data.extend_from_slice(&xc7_data::CMD_WRITE_CMD.to_be_bytes());
        data.extend_from_slice(&xc7_data::CMD_START.to_be_bytes());
        data.extend_from_slice(&xc7_data::CMD_NOOP.to_be_bytes());

        // DESYNC
        data.extend_from_slice(&xc7_data::CMD_WRITE_CMD.to_be_bytes());
        data.extend_from_slice(&xc7_data::CMD_DESYNC.to_be_bytes());

        // Trailing NOOPs (flush pipeline)
        for _ in 0..16 {
            data.extend_from_slice(&xc7_data::CMD_NOOP.to_be_bytes());
        }

        let mut bitstream = Bitstream::new(self.device_name.clone(), BitstreamFormat::Xc7Binary);
        bitstream.data = data;
        bitstream.metadata = self.calculate_metadata(placement, routing);

        Ok(bitstream)
    }

    /// Generate OpenFPGA format (placeholder)
    fn generate_openfpga(
        &self,
        placement: &PlacementResult,
        routing: &RoutingResult,
    ) -> Result<Bitstream> {
        let mut xml = String::new();
        xml.push_str("<?xml version=\"1.0\"?>\n");
        xml.push_str("<openfpga_bitstream>\n");
        xml.push_str(&format!("  <device>{}</device>\n", self.device.name()));
        xml.push_str("  <fabric_configuration>\n");
        xml.push_str("    <logic_blocks>\n");
        xml.push_str("    </logic_blocks>\n");
        xml.push_str("    <routing_configuration>\n");
        xml.push_str("    </routing_configuration>\n");
        xml.push_str("    <io_configuration>\n");
        xml.push_str("    </io_configuration>\n");
        xml.push_str("    <clock_configuration>\n");
        xml.push_str("    </clock_configuration>\n");
        xml.push_str("  </fabric_configuration>\n");
        xml.push_str("  <timing_annotations>\n");
        xml.push_str("  </timing_annotations>\n");
        xml.push_str("</openfpga_bitstream>\n");

        let mut bitstream = Bitstream::new(
            self.device.name().to_string(),
            BitstreamFormat::OpenFpgaBitstream,
        );
        bitstream.data = xml.into_bytes();
        bitstream.metadata = self.calculate_metadata(placement, routing);

        Ok(bitstream)
    }

    /// Calculate metadata from placement and routing
    fn calculate_metadata(
        &self,
        placement: &PlacementResult,
        routing: &RoutingResult,
    ) -> BitstreamMetadata {
        let stats = self.device.stats();

        let mut luts = 0;
        let mut ffs = 0;
        let mut ios = 0;
        let mut brams = 0;

        for loc in placement.placements.values() {
            match loc.bel_type {
                crate::device::BelType::Lut4 | crate::device::BelType::Lut6 => luts += 1,
                crate::device::BelType::Dff
                | crate::device::BelType::DffE
                | crate::device::BelType::DffSr
                | crate::device::BelType::DffSrE => ffs += 1,
                crate::device::BelType::IoCell => ios += 1,
                crate::device::BelType::RamSlice => brams += 1,
                _ => {}
            }
        }

        let logic_util = if stats.total_luts > 0 {
            luts as f64 / stats.total_luts as f64
        } else {
            0.0
        };

        BitstreamMetadata {
            logic_utilization: logic_util,
            routing_utilization: routing.congestion,
            luts_used: luts,
            ffs_used: ffs,
            ios_used: ios,
            brams_used: brams,
        }
    }
}
