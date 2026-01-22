//! Bitstream Generation
//!
//! Generates IceStorm-compatible bitstreams for iCE40 FPGAs.

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
    /// OpenFPGA format
    OpenFpgaBitstream,
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
            BitstreamFormat::OpenFpgaBitstream => "OpenFPGA Bitstream",
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
            BitstreamFormat::IceStormBinary | BitstreamFormat::TrellisBinary => {
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
                if !self.data.starts_with(b"TRELLIS") {
                    return Err(PlaceRouteError::BitstreamFailed(
                        "Invalid Trellis format".to_string(),
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

/// Bitstream generator
pub struct BitstreamGenerator {
    /// Target device
    device: Ice40Device,
    /// Configuration
    config: BitstreamConfig,
}

impl BitstreamGenerator {
    /// Create a new bitstream generator
    pub fn new(device: Ice40Device) -> Self {
        Self {
            device,
            config: BitstreamConfig::default(),
        }
    }

    /// Create with specific configuration
    pub fn with_config(device: Ice40Device, config: BitstreamConfig) -> Self {
        Self { device, config }
    }

    /// Generate bitstream from placement and routing
    pub fn generate(
        &self,
        placement: &PlacementResult,
        routing: &RoutingResult,
    ) -> Result<Bitstream> {
        match self.config.format {
            BitstreamFormat::IceStormAscii => self.generate_ascii(placement, routing),
            BitstreamFormat::IceStormBinary => self.generate_binary(placement, routing),
            BitstreamFormat::VtrBitstream => self.generate_vtr(placement, routing),
            BitstreamFormat::TrellisBinary => self.generate_trellis(placement, routing),
            BitstreamFormat::OpenFpgaBitstream => self.generate_openfpga(placement, routing),
        }
    }

    /// Generate IceStorm ASCII format
    fn generate_ascii(
        &self,
        placement: &PlacementResult,
        routing: &RoutingResult,
    ) -> Result<Bitstream> {
        let ascii_gen = IceStormAscii::new(&self.device);
        let data = ascii_gen.generate(placement, routing)?;

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

    /// Generate Trellis format (placeholder)
    fn generate_trellis(
        &self,
        placement: &PlacementResult,
        routing: &RoutingResult,
    ) -> Result<Bitstream> {
        let mut data = Vec::new();
        data.extend_from_slice(b"TRELLIS_ECP5\n");
        data.extend_from_slice(b"TILES\n");
        data.extend_from_slice(b"IOCONF\n");
        // Placeholder content
        data.extend_from_slice(format!("DEVICE: {}\n", self.device.name()).as_bytes());

        let mut bitstream = Bitstream::new(
            self.device.name().to_string(),
            BitstreamFormat::TrellisBinary,
        );
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
