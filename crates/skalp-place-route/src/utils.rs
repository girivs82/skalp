//! Utilities for iCE40 FPGA Design
//!
//! Provides utility functions similar to IceStorm tools:
//! - PLL parameter calculator (like icepll)
//! - BRAM content swapping (like icebram)

use crate::error::{PlaceRouteError, Result};
use std::collections::HashMap;

// ============================================================================
// PLL Parameter Calculator (icepll equivalent)
// ============================================================================

/// PLL configuration parameters for iCE40
#[derive(Debug, Clone)]
pub struct PllConfig {
    /// Input frequency (MHz)
    pub f_in: f64,
    /// Output frequency (MHz)
    pub f_out: f64,
    /// DIVR (reference divider, 4-bit)
    pub divr: u8,
    /// DIVF (feedback divider, 7-bit)
    pub divf: u8,
    /// DIVQ (output divider, 3-bit)
    pub divq: u8,
    /// FILTER_RANGE (2-bit)
    pub filter_range: u8,
    /// Actual achieved frequency (MHz)
    pub f_achieved: f64,
    /// Frequency error (%)
    pub error_percent: f64,
    /// PFD frequency (MHz)
    pub f_pfd: f64,
    /// VCO frequency (MHz)
    pub f_vco: f64,
}

impl PllConfig {
    /// Generate Verilog instantiation for this PLL configuration
    pub fn to_verilog(&self) -> String {
        format!(
            r#"// PLL configuration for {} MHz -> {} MHz (achieved: {} MHz, error: {:.2}%)
// F_PFD = {} MHz, F_VCO = {} MHz
SB_PLL40_CORE #(
    .FEEDBACK_PATH("SIMPLE"),
    .DIVR(4'd{}),        // DIVR = {}
    .DIVF(7'd{}),        // DIVF = {}
    .DIVQ(3'd{}),        // DIVQ = {}
    .FILTER_RANGE(3'b{:03b})  // FILTER_RANGE = {}
) pll_inst (
    .REFERENCECLK(clk_in),
    .PLLOUTCORE(clk_out),
    .PLLOUTGLOBAL(),
    .BYPASS(1'b0),
    .RESETB(1'b1),
    .LOCK(pll_lock)
);"#,
            self.f_in,
            self.f_out,
            self.f_achieved,
            self.error_percent,
            self.f_pfd,
            self.f_vco,
            self.divr,
            self.divr,
            self.divf,
            self.divf,
            self.divq,
            self.divq,
            self.filter_range,
            self.filter_range
        )
    }
}

/// PLL parameter calculator for iCE40
pub struct PllCalculator {
    /// Minimum PFD frequency (MHz)
    min_pfd: f64,
    /// Maximum PFD frequency (MHz)
    max_pfd: f64,
    /// Minimum VCO frequency (MHz)
    min_vco: f64,
    /// Maximum VCO frequency (MHz)
    max_vco: f64,
}

impl Default for PllCalculator {
    fn default() -> Self {
        Self::new()
    }
}

impl PllCalculator {
    /// Create a new PLL calculator with iCE40 constraints
    pub fn new() -> Self {
        Self {
            // iCE40 PLL constraints
            min_pfd: 10.0,   // MHz
            max_pfd: 133.0,  // MHz
            min_vco: 533.0,  // MHz
            max_vco: 1066.0, // MHz
        }
    }

    /// Calculate PLL parameters for target frequencies
    ///
    /// # Arguments
    /// * `f_in` - Input frequency in MHz
    /// * `f_out` - Desired output frequency in MHz
    ///
    /// # Returns
    /// Best PLL configuration or error if no valid configuration exists
    pub fn calculate(&self, f_in: f64, f_out: f64) -> Result<PllConfig> {
        let mut best_config: Option<PllConfig> = None;
        let mut best_error = f64::MAX;

        // DIVR: 0-15 (reference divider)
        // DIVF: 0-127 (feedback divider)
        // DIVQ: 1-6 (output divider, actual divisor = 2^DIVQ)

        for divr in 0..=15u8 {
            let f_pfd = f_in / (divr as f64 + 1.0);

            // Check PFD frequency constraints
            if f_pfd < self.min_pfd || f_pfd > self.max_pfd {
                continue;
            }

            for divf in 0..=127u8 {
                let f_vco = f_pfd * (divf as f64 + 1.0);

                // Check VCO frequency constraints
                if f_vco < self.min_vco || f_vco > self.max_vco {
                    continue;
                }

                for divq in 1..=6u8 {
                    let divisor = 1u32 << divq;
                    let f_achieved = f_vco / divisor as f64;

                    let error = (f_achieved - f_out).abs();
                    let error_percent = error / f_out * 100.0;

                    if error < best_error {
                        best_error = error;

                        // Calculate filter range based on PFD frequency
                        let filter_range = self.calculate_filter_range(f_pfd);

                        best_config = Some(PllConfig {
                            f_in,
                            f_out,
                            divr,
                            divf,
                            divq,
                            filter_range,
                            f_achieved,
                            error_percent,
                            f_pfd,
                            f_vco,
                        });

                        // If we found an exact match, return early
                        if error < 0.001 {
                            return Ok(best_config.unwrap());
                        }
                    }
                }
            }
        }

        best_config.ok_or_else(|| {
            PlaceRouteError::InvalidConstraint(format!(
                "No valid PLL configuration for {} MHz -> {} MHz",
                f_in, f_out
            ))
        })
    }

    /// Calculate multiple configurations and return top N by accuracy
    pub fn find_configurations(&self, f_in: f64, f_out: f64, max_results: usize) -> Vec<PllConfig> {
        let mut configs: Vec<PllConfig> = Vec::new();

        for divr in 0..=15u8 {
            let f_pfd = f_in / (divr as f64 + 1.0);

            if f_pfd < self.min_pfd || f_pfd > self.max_pfd {
                continue;
            }

            for divf in 0..=127u8 {
                let f_vco = f_pfd * (divf as f64 + 1.0);

                if f_vco < self.min_vco || f_vco > self.max_vco {
                    continue;
                }

                for divq in 1..=6u8 {
                    let divisor = 1u32 << divq;
                    let f_achieved = f_vco / divisor as f64;
                    let error_percent = (f_achieved - f_out).abs() / f_out * 100.0;

                    // Only include configurations within 5% error
                    if error_percent > 5.0 {
                        continue;
                    }

                    let filter_range = self.calculate_filter_range(f_pfd);

                    configs.push(PllConfig {
                        f_in,
                        f_out,
                        divr,
                        divf,
                        divq,
                        filter_range,
                        f_achieved,
                        error_percent,
                        f_pfd,
                        f_vco,
                    });
                }
            }
        }

        // Sort by error and take top results
        configs.sort_by(|a, b| {
            a.error_percent
                .partial_cmp(&b.error_percent)
                .unwrap_or(std::cmp::Ordering::Equal)
        });
        configs.truncate(max_results);

        configs
    }

    /// Calculate filter range based on PFD frequency
    fn calculate_filter_range(&self, f_pfd: f64) -> u8 {
        // From iCE40 LP/HX datasheet Table 3-4
        if f_pfd < 17.0 {
            1
        } else if f_pfd < 26.0 {
            2
        } else if f_pfd < 44.0 {
            3
        } else if f_pfd < 66.0 {
            4
        } else if f_pfd < 101.0 {
            5
        } else {
            6
        }
    }
}

// ============================================================================
// BRAM Content Swapper (icebram equivalent)
// ============================================================================

/// BRAM content for a single RAM block
#[derive(Debug, Clone)]
pub struct BramContent {
    /// RAM block location (e.g., "ram_0_0")
    pub name: String,
    /// Tile X coordinate
    pub tile_x: u32,
    /// Tile Y coordinate
    pub tile_y: u32,
    /// RAM data (256 x 16-bit words = 4096 bits)
    pub data: Vec<u16>,
}

impl BramContent {
    /// Create empty BRAM content
    pub fn new(name: impl Into<String>, tile_x: u32, tile_y: u32) -> Self {
        Self {
            name: name.into(),
            tile_x,
            tile_y,
            data: vec![0; 256],
        }
    }

    /// Create from hex string (each line is a 16-bit word)
    pub fn from_hex_lines(name: impl Into<String>, tile_x: u32, tile_y: u32, hex: &str) -> Self {
        let mut content = Self::new(name, tile_x, tile_y);

        for (i, line) in hex.lines().enumerate() {
            if i >= 256 {
                break;
            }
            let line = line.trim();
            if !line.is_empty() {
                if let Ok(val) = u16::from_str_radix(line, 16) {
                    content.data[i] = val;
                }
            }
        }

        content
    }

    /// Convert to hex string format
    pub fn to_hex_lines(&self) -> String {
        self.data
            .iter()
            .map(|w| format!("{:04x}", w))
            .collect::<Vec<_>>()
            .join("\n")
    }
}

/// BRAM content swapper for updating firmware in bitstreams
pub struct BramSwapper {
    /// Original BRAM contents (from bitstream)
    original: HashMap<String, BramContent>,
    /// New BRAM contents (to replace)
    replacement: HashMap<String, BramContent>,
}

impl BramSwapper {
    /// Create a new BRAM swapper
    pub fn new() -> Self {
        Self {
            original: HashMap::new(),
            replacement: HashMap::new(),
        }
    }

    /// Add original BRAM content (from bitstream analysis)
    pub fn add_original(&mut self, content: BramContent) {
        self.original.insert(content.name.clone(), content);
    }

    /// Add replacement BRAM content
    pub fn add_replacement(&mut self, content: BramContent) {
        self.replacement.insert(content.name.clone(), content);
    }

    /// Load original content from hex file
    /// Format: one hex value per line, with "// ramX_Y" comments to identify blocks
    pub fn load_original_from_hex(&mut self, content: &str) -> Result<()> {
        self.parse_hex_content(content, true)
    }

    /// Load replacement content from hex file
    pub fn load_replacement_from_hex(&mut self, content: &str) -> Result<()> {
        self.parse_hex_content(content, false)
    }

    fn parse_hex_content(&mut self, content: &str, is_original: bool) -> Result<()> {
        let mut current_name: Option<String> = None;
        let mut current_x = 0u32;
        let mut current_y = 0u32;
        let mut current_data: Vec<u16> = Vec::new();

        for line in content.lines() {
            let line = line.trim();

            if line.starts_with("//") || line.starts_with("#") {
                // Comment with RAM block identifier
                if let Some(name_part) =
                    line.strip_prefix("// ").or_else(|| line.strip_prefix("# "))
                {
                    // Save previous block
                    if let Some(name) = current_name.take() {
                        let mut bram = BramContent::new(&name, current_x, current_y);
                        for (i, &val) in current_data.iter().enumerate() {
                            if i < 256 {
                                bram.data[i] = val;
                            }
                        }
                        if is_original {
                            self.original.insert(name, bram);
                        } else {
                            self.replacement.insert(name, bram);
                        }
                    }

                    // Parse new block name (format: "ramX_Y" or similar)
                    current_name = Some(name_part.to_string());
                    current_data.clear();

                    // Try to extract coordinates
                    if let Some(coords) = name_part.strip_prefix("ram") {
                        let parts: Vec<&str> = coords.split('_').collect();
                        if parts.len() >= 2 {
                            current_x = parts[0].parse().unwrap_or(0);
                            current_y = parts[1].parse().unwrap_or(0);
                        }
                    }
                }
            } else if !line.is_empty() {
                // Hex data
                if let Ok(val) = u16::from_str_radix(line, 16) {
                    current_data.push(val);
                }
            }
        }

        // Save last block
        if let Some(name) = current_name {
            let mut bram = BramContent::new(&name, current_x, current_y);
            for (i, &val) in current_data.iter().enumerate() {
                if i < 256 {
                    bram.data[i] = val;
                }
            }
            if is_original {
                self.original.insert(name, bram);
            } else {
                self.replacement.insert(name, bram);
            }
        }

        Ok(())
    }

    /// Swap BRAM contents in an ASCII bitstream
    pub fn swap_in_ascii(&self, asc_content: &str) -> Result<String> {
        let mut output = String::new();
        let mut in_ram_block = false;
        let mut current_ram_name: Option<String> = None;
        let mut ram_data_lines: Vec<String> = Vec::new();

        for line in asc_content.lines() {
            if line.starts_with(".ram_data") {
                // Start of RAM data block
                in_ram_block = true;
                // Extract RAM name from position info
                let parts: Vec<&str> = line.split_whitespace().collect();
                if parts.len() >= 3 {
                    current_ram_name = Some(format!("ram{}_{}", parts[1], parts[2]));
                }
                output.push_str(line);
                output.push('\n');
                ram_data_lines.clear();
            } else if in_ram_block {
                if line.starts_with('.') || line.is_empty() {
                    // End of RAM block - write data
                    if let Some(ref name) = current_ram_name {
                        if let Some(replacement) = self.replacement.get(name) {
                            // Write replacement data
                            for word in &replacement.data {
                                output.push_str(&format!("{:016b}\n", word));
                            }
                        } else {
                            // Write original data
                            for data_line in &ram_data_lines {
                                output.push_str(data_line);
                                output.push('\n');
                            }
                        }
                    }
                    in_ram_block = false;
                    current_ram_name = None;

                    if !line.is_empty() {
                        output.push_str(line);
                        output.push('\n');
                    }
                } else {
                    // Collect RAM data lines
                    ram_data_lines.push(line.to_string());
                }
            } else {
                output.push_str(line);
                output.push('\n');
            }
        }

        Ok(output)
    }

    /// Get list of RAM blocks found
    pub fn original_blocks(&self) -> Vec<&str> {
        self.original.keys().map(|s| s.as_str()).collect()
    }

    /// Get list of replacement blocks
    pub fn replacement_blocks(&self) -> Vec<&str> {
        self.replacement.keys().map(|s| s.as_str()).collect()
    }
}

impl Default for BramSwapper {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_pll_12mhz_to_48mhz() {
        let calc = PllCalculator::new();
        let config = calc.calculate(12.0, 48.0).unwrap();

        assert!(config.error_percent < 1.0);
        assert!((config.f_achieved - 48.0).abs() < 1.0);
        println!("12 MHz -> 48 MHz: {:?}", config);
        println!("{}", config.to_verilog());
    }

    #[test]
    fn test_pll_12mhz_to_100mhz() {
        let calc = PllCalculator::new();
        let config = calc.calculate(12.0, 100.0).unwrap();

        assert!(config.error_percent < 1.0);
        println!("12 MHz -> 100 MHz: {:?}", config);
    }

    #[test]
    fn test_pll_multiple_configs() {
        let calc = PllCalculator::new();
        let configs = calc.find_configurations(12.0, 48.0, 5);

        assert!(!configs.is_empty());
        for (i, config) in configs.iter().enumerate() {
            println!(
                "Config {}: {} MHz (error: {:.4}%)",
                i, config.f_achieved, config.error_percent
            );
        }
    }

    #[test]
    fn test_bram_content() {
        let hex = "0000\n0001\n0002\nFFFF";
        let bram = BramContent::from_hex_lines("ram0_0", 0, 0, hex);

        assert_eq!(bram.data[0], 0x0000);
        assert_eq!(bram.data[1], 0x0001);
        assert_eq!(bram.data[2], 0x0002);
        assert_eq!(bram.data[3], 0xFFFF);
    }

    #[test]
    fn test_bram_swapper() {
        let mut swapper = BramSwapper::new();

        let original = BramContent::new("ram0_0", 0, 0);
        let mut replacement = BramContent::new("ram0_0", 0, 0);
        replacement.data[0] = 0xDEAD;
        replacement.data[1] = 0xBEEF;

        swapper.add_original(original);
        swapper.add_replacement(replacement);

        assert_eq!(swapper.original_blocks().len(), 1);
        assert_eq!(swapper.replacement_blocks().len(), 1);
    }
}
