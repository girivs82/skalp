//! iCE40 ChipDB parser
//!
//! Parses IceStorm's chipdb text files to build the device database.
//! The chipdb files describe the FPGA architecture including tiles, wires, and PIPs.

use super::super::{PackagePins, TileType, WireType};
use super::{Ice40Device, Ice40Variant};
use crate::error::{PlaceRouteError, Result};
use std::collections::HashMap;
use std::path::Path;

/// Parsed chip database
#[derive(Debug, Clone)]
pub struct Ice40ChipDb {
    /// Device variant
    pub variant: Ice40Variant,
    /// Grid dimensions
    pub width: u32,
    pub height: u32,
    /// Package pin mappings
    pub packages: HashMap<String, PackagePins>,
}

impl Ice40ChipDb {
    /// Parse an IceStorm chipdb text file
    pub fn parse(content: &str, variant: Ice40Variant) -> Result<Self> {
        let mut width = 0u32;
        let mut height = 0u32;
        let mut packages = HashMap::new();
        let mut current_package: Option<String> = None;
        let mut package_pins = HashMap::new();

        for line in content.lines() {
            let line = line.trim();

            // Skip empty lines and comments
            if line.is_empty() || line.starts_with('#') {
                continue;
            }

            let parts: Vec<&str> = line.split_whitespace().collect();
            if parts.is_empty() {
                continue;
            }

            match parts[0] {
                ".device" => {
                    // Parse device dimensions
                    // Format: .device 1k/8k
                    // Or: .device ice40hx8k
                }
                ".io_tile" | ".logic_tile" | ".ramb_tile" | ".ramt_tile" => {
                    // Parse tile coordinates
                    if parts.len() >= 3 {
                        let x: u32 = parts[1].parse().unwrap_or(0);
                        let y: u32 = parts[2].parse().unwrap_or(0);
                        width = width.max(x + 1);
                        height = height.max(y + 1);
                    }
                }
                ".pins" => {
                    // Start of pin section
                    if parts.len() >= 2 {
                        // Save previous package
                        if let Some(pkg) = current_package.take() {
                            packages.insert(
                                pkg.clone(),
                                PackagePins {
                                    name: pkg,
                                    pins: std::mem::take(&mut package_pins),
                                },
                            );
                        }
                        current_package = Some(parts[1].to_string());
                    }
                }
                _ => {
                    // Check for pin definitions
                    // Format: <pin_name> <tile_x> <tile_y> <bel_idx>
                    if let Some(ref _pkg) = current_package {
                        if parts.len() >= 4 {
                            if let (Ok(x), Ok(y), Ok(idx)) = (
                                parts[1].parse::<u32>(),
                                parts[2].parse::<u32>(),
                                parts[3].parse::<u8>(),
                            ) {
                                package_pins.insert(parts[0].to_string(), (x, y, idx));
                            }
                        }
                    }
                }
            }
        }

        // Save last package
        if let Some(pkg) = current_package {
            packages.insert(
                pkg.clone(),
                PackagePins {
                    name: pkg,
                    pins: package_pins,
                },
            );
        }

        // Use default dimensions if not parsed
        if width == 0 || height == 0 {
            let (w, h) = variant.grid_size();
            width = w;
            height = h;
        }

        Ok(Self {
            variant,
            width,
            height,
            packages,
        })
    }

    /// Load from an IceStorm chipdb file
    pub fn load_from_file(path: &Path, variant: Ice40Variant) -> Result<Self> {
        let content = std::fs::read_to_string(path).map_err(|e| {
            PlaceRouteError::ChipDbLoad(format!("Failed to read chipdb file: {}", e))
        })?;
        Self::parse(&content, variant)
    }

    /// Create an Ice40Device from this chipdb
    pub fn into_device(self) -> Ice40Device {
        let mut device = Ice40Device::new(self.variant);

        // Merge package pins from chipdb
        for (name, pins) in self.packages {
            device.packages.insert(name, pins);
        }

        device
    }
}

/// Wire type determination from IceStorm wire names
#[allow(dead_code)]
pub fn parse_wire_type(name: &str) -> WireType {
    if name.starts_with("local_g") {
        let idx = name
            .chars()
            .skip(7)
            .take_while(|c| c.is_ascii_digit())
            .collect::<String>()
            .parse()
            .unwrap_or(0);
        WireType::Local(idx)
    } else if name.starts_with("sp4_h") {
        let idx = name
            .chars()
            .rev()
            .take_while(|c| c.is_ascii_digit())
            .collect::<String>()
            .chars()
            .rev()
            .collect::<String>()
            .parse()
            .unwrap_or(0);
        WireType::Span4H(idx)
    } else if name.starts_with("sp4_v") {
        let idx = name
            .chars()
            .rev()
            .take_while(|c| c.is_ascii_digit())
            .collect::<String>()
            .chars()
            .rev()
            .collect::<String>()
            .parse()
            .unwrap_or(0);
        WireType::Span4V(idx)
    } else if name.starts_with("sp12_h") {
        let idx = name
            .chars()
            .rev()
            .take_while(|c| c.is_ascii_digit())
            .collect::<String>()
            .chars()
            .rev()
            .collect::<String>()
            .parse()
            .unwrap_or(0);
        WireType::Span12H(idx)
    } else if name.starts_with("sp12_v") {
        let idx = name
            .chars()
            .rev()
            .take_while(|c| c.is_ascii_digit())
            .collect::<String>()
            .chars()
            .rev()
            .collect::<String>()
            .parse()
            .unwrap_or(0);
        WireType::Span12V(idx)
    } else if name.starts_with("glb") {
        let idx = name
            .chars()
            .skip(3)
            .take_while(|c| c.is_ascii_digit())
            .collect::<String>()
            .parse()
            .unwrap_or(0);
        WireType::Global(idx)
    } else if name.starts_with("neigh") {
        WireType::Neighbour
    } else if name.starts_with("carry") {
        WireType::CarryChain
    } else {
        WireType::Local(0)
    }
}

/// Tile type determination from IceStorm tile names
#[allow(dead_code)]
pub fn parse_tile_type(tile_type: &str) -> TileType {
    match tile_type {
        "logic" | ".logic_tile" => TileType::Logic,
        "io_0" | "io_top" => TileType::IoTop,
        "io_1" | "io_bot" => TileType::IoBottom,
        "io_2" | "io_left" => TileType::IoLeft,
        "io_3" | "io_right" => TileType::IoRight,
        "ramb" | ".ramb_tile" => TileType::RamBottom,
        "ramt" | ".ramt_tile" => TileType::RamTop,
        "dsp" => TileType::Dsp,
        _ => TileType::Empty,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_wire_type() {
        assert!(matches!(parse_wire_type("local_g0"), WireType::Local(0)));
        assert!(matches!(parse_wire_type("local_g7"), WireType::Local(7)));
        assert!(matches!(parse_wire_type("sp4_h_r_0"), WireType::Span4H(0)));
        assert!(matches!(parse_wire_type("sp4_v_b_3"), WireType::Span4V(3)));
        assert!(matches!(
            parse_wire_type("sp12_h_r_1"),
            WireType::Span12H(1)
        ));
        assert!(matches!(parse_wire_type("glb2"), WireType::Global(2)));
        assert!(matches!(parse_wire_type("neigh_op"), WireType::Neighbour));
        assert!(matches!(parse_wire_type("carry_in"), WireType::CarryChain));
    }

    #[test]
    fn test_parse_tile_type() {
        assert_eq!(parse_tile_type("logic"), TileType::Logic);
        assert_eq!(parse_tile_type(".logic_tile"), TileType::Logic);
        assert_eq!(parse_tile_type("io_top"), TileType::IoTop);
        assert_eq!(parse_tile_type("ramb"), TileType::RamBottom);
        assert_eq!(parse_tile_type("ramt"), TileType::RamTop);
    }
}
