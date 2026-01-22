//! Configuration RAM (CRAM) Model
//!
//! Models the configuration memory of iCE40 FPGAs.

use crate::device::ice40::Ice40Device;
use crate::device::{BelType, Device};
use crate::placer::PlacementResult;
use crate::router::RoutingResult;
use serde::{Deserialize, Serialize};
use skalp_lir::gate_netlist::GateNetlist;
use std::collections::HashMap;

/// CRAM bank
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CramBank {
    /// Bank number
    pub bank_num: u8,
    /// Rows in this bank
    pub rows: Vec<CramRow>,
}

/// CRAM row
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CramRow {
    /// Row index
    pub row_idx: u16,
    /// Configuration bits
    pub bits: Vec<u8>,
}

/// Configuration RAM for the entire device
#[derive(Debug, Clone)]
pub struct ConfigRam {
    /// CRAM banks
    pub banks: Vec<CramBank>,
    /// Device dimensions
    width: u32,
    height: u32,
    /// Tile configuration bits
    tile_config: HashMap<(u32, u32), TileConfig>,
}

/// Configuration for a single tile
#[derive(Debug, Clone, Default)]
pub struct TileConfig {
    /// LUT truth tables (8 LUTs per tile, 16 bits each for LUT4)
    pub lut_init: [u16; 8],
    /// FF configuration bits
    pub ff_config: [u8; 8],
    /// Carry chain enable
    pub carry_enable: bool,
    /// Local routing mux settings
    pub routing_mux: Vec<u8>,
    /// I/O configuration (for I/O tiles)
    pub io_config: Option<IoConfig>,
    /// RAM configuration (for RAM tiles)
    pub ram_config: Option<RamConfig>,
}

/// I/O cell configuration
#[derive(Debug, Clone)]
pub struct IoConfig {
    /// Pin is output
    pub is_output: bool,
    /// Pin is input
    pub is_input: bool,
    /// Output enable
    pub output_enable: bool,
    /// I/O standard
    pub io_standard: u8,
    /// Drive strength
    pub drive_strength: u8,
    /// Pull-up enable
    pub pullup: bool,
}

/// RAM configuration
#[derive(Debug, Clone)]
pub struct RamConfig {
    /// Write mode
    pub write_mode: u8,
    /// Read mode
    pub read_mode: u8,
    /// Initial contents
    pub init_data: Vec<u8>,
}

impl ConfigRam {
    /// Create a new CRAM for the given device
    pub fn new(device: &Ice40Device) -> Self {
        let (width, height) = device.grid_size();

        // Create banks based on device size
        let num_banks = 4; // iCE40 typically has 4 CRAM banks
        let rows_per_bank = (height as u16 * 16) / num_banks as u16;

        let banks = (0..num_banks)
            .map(|bank_num| {
                let rows = (0..rows_per_bank)
                    .map(|row_idx| CramRow {
                        row_idx,
                        bits: vec![0; (width as usize) * 54], // 54 bits per column
                    })
                    .collect();
                CramBank {
                    bank_num: bank_num as u8,
                    rows,
                }
            })
            .collect();

        Self {
            banks,
            width,
            height,
            tile_config: HashMap::new(),
        }
    }

    /// Configure a LUT
    pub fn configure_lut(&mut self, x: u32, y: u32, lut_idx: usize, init: u16) {
        let config = self.tile_config.entry((x, y)).or_default();
        if lut_idx < 8 {
            config.lut_init[lut_idx] = init;
        }
    }

    /// Configure a flip-flop
    pub fn configure_ff(&mut self, x: u32, y: u32, ff_idx: usize, cfg: u8) {
        let config = self.tile_config.entry((x, y)).or_default();
        if ff_idx < 8 {
            config.ff_config[ff_idx] = cfg;
        }
    }

    /// Configure carry chain
    pub fn configure_carry(&mut self, x: u32, y: u32, enable: bool) {
        let config = self.tile_config.entry((x, y)).or_default();
        config.carry_enable = enable;
    }

    /// Configure I/O
    pub fn configure_io(&mut self, x: u32, y: u32, io_cfg: IoConfig) {
        let config = self.tile_config.entry((x, y)).or_default();
        config.io_config = Some(io_cfg);
    }

    /// Configure RAM
    pub fn configure_ram(&mut self, x: u32, y: u32, ram_cfg: RamConfig) {
        let config = self.tile_config.entry((x, y)).or_default();
        config.ram_config = Some(ram_cfg);
    }

    /// Configure routing mux
    pub fn configure_routing(&mut self, x: u32, y: u32, mux_settings: Vec<u8>) {
        let config = self.tile_config.entry((x, y)).or_default();
        config.routing_mux = mux_settings;
    }

    /// Get tile configuration
    pub fn get_tile_config(&self, x: u32, y: u32) -> Option<&TileConfig> {
        self.tile_config.get(&(x, y))
    }

    /// Serialize to binary format
    pub fn to_binary(&self) -> Vec<u8> {
        let mut data = Vec::new();

        for bank in &self.banks {
            for row in &bank.rows {
                data.extend_from_slice(&row.bits);
            }
        }

        data
    }

    /// Serialize to ASCII format (for .asc files)
    pub fn to_ascii(&self) -> String {
        let mut ascii = String::new();

        for y in 0..self.height {
            for x in 0..self.width {
                if let Some(config) = self.tile_config.get(&(x, y)) {
                    // Output tile configuration
                    ascii.push_str(&format!(".logic_tile {} {}\n", x, y));

                    // LUT configuration
                    for (i, &init) in config.lut_init.iter().enumerate() {
                        if init != 0 {
                            ascii.push_str(&format!("LC_{} {:016b}\n", i, init));
                        }
                    }

                    // Routing mux configuration
                    for (i, &mux) in config.routing_mux.iter().enumerate() {
                        if mux != 0 {
                            ascii.push_str(&format!("routing_mux_{} {:08b}\n", i, mux));
                        }
                    }

                    ascii.push('\n');
                }
            }
        }

        ascii
    }

    /// Populate CRAM from placement and routing results
    pub fn populate(
        &mut self,
        device: &Ice40Device,
        netlist: &GateNetlist,
        placement: &PlacementResult,
        routing: &RoutingResult,
    ) {
        // Configure cells from placement
        for (&cell_id, loc) in &placement.placements {
            if let Some(cell) = netlist.get_cell(cell_id) {
                match loc.bel_type {
                    BelType::Lut4 => {
                        // Get LUT init value from cell - default to 0 if not specified
                        // In a real implementation, this would come from the cell's attributes
                        let init: u16 = 0;
                        self.configure_lut(loc.tile_x, loc.tile_y, loc.bel_index, init);
                    }
                    BelType::Dff | BelType::DffE | BelType::DffSr | BelType::DffSrE => {
                        // Configure FF
                        let ff_cfg = match loc.bel_type {
                            BelType::DffE => 0x01,   // Enable
                            BelType::DffSr => 0x02,  // Sync reset
                            BelType::DffSrE => 0x03, // Enable + sync reset
                            _ => 0x00,
                        };
                        self.configure_ff(loc.tile_x, loc.tile_y, loc.bel_index, ff_cfg);
                    }
                    BelType::Carry => {
                        self.configure_carry(loc.tile_x, loc.tile_y, true);
                    }
                    BelType::IoCell => {
                        // Determine I/O direction from net connectivity
                        let is_output = cell.outputs.iter().any(|&o| {
                            netlist
                                .nets
                                .get(o.0 as usize)
                                .map(|n| n.is_output)
                                .unwrap_or(false)
                        });
                        let is_input = cell.inputs.iter().any(|&i| {
                            netlist
                                .nets
                                .get(i.0 as usize)
                                .map(|n| n.is_input)
                                .unwrap_or(false)
                        });

                        self.configure_io(
                            loc.tile_x,
                            loc.tile_y,
                            IoConfig {
                                is_output,
                                is_input,
                                output_enable: is_output,
                                io_standard: 0, // LVCMOS33
                                drive_strength: 8,
                                pullup: false,
                            },
                        );
                    }
                    BelType::RamSlice => {
                        self.configure_ram(
                            loc.tile_x,
                            loc.tile_y,
                            RamConfig {
                                write_mode: 0,
                                read_mode: 0,
                                init_data: Vec::new(),
                            },
                        );
                    }
                    _ => {}
                }
            }
        }

        // Configure routing from routing results
        for route in routing.routes.values() {
            for &pip_id in &route.pips {
                if let Some(pip) = device.pip(pip_id) {
                    if pip.configurable {
                        // Add PIP to routing configuration
                        let config = self
                            .tile_config
                            .entry((pip.tile_x, pip.tile_y))
                            .or_default();
                        // Encode PIP as mux setting
                        let mux_setting = (pip.src_wire.0 & 0xFF) as u8;
                        config.routing_mux.push(mux_setting);
                    }
                }
            }
        }
    }
}
