//! IceStorm ASCII (.asc) Format Generator
//!
//! Generates human-readable ASCII bitstream format compatible with IceStorm tools.
//! The .asc format is documented at: https://clifford.at/icestorm/
//!
//! Format overview:
//! - .device <name>
//! - .comment <text>
//! - .logic_tile X Y followed by 16 rows of 54 bits each
//! - .io_tile X Y followed by 16 rows of 18 bits each
//! - .ramb_tile/.ramt_tile X Y followed by 16 rows of 42 bits each

use crate::device::ice40::{chipdb_parser::ChipDb, Ice40Device, Ice40Variant};
use crate::device::{BelType, Device, TileType};
use crate::error::Result;
use crate::placer::PlacementResult;
use crate::router::RoutingResult;
use skalp_lir::gate_netlist::GateNetlist;
use skalp_lir::tech_library::CellFunction;
use std::collections::{HashMap, HashSet};

/// DFF configuration bits for an LC
/// In iCE40, each LC has 20 configuration bits: 16 for LUT init, 4 for DFF config
#[derive(Debug, Clone, Copy, Default)]
struct DffConfig {
    /// Bit 16: Use negative edge of clock
    neg_clk: bool,
    /// Bit 17: Use carry chain enable
    carry_enable: bool,
    /// Bit 18: Use DFF output (vs combinational)
    dff_enable: bool,
    /// Bit 19: Async set/reset mode
    set_no_reset: bool,
}

/// I/O cell configuration for iCE40
/// Each I/O tile has 2 IOBs (IOB_0 and IOB_1) with 6-bit PINTYPE configuration.
///
/// PIN_TYPE encoding (from iCE40 LP/HX Family Data Sheet, Table 3.3):
///   [1:0] = Output Enable Select: 00=none, 01=always, 10=OE registered, 11=OE fabric
///   [3:2] = Output Driver Select: 00=D_OUT_0, 01=registered, 10=DDR, 11=reserved
///   [5:4] = Input Pin Select: 00=registered, 01=simple input, 10=DDR, 11=latch
#[derive(Debug, Clone, Copy, Default)]
struct IoConfig {
    /// PINTYPE[1:0]: Output enable select (0=none, 1=always, 2=OE registered, 3=OE fabric)
    output_enable: u8,
    /// PINTYPE[3:2]: Output driver select (0=DQ, 1=registered, 2=DDR, 3=reserved)
    output_driver: u8,
    /// PINTYPE[5:4]: Input pin select (0=registered, 1=simple, 2=DDR, 3=latch)
    input_pin: u8,
    /// Input enable (IE bit — enables input buffer)
    input_enable: bool,
    /// Pull-up resistor enable
    pullup_enable: bool,
}

/// Global network configuration
/// Tracks which global networks (0-7) are in use
#[derive(Debug, Clone, Default)]
struct GlobalNetworkConfig {
    /// Bitmask of active global networks (bit N = glb_netwk_N is active)
    active_networks: u8,
}

/// RAM block configuration for SB_RAM40_4K
#[derive(Debug, Clone, Default)]
struct RamConfig {
    /// Read mode: 0=256x16, 1=512x8, 2=1024x4, 3=2048x2
    read_mode: u8,
    /// Write mode: 0=256x16, 1=512x8, 2=1024x4, 3=2048x2
    write_mode: u8,
    /// Initialization data (4096 bits = 512 bytes)
    init_data: Option<Vec<u8>>,
}

/// PLL configuration for bitstream generation
#[derive(Debug, Clone)]
struct PllBitstreamConfig {
    /// DIVR (reference divider, 4-bit, value 0-15)
    divr: u8,
    /// DIVF (feedback divider, 7-bit, value 0-127)
    divf: u8,
    /// DIVQ (output divider, 3-bit, value 1-6)
    divq: u8,
    /// FILTER_RANGE (3-bit)
    filter_range: u8,
    /// Feedback path: 0=SIMPLE, 1=DELAY, 2=PHASE_AND_DELAY, 4=EXTERNAL
    feedback_path: u8,
    /// PLLTYPE (3-bit): 0=SB_PLL40_CORE, 1=SB_PLL40_PAD, etc.
    plltype: u8,
}

impl IoConfig {
    /// Create config for simple input
    fn simple_input() -> Self {
        Self {
            output_enable: 0,   // No output
            output_driver: 0,   // Not used
            input_pin: 1,       // Simple input (PIN_TYPE[5:4] = 01)
            input_enable: true, // Enable input buffer
            pullup_enable: false,
        }
    }

    /// Create config for simple output
    fn simple_output() -> Self {
        Self {
            output_enable: 1,    // Output always enabled (PIN_TYPE[1:0] = 01)
            output_driver: 0,    // D_OUT_0 combinational (PIN_TYPE[3:2] = 00)
            input_pin: 0,        // Not used
            input_enable: false,  // No input
            pullup_enable: false,
        }
    }

    /// Create config for bidirectional I/O
    fn bidirectional() -> Self {
        Self {
            output_enable: 3,   // Output enable from fabric (PIN_TYPE[1:0] = 11)
            output_driver: 0,   // D_OUT_0 combinational (PIN_TYPE[3:2] = 00)
            input_pin: 1,       // Simple input (PIN_TYPE[5:4] = 01)
            input_enable: true, // Enable input buffer
            pullup_enable: false,
        }
    }

    /// Get the 6-bit PINTYPE value
    /// Layout: [5:4]=input_pin, [3:2]=output_driver, [1:0]=output_enable
    fn pintype(&self) -> u8 {
        ((self.input_pin & 0x3) << 4)
            | ((self.output_driver & 0x3) << 2)
            | (self.output_enable & 0x3)
    }
}

/// Return the Y coordinates of logic/IO tiles that host column buffer control bits.
/// These tiles distribute global signals and need ColBufCtrl bits set.
/// Derived from the .colbuf section of the IceStorm chipdb.
fn colbuf_source_rows(grid_height: u32) -> [u32; 4] {
    match grid_height {
        18 => [4, 5, 12, 13],       // HX1K / LP1K (14x18)
        34 => [8, 9, 25, 26],       // HX8K / LP8K (34x34)
        33 => [8, 9, 24, 25],       // UP5K (26x33)
        _ => {
            // Conservative fallback: compute quadrant boundaries
            let q = grid_height / 4;
            [q, q + 1, 3 * q, 3 * q + 1]
        }
    }
}

/// Return the Y coordinates of ramb tiles that host column buffer control bits.
/// RAM columns have different colbuf rows than logic columns because RAM tiles
/// occupy odd rows (ramb at odd y, ramt at even y).
/// Derived from the .colbuf section of the IceStorm chipdb for RAM columns.
fn ramb_colbuf_source_rows(grid_height: u32) -> [u32; 4] {
    match grid_height {
        18 => [3, 5, 11, 13],       // HX1K / LP1K
        34 => [7, 9, 23, 25],       // HX8K / LP8K
        33 => [7, 9, 23, 25],       // UP5K
        _ => {
            let q = grid_height / 4;
            [q - 1, q + 1, 3 * q - 1, 3 * q + 1]
        }
    }
}

/// IceStorm ASCII format generator
pub struct IceStormAscii<'a> {
    device: &'a Ice40Device,
    chipdb: Option<ChipDb>,
    /// IO tile positions that have physical pads (derived from chipdb package pins).
    /// Tiles NOT in this set should skip IE defaults.
    padded_io_tiles: HashSet<(u32, u32)>,
}

impl<'a> IceStormAscii<'a> {
    /// Create a new ASCII generator
    pub fn new(device: &'a Ice40Device) -> Self {
        // Try to load chipdb for real bit mappings
        let chipdb = ChipDb::load_embedded(device.variant).ok();

        // Derive which IO tiles have physical pads from chipdb package pin data.
        // Union all packages: any tile that has a pin in ANY package has a pad.
        let mut padded_io_tiles = HashSet::new();
        if let Some(ref db) = chipdb {
            for pins in db.packages.values() {
                for pin in pins {
                    padded_io_tiles.insert((pin.tile_x, pin.tile_y));
                }
            }
        }

        Self {
            device,
            chipdb,
            padded_io_tiles,
        }
    }

    /// Generate ASCII bitstream
    pub fn generate(
        &self,
        placement: &PlacementResult,
        routing: &RoutingResult,
        netlist: Option<&GateNetlist>,
    ) -> Result<String> {
        let mut asc = String::new();
        let (width, height) = self.device.grid_size();

        // Device name for IceStorm (e.g., "1k", "8k")
        let device_name = match self.device.variant {
            Ice40Variant::Hx1k | Ice40Variant::Lp1k => "1k",
            Ice40Variant::Hx4k | Ice40Variant::Lp4k => "5k",
            Ice40Variant::Hx8k | Ice40Variant::Lp8k => "8k",
            Ice40Variant::Up5k => "up5k",
        };

        // Header - no blank line after .comment (icebox parser requirement)
        asc.push_str(&format!(".device {}\n", device_name));
        asc.push_str(".comment Generated by SKALP Native Place & Route\n");

        // Collect LUT init values from placement and netlist
        let lut_inits = self.collect_lut_inits(placement, netlist);

        // Collect DFF configurations from placement and netlist
        let dff_configs = self.collect_dff_configs(placement, netlist);

        // Collect I/O configurations from placement and netlist
        let io_configs = self.collect_io_configs(placement, netlist);

        // Collect tiles that need CarryInSet (carry chain head with CI = VCC)
        let carry_in_set_tiles = self.collect_carry_in_set_tiles(placement, netlist);

        // Collect global network configuration
        let global_config = self.collect_global_config(placement, netlist);

        // Collect RAM configurations from placement and netlist
        let ram_configs = self.collect_ram_configs(placement, netlist);

        // Collect PLL configurations from placement and netlist
        let pll_configs = self.collect_pll_configs(placement, netlist);

        // Resolve PLL config bits to target tile coordinates
        // For HX/LP devices, these target IO tiles; for UP5K, they target ipcon tiles
        let pll_resolved_bits = self.resolve_pll_bits(&pll_configs);

        // Generate tile configurations
        for y in 0..height {
            for x in 0..width {
                if let Some(tile) = self.device.tile_at(x, y) {
                    match tile.tile_type() {
                        TileType::Logic => {
                            self.generate_logic_tile(
                                &mut asc,
                                x,
                                y,
                                placement,
                                routing,
                                &lut_inits,
                                &dff_configs,
                                &global_config,
                                &carry_in_set_tiles,
                            );
                        }
                        TileType::IoTop
                        | TileType::IoBottom
                        | TileType::IoLeft
                        | TileType::IoRight => {
                            self.generate_io_tile(
                                &mut asc,
                                x,
                                y,
                                placement,
                                routing,
                                &io_configs,
                                &pll_resolved_bits,
                                &global_config,
                            );
                        }
                        TileType::RamTop => {
                            self.generate_ramt_tile(&mut asc, x, y, placement, &ram_configs);
                        }
                        TileType::RamBottom => {
                            self.generate_ramb_tile(
                                &mut asc,
                                x,
                                y,
                                placement,
                                &ram_configs,
                                &global_config,
                            );
                        }
                        TileType::Pll => {
                            self.generate_pll_tile(&mut asc, x, y, &pll_resolved_bits);
                        }
                        TileType::Dsp => {
                            self.generate_dsp_tile(&mut asc, x, y, placement);
                        }
                        _ => {}
                    }
                }
            }
        }

        // Emit .ipcon_tile sections for PLL bits that target tiles without
        // their own tile type (i.e., PLL bits resolved to non-Pll, non-IO tiles).
        // For HX/LP, PLL bits target IO tiles and are already included above.
        // For UP5K, PLL bits target ipcon tiles which are TileType::Pll in the grid.
        // This handles edge cases where resolved bits target uncovered tiles.
        for ((tx, ty), bits_to_set) in &pll_resolved_bits {
            // Skip tiles already handled in the main loop
            if let Some(tile) = self.device.tile_at(*tx, *ty) {
                match tile.tile_type() {
                    TileType::IoTop
                    | TileType::IoBottom
                    | TileType::IoLeft
                    | TileType::IoRight
                    | TileType::Pll => continue,
                    _ => {}
                }
            }
            // Emit standalone .ipcon_tile for bits targeting other tile positions
            if let Some(ref chipdb) = self.chipdb {
                let dims = chipdb
                    .tile_dimensions
                    .get(&TileType::Pll)
                    .or_else(|| chipdb.tile_dimensions.get(&TileType::IoBottom))
                    .copied()
                    .unwrap_or(crate::device::ice40::chipdb_parser::TileBitDimensions {
                        columns: 18,
                        rows: 16,
                    });
                asc.push_str(&format!(".ipcon_tile {} {}\n", tx, ty));
                let mut bits = vec![vec![false; dims.columns as usize]; dims.rows as usize];
                for &(row, col) in bits_to_set {
                    if (row as usize) < bits.len() && (col as usize) < bits[0].len() {
                        bits[row as usize][col as usize] = true;
                    }
                }
                for row in &bits {
                    for &bit in row {
                        asc.push(if bit { '1' } else { '0' });
                    }
                    asc.push('\n');
                }
                asc.push('\n');
            }
        }

        Ok(asc)
    }

    /// Collect LUT init values from placement and netlist
    fn collect_lut_inits(
        &self,
        placement: &PlacementResult,
        netlist: Option<&GateNetlist>,
    ) -> HashMap<(u32, u32, usize), u16> {
        let mut lut_inits = HashMap::new();

        for (cell_id, loc) in &placement.placements {
            if matches!(loc.bel_type, BelType::Lut4) {
                // Get LUT init value from netlist cell if available
                let init = if let Some(netlist) = netlist {
                    // Find the cell by ID in the netlist
                    netlist
                        .cells
                        .iter()
                        .find(|c| c.id.0 == cell_id.0)
                        .map(|cell| {
                            // If lut_init is set, use it; otherwise derive from cell type
                            cell.lut_init
                                .map(|init| init as u16)
                                .unwrap_or_else(|| Self::derive_lut_init(&cell.cell_type))
                        })
                        .unwrap_or(0x0000)
                } else {
                    0x0000
                };
                // Convert bel_index to LC index:
                // - Normal LUTs: even bel indices (0, 2, 4, ..., 14) → lc_idx = bel_index / 2
                // - Carry-associated LUTs from legalization: same bel_index as carry (0-14)
                // - Fallback carry LUTs at bel_index=16: map to LC 0 in the tile
                let lc_idx = if loc.bel_index >= 16 { 0 } else { loc.bel_index / 2 };
                lut_inits.insert((loc.tile_x, loc.tile_y, lc_idx), init);
            }
        }

        lut_inits
    }

    /// Derive LUT4 init value from cell type name
    /// Uses standard truth tables for common logic functions
    fn derive_lut_init(cell_type: &str) -> u16 {
        // LUT4 truth table: 16 bits where bit N corresponds to input pattern N
        // For 4 inputs (I3,I2,I1,I0), N = (I3<<3) | (I2<<2) | (I1<<1) | I0
        match cell_type {
            // 1-input functions (use I0)
            "SB_LUT4_NOT" | "INV" => 0x5555, // ~I0

            // 2-input functions (use I0, I1)
            "SB_LUT4_AND2" | "AND2" => 0x8888,   // I0 & I1
            "SB_LUT4_OR2" | "OR2" => 0xEEEE,     // I0 | I1
            "SB_LUT4_XOR2" | "XOR2" => 0x6666,   // I0 ^ I1
            "SB_LUT4_NAND2" | "NAND2" => 0x7777, // ~(I0 & I1)
            "SB_LUT4_NOR2" | "NOR2" => 0x1111,   // ~(I0 | I1)
            "SB_LUT4_XNOR2" | "XNOR2" => 0x9999, // ~(I0 ^ I1)

            // 3-input functions
            "SB_LUT4_AND3" | "AND3" => 0x8080, // I0 & I1 & I2
            "SB_LUT4_OR3" | "OR3" => 0xFEFE,   // I0 | I1 | I2
            "SB_LUT4_XOR3" | "XOR3" => 0x6996, // I0 ^ I1 ^ I2

            // 4-input functions
            "SB_LUT4_AND4" | "AND4" => 0x8000, // I0 & I1 & I2 & I3
            "SB_LUT4_OR4" | "OR4" => 0xFFFE,   // I0 | I1 | I2 | I3
            "SB_LUT4_XOR4" | "XOR4" => 0x6996, // I0 ^ I1 ^ I2 ^ I3

            // MUX functions
            "SB_LUT4_MUX2" | "MUX2" => 0xCACA, // I2 ? I1 : I0

            // Buffer/passthrough
            "SB_LUT4_BUF" | "BUF" => 0xAAAA, // I0 (passthrough)

            // Constant outputs
            "TIE_HIGH" | "VCC" => 0xFFFF,
            "TIE_LOW" | "GND" => 0x0000,

            // Default: passthrough I0 (acts as buffer)
            _ => 0xAAAA,
        }
    }

    /// Collect DFF configurations from placement and netlist
    fn collect_dff_configs(
        &self,
        placement: &PlacementResult,
        netlist: Option<&GateNetlist>,
    ) -> HashMap<(u32, u32, usize), DffConfig> {
        let mut dff_configs = HashMap::new();

        for (cell_id, loc) in &placement.placements {
            // Check if this is a DFF cell type
            let is_dff = matches!(
                loc.bel_type,
                BelType::Dff | BelType::DffE | BelType::DffSr | BelType::DffSrE
            );

            if is_dff {
                // Get cell type from netlist to determine specific DFF configuration
                let config = if let Some(netlist) = netlist {
                    netlist
                        .cells
                        .iter()
                        .find(|c| c.id.0 == cell_id.0)
                        .map(|cell| Self::derive_dff_config(&cell.cell_type))
                        .unwrap_or_else(|| DffConfig {
                            dff_enable: true,
                            ..Default::default()
                        })
                } else {
                    DffConfig {
                        dff_enable: true,
                        ..Default::default()
                    }
                };
                // Convert bel_index to LC index: DFFs are at odd indices (1, 3, 5, ...)
                // LC_idx = bel_index / 2
                let lc_idx = loc.bel_index / 2;
                dff_configs.insert((loc.tile_x, loc.tile_y, lc_idx), config);
            }

            // Also check for carry cells (they set carry_enable bit)
            if matches!(loc.bel_type, BelType::Carry) {
                if loc.bel_index < 16 {
                    // Carry placed at specific LC by chain legalization (bel_index = 2*lc).
                    // Only set carry_enable on that specific LC.
                    let lc_idx = loc.bel_index / 2;
                    let entry = dff_configs
                        .entry((loc.tile_x, loc.tile_y, lc_idx))
                        .or_insert(DffConfig::default());
                    entry.carry_enable = true;
                } else {
                    // Carry placed at tile-level BEL (index 16) by fallback path.
                    // We don't know which specific LC, so enable carry on LC 0
                    // (the carry chain always starts from LC 0 within a tile).
                    let entry = dff_configs
                        .entry((loc.tile_x, loc.tile_y, 0))
                        .or_insert(DffConfig::default());
                    entry.carry_enable = true;
                }
            }
        }

        dff_configs
    }

    /// Detect tiles that need CarryInSet (carry chain head with carry-in = 1).
    ///
    /// CarryInSet (B1[50] in logic tile) forces the carry-in of LC 0 in a tile
    /// to 1 instead of 0. This is needed at the start of carry chains that
    /// require an initial carry of 1 (e.g., +1 counters).
    ///
    /// Detection: find carry cells that are NOT driven by another carry cell
    /// (chain heads), then check if their CI input is driven by a constant-1
    /// source (VCC/TIE_HIGH).
    fn collect_carry_in_set_tiles(
        &self,
        placement: &PlacementResult,
        netlist: Option<&GateNetlist>,
    ) -> HashSet<(u32, u32)> {
        let mut carry_in_set_tiles = HashSet::new();

        let netlist = match netlist {
            Some(n) => n,
            None => return carry_in_set_tiles,
        };

        // Find all carry cells and their locations
        let carry_cells: Vec<_> = placement
            .placements
            .iter()
            .filter(|(_, loc)| matches!(loc.bel_type, BelType::Carry))
            .collect();

        // Build set of carry cell IDs for quick lookup
        let carry_cell_ids: HashSet<_> = carry_cells.iter().map(|(id, _)| **id).collect();

        // For each carry cell, check if it's a chain head (CI not driven by another carry)
        for (&cell_id, loc) in &carry_cells {
            if let Some(cell) = netlist.cells.iter().find(|c| c.id.0 == cell_id.0) {
                // SB_CARRY has 3 inputs: I0, I1, CI. CI is typically the last input.
                // Check if CI is driven by a constant source or by a non-carry cell.
                let ci_input_idx = if cell.inputs.len() >= 3 { 2 } else { continue };
                let ci_net_id = cell.inputs[ci_input_idx];

                if let Some(ci_net) = netlist.nets.iter().find(|n| n.id == ci_net_id) {
                    let ci_driven_by_carry = ci_net
                        .driver
                        .map(|drv| carry_cell_ids.contains(&drv))
                        .unwrap_or(false);

                    if !ci_driven_by_carry {
                        // This is a chain head. Check if CI is driven by VCC/constant-1.
                        let ci_is_vcc = ci_net
                            .driver
                            .and_then(|drv| netlist.cells.iter().find(|c| c.id == drv))
                            .map(|driver_cell| {
                                driver_cell.cell_type.contains("VCC")
                                    || driver_cell.cell_type.contains("TIE_HIGH")
                                    || driver_cell.cell_type == "SB_LUT4_BUF"
                            })
                            .unwrap_or(false);

                        if ci_is_vcc {
                            carry_in_set_tiles.insert((loc.tile_x, loc.tile_y));
                        }
                    }
                }
            }
        }

        carry_in_set_tiles
    }

    /// Derive DFF configuration from cell type name
    /// Maps iCE40 DFF cell types to their configuration bits
    fn derive_dff_config(cell_type: &str) -> DffConfig {
        // DFF configuration bits:
        // - neg_clk: Use falling edge of clock (SB_DFFN* variants)
        // - carry_enable: Use carry chain (SB_CARRY)
        // - dff_enable: Use DFF output instead of LUT output
        // - set_no_reset: Async set/reset mode (SB_DFFSR*, SB_DFFSS* have async set)

        let neg_clk = cell_type.contains("DFFN")
            || cell_type.contains("_N")
            || cell_type.ends_with("N")
            || cell_type.contains("SB_DFFN");

        let set_no_reset = cell_type.contains("DFFSR")
            || cell_type.contains("DFFSS")
            || cell_type.contains("_SR")
            || cell_type.contains("_SS");

        DffConfig {
            neg_clk,
            carry_enable: false, // Set separately for carry cells
            dff_enable: true,    // Always true for DFF cells
            set_no_reset,
        }
    }

    /// Collect I/O configurations from placement and netlist
    fn collect_io_configs(
        &self,
        placement: &PlacementResult,
        netlist: Option<&GateNetlist>,
    ) -> HashMap<(u32, u32, usize), IoConfig> {
        let mut io_configs = HashMap::new();

        for (cell_id, loc) in &placement.placements {
            if matches!(loc.bel_type, BelType::IoCell) {
                // Get I/O direction from cell function (primary) or cell type (fallback)
                let config = if let Some(netlist) = netlist {
                    netlist
                        .cells
                        .iter()
                        .find(|c| c.id.0 == cell_id.0)
                        .map(|cell| {
                            let mut cfg = match &cell.function {
                                Some(CellFunction::InputPad) | Some(CellFunction::ClockPad) => {
                                    IoConfig::simple_input()
                                }
                                Some(CellFunction::OutputPad) => IoConfig::simple_output(),
                                Some(CellFunction::BidirPad) => IoConfig::bidirectional(),
                                _ => Self::derive_io_config(&cell.cell_type),
                            };
                            // Apply pull-up from cell parameters
                            if let Some(pullup) = cell.parameters.get("PULLUP") {
                                if pullup.eq_ignore_ascii_case("yes") || pullup == "1" {
                                    cfg.pullup_enable = true;
                                }
                            }
                            cfg
                        })
                        .unwrap_or_default()
                } else {
                    IoConfig::default()
                };

                // I/O tiles have 2 IOBs (IOB_0 at bel_idx 0, IOB_1 at bel_idx 1)
                let iob_idx = loc.bel_index % 2;
                io_configs.insert((loc.tile_x, loc.tile_y, iob_idx), config);
            }
        }

        io_configs
    }

    /// Derive I/O configuration from cell type
    fn derive_io_config(cell_type: &str) -> IoConfig {
        // SB_IO has a parameter that specifies the pin type
        // For now, derive from cell type naming convention
        if cell_type.contains("_INPUT") || cell_type.ends_with("_I") {
            IoConfig::simple_input()
        } else if cell_type.contains("_OUTPUT") || cell_type.ends_with("_O") {
            IoConfig::simple_output()
        } else if cell_type.contains("_INOUT") || cell_type.contains("_IO") {
            IoConfig::bidirectional()
        } else if cell_type == "SB_IO" {
            // Default SB_IO without direction hint - treat as bidirectional
            IoConfig::bidirectional()
        } else {
            // Default: simple input
            IoConfig::simple_input()
        }
    }

    /// Collect global network configuration from placement and netlist.
    /// Determines which global networks (0-7) are in use.
    /// Enables all 8 column buffer networks for any design that uses clocks or
    /// global buffers, matching nextpnr's behavior (unused column buffers are harmless).
    fn collect_global_config(
        &self,
        _placement: &PlacementResult,
        _netlist: Option<&GateNetlist>,
    ) -> GlobalNetworkConfig {
        // Always enable all 8 global networks. nextpnr unconditionally sets ColBufCtrl
        // bits in all column buffer rows, even for purely combinational designs.
        // Unused column buffers pass the default low signal and are harmless.
        // This ensures clock/reset/global signals can reach all tiles when needed.
        GlobalNetworkConfig {
            active_networks: 0xFF,
        }
    }

    /// Generate logic tile configuration
    /// Logic tiles have 54 columns x 16 rows of config bits
    #[allow(clippy::too_many_arguments)]
    fn generate_logic_tile(
        &self,
        asc: &mut String,
        x: u32,
        y: u32,
        placement: &PlacementResult,
        routing: &RoutingResult,
        lut_inits: &HashMap<(u32, u32, usize), u16>,
        dff_configs: &HashMap<(u32, u32, usize), DffConfig>,
        global_config: &GlobalNetworkConfig,
        carry_in_set_tiles: &HashSet<(u32, u32)>,
    ) {
        // Check if any cells are placed in this tile
        let cells_in_tile: Vec<_> = placement
            .placements
            .iter()
            .filter(|(_, loc)| loc.tile_x == x && loc.tile_y == y)
            .collect();

        // Collect PIPs in this tile from routing
        let pips_in_tile: Vec<_> = routing
            .routes
            .values()
            .flat_map(|route| route.pips.iter())
            .filter(|&&pip_id| {
                self.device
                    .pip(pip_id)
                    .map(|p| p.tile_x == x && p.tile_y == y)
                    .unwrap_or(false)
            })
            .copied()
            .collect();

        // ColBufCtrl bits are only set in tiles at the column buffer rows.
        // For HX1K/LP1K: y = {4, 5, 12, 13}
        // For HX8K/LP8K: y = {8, 9, 25, 26}
        // These rows contain the column buffers that distribute global signals.
        let (_, height) = self.device.grid_size();
        let colbuf_rows = colbuf_source_rows(height);
        let is_colbuf_row = colbuf_rows.contains(&y);
        let needs_colbuf = global_config.active_networks != 0 && is_colbuf_row;

        if cells_in_tile.is_empty() && pips_in_tile.is_empty() && !needs_colbuf {
            return;
        }

        asc.push_str(&format!(".logic_tile {} {}\n", x, y));

        // Create a 16x54 bit array (16 rows, 54 columns)
        let mut bits = [[false; 54]; 16];

        if let Some(ref chipdb) = self.chipdb {
            // Set LUT init bits based on chipdb mappings
            for lc_mapping in &chipdb.lc_mappings {
                let lc_idx = lc_mapping.lc_idx as usize;
                let init = lut_inits.get(&(x, y, lc_idx)).copied().unwrap_or(0);

                // The first 16 bits of lc_mapping.bit_positions are for the LUT init
                // (the remaining 4 are for other LC configuration)
                for (bit_num, &(row, col)) in lc_mapping.bit_positions.iter().take(16).enumerate() {
                    if row < 16 && col < 54 {
                        let bit_value = (init >> bit_num) & 1 == 1;
                        bits[row as usize][col as usize] = bit_value;
                    }
                }

                // Set DFF configuration bits (bits 16-19)
                // Bit 16: NegClk, Bit 17: CarryEnable, Bit 18: DffEnable, Bit 19: Set_NoReset
                if let Some(dff_config) = dff_configs.get(&(x, y, lc_idx)) {
                    let dff_bits = [
                        dff_config.neg_clk,      // Bit 16
                        dff_config.carry_enable, // Bit 17
                        dff_config.dff_enable,   // Bit 18
                        dff_config.set_no_reset, // Bit 19
                    ];

                    // Apply DFF config bits using positions 16-19 from the mapping
                    for (bit_offset, &bit_value) in dff_bits.iter().enumerate() {
                        let bit_idx = 16 + bit_offset;
                        if bit_idx < lc_mapping.bit_positions.len() {
                            let (row, col) = lc_mapping.bit_positions[bit_idx];
                            if row < 16 && col < 54 {
                                bits[row as usize][col as usize] = bit_value;
                            }
                        }
                    }
                }
            }

            // Set routing configuration bits for PIPs
            for pip_id in &pips_in_tile {
                // PipId index corresponds directly to chipdb.pips index
                if let Some(pip_info) = chipdb.pips.get(pip_id.0 as usize) {
                    // Only set bits for PIPs in this tile
                    if pip_info.tile_x == x && pip_info.tile_y == y {
                        for config_bit in &pip_info.config_bits {
                            let row = config_bit.row as usize;
                            let col = config_bit.col as usize;
                            if row < 16 && col < 54 {
                                // If inverted, we want bit=0 to enable, otherwise bit=1
                                bits[row][col] = !config_bit.inverted;
                            }
                        }
                    }
                }
            }

            // Set column buffer control bits for global networks
            // Logic tile ColBufCtrl bit positions (from chipdb):
            //   glb_netwk_0: B0[1], glb_netwk_1: B1[2], glb_netwk_2: B5[2]
            //   glb_netwk_3: B7[2], glb_netwk_4: B9[2], glb_netwk_5: B11[2]
            //   glb_netwk_6: B13[2], glb_netwk_7: B15[2]
            let colbuf_positions: [(usize, usize, u8); 8] = [
                (0, 1, 0),  // glb_netwk_0 at B0[1]
                (1, 2, 1),  // glb_netwk_1 at B1[2]
                (5, 2, 2),  // glb_netwk_2 at B5[2]
                (7, 2, 3),  // glb_netwk_3 at B7[2]
                (9, 2, 4),  // glb_netwk_4 at B9[2]
                (11, 2, 5), // glb_netwk_5 at B11[2]
                (13, 2, 6), // glb_netwk_6 at B13[2]
                (15, 2, 7), // glb_netwk_7 at B15[2]
            ];

            for (row, col, glb_idx) in colbuf_positions {
                if (global_config.active_networks >> glb_idx) & 1 == 1 {
                    bits[row][col] = true;
                }
            }
        }

        // Set CarryInSet bit (forces carry chain input for this tile to 1).
        // Position is read from chipdb tile_bits for TileType::Logic.
        if carry_in_set_tiles.contains(&(x, y)) {
            if let Some(ref chipdb) = self.chipdb {
                if let Some(logic_bits) = chipdb.tile_bits.get(&TileType::Logic) {
                    for cb in logic_bits {
                        if cb.name == "CarryInSet" {
                            let r = cb.row as usize;
                            let c = cb.col as usize;
                            if r < 16 && c < 54 {
                                bits[r][c] = true;
                            }
                            break;
                        }
                    }
                }
            }
        }

        // Output the 16 rows of 54 bits each
        for row in &bits {
            for &bit in row {
                asc.push(if bit { '1' } else { '0' });
            }
            asc.push('\n');
        }

        asc.push('\n');
    }

    /// Generate I/O tile configuration
    /// I/O tiles have 18 columns x 16 rows of config bits
    #[allow(clippy::too_many_arguments)]
    fn generate_io_tile(
        &self,
        asc: &mut String,
        x: u32,
        y: u32,
        placement: &PlacementResult,
        routing: &RoutingResult,
        io_configs: &HashMap<(u32, u32, usize), IoConfig>,
        pll_resolved_bits: &HashMap<(u32, u32), Vec<(u8, u8)>>,
        global_config: &GlobalNetworkConfig,
    ) {
        let cells_in_tile: Vec<_> = placement
            .placements
            .iter()
            .filter(|(_, loc)| {
                loc.tile_x == x && loc.tile_y == y && matches!(loc.bel_type, BelType::IoCell)
            })
            .collect();

        // Collect PIPs in this tile from routing
        let pips_in_tile: Vec<_> = routing
            .routes
            .values()
            .flat_map(|route| route.pips.iter())
            .filter(|&&pip_id| {
                self.device
                    .pip(pip_id)
                    .map(|p| p.tile_x == x && p.tile_y == y)
                    .unwrap_or(false)
            })
            .copied()
            .collect();

        // Check if this IO tile has PLL config bits
        let has_pll_bits = pll_resolved_bits.contains_key(&(x, y));

        // IO tiles at column buffer rows also need ColBufCtrl bits
        let (_, height) = self.device.grid_size();
        let colbuf_rows = colbuf_source_rows(height);
        let is_colbuf_row = colbuf_rows.contains(&y);
        let needs_colbuf = global_config.active_networks != 0 && is_colbuf_row;

        let has_cells_or_pips = !cells_in_tile.is_empty() || !pips_in_tile.is_empty() || has_pll_bits;

        asc.push_str(&format!(".io_tile {} {}\n", x, y));

        // Create a 16x18 bit array (16 rows, 18 columns)
        let mut bits = [[false; 18]; 16];

        // Set default Input Enable (IE) bits for unused IO pads.
        // This prevents floating inputs and matches nextpnr/icestorm behavior.
        // IE_0 at B9[3], IE_1 at B6[3]
        // Skip IE on IO tile positions that lack physical pads (derived from chipdb).
        let has_pads = self.padded_io_tiles.is_empty() || self.padded_io_tiles.contains(&(x, y));
        if !has_cells_or_pips && has_pads {
            bits[9][3] = true; // IE_0 — input enable for IOB_0
            bits[6][3] = true; // IE_1 — input enable for IOB_1
        }

        if let Some(ref chipdb) = self.chipdb {
            // Set I/O cell configuration bits
            // IOB_0 PINTYPE bit positions (from chipdb):
            //   PINTYPE_0: B3[17], PINTYPE_1: B3[16], PINTYPE_2: B0[17]
            //   PINTYPE_3: B0[16], PINTYPE_4: B4[16], PINTYPE_5: B4[17]
            // IOB_1 PINTYPE bit positions:
            //   PINTYPE_0: B13[17], PINTYPE_1: B13[16], PINTYPE_2: B10[17]
            //   PINTYPE_3: B10[16], PINTYPE_4: B14[16], PINTYPE_5: B14[17]
            // IoCtrl bits:
            //   IE_0: B9[3], IE_1: B6[3]
            //   REN_0: B6[2], REN_1: B1[3]

            // IOB_0 configuration
            if let Some(config) = io_configs.get(&(x, y, 0)) {
                let pintype = config.pintype();
                // Set PINTYPE bits for IOB_0
                bits[3][17] = pintype & 1 == 1; // PINTYPE_0
                bits[3][16] = (pintype >> 1) & 1 == 1; // PINTYPE_1
                bits[0][17] = (pintype >> 2) & 1 == 1; // PINTYPE_2
                bits[0][16] = (pintype >> 3) & 1 == 1; // PINTYPE_3
                bits[4][16] = (pintype >> 4) & 1 == 1; // PINTYPE_4
                bits[4][17] = (pintype >> 5) & 1 == 1; // PINTYPE_5

                // Set input enable (IE_0)
                bits[9][3] = config.input_enable;

                // REN_0: disable pull-up on active IO. REN=1 means pull-up disabled.
                // When pullup_enable is explicitly set, leave REN=0 (pull-up on).
                // Otherwise, disable pull-up on active pins to prevent contention.
                bits[6][2] = !config.pullup_enable;
            }

            // IOB_1 configuration
            if let Some(config) = io_configs.get(&(x, y, 1)) {
                let pintype = config.pintype();
                // Set PINTYPE bits for IOB_1
                bits[13][17] = pintype & 1 == 1; // PINTYPE_0
                bits[13][16] = (pintype >> 1) & 1 == 1; // PINTYPE_1
                bits[10][17] = (pintype >> 2) & 1 == 1; // PINTYPE_2
                bits[10][16] = (pintype >> 3) & 1 == 1; // PINTYPE_3
                bits[14][16] = (pintype >> 4) & 1 == 1; // PINTYPE_4
                bits[14][17] = (pintype >> 5) & 1 == 1; // PINTYPE_5

                // Set input enable (IE_1)
                bits[6][3] = config.input_enable;

                // REN_1: disable pull-up on active IO (same logic as REN_0)
                bits[1][3] = !config.pullup_enable;
            }

            // Set routing configuration bits for PIPs
            for pip_id in &pips_in_tile {
                if let Some(pip_info) = chipdb.pips.get(pip_id.0 as usize) {
                    if pip_info.tile_x == x && pip_info.tile_y == y {
                        for config_bit in &pip_info.config_bits {
                            let row = config_bit.row as usize;
                            let col = config_bit.col as usize;
                            if row < 16 && col < 18 {
                                bits[row][col] = !config_bit.inverted;
                            }
                        }
                    }
                }
            }
        }

        // Set PLL config bits that target this IO tile (HX/LP devices)
        if let Some(pll_bits) = pll_resolved_bits.get(&(x, y)) {
            for &(row, col) in pll_bits {
                if (row as usize) < 16 && (col as usize) < 18 {
                    bits[row as usize][col as usize] = true;
                }
            }
        }

        // Set column buffer control bits for global networks in IO tiles.
        // IO tile ColBufCtrl bit positions (from chipdb) differ from logic tiles:
        //   glb_netwk_0: B1[9],  glb_netwk_1: B0[9],  glb_netwk_2: B3[9]
        //   glb_netwk_3: B2[9],  glb_netwk_4: B5[9],  glb_netwk_5: B4[9]
        //   glb_netwk_6: B7[9],  glb_netwk_7: B6[9]
        if needs_colbuf {
            let io_colbuf_positions: [(usize, usize, u8); 8] = [
                (1, 9, 0), // glb_netwk_0 at B1[9]
                (0, 9, 1), // glb_netwk_1 at B0[9]
                (3, 9, 2), // glb_netwk_2 at B3[9]
                (2, 9, 3), // glb_netwk_3 at B2[9]
                (5, 9, 4), // glb_netwk_4 at B5[9]
                (4, 9, 5), // glb_netwk_5 at B4[9]
                (7, 9, 6), // glb_netwk_6 at B7[9]
                (6, 9, 7), // glb_netwk_7 at B6[9]
            ];

            for (row, col, glb_idx) in io_colbuf_positions {
                if (global_config.active_networks >> glb_idx) & 1 == 1 {
                    bits[row][col] = true;
                }
            }
        }

        // Output the 16 rows of 18 bits each
        for row in &bits {
            for &bit in row {
                asc.push(if bit { '1' } else { '0' });
            }
            asc.push('\n');
        }

        asc.push('\n');
    }

    /// Collect RAM configurations from placement and netlist
    fn collect_ram_configs(
        &self,
        placement: &PlacementResult,
        netlist: Option<&GateNetlist>,
    ) -> HashMap<(u32, u32), RamConfig> {
        let mut ram_configs = HashMap::new();

        let netlist = match netlist {
            Some(n) => n,
            None => return ram_configs,
        };

        for (cell_id, loc) in &placement.placements {
            if !matches!(loc.bel_type, BelType::RamSlice) {
                continue;
            }

            if let Some(cell) = netlist.cells.iter().find(|c| c.id.0 == cell_id.0) {
                let read_mode = cell
                    .parameters
                    .get("READ_MODE")
                    .and_then(|v| v.parse::<u8>().ok())
                    .unwrap_or(0);
                let write_mode = cell
                    .parameters
                    .get("WRITE_MODE")
                    .and_then(|v| v.parse::<u8>().ok())
                    .unwrap_or(0);

                let init_data = Self::parse_ram_init_data(&cell.parameters);

                ram_configs.insert(
                    (loc.tile_x, loc.tile_y),
                    RamConfig {
                        read_mode,
                        write_mode,
                        init_data,
                    },
                );
            }
        }

        ram_configs
    }

    /// Parse RAM initialization data from INIT_0..INIT_F parameters
    /// Each INIT_x is a 256-bit hex string (64 hex chars), 16 total = 4096 bits
    fn parse_ram_init_data(parameters: &indexmap::IndexMap<String, String>) -> Option<Vec<u8>> {
        let mut has_any = false;
        let mut data = vec![0u8; 512]; // 4096 bits = 512 bytes

        for i in 0..16u8 {
            let key = format!("INIT_{:X}", i);
            if let Some(hex_str) = parameters.get(&key) {
                has_any = true;
                let hex = hex_str.trim_start_matches("0x").trim_start_matches("0X");
                let offset = i as usize * 32; // 256 bits = 32 bytes per INIT_x
                for (j, chunk) in hex.as_bytes().chunks(2).enumerate() {
                    if offset + j < data.len() {
                        if let Ok(byte) =
                            u8::from_str_radix(std::str::from_utf8(chunk).unwrap_or("00"), 16)
                        {
                            data[offset + j] = byte;
                        }
                    }
                }
            }
        }

        if has_any {
            Some(data)
        } else {
            None
        }
    }

    /// Collect PLL configurations from placement and netlist
    fn collect_pll_configs(
        &self,
        placement: &PlacementResult,
        netlist: Option<&GateNetlist>,
    ) -> HashMap<(u32, u32), PllBitstreamConfig> {
        let mut pll_configs = HashMap::new();

        let netlist = match netlist {
            Some(n) => n,
            None => return pll_configs,
        };

        for (cell_id, loc) in &placement.placements {
            if !matches!(loc.bel_type, BelType::Pll) {
                continue;
            }

            if let Some(cell) = netlist.cells.iter().find(|c| c.id.0 == cell_id.0) {
                let divr = cell
                    .parameters
                    .get("DIVR")
                    .and_then(|v| v.parse::<u8>().ok())
                    .unwrap_or(0);
                let divf = cell
                    .parameters
                    .get("DIVF")
                    .and_then(|v| v.parse::<u8>().ok())
                    .unwrap_or(0);
                let divq = cell
                    .parameters
                    .get("DIVQ")
                    .and_then(|v| v.parse::<u8>().ok())
                    .unwrap_or(0);
                let filter_range = cell
                    .parameters
                    .get("FILTER_RANGE")
                    .and_then(|v| v.parse::<u8>().ok())
                    .unwrap_or(0);
                let feedback_path = match cell.parameters.get("FEEDBACK_PATH").map(|s| s.as_str()) {
                    Some("SIMPLE") | None => 0,
                    Some("DELAY") => 1,
                    Some("PHASE_AND_DELAY") => 2,
                    Some("EXTERNAL") => 4,
                    _ => 0,
                };
                let plltype = match cell.cell_type.as_str() {
                    "SB_PLL40_PAD" => 1,
                    "SB_PLL40_2_PAD" => 4,
                    "SB_PLL40_2F_PAD" => 6,
                    "SB_PLL40_2F_CORE" => 7,
                    _ => 0, // SB_PLL40_CORE
                };

                pll_configs.insert(
                    (loc.tile_x, loc.tile_y),
                    PllBitstreamConfig {
                        divr,
                        divf,
                        divq,
                        filter_range,
                        feedback_path,
                        plltype,
                    },
                );
            }
        }

        pll_configs
    }

    /// Resolve all PLL config parameters to their actual bit positions in target tiles.
    /// Returns a map of (tile_x, tile_y) -> [(row, col)] for all bits that need to be set.
    /// For HX/LP devices, the target tiles are IO tiles. For UP5K, they are ipcon tiles.
    fn resolve_pll_bits(
        &self,
        pll_configs: &HashMap<(u32, u32), PllBitstreamConfig>,
    ) -> HashMap<(u32, u32), Vec<(u8, u8)>> {
        let mut tile_bits: HashMap<(u32, u32), Vec<(u8, u8)>> = HashMap::new();

        let chipdb = match &self.chipdb {
            Some(db) => db,
            None => return tile_bits,
        };

        let pll_extra = match chipdb.pll_extra_cell() {
            Some(p) => p,
            None => return tile_bits,
        };

        for config in pll_configs.values() {
            // Set DIVR bits (4-bit)
            for i in 0..4u8 {
                if (config.divr >> i) & 1 == 1 {
                    let param = format!("DIVR_{}", i);
                    if let Some((tx, ty, row, col)) =
                        chipdb.resolve_pll_param_bit(pll_extra, &param)
                    {
                        tile_bits.entry((tx, ty)).or_default().push((row, col));
                    }
                }
            }

            // Set DIVF bits (7-bit)
            for i in 0..7u8 {
                if (config.divf >> i) & 1 == 1 {
                    let param = format!("DIVF_{}", i);
                    if let Some((tx, ty, row, col)) =
                        chipdb.resolve_pll_param_bit(pll_extra, &param)
                    {
                        tile_bits.entry((tx, ty)).or_default().push((row, col));
                    }
                }
            }

            // Set DIVQ bits (3-bit)
            for i in 0..3u8 {
                if (config.divq >> i) & 1 == 1 {
                    let param = format!("DIVQ_{}", i);
                    if let Some((tx, ty, row, col)) =
                        chipdb.resolve_pll_param_bit(pll_extra, &param)
                    {
                        tile_bits.entry((tx, ty)).or_default().push((row, col));
                    }
                }
            }

            // Set FILTER_RANGE bits (3-bit)
            for i in 0..3u8 {
                if (config.filter_range >> i) & 1 == 1 {
                    let param = format!("FILTER_RANGE_{}", i);
                    if let Some((tx, ty, row, col)) =
                        chipdb.resolve_pll_param_bit(pll_extra, &param)
                    {
                        tile_bits.entry((tx, ty)).or_default().push((row, col));
                    }
                }
            }

            // Set FEEDBACK_PATH bits (3-bit)
            for i in 0..3u8 {
                if (config.feedback_path >> i) & 1 == 1 {
                    let param = format!("FEEDBACK_PATH_{}", i);
                    if let Some((tx, ty, row, col)) =
                        chipdb.resolve_pll_param_bit(pll_extra, &param)
                    {
                        tile_bits.entry((tx, ty)).or_default().push((row, col));
                    }
                }
            }

            // Set PLLTYPE bits (3-bit)
            for i in 0..3u8 {
                if (config.plltype >> i) & 1 == 1 {
                    let param = format!("PLLTYPE_{}", i);
                    if let Some((tx, ty, row, col)) =
                        chipdb.resolve_pll_param_bit(pll_extra, &param)
                    {
                        tile_bits.entry((tx, ty)).or_default().push((row, col));
                    }
                }
            }
        }

        tile_bits
    }

    /// Generate PLL/IpCon tile configuration for dedicated ipcon tiles (UP5K).
    /// Uses pre-resolved PLL bits from resolve_pll_bits().
    fn generate_pll_tile(
        &self,
        asc: &mut String,
        x: u32,
        y: u32,
        pll_resolved_bits: &HashMap<(u32, u32), Vec<(u8, u8)>>,
    ) {
        // Check if any resolved PLL bits target this tile
        let bits_to_set = match pll_resolved_bits.get(&(x, y)) {
            Some(bits) if !bits.is_empty() => bits,
            _ => return,
        };

        let chipdb = match &self.chipdb {
            Some(db) => db,
            None => return,
        };

        let dims = chipdb
            .tile_dimensions
            .get(&TileType::Pll)
            .copied()
            .unwrap_or(crate::device::ice40::chipdb_parser::TileBitDimensions {
                columns: 18,
                rows: 16,
            });

        asc.push_str(&format!(".ipcon_tile {} {}\n", x, y));

        let mut bits = vec![vec![false; dims.columns as usize]; dims.rows as usize];

        for &(row, col) in bits_to_set {
            if (row as usize) < bits.len() && (col as usize) < bits[0].len() {
                bits[row as usize][col as usize] = true;
            }
        }

        for row in &bits {
            for &bit in row {
                asc.push(if bit { '1' } else { '0' });
            }
            asc.push('\n');
        }
        asc.push('\n');
    }

    /// Generate RAM bottom tile configuration
    /// RAM tiles have 42 columns x 16 rows of config bits
    #[allow(clippy::too_many_arguments)]
    fn generate_ramb_tile(
        &self,
        asc: &mut String,
        x: u32,
        y: u32,
        _placement: &PlacementResult,
        ram_configs: &HashMap<(u32, u32), RamConfig>,
        global_config: &GlobalNetworkConfig,
    ) {
        // RAM bottom tiles always need to be emitted for:
        // 1. RamConfig.PowerUp B1[7] — set on ALL ramb tiles
        // 2. ColBufCtrl — set on ramb tiles at column buffer rows
        // 3. RAM config bits — for tiles with placed RAM cells

        let (_, height) = self.device.grid_size();
        let ram_colbuf_rows = ramb_colbuf_source_rows(height);
        let is_colbuf_row = ram_colbuf_rows.contains(&y);
        let needs_colbuf = global_config.active_networks != 0 && is_colbuf_row;

        asc.push_str(&format!(".ramb_tile {} {}\n", x, y));

        // Create a 16x42 bit array (16 rows, 42 columns)
        let mut bits = [[false; 42]; 16];

        // RamConfig.PowerUp B1[7] — always set on all ramb tiles
        bits[1][7] = true;

        // Set READ_MODE/WRITE_MODE config bits from chipdb CBIT positions
        if let Some(config) = ram_configs.get(&(x, y)) {
            if let Some(ref chipdb) = self.chipdb {
                if let Some(tile_bits) = chipdb.tile_bits.get(&TileType::RamBottom) {
                    for cb in tile_bits {
                        let value = match cb.name.as_str() {
                            "RamConfig.CBIT_0" => config.read_mode & 1 != 0,
                            "RamConfig.CBIT_1" => (config.read_mode >> 1) & 1 != 0,
                            "RamConfig.CBIT_2" => config.write_mode & 1 != 0,
                            "RamConfig.CBIT_3" => (config.write_mode >> 1) & 1 != 0,
                            _ => false,
                        };
                        if value {
                            let row = cb.row as usize;
                            let col = cb.col as usize;
                            if row < 16 && col < 42 {
                                bits[row][col] = true;
                            }
                        }
                    }
                }
            }
        }

        // Set ColBufCtrl bits for global networks at column buffer rows.
        // RAM tile ColBufCtrl bit positions are the same as logic tiles:
        //   glb_netwk_0: B0[1], glb_netwk_1: B1[2], glb_netwk_2: B5[2]
        //   glb_netwk_3: B7[2], glb_netwk_4: B9[2], glb_netwk_5: B11[2]
        //   glb_netwk_6: B13[2], glb_netwk_7: B15[2]
        if needs_colbuf {
            let colbuf_positions: [(usize, usize, u8); 8] = [
                (0, 1, 0),  // glb_netwk_0 at B0[1]
                (1, 2, 1),  // glb_netwk_1 at B1[2]
                (5, 2, 2),  // glb_netwk_2 at B5[2]
                (7, 2, 3),  // glb_netwk_3 at B7[2]
                (9, 2, 4),  // glb_netwk_4 at B9[2]
                (11, 2, 5), // glb_netwk_5 at B11[2]
                (13, 2, 6), // glb_netwk_6 at B13[2]
                (15, 2, 7), // glb_netwk_7 at B15[2]
            ];

            for (row, col, glb_idx) in colbuf_positions {
                if (global_config.active_networks >> glb_idx) & 1 == 1 {
                    bits[row][col] = true;
                }
            }
        }

        // Output the 16 rows of 42 bits each
        for row in &bits {
            for &bit in row {
                asc.push(if bit { '1' } else { '0' });
            }
            asc.push('\n');
        }

        asc.push('\n');

        // Emit .ram_data section for initialization data
        if let Some(config) = ram_configs.get(&(x, y)) {
            if let Some(ref init_data) = config.init_data {
                asc.push_str(&format!(".ram_data {} {}\n", x, y));
                // 16 lines (INIT_0..INIT_F), each 64 hex chars (256 bits)
                for line in 0..16 {
                    let offset = line * 32;
                    for j in 0..32 {
                        let byte = if offset + j < init_data.len() {
                            init_data[offset + j]
                        } else {
                            0
                        };
                        asc.push_str(&format!("{:02x}", byte));
                    }
                    asc.push('\n');
                }
                asc.push('\n');
            }
        }
    }

    /// Generate RAM top tile configuration
    fn generate_ramt_tile(
        &self,
        asc: &mut String,
        x: u32,
        y: u32,
        placement: &PlacementResult,
        ram_configs: &HashMap<(u32, u32), RamConfig>,
    ) {
        let cells_in_tile: Vec<_> = placement
            .placements
            .iter()
            .filter(|(_, loc)| {
                loc.tile_x == x && loc.tile_y == y && matches!(loc.bel_type, BelType::RamSlice)
            })
            .collect();

        if cells_in_tile.is_empty() {
            return;
        }

        asc.push_str(&format!(".ramt_tile {} {}\n", x, y));

        // Create a 16x42 bit array (16 rows, 42 columns)
        let mut bits = [[false; 42]; 16];

        // Set READ_MODE/WRITE_MODE config bits from chipdb CBIT positions
        if let Some(config) = ram_configs.get(&(x, y)) {
            if let Some(ref chipdb) = self.chipdb {
                if let Some(tile_bits) = chipdb.tile_bits.get(&TileType::RamTop) {
                    for cb in tile_bits {
                        let value = match cb.name.as_str() {
                            "RamConfig.CBIT_0" => config.read_mode & 1 != 0,
                            "RamConfig.CBIT_1" => (config.read_mode >> 1) & 1 != 0,
                            "RamConfig.CBIT_2" => config.write_mode & 1 != 0,
                            "RamConfig.CBIT_3" => (config.write_mode >> 1) & 1 != 0,
                            _ => false,
                        };
                        if value {
                            let row = cb.row as usize;
                            let col = cb.col as usize;
                            if row < 16 && col < 42 {
                                bits[row][col] = true;
                            }
                        }
                    }
                }
            }
        }

        // Output the 16 rows of 42 bits each
        for row in &bits {
            for &bit in row {
                asc.push(if bit { '1' } else { '0' });
            }
            asc.push('\n');
        }

        asc.push('\n');

        // Emit .ram_data section for initialization data
        if let Some(config) = ram_configs.get(&(x, y)) {
            if let Some(ref init_data) = config.init_data {
                asc.push_str(&format!(".ram_data {} {}\n", x, y));
                // 16 lines (INIT_0..INIT_F), each 64 hex chars (256 bits)
                for line in 0..16 {
                    let offset = line * 32;
                    for j in 0..32 {
                        let byte = if offset + j < init_data.len() {
                            init_data[offset + j]
                        } else {
                            0
                        };
                        asc.push_str(&format!("{:02x}", byte));
                    }
                    asc.push('\n');
                }
                asc.push('\n');
            }
        }
    }

    /// Generate DSP tile configuration (placeholder)
    /// DSP tiles use the same 42x16 format as RAM tiles in iCE40 UP5K
    fn generate_dsp_tile(&self, asc: &mut String, x: u32, y: u32, placement: &PlacementResult) {
        let cells_in_tile: Vec<_> = placement
            .placements
            .iter()
            .filter(|(_, loc)| {
                loc.tile_x == x && loc.tile_y == y && matches!(loc.bel_type, BelType::DspSlice)
            })
            .collect();

        if cells_in_tile.is_empty() {
            return;
        }

        // DSP tiles are emitted as .dsp0_tile through .dsp3_tile in icestorm
        // For now, use a generic format
        asc.push_str(&format!(".dsp0_tile {} {}\n", x, y));

        // Create a 16x42 bit array (all zeros — placeholder)
        let bits = [[false; 42]; 16];

        for row in &bits {
            for &bit in row {
                asc.push(if bit { '1' } else { '0' });
            }
            asc.push('\n');
        }

        asc.push('\n');
    }

}
