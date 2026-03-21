//! Nexus device data from Project Oxide (prjoxide)
//!
//! All device-specific constants in one place. When prjoxide database is updated,
//! only this file needs to change.
//!
//! Sources:
//! - prjoxide devices.json: grid sizes, IDCODEs, bitstream geometry
//! - prjoxide tile database: PLC/EBR/LRAM/DSP tile counts
//! - prjoxide timing (LIFCL speed grade 10, fast corner): cell and interconnect delays
//! - prjoxide globals data: PLL locations, clock spine structure
//!
//! Copyright (C) 2020-21 gatecat <gatecat@ds0.me>
//! Licensed under ISC License — see COPYING in prjoxide repository.

/// All variant-specific device parameters for one Nexus device.
#[derive(Debug, Clone)]
pub struct NexusDieData {
    // -- Fabric geometry (prjoxide devices.json) --
    /// Tile grid dimensions (columns, rows) — (max_col+1, max_row+1)
    pub grid: (u32, u32),
    /// Device name string
    pub name: &'static str,

    // -- Resource counts (prjoxide tile database) --
    /// PLC tile count (each = 4 slices × 2 = 8 LUT4 + 8 FF)
    pub plc_tiles: usize,
    /// EBR blocks (18 Kb each) — EBR tile groups / 8
    pub ebr_blocks: usize,
    /// LRAM blocks (32 Kb each)
    pub lram_blocks: usize,
    /// DSP blocks (DSP_L + DSP_R tile groups / 11)
    pub dsp_blocks: usize,
    /// I/O tile count (SYSIO banks)
    pub io_tiles: usize,
    /// PLLs (GPLL tiles)
    pub plls: u8,
    /// Global clock count (HPBX horizontal branches)
    pub global_clocks: u8,
    /// PCS (SerDes) channel count
    pub pcs_channels: u8,

    // -- Bitstream (prjoxide devices.json) --
    /// JTAG IDCODE
    pub idcode: u32,
    /// Bitstream frame count
    pub bitstream_frames: u32,
    /// Bits per bitstream frame
    pub bits_per_frame: u32,

    // -- Packages (prjoxide devices.json) --
    pub packages: &'static [&'static str],

    // -- Hard IP --
    /// Has hard PCIe (PCIE_LL tile)
    pub has_pcie: bool,
    /// Has hard LPDDR4 (not yet fuzzed in prjoxide)
    pub has_lpddr4: bool,
}

/// LIFCL-40 (CrossLink-NX)
pub const LIFCL_40: NexusDieData = NexusDieData {
    grid: (88, 57),   // prjoxide: max_col=87, max_row=56
    name: "LIFCL-40",
    plc_tiles: 4_032, // = 32,256 LUT4
    ebr_blocks: 21,   // 168 EBR tiles / 8
    lram_blocks: 2,
    dsp_blocks: 14,   // DSP_L(88/11=8) + DSP_R(66/11=6)
    io_tiles: 196,
    plls: 3,          // GPLL_ULC, GPLL_LLC, GPLL_LRC
    global_clocks: 16,
    pcs_channels: 0,
    idcode: 0x0111_0043,
    bitstream_frames: 9172,
    bits_per_frame: 662,
    packages: &["QFN72", "csfBGA289", "caBGA400"],
    has_pcie: false,
    has_lpddr4: false,
};

/// LFCPNX-100 (CertusPro-NX) — KarythraGPU target
pub const LFCPNX_100: NexusDieData = NexusDieData {
    grid: (160, 75),   // prjoxide: max_col=159, max_row=74
    name: "LFCPNX-100",
    plc_tiles: 9_984,  // = 79,872 LUT4
    ebr_blocks: 52,    // 416 EBR tiles / 8
    lram_blocks: 7,
    dsp_blocks: 39,    // DSP_L(198/11=18) + DSP_R(231/11=21)
    io_tiles: 380,
    plls: 4,           // GPLL_ULC, GPLL_URC, GPLL_LLC, GPLL_LRC
    global_clocks: 26, // 13 branches × 2 spines
    pcs_channels: 8,
    idcode: 0x010F_1043,
    bitstream_frames: 16822,
    bits_per_frame: 878,
    packages: &["ASG256", "CBG256", "BBG484", "BFG484", "LFG672"],
    has_pcie: true,
    has_lpddr4: true, // present but not yet fuzzed in prjoxide
};

// ---------------------------------------------------------------------------
// Timing data — prjoxide LIFCL speed grade 10, fast corner (ns)
// ---------------------------------------------------------------------------

pub struct NexusTimingData {
    // -- Cell delays (OXIDE_COMB / OXIDE_FF) --
    pub lut4_delay: f64,
    pub dff_clk_to_q: f64,
    pub dff_setup: f64,
    pub dff_hold: f64,
    pub carry_delay: f64,
    pub io_input_delay: f64,
    pub io_output_delay: f64,
    pub ram_read_delay: f64,

    // -- Interconnect delays --
    pub cib_mux_delay: f64,
    pub span0_delay: f64,
    pub span2_delay: f64,
    pub span6_delay: f64,
    pub global_clock_delay: f64,

    // -- PIP delays --
    pub pip_lut_to_local: f64,
    pub pip_local_to_local: f64,
    pub pip_local_to_span: f64,
    pub pip_span2_to_span2: f64,
    pub pip_span2_to_local: f64,
    pub pip_span6_to_span6: f64,
    pub pip_local_to_bel: f64,
    pub pip_clock_to_local: f64,
    pub fanout_per_load: f64,
}

/// Speed grade 10, max column (fast corner)
pub const TIMING_GRADE10: NexusTimingData = NexusTimingData {
    lut4_delay: 0.270,         // OXIDE_COMB:LUT4 A→F max 270ps
    dff_clk_to_q: 0.441,      // OXIDE_FF CLK→Q max 441ps
    dff_setup: 0.0,            // OXIDE_FF DI@CLK setup 0ps
    dff_hold: 0.192,           // OXIDE_FF DI@CLK hold max 192ps
    carry_delay: 0.066,        // CCU2 FCI→FCO max 66ps
    io_input_delay: 0.8,       // estimated (no IO timing in prjoxide for LFCPNX)
    io_output_delay: 1.5,      // estimated
    ram_read_delay: 2.0,       // estimated (EBR timing not yet in prjoxide)

    cib_mux_delay: 0.044,      // cibmuxi→cibmuxo max 44ps
    span0_delay: 0.105,        // span0h→abcd max 105ps
    span2_delay: 0.058,        // span2w→abcd max 58ps
    span6_delay: 0.169,        // span6w chain max 169ps
    global_clock_delay: 0.031, // hpbx→clk max 31ps

    pip_lut_to_local: 0.134,   // f_lut→f max 134ps
    pip_local_to_local: 0.044, // cibmuxi→cibmuxo max 44ps
    pip_local_to_span: 0.162,  // f→span6w max 162ps
    pip_span2_to_span2: 0.059, // span2w→span0h max 61ps
    pip_span2_to_local: 0.058, // span2w→abcd max 58ps
    pip_span6_to_span6: 0.169, // e6:span6w→span6w max 169ps
    pip_local_to_bel: 0.105,   // span0h→abcd max 105ps
    pip_clock_to_local: 0.031, // hpbx→clk max 31ps
    fanout_per_load: 0.010,    // estimated
};

// ---------------------------------------------------------------------------
// Routing architecture constants
// ---------------------------------------------------------------------------

pub const ROUTING_CHANNELS: (u32, u32) = (32, 32);

/// Wire counts per tile per direction
pub const WIRE_LOCAL_COUNT: u8 = 24;  // CIB local wires
pub const WIRE_SPAN1_COUNT: u8 = 8;  // H01/V01
pub const WIRE_SPAN2_COUNT: u8 = 8;  // H02/V02
pub const WIRE_SPAN6_COUNT: u8 = 4;  // H06/V06

// ---------------------------------------------------------------------------
// Clock network
// ---------------------------------------------------------------------------

/// Maximum fabric frequency, ECLK domain (Hz)
pub const MAX_ECLK_FREQ: f64 = 400.0e6;
/// Maximum fabric frequency, PCLK domain (Hz)
pub const MAX_PCLK_FREQ: f64 = 200.0e6;
/// DLLs per device
pub const DLLS: u8 = 2;

// ---------------------------------------------------------------------------
// Tile architecture constants
// ---------------------------------------------------------------------------

/// LUT4s per PLC logic tile (4 slices × 2)
pub const LOGIC_LUTS_PER_TILE: u8 = 8;
/// FFs per PLC logic tile
pub const LOGIC_FFS_PER_TILE: u8 = 8;
/// IO cells per IO tile
pub const IO_CELLS_PER_TILE: u8 = 2;
/// MACs per DSP tile
pub const DSP_MACS_PER_TILE: u8 = 2;
/// EBR size in bits (18 Kb)
pub const EBR_SIZE_BITS: u32 = 18 * 1024;
/// Supported EBR data widths
pub const EBR_WIDTHS: &[u8] = &[1, 2, 4, 9, 18, 36];

/// Supported I/O standards
pub const IO_STANDARDS: &[&str] = &[
    "LVCMOS33", "LVCMOS25", "LVCMOS18", "LVCMOS12", "SSTL15", "HSUL12",
];
/// Supported drive strengths (mA)
pub const IO_DRIVE_STRENGTHS: &[u8] = &[2, 4, 8, 12, 16];
/// Differential pair support
pub const IO_DIFF_PAIRS: bool = true;

// ---------------------------------------------------------------------------
// Synthetic tile grid parameters
// ---------------------------------------------------------------------------

/// BRAM column spacing (one BRAM column every N columns)
pub const BRAM_COLUMN_SPACING: u32 = 10;
/// DSP column spacing
pub const DSP_COLUMN_SPACING: u32 = 20;

// ---------------------------------------------------------------------------
// Bitstream constants (from prjoxide)
// ---------------------------------------------------------------------------

/// Nexus SPI bitstream format (from prjoxide nxpack):
///
/// Structure is similar to ECP5: [dummy] [preamble] [commands] [frames] [postamble]
/// Nexus uses the same Lattice SPI command set but with different preamble.
///
/// Sources: prjoxide/libprjoxide/src/bitstream.rs, Lattice TN1313

/// Oxide text format magic (for FASM-like output)
pub const TEXT_FORMAT_MAGIC: &[u8] = b"OXIDE_NEXUS\n";

/// Dummy byte for SPI preamble
pub const BITSTREAM_DUMMY: u8 = 0xFF;
/// Number of dummy bytes before preamble
pub const BITSTREAM_DUMMY_COUNT: usize = 8;
/// Preamble / sync word for Nexus (big-endian: 0xFFFFBDB3, same as ECP5)
pub const BITSTREAM_PREAMBLE: [u8; 4] = [0xFF, 0xFF, 0xBD, 0xB3];

/// SPI command: LSC_DEVICE_CTRL — device control (Nexus-specific, replaces VERIFY_ID)
pub const CMD_DEVICE_CTRL: u8 = 0xE2;
/// SPI command: LSC_RESET_CRC
pub const CMD_RESET_CRC: u8 = 0x3B;
/// SPI command: LSC_PROG_CNTRL0
pub const CMD_PROG_CNTRL0: u8 = 0x22;
/// SPI command: LSC_INIT_ADDRESS
pub const CMD_INIT_ADDR: u8 = 0x46;
/// SPI command: LSC_PROG_INCR_NV — write one frame, auto-increment
pub const CMD_PROG_INCR: u8 = 0x70;
/// SPI command: ISC_PROGRAM_DONE
pub const CMD_PROGRAM_DONE: u8 = 0x5E;
/// SPI command: ISC_DISABLE
pub const CMD_ISC_DISABLE: u8 = 0x26;
/// SPI command: DUMMY / NOP
pub const CMD_DUMMY: u8 = 0xFF;

/// PROG_INCR_NV operand with CRC check enabled
pub const PROG_INCR_CRC_FLAG: u8 = 0x80;

/// CRC polynomial for Nexus bitstream
pub const CRC_POLYNOMIAL: u16 = 0x8005;
/// CRC initial value
pub const CRC_INIT: u16 = 0x0000;

/// Postamble trailing bytes
pub const POSTAMBLE_BYTES: usize = 4;
