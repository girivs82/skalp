//! ECP5 device data from Project Trellis and Lattice DS1044
//!
//! All device-specific constants in one place. When prjtrellis-db is updated,
//! only this file needs to change.
//!
//! Sources:
//! - prjtrellis devices.json: grid sizes, IDCODEs, bitstream geometry, PLC2 counts
//! - prjtrellis-db timing/: cell and interconnect delays (speed grade 8)
//! - prjtrellis-db iodb.json: package names and IO counts
//! - Lattice DS1044 Table 1-1: LUT counts, EBR, MULT18, PLL, DLL, IO, dist RAM
//!
//! Copyright (C) 2018 The Project Trellis Authors. All rights reserved.
//! Licensed under ISC License. See: <https://github.com/YosysHQ/prjtrellis>

/// All variant-specific device parameters for one ECP5 die size.
#[derive(Debug, Clone)]
pub struct Ecp5DieData {
    // -- Fabric geometry (prjtrellis devices.json) --
    /// Tile grid dimensions (columns, rows)
    pub grid: (u32, u32),
    /// PLC2 tile count (each = 2 slices = 4 LUT4 + 4 FF)
    pub plc2_tiles: usize,

    // -- Resource counts (DS1044 Table 1-1) --
    /// Approximate LUT4 count (marketing number)
    pub lut4s: usize,
    /// EBR blocks (DP16KD, 18 Kb each)
    pub ebr_blocks: usize,
    /// 18×18 multiplier blocks (MULT18X18D)
    pub mult18s: usize,
    /// Maximum IO count (largest package)
    pub max_ios: usize,
    /// PLLs (EHXPLLL)
    pub plls: u8,
    /// DLLs (delay-locked loops)
    pub dlls: u8,
    /// Distributed RAM capacity (Kbits)
    pub dist_ram_kbits: usize,

    // -- Bitstream (prjtrellis devices.json) --
    /// JTAG IDCODE for LFE5U variant
    pub idcode_u: u32,
    /// JTAG IDCODE for LFE5UM variant (0 if no UM variant)
    pub idcode_um: u32,
    /// JTAG IDCODE for LFE5UM5G variant (0 if no UM5G variant)
    pub idcode_um5g: u32,
    /// Bitstream frame count
    pub bitstream_frames: u32,
    /// Bits per bitstream frame
    pub bits_per_frame: u32,

    // -- Packages (prjtrellis iodb.json) --
    /// Available package names
    pub packages: &'static [&'static str],

    // -- SERDES --
    /// DCU blocks (0 for LFE5U, 1-2 for UM/UM5G)
    pub dcu_blocks: u8,
    /// SERDES channels (2 per DCU)
    pub serdes_channels: u8,
}

/// Die data for 12F (fuse-limited 25F die)
pub const DIE_12F: Ecp5DieData = Ecp5DieData {
    grid: (72, 50),
    plc2_tiles: 3_036,
    lut4s: 12_000,
    ebr_blocks: 32,
    mult18s: 28,
    max_ios: 197,
    plls: 2,
    dlls: 2,
    dist_ram_kbits: 194,
    idcode_u: 0x2111_1043,
    idcode_um: 0, // no UM-12F
    idcode_um5g: 0,
    bitstream_frames: 7562,
    bits_per_frame: 592,
    packages: &["CSFBGA285", "CABGA256", "CABGA381", "TQFP144"],
    dcu_blocks: 0,
    serdes_channels: 0,
};

/// Die data for 25F (same die as 12F)
pub const DIE_25F: Ecp5DieData = Ecp5DieData {
    grid: (72, 50),
    plc2_tiles: 3_036,
    lut4s: 24_000,
    ebr_blocks: 56,
    mult18s: 28,
    max_ios: 197,
    plls: 2,
    dlls: 2,
    dist_ram_kbits: 194,
    idcode_u: 0x4111_1043,
    idcode_um: 0x0111_1043,
    idcode_um5g: 0x8111_1043,
    bitstream_frames: 7562,
    bits_per_frame: 592,
    packages: &["CSFBGA285", "CABGA256", "CABGA381", "TQFP144"],
    dcu_blocks: 1,
    serdes_channels: 2,
};

/// Die data for 45F
pub const DIE_45F: Ecp5DieData = Ecp5DieData {
    grid: (90, 71),
    plc2_tiles: 5_481,
    lut4s: 44_000,
    ebr_blocks: 108,
    mult18s: 72,
    max_ios: 245,
    plls: 4,
    dlls: 4,
    dist_ram_kbits: 351,
    idcode_u: 0x4111_2043,
    idcode_um: 0x0111_2043,
    idcode_um5g: 0x8111_2043,
    bitstream_frames: 9470,
    bits_per_frame: 846,
    packages: &["CSFBGA285", "CABGA256", "CABGA381", "CABGA554", "TQFP144"],
    dcu_blocks: 2,
    serdes_channels: 4,
};

/// Die data for 85F
pub const DIE_85F: Ecp5DieData = Ecp5DieData {
    grid: (126, 95),
    plc2_tiles: 10_455,
    lut4s: 84_000,
    ebr_blocks: 208,
    mult18s: 156,
    max_ios: 365,
    plls: 4,
    dlls: 4,
    dist_ram_kbits: 669,
    idcode_u: 0x4111_3043,
    idcode_um: 0x0111_3043,
    idcode_um5g: 0x8111_3043,
    bitstream_frames: 13294,
    bits_per_frame: 1136,
    packages: &["CSFBGA285", "CABGA381", "CABGA554", "CABGA756"],
    dcu_blocks: 2,
    serdes_channels: 4,
};

// ---------------------------------------------------------------------------
// Timing data — prjtrellis-db timing database, speed grade 8 (fastest)
// ---------------------------------------------------------------------------

/// All timing constants for one speed grade, in nanoseconds.
pub struct Ecp5TimingData {
    // -- Cell delays (SLOGICB, typ values) --
    /// LUT4 A0→F0 propagation
    pub lut4_delay: f64,
    /// DFF CLK→Q0
    pub dff_clk_to_q: f64,
    /// DFF setup (LSR setup, conservative)
    pub dff_setup: f64,
    /// DFF hold (DI hold at CLK)
    pub dff_hold: f64,
    /// Carry FCI→FCO (SCCU2C)
    pub carry_delay: f64,
    /// IO input delay (estimated, package-dependent)
    pub io_input_delay: f64,
    /// IO output delay (estimated, package-dependent)
    pub io_output_delay: f64,
    /// RAM read delay (DP16KD, estimated)
    pub ram_read_delay: f64,

    // -- Interconnect delays (typ values) --
    /// slice_internal (intra-slice)
    pub local_wire_delay: f64,
    /// f_to_span2he (H02 entry from LUT output)
    pub span2_delay: f64,
    /// f_to_span6vn (H06/V06 entry from LUT output)
    pub span6_delay: f64,
    /// Primary clock distribution to tile
    pub global_clock_delay: f64,
    /// Generic CIB mux (f_to_d)
    pub cib_mux_delay: f64,

    // -- PIP delays by type --
    /// LUT output → local (f_to_span0hr)
    pub pip_f_to_local: f64,
    /// Intra-tile (f_to_d)
    pub pip_local_to_local: f64,
    /// Local → H02 (f_to_span2he)
    pub pip_local_to_span2: f64,
    /// H02 cascade (span2he_to_span2he_e2)
    pub pip_span2_cascade: f64,
    /// H02 → slice input (span2he_to_a)
    pub pip_span2_to_bel: f64,
    /// H06 cascade (span6he_to_span6he_e6)
    pub pip_span6_cascade: f64,
    /// Internal → BEL input
    pub pip_to_bel: f64,
    /// Clock → tile
    pub pip_clock_to_local: f64,
    /// Fanout delay per additional load
    pub fanout_per_load: f64,
}

/// Speed grade 8 (fastest), typ column.
pub const TIMING_SPEED8: Ecp5TimingData = Ecp5TimingData {
    // SLOGICB cell delays (speed grade 8, typ)
    lut4_delay: 0.166,    // A0→F0 [153, 166, 180] ps
    dff_clk_to_q: 0.362,  // CLK→Q0 [329, 362, 395] ps
    dff_setup: 0.237,     // LSR setup max 287ps; DI hold 222ps
    dff_hold: 0.222,      // DI hold at CLK [213, 222, 232] ps
    carry_delay: 0.053,   // SCCU2C FCI→FCO [50, 53, 56] ps
    io_input_delay: 1.0,  // estimated (package-dependent)
    io_output_delay: 1.8, // estimated (package-dependent)
    ram_read_delay: 2.5,  // DP16KD estimated

    // Interconnect delays (speed grade 8, typ)
    local_wire_delay: 0.027,   // slice_internal [22, 27, 31] ps
    span2_delay: 0.145,        // f_to_span2he [140, 145, 151] ps
    span6_delay: 0.210,        // f_to_span6vn [195, 210, 226] ps
    global_clock_delay: 0.050, // estimated primary distribution
    cib_mux_delay: 0.068,      // f_to_d [60, 68, 76] ps

    // PIP delays (speed grade 8, typ)
    pip_f_to_local: 0.046,     // f_to_span0hr [39, 46, 52] ps
    pip_local_to_local: 0.068, // f_to_d [60, 68, 76] ps
    pip_local_to_span2: 0.145, // f_to_span2he [140, 145, 151] ps
    pip_span2_cascade: 0.168,  // span2he_to_span2he_e2 [157, 168, 179] ps
    pip_span2_to_bel: 0.301,   // span2he_to_a [251, 301, 351] ps
    pip_span6_cascade: 0.192,  // span6he_to_span6he_e6 [187, 192, 197] ps
    pip_to_bel: 0.031,         // slice_internal → BEL
    pip_clock_to_local: 0.050, // clock distribution to tile
    fanout_per_load: 0.005,    // ~4.5ps/fanout from f_to_d model
};

// ---------------------------------------------------------------------------
// Routing architecture constants (from prjtrellis/nextpnr wire types)
// ---------------------------------------------------------------------------

/// Routing channel width per direction
pub const ROUTING_CHANNELS: (u32, u32) = (56, 56);

/// Wire segment definitions: (length_in_tiles, count_per_tile)
/// H00/V00 local, H01/V01 span-1, H02/V02 span-2, H06/V06 span-6
pub const WIRE_LOCAL_COUNT: u8 = 20;
pub const WIRE_SPAN1_COUNT: u8 = 8; // H01/V01 per direction
pub const WIRE_SPAN2_COUNT: u8 = 6; // H02/V02 per direction
pub const WIRE_SPAN6_COUNT: u8 = 3; // H06/V06 per direction

// ---------------------------------------------------------------------------
// Clock network (from prjtrellis globals.json)
// ---------------------------------------------------------------------------

/// Number of primary + secondary global clocks
pub const GLOBAL_CLOCKS: u8 = 16;

/// Maximum fabric clock frequency (speed grade -8), Hz
pub const MAX_FABRIC_FREQ: f64 = 400.0e6;

/// UM SERDES line rate (Gbps)
pub const SERDES_RATE_UM: f64 = 3.125;
/// UM5G SERDES line rate (Gbps)
pub const SERDES_RATE_UM5G: f64 = 5.0;

// ---------------------------------------------------------------------------
// Tile architecture constants
// ---------------------------------------------------------------------------

/// BELs per PLC2 logic tile: 8 LUT4 + 8 FF + 1 CCU2C carry
pub const LOGIC_LUTS_PER_TILE: u8 = 8;
pub const LOGIC_FFS_PER_TILE: u8 = 8;

/// IO cells per PIC tile
pub const IO_CELLS_PER_TILE: u8 = 2;

/// EBR size in bits (DP16KD = 18 Kb)
pub const EBR_SIZE_BITS: u32 = 18 * 1024;
/// Supported EBR data widths
pub const EBR_WIDTHS: &[u8] = &[1, 2, 4, 9, 18, 36];

/// Supported I/O standards
pub const IO_STANDARDS: &[&str] = &[
    "LVCMOS33", "LVCMOS25", "LVCMOS18", "LVCMOS12", "SSTL15", "LVDS",
];
/// Supported drive strengths (mA)
pub const IO_DRIVE_STRENGTHS: &[u8] = &[4, 8, 12, 16];

/// DSP: MULT18X18D blocks per DSP tile
pub const DSP_MULTS_PER_TILE: u8 = 2;

// ---------------------------------------------------------------------------
// Synthetic tile grid parameters
// ---------------------------------------------------------------------------

/// BRAM column spacing (one BRAM column every N columns)
pub const BRAM_COLUMN_SPACING: u32 = 8;
/// DSP column spacing
pub const DSP_COLUMN_SPACING: u32 = 16;

// ---------------------------------------------------------------------------
// Bitstream constants (from prjtrellis)
// ---------------------------------------------------------------------------

// ECP5 SPI bitstream format (from prjtrellis ecppack):
//
// Structure: [dummy] [preamble] [commands...] [frame data...] [postamble]
//
// Each command is 4 bytes: opcode + 3 operand/padding bytes.
// Frame data follows LSC_PROG_INCR_NV, one frame at a time.
//
// Sources: prjtrellis/libtrellis/src/Bitstream.cpp, Lattice TN1260

/// Trellis text format magic (for .config / FASM-like output)
pub const TEXT_FORMAT_MAGIC: &[u8] = b"TRELLIS_ECP5\n";
/// Section headers in Trellis text format
pub const TEXT_SECTION_TILES: &[u8] = b"TILES\n";
pub const TEXT_SECTION_IOCONF: &[u8] = b"IOCONF\n";

/// Dummy byte for SPI preamble
pub const BITSTREAM_DUMMY: u8 = 0xFF;
/// Number of dummy bytes before preamble
pub const BITSTREAM_DUMMY_COUNT: usize = 8;
/// Preamble / sync word (big-endian u32: 0xFFFFBDB3)
pub const BITSTREAM_PREAMBLE: [u8; 4] = [0xFF, 0xFF, 0xBD, 0xB3];

/// SPI command: VERIFY_ID — checks IDCODE (4-byte operand: IDCODE)
pub const CMD_VERIFY_ID: u8 = 0xE2;
/// SPI command: LSC_RESET_CRC — reset CRC accumulator
pub const CMD_RESET_CRC: u8 = 0x3B;
/// SPI command: LSC_PROG_CNTRL0 — set control register 0 (4-byte operand)
pub const CMD_PROG_CNTRL0: u8 = 0x22;
/// SPI command: LSC_INIT_ADDRESS — reset frame address to 0
pub const CMD_INIT_ADDR: u8 = 0x46;
/// SPI command: LSC_PROG_INCR_NV — write one frame, auto-increment address
/// Operand byte 2 bit 7: CRC_CHECK flag (1 = include 16-bit CRC after frame)
pub const CMD_PROG_INCR: u8 = 0x70;
/// SPI command: ISC_PROGRAM_DONE — release from config mode
pub const CMD_PROGRAM_DONE: u8 = 0x5E;
/// SPI command: ISC_DISABLE — exit ISC mode
pub const CMD_ISC_DISABLE: u8 = 0x26;
/// SPI command: DUMMY — padding / NOP
pub const CMD_DUMMY: u8 = 0xFF;

/// PROG_INCR_NV operand with CRC check enabled (bit 7 of byte 2)
pub const PROG_INCR_CRC_FLAG: u8 = 0x80;

// Bytes per frame = bits_per_frame / 8 (varies by die, see Ecp5DieData)
// Use `die.bits_per_frame / 8` at runtime.

/// CRC polynomial for ECP5 bitstream (CRC-16/AUG-CCITT)
pub const CRC_POLYNOMIAL: u16 = 0x8005;
/// CRC initial value
pub const CRC_INIT: u16 = 0x0000;

/// Postamble trailing bytes
pub const POSTAMBLE_BYTES: usize = 4;
