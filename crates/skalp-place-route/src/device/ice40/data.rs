//! iCE40 device data from Project IceStorm and Lattice datasheets
//!
//! All device-specific constants in one place. The chipdb parser reads real
//! IceStorm data for wire/PIP connectivity; this file covers variant parameters,
//! tile architecture constants, and timing data.
//!
//! Sources:
//! - Project IceStorm chipdb: grid sizes, wire/PIP connectivity (embedded .txt files)
//! - iCE40 LP/HX Family Data Sheet (DS1040): LUT/BRAM counts, IO, timing tables
//! - iCE40 UltraPlus Family Data Sheet (DS1048): UP5K specifics, DSP
//!
//! License: The chipdb data from Project IceStorm is licensed under ISC license.
//! See: <https://github.com/YosysHQ/icestorm>

/// All variant-specific device parameters for one iCE40 device.
#[derive(Debug, Clone)]
pub struct Ice40DieData {
    // -- Fabric geometry (IceStorm chipdb) --
    /// Tile grid dimensions (columns, rows)
    pub grid: (u32, u32),
    /// Device name string
    pub name: &'static str,

    // -- Resource counts (datasheet) --
    /// LUT4 count (= logic cells)
    pub lut4s: usize,
    /// Block RAM count (4 Kbit each)
    pub brams: usize,
    /// Has MAC16 DSP blocks (UP5K only)
    pub has_dsp: bool,

    // -- RAM column positions (IceStorm chipdb) --
    /// X coordinates of RAM tile columns
    pub ram_columns: &'static [u32],

    // -- DSP tile region (UP5K only) --
    /// DSP column x coordinate (0 = no DSP)
    pub dsp_column: u32,
    /// DSP row range (inclusive start, exclusive end)
    pub dsp_rows: (u32, u32),

    // -- Speed family for timing --
    pub speed_family: Ice40SpeedFamily,
}

/// Speed family determines timing characteristics
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Ice40SpeedFamily {
    /// HX series (higher performance)
    Hx,
    /// LP series (~15% slower than HX)
    Lp,
    /// UP series (ultra low power, slowest)
    Up,
}

pub const HX1K: Ice40DieData = Ice40DieData {
    grid: (13, 17),
    name: "ice40hx1k",
    lut4s: 1280,
    brams: 16,
    has_dsp: false,
    ram_columns: &[3, 9],
    dsp_column: 0,
    dsp_rows: (0, 0),
    speed_family: Ice40SpeedFamily::Hx,
};

pub const HX4K: Ice40DieData = Ice40DieData {
    grid: (17, 17),
    name: "ice40hx4k",
    lut4s: 3520,
    brams: 20,
    has_dsp: false,
    ram_columns: &[4, 12],
    dsp_column: 0,
    dsp_rows: (0, 0),
    speed_family: Ice40SpeedFamily::Hx,
};

pub const HX8K: Ice40DieData = Ice40DieData {
    grid: (33, 33),
    name: "ice40hx8k",
    lut4s: 7680,
    brams: 32,
    has_dsp: false,
    ram_columns: &[8, 16, 24],
    dsp_column: 0,
    dsp_rows: (0, 0),
    speed_family: Ice40SpeedFamily::Hx,
};

pub const LP1K: Ice40DieData = Ice40DieData {
    grid: (13, 17),
    name: "ice40lp1k",
    lut4s: 1280,
    brams: 16,
    has_dsp: false,
    ram_columns: &[3, 9],
    dsp_column: 0,
    dsp_rows: (0, 0),
    speed_family: Ice40SpeedFamily::Lp,
};

pub const LP4K: Ice40DieData = Ice40DieData {
    grid: (17, 17),
    name: "ice40lp4k",
    lut4s: 3520,
    brams: 20,
    has_dsp: false,
    ram_columns: &[4, 12],
    dsp_column: 0,
    dsp_rows: (0, 0),
    speed_family: Ice40SpeedFamily::Lp,
};

pub const LP8K: Ice40DieData = Ice40DieData {
    grid: (33, 33),
    name: "ice40lp8k",
    lut4s: 7680,
    brams: 32,
    has_dsp: false,
    ram_columns: &[8, 16, 24],
    dsp_column: 0,
    dsp_rows: (0, 0),
    speed_family: Ice40SpeedFamily::Lp,
};

pub const UP5K: Ice40DieData = Ice40DieData {
    grid: (25, 21),
    name: "ice40up5k",
    lut4s: 5280,
    brams: 30,
    has_dsp: true,
    ram_columns: &[5, 12, 19],
    dsp_column: 23,
    dsp_rows: (1, 9), // y in 1..=8
    speed_family: Ice40SpeedFamily::Up,
};

// ---------------------------------------------------------------------------
// Timing data — iCE40 datasheet Table 4.2 (ns)
// ---------------------------------------------------------------------------

pub struct Ice40TimingData {
    // -- Cell delays --
    pub lut4_delay: f64,
    pub dff_clk_to_q: f64,
    pub dff_setup: f64,
    pub dff_hold: f64,
    pub carry_delay: f64,
    pub io_input_delay: f64,
    pub io_output_delay: f64,
    pub ram_read_delay: f64,

    // -- Wire delays --
    pub local_wire_delay: f64,
    pub span4_delay: f64,
    pub span12_delay: f64,
    pub global_clock_delay: f64,

    // -- PIP delays --
    pub pip_delay: f64,
    pub pip_belpin_to_local: f64,
    pub pip_local_to_local: f64,
    pub pip_local_to_span4: f64,
    pub pip_span4_to_span4: f64,
    pub pip_span4_to_local: f64,
    pub pip_span12_to_span12: f64,
    pub pip_local_to_belpin: f64,
    pub pip_global_to_local: f64,
    pub fanout_per_load: f64,
}

/// Default timing (generic iCE40, datasheet typical)
pub const TIMING_DEFAULT: Ice40TimingData = Ice40TimingData {
    lut4_delay: 0.59,
    dff_clk_to_q: 0.85,
    dff_setup: 0.18,
    dff_hold: 0.0,
    carry_delay: 0.09,
    io_input_delay: 1.2,
    io_output_delay: 2.5,
    ram_read_delay: 3.5,
    local_wire_delay: 0.05,
    span4_delay: 0.2,
    span12_delay: 0.4,
    global_clock_delay: 0.1,
    pip_delay: 0.1,
    pip_belpin_to_local: 0.03,
    pip_local_to_local: 0.05,
    pip_local_to_span4: 0.15,
    pip_span4_to_span4: 0.10,
    pip_span4_to_local: 0.10,
    pip_span12_to_span12: 0.15,
    pip_local_to_belpin: 0.02,
    pip_global_to_local: 0.05,
    fanout_per_load: 0.02,
};

/// HX series (higher performance)
pub const TIMING_HX: Ice40TimingData = Ice40TimingData {
    lut4_delay: 0.54,
    dff_clk_to_q: 0.76,
    dff_setup: 0.15,
    dff_hold: 0.0,
    carry_delay: 0.08,
    io_input_delay: 1.0,
    io_output_delay: 2.2,
    ram_read_delay: 3.2,
    local_wire_delay: 0.04,
    span4_delay: 0.18,
    span12_delay: 0.35,
    global_clock_delay: 0.08,
    pip_delay: 0.09,
    pip_belpin_to_local: 0.025,
    pip_local_to_local: 0.04,
    pip_local_to_span4: 0.13,
    pip_span4_to_span4: 0.09,
    pip_span4_to_local: 0.09,
    pip_span12_to_span12: 0.13,
    pip_local_to_belpin: 0.018,
    pip_global_to_local: 0.04,
    fanout_per_load: 0.018,
};

/// LP series (~15% slower than HX)
pub const TIMING_LP: Ice40TimingData = Ice40TimingData {
    lut4_delay: 0.65,
    dff_clk_to_q: 0.95,
    dff_setup: 0.20,
    dff_hold: 0.0,
    carry_delay: 0.10,
    io_input_delay: 1.4,
    io_output_delay: 2.8,
    ram_read_delay: 3.8,
    local_wire_delay: 0.06,
    span4_delay: 0.22,
    span12_delay: 0.45,
    global_clock_delay: 0.12,
    pip_delay: 0.11,
    pip_belpin_to_local: 0.035,
    pip_local_to_local: 0.058,
    pip_local_to_span4: 0.17,
    pip_span4_to_span4: 0.115,
    pip_span4_to_local: 0.115,
    pip_span12_to_span12: 0.17,
    pip_local_to_belpin: 0.023,
    pip_global_to_local: 0.058,
    fanout_per_load: 0.023,
};

/// UP series (ultra low power, slowest)
pub const TIMING_UP: Ice40TimingData = Ice40TimingData {
    lut4_delay: 0.70,
    dff_clk_to_q: 1.0,
    dff_setup: 0.22,
    dff_hold: 0.0,
    carry_delay: 0.11,
    io_input_delay: 1.5,
    io_output_delay: 3.0,
    ram_read_delay: 4.0,
    local_wire_delay: 0.07,
    span4_delay: 0.25,
    span12_delay: 0.50,
    global_clock_delay: 0.15,
    pip_delay: 0.12,
    pip_belpin_to_local: 0.04,
    pip_local_to_local: 0.065,
    pip_local_to_span4: 0.19,
    pip_span4_to_span4: 0.13,
    pip_span4_to_local: 0.13,
    pip_span12_to_span12: 0.19,
    pip_local_to_belpin: 0.026,
    pip_global_to_local: 0.065,
    fanout_per_load: 0.026,
};

// ---------------------------------------------------------------------------
// Routing architecture constants (from IceStorm)
// ---------------------------------------------------------------------------

pub const ROUTING_CHANNELS: (u32, u32) = (20, 20);

/// Wire counts per tile
pub const WIRE_LOCAL_COUNT: u8 = 8;
pub const WIRE_SPAN4H_COUNT: u8 = 4;
pub const WIRE_SPAN4V_COUNT: u8 = 4;
pub const WIRE_SPAN12H_COUNT: u8 = 2;
pub const WIRE_SPAN12V_COUNT: u8 = 2;

/// Wire delays in synthetic model (ps)
pub const WIRE_LOCAL_DELAY: u32 = 50;
pub const WIRE_SPAN4_DELAY: u32 = 200;
pub const WIRE_SPAN12_DELAY: u32 = 400;
pub const WIRE_NEIGHBOUR_DELAY: u32 = 100;
pub const WIRE_CARRY_DELAY: u32 = 30;
pub const WIRE_GLOBAL_DELAY: u32 = 100;

// ---------------------------------------------------------------------------
// Clock resources (from IceStorm)
// ---------------------------------------------------------------------------

pub const GLOBAL_CLOCKS: u8 = 8;
pub const PLLS: u8 = 1;
pub const DLLS: u8 = 0;
/// Max GCLK frequency (Hz)
pub const MAX_GCLK_FREQ: f64 = 275.0e6;

// ---------------------------------------------------------------------------
// Tile architecture constants
// ---------------------------------------------------------------------------

pub const LOGIC_LUTS_PER_TILE: u8 = 8;
pub const LOGIC_FFS_PER_TILE: u8 = 8;
pub const IO_CELLS_PER_TILE: u8 = 2;
pub const DSP_MACS_PER_TILE: u8 = 1;

/// RAM block size (4 Kbit)
pub const RAM_SIZE_BITS: u32 = 4096;
/// Supported RAM data widths
pub const RAM_WIDTHS: &[u8] = &[1, 2, 4, 8, 16];

/// Supported I/O standards
pub const IO_STANDARDS: &[&str] = &["LVCMOS33", "LVCMOS25", "LVCMOS18"];
/// Supported drive strengths (mA)
pub const IO_DRIVE_STRENGTHS: &[u8] = &[4, 8, 12];
/// No differential pair support on iCE40
pub const IO_DIFF_PAIRS: bool = false;

// ---------------------------------------------------------------------------
// Bitstream constants (from IceStorm)
// ---------------------------------------------------------------------------

/// Bitstream preamble / sync pattern
pub const BITSTREAM_PREAMBLE: [u8; 4] = [0x7E, 0xAA, 0x99, 0x7E];
/// Binary sync pattern (start of .bin file)
pub const BITSTREAM_SYNC: [u8; 4] = [0xFF, 0x00, 0x00, 0xFF];
/// CRAM write command byte
pub const CRAM_WRITE_CMD: u8 = 0x01;
/// CRAM rows per logic tile (bit rows in .asc format)
pub const CRAM_LOGIC_ROWS: u8 = 16;
/// CRAM columns per logic tile
pub const CRAM_LOGIC_COLS: u8 = 54;
/// CRAM columns per IO tile
pub const CRAM_IO_COLS: u8 = 18;
/// CRAM columns per RAM tile
pub const CRAM_RAM_COLS: u8 = 42;
/// CRC polynomial (CRC-16)
pub const CRC_POLYNOMIAL: u16 = 0x8005;
/// CRC initial value
pub const CRC_INIT: u16 = 0xFFFF;
/// File magic for IceStorm ASCII (.asc) format
pub const ASC_MAGIC: &str = ".comment";
