//! Xilinx 7-series device data from Project X-Ray (prjxray)
//!
//! All device-specific constants in one place. When prjxray-db is updated,
//! only this file needs to change.
//!
//! Sources:
//! - prjxray-db: grid sizes, tile types, bitstream geometry, timing (SDF files)
//! - Xilinx DS180 (7 Series Overview): resource counts, packages, speed grades
//! - Xilinx DS181 (Artix-7 DC/AC): timing parameters
//! - Xilinx DS182 (Kintex-7 DC/AC): timing parameters
//! - Xilinx UG474 (7 Series CLB): slice architecture, LUT6/FF details
//! - Xilinx UG473 (7 Series Memory): BRAM architecture
//! - Xilinx UG479 (7 Series DSP48E1): DSP architecture
//!
//! Project X-Ray database is dedicated to the public domain under CC0 1.0.
//! See: <https://github.com/f4pga/prjxray> and <https://github.com/f4pga/prjxray-db>

/// Speed family determines timing characteristics
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Xc7SpeedFamily {
    /// Artix-7 (28nm HPL, lowest cost)
    Artix,
    /// Kintex-7 (28nm HPL, higher performance — ~18% faster than Artix)
    Kintex,
    /// Spartan-7 (28nm HPL, cost-optimized subset of Artix)
    Spartan,
}

/// All variant-specific device parameters for one Xilinx 7-series die.
#[derive(Debug, Clone)]
pub struct Xc7DieData {
    // -- Fabric geometry (prjxray-db) --
    /// Tile grid dimensions (columns, rows) from prjxray tilegrid.json
    pub grid: (u32, u32),
    /// Device name string
    pub name: &'static str,

    // -- Resource counts (DS180 Table 1) --
    /// Logic cell count (= LUT6 count, each slice has 4 LUT6)
    pub lut6s: usize,
    /// Slice count (SLICEL + SLICEM)
    pub slices: usize,
    /// CLB count (2 slices per CLB)
    pub clbs: usize,
    /// Block RAM count (RAMB36E1, 36 Kb each)
    pub bram36s: usize,
    /// Block RAM 18Kb count (RAMB18E1, can use half of BRAM36)
    pub bram18s: usize,
    /// DSP48E1 slice count
    pub dsp48e1s: usize,
    /// Maximum I/O count (HR + HP banks, largest package)
    pub max_ios: usize,
    /// CMT blocks (each has 1 MMCM + 1 PLL)
    pub cmts: u8,
    /// MMCMs (MMCME2_ADV)
    pub mmcms: u8,
    /// PLLs (PLLE2_ADV)
    pub plls: u8,
    /// GTP transceivers (Artix-7)
    pub gtps: u8,
    /// GTX transceivers (Kintex-7)
    pub gtxs: u8,
    /// PCIe hard blocks (PCIE_2_1)
    pub pcie_blocks: u8,
    /// XADC blocks
    pub xadc: u8,

    // -- Speed family --
    pub speed_family: Xc7SpeedFamily,

    // -- Bitstream (prjxray-db) --
    /// JTAG IDCODE
    pub idcode: u32,

    // -- Packages (DS180) --
    pub packages: &'static [&'static str],
}

// ---------------------------------------------------------------------------
// Artix-7 variants (DS180 Table 1 + prjxray-db)
// ---------------------------------------------------------------------------

/// XC7A35T — smallest Artix-7
pub const XC7A35T: Xc7DieData = Xc7DieData {
    grid: (131, 106),
    name: "xc7a35t",
    lut6s: 20_800,
    slices: 5_200,
    clbs: 2_600,
    bram36s: 50,
    bram18s: 100,
    dsp48e1s: 90,
    max_ios: 250,
    cmts: 5,
    mmcms: 5,
    plls: 5,
    gtps: 0,
    gtxs: 0,
    pcie_blocks: 0,
    xadc: 1,
    speed_family: Xc7SpeedFamily::Artix,
    idcode: 0x0362_D093,
    packages: &["CPG236", "CSG324", "CSG325", "FTG256", "FGG484"],
};

/// XC7A50T
pub const XC7A50T: Xc7DieData = Xc7DieData {
    grid: (131, 106),
    name: "xc7a50t",
    lut6s: 32_600,
    slices: 8_150,
    clbs: 4_075,
    bram36s: 75,
    bram18s: 150,
    dsp48e1s: 120,
    max_ios: 250,
    cmts: 5,
    mmcms: 5,
    plls: 5,
    gtps: 0,
    gtxs: 0,
    pcie_blocks: 0,
    xadc: 1,
    speed_family: Xc7SpeedFamily::Artix,
    idcode: 0x0362_C093,
    packages: &["CPG236", "CSG324", "CSG325", "FTG256", "FGG484"],
};

/// XC7A100T
pub const XC7A100T: Xc7DieData = Xc7DieData {
    grid: (185, 178),
    name: "xc7a100t",
    lut6s: 63_400,
    slices: 15_850,
    clbs: 7_925,
    bram36s: 135,
    bram18s: 270,
    dsp48e1s: 240,
    max_ios: 300,
    cmts: 6,
    mmcms: 6,
    plls: 6,
    gtps: 0,
    gtxs: 0,
    pcie_blocks: 0,
    xadc: 1,
    speed_family: Xc7SpeedFamily::Artix,
    idcode: 0x0362_4093,
    packages: &["CSG324", "FGG484", "FGG676", "FFG676"],
};

/// XC7A200T — largest Artix-7
pub const XC7A200T: Xc7DieData = Xc7DieData {
    grid: (221, 260),
    name: "xc7a200t",
    lut6s: 134_600,
    slices: 33_650,
    clbs: 16_825,
    bram36s: 365,
    bram18s: 730,
    dsp48e1s: 740,
    max_ios: 500,
    cmts: 10,
    mmcms: 10,
    plls: 10,
    gtps: 16,
    gtxs: 0,
    pcie_blocks: 1,
    xadc: 1,
    speed_family: Xc7SpeedFamily::Artix,
    idcode: 0x0362_0093,
    packages: &["FBG484", "FBG676", "FFG1156", "SBG484"],
};

// ---------------------------------------------------------------------------
// Kintex-7 variants (DS180 Table 1 + prjxray-db)
// ---------------------------------------------------------------------------

/// XC7K70T — smallest Kintex-7
pub const XC7K70T: Xc7DieData = Xc7DieData {
    grid: (165, 150),
    name: "xc7k70t",
    lut6s: 41_000,
    slices: 10_250,
    clbs: 5_125,
    bram36s: 135,
    bram18s: 270,
    dsp48e1s: 240,
    max_ios: 300,
    cmts: 6,
    mmcms: 6,
    plls: 6,
    gtps: 0,
    gtxs: 8,
    pcie_blocks: 1,
    xadc: 1,
    speed_family: Xc7SpeedFamily::Kintex,
    idcode: 0x0364_7093,
    packages: &["FBG484", "FBG676", "FFG676"],
};

/// XC7K160T
pub const XC7K160T: Xc7DieData = Xc7DieData {
    grid: (203, 222),
    name: "xc7k160t",
    lut6s: 101_400,
    slices: 25_350,
    clbs: 12_675,
    bram36s: 325,
    bram18s: 650,
    dsp48e1s: 600,
    max_ios: 400,
    cmts: 8,
    mmcms: 8,
    plls: 8,
    gtps: 0,
    gtxs: 8,
    pcie_blocks: 1,
    xadc: 1,
    speed_family: Xc7SpeedFamily::Kintex,
    idcode: 0x0364_C093,
    packages: &["FBG484", "FBG676", "FFG676"],
};

/// XC7K325T — KarythraGPU fallback target
pub const XC7K325T: Xc7DieData = Xc7DieData {
    grid: (237, 365),
    name: "xc7k325t",
    lut6s: 203_800,
    slices: 50_950,
    clbs: 25_475,
    bram36s: 445,
    bram18s: 890,
    dsp48e1s: 840,
    max_ios: 500,
    cmts: 10,
    mmcms: 10,
    plls: 10,
    gtps: 0,
    gtxs: 16,
    pcie_blocks: 1,
    xadc: 1,
    speed_family: Xc7SpeedFamily::Kintex,
    idcode: 0x0365_1093,
    packages: &["FBG676", "FBG900", "FFG676", "FFG900"],
};

// ---------------------------------------------------------------------------
// Spartan-7 variants (DS180)
// ---------------------------------------------------------------------------

/// XC7S50 — mid-range Spartan-7
pub const XC7S50: Xc7DieData = Xc7DieData {
    grid: (131, 106),
    name: "xc7s50",
    lut6s: 32_600,
    slices: 8_150,
    clbs: 4_075,
    bram36s: 75,
    bram18s: 150,
    dsp48e1s: 120,
    max_ios: 250,
    cmts: 5,
    mmcms: 5,
    plls: 5,
    gtps: 0,
    gtxs: 0,
    pcie_blocks: 0,
    xadc: 1,
    speed_family: Xc7SpeedFamily::Spartan,
    idcode: 0x0362_F093,
    packages: &["CSGA225", "CSGA324", "FTGB196", "FGGA484"],
};

// ---------------------------------------------------------------------------
// Timing data — prjxray-db SDF files + DS181/DS182
// ---------------------------------------------------------------------------

/// All timing constants for one speed grade, in nanoseconds.
pub struct Xc7TimingData {
    // -- Cell delays --
    /// LUT6 A→O propagation (SLICEL, any input to O6)
    pub lut6_delay: f64,
    /// DFF CLK→Q (FDRE)
    pub dff_clk_to_q: f64,
    /// DFF setup (D@CLK)
    pub dff_setup: f64,
    /// DFF hold (D@CLK)
    pub dff_hold: f64,
    /// CARRY4 CIN→CO[3] propagation
    pub carry4_delay: f64,
    /// I/O input delay (IBUF, estimated)
    pub io_input_delay: f64,
    /// I/O output delay (OBUF, estimated)
    pub io_output_delay: f64,
    /// BRAM read delay (RAMB36E1, CLK→DO)
    pub bram_read_delay: f64,
    /// DSP48E1 A→P delay
    pub dsp_delay: f64,

    // -- Interconnect delays --
    /// Local (intra-site) wire delay
    pub local_wire_delay: f64,
    /// Single wire delay (span 1)
    pub single_delay: f64,
    /// Double wire delay (span 2)
    pub double_delay: f64,
    /// Quad wire delay (span 4-6)
    pub quad_delay: f64,
    /// Long wire delay (span 12)
    pub long_delay: f64,
    /// Global clock delay (BUFG → FF)
    pub global_clock_delay: f64,

    // -- PIP delays --
    /// BEL output → local switch
    pub pip_bel_to_local: f64,
    /// Local → local (intra-INT)
    pub pip_local_to_local: f64,
    /// Local → single
    pub pip_local_to_single: f64,
    /// Single → single cascade
    pub pip_single_to_single: f64,
    /// Single/Double → local
    pub pip_span_to_local: f64,
    /// Local → double
    pub pip_local_to_double: f64,
    /// Double → double cascade
    pub pip_double_to_double: f64,
    /// Local → quad
    pub pip_local_to_quad: f64,
    /// Long → local
    pub pip_long_to_local: f64,
    /// Long → long cascade
    pub pip_long_to_long: f64,
    /// Clock → local
    pub pip_clock_to_local: f64,
    /// Fanout delay per additional load
    pub fanout_per_load: f64,
}

/// Artix-7 speed grade -1 (slowest), from prjxray-db SDF + DS181
pub const TIMING_ARTIX_1: Xc7TimingData = Xc7TimingData {
    // Cell delays (from prjxray SDF, speed grade -1)
    lut6_delay: 0.124,          // A6LUT A→O6 124ps
    dff_clk_to_q: 0.303,       // FDRE CLK→Q 303ps
    dff_setup: 0.058,           // FDRE D@CLK setup 58ps
    dff_hold: 0.115,            // FDRE D@CLK hold 115ps
    carry4_delay: 0.114,        // CARRY4 CIN→CO[3] 114ps
    io_input_delay: 0.800,      // IBUF estimated
    io_output_delay: 1.500,     // OBUF estimated
    bram_read_delay: 2.454,     // RAMB36E1 CLK→DOA 2454ps
    dsp_delay: 1.800,           // DSP48E1 A→P estimated

    // Interconnect delays
    local_wire_delay: 0.030,    // site-internal
    single_delay: 0.090,        // NN1/EE1/SS1/WW1
    double_delay: 0.140,        // NN2/EE2/SS2/WW2
    quad_delay: 0.200,          // NN4/EE4/SS4/WW4 (or 6-tile variants)
    long_delay: 0.350,          // LH/LV 12-tile long wires
    global_clock_delay: 0.100,  // BUFG to FF CLK pin

    // PIP delays
    pip_bel_to_local: 0.035,    // site pin → INT
    pip_local_to_local: 0.040,  // INT internal mux
    pip_local_to_single: 0.100, // INT → single wire entry
    pip_single_to_single: 0.080, // single cascade
    pip_span_to_local: 0.060,   // single/double → INT
    pip_local_to_double: 0.130, // INT → double wire entry
    pip_double_to_double: 0.110, // double cascade
    pip_local_to_quad: 0.170,   // INT → quad wire entry
    pip_long_to_local: 0.080,   // long → INT
    pip_long_to_long: 0.180,    // long cascade
    pip_clock_to_local: 0.040,  // GCLK → tile
    fanout_per_load: 0.008,     // ~8ps per fanout
};

/// Kintex-7 speed grade -1 (slowest), estimated from Artix-7 × 0.82
/// (Kintex uses the same 28nm HPL process but with better routing)
pub const TIMING_KINTEX_1: Xc7TimingData = Xc7TimingData {
    lut6_delay: 0.102,          // 124 × 0.82
    dff_clk_to_q: 0.248,       // 303 × 0.82
    dff_setup: 0.048,           // 58 × 0.82
    dff_hold: 0.094,            // 115 × 0.82
    carry4_delay: 0.093,        // 114 × 0.82
    io_input_delay: 0.656,      // 800 × 0.82
    io_output_delay: 1.230,     // 1500 × 0.82
    bram_read_delay: 2.012,     // 2454 × 0.82
    dsp_delay: 1.476,           // 1800 × 0.82

    local_wire_delay: 0.025,
    single_delay: 0.074,
    double_delay: 0.115,
    quad_delay: 0.164,
    long_delay: 0.287,
    global_clock_delay: 0.082,

    pip_bel_to_local: 0.029,
    pip_local_to_local: 0.033,
    pip_local_to_single: 0.082,
    pip_single_to_single: 0.066,
    pip_span_to_local: 0.049,
    pip_local_to_double: 0.107,
    pip_double_to_double: 0.090,
    pip_local_to_quad: 0.139,
    pip_long_to_local: 0.066,
    pip_long_to_long: 0.148,
    pip_clock_to_local: 0.033,
    fanout_per_load: 0.007,
};

/// Spartan-7 speed grade -1 (same die as Artix, ~5% slower due to binning)
pub const TIMING_SPARTAN_1: Xc7TimingData = Xc7TimingData {
    lut6_delay: 0.130,          // 124 × 1.05
    dff_clk_to_q: 0.318,       // 303 × 1.05
    dff_setup: 0.061,
    dff_hold: 0.121,
    carry4_delay: 0.120,
    io_input_delay: 0.840,
    io_output_delay: 1.575,
    bram_read_delay: 2.577,
    dsp_delay: 1.890,

    local_wire_delay: 0.032,
    single_delay: 0.095,
    double_delay: 0.147,
    quad_delay: 0.210,
    long_delay: 0.368,
    global_clock_delay: 0.105,

    pip_bel_to_local: 0.037,
    pip_local_to_local: 0.042,
    pip_local_to_single: 0.105,
    pip_single_to_single: 0.084,
    pip_span_to_local: 0.063,
    pip_local_to_double: 0.137,
    pip_double_to_double: 0.116,
    pip_local_to_quad: 0.179,
    pip_long_to_local: 0.084,
    pip_long_to_long: 0.189,
    pip_clock_to_local: 0.042,
    fanout_per_load: 0.008,
};

// ---------------------------------------------------------------------------
// Routing architecture constants (from prjxray-db INT tiles)
// ---------------------------------------------------------------------------

/// Routing channel width per direction (INT_L/INT_R combined)
pub const ROUTING_CHANNELS: (u32, u32) = (60, 60);

/// Wire counts per INT tile per direction
/// From prjxray INT tile analysis: ~60 wires per direction
pub const WIRE_LOCAL_COUNT: u8 = 24;    // IMUX, bounce, bypass wires
pub const WIRE_SINGLE_COUNT: u8 = 8;   // NN1/SS1/EE1/WW1 (span 1 tile)
pub const WIRE_DOUBLE_COUNT: u8 = 8;   // NN2/SS2/EE2/WW2 (span 2 tiles)
pub const WIRE_QUAD_COUNT: u8 = 4;     // NN4/SS4/EE4/WW4 (span 4-6 tiles)
pub const WIRE_LONG_COUNT: u8 = 4;     // LH/LV (span 12 tiles)

// ---------------------------------------------------------------------------
// Clock network (UG472)
// ---------------------------------------------------------------------------

/// Global clock buffers (BUFG)
pub const BUFG_COUNT: u8 = 32;
/// Regional clock buffers (BUFR) per region
pub const BUFR_PER_REGION: u8 = 4;
/// Maximum GCLK frequency (Hz) — Artix-7 speed grade -1
pub const MAX_GCLK_FREQ_ARTIX: f64 = 450.0e6;
/// Maximum GCLK frequency (Hz) — Kintex-7 speed grade -1
pub const MAX_GCLK_FREQ_KINTEX: f64 = 625.0e6;

// ---------------------------------------------------------------------------
// Tile architecture constants (UG474, UG473, UG479)
// ---------------------------------------------------------------------------

/// LUT6s per slice (4 LUT6 + 8 FF per slice — A/B/C/D)
pub const LUTS_PER_SLICE: u8 = 4;
/// FFs per slice (8: 4 primary + 4 secondary FF per slice)
pub const FFS_PER_SLICE: u8 = 8;
/// Slices per CLB (2: SLICEL + SLICEL/SLICEM)
pub const SLICES_PER_CLB: u8 = 2;
/// LUT6s per CLB tile (= 2 slices × 4 LUTs)
pub const LOGIC_LUTS_PER_TILE: u8 = 8;
/// FFs per CLB tile (= 2 slices × 8 FFs)
pub const LOGIC_FFS_PER_TILE: u8 = 16;

/// I/O cells per IOB tile
pub const IO_CELLS_PER_TILE: u8 = 2;

/// BRAM36 size in bits (36 Kb + 4 Kb parity = 36,864 bits)
pub const BRAM36_SIZE_BITS: u32 = 36 * 1024;
/// Supported BRAM data widths
pub const BRAM_WIDTHS: &[u8] = &[1, 2, 4, 9, 18, 36, 72];

/// DSP48E1 multiplier width
pub const DSP_MULT_WIDTH: u8 = 25; // 25×18 multiplier

/// Supported I/O standards (HR banks)
pub const IO_STANDARDS: &[&str] = &[
    "LVCMOS33", "LVCMOS25", "LVCMOS18", "LVCMOS15", "LVCMOS12",
    "SSTL135", "SSTL15", "LVDS_25",
];
/// Supported drive strengths (mA)
pub const IO_DRIVE_STRENGTHS: &[u8] = &[4, 8, 12, 16, 24];
/// Differential pair support
pub const IO_DIFF_PAIRS: bool = true;

// ---------------------------------------------------------------------------
// Synthetic tile grid parameters
// ---------------------------------------------------------------------------

/// BRAM column spacing (one BRAM column every N columns)
pub const BRAM_COLUMN_SPACING: u32 = 10;
/// DSP column spacing
pub const DSP_COLUMN_SPACING: u32 = 14;
