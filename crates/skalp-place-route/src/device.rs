//! FPGA device definitions and programming

use std::collections::HashMap;

/// FPGA device information with detailed architecture
#[derive(Debug, Clone)]
pub struct Device {
    /// Device name
    pub name: String,
    /// Device family
    pub family: DeviceFamily,
    /// Grid dimensions (width, height)
    pub grid_size: (usize, usize),
    /// Logic tile information
    pub logic_tiles: Vec<LogicTile>,
    /// I/O tile information
    pub io_tiles: Vec<IoTile>,
    /// Memory blocks
    pub memory_blocks: Vec<MemoryBlock>,
    /// DSP tiles (optional, for devices with dedicated DSP)
    pub dsp_tiles: Option<Vec<DspTile>>,
    /// Clock resources
    pub clock_resources: ClockResources,
    /// Routing architecture
    pub routing: RoutingArchitecture,
}

/// FPGA device families
#[derive(Debug, Clone, PartialEq)]
pub enum DeviceFamily {
    Ice40,
    Ecp5,
    Vtr,
    OpenFpga,
}

/// Logic tile definition
#[derive(Debug, Clone)]
pub struct LogicTile {
    /// Tile position (x, y)
    pub position: (usize, usize),
    /// Number of LUTs
    pub lut_count: usize,
    /// LUT input size
    pub lut_size: usize,
    /// Number of flip-flops
    pub ff_count: usize,
    /// Carry chain support
    pub has_carry: bool,
    /// Available inputs/outputs
    pub pins: Vec<TilePin>,
}

/// I/O tile definition
#[derive(Debug, Clone)]
pub struct IoTile {
    /// Tile position (x, y)
    pub position: (usize, usize),
    /// I/O standard support
    pub io_standards: Vec<IoStandard>,
    /// Drive strength options
    pub drive_strengths: Vec<u8>,
    /// Differential pair support
    pub diff_pairs: bool,
    /// Available pins
    pub pins: Vec<String>,
}

/// Memory block definition
#[derive(Debug, Clone)]
pub struct MemoryBlock {
    /// Block position (x, y)
    pub position: (usize, usize),
    /// Memory type
    pub memory_type: MemoryType,
    /// Size in bits
    pub size_bits: usize,
    /// Data width options
    pub data_widths: Vec<usize>,
}

/// DSP tile definition (for devices with dedicated DSP blocks)
#[derive(Debug, Clone)]
pub struct DspTile {
    /// Tile position (x, y)
    pub position: (usize, usize),
    /// Multiplier size (width_a, width_b)
    pub multiplier_size: (usize, usize),
    /// Has accumulator
    pub has_accumulator: bool,
    /// Has pre-adder
    pub has_pre_adder: bool,
}

/// Clock resources for the device
#[derive(Debug, Clone)]
pub struct ClockResources {
    /// Global clock networks
    pub global_clocks: usize,
    /// PLL resources
    pub plls: usize,
    /// DLL resources
    pub dlls: usize,
    /// Clock domains
    pub clock_domains: Vec<ClockDomain>,
}

/// Routing architecture
#[derive(Debug, Clone)]
pub struct RoutingArchitecture {
    /// Routing channels (horizontal, vertical)
    pub channels: (usize, usize),
    /// Switch box pattern
    pub switch_pattern: SwitchPattern,
    /// Wire segments
    pub wire_segments: Vec<WireSegment>,
    /// Connection boxes
    pub connection_boxes: Vec<ConnectionBox>,
}

/// Tile pin definition
#[derive(Debug, Clone)]
pub struct TilePin {
    /// Pin name
    pub name: String,
    /// Pin direction
    pub direction: PinDirection,
    /// Connected routing tracks
    pub routing_tracks: Vec<usize>,
}

/// I/O standards
#[derive(Debug, Clone)]
pub enum IoStandard {
    Lvcmos33,
    Lvcmos25,
    Lvcmos18,
    Lvcmos15,
    Lvcmos12,
    Lvds,
    Sstl18,
}

/// Memory types
#[derive(Debug, Clone)]
pub enum MemoryType {
    BlockRam,
    DistributedRam,
    Fifo,
    Rom,
}

/// Clock domain definition
#[derive(Debug, Clone)]
pub struct ClockDomain {
    /// Domain ID
    pub id: usize,
    /// Coverage area (tiles)
    pub coverage: Vec<(usize, usize)>,
    /// Maximum frequency
    pub max_frequency: f64,
}

/// Switch box patterns
#[derive(Debug, Clone, PartialEq)]
pub enum SwitchPattern {
    Wilton,
    Universal,
    Subset,
}

/// Wire segment definition
#[derive(Debug, Clone)]
pub struct WireSegment {
    /// Segment length
    pub length: usize,
    /// Frequency (how often it appears)
    pub frequency: f64,
    /// Resistance per unit
    pub resistance: f64,
    /// Capacitance per unit
    pub capacitance: f64,
}

/// Connection box definition
#[derive(Debug, Clone)]
pub struct ConnectionBox {
    /// Connected routing tracks
    pub tracks: Vec<usize>,
    /// Flexibility (fraction of pins connected)
    pub flexibility: f64,
}

/// Pin directions
#[derive(Debug, Clone)]
pub enum PinDirection {
    Input,
    Output,
    Bidirectional,
}

impl Device {
    /// Create ECP5 LFE5U-25F device with complete architecture
    pub fn ecp5_lfe5u_25f() -> Self {
        let grid_size = (84, 84); // 84x84 tile grid for LFE5U-25F

        // Create logic tiles (LUT4 + FF + additional features)
        let mut logic_tiles = Vec::new();
        for x in 1..83 {
            for y in 1..83 {
                // Skip special tiles (DSP, RAM, etc.)
                if Self::is_logic_tile_ecp5(x, y, grid_size) {
                    logic_tiles.push(LogicTile {
                        position: (x, y),
                        lut_count: 8, // 8 LUT4s per slice, similar to iCE40 but different architecture
                        lut_size: 4,  // 4-input LUTs
                        ff_count: 8,  // 8 flip-flops per slice
                        has_carry: true,
                        pins: vec![
                            TilePin {
                                name: "A0".to_string(),
                                direction: PinDirection::Input,
                                routing_tracks: vec![0, 1, 2, 3],
                            },
                            TilePin {
                                name: "B0".to_string(),
                                direction: PinDirection::Input,
                                routing_tracks: vec![4, 5, 6, 7],
                            },
                            TilePin {
                                name: "C0".to_string(),
                                direction: PinDirection::Input,
                                routing_tracks: vec![8, 9, 10, 11],
                            },
                            TilePin {
                                name: "D0".to_string(),
                                direction: PinDirection::Input,
                                routing_tracks: vec![12, 13, 14, 15],
                            },
                            TilePin {
                                name: "F0".to_string(),
                                direction: PinDirection::Output,
                                routing_tracks: vec![16, 17, 18, 19],
                            },
                            TilePin {
                                name: "Q0".to_string(),
                                direction: PinDirection::Output,
                                routing_tracks: vec![20, 21, 22, 23],
                            },
                        ],
                    });
                }
            }
        }

        // Create I/O tiles around perimeter
        let mut io_tiles = Vec::new();
        for x in 0..grid_size.0 {
            for y in 0..grid_size.1 {
                if x == 0 || x == grid_size.0 - 1 || y == 0 || y == grid_size.1 - 1 {
                    io_tiles.push(IoTile {
                        position: (x, y),
                        io_standards: vec![
                            IoStandard::Lvcmos33,
                            IoStandard::Lvcmos25,
                            IoStandard::Lvcmos18,
                            IoStandard::Lvcmos15,
                            IoStandard::Lvcmos12,
                            IoStandard::Lvds,
                            IoStandard::Sstl18,
                        ],
                        drive_strengths: vec![2, 4, 8, 12, 16, 20, 24],
                        diff_pairs: true, // ECP5 supports differential pairs
                        pins: vec![format!("IO_{x}_{y}")],
                    });
                }
            }
        }

        // Create memory blocks (EBR - Embedded Block RAM)
        let mut memory_blocks = Vec::new();
        // ECP5 LFE5U-25F has 56 EBRs arranged in columns
        let ebr_columns = vec![10, 20, 30, 40, 50, 60, 70];
        for col in ebr_columns {
            for row in (10..75).step_by(8) {
                // EBRs every 8 rows
                memory_blocks.push(MemoryBlock {
                    position: (col, row),
                    memory_type: MemoryType::BlockRam,
                    size_bits: 18432,                      // 18Kbit EBR
                    data_widths: vec![1, 2, 4, 9, 18, 36], // ECP5 supports various widths
                });
            }
        }

        // Create DSP tiles (MULT18X18D)
        let mut dsp_tiles = Vec::new();
        let dsp_columns = vec![15, 35, 55, 75];
        for col in dsp_columns {
            for row in (5..80).step_by(10) {
                dsp_tiles.push(DspTile {
                    position: (col, row),
                    multiplier_size: (18, 18), // 18x18 multipliers
                    has_accumulator: true,
                    has_pre_adder: true,
                });
            }
        }

        Self {
            name: "ECP5-LFE5U-25F".to_string(),
            family: DeviceFamily::Ecp5,
            grid_size,
            logic_tiles,
            io_tiles,
            memory_blocks,
            dsp_tiles: Some(dsp_tiles),
            clock_resources: ClockResources {
                global_clocks: 16, // More global clocks than iCE40
                plls: 2,
                dlls: 2, // ECP5 has DLLs
                clock_domains: vec![ClockDomain {
                    id: 0,
                    coverage: (0..grid_size.0)
                        .flat_map(|x| (0..grid_size.1).map(move |y| (x, y)))
                        .collect(),
                    max_frequency: 400.0, // Higher frequency capability
                }],
            },
            routing: RoutingArchitecture {
                channels: (24, 24), // More routing tracks than iCE40
                switch_pattern: SwitchPattern::Universal,
                wire_segments: vec![
                    WireSegment {
                        length: 1,
                        frequency: 0.4,
                        resistance: 80.0,
                        capacitance: 8.0,
                    },
                    WireSegment {
                        length: 2,
                        frequency: 0.3,
                        resistance: 160.0,
                        capacitance: 16.0,
                    },
                    WireSegment {
                        length: 4,
                        frequency: 0.2,
                        resistance: 320.0,
                        capacitance: 32.0,
                    },
                    WireSegment {
                        length: 12,
                        frequency: 0.1,
                        resistance: 960.0,
                        capacitance: 96.0,
                    },
                ],
                connection_boxes: vec![ConnectionBox {
                    tracks: (0..24).collect(),
                    flexibility: 0.6,
                }],
            },
        }
    }

    /// Create ECP5 LFE5U-85F device (larger variant)
    pub fn ecp5_lfe5u_85f() -> Self {
        let grid_size = (102, 102); // Larger grid for 85F

        let mut logic_tiles = Vec::new();
        for x in 1..101 {
            for y in 1..101 {
                if Self::is_logic_tile_ecp5(x, y, grid_size) {
                    logic_tiles.push(LogicTile {
                        position: (x, y),
                        lut_count: 8,
                        lut_size: 4,
                        ff_count: 8,
                        has_carry: true,
                        pins: vec![
                            TilePin {
                                name: "A0".to_string(),
                                direction: PinDirection::Input,
                                routing_tracks: vec![0, 1, 2, 3],
                            },
                            TilePin {
                                name: "F0".to_string(),
                                direction: PinDirection::Output,
                                routing_tracks: vec![16, 17, 18, 19],
                            },
                        ],
                    });
                }
            }
        }

        let mut io_tiles = Vec::new();
        for x in 0..grid_size.0 {
            for y in 0..grid_size.1 {
                if x == 0 || x == grid_size.0 - 1 || y == 0 || y == grid_size.1 - 1 {
                    io_tiles.push(IoTile {
                        position: (x, y),
                        io_standards: vec![
                            IoStandard::Lvcmos33,
                            IoStandard::Lvcmos25,
                            IoStandard::Lvcmos18,
                            IoStandard::Lvds,
                            IoStandard::Sstl18,
                        ],
                        drive_strengths: vec![2, 4, 8, 12, 16, 20, 24],
                        diff_pairs: true,
                        pins: vec![format!("IO_{x}_{y}")],
                    });
                }
            }
        }

        // More EBRs for larger device
        let mut memory_blocks = Vec::new();
        let ebr_columns = vec![8, 16, 24, 32, 40, 48, 56, 64, 72, 80, 88, 96];
        for col in ebr_columns {
            for row in (8..95).step_by(6) {
                memory_blocks.push(MemoryBlock {
                    position: (col, row),
                    memory_type: MemoryType::BlockRam,
                    size_bits: 18432,
                    data_widths: vec![1, 2, 4, 9, 18, 36],
                });
            }
        }

        Self {
            name: "ECP5-LFE5U-85F".to_string(),
            family: DeviceFamily::Ecp5,
            grid_size,
            logic_tiles,
            io_tiles,
            memory_blocks,
            dsp_tiles: None, // Simplified for now
            clock_resources: ClockResources {
                global_clocks: 16,
                plls: 4, // More PLLs
                dlls: 4,
                clock_domains: vec![ClockDomain {
                    id: 0,
                    coverage: (0..grid_size.0)
                        .flat_map(|x| (0..grid_size.1).map(move |y| (x, y)))
                        .collect(),
                    max_frequency: 400.0,
                }],
            },
            routing: RoutingArchitecture {
                channels: (28, 28), // Even more routing for larger device
                switch_pattern: SwitchPattern::Universal,
                wire_segments: vec![
                    WireSegment {
                        length: 1,
                        frequency: 0.4,
                        resistance: 80.0,
                        capacitance: 8.0,
                    },
                    WireSegment {
                        length: 2,
                        frequency: 0.3,
                        resistance: 160.0,
                        capacitance: 16.0,
                    },
                    WireSegment {
                        length: 4,
                        frequency: 0.2,
                        resistance: 320.0,
                        capacitance: 32.0,
                    },
                    WireSegment {
                        length: 12,
                        frequency: 0.1,
                        resistance: 960.0,
                        capacitance: 96.0,
                    },
                ],
                connection_boxes: vec![ConnectionBox {
                    tracks: (0..28).collect(),
                    flexibility: 0.65,
                }],
            },
        }
    }

    /// Helper to determine if a position should have a logic tile for ECP5
    fn is_logic_tile_ecp5(x: usize, y: usize, grid_size: (usize, usize)) -> bool {
        // Skip edges (I/O), and specific tiles for EBR/DSP
        if x == 0 || x >= grid_size.0 - 1 || y == 0 || y >= grid_size.1 - 1 {
            return false;
        }

        // Skip EBR columns (simplified pattern)
        let ebr_columns = vec![10, 20, 30, 40, 50, 60, 70];
        if ebr_columns.contains(&x) && y % 8 < 4 {
            return false;
        }

        // Skip DSP columns
        let dsp_columns = vec![15, 35, 55, 75];
        if dsp_columns.contains(&x) && y % 10 < 2 {
            return false;
        }

        true
    }

    /// Create ICE40 HX8K device with complete architecture
    pub fn ice40_hx8k() -> Self {
        let grid_size = (33, 33); // 33x33 tile grid for HX8K

        // Create logic tiles (LUT4 + FF)
        let mut logic_tiles = Vec::new();
        for x in 1..32 {
            for y in 1..32 {
                // Skip RAM and I/O tiles
                if Self::is_logic_tile_ice40(x, y, grid_size) {
                    logic_tiles.push(LogicTile {
                        position: (x, y),
                        lut_count: 8, // 8 LUT4s per tile
                        lut_size: 4,  // 4-input LUTs
                        ff_count: 8,  // 8 flip-flops per tile
                        has_carry: true,
                        pins: vec![
                            TilePin {
                                name: "I0".to_string(),
                                direction: PinDirection::Input,
                                routing_tracks: vec![0, 1, 2],
                            },
                            TilePin {
                                name: "I1".to_string(),
                                direction: PinDirection::Input,
                                routing_tracks: vec![3, 4, 5],
                            },
                            TilePin {
                                name: "I2".to_string(),
                                direction: PinDirection::Input,
                                routing_tracks: vec![6, 7, 8],
                            },
                            TilePin {
                                name: "I3".to_string(),
                                direction: PinDirection::Input,
                                routing_tracks: vec![9, 10, 11],
                            },
                            TilePin {
                                name: "O".to_string(),
                                direction: PinDirection::Output,
                                routing_tracks: vec![12, 13, 14],
                            },
                        ],
                    });
                }
            }
        }

        // Create I/O tiles around perimeter
        let mut io_tiles = Vec::new();
        for x in 0..grid_size.0 {
            for y in 0..grid_size.1 {
                if x == 0 || x == grid_size.0 - 1 || y == 0 || y == grid_size.1 - 1 {
                    io_tiles.push(IoTile {
                        position: (x, y),
                        io_standards: vec![IoStandard::Lvcmos33, IoStandard::Lvcmos25],
                        drive_strengths: vec![2, 4, 8, 12, 16],
                        diff_pairs: false,
                        pins: vec![format!("IO_{x}_{y}")],
                    });
                }
            }
        }

        // Create memory blocks (Block RAMs)
        let mut memory_blocks = Vec::new();
        // HX8K has 32 Block RAMs in specific locations
        let bram_locations = vec![(8, 16), (16, 8), (24, 16), (16, 24)];
        for (x, y) in bram_locations {
            memory_blocks.push(MemoryBlock {
                position: (x, y),
                memory_type: MemoryType::BlockRam,
                size_bits: 4096, // 4Kbit Block RAM
                data_widths: vec![1, 2, 4, 8, 16],
            });
        }

        Self {
            name: "iCE40HX8K".to_string(),
            family: DeviceFamily::Ice40,
            grid_size,
            logic_tiles,
            io_tiles,
            memory_blocks,
            dsp_tiles: None, // iCE40 doesn't have dedicated DSP tiles
            clock_resources: ClockResources {
                global_clocks: 8,
                plls: 2,
                dlls: 0,
                clock_domains: vec![ClockDomain {
                    id: 0,
                    coverage: (0..grid_size.0)
                        .flat_map(|x| (0..grid_size.1).map(move |y| (x, y)))
                        .collect(),
                    max_frequency: 450.0, // MHz
                }],
            },
            routing: RoutingArchitecture {
                channels: (12, 12), // 12 horizontal, 12 vertical tracks per channel
                switch_pattern: SwitchPattern::Wilton,
                wire_segments: vec![
                    WireSegment {
                        length: 1,
                        frequency: 0.6,
                        resistance: 100.0,
                        capacitance: 10.0,
                    },
                    WireSegment {
                        length: 4,
                        frequency: 0.3,
                        resistance: 400.0,
                        capacitance: 40.0,
                    },
                    WireSegment {
                        length: 12,
                        frequency: 0.1,
                        resistance: 1200.0,
                        capacitance: 120.0,
                    },
                ],
                connection_boxes: vec![ConnectionBox {
                    tracks: (0..12).collect(),
                    flexibility: 0.5,
                }],
            },
        }
    }

    /// Create ICE40 HX1K device
    pub fn ice40_hx1k() -> Self {
        let grid_size = (17, 17); // Smaller grid for HX1K

        let mut logic_tiles = Vec::new();
        for x in 1..16 {
            for y in 1..16 {
                if Self::is_logic_tile_ice40(x, y, grid_size) {
                    logic_tiles.push(LogicTile {
                        position: (x, y),
                        lut_count: 8,
                        lut_size: 4,
                        ff_count: 8,
                        has_carry: true,
                        pins: vec![
                            TilePin {
                                name: "I0".to_string(),
                                direction: PinDirection::Input,
                                routing_tracks: vec![0, 1, 2],
                            },
                            TilePin {
                                name: "O".to_string(),
                                direction: PinDirection::Output,
                                routing_tracks: vec![3, 4, 5],
                            },
                        ],
                    });
                }
            }
        }

        let mut io_tiles = Vec::new();
        for x in 0..grid_size.0 {
            for y in 0..grid_size.1 {
                if x == 0 || x == grid_size.0 - 1 || y == 0 || y == grid_size.1 - 1 {
                    io_tiles.push(IoTile {
                        position: (x, y),
                        io_standards: vec![IoStandard::Lvcmos33],
                        drive_strengths: vec![2, 4, 8],
                        diff_pairs: false,
                        pins: vec![format!("IO_{x}_{y}")],
                    });
                }
            }
        }

        Self {
            name: "iCE40HX1K".to_string(),
            family: DeviceFamily::Ice40,
            grid_size,
            logic_tiles,
            io_tiles,
            memory_blocks: vec![], // No Block RAMs in HX1K
            dsp_tiles: None,       // iCE40 doesn't have dedicated DSP tiles
            clock_resources: ClockResources {
                global_clocks: 8,
                plls: 1,
                dlls: 0,
                clock_domains: vec![ClockDomain {
                    id: 0,
                    coverage: (0..grid_size.0)
                        .flat_map(|x| (0..grid_size.1).map(move |y| (x, y)))
                        .collect(),
                    max_frequency: 450.0,
                }],
            },
            routing: RoutingArchitecture {
                channels: (8, 8),
                switch_pattern: SwitchPattern::Wilton,
                wire_segments: vec![
                    WireSegment {
                        length: 1,
                        frequency: 0.7,
                        resistance: 100.0,
                        capacitance: 10.0,
                    },
                    WireSegment {
                        length: 4,
                        frequency: 0.3,
                        resistance: 400.0,
                        capacitance: 40.0,
                    },
                ],
                connection_boxes: vec![ConnectionBox {
                    tracks: (0..8).collect(),
                    flexibility: 0.4,
                }],
            },
        }
    }

    /// Helper to determine if a position should have a logic tile for iCE40
    fn is_logic_tile_ice40(x: usize, y: usize, grid_size: (usize, usize)) -> bool {
        // Skip edges (I/O), and some specific tiles for Block RAM/DSP
        x > 0 && x < grid_size.0 - 1 && y > 0 && y < grid_size.1 - 1
    }

    /// Create VTR academic FPGA k6_frac_N10_mem32K_40nm device
    /// Based on the VTR benchmark suite standard architecture
    pub fn vtr_k6_frac_n10() -> Self {
        let grid_size = (82, 82); // 82x82 tile grid for large benchmark designs

        // Create logic tiles with fracturable 6-input LUTs
        let mut logic_tiles = Vec::new();
        for x in 1..81 {
            for y in 1..81 {
                if Self::is_logic_tile_vtr(x, y, grid_size) {
                    logic_tiles.push(LogicTile {
                        position: (x, y),
                        lut_count: 10, // 10 LUTs per CLB (Configurable Logic Block)
                        lut_size: 6,   // 6-input fracturable LUTs (can be split to 5-LUT + 4-LUT)
                        ff_count: 20,  // 20 flip-flops per CLB (2 per LUT)
                        has_carry: true,
                        pins: vec![
                            // VTR CLB has 40 input pins and 10 output pins
                            TilePin {
                                name: "I0".to_string(),
                                direction: PinDirection::Input,
                                routing_tracks: vec![0, 1, 2, 3],
                            },
                            TilePin {
                                name: "I1".to_string(),
                                direction: PinDirection::Input,
                                routing_tracks: vec![4, 5, 6, 7],
                            },
                            TilePin {
                                name: "I2".to_string(),
                                direction: PinDirection::Input,
                                routing_tracks: vec![8, 9, 10, 11],
                            },
                            TilePin {
                                name: "I3".to_string(),
                                direction: PinDirection::Input,
                                routing_tracks: vec![12, 13, 14, 15],
                            },
                            TilePin {
                                name: "I4".to_string(),
                                direction: PinDirection::Input,
                                routing_tracks: vec![16, 17, 18, 19],
                            },
                            TilePin {
                                name: "I5".to_string(),
                                direction: PinDirection::Input,
                                routing_tracks: vec![20, 21, 22, 23],
                            },
                            TilePin {
                                name: "O0".to_string(),
                                direction: PinDirection::Output,
                                routing_tracks: vec![24, 25, 26, 27],
                            },
                            TilePin {
                                name: "O1".to_string(),
                                direction: PinDirection::Output,
                                routing_tracks: vec![28, 29, 30, 31],
                            },
                            TilePin {
                                name: "O2".to_string(),
                                direction: PinDirection::Output,
                                routing_tracks: vec![32, 33, 34, 35],
                            },
                            TilePin {
                                name: "carry_in".to_string(),
                                direction: PinDirection::Input,
                                routing_tracks: vec![36],
                            },
                            TilePin {
                                name: "carry_out".to_string(),
                                direction: PinDirection::Output,
                                routing_tracks: vec![37],
                            },
                        ],
                    });
                }
            }
        }

        // Create I/O tiles around the perimeter
        let mut io_tiles = Vec::new();
        for x in 0..grid_size.0 {
            for y in 0..grid_size.1 {
                if x == 0 || x == grid_size.0 - 1 || y == 0 || y == grid_size.1 - 1 {
                    io_tiles.push(IoTile {
                        position: (x, y),
                        io_standards: vec![
                            IoStandard::Lvcmos33,
                            IoStandard::Lvcmos25,
                            IoStandard::Lvcmos18,
                        ],
                        drive_strengths: vec![8, 12, 16],
                        diff_pairs: true,
                        pins: vec!["pad".to_string(), "outpad".to_string(), "inpad".to_string()],
                    });
                }
            }
        }

        // Create memory blocks (32Kb BRAM)
        let mut memory_blocks = Vec::new();
        let bram_columns = vec![20, 41, 62]; // Distributed BRAM columns
        for col in bram_columns {
            for row in (10..71).step_by(8) {
                memory_blocks.push(MemoryBlock {
                    position: (col, row),
                    size_bits: 32768, // 32Kb memory blocks
                    data_widths: vec![8, 16, 32],
                    memory_type: MemoryType::BlockRam,
                });
            }
        }

        // Create DSP blocks
        let dsp_tiles = Some({
            let mut dsps = Vec::new();
            let dsp_columns = vec![25, 56]; // Two DSP columns
            for col in dsp_columns {
                for row in (5..76).step_by(10) {
                    dsps.push(DspTile {
                        position: (col, row),
                        multiplier_size: (18, 25), // 18x25 multipliers for VTR
                        has_accumulator: true,
                        has_pre_adder: true,
                    });
                }
            }
            dsps
        });

        Self {
            family: DeviceFamily::Vtr,
            name: "k6_frac_N10_mem32K_40nm".to_string(),
            grid_size,
            logic_tiles,
            io_tiles,
            memory_blocks,
            dsp_tiles,
            clock_resources: ClockResources {
                global_clocks: 8,
                plls: 4,
                dlls: 2,
                clock_domains: vec![ClockDomain {
                    id: 0,
                    coverage: vec![],
                    max_frequency: 400.0e6,
                }],
            },
            routing: RoutingArchitecture {
                channels: (40, 40), // 40 tracks per channel
                switch_pattern: SwitchPattern::Wilton,
                wire_segments: vec![
                    WireSegment {
                        length: 1,
                        frequency: 0.8,
                        resistance: 10.0,
                        capacitance: 1.0,
                    },
                    WireSegment {
                        length: 4,
                        frequency: 0.15,
                        resistance: 20.0,
                        capacitance: 2.0,
                    },
                    WireSegment {
                        length: 16,
                        frequency: 0.05,
                        resistance: 40.0,
                        capacitance: 4.0,
                    },
                ], // Mix of short and long segments
                connection_boxes: vec![ConnectionBox {
                    tracks: (0..40).collect(),
                    flexibility: 0.5,
                }],
            },
        }
    }

    /// Create OpenFPGA k4_N8 academic device
    /// Standard OpenFPGA benchmark architecture
    pub fn openfpga_k4_n8() -> Self {
        let grid_size = (12, 12); // Smaller grid for academic benchmarks

        // Create logic tiles with 4-input LUTs
        let mut logic_tiles = Vec::new();
        for x in 1..11 {
            for y in 1..11 {
                logic_tiles.push(LogicTile {
                    position: (x, y),
                    lut_count: 8,     // 8 LUTs per CLB (N=8)
                    lut_size: 4,      // 4-input LUTs
                    ff_count: 8,      // 8 flip-flops per CLB
                    has_carry: false, // Simplified for academic use
                    pins: vec![
                        TilePin {
                            name: "I0".to_string(),
                            direction: PinDirection::Input,
                            routing_tracks: vec![0, 1],
                        },
                        TilePin {
                            name: "I1".to_string(),
                            direction: PinDirection::Input,
                            routing_tracks: vec![2, 3],
                        },
                        TilePin {
                            name: "I2".to_string(),
                            direction: PinDirection::Input,
                            routing_tracks: vec![4, 5],
                        },
                        TilePin {
                            name: "I3".to_string(),
                            direction: PinDirection::Input,
                            routing_tracks: vec![6, 7],
                        },
                        TilePin {
                            name: "O0".to_string(),
                            direction: PinDirection::Output,
                            routing_tracks: vec![8, 9],
                        },
                        TilePin {
                            name: "O1".to_string(),
                            direction: PinDirection::Output,
                            routing_tracks: vec![10, 11],
                        },
                    ],
                });
            }
        }

        // Create I/O tiles around the perimeter
        let mut io_tiles = Vec::new();
        for x in 0..grid_size.0 {
            for y in 0..grid_size.1 {
                if x == 0 || x == grid_size.0 - 1 || y == 0 || y == grid_size.1 - 1 {
                    io_tiles.push(IoTile {
                        position: (x, y),
                        io_standards: vec![IoStandard::Lvcmos33],
                        drive_strengths: vec![8],
                        diff_pairs: false,
                        pins: vec!["pad".to_string()],
                    });
                }
            }
        }

        // No memory blocks or DSP for simplified k4_N8 architecture
        let memory_blocks = Vec::new();
        let dsp_tiles = None;

        Self {
            family: DeviceFamily::OpenFpga,
            name: "k4_N8".to_string(),
            grid_size,
            logic_tiles,
            io_tiles,
            memory_blocks,
            dsp_tiles,
            clock_resources: ClockResources {
                global_clocks: 2,
                plls: 1,
                dlls: 0,
                clock_domains: vec![ClockDomain {
                    id: 0,
                    coverage: vec![],
                    max_frequency: 250.0e6,
                }],
            },
            routing: RoutingArchitecture {
                channels: (12, 12), // 12 tracks per channel
                switch_pattern: SwitchPattern::Universal,
                wire_segments: vec![
                    WireSegment {
                        length: 1,
                        frequency: 0.7,
                        resistance: 8.0,
                        capacitance: 0.8,
                    },
                    WireSegment {
                        length: 4,
                        frequency: 0.3,
                        resistance: 16.0,
                        capacitance: 1.6,
                    },
                ], // Mix of short and medium segments
                connection_boxes: vec![ConnectionBox {
                    tracks: (0..12).collect(),
                    flexibility: 0.8,
                }],
            },
        }
    }

    /// Create OpenFPGA k6_frac_N10 device with fracturable LUTs
    pub fn openfpga_k6_frac_n10() -> Self {
        let grid_size = (40, 40); // Medium-sized grid

        // Create logic tiles with fracturable 6-input LUTs
        let mut logic_tiles = Vec::new();
        for x in 1..39 {
            for y in 1..39 {
                if Self::is_logic_tile_openfpga(x, y, grid_size) {
                    logic_tiles.push(LogicTile {
                        position: (x, y),
                        lut_count: 10, // 10 LUTs per CLB (N=10)
                        lut_size: 6,   // 6-input fracturable LUTs
                        ff_count: 20,  // 20 flip-flops per CLB (2 per LUT)
                        has_carry: true,
                        pins: vec![
                            TilePin {
                                name: "I0".to_string(),
                                direction: PinDirection::Input,
                                routing_tracks: vec![0, 1, 2],
                            },
                            TilePin {
                                name: "I1".to_string(),
                                direction: PinDirection::Input,
                                routing_tracks: vec![3, 4, 5],
                            },
                            TilePin {
                                name: "I2".to_string(),
                                direction: PinDirection::Input,
                                routing_tracks: vec![6, 7, 8],
                            },
                            TilePin {
                                name: "I3".to_string(),
                                direction: PinDirection::Input,
                                routing_tracks: vec![9, 10, 11],
                            },
                            TilePin {
                                name: "I4".to_string(),
                                direction: PinDirection::Input,
                                routing_tracks: vec![12, 13, 14],
                            },
                            TilePin {
                                name: "I5".to_string(),
                                direction: PinDirection::Input,
                                routing_tracks: vec![15, 16, 17],
                            },
                            TilePin {
                                name: "O0".to_string(),
                                direction: PinDirection::Output,
                                routing_tracks: vec![18, 19, 20],
                            },
                            TilePin {
                                name: "O1".to_string(),
                                direction: PinDirection::Output,
                                routing_tracks: vec![21, 22, 23],
                            },
                            TilePin {
                                name: "carry_in".to_string(),
                                direction: PinDirection::Input,
                                routing_tracks: vec![24],
                            },
                            TilePin {
                                name: "carry_out".to_string(),
                                direction: PinDirection::Output,
                                routing_tracks: vec![25],
                            },
                        ],
                    });
                }
            }
        }

        // Create I/O tiles around the perimeter
        let mut io_tiles = Vec::new();
        for x in 0..grid_size.0 {
            for y in 0..grid_size.1 {
                if x == 0 || x == grid_size.0 - 1 || y == 0 || y == grid_size.1 - 1 {
                    io_tiles.push(IoTile {
                        position: (x, y),
                        io_standards: vec![IoStandard::Lvcmos33, IoStandard::Lvcmos25],
                        drive_strengths: vec![8, 12],
                        diff_pairs: true,
                        pins: vec!["pad".to_string(), "outpad".to_string(), "inpad".to_string()],
                    });
                }
            }
        }

        // Add some memory blocks for medium-complexity designs
        let mut memory_blocks = Vec::new();
        let bram_columns = vec![15, 25]; // Two BRAM columns
        for col in bram_columns {
            for row in (5..35).step_by(6) {
                memory_blocks.push(MemoryBlock {
                    position: (col, row),
                    size_bits: 16384, // 16Kb memory blocks
                    data_widths: vec![16, 32],
                    memory_type: MemoryType::BlockRam,
                });
            }
        }

        Self {
            family: DeviceFamily::OpenFpga,
            name: "k6_frac_N10".to_string(),
            grid_size,
            logic_tiles,
            io_tiles,
            memory_blocks,
            dsp_tiles: None, // No DSP for this architecture variant
            clock_resources: ClockResources {
                global_clocks: 4,
                plls: 2,
                dlls: 1,
                clock_domains: vec![ClockDomain {
                    id: 0,
                    coverage: vec![],
                    max_frequency: 300.0e6,
                }],
            },
            routing: RoutingArchitecture {
                channels: (24, 24), // 24 tracks per channel
                switch_pattern: SwitchPattern::Wilton,
                wire_segments: vec![
                    WireSegment {
                        length: 1,
                        frequency: 0.6,
                        resistance: 9.0,
                        capacitance: 0.9,
                    },
                    WireSegment {
                        length: 4,
                        frequency: 0.3,
                        resistance: 18.0,
                        capacitance: 1.8,
                    },
                    WireSegment {
                        length: 8,
                        frequency: 0.1,
                        resistance: 32.0,
                        capacitance: 3.2,
                    },
                ], // Mix of segment lengths
                connection_boxes: vec![ConnectionBox {
                    tracks: (0..24).collect(),
                    flexibility: 0.6,
                }],
            },
        }
    }

    /// Helper to determine if a position should have a logic tile for VTR
    fn is_logic_tile_vtr(x: usize, y: usize, grid_size: (usize, usize)) -> bool {
        // Skip edges (I/O), and specific tiles for BRAM/DSP
        if x == 0 || x >= grid_size.0 - 1 || y == 0 || y >= grid_size.1 - 1 {
            return false;
        }

        // Skip BRAM columns
        let bram_columns = vec![20, 41, 62];
        if bram_columns.contains(&x) && y % 8 < 4 {
            return false;
        }

        // Skip DSP columns
        let dsp_columns = vec![25, 56];
        if dsp_columns.contains(&x) && y % 10 < 2 {
            return false;
        }

        true
    }

    /// Helper to determine if a position should have a logic tile for OpenFPGA
    fn is_logic_tile_openfpga(x: usize, y: usize, grid_size: (usize, usize)) -> bool {
        // Skip edges (I/O), and specific tiles for BRAM
        if x == 0 || x >= grid_size.0 - 1 || y == 0 || y >= grid_size.1 - 1 {
            return false;
        }

        // Skip BRAM columns
        let bram_columns = vec![15, 25];
        if bram_columns.contains(&x) && y % 6 < 3 {
            return false;
        }

        true
    }

    /// Get device statistics
    pub fn stats(&self) -> DeviceStats {
        DeviceStats {
            total_luts: self.logic_tiles.iter().map(|t| t.lut_count).sum(),
            total_ffs: self.logic_tiles.iter().map(|t| t.ff_count).sum(),
            total_ios: self.io_tiles.len(),
            total_brams: self.memory_blocks.len(),
            total_dsps: self.dsp_tiles.as_ref().map(|dsps| dsps.len()).unwrap_or(0),
            grid_area: self.grid_size.0 * self.grid_size.1,
        }
    }
}

/// Device statistics summary
#[derive(Debug)]
pub struct DeviceStats {
    pub total_luts: usize,
    pub total_ffs: usize,
    pub total_ios: usize,
    pub total_brams: usize,
    pub total_dsps: usize,
    pub grid_area: usize,
}

/// Device programmer
pub struct DeviceProgrammer {
    /// Programming interface
    interface: ProgrammingInterface,
}

/// Programming interface types
pub enum ProgrammingInterface {
    /// SPI programming
    Spi,
    /// JTAG programming
    Jtag,
    /// USB programming
    Usb,
}

impl DeviceProgrammer {
    /// Create a new device programmer
    pub fn new(interface: ProgrammingInterface) -> Self {
        Self { interface }
    }

    /// Program device with bitstream
    pub fn program(
        &self,
        _device: &Device,
        _bitstream: &super::bitstream::Bitstream,
    ) -> Result<(), ProgrammingError> {
        // Simplified programming - would call actual programmer tools
        Ok(())
    }
}

/// Programming errors
#[derive(Debug, thiserror::Error)]
pub enum ProgrammingError {
    #[error("Programming failed: {0}")]
    Failed(String),
    #[error("Device not found")]
    DeviceNotFound,
}
