//! Technology Library
//!
//! Defines library cells with FIT rates and decomposition rules for
//! mapping word-level operations to gate-level primitives.
//!
//! # Library Format
//!
//! Libraries can be loaded from TOML files or created programmatically.
//! The format supports:
//! - Cell definitions with FIT rates and failure modes
//! - Decomposition rules for word-level operations
//! - Built-in libraries for common technologies (generic_asic, fpga)
//!
//! # Example TOML
//!
//! ```toml
//! [library]
//! name = "generic_asic"
//! process_node = 7
//!
//! [[cells]]
//! name = "NAND2_X1"
//! function = "nand2"
//! fit = 0.1
//! inputs = ["a", "b"]
//! outputs = ["y"]
//! ```

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fs;
use std::path::Path;

use crate::gate_netlist::FaultType;
use crate::lir::LirOp;

// ============================================================================
// Library Cell Types
// ============================================================================

/// A cell in the technology library
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LibraryCell {
    /// Cell name (e.g., "NAND2_X1", "FA_X1")
    pub name: String,
    /// Cell function
    pub function: CellFunction,
    /// Base FIT rate
    pub fit: f64,
    /// Area in square micrometers (optional)
    pub area: Option<f64>,
    /// Transistor count (optional)
    pub transistor_count: Option<u32>,
    /// Input pin names
    pub inputs: Vec<String>,
    /// Output pin names
    pub outputs: Vec<String>,
    /// Failure modes with FIT breakdown
    pub failure_modes: Vec<LibraryFailureMode>,
    // === Drive Strength Characteristics ===
    /// Drive strength multiplier (1 = X1, 2 = X2, 4 = X4, etc.)
    #[serde(default = "default_drive_strength")]
    pub drive_strength: u8,
    /// Maximum output current in microamps (µA)
    #[serde(default)]
    pub max_output_current_ua: Option<u32>,
    /// Output capacitance in femtofarads (fF)
    #[serde(default)]
    pub output_capacitance_ff: Option<u32>,
    /// Input capacitance per pin in femtofarads (fF)
    #[serde(default)]
    pub input_capacitance_ff: Option<u32>,
    /// Maximum fanout (number of standard loads)
    #[serde(default)]
    pub max_fanout: Option<u32>,

    // === Voltage Margin Characteristics (for power domain simulation) ===
    /// Minimum operating voltage in millivolts (mV)
    /// Below this voltage, the cell will fail (output stuck-at-0)
    #[serde(default)]
    pub min_voltage_mv: Option<u16>,
    /// Nominal operating voltage in millivolts (mV)
    #[serde(default)]
    pub nominal_voltage_mv: Option<u16>,
    /// Voltage at which timing starts degrading (mV)
    /// Between min_voltage and timing_margin_voltage, delay increases
    #[serde(default)]
    pub timing_margin_voltage_mv: Option<u16>,
    /// Delay increase coefficient (% delay increase per 100mV drop below timing margin)
    /// e.g., 0.15 means 15% delay increase per 100mV drop
    #[serde(default)]
    pub voltage_delay_coefficient: Option<f64>,
    /// Cell voltage sensitivity ranking (1 = most sensitive, fails first in brownout)
    /// Used for progressive brownout simulation ordering
    #[serde(default)]
    pub voltage_sensitivity: Option<u8>,

    // === Timing Characteristics ===
    /// Timing arcs for combinational cells (input -> output delays)
    #[serde(default)]
    pub timing: Option<CellTiming>,

    /// Setup time in picoseconds (for sequential cells)
    #[serde(default)]
    pub setup_ps: Option<u32>,

    /// Hold time in picoseconds (for sequential cells)
    #[serde(default)]
    pub hold_ps: Option<u32>,

    /// Clock-to-Q delay in picoseconds (for sequential cells)
    #[serde(default)]
    pub clk_to_q_ps: Option<u32>,
}

/// Timing characteristics for a cell
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct CellTiming {
    /// Timing arcs: maps "input_pin->output_pin" to timing arc
    #[serde(default)]
    pub arcs: HashMap<String, TimingArc>,
}

/// A timing arc from input to output
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TimingArc {
    /// Rise delay: base_ps + (slope_ps_per_ff * load_capacitance_ff)
    pub rise_base_ps: u32,
    pub rise_slope_ps_per_ff: f64,

    /// Fall delay: base_ps + (slope_ps_per_ff * load_capacitance_ff)
    pub fall_base_ps: u32,
    pub fall_slope_ps_per_ff: f64,
}

impl TimingArc {
    /// Create a symmetric timing arc (same rise/fall)
    pub fn symmetric(base_ps: u32, slope_ps_per_ff: f64) -> Self {
        Self {
            rise_base_ps: base_ps,
            rise_slope_ps_per_ff: slope_ps_per_ff,
            fall_base_ps: base_ps,
            fall_slope_ps_per_ff: slope_ps_per_ff,
        }
    }

    /// Calculate rise delay for a given load capacitance
    pub fn rise_delay_ps(&self, load_ff: f64) -> f64 {
        self.rise_base_ps as f64 + self.rise_slope_ps_per_ff * load_ff
    }

    /// Calculate fall delay for a given load capacitance
    pub fn fall_delay_ps(&self, load_ff: f64) -> f64 {
        self.fall_base_ps as f64 + self.fall_slope_ps_per_ff * load_ff
    }

    /// Calculate average delay for a given load capacitance
    pub fn avg_delay_ps(&self, load_ff: f64) -> f64 {
        (self.rise_delay_ps(load_ff) + self.fall_delay_ps(load_ff)) / 2.0
    }
}

fn default_drive_strength() -> u8 {
    1
}

impl LibraryCell {
    /// Create a simple combinational cell
    pub fn new_comb(name: &str, function: CellFunction, fit: f64) -> Self {
        let (inputs, outputs) = function.default_pins();
        Self {
            name: name.to_string(),
            function,
            fit,
            area: None,
            transistor_count: None,
            inputs,
            outputs,
            failure_modes: Vec::new(),
            drive_strength: 1,
            max_output_current_ua: None,
            output_capacitance_ff: None,
            input_capacitance_ff: None,
            max_fanout: None,
            min_voltage_mv: None,
            nominal_voltage_mv: None,
            timing_margin_voltage_mv: None,
            voltage_delay_coefficient: None,
            voltage_sensitivity: None,
            timing: None,
            setup_ps: None,
            hold_ps: None,
            clk_to_q_ps: None,
        }
    }

    /// Create a cell with specified drive strength
    pub fn with_drive_strength(mut self, strength: u8) -> Self {
        self.drive_strength = strength;
        // Scale max fanout and current with drive strength
        if let Some(fanout) = self.max_fanout {
            self.max_fanout = Some(fanout * strength as u32);
        }
        if let Some(current) = self.max_output_current_ua {
            self.max_output_current_ua = Some(current * strength as u32);
        }
        self
    }

    /// Set electrical characteristics
    pub fn with_electrical(
        mut self,
        max_current_ua: u32,
        input_cap_ff: u32,
        output_cap_ff: u32,
        max_fanout: u32,
    ) -> Self {
        self.max_output_current_ua = Some(max_current_ua);
        self.input_capacitance_ff = Some(input_cap_ff);
        self.output_capacitance_ff = Some(output_cap_ff);
        self.max_fanout = Some(max_fanout);
        self
    }

    /// Set voltage margin characteristics for power domain simulation
    ///
    /// # Arguments
    /// * `nominal_mv` - Nominal operating voltage (e.g., 1000 for 1.0V)
    /// * `min_mv` - Minimum operating voltage (cell fails below this)
    /// * `timing_margin_mv` - Voltage below which timing degrades
    /// * `delay_coeff` - Delay increase per 100mV drop (e.g., 0.15 = 15%)
    /// * `sensitivity` - Brownout sensitivity (1 = fails first, 10 = most robust)
    pub fn with_voltage_margins(
        mut self,
        nominal_mv: u16,
        min_mv: u16,
        timing_margin_mv: u16,
        delay_coeff: f64,
        sensitivity: u8,
    ) -> Self {
        self.nominal_voltage_mv = Some(nominal_mv);
        self.min_voltage_mv = Some(min_mv);
        self.timing_margin_voltage_mv = Some(timing_margin_mv);
        self.voltage_delay_coefficient = Some(delay_coeff);
        self.voltage_sensitivity = Some(sensitivity);
        self
    }

    /// Check if the cell will fail at a given voltage
    pub fn will_fail_at_voltage(&self, voltage_mv: u16) -> bool {
        if let Some(min) = self.min_voltage_mv {
            voltage_mv < min
        } else {
            // Default: assume cell needs at least 70% of nominal
            let nominal = self.nominal_voltage_mv.unwrap_or(1000);
            voltage_mv < (nominal * 70 / 100)
        }
    }

    /// Calculate delay multiplier at a given voltage
    /// Returns 1.0 at nominal voltage, increases as voltage drops
    pub fn delay_multiplier_at_voltage(&self, voltage_mv: u16) -> f64 {
        let nominal = self.nominal_voltage_mv.unwrap_or(1000);
        let timing_margin = self.timing_margin_voltage_mv.unwrap_or(nominal * 90 / 100);
        let coeff = self.voltage_delay_coefficient.unwrap_or(0.15);

        if voltage_mv >= timing_margin {
            1.0 // No delay increase above timing margin
        } else if voltage_mv < self.min_voltage_mv.unwrap_or(nominal * 70 / 100) {
            f64::INFINITY // Cell fails - infinite delay
        } else {
            // Linear delay increase between timing margin and min voltage
            let drop_mv = (timing_margin - voltage_mv) as f64;
            1.0 + (drop_mv / 100.0) * coeff
        }
    }

    /// Get voltage sensitivity for brownout ordering (lower = more sensitive)
    pub fn get_voltage_sensitivity(&self) -> u8 {
        self.voltage_sensitivity.unwrap_or(5) // Default mid-range sensitivity
    }

    /// Calculate maximum fanout based on load capacitance
    /// Returns number of loads this cell can drive
    pub fn calculate_max_fanout(&self, load_cap_per_pin_ff: u32) -> u32 {
        // If we have explicit max fanout, use it
        if let Some(fanout) = self.max_fanout {
            return fanout;
        }
        // Otherwise estimate from current capability
        // Typical: 4 loads per X1 cell at 10fF per load
        let base_fanout = 4u32;
        base_fanout * self.drive_strength as u32
    }

    /// Create a cell with default stuck-at failure modes
    pub fn with_default_failure_modes(mut self) -> Self {
        self.failure_modes = vec![
            LibraryFailureMode::new("stuck_at_0", self.fit * 0.5, FaultType::StuckAt0),
            LibraryFailureMode::new("stuck_at_1", self.fit * 0.5, FaultType::StuckAt1),
        ];
        self
    }

    /// Calculate derated FIT rate for specific operating conditions
    ///
    /// Applies Arrhenius temperature acceleration, voltage stress,
    /// and process corner factors to the base FIT rate.
    ///
    /// # Arguments
    /// * `conditions` - Target operating conditions
    /// * `ref_temp_c` - Library reference temperature (default 25°C if None)
    /// * `ref_voltage_v` - Library reference voltage (default 1.0V if None)
    pub fn derated_fit(
        &self,
        conditions: &OperatingConditions,
        ref_temp_c: Option<f64>,
        ref_voltage_v: Option<f64>,
    ) -> f64 {
        let ref_temp = ref_temp_c.unwrap_or(25.0);
        let ref_voltage = ref_voltage_v.unwrap_or(1.0);
        let factors = DeratingFactors::calculate(conditions, ref_temp, ref_voltage);
        self.fit * factors.combined_factor
    }

    /// Calculate derated FIT with derating factors struct
    pub fn derated_fit_with_factors(&self, factors: &DeratingFactors) -> f64 {
        self.fit * factors.combined_factor
    }

    /// Calculate derated failure modes for specific operating conditions
    ///
    /// Returns failure modes with FIT rates adjusted for operating conditions.
    pub fn derated_failure_modes(
        &self,
        conditions: &OperatingConditions,
        ref_temp_c: Option<f64>,
        ref_voltage_v: Option<f64>,
    ) -> Vec<LibraryFailureMode> {
        let ref_temp = ref_temp_c.unwrap_or(25.0);
        let ref_voltage = ref_voltage_v.unwrap_or(1.0);
        let factors = DeratingFactors::calculate(conditions, ref_temp, ref_voltage);

        self.failure_modes
            .iter()
            .map(|fm| {
                let mut derated = fm.clone();
                derated.fit *= factors.combined_factor;
                derated
            })
            .collect()
    }
}

/// Cell function type
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum CellFunction {
    // Basic gates
    Inv,
    Nand2,
    Nand3,
    Nand4,
    Nor2,
    Nor3,
    Nor4,
    And2,
    And3,
    And4,
    Or2,
    Or3,
    Or4,
    Xor2,
    Xnor2,
    Buf,
    AndNot, // a & ~b
    OrNot,  // a | ~b

    // Complex gates
    Aoi21, // AND-OR-Invert: ~((a & b) | c)
    Aoi22, // ~((a & b) | (c & d))
    Oai21, // OR-AND-Invert: ~((a | b) & c)
    Oai22, // ~((a | b) & (c | d))

    // Multiplexers
    Mux2,
    Mux4,

    // Arithmetic
    HalfAdder,
    FullAdder,
    Adder(u32), // N-bit adder

    // Sequential
    Dff,
    DffR,  // DFF with reset
    DffE,  // DFF with enable
    DffRE, // DFF with reset and enable
    Latch,

    // Tristate
    Tristate,

    // Power infrastructure
    /// Level shifter: Low-to-High voltage translation
    LevelShifterLH,
    /// Level shifter: High-to-Low voltage translation
    LevelShifterHL,
    /// Isolation cell with AND (clamps to 0)
    IsolationAnd,
    /// Isolation cell with OR (clamps to 1)
    IsolationOr,
    /// Isolation cell with latch (holds last value)
    IsolationLatch,
    /// Retention flip-flop (with balloon latch)
    RetentionDff,
    /// Retention flip-flop with reset
    RetentionDffR,
    /// Power switch header (PMOS, VDD side)
    PowerSwitchHeader,
    /// Power switch footer (NMOS, VSS side)
    PowerSwitchFooter,
    /// Always-on buffer
    AlwaysOnBuf,

    // Tie cells (constant outputs)
    /// Tie-high cell: outputs constant 1
    TieHigh,
    /// Tie-low cell: outputs constant 0
    TieLow,

    // I/O Pads - Digital
    /// Input pad: external → core (with ESD protection)
    InputPad,
    /// Output pad: core → external (with ESD protection)
    OutputPad,
    /// Bidirectional pad: both directions with enable
    BidirPad,
    /// Clock input pad: low-jitter clock input with ESD
    ClockPad,

    // I/O Pads - Power
    /// Power pad: VDD with ESD protection
    PowerPad,
    /// Ground pad: VSS with ESD protection
    GroundPad,

    // I/O Pads - Analog
    /// Analog pad: analog pass-through with ESD
    AnalogPad,

    // I/O Pads - Integrated LDO
    /// Power pad with integrated Low Dropout Regulator
    PowerPadLdo,

    // Custom/vendor-specific
    Custom(String),
}

impl CellFunction {
    /// Get default input/output pin names for this function
    pub fn default_pins(&self) -> (Vec<String>, Vec<String>) {
        match self {
            CellFunction::Inv => (vec!["a".into()], vec!["y".into()]),
            CellFunction::Buf => (vec!["a".into()], vec!["y".into()]),
            CellFunction::Nand2
            | CellFunction::Nor2
            | CellFunction::And2
            | CellFunction::Or2
            | CellFunction::Xor2
            | CellFunction::Xnor2
            | CellFunction::AndNot
            | CellFunction::OrNot => (vec!["a".into(), "b".into()], vec!["y".into()]),
            CellFunction::Nand3 | CellFunction::Nor3 | CellFunction::And3 | CellFunction::Or3 => {
                (vec!["a".into(), "b".into(), "c".into()], vec!["y".into()])
            }
            CellFunction::Nand4 | CellFunction::Nor4 | CellFunction::And4 | CellFunction::Or4 => (
                vec!["a".into(), "b".into(), "c".into(), "d".into()],
                vec!["y".into()],
            ),
            CellFunction::Aoi21 | CellFunction::Oai21 => {
                (vec!["a".into(), "b".into(), "c".into()], vec!["y".into()])
            }
            CellFunction::Aoi22 | CellFunction::Oai22 => (
                vec!["a".into(), "b".into(), "c".into(), "d".into()],
                vec!["y".into()],
            ),
            CellFunction::Mux2 => (
                vec!["sel".into(), "d0".into(), "d1".into()],
                vec!["y".into()],
            ),
            CellFunction::Mux4 => (
                vec![
                    "sel0".into(),
                    "sel1".into(),
                    "d0".into(),
                    "d1".into(),
                    "d2".into(),
                    "d3".into(),
                ],
                vec!["y".into()],
            ),
            CellFunction::HalfAdder => (
                vec!["a".into(), "b".into()],
                vec!["sum".into(), "cout".into()],
            ),
            CellFunction::FullAdder => (
                vec!["a".into(), "b".into(), "cin".into()],
                vec!["sum".into(), "cout".into()],
            ),
            CellFunction::Adder(n) => {
                let mut inputs = Vec::new();
                for i in 0..*n {
                    inputs.push(format!("a{}", i));
                    inputs.push(format!("b{}", i));
                }
                inputs.push("cin".into());
                let mut outputs = Vec::new();
                for i in 0..*n {
                    outputs.push(format!("sum{}", i));
                }
                outputs.push("cout".into());
                (inputs, outputs)
            }
            CellFunction::Dff => (vec!["clk".into(), "d".into()], vec!["q".into()]),
            CellFunction::DffR => (
                vec!["clk".into(), "d".into(), "rst".into()],
                vec!["q".into()],
            ),
            CellFunction::DffE => (
                vec!["clk".into(), "d".into(), "en".into()],
                vec!["q".into()],
            ),
            CellFunction::DffRE => (
                vec!["clk".into(), "d".into(), "rst".into(), "en".into()],
                vec!["q".into()],
            ),
            CellFunction::Latch => (vec!["en".into(), "d".into()], vec!["q".into()]),
            CellFunction::Tristate => (vec!["a".into(), "en".into()], vec!["y".into()]),
            // Power infrastructure
            CellFunction::LevelShifterLH | CellFunction::LevelShifterHL => {
                (vec!["a".into()], vec!["y".into()])
            }
            CellFunction::IsolationAnd | CellFunction::IsolationOr => {
                (vec!["a".into(), "iso_en".into()], vec!["y".into()])
            }
            CellFunction::IsolationLatch => (vec!["a".into(), "iso_en".into()], vec!["y".into()]),
            CellFunction::RetentionDff => (
                vec!["clk".into(), "d".into(), "save".into(), "restore".into()],
                vec!["q".into()],
            ),
            CellFunction::RetentionDffR => (
                vec![
                    "clk".into(),
                    "d".into(),
                    "rst".into(),
                    "save".into(),
                    "restore".into(),
                ],
                vec!["q".into()],
            ),
            CellFunction::PowerSwitchHeader | CellFunction::PowerSwitchFooter => {
                (vec!["en".into()], Vec::new()) // Controls power rail, no logic output
            }
            CellFunction::AlwaysOnBuf => (vec!["a".into()], vec!["y".into()]),
            // Tie cells have no inputs, just output
            CellFunction::TieHigh | CellFunction::TieLow => (Vec::new(), vec!["y".into()]),
            // I/O Pads
            CellFunction::InputPad => (vec!["pad".into()], vec!["core".into()]),
            CellFunction::OutputPad => (vec!["core".into(), "oe".into()], vec!["pad".into()]),
            CellFunction::BidirPad => (
                vec!["core_out".into(), "oe".into()],
                vec!["pad".into(), "core_in".into()],
            ),
            CellFunction::ClockPad => (vec!["pad".into()], vec!["core_clk".into()]),
            CellFunction::PowerPad => (vec!["pad_vdd".into()], vec!["core_vdd".into()]),
            CellFunction::GroundPad => (vec!["pad_vss".into()], vec!["core_vss".into()]),
            CellFunction::AnalogPad => (vec!["pad".into()], vec!["core".into()]),
            CellFunction::PowerPadLdo => (
                vec!["pad_vin".into(), "enable".into()],
                vec!["core_vout".into(), "pgood".into()],
            ),
            CellFunction::Custom(_) => (Vec::new(), Vec::new()),
        }
    }

    /// Check if this is a sequential cell
    pub fn is_sequential(&self) -> bool {
        matches!(
            self,
            CellFunction::Dff
                | CellFunction::DffR
                | CellFunction::DffE
                | CellFunction::DffRE
                | CellFunction::Latch
                | CellFunction::RetentionDff
                | CellFunction::RetentionDffR
                | CellFunction::IsolationLatch
        )
    }

    /// Check if this is a power infrastructure cell
    pub fn is_power_infrastructure(&self) -> bool {
        matches!(
            self,
            CellFunction::LevelShifterLH
                | CellFunction::LevelShifterHL
                | CellFunction::IsolationAnd
                | CellFunction::IsolationOr
                | CellFunction::IsolationLatch
                | CellFunction::RetentionDff
                | CellFunction::RetentionDffR
                | CellFunction::PowerSwitchHeader
                | CellFunction::PowerSwitchFooter
                | CellFunction::AlwaysOnBuf
        )
    }

    /// Check if this is an I/O pad cell
    pub fn is_io_pad(&self) -> bool {
        matches!(
            self,
            CellFunction::InputPad
                | CellFunction::OutputPad
                | CellFunction::BidirPad
                | CellFunction::ClockPad
                | CellFunction::PowerPad
                | CellFunction::GroundPad
                | CellFunction::AnalogPad
                | CellFunction::PowerPadLdo
        )
    }

    /// Check if this is an LDO (Low Dropout Regulator) pad
    pub fn is_ldo_pad(&self) -> bool {
        matches!(self, CellFunction::PowerPadLdo)
    }
}

/// Failure mode in a library cell
///
/// This structure supports detailed failure mode characterization from foundry data.
/// The optional fields can be populated with technology-specific information when available.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LibraryFailureMode {
    /// Failure mode name (e.g., "stuck_at_0", "transient", "timing_violation")
    pub name: String,
    /// FIT contribution for this failure mode
    pub fit: f64,
    /// Fault type for simulation
    pub fault_type: FaultType,

    // === Extended fields for detailed characterization ===
    /// Physical failure mechanism (e.g., "oxide_breakdown", "electromigration", "radiation_seu")
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub mechanism: Option<String>,

    /// Detection method for this failure mode (e.g., "logic_bist", "parity_check", "comparator")
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub detection_method: Option<String>,

    /// Recovery time in nanoseconds (for transient faults)
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub recovery_time_ns: Option<f64>,

    /// Soft error cross-section in cm²/bit (for radiation-induced upsets)
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub soft_error_cross_section: Option<f64>,

    /// Affected nets/pins for this failure mode
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub affected_pins: Vec<String>,
}

impl LibraryFailureMode {
    /// Create a simple failure mode with just name, FIT, and fault type
    pub fn new(name: &str, fit: f64, fault_type: FaultType) -> Self {
        Self {
            name: name.to_string(),
            fit,
            fault_type,
            mechanism: None,
            detection_method: None,
            recovery_time_ns: None,
            soft_error_cross_section: None,
            affected_pins: Vec::new(),
        }
    }

    /// Builder: set physical mechanism
    pub fn with_mechanism(mut self, mechanism: &str) -> Self {
        self.mechanism = Some(mechanism.to_string());
        self
    }

    /// Builder: set detection method
    pub fn with_detection_method(mut self, method: &str) -> Self {
        self.detection_method = Some(method.to_string());
        self
    }

    /// Builder: set recovery time for transient faults
    pub fn with_recovery_time_ns(mut self, ns: f64) -> Self {
        self.recovery_time_ns = Some(ns);
        self
    }

    /// Builder: set soft error cross-section
    pub fn with_soft_error_cross_section(mut self, cross_section: f64) -> Self {
        self.soft_error_cross_section = Some(cross_section);
        self
    }
}

// ============================================================================
// Operating Conditions and Derating
// ============================================================================

/// Operating conditions for FIT derating calculations
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OperatingConditions {
    /// Operating temperature in Celsius
    pub temperature_c: f64,
    /// Operating voltage in volts
    pub voltage_v: f64,
    /// Process corner
    pub corner: ProcessCorner,
}

impl Default for OperatingConditions {
    fn default() -> Self {
        Self {
            temperature_c: 25.0, // Room temperature
            voltage_v: 1.0,      // Nominal voltage
            corner: ProcessCorner::Typical,
        }
    }
}

impl OperatingConditions {
    /// Create operating conditions for junction temperature and supply voltage
    pub fn new(temperature_c: f64, voltage_v: f64, corner: ProcessCorner) -> Self {
        Self {
            temperature_c,
            voltage_v,
            corner,
        }
    }

    /// Automotive junction temperature (125°C)
    pub fn automotive_junction() -> Self {
        Self {
            temperature_c: 125.0,
            voltage_v: 1.0,
            corner: ProcessCorner::Worst,
        }
    }

    /// Industrial conditions (85°C)
    pub fn industrial() -> Self {
        Self {
            temperature_c: 85.0,
            voltage_v: 1.0,
            corner: ProcessCorner::Typical,
        }
    }

    /// Consumer electronics (70°C)
    pub fn consumer() -> Self {
        Self {
            temperature_c: 70.0,
            voltage_v: 1.0,
            corner: ProcessCorner::Typical,
        }
    }
}

/// Process corner for manufacturing variations
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum ProcessCorner {
    /// Best case (fast process)
    Best,
    /// Typical (nominal process)
    Typical,
    /// Worst case (slow process)
    Worst,
}

/// Derating factors calculated for specific operating conditions
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DeratingFactors {
    /// Temperature acceleration factor (Arrhenius model)
    pub temperature_factor: f64,
    /// Voltage stress factor
    pub voltage_factor: f64,
    /// Process variation factor (±3σ)
    pub process_factor: f64,
    /// Combined derating factor (product of all factors)
    pub combined_factor: f64,
}

impl DeratingFactors {
    /// Calculate derating factors from operating conditions
    ///
    /// # Arguments
    /// * `conditions` - Target operating conditions
    /// * `ref_temp_c` - Reference temperature (typically 25°C or 55°C)
    /// * `ref_voltage_v` - Reference voltage (typically nominal)
    pub fn calculate(
        conditions: &OperatingConditions,
        ref_temp_c: f64,
        ref_voltage_v: f64,
    ) -> Self {
        let temperature_factor =
            arrhenius_acceleration_factor(conditions.temperature_c, ref_temp_c);
        let voltage_factor = voltage_acceleration_factor(conditions.voltage_v, ref_voltage_v);
        let process_factor = process_corner_factor(conditions.corner);

        Self {
            temperature_factor,
            voltage_factor,
            process_factor,
            combined_factor: temperature_factor * voltage_factor * process_factor,
        }
    }

    /// Calculate derating with default reference conditions (25°C, 1.0V)
    pub fn calculate_from_defaults(conditions: &OperatingConditions) -> Self {
        Self::calculate(conditions, 25.0, 1.0)
    }
}

/// Calculate temperature acceleration factor using Arrhenius equation
///
/// The Arrhenius equation models failure rate acceleration with temperature:
/// AF = exp(Ea/k × (1/T_ref - 1/T_op))
///
/// # Arguments
/// * `operating_temp_c` - Operating temperature in Celsius
/// * `reference_temp_c` - Reference temperature in Celsius
///
/// # Returns
/// Temperature acceleration factor (1.0 at reference temperature)
pub fn arrhenius_acceleration_factor(operating_temp_c: f64, reference_temp_c: f64) -> f64 {
    // Activation energy (Ea) in eV - typical value for CMOS
    // Oxide breakdown: ~0.3-0.4 eV
    // Electromigration: ~0.7-0.8 eV
    // Using conservative average for general CMOS failure mechanisms
    const ACTIVATION_ENERGY_EV: f64 = 0.7;

    // Boltzmann constant in eV/K
    const BOLTZMANN_EV_PER_K: f64 = 8.617e-5;

    // Convert to Kelvin
    let t_op_k = operating_temp_c + 273.15;
    let t_ref_k = reference_temp_c + 273.15;

    // Arrhenius equation: AF = exp(Ea/k × (1/T_ref - 1/T_op))
    let exponent = (ACTIVATION_ENERGY_EV / BOLTZMANN_EV_PER_K) * (1.0 / t_ref_k - 1.0 / t_op_k);

    exponent.exp()
}

/// Calculate voltage acceleration factor
///
/// Higher voltage increases stress on gate oxide and accelerates failure.
/// Using exponential model: AF = exp(γ × (V_op - V_ref) / V_ref)
///
/// # Arguments
/// * `operating_voltage_v` - Operating voltage in volts
/// * `reference_voltage_v` - Reference (nominal) voltage in volts
///
/// # Returns
/// Voltage acceleration factor (1.0 at reference voltage)
pub fn voltage_acceleration_factor(operating_voltage_v: f64, reference_voltage_v: f64) -> f64 {
    // Voltage acceleration coefficient (typical range: 2-4)
    const VOLTAGE_ACCELERATION_COEFFICIENT: f64 = 3.0;

    if reference_voltage_v <= 0.0 {
        return 1.0;
    }

    let voltage_ratio = operating_voltage_v / reference_voltage_v;

    // Exponential model for voltage stress
    // Factor > 1 for overvoltage, < 1 for undervoltage
    (VOLTAGE_ACCELERATION_COEFFICIENT * (voltage_ratio - 1.0)).exp()
}

/// Get process corner factor
///
/// Returns multiplier for FIT rate based on process variation:
/// - Best corner (fast process): Lower defect density, fewer failures
/// - Typical: Nominal process, baseline FIT
/// - Worst corner (slow process): Higher defect density, more failures
pub fn process_corner_factor(corner: ProcessCorner) -> f64 {
    match corner {
        ProcessCorner::Best => 0.7,    // -30% FIT
        ProcessCorner::Typical => 1.0, // Nominal
        ProcessCorner::Worst => 1.5,   // +50% FIT
    }
}

/// Convenience struct for common derating scenarios
#[derive(Debug, Clone, Copy)]
pub struct DeratingPreset {
    /// Preset name
    pub name: &'static str,
    /// Temperature in Celsius
    pub temperature_c: f64,
    /// Process corner
    pub corner: ProcessCorner,
}

impl DeratingPreset {
    /// ISO 26262 automotive grade (ASIL D worst case)
    pub const AUTOMOTIVE_ASILD: DeratingPreset = DeratingPreset {
        name: "automotive_asild",
        temperature_c: 150.0, // Junction at max automotive grade 0
        corner: ProcessCorner::Worst,
    };

    /// ISO 26262 automotive grade (typical)
    pub const AUTOMOTIVE_TYPICAL: DeratingPreset = DeratingPreset {
        name: "automotive_typical",
        temperature_c: 105.0, // Junction at automotive grade 1
        corner: ProcessCorner::Typical,
    };

    /// Industrial application
    pub const INDUSTRIAL: DeratingPreset = DeratingPreset {
        name: "industrial",
        temperature_c: 85.0,
        corner: ProcessCorner::Typical,
    };

    /// Consumer electronics
    pub const CONSUMER: DeratingPreset = DeratingPreset {
        name: "consumer",
        temperature_c: 70.0,
        corner: ProcessCorner::Typical,
    };

    /// Data center / server
    pub const DATACENTER: DeratingPreset = DeratingPreset {
        name: "datacenter",
        temperature_c: 55.0,
        corner: ProcessCorner::Best,
    };

    /// Convert to operating conditions with nominal voltage
    pub fn to_operating_conditions(self, voltage_v: f64) -> OperatingConditions {
        OperatingConditions::new(self.temperature_c, voltage_v, self.corner)
    }
}

// ============================================================================
// Technology Library
// ============================================================================

/// A technology library containing cells and decomposition rules
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TechLibrary {
    /// Library name
    pub name: String,
    /// Process node in nm (e.g., 7, 28)
    pub process_node: Option<u32>,
    /// Library version
    pub version: Option<String>,
    /// Reference temperature in Celsius
    pub reference_temperature: Option<f64>,
    /// Reference voltage in volts
    pub reference_voltage: Option<f64>,
    /// All library cells indexed by name
    cells: HashMap<String, LibraryCell>,
    /// Cells indexed by function
    cells_by_function: HashMap<CellFunction, Vec<String>>,
    /// Decomposition rules
    pub decomposition_rules: Vec<DecompositionRule>,
}

impl TechLibrary {
    /// Create a new empty library
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            process_node: None,
            version: None,
            reference_temperature: None,
            reference_voltage: None,
            cells: HashMap::new(),
            cells_by_function: HashMap::new(),
            decomposition_rules: Vec::new(),
        }
    }

    /// Add a cell to the library
    pub fn add_cell(&mut self, cell: LibraryCell) {
        let name = cell.name.clone();
        let function = cell.function.clone();
        self.cells.insert(name.clone(), cell);
        self.cells_by_function
            .entry(function)
            .or_default()
            .push(name);
    }

    /// Get a cell by name
    pub fn get_cell(&self, name: &str) -> Option<&LibraryCell> {
        self.cells.get(name)
    }

    /// Find cells by function
    pub fn find_cells_by_function(&self, function: &CellFunction) -> Vec<&LibraryCell> {
        self.cells_by_function
            .get(function)
            .map(|names| names.iter().filter_map(|n| self.cells.get(n)).collect())
            .unwrap_or_default()
    }

    /// Find the best cell for a function (smallest FIT)
    pub fn find_best_cell(&self, function: &CellFunction) -> Option<&LibraryCell> {
        self.find_cells_by_function(function)
            .into_iter()
            .min_by(|a, b| {
                a.fit
                    .partial_cmp(&b.fit)
                    .unwrap_or(std::cmp::Ordering::Equal)
            })
    }

    /// Check if the library has a cell for a function
    pub fn has_function(&self, function: &CellFunction) -> bool {
        self.cells_by_function.contains_key(function)
    }

    /// Add a decomposition rule
    pub fn add_decomposition_rule(&mut self, rule: DecompositionRule) {
        self.decomposition_rules.push(rule);
    }

    /// Find a decomposition rule for a LirOp
    pub fn find_decomposition(&self, op: &LirOp) -> Option<&DecompositionRule> {
        self.decomposition_rules.iter().find(|r| r.matches(op))
    }

    /// Iterate over all cells in the library
    pub fn iter_cells(&self) -> impl Iterator<Item = (&String, &LibraryCell)> {
        self.cells.iter()
    }

    /// Get all cell names
    pub fn cell_names(&self) -> Vec<&str> {
        self.cells.keys().map(|s| s.as_str()).collect()
    }

    /// Get the number of cells in the library
    pub fn cell_count(&self) -> usize {
        self.cells.len()
    }

    // ====== Derating Methods ======

    /// Set reference conditions for the library
    pub fn with_reference_conditions(mut self, temp_c: f64, voltage_v: f64) -> Self {
        self.reference_temperature = Some(temp_c);
        self.reference_voltage = Some(voltage_v);
        self
    }

    /// Calculate derating factors for operating conditions relative to library reference
    pub fn calculate_derating(&self, conditions: &OperatingConditions) -> DeratingFactors {
        let ref_temp = self.reference_temperature.unwrap_or(25.0);
        let ref_voltage = self.reference_voltage.unwrap_or(1.0);
        DeratingFactors::calculate(conditions, ref_temp, ref_voltage)
    }

    /// Get derated FIT for a cell under specific operating conditions
    pub fn get_derated_fit(
        &self,
        cell_name: &str,
        conditions: &OperatingConditions,
    ) -> Option<f64> {
        let cell = self.cells.get(cell_name)?;
        Some(cell.derated_fit(
            conditions,
            self.reference_temperature,
            self.reference_voltage,
        ))
    }

    /// Calculate total derated FIT for all cells in the library
    pub fn total_derated_fit(&self, conditions: &OperatingConditions) -> f64 {
        let factors = self.calculate_derating(conditions);
        self.cells
            .values()
            .map(|c| c.fit * factors.combined_factor)
            .sum()
    }

    /// Get summary of FIT derating across the library
    pub fn derating_summary(&self, conditions: &OperatingConditions) -> LibraryDeratingSummary {
        let factors = self.calculate_derating(conditions);
        let base_total_fit: f64 = self.cells.values().map(|c| c.fit).sum();
        let derated_total_fit = base_total_fit * factors.combined_factor;

        LibraryDeratingSummary {
            library_name: self.name.clone(),
            reference_temperature_c: self.reference_temperature.unwrap_or(25.0),
            reference_voltage_v: self.reference_voltage.unwrap_or(1.0),
            operating_conditions: conditions.clone(),
            derating_factors: factors,
            base_total_fit,
            derated_total_fit,
            cell_count: self.cells.len(),
        }
    }

    // ====== File I/O Methods ======

    /// Load a technology library from a .skalib TOML file
    ///
    /// # Example
    /// ```no_run
    /// use skalp_lir::tech_library::TechLibrary;
    /// let lib = TechLibrary::load_from_file("my_library.skalib").unwrap();
    /// ```
    pub fn load_from_file<P: AsRef<Path>>(path: P) -> Result<Self, LibraryLoadError> {
        let content = fs::read_to_string(path.as_ref())
            .map_err(|e| LibraryLoadError::IoError(e.to_string()))?;

        Self::from_toml(&content)
    }

    /// Parse a technology library from a TOML string
    pub fn from_toml(content: &str) -> Result<Self, LibraryLoadError> {
        let toml_lib: TomlLibrary =
            toml::from_str(content).map_err(|e| LibraryLoadError::ParseError(e.to_string()))?;

        toml_lib.into_tech_library()
    }

    /// Save the library to a .skalib TOML file
    pub fn save_to_file<P: AsRef<Path>>(&self, path: P) -> Result<(), LibraryLoadError> {
        let toml_lib = TomlLibrary::from_tech_library(self);
        let content = toml::to_string_pretty(&toml_lib)
            .map_err(|e| LibraryLoadError::SerializeError(e.to_string()))?;

        fs::write(path, content).map_err(|e| LibraryLoadError::IoError(e.to_string()))
    }

    /// Serialize the library to a TOML string
    pub fn to_toml(&self) -> Result<String, LibraryLoadError> {
        let toml_lib = TomlLibrary::from_tech_library(self);
        toml::to_string_pretty(&toml_lib)
            .map_err(|e| LibraryLoadError::SerializeError(e.to_string()))
    }
}

// ============================================================================
// TOML Schema for .skalib files
// ============================================================================

/// Error type for library loading/saving operations
#[derive(Debug, Clone)]
pub enum LibraryLoadError {
    IoError(String),
    ParseError(String),
    SerializeError(String),
    ValidationError(String),
    NotFound(String),
}

impl std::fmt::Display for LibraryLoadError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LibraryLoadError::IoError(s) => write!(f, "I/O error: {}", s),
            LibraryLoadError::ParseError(s) => write!(f, "Parse error: {}", s),
            LibraryLoadError::SerializeError(s) => write!(f, "Serialization error: {}", s),
            LibraryLoadError::ValidationError(s) => write!(f, "Validation error: {}", s),
            LibraryLoadError::NotFound(s) => write!(f, "Library not found: {}", s),
        }
    }
}

// ============================================================================
// Standard Library Access
// ============================================================================

// Embed standard library files at compile time for distribution
const GENERIC_ASIC_SKLIB: &str = include_str!("../../skalp-stdlib/libraries/generic_asic.sklib");
const ASIC_7NM_SKLIB: &str = include_str!("../../skalp-stdlib/libraries/asic_7nm.sklib");
const ASIC_28NM_SKLIB: &str = include_str!("../../skalp-stdlib/libraries/asic_28nm.sklib");
const FPGA_LUT4_SKLIB: &str = include_str!("../../skalp-stdlib/libraries/fpga_lut4.sklib");
const FPGA_LUT6_SKLIB: &str = include_str!("../../skalp-stdlib/libraries/fpga_lut6.sklib");

/// List all available standard library names
pub fn list_stdlib_libraries() -> Vec<&'static str> {
    vec![
        "generic_asic",
        "asic_7nm",
        "asic_28nm",
        "fpga_lut4",
        "fpga_lut6",
    ]
}

/// Get a standard technology library by name
///
/// This function looks for libraries in two locations:
/// 1. `$SKALP_STDLIB_PATH/libraries/{name}.sklib` - for custom/development libraries
/// 2. Embedded libraries bundled with the binary - for distribution
///
/// Available libraries:
/// - `generic_asic` (aliases: `generic`, `default`) - Generic 28nm ASIC
/// - `asic_7nm` (alias: `7nm`) - 7nm process node
/// - `asic_28nm` (alias: `28nm`) - 28nm process node
/// - `fpga_lut4` (alias: `fpga`) - FPGA with 4-input LUTs
/// - `fpga_lut6` - FPGA with 6-input LUTs
pub fn get_stdlib_library(name: &str) -> Result<TechLibrary, LibraryLoadError> {
    // Normalize name to canonical form
    let canonical_name = match name {
        "generic_asic" | "generic" | "default" => "generic_asic",
        "asic_7nm" | "7nm" => "asic_7nm",
        "asic_28nm" | "28nm" => "asic_28nm",
        "fpga_lut4" | "fpga" => "fpga_lut4",
        "fpga_lut6" => "fpga_lut6",
        _ => {
            return Err(LibraryLoadError::NotFound(format!(
                "Unknown library '{}'. Available: {}",
                name,
                list_stdlib_libraries().join(", ")
            )))
        }
    };

    // Check for SKALP_STDLIB_PATH environment variable first
    if let Ok(stdlib_path) = std::env::var("SKALP_STDLIB_PATH") {
        let lib_path = std::path::Path::new(&stdlib_path)
            .join("libraries")
            .join(format!("{}.sklib", canonical_name));

        if lib_path.exists() {
            return TechLibrary::load_from_file(&lib_path);
        }
    }

    // Fall back to embedded libraries
    let content = match canonical_name {
        "generic_asic" => GENERIC_ASIC_SKLIB,
        "asic_7nm" => ASIC_7NM_SKLIB,
        "asic_28nm" => ASIC_28NM_SKLIB,
        "fpga_lut4" => FPGA_LUT4_SKLIB,
        "fpga_lut6" => FPGA_LUT6_SKLIB,
        _ => unreachable!(), // Already validated above
    };

    TechLibrary::from_toml(content)
}

impl std::error::Error for LibraryLoadError {}

/// TOML representation of a technology library
#[derive(Debug, Clone, Serialize, Deserialize)]
struct TomlLibrary {
    /// Library metadata
    library: TomlLibraryMeta,
    /// Library cells
    #[serde(default)]
    cells: Vec<TomlCell>,
}

/// Library metadata in TOML format
#[derive(Debug, Clone, Serialize, Deserialize)]
struct TomlLibraryMeta {
    name: String,
    #[serde(default)]
    version: Option<String>,
    #[serde(default)]
    process_node_nm: Option<u32>,
    #[serde(default)]
    reference_temperature_c: Option<f64>,
    #[serde(default)]
    reference_voltage_v: Option<f64>,
}

/// Cell definition in TOML format
#[derive(Debug, Clone, Serialize, Deserialize)]
struct TomlCell {
    name: String,
    function: String,
    #[serde(default)]
    fit: f64,
    #[serde(default)]
    area_um2: Option<f64>,
    #[serde(default)]
    transistor_count: Option<u32>,
    #[serde(default)]
    inputs: Vec<String>,
    #[serde(default)]
    outputs: Vec<String>,
    #[serde(default)]
    drive_strength: Option<u8>,
    #[serde(default)]
    input_capacitance_ff: Option<u32>,
    #[serde(default)]
    output_capacitance_ff: Option<u32>,
    #[serde(default)]
    max_fanout: Option<u32>,
    // Voltage margins
    #[serde(default)]
    nominal_voltage_mv: Option<u16>,
    #[serde(default)]
    min_voltage_mv: Option<u16>,
    // Timing (combinational)
    #[serde(default)]
    timing: Option<TomlCellTiming>,
    // Timing (sequential)
    #[serde(default)]
    setup_ps: Option<u32>,
    #[serde(default)]
    hold_ps: Option<u32>,
    #[serde(default)]
    clk_to_q_ps: Option<u32>,
    // Failure modes
    #[serde(default)]
    failure_modes: Vec<TomlFailureMode>,
}

/// Timing information in TOML format
#[derive(Debug, Clone, Serialize, Deserialize)]
struct TomlCellTiming {
    #[serde(default)]
    arcs: Vec<TomlTimingArc>,
}

/// Timing arc in TOML format
#[derive(Debug, Clone, Serialize, Deserialize)]
struct TomlTimingArc {
    /// Arc name: "a->y" format
    arc: String,
    /// Rise delay base in picoseconds
    rise_base_ps: u32,
    /// Rise delay slope in ps/fF
    #[serde(default)]
    rise_slope_ps_per_ff: f64,
    /// Fall delay base in picoseconds
    fall_base_ps: u32,
    /// Fall delay slope in ps/fF
    #[serde(default)]
    fall_slope_ps_per_ff: f64,
}

/// Failure mode in TOML format
#[derive(Debug, Clone, Serialize, Deserialize)]
struct TomlFailureMode {
    name: String,
    fit: f64,
    #[serde(default = "default_fault_type")]
    fault_type: String,
    #[serde(default)]
    mechanism: Option<String>,
    #[serde(default)]
    detection_method: Option<String>,
}

fn default_fault_type() -> String {
    "stuck_at_0".to_string()
}

impl TomlLibrary {
    /// Convert TOML representation to TechLibrary
    fn into_tech_library(self) -> Result<TechLibrary, LibraryLoadError> {
        let mut lib = TechLibrary::new(&self.library.name);
        lib.process_node = self.library.process_node_nm;
        lib.version = self.library.version;
        lib.reference_temperature = self.library.reference_temperature_c;
        lib.reference_voltage = self.library.reference_voltage_v;

        for cell in self.cells {
            let library_cell = cell.into_library_cell()?;
            lib.add_cell(library_cell);
        }

        Ok(lib)
    }

    /// Create TOML representation from TechLibrary
    fn from_tech_library(lib: &TechLibrary) -> Self {
        let cells: Vec<TomlCell> = lib
            .cells
            .values()
            .map(TomlCell::from_library_cell)
            .collect();

        TomlLibrary {
            library: TomlLibraryMeta {
                name: lib.name.clone(),
                version: lib.version.clone(),
                process_node_nm: lib.process_node,
                reference_temperature_c: lib.reference_temperature,
                reference_voltage_v: lib.reference_voltage,
            },
            cells,
        }
    }
}

impl TomlCell {
    /// Convert TOML cell to LibraryCell
    fn into_library_cell(self) -> Result<LibraryCell, LibraryLoadError> {
        let function = parse_cell_function(&self.function)?;

        // Use provided pins or default
        let (default_inputs, default_outputs) = function.default_pins();
        let inputs = if self.inputs.is_empty() {
            default_inputs
        } else {
            self.inputs
        };
        let outputs = if self.outputs.is_empty() {
            default_outputs
        } else {
            self.outputs
        };

        // Parse timing arcs
        let timing = self.timing.map(|t| {
            let mut arcs = HashMap::new();
            for arc in t.arcs {
                arcs.insert(
                    arc.arc,
                    TimingArc {
                        rise_base_ps: arc.rise_base_ps,
                        rise_slope_ps_per_ff: arc.rise_slope_ps_per_ff,
                        fall_base_ps: arc.fall_base_ps,
                        fall_slope_ps_per_ff: arc.fall_slope_ps_per_ff,
                    },
                );
            }
            CellTiming { arcs }
        });

        // Parse failure modes
        let failure_modes: Vec<LibraryFailureMode> = self
            .failure_modes
            .into_iter()
            .map(|fm| {
                let fault_type = parse_fault_type(&fm.fault_type);
                let mut mode = LibraryFailureMode::new(&fm.name, fm.fit, fault_type);
                if let Some(m) = fm.mechanism {
                    mode = mode.with_mechanism(&m);
                }
                if let Some(d) = fm.detection_method {
                    mode = mode.with_detection_method(&d);
                }
                mode
            })
            .collect();

        Ok(LibraryCell {
            name: self.name,
            function,
            fit: self.fit,
            area: self.area_um2,
            transistor_count: self.transistor_count,
            inputs,
            outputs,
            failure_modes,
            drive_strength: self.drive_strength.unwrap_or(1),
            max_output_current_ua: None,
            output_capacitance_ff: self.output_capacitance_ff,
            input_capacitance_ff: self.input_capacitance_ff,
            max_fanout: self.max_fanout,
            min_voltage_mv: self.min_voltage_mv,
            nominal_voltage_mv: self.nominal_voltage_mv,
            timing_margin_voltage_mv: None,
            voltage_delay_coefficient: None,
            voltage_sensitivity: None,
            timing,
            setup_ps: self.setup_ps,
            hold_ps: self.hold_ps,
            clk_to_q_ps: self.clk_to_q_ps,
        })
    }

    /// Create TOML cell from LibraryCell
    fn from_library_cell(cell: &LibraryCell) -> Self {
        let timing = cell.timing.as_ref().map(|t| TomlCellTiming {
            arcs: t
                .arcs
                .iter()
                .map(|(arc, ta)| TomlTimingArc {
                    arc: arc.clone(),
                    rise_base_ps: ta.rise_base_ps,
                    rise_slope_ps_per_ff: ta.rise_slope_ps_per_ff,
                    fall_base_ps: ta.fall_base_ps,
                    fall_slope_ps_per_ff: ta.fall_slope_ps_per_ff,
                })
                .collect(),
        });

        let failure_modes: Vec<TomlFailureMode> = cell
            .failure_modes
            .iter()
            .map(|fm| TomlFailureMode {
                name: fm.name.clone(),
                fit: fm.fit,
                fault_type: format_fault_type(&fm.fault_type),
                mechanism: fm.mechanism.clone(),
                detection_method: fm.detection_method.clone(),
            })
            .collect();

        TomlCell {
            name: cell.name.clone(),
            function: format_cell_function(&cell.function),
            fit: cell.fit,
            area_um2: cell.area,
            transistor_count: cell.transistor_count,
            inputs: cell.inputs.clone(),
            outputs: cell.outputs.clone(),
            drive_strength: Some(cell.drive_strength),
            input_capacitance_ff: cell.input_capacitance_ff,
            output_capacitance_ff: cell.output_capacitance_ff,
            max_fanout: cell.max_fanout,
            nominal_voltage_mv: cell.nominal_voltage_mv,
            min_voltage_mv: cell.min_voltage_mv,
            timing,
            setup_ps: cell.setup_ps,
            hold_ps: cell.hold_ps,
            clk_to_q_ps: cell.clk_to_q_ps,
            failure_modes,
        }
    }
}

/// Parse a function name string to CellFunction
fn parse_cell_function(s: &str) -> Result<CellFunction, LibraryLoadError> {
    let lower = s.to_lowercase();
    match lower.as_str() {
        "inv" | "not" => Ok(CellFunction::Inv),
        "buf" | "buffer" => Ok(CellFunction::Buf),
        "nand2" => Ok(CellFunction::Nand2),
        "nand3" => Ok(CellFunction::Nand3),
        "nand4" => Ok(CellFunction::Nand4),
        "nor2" => Ok(CellFunction::Nor2),
        "nor3" => Ok(CellFunction::Nor3),
        "nor4" => Ok(CellFunction::Nor4),
        "and2" => Ok(CellFunction::And2),
        "and3" => Ok(CellFunction::And3),
        "and4" => Ok(CellFunction::And4),
        "or2" => Ok(CellFunction::Or2),
        "or3" => Ok(CellFunction::Or3),
        "or4" => Ok(CellFunction::Or4),
        "xor2" => Ok(CellFunction::Xor2),
        "xnor2" => Ok(CellFunction::Xnor2),
        "andnot" | "and_not" => Ok(CellFunction::AndNot),
        "ornot" | "or_not" => Ok(CellFunction::OrNot),
        "aoi21" => Ok(CellFunction::Aoi21),
        "aoi22" => Ok(CellFunction::Aoi22),
        "oai21" => Ok(CellFunction::Oai21),
        "oai22" => Ok(CellFunction::Oai22),
        "mux2" => Ok(CellFunction::Mux2),
        "mux4" => Ok(CellFunction::Mux4),
        "ha" | "half_adder" | "halfadder" => Ok(CellFunction::HalfAdder),
        "fa" | "full_adder" | "fulladder" => Ok(CellFunction::FullAdder),
        "dff" => Ok(CellFunction::Dff),
        "dffr" | "dff_r" => Ok(CellFunction::DffR),
        "dffe" | "dff_e" => Ok(CellFunction::DffE),
        "dffre" | "dff_re" => Ok(CellFunction::DffRE),
        "latch" => Ok(CellFunction::Latch),
        "tristate" | "tri" => Ok(CellFunction::Tristate),
        // Power infrastructure
        "level_shifter_lh" | "lslh" => Ok(CellFunction::LevelShifterLH),
        "level_shifter_hl" | "lshl" => Ok(CellFunction::LevelShifterHL),
        "isolation_and" | "iso_and" => Ok(CellFunction::IsolationAnd),
        "isolation_or" | "iso_or" => Ok(CellFunction::IsolationOr),
        "isolation_latch" | "iso_latch" => Ok(CellFunction::IsolationLatch),
        "retention_dff" | "ret_dff" => Ok(CellFunction::RetentionDff),
        "retention_dffr" | "ret_dffr" => Ok(CellFunction::RetentionDffR),
        "power_switch_header" | "psh" => Ok(CellFunction::PowerSwitchHeader),
        "power_switch_footer" | "psf" => Ok(CellFunction::PowerSwitchFooter),
        "always_on_buf" | "aob" => Ok(CellFunction::AlwaysOnBuf),
        "tie_high" | "tieh" => Ok(CellFunction::TieHigh),
        "tie_low" | "tiel" => Ok(CellFunction::TieLow),
        // I/O Pads
        "input_pad" | "ipad" => Ok(CellFunction::InputPad),
        "output_pad" | "opad" => Ok(CellFunction::OutputPad),
        "bidir_pad" | "iopad" => Ok(CellFunction::BidirPad),
        "clock_pad" | "clkpad" => Ok(CellFunction::ClockPad),
        "power_pad" | "vddpad" => Ok(CellFunction::PowerPad),
        "ground_pad" | "vsspad" | "gndpad" => Ok(CellFunction::GroundPad),
        "analog_pad" | "apad" => Ok(CellFunction::AnalogPad),
        "power_pad_ldo" | "vddpad_ldo" | "ldopad" => Ok(CellFunction::PowerPadLdo),
        _ => {
            // Check for adder with width
            if lower.starts_with("adder") {
                if let Some(width_str) = lower.strip_prefix("adder") {
                    if let Ok(width) = width_str.parse::<u32>() {
                        return Ok(CellFunction::Adder(width));
                    }
                }
            }
            // Default to custom
            Ok(CellFunction::Custom(s.to_string()))
        }
    }
}

/// Format CellFunction to string for TOML
fn format_cell_function(f: &CellFunction) -> String {
    match f {
        CellFunction::Inv => "inv".to_string(),
        CellFunction::Buf => "buf".to_string(),
        CellFunction::Nand2 => "nand2".to_string(),
        CellFunction::Nand3 => "nand3".to_string(),
        CellFunction::Nand4 => "nand4".to_string(),
        CellFunction::Nor2 => "nor2".to_string(),
        CellFunction::Nor3 => "nor3".to_string(),
        CellFunction::Nor4 => "nor4".to_string(),
        CellFunction::And2 => "and2".to_string(),
        CellFunction::And3 => "and3".to_string(),
        CellFunction::And4 => "and4".to_string(),
        CellFunction::Or2 => "or2".to_string(),
        CellFunction::Or3 => "or3".to_string(),
        CellFunction::Or4 => "or4".to_string(),
        CellFunction::Xor2 => "xor2".to_string(),
        CellFunction::Xnor2 => "xnor2".to_string(),
        CellFunction::AndNot => "andnot".to_string(),
        CellFunction::OrNot => "ornot".to_string(),
        CellFunction::Aoi21 => "aoi21".to_string(),
        CellFunction::Aoi22 => "aoi22".to_string(),
        CellFunction::Oai21 => "oai21".to_string(),
        CellFunction::Oai22 => "oai22".to_string(),
        CellFunction::Mux2 => "mux2".to_string(),
        CellFunction::Mux4 => "mux4".to_string(),
        CellFunction::HalfAdder => "half_adder".to_string(),
        CellFunction::FullAdder => "full_adder".to_string(),
        CellFunction::Adder(n) => format!("adder{}", n),
        CellFunction::Dff => "dff".to_string(),
        CellFunction::DffR => "dffr".to_string(),
        CellFunction::DffE => "dffe".to_string(),
        CellFunction::DffRE => "dffre".to_string(),
        CellFunction::Latch => "latch".to_string(),
        CellFunction::Tristate => "tristate".to_string(),
        CellFunction::LevelShifterLH => "level_shifter_lh".to_string(),
        CellFunction::LevelShifterHL => "level_shifter_hl".to_string(),
        CellFunction::IsolationAnd => "isolation_and".to_string(),
        CellFunction::IsolationOr => "isolation_or".to_string(),
        CellFunction::IsolationLatch => "isolation_latch".to_string(),
        CellFunction::RetentionDff => "retention_dff".to_string(),
        CellFunction::RetentionDffR => "retention_dffr".to_string(),
        CellFunction::PowerSwitchHeader => "power_switch_header".to_string(),
        CellFunction::PowerSwitchFooter => "power_switch_footer".to_string(),
        CellFunction::AlwaysOnBuf => "always_on_buf".to_string(),
        CellFunction::TieHigh => "tie_high".to_string(),
        CellFunction::TieLow => "tie_low".to_string(),
        // I/O Pads
        CellFunction::InputPad => "input_pad".to_string(),
        CellFunction::OutputPad => "output_pad".to_string(),
        CellFunction::BidirPad => "bidir_pad".to_string(),
        CellFunction::ClockPad => "clock_pad".to_string(),
        CellFunction::PowerPad => "power_pad".to_string(),
        CellFunction::GroundPad => "ground_pad".to_string(),
        CellFunction::AnalogPad => "analog_pad".to_string(),
        CellFunction::PowerPadLdo => "power_pad_ldo".to_string(),
        CellFunction::Custom(s) => s.clone(),
    }
}

/// Parse fault type string
fn parse_fault_type(s: &str) -> FaultType {
    match s.to_lowercase().as_str() {
        "stuck_at_0" | "sa0" => FaultType::StuckAt0,
        "stuck_at_1" | "sa1" => FaultType::StuckAt1,
        "transient" | "seu" => FaultType::Transient,
        "delay" => FaultType::Delay,
        "bridge" => FaultType::Bridge,
        "timing" | "setup_hold" => FaultType::Timing,
        "open" | "open_circuit" => FaultType::Open,
        "data_retention" | "retention" => FaultType::DataRetention,
        "clock_path" | "clock" => FaultType::ClockPath,
        "reset_path" | "reset" => FaultType::ResetPath,
        _ => FaultType::StuckAt0, // Default
    }
}

/// Format fault type to string
fn format_fault_type(f: &FaultType) -> String {
    match f {
        FaultType::StuckAt0 => "stuck_at_0".to_string(),
        FaultType::StuckAt1 => "stuck_at_1".to_string(),
        FaultType::Transient => "transient".to_string(),
        FaultType::Delay => "delay".to_string(),
        FaultType::Bridge => "bridge".to_string(),
        FaultType::Timing => "timing".to_string(),
        FaultType::Open => "open".to_string(),
        FaultType::DataRetention => "data_retention".to_string(),
        FaultType::ClockPath => "clock_path".to_string(),
        FaultType::ResetPath => "reset_path".to_string(),
    }
}

/// Summary of derating calculations for a library
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LibraryDeratingSummary {
    /// Library name
    pub library_name: String,
    /// Reference temperature
    pub reference_temperature_c: f64,
    /// Reference voltage
    pub reference_voltage_v: f64,
    /// Operating conditions used
    pub operating_conditions: OperatingConditions,
    /// Calculated derating factors
    pub derating_factors: DeratingFactors,
    /// Total FIT at reference conditions
    pub base_total_fit: f64,
    /// Total FIT at operating conditions
    pub derated_total_fit: f64,
    /// Number of cells in library
    pub cell_count: usize,
}

impl LibraryDeratingSummary {
    /// Format a human-readable summary
    pub fn format_report(&self) -> String {
        let mut output = String::new();
        output.push_str(&format!(
            "FIT Derating Summary for: {}\n",
            self.library_name
        ));
        output.push_str("═══════════════════════════════════════════════════════════════\n\n");

        output.push_str("Reference Conditions:\n");
        output.push_str(&format!(
            "  Temperature: {:.1}°C\n",
            self.reference_temperature_c
        ));
        output.push_str(&format!("  Voltage: {:.2}V\n\n", self.reference_voltage_v));

        output.push_str("Operating Conditions:\n");
        output.push_str(&format!(
            "  Temperature: {:.1}°C\n",
            self.operating_conditions.temperature_c
        ));
        output.push_str(&format!(
            "  Voltage: {:.2}V\n",
            self.operating_conditions.voltage_v
        ));
        output.push_str(&format!(
            "  Process Corner: {:?}\n\n",
            self.operating_conditions.corner
        ));

        output.push_str("Derating Factors:\n");
        output.push_str(&format!(
            "  Temperature (Arrhenius): {:.3}×\n",
            self.derating_factors.temperature_factor
        ));
        output.push_str(&format!(
            "  Voltage stress: {:.3}×\n",
            self.derating_factors.voltage_factor
        ));
        output.push_str(&format!(
            "  Process corner: {:.3}×\n",
            self.derating_factors.process_factor
        ));
        output.push_str(&format!(
            "  Combined: {:.3}×\n\n",
            self.derating_factors.combined_factor
        ));

        output.push_str(&format!("Library: {} cells\n", self.cell_count));
        output.push_str(&format!(
            "Base Total FIT (at ref): {:.4} FIT\n",
            self.base_total_fit
        ));
        output.push_str(&format!(
            "Derated Total FIT (at op): {:.4} FIT\n",
            self.derated_total_fit
        ));

        output
    }
}

// ============================================================================
// Decomposition Rules
// ============================================================================

/// Rule for decomposing word-level operations to library cells
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DecompositionRule {
    /// Name/description of the rule
    pub name: String,
    /// Source operation type
    pub source: DecompSource,
    /// Target cell functions
    pub targets: Vec<CellFunction>,
    /// How to wire the decomposed cells
    pub connectivity: DecompConnectivity,
    /// FIT multiplier (for adjusting shared failure modes)
    pub fit_multiplier: f64,
}

impl DecompositionRule {
    /// Check if this rule matches a LirOp
    pub fn matches(&self, op: &LirOp) -> bool {
        matches!(
            (&self.source, op),
            (DecompSource::Xor, LirOp::Xor { .. })
                | (DecompSource::And, LirOp::And { .. })
                | (DecompSource::Or, LirOp::Or { .. })
                | (DecompSource::Not, LirOp::Not { .. })
                | (DecompSource::Mux2, LirOp::Mux2 { .. })
                | (DecompSource::Add, LirOp::Add { .. })
                | (DecompSource::Sub, LirOp::Sub { .. })
                | (DecompSource::Eq, LirOp::Eq { .. })
                | (DecompSource::Lt, LirOp::Lt { .. })
                | (DecompSource::Reg, LirOp::Reg { .. })
        )
    }
}

/// Source operation for decomposition
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum DecompSource {
    // Basic logic
    And,
    Or,
    Xor,
    Not,
    Nand,
    Nor,

    // Mux
    Mux2,
    MuxN,

    // Arithmetic
    Add,
    Sub,
    Mul,

    // Comparison
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,

    // Sequential
    Reg,
    Latch,

    // Reduction
    RedAnd,
    RedOr,
    RedXor,

    // Shift
    Shl,
    Shr,
}

/// How to connect decomposed cells
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum DecompConnectivity {
    /// Simple chain (output of one feeds next)
    Chain,
    /// Parallel with reduction (e.g., bit-slice operations)
    ParallelReduce,
    /// Tree structure (e.g., for wide AND/OR)
    Tree,
    /// Ripple carry (for adders)
    RippleCarry,
    /// Custom connectivity pattern
    Custom(String),
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_library_creation() {
        let mut lib = TechLibrary::new("test_lib");

        lib.add_cell(LibraryCell::new_comb("INV_X1", CellFunction::Inv, 0.05));
        lib.add_cell(LibraryCell::new_comb("NAND2_X1", CellFunction::Nand2, 0.1));

        assert_eq!(lib.cell_count(), 2);
        assert!(lib.get_cell("INV_X1").is_some());
        assert!(lib.get_cell("NAND2_X1").is_some());
        assert!(lib.get_cell("NONEXISTENT").is_none());
    }

    #[test]
    fn test_find_by_function() {
        let mut lib = TechLibrary::new("test_lib");

        lib.add_cell(LibraryCell::new_comb("NAND2_X1", CellFunction::Nand2, 0.1));
        lib.add_cell(LibraryCell::new_comb("NAND2_X2", CellFunction::Nand2, 0.15));

        let nand_cells = lib.find_cells_by_function(&CellFunction::Nand2);
        assert_eq!(nand_cells.len(), 2);
    }

    #[test]
    fn test_find_best_cell() {
        let mut lib = TechLibrary::new("test_lib");

        lib.add_cell(LibraryCell::new_comb("NAND2_X2", CellFunction::Nand2, 0.15));
        lib.add_cell(LibraryCell::new_comb("NAND2_X1", CellFunction::Nand2, 0.1));

        let best = lib.find_best_cell(&CellFunction::Nand2).unwrap();
        assert_eq!(best.name, "NAND2_X1");
        assert!((best.fit - 0.1).abs() < 0.001);
    }

    #[test]
    fn test_cell_with_failure_modes() {
        let cell = LibraryCell::new_comb("NAND2_X1", CellFunction::Nand2, 0.1)
            .with_default_failure_modes();

        assert_eq!(cell.failure_modes.len(), 2);
        let total_fit: f64 = cell.failure_modes.iter().map(|f| f.fit).sum();
        assert!((total_fit - 0.1).abs() < 0.001);
    }

    #[test]
    fn test_decomposition_rule() {
        let rule = DecompositionRule {
            name: "xor_to_nand".to_string(),
            source: DecompSource::Xor,
            targets: vec![
                CellFunction::Nand2,
                CellFunction::Nand2,
                CellFunction::Nand2,
                CellFunction::Nand2,
            ],
            connectivity: DecompConnectivity::Custom("xor_nand_pattern".to_string()),
            fit_multiplier: 0.9,
        };

        assert!(rule.matches(&LirOp::Xor { width: 1 }));
        assert!(!rule.matches(&LirOp::And { width: 1 }));
    }

    #[test]
    fn test_cell_function_pins() {
        let (inputs, outputs) = CellFunction::FullAdder.default_pins();
        assert_eq!(inputs.len(), 3); // a, b, cin
        assert_eq!(outputs.len(), 2); // sum, cout

        let (inputs, outputs) = CellFunction::Dff.default_pins();
        assert_eq!(inputs.len(), 2); // clk, d
        assert_eq!(outputs.len(), 1); // q
    }

    #[test]
    fn test_sequential_detection() {
        assert!(CellFunction::Dff.is_sequential());
        assert!(CellFunction::DffR.is_sequential());
        assert!(CellFunction::Latch.is_sequential());
        assert!(!CellFunction::Nand2.is_sequential());
        assert!(!CellFunction::FullAdder.is_sequential());
    }

    // ======================================================================
    // Derating Tests
    // ======================================================================

    #[test]
    fn test_arrhenius_at_reference() {
        // At reference temperature, factor should be 1.0
        let factor = arrhenius_acceleration_factor(25.0, 25.0);
        assert!((factor - 1.0).abs() < 0.001);
    }

    #[test]
    fn test_arrhenius_higher_temp() {
        // At higher temperature, factor should be > 1
        let factor = arrhenius_acceleration_factor(85.0, 25.0);
        assert!(factor > 1.0);

        // At automotive junction temperature (125°C), should be significantly higher
        let auto_factor = arrhenius_acceleration_factor(125.0, 25.0);
        assert!(auto_factor > factor);
    }

    #[test]
    fn test_arrhenius_lower_temp() {
        // At lower temperature, factor should be < 1
        let factor = arrhenius_acceleration_factor(0.0, 25.0);
        assert!(factor < 1.0);
    }

    #[test]
    fn test_voltage_at_reference() {
        // At reference voltage, factor should be 1.0
        let factor = voltage_acceleration_factor(1.0, 1.0);
        assert!((factor - 1.0).abs() < 0.001);
    }

    #[test]
    fn test_voltage_overvoltage() {
        // At 10% overvoltage, factor should be > 1
        let factor = voltage_acceleration_factor(1.1, 1.0);
        assert!(factor > 1.0);
    }

    #[test]
    fn test_voltage_undervoltage() {
        // At undervoltage, factor should be < 1
        let factor = voltage_acceleration_factor(0.9, 1.0);
        assert!(factor < 1.0);
    }

    #[test]
    fn test_process_corner_factors() {
        assert!((process_corner_factor(ProcessCorner::Best) - 0.7).abs() < 0.001);
        assert!((process_corner_factor(ProcessCorner::Typical) - 1.0).abs() < 0.001);
        assert!((process_corner_factor(ProcessCorner::Worst) - 1.5).abs() < 0.001);
    }

    #[test]
    fn test_derating_factors_combined() {
        let conditions = OperatingConditions::new(85.0, 1.0, ProcessCorner::Typical);
        let factors = DeratingFactors::calculate(&conditions, 25.0, 1.0);

        // Combined should be product of individual factors
        let expected = factors.temperature_factor * factors.voltage_factor * factors.process_factor;
        assert!((factors.combined_factor - expected).abs() < 0.001);
    }

    #[test]
    fn test_cell_derated_fit() {
        let cell = LibraryCell::new_comb("NAND2_X1", CellFunction::Nand2, 1.0);

        // At reference conditions, derated FIT should equal base FIT
        let ref_conditions = OperatingConditions::default();
        let derated = cell.derated_fit(&ref_conditions, Some(25.0), Some(1.0));
        assert!((derated - 1.0).abs() < 0.001);

        // At elevated temperature, derated FIT should be higher
        let hot_conditions = OperatingConditions::new(85.0, 1.0, ProcessCorner::Typical);
        let derated_hot = cell.derated_fit(&hot_conditions, Some(25.0), Some(1.0));
        assert!(derated_hot > 1.0);
    }

    #[test]
    fn test_library_derating() {
        let mut lib = TechLibrary::new("test_lib").with_reference_conditions(25.0, 1.0);

        lib.add_cell(LibraryCell::new_comb("INV_X1", CellFunction::Inv, 0.05));
        lib.add_cell(LibraryCell::new_comb("NAND2_X1", CellFunction::Nand2, 0.1));

        let conditions = OperatingConditions::automotive_junction();
        let derated_inv = lib.get_derated_fit("INV_X1", &conditions).unwrap();

        // Should be higher than base FIT (0.05) due to elevated temp
        assert!(derated_inv > 0.05);
    }

    #[test]
    fn test_library_total_derated_fit() {
        let mut lib = TechLibrary::new("test_lib").with_reference_conditions(25.0, 1.0);

        lib.add_cell(LibraryCell::new_comb("INV_X1", CellFunction::Inv, 0.05));
        lib.add_cell(LibraryCell::new_comb("NAND2_X1", CellFunction::Nand2, 0.1));

        // At reference, total should be 0.15
        let ref_conditions = OperatingConditions::default();
        let ref_total = lib.total_derated_fit(&ref_conditions);
        assert!((ref_total - 0.15).abs() < 0.001);

        // At elevated temp, should be higher
        let hot_conditions = OperatingConditions::industrial();
        let hot_total = lib.total_derated_fit(&hot_conditions);
        assert!(hot_total > ref_total);
    }

    #[test]
    fn test_derating_presets() {
        // Test that presets produce expected relative ordering
        let automotive = DeratingPreset::AUTOMOTIVE_ASILD.to_operating_conditions(1.0);
        let consumer = DeratingPreset::CONSUMER.to_operating_conditions(1.0);

        let auto_factors = DeratingFactors::calculate_from_defaults(&automotive);
        let consumer_factors = DeratingFactors::calculate_from_defaults(&consumer);

        // Automotive should have higher derating factor than consumer
        assert!(auto_factors.combined_factor > consumer_factors.combined_factor);
    }

    #[test]
    fn test_derating_summary() {
        let mut lib = TechLibrary::new("test_lib").with_reference_conditions(25.0, 1.0);

        lib.add_cell(LibraryCell::new_comb("NAND2_X1", CellFunction::Nand2, 0.1));

        let conditions = OperatingConditions::industrial();
        let summary = lib.derating_summary(&conditions);

        assert_eq!(summary.library_name, "test_lib");
        assert_eq!(summary.cell_count, 1);
        assert!((summary.base_total_fit - 0.1).abs() < 0.001);
        assert!(summary.derated_total_fit > summary.base_total_fit);
    }

    #[test]
    fn test_derating_report_format() {
        let mut lib = TechLibrary::new("test_lib").with_reference_conditions(25.0, 1.0);

        lib.add_cell(LibraryCell::new_comb("NAND2_X1", CellFunction::Nand2, 0.1));

        let summary = lib.derating_summary(&OperatingConditions::automotive_junction());
        let report = summary.format_report();

        assert!(report.contains("FIT Derating Summary"));
        assert!(report.contains("test_lib"));
        assert!(report.contains("Temperature (Arrhenius)"));
        assert!(report.contains("Combined:"));
    }

    #[test]
    fn test_cell_derated_failure_modes() {
        let cell = LibraryCell::new_comb("NAND2_X1", CellFunction::Nand2, 0.1)
            .with_default_failure_modes();

        let hot_conditions = OperatingConditions::new(100.0, 1.0, ProcessCorner::Worst);
        let derated_modes = cell.derated_failure_modes(&hot_conditions, Some(25.0), Some(1.0));

        // Should have same number of failure modes
        assert_eq!(derated_modes.len(), cell.failure_modes.len());

        // Each derated mode should be higher than base
        for (base, derated) in cell.failure_modes.iter().zip(derated_modes.iter()) {
            assert!(derated.fit > base.fit);
        }
    }

    // ======================================================================
    // TOML Parsing Tests
    // ======================================================================

    #[test]
    fn test_toml_parse_simple_library() {
        let toml_content = r#"
            [library]
            name = "test_lib"
            version = "1.0"
            process_node_nm = 7
            reference_temperature_c = 25.0
            reference_voltage_v = 1.0

            [[cells]]
            name = "INV_X1"
            function = "inv"
            fit = 0.05
        "#;

        let lib = TechLibrary::from_toml(toml_content).expect("Failed to parse TOML");

        assert_eq!(lib.name, "test_lib");
        assert_eq!(lib.process_node, Some(7));
        assert_eq!(lib.cell_count(), 1);

        let inv = lib.get_cell("INV_X1").expect("INV_X1 not found");
        assert_eq!(inv.function, CellFunction::Inv);
        assert!((inv.fit - 0.05).abs() < 0.001);
    }

    #[test]
    fn test_toml_parse_with_timing() {
        let toml_content = r#"
            [library]
            name = "timing_test"

            [[cells]]
            name = "NAND2_X1"
            function = "nand2"
            fit = 0.10

            [cells.timing]
            arcs = [
                { arc = "a->y", rise_base_ps = 20, rise_slope_ps_per_ff = 0.5, fall_base_ps = 18, fall_slope_ps_per_ff = 0.4 },
                { arc = "b->y", rise_base_ps = 22, rise_slope_ps_per_ff = 0.6, fall_base_ps = 19, fall_slope_ps_per_ff = 0.5 }
            ]
        "#;

        let lib = TechLibrary::from_toml(toml_content).expect("Failed to parse TOML");
        let cell = lib.get_cell("NAND2_X1").expect("NAND2_X1 not found");

        let timing = cell.timing.as_ref().expect("No timing data");
        assert_eq!(timing.arcs.len(), 2);

        let arc_a = timing.arcs.get("a->y").expect("arc a->y not found");
        assert_eq!(arc_a.rise_base_ps, 20);
        assert!((arc_a.rise_slope_ps_per_ff - 0.5).abs() < 0.001);
    }

    #[test]
    fn test_toml_parse_with_failure_modes() {
        let toml_content = r#"
            [library]
            name = "failure_test"

            [[cells]]
            name = "DFFE_X1"
            function = "dffe"
            fit = 0.25
            setup_ps = 50
            hold_ps = 20
            clk_to_q_ps = 80

            [[cells.failure_modes]]
            name = "stuck_at_0"
            fit = 0.05
            fault_type = "stuck_at_0"

            [[cells.failure_modes]]
            name = "setup_violation"
            fit = 0.05
            fault_type = "timing"
            mechanism = "setup_slack_violation"
        "#;

        let lib = TechLibrary::from_toml(toml_content).expect("Failed to parse TOML");
        let cell = lib.get_cell("DFFE_X1").expect("DFFE_X1 not found");

        assert_eq!(cell.setup_ps, Some(50));
        assert_eq!(cell.hold_ps, Some(20));
        assert_eq!(cell.clk_to_q_ps, Some(80));

        assert_eq!(cell.failure_modes.len(), 2);
        assert_eq!(cell.failure_modes[0].name, "stuck_at_0");
        assert_eq!(cell.failure_modes[1].fault_type, FaultType::Timing);
    }

    #[test]
    fn test_toml_roundtrip() {
        // Create a library programmatically
        let mut lib = TechLibrary::new("roundtrip_test");
        lib.version = Some("1.0".to_string());
        lib.process_node = Some(7);
        lib.reference_temperature = Some(25.0);
        lib.reference_voltage = Some(1.0);

        lib.add_cell(
            LibraryCell::new_comb("NAND2_X1", CellFunction::Nand2, 0.1)
                .with_default_failure_modes(),
        );

        // Convert to TOML and back
        let toml_str = lib.to_toml().expect("Failed to serialize");
        let lib2 = TechLibrary::from_toml(&toml_str).expect("Failed to parse");

        assert_eq!(lib2.name, "roundtrip_test");
        assert_eq!(lib2.cell_count(), 1);

        let cell = lib2.get_cell("NAND2_X1").expect("NAND2_X1 not found");
        assert!((cell.fit - 0.1).abs() < 0.001);
        assert_eq!(cell.failure_modes.len(), 2);
    }

    #[test]
    fn test_parse_cell_function() {
        // Test various function name formats
        assert_eq!(parse_cell_function("inv").unwrap(), CellFunction::Inv);
        assert_eq!(parse_cell_function("NOT").unwrap(), CellFunction::Inv);
        assert_eq!(parse_cell_function("nand2").unwrap(), CellFunction::Nand2);
        assert_eq!(parse_cell_function("NAND2").unwrap(), CellFunction::Nand2);
        assert_eq!(parse_cell_function("xor2").unwrap(), CellFunction::Xor2);
        assert_eq!(parse_cell_function("dff").unwrap(), CellFunction::Dff);
        assert_eq!(parse_cell_function("dffe").unwrap(), CellFunction::DffE);
        assert_eq!(
            parse_cell_function("level_shifter_lh").unwrap(),
            CellFunction::LevelShifterLH
        );
        assert_eq!(
            parse_cell_function("iso_and").unwrap(),
            CellFunction::IsolationAnd
        );
    }
}
