//! Timing Data Structures
//!
//! This module provides data structures for representing timing information
//! used in static timing analysis (STA) and technology mapping.
//!
//! # Key Concepts
//!
//! - **NLDM (Non-Linear Delay Model)**: 2D lookup table for delay as a function
//!   of input slew and output load.
//! - **Timing Arc**: Delay path from an input pin to an output pin of a cell.
//! - **Slew**: Rate of signal transition (rise/fall time).
//! - **Load**: Capacitive load on a net.
//!
//! # References
//!
//! - Synopsys Liberty format specification
//! - Static Timing Analysis for Nanometer Designs (Bhasker & Chadha)

use indexmap::IndexMap;

/// Time unit in picoseconds
pub type TimePs = f64;

/// Capacitance unit in femtofarads
pub type CapacitanceFf = f64;

/// Non-Linear Delay Model (NLDM) lookup table
///
/// Provides delay or slew as a function of input slew and output load.
/// Values are interpolated for inputs between axis points.
#[derive(Debug, Clone)]
pub struct NldmTable {
    /// Input slew axis values (ps)
    pub input_slew_axis: Vec<TimePs>,
    /// Output load axis values (fF)
    pub output_load_axis: Vec<CapacitanceFf>,
    /// 2D lookup table values [slew_idx][load_idx]
    pub values: Vec<Vec<f64>>,
}

impl NldmTable {
    /// Create a new NLDM table
    pub fn new(
        input_slew_axis: Vec<TimePs>,
        output_load_axis: Vec<CapacitanceFf>,
        values: Vec<Vec<f64>>,
    ) -> Self {
        Self {
            input_slew_axis,
            output_load_axis,
            values,
        }
    }

    /// Create a constant delay table (single value)
    pub fn constant(value: f64) -> Self {
        Self {
            input_slew_axis: vec![0.0],
            output_load_axis: vec![0.0],
            values: vec![vec![value]],
        }
    }

    /// Look up a value with bilinear interpolation
    pub fn lookup(&self, input_slew: TimePs, output_load: CapacitanceFf) -> f64 {
        if self.values.is_empty() || self.values[0].is_empty() {
            return 0.0;
        }

        // Handle degenerate cases
        if self.input_slew_axis.len() == 1 && self.output_load_axis.len() == 1 {
            return self.values[0][0];
        }

        // Find interpolation indices for input slew
        let (slew_idx, slew_frac) = self.find_interp_index(&self.input_slew_axis, input_slew);

        // Find interpolation indices for output load
        let (load_idx, load_frac) = self.find_interp_index(&self.output_load_axis, output_load);

        // Bilinear interpolation
        let v00 = self.values[slew_idx][load_idx];
        let v01 = self.values[slew_idx]
            .get(load_idx + 1)
            .copied()
            .unwrap_or(v00);
        let v10 = self
            .values
            .get(slew_idx + 1)
            .and_then(|row| row.get(load_idx).copied())
            .unwrap_or(v00);
        let v11 = self
            .values
            .get(slew_idx + 1)
            .and_then(|row| row.get(load_idx + 1).copied())
            .unwrap_or(v00);

        let v0 = v00 + (v01 - v00) * load_frac;
        let v1 = v10 + (v11 - v10) * load_frac;

        v0 + (v1 - v0) * slew_frac
    }

    /// Find interpolation index and fraction for a value in an axis
    fn find_interp_index(&self, axis: &[f64], value: f64) -> (usize, f64) {
        if axis.len() <= 1 {
            return (0, 0.0);
        }

        // Clamp to axis range
        if value <= axis[0] {
            return (0, 0.0);
        }
        if value >= axis[axis.len() - 1] {
            return (axis.len() - 2, 1.0);
        }

        // Find the interval
        for i in 0..axis.len() - 1 {
            if value >= axis[i] && value < axis[i + 1] {
                let frac = (value - axis[i]) / (axis[i + 1] - axis[i]);
                return (i, frac);
            }
        }

        (axis.len() - 2, 1.0)
    }
}

impl Default for NldmTable {
    fn default() -> Self {
        Self::constant(0.0)
    }
}

/// Timing sense of an arc
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TimingSense {
    /// Output rises when input rises
    PositiveUnate,
    /// Output falls when input rises
    NegativeUnate,
    /// Output can rise or fall depending on other inputs
    NonUnate,
}

/// Type of timing arc
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TimingType {
    /// Combinational delay
    Combinational,
    /// Rising edge of clock to output
    RisingEdge,
    /// Falling edge of clock to output
    FallingEdge,
    /// Setup time constraint
    Setup,
    /// Hold time constraint
    Hold,
    /// Recovery time (async reset to clock)
    Recovery,
    /// Removal time (clock to async reset)
    Removal,
}

/// A timing arc represents the delay from an input pin to an output pin
#[derive(Debug, Clone)]
pub struct TimingArc {
    /// Source pin name
    pub from_pin: String,
    /// Destination pin name
    pub to_pin: String,
    /// Timing sense
    pub sense: TimingSense,
    /// Timing type
    pub timing_type: TimingType,
    /// Rising delay table
    pub cell_rise: Option<NldmTable>,
    /// Falling delay table
    pub cell_fall: Option<NldmTable>,
    /// Rising output slew table
    pub rise_transition: Option<NldmTable>,
    /// Falling output slew table
    pub fall_transition: Option<NldmTable>,
    /// Setup time (for sequential cells)
    pub setup: Option<NldmTable>,
    /// Hold time (for sequential cells)
    pub hold: Option<NldmTable>,
}

impl TimingArc {
    /// Create a new combinational timing arc
    pub fn combinational(from_pin: &str, to_pin: &str) -> Self {
        Self {
            from_pin: from_pin.to_string(),
            to_pin: to_pin.to_string(),
            sense: TimingSense::PositiveUnate,
            timing_type: TimingType::Combinational,
            cell_rise: None,
            cell_fall: None,
            rise_transition: None,
            fall_transition: None,
            setup: None,
            hold: None,
        }
    }

    /// Create a new sequential timing arc (clock to Q)
    pub fn sequential(clock_pin: &str, q_pin: &str, rising_edge: bool) -> Self {
        Self {
            from_pin: clock_pin.to_string(),
            to_pin: q_pin.to_string(),
            sense: TimingSense::NonUnate,
            timing_type: if rising_edge {
                TimingType::RisingEdge
            } else {
                TimingType::FallingEdge
            },
            cell_rise: None,
            cell_fall: None,
            rise_transition: None,
            fall_transition: None,
            setup: None,
            hold: None,
        }
    }

    /// Set the timing sense
    pub fn with_sense(mut self, sense: TimingSense) -> Self {
        self.sense = sense;
        self
    }

    /// Set rise/fall delays
    pub fn with_delays(mut self, rise: NldmTable, fall: NldmTable) -> Self {
        self.cell_rise = Some(rise);
        self.cell_fall = Some(fall);
        self
    }

    /// Set rise/fall transitions
    pub fn with_transitions(mut self, rise: NldmTable, fall: NldmTable) -> Self {
        self.rise_transition = Some(rise);
        self.fall_transition = Some(fall);
        self
    }

    /// Get the delay for a given transition
    pub fn get_delay(
        &self,
        rising: bool,
        input_slew: TimePs,
        output_load: CapacitanceFf,
    ) -> TimePs {
        let table = if rising {
            self.cell_rise.as_ref()
        } else {
            self.cell_fall.as_ref()
        };

        table
            .map(|t| t.lookup(input_slew, output_load))
            .unwrap_or(0.0)
    }

    /// Get the output slew for a given transition
    pub fn get_transition(
        &self,
        rising: bool,
        input_slew: TimePs,
        output_load: CapacitanceFf,
    ) -> TimePs {
        let table = if rising {
            self.rise_transition.as_ref()
        } else {
            self.fall_transition.as_ref()
        };

        table
            .map(|t| t.lookup(input_slew, output_load))
            .unwrap_or(input_slew) // Default: preserve input slew
    }
}

/// Pin direction
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PinDirection {
    Input,
    Output,
    Inout,
    Internal,
}

/// Pin timing information
#[derive(Debug, Clone)]
pub struct PinTiming {
    /// Pin name
    pub name: String,
    /// Pin direction
    pub direction: PinDirection,
    /// Input capacitance (for input pins)
    pub capacitance: Option<CapacitanceFf>,
    /// Maximum capacitance (for output pins)
    pub max_capacitance: Option<CapacitanceFf>,
    /// Maximum transition (slew)
    pub max_transition: Option<TimePs>,
    /// Is this a clock pin?
    pub is_clock: bool,
}

impl PinTiming {
    /// Create a new input pin
    pub fn input(name: &str, capacitance: CapacitanceFf) -> Self {
        Self {
            name: name.to_string(),
            direction: PinDirection::Input,
            capacitance: Some(capacitance),
            max_capacitance: None,
            max_transition: None,
            is_clock: false,
        }
    }

    /// Create a new output pin
    pub fn output(name: &str) -> Self {
        Self {
            name: name.to_string(),
            direction: PinDirection::Output,
            capacitance: None,
            max_capacitance: None,
            max_transition: None,
            is_clock: false,
        }
    }

    /// Create a new clock pin
    pub fn clock(name: &str, capacitance: CapacitanceFf) -> Self {
        Self {
            name: name.to_string(),
            direction: PinDirection::Input,
            capacitance: Some(capacitance),
            max_capacitance: None,
            max_transition: None,
            is_clock: true,
        }
    }
}

/// Cell timing model
#[derive(Debug, Clone)]
pub struct CellTiming {
    /// Cell name
    pub name: String,
    /// Cell area (in library units)
    pub area: f64,
    /// Pin information
    pub pins: IndexMap<String, PinTiming>,
    /// Timing arcs
    pub timing_arcs: Vec<TimingArc>,
    /// Is this a sequential cell?
    pub is_sequential: bool,
    /// Leakage power (optional)
    pub leakage_power: Option<f64>,
}

impl CellTiming {
    /// Create a new cell timing model
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            area: 0.0,
            pins: IndexMap::new(),
            timing_arcs: Vec::new(),
            is_sequential: false,
            leakage_power: None,
        }
    }

    /// Set cell area
    pub fn with_area(mut self, area: f64) -> Self {
        self.area = area;
        self
    }

    /// Add a pin
    pub fn add_pin(&mut self, pin: PinTiming) {
        self.pins.insert(pin.name.clone(), pin);
    }

    /// Add a timing arc
    pub fn add_timing_arc(&mut self, arc: TimingArc) {
        // Check if this makes it sequential
        if matches!(
            arc.timing_type,
            TimingType::RisingEdge | TimingType::FallingEdge
        ) {
            self.is_sequential = true;
        }
        self.timing_arcs.push(arc);
    }

    /// Get timing arcs from a specific pin
    pub fn arcs_from_pin(&self, pin: &str) -> Vec<&TimingArc> {
        self.timing_arcs
            .iter()
            .filter(|arc| arc.from_pin == pin)
            .collect()
    }

    /// Get timing arcs to a specific pin
    pub fn arcs_to_pin(&self, pin: &str) -> Vec<&TimingArc> {
        self.timing_arcs
            .iter()
            .filter(|arc| arc.to_pin == pin)
            .collect()
    }

    /// Get the total input capacitance
    pub fn total_input_capacitance(&self) -> CapacitanceFf {
        self.pins
            .values()
            .filter(|p| p.direction == PinDirection::Input)
            .filter_map(|p| p.capacitance)
            .sum()
    }
}

/// Timing information for a net in the design
#[derive(Debug, Clone, Default)]
pub struct NetTiming {
    /// Arrival time (latest signal arrival)
    pub arrival_rise: TimePs,
    pub arrival_fall: TimePs,
    /// Required time (latest allowed arrival)
    pub required_rise: TimePs,
    pub required_fall: TimePs,
    /// Input slew
    pub slew_rise: TimePs,
    pub slew_fall: TimePs,
    /// Capacitive load
    pub load: CapacitanceFf,
}

impl NetTiming {
    /// Create new net timing with zero values
    pub fn new() -> Self {
        Self::default()
    }

    /// Get the worst-case slack (minimum of rise and fall)
    pub fn slack(&self) -> TimePs {
        let rise_slack = self.required_rise - self.arrival_rise;
        let fall_slack = self.required_fall - self.arrival_fall;
        rise_slack.min(fall_slack)
    }

    /// Get the worst-case arrival time
    pub fn arrival(&self) -> TimePs {
        self.arrival_rise.max(self.arrival_fall)
    }

    /// Get the worst-case required time
    pub fn required(&self) -> TimePs {
        self.required_rise.min(self.required_fall)
    }

    /// Get the worst-case slew
    pub fn slew(&self) -> TimePs {
        self.slew_rise.max(self.slew_fall)
    }

    /// Check if this is a critical path (negative slack)
    pub fn is_critical(&self) -> bool {
        self.slack() < 0.0
    }

    /// Check if timing is met
    pub fn is_met(&self) -> bool {
        self.slack() >= 0.0
    }
}

/// Operating conditions
#[derive(Debug, Clone)]
pub struct OperatingConditions {
    /// Process corner (typical, slow, fast)
    pub process: ProcessCorner,
    /// Temperature in Celsius
    pub temperature: f64,
    /// Supply voltage
    pub voltage: f64,
}

/// Process corner
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ProcessCorner {
    Typical,
    Slow,
    Fast,
    SlowSlow, // Slow NMOS, Slow PMOS
    FastFast, // Fast NMOS, Fast PMOS
    SlowFast, // Slow NMOS, Fast PMOS
    FastSlow, // Fast NMOS, Slow PMOS
}

impl Default for OperatingConditions {
    fn default() -> Self {
        Self {
            process: ProcessCorner::Typical,
            temperature: 25.0,
            voltage: 1.0,
        }
    }
}

impl OperatingConditions {
    /// Create typical operating conditions
    pub fn typical() -> Self {
        Self::default()
    }

    /// Create slow corner (worst case for setup)
    pub fn slow() -> Self {
        Self {
            process: ProcessCorner::Slow,
            temperature: 125.0, // Hot
            voltage: 0.9,       // Low voltage
        }
    }

    /// Create fast corner (worst case for hold)
    pub fn fast() -> Self {
        Self {
            process: ProcessCorner::Fast,
            temperature: -40.0, // Cold
            voltage: 1.1,       // High voltage
        }
    }
}

/// Timing constraints
#[derive(Debug, Clone)]
pub struct TimingConstraints {
    /// Clock definitions
    pub clocks: IndexMap<String, ClockDefinition>,
    /// Input delays
    pub input_delays: IndexMap<String, InputDelay>,
    /// Output delays
    pub output_delays: IndexMap<String, OutputDelay>,
    /// False paths
    pub false_paths: Vec<FalsePath>,
    /// Multi-cycle paths
    pub multicycle_paths: Vec<MulticyclePath>,
}

impl Default for TimingConstraints {
    fn default() -> Self {
        Self::new()
    }
}

impl TimingConstraints {
    /// Create empty timing constraints
    pub fn new() -> Self {
        Self {
            clocks: IndexMap::new(),
            input_delays: IndexMap::new(),
            output_delays: IndexMap::new(),
            false_paths: Vec::new(),
            multicycle_paths: Vec::new(),
        }
    }

    /// Add a clock definition
    pub fn add_clock(&mut self, name: &str, period: TimePs) {
        self.clocks.insert(
            name.to_string(),
            ClockDefinition {
                name: name.to_string(),
                period,
                waveform: (0.0, period / 2.0),
                source: None,
            },
        );
    }

    /// Add an input delay constraint
    pub fn add_input_delay(&mut self, port: &str, clock: &str, delay: TimePs) {
        self.input_delays.insert(
            port.to_string(),
            InputDelay {
                port: port.to_string(),
                clock: clock.to_string(),
                delay,
            },
        );
    }

    /// Add an output delay constraint
    pub fn add_output_delay(&mut self, port: &str, clock: &str, delay: TimePs) {
        self.output_delays.insert(
            port.to_string(),
            OutputDelay {
                port: port.to_string(),
                clock: clock.to_string(),
                delay,
            },
        );
    }
}

/// Clock definition
#[derive(Debug, Clone)]
pub struct ClockDefinition {
    /// Clock name
    pub name: String,
    /// Clock period (ps)
    pub period: TimePs,
    /// Waveform (rise_time, fall_time)
    pub waveform: (TimePs, TimePs),
    /// Source port/pin (for generated clocks)
    pub source: Option<String>,
}

/// Input delay constraint
#[derive(Debug, Clone)]
pub struct InputDelay {
    /// Port name
    pub port: String,
    /// Reference clock
    pub clock: String,
    /// Delay value (ps)
    pub delay: TimePs,
}

/// Output delay constraint
#[derive(Debug, Clone)]
pub struct OutputDelay {
    /// Port name
    pub port: String,
    /// Reference clock
    pub clock: String,
    /// Delay value (ps)
    pub delay: TimePs,
}

/// False path specification
#[derive(Debug, Clone)]
pub struct FalsePath {
    /// Starting point(s)
    pub from: Vec<String>,
    /// Ending point(s)
    pub to: Vec<String>,
}

/// Multi-cycle path specification
#[derive(Debug, Clone)]
pub struct MulticyclePath {
    /// Starting point(s)
    pub from: Vec<String>,
    /// Ending point(s)
    pub to: Vec<String>,
    /// Number of cycles
    pub cycles: u32,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_nldm_constant() {
        let table = NldmTable::constant(10.0);
        assert_eq!(table.lookup(0.0, 0.0), 10.0);
        assert_eq!(table.lookup(100.0, 50.0), 10.0);
    }

    #[test]
    fn test_nldm_interpolation() {
        let table = NldmTable::new(
            vec![0.0, 100.0],
            vec![0.0, 10.0],
            vec![vec![1.0, 2.0], vec![3.0, 4.0]],
        );

        // Corner values
        assert_eq!(table.lookup(0.0, 0.0), 1.0);
        assert_eq!(table.lookup(0.0, 10.0), 2.0);
        assert_eq!(table.lookup(100.0, 0.0), 3.0);
        assert_eq!(table.lookup(100.0, 10.0), 4.0);

        // Midpoint
        let mid = table.lookup(50.0, 5.0);
        assert!((mid - 2.5).abs() < 0.001);
    }

    #[test]
    fn test_nldm_extrapolation() {
        let table = NldmTable::new(
            vec![10.0, 20.0],
            vec![5.0, 15.0],
            vec![vec![1.0, 2.0], vec![3.0, 4.0]],
        );

        // Below range - should clamp to edge
        let below = table.lookup(0.0, 0.0);
        assert_eq!(below, 1.0);

        // Above range - should clamp to edge
        let above = table.lookup(100.0, 100.0);
        assert_eq!(above, 4.0);
    }

    #[test]
    fn test_timing_arc_creation() {
        let arc = TimingArc::combinational("A", "Y")
            .with_sense(TimingSense::PositiveUnate)
            .with_delays(NldmTable::constant(5.0), NldmTable::constant(6.0));

        assert_eq!(arc.from_pin, "A");
        assert_eq!(arc.to_pin, "Y");
        assert_eq!(arc.sense, TimingSense::PositiveUnate);
        assert_eq!(arc.get_delay(true, 0.0, 0.0), 5.0);
        assert_eq!(arc.get_delay(false, 0.0, 0.0), 6.0);
    }

    #[test]
    fn test_cell_timing() {
        let mut cell = CellTiming::new("INV_X1").with_area(1.0);

        cell.add_pin(PinTiming::input("A", 2.0));
        cell.add_pin(PinTiming::output("Y"));

        let arc = TimingArc::combinational("A", "Y")
            .with_sense(TimingSense::NegativeUnate)
            .with_delays(NldmTable::constant(10.0), NldmTable::constant(12.0));
        cell.add_timing_arc(arc);

        assert!(!cell.is_sequential);
        assert_eq!(cell.pins.len(), 2);
        assert_eq!(cell.timing_arcs.len(), 1);
        assert_eq!(cell.total_input_capacitance(), 2.0);
    }

    #[test]
    fn test_sequential_cell() {
        let mut cell = CellTiming::new("DFF_X1");

        cell.add_pin(PinTiming::clock("CK", 3.0));
        cell.add_pin(PinTiming::input("D", 2.0));
        cell.add_pin(PinTiming::output("Q"));

        let arc = TimingArc::sequential("CK", "Q", true)
            .with_delays(NldmTable::constant(50.0), NldmTable::constant(55.0));
        cell.add_timing_arc(arc);

        assert!(cell.is_sequential);
    }

    #[test]
    fn test_net_timing() {
        let mut timing = NetTiming::new();
        timing.arrival_rise = 100.0;
        timing.arrival_fall = 110.0;
        timing.required_rise = 150.0;
        timing.required_fall = 145.0;

        assert_eq!(timing.arrival(), 110.0); // Max of rise/fall
        assert_eq!(timing.required(), 145.0); // Min of rise/fall
        assert_eq!(timing.slack(), 35.0); // 145 - 110
        assert!(timing.is_met());
    }

    #[test]
    fn test_net_timing_violated() {
        let mut timing = NetTiming::new();
        timing.arrival_rise = 200.0;
        timing.arrival_fall = 190.0;
        timing.required_rise = 150.0;
        timing.required_fall = 145.0;

        assert!(timing.slack() < 0.0);
        assert!(timing.is_critical());
        assert!(!timing.is_met());
    }

    #[test]
    fn test_timing_constraints() {
        let mut constraints = TimingConstraints::new();
        constraints.add_clock("clk", 10000.0); // 10ns = 100MHz
        constraints.add_input_delay("data_in", "clk", 2000.0);
        constraints.add_output_delay("data_out", "clk", 3000.0);

        assert_eq!(constraints.clocks["clk"].period, 10000.0);
        assert_eq!(constraints.input_delays["data_in"].delay, 2000.0);
        assert_eq!(constraints.output_delays["data_out"].delay, 3000.0);
    }

    #[test]
    fn test_operating_conditions() {
        let typical = OperatingConditions::typical();
        assert_eq!(typical.temperature, 25.0);

        let slow = OperatingConditions::slow();
        assert_eq!(slow.temperature, 125.0);
        assert_eq!(slow.voltage, 0.9);

        let fast = OperatingConditions::fast();
        assert_eq!(fast.temperature, -40.0);
        assert_eq!(fast.voltage, 1.1);
    }
}
