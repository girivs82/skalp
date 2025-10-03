//! SDC (Synopsys Design Constraints) Support
//!
//! Industry-standard constraint format for timing and design rules

use crate::AsicError;
use std::collections::{HashMap, HashSet};
use std::fs::File;
use std::io::{BufRead, BufReader, Write};
use std::path::Path;

/// SDC constraint manager
pub struct SDCManager {
    /// Clock definitions
    pub clocks: Vec<ClockDefinition>,
    /// Generated clocks
    pub generated_clocks: Vec<GeneratedClock>,
    /// Input delays
    pub input_delays: Vec<InputDelay>,
    /// Output delays
    pub output_delays: Vec<OutputDelay>,
    /// False paths
    pub false_paths: Vec<FalsePath>,
    /// Multicycle paths
    pub multicycle_paths: Vec<MulticyclePath>,
    /// Max delay constraints
    pub max_delays: Vec<MaxDelay>,
    /// Min delay constraints
    pub min_delays: Vec<MinDelay>,
    /// Case analysis
    pub case_analysis: Vec<CaseAnalysis>,
    /// Disable timing arcs
    pub disabled_arcs: Vec<DisabledArc>,
    /// Operating conditions
    pub operating_conditions: OperatingConditions,
    /// Design rules
    pub design_rules: DesignRules,
    /// Exceptions
    pub exceptions: Vec<TimingException>,
    /// Variables
    pub variables: HashMap<String, String>,
}

/// Clock definition
#[derive(Debug, Clone)]
pub struct ClockDefinition {
    /// Clock name
    pub name: String,
    /// Period in nanoseconds
    pub period: f64,
    /// Waveform [rise, fall]
    pub waveform: Vec<f64>,
    /// Clock source pin/port
    pub source: String,
    /// Clock sense (positive/negative)
    pub sense: ClockSense,
    /// Uncertainty
    pub uncertainty: Option<ClockUncertainty>,
    /// Latency
    pub latency: Option<ClockLatency>,
    /// Transition
    pub transition: Option<f64>,
}

/// Clock sense
#[derive(Debug, Clone)]
pub enum ClockSense {
    Positive,
    Negative,
    Stop,
}

/// Clock uncertainty
#[derive(Debug, Clone)]
pub struct ClockUncertainty {
    /// Setup uncertainty
    pub setup: f64,
    /// Hold uncertainty
    pub hold: f64,
    /// From clock (for inter-clock)
    pub from_clock: Option<String>,
    /// To clock (for inter-clock)
    pub to_clock: Option<String>,
}

/// Clock latency
#[derive(Debug, Clone)]
pub struct ClockLatency {
    /// Source latency
    pub source: f64,
    /// Network latency
    pub network: f64,
    /// Early/late values
    pub early: Option<f64>,
    pub late: Option<f64>,
}

/// Generated clock
#[derive(Debug, Clone)]
pub struct GeneratedClock {
    /// Clock name
    pub name: String,
    /// Master clock
    pub master_clock: String,
    /// Master pin
    pub master_pin: String,
    /// Source pin
    pub source: String,
    /// Divide by
    pub divide_by: Option<u32>,
    /// Multiply by
    pub multiply_by: Option<u32>,
    /// Duty cycle
    pub duty_cycle: Option<f64>,
    /// Invert
    pub invert: bool,
    /// Edge shift
    pub edges: Option<Vec<f64>>,
}

/// Input delay constraint
#[derive(Debug, Clone)]
pub struct InputDelay {
    /// Delay value
    pub delay: f64,
    /// Port/pin name
    pub port: String,
    /// Reference clock
    pub clock: String,
    /// Clock edge (rise/fall)
    pub clock_edge: Option<ClockEdge>,
    /// Min delay
    pub min: Option<f64>,
    /// Max delay
    pub max: Option<f64>,
    /// Add delay
    pub add_delay: bool,
}

/// Output delay constraint
#[derive(Debug, Clone)]
pub struct OutputDelay {
    /// Delay value
    pub delay: f64,
    /// Port/pin name
    pub port: String,
    /// Reference clock
    pub clock: String,
    /// Clock edge
    pub clock_edge: Option<ClockEdge>,
    /// Min delay
    pub min: Option<f64>,
    /// Max delay
    pub max: Option<f64>,
    /// Add delay
    pub add_delay: bool,
}

/// Clock edge specification
#[derive(Debug, Clone)]
pub enum ClockEdge {
    Rise,
    Fall,
    Both,
}

/// False path specification
#[derive(Debug, Clone)]
pub struct FalsePath {
    /// From points
    pub from: Vec<String>,
    /// Through points
    pub through: Vec<String>,
    /// To points
    pub to: Vec<String>,
    /// Setup only
    pub setup: bool,
    /// Hold only
    pub hold: bool,
    /// Comment
    pub comment: Option<String>,
}

/// Multicycle path
#[derive(Debug, Clone)]
pub struct MulticyclePath {
    /// Path multiplier
    pub path_multiplier: u32,
    /// From points
    pub from: Vec<String>,
    /// Through points
    pub through: Vec<String>,
    /// To points
    pub to: Vec<String>,
    /// Setup multiplier
    pub setup: Option<u32>,
    /// Hold multiplier
    pub hold: Option<u32>,
    /// Start/end specification
    pub start: bool,
    pub end: bool,
}

/// Max delay constraint
#[derive(Debug, Clone)]
pub struct MaxDelay {
    /// Delay value
    pub delay: f64,
    /// From points
    pub from: Vec<String>,
    /// Through points
    pub through: Vec<String>,
    /// To points
    pub to: Vec<String>,
    /// Ignore clock latency
    pub ignore_clock_latency: bool,
}

/// Min delay constraint
#[derive(Debug, Clone)]
pub struct MinDelay {
    /// Delay value
    pub delay: f64,
    /// From points
    pub from: Vec<String>,
    /// Through points
    pub through: Vec<String>,
    /// To points
    pub to: Vec<String>,
    /// Ignore clock latency
    pub ignore_clock_latency: bool,
}

/// Case analysis
#[derive(Debug, Clone)]
pub struct CaseAnalysis {
    /// Value (0 or 1)
    pub value: bool,
    /// Pin/port
    pub pin: String,
    /// Propagate through logic
    pub propagate: bool,
}

/// Disabled timing arc
#[derive(Debug, Clone)]
pub struct DisabledArc {
    /// From pin
    pub from: String,
    /// To pin
    pub to: String,
    /// Cell instance
    pub cell: Option<String>,
}

/// Operating conditions
#[derive(Debug, Clone)]
pub struct OperatingConditions {
    /// Process corner
    pub process: ProcessCorner,
    /// Voltage
    pub voltage: f64,
    /// Temperature
    pub temperature: f64,
    /// Library name
    pub library: Option<String>,
}

/// Process corner
#[derive(Debug, Clone)]
pub enum ProcessCorner {
    Typical,
    Fast,
    Slow,
    FastSlow,
    SlowFast,
}

/// Design rules
#[derive(Debug, Clone)]
pub struct DesignRules {
    /// Max fanout
    pub max_fanout: Option<f64>,
    /// Max transition
    pub max_transition: Option<f64>,
    /// Max capacitance
    pub max_capacitance: Option<f64>,
    /// Min capacitance
    pub min_capacitance: Option<f64>,
    /// Max area
    pub max_area: Option<f64>,
}

/// Timing exception
#[derive(Debug, Clone)]
pub struct TimingException {
    /// Exception type
    pub exception_type: ExceptionType,
    /// Objects
    pub objects: Vec<String>,
    /// Value
    pub value: Option<f64>,
}

/// Exception types
#[derive(Debug, Clone)]
pub enum ExceptionType {
    GroupPath,
    IdealNetwork,
    IdealLatency,
    PropagatedClock,
}

impl SDCManager {
    /// Create new SDC manager
    pub fn new() -> Self {
        Self {
            clocks: Vec::new(),
            generated_clocks: Vec::new(),
            input_delays: Vec::new(),
            output_delays: Vec::new(),
            false_paths: Vec::new(),
            multicycle_paths: Vec::new(),
            max_delays: Vec::new(),
            min_delays: Vec::new(),
            case_analysis: Vec::new(),
            disabled_arcs: Vec::new(),
            operating_conditions: OperatingConditions::default(),
            design_rules: DesignRules::default(),
            exceptions: Vec::new(),
            variables: HashMap::new(),
        }
    }

    /// Parse SDC file
    pub fn parse_file(&mut self, path: &Path) -> Result<(), AsicError> {
        let file = File::open(path)
            .map_err(|e| AsicError::TechnologyError(format!("Failed to open SDC: {}", e)))?;
        let reader = BufReader::new(file);

        let mut line_num = 0;
        for line in reader.lines() {
            line_num += 1;
            let line =
                line.map_err(|e| AsicError::TechnologyError(format!("Read error: {}", e)))?;

            // Skip comments and empty lines
            let line = line.trim();
            if line.is_empty() || line.starts_with('#') {
                continue;
            }

            // Parse command
            self.parse_command(&line, line_num)?;
        }

        Ok(())
    }

    /// Parse SDC command
    fn parse_command(&mut self, line: &str, line_num: usize) -> Result<(), AsicError> {
        let parts: Vec<&str> = line.split_whitespace().collect();
        if parts.is_empty() {
            return Ok(());
        }

        match parts[0] {
            "create_clock" => self.parse_create_clock(&parts[1..])?,
            "create_generated_clock" => self.parse_generated_clock(&parts[1..])?,
            "set_input_delay" => self.parse_input_delay(&parts[1..])?,
            "set_output_delay" => self.parse_output_delay(&parts[1..])?,
            "set_false_path" => self.parse_false_path(&parts[1..])?,
            "set_multicycle_path" => self.parse_multicycle_path(&parts[1..])?,
            "set_max_delay" => self.parse_max_delay(&parts[1..])?,
            "set_min_delay" => self.parse_min_delay(&parts[1..])?,
            "set_case_analysis" => self.parse_case_analysis(&parts[1..])?,
            "set_disable_timing" => self.parse_disable_timing(&parts[1..])?,
            "set_clock_uncertainty" => self.parse_clock_uncertainty(&parts[1..])?,
            "set_clock_latency" => self.parse_clock_latency(&parts[1..])?,
            "set" => self.parse_variable(&parts[1..])?,
            _ => {
                // Unknown command - could log warning
            }
        }

        Ok(())
    }

    /// Parse create_clock command
    fn parse_create_clock(&mut self, args: &[&str]) -> Result<(), AsicError> {
        let mut name = String::new();
        let mut period = 0.0;
        let mut waveform = vec![0.0, 0.0];
        let mut source = String::new();

        let mut i = 0;
        while i < args.len() {
            match args[i] {
                "-name" => {
                    i += 1;
                    if i < args.len() {
                        name = args[i].to_string();
                    }
                }
                "-period" => {
                    i += 1;
                    if i < args.len() {
                        period = args[i].parse().unwrap_or(0.0);
                        waveform[1] = period / 2.0;
                    }
                }
                "-waveform" => {
                    i += 1;
                    if i + 1 < args.len() {
                        waveform[0] = args[i].parse().unwrap_or(0.0);
                        waveform[1] = args[i + 1].parse().unwrap_or(period / 2.0);
                        i += 1;
                    }
                }
                _ => {
                    if !args[i].starts_with('-') {
                        source = args[i].to_string();
                    }
                }
            }
            i += 1;
        }

        if name.is_empty() {
            name = source.clone();
        }

        self.clocks.push(ClockDefinition {
            name,
            period,
            waveform,
            source,
            sense: ClockSense::Positive,
            uncertainty: None,
            latency: None,
            transition: None,
        });

        Ok(())
    }

    /// Parse other commands (stubs)
    fn parse_generated_clock(&mut self, args: &[&str]) -> Result<(), AsicError> {
        // Parse: create_generated_clock -name <name> -source <source> [-divide_by <div>] [-multiply_by <mult>] [-invert] <pin>
        let mut name = String::new();
        let mut source = String::new();
        let mut master_clock = String::new();
        let mut master_pin = String::new();
        let mut divide_by = None;
        let mut multiply_by = None;
        let mut invert = false;

        let mut i = 0;
        while i < args.len() {
            match args[i] {
                "-name" => {
                    if i + 1 < args.len() {
                        name = args[i + 1].to_string();
                        i += 2;
                    } else {
                        i += 1;
                    }
                }
                "-source" => {
                    if i + 1 < args.len() {
                        master_pin = args[i + 1].to_string();
                        i += 2;
                    } else {
                        i += 1;
                    }
                }
                "-divide_by" => {
                    if i + 1 < args.len() && args[i + 1].parse::<u32>().is_ok() {
                        divide_by = Some(args[i + 1].parse().unwrap());
                        i += 2;
                    } else {
                        i += 1;
                    }
                }
                "-multiply_by" => {
                    if i + 1 < args.len() && args[i + 1].parse::<u32>().is_ok() {
                        multiply_by = Some(args[i + 1].parse().unwrap());
                        i += 2;
                    } else {
                        i += 1;
                    }
                }
                "-invert" => {
                    invert = true;
                    i += 1;
                }
                _ => {
                    if source.is_empty() {
                        source = args[i].to_string();
                    }
                    i += 1;
                }
            }
        }

        if !name.is_empty() && !source.is_empty() {
            self.generated_clocks.push(GeneratedClock {
                name,
                master_clock,
                master_pin,
                divide_by,
                multiply_by,
                invert,
                source,
                duty_cycle: None,
                edges: None,
            });
        }

        Ok(())
    }

    fn parse_input_delay(&mut self, args: &[&str]) -> Result<(), AsicError> {
        // Parse: set_input_delay -clock <clock> <delay> [get_ports <ports>]
        let mut delay = 0.0;
        let mut clock = String::new();
        let mut port = String::new();

        let mut i = 0;
        while i < args.len() {
            match args[i] {
                "-clock" => {
                    if i + 1 < args.len() {
                        clock = args[i + 1].to_string();
                        i += 2;
                    } else {
                        return Err(AsicError::TechnologyError("Missing clock name".to_string()));
                    }
                }
                arg if arg.parse::<f64>().is_ok() => {
                    delay = arg.parse().unwrap();
                    i += 1;
                }
                _ if args[i].starts_with("[get_ports") => {
                    // Extract port name from [get_ports {port_name}]
                    if i + 1 < args.len() {
                        port = args[i + 1].trim_matches(&['{', '}', ']'][..]).to_string();
                        i += 2;
                    } else {
                        i += 1;
                    }
                }
                _ => i += 1,
            }
        }

        if !clock.is_empty() && !port.is_empty() {
            self.input_delays.push(InputDelay {
                delay,
                clock,
                port,
                clock_edge: None,
                min: None,
                max: None,
                add_delay: false,
            });
        }

        Ok(())
    }

    fn parse_output_delay(&mut self, args: &[&str]) -> Result<(), AsicError> {
        // Parse: set_output_delay -clock <clock> <delay> [get_ports <ports>]
        let mut delay = 0.0;
        let mut clock = String::new();
        let mut port = String::new();

        let mut i = 0;
        while i < args.len() {
            match args[i] {
                "-clock" => {
                    if i + 1 < args.len() {
                        clock = args[i + 1].to_string();
                        i += 2;
                    } else {
                        return Err(AsicError::TechnologyError("Missing clock name".to_string()));
                    }
                }
                arg if arg.parse::<f64>().is_ok() => {
                    delay = arg.parse().unwrap();
                    i += 1;
                }
                _ if args[i].starts_with("[get_ports") => {
                    // Extract port name from [get_ports {port_name}]
                    if i + 1 < args.len() {
                        port = args[i + 1].trim_matches(&['{', '}', ']'][..]).to_string();
                        i += 2;
                    } else {
                        i += 1;
                    }
                }
                _ => i += 1,
            }
        }

        if !clock.is_empty() && !port.is_empty() {
            self.output_delays.push(OutputDelay {
                delay,
                clock,
                port,
                clock_edge: None,
                min: None,
                max: None,
                add_delay: false,
            });
        }

        Ok(())
    }

    fn parse_false_path(&mut self, args: &[&str]) -> Result<(), AsicError> {
        let mut from = Vec::new();
        let mut through = Vec::new();
        let mut to = Vec::new();

        let mut i = 0;
        while i < args.len() {
            match args[i] {
                "-from" => {
                    i += 1;
                    while i < args.len() && !args[i].starts_with('-') {
                        from.push(args[i].to_string());
                        i += 1;
                    }
                    i -= 1;
                }
                "-through" => {
                    i += 1;
                    while i < args.len() && !args[i].starts_with('-') {
                        through.push(args[i].to_string());
                        i += 1;
                    }
                    i -= 1;
                }
                "-to" => {
                    i += 1;
                    while i < args.len() && !args[i].starts_with('-') {
                        to.push(args[i].to_string());
                        i += 1;
                    }
                    i -= 1;
                }
                _ => {}
            }
            i += 1;
        }

        self.false_paths.push(FalsePath {
            from,
            through,
            to,
            setup: true,
            hold: true,
            comment: None,
        });

        Ok(())
    }

    fn parse_multicycle_path(&mut self, _args: &[&str]) -> Result<(), AsicError> {
        // TODO: Implement
        Ok(())
    }

    fn parse_max_delay(&mut self, _args: &[&str]) -> Result<(), AsicError> {
        // TODO: Implement
        Ok(())
    }

    fn parse_min_delay(&mut self, _args: &[&str]) -> Result<(), AsicError> {
        // TODO: Implement
        Ok(())
    }

    fn parse_case_analysis(&mut self, _args: &[&str]) -> Result<(), AsicError> {
        // TODO: Implement
        Ok(())
    }

    fn parse_disable_timing(&mut self, _args: &[&str]) -> Result<(), AsicError> {
        // TODO: Implement
        Ok(())
    }

    fn parse_clock_uncertainty(&mut self, _args: &[&str]) -> Result<(), AsicError> {
        // TODO: Implement
        Ok(())
    }

    fn parse_clock_latency(&mut self, _args: &[&str]) -> Result<(), AsicError> {
        // TODO: Implement
        Ok(())
    }

    fn parse_variable(&mut self, args: &[&str]) -> Result<(), AsicError> {
        if args.len() >= 2 {
            self.variables
                .insert(args[0].to_string(), args[1].to_string());
        }
        Ok(())
    }

    /// Generate SDC file
    pub fn write_file(&self, path: &Path) -> Result<(), AsicError> {
        let mut file = File::create(path)
            .map_err(|e| AsicError::TechnologyError(format!("Failed to create SDC: {}", e)))?;

        writeln!(file, "# SDC Constraints Generated by SKALP")?;
        writeln!(
            file,
            "# Date: {}",
            chrono::Local::now().format("%Y-%m-%d %H:%M:%S")
        )?;
        writeln!(file)?;

        // Write operating conditions
        writeln!(file, "# Operating Conditions")?;
        writeln!(
            file,
            "set_operating_conditions -library {} \\",
            self.operating_conditions
                .library
                .as_ref()
                .unwrap_or(&"default".to_string())
        )?;
        writeln!(
            file,
            "  -process {:?} -voltage {} -temperature {}",
            self.operating_conditions.process,
            self.operating_conditions.voltage,
            self.operating_conditions.temperature
        )?;
        writeln!(file)?;

        // Write clocks
        writeln!(file, "# Clock Definitions")?;
        for clock in &self.clocks {
            writeln!(
                file,
                "create_clock -name {} -period {:.3} \\",
                clock.name, clock.period
            )?;
            writeln!(
                file,
                "  -waveform {{{:.3} {:.3}}} [get_ports {}]",
                clock.waveform[0], clock.waveform[1], clock.source
            )?;

            if let Some(ref unc) = clock.uncertainty {
                writeln!(
                    file,
                    "set_clock_uncertainty -setup {:.3} [get_clocks {}]",
                    unc.setup, clock.name
                )?;
                writeln!(
                    file,
                    "set_clock_uncertainty -hold {:.3} [get_clocks {}]",
                    unc.hold, clock.name
                )?;
            }

            if let Some(ref lat) = clock.latency {
                writeln!(
                    file,
                    "set_clock_latency -source {:.3} [get_clocks {}]",
                    lat.source, clock.name
                )?;
                writeln!(
                    file,
                    "set_clock_latency {:.3} [get_clocks {}]",
                    lat.network, clock.name
                )?;
            }
        }
        writeln!(file)?;

        // Write generated clocks
        if !self.generated_clocks.is_empty() {
            writeln!(file, "# Generated Clocks")?;
            for gclock in &self.generated_clocks {
                write!(file, "create_generated_clock -name {} ", gclock.name)?;
                if let Some(div) = gclock.divide_by {
                    write!(file, "-divide_by {} ", div)?;
                }
                if let Some(mult) = gclock.multiply_by {
                    write!(file, "-multiply_by {} ", mult)?;
                }
                if gclock.invert {
                    write!(file, "-invert ")?;
                }
                writeln!(file, "-source {} \\", gclock.master_pin)?;
                writeln!(
                    file,
                    "  -master_clock {} [get_pins {}]",
                    gclock.master_clock, gclock.source
                )?;
            }
            writeln!(file)?;
        }

        // Write input delays
        if !self.input_delays.is_empty() {
            writeln!(file, "# Input Delays")?;
            for delay in &self.input_delays {
                writeln!(
                    file,
                    "set_input_delay {:.3} -clock {} [get_ports {}]",
                    delay.delay, delay.clock, delay.port
                )?;
            }
            writeln!(file)?;
        }

        // Write output delays
        if !self.output_delays.is_empty() {
            writeln!(file, "# Output Delays")?;
            for delay in &self.output_delays {
                writeln!(
                    file,
                    "set_output_delay {:.3} -clock {} [get_ports {}]",
                    delay.delay, delay.clock, delay.port
                )?;
            }
            writeln!(file)?;
        }

        // Write false paths
        if !self.false_paths.is_empty() {
            writeln!(file, "# False Paths")?;
            for fpath in &self.false_paths {
                write!(file, "set_false_path")?;
                if !fpath.from.is_empty() {
                    write!(file, " -from [get_pins {{{}}}]", fpath.from.join(" "))?;
                }
                if !fpath.through.is_empty() {
                    write!(file, " -through [get_pins {{{}}}]", fpath.through.join(" "))?;
                }
                if !fpath.to.is_empty() {
                    write!(file, " -to [get_pins {{{}}}]", fpath.to.join(" "))?;
                }
                writeln!(file)?;
            }
            writeln!(file)?;
        }

        // Write multicycle paths
        if !self.multicycle_paths.is_empty() {
            writeln!(file, "# Multicycle Paths")?;
            for mpath in &self.multicycle_paths {
                write!(file, "set_multicycle_path {}", mpath.path_multiplier)?;
                if !mpath.from.is_empty() {
                    write!(file, " -from [get_pins {{{}}}]", mpath.from.join(" "))?;
                }
                if !mpath.to.is_empty() {
                    write!(file, " -to [get_pins {{{}}}]", mpath.to.join(" "))?;
                }
                writeln!(file)?;
            }
            writeln!(file)?;
        }

        // Write design rules
        writeln!(file, "# Design Rules")?;
        if let Some(fanout) = self.design_rules.max_fanout {
            writeln!(file, "set_max_fanout {:.2} [current_design]", fanout)?;
        }
        if let Some(trans) = self.design_rules.max_transition {
            writeln!(file, "set_max_transition {:.3} [current_design]", trans)?;
        }
        if let Some(cap) = self.design_rules.max_capacitance {
            writeln!(file, "set_max_capacitance {:.3} [current_design]", cap)?;
        }

        Ok(())
    }

    /// Validate constraints
    pub fn validate(&self) -> Vec<ValidationError> {
        let mut errors = Vec::new();

        // Check for clock definitions
        if self.clocks.is_empty() {
            errors.push(ValidationError {
                error_type: ValidationErrorType::MissingClock,
                message: "No clock definitions found".to_string(),
                severity: Severity::Error,
            });
        }

        // Check for conflicting constraints
        for clock in &self.clocks {
            if clock.period <= 0.0 {
                errors.push(ValidationError {
                    error_type: ValidationErrorType::InvalidValue,
                    message: format!("Invalid clock period for {}", clock.name),
                    severity: Severity::Error,
                });
            }
        }

        // Check false paths
        for fpath in &self.false_paths {
            if fpath.from.is_empty() && fpath.to.is_empty() {
                errors.push(ValidationError {
                    error_type: ValidationErrorType::IncompleteConstraint,
                    message: "False path missing from/to specification".to_string(),
                    severity: Severity::Warning,
                });
            }
        }

        errors
    }

    /// Get constraint summary
    pub fn summary(&self) -> String {
        let mut summary = String::new();

        summary.push_str("=== SDC Constraint Summary ===\n\n");
        summary.push_str(&format!("Clocks: {}\n", self.clocks.len()));
        summary.push_str(&format!(
            "Generated Clocks: {}\n",
            self.generated_clocks.len()
        ));
        summary.push_str(&format!("Input Delays: {}\n", self.input_delays.len()));
        summary.push_str(&format!("Output Delays: {}\n", self.output_delays.len()));
        summary.push_str(&format!("False Paths: {}\n", self.false_paths.len()));
        summary.push_str(&format!(
            "Multicycle Paths: {}\n",
            self.multicycle_paths.len()
        ));
        summary.push_str(&format!(
            "Max Delay Constraints: {}\n",
            self.max_delays.len()
        ));
        summary.push_str(&format!(
            "Min Delay Constraints: {}\n",
            self.min_delays.len()
        ));

        if !self.clocks.is_empty() {
            summary.push_str("\nClock Details:\n");
            for clock in &self.clocks {
                summary.push_str(&format!(
                    "  {} : {:.3} ns ({:.1} MHz)\n",
                    clock.name,
                    clock.period,
                    1000.0 / clock.period
                ));
            }
        }

        summary
    }
}

/// Validation error
#[derive(Debug, Clone)]
pub struct ValidationError {
    pub error_type: ValidationErrorType,
    pub message: String,
    pub severity: Severity,
}

/// Validation error types
#[derive(Debug, Clone)]
pub enum ValidationErrorType {
    MissingClock,
    InvalidValue,
    IncompleteConstraint,
    ConflictingConstraints,
    UnreferencedObject,
}

/// Severity levels
#[derive(Debug, Clone)]
pub enum Severity {
    Info,
    Warning,
    Error,
}

impl Default for OperatingConditions {
    fn default() -> Self {
        Self {
            process: ProcessCorner::Typical,
            voltage: 1.8,
            temperature: 25.0,
            library: None,
        }
    }
}

impl Default for DesignRules {
    fn default() -> Self {
        Self {
            max_fanout: None,
            max_transition: None,
            max_capacitance: None,
            min_capacitance: None,
            max_area: None,
        }
    }
}
