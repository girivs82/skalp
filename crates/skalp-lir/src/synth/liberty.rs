//! Liberty File Parser
//!
//! This module provides a basic parser for Liberty (.lib) timing library files.
//! Liberty is the industry standard format for describing cell timing, power,
//! and functionality.
//!
//! # Supported Features
//!
//! - Cell definitions with area
//! - Pin definitions (direction, capacitance)
//! - Timing arcs with NLDM tables
//! - Basic timing groups (cell_rise, cell_fall, rise_transition, fall_transition)
//!
//! # Limitations
//!
//! This is a simplified parser that handles common cases. Full Liberty support
//! would require a more comprehensive parser.
//!
//! # References
//!
//! - Synopsys Liberty User Guide

use super::timing::{
    CapacitanceFf, CellTiming, NldmTable, PinDirection, PinTiming, TimePs, TimingArc, TimingSense,
    TimingType,
};
use std::collections::HashMap;
use std::str::FromStr;

/// Liberty library
#[derive(Debug, Clone)]
pub struct LibertyLibrary {
    /// Library name
    pub name: String,
    /// Time unit (e.g., "1ps", "1ns")
    pub time_unit: String,
    /// Capacitance unit (e.g., "1fF", "1pF")
    pub capacitance_unit: String,
    /// Cells in the library
    pub cells: HashMap<String, CellTiming>,
    /// Default operating conditions
    pub nom_voltage: f64,
    pub nom_temperature: f64,
    pub nom_process: f64,
}

impl Default for LibertyLibrary {
    fn default() -> Self {
        Self::new("default")
    }
}

impl LibertyLibrary {
    /// Create a new empty library
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            time_unit: "1ps".to_string(),
            capacitance_unit: "1fF".to_string(),
            cells: HashMap::new(),
            nom_voltage: 1.0,
            nom_temperature: 25.0,
            nom_process: 1.0,
        }
    }

    /// Parse a Liberty file from a string
    pub fn parse(content: &str) -> Result<Self, LibertyError> {
        let mut parser = LibertyParser::new(content);
        parser.parse()
    }

    /// Add a cell to the library
    pub fn add_cell(&mut self, cell: CellTiming) {
        self.cells.insert(cell.name.clone(), cell);
    }

    /// Get a cell by name
    pub fn get_cell(&self, name: &str) -> Option<&CellTiming> {
        self.cells.get(name)
    }

    /// Get all cell names
    pub fn cell_names(&self) -> impl Iterator<Item = &str> {
        self.cells.keys().map(|s| s.as_str())
    }

    /// Get time unit multiplier (to convert to ps)
    pub fn time_multiplier(&self) -> f64 {
        parse_unit_multiplier(&self.time_unit, 1e12) // Convert to ps
    }

    /// Get capacitance unit multiplier (to convert to fF)
    pub fn capacitance_multiplier(&self) -> f64 {
        parse_unit_multiplier(&self.capacitance_unit, 1e15) // Convert to fF
    }
}

/// Parse a unit string like "1ns" or "1fF" and return multiplier
fn parse_unit_multiplier(unit: &str, base_factor: f64) -> f64 {
    // Extract numeric prefix and unit suffix
    let mut num_end = 0;
    for (i, c) in unit.chars().enumerate() {
        if c.is_ascii_digit() || c == '.' {
            num_end = i + 1;
        } else {
            break;
        }
    }

    let num: f64 = unit[..num_end].parse().unwrap_or(1.0);
    let suffix = &unit[num_end..];

    let unit_factor = match suffix.to_lowercase().as_str() {
        "s" => 1.0,
        "ms" => 1e-3,
        "us" => 1e-6,
        "ns" => 1e-9,
        "ps" => 1e-12,
        "fs" => 1e-15,
        "f" => 1.0,
        "pf" => 1e-12,
        "ff" => 1e-15,
        _ => 1.0,
    };

    num * unit_factor * base_factor
}

/// Liberty parsing error
#[derive(Debug, Clone)]
pub struct LibertyError {
    pub message: String,
    pub line: usize,
}

impl std::fmt::Display for LibertyError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Liberty error at line {}: {}", self.line, self.message)
    }
}

impl std::error::Error for LibertyError {}

/// Liberty file parser
struct LibertyParser<'a> {
    content: &'a str,
    pos: usize,
    line: usize,
}

impl<'a> LibertyParser<'a> {
    fn new(content: &'a str) -> Self {
        Self {
            content,
            pos: 0,
            line: 1,
        }
    }

    fn parse(&mut self) -> Result<LibertyLibrary, LibertyError> {
        let mut library = LibertyLibrary::new("unknown");

        self.skip_whitespace();

        // Expect "library" keyword
        if !self.consume_keyword("library") {
            return Err(self.error("Expected 'library' keyword"));
        }

        self.skip_whitespace();

        // Parse library name
        if !self.consume_char('(') {
            return Err(self.error("Expected '('"));
        }
        library.name = self.parse_identifier()?;
        if !self.consume_char(')') {
            return Err(self.error("Expected ')'"));
        }

        self.skip_whitespace();

        // Parse library body
        if !self.consume_char('{') {
            return Err(self.error("Expected '{'"));
        }

        self.parse_library_body(&mut library)?;

        if !self.consume_char('}') {
            return Err(self.error("Expected '}'"));
        }

        Ok(library)
    }

    fn parse_library_body(&mut self, library: &mut LibertyLibrary) -> Result<(), LibertyError> {
        loop {
            self.skip_whitespace();

            if self.peek_char() == Some('}') {
                break;
            }

            if self.is_eof() {
                break;
            }

            // Parse attribute or group
            let name = self.parse_identifier()?;
            self.skip_whitespace();

            if self.consume_char(':') {
                // Simple attribute
                self.skip_whitespace();
                let value = self.parse_value()?;
                self.consume_char(';');

                match name.as_str() {
                    "time_unit" => library.time_unit = value,
                    "capacitive_load_unit" => library.capacitance_unit = value,
                    "nom_voltage" => library.nom_voltage = value.parse().unwrap_or(1.0),
                    "nom_temperature" => library.nom_temperature = value.parse().unwrap_or(25.0),
                    "nom_process" => library.nom_process = value.parse().unwrap_or(1.0),
                    _ => {} // Ignore unknown attributes
                }
            } else if self.consume_char('(') {
                // Group
                let group_name = self.parse_identifier().unwrap_or_default();
                if !self.consume_char(')') {
                    // Skip until )
                    while self.peek_char() != Some(')') && !self.is_eof() {
                        self.advance();
                    }
                    self.consume_char(')');
                }

                self.skip_whitespace();

                if self.consume_char('{') {
                    if name == "cell" {
                        let cell = self.parse_cell(&group_name)?;
                        library.add_cell(cell);
                    } else {
                        // Skip unknown group
                        self.skip_group()?;
                    }
                }
            } else {
                // Skip unknown content
                self.skip_until_semicolon_or_brace();
            }
        }

        Ok(())
    }

    fn parse_cell(&mut self, name: &str) -> Result<CellTiming, LibertyError> {
        let mut cell = CellTiming::new(name);

        loop {
            self.skip_whitespace();

            if self.peek_char() == Some('}') {
                self.consume_char('}');
                break;
            }

            if self.is_eof() {
                break;
            }

            let attr_name = self.parse_identifier()?;
            self.skip_whitespace();

            if self.consume_char(':') {
                // Simple attribute
                self.skip_whitespace();
                let value = self.parse_value()?;
                self.consume_char(';');

                match attr_name.as_str() {
                    "area" => cell.area = value.parse().unwrap_or(0.0),
                    "cell_leakage_power" => cell.leakage_power = Some(value.parse().unwrap_or(0.0)),
                    _ => {}
                }
            } else if self.consume_char('(') {
                let group_name = self.parse_identifier().unwrap_or_default();
                if !self.consume_char(')') {
                    while self.peek_char() != Some(')') && !self.is_eof() {
                        self.advance();
                    }
                    self.consume_char(')');
                }

                self.skip_whitespace();

                if self.consume_char('{') {
                    match attr_name.as_str() {
                        "pin" => {
                            let pin = self.parse_pin(&group_name)?;
                            cell.add_pin(pin);
                        }
                        _ => {
                            self.skip_group()?;
                        }
                    }
                }
            } else {
                self.skip_until_semicolon_or_brace();
            }
        }

        Ok(cell)
    }

    fn parse_pin(&mut self, name: &str) -> Result<PinTiming, LibertyError> {
        let mut pin = PinTiming {
            name: name.to_string(),
            direction: PinDirection::Input,
            capacitance: None,
            max_capacitance: None,
            max_transition: None,
            is_clock: false,
        };

        let mut timing_arcs: Vec<TimingArc> = Vec::new();

        loop {
            self.skip_whitespace();

            if self.peek_char() == Some('}') {
                self.consume_char('}');
                break;
            }

            if self.is_eof() {
                break;
            }

            let attr_name = self.parse_identifier()?;
            self.skip_whitespace();

            if self.consume_char(':') {
                self.skip_whitespace();
                let value = self.parse_value()?;
                self.consume_char(';');

                match attr_name.as_str() {
                    "direction" => {
                        pin.direction = match value.as_str() {
                            "input" => PinDirection::Input,
                            "output" => PinDirection::Output,
                            "inout" => PinDirection::Inout,
                            _ => PinDirection::Internal,
                        };
                    }
                    "capacitance" => pin.capacitance = Some(value.parse().unwrap_or(0.0)),
                    "max_capacitance" => pin.max_capacitance = Some(value.parse().unwrap_or(0.0)),
                    "max_transition" => pin.max_transition = Some(value.parse().unwrap_or(0.0)),
                    "clock" => pin.is_clock = value == "true",
                    _ => {}
                }
            } else if self.consume_char('(') {
                let group_name = self.parse_identifier().unwrap_or_default();
                if !self.consume_char(')') {
                    while self.peek_char() != Some(')') && !self.is_eof() {
                        self.advance();
                    }
                    self.consume_char(')');
                }

                self.skip_whitespace();

                if self.consume_char('{') {
                    if attr_name == "timing" {
                        if let Ok(arc) = self.parse_timing_group(name) {
                            timing_arcs.push(arc);
                        }
                    } else {
                        self.skip_group()?;
                    }
                }
            } else {
                self.skip_until_semicolon_or_brace();
            }
        }

        Ok(pin)
    }

    fn parse_timing_group(&mut self, to_pin: &str) -> Result<TimingArc, LibertyError> {
        let mut arc = TimingArc::combinational("", to_pin);

        loop {
            self.skip_whitespace();

            if self.peek_char() == Some('}') {
                self.consume_char('}');
                break;
            }

            if self.is_eof() {
                break;
            }

            let attr_name = self.parse_identifier()?;
            self.skip_whitespace();

            if self.consume_char(':') {
                self.skip_whitespace();
                let value = self.parse_value()?;
                self.consume_char(';');

                match attr_name.as_str() {
                    "related_pin" => arc.from_pin = value.trim_matches('"').to_string(),
                    "timing_sense" => {
                        arc.sense = match value.as_str() {
                            "positive_unate" => TimingSense::PositiveUnate,
                            "negative_unate" => TimingSense::NegativeUnate,
                            _ => TimingSense::NonUnate,
                        };
                    }
                    "timing_type" => {
                        arc.timing_type = match value.as_str() {
                            "rising_edge" => TimingType::RisingEdge,
                            "falling_edge" => TimingType::FallingEdge,
                            "setup_rising" | "setup_falling" => TimingType::Setup,
                            "hold_rising" | "hold_falling" => TimingType::Hold,
                            _ => TimingType::Combinational,
                        };
                    }
                    _ => {}
                }
            } else if self.consume_char('(') {
                let group_name = self.parse_identifier().unwrap_or_default();
                if !self.consume_char(')') {
                    while self.peek_char() != Some(')') && !self.is_eof() {
                        self.advance();
                    }
                    self.consume_char(')');
                }

                self.skip_whitespace();

                if self.consume_char('{') {
                    match attr_name.as_str() {
                        "cell_rise" => {
                            arc.cell_rise = self.parse_nldm_table().ok();
                        }
                        "cell_fall" => {
                            arc.cell_fall = self.parse_nldm_table().ok();
                        }
                        "rise_transition" => {
                            arc.rise_transition = self.parse_nldm_table().ok();
                        }
                        "fall_transition" => {
                            arc.fall_transition = self.parse_nldm_table().ok();
                        }
                        _ => {
                            self.skip_group()?;
                        }
                    }
                }
            } else {
                self.skip_until_semicolon_or_brace();
            }
        }

        Ok(arc)
    }

    fn parse_nldm_table(&mut self) -> Result<NldmTable, LibertyError> {
        let mut index_1: Vec<f64> = Vec::new();
        let mut index_2: Vec<f64> = Vec::new();
        let mut values: Vec<Vec<f64>> = Vec::new();

        loop {
            self.skip_whitespace();

            if self.peek_char() == Some('}') {
                self.consume_char('}');
                break;
            }

            if self.is_eof() {
                break;
            }

            let attr_name = self.parse_identifier()?;
            self.skip_whitespace();

            if self.consume_char('(') {
                let list_content = self.parse_until_matching_paren()?;

                match attr_name.as_str() {
                    "index_1" => index_1 = self.parse_number_list(&list_content),
                    "index_2" => index_2 = self.parse_number_list(&list_content),
                    "values" => values = self.parse_2d_number_list(&list_content),
                    _ => {}
                }
            } else if self.consume_char(':') {
                self.skip_whitespace();
                self.parse_value()?;
                self.consume_char(';');
            } else {
                self.skip_until_semicolon_or_brace();
            }
        }

        // Handle scalar or 1D tables
        if index_1.is_empty() {
            index_1 = vec![0.0];
        }
        if index_2.is_empty() {
            index_2 = vec![0.0];
        }
        if values.is_empty() {
            values = vec![vec![0.0]];
        }

        Ok(NldmTable::new(index_1, index_2, values))
    }

    fn parse_number_list(&self, content: &str) -> Vec<f64> {
        content
            .split(',')
            .filter_map(|s| s.trim().trim_matches('"').parse().ok())
            .collect()
    }

    fn parse_2d_number_list(&self, content: &str) -> Vec<Vec<f64>> {
        // Values are separated by \\ for rows and , for columns
        content
            .split("\\\\")
            .map(|row| self.parse_number_list(row.trim().trim_matches('"')))
            .filter(|row| !row.is_empty())
            .collect()
    }

    fn parse_until_matching_paren(&mut self) -> Result<String, LibertyError> {
        let mut result = String::new();
        let mut depth = 1;

        while depth > 0 {
            match self.peek_char() {
                Some('(') => {
                    depth += 1;
                    result.push('(');
                    self.advance();
                }
                Some(')') => {
                    depth -= 1;
                    if depth > 0 {
                        result.push(')');
                    }
                    self.advance();
                }
                Some(c) => {
                    if c == '\n' {
                        self.line += 1;
                    }
                    result.push(c);
                    self.advance();
                }
                None => {
                    return Err(self.error("Unexpected end of file in parentheses"));
                }
            }
        }

        Ok(result)
    }

    fn skip_group(&mut self) -> Result<(), LibertyError> {
        let mut depth = 1;
        while depth > 0 {
            match self.peek_char() {
                Some('{') => {
                    depth += 1;
                    self.advance();
                }
                Some('}') => {
                    depth -= 1;
                    self.advance();
                }
                Some('\n') => {
                    self.line += 1;
                    self.advance();
                }
                Some(_) => {
                    self.advance();
                }
                None => {
                    return Err(self.error("Unexpected end of file in group"));
                }
            }
        }
        Ok(())
    }

    fn parse_identifier(&mut self) -> Result<String, LibertyError> {
        let mut result = String::new();

        while let Some(c) = self.peek_char() {
            if c.is_alphanumeric() || c == '_' {
                result.push(c);
                self.advance();
            } else {
                break;
            }
        }

        if result.is_empty() {
            Err(self.error("Expected identifier"))
        } else {
            Ok(result)
        }
    }

    fn parse_value(&mut self) -> Result<String, LibertyError> {
        self.skip_whitespace();

        let mut result = String::new();

        // Handle quoted strings
        if self.peek_char() == Some('"') {
            self.advance();
            while let Some(c) = self.peek_char() {
                if c == '"' {
                    self.advance();
                    break;
                }
                result.push(c);
                self.advance();
            }
        } else {
            // Unquoted value
            while let Some(c) = self.peek_char() {
                if c == ';' || c == '{' || c == '}' || c.is_whitespace() {
                    break;
                }
                result.push(c);
                self.advance();
            }
        }

        Ok(result)
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.peek_char() {
            if c.is_whitespace() {
                if c == '\n' {
                    self.line += 1;
                }
                self.advance();
            } else if c == '/' {
                // Handle comments
                self.advance();
                if self.peek_char() == Some('*') {
                    // Block comment
                    self.advance();
                    while !self.is_eof() {
                        if self.peek_char() == Some('*') {
                            self.advance();
                            if self.peek_char() == Some('/') {
                                self.advance();
                                break;
                            }
                        } else {
                            if self.peek_char() == Some('\n') {
                                self.line += 1;
                            }
                            self.advance();
                        }
                    }
                } else if self.peek_char() == Some('/') {
                    // Line comment
                    while let Some(c) = self.peek_char() {
                        self.advance();
                        if c == '\n' {
                            self.line += 1;
                            break;
                        }
                    }
                } else {
                    // Not a comment, put back the /
                    self.pos -= 1;
                    break;
                }
            } else {
                break;
            }
        }
    }

    fn skip_until_semicolon_or_brace(&mut self) {
        while let Some(c) = self.peek_char() {
            if c == ';' {
                self.advance();
                break;
            }
            if c == '{' || c == '}' {
                break;
            }
            if c == '\n' {
                self.line += 1;
            }
            self.advance();
        }
    }

    fn consume_keyword(&mut self, keyword: &str) -> bool {
        let start = self.pos;
        for expected in keyword.chars() {
            if self.peek_char() != Some(expected) {
                self.pos = start;
                return false;
            }
            self.advance();
        }
        true
    }

    fn consume_char(&mut self, expected: char) -> bool {
        self.skip_whitespace();
        if self.peek_char() == Some(expected) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn peek_char(&self) -> Option<char> {
        self.content[self.pos..].chars().next()
    }

    fn advance(&mut self) {
        if let Some(c) = self.peek_char() {
            self.pos += c.len_utf8();
        }
    }

    fn is_eof(&self) -> bool {
        self.pos >= self.content.len()
    }

    fn error(&self, message: &str) -> LibertyError {
        LibertyError {
            message: message.to_string(),
            line: self.line,
        }
    }
}

/// Create a basic standard cell library with common cells
pub fn create_basic_library() -> LibertyLibrary {
    let mut lib = LibertyLibrary::new("basic_stdcell");

    // Inverter
    let mut inv = CellTiming::new("INV_X1").with_area(1.0);
    inv.add_pin(PinTiming::input("A", 2.0));
    inv.add_pin(PinTiming::output("Y"));
    inv.add_timing_arc(
        TimingArc::combinational("A", "Y")
            .with_sense(TimingSense::NegativeUnate)
            .with_delays(NldmTable::constant(15.0), NldmTable::constant(12.0)),
    );
    lib.add_cell(inv);

    // 2-input AND
    let mut and2 = CellTiming::new("AND2_X1").with_area(2.0);
    and2.add_pin(PinTiming::input("A", 2.0));
    and2.add_pin(PinTiming::input("B", 2.0));
    and2.add_pin(PinTiming::output("Y"));
    and2.add_timing_arc(
        TimingArc::combinational("A", "Y")
            .with_sense(TimingSense::PositiveUnate)
            .with_delays(NldmTable::constant(25.0), NldmTable::constant(20.0)),
    );
    and2.add_timing_arc(
        TimingArc::combinational("B", "Y")
            .with_sense(TimingSense::PositiveUnate)
            .with_delays(NldmTable::constant(25.0), NldmTable::constant(20.0)),
    );
    lib.add_cell(and2);

    // 2-input OR
    let mut or2 = CellTiming::new("OR2_X1").with_area(2.0);
    or2.add_pin(PinTiming::input("A", 2.0));
    or2.add_pin(PinTiming::input("B", 2.0));
    or2.add_pin(PinTiming::output("Y"));
    or2.add_timing_arc(
        TimingArc::combinational("A", "Y")
            .with_sense(TimingSense::PositiveUnate)
            .with_delays(NldmTable::constant(22.0), NldmTable::constant(18.0)),
    );
    lib.add_cell(or2);

    // 2-input NAND
    let mut nand2 = CellTiming::new("NAND2_X1").with_area(1.5);
    nand2.add_pin(PinTiming::input("A", 2.0));
    nand2.add_pin(PinTiming::input("B", 2.0));
    nand2.add_pin(PinTiming::output("Y"));
    nand2.add_timing_arc(
        TimingArc::combinational("A", "Y")
            .with_sense(TimingSense::NegativeUnate)
            .with_delays(NldmTable::constant(18.0), NldmTable::constant(15.0)),
    );
    lib.add_cell(nand2);

    // 2-input NOR
    let mut nor2 = CellTiming::new("NOR2_X1").with_area(1.5);
    nor2.add_pin(PinTiming::input("A", 2.0));
    nor2.add_pin(PinTiming::input("B", 2.0));
    nor2.add_pin(PinTiming::output("Y"));
    nor2.add_timing_arc(
        TimingArc::combinational("A", "Y")
            .with_sense(TimingSense::NegativeUnate)
            .with_delays(NldmTable::constant(20.0), NldmTable::constant(16.0)),
    );
    lib.add_cell(nor2);

    // 2-input XOR
    let mut xor2 = CellTiming::new("XOR2_X1").with_area(3.0);
    xor2.add_pin(PinTiming::input("A", 2.5));
    xor2.add_pin(PinTiming::input("B", 2.5));
    xor2.add_pin(PinTiming::output("Y"));
    xor2.add_timing_arc(
        TimingArc::combinational("A", "Y")
            .with_sense(TimingSense::NonUnate)
            .with_delays(NldmTable::constant(35.0), NldmTable::constant(30.0)),
    );
    lib.add_cell(xor2);

    // D Flip-Flop
    let mut dff = CellTiming::new("DFF_X1").with_area(4.0);
    dff.add_pin(PinTiming::clock("CK", 3.0));
    dff.add_pin(PinTiming::input("D", 2.0));
    dff.add_pin(PinTiming::output("Q"));
    dff.add_timing_arc(
        TimingArc::sequential("CK", "Q", true)
            .with_delays(NldmTable::constant(50.0), NldmTable::constant(55.0)),
    );
    lib.add_cell(dff);

    // Buffer
    let mut buf = CellTiming::new("BUF_X1").with_area(1.0);
    buf.add_pin(PinTiming::input("A", 2.0));
    buf.add_pin(PinTiming::output("Y"));
    buf.add_timing_arc(
        TimingArc::combinational("A", "Y")
            .with_sense(TimingSense::PositiveUnate)
            .with_delays(NldmTable::constant(12.0), NldmTable::constant(10.0)),
    );
    lib.add_cell(buf);

    // Larger buffers
    let mut buf_x2 = CellTiming::new("BUF_X2").with_area(1.5);
    buf_x2.add_pin(PinTiming::input("A", 3.0));
    buf_x2.add_pin(PinTiming::output("Y"));
    buf_x2.add_timing_arc(
        TimingArc::combinational("A", "Y")
            .with_sense(TimingSense::PositiveUnate)
            .with_delays(NldmTable::constant(10.0), NldmTable::constant(8.0)),
    );
    lib.add_cell(buf_x2);

    let mut buf_x4 = CellTiming::new("BUF_X4").with_area(2.0);
    buf_x4.add_pin(PinTiming::input("A", 4.0));
    buf_x4.add_pin(PinTiming::output("Y"));
    buf_x4.add_timing_arc(
        TimingArc::combinational("A", "Y")
            .with_sense(TimingSense::PositiveUnate)
            .with_delays(NldmTable::constant(8.0), NldmTable::constant(6.0)),
    );
    lib.add_cell(buf_x4);

    // MUX
    let mut mux2 = CellTiming::new("MUX2_X1").with_area(3.0);
    mux2.add_pin(PinTiming::input("A", 2.0));
    mux2.add_pin(PinTiming::input("B", 2.0));
    mux2.add_pin(PinTiming::input("S", 2.0));
    mux2.add_pin(PinTiming::output("Y"));
    mux2.add_timing_arc(
        TimingArc::combinational("A", "Y")
            .with_sense(TimingSense::PositiveUnate)
            .with_delays(NldmTable::constant(28.0), NldmTable::constant(25.0)),
    );
    mux2.add_timing_arc(
        TimingArc::combinational("B", "Y")
            .with_sense(TimingSense::PositiveUnate)
            .with_delays(NldmTable::constant(28.0), NldmTable::constant(25.0)),
    );
    mux2.add_timing_arc(
        TimingArc::combinational("S", "Y")
            .with_sense(TimingSense::NonUnate)
            .with_delays(NldmTable::constant(30.0), NldmTable::constant(28.0)),
    );
    lib.add_cell(mux2);

    lib
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_library() {
        let lib = create_basic_library();

        assert!(lib.get_cell("INV_X1").is_some());
        assert!(lib.get_cell("AND2_X1").is_some());
        assert!(lib.get_cell("DFF_X1").is_some());

        let inv = lib.get_cell("INV_X1").unwrap();
        assert_eq!(inv.area, 1.0);
        assert!(!inv.is_sequential);

        let dff = lib.get_cell("DFF_X1").unwrap();
        assert!(dff.is_sequential);
    }

    #[test]
    fn test_unit_multiplier() {
        assert!((parse_unit_multiplier("1ps", 1e12) - 1.0).abs() < 1e-10);
        assert!((parse_unit_multiplier("1ns", 1e12) - 1000.0).abs() < 1e-10);
        assert!((parse_unit_multiplier("10ps", 1e12) - 10.0).abs() < 1e-10);
        assert!((parse_unit_multiplier("1fF", 1e15) - 1.0).abs() < 1e-10);
        assert!((parse_unit_multiplier("1pF", 1e15) - 1000.0).abs() < 1e-10);
    }

    #[test]
    fn test_parse_simple_liberty() {
        let content = r#"
            library(test_lib) {
                time_unit : "1ps";
                capacitive_load_unit : "1fF";

                cell(INV_X1) {
                    area : 1.0;

                    pin(A) {
                        direction : input;
                        capacitance : 2.0;
                    }

                    pin(Y) {
                        direction : output;
                    }
                }
            }
        "#;

        let lib = LibertyLibrary::parse(content).expect("Failed to parse Liberty");
        assert_eq!(lib.name, "test_lib");
        assert_eq!(lib.time_unit, "1ps");

        let cell = lib.get_cell("INV_X1").expect("Cell not found");
        assert_eq!(cell.area, 1.0);
    }

    #[test]
    fn test_parse_with_comments() {
        let content = r#"
            /* Block comment */
            library(test_lib) {
                // Line comment
                time_unit : "1ns";

                /* Another
                   multiline
                   comment */
                cell(BUF) {
                    area : 1.5;
                }
            }
        "#;

        let lib = LibertyLibrary::parse(content).expect("Failed to parse");
        assert!(lib.get_cell("BUF").is_some());
    }

    #[test]
    fn test_library_cell_iteration() {
        let lib = create_basic_library();

        let cell_names: Vec<&str> = lib.cell_names().collect();
        assert!(cell_names.contains(&"INV_X1"));
        assert!(cell_names.contains(&"AND2_X1"));
        assert!(cell_names.contains(&"DFF_X1"));
    }
}
