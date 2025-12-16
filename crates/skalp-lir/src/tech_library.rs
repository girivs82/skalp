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

use crate::gate_netlist::FaultType;
use crate::word_lir::WordOp;

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
        }
    }

    /// Create a cell with default stuck-at failure modes
    pub fn with_default_failure_modes(mut self) -> Self {
        self.failure_modes = vec![
            LibraryFailureMode::new("stuck_at_0", self.fit * 0.5, FaultType::StuckAt0),
            LibraryFailureMode::new("stuck_at_1", self.fit * 0.5, FaultType::StuckAt1),
        ];
        self
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
            | CellFunction::Xnor2 => (vec!["a".into(), "b".into()], vec!["y".into()]),
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
        )
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

    /// Find a decomposition rule for a WordOp
    pub fn find_decomposition(&self, op: &WordOp) -> Option<&DecompositionRule> {
        self.decomposition_rules.iter().find(|r| r.matches(op))
    }

    /// Get all cell names
    pub fn cell_names(&self) -> Vec<&str> {
        self.cells.keys().map(|s| s.as_str()).collect()
    }

    /// Get the number of cells in the library
    pub fn cell_count(&self) -> usize {
        self.cells.len()
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
    /// Check if this rule matches a WordOp
    pub fn matches(&self, op: &WordOp) -> bool {
        matches!(
            (&self.source, op),
            (DecompSource::Xor, WordOp::Xor { .. })
                | (DecompSource::And, WordOp::And { .. })
                | (DecompSource::Or, WordOp::Or { .. })
                | (DecompSource::Not, WordOp::Not { .. })
                | (DecompSource::Mux2, WordOp::Mux2 { .. })
                | (DecompSource::Add, WordOp::Add { .. })
                | (DecompSource::Sub, WordOp::Sub { .. })
                | (DecompSource::Eq, WordOp::Eq { .. })
                | (DecompSource::Lt, WordOp::Lt { .. })
                | (DecompSource::Reg, WordOp::Reg { .. })
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

        assert!(rule.matches(&WordOp::Xor { width: 1 }));
        assert!(!rule.matches(&WordOp::And { width: 1 }));
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
}
