//! Technology Mapping
//!
//! This module implements cut-based technology mapping for converting
//! AIGs to gate-level netlists using cells from a technology library.
//!
//! # Algorithm Overview
//!
//! 1. **Cut Enumeration**: Find K-feasible cuts for each node
//! 2. **Cut Evaluation**: Score cuts based on area, delay, or other metrics
//! 3. **Covering**: Select best cuts to cover the entire AIG
//! 4. **Recovery**: Apply area recovery on non-critical paths
//!
//! # References
//!
//! - Cong, J., & Ding, Y. (1994). FlowMap: An optimal technology mapping algorithm.
//! - Mishchenko, A., et al. (2007). Combinational and sequential mapping with priority cuts.

mod cell_sizer;
mod cut_mapper;
mod delay_mapper;

pub use cell_sizer::{
    size_cells, size_cells_for_area, size_cells_for_timing, CellSizer, CellSizingConfig,
    CellSizingStats, DriveStrength,
};
pub use cut_mapper::{CutMapper, CutMapperConfig, MappingResult};
pub use delay_mapper::{DelayMapper, DelayMappingConfig};

use super::timing::CellTiming;
use super::{Aig, AigNodeId, Cut};
use crate::tech_library::{CellFunction, TechLibrary};
use std::collections::HashMap;

/// Mapping objective
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum MappingObjective {
    /// Minimize area (gate count)
    Area,
    /// Minimize delay (critical path)
    Delay,
    /// Balance area and delay
    #[default]
    Balanced,
}

/// A mapped node in the netlist
#[derive(Debug, Clone)]
pub struct MappedNode {
    /// Cell type from library
    pub cell_type: String,
    /// Input connections (AigNodeId, inverted)
    pub inputs: Vec<(AigNodeId, bool)>,
    /// Cell area
    pub area: f64,
    /// Cell delay (worst-case)
    pub delay: f64,
    /// Whether the cell's output needs to be inverted
    /// When true, the actual function is !cell_function
    /// This can be absorbed if consumers use the inverted output
    pub output_inverted: bool,
}

/// Mapping statistics
#[derive(Debug, Clone, Default)]
pub struct MappingStats {
    /// Total number of cells
    pub cell_count: usize,
    /// Total area
    pub total_area: f64,
    /// Critical path delay
    pub critical_delay: f64,
    /// Number of inverters
    pub inverter_count: usize,
    /// Number of buffers
    pub buffer_count: usize,
    /// Cells by type
    pub cells_by_type: HashMap<String, usize>,
}

impl MappingStats {
    /// Create new empty stats
    pub fn new() -> Self {
        Self::default()
    }

    /// Record a cell
    pub fn record_cell(&mut self, cell_type: &str, area: f64) {
        self.cell_count += 1;
        self.total_area += area;

        *self.cells_by_type.entry(cell_type.to_string()).or_insert(0) += 1;

        if cell_type.starts_with("INV") {
            self.inverter_count += 1;
        } else if cell_type.starts_with("BUF") {
            self.buffer_count += 1;
        }
    }

    /// Get summary string
    pub fn summary(&self) -> String {
        format!(
            "Cells: {}, Area: {:.1}, Delay: {:.1}ps, Inv: {}, Buf: {}",
            self.cell_count,
            self.total_area,
            self.critical_delay,
            self.inverter_count,
            self.buffer_count
        )
    }
}

/// Cut match: associates a cut with a library cell
#[derive(Debug, Clone)]
pub struct CutMatch {
    /// The cut being matched
    pub cut: Cut,
    /// Matched cell type
    pub cell_type: String,
    /// Cell area
    pub area: f64,
    /// Cell delay
    pub delay: f64,
    /// Input pin mapping (cut leaf index -> cell pin name)
    pub pin_mapping: Vec<String>,
    /// Whether each input should be inverted
    pub input_inversions: Vec<bool>,
    /// Whether output should be inverted
    pub output_inverted: bool,
}

/// Library cell matcher
#[derive(Debug)]
pub struct CellMatcher {
    /// Cell library
    cells: HashMap<String, CellTiming>,
    /// Truth table to cell mapping
    tt_to_cell: HashMap<u64, Vec<CellMatch>>,
}

#[derive(Debug, Clone)]
struct CellMatch {
    cell_type: String,
    area: f64,
    delay: f64,
    input_pins: Vec<String>,
}

impl CellMatcher {
    /// Create a new cell matcher with default/hardcoded cells
    pub fn new() -> Self {
        let mut matcher = Self {
            cells: HashMap::new(),
            tt_to_cell: HashMap::new(),
        };
        matcher.init_basic_cells();
        matcher
    }

    /// Create a cell matcher from a technology library
    ///
    /// This is the preferred way to create a CellMatcher as it uses
    /// the actual cells available in the library with their real costs.
    pub fn from_library(library: &TechLibrary) -> Self {
        let mut matcher = Self {
            cells: HashMap::new(),
            tt_to_cell: HashMap::new(),
        };

        // Track which functions are covered by the library
        let mut covered_functions = std::collections::HashSet::new();

        // Add cells from the library based on their function
        for (name, cell) in library.iter_cells() {
            if let Some(tt_pin_pairs) = Self::function_to_truth_tables(&cell.function) {
                covered_functions.insert(cell.function.clone());

                // Use library cell costs - fit represents relative complexity
                // Scale to reasonable area units (fit ~0.05-0.2 → area ~1-4)
                let area = 1.0 + cell.fit * 15.0;
                let delay = 15.0 + cell.fit * 50.0;

                // Each pair has its own truth table and pin mapping
                for (tt, pins) in tt_pin_pairs {
                    matcher.add_cell_match(tt, name, area, delay, pins);
                }
            }
        }

        // Add hardcoded cells for functions NOT covered by the library
        // This ensures we always have basic coverage even with incomplete libraries
        matcher.init_basic_cells_for_missing(&covered_functions);

        matcher
    }

    /// Map a CellFunction to its truth table(s) and pin names
    ///
    /// Returns None for functions that don't have a simple truth table
    /// (like sequential elements or custom cells).
    /// Each entry in the vector is (truth_table, pin_mapping) to correctly
    /// handle permutations where inputs need reordering.
    fn function_to_truth_tables(function: &CellFunction) -> Option<Vec<(u64, Vec<&'static str>)>> {
        match function {
            // 2-input gates - symmetric, no need for permutations
            CellFunction::And2 => Some(vec![(0x8, vec!["A", "B"])]),
            CellFunction::Or2 => Some(vec![(0xE, vec!["A", "B"])]),
            CellFunction::Nand2 => Some(vec![(0x7, vec!["A", "B"])]),
            CellFunction::Nor2 => Some(vec![(0x1, vec!["A", "B"])]),
            CellFunction::Xor2 => Some(vec![(0x6, vec!["A", "B"])]),
            CellFunction::Xnor2 => Some(vec![(0x9, vec!["A", "B"])]),
            // ANDNOT/ORNOT need different pin orderings for each permutation
            CellFunction::AndNot => Some(vec![
                (0x2, vec!["A", "B"]), // a & ~b
                (0x4, vec!["B", "A"]), // ~a & b (swap inputs)
            ]),
            CellFunction::OrNot => Some(vec![
                (0xB, vec!["A", "B"]), // a | ~b
                (0xD, vec!["B", "A"]), // ~a | b (swap inputs)
            ]),

            // 3-input gates - symmetric
            CellFunction::And3 => Some(vec![(0x80, vec!["A", "B", "C"])]),
            CellFunction::Or3 => Some(vec![(0xFE, vec!["A", "B", "C"])]),
            CellFunction::Nand3 => Some(vec![(0x7F, vec!["A", "B", "C"])]),
            CellFunction::Nor3 => Some(vec![(0x01, vec!["A", "B", "C"])]),

            // 4-input gates - symmetric
            CellFunction::And4 => Some(vec![(0x8000, vec!["A", "B", "C", "D"])]),
            CellFunction::Or4 => Some(vec![(0xFFFE, vec!["A", "B", "C", "D"])]),
            CellFunction::Nand4 => Some(vec![(0x7FFF, vec!["A", "B", "C", "D"])]),
            CellFunction::Nor4 => Some(vec![(0x0001, vec!["A", "B", "C", "D"])]),

            // Complex gates - each permutation needs its own pin mapping
            CellFunction::Aoi21 => Some(vec![
                (0x15, vec!["A", "B", "C"]),
                (0x07, vec!["A", "B", "C"]), // !(a&b | c)
                (0x23, vec!["A", "C", "B"]), // !(a&c | b)
                (0x45, vec!["B", "C", "A"]), // !(b&c | a)
            ]),
            CellFunction::Oai21 => Some(vec![
                (0x57, vec!["A", "B", "C"]),
                (0x1F, vec!["A", "B", "C"]), // !((a|b) & c)
                (0x2F, vec!["A", "C", "B"]), // !((a|c) & b)
                (0x4F, vec!["B", "C", "A"]), // !((b|c) & a)
            ]),
            CellFunction::Aoi22 => Some(vec![
                (0x0777, vec!["A", "B", "C", "D"]),
                (0x135F, vec!["A", "C", "B", "D"]),
                (0x153F, vec!["A", "D", "B", "C"]),
            ]),
            CellFunction::Oai22 => Some(vec![
                (0x111F, vec!["A", "B", "C", "D"]),
                (0x0537, vec!["A", "C", "B", "D"]),
                (0x0357, vec!["A", "D", "B", "C"]),
            ]),

            // MUX: s ? b : a
            // All 6 input permutations for proper matching regardless of leaf ordering
            // The truth table depends on which input maps to index 0, 1, 2
            CellFunction::Mux2 => Some(vec![
                (0xCA, vec!["A", "B", "S"]), // inputs: (A=i0, B=i1, S=i2)
                (0xD2, vec!["A", "S", "B"]), // inputs: (A=i0, S=i1, B=i2)
                (0xAC, vec!["B", "A", "S"]), // inputs: (B=i0, A=i1, S=i2)
                (0xB4, vec!["B", "S", "A"]), // inputs: (B=i0, S=i1, A=i2)
                (0xE4, vec!["S", "A", "B"]), // inputs: (S=i0, A=i1, B=i2)
                (0xD8, vec!["S", "B", "A"]), // inputs: (S=i0, B=i1, A=i2)
            ]),

            // Sequential and other cells don't have truth tables
            _ => None,
        }
    }

    /// Initialize with basic combinational cells
    fn init_basic_cells(&mut self) {
        // 2-input gates (truth table for 2 inputs: bits [0..3])
        // AND: f = a & b, tt = 1000 = 0x8
        self.add_cell_match(0x8, "AND2_X1", 2.0, 25.0, vec!["A", "B"]);

        // OR: f = a | b, tt = 1110 = 0xE
        self.add_cell_match(0xE, "OR2_X1", 2.0, 22.0, vec!["A", "B"]);

        // NAND: f = !(a & b), tt = 0111 = 0x7
        self.add_cell_match(0x7, "NAND2_X1", 1.5, 18.0, vec!["A", "B"]);

        // NOR: f = !(a | b), tt = 0001 = 0x1
        self.add_cell_match(0x1, "NOR2_X1", 1.5, 20.0, vec!["A", "B"]);

        // XOR: f = a ^ b, tt = 0110 = 0x6
        self.add_cell_match(0x6, "XOR2_X1", 3.0, 35.0, vec!["A", "B"]);

        // XNOR: f = !(a ^ b), tt = 1001 = 0x9
        self.add_cell_match(0x9, "XNOR2_X1", 3.0, 35.0, vec!["A", "B"]);

        // ANDNOT: f = a & ~b, tt = 0010 = 0x2
        // (output when: a=0,b=0->0, a=0,b=1->0, a=1,b=0->1, a=1,b=1->0)
        self.add_cell_match(0x2, "ANDNOT_X1", 1.5, 20.0, vec!["A", "B"]);
        // ANDNOT permuted: f = ~a & b, tt = 0100 = 0x4 (swap inputs)
        self.add_cell_match(0x4, "ANDNOT_X1", 1.5, 20.0, vec!["B", "A"]);

        // ORNOT: f = a | ~b, tt = 1011 = 0xB
        // (output when: a=0,b=0->1, a=0,b=1->0, a=1,b=0->1, a=1,b=1->1)
        self.add_cell_match(0xB, "ORNOT_X1", 1.5, 20.0, vec!["A", "B"]);
        // ORNOT permuted: f = ~a | b, tt = 1101 = 0xD (swap inputs)
        self.add_cell_match(0xD, "ORNOT_X1", 1.5, 20.0, vec!["B", "A"]);

        // Single input gates
        // Inverter: f = !a, tt = 01 = 0x1 for 1 input
        // Buffer: f = a, tt = 10 = 0x2 for 1 input
        // Note: These are handled specially in mapping

        // 3-input gates
        // AND3: tt for 3 inputs (8 bits), f = a & b & c
        self.add_cell_match(0x80, "AND3_X1", 2.5, 30.0, vec!["A", "B", "C"]);

        // OR3: f = a | b | c
        self.add_cell_match(0xFE, "OR3_X1", 2.5, 28.0, vec!["A", "B", "C"]);

        // NAND3: f = !(a & b & c)
        self.add_cell_match(0x7F, "NAND3_X1", 2.0, 22.0, vec!["A", "B", "C"]);

        // NOR3: f = !(a | b | c)
        self.add_cell_match(0x01, "NOR3_X1", 2.0, 24.0, vec!["A", "B", "C"]);

        // MUX (3-input function): s ? b : a
        // All 6 input permutations for proper matching regardless of leaf ordering
        self.add_cell_match(0xCA, "MUX2_X1", 3.0, 30.0, vec!["A", "B", "S"]);
        self.add_cell_match(0xD2, "MUX2_X1", 3.0, 30.0, vec!["A", "S", "B"]);
        self.add_cell_match(0xAC, "MUX2_X1", 3.0, 30.0, vec!["B", "A", "S"]);
        self.add_cell_match(0xB4, "MUX2_X1", 3.0, 30.0, vec!["B", "S", "A"]);
        self.add_cell_match(0xE4, "MUX2_X1", 3.0, 30.0, vec!["S", "A", "B"]);
        self.add_cell_match(0xD8, "MUX2_X1", 3.0, 30.0, vec!["S", "B", "A"]);

        // AOI21: !(a & b | c)
        self.add_cell_match(0x15, "AOI21_X1", 2.0, 20.0, vec!["A", "B", "C"]);

        // OAI21: !((a | b) & c)
        self.add_cell_match(0x57, "OAI21_X1", 2.0, 20.0, vec!["A", "B", "C"]);

        // 4-input gates (16 bits)
        self.add_cell_match(0x8000, "AND4_X1", 3.0, 35.0, vec!["A", "B", "C", "D"]);
        self.add_cell_match(0xFFFE, "OR4_X1", 3.0, 32.0, vec!["A", "B", "C", "D"]);
        self.add_cell_match(0x7FFF, "NAND4_X1", 2.5, 25.0, vec!["A", "B", "C", "D"]);
        self.add_cell_match(0x0001, "NOR4_X1", 2.5, 28.0, vec!["A", "B", "C", "D"]);

        // AOI4: !(a&b | c&d) - 4-input AND-OR-Invert with 2+2 structure
        // Truth tables for different pairings of 4 inputs:
        // !(a&b | c&d) = 0x0777 - pairs are (0,1) and (2,3)
        // !(a&c | b&d) = 0x135F - pairs are (0,2) and (1,3)
        // !(a&d | b&c) = 0x153F - pairs are (0,3) and (1,2)
        self.add_cell_match(0x0777, "AOI4_X1", 2.0, 22.0, vec!["A", "B", "C", "D"]);
        self.add_cell_match(0x135F, "AOI4_X1", 2.0, 22.0, vec!["A", "C", "B", "D"]);
        self.add_cell_match(0x153F, "AOI4_X1", 2.0, 22.0, vec!["A", "D", "B", "C"]);

        // OAI4: !((a|b) & (c|d)) - 4-input OR-AND-Invert with 2+2 structure
        // Truth tables for different pairings:
        // !((a|b) & (c|d)) = 0x111F - pairs are (0,1) and (2,3)
        // !((a|c) & (b|d)) = 0x0537 - pairs are (0,2) and (1,3)
        // !((a|d) & (b|c)) = 0x0357 - pairs are (0,3) and (1,2)
        self.add_cell_match(0x111F, "OAI4_X1", 2.0, 22.0, vec!["A", "B", "C", "D"]);
        self.add_cell_match(0x0537, "OAI4_X1", 2.0, 22.0, vec!["A", "C", "B", "D"]);
        self.add_cell_match(0x0357, "OAI4_X1", 2.0, 22.0, vec!["A", "D", "B", "C"]);

        // Additional AOI21/OAI21 permutations for better matching
        // Standard AOI21: !(a&b | c) with c as the single input
        // 0x15 is !(a | b&c), so we need to add !(a&b | c) = 0x07
        self.add_cell_match(0x07, "AOI21_X1", 2.0, 20.0, vec!["A", "B", "C"]); // !(a&b | c)
                                                                               // Other permutations
        self.add_cell_match(0x23, "AOI21_X1", 2.0, 20.0, vec!["A", "C", "B"]); // !(a&c | b)
        self.add_cell_match(0x45, "AOI21_X1", 2.0, 20.0, vec!["B", "C", "A"]); // !(b&c | a)

        // Standard OAI21: !((a|b) & c) with c as the single input
        // 0x57 is !((a|b) & c), let's add permutations
        self.add_cell_match(0x1F, "OAI21_X1", 2.0, 20.0, vec!["A", "B", "C"]); // !((a|b) & c)
        self.add_cell_match(0x2F, "OAI21_X1", 2.0, 20.0, vec!["A", "C", "B"]); // !((a|c) & b)
        self.add_cell_match(0x4F, "OAI21_X1", 2.0, 20.0, vec!["B", "C", "A"]); // !((b|c) & a)
    }

    /// Initialize hardcoded cells only for functions NOT already covered by the library
    ///
    /// This ensures we have baseline coverage for all common functions while
    /// preferring library cells when available (since they have accurate costs).
    fn init_basic_cells_for_missing(&mut self, covered: &std::collections::HashSet<CellFunction>) {
        // 2-input gates
        if !covered.contains(&CellFunction::And2) {
            self.add_cell_match(0x8, "AND2_X1", 2.0, 25.0, vec!["A", "B"]);
        }
        if !covered.contains(&CellFunction::Or2) {
            self.add_cell_match(0xE, "OR2_X1", 2.0, 22.0, vec!["A", "B"]);
        }
        if !covered.contains(&CellFunction::Nand2) {
            self.add_cell_match(0x7, "NAND2_X1", 1.5, 18.0, vec!["A", "B"]);
        }
        if !covered.contains(&CellFunction::Nor2) {
            self.add_cell_match(0x1, "NOR2_X1", 1.5, 20.0, vec!["A", "B"]);
        }
        if !covered.contains(&CellFunction::Xor2) {
            self.add_cell_match(0x6, "XOR2_X1", 3.0, 35.0, vec!["A", "B"]);
        }
        if !covered.contains(&CellFunction::Xnor2) {
            self.add_cell_match(0x9, "XNOR2_X1", 3.0, 35.0, vec!["A", "B"]);
        }
        if !covered.contains(&CellFunction::AndNot) {
            self.add_cell_match(0x2, "ANDNOT_X1", 1.5, 20.0, vec!["A", "B"]);
            self.add_cell_match(0x4, "ANDNOT_X1", 1.5, 20.0, vec!["B", "A"]);
        }
        if !covered.contains(&CellFunction::OrNot) {
            self.add_cell_match(0xB, "ORNOT_X1", 1.5, 20.0, vec!["A", "B"]);
            self.add_cell_match(0xD, "ORNOT_X1", 1.5, 20.0, vec!["B", "A"]);
        }

        // 3-input gates
        if !covered.contains(&CellFunction::And3) {
            self.add_cell_match(0x80, "AND3_X1", 2.5, 30.0, vec!["A", "B", "C"]);
        }
        if !covered.contains(&CellFunction::Or3) {
            self.add_cell_match(0xFE, "OR3_X1", 2.5, 28.0, vec!["A", "B", "C"]);
        }
        if !covered.contains(&CellFunction::Nand3) {
            self.add_cell_match(0x7F, "NAND3_X1", 2.0, 22.0, vec!["A", "B", "C"]);
        }
        if !covered.contains(&CellFunction::Nor3) {
            self.add_cell_match(0x01, "NOR3_X1", 2.0, 24.0, vec!["A", "B", "C"]);
        }

        // MUX - all 6 input permutations
        if !covered.contains(&CellFunction::Mux2) {
            self.add_cell_match(0xCA, "MUX2_X1", 3.0, 30.0, vec!["A", "B", "S"]);
            self.add_cell_match(0xD2, "MUX2_X1", 3.0, 30.0, vec!["A", "S", "B"]);
            self.add_cell_match(0xAC, "MUX2_X1", 3.0, 30.0, vec!["B", "A", "S"]);
            self.add_cell_match(0xB4, "MUX2_X1", 3.0, 30.0, vec!["B", "S", "A"]);
            self.add_cell_match(0xE4, "MUX2_X1", 3.0, 30.0, vec!["S", "A", "B"]);
            self.add_cell_match(0xD8, "MUX2_X1", 3.0, 30.0, vec!["S", "B", "A"]);
        }

        // AOI/OAI 21
        if !covered.contains(&CellFunction::Aoi21) {
            self.add_cell_match(0x15, "AOI21_X1", 2.0, 20.0, vec!["A", "B", "C"]);
            self.add_cell_match(0x07, "AOI21_X1", 2.0, 20.0, vec!["A", "B", "C"]);
            self.add_cell_match(0x23, "AOI21_X1", 2.0, 20.0, vec!["A", "C", "B"]);
            self.add_cell_match(0x45, "AOI21_X1", 2.0, 20.0, vec!["B", "C", "A"]);
        }
        if !covered.contains(&CellFunction::Oai21) {
            self.add_cell_match(0x57, "OAI21_X1", 2.0, 20.0, vec!["A", "B", "C"]);
            self.add_cell_match(0x1F, "OAI21_X1", 2.0, 20.0, vec!["A", "B", "C"]);
            self.add_cell_match(0x2F, "OAI21_X1", 2.0, 20.0, vec!["A", "C", "B"]);
            self.add_cell_match(0x4F, "OAI21_X1", 2.0, 20.0, vec!["B", "C", "A"]);
        }

        // 4-input gates
        if !covered.contains(&CellFunction::And4) {
            self.add_cell_match(0x8000, "AND4_X1", 3.0, 35.0, vec!["A", "B", "C", "D"]);
        }
        if !covered.contains(&CellFunction::Or4) {
            self.add_cell_match(0xFFFE, "OR4_X1", 3.0, 32.0, vec!["A", "B", "C", "D"]);
        }
        if !covered.contains(&CellFunction::Nand4) {
            self.add_cell_match(0x7FFF, "NAND4_X1", 2.5, 25.0, vec!["A", "B", "C", "D"]);
        }
        if !covered.contains(&CellFunction::Nor4) {
            self.add_cell_match(0x0001, "NOR4_X1", 2.5, 28.0, vec!["A", "B", "C", "D"]);
        }

        // AOI/OAI 22
        if !covered.contains(&CellFunction::Aoi22) {
            self.add_cell_match(0x0777, "AOI4_X1", 2.0, 22.0, vec!["A", "B", "C", "D"]);
            self.add_cell_match(0x135F, "AOI4_X1", 2.0, 22.0, vec!["A", "C", "B", "D"]);
            self.add_cell_match(0x153F, "AOI4_X1", 2.0, 22.0, vec!["A", "D", "B", "C"]);
        }
        if !covered.contains(&CellFunction::Oai22) {
            self.add_cell_match(0x111F, "OAI4_X1", 2.0, 22.0, vec!["A", "B", "C", "D"]);
            self.add_cell_match(0x0537, "OAI4_X1", 2.0, 22.0, vec!["A", "C", "B", "D"]);
            self.add_cell_match(0x0357, "OAI4_X1", 2.0, 22.0, vec!["A", "D", "B", "C"]);
        }
    }

    fn add_cell_match(
        &mut self,
        truth_table: u64,
        cell_type: &str,
        area: f64,
        delay: f64,
        pins: Vec<&str>,
    ) {
        let entry = self.tt_to_cell.entry(truth_table).or_default();
        entry.push(CellMatch {
            cell_type: cell_type.to_string(),
            area,
            delay,
            input_pins: pins.iter().map(|s| s.to_string()).collect(),
        });
    }

    /// Find matching cells for a truth table
    pub fn find_matches(&self, truth_table: u64, num_inputs: usize) -> Vec<CutMatch> {
        let mut matches = Vec::new();

        // Mask truth table to the actual number of inputs
        let mask = (1u64 << (1 << num_inputs)) - 1;
        let tt = truth_table & mask;

        // Try with output inversion FIRST
        // Rationale: In sequential circuits, many nodes are used inverted (e.g., latch data inputs
        // for toggle logic). By preferring output_inverted cells, we can absorb the inversion
        // into the cell mapping and avoid creating separate inverter cells.
        let inverted_tt = !tt & mask;
        if let Some(cells) = self.tt_to_cell.get(&inverted_tt) {
            for cell in cells {
                if cell.input_pins.len() == num_inputs {
                    matches.push(CutMatch {
                        cut: Cut {
                            leaves: Vec::new(),
                            truth_table: tt,
                            area_cost: cell.area as f32,
                            arrival_time: cell.delay as f32,
                            area_flow: cell.area as f32,
                            edge_count: num_inputs as u32,
                        },
                        cell_type: cell.cell_type.clone(),
                        area: cell.area,
                        delay: cell.delay,
                        pin_mapping: cell.input_pins.clone(),
                        input_inversions: vec![false; num_inputs],
                        output_inverted: true,
                    });
                }
            }
        }

        // Direct match (non-inverted)
        if let Some(cells) = self.tt_to_cell.get(&tt) {
            for cell in cells {
                if cell.input_pins.len() == num_inputs {
                    matches.push(CutMatch {
                        cut: Cut {
                            leaves: Vec::new(), // Will be filled by caller
                            truth_table: tt,
                            area_cost: cell.area as f32,
                            arrival_time: cell.delay as f32,
                            area_flow: cell.area as f32,
                            edge_count: num_inputs as u32,
                        },
                        cell_type: cell.cell_type.clone(),
                        area: cell.area,
                        delay: cell.delay,
                        pin_mapping: cell.input_pins.clone(),
                        input_inversions: vec![false; num_inputs],
                        output_inverted: false,
                    });
                }
            }
        }

        matches
    }

    /// Get area for a cell type
    pub fn get_area(&self, cell_type: &str) -> f64 {
        // Search through all truth table entries
        for cells in self.tt_to_cell.values() {
            for cell in cells {
                if cell.cell_type == cell_type {
                    return cell.area;
                }
            }
        }
        1.0 // Default
    }

    /// Get delay for a cell type
    pub fn get_delay(&self, cell_type: &str) -> f64 {
        for cells in self.tt_to_cell.values() {
            for cell in cells {
                if cell.cell_type == cell_type {
                    return cell.delay;
                }
            }
        }
        20.0 // Default
    }
}

impl Default for CellMatcher {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_mapping_stats() {
        let mut stats = MappingStats::new();
        stats.record_cell("AND2_X1", 2.0);
        stats.record_cell("INV_X1", 1.0);
        stats.record_cell("BUF_X1", 1.0);

        assert_eq!(stats.cell_count, 3);
        assert_eq!(stats.total_area, 4.0);
        assert_eq!(stats.inverter_count, 1);
        assert_eq!(stats.buffer_count, 1);
    }

    #[test]
    fn test_cell_matcher() {
        let matcher = CellMatcher::new();

        // AND2 should match truth table 0x8
        let matches = matcher.find_matches(0x8, 2);
        assert!(!matches.is_empty());
        assert!(matches.iter().any(|m| m.cell_type == "AND2_X1"));

        // OR2 should match truth table 0xE
        let matches = matcher.find_matches(0xE, 2);
        assert!(matches.iter().any(|m| m.cell_type == "OR2_X1"));
    }

    #[test]
    fn test_cell_matcher_with_inversion() {
        let matcher = CellMatcher::new();

        // NAND2 has tt = 0x7 = !AND2 (0x8)
        // Looking for AND with inverted output should find NAND
        let matches = matcher.find_matches(0x8, 2);
        // Direct AND2 match
        assert!(matches
            .iter()
            .any(|m| m.cell_type == "AND2_X1" && !m.output_inverted));
    }

    #[test]
    fn test_mapping_objective_default() {
        let obj = MappingObjective::default();
        assert_eq!(obj, MappingObjective::Balanced);
    }

    #[test]
    fn test_andnot_cell_matching() {
        let matcher = CellMatcher::new();

        // ANDNOT: a & ~b has truth table 0x2
        // Row 0 (a=0, b=0): 0 & ~0 = 0 & 1 = 0
        // Row 1 (a=1, b=0): 1 & ~0 = 1 & 1 = 1 <- bit 1
        // Row 2 (a=0, b=1): 0 & ~1 = 0 & 0 = 0
        // Row 3 (a=1, b=1): 1 & ~1 = 1 & 0 = 0
        // So truth table = 0010 = 0x2
        let matches = matcher.find_matches(0x2, 2);
        assert!(
            matches.iter().any(|m| m.cell_type == "ANDNOT_X1"),
            "Expected ANDNOT_X1 for truth table 0x2, got: {:?}",
            matches.iter().map(|m| &m.cell_type).collect::<Vec<_>>()
        );

        // ~a & b has truth table 0x4 (swap inputs)
        let matches = matcher.find_matches(0x4, 2);
        assert!(
            matches.iter().any(|m| m.cell_type == "ANDNOT_X1"),
            "Expected ANDNOT_X1 for truth table 0x4"
        );

        // ORNOT: a | ~b has truth table 0xB
        let matches = matcher.find_matches(0xB, 2);
        assert!(
            matches.iter().any(|m| m.cell_type == "ORNOT_X1"),
            "Expected ORNOT_X1 for truth table 0xB"
        );

        // ~a | b has truth table 0xD
        let matches = matcher.find_matches(0xD, 2);
        assert!(
            matches.iter().any(|m| m.cell_type == "ORNOT_X1"),
            "Expected ORNOT_X1 for truth table 0xD"
        );
    }

    #[test]
    fn test_library_aware_cell_matcher() {
        use crate::builtin_libraries;

        let lib = builtin_libraries::get_builtin_library("7nm").unwrap();
        let matcher = CellMatcher::from_library(&lib);

        // The library should have ANDNOT cells
        let matches = matcher.find_matches(0x2, 2);
        assert!(
            matches.iter().any(|m| m.cell_type.contains("ANDNOT")),
            "Library-aware matcher should find ANDNOT for truth table 0x2"
        );

        // Basic gates should still work
        let matches = matcher.find_matches(0x8, 2);
        assert!(
            matches.iter().any(|m| m.cell_type.contains("AND")),
            "Library-aware matcher should find AND2 for truth table 0x8"
        );
    }

    #[test]
    fn test_mux_all_permutations() {
        let matcher = CellMatcher::new();

        // All 6 MUX permutations should match MUX2_X1
        let mux_truth_tables = [0xCA, 0xD2, 0xAC, 0xB4, 0xE4, 0xD8];

        for tt in mux_truth_tables {
            let matches = matcher.find_matches(tt, 3);
            assert!(
                matches.iter().any(|m| m.cell_type == "MUX2_X1"),
                "Expected MUX2_X1 for truth table 0x{:02X}, got: {:?}",
                tt,
                matches.iter().map(|m| &m.cell_type).collect::<Vec<_>>()
            );
        }
    }
}
