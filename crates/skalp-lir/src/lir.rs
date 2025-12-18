//! LIR - Low-level Intermediate Representation
//!
//! This module provides the word-level intermediate representation for hardware designs,
//! preserving multi-bit operations for smarter technology mapping.
//!
//! The compilation flow is: HIR → MIR → Lir → TechMapper → GateNetlist → SIR
//!
//! Key types:
//! - `Lir` - Word-level IR preserving multi-bit operations
//! - `LirOp` - Word-level operations (Add, Mux2, Reg, etc.)
//! - `PrimitiveType` - Gate-level primitive types (AND, OR, DFF, etc.)
//! - `PrimitiveId`, `NetId` - Numeric IDs for efficient lookup
//!
//! # Why Word-Level?
//!
//! - Technology libraries may have compound cells (ADDER8, MUX4, AOI22)
//! - Eager decomposition loses information needed for optimal mapping
//! - Decomposition decisions are technology-dependent

use serde::{Deserialize, Serialize};
use std::collections::HashMap;

// ============================================================================
// IDs
// ============================================================================

/// Primitive ID for unique identification within a design
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct PrimitiveId(pub u32);

/// Net ID for unique identification within a design
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct NetId(pub u32);

// ============================================================================
// Primitive Types
// ============================================================================

/// Extended gate types for technology-independent synthesis and fault simulation.
///
/// These primitives cover all basic digital logic elements needed for
/// gate-level simulation without requiring external foundry PDKs.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum PrimitiveType {
    // === Combinational Logic ===
    /// N-input AND gate
    And { inputs: u8 },
    /// N-input OR gate
    Or { inputs: u8 },
    /// N-input NAND gate
    Nand { inputs: u8 },
    /// N-input NOR gate
    Nor { inputs: u8 },
    /// 2-input XOR gate
    Xor,
    /// 2-input XNOR gate
    Xnor,
    /// Inverter
    Inv,
    /// Buffer (non-inverting)
    Buf,
    /// Tri-state buffer
    Tribuf {
        /// Enable is active high if true
        enable_active_high: bool,
    },
    /// 2:1 Multiplexer (inputs: [sel, d0, d1], output: sel ? d1 : d0)
    Mux2,
    /// 4:1 Multiplexer (inputs: [sel0, sel1, d0, d1, d2, d3])
    Mux4,
    /// N:1 Multiplexer (general case)
    MuxN {
        /// Number of select bits (2^n inputs)
        select_bits: u8,
    },

    // === Sequential Logic ===
    /// D Flip-Flop (rising edge, async active-high reset)
    DffP,
    /// D Flip-Flop (rising edge, async active-low reset)
    DffN,
    /// D Flip-Flop (falling edge triggered)
    DffNeg,
    /// D Flip-Flop with clock enable (inputs: [clk, d, en, rst])
    DffE,
    /// D Flip-Flop with async reset (inputs: [clk, d, rst])
    DffAR,
    /// D Flip-Flop with async set (inputs: [clk, d, set])
    DffAS,
    /// D Flip-Flop with scan (inputs: [clk, d, scan_in, scan_en, rst])
    DffScan,
    /// D Latch (level-sensitive, inputs: [en, d])
    Dlatch,
    /// SR Latch (inputs: [s, r])
    SRlatch,

    // === Arithmetic (for accurate FIT estimation) ===
    /// Half adder (inputs: [a, b], outputs: [sum, carry])
    HalfAdder,
    /// Full adder (inputs: [a, b, cin], outputs: [sum, cout])
    FullAdder,
    /// Comparator bit (inputs: [a, b, lt_in, eq_in], outputs: [lt_out, eq_out])
    CompBit,

    // === Memory Elements (for SRAM/register file modeling) ===
    /// Single-bit memory cell (higher SEU susceptibility)
    MemCell,
    /// Register file cell
    RegCell,

    // === Special ===
    /// Clock buffer (special timing properties)
    ClkBuf,
    /// Constant driver (outputs constant 0 or 1)
    Constant {
        /// Value to drive
        value: bool,
    },

    // === Power Infrastructure ===
    /// Level shifter for voltage domain crossing
    LevelShifter {
        /// Source voltage domain (stored as mV)
        from_voltage: u16,
        /// Target voltage domain (stored as mV)
        to_voltage: u16,
    },
    /// Isolation cell for power gating
    IsolationCell {
        /// Clamp value when isolated (0, 1, or 2 for hold-last-value)
        clamp_value: u8,
        /// Enable signal is active high if true
        enable_active_high: bool,
    },
    /// Retention flip-flop for state preservation during power-down
    RetentionDff {
        /// Has async reset
        has_reset: bool,
    },
    /// Power switch (header or footer cell)
    PowerSwitch {
        /// True for PMOS header (VDD side), false for NMOS footer (VSS side)
        is_header: bool,
    },
    /// Always-on buffer for control signal routing
    AlwaysOnBuf,
}

impl PrimitiveType {
    /// Returns true if this primitive is sequential (state-holding)
    pub fn is_sequential(&self) -> bool {
        matches!(
            self,
            PrimitiveType::DffP
                | PrimitiveType::DffN
                | PrimitiveType::DffNeg
                | PrimitiveType::DffE
                | PrimitiveType::DffAR
                | PrimitiveType::DffAS
                | PrimitiveType::DffScan
                | PrimitiveType::Dlatch
                | PrimitiveType::SRlatch
                | PrimitiveType::MemCell
                | PrimitiveType::RegCell
                | PrimitiveType::RetentionDff { .. }
        )
    }

    /// Returns true if this primitive is power infrastructure
    pub fn is_power_infrastructure(&self) -> bool {
        matches!(
            self,
            PrimitiveType::LevelShifter { .. }
                | PrimitiveType::IsolationCell { .. }
                | PrimitiveType::RetentionDff { .. }
                | PrimitiveType::PowerSwitch { .. }
                | PrimitiveType::AlwaysOnBuf
        )
    }

    /// Returns the number of input pins for this primitive type
    pub fn input_count(&self) -> u8 {
        match self {
            PrimitiveType::And { inputs } => *inputs,
            PrimitiveType::Or { inputs } => *inputs,
            PrimitiveType::Nand { inputs } => *inputs,
            PrimitiveType::Nor { inputs } => *inputs,
            PrimitiveType::Xor => 2,
            PrimitiveType::Xnor => 2,
            PrimitiveType::Inv => 1,
            PrimitiveType::Buf => 1,
            PrimitiveType::Tribuf { .. } => 2,
            PrimitiveType::Mux2 => 3,
            PrimitiveType::Mux4 => 6,
            PrimitiveType::MuxN { select_bits } => *select_bits + (1 << *select_bits),
            PrimitiveType::DffP => 3,
            PrimitiveType::DffN => 3,
            PrimitiveType::DffNeg => 3,
            PrimitiveType::DffE => 4,
            PrimitiveType::DffAR => 3,
            PrimitiveType::DffAS => 3,
            PrimitiveType::DffScan => 5,
            PrimitiveType::Dlatch => 2,
            PrimitiveType::SRlatch => 2,
            PrimitiveType::HalfAdder => 2,
            PrimitiveType::FullAdder => 3,
            PrimitiveType::CompBit => 4,
            PrimitiveType::MemCell => 3,
            PrimitiveType::RegCell => 3,
            PrimitiveType::ClkBuf => 1,
            PrimitiveType::Constant { .. } => 0,
            PrimitiveType::LevelShifter { .. } => 1,
            PrimitiveType::IsolationCell { .. } => 2,
            PrimitiveType::RetentionDff { has_reset } => {
                if *has_reset {
                    5
                } else {
                    4
                }
            }
            PrimitiveType::PowerSwitch { .. } => 1,
            PrimitiveType::AlwaysOnBuf => 1,
        }
    }

    /// Returns the number of output pins for this primitive type
    pub fn output_count(&self) -> u8 {
        match self {
            PrimitiveType::HalfAdder => 2,
            PrimitiveType::FullAdder => 2,
            PrimitiveType::CompBit => 2,
            PrimitiveType::PowerSwitch { .. } => 0,
            _ => 1,
        }
    }

    /// Returns the technology-independent base FIT rate for this primitive.
    pub fn base_fit(&self) -> f64 {
        match self {
            PrimitiveType::And { .. }
            | PrimitiveType::Or { .. }
            | PrimitiveType::Nand { .. }
            | PrimitiveType::Nor { .. } => 0.1,
            PrimitiveType::Xor | PrimitiveType::Xnor => 0.15,
            PrimitiveType::Inv | PrimitiveType::Buf => 0.05,
            PrimitiveType::Tribuf { .. } => 0.2,
            PrimitiveType::Mux2 => 0.2,
            PrimitiveType::Mux4 => 0.4,
            PrimitiveType::MuxN { select_bits } => 0.2 * (1 << *select_bits) as f64,
            PrimitiveType::DffP
            | PrimitiveType::DffN
            | PrimitiveType::DffNeg
            | PrimitiveType::DffAR
            | PrimitiveType::DffAS => 1.0,
            PrimitiveType::DffE => 1.2,
            PrimitiveType::DffScan => 1.5,
            PrimitiveType::Dlatch | PrimitiveType::SRlatch => 0.8,
            PrimitiveType::HalfAdder => 0.2,
            PrimitiveType::FullAdder => 0.3,
            PrimitiveType::CompBit => 0.15,
            PrimitiveType::MemCell => 2.0,
            PrimitiveType::RegCell => 1.5,
            PrimitiveType::ClkBuf => 0.1,
            PrimitiveType::Constant { .. } => 0.0,
            PrimitiveType::LevelShifter { .. } => 0.3,
            PrimitiveType::IsolationCell { .. } => 0.15,
            PrimitiveType::RetentionDff { .. } => 1.5,
            PrimitiveType::PowerSwitch { .. } => 0.5,
            PrimitiveType::AlwaysOnBuf => 0.1,
        }
    }

    /// Returns true if this primitive type represents memory
    pub fn is_memory(&self) -> bool {
        matches!(self, PrimitiveType::MemCell | PrimitiveType::RegCell)
    }

    /// Returns the short name for this primitive type
    pub fn short_name(&self) -> &'static str {
        match self {
            PrimitiveType::And { .. } => "AND",
            PrimitiveType::Or { .. } => "OR",
            PrimitiveType::Nand { .. } => "NAND",
            PrimitiveType::Nor { .. } => "NOR",
            PrimitiveType::Xor => "XOR",
            PrimitiveType::Xnor => "XNOR",
            PrimitiveType::Inv => "INV",
            PrimitiveType::Buf => "BUF",
            PrimitiveType::Tribuf { .. } => "TRIBUF",
            PrimitiveType::Mux2 => "MUX2",
            PrimitiveType::Mux4 => "MUX4",
            PrimitiveType::MuxN { .. } => "MUXN",
            PrimitiveType::DffP => "DFFP",
            PrimitiveType::DffN => "DFFN",
            PrimitiveType::DffNeg => "DFFNEG",
            PrimitiveType::DffE => "DFFE",
            PrimitiveType::DffAR => "DFFAR",
            PrimitiveType::DffAS => "DFFAS",
            PrimitiveType::DffScan => "DFFSCAN",
            PrimitiveType::Dlatch => "DLATCH",
            PrimitiveType::SRlatch => "SRLATCH",
            PrimitiveType::HalfAdder => "HA",
            PrimitiveType::FullAdder => "FA",
            PrimitiveType::CompBit => "COMP",
            PrimitiveType::MemCell => "MEM",
            PrimitiveType::RegCell => "REG",
            PrimitiveType::ClkBuf => "CLKBUF",
            PrimitiveType::Constant { .. } => "CONST",
            PrimitiveType::LevelShifter { .. } => "LVLSHIFT",
            PrimitiveType::IsolationCell { .. } => "ISO",
            PrimitiveType::RetentionDff { .. } => "RETDFF",
            PrimitiveType::PowerSwitch { .. } => "PWRSW",
            PrimitiveType::AlwaysOnBuf => "AONBUF",
        }
    }
}

impl std::fmt::Display for PrimitiveType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PrimitiveType::And { inputs } => write!(f, "AND{}", inputs),
            PrimitiveType::Or { inputs } => write!(f, "OR{}", inputs),
            PrimitiveType::Nand { inputs } => write!(f, "NAND{}", inputs),
            PrimitiveType::Nor { inputs } => write!(f, "NOR{}", inputs),
            PrimitiveType::MuxN { select_bits } => write!(f, "MUX{}:1", 1 << select_bits),
            PrimitiveType::Tribuf { enable_active_high } => {
                if *enable_active_high {
                    write!(f, "TRIBUF")
                } else {
                    write!(f, "TRIBUFN")
                }
            }
            PrimitiveType::Constant { value } => write!(f, "CONST{}", if *value { 1 } else { 0 }),
            PrimitiveType::LevelShifter {
                from_voltage,
                to_voltage,
            } => {
                write!(f, "LVLSHIFT_{}mV_{}mV", from_voltage, to_voltage)
            }
            PrimitiveType::IsolationCell { clamp_value, .. } => {
                let clamp = match clamp_value {
                    0 => "LO",
                    1 => "HI",
                    _ => "HOLD",
                };
                write!(f, "ISO_{}", clamp)
            }
            PrimitiveType::RetentionDff { has_reset } => {
                if *has_reset {
                    write!(f, "RETDFFR")
                } else {
                    write!(f, "RETDFF")
                }
            }
            PrimitiveType::PowerSwitch { is_header } => {
                if *is_header {
                    write!(f, "PWRSW_HDR")
                } else {
                    write!(f, "PWRSW_FTR")
                }
            }
            _ => write!(f, "{}", self.short_name()),
        }
    }
}

// ============================================================================
// Safety Info
// ============================================================================

/// Safety information for primitives/cells
/// Used to propagate safety context from MIR to gate-level cells
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct LirSafetyInfo {
    /// Safety goal this primitive helps implement
    pub goal_name: Option<String>,
    /// Name of the safety mechanism
    pub mechanism_name: Option<String>,
    /// True if this is a safety mechanism of another safety mechanism
    pub is_sm_of_sm: bool,
    /// Protected mechanism name (for SM-of-SM)
    pub protected_sm_name: Option<String>,
    /// True if this hardware is only active during boot/test mode
    #[serde(default)]
    pub is_boot_time_only: bool,
}

// ============================================================================
// Hierarchy
// ============================================================================

/// Hierarchy node for traceability back to RTL.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HierarchyNode {
    /// Instance path (e.g., "top.cpu.alu")
    pub path: String,
    /// Original RTL module name
    pub module: String,
    /// Primitive ID range [start, end) belonging to this instance
    pub primitive_range: (u32, u32),
    /// Parent node index (None for top)
    pub parent: Option<usize>,
    /// Child node indices
    pub children: Vec<usize>,
}

// ============================================================================
// FIT Overrides
// ============================================================================

/// Configuration for FIT rate overrides.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct FitOverrides {
    /// Override FIT for specific primitive types (e.g., "DFFP" => 0.8)
    pub primitive_overrides: HashMap<String, f64>,
    /// Global scaling factor (e.g., 0.7 for hardened process)
    pub global_scale: f64,
    /// Temperature derating factor
    pub temperature_factor: f64,
    /// Voltage derating factor
    pub voltage_factor: f64,
}

impl FitOverrides {
    /// Create default overrides (no modifications)
    pub fn none() -> Self {
        Self {
            primitive_overrides: HashMap::new(),
            global_scale: 1.0,
            temperature_factor: 1.0,
            voltage_factor: 1.0,
        }
    }

    /// Get the effective FIT rate for a primitive type
    pub fn effective_fit(&self, ptype: &PrimitiveType) -> f64 {
        let base = ptype.base_fit();
        let name = ptype.short_name();
        let type_fit = self.primitive_overrides.get(name).copied().unwrap_or(base);
        type_fit * self.global_scale * self.temperature_factor * self.voltage_factor
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_primitive_type_properties() {
        assert!(PrimitiveType::DffP.is_sequential());
        assert!(PrimitiveType::Dlatch.is_sequential());
        assert!(PrimitiveType::MemCell.is_sequential());
        assert!(!PrimitiveType::And { inputs: 2 }.is_sequential());
        assert!(!PrimitiveType::Mux2.is_sequential());
    }

    #[test]
    fn test_primitive_type_pin_counts() {
        assert_eq!(PrimitiveType::And { inputs: 4 }.input_count(), 4);
        assert_eq!(PrimitiveType::Inv.input_count(), 1);
        assert_eq!(PrimitiveType::Mux2.input_count(), 3);
        assert_eq!(PrimitiveType::DffP.input_count(), 3);
        assert_eq!(PrimitiveType::And { inputs: 4 }.output_count(), 1);
        assert_eq!(PrimitiveType::FullAdder.output_count(), 2);
    }

    #[test]
    fn test_primitive_type_fit() {
        assert!(PrimitiveType::DffP.base_fit() > PrimitiveType::And { inputs: 2 }.base_fit());
        assert!(PrimitiveType::MemCell.base_fit() > PrimitiveType::DffP.base_fit());
        assert_eq!(PrimitiveType::Constant { value: true }.base_fit(), 0.0);
    }

    #[test]
    fn test_primitive_type_display() {
        assert_eq!(format!("{}", PrimitiveType::And { inputs: 3 }), "AND3");
        assert_eq!(
            format!("{}", PrimitiveType::MuxN { select_bits: 3 }),
            "MUX8:1"
        );
        assert_eq!(format!("{}", PrimitiveType::DffP), "DFFP");
    }

    #[test]
    fn test_fit_overrides() {
        let mut overrides = FitOverrides::none();
        overrides
            .primitive_overrides
            .insert("DFFP".to_string(), 0.5);
        overrides.global_scale = 0.8;

        let dff_fit = overrides.effective_fit(&PrimitiveType::DffP);
        assert_eq!(dff_fit, 0.5 * 0.8);

        let and_fit = overrides.effective_fit(&PrimitiveType::And { inputs: 2 });
        assert_eq!(and_fit, 0.1 * 0.8);
    }
}

// ============================================================================
// Word-Level LIR Types
// ============================================================================

/// Word-level operation types.
///
/// These represent multi-bit operations that will be decomposed
/// during technology mapping based on available library cells.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum LirOp {
    // === Arithmetic ===
    /// Addition: result = a + b (with optional carry-out)
    Add { width: u32, has_carry: bool },
    /// Subtraction: result = a - b (with optional borrow-out)
    Sub { width: u32, has_borrow: bool },
    /// Multiplication: result = a * b
    Mul { width: u32, result_width: u32 },

    // === Bitwise Logic ===
    /// Bitwise AND
    And { width: u32 },
    /// Bitwise OR
    Or { width: u32 },
    /// Bitwise XOR
    Xor { width: u32 },
    /// Bitwise NOT (invert)
    Not { width: u32 },
    /// Bitwise NAND
    Nand { width: u32 },
    /// Bitwise NOR
    Nor { width: u32 },

    // === Comparison ===
    /// Equality: result = (a == b)
    Eq { width: u32 },
    /// Not equal: result = (a != b)
    Ne { width: u32 },
    /// Less than (unsigned): result = (a < b)
    Lt { width: u32 },
    /// Less than or equal (unsigned): result = (a <= b)
    Le { width: u32 },
    /// Greater than (unsigned): result = (a > b)
    Gt { width: u32 },
    /// Greater than or equal (unsigned): result = (a >= b)
    Ge { width: u32 },
    /// Less than (signed)
    Slt { width: u32 },

    // === Multiplexing ===
    /// 2:1 Multiplexer: result = sel ? b : a
    Mux2 { width: u32 },
    /// N:1 Multiplexer
    MuxN { width: u32, ways: u32 },

    // === Shift ===
    /// Logical left shift
    Shl { width: u32 },
    /// Logical right shift
    Shr { width: u32 },
    /// Arithmetic right shift (sign-extend)
    Sar { width: u32 },
    /// Rotate left
    Rol { width: u32 },
    /// Rotate right
    Ror { width: u32 },

    // === Reduction ===
    /// Reduction AND: result = &a (all bits ANDed)
    RedAnd { width: u32 },
    /// Reduction OR: result = |a (all bits ORed)
    RedOr { width: u32 },
    /// Reduction XOR: result = ^a (all bits XORed, parity)
    RedXor { width: u32 },

    // === Bit Manipulation ===
    /// Concatenation: result = {a, b, ...}
    Concat { widths: Vec<u32> },
    /// Bit select: result = a[index]
    BitSelect { width: u32 },
    /// Range select: result = a[high:low]
    RangeSelect { width: u32, high: u32, low: u32 },
    /// Zero extend
    ZeroExtend { from: u32, to: u32 },
    /// Sign extend
    SignExtend { from: u32, to: u32 },

    // === Sequential ===
    /// Register (D flip-flop array)
    Reg {
        width: u32,
        has_enable: bool,
        has_reset: bool,
        reset_value: Option<u64>,
    },
    /// Latch array
    Latch { width: u32 },

    // === Memory ===
    /// Memory read port
    MemRead { data_width: u32, addr_width: u32 },
    /// Memory write port
    MemWrite { data_width: u32, addr_width: u32 },

    // === Special ===
    /// Constant value
    Constant { width: u32, value: u64 },
    /// Buffer (for fanout or pipeline)
    Buffer { width: u32 },
    /// Tristate buffer
    Tristate { width: u32 },
}

impl LirOp {
    /// Returns the output width of this operation
    pub fn output_width(&self) -> u32 {
        match self {
            LirOp::Add { width, has_carry } => {
                if *has_carry {
                    width + 1
                } else {
                    *width
                }
            }
            LirOp::Sub { width, has_borrow } => {
                if *has_borrow {
                    width + 1
                } else {
                    *width
                }
            }
            LirOp::Mul { result_width, .. } => *result_width,
            LirOp::And { width }
            | LirOp::Or { width }
            | LirOp::Xor { width }
            | LirOp::Not { width }
            | LirOp::Nand { width }
            | LirOp::Nor { width } => *width,
            LirOp::Eq { .. }
            | LirOp::Ne { .. }
            | LirOp::Lt { .. }
            | LirOp::Le { .. }
            | LirOp::Gt { .. }
            | LirOp::Ge { .. }
            | LirOp::Slt { .. } => 1, // Comparison results are 1-bit
            LirOp::Mux2 { width } => *width,
            LirOp::MuxN { width, .. } => *width,
            LirOp::Shl { width }
            | LirOp::Shr { width }
            | LirOp::Sar { width }
            | LirOp::Rol { width }
            | LirOp::Ror { width } => *width,
            LirOp::RedAnd { .. } | LirOp::RedOr { .. } | LirOp::RedXor { .. } => 1,
            LirOp::Concat { widths } => widths.iter().sum(),
            LirOp::BitSelect { .. } => 1,
            LirOp::RangeSelect { high, low, .. } => high - low + 1,
            LirOp::ZeroExtend { to, .. } | LirOp::SignExtend { to, .. } => *to,
            LirOp::Reg { width, .. } | LirOp::Latch { width } => *width,
            LirOp::MemRead { data_width, .. } => *data_width,
            LirOp::MemWrite { .. } => 0, // Write has no data output
            LirOp::Constant { width, .. } | LirOp::Buffer { width } | LirOp::Tristate { width } => {
                *width
            }
        }
    }

    /// Returns the number of input operands
    pub fn input_count(&self) -> usize {
        match self {
            LirOp::Not { .. }
            | LirOp::RedAnd { .. }
            | LirOp::RedOr { .. }
            | LirOp::RedXor { .. } => 1,
            LirOp::Add { .. }
            | LirOp::Sub { .. }
            | LirOp::Mul { .. }
            | LirOp::And { .. }
            | LirOp::Or { .. }
            | LirOp::Xor { .. }
            | LirOp::Nand { .. }
            | LirOp::Nor { .. }
            | LirOp::Eq { .. }
            | LirOp::Ne { .. }
            | LirOp::Lt { .. }
            | LirOp::Le { .. }
            | LirOp::Gt { .. }
            | LirOp::Ge { .. }
            | LirOp::Slt { .. }
            | LirOp::Shl { .. }
            | LirOp::Shr { .. }
            | LirOp::Sar { .. }
            | LirOp::Rol { .. }
            | LirOp::Ror { .. } => 2,
            LirOp::Mux2 { .. } => 3,                        // sel, a, b
            LirOp::MuxN { ways, .. } => 1 + *ways as usize, // sel + inputs
            LirOp::Concat { widths } => widths.len(),
            LirOp::BitSelect { .. } => 2,   // data, index
            LirOp::RangeSelect { .. } => 1, // data (range is fixed)
            LirOp::ZeroExtend { .. } | LirOp::SignExtend { .. } => 1,
            LirOp::Reg {
                has_enable,
                has_reset,
                ..
            } => {
                1 + if *has_enable { 1 } else { 0 } + if *has_reset { 1 } else { 0 }
                // d + optional en + optional rst
            }
            LirOp::Latch { .. } => 2,    // en, d
            LirOp::MemRead { .. } => 1,  // addr
            LirOp::MemWrite { .. } => 3, // addr, data, we
            LirOp::Constant { .. } => 0,
            LirOp::Buffer { .. } => 1,
            LirOp::Tristate { .. } => 2, // data, enable
        }
    }

    /// Returns true if this operation is sequential (state-holding)
    pub fn is_sequential(&self) -> bool {
        matches!(
            self,
            LirOp::Reg { .. }
                | LirOp::Latch { .. }
                | LirOp::MemRead { .. }
                | LirOp::MemWrite { .. }
        )
    }
}

// ============================================================================
// Word-Level Netlist
// ============================================================================

/// Unique identifier for a LIR node
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct LirNodeId(pub u32);

/// Unique identifier for a LIR signal
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct LirSignalId(pub u32);

/// A node in the LIR netlist (an operation instance)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LirNode {
    /// Unique ID
    pub id: LirNodeId,
    /// Operation type
    pub op: LirOp,
    /// Input signal IDs
    pub inputs: Vec<LirSignalId>,
    /// Output signal ID
    pub output: LirSignalId,
    /// Hierarchical path for traceability
    pub path: String,
    /// Clock signal (for sequential ops)
    pub clock: Option<LirSignalId>,
    /// Reset signal (for sequential ops)
    pub reset: Option<LirSignalId>,
}

/// A signal in the LIR netlist
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LirSignal {
    /// Unique ID
    pub id: LirSignalId,
    /// Signal name
    pub name: String,
    /// Bit width
    pub width: u32,
    /// Driver node (None for primary inputs)
    pub driver: Option<LirNodeId>,
    /// Is this a primary input?
    pub is_input: bool,
    /// Is this a primary output?
    pub is_output: bool,
    /// Is this a detection signal (for safety analysis)?
    /// Set via #[detection_signal] attribute on port
    #[serde(default)]
    pub is_detection: bool,
}

/// Low-level Intermediate Representation (word-level)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Lir {
    /// Design name
    pub name: String,
    /// All nodes (operations)
    pub nodes: Vec<LirNode>,
    /// All signals
    pub signals: Vec<LirSignal>,
    /// Primary input signal IDs
    pub inputs: Vec<LirSignalId>,
    /// Primary output signal IDs
    pub outputs: Vec<LirSignalId>,
    /// Clock signals
    pub clocks: Vec<LirSignalId>,
    /// Reset signals
    pub resets: Vec<LirSignalId>,
    /// Detection signals (for safety analysis)
    /// These are output signals marked with #[detection_signal]
    #[serde(default)]
    pub detection_signals: Vec<LirSignalId>,
    /// Signal name to ID mapping
    signal_map: HashMap<String, LirSignalId>,
    /// Module-level safety information (from MIR SafetyContext)
    /// Propagated to all cells during technology mapping
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub module_safety_info: Option<LirSafetyInfo>,
}

impl Lir {
    /// Create a new empty LIR
    pub fn new(name: String) -> Self {
        Self {
            name,
            nodes: Vec::new(),
            signals: Vec::new(),
            inputs: Vec::new(),
            outputs: Vec::new(),
            clocks: Vec::new(),
            resets: Vec::new(),
            detection_signals: Vec::new(),
            signal_map: HashMap::new(),
            module_safety_info: None,
        }
    }

    /// Add a signal and return its ID
    pub fn add_signal(&mut self, name: String, width: u32) -> LirSignalId {
        let id = LirSignalId(self.signals.len() as u32);
        self.signal_map.insert(name.clone(), id);
        self.signals.push(LirSignal {
            id,
            name,
            width,
            driver: None,
            is_input: false,
            is_output: false,
            is_detection: false,
        });
        id
    }

    /// Add a primary input signal
    pub fn add_input(&mut self, name: String, width: u32) -> LirSignalId {
        let id = self.add_signal(name, width);
        self.signals[id.0 as usize].is_input = true;
        self.inputs.push(id);
        id
    }

    /// Add a primary output signal
    pub fn add_output(&mut self, name: String, width: u32) -> LirSignalId {
        let id = self.add_signal(name, width);
        self.signals[id.0 as usize].is_output = true;
        self.outputs.push(id);
        id
    }

    /// Add a detection signal output (for safety analysis)
    pub fn add_detection_output(&mut self, name: String, width: u32) -> LirSignalId {
        let id = self.add_output(name, width);
        self.signals[id.0 as usize].is_detection = true;
        self.detection_signals.push(id);
        id
    }

    /// Mark an existing signal as a detection signal
    pub fn mark_as_detection(&mut self, id: LirSignalId) {
        if let Some(signal) = self.signals.get_mut(id.0 as usize) {
            signal.is_detection = true;
            if !self.detection_signals.contains(&id) {
                self.detection_signals.push(id);
            }
        }
    }

    /// Add a node (operation) and return its ID
    pub fn add_node(
        &mut self,
        op: LirOp,
        inputs: Vec<LirSignalId>,
        output: LirSignalId,
        path: String,
    ) -> LirNodeId {
        let id = LirNodeId(self.nodes.len() as u32);
        self.signals[output.0 as usize].driver = Some(id);
        self.nodes.push(LirNode {
            id,
            op,
            inputs,
            output,
            path,
            clock: None,
            reset: None,
        });
        id
    }

    /// Add a sequential node with clock and reset
    pub fn add_seq_node(
        &mut self,
        op: LirOp,
        inputs: Vec<LirSignalId>,
        output: LirSignalId,
        path: String,
        clock: LirSignalId,
        reset: Option<LirSignalId>,
    ) -> LirNodeId {
        let id = LirNodeId(self.nodes.len() as u32);
        self.signals[output.0 as usize].driver = Some(id);
        self.nodes.push(LirNode {
            id,
            op,
            inputs,
            output,
            path,
            clock: Some(clock),
            reset,
        });
        id
    }

    /// Get a signal by name
    pub fn get_signal(&self, name: &str) -> Option<&LirSignal> {
        self.signal_map
            .get(name)
            .map(|id| &self.signals[id.0 as usize])
    }

    /// Get total number of operations
    pub fn node_count(&self) -> usize {
        self.nodes.len()
    }

    /// Get total number of signals
    pub fn signal_count(&self) -> usize {
        self.signals.len()
    }

    /// Calculate total bit-width of all signals (for statistics)
    pub fn total_bits(&self) -> u32 {
        self.signals.iter().map(|s| s.width).sum()
    }
}

// ============================================================================
// Statistics
// ============================================================================

/// Statistics for LIR
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct LirStats {
    /// Total nodes
    pub total_nodes: usize,
    /// Total signals
    pub total_signals: usize,
    /// Total bits
    pub total_bits: u32,
    /// Arithmetic ops
    pub arithmetic_ops: usize,
    /// Logic ops
    pub logic_ops: usize,
    /// Comparison ops
    pub comparison_ops: usize,
    /// Mux ops
    pub mux_ops: usize,
    /// Sequential ops
    pub sequential_ops: usize,
}

impl LirStats {
    /// Calculate statistics from a Lir
    pub fn from_lir(lir: &Lir) -> Self {
        let mut stats = Self {
            total_nodes: lir.nodes.len(),
            total_signals: lir.signals.len(),
            total_bits: lir.total_bits(),
            ..Default::default()
        };

        for node in &lir.nodes {
            match &node.op {
                LirOp::Add { .. } | LirOp::Sub { .. } | LirOp::Mul { .. } => {
                    stats.arithmetic_ops += 1;
                }
                LirOp::And { .. }
                | LirOp::Or { .. }
                | LirOp::Xor { .. }
                | LirOp::Not { .. }
                | LirOp::Nand { .. }
                | LirOp::Nor { .. } => {
                    stats.logic_ops += 1;
                }
                LirOp::Eq { .. }
                | LirOp::Ne { .. }
                | LirOp::Lt { .. }
                | LirOp::Le { .. }
                | LirOp::Gt { .. }
                | LirOp::Ge { .. }
                | LirOp::Slt { .. } => {
                    stats.comparison_ops += 1;
                }
                LirOp::Mux2 { .. } | LirOp::MuxN { .. } => {
                    stats.mux_ops += 1;
                }
                LirOp::Reg { .. } | LirOp::Latch { .. } => {
                    stats.sequential_ops += 1;
                }
                _ => {}
            }
        }

        stats
    }
}

// ============================================================================
// Backward Compatibility Type Aliases
// ============================================================================

/// Type alias for backward compatibility
pub type WordLir = Lir;
/// Type alias for backward compatibility
pub type WordOp = LirOp;
/// Type alias for backward compatibility
pub type WordNode = LirNode;
/// Type alias for backward compatibility
pub type WordSignal = LirSignal;
/// Type alias for backward compatibility
pub type WordNodeId = LirNodeId;
/// Type alias for backward compatibility
pub type WordSignalId = LirSignalId;
/// Type alias for backward compatibility
pub type WordLirStats = LirStats;

// ============================================================================
// LIR Tests
// ============================================================================

#[cfg(test)]
mod lir_tests {
    use super::*;

    #[test]
    fn test_lir_op_output_width() {
        assert_eq!(
            LirOp::Add {
                width: 8,
                has_carry: false
            }
            .output_width(),
            8
        );
        assert_eq!(
            LirOp::Add {
                width: 8,
                has_carry: true
            }
            .output_width(),
            9
        );
        assert_eq!(LirOp::Eq { width: 32 }.output_width(), 1);
        assert_eq!(LirOp::Mux2 { width: 16 }.output_width(), 16);
    }

    #[test]
    fn test_lir_creation() {
        let mut lir = Lir::new("test".to_string());

        let a = lir.add_input("a".to_string(), 8);
        let b = lir.add_input("b".to_string(), 8);
        let sum = lir.add_signal("sum".to_string(), 8);
        let out = lir.add_output("out".to_string(), 8);

        lir.add_node(
            LirOp::Add {
                width: 8,
                has_carry: false,
            },
            vec![a, b],
            sum,
            "add".to_string(),
        );

        lir.add_node(
            LirOp::Buffer { width: 8 },
            vec![sum],
            out,
            "out_buf".to_string(),
        );

        assert_eq!(lir.node_count(), 2);
        assert_eq!(lir.signal_count(), 4);
        assert_eq!(lir.inputs.len(), 2);
        assert_eq!(lir.outputs.len(), 1);
    }

    #[test]
    fn test_lir_op_is_sequential() {
        assert!(LirOp::Reg {
            width: 8,
            has_enable: false,
            has_reset: true,
            reset_value: Some(0)
        }
        .is_sequential());
        assert!(!LirOp::Add {
            width: 8,
            has_carry: false
        }
        .is_sequential());
    }
}
