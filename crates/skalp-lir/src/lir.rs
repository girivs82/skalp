//! LIR - Low-level Intermediate Representation
//!
//! Gate-level representation of hardware designs for simulation and fault analysis.
//!
//! This module provides technology-independent primitive types (gates, flip-flops, muxes, etc.)
//! for gate-level simulation and ISO 26262 safety analysis without requiring external synthesized netlists.
//!
//! The compilation flow is: HIR → MIR → LIR → SIR (for simulation)
//!
//! Key types:
//! - `Lir` - The gate-level netlist (collection of primitives and nets)
//! - `Primitive` - Individual gate/flip-flop/mux with hierarchical path for traceability
//! - `PrimitiveType` - Type of primitive (AND, OR, DFF, MUX2, etc.)
//! - `LirNet` - Wire connecting primitives
//! - `NetId`, `PrimitiveId` - Numeric IDs for efficient lookup

use serde::{Deserialize, Serialize};
use std::collections::HashMap;

// ============================================================================
// Extended Primitive Types for Fault Simulation
// ============================================================================

/// Primitive ID for unique identification within a design
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct PrimitiveId(pub u32);

/// Net ID for unique identification within a design
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct NetId(pub u32);

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
            PrimitiveType::Tribuf { .. } => 2, // data, enable
            PrimitiveType::Mux2 => 3,          // sel, d0, d1
            PrimitiveType::Mux4 => 6,          // sel0, sel1, d0-d3
            PrimitiveType::MuxN { select_bits } => *select_bits + (1 << *select_bits),
            PrimitiveType::DffP => 3,      // clk, d, rst
            PrimitiveType::DffN => 3,      // clk, d, rst
            PrimitiveType::DffNeg => 3,    // clk, d, rst
            PrimitiveType::DffE => 4,      // clk, d, en, rst
            PrimitiveType::DffAR => 3,     // clk, d, rst
            PrimitiveType::DffAS => 3,     // clk, d, set
            PrimitiveType::DffScan => 5,   // clk, d, scan_in, scan_en, rst
            PrimitiveType::Dlatch => 2,    // en, d
            PrimitiveType::SRlatch => 2,   // s, r
            PrimitiveType::HalfAdder => 2, // a, b
            PrimitiveType::FullAdder => 3, // a, b, cin
            PrimitiveType::CompBit => 4,   // a, b, lt_in, eq_in
            PrimitiveType::MemCell => 3,   // data_in, write_en, clk
            PrimitiveType::RegCell => 3,   // data_in, write_en, clk
            PrimitiveType::ClkBuf => 1,
            PrimitiveType::Constant { .. } => 0,
        }
    }

    /// Returns the number of output pins for this primitive type
    pub fn output_count(&self) -> u8 {
        match self {
            PrimitiveType::HalfAdder => 2, // sum, carry
            PrimitiveType::FullAdder => 2, // sum, cout
            PrimitiveType::CompBit => 2,   // lt_out, eq_out
            _ => 1,                        // All others have single output
        }
    }

    /// Returns the technology-independent base FIT rate for this primitive.
    ///
    /// FIT = Failures In Time = failures per billion device hours
    /// These are representative values from industry averages.
    /// They can be overridden via FitOverrides for specific processes.
    pub fn base_fit(&self) -> f64 {
        match self {
            // Combinational logic - low FIT, mainly SETs
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

            // Sequential logic - higher FIT due to SEUs
            PrimitiveType::DffP
            | PrimitiveType::DffN
            | PrimitiveType::DffNeg
            | PrimitiveType::DffAR
            | PrimitiveType::DffAS => 1.0,
            PrimitiveType::DffE => 1.2,
            PrimitiveType::DffScan => 1.5,
            PrimitiveType::Dlatch | PrimitiveType::SRlatch => 0.8,

            // Arithmetic - moderate FIT
            PrimitiveType::HalfAdder => 0.2,
            PrimitiveType::FullAdder => 0.3,
            PrimitiveType::CompBit => 0.15,

            // Memory - highest FIT per bit due to SEU susceptibility
            PrimitiveType::MemCell => 2.0,
            PrimitiveType::RegCell => 1.5,

            // Special
            PrimitiveType::ClkBuf => 0.1,
            PrimitiveType::Constant { .. } => 0.0, // No failures possible
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
            _ => write!(f, "{}", self.short_name()),
        }
    }
}

/// Primitive instance with hierarchical path for traceability.
///
/// Each primitive represents a single gate, flip-flop, or memory cell
/// in the flattened gate-level netlist.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Primitive {
    /// Unique ID within the design
    pub id: PrimitiveId,
    /// Primitive type
    pub ptype: PrimitiveType,
    /// Hierarchical path (e.g., "top.cpu.alu.adder_bit3")
    /// Used for traceability back to RTL and error reporting
    pub path: String,
    /// Input net IDs (order depends on primitive type)
    pub inputs: Vec<NetId>,
    /// Output net IDs (order depends on primitive type)
    pub outputs: Vec<NetId>,
    /// Clock net (for sequential elements)
    pub clock: Option<NetId>,
    /// Reset net (for sequential elements)
    pub reset: Option<NetId>,
    /// Enable net (for gated elements)
    pub enable: Option<NetId>,
    /// Bit index within multi-bit signal (for traceability)
    pub bit_index: Option<u32>,
}

impl Primitive {
    /// Create a new combinational primitive
    pub fn new_comb(
        id: PrimitiveId,
        ptype: PrimitiveType,
        path: String,
        inputs: Vec<NetId>,
        outputs: Vec<NetId>,
    ) -> Self {
        Self {
            id,
            ptype,
            path,
            inputs,
            outputs,
            clock: None,
            reset: None,
            enable: None,
            bit_index: None,
        }
    }

    /// Create a new sequential primitive (flip-flop)
    pub fn new_seq(
        id: PrimitiveId,
        ptype: PrimitiveType,
        path: String,
        inputs: Vec<NetId>,
        outputs: Vec<NetId>,
        clock: NetId,
        reset: Option<NetId>,
    ) -> Self {
        Self {
            id,
            ptype,
            path,
            inputs,
            outputs,
            clock: Some(clock),
            reset,
            enable: None,
            bit_index: None,
        }
    }

    /// Returns the FIT rate for this primitive
    pub fn fit(&self) -> f64 {
        self.ptype.base_fit()
    }
}

/// Net in gate-level netlist.
///
/// Represents a wire connecting primitive outputs to inputs.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LirNet {
    /// Net ID
    pub id: NetId,
    /// Name (for debugging and traceability)
    pub name: String,
    /// Driver primitive and output pin (None for primary inputs)
    pub driver: Option<(PrimitiveId, u8)>,
    /// Load primitives and input pins
    pub loads: Vec<(PrimitiveId, u8)>,
    /// Is this a primary input?
    pub is_primary_input: bool,
    /// Is this a primary output?
    pub is_primary_output: bool,
    /// Is this driven by a state element (flip-flop output)?
    pub is_state_output: bool,
    /// Bit width (usually 1 for gate-level)
    pub width: u8,
}

impl LirNet {
    /// Create a new net
    pub fn new(id: NetId, name: String) -> Self {
        Self {
            id,
            name,
            driver: None,
            loads: Vec::new(),
            is_primary_input: false,
            is_primary_output: false,
            is_state_output: false,
            width: 1,
        }
    }

    /// Create a primary input net
    pub fn new_primary_input(id: NetId, name: String) -> Self {
        Self {
            id,
            name,
            driver: None,
            loads: Vec::new(),
            is_primary_input: true,
            is_primary_output: false,
            is_state_output: false,
            width: 1,
        }
    }

    /// Create a primary output net
    pub fn new_primary_output(id: NetId, name: String, driver: (PrimitiveId, u8)) -> Self {
        Self {
            id,
            name,
            driver: Some(driver),
            loads: Vec::new(),
            is_primary_input: false,
            is_primary_output: true,
            is_state_output: false,
            width: 1,
        }
    }

    /// Returns the fanout (number of loads)
    pub fn fanout(&self) -> usize {
        self.loads.len()
    }
}

/// Hierarchy node for traceability back to RTL.
///
/// Maps primitive ID ranges to their source RTL module instances.
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

/// Netlist statistics for reporting.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct NetlistStats {
    /// Total number of primitives
    pub total_primitives: u64,
    /// Number of combinational gates
    pub comb_gates: u64,
    /// Number of flip-flops
    pub flip_flops: u64,
    /// Number of latches
    pub latches: u64,
    /// Number of muxes
    pub muxes: u64,
    /// Number of memory cells
    pub mem_cells: u64,
    /// Total number of nets
    pub total_nets: u64,
    /// Maximum fanout
    pub max_fanout: u32,
    /// Combinational logic depth (longest path)
    pub logic_depth: u32,
    /// Total estimated FIT
    pub total_fit: f64,
    /// Breakdown by primitive type
    pub primitive_counts: HashMap<String, u64>,
}

impl NetlistStats {
    /// Calculate statistics from a netlist
    pub fn from_netlist(netlist: &Lir) -> Self {
        let mut stats = NetlistStats {
            total_primitives: netlist.primitives.len() as u64,
            total_nets: netlist.nets.len() as u64,
            ..Default::default()
        };

        for prim in &netlist.primitives {
            stats.total_fit += prim.fit();

            let name = prim.ptype.short_name().to_string();
            *stats.primitive_counts.entry(name).or_insert(0) += 1;

            match &prim.ptype {
                PrimitiveType::Mux2 | PrimitiveType::Mux4 | PrimitiveType::MuxN { .. } => {
                    stats.muxes += 1;
                }
                PrimitiveType::DffP
                | PrimitiveType::DffN
                | PrimitiveType::DffNeg
                | PrimitiveType::DffE
                | PrimitiveType::DffAR
                | PrimitiveType::DffAS
                | PrimitiveType::DffScan => {
                    stats.flip_flops += 1;
                }
                PrimitiveType::Dlatch | PrimitiveType::SRlatch => {
                    stats.latches += 1;
                }
                PrimitiveType::MemCell | PrimitiveType::RegCell => {
                    stats.mem_cells += 1;
                }
                _ => {
                    stats.comb_gates += 1;
                }
            }
        }

        for net in &netlist.nets {
            if net.fanout() as u32 > stats.max_fanout {
                stats.max_fanout = net.fanout() as u32;
            }
        }

        stats
    }
}

/// Technology-independent gate-level netlist.
///
/// This is the flattened, elaborated representation of a design
/// used for gate-level simulation and fault injection.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Lir {
    /// Design name
    pub name: String,
    /// Original module hierarchy (for traceability)
    pub hierarchy: Vec<HierarchyNode>,
    /// All primitives in flattened design
    pub primitives: Vec<Primitive>,
    /// All nets in flattened design
    pub nets: Vec<LirNet>,
    /// Primary input net IDs
    pub inputs: Vec<NetId>,
    /// Primary output net IDs
    pub outputs: Vec<NetId>,
    /// Clock net IDs
    pub clocks: Vec<NetId>,
    /// Reset net IDs
    pub resets: Vec<NetId>,
    /// Statistics
    pub stats: NetlistStats,
}

impl Lir {
    /// Create a new empty gate netlist
    pub fn new(name: String) -> Self {
        Self {
            name,
            hierarchy: Vec::new(),
            primitives: Vec::new(),
            nets: Vec::new(),
            inputs: Vec::new(),
            outputs: Vec::new(),
            clocks: Vec::new(),
            resets: Vec::new(),
            stats: NetlistStats::default(),
        }
    }

    /// Add a primitive to the netlist
    pub fn add_primitive(&mut self, prim: Primitive) -> PrimitiveId {
        let id = prim.id;
        self.primitives.push(prim);
        id
    }

    /// Add a net to the netlist
    pub fn add_net(&mut self, net: LirNet) -> NetId {
        let id = net.id;
        self.nets.push(net);
        id
    }

    /// Get a primitive by ID
    pub fn get_primitive(&self, id: PrimitiveId) -> Option<&Primitive> {
        self.primitives.iter().find(|p| p.id == id)
    }

    /// Get a net by ID
    pub fn get_net(&self, id: NetId) -> Option<&LirNet> {
        self.nets.iter().find(|n| n.id == id)
    }

    /// Get mutable primitive by ID
    pub fn get_primitive_mut(&mut self, id: PrimitiveId) -> Option<&mut Primitive> {
        self.primitives.iter_mut().find(|p| p.id == id)
    }

    /// Get mutable net by ID
    pub fn get_net_mut(&mut self, id: NetId) -> Option<&mut LirNet> {
        self.nets.iter_mut().find(|n| n.id == id)
    }

    /// Calculate and update statistics
    pub fn update_stats(&mut self) {
        self.stats = NetlistStats::from_netlist(self);
    }

    /// Get all sequential primitives (flip-flops, latches, memory)
    pub fn sequential_primitives(&self) -> Vec<&Primitive> {
        self.primitives
            .iter()
            .filter(|p| p.ptype.is_sequential())
            .collect()
    }

    /// Get all combinational primitives
    pub fn combinational_primitives(&self) -> Vec<&Primitive> {
        self.primitives
            .iter()
            .filter(|p| !p.ptype.is_sequential())
            .collect()
    }

    /// Get primitive path from hierarchy
    pub fn get_rtl_path(&self, prim_id: PrimitiveId) -> Option<&str> {
        self.get_primitive(prim_id).map(|p| p.path.as_str())
    }

    /// Get all primitives in a hierarchical instance
    pub fn primitives_in_instance(&self, instance_path: &str) -> Vec<&Primitive> {
        self.primitives
            .iter()
            .filter(|p| p.path.starts_with(instance_path))
            .collect()
    }
}

/// Configuration for FIT rate overrides.
///
/// Allows users to customize FIT rates for specific processes or conditions.
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

        // Check for type-specific override
        let type_fit = self.primitive_overrides.get(name).copied().unwrap_or(base);

        // Apply global factors
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
        // Sequential elements have higher FIT
        assert!(PrimitiveType::DffP.base_fit() > PrimitiveType::And { inputs: 2 }.base_fit());
        // Memory has highest FIT
        assert!(PrimitiveType::MemCell.base_fit() > PrimitiveType::DffP.base_fit());
        // Constant has no failures
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
    fn test_lir_creation() {
        let mut lir = Lir::new("test".to_string());

        // Add some primitives
        let prim = Primitive::new_comb(
            PrimitiveId(0),
            PrimitiveType::And { inputs: 2 },
            "top.and_gate".to_string(),
            vec![NetId(0), NetId(1)],
            vec![NetId(2)],
        );
        lir.add_primitive(prim);

        // Add some nets
        let net_a = LirNet::new_primary_input(NetId(0), "a".to_string());
        let net_b = LirNet::new_primary_input(NetId(1), "b".to_string());
        let net_out = LirNet::new_primary_output(NetId(2), "out".to_string(), (PrimitiveId(0), 0));
        lir.add_net(net_a);
        lir.add_net(net_b);
        lir.add_net(net_out);

        lir.update_stats();

        assert_eq!(lir.stats.total_primitives, 1);
        assert_eq!(lir.stats.comb_gates, 1);
        assert_eq!(lir.stats.total_nets, 3);
    }

    #[test]
    fn test_fit_overrides() {
        let mut overrides = FitOverrides::none();
        overrides
            .primitive_overrides
            .insert("DFFP".to_string(), 0.5);
        overrides.global_scale = 0.8;

        let dff_fit = overrides.effective_fit(&PrimitiveType::DffP);
        assert_eq!(dff_fit, 0.5 * 0.8); // Override * scale

        let and_fit = overrides.effective_fit(&PrimitiveType::And { inputs: 2 });
        assert_eq!(and_fit, 0.1 * 0.8); // Base * scale
    }

    #[test]
    fn test_lir_stats() {
        let mut lir = Lir::new("test".to_string());

        // Add mixed primitives
        lir.add_primitive(Primitive::new_comb(
            PrimitiveId(0),
            PrimitiveType::And { inputs: 2 },
            "top.and1".to_string(),
            vec![],
            vec![],
        ));
        lir.add_primitive(Primitive::new_seq(
            PrimitiveId(1),
            PrimitiveType::DffP,
            "top.ff1".to_string(),
            vec![],
            vec![],
            NetId(0),
            None,
        ));
        lir.add_primitive(Primitive::new_comb(
            PrimitiveId(2),
            PrimitiveType::Mux2,
            "top.mux1".to_string(),
            vec![],
            vec![],
        ));

        lir.update_stats();

        assert_eq!(lir.stats.total_primitives, 3);
        assert_eq!(lir.stats.comb_gates, 1); // AND only
        assert_eq!(lir.stats.flip_flops, 1);
        assert_eq!(lir.stats.muxes, 1);
        assert!(lir.stats.total_fit > 0.0);
    }

    #[test]
    fn test_sequential_combinational_separation() {
        let mut lir = Lir::new("test".to_string());

        lir.add_primitive(Primitive::new_comb(
            PrimitiveId(0),
            PrimitiveType::And { inputs: 2 },
            "top.and1".to_string(),
            vec![],
            vec![],
        ));
        lir.add_primitive(Primitive::new_seq(
            PrimitiveId(1),
            PrimitiveType::DffP,
            "top.ff1".to_string(),
            vec![],
            vec![],
            NetId(0),
            None,
        ));

        assert_eq!(lir.sequential_primitives().len(), 1);
        assert_eq!(lir.combinational_primitives().len(), 1);
    }
}

// Old deprecated types have been removed.
// Use the new types: Lir, LirNet, Primitive, PrimitiveType
