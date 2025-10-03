//! LIR - Low-level Intermediate Representation
//!
//! Gate-level representation of hardware designs

use serde::{Deserialize, Serialize};

/// Low-level Intermediate Representation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Lir {
    /// Module name
    pub name: String,
    /// Gates in the design
    pub gates: Vec<Gate>,
    /// Nets connecting gates
    pub nets: Vec<Net>,
}

/// Complete LIR design with multiple modules
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LirDesign {
    /// Design name
    pub name: String,
    /// Modules in the design
    pub modules: Vec<LirModule>,
}

/// LIR module representation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LirModule {
    /// Module name
    pub name: String,
    /// Signals in the module
    pub signals: Vec<LirSignal>,
    /// Gates in the module
    pub gates: Vec<Gate>,
    /// Nets connecting gates
    pub nets: Vec<Net>,
}

/// LIR signal representation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LirSignal {
    /// Signal name
    pub name: String,
    /// Signal type
    pub signal_type: String,
    /// Whether this is an input signal
    pub is_input: bool,
    /// Whether this is an output signal
    pub is_output: bool,
    /// Whether this is a register
    pub is_register: bool,
}

/// Hardware gate representation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Gate {
    /// Gate identifier
    pub id: String,
    /// Gate type (AND, OR, NOT, etc.)
    pub gate_type: GateType,
    /// Input nets
    pub inputs: Vec<String>,
    /// Output nets
    pub outputs: Vec<String>,
}

/// Types of hardware gates
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum GateType {
    And,
    Or,
    Not,
    Nand,
    Nor,
    Xor,
    Xnor,
    Buffer,
    DFF,
    Latch,
}

impl std::fmt::Display for GateType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            GateType::And => write!(f, "AND"),
            GateType::Or => write!(f, "OR"),
            GateType::Not => write!(f, "NOT"),
            GateType::Nand => write!(f, "NAND"),
            GateType::Nor => write!(f, "NOR"),
            GateType::Xor => write!(f, "XOR"),
            GateType::Xnor => write!(f, "XNOR"),
            GateType::Buffer => write!(f, "BUF"),
            GateType::DFF => write!(f, "DFF"),
            GateType::Latch => write!(f, "LATCH"),
        }
    }
}

/// Hardware net (wire)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Net {
    /// Net identifier
    pub id: String,
    /// Width in bits
    pub width: usize,
    /// Driver gate
    pub driver: Option<String>,
    /// Load gates
    pub loads: Vec<String>,
    /// Whether this net connects to an output port
    pub is_output: bool,
    /// Whether this net connects to an input port
    pub is_input: bool,
}

impl Lir {
    /// Create a new empty LIR
    pub fn new(name: String) -> Self {
        Self {
            name,
            gates: Vec::new(),
            nets: Vec::new(),
        }
    }
}
