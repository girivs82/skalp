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