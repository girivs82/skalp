//! Hardware primitives for SKALP LIR

use serde::{Deserialize, Serialize};

/// Hardware primitive types
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Primitive {
    /// Basic logic gates
    Logic(LogicPrimitive),
    /// Storage elements
    Storage(StoragePrimitive),
    /// Arithmetic units
    Arithmetic(ArithmeticPrimitive),
    /// Memory primitives
    Memory(MemoryPrimitive),
}

/// Basic logic gate primitives
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum LogicPrimitive {
    And { inputs: usize },
    Or { inputs: usize },
    Not,
    Nand { inputs: usize },
    Nor { inputs: usize },
    Xor { inputs: usize },
    Xnor { inputs: usize },
    Buffer,
    TriState,
}

/// Storage element primitives
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum StoragePrimitive {
    DFlipFlop {
        has_reset: bool,
        reset_active_high: bool,
        has_enable: bool,
    },
    Latch {
        is_transparent: bool,
        has_reset: bool,
    },
    Register {
        width: usize,
        has_reset: bool,
        reset_value: u64,
    },
}

/// Arithmetic primitive units
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ArithmeticPrimitive {
    Adder { width: usize },
    Subtractor { width: usize },
    Multiplier { width: usize },
    Divider { width: usize },
    Comparator { width: usize, operation: CompareOp },
    Shifter { width: usize, direction: ShiftDirection },
}

/// Comparison operations
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum CompareOp {
    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
}

/// Shift directions
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ShiftDirection {
    Left,
    Right,
    ArithmeticRight,
}

/// Memory primitives
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum MemoryPrimitive {
    SRAM {
        depth: usize,
        width: usize,
        ports: usize,
    },
    ROM {
        depth: usize,
        width: usize,
        data: Vec<u64>,
    },
    FIFO {
        depth: usize,
        width: usize,
    },
}

/// Primitive characteristics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PrimitiveInfo {
    /// Primitive type
    pub primitive: Primitive,
    /// Delay characteristics
    pub delay: DelayInfo,
    /// Power characteristics
    pub power: PowerInfo,
    /// Area characteristics
    pub area: AreaInfo,
}

/// Timing delay information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DelayInfo {
    /// Propagation delay (picoseconds)
    pub propagation_delay: u32,
    /// Setup time (picoseconds)
    pub setup_time: u32,
    /// Hold time (picoseconds)
    pub hold_time: u32,
}

/// Power consumption information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PowerInfo {
    /// Static power (nanowatts)
    pub static_power: f64,
    /// Dynamic power per toggle (picojoules)
    pub dynamic_power: f64,
}

/// Area information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AreaInfo {
    /// Gate equivalent count
    pub gate_count: u32,
    /// Physical area (square microns)
    pub area_um2: f64,
}