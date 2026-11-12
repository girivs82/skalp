//! What gate-level primitives compute, shared by the gate-level simulator
//! and by netlist transforms that need a cell's value (retiming computes the
//! initial value of a register it moves through a cell).

use crate::gate_netlist::Cell;
use crate::lir::PrimitiveType;
use crate::tech_library::CellFunction;

/// The primitive a gate-level cell simulates as: its truth table if it has
/// one, else the block or function its library gives it, else a guess from
/// its type name
pub fn primitive_for_cell(cell: &Cell) -> PrimitiveType {
    let ptype = if let Some(init) = cell.lut_init {
        // Determine LUT size from number of inputs
        let num_inputs = cell.inputs.len();
        if num_inputs <= 4 {
            PrimitiveType::Lut4 { init: init as u16 }
        } else {
            PrimitiveType::Lut6 { init }
        }
    } else if cell.function.as_ref() == Some(&CellFunction::Ram) {
        // RAM block: compute addr/data widths from pin counts
        // Outputs = rdata[0..data_width]
        let data_width = cell.outputs.len() as u8;
        // Inputs: raddr[aw] + RCLK + RCLKE + RE + wdata[dw] + waddr[aw] + WCLK + WCLKE + WE + [wmask[dw]]
        // With mask:    total = 2*aw + 2*dw + 6  →  aw = (total - 2*dw - 6) / 2
        // Without mask: total = 2*aw + dw + 6    →  aw = (total - dw - 6) / 2
        let total_inputs = cell.inputs.len();
        let dw = data_width as usize;
        // Try with mask first (most common for real BRAMs)
        let addr_width_with_mask = total_inputs.saturating_sub(2 * dw + 6) / 2;
        let addr_width_without_mask = total_inputs.saturating_sub(dw + 6) / 2;
        // Use the one that gives a power-of-2 depth and valid addr width
        let addr_width =
            if addr_width_with_mask > 0 && 2 * addr_width_with_mask + 2 * dw + 6 == total_inputs {
                addr_width_with_mask as u8
            } else {
                addr_width_without_mask as u8
            };
        PrimitiveType::RamBlock {
            addr_width,
            data_width,
        }
    } else if let Some(ports) = cell.dsp_ports() {
        PrimitiveType::Multiplier {
            a_width: ports.a.len() as u8,
            b_width: ports.b.len() as u8,
            p_width: ports.product_width as u8,
        }
    } else if cell.function.as_ref() == Some(&CellFunction::Ccu2) {
        // Lattice carry cell: the truth tables are cell parameters
        let (init0, init1) = crate::carry_chain::ccu2_inits(cell).unwrap_or_else(|| {
            panic!(
                "CCU2 cell '{}' has no valid INIT0/INIT1 parameters",
                cell.path
            )
        });
        PrimitiveType::Ccu2 { init0, init1 }
    } else if let Some(func) = &cell.function {
        // Use the library-provided function for accurate mapping
        cell_function_to_primitive(func)
    } else {
        // Fallback to cell type name parsing
        cell_type_to_primitive(&cell.cell_type)
    };
    // A flip-flop with a reset pin resets through it, whatever its
    // name says
    match ptype {
        PrimitiveType::DffP if cell.reset.is_some() && cell.clock.is_some() => PrimitiveType::DffSR,
        other => other,
    }
}

/// Map CellFunction (from library) to PrimitiveType
/// This is the preferred method as it uses the library's semantic function definition
pub fn cell_function_to_primitive(func: &CellFunction) -> PrimitiveType {
    match func {
        // Basic gates
        CellFunction::Inv => PrimitiveType::Inv,
        CellFunction::Nand2 => PrimitiveType::Nand { inputs: 2 },
        CellFunction::Nand3 => PrimitiveType::Nand { inputs: 3 },
        CellFunction::Nand4 => PrimitiveType::Nand { inputs: 4 },
        CellFunction::Nor2 => PrimitiveType::Nor { inputs: 2 },
        CellFunction::Nor3 => PrimitiveType::Nor { inputs: 3 },
        CellFunction::Nor4 => PrimitiveType::Nor { inputs: 4 },
        CellFunction::And2 => PrimitiveType::And { inputs: 2 },
        CellFunction::And3 => PrimitiveType::And { inputs: 3 },
        CellFunction::And4 => PrimitiveType::And { inputs: 4 },
        CellFunction::Or2 => PrimitiveType::Or { inputs: 2 },
        CellFunction::Or3 => PrimitiveType::Or { inputs: 3 },
        CellFunction::Or4 => PrimitiveType::Or { inputs: 4 },
        CellFunction::Xor2 => PrimitiveType::Xor,
        CellFunction::Xnor2 => PrimitiveType::Xnor,
        CellFunction::Buf => PrimitiveType::Buf,
        CellFunction::AndNot => PrimitiveType::AndNot,
        CellFunction::OrNot => PrimitiveType::OrNot,

        // Complex gates
        CellFunction::Aoi21 => PrimitiveType::Aoi21,
        CellFunction::Aoi22 => PrimitiveType::Aoi22,
        CellFunction::Oai21 => PrimitiveType::Oai21,
        CellFunction::Oai22 => PrimitiveType::Oai22,

        // Multiplexers
        CellFunction::Mux2 => PrimitiveType::Mux2,
        CellFunction::Mux4 => PrimitiveType::Mux4,

        // Arithmetic
        CellFunction::HalfAdder => PrimitiveType::HalfAdder,
        CellFunction::FullAdder => PrimitiveType::FullAdder,
        CellFunction::Adder(_) => PrimitiveType::FullAdder, // Treat as full adder chain
        CellFunction::Carry => PrimitiveType::CarryCell,

        // Floating-Point
        CellFunction::FpAdd32 => PrimitiveType::Fp32Add,
        CellFunction::FpSub32 => PrimitiveType::Fp32Sub,
        CellFunction::FpMul32 => PrimitiveType::Fp32Mul,
        CellFunction::FpDiv32 => PrimitiveType::Fp32Div,
        CellFunction::FpLt32 => PrimitiveType::Fp32Lt,
        CellFunction::FpGt32 => PrimitiveType::Fp32Gt,
        CellFunction::FpLe32 => PrimitiveType::Fp32Le,
        CellFunction::FpGe32 => PrimitiveType::Fp32Ge,

        // Sequential
        // TRIAGE #33: tie cells MUST map to constants. The `_ => Buf`
        // fallthrough turned them into zero-input buffers that evaluate
        // to false — TIE_HIGH nets read 0, so any register fed (directly
        // or through logic) by const_1 was stuck at the wrong value on
        // the EC gate side. (const_0 was accidentally correct.)
        CellFunction::TieHigh => PrimitiveType::Constant { value: true },
        CellFunction::TieLow => PrimitiveType::Constant { value: false },
        CellFunction::Dff => PrimitiveType::DffP,
        CellFunction::DffR => PrimitiveType::DffSR, // DFF with sync reset to 0
        CellFunction::DffE => PrimitiveType::DffE,  // DFF with enable
        CellFunction::DffRE => PrimitiveType::DffE, // DFF with reset and enable
        CellFunction::Latch => PrimitiveType::Dlatch,

        // Tristate
        CellFunction::Tristate => PrimitiveType::Tribuf {
            enable_active_high: true,
        },

        // Power infrastructure - treat as buffers for simulation
        CellFunction::LevelShifterLH
        | CellFunction::LevelShifterHL
        | CellFunction::IsolationAnd
        | CellFunction::IsolationOr
        | CellFunction::IsolationLatch => PrimitiveType::Buf,
        CellFunction::RetentionDff => PrimitiveType::DffP,

        // NCL gates
        CellFunction::Th12 => PrimitiveType::Th12,
        CellFunction::Th22 => PrimitiveType::Th22,
        CellFunction::Th13 => PrimitiveType::Th13,
        CellFunction::Th23 => PrimitiveType::Th23,
        CellFunction::Th33 => PrimitiveType::Th33,
        CellFunction::Th14 => PrimitiveType::Th14,
        CellFunction::Th24 => PrimitiveType::Th24,
        CellFunction::Th34 => PrimitiveType::Th34,
        CellFunction::Th44 => PrimitiveType::Th44,
        // Generic THmn gate - map based on m and n values
        CellFunction::Thmn { m, n } => {
            // Map to the closest standard TH gate
            match (m, n) {
                (1, 2) => PrimitiveType::Th12,
                (2, 2) => PrimitiveType::Th22,
                (1, 3) => PrimitiveType::Th13,
                (2, 3) => PrimitiveType::Th23,
                (3, 3) => PrimitiveType::Th33,
                (1, 4) => PrimitiveType::Th14,
                (2, 4) => PrimitiveType::Th24,
                (3, 4) => PrimitiveType::Th34,
                (4, 4) => PrimitiveType::Th44,
                _ => PrimitiveType::Th22, // Default to Th22 for unknown
            }
        }

        // Default: treat unknown functions as buffers
        _ => PrimitiveType::Buf,
    }
}

/// Map library cell type name to PrimitiveType
pub fn cell_type_to_primitive(cell_type: &str) -> PrimitiveType {
    let upper = cell_type.to_uppercase();

    // Check for TIE cells first (before stripping suffix)
    if upper.starts_with("TIE_HIGH") || upper.starts_with("TIEH") {
        return PrimitiveType::Constant { value: true };
    }
    if upper.starts_with("TIE_LOW") || upper.starts_with("TIEL") {
        return PrimitiveType::Constant { value: false };
    }

    // Check for FP32 cells before stripping suffix (FP32_ADD, FP32_MUL, etc.)
    // These have underscores that are part of the cell name, not drive strength suffixes
    // Tech library generates fp_add32 (uppercase: FP_ADD32), also match FP32_ADD variants
    if upper.starts_with("FP32_ADD")
        || upper.starts_with("FPADD32")
        || upper.starts_with("FP_ADD32")
    {
        return PrimitiveType::Fp32Add;
    }
    if upper.starts_with("FP32_SUB")
        || upper.starts_with("FPSUB32")
        || upper.starts_with("FP_SUB32")
    {
        return PrimitiveType::Fp32Sub;
    }
    if upper.starts_with("FP32_MUL")
        || upper.starts_with("FPMUL32")
        || upper.starts_with("FP_MUL32")
    {
        return PrimitiveType::Fp32Mul;
    }
    if upper.starts_with("FP32_DIV")
        || upper.starts_with("FPDIV32")
        || upper.starts_with("FP_DIV32")
    {
        return PrimitiveType::Fp32Div;
    }

    // FP32 comparison cells (BUG #191 FIX)
    if upper.starts_with("FP32_LT") || upper.starts_with("FPLT32") || upper.starts_with("FP_LT32") {
        return PrimitiveType::Fp32Lt;
    }
    if upper.starts_with("FP32_GT") || upper.starts_with("FPGT32") || upper.starts_with("FP_GT32") {
        return PrimitiveType::Fp32Gt;
    }
    if upper.starts_with("FP32_LE") || upper.starts_with("FPLE32") || upper.starts_with("FP_LE32") {
        return PrimitiveType::Fp32Le;
    }
    if upper.starts_with("FP32_GE") || upper.starts_with("FPGE32") || upper.starts_with("FP_GE32") {
        return PrimitiveType::Fp32Ge;
    }

    // Handle iCE40 FPGA cell names (SB_LUT4_*, SB_DFF*, SB_CARRY, etc.)
    if upper.starts_with("SB_LUT4_AND2") {
        return PrimitiveType::And { inputs: 2 };
    }
    if upper.starts_with("SB_LUT4_AND3") {
        return PrimitiveType::And { inputs: 3 };
    }
    if upper.starts_with("SB_LUT4_AND4") {
        return PrimitiveType::And { inputs: 4 };
    }
    if upper.starts_with("SB_LUT4_OR2") {
        return PrimitiveType::Or { inputs: 2 };
    }
    if upper.starts_with("SB_LUT4_OR3") {
        return PrimitiveType::Or { inputs: 3 };
    }
    if upper.starts_with("SB_LUT4_OR4") {
        return PrimitiveType::Or { inputs: 4 };
    }
    if upper.starts_with("SB_LUT4_XOR2") {
        return PrimitiveType::Xor;
    }
    if upper.starts_with("SB_LUT4_XNOR2") {
        return PrimitiveType::Xnor;
    }
    if upper.starts_with("SB_LUT4_NAND2") {
        return PrimitiveType::Nand { inputs: 2 };
    }
    if upper.starts_with("SB_LUT4_NOR2") {
        return PrimitiveType::Nor { inputs: 2 };
    }
    if upper.starts_with("SB_LUT4_MUX2") {
        return PrimitiveType::Mux2;
    }
    if upper.starts_with("SB_LUT4_INV") {
        return PrimitiveType::Inv;
    }
    if upper.starts_with("SB_LUT4_BUF") {
        return PrimitiveType::Buf;
    }
    // Generic LUT4 - treat as buffer (will be handled by LUT evaluation)
    if upper.starts_with("SB_LUT4") {
        return PrimitiveType::Buf;
    }
    // iCE40 flip-flops
    if upper.starts_with("SB_DFFER") || upper.starts_with("SB_DFFESR") {
        return PrimitiveType::DffE; // DFF with enable and reset
    }
    if upper.starts_with("SB_DFFE") {
        return PrimitiveType::DffE; // DFF with enable
    }
    if upper.starts_with("SB_DFFSR") || upper.starts_with("SB_DFFR") {
        return PrimitiveType::DffP; // DFF with reset
    }
    if upper.starts_with("SB_DFF") {
        return PrimitiveType::DffP; // Basic DFF
    }
    // iCE40 carry cell (used in adders)
    // CO = (I0 & I1) | ((I0 | I1) & CI)
    if upper.starts_with("SB_CARRY") {
        return PrimitiveType::CarryCell;
    }
    // iCE40 tie cells
    if upper.starts_with("SB_VCC") {
        return PrimitiveType::Constant { value: true };
    }
    if upper.starts_with("SB_GND") {
        return PrimitiveType::Constant { value: false };
    }
    // iCE40 global buffer
    if upper.starts_with("SB_GB") {
        return PrimitiveType::Buf;
    }
    // iCE40 I/O cell
    if upper.starts_with("SB_IO") {
        return PrimitiveType::Buf;
    }
    // ECP5 I/O cells
    if upper == "BB" || upper == "IB" || upper == "OB" || upper == "OBZ" {
        return PrimitiveType::Buf;
    }

    // Normalize cell type (remove drive strength suffix like "_X1", "_X2")
    let base_type = cell_type
        .split('_')
        .next()
        .unwrap_or(cell_type)
        .to_uppercase();

    match base_type.as_str() {
        // Basic gates
        "INV" | "NOT" => PrimitiveType::Inv,
        "BUF" => PrimitiveType::Buf,
        "AND" | "AND2" => PrimitiveType::And { inputs: 2 },
        "AND3" => PrimitiveType::And { inputs: 3 },
        "AND4" => PrimitiveType::And { inputs: 4 },
        "OR" | "OR2" => PrimitiveType::Or { inputs: 2 },
        "OR3" => PrimitiveType::Or { inputs: 3 },
        "OR4" => PrimitiveType::Or { inputs: 4 },
        "XOR" | "XOR2" => PrimitiveType::Xor,
        "XNOR" | "XNOR2" => PrimitiveType::Xnor,
        "NAND" | "NAND2" => PrimitiveType::Nand { inputs: 2 },
        "NAND3" => PrimitiveType::Nand { inputs: 3 },
        "NAND4" => PrimitiveType::Nand { inputs: 4 },
        "NOR" | "NOR2" => PrimitiveType::Nor { inputs: 2 },
        "NOR3" => PrimitiveType::Nor { inputs: 3 },
        "NOR4" => PrimitiveType::Nor { inputs: 4 },

        // Multiplexers
        "MUX" | "MUX2" => PrimitiveType::Mux2,
        "MUX4" => PrimitiveType::Mux4,

        // Complex gates
        "AOI21" => PrimitiveType::Aoi21,
        "AOI22" => PrimitiveType::Aoi22,
        "OAI21" => PrimitiveType::Oai21,
        "OAI22" => PrimitiveType::Oai22,

        // Arithmetic
        "HA" | "HALFADDER" => PrimitiveType::HalfAdder,
        "FA" | "FULLADDER" => PrimitiveType::FullAdder,

        // Floating-Point (soft macros)
        // Accept both underscore (FP32_MUL) and no-underscore (FP32MUL) versions
        "FP32" => PrimitiveType::Fp32Add, // Default to add for generic FP32
        "FP32ADD" | "FP32_ADD" | "FPADD32" => PrimitiveType::Fp32Add,
        "FP32SUB" | "FP32_SUB" | "FPSUB32" => PrimitiveType::Fp32Sub,
        "FP32MUL" | "FP32_MUL" | "FPMUL32" => PrimitiveType::Fp32Mul,
        "FP32DIV" | "FP32_DIV" | "FPDIV32" => PrimitiveType::Fp32Div,
        // FP32 comparisons (BUG #191 FIX)
        "FP32LT" | "FP32_LT" | "FPLT32" => PrimitiveType::Fp32Lt,
        "FP32GT" | "FP32_GT" | "FPGT32" => PrimitiveType::Fp32Gt,
        "FP32LE" | "FP32_LE" | "FPLE32" => PrimitiveType::Fp32Le,
        "FP32GE" | "FP32_GE" | "FPGE32" => PrimitiveType::Fp32Ge,

        // Sequential - D Flip-Flops
        "DFF" | "DFFR" | "DFFRQ" => PrimitiveType::DffP, // Rising edge, with reset
        "DFFN" | "DFFRN" => PrimitiveType::DffN,         // Active-low reset
        "DFFNEG" => PrimitiveType::DffNeg,               // Falling edge
        "DFFE" | "DFFER" => PrimitiveType::DffE,         // With enable (and optional reset)
        "DFFAR" => PrimitiveType::DffAR,                 // Async reset
        "DFFAS" => PrimitiveType::DffAS,                 // Async set
        "DFFSCAN" => PrimitiveType::DffScan,             // Scan chain

        // Latches
        "LATCH" | "LAT" | "DLATCH" => PrimitiveType::Dlatch,
        "SRLATCH" | "SR" => PrimitiveType::SRlatch,

        // Tri-state
        "TRIBUF" | "TRI" => PrimitiveType::Tribuf {
            enable_active_high: true,
        },

        // Tie cells (constant generators)
        // TIE1 = logic high, TIE0 = logic low (common naming convention)
        "TIE" | "TIE1" | "TIEH" | "TIEHI" | "TIEHIGH" | "VCC" | "ONE" => {
            PrimitiveType::Constant { value: true }
        }
        "TIE0" | "TIEL" | "TIELO" | "TIELOW" | "GND" | "ZERO" => {
            PrimitiveType::Constant { value: false }
        }

        // Default to buffer for unknown types
        _ => {
            PrimitiveType::Buf // Use buffer as fallback for unknown types
        }
    }
}

/// Evaluate a gate primitive without fault injection
///
/// # Arguments
///
/// * `ptype` - The primitive type to evaluate
/// * `inputs` - Input values as booleans (1-bit signals)
///
/// # Returns
///
/// Vector of output boolean values (typically 1, but 2 for adders/comparators)
pub fn evaluate_primitive(ptype: &PrimitiveType, inputs: &[bool]) -> Vec<bool> {
    match ptype {
        // === Combinational Logic ===
        PrimitiveType::And { inputs: n } => {
            let n = *n as usize;
            let result = inputs.iter().take(n).all(|&x| x);
            vec![result]
        }

        PrimitiveType::Or { inputs: n } => {
            let n = *n as usize;
            let result = inputs.iter().take(n).any(|&x| x);
            vec![result]
        }

        PrimitiveType::Nand { inputs: n } => {
            let n = *n as usize;
            let result = !inputs.iter().take(n).all(|&x| x);
            vec![result]
        }

        PrimitiveType::Nor { inputs: n } => {
            let n = *n as usize;
            let result = !inputs.iter().take(n).any(|&x| x);
            vec![result]
        }

        PrimitiveType::Xor => {
            let a = inputs.first().copied().unwrap_or(false);
            let b = inputs.get(1).copied().unwrap_or(false);
            vec![a ^ b]
        }

        PrimitiveType::Xnor => {
            let a = inputs.first().copied().unwrap_or(false);
            let b = inputs.get(1).copied().unwrap_or(false);
            vec![!(a ^ b)]
        }

        PrimitiveType::Inv => {
            let a = inputs.first().copied().unwrap_or(false);
            vec![!a]
        }

        PrimitiveType::AndNot => {
            // a & ~b
            let a = inputs.first().copied().unwrap_or(false);
            let b = inputs.get(1).copied().unwrap_or(false);
            vec![a & !b]
        }

        PrimitiveType::OrNot => {
            // a | ~b
            let a = inputs.first().copied().unwrap_or(false);
            let b = inputs.get(1).copied().unwrap_or(false);
            vec![a | !b]
        }

        PrimitiveType::Aoi21 => {
            // ~((a & b) | c)
            let a = inputs.first().copied().unwrap_or(false);
            let b = inputs.get(1).copied().unwrap_or(false);
            let c = inputs.get(2).copied().unwrap_or(false);
            vec![!((a & b) | c)]
        }

        PrimitiveType::Aoi22 => {
            // ~((a & b) | (c & d))
            let a = inputs.first().copied().unwrap_or(false);
            let b = inputs.get(1).copied().unwrap_or(false);
            let c = inputs.get(2).copied().unwrap_or(false);
            let d = inputs.get(3).copied().unwrap_or(false);
            vec![!((a & b) | (c & d))]
        }

        PrimitiveType::Oai21 => {
            // ~((a | b) & c)
            let a = inputs.first().copied().unwrap_or(false);
            let b = inputs.get(1).copied().unwrap_or(false);
            let c = inputs.get(2).copied().unwrap_or(false);
            vec![!((a | b) & c)]
        }

        PrimitiveType::Oai22 => {
            // ~((a | b) & (c | d))
            let a = inputs.first().copied().unwrap_or(false);
            let b = inputs.get(1).copied().unwrap_or(false);
            let c = inputs.get(2).copied().unwrap_or(false);
            let d = inputs.get(3).copied().unwrap_or(false);
            vec![!((a | b) & (c | d))]
        }

        PrimitiveType::Buf => {
            let a = inputs.first().copied().unwrap_or(false);
            vec![a]
        }

        PrimitiveType::ClkBuf => {
            // Clock buffer - same as regular buffer
            let a = inputs.first().copied().unwrap_or(false);
            vec![a]
        }

        PrimitiveType::Tribuf { enable_active_high } => {
            // inputs: [data, enable]
            let data = inputs.first().copied().unwrap_or(false);
            let enable = inputs.get(1).copied().unwrap_or(false);
            let is_enabled = if *enable_active_high { enable } else { !enable };
            // For simulation, output data when enabled, false otherwise (no Z state)
            vec![if is_enabled { data } else { false }]
        }

        PrimitiveType::Mux2 => {
            // inputs: [sel, d0, d1] - output: sel ? d1 : d0
            let sel = inputs.first().copied().unwrap_or(false);
            let d0 = inputs.get(1).copied().unwrap_or(false);
            let d1 = inputs.get(2).copied().unwrap_or(false);
            vec![if sel { d1 } else { d0 }]
        }

        PrimitiveType::Mux4 => {
            // inputs: [sel0, sel1, d0, d1, d2, d3]
            let sel0 = inputs.first().copied().unwrap_or(false);
            let sel1 = inputs.get(1).copied().unwrap_or(false);
            let sel = (sel1 as usize) * 2 + (sel0 as usize);
            let d = inputs.get(2 + sel).copied().unwrap_or(false);
            vec![d]
        }

        PrimitiveType::MuxN { select_bits } => {
            let n_sel = *select_bits as usize;
            let mut sel = 0usize;
            for i in 0..n_sel {
                if inputs.get(i).copied().unwrap_or(false) {
                    sel |= 1 << i;
                }
            }
            let d = inputs.get(n_sel + sel).copied().unwrap_or(false);
            vec![d]
        }

        PrimitiveType::Constant { value } => {
            vec![*value]
        }

        // === Arithmetic ===
        PrimitiveType::HalfAdder => {
            // inputs: [a, b], outputs: [sum, carry]
            let a = inputs.first().copied().unwrap_or(false);
            let b = inputs.get(1).copied().unwrap_or(false);
            let sum = a ^ b;
            let carry = a & b;
            vec![sum, carry]
        }

        PrimitiveType::FullAdder => {
            // inputs: [a, b, cin], outputs: [sum, cout]
            let a = inputs.first().copied().unwrap_or(false);
            let b = inputs.get(1).copied().unwrap_or(false);
            let cin = inputs.get(2).copied().unwrap_or(false);
            let sum = a ^ b ^ cin;
            let cout = (a & b) | (cin & (a ^ b));
            vec![sum, cout]
        }

        PrimitiveType::CarryCell => {
            // FPGA carry cell: inputs [i0, i1, ci], output [co]
            // CO = (I0 & I1) | ((I0 | I1) & CI)
            let i0 = inputs.first().copied().unwrap_or(false);
            let i1 = inputs.get(1).copied().unwrap_or(false);
            let ci = inputs.get(2).copied().unwrap_or(false);
            let co = (i0 & i1) | ((i0 | i1) & ci);
            vec![co]
        }

        PrimitiveType::Ccu2 { init0, init1 } => {
            // inputs: [A0, B0, C0, D0, A1, B1, C1, D1, CIN], outputs: [S0, S1, COUT]
            crate::carry_chain::ccu2_eval(*init0, *init1, inputs).to_vec()
        }

        PrimitiveType::CompBit => {
            // inputs: [a, b, lt_in, eq_in], outputs: [lt_out, eq_out]
            let a = inputs.first().copied().unwrap_or(false);
            let b = inputs.get(1).copied().unwrap_or(false);
            let lt_in = inputs.get(2).copied().unwrap_or(false);
            let eq_in = inputs.get(3).copied().unwrap_or(true); // eq starts true

            // If previous bits weren't equal, propagate that decision
            if !eq_in {
                return vec![lt_in, false];
            }

            // This is the first differing bit (or still equal)
            let eq_out = a == b;
            let lt_out = if eq_out { lt_in } else { !a && b };
            vec![lt_out, eq_out]
        }

        // === Sequential Logic ===
        // For sequential elements, we return the current state (Q output)
        // The state update is handled separately by the simulation engine
        PrimitiveType::DffP
        | PrimitiveType::DffN
        | PrimitiveType::DffNeg
        | PrimitiveType::DffAR
        | PrimitiveType::DffAS
        | PrimitiveType::DffSR => {
            // DFF inputs: [d] - data input only
            // Clock is associated with the sequential block, not the primitive inputs
            // On clock edge, sample D input and store to Q output
            // Sequential behavior is handled by SequentialBlock processing
            let d = inputs.first().copied().unwrap_or(false);
            vec![d]
        }

        PrimitiveType::DffE => {
            // inputs: [d, en] - data and enable
            // Clock is associated with the sequential block
            let d = inputs.first().copied().unwrap_or(false);
            let en = inputs.get(1).copied().unwrap_or(true); // default enable to true
                                                             // Note: enable logic is often handled at the block level, but if enabled=false,
                                                             // the DFF should retain its current value (not modeled here as we don't have state)
            if en {
                vec![d]
            } else {
                vec![false]
            } // Simplified: returns d if enabled
        }

        PrimitiveType::DffScan => {
            // inputs: [clk, d, scan_in, scan_en, rst]
            let d = inputs.get(1).copied().unwrap_or(false);
            let scan_in = inputs.get(2).copied().unwrap_or(false);
            let scan_en = inputs.get(3).copied().unwrap_or(false);
            vec![if scan_en { scan_in } else { d }]
        }

        PrimitiveType::Dlatch => {
            // inputs: [en, d]
            let d = inputs.get(1).copied().unwrap_or(false);
            vec![d]
        }

        PrimitiveType::SRlatch => {
            // inputs: [s, r]
            // For simulation, priority: reset > set
            let s = inputs.first().copied().unwrap_or(false);
            let r = inputs.get(1).copied().unwrap_or(false);
            if r {
                vec![false]
            } else if s {
                vec![true]
            } else {
                vec![false] // Hold state - needs external state tracking
            }
        }

        // === Memory ===
        PrimitiveType::MemCell | PrimitiveType::RegCell => {
            // inputs: [data_in, write_en, clk]
            // For combinational read, return data_in (actual behavior needs state)
            let data_in = inputs.first().copied().unwrap_or(false);
            vec![data_in]
        }

        PrimitiveType::Multiplier {
            a_width,
            b_width,
            p_width,
        } => {
            let (a_width, b_width) = (*a_width as usize, *b_width as usize);
            // A, B, the C bus, then the sign controls.
            let sign_controls = a_width + b_width + crate::gate_netlist::DSP_C_BITS;
            let signed = |index: usize| inputs.get(index).copied().unwrap_or(false);
            let operand = |start: usize, width: usize, is_signed: bool| -> i128 {
                let mut value = (0..width).fold(0i128, |value, bit| {
                    value | (i128::from(inputs.get(start + bit).copied().unwrap_or(false)) << bit)
                });
                if is_signed && (value >> (width - 1)) & 1 == 1 {
                    value -= 1i128 << width;
                }
                value
            };
            let product = operand(0, a_width, signed(sign_controls))
                * operand(a_width, b_width, signed(sign_controls + 1));
            (0..*p_width as usize)
                .map(|bit| (product >> bit) & 1 == 1)
                .collect()
        }

        PrimitiveType::RamBlock { data_width, .. } => {
            // RAM evaluation is handled specially in gate_simulator.rs (needs persistent state).
            // Return all-zero as fallback (actual read is done by simulator's RAM state).
            vec![false; *data_width as usize]
        }

        // Distributed RAM ports read and write the simulator's memory state
        // (gate_simulator.rs); stateless evaluation has no memory to read.
        PrimitiveType::DistRamWrite { .. } => vec![],
        PrimitiveType::DistRamRead { data_width, .. } => vec![false; *data_width as usize],

        // === Power Infrastructure ===
        PrimitiveType::LevelShifter { .. } => {
            // Level shifter passes signal through with voltage translation
            // Logic behavior is identical to buffer
            let a = inputs.first().copied().unwrap_or(false);
            vec![a]
        }

        PrimitiveType::IsolationCell {
            clamp_value,
            enable_active_high,
        } => {
            // inputs: [data_in, iso_en]
            let data = inputs.first().copied().unwrap_or(false);
            let iso_en = inputs.get(1).copied().unwrap_or(false);
            let is_isolated = if *enable_active_high { iso_en } else { !iso_en };

            if is_isolated {
                // Clamp to specified value (0, 1, or 2=hold)
                match clamp_value {
                    0 => vec![false],
                    1 => vec![true],
                    _ => vec![data], // Hold last value (simplified - pass through)
                }
            } else {
                vec![data]
            }
        }

        PrimitiveType::RetentionDff { .. } => {
            // inputs: [clk, d, save, restore] or [clk, d, rst, save, restore]
            // Same as DFF for logic simulation - retention behavior is state management
            let d = inputs.get(1).copied().unwrap_or(false);
            vec![d]
        }

        PrimitiveType::PowerSwitch { .. } => {
            // Power switches control power rails, no logic output
            vec![]
        }

        PrimitiveType::AlwaysOnBuf => {
            // Always-on buffer - same as regular buffer
            let a = inputs.first().copied().unwrap_or(false);
            vec![a]
        }

        // === Floating-Point Soft Macros ===
        // These are 32-bit operations that take 64 inputs (a[0..31], b[0..31])
        // and produce 32 outputs (result[0..31])
        PrimitiveType::Fp32Add => evaluate_fp32_operation(inputs, |a, b| a + b),

        PrimitiveType::Fp32Sub => evaluate_fp32_operation(inputs, |a, b| a - b),

        PrimitiveType::Fp32Mul => evaluate_fp32_operation(inputs, |a, b| a * b),

        PrimitiveType::Fp32Div => {
            evaluate_fp32_operation(inputs, |a, b| if b != 0.0 { a / b } else { f32::NAN })
        }

        // FP32 comparisons return 1 bit (true/false)
        PrimitiveType::Fp32Lt => evaluate_fp32_comparison(inputs, |a, b| a < b),

        PrimitiveType::Fp32Gt => evaluate_fp32_comparison(inputs, |a, b| a > b),

        PrimitiveType::Fp32Le => evaluate_fp32_comparison(inputs, |a, b| a <= b),

        PrimitiveType::Fp32Ge => evaluate_fp32_comparison(inputs, |a, b| a >= b),

        // === NCL Threshold Gates ===
        // Note: These are simplified stateless evaluations.
        // Full NCL simulation with hysteresis (state-holding) is implemented in ncl_sim.rs (Phase 6).
        // THmn: Output HIGH when ≥m of n inputs are HIGH, LOW when all inputs are LOW
        PrimitiveType::Th12 => {
            // 1-of-2: OR with hysteresis (set when ≥1 input high, reset when all low)
            // Stateless version: just OR
            let count = inputs.iter().take(2).filter(|&&x| x).count();
            vec![count >= 1]
        }

        PrimitiveType::Th22 => {
            // 2-of-2: C-element (set when all inputs high, reset when all low)
            // Stateless version: AND
            let count = inputs.iter().take(2).filter(|&&x| x).count();
            vec![count >= 2]
        }

        PrimitiveType::Th13 => {
            let count = inputs.iter().take(3).filter(|&&x| x).count();
            vec![count >= 1]
        }

        PrimitiveType::Th23 => {
            let count = inputs.iter().take(3).filter(|&&x| x).count();
            vec![count >= 2]
        }

        PrimitiveType::Th33 => {
            let count = inputs.iter().take(3).filter(|&&x| x).count();
            vec![count >= 3]
        }

        PrimitiveType::Th14 => {
            let count = inputs.iter().take(4).filter(|&&x| x).count();
            vec![count >= 1]
        }

        PrimitiveType::Th24 => {
            let count = inputs.iter().take(4).filter(|&&x| x).count();
            vec![count >= 2]
        }

        PrimitiveType::Th34 => {
            let count = inputs.iter().take(4).filter(|&&x| x).count();
            vec![count >= 3]
        }

        PrimitiveType::Th44 => {
            let count = inputs.iter().take(4).filter(|&&x| x).count();
            vec![count >= 4]
        }

        PrimitiveType::Thmn { m, n } => {
            let count = inputs.iter().take(*n as usize).filter(|&&x| x).count();
            vec![count >= *m as usize]
        }

        PrimitiveType::ThmnW { m, n, weights } => {
            // Weighted threshold: sum(input[i] * weight[i]) >= m
            let weighted_sum: u32 = inputs
                .iter()
                .take(*n as usize)
                .zip(weights.iter())
                .map(|(&input, &weight)| if input { weight as u32 } else { 0 })
                .sum();
            vec![weighted_sum >= *m as u32]
        }

        PrimitiveType::NclCompletion { width } => {
            // Completion detection: all dual-rail signals must be in same phase
            // Input is width*2 bits (t0,f0, t1,f1, ...)
            // Complete when: all DATA (one rail high each) or all NULL (both rails low each)
            let w = *width as usize;
            let mut all_data = true;
            let mut all_null = true;

            for i in 0..w {
                let t = inputs.get(i * 2).copied().unwrap_or(false);
                let f = inputs.get(i * 2 + 1).copied().unwrap_or(false);

                // DATA: exactly one rail is high (t xor f)
                // NULL: both rails are low
                let is_data = t ^ f;
                let is_null = !t && !f;

                all_data = all_data && is_data;
                all_null = all_null && is_null;
            }

            vec![all_data || all_null]
        }

        // === FPGA Look-Up Tables ===
        PrimitiveType::Lut4 { init } => {
            // 4-input LUT: output = (init >> addr) & 1
            // Address is formed from inputs: addr = i3*8 + i2*4 + i1*2 + i0
            // iCE40 SB_LUT4 input ordering: I0=LSB, I3=MSB
            let i0 = inputs.first().copied().unwrap_or(false) as u16;
            let i1 = inputs.get(1).copied().unwrap_or(false) as u16;
            let i2 = inputs.get(2).copied().unwrap_or(false) as u16;
            let i3 = inputs.get(3).copied().unwrap_or(false) as u16;
            let addr = i0 | (i1 << 1) | (i2 << 2) | (i3 << 3);
            let result = (init >> addr) & 1 == 1;
            vec![result]
        }

        PrimitiveType::Lut6 { init } => {
            // 6-input LUT: output = (init >> addr) & 1
            // Address is formed from inputs: addr = i5*32 + i4*16 + i3*8 + i2*4 + i1*2 + i0
            let i0 = inputs.first().copied().unwrap_or(false) as u64;
            let i1 = inputs.get(1).copied().unwrap_or(false) as u64;
            let i2 = inputs.get(2).copied().unwrap_or(false) as u64;
            let i3 = inputs.get(3).copied().unwrap_or(false) as u64;
            let i4 = inputs.get(4).copied().unwrap_or(false) as u64;
            let i5 = inputs.get(5).copied().unwrap_or(false) as u64;
            let addr = i0 | (i1 << 1) | (i2 << 2) | (i3 << 3) | (i4 << 4) | (i5 << 5);
            let result = (init >> addr) & 1 == 1;
            vec![result]
        }
    }
}

/// Helper function to evaluate FP32 operations
/// Converts 64 boolean inputs (a[0..31], b[0..31]) to two f32 values,
/// applies the operation, and converts the result back to 32 boolean outputs
fn evaluate_fp32_operation<F>(inputs: &[bool], op: F) -> Vec<bool>
where
    F: Fn(f32, f32) -> f32,
{
    // Extract a[0..31] from inputs[0..31]
    let mut a_bits: u32 = 0;
    for i in 0..32 {
        if inputs.get(i).copied().unwrap_or(false) {
            a_bits |= 1 << i;
        }
    }

    // Extract b[0..31] from inputs[32..63]
    let mut b_bits: u32 = 0;
    for i in 0..32 {
        if inputs.get(32 + i).copied().unwrap_or(false) {
            b_bits |= 1 << i;
        }
    }

    // Convert to f32 and perform operation
    let a = f32::from_bits(a_bits);
    let b = f32::from_bits(b_bits);
    let result = op(a, b);

    // Convert result back to bits
    let result_bits = result.to_bits();

    // Return as 32 booleans
    (0..32).map(|i| (result_bits >> i) & 1 == 1).collect()
}

/// Helper function to evaluate FP32 comparison operations
/// Converts 64 boolean inputs (a[0..31], b[0..31]) to two f32 values,
/// applies the comparison, and returns a single boolean output
fn evaluate_fp32_comparison<F>(inputs: &[bool], op: F) -> Vec<bool>
where
    F: Fn(f32, f32) -> bool,
{
    // Extract a[0..31] from inputs[0..31]
    let mut a_bits: u32 = 0;
    for i in 0..32 {
        if inputs.get(i).copied().unwrap_or(false) {
            a_bits |= 1 << i;
        }
    }

    // Extract b[0..31] from inputs[32..63]
    let mut b_bits: u32 = 0;
    for i in 0..32 {
        if inputs.get(32 + i).copied().unwrap_or(false) {
            b_bits |= 1 << i;
        }
    }

    // Convert to f32 and perform comparison
    let a = f32::from_bits(a_bits);
    let b = f32::from_bits(b_bits);
    let result = op(a, b);

    // Return as single boolean
    vec![result]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_cell_type_mapping() {
        assert!(matches!(
            cell_type_to_primitive("NAND2_X1"),
            PrimitiveType::Nand { inputs: 2 }
        ));
        assert!(matches!(
            cell_type_to_primitive("AND3_X2"),
            PrimitiveType::And { inputs: 3 }
        ));
        assert!(matches!(cell_type_to_primitive("XOR2"), PrimitiveType::Xor));
        assert!(matches!(
            cell_type_to_primitive("DFF_X1"),
            PrimitiveType::DffP
        ));
        assert!(matches!(
            cell_type_to_primitive("FA_X1"),
            PrimitiveType::FullAdder
        ));
        assert!(matches!(
            cell_type_to_primitive("MUX2"),
            PrimitiveType::Mux2
        ));
    }
}
