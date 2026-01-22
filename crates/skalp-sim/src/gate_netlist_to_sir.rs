//! GateNetlist to SIR Conversion
//!
//! Converts a technology-mapped `GateNetlist` (from TechMapper) into `SIR`
//! (Simulation Intermediate Representation) for gate-level simulation with
//! fault injection support.
//!
//! # Flow
//!
//! ```text
//! WordLir → TechMapper → GateNetlist → [This Module] → SIR → Simulator
//! ```
//!
//! The conversion preserves:
//! - FIT rates from library cells
//! - Hierarchical paths for RTL traceability
//! - Cell-to-primitive mapping for fault injection targeting

use crate::sir::{
    CombBlockId, CombinationalBlock, EdgeType, ResetSpec, SeqBlockId, SequentialBlock, Sir,
    SirDetectionMode, SirModule, SirOperation, SirPortDirection, SirSignal, SirSignalId,
    SirSignalType, StructuralBlockInfo,
};
use indexmap::IndexMap;
use skalp_frontend::hir::DetectionMode;
use skalp_lir::gate_netlist::{Cell, CellId, GateNet, GateNetId, GateNetlist};
use skalp_lir::lir::{PrimitiveId, PrimitiveType};
use skalp_lir::tech_library::CellFunction;
use std::collections::VecDeque;

// ============================================================================
// Converter
// ============================================================================

/// Converts GateNetlist to SIR for simulation
pub struct GateNetlistToSirConverter {
    /// Mapping from GateNetId to SirSignalId
    net_to_signal: IndexMap<GateNetId, SirSignalId>,
    /// Mapping from CellId to PrimitiveId
    cell_to_primitive: IndexMap<CellId, PrimitiveId>,
    /// Next signal ID
    next_signal_id: u32,
    /// Next primitive ID
    next_primitive_id: u32,
    /// Accumulated FIT for structural info
    total_fit: f64,
}

impl GateNetlistToSirConverter {
    /// Create a new converter
    pub fn new() -> Self {
        Self {
            net_to_signal: IndexMap::new(),
            cell_to_primitive: IndexMap::new(),
            next_signal_id: 0,
            next_primitive_id: 0,
            total_fit: 0.0,
        }
    }

    /// Convert a GateNetlist to SIR
    pub fn convert(&mut self, netlist: &GateNetlist) -> GateNetlistToSirResult {
        let mut sir = Sir::new(netlist.name.clone());

        // Phase 1: Convert all nets to signals
        self.convert_nets(&netlist.nets, &mut sir.top_module);

        // Phase 2: Convert cells to primitives
        let (mut comb_ops, seq_ops) = self.convert_cells(&netlist.cells);

        // Phase 2.5: Create buffer primitives for output ports that have drivers
        // This ensures output ports are connected to their driving nets
        self.create_output_buffers(netlist, &mut comb_ops);

        // Phase 3: Build combinational and sequential blocks
        self.build_blocks(&mut sir.top_module, comb_ops, seq_ops, netlist);

        // Create result with conversion stats
        let has_comb = netlist.cells.iter().any(|c| !c.is_sequential());
        let has_seq = netlist.cells.iter().any(|c| c.is_sequential());

        GateNetlistToSirResult {
            sir,
            stats: ConversionStats {
                signals_created: self.next_signal_id as usize,
                primitives_created: self.next_primitive_id as usize,
                comb_blocks: if has_comb { 1 } else { 0 },
                seq_blocks: if has_seq { 1 } else { 0 },
                total_fit: self.total_fit,
            },
        }
    }

    /// Convert all nets to SIR signals
    fn convert_nets(&mut self, nets: &[GateNet], module: &mut SirModule) {
        for net in nets {
            let signal_id = SirSignalId(self.next_signal_id);
            self.next_signal_id += 1;

            let signal_type = if net.is_clock {
                SirSignalType::Port {
                    direction: SirPortDirection::Input,
                }
            } else if net.is_reset {
                SirSignalType::Port {
                    direction: SirPortDirection::Input,
                }
            } else if net.is_input {
                SirSignalType::Port {
                    direction: SirPortDirection::Input,
                }
            } else if net.is_output {
                SirSignalType::Port {
                    direction: SirPortDirection::Output,
                }
            } else {
                SirSignalType::Wire
            };

            // Convert detection mode from HIR DetectionMode to SIR SirDetectionMode
            let detection_mode = net
                .detection_config
                .as_ref()
                .map(|c| match c.mode {
                    DetectionMode::Continuous => SirDetectionMode::Continuous,
                    DetectionMode::Boot => SirDetectionMode::Boot,
                    DetectionMode::Periodic => SirDetectionMode::Periodic,
                    DetectionMode::OnDemand => SirDetectionMode::OnDemand,
                })
                .unwrap_or(SirDetectionMode::Continuous);

            let signal = SirSignal {
                id: signal_id,
                name: net.name.clone(),
                width: 1, // Gate-level nets are typically single-bit
                signal_type,
                initial_value: None,
                is_detection: net.is_detection, // Propagate detection signal flag
                detection_mode,                 // Propagate detection mode for FI analysis
            };

            module.signals.push(signal);
            self.net_to_signal.insert(net.id, signal_id);
        }
    }

    /// Convert cells to primitive operations
    fn convert_cells(&mut self, cells: &[Cell]) -> (Vec<SirOperation>, Vec<SirOperation>) {
        let mut comb_ops = Vec::new();
        let mut seq_ops = Vec::new();

        for cell in cells {
            let primitive_id = PrimitiveId(self.next_primitive_id);
            self.next_primitive_id += 1;
            self.cell_to_primitive.insert(cell.id, primitive_id);

            // Accumulate FIT
            self.total_fit += cell.fit;

            // Map cell type to PrimitiveType
            // Priority:
            // 1. LUT init value -> LUT4/LUT6
            // 2. Cell function from library -> direct mapping
            // 3. Cell type name -> pattern matching fallback
            let ptype = if let Some(init) = cell.lut_init {
                // Determine LUT size from number of inputs
                let num_inputs = cell.inputs.len();
                if num_inputs <= 4 {
                    PrimitiveType::Lut4 { init: init as u16 }
                } else {
                    PrimitiveType::Lut6 { init }
                }
            } else if let Some(func) = &cell.function {
                // Use the library-provided function for accurate mapping
                self.cell_function_to_primitive(func)
            } else {
                // Fallback to cell type name parsing
                self.cell_type_to_primitive(&cell.cell_type)
            };

            // Convert input/output net IDs to signal IDs
            let inputs: Vec<SirSignalId> = cell
                .inputs
                .iter()
                .filter_map(|net_id| self.net_to_signal.get(net_id).copied())
                .collect();

            let outputs: Vec<SirSignalId> = cell
                .outputs
                .iter()
                .filter_map(|net_id| self.net_to_signal.get(net_id).copied())
                .collect();

            let op = SirOperation::Primitive {
                id: primitive_id,
                ptype,
                inputs,
                outputs,
                path: cell.path.clone(),
            };

            if cell.is_sequential() {
                seq_ops.push(op);
            } else {
                comb_ops.push(op);
            }
        }

        (comb_ops, seq_ops)
    }

    /// Create buffer primitives for output ports that have drivers
    ///
    /// In the gate netlist, output ports may be driven by internal nets via assignments
    /// like `output_port = internal_signal;`. We need to create buffer primitives to
    /// connect the driver output to the output port signal.
    fn create_output_buffers(&mut self, netlist: &GateNetlist, comb_ops: &mut Vec<SirOperation>) {
        // Debug: List all output ports and their driver status
        if std::env::var("SKALP_DEBUG_GPU").is_ok() {
            // Print cell ID range in netlist
            let min_id = netlist.cells.iter().map(|c| c.id.0).min().unwrap_or(0);
            let max_id = netlist.cells.iter().map(|c| c.id.0).max().unwrap_or(0);
            println!(
                "[SIR] Netlist has {} cells with IDs from {} to {}",
                netlist.cells.len(),
                min_id,
                max_id
            );

            println!("[SIR] Output port analysis:");
            for net in &netlist.nets {
                if net.is_output && net.name.contains("add_result[0]") {
                    let driver_info = if let Some(driver_id) = &net.driver {
                        if let Some(cell) = netlist.cells.iter().find(|c| c.id == *driver_id) {
                            format!("cell_type={}, outputs={:?}", cell.cell_type, cell.outputs)
                        } else {
                            format!("CELL {} NOT FOUND in netlist", driver_id.0)
                        }
                    } else {
                        "NO DRIVER".to_string()
                    };
                    println!(
                        "[SIR]   Output '{}' (net_id={:?}): driver={:?}, {}",
                        net.name, net.id, net.driver, driver_info
                    );
                }
            }
        }

        for net in &netlist.nets {
            // Only process output ports that have a driver
            if net.is_output && net.driver.is_some() {
                let output_signal_id = match self.net_to_signal.get(&net.id) {
                    Some(id) => *id,
                    None => continue,
                };

                // Find the driver cell and its output net
                let driver_cell_id = net.driver.unwrap();
                let driver_pin = net.driver_pin.unwrap_or(0);

                // Find the driver cell to get its output net
                if let Some(driver_cell) = netlist.cells.iter().find(|c| c.id == driver_cell_id) {
                    if let Some(&driver_output_net_id) = driver_cell.outputs.get(driver_pin) {
                        // Get the signal ID for the driver's output
                        if let Some(&driver_signal_id) =
                            self.net_to_signal.get(&driver_output_net_id)
                        {
                            // Only create buffer if the driver output is different from the output port
                            if driver_signal_id != output_signal_id {
                                // Create a buffer primitive: driver_output -> output_port
                                let primitive_id = PrimitiveId(self.next_primitive_id);
                                self.next_primitive_id += 1;

                                let buffer_op = SirOperation::Primitive {
                                    id: primitive_id,
                                    ptype: PrimitiveType::Buf,
                                    inputs: vec![driver_signal_id],
                                    outputs: vec![output_signal_id],
                                    path: format!("output_buffer_{}", net.name),
                                };

                                if std::env::var("SKALP_DEBUG_GPU").is_ok() {
                                    println!(
                                        "[SIR]   Created buffer for output '{}': {} -> {}",
                                        net.name, driver_signal_id.0, output_signal_id.0
                                    );
                                }

                                comb_ops.push(buffer_op);
                            }
                        }
                    }
                }
            }
        }
    }

    /// Map CellFunction (from library) to PrimitiveType
    /// This is the preferred method as it uses the library's semantic function definition
    fn cell_function_to_primitive(&self, func: &CellFunction) -> PrimitiveType {
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
            CellFunction::AndNot => PrimitiveType::And { inputs: 2 }, // Approximation
            CellFunction::OrNot => PrimitiveType::Or { inputs: 2 },   // Approximation

            // Complex gates (approximations)
            CellFunction::Aoi21 | CellFunction::Aoi22 => PrimitiveType::Nand { inputs: 2 },
            CellFunction::Oai21 | CellFunction::Oai22 => PrimitiveType::Nor { inputs: 2 },

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
            CellFunction::Dff => PrimitiveType::DffP,
            CellFunction::DffR => PrimitiveType::DffP, // DFF with reset
            CellFunction::DffE => PrimitiveType::DffE, // DFF with enable
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
    fn cell_type_to_primitive(&self, cell_type: &str) -> PrimitiveType {
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
        if upper.starts_with("FP32_LT")
            || upper.starts_with("FPLT32")
            || upper.starts_with("FP_LT32")
        {
            return PrimitiveType::Fp32Lt;
        }
        if upper.starts_with("FP32_GT")
            || upper.starts_with("FPGT32")
            || upper.starts_with("FP_GT32")
        {
            return PrimitiveType::Fp32Gt;
        }
        if upper.starts_with("FP32_LE")
            || upper.starts_with("FPLE32")
            || upper.starts_with("FP_LE32")
        {
            return PrimitiveType::Fp32Le;
        }
        if upper.starts_with("FP32_GE")
            || upper.starts_with("FPGE32")
            || upper.starts_with("FP_GE32")
        {
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

            // Complex gates - map to equivalent primitive gates
            // AOI21 = ~(A & (B | C)) - map to NAND as approximation
            "AOI21" | "AOI22" => PrimitiveType::Nand { inputs: 2 },
            // OAI21 = ~(A | (B & C)) - map to NOR as approximation
            "OAI21" | "OAI22" => PrimitiveType::Nor { inputs: 2 },

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
            "DFF" | "DFFRQ" => PrimitiveType::DffP, // Rising edge, async reset
            "DFFN" | "DFFRN" => PrimitiveType::DffN, // Active-low reset
            "DFFNEG" => PrimitiveType::DffNeg,      // Falling edge
            "DFFE" => PrimitiveType::DffE,          // With enable
            "DFFAR" => PrimitiveType::DffAR,        // Async reset
            "DFFAS" => PrimitiveType::DffAS,        // Async set
            "DFFSCAN" => PrimitiveType::DffScan,    // Scan chain

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
                eprintln!(
                    "Warning: Unknown cell type '{}', mapping to buffer",
                    cell_type
                );
                PrimitiveType::Buf // Use buffer as fallback
            }
        }
    }

    /// Compute topological order for operations using Kahn's algorithm
    ///
    /// Returns indices into the operations vector in evaluation order.
    /// Operations with no dependencies come first.
    fn compute_topological_order(ops: &[SirOperation]) -> Vec<usize> {
        let n = ops.len();
        if n == 0 {
            return vec![];
        }

        // Build dependency graph
        // signal_producers: signal_id -> operation index that produces it
        let mut signal_producers: IndexMap<u32, usize> = IndexMap::new();

        // For each operation, record which signals it produces
        for (idx, op) in ops.iter().enumerate() {
            if let SirOperation::Primitive { outputs, .. } = op {
                for out_sig in outputs {
                    signal_producers.insert(out_sig.0, idx);
                }
            }
        }

        // Build dependency counts: how many operations must run before this one
        let mut in_degree = vec![0usize; n];
        let mut dependents: Vec<Vec<usize>> = vec![Vec::new(); n];

        for (idx, op) in ops.iter().enumerate() {
            if let SirOperation::Primitive { inputs, .. } = op {
                for in_sig in inputs {
                    if let Some(&producer_idx) = signal_producers.get(&in_sig.0) {
                        if producer_idx != idx {
                            in_degree[idx] += 1;
                            dependents[producer_idx].push(idx);
                        }
                    }
                }
            }
        }

        // Kahn's algorithm
        let mut result = Vec::with_capacity(n);
        let mut queue: VecDeque<usize> = VecDeque::new();

        // Start with operations that have no dependencies
        for (idx, &deg) in in_degree.iter().enumerate() {
            if deg == 0 {
                queue.push_back(idx);
            }
        }

        while let Some(idx) = queue.pop_front() {
            result.push(idx);

            // Reduce in-degree of dependent operations
            for &dep_idx in &dependents[idx] {
                in_degree[dep_idx] -= 1;
                if in_degree[dep_idx] == 0 {
                    queue.push_back(dep_idx);
                }
            }
        }

        // If we didn't process all operations, there's a cycle (shouldn't happen for combinational)
        if result.len() != n {
            // Fall back to original order
            eprintln!("Warning: Cycle detected in combinational logic, using original order");
            return (0..n).collect();
        }

        result
    }

    /// Build combinational and sequential blocks from operations
    fn build_blocks(
        &self,
        module: &mut SirModule,
        comb_ops: Vec<SirOperation>,
        seq_ops: Vec<SirOperation>,
        netlist: &GateNetlist,
    ) {
        // Build combinational block
        if !comb_ops.is_empty() {
            let mut inputs = Vec::new();
            let mut outputs = Vec::new();

            // Collect all inputs and outputs from operations
            for op in &comb_ops {
                if let SirOperation::Primitive {
                    inputs: op_inputs,
                    outputs: op_outputs,
                    ..
                } = op
                {
                    for &input in op_inputs {
                        if !inputs.contains(&input) && !outputs.contains(&input) {
                            inputs.push(input);
                        }
                    }
                    for &output in op_outputs {
                        if !outputs.contains(&output) {
                            outputs.push(output);
                        }
                    }
                }
            }

            // Remove internal signals from inputs (signals driven by other ops)
            inputs.retain(|sig| !outputs.contains(sig));

            // Compute topological order for correct evaluation
            let eval_order = Self::compute_topological_order(&comb_ops);

            let comb_block = CombinationalBlock {
                id: CombBlockId(0),
                inputs,
                outputs,
                operations: comb_ops,
                workgroup_size_hint: Some(64),
                structural_info: Some(StructuralBlockInfo {
                    is_structural: true,
                    eval_order: Some(eval_order),
                    total_fit: self.total_fit,
                }),
            };

            module.comb_blocks.push(comb_block);
        }

        // Build sequential block (if any sequential cells exist)
        if !seq_ops.is_empty() {
            // Find clock signal (use first clock from netlist)
            let clock_signal = netlist
                .clocks
                .first()
                .and_then(|clk_net| self.net_to_signal.get(clk_net).copied())
                .unwrap_or(SirSignalId(0));

            // Find reset signal if present
            let reset = netlist.resets.first().and_then(|rst_net| {
                self.net_to_signal.get(rst_net).map(|&signal| ResetSpec {
                    signal,
                    active_high: true, // Assume active-high reset
                    edge: None,        // Synchronous reset
                })
            });

            // Collect registers (output signals of sequential cells)
            let mut registers = Vec::new();
            for op in &seq_ops {
                if let SirOperation::Primitive { outputs, .. } = op {
                    registers.extend(outputs.iter().copied());
                }
            }

            let seq_block = SequentialBlock {
                id: SeqBlockId(0),
                clock: clock_signal,
                clock_edge: EdgeType::Rising,
                reset,
                registers,
                operations: seq_ops,
            };

            module.seq_blocks.push(seq_block);
        }
    }

    /// Get the signal ID for a net
    pub fn get_signal_id(&self, net_id: GateNetId) -> Option<SirSignalId> {
        self.net_to_signal.get(&net_id).copied()
    }

    /// Get the primitive ID for a cell
    pub fn get_primitive_id(&self, cell_id: CellId) -> Option<PrimitiveId> {
        self.cell_to_primitive.get(&cell_id).copied()
    }
}

impl Default for GateNetlistToSirConverter {
    fn default() -> Self {
        Self::new()
    }
}

// ============================================================================
// Result Types
// ============================================================================

/// Result of GateNetlist to SIR conversion
#[derive(Debug)]
pub struct GateNetlistToSirResult {
    /// The converted SIR
    pub sir: Sir,
    /// Conversion statistics
    pub stats: ConversionStats,
}

/// Statistics from the conversion
#[derive(Debug, Clone, Default)]
pub struct ConversionStats {
    /// Number of SIR signals created
    pub signals_created: usize,
    /// Number of primitives created
    pub primitives_created: usize,
    /// Number of combinational blocks
    pub comb_blocks: usize,
    /// Number of sequential blocks
    pub seq_blocks: usize,
    /// Total FIT from all cells
    pub total_fit: f64,
}

// ============================================================================
// Convenience Function
// ============================================================================

/// Convert a GateNetlist to SIR
pub fn convert_gate_netlist_to_sir(netlist: &GateNetlist) -> GateNetlistToSirResult {
    let mut converter = GateNetlistToSirConverter::new();
    converter.convert(netlist)
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use skalp_lir::gate_netlist::{Cell, GateNet};

    fn create_simple_netlist() -> GateNetlist {
        let mut netlist = GateNetlist::new("test".to_string(), "generic_asic".to_string());

        // Add inputs
        let a = netlist.add_input("a".to_string());
        let b = netlist.add_input("b".to_string());

        // Add internal net
        let and_out = netlist.add_net(GateNet::new(GateNetId(0), "and_out".to_string()));

        // Add output
        let y = netlist.add_output("y".to_string());

        // Add NAND gate
        let nand_cell = Cell::new_comb(
            CellId(0),
            "NAND2_X1".to_string(),
            "generic_asic".to_string(),
            0.1,
            "top.nand".to_string(),
            vec![a, b],
            vec![and_out],
        );
        netlist.add_cell(nand_cell);

        // Add inverter (to make output = a AND b)
        let inv_cell = Cell::new_comb(
            CellId(0),
            "INV_X1".to_string(),
            "generic_asic".to_string(),
            0.05,
            "top.inv".to_string(),
            vec![and_out],
            vec![y],
        );
        netlist.add_cell(inv_cell);

        netlist
    }

    #[test]
    fn test_basic_conversion() {
        let netlist = create_simple_netlist();
        let result = convert_gate_netlist_to_sir(&netlist);

        assert_eq!(result.sir.name, "test");
        assert_eq!(result.stats.signals_created, 4); // a, b, and_out, y
        assert_eq!(result.stats.primitives_created, 2); // NAND, INV
        assert_eq!(result.stats.comb_blocks, 1);
        assert_eq!(result.stats.seq_blocks, 0);
        assert!((result.stats.total_fit - 0.15).abs() < 0.001);
    }

    #[test]
    fn test_signal_conversion() {
        let netlist = create_simple_netlist();
        let result = convert_gate_netlist_to_sir(&netlist);

        let module = &result.sir.top_module;
        assert_eq!(module.signals.len(), 4);

        // Check input signals
        let input_signals: Vec<_> = module
            .signals
            .iter()
            .filter(|s| {
                matches!(
                    s.signal_type,
                    SirSignalType::Port {
                        direction: SirPortDirection::Input
                    }
                )
            })
            .collect();
        assert_eq!(input_signals.len(), 2);

        // Check output signal
        let output_signals: Vec<_> = module
            .signals
            .iter()
            .filter(|s| {
                matches!(
                    s.signal_type,
                    SirSignalType::Port {
                        direction: SirPortDirection::Output
                    }
                )
            })
            .collect();
        assert_eq!(output_signals.len(), 1);
    }

    #[test]
    fn test_primitive_operations() {
        let netlist = create_simple_netlist();
        let result = convert_gate_netlist_to_sir(&netlist);

        let comb_block = &result.sir.top_module.comb_blocks[0];
        assert_eq!(comb_block.operations.len(), 2);

        // Check that operations are Primitive type
        for op in &comb_block.operations {
            match op {
                SirOperation::Primitive { ptype, path, .. } => {
                    assert!(path.starts_with("top."));
                    // Should be NAND or INV
                    assert!(matches!(
                        ptype,
                        PrimitiveType::Nand { inputs: 2 } | PrimitiveType::Inv
                    ));
                }
                _ => panic!("Expected Primitive operation"),
            }
        }
    }

    #[test]
    fn test_structural_info() {
        let netlist = create_simple_netlist();
        let result = convert_gate_netlist_to_sir(&netlist);

        let comb_block = &result.sir.top_module.comb_blocks[0];
        assert!(comb_block.structural_info.is_some());

        let info = comb_block.structural_info.as_ref().unwrap();
        assert!(info.is_structural);
        assert!((info.total_fit - 0.15).abs() < 0.001);
    }

    #[test]
    fn test_sequential_cell_conversion() {
        let mut netlist = GateNetlist::new("seq_test".to_string(), "generic_asic".to_string());

        // Add clock and reset
        let clk = netlist.add_clock("clk".to_string());
        let rst = netlist.add_reset("rst".to_string());

        // Add data input
        let d = netlist.add_input("d".to_string());

        // Add output
        let q = netlist.add_output("q".to_string());

        // Add DFF
        let dff = Cell::new_seq(
            CellId(0),
            "DFF_X1".to_string(),
            "generic_asic".to_string(),
            0.2,
            "top.dff".to_string(),
            vec![d],
            vec![q],
            clk,
            Some(rst),
        );
        netlist.add_cell(dff);

        let result = convert_gate_netlist_to_sir(&netlist);

        assert_eq!(result.stats.seq_blocks, 1);
        assert_eq!(result.stats.comb_blocks, 0); // No combinational cells

        let seq_block = &result.sir.top_module.seq_blocks[0];
        assert_eq!(seq_block.operations.len(), 1);
        assert!(seq_block.reset.is_some());
    }

    #[test]
    fn test_cell_type_mapping() {
        let converter = GateNetlistToSirConverter::new();

        assert!(matches!(
            converter.cell_type_to_primitive("NAND2_X1"),
            PrimitiveType::Nand { inputs: 2 }
        ));
        assert!(matches!(
            converter.cell_type_to_primitive("AND3_X2"),
            PrimitiveType::And { inputs: 3 }
        ));
        assert!(matches!(
            converter.cell_type_to_primitive("XOR2"),
            PrimitiveType::Xor
        ));
        assert!(matches!(
            converter.cell_type_to_primitive("DFF_X1"),
            PrimitiveType::DffP
        ));
        assert!(matches!(
            converter.cell_type_to_primitive("FA_X1"),
            PrimitiveType::FullAdder
        ));
        assert!(matches!(
            converter.cell_type_to_primitive("MUX2"),
            PrimitiveType::Mux2
        ));
    }

    #[test]
    fn test_mixed_comb_seq() {
        let mut netlist = GateNetlist::new("mixed".to_string(), "generic_asic".to_string());

        let clk = netlist.add_clock("clk".to_string());
        let a = netlist.add_input("a".to_string());
        let b = netlist.add_input("b".to_string());
        let q = netlist.add_output("q".to_string());
        let and_out = netlist.add_net(GateNet::new(GateNetId(0), "and_out".to_string()));

        // Combinational: AND gate
        let and_cell = Cell::new_comb(
            CellId(0),
            "AND2_X1".to_string(),
            "generic_asic".to_string(),
            0.1,
            "top.and".to_string(),
            vec![a, b],
            vec![and_out],
        );
        netlist.add_cell(and_cell);

        // Sequential: DFF
        let dff = Cell::new_seq(
            CellId(0),
            "DFF_X1".to_string(),
            "generic_asic".to_string(),
            0.2,
            "top.dff".to_string(),
            vec![and_out],
            vec![q],
            clk,
            None,
        );
        netlist.add_cell(dff);

        let result = convert_gate_netlist_to_sir(&netlist);

        assert_eq!(result.stats.comb_blocks, 1);
        assert_eq!(result.stats.seq_blocks, 1);
        assert_eq!(result.stats.primitives_created, 2);
        assert!((result.stats.total_fit - 0.3).abs() < 0.001);
    }
}
