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
    SirModule, SirOperation, SirPortDirection, SirSignal, SirSignalId, SirSignalType,
    StructuralBlockInfo,
};
use skalp_lir::gate_netlist::{Cell, CellId, GateNet, GateNetId, GateNetlist};
use skalp_lir::lir::{PrimitiveId, PrimitiveType};
use std::collections::HashMap;

// ============================================================================
// Converter
// ============================================================================

/// Converts GateNetlist to SIR for simulation
pub struct GateNetlistToSirConverter {
    /// Mapping from GateNetId to SirSignalId
    net_to_signal: HashMap<GateNetId, SirSignalId>,
    /// Mapping from CellId to PrimitiveId
    cell_to_primitive: HashMap<CellId, PrimitiveId>,
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
            net_to_signal: HashMap::new(),
            cell_to_primitive: HashMap::new(),
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
        let (comb_ops, seq_ops) = self.convert_cells(&netlist.cells);

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

            let signal = SirSignal {
                id: signal_id,
                name: net.name.clone(),
                width: 1, // Gate-level nets are typically single-bit
                signal_type,
                initial_value: None,
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
            let ptype = self.cell_type_to_primitive(&cell.cell_type);

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

    /// Map library cell type name to PrimitiveType
    fn cell_type_to_primitive(&self, cell_type: &str) -> PrimitiveType {
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

            let comb_block = CombinationalBlock {
                id: CombBlockId(0),
                inputs,
                outputs,
                operations: comb_ops,
                workgroup_size_hint: Some(64),
                structural_info: Some(StructuralBlockInfo {
                    is_structural: true,
                    eval_order: None, // TODO: Compute topological order
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
