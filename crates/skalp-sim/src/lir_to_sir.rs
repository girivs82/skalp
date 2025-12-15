//! LIR to SIR Conversion for Gate-Level Simulation
//!
//! Converts a `Lir` (gate-level netlist) to `Sir` in structural mode, enabling
//! gate-level simulation with fault injection support.
//!
//! # Conversion Process
//!
//! 1. Create SIR signals for all LIR nets
//! 2. Separate primitives into combinational and sequential blocks
//! 3. Compute topological order for combinational evaluation
//! 4. Map primitives to `SirOperation::Primitive` variants
//! 5. Calculate FIT statistics for the structural blocks

use crate::sir::{
    CombBlockId, CombinationalBlock, EdgeType, ResetSpec, SeqBlockId, SequentialBlock, Sir,
    SirModule, SirOperation, SirPortDirection, SirSignal, SirSignalId, SirSignalType,
    StructuralBlockInfo,
};
use bitvec::prelude::*;
use petgraph::algo::toposort;
use petgraph::graph::DiGraph;
use skalp_lir::lir::{Lir, LirNet, NetId, Primitive, PrimitiveId};
use std::collections::HashMap;

/// Result of LIR to SIR conversion
#[derive(Debug)]
pub struct LirToSirResult {
    /// The converted SIR
    pub sir: Sir,
    /// Mapping from LIR net IDs to SIR signal IDs
    pub net_to_signal: HashMap<NetId, SirSignalId>,
    /// Mapping from LIR primitive IDs to SIR operation indices
    pub primitive_to_op: HashMap<PrimitiveId, (usize, usize)>, // (block_idx, op_idx)
    /// Total FIT contribution
    pub total_fit: f64,
    /// Conversion statistics
    pub stats: ConversionStats,
}

/// Statistics from conversion
#[derive(Debug, Default)]
pub struct ConversionStats {
    /// Number of signals created
    pub signals: usize,
    /// Number of combinational blocks created
    pub comb_blocks: usize,
    /// Number of sequential blocks created
    pub seq_blocks: usize,
    /// Number of primitive operations created
    pub primitive_ops: usize,
    /// Number of primitives that are combinational
    pub comb_primitives: usize,
    /// Number of primitives that are sequential
    pub seq_primitives: usize,
}

/// Converter from LIR Lir to SIR
pub struct LirToSirConverter {
    /// Mapping from LIR net IDs to SIR signal IDs
    net_to_signal: HashMap<NetId, SirSignalId>,
    /// Next signal ID to assign
    next_signal_id: u32,
    /// Next comb block ID
    next_comb_id: u32,
    /// Next seq block ID
    next_seq_id: u32,
    /// Mapping from primitive to operation location
    primitive_to_op: HashMap<PrimitiveId, (usize, usize)>,
}

impl LirToSirConverter {
    /// Create a new converter
    pub fn new() -> Self {
        Self {
            net_to_signal: HashMap::new(),
            next_signal_id: 0,
            next_comb_id: 0,
            next_seq_id: 0,
            primitive_to_op: HashMap::new(),
        }
    }

    /// Convert a Lir to SIR
    pub fn convert(&mut self, netlist: &Lir) -> LirToSirResult {
        let mut stats = ConversionStats::default();

        // Phase 1: Create signals for all nets
        let signals = self.convert_nets(&netlist.nets, &netlist.inputs, &netlist.outputs);
        stats.signals = signals.len();

        // Phase 2: Partition primitives into combinational and sequential
        let (comb_prims, seq_prims): (Vec<_>, Vec<_>) = netlist
            .primitives
            .iter()
            .partition(|p| !p.ptype.is_sequential());

        stats.comb_primitives = comb_prims.len();
        stats.seq_primitives = seq_prims.len();

        // Phase 3: Create combinational blocks with topological order
        let comb_blocks = self.create_comb_blocks(&comb_prims, netlist);
        stats.comb_blocks = comb_blocks.len();

        // Phase 4: Create sequential blocks (grouped by clock domain)
        let seq_blocks = self.create_seq_blocks(&seq_prims, netlist);
        stats.seq_blocks = seq_blocks.len();

        // Count primitive operations
        for block in &comb_blocks {
            stats.primitive_ops += block.operations.len();
        }
        for block in &seq_blocks {
            stats.primitive_ops += block.operations.len();
        }

        // Calculate total FIT
        let total_fit = netlist.primitives.iter().map(|p| p.fit()).sum();

        // Build the SIR module
        let module = SirModule {
            name: netlist.name.clone(),
            signals,
            comb_blocks,
            seq_blocks,
            instances: Vec::new(),   // No sub-instances in flattened netlist
            connections: Vec::new(), // All connections are via signals
        };

        let sir = Sir {
            name: netlist.name.clone(),
            top_module: module,
            modules: HashMap::new(),
        };

        LirToSirResult {
            sir,
            net_to_signal: self.net_to_signal.clone(),
            primitive_to_op: self.primitive_to_op.clone(),
            total_fit,
            stats,
        }
    }

    /// Convert LIR nets to SIR signals
    fn convert_nets(
        &mut self,
        nets: &[LirNet],
        _inputs: &[NetId],
        _outputs: &[NetId],
    ) -> Vec<SirSignal> {
        let mut signals = Vec::with_capacity(nets.len());

        for net in nets {
            let signal_id = self.alloc_signal_id();
            self.net_to_signal.insert(net.id, signal_id);

            let signal_type = if net.is_primary_input {
                SirSignalType::Port {
                    direction: SirPortDirection::Input,
                }
            } else if net.is_primary_output {
                SirSignalType::Port {
                    direction: SirPortDirection::Output,
                }
            } else if net.is_state_output {
                // This net is driven by a flip-flop - it's a register
                // We'll set the clock later when we process sequential blocks
                SirSignalType::Register {
                    clock: SirSignalId(0), // Placeholder, set later
                    reset: None,
                    reset_active_high: true,
                }
            } else {
                SirSignalType::Wire
            };

            signals.push(SirSignal {
                id: signal_id,
                name: net.name.clone(),
                width: net.width as usize,
                signal_type,
                initial_value: Some(bitvec![0; net.width as usize]),
            });
        }

        signals
    }

    /// Create combinational blocks from combinational primitives
    fn create_comb_blocks(
        &mut self,
        comb_prims: &[&Primitive],
        netlist: &Lir,
    ) -> Vec<CombinationalBlock> {
        if comb_prims.is_empty() {
            return Vec::new();
        }

        // Build dependency graph for topological sort
        let (topo_order, prim_indices) = self.compute_topological_order(comb_prims, netlist);

        // Create operations in topological order
        let mut operations = Vec::with_capacity(comb_prims.len());
        let mut total_fit = 0.0;

        // Collect all inputs and outputs for the block
        let mut all_inputs = Vec::new();
        let mut all_outputs = Vec::new();

        for idx in &topo_order {
            let prim_idx = prim_indices[*idx];
            let prim = comb_prims[prim_idx];

            // Convert inputs
            let inputs: Vec<SirSignalId> = prim
                .inputs
                .iter()
                .filter_map(|net_id| self.net_to_signal.get(net_id).copied())
                .collect();

            // Convert outputs
            let outputs: Vec<SirSignalId> = prim
                .outputs
                .iter()
                .filter_map(|net_id| self.net_to_signal.get(net_id).copied())
                .collect();

            // Track inputs/outputs for block
            for input in &inputs {
                if !all_inputs.contains(input) && !all_outputs.contains(input) {
                    all_inputs.push(*input);
                }
            }
            for output in &outputs {
                if !all_outputs.contains(output) {
                    all_outputs.push(*output);
                }
            }

            let op = SirOperation::Primitive {
                id: prim.id,
                ptype: prim.ptype.clone(),
                inputs,
                outputs,
                path: prim.path.clone(),
            };

            // Store mapping
            self.primitive_to_op.insert(prim.id, (0, operations.len()));

            operations.push(op);
            total_fit += prim.fit();
        }

        // Create a single combinational block containing all comb logic
        // (Could split into multiple blocks for GPU parallelism in future)
        // Operations are already in topological order, so eval_order is just 0, 1, 2, ...
        let eval_order: Vec<usize> = (0..operations.len()).collect();
        let block = CombinationalBlock {
            id: self.alloc_comb_id(),
            inputs: all_inputs,
            outputs: all_outputs,
            operations,
            workgroup_size_hint: Some(64), // Default for GPU
            structural_info: Some(StructuralBlockInfo {
                is_structural: true,
                eval_order: Some(eval_order),
                total_fit,
            }),
        };

        vec![block]
    }

    /// Compute topological order for combinational primitives
    fn compute_topological_order(
        &self,
        comb_prims: &[&Primitive],
        _netlist: &Lir,
    ) -> (Vec<usize>, Vec<usize>) {
        let mut graph = DiGraph::<usize, ()>::new();
        let mut prim_to_node = HashMap::new();
        let mut prim_indices = Vec::new();

        // Add nodes for each primitive
        for (idx, prim) in comb_prims.iter().enumerate() {
            let node = graph.add_node(idx);
            prim_to_node.insert(prim.id, node);
            prim_indices.push(idx);
        }

        // Build output-to-primitive mapping for dependency edges
        let mut output_to_prim: HashMap<NetId, PrimitiveId> = HashMap::new();
        for prim in comb_prims {
            for output in &prim.outputs {
                output_to_prim.insert(*output, prim.id);
            }
        }

        // Add edges: if prim B's input is driven by prim A's output, A -> B
        for prim in comb_prims {
            let b_node = prim_to_node[&prim.id];
            for input in &prim.inputs {
                if let Some(a_id) = output_to_prim.get(input) {
                    if let Some(&a_node) = prim_to_node.get(a_id) {
                        graph.add_edge(a_node, b_node, ());
                    }
                }
            }
        }

        // Perform topological sort
        match toposort(&graph, None) {
            Ok(order) => {
                let topo_order: Vec<usize> = order.into_iter().map(|n| graph[n]).collect();
                (topo_order, prim_indices)
            }
            Err(_) => {
                // Cycle detected - shouldn't happen in combinational logic
                // Fall back to original order
                let order: Vec<usize> = (0..comb_prims.len()).collect();
                (order, prim_indices)
            }
        }
    }

    /// Create sequential blocks from sequential primitives
    fn create_seq_blocks(
        &mut self,
        seq_prims: &[&Primitive],
        _netlist: &Lir,
    ) -> Vec<SequentialBlock> {
        if seq_prims.is_empty() {
            return Vec::new();
        }

        // Group primitives by clock signal
        let mut by_clock: HashMap<NetId, Vec<&Primitive>> = HashMap::new();

        for prim in seq_prims {
            if let Some(clk) = prim.clock {
                by_clock.entry(clk).or_default().push(*prim);
            }
        }

        let mut blocks = Vec::new();
        let mut block_idx = 0;

        for (clk_net, prims) in by_clock {
            let clk_signal = self
                .net_to_signal
                .get(&clk_net)
                .copied()
                .unwrap_or(SirSignalId(0));

            let mut operations = Vec::new();
            let mut registers = Vec::new();
            let mut reset_signal = None;

            for prim in &prims {
                // Convert inputs
                let inputs: Vec<SirSignalId> = prim
                    .inputs
                    .iter()
                    .filter_map(|net_id| self.net_to_signal.get(net_id).copied())
                    .collect();

                // Convert outputs
                let outputs: Vec<SirSignalId> = prim
                    .outputs
                    .iter()
                    .filter_map(|net_id| self.net_to_signal.get(net_id).copied())
                    .collect();

                // Track output registers
                for output in &outputs {
                    if !registers.contains(output) {
                        registers.push(*output);
                    }
                }

                // Track reset if present
                if reset_signal.is_none() {
                    if let Some(rst) = prim.reset {
                        reset_signal = self.net_to_signal.get(&rst).copied();
                    }
                }

                let op = SirOperation::Primitive {
                    id: prim.id,
                    ptype: prim.ptype.clone(),
                    inputs,
                    outputs,
                    path: prim.path.clone(),
                };

                // Store mapping
                self.primitive_to_op
                    .insert(prim.id, (block_idx, operations.len()));

                operations.push(op);
            }

            let reset = reset_signal.map(|sig| ResetSpec {
                signal: sig,
                active_high: true, // Default; could be determined from primitive type
                edge: None,        // Async reset by default
            });

            let block = SequentialBlock {
                id: self.alloc_seq_id(),
                clock: clk_signal,
                clock_edge: EdgeType::Rising, // Default to rising edge
                reset,
                registers,
                operations,
            };

            blocks.push(block);
            block_idx += 1;
        }

        blocks
    }

    /// Allocate a new signal ID
    fn alloc_signal_id(&mut self) -> SirSignalId {
        let id = SirSignalId(self.next_signal_id);
        self.next_signal_id += 1;
        id
    }

    /// Allocate a new combinational block ID
    fn alloc_comb_id(&mut self) -> CombBlockId {
        let id = CombBlockId(self.next_comb_id);
        self.next_comb_id += 1;
        id
    }

    /// Allocate a new sequential block ID
    fn alloc_seq_id(&mut self) -> SeqBlockId {
        let id = SeqBlockId(self.next_seq_id);
        self.next_seq_id += 1;
        id
    }
}

impl Default for LirToSirConverter {
    fn default() -> Self {
        Self::new()
    }
}

/// Convert a Lir to SIR for gate-level simulation
///
/// # Example
///
/// ```ignore
/// use skalp_lir::lir::Lir;
/// use skalp_sim::lir_to_sir::convert_lir_to_sir;
///
/// let netlist = Lir::new("my_design".to_string());
/// let result = convert_lir_to_sir(&netlist);
/// println!("Converted {} signals, {} comb blocks, {} seq blocks",
///          result.stats.signals, result.stats.comb_blocks, result.stats.seq_blocks);
/// ```
pub fn convert_lir_to_sir(netlist: &Lir) -> LirToSirResult {
    let mut converter = LirToSirConverter::new();
    converter.convert(netlist)
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use skalp_lir::lir::{LirNet, Primitive, PrimitiveType};

    fn make_test_netlist() -> Lir {
        let mut netlist = Lir::new("test_design".to_string());

        // Create nets: input a, b, internal wire w, output y
        let net_a = LirNet::new_primary_input(NetId(0), "a".to_string());
        let net_b = LirNet::new_primary_input(NetId(1), "b".to_string());
        let mut net_w = LirNet::new(NetId(2), "w".to_string());
        net_w.driver = Some((PrimitiveId(0), 0));
        let net_y = LirNet::new_primary_output(NetId(3), "y".to_string(), (PrimitiveId(1), 0));

        netlist.add_net(net_a);
        netlist.add_net(net_b);
        netlist.add_net(net_w);
        netlist.add_net(net_y);

        netlist.inputs = vec![NetId(0), NetId(1)];
        netlist.outputs = vec![NetId(3)];

        // Create primitives: AND gate (a & b -> w), INV (!w -> y)
        let and_gate = Primitive::new_comb(
            PrimitiveId(0),
            PrimitiveType::And { inputs: 2 },
            "top.and_0".to_string(),
            vec![NetId(0), NetId(1)],
            vec![NetId(2)],
        );

        let inv_gate = Primitive::new_comb(
            PrimitiveId(1),
            PrimitiveType::Inv,
            "top.inv_0".to_string(),
            vec![NetId(2)],
            vec![NetId(3)],
        );

        netlist.add_primitive(and_gate);
        netlist.add_primitive(inv_gate);

        netlist
    }

    #[test]
    fn test_convert_empty_netlist() {
        let netlist = Lir::new("empty".to_string());
        let result = convert_lir_to_sir(&netlist);

        assert_eq!(result.sir.name, "empty");
        assert_eq!(result.stats.signals, 0);
        assert_eq!(result.stats.comb_blocks, 0);
        assert_eq!(result.stats.seq_blocks, 0);
    }

    #[test]
    fn test_convert_simple_comb_logic() {
        let netlist = make_test_netlist();
        let result = convert_lir_to_sir(&netlist);

        assert_eq!(result.sir.name, "test_design");
        assert_eq!(result.stats.signals, 4); // a, b, w, y
        assert_eq!(result.stats.comb_blocks, 1);
        assert_eq!(result.stats.seq_blocks, 0);
        assert_eq!(result.stats.primitive_ops, 2); // AND + INV
        assert_eq!(result.stats.comb_primitives, 2);
        assert_eq!(result.stats.seq_primitives, 0);
    }

    #[test]
    fn test_topological_order() {
        let netlist = make_test_netlist();
        let result = convert_lir_to_sir(&netlist);

        // Check that operations are in correct order (AND before INV)
        let comb_block = &result.sir.top_module.comb_blocks[0];
        assert_eq!(comb_block.operations.len(), 2);

        // First operation should be AND (produces w)
        match &comb_block.operations[0] {
            SirOperation::Primitive { ptype, .. } => {
                assert!(matches!(ptype, PrimitiveType::And { inputs: 2 }));
            }
            _ => panic!("Expected Primitive operation"),
        }

        // Second operation should be INV (consumes w)
        match &comb_block.operations[1] {
            SirOperation::Primitive { ptype, .. } => {
                assert!(matches!(ptype, PrimitiveType::Inv));
            }
            _ => panic!("Expected Primitive operation"),
        }
    }

    #[test]
    fn test_net_to_signal_mapping() {
        let netlist = make_test_netlist();
        let result = convert_lir_to_sir(&netlist);

        // All 4 nets should be mapped
        assert_eq!(result.net_to_signal.len(), 4);
        assert!(result.net_to_signal.contains_key(&NetId(0)));
        assert!(result.net_to_signal.contains_key(&NetId(1)));
        assert!(result.net_to_signal.contains_key(&NetId(2)));
        assert!(result.net_to_signal.contains_key(&NetId(3)));
    }

    #[test]
    fn test_primitive_to_op_mapping() {
        let netlist = make_test_netlist();
        let result = convert_lir_to_sir(&netlist);

        // Both primitives should be mapped
        assert_eq!(result.primitive_to_op.len(), 2);
        assert!(result.primitive_to_op.contains_key(&PrimitiveId(0)));
        assert!(result.primitive_to_op.contains_key(&PrimitiveId(1)));
    }

    #[test]
    fn test_signal_types() {
        let netlist = make_test_netlist();
        let result = convert_lir_to_sir(&netlist);

        let signals = &result.sir.top_module.signals;

        // Find signals by name
        let sig_a = signals.iter().find(|s| s.name == "a").unwrap();
        let sig_b = signals.iter().find(|s| s.name == "b").unwrap();
        let sig_w = signals.iter().find(|s| s.name == "w").unwrap();
        let sig_y = signals.iter().find(|s| s.name == "y").unwrap();

        // a and b should be input ports
        assert!(matches!(
            sig_a.signal_type,
            SirSignalType::Port {
                direction: SirPortDirection::Input
            }
        ));
        assert!(matches!(
            sig_b.signal_type,
            SirSignalType::Port {
                direction: SirPortDirection::Input
            }
        ));

        // w should be a wire
        assert!(matches!(sig_w.signal_type, SirSignalType::Wire));

        // y should be an output port
        assert!(matches!(
            sig_y.signal_type,
            SirSignalType::Port {
                direction: SirPortDirection::Output
            }
        ));
    }

    #[test]
    fn test_structural_info() {
        let netlist = make_test_netlist();
        let result = convert_lir_to_sir(&netlist);

        let comb_block = &result.sir.top_module.comb_blocks[0];
        assert!(comb_block.structural_info.is_some());

        let info = comb_block.structural_info.as_ref().unwrap();
        assert!(info.is_structural);
        assert!(info.eval_order.is_some());
        assert!(info.total_fit > 0.0); // AND + INV have non-zero FIT
    }

    #[test]
    fn test_total_fit() {
        let netlist = make_test_netlist();
        let result = convert_lir_to_sir(&netlist);

        // AND gate FIT = 0.1, INV FIT = 0.05
        let expected_fit = 0.1 + 0.05;
        assert!((result.total_fit - expected_fit).abs() < 0.001);
    }

    #[test]
    fn test_sequential_primitives() {
        let mut netlist = Lir::new("seq_test".to_string());

        // Create nets
        let net_clk = LirNet::new_primary_input(NetId(0), "clk".to_string());
        let net_d = LirNet::new_primary_input(NetId(1), "d".to_string());
        let net_rst = LirNet::new_primary_input(NetId(2), "rst".to_string());
        let mut net_q = LirNet::new_primary_output(NetId(3), "q".to_string(), (PrimitiveId(0), 0));
        net_q.is_state_output = true;

        netlist.add_net(net_clk);
        netlist.add_net(net_d);
        netlist.add_net(net_rst);
        netlist.add_net(net_q);

        netlist.inputs = vec![NetId(0), NetId(1), NetId(2)];
        netlist.outputs = vec![NetId(3)];
        netlist.clocks = vec![NetId(0)];
        netlist.resets = vec![NetId(2)];

        // Create DFF
        let dff = Primitive::new_seq(
            PrimitiveId(0),
            PrimitiveType::DffP,
            "top.dff_0".to_string(),
            vec![NetId(1)], // d input
            vec![NetId(3)], // q output
            NetId(0),       // clock
            Some(NetId(2)), // reset
        );

        netlist.add_primitive(dff);

        let result = convert_lir_to_sir(&netlist);

        assert_eq!(result.stats.comb_blocks, 0);
        assert_eq!(result.stats.seq_blocks, 1);
        assert_eq!(result.stats.seq_primitives, 1);

        // Check sequential block
        let seq_block = &result.sir.top_module.seq_blocks[0];
        assert_eq!(seq_block.operations.len(), 1);
        assert!(seq_block.reset.is_some());
        assert_eq!(seq_block.registers.len(), 1);

        match &seq_block.operations[0] {
            SirOperation::Primitive { ptype, .. } => {
                assert!(matches!(ptype, PrimitiveType::DffP));
            }
            _ => panic!("Expected Primitive operation"),
        }
    }
}
