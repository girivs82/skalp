//! MIR to GateNetlist Transformation
//!
//! Converts Mid-level IR (MIR) to technology-independent gate-level netlist
//! (`GateNetlist`) for gate-level simulation and fault injection.
//!
//! # Design
//!
//! This transformation produces a flat gate-level representation with:
//! - Primitives for combinational logic (AND, OR, XOR, MUX, etc.)
//! - Primitives for sequential logic (DFF, Latch)
//! - Primitives for arithmetic (adders, comparators)
//! - Full hierarchy traceability via `Primitive.path`
//! - FIT estimation for ISO 26262 safety analysis
//!
//! # Multi-bit Signal Handling
//!
//! Multi-bit signals are decomposed into individual bit primitives:
//! - `signal[7:0]` becomes 8 individual nets
//! - Operations on multi-bit signals become parallel primitive arrays
//! - This enables precise per-bit fault injection

use crate::lir::{
    GateNet, GateNetlist, HierarchyNode, NetId, NetlistStats, Primitive, PrimitiveId, PrimitiveType,
};
use skalp_mir::mir::{
    AssignmentKind, BinaryOp, Block, ContinuousAssign, DataType, EdgeType, Expression,
    ExpressionKind, LValue, Module, PortDirection, PortId, Process, ProcessKind, SensitivityList,
    SignalId, Statement, UnaryOp, Value,
};
use std::collections::HashMap;

/// Result of MIR to GateNetlist transformation
#[derive(Debug)]
pub struct MirToGateNetlistResult {
    /// The generated gate netlist
    pub netlist: GateNetlist,
    /// Transformation statistics
    pub stats: TransformStats,
    /// Warnings generated during transformation
    pub warnings: Vec<String>,
}

/// Statistics from transformation
#[derive(Debug, Default)]
pub struct TransformStats {
    /// Number of ports processed
    pub ports: usize,
    /// Number of signals processed
    pub signals: usize,
    /// Number of processes transformed
    pub processes: usize,
    /// Number of continuous assignments transformed
    pub assignments: usize,
    /// Number of primitives created
    pub primitives: usize,
    /// Number of nets created
    pub nets: usize,
    /// Total bits (for multi-bit signals)
    pub total_bits: usize,
}

/// MIR to GateNetlist transformer
pub struct MirToGateNetlistTransform {
    /// Output netlist being built
    netlist: GateNetlist,
    /// Next primitive ID
    next_prim_id: u32,
    /// Next net ID
    next_net_id: u32,
    /// Mapping from (LValue type, id, bit_index) to NetId
    /// Key: (discriminant, id, bit_index)
    lvalue_to_net: HashMap<(u8, u32, u32), NetId>,
    /// Width of each port/signal for bit decomposition
    port_widths: HashMap<PortId, usize>,
    signal_widths: HashMap<SignalId, usize>,
    /// Current hierarchy path (for primitive naming)
    hierarchy_path: String,
    /// Statistics
    stats: TransformStats,
    /// Warnings
    warnings: Vec<String>,
    /// Clock net IDs (for sequential primitives)
    clock_nets: Vec<NetId>,
    /// Reset net IDs
    reset_nets: Vec<NetId>,
}

impl MirToGateNetlistTransform {
    /// Create a new transformer
    pub fn new(module_name: &str) -> Self {
        Self {
            netlist: GateNetlist::new(module_name.to_string()),
            next_prim_id: 0,
            next_net_id: 0,
            lvalue_to_net: HashMap::new(),
            port_widths: HashMap::new(),
            signal_widths: HashMap::new(),
            hierarchy_path: "top".to_string(),
            stats: TransformStats::default(),
            warnings: Vec::new(),
            clock_nets: Vec::new(),
            reset_nets: Vec::new(),
        }
    }

    /// Transform a MIR module to GateNetlist
    pub fn transform(&mut self, module: &Module) -> MirToGateNetlistResult {
        self.hierarchy_path = module.name.clone();

        // Add hierarchy root node
        self.netlist.hierarchy.push(HierarchyNode {
            path: self.hierarchy_path.clone(),
            module: module.name.clone(),
            primitive_range: (0, 0), // Updated at end
            parent: None,
            children: Vec::new(),
        });

        // Phase 1: Create nets for all ports
        for port in &module.ports {
            self.create_port_nets(port, module);
            self.stats.ports += 1;
        }

        // Phase 2: Create nets for all internal signals
        for signal in &module.signals {
            self.create_signal_nets(signal);
            self.stats.signals += 1;
        }

        // Phase 3: Transform continuous assignments
        for assign in &module.assignments {
            self.transform_continuous_assign(assign);
            self.stats.assignments += 1;
        }

        // Phase 4: Transform processes
        for process in &module.processes {
            self.transform_process(process);
            self.stats.processes += 1;
        }

        // Update hierarchy node with final primitive range
        if let Some(node) = self.netlist.hierarchy.first_mut() {
            node.primitive_range = (0, self.next_prim_id);
        }

        // Update statistics
        self.stats.primitives = self.netlist.primitives.len();
        self.stats.nets = self.netlist.nets.len();
        self.netlist.update_stats();

        MirToGateNetlistResult {
            netlist: self.netlist.clone(),
            stats: std::mem::take(&mut self.stats),
            warnings: std::mem::take(&mut self.warnings),
        }
    }

    /// Create nets for a port (one per bit)
    fn create_port_nets(&mut self, port: &skalp_mir::mir::Port, module: &Module) {
        let width = self.get_type_width(&port.port_type);
        self.port_widths.insert(port.id, width);

        for bit in 0..width {
            let net_name = if width == 1 {
                port.name.clone()
            } else {
                format!("{}[{}]", port.name, bit)
            };

            let net_id = self.alloc_net_id();
            let net = match port.direction {
                PortDirection::Input => {
                    self.netlist.inputs.push(net_id);
                    // Check if this is a clock
                    if matches!(port.port_type, DataType::Clock { .. }) {
                        self.clock_nets.push(net_id);
                    }
                    // Check if this is a reset
                    if matches!(port.port_type, DataType::Reset { .. }) {
                        self.reset_nets.push(net_id);
                    }
                    GateNet::new_primary_input(net_id, net_name)
                }
                PortDirection::Output => {
                    self.netlist.outputs.push(net_id);
                    // Output nets need a driver - will be set when we process assignments
                    let mut net = GateNet::new(net_id, net_name);
                    net.is_primary_output = true;
                    net
                }
                PortDirection::InOut => {
                    self.netlist.inputs.push(net_id);
                    self.netlist.outputs.push(net_id);
                    let mut net = GateNet::new(net_id, net_name);
                    net.is_primary_input = true;
                    net.is_primary_output = true;
                    net
                }
            };

            self.netlist.add_net(net);
            self.lvalue_to_net.insert((0, port.id.0, bit as u32), net_id);
        }

        self.stats.total_bits += width;
    }

    /// Create nets for an internal signal (one per bit)
    fn create_signal_nets(&mut self, signal: &skalp_mir::mir::Signal) {
        let width = self.get_type_width(&signal.signal_type);
        self.signal_widths.insert(signal.id, width);

        for bit in 0..width {
            let net_name = if width == 1 {
                signal.name.clone()
            } else {
                format!("{}[{}]", signal.name, bit)
            };

            let net_id = self.alloc_net_id();
            let mut net = GateNet::new(net_id, net_name);

            // If signal has initial value, it's a register output
            if signal.initial.is_some() {
                net.is_state_output = true;
            }

            self.netlist.add_net(net);
            self.lvalue_to_net.insert((1, signal.id.0, bit as u32), net_id);
        }

        self.stats.total_bits += width;
    }

    /// Transform a continuous assignment to primitives
    fn transform_continuous_assign(&mut self, assign: &ContinuousAssign) {
        // Get target nets
        let target_nets = self.get_lvalue_nets(&assign.lhs);

        // Decompose RHS expression and get output nets
        let expr_nets = self.decompose_expression(&assign.rhs);

        // Connect expression outputs to targets via buffers (or direct if same width)
        for (i, &target_net) in target_nets.iter().enumerate() {
            if let Some(&expr_net) = expr_nets.get(i) {
                if expr_net != target_net {
                    // Create buffer to connect
                    let prim = Primitive::new_comb(
                        self.alloc_prim_id(),
                        PrimitiveType::Buf,
                        format!("{}.assign_{}", self.hierarchy_path, target_net.0),
                        vec![expr_net],
                        vec![target_net],
                    );
                    self.netlist.add_primitive(prim);
                }
            }
        }
    }

    /// Transform a process to primitives
    fn transform_process(&mut self, process: &Process) {
        match process.kind {
            ProcessKind::Sequential => {
                // Extract clock from sensitivity list
                let clock_net = self.get_clock_from_sensitivity(&process.sensitivity);
                let reset_net = self.get_reset_from_sensitivity(&process.sensitivity);

                // Transform sequential statements (create flip-flops)
                self.transform_sequential_block(&process.body, clock_net, reset_net);
            }
            ProcessKind::Combinational | ProcessKind::General => {
                // Transform combinational statements
                self.transform_combinational_block(&process.body);
            }
        }
    }

    /// Transform a sequential block (creates flip-flops)
    fn transform_sequential_block(
        &mut self,
        block: &Block,
        clock_net: Option<NetId>,
        reset_net: Option<NetId>,
    ) {
        for stmt in &block.statements {
            self.transform_sequential_statement(stmt, clock_net, reset_net);
        }
    }

    /// Transform a sequential statement
    fn transform_sequential_statement(
        &mut self,
        stmt: &Statement,
        clock_net: Option<NetId>,
        reset_net: Option<NetId>,
    ) {
        match stmt {
            Statement::Assignment(assign) => {
                if matches!(assign.kind, AssignmentKind::NonBlocking) {
                    // Non-blocking assignment -> create DFF
                    let target_nets = self.get_lvalue_nets(&assign.lhs);
                    let expr_nets = self.decompose_expression(&assign.rhs);

                    for (i, &target_net) in target_nets.iter().enumerate() {
                        if let Some(&d_net) = expr_nets.get(i) {
                            let mut prim = Primitive::new_comb(
                                self.alloc_prim_id(),
                                PrimitiveType::DffP,
                                format!("{}.dff_{}", self.hierarchy_path, target_net.0),
                                vec![d_net],
                                vec![target_net],
                            );

                            // Set clock and reset
                            if let Some(clk) = clock_net {
                                prim.clock = Some(clk);
                                prim.inputs.insert(0, clk); // DFF: clk, d
                            }
                            if let Some(rst) = reset_net {
                                prim.reset = Some(rst);
                            }

                            self.netlist.add_primitive(prim);

                            // Mark target net as state output
                            if let Some(net) = self.netlist.get_net_mut(target_net) {
                                net.is_state_output = true;
                            }
                        }
                    }
                } else {
                    // Blocking assignment in sequential - treat as combinational
                    self.transform_combinational_statement(stmt);
                }
            }
            Statement::If(if_stmt) => {
                // Transform branches recursively
                self.transform_sequential_block(&if_stmt.then_block, clock_net, reset_net);
                if let Some(ref else_block) = if_stmt.else_block {
                    self.transform_sequential_block(else_block, clock_net, reset_net);
                }
            }
            Statement::Block(block) => {
                self.transform_sequential_block(block, clock_net, reset_net);
            }
            _ => {
                // Other statements - treat as combinational
                self.transform_combinational_statement(stmt);
            }
        }
    }

    /// Transform a combinational block
    fn transform_combinational_block(&mut self, block: &Block) {
        for stmt in &block.statements {
            self.transform_combinational_statement(stmt);
        }
    }

    /// Transform a combinational statement
    fn transform_combinational_statement(&mut self, stmt: &Statement) {
        match stmt {
            Statement::Assignment(assign) => {
                let target_nets = self.get_lvalue_nets(&assign.lhs);
                let expr_nets = self.decompose_expression(&assign.rhs);

                for (i, &target_net) in target_nets.iter().enumerate() {
                    if let Some(&expr_net) = expr_nets.get(i) {
                        if expr_net != target_net {
                            let prim = Primitive::new_comb(
                                self.alloc_prim_id(),
                                PrimitiveType::Buf,
                                format!("{}.wire_{}", self.hierarchy_path, target_net.0),
                                vec![expr_net],
                                vec![target_net],
                            );
                            self.netlist.add_primitive(prim);
                        }
                    }
                }
            }
            Statement::If(if_stmt) => {
                // For combinational if, we'd need to create muxes
                // Simplified: just transform both branches
                self.transform_combinational_block(&if_stmt.then_block);
                if let Some(ref else_block) = if_stmt.else_block {
                    self.transform_combinational_block(else_block);
                }
            }
            Statement::Block(block) => {
                self.transform_combinational_block(block);
            }
            _ => {
                // Other statements not yet implemented
            }
        }
    }

    /// Decompose an expression into primitives and return output net IDs
    fn decompose_expression(&mut self, expr: &Expression) -> Vec<NetId> {
        match &expr.kind {
            ExpressionKind::Literal(value) => {
                self.create_constant_nets(value)
            }
            ExpressionKind::Ref(lvalue) => {
                self.get_lvalue_nets(lvalue)
            }
            ExpressionKind::Binary { op, left, right } => {
                let left_nets = self.decompose_expression(left);
                let right_nets = self.decompose_expression(right);
                self.create_binary_op_primitives(*op, &left_nets, &right_nets)
            }
            ExpressionKind::Unary { op, operand } => {
                let operand_nets = self.decompose_expression(operand);
                self.create_unary_op_primitives(*op, &operand_nets)
            }
            ExpressionKind::Conditional { cond, then_expr, else_expr } => {
                let cond_nets = self.decompose_expression(cond);
                let then_nets = self.decompose_expression(then_expr);
                let else_nets = self.decompose_expression(else_expr);
                self.create_mux_primitives(&cond_nets, &then_nets, &else_nets)
            }
            ExpressionKind::Concat(exprs) => {
                // Concatenate all expression nets
                let mut result = Vec::new();
                for e in exprs {
                    result.extend(self.decompose_expression(e));
                }
                result
            }
            _ => {
                // Unsupported expression - create placeholder net
                self.warnings.push(format!("Unsupported expression kind: {:?}", expr.kind));
                let net_id = self.alloc_net_id();
                let net = GateNet::new(net_id, format!("unsupported_{}", net_id.0));
                self.netlist.add_net(net);
                vec![net_id]
            }
        }
    }

    /// Create constant driver primitives
    fn create_constant_nets(&mut self, value: &Value) -> Vec<NetId> {
        match value {
            Value::Integer(i) => {
                // Determine width from value
                let width = if *i == 0 { 1 } else { (64 - i.leading_zeros()) as usize };
                let mut nets = Vec::new();

                for bit in 0..width {
                    let bit_val = ((*i as u64) >> bit) & 1 != 0;
                    let net_id = self.alloc_net_id();
                    let net = GateNet::new(net_id, format!("const_{}_{}", i, bit));
                    self.netlist.add_net(net);

                    let prim = Primitive::new_comb(
                        self.alloc_prim_id(),
                        PrimitiveType::Constant { value: bit_val },
                        format!("{}.const_{}_{}", self.hierarchy_path, i, bit),
                        vec![],
                        vec![net_id],
                    );
                    self.netlist.add_primitive(prim);
                    nets.push(net_id);
                }
                if nets.is_empty() {
                    // Zero constant - create one bit
                    let net_id = self.alloc_net_id();
                    let net = GateNet::new(net_id, "const_0".to_string());
                    self.netlist.add_net(net);

                    let prim = Primitive::new_comb(
                        self.alloc_prim_id(),
                        PrimitiveType::Constant { value: false },
                        format!("{}.const_0", self.hierarchy_path),
                        vec![],
                        vec![net_id],
                    );
                    self.netlist.add_primitive(prim);
                    nets.push(net_id);
                }
                nets
            }
            Value::BitVector { width, value: v } => {
                let mut nets = Vec::new();
                for bit in 0..*width {
                    let bit_val = (v >> bit) & 1 != 0;
                    let net_id = self.alloc_net_id();
                    let net = GateNet::new(net_id, format!("const_bv_{}_{}", v, bit));
                    self.netlist.add_net(net);

                    let prim = Primitive::new_comb(
                        self.alloc_prim_id(),
                        PrimitiveType::Constant { value: bit_val },
                        format!("{}.const_bv_{}_{}", self.hierarchy_path, v, bit),
                        vec![],
                        vec![net_id],
                    );
                    self.netlist.add_primitive(prim);
                    nets.push(net_id);
                }
                nets
            }
            _ => {
                // Other value types - create single unknown net
                let net_id = self.alloc_net_id();
                let net = GateNet::new(net_id, "const_unknown".to_string());
                self.netlist.add_net(net);

                let prim = Primitive::new_comb(
                    self.alloc_prim_id(),
                    PrimitiveType::Constant { value: false },
                    format!("{}.const_unknown", self.hierarchy_path),
                    vec![],
                    vec![net_id],
                );
                self.netlist.add_primitive(prim);
                vec![net_id]
            }
        }
    }

    /// Create primitives for binary operations
    fn create_binary_op_primitives(
        &mut self,
        op: BinaryOp,
        left: &[NetId],
        right: &[NetId],
    ) -> Vec<NetId> {
        let width = left.len().max(right.len());
        let mut result_nets = Vec::new();

        match op {
            // Bitwise operations - one primitive per bit
            BinaryOp::And | BinaryOp::BitwiseAnd | BinaryOp::LogicalAnd => {
                for i in 0..width {
                    let l = left.get(i).copied().unwrap_or(left[0]);
                    let r = right.get(i).copied().unwrap_or(right[0]);
                    let out = self.alloc_net_id();
                    self.netlist.add_net(GateNet::new(out, format!("and_out_{}", out.0)));

                    let prim = Primitive::new_comb(
                        self.alloc_prim_id(),
                        PrimitiveType::And { inputs: 2 },
                        format!("{}.and_{}", self.hierarchy_path, out.0),
                        vec![l, r],
                        vec![out],
                    );
                    self.netlist.add_primitive(prim);
                    result_nets.push(out);
                }
            }
            BinaryOp::Or | BinaryOp::BitwiseOr | BinaryOp::LogicalOr => {
                for i in 0..width {
                    let l = left.get(i).copied().unwrap_or(left[0]);
                    let r = right.get(i).copied().unwrap_or(right[0]);
                    let out = self.alloc_net_id();
                    self.netlist.add_net(GateNet::new(out, format!("or_out_{}", out.0)));

                    let prim = Primitive::new_comb(
                        self.alloc_prim_id(),
                        PrimitiveType::Or { inputs: 2 },
                        format!("{}.or_{}", self.hierarchy_path, out.0),
                        vec![l, r],
                        vec![out],
                    );
                    self.netlist.add_primitive(prim);
                    result_nets.push(out);
                }
            }
            BinaryOp::Xor | BinaryOp::BitwiseXor => {
                for i in 0..width {
                    let l = left.get(i).copied().unwrap_or(left[0]);
                    let r = right.get(i).copied().unwrap_or(right[0]);
                    let out = self.alloc_net_id();
                    self.netlist.add_net(GateNet::new(out, format!("xor_out_{}", out.0)));

                    let prim = Primitive::new_comb(
                        self.alloc_prim_id(),
                        PrimitiveType::Xor,
                        format!("{}.xor_{}", self.hierarchy_path, out.0),
                        vec![l, r],
                        vec![out],
                    );
                    self.netlist.add_primitive(prim);
                    result_nets.push(out);
                }
            }
            // Arithmetic - use adder chain
            BinaryOp::Add => {
                let mut carry = None;
                for i in 0..width {
                    let l = left.get(i).copied().unwrap_or(left[0]);
                    let r = right.get(i).copied().unwrap_or(right[0]);
                    let sum = self.alloc_net_id();
                    let cout = self.alloc_net_id();
                    self.netlist.add_net(GateNet::new(sum, format!("add_sum_{}", sum.0)));
                    self.netlist.add_net(GateNet::new(cout, format!("add_cout_{}", cout.0)));

                    let (ptype, inputs) = if let Some(cin) = carry {
                        (PrimitiveType::FullAdder, vec![l, r, cin])
                    } else {
                        (PrimitiveType::HalfAdder, vec![l, r])
                    };

                    let prim = Primitive::new_comb(
                        self.alloc_prim_id(),
                        ptype,
                        format!("{}.add_{}", self.hierarchy_path, i),
                        inputs,
                        vec![sum, cout],
                    );
                    self.netlist.add_primitive(prim);
                    result_nets.push(sum);
                    carry = Some(cout);
                }
            }
            // Comparison - single bit output
            BinaryOp::Equal | BinaryOp::NotEqual => {
                // XNOR all bits, then AND them together
                let mut xnor_outs = Vec::new();
                for i in 0..width {
                    let l = left.get(i).copied().unwrap_or(left[0]);
                    let r = right.get(i).copied().unwrap_or(right[0]);
                    let out = self.alloc_net_id();
                    self.netlist.add_net(GateNet::new(out, format!("eq_xnor_{}", out.0)));

                    let prim = Primitive::new_comb(
                        self.alloc_prim_id(),
                        PrimitiveType::Xnor,
                        format!("{}.eq_xnor_{}", self.hierarchy_path, i),
                        vec![l, r],
                        vec![out],
                    );
                    self.netlist.add_primitive(prim);
                    xnor_outs.push(out);
                }

                // AND all XNOR outputs
                let eq_out = self.alloc_net_id();
                self.netlist.add_net(GateNet::new(eq_out, format!("eq_out_{}", eq_out.0)));

                let prim = Primitive::new_comb(
                    self.alloc_prim_id(),
                    PrimitiveType::And { inputs: xnor_outs.len() as u8 },
                    format!("{}.eq_and", self.hierarchy_path),
                    xnor_outs,
                    vec![eq_out],
                );
                self.netlist.add_primitive(prim);

                if matches!(op, BinaryOp::NotEqual) {
                    // Invert for NotEqual
                    let neq_out = self.alloc_net_id();
                    self.netlist.add_net(GateNet::new(neq_out, format!("neq_out_{}", neq_out.0)));

                    let prim = Primitive::new_comb(
                        self.alloc_prim_id(),
                        PrimitiveType::Inv,
                        format!("{}.neq_inv", self.hierarchy_path),
                        vec![eq_out],
                        vec![neq_out],
                    );
                    self.netlist.add_primitive(prim);
                    result_nets.push(neq_out);
                } else {
                    result_nets.push(eq_out);
                }
            }
            _ => {
                // Other ops - create placeholder
                self.warnings.push(format!("Unsupported binary op: {:?}", op));
                let out = self.alloc_net_id();
                self.netlist.add_net(GateNet::new(out, format!("unsup_binop_{}", out.0)));
                result_nets.push(out);
            }
        }

        result_nets
    }

    /// Create primitives for unary operations
    fn create_unary_op_primitives(&mut self, op: UnaryOp, operand: &[NetId]) -> Vec<NetId> {
        let mut result_nets = Vec::new();

        match op {
            UnaryOp::Not | UnaryOp::BitwiseNot => {
                for &net in operand {
                    let out = self.alloc_net_id();
                    self.netlist.add_net(GateNet::new(out, format!("not_out_{}", out.0)));

                    let prim = Primitive::new_comb(
                        self.alloc_prim_id(),
                        PrimitiveType::Inv,
                        format!("{}.inv_{}", self.hierarchy_path, out.0),
                        vec![net],
                        vec![out],
                    );
                    self.netlist.add_primitive(prim);
                    result_nets.push(out);
                }
            }
            UnaryOp::Negate => {
                // Two's complement: invert and add 1
                let inverted = self.create_unary_op_primitives(UnaryOp::BitwiseNot, operand);
                let one = self.create_constant_nets(&Value::Integer(1));
                result_nets = self.create_binary_op_primitives(BinaryOp::Add, &inverted, &one);
            }
            _ => {
                // Other unary ops - pass through
                result_nets.extend_from_slice(operand);
            }
        }

        result_nets
    }

    /// Create MUX primitives for conditional expression
    fn create_mux_primitives(
        &mut self,
        sel: &[NetId],
        then_nets: &[NetId],
        else_nets: &[NetId],
    ) -> Vec<NetId> {
        let sel_net = sel.first().copied().unwrap_or(NetId(0));
        let width = then_nets.len().max(else_nets.len());
        let mut result_nets = Vec::new();

        for i in 0..width {
            let d0 = else_nets.get(i).copied().unwrap_or(else_nets[0]);
            let d1 = then_nets.get(i).copied().unwrap_or(then_nets[0]);
            let out = self.alloc_net_id();
            self.netlist.add_net(GateNet::new(out, format!("mux_out_{}", out.0)));

            let prim = Primitive::new_comb(
                self.alloc_prim_id(),
                PrimitiveType::Mux2,
                format!("{}.mux_{}", self.hierarchy_path, out.0),
                vec![sel_net, d0, d1],
                vec![out],
            );
            self.netlist.add_primitive(prim);
            result_nets.push(out);
        }

        result_nets
    }

    /// Get net IDs for an LValue
    fn get_lvalue_nets(&mut self, lvalue: &LValue) -> Vec<NetId> {
        match lvalue {
            LValue::Port(port_id) => {
                let width = self.port_widths.get(port_id).copied().unwrap_or(1);
                (0..width)
                    .filter_map(|bit| self.lvalue_to_net.get(&(0, port_id.0, bit as u32)).copied())
                    .collect()
            }
            LValue::Signal(signal_id) => {
                let width = self.signal_widths.get(signal_id).copied().unwrap_or(1);
                (0..width)
                    .filter_map(|bit| self.lvalue_to_net.get(&(1, signal_id.0, bit as u32)).copied())
                    .collect()
            }
            LValue::Variable(var_id) => {
                // Variables - create temporary nets
                let net_id = self.alloc_net_id();
                let net = GateNet::new(net_id, format!("var_{}", var_id.0));
                self.netlist.add_net(net);
                vec![net_id]
            }
            LValue::BitSelect { base, index } => {
                // Get the specific bit
                let base_nets = self.get_lvalue_nets(base);
                if let ExpressionKind::Literal(Value::Integer(i)) = &index.kind {
                    if let Some(&net) = base_nets.get(*i as usize) {
                        return vec![net];
                    }
                }
                // Dynamic index - would need mux tree, return first for now
                base_nets.into_iter().take(1).collect()
            }
            LValue::RangeSelect { base, high, low } => {
                let base_nets = self.get_lvalue_nets(base);
                if let (
                    ExpressionKind::Literal(Value::Integer(h)),
                    ExpressionKind::Literal(Value::Integer(l)),
                ) = (&high.kind, &low.kind)
                {
                    let lo = *l as usize;
                    let hi = *h as usize;
                    return base_nets.into_iter().skip(lo).take(hi - lo + 1).collect();
                }
                base_nets
            }
            LValue::Concat(parts) => {
                let mut result = Vec::new();
                for part in parts {
                    result.extend(self.get_lvalue_nets(part));
                }
                result
            }
        }
    }

    /// Get clock net from sensitivity list
    fn get_clock_from_sensitivity(&self, sens: &SensitivityList) -> Option<NetId> {
        if let SensitivityList::Edge(edges) = sens {
            for edge in edges {
                if matches!(edge.edge, EdgeType::Rising | EdgeType::Falling) {
                    let nets = self.get_lvalue_nets_readonly(&edge.signal);
                    return nets.into_iter().next();
                }
            }
        }
        self.clock_nets.first().copied()
    }

    /// Get reset net from sensitivity list
    fn get_reset_from_sensitivity(&self, sens: &SensitivityList) -> Option<NetId> {
        if let SensitivityList::Edge(edges) = sens {
            for edge in edges {
                if matches!(edge.edge, EdgeType::Active | EdgeType::Inactive) {
                    let nets = self.get_lvalue_nets_readonly(&edge.signal);
                    return nets.into_iter().next();
                }
            }
        }
        self.reset_nets.first().copied()
    }

    /// Get LValue nets without modifying state (for sensitivity list parsing)
    fn get_lvalue_nets_readonly(&self, lvalue: &LValue) -> Vec<NetId> {
        match lvalue {
            LValue::Port(port_id) => {
                let width = self.port_widths.get(port_id).copied().unwrap_or(1);
                (0..width)
                    .filter_map(|bit| self.lvalue_to_net.get(&(0, port_id.0, bit as u32)).copied())
                    .collect()
            }
            LValue::Signal(signal_id) => {
                let width = self.signal_widths.get(signal_id).copied().unwrap_or(1);
                (0..width)
                    .filter_map(|bit| self.lvalue_to_net.get(&(1, signal_id.0, bit as u32)).copied())
                    .collect()
            }
            _ => Vec::new(),
        }
    }

    /// Get width of a data type
    fn get_type_width(&self, dtype: &DataType) -> usize {
        match dtype {
            DataType::Bit(w) | DataType::Logic(w) | DataType::Int(w) | DataType::Nat(w) => *w,
            DataType::Bool => 1,
            DataType::Clock { .. } => 1,
            DataType::Reset { .. } => 1,
            DataType::Float16 => 16,
            DataType::Float32 => 32,
            DataType::Float64 => 64,
            DataType::Array(inner, size) => self.get_type_width(inner) * size,
            DataType::BitParam { default, .. }
            | DataType::LogicParam { default, .. }
            | DataType::IntParam { default, .. }
            | DataType::NatParam { default, .. } => *default,
            DataType::BitExpr { default, .. }
            | DataType::LogicExpr { default, .. }
            | DataType::IntExpr { default, .. }
            | DataType::NatExpr { default, .. } => *default,
            DataType::Vec2(inner) => self.get_type_width(inner) * 2,
            DataType::Vec3(inner) => self.get_type_width(inner) * 3,
            DataType::Vec4(inner) => self.get_type_width(inner) * 4,
            _ => 1, // Default for unsupported types
        }
    }

    /// Allocate a new primitive ID
    fn alloc_prim_id(&mut self) -> PrimitiveId {
        let id = PrimitiveId(self.next_prim_id);
        self.next_prim_id += 1;
        id
    }

    /// Allocate a new net ID
    fn alloc_net_id(&mut self) -> NetId {
        let id = NetId(self.next_net_id);
        self.next_net_id += 1;
        id
    }
}

/// Transform a MIR module to GateNetlist
pub fn transform_mir_to_gate_netlist(module: &Module) -> MirToGateNetlistResult {
    let mut transformer = MirToGateNetlistTransform::new(&module.name);
    transformer.transform(module)
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use skalp_mir::mir::{Module, ModuleId, Port, PortId, Signal, SignalId};

    fn make_simple_module() -> Module {
        Module {
            id: ModuleId(0),
            name: "test_module".to_string(),
            parameters: Vec::new(),
            ports: vec![
                Port {
                    id: PortId(0),
                    name: "a".to_string(),
                    direction: PortDirection::Input,
                    port_type: DataType::Bit(1),
                    physical_constraints: None,
                    span: None,
                },
                Port {
                    id: PortId(1),
                    name: "b".to_string(),
                    direction: PortDirection::Input,
                    port_type: DataType::Bit(1),
                    physical_constraints: None,
                    span: None,
                },
                Port {
                    id: PortId(2),
                    name: "y".to_string(),
                    direction: PortDirection::Output,
                    port_type: DataType::Bit(1),
                    physical_constraints: None,
                    span: None,
                },
            ],
            signals: Vec::new(),
            variables: Vec::new(),
            processes: Vec::new(),
            assignments: vec![ContinuousAssign {
                lhs: LValue::Port(PortId(2)),
                rhs: Expression::with_unknown_type(ExpressionKind::Binary {
                    op: BinaryOp::And,
                    left: Box::new(Expression::with_unknown_type(ExpressionKind::Ref(
                        LValue::Port(PortId(0)),
                    ))),
                    right: Box::new(Expression::with_unknown_type(ExpressionKind::Ref(
                        LValue::Port(PortId(1)),
                    ))),
                }),
                span: None,
            }],
            instances: Vec::new(),
            clock_domains: Vec::new(),
            generate_blocks: Vec::new(),
            assertions: Vec::new(),
            span: None,
            pipeline_config: None,
            vendor_ip_config: None,
            power_domains: Vec::new(),
        }
    }

    #[test]
    fn test_transform_empty_module() {
        let module = Module {
            id: ModuleId(0),
            name: "empty".to_string(),
            parameters: Vec::new(),
            ports: Vec::new(),
            signals: Vec::new(),
            variables: Vec::new(),
            processes: Vec::new(),
            assignments: Vec::new(),
            instances: Vec::new(),
            clock_domains: Vec::new(),
            generate_blocks: Vec::new(),
            assertions: Vec::new(),
            span: None,
            pipeline_config: None,
            vendor_ip_config: None,
            power_domains: Vec::new(),
        };

        let result = transform_mir_to_gate_netlist(&module);
        assert_eq!(result.netlist.name, "empty");
        assert_eq!(result.stats.ports, 0);
        assert_eq!(result.stats.signals, 0);
    }

    #[test]
    fn test_transform_simple_and_gate() {
        let module = make_simple_module();
        let result = transform_mir_to_gate_netlist(&module);

        assert_eq!(result.netlist.name, "test_module");
        assert_eq!(result.stats.ports, 3);
        assert_eq!(result.netlist.inputs.len(), 2);
        assert_eq!(result.netlist.outputs.len(), 1);

        // Should have at least one AND primitive
        let and_count = result
            .netlist
            .primitives
            .iter()
            .filter(|p| matches!(p.ptype, PrimitiveType::And { .. }))
            .count();
        assert!(and_count >= 1);
    }

    #[test]
    fn test_transform_multi_bit_port() {
        let module = Module {
            id: ModuleId(0),
            name: "multi_bit".to_string(),
            parameters: Vec::new(),
            ports: vec![
                Port {
                    id: PortId(0),
                    name: "data".to_string(),
                    direction: PortDirection::Input,
                    port_type: DataType::Bit(8),
                    physical_constraints: None,
                    span: None,
                },
            ],
            signals: Vec::new(),
            variables: Vec::new(),
            processes: Vec::new(),
            assignments: Vec::new(),
            instances: Vec::new(),
            clock_domains: Vec::new(),
            generate_blocks: Vec::new(),
            assertions: Vec::new(),
            span: None,
            pipeline_config: None,
            vendor_ip_config: None,
            power_domains: Vec::new(),
        };

        let result = transform_mir_to_gate_netlist(&module);

        // 8-bit port should create 8 nets
        assert_eq!(result.netlist.inputs.len(), 8);
        assert_eq!(result.stats.total_bits, 8);
    }

    #[test]
    fn test_constant_folding() {
        let module = Module {
            id: ModuleId(0),
            name: "const_test".to_string(),
            parameters: Vec::new(),
            ports: vec![Port {
                id: PortId(0),
                name: "out".to_string(),
                direction: PortDirection::Output,
                port_type: DataType::Bit(1),
                physical_constraints: None,
                span: None,
            }],
            signals: Vec::new(),
            variables: Vec::new(),
            processes: Vec::new(),
            assignments: vec![ContinuousAssign {
                lhs: LValue::Port(PortId(0)),
                rhs: Expression::with_unknown_type(ExpressionKind::Literal(Value::Integer(1))),
                span: None,
            }],
            instances: Vec::new(),
            clock_domains: Vec::new(),
            generate_blocks: Vec::new(),
            assertions: Vec::new(),
            span: None,
            pipeline_config: None,
            vendor_ip_config: None,
            power_domains: Vec::new(),
        };

        let result = transform_mir_to_gate_netlist(&module);

        // Should have a constant primitive
        let const_count = result
            .netlist
            .primitives
            .iter()
            .filter(|p| matches!(p.ptype, PrimitiveType::Constant { .. }))
            .count();
        assert!(const_count >= 1);
    }

    #[test]
    fn test_hierarchy_tracking() {
        let module = make_simple_module();
        let result = transform_mir_to_gate_netlist(&module);

        assert!(!result.netlist.hierarchy.is_empty());
        assert_eq!(result.netlist.hierarchy[0].module, "test_module");
    }

    #[test]
    fn test_transform_xor_gate() {
        let module = Module {
            id: ModuleId(0),
            name: "xor_module".to_string(),
            parameters: Vec::new(),
            ports: vec![
                Port {
                    id: PortId(0),
                    name: "a".to_string(),
                    direction: PortDirection::Input,
                    port_type: DataType::Bit(1),
                    physical_constraints: None,
                    span: None,
                },
                Port {
                    id: PortId(1),
                    name: "b".to_string(),
                    direction: PortDirection::Input,
                    port_type: DataType::Bit(1),
                    physical_constraints: None,
                    span: None,
                },
                Port {
                    id: PortId(2),
                    name: "y".to_string(),
                    direction: PortDirection::Output,
                    port_type: DataType::Bit(1),
                    physical_constraints: None,
                    span: None,
                },
            ],
            signals: Vec::new(),
            variables: Vec::new(),
            processes: Vec::new(),
            assignments: vec![ContinuousAssign {
                lhs: LValue::Port(PortId(2)),
                rhs: Expression::with_unknown_type(ExpressionKind::Binary {
                    op: BinaryOp::Xor,
                    left: Box::new(Expression::with_unknown_type(ExpressionKind::Ref(
                        LValue::Port(PortId(0)),
                    ))),
                    right: Box::new(Expression::with_unknown_type(ExpressionKind::Ref(
                        LValue::Port(PortId(1)),
                    ))),
                }),
                span: None,
            }],
            instances: Vec::new(),
            clock_domains: Vec::new(),
            generate_blocks: Vec::new(),
            assertions: Vec::new(),
            span: None,
            pipeline_config: None,
            vendor_ip_config: None,
            power_domains: Vec::new(),
        };

        let result = transform_mir_to_gate_netlist(&module);

        // Should have at least one XOR primitive
        let xor_count = result
            .netlist
            .primitives
            .iter()
            .filter(|p| matches!(p.ptype, PrimitiveType::Xor))
            .count();
        assert!(xor_count >= 1, "Expected XOR primitive, found none");
    }

    #[test]
    fn test_transform_or_gate() {
        let module = Module {
            id: ModuleId(0),
            name: "or_module".to_string(),
            parameters: Vec::new(),
            ports: vec![
                Port {
                    id: PortId(0),
                    name: "a".to_string(),
                    direction: PortDirection::Input,
                    port_type: DataType::Bit(1),
                    physical_constraints: None,
                    span: None,
                },
                Port {
                    id: PortId(1),
                    name: "b".to_string(),
                    direction: PortDirection::Input,
                    port_type: DataType::Bit(1),
                    physical_constraints: None,
                    span: None,
                },
                Port {
                    id: PortId(2),
                    name: "y".to_string(),
                    direction: PortDirection::Output,
                    port_type: DataType::Bit(1),
                    physical_constraints: None,
                    span: None,
                },
            ],
            signals: Vec::new(),
            variables: Vec::new(),
            processes: Vec::new(),
            assignments: vec![ContinuousAssign {
                lhs: LValue::Port(PortId(2)),
                rhs: Expression::with_unknown_type(ExpressionKind::Binary {
                    op: BinaryOp::Or,
                    left: Box::new(Expression::with_unknown_type(ExpressionKind::Ref(
                        LValue::Port(PortId(0)),
                    ))),
                    right: Box::new(Expression::with_unknown_type(ExpressionKind::Ref(
                        LValue::Port(PortId(1)),
                    ))),
                }),
                span: None,
            }],
            instances: Vec::new(),
            clock_domains: Vec::new(),
            generate_blocks: Vec::new(),
            assertions: Vec::new(),
            span: None,
            pipeline_config: None,
            vendor_ip_config: None,
            power_domains: Vec::new(),
        };

        let result = transform_mir_to_gate_netlist(&module);

        // Should have at least one OR primitive
        let or_count = result
            .netlist
            .primitives
            .iter()
            .filter(|p| matches!(p.ptype, PrimitiveType::Or { .. }))
            .count();
        assert!(or_count >= 1, "Expected OR primitive, found none");
    }

    #[test]
    fn test_transform_inverter() {
        let module = Module {
            id: ModuleId(0),
            name: "inv_module".to_string(),
            parameters: Vec::new(),
            ports: vec![
                Port {
                    id: PortId(0),
                    name: "a".to_string(),
                    direction: PortDirection::Input,
                    port_type: DataType::Bit(1),
                    physical_constraints: None,
                    span: None,
                },
                Port {
                    id: PortId(1),
                    name: "y".to_string(),
                    direction: PortDirection::Output,
                    port_type: DataType::Bit(1),
                    physical_constraints: None,
                    span: None,
                },
            ],
            signals: Vec::new(),
            variables: Vec::new(),
            processes: Vec::new(),
            assignments: vec![ContinuousAssign {
                lhs: LValue::Port(PortId(1)),
                rhs: Expression::with_unknown_type(ExpressionKind::Unary {
                    op: UnaryOp::Not,
                    operand: Box::new(Expression::with_unknown_type(ExpressionKind::Ref(
                        LValue::Port(PortId(0)),
                    ))),
                }),
                span: None,
            }],
            instances: Vec::new(),
            clock_domains: Vec::new(),
            generate_blocks: Vec::new(),
            assertions: Vec::new(),
            span: None,
            pipeline_config: None,
            vendor_ip_config: None,
            power_domains: Vec::new(),
        };

        let result = transform_mir_to_gate_netlist(&module);

        // Should have at least one INV primitive
        let inv_count = result
            .netlist
            .primitives
            .iter()
            .filter(|p| matches!(p.ptype, PrimitiveType::Inv))
            .count();
        assert!(inv_count >= 1, "Expected INV primitive, found none");
    }

    #[test]
    fn test_transform_adder() {
        let module = Module {
            id: ModuleId(0),
            name: "adder_module".to_string(),
            parameters: Vec::new(),
            ports: vec![
                Port {
                    id: PortId(0),
                    name: "a".to_string(),
                    direction: PortDirection::Input,
                    port_type: DataType::Bit(4),
                    physical_constraints: None,
                    span: None,
                },
                Port {
                    id: PortId(1),
                    name: "b".to_string(),
                    direction: PortDirection::Input,
                    port_type: DataType::Bit(4),
                    physical_constraints: None,
                    span: None,
                },
                Port {
                    id: PortId(2),
                    name: "sum".to_string(),
                    direction: PortDirection::Output,
                    port_type: DataType::Bit(4),
                    physical_constraints: None,
                    span: None,
                },
            ],
            signals: Vec::new(),
            variables: Vec::new(),
            processes: Vec::new(),
            assignments: vec![ContinuousAssign {
                lhs: LValue::Port(PortId(2)),
                rhs: Expression::with_unknown_type(ExpressionKind::Binary {
                    op: BinaryOp::Add,
                    left: Box::new(Expression::with_unknown_type(ExpressionKind::Ref(
                        LValue::Port(PortId(0)),
                    ))),
                    right: Box::new(Expression::with_unknown_type(ExpressionKind::Ref(
                        LValue::Port(PortId(1)),
                    ))),
                }),
                span: None,
            }],
            instances: Vec::new(),
            clock_domains: Vec::new(),
            generate_blocks: Vec::new(),
            assertions: Vec::new(),
            span: None,
            pipeline_config: None,
            vendor_ip_config: None,
            power_domains: Vec::new(),
        };

        let result = transform_mir_to_gate_netlist(&module);

        // Should have adder primitives (HalfAdder + FullAdders)
        let adder_count = result
            .netlist
            .primitives
            .iter()
            .filter(|p| {
                matches!(
                    p.ptype,
                    PrimitiveType::HalfAdder | PrimitiveType::FullAdder
                )
            })
            .count();
        assert!(
            adder_count >= 1,
            "Expected adder primitives, found {} primitives total",
            result.netlist.primitives.len()
        );
    }

    #[test]
    fn test_transform_mux() {
        let module = Module {
            id: ModuleId(0),
            name: "mux_module".to_string(),
            parameters: Vec::new(),
            ports: vec![
                Port {
                    id: PortId(0),
                    name: "sel".to_string(),
                    direction: PortDirection::Input,
                    port_type: DataType::Bit(1),
                    physical_constraints: None,
                    span: None,
                },
                Port {
                    id: PortId(1),
                    name: "a".to_string(),
                    direction: PortDirection::Input,
                    port_type: DataType::Bit(1),
                    physical_constraints: None,
                    span: None,
                },
                Port {
                    id: PortId(2),
                    name: "b".to_string(),
                    direction: PortDirection::Input,
                    port_type: DataType::Bit(1),
                    physical_constraints: None,
                    span: None,
                },
                Port {
                    id: PortId(3),
                    name: "y".to_string(),
                    direction: PortDirection::Output,
                    port_type: DataType::Bit(1),
                    physical_constraints: None,
                    span: None,
                },
            ],
            signals: Vec::new(),
            variables: Vec::new(),
            processes: Vec::new(),
            assignments: vec![ContinuousAssign {
                lhs: LValue::Port(PortId(3)),
                rhs: Expression::with_unknown_type(ExpressionKind::Conditional {
                    cond: Box::new(Expression::with_unknown_type(ExpressionKind::Ref(
                        LValue::Port(PortId(0)),
                    ))),
                    then_expr: Box::new(Expression::with_unknown_type(ExpressionKind::Ref(
                        LValue::Port(PortId(1)),
                    ))),
                    else_expr: Box::new(Expression::with_unknown_type(ExpressionKind::Ref(
                        LValue::Port(PortId(2)),
                    ))),
                }),
                span: None,
            }],
            instances: Vec::new(),
            clock_domains: Vec::new(),
            generate_blocks: Vec::new(),
            assertions: Vec::new(),
            span: None,
            pipeline_config: None,
            vendor_ip_config: None,
            power_domains: Vec::new(),
        };

        let result = transform_mir_to_gate_netlist(&module);

        // Should have at least one MUX2 primitive
        let mux_count = result
            .netlist
            .primitives
            .iter()
            .filter(|p| matches!(p.ptype, PrimitiveType::Mux2))
            .count();
        assert!(mux_count >= 1, "Expected MUX2 primitive, found none");
    }

    #[test]
    fn test_transform_equality() {
        let module = Module {
            id: ModuleId(0),
            name: "eq_module".to_string(),
            parameters: Vec::new(),
            ports: vec![
                Port {
                    id: PortId(0),
                    name: "a".to_string(),
                    direction: PortDirection::Input,
                    port_type: DataType::Bit(4),
                    physical_constraints: None,
                    span: None,
                },
                Port {
                    id: PortId(1),
                    name: "b".to_string(),
                    direction: PortDirection::Input,
                    port_type: DataType::Bit(4),
                    physical_constraints: None,
                    span: None,
                },
                Port {
                    id: PortId(2),
                    name: "eq".to_string(),
                    direction: PortDirection::Output,
                    port_type: DataType::Bit(1),
                    physical_constraints: None,
                    span: None,
                },
            ],
            signals: Vec::new(),
            variables: Vec::new(),
            processes: Vec::new(),
            assignments: vec![ContinuousAssign {
                lhs: LValue::Port(PortId(2)),
                rhs: Expression::with_unknown_type(ExpressionKind::Binary {
                    op: BinaryOp::Equal,
                    left: Box::new(Expression::with_unknown_type(ExpressionKind::Ref(
                        LValue::Port(PortId(0)),
                    ))),
                    right: Box::new(Expression::with_unknown_type(ExpressionKind::Ref(
                        LValue::Port(PortId(1)),
                    ))),
                }),
                span: None,
            }],
            instances: Vec::new(),
            clock_domains: Vec::new(),
            generate_blocks: Vec::new(),
            assertions: Vec::new(),
            span: None,
            pipeline_config: None,
            vendor_ip_config: None,
            power_domains: Vec::new(),
        };

        let result = transform_mir_to_gate_netlist(&module);

        // Should have XNOR primitives (for bit comparison) and AND (for combining)
        let xnor_count = result
            .netlist
            .primitives
            .iter()
            .filter(|p| matches!(p.ptype, PrimitiveType::Xnor))
            .count();
        assert!(
            xnor_count >= 1,
            "Expected XNOR primitives for equality comparison"
        );
    }

    #[test]
    fn test_internal_signal() {
        let module = Module {
            id: ModuleId(0),
            name: "signal_module".to_string(),
            parameters: Vec::new(),
            ports: vec![
                Port {
                    id: PortId(0),
                    name: "in".to_string(),
                    direction: PortDirection::Input,
                    port_type: DataType::Bit(4),
                    physical_constraints: None,
                    span: None,
                },
                Port {
                    id: PortId(1),
                    name: "out".to_string(),
                    direction: PortDirection::Output,
                    port_type: DataType::Bit(4),
                    physical_constraints: None,
                    span: None,
                },
            ],
            signals: vec![Signal {
                id: SignalId(0),
                name: "internal".to_string(),
                signal_type: DataType::Bit(4),
                initial: None,
                clock_domain: None,
                span: None,
                memory_config: None,
                trace_config: None,
                cdc_config: None,
                breakpoint_config: None,
                power_config: None,
            }],
            variables: Vec::new(),
            processes: Vec::new(),
            assignments: Vec::new(),
            instances: Vec::new(),
            clock_domains: Vec::new(),
            generate_blocks: Vec::new(),
            assertions: Vec::new(),
            span: None,
            pipeline_config: None,
            vendor_ip_config: None,
            power_domains: Vec::new(),
        };

        let result = transform_mir_to_gate_netlist(&module);

        // 4-bit input + 4-bit output + 4-bit internal = 12 total bits
        assert_eq!(result.stats.total_bits, 12);
        assert_eq!(result.stats.signals, 1);
    }

    #[test]
    fn test_fit_estimation() {
        let module = make_simple_module();
        let result = transform_mir_to_gate_netlist(&module);

        // Calculate total FIT from primitives
        let total_fit: f64 = result
            .netlist
            .primitives
            .iter()
            .map(|p| p.ptype.base_fit())
            .sum();

        // Should have some non-zero FIT (from the AND gate + buffers)
        assert!(total_fit > 0.0, "Expected positive total FIT");
    }
}
