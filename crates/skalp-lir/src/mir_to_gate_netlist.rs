//! MIR to Lir Transformation
//!
//! Converts Mid-level IR (MIR) to technology-independent gate-level netlist
//! (`Lir`) for gate-level simulation and fault injection.
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
    HierarchyNode, Lir, LirNet, LirSafetyInfo, NetId, NetlistStats, Primitive, PrimitiveId,
    PrimitiveType,
};
use skalp_mir::mir::{
    AssignmentKind, BinaryOp, Block, ContinuousAssign, DataType, EdgeType, Expression,
    ExpressionKind, LValue, Module, PortDirection, PortId, Process, ProcessKind, SafetyContext,
    SensitivityList, SignalId, Statement, UnaryOp, Value,
};
use std::collections::HashMap;

/// Result of MIR to LIR transformation
#[derive(Debug, serde::Serialize, serde::Deserialize)]
pub struct MirToLirResult {
    /// The generated LIR (gate-level netlist)
    pub lir: Lir,
    /// Transformation statistics
    pub stats: TransformStats,
    /// Warnings generated during transformation
    pub warnings: Vec<String>,
}

impl MirToLirResult {
    /// Backward-compatible access to the LIR as `netlist`
    #[deprecated(since = "0.2.0", note = "Use `lir` field instead")]
    pub fn netlist(&self) -> &Lir {
        &self.lir
    }
}

/// Statistics from transformation
#[derive(Debug, Default, serde::Serialize, serde::Deserialize)]
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

/// MIR to LIR transformer
pub struct MirToLirTransform {
    /// Output LIR being built
    lir: Lir,
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
    /// Cached constant 0 net (for padding shorter operands)
    const_zero_net: Option<NetId>,
    /// Module-level safety context (from MIR)
    /// Propagated to all primitives created in this module
    module_safety_context: Option<SafetyContext>,
}

impl MirToLirTransform {
    /// Create a new transformer
    pub fn new(module_name: &str) -> Self {
        Self {
            lir: Lir::new(module_name.to_string()),
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
            const_zero_net: None,
            module_safety_context: None,
        }
    }

    /// Convert MIR SafetyContext to LIR LirSafetyInfo
    fn safety_context_to_lir_info(ctx: &SafetyContext) -> LirSafetyInfo {
        LirSafetyInfo {
            goal_name: ctx.implementing_goal.clone(),
            mechanism_name: ctx.mechanism_name.clone(),
            is_sm_of_sm: false, // SM-of-SM detection is done at safety analysis level
            protected_sm_name: None,
        }
    }

    /// Apply module-level safety context to a primitive
    fn apply_safety_info(&self, mut prim: Primitive) -> Primitive {
        if let Some(ref ctx) = self.module_safety_context {
            if ctx.has_safety_annotation() {
                prim.safety_info = Some(Self::safety_context_to_lir_info(ctx));
            }
        }
        prim
    }

    /// Add a primitive to the LIR with safety context applied
    fn add_primitive_with_safety(&mut self, prim: Primitive) {
        let prim_with_safety = self.apply_safety_info(prim);
        self.lir.add_primitive(prim_with_safety);
    }

    /// Transform a MIR module to Lir
    pub fn transform(&mut self, module: &Module) -> MirToLirResult {
        self.hierarchy_path = module.name.clone();

        // Store module-level safety context for propagation to primitives
        self.module_safety_context = module.safety_context.clone();

        // Add hierarchy root node
        self.lir.hierarchy.push(HierarchyNode {
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
        if let Some(node) = self.lir.hierarchy.first_mut() {
            node.primitive_range = (0, self.next_prim_id);
        }

        // Update statistics
        self.stats.primitives = self.lir.primitives.len();
        self.stats.nets = self.lir.nets.len();
        self.lir.update_stats();

        MirToLirResult {
            lir: self.lir.clone(),
            stats: std::mem::take(&mut self.stats),
            warnings: std::mem::take(&mut self.warnings),
        }
    }

    /// Create nets for a port (one per bit)
    fn create_port_nets(&mut self, port: &skalp_mir::mir::Port, module: &Module) {
        let width = Self::get_type_width(&port.port_type);
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
                    self.lir.inputs.push(net_id);
                    // Check if this is a clock
                    if matches!(port.port_type, DataType::Clock { .. }) {
                        self.clock_nets.push(net_id);
                    }
                    // Check if this is a reset
                    if matches!(port.port_type, DataType::Reset { .. }) {
                        self.reset_nets.push(net_id);
                    }
                    LirNet::new_primary_input(net_id, net_name)
                }
                PortDirection::Output => {
                    self.lir.outputs.push(net_id);
                    // Output nets need a driver - will be set when we process assignments
                    let mut net = LirNet::new(net_id, net_name);
                    net.is_primary_output = true;
                    net
                }
                PortDirection::InOut => {
                    self.lir.inputs.push(net_id);
                    self.lir.outputs.push(net_id);
                    let mut net = LirNet::new(net_id, net_name);
                    net.is_primary_input = true;
                    net.is_primary_output = true;
                    net
                }
            };

            self.lir.add_net(net);
            self.lvalue_to_net
                .insert((0, port.id.0, bit as u32), net_id);
        }

        self.stats.total_bits += width;
    }

    /// Create nets for an internal signal (one per bit)
    fn create_signal_nets(&mut self, signal: &skalp_mir::mir::Signal) {
        let width = Self::get_type_width(&signal.signal_type);
        self.signal_widths.insert(signal.id, width);

        for bit in 0..width {
            let net_name = if width == 1 {
                signal.name.clone()
            } else {
                format!("{}[{}]", signal.name, bit)
            };

            let net_id = self.alloc_net_id();
            let mut net = LirNet::new(net_id, net_name);

            // If signal has initial value, it's a register output
            if signal.initial.is_some() {
                net.is_state_output = true;
            }

            self.lir.add_net(net);
            self.lvalue_to_net
                .insert((1, signal.id.0, bit as u32), net_id);
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
                    self.add_primitive_with_safety(prim);
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

                            self.add_primitive_with_safety(prim);

                            // Mark target net as state output
                            if let Some(net) = self.lir.get_net_mut(target_net) {
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
                // For sequential if, we need to create muxes + DFFs
                // 1. Identify target signals in both branches
                // 2. Decompose the condition
                // 3. For each target: create mux(cond, then_value, else_value) -> DFF -> target

                // Get condition nets
                let cond_nets = self.decompose_expression(&if_stmt.condition);

                // Collect assignments from each branch
                let then_assigns = Self::collect_assignments(&if_stmt.then_block);
                let else_assigns = if let Some(ref else_block) = if_stmt.else_block {
                    Self::collect_assignments(else_block)
                } else {
                    Vec::new()
                };

                // Get all target lvalues (deduplicated by comparing)
                let mut all_targets: Vec<LValue> = Vec::new();
                for (lv, _) in &then_assigns {
                    if !all_targets.contains(lv) {
                        all_targets.push(lv.clone());
                    }
                }
                for (lv, _) in &else_assigns {
                    if !all_targets.contains(lv) {
                        all_targets.push(lv.clone());
                    }
                }

                for target in &all_targets {
                    // Get then and else expressions for this target
                    let then_expr = Self::find_assignment_expr(&then_assigns, target);
                    let else_expr = Self::find_assignment_expr(&else_assigns, target);

                    // Get target nets (current value for feedback)
                    let target_nets = self.get_lvalue_nets(target);

                    // Decompose then/else values
                    let then_nets = if let Some(expr) = then_expr {
                        self.decompose_expression(expr)
                    } else {
                        // Use current value (no assignment in this branch)
                        target_nets.clone()
                    };

                    let else_nets = if let Some(expr) = else_expr {
                        self.decompose_expression(expr)
                    } else {
                        // Use current value (no assignment in this branch)
                        target_nets.clone()
                    };

                    // Create mux: sel=cond, d1=then, d0=else
                    let mux_output = self.create_mux_primitives(&cond_nets, &then_nets, &else_nets);

                    // Create DFFs: mux_output -> DFF -> target
                    for (i, &target_net) in target_nets.iter().enumerate() {
                        if let Some(&d_net) = mux_output.get(i) {
                            let mut prim = Primitive::new_comb(
                                self.alloc_prim_id(),
                                PrimitiveType::DffP,
                                format!("{}.dff_{}", self.hierarchy_path, target_net.0),
                                vec![d_net],
                                vec![target_net],
                            );

                            if let Some(clk) = clock_net {
                                prim.clock = Some(clk);
                                prim.inputs.insert(0, clk);
                            }
                            if let Some(rst) = reset_net {
                                prim.reset = Some(rst);
                            }

                            self.add_primitive_with_safety(prim);

                            if let Some(net) = self.lir.get_net_mut(target_net) {
                                net.is_state_output = true;
                            }
                        }
                    }
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

    /// Collect non-blocking assignments from a block (target, expression) pairs
    fn collect_assignments(block: &Block) -> Vec<(LValue, Expression)> {
        let mut assigns = Vec::new();
        for stmt in &block.statements {
            match stmt {
                Statement::Assignment(assign)
                    if matches!(assign.kind, AssignmentKind::NonBlocking) =>
                {
                    assigns.push((assign.lhs.clone(), assign.rhs.clone()));
                }
                Statement::Block(inner_block) => {
                    assigns.extend(Self::collect_assignments(inner_block));
                }
                _ => {}
            }
        }
        assigns
    }

    /// Find expression for a target LValue in assignments
    fn find_assignment_expr<'a>(
        assigns: &'a [(LValue, Expression)],
        target: &LValue,
    ) -> Option<&'a Expression> {
        assigns
            .iter()
            .find(|(lv, _)| lv == target)
            .map(|(_, expr)| expr)
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
                            self.add_primitive_with_safety(prim);
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
            ExpressionKind::Literal(value) => self.create_constant_nets(value),
            ExpressionKind::Ref(lvalue) => self.get_lvalue_nets(lvalue),
            ExpressionKind::Binary { op, left, right } => {
                let left_nets = self.decompose_expression(left);
                let right_nets = self.decompose_expression(right);
                self.create_binary_op_primitives(*op, &left_nets, &right_nets)
            }
            ExpressionKind::Unary { op, operand } => {
                let operand_nets = self.decompose_expression(operand);
                self.create_unary_op_primitives(*op, &operand_nets)
            }
            ExpressionKind::Conditional {
                cond,
                then_expr,
                else_expr,
            } => {
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
                self.warnings
                    .push(format!("Unsupported expression kind: {:?}", expr.kind));
                let net_id = self.alloc_net_id();
                let net = LirNet::new(net_id, format!("unsupported_{}", net_id.0));
                self.lir.add_net(net);
                vec![net_id]
            }
        }
    }

    /// Create constant driver primitives
    fn create_constant_nets(&mut self, value: &Value) -> Vec<NetId> {
        match value {
            Value::Integer(i) => {
                // Determine width from value
                let width = if *i == 0 {
                    1
                } else {
                    (64 - i.leading_zeros()) as usize
                };
                let mut nets = Vec::new();

                for bit in 0..width {
                    let bit_val = ((*i as u64) >> bit) & 1 != 0;
                    let net_id = self.alloc_net_id();
                    let net = LirNet::new(net_id, format!("const_{}_{}", i, bit));
                    self.lir.add_net(net);

                    let prim = Primitive::new_comb(
                        self.alloc_prim_id(),
                        PrimitiveType::Constant { value: bit_val },
                        format!("{}.const_{}_{}", self.hierarchy_path, i, bit),
                        vec![],
                        vec![net_id],
                    );
                    self.add_primitive_with_safety(prim);
                    nets.push(net_id);
                }
                if nets.is_empty() {
                    // Zero constant - create one bit
                    let net_id = self.alloc_net_id();
                    let net = LirNet::new(net_id, "const_0".to_string());
                    self.lir.add_net(net);

                    let prim = Primitive::new_comb(
                        self.alloc_prim_id(),
                        PrimitiveType::Constant { value: false },
                        format!("{}.const_0", self.hierarchy_path),
                        vec![],
                        vec![net_id],
                    );
                    self.add_primitive_with_safety(prim);
                    nets.push(net_id);
                }
                nets
            }
            Value::BitVector { width, value: v } => {
                let mut nets = Vec::new();
                for bit in 0..*width {
                    let bit_val = (v >> bit) & 1 != 0;
                    let net_id = self.alloc_net_id();
                    let net = LirNet::new(net_id, format!("const_bv_{}_{}", v, bit));
                    self.lir.add_net(net);

                    let prim = Primitive::new_comb(
                        self.alloc_prim_id(),
                        PrimitiveType::Constant { value: bit_val },
                        format!("{}.const_bv_{}_{}", self.hierarchy_path, v, bit),
                        vec![],
                        vec![net_id],
                    );
                    self.add_primitive_with_safety(prim);
                    nets.push(net_id);
                }
                nets
            }
            _ => {
                // Other value types - create single unknown net
                let net_id = self.alloc_net_id();
                let net = LirNet::new(net_id, "const_unknown".to_string());
                self.lir.add_net(net);

                let prim = Primitive::new_comb(
                    self.alloc_prim_id(),
                    PrimitiveType::Constant { value: false },
                    format!("{}.const_unknown", self.hierarchy_path),
                    vec![],
                    vec![net_id],
                );
                self.add_primitive_with_safety(prim);
                vec![net_id]
            }
        }
    }

    /// Get or create a cached constant 0 net for padding shorter operands
    fn get_const_zero_net(&mut self) -> NetId {
        if let Some(net) = self.const_zero_net {
            net
        } else {
            let net_id = self.alloc_net_id();
            let net = LirNet::new(net_id, "const_pad_0".to_string());
            self.lir.add_net(net);

            let prim = Primitive::new_comb(
                self.alloc_prim_id(),
                PrimitiveType::Constant { value: false },
                format!("{}.const_pad_0", self.hierarchy_path),
                vec![],
                vec![net_id],
            );
            self.add_primitive_with_safety(prim);
            self.const_zero_net = Some(net_id);
            net_id
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
                    self.lir
                        .add_net(LirNet::new(out, format!("and_out_{}", out.0)));

                    let prim = Primitive::new_comb(
                        self.alloc_prim_id(),
                        PrimitiveType::And { inputs: 2 },
                        format!("{}.and_{}", self.hierarchy_path, out.0),
                        vec![l, r],
                        vec![out],
                    );
                    self.add_primitive_with_safety(prim);
                    result_nets.push(out);
                }
            }
            BinaryOp::Or | BinaryOp::BitwiseOr | BinaryOp::LogicalOr => {
                for i in 0..width {
                    let l = left.get(i).copied().unwrap_or(left[0]);
                    let r = right.get(i).copied().unwrap_or(right[0]);
                    let out = self.alloc_net_id();
                    self.lir
                        .add_net(LirNet::new(out, format!("or_out_{}", out.0)));

                    let prim = Primitive::new_comb(
                        self.alloc_prim_id(),
                        PrimitiveType::Or { inputs: 2 },
                        format!("{}.or_{}", self.hierarchy_path, out.0),
                        vec![l, r],
                        vec![out],
                    );
                    self.add_primitive_with_safety(prim);
                    result_nets.push(out);
                }
            }
            BinaryOp::Xor | BinaryOp::BitwiseXor => {
                for i in 0..width {
                    let l = left.get(i).copied().unwrap_or(left[0]);
                    let r = right.get(i).copied().unwrap_or(right[0]);
                    let out = self.alloc_net_id();
                    self.lir
                        .add_net(LirNet::new(out, format!("xor_out_{}", out.0)));

                    let prim = Primitive::new_comb(
                        self.alloc_prim_id(),
                        PrimitiveType::Xor,
                        format!("{}.xor_{}", self.hierarchy_path, out.0),
                        vec![l, r],
                        vec![out],
                    );
                    self.add_primitive_with_safety(prim);
                    result_nets.push(out);
                }
            }
            // Arithmetic - use adder chain
            BinaryOp::Add => {
                // Get constant 0 for padding shorter operands (important for correct arithmetic!)
                let zero_net = self.get_const_zero_net();
                let mut carry = None;
                for i in 0..width {
                    // Pad shorter operand with 0, NOT with its first element
                    let l = left.get(i).copied().unwrap_or(zero_net);
                    let r = right.get(i).copied().unwrap_or(zero_net);
                    let sum = self.alloc_net_id();
                    let cout = self.alloc_net_id();
                    self.lir
                        .add_net(LirNet::new(sum, format!("add_sum_{}", sum.0)));
                    self.lir
                        .add_net(LirNet::new(cout, format!("add_cout_{}", cout.0)));

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
                    self.add_primitive_with_safety(prim);
                    result_nets.push(sum);
                    carry = Some(cout);
                }
                // Push the final carry as the MSB (for (N+1)-bit results like N-bit + N-bit)
                // This is needed when the result is wider than the operands (e.g., temp: bit[5] = a + b)
                if let Some(final_carry) = carry {
                    result_nets.push(final_carry);
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
                    self.lir
                        .add_net(LirNet::new(out, format!("eq_xnor_{}", out.0)));

                    let prim = Primitive::new_comb(
                        self.alloc_prim_id(),
                        PrimitiveType::Xnor,
                        format!("{}.eq_xnor_{}", self.hierarchy_path, i),
                        vec![l, r],
                        vec![out],
                    );
                    self.add_primitive_with_safety(prim);
                    xnor_outs.push(out);
                }

                // AND all XNOR outputs
                let eq_out = self.alloc_net_id();
                self.lir
                    .add_net(LirNet::new(eq_out, format!("eq_out_{}", eq_out.0)));

                let prim = Primitive::new_comb(
                    self.alloc_prim_id(),
                    PrimitiveType::And {
                        inputs: xnor_outs.len() as u8,
                    },
                    format!("{}.eq_and", self.hierarchy_path),
                    xnor_outs,
                    vec![eq_out],
                );
                self.add_primitive_with_safety(prim);

                if matches!(op, BinaryOp::NotEqual) {
                    // Invert for NotEqual
                    let neq_out = self.alloc_net_id();
                    self.lir
                        .add_net(LirNet::new(neq_out, format!("neq_out_{}", neq_out.0)));

                    let prim = Primitive::new_comb(
                        self.alloc_prim_id(),
                        PrimitiveType::Inv,
                        format!("{}.neq_inv", self.hierarchy_path),
                        vec![eq_out],
                        vec![neq_out],
                    );
                    self.add_primitive_with_safety(prim);
                    result_nets.push(neq_out);
                } else {
                    result_nets.push(eq_out);
                }
            }
            // Unsigned comparison using subtractor with borrow
            // For a < b: compute b - a, if borrow out is 0, then a < b (actually compute a - b, borrow=1 means a < b)
            // We use a comparator chain approach: compare from MSB to LSB
            BinaryOp::Less | BinaryOp::Greater | BinaryOp::LessEqual | BinaryOp::GreaterEqual => {
                // For Greater/GreaterEqual, swap left and right
                let (cmp_left, cmp_right) =
                    if matches!(op, BinaryOp::Greater | BinaryOp::GreaterEqual) {
                        (right, left) // a > b is same as b < a
                    } else {
                        (left, right)
                    };

                // Build a subtractor chain (cmp_left - cmp_right) to get borrow out
                // Borrow out = 1 means cmp_left < cmp_right
                let zero_net = self.get_const_zero_net();
                let mut borrow = zero_net; // Initial borrow = 0

                for i in 0..width {
                    let l = cmp_left.get(i).copied().unwrap_or(zero_net);
                    let r = cmp_right.get(i).copied().unwrap_or(zero_net);

                    // Full subtractor borrow_out formula:
                    // borrow_out = (!l AND r) OR ((!l OR r) AND borrow_in)
                    // which is equivalent to: (!l AND r) OR ((l XNOR r) AND borrow_in)
                    // when l==r, borrow propagates; when l!=r, depends on !l AND r

                    // XNOR l, r (for borrow propagation when l==r)
                    let l_xnor_r = self.alloc_net_id();
                    self.lir
                        .add_net(LirNet::new(l_xnor_r, format!("cmp_xnor_{}", i)));
                    let prim = Primitive::new_comb(
                        self.alloc_prim_id(),
                        PrimitiveType::Xnor,
                        format!("{}.cmp_xnor_{}", self.hierarchy_path, i),
                        vec![l, r],
                        vec![l_xnor_r],
                    );
                    self.add_primitive_with_safety(prim);

                    // NOT l
                    let not_l = self.alloc_net_id();
                    self.lir
                        .add_net(LirNet::new(not_l, format!("cmp_not_l_{}", i)));
                    let prim = Primitive::new_comb(
                        self.alloc_prim_id(),
                        PrimitiveType::Inv,
                        format!("{}.cmp_not_l_{}", self.hierarchy_path, i),
                        vec![l],
                        vec![not_l],
                    );
                    self.add_primitive_with_safety(prim);

                    // !l AND r (generate borrow when l=0, r=1)
                    let not_l_and_r = self.alloc_net_id();
                    self.lir
                        .add_net(LirNet::new(not_l_and_r, format!("cmp_and1_{}", i)));
                    let prim = Primitive::new_comb(
                        self.alloc_prim_id(),
                        PrimitiveType::And { inputs: 2 },
                        format!("{}.cmp_and1_{}", self.hierarchy_path, i),
                        vec![not_l, r],
                        vec![not_l_and_r],
                    );
                    self.add_primitive_with_safety(prim);

                    // (l XNOR r) AND borrow_in (propagate borrow when l==r)
                    let xnor_and_borrow = self.alloc_net_id();
                    self.lir
                        .add_net(LirNet::new(xnor_and_borrow, format!("cmp_and2_{}", i)));
                    let prim = Primitive::new_comb(
                        self.alloc_prim_id(),
                        PrimitiveType::And { inputs: 2 },
                        format!("{}.cmp_and2_{}", self.hierarchy_path, i),
                        vec![l_xnor_r, borrow],
                        vec![xnor_and_borrow],
                    );
                    self.add_primitive_with_safety(prim);

                    // borrow_out = (!l AND r) OR ((l XNOR r) AND borrow_in)
                    let borrow_out = self.alloc_net_id();
                    self.lir
                        .add_net(LirNet::new(borrow_out, format!("cmp_borrow_{}", i)));
                    let prim = Primitive::new_comb(
                        self.alloc_prim_id(),
                        PrimitiveType::Or { inputs: 2 },
                        format!("{}.cmp_borrow_{}", self.hierarchy_path, i),
                        vec![not_l_and_r, xnor_and_borrow],
                        vec![borrow_out],
                    );
                    self.add_primitive_with_safety(prim);

                    borrow = borrow_out;
                }

                // borrow is now 1 if cmp_left < cmp_right
                let lt_result = borrow;

                // For LessEqual/GreaterEqual, we need (a < b) OR (a == b)
                if matches!(op, BinaryOp::LessEqual | BinaryOp::GreaterEqual) {
                    // First compute equality
                    let mut xnor_outs = Vec::new();
                    for i in 0..width {
                        let l = left.get(i).copied().unwrap_or(zero_net);
                        let r = right.get(i).copied().unwrap_or(zero_net);
                        let out = self.alloc_net_id();
                        self.lir
                            .add_net(LirNet::new(out, format!("cmp_eq_xnor_{}", out.0)));

                        let prim = Primitive::new_comb(
                            self.alloc_prim_id(),
                            PrimitiveType::Xnor,
                            format!("{}.cmp_eq_xnor_{}", self.hierarchy_path, i),
                            vec![l, r],
                            vec![out],
                        );
                        self.add_primitive_with_safety(prim);
                        xnor_outs.push(out);
                    }

                    // AND all XNOR outputs for equality
                    let eq_out = self.alloc_net_id();
                    self.lir
                        .add_net(LirNet::new(eq_out, format!("cmp_eq_out_{}", eq_out.0)));

                    let prim = Primitive::new_comb(
                        self.alloc_prim_id(),
                        PrimitiveType::And {
                            inputs: xnor_outs.len() as u8,
                        },
                        format!("{}.cmp_eq_and", self.hierarchy_path),
                        xnor_outs,
                        vec![eq_out],
                    );
                    self.add_primitive_with_safety(prim);

                    // OR lt_result with eq_out
                    let le_result = self.alloc_net_id();
                    self.lir.add_net(LirNet::new(
                        le_result,
                        format!("cmp_le_out_{}", le_result.0),
                    ));

                    let prim = Primitive::new_comb(
                        self.alloc_prim_id(),
                        PrimitiveType::Or { inputs: 2 },
                        format!("{}.cmp_le_or", self.hierarchy_path),
                        vec![lt_result, eq_out],
                        vec![le_result],
                    );
                    self.add_primitive_with_safety(prim);

                    result_nets.push(le_result);
                } else {
                    result_nets.push(lt_result);
                }
            }
            _ => {
                // Other ops - create placeholder
                self.warnings
                    .push(format!("Unsupported binary op: {:?}", op));
                let out = self.alloc_net_id();
                self.lir
                    .add_net(LirNet::new(out, format!("unsup_binop_{}", out.0)));
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
                    self.lir
                        .add_net(LirNet::new(out, format!("not_out_{}", out.0)));

                    let prim = Primitive::new_comb(
                        self.alloc_prim_id(),
                        PrimitiveType::Inv,
                        format!("{}.inv_{}", self.hierarchy_path, out.0),
                        vec![net],
                        vec![out],
                    );
                    self.add_primitive_with_safety(prim);
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
            self.lir
                .add_net(LirNet::new(out, format!("mux_out_{}", out.0)));

            let prim = Primitive::new_comb(
                self.alloc_prim_id(),
                PrimitiveType::Mux2,
                format!("{}.mux_{}", self.hierarchy_path, out.0),
                vec![sel_net, d0, d1],
                vec![out],
            );
            self.add_primitive_with_safety(prim);
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
                    .filter_map(|bit| {
                        self.lvalue_to_net
                            .get(&(1, signal_id.0, bit as u32))
                            .copied()
                    })
                    .collect()
            }
            LValue::Variable(var_id) => {
                // Variables - create temporary nets
                let net_id = self.alloc_net_id();
                let net = LirNet::new(net_id, format!("var_{}", var_id.0));
                self.lir.add_net(net);
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
                    .filter_map(|bit| {
                        self.lvalue_to_net
                            .get(&(1, signal_id.0, bit as u32))
                            .copied()
                    })
                    .collect()
            }
            _ => Vec::new(),
        }
    }

    /// Get width of a data type
    fn get_type_width(dtype: &DataType) -> usize {
        match dtype {
            DataType::Bit(w) | DataType::Logic(w) | DataType::Int(w) | DataType::Nat(w) => *w,
            DataType::Bool => 1,
            DataType::Clock { .. } => 1,
            DataType::Reset { .. } => 1,
            DataType::Float16 => 16,
            DataType::Float32 => 32,
            DataType::Float64 => 64,
            DataType::Array(inner, size) => Self::get_type_width(inner) * size,
            DataType::BitParam { default, .. }
            | DataType::LogicParam { default, .. }
            | DataType::IntParam { default, .. }
            | DataType::NatParam { default, .. } => *default,
            DataType::BitExpr { default, .. }
            | DataType::LogicExpr { default, .. }
            | DataType::IntExpr { default, .. }
            | DataType::NatExpr { default, .. } => *default,
            DataType::Vec2(inner) => Self::get_type_width(inner) * 2,
            DataType::Vec3(inner) => Self::get_type_width(inner) * 3,
            DataType::Vec4(inner) => Self::get_type_width(inner) * 4,
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

/// Transform a MIR module to Lir
pub fn lower_mir_module_to_lir(module: &Module) -> MirToLirResult {
    let mut transformer = MirToLirTransform::new(&module.name);
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
            safety_context: None,
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
            safety_context: None,
        };

        let result = lower_mir_module_to_lir(&module);
        assert_eq!(result.lir.name, "empty");
        assert_eq!(result.stats.ports, 0);
        assert_eq!(result.stats.signals, 0);
    }

    #[test]
    fn test_transform_simple_and_gate() {
        let module = make_simple_module();
        let result = lower_mir_module_to_lir(&module);

        assert_eq!(result.lir.name, "test_module");
        assert_eq!(result.stats.ports, 3);
        assert_eq!(result.lir.inputs.len(), 2);
        assert_eq!(result.lir.outputs.len(), 1);

        // Should have at least one AND primitive
        let and_count = result
            .lir
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
            ports: vec![Port {
                id: PortId(0),
                name: "data".to_string(),
                direction: PortDirection::Input,
                port_type: DataType::Bit(8),
                physical_constraints: None,
                span: None,
            }],
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
            safety_context: None,
        };

        let result = lower_mir_module_to_lir(&module);

        // 8-bit port should create 8 nets
        assert_eq!(result.lir.inputs.len(), 8);
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
            safety_context: None,
        };

        let result = lower_mir_module_to_lir(&module);

        // Should have a constant primitive
        let const_count = result
            .lir
            .primitives
            .iter()
            .filter(|p| matches!(p.ptype, PrimitiveType::Constant { .. }))
            .count();
        assert!(const_count >= 1);
    }

    #[test]
    fn test_hierarchy_tracking() {
        let module = make_simple_module();
        let result = lower_mir_module_to_lir(&module);

        assert!(!result.lir.hierarchy.is_empty());
        assert_eq!(result.lir.hierarchy[0].module, "test_module");
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
            safety_context: None,
        };

        let result = lower_mir_module_to_lir(&module);

        // Should have at least one XOR primitive
        let xor_count = result
            .lir
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
            safety_context: None,
        };

        let result = lower_mir_module_to_lir(&module);

        // Should have at least one OR primitive
        let or_count = result
            .lir
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
            safety_context: None,
        };

        let result = lower_mir_module_to_lir(&module);

        // Should have at least one INV primitive
        let inv_count = result
            .lir
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
            safety_context: None,
        };

        let result = lower_mir_module_to_lir(&module);

        // Should have adder primitives (HalfAdder + FullAdders)
        let adder_count = result
            .lir
            .primitives
            .iter()
            .filter(|p| matches!(p.ptype, PrimitiveType::HalfAdder | PrimitiveType::FullAdder))
            .count();
        assert!(
            adder_count >= 1,
            "Expected adder primitives, found {} primitives total",
            result.lir.primitives.len()
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
            safety_context: None,
        };

        let result = lower_mir_module_to_lir(&module);

        // Should have at least one MUX2 primitive
        let mux_count = result
            .lir
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
            safety_context: None,
        };

        let result = lower_mir_module_to_lir(&module);

        // Should have XNOR primitives (for bit comparison) and AND (for combining)
        let xnor_count = result
            .lir
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
                safety_context: None,
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
            safety_context: None,
        };

        let result = lower_mir_module_to_lir(&module);

        // 4-bit input + 4-bit output + 4-bit internal = 12 total bits
        assert_eq!(result.stats.total_bits, 12);
        assert_eq!(result.stats.signals, 1);
    }

    #[test]
    fn test_fit_estimation() {
        let module = make_simple_module();
        let result = lower_mir_module_to_lir(&module);

        // Calculate total FIT from primitives
        let total_fit: f64 = result
            .lir
            .primitives
            .iter()
            .map(|p| p.ptype.base_fit())
            .sum();

        // Should have some non-zero FIT (from the AND gate + buffers)
        assert!(total_fit > 0.0, "Expected positive total FIT");
    }
}
