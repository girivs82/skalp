//! MIR to LIR Transformation
//!
//! Converts Mid-level IR (MIR) to word-level intermediate representation
//! (`Lir`) for technology mapping.
//!
//! Unlike gate-level transformation, this preserves word-level
//! operations (Add, Mul, Mux, etc.) without decomposing them to gates.
//! Decomposition is deferred to the technology mapping phase.
//!
//! # Flow
//!
//! ```text
//! MIR → Lir (word-level) → TechMapper → GateNetlist (gate-level)
//! ```

use crate::lir::{Lir, LirOp, LirSafetyInfo, LirSignalId, LirStats};
use skalp_mir::mir::{
    AssignmentKind, BinaryOp, Block, ContinuousAssign, DataType, EdgeType, Expression,
    ExpressionKind, LValue, Module, PortDirection, PortId, Process, ProcessKind, ReduceOp,
    SafetyContext, SensitivityList, SignalId, Statement, UnaryOp, Value,
};
use std::collections::HashMap;

/// Convert MIR SafetyContext to LIR LirSafetyInfo
fn safety_context_to_lir_info(ctx: &SafetyContext) -> LirSafetyInfo {
    // Detect boot-time-only hardware by mechanism name pattern
    let is_boot_time_only = ctx
        .mechanism_name
        .as_ref()
        .map(|name| {
            let lower = name.to_lowercase();
            lower.contains("bist") || lower.contains("boot") || lower.contains("selftest")
        })
        .unwrap_or(false);

    LirSafetyInfo {
        goal_name: ctx.implementing_goal.clone(),
        mechanism_name: ctx.mechanism_name.clone(),
        is_sm_of_sm: false,
        protected_sm_name: None,
        is_boot_time_only,
    }
}

/// Result of MIR to LIR transformation
#[derive(Debug, serde::Serialize, serde::Deserialize)]
pub struct MirToLirResult {
    /// The generated word-level LIR
    pub lir: Lir,
    /// Transformation statistics
    pub stats: LirStats,
    /// Warnings generated during transformation
    pub warnings: Vec<String>,
}

/// Backward-compatible type alias
pub type MirToWordLirResult = MirToLirResult;

/// MIR to LIR transformer
pub struct MirToLirTransform {
    /// Output LIR being built
    lir: Lir,
    /// Mapping from PortId to signal ID
    port_to_signal: HashMap<PortId, LirSignalId>,
    /// Mapping from SignalId to signal ID
    signal_to_lir_signal: HashMap<SignalId, LirSignalId>,
    /// Width of each port/signal
    port_widths: HashMap<PortId, u32>,
    signal_widths: HashMap<SignalId, u32>,
    /// Current hierarchy path (for node naming)
    hierarchy_path: String,
    /// Warnings
    warnings: Vec<String>,
    /// Clock signal IDs
    clock_signals: Vec<LirSignalId>,
    /// Reset signal IDs
    reset_signals: Vec<LirSignalId>,
    /// Counter for generating unique signal names
    temp_counter: u32,
}

impl MirToLirTransform {
    /// Create a new transformer
    pub fn new(module_name: &str) -> Self {
        Self {
            lir: Lir::new(module_name.to_string()),
            port_to_signal: HashMap::new(),
            signal_to_lir_signal: HashMap::new(),
            port_widths: HashMap::new(),
            signal_widths: HashMap::new(),
            hierarchy_path: "top".to_string(),
            warnings: Vec::new(),
            clock_signals: Vec::new(),
            reset_signals: Vec::new(),
            temp_counter: 0,
        }
    }

    /// Transform a MIR module to LIR
    pub fn transform(&mut self, module: &Module) -> MirToLirResult {
        self.hierarchy_path = module.name.clone();

        // Propagate module-level safety context to LIR
        if let Some(ref ctx) = module.safety_context {
            if ctx.has_safety_annotation() {
                self.lir.module_safety_info = Some(safety_context_to_lir_info(ctx));
            }
        }

        // Phase 1: Create signals for all ports (preserving width)
        for port in &module.ports {
            self.create_port_signal(port);
        }

        // Phase 2: Create signals for all internal signals
        for signal in &module.signals {
            self.create_internal_signal(signal);
        }

        // Phase 3: Transform continuous assignments
        for assign in &module.assignments {
            self.transform_continuous_assign(assign);
        }

        // Phase 4: Transform processes
        for process in &module.processes {
            self.transform_process(process);
        }

        // Phase 5: Populate clock and reset nets
        self.lir.clocks = std::mem::take(&mut self.clock_signals);
        self.lir.resets = std::mem::take(&mut self.reset_signals);

        let stats = LirStats::from_lir(&self.lir);

        MirToLirResult {
            lir: self.lir.clone(),
            stats,
            warnings: std::mem::take(&mut self.warnings),
        }
    }

    /// Create a signal for a port
    fn create_port_signal(&mut self, port: &skalp_mir::mir::Port) {
        let width = Self::get_type_width(&port.port_type);
        self.port_widths.insert(port.id, width);

        let signal_id = match port.direction {
            PortDirection::Input => {
                let id = self.lir.add_input(port.name.clone(), width);
                // Track clocks and resets
                if matches!(port.port_type, DataType::Clock { .. }) {
                    self.clock_signals.push(id);
                }
                if matches!(port.port_type, DataType::Reset { .. }) {
                    self.reset_signals.push(id);
                }
                id
            }
            PortDirection::Output => {
                // Check if this is a detection signal output
                if port.is_detection_signal() {
                    self.lir.add_detection_output(port.name.clone(), width)
                } else {
                    self.lir.add_output(port.name.clone(), width)
                }
            }
            PortDirection::InOut => {
                // InOut is both input and output
                let id = self.lir.add_input(port.name.clone(), width);
                self.lir.outputs.push(id);
                self.lir.signals[id.0 as usize].is_output = true;
                // Propagate detection signal flag for InOut ports
                if port.is_detection_signal() {
                    self.lir.mark_as_detection(id);
                }
                id
            }
        };

        self.port_to_signal.insert(port.id, signal_id);
    }

    /// Create a signal for an internal signal
    fn create_internal_signal(&mut self, signal: &skalp_mir::mir::Signal) {
        let width = Self::get_type_width(&signal.signal_type);
        self.signal_widths.insert(signal.id, width);

        let signal_id = self.lir.add_signal(signal.name.clone(), width);
        self.signal_to_lir_signal.insert(signal.id, signal_id);

        // Propagate detection signal flag from MIR internal signals
        // This is critical for hierarchical flattening where sub-module output ports
        // marked with #[detection_signal] become internal signals in the flattened design
        if signal.is_detection_signal() {
            println!(
                "✅ [WORD_LIR_DETECTION] Marking internal signal '{}' as detection",
                signal.name
            );
            self.lir.mark_as_detection(signal_id);
        }
    }

    /// Transform a continuous assignment
    fn transform_continuous_assign(&mut self, assign: &ContinuousAssign) {
        let target_signal = self.get_lvalue_signal(&assign.lhs);
        let target_width = self.get_lvalue_width(&assign.lhs);

        // Transform the RHS expression
        let expr_signal = self.transform_expression(&assign.rhs, target_width);

        // If they're different signals, create a buffer
        if expr_signal != target_signal {
            self.lir.add_node(
                LirOp::Buffer {
                    width: target_width,
                },
                vec![expr_signal],
                target_signal,
                format!("{}.assign", self.hierarchy_path),
            );
        }
    }

    /// Transform a process
    fn transform_process(&mut self, process: &Process) {
        match process.kind {
            ProcessKind::Sequential => {
                // Extract clock from sensitivity list
                let clock_signal = self.get_clock_from_sensitivity(&process.sensitivity);
                let reset_signal = self.get_reset_from_sensitivity(&process.sensitivity);

                // Transform sequential statements (create registers)
                self.transform_sequential_block(&process.body, clock_signal, reset_signal);
            }
            ProcessKind::Combinational | ProcessKind::General => {
                // Transform combinational statements
                self.transform_combinational_block(&process.body);
            }
        }
    }

    /// Transform a sequential block
    fn transform_sequential_block(
        &mut self,
        block: &Block,
        clock_signal: Option<LirSignalId>,
        reset_signal: Option<LirSignalId>,
    ) {
        for stmt in &block.statements {
            self.transform_sequential_statement(stmt, clock_signal, reset_signal);
        }
    }

    /// Transform a sequential statement
    fn transform_sequential_statement(
        &mut self,
        stmt: &Statement,
        clock_signal: Option<LirSignalId>,
        reset_signal: Option<LirSignalId>,
    ) {
        match stmt {
            Statement::Assignment(assign) => {
                if matches!(assign.kind, AssignmentKind::NonBlocking) {
                    // Non-blocking assignment -> create register
                    let target_signal = self.get_lvalue_signal(&assign.lhs);
                    let target_width = self.get_lvalue_width(&assign.lhs);
                    let d_signal = self.transform_expression(&assign.rhs, target_width);

                    let reg_op = LirOp::Reg {
                        width: target_width,
                        has_enable: false,
                        has_reset: reset_signal.is_some(),
                        reset_value: Some(0), // Default reset value
                    };

                    if let Some(clk) = clock_signal {
                        self.lir.add_seq_node(
                            reg_op,
                            vec![d_signal],
                            target_signal,
                            format!("{}.reg", self.hierarchy_path),
                            clk,
                            reset_signal,
                        );
                    } else {
                        self.lir.add_node(
                            reg_op,
                            vec![d_signal],
                            target_signal,
                            format!("{}.reg", self.hierarchy_path),
                        );
                    }
                } else {
                    // Blocking assignment in sequential - treat as combinational
                    self.transform_combinational_statement(stmt);
                }
            }
            Statement::If(if_stmt) => {
                // Check for SDFF pattern: if (reset) { reg = const } else { reg = value }
                // When detected, skip the mux and use integrated synchronous reset
                let is_reset_condition = self.is_reset_signal_reference(&if_stmt.condition);

                // Collect assignments from each branch
                let then_assigns = Self::collect_assignments(&if_stmt.then_block);
                let else_assigns = if let Some(ref else_block) = if_stmt.else_block {
                    Self::collect_assignments(else_block)
                } else {
                    Vec::new()
                };

                // Get all target lvalues
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
                    let target_signal = self.get_lvalue_signal(target);
                    let target_width = self.get_lvalue_width(target);

                    // Get then and else expressions
                    let then_expr = Self::find_assignment_expr(&then_assigns, target);
                    let else_expr = Self::find_assignment_expr(&else_assigns, target);

                    // Check if else branch has nested ifs for this target
                    // If so, skip this target at this level - the nested ifs will handle it
                    let else_has_nested_ifs = if let Some(ref else_block) = if_stmt.else_block {
                        Self::block_has_nested_if_for_target(else_block, target)
                    } else {
                        false
                    };

                    // Skip targets that are handled by nested ifs to avoid duplicate registers
                    if else_has_nested_ifs {
                        continue;
                    }

                    // Check for SDFF pattern:
                    // - Condition is reset signal
                    // - Then branch (reset case) is a constant
                    // - There's an else branch with the actual next value
                    let sdff_pattern = if is_reset_condition && else_expr.is_some() {
                        then_expr.and_then(Self::try_extract_constant)
                    } else {
                        None
                    };

                    if let Some(reset_value) = sdff_pattern {
                        // SDFF pattern detected: skip mux, use integrated sync reset
                        // The register's internal reset will handle the reset behavior
                        let else_signal = if let Some(expr) = else_expr {
                            self.transform_expression(expr, target_width)
                        } else {
                            target_signal // Feedback: keep current value
                        };

                        // Create register with sync reset (no mux needed)
                        let reg_op = LirOp::Reg {
                            width: target_width,
                            has_enable: false,
                            has_reset: true,
                            reset_value: Some(reset_value),
                        };

                        if let Some(clk) = clock_signal {
                            self.lir.add_seq_node(
                                reg_op,
                                vec![else_signal], // Direct value, no mux
                                target_signal,
                                format!("{}.reg", self.hierarchy_path),
                                clk,
                                reset_signal,
                            );
                        } else {
                            self.lir.add_node(
                                reg_op,
                                vec![else_signal],
                                target_signal,
                                format!("{}.reg", self.hierarchy_path),
                            );
                        }
                    } else {
                        // Standard pattern: create mux + register
                        let cond_signal = self.transform_expression(&if_stmt.condition, 1);

                        // Transform values
                        let then_signal = if let Some(expr) = then_expr {
                            self.transform_expression(expr, target_width)
                        } else {
                            target_signal // Feedback: keep current value
                        };

                        let else_signal = if let Some(expr) = else_expr {
                            self.transform_expression(expr, target_width)
                        } else {
                            target_signal // Feedback: keep current value
                        };

                        // Create mux: sel, d0 (else), d1 (then)
                        let mux_out = self.alloc_temp_signal(target_width);
                        self.lir.add_node(
                            LirOp::Mux2 {
                                width: target_width,
                            },
                            vec![cond_signal, else_signal, then_signal],
                            mux_out,
                            format!("{}.mux", self.hierarchy_path),
                        );

                        // Create register
                        let reg_op = LirOp::Reg {
                            width: target_width,
                            has_enable: false,
                            has_reset: reset_signal.is_some(),
                            reset_value: Some(0),
                        };

                        if let Some(clk) = clock_signal {
                            self.lir.add_seq_node(
                                reg_op,
                                vec![mux_out],
                                target_signal,
                                format!("{}.reg", self.hierarchy_path),
                                clk,
                                reset_signal,
                            );
                        } else {
                            self.lir.add_node(
                                reg_op,
                                vec![mux_out],
                                target_signal,
                                format!("{}.reg", self.hierarchy_path),
                            );
                        }
                    }
                }

                // Recursively process nested if statements in then and else blocks
                // This handles targets that are assigned inside nested conditionals
                for stmt in &if_stmt.then_block.statements {
                    if let Statement::If(_) = stmt {
                        self.transform_sequential_statement(stmt, clock_signal, reset_signal);
                    }
                }
                if let Some(ref else_block) = if_stmt.else_block {
                    for stmt in &else_block.statements {
                        if let Statement::If(_) = stmt {
                            self.transform_sequential_statement(stmt, clock_signal, reset_signal);
                        }
                    }
                }
            }
            Statement::Block(block) => {
                self.transform_sequential_block(block, clock_signal, reset_signal);
            }
            _ => {
                self.transform_combinational_statement(stmt);
            }
        }
    }

    /// Collect non-blocking assignments from a block
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

    /// Check if a block contains nested if statements that assign to a target
    /// This is used to determine if SDFF pattern can be safely applied
    fn block_has_nested_if_for_target(block: &Block, target: &LValue) -> bool {
        for stmt in &block.statements {
            match stmt {
                Statement::If(if_stmt) => {
                    // Check if this if statement or its children assign to target
                    let then_assigns = Self::collect_all_assignments_recursive(&if_stmt.then_block);
                    if then_assigns.iter().any(|(lv, _)| lv == target) {
                        return true;
                    }
                    if let Some(ref else_block) = if_stmt.else_block {
                        let else_assigns = Self::collect_all_assignments_recursive(else_block);
                        if else_assigns.iter().any(|(lv, _)| lv == target) {
                            return true;
                        }
                    }
                }
                Statement::Block(inner_block) => {
                    if Self::block_has_nested_if_for_target(inner_block, target) {
                        return true;
                    }
                }
                _ => {}
            }
        }
        false
    }

    /// Recursively collect all assignments from a block, including those in nested ifs
    fn collect_all_assignments_recursive(block: &Block) -> Vec<(LValue, Expression)> {
        let mut assigns = Vec::new();
        for stmt in &block.statements {
            match stmt {
                Statement::Assignment(assign)
                    if matches!(assign.kind, AssignmentKind::NonBlocking) =>
                {
                    assigns.push((assign.lhs.clone(), assign.rhs.clone()));
                }
                Statement::Block(inner_block) => {
                    assigns.extend(Self::collect_all_assignments_recursive(inner_block));
                }
                Statement::If(if_stmt) => {
                    assigns.extend(Self::collect_all_assignments_recursive(&if_stmt.then_block));
                    if let Some(ref else_block) = if_stmt.else_block {
                        assigns.extend(Self::collect_all_assignments_recursive(else_block));
                    }
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
                let target_signal = self.get_lvalue_signal(&assign.lhs);
                let target_width = self.get_lvalue_width(&assign.lhs);
                let expr_signal = self.transform_expression(&assign.rhs, target_width);

                if expr_signal != target_signal {
                    self.lir.add_node(
                        LirOp::Buffer {
                            width: target_width,
                        },
                        vec![expr_signal],
                        target_signal,
                        format!("{}.wire", self.hierarchy_path),
                    );
                }
            }
            Statement::If(if_stmt) => {
                // For combinational if, create mux
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

    /// Transform an expression and return its output signal ID
    fn transform_expression(&mut self, expr: &Expression, expected_width: u32) -> LirSignalId {
        match &expr.kind {
            ExpressionKind::Literal(value) => self.create_constant(value, expected_width),
            ExpressionKind::Ref(lvalue) => self.get_lvalue_signal(lvalue),
            ExpressionKind::Binary { op, left, right } => {
                self.transform_binary_op(*op, left, right, expected_width)
            }
            ExpressionKind::Unary { op, operand } => {
                self.transform_unary_op(*op, operand, expected_width)
            }
            ExpressionKind::Conditional {
                cond,
                then_expr,
                else_expr,
            } => {
                let cond_signal = self.transform_expression(cond, 1);
                let then_signal = self.transform_expression(then_expr, expected_width);
                let else_signal = self.transform_expression(else_expr, expected_width);
                let out = self.alloc_temp_signal(expected_width);

                self.lir.add_node(
                    LirOp::Mux2 {
                        width: expected_width,
                    },
                    vec![cond_signal, else_signal, then_signal],
                    out,
                    format!("{}.mux", self.hierarchy_path),
                );
                out
            }
            ExpressionKind::Concat(exprs) => {
                let mut widths = Vec::new();
                let mut signals = Vec::new();

                for e in exprs {
                    let w = self.infer_expression_width(e);
                    widths.push(w);
                    signals.push(self.transform_expression(e, w));
                }

                let out = self.alloc_temp_signal(widths.iter().sum());
                self.lir.add_node(
                    LirOp::Concat { widths },
                    signals,
                    out,
                    format!("{}.concat", self.hierarchy_path),
                );
                out
            }
            _ => {
                self.warnings
                    .push(format!("Unsupported expression kind: {:?}", expr.kind));
                self.alloc_temp_signal(expected_width)
            }
        }
    }

    /// Transform a binary operation
    fn transform_binary_op(
        &mut self,
        op: BinaryOp,
        left: &Expression,
        right: &Expression,
        expected_width: u32,
    ) -> LirSignalId {
        let left_width = self.infer_expression_width(left);
        let right_width = self.infer_expression_width(right);
        let operand_width = left_width.max(right_width);

        let left_signal = self.transform_expression(left, operand_width);
        let right_signal = self.transform_expression(right, operand_width);

        let (word_op, result_width) = match op {
            // Arithmetic
            BinaryOp::Add => (
                LirOp::Add {
                    width: operand_width,
                    has_carry: expected_width > operand_width,
                },
                expected_width,
            ),
            BinaryOp::Sub => (
                LirOp::Sub {
                    width: operand_width,
                    has_borrow: false,
                },
                operand_width,
            ),
            BinaryOp::Mul => (
                LirOp::Mul {
                    width: operand_width,
                    result_width: expected_width,
                },
                expected_width,
            ),

            // Bitwise logic
            BinaryOp::And | BinaryOp::BitwiseAnd | BinaryOp::LogicalAnd => (
                LirOp::And {
                    width: operand_width,
                },
                operand_width,
            ),
            BinaryOp::Or | BinaryOp::BitwiseOr | BinaryOp::LogicalOr => (
                LirOp::Or {
                    width: operand_width,
                },
                operand_width,
            ),
            BinaryOp::Xor | BinaryOp::BitwiseXor => (
                LirOp::Xor {
                    width: operand_width,
                },
                operand_width,
            ),

            // Comparison (result is 1 bit)
            BinaryOp::Equal => (
                LirOp::Eq {
                    width: operand_width,
                },
                1,
            ),
            BinaryOp::NotEqual => (
                LirOp::Ne {
                    width: operand_width,
                },
                1,
            ),
            BinaryOp::Less => (
                LirOp::Lt {
                    width: operand_width,
                },
                1,
            ),
            BinaryOp::LessEqual => (
                LirOp::Le {
                    width: operand_width,
                },
                1,
            ),
            BinaryOp::Greater => (
                LirOp::Gt {
                    width: operand_width,
                },
                1,
            ),
            BinaryOp::GreaterEqual => (
                LirOp::Ge {
                    width: operand_width,
                },
                1,
            ),

            // Shifts
            BinaryOp::LeftShift => (
                LirOp::Shl {
                    width: operand_width,
                },
                operand_width,
            ),
            BinaryOp::RightShift => (
                LirOp::Shr {
                    width: operand_width,
                },
                operand_width,
            ),

            _ => {
                self.warnings
                    .push(format!("Unsupported binary op: {:?}", op));
                (
                    LirOp::Buffer {
                        width: operand_width,
                    },
                    operand_width,
                )
            }
        };

        let out = self.alloc_temp_signal(result_width);
        self.lir.add_node(
            word_op,
            vec![left_signal, right_signal],
            out,
            format!("{}.{:?}", self.hierarchy_path, op),
        );
        out
    }

    /// Transform a unary operation
    fn transform_unary_op(
        &mut self,
        op: UnaryOp,
        operand: &Expression,
        expected_width: u32,
    ) -> LirSignalId {
        let operand_width = self.infer_expression_width(operand);
        let operand_signal = self.transform_expression(operand, operand_width);

        let (word_op, result_width) = match op {
            UnaryOp::Not | UnaryOp::BitwiseNot => (
                LirOp::Not {
                    width: operand_width,
                },
                operand_width,
            ),
            UnaryOp::Negate => {
                // Two's complement negation
                // First invert
                let inv_out = self.alloc_temp_signal(operand_width);
                self.lir.add_node(
                    LirOp::Not {
                        width: operand_width,
                    },
                    vec![operand_signal],
                    inv_out,
                    format!("{}.neg_inv", self.hierarchy_path),
                );

                // Then add 1
                let one = self.create_constant(&Value::Integer(1), operand_width);
                let out = self.alloc_temp_signal(operand_width);
                self.lir.add_node(
                    LirOp::Add {
                        width: operand_width,
                        has_carry: false,
                    },
                    vec![inv_out, one],
                    out,
                    format!("{}.neg_add", self.hierarchy_path),
                );
                return out;
            }
            UnaryOp::Reduce(ReduceOp::And) => (
                LirOp::RedAnd {
                    width: operand_width,
                },
                1,
            ),
            UnaryOp::Reduce(ReduceOp::Or) => (
                LirOp::RedOr {
                    width: operand_width,
                },
                1,
            ),
            UnaryOp::Reduce(ReduceOp::Xor) => (
                LirOp::RedXor {
                    width: operand_width,
                },
                1,
            ),
            UnaryOp::Reduce(_) => {
                // Other reductions (Nand, Nor, Xnor) - treat as unsupported for now
                self.warnings
                    .push(format!("Unsupported reduction op: {:?}", op));
                (
                    LirOp::Buffer {
                        width: operand_width,
                    },
                    operand_width,
                )
            }
            _ => {
                self.warnings
                    .push(format!("Unsupported unary op: {:?}", op));
                (
                    LirOp::Buffer {
                        width: operand_width,
                    },
                    operand_width,
                )
            }
        };

        let out = self.alloc_temp_signal(result_width);
        self.lir.add_node(
            word_op,
            vec![operand_signal],
            out,
            format!("{}.{:?}", self.hierarchy_path, op),
        );
        out
    }

    /// Create a constant signal
    fn create_constant(&mut self, value: &Value, width: u32) -> LirSignalId {
        let (val, w) = match value {
            Value::Integer(i) => (*i as u64, width),
            Value::BitVector {
                width: bw,
                value: v,
            } => (*v, *bw as u32),
            _ => (0, width),
        };

        let out = self.alloc_temp_signal(w);
        self.lir.add_node(
            LirOp::Constant {
                width: w,
                value: val,
            },
            vec![],
            out,
            format!("{}.const_{}", self.hierarchy_path, val),
        );
        out
    }

    /// Get signal ID for an LValue
    fn get_lvalue_signal(&mut self, lvalue: &LValue) -> LirSignalId {
        match lvalue {
            LValue::Port(port_id) => {
                self.port_to_signal
                    .get(port_id)
                    .copied()
                    .unwrap_or_else(|| {
                        self.warnings.push(format!("Unknown port: {:?}", port_id));
                        self.alloc_temp_signal(1)
                    })
            }
            LValue::Signal(signal_id) => self
                .signal_to_lir_signal
                .get(signal_id)
                .copied()
                .unwrap_or_else(|| {
                    self.warnings
                        .push(format!("Unknown signal: {:?}", signal_id));
                    self.alloc_temp_signal(1)
                }),
            LValue::Variable(var_id) => {
                // Variables become temporary signals
                let signal = self.alloc_temp_signal(1);
                self.warnings
                    .push(format!("Variable {:?} treated as temp signal", var_id));
                signal
            }
            LValue::BitSelect { base, index } => {
                let base_signal = self.get_lvalue_signal(base);
                let base_width = self.get_lvalue_width(base);

                if let ExpressionKind::Literal(Value::Integer(i)) = &index.kind {
                    let out = self.alloc_temp_signal(1);
                    self.lir.add_node(
                        LirOp::RangeSelect {
                            width: base_width,
                            high: *i as u32,
                            low: *i as u32,
                        },
                        vec![base_signal],
                        out,
                        format!("{}.bit_sel", self.hierarchy_path),
                    );
                    out
                } else {
                    // Dynamic bit select
                    let idx_signal = self.transform_expression(index, 32);
                    let out = self.alloc_temp_signal(1);
                    self.lir.add_node(
                        LirOp::BitSelect { width: base_width },
                        vec![base_signal, idx_signal],
                        out,
                        format!("{}.dyn_bit_sel", self.hierarchy_path),
                    );
                    out
                }
            }
            LValue::RangeSelect { base, high, low } => {
                let base_signal = self.get_lvalue_signal(base);
                let base_width = self.get_lvalue_width(base);

                if let (
                    ExpressionKind::Literal(Value::Integer(h)),
                    ExpressionKind::Literal(Value::Integer(l)),
                ) = (&high.kind, &low.kind)
                {
                    let out_width = (*h - *l + 1) as u32;
                    let out = self.alloc_temp_signal(out_width);
                    self.lir.add_node(
                        LirOp::RangeSelect {
                            width: base_width,
                            high: *h as u32,
                            low: *l as u32,
                        },
                        vec![base_signal],
                        out,
                        format!("{}.range_sel", self.hierarchy_path),
                    );
                    out
                } else {
                    self.warnings
                        .push("Dynamic range select not supported".to_string());
                    base_signal
                }
            }
            LValue::Concat(parts) => {
                let mut widths = Vec::new();
                let mut signals = Vec::new();

                for part in parts {
                    let w = self.get_lvalue_width(part);
                    widths.push(w);
                    signals.push(self.get_lvalue_signal(part));
                }

                let out = self.alloc_temp_signal(widths.iter().sum());
                self.lir.add_node(
                    LirOp::Concat { widths },
                    signals,
                    out,
                    format!("{}.concat", self.hierarchy_path),
                );
                out
            }
        }
    }

    /// Get width of an LValue
    fn get_lvalue_width(&self, lvalue: &LValue) -> u32 {
        match lvalue {
            LValue::Port(port_id) => self.port_widths.get(port_id).copied().unwrap_or(1),
            LValue::Signal(signal_id) => self.signal_widths.get(signal_id).copied().unwrap_or(1),
            LValue::Variable(_) => 1,
            LValue::BitSelect { .. } => 1,
            LValue::RangeSelect { high, low, .. } => {
                if let (
                    ExpressionKind::Literal(Value::Integer(h)),
                    ExpressionKind::Literal(Value::Integer(l)),
                ) = (&high.kind, &low.kind)
                {
                    (*h - *l + 1) as u32
                } else {
                    1
                }
            }
            LValue::Concat(parts) => parts.iter().map(|p| self.get_lvalue_width(p)).sum(),
        }
    }

    /// Infer the width of an expression
    fn infer_expression_width(&self, expr: &Expression) -> u32 {
        match &expr.kind {
            ExpressionKind::Literal(value) => match value {
                Value::Integer(i) => {
                    if *i == 0 {
                        1
                    } else {
                        64 - (*i as u64).leading_zeros()
                    }
                }
                Value::BitVector { width, .. } => *width as u32,
                _ => 1,
            },
            ExpressionKind::Ref(lvalue) => self.get_lvalue_width(lvalue),
            ExpressionKind::Binary {
                left, right, op, ..
            } => {
                let lw = self.infer_expression_width(left);
                let rw = self.infer_expression_width(right);
                match op {
                    // Comparison results are 1 bit
                    BinaryOp::Equal
                    | BinaryOp::NotEqual
                    | BinaryOp::Less
                    | BinaryOp::LessEqual
                    | BinaryOp::Greater
                    | BinaryOp::GreaterEqual => 1,
                    // Multiplication doubles width
                    BinaryOp::Mul => lw + rw,
                    // Others preserve max width
                    _ => lw.max(rw),
                }
            }
            ExpressionKind::Unary { operand, op, .. } => match op {
                UnaryOp::Reduce(_) => 1, // All reductions produce 1-bit output
                _ => self.infer_expression_width(operand),
            },
            ExpressionKind::Conditional { then_expr, .. } => self.infer_expression_width(then_expr),
            ExpressionKind::Concat(exprs) => {
                exprs.iter().map(|e| self.infer_expression_width(e)).sum()
            }
            _ => 1,
        }
    }

    /// Get clock signal from sensitivity list
    fn get_clock_from_sensitivity(&self, sens: &SensitivityList) -> Option<LirSignalId> {
        if let SensitivityList::Edge(edges) = sens {
            for edge in edges {
                if matches!(edge.edge, EdgeType::Rising | EdgeType::Falling) {
                    return self.get_lvalue_signal_readonly(&edge.signal);
                }
            }
        }
        self.clock_signals.first().copied()
    }

    /// Get reset signal from sensitivity list
    fn get_reset_from_sensitivity(&self, sens: &SensitivityList) -> Option<LirSignalId> {
        if let SensitivityList::Edge(edges) = sens {
            for edge in edges {
                if matches!(edge.edge, EdgeType::Active | EdgeType::Inactive) {
                    return self.get_lvalue_signal_readonly(&edge.signal);
                }
            }
        }
        self.reset_signals.first().copied()
    }

    /// Get LValue signal without modifying state
    fn get_lvalue_signal_readonly(&self, lvalue: &LValue) -> Option<LirSignalId> {
        match lvalue {
            LValue::Port(port_id) => self.port_to_signal.get(port_id).copied(),
            LValue::Signal(signal_id) => self.signal_to_lir_signal.get(signal_id).copied(),
            _ => None,
        }
    }

    /// Check if an expression is a reference to the reset signal
    /// Returns true if the expression directly references a tracked reset signal
    fn is_reset_signal_reference(&self, expr: &Expression) -> bool {
        match &expr.kind {
            ExpressionKind::Ref(lvalue) => match lvalue {
                LValue::Port(port_id) => {
                    if let Some(&sig_id) = self.port_to_signal.get(port_id) {
                        self.reset_signals.contains(&sig_id)
                    } else {
                        false
                    }
                }
                LValue::Signal(signal_id) => {
                    if let Some(&sig_id) = self.signal_to_lir_signal.get(signal_id) {
                        self.reset_signals.contains(&sig_id)
                    } else {
                        false
                    }
                }
                _ => false,
            },
            _ => false,
        }
    }

    /// Try to extract a constant integer value from an expression
    /// Returns Some(value) if the expression is a literal integer, None otherwise
    fn try_extract_constant(expr: &Expression) -> Option<u64> {
        match &expr.kind {
            ExpressionKind::Literal(Value::Integer(i)) => Some(*i as u64),
            ExpressionKind::Literal(Value::BitVector { value, .. }) => Some(*value),
            _ => None,
        }
    }

    /// Get width of a data type
    fn get_type_width(dtype: &DataType) -> u32 {
        match dtype {
            DataType::Bit(w) | DataType::Logic(w) | DataType::Int(w) | DataType::Nat(w) => {
                *w as u32
            }
            DataType::Bool => 1,
            DataType::Clock { .. } => 1,
            DataType::Reset { .. } => 1,
            DataType::Float16 => 16,
            DataType::Float32 => 32,
            DataType::Float64 => 64,
            DataType::Array(inner, size) => Self::get_type_width(inner) * (*size as u32),
            DataType::BitParam { default, .. }
            | DataType::LogicParam { default, .. }
            | DataType::IntParam { default, .. }
            | DataType::NatParam { default, .. } => *default as u32,
            DataType::BitExpr { default, .. }
            | DataType::LogicExpr { default, .. }
            | DataType::IntExpr { default, .. }
            | DataType::NatExpr { default, .. } => *default as u32,
            DataType::Vec2(inner) => Self::get_type_width(inner) * 2,
            DataType::Vec3(inner) => Self::get_type_width(inner) * 3,
            DataType::Vec4(inner) => Self::get_type_width(inner) * 4,
            _ => 1,
        }
    }

    /// Allocate a temporary signal
    fn alloc_temp_signal(&mut self, width: u32) -> LirSignalId {
        let name = format!("_t{}", self.temp_counter);
        self.temp_counter += 1;
        self.lir.add_signal(name, width)
    }
}

/// Transform a MIR module to LIR
pub fn lower_mir_module_to_lir(module: &Module) -> MirToLirResult {
    let mut transformer = MirToLirTransform::new(&module.name);
    transformer.transform(module)
}

/// Backward-compatible alias
pub fn lower_mir_module_to_word_lir(module: &Module) -> MirToLirResult {
    lower_mir_module_to_lir(module)
}

// ============================================================================
// Hierarchical MIR Traversal
// ============================================================================

use skalp_mir::mir::{Mir, ModuleId, ModuleInstance};
use std::collections::HashSet;

/// Result of hierarchical MIR to LIR transformation
#[derive(Debug)]
pub struct HierarchicalMirToLirResult {
    /// Map from instance path to its LIR result
    pub instances: HashMap<String, InstanceLirResult>,
    /// Top module name
    pub top_module: String,
    /// Instance hierarchy (parent -> children)
    pub hierarchy: HashMap<String, Vec<String>>,
}

/// LIR result for a single instance
#[derive(Debug)]
pub struct InstanceLirResult {
    /// Module name (before specialization)
    pub module_name: String,
    /// The generated LIR
    pub lir_result: MirToLirResult,
    /// Port connections from parent
    pub port_connections: HashMap<String, PortConnectionInfo>,
    /// Child instance paths
    pub children: Vec<String>,
}

/// Information about a port connection
#[derive(Debug, Clone)]
pub enum PortConnectionInfo {
    /// Connected to a signal in parent
    Signal(String),
    /// Connected to a constant value
    Constant(u64),
    /// Connected to another instance's port
    InstancePort(String, String), // (instance_path, port_name)
}

/// Lower entire MIR hierarchy to per-instance LIR
///
/// This function traverses the module hierarchy starting from the top module,
/// creating a separate LIR for each instance. Each instance can be specialized
/// based on its context (constant inputs, unused outputs).
pub fn lower_mir_hierarchical(mir: &Mir) -> HierarchicalMirToLirResult {
    // Build module lookup by ID
    let module_map: HashMap<ModuleId, &Module> = mir
        .modules
        .iter()
        .map(|m| (m.id, m))
        .collect();

    // Find modules that are instantiated (have parents)
    let mut instantiated_modules: HashSet<ModuleId> = HashSet::new();
    for module in &mir.modules {
        for inst in &module.instances {
            instantiated_modules.insert(inst.module);
        }
    }

    // Find top module (not instantiated by any other)
    let top_module = mir.modules
        .iter()
        .find(|m| !instantiated_modules.contains(&m.id))
        .unwrap_or_else(|| &mir.modules[0]);

    let mut result = HierarchicalMirToLirResult {
        instances: HashMap::new(),
        top_module: top_module.name.clone(),
        hierarchy: HashMap::new(),
    };

    // Recursively elaborate instances
    elaborate_instance(
        &module_map,
        top_module,
        "top",
        &HashMap::new(), // No constant inputs at top level
        &mut result,
    );

    result
}

/// Recursively elaborate a module instance
fn elaborate_instance(
    module_map: &HashMap<ModuleId, &Module>,
    module: &Module,
    instance_path: &str,
    parent_connections: &HashMap<String, PortConnectionInfo>,
    result: &mut HierarchicalMirToLirResult,
) {
    // Lower this module to LIR
    let lir_result = lower_mir_module_to_lir(module);

    // Collect child instance paths
    let mut children: Vec<String> = Vec::new();

    // Process child instances
    for inst in &module.instances {
        let child_path = format!("{}.{}", instance_path, inst.name);
        children.push(child_path.clone());

        if let Some(child_module) = module_map.get(&inst.module) {
            // Extract connection info
            let child_connections = extract_connection_info(&inst.connections);

            // Recursively elaborate child
            elaborate_instance(
                module_map,
                child_module,
                &child_path,
                &child_connections,
                result,
            );
        }
    }

    // Record hierarchy
    result.hierarchy.insert(instance_path.to_string(), children.clone());

    // Store this instance's result
    result.instances.insert(
        instance_path.to_string(),
        InstanceLirResult {
            module_name: module.name.clone(),
            lir_result,
            port_connections: parent_connections.clone(),
            children,
        },
    );
}

/// Extract connection information from port connections
fn extract_connection_info(
    connections: &HashMap<String, Expression>,
) -> HashMap<String, PortConnectionInfo> {
    let mut result = HashMap::new();

    for (port_name, expr) in connections {
        let info = match &expr.kind {
            ExpressionKind::Literal(value) => {
                // Constant connection
                let const_val = value_to_u64(value);
                PortConnectionInfo::Constant(const_val)
            }
            ExpressionKind::Ref(lvalue) => {
                // Signal reference
                let signal_name = lvalue_to_name(lvalue);
                PortConnectionInfo::Signal(signal_name)
            }
            _ => {
                // Complex expression - treat as signal
                PortConnectionInfo::Signal(format!("expr_{}", port_name))
            }
        };
        result.insert(port_name.clone(), info);
    }

    result
}

/// Convert a Value to u64
fn value_to_u64(value: &Value) -> u64 {
    match value {
        Value::Integer(i) => *i as u64,
        Value::BitVector { value, .. } => *value,
        Value::Float(f) => *f as u64,
        _ => 0, // Default for String, HighZ, Unknown
    }
}

/// Convert an LValue to a signal name
fn lvalue_to_name(lvalue: &LValue) -> String {
    match lvalue {
        LValue::Signal(id) => format!("signal_{}", id.0),
        LValue::Port(id) => format!("port_{}", id.0),
        LValue::Variable(id) => format!("var_{}", id.0),
        LValue::BitSelect { base, index: _ } => {
            format!("{}[...]", lvalue_to_name(base))
        }
        LValue::RangeSelect { base, high: _, low: _ } => {
            format!("{}[...]", lvalue_to_name(base))
        }
        LValue::Concat(parts) => {
            parts.iter().map(lvalue_to_name).collect::<Vec<_>>().join("_")
        }
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use skalp_mir::mir::{ContinuousAssign, Module, ModuleId, Port, PortId};

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
                    port_type: DataType::Bit(8),
                    physical_constraints: None,
                    span: None,
                    detection_config: None,
                },
                Port {
                    id: PortId(1),
                    name: "b".to_string(),
                    direction: PortDirection::Input,
                    port_type: DataType::Bit(8),
                    physical_constraints: None,
                    span: None,
                    detection_config: None,
                },
                Port {
                    id: PortId(2),
                    name: "sum".to_string(),
                    direction: PortDirection::Output,
                    port_type: DataType::Bit(8),
                    physical_constraints: None,
                    span: None,
                    detection_config: None,
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
            power_domain_config: None,
            safety_context: None,
        }
    }

    #[test]
    fn test_word_lir_preserves_add() {
        let module = make_simple_module();
        let result = lower_mir_module_to_word_lir(&module);

        // Should have a single Add node (not decomposed into full adders)
        let add_count = result
            .lir
            .nodes
            .iter()
            .filter(|n| matches!(n.op, LirOp::Add { .. }))
            .count();

        assert_eq!(add_count, 1, "Expected single Add node, got {}", add_count);

        // Check that the Add has width 8
        let add_node = result
            .lir
            .nodes
            .iter()
            .find(|n| matches!(n.op, LirOp::Add { .. }))
            .unwrap();

        if let LirOp::Add { width, .. } = &add_node.op {
            assert_eq!(*width, 8, "Expected Add width 8, got {}", width);
        }
    }

    #[test]
    fn test_word_lir_signals_have_width() {
        let module = make_simple_module();
        let result = lower_mir_module_to_word_lir(&module);

        // Input signals should have width 8
        for input_id in &result.lir.inputs {
            let signal = &result.lir.signals[input_id.0 as usize];
            assert_eq!(signal.width, 8, "Input {} should have width 8", signal.name);
        }
    }

    #[test]
    fn test_word_lir_comparison() {
        let module = Module {
            id: ModuleId(0),
            name: "cmp_module".to_string(),
            parameters: Vec::new(),
            ports: vec![
                Port {
                    id: PortId(0),
                    name: "a".to_string(),
                    direction: PortDirection::Input,
                    port_type: DataType::Bit(8),
                    physical_constraints: None,
                    span: None,
                    detection_config: None,
                },
                Port {
                    id: PortId(1),
                    name: "b".to_string(),
                    direction: PortDirection::Input,
                    port_type: DataType::Bit(8),
                    physical_constraints: None,
                    span: None,
                    detection_config: None,
                },
                Port {
                    id: PortId(2),
                    name: "eq".to_string(),
                    direction: PortDirection::Output,
                    port_type: DataType::Bit(1),
                    physical_constraints: None,
                    span: None,
                    detection_config: None,
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
            power_domain_config: None,
            safety_context: None,
        };

        let result = lower_mir_module_to_word_lir(&module);

        // Should have a single Eq node
        let eq_count = result
            .lir
            .nodes
            .iter()
            .filter(|n| matches!(n.op, LirOp::Eq { .. }))
            .count();

        assert_eq!(eq_count, 1, "Expected single Eq node");

        // Eq should have 8-bit operands but 1-bit output
        let eq_node = result
            .lir
            .nodes
            .iter()
            .find(|n| matches!(n.op, LirOp::Eq { .. }))
            .unwrap();

        if let LirOp::Eq { width } = &eq_node.op {
            assert_eq!(*width, 8, "Expected Eq operand width 8");
        }

        // Output should be 1-bit
        let out_signal = &result.lir.signals[eq_node.output.0 as usize];
        assert_eq!(out_signal.width, 1, "Expected 1-bit output");
    }

    #[test]
    fn test_word_lir_mux() {
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
                    detection_config: None,
                },
                Port {
                    id: PortId(1),
                    name: "a".to_string(),
                    direction: PortDirection::Input,
                    port_type: DataType::Bit(16),
                    physical_constraints: None,
                    span: None,
                    detection_config: None,
                },
                Port {
                    id: PortId(2),
                    name: "b".to_string(),
                    direction: PortDirection::Input,
                    port_type: DataType::Bit(16),
                    physical_constraints: None,
                    span: None,
                    detection_config: None,
                },
                Port {
                    id: PortId(3),
                    name: "y".to_string(),
                    direction: PortDirection::Output,
                    port_type: DataType::Bit(16),
                    physical_constraints: None,
                    span: None,
                    detection_config: None,
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
            power_domain_config: None,
            safety_context: None,
        };

        let result = lower_mir_module_to_word_lir(&module);

        // Should have a single Mux2 node with width 16
        let mux_count = result
            .lir
            .nodes
            .iter()
            .filter(|n| matches!(n.op, LirOp::Mux2 { width: 16 }))
            .count();

        assert_eq!(mux_count, 1, "Expected single 16-bit Mux2 node");
    }

    #[test]
    fn test_word_lir_stats() {
        let module = make_simple_module();
        let result = lower_mir_module_to_word_lir(&module);

        assert!(
            result.stats.arithmetic_ops >= 1,
            "Should count arithmetic ops"
        );
        assert!(result.stats.total_nodes >= 1, "Should have nodes");
        assert!(
            result.stats.total_signals >= 3,
            "Should have signals for ports"
        );
    }
}
