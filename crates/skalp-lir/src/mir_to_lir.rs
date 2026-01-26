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

use crate::compiled_ip::CompiledIp;
use crate::lir::{Lir, LirOp, LirSafetyInfo, LirSignalId, LirStats};
use crate::ncl_expand::{expand_to_ncl, NclConfig};
use indexmap::IndexMap;
use skalp_mir::mir::{
    AssignmentKind, BinaryOp, Block, CaseStatement, ContinuousAssign, DataType, EdgeType, Expression,
    ExpressionKind, IfStatement, LValue, Module, PortDirection, PortId, Process, ProcessKind, ReduceOp,
    SafetyContext, SensitivityList, SignalId, Statement, UnaryOp, Value, Variable, VariableId,
};
use std::collections::HashMap;
use tracing::{debug, info, trace};

/// Type alias for LIR cache key: (module_name, is_async_context)
type LirCacheKey = (String, bool);

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

/// Information about a blackbox module (vendor IP, analog macro, etc.)
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct BlackboxInfo {
    /// The IP/cell name (e.g., "PLL_ADV", "IOBUF")
    pub cell_name: String,
    /// Vendor type for RTL generation
    pub vendor: Option<String>,
    /// Input port names in order
    pub inputs: Vec<String>,
    /// Output port names in order
    pub outputs: Vec<String>,
    /// InOut port names (bidirectional)
    pub inouts: Vec<String>,
    /// Port widths (port_name -> width)
    pub port_widths: IndexMap<String, u32>,
    /// Additional parameters for the cell instance
    pub parameters: Vec<(String, String)>,
}

/// Result of MIR to LIR transformation
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MirToLirResult {
    /// The generated word-level LIR
    pub lir: Lir,
    /// Transformation statistics
    pub stats: LirStats,
    /// Warnings generated during transformation
    pub warnings: Vec<String>,
    /// Path to compiled IP file (if this is a blackbox with pre-compiled netlist)
    /// When set, the tech mapper should load the netlist from this file instead of synthesizing
    #[serde(skip_serializing_if = "Option::is_none")]
    pub compiled_ip_path: Option<String>,
    /// Blackbox info (if this module is a vendor IP or analog macro)
    /// When set, the tech mapper should create a blackbox cell instead of synthesizing
    #[serde(skip_serializing_if = "Option::is_none")]
    pub blackbox_info: Option<BlackboxInfo>,
}

/// Backward-compatible type alias
pub type MirToWordLirResult = MirToLirResult;

/// MIR to LIR transformer
pub struct MirToLirTransform {
    /// Output LIR being built
    lir: Lir,
    /// Mapping from PortId to signal ID
    port_to_signal: IndexMap<PortId, LirSignalId>,
    /// Mapping from SignalId to signal ID
    signal_to_lir_signal: IndexMap<SignalId, LirSignalId>,
    /// Mapping from VariableId to signal ID (BUG #150 FIX)
    variable_to_signal: IndexMap<VariableId, LirSignalId>,
    /// Width of each variable (BUG #150 FIX)
    variable_widths: IndexMap<VariableId, u32>,
    /// Width of each port/signal
    port_widths: IndexMap<PortId, u32>,
    signal_widths: IndexMap<SignalId, u32>,
    /// Signedness of each port/signal/variable (for signed comparisons)
    port_is_signed: IndexMap<PortId, bool>,
    signal_is_signed: IndexMap<SignalId, bool>,
    variable_is_signed: IndexMap<VariableId, bool>,
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
    /// Counter for generating unique node names
    node_counter: u32,
}

impl MirToLirTransform {
    /// Create a new transformer
    pub fn new(module_name: &str) -> Self {
        Self {
            lir: Lir::new(module_name.to_string()),
            port_to_signal: IndexMap::new(),
            signal_to_lir_signal: IndexMap::new(),
            variable_to_signal: IndexMap::new(),
            variable_widths: IndexMap::new(),
            port_widths: IndexMap::new(),
            signal_widths: IndexMap::new(),
            port_is_signed: IndexMap::new(),
            signal_is_signed: IndexMap::new(),
            variable_is_signed: IndexMap::new(),
            hierarchy_path: "top".to_string(),
            warnings: Vec::new(),
            clock_signals: Vec::new(),
            reset_signals: Vec::new(),
            temp_counter: 0,
            node_counter: 0,
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

        // Phase 2b: Create signals for all variables (BUG #150 FIX)
        // Variables in MIR are intermediate values from let bindings.
        // They need to be transformed to LIR signals so that subsequent
        // assignments can reference them.
        for variable in &module.variables {
            self.create_variable_signal(variable);
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

        // Phase 6: NCL expansion for async modules
        // Convert synchronous LIR to dual-rail NCL logic
        let final_lir = if module.is_async {
            trace!(
                "⚡ NCL: Expanding module '{}' to dual-rail NCL logic",
                module.name
            );
            let ncl_result = expand_to_ncl(&self.lir, &NclConfig::default());
            trace!(
                "⚡ NCL: Expanded {} signals -> {} dual-rail signals",
                self.lir.signals.len(),
                ncl_result.lir.signals.len()
            );
            ncl_result.lir
        } else {
            self.lir.clone()
        };

        let stats = LirStats::from_lir(&final_lir);

        MirToLirResult {
            lir: final_lir,
            stats,
            warnings: std::mem::take(&mut self.warnings),
            compiled_ip_path: None,
            blackbox_info: None,
        }
    }

    /// Transform with async context propagation
    ///
    /// This is used when a child module is instantiated within an async parent.
    /// The `is_async_context` flag forces NCL expansion even if the module
    /// itself wasn't declared with `async entity`.
    pub fn transform_with_async_context(
        &mut self,
        module: &Module,
        is_async_context: bool,
    ) -> MirToLirResult {
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

        // Phase 2b: Create signals for all variables (BUG #150 FIX)
        for variable in &module.variables {
            self.create_variable_signal(variable);
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

        // Phase 6: NCL expansion for async modules or async context
        // Convert synchronous LIR to dual-rail NCL logic
        // Use OR to combine module's async flag with inherited async context
        let is_async = module.is_async || is_async_context;
        let final_lir = if is_async {
            trace!(
                "⚡ NCL: Expanding module '{}' to dual-rail NCL logic{}",
                module.name,
                if is_async_context && !module.is_async {
                    " (inherited from parent)"
                } else {
                    ""
                }
            );
            let ncl_result = expand_to_ncl(&self.lir, &NclConfig::default());
            trace!(
                "⚡ NCL: Expanded {} signals -> {} dual-rail signals",
                self.lir.signals.len(),
                ncl_result.lir.signals.len()
            );
            ncl_result.lir
        } else {
            self.lir.clone()
        };

        let stats = LirStats::from_lir(&final_lir);

        MirToLirResult {
            lir: final_lir,
            stats,
            warnings: std::mem::take(&mut self.warnings),
            compiled_ip_path: None,
            blackbox_info: None,
        }
    }

    /// Transform without NCL expansion (for optimize-first flow)
    ///
    /// This always produces single-rail LIR, even for async modules.
    /// The dual-rail NCL conversion is done later after gate-level optimization.
    pub fn transform_skip_ncl(&mut self, module: &Module) -> MirToLirResult {
        self.hierarchy_path = module.name.clone();

        // Propagate module-level safety context to LIR
        if let Some(ref ctx) = module.safety_context {
            if ctx.has_safety_annotation() {
                self.lir.module_safety_info = Some(safety_context_to_lir_info(ctx));
            }
        }

        // Phase 1: Create signals for all ports
        for port in &module.ports {
            self.create_port_signal(port);
        }

        // Phase 2: Create signals for all internal signals
        for signal in &module.signals {
            self.create_internal_signal(signal);
        }

        // Phase 2b: Create signals for all variables (BUG #150 FIX)
        // Variables in MIR are intermediate values from let bindings.
        // They need to be transformed to LIR signals so that subsequent
        // assignments can reference them.
        for variable in &module.variables {
            self.create_variable_signal(variable);
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

        // Phase 6: SKIP NCL expansion - keep as single-rail LIR
        // The dual-rail conversion will be done after gate-level optimization
        if module.is_async {
            trace!(
                "⚡ NCL Optimize-First: Skipping NCL expansion for async module '{}' (will convert after optimization)",
                module.name
            );
        }

        let stats = LirStats::from_lir(&self.lir);

        MirToLirResult {
            lir: self.lir.clone(),
            stats,
            warnings: std::mem::take(&mut self.warnings),
            compiled_ip_path: None,
            blackbox_info: None,
        }
    }

    /// Create a signal for a port
    fn create_port_signal(&mut self, port: &skalp_mir::mir::Port) {
        let width = Self::get_type_width(&port.port_type);
        self.port_widths.insert(port.id, width);
        // Track signedness for signed comparison operations
        let is_signed = matches!(
            port.port_type,
            DataType::Int(_) | DataType::IntParam { .. } | DataType::IntExpr { .. }
        );
        self.port_is_signed.insert(port.id, is_signed);

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
        // Track signedness for signed comparison operations
        let is_signed = matches!(
            signal.signal_type,
            DataType::Int(_) | DataType::IntParam { .. } | DataType::IntExpr { .. }
        );
        self.signal_is_signed.insert(signal.id, is_signed);

        let signal_id = self.lir.add_signal(signal.name.clone(), width);
        self.signal_to_lir_signal.insert(signal.id, signal_id);

        // Propagate detection signal flag from MIR internal signals
        // This is critical for hierarchical flattening where sub-module output ports
        // marked with #[detection_signal] become internal signals in the flattened design
        if signal.is_detection_signal() {
            trace!(
                "✅ [WORD_LIR_DETECTION] Marking internal signal '{}' as detection",
                signal.name
            );
            self.lir.mark_as_detection(signal_id);
        }
    }

    /// Create a signal for a variable (BUG #150 FIX)
    ///
    /// Variables in MIR are intermediate values from let bindings.
    /// They need to be converted to LIR signals so that assignments
    /// to variables can be properly converted and subsequent references
    /// to those variables will resolve correctly.
    ///
    /// BUG #158 FIX: If the variable has an initial value (from `let a = 100`),
    /// we must create a constant LIR node to drive the signal. Otherwise, the
    /// signal will be floating and won't synthesize to proper TIE cells.
    fn create_variable_signal(&mut self, variable: &Variable) {
        let width = Self::get_type_width(&variable.var_type);
        self.variable_widths.insert(variable.id, width);
        // Track signedness for signed comparison operations
        let is_signed = matches!(
            variable.var_type,
            DataType::Int(_) | DataType::IntParam { .. } | DataType::IntExpr { .. }
        );
        self.variable_is_signed.insert(variable.id, is_signed);

        // Create a signal for the variable
        let signal_id = self.lir.add_signal(format!("_v_{}", variable.name), width);
        self.variable_to_signal.insert(variable.id, signal_id);

        // BUG #158 FIX: If the variable has an initial value, create a constant
        // to drive the signal. This ensures constants like `let a = 100` get
        // synthesized to TIE_HIGH/TIE_LOW cells in the gate netlist.
        if let Some(ref initial_value) = variable.initial {
            let const_val = match initial_value {
                Value::Integer(i) => *i as u64,
                Value::BitVector { value, .. } => *value,
                // BUG #165 FIX: Use width to determine if we should convert to f32 or f64
                Value::Float(f) => {
                    if width <= 32 {
                        (*f as f32).to_bits() as u64
                    } else {
                        f.to_bits()
                    }
                }
                _ => 0,
            };

            self.lir.add_node(
                LirOp::Constant {
                    width,
                    value: const_val,
                },
                vec![],
                signal_id,
                format!("{}.var_{}_init", self.hierarchy_path, variable.name),
            );
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
            ProcessKind::Async => {
                // NCL async process - transform as combinational for now
                // Phase 4 (ncl_expand.rs) will convert to proper NCL gates
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
        // BUG #237 FIX: Collect leading assignments (before any if/case) as defaults
        // These serve as implicit defaults for conditionals that don't assign to a target
        // Key is the LValue converted to a comparable form
        let mut sequential_defaults: Vec<(LValue, Expression)> = Vec::new();
        let mut past_leading_section = false;

        for stmt in &block.statements {
            match stmt {
                Statement::Assignment(assign) if !past_leading_section => {
                    if matches!(assign.kind, AssignmentKind::NonBlocking) {
                        // Track this as a default value for this target
                        // Remove any previous default for this target
                        sequential_defaults.retain(|(lv, _)| lv != &assign.lhs);
                        sequential_defaults.push((assign.lhs.clone(), assign.rhs.clone()));
                    }
                }
                Statement::If(_) | Statement::Case(_) => {
                    // We've hit a conditional - mark end of leading section
                    past_leading_section = true;
                }
                _ => {}
            }
        }

        // Now process statements with the context
        for stmt in &block.statements {
            self.transform_sequential_statement_with_defaults(
                stmt,
                clock_signal,
                reset_signal,
                &sequential_defaults,
            );
        }
    }

    /// Transform a sequential statement with context of preceding defaults
    fn transform_sequential_statement_with_defaults(
        &mut self,
        stmt: &Statement,
        clock_signal: Option<LirSignalId>,
        reset_signal: Option<LirSignalId>,
        sequential_defaults: &[(LValue, Expression)],
    ) {
        match stmt {
            Statement::Case(case_stmt) => {
                // BUG #237 FIX: Pass sequential defaults to case handler
                self.transform_sequential_case_with_defaults(
                    case_stmt,
                    clock_signal,
                    reset_signal,
                    sequential_defaults,
                );
            }
            _ => {
                // Other statements use the original handler
                self.transform_sequential_statement(stmt, clock_signal, reset_signal);
            }
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

                // Collect assignments from each branch (direct only, not nested)
                let then_assigns = Self::collect_assignments(&if_stmt.then_block);
                let else_assigns = if let Some(ref else_block) = if_stmt.else_block {
                    Self::collect_assignments(else_block)
                } else {
                    Vec::new()
                };

                // BUG #238 FIX: Get ALL targets including those in nested ifs
                // This ensures we handle targets assigned in sibling nested branches
                let all_then_assigns = Self::collect_all_assignments_recursive(&if_stmt.then_block);
                let all_else_assigns = if let Some(ref else_block) = if_stmt.else_block {
                    Self::collect_all_assignments_recursive(else_block)
                } else {
                    Vec::new()
                };

                // Get all unique target lvalues (including nested)
                let mut all_targets: Vec<LValue> = Vec::new();
                for (lv, _) in &all_then_assigns {
                    if !all_targets.contains(lv) {
                        all_targets.push(lv.clone());
                    }
                }
                for (lv, _) in &all_else_assigns {
                    if !all_targets.contains(lv) {
                        all_targets.push(lv.clone());
                    }
                }

                // Track which targets we handle at this level
                let mut handled_targets: Vec<LValue> = Vec::new();

                for target in &all_targets {
                    let target_signal = self.get_lvalue_signal(target);
                    let target_width = self.get_lvalue_width(target);

                    // Get then and else expressions (direct assignments only)
                    let then_expr = Self::find_assignment_expr(&then_assigns, target);
                    let else_expr = Self::find_assignment_expr(&else_assigns, target);

                    // BUG #238 FIX: Check if we should handle this target here
                    // We must handle it here if:
                    // 1. Target is directly assigned in then or else branch, OR
                    // 2. Target is assigned in nested ifs in BOTH branches (to avoid multi-driver)
                    let then_has_nested = Self::block_has_nested_if_for_target(&if_stmt.then_block, target);
                    let else_has_nested = if let Some(ref else_block) = if_stmt.else_block {
                        Self::block_has_nested_if_for_target(else_block, target)
                    } else {
                        false
                    };

                    let directly_assigned = then_expr.is_some() || else_expr.is_some();
                    let in_sibling_nested = then_has_nested && else_has_nested;
                    let in_only_else_nested = !then_has_nested && else_has_nested && then_expr.is_none();

                    // Skip if only in nested else ifs - let them handle it
                    // (But only if not directly assigned in then branch!)
                    if in_only_else_nested {
                        continue;
                    }

                    // If in sibling nested ifs, we MUST handle here to avoid multi-driver
                    // Collect all conditional paths and build a combined mux chain
                    if in_sibling_nested && !directly_assigned {
                        handled_targets.push(target.clone());

                        // Collect all conditional paths through the entire if tree
                        let mut paths = Vec::new();

                        // Paths from then branch
                        let then_paths = Self::collect_conditional_paths_for_target(
                            &if_stmt.then_block,
                            target,
                            Some(&if_stmt.condition),
                        );
                        paths.extend(then_paths);

                        // Paths from else branch
                        if let Some(ref else_block) = if_stmt.else_block {
                            let negated_cond = Expression {
                                kind: ExpressionKind::Unary {
                                    op: UnaryOp::Not,
                                    operand: Box::new(if_stmt.condition.clone()),
                                },
                                ty: skalp_frontend::types::Type::Bool,
                            span: None,
                            };
                            let else_paths = Self::collect_conditional_paths_for_target(
                                else_block,
                                target,
                                Some(&negated_cond),
                            );
                            paths.extend(else_paths);
                        }

                        // Build priority mux chain: first matching condition wins
                        // Start with feedback (keep current value) as the default
                        let mut current_value = target_signal;

                        // Process paths in reverse order so earlier paths have higher priority
                        for (condition, expr) in paths.into_iter().rev() {
                            let expr_signal = self.transform_expression(&expr, target_width);

                            if let Some(cond) = condition {
                                let cond_signal = self.transform_expression(&cond, 1);
                                let mux_out = self.alloc_temp_signal(target_width);
                                self.lir.add_node(
                                    LirOp::Mux2 { width: target_width },
                                    vec![cond_signal, current_value, expr_signal],
                                    mux_out,
                                    format!("{}.mux", self.hierarchy_path),
                                );
                                current_value = mux_out;
                            } else {
                                // Unconditional - this becomes the new base value
                                current_value = expr_signal;
                            }
                        }

                        // Create ONE register for this target
                        let reg_op = LirOp::Reg {
                            width: target_width,
                            has_enable: false,
                            has_reset: reset_signal.is_some(),
                            reset_value: Some(0),
                        };

                        if let Some(clk) = clock_signal {
                            self.lir.add_seq_node(
                                reg_op,
                                vec![current_value],
                                target_signal,
                                format!("{}.reg", self.hierarchy_path),
                                clk,
                                reset_signal,
                            );
                        } else {
                            self.lir.add_node(
                                reg_op,
                                vec![current_value],
                                target_signal,
                                format!("{}.reg", self.hierarchy_path),
                            );
                        }
                        continue;
                    }

                    // Check for SDFF pattern:
                    // - Condition is reset signal
                    // - Then branch (reset case) is a constant
                    // - There's an else branch with the actual next value
                    let sdff_pattern = if is_reset_condition && (else_expr.is_some() || else_has_nested) {
                        then_expr.and_then(Self::try_extract_constant)
                    } else {
                        None
                    };

                    if let Some(reset_value) = sdff_pattern {
                        handled_targets.push(target.clone());

                        // SDFF pattern detected: skip mux, use integrated sync reset
                        // For nested else ifs, we need to collect all paths from else branch
                        let else_signal = if else_has_nested {
                            // Collect paths from else branch and build mux chain
                            if let Some(ref else_block) = if_stmt.else_block {
                                let else_paths = Self::collect_conditional_paths_for_target(
                                    else_block,
                                    target,
                                    None,
                                );

                                let mut current_value = target_signal; // Feedback as default

                                for (condition, expr) in else_paths.into_iter().rev() {
                                    let expr_signal = self.transform_expression(&expr, target_width);

                                    if let Some(cond) = condition {
                                        let cond_signal = self.transform_expression(&cond, 1);
                                        let mux_out = self.alloc_temp_signal(target_width);
                                        self.lir.add_node(
                                            LirOp::Mux2 { width: target_width },
                                            vec![cond_signal, current_value, expr_signal],
                                            mux_out,
                                            format!("{}.mux", self.hierarchy_path),
                                        );
                                        current_value = mux_out;
                                    } else {
                                        current_value = expr_signal;
                                    }
                                }
                                current_value
                            } else {
                                target_signal
                            }
                        } else if let Some(expr) = else_expr {
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
                    } else if directly_assigned {
                        handled_targets.push(target.clone());

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

                // BUG #238 FIX: Recursively process nested if statements, but SKIP targets
                // that we've already handled at this level to avoid multi-driver
                // We need a different approach: process nested ifs but track handled targets
                for stmt in &if_stmt.then_block.statements {
                    if let Statement::If(nested_if) = stmt {
                        self.transform_sequential_if_excluding_targets(
                            nested_if,
                            clock_signal,
                            reset_signal,
                            &handled_targets,
                        );
                    }
                }
                if let Some(ref else_block) = if_stmt.else_block {
                    for stmt in &else_block.statements {
                        if let Statement::If(nested_if) = stmt {
                            self.transform_sequential_if_excluding_targets(
                                nested_if,
                                clock_signal,
                                reset_signal,
                                &handled_targets,
                            );
                        }
                    }
                }
            }
            Statement::Block(block) => {
                self.transform_sequential_block(block, clock_signal, reset_signal);
            }
            Statement::Case(case_stmt) => {
                // BUG #237 FIX: Handle Case/Match statements in LIR lowering
                // Convert case statement to chain of muxes for state machine synthesis
                // Note: When called from transform_sequential_statement_with_defaults,
                // that version handles defaults. This is a fallback for direct calls.
                self.transform_sequential_case_with_defaults(
                    case_stmt,
                    clock_signal,
                    reset_signal,
                    &[],
                );
            }
            _ => {
                self.transform_combinational_statement(stmt);
            }
        }
    }

    /// Transform a Case statement in sequential context with preceding defaults
    /// Creates a mux chain for state machine transitions
    fn transform_sequential_case_with_defaults(
        &mut self,
        case_stmt: &CaseStatement,
        clock_signal: Option<LirSignalId>,
        reset_signal: Option<LirSignalId>,
        sequential_defaults: &[(LValue, Expression)],
    ) {
        // Transform the case expression (e.g., state_reg)
        let case_expr_width = self.infer_expression_width(&case_stmt.expr);
        let case_expr_signal = self.transform_expression(&case_stmt.expr, case_expr_width);

        // Collect all assignments from all case arms and default
        let mut all_targets: Vec<LValue> = Vec::new();
        let mut case_arms_assignments: Vec<(Vec<Expression>, Vec<(LValue, Expression)>)> = Vec::new();

        for item in &case_stmt.items {
            let assigns = Self::collect_assignments(&item.block);
            for (lv, _) in &assigns {
                if !all_targets.contains(lv) {
                    all_targets.push(lv.clone());
                }
            }
            case_arms_assignments.push((item.values.clone(), assigns));
        }

        let default_assigns = if let Some(ref default_block) = case_stmt.default {
            let assigns = Self::collect_assignments(default_block);
            for (lv, _) in &assigns {
                if !all_targets.contains(lv) {
                    all_targets.push(lv.clone());
                }
            }
            assigns
        } else {
            Vec::new()
        };

        // BUG #237 FIX: Also include targets from sequential_defaults that might not
        // appear in case arms but have a leading assignment
        for (lv, _) in sequential_defaults.iter() {
            if !all_targets.contains(lv) {
                all_targets.push(lv.clone());
            }
        }

        // For each target signal, build a mux chain
        for target in &all_targets {
            let target_signal = self.get_lvalue_signal(target);
            let target_width = self.get_lvalue_width(target);

            // BUG #237 FIX: Determine default value with priority:
            // 1. Explicit default block assignment
            // 2. Sequential default (assignment before the case)
            // 3. Feedback (keep current value)
            let default_signal = if let Some(expr) = Self::find_assignment_expr(&default_assigns, target) {
                self.transform_expression(expr, target_width)
            } else if let Some(expr) = Self::find_assignment_expr(sequential_defaults, target) {
                self.transform_expression(expr, target_width)
            } else {
                target_signal // Feedback: keep current value
            };

            // Build mux chain from last case to first (so first has priority)
            let mut current_result = default_signal;

            for (case_values, assigns) in case_arms_assignments.iter().rev() {
                // Get the expression for this target in this arm (or use current as feedback)
                let arm_signal = if let Some(expr) = Self::find_assignment_expr(assigns, target) {
                    self.transform_expression(expr, target_width)
                } else {
                    current_result // Not assigned in this arm, use previous result
                };

                // Build condition: case_expr == value (for each value in values list)
                if case_values.is_empty() {
                    continue;
                }

                let condition = if case_values.len() == 1 {
                    // Single value match: case_expr == value
                    let val_signal = self.transform_expression(&case_values[0], case_expr_width);
                    let eq_out = self.alloc_temp_signal(1);
                    self.lir.add_node(
                        LirOp::Eq { width: case_expr_width },
                        vec![case_expr_signal, val_signal],
                        eq_out,
                        format!("{}.case_eq", self.hierarchy_path),
                    );
                    eq_out
                } else {
                    // Multiple values: OR of all comparisons
                    let mut or_result = {
                        let val_signal = self.transform_expression(&case_values[0], case_expr_width);
                        let eq_out = self.alloc_temp_signal(1);
                        self.lir.add_node(
                            LirOp::Eq { width: case_expr_width },
                            vec![case_expr_signal, val_signal],
                            eq_out,
                            format!("{}.case_eq", self.hierarchy_path),
                        );
                        eq_out
                    };
                    for value in &case_values[1..] {
                        let val_signal = self.transform_expression(value, case_expr_width);
                        let eq_out = self.alloc_temp_signal(1);
                        self.lir.add_node(
                            LirOp::Eq { width: case_expr_width },
                            vec![case_expr_signal, val_signal],
                            eq_out,
                            format!("{}.case_eq", self.hierarchy_path),
                        );
                        let or_out = self.alloc_temp_signal(1);
                        self.lir.add_node(
                            LirOp::Or { width: 1 },
                            vec![or_result, eq_out],
                            or_out,
                            format!("{}.case_or", self.hierarchy_path),
                        );
                        or_result = or_out;
                    }
                    or_result
                };

                // Create mux: if condition then arm_signal else current_result
                let mux_out = self.alloc_temp_signal(target_width);
                self.lir.add_node(
                    LirOp::Mux2 { width: target_width },
                    vec![condition, current_result, arm_signal],
                    mux_out,
                    format!("{}.case_mux", self.hierarchy_path),
                );
                current_result = mux_out;
            }

            // Create register for the final mux result
            let reg_op = LirOp::Reg {
                width: target_width,
                has_enable: false,
                has_reset: reset_signal.is_some(),
                reset_value: Some(0),
            };

            if let Some(clk) = clock_signal {
                self.lir.add_seq_node(
                    reg_op,
                    vec![current_result],
                    target_signal,
                    format!("{}.case_reg", self.hierarchy_path),
                    clk,
                    reset_signal,
                );
            } else {
                self.lir.add_node(
                    reg_op,
                    vec![current_result],
                    target_signal,
                    format!("{}.case_reg", self.hierarchy_path),
                );
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
                Statement::Case(case_stmt) => {
                    // Collect assignments from all case arms
                    for item in &case_stmt.items {
                        assigns.extend(Self::collect_assignments(&item.block));
                    }
                    if let Some(ref default_block) = case_stmt.default {
                        assigns.extend(Self::collect_assignments(default_block));
                    }
                }
                _ => {}
            }
        }
        assigns
    }

    /// Check if a block contains nested if/case statements that assign to a target
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
                Statement::Case(case_stmt) => {
                    // BUG #238 FIX: Check if case statement assigns to target
                    for item in &case_stmt.items {
                        let arm_assigns = Self::collect_all_assignments_recursive(&item.block);
                        if arm_assigns.iter().any(|(lv, _)| lv == target) {
                            return true;
                        }
                    }
                    if let Some(ref default_block) = case_stmt.default {
                        let default_assigns = Self::collect_all_assignments_recursive(default_block);
                        if default_assigns.iter().any(|(lv, _)| lv == target) {
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

    /// Recursively collect all assignments from a block, including those in nested ifs/cases
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
                Statement::Case(case_stmt) => {
                    for item in &case_stmt.items {
                        assigns.extend(Self::collect_all_assignments_recursive(&item.block));
                    }
                    if let Some(ref default_block) = case_stmt.default {
                        assigns.extend(Self::collect_all_assignments_recursive(default_block));
                    }
                }
                _ => {}
            }
        }
        assigns
    }

    /// BUG #238 FIX: Collect all conditional paths for a specific target through nested ifs
    /// Returns a list of (condition, expression) pairs in priority order (first match wins)
    fn collect_conditional_paths_for_target(
        block: &Block,
        target: &LValue,
        outer_condition: Option<&Expression>,
    ) -> Vec<(Option<Expression>, Expression)> {
        let mut paths = Vec::new();

        for stmt in &block.statements {
            match stmt {
                Statement::Assignment(assign)
                    if matches!(assign.kind, AssignmentKind::NonBlocking) && &assign.lhs == target =>
                {
                    // Direct assignment to target - add as a path
                    paths.push((outer_condition.cloned(), assign.rhs.clone()));
                }
                Statement::If(if_stmt) => {
                    // Build the combined condition for the then branch
                    let then_condition = if let Some(outer) = outer_condition {
                        Some(Expression {
                            kind: ExpressionKind::Binary {
                                op: BinaryOp::And,
                                left: Box::new(outer.clone()),
                                right: Box::new(if_stmt.condition.clone()),
                            },
                            ty: skalp_frontend::types::Type::Bool,
                            span: None,
                        })
                    } else {
                        Some(if_stmt.condition.clone())
                    };

                    // Collect paths from then branch
                    paths.extend(Self::collect_conditional_paths_for_target(
                        &if_stmt.then_block,
                        target,
                        then_condition.as_ref(),
                    ));

                    // Build the combined condition for the else branch
                    if let Some(ref else_block) = if_stmt.else_block {
                        let negated_condition = Expression {
                            kind: ExpressionKind::Unary {
                                op: UnaryOp::Not,
                                operand: Box::new(if_stmt.condition.clone()),
                            },
                            ty: skalp_frontend::types::Type::Bool,
                            span: None,
                        };

                        let else_condition = if let Some(outer) = outer_condition {
                            Some(Expression {
                                kind: ExpressionKind::Binary {
                                    op: BinaryOp::And,
                                    left: Box::new(outer.clone()),
                                    right: Box::new(negated_condition),
                                },
                                ty: skalp_frontend::types::Type::Bool,
                            span: None,
                            })
                        } else {
                            Some(negated_condition)
                        };

                        paths.extend(Self::collect_conditional_paths_for_target(
                            else_block,
                            target,
                            else_condition.as_ref(),
                        ));
                    }
                }
                Statement::Block(inner_block) => {
                    paths.extend(Self::collect_conditional_paths_for_target(
                        inner_block,
                        target,
                        outer_condition,
                    ));
                }
                Statement::Case(case_stmt) => {
                    // BUG #238 FIX: Handle case statements in path collection
                    // For each case arm, build condition (outer AND case_expr == arm_value)
                    for item in &case_stmt.items {
                        for value in &item.values {
                            // Build equality condition: case_expr == value
                            let eq_condition = Expression {
                                kind: ExpressionKind::Binary {
                                    op: BinaryOp::Equal,
                                    left: Box::new(case_stmt.expr.clone()),
                                    right: Box::new(value.clone()),
                                },
                                ty: skalp_frontend::types::Type::Bool,
                                span: None,
                            };

                            // Combine with outer condition
                            let combined_condition = if let Some(outer) = outer_condition {
                                Some(Expression {
                                    kind: ExpressionKind::Binary {
                                        op: BinaryOp::And,
                                        left: Box::new(outer.clone()),
                                        right: Box::new(eq_condition),
                                    },
                                    ty: skalp_frontend::types::Type::Bool,
                                    span: None,
                                })
                            } else {
                                Some(eq_condition)
                            };

                            // Collect paths from this arm
                            paths.extend(Self::collect_conditional_paths_for_target(
                                &item.block,
                                target,
                                combined_condition.as_ref(),
                            ));
                        }
                    }

                    // Handle default block - it matches when no other arm matches
                    // For simplicity, we'll treat default as having the outer condition
                    // (proper implementation would negate all arm conditions)
                    if let Some(ref default_block) = case_stmt.default {
                        paths.extend(Self::collect_conditional_paths_for_target(
                            default_block,
                            target,
                            outer_condition,
                        ));
                    }
                }
                _ => {}
            }
        }

        paths
    }

    /// BUG #238 FIX: Process a nested if statement while excluding targets already handled
    fn transform_sequential_if_excluding_targets(
        &mut self,
        if_stmt: &IfStatement,
        clock_signal: Option<LirSignalId>,
        reset_signal: Option<LirSignalId>,
        excluded_targets: &[LValue],
    ) {
        // Collect assignments from each branch (direct only)
        let then_assigns = Self::collect_assignments(&if_stmt.then_block);
        let else_assigns = if let Some(ref else_block) = if_stmt.else_block {
            Self::collect_assignments(else_block)
        } else {
            Vec::new()
        };

        // Get all target lvalues (excluding already-handled ones)
        let mut all_targets: Vec<LValue> = Vec::new();
        for (lv, _) in &then_assigns {
            if !all_targets.contains(lv) && !excluded_targets.contains(lv) {
                all_targets.push(lv.clone());
            }
        }
        for (lv, _) in &else_assigns {
            if !all_targets.contains(lv) && !excluded_targets.contains(lv) {
                all_targets.push(lv.clone());
            }
        }

        // Track new handled targets at this level
        let mut newly_handled: Vec<LValue> = Vec::new();

        for target in &all_targets {
            let target_signal = self.get_lvalue_signal(target);
            let target_width = self.get_lvalue_width(target);

            let then_expr = Self::find_assignment_expr(&then_assigns, target);
            let else_expr = Self::find_assignment_expr(&else_assigns, target);

            // Check for nested ifs
            let else_has_nested = if let Some(ref else_block) = if_stmt.else_block {
                Self::block_has_nested_if_for_target(else_block, target)
            } else {
                false
            };

            // Skip if only in nested else (will be handled by recursion)
            if else_has_nested && then_expr.is_none() && else_expr.is_none() {
                continue;
            }

            newly_handled.push(target.clone());

            // Standard pattern: create mux + register
            let cond_signal = self.transform_expression(&if_stmt.condition, 1);

            let then_signal = if let Some(expr) = then_expr {
                self.transform_expression(expr, target_width)
            } else {
                target_signal
            };

            let else_signal = if let Some(expr) = else_expr {
                self.transform_expression(expr, target_width)
            } else {
                target_signal
            };

            let mux_out = self.alloc_temp_signal(target_width);
            self.lir.add_node(
                LirOp::Mux2 { width: target_width },
                vec![cond_signal, else_signal, then_signal],
                mux_out,
                format!("{}.mux", self.hierarchy_path),
            );

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

        // Combine excluded targets with newly handled ones for recursion
        let mut combined_excluded = excluded_targets.to_vec();
        combined_excluded.extend(newly_handled);

        // Recurse into nested ifs
        for stmt in &if_stmt.then_block.statements {
            if let Statement::If(nested_if) = stmt {
                self.transform_sequential_if_excluding_targets(
                    nested_if,
                    clock_signal,
                    reset_signal,
                    &combined_excluded,
                );
            }
        }
        if let Some(ref else_block) = if_stmt.else_block {
            for stmt in &else_block.statements {
                if let Statement::If(nested_if) = stmt {
                    self.transform_sequential_if_excluding_targets(
                        nested_if,
                        clock_signal,
                        reset_signal,
                        &combined_excluded,
                    );
                }
            }
        }
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

                for e in exprs.iter() {
                    let w = self.infer_expression_width(e);
                    widths.push(w);
                    let sig = self.transform_expression(e, w);
                    signals.push(sig);
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
            ExpressionKind::Cast { expr, target_type } => {
                // Get source width and target width
                let source_width = self.infer_expression_width(expr);
                let target_width = self.get_datatype_width(target_type);

                // Transform the inner expression
                let source_signal = self.transform_expression(expr, source_width);

                match target_width.cmp(&source_width) {
                    std::cmp::Ordering::Equal => {
                        // Same width - just pass through
                        source_signal
                    }
                    std::cmp::Ordering::Greater => {
                        // Widening - use ZeroExtend (unsigned) or SignExtend (signed)
                        let out = self.alloc_temp_signal(target_width);
                        let is_signed = matches!(
                            target_type,
                            DataType::Int(_)
                                | DataType::IntParam { .. }
                                | DataType::IntExpr { .. }
                                | DataType::Float16
                                | DataType::Float32
                                | DataType::Float64
                        );

                        if is_signed {
                            self.lir.add_node(
                                LirOp::SignExtend {
                                    from: source_width,
                                    to: target_width,
                                },
                                vec![source_signal],
                                out,
                                format!("{}.sext", self.hierarchy_path),
                            );
                        } else {
                            self.lir.add_node(
                                LirOp::ZeroExtend {
                                    from: source_width,
                                    to: target_width,
                                },
                                vec![source_signal],
                                out,
                                format!("{}.zext", self.hierarchy_path),
                            );
                        }
                        out
                    }
                    std::cmp::Ordering::Less => {
                        // Narrowing - use RangeSelect (truncation)
                        let out = self.alloc_temp_signal(target_width);
                        self.lir.add_node(
                            LirOp::RangeSelect {
                                width: source_width,
                                high: target_width - 1,
                                low: 0,
                            },
                            vec![source_signal],
                            out,
                            format!("{}.trunc", self.hierarchy_path),
                        );
                        out
                    }
                }
            }
            // BUG #186 FIX: Handle tuple field access for async entities
            // TupleFieldAccess extracts an element from a tuple (Concat of signals)
            // e.g., let (x, y, z) = func() where func returns (a, b, c)
            ExpressionKind::TupleFieldAccess { base, index } => {
                trace!(
                    "🔍 TUPLE_FIELD_ACCESS: index={}, expected_width={}, base.kind={:?}",
                    index,
                    expected_width,
                    std::mem::discriminant(&base.kind)
                );

                // Get the base signal (should be a Concat representing the tuple)
                let base_width = self.infer_expression_width(base);
                let base_signal = self.transform_expression(base, base_width);

                trace!(
                    "🔍 TUPLE_FIELD_ACCESS: base_width={}, base_signal={:?}",
                    base_width,
                    base_signal
                );

                // For a 3-element tuple of 32-bit values (total 96 bits):
                // - element 0 is at bits [31:0] (LSB)
                // - element 1 is at bits [63:32]
                // - element 2 is at bits [95:64] (MSB)
                // Each element is `expected_width` bits wide
                let low = (*index as u32) * expected_width;
                let high = low + expected_width - 1;

                let out = self.alloc_temp_signal(expected_width);
                self.lir.add_node(
                    LirOp::RangeSelect {
                        width: base_width,
                        high,
                        low,
                    },
                    vec![base_signal],
                    out,
                    format!("{}.tuple_{}", self.hierarchy_path, index),
                );
                out
            }
            // Handle function calls that weren't inlined
            ExpressionKind::FunctionCall { name, args } => {
                self.warnings.push(format!(
                    "Function call '{}' with {} args not inlined - treating as zero",
                    name,
                    args.len()
                ));
                self.create_constant(&Value::Integer(0), expected_width)
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

        // For arithmetic operations, use expected_width for operands to ensure
        // proper width propagation from assignment target (e.g., bit[32] x = 5 + 3
        // should compute 5 + 3 as 32-bit, not 3-bit which would overflow)
        let arithmetic_operand_width = expected_width.max(operand_width);

        let (left_signal, right_signal, word_op, result_width) = match op {
            // Arithmetic - use expected_width for operands
            BinaryOp::Add => {
                let left_sig = self.transform_expression(left, arithmetic_operand_width);
                let right_sig = self.transform_expression(right, arithmetic_operand_width);
                (
                    left_sig,
                    right_sig,
                    LirOp::Add {
                        width: arithmetic_operand_width,
                        has_carry: false, // No extra carry needed, width already accounts for it
                    },
                    expected_width,
                )
            }
            BinaryOp::Sub => {
                let left_sig = self.transform_expression(left, arithmetic_operand_width);
                let right_sig = self.transform_expression(right, arithmetic_operand_width);
                (
                    left_sig,
                    right_sig,
                    LirOp::Sub {
                        width: arithmetic_operand_width,
                        has_borrow: false,
                    },
                    expected_width,
                )
            }
            BinaryOp::Mul => {
                // For multiplication, use the natural operand width (not widened to expected_width)
                // because the multiplier produces a wider result from narrower inputs.
                // E.g., 4-bit * 4-bit = 8-bit uses a 4x4 multiplier, not an 8x8 multiplier.
                let left_sig = self.transform_expression(left, operand_width);
                let right_sig = self.transform_expression(right, operand_width);
                (
                    left_sig,
                    right_sig,
                    LirOp::Mul {
                        width: operand_width,
                        result_width: expected_width,
                    },
                    expected_width,
                )
            }

            // Bitwise logic - use inferred operand width
            BinaryOp::And | BinaryOp::BitwiseAnd | BinaryOp::LogicalAnd => {
                let left_sig = self.transform_expression(left, operand_width);
                let right_sig = self.transform_expression(right, operand_width);
                (
                    left_sig,
                    right_sig,
                    LirOp::And {
                        width: operand_width,
                    },
                    operand_width,
                )
            }
            BinaryOp::Or | BinaryOp::BitwiseOr | BinaryOp::LogicalOr => {
                let left_sig = self.transform_expression(left, operand_width);
                let right_sig = self.transform_expression(right, operand_width);
                (
                    left_sig,
                    right_sig,
                    LirOp::Or {
                        width: operand_width,
                    },
                    operand_width,
                )
            }
            BinaryOp::Xor | BinaryOp::BitwiseXor => {
                let left_sig = self.transform_expression(left, operand_width);
                let right_sig = self.transform_expression(right, operand_width);
                (
                    left_sig,
                    right_sig,
                    LirOp::Xor {
                        width: operand_width,
                    },
                    operand_width,
                )
            }

            // Comparison (result is 1 bit)
            BinaryOp::Equal => {
                let left_sig = self.transform_expression(left, operand_width);
                let right_sig = self.transform_expression(right, operand_width);
                (
                    left_sig,
                    right_sig,
                    LirOp::Eq {
                        width: operand_width,
                    },
                    1,
                )
            }
            BinaryOp::NotEqual => {
                let left_sig = self.transform_expression(left, operand_width);
                let right_sig = self.transform_expression(right, operand_width);
                (
                    left_sig,
                    right_sig,
                    LirOp::Ne {
                        width: operand_width,
                    },
                    1,
                )
            }
            BinaryOp::Less => {
                let left_sig = self.transform_expression(left, operand_width);
                let right_sig = self.transform_expression(right, operand_width);
                // Use signed comparison if either operand is signed
                let is_signed =
                    self.infer_expression_is_signed(left) || self.infer_expression_is_signed(right);
                let op = if is_signed {
                    LirOp::Slt {
                        width: operand_width,
                    }
                } else {
                    LirOp::Lt {
                        width: operand_width,
                    }
                };
                (left_sig, right_sig, op, 1)
            }
            BinaryOp::LessEqual => {
                let left_sig = self.transform_expression(left, operand_width);
                let right_sig = self.transform_expression(right, operand_width);
                // Use signed comparison if either operand is signed
                let is_signed =
                    self.infer_expression_is_signed(left) || self.infer_expression_is_signed(right);
                let op = if is_signed {
                    LirOp::Sle {
                        width: operand_width,
                    }
                } else {
                    LirOp::Le {
                        width: operand_width,
                    }
                };
                (left_sig, right_sig, op, 1)
            }
            BinaryOp::Greater => {
                let left_sig = self.transform_expression(left, operand_width);
                let right_sig = self.transform_expression(right, operand_width);
                // Use signed comparison if either operand is signed
                let is_signed =
                    self.infer_expression_is_signed(left) || self.infer_expression_is_signed(right);
                let op = if is_signed {
                    LirOp::Sgt {
                        width: operand_width,
                    }
                } else {
                    LirOp::Gt {
                        width: operand_width,
                    }
                };
                (left_sig, right_sig, op, 1)
            }
            BinaryOp::GreaterEqual => {
                let left_sig = self.transform_expression(left, operand_width);
                let right_sig = self.transform_expression(right, operand_width);
                // Use signed comparison if either operand is signed
                let is_signed =
                    self.infer_expression_is_signed(left) || self.infer_expression_is_signed(right);
                let op = if is_signed {
                    LirOp::Sge {
                        width: operand_width,
                    }
                } else {
                    LirOp::Ge {
                        width: operand_width,
                    }
                };
                (left_sig, right_sig, op, 1)
            }

            // Shifts
            BinaryOp::LeftShift => {
                let left_sig = self.transform_expression(left, operand_width);
                let right_sig = self.transform_expression(right, operand_width);
                (
                    left_sig,
                    right_sig,
                    LirOp::Shl {
                        width: operand_width,
                    },
                    operand_width,
                )
            }
            BinaryOp::RightShift => {
                let left_sig = self.transform_expression(left, operand_width);
                let right_sig = self.transform_expression(right, operand_width);
                (
                    left_sig,
                    right_sig,
                    LirOp::Shr {
                        width: operand_width,
                    },
                    operand_width,
                )
            }

            _ => {
                self.warnings
                    .push(format!("Unsupported binary op: {:?}", op));
                let left_sig = self.transform_expression(left, operand_width);
                let right_sig = self.transform_expression(right, operand_width);
                (
                    left_sig,
                    right_sig,
                    LirOp::Buffer {
                        width: operand_width,
                    },
                    operand_width,
                )
            }
        };

        let out = self.alloc_temp_signal(result_width);
        let node_id = self.node_counter;
        self.node_counter += 1;
        self.lir.add_node(
            word_op,
            vec![left_signal, right_signal],
            out,
            format!("{}.{:?}_{}", self.hierarchy_path, op, node_id),
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
                // Reductions are handled specially above - this is for unsupported reduction ops
                self.warnings
                    .push(format!("Unsupported reduction op: {:?}", op));
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
            // BUG #164 FIX: Handle Float literals by converting to their bit representation
            // If width is 32, treat as fp32; if width is 64, treat as fp64
            Value::Float(f) => {
                if width <= 32 {
                    ((*f as f32).to_bits() as u64, width)
                } else {
                    (f.to_bits(), width)
                }
            }
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
                // BUG #150 FIX: Look up variable in the variable_to_signal map
                self.variable_to_signal
                    .get(var_id)
                    .copied()
                    .unwrap_or_else(|| {
                        self.warnings
                            .push(format!("Unknown variable: {:?}", var_id));
                        self.alloc_temp_signal(1)
                    })
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

    /// Get width of a DataType
    #[allow(clippy::only_used_in_recursion)]
    fn get_datatype_width(&self, dtype: &DataType) -> u32 {
        match dtype {
            DataType::Bit(w) | DataType::Logic(w) | DataType::Int(w) | DataType::Nat(w) => {
                *w as u32
            }
            DataType::Bool => 1,
            DataType::Clock { .. } | DataType::Reset { .. } | DataType::Event => 1,
            DataType::Float16 => 16,
            DataType::Float32 => 32,
            DataType::Float64 => 64,
            // Parametric types use their default value
            DataType::BitParam { default, .. }
            | DataType::LogicParam { default, .. }
            | DataType::IntParam { default, .. }
            | DataType::NatParam { default, .. }
            | DataType::BitExpr { default, .. }
            | DataType::LogicExpr { default, .. }
            | DataType::IntExpr { default, .. }
            | DataType::NatExpr { default, .. } => *default as u32,
            // Vector types
            DataType::Vec2(elem) => 2 * self.get_datatype_width(elem),
            DataType::Vec3(elem) => 3 * self.get_datatype_width(elem),
            DataType::Vec4(elem) => 4 * self.get_datatype_width(elem),
            // Array type: element width * count
            DataType::Array(elem, count) => self.get_datatype_width(elem) * (*count as u32),
            // Complex types - would need more detailed handling
            // In practice, casts to/from struct/enum/union types are rare
            DataType::Struct(_) | DataType::Enum(_) | DataType::Union(_) => 32,
            // NCL dual-rail type - physical width is 2x logical width
            DataType::Ncl(logical_width) => (*logical_width as u32) * 2,
        }
    }

    /// Get width of an LValue
    fn get_lvalue_width(&self, lvalue: &LValue) -> u32 {
        match lvalue {
            LValue::Port(port_id) => self.port_widths.get(port_id).copied().unwrap_or(1),
            LValue::Signal(signal_id) => self.signal_widths.get(signal_id).copied().unwrap_or(1),
            // BUG #150 FIX: Look up variable width from the variable_widths map
            LValue::Variable(var_id) => self.variable_widths.get(var_id).copied().unwrap_or(1),
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
                // BUG #164 FIX: Float values should have 32-bit width (fp32) or 64-bit (fp64)
                // Default to 32 bits for floating-point literals
                Value::Float(_) => 32,
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
            // BUG #164 FIX: Cast expressions should return target type width
            ExpressionKind::Cast { target_type, .. } => self.get_datatype_width(target_type),
            _ => 1,
        }
    }

    /// Infer whether an expression is signed (uses int type)
    fn infer_expression_is_signed(&self, expr: &Expression) -> bool {
        match &expr.kind {
            // BUG FIX: Integer literals should NOT automatically be signed.
            // They should adopt the signedness of the context (typically unsigned).
            // This was causing unsigned comparisons like `nat[4] < 10` to use
            // signed comparison, giving wrong results when MSB is set.
            ExpressionKind::Literal(Value::Integer(_)) => false,
            ExpressionKind::Ref(lvalue) => self.is_lvalue_signed(lvalue),
            ExpressionKind::Binary { left, right, .. } => {
                // If either operand is signed, treat as signed comparison
                self.infer_expression_is_signed(left) || self.infer_expression_is_signed(right)
            }
            ExpressionKind::Unary { operand, .. } => self.infer_expression_is_signed(operand),
            ExpressionKind::Conditional {
                then_expr,
                else_expr,
                ..
            } => {
                self.infer_expression_is_signed(then_expr)
                    || self.infer_expression_is_signed(else_expr)
            }
            ExpressionKind::Cast { target_type, .. } => matches!(
                target_type,
                DataType::Int(_) | DataType::IntParam { .. } | DataType::IntExpr { .. }
            ),
            _ => false,
        }
    }

    /// Check if an lvalue has a signed type
    fn is_lvalue_signed(&self, lvalue: &LValue) -> bool {
        match lvalue {
            LValue::Signal(signal_id) => self
                .signal_is_signed
                .get(signal_id)
                .copied()
                .unwrap_or(false),
            LValue::Port(port_id) => self.port_is_signed.get(port_id).copied().unwrap_or(false),
            LValue::Variable(var_id) => self
                .variable_is_signed
                .get(var_id)
                .copied()
                .unwrap_or(false),
            LValue::BitSelect { base, .. } => self.is_lvalue_signed(base),
            LValue::RangeSelect { base, .. } => self.is_lvalue_signed(base),
            LValue::Concat(_) => false, // Concatenation is typically bit-level
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
            // BUG #150 FIX: Also look up variables
            LValue::Variable(var_id) => self.variable_to_signal.get(var_id).copied(),
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
            // BUG #188 FIX: Handle Struct types (used for tuples)
            // Sum of all field widths
            DataType::Struct(struct_type) => struct_type
                .fields
                .iter()
                .map(|f| Self::get_type_width(&f.field_type))
                .sum(),
            // NCL dual-rail types
            DataType::Ncl(w) => (*w as u32) * 2,
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
    lower_mir_module_to_lir_with_context(module, false)
}

/// Lower MIR module to LIR with async context propagation
///
/// When `is_async_context` is true, the module is treated as async (NCL)
/// even if it wasn't declared with `async entity`. This is used when a
/// sync module is instantiated within an async parent module.
pub fn lower_mir_module_to_lir_with_context(
    module: &Module,
    is_async_context: bool,
) -> MirToLirResult {
    let mut transformer = MirToLirTransform::new(&module.name);
    transformer.transform_with_async_context(module, is_async_context)
}

/// Backward-compatible alias
pub fn lower_mir_module_to_word_lir(module: &Module) -> MirToLirResult {
    lower_mir_module_to_lir(module)
}

/// Lower MIR module to LIR without NCL expansion
///
/// This is used by the boundary-only NCL synthesis flow where we want to:
/// 1. Lower to single-rail LIR (no NCL expansion)
/// 2. Apply boundary-only NCL using expand_to_ncl_boundary()
/// 3. Tech map to gates (internal logic stays single-rail)
/// 4. Optimize the single-rail internal logic (AIG/ABC passes)
///
/// This function ALWAYS skips NCL expansion, even for async modules.
pub fn lower_mir_module_to_lir_skip_ncl(module: &Module) -> MirToLirResult {
    let mut transformer = MirToLirTransform::new(&module.name);
    transformer.transform_skip_ncl(module)
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
    pub instances: IndexMap<String, InstanceLirResult>,
    /// Top module name
    pub top_module: String,
    /// Instance hierarchy (parent -> children)
    pub hierarchy: IndexMap<String, Vec<String>>,
}

/// LIR result for a single instance
#[derive(Debug)]
pub struct InstanceLirResult {
    /// Module name (before specialization)
    pub module_name: String,
    /// The generated LIR
    pub lir_result: MirToLirResult,
    /// Port connections from parent
    pub port_connections: IndexMap<String, PortConnectionInfo>,
    /// Child instance paths
    pub children: Vec<String>,
    /// Whether this module is async (needs NCL)
    pub is_async: bool,
    /// NCL boundary mode override (from module attribute)
    /// None = automatic detection (sibling asyncs get boundaries, parent-child coalesce)
    pub ncl_boundary_mode: Option<skalp_mir::NclBoundaryMode>,
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
    /// Connected to a range of a signal (signal_name, high_bit, low_bit)
    Range(String, usize, usize),
    /// Connected to a single bit of a signal (signal_name, bit_index)
    BitSelect(String, usize),
}

/// Load a compiled IP and create a minimal LIR for it
///
/// This creates a placeholder LIR with just the port interfaces.
/// The actual gate netlist will be loaded during technology mapping.
fn load_compiled_ip_as_lir(
    config: &skalp_frontend::hir::CompiledIpConfig,
    module: &Module,
) -> Result<MirToLirResult, String> {
    use std::path::Path;

    trace!(
        "📦 COMPILED_IP: Loading pre-compiled IP from '{}'",
        config.skb_path
    );

    let skb_path = Path::new(&config.skb_path);

    // Try to resolve the path relative to the current directory or absolute
    let resolved_path = if skb_path.is_absolute() {
        skb_path.to_path_buf()
    } else {
        // Try current directory first
        let cwd_path = std::env::current_dir()
            .map(|cwd| cwd.join(skb_path))
            .unwrap_or_else(|_| skb_path.to_path_buf());

        if cwd_path.exists() {
            cwd_path
        } else {
            skb_path.to_path_buf()
        }
    };

    // Verify the file exists
    if !resolved_path.exists() {
        return Err(format!(
            "Compiled IP file not found: {} (resolved to {:?})",
            config.skb_path, resolved_path
        ));
    }

    // Load and verify the compiled IP
    let key = if config.encrypted {
        // Try to load encryption key from environment or key file
        // For now, require SKALP_IP_KEY environment variable
        if let Ok(key_hex) = std::env::var("SKALP_IP_KEY") {
            let key_bytes: Result<Vec<u8>, _> = (0..key_hex.len())
                .step_by(2)
                .map(|i| u8::from_str_radix(&key_hex[i..i + 2], 16))
                .collect();
            match key_bytes {
                Ok(bytes) if bytes.len() == 32 => {
                    let mut key = [0u8; 32];
                    key.copy_from_slice(&bytes);
                    Some(key)
                }
                _ => return Err("Invalid SKALP_IP_KEY format (expected 64 hex chars)".to_string()),
            }
        } else if let Some(ref _key_id) = config.key_id {
            return Err(format!(
                "Encrypted compiled IP requires SKALP_IP_KEY environment variable (key_id: {:?})",
                config.key_id
            ));
        } else {
            return Err(
                "Encrypted compiled IP requires SKALP_IP_KEY environment variable".to_string(),
            );
        }
    } else {
        None
    };

    // Load the compiled IP
    let compiled_ip = CompiledIp::read_from_file(&resolved_path, key.as_ref())
        .map_err(|e| format!("Failed to read compiled IP: {}", e))?;

    // Verify port count matches (basic sanity check)
    if compiled_ip.port_info.len() != module.ports.len() {
        trace!(
            "⚠️ COMPILED_IP: Port count mismatch - compiled IP has {} ports, module has {}",
            compiled_ip.port_info.len(),
            module.ports.len()
        );
    }

    trace!(
        "📦 COMPILED_IP: Loaded '{}' successfully ({} ports, {} cells)",
        compiled_ip.header.library_name,
        compiled_ip.port_info.len(),
        compiled_ip.netlist.cells.len()
    );

    // Create a minimal LIR with just port interfaces
    // The actual implementation will come from the pre-compiled netlist
    let mut lir = Lir::new(module.name.clone());

    // Add port signals
    for port in &module.ports {
        let width = MirToLirTransform::get_type_width(&port.port_type);
        let is_input = matches!(port.direction, PortDirection::Input);

        let signal_id = if is_input {
            lir.add_input(port.name.clone(), width)
        } else {
            lir.add_output(port.name.clone(), width)
        };

        // Mark clock and reset signals
        if port.name.contains("clk") || port.name.contains("clock") {
            lir.clocks.push(signal_id);
        }
        if port.name.contains("rst") || port.name.contains("reset") {
            lir.resets.push(signal_id);
        }
    }

    let stats = LirStats::from_lir(&lir);

    Ok(MirToLirResult {
        lir,
        stats,
        warnings: vec![format!(
            "Module '{}' uses pre-compiled IP from '{}'",
            module.name, config.skb_path
        )],
        compiled_ip_path: Some(resolved_path.to_string_lossy().to_string()),
        blackbox_info: None,
    })
}

/// Create a placeholder LIR for a blackbox/vendor IP module
///
/// This creates a minimal LIR with just port interfaces. The tech mapper
/// will create a Blackbox cell for this module instead of synthesizing it.
fn create_blackbox_lir_placeholder(
    module: &Module,
    vendor_config: &skalp_frontend::hir::VendorIpConfig,
) -> MirToLirResult {
    use crate::lir::Lir;

    // Create minimal LIR with port interfaces
    let mut lir = Lir::new(module.name.clone());

    let mut inputs = Vec::new();
    let mut outputs = Vec::new();
    let mut inouts = Vec::new();
    let mut port_widths = IndexMap::new();

    // Add port signals
    for port in &module.ports {
        let width = MirToLirTransform::get_type_width(&port.port_type);
        port_widths.insert(port.name.clone(), width);

        match port.direction {
            PortDirection::Input => {
                let signal_id = lir.add_input(port.name.clone(), width);
                inputs.push(port.name.clone());

                // Track clock and reset signals
                if matches!(port.port_type, DataType::Clock { .. }) {
                    lir.clocks.push(signal_id);
                }
                if matches!(port.port_type, DataType::Reset { .. }) {
                    lir.resets.push(signal_id);
                }
            }
            PortDirection::Output => {
                lir.add_output(port.name.clone(), width);
                outputs.push(port.name.clone());
            }
            PortDirection::InOut => {
                // InOut ports are both input and output
                let signal_id = lir.add_input(port.name.clone(), width);
                lir.outputs.push(signal_id);
                lir.signals[signal_id.0 as usize].is_output = true;
                inouts.push(port.name.clone());
            }
        }
    }

    let stats = LirStats::from_lir(&lir);

    // Create blackbox info
    let blackbox_info = BlackboxInfo {
        cell_name: vendor_config.ip_name.clone(),
        vendor: Some(format!("{:?}", vendor_config.vendor)),
        inputs,
        outputs,
        inouts,
        port_widths,
        parameters: vendor_config.parameters.clone(),
    };

    MirToLirResult {
        lir,
        stats,
        warnings: vec![format!(
            "Module '{}' is a blackbox (vendor IP: {})",
            module.name, vendor_config.ip_name
        )],
        compiled_ip_path: None,
        blackbox_info: Some(blackbox_info),
    }
}

/// Lower entire MIR hierarchy to per-instance LIR
///
/// This function traverses the module hierarchy starting from the top module,
/// creating a separate LIR for each instance. Each instance can be specialized
/// based on its context (constant inputs, unused outputs).
pub fn lower_mir_hierarchical(mir: &Mir) -> HierarchicalMirToLirResult {
    // Build module lookup by ID and by name (for fallback)
    let module_map: IndexMap<ModuleId, &Module> = mir.modules.iter().map(|m| (m.id, m)).collect();
    let module_by_name: IndexMap<&str, &Module> =
        mir.modules.iter().map(|m| (m.name.as_str(), m)).collect();

    // DEBUG: Print module ID to name mapping
    trace!(
        "[HIER_LIR_DEBUG] Module ID mapping (first 10): {:?}",
        mir.modules
            .iter()
            .take(10)
            .map(|m| (m.id.0, &m.name))
            .collect::<Vec<_>>()
    );
    // Find KarythraCLEAsync module ID
    if let Some(kcle) = mir.modules.iter().find(|m| m.name == "KarythraCLEAsync") {
        trace!(
            "[HIER_LIR_DEBUG] KarythraCLEAsync has ModuleId({})",
            kcle.id.0
        );
    }

    // Find modules that are instantiated (have parents)
    let mut instantiated_modules: HashSet<ModuleId> = HashSet::new();
    let mut modules_with_instances: HashSet<ModuleId> = HashSet::new();

    for module in &mir.modules {
        if !module.instances.is_empty() {
            modules_with_instances.insert(module.id);
        }
        for inst in &module.instances {
            // Debug: track what's instantiating what
            if module.name == "FpSub" && inst.name == "adder" {
                trace!(
                    "[HIER_LIR_DEBUG] FpSub's 'adder' instance has module=ModuleId({})",
                    inst.module.0
                );
            }
            if let Some(child_module) = mir.modules.iter().find(|m| m.id == inst.module) {
                if child_module.name == "KarythraCLEAsync" {
                    trace!(
                        "[HIER_LIR_DEBUG] Module '{}' instantiates 'KarythraCLEAsync' via instance '{}'",
                        module.name, inst.name
                    );
                }
            }
            instantiated_modules.insert(inst.module);
        }
    }

    // Find top module: has instances but is not instantiated by others
    // If no such module exists, fall back to first non-instantiated module
    trace!(
        "[HIER_LIR] Looking for top module among {} modules",
        mir.modules.len()
    );
    trace!(
        "[HIER_LIR] modules_with_instances: {:?}",
        mir.modules
            .iter()
            .filter(|m| modules_with_instances.contains(&m.id))
            .map(|m| &m.name)
            .collect::<Vec<_>>()
    );
    trace!(
        "[HIER_LIR] instantiated_modules (have parents): {:?}",
        mir.modules
            .iter()
            .filter(|m| instantiated_modules.contains(&m.id))
            .map(|m| &m.name)
            .collect::<Vec<_>>()
    );

    // BUG FIX: Select the top module with the most UNIQUE instance types.
    // The user's main module typically instantiates many different module types
    // (func_exec_l2, func_exec_l3, etc.), while stdlib modules like FpSub only
    // instantiate one or two types (e.g., just FpAdd).
    let top_candidates: Vec<_> = mir
        .modules
        .iter()
        .filter(|m| modules_with_instances.contains(&m.id) && !instantiated_modules.contains(&m.id))
        .collect();

    // Helper to check if a module name looks like a stdlib module
    fn is_stdlib_name(name: &str) -> bool {
        // Stdlib modules typically start with these prefixes
        let stdlib_prefixes = [
            "Fp",
            "Cordic",
            "Int",
            "Shift",
            "Vec",
            "Matrix",
            "Fifo",
            "Memory",
            "Clz",
            "Popcount",
            "Parity",
            "Bitreverse",
        ];
        stdlib_prefixes
            .iter()
            .any(|prefix| name.starts_with(prefix))
    }

    let top_module = if !top_candidates.is_empty() {
        // First try: prefer user modules (non-stdlib names)
        let user_candidates: Vec<_> = top_candidates
            .iter()
            .filter(|m| !is_stdlib_name(&m.name))
            .copied()
            .collect();

        if !user_candidates.is_empty() {
            // Among user modules, pick the one with most unique instance types
            user_candidates
                .into_iter()
                .max_by_key(|m| {
                    let unique_types: std::collections::HashSet<_> =
                        m.instances.iter().map(|i| i.module).collect();
                    unique_types.len()
                })
                .unwrap()
        } else {
            // No user modules found, fall back to most unique instance types
            top_candidates
                .into_iter()
                .max_by_key(|m| {
                    let unique_types: std::collections::HashSet<_> =
                        m.instances.iter().map(|i| i.module).collect();
                    unique_types.len()
                })
                .unwrap()
        }
    } else {
        // Fallback: find the module with most unique instance types among non-instantiated
        mir.modules
            .iter()
            .filter(|m| !instantiated_modules.contains(&m.id))
            .max_by_key(|m| {
                let unique_types: std::collections::HashSet<_> =
                    m.instances.iter().map(|i| i.module).collect();
                unique_types.len()
            })
            .unwrap_or(&mir.modules[0])
    };

    trace!(
        "[HIER_LIR] Selected top module: '{}' (id={}, {} instances)",
        top_module.name,
        top_module.id.0,
        top_module.instances.len()
    );

    let mut result = HierarchicalMirToLirResult {
        instances: IndexMap::new(),
        top_module: top_module.name.clone(),
        hierarchy: IndexMap::new(),
    };

    // Create LIR cache to avoid redundant NCL expansion for same module types
    let mut lir_cache: HashMap<LirCacheKey, MirToLirResult> = HashMap::new();

    // Recursively elaborate instances
    // Top module uses its own is_async flag, no inherited context
    elaborate_instance(
        &module_map,
        &module_by_name,
        top_module,
        "top",
        &IndexMap::new(), // No constant inputs at top level
        false,            // No inherited async context for top module
        &mut result,
        &mut lir_cache,
    );

    // Log cache statistics
    trace!(
        "[HIER_LIR] LIR cache: {} unique module/async combinations cached",
        lir_cache.len()
    );

    result
}

/// Lower entire MIR hierarchy with explicit top module selection
///
/// Same as `lower_mir_hierarchical` but allows specifying the top module name.
/// If `top_name` is None, falls back to automatic top module detection.
pub fn lower_mir_hierarchical_with_top(mir: &Mir, top_name: Option<&str>) -> HierarchicalMirToLirResult {
    // If no specific top module requested, use automatic detection
    if top_name.is_none() {
        return lower_mir_hierarchical(mir);
    }

    let requested_top = top_name.unwrap();

    // Build module lookup by ID and by name
    let module_map: IndexMap<ModuleId, &Module> = mir.modules.iter().map(|m| (m.id, m)).collect();
    let module_by_name: IndexMap<&str, &Module> =
        mir.modules.iter().map(|m| (m.name.as_str(), m)).collect();

    // Find the requested top module
    let top_module = mir
        .modules
        .iter()
        .find(|m| m.name == requested_top)
        .unwrap_or_else(|| {
            panic!(
                "Requested top module '{}' not found. Available modules: {:?}",
                requested_top,
                mir.modules.iter().map(|m| &m.name).collect::<Vec<_>>()
            )
        });

    trace!(
        "[HIER_LIR] Using explicitly requested top module: '{}' (id={}, {} instances)",
        top_module.name,
        top_module.id.0,
        top_module.instances.len()
    );

    let mut result = HierarchicalMirToLirResult {
        instances: IndexMap::new(),
        top_module: top_module.name.clone(),
        hierarchy: IndexMap::new(),
    };

    // Create LIR cache to avoid redundant NCL expansion for same module types
    let mut lir_cache: HashMap<LirCacheKey, MirToLirResult> = HashMap::new();

    // Recursively elaborate instances
    elaborate_instance(
        &module_map,
        &module_by_name,
        top_module,
        "top",
        &IndexMap::new(),
        false,
        &mut result,
        &mut lir_cache,
    );

    trace!(
        "[HIER_LIR] LIR cache: {} unique module/async combinations cached",
        lir_cache.len()
    );

    result
}

/// Lower entire MIR hierarchy for optimize-first NCL synthesis
///
/// This is the standard approach for async (NCL) circuits.
/// It skips NCL expansion during MIR→LIR lowering, producing single-rail gates
/// that can be optimized before converting to dual-rail NCL.
///
/// Returns:
/// - The hierarchical LIR result (without NCL expansion)
/// - A flag indicating if any module was async (needs dual-rail conversion)
pub fn lower_mir_hierarchical_for_optimize_first(mir: &Mir) -> (HierarchicalMirToLirResult, bool) {
    use skalp_mir::ModuleId;
    use std::collections::HashSet;

    // Check if any module is async
    let has_async = mir.modules.iter().any(|m| m.is_async);
    if has_async {
        trace!(
            "⚡ NCL Optimize-First: Skipping NCL expansion for {} modules",
            mir.modules.iter().filter(|m| m.is_async).count()
        );
    }

    // Build module lookup by ID and by name
    let module_map: IndexMap<ModuleId, &Module> = mir.modules.iter().map(|m| (m.id, m)).collect();
    let module_by_name: IndexMap<&str, &Module> =
        mir.modules.iter().map(|m| (m.name.as_str(), m)).collect();

    // Find modules that are instantiated (have parents)
    let mut instantiated_modules: HashSet<ModuleId> = HashSet::new();
    let mut modules_with_instances: HashSet<ModuleId> = HashSet::new();

    for module in &mir.modules {
        if !module.instances.is_empty() {
            modules_with_instances.insert(module.id);
        }
        for inst in &module.instances {
            instantiated_modules.insert(inst.module);
        }
    }

    // Helper to check if a module name looks like a stdlib module
    fn is_stdlib_name_opt(name: &str) -> bool {
        let stdlib_prefixes = [
            "Fp",
            "Cordic",
            "Int",
            "Shift",
            "Vec",
            "Matrix",
            "Fifo",
            "Memory",
            "Clz",
            "Popcount",
            "Parity",
            "Bitreverse",
        ];
        stdlib_prefixes
            .iter()
            .any(|prefix| name.starts_with(prefix))
    }

    // Find top module - a module that is NOT instantiated by any other module
    // Priority:
    // 1. User modules (non-stdlib) that are not instantiated
    // 2. Among those, prefer modules with more instances (has children)
    // 3. If no user modules found, fall back to any non-instantiated module

    // All modules that are not instantiated by anyone (potential top modules)
    let not_instantiated: Vec<_> = mir
        .modules
        .iter()
        .filter(|m| !instantiated_modules.contains(&m.id))
        .collect();

    // First priority: user modules (non-stdlib names) that are not instantiated
    let user_candidates: Vec<_> = not_instantiated
        .iter()
        .filter(|m| !is_stdlib_name_opt(&m.name))
        .copied()
        .collect();

    // Helper to check if a module has actual content (not just a generic shell)
    fn has_content(m: &Module) -> bool {
        !m.instances.is_empty()
            || !m.signals.is_empty()
            || !m.processes.is_empty()
            || !m.assignments.is_empty()
    }

    // Find valid top module: user module with content that's not instantiated
    let user_with_content: Vec<_> = user_candidates
        .iter()
        .filter(|m| has_content(m))
        .copied()
        .collect();

    let top_module = if !user_with_content.is_empty() {
        // Pick user module with content, preferring more instances
        user_with_content
            .into_iter()
            .max_by_key(|m| {
                let unique_types: std::collections::HashSet<_> =
                    m.instances.iter().map(|i| i.module).collect();
                (unique_types.len(), m.id.0)
            })
            .unwrap()
    } else {
        // No valid user module found - fall back to stdlib-looking modules with content
        // This handles user modules that happen to have stdlib-like prefixes (e.g., "BitreverseMWE")
        let stdlib_with_content: Vec<_> = not_instantiated
            .iter()
            .filter(|m| is_stdlib_name_opt(&m.name) && has_content(m))
            .copied()
            .collect();

        if !stdlib_with_content.is_empty() {
            // Found a module with stdlib-like prefix that has content - use it
            stdlib_with_content
                .into_iter()
                .max_by_key(|m| {
                    let unique_types: std::collections::HashSet<_> =
                        m.instances.iter().map(|i| i.module).collect();
                    (unique_types.len(), m.id.0)
                })
                .unwrap()
        } else {
            // No valid user module found - check if there are user modules without content
            let empty_user_modules: Vec<_> = user_candidates
                .iter()
                .filter(|m| !has_content(m))
                .map(|m| m.name.as_str())
                .collect();

            if !empty_user_modules.is_empty() {
                panic!(
                    "Top module detection failed: Found user module(s) {:?} but they have no content. \
                     This usually happens when an entity has generic parameters (like clock domains) \
                     but is never instantiated. Either:\n\
                     1. Remove generic parameters from the top-level entity, or\n\
                     2. Create a wrapper entity that instantiates it with concrete types.",
                    empty_user_modules
                );
            } else {
                panic!(
                    "Top module detection failed: No valid user module found. \
                     Only stdlib modules are present in the design. \
                     Ensure your source file defines a synthesizable entity."
                );
            }
        }
    };

    trace!(
        "[TOP_DETECT] Selected '{}' as top module (has {} instances, is_async={})",
        top_module.name,
        top_module.instances.len(),
        top_module.is_async
    );

    let mut result = HierarchicalMirToLirResult {
        instances: IndexMap::new(),
        top_module: top_module.name.clone(),
        hierarchy: IndexMap::new(),
    };

    // Create LIR cache to avoid redundant LIR computation for same module types
    let mut lir_cache: HashMap<String, MirToLirResult> = HashMap::new();

    // Elaborate instances with is_async_context = FALSE to skip NCL expansion
    // We pass false even for async modules so they get single-rail gates
    elaborate_instance_for_optimize_first(
        &module_map,
        &module_by_name,
        top_module,
        "top",
        &IndexMap::new(),
        &mut result,
        &mut lir_cache,
    );

    // Log cache statistics
    trace!(
        "[HIER_LIR_OPT] LIR cache: {} unique modules cached",
        lir_cache.len()
    );

    (result, has_async)
}

/// Recursively elaborate a module instance for optimize-first flow
///
/// This always passes is_async_context = false to skip NCL expansion,
/// regardless of whether the module is async. The dual-rail conversion
/// will be done later after optimization.
#[allow(clippy::too_many_arguments)]
fn elaborate_instance_for_optimize_first(
    module_map: &IndexMap<ModuleId, &Module>,
    _module_by_name: &IndexMap<&str, &Module>,
    module: &Module,
    instance_path: &str,
    parent_connections: &IndexMap<String, PortConnectionInfo>,
    result: &mut HierarchicalMirToLirResult,
    lir_cache: &mut HashMap<String, MirToLirResult>,
) {
    // Use skip_ncl to completely skip NCL expansion for optimize-first flow
    let lir_result = if let Some(ref vendor_config) = module.vendor_ip_config {
        trace!(
            "🔌 VENDOR_IP: Module '{}' is a blackbox (ip={}), skipping internal elaboration",
            module.name,
            vendor_config.ip_name
        );
        create_blackbox_lir_placeholder(module, vendor_config)
    } else if let Some(ref config) = module.compiled_ip_config {
        match load_compiled_ip_as_lir(config, module) {
            Ok(lir_result) => lir_result,
            Err(e) => {
                trace!(
                    "⚠️ COMPILED_IP: Failed to load '{}' for module '{}': {}",
                    config.skb_path,
                    module.name,
                    e
                );
                // Use skip_ncl to get single-rail LIR
                lower_mir_module_to_lir_skip_ncl(module)
            }
        }
    } else {
        // Use skip_ncl to get single-rail LIR for optimize-first flow
        // Use cache to avoid redundant LIR computation for same module type
        if let Some(cached_result) = lir_cache.get(&module.name) {
            trace!(
                "[LIR_CACHE] Cache hit for module '{}' (skip_ncl)",
                module.name
            );
            cached_result.clone()
        } else {
            trace!(
                "[LIR_CACHE] Cache miss for module '{}' (skip_ncl), computing LIR",
                module.name
            );
            let computed_result = lower_mir_module_to_lir_skip_ncl(module);
            lir_cache.insert(module.name.clone(), computed_result.clone());
            computed_result
        }
    };

    // Collect child instance paths
    let mut children: Vec<String> = Vec::new();

    trace!(
        "[ELABORATE-OPT] Module '{}' at path '{}' has {} instances (async={})",
        module.name,
        instance_path,
        module.instances.len(),
        module.is_async
    );

    // Process child instances
    for inst in &module.instances {
        let child_path = format!("{}.{}", instance_path, inst.name);
        children.push(child_path.clone());

        if let Some(child_mod) = module_map.get(&inst.module).copied() {
            // Use the same connection extraction as the regular elaborate_instance
            let child_connections = extract_connection_info(&inst.connections, module);

            // Recurse with optimize-first flow
            elaborate_instance_for_optimize_first(
                module_map,
                _module_by_name,
                child_mod,
                &child_path,
                &child_connections,
                result,
                lir_cache,
            );
        }
    }

    // Record hierarchy
    result
        .hierarchy
        .insert(instance_path.to_string(), children.clone());

    // Store this instance's result
    result.instances.insert(
        instance_path.to_string(),
        InstanceLirResult {
            module_name: module.name.clone(),
            lir_result,
            port_connections: parent_connections.clone(),
            children,
            is_async: module.is_async,
            ncl_boundary_mode: module.ncl_boundary_mode,
        },
    );
}

/// Recursively elaborate a module instance
///
/// The `is_async_context` parameter indicates whether this module is being
/// instantiated within an async parent. If true, the module will be NCL-expanded
/// even if it wasn't declared with `async entity`.
///
/// The `lir_cache` parameter caches LIR results by (module_name, is_async_context)
/// to avoid redundant NCL expansion for multiple instances of the same module type.
#[allow(clippy::too_many_arguments)]
fn elaborate_instance(
    module_map: &IndexMap<ModuleId, &Module>,
    _module_by_name: &IndexMap<&str, &Module>,
    module: &Module,
    instance_path: &str,
    parent_connections: &IndexMap<String, PortConnectionInfo>,
    is_async_context: bool,
    result: &mut HierarchicalMirToLirResult,
    lir_cache: &mut HashMap<LirCacheKey, MirToLirResult>,
) {
    // Compute effective async status: module's own flag OR inherited from parent
    let effective_is_async = module.is_async || is_async_context;

    // Check if this module is a vendor IP (blackbox - don't synthesize internals)
    let lir_result = if let Some(ref vendor_config) = module.vendor_ip_config {
        // This is a vendor IP / blackbox - create a placeholder LIR result
        // The actual blackbox cell will be created during tech mapping
        trace!(
            "🔌 VENDOR_IP: Module '{}' is a blackbox (ip={}), skipping internal elaboration",
            module.name,
            vendor_config.ip_name
        );
        create_blackbox_lir_placeholder(module, vendor_config)
    } else if let Some(ref config) = module.compiled_ip_config {
        // Load the compiled IP from the .skb file
        match load_compiled_ip_as_lir(config, module) {
            Ok(lir_result) => lir_result,
            Err(e) => {
                trace!(
                    "⚠️ COMPILED_IP: Failed to load '{}' for module '{}': {}",
                    config.skb_path,
                    module.name,
                    e
                );
                trace!("⚠️ COMPILED_IP: Falling back to normal elaboration");
                lower_mir_module_to_lir_with_context(module, is_async_context)
            }
        }
    } else {
        // Normal MIR to LIR transformation with async context propagation
        // Use cache to avoid redundant NCL expansion for same module type
        let cache_key = (module.name.clone(), is_async_context);
        if let Some(cached_result) = lir_cache.get(&cache_key) {
            trace!(
                "[LIR_CACHE] Cache hit for module '{}' (async={})",
                module.name,
                is_async_context
            );
            cached_result.clone()
        } else {
            trace!(
                "[LIR_CACHE] Cache miss for module '{}' (async={}), computing LIR",
                module.name,
                is_async_context
            );
            let computed_result = lower_mir_module_to_lir_with_context(module, is_async_context);
            lir_cache.insert(cache_key, computed_result.clone());
            computed_result
        }
    };

    // Collect child instance paths
    let mut children: Vec<String> = Vec::new();

    trace!(
        "[ELABORATE] Module '{}' at path '{}' has {} instances",
        module.name,
        instance_path,
        module.instances.len()
    );

    // Process child instances
    for inst in &module.instances {
        trace!(
            "[ELABORATE]   Processing child instance '{}' -> module_id={}",
            inst.name,
            inst.module.0
        );
        let child_path = format!("{}.{}", instance_path, inst.name);
        children.push(child_path.clone());

        // Find child module by ID
        if let Some(child_mod) = module_map.get(&inst.module).copied() {
            // Extract connection info using parent module for name lookup
            trace!(
                "[ELABORATE]   Instance '{}' has {} connections:",
                inst.name,
                inst.connections.len()
            );
            for (port_name, expr) in &inst.connections {
                trace!("[ELABORATE]     {} -> {:?}", port_name, expr.kind);
            }
            let child_connections = extract_connection_info(&inst.connections, module);

            // Recursively elaborate child, propagating async context
            // If parent is async (by declaration or inheritance), children inherit it
            elaborate_instance(
                module_map,
                _module_by_name,
                child_mod,
                &child_path,
                &child_connections,
                effective_is_async, // Propagate async context to child
                result,
                lir_cache, // Pass cache to avoid redundant LIR computation
            );
        }
    }

    // Record hierarchy
    result
        .hierarchy
        .insert(instance_path.to_string(), children.clone());

    // Store this instance's result
    result.instances.insert(
        instance_path.to_string(),
        InstanceLirResult {
            module_name: module.name.clone(),
            lir_result,
            port_connections: parent_connections.clone(),
            children,
            is_async: effective_is_async,
            ncl_boundary_mode: module.ncl_boundary_mode,
        },
    );
}

/// Extract connection information from port connections
/// Uses the parent module to look up actual signal/port names by ID
/// BUG #168 FIX: Also resolves variable references to their constant values when possible
fn extract_connection_info(
    connections: &IndexMap<String, Expression>,
    parent_module: &Module,
) -> IndexMap<String, PortConnectionInfo> {
    let mut result = IndexMap::new();

    // Sort port names for deterministic iteration order (HashMap is non-deterministic)
    let mut sorted_port_names: Vec<_> = connections.keys().collect();
    sorted_port_names.sort();

    for port_name in sorted_port_names {
        let expr = connections.get(port_name).unwrap();
        let info = match &expr.kind {
            ExpressionKind::Literal(value) => {
                // Constant connection
                let const_val = value_to_u64(value);
                PortConnectionInfo::Constant(const_val)
            }
            ExpressionKind::Ref(lvalue) => {
                // Check for RangeSelect or BitSelect patterns
                match lvalue {
                    LValue::RangeSelect { base, high, low } => {
                        // Range connection like b[4:0]
                        let base_name = lvalue_to_name_with_module(base, parent_module);
                        let high_val = extract_const_index(high).unwrap_or(0);
                        let low_val = extract_const_index(low).unwrap_or(0);
                        PortConnectionInfo::Range(base_name, high_val, low_val)
                    }
                    LValue::BitSelect { base, index } => {
                        // Single bit connection like op[0]
                        let base_name = lvalue_to_name_with_module(base, parent_module);
                        let bit_idx = extract_const_index(index).unwrap_or(0);
                        PortConnectionInfo::BitSelect(base_name, bit_idx)
                    }
                    LValue::Variable(var_id) => {
                        // BUG #168 FIX: Try to resolve variable to its constant value
                        // This handles cases where a let binding with a constant value
                        // is passed as an argument to a module instance
                        if let Some(const_val) = lookup_variable_constant(parent_module, *var_id) {
                            trace!(
                                "[EXTRACT_CONN] BUG #168 FIX: Resolved var_{} to constant 0x{:X}",
                                var_id.0,
                                const_val
                            );
                            PortConnectionInfo::Constant(const_val)
                        } else {
                            // Fall back to signal reference
                            let signal_name = lvalue_to_name_with_module(lvalue, parent_module);
                            PortConnectionInfo::Signal(signal_name)
                        }
                    }
                    _ => {
                        // Simple signal reference
                        let signal_name = lvalue_to_name_with_module(lvalue, parent_module);
                        PortConnectionInfo::Signal(signal_name)
                    }
                }
            }
            // BUG #190 FIX: Handle Cast expressions that wrap references
            // When a Cast expression is used to connect an entity port (e.g., fp32(a) -> fp_adder.a),
            // we need to unwrap the Cast to get at the underlying signal reference.
            // The Cast is just a type annotation; the underlying signal is what we connect.
            ExpressionKind::Cast { expr: inner, .. } => {
                // Recursively unwrap casts until we hit the actual expression
                fn extract_inner_ref(expr: &Expression) -> &Expression {
                    if let ExpressionKind::Cast { expr: inner, .. } = &expr.kind {
                        extract_inner_ref(inner)
                    } else {
                        expr
                    }
                }
                let inner_expr = extract_inner_ref(expr);

                match &inner_expr.kind {
                    ExpressionKind::Ref(lvalue) => {
                        // BUG #200 FIX: Handle RangeSelect and BitSelect properly inside Cast
                        // When a Cast wraps a RangeSelect like Cast(Cast(var_12[15:0])),
                        // we need to extract the Range info for proper NCL stitching,
                        // not just convert to a string "var_12[15:0]" which doesn't exist as a signal
                        match lvalue {
                            LValue::RangeSelect { base, high, low } => {
                                let base_name = lvalue_to_name_with_module(base, parent_module);
                                let high_val = extract_const_index(high).unwrap_or(0);
                                let low_val = extract_const_index(low).unwrap_or(0);
                                trace!(
                                    "[EXTRACT_CONN] BUG #200 FIX: Cast->RangeSelect: {}[{}:{}]",
                                    base_name,
                                    high_val,
                                    low_val
                                );
                                PortConnectionInfo::Range(base_name, high_val, low_val)
                            }
                            LValue::BitSelect { base, index } => {
                                let base_name = lvalue_to_name_with_module(base, parent_module);
                                let bit_idx = extract_const_index(index).unwrap_or(0);
                                trace!(
                                    "[EXTRACT_CONN] BUG #200 FIX: Cast->BitSelect: {}[{}]",
                                    base_name,
                                    bit_idx
                                );
                                PortConnectionInfo::BitSelect(base_name, bit_idx)
                            }
                            _ => {
                                let signal_name = lvalue_to_name_with_module(lvalue, parent_module);
                                trace!(
                                    "[EXTRACT_CONN] BUG #190 FIX: Unwrapped Cast to signal '{}'",
                                    signal_name
                                );
                                PortConnectionInfo::Signal(signal_name)
                            }
                        }
                    }
                    ExpressionKind::Literal(value) => {
                        let const_val = value_to_u64(value);
                        trace!(
                            "[EXTRACT_CONN] BUG #190 FIX: Unwrapped Cast to constant 0x{:X}",
                            const_val
                        );
                        PortConnectionInfo::Constant(const_val)
                    }
                    _ => {
                        // Complex inner expression - still need to create synthetic signal
                        trace!(
                            "[EXTRACT_CONN] BUG #190: Cast inner is complex: {:?}",
                            inner_expr.kind
                        );
                        PortConnectionInfo::Signal(format!("expr_{}", port_name))
                    }
                }
            }
            _ => {
                // Complex expression - treat as signal
                trace!(
                    "[EXTRACT_CONN] Complex expression for port '{}': {:?}",
                    port_name,
                    expr.kind
                );
                PortConnectionInfo::Signal(format!("expr_{}", port_name))
            }
        };
        result.insert(port_name.clone(), info);
    }

    result
}

/// BUG #168 FIX: Look up a variable's constant value from the module
/// Returns Some(value) if the variable is assigned a constant, None otherwise
fn lookup_variable_constant(module: &Module, var_id: VariableId) -> Option<u64> {
    trace!(
        "[LOOKUP_VAR] Looking for var_{} in module '{}' ({} assignments, {} processes)",
        var_id.0,
        module.name,
        module.assignments.len(),
        module.processes.len()
    );

    // Search through continuous assignments
    for (idx, assign) in module.assignments.iter().enumerate() {
        if let LValue::Variable(id) = &assign.lhs {
            if *id == var_id {
                let value = extract_constant_value(&assign.rhs);
                trace!(
                    "[LOOKUP_VAR] Found var_{} in continuous assignment #{} -> {:?}",
                    var_id.0,
                    idx,
                    value
                );
                return value;
            }
        }
    }

    // Search through process blocks
    for (proc_idx, process) in module.processes.iter().enumerate() {
        for stmt in &process.body.statements {
            if let Some(value) = find_variable_constant_in_stmt(stmt, var_id) {
                trace!(
                    "[LOOKUP_VAR] Found var_{} in process #{} -> 0x{:X}",
                    var_id.0,
                    proc_idx,
                    value
                );
                return Some(value);
            }
        }
    }

    trace!(
        "[LOOKUP_VAR] var_{} NOT FOUND in module '{}'",
        var_id.0,
        module.name
    );
    None
}

/// Helper to recursively search for a variable's constant assignment in a statement
fn find_variable_constant_in_stmt(stmt: &Statement, var_id: VariableId) -> Option<u64> {
    match stmt {
        Statement::Assignment(assign) => {
            // Check if this is an assignment to our variable
            if let LValue::Variable(id) = &assign.lhs {
                if *id == var_id {
                    // Check if RHS is a constant
                    return extract_constant_value(&assign.rhs);
                }
            }
            None
        }
        Statement::Block(block) => {
            // Search through block statements
            for inner_stmt in &block.statements {
                if let Some(value) = find_variable_constant_in_stmt(inner_stmt, var_id) {
                    return Some(value);
                }
            }
            None
        }
        _ => None,
    }
}

/// Extract a constant value from an expression if it's a literal
fn extract_constant_value(expr: &Expression) -> Option<u64> {
    match &expr.kind {
        ExpressionKind::Literal(value) => Some(value_to_u64(value)),
        ExpressionKind::Cast { expr: inner, .. } => {
            // Handle casts like (3.0 as fp32) as bit[32]
            extract_constant_value(inner)
        }
        ExpressionKind::Unary { operand, .. } => {
            // Handle unary operations (might be casts)
            extract_constant_value(operand)
        }
        _ => None,
    }
}

/// Extract a constant index value from an expression
fn extract_const_index(expr: &Expression) -> Option<usize> {
    match &expr.kind {
        ExpressionKind::Literal(Value::Integer(v)) => Some(*v as usize),
        ExpressionKind::Literal(Value::BitVector { value, .. }) => Some(*value as usize),
        _ => None,
    }
}

/// Convert a Value to u64
/// For Float values, converts to IEEE 754 bit representation (not truncation)
fn value_to_u64(value: &Value) -> u64 {
    match value {
        Value::Integer(i) => *i as u64,
        Value::BitVector { value, .. } => *value,
        Value::Float(f) => {
            // BUG #168 FIX: Convert float to IEEE 754 bit representation
            // For fp32 (single precision), the bits are in the lower 32 bits
            let f32_val = *f as f32;
            f32_val.to_bits() as u64
        }
        _ => 0, // Default for String, HighZ, Unknown
    }
}

/// Convert an LValue to a signal name using the module to look up actual names
fn lvalue_to_name_with_module(lvalue: &LValue, module: &Module) -> String {
    match lvalue {
        LValue::Signal(id) => {
            // Look up signal by ID in the module
            module
                .signals
                .iter()
                .find(|s| s.id == *id)
                .map(|s| s.name.clone())
                .unwrap_or_else(|| format!("signal_{}", id.0))
        }
        LValue::Port(id) => {
            // Look up port by ID in the module
            module
                .ports
                .iter()
                .find(|p| p.id == *id)
                .map(|p| p.name.clone())
                .unwrap_or_else(|| format!("port_{}", id.0))
        }
        LValue::Variable(id) => {
            // BUG #200 FIX: Look up variable by ID in the module to get actual name
            // Variables representing function parameters become signals like "param_0", "param_1"
            // We must use their actual names for NCL stitching to find the right nets
            let result = module
                .variables
                .iter()
                .find(|v| v.id == *id)
                .map(|v| v.name.clone())
                .unwrap_or_else(|| format!("var_{}", id.0));
            // DEBUG: Log variable lookup
            if result.starts_with("var_") {
                // Check assignments for variable name
                let from_assign = module.assignments.iter().find(|a| {
                    if let LValue::Variable(var_id) = &a.lhs {
                        var_id == id
                    } else {
                        false
                    }
                });
                // BUG #200: Try to find a signal whose assignment RHS references this Variable
                // The pattern is: continuous assignments like `assign a_0 = param_1[31:0]`
                // where Variable(12) is used in the entity connection but a_0 is the actual signal.
                // We need to find the signal that gets assigned from expressions involving this Variable.
                //
                // However, by this point, the assignments no longer have Variable LHS - they have Signal LHS.
                // The Variables were converted to Signals during HIR→MIR.
                //
                // A simpler approach: Look for a signal whose name might match a pattern
                // Check if there's a signal named "var_{id}", "_v_{something}", or look at assignment sources
                let var_name = format!("var_{}", id.0);
                let matching_signal = module.signals.iter().find(|s| s.name == var_name);

                // Print first assignment RHS to understand the structure
                if let Some(first_assign) = module.assignments.first() {
                    trace!(
                        "[BUG200_ASSIGN_DEBUG] First assignment LHS={:?}, RHS has Variable refs: {}",
                        first_assign.lhs,
                        contains_variable_ref(&first_assign.rhs, *id)
                    );
                }

                trace!(
                    "[BUG200_DEBUG] Variable({}) NOT FOUND in module '{}' - signals: {}, ports: {}",
                    id.0,
                    module.name,
                    module.signals.len(),
                    module.ports.len()
                );
            }
            result
        }
        LValue::BitSelect { base, index } => {
            let base_name = lvalue_to_name_with_module(base, module);
            // Try to get the index value if it's a literal
            if let ExpressionKind::Literal(Value::Integer(idx)) = &index.kind {
                format!("{}[{}]", base_name, idx)
            } else if let ExpressionKind::Literal(Value::BitVector { value, .. }) = &index.kind {
                format!("{}[{}]", base_name, value)
            } else {
                format!("{}[...]", base_name)
            }
        }
        LValue::RangeSelect { base, high, low } => {
            let base_name = lvalue_to_name_with_module(base, module);
            // Try to extract integer values from high/low expressions
            let high_str = match &high.kind {
                ExpressionKind::Literal(Value::Integer(v)) => v.to_string(),
                ExpressionKind::Literal(Value::BitVector { value, .. }) => value.to_string(),
                _ => "?".to_string(),
            };
            let low_str = match &low.kind {
                ExpressionKind::Literal(Value::Integer(v)) => v.to_string(),
                ExpressionKind::Literal(Value::BitVector { value, .. }) => value.to_string(),
                _ => "?".to_string(),
            };
            format!("{}[{}:{}]", base_name, high_str, low_str)
        }
        LValue::Concat(parts) => parts
            .iter()
            .map(|p| lvalue_to_name_with_module(p, module))
            .collect::<Vec<_>>()
            .join("_"),
    }
}

/// Check if an expression contains a reference to a specific Variable
fn contains_variable_ref(expr: &Expression, var_id: VariableId) -> bool {
    match &expr.kind {
        ExpressionKind::Ref(lvalue) => match lvalue {
            LValue::Variable(id) => *id == var_id,
            LValue::RangeSelect { base, .. } | LValue::BitSelect { base, .. } => {
                if let LValue::Variable(id) = base.as_ref() {
                    *id == var_id
                } else {
                    false
                }
            }
            _ => false,
        },
        ExpressionKind::Cast { expr: inner, .. } => contains_variable_ref(inner, var_id),
        ExpressionKind::Binary { left, right, .. } => {
            contains_variable_ref(left, var_id) || contains_variable_ref(right, var_id)
        }
        ExpressionKind::Unary { operand, .. } => contains_variable_ref(operand, var_id),
        ExpressionKind::Conditional {
            cond,
            then_expr,
            else_expr,
        } => {
            contains_variable_ref(cond, var_id)
                || contains_variable_ref(then_expr, var_id)
                || contains_variable_ref(else_expr, var_id)
        }
        ExpressionKind::Concat(parts) => parts.iter().any(|p| contains_variable_ref(p, var_id)),
        _ => false,
    }
}

/// Convert an LValue to a signal name (fallback without module context)
#[allow(dead_code)]
fn lvalue_to_name(lvalue: &LValue) -> String {
    match lvalue {
        LValue::Signal(id) => format!("signal_{}", id.0),
        LValue::Port(id) => format!("port_{}", id.0),
        LValue::Variable(id) => format!("var_{}", id.0),
        LValue::BitSelect { base, index: _ } => {
            format!("{}[...]", lvalue_to_name(base))
        }
        LValue::RangeSelect {
            base,
            high: _,
            low: _,
        } => {
            format!("{}[...]", lvalue_to_name(base))
        }
        LValue::Concat(parts) => parts
            .iter()
            .map(lvalue_to_name)
            .collect::<Vec<_>>()
            .join("_"),
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
            compiled_ip_config: None,
            power_domains: Vec::new(),
            power_domain_config: None,
            safety_context: None,
            is_async: false,
            ncl_boundary_mode: None,
            barriers: Vec::new(),
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
            compiled_ip_config: None,
            power_domains: Vec::new(),
            power_domain_config: None,
            safety_context: None,
            is_async: false,
            ncl_boundary_mode: None,
            barriers: Vec::new(),
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
            compiled_ip_config: None,
            power_domains: Vec::new(),
            power_domain_config: None,
            safety_context: None,
            is_async: false,
            ncl_boundary_mode: None,
            barriers: Vec::new(),
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
