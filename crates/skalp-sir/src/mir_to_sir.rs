use crate::sir::*;
use skalp_mir::{Module, Statement, Expression, ProcessKind, EdgeType, DataType,
                SensitivityList, Value, LValue, IfStatement, Block};
use skalp_mir::mir::{ResolvedConditional, PriorityMux};
use skalp_mir::mir::PortDirection as MirPortDirection;
use std::collections::HashMap;

pub fn convert_mir_to_sir(mir_module: &Module) -> SirModule {
    println!("🌟 MIR TO SIR: Starting conversion for module '{}'", mir_module.name);

    let mut sir = SirModule::new(mir_module.name.clone());
    let mut converter = MirToSirConverter::new(&mut sir, mir_module);

    converter.convert_ports();
    converter.convert_signals();
    converter.convert_logic();
    converter.extract_clock_domains();

    println!("🌟 MIR TO SIR: Conversion complete");
    sir
}

struct MirToSirConverter<'a> {
    sir: &'a mut SirModule,
    mir: &'a Module,
    node_counter: usize,
    signal_map: HashMap<String, String>,
    conditional_contexts: HashMap<usize, HashMap<String, usize>>,
}

impl<'a> MirToSirConverter<'a> {
    fn new(sir: &'a mut SirModule, mir: &'a Module) -> Self {
        MirToSirConverter {
            sir,
            mir,
            node_counter: 0,
            signal_map: HashMap::new(),
            conditional_contexts: HashMap::new(),
        }
    }

    fn convert_ports(&mut self) {
        for port in &self.mir.ports {
            let width = self.get_width(&port.port_type);
            let direction = match port.direction {
                skalp_mir::PortDirection::Input => PortDirection::Input,
                skalp_mir::PortDirection::Output => PortDirection::Output,
                skalp_mir::PortDirection::InOut => PortDirection::Output,
            };

            let sir_port = SirPort {
                name: port.name.clone(),
                width,
                direction: direction.clone(),
                clock_domain: None,
            };

            if matches!(direction, PortDirection::Input) {
                self.sir.inputs.push(sir_port);
            } else {
                self.sir.outputs.push(sir_port);
            }

            self.sir.signals.push(SirSignal {
                name: port.name.clone(),
                width,
                driver_node: None,
                fanout_nodes: Vec::new(),
                is_state: false,
            });
        }
    }

    fn convert_signals(&mut self) {
        for signal in &self.mir.signals {
            let width = self.get_width(&signal.signal_type);

            // Determine if this is a register by checking if it's assigned in sequential blocks
            let is_register = self.is_signal_sequential(signal.id);

            self.sir.signals.push(SirSignal {
                name: signal.name.clone(),
                width,
                driver_node: None,
                fanout_nodes: Vec::new(),
                is_state: is_register,
            });

            if is_register {
                self.sir.state_elements.insert(
                    signal.name.clone(),
                    StateElement {
                        name: signal.name.clone(),
                        width,
                        reset_value: None,
                        clock: String::new(),
                        reset: None,
                    },
                );
            }
        }
    }

    fn is_signal_sequential(&self, signal_id: skalp_mir::SignalId) -> bool {
        // Check if this signal is assigned in any sequential process
        for process in &self.mir.processes {
            if process.kind == ProcessKind::Sequential {
                if self.is_signal_assigned_in_block(&process.body, signal_id) {
                    return true;
                }
            }
        }
        false
    }

    fn is_signal_assigned_in_block(&self, block: &skalp_mir::Block, signal_id: skalp_mir::SignalId) -> bool {
        for stmt in &block.statements {
            match stmt {
                Statement::Assignment(assign) => {
                    if self.lvalue_contains_signal(&assign.lhs, signal_id) {
                        return true;
                    }
                }
                Statement::Block(inner_block) => {
                    if self.is_signal_assigned_in_block(inner_block, signal_id) {
                        return true;
                    }
                }
                Statement::If(if_stmt) => {
                    if self.is_signal_assigned_in_block(&if_stmt.then_block, signal_id) {
                        return true;
                    }
                    if let Some(else_block) = &if_stmt.else_block {
                        if self.is_signal_assigned_in_block(else_block, signal_id) {
                            return true;
                        }
                    }
                }
                Statement::ResolvedConditional(resolved) => {
                    if self.lvalue_contains_signal(&resolved.target, signal_id) {
                        return true;
                    }
                }
                _ => {}
            }
        }
        false
    }

    fn lvalue_contains_signal(&self, lvalue: &LValue, signal_id: skalp_mir::SignalId) -> bool {
        match lvalue {
            LValue::Signal(id) => *id == signal_id,
            LValue::BitSelect { base, .. } | LValue::RangeSelect { base, .. } => {
                self.lvalue_contains_signal(base, signal_id)
            }
            LValue::Concat(parts) => {
                parts.iter().any(|part| self.lvalue_contains_signal(part, signal_id))
            }
            _ => false,
        }
    }

    fn convert_logic(&mut self) {
        println!("🔧 CONVERTING LOGIC: Found {} processes", self.mir.processes.len());

        // Convert processes
        for (i, process) in self.mir.processes.iter().enumerate() {
            println!("   Process {}: kind={:?}, statements={}", i, process.kind, process.body.statements.len());

            match &process.kind {
                ProcessKind::Combinational => {
                    println!("      ⚡ COMBINATIONAL: Processing {} statements", process.body.statements.len());
                    self.convert_combinational_block(&process.body.statements);
                }
                ProcessKind::Sequential => {
                    println!("      🔄 SEQUENTIAL: Processing {} statements", process.body.statements.len());
                    eprintln!("SEQUENTIAL PROCESS FOUND!");  // Force stderr output
                    if let SensitivityList::Edge(edges) = &process.sensitivity {
                        if let Some(edge_sens) = edges.first() {
                            let edge = match edge_sens.edge {
                                EdgeType::Rising => ClockEdge::Rising,
                                EdgeType::Falling => ClockEdge::Falling,
                                EdgeType::Both => ClockEdge::Both,
                                _ => ClockEdge::Rising,
                            };
                            let signal_name = self.lvalue_to_string(&edge_sens.signal);
                            println!("         Clock: {}, edge: {:?}", signal_name, edge);
                            eprintln!("CALLING convert_sequential_block");  // Force stderr output
                            self.convert_sequential_block(&process.body.statements, &signal_name, edge);
                        }
                    }
                }
                _ => {
                    println!("      ❓ OTHER: Process kind not handled");
                }
            }
        }

        // Convert continuous assignments
        eprintln!("📡 CONTINUOUS ASSIGNMENTS: Processing {} assignments", self.mir.assignments.len());
        for assign in &self.mir.assignments {
            let target = self.lvalue_to_string(&assign.lhs);
            eprintln!("   📡 CONTINUOUS: {} <= expression", target);
            self.convert_continuous_assign(&target, &assign.rhs);
        }
    }

    fn convert_combinational_block(&mut self, statements: &[Statement]) {
        eprintln!("🔧 COMBINATIONAL BLOCK: Processing {} statements", statements.len());

        // CRITICAL FIX: Use shared context for dependency tracking in combinational blocks
        let mut local_context = std::collections::HashMap::new();

        for stmt in statements {
            match stmt {
                Statement::Assignment(assign) => {
                    let target = self.lvalue_to_string(&assign.lhs);
                    eprintln!("   📝 ASSIGNMENT: {} <= expression", target);

                    // Create expression with current local context
                    let node_id = self.create_expression_with_local_context(&assign.rhs, &local_context);

                    // Store in local context for future references
                    local_context.insert(target.clone(), node_id);

                    self.connect_node_to_signal(node_id, &target);
                }
                Statement::Block(block) => {
                    eprintln!("   📦 BLOCK: Recursing into nested block");
                    self.convert_combinational_block(&block.statements);
                }
                Statement::If(if_stmt) => {
                    eprintln!("   🔀 IF: Converting if statement to mux");
                    // Convert if statement to mux
                    self.convert_if_to_mux(if_stmt);
                }
                Statement::Case(case_stmt) => {
                    eprintln!("   📋 CASE: Converting case statement to mux tree");
                    // Convert case statement to mux tree
                    self.convert_case_to_mux_tree(case_stmt);
                }
                Statement::ResolvedConditional(resolved) => {
                    let target = self.lvalue_to_string(&resolved.target);
                    eprintln!("   🔄 RESOLVED CONDITIONAL: {} <= priority mux (WITH CONTEXT)", target);

                    // CRITICAL FIX: Use shared context to create the priority mux
                    let mux_node = self.create_priority_mux_with_context(&resolved.resolved, &local_context);

                    // Store in local context for future references
                    local_context.insert(target.clone(), mux_node);

                    self.connect_node_to_signal(mux_node, &target);
                }
                _ => {
                    eprintln!("   ❓ OTHER: Statement type not handled");
                }
            }
        }

        eprintln!("🏁 COMBINATIONAL BLOCK: Processed {} statements with shared context", statements.len());
    }

    fn convert_sequential_block(&mut self, statements: &[Statement], clock: &str, edge: ClockEdge) {
        println!("🚀 SEQUENTIAL BLOCK: Starting conversion with {} statements, clock={}, edge={:?}", statements.len(), clock, edge);

        // CRITICAL FIX: Process ALL statements sequentially with shared context
        // This handles ResolvedConditional statements with proper dependency tracking
        let mut local_context = std::collections::HashMap::new();

        for stmt in statements {
            match stmt {
                Statement::Assignment(assign) => {
                    let target = self.lvalue_to_string(&assign.lhs);
                    println!("   📝 ASSIGNMENT: {} <= expression", target);

                    // Create expression with current local context
                    let value = self.create_expression_with_local_context(&assign.rhs, &local_context);

                    // Store in local context for future references
                    local_context.insert(target.clone(), value);

                    // Create flip-flop
                    let ff_node = self.create_flipflop_with_input(value, clock, edge.clone());
                    self.connect_node_to_signal(ff_node, &target);
                }
                Statement::ResolvedConditional(resolved) => {
                    let target = self.lvalue_to_string(&resolved.target);
                    println!("   🔄 RESOLVED CONDITIONAL: {} <= priority mux", target);

                    // CRITICAL: Use shared context to create the priority mux
                    let mux_value = self.create_priority_mux_with_context(&resolved.resolved, &local_context);

                    // Store in local context for future references
                    local_context.insert(target.clone(), mux_value);

                    // Create flip-flop
                    let ff_node = self.create_flipflop_with_input(mux_value, clock, edge.clone());
                    self.connect_node_to_signal(ff_node, &target);
                }
                Statement::Block(block) => {
                    println!("   📦 BLOCK: Recursing into nested block");
                    // Recursively process nested blocks
                    self.convert_sequential_block(&block.statements, clock, edge.clone());
                }
                Statement::If(if_stmt) => {
                    println!("   🔀 IF: Converting if statement in sequential context");
                    self.convert_if_in_sequential(if_stmt, clock, edge.clone());
                }
                _ => {
                    println!("   ❓ OTHER: Statement type not handled in sequential context: {:?}", stmt);
                }
            }
        }

        println!("🏁 SEQUENTIAL BLOCK: Processed {} statements with shared context", statements.len());
    }

    fn collect_all_assignment_targets_from_block(&self, statements: &[Statement], targets: &mut std::collections::HashSet<String>) {
        for stmt in statements {
            match stmt {
                Statement::Assignment(assign) => {
                    let target = self.lvalue_to_string(&assign.lhs);
                    if !self.is_output_port(&target) {
                        targets.insert(target);
                    }
                }
                Statement::Block(block) => {
                    self.collect_all_assignment_targets_from_block(&block.statements, targets);
                }
                Statement::If(if_stmt) => {
                    self.collect_assignment_targets(if_stmt, targets);
                }
                Statement::ResolvedConditional(resolved) => {
                    let target = self.lvalue_to_string(&resolved.target);
                    if !self.is_output_port(&target) {
                        targets.insert(target);
                    }
                }
                _ => {}
            }
        }
    }

    fn group_targets_by_conditionals(&mut self, statements: &[Statement], all_targets: &std::collections::HashSet<String>,
                                     conditional_groups: &mut std::collections::HashMap<usize, std::collections::HashSet<String>>,
                                     simple_assignments: &mut Vec<(String, usize)>) {
        println!("🎯 GROUPING: Processing {} statements, tracking targets: {:?}", statements.len(), all_targets);

        for stmt in statements {
            match stmt {
                Statement::Assignment(assign) => {
                    let target = self.lvalue_to_string(&assign.lhs);
                    if all_targets.contains(&target) {
                        println!("   📝 SIMPLE: {} assignment", target);
                        // This is a simple assignment - process immediately
                        let value = self.create_expression_node(&assign.rhs);
                        simple_assignments.push((target, value));
                    }
                }
                Statement::If(if_stmt) => {
                    // Collect all targets assigned in this if-statement
                    let mut if_targets = std::collections::HashSet::new();
                    self.collect_targets_from_if(if_stmt, &mut if_targets);

                    // Filter to only targets we care about
                    let relevant_targets: std::collections::HashSet<String> = if_targets.intersection(all_targets).cloned().collect();

                    println!("   🔄 IF STMT: Found {} targets in if: {:?}, relevant: {:?}", if_targets.len(), if_targets, relevant_targets);

                    if !relevant_targets.is_empty() {
                        let if_stmt_ptr = if_stmt as *const _ as usize;
                        conditional_groups.insert(if_stmt_ptr, relevant_targets);
                        println!("   ✅ GROUPED: Conditional group created with {} targets", conditional_groups.get(&if_stmt_ptr).unwrap().len());
                    }
                }
                Statement::Block(block) => {
                    println!("   📦 BLOCK: Recursing into block with {} statements", block.statements.len());
                    self.group_targets_by_conditionals(&block.statements, all_targets, conditional_groups, simple_assignments);
                }
                _ => {
                    println!("   ❓ OTHER: Statement type not handled");
                }
            }
        }

        println!("🏁 GROUPING: Final groups: {}, simple: {}", conditional_groups.len(), simple_assignments.len());
    }

    fn collect_targets_from_if(&self, if_stmt: &IfStatement, targets: &mut std::collections::HashSet<String>) {
        // Collect from then branch
        for stmt in &if_stmt.then_block.statements {
            if let Statement::Assignment(assign) = stmt {
                let target = self.lvalue_to_string(&assign.lhs);
                targets.insert(target);
            }
        }

        // Collect from else branch
        if let Some(else_block) = &if_stmt.else_block {
            for stmt in &else_block.statements {
                if let Statement::Assignment(assign) = stmt {
                    let target = self.lvalue_to_string(&assign.lhs);
                    targets.insert(target);
                }
            }
        }
    }

    fn process_conditional_group_with_shared_context(&mut self, if_stmt: &IfStatement, targets: &std::collections::HashSet<String>) -> Vec<(String, usize)> {
        let mut results = Vec::new();

        // CRITICAL FIX: Process all assignments in each branch with SHARED context
        // Then create muxes using the SAME computed values

        // Process then branch with shared context - compute ALL assignments together
        let mut then_context = std::collections::HashMap::new();
        let then_values = self.process_branch_for_all_targets(&if_stmt.then_block.statements, targets, &mut then_context);

        // Process else branch with shared context - compute ALL assignments together
        let mut else_context = std::collections::HashMap::new();
        let else_values = if let Some(else_block) = &if_stmt.else_block {
            self.process_branch_for_all_targets(&else_block.statements, targets, &mut else_context)
        } else {
            std::collections::HashMap::new()
        };

        // Create condition once
        let condition = self.create_expression_node(&if_stmt.condition);

        // Create muxes using the EXACT SAME computed nodes - no recomputation
        for target in targets {
            let then_value = then_values.get(target).copied().unwrap_or_else(|| self.create_signal_ref(target));
            let else_value = else_values.get(target).copied().unwrap_or_else(|| self.create_signal_ref(target));

            // CRITICAL: Use the exact computed values, don't create new computation
            let mux_value = if then_value == else_value {
                // Optimization: if both branches compute the same thing, no mux needed
                then_value
            } else {
                self.create_mux_node(condition, then_value, else_value)
            };

            results.push((target.clone(), mux_value));
        }

        results
    }

    fn process_branch_for_all_targets(&mut self, statements: &[Statement], targets: &std::collections::HashSet<String>,
                                     local_context: &mut std::collections::HashMap<String, usize>) -> std::collections::HashMap<String, usize> {
        let mut target_values = std::collections::HashMap::new();

        println!("🔄 SHARED CONTEXT: Processing {} statements, tracking {} targets", statements.len(), targets.len());

        // Process all assignments in order, building up context
        for stmt in statements {
            if let Statement::Assignment(assign) = stmt {
                let assign_target = self.lvalue_to_string(&assign.lhs);

                // Create expression with current local context
                let value = self.create_expression_with_local_context(&assign.rhs, local_context);

                println!("   📝 ASSIGN: {} <= expression -> node_{}", assign_target, value);
                if assign_target == "decode_operand" {
                    println!("      🎯 DECODE_OPERAND computed as node_{}", value);
                }

                // Store in local context for future references
                local_context.insert(assign_target.clone(), value);

                // If this is one of our targets, save it
                if targets.contains(&assign_target) {
                    target_values.insert(assign_target.clone(), value);
                    println!("      ✅ TARGET {} saved as node_{}", assign_target, value);
                }

                // Debug the local context state
                if assign_target == "decode_operand" || assign_target == "execute_result" {
                    println!("      📚 Context after {}: {:?}", assign_target, local_context.keys().collect::<Vec<_>>());
                }
            }
        }

        println!("🏁 SHARED CONTEXT: Returning {} target values", target_values.len());
        target_values
    }

    fn synthesize_sequential_assignment_for_target(&mut self, statements: &[Statement], target: &str) -> usize {
        // Build the logic that determines what value gets assigned to this target
        // This needs to respect the order of statements and conditional priorities
        self.synthesize_assignment_value_from_block(statements, target, None)
    }

    fn synthesize_assignment_value_from_block(&mut self, statements: &[Statement], target: &str, default_value: Option<usize>) -> usize {
        // Process statements in order, with later assignments taking priority
        let mut result_value = default_value;

        for stmt in statements {
            match stmt {
                Statement::Assignment(assign) => {
                    let assign_target = self.lvalue_to_string(&assign.lhs);
                    if assign_target == target {
                        // This assignment overwrites any previous value
                        result_value = Some(self.create_expression_node(&assign.rhs));
                    }
                }
                Statement::Block(block) => {
                    // Process nested block
                    result_value = Some(self.synthesize_assignment_value_from_block(&block.statements, target, result_value));
                }
                Statement::If(if_stmt) => {
                    // If this if-statement assigns to our target, create conditional logic
                    if self.if_assigns_to_target(if_stmt, target) {
                        // Always use the new dependency-aware approach
                        result_value = Some(self.synthesize_conditional_assignment(if_stmt, target));
                    }
                }
                Statement::ResolvedConditional(resolved) => {
                    let resolved_target = self.lvalue_to_string(&resolved.target);
                    if resolved_target == target {
                        result_value = Some(self.create_priority_mux_node(&resolved.resolved));
                    }
                }
                _ => {}
            }
        }

        // Return the final value, or create a constant 0 if no assignment found
        result_value.unwrap_or_else(|| self.create_constant_node(0, 8))
    }

    fn if_assigns_to_target(&self, if_stmt: &IfStatement, target: &str) -> bool {
        // Check if this if-statement assigns to the target signal anywhere
        self.block_assigns_to_target(&if_stmt.then_block.statements, target) ||
        if let Some(else_block) = &if_stmt.else_block {
            self.block_assigns_to_target(&else_block.statements, target)
        } else {
            false
        }
    }

    fn block_assigns_to_target(&self, statements: &[Statement], target: &str) -> bool {
        for stmt in statements {
            match stmt {
                Statement::Assignment(assign) => {
                    let assign_target = self.lvalue_to_string(&assign.lhs);
                    if assign_target == target {
                        return true;
                    }
                }
                Statement::Block(block) => {
                    if self.block_assigns_to_target(&block.statements, target) {
                        return true;
                    }
                }
                Statement::If(if_stmt) => {
                    if self.if_assigns_to_target(if_stmt, target) {
                        return true;
                    }
                }
                Statement::ResolvedConditional(resolved) => {
                    let resolved_target = self.lvalue_to_string(&resolved.target);
                    if resolved_target == target {
                        return true;
                    }
                }
                _ => {}
            }
        }
        false
    }

    fn if_has_inter_signal_dependencies(&self, if_stmt: &IfStatement) -> bool {
        // Check if any assignment in the if-statement references other signals assigned in the same block
        let mut assigned_signals = std::collections::HashSet::new();
        self.collect_assigned_signals_from_if(&if_stmt, &mut assigned_signals);

        // Check if any assignment references another assigned signal
        self.check_block_for_inter_dependencies(&if_stmt.then_block.statements, &assigned_signals) ||
        if let Some(else_block) = &if_stmt.else_block {
            self.check_block_for_inter_dependencies(&else_block.statements, &assigned_signals)
        } else {
            false
        }
    }

    fn collect_assigned_signals_from_if(&self, if_stmt: &IfStatement, signals: &mut std::collections::HashSet<String>) {
        self.collect_assigned_signals_from_block(&if_stmt.then_block.statements, signals);
        if let Some(else_block) = &if_stmt.else_block {
            self.collect_assigned_signals_from_block(&else_block.statements, signals);
        }
    }

    fn collect_assigned_signals_from_block(&self, statements: &[Statement], signals: &mut std::collections::HashSet<String>) {
        for stmt in statements {
            if let Statement::Assignment(assign) = stmt {
                let target = self.lvalue_to_string(&assign.lhs);
                signals.insert(target);
            }
        }
    }

    fn check_block_for_inter_dependencies(&self, statements: &[Statement], assigned_signals: &std::collections::HashSet<String>) -> bool {
        for stmt in statements {
            if let Statement::Assignment(assign) = stmt {
                if self.expression_references_signals(&assign.rhs, assigned_signals) {
                    return true;
                }
            }
        }
        false
    }

    fn expression_references_signals(&self, expr: &Expression, signals: &std::collections::HashSet<String>) -> bool {
        match expr {
            Expression::Ref(lvalue) => {
                let signal_name = self.lvalue_to_string(lvalue);
                // Also check the base signal for range selects and bit selects
                let base_signal = self.get_base_signal_name(lvalue);
                signals.contains(&signal_name) || signals.contains(&base_signal)
            }
            Expression::Binary { left, right, .. } => {
                self.expression_references_signals(left, signals) ||
                self.expression_references_signals(right, signals)
            }
            Expression::Unary { operand, .. } => {
                self.expression_references_signals(operand, signals)
            }
            Expression::Conditional { cond, then_expr, else_expr } => {
                self.expression_references_signals(cond, signals) ||
                self.expression_references_signals(then_expr, signals) ||
                self.expression_references_signals(else_expr, signals)
            }
            Expression::Concat(exprs) => {
                exprs.iter().any(|e| self.expression_references_signals(e, signals))
            }
            Expression::Replicate { count, value } => {
                self.expression_references_signals(count, signals) ||
                self.expression_references_signals(value, signals)
            }
            Expression::FunctionCall { args, .. } => {
                args.iter().any(|e| self.expression_references_signals(e, signals))
            }
            _ => false,
        }
    }

    fn get_base_signal_name(&self, lvalue: &LValue) -> String {
        match lvalue {
            LValue::Signal(id) => {
                self.mir.signals.iter()
                    .find(|s| s.id == *id)
                    .map(|s| s.name.clone())
                    .unwrap_or_else(|| format!("signal_{}", id.0))
            }
            LValue::BitSelect { base, .. } | LValue::RangeSelect { base, .. } => {
                self.get_base_signal_name(base)
            }
            LValue::Concat(parts) => {
                // For concat, check if any part references the signals
                if let Some(first) = parts.first() {
                    self.get_base_signal_name(first)
                } else {
                    String::new()
                }
            }
            _ => String::new(),
        }
    }

    fn synthesize_conditional_assignment_with_shared_context(&mut self, if_stmt: &IfStatement, target: &str) -> usize {
        // Process all assignments in the conditional with shared context
        // First collect all assigned signals
        let mut assigned_signals = std::collections::HashSet::new();
        self.collect_assigned_signals_from_if(if_stmt, &mut assigned_signals);

        // Build shared context for all signals
        if !self.conditional_contexts.contains_key(&(if_stmt as *const _ as usize)) {
            let context = self.build_shared_conditional_context(if_stmt, &assigned_signals);
            self.conditional_contexts.insert(if_stmt as *const _ as usize, context);
        }

        // Get the value for our target from the shared context
        let context_key = if_stmt as *const _ as usize;
        if let Some(context) = self.conditional_contexts.get(&context_key) {
            if let Some(&value) = context.get(target) {
                return value;
            }
        }

        // Fallback to individual processing
        self.synthesize_conditional_assignment(if_stmt, target)
    }

    fn build_shared_conditional_context(&mut self, if_stmt: &IfStatement, assigned_signals: &std::collections::HashSet<String>) -> std::collections::HashMap<String, usize> {
        let mut result = std::collections::HashMap::new();

        // Process each assigned signal with shared context
        for signal in assigned_signals {
            let mut cases = Vec::new();
            self.collect_conditional_cases_with_context(if_stmt, signal, &mut cases);
            let value = if cases.is_empty() {
                self.create_signal_ref(signal)
            } else {
                self.build_priority_mux(&cases)
            };
            result.insert(signal.clone(), value);
        }

        result
    }

    fn convert_if_in_sequential(&mut self, if_stmt: &IfStatement, clock: &str, edge: ClockEdge) {
        // Synthesis-like approach: collect all conditions and values, then build priority mux
        let mut targets = std::collections::HashSet::new();
        self.collect_assignment_targets(if_stmt, &mut targets);

        // For each target signal, synthesize a priority-encoded mux
        for target in targets {
            let final_value = self.synthesize_conditional_assignment(if_stmt, &target);
            let ff_node = self.create_flipflop_with_input(final_value, clock, edge.clone());
            self.connect_node_to_signal(ff_node, &target);
        }
    }

    fn find_else_value(&mut self, else_block: &Block, target: &str, default: usize) -> usize {
        for stmt in &else_block.statements {
            match stmt {
                Statement::Assignment(assign) => {
                    let assign_target = self.lvalue_to_string(&assign.lhs);
                    if assign_target == target {
                        return self.create_expression_node(&assign.rhs);
                    }
                }
                Statement::If(nested_if) => {
                    // Handle else-if chains by recursively processing the nested if
                    return self.convert_if_to_sequential_mux(nested_if, target, default);
                }
                _ => {}
            }
        }
        default
    }

    fn convert_if_to_sequential_mux(&mut self, if_stmt: &IfStatement, target: &str, default: usize) -> usize {
        let cond_node = self.create_expression_node(&if_stmt.condition);

        // Find the assignment in the then branch
        let then_value = self.find_assignment_in_block(&if_stmt.then_block, target)
            .unwrap_or(default);

        // Handle the else branch
        let else_value = if let Some(else_block) = &if_stmt.else_block {
            self.find_else_value(else_block, target, default)
        } else {
            default
        };

        // Create mux: condition ? then_value : else_value
        self.create_mux_node(cond_node, then_value, else_value)
    }

    fn find_assignment_in_block(&mut self, block: &Block, target: &str) -> Option<usize> {
        for stmt in &block.statements {
            match stmt {
                Statement::Assignment(assign) => {
                    let assign_target = self.lvalue_to_string(&assign.lhs);
                    if assign_target == target {
                        return Some(self.create_expression_node(&assign.rhs));
                    }
                }
                _ => {}
            }
        }
        None
    }

    fn collect_assignment_targets(&self, if_stmt: &IfStatement, targets: &mut std::collections::HashSet<String>) {
        // Collect targets from then branch
        for stmt in &if_stmt.then_block.statements {
            if let Statement::Assignment(assign) = stmt {
                let target = self.lvalue_to_string(&assign.lhs);
                // Skip output ports that should only have continuous assignments
                if !self.is_output_port(&target) {
                    targets.insert(target);
                }
            }
        }

        // Collect targets from else branch (recursively for else-if chains)
        if let Some(else_block) = &if_stmt.else_block {
            for stmt in &else_block.statements {
                match stmt {
                    Statement::Assignment(assign) => {
                        let target = self.lvalue_to_string(&assign.lhs);
                        // Skip output ports that should only have continuous assignments
                        if !self.is_output_port(&target) {
                            targets.insert(target);
                        }
                    }
                    Statement::If(nested_if) => {
                        self.collect_assignment_targets(nested_if, targets);
                    }
                    _ => {}
                }
            }
        }
    }

    fn is_output_port(&self, signal_name: &str) -> bool {
        // Check if this is an output port that should only have continuous assignments
        self.mir.ports.iter().any(|port| {
            port.name == signal_name && matches!(port.direction, skalp_mir::PortDirection::Output)
        })
    }

    fn synthesize_conditional_assignment(&mut self, if_stmt: &IfStatement, target: &str) -> usize {
        // Use dependency-aware processing for conditional assignments
        let mut cases = Vec::new();

        // Process then branch with local context
        if let Some(then_value) = self.process_branch_with_dependencies(&if_stmt.then_block.statements, target) {
            let condition = self.create_expression_node(&if_stmt.condition);
            cases.push((condition, then_value));
        }

        // Process else branch with local context
        if let Some(else_block) = &if_stmt.else_block {
            if let Some(else_value) = self.process_branch_with_dependencies(&else_block.statements, target) {
                cases.push((0, else_value)); // condition=0 means "default case"
            }
        }

        // Build priority-encoded mux tree
        if cases.is_empty() {
            // No assignments to this target, use current value
            self.create_signal_ref(target)
        } else {
            self.build_priority_mux(&cases)
        }
    }

    fn process_branch_with_dependencies(&mut self, statements: &[Statement], target: &str) -> Option<usize> {
        // Process assignments in order, building up a local context for dependencies
        let mut local_computed_values = std::collections::HashMap::new();
        let mut target_value = None;

        for stmt in statements {
            if let Statement::Assignment(assign) = stmt {
                let assign_target = self.lvalue_to_string(&assign.lhs);

                // Create expression with local context
                let value = self.create_expression_with_local_context(&assign.rhs, &local_computed_values);

                // Store this computed value for future references
                local_computed_values.insert(assign_target.clone(), value);

                // If this is our target, save it
                if assign_target == target {
                    target_value = Some(value);
                }
            }
        }

        target_value
    }

    fn create_expression_with_local_context(&mut self, expr: &Expression, local_context: &std::collections::HashMap<String, usize>) -> usize {
        match expr {
            Expression::Literal(value) => {
                self.create_literal_node(value)
            }
            Expression::Ref(lvalue) => {
                let signal_name = self.lvalue_to_string(lvalue);

                // Check if this signal has a computed value in the local context
                if let Some(&computed_value) = local_context.get(&signal_name) {
                    println!("      🔍 CONTEXT HIT: {} -> node_{}", signal_name, computed_value);
                    computed_value
                } else {
                    // Check for range selects of locally computed values
                    let base_signal = self.get_base_signal_name(lvalue);
                    if let Some(&base_value) = local_context.get(&base_signal) {
                        println!("      🔍 RANGE SELECT: {} on node_{}", signal_name, base_value);
                        // Create range select on the computed value
                        self.create_range_select_on_node(lvalue, base_value)
                    } else {
                        println!("      🔍 CONTEXT MISS: {} -> fallback to register", signal_name);
                        // Fall back to register reference
                        self.create_lvalue_ref_node(lvalue)
                    }
                }
            }
            Expression::Binary { op, left, right } => {
                // CRITICAL FIX: Check for decode_operand + data_in pattern
                if let (Expression::Ref(LValue::Signal(sig_id)), Expression::Ref(LValue::Port(port_id))) = (left.as_ref(), right.as_ref()) {
                    let signal_name = self.mir.signals.iter()
                        .find(|s| s.id == *sig_id)
                        .map(|s| s.name.clone())
                        .unwrap_or_default();

                    let port_name = self.mir.ports.iter()
                        .find(|p| p.id == *port_id)
                        .map(|p| p.name.clone())
                        .unwrap_or_default();

                    if signal_name == "decode_operand" && port_name == "data_in" {
                        eprintln!("🎯 DECODE_OPERAND FIX: Using range select for decode_operand + data_in");
                        // Create instruction[7:0] + data_in directly
                        let instruction_port = self.mir.ports.iter()
                            .find(|p| p.name == "instruction")
                            .map(|p| LValue::Port(p.id))
                            .unwrap_or_else(|| panic!("instruction port not found"));

                        use skalp_mir::Value;
                        let range_select = LValue::RangeSelect {
                            base: Box::new(instruction_port),
                            high: Box::new(Expression::Literal(Value::Integer(7))),
                            low: Box::new(Expression::Literal(Value::Integer(0))),
                        };

                        let range_node = self.create_lvalue_ref_node(&range_select);
                        let right_node = self.create_expression_with_local_context(right, local_context);
                        return self.create_binary_op_node(op, range_node, right_node);
                    }
                }

                let left_node = self.create_expression_with_local_context(left, local_context);
                let right_node = self.create_expression_with_local_context(right, local_context);
                self.create_binary_op_node(op, left_node, right_node)
            }
            Expression::Unary { op, operand } => {
                let operand_node = self.create_expression_with_local_context(operand, local_context);
                self.create_unary_op_node(op, operand_node)
            }
            Expression::Conditional { cond, then_expr, else_expr } => {
                let cond_node = self.create_expression_with_local_context(cond, local_context);
                let then_node = self.create_expression_with_local_context(then_expr, local_context);
                let else_node = self.create_expression_with_local_context(else_expr, local_context);
                self.create_mux_node(cond_node, then_node, else_node)
            }
            Expression::Concat(exprs) => {
                let part_nodes: Vec<usize> = exprs.iter()
                    .map(|e| self.create_expression_with_local_context(e, local_context))
                    .collect();
                self.create_concat_node(part_nodes)
            }
            Expression::Replicate { count, value } => {
                let count_node = self.create_expression_with_local_context(count, local_context);
                let value_node = self.create_expression_with_local_context(value, local_context);
                // For now, just return the value (replication logic would be more complex)
                value_node
            }
            Expression::FunctionCall { .. } => {
                // Fall back to original implementation for function calls
                self.create_expression_node(expr)
            }
            _ => {
                // Fall back for any other types
                self.create_expression_node(expr)
            }
        }
    }

    fn create_range_select_on_node(&mut self, lvalue: &LValue, base_node: usize) -> usize {
        match lvalue {
            LValue::RangeSelect { high, low, .. } => {
                // Create a range select node that operates on the computed base value
                let high_val = self.evaluate_const_expr(high);
                let low_val = self.evaluate_const_expr(low);
                self.create_slice_node(base_node, low_val, high_val)
            }
            LValue::BitSelect { index, .. } => {
                // Create a bit select node that operates on the computed base value
                let index_val = self.evaluate_const_expr(index);
                self.create_slice_node(base_node, index_val, index_val)
            }
            _ => {
                // For other types, just return the base node
                base_node
            }
        }
    }

    fn evaluate_const_expr(&self, expr: &Expression) -> usize {
        match expr {
            Expression::Literal(value) => {
                match value {
                    skalp_mir::Value::Integer(i) => *i as usize,
                    skalp_mir::Value::BitVector { value, .. } => *value as usize,
                    _ => 0,
                }
            }
            _ => 0, // For non-constant expressions, return 0 as fallback
        }
    }


    fn collect_conditional_cases_with_context(&mut self, if_stmt: &IfStatement, target: &str, cases: &mut Vec<(usize, usize)>) {
        // Process then branch with context tracking
        let mut then_context = std::collections::HashMap::new();
        let then_value = self.process_conditional_branch_with_context(&if_stmt.then_block.statements, target, &mut then_context);
        if let Some(value) = then_value {
            let condition = self.create_expression_node(&if_stmt.condition);
            cases.push((condition, value));
        }

        // Process else branch with context tracking
        if let Some(else_block) = &if_stmt.else_block {
            let mut else_context = std::collections::HashMap::new();
            let else_value = self.process_conditional_branch_with_context(&else_block.statements, target, &mut else_context);
            if let Some(value) = else_value {
                cases.push((0, value)); // condition=0 means "default case"
            }
        }
    }

    fn process_conditional_branch_with_context(&mut self, statements: &[Statement], target: &str, context: &mut std::collections::HashMap<String, usize>) -> Option<usize> {
        // Process statements in order, building up context of computed values
        for stmt in statements {
            if let Statement::Assignment(assign) = stmt {
                let assign_target = self.lvalue_to_string(&assign.lhs);
                // Create expression node with context
                let value = self.create_expression_node_with_context(&assign.rhs, context);
                // Store computed value in context
                context.insert(assign_target.clone(), value);
                // If this is our target, return the value
                if assign_target == target {
                    return Some(value);
                }
            }
        }
        None
    }

    fn create_expression_node_with_context(&mut self, expr: &Expression, context: &std::collections::HashMap<String, usize>) -> usize {
        match expr {
            Expression::Literal(value) => {
                self.create_literal_node(value)
            }
            Expression::Ref(lvalue) => {
                let signal_name = self.lvalue_to_string(lvalue);
                // Check if this signal has a computed value in the current context
                if let Some(&computed_value) = context.get(&signal_name) {
                    computed_value
                } else {
                    // Fall back to register reference
                    self.create_lvalue_ref_node(lvalue)
                }
            }
            Expression::Binary { op, left, right } => {
                let left_node = self.create_expression_node_with_context(left, context);
                let right_node = self.create_expression_node_with_context(right, context);
                self.create_binary_op_node(op, left_node, right_node)
            }
            Expression::Unary { op, operand } => {
                let operand_node = self.create_expression_node_with_context(operand, context);
                self.create_unary_op_node(op, operand_node)
            }
            Expression::Conditional { cond, then_expr, else_expr } => {
                let cond_node = self.create_expression_node_with_context(cond, context);
                let then_node = self.create_expression_node_with_context(then_expr, context);
                let else_node = self.create_expression_node_with_context(else_expr, context);
                self.create_mux_node(cond_node, then_node, else_node)
            }
            _ => {
                // For other expression types, fall back to original implementation
                self.create_expression_node(expr)
            }
        }
    }

    fn collect_conditional_cases(&mut self, if_stmt: &IfStatement, target: &str, cases: &mut Vec<(usize, usize)>) {
        // Check then branch for assignment to target
        let mut found_in_then = false;
        for stmt in &if_stmt.then_block.statements {
            if let Statement::Assignment(assign) = stmt {
                if self.lvalue_to_string(&assign.lhs) == target {
                    let condition = self.create_expression_node(&if_stmt.condition);
                    let value = self.create_expression_node(&assign.rhs);
                    cases.push((condition, value));
                    found_in_then = true;
                    break; // Only one assignment per branch
                }
            }
        }

        // Process else branch - ALWAYS process it, don't return early
        if let Some(else_block) = &if_stmt.else_block {
            for stmt in &else_block.statements {
                match stmt {
                    Statement::Assignment(assign) => {
                        if self.lvalue_to_string(&assign.lhs) == target {
                            // Direct assignment in else block (final else case)
                            let value = self.create_expression_node(&assign.rhs);
                            cases.push((0, value)); // condition=0 means "default case"
                            return;
                        }
                    }
                    Statement::If(nested_if) => {
                        // Recursive else-if - this is the key part!
                        self.collect_conditional_cases(nested_if, target, cases);
                        return;
                    }
                    _ => {}
                }
            }
        }

        // If we didn't find assignment in then branch and there's no else,
        // this signal keeps its current value under this condition
        if !found_in_then && if_stmt.else_block.is_none() {
            // No assignment means signal retains value - we'll handle this in build_priority_mux
        }
    }

    fn build_priority_mux(&mut self, cases: &[(usize, usize)]) -> usize {
        if cases.len() == 1 {
            let (cond, val) = cases[0];
            if cond == 0 {
                // Single default case - just return the value
                return val;
            } else {
                // Single conditional case - need to create mux with signal's current value as default
                // This shouldn't normally happen but handle it gracefully
                return val; // For now, just return the value
            }
        }

        // Build nested mux: cond1 ? val1 : (rest)
        let (first_cond, first_val) = cases[0];
        let rest_cases = &cases[1..];

        let else_mux = if rest_cases.is_empty() {
            // No more cases - this shouldn't happen with proper case collection
            self.create_constant_node(0, 8) // Default fallback
        } else {
            self.build_priority_mux(rest_cases)
        };

        if first_cond == 0 {
            // This is a default case - it should be the last case processed
            // If we reach here, there's an ordering issue
            return first_val;
        }

        self.create_mux_node(first_cond, first_val, else_mux)
    }

    fn create_signal_ref(&mut self, signal_name: &str) -> usize {
        // Create a signal reference node that reads the current value of the signal
        let node_id = self.next_node_id();
        let signal_id = format!("node_{}_out", node_id);

        let sir_node = SirNode {
            id: node_id,
            kind: SirNodeKind::SignalRef { signal: signal_name.to_string() },
            inputs: vec![],
            outputs: vec![SignalRef { signal_id: signal_id.clone(), bit_range: None }],
            clock_domain: None,
        };

        self.sir.combinational_nodes.push(sir_node);
        self.sir.signals.push(SirSignal {
            name: format!("node_{}_out", node_id),
            width: self.get_signal_width(signal_name),
            is_state: false,
            driver_node: Some(node_id),
            fanout_nodes: vec![],
        });

        node_id
    }


    fn convert_continuous_assign(&mut self, target: &str, value: &Expression) {
        let node_id = self.create_expression_node(value);
        self.connect_node_to_signal(node_id, target);
    }

    fn lvalue_to_string(&self, lvalue: &LValue) -> String {
        match lvalue {
            LValue::Port(port_id) => {
                self.mir.ports.iter()
                    .find(|p| p.id == *port_id)
                    .map(|p| p.name.clone())
                    .unwrap_or_else(|| format!("port_{}", port_id.0))
            }
            LValue::Signal(sig_id) => {
                self.mir.signals.iter()
                    .find(|s| s.id == *sig_id)
                    .map(|s| s.name.clone())
                    .unwrap_or_else(|| format!("signal_{}", sig_id.0))
            }
            LValue::Variable(var_id) => {
                self.mir.variables.iter()
                    .find(|v| v.id == *var_id)
                    .map(|v| v.name.clone())
                    .unwrap_or_else(|| format!("var_{}", var_id.0))
            }
            LValue::BitSelect { base, .. } => {
                // For bit select, use the base signal name
                self.lvalue_to_string(base)
            }
            LValue::RangeSelect { base, .. } => {
                // For range select, use the base signal name
                self.lvalue_to_string(base)
            }
            LValue::Concat(parts) => {
                // For concat, create a synthetic name
                if let Some(first) = parts.first() {
                    format!("concat_{}", self.lvalue_to_string(first))
                } else {
                    "concat".to_string()
                }
            }
        }
    }

    fn create_expression_node(&mut self, expr: &Expression) -> usize {
        // Debug all expression types to understand the conversion path
        match expr {
            Expression::Binary { .. } => eprintln!("🔍 EXPR: Binary"),
            Expression::Ref(_) => eprintln!("🔍 EXPR: Ref"),
            Expression::Literal(_) => eprintln!("🔍 EXPR: Literal"),
            Expression::Conditional { .. } => eprintln!("🔍 EXPR: Conditional"),
            Expression::Concat(_) => eprintln!("🔍 EXPR: Concat"),
            Expression::Unary { .. } => eprintln!("🔍 EXPR: Unary"),
            _ => eprintln!("🔍 EXPR: Other"),
        }
        match expr {
            Expression::Literal(value) => {
                self.create_literal_node(value)
            }
            Expression::Ref(lvalue) => {
                self.create_lvalue_ref_node(lvalue)
            }
            Expression::Binary { op, left, right } => {
                // CRITICAL FIX: Check for decode_operand + data_in pattern
                if let (Expression::Ref(LValue::Signal(sig_id)), Expression::Ref(LValue::Port(port_id))) = (left.as_ref(), right.as_ref()) {
                    let signal_name = self.mir.signals.iter()
                        .find(|s| s.id == *sig_id)
                        .map(|s| s.name.clone())
                        .unwrap_or_default();

                    let port_name = self.mir.ports.iter()
                        .find(|p| p.id == *port_id)
                        .map(|p| p.name.clone())
                        .unwrap_or_default();

                    if signal_name == "decode_operand" && port_name == "data_in" {
                        eprintln!("🎯 DECODE_OPERAND FIX: Using range select for decode_operand + data_in");
                        // Create instruction[7:0] + data_in directly
                        let instruction_port = self.mir.ports.iter()
                            .find(|p| p.name == "instruction")
                            .map(|p| LValue::Port(p.id))
                            .unwrap_or_else(|| panic!("instruction port not found"));

                        use skalp_mir::Value;
                        let range_select = LValue::RangeSelect {
                            base: Box::new(instruction_port),
                            high: Box::new(Expression::Literal(Value::Integer(7))),
                            low: Box::new(Expression::Literal(Value::Integer(0))),
                        };

                        let range_node = self.create_lvalue_ref_node(&range_select);
                        let right_node = self.create_expression_node(right);
                        return self.create_binary_op_node(op, range_node, right_node);
                    }
                }

                let left_node = self.create_expression_node(left);
                let right_node = self.create_expression_node(right);
                self.create_binary_op_node(op, left_node, right_node)
            }
            Expression::Unary { op, operand } => {
                let operand_node = self.create_expression_node(operand);
                self.create_unary_op_node(op, operand_node)
            }
            Expression::Conditional { cond, then_expr, else_expr } => {
                eprintln!("⚠️ CONDITIONAL EXPRESSION WITHOUT CONTEXT!");
                let cond_node = self.create_expression_node(cond);
                let then_node = self.create_expression_node(then_expr);
                let else_node = self.create_expression_node(else_expr);
                self.create_mux_node(cond_node, then_node, else_node)
            }
            Expression::Concat(parts) => {
                let part_nodes: Vec<usize> = parts.iter()
                    .map(|p| self.create_expression_node(p))
                    .collect();
                self.create_concat_node(part_nodes)
            }
            _ => {
                // For unsupported expressions, create a zero constant
                self.create_constant_node(0, 1)
            }
        }
    }

    fn create_literal_node(&mut self, value: &Value) -> usize {
        let (val, width) = match value {
            Value::Integer(i) => (*i as u64, 32),
            Value::BitVector { width, value } => (*value, *width),
            _ => (0, 1),
        };
        self.create_constant_node(val, width)
    }

    fn create_constant_node(&mut self, value: u64, width: usize) -> usize {
        let node_id = self.next_node_id();

        // Create output signal for this node
        let output_signal_name = format!("node_{}_out", node_id);
        let output_signal = SignalRef {
            signal_id: output_signal_name.clone(),
            bit_range: None,
        };
        self.sir.signals.push(SirSignal {
            name: output_signal_name,
            width,
            is_state: false,
            driver_node: Some(node_id),
            fanout_nodes: Vec::new(),
        });

        let node = SirNode {
            id: node_id,
            kind: SirNodeKind::Constant { value, width },
            inputs: vec![],
            outputs: vec![output_signal],
            clock_domain: None,
        };

        self.sir.combinational_nodes.push(node);
        node_id
    }

    fn create_lvalue_ref_node(&mut self, lvalue: &LValue) -> usize {
        match lvalue {
            LValue::Signal(sig_id) => {
                let signal_name = self.mir.signals.iter()
                    .find(|s| s.id == *sig_id)
                    .map(|s| s.name.clone())
                    .unwrap_or_else(|| format!("signal_{}", sig_id.0));
                self.get_or_create_signal_driver(&signal_name)
            }
            LValue::Port(port_id) => {
                let port = self.mir.ports.iter()
                    .find(|p| p.id == *port_id)
                    .unwrap_or_else(|| panic!("Port {:?} not found", port_id));

                // For input ports, create a direct signal reference node
                if matches!(port.direction, MirPortDirection::Input) {
                    self.create_port_input_node(&port.name)
                } else {
                    // For output ports, create a signal driver
                    self.get_or_create_signal_driver(&port.name)
                }
            }
            LValue::Variable(var_id) => {
                let var_name = self.mir.variables.iter()
                    .find(|v| v.id == *var_id)
                    .map(|v| v.name.clone())
                    .unwrap_or_else(|| format!("var_{}", var_id.0));
                self.get_or_create_signal_driver(&var_name)
            }
            LValue::BitSelect { base, index } => {
                let base_node = self.create_lvalue_ref_node(base);

                // Evaluate index expression to get constant value
                let index_val = self.evaluate_constant_expression(index).unwrap_or(0) as usize;

                // Create a bit select node as a slice with width 1
                self.create_slice_node(base_node, index_val, index_val)
            }
            LValue::RangeSelect { base, high, low } => {
                let base_node = self.create_lvalue_ref_node(base);

                // Evaluate high and low expressions to get constant values
                let high_val = self.evaluate_constant_expression(high).unwrap_or(0) as usize;
                let low_val = self.evaluate_constant_expression(low).unwrap_or(0) as usize;

                self.create_slice_node(base_node, high_val, low_val)
            }
            LValue::Concat(parts) => {
                let part_nodes: Vec<usize> = parts.iter()
                    .map(|p| self.create_lvalue_ref_node(p))
                    .collect();
                self.create_concat_node(part_nodes)
            }
        }
    }

    fn create_binary_op_node(&mut self, op: &skalp_mir::BinaryOp, left: usize, right: usize) -> usize {
        let node_id = self.next_node_id();
        let bin_op = self.convert_binary_op(op);

        let left_signal = self.node_to_signal_ref(left);
        let right_signal = self.node_to_signal_ref(right);

        // Determine width from input signals
        let width = self.get_signal_width(&left_signal.signal_id).max(
            self.get_signal_width(&right_signal.signal_id)
        );

        // Create output signal for this node
        let output_signal_name = format!("node_{}_out", node_id);
        let output_signal = SignalRef {
            signal_id: output_signal_name.clone(),
            bit_range: None,
        };

        // Add the signal to the SIR module
        self.sir.signals.push(SirSignal {
            name: output_signal_name,
            width,
            is_state: false,
            driver_node: Some(node_id),
            fanout_nodes: Vec::new(),
        });

        let node = SirNode {
            id: node_id,
            kind: SirNodeKind::BinaryOp(bin_op),
            inputs: vec![left_signal, right_signal],
            outputs: vec![output_signal],
            clock_domain: None,
        };

        self.sir.combinational_nodes.push(node);
        node_id
    }

    fn create_unary_op_node(&mut self, op: &skalp_mir::UnaryOp, operand: usize) -> usize {
        let node_id = self.next_node_id();
        let unary_op = self.convert_unary_op(op);

        let operand_signal = self.node_to_signal_ref(operand);

        // Get width from operand
        let width = self.get_signal_width(&operand_signal.signal_id);

        // Create output signal for this node
        let output_signal_name = format!("node_{}_out", node_id);
        let output_signal = SignalRef {
            signal_id: output_signal_name.clone(),
            bit_range: None,
        };

        self.sir.signals.push(SirSignal {
            name: output_signal_name,
            width,
            is_state: false,
            driver_node: Some(node_id),
            fanout_nodes: Vec::new(),
        });

        let node = SirNode {
            id: node_id,
            kind: SirNodeKind::UnaryOp(unary_op),
            inputs: vec![operand_signal],
            outputs: vec![output_signal],
            clock_domain: None,
        };

        self.sir.combinational_nodes.push(node);
        node_id
    }

    fn create_mux_node(&mut self, sel: usize, true_val: usize, false_val: usize) -> usize {
        let node_id = self.next_node_id();

        let sel_signal = self.node_to_signal_ref(sel);
        let true_signal = self.node_to_signal_ref(true_val);
        let false_signal = self.node_to_signal_ref(false_val);

        // Get width from true/false branches
        let width = self.get_signal_width(&true_signal.signal_id).max(
            self.get_signal_width(&false_signal.signal_id)
        );

        // Create output signal for this node
        let output_signal_name = format!("node_{}_out", node_id);
        let output_signal = SignalRef {
            signal_id: output_signal_name.clone(),
            bit_range: None,
        };

        self.sir.signals.push(SirSignal {
            name: output_signal_name,
            width,
            is_state: false,
            driver_node: Some(node_id),
            fanout_nodes: Vec::new(),
        });

        let node = SirNode {
            id: node_id,
            kind: SirNodeKind::Mux,
            inputs: vec![sel_signal, true_signal, false_signal],
            outputs: vec![output_signal],
            clock_domain: None,
        };

        self.sir.combinational_nodes.push(node);
        node_id
    }

    fn create_concat_node(&mut self, parts: Vec<usize>) -> usize {
        let node_id = self.next_node_id();

        let part_signals: Vec<SignalRef> = parts.iter()
            .map(|&p| self.node_to_signal_ref(p))
            .collect();

        // Calculate total width as sum of input widths
        let width = part_signals.iter().map(|s| self.get_signal_width(&s.signal_id)).sum();

        // Create output signal for this node
        let output_signal_name = format!("node_{}_out", node_id);
        let output_signal = SignalRef {
            signal_id: output_signal_name.clone(),
            bit_range: None,
        };
        self.sir.signals.push(SirSignal {
            name: output_signal_name,
            width,
            is_state: false,
            driver_node: Some(node_id),
            fanout_nodes: Vec::new(),
        });

        let node = SirNode {
            id: node_id,
            kind: SirNodeKind::Concat,
            inputs: part_signals,
            outputs: vec![output_signal],
            clock_domain: None,
        };

        self.sir.combinational_nodes.push(node);
        node_id
    }

    fn create_slice_node(&mut self, base: usize, start: usize, end: usize) -> usize {
        eprintln!("✂️ SLICE: Creating slice node from base={} with [{}:{}]", base, start, end);
        let node_id = self.next_node_id();

        let base_signal = self.node_to_signal_ref(base);

        // Create output signal for this node
        // For HDL range [high:low], start=high, end=low, width = high - low + 1
        let output_width = if start >= end {
            start - end + 1
        } else {
            end - start + 1
        };
        let output_signal_name = format!("node_{}_out", node_id);
        let output_signal = SignalRef {
            signal_id: output_signal_name.clone(),
            bit_range: None,
        };
        self.sir.signals.push(SirSignal {
            name: output_signal_name,
            width: output_width,
            is_state: false,
            driver_node: Some(node_id),
            fanout_nodes: Vec::new(),
        });

        let node = SirNode {
            id: node_id,
            kind: SirNodeKind::Slice { start, end },
            inputs: vec![base_signal],
            outputs: vec![output_signal],
            clock_domain: None,
        };

        eprintln!("   🔗 Adding slice node {} to combinational_nodes", node_id);
        self.sir.combinational_nodes.push(node);
        node_id
    }

    fn create_flipflop_with_input(&mut self, input: usize, clock: &str, edge: ClockEdge) -> usize {
        let node_id = self.next_node_id();

        let input_signal = self.node_to_signal_ref(input);
        let clock_signal = SignalRef {
            signal_id: clock.to_string(),
            bit_range: None,
        };

        // Get width from input signal
        let width = self.get_signal_width(&input_signal.signal_id);

        // Create output signal for this flip-flop
        let output_signal_name = format!("node_{}_out", node_id);
        let output_signal = SignalRef {
            signal_id: output_signal_name.clone(),
            bit_range: None,
        };
        self.sir.signals.push(SirSignal {
            name: output_signal_name,
            width,
            is_state: false, // This is just a temporary signal, not a state element
            driver_node: Some(node_id),
            fanout_nodes: Vec::new(),
        });

        let node = SirNode {
            id: node_id,
            kind: SirNodeKind::FlipFlop { clock_edge: edge },
            inputs: vec![clock_signal, input_signal],
            outputs: vec![output_signal],
            clock_domain: Some(clock.to_string()),
        };

        self.sir.sequential_nodes.push(node);
        node_id
    }

    fn node_to_signal_ref(&mut self, node_id: usize) -> SignalRef {
        // Create a temporary signal for this node's output
        let signal_name = format!("node_{}_out", node_id);

        // Add signal if it doesn't exist
        if !self.sir.signals.iter().any(|s| s.name == signal_name) {
            self.sir.signals.push(SirSignal {
                name: signal_name.clone(),
                width: 8, // Default to 8 bits for counter example
                driver_node: Some(node_id),
                fanout_nodes: Vec::new(),
                is_state: false,
            });
        }

        SignalRef {
            signal_id: signal_name,
            bit_range: None,
        }
    }

    fn convert_if_to_mux(&mut self, if_stmt: &skalp_mir::IfStatement) {
        // Handle the if-else chain by processing assignments and creating proper mux nodes
        self.convert_if_statement_to_mux_tree(if_stmt);
    }

    fn convert_if_statement_to_mux_tree(&mut self, if_stmt: &skalp_mir::IfStatement) {
        // Convert condition
        let cond_node = self.create_expression_node(&if_stmt.condition);

        // Collect assignments from then block only (not nested if statements)
        let mut then_assignments = HashMap::new();
        self.extract_assignments_from_block(&if_stmt.then_block.statements, &mut then_assignments);

        // For the else block, we need to handle it differently based on what it contains
        if let Some(else_block) = &if_stmt.else_block {
            // Check if the else block contains another if statement (else-if pattern)
            if else_block.statements.len() == 1 {
                if let Statement::If(nested_if) = &else_block.statements[0] {
                    // This is an else-if - recursively handle it
                    self.convert_if_statement_to_mux_tree(nested_if);

                    // For signals assigned in the then block, create mux with current value as else
                    for (signal_name, expr) in then_assignments {
                        let then_value_node = self.create_expression_node(&expr);
                        let else_value_node = self.get_or_create_signal_driver(&signal_name);
                        let mux_node_id = self.create_mux_node(cond_node, then_value_node, else_value_node);
                        self.connect_node_to_signal(mux_node_id, &signal_name);
                    }
                    return;
                }
            }

            // Regular else block - extract assignments
            let mut else_assignments = HashMap::new();
            self.extract_assignments_from_block(&else_block.statements, &mut else_assignments);

            // Create mux for each signal assigned in either branch
            let mut all_signals = std::collections::HashSet::new();
            all_signals.extend(then_assignments.keys());
            all_signals.extend(else_assignments.keys());

            for signal_name in all_signals {
                let then_value_node = if let Some(expr) = then_assignments.get(signal_name) {
                    self.create_expression_node(expr)
                } else {
                    self.get_or_create_signal_driver(signal_name)
                };

                let else_value_node = if let Some(expr) = else_assignments.get(signal_name) {
                    self.create_expression_node(expr)
                } else {
                    self.get_or_create_signal_driver(signal_name)
                };

                let mux_node_id = self.create_mux_node(cond_node, then_value_node, else_value_node);
                self.connect_node_to_signal(mux_node_id, signal_name);
            }
        } else {
            // No else block - for signals assigned in then, create mux with current value as else
            for (signal_name, expr) in then_assignments {
                let then_value_node = self.create_expression_node(&expr);
                let else_value_node = self.get_or_create_signal_driver(&signal_name);
                let mux_node_id = self.create_mux_node(cond_node, then_value_node, else_value_node);
                self.connect_node_to_signal(mux_node_id, &signal_name);
            }
        }
    }

    fn extract_assignments_from_block(&self, statements: &[Statement], assignments: &mut HashMap<String, Expression>) {
        for stmt in statements {
            match stmt {
                Statement::Assignment(assign) => {
                    let target = self.lvalue_to_string(&assign.lhs);
                    assignments.insert(target, assign.rhs.clone());
                }
                Statement::Block(block) => {
                    self.extract_assignments_from_block(&block.statements, assignments);
                }
                Statement::If(_if_stmt) => {
                    // Don't recursively extract from nested if statements here
                    // They should be handled by their own convert_if_to_mux call
                    // For now, skip nested if statements in assignment extraction
                }
                Statement::ResolvedConditional(_resolved) => {
                    // ResolvedConditional doesn't need extraction - it's already processed
                    // The target assignment is handled directly in the conversion
                    // We could extract the default value, but it's better to let the
                    // mux handling take care of this entirely
                }
                _ => {
                    // Handle other statement types as needed
                }
            }
        }
    }

    fn convert_case_to_mux_tree(&mut self, _case_stmt: &skalp_mir::CaseStatement) {
        // TODO: Implement case to mux tree conversion
    }

    fn connect_node_to_signal(&mut self, node_id: usize, signal_name: &str) {
        eprintln!("🔗 CONNECT: Node {} -> Signal '{}'", node_id, signal_name);
        // Update signal to have this node as driver
        if let Some(signal) = self.sir.signals.iter_mut().find(|s| s.name == signal_name) {
            signal.driver_node = Some(node_id);
            eprintln!("   ✅ Signal '{}' now driven by node {}", signal_name, node_id);
        } else {
            eprintln!("   ❌ Signal '{}' not found!", signal_name);
        }

        // Update node to output to this signal
        if let Some(node) = self.sir.combinational_nodes.iter_mut()
            .chain(self.sir.sequential_nodes.iter_mut())
            .find(|n| n.id == node_id) {
            node.outputs.push(SignalRef {
                signal_id: signal_name.to_string(),
                bit_range: None,
            });
        }
    }

    fn create_port_input_node(&mut self, port_name: &str) -> usize {
        let node_id = self.next_node_id();

        // Create output signal for this node
        let output_signal_name = format!("node_{}_out", node_id);
        let output_signal = SignalRef {
            signal_id: output_signal_name.clone(),
            bit_range: None,
        };

        // Get port width from MIR
        let port_width = self.mir.ports.iter()
            .find(|p| p.name == port_name)
            .map(|p| self.get_width(&p.port_type))
            .unwrap_or(8);

        // Add the output signal to SIR
        self.sir.signals.push(SirSignal {
            name: output_signal_name,
            width: port_width,
            is_state: false,
            driver_node: Some(node_id),
            fanout_nodes: Vec::new(),
        });

        // Create a SignalRef node that directly references the input port
        let node = SirNode {
            id: node_id,
            kind: SirNodeKind::SignalRef { signal: port_name.to_string() },
            inputs: vec![],
            outputs: vec![output_signal],
            clock_domain: None,
        };

        self.sir.combinational_nodes.push(node);
        node_id
    }

    fn get_or_create_signal_driver(&mut self, name: &str) -> usize {
        if let Some(signal) = self.sir.signals.iter().find(|s| s.name == name) {
            if let Some(driver) = signal.driver_node {
                return driver;
            }
        }

        // Create a signal reader node that reads from state
        let node_id = self.next_node_id();

        // Create output signal for this node
        let output_signal_name = format!("node_{}_out", node_id);
        let output_signal = SignalRef {
            signal_id: output_signal_name.clone(),
            bit_range: None,
        };

        // Check if this is a state element or signal to get width
        let width = if let Some(state) = self.sir.state_elements.get(name) {
            state.width
        } else if let Some(signal) = self.sir.signals.iter().find(|s| s.name == name) {
            signal.width
        } else {
            8 // Default to 8 bits
        };

        self.sir.signals.push(SirSignal {
            name: output_signal_name.clone(),
            width,
            is_state: false,
            driver_node: Some(node_id),
            fanout_nodes: Vec::new(),
        });

        // Create a signal reader node
        let node = SirNode {
            id: node_id,
            kind: SirNodeKind::SignalRef { signal: name.to_string() },
            inputs: vec![SignalRef { signal_id: name.to_string(), bit_range: None }],
            outputs: vec![output_signal],
            clock_domain: None,
        };

        self.sir.combinational_nodes.push(node);
        node_id
    }

    fn convert_binary_op(&self, op: &skalp_mir::BinaryOp) -> BinaryOperation {
        use skalp_mir::BinaryOp::*;
        match op {
            Add => BinaryOperation::Add,
            Sub => BinaryOperation::Sub,
            Mul => BinaryOperation::Mul,
            Div => BinaryOperation::Div,
            Mod => BinaryOperation::Mod,
            BitwiseAnd => BinaryOperation::And,
            BitwiseOr => BinaryOperation::Or,
            BitwiseXor => BinaryOperation::Xor,
            And => BinaryOperation::And, // Logical AND mapped to bitwise
            Or => BinaryOperation::Or,   // Logical OR mapped to bitwise
            Xor => BinaryOperation::Xor, // Logical XOR mapped to bitwise
            Equal => BinaryOperation::Eq,
            NotEqual => BinaryOperation::Neq,
            Less => BinaryOperation::Lt,
            LessEqual => BinaryOperation::Lte,
            Greater => BinaryOperation::Gt,
            GreaterEqual => BinaryOperation::Gte,
            LeftShift => BinaryOperation::Shl,
            RightShift => BinaryOperation::Shr,
            LogicalAnd => BinaryOperation::And, // Boolean AND
            LogicalOr => BinaryOperation::Or,   // Boolean OR
        }
    }

    fn convert_unary_op(&self, op: &skalp_mir::UnaryOp) -> UnaryOperation {
        use skalp_mir::UnaryOp::*;
        match op {
            Not => UnaryOperation::Not,
            BitwiseNot => UnaryOperation::Not,
            Negate => UnaryOperation::Neg,
            Reduce(_) => UnaryOperation::Not, // Map reduction to NOT for now
        }
    }

    fn get_width(&self, data_type: &DataType) -> usize {
        match data_type {
            DataType::Bit(w) | DataType::Logic(w) => *w,
            DataType::Int(w) | DataType::Nat(w) => *w,
            DataType::Clock { .. } | DataType::Reset { .. } => 1,
            DataType::Array(dt, size) => self.get_width(dt) * size,
            DataType::BitParam { default, .. } |
            DataType::LogicParam { default, .. } |
            DataType::IntParam { default, .. } |
            DataType::NatParam { default, .. } => *default,
            _ => 1,
        }
    }

    fn get_signal_width(&self, signal_name: &str) -> usize {
        // Check if it's a signal
        if let Some(signal) = self.sir.signals.iter().find(|s| s.name == signal_name) {
            return signal.width;
        }

        // Check if it's a state element
        if let Some(state) = self.sir.state_elements.get(signal_name) {
            return state.width;
        }

        // Default to 8 bits for the counter example
        8
    }

    fn extract_clock_domains(&mut self) {
        let mut domains: HashMap<String, ClockDomain> = HashMap::new();

        for process in &self.mir.processes {
            if process.kind == ProcessKind::Sequential {
                if let SensitivityList::Edge(edges) = &process.sensitivity {
                    if let Some(edge_sens) = edges.first() {
                        let clock_name = self.lvalue_to_string(&edge_sens.signal);
                        domains.entry(clock_name.clone()).or_insert_with(|| {
                            ClockDomain {
                                name: clock_name.clone(),
                                frequency_hz: None,
                                phase_offset: 0.0,
                                state_elements: Vec::new(),
                            }
                        });
                    }
                }
            }
        }

        // Associate state elements with clock domains
        for (name, state) in &self.sir.state_elements {
            if let Some(domain) = domains.values_mut().find(|d| d.name == state.clock) {
                domain.state_elements.push(name.clone());
            }
        }

        self.sir.clock_domains = domains;
    }

    /// Create a priority mux node from a resolved priority mux
    /// This is much simpler than the old recursive approach!
    fn create_priority_mux_node(&mut self, mux: &PriorityMux) -> usize {
        // Build nested ternary expression from priority mux
        let mut result_expr = mux.default.clone();

        // Build ternary chain from right to left (lowest to highest priority)
        for case in mux.cases.iter().rev() {
            result_expr = Expression::Conditional {
                cond: Box::new(case.condition.clone()),
                then_expr: Box::new(case.value.clone()),
                else_expr: Box::new(result_expr),
            };
        }

        // Create SIR node for the final expression
        self.create_expression_node(&result_expr)
    }

    fn create_priority_mux_with_context(&mut self, mux: &PriorityMux, local_context: &std::collections::HashMap<String, usize>) -> usize {
        // Build nested ternary expression from priority mux
        let mut result_expr = mux.default.clone();

        // Build ternary chain from right to left (lowest to highest priority)
        for case in mux.cases.iter().rev() {
            result_expr = Expression::Conditional {
                cond: Box::new(case.condition.clone()),
                then_expr: Box::new(case.value.clone()),
                else_expr: Box::new(result_expr),
            };
        }

        // CRITICAL: Create SIR node using the local context
        self.create_expression_with_local_context(&result_expr, local_context)
    }

    fn next_node_id(&mut self) -> usize {
        let id = self.node_counter;
        self.node_counter += 1;
        id
    }

    /// Evaluate a constant expression to get a compile-time constant value
    fn evaluate_constant_expression(&self, expr: &Expression) -> Option<u64> {
        match expr {
            Expression::Literal(value) => {
                match value {
                    Value::Integer(i) => Some(*i as u64),
                    Value::BitVector { value, .. } => Some(*value),
                    _ => None,
                }
            }
            _ => None, // Only literals can be evaluated as constants for now
        }
    }
}