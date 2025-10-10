use crate::sir::*;
use skalp_mir::mir::PortDirection as MirPortDirection;
use skalp_mir::mir::PriorityMux;
use skalp_mir::{
    Block, DataType, EdgeType, Expression, IfStatement, LValue, Mir, Module, ProcessKind,
    SensitivityList, Statement, Value,
};
use std::collections::HashMap;

/// Convert MIR to SIR with full hierarchical elaboration
/// This function takes the entire Mir (all modules) and elaborates the first module
/// by recursively flattening all instantiated submodules
pub fn convert_mir_to_sir(mir_module: &Module) -> SirModule {
    // For backward compatibility, create a minimal Mir with just this module
    let mir = Mir {
        name: "design".to_string(),
        modules: vec![mir_module.clone()],
    };
    convert_mir_to_sir_with_hierarchy(&mir, mir_module)
}

/// Convert MIR to SIR with hierarchical elaboration
/// Takes the full Mir design (all modules) to resolve instances
pub fn convert_mir_to_sir_with_hierarchy(mir: &Mir, top_module: &Module) -> SirModule {
    println!(
        "🌟 MIR TO SIR: Starting hierarchical conversion for module '{}'",
        top_module.name
    );

    let mut sir = SirModule::new(top_module.name.clone());
    let mut converter = MirToSirConverter::new(&mut sir, top_module, mir);

    converter.convert_ports();
    converter.convert_signals();
    converter.convert_logic();

    // CRITICAL: Flatten hierarchical instances
    converter.flatten_instances("");

    converter.extract_clock_domains();

    println!("🌟 MIR TO SIR: Hierarchical conversion complete");
    println!(
        "📥 INPUTS: {:?}",
        sir.inputs.iter().map(|i| &i.name).collect::<Vec<_>>()
    );
    println!(
        "📤 OUTPUTS: {:?}",
        sir.outputs.iter().map(|o| &o.name).collect::<Vec<_>>()
    );
    println!("🔧 COMBINATIONAL NODES: {}", sir.combinational_nodes.len());
    for node in &sir.combinational_nodes {
        println!(
            "   Node {}: {:?}, inputs={:?}, outputs={:?}",
            node.id,
            node.kind,
            node.inputs.iter().map(|i| &i.signal_id).collect::<Vec<_>>(),
            node.outputs
                .iter()
                .map(|o| &o.signal_id)
                .collect::<Vec<_>>()
        );
    }
    println!("⚡ SEQUENTIAL NODES: {}", sir.sequential_nodes.len());
    for node in &sir.sequential_nodes {
        println!(
            "   Node {}: {:?}, inputs={:?}, outputs={:?}",
            node.id,
            node.kind,
            node.inputs.iter().map(|i| &i.signal_id).collect::<Vec<_>>(),
            node.outputs
                .iter()
                .map(|o| &o.signal_id)
                .collect::<Vec<_>>()
        );
    }
    sir
}

struct MirToSirConverter<'a> {
    sir: &'a mut SirModule,
    mir: &'a Module,     // Current module being converted
    mir_design: &'a Mir, // Full design with all modules for instance resolution
    node_counter: usize,
    #[allow(dead_code)]
    signal_map: HashMap<String, String>,
    #[allow(dead_code)]
    conditional_contexts: HashMap<usize, HashMap<String, usize>>,
}

impl<'a> MirToSirConverter<'a> {
    fn new(sir: &'a mut SirModule, mir: &'a Module, mir_design: &'a Mir) -> Self {
        MirToSirConverter {
            sir,
            mir,
            mir_design,
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

            // Check if this output port is assigned in sequential logic
            let is_sequential_port =
                matches!(direction, PortDirection::Output) && self.is_port_sequential(port.id);

            self.sir.signals.push(SirSignal {
                name: port.name.clone(),
                width,
                driver_node: None,
                fanout_nodes: Vec::new(),
                is_state: is_sequential_port,
            });

            // Create state element for sequential output ports
            if is_sequential_port {
                self.sir.state_elements.insert(
                    port.name.clone(),
                    StateElement {
                        name: port.name.clone(),
                        width,
                        reset_value: None,
                        clock: String::new(),
                        reset: None,
                    },
                );
            }
        }
    }

    fn convert_signals(&mut self) {
        for signal in &self.mir.signals {
            let width = self.get_width(&signal.signal_type);

            eprintln!(
                "📏 Signal '{}': type={:?}, width={}",
                signal.name, signal.signal_type, width
            );

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

    fn is_port_sequential(&self, port_id: skalp_mir::PortId) -> bool {
        // Check if this port is assigned in any sequential process
        for process in &self.mir.processes {
            if process.kind == ProcessKind::Sequential
                && self.is_port_assigned_in_block(&process.body, port_id)
            {
                return true;
            }
        }
        false
    }

    fn is_signal_sequential(&self, signal_id: skalp_mir::SignalId) -> bool {
        // Check if this signal is assigned in any sequential process
        for process in &self.mir.processes {
            if process.kind == ProcessKind::Sequential
                && self.is_signal_assigned_in_block(&process.body, signal_id)
            {
                return true;
            }
        }
        false
    }

    fn is_signal_assigned_in_block(
        &self,
        block: &skalp_mir::Block,
        signal_id: skalp_mir::SignalId,
    ) -> bool {
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
                Statement::Case(case_stmt) => {
                    // Check all case arms
                    for item in &case_stmt.items {
                        if self.is_signal_assigned_in_block(&item.block, signal_id) {
                            return true;
                        }
                    }
                    // Check default case
                    if let Some(default_block) = &case_stmt.default {
                        if self.is_signal_assigned_in_block(default_block, signal_id) {
                            return true;
                        }
                    }
                }
                _ => {}
            }
        }
        false
    }

    fn is_port_assigned_in_block(
        &self,
        block: &skalp_mir::Block,
        port_id: skalp_mir::PortId,
    ) -> bool {
        for stmt in &block.statements {
            match stmt {
                Statement::Assignment(assign) => {
                    if self.lvalue_contains_port(&assign.lhs, port_id) {
                        return true;
                    }
                }
                Statement::Block(inner_block) => {
                    if self.is_port_assigned_in_block(inner_block, port_id) {
                        return true;
                    }
                }
                Statement::If(if_stmt) => {
                    if self.is_port_assigned_in_block(&if_stmt.then_block, port_id) {
                        return true;
                    }
                    if let Some(else_block) = &if_stmt.else_block {
                        if self.is_port_assigned_in_block(else_block, port_id) {
                            return true;
                        }
                    }
                }
                Statement::ResolvedConditional(resolved) => {
                    if self.lvalue_contains_port(&resolved.target, port_id) {
                        return true;
                    }
                }
                Statement::Case(case_stmt) => {
                    // Check all case arms
                    for item in &case_stmt.items {
                        if self.is_port_assigned_in_block(&item.block, port_id) {
                            return true;
                        }
                    }
                    // Check default case
                    if let Some(default_block) = &case_stmt.default {
                        if self.is_port_assigned_in_block(default_block, port_id) {
                            return true;
                        }
                    }
                }
                _ => {}
            }
        }
        false
    }

    #[allow(clippy::only_used_in_recursion)]
    fn lvalue_contains_signal(&self, lvalue: &LValue, signal_id: skalp_mir::SignalId) -> bool {
        match lvalue {
            LValue::Signal(id) => *id == signal_id,
            LValue::BitSelect { base, .. } | LValue::RangeSelect { base, .. } => {
                self.lvalue_contains_signal(base, signal_id)
            }
            LValue::Concat(parts) => parts
                .iter()
                .any(|part| self.lvalue_contains_signal(part, signal_id)),
            _ => false,
        }
    }

    #[allow(clippy::only_used_in_recursion)]
    fn lvalue_contains_port(&self, lvalue: &LValue, port_id: skalp_mir::PortId) -> bool {
        match lvalue {
            LValue::Port(id) => *id == port_id,
            LValue::BitSelect { base, .. } | LValue::RangeSelect { base, .. } => {
                self.lvalue_contains_port(base, port_id)
            }
            LValue::Concat(parts) => parts
                .iter()
                .any(|part| self.lvalue_contains_port(part, port_id)),
            _ => false,
        }
    }

    fn convert_logic(&mut self) {
        println!(
            "🔧 CONVERTING LOGIC: Found {} processes and {} continuous assignments",
            self.mir.processes.len(),
            self.mir.assignments.len()
        );

        // CRITICAL FIX: Convert continuous assignments BEFORE processes
        // This ensures that when sequential processes reference combinational signals,
        // those signals already exist with their driver nodes
        eprintln!(
            "📡 CONTINUOUS ASSIGNMENTS: Processing {} assignments",
            self.mir.assignments.len()
        );
        for assign in &self.mir.assignments {
            let target = self.lvalue_to_string(&assign.lhs);
            eprintln!("   📡 CONTINUOUS: {} <= expression", target);
            self.convert_continuous_assign(&target, &assign.rhs);
        }

        // Convert processes
        for (i, process) in self.mir.processes.iter().enumerate() {
            println!(
                "   Process {}: kind={:?}, statements={}",
                i,
                process.kind,
                process.body.statements.len()
            );

            match &process.kind {
                ProcessKind::Combinational => {
                    println!(
                        "      ⚡ COMBINATIONAL: Processing {} statements",
                        process.body.statements.len()
                    );
                    self.convert_combinational_block(&process.body.statements);
                }
                ProcessKind::Sequential => {
                    println!(
                        "      🔄 SEQUENTIAL: Processing {} statements",
                        process.body.statements.len()
                    );
                    eprintln!("SEQUENTIAL PROCESS FOUND!"); // Force stderr output
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
                            eprintln!("CALLING convert_sequential_block"); // Force stderr output
                            self.convert_sequential_block(
                                &process.body.statements,
                                &signal_name,
                                edge,
                            );
                        }
                    }
                }
                _ => {
                    println!("      ❓ OTHER: Process kind not handled");
                }
            }
        }
    }

    fn convert_combinational_block(&mut self, statements: &[Statement]) {
        eprintln!(
            "🔧 COMBINATIONAL BLOCK: Processing {} statements",
            statements.len()
        );

        // CRITICAL FIX: Use shared context for dependency tracking in combinational blocks
        let mut local_context = std::collections::HashMap::new();

        for stmt in statements {
            match stmt {
                Statement::Assignment(assign) => {
                    let target = self.lvalue_to_string(&assign.lhs);
                    eprintln!("   📝 ASSIGNMENT: {} <= expression", target);

                    // Create expression with current local context
                    let node_id =
                        self.create_expression_with_local_context(&assign.rhs, &local_context);

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
                    eprintln!(
                        "   🔄 RESOLVED CONDITIONAL: {} <= priority mux (WITH CONTEXT)",
                        target
                    );

                    // CRITICAL FIX: Use shared context to create the priority mux
                    let mux_node =
                        self.create_priority_mux_with_context(&resolved.resolved, &local_context);

                    // Store in local context for future references
                    local_context.insert(target.clone(), mux_node);

                    self.connect_node_to_signal(mux_node, &target);
                }
                _ => {
                    eprintln!("   ❓ OTHER: Statement type not handled");
                }
            }
        }

        eprintln!(
            "🏁 COMBINATIONAL BLOCK: Processed {} statements with shared context",
            statements.len()
        );
    }

    fn convert_sequential_block(&mut self, statements: &[Statement], clock: &str, edge: ClockEdge) {
        println!(
            "🚀 SEQUENTIAL BLOCK: Starting conversion with {} statements, clock={}, edge={:?}",
            statements.len(),
            clock,
            edge
        );

        // Process ALL statements sequentially
        // This handles ResolvedConditional statements with proper dependency tracking
        for stmt in statements {
            match stmt {
                Statement::Assignment(assign) => {
                    // Check if this is an array write (BitSelect with non-constant index)
                    eprintln!(
                        "DEBUG: Assignment lhs type: {:?}",
                        std::mem::discriminant(&assign.lhs)
                    );
                    if let LValue::BitSelect { base, index } = &assign.lhs {
                        eprintln!("DEBUG: BitSelect found, checking if index is constant...");
                        let is_const = self.evaluate_constant_expression(index);
                        eprintln!("DEBUG: Index constant check: {:?}", is_const);
                        if is_const.is_none() {
                            // Dynamic array write: array[index] <= value
                            let array_name = self.lvalue_to_string(base);
                            println!(
                                "   📝 ARRAY ASSIGNMENT: {}[index] <= expression",
                                array_name
                            );

                            // Create nodes for the array write
                            // For arrays that are state elements, we need to read from the register
                            let old_array = self.create_lvalue_ref_node(base);
                            let index_node = self.create_expression_node(index);
                            let value_node = self.create_expression_node(&assign.rhs);

                            // Create ArrayWrite node
                            let array_write =
                                self.create_array_write_node(old_array, index_node, value_node);

                            // Create flip-flop to register the new array state
                            let ff_node =
                                self.create_flipflop_with_input(array_write, clock, edge.clone());
                            self.connect_node_to_signal(ff_node, &array_name);
                        } else {
                            // Static bit select - treat as normal assignment
                            let target = self.lvalue_to_string(&assign.lhs);
                            println!("   📝 ASSIGNMENT: {} <= expression", target);
                            let value = self.create_expression_node(&assign.rhs);
                            let ff_node =
                                self.create_flipflop_with_input(value, clock, edge.clone());
                            self.connect_node_to_signal(ff_node, &target);
                        }
                    } else {
                        // Normal assignment (not BitSelect)
                        let target = self.lvalue_to_string(&assign.lhs);
                        println!("   📝 ASSIGNMENT: {} <= expression", target);

                        // CRITICAL: For sequential blocks (non-blocking assignments), all RHS
                        // values are sampled at the beginning of the clock cycle.
                        // We should NOT use a local context that tracks intermediate values.
                        let value = self.create_expression_node(&assign.rhs);

                        // Create flip-flop
                        let ff_node = self.create_flipflop_with_input(value, clock, edge.clone());
                        self.connect_node_to_signal(ff_node, &target);
                    }
                }
                Statement::ResolvedConditional(resolved) => {
                    // Check if this is an array write (BitSelect with non-constant index)
                    eprintln!(
                        "DEBUG: ResolvedConditional target type: {:?}",
                        std::mem::discriminant(&resolved.target)
                    );
                    if let LValue::BitSelect { base, index } = &resolved.target {
                        if self.evaluate_constant_expression(index).is_none() {
                            // Dynamic array write with conditional: array[index] <= mux(conds, values)
                            let array_name = self.lvalue_to_string(base);
                            println!(
                                "   🔄 RESOLVED CONDITIONAL ARRAY: {}[index] <= priority mux",
                                array_name
                            );

                            // For array writes, we need to create ArrayWrite for each branch
                            // Then mux between them based on conditions
                            // For now, use a simplified approach: evaluate the mux value, then write to array
                            let old_array = self.create_lvalue_ref_node(base);
                            let index_node = self.create_expression_node(index);
                            let mux_value = self.create_priority_mux_node(&resolved.resolved);

                            // Create ArrayWrite node
                            let array_write =
                                self.create_array_write_node(old_array, index_node, mux_value);

                            // Create flip-flop to register the new array state
                            let ff_node =
                                self.create_flipflop_with_input(array_write, clock, edge.clone());
                            self.connect_node_to_signal(ff_node, &array_name);
                        } else {
                            // Static bit select
                            let target = self.lvalue_to_string(&resolved.target);
                            println!("   🔄 RESOLVED CONDITIONAL: {} <= priority mux", target);
                            let mux_value = self.create_priority_mux_node(&resolved.resolved);
                            let ff_node =
                                self.create_flipflop_with_input(mux_value, clock, edge.clone());
                            self.connect_node_to_signal(ff_node, &target);
                        }
                    } else {
                        // Normal resolved conditional (not array write)
                        let target = self.lvalue_to_string(&resolved.target);
                        println!("   🔄 RESOLVED CONDITIONAL: {} <= priority mux", target);

                        // CRITICAL: For sequential blocks, we should NOT use local_context
                        // because in sequential logic (non-blocking assignments), all RHS
                        // values are sampled at the beginning of the clock cycle.
                        let mux_value = self.create_priority_mux_node(&resolved.resolved);

                        // Create flip-flop
                        let ff_node =
                            self.create_flipflop_with_input(mux_value, clock, edge.clone());
                        self.connect_node_to_signal(ff_node, &target);
                    }
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
                    println!(
                        "   ❓ OTHER: Statement type not handled in sequential context: {:?}",
                        stmt
                    );
                }
            }
        }

        println!(
            "🏁 SEQUENTIAL BLOCK: Processed {} statements",
            statements.len()
        );
    }
    #[allow(dead_code)]
    fn collect_all_assignment_targets_from_block(
        &self,
        statements: &[Statement],
        targets: &mut std::collections::HashSet<String>,
    ) {
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

    #[allow(dead_code)]
    fn group_targets_by_conditionals(
        &mut self,
        statements: &[Statement],
        all_targets: &std::collections::HashSet<String>,
        conditional_groups: &mut std::collections::HashMap<
            usize,
            std::collections::HashSet<String>,
        >,
        simple_assignments: &mut Vec<(String, usize)>,
    ) {
        println!(
            "🎯 GROUPING: Processing {} statements, tracking targets: {:?}",
            statements.len(),
            all_targets
        );

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
                    let relevant_targets: std::collections::HashSet<String> =
                        if_targets.intersection(all_targets).cloned().collect();

                    println!(
                        "   🔄 IF STMT: Found {} targets in if: {:?}, relevant: {:?}",
                        if_targets.len(),
                        if_targets,
                        relevant_targets
                    );

                    if !relevant_targets.is_empty() {
                        let if_stmt_ptr = if_stmt as *const _ as usize;
                        conditional_groups.insert(if_stmt_ptr, relevant_targets);
                        println!(
                            "   ✅ GROUPED: Conditional group created with {} targets",
                            conditional_groups.get(&if_stmt_ptr).unwrap().len()
                        );
                    }
                }
                Statement::Block(block) => {
                    println!(
                        "   📦 BLOCK: Recursing into block with {} statements",
                        block.statements.len()
                    );
                    self.group_targets_by_conditionals(
                        &block.statements,
                        all_targets,
                        conditional_groups,
                        simple_assignments,
                    );
                }
                _ => {
                    println!("   ❓ OTHER: Statement type not handled");
                }
            }
        }

        println!(
            "🏁 GROUPING: Final groups: {}, simple: {}",
            conditional_groups.len(),
            simple_assignments.len()
        );
    }

    #[allow(dead_code)]
    fn collect_targets_from_if(
        &self,
        if_stmt: &IfStatement,
        targets: &mut std::collections::HashSet<String>,
    ) {
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

    #[allow(dead_code)]
    fn process_conditional_group_with_shared_context(
        &mut self,
        if_stmt: &IfStatement,
        targets: &std::collections::HashSet<String>,
    ) -> Vec<(String, usize)> {
        let mut results = Vec::new();

        // CRITICAL FIX: Process all assignments in each branch with SHARED context
        // Then create muxes using the SAME computed values

        // Process then branch with shared context - compute ALL assignments together
        let mut then_context = std::collections::HashMap::new();
        let then_values = self.process_branch_for_all_targets(
            &if_stmt.then_block.statements,
            targets,
            &mut then_context,
        );

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
            let then_value = then_values
                .get(target)
                .copied()
                .unwrap_or_else(|| self.create_signal_ref(target));
            let else_value = else_values
                .get(target)
                .copied()
                .unwrap_or_else(|| self.create_signal_ref(target));

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

    fn process_branch_for_all_targets(
        &mut self,
        statements: &[Statement],
        targets: &std::collections::HashSet<String>,
        local_context: &mut std::collections::HashMap<String, usize>,
    ) -> std::collections::HashMap<String, usize> {
        let mut target_values = std::collections::HashMap::new();

        println!(
            "🔄 SHARED CONTEXT: Processing {} statements, tracking {} targets",
            statements.len(),
            targets.len()
        );

        // Process all assignments in order, building up context
        for stmt in statements {
            if let Statement::Assignment(assign) = stmt {
                let assign_target = self.lvalue_to_string(&assign.lhs);

                // Create expression with current local context
                let value = self.create_expression_with_local_context(&assign.rhs, local_context);

                println!(
                    "   📝 ASSIGN: {} <= expression -> node_{}",
                    assign_target, value
                );
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
                    println!(
                        "      📚 Context after {}: {:?}",
                        assign_target,
                        local_context.keys().collect::<Vec<_>>()
                    );
                }
            }
        }

        println!(
            "🏁 SHARED CONTEXT: Returning {} target values",
            target_values.len()
        );
        target_values
    }

    #[allow(dead_code)]
    fn synthesize_sequential_assignment_for_target(
        &mut self,
        statements: &[Statement],
        target: &str,
    ) -> usize {
        // Build the logic that determines what value gets assigned to this target
        // This needs to respect the order of statements and conditional priorities
        self.synthesize_assignment_value_from_block(statements, target, None)
    }

    fn synthesize_assignment_value_from_block(
        &mut self,
        statements: &[Statement],
        target: &str,
        default_value: Option<usize>,
    ) -> usize {
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
                    result_value = Some(self.synthesize_assignment_value_from_block(
                        &block.statements,
                        target,
                        result_value,
                    ));
                }
                Statement::If(if_stmt) => {
                    // If this if-statement assigns to our target, create conditional logic
                    if self.if_assigns_to_target(if_stmt, target) {
                        // Always use the new dependency-aware approach
                        result_value =
                            Some(self.synthesize_conditional_assignment(if_stmt, target));
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
        self.block_assigns_to_target(&if_stmt.then_block.statements, target)
            || if let Some(else_block) = &if_stmt.else_block {
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

    #[allow(dead_code)]
    fn if_has_inter_signal_dependencies(&self, if_stmt: &IfStatement) -> bool {
        // Check if any assignment in the if-statement references other signals assigned in the same block
        let mut assigned_signals = std::collections::HashSet::new();
        self.collect_assigned_signals_from_if(if_stmt, &mut assigned_signals);

        // Check if any assignment references another assigned signal
        self.check_block_for_inter_dependencies(&if_stmt.then_block.statements, &assigned_signals)
            || if let Some(else_block) = &if_stmt.else_block {
                self.check_block_for_inter_dependencies(&else_block.statements, &assigned_signals)
            } else {
                false
            }
    }

    fn collect_assigned_signals_from_if(
        &self,
        if_stmt: &IfStatement,
        signals: &mut std::collections::HashSet<String>,
    ) {
        self.collect_assigned_signals_from_block(&if_stmt.then_block.statements, signals);
        if let Some(else_block) = &if_stmt.else_block {
            self.collect_assigned_signals_from_block(&else_block.statements, signals);
        }
    }

    fn collect_assigned_signals_from_block(
        &self,
        statements: &[Statement],
        signals: &mut std::collections::HashSet<String>,
    ) {
        for stmt in statements {
            if let Statement::Assignment(assign) = stmt {
                let target = self.lvalue_to_string(&assign.lhs);
                signals.insert(target);
            }
        }
    }

    fn check_block_for_inter_dependencies(
        &self,
        statements: &[Statement],
        assigned_signals: &std::collections::HashSet<String>,
    ) -> bool {
        for stmt in statements {
            if let Statement::Assignment(assign) = stmt {
                if self.expression_references_signals(&assign.rhs, assigned_signals) {
                    return true;
                }
            }
        }
        false
    }

    fn expression_references_signals(
        &self,
        expr: &Expression,
        signals: &std::collections::HashSet<String>,
    ) -> bool {
        match expr {
            Expression::Ref(lvalue) => {
                let signal_name = self.lvalue_to_string(lvalue);
                // Also check the base signal for range selects and bit selects
                let base_signal = self.get_base_signal_name(lvalue);
                signals.contains(&signal_name) || signals.contains(&base_signal)
            }
            Expression::Binary { left, right, .. } => {
                self.expression_references_signals(left, signals)
                    || self.expression_references_signals(right, signals)
            }
            Expression::Unary { operand, .. } => {
                self.expression_references_signals(operand, signals)
            }
            Expression::Conditional {
                cond,
                then_expr,
                else_expr,
            } => {
                self.expression_references_signals(cond, signals)
                    || self.expression_references_signals(then_expr, signals)
                    || self.expression_references_signals(else_expr, signals)
            }
            Expression::Concat(exprs) => exprs
                .iter()
                .any(|e| self.expression_references_signals(e, signals)),
            Expression::Replicate { count, value } => {
                self.expression_references_signals(count, signals)
                    || self.expression_references_signals(value, signals)
            }
            Expression::FunctionCall { args, .. } => args
                .iter()
                .any(|e| self.expression_references_signals(e, signals)),
            _ => false,
        }
    }

    fn get_base_signal_name(&self, lvalue: &LValue) -> String {
        match lvalue {
            LValue::Signal(id) => self
                .mir
                .signals
                .iter()
                .find(|s| s.id == *id)
                .map(|s| s.name.clone())
                .unwrap_or_else(|| format!("signal_{}", id.0)),
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

    #[allow(dead_code)]
    fn synthesize_conditional_assignment_with_shared_context(
        &mut self,
        if_stmt: &IfStatement,
        target: &str,
    ) -> usize {
        // Process all assignments in the conditional with shared context
        // First collect all assigned signals
        let mut assigned_signals = std::collections::HashSet::new();
        self.collect_assigned_signals_from_if(if_stmt, &mut assigned_signals);

        // Build shared context for all signals
        let context_key = if_stmt as *const _ as usize;
        #[allow(clippy::map_entry)]
        if !self.conditional_contexts.contains_key(&context_key) {
            let context = self.build_shared_conditional_context(if_stmt, &assigned_signals);
            self.conditional_contexts.insert(context_key, context);
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

    fn build_shared_conditional_context(
        &mut self,
        if_stmt: &IfStatement,
        assigned_signals: &std::collections::HashSet<String>,
    ) -> std::collections::HashMap<String, usize> {
        let mut result = std::collections::HashMap::new();

        // Process each assigned signal with shared context
        for signal in assigned_signals {
            let mut cases = Vec::new();
            self.collect_conditional_cases_with_context(if_stmt, signal, &mut cases);
            let value = if cases.is_empty() {
                self.create_signal_ref(signal)
            } else {
                self.build_priority_mux(&cases, signal)
            };
            result.insert(signal.clone(), value);
        }

        result
    }

    fn convert_if_in_sequential(&mut self, if_stmt: &IfStatement, clock: &str, edge: ClockEdge) {
        // First, collect array writes to exclude them from general processing
        let mut array_writes = Vec::new();
        println!("   🔍 SCANNING FOR ARRAY WRITES IN IF");
        self.collect_array_writes(if_stmt, &mut array_writes);

        // Create a set of array names that have been processed
        let array_names: std::collections::HashSet<String> =
            array_writes.iter().map(|(_, name)| name.clone()).collect();
        println!(
            "   📊 FOUND {} ARRAY WRITES: {:?}",
            array_writes.len(),
            array_names
        );

        // Handle array writes separately since they need special processing
        self.handle_array_writes_in_if_with_list(if_stmt, clock, edge.clone(), array_writes);

        // Synthesis-like approach: collect all conditions and values, then build priority mux
        let mut targets = std::collections::HashSet::new();
        self.collect_assignment_targets(if_stmt, &mut targets);

        println!("   📋 COLLECTED TARGETS (before filtering): {:?}", targets);

        // CRITICAL FIX: Exclude array write targets since they're already handled
        targets.retain(|target| !array_names.contains(target));

        println!("   📋 COLLECTED TARGETS (after filtering): {:?}", targets);
        println!(
            "   📋 AVAILABLE MIR SIGNALS: {:?}",
            self.mir
                .signals
                .iter()
                .map(|s| format!("{}(id={})", s.name, s.id.0))
                .collect::<Vec<_>>()
        );
        println!(
            "   📋 STATE ELEMENTS: {:?}",
            self.sir.state_elements.keys().collect::<Vec<_>>()
        );

        // For each target signal, synthesize a priority-encoded mux
        // Sort targets for deterministic ordering
        let mut sorted_targets: Vec<_> = targets.into_iter().collect();
        sorted_targets.sort();
        for target in sorted_targets {
            let final_value = self.synthesize_conditional_assignment(if_stmt, &target);
            let ff_node = self.create_flipflop_with_input(final_value, clock, edge.clone());
            self.connect_node_to_signal(ff_node, &target);
        }
    }

    fn handle_array_writes_in_if_with_list(
        &mut self,
        if_stmt: &IfStatement,
        clock: &str,
        edge: ClockEdge,
        array_writes: Vec<(LValue, String)>,
    ) {
        // Process each unique array write
        for (array_base, array_name) in array_writes {
            println!(
                "   🔄 IF ARRAY WRITE: {}[index] with conditional assignment",
                array_name
            );

            // Synthesize the conditional array write:
            // 1. Get the old array value
            // 2. Build a mux tree for the write conditions
            // 3. Create ArrayWrite node
            // 4. Create flip-flop
            let old_array = self.create_lvalue_ref_node(&array_base);
            let (index_node, value_node) = self.synthesize_array_write_in_if(if_stmt, &array_base);

            if index_node != 0 && value_node != 0 {
                let array_write = self.create_array_write_node(old_array, index_node, value_node);
                let ff_node = self.create_flipflop_with_input(array_write, clock, edge.clone());
                self.connect_node_to_signal(ff_node, &array_name);
            }
        }
    }

    fn collect_array_writes(
        &self,
        if_stmt: &IfStatement,
        array_writes: &mut Vec<(LValue, String)>,
    ) {
        // Check then branch
        println!(
            "      🔎 SCANNING THEN BRANCH: {} statements",
            if_stmt.then_block.statements.len()
        );
        self.scan_block_for_array_writes(&if_stmt.then_block.statements, array_writes);

        // Check else branch
        if let Some(else_block) = &if_stmt.else_block {
            println!(
                "      🔎 SCANNING ELSE BRANCH: {} statements",
                else_block.statements.len()
            );
            self.scan_block_for_array_writes(&else_block.statements, array_writes);
        }
    }

    fn scan_block_for_array_writes(
        &self,
        statements: &[Statement],
        array_writes: &mut Vec<(LValue, String)>,
    ) {
        for (i, stmt) in statements.iter().enumerate() {
            match stmt {
                Statement::Assignment(assign) => {
                    let lval_name = self.lvalue_to_string(&assign.lhs);
                    println!(
                        "         Statement[{}]: Assignment to {:?} (name={}), RHS: {:?}",
                        i,
                        std::mem::discriminant(&assign.lhs),
                        lval_name,
                        std::mem::discriminant(&assign.rhs)
                    );

                    // Check if RHS contains array writes even if LHS is just a Signal
                    if lval_name == "memory" {
                        println!("            📝 MEMORY ASSIGNMENT FOUND - checking if this is an array write");
                    }

                    if let LValue::BitSelect { base, index } = &assign.lhs {
                        let is_const = self.evaluate_constant_expression(index);
                        let base_name = self.lvalue_to_string(base);
                        println!(
                            "            BitSelect[{}] - is_const: {:?}",
                            base_name, is_const
                        );
                        if is_const.is_none() {
                            // Dynamic array write
                            println!("            ✅ FOUND DYNAMIC ARRAY WRITE: {}", base_name);
                            // Only add if not already present
                            if !array_writes.iter().any(|(_, name)| name == &base_name) {
                                array_writes.push(((**base).clone(), base_name));
                            }
                        }
                    }
                }
                Statement::If(nested_if) => {
                    println!("         Statement[{}]: Nested If, recursing...", i);
                    self.collect_array_writes(nested_if, array_writes);
                }
                Statement::Block(block) => {
                    println!(
                        "         Statement[{}]: Block with {} statements, recursing...",
                        i,
                        block.statements.len()
                    );
                    self.scan_block_for_array_writes(&block.statements, array_writes);
                }
                Statement::ResolvedConditional(resolved) => {
                    println!(
                        "         Statement[{}]: ResolvedConditional to {:?}",
                        i,
                        std::mem::discriminant(&resolved.target)
                    );
                    if let LValue::BitSelect { base, index } = &resolved.target {
                        let is_const = self.evaluate_constant_expression(index);
                        let base_name = self.lvalue_to_string(base);
                        println!(
                            "            BitSelect[{}] - is_const: {:?}",
                            base_name, is_const
                        );
                        if is_const.is_none() {
                            println!(
                                "            ✅ FOUND DYNAMIC ARRAY WRITE (resolved): {}",
                                base_name
                            );
                            if !array_writes.iter().any(|(_, name)| name == &base_name) {
                                array_writes.push(((**base).clone(), base_name));
                            }
                        }
                    }
                }
                _ => {
                    println!(
                        "         Statement[{}]: Other type: {:?}",
                        i,
                        std::mem::discriminant(stmt)
                    );
                }
            }
        }
    }

    fn synthesize_array_write_in_if(
        &mut self,
        if_stmt: &IfStatement,
        array_base: &LValue,
    ) -> (usize, usize) {
        // Find array write in then branch (recursively search nested ifs)
        let mut then_index = 0;
        let mut then_value = 0;
        for stmt in &if_stmt.then_block.statements {
            match stmt {
                Statement::Assignment(assign) => {
                    if let LValue::BitSelect { base, index } = &assign.lhs {
                        if self.lvalues_match(base, array_base) {
                            then_index = self.create_expression_node(index);
                            then_value = self.create_expression_node(&assign.rhs);
                            break;
                        }
                    }
                }
                Statement::If(nested_if) => {
                    // Recursively search nested if statements
                    let (nested_index, nested_value) =
                        self.synthesize_array_write_in_if(nested_if, array_base);
                    if nested_index != 0 && nested_value != 0 {
                        then_index = nested_index;
                        then_value = nested_value;
                        break;
                    }
                }
                _ => {}
            }
        }

        // Find array write in else branch (if any, recursively search nested ifs)
        let mut else_index = 0;
        let mut else_value = 0;
        if let Some(else_block) = &if_stmt.else_block {
            for stmt in &else_block.statements {
                match stmt {
                    Statement::Assignment(assign) => {
                        if let LValue::BitSelect { base, index } = &assign.lhs {
                            if self.lvalues_match(base, array_base) {
                                else_index = self.create_expression_node(index);
                                else_value = self.create_expression_node(&assign.rhs);
                                break;
                            }
                        }
                    }
                    Statement::If(nested_if) => {
                        // Recursively search nested if statements
                        let (nested_index, nested_value) =
                            self.synthesize_array_write_in_if(nested_if, array_base);
                        if nested_index != 0 && nested_value != 0 {
                            else_index = nested_index;
                            else_value = nested_value;
                            break;
                        }
                    }
                    _ => {}
                }
            }
        }

        // Build conditional mux for both index and value
        let condition = self.create_expression_node(&if_stmt.condition);

        // If only then branch has array write, use then values when condition is true
        // For now, we'll assume writes happen in both branches or we need to preserve old value
        // TODO: Handle case where write only happens in one branch (need to preserve array)
        if then_index != 0 && else_index == 0 {
            // Only then branch writes - for now just use then values
            // Proper implementation would require "no-write" muxing
            (then_index, then_value)
        } else if then_index == 0 && else_index != 0 {
            // Only else branch writes
            (else_index, else_value)
        } else if then_index != 0 && else_index != 0 {
            // Both branches write - mux the index and value
            let final_index = self.create_mux_node(condition, then_index, else_index);
            let final_value = self.create_mux_node(condition, then_value, else_value);
            (final_index, final_value)
        } else {
            // No writes found
            (0, 0)
        }
    }

    fn lvalues_match(&self, lv1: &LValue, lv2: &LValue) -> bool {
        self.lvalue_to_string(lv1) == self.lvalue_to_string(lv2)
    }

    #[allow(dead_code)]
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

    #[allow(dead_code)]
    fn convert_if_to_sequential_mux(
        &mut self,
        if_stmt: &IfStatement,
        target: &str,
        default: usize,
    ) -> usize {
        let cond_node = self.create_expression_node(&if_stmt.condition);

        // Find the assignment in the then branch
        let then_value = self
            .find_assignment_in_block(&if_stmt.then_block, target)
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

    #[allow(dead_code)]
    fn find_assignment_in_block(&mut self, block: &Block, target: &str) -> Option<usize> {
        for stmt in &block.statements {
            if let Statement::Assignment(assign) = stmt {
                let assign_target = self.lvalue_to_string(&assign.lhs);
                if assign_target == target {
                    return Some(self.create_expression_node(&assign.rhs));
                }
            }
        }
        None
    }

    fn collect_assignment_targets(
        &self,
        if_stmt: &IfStatement,
        targets: &mut std::collections::HashSet<String>,
    ) {
        // Collect targets from then branch
        for stmt in &if_stmt.then_block.statements {
            if let Statement::Assignment(assign) = stmt {
                // Skip array writes (handled separately)
                if let LValue::BitSelect { index, .. } = &assign.lhs {
                    if self.evaluate_constant_expression(index).is_none() {
                        continue; // Dynamic array write, skip
                    }
                }

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
                        // Skip array writes (handled separately)
                        if let LValue::BitSelect { index, .. } = &assign.lhs {
                            if self.evaluate_constant_expression(index).is_none() {
                                continue; // Dynamic array write, skip
                            }
                        }

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
        println!(
            "      🔨 SYNTHESIZE_CONDITIONAL_ASSIGNMENT: target={}",
            target
        );
        let mut cases = Vec::new();

        // Process then branch with local context
        let then_value =
            self.process_branch_with_dependencies(&if_stmt.then_block.statements, target);
        println!("         DEBUG: then_value = {:?}", then_value);
        let else_value = if let Some(else_block) = &if_stmt.else_block {
            let result = self.process_branch_with_dependencies(&else_block.statements, target);
            println!("         DEBUG: else_value = {:?}", result);
            result
        } else {
            println!("         DEBUG: no else block");
            None
        };

        // Build cases based on what we found
        match (then_value, else_value) {
            (Some(then_val), Some(else_val)) => {
                // Both branches assign: create proper mux
                let condition = self.create_expression_node(&if_stmt.condition);
                println!(
                    "         ✅ THEN: node_{}, ELSE: node_{}, cond: node_{}",
                    then_val, else_val, condition
                );
                cases.push((condition, then_val));
                cases.push((0, else_val));
            }
            (Some(then_val), None) => {
                // Only then assigns: mux(cond, then_val, keep_current)
                let condition = self.create_expression_node(&if_stmt.condition);
                println!(
                    "         ✅ THEN: node_{}, ELSE: keep_{}, cond: node_{}",
                    then_val, target, condition
                );
                let keep_val = self.create_signal_ref(target);
                cases.push((condition, then_val));
                cases.push((0, keep_val));
            }
            (None, Some(else_val)) => {
                // Only else assigns: mux(cond, keep_current, else_val)
                let condition = self.create_expression_node(&if_stmt.condition);
                println!(
                    "         ✅ THEN: keep_{}, ELSE: node_{}, cond: node_{}",
                    target, else_val, condition
                );
                let keep_val = self.create_signal_ref(target);
                cases.push((condition, keep_val));
                cases.push((0, else_val));
            }
            (None, None) => {
                // Neither assigns: keep current value
                println!("         ❌ No assignment to {} in either branch", target);
            }
        }

        // Build priority-encoded mux tree
        println!("         📊 Total cases: {}", cases.len());
        let result = if cases.is_empty() {
            // No assignments to this target, use current value
            println!("         ⚠️ No cases - using signal ref");
            self.create_signal_ref(target)
        } else {
            self.build_priority_mux(&cases, target)
        };
        println!("         ➡️ RESULT: node_{}", result);
        result
    }

    fn process_branch_with_dependencies(
        &mut self,
        statements: &[Statement],
        target: &str,
    ) -> Option<usize> {
        // CRITICAL: In sequential blocks (non-blocking assignments), all RHS values
        // are sampled at the beginning of the clock cycle. We should NOT track
        // intermediate computed values in a local context.

        // Process ALL statements and collect results from nested ifs.
        // For multiple independent ifs, we need to combine them properly:
        // - Each nested if may or may not assign to our target
        // - We need to merge all nested ifs that DO assign to target into a priority chain
        // - Later statements override earlier ones (sequential semantics)

        let mut nested_if_results = Vec::new();

        for stmt in statements {
            match stmt {
                Statement::Assignment(assign) => {
                    let assign_target = self.lvalue_to_string(&assign.lhs);

                    // If this is a direct assignment to our target, it overrides everything
                    if assign_target == target {
                        let value = self.create_expression_node(&assign.rhs);
                        // Clear previous nested ifs and return this direct assignment
                        return Some(value);
                    }
                }
                Statement::If(nested_if) => {
                    // Handle nested if/else-if chains
                    println!(
                        "         🔁 NESTED IF found in branch for target={}",
                        target
                    );
                    let nested_result = self.synthesize_conditional_assignment(nested_if, target);
                    nested_if_results.push(nested_result);
                }
                _ => {}
            }
        }

        // Now we have all nested if results. We need to combine them.
        // Each nested_if_result is already a full mux tree for that if statement.
        // However, if a nested if doesn't assign to our target, it returns a SignalRef
        // that just keeps the current value. We need to skip those and find the one
        // that actually assigns.
        //
        // Filter out "keep value" signal refs by checking if the node is a SignalRef to target.
        // Then use the last remaining result (sequential override semantics).
        let meaningful_results: Vec<usize> = nested_if_results.into_iter()
            .filter(|&node_id| {
                // Check if this node is just a SignalRef to our target
                let is_keep_value = self.sir.combinational_nodes.iter().any(|n| {
                    n.id == node_id && matches!(n.kind, SirNodeKind::SignalRef { ref signal } if signal == target)
                });
                !is_keep_value
            })
            .collect();

        if meaningful_results.is_empty() {
            None
        } else {
            // Use the last meaningful result (latest if statement that assigns)
            Some(*meaningful_results.last().unwrap())
        }
    }

    fn create_expression_with_local_context(
        &mut self,
        expr: &Expression,
        local_context: &std::collections::HashMap<String, usize>,
    ) -> usize {
        match expr {
            Expression::Literal(value) => self.create_literal_node(value),
            Expression::Ref(lvalue) => {
                let signal_name = self.lvalue_to_string(lvalue);

                // Check if this signal has a computed value in the local context
                if let Some(&computed_value) = local_context.get(&signal_name) {
                    println!(
                        "      🔍 CONTEXT HIT: {} -> node_{}",
                        signal_name, computed_value
                    );
                    computed_value
                } else {
                    // Check for range selects of locally computed values
                    let base_signal = self.get_base_signal_name(lvalue);
                    if let Some(&base_value) = local_context.get(&base_signal) {
                        println!(
                            "      🔍 RANGE SELECT: {} on node_{}",
                            signal_name, base_value
                        );
                        // Create range select on the computed value
                        self.create_range_select_on_node(lvalue, base_value)
                    } else {
                        println!(
                            "      🔍 CONTEXT MISS: {} -> fallback to register",
                            signal_name
                        );
                        // Fall back to register reference
                        self.create_lvalue_ref_node(lvalue)
                    }
                }
            }
            Expression::Binary { op, left, right } => {
                let left_node = self.create_expression_with_local_context(left, local_context);
                let right_node = self.create_expression_with_local_context(right, local_context);
                self.create_binary_op_node(op, left_node, right_node)
            }
            Expression::Unary { op, operand } => {
                let operand_node =
                    self.create_expression_with_local_context(operand, local_context);
                self.create_unary_op_node(op, operand_node)
            }
            Expression::Conditional {
                cond,
                then_expr,
                else_expr,
            } => {
                let cond_node = self.create_expression_with_local_context(cond, local_context);
                let then_node = self.create_expression_with_local_context(then_expr, local_context);
                let else_node = self.create_expression_with_local_context(else_expr, local_context);
                self.create_mux_node(cond_node, then_node, else_node)
            }
            Expression::Concat(exprs) => {
                let part_nodes: Vec<usize> = exprs
                    .iter()
                    .map(|e| self.create_expression_with_local_context(e, local_context))
                    .collect();
                self.create_concat_node(part_nodes)
            }
            Expression::Replicate { count, value } => {
                let _count_node = self.create_expression_with_local_context(count, local_context);
                // For now, just return the value (replication logic would be more complex)
                self.create_expression_with_local_context(value, local_context)
            }
            Expression::FunctionCall { .. } => {
                // Fall back to original implementation for function calls
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
            Expression::Literal(value) => match value {
                skalp_mir::Value::Integer(i) => *i as usize,
                skalp_mir::Value::BitVector { value, .. } => *value as usize,
                _ => 0,
            },
            _ => 0, // For non-constant expressions, return 0 as fallback
        }
    }

    fn collect_conditional_cases_with_context(
        &mut self,
        if_stmt: &IfStatement,
        target: &str,
        cases: &mut Vec<(usize, usize)>,
    ) {
        // Process then branch with context tracking
        let mut then_context = std::collections::HashMap::new();
        let then_value = self.process_conditional_branch_with_context(
            &if_stmt.then_block.statements,
            target,
            &mut then_context,
        );
        if let Some(value) = then_value {
            let condition = self.create_expression_node(&if_stmt.condition);
            cases.push((condition, value));
        }

        // Process else branch with context tracking
        if let Some(else_block) = &if_stmt.else_block {
            let mut else_context = std::collections::HashMap::new();
            let else_value = self.process_conditional_branch_with_context(
                &else_block.statements,
                target,
                &mut else_context,
            );
            if let Some(value) = else_value {
                cases.push((0, value)); // condition=0 means "default case"
            }
        }
    }

    fn process_conditional_branch_with_context(
        &mut self,
        statements: &[Statement],
        target: &str,
        context: &mut std::collections::HashMap<String, usize>,
    ) -> Option<usize> {
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

    fn create_expression_node_with_context(
        &mut self,
        expr: &Expression,
        context: &std::collections::HashMap<String, usize>,
    ) -> usize {
        match expr {
            Expression::Literal(value) => self.create_literal_node(value),
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
            Expression::Conditional {
                cond,
                then_expr,
                else_expr,
            } => {
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

    #[allow(dead_code)]
    fn collect_conditional_cases(
        &mut self,
        if_stmt: &IfStatement,
        target: &str,
        cases: &mut Vec<(usize, usize)>,
    ) {
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

    fn build_priority_mux(&mut self, cases: &[(usize, usize)], target: &str) -> usize {
        if cases.len() == 1 {
            let (cond, val) = cases[0];
            if cond == 0 {
                // Single default case - just return the value
                return val;
            } else {
                // Single conditional case - need to create mux with signal's current value as default
                // When no else case exists, the signal retains its value
                println!("         🔧 BUILD_PRIORITY_MUX: Single case, creating mux(cond={}, then={}, else=keep_{})", cond, val, target);
                let keep_value = self.create_signal_ref(target);
                return self.create_mux_node(cond, val, keep_value);
            }
        }

        // Build nested mux: cond1 ? val1 : (rest)
        let (first_cond, first_val) = cases[0];
        let rest_cases = &cases[1..];

        let else_mux = if rest_cases.is_empty() {
            // No more cases - this shouldn't happen with proper case collection
            self.create_constant_node(0, 8) // Default fallback
        } else {
            self.build_priority_mux(rest_cases, target)
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
            kind: SirNodeKind::SignalRef {
                signal: signal_name.to_string(),
            },
            inputs: vec![SignalRef {
                signal_id: signal_name.to_string(),
                bit_range: None,
            }],
            outputs: vec![SignalRef {
                signal_id: signal_id.clone(),
                bit_range: None,
            }],
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
            LValue::Port(port_id) => self
                .mir
                .ports
                .iter()
                .find(|p| p.id == *port_id)
                .map(|p| p.name.clone())
                .unwrap_or_else(|| format!("port_{}", port_id.0)),
            LValue::Signal(sig_id) => self
                .mir
                .signals
                .iter()
                .find(|s| s.id == *sig_id)
                .map(|s| s.name.clone())
                .unwrap_or_else(|| format!("signal_{}", sig_id.0)),
            LValue::Variable(var_id) => self
                .mir
                .variables
                .iter()
                .find(|v| v.id == *var_id)
                .map(|v| v.name.clone())
                .unwrap_or_else(|| format!("var_{}", var_id.0)),
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
            Expression::Literal(value) => self.create_literal_node(value),
            Expression::Ref(lvalue) => self.create_lvalue_ref_node(lvalue),
            Expression::Binary { op, left, right } => {
                let left_node = self.create_expression_node(left);
                let right_node = self.create_expression_node(right);
                self.create_binary_op_node(op, left_node, right_node)
            }
            Expression::Unary { op, operand } => {
                let operand_node = self.create_expression_node(operand);
                self.create_unary_op_node(op, operand_node)
            }
            Expression::Conditional {
                cond,
                then_expr,
                else_expr,
            } => {
                eprintln!("⚠️ CONDITIONAL EXPRESSION WITHOUT CONTEXT!");
                let cond_node = self.create_expression_node(cond);
                let then_node = self.create_expression_node(then_expr);
                let else_node = self.create_expression_node(else_expr);
                self.create_mux_node(cond_node, then_node, else_node)
            }
            Expression::Concat(parts) => {
                let part_nodes: Vec<usize> = parts
                    .iter()
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
                let signal_name = self
                    .mir
                    .signals
                    .iter()
                    .find(|s| s.id == *sig_id)
                    .map(|s| s.name.clone())
                    .unwrap_or_else(|| format!("signal_{}", sig_id.0));
                self.get_or_create_signal_driver(&signal_name)
            }
            LValue::Port(port_id) => {
                let port = self
                    .mir
                    .ports
                    .iter()
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
                let var_name = self
                    .mir
                    .variables
                    .iter()
                    .find(|v| v.id == *var_id)
                    .map(|v| v.name.clone())
                    .unwrap_or_else(|| format!("var_{}", var_id.0));
                self.get_or_create_signal_driver(&var_name)
            }
            LValue::BitSelect { base, index } => {
                // Try to evaluate index as constant first
                if let Some(index_val) = self.evaluate_constant_expression(index) {
                    // Static bit select - create a slice node
                    let base_node = self.create_lvalue_ref_node(base);
                    self.create_slice_node(base_node, index_val as usize, index_val as usize)
                } else {
                    // Dynamic array indexing - create ArrayRead node
                    let base_node = self.create_lvalue_ref_node(base);
                    let index_node = self.create_expression_node(index);
                    self.create_array_read_node(base_node, index_node)
                }
            }
            LValue::RangeSelect { base, high, low } => {
                let base_node = self.create_lvalue_ref_node(base);

                // Evaluate high and low expressions to get constant values
                let high_val = self.evaluate_constant_expression(high).unwrap_or(0) as usize;
                let low_val = self.evaluate_constant_expression(low).unwrap_or(0) as usize;

                self.create_slice_node(base_node, high_val, low_val)
            }
            LValue::Concat(parts) => {
                let part_nodes: Vec<usize> = parts
                    .iter()
                    .map(|p| self.create_lvalue_ref_node(p))
                    .collect();
                self.create_concat_node(part_nodes)
            }
        }
    }

    fn create_binary_op_node(
        &mut self,
        op: &skalp_mir::BinaryOp,
        left: usize,
        right: usize,
    ) -> usize {
        let node_id = self.next_node_id();
        let bin_op = self.convert_binary_op(op);

        let left_signal = self.node_to_signal_ref(left);
        let right_signal = self.node_to_signal_ref(right);

        // Determine width from input signals
        let width = self
            .get_signal_width(&left_signal.signal_id)
            .max(self.get_signal_width(&right_signal.signal_id));

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
        let width = self
            .get_signal_width(&true_signal.signal_id)
            .max(self.get_signal_width(&false_signal.signal_id));

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

        let part_signals: Vec<SignalRef> =
            parts.iter().map(|&p| self.node_to_signal_ref(p)).collect();

        // Calculate total width as sum of input widths
        let width = part_signals
            .iter()
            .map(|s| self.get_signal_width(&s.signal_id))
            .sum();

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
        eprintln!(
            "✂️ SLICE: Creating slice node from base={} with [{}:{}]",
            base, start, end
        );
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

    fn create_array_read_node(&mut self, array: usize, index: usize) -> usize {
        eprintln!(
            "📚 ARRAY READ: Creating array read node from array={}, index={}",
            array, index
        );
        let node_id = self.next_node_id();

        let array_signal = self.node_to_signal_ref(array);
        let index_signal = self.node_to_signal_ref(index);

        // Get the array signal to determine element width
        // For now, assume 8-bit elements (we'll need to track this properly later)
        let element_width = 8; // TODO: Get from array type info

        let output_signal_name = format!("node_{}_out", node_id);
        let output_signal = SignalRef {
            signal_id: output_signal_name.clone(),
            bit_range: None,
        };
        self.sir.signals.push(SirSignal {
            name: output_signal_name,
            width: element_width,
            is_state: false,
            driver_node: Some(node_id),
            fanout_nodes: Vec::new(),
        });

        let node = SirNode {
            id: node_id,
            kind: SirNodeKind::ArrayRead,
            inputs: vec![array_signal, index_signal],
            outputs: vec![output_signal],
            clock_domain: None,
        };

        eprintln!(
            "   🔗 Adding array read node {} to combinational_nodes",
            node_id
        );
        self.sir.combinational_nodes.push(node);
        node_id
    }

    fn create_array_write_node(&mut self, old_array: usize, index: usize, value: usize) -> usize {
        eprintln!(
            "✍️ ARRAY WRITE: Creating array write node from old_array={}, index={}, value={}",
            old_array, index, value
        );
        let node_id = self.next_node_id();

        let old_array_signal = self.node_to_signal_ref(old_array);
        let index_signal = self.node_to_signal_ref(index);
        let value_signal = self.node_to_signal_ref(value);

        // Get the array signal to determine total width
        // For now, assume same width as old array
        let array_width = self.get_signal_width(&old_array_signal.signal_id);

        let output_signal_name = format!("node_{}_out", node_id);
        let output_signal = SignalRef {
            signal_id: output_signal_name.clone(),
            bit_range: None,
        };
        self.sir.signals.push(SirSignal {
            name: output_signal_name,
            width: array_width,
            is_state: false,
            driver_node: Some(node_id),
            fanout_nodes: Vec::new(),
        });

        let node = SirNode {
            id: node_id,
            kind: SirNodeKind::ArrayWrite,
            inputs: vec![old_array_signal, index_signal, value_signal],
            outputs: vec![output_signal],
            clock_domain: None,
        };

        eprintln!(
            "   🔗 Adding array write node {} to combinational_nodes",
            node_id
        );
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
                        let mux_node_id =
                            self.create_mux_node(cond_node, then_value_node, else_value_node);
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

    fn extract_assignments_from_block(
        &self,
        statements: &[Statement],
        assignments: &mut HashMap<String, Expression>,
    ) {
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
            eprintln!(
                "   ✅ Signal '{}' now driven by node {}",
                signal_name, node_id
            );
        } else {
            eprintln!("   ❌ Signal '{}' not found!", signal_name);
        }

        // Update node to output to this signal
        if let Some(node) = self
            .sir
            .combinational_nodes
            .iter_mut()
            .chain(self.sir.sequential_nodes.iter_mut())
            .find(|n| n.id == node_id)
        {
            // For sequential nodes (FlipFlops), replace the temporary output with the actual state signal
            // For combinational nodes, just add the output
            let is_sequential = matches!(node.kind, SirNodeKind::FlipFlop { .. });
            if is_sequential && !node.outputs.is_empty() {
                // Replace the first output (node_X_out) with the actual state signal
                node.outputs[0] = SignalRef {
                    signal_id: signal_name.to_string(),
                    bit_range: None,
                };
                eprintln!(
                    "   🔄 SEQUENTIAL: Replaced output with state signal '{}'",
                    signal_name
                );
            } else {
                node.outputs.push(SignalRef {
                    signal_id: signal_name.to_string(),
                    bit_range: None,
                });
            }
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
        let port_width = self
            .mir
            .ports
            .iter()
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
            kind: SirNodeKind::SignalRef {
                signal: port_name.to_string(),
            },
            inputs: vec![],
            outputs: vec![output_signal],
            clock_domain: None,
        };

        self.sir.combinational_nodes.push(node);
        node_id
    }

    fn get_or_create_signal_driver(&mut self, name: &str) -> usize {
        // CRITICAL: If this is a state element (register), we MUST create a SignalRef
        // reader node, even if the signal already has a driver (the flip-flop).
        // The flip-flop is the WRITER, but we need a READER node for combinational logic.
        let is_state_element = self.sir.state_elements.contains_key(name);

        if !is_state_element {
            // For non-state signals, return existing driver if available
            if let Some(signal) = self.sir.signals.iter().find(|s| s.name == name) {
                if let Some(driver) = signal.driver_node {
                    println!(
                        "      ✅ DRIVER FOUND: {} already has driver node_{}",
                        name, driver
                    );
                    return driver;
                }
                println!(
                    "      ⚠️ SIGNAL EXISTS BUT NO DRIVER: {} (will create reader)",
                    name
                );
            } else {
                println!("      ❌ SIGNAL NOT FOUND: {} (will create reader)", name);
            }
        } else {
            println!(
                "      📖 STATE ELEMENT READ: {} (creating SignalRef reader)",
                name
            );
        }

        // Create a signal reader node that reads from state or signals
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
            kind: SirNodeKind::SignalRef {
                signal: name.to_string(),
            },
            inputs: vec![SignalRef {
                signal_id: name.to_string(),
                bit_range: None,
            }],
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
            FAdd => BinaryOperation::Add,
            FSub => BinaryOperation::Sub,
            FMul => BinaryOperation::Mul,
            FDiv => BinaryOperation::Div,
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
            FEqual => BinaryOperation::Eq,
            FNotEqual => BinaryOperation::Neq,
            FLess => BinaryOperation::Lt,
            FLessEqual => BinaryOperation::Lte,
            FGreater => BinaryOperation::Gt,
            FGreaterEqual => BinaryOperation::Gte,
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

    #[allow(clippy::only_used_in_recursion)]
    fn get_width(&self, data_type: &DataType) -> usize {
        match data_type {
            DataType::Bit(w) | DataType::Logic(w) => *w,
            DataType::Int(w) | DataType::Nat(w) => *w,
            DataType::Clock { .. } | DataType::Reset { .. } => 1,
            DataType::Array(dt, size) => self.get_width(dt) * size,
            DataType::BitParam { default, .. }
            | DataType::LogicParam { default, .. }
            | DataType::IntParam { default, .. }
            | DataType::NatParam { default, .. } => *default,
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

    /// Flatten hierarchical module instances into the SIR
    /// This recursively elaborates all instances, creating unique signals/states for each
    ///
    /// # Arguments
    /// * `instance_prefix` - Hierarchical path prefix (e.g., "stage1." for nested instances)
    fn flatten_instances(&mut self, instance_prefix: &str) {
        println!(
            "🔧 Flattening instances in module '{}' with prefix '{}'",
            self.mir.name, instance_prefix
        );

        // Clone the instances list to avoid borrow checker issues
        let instances = self.mir.instances.clone();

        for instance in &instances {
            println!(
                "   📦 Processing instance '{}' of module {:?}",
                instance.name, instance.module
            );

            // Find the module being instantiated
            let child_module = self
                .mir_design
                .modules
                .iter()
                .find(|m| m.id == instance.module)
                .unwrap_or_else(|| {
                    panic!(
                        "Module {:?} not found for instance {}",
                        instance.module, instance.name
                    )
                });

            println!("      ├─ Child module: {}", child_module.name);
            println!("      ├─ Ports: {}", child_module.ports.len());
            println!("      ├─ Signals: {}", child_module.signals.len());
            println!("      ├─ Processes: {}", child_module.processes.len());
            println!("      └─ Instances: {}", child_module.instances.len());

            // Create hierarchical prefix for this instance
            let inst_prefix = format!("{}{}", instance_prefix, instance.name);

            // Elaborate child module into parent's SIR
            self.elaborate_instance(instance, child_module, &inst_prefix);
        }
    }

    /// Elaborate a single instance by copying its logic into the parent SIR
    /// All signals/states are prefixed with the instance path to maintain uniqueness
    fn elaborate_instance(
        &mut self,
        instance: &skalp_mir::ModuleInstance,
        child_module: &Module,
        inst_prefix: &str,
    ) {
        println!("      🔨 Elaborating instance '{}'", inst_prefix);

        // Step 1: Create signals for child module's internal signals
        // These get prefixed with instance name (e.g., "stage1.reg")
        for signal in &child_module.signals {
            let prefixed_name = format!("{}.{}", inst_prefix, signal.name);
            let width = self.get_width(&signal.signal_type);

            // Check if this signal is a register in the child module
            let is_register = self.is_signal_sequential_in_module(signal.id, child_module);

            println!(
                "         ├─ Signal: {} (width={}, is_reg={})",
                prefixed_name, width, is_register
            );

            self.sir.signals.push(SirSignal {
                name: prefixed_name.clone(),
                width,
                driver_node: None,
                fanout_nodes: Vec::new(),
                is_state: is_register,
            });

            if is_register {
                self.sir.state_elements.insert(
                    prefixed_name.clone(),
                    StateElement {
                        name: prefixed_name,
                        width,
                        reset_value: None,
                        clock: String::new(),
                        reset: None,
                    },
                );
            }
        }

        // Step 2: Convert child module's logic with instance prefix
        // This includes both combinational assignments and sequential processes
        self.elaborate_child_logic(child_module, inst_prefix, instance);

        // Step 3: Recursively elaborate any instances within the child
        if !child_module.instances.is_empty() {
            println!(
                "         └─ Recursively elaborating {} nested instances",
                child_module.instances.len()
            );
            let nested_prefix = format!("{}.", inst_prefix);
            self.flatten_instances_for_module(child_module, &nested_prefix);
        }
    }

    /// Elaborate child module's logic (assignments and processes) with instance prefix
    fn elaborate_child_logic(
        &mut self,
        child_module: &Module,
        inst_prefix: &str,
        instance: &skalp_mir::ModuleInstance,
    ) {
        // Create a mapping from child port names to parent expressions
        // This connects the instance ports to the parent's signals
        let mut port_mapping: HashMap<String, Expression> = HashMap::new();
        for (port_name, parent_expr) in &instance.connections {
            port_mapping.insert(port_name.clone(), parent_expr.clone());
        }

        // Convert combinational assignments from child module
        for assign in &child_module.assignments {
            self.convert_child_assignment(assign, inst_prefix, &port_mapping, child_module);
        }

        // Convert sequential processes from child module
        for process in &child_module.processes {
            self.convert_child_process(process, inst_prefix, &port_mapping, child_module);
        }
    }

    /// Convert child module assignment with instance prefix and port mapping
    fn convert_child_assignment(
        &mut self,
        assign: &skalp_mir::ContinuousAssign,
        inst_prefix: &str,
        port_mapping: &HashMap<String, Expression>,
        child_module: &Module,
    ) {
        // Translate LHS: if it's a port (output), map to parent signal
        // Otherwise prefix with instance name
        eprintln!(
            "🔍 convert_child_assignment: LHS = {:?} in instance {}",
            assign.lhs, inst_prefix
        );
        let lhs_signal = match &assign.lhs {
            LValue::Signal(sig_id) => {
                if let Some(signal) = child_module.signals.iter().find(|s| s.id == *sig_id) {
                    eprintln!("   → Internal signal: {}.{}", inst_prefix, signal.name);
                    format!("{}.{}", inst_prefix, signal.name)
                } else if let Some(port) = child_module.ports.iter().find(|p| p.id.0 == sig_id.0) {
                    eprintln!("   → Port: {} (direction={:?})", port.name, port.direction);
                    // Output port - this connects to parent signal
                    if let Some(parent_expr) = port_mapping.get(&port.name) {
                        let parent_sig = self.get_signal_name_from_expression(parent_expr);
                        eprintln!("   → Mapped to parent signal: {}", parent_sig);
                        parent_sig
                    } else {
                        eprintln!("   ⚠️ Port {} not in connections!", port.name);
                        format!("{}.{}", inst_prefix, port.name)
                    }
                } else {
                    eprintln!("   ⚠️ Signal {:?} not found", sig_id);
                    format!("{}.unknown", inst_prefix)
                }
            }
            LValue::Port(port_id) => {
                eprintln!("   → Direct LValue::Port({:?})", port_id);
                if let Some(port) = child_module.ports.iter().find(|p| p.id == *port_id) {
                    eprintln!("   → Port: {} (direction={:?})", port.name, port.direction);
                    if let Some(parent_expr) = port_mapping.get(&port.name) {
                        let parent_sig = self.get_signal_name_from_expression(parent_expr);
                        eprintln!("   → Mapped to parent signal: {}", parent_sig);
                        parent_sig
                    } else {
                        eprintln!("   ⚠️ Port {} not in connections!", port.name);
                        format!("{}.{}", inst_prefix, port.name)
                    }
                } else {
                    eprintln!("   ⚠️ Port {:?} not found", port_id);
                    format!("{}.unknown_port", inst_prefix)
                }
            }
            _ => {
                eprintln!("   → Complex LHS: {:?}", assign.lhs);
                format!("{}.complex_lhs", inst_prefix)
            }
        };

        println!("            ├─ Assignment: {} = <expr>", lhs_signal);

        // Translate RHS expression with port mapping
        let node_id = self.node_counter;
        self.node_counter += 1;

        // Convert the RHS expression, substituting port references with parent signals
        self.convert_expression_for_instance(
            &assign.rhs,
            inst_prefix,
            port_mapping,
            child_module,
            &lhs_signal,
            node_id,
        );
    }

    /// Convert child process (always block) with instance prefix
    fn convert_child_process(
        &mut self,
        process: &skalp_mir::Process,
        inst_prefix: &str,
        port_mapping: &HashMap<String, Expression>,
        child_module: &Module,
    ) {
        println!("            ├─ Process: {:?}", process.kind);

        // For sequential processes, we need to:
        // 1. Extract clock edge from sensitivity list
        // 2. Map the clock/reset signals through port connections
        // 3. Create FlipFlop nodes for sequential assignments

        if process.kind == ProcessKind::Sequential {
            // Extract clock edge info
            if let SensitivityList::Edge(edges) = &process.sensitivity {
                if let Some(edge_sens) = edges.first() {
                    let clock_edge = match edge_sens.edge {
                        skalp_mir::EdgeType::Rising => ClockEdge::Rising,
                        skalp_mir::EdgeType::Falling => ClockEdge::Falling,
                        _ => ClockEdge::Rising, // Default for Both/Active/Inactive
                    };

                    // Get clock signal name, mapping through ports if needed
                    let clock_lvalue = &edge_sens.signal;
                    let clock_signal = if let Some(sig_name) = self.get_signal_from_lvalue(
                        clock_lvalue,
                        inst_prefix,
                        port_mapping,
                        child_module,
                    ) {
                        sig_name
                    } else {
                        "clk".to_string()
                    };

                    // Process sequential block: collect targets, build MUX trees, create FlipFlops
                    // This mirrors how the regular (non-hierarchical) conversion works

                    // Collect all targets assigned in this sequential block
                    let mut targets = std::collections::HashSet::new();
                    for statement in &process.body.statements {
                        self.collect_assignment_targets_for_instance(
                            statement,
                            inst_prefix,
                            port_mapping,
                            child_module,
                            &mut targets,
                        );
                    }

                    // For each target, synthesize conditional logic and create FlipFlop
                    for target in targets {
                        // Build combinational logic (including MUXes for conditionals)
                        let data_node = self.synthesize_sequential_logic_for_instance(
                            &process.body.statements,
                            inst_prefix,
                            port_mapping,
                            child_module,
                            &target,
                        );

                        let node_id = self.node_counter;
                        self.node_counter += 1;

                        // Create FlipFlop node with clock and synthesized data
                        let ff_node = SirNode {
                            id: node_id,
                            kind: SirNodeKind::FlipFlop {
                                clock_edge: clock_edge.clone(),
                            },
                            inputs: vec![
                                SignalRef {
                                    signal_id: clock_signal.clone(),
                                    bit_range: None,
                                },
                                SignalRef {
                                    signal_id: format!("node_{}_out", data_node),
                                    bit_range: None,
                                },
                            ],
                            outputs: vec![SignalRef {
                                signal_id: target.clone(),
                                bit_range: None,
                            }],
                            clock_domain: Some(clock_signal.clone()),
                        };

                        self.sir.sequential_nodes.push(ff_node);

                        // Mark the signal as having this driver
                        if let Some(sig) = self.sir.signals.iter_mut().find(|s| s.name == target) {
                            sig.driver_node = Some(node_id);
                        }
                    }
                }
            }
        } else {
            // Combinational process
            for statement in &process.body.statements {
                self.convert_statement_for_instance(
                    statement,
                    inst_prefix,
                    port_mapping,
                    child_module,
                    false,
                );
            }
        }
    }

    /// Collect assignment targets in a sequential block for an instance
    #[allow(clippy::only_used_in_recursion)]
    fn collect_assignment_targets_for_instance(
        &self,
        statement: &Statement,
        inst_prefix: &str,
        port_mapping: &HashMap<String, Expression>,
        child_module: &Module,
        targets: &mut std::collections::HashSet<String>,
    ) {
        match statement {
            Statement::Assignment(assign) => {
                // Get LHS signal name with prefix
                let lhs_signal = match &assign.lhs {
                    LValue::Signal(sig_id) => {
                        if let Some(signal) = child_module.signals.iter().find(|s| s.id == *sig_id)
                        {
                            format!("{}.{}", inst_prefix, signal.name)
                        } else {
                            return; // Skip if not found
                        }
                    }
                    _ => return,
                };
                targets.insert(lhs_signal);
            }
            Statement::If(if_stmt) => {
                // Recursively collect from both branches
                for stmt in &if_stmt.then_block.statements {
                    self.collect_assignment_targets_for_instance(
                        stmt,
                        inst_prefix,
                        port_mapping,
                        child_module,
                        targets,
                    );
                }
                if let Some(else_block) = &if_stmt.else_block {
                    for stmt in &else_block.statements {
                        self.collect_assignment_targets_for_instance(
                            stmt,
                            inst_prefix,
                            port_mapping,
                            child_module,
                            targets,
                        );
                    }
                }
            }
            _ => {}
        }
    }

    /// Synthesize sequential logic for a target signal in instance
    /// This builds MUX trees for conditional assignments
    fn synthesize_sequential_logic_for_instance(
        &mut self,
        statements: &[Statement],
        inst_prefix: &str,
        port_mapping: &HashMap<String, Expression>,
        child_module: &Module,
        target: &str,
    ) -> usize {
        // Process all statements to find assignments to target
        for statement in statements {
            match statement {
                Statement::Assignment(assign) => {
                    // Check if this assigns to our target
                    let lhs = match &assign.lhs {
                        LValue::Signal(sig_id) => {
                            if let Some(signal) =
                                child_module.signals.iter().find(|s| s.id == *sig_id)
                            {
                                format!("{}.{}", inst_prefix, signal.name)
                            } else {
                                continue;
                            }
                        }
                        _ => continue,
                    };

                    if lhs == target {
                        // Direct assignment to target - create node for RHS
                        return self.create_expression_node_for_instance(
                            &assign.rhs,
                            inst_prefix,
                            port_mapping,
                            child_module,
                        );
                    }
                }
                Statement::If(if_stmt) => {
                    // Build conditional MUX
                    return self.synthesize_conditional_for_instance(
                        if_stmt,
                        inst_prefix,
                        port_mapping,
                        child_module,
                        target,
                    );
                }
                _ => {}
            }
        }

        // No assignment found - use signal ref (keep current value)
        self.create_signal_ref(target)
    }

    /// Synthesize conditional assignment for instance (creates MUX nodes)
    fn synthesize_conditional_for_instance(
        &mut self,
        if_stmt: &IfStatement,
        inst_prefix: &str,
        port_mapping: &HashMap<String, Expression>,
        child_module: &Module,
        target: &str,
    ) -> usize {
        // Get values from then and else branches
        let then_value = self.find_assignment_in_branch_for_instance(
            &if_stmt.then_block.statements,
            inst_prefix,
            port_mapping,
            child_module,
            target,
        );

        let else_value = if let Some(else_block) = &if_stmt.else_block {
            self.find_assignment_in_branch_for_instance(
                &else_block.statements,
                inst_prefix,
                port_mapping,
                child_module,
                target,
            )
        } else {
            None
        };

        // Build MUX based on what was found
        match (then_value, else_value) {
            (Some(then_val), Some(else_val)) => {
                // Both branches assign: mux(cond, then, else)
                let condition = self.create_expression_node_for_instance(
                    &if_stmt.condition,
                    inst_prefix,
                    port_mapping,
                    child_module,
                );
                self.create_mux_node(condition, then_val, else_val)
            }
            (Some(then_val), None) => {
                // Only then assigns: mux(cond, then, keep)
                let condition = self.create_expression_node_for_instance(
                    &if_stmt.condition,
                    inst_prefix,
                    port_mapping,
                    child_module,
                );
                let keep_val = self.create_signal_ref(target);
                self.create_mux_node(condition, then_val, keep_val)
            }
            (None, Some(else_val)) => {
                // Only else assigns: mux(cond, keep, else)
                let condition = self.create_expression_node_for_instance(
                    &if_stmt.condition,
                    inst_prefix,
                    port_mapping,
                    child_module,
                );
                let keep_val = self.create_signal_ref(target);
                self.create_mux_node(condition, keep_val, else_val)
            }
            (None, None) => {
                // Neither assigns: keep current value
                self.create_signal_ref(target)
            }
        }
    }

    /// Find assignment to target in a branch
    fn find_assignment_in_branch_for_instance(
        &mut self,
        statements: &[Statement],
        inst_prefix: &str,
        port_mapping: &HashMap<String, Expression>,
        child_module: &Module,
        target: &str,
    ) -> Option<usize> {
        for stmt in statements {
            match stmt {
                Statement::Assignment(assign) => {
                    let lhs = match &assign.lhs {
                        LValue::Signal(sig_id) => {
                            if let Some(signal) =
                                child_module.signals.iter().find(|s| s.id == *sig_id)
                            {
                                format!("{}.{}", inst_prefix, signal.name)
                            } else {
                                continue;
                            }
                        }
                        _ => continue,
                    };

                    if lhs == target {
                        return Some(self.create_expression_node_for_instance(
                            &assign.rhs,
                            inst_prefix,
                            port_mapping,
                            child_module,
                        ));
                    }
                }
                Statement::If(nested_if) => {
                    // Recursively handle nested if
                    return Some(self.synthesize_conditional_for_instance(
                        nested_if,
                        inst_prefix,
                        port_mapping,
                        child_module,
                        target,
                    ));
                }
                _ => {}
            }
        }
        None
    }

    /// Create expression node for instance context
    fn create_expression_node_for_instance(
        &mut self,
        expr: &Expression,
        inst_prefix: &str,
        port_mapping: &HashMap<String, Expression>,
        child_module: &Module,
    ) -> usize {
        match expr {
            Expression::Ref(lvalue) => {
                // Map signal reference through ports
                eprintln!(
                    "🔍 Expression::Ref processing lvalue: {:?} in instance {}",
                    lvalue, inst_prefix
                );
                if let Some(sig_name) =
                    self.get_signal_from_lvalue(lvalue, inst_prefix, port_mapping, child_module)
                {
                    eprintln!("   ✓ get_signal_from_lvalue returned: {}", sig_name);
                    self.create_signal_ref(&sig_name)
                } else {
                    eprintln!("   ✗ get_signal_from_lvalue returned None, trying fallback");
                    // Try to resolve as a simple signal name for debugging
                    let fallback_name = match lvalue {
                        LValue::Signal(sig_id) => {
                            // Try to find the signal in child module
                            if let Some(signal) =
                                child_module.signals.iter().find(|s| s.id == *sig_id)
                            {
                                format!("{}.{}", inst_prefix, signal.name)
                            } else if let Some(port) =
                                child_module.ports.iter().find(|p| p.id.0 == sig_id.0)
                            {
                                // Port reference - map through port_mapping
                                if let Some(parent_expr) = port_mapping.get(&port.name) {
                                    self.get_signal_name_from_expression(parent_expr)
                                } else {
                                    eprintln!(
                                        "⚠️ Port {} not in port_mapping for instance {}",
                                        port.name, inst_prefix
                                    );
                                    port.name.clone()
                                }
                            } else {
                                eprintln!(
                                    "⚠️ Unresolved signal {:?} in instance {}",
                                    sig_id, inst_prefix
                                );
                                "unknown".to_string()
                            }
                        }
                        LValue::Port(port_id) => {
                            // Direct port reference
                            eprintln!(
                                "🔍 LValue::Port matched for port_id={:?} in instance {}",
                                port_id, inst_prefix
                            );
                            if let Some(port) = child_module.ports.iter().find(|p| p.id == *port_id)
                            {
                                eprintln!("   Found port: {}", port.name);
                                // Map through port_mapping
                                if let Some(parent_expr) = port_mapping.get(&port.name) {
                                    let resolved =
                                        self.get_signal_name_from_expression(parent_expr);
                                    eprintln!(
                                        "   Port {} mapped to expression → signal: {}",
                                        port.name, resolved
                                    );
                                    resolved
                                } else {
                                    eprintln!(
                                        "⚠️ Port {} not in port_mapping for instance {}",
                                        port.name, inst_prefix
                                    );
                                    port.name.clone()
                                }
                            } else {
                                eprintln!(
                                    "⚠️ Unresolved port {:?} in instance {}",
                                    port_id, inst_prefix
                                );
                                "unknown".to_string()
                            }
                        }
                        _ => {
                            eprintln!("⚠️ Complex lvalue {:?} in instance {}", lvalue, inst_prefix);
                            "complex_lvalue".to_string()
                        }
                    };
                    self.create_signal_ref(&fallback_name)
                }
            }
            Expression::Literal(value) => {
                // Create constant node
                let (val, width) = match value {
                    skalp_mir::Value::Integer(i) => {
                        let w = if *i <= 0xFF {
                            8
                        } else if *i <= 0xFFFF {
                            16
                        } else {
                            32
                        };
                        (*i as u64, w)
                    }
                    skalp_mir::Value::BitVector { width, value } => (*value, *width),
                    _ => (0, 8),
                };
                self.create_constant_node(val, width)
            }
            Expression::Binary { op, left, right } => {
                let left_node = self.create_expression_node_for_instance(
                    left,
                    inst_prefix,
                    port_mapping,
                    child_module,
                );
                let right_node = self.create_expression_node_for_instance(
                    right,
                    inst_prefix,
                    port_mapping,
                    child_module,
                );

                // Use the existing create_binary_op_node which handles BinaryOp correctly
                self.create_binary_op_node(op, left_node, right_node)
            }
            _ => {
                // For complex expressions, create a placeholder
                let node_id = self.node_counter;
                self.node_counter += 1;
                node_id
            }
        }
    }

    /// Convert a statement from child module with instance prefix
    fn convert_statement_for_instance(
        &mut self,
        statement: &Statement,
        inst_prefix: &str,
        port_mapping: &HashMap<String, Expression>,
        child_module: &Module,
        is_sequential: bool,
    ) {
        match statement {
            Statement::Assignment(assign) => {
                // Get LHS signal name with prefix
                let lhs_signal = match &assign.lhs {
                    LValue::Signal(sig_id) => {
                        if let Some(signal) = child_module.signals.iter().find(|s| s.id == *sig_id)
                        {
                            format!("{}.{}", inst_prefix, signal.name)
                        } else if let Some(port) =
                            child_module.ports.iter().find(|p| p.id.0 == sig_id.0)
                        {
                            // Output port
                            if let Some(parent_expr) = port_mapping.get(&port.name) {
                                self.get_signal_name_from_expression(parent_expr)
                            } else {
                                format!("{}.{}", inst_prefix, port.name)
                            }
                        } else {
                            format!("{}.unknown", inst_prefix)
                        }
                    }
                    _ => format!("{}.complex", inst_prefix),
                };

                let node_id = self.node_counter;
                self.node_counter += 1;

                if is_sequential {
                    // Sequential assignment - create sequential node
                    self.convert_expression_for_instance(
                        &assign.rhs,
                        inst_prefix,
                        port_mapping,
                        child_module,
                        &lhs_signal,
                        node_id,
                    );

                    // Mark the signal as having sequential driver
                    if let Some(sig) = self.sir.signals.iter_mut().find(|s| s.name == lhs_signal) {
                        sig.driver_node = Some(node_id);
                    }
                } else {
                    // Combinational assignment
                    self.convert_expression_for_instance(
                        &assign.rhs,
                        inst_prefix,
                        port_mapping,
                        child_module,
                        &lhs_signal,
                        node_id,
                    );
                }
            }
            Statement::If(if_stmt) => {
                // Recursively handle if statements
                for stmt in &if_stmt.then_block.statements {
                    self.convert_statement_for_instance(
                        stmt,
                        inst_prefix,
                        port_mapping,
                        child_module,
                        is_sequential,
                    );
                }
                if let Some(else_block) = &if_stmt.else_block {
                    for stmt in &else_block.statements {
                        self.convert_statement_for_instance(
                            stmt,
                            inst_prefix,
                            port_mapping,
                            child_module,
                            is_sequential,
                        );
                    }
                }
            }
            _ => {
                // Handle other statement types as needed
            }
        }
    }

    /// Convert expression from child module, mapping ports to parent signals
    fn convert_expression_for_instance(
        &mut self,
        expr: &Expression,
        inst_prefix: &str,
        port_mapping: &HashMap<String, Expression>,
        child_module: &Module,
        output_signal: &str,
        node_id: usize,
    ) {
        // For simple signal references, create a SignalRef node
        // For complex expressions, we'd need to build the full expression tree

        match expr {
            Expression::Ref(lvalue) => {
                // Simple signal reference: output = input
                if let Some(input_sig) =
                    self.get_signal_from_lvalue(lvalue, inst_prefix, port_mapping, child_module)
                {
                    let node = SirNode {
                        id: node_id,
                        kind: SirNodeKind::SignalRef {
                            signal: input_sig.clone(), // Signal being READ FROM
                        },
                        inputs: vec![SignalRef {
                            signal_id: input_sig,
                            bit_range: None,
                        }],
                        outputs: vec![SignalRef {
                            signal_id: output_signal.to_string(),
                            bit_range: None,
                        }],
                        clock_domain: None,
                    };
                    self.sir.combinational_nodes.push(node);
                    // Register this node as the driver of the output signal
                    self.connect_node_to_signal(node_id, output_signal);
                } else {
                    eprintln!(
                        "⚠️ Failed to resolve signal reference for assignment to {}",
                        output_signal
                    );
                }
            }
            _ => {
                // Complex expression - collect inputs and create a generic node
                let inputs =
                    self.collect_expression_inputs(expr, inst_prefix, port_mapping, child_module);

                let node = SirNode {
                    id: node_id,
                    kind: SirNodeKind::SignalRef {
                        signal: output_signal.to_string(),
                    },
                    inputs,
                    outputs: vec![SignalRef {
                        signal_id: output_signal.to_string(),
                        bit_range: None,
                    }],
                    clock_domain: None,
                };

                self.sir.combinational_nodes.push(node);
                // Register this node as the driver of the output signal
                self.connect_node_to_signal(node_id, output_signal);
            }
        }
    }

    /// Collect input signal references from expression
    fn collect_expression_inputs(
        &self,
        expr: &Expression,
        inst_prefix: &str,
        port_mapping: &HashMap<String, Expression>,
        child_module: &Module,
    ) -> Vec<SignalRef> {
        let mut inputs = Vec::new();

        match expr {
            Expression::Ref(lvalue) => {
                // Extract signal from LValue
                if let Some(sig_name) =
                    self.get_signal_from_lvalue(lvalue, inst_prefix, port_mapping, child_module)
                {
                    inputs.push(SignalRef {
                        signal_id: sig_name,
                        bit_range: None,
                    });
                }
            }
            Expression::Binary { op: _, left, right } => {
                inputs.extend(self.collect_expression_inputs(
                    left,
                    inst_prefix,
                    port_mapping,
                    child_module,
                ));
                inputs.extend(self.collect_expression_inputs(
                    right,
                    inst_prefix,
                    port_mapping,
                    child_module,
                ));
            }
            Expression::Unary { op: _, operand } => {
                inputs.extend(self.collect_expression_inputs(
                    operand,
                    inst_prefix,
                    port_mapping,
                    child_module,
                ));
            }
            Expression::Conditional {
                cond,
                then_expr,
                else_expr,
            } => {
                inputs.extend(self.collect_expression_inputs(
                    cond,
                    inst_prefix,
                    port_mapping,
                    child_module,
                ));
                inputs.extend(self.collect_expression_inputs(
                    then_expr,
                    inst_prefix,
                    port_mapping,
                    child_module,
                ));
                inputs.extend(self.collect_expression_inputs(
                    else_expr,
                    inst_prefix,
                    port_mapping,
                    child_module,
                ));
            }
            _ => {}
        }

        inputs
    }

    /// Get signal name from LValue, handling instance prefixing and port mapping
    fn get_signal_from_lvalue(
        &self,
        lvalue: &LValue,
        inst_prefix: &str,
        port_mapping: &HashMap<String, Expression>,
        child_module: &Module,
    ) -> Option<String> {
        match lvalue {
            LValue::Signal(sig_id) => {
                if let Some(signal) = child_module.signals.iter().find(|s| s.id == *sig_id) {
                    Some(format!("{}.{}", inst_prefix, signal.name))
                } else if let Some(port) = child_module.ports.iter().find(|p| p.id.0 == sig_id.0) {
                    // Input port - map to parent signal
                    if let Some(parent_expr) = port_mapping.get(&port.name) {
                        Some(self.get_signal_name_from_expression(parent_expr))
                    } else {
                        Some(format!("{}.{}", inst_prefix, port.name))
                    }
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    /// Extract signal name from expression (for port mapping)
    fn get_signal_name_from_expression(&self, expr: &Expression) -> String {
        match expr {
            Expression::Ref(lvalue) => {
                match lvalue {
                    LValue::Signal(sig_id) => {
                        // Find signal by ID in current module
                        if let Some(signal) = self.mir.signals.iter().find(|s| s.id == *sig_id) {
                            signal.name.clone()
                        } else if let Some(port) =
                            self.mir.ports.iter().find(|p| p.id.0 == sig_id.0)
                        {
                            port.name.clone()
                        } else {
                            format!("unknown_signal_{}", sig_id.0)
                        }
                    }
                    LValue::Port(port_id) => {
                        // Find port by ID in current module (top-level)
                        if let Some(port) = self.mir.ports.iter().find(|p| p.id == *port_id) {
                            port.name.clone()
                        } else {
                            format!("unknown_port_{:?}", port_id)
                        }
                    }
                    _ => "complex_lvalue".to_string(),
                }
            }
            _ => "complex_expr".to_string(),
        }
    }

    /// Check if signal is sequential in a given module
    fn is_signal_sequential_in_module(
        &self,
        signal_id: skalp_mir::SignalId,
        module: &Module,
    ) -> bool {
        for process in &module.processes {
            if process.kind == ProcessKind::Sequential
                && self.is_signal_assigned_in_block(&process.body, signal_id)
            {
                return true;
            }
        }
        false
    }

    /// Flatten instances for a specific module (used for recursion)
    fn flatten_instances_for_module(&mut self, module: &Module, instance_prefix: &str) {
        let instances = module.instances.clone();

        for instance in &instances {
            let child_module = self
                .mir_design
                .modules
                .iter()
                .find(|m| m.id == instance.module)
                .unwrap_or_else(|| panic!("Module {:?} not found", instance.module));

            let inst_prefix = format!("{}{}", instance_prefix, instance.name);
            self.elaborate_instance(instance, child_module, &inst_prefix);
        }
    }

    fn extract_clock_domains(&mut self) {
        let mut domains: HashMap<String, ClockDomain> = HashMap::new();

        for process in &self.mir.processes {
            if process.kind == ProcessKind::Sequential {
                if let SensitivityList::Edge(edges) = &process.sensitivity {
                    if let Some(edge_sens) = edges.first() {
                        let clock_name = self.lvalue_to_string(&edge_sens.signal);
                        domains
                            .entry(clock_name.clone())
                            .or_insert_with(|| ClockDomain {
                                name: clock_name.clone(),
                                frequency_hz: None,
                                phase_offset: 0.0,
                                state_elements: Vec::new(),
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

    fn create_priority_mux_with_context(
        &mut self,
        mux: &PriorityMux,
        local_context: &std::collections::HashMap<String, usize>,
    ) -> usize {
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
            Expression::Literal(value) => match value {
                Value::Integer(i) => Some(*i as u64),
                Value::BitVector { value, .. } => Some(*value),
                _ => None,
            },
            _ => None, // Only literals can be evaluated as constants for now
        }
    }
}
