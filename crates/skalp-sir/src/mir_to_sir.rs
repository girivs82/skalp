use crate::sir::*;
use skalp_mir::mir::PortDirection as MirPortDirection;
use skalp_mir::mir::PriorityMux;
use skalp_mir::{
    BinaryOp, Block, DataType, EdgeType, Expression, ExpressionKind, IfStatement, LValue, Mir, Module, ProcessKind,
    SensitivityList, SignalId, Statement, Value,
};
use std::collections::HashMap;

// Disable verbose debug output for performance
macro_rules! eprintln {
    ($($arg:tt)*) => {{}};
}

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
    eprintln!("📊 Available modules in MIR:");
    for m in &mir.modules {
        eprintln!(
            "   - {} (ID={:?}): {} signals",
            m.name,
            m.id,
            m.signals.len()
        );
        if m.name.contains("AsyncFifo_8") {
            eprintln!("      🔍 AsyncFifo_8 signals:");
            for sig in m.signals.iter().take(15) {
                eprintln!("         - {}: {:?}", sig.name, sig.signal_type);
            }
        }
    }

    let mut sir = SirModule::new(top_module.name.clone());

    // Propagate pipeline configuration from MIR to SIR
    sir.pipeline_config = top_module.pipeline_config.clone();
    if sir.pipeline_config.is_some() {
        println!("🔧 PIPELINE: Module '{}' has pipeline config: {:?}", top_module.name, sir.pipeline_config);
    }

    let mut converter = MirToSirConverter::new(&mut sir, top_module, mir);

    converter.convert_ports();
    converter.convert_signals();
    converter.convert_variables(); // Convert MIR variables (let bindings) to SIR signals

    // CRITICAL: Flatten hierarchical instances BEFORE converting top-level logic
    // This ensures that instance output signals are driven before the top-level
    // logic tries to read from them. Without this order, instance result signals
    // (like exec_l4_l5_result_109) won't have drivers when top-level logic
    // tries to use them.
    converter.flatten_instances("");

    converter.convert_logic();

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

    // Pre-compute topological order for efficient simulation
    sir.finalize_topological_order();
    println!(
        "✅ SIR: Pre-computed topological order for {} combinational nodes",
        sir.sorted_combinational_node_ids.len()
    );

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
    // BUG FIX #85: Map tuple temp signal IDs to their module instance prefix
    // This is used to resolve TupleFieldAccess when the base is a _tuple_tmp signal
    tuple_source_map: HashMap<SignalId, String>,
    // BUG FIX #124: Store port_mapping for each instance by prefix
    // This allows nested instances to resolve parent port references during expression conversion
    instance_port_mappings: HashMap<String, HashMap<String, Expression>>,
}

impl<'a> MirToSirConverter<'a> {
    fn new(sir: &'a mut SirModule, mir: &'a Module, mir_design: &'a Mir) -> Self {
        // BUG FIX #85: Build tuple source mapping for TupleFieldAccess resolution
        let tuple_source_map = Self::build_tuple_source_mapping(mir);

        MirToSirConverter {
            sir,
            mir,
            mir_design,
            node_counter: 0,
            signal_map: HashMap::new(),
            conditional_contexts: HashMap::new(),
            tuple_source_map,
            instance_port_mappings: HashMap::new(),
        }
    }

    /// BUG FIX #85: Build a mapping of signal IDs to their module instance source
    /// This is used to correctly resolve TupleFieldAccess expressions when the base
    /// signal is assigned from a module instance result (e.g., `_tuple_tmp_76 = inst_result_0`)
    fn build_tuple_source_mapping(mir: &Module) -> HashMap<SignalId, String> {
        let mut mapping = HashMap::new();

        println!("🔍🔍🔍 build_tuple_source_mapping: scanning {} assignments, {} signals",
                 mir.assignments.len(), mir.signals.len());
        for sig in &mir.signals {
            if sig.name.contains("_tuple_tmp") {
                println!("🔍🔍🔍   Found tuple_tmp signal: {} (id={})", sig.name, sig.id.0);
            }
        }

        for (idx, assign) in mir.assignments.iter().enumerate() {
            // Check if LHS is a signal
            if let LValue::Signal(lhs_id) = &assign.lhs {
                let lhs_signal_name = mir.signals.iter()
                    .find(|s| s.id == *lhs_id)
                    .map(|s| s.name.as_str())
                    .unwrap_or("unknown");

                // Only print tuple_tmp assignments for debugging
                if lhs_signal_name.contains("_tuple_tmp") {
                    println!("🔍🔍🔍   TUPLE_TMP Assignment {}: {} (id={}) = {:?}",
                             idx, lhs_signal_name, lhs_id.0, std::mem::discriminant(&assign.rhs.kind));
                    // Print more details about RHS
                    match &assign.rhs.kind {
                        ExpressionKind::Ref(lv) => println!("🔍🔍🔍     RHS: Ref({:?})", lv),
                        ExpressionKind::Concat(parts) => {
                            println!("🔍🔍🔍     RHS: Concat with {} parts:", parts.len());
                            for (i, part) in parts.iter().enumerate() {
                                println!("🔍🔍🔍       Part {}: {:?}", i, std::mem::discriminant(&part.kind));
                            }
                        }
                        ExpressionKind::FunctionCall { name, .. } => {
                            println!("🔍🔍🔍     RHS: FunctionCall({})", name);
                        }
                        _ => println!("🔍🔍🔍     RHS: Other expression"),
                    }
                }

                // Case 1: RHS is a reference to another signal
                if let ExpressionKind::Ref(LValue::Signal(rhs_id)) = &assign.rhs.kind {
                    // Check if RHS signal name contains "_inst_" and "_result_"
                    // This indicates it's a module instance result signal
                    if let Some(rhs_signal) = mir.signals.iter().find(|s| s.id == *rhs_id) {
                        println!("🔍🔍🔍     RHS is signal ref: '{}'", rhs_signal.name);
                        if rhs_signal.name.contains("_inst_") && rhs_signal.name.contains("_result_") {
                            // Extract the prefix (everything before "_result_N")
                            if let Some(pos) = rhs_signal.name.rfind("_result_") {
                                let prefix = &rhs_signal.name[..pos];
                                println!("🔍🔍🔍     -> Adding mapping: {} -> '{}'", lhs_id.0, prefix);
                                mapping.insert(*lhs_id, prefix.to_string());
                            }
                        }
                    }
                }

                // Case 2: RHS is a Concat - check if first element is a module instance result
                if let ExpressionKind::Concat(parts) = &assign.rhs.kind {
                    println!("🔍🔍🔍     RHS is Concat with {} parts", parts.len());
                    if let Some(first) = parts.first() {
                        if let ExpressionKind::Ref(LValue::Signal(first_sig_id)) = &first.kind {
                            if let Some(first_signal) = mir.signals.iter().find(|s| s.id == *first_sig_id) {
                                println!("🔍🔍🔍     First concat part: '{}'", first_signal.name);
                                if first_signal.name.contains("_inst_") && first_signal.name.contains("_result_") {
                                    if let Some(pos) = first_signal.name.rfind("_result_") {
                                        let prefix = &first_signal.name[..pos];
                                        println!("🔍🔍🔍     -> Adding mapping from Concat: {} -> '{}'", lhs_id.0, prefix);
                                        mapping.insert(*lhs_id, prefix.to_string());
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        println!("🔍🔍🔍 build_tuple_source_mapping: found {} mappings", mapping.len());
        mapping
    }

    fn convert_ports(&mut self) {
        for port in &self.mir.ports {
            let sir_type = self.convert_type(&port.port_type);
            let width = sir_type.width();
            let direction = match port.direction {
                skalp_mir::PortDirection::Input => PortDirection::Input,
                skalp_mir::PortDirection::Output => PortDirection::Output,
                skalp_mir::PortDirection::InOut => PortDirection::Output,
            };

            let sir_port = SirPort {
                name: port.name.clone(),
                width,
                sir_type: sir_type.clone(),
                direction: direction.clone(),
                clock_domain: None,
                span: port.span.clone(),
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
                sir_type: sir_type.clone(),
                driver_node: None,
                fanout_nodes: Vec::new(),
                is_state: is_sequential_port,
                span: port.span.clone(),
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
                        span: port.span.clone(),
                    },
                );
            }
        }
    }

    fn convert_signals(&mut self) {
        for signal in &self.mir.signals {
            let sir_type = self.convert_type(&signal.signal_type);
            let width = sir_type.width();

            eprintln!(
                "📏 Signal '{}': MIR type={:?}, SIR type={:?}, width={}",
                signal.name, signal.signal_type, sir_type, width
            );

            // Determine if this is a register by checking if it's assigned in sequential blocks
            let is_register = self.is_signal_sequential(signal.id);

            self.sir.signals.push(SirSignal {
                name: signal.name.clone(),
                width,
                sir_type: sir_type.clone(),
                driver_node: None,
                fanout_nodes: Vec::new(),
                is_state: is_register,
                span: signal.span.clone(),
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
                        span: signal.span.clone(),
                    },
                );
            }
        }
    }

    /// Convert MIR variables (let bindings) to SIR signals
    /// Variables are treated as combinational wires
    fn convert_variables(&mut self) {
        for variable in &self.mir.variables {
            let sir_type = self.convert_type(&variable.var_type);
            let width = sir_type.width();

            // BUG FIX #86: Use unique signal names (name + id) to prevent collisions
            // between variables with the same name in different scopes
            let unique_name = format!("{}_{}", variable.name, variable.id.0);

            // Variables (let bindings) are always combinational wires, never registers
            self.sir.signals.push(SirSignal {
                name: unique_name,
                width,
                sir_type: sir_type.clone(),
                driver_node: None,
                fanout_nodes: Vec::new(),
                is_state: false, // Variables are never state elements
                span: variable.span.clone(),
            });
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
        println!(
            "🔍 Processing {} continuous assignments",
            self.mir.assignments.len()
        );
        for assign in &self.mir.assignments {
            let target = self.lvalue_to_string(&assign.lhs);
            println!("   📡 CONTINUOUS ASSIGN: {} <= expression", target);
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
        eprintln!(
            "🔍 SEQUENTIAL BLOCK: Processing {} statements",
            statements.len()
        );
        for (stmt_idx, stmt) in statements.iter().enumerate() {
            eprintln!(
                "   Statement {}: {:?}",
                stmt_idx,
                std::mem::discriminant(stmt)
            );
            match stmt {
                Statement::Assignment(assign) => {
                    eprintln!(
                        "      Assignment target: {}",
                        self.lvalue_to_string(&assign.lhs)
                    );
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
                    eprintln!(
                        "      ResolvedConditional target: {}",
                        self.lvalue_to_string(&resolved.target)
                    );
                    eprintln!(
                        "      ResolvedConditional has {} branches",
                        resolved.resolved.cases.len()
                    );
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
        match &expr.kind {
            ExpressionKind::Ref(lvalue) => {
                let signal_name = self.lvalue_to_string(lvalue);
                // Also check the base signal for range selects and bit selects
                let base_signal = self.get_base_signal_name(lvalue);
                signals.contains(&signal_name) || signals.contains(&base_signal)
            }
            ExpressionKind::Binary { left, right, .. } => {
                self.expression_references_signals(left, signals)
                    || self.expression_references_signals(right, signals)
            }
            ExpressionKind::Unary { operand, .. } => {
                self.expression_references_signals(operand, signals)
            }
            ExpressionKind::Conditional {
                cond,
                then_expr,
                else_expr,
            } => {
                self.expression_references_signals(cond, signals)
                    || self.expression_references_signals(then_expr, signals)
                    || self.expression_references_signals(else_expr, signals)
            }
            ExpressionKind::Concat(exprs) => exprs
                .iter()
                .any(|e| self.expression_references_signals(e, signals)),
            ExpressionKind::Replicate { count, value } => {
                self.expression_references_signals(count, signals)
                    || self.expression_references_signals(value, signals)
            }
            ExpressionKind::FunctionCall { args, .. } => args
                .iter()
                .any(|e| self.expression_references_signals(e, signals)),
            // BUG FIX #85: Handle tuple/field access
            ExpressionKind::TupleFieldAccess { base, .. } => {
                self.expression_references_signals(base, signals)
            }
            ExpressionKind::FieldAccess { base, .. } => {
                self.expression_references_signals(base, signals)
            }
            ExpressionKind::Cast { expr, .. } => {
                self.expression_references_signals(expr, signals)
            }
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
                        std::mem::discriminant(&assign.rhs.kind)
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
        // CRITICAL FIX: Recursively collect targets from then branch (including nested If and Block)
        self.collect_targets_from_block(&if_stmt.then_block.statements, targets);

        // Collect targets from else branch (recursively for else-if chains)
        if let Some(else_block) = &if_stmt.else_block {
            self.collect_targets_from_block(&else_block.statements, targets);
        }
    }

    /// CRITICAL FIX: Recursively collect assignment targets from nested structures
    /// This handles cases where expanded array assignments are nested in Block statements
    fn collect_targets_from_block(
        &self,
        statements: &[Statement],
        targets: &mut std::collections::HashSet<String>,
    ) {
        for stmt in statements {
            match stmt {
                Statement::Assignment(assign) => {
                    // Skip array writes (handled separately)
                    if let LValue::BitSelect { index, .. } = &assign.lhs {
                        if self.evaluate_constant_expression(index).is_none() {
                            continue; // Dynamic array write, skip
                        }
                    }

                    let target = self.lvalue_to_string(&assign.lhs);
                    // Include all assignment targets (output ports can be driven by sequential logic)
                    targets.insert(target);
                }
                Statement::If(nested_if) => {
                    // Recurse into nested if statements
                    self.collect_assignment_targets(nested_if, targets);
                }
                Statement::Block(block) => {
                    // Recurse into nested blocks (CRITICAL for expanded array assignments)
                    self.collect_targets_from_block(&block.statements, targets);
                }
                _ => {}
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
                Statement::Block(block) => {
                    // CRITICAL FIX: Recurse into nested blocks (same bug as Bug #19)
                    // Expanded array assignments are wrapped in Block statements
                    println!(
                        "         📦 BLOCK found in branch for target={}, recursing...",
                        target
                    );
                    if let Some(result) =
                        self.process_branch_with_dependencies(&block.statements, target)
                    {
                        return Some(result);
                    }
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
        match &expr.kind {
            ExpressionKind::Literal(value) => self.create_literal_node(value),
            ExpressionKind::Ref(lvalue) => {
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
            ExpressionKind::Binary { op, left, right } => {
                // BUG #71 DEBUG: Check if operands are variable references to Concat
                if matches!(
                    op,
                    BinaryOp::FAdd | BinaryOp::FSub | BinaryOp::FMul | BinaryOp::FDiv
                ) {
                    eprintln!("[BUG #71 BINARY] FP Binary operation: op={:?}", op);
                    eprintln!(
                        "[BUG #71 BINARY]   left: {:?}",
                        std::mem::discriminant(&left.kind)
                    );
                    eprintln!(
                        "[BUG #71 BINARY]   right: {:?}",
                        std::mem::discriminant(&right.kind)
                    );
                    if let ExpressionKind::Ref(lval) = &left.kind {
                        eprintln!(
                            "[BUG #71 BINARY]   left is Ref: {:?}",
                            std::mem::discriminant(lval)
                        );
                    }
                    if let ExpressionKind::Ref(lval) = &right.kind {
                        eprintln!(
                            "[BUG #71 BINARY]   right is Ref: {:?}",
                            std::mem::discriminant(lval)
                        );
                    }
                }
                let left_node = self.create_expression_with_local_context(left, local_context);
                let right_node = self.create_expression_with_local_context(right, local_context);
                self.create_binary_op_node(op, left_node, right_node)
            }
            ExpressionKind::Unary { op, operand } => {
                let operand_node =
                    self.create_expression_with_local_context(operand, local_context);
                self.create_unary_op_node(op, operand_node)
            }
            ExpressionKind::Conditional {
                cond,
                then_expr,
                else_expr,
            } => {
                let cond_node = self.create_expression_with_local_context(cond, local_context);
                let then_node = self.create_expression_with_local_context(then_expr, local_context);
                let else_node = self.create_expression_with_local_context(else_expr, local_context);
                self.create_mux_node(cond_node, then_node, else_node)
            }
            ExpressionKind::Concat(exprs) => {
                eprintln!(
                    "[BUG #71 CONCAT] Creating Concat with {} parts",
                    exprs.len()
                );
                for (i, expr) in exprs.iter().enumerate() {
                    eprintln!(
                        "[BUG #71 CONCAT]   Part {}: {:?}",
                        i,
                        std::mem::discriminant(&expr.kind)
                    );
                }
                let part_nodes: Vec<usize> = exprs
                    .iter()
                    .map(|e| self.create_expression_with_local_context(e, local_context))
                    .collect();
                let concat_node = self.create_concat_node(part_nodes);
                eprintln!("[BUG #71 CONCAT] Created Concat node_{}", concat_node);
                concat_node
            }
            ExpressionKind::Replicate { count, value } => {
                let _count_node = self.create_expression_with_local_context(count, local_context);
                // For now, just return the value (replication logic would be more complex)
                self.create_expression_with_local_context(value, local_context)
            }
            ExpressionKind::FunctionCall { .. } => {
                // Fall back to original implementation for function calls
                self.create_expression_node(expr)
            }
            ExpressionKind::Cast { expr, .. } => {
                // Cast is a no-op for hardware generation (bitwise reinterpretation)
                // Just process the inner expression
                self.create_expression_with_local_context(expr, local_context)
            }
            // BUG FIX #85: Handle tuple field access for module synthesis
            ExpressionKind::TupleFieldAccess { base, index } => {
                // Check if the base is a Signal reference (module instance result)
                if let ExpressionKind::Ref(LValue::Signal(sig_id)) = &base.kind {
                    // First, check if this signal is in our tuple source mapping
                    // This handles _tuple_tmp signals that are assigned from module results
                    if let Some(inst_prefix) = self.tuple_source_map.get(sig_id) {
                        let target_signal_name = format!("{}_result_{}", inst_prefix, index);
                        println!("🔍🔍🔍 TUPLE_FIELD_ACCESS (via mapping): signal {} index={} -> target='{}'",
                                sig_id.0, index, target_signal_name);
                        return self.get_or_create_signal_driver(&target_signal_name);
                    }

                    // Look up the signal name for direct module result signals
                    if let Some(signal) = self.mir.signals.iter().find(|s| s.id == *sig_id) {
                        // Check if this is a module instance result signal (ends with _result_0)
                        if signal.name.ends_with("_result_0") || signal.name.contains("_inst_") {
                            // Derive the result_N signal name by replacing _result_0 with _result_N
                            // Signal naming convention: {func}_inst_{id}_result_{N}
                            let base_name = if let Some(pos) = signal.name.rfind("_result_") {
                                &signal.name[..pos]
                            } else {
                                // If no _result_ suffix, this is the first element - use base name
                                &signal.name[..]
                            };
                            let target_signal_name = format!("{}_result_{}", base_name, index);
                            println!("🔍🔍🔍 TUPLE_FIELD_ACCESS: base='{}' index={} -> target='{}'",
                                    signal.name, index, target_signal_name);

                            // Look up or create a reference to the target signal
                            return self.get_or_create_signal_driver(&target_signal_name);
                        }
                    }
                }

                // Fallback: For non-module results (actual concatenated tuples), use bit slicing
                let base_node = self.create_expression_with_local_context(base, local_context);
                // Create a slice to extract the specified element
                // For now, assume 32-bit elements and extract accordingly
                let element_width = 32;
                let low_bit = (*index) * element_width;
                let high_bit = low_bit + element_width - 1;
                // BUG FIX #92: create_slice_node expects (high, low) for HDL [high:low] notation
                self.create_slice_node(base_node, high_bit, low_bit)
            }
            ExpressionKind::FieldAccess { base, field } => {
                // For named field access, fall back to base expression
                // This would need type information to determine field offsets
                eprintln!("    ⚠️ FieldAccess on field '{}' - falling back to base", field);
                self.create_expression_with_local_context(base, local_context)
            }
        }
    }

    fn create_range_select_on_node(&mut self, lvalue: &LValue, base_node: usize) -> usize {
        match lvalue {
            LValue::RangeSelect { high, low, .. } => {
                // Create a range select node that operates on the computed base value
                let high_val = self.evaluate_const_expr(high);
                let low_val = self.evaluate_const_expr(low);
                // BUG FIX #92: create_slice_node expects (high, low) for HDL [high:low] notation
                self.create_slice_node(base_node, high_val, low_val)
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
        match &expr.kind {
            ExpressionKind::Literal(value) => match value {
                skalp_mir::Value::Integer(i) => *i as usize,
                skalp_mir::Value::BitVector { value, .. } => *value as usize,
                _ => 0,
            },
            _ => 0, // For non-constant expressions, return 0 as fallback
        }
    }

    /// BUG FIX #85: Create a range select node on a base node
    /// Used for expressions like data1[31:0] where we need to extract a bit range
    /// Note: create_slice_node expects (base, start=high, end=low) per HDL convention
    fn create_range_select_node_on_base(&mut self, base_node: usize, high: usize, low: usize) -> usize {
        self.create_slice_node(base_node, high, low)
    }

    /// BUG FIX #85: Create a bit select node on a base node
    /// Used for expressions like data1[5] where we need to extract a single bit
    fn create_bit_select_node_on_base(&mut self, base_node: usize, index: usize) -> usize {
        self.create_slice_node(base_node, index, index)
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
        match &expr.kind {
            ExpressionKind::Literal(value) => self.create_literal_node(value),
            ExpressionKind::Ref(lvalue) => {
                let signal_name = self.lvalue_to_string(lvalue);
                // Check if this signal has a computed value in the current context
                if let Some(&computed_value) = context.get(&signal_name) {
                    computed_value
                } else {
                    // Fall back to register reference
                    self.create_lvalue_ref_node(lvalue)
                }
            }
            ExpressionKind::Binary { op, left, right } => {
                let left_node = self.create_expression_node_with_context(left, context);
                let right_node = self.create_expression_node_with_context(right, context);
                self.create_binary_op_node(op, left_node, right_node)
            }
            ExpressionKind::Unary { op, operand } => {
                let operand_node = self.create_expression_node_with_context(operand, context);
                self.create_unary_op_node(op, operand_node)
            }
            ExpressionKind::Conditional {
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
            impl_style_hint: ImplStyleHint::default(),
            span: None,
        };

        self.sir.combinational_nodes.push(sir_node);
        let sir_type = self.get_signal_type(signal_name);
        self.sir.signals.push(SirSignal {
            name: format!("node_{}_out", node_id),
            width: sir_type.width(),
            sir_type,
            is_state: false,
            driver_node: Some(node_id),
            fanout_nodes: vec![],
            span: None,
        });

        node_id
    }

    fn convert_continuous_assign(&mut self, target: &str, value: &Expression) {
        // Get target signal width to propagate to expression tree
        let target_width = self.get_signal_width(target);
        println!(
            "🚀 CONVERT_CONTINUOUS_ASSIGN: target='{}', width={}, rhs_kind={:?}",
            target, target_width, std::mem::discriminant(&value.kind)
        );

        // Debug: print details for tuple_tmp and tuple-related assignments
        if target.contains("_tuple_tmp") || target.contains("valid") || target.contains("x1_") || target.contains("x2_") {
            println!("🔍🔍🔍 TUPLE-RELATED ASSIGNMENT: {} = {:?}", target, &value.kind);
            // Print more detail about Ref type
            if let ExpressionKind::Ref(lv) = &value.kind {
                println!("🔍🔍🔍   Ref LValue: {:?}", lv);
            }
        }

        let node_id = self.create_expression_node_with_width(value, Some(target_width));
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
            LValue::Signal(sig_id) => {
                let result = self
                    .mir
                    .signals
                    .iter()
                    .find(|s| s.id == *sig_id)
                    .map(|s| s.name.clone())
                    .unwrap_or_else(|| {
                        eprintln!(
                            "⚠️  Signal ID {} not found in MIR signals! Available signals:",
                            sig_id.0
                        );
                        for sig in &self.mir.signals {
                            eprintln!("     - Signal {}: {}", sig.id.0, sig.name);
                        }
                        format!("signal_{}", sig_id.0)
                    });
                result
            }
            LValue::Variable(var_id) => self
                .mir
                .variables
                .iter()
                .find(|v| v.id == *var_id)
                .map(|v| format!("{}_{}", v.name, var_id.0))  // BUG FIX #86: Unique name
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

    /// Extract bit width from a Type (BUG #76 FIX - full Option A)
    fn type_to_width(ty: &skalp_frontend::types::Type) -> Option<usize> {
        use skalp_frontend::types::{Type, Width};

        match ty {
            Type::Bit(Width::Fixed(w)) => Some(*w as usize),
            Type::Logic(Width::Fixed(w)) => Some(*w as usize),
            Type::Int(Width::Fixed(w)) => Some(*w as usize),
            Type::Nat(Width::Fixed(w)) => Some(*w as usize),
            Type::Bool => Some(1),
            Type::Tuple(elements) => {
                // Sum of all element widths
                let total: usize = elements.iter()
                    .filter_map(|t| Self::type_to_width(t))
                    .sum();
                if total > 0 { Some(total) } else { None }
            }
            Type::Array { element_type, size } => {
                Self::type_to_width(element_type).map(|w| w * (*size as usize))
            }
            _ => None,
        }
    }

    fn create_expression_node(&mut self, expr: &Expression) -> usize {
        // Call with no width hint
        self.create_expression_node_with_width(expr, None)
    }

    fn create_expression_node_with_width(
        &mut self,
        expr: &Expression,
        target_width: Option<usize>,
    ) -> usize {
        // BUG #76 FIX: Use expression's type information when target_width not provided
        let target_width = target_width.or_else(|| Self::type_to_width(&expr.ty));

        match &expr.kind {
            ExpressionKind::Literal(value) => self.create_literal_node_with_width(value, target_width),
            ExpressionKind::Ref(lvalue) => self.create_lvalue_ref_node(lvalue),
            ExpressionKind::Binary { op, left, right } => {
                // BUG #71 DEBUG: Check if operands contain Cast with Concat inside
                if matches!(
                    op,
                    BinaryOp::FAdd | BinaryOp::FSub | BinaryOp::FMul | BinaryOp::FDiv
                ) {
                    eprintln!("[BUG #71 BINARY NODE] FP Binary operation: op={:?}", op);
                    eprintln!(
                        "[BUG #71 BINARY NODE]   left: {:?}",
                        std::mem::discriminant(&left.kind)
                    );
                    eprintln!(
                        "[BUG #71 BINARY NODE]   right: {:?}",
                        std::mem::discriminant(&right.kind)
                    );

                    // Check if left is Cast wrapping a Ref to a variable
                    if let ExpressionKind::Cast { expr, target_type } = &left.kind {
                        if let ExpressionKind::Ref(LValue::Variable(var_id)) = &expr.kind {
                            eprintln!(
                                "[BUG #71 BINARY NODE]   left is Cast(Ref(Variable({:?}))) to {:?}",
                                var_id, target_type
                            );
                        }
                    }

                    // Check if right is Cast wrapping a Ref to a variable
                    if let ExpressionKind::Cast { expr, target_type } = &right.kind {
                        if let ExpressionKind::Ref(LValue::Variable(var_id)) = &expr.kind {
                            eprintln!("[BUG #71 BINARY NODE]   right is Cast(Ref(Variable({:?}))) to {:?}", var_id, target_type);
                        }
                    }
                }
                let left_node = self.create_expression_node(left);
                let right_node = self.create_expression_node(right);
                self.create_binary_op_node(op, left_node, right_node)
            }
            ExpressionKind::Unary { op, operand } => {
                let operand_node = self.create_expression_node(operand);
                self.create_unary_op_node(op, operand_node)
            }
            ExpressionKind::Conditional {
                cond,
                then_expr,
                else_expr,
            } => {
                eprintln!("⚠️ CONDITIONAL: target_width={:?}", target_width);
                let cond_node = self.create_expression_node(cond);
                // Propagate width hint to both branches
                let then_node = self.create_expression_node_with_width(then_expr, target_width);
                let else_node = self.create_expression_node_with_width(else_expr, target_width);
                self.create_mux_node(cond_node, then_node, else_node)
            }
            ExpressionKind::Concat(parts) => {
                eprintln!("🔍 [BUG #76] Converting Concat with type: {:?}", expr.ty);

                // Extract element widths from tuple type
                let part_widths: Vec<Option<usize>> = if let skalp_frontend::types::Type::Tuple(element_types) = &expr.ty {
                    element_types.iter().map(|t| Self::type_to_width(t)).collect()
                } else {
                    vec![None; parts.len()]
                };

                // Create nodes with proper widths
                let part_nodes: Vec<usize> = parts.iter().zip(part_widths.iter())
                    .enumerate()
                    .map(|(i, (p, width))| {
                        eprintln!("  Part {}: inferred_width={:?}, type={:?}", i, width, p.ty);
                        self.create_expression_node_with_width(p, *width)
                    })
                    .collect();

                eprintln!("  → Created {} SIR nodes for concat", part_nodes.len());
                self.create_concat_node_with_width(part_nodes, target_width)
            }
            ExpressionKind::Cast { expr, .. } => {
                // Cast is a no-op for hardware generation (bitwise reinterpretation)
                // Just process the inner expression
                self.create_expression_node_with_width(expr, target_width)
            }
            // BUG FIX #85: Handle tuple field access for module synthesis
            ExpressionKind::TupleFieldAccess { base, index } => {
                // Check if the base is a Signal reference (module instance result)
                if let ExpressionKind::Ref(LValue::Signal(sig_id)) = &base.kind {
                    // Look up the signal name
                    if let Some(signal) = self.mir.signals.iter().find(|s| s.id == *sig_id) {
                        println!("🔍🔍🔍 TUPLE_FIELD_ACCESS (node): checking base='{}' (id={}) index={}",
                                signal.name, sig_id.0, index);

                        // Check if this is a module instance result signal
                        if signal.name.ends_with("_result_0") || signal.name.contains("_inst_") {
                            let base_name = if let Some(pos) = signal.name.rfind("_result_") {
                                &signal.name[..pos]
                            } else {
                                &signal.name[..]
                            };
                            let target_signal_name = format!("{}_result_{}", base_name, index);
                            println!("🔍🔍🔍   -> Direct match: target='{}'", target_signal_name);
                            return self.get_or_create_signal_driver(&target_signal_name);
                        }

                        // BUG FIX #85: Check if this is a _tuple_tmp signal assigned from a module result
                        // Look at the assignment to this signal to find the source
                        if signal.name.contains("_tuple_tmp") {
                            println!("🔍🔍🔍   -> _tuple_tmp detected, scanning {} assignments", self.mir.assignments.len());
                            for assign in &self.mir.assignments {
                                if let LValue::Signal(lhs_id) = &assign.lhs {
                                    if lhs_id == sig_id {
                                        println!("🔍🔍🔍     Found assignment: RHS kind={:?}", std::mem::discriminant(&assign.rhs.kind));
                                        if let ExpressionKind::Ref(LValue::Signal(rhs_sig_id)) = &assign.rhs.kind {
                                            if let Some(rhs_signal) = self.mir.signals.iter().find(|s| s.id == *rhs_sig_id) {
                                                println!("🔍🔍🔍     RHS signal: '{}'", rhs_signal.name);
                                                if rhs_signal.name.contains("_result_") {
                                                    // Extract the base name (everything before _result_N)
                                                    if let Some(pos) = rhs_signal.name.rfind("_result_") {
                                                        let base_name = &rhs_signal.name[..pos];
                                                        let target_signal_name = format!("{}_result_{}", base_name, index);
                                                        println!("🔍🔍🔍   -> Via assignment: target='{}'", target_signal_name);
                                                        return self.get_or_create_signal_driver(&target_signal_name);
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                // Fallback: bit slicing
                println!("🔍🔍🔍   -> Fallback to bit slicing for index={}", index);
                let base_node = self.create_expression_node_with_width(base, None);
                let element_width = 32;
                let low_bit = (*index) * element_width;
                let high_bit = low_bit + element_width - 1;
                // BUG FIX #92: create_slice_node expects (high, low) for HDL [high:low] notation
                self.create_slice_node(base_node, high_bit, low_bit)
            }
            ExpressionKind::FieldAccess { base, field } => {
                // For named field access, fall back to base expression
                eprintln!("    ⚠️ FieldAccess on field '{}' - falling back to base", field);
                self.create_expression_node_with_width(base, target_width)
            }
            _ => {
                // For unsupported expressions, create a zero constant
                self.create_constant_node(0, 1)
            }
        }
    }

    fn create_literal_node(&mut self, value: &Value) -> usize {
        self.create_literal_node_with_width(value, None)
    }

    fn create_literal_node_with_width(&mut self, value: &Value, target_width: Option<usize>) -> usize {
        let (val, width) = match value {
            Value::Integer(i) => {
                // BUG #76 FIX: Use target width for integer literals when provided
                let inferred_width = target_width.unwrap_or(32);
                (*i as u64, inferred_width)
            }
            Value::BitVector { width, value } => (*value, *width),
            // BUG FIX: Handle float literals by converting to IEEE 754 bit representation
            Value::Float(f) => {
                // Convert f64 to IEEE 754 bits - we'll use f32 for now (32-bit floats)
                let f32_val = *f as f32;
                let bits = f32_val.to_bits();
                (bits as u64, 32)
            }
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
            sir_type: SirType::Bits(width),
            is_state: false,
            driver_node: Some(node_id),
            fanout_nodes: Vec::new(),
            span: None,
        });

        let node = SirNode {
            id: node_id,
            kind: SirNodeKind::Constant { value, width },
            inputs: vec![],
            outputs: vec![output_signal],
            clock_domain: None,
            impl_style_hint: ImplStyleHint::default(),
            span: None,
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
                // BUG FIX #86: Use unique signal names to prevent combinational cycles
                // from signal name collisions. Variables with the same name in different
                // scopes (e.g., "x" in multiple functions) must map to different signals.
                // Format: {name}_{id} ensures uniqueness while preserving readability.
                let var_name = self
                    .mir
                    .variables
                    .iter()
                    .find(|v| v.id == *var_id)
                    .map(|v| format!("{}_{}", v.name, var_id.0))  // Unique: name + id
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
                // BUG FIX #85: Check if base is a _tuple_tmp variable assigned from module result
                // If so, map the range select to the appropriate result signal
                if let LValue::Variable(var_id) = base.as_ref() {
                    // Get variable name
                    if let Some(var) = self.mir.variables.iter().find(|v| v.id == *var_id) {
                        let var_name = format!("{}_{}", var.name, var_id.0);
                        println!("🔍🔍🔍 RangeSelect on variable: {} (id={})", var_name, var_id.0);

                        // Check if this is a _tuple_tmp variable
                        if var.name.contains("_tuple_tmp") {
                            println!("🔍🔍🔍   Detected _tuple_tmp variable, searching for source assignment");

                            // Look for assignment to this VARIABLE (not signal) to find source
                            // MIR assignments can have LValue::Variable as LHS
                            for assign in &self.mir.assignments {
                                // Check if LHS is this variable
                                let is_this_var = match &assign.lhs {
                                    LValue::Variable(lhs_var_id) => *lhs_var_id == *var_id,
                                    _ => false,
                                };

                                if is_this_var {
                                    println!("🔍🔍🔍   Found assignment to variable {:?}", var_id);
                                    // Check if RHS is a signal reference
                                    if let ExpressionKind::Ref(LValue::Signal(rhs_sig_id)) = &assign.rhs.kind {
                                        if let Some(rhs_sig) = self.mir.signals.iter().find(|s| s.id == *rhs_sig_id) {
                                            println!("🔍🔍🔍   Assignment RHS is signal: {}", rhs_sig.name);
                                            if rhs_sig.name.contains("_result_") {
                                                // Extract base name and compute index from range
                                                let high_val = self.evaluate_constant_expression(high).unwrap_or(0) as usize;
                                                let low_val = self.evaluate_constant_expression(low).unwrap_or(0) as usize;

                                                // BUG FIX #92: Tuple elements are packed LSB-first
                                                // For a tuple (bool, f32, f32) with 32-bit elements:
                                                //   - result_0 at bits 31:0 (element 0 at LSB)
                                                //   - result_1 at bits 63:32
                                                //   - result_2 at bits 95:64 (element 2 at MSB)
                                                // Map bit range to element index: index = low_val / 32
                                                let index = low_val / 32;

                                                if let Some(pos) = rhs_sig.name.rfind("_result_") {
                                                    let base_name = &rhs_sig.name[..pos];
                                                    let target_signal_name = format!("{}_result_{}", base_name, index);
                                                    println!("🔍🔍🔍   -> Mapping range [{}:{}] to: '{}'", high_val, low_val, target_signal_name);
                                                    return self.get_or_create_signal_driver(&target_signal_name);
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                            println!("🔍🔍🔍   No matching assignment found for variable");
                        }
                    }
                }

                // Default: create a slice node
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

        // Determine type from operation and inputs
        let left_type = self.get_signal_type(&left_signal.signal_id);
        let right_type = self.get_signal_type(&right_signal.signal_id);

        // BUG DEBUG #65: Log signal names being looked up
        if bin_op.is_float_op() {
            eprintln!(
                "  🔍 Looking up types: left='{}', right='{}'",
                left_signal.signal_id, right_signal.signal_id
            );
            eprintln!(
                "  🔍 Type results: left_type={:?} (width={}), right_type={:?} (width={})",
                left_type,
                left_type.width(),
                right_type,
                right_type.width()
            );
            if left_type.width() > 256 || right_type.width() > 256 {
                eprintln!(
                    "  ❌ BUG #71: Signal with width > 256 found! left='{}' ({}), right='{}' ({})",
                    left_signal.signal_id,
                    left_type.width(),
                    right_signal.signal_id,
                    right_type.width()
                );
            }
        }

        let sir_type = if bin_op.is_float_op() {
            // BUG FIX #65: FP operations ALWAYS return float types
            // Use the float type from inputs, or infer from width
            if left_type.is_float() {
                left_type.clone()
            } else if right_type.is_float() {
                right_type.clone()
            } else {
                // Neither input is float type (they're Bits), but this is an FP operation
                // Infer float type from width: 16 -> Float16, 32 -> Float32, 64 -> Float64
                let width = left_type.width().max(right_type.width());
                match width {
                    16 => SirType::Float16,
                    32 => SirType::Float32,
                    64 => SirType::Float64,
                    _ => SirType::Float32, // Default to Float32
                }
            }
        } else if matches!(
            bin_op,
            BinaryOperation::Eq
                | BinaryOperation::Neq
                | BinaryOperation::Lt
                | BinaryOperation::Lte
                | BinaryOperation::Gt
                | BinaryOperation::Gte
                | BinaryOperation::FEq
                | BinaryOperation::FNeq
                | BinaryOperation::FLt
                | BinaryOperation::FLte
                | BinaryOperation::FGt
                | BinaryOperation::FGte
        ) {
            // Comparison operations return 1-bit boolean
            SirType::Bits(1)
        } else {
            // Arithmetic/logic operations use max width
            let width = left_type.width().max(right_type.width());
            SirType::Bits(width)
        };

        let width = sir_type.width();

        // BUG DEBUG #65: Check if FP operations have correct output width
        if bin_op.is_float_op() {
            eprintln!(
                "🔧 BUG #65 DEBUG: FP BinaryOp node_{} ({:?}): left_type={:?} (width={}), right_type={:?} (width={}), output_type={:?} (width={})",
                node_id,
                bin_op,
                left_type,
                left_type.width(),
                right_type,
                right_type.width(),
                sir_type,
                width
            );
        }

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
            sir_type,
            is_state: false,
            driver_node: Some(node_id),
            fanout_nodes: Vec::new(),
            span: None,
        });

        let node = SirNode {
            id: node_id,
            kind: SirNodeKind::BinaryOp(bin_op),
            inputs: vec![left_signal, right_signal],
            outputs: vec![output_signal],
            clock_domain: None,
            impl_style_hint: ImplStyleHint::default(),
            span: None,
        };

        self.sir.combinational_nodes.push(node);
        node_id
    }

    fn create_unary_op_node(&mut self, op: &skalp_mir::UnaryOp, operand: usize) -> usize {
        let node_id = self.next_node_id();
        let operand_signal = self.node_to_signal_ref(operand);

        // BUG FIX #87: For Negate operation, check operand type to use FNeg for floats
        // Without this, FP negation like fp_neg(b) would generate integer negation
        // instead of proper floating-point negation (as_type<uint>(-as_type<float>(...)))
        let unary_op = if matches!(op, skalp_mir::UnaryOp::Negate) {
            let operand_type = self.get_signal_type(&operand_signal.signal_id);
            if operand_type.is_float() {
                UnaryOperation::FNeg
            } else {
                self.convert_unary_op(op)
            }
        } else {
            self.convert_unary_op(op)
        };

        // Get type from operand - unary ops preserve type
        let sir_type = self.get_signal_type(&operand_signal.signal_id);
        let width = sir_type.width();

        // Create output signal for this node
        let output_signal_name = format!("node_{}_out", node_id);
        let output_signal = SignalRef {
            signal_id: output_signal_name.clone(),
            bit_range: None,
        };

        self.sir.signals.push(SirSignal {
            name: output_signal_name,
            width,
            sir_type,
            is_state: false,
            driver_node: Some(node_id),
            fanout_nodes: Vec::new(),
            span: None,
        });

        let node = SirNode {
            id: node_id,
            kind: SirNodeKind::UnaryOp(unary_op),
            inputs: vec![operand_signal],
            outputs: vec![output_signal],
            clock_domain: None,
            impl_style_hint: ImplStyleHint::default(),
            span: None,
        };

        self.sir.combinational_nodes.push(node);
        node_id
    }

    fn create_mux_node(&mut self, sel: usize, true_val: usize, false_val: usize) -> usize {
        let node_id = self.next_node_id();

        let sel_signal = self.node_to_signal_ref(sel);
        let true_signal = self.node_to_signal_ref(true_val);
        let false_signal = self.node_to_signal_ref(false_val);

        // Get type from true/false branches - use the wider type
        let true_type = self.get_signal_type(&true_signal.signal_id);
        let false_type = self.get_signal_type(&false_signal.signal_id);
        let true_width = true_type.width();
        let false_width = false_type.width();
        let sir_type = if true_width >= false_width {
            true_type
        } else {
            false_type
        };
        let width = sir_type.width();

        // BUG #125 FIX: Handle width mismatch in mux inputs
        // When one input is narrower than the other, check if it's a zero constant
        // and replace it with a properly-sized zero to avoid truncation issues
        let mut actual_true_signal = true_signal;
        let mut actual_false_signal = false_signal;

        if true_width != false_width {
            eprintln!("[BUG #125] Mux width mismatch: true={}, false={}, target={}",
                     true_width, false_width, width);

            // Check if the narrower input is a zero constant that needs widening
            if false_width < true_width {
                // false branch is narrower - check if it's a zero constant
                if let Some(zero_value) = self.is_zero_constant_node(false_val) {
                    eprintln!("[BUG #125] Replacing narrow zero constant (width={}) with wide zero (width={})",
                             false_width, width);
                    // Create a new zero constant with the correct width
                    let wide_zero = self.create_constant_node(zero_value, width);
                    actual_false_signal = self.node_to_signal_ref(wide_zero);
                }
            } else if true_width < false_width {
                // true branch is narrower - check if it's a zero constant
                if let Some(zero_value) = self.is_zero_constant_node(true_val) {
                    eprintln!("[BUG #125] Replacing narrow zero constant (width={}) with wide zero (width={})",
                             true_width, width);
                    // Create a new zero constant with the correct width
                    let wide_zero = self.create_constant_node(zero_value, width);
                    actual_true_signal = self.node_to_signal_ref(wide_zero);
                }
            }
        }

        // Create output signal for this node
        let output_signal_name = format!("node_{}_out", node_id);
        let output_signal = SignalRef {
            signal_id: output_signal_name.clone(),
            bit_range: None,
        };

        self.sir.signals.push(SirSignal {
            name: output_signal_name,
            width,
            sir_type,
            is_state: false,
            driver_node: Some(node_id),
            fanout_nodes: Vec::new(),
            span: None,
        });

        let node = SirNode {
            id: node_id,
            kind: SirNodeKind::Mux,
            inputs: vec![sel_signal, actual_true_signal, actual_false_signal],
            outputs: vec![output_signal],
            clock_domain: None,
            impl_style_hint: ImplStyleHint::default(),
            span: None,
        };

        self.sir.combinational_nodes.push(node);
        node_id
    }

    /// Check if a node is a zero constant and return its value
    fn is_zero_constant_node(&self, node_id: usize) -> Option<u64> {
        // Look through all combinational nodes to find this node
        for node in &self.sir.combinational_nodes {
            if node.id == node_id {
                if let SirNodeKind::Constant { value, .. } = &node.kind {
                    // Return the value if it's zero (or any constant actually,
                    // since we want to widen constants in general)
                    if *value == 0 {
                        return Some(*value);
                    }
                }
            }
        }
        None
    }

    fn create_concat_node(&mut self, parts: Vec<usize>) -> usize {
        self.create_concat_node_with_width(parts, None)
    }

    fn create_concat_node_with_width(
        &mut self,
        parts: Vec<usize>,
        target_width: Option<usize>,
    ) -> usize {
        let node_id = self.next_node_id();

        let part_signals: Vec<SignalRef> =
            parts.iter().map(|&p| self.node_to_signal_ref(p)).collect();

        // Calculate total width as sum of input widths
        eprintln!(
            "🔍 CONCAT DEBUG node_{}: {} parts",
            node_id,
            part_signals.len()
        );
        for (i, s) in part_signals.iter().enumerate() {
            let width = self.get_signal_width(&s.signal_id);
            eprintln!("  Part {}: signal='{}', width={}", i, s.signal_id, width);
        }

        let sum_width: usize = part_signals
            .iter()
            .map(|s| self.get_signal_width(&s.signal_id))
            .sum();

        // BUG FIX #92: Check if this is a tuple concat before we move part_signals
        // Tuple concats have multiple 32-bit elements and should NOT be sliced
        let is_tuple_concat = parts.len() >= 2 && part_signals.iter().all(|s| {
            let width = self.get_signal_width(&s.signal_id);
            width == 32 // All elements are 32-bit (typical tuple element width)
        });

        eprintln!("  Sum width: {}, is_tuple_concat: {}", sum_width, is_tuple_concat);

        // BUG FIX #49/#71e/#73: Handle target width mismatch and Metal width limitations
        // If target < sum: Create Concat with full width, then Slice to extract target bits
        // If target > sum: This is padding (for constants), create as target width
        // If target == sum: No adjustment needed
        // BUG FIX #73: If no target but sum > 256: Auto-slice to 256 bits for Metal compatibility
        let needs_slice = if let Some(t) = target_width {
            t < sum_width
        } else {
            // No explicit target width, but if sum > 256 bits, we need to decompose for Metal
            sum_width > 256
        };

        let concat_width = sum_width; // Concat always outputs full sum width

        if let Some(target) = target_width {
            if target != sum_width {
                eprintln!(
                    "🔧 CONCAT WIDTH FIX: node_{} sum={} → target={} (diff={}, needs_slice={})",
                    node_id,
                    sum_width,
                    target,
                    target as i64 - sum_width as i64,
                    needs_slice
                );
            }
        } else if sum_width > 256 {
            // BUG FIX #73: Auto-decomposition for Metal backend compatibility
            eprintln!(
                "🔧 BUG FIX #73: Auto-decompose {}-bit Concat (no target width, exceeds Metal 256-bit limit)",
                sum_width
            );
        }

        // Create output signal for the Concat node with its actual full width
        let concat_output_name = format!("node_{}_out", node_id);
        let concat_output = SignalRef {
            signal_id: concat_output_name.clone(),
            bit_range: None,
        };
        self.sir.signals.push(SirSignal {
            name: concat_output_name,
            width: concat_width,
            sir_type: SirType::Bits(concat_width),
            is_state: false,
            driver_node: Some(node_id),
            fanout_nodes: Vec::new(),
            span: None,
        });

        let node = SirNode {
            id: node_id,
            kind: SirNodeKind::Concat,
            inputs: part_signals,
            outputs: vec![concat_output],
            clock_domain: None,
            impl_style_hint: ImplStyleHint::default(),
            span: None,
        };

        self.sir.combinational_nodes.push(node);

        // BUG FIX #71e/#73: If target < sum, create a Slice to extract the target bits
        // BUG FIX #92: SKIP slicing for tuple concatenations - the target_width is from
        // the tuple TYPE (e.g., 65 bits for (bit, bit[32], bit[32])) but the actual concat
        // uses 32-bit signals for ALL elements (96 bits). Slicing would corrupt the layout.
        // TupleFieldAccess will extract elements correctly from the full concat.
        if needs_slice && !is_tuple_concat {
            if let Some(target) = target_width {
                // Explicit target width provided - slice to that width
                eprintln!(
                    "  ✂️ BUG FIX #71e: Creating Slice node to extract bits [0:{}] from {}-bit Concat",
                    target - 1,
                    concat_width
                );
                let slice_node = self.create_slice_node(node_id, target - 1, 0);
                return slice_node;
            } else {
                // BUG FIX #73: No target width, but concat > 256 bits
                // Don't create a slice (would lose data) - Metal backend will decompose
                eprintln!(
                    "  ⚠️ BUG FIX #73: {}-bit Concat created without target width - Metal backend will decompose",
                    concat_width
                );
                // Signal will be automatically decomposed by Metal codegen
            }
        }

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
            sir_type: SirType::Bits(output_width),
            is_state: false,
            driver_node: Some(node_id),
            fanout_nodes: Vec::new(),
            span: None,
        });

        let node = SirNode {
            id: node_id,
            kind: SirNodeKind::Slice { start, end },
            inputs: vec![base_signal],
            outputs: vec![output_signal],
            clock_domain: None,
            impl_style_hint: ImplStyleHint::default(),
            span: None,
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

        // Get the array signal to determine element width and type
        let array_type = self.get_signal_type(&array_signal.signal_id);
        let (element_type, element_width) = match &array_type {
            SirType::Array(elem_type, _size) => {
                let width = elem_type.width();
                ((**elem_type).clone(), width)
            }
            _ => {
                eprintln!(
                    "⚠️  WARNING: Array read from non-array type {:?}, defaulting to 8-bit",
                    array_type
                );
                (SirType::Bits(8), 8)
            }
        };

        let output_signal_name = format!("node_{}_out", node_id);
        let output_signal = SignalRef {
            signal_id: output_signal_name.clone(),
            bit_range: None,
        };
        self.sir.signals.push(SirSignal {
            name: output_signal_name,
            width: element_width,
            sir_type: element_type,
            is_state: false,
            driver_node: Some(node_id),
            fanout_nodes: Vec::new(),
            span: None,
        });

        let node = SirNode {
            id: node_id,
            kind: SirNodeKind::ArrayRead,
            inputs: vec![array_signal, index_signal],
            outputs: vec![output_signal],
            clock_domain: None,
            impl_style_hint: ImplStyleHint::default(),
            span: None,
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
        // Array write preserves the array type
        let sir_type = self.get_signal_type(&old_array_signal.signal_id);
        self.sir.signals.push(SirSignal {
            name: output_signal_name,
            width: array_width,
            sir_type,
            is_state: false,
            driver_node: Some(node_id),
            fanout_nodes: Vec::new(),
            span: None,
        });

        let node = SirNode {
            id: node_id,
            kind: SirNodeKind::ArrayWrite,
            inputs: vec![old_array_signal, index_signal, value_signal],
            outputs: vec![output_signal],
            clock_domain: None,
            impl_style_hint: ImplStyleHint::default(),
            span: None,
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

        // Get type from input signal - flip-flop preserves type
        let sir_type = self.get_signal_type(&input_signal.signal_id);
        let width = sir_type.width();

        // Create output signal for this flip-flop
        let output_signal_name = format!("node_{}_out", node_id);
        let output_signal = SignalRef {
            signal_id: output_signal_name.clone(),
            bit_range: None,
        };
        self.sir.signals.push(SirSignal {
            name: output_signal_name,
            width,
            sir_type,
            is_state: false, // This is just a temporary signal, not a state element
            driver_node: Some(node_id),
            fanout_nodes: Vec::new(),
            span: None,
        });

        let node = SirNode {
            id: node_id,
            kind: SirNodeKind::FlipFlop { clock_edge: edge },
            inputs: vec![clock_signal, input_signal],
            outputs: vec![output_signal],
            clock_domain: Some(clock.to_string()),
            impl_style_hint: ImplStyleHint::default(),
            span: None,
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
                sir_type: SirType::Bits(8),
                driver_node: Some(node_id),
                fanout_nodes: Vec::new(),
                is_state: false,
                span: None,
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

        // BUG FIX #8: Get the node's output width BEFORE modifying the signal
        // The node was created with the correct expression width (e.g., 96 bits for concat of 3x32-bit values)
        // But the signal may have been declared with the wrong width (e.g., 32 bits from variable type)
        let node_output_width = if let Some(node) = self
            .sir
            .combinational_nodes
            .iter()
            .chain(self.sir.sequential_nodes.iter())
            .find(|n| n.id == node_id)
        {
            eprintln!("   🔍 Node {} has {} outputs", node_id, node.outputs.len());
            // Get the width of the node's output signal (node_X_out)
            if !node.outputs.is_empty() {
                let output_signal_name = &node.outputs[0].signal_id;
                let width = self.get_signal_width(output_signal_name);
                eprintln!(
                    "   🔍 Node {} output signal '{}' has width {}",
                    node_id, output_signal_name, width
                );
                Some(width)
            } else {
                eprintln!("   ⚠️  Node {} has no outputs!", node_id);
                None
            }
        } else {
            eprintln!("   ❌ Node {} not found!", node_id);
            None
        };

        eprintln!("   🔍 node_output_width = {:?}", node_output_width);

        // Update signal to have this node as driver
        if let Some(signal) = self.sir.signals.iter_mut().find(|s| s.name == signal_name) {
            if let Some(existing_driver) = signal.driver_node {
                eprintln!(
                    "   ⚠️  WARNING: Signal '{}' already driven by node {} (NOT overwriting with node {})",
                    signal_name, existing_driver, node_id
                );
                // Don't overwrite - keep the first driver
                return;
            }

            // BUG FIX #8: Update signal width to match node output width if different
            if let Some(output_width) = node_output_width {
                if signal.width != output_width {
                    eprintln!(
                        "   🔧 BUG #8 FIX: Updating signal '{}' width: {} → {} bits (node {} output)",
                        signal_name, signal.width, output_width, node_id
                    );
                    signal.width = output_width;
                    // Also update the SirType to match the new width
                    signal.sir_type = SirType::Bits(output_width);

                    // BUG FIX: Also update StateElement width if this is a state element
                    // This fixes inconsistency where signal is updated to Bits(new_width)
                    // but StateElement remains at old width, causing Metal type mismatches
                    if let Some(state_elem) = self.sir.state_elements.get_mut(signal_name) {
                        state_elem.width = output_width;
                    }
                }
            }

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
                // Check if this signal is already an output of this node to prevent duplicates
                if !node.outputs.iter().any(|o| o.signal_id == signal_name) {
                    node.outputs.push(SignalRef {
                        signal_id: signal_name.to_string(),
                        bit_range: None,
                    });
                    eprintln!("   ✅ Added '{}' to node {} outputs", signal_name, node_id);
                } else {
                    eprintln!(
                        "   ⚠️  Signal '{}' already in node {} outputs (skipping duplicate)",
                        signal_name, node_id
                    );
                }
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

        // Get port type from MIR
        let sir_type = self
            .mir
            .ports
            .iter()
            .find(|p| p.name == port_name)
            .map(|p| self.convert_type(&p.port_type))
            .unwrap_or(SirType::Bits(8));
        let port_width = sir_type.width();

        // Add the output signal to SIR
        self.sir.signals.push(SirSignal {
            name: output_signal_name,
            width: port_width,
            sir_type,
            is_state: false,
            driver_node: Some(node_id),
            fanout_nodes: Vec::new(),
            span: None,
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
            impl_style_hint: ImplStyleHint::default(),
            span: None,
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

        // Check if this is a state element or signal to get type
        let sir_type = if let Some(signal) = self.sir.signals.iter().find(|s| s.name == name) {
            signal.sir_type.clone()
        } else {
            SirType::Bits(8) // Default to 8 bits
        };
        let width = sir_type.width();

        self.sir.signals.push(SirSignal {
            name: output_signal_name.clone(),
            width,
            sir_type,
            is_state: false,
            driver_node: Some(node_id),
            fanout_nodes: Vec::new(),
            span: None,
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
            impl_style_hint: ImplStyleHint::default(),
            span: None,
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
            FAdd => BinaryOperation::FAdd,
            FSub => BinaryOperation::FSub,
            FMul => BinaryOperation::FMul,
            FDiv => BinaryOperation::FDiv,
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
            FEqual => BinaryOperation::FEq,
            FNotEqual => BinaryOperation::FNeq,
            FLess => BinaryOperation::FLt,
            FLessEqual => BinaryOperation::FLte,
            FGreater => BinaryOperation::FGt,
            FGreaterEqual => BinaryOperation::FGte,
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
            FSqrt => UnaryOperation::FSqrt,   // FP square root intrinsic
            // BUG FIX #102: FNegate is explicitly FP negation (set during HIR->MIR conversion)
            FNegate => UnaryOperation::FNeg,
        }
    }

    #[allow(clippy::only_used_in_recursion)]
    /// Convert MIR DataType to SIR SirType
    fn convert_type(&self, data_type: &DataType) -> SirType {
        match data_type {
            DataType::Bit(w) | DataType::Logic(w) | DataType::Int(w) | DataType::Nat(w) => {
                SirType::Bits(*w)
            }
            DataType::Bool => SirType::Bits(1),
            DataType::Clock { .. } | DataType::Reset { .. } | DataType::Event => SirType::Bits(1),
            DataType::Float16 => SirType::Float16,
            DataType::Float32 => SirType::Float32,
            DataType::Float64 => SirType::Float64,
            DataType::Vec2(elem) => SirType::Vec2(Box::new(self.convert_type(elem))),
            DataType::Vec3(elem) => SirType::Vec3(Box::new(self.convert_type(elem))),
            DataType::Vec4(elem) => SirType::Vec4(Box::new(self.convert_type(elem))),
            DataType::Array(elem, size) => SirType::Array(Box::new(self.convert_type(elem)), *size),
            DataType::BitParam { default, .. }
            | DataType::LogicParam { default, .. }
            | DataType::IntParam { default, .. }
            | DataType::NatParam { default, .. }
            | DataType::BitExpr { default, .. }
            | DataType::LogicExpr { default, .. }
            | DataType::IntExpr { default, .. }
            | DataType::NatExpr { default, .. } => SirType::Bits(*default),
            // BUG FIX #65 & Metal Backend Fix: Struct types need proper type preservation
            DataType::Struct(struct_type) => {
                // BUG FIX: Metal Backend - Detect vec2/vec3/vec4 structs and preserve them as vector types
                // instead of flattening to Bits, so Metal can generate proper float2/float3/float4 types
                let struct_name_lower = struct_type.name.to_lowercase();

                // Check if this is a vec2/vec3/vec4 struct
                if (struct_name_lower == "vec2" || struct_name_lower == "vector2")
                    && struct_type.fields.len() >= 2
                {
                    // Get the element type from the first field (assume all fields have same type)
                    let elem_type = self.convert_type(&struct_type.fields[0].field_type);
                    eprintln!(
                        "🔧 Metal Backend Fix: Converting struct '{}' to SirType::Vec2({:?})",
                        struct_type.name, elem_type
                    );
                    return SirType::Vec2(Box::new(elem_type));
                } else if (struct_name_lower == "vec3" || struct_name_lower == "vector3")
                    && struct_type.fields.len() >= 3
                {
                    let elem_type = self.convert_type(&struct_type.fields[0].field_type);
                    eprintln!(
                        "🔧 Metal Backend Fix: Converting struct '{}' to SirType::Vec3({:?})",
                        struct_type.name, elem_type
                    );
                    return SirType::Vec3(Box::new(elem_type));
                } else if (struct_name_lower == "vec4" || struct_name_lower == "vector4")
                    && struct_type.fields.len() >= 4
                {
                    let elem_type = self.convert_type(&struct_type.fields[0].field_type);
                    eprintln!(
                        "🔧 Metal Backend Fix: Converting struct '{}' to SirType::Vec4({:?})",
                        struct_type.name, elem_type
                    );
                    return SirType::Vec4(Box::new(elem_type));
                }

                // BUG FIX #65: Other structs/tuples - calculate total width
                let total_width: usize = struct_type
                    .fields
                    .iter()
                    .map(|f| {
                        let field_sir_type = self.convert_type(&f.field_type);
                        field_sir_type.width()
                    })
                    .sum();

                eprintln!(
                    "🔧 BUG FIX #65: Converting struct '{}' with {} fields to Bits({})",
                    struct_type.name,
                    struct_type.fields.len(),
                    total_width
                );

                SirType::Bits(total_width)
            }
            // Enum, Union are not yet supported for simulation
            DataType::Enum(_) | DataType::Union(_) => {
                eprintln!("Warning: Enum/Union types not yet supported in SIR, treating as 1-bit");
                SirType::Bits(1)
            }
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

    fn get_signal_type(&self, signal_name: &str) -> SirType {
        // Check if it's a signal
        if let Some(signal) = self.sir.signals.iter().find(|s| s.name == signal_name) {
            return signal.sir_type.clone();
        }

        // Default to Bits based on width
        SirType::Bits(self.get_signal_width(signal_name))
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
                    eprintln!(
                        "❌ ERROR: Module {:?} not found for instance '{}'",
                        instance.module, instance.name
                    );
                    eprintln!("Available modules:");
                    for m in &self.mir_design.modules {
                        eprintln!("   - {:?}: {}", m.id, m.name);
                    }
                    let location = instance.span.as_ref()
                        .map(|s| format!(" at {}", s))
                        .unwrap_or_default();
                    panic!(
                        "Module {:?} not found for instance {}{}",
                        instance.module, instance.name, location
                    )
                });

            println!(
                "      ├─ Child module: {} (ID={:?})",
                child_module.name, child_module.id
            );
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
        // Call with no parent context (for top-level instances)
        self.elaborate_instance_with_context(instance, child_module, inst_prefix, None, "");
    }

    /// Elaborate a single instance with optional parent module context
    /// For nested instances, parent_module_for_signals contains the module
    /// that owns the SignalIds in instance.connections
    fn elaborate_instance_with_context(
        &mut self,
        instance: &skalp_mir::ModuleInstance,
        child_module: &Module,
        inst_prefix: &str,
        parent_module_for_signals: Option<&Module>,
        parent_prefix: &str,
    ) {
        println!(
            "      🔨 Elaborating instance '{}' (module has {} signals)",
            inst_prefix,
            child_module.signals.len()
        );
        if let Some(parent) = parent_module_for_signals {
            println!(
                "         Parent context: {} signals, prefix='{}'",
                parent.signals.len(),
                parent_prefix
            );
        }
        println!("         All signals in module:");
        for sig in &child_module.signals {
            println!("            - {}: {:?}", sig.name, sig.signal_type);
        }

        // Step 1: Create signals for child module's internal signals
        // These get prefixed with instance name (e.g., "stage1.reg")
        // IMPORTANT: We must flatten composite types (structs, arrays) into scalar signals
        for signal in &child_module.signals {
            let _prefixed_name = format!("{}.{}", inst_prefix, signal.name);

            // Check if this signal is a register in the child module
            // With the proper HIR→MIR fix, all array elements have their own assignments
            // so we can just check this specific signal
            let is_register = self.is_signal_sequential_in_module(signal.id, child_module);

            // Use TypeFlattener to handle composite types (structs, arrays, etc.)
            let mut flattener = skalp_mir::type_flattening::TypeFlattener::default();
            let (flattened_signals, _fields) = flattener.flatten_signal(
                &signal.name,
                &signal.signal_type,
                signal.initial.clone(),
                signal.clock_domain,
            );

            println!(
                "         ├─ Signal: {} (type={:?}) → {} flattened signals (is_reg={})",
                signal.name,
                signal.signal_type,
                flattened_signals.len(),
                is_register
            );

            // Create SIR signals for each flattened component
            for flat_signal in flattened_signals {
                let full_name = format!("{}.{}", inst_prefix, flat_signal.name);
                let sir_type = self.convert_type(&flat_signal.signal_type);
                let width = sir_type.width();

                println!("            ├─ Flattened: {} (width={})", full_name, width);

                self.sir.signals.push(SirSignal {
                    name: full_name.clone(),
                    width,
                    sir_type: sir_type.clone(),
                    driver_node: None,
                    fanout_nodes: Vec::new(),
                    is_state: is_register,
                    span: None,
                });

                if is_register {
                    eprintln!("            ✅ Adding to state_elements: {}", full_name);
                    self.sir.state_elements.insert(
                        full_name.clone(),
                        StateElement {
                            name: full_name,
                            width,
                            reset_value: None,
                            clock: String::new(),
                            reset: None,
                            span: None,
                        },
                    );
                }
            }
        }

        // Step 1b: Also create signals for child module's variables (let bindings)
        // Variables need to become signals so they can be referenced by nested instances
        for variable in &child_module.variables {
            let full_name = format!("{}.{}", inst_prefix, variable.name);
            let sir_type = self.convert_type(&variable.var_type);
            let width = sir_type.width();

            println!(
                "         ├─ Variable: {} (type={:?}) → SIR signal (width={})",
                variable.name, variable.var_type, width
            );

            self.sir.signals.push(SirSignal {
                name: full_name.clone(),
                width,
                sir_type: sir_type.clone(),
                driver_node: None,
                fanout_nodes: Vec::new(),
                is_state: false, // Variables are not registers
                span: None,
            });
        }

        // Step 1c: Create signals for INPUT ports of the child module
        // BUG #124 FIX: Nested instances (e.g., quadratic_solve inside exec_l4_l5) may reference
        // parent module ports (e.g., exec_l4_l5.param_1). These need to exist as actual SIR signals
        // so nested instances can read them. Without this, the signal lookup fails and returns 0.
        for port in &child_module.ports {
            if matches!(port.direction, skalp_mir::PortDirection::Input) {
                let full_name = format!("{}.{}", inst_prefix, port.name);
                let sir_type = self.convert_type(&port.port_type);
                let width = sir_type.width();

                println!(
                    "         ├─ Input Port: {} (type={:?}) → SIR signal (width={}) [BUG #124 FIX]",
                    port.name, port.port_type, width
                );

                // Check if signal already exists (avoid duplicates)
                if !self.sir.signals.iter().any(|s| s.name == full_name) {
                    self.sir.signals.push(SirSignal {
                        name: full_name.clone(),
                        width,
                        sir_type: sir_type.clone(),
                        driver_node: None,
                        fanout_nodes: Vec::new(),
                        is_state: false, // Input ports are not registers
                        span: None,
                    });
                }
            }
        }

        eprintln!(
            "         📊 After elaborating '{}': total {} state_elements",
            inst_prefix,
            self.sir.state_elements.len()
        );

        // BUG FIX #124: Store the basic port_mapping BEFORE elaborating nested instances
        // This ensures nested instances can look up the parent's port_mapping during their elaboration
        // The full port_mapping (with flattened fields) is built later in elaborate_child_logic_with_context,
        // but we need the basic mapping available for nested Port resolution now.
        {
            let mut basic_port_mapping: HashMap<String, Expression> = HashMap::new();
            for (port_name, parent_expr) in &instance.connections {
                basic_port_mapping.insert(port_name.clone(), parent_expr.clone());
            }
            println!("🔑🔑🔑 BUG #124 EARLY: Storing basic port_mapping for inst_prefix='{}' with {} entries BEFORE nested elaboration", inst_prefix, basic_port_mapping.len());
            self.instance_port_mappings.insert(inst_prefix.to_string(), basic_port_mapping);
        }

        // Step 2: FIRST recursively elaborate any instances within the child
        // BUG FIX #85: This MUST happen before Step 3 so that nested module outputs
        // have their drivers set up before we process TupleFieldAccess expressions
        // that reference those outputs
        // IMPORTANT: Pass child_module as the parent context for nested instances
        // The nested instances' connections have SignalIds from child_module
        if !child_module.instances.is_empty() {
            println!(
                "         └─ Recursively elaborating {} nested instances FIRST",
                child_module.instances.len()
            );
            let nested_prefix = format!("{}.", inst_prefix);
            // Pass child_module as the parent context and inst_prefix as the parent_prefix
            self.flatten_instances_for_module(
                child_module,
                &nested_prefix,
                Some(child_module),
                &format!("{}.", inst_prefix),
            );
        }

        // Step 3: Convert child module's logic with instance prefix
        // This includes both combinational assignments and sequential processes
        // NOW nested instances are elaborated, so their output signals have drivers
        eprintln!("💥💥💥 CALLING elaborate_child_logic_with_context for inst_prefix='{}', child_module='{}'", inst_prefix, child_module.name);
        self.elaborate_child_logic_with_context(
            child_module,
            inst_prefix,
            instance,
            parent_module_for_signals,
            parent_prefix,
        );
    }

    /// Elaborate child module's logic (assignments and processes) with instance prefix
    fn elaborate_child_logic(
        &mut self,
        child_module: &Module,
        inst_prefix: &str,
        instance: &skalp_mir::ModuleInstance,
    ) {
        // Call without parent context (for top-level instances)
        self.elaborate_child_logic_with_context(child_module, inst_prefix, instance, None, "");
    }

    /// Elaborate child module's logic with optional parent module context
    /// For nested instances, parent_module_for_signals contains the module
    /// that owns the SignalIds in instance.connections
    fn elaborate_child_logic_with_context(
        &mut self,
        child_module: &Module,
        inst_prefix: &str,
        instance: &skalp_mir::ModuleInstance,
        parent_module_for_signals: Option<&Module>,
        parent_prefix: &str,
    ) {
        // Create a mapping from child port names to parent expressions
        // This connects the instance ports to the parent's signals
        let mut port_mapping: HashMap<String, Expression> = HashMap::new();
        for (port_name, parent_expr) in &instance.connections {
            port_mapping.insert(port_name.clone(), parent_expr.clone());

            // CRITICAL FIX for Bug #8: If this port was flattened from a struct/array,
            // we need to also add mappings for all the flattened field ports.
            // Example: "wr_data" → write_vertex becomes:
            //   "wr_data_x" → write_vertex_x
            //   "wr_data_y" → write_vertex_y
            //   "wr_data_z" → write_vertex_z
            //
            // Find all child ports that start with port_name_ (flattened variants)
            let port_prefix = format!("{}_", port_name);
            eprintln!(
                "   🔎 Checking for flattened ports with prefix '{}'",
                port_prefix
            );
            for child_port in &child_module.ports {
                eprintln!("      Child port: '{}'", child_port.name);
                if child_port.name.starts_with(&port_prefix) {
                    eprintln!("      ✅ MATCHES PREFIX!");

                    // This is a flattened field port like "wr_data_x"
                    // Get the suffix (e.g., "x" from "wr_data_x")
                    let suffix = &child_port.name[port_prefix.len()..];

                    // Create corresponding parent signal name
                    // If parent_expr is a Ref to a signal/port, create Ref to signal_suffix
                    // Parent signals/ports are in self.mir (the parent module being elaborated)
                    eprintln!(
                        "         Suffix: '{}', parent_expr: {:?}",
                        suffix, parent_expr
                    );
                    let parent_flattened_expr = if let ExpressionKind::Ref(lval) = &parent_expr.kind {
                        eprintln!("         Parent is ExpressionKind::Ref");
                        if let LValue::Signal(parent_sig_id) = lval {
                            eprintln!("         Parent is LValue::Signal({:?})", parent_sig_id);
                            // Find parent signal in MIR module
                            let parent_sig_opt =
                                self.mir.signals.iter().find(|s| s.id == *parent_sig_id);
                            eprintln!(
                                "         Found parent signal: {:?}",
                                parent_sig_opt.map(|s| &s.name)
                            );

                            if let Some(parent_sig) = parent_sig_opt {
                                // CRITICAL: The parent might ALREADY be flattened (e.g., "wr_data_x")
                                // If so, we need to REPLACE the last field, not append
                                let parent_flattened_name = if let Some((
                                    base,
                                    idx,
                                    current_field,
                                )) =
                                    self.parse_flattened_name(&parent_sig.name)
                                {
                                    if !current_field.is_empty() {
                                        // Parent is already flattened - replace the field
                                        eprintln!("         Parent ALREADY flattened with base '{}', idx '{}', field '{}', replacing with '{}'", base, idx, current_field, suffix);
                                        // Reconstruct with new field
                                        if idx.is_empty() {
                                            format!("{}_{}", base, suffix)
                                        } else {
                                            format!("{}_{}_{}", base, idx, suffix)
                                        }
                                    } else {
                                        // Not flattened - append suffix
                                        format!("{}_{}", parent_sig.name, suffix)
                                    }
                                } else {
                                    // Not a flattened name - append suffix
                                    format!("{}_{}", parent_sig.name, suffix)
                                };

                                eprintln!(
                                    "         Looking for flattened: '{}'",
                                    parent_flattened_name
                                );
                                let parent_flattened_sig_opt = self
                                    .mir
                                    .signals
                                    .iter()
                                    .find(|s| s.name == parent_flattened_name);
                                eprintln!(
                                    "         Found flattened signal: {:?}",
                                    parent_flattened_sig_opt.map(|s| &s.name)
                                );

                                parent_flattened_sig_opt
                                    .map(|s| Expression::with_unknown_type(ExpressionKind::Ref(LValue::Signal(s.id))))
                            } else {
                                None
                            }
                        } else if let LValue::Port(parent_port_id) = lval {
                            eprintln!("         Parent is LValue::Port({:?})", parent_port_id);
                            // Find parent port in MIR module
                            let orig_port_opt =
                                self.mir.ports.iter().find(|p| p.id == *parent_port_id);

                            if let Some(orig_port) = orig_port_opt {
                                // Same fix as for signals - check if already flattened
                                let parent_flattened_name =
                                    if let Some((base, idx, current_field)) =
                                        self.parse_flattened_name(&orig_port.name)
                                    {
                                        if !current_field.is_empty() {
                                            // Port is already flattened - replace the field
                                            if idx.is_empty() {
                                                format!("{}_{}", base, suffix)
                                            } else {
                                                format!("{}_{}_{}", base, idx, suffix)
                                            }
                                        } else {
                                            format!("{}_{}", orig_port.name, suffix)
                                        }
                                    } else {
                                        format!("{}_{}", orig_port.name, suffix)
                                    };

                                let parent_flattened_port_opt = self
                                    .mir
                                    .ports
                                    .iter()
                                    .find(|p| p.name == parent_flattened_name);

                                parent_flattened_port_opt
                                    .map(|p| Expression::with_unknown_type(ExpressionKind::Ref(LValue::Port(p.id))))
                            } else {
                                None
                            }
                        } else {
                            None
                        }
                    } else {
                        None
                    };

                    if let Some(expr) = parent_flattened_expr {
                        eprintln!(
                            "   🔗 EXPANDING PORT MAPPING: {} → {:?}",
                            child_port.name, expr
                        );
                        port_mapping.insert(child_port.name.clone(), expr);
                    }
                }
            }
        }

        // BUG FIX #124: Store the port_mapping for this instance so nested instances can look it up
        // This enables recursive port resolution for 3-level nesting (e.g., CLE → exec_l4_l5 → quadratic_solve)
        println!("🔑🔑🔑 BUG #124: Storing port_mapping for inst_prefix='{}' with {} entries", inst_prefix, port_mapping.len());
        self.instance_port_mappings.insert(inst_prefix.to_string(), port_mapping.clone());

        // Convert combinational assignments from child module
        for assign in &child_module.assignments {
            self.convert_child_assignment_with_context(
                assign,
                inst_prefix,
                &port_mapping,
                child_module,
                parent_module_for_signals,
                parent_prefix,
            );
        }

        // Convert sequential processes from child module
        for process in &child_module.processes {
            self.convert_child_process_with_context(
                process,
                inst_prefix,
                &port_mapping,
                child_module,
                parent_module_for_signals,
                parent_prefix,
            );
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
        // Call without parent context
        self.convert_child_assignment_with_context(
            assign,
            inst_prefix,
            port_mapping,
            child_module,
            None,
            "",
        );
    }

    /// Convert child module assignment with optional parent module context
    fn convert_child_assignment_with_context(
        &mut self,
        assign: &skalp_mir::ContinuousAssign,
        inst_prefix: &str,
        port_mapping: &HashMap<String, Expression>,
        child_module: &Module,
        parent_module_for_signals: Option<&Module>,
        parent_prefix: &str,
    ) {
        println!("⚡⚡⚡ CONVERT_CHILD_ASSIGN_WITH_CTX: inst_prefix='{}', child_module='{}'", inst_prefix, child_module.name);
        // Translate LHS: if it's a port (output), map to parent signal
        // Otherwise prefix with instance name
        let lhs_signal = match &assign.lhs {
            LValue::Signal(sig_id) => {
                if let Some(signal) = child_module.signals.iter().find(|s| s.id == *sig_id) {
                    format!("{}.{}", inst_prefix, signal.name)
                } else if let Some(port) = child_module.ports.iter().find(|p| p.id.0 == sig_id.0) {
                    // Output port - this connects to parent signal
                    if let Some(parent_expr) = port_mapping.get(&port.name) {
                        // Use context-aware lookup for nested instances
                        self.get_signal_name_from_expression_with_context(
                            parent_expr,
                            parent_module_for_signals,
                            parent_prefix,
                        )
                    } else {
                        format!("{}.{}", inst_prefix, port.name)
                    }
                } else {
                    format!("{}.unknown", inst_prefix)
                }
            }
            LValue::Port(port_id) => {
                // First try direct ID lookup
                if let Some(port) = child_module.ports.iter().find(|p| p.id == *port_id) {
                    if let Some(parent_expr) = port_mapping.get(&port.name) {
                        // Use context-aware lookup for nested instances
                        self.get_signal_name_from_expression_with_context(
                            parent_expr,
                            parent_module_for_signals,
                            parent_prefix,
                        )
                    } else {
                        format!("{}.{}", inst_prefix, port.name)
                    }
                } else {
                    // BUG#25 FIX: Same as Bug#24 - port IDs renumbered during monomorphization
                    // Fall back to index-based lookup for continuous assignments to output ports
                    let port_index = port_id.0 as usize;
                    if port_index < child_module.ports.len() {
                        let port = &child_module.ports[port_index];
                        if let Some(parent_expr) = port_mapping.get(&port.name) {
                            // Use context-aware lookup for nested instances
                            self.get_signal_name_from_expression_with_context(
                                parent_expr,
                                parent_module_for_signals,
                                parent_prefix,
                            )
                        } else {
                            format!("{}.{}", inst_prefix, port.name)
                        }
                    } else {
                        format!("{}.unknown_port", inst_prefix)
                    }
                }
            }
            _ => {
                format!("{}.complex_lhs", inst_prefix)
            }
        };

        println!("            ├─ Assignment: {} = <expr>", lhs_signal);

        // Translate RHS expression with port mapping
        let node_id = self.node_counter;
        self.node_counter += 1;

        // Convert the RHS expression, substituting port references with parent signals
        // Use context-aware version for nested instances
        self.convert_expression_for_instance_with_context(
            &assign.rhs,
            inst_prefix,
            port_mapping,
            child_module,
            &lhs_signal,
            node_id,
            parent_module_for_signals,
            parent_prefix,
        );
    }

    /// Convert child process with parent module context
    /// For now, sequential processes don't usually write to output ports directly,
    /// so we delegate to the original function. Can be enhanced if needed.
    fn convert_child_process_with_context(
        &mut self,
        process: &skalp_mir::Process,
        inst_prefix: &str,
        port_mapping: &HashMap<String, Expression>,
        child_module: &Module,
        _parent_module_for_signals: Option<&Module>,
        _parent_prefix: &str,
    ) {
        // For now, delegate to the original implementation
        // Sequential processes typically don't write directly to output ports
        // If issues arise with nested sequential process signal resolution, enhance this
        self.convert_child_process(process, inst_prefix, port_mapping, child_module);
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
                    eprintln!(
                        "🕐 CLOCK MAPPING: Attempting to map clock_lvalue={:?} for instance '{}'",
                        clock_lvalue, inst_prefix
                    );
                    eprintln!(
                        "   port_mapping keys: {:?}",
                        port_mapping.keys().collect::<Vec<_>>()
                    );

                    let clock_signal = if let Some(sig_name) = self.get_signal_from_lvalue(
                        clock_lvalue,
                        inst_prefix,
                        port_mapping,
                        child_module,
                    ) {
                        eprintln!("   ✅ Mapped to: {}", sig_name);
                        sig_name
                    } else {
                        eprintln!("   ❌ MAPPING FAILED! Defaulting to 'clk'");
                        "clk".to_string()
                    };

                    // Process sequential block: collect targets, build MUX trees, create FlipFlops
                    // This mirrors how the regular (non-hierarchical) conversion works

                    // Collect all targets assigned in this sequential block
                    // CRITICAL: Use Vec instead of HashSet to preserve deterministic order
                    // HashSet iteration order is unpredictable, causing FlipFlops to connect
                    // to wrong data nodes (Bug #15)
                    let mut targets = Vec::new();
                    for statement in &process.body.statements {
                        self.collect_assignment_targets_for_instance(
                            statement,
                            inst_prefix,
                            port_mapping,
                            child_module,
                            &mut targets,
                        );
                    }

                    // Deduplicate while preserving order
                    targets.sort();
                    targets.dedup();

                    // For each target, synthesize conditional logic and create FlipFlop
                    // CRITICAL: If target is a base signal for a flattened array, we need to
                    // create FlipFlops for ALL flattened elements
                    for target in targets {
                        // Expand target if it's a flattened array base
                        let expanded_targets = self.expand_flattened_target(&target);

                        for actual_target in expanded_targets {
                            // Build combinational logic (including MUXes for conditionals)
                            let data_node = self.synthesize_sequential_logic_for_instance(
                                &process.body.statements,
                                inst_prefix,
                                port_mapping,
                                child_module,
                                &actual_target,
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
                                    signal_id: actual_target.clone(),
                                    bit_range: None,
                                }],
                                clock_domain: Some(clock_signal.clone()),
                                impl_style_hint: ImplStyleHint::default(),
                                span: None,
                            };

                            self.sir.sequential_nodes.push(ff_node);

                            // Mark the signal as having this driver
                            if let Some(sig) = self
                                .sir
                                .signals
                                .iter_mut()
                                .find(|s| s.name == actual_target)
                            {
                                sig.driver_node = Some(node_id);
                            }
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

    /// Expand a target signal if it's a base for flattened array elements
    /// Example: "input_fifo.mem" → ["input_fifo.mem_0_x", "input_fifo.mem_0_y", ...]
    fn expand_flattened_target(&self, target: &str) -> Vec<String> {
        // CRITICAL: First check if the target signal exists as-is
        // If it does, return just that signal without expansion
        // This prevents incorrectly expanding "wr_ptr" to match "wr_ptr_gray", "wr_ptr_gray_sync1", etc.
        if self.sir.signals.iter().any(|sig| sig.name == target) {
            return vec![target.to_string()];
        }

        // If signal doesn't exist, it might be a flattened array base (e.g., "mem")
        // Check if there are flattened array elements: <target>_<digit> or <target>_<digit>_<field>
        let prefix = format!("{}_", target);
        let mut expanded: Vec<String> = self
            .sir
            .signals
            .iter()
            .filter(|sig| {
                if !sig.name.starts_with(&prefix) {
                    return false;
                }

                // Verify it's an array element pattern: _<digit>_<field> or _<digit>
                let suffix = &sig.name[prefix.len()..];

                // Check for pattern: <digit>_<field> or just <digit>
                if let Some(underscore_pos) = suffix.find('_') {
                    // Pattern: <digit>_<field>
                    let index_part = &suffix[..underscore_pos];
                    index_part.chars().all(|c| c.is_ascii_digit())
                } else {
                    // Pattern: just <digit>
                    suffix.chars().all(|c| c.is_ascii_digit())
                }
            })
            .map(|sig| sig.name.clone())
            .collect();

        if expanded.is_empty() {
            // Not a flattened array base and signal doesn't exist - return as-is
            // This might be an error case, but let the caller handle it
            vec![target.to_string()]
        } else {
            // Sort to ensure consistent ordering (mem_0_x, mem_0_y, mem_0_z, mem_1_x, ...)
            expanded.sort();
            expanded
        }
    }

    /// Extract base signal name from LValue (handles arrays, bit/range selection)
    /// For flattened arrays, this strips the index suffix to get the true base
    /// Example: mem_0_x -> mem, mem_5_z -> mem
    fn extract_base_signal_for_instance(
        &self,
        lvalue: &LValue,
        inst_prefix: &str,
        child_module: &Module,
    ) -> Option<String> {
        match lvalue {
            LValue::Signal(sig_id) => {
                // Direct signal reference
                child_module
                    .signals
                    .iter()
                    .find(|s| s.id == *sig_id)
                    .map(|signal| format!("{}.{}", inst_prefix, signal.name))
            }
            LValue::BitSelect { base, index: _ } => {
                // Array/bit index: extract base signal and strip flattened index suffix
                // e.g., mem[wr_ptr] where base resolves to "mem_0_x" -> we want just "mem"
                self.extract_base_signal_for_instance(base, inst_prefix, child_module)
                    .map(|base_name| self.strip_flattened_index_suffix(&base_name))
            }
            LValue::RangeSelect {
                base,
                high: _,
                low: _,
            } => {
                // Bit range slice: extract base signal
                // e.g., data[7:0] -> base is "data"
                self.extract_base_signal_for_instance(base, inst_prefix, child_module)
            }
            _ => None,
        }
    }

    /// Strip flattened array index suffix from a signal name
    /// Examples:
    ///   "input_fifo.mem_0_x" -> "input_fifo.mem"
    ///   "input_fifo.mem_7_z" -> "input_fifo.mem"
    ///   "input_fifo.wr_ptr" -> "input_fifo.wr_ptr" (unchanged)
    fn strip_flattened_index_suffix(&self, name: &str) -> String {
        // Try to match pattern: <base>_<digit>_<field> or <base>_<digit>
        // We need to be careful not to strip legitimate signal names like "wr_ptr_gray"

        // Look for the pattern: "_<digit>_<letter>" or "_<digit>" at the end
        if let Some(last_underscore_pos) = name.rfind('_') {
            let after_underscore = &name[last_underscore_pos + 1..];

            // Check if it's a single letter (struct field) or empty
            if after_underscore.len() <= 1 && after_underscore.chars().all(|c| c.is_alphabetic()) {
                // This might be "_x", "_y", "_z" - check for preceding digit
                if let Some(prev_underscore_pos) = name[..last_underscore_pos].rfind('_') {
                    let between = &name[prev_underscore_pos + 1..last_underscore_pos];
                    if between.chars().all(|c| c.is_ascii_digit()) {
                        // Found pattern "_<digit>_<letter>" - strip both
                        return name[..prev_underscore_pos].to_string();
                    }
                }
            } else if after_underscore.chars().all(|c| c.is_ascii_digit()) {
                // Pattern: "_<digit>" at the end - strip it
                return name[..last_underscore_pos].to_string();
            }
        }

        // No flattened index pattern found - return as-is
        name.to_string()
    }

    /// Extract element index from flattened array element name
    /// e.g., "input_fifo.mem_3_x" → Some(3), "input_fifo.mem_5" → Some(5)
    /// Returns None if no index pattern found
    fn extract_element_index(&self, name: &str) -> Option<usize> {
        // Look for pattern: <base>_<digit>_<field> or <base>_<digit>
        if let Some(last_underscore_pos) = name.rfind('_') {
            let after_underscore = &name[last_underscore_pos + 1..];

            // Check if it's a single letter (struct field)
            if after_underscore.len() == 1 && after_underscore.chars().all(|c| c.is_alphabetic()) {
                // Pattern: "_<digit>_<letter>" - check for preceding digit
                if let Some(prev_underscore_pos) = name[..last_underscore_pos].rfind('_') {
                    let between = &name[prev_underscore_pos + 1..last_underscore_pos];
                    if let Ok(idx) = between.parse::<usize>() {
                        return Some(idx);
                    }
                }
            } else if let Ok(idx) = after_underscore.parse::<usize>() {
                // Pattern: "_<digit>" at the end
                return Some(idx);
            }
        }

        None
    }

    /// Collect assignment targets in a sequential block for an instance
    #[allow(clippy::only_used_in_recursion)]
    fn collect_assignment_targets_for_instance(
        &self,
        statement: &Statement,
        inst_prefix: &str,
        port_mapping: &HashMap<String, Expression>,
        child_module: &Module,
        targets: &mut Vec<String>,
    ) {
        match statement {
            Statement::Assignment(assign) => {
                // With the proper HIR→MIR fix, each flattened element has its own assignment
                // So we should collect the ACTUAL signal name, not the stripped base
                let lhs_signal = match &assign.lhs {
                    LValue::Signal(sig_id) => child_module
                        .signals
                        .iter()
                        .find(|s| s.id == *sig_id)
                        .map(|signal| format!("{}.{}", inst_prefix, signal.name)),
                    LValue::Port(port_id) => child_module
                        .ports
                        .iter()
                        .find(|p| p.id == *port_id)
                        .map(|port| format!("{}.{}", inst_prefix, port.name)),
                    _ => None,
                };

                if let Some(sig_name) = lhs_signal {
                    targets.push(sig_name);
                }
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
            Statement::Block(block) => {
                // Handle blocks (created by expanded assignments)
                for stmt in &block.statements {
                    self.collect_assignment_targets_for_instance(
                        stmt,
                        inst_prefix,
                        port_mapping,
                        child_module,
                        targets,
                    );
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
                        LValue::BitSelect { base, index } => {
                            // Array assignment: mem[index_expr] <= value_expr
                            // For flattened target like "input_fifo.mem_3_x", we need to create:
                            // if index_expr == 3 then value_expr else mem_3_x

                            // Extract the base signal name
                            if let Some(base_name) = self.extract_base_signal_for_instance(
                                base,
                                inst_prefix,
                                child_module,
                            ) {
                                let stripped_base = self.strip_flattened_index_suffix(&base_name);
                                let stripped_target = self.strip_flattened_index_suffix(target);

                                if stripped_base == stripped_target {
                                    // This array assignment is for our target
                                    // Extract the element index from the flattened name
                                    // e.g., "input_fifo.mem_3_x" → 3
                                    if let Some(element_idx) = self.extract_element_index(target) {
                                        eprintln!(
                                            "🔍 BUG#26 FIX: Array assignment {}[index] <= value, target={}, element_idx={}",
                                            stripped_base, target, element_idx
                                        );

                                        // Create condition: index_expr == element_idx
                                        let index_node = self.create_expression_node_for_instance(
                                            index,
                                            inst_prefix,
                                            port_mapping,
                                            child_module,
                                        );
                                        let const_idx =
                                            self.create_constant_node(element_idx as u64, 32);
                                        eprintln!(
                                            "   🔢 Created constant node {} with value {} for target={}",
                                            const_idx, element_idx, target
                                        );
                                        let cond_node = self.create_binary_op_node(
                                            &BinaryOp::Equal,
                                            index_node,
                                            const_idx,
                                        );

                                        // Create value node (RHS)
                                        let value_node = self.create_expression_node_for_instance(
                                            &assign.rhs,
                                            inst_prefix,
                                            port_mapping,
                                            child_module,
                                        );

                                        // Create current value node (keep current value if condition false)
                                        let current_node = self.create_signal_ref(target);

                                        // Create MUX: cond ? value : current
                                        return self.create_mux_node(
                                            cond_node,
                                            value_node,
                                            current_node,
                                        );
                                    }
                                }
                            }
                            continue;
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
                    // BUG #34 FIX: After HIR→MIR expansion, each flattened element (mem_0_x, mem_1_x, etc.)
                    // has its own assignment with a unique RHS expression (containing the correct index literal).
                    // We must match the EXACT signal name, not the stripped base, otherwise all targets
                    // will match the first assignment and use the wrong constant!

                    let lhs_signal = match &assign.lhs {
                        LValue::Signal(sig_id) => {
                            // Signal assignment - use EXACT name, don't strip!
                            // Each flattened element has its own assignment in MIR
                            child_module
                                .signals
                                .iter()
                                .find(|s| s.id == *sig_id)
                                .map(|signal| format!("{}.{}", inst_prefix, signal.name))
                        }
                        LValue::BitSelect { base, .. } => {
                            // Array index like mem[wr_ptr] - strip to match flattened targets
                            // This case shouldn't happen after HIR→MIR expansion, but keep for safety
                            let extracted = self.extract_base_signal_for_instance(
                                base,
                                inst_prefix,
                                child_module,
                            );
                            extracted
                                .as_ref()
                                .map(|base_name| self.strip_flattened_index_suffix(base_name))
                        }
                        _ => None,
                    };

                    if let Some(lhs) = lhs_signal {
                        // For Signal assignments: exact match (fifo.mem_0_x == fifo.mem_0_x)
                        // For BitSelect assignments: compare stripped names
                        let matches = if matches!(assign.lhs, LValue::Signal(_)) {
                            lhs == target
                        } else {
                            let target_stripped = self.strip_flattened_index_suffix(target);
                            lhs == target_stripped
                        };

                        if matches {
                            // Found assignment for this target!
                            return Some(self.create_expression_node_for_instance(
                                &assign.rhs,
                                inst_prefix,
                                port_mapping,
                                child_module,
                            ));
                        }
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
                Statement::Block(block) => {
                    // Recursively search within the block
                    if let Some(result) = self.find_assignment_in_branch_for_instance(
                        &block.statements,
                        inst_prefix,
                        port_mapping,
                        child_module,
                        target,
                    ) {
                        return Some(result);
                    }
                }
                _ => {}
            }
        }
        None
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
        _node_id: usize,
    ) {
        // Build the full expression tree recursively with port mapping (no parent context)
        self.convert_expression_for_instance_with_context(
            expr,
            inst_prefix,
            port_mapping,
            child_module,
            output_signal,
            _node_id,
            None,
            "",
        );
    }

    /// Convert expression for instance with optional parent module context
    fn convert_expression_for_instance_with_context(
        &mut self,
        expr: &Expression,
        inst_prefix: &str,
        port_mapping: &HashMap<String, Expression>,
        child_module: &Module,
        output_signal: &str,
        _node_id: usize,
        parent_module_for_signals: Option<&Module>,
        parent_prefix: &str,
    ) {
        println!("🔥🔥🔥 CONVERT_EXPR_FOR_INST_WITH_CTX: output_signal='{}', inst_prefix='{}', expr.kind={:?}", output_signal, inst_prefix, std::mem::discriminant(&expr.kind));
        // Build the full expression tree recursively with port mapping
        let result_node = self.create_expression_node_for_instance_with_context(
            expr,
            inst_prefix,
            port_mapping,
            child_module,
            parent_module_for_signals,
            parent_prefix,
        );
        // Connect the result node's output to the target signal
        self.connect_node_to_signal(result_node, output_signal);
    }

    /// Create expression node with instance context (handles port mapping)
    fn create_expression_node_for_instance(
        &mut self,
        expr: &Expression,
        inst_prefix: &str,
        port_mapping: &HashMap<String, Expression>,
        child_module: &Module,
    ) -> usize {
        // Call without parent context
        self.create_expression_node_for_instance_with_context(
            expr,
            inst_prefix,
            port_mapping,
            child_module,
            None,
            "",
        )
    }

    /// Create expression node with instance context and optional parent module context
    fn create_expression_node_for_instance_with_context(
        &mut self,
        expr: &Expression,
        inst_prefix: &str,
        port_mapping: &HashMap<String, Expression>,
        child_module: &Module,
        parent_module_for_signals: Option<&Module>,
        parent_prefix: &str,
    ) -> usize {
        println!("🎨🎨🎨 CREATE_EXPR_NODE_FOR_INSTANCE: expr.kind={:?}, inst_prefix='{}'", std::mem::discriminant(&expr.kind), inst_prefix);
        let result = match &expr.kind {
            ExpressionKind::Literal(value) => self.create_literal_node(value),
            ExpressionKind::Ref(lvalue) => {
                println!("🎨🎨🎨   -> ExpressionKind::Ref({:?})", lvalue);

                // BUG FIX #85: Handle RangeSelect and BitSelect specially
                // For these, we need to create a node that selects bits from the base signal
                match lvalue {
                    LValue::RangeSelect { base, high, low } => {
                        println!("🎨🎨🎨   -> RangeSelect on base {:?} [high:low]", base);
                        // First, get the base signal node
                        let base_expr = Expression::with_unknown_type(ExpressionKind::Ref((**base).clone()));
                        let base_node = self.create_expression_node_for_instance_with_context(
                            &base_expr,
                            inst_prefix,
                            port_mapping,
                            child_module,
                            parent_module_for_signals,
                            parent_prefix,
                        );
                        // Now create a range select node on top
                        let high_val = self.evaluate_const_expr(high);
                        let low_val = self.evaluate_const_expr(low);
                        println!("🎨🎨🎨   -> Creating RangeSelect node [{}:{}] on base_node={}", high_val, low_val, base_node);
                        self.create_range_select_node_on_base(base_node, high_val as usize, low_val as usize)
                    }
                    LValue::BitSelect { base, index } => {
                        println!("🎨🎨🎨   -> BitSelect on base {:?} [index]", base);
                        // First, get the base signal node
                        let base_expr = Expression::with_unknown_type(ExpressionKind::Ref((**base).clone()));
                        let base_node = self.create_expression_node_for_instance_with_context(
                            &base_expr,
                            inst_prefix,
                            port_mapping,
                            child_module,
                            parent_module_for_signals,
                            parent_prefix,
                        );
                        // Now create a bit select node on top
                        let index_val = self.evaluate_const_expr(index);
                        println!("🎨🎨🎨   -> Creating BitSelect node [{}] on base_node={}", index_val, base_node);
                        self.create_bit_select_node_on_base(base_node, index_val as usize)
                    }
                    // BUG FIX #113: Handle Port specially to preserve RangeSelect in port_mapping
                    // When a port maps to a RangeSelect expression (e.g., param_0 -> data1[31:0]),
                    // we need to recursively create a node for that expression, not just extract
                    // the base signal name.
                    LValue::Port(port_id) => {
                        println!("🎨🎨🎨   -> LValue::Port({}), checking port_mapping", port_id.0);

                        // Look up the port name in child_module
                        let port_name = if let Some(port) = child_module.ports.iter().find(|p| p.id == *port_id) {
                            Some(port.name.clone())
                        } else {
                            // Try by index (BUG #24 fallback)
                            let port_index = port_id.0 as usize;
                            if port_index < child_module.ports.len() {
                                Some(child_module.ports[port_index].name.clone())
                            } else {
                                None
                            }
                        };

                        if let Some(ref name) = port_name {
                            println!("🎨🎨🎨   -> Port name: '{}', looking in port_mapping", name);
                            if let Some(parent_expr) = port_mapping.get(name) {
                                println!("🎨🎨🎨   -> Found mapping for '{}': {:?}", name, std::mem::discriminant(&parent_expr.kind));
                                // BUG #113: Recursively create node for the mapped expression
                                // This properly handles RangeSelect in the mapped expression
                                // IMPORTANT: The mapped expression is in PARENT context.
                                // CRITICAL: Pass parent_module_for_signals as child_module since the
                                // expression contains Port references from the PARENT module, not the child.
                                // BUG #124 FIX: Look up the PARENT's port_mapping from instance_port_mappings
                                // so we can resolve parent ports (like exec_l4_l5.param_1) to grandparent signals (like CLE.data1).
                                // Handle key format: parent_prefix may have trailing dot (e.g., "exec_l4_l5_inst_233.")
                                // but we store without trailing dot (e.g., "exec_l4_l5_inst_233")
                                let parent_key = parent_prefix.trim_end_matches('.');
                                let parent_port_mapping = self.instance_port_mappings.get(parent_key).cloned().unwrap_or_default();
                                if let Some(parent_module) = parent_module_for_signals {
                                    println!("🎨🎨🎨   -> Recursing with parent_module='{}' as child_module, parent_prefix='{}', parent_port_mapping.len()={}", parent_module.name, parent_prefix, parent_port_mapping.len());
                                    // BUG #113 FIX: When recursing to resolve parent expressions, we need to
                                    // provide proper context for nested modules. If the parent module's ports
                                    // reference the grandparent (top-level module), we need to provide that context.
                                    // Use self.mir as the grandparent context since it's the top-level module.
                                    return self.create_expression_node_for_instance_with_context(
                                        parent_expr,
                                        parent_prefix,  // Use parent prefix for parent context
                                        &parent_port_mapping,  // BUG #124: Use parent's port_mapping for proper resolution
                                        parent_module,   // BUG #113: Use parent module since Port refs are from parent
                                        Some(&self.mir.clone()),  // Use top-level module as grandparent context
                                        "",              // Top-level has no prefix
                                    );
                                } else {
                                    // No parent module - we're at top level, look up in self.mir
                                    // BUG #124: At top level, there's no parent prefix so no stored mapping needed
                                    let top_level_mapping = HashMap::new();
                                    println!("🎨🎨🎨   -> No parent_module, using self.mir for context");
                                    return self.create_expression_node_for_instance_with_context(
                                        parent_expr,
                                        "",  // No instance prefix at top level
                                        &top_level_mapping,
                                        &self.mir.clone(),  // Use top-level module
                                        None,
                                        "",
                                    );
                                }
                            }
                        }

                        // BUG #113 FIX: If port_mapping is empty, we're in parent context after recursion.
                        // The key insight is that after recursion, the inst_prefix refers to the PARENT's
                        // instance, not the current child_module. So we should look up ports in
                        // parent_module_for_signals FIRST, then self.mir, then child_module as last resort.
                        if port_mapping.is_empty() {
                            println!("🎨🎨🎨   -> Empty port_mapping, looking up port {} (inst_prefix='{}', parent_prefix='{}')",
                                     port_id.0, inst_prefix, parent_prefix);

                            // BUG #114 FIX: We already resolved the port_name from child_module above.
                            // If we have a port_name, use it directly to look up the signal!
                            // Don't try to re-lookup by port_id in parent_module/self.mir since those
                            // have different port IDs. The port_name is the key.
                            if let Some(ref name) = port_name {
                                // BUG #118 FIX: ONLY use top-level signals when we're ACTUALLY at top level
                                // (both prefixes empty). For nested instances, we must NOT fall back to
                                // top-level signals, as this creates wrong connections.
                                if inst_prefix.is_empty() && parent_prefix.is_empty() {
                                    // Try to find this port in self.mir (top-level) by NAME
                                    if let Some(mir_port) = self.mir.ports.iter().find(|p| p.name == *name) {
                                        // Found in top-level - use the signal directly (no prefix)
                                        println!("🎨🎨🎨   -> BUG #118 FIX: At TOP LEVEL, found port '{}' by NAME in self.mir, using signal directly", name);
                                        return self.get_or_create_signal_driver(&mir_port.name);
                                    }
                                    // Try to find as a signal in self.mir by name
                                    if let Some(_mir_signal) = self.mir.signals.iter().find(|s| s.name == *name) {
                                        println!("🎨🎨🎨   -> BUG #118 FIX: At TOP LEVEL, found signal '{}' by NAME in self.mir, using it directly", name);
                                        return self.get_or_create_signal_driver(name);
                                    }
                                    // At top-level, just try the name directly
                                    println!("🎨🎨🎨   -> BUG #118 FIX: At TOP LEVEL, using port name '{}' directly", name);
                                    return self.get_or_create_signal_driver(name);
                                }
                                // For nested instances, continue to the parent_module lookup below
                                println!("🎨🎨🎨   -> BUG #118 FIX: In NESTED context (inst='{}', parent='{}'), skipping top-level lookup for '{}'",
                                         inst_prefix, parent_prefix, name);
                            }

                            // BUG #113 FIX: After recursion, inst_prefix is from the PARENT context.
                            // The Port reference is from child_module, but we need to look it up using
                            // parent context. Try parent_module_for_signals FIRST.
                            if let Some(parent_module) = parent_module_for_signals {
                                println!("🎨🎨🎨   -> Trying parent_module '{}' first", parent_module.name);
                                if let Some(port) = parent_module.ports.iter().find(|p| p.id == *port_id) {
                                    let sig_name = format!("{}{}", parent_prefix, port.name);
                                    println!("🎨🎨🎨   -> Found port '{}' in parent_module, signal='{}'", port.name, sig_name);
                                    return self.get_or_create_signal_driver(&sig_name);
                                }
                                // BUG #114 FIX: Also try by NAME in parent_module
                                if let Some(ref name) = port_name {
                                    if let Some(port) = parent_module.ports.iter().find(|p| p.name == *name) {
                                        let sig_name = if parent_prefix.is_empty() {
                                            port.name.clone()
                                        } else {
                                            format!("{}{}", parent_prefix, port.name)
                                        };
                                        println!("🎨🎨🎨   -> BUG #114 FIX: Found port '{}' by NAME in parent_module, signal='{}'", port.name, sig_name);
                                        return self.get_or_create_signal_driver(&sig_name);
                                    }
                                }
                                // Try by index in parent_module
                                let port_index = port_id.0 as usize;
                                if port_index < parent_module.ports.len() {
                                    let port = &parent_module.ports[port_index];
                                    let sig_name = format!("{}{}", parent_prefix, port.name);
                                    println!("🎨🎨🎨   -> Found port '{}' by index in parent_module, signal='{}'", port.name, sig_name);
                                    return self.get_or_create_signal_driver(&sig_name);
                                }
                            }

                            // Try self.mir (top-level module) - ports here don't need prefix
                            println!("🎨🎨🎨   -> Trying self.mir '{}'", self.mir.name);
                            if let Some(port) = self.mir.ports.iter().find(|p| p.id == *port_id) {
                                println!("🎨🎨🎨   -> Found port '{}' in self.mir (no prefix)", port.name);
                                return self.get_or_create_signal_driver(&port.name);
                            }
                            let port_index = port_id.0 as usize;
                            if port_index < self.mir.ports.len() {
                                let port = &self.mir.ports[port_index];
                                println!("🎨🎨🎨   -> Found port '{}' by index in self.mir (no prefix)", port.name);
                                return self.get_or_create_signal_driver(&port.name);
                            }

                            // BUG #113 FIX: DO NOT try child_module with inst_prefix here!
                            // After recursion, inst_prefix is from the PARENT context, not current
                            // child_module. Using child_module.ports with inst_prefix creates
                            // wrong signal names like "exec_l4_l5_inst_233.param_1".
                            // Let it fall through to the fallback lookup below instead.
                            println!("🎨🎨🎨   -> NOT using child_module '{}' with inst_prefix (Bug #113 fix)", child_module.name);
                        }

                        // Fallback to original behavior
                        println!("🎨🎨🎨   -> Port not in port_mapping, using fallback lookup");
                        if let Some(sig_name) = self.get_signal_from_lvalue_with_context(
                            lvalue,
                            inst_prefix,
                            port_mapping,
                            child_module,
                            parent_module_for_signals,
                            parent_prefix,
                        ) {
                            self.get_or_create_signal_driver(&sig_name)
                        } else {
                            eprintln!("🎨🎨🎨   -> FALLBACK: port {:?} not found, creating Constant(0,1)", port_id);
                            self.create_constant_node(0, 1)
                        }
                    }
                    _ => {
                        // Map through ports if needed, using context-aware lookup
                        if let Some(sig_name) = self.get_signal_from_lvalue_with_context(
                            lvalue,
                            inst_prefix,
                            port_mapping,
                            child_module,
                            parent_module_for_signals,
                            parent_prefix,
                        ) {
                            self.get_or_create_signal_driver(&sig_name)
                        } else {
                            // Fallback: create a zero constant
                            eprintln!("🎨🎨🎨   -> FALLBACK: lvalue {:?} not found, creating Constant(0,1)", lvalue);
                            self.create_constant_node(0, 1)
                        }
                    }
                }
            }
            ExpressionKind::Binary { op, left, right } => {
                let left_node = self.create_expression_node_for_instance_with_context(
                    left,
                    inst_prefix,
                    port_mapping,
                    child_module,
                    parent_module_for_signals,
                    parent_prefix,
                );
                let right_node = self.create_expression_node_for_instance_with_context(
                    right,
                    inst_prefix,
                    port_mapping,
                    child_module,
                    parent_module_for_signals,
                    parent_prefix,
                );
                self.create_binary_op_node(op, left_node, right_node)
            }
            ExpressionKind::Unary { op, operand } => {
                let operand_node = self.create_expression_node_for_instance_with_context(
                    operand,
                    inst_prefix,
                    port_mapping,
                    child_module,
                    parent_module_for_signals,
                    parent_prefix,
                );
                self.create_unary_op_node(op, operand_node)
            }
            ExpressionKind::Conditional {
                cond,
                then_expr,
                else_expr,
            } => {
                let cond_node = self.create_expression_node_for_instance_with_context(
                    cond,
                    inst_prefix,
                    port_mapping,
                    child_module,
                    parent_module_for_signals,
                    parent_prefix,
                );
                let then_node = self.create_expression_node_for_instance_with_context(
                    then_expr,
                    inst_prefix,
                    port_mapping,
                    child_module,
                    parent_module_for_signals,
                    parent_prefix,
                );
                let else_node = self.create_expression_node_for_instance_with_context(
                    else_expr,
                    inst_prefix,
                    port_mapping,
                    child_module,
                    parent_module_for_signals,
                    parent_prefix,
                );
                self.create_mux_node(cond_node, then_node, else_node)
            }
            ExpressionKind::Concat(parts) => {
                let part_nodes: Vec<usize> = parts
                    .iter()
                    .map(|p| {
                        self.create_expression_node_for_instance_with_context(
                            p,
                            inst_prefix,
                            port_mapping,
                            child_module,
                            parent_module_for_signals,
                            parent_prefix,
                        )
                    })
                    .collect();
                self.create_concat_node(part_nodes)
            }
            // BUG FIX #85: Handle tuple field access for module synthesis
            ExpressionKind::TupleFieldAccess { base, index } => {
                println!("🔍🔍🔍 TUPLE_FIELD_ACCESS (inst): ENTERING with index={}, base.kind={:?}", index, std::mem::discriminant(&base.kind));
                // Check if the base is a Signal reference (module instance result)
                if let ExpressionKind::Ref(LValue::Signal(sig_id)) = &base.kind {
                    println!("🔍🔍🔍   Base is Signal({}), looking in child_module with {} signals", sig_id.0, child_module.signals.len());
                    // Look up the signal name in child module
                    if let Some(signal) = child_module.signals.iter().find(|s| s.id == *sig_id) {
                        println!("🔍🔍🔍   Found signal: '{}' (checking pattern)", signal.name);

                        // BUG FIX #85: Check for _tuple_tmp_ pattern - this is a tuple temporary
                        // The _tuple_tmp_XX signals are intermediates that need to find the actual result signals
                        // For example, _tuple_tmp_76_76 -> look for assignments FROM quadratic_solve_inst_145_result_N
                        if signal.name.contains("_tuple_tmp_") {
                            println!("🔍🔍🔍   Detected _tuple_tmp_ pattern - looking for module instance result");
                            println!("🔍🔍🔍   Looking for assignment to signal {} in {} assignments", sig_id.0, child_module.assignments.len());
                            // Find the assignment to this signal to determine the source module instance
                            // The assignment should be: _tuple_tmp_76_76 = quadratic_solve_inst_145_result_0
                            // From that we can derive quadratic_solve_inst_145_result_N for index N
                            for (assign_idx, assign) in child_module.assignments.iter().enumerate() {
                                if let LValue::Signal(assign_sig_id) = &assign.lhs {
                                    if assign_sig_id == sig_id {
                                        println!("🔍🔍🔍   Found assignment #{} to signal {}, RHS kind: {:?}", assign_idx, sig_id.0, std::mem::discriminant(&assign.rhs.kind));
                                        // Found the assignment to this _tuple_tmp_ signal
                                        // Check if RHS is a reference to another signal
                                        if let ExpressionKind::Ref(LValue::Signal(src_sig_id)) = &assign.rhs.kind {
                                            println!("🔍🔍🔍   RHS is Signal({})", src_sig_id.0);
                                            if let Some(src_signal) = child_module.signals.iter().find(|s| s.id == *src_sig_id) {
                                                println!("🔍🔍🔍   Source signal name: '{}'", src_signal.name);
                                                if src_signal.name.contains("_inst_") && src_signal.name.contains("_result_") {
                                                    // Found the source module instance result signal
                                                    // Replace _result_0 (or _result_N) with _result_{index}
                                                    let base_name = if let Some(pos) = src_signal.name.rfind("_result_") {
                                                        &src_signal.name[..pos]
                                                    } else {
                                                        &src_signal.name[..]
                                                    };
                                                    let target_signal_name = format!("{}.{}_result_{}", inst_prefix, base_name, index);
                                                    println!("🔍🔍🔍   Found source: '{}' -> target: '{}'", src_signal.name, target_signal_name);
                                                    return self.get_or_create_signal_driver(&target_signal_name);
                                                } else {
                                                    println!("🔍🔍🔍   Source signal doesn't match _inst_/_result_ pattern");
                                                }
                                            } else {
                                                println!("🔍🔍🔍   Source signal {} not found in child_module.signals", src_sig_id.0);
                                            }
                                        // BUG FIX #85: Handle Concat RHS - tuple is built from concatenated results
                                        } else if let ExpressionKind::Concat(concat_parts) = &assign.rhs.kind {
                                            println!("🔍🔍🔍   RHS is Concat with {} parts, looking for index {}", concat_parts.len(), index);
                                            // The tuple is a concatenation of result signals
                                            // Index 0 -> first part, Index 1 -> second part, etc.
                                            if *index < concat_parts.len() {
                                                let target_part = &concat_parts[*index];
                                                println!("🔍🔍🔍   Concat part {} kind: {:?}", index, std::mem::discriminant(&target_part.kind));
                                                if let ExpressionKind::Ref(LValue::Signal(part_sig_id)) = &target_part.kind {
                                                    if let Some(part_signal) = child_module.signals.iter().find(|s| s.id == *part_sig_id) {
                                                        println!("🔍🔍🔍   Found concat part signal: '{}' for index {}", part_signal.name, index);
                                                        // Found the signal for this index in the concat
                                                        let target_signal_name = format!("{}.{}", inst_prefix, part_signal.name);
                                                        println!("🔍🔍🔍   Resolved to: '{}'", target_signal_name);
                                                        return self.get_or_create_signal_driver(&target_signal_name);
                                                    }
                                                }
                                            }
                                            println!("🔍🔍🔍   Could not resolve concat part at index {}", index);
                                        } else {
                                            println!("🔍🔍🔍   RHS is not a Signal reference or Concat");
                                        }
                                    }
                                }
                            }
                            println!("🔍🔍🔍   Could not find source assignment for _tuple_tmp_ signal");
                        } else if signal.name.ends_with("_result_0") || signal.name.contains("_inst_") {
                            let base_name = if let Some(pos) = signal.name.rfind("_result_") {
                                &signal.name[..pos]
                            } else {
                                &signal.name[..]
                            };
                            let target_signal_name = format!("{}.{}_result_{}", inst_prefix, base_name, index);
                            println!("🔍🔍🔍 TUPLE_FIELD_ACCESS (inst): base='{}' index={} -> target='{}'",
                                    signal.name, index, target_signal_name);
                            return self.get_or_create_signal_driver(&target_signal_name);
                        } else {
                            println!("🔍🔍🔍   Signal name doesn't match pattern");
                        }
                    } else {
                        println!("🔍🔍🔍   Signal {} NOT FOUND in child_module.signals", sig_id.0);
                    }
                } else {
                    println!("🔍🔍🔍   Base is NOT a Signal reference, falling back");
                }
                // Fallback: bit slicing
                let base_node = self.create_expression_node_for_instance_with_context(
                    base,
                    inst_prefix,
                    port_mapping,
                    child_module,
                    parent_module_for_signals,
                    parent_prefix,
                );
                let element_width = 32;
                let start_bit = (*index) * element_width;
                let end_bit = start_bit + element_width - 1;
                self.create_slice_node(base_node, start_bit, end_bit)
            }
            ExpressionKind::FieldAccess { base, field } => {
                // For named field access, fall back to base expression
                eprintln!("    ⚠️ FieldAccess on field '{}' - falling back to base (inst)", field);
                self.create_expression_node_for_instance_with_context(
                    base,
                    inst_prefix,
                    port_mapping,
                    child_module,
                    parent_module_for_signals,
                    parent_prefix,
                )
            }
            _ => {
                // For unsupported expressions, create a zero constant
                self.create_constant_node(0, 1)
            }
        };
        eprintln!(
            "🔧 create_expression_node_for_instance: returning node {}",
            result
        );
        result
    }

    /// Collect input signal references from expression
    #[allow(dead_code)]
    fn collect_expression_inputs(
        &self,
        expr: &Expression,
        inst_prefix: &str,
        port_mapping: &HashMap<String, Expression>,
        child_module: &Module,
    ) -> Vec<SignalRef> {
        let mut inputs = Vec::new();

        match &expr.kind {
            ExpressionKind::Ref(lvalue) => {
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
            ExpressionKind::Binary { op: _, left, right } => {
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
            ExpressionKind::Unary { op: _, operand } => {
                inputs.extend(self.collect_expression_inputs(
                    operand,
                    inst_prefix,
                    port_mapping,
                    child_module,
                ));
            }
            ExpressionKind::Conditional {
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
        // Call with no parent context (for top-level instances)
        self.get_signal_from_lvalue_with_context(
            lvalue,
            inst_prefix,
            port_mapping,
            child_module,
            None,
            "",
        )
    }

    /// Get signal name from lvalue with optional parent module context
    /// For nested instances, parent_module_for_signals contains the module
    /// that owns the SignalIds in port_mapping values
    fn get_signal_from_lvalue_with_context(
        &self,
        lvalue: &LValue,
        inst_prefix: &str,
        port_mapping: &HashMap<String, Expression>,
        child_module: &Module,
        parent_module_for_signals: Option<&Module>,
        parent_prefix: &str,
    ) -> Option<String> {
        eprintln!("🔍🔍🔍 GET_SIGNAL_FROM_LVALUE: lvalue={:?}, inst_prefix='{}'", lvalue, inst_prefix);
        match lvalue {
            LValue::Signal(sig_id) => {
                eprintln!("🔍🔍🔍   -> LValue::Signal({})", sig_id.0);
                // LValue::Signal should only match signals, not ports
                // (ports have their own LValue::Port variant)
                child_module
                    .signals
                    .iter()
                    .find(|s| s.id == *sig_id)
                    .map(|signal| {
                        // BUG FIX #113: Avoid double dots if inst_prefix ends with a dot
                        if inst_prefix.is_empty() {
                            signal.name.clone()
                        } else if inst_prefix.ends_with('.') {
                            format!("{}{}", inst_prefix, signal.name)
                        } else {
                            format!("{}.{}", inst_prefix, signal.name)
                        }
                    })
            }
            LValue::Port(port_id) => {
                // Direct port reference - map to parent signal through port connections
                // First try direct ID lookup
                println!("🔌🔌🔌 PORT LOOKUP: Looking for port_id={} in child_module '{}' (has {} ports)", port_id.0, child_module.name, child_module.ports.len());
                for p in &child_module.ports {
                    println!("🔌🔌🔌   Child port: id={}, name='{}'", p.id.0, p.name);
                }
                println!("🔌🔌🔌 PORT MAPPING keys: {:?}", port_mapping.keys().collect::<Vec<_>>());
                if let Some(port) = child_module.ports.iter().find(|p| p.id == *port_id) {
                    println!("🔌🔌🔌 PORT LOOKUP: Found port '{}' by id={}", port.name, port_id.0);
                    if let Some(parent_expr) = port_mapping.get(&port.name) {
                        println!("🔌🔌🔌 PORT LOOKUP: Found mapping for '{}' -> {:?}", port.name, parent_expr.kind);
                        // Use context-aware lookup for nested instances
                        let mapped_name = self.get_signal_name_from_expression_with_context(
                            parent_expr,
                            parent_module_for_signals,
                            parent_prefix,
                        );
                        println!("🔌🔌🔌 PORT LOOKUP: Mapped to '{}'", mapped_name);
                        Some(mapped_name)
                    } else {
                        println!("🔌🔌🔌 PORT LOOKUP: NO mapping for '{}' - using fallback", port.name);
                        // No mapping - use prefixed port name as fallback
                        // BUG FIX #113: Avoid double dots if inst_prefix ends with a dot
                        let prefixed = if inst_prefix.is_empty() {
                            port.name.clone()
                        } else if inst_prefix.ends_with('.') {
                            format!("{}{}", inst_prefix, port.name)
                        } else {
                            format!("{}.{}", inst_prefix, port.name)
                        };
                        Some(prefixed)
                    }
                } else {
                    println!("🔌🔌🔌 PORT LOOKUP: Port id={} NOT FOUND by direct lookup, trying index", port_id.0);
                    // BUG#24 FIX: Port IDs get renumbered during monomorphization, but MIR Process
                    // objects still reference the original IDs. Fall back to using port ID value
                    // as an index, since ports remain in the same order after specialization.
                    let port_index = port_id.0 as usize;
                    if port_index < child_module.ports.len() {
                        let port = &child_module.ports[port_index];
                        if let Some(parent_expr) = port_mapping.get(&port.name) {
                            // Use context-aware lookup for nested instances
                            let mapped_name = self.get_signal_name_from_expression_with_context(
                                parent_expr,
                                parent_module_for_signals,
                                parent_prefix,
                            );
                            Some(mapped_name)
                        } else {
                            // BUG FIX #113: Avoid double dots if inst_prefix ends with a dot
                            let prefixed = if inst_prefix.is_empty() {
                                port.name.clone()
                            } else if inst_prefix.ends_with('.') {
                                format!("{}{}", inst_prefix, port.name)
                            } else {
                                format!("{}.{}", inst_prefix, port.name)
                            };
                            Some(prefixed)
                        }
                    } else {
                        None
                    }
                }
            }
            // BUG #116 FIX: Handle Variable references in nested module contexts
            // Variables can appear in port mappings for nested instances. We need to look up
            // the variable in the parent module (for nested instances) or self.mir (for top-level).
            LValue::Variable(var_id) => {
                println!("🔧🔧🔧 BUG #116 FIX: LValue::Variable({}) in get_signal_from_lvalue_with_context", var_id.0);
                println!("🔧🔧🔧   inst_prefix='{}', parent_prefix='{}'", inst_prefix, parent_prefix);

                // First try child_module (the current module context)
                if let Some(var) = child_module.variables.iter().find(|v| v.id == *var_id) {
                    let var_name = format!("{}_{}", var.name, var_id.0);
                    let prefixed = if inst_prefix.is_empty() {
                        var_name
                    } else if inst_prefix.ends_with('.') {
                        format!("{}{}", inst_prefix, var_name)
                    } else {
                        format!("{}.{}", inst_prefix, var_name)
                    };
                    println!("🔧🔧🔧   -> Found in child_module '{}', signal='{}'", child_module.name, prefixed);
                    return Some(prefixed);
                }

                // Try parent_module_for_signals if provided (for nested instances)
                if let Some(parent) = parent_module_for_signals {
                    if let Some(var) = parent.variables.iter().find(|v| v.id == *var_id) {
                        let var_name = format!("{}_{}", var.name, var_id.0);
                        let prefixed = if parent_prefix.is_empty() {
                            var_name
                        } else if parent_prefix.ends_with('.') {
                            format!("{}{}", parent_prefix, var_name)
                        } else {
                            format!("{}.{}", parent_prefix, var_name)
                        };
                        println!("🔧🔧🔧   -> Found in parent_module '{}', signal='{}'", parent.name, prefixed);
                        return Some(prefixed);
                    }
                }

                // Fallback to self.mir (top-level module)
                if let Some(var) = self.mir.variables.iter().find(|v| v.id == *var_id) {
                    let var_name = format!("{}_{}", var.name, var_id.0);
                    println!("🔧🔧🔧   -> Found in self.mir '{}', signal='{}' (no prefix)", self.mir.name, var_name);
                    return Some(var_name);
                }

                println!("🔧🔧🔧   -> Variable {} NOT FOUND in any module!", var_id.0);
                None
            }
            _ => None,
        }
    }

    /// Extract signal name from expression (for port mapping)
    fn get_signal_name_from_expression(&self, expr: &Expression) -> String {
        self.get_signal_name_from_expression_with_context(expr, None, "")
    }

    /// Extract signal name from expression with optional parent module context
    /// For nested instances, we need to look up signals in the parent module, not self.mir
    /// The inst_prefix is prepended to signal names found in the parent module
    fn get_signal_name_from_expression_with_context(
        &self,
        expr: &Expression,
        parent_module: Option<&Module>,
        inst_prefix: &str,
    ) -> String {
        match &expr.kind {
            ExpressionKind::Ref(lvalue) => {
                match lvalue {
                    LValue::Signal(sig_id) => {
                        // First try parent module if provided (for nested instances)
                        if let Some(parent) = parent_module {
                            if let Some(signal) = parent.signals.iter().find(|s| s.id == *sig_id) {
                                // Signal found in parent module - prepend instance prefix
                                return format!("{}{}", inst_prefix, signal.name);
                            }
                            if let Some(port) = parent.ports.iter().find(|p| p.id.0 == sig_id.0) {
                                return format!("{}{}", inst_prefix, port.name);
                            }
                        }

                        // Fall back to current top-level module
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
                        // First try parent module if provided
                        if let Some(parent) = parent_module {
                            if let Some(port) = parent.ports.iter().find(|p| p.id == *port_id) {
                                return format!("{}{}", inst_prefix, port.name);
                            }
                        }

                        // Find port by ID in current module (top-level)
                        if let Some(port) = self.mir.ports.iter().find(|p| p.id == *port_id) {
                            port.name.clone()
                        } else {
                            format!("unknown_port_{:?}", port_id)
                        }
                    }
                    LValue::Variable(var_id) => {
                        // BUG FIX #86: Use unique signal names (name + id) to prevent collisions
                        // First try parent module if provided
                        if let Some(parent) = parent_module {
                            if let Some(var) = parent.variables.iter().find(|v| v.id == *var_id) {
                                return format!("{}{}_{}",  inst_prefix, var.name, var_id.0);
                            }
                        }

                        // Find variable by ID in current module
                        if let Some(var) = self.mir.variables.iter().find(|v| v.id == *var_id) {
                            format!("{}_{}", var.name, var_id.0)
                        } else {
                            format!("var_{}", var_id.0)
                        }
                    }
                    LValue::BitSelect { base, .. } | LValue::RangeSelect { base, .. } => {
                        // For bit/range selects, get the base signal name
                        self.get_signal_name_from_expression_with_context(
                            &Expression::with_unknown_type(ExpressionKind::Ref((**base).clone())),
                            parent_module,
                            inst_prefix,
                        )
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

    /// Check if signal or any of its flattened siblings is sequential
    ///
    /// WORKAROUND for Bug #8: HIR→MIR doesn't properly expand array index assignments
    /// like `mem[index] <= value` on flattened arrays. It only assigns to the first
    /// flattened element (e.g., mem_0_x), so only that element is detected as sequential.
    ///
    /// This function checks if the signal OR any sibling with a matching flattened name
    /// pattern is sequential. For example, if mem_0_x is sequential, then mem_0_y, mem_0_z,
    /// mem_1_x, etc. should all be treated as sequential.
    #[allow(dead_code)]
    fn is_signal_or_sibling_sequential(&self, signal: &skalp_mir::Signal, module: &Module) -> bool {
        // First check the signal itself
        if self.is_signal_sequential_in_module(signal.id, module) {
            return true;
        }

        // Check if this signal is part of a flattened array by looking for the pattern:
        // base_N_field or base_N where N is a digit
        let signal_name = &signal.name;

        // Try to extract the base name (everything before the flattened array index)
        // Pattern: "mem_0_x" -> base = "mem"
        // Pattern: "mem_5_z" -> base = "mem"
        // Pattern: "wr_ptr" -> no pattern, return false

        // Look for all signals that might be siblings in the same flattened array
        // and check if ANY of them are sequential
        for other_signal in &module.signals {
            if other_signal.id == signal.id {
                continue; // Skip self (already checked)
            }

            // Check if this could be a sibling in a flattened array
            // Heuristic: if both names share a common prefix and differ by a digit/field suffix
            if self.are_flattened_siblings(signal_name, &other_signal.name)
                && self.is_signal_sequential_in_module(other_signal.id, module)
            {
                eprintln!(
                    "      ⚠️ SIBLING DETECTED: '{}' is sequential, so '{}' should be too!",
                    other_signal.name, signal_name
                );
                return true;
            }
        }

        false
    }

    /// Check if two signal names are siblings in a flattened array
    /// Examples:
    ///   - "mem_0_x" and "mem_0_y" -> true (same array element, different fields)
    ///   - "mem_0_x" and "mem_1_x" -> true (different array elements, same field)
    ///   - "mem_0_x" and "wr_ptr" -> false (not related)
    #[allow(dead_code)]
    fn are_flattened_siblings(&self, name1: &str, name2: &str) -> bool {
        // Extract components: base, index, field for each name
        let components1 = self.parse_flattened_name(name1);
        let components2 = self.parse_flattened_name(name2);

        if let (Some((base1, _, _)), Some((base2, _, _))) = (components1, components2) {
            // They're siblings if they have the same base name
            base1 == base2
        } else {
            false
        }
    }

    /// Parse a flattened signal name into (base, index, field) components
    /// Examples:
    ///   - "mem_0_x" -> Some(("mem", "0", "x"))  (array of structs)
    ///   - "mem_7_z" -> Some(("mem", "7", "z"))  (array of structs)
    ///   - "wr_data_x" -> Some(("wr_data", "", "x"))  (struct, no array)
    ///   - "wr_ptr" -> None (not a flattened name)
    fn parse_flattened_name(&self, name: &str) -> Option<(String, String, String)> {
        let parts: Vec<&str> = name.split('_').collect();

        // Pattern 1: base_N_field (at least 3 parts, second-to-last is digit)
        // Example: mem_0_x, mem_7_z
        if parts.len() >= 3 {
            // Check if the second-to-last part is a digit (the array index)
            let potential_index = parts[parts.len() - 2];
            if potential_index.chars().all(|c| c.is_ascii_digit()) {
                let base = parts[..parts.len() - 2].join("_");
                let index = potential_index.to_string();
                let field = parts[parts.len() - 1].to_string();
                return Some((base, index, field));
            }
        }

        // Pattern 2: base_field (struct flattening, no array index)
        // Example: wr_data_x, wr_data_y
        // Check if the last part is a single letter (struct field like x, y, z, w)
        if parts.len() >= 2 {
            let potential_field = parts[parts.len() - 1];
            if potential_field.len() == 1 && potential_field.chars().all(|c| c.is_alphabetic()) {
                // Make sure the preceding part is NOT a digit (to avoid confusing with Pattern 3)
                let preceding = parts[parts.len() - 2];
                if !preceding.chars().all(|c| c.is_ascii_digit()) {
                    let base = parts[..parts.len() - 1].join("_");
                    let field = potential_field.to_string();
                    return Some((base, String::new(), field));
                }
            }
        }

        // Pattern 3: base_N (exactly 2 parts, second is digit - array without struct)
        if parts.len() == 2 && parts[1].chars().all(|c| c.is_ascii_digit()) {
            let base = parts[0].to_string();
            let index = parts[1].to_string();
            return Some((base, index, String::new()));
        }

        None
    }

    /// Adapt RHS expression to match target's field suffix
    ///
    /// WORKAROUND for Bug #8: When assigning to flattened arrays, the RHS might
    /// reference only the first flattened element. This function adapts the RHS
    /// to use the correct field based on the target.
    ///
    /// Example:
    ///   - Target: "input_fifo.mem_0_y" (field = "y")
    ///   - RHS: Ref(Signal(data_x))
    ///   - Result: Ref(Signal(data_y))
    #[allow(dead_code)]
    fn adapt_rhs_for_flattened_target(
        &self,
        rhs: &Expression,
        target: &str,
        child_module: &Module,
    ) -> Expression {
        // Extract the field suffix from the target
        // Remove instance prefix first (e.g., "input_fifo.mem_0_y" -> "mem_0_y")
        let target_short = target.split('.').next_back().unwrap_or(target);
        let target_field = if let Some((_, _, field)) = self.parse_flattened_name(target_short) {
            if !field.is_empty() {
                Some(field)
            } else {
                None
            }
        } else {
            None
        };

        // If target has no field, return RHS unchanged
        let target_field = match target_field {
            Some(f) => f,
            None => return rhs.clone(),
        };

        // Recursively adapt the expression
        self.adapt_expression_field(rhs, &target_field, child_module)
    }

    /// Recursively adapt an expression to use a different field suffix
    #[allow(dead_code)]
    fn adapt_expression_field(
        &self,
        expr: &Expression,
        target_field: &str,
        child_module: &Module,
    ) -> Expression {
        let new_kind = match &expr.kind {
            ExpressionKind::Ref(lvalue) => {
                ExpressionKind::Ref(self.adapt_lvalue_field(lvalue, target_field, child_module))
            }
            ExpressionKind::Binary { op, left, right } => ExpressionKind::Binary {
                op: *op,
                left: Box::new(self.adapt_expression_field(left, target_field, child_module)),
                right: Box::new(self.adapt_expression_field(right, target_field, child_module)),
            },
            ExpressionKind::Unary { op, operand } => ExpressionKind::Unary {
                op: *op,
                operand: Box::new(self.adapt_expression_field(operand, target_field, child_module)),
            },
            ExpressionKind::Conditional {
                cond,
                then_expr,
                else_expr,
            } => ExpressionKind::Conditional {
                cond: Box::new(self.adapt_expression_field(cond, target_field, child_module)),
                then_expr: Box::new(self.adapt_expression_field(
                    then_expr,
                    target_field,
                    child_module,
                )),
                else_expr: Box::new(self.adapt_expression_field(
                    else_expr,
                    target_field,
                    child_module,
                )),
            },
            // For other expression types, return as-is
            _ => return expr.clone(),
        };
        Expression::with_unknown_type(new_kind)
    }

    /// Adapt an LValue to use a different field suffix
    #[allow(dead_code)]
    fn adapt_lvalue_field(
        &self,
        lvalue: &LValue,
        target_field: &str,
        child_module: &Module,
    ) -> LValue {
        match lvalue {
            LValue::Signal(sig_id) => {
                // Find the signal in the child module
                if let Some(signal) = child_module.signals.iter().find(|s| s.id == *sig_id) {
                    // Check if this signal is a flattened element
                    if let Some((base, index, current_field)) =
                        self.parse_flattened_name(&signal.name)
                    {
                        if !current_field.is_empty() && current_field != target_field {
                            // Build new name based on whether there's an array index
                            let new_name = if index.is_empty() {
                                // Struct only: base_field
                                format!("{}_{}", base, target_field)
                            } else {
                                // Array of structs: base_index_field
                                format!("{}_{}_{}", base, index, target_field)
                            };
                            // Find the signal with the new name
                            if let Some(new_signal) =
                                child_module.signals.iter().find(|s| s.name == new_name)
                            {
                                return LValue::Signal(new_signal.id);
                            }
                        }
                    }
                }
                lvalue.clone()
            }
            LValue::Port(port_id) => {
                // Similar logic for ports
                if let Some(port) = child_module.ports.iter().find(|p| p.id == *port_id) {
                    if let Some((base, index, current_field)) =
                        self.parse_flattened_name(&port.name)
                    {
                        if !current_field.is_empty() && current_field != target_field {
                            // Build new name based on whether there's an array index
                            let new_name = if index.is_empty() {
                                // Struct only: base_field
                                format!("{}_{}", base, target_field)
                            } else {
                                // Array of structs: base_index_field
                                format!("{}_{}_{}", base, index, target_field)
                            };
                            if let Some(new_port) =
                                child_module.ports.iter().find(|p| p.name == new_name)
                            {
                                return LValue::Port(new_port.id);
                            }
                        }
                    }
                }
                lvalue.clone()
            }
            _ => lvalue.clone(),
        }
    }

    /// Flatten instances for a specific module (used for recursion)
    /// parent_module_for_signals: The module containing the SignalIds in instance.connections
    /// parent_prefix: The prefix to use when looking up signals from parent_module_for_signals
    fn flatten_instances_for_module(
        &mut self,
        module: &Module,
        instance_prefix: &str,
        parent_module_for_signals: Option<&Module>,
        parent_prefix: &str,
    ) {
        let instances = module.instances.clone();

        for instance in &instances {
            let child_module = self
                .mir_design
                .modules
                .iter()
                .find(|m| m.id == instance.module)
                .unwrap_or_else(|| {
                    let location = instance.span.as_ref()
                        .map(|s| format!(" at {}", s))
                        .unwrap_or_default();
                    panic!("Module {:?} not found for instance '{}'{}", instance.module, instance.name, location)
                });

            let inst_prefix = format!("{}{}", instance_prefix, instance.name);
            self.elaborate_instance_with_context(
                instance,
                child_module,
                &inst_prefix,
                parent_module_for_signals,
                parent_prefix,
            );
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
                                span: None,
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
            result_expr = Expression::with_unknown_type(ExpressionKind::Conditional {
                cond: Box::new(case.condition.clone()),
                then_expr: Box::new(case.value.clone()),
                else_expr: Box::new(result_expr),
            });
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
            result_expr = Expression::with_unknown_type(ExpressionKind::Conditional {
                cond: Box::new(case.condition.clone()),
                then_expr: Box::new(case.value.clone()),
                else_expr: Box::new(result_expr),
            });
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
        match &expr.kind {
            ExpressionKind::Literal(value) => match value {
                Value::Integer(i) => Some(*i as u64),
                Value::BitVector { value, .. } => Some(*value),
                _ => None,
            },
            _ => None, // Only literals can be evaluated as constants for now
        }
    }
}
