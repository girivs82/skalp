use crate::pipeline::insert_pipeline_registers;
use crate::sir::*;
use skalp_frontend::safety_attributes::ModuleSafetyDefinitions;
use skalp_mir::mir::PortDirection as MirPortDirection;
use skalp_mir::mir::PriorityMux;
use skalp_mir::signal_naming::flattened_to_hierarchical;
use skalp_mir::{
    BinaryOp, Block, DataType, EdgeType, Expression, ExpressionKind, IfStatement, LValue, Mir,
    Module, NameKind, ProcessKind, SensitivityList, SignalId, Statement, Value,
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
        safety_definitions: ModuleSafetyDefinitions::default(),
    };
    convert_mir_to_sir_with_hierarchy(&mir, mir_module)
}

/// Convert MIR to SIR with hierarchical elaboration
/// Takes the full Mir design (all modules) to resolve instances
pub fn convert_mir_to_sir_with_hierarchy(mir: &Mir, top_module: &Module) -> SirModule {
    let mut sir = SirModule::new(top_module.name.clone());

    // Propagate pipeline configuration from MIR to SIR
    sir.pipeline_config = top_module.pipeline_config.clone();

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

    // Run pipeline register insertion pass if configured
    let _ = insert_pipeline_registers(&mut sir);

    // Pre-compute topological order for efficient simulation
    sir.finalize_topological_order();

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
    // BUG FIX: Track elaborated instance paths to detect circular references
    // This prevents infinite recursion when modules have circular instantiation chains
    elaborated_instances: std::collections::HashSet<String>,
    // BUG FIX #209: Track parent module ID for each instance prefix
    // This allows us to look up the correct grandparent module when resolving 3+ levels of hierarchy
    instance_parent_module_ids: HashMap<String, skalp_mir::ModuleId>,
    /// Maps MIR signal/port names to their internal names (_s0, _s1, etc.)
    /// This is populated during convert_ports/convert_signals and used during expression conversion
    mir_to_internal_name: HashMap<String, String>,
    /// BUG #226 FIX: Stores default values from unconditional assignments in a sequential block
    /// These are used by conditional handlers (if/case) when their branches don't override the target.
    sequential_defaults: HashMap<String, usize>,
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
            elaborated_instances: std::collections::HashSet::new(),
            instance_parent_module_ids: HashMap::new(),
            mir_to_internal_name: HashMap::new(),
            sequential_defaults: HashMap::new(),
        }
    }

    /// BUG FIX #85: Build a mapping of signal IDs to their module instance source
    /// This is used to correctly resolve TupleFieldAccess expressions when the base
    /// signal is assigned from a module instance result (e.g., `_tuple_tmp_76 = inst_result_0`)
    fn build_tuple_source_mapping(mir: &Module) -> HashMap<SignalId, String> {
        let mut mapping = HashMap::new();

        for assign in &mir.assignments {
            // Check if LHS is a signal
            if let LValue::Signal(lhs_id) = &assign.lhs {
                // Case 1: RHS is a reference to another signal
                if let ExpressionKind::Ref(LValue::Signal(rhs_id)) = &assign.rhs.kind {
                    // Check if RHS signal name contains "_inst_" and "_result_"
                    // This indicates it's a module instance result signal
                    if let Some(rhs_signal) = mir.signals.iter().find(|s| s.id == *rhs_id) {
                        if rhs_signal.name.contains("_inst_")
                            && rhs_signal.name.contains("_result_")
                        {
                            // Extract the prefix (everything before "_result_N")
                            if let Some(pos) = rhs_signal.name.rfind("_result_") {
                                let prefix = &rhs_signal.name[..pos];
                                mapping.insert(*lhs_id, prefix.to_string());
                            }
                        }
                    }
                }

                // Case 2: RHS is a Concat - check if first element is a module instance result
                if let ExpressionKind::Concat(parts) = &assign.rhs.kind {
                    if let Some(first) = parts.first() {
                        if let ExpressionKind::Ref(LValue::Signal(first_sig_id)) = &first.kind {
                            if let Some(first_signal) =
                                mir.signals.iter().find(|s| s.id == *first_sig_id)
                            {
                                if first_signal.name.contains("_inst_")
                                    && first_signal.name.contains("_result_")
                                {
                                    if let Some(pos) = first_signal.name.rfind("_result_") {
                                        let prefix = &first_signal.name[..pos];
                                        mapping.insert(*lhs_id, prefix.to_string());
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

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

            // Convert flattened MIR name (e.g., bms__connected) to hierarchical path (e.g., bms.connected)
            let hierarchical_path = flattened_to_hierarchical(&port.name);

            // Register in name registry for testbench path resolution
            // Registry generates collision-proof internal names (_s0, _s1, etc.)
            let name_kind = match port.direction {
                skalp_mir::PortDirection::Input => NameKind::Input,
                skalp_mir::PortDirection::Output => NameKind::Output,
                skalp_mir::PortDirection::InOut => NameKind::InOut,
            };
            let internal_name = self.sir.name_registry.register(
                &hierarchical_path,
                &port.name, // source name for debugging
                name_kind,
                width,
            );

            // Store mapping from MIR name to internal name for expression conversion
            self.mir_to_internal_name
                .insert(port.name.clone(), internal_name.clone());

            let sir_port = SirPort {
                name: internal_name.clone(), // Use internal name in SIR
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
                name: internal_name.clone(), // Use internal name in SIR
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
                    internal_name.clone(), // Use internal name as key
                    StateElement {
                        name: internal_name.clone(), // Use internal name
                        width,
                        sir_type: Some(sir_type.clone()),
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

            // Determine if this is a register by checking if it's assigned in sequential blocks
            let is_register = self.is_signal_sequential(signal.id);

            // Convert flattened MIR name to hierarchical path
            let hierarchical_path = flattened_to_hierarchical(&signal.name);

            // Register in name registry for testbench path resolution
            // Registry generates collision-proof internal names (_s0, _s1, etc.)
            let name_kind = if is_register {
                NameKind::StateElement
            } else {
                NameKind::Signal
            };
            let internal_name = self.sir.name_registry.register(
                &hierarchical_path,
                &signal.name, // source name for debugging
                name_kind,
                width,
            );

            // Store mapping from MIR name to internal name for expression conversion
            self.mir_to_internal_name
                .insert(signal.name.clone(), internal_name.clone());

            self.sir.signals.push(SirSignal {
                name: internal_name.clone(), // Use internal name in SIR
                width,
                sir_type: sir_type.clone(),
                driver_node: None,
                fanout_nodes: Vec::new(),
                is_state: is_register,
                span: signal.span.clone(),
            });

            if is_register {
                self.sir.state_elements.insert(
                    internal_name.clone(), // Use internal name as key
                    StateElement {
                        name: internal_name.clone(), // Use internal name
                        width,
                        sir_type: Some(sir_type.clone()),
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
            let unique_mir_name = format!("{}_{}", variable.name, variable.id.0);

            // Register in name registry for testbench path resolution
            // Registry generates collision-proof internal names (_s0, _s1, etc.)
            let internal_name = self.sir.name_registry.register(
                &unique_mir_name, // Variables use their unique MIR name as the path
                &variable.name,   // source name for debugging
                NameKind::Variable,
                width,
            );

            // Store mapping from MIR name to internal name for expression conversion
            self.mir_to_internal_name
                .insert(unique_mir_name, internal_name.clone());

            // Variables (let bindings) are always combinational wires, never registers
            self.sir.signals.push(SirSignal {
                name: internal_name, // Use internal name in SIR
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
        // CRITICAL FIX: Convert continuous assignments BEFORE processes
        // This ensures that when sequential processes reference combinational signals,
        // those signals already exist with their driver nodes
        for assign in &self.mir.assignments {
            let target = self.lvalue_to_string(&assign.lhs);
            self.convert_continuous_assign(&target, &assign.rhs);
        }

        // Convert processes
        for process in self.mir.processes.iter() {
            match &process.kind {
                ProcessKind::Combinational => {
                    self.convert_combinational_block(&process.body.statements);
                }
                ProcessKind::Sequential => {
                    if let SensitivityList::Edge(edges) = &process.sensitivity {
                        // Find the clock edge (Rising/Falling), skipping Active/Inactive reset edges
                        let clock_edge = edges.iter().find(|e| {
                            matches!(
                                e.edge,
                                EdgeType::Rising | EdgeType::Falling | EdgeType::Both
                            )
                        });
                        if let Some(edge_sens) = clock_edge {
                            let edge = match edge_sens.edge {
                                EdgeType::Rising => ClockEdge::Rising,
                                EdgeType::Falling => ClockEdge::Falling,
                                EdgeType::Both => ClockEdge::Both,
                                _ => ClockEdge::Rising,
                            };
                            let signal_name = self.lvalue_to_string(&edge_sens.signal);
                            self.convert_sequential_block(
                                &process.body.statements,
                                &signal_name,
                                edge,
                            );
                        }
                    }
                }
                _ => {}
            }
        }
    }

    fn convert_combinational_block(&mut self, statements: &[Statement]) {
        // CRITICAL FIX: Use shared context for dependency tracking in combinational blocks
        let mut local_context = std::collections::HashMap::new();

        for stmt in statements {
            match stmt {
                Statement::Assignment(assign) => {
                    let target = self.lvalue_to_string(&assign.lhs);

                    // Create expression with current local context
                    let node_id =
                        self.create_expression_with_local_context(&assign.rhs, &local_context);

                    // Store in local context for future references
                    local_context.insert(target.clone(), node_id);

                    self.connect_node_to_signal(node_id, &target);
                }
                Statement::Block(block) => {
                    self.convert_combinational_block(&block.statements);
                }
                Statement::If(if_stmt) => {
                    // Convert if statement to mux
                    self.convert_if_to_mux(if_stmt);
                }
                Statement::Case(case_stmt) => {
                    // Convert case statement to mux tree
                    self.convert_case_to_mux_tree(case_stmt);
                }
                Statement::ResolvedConditional(resolved) => {
                    let target = self.lvalue_to_string(&resolved.target);

                    // CRITICAL FIX: Use shared context to create the priority mux
                    let mux_node =
                        self.create_priority_mux_with_context(&resolved.resolved, &local_context);

                    // Store in local context for future references
                    local_context.insert(target.clone(), mux_node);

                    self.connect_node_to_signal(mux_node, &target);
                }
                _ => {}
            }
        }
    }

    fn convert_sequential_block(&mut self, statements: &[Statement], clock: &str, edge: ClockEdge) {
        // BUG #226 FIX: Clear any previous defaults from earlier sequential blocks
        // These will be populated with unconditional assignments that precede conditionals.
        self.sequential_defaults.clear();

        // First pass: Collect targets that have both unconditional and conditional assignments
        let mut targets_with_conditionals: std::collections::HashSet<String> =
            std::collections::HashSet::new();
        for stmt in statements.iter() {
            match stmt {
                Statement::If(if_stmt) => {
                    self.collect_targets_from_if(if_stmt, &mut targets_with_conditionals);
                }
                Statement::Case(case_stmt) => {
                    for item in &case_stmt.items {
                        self.collect_all_targets_from_statements(
                            &item.block.statements,
                            &mut targets_with_conditionals,
                        );
                    }
                    if let Some(default_block) = &case_stmt.default {
                        self.collect_all_targets_from_statements(
                            &default_block.statements,
                            &mut targets_with_conditionals,
                        );
                    }
                }
                _ => {}
            }
        }

        // Process ALL statements sequentially
        // This handles ResolvedConditional statements with proper dependency tracking
        for stmt in statements.iter() {
            match stmt {
                Statement::Assignment(assign) => {
                    // Check if this is an array write (BitSelect with non-constant index)
                    if let LValue::BitSelect { base, index } = &assign.lhs {
                        let is_const = self.evaluate_constant_expression(index);
                        if is_const.is_none() {
                            // Dynamic array write: array[index] <= value
                            let array_name = self.lvalue_to_string(base);

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
                            // BUG FIX: Pass target signal width to expression creation
                            let target_width = self.get_signal_width(&target);
                            let value = self
                                .create_expression_node_with_width(&assign.rhs, Some(target_width));
                            let ff_node =
                                self.create_flipflop_with_input(value, clock, edge.clone());
                            self.connect_node_to_signal(ff_node, &target);
                        }
                    } else {
                        // Normal assignment (not BitSelect)
                        let target = self.lvalue_to_string(&assign.lhs);

                        // CRITICAL: For sequential blocks (non-blocking assignments), all RHS
                        // values are sampled at the beginning of the clock cycle.
                        // We should NOT use a local context that tracks intermediate values.

                        // BUG FIX: Pass target signal width to expression creation
                        // This ensures integer literals get the correct width instead of defaulting to 32
                        let target_width = self.get_signal_width(&target);
                        let value =
                            self.create_expression_node_with_width(&assign.rhs, Some(target_width));

                        // BUG #226 FIX: If this target also has a conditional assignment later,
                        // store this value as the default instead of creating a flip-flop now.
                        // The conditional handler will use this as the default when its branches
                        // don't explicitly assign to this target.
                        if targets_with_conditionals.contains(&target) {
                            self.sequential_defaults.insert(target.clone(), value);
                        } else {
                            // Create flip-flop directly (no conditional overrides this)
                            let ff_node =
                                self.create_flipflop_with_input(value, clock, edge.clone());
                            self.connect_node_to_signal(ff_node, &target);
                        }
                    }
                }
                Statement::ResolvedConditional(resolved) => {
                    // Check if this is an array write (BitSelect with non-constant index)
                    if let LValue::BitSelect { base, index } = &resolved.target {
                        if self.evaluate_constant_expression(index).is_none() {
                            // Dynamic array write with conditional: array[index] <= mux(conds, values)
                            let array_name = self.lvalue_to_string(base);

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
                            let mux_value = self.create_priority_mux_node(&resolved.resolved);
                            let ff_node =
                                self.create_flipflop_with_input(mux_value, clock, edge.clone());
                            self.connect_node_to_signal(ff_node, &target);
                        }
                    } else {
                        // Normal resolved conditional (not array write)
                        let target = self.lvalue_to_string(&resolved.target);

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
                    // Recursively process nested blocks
                    self.convert_sequential_block(&block.statements, clock, edge.clone());
                }
                Statement::If(if_stmt) => {
                    self.convert_if_in_sequential(if_stmt, clock, edge.clone());
                }
                Statement::Case(case_stmt) => {
                    self.convert_case_in_sequential(case_stmt, clock, edge.clone());
                }
                _ => {}
            }
        }
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
        for stmt in statements {
            match stmt {
                Statement::Assignment(assign) => {
                    let target = self.lvalue_to_string(&assign.lhs);
                    if all_targets.contains(&target) {
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

                    if !relevant_targets.is_empty() {
                        let if_stmt_ptr = if_stmt as *const _ as usize;
                        conditional_groups.insert(if_stmt_ptr, relevant_targets);
                    }
                }
                Statement::Block(block) => {
                    self.group_targets_by_conditionals(
                        &block.statements,
                        all_targets,
                        conditional_groups,
                        simple_assignments,
                    );
                }
                _ => {}
            }
        }
    }

    #[allow(dead_code)]
    fn collect_targets_from_if(
        &self,
        if_stmt: &IfStatement,
        targets: &mut std::collections::HashSet<String>,
    ) {
        // Collect from then branch
        for stmt in &if_stmt.then_block.statements {
            match stmt {
                Statement::Assignment(assign) => {
                    let target = self.lvalue_to_string(&assign.lhs);
                    targets.insert(target);
                }
                // BUG #270 FIX: Also collect from ResolvedConditional
                Statement::ResolvedConditional(resolved) => {
                    let target = self.lvalue_to_string(&resolved.target);
                    targets.insert(target);
                }
                _ => {}
            }
        }

        // Collect from else branch
        if let Some(else_block) = &if_stmt.else_block {
            for stmt in &else_block.statements {
                match stmt {
                    Statement::Assignment(assign) => {
                        let target = self.lvalue_to_string(&assign.lhs);
                        targets.insert(target);
                    }
                    // BUG #270 FIX: Also collect from ResolvedConditional
                    Statement::ResolvedConditional(resolved) => {
                        let target = self.lvalue_to_string(&resolved.target);
                        targets.insert(target);
                    }
                    _ => {}
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

        // Process all assignments in order, building up context
        for stmt in statements {
            if let Statement::Assignment(assign) = stmt {
                let assign_target = self.lvalue_to_string(&assign.lhs);

                // Create expression with current local context
                let value = self.create_expression_with_local_context(&assign.rhs, local_context);

                // Store in local context for future references
                local_context.insert(assign_target.clone(), value);

                // If this is one of our targets, save it
                if targets.contains(&assign_target) {
                    target_values.insert(assign_target.clone(), value);
                }
            }
        }

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
                        // BUG #226 FIX: Pass prior assignment value as default
                        // This ensures that unconditional assignments before the if-statement
                        // are used as the default when a branch doesn't assign to the target.
                        result_value = Some(self.synthesize_conditional_assignment_with_default(
                            if_stmt,
                            target,
                            result_value,
                        ));
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
            ExpressionKind::Cast { expr, .. } => self.expression_references_signals(expr, signals),
            _ => false,
        }
    }

    fn get_base_signal_name(&self, lvalue: &LValue) -> String {
        match lvalue {
            LValue::Signal(id) => {
                let mir_name = self
                    .mir
                    .signals
                    .iter()
                    .find(|s| s.id == *id)
                    .map(|s| s.name.clone())
                    .unwrap_or_else(|| format!("signal_{}", id.0));
                // Translate to internal name
                self.translate_to_internal_name(&mir_name)
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
        self.collect_array_writes(if_stmt, &mut array_writes);

        // Create a set of array names that have been processed
        let array_names: std::collections::HashSet<String> =
            array_writes.iter().map(|(_, name)| name.clone()).collect();

        // Handle array writes separately since they need special processing
        self.handle_array_writes_in_if_with_list(if_stmt, clock, edge.clone(), array_writes);

        // Synthesis-like approach: collect all conditions and values, then build priority mux
        let mut targets = std::collections::HashSet::new();
        self.collect_assignment_targets(if_stmt, &mut targets);

        // CRITICAL FIX: Exclude array write targets since they're already handled
        targets.retain(|target| !array_names.contains(target));

        // BUG FIX: Filter out output ports that are NOT state elements
        // Registered outputs (assigned in on(clk.rise)) need FlipFlops, not continuous assignments
        // BUG #249 FIX: Must translate target name to internal name before checking state_elements
        // because state_elements uses internal names (_s123) not MIR names (state_reg)
        targets.retain(|target| {
            let is_output = self.is_output_port(target);
            let internal_target = self.translate_to_internal_name(target);
            let is_state_element = self.sir.state_elements.contains_key(&internal_target);
            // Keep if: not an output, OR is an output that's also a state element (registered)
            !is_output || is_state_element
        });

        // BUG #227 FIX: Filter out variables (let bindings) - they should remain combinational
        // Variables inside sequential blocks are temporary values computed each cycle, not state.
        // We'll handle them separately with continuous assignments, not flip-flops.
        // BUG #249 FIX: Must translate target name to internal name before checking state_elements
        let variable_targets: Vec<String> = targets
            .iter()
            .filter(|target| {
                // Check if this is NOT a state element (variables are never state elements)
                let internal_target = self.translate_to_internal_name(target);
                !self.sir.state_elements.contains_key(&internal_target)
            })
            .cloned()
            .collect();

        // Remove variables from flip-flop creation
        // BUG #249 FIX: Translate to internal name before checking
        targets.retain(|target| {
            let internal_target = self.translate_to_internal_name(target);
            self.sir.state_elements.contains_key(&internal_target)
        });

        // For each target signal, synthesize a priority-encoded mux
        // Sort targets for deterministic ordering
        let mut sorted_targets: Vec<_> = targets.into_iter().collect();
        sorted_targets.sort();
        for target in sorted_targets {
            // BUG #226 FIX: Use sequential default if present
            let default_value = self.sequential_defaults.get(&target).copied();
            let final_value = self.synthesize_conditional_assignment_with_default(
                if_stmt,
                &target,
                default_value,
            );
            let ff_node = self.create_flipflop_with_input(final_value, clock, edge.clone());
            self.connect_node_to_signal(ff_node, &target);
        }

        // BUG #227 FIX: Handle variable targets as combinational assignments
        // Variables (let bindings) inside sequential blocks are just temporary values,
        // not state that needs to be registered.
        //
        // BUG FIX #117b: For variable targets, pass a constant 0 as the default value
        // instead of None. When default=None, synthesize_conditional_assignment_with_default
        // uses create_signal_ref(target) as the "keep" value, which creates a self-referencing
        // combinational loop (signal → mux → signal). This loop doesn't exist in the behavioral
        // model (MIR fast sim), where let-bindings are fresh temporaries each cycle. Using 0
        // as the default breaks the loop without affecting correctness, because:
        // 1. When the variable IS assigned, it gets the correct computed value
        // 2. When NOT assigned, the consuming logic (e.g., int_accum mux) already selects
        //    a different value through its own condition, so the variable's default is unused.
        for target in variable_targets {
            let target_width = self.get_signal_width(&target);
            let zero_default = self.create_constant_node(0, target_width);
            let final_value = self.synthesize_conditional_assignment_with_default(
                if_stmt,
                &target,
                Some(zero_default),
            );
            self.connect_node_to_signal(final_value, &target);
        }
    }

    /// Convert case/match statement in sequential context (event blocks)
    /// BUG #117r FIX: State machines use match statements which need to generate flip-flops
    fn convert_case_in_sequential(
        &mut self,
        case_stmt: &skalp_mir::CaseStatement,
        clock: &str,
        edge: ClockEdge,
    ) {
        // Create expression node for the case expression (e.g., state_reg)
        let case_expr_node = self.create_expression_node(&case_stmt.expr);

        // BUG FIX: Handle nested if statements within case arms properly
        // We need to collect ALL targets assigned in any case arm (including nested if)
        // and build mux trees that combine the case condition with the nested if conditions

        // Step 1: Collect all targets assigned anywhere in the case statement
        let mut all_targets: std::collections::HashSet<String> = std::collections::HashSet::new();

        for item in &case_stmt.items {
            self.collect_all_targets_from_statements(&item.block.statements, &mut all_targets);
        }
        if let Some(default_block) = &case_stmt.default {
            self.collect_all_targets_from_statements(&default_block.statements, &mut all_targets);
        }

        // BUG FIX: Filter out output ports that are NOT state elements
        // Registered outputs (assigned in on(clk.rise)) need FlipFlops, not continuous assignments
        // BUG #249 FIX: Must translate target name to internal name before checking state_elements
        // because state_elements uses internal names (_s123) not MIR names (state_reg)
        all_targets.retain(|target| {
            let is_output = self.is_output_port(target);
            let internal_target = self.translate_to_internal_name(target);
            let is_state_element = self.sir.state_elements.contains_key(&internal_target);
            !is_output || is_state_element
        });

        // Step 2: For each target, build a mux tree that handles all case arms and nested conditions
        for target in &all_targets {
            // BUG #226 FIX: Start with the default from an unconditional assignment if present,
            // otherwise use the current signal value (state holding when no assignment matches)
            let mut current_value = if let Some(&default_val) = self.sequential_defaults.get(target)
            {
                default_val
            } else {
                self.get_or_create_signal_driver(target)
            };

            // Build mux from last case to first (priority order)
            // For each case arm, build the value expression considering nested if statements
            for item in case_stmt.items.iter().rev() {
                // Create case guard: case_expr == value
                let case_guard = if item.values.is_empty() {
                    continue;
                } else if item.values.len() == 1 {
                    let val_node = self.create_expression_node(&item.values[0]);
                    self.create_binary_op_node(
                        &skalp_mir::BinaryOp::Equal,
                        case_expr_node,
                        val_node,
                    )
                } else {
                    let mut or_node = {
                        let val_node = self.create_expression_node(&item.values[0]);
                        self.create_binary_op_node(
                            &skalp_mir::BinaryOp::Equal,
                            case_expr_node,
                            val_node,
                        )
                    };
                    for value in &item.values[1..] {
                        let val_node = self.create_expression_node(value);
                        let eq_node = self.create_binary_op_node(
                            &skalp_mir::BinaryOp::Equal,
                            case_expr_node,
                            val_node,
                        );
                        or_node =
                            self.create_binary_op_node(&skalp_mir::BinaryOp::Or, or_node, eq_node);
                    }
                    or_node
                };

                // Build the value for this target within this case arm
                // This handles nested if statements by creating nested muxes
                let arm_value = self.build_value_from_statements_for_target(
                    &item.block.statements,
                    target,
                    current_value, // fallthrough value if not assigned in this arm
                );

                // MUX: case_guard ? arm_value : current_value
                current_value = self.create_mux_node(case_guard, arm_value, current_value);
            }

            // Handle default case
            if let Some(default_block) = &case_stmt.default {
                // The default should apply when none of the explicit cases match
                // Since we're building from last to first with priority, the default
                // is already represented by the initial current_value for cases that
                // don't match. But if there are explicit assignments in default, we
                // need to handle them.
                let default_value = self.build_value_from_statements_for_target(
                    &default_block.statements,
                    target,
                    current_value,
                );

                // Only update if default has a different value (i.e., there was an assignment)
                if default_value != current_value {
                    // Create condition: none of the explicit cases matched
                    // This is complex, so for now we assume default is the fallthrough
                    // The current implementation already handles this via the initial value
                    current_value = default_value;
                }
            }

            // Create flip-flop with the final mux tree
            let ff_node = self.create_flipflop_with_input(current_value, clock, edge.clone());
            self.connect_node_to_signal(ff_node, target);
        }
    }

    /// Collect all signal targets assigned anywhere in a list of statements
    /// (including nested if statements, blocks, etc.)
    fn collect_all_targets_from_statements(
        &self,
        statements: &[Statement],
        targets: &mut std::collections::HashSet<String>,
    ) {
        for stmt in statements {
            match stmt {
                Statement::Assignment(assign) => {
                    let target = self.lvalue_to_string(&assign.lhs);
                    targets.insert(target);
                }
                Statement::If(if_stmt) => {
                    self.collect_all_targets_from_statements(
                        &if_stmt.then_block.statements,
                        targets,
                    );
                    if let Some(else_block) = &if_stmt.else_block {
                        self.collect_all_targets_from_statements(&else_block.statements, targets);
                    }
                }
                Statement::Block(block) => {
                    self.collect_all_targets_from_statements(&block.statements, targets);
                }
                Statement::Case(case) => {
                    for item in &case.items {
                        self.collect_all_targets_from_statements(&item.block.statements, targets);
                    }
                    if let Some(default_block) = &case.default {
                        self.collect_all_targets_from_statements(
                            &default_block.statements,
                            targets,
                        );
                    }
                }
                Statement::ResolvedConditional(resolved) => {
                    // BUG #270 FIX: Collect target from ResolvedConditional
                    let target = self.lvalue_to_string(&resolved.target);
                    targets.insert(target);
                }
                _ => {}
            }
        }
    }

    /// Build the value expression for a target signal from a list of statements
    /// This recursively handles nested if statements by building mux trees
    fn build_value_from_statements_for_target(
        &mut self,
        statements: &[Statement],
        target: &str,
        fallback_value: usize,
    ) -> usize {
        let mut current_value = fallback_value;

        for stmt in statements {
            match stmt {
                Statement::Assignment(assign) => {
                    let assign_target = self.lvalue_to_string(&assign.lhs);
                    if assign_target == target {
                        // Direct assignment to our target
                        current_value = self.create_expression_node(&assign.rhs);
                    }
                }
                Statement::If(if_stmt) => {
                    // Build condition node
                    let condition = self.create_expression_node(&if_stmt.condition);

                    // Recursively build then branch value
                    let then_value = self.build_value_from_statements_for_target(
                        &if_stmt.then_block.statements,
                        target,
                        current_value,
                    );

                    // Recursively build else branch value
                    let else_value = if let Some(else_block) = &if_stmt.else_block {
                        self.build_value_from_statements_for_target(
                            &else_block.statements,
                            target,
                            current_value,
                        )
                    } else {
                        current_value
                    };

                    // Create mux: condition ? then_value : else_value
                    current_value = self.create_mux_node(condition, then_value, else_value);
                }
                Statement::Block(block) => {
                    // Recurse into nested blocks
                    current_value = self.build_value_from_statements_for_target(
                        &block.statements,
                        target,
                        current_value,
                    );
                }
                Statement::Case(case_stmt) => {
                    // BUG #233 FIX: Handle Case/match statements in value building
                    // Build a cascaded mux for each case arm
                    let selector = self.create_expression_node(&case_stmt.expr);

                    // Start with default value (current_value or default block)
                    let default_value = if let Some(default_block) = &case_stmt.default {
                        self.build_value_from_statements_for_target(
                            &default_block.statements,
                            target,
                            current_value,
                        )
                    } else {
                        current_value
                    };

                    // Build cascaded mux: for each arm, check if selector matches pattern
                    // The result is: arm1_match ? arm1_val : (arm2_match ? arm2_val : ... : default)
                    let mut result = default_value;

                    // Process arms in reverse order to build the mux chain correctly
                    for item in case_stmt.items.iter().rev() {
                        // Get the value for this arm
                        let arm_value = self.build_value_from_statements_for_target(
                            &item.block.statements,
                            target,
                            current_value, // Each arm starts with current_value as fallback
                        );

                        // Build condition: selector == pattern
                        // Handle multiple patterns with OR
                        let mut arm_condition: Option<usize> = None;
                        for value in &item.values {
                            let pattern_node = self.create_expression_node(value);
                            let eq_node = self.create_binary_op_node(
                                &skalp_mir::BinaryOp::Equal,
                                selector,
                                pattern_node,
                            );
                            arm_condition = Some(match arm_condition {
                                Some(prev) => self.create_binary_op_node(
                                    &skalp_mir::BinaryOp::Or,
                                    prev,
                                    eq_node,
                                ),
                                None => eq_node,
                            });
                        }

                        if let Some(cond) = arm_condition {
                            result = self.create_mux_node(cond, arm_value, result);
                        }
                    }

                    current_value = result;
                }
                Statement::ResolvedConditional(resolved) => {
                    // BUG #270 FIX: Handle ResolvedConditional in value building
                    let resolved_target = self.lvalue_to_string(&resolved.target);
                    if resolved_target == target {
                        let if_stmt = &resolved.original;
                        let condition = self.create_expression_node(&if_stmt.condition);
                        let then_value = self.build_value_from_statements_for_target(
                            &if_stmt.then_block.statements,
                            target,
                            current_value,
                        );
                        let else_value = if let Some(else_block) = &if_stmt.else_block {
                            self.build_value_from_statements_for_target(
                                &else_block.statements,
                                target,
                                current_value,
                            )
                        } else {
                            current_value
                        };
                        current_value = self.create_mux_node(condition, then_value, else_value);
                    }
                }
                _ => {
                    // Skip other statement types for now
                }
            }
        }

        current_value
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
        self.scan_block_for_array_writes(&if_stmt.then_block.statements, array_writes);

        // Check else branch
        if let Some(else_block) = &if_stmt.else_block {
            self.scan_block_for_array_writes(&else_block.statements, array_writes);
        }
    }

    fn scan_block_for_array_writes(
        &self,
        statements: &[Statement],
        array_writes: &mut Vec<(LValue, String)>,
    ) {
        for stmt in statements.iter() {
            match stmt {
                Statement::Assignment(assign) => {
                    if let LValue::BitSelect { base, index } = &assign.lhs {
                        let is_const = self.evaluate_constant_expression(index);
                        let base_name = self.lvalue_to_string(base);
                        if is_const.is_none() {
                            // Dynamic array write
                            // Only add if not already present
                            if !array_writes.iter().any(|(_, name)| name == &base_name) {
                                array_writes.push(((**base).clone(), base_name));
                            }
                        }
                    }
                }
                Statement::If(nested_if) => {
                    self.collect_array_writes(nested_if, array_writes);
                }
                Statement::Block(block) => {
                    self.scan_block_for_array_writes(&block.statements, array_writes);
                }
                Statement::ResolvedConditional(resolved) => {
                    if let LValue::BitSelect { base, index } = &resolved.target {
                        let is_const = self.evaluate_constant_expression(index);
                        let base_name = self.lvalue_to_string(base);
                        if is_const.is_none()
                            && !array_writes.iter().any(|(_, name)| name == &base_name)
                        {
                            array_writes.push(((**base).clone(), base_name));
                        }
                    }
                }
                _ => {}
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
                Statement::Case(case_stmt) => {
                    // BUG #117r FIX: Recurse into case/match statements
                    // Collect targets from all case arms
                    for item in &case_stmt.items {
                        self.collect_targets_from_block(&item.block.statements, targets);
                    }
                    // Collect targets from default case
                    if let Some(default_block) = &case_stmt.default {
                        self.collect_targets_from_block(&default_block.statements, targets);
                    }
                }
                Statement::ResolvedConditional(resolved) => {
                    // BUG #270 FIX: Collect target from ResolvedConditional
                    let target = self.lvalue_to_string(&resolved.target);
                    targets.insert(target);
                }
                _ => {}
            }
        }
    }

    fn is_output_port(&self, signal_name: &str) -> bool {
        // Check if this is an output port that should only have continuous assignments
        // BUG FIX: signal_name is the internal name (e.g., "_s16"), but port.name is the MIR name (e.g., "state")
        // We need to translate the port name to internal name for comparison
        self.mir.ports.iter().any(|port| {
            let port_internal_name = self.translate_to_internal_name(&port.name);
            port_internal_name == signal_name
                && matches!(port.direction, skalp_mir::PortDirection::Output)
        })
    }

    fn synthesize_conditional_assignment(&mut self, if_stmt: &IfStatement, target: &str) -> usize {
        // Use dependency-aware processing for conditional assignments
        let mut cases = Vec::new();

        // Process then branch with local context
        let then_value =
            self.process_branch_with_dependencies(&if_stmt.then_block.statements, target);
        let else_value = if let Some(else_block) = &if_stmt.else_block {
            self.process_branch_with_dependencies(&else_block.statements, target)
        } else {
            None
        };

        // Build cases based on what we found
        match (then_value, else_value) {
            (Some(then_val), Some(else_val)) => {
                // Both branches assign: create proper mux
                let condition = self.create_expression_node(&if_stmt.condition);
                cases.push((condition, then_val));
                cases.push((0, else_val));
            }
            (Some(then_val), None) => {
                // Only then assigns: mux(cond, then_val, keep_current)
                let condition = self.create_expression_node(&if_stmt.condition);
                let keep_val = self.create_signal_ref(target);
                cases.push((condition, then_val));
                cases.push((0, keep_val));
            }
            (None, Some(else_val)) => {
                // Only else assigns: mux(cond, keep_current, else_val)
                let condition = self.create_expression_node(&if_stmt.condition);
                let keep_val = self.create_signal_ref(target);
                cases.push((condition, keep_val));
                cases.push((0, else_val));
            }
            (None, None) => {
                // Neither assigns: keep current value
            }
        }

        // Build priority-encoded mux tree
        if cases.is_empty() {
            // No assignments to this target, use current value
            self.create_signal_ref(target)
        } else {
            self.build_priority_mux(&cases, target)
        }
    }

    /// BUG #222 FIX: Synthesize conditional assignment with an explicit default value
    /// This is used when a direct assignment precedes the conditional in the same branch.
    fn synthesize_conditional_assignment_with_default(
        &mut self,
        if_stmt: &IfStatement,
        target: &str,
        default_value: Option<usize>,
    ) -> usize {
        let mut cases = Vec::new();

        // BUG #226 FIX: Pass default_value to branch processing so nested ifs can use it
        let then_value = self.process_branch_with_dependencies_and_default(
            &if_stmt.then_block.statements,
            target,
            default_value,
        );
        let else_value = if let Some(else_block) = &if_stmt.else_block {
            self.process_branch_with_dependencies_and_default(
                &else_block.statements,
                target,
                default_value,
            )
        } else {
            None
        };

        // BUG #222 FIX: Use provided default when neither branch assigns
        let keep_value = match default_value {
            Some(default) => default,
            None => self.create_signal_ref(target),
        };

        // Build cases based on what we found
        match (then_value, else_value) {
            (Some(then_val), Some(else_val)) => {
                // Both branches assign: create proper mux
                let condition = self.create_expression_node(&if_stmt.condition);
                cases.push((condition, then_val));
                cases.push((0, else_val));
            }
            (Some(then_val), None) => {
                // Only then assigns: mux(cond, then_val, keep_current or default)
                let condition = self.create_expression_node(&if_stmt.condition);
                cases.push((condition, then_val));
                cases.push((0, keep_value));
            }
            (None, Some(else_val)) => {
                // Only else assigns: mux(cond, keep_current or default, else_val)
                let condition = self.create_expression_node(&if_stmt.condition);
                cases.push((condition, keep_value));
                cases.push((0, else_val));
            }
            (None, None) => {
                // Neither assigns: return the default/keep value
                return keep_value;
            }
        }

        // Build priority-encoded mux tree
        if cases.is_empty() {
            keep_value
        } else {
            self.build_priority_mux(&cases, target)
        }
    }

    /// Synthesize a mux tree for a specific target within a case statement
    /// BUG #117r FIX: Handles match statements for state machine transitions
    #[allow(dead_code)]
    fn synthesize_case_for_target(
        &mut self,
        case_stmt: &skalp_mir::CaseStatement,
        target: &str,
    ) -> Option<usize> {
        // Create expression node for the case expression (e.g., state_reg)
        let case_expr_node = self.create_expression_node(&case_stmt.expr);

        // Collect values and expressions for this target from all case arms
        let mut case_arms: Vec<(Vec<&Expression>, Option<usize>)> = Vec::new();
        let mut found_target = false;

        for item in case_stmt.items.iter() {
            // Check if this arm assigns to our target
            let arm_value = self.find_target_in_block(&item.block.statements, target);
            if arm_value.is_some() {
                found_target = true;
            }
            case_arms.push((item.values.iter().collect(), arm_value));
        }

        // Get default value
        let default_value = if let Some(default_block) = &case_stmt.default {
            let val = self.find_target_in_block(&default_block.statements, target);
            if val.is_some() {
                found_target = true;
            }
            val
        } else {
            None
        };

        // If no arm assigns to this target, return None
        if !found_target {
            return None;
        }

        // Build the mux tree
        // Start with default value (or current signal value)
        let mut current_mux = match default_value {
            Some(val) => val,
            None => self.create_signal_ref(target),
        };

        // Build mux chain from last case to first (so first has priority)
        for (values, arm_value) in case_arms.iter().rev() {
            // Skip arms that don't assign to this target
            let value_node = match arm_value {
                Some(val) => *val,
                None => current_mux, // Keep previous value
            };

            // Create condition: case_expr == value
            if values.is_empty() {
                continue;
            }

            let condition_node = if values.len() == 1 {
                let val_node = self.create_expression_node(values[0]);
                self.create_binary_op_node(&skalp_mir::BinaryOp::Equal, case_expr_node, val_node)
            } else {
                // Multiple values: (case_expr == v1) | (case_expr == v2) | ...
                let mut or_node = {
                    let val_node = self.create_expression_node(values[0]);
                    self.create_binary_op_node(
                        &skalp_mir::BinaryOp::Equal,
                        case_expr_node,
                        val_node,
                    )
                };
                for value in &values[1..] {
                    let val_node = self.create_expression_node(value);
                    let eq_node = self.create_binary_op_node(
                        &skalp_mir::BinaryOp::Equal,
                        case_expr_node,
                        val_node,
                    );
                    or_node =
                        self.create_binary_op_node(&skalp_mir::BinaryOp::Or, or_node, eq_node);
                }
                or_node
            };

            // Create mux: condition ? value_node : current_mux
            current_mux = self.create_mux_node(condition_node, value_node, current_mux);
        }

        Some(current_mux)
    }

    /// BUG #222 FIX: Synthesize case with an explicit default value
    /// This is used when a direct assignment precedes the case statement.
    fn synthesize_case_for_target_with_default(
        &mut self,
        case_stmt: &skalp_mir::CaseStatement,
        target: &str,
        default_value: Option<usize>,
    ) -> Option<usize> {
        // Create expression node for the case expression (e.g., state_reg)
        let case_expr_node = self.create_expression_node(&case_stmt.expr);

        // Collect values and expressions for this target from all case arms
        let mut case_arms: Vec<(Vec<&Expression>, Option<usize>)> = Vec::new();
        let mut found_target = false;

        for item in case_stmt.items.iter() {
            // BUG #226 FIX: Use find_target_in_block_with_default to properly handle nested conditionals
            // Pass the default value so nested ifs can use it as the "keep" value
            let arm_value = self.find_target_in_block_with_default(
                &item.block.statements,
                target,
                default_value,
            );
            if arm_value.is_some() {
                found_target = true;
            }
            case_arms.push((item.values.iter().collect(), arm_value));
        }

        // Get default block value
        let default_block_value = if let Some(default_block) = &case_stmt.default {
            let val = self.find_target_in_block_with_default(
                &default_block.statements,
                target,
                default_value,
            );
            if val.is_some() {
                found_target = true;
            }
            val
        } else {
            None
        };

        // If no arm assigns to this target, return None
        if !found_target {
            return None;
        }

        // BUG #222 FIX: Use provided default, or the default block value, or signal ref
        let mut current_mux = match (default_block_value, default_value) {
            (Some(block_val), _) => block_val, // Default block has assignment
            (None, Some(provided_default)) => provided_default, // Use provided default
            (None, None) => self.create_signal_ref(target), // Keep current value
        };

        // Build mux chain from last case to first (so first has priority)
        for (values, arm_value) in case_arms.iter().rev() {
            let value_node = match arm_value {
                Some(val) => *val,
                None => current_mux,
            };

            if values.is_empty() {
                continue;
            }

            let condition_node = if values.len() == 1 {
                let val_node = self.create_expression_node(values[0]);
                self.create_binary_op_node(&skalp_mir::BinaryOp::Equal, case_expr_node, val_node)
            } else {
                let mut or_node = {
                    let val_node = self.create_expression_node(values[0]);
                    self.create_binary_op_node(
                        &skalp_mir::BinaryOp::Equal,
                        case_expr_node,
                        val_node,
                    )
                };
                for value in &values[1..] {
                    let val_node = self.create_expression_node(value);
                    let eq_node = self.create_binary_op_node(
                        &skalp_mir::BinaryOp::Equal,
                        case_expr_node,
                        val_node,
                    );
                    or_node =
                        self.create_binary_op_node(&skalp_mir::BinaryOp::Or, or_node, eq_node);
                }
                or_node
            };

            current_mux = self.create_mux_node(condition_node, value_node, current_mux);
        }

        Some(current_mux)
    }

    /// Find assignment to a specific target in a block
    #[allow(dead_code)]
    fn find_target_in_block(&mut self, statements: &[Statement], target: &str) -> Option<usize> {
        for stmt in statements.iter() {
            match stmt {
                Statement::Assignment(assign) => {
                    let assign_target = self.lvalue_to_string(&assign.lhs);
                    if assign_target == target {
                        return Some(self.create_expression_node(&assign.rhs));
                    }
                }
                Statement::Block(block) => {
                    if let Some(val) = self.find_target_in_block(&block.statements, target) {
                        return Some(val);
                    }
                }
                Statement::If(nested_if) => {
                    // Recurse into nested if - synthesize it as a conditional
                    let result = self.synthesize_conditional_assignment(nested_if, target);
                    // Check if the result is not just a keep-value SignalRef
                    let is_keep_value = self.sir.combinational_nodes.iter().any(|n| {
                        n.id == result
                            && matches!(n.kind, SirNodeKind::SignalRef { ref signal } if signal == target)
                    });
                    if !is_keep_value {
                        return Some(result);
                    }
                }
                Statement::Case(nested_case) => {
                    if let Some(val) = self.synthesize_case_for_target(nested_case, target) {
                        return Some(val);
                    }
                }
                Statement::ResolvedConditional(resolved) => {
                    // BUG #270 FIX: Handle ResolvedConditional
                    let resolved_target = self.lvalue_to_string(&resolved.target);
                    if resolved_target == target {
                        let result =
                            self.synthesize_conditional_assignment(&resolved.original, target);
                        let is_keep_value = self.sir.combinational_nodes.iter().any(|n| {
                            n.id == result
                                && matches!(n.kind, SirNodeKind::SignalRef { ref signal } if signal == target)
                        });
                        if !is_keep_value {
                            return Some(result);
                        }
                    }
                }
                _ => {}
            }
        }
        None
    }

    /// BUG #226 FIX: Find assignment to a specific target in a block, with a default value
    /// for nested conditionals that don't assign in all branches.
    fn find_target_in_block_with_default(
        &mut self,
        statements: &[Statement],
        target: &str,
        default_value: Option<usize>,
    ) -> Option<usize> {
        for stmt in statements.iter() {
            match stmt {
                Statement::Assignment(assign) => {
                    let assign_target = self.lvalue_to_string(&assign.lhs);
                    if assign_target == target {
                        return Some(self.create_expression_node(&assign.rhs));
                    }
                }
                Statement::Block(block) => {
                    if let Some(val) = self.find_target_in_block_with_default(
                        &block.statements,
                        target,
                        default_value,
                    ) {
                        return Some(val);
                    }
                }
                Statement::If(nested_if) => {
                    // BUG #226 FIX: Pass the default value to nested conditionals
                    let result = self.synthesize_conditional_assignment_with_default(
                        nested_if,
                        target,
                        default_value,
                    );
                    // Check if the result is not just a keep-value SignalRef
                    let is_keep_value = self.sir.combinational_nodes.iter().any(|n| {
                        n.id == result
                            && matches!(n.kind, SirNodeKind::SignalRef { ref signal } if signal == target)
                    });
                    if !is_keep_value {
                        return Some(result);
                    }
                }
                Statement::Case(nested_case) => {
                    // BUG #226 FIX: Pass the default value to nested case statements
                    if let Some(val) = self.synthesize_case_for_target_with_default(
                        nested_case,
                        target,
                        default_value,
                    ) {
                        return Some(val);
                    }
                }
                Statement::ResolvedConditional(resolved) => {
                    // BUG #270 FIX: Handle ResolvedConditional with default
                    let resolved_target = self.lvalue_to_string(&resolved.target);
                    if resolved_target == target {
                        let result = self.synthesize_conditional_assignment_with_default(
                            &resolved.original,
                            target,
                            default_value,
                        );
                        let is_keep_value = self.sir.combinational_nodes.iter().any(|n| {
                            n.id == result
                                && matches!(n.kind, SirNodeKind::SignalRef { ref signal } if signal == target)
                        });
                        if !is_keep_value {
                            return Some(result);
                        }
                    }
                }
                _ => {}
            }
        }
        None
    }

    fn process_branch_with_dependencies(
        &mut self,
        statements: &[Statement],
        target: &str,
    ) -> Option<usize> {
        // BUG #226 FIX: Forward to the version with initial default = None
        self.process_branch_with_dependencies_and_default(statements, target, None)
    }

    /// BUG #226 FIX: Process a branch with an initial default value that can be overridden
    fn process_branch_with_dependencies_and_default(
        &mut self,
        statements: &[Statement],
        target: &str,
        initial_default: Option<usize>,
    ) -> Option<usize> {
        // BUG #222 FIX: Sequential assignment order handling
        //
        // In sequential blocks (on clk.rise), the semantics are:
        // 1. All RHS values are sampled at the beginning of the clock cycle
        // 2. Later assignments OVERRIDE earlier ones (last-write-wins)
        //
        // Example:
        //   state_timer = state_timer + 1  // Default: increment
        //   if enable && !fault {
        //       state_timer = 0            // Override: reset when condition met
        //   }
        //
        // Should produce: (enable && !fault) ? 0 : (state_timer + 1)
        //
        // The fix: Track a "current default" value. When we see conditional
        // statements that might override, build muxes that use the default
        // when conditions aren't met.

        // BUG #226 FIX: Start with the initial default from the outer context
        let mut current_default: Option<usize> = initial_default;
        let mut conditional_results = Vec::new();
        // BUG FIX #117c: Track whether this branch actually assigns to target
        // (vs. just inheriting the initial_default from the parent context).
        // This prevents the caller from treating inherited defaults as actual assignments,
        // which would create unnecessary mux nodes with self-referencing conditions.
        let mut has_direct_assignment = false;

        for stmt in statements {
            match stmt {
                Statement::Assignment(assign) => {
                    let assign_target = self.lvalue_to_string(&assign.lhs);

                    // BUG #222 FIX: Direct assignment sets the default, but DON'T return early!
                    // Later conditionals can still override this.
                    if assign_target == target {
                        let value = self.create_expression_node(&assign.rhs);
                        current_default = Some(value);
                        has_direct_assignment = true; // BUG FIX #117c
                    }
                }
                Statement::If(nested_if) => {
                    // Handle nested if/else-if chains
                    // BUG #222 FIX: Pass current_default to use as the "keep" value
                    let nested_result = self.synthesize_conditional_assignment_with_default(
                        nested_if,
                        target,
                        current_default,
                    );
                    conditional_results.push(nested_result);

                    // BUG #235 FIX: Update current_default for subsequent if statements
                    // When there are multiple independent if statements for the same target,
                    // each subsequent if should use the previous if's result as its default.
                    // This ensures proper "last-write-wins" semantics:
                    //   if cond1 { target = val1 }  // produces result1
                    //   if cond2 { target = val2 }  // should use result1 as default, not original target
                    // Final value: cond2 ? val2 : (cond1 ? val1 : original_target)
                    current_default = Some(nested_result);
                }
                Statement::Block(block) => {
                    // CRITICAL FIX: Recurse into nested blocks (same bug as Bug #19)
                    // Expanded array assignments are wrapped in Block statements
                    if let Some(result) =
                        self.process_branch_with_dependencies(&block.statements, target)
                    {
                        // BUG #222 FIX: Don't return early - update current_default instead
                        current_default = Some(result);
                        has_direct_assignment = true; // Block contains real assignment to target
                    }
                }
                Statement::Case(case_stmt) => {
                    // BUG #117r FIX: Handle case/match statements in sequential branches
                    // BUG #222 FIX: Pass current_default to case synthesis
                    if let Some(result) = self.synthesize_case_for_target_with_default(
                        case_stmt,
                        target,
                        current_default,
                    ) {
                        // Case statement assigned to target - use as new result
                        conditional_results.push(result);
                    }
                }
                Statement::ResolvedConditional(resolved) => {
                    // BUG #270 FIX: Handle ResolvedConditional (single-target optimized if-else-if)
                    // Check if this resolved conditional targets our signal
                    let resolved_target = self.lvalue_to_string(&resolved.target);
                    if resolved_target == target {
                        // Use the original IfStatement to synthesize the conditional
                        let nested_result = self.synthesize_conditional_assignment_with_default(
                            &resolved.original,
                            target,
                            current_default,
                        );
                        conditional_results.push(nested_result);
                        current_default = Some(nested_result);
                        has_direct_assignment = true;
                    }
                }
                _ => {}
            }
        }

        // BUG #222 FIX: Combine all results
        // - If we have conditional results, use the last one (which should have incorporated
        //   the default as its "keep" value)
        // - Otherwise, use the direct assignment default
        // - If neither, return None (no assignment to target in this branch)

        // Filter out "keep value" signal refs (conditions that don't actually assign)
        let meaningful_results: Vec<usize> = conditional_results.into_iter()
            .filter(|&node_id| {
                // Check if this node is just a SignalRef to our target
                let is_keep_value = self.sir.combinational_nodes.iter().any(|n| {
                    n.id == node_id && matches!(n.kind, SirNodeKind::SignalRef { ref signal } if signal == target)
                });
                !is_keep_value
            })
            .collect();

        if !meaningful_results.is_empty() {
            // Use the last meaningful conditional result (which incorporates the default)
            Some(*meaningful_results.last().unwrap())
        } else if has_direct_assignment && current_default.is_some() {
            // BUG FIX #117c: Only return current_default if THIS branch actually assigned
            // to the target. If current_default was just inherited from initial_default
            // (no direct assignment), return None so the caller doesn't create a spurious
            // mux with the same value on both branches, which can introduce self-referencing
            // combinational loops for variable targets.
            current_default
        } else {
            None
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
                    computed_value
                } else {
                    // Check for range selects of locally computed values
                    let base_signal = self.get_base_signal_name(lvalue);
                    if let Some(&base_value) = local_context.get(&base_signal) {
                        // Create range select on the computed value
                        self.create_range_select_on_node(lvalue, base_value)
                    } else {
                        // Fall back to register reference
                        self.create_lvalue_ref_node(lvalue)
                    }
                }
            }
            ExpressionKind::Binary { op, left, right } => {
                let left_node = self.create_expression_with_local_context(left, local_context);
                let right_node = self.create_expression_with_local_context(right, local_context);
                // BUG FIX #247: Check signedness for signed arithmetic/comparison operations
                let left_is_signed = self.is_expression_signed(left);
                let right_is_signed = self.is_expression_signed(right);
                let is_signed = left_is_signed || right_is_signed;
                self.create_binary_op_node_with_signedness(op, left_node, right_node, is_signed)
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
                let part_nodes: Vec<usize> = exprs
                    .iter()
                    .map(|e| self.create_expression_with_local_context(e, local_context))
                    .collect();
                self.create_concat_node(part_nodes)
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
            ExpressionKind::Cast {
                expr: inner_expr,
                target_type,
            } => {
                // BUG #245 FIX: Widening casts need sign/zero extension
                let target_width = Self::datatype_to_width(target_type);

                // Create node for the inner expression first
                let inner_node =
                    self.create_expression_with_local_context(inner_expr, local_context);

                // Get actual width from the created node, not from type annotation (which can be Unknown)
                let source_width = self.get_node_output_width(inner_node);

                if target_width > source_width {
                    // Widening cast - need to extend
                    let extend_bits = target_width - source_width;
                    // BUG FIX: Only sign-extend if the SOURCE is signed
                    let is_signed = self.is_expression_signed(inner_expr);

                    if is_signed {
                        // Sign extension: replicate MSB (sign bit) for the extension
                        // Create a slice to extract the MSB (sign bit)
                        let msb_index = source_width - 1;
                        let sign_bit_node =
                            self.create_slice_node(inner_node, msb_index, msb_index);

                        // Create a constant with all 1s for the extension width (used with MUX)
                        let ones_constant =
                            self.create_constant_node((1u64 << extend_bits) - 1, extend_bits);
                        // Create a constant with all 0s for the extension width
                        let zeros_constant = self.create_constant_node(0, extend_bits);

                        // Create a MUX: if sign_bit == 1 then extend with 1s else extend with 0s
                        let sign_extend_node =
                            self.create_mux_node(sign_bit_node, ones_constant, zeros_constant);

                        // Concatenate: [sign_extension, original_value]
                        // In HDL, concat is MSB-first, so extension bits go first
                        self.create_concat_node(vec![sign_extend_node, inner_node])
                    } else {
                        // Zero extension: pad with zeros
                        let zeros_node = self.create_constant_node(0, extend_bits);
                        // Concatenate: [zeros, original_value]
                        self.create_concat_node(vec![zeros_node, inner_node])
                    }
                } else if target_width < source_width {
                    // Narrowing cast - truncate by slicing
                    self.create_slice_node(inner_node, target_width - 1, 0)
                } else {
                    // Same width - just return the inner expression (bitwise reinterpretation)
                    inner_node
                }
            }
            // BUG FIX #85: Handle tuple field access for module synthesis
            ExpressionKind::TupleFieldAccess { base, index } => {
                // Check if the base is a Signal reference (module instance result)
                if let ExpressionKind::Ref(LValue::Signal(sig_id)) = &base.kind {
                    // First, check if this signal is in our tuple source mapping
                    // This handles _tuple_tmp signals that are assigned from module results
                    if let Some(inst_prefix) = self.tuple_source_map.get(sig_id) {
                        let target_signal_name = format!("{}_result_{}", inst_prefix, index);
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
                // BUG #220 FIX: Properly resolve FieldAccess to flattened signal names
                if let Some(node_id) = self.resolve_field_access_to_signal(base, field) {
                    node_id
                } else {
                    // Fallback to base expression if resolution fails
                    self.create_expression_with_local_context(base, local_context)
                }
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
    fn create_range_select_node_on_base(
        &mut self,
        base_node: usize,
        high: usize,
        low: usize,
    ) -> usize {
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
                // BUG FIX #247: Check signedness for signed arithmetic/comparison operations
                let left_is_signed = self.is_expression_signed(left);
                let right_is_signed = self.is_expression_signed(right);
                let is_signed = left_is_signed || right_is_signed;
                self.create_binary_op_node_with_signedness(op, left_node, right_node, is_signed)
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
        // Translate MIR name to internal name for SignalRef creation
        let internal_name = self.translate_to_internal_name(signal_name);

        // Create a signal reference node that reads the current value of the signal
        let node_id = self.next_node_id();
        let signal_id = format!("node_{}_out", node_id);

        let sir_node = SirNode {
            id: node_id,
            kind: SirNodeKind::SignalRef {
                signal: internal_name.clone(),
            },
            inputs: vec![SignalRef {
                signal_id: internal_name.clone(),
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
        let sir_type = self.get_signal_type(&internal_name);
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

    /// BUG #117r FIX PART 3: Create SignalRef for instance context, resolving output ports
    /// through port_mapping to parent signals.
    ///
    /// When target is a hierarchical path like "pwm_gen.primary_bridge.leg_a.gate_high",
    /// we need to:
    /// 1. Extract the local port name (gate_high)
    /// 2. Check if it's an output port in port_mapping
    /// 3. If so, resolve to the parent signal name
    /// 4. Create SignalRef with the resolved internal name
    fn create_signal_ref_for_instance(
        &mut self,
        target: &str,
        inst_prefix: &str,
        port_mapping: &HashMap<String, Expression>,
        parent_module_for_signals: Option<&Module>,
        parent_prefix: &str,
    ) -> usize {
        // Extract the local signal/port name from the hierarchical path
        // e.g., "pwm_gen.primary_bridge.leg_a.gate_high" -> "gate_high"
        let local_name = if target.starts_with(inst_prefix) && !inst_prefix.is_empty() {
            target
                .strip_prefix(inst_prefix)
                .and_then(|s| s.strip_prefix('.'))
                .unwrap_or(target)
        } else {
            target
        };

        // Check if this local name is an output port in port_mapping
        if let Some(parent_expr) = port_mapping.get(local_name) {
            // Output port connected to parent signal - resolve to parent
            let resolved_name = self.get_signal_name_from_expression_with_context(
                parent_expr,
                parent_module_for_signals,
                parent_prefix,
            );
            let internal_name = self.translate_to_internal_name(&resolved_name);
            return self.create_signal_ref(&internal_name);
        }

        // Not an output port in port_mapping - use standard path
        self.create_signal_ref(target)
    }

    fn convert_continuous_assign(&mut self, target: &str, value: &Expression) {
        // Get target signal width to propagate to expression tree
        let target_width = self.get_signal_width(target);
        let node_id = self.create_expression_node_with_width(value, Some(target_width));

        // BUG FIX: Truncate expression result if it's wider than target
        // This is needed because operations like multiplication produce wider results,
        // and even after shifts, the SIR doesn't automatically narrow the width.
        // The result must be truncated to match the declared target type.
        let expr_width = self.get_node_output_width(node_id);
        let final_node_id = if expr_width > target_width && target_width > 0 {
            self.create_slice_node(node_id, target_width - 1, 0)
        } else {
            node_id
        };

        self.connect_node_to_signal(final_node_id, target);
    }

    /// Translate a MIR signal/port name to its internal name in SIR
    /// Falls back to the MIR name if no mapping exists (e.g., for newly created signals)
    fn translate_to_internal_name(&self, mir_name: &str) -> String {
        self.mir_to_internal_name
            .get(mir_name)
            .cloned()
            .unwrap_or_else(|| mir_name.to_string())
    }

    fn lvalue_to_string(&self, lvalue: &LValue) -> String {
        match lvalue {
            LValue::Port(port_id) => {
                // BUG #233 FIX: Translate port names to internal names
                // Previously, this returned the MIR port name without translation,
                // causing target comparisons to fail when searching for internal names.

                // First try current module's ports
                if let Some(port) = self.mir.ports.iter().find(|p| p.id == *port_id) {
                    return self.translate_to_internal_name(&port.name);
                }

                // BUG #233 FIX: Search child modules for the port (same as signal handling)
                for child_module in &self.mir_design.modules {
                    if let Some(port) = child_module.ports.iter().find(|p| p.id == *port_id) {
                        // Find instance of this module in current module
                        for instance in &self.mir.instances {
                            if instance.module == child_module.id {
                                let hier_name = format!("{}.{}", instance.name, port.name);
                                return self.translate_to_internal_name(&hier_name);
                            }
                        }
                        // No instance found, return translated port name
                        return self.translate_to_internal_name(&port.name);
                    }
                }

                // Not found anywhere - use fallback (this shouldn't happen)
                format!("port_{}", port_id.0)
            }
            LValue::Signal(sig_id) => {
                // First try current module
                if let Some(signal) = self.mir.signals.iter().find(|s| s.id == *sig_id) {
                    return self.translate_to_internal_name(&signal.name);
                }

                // BUG #209 FIX: Search child modules for the signal
                for child_module in &self.mir_design.modules {
                    if let Some(signal) = child_module.signals.iter().find(|s| s.id == *sig_id) {
                        // Find instance of this module in current module
                        for instance in &self.mir.instances {
                            if instance.module == child_module.id {
                                let hier_name = format!("{}.{}", instance.name, signal.name);
                                return self.translate_to_internal_name(&hier_name);
                            }
                        }
                        // No instance found, return translated signal name
                        return self.translate_to_internal_name(&signal.name);
                    }
                }

                // Not found anywhere - use fallback
                format!("signal_{}", sig_id.0)
            }
            LValue::Variable(var_id) => {
                let mir_name = self
                    .mir
                    .variables
                    .iter()
                    .find(|v| v.id == *var_id)
                    .map(|v| format!("{}_{}", v.name, var_id.0)) // BUG FIX #86: Unique name
                    .unwrap_or_else(|| format!("var_{}", var_id.0));
                self.translate_to_internal_name(&mir_name)
            }
            LValue::BitSelect { base, .. } => {
                // For bit select, use the base signal name (already translated)
                self.lvalue_to_string(base)
            }
            LValue::RangeSelect { base, .. } => {
                // For range select, use the base signal name (already translated)
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
                let total: usize = elements.iter().filter_map(Self::type_to_width).sum();
                if total > 0 {
                    Some(total)
                } else {
                    None
                }
            }
            Type::Array { element_type, size } => {
                Self::type_to_width(element_type).map(|w| w * (*size as usize))
            }
            _ => None,
        }
    }

    /// BUG #245 FIX: Get width from MIR DataType for cast handling
    fn datatype_to_width(dt: &DataType) -> usize {
        match dt {
            DataType::Bit(w) | DataType::Logic(w) | DataType::Int(w) | DataType::Nat(w) => *w,
            DataType::Bool => 1,
            DataType::Clock { .. } | DataType::Reset { .. } | DataType::Event => 1,
            DataType::Float16 => 16,
            DataType::Float32 => 32,
            DataType::Float64 => 64,
            DataType::BitParam { default, .. }
            | DataType::LogicParam { default, .. }
            | DataType::IntParam { default, .. }
            | DataType::NatParam { default, .. }
            | DataType::BitExpr { default, .. }
            | DataType::LogicExpr { default, .. }
            | DataType::IntExpr { default, .. }
            | DataType::NatExpr { default, .. } => *default,
            DataType::Array(elem, size) => Self::datatype_to_width(elem) * size,
            DataType::Vec2(elem) => Self::datatype_to_width(elem) * 2,
            DataType::Vec3(elem) => Self::datatype_to_width(elem) * 3,
            DataType::Vec4(elem) => Self::datatype_to_width(elem) * 4,
            DataType::Ncl(w) => w * 2, // Dual-rail encoding
            DataType::Struct(s) => s
                .fields
                .iter()
                .map(|f| Self::datatype_to_width(&f.field_type))
                .sum(),
            DataType::Enum(_) | DataType::Union(_) => 32, // Default assumption
        }
    }

    /// BUG #245 FIX: Check if a DataType is signed
    #[allow(dead_code)]
    fn is_datatype_signed(dt: &DataType) -> bool {
        matches!(
            dt,
            DataType::Int(_) | DataType::IntParam { .. } | DataType::IntExpr { .. }
        )
    }

    /// Check if a type is signed (Int types are signed)
    fn is_type_signed(ty: &skalp_frontend::types::Type) -> bool {
        use skalp_frontend::types::Type;
        match ty {
            Type::Int(_) => true,
            // Recurse into tuples/arrays if needed
            Type::Tuple(elements) => elements.iter().any(Self::is_type_signed),
            Type::Array { element_type, .. } => Self::is_type_signed(element_type),
            _ => false,
        }
    }

    /// Check if an expression is signed by looking at its type or recursing into operands
    fn is_expression_signed(&self, expr: &Expression) -> bool {
        // First check the expression's type field
        if Self::is_type_signed(&expr.ty) {
            return true;
        }

        // If type is Unknown, look at the expression kind
        match &expr.kind {
            ExpressionKind::Ref(lvalue) => {
                // Look up the signal/port type from the MIR module
                self.is_lvalue_signed(lvalue)
            }
            ExpressionKind::Binary { left, right, .. } => {
                // Recurse into binary operands
                self.is_expression_signed(left) || self.is_expression_signed(right)
            }
            ExpressionKind::Unary { operand, .. } => self.is_expression_signed(operand),
            ExpressionKind::Conditional {
                then_expr,
                else_expr,
                ..
            } => self.is_expression_signed(then_expr) || self.is_expression_signed(else_expr),
            ExpressionKind::Cast { target_type, .. } => {
                // Check if the target type is signed
                matches!(
                    target_type,
                    DataType::Int(_) | DataType::IntParam { .. } | DataType::IntExpr { .. }
                )
            }
            _ => false,
        }
    }

    /// Check if an lvalue refers to a signed type
    /// This searches across all modules in the design since signals may be from submodules
    fn is_lvalue_signed(&self, lvalue: &LValue) -> bool {
        match lvalue {
            LValue::Signal(signal_id) => {
                // First try current module
                if let Some(s) = self.mir.signals.iter().find(|s| &s.id == signal_id) {
                    return matches!(
                        s.signal_type,
                        DataType::Int(_) | DataType::IntParam { .. } | DataType::IntExpr { .. }
                    );
                }
                // Search across all modules in the design
                for module in &self.mir_design.modules {
                    if let Some(s) = module.signals.iter().find(|s| &s.id == signal_id) {
                        return matches!(
                            s.signal_type,
                            DataType::Int(_) | DataType::IntParam { .. } | DataType::IntExpr { .. }
                        );
                    }
                }
                false
            }
            LValue::Port(port_id) => {
                // First try current module
                if let Some(p) = self.mir.ports.iter().find(|p| &p.id == port_id) {
                    return matches!(
                        p.port_type,
                        DataType::Int(_) | DataType::IntParam { .. } | DataType::IntExpr { .. }
                    );
                }
                // Search across all modules in the design
                for module in &self.mir_design.modules {
                    if let Some(p) = module.ports.iter().find(|p| &p.id == port_id) {
                        return matches!(
                            p.port_type,
                            DataType::Int(_) | DataType::IntParam { .. } | DataType::IntExpr { .. }
                        );
                    }
                }
                false
            }
            LValue::Variable(var_id) => {
                // First try current module
                if let Some(v) = self.mir.variables.iter().find(|v| &v.id == var_id) {
                    return matches!(
                        v.var_type,
                        DataType::Int(_) | DataType::IntParam { .. } | DataType::IntExpr { .. }
                    );
                }
                // Search across all modules in the design
                for module in &self.mir_design.modules {
                    if let Some(v) = module.variables.iter().find(|v| &v.id == var_id) {
                        return matches!(
                            v.var_type,
                            DataType::Int(_) | DataType::IntParam { .. } | DataType::IntExpr { .. }
                        );
                    }
                }
                false
            }
            LValue::BitSelect { base, .. } | LValue::RangeSelect { base, .. } => {
                self.is_lvalue_signed(base)
            }
            LValue::Concat(_) => false,
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
            ExpressionKind::Literal(value) => {
                self.create_literal_node_with_width(value, target_width)
            }
            ExpressionKind::Ref(lvalue) => self.create_lvalue_ref_node(lvalue),
            ExpressionKind::Binary { op, left, right } => {
                // BUG #214 FIX: For comparison operations, don't propagate target_width to operands
                // Comparison results are 1-bit, but operands should use their own type widths
                // Otherwise 32-bit constants like BMS_TIMEOUT get truncated to 1-bit
                use skalp_mir::BinaryOp;
                let is_comparison = matches!(
                    op,
                    BinaryOp::Equal
                        | BinaryOp::NotEqual
                        | BinaryOp::Less
                        | BinaryOp::LessEqual
                        | BinaryOp::Greater
                        | BinaryOp::GreaterEqual
                );

                let operand_width = if is_comparison {
                    // Comparisons: operands should use their own type widths, not the 1-bit result width
                    None
                } else {
                    // Arithmetic: propagate target width so integer literals get correct width
                    target_width
                };

                // BUG FIX: Check MIR expression types for signedness (for signed comparisons)
                // Use is_expression_signed which also looks up signal/port types when expr.ty is Unknown
                let left_is_signed = self.is_expression_signed(left);
                let right_is_signed = self.is_expression_signed(right);
                let is_signed = left_is_signed || right_is_signed;

                let left_node = self.create_expression_node_with_width(left, operand_width);
                let right_node = self.create_expression_node_with_width(right, operand_width);

                self.create_binary_op_node_with_signedness(op, left_node, right_node, is_signed)
            }
            ExpressionKind::Unary { op, operand } => {
                let operand_node = self.create_expression_node_with_width(operand, target_width);
                self.create_unary_op_node(op, operand_node)
            }
            ExpressionKind::Conditional {
                cond,
                then_expr,
                else_expr,
            } => {
                let cond_node = self.create_expression_node(cond);
                // Propagate width hint to both branches
                let then_node = self.create_expression_node_with_width(then_expr, target_width);
                let else_node = self.create_expression_node_with_width(else_expr, target_width);
                self.create_mux_node(cond_node, then_node, else_node)
            }
            ExpressionKind::Concat(parts) => {
                // Extract element widths from tuple type
                let part_widths: Vec<Option<usize>> =
                    if let skalp_frontend::types::Type::Tuple(element_types) = &expr.ty {
                        element_types.iter().map(Self::type_to_width).collect()
                    } else {
                        vec![None; parts.len()]
                    };

                // Create nodes with proper widths
                let part_nodes: Vec<usize> = parts
                    .iter()
                    .zip(part_widths.iter())
                    .map(|(p, width)| self.create_expression_node_with_width(p, *width))
                    .collect();

                // BUG FIX #214: Truncate elements that exceed their target width
                // This handles cases like 64-bit multiply results going into 32-bit tuple elements
                let truncated_nodes: Vec<usize> = part_nodes
                    .iter()
                    .zip(part_widths.iter())
                    .map(|(&node, &target_opt)| {
                        if let Some(target_width) = target_opt {
                            let actual_width = self.get_node_output_width(node);
                            if actual_width > target_width {
                                // Create slice to extract low bits [target_width-1:0]
                                self.create_slice_node(node, target_width - 1, 0)
                            } else {
                                node
                            }
                        } else {
                            node
                        }
                    })
                    .collect();

                self.create_concat_node_with_width(truncated_nodes, target_width)
            }
            ExpressionKind::Cast {
                expr: inner_expr,
                target_type,
            } => {
                // BUG #245 FIX: Widening casts need sign/zero extension
                let cast_target_width = Self::datatype_to_width(target_type);

                // Create node for the inner expression at its native width (don't pass target width
                // since we want the natural width of the source expression)
                let inner_node = self.create_expression_node_with_width(inner_expr, None);

                // Get actual width from the created node, not from type annotation (which can be Unknown)
                let source_width = self.get_node_output_width(inner_node);

                if cast_target_width > source_width {
                    // Widening cast - need to extend
                    let extend_bits = cast_target_width - source_width;
                    // BUG FIX: Only sign-extend if the SOURCE is signed, not the target
                    // Casting unsigned to signed should zero-extend, not sign-extend
                    let is_signed = self.is_expression_signed(inner_expr);

                    if is_signed {
                        // Sign extension: replicate MSB (sign bit) for the extension
                        let msb_index = source_width - 1;
                        let sign_bit_node =
                            self.create_slice_node(inner_node, msb_index, msb_index);
                        let ones_constant =
                            self.create_constant_node((1u64 << extend_bits) - 1, extend_bits);
                        let zeros_constant = self.create_constant_node(0, extend_bits);
                        let sign_extend_node =
                            self.create_mux_node(sign_bit_node, ones_constant, zeros_constant);
                        self.create_concat_node(vec![sign_extend_node, inner_node])
                    } else {
                        // Zero extension: pad with zeros
                        let zeros_node = self.create_constant_node(0, extend_bits);
                        self.create_concat_node(vec![zeros_node, inner_node])
                    }
                } else if cast_target_width < source_width {
                    // Narrowing cast - truncate by slicing
                    self.create_slice_node(inner_node, cast_target_width - 1, 0)
                } else {
                    // Same width - just return the inner expression (bitwise reinterpretation)
                    inner_node
                }
            }
            // BUG FIX #85: Handle tuple field access for module synthesis
            ExpressionKind::TupleFieldAccess { base, index } => {
                // Check if the base is a Signal reference (module instance result)
                if let ExpressionKind::Ref(LValue::Signal(sig_id)) = &base.kind {
                    // Look up the signal name
                    if let Some(signal) = self.mir.signals.iter().find(|s| s.id == *sig_id) {
                        // Check if this is a module instance result signal
                        if signal.name.ends_with("_result_0") || signal.name.contains("_inst_") {
                            let base_name = if let Some(pos) = signal.name.rfind("_result_") {
                                &signal.name[..pos]
                            } else {
                                &signal.name[..]
                            };
                            let target_signal_name = format!("{}_result_{}", base_name, index);
                            return self.get_or_create_signal_driver(&target_signal_name);
                        }

                        // BUG FIX #85: Check if this is a _tuple_tmp signal assigned from a module result
                        // Look at the assignment to this signal to find the source
                        if signal.name.contains("_tuple_tmp") {
                            for assign in &self.mir.assignments {
                                if let LValue::Signal(lhs_id) = &assign.lhs {
                                    if lhs_id == sig_id {
                                        if let ExpressionKind::Ref(LValue::Signal(rhs_sig_id)) =
                                            &assign.rhs.kind
                                        {
                                            if let Some(rhs_signal) = self
                                                .mir
                                                .signals
                                                .iter()
                                                .find(|s| s.id == *rhs_sig_id)
                                            {
                                                if rhs_signal.name.contains("_result_") {
                                                    // Extract the base name (everything before _result_N)
                                                    if let Some(pos) =
                                                        rhs_signal.name.rfind("_result_")
                                                    {
                                                        let base_name = &rhs_signal.name[..pos];
                                                        let target_signal_name = format!(
                                                            "{}_result_{}",
                                                            base_name, index
                                                        );
                                                        return self.get_or_create_signal_driver(
                                                            &target_signal_name,
                                                        );
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
                let base_node = self.create_expression_node_with_width(base, None);
                let element_width = 32;
                let low_bit = (*index) * element_width;
                let high_bit = low_bit + element_width - 1;
                // BUG FIX #92: create_slice_node expects (high, low) for HDL [high:low] notation
                self.create_slice_node(base_node, high_bit, low_bit)
            }
            ExpressionKind::FieldAccess { base, field } => {
                // BUG #220 FIX: Properly resolve FieldAccess to flattened signal names
                // Walk up the FieldAccess chain to find root and build field path
                let mut field_path = vec![field.clone()];
                let mut current_base = base.as_ref();

                loop {
                    match &current_base.kind {
                        ExpressionKind::FieldAccess {
                            base: inner_base,
                            field: inner_field,
                        } => {
                            field_path.insert(0, inner_field.clone());
                            current_base = inner_base.as_ref();
                        }
                        ExpressionKind::Ref(lvalue) => {
                            // Found the root - construct the flattened signal name
                            let root_name =
                                match lvalue {
                                    LValue::Signal(sig_id) => self
                                        .mir
                                        .signals
                                        .iter()
                                        .find(|s| s.id == *sig_id)
                                        .map(|s| s.name.clone()),
                                    LValue::Port(port_id) => self
                                        .mir
                                        .ports
                                        .iter()
                                        .find(|p| p.id == *port_id)
                                        .map(|p| p.name.clone()),
                                    LValue::Variable(var_id) => {
                                        self.mir.variables.iter().find(|v| v.id == *var_id).map(
                                            |v| {
                                                // Variables use unique naming: name_id
                                                format!("{}_{}", v.name, v.id.0)
                                            },
                                        )
                                    }
                                    _ => None,
                                };

                            if let Some(root) = root_name {
                                // Build flattened name: root__field1__field2...
                                let flattened_name = format!("{}__{}", root, field_path.join("__"));

                                // Look up the flattened signal
                                if let Some(internal_name) =
                                    self.mir_to_internal_name.get(&flattened_name).cloned()
                                {
                                    return self.get_or_create_signal_driver(&internal_name);
                                }

                                // Try with single underscore (some flattening uses _ instead of __)
                                let flattened_name_single =
                                    format!("{}_{}", root, field_path.join("_"));
                                if let Some(internal_name) = self
                                    .mir_to_internal_name
                                    .get(&flattened_name_single)
                                    .cloned()
                                {
                                    return self.get_or_create_signal_driver(&internal_name);
                                }

                                // Try looking in MIR signals directly for flattened names
                                for signal in &self.mir.signals {
                                    if signal.name == flattened_name
                                        || signal.name == flattened_name_single
                                    {
                                        return self.get_or_create_signal_driver(&signal.name);
                                    }
                                }
                            }
                            break;
                        }
                        _ => break,
                    }
                }

                // Fallback: just use the base expression
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

    fn create_literal_node_with_width(
        &mut self,
        value: &Value,
        target_width: Option<usize>,
    ) -> usize {
        // BUG #157 FIX: Track if this is a float literal so we can preserve float type
        let is_float = matches!(value, Value::Float(_));

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

        // BUG #157 FIX: Use create_constant_node_with_type for floats to preserve type info
        if is_float {
            self.create_constant_node_with_type(val, width, SirType::Float32)
        } else {
            self.create_constant_node(val, width)
        }
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

    /// BUG #157 FIX: Create a constant node with explicit type information
    /// Used to preserve float type for float literals so that unary negation
    /// can detect them and use FNeg instead of integer negation
    fn create_constant_node_with_type(
        &mut self,
        value: u64,
        width: usize,
        sir_type: SirType,
    ) -> usize {
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
            sir_type,
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
                // First try to find signal in current (top-level) module
                if let Some(signal) = self.mir.signals.iter().find(|s| s.id == *sig_id) {
                    // Pass MIR name - get_or_create_signal_driver will translate to internal name
                    return self.get_or_create_signal_driver(&signal.name);
                }

                // BUG #209 FIX: Signal not found in top-level module - search in child modules
                // This happens when trait method inlining creates entity instances that are
                // drained into child modules (e.g., func_exec_l2), but the expressions
                // referencing their outputs remain in the top-level module.
                //
                // For each child module, find:
                // 1. The signal by ID
                // 2. Which instance(s) of that module exist in the current module
                // 3. Construct the prefixed signal name (e.g., "exec_l2_inst.signal_name")
                for child_module in &self.mir_design.modules {
                    if let Some(signal) = child_module.signals.iter().find(|s| s.id == *sig_id) {
                        // Found the signal in a child module
                        // Now find which instance(s) of this module exist in self.mir
                        for instance in &self.mir.instances {
                            if instance.module == child_module.id {
                                // Found an instance of the child module
                                let prefixed_name = format!("{}.{}", instance.name, signal.name);
                                return self.get_or_create_signal_driver(&prefixed_name);
                            }
                        }
                        // Signal found in child module but no matching instance in current module
                        // This could happen with deeply nested modules - use the signal name directly
                        return self.get_or_create_signal_driver(&signal.name);
                    }
                }

                // Still not found - generate a warning and use fallback name
                let signal_name = format!("signal_{}", sig_id.0);
                self.get_or_create_signal_driver(&signal_name)
            }
            LValue::Port(port_id) => {
                // First try to find the port in the current (top-level) module
                if let Some(port) = self.mir.ports.iter().find(|p| p.id == *port_id) {
                    // For input ports, create a direct signal reference node
                    // Pass MIR name - create_port_input_node will translate to internal name
                    if matches!(port.direction, MirPortDirection::Input) {
                        return self.create_port_input_node(&port.name);
                    } else {
                        // For output ports, create a signal driver
                        // Pass MIR name - get_or_create_signal_driver will translate
                        return self.get_or_create_signal_driver(&port.name);
                    }
                }

                // BUG FIX: Port not found in top-level module - search in child modules
                // After monomorphization/module merging, expressions may still contain
                // LValue::Port references with port IDs from child modules. These ports
                // were converted to signals during the merge, so we need to find the
                // original port name and use it as a signal reference.
                for child_module in &self.mir_design.modules {
                    if let Some(port) = child_module.ports.iter().find(|p| p.id == *port_id) {
                        // The port has been merged as a signal - use get_or_create_signal_driver
                        return self.get_or_create_signal_driver(&port.name);
                    }
                }

                // Still not found - this is a bug in the MIR, but provide graceful fallback
                self.create_constant_node(0, 1)
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
                    .map(|v| format!("{}_{}", v.name, var_id.0)) // Unique: name + id
                    .unwrap_or_else(|| format!("var_{}", var_id.0));
                // Pass MIR variable name - get_or_create_signal_driver will translate
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
                        // Check if this is a _tuple_tmp variable
                        if var.name.contains("_tuple_tmp") {
                            // Look for assignment to this VARIABLE (not signal) to find source
                            // MIR assignments can have LValue::Variable as LHS
                            for assign in &self.mir.assignments {
                                // Check if LHS is this variable
                                let is_this_var = match &assign.lhs {
                                    LValue::Variable(lhs_var_id) => *lhs_var_id == *var_id,
                                    _ => false,
                                };

                                if is_this_var {
                                    // Check if RHS is a signal reference
                                    if let ExpressionKind::Ref(LValue::Signal(rhs_sig_id)) =
                                        &assign.rhs.kind
                                    {
                                        if let Some(rhs_sig) =
                                            self.mir.signals.iter().find(|s| s.id == *rhs_sig_id)
                                        {
                                            if rhs_sig.name.contains("_result_") {
                                                // Extract base name and compute index from range
                                                let _high_val = self
                                                    .evaluate_constant_expression(high)
                                                    .unwrap_or(0)
                                                    as usize;
                                                let low_val = self
                                                    .evaluate_constant_expression(low)
                                                    .unwrap_or(0)
                                                    as usize;

                                                // BUG FIX #92: Tuple elements are packed LSB-first
                                                // For a tuple (bool, f32, f32) with 32-bit elements:
                                                //   - result_0 at bits 31:0 (element 0 at LSB)
                                                //   - result_1 at bits 63:32
                                                //   - result_2 at bits 95:64 (element 2 at MSB)
                                                // Map bit range to element index: index = low_val / 32
                                                let index = low_val / 32;

                                                if let Some(pos) = rhs_sig.name.rfind("_result_") {
                                                    let base_name = &rhs_sig.name[..pos];
                                                    let target_signal_name =
                                                        format!("{}_result_{}", base_name, index);
                                                    return self.get_or_create_signal_driver(
                                                        &target_signal_name,
                                                    );
                                                }
                                            }
                                        }
                                    }
                                }
                            }
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
        // Default to unsigned when signedness is not known from MIR expression types
        self.create_binary_op_node_with_signedness(op, left, right, false)
    }

    fn create_binary_op_node_with_signedness(
        &mut self,
        op: &skalp_mir::BinaryOp,
        left: usize,
        right: usize,
        is_signed_from_mir: bool,
    ) -> usize {
        let node_id = self.next_node_id();

        let left_signal = self.node_to_signal_ref(left);
        let right_signal = self.node_to_signal_ref(right);

        // Determine type from operation and inputs
        let left_type = self.get_signal_type(&left_signal.signal_id);
        let right_type = self.get_signal_type(&right_signal.signal_id);

        // BUG FIX: Use signed comparisons when MIR expression types indicate signed operands
        // This ensures correct evaluation of signed fixed-point comparisons (e.g., q16_16)
        // The is_signed_from_mir flag is determined from the original MIR Expression.ty field,
        // which is more reliable than looking at intermediate SIR signal types.
        let is_signed = is_signed_from_mir || left_type.is_signed() || right_type.is_signed();
        let is_float = left_type.is_float() || right_type.is_float();

        let bin_op = if is_float {
            self.convert_binary_op_for_float(op)
        } else {
            self.convert_binary_op_with_signedness(op, is_signed)
        };

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
                | BinaryOperation::Slt
                | BinaryOperation::Slte
                | BinaryOperation::Sgt
                | BinaryOperation::Sgte
                | BinaryOperation::FEq
                | BinaryOperation::FNeq
                | BinaryOperation::FLt
                | BinaryOperation::FLte
                | BinaryOperation::FGt
                | BinaryOperation::FGte
        ) {
            // Comparison operations return 1-bit boolean
            SirType::Bits(1)
        } else if bin_op == BinaryOperation::Mul || bin_op == BinaryOperation::SMul {
            // BUG FIX #214, #247: Multiply produces full precision result
            // m-bit × n-bit = (m+n)-bit (hardware correct semantics)
            // Truncation happens at assignment to typed targets (tuples, typed variables)
            // Both unsigned (Mul) and signed (SMul) multiplication produce full width results
            let width = left_type.width() + right_type.width();
            if is_signed {
                SirType::SignedBits(width)
            } else {
                SirType::Bits(width)
            }
        } else {
            // Other arithmetic/logic operations use max width
            let width = left_type.width().max(right_type.width());
            SirType::Bits(width)
        };

        let width = sir_type.width();

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

        // Get type from true/false branches
        let true_type = self.get_signal_type(&true_signal.signal_id);
        let false_type = self.get_signal_type(&false_signal.signal_id);
        let true_width = true_type.width();
        let false_width = false_type.width();

        // BUG FIX #181: Prefer Float32 type over wider Bits types
        // When one branch is Float32 (32-bit) and another is a wider Bits type (e.g., 65-bit
        // from concat operations), the semantic output should be Float32, not the wider Bits.
        // This happens in FP operations where intermediate concat results get mixed with fp32 values.
        let true_is_float = true_type.is_float();
        let false_is_float = false_type.is_float();

        let (sir_type, width) = if true_is_float && !false_is_float {
            // True branch is Float32, false branch is Bits - prefer Float32
            (true_type.clone(), true_width)
        } else if false_is_float && !true_is_float {
            // False branch is Float32, true branch is Bits - prefer Float32
            (false_type.clone(), false_width)
        } else {
            // Both same type category - use the wider type (original behavior)
            if true_width >= false_width {
                (true_type.clone(), true_width)
            } else {
                (false_type.clone(), false_width)
            }
        };

        // BUG #125 FIX: Handle width mismatch in mux inputs
        // When one input is narrower than the other, check if it's a zero constant
        // and replace it with a properly-sized zero to avoid truncation issues
        let mut actual_true_signal = true_signal;
        let mut actual_false_signal = false_signal;
        let mut actual_true_val = true_val;
        let mut actual_false_val = false_val;

        if true_width != false_width {
            // BUG FIX #181: When preferring Float32, slice wider Bits branch to 32 bits
            if true_is_float && false_width > width {
                // Slice the false branch (Bits) to match Float32 width
                let sliced_false = self.create_slice_node(false_val, width - 1, 0);
                actual_false_val = sliced_false;
                actual_false_signal = self.node_to_signal_ref(sliced_false);
            } else if false_is_float && true_width > width {
                // Slice the true branch (Bits) to match Float32 width
                let sliced_true = self.create_slice_node(true_val, width - 1, 0);
                actual_true_val = sliced_true;
                actual_true_signal = self.node_to_signal_ref(sliced_true);
            } else {
                // BUG FIX #117/#125: Widen narrower mux input to match the target width.
                // Previously only zero constants were widened; now we widen any constant,
                // and for non-constant narrow values we zero-extend via a concat with
                // a zero-padding constant.
                #[allow(clippy::comparison_chain)]
                if false_width < width {
                    // false branch is narrower than target
                    if let Some(const_value) = self.is_constant_node(actual_false_val) {
                        let wide_const = self.create_constant_node(const_value, width);
                        actual_false_val = wide_const;
                        actual_false_signal = self.node_to_signal_ref(wide_const);
                    } else {
                        // Non-constant: zero-extend by concatenating with zero padding
                        let pad_width = width - false_width;
                        let zero_pad = self.create_constant_node(0, pad_width);
                        let extended = self.create_concat_node(vec![zero_pad, actual_false_val]);
                        actual_false_val = extended;
                        actual_false_signal = self.node_to_signal_ref(extended);
                    }
                }
                if true_width < width {
                    // true branch is narrower than target
                    if let Some(const_value) = self.is_constant_node(actual_true_val) {
                        let wide_const = self.create_constant_node(const_value, width);
                        actual_true_val = wide_const;
                        actual_true_signal = self.node_to_signal_ref(wide_const);
                    } else {
                        // Non-constant: zero-extend by concatenating with zero padding
                        let pad_width = width - true_width;
                        let zero_pad = self.create_constant_node(0, pad_width);
                        let extended = self.create_concat_node(vec![zero_pad, actual_true_val]);
                        actual_true_val = extended;
                        actual_true_signal = self.node_to_signal_ref(extended);
                    }
                }
            }
        }

        // Suppress unused variable warnings
        let _ = actual_true_val;
        let _ = actual_false_val;

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
    #[allow(dead_code)]
    fn is_zero_constant_node(&self, node_id: usize) -> Option<u64> {
        // Look through all combinational nodes to find this node
        for node in &self.sir.combinational_nodes {
            if node.id == node_id {
                if let SirNodeKind::Constant { value, .. } = &node.kind {
                    if *value == 0 {
                        return Some(*value);
                    }
                }
            }
        }
        None
    }

    /// Check if a node is a constant (any value) and return its value
    /// BUG FIX #117: Used by mux width mismatch handler to widen narrow constants
    fn is_constant_node(&self, node_id: usize) -> Option<u64> {
        for node in &self.sir.combinational_nodes {
            if node.id == node_id {
                if let SirNodeKind::Constant { value, .. } = &node.kind {
                    return Some(*value);
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
        for s in part_signals.iter() {
            let _width = self.get_signal_width(&s.signal_id);
        }

        let sum_width: usize = part_signals
            .iter()
            .map(|s| self.get_signal_width(&s.signal_id))
            .sum();

        // BUG FIX #92: Check if this is a tuple concat before we move part_signals
        // Tuple concats have multiple 32-bit elements and should NOT be sliced
        let is_tuple_concat = parts.len() >= 2
            && part_signals.iter().all(|s| {
                let width = self.get_signal_width(&s.signal_id);
                width == 32 // All elements are 32-bit (typical tuple element width)
            });

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
                let slice_node = self.create_slice_node(node_id, target - 1, 0);
                return slice_node;
            } else {
                // BUG FIX #73: No target width, but concat > 256 bits
                // Don't create a slice (would lose data) - Metal backend will decompose
                // Signal will be automatically decomposed by Metal codegen
            }
        }

        node_id
    }

    fn create_slice_node(&mut self, base: usize, start: usize, end: usize) -> usize {
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

        self.sir.combinational_nodes.push(node);
        node_id
    }

    fn create_array_read_node(&mut self, array: usize, index: usize) -> usize {
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
            _ => (SirType::Bits(8), 8),
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

        self.sir.combinational_nodes.push(node);
        node_id
    }

    fn create_array_write_node(&mut self, old_array: usize, index: usize, value: usize) -> usize {
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

        self.sir.combinational_nodes.push(node);
        node_id
    }

    fn create_flipflop_with_input(&mut self, input: usize, clock: &str, edge: ClockEdge) -> usize {
        let node_id = self.next_node_id();

        // Translate MIR clock name to internal name
        let internal_clock = self.translate_to_internal_name(clock);

        let input_signal = self.node_to_signal_ref(input);
        let clock_signal = SignalRef {
            signal_id: internal_clock.clone(),
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
            clock_domain: Some(internal_clock),
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

    fn convert_case_to_mux_tree(&mut self, case_stmt: &skalp_mir::CaseStatement) {
        // BUG #117r FIX: Implement case/match statement to mux tree conversion
        // This enables state machines with match statements to generate proper transitions

        // Create expression node for the case expression (e.g., state_reg)
        let case_expr_node = self.create_expression_node(&case_stmt.expr);

        // Collect all signals assigned in any case arm or default
        let mut all_assignments: HashMap<String, Vec<(Vec<&Expression>, &Expression)>> =
            HashMap::new();
        let mut default_assignments: HashMap<String, &Expression> = HashMap::new();

        // Extract assignments from each case item
        for item in &case_stmt.items {
            let mut item_assignments = HashMap::new();
            self.extract_assignments_from_block_ref(&item.block.statements, &mut item_assignments);

            for (signal_name, expr) in item_assignments {
                all_assignments
                    .entry(signal_name)
                    .or_default()
                    .push((item.values.iter().collect(), expr));
            }
        }

        // Extract default assignments
        if let Some(default_block) = &case_stmt.default {
            self.extract_assignments_from_block_ref(
                &default_block.statements,
                &mut default_assignments,
            );
        }

        // For each signal, build a mux tree
        for (signal_name, case_arms) in &all_assignments {
            // Start with default value
            let mut current_mux = if let Some(default_expr) = default_assignments.get(signal_name) {
                self.create_expression_node(default_expr)
            } else {
                // No default - use current signal value (for state holding)
                self.get_or_create_signal_driver(signal_name)
            };

            // Build mux chain from last case to first (so first has priority)
            for (values, value_expr) in case_arms.iter().rev() {
                let value_node = self.create_expression_node(value_expr);

                // Create condition: case_expr == value (for each value in values list)
                // For multiple values (e.g., State::A | State::B), we need to OR them
                let condition_node = if values.is_empty() {
                    // Should not happen, but handle gracefully
                    continue;
                } else if values.len() == 1 {
                    // Single value match: case_expr == value
                    let val_node = self.create_expression_node(values[0]);
                    self.create_binary_op_node(
                        &skalp_mir::BinaryOp::Equal,
                        case_expr_node,
                        val_node,
                    )
                } else {
                    // Multiple values: (case_expr == v1) | (case_expr == v2) | ...
                    let mut or_node = {
                        let val_node = self.create_expression_node(values[0]);
                        self.create_binary_op_node(
                            &skalp_mir::BinaryOp::Equal,
                            case_expr_node,
                            val_node,
                        )
                    };
                    for value in &values[1..] {
                        let val_node = self.create_expression_node(value);
                        let eq_node = self.create_binary_op_node(
                            &skalp_mir::BinaryOp::Equal,
                            case_expr_node,
                            val_node,
                        );
                        or_node =
                            self.create_binary_op_node(&skalp_mir::BinaryOp::Or, or_node, eq_node);
                    }
                    or_node
                };

                // Create mux: condition ? value_node : current_mux
                current_mux = self.create_mux_node(condition_node, value_node, current_mux);
            }

            // Connect final mux to signal
            self.connect_node_to_signal(current_mux, signal_name);
        }

        // Handle signals that only appear in default (not in any case arm)
        for (signal_name, default_expr) in &default_assignments {
            if !all_assignments.contains_key(signal_name) {
                let value_node = self.create_expression_node(default_expr);
                self.connect_node_to_signal(value_node, signal_name);
            }
        }
    }

    /// Extract assignments from a block, returning references to expressions
    fn extract_assignments_from_block_ref<'b>(
        &self,
        statements: &'b [Statement],
        assignments: &mut HashMap<String, &'b Expression>,
    ) {
        for stmt in statements {
            match stmt {
                Statement::Assignment(assign) => {
                    let target = self.lvalue_to_string(&assign.lhs);
                    assignments.insert(target, &assign.rhs);
                }
                Statement::Block(block) => {
                    self.extract_assignments_from_block_ref(&block.statements, assignments);
                }
                _ => {
                    // Skip nested if/case statements - they should be handled separately
                }
            }
        }
    }

    fn connect_node_to_signal(&mut self, node_id: usize, signal_name: &str) {
        // Translate MIR name to internal name for SIR lookups
        let internal_name = self.translate_to_internal_name(signal_name);

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
            // Get the width of the node's output signal (node_X_out)
            if !node.outputs.is_empty() {
                let output_signal_name = &node.outputs[0].signal_id;
                Some(self.get_signal_width(output_signal_name))
            } else {
                None
            }
        } else {
            None
        };

        // Update signal to have this node as driver (using internal name for SIR lookup)
        // First check if the new node is a flip-flop (for state elements)
        let new_node_is_flipflop = self
            .sir
            .sequential_nodes
            .iter()
            .any(|n| n.id == node_id && matches!(n.kind, SirNodeKind::FlipFlop { .. }));

        // Track the old driver that needs to have this signal removed from its outputs
        let mut old_driver_to_update: Option<usize> = None;

        // BUG FIX #117: Track whether we need to insert a truncation slice node.
        // This must be done after releasing the mutable borrow on self.sir.signals.
        let mut needs_truncation: Option<(usize, usize)> = None; // (declared_width, output_width)

        if let Some(signal) = self
            .sir
            .signals
            .iter_mut()
            .find(|s| s.name == internal_name)
        {
            if let Some(existing_driver) = signal.driver_node {
                // BUG FIX #117r: Allow flip-flops to overwrite continuous assignment drivers for state elements
                // This happens when a signal has an initialization value (continuous assign) but is also
                // assigned in sequential logic (needs flip-flop). The flip-flop should be the driver.
                if new_node_is_flipflop && signal.is_state {
                    // Remember to remove the signal from the old driver's outputs
                    old_driver_to_update = Some(existing_driver);
                    // Continue to set the flip-flop as driver
                } else {
                    // Don't overwrite - keep the first driver
                    return;
                }
            }

            // BUG FIX #117: Check if node output is wider than declared signal width.
            if let Some(output_width) = node_output_width {
                if output_width > signal.width {
                    needs_truncation = Some((signal.width, output_width));
                }
            }

            if needs_truncation.is_none() {
                // Normal case: no truncation needed, set driver directly
                signal.driver_node = Some(node_id);
            }
        }

        // BUG FIX #117: Insert truncation slice node when node output is wider than declared signal
        // BUG #249 FIX: For FlipFlops, don't use a slice - the codegen already handles masking.
        // The FlipFlop output should be the state signal so sequential codegen works correctly.
        if let Some((declared_width, _output_width)) = needs_truncation {
            if !new_node_is_flipflop {
                // Only create slice for combinational nodes, not FlipFlops
                let slice_node = self.create_slice_node(node_id, declared_width - 1, 0);
                // Set the signal's driver to the slice node
                if let Some(signal) = self
                    .sir
                    .signals
                    .iter_mut()
                    .find(|s| s.name == internal_name)
                {
                    signal.driver_node = Some(slice_node);
                }
                // Add the signal name to the slice node's outputs
                if let Some(slice) = self
                    .sir
                    .combinational_nodes
                    .iter_mut()
                    .find(|n| n.id == slice_node)
                {
                    slice.outputs.push(SignalRef {
                        signal_id: internal_name.clone(),
                        bit_range: None,
                    });
                }

                // Handle old driver cleanup then return early (skip normal output wiring)
                if let Some(old_driver_id) = old_driver_to_update {
                    if let Some(old_node) = self
                        .sir
                        .combinational_nodes
                        .iter_mut()
                        .chain(self.sir.sequential_nodes.iter_mut())
                        .find(|n| n.id == old_driver_id)
                    {
                        old_node.outputs.retain(|o| o.signal_id != internal_name);
                    }
                }
                return;
            }
            // For FlipFlops, fall through to update the FlipFlop output directly
            // The codegen will apply the appropriate width mask
            if let Some(signal) = self
                .sir
                .signals
                .iter_mut()
                .find(|s| s.name == internal_name)
            {
                signal.driver_node = Some(node_id);
            }
        }

        // BUG FIX #117r: Remove signal from old driver's outputs when flip-flop takes over
        // This prevents the old initialization node from overwriting the flip-flop's value each cycle
        if let Some(old_driver_id) = old_driver_to_update {
            if let Some(old_node) = self
                .sir
                .combinational_nodes
                .iter_mut()
                .find(|n| n.id == old_driver_id)
            {
                old_node.outputs.retain(|o| o.signal_id != internal_name);
            }
        }

        // Update node to output to this signal (using internal name)
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
                    signal_id: internal_name.clone(),
                    bit_range: None,
                };
            } else {
                // Check if this signal is already an output of this node to prevent duplicates
                if !node.outputs.iter().any(|o| o.signal_id == internal_name) {
                    node.outputs.push(SignalRef {
                        signal_id: internal_name.clone(),
                        bit_range: None,
                    });
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

        // Get port type from MIR (using MIR name for lookup)
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

        // Translate MIR port name to internal name for SignalRef
        let internal_name = self.translate_to_internal_name(port_name);

        // Create a SignalRef node that directly references the input port (using internal name)
        let node = SirNode {
            id: node_id,
            kind: SirNodeKind::SignalRef {
                signal: internal_name.clone(),
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
        // Translate MIR name to internal name for all SIR lookups and SignalRef creation
        let internal_name = self.translate_to_internal_name(name);

        // CRITICAL: If this is a state element (register), we MUST create a SignalRef
        // reader node, even if the signal already has a driver (the flip-flop).
        // The flip-flop is the WRITER, but we need a READER node for combinational logic.
        let is_state_element = self.sir.state_elements.contains_key(&internal_name);

        if !is_state_element {
            // For non-state signals, return existing driver if available
            if let Some(signal) = self.sir.signals.iter().find(|s| s.name == internal_name) {
                if let Some(driver) = signal.driver_node {
                    return driver;
                }
            }
        }

        // Create a signal reader node that reads from state or signals
        let node_id = self.next_node_id();

        // Create output signal for this node
        let output_signal_name = format!("node_{}_out", node_id);
        let output_signal = SignalRef {
            signal_id: output_signal_name.clone(),
            bit_range: None,
        };

        // Check if this is a state element or signal to get type (using internal name)
        let sir_type =
            if let Some(signal) = self.sir.signals.iter().find(|s| s.name == internal_name) {
                signal.sir_type.clone()
            } else if let Some(state) = self.sir.state_elements.get(&internal_name) {
                state.sir_type.clone().unwrap_or(SirType::Bits(state.width))
            } else {
                // BUG #278 FIX: Try to resolve width from MIR port types by parsing
                // the hierarchical name (e.g., "inner.current_magnitude" → instance "inner", port "current_magnitude")
                let mut resolved_type = None;
                if let Some(dot_pos) = name.rfind('.') {
                    let port_name = &name[dot_pos + 1..];
                    // Search all MIR modules for a port with this name
                    for module in &self.mir_design.modules {
                        if let Some(port) = module.ports.iter().find(|p| p.name == port_name) {
                            resolved_type = Some(self.convert_type(&port.port_type));
                            break;
                        }
                    }
                }
                resolved_type.unwrap_or(SirType::Bits(32)) // Default to 32 bits (safer than 8)
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

        // Create a signal reader node (using internal name)
        let node = SirNode {
            id: node_id,
            kind: SirNodeKind::SignalRef {
                signal: internal_name.clone(),
            },
            inputs: vec![SignalRef {
                signal_id: internal_name.clone(),
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

    /// BUG #255 FIX: Create a SignalRef node that reads from a child input port signal.
    ///
    /// This is different from `get_or_create_signal_driver` because we ALWAYS create a
    /// new SignalRef node, even if the signal already has a driver. This ensures the
    /// output type comes from the signal's declared type (which matches the child port),
    /// not from the driver's type (which may come from a parent expression with different
    /// signedness).
    ///
    /// Example: If parent passes `current_magnitude as nat[16]` to child port `value: nat[16]`,
    /// the driver might be signed (from current which is int[16]), but the child port is unsigned.
    /// By creating a fresh SignalRef with the signal's declared type, comparisons in the child
    /// module will use unsigned operations as expected.
    fn create_signal_reader_for_child_port(&mut self, signal_name: &str) -> usize {
        let node_id = self.next_node_id();

        // Create output signal for this node
        let output_signal_name = format!("node_{}_out", node_id);
        let output_signal = SignalRef {
            signal_id: output_signal_name.clone(),
            bit_range: None,
        };

        // Get the signal's declared type (NOT the driver's type)
        let sir_type = if let Some(signal) = self.sir.signals.iter().find(|s| s.name == signal_name)
        {
            signal.sir_type.clone()
        } else {
            SirType::Bits(16) // Default fallback
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
                signal: signal_name.to_string(),
            },
            inputs: vec![SignalRef {
                signal_id: signal_name.to_string(),
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

    /// BUG #220 FIX: Resolve a FieldAccess expression chain to a flattened signal name
    ///
    /// Walks up the FieldAccess chain to find the root signal/port/variable,
    /// then builds the flattened signal name (e.g., `faults.ov` -> `faults__ov`).
    ///
    /// Returns the node ID of the signal driver if found, None otherwise.
    fn resolve_field_access_to_signal(&mut self, base: &Expression, field: &str) -> Option<usize> {
        // Walk up the FieldAccess chain to find root and build field path
        let mut field_path = vec![field.to_string()];
        let mut current_base = base;

        loop {
            match &current_base.kind {
                ExpressionKind::FieldAccess {
                    base: inner_base,
                    field: inner_field,
                } => {
                    field_path.insert(0, inner_field.clone());
                    current_base = inner_base.as_ref();
                }
                ExpressionKind::Ref(lvalue) => {
                    // Found the root - construct the flattened signal name
                    // BUG #246 FIX: Search child modules when signal/port not found in current module
                    // This happens when field access is on an instance output port (e.g., prot_faults.ov)
                    let root_name = match lvalue {
                        LValue::Signal(sig_id) => {
                            // First try current module
                            if let Some(signal) = self.mir.signals.iter().find(|s| s.id == *sig_id)
                            {
                                Some(signal.name.clone())
                            } else {
                                // BUG #246: Search child modules and prefix with instance name
                                let mut result = None;
                                for child_module in &self.mir_design.modules {
                                    if let Some(signal) =
                                        child_module.signals.iter().find(|s| s.id == *sig_id)
                                    {
                                        // Found in child module - look for instance of this module
                                        for instance in &self.mir.instances {
                                            if instance.module == child_module.id {
                                                result = Some(format!(
                                                    "{}.{}",
                                                    instance.name, signal.name
                                                ));
                                                break;
                                            }
                                        }
                                        if result.is_some() {
                                            break;
                                        }
                                    }
                                }
                                result
                            }
                        }
                        LValue::Port(port_id) => {
                            // First try current module
                            if let Some(port) = self.mir.ports.iter().find(|p| p.id == *port_id) {
                                Some(port.name.clone())
                            } else {
                                // BUG #246: Search child modules and prefix with instance name
                                let mut result = None;
                                for child_module in &self.mir_design.modules {
                                    if let Some(port) =
                                        child_module.ports.iter().find(|p| p.id == *port_id)
                                    {
                                        // Found in child module - look for instance of this module
                                        for instance in &self.mir.instances {
                                            if instance.module == child_module.id {
                                                result = Some(format!(
                                                    "{}.{}",
                                                    instance.name, port.name
                                                ));
                                                break;
                                            }
                                        }
                                        if result.is_some() {
                                            break;
                                        }
                                    }
                                }
                                result
                            }
                        }
                        LValue::Variable(var_id) => {
                            self.mir
                                .variables
                                .iter()
                                .find(|v| v.id == *var_id)
                                .map(|v| {
                                    // Variables use unique naming: name_id
                                    format!("{}_{}", v.name, v.id.0)
                                })
                        }
                        _ => None,
                    };

                    if let Some(root) = root_name {
                        // Build flattened name: root__field1__field2...
                        let flattened_name = format!("{}__{}", root, field_path.join("__"));

                        // Look up the flattened signal
                        if let Some(internal_name) =
                            self.mir_to_internal_name.get(&flattened_name).cloned()
                        {
                            return Some(self.get_or_create_signal_driver(&internal_name));
                        }

                        // Try with single underscore (some flattening uses _ instead of __)
                        let flattened_name_single = format!("{}_{}", root, field_path.join("_"));
                        if let Some(internal_name) = self
                            .mir_to_internal_name
                            .get(&flattened_name_single)
                            .cloned()
                        {
                            return Some(self.get_or_create_signal_driver(&internal_name));
                        }

                        // Try looking in MIR signals directly for flattened names
                        let mir_match = self
                            .mir
                            .signals
                            .iter()
                            .find(|s| s.name == flattened_name || s.name == flattened_name_single)
                            .map(|s| s.name.clone());
                        if let Some(name) = mir_match {
                            return Some(self.get_or_create_signal_driver(&name));
                        }

                        // Try looking in SIR signals directly
                        let sir_match = self
                            .sir
                            .signals
                            .iter()
                            .find(|s| s.name == flattened_name || s.name == flattened_name_single)
                            .map(|s| s.name.clone());
                        if let Some(name) = sir_match {
                            return Some(self.get_or_create_signal_driver(&name));
                        }
                    }
                    return None;
                }
                _ => return None,
            }
        }
    }

    #[allow(dead_code)]
    fn convert_binary_op(&self, op: &skalp_mir::BinaryOp) -> BinaryOperation {
        // Default to unsigned comparisons
        self.convert_binary_op_with_signedness(op, false)
    }

    /// Convert MIR BinaryOp to SIR BinaryOperation, using signed operations when is_signed is true
    fn convert_binary_op_with_signedness(
        &self,
        op: &skalp_mir::BinaryOp,
        is_signed: bool,
    ) -> BinaryOperation {
        use skalp_mir::BinaryOp::*;
        match op {
            Add => BinaryOperation::Add,
            Sub => BinaryOperation::Sub,
            // BUG FIX #247: Use signed arithmetic when operands are signed
            Mul => {
                if is_signed {
                    BinaryOperation::SMul
                } else {
                    BinaryOperation::Mul
                }
            }
            Div => {
                if is_signed {
                    BinaryOperation::SDiv
                } else {
                    BinaryOperation::Div
                }
            }
            Mod => {
                if is_signed {
                    BinaryOperation::SMod
                } else {
                    BinaryOperation::Mod
                }
            }
            BitwiseAnd => BinaryOperation::And,
            BitwiseOr => BinaryOperation::Or,
            BitwiseXor => BinaryOperation::Xor,
            And => BinaryOperation::And, // Logical AND mapped to bitwise
            Or => BinaryOperation::Or,   // Logical OR mapped to bitwise
            Xor => BinaryOperation::Xor, // Logical XOR mapped to bitwise
            Equal => BinaryOperation::Eq,
            NotEqual => BinaryOperation::Neq,
            // BUG FIX: Use signed comparisons when operands are signed
            Less => {
                if is_signed {
                    BinaryOperation::Slt
                } else {
                    BinaryOperation::Lt
                }
            }
            LessEqual => {
                if is_signed {
                    BinaryOperation::Slte
                } else {
                    BinaryOperation::Lte
                }
            }
            Greater => {
                if is_signed {
                    BinaryOperation::Sgt
                } else {
                    BinaryOperation::Gt
                }
            }
            GreaterEqual => {
                if is_signed {
                    BinaryOperation::Sgte
                } else {
                    BinaryOperation::Gte
                }
            }
            LeftShift => BinaryOperation::Shl,
            // BUG FIX #247: Use arithmetic right shift for signed values
            RightShift => {
                if is_signed {
                    BinaryOperation::Sar
                } else {
                    BinaryOperation::Shr
                }
            }
            LogicalAnd => BinaryOperation::And, // Boolean AND
            LogicalOr => BinaryOperation::Or,   // Boolean OR
        }
    }

    /// Convert MIR BinaryOp to SIR float BinaryOperation
    fn convert_binary_op_for_float(&self, op: &skalp_mir::BinaryOp) -> BinaryOperation {
        use skalp_mir::BinaryOp::*;
        match op {
            Add => BinaryOperation::FAdd,
            Sub => BinaryOperation::FSub,
            Mul => BinaryOperation::FMul,
            Div => BinaryOperation::FDiv,
            Mod => BinaryOperation::FMod,
            Equal => BinaryOperation::FEq,
            NotEqual => BinaryOperation::FNeq,
            Less => BinaryOperation::FLt,
            LessEqual => BinaryOperation::FLte,
            Greater => BinaryOperation::FGt,
            GreaterEqual => BinaryOperation::FGte,
            // Bitwise/logical/shift ops don't have float variants — fall back to integer
            _ => self.convert_binary_op_with_signedness(op, false),
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
    /// Convert MIR DataType to SIR SirType
    fn convert_type(&self, data_type: &DataType) -> SirType {
        match data_type {
            // Signed types: Int is signed
            DataType::Int(w) => SirType::SignedBits(*w),
            // Unsigned types: Bit, Logic, Nat are unsigned
            DataType::Bit(w) | DataType::Logic(w) | DataType::Nat(w) => SirType::Bits(*w),
            DataType::Bool => SirType::Bits(1),
            DataType::Clock { .. } | DataType::Reset { .. } | DataType::Event => SirType::Bits(1),
            DataType::Float16 => SirType::Float16,
            DataType::Float32 => SirType::Float32,
            DataType::Float64 => SirType::Float64,
            DataType::Vec2(elem) => SirType::Vec2(Box::new(self.convert_type(elem))),
            DataType::Vec3(elem) => SirType::Vec3(Box::new(self.convert_type(elem))),
            DataType::Vec4(elem) => SirType::Vec4(Box::new(self.convert_type(elem))),
            DataType::Array(elem, size) => SirType::Array(Box::new(self.convert_type(elem)), *size),
            // Signed parametric types
            DataType::IntParam { default, .. } | DataType::IntExpr { default, .. } => {
                SirType::SignedBits(*default)
            }
            // Unsigned parametric types
            DataType::BitParam { default, .. }
            | DataType::LogicParam { default, .. }
            | DataType::NatParam { default, .. }
            | DataType::BitExpr { default, .. }
            | DataType::LogicExpr { default, .. }
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
                    return SirType::Vec2(Box::new(elem_type));
                } else if (struct_name_lower == "vec3" || struct_name_lower == "vector3")
                    && struct_type.fields.len() >= 3
                {
                    let elem_type = self.convert_type(&struct_type.fields[0].field_type);
                    return SirType::Vec3(Box::new(elem_type));
                } else if (struct_name_lower == "vec4" || struct_name_lower == "vector4")
                    && struct_type.fields.len() >= 4
                {
                    let elem_type = self.convert_type(&struct_type.fields[0].field_type);
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

                SirType::Bits(total_width)
            }
            // Enum, Union are not yet supported for simulation
            DataType::Enum(_) | DataType::Union(_) => SirType::Bits(1),
            // NCL dual-rail type - physical width is 2x logical width
            DataType::Ncl(logical_width) => SirType::Bits(logical_width * 2),
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

        // Default to 32 bits (safer than 8 — see BUG #278)
        32
    }

    /// Get the output width of a node by looking at its output signal
    fn get_node_output_width(&self, node_id: usize) -> usize {
        // First check combinational nodes
        for node in &self.sir.combinational_nodes {
            if node.id == node_id {
                if let Some(output) = node.outputs.first() {
                    return self.get_signal_width(&output.signal_id);
                }
            }
        }
        // Check sequential nodes
        for node in &self.sir.sequential_nodes {
            if node.id == node_id {
                if let Some(output) = node.outputs.first() {
                    return self.get_signal_width(&output.signal_id);
                }
            }
        }
        // Default fallback
        32
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
        // Clone the instances list to avoid borrow checker issues
        let instances = self.mir.instances.clone();

        for instance in &instances {
            // Find the module being instantiated
            let child_module = self
                .mir_design
                .modules
                .iter()
                .find(|m| m.id == instance.module)
                .unwrap_or_else(|| {
                    let location = instance
                        .span
                        .as_ref()
                        .map(|s| format!(" at {}", s))
                        .unwrap_or_default();
                    panic!(
                        "Module {:?} not found for instance {}{}",
                        instance.module, instance.name, location
                    )
                });

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
        // BUG #208 FIX: Pass self.mir as parent context for top-level instances
        // The instance.connections contain SignalIds that belong to self.mir, not child_module.
        // Previously we passed None, which caused FP specialization to fail to resolve signals.
        let parent_mir = self.mir.clone();
        self.elaborate_instance_with_context(
            instance,
            child_module,
            inst_prefix,
            Some(&parent_mir),
            "",
        );
    }

    /// GPU OPTIMIZATION: Try to specialize FP module instances to native operations
    ///
    /// For GPU simulation, we want to use native floating-point operations instead of
    /// elaborating the full IEEE 754 logic (which is needed for RTL synthesis but slow for GPU).
    ///
    /// Recognized modules: FpAdd, FpSub, FpMul, FpDiv (and their monomorphized variants like FpAdd_fp32)
    ///
    /// Returns true if specialization was applied, false to fall back to normal elaboration.
    #[allow(unused_variables)]
    fn try_specialize_fp_instance(
        &mut self,
        instance: &skalp_mir::ModuleInstance,
        child_module: &Module,
        inst_prefix: &str,
        parent_module_for_signals: Option<&Module>,
        parent_prefix: &str,
    ) -> bool {
        let module_name = &child_module.name;

        // Determine FP width from module name
        let fp_width = if module_name.contains("fp64") || module_name.contains("IEEE754_64") {
            64
        } else if module_name.contains("fp16") || module_name.contains("IEEE754_16") {
            16
        } else {
            32 // Default to fp32
        };

        let sir_type = match fp_width {
            16 => SirType::Float16,
            64 => SirType::Float64,
            _ => SirType::Float32,
        };

        // Build port mapping for expression conversion
        let mut port_mapping: HashMap<String, Expression> = HashMap::new();
        for (port_name, expr) in &instance.connections {
            port_mapping.insert(port_name.clone(), expr.clone());
        }

        let module_for_lookup = parent_module_for_signals.unwrap_or(self.mir);

        // Check for different FP module types
        if module_name.starts_with("FpAdd")
            || module_name.starts_with("FpSub")
            || module_name.starts_with("FpMul")
            || module_name.starts_with("FpDiv")
        {
            // Binary FP operations: FpAdd, FpSub, FpMul, FpDiv
            let fp_op = if module_name.starts_with("FpAdd") {
                BinaryOperation::FAdd
            } else if module_name.starts_with("FpSub") {
                BinaryOperation::FSub
            } else if module_name.starts_with("FpMul") {
                BinaryOperation::FMul
            } else {
                BinaryOperation::FDiv
            };

            return self.specialize_fp_binary_op(
                instance,
                child_module,
                inst_prefix,
                parent_module_for_signals,
                parent_prefix,
                &port_mapping,
                module_for_lookup,
                fp_op,
                fp_width,
                sir_type,
            );
        } else if module_name.starts_with("FpSqrt") || module_name.starts_with("CordicSqrt") {
            // Unary FP operation: FpSqrt
            return self.specialize_fp_sqrt(
                instance,
                child_module,
                inst_prefix,
                parent_module_for_signals,
                parent_prefix,
                &port_mapping,
                module_for_lookup,
                fp_width,
                sir_type,
            );
        } else if module_name.starts_with("FpCompare") {
            // FP comparison: FpCompare
            return self.specialize_fp_compare(
                instance,
                child_module,
                inst_prefix,
                parent_module_for_signals,
                parent_prefix,
                &port_mapping,
                module_for_lookup,
                fp_width,
            );
        }

        false // Not an FP module we can specialize
    }

    /// Specialize binary FP operations (FpAdd, FpSub, FpMul, FpDiv)
    #[allow(clippy::too_many_arguments)]
    fn specialize_fp_binary_op(
        &mut self,
        instance: &skalp_mir::ModuleInstance,
        child_module: &Module,
        inst_prefix: &str,
        parent_module_for_signals: Option<&Module>,
        parent_prefix: &str,
        port_mapping: &HashMap<String, Expression>,
        module_for_lookup: &Module,
        fp_op: BinaryOperation,
        fp_width: usize,
        sir_type: SirType,
    ) -> bool {
        // Get connections: a, b, result
        let (a_expr, b_expr, result_expr) = match (
            instance.connections.get("a"),
            instance.connections.get("b"),
            instance.connections.get("result"),
        ) {
            (Some(a), Some(b), Some(r)) => (a, b, r),
            _ => {
                return false;
            }
        };

        // Convert inputs - strip incorrect FP casts for non-FP32 widths
        // The MIR compiler may resolve fp<IEEE754_16> as Float32 instead of Float16,
        // causing incorrect zero-extension. Strip the cast when we know the correct width.
        let a_input = self.strip_fp_cast_if_needed(a_expr, fp_width);
        let b_input = self.strip_fp_cast_if_needed(b_expr, fp_width);
        let a_node = self.create_expression_node_for_instance_with_context(
            &a_input,
            inst_prefix,
            port_mapping,
            child_module,
            parent_module_for_signals,
            parent_prefix,
        );
        let b_node = self.create_expression_node_for_instance_with_context(
            &b_input,
            inst_prefix,
            port_mapping,
            child_module,
            parent_module_for_signals,
            parent_prefix,
        );

        // Get output signal name (MIR name) and translate to SIR internal name
        let output_signal_name_mir = self
            .get_output_signal_name(result_expr, module_for_lookup, parent_prefix)
            .unwrap_or_else(|| {
                self.create_fallback_signal(inst_prefix, "result", fp_width, &sir_type)
            });
        let output_signal_name = self.translate_to_internal_name(&output_signal_name_mir);

        // Create the native FP binary operation node
        let a_signal = self.node_to_signal_ref(a_node);
        let b_signal = self.node_to_signal_ref(b_node);
        let output_signal = SignalRef {
            signal_id: output_signal_name.clone(),
            bit_range: None,
        };

        let node_id = self.next_node_id();

        // BUG #210 FIX: Also include node_N_out as an output so that when mux nodes
        // reference this node via node_to_signal_ref, they get a valid connection.
        // Without this, node_to_signal_ref creates a new unconnected node_N_out signal.
        let node_out_signal = SignalRef {
            signal_id: format!("node_{}_out", node_id),
            bit_range: None,
        };

        let node = SirNode {
            id: node_id,
            kind: SirNodeKind::BinaryOp(fp_op.clone()),
            inputs: vec![a_signal, b_signal],
            outputs: vec![output_signal, node_out_signal],
            clock_domain: None,
            impl_style_hint: ImplStyleHint::default(),
            span: None,
        };

        self.sir.combinational_nodes.push(node);
        self.update_signal_driver(&output_signal_name, node_id);
        // BUG #210 FIX: Also update driver for node_N_out
        self.update_signal_driver(&format!("node_{}_out", node_id), node_id);

        true
    }

    /// Specialize FpSqrt to native FSqrt
    #[allow(clippy::too_many_arguments)]
    fn specialize_fp_sqrt(
        &mut self,
        instance: &skalp_mir::ModuleInstance,
        child_module: &Module,
        inst_prefix: &str,
        parent_module_for_signals: Option<&Module>,
        parent_prefix: &str,
        port_mapping: &HashMap<String, Expression>,
        module_for_lookup: &Module,
        fp_width: usize,
        sir_type: SirType,
    ) -> bool {
        // FpSqrt has: in x, out result, out flags
        // CordicSqrt uses "value" instead of "x" for the input port
        let (x_expr, result_expr) = match (
            instance
                .connections
                .get("x")
                .or_else(|| instance.connections.get("value")),
            instance.connections.get("result"),
        ) {
            (Some(x), Some(r)) => (x, r),
            _ => {
                return false;
            }
        };

        // Convert input - strip incorrect FP casts for non-FP32 widths
        let x_input = self.strip_fp_cast_if_needed(x_expr, fp_width);
        let x_node = self.create_expression_node_for_instance_with_context(
            &x_input,
            inst_prefix,
            port_mapping,
            child_module,
            parent_module_for_signals,
            parent_prefix,
        );

        // Get output signal name (MIR name) and translate to SIR internal name
        let output_signal_name_mir = self
            .get_output_signal_name(result_expr, module_for_lookup, parent_prefix)
            .unwrap_or_else(|| {
                self.create_fallback_signal(inst_prefix, "result", fp_width, &sir_type)
            });
        let output_signal_name = self.translate_to_internal_name(&output_signal_name_mir);

        // Create the native FSqrt unary operation node
        let x_signal = self.node_to_signal_ref(x_node);
        let output_signal = SignalRef {
            signal_id: output_signal_name.clone(),
            bit_range: None,
        };

        let node_id = self.next_node_id();

        // BUG #210 FIX: Also include node_N_out as an output for mux connections
        let node_out_signal = SignalRef {
            signal_id: format!("node_{}_out", node_id),
            bit_range: None,
        };

        let node = SirNode {
            id: node_id,
            kind: SirNodeKind::UnaryOp(UnaryOperation::FSqrt),
            inputs: vec![x_signal],
            outputs: vec![output_signal, node_out_signal],
            clock_domain: None,
            impl_style_hint: ImplStyleHint::default(),
            span: None,
        };

        self.sir.combinational_nodes.push(node);
        self.update_signal_driver(&output_signal_name, node_id);
        // BUG #210 FIX: Also update driver for node_N_out
        self.update_signal_driver(&format!("node_{}_out", node_id), node_id);

        true
    }

    /// Specialize FpCompare to native FP comparison operations
    #[allow(clippy::too_many_arguments)]
    fn specialize_fp_compare(
        &mut self,
        instance: &skalp_mir::ModuleInstance,
        child_module: &Module,
        inst_prefix: &str,
        parent_module_for_signals: Option<&Module>,
        parent_prefix: &str,
        port_mapping: &HashMap<String, Expression>,
        module_for_lookup: &Module,
        _fp_width: usize,
    ) -> bool {
        // FpCompare has: in a, in b, out lt, out eq, out gt, out unordered
        let (a_expr, b_expr) = match (instance.connections.get("a"), instance.connections.get("b"))
        {
            (Some(a), Some(b)) => (a, b),
            _ => {
                return false;
            }
        };

        // Convert inputs - strip incorrect FP casts for non-FP32 widths
        let a_input = self.strip_fp_cast_if_needed(a_expr, _fp_width);
        let b_input = self.strip_fp_cast_if_needed(b_expr, _fp_width);
        let a_node = self.create_expression_node_for_instance_with_context(
            &a_input,
            inst_prefix,
            port_mapping,
            child_module,
            parent_module_for_signals,
            parent_prefix,
        );
        let b_node = self.create_expression_node_for_instance_with_context(
            &b_input,
            inst_prefix,
            port_mapping,
            child_module,
            parent_module_for_signals,
            parent_prefix,
        );

        let a_signal = self.node_to_signal_ref(a_node);
        let b_signal = self.node_to_signal_ref(b_node);

        let mut created_ops = Vec::new();

        // Create comparison operations for each connected output
        // lt output -> FLt
        if let Some(lt_expr) = instance.connections.get("lt") {
            if let Some(output_name_mir) =
                self.get_output_signal_name(lt_expr, module_for_lookup, parent_prefix)
            {
                let output_name = self.translate_to_internal_name(&output_name_mir);
                let output_signal = SignalRef {
                    signal_id: output_name.clone(),
                    bit_range: None,
                };
                let node_id = self.next_node_id();
                // BUG #210 FIX: Also include node_N_out for mux connections
                let node_out_signal = SignalRef {
                    signal_id: format!("node_{}_out", node_id),
                    bit_range: None,
                };
                let node = SirNode {
                    id: node_id,
                    kind: SirNodeKind::BinaryOp(BinaryOperation::FLt),
                    inputs: vec![a_signal.clone(), b_signal.clone()],
                    outputs: vec![output_signal, node_out_signal],
                    clock_domain: None,
                    impl_style_hint: ImplStyleHint::default(),
                    span: None,
                };
                self.sir.combinational_nodes.push(node);
                self.update_signal_driver(&output_name, node_id);
                self.update_signal_driver(&format!("node_{}_out", node_id), node_id);
                created_ops.push(("FLt", node_id, output_name));
            }
        }

        // eq output -> FEq
        if let Some(eq_expr) = instance.connections.get("eq") {
            if let Some(output_name_mir) =
                self.get_output_signal_name(eq_expr, module_for_lookup, parent_prefix)
            {
                let output_name = self.translate_to_internal_name(&output_name_mir);
                let output_signal = SignalRef {
                    signal_id: output_name.clone(),
                    bit_range: None,
                };
                let node_id = self.next_node_id();
                // BUG #210 FIX: Also include node_N_out for mux connections
                let node_out_signal = SignalRef {
                    signal_id: format!("node_{}_out", node_id),
                    bit_range: None,
                };
                let node = SirNode {
                    id: node_id,
                    kind: SirNodeKind::BinaryOp(BinaryOperation::FEq),
                    inputs: vec![a_signal.clone(), b_signal.clone()],
                    outputs: vec![output_signal, node_out_signal],
                    clock_domain: None,
                    impl_style_hint: ImplStyleHint::default(),
                    span: None,
                };
                self.sir.combinational_nodes.push(node);
                self.update_signal_driver(&output_name, node_id);
                self.update_signal_driver(&format!("node_{}_out", node_id), node_id);
                created_ops.push(("FEq", node_id, output_name));
            }
        }

        // gt output -> FGt
        if let Some(gt_expr) = instance.connections.get("gt") {
            if let Some(output_name_mir) =
                self.get_output_signal_name(gt_expr, module_for_lookup, parent_prefix)
            {
                let output_name = self.translate_to_internal_name(&output_name_mir);
                let output_signal = SignalRef {
                    signal_id: output_name.clone(),
                    bit_range: None,
                };
                let node_id = self.next_node_id();
                // BUG #210 FIX: Also include node_N_out for mux connections
                let node_out_signal = SignalRef {
                    signal_id: format!("node_{}_out", node_id),
                    bit_range: None,
                };
                let node = SirNode {
                    id: node_id,
                    kind: SirNodeKind::BinaryOp(BinaryOperation::FGt),
                    inputs: vec![a_signal.clone(), b_signal.clone()],
                    outputs: vec![output_signal, node_out_signal],
                    clock_domain: None,
                    impl_style_hint: ImplStyleHint::default(),
                    span: None,
                };
                self.sir.combinational_nodes.push(node);
                self.update_signal_driver(&output_name, node_id);
                self.update_signal_driver(&format!("node_{}_out", node_id), node_id);
                created_ops.push(("FGt", node_id, output_name));
            }
        }

        // Note: 'unordered' output (NaN check) is not directly supported as a native op
        // We skip it for GPU simulation - NaN handling uses native FP behavior

        if created_ops.is_empty() {
            return false;
        }

        true
    }

    /// Helper: Strip incorrect FP casts from connection expressions
    /// The MIR compiler may resolve fp<IEEE754_16> as Float32 instead of Float16,
    /// causing widening casts that zero-extend 16-bit values to 32 bits.
    /// When we know the correct FP width from the module name, strip these casts.
    fn strip_fp_cast_if_needed(&self, expr: &Expression, fp_width: usize) -> Expression {
        if let ExpressionKind::Cast {
            expr: inner,
            target_type,
        } = &expr.kind
        {
            let cast_width = match target_type {
                DataType::Float16 => 16,
                DataType::Float32 => 32,
                DataType::Float64 => 64,
                DataType::Bit(w) => *w,
                _ => 0,
            };
            // If the cast is to a wider FP type than the actual FP width, strip it
            if cast_width > fp_width && fp_width > 0 {
                return (**inner).clone();
            }
        }
        expr.clone()
    }

    /// Helper: Get output signal name from an expression
    fn get_output_signal_name(
        &self,
        expr: &Expression,
        module_for_lookup: &Module,
        parent_prefix: &str,
    ) -> Option<String> {
        match &expr.kind {
            skalp_mir::ExpressionKind::Ref(lvalue) => match lvalue {
                skalp_mir::LValue::Signal(sig_id) => module_for_lookup
                    .signals
                    .iter()
                    .find(|s| s.id == *sig_id)
                    .map(|s| {
                        if parent_prefix.is_empty() {
                            s.name.clone()
                        } else if parent_prefix.ends_with('.') {
                            // BUG #186 FIX: parent_prefix already has trailing dot, don't add another
                            format!("{}{}", parent_prefix, s.name)
                        } else {
                            format!("{}.{}", parent_prefix, s.name)
                        }
                    }),
                skalp_mir::LValue::Port(port_id) => module_for_lookup
                    .ports
                    .iter()
                    .find(|p| p.id == *port_id)
                    .map(|p| {
                        if parent_prefix.is_empty() {
                            p.name.clone()
                        } else if parent_prefix.ends_with('.') {
                            // BUG #186 FIX: parent_prefix already has trailing dot, don't add another
                            format!("{}{}", parent_prefix, p.name)
                        } else {
                            format!("{}.{}", parent_prefix, p.name)
                        }
                    }),
                skalp_mir::LValue::Variable(var_id) => module_for_lookup
                    .variables
                    .iter()
                    .find(|v| v.id == *var_id)
                    .map(|v| {
                        if parent_prefix.is_empty() {
                            v.name.clone()
                        } else if parent_prefix.ends_with('.') {
                            // BUG #186 FIX: parent_prefix already has trailing dot, don't add another
                            format!("{}{}", parent_prefix, v.name)
                        } else {
                            format!("{}.{}", parent_prefix, v.name)
                        }
                    }),
                _ => None,
            },
            _ => None,
        }
    }

    /// Helper: Create a fallback signal for instance output
    fn create_fallback_signal(
        &mut self,
        inst_prefix: &str,
        port_name: &str,
        width: usize,
        sir_type: &SirType,
    ) -> String {
        let name = format!("{}.{}", inst_prefix, port_name);
        if !self.sir.signals.iter().any(|s| s.name == name) {
            self.sir.signals.push(SirSignal {
                name: name.clone(),
                width,
                sir_type: sir_type.clone(),
                is_state: false,
                driver_node: None,
                fanout_nodes: Vec::new(),
                span: None,
            });
        }
        name
    }

    /// Helper: Update a signal's driver_node
    fn update_signal_driver(&mut self, signal_name: &str, node_id: usize) {
        if let Some(sig) = self.sir.signals.iter_mut().find(|s| s.name == signal_name) {
            sig.driver_node = Some(node_id);
        }
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
        // BUG FIX: Detect circular elaboration to prevent infinite recursion
        // This can happen when trait implementations create entity instances that
        // indirectly reference back to the original entity
        if self.elaborated_instances.contains(inst_prefix) {
            return;
        }
        self.elaborated_instances.insert(inst_prefix.to_string());

        // GPU OPTIMIZATION: Specialize FP module instances to native operations
        // Instead of elaborating full IEEE 754 logic, use native FP operations for GPU simulation
        if self.try_specialize_fp_instance(
            instance,
            child_module,
            inst_prefix,
            parent_module_for_signals,
            parent_prefix,
        ) {
            return;
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

            // Create SIR signals for each flattened component
            for flat_signal in flattened_signals {
                // Hierarchical path for instance signals: inst_prefix.signal_name
                let hierarchical_path = format!("{}.{}", inst_prefix, flat_signal.name);
                let sir_type = self.convert_type(&flat_signal.signal_type);
                let width = sir_type.width();

                // Register in name registry for testbench path resolution
                // BUG #117r FIX: Instance-flattened signals need registry for proper path resolution
                let name_kind = if is_register {
                    NameKind::StateElement
                } else {
                    NameKind::Signal
                };
                let internal_name = self.sir.name_registry.register(
                    &hierarchical_path,
                    &flat_signal.name, // source name for debugging
                    name_kind,
                    width,
                );

                // Store mapping from hierarchical path to internal name
                self.mir_to_internal_name
                    .insert(hierarchical_path.clone(), internal_name.clone());

                self.sir.signals.push(SirSignal {
                    name: internal_name.clone(),
                    width,
                    sir_type: sir_type.clone(),
                    driver_node: None,
                    fanout_nodes: Vec::new(),
                    is_state: is_register,
                    span: None,
                });

                if is_register {
                    self.sir.state_elements.insert(
                        internal_name.clone(),
                        StateElement {
                            name: internal_name,
                            width,
                            sir_type: Some(sir_type.clone()),
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
        // BUG FIX #211: Use unique variable names with ID suffix to match reference naming
        for variable in &child_module.variables {
            // Use the same naming convention as get_signal_from_lvalue_with_context:
            // format!("{}_{}", var.name, var_id.0)
            let var_unique_name = format!("{}_{}", variable.name, variable.id.0);
            // Hierarchical path for instance variables
            let hierarchical_path = format!("{}.{}", inst_prefix, var_unique_name);
            let sir_type = self.convert_type(&variable.var_type);
            let width = sir_type.width();

            // Register in name registry for testbench path resolution
            // BUG #117r FIX: Instance variables need registry for proper path resolution
            let internal_name = self.sir.name_registry.register(
                &hierarchical_path,
                &variable.name, // source name for debugging
                NameKind::Variable,
                width,
            );

            // Store mapping from hierarchical path to internal name
            self.mir_to_internal_name
                .insert(hierarchical_path.clone(), internal_name.clone());

            self.sir.signals.push(SirSignal {
                name: internal_name.clone(),
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
                // Hierarchical path for instance input ports
                let hierarchical_path = format!("{}.{}", inst_prefix, port.name);
                let sir_type = self.convert_type(&port.port_type);
                let width = sir_type.width();

                // Check if already registered (avoid duplicates)
                if self.mir_to_internal_name.contains_key(&hierarchical_path) {
                    continue;
                }

                // Register in name registry for testbench path resolution
                // BUG #117r FIX: Instance input ports need registry for proper path resolution
                let internal_name = self.sir.name_registry.register(
                    &hierarchical_path,
                    &port.name, // source name for debugging
                    NameKind::Input,
                    width,
                );

                // Store mapping from hierarchical path to internal name
                self.mir_to_internal_name
                    .insert(hierarchical_path.clone(), internal_name.clone());

                self.sir.signals.push(SirSignal {
                    name: internal_name.clone(),
                    width,
                    sir_type: sir_type.clone(),
                    driver_node: None,
                    fanout_nodes: Vec::new(),
                    is_state: false, // Input ports are not registers
                    span: None,
                });

                // BUG #236 FIX: Connect the parent's expression to the child's input port signal
                // When a child module's input port is connected to a parent signal (e.g., clear: clear_faults),
                // we need to create a driver that routes the parent's signal to the child's input.
                // Without this, the child's input port signal remains undriven (always 0).
                if let Some(parent_expr) = instance.connections.get(&port.name) {
                    // BUG #276 FIX: Use the parent instance's port mapping so that references to
                    // connected output ports (e.g., current_magnitude) resolve through the mapping
                    // chain to the grandparent signal (e.g., magnitude_out). Without this, a
                    // grandchild's input connected to a parent's connected output port reads from
                    // an undriven intermediate signal (always 0).
                    let parent_mapping_key = parent_prefix.trim_end_matches('.').to_string();
                    let parent_port_mapping = self
                        .instance_port_mappings
                        .get(&parent_mapping_key)
                        .cloned()
                        .unwrap_or_default();
                    // Create a node for the parent expression
                    let parent_node = self.create_expression_node_for_instance_with_context(
                        parent_expr,
                        parent_prefix,
                        &parent_port_mapping,
                        parent_module_for_signals.unwrap_or(self.mir),
                        Some(self.mir),
                        "",
                    );
                    // Connect it to the child's input port signal
                    self.connect_node_to_signal(parent_node, &internal_name);
                }
            }
        }

        // Step 1d: Create signals for OUTPUT ports of the child module
        // BUG #185 FIX: Output port signals need to be declared so they can be used
        // as assignment targets when the port isn't connected to a parent signal.
        // This happens with trait method inlining where entity instances have their
        // output ports assigned internally (e.g., FpSub's `flags` and `result` ports).
        for port in &child_module.ports {
            if matches!(port.direction, skalp_mir::PortDirection::Output) {
                // Hierarchical path for instance output ports
                let hierarchical_path = format!("{}.{}", inst_prefix, port.name);
                let sir_type = self.convert_type(&port.port_type);
                let width = sir_type.width();

                // Check if already registered (avoid duplicates)
                if self.mir_to_internal_name.contains_key(&hierarchical_path) {
                    continue;
                }

                // BUG #117r FIX: Check if this port (or its parent struct port) is connected
                // For flattened ports like "faults_bms_fault", we need to check if any parent
                // prefix (e.g., "faults" or "faults_bms") is in connections.
                // This handles nested struct flattening where names have multiple underscores.
                //
                // BUG #249 FIX: Also check for double underscore "__" which is used for struct flattening
                let is_connected = if instance.connections.contains_key(&port.name) {
                    true
                } else {
                    // Check if any prefix ending with "_" or "__" matches a connection
                    // For "faults__bms_fault", check: "faults", "faults__bms"
                    let mut found = false;

                    // First check double underscore (struct flattening)
                    if let Some(pos) = port.name.find("__") {
                        let prefix = &port.name[..pos];
                        if instance.connections.contains_key(prefix) {
                            found = true;
                        }
                    }

                    // Also check single underscore for backward compatibility
                    // BUG #278 FIX: Only treat as struct-flattened connection if the prefix
                    // is NOT itself a complete port in the child module. Otherwise, ports like
                    // "current_magnitude" falsely match "current" (a separate input port),
                    // causing the output port signal to be skipped with wrong default width.
                    if !found {
                        let mut remaining = &port.name[..];
                        while let Some(underscore_pos) = remaining.find('_') {
                            let prefix =
                                &port.name[..port.name.len() - remaining.len() + underscore_pos];
                            if instance.connections.contains_key(prefix) {
                                // Verify this is actually a struct prefix, not a coincidental
                                // name collision with another port
                                let prefix_is_separate_port =
                                    child_module.ports.iter().any(|p| p.name == prefix);
                                if !prefix_is_separate_port {
                                    found = true;
                                    break;
                                }
                            }
                            remaining = &remaining[underscore_pos + 1..];
                        }
                    }
                    found
                };

                // Only create if not connected to parent (handled via port_mapping)
                if !is_connected {
                    // Register in name registry for testbench path resolution
                    // BUG #117r FIX: Instance output ports need registry for proper path resolution
                    let internal_name = self.sir.name_registry.register(
                        &hierarchical_path,
                        &port.name, // source name for debugging
                        NameKind::Output,
                        width,
                    );

                    // Store mapping from hierarchical path to internal name
                    self.mir_to_internal_name
                        .insert(hierarchical_path.clone(), internal_name.clone());

                    self.sir.signals.push(SirSignal {
                        name: internal_name.clone(),
                        width,
                        sir_type: sir_type.clone(),
                        driver_node: None,
                        fanout_nodes: Vec::new(),
                        is_state: false, // Output ports are not registers
                        span: None,
                    });
                }
            }
        }

        // BUG FIX #124: Store the basic port_mapping BEFORE elaborating nested instances
        // This ensures nested instances can look up the parent's port_mapping during their elaboration
        // The full port_mapping (with flattened fields) is built later in elaborate_child_logic_with_context,
        // but we need the basic mapping available for nested Port resolution now.
        {
            let mut basic_port_mapping: HashMap<String, Expression> = HashMap::new();
            for (port_name, parent_expr) in &instance.connections {
                basic_port_mapping.insert(port_name.clone(), parent_expr.clone());
            }
            self.instance_port_mappings
                .insert(inst_prefix.to_string(), basic_port_mapping);

            // BUG FIX #209: Also store the parent module ID so we can look up the correct
            // grandparent module when resolving 3+ levels of hierarchy
            if let Some(parent_module) = parent_module_for_signals {
                self.instance_parent_module_ids
                    .insert(inst_prefix.to_string(), parent_module.id);
            }
        }

        // Step 2: FIRST recursively elaborate any instances within the child
        // BUG FIX #85: This MUST happen before Step 3 so that nested module outputs
        // have their drivers set up before we process TupleFieldAccess expressions
        // that reference those outputs
        // IMPORTANT: Pass child_module as the parent context for nested instances
        // The nested instances' connections have SignalIds from child_module
        if !child_module.instances.is_empty() {
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
        self.elaborate_child_logic_with_context(
            child_module,
            inst_prefix,
            instance,
            parent_module_for_signals,
            parent_prefix,
        );
    }

    /// Elaborate child module's logic (assignments and processes) with instance prefix
    #[allow(dead_code)]
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
            //   "wr_data__x" → write_vertex__x
            //   "wr_data__y" → write_vertex__y
            //   "wr_data__z" → write_vertex__z
            //
            // Find all child ports that start with port_name__ (flattened variants)
            // BUG #XXX FIX: MIR uses DOUBLE underscores for struct field flattening
            let port_prefix_double = format!("{}_", port_name); // Try single first for backward compat
            let port_prefix_single = format!("{}__", port_name); // Double underscore is standard
            for child_port in &child_module.ports {
                let matches_single = child_port.name.starts_with(&port_prefix_single);
                let matches_double = child_port.name.starts_with(&port_prefix_double);
                if matches_single || matches_double {
                    // This is a flattened field port like "wr_data__x"
                    // Get the suffix (e.g., "x" from "wr_data__x")
                    let prefix_len = if matches_double {
                        port_prefix_double.len()
                    } else {
                        port_prefix_single.len()
                    };
                    let suffix = &child_port.name[prefix_len..];

                    // Create corresponding parent signal name
                    // If parent_expr is a Ref to a signal/port, create Ref to signal_suffix
                    // Parent signals/ports are in self.mir (the parent module being elaborated)
                    let parent_flattened_expr = if let ExpressionKind::Ref(lval) = &parent_expr.kind
                    {
                        if let LValue::Signal(parent_sig_id) = lval {
                            // Find parent signal in MIR module
                            let parent_sig_opt =
                                self.mir.signals.iter().find(|s| s.id == *parent_sig_id);

                            if let Some(parent_sig) = parent_sig_opt {
                                // CRITICAL: The parent might ALREADY be flattened (e.g., "wr_data_x")
                                // If so, we need to REPLACE the last field, not append
                                let parent_flattened_name =
                                    if let Some((base, idx, current_field)) =
                                        self.parse_flattened_name(&parent_sig.name)
                                    {
                                        if !current_field.is_empty() {
                                            // Parent is already flattened - replace the field
                                            // Reconstruct with new field
                                            // BUG FIX: Use double underscore for struct field flattening
                                            if idx.is_empty() {
                                                format!("{}__{}", base, suffix)
                                            } else {
                                                format!("{}__{}_{}", base, idx, suffix)
                                            }
                                        } else {
                                            // Not flattened - append suffix with double underscore
                                            format!("{}__{}", parent_sig.name, suffix)
                                        }
                                    } else {
                                        // Not a flattened name - append suffix with double underscore
                                        format!("{}__{}", parent_sig.name, suffix)
                                    };

                                let parent_flattened_sig_opt = self
                                    .mir
                                    .signals
                                    .iter()
                                    .find(|s| s.name == parent_flattened_name);

                                parent_flattened_sig_opt.map(|s| {
                                    Expression::with_unknown_type(ExpressionKind::Ref(
                                        LValue::Signal(s.id),
                                    ))
                                })
                            } else {
                                None
                            }
                        } else if let LValue::Port(parent_port_id) = lval {
                            // Find parent port in MIR module
                            let orig_port_opt =
                                self.mir.ports.iter().find(|p| p.id == *parent_port_id);

                            if let Some(orig_port) = orig_port_opt {
                                // Same fix as for signals - check if already flattened
                                // BUG #225 FIX: Use double underscores for struct field flattening (consistent with signals)
                                let parent_flattened_name =
                                    if let Some((base, idx, current_field)) =
                                        self.parse_flattened_name(&orig_port.name)
                                    {
                                        if !current_field.is_empty() {
                                            // Port is already flattened - replace the field
                                            if idx.is_empty() {
                                                format!("{}__{}", base, suffix)
                                            } else {
                                                format!("{}__{}_{}", base, idx, suffix)
                                            }
                                        } else {
                                            format!("{}__{}", orig_port.name, suffix)
                                        }
                                    } else {
                                        format!("{}__{}", orig_port.name, suffix)
                                    };

                                let parent_flattened_port_opt = self
                                    .mir
                                    .ports
                                    .iter()
                                    .find(|p| p.name == parent_flattened_name);

                                parent_flattened_port_opt.map(|p| {
                                    Expression::with_unknown_type(ExpressionKind::Ref(
                                        LValue::Port(p.id),
                                    ))
                                })
                            } else {
                                None
                            }
                        } else if let LValue::RangeSelect {
                            base: base_lval,
                            high: _,
                            low: _,
                        } = lval
                        {
                            // BUG #225 FIX: Handle RangeSelect (struct field passed to submodule)
                            // MIR represents config.protection as RangeSelect(config, [512:257])
                            // We need to find the flattened parent port that matches the child field

                            // Get the base port name
                            let base_port_name = if let LValue::Port(port_id) = base_lval.as_ref() {
                                self.mir
                                    .ports
                                    .iter()
                                    .find(|p| p.id == *port_id)
                                    .map(|p| p.name.clone())
                            } else {
                                None
                            };

                            if let Some(base_name) = base_port_name {
                                // The suffix is the child field name (e.g., "v_ov_soft" from "thresholds__v_ov_soft")
                                // We need to find a parent port that ends with this suffix
                                // Pattern: {base}__{something}__{suffix}
                                let suffix_with_double =
                                    format!("__{}", suffix.trim_start_matches('_'));

                                // Search for a matching flattened port
                                let parent_flattened_port_opt = self.mir.ports.iter().find(|p| {
                                    p.name.starts_with(base_name.as_str())
                                        && p.name.ends_with(&suffix_with_double)
                                });

                                if let Some(p) = parent_flattened_port_opt {
                                    Some(Expression::with_unknown_type(ExpressionKind::Ref(
                                        LValue::Port(p.id),
                                    )))
                                } else {
                                    // Try signals too
                                    let parent_flattened_sig_opt =
                                        self.mir.signals.iter().find(|s| {
                                            s.name.starts_with(&base_name)
                                                && s.name.ends_with(&suffix_with_double)
                                        });

                                    parent_flattened_sig_opt.map(|s| {
                                        Expression::with_unknown_type(ExpressionKind::Ref(
                                            LValue::Signal(s.id),
                                        ))
                                    })
                                }
                            } else {
                                None
                            }
                        } else {
                            None
                        }
                    } else if let ExpressionKind::FieldAccess {
                        base,
                        field: field_name,
                    } = &parent_expr.kind
                    {
                        // BUG #225 FIX: Handle field accesses like config.protection
                        // Recursively resolve the base expression to get the base port/signal name

                        fn resolve_expr_to_base_name(
                            expr: &Expression,
                            mir: &skalp_mir::Module,
                        ) -> Option<String> {
                            match &expr.kind {
                                ExpressionKind::Ref(LValue::Port(port_id)) => mir
                                    .ports
                                    .iter()
                                    .find(|p| p.id == *port_id)
                                    .map(|p| p.name.clone()),
                                ExpressionKind::Ref(LValue::Signal(sig_id)) => mir
                                    .signals
                                    .iter()
                                    .find(|s| s.id == *sig_id)
                                    .map(|s| s.name.clone()),
                                ExpressionKind::FieldAccess {
                                    base: inner_base,
                                    field: fname,
                                } => resolve_expr_to_base_name(inner_base, mir)
                                    .map(|base| format!("{}__{}", base, fname)),
                                _ => None,
                            }
                        }

                        if let Some(base_name) = resolve_expr_to_base_name(base, self.mir) {
                            // Construct the full flattened name: base__field__suffix
                            // The suffix already has the leading underscore stripped, so we need field + suffix
                            let parent_flattened_name =
                                format!("{}__{}__{}", base_name, field_name, suffix);

                            // First try to find it as a port
                            let parent_flattened_port_opt = self
                                .mir
                                .ports
                                .iter()
                                .find(|p| p.name == parent_flattened_name);

                            if let Some(p) = parent_flattened_port_opt {
                                Some(Expression::with_unknown_type(ExpressionKind::Ref(
                                    LValue::Port(p.id),
                                )))
                            } else {
                                // Try as a signal
                                let parent_flattened_sig_opt = self
                                    .mir
                                    .signals
                                    .iter()
                                    .find(|s| s.name == parent_flattened_name);

                                parent_flattened_sig_opt.map(|s| {
                                    Expression::with_unknown_type(ExpressionKind::Ref(
                                        LValue::Signal(s.id),
                                    ))
                                })
                            }
                        } else {
                            None
                        }
                    } else {
                        None
                    };

                    if let Some(expr) = parent_flattened_expr {
                        port_mapping.insert(child_port.name.clone(), expr);
                    }
                }
            }
        }

        // BUG FIX #124: Store the port_mapping for this instance so nested instances can look it up
        // This enables recursive port resolution for 3-level nesting (e.g., CLE → exec_l4_l5 → quadratic_solve)
        self.instance_port_mappings
            .insert(inst_prefix.to_string(), port_mapping.clone());

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
    #[allow(dead_code)]
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
        // Translate LHS: if it's a port (output), map to parent signal
        // Otherwise prefix with instance name
        let lhs_signal = match &assign.lhs {
            LValue::Signal(sig_id) => {
                if let Some(signal) = child_module.signals.iter().find(|s| s.id == *sig_id) {
                    // BUG #185 FIX: Check if this signal shares a name with a port that is in port_mapping
                    // This happens when an output port like 'result' is used as assignment LHS.
                    // If the port is mapped to a parent signal, use that instead of the instance-prefixed name.
                    if let Some(parent_expr) = port_mapping.get(&signal.name) {
                        self.get_signal_name_from_expression_with_context(
                            parent_expr,
                            parent_module_for_signals,
                            parent_prefix,
                        )
                    } else {
                        format!("{}.{}", inst_prefix, signal.name)
                    }
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
    /// BUG FIX #210: Pass parent context to properly resolve multi-level clock port mappings
    fn convert_child_process_with_context(
        &mut self,
        process: &skalp_mir::Process,
        inst_prefix: &str,
        port_mapping: &HashMap<String, Expression>,
        child_module: &Module,
        parent_module_for_signals: Option<&Module>,
        parent_prefix: &str,
    ) {
        // Actually use the parent context for proper hierarchical resolution
        self.convert_child_process_impl(
            process,
            inst_prefix,
            port_mapping,
            child_module,
            parent_module_for_signals,
            parent_prefix,
        );
    }

    /// Convert child process (always block) with instance prefix
    #[allow(dead_code)]
    fn convert_child_process(
        &mut self,
        process: &skalp_mir::Process,
        inst_prefix: &str,
        port_mapping: &HashMap<String, Expression>,
        child_module: &Module,
    ) {
        // Call without parent context (for top-level instances)
        self.convert_child_process_impl(process, inst_prefix, port_mapping, child_module, None, "");
    }

    /// Implementation of child process conversion with optional parent context
    /// BUG FIX #210: For nested instances, parent_module_for_signals contains the module
    /// that owns the SignalIds in port_mapping values. This is needed to properly resolve
    /// multi-level hierarchical port mappings (e.g., grandparent -> parent -> child for clocks).
    fn convert_child_process_impl(
        &mut self,
        process: &skalp_mir::Process,
        inst_prefix: &str,
        port_mapping: &HashMap<String, Expression>,
        child_module: &Module,
        parent_module_for_signals: Option<&Module>,
        parent_prefix: &str,
    ) {
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
                    // BUG FIX #210: Use context-aware version to properly resolve multi-level hierarchy
                    let clock_lvalue = &edge_sens.signal;

                    let clock_signal_mir = if let Some(sig_name) = self
                        .get_signal_from_lvalue_with_context(
                            clock_lvalue,
                            inst_prefix,
                            port_mapping,
                            child_module,
                            parent_module_for_signals,
                            parent_prefix,
                        ) {
                        sig_name
                    } else {
                        "clk".to_string()
                    };
                    // Translate MIR clock name to internal name for SIR
                    let clock_signal = self.translate_to_internal_name(&clock_signal_mir);

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
                                parent_module_for_signals,
                                parent_prefix,
                            );

                            let node_id = self.node_counter;
                            self.node_counter += 1;

                            // BUG #117r FIX: For output ports connected to parent signals,
                            // resolve through port_mapping to get the correct output signal.
                            // This ensures the FlipFlop outputs to the parent's signal, not an
                            // internal hierarchical path that doesn't exist.
                            let local_target = actual_target
                                .strip_prefix(inst_prefix)
                                .and_then(|s| s.strip_prefix('.'))
                                .unwrap_or(&actual_target);

                            // Check if this target is an output port with a connection
                            let internal_target = if let Some(parent_expr) =
                                port_mapping.get(local_target)
                            {
                                // Output port connected to parent - use parent's signal
                                let parent_sig = self.get_signal_name_from_expression_with_context(
                                    parent_expr,
                                    parent_module_for_signals,
                                    parent_prefix,
                                );
                                self.translate_to_internal_name(&parent_sig)
                            } else {
                                // Not connected to parent - use hierarchical name
                                self.translate_to_internal_name(&actual_target)
                            };

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
                                    signal_id: internal_target.clone(),
                                    bit_range: None,
                                }],
                                clock_domain: Some(clock_signal.clone()),
                                impl_style_hint: ImplStyleHint::default(),
                                span: None,
                            };

                            self.sir.sequential_nodes.push(ff_node);

                            // BUG #117r FIX: Ensure the FlipFlop output signal is registered
                            // as a state element so it can be found during simulation.
                            // Sequential outputs from elaborated instances need to be in state_elements.
                            let signal_exists_in_signals =
                                self.sir.signals.iter().any(|s| s.name == internal_target);
                            let signal_exists_in_state =
                                self.sir.state_elements.contains_key(&internal_target);

                            if !signal_exists_in_signals && !signal_exists_in_state {
                                // Get signal width from the target
                                let width = self.get_signal_width(&actual_target);
                                let sir_type = SirType::Bits(width);

                                // Add to state_elements (FlipFlop outputs are state)
                                self.sir.state_elements.insert(
                                    internal_target.clone(),
                                    StateElement {
                                        name: internal_target.clone(),
                                        width,
                                        sir_type: Some(sir_type.clone()),
                                        reset_value: None,
                                        clock: clock_signal.clone(),
                                        reset: None,
                                        span: None,
                                    },
                                );
                            } else if signal_exists_in_signals {
                                // Mark the signal as having this driver (use internal name)
                                if let Some(sig) = self
                                    .sir
                                    .signals
                                    .iter_mut()
                                    .find(|s| s.name == internal_target)
                                {
                                    sig.driver_node = Some(node_id);
                                }
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
    /// BUG #117r FIX: Works with hierarchical paths, using mir_to_internal_name mapping
    fn expand_flattened_target(&self, target: &str) -> Vec<String> {
        // CRITICAL: First check if the target has a registered internal name
        // If it does, return just that target without expansion
        // This prevents incorrectly expanding "wr_ptr" to match "wr_ptr_gray", "wr_ptr_gray_sync1", etc.
        if self.mir_to_internal_name.contains_key(target) {
            return vec![target.to_string()];
        }

        // If target isn't directly registered, it might be a flattened array base (e.g., "mem")
        // Check if there are flattened array elements: <target>_<digit> or <target>_<digit>_<field>
        let prefix = format!("{}_", target);
        let mut expanded: Vec<String> = self
            .mir_to_internal_name
            .keys()
            .filter(|path| {
                if !path.starts_with(&prefix) {
                    return false;
                }

                // Verify it's an array element pattern: _<digit>_<field> or _<digit>
                let suffix = &path[prefix.len()..];

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
            .cloned()
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
            Statement::Case(case_stmt) => {
                // BUG #237 FIX: Collect targets from Case/match statement arms
                // This is critical for state machines that assign in match arms
                for item in &case_stmt.items {
                    for stmt in &item.block.statements {
                        self.collect_assignment_targets_for_instance(
                            stmt,
                            inst_prefix,
                            port_mapping,
                            child_module,
                            targets,
                        );
                    }
                }
                // Also check default block
                if let Some(default_block) = &case_stmt.default {
                    for stmt in &default_block.statements {
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
            Statement::ResolvedConditional(resolved) => {
                // BUG #269 FIX: ResolvedConditional is an optimized form of if-else-if
                // chains created by the HIR→MIR optimizer. It has a single target and
                // a priority mux tree. Without handling this, DFF registers are silently
                // dropped for sub-entity instances that use simple conditional assignments
                // (e.g., `if rst { fault_reg = 0 } else { fault_reg = value > threshold }`).
                let lhs_signal = match &resolved.target {
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
            _ => {}
        }
    }

    /// Synthesize sequential logic for a target signal in instance
    /// This builds MUX trees for conditional assignments
    #[allow(clippy::too_many_arguments)]
    fn synthesize_sequential_logic_for_instance(
        &mut self,
        statements: &[Statement],
        inst_prefix: &str,
        port_mapping: &HashMap<String, Expression>,
        child_module: &Module,
        target: &str,
        parent_module_for_signals: Option<&Module>,
        parent_prefix: &str,
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
                                        // Create condition: index_expr == element_idx
                                        let index_node = self
                                            .create_expression_node_for_instance_with_context(
                                                index,
                                                inst_prefix,
                                                port_mapping,
                                                child_module,
                                                parent_module_for_signals,
                                                parent_prefix,
                                            );
                                        let const_idx =
                                            self.create_constant_node(element_idx as u64, 32);
                                        let cond_node = self.create_binary_op_node(
                                            &BinaryOp::Equal,
                                            index_node,
                                            const_idx,
                                        );

                                        // Create value node (RHS)
                                        let value_node = self
                                            .create_expression_node_for_instance_with_context(
                                                &assign.rhs,
                                                inst_prefix,
                                                port_mapping,
                                                child_module,
                                                parent_module_for_signals,
                                                parent_prefix,
                                            );

                                        // Create current value node (keep current value if condition false)
                                        // BUG #117r FIX: Use instance-aware SignalRef for output port resolution
                                        let current_node = self.create_signal_ref_for_instance(
                                            target,
                                            inst_prefix,
                                            port_mapping,
                                            parent_module_for_signals,
                                            parent_prefix,
                                        );

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
                        return self.create_expression_node_for_instance_with_context(
                            &assign.rhs,
                            inst_prefix,
                            port_mapping,
                            child_module,
                            parent_module_for_signals,
                            parent_prefix,
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
                        parent_module_for_signals,
                        parent_prefix,
                    );
                }
                Statement::Case(case_stmt) => {
                    // BUG #237 FIX: Handle top-level Case statements too
                    if let Some(val) = self.synthesize_case_for_target_for_instance(
                        case_stmt,
                        inst_prefix,
                        port_mapping,
                        child_module,
                        target,
                        parent_module_for_signals,
                        parent_prefix,
                    ) {
                        return val;
                    }
                }
                Statement::ResolvedConditional(resolved) => {
                    // BUG #269 FIX: Handle ResolvedConditional (optimized if-else-if chains)
                    // Check if this targets our signal, then use the original IfStatement
                    // for synthesis (it has the same semantics as the resolved form).
                    let lhs = match &resolved.target {
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
                    if let Some(lhs_name) = lhs {
                        if lhs_name == target {
                            return self.synthesize_conditional_for_instance(
                                &resolved.original,
                                inst_prefix,
                                port_mapping,
                                child_module,
                                target,
                                parent_module_for_signals,
                                parent_prefix,
                            );
                        }
                    }
                }
                _ => {}
            }
        }

        // No assignment found - use signal ref (keep current value)
        // BUG #117r FIX: Use instance-aware SignalRef for output port resolution
        self.create_signal_ref_for_instance(
            target,
            inst_prefix,
            port_mapping,
            parent_module_for_signals,
            parent_prefix,
        )
    }

    /// Synthesize conditional assignment for instance (creates MUX nodes)
    #[allow(clippy::too_many_arguments)]
    fn synthesize_conditional_for_instance(
        &mut self,
        if_stmt: &IfStatement,
        inst_prefix: &str,
        port_mapping: &HashMap<String, Expression>,
        child_module: &Module,
        target: &str,
        parent_module_for_signals: Option<&Module>,
        parent_prefix: &str,
    ) -> usize {
        // Get values from then and else branches
        let then_value = self.find_assignment_in_branch_for_instance(
            &if_stmt.then_block.statements,
            inst_prefix,
            port_mapping,
            child_module,
            target,
            parent_module_for_signals,
            parent_prefix,
        );

        let else_value = if let Some(else_block) = &if_stmt.else_block {
            self.find_assignment_in_branch_for_instance(
                &else_block.statements,
                inst_prefix,
                port_mapping,
                child_module,
                target,
                parent_module_for_signals,
                parent_prefix,
            )
        } else {
            None
        };

        // Build MUX based on what was found
        match (then_value, else_value) {
            (Some(then_val), Some(else_val)) => {
                // Both branches assign: mux(cond, then, else)
                let condition = self.create_expression_node_for_instance_with_context(
                    &if_stmt.condition,
                    inst_prefix,
                    port_mapping,
                    child_module,
                    parent_module_for_signals,
                    parent_prefix,
                );
                self.create_mux_node(condition, then_val, else_val)
            }
            (Some(then_val), None) => {
                // Only then assigns: mux(cond, then, keep)
                let condition = self.create_expression_node_for_instance_with_context(
                    &if_stmt.condition,
                    inst_prefix,
                    port_mapping,
                    child_module,
                    parent_module_for_signals,
                    parent_prefix,
                );
                // BUG #117r FIX: Use instance-aware SignalRef for output port resolution
                let keep_val = self.create_signal_ref_for_instance(
                    target,
                    inst_prefix,
                    port_mapping,
                    parent_module_for_signals,
                    parent_prefix,
                );
                self.create_mux_node(condition, then_val, keep_val)
            }
            (None, Some(else_val)) => {
                // Only else assigns: mux(cond, keep, else)
                let condition = self.create_expression_node_for_instance_with_context(
                    &if_stmt.condition,
                    inst_prefix,
                    port_mapping,
                    child_module,
                    parent_module_for_signals,
                    parent_prefix,
                );
                // BUG #117r FIX: Use instance-aware SignalRef for output port resolution
                let keep_val = self.create_signal_ref_for_instance(
                    target,
                    inst_prefix,
                    port_mapping,
                    parent_module_for_signals,
                    parent_prefix,
                );
                self.create_mux_node(condition, keep_val, else_val)
            }
            (None, None) => {
                // Neither assigns: keep current value
                // BUG #117r FIX: Use instance-aware SignalRef for output port resolution
                self.create_signal_ref_for_instance(
                    target,
                    inst_prefix,
                    port_mapping,
                    parent_module_for_signals,
                    parent_prefix,
                )
            }
        }
    }

    /// BUG #238 FIX: Synthesize conditional with explicit default value
    /// This allows chaining multiple If statements where later ones wrap earlier results
    #[allow(clippy::too_many_arguments)]
    fn synthesize_conditional_for_instance_with_default(
        &mut self,
        if_stmt: &IfStatement,
        inst_prefix: &str,
        port_mapping: &HashMap<String, Expression>,
        child_module: &Module,
        target: &str,
        parent_module_for_signals: Option<&Module>,
        parent_prefix: &str,
        default_result: Option<usize>,
    ) -> usize {
        // Get values from then and else branches, passing current result as default
        let then_value = self.find_assignment_in_branch_for_instance_with_default(
            &if_stmt.then_block.statements,
            inst_prefix,
            port_mapping,
            child_module,
            target,
            parent_module_for_signals,
            parent_prefix,
            None, // Then branch doesn't inherit default
        );

        let else_value = if let Some(else_block) = &if_stmt.else_block {
            self.find_assignment_in_branch_for_instance_with_default(
                &else_block.statements,
                inst_prefix,
                port_mapping,
                child_module,
                target,
                parent_module_for_signals,
                parent_prefix,
                default_result, // BUG #238 FIX: Pass default_result to else branch for proper chaining
            )
        } else {
            None
        };

        // Get the default node to use when this if doesn't assign
        // BUG #117r FIX: Use instance-aware SignalRef for output port resolution
        let default_node = default_result.unwrap_or_else(|| {
            self.create_signal_ref_for_instance(
                target,
                inst_prefix,
                port_mapping,
                parent_module_for_signals,
                parent_prefix,
            )
        });

        // Build MUX based on what was found
        match (then_value, else_value) {
            (Some(then_val), Some(else_val)) => {
                // Both branches assign: mux(cond, then, else)
                let condition = self.create_expression_node_for_instance_with_context(
                    &if_stmt.condition,
                    inst_prefix,
                    port_mapping,
                    child_module,
                    parent_module_for_signals,
                    parent_prefix,
                );
                self.create_mux_node(condition, then_val, else_val)
            }
            (Some(then_val), None) => {
                // Only then assigns: mux(cond, then, default)
                let condition = self.create_expression_node_for_instance_with_context(
                    &if_stmt.condition,
                    inst_prefix,
                    port_mapping,
                    child_module,
                    parent_module_for_signals,
                    parent_prefix,
                );
                self.create_mux_node(condition, then_val, default_node)
            }
            (None, Some(else_val)) => {
                // Only else assigns: mux(cond, default, else)
                let condition = self.create_expression_node_for_instance_with_context(
                    &if_stmt.condition,
                    inst_prefix,
                    port_mapping,
                    child_module,
                    parent_module_for_signals,
                    parent_prefix,
                );
                self.create_mux_node(condition, default_node, else_val)
            }
            (None, None) => {
                // Neither assigns: return the default (previous result in chain)
                default_node
            }
        }
    }

    /// Find assignment to target in a branch
    /// BUG #238 FIX: Process ALL statements and chain them together.
    /// In RTL, later assignments in a clocked block take priority over earlier ones.
    /// So `if A { x = 1 } if B { x = 0 }` should give: mux(B, 0, mux(A, 1, current_x))
    #[allow(clippy::too_many_arguments)]
    fn find_assignment_in_branch_for_instance(
        &mut self,
        statements: &[Statement],
        inst_prefix: &str,
        port_mapping: &HashMap<String, Expression>,
        child_module: &Module,
        target: &str,
        parent_module_for_signals: Option<&Module>,
        parent_prefix: &str,
    ) -> Option<usize> {
        // BUG #238 FIX: Collect all statement results, then chain them together
        // Later statements have higher priority (override earlier ones)
        let mut result: Option<usize> = None;

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
                            // Found assignment for this target - this overrides any previous result
                            result = Some(self.create_expression_node_for_instance_with_context(
                                &assign.rhs,
                                inst_prefix,
                                port_mapping,
                                child_module,
                                parent_module_for_signals,
                                parent_prefix,
                            ));
                        }
                    }
                }
                Statement::If(nested_if) => {
                    // BUG #238 FIX: Don't return immediately! Chain conditionals together.
                    // Each If statement should wrap the previous result as the else case.
                    let if_result = self.synthesize_conditional_for_instance_with_default(
                        nested_if,
                        inst_prefix,
                        port_mapping,
                        child_module,
                        target,
                        parent_module_for_signals,
                        parent_prefix,
                        result, // Pass current result as default (else case)
                    );
                    result = Some(if_result);
                }
                Statement::Block(block) => {
                    // Recursively search within the block, passing current result as base
                    if let Some(block_result) = self
                        .find_assignment_in_branch_for_instance_with_default(
                            &block.statements,
                            inst_prefix,
                            port_mapping,
                            child_module,
                            target,
                            parent_module_for_signals,
                            parent_prefix,
                            result,
                        )
                    {
                        result = Some(block_result);
                    }
                }
                Statement::Case(case_stmt) => {
                    // BUG #237 FIX: Handle Case/match statements in instance branches
                    // This is critical for state machines that use match statements
                    // BUG #244 FIX: Pass current result so unconditional assignments before
                    // the case statement are used as the default when no case arm matches
                    if let Some(val) = self.synthesize_case_for_target_for_instance_with_default(
                        case_stmt,
                        inst_prefix,
                        port_mapping,
                        child_module,
                        target,
                        parent_module_for_signals,
                        parent_prefix,
                        result, // BUG #244: Pass current result as default
                    ) {
                        result = Some(val);
                    }
                }
                _ => {}
            }
        }
        result
    }

    /// Helper for find_assignment_in_branch_for_instance that carries a default value
    #[allow(clippy::too_many_arguments)]
    fn find_assignment_in_branch_for_instance_with_default(
        &mut self,
        statements: &[Statement],
        inst_prefix: &str,
        port_mapping: &HashMap<String, Expression>,
        child_module: &Module,
        target: &str,
        parent_module_for_signals: Option<&Module>,
        parent_prefix: &str,
        default_result: Option<usize>,
    ) -> Option<usize> {
        let mut result = default_result;

        for stmt in statements {
            match stmt {
                Statement::Assignment(assign) => {
                    let lhs_signal = match &assign.lhs {
                        LValue::Signal(sig_id) => child_module
                            .signals
                            .iter()
                            .find(|s| s.id == *sig_id)
                            .map(|signal| format!("{}.{}", inst_prefix, signal.name)),
                        LValue::BitSelect { base, .. } => {
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
                        let matches = if matches!(assign.lhs, LValue::Signal(_)) {
                            lhs == target
                        } else {
                            let target_stripped = self.strip_flattened_index_suffix(target);
                            lhs == target_stripped
                        };

                        if matches {
                            result = Some(self.create_expression_node_for_instance_with_context(
                                &assign.rhs,
                                inst_prefix,
                                port_mapping,
                                child_module,
                                parent_module_for_signals,
                                parent_prefix,
                            ));
                        }
                    }
                }
                Statement::If(nested_if) => {
                    let if_result = self.synthesize_conditional_for_instance_with_default(
                        nested_if,
                        inst_prefix,
                        port_mapping,
                        child_module,
                        target,
                        parent_module_for_signals,
                        parent_prefix,
                        result,
                    );
                    result = Some(if_result);
                }
                Statement::Block(block) => {
                    if let Some(block_result) = self
                        .find_assignment_in_branch_for_instance_with_default(
                            &block.statements,
                            inst_prefix,
                            port_mapping,
                            child_module,
                            target,
                            parent_module_for_signals,
                            parent_prefix,
                            result,
                        )
                    {
                        result = Some(block_result);
                    }
                }
                Statement::Case(case_stmt) => {
                    // BUG #244 FIX: Pass current result so unconditional assignments before
                    // the case statement are used as the default when no case arm matches
                    if let Some(val) = self.synthesize_case_for_target_for_instance_with_default(
                        case_stmt,
                        inst_prefix,
                        port_mapping,
                        child_module,
                        target,
                        parent_module_for_signals,
                        parent_prefix,
                        result, // BUG #244: Pass current result as default
                    ) {
                        result = Some(val);
                    }
                }
                _ => {}
            }
        }
        result
    }

    /// BUG #237 FIX: Synthesize case statement for a specific target in instance context
    /// This is the instance-aware version of synthesize_case_for_target_with_default
    #[allow(clippy::too_many_arguments)]
    fn synthesize_case_for_target_for_instance(
        &mut self,
        case_stmt: &skalp_mir::CaseStatement,
        inst_prefix: &str,
        port_mapping: &HashMap<String, Expression>,
        child_module: &Module,
        target: &str,
        parent_module_for_signals: Option<&Module>,
        parent_prefix: &str,
    ) -> Option<usize> {
        // Create expression node for the case expression (e.g., state_reg)
        let case_expr_node = self.create_expression_node_for_instance_with_context(
            &case_stmt.expr,
            inst_prefix,
            port_mapping,
            child_module,
            parent_module_for_signals,
            parent_prefix,
        );

        // Collect values and expressions for this target from all case arms
        let mut case_arms: Vec<(Vec<&Expression>, Option<usize>)> = Vec::new();
        let mut found_target = false;

        for item in case_stmt.items.iter() {
            // Recursively find assignment in this arm's block
            let arm_value = self.find_assignment_in_branch_for_instance(
                &item.block.statements,
                inst_prefix,
                port_mapping,
                child_module,
                target,
                parent_module_for_signals,
                parent_prefix,
            );
            if arm_value.is_some() {
                found_target = true;
            }
            case_arms.push((item.values.iter().collect(), arm_value));
        }

        // Get default block value
        let default_block_value = if let Some(default_block) = &case_stmt.default {
            let val = self.find_assignment_in_branch_for_instance(
                &default_block.statements,
                inst_prefix,
                port_mapping,
                child_module,
                target,
                parent_module_for_signals,
                parent_prefix,
            );
            if val.is_some() {
                found_target = true;
            }
            val
        } else {
            None
        };

        // If no arm assigns to this target, return None
        if !found_target {
            return None;
        }

        // Use default block value, or signal ref (keep current value)
        // BUG #117r FIX: Use instance-aware SignalRef for output port resolution
        let mut current_mux = match default_block_value {
            Some(block_val) => block_val, // Default block has assignment
            None => self.create_signal_ref_for_instance(
                target,
                inst_prefix,
                port_mapping,
                parent_module_for_signals,
                parent_prefix,
            ), // Keep current value
        };

        // Build mux chain from last case to first (so first has priority)
        for (values, arm_value) in case_arms.iter().rev() {
            let value_node = match arm_value {
                Some(val) => *val,
                None => current_mux,
            };

            if values.is_empty() {
                continue;
            }

            let condition_node = if values.len() == 1 {
                let val_node = self.create_expression_node_for_instance_with_context(
                    values[0],
                    inst_prefix,
                    port_mapping,
                    child_module,
                    parent_module_for_signals,
                    parent_prefix,
                );
                self.create_binary_op_node(&skalp_mir::BinaryOp::Equal, case_expr_node, val_node)
            } else {
                let mut or_node = {
                    let val_node = self.create_expression_node_for_instance_with_context(
                        values[0],
                        inst_prefix,
                        port_mapping,
                        child_module,
                        parent_module_for_signals,
                        parent_prefix,
                    );
                    self.create_binary_op_node(
                        &skalp_mir::BinaryOp::Equal,
                        case_expr_node,
                        val_node,
                    )
                };
                for value in &values[1..] {
                    let val_node = self.create_expression_node_for_instance_with_context(
                        value,
                        inst_prefix,
                        port_mapping,
                        child_module,
                        parent_module_for_signals,
                        parent_prefix,
                    );
                    let eq_node = self.create_binary_op_node(
                        &skalp_mir::BinaryOp::Equal,
                        case_expr_node,
                        val_node,
                    );
                    or_node =
                        self.create_binary_op_node(&skalp_mir::BinaryOp::Or, or_node, eq_node);
                }
                or_node
            };

            let old_mux = current_mux;
            current_mux = self.create_mux_node(condition_node, value_node, old_mux);
        }

        Some(current_mux)
    }

    /// BUG #244 FIX: Synthesize case statement with an explicit default value
    /// When an unconditional assignment precedes the case statement, pass it as default
    /// so that if no case arm matches, we use the unconditional value instead of "keep current"
    #[allow(clippy::too_many_arguments)]
    fn synthesize_case_for_target_for_instance_with_default(
        &mut self,
        case_stmt: &skalp_mir::CaseStatement,
        inst_prefix: &str,
        port_mapping: &HashMap<String, Expression>,
        child_module: &Module,
        target: &str,
        parent_module_for_signals: Option<&Module>,
        parent_prefix: &str,
        default_result: Option<usize>,
    ) -> Option<usize> {
        // Create expression node for the case expression (e.g., state_reg)
        let case_expr_node = self.create_expression_node_for_instance_with_context(
            &case_stmt.expr,
            inst_prefix,
            port_mapping,
            child_module,
            parent_module_for_signals,
            parent_prefix,
        );

        // Collect values and expressions for this target from all case arms
        let mut case_arms: Vec<(Vec<&Expression>, Option<usize>)> = Vec::new();
        let mut found_target = false;

        for item in case_stmt.items.iter() {
            // Recursively find assignment in this arm's block, passing default_result for chaining
            let arm_value = self.find_assignment_in_branch_for_instance_with_default(
                &item.block.statements,
                inst_prefix,
                port_mapping,
                child_module,
                target,
                parent_module_for_signals,
                parent_prefix,
                None, // Case arms don't inherit the default
            );
            if arm_value.is_some() {
                found_target = true;
            }
            case_arms.push((item.values.iter().collect(), arm_value));
        }

        // Get default block value
        let default_block_value = if let Some(default_block) = &case_stmt.default {
            let val = self.find_assignment_in_branch_for_instance_with_default(
                &default_block.statements,
                inst_prefix,
                port_mapping,
                child_module,
                target,
                parent_module_for_signals,
                parent_prefix,
                default_result, // BUG #244 FIX: pass default_result for proper chaining
            );
            if val.is_some() {
                found_target = true;
            }
            val
        } else {
            None
        };

        // BUG #244 FIX: If we have a default_result (from unconditional assignment before case),
        // treat the case statement as assigning to this target even if no arm explicitly assigns
        if !found_target && default_result.is_none() {
            return None;
        }

        // BUG #244 FIX: Use default_result (unconditional assignment value) when:
        // 1. No default block in the case statement, AND
        // 2. We have a default_result from a preceding unconditional assignment
        // Otherwise fall back to keeping current value
        // BUG #117r FIX: Use instance-aware SignalRef for output port resolution
        let mut current_mux = match default_block_value {
            Some(block_val) => block_val, // Default block has assignment
            None => {
                // BUG #244 FIX: Use unconditional assignment if provided
                match default_result {
                    Some(default_val) => default_val,
                    None => self.create_signal_ref_for_instance(
                        target,
                        inst_prefix,
                        port_mapping,
                        parent_module_for_signals,
                        parent_prefix,
                    ), // Keep current value
                }
            }
        };

        // Build mux chain from last case to first (so first has priority)
        for (values, arm_value) in case_arms.iter().rev() {
            let value_node = match arm_value {
                Some(val) => *val,
                None => current_mux,
            };

            if values.is_empty() {
                continue;
            }

            let condition_node = if values.len() == 1 {
                let val_node = self.create_expression_node_for_instance_with_context(
                    values[0],
                    inst_prefix,
                    port_mapping,
                    child_module,
                    parent_module_for_signals,
                    parent_prefix,
                );
                self.create_binary_op_node(&skalp_mir::BinaryOp::Equal, case_expr_node, val_node)
            } else {
                let mut or_node = {
                    let val_node = self.create_expression_node_for_instance_with_context(
                        values[0],
                        inst_prefix,
                        port_mapping,
                        child_module,
                        parent_module_for_signals,
                        parent_prefix,
                    );
                    self.create_binary_op_node(
                        &skalp_mir::BinaryOp::Equal,
                        case_expr_node,
                        val_node,
                    )
                };
                for value in &values[1..] {
                    let val_node = self.create_expression_node_for_instance_with_context(
                        value,
                        inst_prefix,
                        port_mapping,
                        child_module,
                        parent_module_for_signals,
                        parent_prefix,
                    );
                    let eq_node = self.create_binary_op_node(
                        &skalp_mir::BinaryOp::Equal,
                        case_expr_node,
                        val_node,
                    );
                    or_node =
                        self.create_binary_op_node(&skalp_mir::BinaryOp::Or, or_node, eq_node);
                }
                or_node
            };

            current_mux = self.create_mux_node(condition_node, value_node, current_mux);
        }

        Some(current_mux)
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
    #[allow(clippy::too_many_arguments)]
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
    #[allow(dead_code)]
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
        match &expr.kind {
            ExpressionKind::Literal(value) => self.create_literal_node(value),
            ExpressionKind::Ref(lvalue) => {
                // BUG FIX #85: Handle RangeSelect and BitSelect specially
                // For these, we need to create a node that selects bits from the base signal
                match lvalue {
                    LValue::RangeSelect { base, high, low } => {
                        // First, get the base signal node
                        let base_expr =
                            Expression::with_unknown_type(ExpressionKind::Ref((**base).clone()));
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
                        self.create_range_select_node_on_base(base_node, high_val, low_val)
                    }
                    LValue::BitSelect { base, index } => {
                        // First, get the base signal node
                        let base_expr =
                            Expression::with_unknown_type(ExpressionKind::Ref((**base).clone()));
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
                        self.create_bit_select_node_on_base(base_node, index_val)
                    }
                    // BUG FIX #113: Handle Port specially to preserve RangeSelect in port_mapping
                    // When a port maps to a RangeSelect expression (e.g., param_0 -> data1[31:0]),
                    // we need to recursively create a node for that expression, not just extract
                    // the base signal name.
                    LValue::Port(port_id) => {
                        // Look up the port name in child_module
                        let port_name = if let Some(port) =
                            child_module.ports.iter().find(|p| p.id == *port_id)
                        {
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
                            // BUG #236 FIX PART 2: For child input ports, use the SIR signal we created
                            // instead of following port_mapping to the parent's signal.
                            // The external name format is "{inst_prefix}.{port_name}"
                            //
                            // BUG #255 FIX: We must create a NEW SignalRef that reads from the child input
                            // port signal, rather than returning the driver node. The driver might have
                            // a different type (e.g., signed) than the child port's declared type (unsigned).
                            // By creating a new SignalRef with the child signal's type, we ensure comparisons
                            // use the correct signedness.
                            if !inst_prefix.is_empty() {
                                let child_port_external_name =
                                    format!("{}.{}", inst_prefix.trim_end_matches('.'), name);
                                if let Some(internal_name) = self
                                    .mir_to_internal_name
                                    .get(&child_port_external_name)
                                    .cloned()
                                {
                                    // BUG #255 FIX: Create a fresh SignalRef to read from the child port signal
                                    // with the signal's declared type, NOT the driver's type.
                                    return self
                                        .create_signal_reader_for_child_port(&internal_name);
                                }
                            }

                            if let Some(parent_expr) = port_mapping.get(name) {
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
                                let parent_port_mapping = self
                                    .instance_port_mappings
                                    .get(parent_key)
                                    .cloned()
                                    .unwrap_or_default();
                                if let Some(parent_module) = parent_module_for_signals {
                                    // BUG FIX #209: Look up the correct grandparent module for 3+ level hierarchies
                                    // instead of always using self.mir. The grandparent is the module that owns
                                    // the SignalIds in parent_port_mapping.
                                    let (grandparent_module, grandparent_prefix) =
                                        if let Some(grandparent_id) =
                                            self.instance_parent_module_ids.get(parent_key)
                                        {
                                            // Find the grandparent module by ID
                                            if let Some(gp_module) = self
                                                .mir_design
                                                .modules
                                                .iter()
                                                .find(|m| m.id == *grandparent_id)
                                            {
                                                // Calculate grandparent prefix by stripping the last component from parent_key
                                                // e.g., "m.ppgen" -> "m.", "m" -> ""
                                                let gp_prefix =
                                                    if let Some(pos) = parent_key.rfind('.') {
                                                        format!("{}.", &parent_key[..pos])
                                                    } else {
                                                        String::new()
                                                    };
                                                (gp_module.clone(), gp_prefix)
                                            } else {
                                                // Fallback to self.mir if module not found
                                                (self.mir.clone(), String::new())
                                            }
                                        } else {
                                            // No stored parent - we're at first level nesting, use self.mir
                                            (self.mir.clone(), String::new())
                                        };

                                    return self.create_expression_node_for_instance_with_context(
                                        parent_expr,
                                        parent_prefix, // Use parent prefix for parent context
                                        &parent_port_mapping, // BUG #124: Use parent's port_mapping for proper resolution
                                        parent_module, // BUG #113: Use parent module since Port refs are from parent
                                        Some(&grandparent_module), // BUG #209: Use correct grandparent, not always self.mir
                                        &grandparent_prefix, // BUG #209: Use correct grandparent prefix
                                    );
                                } else {
                                    // No parent module - we're at top level, look up in self.mir
                                    // BUG #124: At top level, there's no parent prefix so no stored mapping needed
                                    let top_level_mapping = HashMap::new();
                                    return self.create_expression_node_for_instance_with_context(
                                        parent_expr,
                                        "", // No instance prefix at top level
                                        &top_level_mapping,
                                        &self.mir.clone(), // Use top-level module
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
                                    if let Some(mir_port) =
                                        self.mir.ports.iter().find(|p| p.name == *name)
                                    {
                                        // Found in top-level - use the signal directly (no prefix)
                                        return self.get_or_create_signal_driver(&mir_port.name);
                                    }
                                    // Try to find as a signal in self.mir by name
                                    if self.mir.signals.iter().any(|s| s.name == *name) {
                                        return self.get_or_create_signal_driver(name);
                                    }
                                    // At top-level, just try the name directly
                                    return self.get_or_create_signal_driver(name);
                                }
                                // For nested instances, continue to the parent_module lookup below
                            }

                            // BUG #113 FIX: After recursion, inst_prefix is from the PARENT context.
                            // The Port reference is from child_module, but we need to look it up using
                            // parent context. Try parent_module_for_signals FIRST.
                            if let Some(parent_module) = parent_module_for_signals {
                                if let Some(port) =
                                    parent_module.ports.iter().find(|p| p.id == *port_id)
                                {
                                    let sig_name = format!("{}{}", parent_prefix, port.name);
                                    return self.get_or_create_signal_driver(&sig_name);
                                }
                                // BUG #114 FIX: Also try by NAME in parent_module
                                if let Some(ref name) = port_name {
                                    if let Some(port) =
                                        parent_module.ports.iter().find(|p| p.name == *name)
                                    {
                                        let sig_name = if parent_prefix.is_empty() {
                                            port.name.clone()
                                        } else {
                                            format!("{}{}", parent_prefix, port.name)
                                        };
                                        return self.get_or_create_signal_driver(&sig_name);
                                    }
                                }
                                // Try by index in parent_module
                                let port_index = port_id.0 as usize;
                                if port_index < parent_module.ports.len() {
                                    let port = &parent_module.ports[port_index];
                                    let sig_name = format!("{}{}", parent_prefix, port.name);
                                    return self.get_or_create_signal_driver(&sig_name);
                                }
                            }

                            // Try self.mir (top-level module) - ports here don't need prefix
                            if let Some(port) = self.mir.ports.iter().find(|p| p.id == *port_id) {
                                return self.get_or_create_signal_driver(&port.name);
                            }
                            let port_index = port_id.0 as usize;
                            if port_index < self.mir.ports.len() {
                                let port = &self.mir.ports[port_index];
                                return self.get_or_create_signal_driver(&port.name);
                            }

                            // BUG #113 FIX: DO NOT try child_module with inst_prefix here!
                            // After recursion, inst_prefix is from the PARENT context, not current
                            // child_module. Using child_module.ports with inst_prefix creates
                            // wrong signal names like "exec_l4_l5_inst_233.param_1".
                            // Let it fall through to the fallback lookup below instead.
                        }

                        // Fallback to original behavior
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
                            // BUG #237 FIX: Before giving up, try to find the port in parent modules
                            // This handles the case where a port ID from a parent module is referenced
                            // in a child context (common with deeply nested state machines)
                            if let Some(parent_module) = parent_module_for_signals {
                                if let Some(parent_port) =
                                    parent_module.ports.iter().find(|p| p.id == *port_id)
                                {
                                    let sig_name = if parent_prefix.is_empty() {
                                        parent_port.name.clone()
                                    } else {
                                        format!("{}{}", parent_prefix, parent_port.name)
                                    };
                                    return self.get_or_create_signal_driver(&sig_name);
                                }
                            }
                            // Also try self.mir (top-level)
                            if let Some(mir_port) = self.mir.ports.iter().find(|p| p.id == *port_id)
                            {
                                return self.get_or_create_signal_driver(&mir_port.name);
                            }
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
                // BUG FIX: Check signedness for comparison operations in instance context
                // This ensures signed comparisons (e.g., q16_16 > out_max) use signed operations
                let left_is_signed = self.is_expression_signed(left);
                let right_is_signed = self.is_expression_signed(right);
                let is_signed = left_is_signed || right_is_signed;
                self.create_binary_op_node_with_signedness(op, left_node, right_node, is_signed)
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
                // Check if the base is a Signal reference (module instance result)
                if let ExpressionKind::Ref(LValue::Signal(sig_id)) = &base.kind {
                    // Look up the signal name in child module
                    if let Some(signal) = child_module.signals.iter().find(|s| s.id == *sig_id) {
                        // BUG FIX #85: Check for _tuple_tmp_ pattern - this is a tuple temporary
                        // The _tuple_tmp_XX signals are intermediates that need to find the actual result signals
                        // For example, _tuple_tmp_76_76 -> look for assignments FROM quadratic_solve_inst_145_result_N
                        if signal.name.contains("_tuple_tmp_") {
                            // Find the assignment to this signal to determine the source module instance
                            // The assignment should be: _tuple_tmp_76_76 = quadratic_solve_inst_145_result_0
                            // From that we can derive quadratic_solve_inst_145_result_N for index N
                            for assign in &child_module.assignments {
                                if let LValue::Signal(assign_sig_id) = &assign.lhs {
                                    if assign_sig_id == sig_id {
                                        // Found the assignment to this _tuple_tmp_ signal
                                        // Check if RHS is a reference to another signal
                                        if let ExpressionKind::Ref(LValue::Signal(src_sig_id)) =
                                            &assign.rhs.kind
                                        {
                                            if let Some(src_signal) = child_module
                                                .signals
                                                .iter()
                                                .find(|s| s.id == *src_sig_id)
                                            {
                                                if src_signal.name.contains("_inst_")
                                                    && src_signal.name.contains("_result_")
                                                {
                                                    // Found the source module instance result signal
                                                    // Replace _result_0 (or _result_N) with _result_{index}
                                                    let base_name = if let Some(pos) =
                                                        src_signal.name.rfind("_result_")
                                                    {
                                                        &src_signal.name[..pos]
                                                    } else {
                                                        &src_signal.name[..]
                                                    };
                                                    let target_signal_name = format!(
                                                        "{}.{}_result_{}",
                                                        inst_prefix, base_name, index
                                                    );
                                                    return self.get_or_create_signal_driver(
                                                        &target_signal_name,
                                                    );
                                                }
                                            }
                                        // BUG FIX #85: Handle Concat RHS - tuple is built from concatenated results
                                        } else if let ExpressionKind::Concat(concat_parts) =
                                            &assign.rhs.kind
                                        {
                                            // The tuple is a concatenation of result signals
                                            // Index 0 -> first part, Index 1 -> second part, etc.
                                            if *index < concat_parts.len() {
                                                let target_part = &concat_parts[*index];
                                                if let ExpressionKind::Ref(LValue::Signal(
                                                    part_sig_id,
                                                )) = &target_part.kind
                                                {
                                                    if let Some(part_signal) = child_module
                                                        .signals
                                                        .iter()
                                                        .find(|s| s.id == *part_sig_id)
                                                    {
                                                        // Found the signal for this index in the concat
                                                        let target_signal_name = format!(
                                                            "{}.{}",
                                                            inst_prefix, part_signal.name
                                                        );
                                                        return self.get_or_create_signal_driver(
                                                            &target_signal_name,
                                                        );
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        } else if signal.name.ends_with("_result_0")
                            || signal.name.contains("_inst_")
                        {
                            let base_name = if let Some(pos) = signal.name.rfind("_result_") {
                                &signal.name[..pos]
                            } else {
                                &signal.name[..]
                            };
                            let target_signal_name =
                                format!("{}.{}_result_{}", inst_prefix, base_name, index);
                            return self.get_or_create_signal_driver(&target_signal_name);
                        }
                    }
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
                // BUG #220 FIX: Properly resolve FieldAccess to flattened signal names
                if let Some(node_id) = self.resolve_field_access_to_signal(base, field) {
                    node_id
                } else {
                    // Fallback to base expression if resolution fails
                    self.create_expression_node_for_instance_with_context(
                        base,
                        inst_prefix,
                        port_mapping,
                        child_module,
                        parent_module_for_signals,
                        parent_prefix,
                    )
                }
            }
            ExpressionKind::Cast {
                expr: inner_expr,
                target_type,
            } => {
                // BUG #245 FIX: Widening casts need sign/zero extension
                // BUG #186 NOTE: For same-width casts (e.g., bit[32] to fp32), this is still a no-op
                let cast_target_width = Self::datatype_to_width(target_type);

                // Create node for the inner expression first
                let inner_node = self.create_expression_node_for_instance_with_context(
                    inner_expr,
                    inst_prefix,
                    port_mapping,
                    child_module,
                    parent_module_for_signals,
                    parent_prefix,
                );

                // Get actual width from the created node, not from type annotation (which can be Unknown)
                let source_width = self.get_node_output_width(inner_node);

                if cast_target_width > source_width {
                    // Widening cast - need to extend
                    let extend_bits = cast_target_width - source_width;
                    // BUG FIX: Only sign-extend if the SOURCE is signed
                    let is_signed = self.is_expression_signed(inner_expr);

                    if is_signed {
                        // Sign extension: replicate MSB (sign bit) for the extension
                        let msb_index = source_width - 1;
                        let sign_bit_node =
                            self.create_slice_node(inner_node, msb_index, msb_index);
                        let ones_constant =
                            self.create_constant_node((1u64 << extend_bits) - 1, extend_bits);
                        let zeros_constant = self.create_constant_node(0, extend_bits);
                        let sign_extend_node =
                            self.create_mux_node(sign_bit_node, ones_constant, zeros_constant);
                        self.create_concat_node(vec![sign_extend_node, inner_node])
                    } else {
                        // Zero extension: pad with zeros
                        let zeros_node = self.create_constant_node(0, extend_bits);
                        self.create_concat_node(vec![zeros_node, inner_node])
                    }
                } else if cast_target_width < source_width {
                    // Narrowing cast - truncate by slicing
                    self.create_slice_node(inner_node, cast_target_width - 1, 0)
                } else {
                    // Same width - just return the inner expression (bitwise reinterpretation)
                    // This handles cases like bit[32] cast to fp32 where the bits stay the same
                    inner_node
                }
            }
            _ => {
                // For unsupported expressions, create a zero constant
                self.create_constant_node(0, 1)
            }
        }
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
                    // BUG #117r FIX: Translate to internal name for SignalRef
                    let internal_name = self.translate_to_internal_name(&sig_name);
                    inputs.push(SignalRef {
                        signal_id: internal_name,
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
        match lvalue {
            LValue::Signal(sig_id) => {
                // LValue::Signal should only match signals, not ports
                // (ports have their own LValue::Port variant)
                // First try child module
                if let Some(signal) = child_module.signals.iter().find(|s| s.id == *sig_id) {
                    // BUG #117r FIX PART 2: Check if this signal shares a name with an output port
                    // that is in port_mapping. This happens when an output port is READ from
                    // in an expression (not just written to). If the port is connected to a parent
                    // signal, we need to use the parent signal name, not the hierarchical path.
                    if let Some(parent_expr) = port_mapping.get(&signal.name) {
                        // Signal name matches an output port in port_mapping - resolve to parent
                        let mapped_name = self.get_signal_name_from_expression_with_context(
                            parent_expr,
                            parent_module_for_signals,
                            parent_prefix,
                        );
                        return Some(mapped_name);
                    }

                    // BUG FIX #113: Avoid double dots if inst_prefix ends with a dot
                    let result = if inst_prefix.is_empty() {
                        signal.name.clone()
                    } else if inst_prefix.ends_with('.') {
                        format!("{}{}", inst_prefix, signal.name)
                    } else {
                        format!("{}.{}", inst_prefix, signal.name)
                    };
                    return Some(result);
                }
                // BUG #186 FIX: If not found in child module, try parent module
                // This handles signals from parent scope referenced in FP specialization context
                if let Some(parent_mod) = parent_module_for_signals {
                    if let Some(signal) = parent_mod.signals.iter().find(|s| s.id == *sig_id) {
                        // Use parent_prefix for parent module signals
                        let result = if parent_prefix.is_empty() {
                            signal.name.clone()
                        } else if parent_prefix.ends_with('.') {
                            format!("{}{}", parent_prefix, signal.name)
                        } else {
                            format!("{}.{}", parent_prefix, signal.name)
                        };
                        return Some(result);
                    }
                }
                None
            }
            LValue::Port(port_id) => {
                // Direct port reference - map to parent signal through port connections
                // First try direct ID lookup
                if let Some(port) = child_module.ports.iter().find(|p| p.id == *port_id) {
                    if let Some(parent_expr) = port_mapping.get(&port.name) {
                        // Use context-aware lookup for nested instances
                        let mapped_name = self.get_signal_name_from_expression_with_context(
                            parent_expr,
                            parent_module_for_signals,
                            parent_prefix,
                        );
                        Some(mapped_name)
                    } else {
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
                    // BUG #223 FIX: Try to find port by scanning for any port that could match
                    // If the port_id is much larger than the number of ports, the old index fallback won't work.
                    // Instead, try to use the port_mapping keys to guide the lookup.
                    // This is a workaround for port ID renumbering during specialization.

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
                    } else if let Some(parent_mod) = parent_module_for_signals {
                        // BUG FIX: Port might belong to the parent module (func_quadratic_solve)
                        // when nested entity instances (FpSqrt) reference ports from the enclosing
                        // function module. Try parent module's ports.
                        if let Some(port) = parent_mod.ports.iter().find(|p| p.id == *port_id) {
                            // Resolve through parent's port mapping (stored in instance_port_mappings)
                            let parent_key = parent_prefix.trim_end_matches('.');
                            if let Some(parent_port_mapping) =
                                self.instance_port_mappings.get(parent_key)
                            {
                                if let Some(grandparent_expr) = parent_port_mapping.get(&port.name)
                                {
                                    let mapped_name =
                                        self.get_signal_name_from_expression(grandparent_expr);
                                    return Some(mapped_name);
                                }
                            }
                            // Fallback: use parent-prefixed port name
                            let result = if parent_prefix.is_empty() {
                                port.name.clone()
                            } else {
                                format!("{}.{}", parent_prefix.trim_end_matches('.'), port.name)
                            };
                            Some(result)
                        } else {
                            // Also try parent module's ports by index as last resort
                            let port_index = port_id.0 as usize;
                            if port_index < parent_mod.ports.len() {
                                let port = &parent_mod.ports[port_index];
                                let parent_key = parent_prefix.trim_end_matches('.');
                                if let Some(parent_port_mapping) =
                                    self.instance_port_mappings.get(parent_key)
                                {
                                    if let Some(grandparent_expr) =
                                        parent_port_mapping.get(&port.name)
                                    {
                                        let mapped_name =
                                            self.get_signal_name_from_expression(grandparent_expr);
                                        return Some(mapped_name);
                                    }
                                }
                                let result = if parent_prefix.is_empty() {
                                    port.name.clone()
                                } else {
                                    format!("{}.{}", parent_prefix.trim_end_matches('.'), port.name)
                                };
                                Some(result)
                            } else {
                                None
                            }
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
                        return Some(prefixed);
                    }
                }

                // Fallback to self.mir (top-level module)
                if let Some(var) = self.mir.variables.iter().find(|v| v.id == *var_id) {
                    let var_name = format!("{}_{}", var.name, var_id.0);
                    return Some(var_name);
                }

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
                                // BUG #185 FIX: Check if the parent instance has this port mapped
                                // to yet another signal (nested port mapping chain).
                                // The inst_prefix without trailing dot is the parent instance name.
                                let parent_inst_name = inst_prefix.trim_end_matches('.');
                                if let Some(parent_port_mapping) =
                                    self.instance_port_mappings.get(parent_inst_name)
                                {
                                    if let Some(grandparent_expr) =
                                        parent_port_mapping.get(&port.name)
                                    {
                                        // BUG FIX #209: The parent's port is mapped to grandparent signal
                                        // We need to recursively resolve WITH proper grandparent context
                                        // Look up the grandparent module from instance_parent_module_ids
                                        let (gp_module, gp_prefix) = if let Some(grandparent_id) =
                                            self.instance_parent_module_ids.get(parent_inst_name)
                                        {
                                            if let Some(gp_mod) = self
                                                .mir_design
                                                .modules
                                                .iter()
                                                .find(|m| m.id == *grandparent_id)
                                            {
                                                // Calculate grandparent prefix
                                                let gp_pref = if let Some(pos) =
                                                    parent_inst_name.rfind('.')
                                                {
                                                    format!("{}.", &parent_inst_name[..pos])
                                                } else {
                                                    String::new()
                                                };
                                                (Some(gp_mod), gp_pref)
                                            } else {
                                                (None, String::new())
                                            }
                                        } else {
                                            (None, String::new())
                                        };

                                        return self.get_signal_name_from_expression_with_context(
                                            grandparent_expr,
                                            gp_module,
                                            &gp_prefix,
                                        );
                                    }
                                }
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
                                return format!("{}{}_{}", inst_prefix, var.name, var_id.0);
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
        // Example: wr_data_x, wr_data_y, prot_faults_ov, faults_bms_fault
        // BUG #117r FIX: Allow multi-character field names (ov, uv, bms_fault, etc.)
        // Check if the last part starts with a letter (struct field)
        if parts.len() >= 2 {
            let potential_field = parts[parts.len() - 1];
            // Field must start with a letter (to distinguish from array indices which are all digits)
            if !potential_field.is_empty()
                && potential_field
                    .chars()
                    .next()
                    .is_some_and(|c| c.is_alphabetic())
            {
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
                    let location = instance
                        .span
                        .as_ref()
                        .map(|s| format!(" at {}", s))
                        .unwrap_or_default();
                    panic!(
                        "Module {:?} not found for instance '{}'{}",
                        instance.module, instance.name, location
                    )
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
