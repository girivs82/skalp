//! HIR to MIR transformation
//!
//! This module converts the high-level HIR representation
//! to the mid-level MIR suitable for code generation

use crate::mir::*;
use crate::type_flattening::{FlattenedField as TypeFlattenedField, TypeFlattener};
use skalp_frontend::const_eval::{ConstEvaluator, ConstValue};
use skalp_frontend::hir::{self as hir, Hir};
use std::collections::HashMap;

/// Information about a flattened port or signal field
#[derive(Debug, Clone)]
struct FlattenedField {
    /// MIR port/signal ID for this field
    id: u32,
    /// Field path (e.g., ["position", "x"] for vertex.position.x)
    field_path: Vec<String>,
    /// Leaf type of this field
    leaf_type: DataType,
}

/// HIR to MIR transformer
pub struct HirToMir<'hir> {
    /// Next module ID
    next_module_id: u32,
    /// Next port ID
    next_port_id: u32,
    /// Next signal ID
    next_signal_id: u32,
    /// Next variable ID
    next_variable_id: u32,
    /// Next process ID
    next_process_id: u32,
    /// Next clock domain ID
    next_clock_domain_id: u32,
    /// Entity to module ID mapping
    entity_map: HashMap<hir::EntityId, ModuleId>,
    /// Port ID mapping (HIR to MIR) - now 1-to-many for flattened structs
    port_map: HashMap<hir::PortId, PortId>,
    /// Flattened ports: HIR port ID -> list of flattened MIR ports
    flattened_ports: HashMap<hir::PortId, Vec<FlattenedField>>,
    /// Reverse port lookup: MIR port ID -> (HIR port ID, field path)
    /// BUG #33 FIX: This allows finding the correct HIR port for a given MIR port
    port_to_hir: HashMap<PortId, (hir::PortId, Vec<String>)>,
    /// Signal ID mapping (HIR to MIR) - now 1-to-many for flattened structs
    signal_map: HashMap<hir::SignalId, SignalId>,
    /// Flattened signals: HIR signal ID -> list of flattened MIR signals
    flattened_signals: HashMap<hir::SignalId, Vec<FlattenedField>>,
    /// Reverse signal lookup: MIR signal ID -> (HIR signal ID, field path)
    /// BUG #33 FIX: This allows finding the correct HIR signal for a given MIR signal
    signal_to_hir: HashMap<SignalId, (hir::SignalId, Vec<String>)>,
    /// Variable ID mapping (HIR to MIR)
    variable_map: HashMap<hir::VariableId, VariableId>,
    /// Clock domain ID mapping (HIR to MIR)
    clock_domain_map: HashMap<hir::ClockDomainId, ClockDomainId>,
    /// Reference to HIR for type resolution
    hir: Option<&'hir Hir>,
    /// Current entity being converted (for generic parameter resolution)
    current_entity_id: Option<hir::EntityId>,
    /// Const evaluator for evaluating const expressions (including user-defined const functions)
    const_evaluator: ConstEvaluator,
    /// Track dynamically created variables (from let bindings in event blocks)
    /// Maps HIR VariableId to (MIR VariableId, name, type)
    dynamic_variables: HashMap<hir::VariableId, (VariableId, String, hir::HirType)>,
    /// Type flattener for consistent struct/vector expansion
    type_flattener: TypeFlattener,
}

impl<'hir> HirToMir<'hir> {
    /// Create a new transformer
    pub fn new() -> Self {
        Self {
            next_module_id: 0,
            next_port_id: 0,
            next_signal_id: 0,
            next_variable_id: 0,
            next_process_id: 0,
            next_clock_domain_id: 0,
            entity_map: HashMap::new(),
            port_map: HashMap::new(),
            flattened_ports: HashMap::new(),
            port_to_hir: HashMap::new(),
            signal_map: HashMap::new(),
            flattened_signals: HashMap::new(),
            signal_to_hir: HashMap::new(),
            variable_map: HashMap::new(),
            clock_domain_map: HashMap::new(),
            hir: None,
            current_entity_id: None,
            const_evaluator: ConstEvaluator::new(),
            dynamic_variables: HashMap::new(),
            type_flattener: TypeFlattener::new(0), // Will be re-initialized per use
        }
    }

    /// Transform HIR to MIR
    pub fn transform(&mut self, hir: &'hir Hir) -> Mir {
        self.hir = Some(hir);

        // Register all user-defined const functions in the evaluator
        self.const_evaluator.register_functions(&hir.functions);

        let mut mir = Mir::new(hir.name.clone());

        // First pass: create modules for entities
        for entity in &hir.entities {
            let module_id = self.next_module_id();
            self.entity_map.insert(entity.id, module_id);

            let mut module = Module::new(module_id, entity.name.clone());

            // Convert generic parameters
            for hir_generic in &entity.generics {
                let parameter = GenericParameter {
                    name: hir_generic.name.clone(),
                    param_type: self.convert_generic_type(&hir_generic.param_type),
                    default: hir_generic
                        .default_value
                        .as_ref()
                        .and_then(|expr| self.convert_literal_expr(expr)),
                };
                module.parameters.push(parameter);
            }

            // Convert ports - flatten structs/vectors into individual ports
            for hir_port in &entity.ports {
                let port_type = self.convert_type(&hir_port.port_type);
                let direction = self.convert_port_direction(&hir_port.direction);

                let (flattened_ports, flattened_fields) = self.flatten_port(
                    &hir_port.name,
                    &port_type,
                    direction,
                    hir_port.physical_constraints.clone(),
                );

                // BUG #29 FIX: Track ALL flattened composite types, not just multi-field ones
                // For single-field structs like TestData{value}, we still need to track the flattening
                // because the type changed (Struct→Bit) and the name changed (rd_data→rd_data_value)
                // This matches the signal flattening logic (Bug #21 fix)
                if !flattened_fields.is_empty() {
                    self.flattened_ports
                        .insert(hir_port.id, flattened_fields.clone());
                }

                // BUG #33 FIX: Also populate reverse map for each flattened port
                for field in &flattened_fields {
                    self.port_to_hir
                        .insert(PortId(field.id), (hir_port.id, field.field_path.clone()));
                }

                // For simple mapping (first port or single non-struct port)
                if let Some(first_port) = flattened_ports.first() {
                    self.port_map.insert(hir_port.id, first_port.id);
                }

                // Add all flattened ports to the module
                for port in flattened_ports {
                    module.ports.push(port);
                }
            }

            mir.add_module(module);
        }

        // Second pass: add implementations
        for impl_block in &hir.implementations {
            if let Some(&module_id) = self.entity_map.get(&impl_block.entity) {
                // Set current entity for generic parameter resolution
                self.current_entity_id = Some(impl_block.entity);

                // Bind generic parameter default values into const evaluator for expression evaluation
                // Track which names we bound so we can clean them up later
                let mut bound_generic_names = Vec::new();
                if let Some(entity) = hir.entities.iter().find(|e| e.id == impl_block.entity) {
                    for generic in &entity.generics {
                        if let Some(default_expr) = &generic.default_value {
                            // Try to evaluate the default value and bind it
                            if let Ok(const_val) = self.const_evaluator.eval(default_expr) {
                                self.const_evaluator.bind(generic.name.clone(), const_val);
                                bound_generic_names.push(generic.name.clone());
                            }
                        }
                    }
                }

                // Find the module
                if let Some(module) = mir.modules.iter_mut().find(|m| m.id == module_id) {
                    // Add signals - flatten structs/vectors into individual signals
                    for hir_signal in &impl_block.signals {
                        let signal_type = self.convert_type(&hir_signal.signal_type);
                        let initial = hir_signal
                            .initial_value
                            .as_ref()
                            .and_then(|expr| self.convert_literal_expr(expr));
                        let clock_domain = hir_signal.clock_domain.map(|id| ClockDomainId(id.0));

                        let (flattened_signals, flattened_fields) = self.flatten_signal(
                            &hir_signal.name,
                            &signal_type,
                            initial,
                            clock_domain,
                        );
                        // CRITICAL FIX (Bug #21): Store flattening info for ALL composite types
                        // Even single-field structs need mapping because field name != signal name
                        // (e.g., signal "data: SimpleData" → flattened signal "data_value")
                        if !flattened_fields.is_empty() {
                            self.flattened_signals
                                .insert(hir_signal.id, flattened_fields.clone());
                        }

                        // BUG #33 FIX: Also populate reverse map for each flattened signal
                        for field in &flattened_fields {
                            self.signal_to_hir.insert(
                                SignalId(field.id),
                                (hir_signal.id, field.field_path.clone()),
                            );
                        }

                        // For simple mapping (first signal or single non-struct signal)
                        if let Some(first_signal) = flattened_signals.first() {
                            self.signal_map.insert(hir_signal.id, first_signal.id);
                        }

                        // Add all flattened signals to the module
                        for signal in flattened_signals {
                            module.signals.push(signal);
                        }
                    }
                    if module.name.contains("AsyncFifo_8") {
                        for sig in &module.signals {}
                    }

                    // Add variables
                    for hir_var in &impl_block.variables {
                        let var_id = self.next_variable_id();
                        self.variable_map.insert(hir_var.id, var_id);

                        let variable = Variable {
                            id: var_id,
                            name: hir_var.name.clone(),
                            var_type: self.convert_type(&hir_var.var_type),
                            initial: hir_var
                                .initial_value
                                .as_ref()
                                .and_then(|expr| self.convert_literal_expr(expr)),
                        };
                        module.variables.push(variable);
                    }

                    // Convert event blocks to processes
                    for event_block in &impl_block.event_blocks {
                        let process = self.convert_event_block(event_block);
                        module.processes.push(process);
                    }

                    // Add any dynamically created variables (from let bindings in event blocks)
                    // Note: Duplicates are already handled at creation time - each unique name
                    // only gets one variable ID, which is reused by all let bindings with that name
                    let dynamic_vars: Vec<_> = self.dynamic_variables.values().cloned().collect();

                    for (mir_var_id, name, hir_type) in dynamic_vars {
                        let mir_type = self.convert_type(&hir_type);
                        let variable = Variable {
                            id: mir_var_id,
                            name,
                            var_type: mir_type,
                            initial: None,
                        };
                        module.variables.push(variable);
                    }
                    // Clear dynamic variables for next impl block
                    self.dynamic_variables.clear();

                    // Convert continuous assignments (may expand to multiple for structs)
                    for hir_assign in &impl_block.assignments {
                        let assigns = self.convert_continuous_assignment_expanded(hir_assign);
                        module.assignments.extend(assigns);
                    }

                    // Convert module instances
                    for hir_instance in &impl_block.instances {
                        if let Some(instance) = self.convert_instance(hir_instance) {
                            module.instances.push(instance);
                        }
                    }

                    // Clean up generic parameter bindings for this impl block
                    for name in &bound_generic_names {
                        self.const_evaluator.unbind(name);
                    }
                }
            }
        }

        mir
    }

    /// Convert event block to process
    fn convert_event_block(&mut self, block: &hir::HirEventBlock) -> Process {
        let id = self.next_process_id();

        // Determine process kind and sensitivity
        let (kind, sensitivity) = self.analyze_event_block(block);

        // Convert body
        let body = self.convert_statements(&block.statements);

        Process {
            id,
            kind,
            sensitivity,
            body,
        }
    }

    /// Analyze event block to determine process kind and sensitivity
    fn analyze_event_block(&self, block: &hir::HirEventBlock) -> (ProcessKind, SensitivityList) {
        if block.triggers.is_empty() {
            return (ProcessKind::Combinational, SensitivityList::Always);
        }

        let mut edges = Vec::new();
        let mut is_sequential = false;

        for trigger in &block.triggers {
            // Convert the signal reference to an LValue
            let signal_lvalue = match &trigger.signal {
                hir::HirEventSignal::Port(port_id) => {
                    if let Some(&mir_port_id) = self.port_map.get(port_id) {
                        LValue::Port(mir_port_id)
                    } else {
                        continue;
                    }
                }
                hir::HirEventSignal::Signal(signal_id) => {
                    if let Some(&mir_signal_id) = self.signal_map.get(signal_id) {
                        LValue::Signal(mir_signal_id)
                    } else {
                        continue;
                    }
                }
            };

            // Check if it's a clock edge or reset
            match &trigger.edge {
                hir::HirEdgeType::Rising | hir::HirEdgeType::Falling => {
                    is_sequential = true;
                    edges.push(EdgeSensitivity {
                        signal: signal_lvalue,
                        edge: self.convert_edge_type(&trigger.edge),
                    });
                }
                hir::HirEdgeType::Both => {
                    edges.push(EdgeSensitivity {
                        signal: signal_lvalue,
                        edge: EdgeType::Both,
                    });
                }
                hir::HirEdgeType::Active | hir::HirEdgeType::Inactive => {
                    // Reset events are typically asynchronous and make the process sequential
                    is_sequential = true;
                    edges.push(EdgeSensitivity {
                        signal: signal_lvalue,
                        edge: self.convert_edge_type(&trigger.edge),
                    });
                }
            }
        }

        let kind = if is_sequential {
            ProcessKind::Sequential
        } else {
            ProcessKind::Combinational
        };

        let sensitivity = if !edges.is_empty() {
            SensitivityList::Edge(edges)
        } else {
            SensitivityList::Always
        };

        (kind, sensitivity)
    }

    /// Convert HIR statements to MIR block
    fn convert_statements(&mut self, hir_stmts: &[hir::HirStatement]) -> Block {
        let mut statements = Vec::new();

        for hir_stmt in hir_stmts {
            if let Some(stmt) = self.convert_statement(hir_stmt) {
                statements.push(stmt);
            }
        }

        Block { statements }
    }

    /// Convert HIR statement to MIR statement
    fn convert_statement(&mut self, stmt: &hir::HirStatement) -> Option<Statement> {
        match stmt {
            hir::HirStatement::Assignment(assign) => {
                let assigns = self.convert_assignment_expanded(assign);
                match assigns.len() {
                    0 => None,
                    1 => Some(Statement::Assignment(assigns.into_iter().next().unwrap())),
                    _ => {
                        // Multiple assignments from struct expansion - wrap in block
                        Some(Statement::Block(Block {
                            statements: assigns.into_iter().map(Statement::Assignment).collect(),
                        }))
                    }
                }
            }
            hir::HirStatement::If(if_stmt) => {
                // Try synthesis resolution first for complex conditionals
                if let Some(resolved) = self.try_synthesis_resolve_if(if_stmt) {
                    Some(Statement::ResolvedConditional(resolved))
                } else {
                    self.convert_if_statement(if_stmt).map(Statement::If)
                }
            }
            hir::HirStatement::Match(match_stmt) => self
                .convert_match_statement(match_stmt)
                .map(Statement::Case),
            hir::HirStatement::Block(stmts) => {
                // Recursively apply synthesis resolution within blocks
                Some(Statement::Block(self.convert_statements(stmts)))
            }
            hir::HirStatement::Flow(flow_stmt) => {
                // Flow statements represent pipeline stages
                // Convert to sequential assignments between pipeline stages
                self.convert_flow_statement(flow_stmt)
            }
            hir::HirStatement::Assert(_assert_stmt) => {
                // For now, skip assertions in MIR generation
                // They will be handled by the verification backend
                // TODO: Implement assertion conversion for simulation and formal verification
                None
            }
            hir::HirStatement::Property(_property_stmt) => {
                // Properties are handled by the verification backend
                // Skip in MIR generation for now
                // TODO: Implement property conversion for formal verification
                None
            }
            hir::HirStatement::Cover(_cover_stmt) => {
                // Cover statements are handled by the verification backend
                // Skip in MIR generation for now
                // TODO: Implement cover conversion for coverage collection
                None
            }
            hir::HirStatement::Let(let_stmt) => {
                // Convert let statement to assignment
                // Let bindings are local variables that need to be treated as blocking assignments

                // Check if the variable is already registered (e.g., from impl block level)
                // If not, create it dynamically (for let bindings inside event blocks)
                let var_id = if let Some(&id) = self.variable_map.get(&let_stmt.id) {
                    // Already registered
                    id
                } else {
                    // Check if we already have a dynamic variable with this name
                    // If so, reuse its ID to avoid duplicate declarations
                    let existing_var = self
                        .dynamic_variables
                        .values()
                        .find(|(_, name, _)| name == &let_stmt.name);

                    let new_id = if let Some((existing_id, _, _)) = existing_var {
                        // Reuse the existing variable ID for this name
                        *existing_id
                    } else {
                        // Create a new MIR variable on the fly for event block let bindings
                        let new_id = self.next_variable_id();

                        // Track this dynamically created variable so we can add it to the module later
                        self.dynamic_variables.insert(
                            let_stmt.id,
                            (new_id, let_stmt.name.clone(), let_stmt.var_type.clone()),
                        );

                        new_id
                    };

                    // Map this HIR variable ID to the MIR variable ID (whether new or reused)
                    self.variable_map.insert(let_stmt.id, new_id);

                    new_id
                };

                let lhs = LValue::Variable(var_id);
                let rhs = self.convert_expression(&let_stmt.value)?;
                Some(Statement::Assignment(Assignment {
                    lhs,
                    rhs,
                    kind: AssignmentKind::Blocking,
                }))
            }
            hir::HirStatement::Return(_return_expr) => {
                // Return statements are function constructs
                // For now, skip them in MIR as functions aren't yet lowered to hardware
                // TODO: Implement function support in MIR
                None
            }
            hir::HirStatement::Expression(_expr) => {
                // Expression statements (standalone expressions without assignment)
                // These represent implicit return values in functions
                // For now, skip them in MIR as they don't map to hardware
                // TODO: Handle when implementing function support in MIR
                None
            }
        }
    }

    /// Convert HIR flow statement to MIR statements
    fn convert_flow_statement(&mut self, flow_stmt: &hir::HirFlowStatement) -> Option<Statement> {
        // Convert flow pipeline to sequential assignments
        let statements = self.convert_flow_pipeline(&flow_stmt.pipeline);
        if statements.is_empty() {
            None
        } else {
            Some(Statement::Block(Block { statements }))
        }
    }

    /// Convert HIR flow pipeline to sequential assignment statements
    fn convert_flow_pipeline(&mut self, pipeline: &hir::HirFlowPipeline) -> Vec<Statement> {
        let mut statements = Vec::new();

        // Collect all stages (start + subsequent stages)
        let mut all_stages = vec![&pipeline.start];
        all_stages.extend(&pipeline.stages);

        // Convert each stage to appropriate statements
        for (i, stage) in all_stages.iter().enumerate() {
            match stage {
                hir::HirPipelineStage::Expression(expr) => {
                    // For expression stages, we need to generate assignments
                    // This represents data flowing through the pipeline
                    if let Some(assignment) = self.convert_pipeline_expression_stage(expr, i) {
                        statements.push(Statement::Assignment(assignment));
                    }
                }
                hir::HirPipelineStage::Block(stage_stmts) => {
                    // For block stages, convert the statements directly
                    for hir_stmt in stage_stmts {
                        if let Some(stmt) = self.convert_statement(hir_stmt) {
                            statements.push(stmt);
                        }
                    }
                }
            }
        }

        statements
    }

    /// Convert a pipeline expression stage to an assignment
    fn convert_pipeline_expression_stage(
        &mut self,
        expr: &hir::HirExpression,
        _stage_index: usize,
    ) -> Option<Assignment> {
        // For pipeline expressions like `a |> b |> c`, each stage represents
        // a data flow where the previous stage's output becomes the next stage's input

        // For now, convert expression directly - this will handle signal references
        // In a more sophisticated implementation, we would track pipeline registers
        match expr {
            hir::HirExpression::Signal(_)
            | hir::HirExpression::Port(_)
            | hir::HirExpression::Variable(_) => {
                // These represent signal references in the pipeline
                // For now, this doesn't generate an assignment by itself
                // The pipeline semantics will be handled by the overall flow
                None
            }
            _ => {
                // For other expressions, we can't easily convert to assignments
                // without more context about the pipeline targets
                None
            }
        }
    }

    /// Convert HIR assignment to MIR assignment
    /// Convert HIR assignment - may expand to multiple MIR assignments for struct types
    fn convert_assignment_expanded(&mut self, assign: &hir::HirAssignment) -> Vec<Assignment> {
        // CRITICAL FIX for Bug #8: Try to expand array index assignments first
        // This handles cases like: mem[index] <= data
        // Must come BEFORE struct expansion to catch array-of-struct assignments
        if let Some(assignments) = self.try_expand_array_index_assignment(assign) {
            return assignments;
        }

        // Try to expand field access assignments (simple and nested)
        // This handles: out_data.field_x <= value and out_vertex.position.x <= value
        // Must come BEFORE struct expansion to catch specific field assignments
        if let Some(assignments) = self.try_expand_field_assignment(assign) {
            return assignments;
        }

        // Try to expand struct-to-struct assignments
        if let Some(assignments) = self.try_expand_struct_assignment(assign) {
            return assignments;
        }

        // Fall back to single assignment
        if let Some(single) = self.convert_assignment(assign) {
            vec![single]
        } else {
            vec![]
        }
    }

    fn convert_assignment(&mut self, assign: &hir::HirAssignment) -> Option<Assignment> {
        let lhs = self.convert_lvalue(&assign.lhs)?;
        let rhs = self.convert_expression(&assign.rhs)?;
        let kind = match assign.assignment_type {
            hir::HirAssignmentType::NonBlocking => AssignmentKind::NonBlocking,
            hir::HirAssignmentType::Blocking => AssignmentKind::Blocking,
            hir::HirAssignmentType::Combinational => AssignmentKind::Blocking,
        };

        Some(Assignment { lhs, rhs, kind })
    }

    /// Try to expand a struct assignment into multiple field assignments
    fn try_expand_struct_assignment(
        &mut self,
        assign: &hir::HirAssignment,
    ) -> Option<Vec<Assignment>> {
        // Get assignment kind
        let kind = match assign.assignment_type {
            hir::HirAssignmentType::NonBlocking => AssignmentKind::NonBlocking,
            hir::HirAssignmentType::Blocking => AssignmentKind::Blocking,
            hir::HirAssignmentType::Combinational => AssignmentKind::Blocking,
        };

        // Check if LHS is a simple signal/port that was flattened
        let (lhs_hir_id, lhs_is_signal) = match &assign.lhs {
            hir::HirLValue::Signal(id) => {
                if self.flattened_signals.contains_key(id) {
                    (id.0, true)
                } else {
                    return None;
                }
            }
            hir::HirLValue::Port(id) => {
                if self.flattened_ports.contains_key(id) {
                    (id.0, false)
                } else {
                    return None;
                }
            }
            _ => return None, // Not a simple signal/port
        };

        // Check if RHS is a field access or signal that can be expanded
        let (rhs_hir_id, rhs_is_signal, rhs_field_path) = match &assign.rhs {
            hir::HirExpression::Signal(id) => {
                if self.flattened_signals.contains_key(id) {
                    (id.0, true, vec![])
                } else {
                    return None;
                }
            }
            hir::HirExpression::Port(id) => {
                if self.flattened_ports.contains_key(id) {
                    (id.0, false, vec![])
                } else {
                    return None;
                }
            }
            hir::HirExpression::FieldAccess { base, field } => {
                // Extract field path and base signal/port
                let mut field_path = vec![field.clone()];
                let mut current = base.as_ref();
                loop {
                    match current {
                        hir::HirExpression::FieldAccess {
                            base: inner_base,
                            field: inner_field,
                        } => {
                            field_path.insert(0, inner_field.clone());
                            current = inner_base.as_ref();
                        }
                        hir::HirExpression::Signal(id) => {
                            if self.flattened_signals.contains_key(id) {
                                break (id.0, true, field_path);
                            } else {
                                return None;
                            }
                        }
                        hir::HirExpression::Port(id) => {
                            if self.flattened_ports.contains_key(id) {
                                break (id.0, false, field_path);
                            } else {
                                return None;
                            }
                        }
                        _ => return None,
                    }
                }
            }
            _ => return None,
        };

        // Get flattened fields for LHS and RHS
        let lhs_fields = if lhs_is_signal {
            self.flattened_signals.get(&hir::SignalId(lhs_hir_id))?
        } else {
            self.flattened_ports.get(&hir::PortId(lhs_hir_id))?
        };

        let rhs_fields_all = if rhs_is_signal {
            self.flattened_signals.get(&hir::SignalId(rhs_hir_id))?
        } else {
            self.flattened_ports.get(&hir::PortId(rhs_hir_id))?
        };

        // If RHS has a field path, filter to only those fields
        let rhs_fields: Vec<_> = if rhs_field_path.is_empty() {
            rhs_fields_all.clone()
        } else {
            // Find fields that start with the field path
            rhs_fields_all
                .iter()
                .filter(|f| f.field_path.starts_with(&rhs_field_path))
                .cloned()
                .collect()
        };

        // Check if field counts match
        if lhs_fields.len() != rhs_fields.len() {
            return None;
        }

        // Generate assignments for each field
        let mut assignments = Vec::new();
        for (lhs_field, rhs_field) in lhs_fields.iter().zip(rhs_fields.iter()) {
            let lhs_lval = if lhs_is_signal {
                LValue::Signal(SignalId(lhs_field.id))
            } else {
                LValue::Port(PortId(lhs_field.id))
            };

            let rhs_expr = if rhs_is_signal {
                Expression::Ref(LValue::Signal(SignalId(rhs_field.id)))
            } else {
                Expression::Ref(LValue::Port(PortId(rhs_field.id)))
            };

            assignments.push(Assignment {
                lhs: lhs_lval,
                rhs: rhs_expr,
                kind,
            });
        }

        Some(assignments)
    }

    /// Extract the root signal ID and field path from a field access LValue
    ///
    /// Example: For `out_vertex.position.x`, returns (SignalId for out_vertex, ["position", "x"])
    fn extract_field_access_path(
        &self,
        lval: &hir::HirLValue,
    ) -> Option<(hir::SignalId, Vec<String>)> {
        let mut field_path = Vec::new();
        let mut current = lval;

        // Walk up the field access chain to build the complete path
        loop {
            match current {
                hir::HirLValue::FieldAccess { base, field } => {
                    // Insert at front to maintain left-to-right order
                    field_path.insert(0, field.clone());
                    current = base.as_ref();
                }
                hir::HirLValue::Signal(sig_id) => {
                    return Some((*sig_id, field_path));
                }
                _ => {
                    // Not a simple signal-based field access
                    return None;
                }
            }
        }
    }

    /// Try to expand a field access assignment into a single assignment to the flattened signal
    ///
    /// This handles assignments like:
    /// - Simple: `out_data.field_x <= input_value`
    /// - Nested: `out_vertex.position.x <= input_value`
    ///
    /// The field access chain is resolved to find the corresponding flattened signal,
    /// and a single assignment is generated.
    ///
    /// This is the fix for "Struct Field Assignments in Sequential Blocks" - the last
    /// remaining known issue in the compiler.
    fn try_expand_field_assignment(
        &mut self,
        assign: &hir::HirAssignment,
    ) -> Option<Vec<Assignment>> {
        // Only handle FieldAccess LValues
        if !matches!(&assign.lhs, hir::HirLValue::FieldAccess { .. }) {
            return None;
        }

        // Extract root signal and field path
        let (root_sig_id, field_path) = self.extract_field_access_path(&assign.lhs)?;

        // Look up the flattened signals for this root signal
        let flattened = self.flattened_signals.get(&root_sig_id)?;

        // Find the flattened field matching this field path
        for flat_field in flattened {
            if flat_field.field_path == field_path {
                // Determine assignment kind
                let kind = match assign.assignment_type {
                    hir::HirAssignmentType::NonBlocking => AssignmentKind::NonBlocking,
                    hir::HirAssignmentType::Blocking => AssignmentKind::Blocking,
                    hir::HirAssignmentType::Combinational => AssignmentKind::Blocking,
                };

                // Create assignment to the flattened signal
                let lhs_lval = LValue::Signal(SignalId(flat_field.id));
                let rhs_expr = self.convert_expression(&assign.rhs)?;

                return Some(vec![Assignment {
                    lhs: lhs_lval,
                    rhs: rhs_expr,
                    kind,
                }]);
            }
        }

        None // Field path not found in flattened signals
    }

    /// Try to expand an array index assignment into multiple conditional assignments
    ///
    /// This handles assignments like: `mem[index] <= data` where `mem` is a flattened array
    ///
    /// For each flattened element, generates:
    /// ```text
    /// mem_N_field <= (index == N) ? data_field : mem_N_field
    /// ```
    ///
    /// This is the ROOT CAUSE FIX for Bug #8: Properly expanding array assignments at HIR→MIR level
    fn try_expand_array_index_assignment(
        &mut self,
        assign: &hir::HirAssignment,
    ) -> Option<Vec<Assignment>> {
        // Get assignment kind
        let kind = match assign.assignment_type {
            hir::HirAssignmentType::NonBlocking => AssignmentKind::NonBlocking,
            hir::HirAssignmentType::Blocking => AssignmentKind::Blocking,
            hir::HirAssignmentType::Combinational => AssignmentKind::Blocking,
        };

        // Check if LHS is an array index: signal[index] or port[index]
        let (base_hir_lval, index_expr, is_signal) = match &assign.lhs {
            hir::HirLValue::Index(base, index) => {
                // Check if base is a flattened signal or port
                match base.as_ref() {
                    hir::HirLValue::Signal(sig_id) => {
                        if self.flattened_signals.contains_key(sig_id) {
                            (sig_id.0, index, true)
                        } else {
                            return None;
                        }
                    }
                    hir::HirLValue::Port(port_id) => {
                        if self.flattened_ports.contains_key(port_id) {
                            (port_id.0, index, false)
                        } else {
                            return None;
                        }
                    }
                    _ => {
                        return None;
                    }
                }
            }
            _ => {
                return None;
            }
        };

        // Get flattened fields for the base array (clone to avoid borrow issues)
        let base_fields = if is_signal {
            self.flattened_signals
                .get(&hir::SignalId(base_hir_lval))?
                .clone()
        } else {
            self.flattened_ports
                .get(&hir::PortId(base_hir_lval))?
                .clone()
        };

        // Convert the index expression to MIR
        let mir_index = self.convert_expression(index_expr)?;

        // Convert the RHS expression to MIR
        let mir_rhs = self.convert_expression(&assign.rhs)?;

        // Group flattened fields by array index
        // field_path will be like ["0", "x"], ["0", "y"], ["1", "x"], etc.
        let mut array_indices: std::collections::HashMap<String, Vec<FlattenedField>> =
            std::collections::HashMap::new();

        for field in base_fields {
            if let Some(first_component) = field.field_path.first() {
                // Check if first component is a numeric index
                if first_component.chars().all(|c| c.is_ascii_digit()) {
                    array_indices
                        .entry(first_component.clone())
                        .or_default()
                        .push(field);
                }
            }
        }

        if array_indices.is_empty() {
            return None;
        }

        // Generate assignments for each flattened element
        let mut assignments = Vec::new();

        for (idx_str, fields_at_index) in array_indices {
            let array_index: usize = idx_str.parse().ok()?;
            // Create condition: index == array_index
            let index_literal = Expression::Literal(Value::Integer(array_index as i64));
            let condition = Expression::Binary {
                op: BinaryOp::Equal,
                left: Box::new(mir_index.clone()),
                right: Box::new(index_literal),
            };

            // For each field at this index (e.g., x, y, z for a struct element)
            for field_info in &fields_at_index {
                // Build LHS: mem_N_field
                let lhs_lval = if is_signal {
                    LValue::Signal(SignalId(field_info.id))
                } else {
                    LValue::Port(PortId(field_info.id))
                };

                // Build RHS for this specific field
                // Need to adapt mir_rhs to reference the correct field
                let rhs_for_field = self.adapt_rhs_for_array_element(&mir_rhs, field_info);

                // Create self-reference for "keep" value: mem_N_field
                let keep_value = Expression::Ref(lhs_lval.clone());

                // Build conditional: (index == N) ? data_field : mem_N_field
                let conditional_rhs = Expression::Conditional {
                    cond: Box::new(condition.clone()),
                    then_expr: Box::new(rhs_for_field),
                    else_expr: Box::new(keep_value),
                };

                assignments.push(Assignment {
                    lhs: lhs_lval,
                    rhs: conditional_rhs,
                    kind,
                });
            }
        }

        Some(assignments)
    }

    /// Expand array index read expressions for flattened arrays (Bug #9 - RHS counterpart to Bug #8)
    ///
    /// Handles cases like: rd_data = mem[rd_addr]
    /// Where:
    /// - LHS is a flattened struct: rd_data → rd_data_x, rd_data_y, rd_data_z
    /// - RHS is an indexed flattened array: mem[rd_addr] → mem_0_x, ..., mem_7_z
    ///
    /// Expands to multiplexer logic for each field:
    /// rd_data_x = (rd_addr == 0) ? mem_0_x : (rd_addr == 1) ? mem_1_x : ... : mem_7_x
    fn try_expand_array_index_read_assignment(
        &mut self,
        assign: &hir::HirAssignment,
    ) -> Option<Vec<ContinuousAssign>> {
        // Check if LHS is a flattened signal/port (struct)
        let (lhs_hir_id, lhs_is_signal) = match &assign.lhs {
            hir::HirLValue::Signal(id) => {
                if self.flattened_signals.contains_key(id) {
                    (id.0, true)
                } else {
                    return None;
                }
            }
            hir::HirLValue::Port(id) => {
                if self.flattened_ports.contains_key(id) {
                    (id.0, false)
                } else {
                    return None;
                }
            }
            _ => {
                return None;
            }
        };

        // Check if RHS is an Index expression
        let (array_base, index_expr) = match &assign.rhs {
            hir::HirExpression::Index(base, index) => (base.as_ref(), index.as_ref()),
            _ => {
                return None;
            }
        };

        // Check if the array base is a flattened signal/port
        let (array_hir_id, array_is_signal) = match array_base {
            hir::HirExpression::Signal(id) => {
                if self.flattened_signals.contains_key(id) {
                    (id.0, true)
                } else {
                    return None;
                }
            }
            hir::HirExpression::Port(id) => {
                if self.flattened_ports.contains_key(id) {
                    (id.0, false)
                } else {
                    return None;
                }
            }
            _ => {
                return None;
            }
        };

        // Get flattened fields for LHS (struct) and array (array base)
        let lhs_fields = if lhs_is_signal {
            self.flattened_signals
                .get(&hir::SignalId(lhs_hir_id))?
                .clone()
        } else {
            self.flattened_ports.get(&hir::PortId(lhs_hir_id))?.clone()
        };

        let array_fields = if array_is_signal {
            self.flattened_signals
                .get(&hir::SignalId(array_hir_id))?
                .clone()
        } else {
            self.flattened_ports
                .get(&hir::PortId(array_hir_id))?
                .clone()
        };

        // Group array fields by index
        let mut array_by_index: std::collections::HashMap<String, Vec<FlattenedField>> =
            std::collections::HashMap::new();

        for field in array_fields {
            if let Some(first_component) = field.field_path.first() {
                if first_component.chars().all(|c| c.is_ascii_digit()) {
                    array_by_index
                        .entry(first_component.clone())
                        .or_default()
                        .push(field);
                }
            }
        }

        // Convert index expression
        let mir_index = self.convert_expression(index_expr)?;

        // Generate continuous assignments with multiplexer logic
        let mut assignments = Vec::new();

        for lhs_field in &lhs_fields {
            // Extract the field suffix for this LHS field
            // For rd_data_x, we want "x"
            // For rd_data_position_x, we want "position_x"
            let field_suffix = lhs_field.field_path.join("_");

            // Build multiplexer by iterating from last index to first
            // Result: (index == 0) ? mem_0_x : (index == 1) ? mem_1_x : ... : mem_7_x
            let mut sorted_indices: Vec<_> = array_by_index.keys().collect();
            sorted_indices.sort_by_key(|s| s.parse::<usize>().unwrap_or(0));

            let mut mux_expr: Option<Expression> = None;

            for idx_str in sorted_indices.iter().rev() {
                let array_index: usize = idx_str.parse().ok()?;

                // Find the field at this index that matches our LHS field suffix
                let fields_at_index = array_by_index.get(*idx_str)?;
                let matching_array_field = fields_at_index.iter().find(|f| {
                    // Get the field suffix (everything after array index)
                    let array_field_suffix: Vec<String> =
                        f.field_path.iter().skip(1).cloned().collect();
                    let array_field_suffix_str = array_field_suffix.join("_");

                    // BUG #31 FIX: When LHS field_path is empty ([]), it means we're working with
                    // an already-flattened leaf port (e.g., rd_data_value). In this case, we should
                    // match array elements that also have a single-component suffix after the index.
                    // Example: LHS=rd_data_value (field_path=[]), array=mem_N_value (field_path=["N","value"])
                    if field_suffix.is_empty() && array_field_suffix.len() == 1 {
                        return true;
                    }

                    array_field_suffix_str == field_suffix
                })?;

                // Create reference to this array element's field
                let array_element_ref = if array_is_signal {
                    Expression::Ref(LValue::Signal(SignalId(matching_array_field.id)))
                } else {
                    Expression::Ref(LValue::Port(PortId(matching_array_field.id)))
                };

                if mux_expr.is_none() {
                    // First iteration (highest index) - use as default
                    mux_expr = Some(array_element_ref);
                } else {
                    // Subsequent iterations - wrap previous with conditional
                    let condition = Expression::Binary {
                        op: BinaryOp::Equal,
                        left: Box::new(mir_index.clone()),
                        right: Box::new(Expression::Literal(Value::Integer(array_index as i64))),
                    };

                    mux_expr = Some(Expression::Conditional {
                        cond: Box::new(condition),
                        then_expr: Box::new(array_element_ref),
                        else_expr: Box::new(mux_expr.unwrap()),
                    });
                }
            }

            // Create LHS reference
            let lhs_lval = if lhs_is_signal {
                LValue::Signal(SignalId(lhs_field.id))
            } else {
                LValue::Port(PortId(lhs_field.id))
            };

            assignments.push(ContinuousAssign {
                lhs: lhs_lval,
                rhs: mux_expr?,
            });
        }

        Some(assignments)
    }

    /// Adapt RHS expression to reference the correct field for an array element
    ///
    /// For example, if RHS is `Expression::Ref(LValue::Signal(wr_data))` and we need field "x",
    /// this should return `Expression::Ref(LValue::Signal(wr_data_x))`
    fn adapt_rhs_for_array_element(
        &self,
        rhs: &Expression,
        target_field: &FlattenedField,
    ) -> Expression {
        // Extract the struct field suffix from target_field.field_path
        // For ["0", "x"], we want "x"
        // For ["0", "position", "x"], we want "position_x"
        let field_suffix: Vec<String> = target_field
            .field_path
            .iter()
            .skip(1) // Skip the array index
            .cloned()
            .collect();

        if field_suffix.is_empty() {
            // No struct fields, just array - return RHS as-is
            return rhs.clone();
        }

        // Recursively adapt the expression to reference the field
        self.adapt_expression_for_field(rhs, &field_suffix)
    }

    /// Recursively adapt an expression to reference a specific field
    fn adapt_expression_for_field(&self, expr: &Expression, field_path: &[String]) -> Expression {
        match expr {
            Expression::Ref(lval) => Expression::Ref(self.adapt_lvalue_for_field(lval, field_path)),
            Expression::Binary { op, left, right } => Expression::Binary {
                op: *op,
                left: Box::new(self.adapt_expression_for_field(left, field_path)),
                right: Box::new(self.adapt_expression_for_field(right, field_path)),
            },
            Expression::Unary { op, operand } => Expression::Unary {
                op: *op,
                operand: Box::new(self.adapt_expression_for_field(operand, field_path)),
            },
            Expression::Conditional {
                cond,
                then_expr,
                else_expr,
            } => Expression::Conditional {
                cond: Box::new(self.adapt_expression_for_field(cond, field_path)),
                then_expr: Box::new(self.adapt_expression_for_field(then_expr, field_path)),
                else_expr: Box::new(self.adapt_expression_for_field(else_expr, field_path)),
            },
            // Literals and other non-reference expressions don't need adaptation
            _ => expr.clone(),
        }
    }

    /// Adapt an LValue to reference a specific field
    fn adapt_lvalue_for_field(&self, lval: &LValue, field_path: &[String]) -> LValue {
        match lval {
            LValue::Signal(sig_id) => {
                // BUG #33 FIX: Use reverse map to find the HIR signal this MIR signal belongs to
                if let Some((hir_sig_id, _mir_field_path)) = self.signal_to_hir.get(sig_id) {
                    // Get all sibling fields for this HIR signal
                    if let Some(fields) = self.flattened_signals.get(hir_sig_id) {
                        // Find the sibling whose path ends with the requested field_path
                        for sibling in fields {
                            if sibling.field_path.ends_with(field_path) {
                                return LValue::Signal(SignalId(sibling.id));
                            }
                        }
                    }
                }
                lval.clone()
            }
            LValue::Port(port_id) => {
                // BUG #33 FIX: Use reverse map to find the HIR port this MIR port belongs to
                if let Some((hir_port_id, _mir_field_path)) = self.port_to_hir.get(port_id) {
                    // Get all sibling fields for this HIR port
                    if let Some(fields) = self.flattened_ports.get(hir_port_id) {
                        // Find the sibling whose path ends with the requested field_path
                        for sibling in fields {
                            if sibling.field_path.ends_with(field_path) {
                                return LValue::Port(PortId(sibling.id));
                            }
                        }
                    }
                }
                lval.clone()
            }
            // For other LValue types, recurse on base
            LValue::BitSelect { base, index } => LValue::BitSelect {
                base: Box::new(self.adapt_lvalue_for_field(base, field_path)),
                index: index.clone(),
            },
            LValue::RangeSelect { base, high, low } => LValue::RangeSelect {
                base: Box::new(self.adapt_lvalue_for_field(base, field_path)),
                high: high.clone(),
                low: low.clone(),
            },
            _ => lval.clone(),
        }
    }

    /// Convert continuous assignment - may expand to multiple assignments for struct types
    fn convert_continuous_assignment_expanded(
        &mut self,
        assign: &hir::HirAssignment,
    ) -> Vec<ContinuousAssign> {
        // Only combinational assignments become continuous assigns
        if !matches!(
            assign.assignment_type,
            hir::HirAssignmentType::Combinational
        ) {
            return vec![];
        }

        // CRITICAL FIX for Bug #9: Try to expand array index read assignments first
        // This handles cases like: rd_data = mem[index]
        // This is the RHS counterpart to Bug #8 (array index writes)
        if let Some(assigns) = self.try_expand_array_index_read_assignment(assign) {
            return assigns;
        }

        // Try to expand struct-to-struct assignments
        if let Some(assigns) = self.try_expand_struct_continuous_assignment(assign) {
            return assigns;
        }

        // Fall back to single assignment
        if let Some(single) = self.convert_continuous_assignment(assign) {
            vec![single]
        } else {
            vec![]
        }
    }

    fn convert_continuous_assignment(
        &mut self,
        assign: &hir::HirAssignment,
    ) -> Option<ContinuousAssign> {
        // Only combinational assignments become continuous assigns
        if !matches!(
            assign.assignment_type,
            hir::HirAssignmentType::Combinational
        ) {
            return None;
        }

        let lhs = self.convert_lvalue(&assign.lhs)?;
        let rhs = self.convert_expression(&assign.rhs)?;

        Some(ContinuousAssign { lhs, rhs })
    }

    /// Try to expand a struct continuous assignment into multiple field assignments
    fn try_expand_struct_continuous_assignment(
        &mut self,
        assign: &hir::HirAssignment,
    ) -> Option<Vec<ContinuousAssign>> {
        // Check if LHS is a simple signal/port that was flattened
        let (lhs_hir_id, lhs_is_signal) = match &assign.lhs {
            hir::HirLValue::Signal(id) => {
                if self.flattened_signals.contains_key(id) {
                    (id.0, true)
                } else {
                    return None;
                }
            }
            hir::HirLValue::Port(id) => {
                if self.flattened_ports.contains_key(id) {
                    (id.0, false)
                } else {
                    return None;
                }
            }
            _ => return None, // Not a simple signal/port
        };

        // Check if RHS is a field access or signal that can be expanded
        let (rhs_hir_id, rhs_is_signal, rhs_field_path) = match &assign.rhs {
            hir::HirExpression::Signal(id) => {
                if self.flattened_signals.contains_key(id) {
                    (id.0, true, vec![])
                } else {
                    return None;
                }
            }
            hir::HirExpression::Port(id) => {
                if self.flattened_ports.contains_key(id) {
                    (id.0, false, vec![])
                } else {
                    return None;
                }
            }
            hir::HirExpression::FieldAccess { base, field } => {
                // Extract field path and base signal/port
                let mut field_path = vec![field.clone()];
                let mut current = base.as_ref();
                loop {
                    match current {
                        hir::HirExpression::FieldAccess {
                            base: inner_base,
                            field: inner_field,
                        } => {
                            field_path.insert(0, inner_field.clone());
                            current = inner_base.as_ref();
                        }
                        hir::HirExpression::Signal(id) => {
                            if self.flattened_signals.contains_key(id) {
                                break (id.0, true, field_path);
                            } else {
                                return None;
                            }
                        }
                        hir::HirExpression::Port(id) => {
                            if self.flattened_ports.contains_key(id) {
                                break (id.0, false, field_path);
                            } else {
                                return None;
                            }
                        }
                        _ => return None,
                    }
                }
            }
            _ => return None,
        };

        // Get flattened fields for LHS and RHS
        let lhs_fields = if lhs_is_signal {
            self.flattened_signals.get(&hir::SignalId(lhs_hir_id))?
        } else {
            self.flattened_ports.get(&hir::PortId(lhs_hir_id))?
        };

        let rhs_fields_all = if rhs_is_signal {
            self.flattened_signals.get(&hir::SignalId(rhs_hir_id))?
        } else {
            self.flattened_ports.get(&hir::PortId(rhs_hir_id))?
        };

        // If RHS has a field path, filter to only those fields
        let rhs_fields: Vec<_> = if rhs_field_path.is_empty() {
            rhs_fields_all.clone()
        } else {
            // Find fields that start with the field path
            rhs_fields_all
                .iter()
                .filter(|f| f.field_path.starts_with(&rhs_field_path))
                .cloned()
                .collect()
        };

        // Check if field counts match
        if lhs_fields.len() != rhs_fields.len() {
            return None;
        }

        // Generate continuous assignments for each field
        let mut assignments = Vec::new();
        for (lhs_field, rhs_field) in lhs_fields.iter().zip(rhs_fields.iter()) {
            let lhs_lval = if lhs_is_signal {
                LValue::Signal(SignalId(lhs_field.id))
            } else {
                LValue::Port(PortId(lhs_field.id))
            };

            let rhs_expr = if rhs_is_signal {
                Expression::Ref(LValue::Signal(SignalId(rhs_field.id)))
            } else {
                Expression::Ref(LValue::Port(PortId(rhs_field.id)))
            };

            assignments.push(ContinuousAssign {
                lhs: lhs_lval,
                rhs: rhs_expr,
            });
        }

        Some(assignments)
    }

    /// Convert module instance
    fn convert_instance(&mut self, instance: &hir::HirInstance) -> Option<ModuleInstance> {
        // Map entity ID to module ID
        let module_id = *self.entity_map.get(&instance.entity)?;

        // Convert connections
        // CRITICAL FIX for Bug #10: Expand struct/array connections into flattened field connections
        let mut connections = std::collections::HashMap::new();

        for conn in &instance.connections {
            // Check if this port is flattened (struct or array type)
            // If so, we need to create multiple connections for each flattened field

            // First, try to get the HIR port from the entity to check its type
            let entity = self
                .hir
                .as_ref()?
                .entities
                .iter()
                .find(|e| e.id == instance.entity)?;

            // Find the port in the entity
            let port_opt = entity.ports.iter().find(|p| p.name == conn.port);

            // Check if the RHS expression is a reference to a flattened signal/port
            let expanded_connections = if let Some(port) = port_opt {
                // Check if port type is a struct or array that gets flattened
                let needs_expansion = matches!(
                    port.port_type,
                    hir::HirType::Struct(_) | hir::HirType::Array(_, _)
                );

                if needs_expansion {
                    // Try to expand the connection
                    self.expand_instance_connection(&conn.port, &conn.expr)?
                } else {
                    // Simple connection
                    vec![(conn.port.clone(), self.convert_expression(&conn.expr)?)]
                }
            } else {
                // Port not found in entity, use default conversion
                vec![(conn.port.clone(), self.convert_expression(&conn.expr)?)]
            };

            for (port_name, expr) in expanded_connections {
                connections.insert(port_name, expr);
            }
        }

        Some(ModuleInstance {
            name: instance.name.clone(),
            module: module_id,
            connections,
            parameters: std::collections::HashMap::new(),
        })
    }

    /// Expand a struct/array instance connection into multiple flattened connections
    /// Example: rd_data: input_fifo_rd_data becomes:
    ///   rd_data_x: input_fifo_rd_data_x
    ///   rd_data_y: input_fifo_rd_data_y
    ///   rd_data_z: input_fifo_rd_data_z
    fn expand_instance_connection(
        &self,
        base_port_name: &str,
        rhs_expr: &hir::HirExpression,
    ) -> Option<Vec<(String, Expression)>> {
        // Get the base signal/port from RHS
        let (base_hir_id, is_signal) = match rhs_expr {
            hir::HirExpression::Signal(id) => (id.0, true),
            hir::HirExpression::Port(id) => (id.0, false),
            _ => {
                return None; // Complex expression, can't expand
            }
        };

        // Get flattened fields for the RHS signal/port
        let rhs_fields = if is_signal {
            match self.flattened_signals.get(&hir::SignalId(base_hir_id)) {
                Some(fields) => fields.clone(),
                None => {
                    return None;
                }
            }
        } else {
            match self.flattened_ports.get(&hir::PortId(base_hir_id)) {
                Some(fields) => fields.clone(),
                None => {
                    return None;
                }
            }
        };

        // Create connections for each flattened field
        let mut connections = Vec::new();
        for rhs_field in &rhs_fields {
            // Build port name: base_port_name + field_path
            // For example: rd_data + ["x"] = rd_data_x
            let port_field_name = if rhs_field.field_path.is_empty() {
                base_port_name.to_string()
            } else {
                format!("{}_{}", base_port_name, rhs_field.field_path.join("_"))
            };

            // Create expression referencing the flattened RHS field
            let rhs_field_expr = if is_signal {
                Expression::Ref(LValue::Signal(SignalId(rhs_field.id)))
            } else {
                Expression::Ref(LValue::Port(PortId(rhs_field.id)))
            };

            connections.push((port_field_name, rhs_field_expr));
        }

        Some(connections)
    }

    /// Convert HIR if statement to MIR
    fn convert_if_statement(&mut self, if_stmt: &hir::HirIfStatement) -> Option<IfStatement> {
        let condition = self.convert_expression(&if_stmt.condition)?;
        let then_block = self.convert_statements(&if_stmt.then_statements);
        let else_block = if_stmt
            .else_statements
            .as_ref()
            .map(|stmts| self.convert_statements(stmts));

        Some(IfStatement {
            condition,
            then_block,
            else_block,
        })
    }

    /// Convert HIR match statement to MIR case statement
    fn convert_match_statement(
        &mut self,
        match_stmt: &hir::HirMatchStatement,
    ) -> Option<CaseStatement> {
        let expr = self.convert_expression(&match_stmt.expr)?;

        let mut items = Vec::new();
        let mut default = None;

        for arm in &match_stmt.arms {
            let block = self.convert_statements(&arm.statements);
            let final_block = if let Some(guard_expr) = &arm.guard {
                // If there's a guard, wrap the statements in an if statement
                let guard_condition = self.convert_expression(guard_expr)?;
                let if_stmt = Statement::If(IfStatement {
                    condition: guard_condition,
                    then_block: block,
                    else_block: None,
                });
                Block {
                    statements: vec![if_stmt],
                }
            } else {
                block
            };

            // Convert pattern to expression for case matching
            match self.convert_pattern_to_expr(&arm.pattern) {
                Some(expr_val) => {
                    items.push(CaseItem {
                        values: vec![expr_val],
                        block: final_block,
                    });
                }
                None => {
                    // Wildcard pattern becomes default case
                    default = Some(final_block);
                }
            }
        }

        Some(CaseStatement {
            expr,
            items,
            default,
        })
    }

    /// Convert pattern to expression (for case values)
    fn convert_pattern_to_expr(&mut self, pattern: &hir::HirPattern) -> Option<Expression> {
        match pattern {
            hir::HirPattern::Literal(lit) => {
                // Convert literal pattern to expression
                match lit {
                    hir::HirLiteral::Integer(val) => {
                        Some(Expression::Literal(Value::Integer(*val as i64)))
                    }
                    hir::HirLiteral::Float(f) => Some(Expression::Literal(Value::Float(*f))),
                    hir::HirLiteral::String(s) => {
                        Some(Expression::Literal(Value::String(s.clone())))
                    }
                    hir::HirLiteral::Boolean(b) => {
                        // Convert boolean to integer (true = 1, false = 0)
                        Some(Expression::Literal(Value::Integer(if *b { 1 } else { 0 })))
                    }
                    hir::HirLiteral::BitVector(bits) => {
                        // Convert bit vector to integer value
                        let width = bits.len();
                        let mut value = 0u64;
                        for (i, bit) in bits.iter().enumerate() {
                            if *bit {
                                value |= 1 << i;
                            }
                        }
                        Some(Expression::Literal(Value::BitVector { width, value }))
                    }
                }
            }
            hir::HirPattern::Path(enum_name, variant_name) => {
                // For enum patterns, we need to convert to the enum variant value
                // TODO: Implement proper enum variant value lookup from type information
                // For now, we'll use a simple mapping based on common variant names
                let variant_value = match variant_name.as_str() {
                    "Idle" => 0,
                    "Active" => 1,
                    "Done" => 2,
                    _ => {
                        // Calculate a simple hash for unknown variants to ensure consistent values
                        variant_name.chars().map(|c| c as u32).sum::<u32>() % 16
                    }
                };
                Some(Expression::Literal(Value::Integer(variant_value as i64)))
            }
            hir::HirPattern::Variable(_) => {
                // Variable patterns can't be converted to case values directly
                // They should be handled differently (as binding patterns)
                None
            }
            hir::HirPattern::Wildcard => {
                // Wildcard patterns become the default case
                None
            }
            hir::HirPattern::Tuple(_patterns) => {
                // Tuple patterns are complex - for now, treat as wildcard
                // TODO: Implement proper tuple pattern handling
                None
            }
        }
    }

    /// Convert HIR lvalue to MIR
    fn convert_lvalue(&mut self, lval: &hir::HirLValue) -> Option<LValue> {
        let result = match lval {
            hir::HirLValue::Signal(id) => self.signal_map.get(id).map(|&id| LValue::Signal(id)),
            hir::HirLValue::Variable(id) => {
                self.variable_map.get(id).map(|&id| LValue::Variable(id))
            }
            hir::HirLValue::Port(id) => self.port_map.get(id).map(|&id| LValue::Port(id)),
            hir::HirLValue::Index(base, index) => {
                let base = Box::new(self.convert_lvalue(base)?);
                let index = Box::new(self.convert_expression(index)?);
                Some(LValue::BitSelect { base, index })
            }
            hir::HirLValue::Range(base, high, low) => {
                let base = Box::new(self.convert_lvalue(base)?);
                let high = Box::new(self.convert_expression(high)?);
                let low = Box::new(self.convert_expression(low)?);
                Some(LValue::RangeSelect { base, high, low })
            }
            hir::HirLValue::FieldAccess { base, field } => {
                // Check if this is a field access on a flattened signal/port
                // Build the complete field path from nested accesses
                let mut field_path = vec![field.clone()];
                let mut current_base = base.as_ref();

                // Walk up the chain to find the root signal/port and complete field path
                loop {
                    match current_base {
                        hir::HirLValue::FieldAccess {
                            base: inner_base,
                            field: inner_field,
                        } => {
                            field_path.insert(0, inner_field.clone());
                            current_base = inner_base.as_ref();
                        }
                        hir::HirLValue::Signal(sig_id) => {
                            // Check if this signal was flattened
                            if let Some(flattened) = self.flattened_signals.get(sig_id) {
                                // Find the flattened field with matching path
                                for flat_field in flattened {
                                    if flat_field.field_path == field_path {
                                        return Some(LValue::Signal(SignalId(flat_field.id)));
                                    }
                                }
                            }
                            // Not flattened or field not found - fall back to old behavior
                            break;
                        }
                        hir::HirLValue::Port(port_id) => {
                            // Check if this port was flattened
                            if let Some(flattened) = self.flattened_ports.get(port_id) {
                                // Find the flattened field with matching path
                                for flat_field in flattened {
                                    if flat_field.field_path == field_path {
                                        return Some(LValue::Port(PortId(flat_field.id)));
                                    }
                                }
                            }
                            // Not flattened or field not found - fall back to old behavior
                            break;
                        }
                        _ => {
                            // Complex base - fall back to old behavior
                            break;
                        }
                    }
                }

                // Fallback: convert struct field access to bit range (for non-flattened types)
                let base_mir = self.convert_lvalue(base)?;
                let base_expr = self.lvalue_to_expression(base)?;
                let (high_bit, low_bit) = self.get_field_bit_range(&base_expr, field)?;

                let high_expr = Expression::Literal(Value::Integer(high_bit as i64));
                let low_expr = Expression::Literal(Value::Integer(low_bit as i64));

                Some(LValue::RangeSelect {
                    base: Box::new(base_mir),
                    high: Box::new(high_expr),
                    low: Box::new(low_expr),
                })
            }
        };
        result
    }

    /// Convert LValue to HirExpression for type inference
    fn lvalue_to_expression(&self, lval: &hir::HirLValue) -> Option<hir::HirExpression> {
        match lval {
            hir::HirLValue::Signal(id) => Some(hir::HirExpression::Signal(*id)),
            hir::HirLValue::Variable(id) => Some(hir::HirExpression::Variable(*id)),
            hir::HirLValue::Port(id) => Some(hir::HirExpression::Port(*id)),
            _ => None, // For complex LValues, we can't easily convert back
        }
    }

    /// Convert HIR expression to MIR
    fn convert_expression(&mut self, expr: &hir::HirExpression) -> Option<Expression> {
        match expr {
            hir::HirExpression::Literal(lit) => self.convert_literal(lit).map(Expression::Literal),
            hir::HirExpression::Signal(id) => {
                // First try signal_map
                if let Some(&signal_id) = self.signal_map.get(id) {
                    Some(Expression::Ref(LValue::Signal(signal_id)))
                } else {
                    // If not found in signals, check if it's a port masquerading as a signal
                    // (due to HIR builder converting ports to signals in expressions)
                    let hir_port_id = hir::PortId(id.0);
                    if let Some(&mir_port_id) = self.port_map.get(&hir_port_id) {
                        Some(Expression::Ref(LValue::Port(mir_port_id)))
                    } else {
                        None
                    }
                }
            }
            hir::HirExpression::Port(id) => {
                // Handle port references in expressions
                if let Some(&mir_port_id) = self.port_map.get(id) {
                    Some(Expression::Ref(LValue::Port(mir_port_id)))
                } else {
                    None
                }
            }
            hir::HirExpression::Variable(id) => self
                .variable_map
                .get(id)
                .map(|&id| Expression::Ref(LValue::Variable(id))),
            hir::HirExpression::Constant(id) => {
                // Look up and evaluate the constant value in HIR
                if let Some(hir) = self.hir {
                    // Create evaluator and register all constants from all implementations
                    let mut evaluator = ConstEvaluator::new();
                    for implementation in &hir.implementations {
                        evaluator.register_constants(&implementation.constants);
                    }

                    // Find the constant and evaluate it
                    for implementation in &hir.implementations {
                        for constant in &implementation.constants {
                            if constant.id == *id {
                                // Evaluate the constant's value expression to a concrete value
                                if let Ok(const_value) = evaluator.eval(&constant.value) {
                                    // Convert ConstValue to MIR literal expression
                                    return Some(self.const_value_to_mir_expression(&const_value));
                                } else {
                                    // If evaluation fails, try recursive conversion as fallback
                                    return self.convert_expression(&constant.value);
                                }
                            }
                        }
                    }
                }
                // Fallback if constant not found
                Some(Expression::Literal(Value::Integer(0)))
            }
            hir::HirExpression::GenericParam(param_name) => {
                // Look up the generic parameter value in the current context
                // Generic parameters are treated as constants after monomorphization
                if let Some(hir) = self.hir {
                    // First, try to find it in the current entity's generic parameters
                    if let Some(entity_id) = self.current_entity_id {
                        for entity in &hir.entities {
                            if entity.id == entity_id {
                                for generic in &entity.generics {
                                    if generic.name == *param_name {
                                        // Found the generic parameter, use its default value
                                        if let Some(default_expr) = &generic.default_value {
                                            // Recursively convert the default value expression
                                            return self.convert_expression(default_expr);
                                        }
                                    }
                                }
                                break;
                            }
                        }
                    }

                    // If not found as generic param, try constants in implementation
                    for implementation in &hir.implementations {
                        for constant in &implementation.constants {
                            if constant.name == *param_name {
                                return self.convert_expression(&constant.value);
                            }
                        }
                    }
                }
                // If not found, return a literal 0 as fallback
                // This will be properly resolved during monomorphization/type checking
                Some(Expression::Literal(Value::Integer(0)))
            }
            hir::HirExpression::Binary(binary) => {
                let left = Box::new(self.convert_expression(&binary.left)?);
                let right = Box::new(self.convert_expression(&binary.right)?);
                let op = self.convert_binary_op(&binary.op, &binary.left);
                Some(Expression::Binary { op, left, right })
            }
            hir::HirExpression::Unary(unary) => {
                let operand = Box::new(self.convert_expression(&unary.operand)?);
                let op = self.convert_unary_op(&unary.op);
                Some(Expression::Unary { op, operand })
            }
            hir::HirExpression::Call(_call) => {
                // TODO: Handle function calls
                None
            }
            hir::HirExpression::Index(base, index) => {
                // BUG #27 FIX: Check if this is a constant index into a flattened array
                // If so, directly reference the flattened signal instead of creating BitSelect
                let mut try_const_array_index = || -> Option<Expression> {
                    // Check if base is a flattened signal or port
                    let (base_hir_id, is_signal) = match base.as_ref() {
                        hir::HirExpression::Signal(id) => {
                            if self.flattened_signals.contains_key(id) {
                                (id.0, true)
                            } else {
                                return None;
                            }
                        }
                        hir::HirExpression::Port(id) => {
                            if self.flattened_ports.contains_key(id) {
                                (id.0, false)
                            } else {
                                return None;
                            }
                        }
                        _ => return None,
                    };

                    // Try to evaluate index as constant
                    let index_val = self.try_eval_const_expr(index)?;

                    // Get flattened fields
                    let fields = if is_signal {
                        self.flattened_signals.get(&hir::SignalId(base_hir_id))?
                    } else {
                        self.flattened_ports.get(&hir::PortId(base_hir_id))?
                    };

                    // Find the field with matching index
                    let index_str = index_val.to_string();
                    for field in fields {
                        if field.field_path.first() == Some(&index_str)
                            && field.field_path.len() == 1
                        {
                            // Found it! Return direct signal/port reference
                            return if is_signal {
                                Some(Expression::Ref(LValue::Signal(SignalId(field.id))))
                            } else {
                                Some(Expression::Ref(LValue::Port(PortId(field.id))))
                            };
                        }
                    }
                    None
                };

                // Try constant array index optimization
                if let Some(expr) = try_const_array_index() {
                    return Some(expr);
                }

                // BUG #29 FIX: Check if this is a dynamic index into a flattened array
                // If so, generate a MUX tree instead of BitSelect
                let mut try_dynamic_array_index = || -> Option<Expression> {
                    // Check if base is a flattened signal or port
                    let (base_hir_id, is_signal) = match base.as_ref() {
                        hir::HirExpression::Signal(id) => {
                            if self.flattened_signals.contains_key(id) {
                                (id.0, true)
                            } else {
                                return None;
                            }
                        }
                        hir::HirExpression::Port(id) => {
                            if self.flattened_ports.contains_key(id) {
                                (id.0, false)
                            } else {
                                return None;
                            }
                        }
                        _ => return None,
                    };

                    // Get flattened fields and clone to avoid borrow issues
                    let fields = if is_signal {
                        self.flattened_signals
                            .get(&hir::SignalId(base_hir_id))?
                            .clone()
                    } else {
                        self.flattened_ports.get(&hir::PortId(base_hir_id))?.clone()
                    };

                    // Group fields by array index (first component of path)
                    // For simple arrays: ["0"], ["1"], etc.
                    // For arrays of structs: ["0", "x"], ["0", "y"], ["1", "x"], ["1", "y"], etc.
                    use std::collections::HashMap as StdHashMap;
                    let mut array_groups: StdHashMap<usize, Vec<FlattenedField>> =
                        StdHashMap::new();

                    for field in &fields {
                        if let Some(first) = field.field_path.first() {
                            // Check if first component is numeric (array index)
                            if first.chars().all(|c| c.is_ascii_digit()) {
                                if let Ok(idx) = first.parse::<usize>() {
                                    array_groups.entry(idx).or_default().push(field.clone());
                                }
                            }
                        }
                    }

                    // Must have at least one array element
                    if array_groups.is_empty() {
                        return None;
                    }

                    // Convert array_groups to sorted vec
                    let mut array_elements: Vec<(usize, Vec<FlattenedField>)> =
                        array_groups.into_iter().collect();
                    array_elements.sort_by_key(|(idx, _)| *idx);

                    // Convert index expression to MIR (needs mutable borrow)
                    let mir_index = self.convert_expression(index)?;

                    // NOTE: For arrays of structs, we can't build struct literals in MIR
                    // because MIR only has scalar expressions. The array read should be
                    // handled by try_expand_array_index_read_assignment which creates
                    // multiple assignments (one per struct field).
                    //
                    // This code path is only for simple scalar arrays where a single
                    // MUX tree can select the value.

                    // Build element expressions - only handle simple scalar arrays here
                    let mut element_exprs: Vec<(usize, Expression)> = Vec::new();

                    for (array_idx, group_fields) in &array_elements {
                        if group_fields.len() == 1 && group_fields[0].field_path.len() == 1 {
                            // Simple scalar array element - just reference the field
                            let field = &group_fields[0];
                            let elem_expr = if is_signal {
                                Expression::Ref(LValue::Signal(SignalId(field.id)))
                            } else {
                                Expression::Ref(LValue::Port(PortId(field.id)))
                            };
                            element_exprs.push((*array_idx, elem_expr));
                        } else {
                            // Array of structs - this should be handled by try_expand_array_index_read_assignment
                            // instead of here. Fall back to BitSelect.
                            return None;
                        }
                    }

                    // Build MUX tree: (idx == 0) ? elem_0 : ((idx == 1) ? elem_1 : ...)
                    let mut mux_expr = None;

                    // Build from last to first (nested ternaries)
                    for (array_idx, elem_expr) in element_exprs.iter().rev() {
                        if let Some(else_expr) = mux_expr {
                            // Build condition: index == array_idx
                            let index_literal =
                                Expression::Literal(Value::Integer(*array_idx as i64));
                            let condition = Expression::Binary {
                                op: BinaryOp::Equal,
                                left: Box::new(mir_index.clone()),
                                right: Box::new(index_literal),
                            };

                            // Build ternary: condition ? elem_expr : else_expr
                            mux_expr = Some(Expression::Conditional {
                                cond: Box::new(condition),
                                then_expr: Box::new(elem_expr.clone()),
                                else_expr: Box::new(else_expr),
                            });
                        } else {
                            // Last element (no else branch)
                            mux_expr = Some(elem_expr.clone());
                        }
                    }

                    mux_expr
                };

                // Try dynamic array index expansion
                if let Some(expr) = try_dynamic_array_index() {
                    return Some(expr);
                }

                // Fall back to bit select
                let base_lval = self.expr_to_lvalue(base)?;
                let index_expr = self.convert_expression(index)?;
                Some(Expression::Ref(LValue::BitSelect {
                    base: Box::new(base_lval),
                    index: Box::new(index_expr),
                }))
            }
            hir::HirExpression::Range(base, high, low) => {
                // Convert range expression to range select
                let base_lval = self.expr_to_lvalue(base)?;
                let high_expr = self.convert_expression(high)?;
                let low_expr = self.convert_expression(low)?;
                Some(Expression::Ref(LValue::RangeSelect {
                    base: Box::new(base_lval),
                    high: Box::new(high_expr),
                    low: Box::new(low_expr),
                }))
            }
            hir::HirExpression::FieldAccess { base, field } => {
                // Convert struct field access to bit slice (range select)
                self.convert_field_access(base, field)
            }
            hir::HirExpression::EnumVariant { enum_type, variant } => {
                // Look up the enum variant value
                self.resolve_enum_variant_value(enum_type, variant)
            }
            hir::HirExpression::AssociatedConstant {
                type_name,
                constant_name,
            } => {
                // Associated constants like fp32::ZERO, T::MAX_VALUE, etc.
                // These should be resolved during monomorphization/type checking
                // For now, return a placeholder value
                // TODO: Implement proper trait-based resolution of associated constants
                //       - Look up trait implementations for the type
                //       - Find the constant definition
                //       - Return its value
                // Common associated constants:
                // - ZERO: 0
                // - ONE: 1
                // - MAX_VALUE: type maximum
                // - MIN_VALUE: type minimum
                match constant_name.as_str() {
                    "ZERO" => Some(Expression::Literal(Value::Integer(0))),
                    "ONE" => Some(Expression::Literal(Value::Integer(1))),
                    "MAX_VALUE" => Some(Expression::Literal(Value::Integer(i64::MAX))),
                    "MIN_VALUE" => Some(Expression::Literal(Value::Integer(i64::MIN))),
                    _ => {
                        // Unknown associated constant - return 0 as fallback
                        // This will be properly resolved during type checking/monomorphization
                        Some(Expression::Literal(Value::Integer(0)))
                    }
                }
            }
            hir::HirExpression::ArrayRepeat { value, count } => {
                // Array repeat expression: [value; count]
                // This creates an array with `count` copies of `value`
                // For MIR, we could:
                // 1. Expand the array at compile-time if count is a constant
                // 2. Generate initialization code at runtime
                //
                // For now, we'll try to evaluate count as a const expression
                // If it's constant and reasonably small, expand it
                // Otherwise, return a placeholder (proper array support in MIR needed)

                // Try to evaluate count
                if let Ok(count_val) = self.const_evaluator.eval(count) {
                    if let Some(count_nat) = count_val.as_nat() {
                        // For very large arrays, don't expand (could cause memory issues)
                        // Just return a zero value as placeholder
                        // TODO: Add proper array initialization support in MIR/codegen
                        if count_nat > 1024 {
                            // Return zero for large arrays
                            return Some(Expression::Literal(Value::Integer(0)));
                        }

                        // For small arrays, we could expand, but for now just return
                        // the value (proper array support needed)
                        return self.convert_expression(value);
                    }
                }

                // If count couldn't be evaluated, return placeholder
                Some(Expression::Literal(Value::Integer(0)))
            }
            hir::HirExpression::Concat(expressions) => {
                // Bit concatenation: {a, b, c}
                // In hardware, concatenation combines multiple bit vectors into a single wider vector
                // The first element becomes the most significant bits
                //
                // For now, we'll use binary operations to combine the values
                // TODO: Add proper concatenation operator in MIR

                if expressions.is_empty() {
                    return Some(Expression::Literal(Value::Integer(0)));
                }

                if expressions.len() == 1 {
                    return self.convert_expression(&expressions[0]);
                }

                // For multiple expressions, chain them together with shifts and ORs
                // {a, b, c} becomes (a << (width_b + width_c)) | (b << width_c) | c
                // For now, return the first expression as a placeholder
                // This will need proper bit width calculation and concatenation support
                // TODO: Implement proper concatenation with bit width tracking
                self.convert_expression(&expressions[0])
            }
            hir::HirExpression::Ternary {
                condition,
                true_expr,
                false_expr,
            } => {
                // Ternary conditional expression: condition ? true_expr : false_expr
                // This is identical to an if-expression, so convert to MIR conditional
                let cond = Box::new(self.convert_expression(condition)?);
                let then_expr = Box::new(self.convert_expression(true_expr)?);
                let else_expr = Box::new(self.convert_expression(false_expr)?);

                Some(Expression::Conditional {
                    cond,
                    then_expr,
                    else_expr,
                })
            }
            hir::HirExpression::If(if_expr) => {
                // Convert if-expression to a conditional (ternary) expression in MIR
                // MIR represents this as: condition ? then_expr : else_expr
                let cond = Box::new(self.convert_expression(&if_expr.condition)?);
                let then_expr = Box::new(self.convert_expression(&if_expr.then_expr)?);
                let else_expr = Box::new(self.convert_expression(&if_expr.else_expr)?);

                Some(Expression::Conditional {
                    cond,
                    then_expr,
                    else_expr,
                })
            }
            hir::HirExpression::Match(match_expr) => {
                // Convert match-expression to nested conditionals
                // match x { 1 => a, 2 => b, _ => c } becomes: (x == 1) ? a : ((x == 2) ? b : c)
                self.convert_match_to_conditionals(&match_expr.expr, &match_expr.arms)
            }
            hir::HirExpression::Cast(cast_expr) => {
                // Convert type cast expression
                // For now, just convert the inner expression
                // Type checking and proper conversion will be handled during codegen
                // TODO: Add explicit type conversion operations in MIR when needed for:
                //   - Integer width changes (truncation/extension)
                //   - Signed/unsigned conversions
                //   - Fixed-point/floating-point conversions
                self.convert_expression(&cast_expr.expr)
            }
            hir::HirExpression::StructLiteral(struct_lit) => {
                // Convert struct literal to packed bit vector
                // Each field value is evaluated and packed into the correct bit positions
                self.convert_struct_literal(&struct_lit.type_name, &struct_lit.fields)
            }
            hir::HirExpression::TupleLiteral(elements) => {
                // Convert tuple literal to an anonymous struct
                // Tuple (a, b, c) becomes struct { _0: typeof(a), _1: typeof(b), _2: typeof(c) }
                // This is then packed into a bit vector like any other struct
                self.convert_tuple_literal(elements)
            }
            hir::HirExpression::ArrayLiteral(elements) => {
                // Convert array literal [a, b, c] to concatenation of elements
                // The elements are concatenated in order: {a, b, c} in SystemVerilog
                self.convert_array_literal(elements)
            }
            hir::HirExpression::Block {
                statements,
                result_expr,
            } => {
                // Convert block expression: { stmt1; stmt2; result_expr }
                // 1. Process all statements in the block (they get added to current statement list)
                // 2. Return the result expression
                for stmt in statements {
                    self.convert_statement(stmt);
                }
                // Convert and return the final expression
                self.convert_expression(result_expr)
            }
        }
    }

    /// Convert struct literal to packed bit vector expression
    fn convert_struct_literal(
        &mut self,
        type_name: &str,
        fields: &[hir::HirStructFieldInit],
    ) -> Option<Expression> {
        // Find the struct type definition to get field layout
        let hir = self.hir?;
        let mut struct_type: Option<&hir::HirStructType> = None;

        // Search for struct type in entities and implementations
        for entity in &hir.entities {
            for port in &entity.ports {
                if let hir::HirType::Struct(ref st) = &port.port_type {
                    if st.name == type_name {
                        struct_type = Some(st);
                        break;
                    }
                }
            }
        }

        if struct_type.is_none() {
            for impl_block in &hir.implementations {
                for signal in &impl_block.signals {
                    if let hir::HirType::Struct(ref st) = &signal.signal_type {
                        if st.name == type_name {
                            struct_type = Some(st);
                            break;
                        }
                    }
                }
            }
        }

        let struct_def = struct_type?;

        // Convert field values and pack them into a bit vector
        // For now, create a binary expression that concatenates the field values
        // In a real implementation, we'd pack these properly based on bit offsets

        // Collect field expressions in order
        let mut field_exprs = Vec::new();
        for struct_field in &struct_def.fields {
            // Find the corresponding field init
            let field_init = fields.iter().find(|f| f.name == struct_field.name)?;
            let field_expr = self.convert_expression(&field_init.value)?;
            field_exprs.push(field_expr);
        }

        // For simplicity, if there's only one field, return that expression
        // For multiple fields, we'd need proper concatenation support in MIR
        // TODO: Implement proper struct packing in MIR
        if field_exprs.len() == 1 {
            Some(field_exprs.into_iter().next().unwrap())
        } else {
            // For now, return the first field as a placeholder
            // This is a limitation that will need proper struct support in MIR
            field_exprs.into_iter().next()
        }
    }

    /// Convert tuple literal to packed bit vector expression
    /// Tuples are lowered to anonymous structs with fields named _0, _1, _2, etc.
    /// The tuple elements are concatenated together into a single packed bit vector.
    fn convert_tuple_literal(&mut self, elements: &[hir::HirExpression]) -> Option<Expression> {
        // Convert each tuple element expression
        let mut element_exprs = Vec::new();
        for element in elements {
            let element_expr = self.convert_expression(element)?;
            element_exprs.push(element_expr);
        }

        // Handle empty tuple
        if element_exprs.is_empty() {
            return None;
        }

        // Single-element tuple - just return the element
        if element_exprs.len() == 1 {
            return Some(element_exprs.into_iter().next().unwrap());
        }

        // Multi-element tuple - concatenate all elements
        // Elements are packed from left to right: (a, b, c) becomes {a, b, c}
        // In bit representation: MSB is 'a', LSB is 'c'
        Some(Expression::Concat(element_exprs))
    }

    /// Convert array literal to concatenation expression
    fn convert_array_literal(&mut self, elements: &[hir::HirExpression]) -> Option<Expression> {
        // Convert each array element expression
        let mut element_exprs = Vec::new();
        for element in elements {
            let element_expr = self.convert_expression(element)?;
            element_exprs.push(element_expr);
        }

        // Handle empty array
        if element_exprs.is_empty() {
            return None;
        }

        // Single-element array - still create a concat to maintain array semantics
        // [x] is different from x in type context
        if element_exprs.len() == 1 {
            return Some(element_exprs.into_iter().next().unwrap());
        }

        // Multi-element array - concatenate all elements
        // Array elements are packed from left to right: [a, b, c] becomes {a, b, c}
        // In SystemVerilog: element 0 is at MSB, element N-1 is at LSB
        Some(Expression::Concat(element_exprs))
    }

    /// Convert match expression to nested conditional expressions
    fn convert_match_to_conditionals(
        &mut self,
        match_value: &hir::HirExpression,
        arms: &[hir::HirMatchArmExpr],
    ) -> Option<Expression> {
        if arms.is_empty() {
            return None;
        }

        // Convert the match value expression ONCE to avoid exponential blowup
        // when we create N comparisons against it
        let match_value_expr = self.convert_expression(match_value)?;

        // Build nested conditionals from right to left
        // Start with the last arm as the default (usually wildcard)
        let mut result = self.convert_expression(&arms.last()?.expr)?;

        // Work backwards through the arms (excluding the last one which is the default)
        for arm in arms[..arms.len() - 1].iter().rev() {
            // Build condition: match_value == pattern
            let condition = match &arm.pattern {
                hir::HirPattern::Literal(lit) => {
                    // Compare match_value with literal
                    // Clone the pre-converted match value instead of re-converting
                    let left = Box::new(match_value_expr.clone());
                    let right = Box::new(Expression::Literal(self.convert_literal(lit)?));
                    Some(Expression::Binary {
                        op: BinaryOp::Equal,
                        left,
                        right,
                    })
                }
                hir::HirPattern::Wildcard => {
                    // Wildcard always matches - shouldn't appear except as last arm
                    // Skip it
                    None
                }
                _ => {
                    // For other patterns, we'll need more complex logic
                    // For now, skip them
                    None
                }
            };

            // Skip if we couldn't build a condition
            let condition = match condition {
                Some(c) => c,
                None => continue,
            };

            // Apply guard if present
            let final_condition = if let Some(guard) = &arm.guard {
                let guard_expr = Box::new(self.convert_expression(guard)?);
                let cond_expr = Box::new(condition);
                Expression::Binary {
                    op: BinaryOp::LogicalAnd,
                    left: cond_expr,
                    right: guard_expr,
                }
            } else {
                condition
            };

            // Build conditional: (condition) ? arm_expr : rest
            result = Expression::Conditional {
                cond: Box::new(final_condition),
                then_expr: Box::new(self.convert_expression(&arm.expr)?),
                else_expr: Box::new(result),
            };
        }

        Some(result)
    }

    /// Convert literal expression (for initial values)
    fn convert_literal_expr(&mut self, expr: &hir::HirExpression) -> Option<Value> {
        if let hir::HirExpression::Literal(lit) = expr {
            self.convert_literal(lit)
        } else {
            None
        }
    }

    /// Convert HIR literal to MIR value
    fn convert_literal(&mut self, lit: &hir::HirLiteral) -> Option<Value> {
        match lit {
            hir::HirLiteral::Integer(val) => Some(Value::Integer(*val as i64)),
            hir::HirLiteral::Boolean(b) => Some(Value::Integer(if *b { 1 } else { 0 })),
            hir::HirLiteral::Float(f) => Some(Value::Float(*f)),
            hir::HirLiteral::String(s) => Some(Value::String(s.clone())),
            hir::HirLiteral::BitVector(bits) => {
                // Convert vector of bools to integer value
                let mut value = 0u64;
                for (i, &bit) in bits.iter().enumerate() {
                    if bit {
                        value |= 1u64 << i;
                    }
                }
                Some(Value::BitVector {
                    width: bits.len(),
                    value,
                })
            }
        }
    }

    /// Convert const value to MIR expression
    fn const_value_to_mir_expression(&self, const_value: &ConstValue) -> Expression {
        match const_value {
            ConstValue::Nat(n) => Expression::Literal(Value::Integer(*n as i64)),
            ConstValue::Int(i) => Expression::Literal(Value::Integer(*i)),
            ConstValue::Bool(b) => Expression::Literal(Value::Integer(if *b { 1 } else { 0 })),
            ConstValue::Float(f) => Expression::Literal(Value::Float(*f)),
            ConstValue::String(s) => Expression::Literal(Value::String(s.clone())),
            // For complex types that don't have direct MIR equivalents, return 0 as fallback
            ConstValue::FloatFormat(_) | ConstValue::Struct(_) => {
                Expression::Literal(Value::Integer(0))
            }
        }
    }

    /// Helper to convert expression to lvalue (for index/range operations)
    fn expr_to_lvalue(&mut self, expr: &hir::HirExpression) -> Option<LValue> {
        match expr {
            hir::HirExpression::Signal(id) => self.signal_map.get(id).map(|&id| LValue::Signal(id)),
            hir::HirExpression::Port(id) => self.port_map.get(id).map(|&id| LValue::Port(id)),
            hir::HirExpression::Variable(id) => {
                self.variable_map.get(id).map(|&id| LValue::Variable(id))
            }
            _ => None,
        }
    }

    /// Convert HIR binary op to MIR
    /// Infer HIR expression type
    fn infer_hir_type(&self, expr: &hir::HirExpression) -> Option<hir::HirType> {
        let hir = self.hir?;
        match expr {
            hir::HirExpression::Literal(lit) => match lit {
                hir::HirLiteral::Integer(_) => Some(hir::HirType::Bit(32)), // Default integer width
                hir::HirLiteral::Boolean(_) => Some(hir::HirType::Bool),
                hir::HirLiteral::Float(_) => Some(hir::HirType::Float32), // Default float type
                hir::HirLiteral::String(_) => None,
                hir::HirLiteral::BitVector(bits) => Some(hir::HirType::Bit(bits.len() as u32)),
            },
            hir::HirExpression::Signal(signal_id) => {
                // Look up signal in current entity's implementation
                let entity_id = self.current_entity_id?;
                let impl_block = hir.implementations.iter().find(|i| i.entity == entity_id)?;
                let signal = impl_block.signals.iter().find(|s| s.id == *signal_id)?;
                Some(signal.signal_type.clone())
            }
            hir::HirExpression::Port(port_id) => {
                // Look up port in current entity
                let entity_id = self.current_entity_id?;
                let entity = hir.entities.iter().find(|e| e.id == entity_id)?;
                let port = entity.ports.iter().find(|p| p.id == *port_id)?;
                Some(port.port_type.clone())
            }
            hir::HirExpression::Variable(var_id) => {
                // Look up variable in current entity's implementation
                let entity_id = self.current_entity_id?;
                let impl_block = hir.implementations.iter().find(|i| i.entity == entity_id)?;
                let var = impl_block.variables.iter().find(|v| v.id == *var_id)?;
                Some(var.var_type.clone())
            }
            hir::HirExpression::Binary(binary) => {
                // For binary expressions, infer from operands
                self.infer_hir_type(&binary.left)
            }
            hir::HirExpression::FieldAccess { base, field } => {
                // Infer from base expression and field
                let base_type = self.infer_hir_type(base)?;

                // BUG FIX #46: Handle Custom("vec2/3/4") which should be Vec2/3/4(Float32)
                // This is needed because Bug #45 causes HIR to store vec2<fp32> as Custom("vec2")
                if let hir::HirType::Custom(type_name) = &base_type {
                    if type_name.starts_with("vec")
                        && matches!(field.as_str(), "x" | "y" | "z" | "w")
                    {
                        // Vec components are Float32 by default (matching convert_type workaround)
                        return Some(hir::HirType::Float32);
                    }
                }

                // For vector types, return element type
                match base_type {
                    hir::HirType::Vec2(element_type)
                    | hir::HirType::Vec3(element_type)
                    | hir::HirType::Vec4(element_type) => {
                        // Vector component access returns element type
                        Some(*element_type)
                    }
                    hir::HirType::Struct(ref struct_type) => {
                        // Look up field type in struct definition
                        for struct_field in &struct_type.fields {
                            if struct_field.name == *field {
                                return Some(struct_field.field_type.clone());
                            }
                        }
                        None
                    }
                    _ => None,
                }
            }
            hir::HirExpression::Index(base, _) => {
                // For array indexing, infer element type
                let base_type = self.infer_hir_type(base)?;
                match base_type {
                    hir::HirType::Array(elem_type, _) => Some(*elem_type),
                    _ => Some(hir::HirType::Bit(1)), // Bit select
                }
            }
            _ => None,
        }
    }

    /// Check if HIR type is a floating-point type
    fn is_float_type(&self, hir_type: &hir::HirType) -> bool {
        matches!(
            hir_type,
            hir::HirType::Float16 | hir::HirType::Float32 | hir::HirType::Float64
        )
    }

    fn convert_binary_op(&self, op: &hir::HirBinaryOp, left_expr: &hir::HirExpression) -> BinaryOp {
        // Infer type from left operand to determine if we need FP operators
        let is_fp = if let Some(ty) = self.infer_hir_type(left_expr) {
            self.is_float_type(&ty)
        } else {
            false
        };

        match op {
            hir::HirBinaryOp::Add => {
                if is_fp {
                    BinaryOp::FAdd
                } else {
                    BinaryOp::Add
                }
            }
            hir::HirBinaryOp::Sub => {
                if is_fp {
                    BinaryOp::FSub
                } else {
                    BinaryOp::Sub
                }
            }
            hir::HirBinaryOp::Mul => {
                if is_fp {
                    BinaryOp::FMul
                } else {
                    BinaryOp::Mul
                }
            }
            hir::HirBinaryOp::Div => {
                if is_fp {
                    BinaryOp::FDiv
                } else {
                    BinaryOp::Div
                }
            }
            hir::HirBinaryOp::Mod => BinaryOp::Mod, // No FP modulo
            hir::HirBinaryOp::And => BinaryOp::BitwiseAnd,
            hir::HirBinaryOp::Or => BinaryOp::BitwiseOr,
            hir::HirBinaryOp::Xor => BinaryOp::BitwiseXor,
            hir::HirBinaryOp::Equal => {
                if is_fp {
                    BinaryOp::FEqual
                } else {
                    BinaryOp::Equal
                }
            }
            hir::HirBinaryOp::NotEqual => {
                if is_fp {
                    BinaryOp::FNotEqual
                } else {
                    BinaryOp::NotEqual
                }
            }
            hir::HirBinaryOp::Less => {
                if is_fp {
                    BinaryOp::FLess
                } else {
                    BinaryOp::Less
                }
            }
            hir::HirBinaryOp::LessEqual => {
                if is_fp {
                    BinaryOp::FLessEqual
                } else {
                    BinaryOp::LessEqual
                }
            }
            hir::HirBinaryOp::Greater => {
                if is_fp {
                    BinaryOp::FGreater
                } else {
                    BinaryOp::Greater
                }
            }
            hir::HirBinaryOp::GreaterEqual => {
                if is_fp {
                    BinaryOp::FGreaterEqual
                } else {
                    BinaryOp::GreaterEqual
                }
            }
            hir::HirBinaryOp::LogicalAnd => BinaryOp::LogicalAnd,
            hir::HirBinaryOp::LogicalOr => BinaryOp::LogicalOr,
            hir::HirBinaryOp::LeftShift => BinaryOp::LeftShift,
            hir::HirBinaryOp::RightShift => BinaryOp::RightShift,
        }
    }

    /// Convert HIR unary op to MIR
    fn convert_unary_op(&self, op: &hir::HirUnaryOp) -> UnaryOp {
        match op {
            hir::HirUnaryOp::Not => UnaryOp::Not,
            hir::HirUnaryOp::BitwiseNot => UnaryOp::BitwiseNot,
            hir::HirUnaryOp::Negate => UnaryOp::Negate,
            hir::HirUnaryOp::AndReduce => UnaryOp::Reduce(ReduceOp::And),
            hir::HirUnaryOp::OrReduce => UnaryOp::Reduce(ReduceOp::Or),
            hir::HirUnaryOp::XorReduce => UnaryOp::Reduce(ReduceOp::Xor),
        }
    }

    /// Convert HIR type to MIR data type
    fn convert_type(&mut self, hir_type: &hir::HirType) -> DataType {
        match hir_type {
            hir::HirType::Bit(width) => DataType::Bit(*width as usize),
            hir::HirType::Bool => DataType::Bool,
            hir::HirType::Logic(width) => DataType::Logic(*width as usize),
            hir::HirType::Int(width) => DataType::Int(*width as usize),
            hir::HirType::Nat(width) => DataType::Nat(*width as usize),
            hir::HirType::Clock(domain) => DataType::Clock {
                domain: domain.map(|id| ClockDomainId(id.0)),
            },
            hir::HirType::Reset {
                polarity,
                clock_domain,
            } => {
                // Convert HIR polarity to MIR active_high boolean
                let active_high = matches!(polarity, hir::HirResetPolarity::ActiveHigh);
                DataType::Reset {
                    active_high,
                    domain: clock_domain.map(|id| ClockDomainId(id.0)),
                }
            }
            hir::HirType::Event => DataType::Event,
            hir::HirType::Stream(inner_type) => {
                // Stream types include implicit handshaking signals
                // For now, convert to the inner type
                // TODO: Add proper stream protocol support in MIR
                self.convert_type(inner_type)
            }
            hir::HirType::Array(inner_type, size) => {
                DataType::Array(Box::new(self.convert_type(inner_type)), *size as usize)
            }
            hir::HirType::Custom(name) => {
                // BUG FIX #45: HIR builder stores vec2<fp32> as Custom("vec2") instead of Vec2(Float32)
                // Recognize built-in vector types before looking up user types
                if name.starts_with("vec") {
                    // Parse "vec2", "vec3", "vec4"
                    // TODO: This loses element type information (e.g., <fp32>)
                    // For now, default to fp32 which is the most common case
                    if let Some(dim_char) = name.chars().nth(3) {
                        if let Some(dimension) = dim_char.to_digit(10) {
                            match dimension {
                                2 => return DataType::Vec2(Box::new(DataType::Float32)),
                                3 => return DataType::Vec3(Box::new(DataType::Float32)),
                                4 => return DataType::Vec4(Box::new(DataType::Float32)),
                                _ => {}
                            }
                        }
                    }
                }

                // Resolve custom types by looking them up in HIR
                if let Some(hir) = self.hir {
                    // First check user-defined types (structs, enums, unions)
                    for user_type in &hir.user_defined_types {
                        if user_type.name == *name {
                            // Found the user-defined type - convert its type_def
                            return self.convert_type(&user_type.type_def);
                        }
                    }

                    // Then check type aliases
                    for type_alias in &hir.type_aliases {
                        if type_alias.name == *name {
                            // Found the type alias - recursively convert its target type
                            // This handles nested type aliases correctly
                            return self.convert_type(&type_alias.target_type);
                        }
                    }
                }
                // If type not found, default to Bit(1) as fallback
                // This maintains backward compatibility for unresolved types
                DataType::Bit(1)
            }
            hir::HirType::Struct(struct_type) => {
                DataType::Struct(Box::new(self.convert_struct_type(struct_type)))
            }
            hir::HirType::Enum(enum_type) => {
                DataType::Enum(Box::new(self.convert_enum_type(enum_type)))
            }
            hir::HirType::Union(union_type) => {
                DataType::Union(Box::new(self.convert_union_type(union_type)))
            }
            // Floating-point types
            hir::HirType::Float16 => DataType::Float16,
            hir::HirType::Float32 => DataType::Float32,
            hir::HirType::Float64 => DataType::Float64,
            // Vector types
            hir::HirType::Vec2(element_type) => {
                DataType::Vec2(Box::new(self.convert_type(element_type)))
            }
            hir::HirType::Vec3(element_type) => {
                DataType::Vec3(Box::new(self.convert_type(element_type)))
            }
            hir::HirType::Vec4(element_type) => {
                DataType::Vec4(Box::new(self.convert_type(element_type)))
            }
            // Parametric types - preserve parameter name and default
            hir::HirType::BitParam(param_name) => DataType::BitParam {
                param: param_name.clone(),
                default: 8,
            },
            hir::HirType::LogicParam(param_name) => DataType::LogicParam {
                param: param_name.clone(),
                default: 8,
            },
            hir::HirType::IntParam(param_name) => DataType::IntParam {
                param: param_name.clone(),
                default: 32,
            },
            hir::HirType::NatParam(param_name) => DataType::NatParam {
                param: param_name.clone(),
                default: 32,
            },
            // Expression-based types - need const evaluation
            hir::HirType::BitExpr(expr) => {
                // Try to evaluate const expression to get concrete width
                match self.try_eval_const_expr(expr) {
                    Some(val) => DataType::Bit(val as usize),
                    None => {
                        // Can't evaluate - convert to MIR expression for codegen
                        DataType::BitExpr {
                            expr: Box::new(self.convert_const_expr_to_mir(expr)),
                            default: 8,
                        }
                    }
                }
            }
            hir::HirType::LogicExpr(expr) => match self.try_eval_const_expr(expr) {
                Some(val) => DataType::Logic(val as usize),
                None => DataType::LogicExpr {
                    expr: Box::new(self.convert_const_expr_to_mir(expr)),
                    default: 8,
                },
            },
            hir::HirType::IntExpr(expr) => match self.try_eval_const_expr(expr) {
                Some(val) => DataType::Int(val as usize),
                None => DataType::IntExpr {
                    expr: Box::new(self.convert_const_expr_to_mir(expr)),
                    default: 32,
                },
            },
            hir::HirType::NatExpr(expr) => match self.try_eval_const_expr(expr) {
                Some(val) => DataType::Nat(val as usize),
                None => DataType::NatExpr {
                    expr: Box::new(self.convert_const_expr_to_mir(expr)),
                    default: 32,
                },
            },
            hir::HirType::ArrayExpr(inner_type, size_expr) => {
                // Evaluate size expression
                match self.try_eval_const_expr(size_expr) {
                    Some(val) => {
                        DataType::Array(Box::new(self.convert_type(inner_type)), val as usize)
                    }
                    None => DataType::Array(Box::new(self.convert_type(inner_type)), 1), // Fallback
                }
            }

            // Tuple types - convert to anonymous packed structs
            hir::HirType::Tuple(element_types) => {
                // Convert (bit[32], bit[8]) to struct __tuple_2 { _0: bit[32], _1: bit[8] }
                let fields: Vec<StructField> = element_types
                    .iter()
                    .enumerate()
                    .map(|(i, ty)| StructField {
                        name: format!("_{}", i),
                        field_type: self.convert_type(ty),
                    })
                    .collect();

                DataType::Struct(Box::new(StructType {
                    name: format!("__tuple_{}", fields.len()),
                    fields,
                    packed: true,
                }))
            }

            // Parametric numeric types - These will be monomorphized later
            hir::HirType::FpParametric { format } => {
                // TODO: Evaluate format expression to get concrete FloatFormat
                // For now, default to Float32
                match self.try_eval_const_expr(format) {
                    Some(_) => DataType::Float32, // Placeholder
                    None => DataType::Float32,
                }
            }
            hir::HirType::FixedParametric {
                width,
                frac,
                signed: _,
            } => {
                // TODO: Evaluate width and frac expressions
                // For now, map to Bit type with evaluated width
                match (
                    self.try_eval_const_expr(width),
                    self.try_eval_const_expr(frac),
                ) {
                    (Some(w), Some(_f)) => DataType::Bit(w as usize),
                    _ => DataType::Bit(32), // Fallback
                }
            }
            hir::HirType::IntParametric { width, signed } => {
                // Evaluate width and signed expressions
                match (
                    self.try_eval_const_expr(width),
                    self.try_eval_const_expr(signed),
                ) {
                    (Some(w), Some(s)) if s != 0 => DataType::Int(w as usize),
                    (Some(w), _) => DataType::Nat(w as usize),
                    _ => DataType::Int(32), // Fallback
                }
            }
            hir::HirType::VecParametric {
                element_type,
                dimension,
            } => {
                // Evaluate dimension expression
                let inner = self.convert_type(element_type);
                match self.try_eval_const_expr(dimension) {
                    Some(2) => DataType::Vec2(Box::new(inner)),
                    Some(3) => DataType::Vec3(Box::new(inner)),
                    Some(4) => DataType::Vec4(Box::new(inner)),
                    Some(n) => DataType::Array(Box::new(inner), n as usize),
                    None => DataType::Vec3(Box::new(inner)), // Fallback
                }
            }
        }
    }

    /// Try to evaluate a const expression at compile time
    /// Returns Some(value) if the expression can be evaluated, None otherwise
    #[allow(clippy::only_used_in_recursion)]
    fn try_eval_const_expr(&mut self, expr: &hir::HirExpression) -> Option<u64> {
        // Try using the const evaluator first (handles both built-in and user-defined functions)
        if let Ok(const_val) = self.const_evaluator.eval(expr) {
            return const_val.as_nat().map(|n| n as u64);
        }

        // Fallback to manual evaluation for expressions the evaluator can't handle
        match expr {
            hir::HirExpression::Literal(hir::HirLiteral::Integer(val)) => Some(*val),
            hir::HirExpression::Binary(bin_expr) => {
                // Recursively evaluate binary expressions
                let left = self.try_eval_const_expr(&bin_expr.left)?;
                let right = self.try_eval_const_expr(&bin_expr.right)?;

                match bin_expr.op {
                    hir::HirBinaryOp::Add => Some(left + right),
                    hir::HirBinaryOp::Sub => Some(left.saturating_sub(right)),
                    hir::HirBinaryOp::Mul => Some(left * right),
                    hir::HirBinaryOp::Div if right != 0 => Some(left / right),
                    hir::HirBinaryOp::Mod if right != 0 => Some(left % right),
                    _ => None, // Can't evaluate other operations
                }
            }
            hir::HirExpression::Call(call_expr) => {
                // Handle intrinsic functions (for backward compatibility)
                match call_expr.function.as_str() {
                    "clog2" => {
                        // Calculate ceiling log2
                        if call_expr.args.len() == 1 {
                            let arg = self.try_eval_const_expr(&call_expr.args[0])?;
                            if arg == 0 {
                                return Some(0);
                            }
                            Some((64 - (arg - 1).leading_zeros()) as u64)
                        } else {
                            None
                        }
                    }
                    "pow2" => {
                        // Calculate 2^n
                        if call_expr.args.len() == 1 {
                            let arg = self.try_eval_const_expr(&call_expr.args[0])?;
                            if arg < 64 {
                                Some(1u64 << arg)
                            } else {
                                None // Overflow
                            }
                        } else {
                            None
                        }
                    }
                    _ => None, // Unknown function - const evaluator already tried
                }
            }
            hir::HirExpression::GenericParam(param_name) => {
                // Look up generic parameter value from current entity
                if let Some(hir) = self.hir {
                    if let Some(entity_id) = self.current_entity_id {
                        for entity in &hir.entities {
                            if entity.id == entity_id {
                                for generic in &entity.generics {
                                    if generic.name == *param_name {
                                        if let Some(default_expr) = &generic.default_value {
                                            return self.try_eval_const_expr(default_expr);
                                        }
                                    }
                                }
                                break;
                            }
                        }
                    }
                }
                None
            }
            _ => None, // Can't evaluate variables, constants, etc. without context
        }
    }

    /// Convert a HIR expression to a MIR expression for use in type positions
    /// This is specifically for const expressions in types (like clog2(SIZE) in nat[clog2(SIZE)])
    #[allow(clippy::only_used_in_recursion)]
    fn convert_const_expr_to_mir(&self, expr: &hir::HirExpression) -> Expression {
        match expr {
            hir::HirExpression::Literal(hir::HirLiteral::Integer(val)) => {
                Expression::Literal(Value::Integer(*val as i64))
            }
            hir::HirExpression::Binary(bin_expr) => {
                let left = Box::new(self.convert_const_expr_to_mir(&bin_expr.left));
                let right = Box::new(self.convert_const_expr_to_mir(&bin_expr.right));
                let op = match bin_expr.op {
                    hir::HirBinaryOp::Add => BinaryOp::Add,
                    hir::HirBinaryOp::Sub => BinaryOp::Sub,
                    hir::HirBinaryOp::Mul => BinaryOp::Mul,
                    hir::HirBinaryOp::Div => BinaryOp::Div,
                    hir::HirBinaryOp::Mod => BinaryOp::Mod,
                    _ => {
                        // For unsupported operators, fall back to a literal 0
                        return Expression::Literal(Value::Integer(0));
                    }
                };
                Expression::Binary { op, left, right }
            }
            hir::HirExpression::Call(call_expr) => {
                // Convert function call (like clog2(SIZE))
                let args = call_expr
                    .args
                    .iter()
                    .map(|arg| self.convert_const_expr_to_mir(arg))
                    .collect();
                Expression::FunctionCall {
                    name: call_expr.function.clone(),
                    args,
                }
            }
            hir::HirExpression::GenericParam(param_name) => {
                // Reference to a generic parameter (like SIZE)
                // We can't resolve this yet, so we keep it as a function call-like reference
                // The codegen will handle emitting the parameter name
                Expression::FunctionCall {
                    name: param_name.clone(),
                    args: vec![],
                }
            }
            _ => {
                // For other expression types, fall back to literal 0
                // This shouldn't normally happen for const type expressions
                Expression::Literal(Value::Integer(0))
            }
        }
    }

    /// Convert HIR port direction
    fn convert_port_direction(&self, dir: &hir::HirPortDirection) -> PortDirection {
        match dir {
            hir::HirPortDirection::Input => PortDirection::Input,
            hir::HirPortDirection::Output => PortDirection::Output,
            hir::HirPortDirection::Bidirectional => PortDirection::InOut,
            hir::HirPortDirection::Protocol => PortDirection::InOut, // Protocols are bidirectional by nature
        }
    }

    /// Convert HIR edge type
    fn convert_edge_type(&self, edge: &hir::HirEdgeType) -> EdgeType {
        match edge {
            hir::HirEdgeType::Rising => EdgeType::Rising,
            hir::HirEdgeType::Falling => EdgeType::Falling,
            hir::HirEdgeType::Both => EdgeType::Both,
            hir::HirEdgeType::Active => EdgeType::Active,
            hir::HirEdgeType::Inactive => EdgeType::Inactive,
        }
    }

    // ID generation methods
    /// Convert HIR generic type to MIR generic parameter type
    fn convert_generic_type(&mut self, hir_type: &hir::HirGenericType) -> GenericParameterType {
        match hir_type {
            hir::HirGenericType::Type => GenericParameterType::Type,
            hir::HirGenericType::Const(hir_data_type) => {
                GenericParameterType::Const(self.convert_type(hir_data_type))
            }
            hir::HirGenericType::Width => GenericParameterType::Width,
            hir::HirGenericType::ClockDomain => GenericParameterType::ClockDomain,
            hir::HirGenericType::Intent => GenericParameterType::Intent,
        }
    }

    /// Convert HIR struct type to MIR struct type
    fn convert_struct_type(&mut self, hir_struct: &hir::HirStructType) -> StructType {
        let fields = hir_struct
            .fields
            .iter()
            .map(|field| StructField {
                name: field.name.clone(),
                field_type: self.convert_type(&field.field_type),
            })
            .collect();

        StructType {
            name: hir_struct.name.clone(),
            fields,
            packed: hir_struct.packed,
        }
    }

    /// Convert HIR enum type to MIR enum type
    fn convert_enum_type(&mut self, hir_enum: &hir::HirEnumType) -> EnumType {
        let variants = hir_enum
            .variants
            .iter()
            .map(|variant| EnumVariant {
                name: variant.name.clone(),
                value: variant
                    .value
                    .as_ref()
                    .and_then(|expr| self.convert_literal_expr(expr)),
            })
            .collect();

        EnumType {
            name: hir_enum.name.clone(),
            variants,
            base_type: self.convert_type(&hir_enum.base_type),
        }
    }

    /// Convert HIR union type to MIR union type
    fn convert_union_type(&mut self, hir_union: &hir::HirUnionType) -> UnionType {
        let fields = hir_union
            .fields
            .iter()
            .map(|field| StructField {
                name: field.name.clone(),
                field_type: self.convert_type(&field.field_type),
            })
            .collect();

        UnionType {
            name: hir_union.name.clone(),
            fields,
            packed: hir_union.packed,
        }
    }

    /// Convert struct/vector field access to bit slice
    fn convert_field_access(
        &mut self,
        base: &hir::HirExpression,
        field_name: &str,
    ) -> Option<Expression> {
        // Map numeric field names (tuple indices) to struct field names
        // e.g., "0" -> "_0", "1" -> "_1", "2" -> "_2"
        let normalized_field_name = if field_name.chars().all(|c| c.is_ascii_digit()) {
            format!("_{}", field_name)
        } else {
            field_name.to_string()
        };

        // Build the complete field path from nested accesses
        let mut field_path = vec![normalized_field_name.clone()];
        let mut current_base = base;

        // Walk up the chain to find the root signal/port/variable
        loop {
            match current_base {
                hir::HirExpression::FieldAccess {
                    base: inner_base,
                    field: inner_field,
                } => {
                    // Normalize inner field names too (for nested tuple access)
                    let normalized_inner = if inner_field.chars().all(|c| c.is_ascii_digit()) {
                        format!("_{}", inner_field)
                    } else {
                        inner_field.clone()
                    };
                    field_path.insert(0, normalized_inner);
                    current_base = inner_base.as_ref();
                }
                hir::HirExpression::Signal(sig_id) => {
                    // Check if this signal was flattened
                    if let Some(flattened) = self.flattened_signals.get(sig_id) {
                        // Find the flattened field with matching path
                        for flat_field in flattened {
                            if flat_field.field_path == field_path {
                                return Some(Expression::Ref(LValue::Signal(SignalId(
                                    flat_field.id,
                                ))));
                            }
                        }
                    }
                    // Not flattened - use mapped signal ID (or fall back to bit range)
                    if let Some(signal_id) = self.signal_map.get(sig_id) {
                        // Fall back to bit range approach
                        let base_lval = LValue::Signal(*signal_id);
                        let (high_bit, low_bit) =
                            self.get_field_bit_range(base, &normalized_field_name)?;
                        let high_expr = Expression::Literal(Value::Integer(high_bit as i64));
                        let low_expr = Expression::Literal(Value::Integer(low_bit as i64));
                        return Some(Expression::Ref(LValue::RangeSelect {
                            base: Box::new(base_lval),
                            high: Box::new(high_expr),
                            low: Box::new(low_expr),
                        }));
                    }
                    return None;
                }
                hir::HirExpression::Port(port_id) => {
                    // Check if this port was flattened
                    if let Some(flattened) = self.flattened_ports.get(port_id) {
                        // Find the flattened field with matching path
                        for flat_field in flattened {
                            if flat_field.field_path == field_path {
                                return Some(Expression::Ref(LValue::Port(PortId(flat_field.id))));
                            }
                        }
                    }
                    // Not flattened - use mapped port ID (or fall back to bit range)
                    if let Some(&port_id_mir) = self.port_map.get(port_id) {
                        // Fall back to bit range approach
                        let base_lval = LValue::Port(port_id_mir);
                        let (high_bit, low_bit) =
                            self.get_field_bit_range(base, &normalized_field_name)?;
                        let high_expr = Expression::Literal(Value::Integer(high_bit as i64));
                        let low_expr = Expression::Literal(Value::Integer(low_bit as i64));
                        return Some(Expression::Ref(LValue::RangeSelect {
                            base: Box::new(base_lval),
                            high: Box::new(high_expr),
                            low: Box::new(low_expr),
                        }));
                    }
                    return None;
                }
                hir::HirExpression::Variable(var_id) => {
                    // Variables don't get flattened the same way, use bit range approach
                    if let Some(var_id_mir) = self.variable_map.get(var_id) {
                        let base_lval = LValue::Variable(*var_id_mir);
                        let (high_bit, low_bit) =
                            self.get_field_bit_range(base, &normalized_field_name)?;
                        let high_expr = Expression::Literal(Value::Integer(high_bit as i64));
                        let low_expr = Expression::Literal(Value::Integer(low_bit as i64));
                        return Some(Expression::Ref(LValue::RangeSelect {
                            base: Box::new(base_lval),
                            high: Box::new(high_expr),
                            low: Box::new(low_expr),
                        }));
                    }
                    return None;
                }
                _ => {
                    // Complex base - can't handle
                    return None;
                }
            }
        }
    }

    /// Get the bit range for a struct field or vector component
    fn get_field_bit_range(
        &mut self,
        base: &hir::HirExpression,
        field_name: &str,
    ) -> Option<(usize, usize)> {
        // First, check if this is a vector type
        if let Some(base_type) = self.infer_hir_type(base) {
            // Handle Custom types that might be vec2/vec3/vec4
            // BUG FIX #44: HIR stores vec2<fp32> as Custom("vec2") instead of Vec2(Float32)
            if let hir::HirType::Custom(type_name) = &base_type {
                // Parse custom type names like "vec2", "vec3", "vec4"
                if type_name.starts_with("vec") {
                    // Extract the dimension (2, 3, or 4)
                    if let Some(dim_char) = type_name.chars().nth(3) {
                        if let Some(dimension) = dim_char.to_digit(10) {
                            // Default to fp32 element width (32 bits) for custom vec types
                            // TODO: Parse element type from full type string if available
                            let elem_width = 32;
                            match (dimension, field_name) {
                                (2, "x") => return Some((elem_width - 1, 0)),
                                (2, "y") => return Some((2 * elem_width - 1, elem_width)),
                                (3, "x") => return Some((elem_width - 1, 0)),
                                (3, "y") => return Some((2 * elem_width - 1, elem_width)),
                                (3, "z") => return Some((3 * elem_width - 1, 2 * elem_width)),
                                (4, "x") => return Some((elem_width - 1, 0)),
                                (4, "y") => return Some((2 * elem_width - 1, elem_width)),
                                (4, "z") => return Some((3 * elem_width - 1, 2 * elem_width)),
                                (4, "w") => return Some((4 * elem_width - 1, 3 * elem_width)),
                                _ => return None,
                            }
                        }
                    }
                }
            }

            match base_type {
                hir::HirType::Vec2(ref element_type) => {
                    let elem_width = self.get_hir_type_width(element_type);
                    match field_name {
                        "x" => return Some((elem_width - 1, 0)),
                        "y" => return Some((2 * elem_width - 1, elem_width)),
                        _ => return None, // Invalid component for vec2
                    }
                }
                hir::HirType::Vec3(ref element_type) => {
                    let elem_width = self.get_hir_type_width(element_type);
                    match field_name {
                        "x" => return Some((elem_width - 1, 0)),
                        "y" => return Some((2 * elem_width - 1, elem_width)),
                        "z" => return Some((3 * elem_width - 1, 2 * elem_width)),
                        _ => return None, // Invalid component for vec3
                    }
                }
                hir::HirType::Vec4(ref element_type) => {
                    let elem_width = self.get_hir_type_width(element_type);
                    match field_name {
                        "x" => return Some((elem_width - 1, 0)),
                        "y" => return Some((2 * elem_width - 1, elem_width)),
                        "z" => return Some((3 * elem_width - 1, 2 * elem_width)),
                        "w" => return Some((4 * elem_width - 1, 3 * elem_width)),
                        _ => return None, // Invalid component for vec4
                    }
                }
                _ => {
                    // Not a vector type, fall through to struct handling
                }
            }
        }

        // Get the struct type from the base expression
        let struct_type = self.get_expression_struct_type(base)?;

        // Clone the fields to avoid borrow checker issues
        let fields: Vec<_> = struct_type
            .fields
            .iter()
            .map(|f| (f.name.clone(), f.field_type.clone()))
            .collect();

        // Calculate field offset
        let mut current_offset = 0;
        for (field_name_in_struct, field_type) in fields {
            let field_width = self.get_hir_type_width(&field_type);
            if field_name_in_struct == field_name {
                // Found the field - return its bit range
                let high_bit = current_offset + field_width - 1;
                let low_bit = current_offset;
                return Some((high_bit, low_bit));
            }
            current_offset += field_width;
        }
        None // Field not found
    }

    /// Get the struct type of an expression
    fn get_expression_struct_type(&self, expr: &hir::HirExpression) -> Option<&hir::HirStructType> {
        let hir = self.hir?;

        match expr {
            hir::HirExpression::Signal(id) => {
                // Find the signal in implementations
                for impl_block in &hir.implementations {
                    for signal in &impl_block.signals {
                        if signal.id == *id {
                            match &signal.signal_type {
                                hir::HirType::Struct(ref struct_type) => {
                                    return Some(struct_type);
                                }
                                hir::HirType::Tuple(element_types) => {
                                    // Convert tuple to anonymous struct for field access
                                    let fields: Vec<hir::HirStructField> = element_types
                                        .iter()
                                        .enumerate()
                                        .map(|(i, ty)| hir::HirStructField {
                                            name: format!("_{}", i),
                                            field_type: ty.clone(),
                                        })
                                        .collect();

                                    let temp_struct = Box::leak(Box::new(hir::HirStructType {
                                        name: format!("__tuple_{}", fields.len()),
                                        fields,
                                        packed: true,
                                    }));
                                    return Some(temp_struct);
                                }
                                _ => {}
                            }
                        }
                    }
                }

                // If not found as signal, check if it's actually a port
                for entity in &hir.entities {
                    for port in &entity.ports {
                        // Compare based on the numeric ID (this is a heuristic)
                        if port.id.0 == id.0 {
                            match &port.port_type {
                                hir::HirType::Struct(ref struct_type) => {
                                    return Some(struct_type);
                                }
                                hir::HirType::Tuple(element_types) => {
                                    // Convert tuple to anonymous struct for field access
                                    let fields: Vec<hir::HirStructField> = element_types
                                        .iter()
                                        .enumerate()
                                        .map(|(i, ty)| hir::HirStructField {
                                            name: format!("_{}", i),
                                            field_type: ty.clone(),
                                        })
                                        .collect();

                                    let temp_struct = Box::leak(Box::new(hir::HirStructType {
                                        name: format!("__tuple_{}", fields.len()),
                                        fields,
                                        packed: true,
                                    }));
                                    return Some(temp_struct);
                                }
                                _ => {}
                            }
                        }
                    }
                }
            }
            // Note: Port access will be added when we understand how ports are referenced
            hir::HirExpression::Variable(id) => {
                // First check dynamic_variables (for let-bound variables)
                if let Some((_mir_id, _name, var_type)) = self.dynamic_variables.get(id) {
                    match var_type {
                        hir::HirType::Struct(ref struct_type) => {
                            return Some(struct_type);
                        }
                        hir::HirType::Tuple(element_types) => {
                            // Convert tuple to anonymous struct for field access
                            let fields: Vec<hir::HirStructField> = element_types
                                .iter()
                                .enumerate()
                                .map(|(i, ty)| hir::HirStructField {
                                    name: format!("_{}", i),
                                    field_type: ty.clone(),
                                })
                                .collect();

                            let temp_struct = Box::leak(Box::new(hir::HirStructType {
                                name: format!("__tuple_{}", fields.len()),
                                fields,
                                packed: true,
                            }));
                            return Some(temp_struct);
                        }
                        _ => {}
                    }
                }

                // Otherwise check HIR impl block variables
                for impl_block in &hir.implementations {
                    for variable in &impl_block.variables {
                        if variable.id == *id {
                            // Check both Struct and Tuple types
                            match &variable.var_type {
                                hir::HirType::Struct(ref struct_type) => {
                                    return Some(struct_type);
                                }
                                hir::HirType::Tuple(element_types) => {
                                    // Convert tuple to anonymous struct for field access
                                    let fields: Vec<hir::HirStructField> = element_types
                                        .iter()
                                        .enumerate()
                                        .map(|(i, ty)| hir::HirStructField {
                                            name: format!("_{}", i),
                                            field_type: ty.clone(),
                                        })
                                        .collect();

                                    let temp_struct = Box::leak(Box::new(hir::HirStructType {
                                        name: format!("__tuple_{}", fields.len()),
                                        fields,
                                        packed: true,
                                    }));
                                    return Some(temp_struct);
                                }
                                _ => {}
                            }
                        }
                    }
                }
            }
            _ => {}
        }
        None
    }

    /// Get the width in bits of a HIR type
    #[allow(clippy::only_used_in_recursion)]
    fn get_hir_type_width(&mut self, hir_type: &hir::HirType) -> usize {
        match hir_type {
            hir::HirType::Bit(width) => *width as usize,
            hir::HirType::Bool => 1, // Boolean is represented as 1 bit in hardware
            hir::HirType::Logic(width) => *width as usize,
            hir::HirType::Int(width) => *width as usize,
            hir::HirType::Nat(width) => *width as usize,
            hir::HirType::Clock(_) => 1,
            hir::HirType::Reset { .. } => 1,
            hir::HirType::Event => 0,
            hir::HirType::Struct(struct_type) => {
                let mut total_width = 0;
                for field in &struct_type.fields {
                    total_width += self.get_hir_type_width(&field.field_type);
                }
                total_width
            }
            hir::HirType::Enum(enum_type) => self.get_hir_type_width(&enum_type.base_type),
            hir::HirType::Union(union_type) => {
                // Union width is the maximum of all field widths
                union_type
                    .fields
                    .iter()
                    .map(|field| self.get_hir_type_width(&field.field_type))
                    .max()
                    .unwrap_or(0)
            }
            hir::HirType::Array(element_type, size) => {
                self.get_hir_type_width(element_type) * (*size as usize)
            }
            hir::HirType::Stream(element_type) => {
                // Stream types don't have a defined width in hardware
                self.get_hir_type_width(element_type)
            }
            hir::HirType::Custom(_) => {
                // Custom types default to 32 bits
                32
            }
            // Parametric types - use default widths
            hir::HirType::BitParam(_) => 8,   // Default bit width
            hir::HirType::LogicParam(_) => 8, // Default logic width
            hir::HirType::IntParam(_) => 32,  // Default int width
            hir::HirType::NatParam(_) => 32,  // Default nat width
            // Expression-based types - evaluate or use default
            hir::HirType::BitExpr(expr) => self.try_eval_const_expr(expr).unwrap_or(8) as usize,
            hir::HirType::LogicExpr(expr) => self.try_eval_const_expr(expr).unwrap_or(8) as usize,
            hir::HirType::IntExpr(expr) => self.try_eval_const_expr(expr).unwrap_or(32) as usize,
            hir::HirType::NatExpr(expr) => self.try_eval_const_expr(expr).unwrap_or(32) as usize,
            hir::HirType::ArrayExpr(element_type, size_expr) => {
                let element_width = self.get_hir_type_width(element_type);
                let size = self.try_eval_const_expr(size_expr).unwrap_or(1) as usize;
                element_width * size
            }
            // Floating-point types have fixed widths
            hir::HirType::Float16 => 16,
            hir::HirType::Float32 => 32,
            hir::HirType::Float64 => 64,
            // Vector types - width is element_width * component_count
            hir::HirType::Vec2(element_type) => self.get_hir_type_width(element_type) * 2,
            hir::HirType::Vec3(element_type) => self.get_hir_type_width(element_type) * 3,
            hir::HirType::Vec4(element_type) => self.get_hir_type_width(element_type) * 4,

            // Parametric numeric types
            hir::HirType::FpParametric { format } => {
                // TODO: Evaluate format to get concrete width
                // For now, default to Float32 width
                self.try_eval_const_expr(format).unwrap_or(32) as usize
            }
            hir::HirType::FixedParametric {
                width,
                frac: _,
                signed: _,
            } => {
                // Width is the total width of the fixed-point number
                self.try_eval_const_expr(width).unwrap_or(32) as usize
            }
            hir::HirType::IntParametric { width, signed: _ } => {
                // Width is the total width of the integer
                self.try_eval_const_expr(width).unwrap_or(32) as usize
            }
            hir::HirType::VecParametric {
                element_type,
                dimension,
            } => {
                let elem_width = self.get_hir_type_width(element_type);
                let dim = self.try_eval_const_expr(dimension).unwrap_or(3) as usize;
                elem_width * dim
            }
            hir::HirType::Tuple(element_types) => {
                // Tuple width is the sum of all element widths (packed representation)
                element_types
                    .iter()
                    .map(|ty| self.get_hir_type_width(ty))
                    .sum()
            }
        }
    }

    /// Resolve enum variant to its integer value
    fn resolve_enum_variant_value(&self, enum_type: &str, variant: &str) -> Option<Expression> {
        // Find the enum type in the HIR
        let hir = self.hir?;

        // Look through all entities and implementations to find the enum type
        for entity in &hir.entities {
            for port in &entity.ports {
                if let hir::HirType::Enum(ref enum_def) = &port.port_type {
                    if enum_def.name == enum_type {
                        return self.find_variant_value(enum_def, variant);
                    }
                }
            }
        }

        for impl_block in &hir.implementations {
            for signal in &impl_block.signals {
                if let hir::HirType::Enum(ref enum_def) = &signal.signal_type {
                    if enum_def.name == enum_type {
                        return self.find_variant_value(enum_def, variant);
                    }
                }
            }
        }

        // If not found, return default value 0
        Some(Expression::Literal(Value::Integer(0)))
    }

    /// Find the value of a specific variant in an enum
    fn find_variant_value(&self, enum_def: &hir::HirEnumType, variant: &str) -> Option<Expression> {
        for (index, enum_variant) in enum_def.variants.iter().enumerate() {
            if enum_variant.name == variant {
                // If the variant has an explicit value, use it
                if let Some(ref value_expr) = enum_variant.value {
                    if let Some(value) = self.convert_literal_expr_immutable(value_expr) {
                        return Some(Expression::Literal(value));
                    }
                }
                // Otherwise, use the index
                return Some(Expression::Literal(Value::Integer(index as i64)));
            }
        }
        // Variant not found, return 0
        Some(Expression::Literal(Value::Integer(0)))
    }

    /// Convert literal expression (immutable version)
    fn convert_literal_expr_immutable(&self, expr: &hir::HirExpression) -> Option<Value> {
        if let hir::HirExpression::Literal(lit) = expr {
            self.convert_literal_immutable(lit)
        } else {
            None
        }
    }

    /// Convert literal (immutable version)
    fn convert_literal_immutable(&self, lit: &hir::HirLiteral) -> Option<Value> {
        match lit {
            hir::HirLiteral::Integer(val) => Some(Value::Integer(*val as i64)),
            hir::HirLiteral::Boolean(b) => Some(Value::Integer(if *b { 1 } else { 0 })),
            hir::HirLiteral::Float(f) => Some(Value::Float(*f)),
            hir::HirLiteral::String(s) => Some(Value::String(s.clone())),
            hir::HirLiteral::BitVector(bits) => {
                // Convert bit vector to integer
                let mut value = 0i64;
                for (i, bit) in bits.iter().enumerate() {
                    if *bit {
                        value |= 1 << i;
                    }
                }
                Some(Value::Integer(value))
            }
        }
    }

    fn next_module_id(&mut self) -> ModuleId {
        let id = ModuleId(self.next_module_id);
        self.next_module_id += 1;
        id
    }

    fn next_port_id(&mut self) -> PortId {
        let id = PortId(self.next_port_id);
        self.next_port_id += 1;
        id
    }

    fn next_signal_id(&mut self) -> SignalId {
        let id = SignalId(self.next_signal_id);
        self.next_signal_id += 1;
        id
    }

    fn next_variable_id(&mut self) -> VariableId {
        let id = VariableId(self.next_variable_id);
        self.next_variable_id += 1;
        id
    }

    fn next_process_id(&mut self) -> ProcessId {
        let id = ProcessId(self.next_process_id);
        self.next_process_id += 1;
        id
    }

    /// Try to synthesis-resolve a complex if-else-if chain
    /// Returns Some(ResolvedConditional) if this is a complex conditional assignment
    /// Returns None if this should remain as a regular if statement
    fn try_synthesis_resolve_if(
        &mut self,
        if_stmt: &hir::HirIfStatement,
    ) -> Option<ResolvedConditional> {
        // Only apply synthesis resolution to complex if-else-if chains, not simple if-else patterns
        if !self.is_complex_if_else_if_chain(if_stmt) {
            return None;
        }

        // Check if this is a conditional assignment pattern suitable for synthesis resolution
        if let Some((target, kind)) = self.extract_conditional_assignment_target(if_stmt) {
            // This is a conditional assignment - resolve it using synthesis approach
            let condition = self.convert_expression(&if_stmt.condition)?;
            let original_if = IfStatement {
                condition: condition.clone(),
                then_block: self.convert_statements(&if_stmt.then_statements),
                else_block: if_stmt
                    .else_statements
                    .as_ref()
                    .map(|stmts| self.convert_statements(stmts)),
            };

            // Build priority mux from if-else-if chain
            let resolved = self.build_priority_mux_from_hir(if_stmt)?;

            Some(ResolvedConditional {
                target,
                kind,
                original: Box::new(original_if),
                resolved,
            })
        } else {
            None
        }
    }

    /// Extract the target signal and assignment kind from a conditional assignment pattern
    /// Returns Some((target, kind)) if all branches assign to the same signal
    fn extract_conditional_assignment_target(
        &mut self,
        if_stmt: &hir::HirIfStatement,
    ) -> Option<(LValue, AssignmentKind)> {
        // Check if then branch has exactly one assignment
        if let Some((then_target, then_kind)) =
            self.extract_single_assignment(&if_stmt.then_statements)
        {
            // Check else branch
            if let Some(else_stmts) = &if_stmt.else_statements {
                if let Some((else_target, else_kind)) =
                    self.extract_assignment_or_nested_if(else_stmts)
                {
                    // Both branches must assign to the same target with the same kind
                    if self.lvalues_match(&then_target, &else_target) && then_kind == else_kind {
                        return Some((then_target, then_kind));
                    }
                }
            }
        }
        None
    }

    /// Extract assignment from a single statement list or nested if
    fn extract_assignment_or_nested_if(
        &mut self,
        stmts: &[hir::HirStatement],
    ) -> Option<(LValue, AssignmentKind)> {
        if stmts.len() == 1 {
            match &stmts[0] {
                hir::HirStatement::Assignment(assign) => {
                    let target = self.convert_lvalue(&assign.lhs)?;
                    let kind = match assign.assignment_type {
                        hir::HirAssignmentType::NonBlocking => AssignmentKind::NonBlocking,
                        hir::HirAssignmentType::Blocking => AssignmentKind::Blocking,
                        hir::HirAssignmentType::Combinational => AssignmentKind::Blocking,
                    };
                    Some((target, kind))
                }
                hir::HirStatement::If(nested_if) => {
                    // Recursively check nested if
                    self.extract_conditional_assignment_target(nested_if)
                }
                _ => None,
            }
        } else {
            None
        }
    }

    /// Extract single assignment from statement list
    fn extract_single_assignment(
        &mut self,
        stmts: &[hir::HirStatement],
    ) -> Option<(LValue, AssignmentKind)> {
        if stmts.len() == 1 {
            if let hir::HirStatement::Assignment(assign) = &stmts[0] {
                let target = self.convert_lvalue(&assign.lhs)?;
                let kind = match assign.assignment_type {
                    hir::HirAssignmentType::NonBlocking => AssignmentKind::NonBlocking,
                    hir::HirAssignmentType::Blocking => AssignmentKind::Blocking,
                    hir::HirAssignmentType::Combinational => AssignmentKind::Blocking,
                };
                return Some((target, kind));
            }
        }
        None
    }

    /// Check if two LValues refer to the same target
    fn lvalues_match(&self, lval1: &LValue, lval2: &LValue) -> bool {
        match (lval1, lval2) {
            (LValue::Signal(id1), LValue::Signal(id2)) => id1 == id2,
            (LValue::Variable(id1), LValue::Variable(id2)) => id1 == id2,
            (LValue::Port(id1), LValue::Port(id2)) => id1 == id2,
            _ => false, // TODO: Handle more complex LValue matching
        }
    }

    /// Build priority mux from HIR if-else-if chain
    fn build_priority_mux_from_hir(
        &mut self,
        if_stmt: &hir::HirIfStatement,
    ) -> Option<PriorityMux> {
        let mut cases = Vec::new();

        // Collect condition-value pairs
        self.collect_conditional_cases_from_hir(if_stmt, &mut cases)?;

        // Get default value (last else clause)
        let default = self.extract_default_value_from_hir(if_stmt)?;

        Some(PriorityMux { cases, default })
    }

    /// Collect conditional cases from HIR if-else-if chain
    fn collect_conditional_cases_from_hir(
        &mut self,
        if_stmt: &hir::HirIfStatement,
        cases: &mut Vec<ConditionalCase>,
    ) -> Option<()> {
        // Add current condition-value pair
        let condition = self.convert_expression(&if_stmt.condition)?;
        let value = self.extract_assignment_value(&if_stmt.then_statements)?;

        cases.push(ConditionalCase { condition, value });

        // Process else branch
        if let Some(else_stmts) = &if_stmt.else_statements {
            if else_stmts.len() == 1 {
                if let hir::HirStatement::If(nested_if) = &else_stmts[0] {
                    // Recursive case: else-if
                    self.collect_conditional_cases_from_hir(nested_if, cases)?;
                }
                // Terminal case: final else handled in extract_default_value_from_hir
            }
        }

        Some(())
    }

    /// Extract assignment value from statement list
    fn extract_assignment_value(&mut self, stmts: &[hir::HirStatement]) -> Option<Expression> {
        if stmts.len() == 1 {
            if let hir::HirStatement::Assignment(assign) = &stmts[0] {
                return self.convert_expression(&assign.rhs);
            }
        }
        None
    }

    /// Extract default value from the final else clause
    fn extract_default_value_from_hir(
        &mut self,
        if_stmt: &hir::HirIfStatement,
    ) -> Option<Expression> {
        if let Some(else_stmts) = &if_stmt.else_statements {
            if else_stmts.len() == 1 {
                match &else_stmts[0] {
                    hir::HirStatement::Assignment(assign) => {
                        // This is the final else clause
                        return self.convert_expression(&assign.rhs);
                    }
                    hir::HirStatement::If(nested_if) => {
                        // Continue searching in nested if
                        return self.extract_default_value_from_hir(nested_if);
                    }
                    _ => {}
                }
            }
        }

        // If no explicit default, use zero (synthesizers typically do this)
        Some(Expression::Literal(Value::Integer(0)))
    }

    /// Check if this is a complex if-else-if chain (multiple conditions)
    /// rather than a simple if-else pattern (like reset logic)
    fn is_complex_if_else_if_chain(&self, if_stmt: &hir::HirIfStatement) -> bool {
        // Count the number of conditions in the chain
        let condition_count = self.count_if_else_conditions(if_stmt);

        // Only consider it complex if there are multiple conditions (2+)
        // This excludes simple if-else patterns like reset logic
        condition_count >= 2
    }

    /// Count the number of conditions in an if-else-if chain
    #[allow(clippy::only_used_in_recursion)]
    fn count_if_else_conditions(&self, if_stmt: &hir::HirIfStatement) -> usize {
        let mut count = 1; // Count the initial if condition

        // Check if the else branch contains another if statement (else-if)
        if let Some(else_stmts) = &if_stmt.else_statements {
            if else_stmts.len() == 1 {
                if let hir::HirStatement::If(nested_if) = &else_stmts[0] {
                    // Recursively count nested if-else-if conditions
                    count += self.count_if_else_conditions(nested_if);
                }
            }
        }

        count
    }

    /// Flatten a port with struct/vector type into multiple MIR ports
    /// Returns the list of created ports and flattened field information
    ///
    /// **New Implementation:** Uses the shared TypeFlattener module for consistent flattening
    fn flatten_port(
        &mut self,
        base_name: &str,
        port_type: &DataType,
        direction: PortDirection,
        physical_constraints: Option<skalp_frontend::hir::PhysicalConstraints>,
    ) -> (Vec<Port>, Vec<FlattenedField>) {
        // Use shared TypeFlattener with current port ID counter
        let mut flattener = TypeFlattener::new(self.next_port_id);
        let (ports, type_fields) =
            flattener.flatten_port(base_name, port_type, direction, physical_constraints);

        // Update our port ID counter based on how many ports were created
        self.next_port_id += ports.len() as u32;

        // Convert TypeFlattenedField to our local FlattenedField type
        let fields: Vec<FlattenedField> = type_fields
            .into_iter()
            .map(|tf| FlattenedField {
                id: tf.id,
                field_path: tf.field_path,
                leaf_type: tf.leaf_type,
            })
            .collect();

        (ports, fields)
    }

    /// Flatten a signal with struct/vector type into multiple MIR signals
    /// Returns the list of created signals and flattened field information
    ///
    /// **New Implementation:** Uses the shared TypeFlattener module for consistent flattening
    fn flatten_signal(
        &mut self,
        base_name: &str,
        signal_type: &DataType,
        initial: Option<Value>,
        clock_domain: Option<ClockDomainId>,
    ) -> (Vec<Signal>, Vec<FlattenedField>) {
        // Use shared TypeFlattener with current signal ID counter
        let mut flattener = TypeFlattener::new(self.next_signal_id);
        let (signals, type_fields) =
            flattener.flatten_signal(base_name, signal_type, initial, clock_domain);

        // Update our signal ID counter based on how many signals were created
        self.next_signal_id += signals.len() as u32;

        // Convert TypeFlattenedField to our local FlattenedField type
        let fields: Vec<FlattenedField> = type_fields
            .into_iter()
            .map(|tf| FlattenedField {
                id: tf.id,
                field_path: tf.field_path,
                leaf_type: tf.leaf_type,
            })
            .collect();

        (signals, fields)
    }
}

impl Default for HirToMir<'_> {
    fn default() -> Self {
        Self::new()
    }
}
