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
    /// Next match expression ID (for globally unique match arm variable prefixes)
    /// BUG FIX #6: Prevents variable name collisions when multiple functions with
    /// match expressions are inlined into the same entity
    next_match_id: u32,
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
    /// Context-aware variable mapping for match arm isolation
    /// Maps (match_arm_prefix, HIR VariableId) -> MIR VariableId
    /// This prevents HIR VariableId collisions across different match arms
    context_variable_map: HashMap<(Option<String>, hir::VariableId), VariableId>,
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
    /// Pending statements from block expressions
    /// These need to be emitted before the current assignment/statement
    pending_statements: Vec<Statement>,
    /// Variable name prefix for match arm scoping
    /// When set, all variables created in this context will be prefixed
    /// to avoid name collisions between match arms
    match_arm_prefix: Option<String>,
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
            next_match_id: 0, // BUG FIX #6
            entity_map: HashMap::new(),
            port_map: HashMap::new(),
            flattened_ports: HashMap::new(),
            port_to_hir: HashMap::new(),
            signal_map: HashMap::new(),
            flattened_signals: HashMap::new(),
            signal_to_hir: HashMap::new(),
            variable_map: HashMap::new(),
            context_variable_map: HashMap::new(),
            clock_domain_map: HashMap::new(),
            hir: None,
            current_entity_id: None,
            const_evaluator: ConstEvaluator::new(),
            dynamic_variables: HashMap::new(),
            type_flattener: TypeFlattener::new(0), // Will be re-initialized per use
            pending_statements: Vec::new(),
            match_arm_prefix: None,
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

                        let mir_var_type = self.convert_type(&hir_var.var_type);
                        eprintln!("[BUG #65/#66 DEBUG] impl_block.variables: name={}, hir_type={:?}, mir_type={:?}",
                            hir_var.name, hir_var.var_type, mir_var_type);
                        let variable = Variable {
                            id: var_id,
                            name: hir_var.name.clone(),
                            var_type: mir_var_type,
                            initial: hir_var
                                .initial_value
                                .as_ref()
                                .and_then(|expr| self.convert_literal_expr(expr)),
                        };
                        eprintln!(
                            "[BUG #71 PUSH LOC1] Pushing variable: id={:?}, name={}",
                            var_id, hir_var.name
                        );
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

                    // BUG #71 DEBUG: Check if var_148 is in dynamic_variables before adding
                    if let Some((_, name, _)) = self
                        .dynamic_variables
                        .values()
                        .find(|(id, _, _)| id.0 == 148)
                    {
                        eprintln!(
                            "[BUG #71 BEFORE LOC2] var_148 IS in dynamic_variables with name: {}",
                            name
                        );
                    } else {
                        eprintln!(
                            "[BUG #71 BEFORE LOC2] var_148 NOT in dynamic_variables (size={})",
                            self.dynamic_variables.len()
                        );
                    }

                    for (mir_var_id, name, hir_type) in dynamic_vars {
                        eprintln!(
                            "[DEBUG] Adding dynamic variable: name={}, hir_type={:?}",
                            name, hir_type
                        );
                        let mir_type = self.convert_type(&hir_type);
                        eprintln!("[DEBUG]   -> mir_type={:?}", mir_type);
                        // BUG #71 DEBUG
                        if name.contains("edge1")
                            || name.contains("edge2")
                            || name.contains("_h")
                            || name.contains("_s")
                        {
                            eprintln!("[BUG #71 EVENT BLOCK VAR] Adding variable '{}' (MIR {:?}): HIR={:?} -> MIR={:?}",
                                name, mir_var_id, hir_type, mir_type);
                        }
                        let variable = Variable {
                            id: mir_var_id,
                            name: name.clone(),
                            var_type: mir_type,
                            initial: None,
                        };
                        eprintln!(
                            "[BUG #71 PUSH LOC2] Pushing dynamic variable: id={:?}, name={}",
                            mir_var_id, name
                        );
                        module.variables.push(variable);
                    }
                    // Clear dynamic variables for next impl block
                    self.dynamic_variables.clear();

                    // Convert continuous assignments (may expand to multiple for structs)
                    eprintln!(
                        "[DEBUG] Processing {} assignments from impl block",
                        impl_block.assignments.len()
                    );
                    for (idx, hir_assign) in impl_block.assignments.iter().enumerate() {
                        eprintln!(
                            "[DEBUG] Assignment {}: LHS={:?}, RHS={:?}",
                            idx,
                            std::mem::discriminant(&hir_assign.lhs),
                            std::mem::discriminant(&hir_assign.rhs)
                        );
                        // Clear any pending statements from previous assignments
                        self.pending_statements.clear();

                        // Convert the assignment (may generate pending statements from block expressions)
                        let assigns = self.convert_continuous_assignment_expanded(hir_assign);

                        // BUG #71 DEBUG: Check variable_map size after assignment conversion
                        eprintln!(
                            "[BUG #71 AFTER ASSIGNMENT {}] variable_map size={}",
                            idx,
                            self.variable_map.len()
                        );
                        eprintln!("[BUG #71 AFTER ASSIGNMENT {}] Checking if HIR VariableId(5) is in variable_map: {}",
                            idx, self.variable_map.contains_key(&hir::VariableId(5)));

                        // BUGFIX: First, scan pending_statements for any variables that need to be declared
                        // These come from let bindings in block expressions that don't go through
                        // the dynamic_variables mechanism (e.g., when functions are inlined)

                        // BUG FIX #71: Don't use a snapshot - use current dynamic_variables directly!
                        // Variables are created during assignment conversion (via match expressions, etc.)
                        // and preserved by BUG #68. A snapshot taken before conversion would be empty.
                        //
                        // Original BUG #56 tried to use a snapshot to avoid borrow checker issues,
                        // but that was taken BEFORE assignment conversion when dynamic_variables was
                        // empty (cleared at line 308).
                        //
                        // The correct approach: Process pending statements and look up types from
                        // the CURRENT state of dynamic_variables, which includes all preserved variables.
                        let pending_stmts_snapshot = self.pending_statements.clone();

                        // BUG #71 DEBUG: Check variable_map size before processing pending statements
                        eprintln!(
                            "[BUG #71 BEFORE PENDING LOOP] variable_map size={}",
                            self.variable_map.len()
                        );
                        eprintln!("[BUG #71 BEFORE PENDING LOOP] Checking if HIR VariableId(5) is in variable_map: {}",
                            self.variable_map.contains_key(&hir::VariableId(5)));

                        for pending_stmt in &pending_stmts_snapshot {
                            if let Statement::Assignment(assign) = pending_stmt {
                                if let LValue::Variable(var_id) = &assign.lhs {
                                    // BUG #71 DEBUG: Check if var_148 is already in the module
                                    let in_module =
                                        module.variables.iter().any(|v| v.id == *var_id);
                                    if var_id.0 == 148 {
                                        eprintln!(
                                            "[BUG #71 IN_MODULE] var_148 in module? {}",
                                            in_module
                                        );
                                    }
                                    // Check if this variable is already in the module
                                    if !in_module {
                                        // BUG FIX #56/#71: Look up type from dynamic_variables instead of inferring from RHS
                                        // This preserves Float types that were declared in let statements
                                        // BUG #71: Use CURRENT dynamic_variables, not empty snapshot

                                        // BUG #71 FIX: Search dynamic_variables by MIR ID, not HIR ID!
                                        // HIR IDs are reused across function contexts, but MIR IDs are unique.
                                        // dynamic_variables is keyed by HIR ID, but the values contain MIR IDs.
                                        // We must search the values to find the entry with matching MIR ID.

                                        // BUG #71 DEBUG: Check if var_148 is in dynamic_variables
                                        if var_id.0 == 148 {
                                            eprintln!("[BUG #71 DYN_VAR LOOKUP] Searching for MIR {:?} in dynamic_variables (size={})",
                                                var_id, self.dynamic_variables.len());
                                            let found = self
                                                .dynamic_variables
                                                .values()
                                                .any(|(id, _, _)| id == var_id);
                                            eprintln!(
                                                "[BUG #71 DYN_VAR LOOKUP] Found var_148? {}",
                                                found
                                            );
                                            if !found {
                                                eprintln!("[BUG #71 DYN_VAR LOOKUP] Listing all variables with 'edge' in name:");
                                                for (mir_id, name, hir_type) in
                                                    self.dynamic_variables.values()
                                                {
                                                    if name.contains("edge") {
                                                        eprintln!("  - MIR {:?}: name='{}', HIR type={:?}", mir_id, name, hir_type);
                                                    }
                                                }
                                            }
                                        }

                                        let dyn_var_info = self
                                            .dynamic_variables
                                            .values()
                                            .find(|(id, _, _)| id == var_id)
                                            .cloned();

                                        let (var_name, var_type) = if let Some((
                                            _,
                                            name,
                                            hir_type,
                                        )) = dyn_var_info
                                        {
                                            // Found in dynamic_variables - use its declared type
                                            let mir_type = self.convert_type(&hir_type);
                                            eprintln!("[BUG #65/#66 DEBUG] Using dyn_var: var_id={}, name={}, hir_type={:?}, mir_type={:?}",
                                                    var_id.0, name, hir_type, mir_type);
                                            // BUG #71 DEBUG
                                            if name.contains("edge1")
                                                || name.contains("edge2")
                                                || name.contains("_h")
                                                || name.contains("_s")
                                            {
                                                eprintln!("[BUG #71 PENDING VAR] Variable '{}' (from pending_statements): HIR={:?} -> MIR={:?}",
                                                    name, hir_type, mir_type);
                                            }
                                            (name, mir_type)
                                        } else {
                                            // Not in dynamic_variables - fall back to name lookup and type inference
                                            let var_name = self
                                                .variable_map
                                                .iter()
                                                .find(|(_, &mir_id)| mir_id == *var_id)
                                                .and_then(|(hir_id, _)| {
                                                    // Try to find the variable name from the HIR
                                                    self.find_variable_name(*hir_id)
                                                })
                                                .unwrap_or_else(|| format!("var_{}", var_id.0));

                                            // Infer the type from the RHS expression
                                            let var_type = self.infer_expression_type_with_module(
                                                &assign.rhs,
                                                module,
                                            );
                                            // BUG #71 DEBUG
                                            if var_name.contains("edge1")
                                                || var_name.contains("edge2")
                                                || var_name.contains("_h")
                                                || var_name.contains("_s")
                                            {
                                                eprintln!("[BUG #71 PENDING VAR INFERRED] Variable '{}' (from pending_statements, TYPE INFERRED): inferred_type={:?}",
                                                    var_name, var_type);
                                            }
                                            (var_name, var_type)
                                        };

                                        let variable = Variable {
                                            id: *var_id,
                                            name: var_name.clone(),
                                            var_type,
                                            initial: None,
                                        };
                                        eprintln!("[BUG #71 PUSH LOC3] Pushing pending variable: id={:?}, name={}", var_id, var_name);
                                        module.variables.push(variable);
                                    }
                                }
                            }
                        }

                        // Add any new dynamic variables that were created during conversion
                        // This ensures variables from let bindings in block expressions are declared
                        // before we try to assign to them
                        let dynamic_vars: Vec<_> =
                            self.dynamic_variables.values().cloned().collect();
                        for (mir_var_id, name, hir_type) in dynamic_vars {
                            // Check if this variable is already in the module
                            if !module.variables.iter().any(|v| v.id == mir_var_id) {
                                eprintln!("[DEBUG] Adding dynamic variable (in impl): name={}, hir_type={:?}", name, hir_type);
                                let mir_type = self.convert_type(&hir_type);
                                eprintln!("[DEBUG]   -> mir_type={:?}", mir_type);
                                // BUG #71 DEBUG: Check if this is one of the problematic variables
                                if name.contains("edge1")
                                    || name.contains("edge2")
                                    || name.contains("_h")
                                    || name.contains("_s")
                                {
                                    eprintln!("[BUG #71 DYNAMIC VAR] Variable '{}' (MIR {:?}): HIR={:?} -> MIR={:?}",
                                        name, mir_var_id, hir_type, mir_type);
                                }
                                let variable = Variable {
                                    id: mir_var_id,
                                    name: name.clone(),
                                    var_type: mir_type,
                                    initial: None,
                                };
                                eprintln!("[BUG #71 PUSH LOC4] Pushing assignment-dynamic variable: id={:?}, name={}", mir_var_id, name);
                                module.variables.push(variable);
                            }
                        }

                        // Emit pending statements as continuous assignments first
                        // These come from let bindings in block expressions within conditionals
                        for pending_stmt in self.pending_statements.drain(..) {
                            if let Statement::Assignment(assign) = pending_stmt {
                                let continuous = ContinuousAssign {
                                    lhs: assign.lhs,
                                    rhs: assign.rhs,
                                };
                                module.assignments.push(continuous);
                            }
                        }

                        // Then emit the main assignment
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
                eprintln!(
                    "[DEBUG] convert_statement: Processing Let for '{}' (ID {:?})",
                    let_stmt.name, let_stmt.id
                );
                if let_stmt.name == "_tuple_tmp_66" {
                    eprintln!("[MIR_LET_TRACE] *** Processing _tuple_tmp_66 - will trace through entire function ***");
                }
                // Convert let statement to assignment
                // Let bindings are local variables that need to be treated as blocking assignments

                // BUG FIX: Check context-aware map first when in match arm context
                let var_id = if let Some(ref prefix) = self.match_arm_prefix {
                    let context_key = (Some(prefix.clone()), let_stmt.id);
                    if let Some(&id) = self.context_variable_map.get(&context_key) {
                        eprintln!("[DEBUG] Let '{}' (ID {:?}): Found in context_variable_map for '{}' as MIR ID={:?}", let_stmt.name, let_stmt.id, prefix, id);
                        id
                    } else {
                        eprintln!("[DEBUG] Let '{}' (ID {:?}): NOT in context_variable_map for '{}', will check variable_map", let_stmt.name, let_stmt.id, prefix);
                        VariableId(u32::MAX) // Will check variable_map next
                    }
                } else if let Some(&id) = self.variable_map.get(&let_stmt.id) {
                    eprintln!(
                        "[DEBUG] Let '{}' (ID {:?}): Found in variable_map as MIR ID={:?}",
                        let_stmt.name, let_stmt.id, id
                    );
                    // Check if we're in a match arm context and the existing variable
                    // doesn't have the correct prefix - this can happen when a variable
                    // from an outer scope (like _tuple_tmp_0) has the same HIR VariableId
                    // as a function-local variable in an inlined function
                    let should_create_new = if let Some(ref prefix) = self.match_arm_prefix {
                        // Check if the existing variable has the wrong prefix
                        if let Some((_, existing_name, _)) =
                            self.dynamic_variables.get(&let_stmt.id)
                        {
                            let wrong_prefix = !existing_name.starts_with(prefix);
                            eprintln!("[DEBUG] Let '{}': match_arm_prefix={}, existing_name={}, wrong_prefix={}",
                                let_stmt.name, prefix, existing_name, wrong_prefix);
                            wrong_prefix
                        } else {
                            // Not in dynamic_variables, might be a module-level variable
                            // Create new to be safe
                            eprintln!(
                                "[DEBUG] Let '{}': Not in dynamic_variables, will create new",
                                let_stmt.name
                            );
                            true
                        }
                    } else {
                        eprintln!(
                            "[DEBUG] Let '{}': No match_arm_prefix, reusing existing",
                            let_stmt.name
                        );
                        false
                    };

                    if should_create_new {
                        eprintln!("[DEBUG] Let '{}': Will create new variable (wrong prefix or not in dynamic_variables)", let_stmt.name);
                        // Don't reuse the existing variable, fall through to create a new one
                        VariableId(u32::MAX) // Sentinel value that won't match any existing variable
                    } else {
                        eprintln!(
                            "[DEBUG] Let '{}': Reusing existing MIR ID={:?}",
                            let_stmt.name, id
                        );
                        // Already registered with correct scope
                        id
                    }
                } else {
                    eprintln!(
                        "[DEBUG] Let '{}' (ID {:?}): NOT in variable_map, will create new",
                        let_stmt.name, let_stmt.id
                    );
                    VariableId(u32::MAX) // Sentinel value to trigger new variable creation
                };

                // BUG FIX #67 with REGRESSION FIX: Context-aware RHS conversion
                //
                // We need to handle two cases:
                // 1. Simple function call RHS (Bug #67 case): Convert RHS first to get correct types
                //    Example: let (rx, ry, rz) = vec3_add(...)
                //
                // 2. Complex RHS that references the let variable (regression case): Register variable first
                //    Example: let rx = match opcode { 32 => { let (rx, ...) = ...; {0, rz, ry, rx} }, _ => 0 }
                //
                // Heuristic: If RHS is a simple direct function call OR if we're extracting from
                // a tuple (which comes from a function call), convert first.
                // Otherwise, register variable first to allow RHS to reference it.

                let is_simple_function_call = matches!(let_stmt.value, hir::HirExpression::Call(_));

                // Check if this is tuple element extraction from a function call
                // Pattern: let rx = <tuple_tmp>.0 where <tuple_tmp> was from a function call
                // BUT: Exclude cases where the base is a Block, because Blocks may reference
                // variables that haven't been created yet (circular dependency during inlining)
                let (base_is_block, base_type, rhs_is_field_access, base_is_dynamic_var) =
                    if let hir::HirExpression::FieldAccess { base, field } = &let_stmt.value {
                        let is_block = matches!(**base, hir::HirExpression::Block { .. });
                        let discriminant = std::mem::discriminant(&**base);
                        // Check if base is a dynamically-created variable (from let bindings in inlined functions)
                        let is_dynamic = if let hir::HirExpression::Variable(var_id) = &**base {
                            self.dynamic_variables.contains_key(var_id)
                        } else {
                            false
                        };
                        eprintln!("[DEBUG] Let '{}': FieldAccess.field={}, base type={:?}, base_is_dynamic={}", let_stmt.name, field, discriminant, is_dynamic);
                        (is_block, Some(discriminant), true, is_dynamic)
                    } else {
                        (false, None, false, false)
                    };
                if let_stmt.name == "rw" && let_stmt.id == hir::VariableId(70) {
                    eprintln!("[DEBUG] Let 'rw' (70): RHS type {:?}, rhs_is_field_access={}, base_is_block={}, base_type={:?}",
                        std::mem::discriminant(&let_stmt.value), rhs_is_field_access, base_is_block, base_type);
                    eprintln!("[DEBUG] Let 'rw' (70): should_convert_first will be calculated...");
                }
                let is_tuple_element_extraction = matches!(
                    &let_stmt.value,
                    hir::HirExpression::FieldAccess { base, field }
                    if field.chars().all(|c| c.is_ascii_digit()) // Numeric field like "0", "1", "2"
                    && !base_is_block  // Exclude if base is Block (circular dependency)
                    && !base_is_dynamic_var  // Exclude if base is dynamic variable (may have complex RHS)
                );

                // Check if this is a cast expression
                // Pattern: let ax_fp = ax as fp32
                // Bug #67: Cast expressions should convert first to get proper type from the cast
                let is_cast_expression = matches!(let_stmt.value, hir::HirExpression::Cast(_));

                // BUG FIX: Don't convert first when in match arm context during function inlining
                // because the RHS might reference variables that haven't been created yet (circular dependency)
                let in_match_context = self.match_arm_prefix.is_some();
                let should_convert_first =
                    (is_simple_function_call || is_tuple_element_extraction || is_cast_expression)
                        && !in_match_context; // Always register variable first in match arms

                if let_stmt.name == "rw" && let_stmt.id == hir::VariableId(70) {
                    eprintln!("[DEBUG] Let 'rw' (70): is_tuple_element_extraction={}, in_match_context={}, should_convert_first={}",
                        is_tuple_element_extraction, in_match_context, should_convert_first);
                }

                let (rhs, needs_type_inference) = if should_convert_first {
                    // Simple function call, tuple element extraction, or cast: convert first for Bug #67 fix
                    if let_stmt.name == "rw" && let_stmt.id == hir::VariableId(70) {
                        eprintln!(
                            "[DEBUG] Let 'rw' (70): About to convert RHS (convert_first=true)"
                        );
                    }
                    if let_stmt.name == "_tuple_tmp_66" {
                        eprintln!("[MIR_LET_TRACE] _tuple_tmp_66: should_convert_first=true, converting RHS now");
                    }
                    (self.convert_expression(&let_stmt.value)?, true)
                } else {
                    // Complex expression: will convert after variable registration
                    if let_stmt.name == "_tuple_tmp_66" {
                        eprintln!("[MIR_LET_TRACE] _tuple_tmp_66: should_convert_first=false, using placeholder");
                    }
                    (Expression::Literal(Value::Integer(0)), false) // Placeholder, will be replaced
                };

                let var_id = if var_id == VariableId(u32::MAX) {
                    if let_stmt.name == "_tuple_tmp_66" {
                        eprintln!("[MIR_LET_TRACE] _tuple_tmp_66: var_id is u32::MAX, creating new variable");
                    }
                    // Apply match arm prefix if we're in a match arm context
                    // This prevents variable name collisions between different match arms
                    // IMPORTANT: Do this BEFORE checking for duplicates so we check the correct name
                    let var_name = if let Some(ref prefix) = self.match_arm_prefix {
                        format!("{}_{}", prefix, let_stmt.name)
                    } else {
                        let_stmt.name.clone()
                    };

                    // Check if we already have a dynamic variable with this name
                    // If so, reuse its ID to avoid duplicate declarations
                    let existing_var = self
                        .dynamic_variables
                        .values()
                        .find(|(_, name, _)| name == &var_name);

                    let new_id = if let Some((existing_id, _, _)) = existing_var {
                        // Reuse the existing variable ID for this name
                        eprintln!(
                            "[DEBUG] Reusing existing variable '{}' with MIR ID={:?}",
                            var_name, existing_id
                        );
                        *existing_id
                    } else {
                        // Create a new MIR variable on the fly for event block let bindings
                        let new_id = self.next_variable_id();

                        // BUG FIX #67: Infer type from the CONVERTED expression (after inlining)
                        // instead of using the HIR placeholder type - BUT ONLY for simple function calls
                        let final_hir_type = if needs_type_inference {
                            // Simple function call case: infer from converted expression
                            let inferred_type = self.infer_expression_type(&rhs);
                            let hir_placeholder_type = &let_stmt.var_type;

                            // Use inferred type if it's more specific than the HIR placeholder
                            // The HIR placeholder is often Nat(32) for unknown types
                            if matches!(hir_placeholder_type, hir::HirType::Nat(32))
                                && !matches!(inferred_type, DataType::Nat(32))
                            {
                                // Use inferred type if it's more specific than the Nat(32) placeholder
                                self.convert_mir_to_hir_type(&inferred_type)
                            } else if matches!(hir_placeholder_type, hir::HirType::Tuple(_)) {
                                // For tuple types, use the HIR placeholder directly
                                // Don't convert to MIR and back - preserve the original tuple type
                                hir_placeholder_type.clone()
                            } else {
                                // For other types, convert through MIR to ensure consistency
                                let converted_hir_type = self.convert_type(hir_placeholder_type);
                                // Convert back to HIR for storage
                                self.convert_mir_to_hir_type(&converted_hir_type)
                            }
                        } else {
                            // Complex RHS case: use HIR placeholder, will be refined later if needed
                            eprintln!(
                                "[BUG #67] Variable '{}': Using HIR placeholder type {:?} for complex RHS",
                                var_name, let_stmt.var_type
                            );
                            eprintln!(
                                "[BUG #71] Variable '{}'  (MIR ID={:?}): HIR type={:?}, RHS discriminant={:?}",
                                var_name, new_id, let_stmt.var_type, std::mem::discriminant(&let_stmt.value)
                            );
                            // BUG #71 FIX: For complex RHS with tuple type placeholders, try to infer from RHS after conversion
                            if matches!(let_stmt.var_type, hir::HirType::Nat(32)) {
                                eprintln!("[BUG #71] Variable '{}': HIR type is Nat(32) placeholder - will need to infer from RHS later", var_name);
                            }
                            let_stmt.var_type.clone()
                        };

                        // Track this dynamically created variable so we can add it to the module later
                        eprintln!(
                            "[DEBUG] Creating dynamic variable: name={}, HIR ID={:?}, MIR ID={:?}, type={:?}",
                            var_name, let_stmt.id, new_id, final_hir_type
                        );
                        self.dynamic_variables
                            .insert(let_stmt.id, (new_id, var_name.clone(), final_hir_type));

                        new_id
                    };

                    // Map this HIR variable ID to the MIR variable ID (whether new or reused)
                    // BUG FIX: Use context-aware map when in match arm context to prevent collisions
                    if let Some(ref prefix) = self.match_arm_prefix {
                        let context_key = (Some(prefix.clone()), let_stmt.id);
                        eprintln!(
                            "[DEBUG] Storing context-aware mapping: {:?} in '{}' -> MIR {:?}",
                            let_stmt.id, prefix, new_id
                        );
                        self.context_variable_map.insert(context_key, new_id);
                        // BUG #71 FIX: Also add to variable_map so pending statements can find it
                        // This is needed because pending statements only have the MIR ID and need
                        // to reverse-lookup the HIR ID to find the variable in dynamic_variables
                        if new_id.0 >= 148 && new_id.0 <= 150 {
                            eprintln!(
                                "[BUG #71 INSERT] BEFORE insert: variable_map size={}",
                                self.variable_map.len()
                            );
                            eprintln!(
                                "[BUG #71 INSERT] Inserting HIR {:?} -> MIR {:?} into variable_map",
                                let_stmt.id, new_id
                            );
                        }
                        self.variable_map.insert(let_stmt.id, new_id);
                        if new_id.0 >= 148 && new_id.0 <= 150 {
                            eprintln!(
                                "[BUG #71 INSERT] AFTER insert: variable_map size={}",
                                self.variable_map.len()
                            );
                            eprintln!(
                                "[BUG #71 INSERT] Verify: variable_map contains {:?}? {}",
                                let_stmt.id,
                                self.variable_map.contains_key(&let_stmt.id)
                            );
                        }
                    } else {
                        self.variable_map.insert(let_stmt.id, new_id);
                    }

                    new_id
                } else {
                    if let_stmt.name == "_tuple_tmp_66" {
                        eprintln!(
                            "[MIR_LET_TRACE] _tuple_tmp_66: var_id exists, using {:?}",
                            var_id
                        );
                    }
                    // Variable already exists with correct scope, use it
                    var_id
                };

                if let_stmt.name == "_tuple_tmp_66" {
                    eprintln!(
                        "[MIR_LET_TRACE] _tuple_tmp_66: About to convert final RHS, var_id={:?}",
                        var_id
                    );
                }
                // For complex RHS, convert it now that the variable is registered
                eprintln!(
                    "[MIR_LET_FINAL] Converting final RHS for var {:?}, needs_type_inference={}",
                    var_id, needs_type_inference
                );
                let final_rhs = if needs_type_inference {
                    // Already converted
                    eprintln!("[MIR_LET_FINAL]   Using already converted RHS");
                    rhs
                } else {
                    // Convert now that variable is registered and available
                    eprintln!("[MIR_LET_FINAL]   Converting RHS now: {:?}", let_stmt.value);
                    if let_stmt.name == "_tuple_tmp_66" {
                        eprintln!(
                            "[MIR_LET_TRACE] _tuple_tmp_66: Calling convert_expression on RHS"
                        );
                    }
                    let converted = self.convert_expression(&let_stmt.value);
                    if let_stmt.name == "_tuple_tmp_66" {
                        eprintln!(
                            "[MIR_LET_TRACE] _tuple_tmp_66: convert_expression returned: {:?}",
                            converted.is_some()
                        );
                        if converted.is_none() {
                            eprintln!("[MIR_LET_TRACE] _tuple_tmp_66: convert_expression returned None - returning None from convert_statement!");
                        }
                    }
                    let converted = converted?; // This will return None if conversion failed
                    eprintln!("[MIR_LET_FINAL]   Converted RHS: {:?}", converted);
                    converted
                };

                let lhs = LValue::Variable(var_id);
                eprintln!(
                    "[MIR_LET_FINAL] Creating assignment: {:?} = {:?}",
                    lhs, final_rhs
                );
                Some(Statement::Assignment(Assignment {
                    lhs,
                    rhs: final_rhs,
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
            eprintln!(
                "[DEBUG] Array index read expansion returned Some with {} assigns",
                assigns.len()
            );
            return assigns;
        }

        // Try to expand struct-to-struct assignments
        if let Some(assigns) = self.try_expand_struct_continuous_assignment(assign) {
            eprintln!(
                "[DEBUG] Struct expansion returned Some with {} assigns",
                assigns.len()
            );
            return assigns;
        }

        eprintln!(
            "[DEBUG] Trying to convert continuous assignment, RHS type: {:?}",
            std::mem::discriminant(&assign.rhs)
        );

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

        let lhs = self.convert_lvalue(&assign.lhs);
        if lhs.is_none() {
            eprintln!(
                "[DEBUG] convert_lvalue returned None for LHS: {:?}",
                std::mem::discriminant(&assign.lhs)
            );
            return None;
        }
        let lhs = lhs?;
        eprintln!(
            "[DEBUG] LHS converted successfully: {:?}",
            std::mem::discriminant(&lhs)
        );

        let rhs = self.convert_expression(&assign.rhs);
        if rhs.is_none() {
            eprintln!(
                "[DEBUG] convert_expression returned None for RHS: {:?}",
                std::mem::discriminant(&assign.rhs)
            );
            return None;
        }
        let rhs = rhs?;

        eprintln!("[DEBUG] Continuous assignment successful!");
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
                // BUG #20 FIX: Check variable_map first, then fall back to dynamic_variables
                // Same as expr_to_lvalue - dynamic variables need to be accessible as LValues
                if let Some(&mir_id) = self.variable_map.get(id) {
                    Some(LValue::Variable(mir_id))
                } else if let Some((mir_id, _, _)) = self.dynamic_variables.get(id) {
                    Some(LValue::Variable(*mir_id))
                } else {
                    None
                }
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
            hir::HirExpression::Variable(id) => {
                // BUG FIX: Check context-aware map first when in match arm context
                // This prevents HIR VariableId collisions across different match arms
                if let Some(ref prefix) = self.match_arm_prefix {
                    let context_key = (Some(prefix.clone()), *id);
                    if let Some(&mir_id) = self.context_variable_map.get(&context_key) {
                        eprintln!(
                            "[DEBUG] Variable lookup: Found context-aware mapping for {:?} in '{}' -> MIR {:?}",
                            id, prefix, mir_id
                        );
                        return Some(Expression::Ref(LValue::Variable(mir_id)));
                    }
                }

                // Check regular variable_map
                if let Some(&mir_id) = self.variable_map.get(id) {
                    Some(Expression::Ref(LValue::Variable(mir_id)))
                } else if let Some((mir_id, name, _)) = self.dynamic_variables.get(id) {
                    // Check dynamic_variables for let bindings from inlined functions
                    eprintln!(
                        "[DEBUG] Variable lookup: Found '{}' in dynamic_variables -> {:?}",
                        name, mir_id
                    );
                    Some(Expression::Ref(LValue::Variable(*mir_id)))
                } else {
                    eprintln!(
                        "[DEBUG] Variable not found in variable_map or dynamic_variables: HIR ID {:?}",
                        id
                    );
                    eprintln!(
                        "[DEBUG]   Current match_arm_prefix: {:?}",
                        self.match_arm_prefix
                    );
                    eprintln!(
                        "[DEBUG]   Current dynamic_variables: {:?}",
                        self.dynamic_variables
                            .iter()
                            .map(|(_, (_, name, _))| name)
                            .collect::<Vec<_>>()
                    );
                    eprintln!(
                        "[DEBUG]   context_variable_map keys: {:?}",
                        self.context_variable_map.keys().collect::<Vec<_>>()
                    );
                    None
                }
            }
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
                // BUG FIX #7: WORKAROUND for Bug #42/43 - Cast expressions create malformed AST
                // causing variable references to appear as GenericParam instead of Variable.
                // First check if this is actually a variable from a let-binding (e.g., from inlined function)
                if let Some((mir_var_id, _, _)) = self
                    .dynamic_variables
                    .values()
                    .find(|(_, var_name, _)| var_name == param_name)
                {
                    return Some(Expression::Ref(LValue::Variable(*mir_var_id)));
                }

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
                let left = self.convert_expression(&binary.left)?;
                let right = self.convert_expression(&binary.right)?;
                let left = Box::new(left);
                let right = Box::new(right);
                let op = self.convert_binary_op(&binary.op, &binary.left);
                Some(Expression::Binary { op, left, right })
            }
            hir::HirExpression::Unary(unary) => {
                let operand = Box::new(self.convert_expression(&unary.operand)?);
                let op = self.convert_unary_op(&unary.op);
                Some(Expression::Unary { op, operand })
            }
            hir::HirExpression::Call(call) => {
                eprintln!(
                    "[MIR_CALL] Converting Call expression: function='{}', args={}",
                    call.function,
                    call.args.len()
                );
                // Check if this is a built-in FP method call (e.g., a_fp32.add(b_fp32))
                // FP methods are transformed to method(receiver, args) by HIR builder
                // So args[0] is the receiver for FP methods
                // Try to detect FP method calls by checking if any argument has an FP type
                // This handles cases where after function inlining, the receiver variable
                // doesn't exist in HIR's variable list, causing type inference to fail
                let mut is_fp_method = false;
                if !call.args.is_empty() {
                    // First try the receiver (args[0])
                    if let Some(receiver_type) = self.infer_hir_type(&call.args[0]) {
                        if self.is_float_type(&receiver_type) {
                            is_fp_method = true;
                        }
                    }

                    // If receiver type inference failed, try other arguments
                    // This handles inlined function bodies where variables aren't in HIR
                    if !is_fp_method {
                        for arg in call.args.iter() {
                            if let Some(arg_type) = self.infer_hir_type(arg) {
                                if self.is_float_type(&arg_type) {
                                    is_fp_method = true;
                                    break;
                                }
                            }
                        }
                    }
                }

                if is_fp_method {
                    // This is a method call on a float type
                    // Map FP methods to MIR operations
                    match call.function.as_str() {
                        // Binary FP operations
                        "add" if call.args.len() == 2 => {
                            eprintln!("[MIR_CALL] Converting FP add with {} args", call.args.len());
                            let left_result = self.convert_expression(&call.args[0]);
                            if left_result.is_none() {
                                eprintln!("[MIR_CALL] FP add: left argument conversion FAILED");
                                return None;
                            }
                            let left = Box::new(left_result?);
                            let right_result = self.convert_expression(&call.args[1]);
                            if right_result.is_none() {
                                eprintln!("[MIR_CALL] FP add: right argument conversion FAILED");
                                return None;
                            }
                            let right = Box::new(right_result?);
                            eprintln!("[MIR_CALL] FP add conversion succeeded");
                            return Some(Expression::Binary {
                                op: BinaryOp::FAdd,
                                left,
                                right,
                            });
                        }
                        "sub" if call.args.len() == 2 => {
                            let left = Box::new(self.convert_expression(&call.args[0])?);
                            let right = Box::new(self.convert_expression(&call.args[1])?);
                            return Some(Expression::Binary {
                                op: BinaryOp::FSub,
                                left,
                                right,
                            });
                        }
                        "mul" if call.args.len() == 2 => {
                            let left = Box::new(self.convert_expression(&call.args[0])?);
                            let right = Box::new(self.convert_expression(&call.args[1])?);
                            return Some(Expression::Binary {
                                op: BinaryOp::FMul,
                                left,
                                right,
                            });
                        }
                        "div" if call.args.len() == 2 => {
                            let left = Box::new(self.convert_expression(&call.args[0])?);
                            let right = Box::new(self.convert_expression(&call.args[1])?);
                            return Some(Expression::Binary {
                                op: BinaryOp::FDiv,
                                left,
                                right,
                            });
                        }
                        // Unary FP operations
                        "sqrt" if call.args.len() == 1 => {
                            let operand = Box::new(self.convert_expression(&call.args[0])?);
                            return Some(Expression::Unary {
                                op: UnaryOp::FSqrt,
                                operand,
                            });
                        }
                        "abs" if call.args.len() == 1 => {
                            let arg = self.convert_expression(&call.args[0])?;
                            return Some(Expression::FunctionCall {
                                name: "fabs".to_string(),
                                args: vec![arg],
                            });
                        }
                        // BUG FIX #4: FP comparison methods
                        "lt" if call.args.len() == 2 => {
                            let left = Box::new(self.convert_expression(&call.args[0])?);
                            let right = Box::new(self.convert_expression(&call.args[1])?);
                            return Some(Expression::Binary {
                                op: BinaryOp::FLess,
                                left,
                                right,
                            });
                        }
                        "gt" if call.args.len() == 2 => {
                            let left = Box::new(self.convert_expression(&call.args[0])?);
                            let right = Box::new(self.convert_expression(&call.args[1])?);
                            return Some(Expression::Binary {
                                op: BinaryOp::FGreater,
                                left,
                                right,
                            });
                        }
                        "le" if call.args.len() == 2 => {
                            let left = Box::new(self.convert_expression(&call.args[0])?);
                            let right = Box::new(self.convert_expression(&call.args[1])?);
                            return Some(Expression::Binary {
                                op: BinaryOp::FLessEqual,
                                left,
                                right,
                            });
                        }
                        "ge" if call.args.len() == 2 => {
                            let left = Box::new(self.convert_expression(&call.args[0])?);
                            let right = Box::new(self.convert_expression(&call.args[1])?);
                            return Some(Expression::Binary {
                                op: BinaryOp::FGreaterEqual,
                                left,
                                right,
                            });
                        }
                        "eq" if call.args.len() == 2 => {
                            let left = Box::new(self.convert_expression(&call.args[0])?);
                            let right = Box::new(self.convert_expression(&call.args[1])?);
                            return Some(Expression::Binary {
                                op: BinaryOp::FEqual,
                                left,
                                right,
                            });
                        }
                        "ne" if call.args.len() == 2 => {
                            let left = Box::new(self.convert_expression(&call.args[0])?);
                            let right = Box::new(self.convert_expression(&call.args[1])?);
                            return Some(Expression::Binary {
                                op: BinaryOp::FNotEqual,
                                left,
                                right,
                            });
                        }
                        _ => {
                            // Unknown FP method - fall through to regular inlining
                        }
                    }
                }

                // Check for standalone sqrt() function call (not a method)
                // This handles: sqrt(value) as a compiler intrinsic
                if call.function == "sqrt" && call.args.len() == 1 {
                    // Check if argument has FP type
                    if let Some(arg_type) = self.infer_hir_type(&call.args[0]) {
                        if self.is_float_type(&arg_type) {
                            let operand = Box::new(self.convert_expression(&call.args[0])?);
                            return Some(Expression::Unary {
                                op: UnaryOp::FSqrt,
                                operand,
                            });
                        }
                    }
                }

                // Not an FP method or intrinsic - inline the function call
                eprintln!(
                    "[MIR_CALL] Call '{}' is not FP method/intrinsic, will inline",
                    call.function
                );
                let result = self.inline_function_call(call);
                if result.is_none() {
                    eprintln!("[MIR_CALL] Call '{}' inlining FAILED", call.function);
                } else {
                    eprintln!("[MIR_CALL] Call '{}' inlining succeeded", call.function);
                }
                result
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
                eprintln!(
                    "[BUG #71 FIELD] Converting FieldAccess: field='{}', base={:?}",
                    field,
                    std::mem::discriminant(&**base)
                );
                if let hir::HirExpression::Variable(var_id) = &**base {
                    eprintln!("[BUG #71 FIELD]   Base is Variable({})", var_id.0);
                    // Check if this variable is in dynamic_variables (tuple temporary)
                    if let Some((mir_id, name, hir_type)) = self.dynamic_variables.get(var_id) {
                        eprintln!(
                            "[BUG #71 FIELD]   Found in dynamic_variables: name='{}', type={:?}",
                            name, hir_type
                        );
                        if matches!(hir_type, hir::HirType::Tuple(_)) {
                            eprintln!("[BUG #71 FIELD]   ✅ This is a TUPLE variable!");
                        }
                    }
                }
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

                eprintln!(
                    "[DEBUG] Converting Concat with {} elements",
                    expressions.len()
                );
                for (i, expr) in expressions.iter().enumerate() {
                    eprintln!(
                        "[DEBUG] Concat element {}: {:?}",
                        i,
                        std::mem::discriminant(expr)
                    );
                }

                if expressions.is_empty() {
                    return Some(Expression::Literal(Value::Integer(0)));
                }

                if expressions.len() == 1 {
                    eprintln!("[DEBUG] Concat has only 1 element, unwrapping");
                    return self.convert_expression(&expressions[0]);
                }

                // Convert all concat elements to MIR expressions
                let mut mir_exprs = Vec::new();
                for (idx, expr) in expressions.iter().enumerate() {
                    eprintln!(
                        "[DEBUG] Concat: converting element {} of {}, type: {:?}",
                        idx,
                        expressions.len(),
                        std::mem::discriminant(expr)
                    );
                    if let hir::HirExpression::Variable(var_id) = expr {
                        eprintln!("[DEBUG] Concat element {} is Variable({:?})", idx, var_id);
                        eprintln!(
                            "[DEBUG]   variable_map contains: {}",
                            self.variable_map.contains_key(var_id)
                        );
                        eprintln!(
                            "[DEBUG]   dynamic_variables contains: {}",
                            self.dynamic_variables.contains_key(var_id)
                        );
                    }
                    if let Some(mir_expr) = self.convert_expression(expr) {
                        mir_exprs.push(mir_expr);
                    } else {
                        eprintln!(
                            "[DEBUG] Concat: failed to convert element {}, type: {:?}",
                            idx,
                            std::mem::discriminant(expr)
                        );
                        return None;
                    }
                }

                eprintln!(
                    "[DEBUG] Concat successfully converted with {} MIR elements",
                    mir_exprs.len()
                );
                Some(Expression::Concat(mir_exprs))
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
                //
                // BUG FIX #50: Isolate dynamic variable scopes between branches
                // Each branch can create variables with the same names (e.g., result_32)
                // We must prevent variable name collisions between branches
                //
                // BUG FIX #61: Preserve variables that were hoisted to pending_statements
                // When block expressions in branches contain let bindings, those bindings
                // are added to pending_statements (to be emitted as global assignments).
                // We must preserve these variables across branch isolation so that the
                // result expression can reference them.
                eprintln!("[IF_DEBUG] ========== STARTING IF EXPRESSION CONVERSION ==========");
                eprintln!(
                    "[IF_DEBUG] Converting If-expression condition, type: {:?}",
                    std::mem::discriminant(&*if_expr.condition)
                );
                if let hir::HirExpression::Call(call) = &*if_expr.condition {
                    eprintln!(
                        "[IF_DEBUG]   Condition is Call: function={}, args={}",
                        call.function,
                        call.args.len()
                    );
                }
                let cond = self.convert_expression(&if_expr.condition);
                if cond.is_none() {
                    eprintln!("[IF_DEBUG] ❌ CONDITION conversion FAILED");
                    return None;
                }
                let cond = cond?;
                eprintln!(
                    "[IF_DEBUG] ✅ Converted condition to MIR: {:?}",
                    std::mem::discriminant(&cond)
                );

                // Save current dynamic_variables state and pending_statements count
                let saved_dynamic_vars = self.dynamic_variables.clone();
                let saved_pending_count = self.pending_statements.len();

                eprintln!(
                    "[IF_DEBUG] Converting THEN branch, expr type: {:?}",
                    std::mem::discriminant(&*if_expr.then_expr)
                );
                if let hir::HirExpression::Call(call) = &*if_expr.then_expr {
                    eprintln!(
                        "[IF_DEBUG]   Then-expr is Call: function={}, args={}",
                        call.function,
                        call.args.len()
                    );
                }
                let then_expr = self.convert_expression(&if_expr.then_expr);
                if then_expr.is_none() {
                    eprintln!(
                        "[IF_DEBUG] ❌ THEN BRANCH conversion FAILED, type: {:?}",
                        std::mem::discriminant(&*if_expr.then_expr)
                    );
                    return None;
                }
                let then_expr = then_expr?;
                eprintln!("[IF_DEBUG] ✅ THEN branch converted successfully");

                eprintln!("[DEBUG] If-expr: then_expr = {:?}", then_expr);

                // BUG FIX #63/#68: Capture variables created in then-branch before restoring
                // We need to track ALL new variables from BOTH branches for nested if/else.
                let then_branch_vars = self.dynamic_variables.clone();

                // BUG FIX #71: Before restoring for else-branch, merge new then-branch variables
                // into saved state. This prevents nested restorations in the else-branch from
                // losing variables created in deeply nested scopes of the then-branch.
                //
                // Without this fix, variables like edge1 created in nested matches inside the
                // then-branch would be lost when the else-branch does its own restorations,
                // and couldn't be recovered by the final restoration.
                let mut state_for_else = saved_dynamic_vars.clone();
                for (hir_var_id, (mir_var_id, name, hir_type)) in then_branch_vars.iter() {
                    if !saved_dynamic_vars.contains_key(hir_var_id) {
                        // Preserve new variables from then-branch for else-branch context
                        state_for_else
                            .insert(*hir_var_id, (*mir_var_id, name.clone(), hir_type.clone()));
                    }
                }

                // BUG FIX #63: Restore dynamic_variables before else-branch
                // Start else-branch with original variables PLUS new variables from then-branch.
                // This prevents variable name collisions while preserving nested scope variables.
                self.dynamic_variables = state_for_else;

                eprintln!(
                    "[IF_DEBUG] Converting ELSE branch, expr type: {:?}",
                    std::mem::discriminant(&*if_expr.else_expr)
                );
                if let hir::HirExpression::Call(call) = &*if_expr.else_expr {
                    eprintln!(
                        "[IF_DEBUG]   Else-expr is Call: function={}, args={}",
                        call.function,
                        call.args.len()
                    );
                }
                if let hir::HirExpression::If(_) = &*if_expr.else_expr {
                    eprintln!("[IF_DEBUG]   Else-expr is nested If (else-if chain)");
                }
                let else_expr = self.convert_expression(&if_expr.else_expr);
                if else_expr.is_none() {
                    eprintln!(
                        "[IF_DEBUG] ❌ ELSE BRANCH conversion FAILED, type: {:?}",
                        std::mem::discriminant(&*if_expr.else_expr)
                    );
                    return None;
                }
                let else_expr = else_expr?;
                eprintln!("[IF_DEBUG] ✅ ELSE branch converted successfully");

                // BUG FIX #63/#68: After both branches are processed, restore to the original state
                // BUT preserve NEW variables created during branch processing from BOTH branches.
                //
                // In nested if/else expressions, inner branches may create variables (via function
                // inlining with tuple destructuring) that outer expressions need to reference.
                // We must preserve these newly created variables in dynamic_variables while still
                // isolating branch-local variables to prevent name collisions between sibling branches.
                //
                // Example: 4-level nested if/else with function inlining
                //   if a < 10 { 0 } else if a < 20 { 0 } else if a < 30 { exec(...) } else { 0 }
                // The exec() call creates variables that must remain accessible to parent if/else expressions.
                //
                // The key insight: We need to merge new variables from BOTH then-branch AND else-branch,
                // because in deeply nested if/else, the variables may be created in the then-branch
                // and then cleared by subsequent scope restorations in nested else-branches.

                let mut restored_vars = saved_dynamic_vars.clone();
                let mut preserved_count = 0;

                // BUG #71 DEBUG: Check if edge1 is in then_branch_vars
                let edge1_in_then = then_branch_vars
                    .values()
                    .any(|(id, name, _)| id.0 == 148 || name.contains("edge1"));
                eprintln!(
                    "[BUG #71 THEN_VARS] edge1/var_148 in then_branch_vars? {}",
                    edge1_in_then
                );
                if edge1_in_then {
                    for (mir_id, name, _) in then_branch_vars.values() {
                        if mir_id.0 == 148 || name.contains("edge1") {
                            eprintln!(
                                "[BUG #71 THEN_VARS]   Found: MIR {:?}, name='{}'",
                                mir_id, name
                            );
                        }
                    }
                }

                // Preserve new variables from then-branch
                for (hir_var_id, (mir_var_id, name, hir_type)) in then_branch_vars.iter() {
                    if !saved_dynamic_vars.contains_key(hir_var_id) {
                        eprintln!("[BUG #68] Preserving variable from then-branch: HIR {:?} -> MIR {:?} ({})",
                            hir_var_id, mir_var_id, name);
                        restored_vars
                            .insert(*hir_var_id, (*mir_var_id, name.clone(), hir_type.clone()));
                        preserved_count += 1;
                    }
                }

                // Preserve new variables from else-branch
                for (hir_var_id, (mir_var_id, name, hir_type)) in self.dynamic_variables.iter() {
                    if !saved_dynamic_vars.contains_key(hir_var_id)
                        && !restored_vars.contains_key(hir_var_id)
                    {
                        eprintln!("[BUG #68] Preserving variable from else-branch: HIR {:?} -> MIR {:?} ({})",
                            hir_var_id, mir_var_id, name);
                        restored_vars
                            .insert(*hir_var_id, (*mir_var_id, name.clone(), hir_type.clone()));
                        preserved_count += 1;
                    }
                }

                eprintln!("[BUG #68] If-expression restoration: saved {} vars, then-branch {} vars, else-branch {} vars, preserved {} new vars",
                    saved_dynamic_vars.len(), then_branch_vars.len(), self.dynamic_variables.len(), preserved_count);

                // BUG #71 DEBUG: Check if edge1 is in restored_vars before assignment
                let has_edge1_before = restored_vars
                    .values()
                    .any(|(id, name, _)| id.0 == 148 || name.contains("edge1"));
                eprintln!(
                    "[BUG #71 RESTORE] About to restore dynamic_variables, has edge1/var_148? {}",
                    has_edge1_before
                );
                if has_edge1_before {
                    for (mir_id, name, _) in restored_vars.values() {
                        if mir_id.0 == 148 || name.contains("edge1") {
                            eprintln!(
                                "[BUG #71 RESTORE]   Found: MIR {:?}, name='{}'",
                                mir_id, name
                            );
                        }
                    }
                }

                self.dynamic_variables = restored_vars;

                // BUG #71 DEBUG: Verify edge1 is still there after assignment
                let has_edge1_after = self
                    .dynamic_variables
                    .values()
                    .any(|(id, name, _)| id.0 == 148 || name.contains("edge1"));
                eprintln!(
                    "[BUG #71 RESTORE] After restore, has edge1/var_148? {}",
                    has_edge1_after
                );

                let cond = Box::new(cond);
                let then_expr = Box::new(then_expr);
                let else_expr = Box::new(else_expr);

                eprintln!("[IF_DEBUG] ✅ IF EXPRESSION CONVERSION COMPLETED SUCCESSFULLY");
                eprintln!("[IF_DEBUG] ========================================");
                Some(Expression::Conditional {
                    cond,
                    then_expr,
                    else_expr,
                })
            }
            hir::HirExpression::Match(match_expr) => {
                // Convert match-expression to nested conditionals
                // match x { 1 => a, 2 => b, _ => c } becomes: (x == 1) ? a : ((x == 2) ? b : c)
                eprintln!(
                    "[DEBUG] Match expression conversion: {} arms",
                    match_expr.arms.len()
                );
                for (i, arm) in match_expr.arms.iter().enumerate() {
                    eprintln!(
                        "[DEBUG] Match arm {}: pattern {:?}, expr type {:?}",
                        i,
                        arm.pattern,
                        std::mem::discriminant(&arm.expr)
                    );
                }
                self.convert_match_to_conditionals(&match_expr.expr, &match_expr.arms)
            }
            hir::HirExpression::Cast(cast_expr) => {
                // Convert type cast expression
                // Preserve the cast in MIR so codegen knows the intended type
                // For FP/bit reinterpretation casts, this is a no-op in hardware
                // but critical for type tracking
                let inner_expr = self.convert_expression(&cast_expr.expr)?;
                let mut target_type = self.convert_type(&cast_expr.target_type);

                // BUG #65/#66 FIX: Detect and correct erroneous Float16 casts from 32-bit values
                // If casting from a 32-bit type (Bit[32]/Nat[32]) to Float16, this is likely
                // a bug in HIR where fp32 was inferred as fp16. Correct it to Float32.
                if matches!(target_type, DataType::Float16) {
                    let inner_type = self.infer_expression_type_internal(&inner_expr, None);
                    let inner_width = match inner_type {
                        DataType::Bit(w)
                        | DataType::Nat(w)
                        | DataType::Int(w)
                        | DataType::Logic(w) => w,
                        _ => 0,
                    };
                    if inner_width == 32 {
                        eprintln!("[BUG #65/#66 FIX] Correcting Cast from {:?} (width={}) to Float16 → Float32", inner_type, inner_width);
                        target_type = DataType::Float32;
                    }
                }

                Some(Expression::Cast {
                    expr: Box::new(inner_expr),
                    target_type,
                })
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
                // Block expressions with statements cannot exist as pure expressions in hardware.
                // We hoist the statements to pending_statements, which will be emitted before
                // the parent assignment/statement.
                // Example: result = if cond { let x = f(); x } else { 0 }
                // Becomes: let x = f(); result = if cond { x } else { 0 }
                eprintln!(
                    "[DEBUG] Block expression: {} statements, result_expr type: {:?}",
                    statements.len(),
                    std::mem::discriminant(&**result_expr)
                );
                for stmt in statements {
                    if let Some(mir_stmt) = self.convert_statement(stmt) {
                        self.pending_statements.push(mir_stmt);
                    }
                }
                // Convert and return the final expression
                eprintln!(
                    "[DEBUG] Block expression: About to convert result_expr, type: {:?}",
                    std::mem::discriminant(&**result_expr)
                );
                eprintln!(
                    "[DEBUG] Block expression: Current dynamic_variables: {:?}",
                    self.dynamic_variables
                        .iter()
                        .map(|(_, (_, name, _))| name.as_str())
                        .collect::<Vec<_>>()
                );
                if let hir::HirExpression::Variable(var_id) = &**result_expr {
                    eprintln!(
                        "[DEBUG] Block expression: result_expr is Variable({:?})",
                        var_id
                    );
                    eprintln!(
                        "[DEBUG] Block expression: variable_map contains: {:?}",
                        self.variable_map.contains_key(var_id)
                    );
                    eprintln!(
                        "[DEBUG] Block expression: dynamic_variables contains: {:?}",
                        self.dynamic_variables.contains_key(var_id)
                    );
                    if let Some((mir_id, name, _)) = self.dynamic_variables.get(var_id) {
                        eprintln!("[DEBUG] Block expression: Found in dynamic_variables as {} (MIR ID {:?})", name, mir_id);
                    }
                }
                let result = self.convert_expression(result_expr);
                if result.is_none() {
                    eprintln!(
                        "[DEBUG] Block expression: result_expr conversion failed, type: {:?}",
                        std::mem::discriminant(&**result_expr)
                    );
                } else {
                    eprintln!(
                        "[DEBUG] Block expression: result_expr converted to MIR type: {:?}",
                        std::mem::discriminant(result.as_ref().unwrap())
                    );
                }
                result
            }
        }
    }

    /// Convert struct literal to packed bit vector expression
    fn convert_struct_literal(
        &mut self,
        type_name: &str,
        fields: &[hir::HirStructFieldInit],
    ) -> Option<Expression> {
        eprintln!(
            "[DEBUG] convert_struct_literal: type={}, {} fields",
            type_name,
            fields.len()
        );
        for (i, f) in fields.iter().enumerate() {
            eprintln!("  [{}] field: {}", i, f.name);
        }
        // Handle built-in vector types (vec2, vec3, vec4) specially
        // These use struct literal syntax: vec3 { x: a, y: b, z: c }
        // But they're built-in types, not user-defined structs
        if type_name == "vec2" || type_name == "vec3" || type_name == "vec4" {
            // For vec2/vec3/vec4, concatenate fields in order: { x, y, z }
            // Standard field order: x, y, z, w
            let field_order = ["x", "y", "z", "w"];
            let num_fields = match type_name {
                "vec2" => 2,
                "vec3" => 3,
                "vec4" => 4,
                _ => unreachable!(),
            };

            let mut field_exprs = Vec::new();
            for &field_name in field_order.iter().take(num_fields) {
                eprintln!(
                    "[DEBUG] convert_struct_literal: looking for field '{}'",
                    field_name
                );
                let field_init = fields.iter().find(|f| f.name == field_name);
                if field_init.is_none() {
                    eprintln!(
                        "[DEBUG] convert_struct_literal: field '{}' NOT FOUND, returning None",
                        field_name
                    );
                    return None;
                }
                let field_init = field_init.unwrap();
                let field_expr = self.convert_expression(&field_init.value)?;
                field_exprs.push(field_expr);
            }

            eprintln!(
                "[DEBUG] convert_struct_literal: SUCCESS, converted {} fields",
                field_exprs.len()
            );
            // Concatenate all fields: { x, y, z } becomes {x, y, z}
            return Some(Expression::Concat(field_exprs));
        }

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

        // BUG FIX #6: Use globally unique match ID instead of arm index to prevent
        // variable name collisions when multiple functions with match expressions
        // are inlined into the same entity
        let match_id = self.next_match_id;
        self.next_match_id += 1;

        // Set match arm prefix for the last arm to isolate its variables
        let last_arm_idx = arms.len() - 1;
        let last_arm_prefix = format!("match_{}_{}", match_id, last_arm_idx);
        self.match_arm_prefix = Some(last_arm_prefix);

        let last_expr = self.convert_expression(&arms.last()?.expr);

        // Clear the prefix after processing
        self.match_arm_prefix = None;

        let mut result = last_expr?;

        // Work backwards through the arms (excluding the last one which is the default)
        for (arm_idx, arm) in arms[..arms.len() - 1].iter().enumerate().rev() {
            // Build condition: match_value == pattern
            eprintln!(
                "[DEBUG] Match: processing arm with pattern {:?}",
                std::mem::discriminant(&arm.pattern)
            );
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
                hir::HirPattern::Path(enum_name, variant) => {
                    // BUG #33 FIX: Check if this is a constant pattern marked with "__CONST__"
                    if enum_name == "__CONST__" {
                        // This is a constant reference - resolve it to its value
                        if let Some(const_value) = self.resolve_constant_value(variant) {
                            let left = Box::new(match_value_expr.clone());
                            let right = Box::new(const_value);
                            Some(Expression::Binary {
                                op: BinaryOp::Equal,
                                left,
                                right,
                            })
                        } else {
                            None
                        }
                    } else {
                        // Regular enum pattern - resolve to the enum variant value
                        if let Some(variant_value) =
                            self.resolve_enum_variant_value(enum_name, variant)
                        {
                            let left = Box::new(match_value_expr.clone());
                            let right = Box::new(variant_value);
                            Some(Expression::Binary {
                                op: BinaryOp::Equal,
                                left,
                                right,
                            })
                        } else {
                            eprintln!(
                                "[DEBUG] Match: failed to resolve enum variant {}::{}",
                                enum_name, variant
                            );
                            None
                        }
                    }
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

            // BUG FIX #6: Use global match ID with arm index to make prefix unique
            let arm_prefix = format!("match_{}_{}", match_id, arm_idx);
            self.match_arm_prefix = Some(arm_prefix.clone());

            let arm_expr = self.convert_expression(&arm.expr);

            // Clear the prefix after processing this arm
            self.match_arm_prefix = None;

            if arm_expr.is_none() {
                eprintln!(
                    "[DEBUG] Match: FAILED to convert arm expression, type: {:?}",
                    std::mem::discriminant(&arm.expr)
                );
                return None;
            }
            result = Expression::Conditional {
                cond: Box::new(final_condition),
                then_expr: Box::new(arm_expr.unwrap()),
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

    /// Find a function by name in the current implementation or top-level functions
    fn find_function(&self, function_name: &str) -> Option<&hir::HirFunction> {
        let hir = self.hir?;

        // BUG #21 FIX: Handle module-qualified function names (e.g., "imported_funcs::process_data")
        // The HIR stores only the simple function name (e.g., "process_data"), not the full path.
        // Extract the simple name by taking the last component after "::"
        let simple_name = function_name.rsplit("::").next().unwrap_or(function_name);

        // First, try to find in current implementation's functions
        if let Some(entity_id) = self.current_entity_id {
            if let Some(impl_block) = hir
                .implementations
                .iter()
                .find(|impl_b| impl_b.entity == entity_id)
            {
                for func in &impl_block.functions {
                    if func.name == simple_name {
                        return Some(func);
                    }
                }
            }
        }

        // Second, try top-level functions
        hir.functions.iter().find(|func| func.name == simple_name)
    }

    /// Transform early returns into nested if-else expressions
    ///
    /// Converts:
    ///   if cond { return val1; }
    ///   stmt2;
    ///   return val2;
    ///
    /// Into:
    ///   if cond { return val1; } else { stmt2; return val2; }
    fn transform_early_returns(&self, body: Vec<hir::HirStatement>) -> Vec<hir::HirStatement> {
        self.transform_early_returns_recursive(body)
    }

    #[allow(clippy::only_used_in_recursion)]
    fn transform_early_returns_recursive(
        &self,
        body: Vec<hir::HirStatement>,
    ) -> Vec<hir::HirStatement> {
        if body.is_empty() {
            return body;
        }

        // Find the first statement that's an If with a return in then-branch
        for (i, stmt) in body.iter().enumerate() {
            if let hir::HirStatement::If(if_stmt) = stmt {
                // Check if then-branch ends with a return
                if let Some(last) = if_stmt.then_statements.last() {
                    if matches!(last, hir::HirStatement::Return(_)) {
                        // Found early return pattern!
                        // Collect all statements before this if
                        let mut result = body[..i].to_vec();

                        // Transform this if statement
                        let remaining_stmts = body[i + 1..].to_vec();

                        let new_if = if remaining_stmts.is_empty() {
                            // No statements after, keep as-is but recursively transform branches
                            hir::HirStatement::If(hir::HirIfStatement {
                                condition: if_stmt.condition.clone(),
                                then_statements: self.transform_early_returns_recursive(
                                    if_stmt.then_statements.clone(),
                                ),
                                else_statements: if_stmt.else_statements.as_ref().map(|stmts| {
                                    self.transform_early_returns_recursive(stmts.clone())
                                }),
                            })
                        } else {
                            // Move remaining statements into else-branch
                            let else_body = self.transform_early_returns_recursive(remaining_stmts);

                            hir::HirStatement::If(hir::HirIfStatement {
                                condition: if_stmt.condition.clone(),
                                then_statements: self.transform_early_returns_recursive(
                                    if_stmt.then_statements.clone(),
                                ),
                                else_statements: Some(else_body),
                            })
                        };

                        result.push(new_if);
                        return result;
                    }
                }
            }
        }

        // No early returns found, recursively transform nested structures
        body.into_iter()
            .map(|stmt| match stmt {
                hir::HirStatement::If(if_stmt) => hir::HirStatement::If(hir::HirIfStatement {
                    condition: if_stmt.condition,
                    then_statements: self
                        .transform_early_returns_recursive(if_stmt.then_statements),
                    else_statements: if_stmt
                        .else_statements
                        .map(|stmts| self.transform_early_returns_recursive(stmts)),
                }),
                hir::HirStatement::Block(stmts) => {
                    hir::HirStatement::Block(self.transform_early_returns_recursive(stmts))
                }
                // TODO: Handle Match statements with early returns
                other => other,
            })
            .collect()
    }

    /// Convert a statement-based function body (possibly with if-returns) into an expression
    /// This handles the case where early returns have been transformed into nested if-else
    fn convert_body_to_expression(&self, body: &[hir::HirStatement]) -> Option<hir::HirExpression> {
        if body.is_empty() {
            eprintln!("[DEBUG] convert_body_to_expression: empty body");
            return None;
        }

        eprintln!(
            "[DEBUG] convert_body_to_expression: {} statements",
            body.len()
        );
        for (i, stmt) in body.iter().enumerate() {
            eprintln!("[DEBUG]   stmt[{}]: {:?}", i, std::mem::discriminant(stmt));
        }

        // Collect all let bindings
        let mut let_bindings = Vec::new();
        let mut remaining_stmts = body;

        // Extract leading let statements
        while let Some(hir::HirStatement::Let(let_stmt)) = remaining_stmts.first() {
            let_bindings.push(let_stmt.clone());
            remaining_stmts = &remaining_stmts[1..];
        }

        eprintln!(
            "[DEBUG] convert_body_to_expression: {} let bindings, {} remaining stmts",
            let_bindings.len(),
            remaining_stmts.len()
        );

        if remaining_stmts.is_empty() {
            // All statements are let bindings - check for implicit return
            // If the last let binding's value is an expression without semicolon,
            // treat it as an implicit return
            if let Some(last_let) = let_bindings.last() {
                eprintln!(
                    "[DEBUG] convert_body_to_expression: treating last let '{}' as implicit return",
                    last_let.name
                );
                // Take all but the last let binding for the block,
                // and use the last let's value as the return expression
                let block_stmts: Vec<_> = let_bindings
                    .iter()
                    .take(let_bindings.len() - 1)
                    .map(|let_stmt| hir::HirStatement::Let(let_stmt.clone()))
                    .collect();

                if block_stmts.is_empty() {
                    // Only one let binding - return its value directly
                    return Some(last_let.value.clone());
                } else {
                    // Multiple let bindings - wrap in block
                    return Some(hir::HirExpression::Block {
                        statements: block_stmts,
                        result_expr: Box::new(last_let.value.clone()),
                    });
                }
            }
            eprintln!(
                "[DEBUG] convert_body_to_expression: no remaining statements and no let bindings!"
            );
            return None;
        }

        // Convert the remaining statements to an expression
        let body_expr = match remaining_stmts {
            // Single return statement with expression
            [hir::HirStatement::Return(Some(expr))] => Some(expr.clone()),

            // Single return statement without expression (void return or implicit unit)
            // In hardware context, this shouldn't happen for non-void functions
            [hir::HirStatement::Return(None)] => {
                eprintln!("Warning: Return statement with no expression in function body");
                eprintln!("  This may indicate an issue with HIR building");
                None
            }

            // If statement (possibly with returns in branches)
            [hir::HirStatement::If(if_stmt)] => self.convert_if_stmt_to_expr(if_stmt),

            // Match expression as final statement - convert to HIR match expression
            [hir::HirStatement::Match(match_stmt)] => self.convert_match_stmt_to_expr(match_stmt),

            // Multiple statements - need to convert to block expression
            // This happens when function has statements followed by return
            stmts if stmts.len() > 1 => {
                // Check if last statement is a return
                if let Some(hir::HirStatement::Return(Some(return_expr))) = stmts.last() {
                    // Build block with all statements except the last, then use return expr as result
                    let block_stmts: Vec<_> = stmts[..stmts.len() - 1].to_vec();
                    Some(hir::HirExpression::Block {
                        statements: block_stmts,
                        result_expr: Box::new(return_expr.clone()),
                    })
                } else {
                    eprintln!(
                        "convert_body_to_expression: multiple statements without final return"
                    );
                    eprintln!("  Statements: {:?}", stmts.len());
                    None
                }
            }

            // Single expression statement (implicit return)
            [hir::HirStatement::Expression(expr)] => Some(expr.clone()),

            // Other cases not yet supported
            _ => {
                eprintln!("convert_body_to_expression: unsupported statement pattern");
                eprintln!("  Remaining statements: {:?}", remaining_stmts.len());
                if !remaining_stmts.is_empty() {
                    eprintln!(
                        "  First statement type: {:?}",
                        std::mem::discriminant(&remaining_stmts[0])
                    );
                    match &remaining_stmts[0] {
                        hir::HirStatement::Return(opt) => {
                            eprintln!("    Return statement with expr: {}", opt.is_some());
                        }
                        other => eprintln!("    Statement variant: {:?}", other),
                    }
                }
                None
            }
        }?;

        // Wrap with let bindings if any
        if let_bindings.is_empty() {
            Some(body_expr)
        } else {
            Some(self.build_let_expression(let_bindings, body_expr))
        }
    }

    /// Convert an if-statement (with returns in branches) to an if-expression
    fn convert_if_stmt_to_expr(&self, if_stmt: &hir::HirIfStatement) -> Option<hir::HirExpression> {
        eprintln!(
            "[BUG #7] convert_if_stmt_to_expr: condition type = {:?}",
            std::mem::discriminant(&if_stmt.condition)
        );
        if let hir::HirExpression::Call(call) = &if_stmt.condition {
            eprintln!(
                "[BUG #7]   condition is Call: function={}, args={}",
                call.function,
                call.args.len()
            );
        }
        if let hir::HirExpression::Variable(var_id) = &if_stmt.condition {
            eprintln!("[BUG #7]   condition is Variable: {:?}", var_id);
        }

        // Recursively convert then-branch
        let then_expr = self.convert_body_to_expression(&if_stmt.then_statements)?;

        // Recursively convert else-branch
        let else_expr = if let Some(else_stmts) = &if_stmt.else_statements {
            self.convert_body_to_expression(else_stmts)?
        } else {
            eprintln!("convert_if_stmt_to_expr: if-statement missing else branch");
            return None;
        };

        let result = hir::HirExpression::If(hir::HirIfExpr {
            condition: Box::new(if_stmt.condition.clone()),
            then_expr: Box::new(then_expr),
            else_expr: Box::new(else_expr),
        });

        eprintln!(
            "[BUG #7] convert_if_stmt_to_expr: result condition type = {:?}",
            std::mem::discriminant(&if_stmt.condition)
        );

        Some(result)
    }

    /// Convert a match-statement to a match-expression
    fn convert_match_stmt_to_expr(
        &self,
        match_stmt: &hir::HirMatchStatement,
    ) -> Option<hir::HirExpression> {
        // Convert each arm from statement form to expression form
        let arms: Option<Vec<_>> = match_stmt
            .arms
            .iter()
            .map(|arm| {
                // Convert arm's statements to an expression
                let expr = self.convert_body_to_expression(&arm.statements)?;
                Some(hir::HirMatchArmExpr {
                    pattern: arm.pattern.clone(),
                    guard: arm.guard.clone(),
                    expr,
                })
            })
            .collect();

        let arms = arms?;

        Some(hir::HirExpression::Match(hir::HirMatchExpr {
            expr: Box::new(match_stmt.expr.clone()),
            arms,
        }))
    }

    /// Build a let expression from a list of let bindings and a body expression
    /// Uses Block expression syntax since HIR doesn't have a Let variant in HirExpression
    fn build_let_expression(
        &self,
        bindings: Vec<hir::HirLetStatement>,
        body: hir::HirExpression,
    ) -> hir::HirExpression {
        // Build a block expression with the let statements and result expression
        let statements = bindings.into_iter().map(hir::HirStatement::Let).collect();

        hir::HirExpression::Block {
            statements,
            result_expr: Box::new(body),
        }
    }

    /// Extract return expression from a function body
    /// Phase 2: Simple functions with single return
    /// Phase 3: Functions with let bindings + return
    fn extract_return_expression<'a>(
        &self,
        body: &'a [hir::HirStatement],
    ) -> Option<&'a hir::HirExpression> {
        // Body must be non-empty and end with return
        if body.is_empty() {
            eprintln!("Function inlining: empty function body");
            return None;
        }

        // Last statement must be a return
        match body.last() {
            Some(hir::HirStatement::Return(Some(expr))) => Some(expr),
            Some(hir::HirStatement::Return(None)) => {
                eprintln!("Function inlining: Return statement has None expression");
                None
            }
            Some(hir::HirStatement::Expression(expr)) => {
                // For functions with implicit returns (last expression)
                eprintln!("Function inlining: treating last expression as implicit return");
                Some(expr)
            }
            last_stmt => {
                eprintln!("Function inlining: function must end with return statement");
                eprintln!("  Body length: {}", body.len());
                eprintln!("  Last statement: {:?}", last_stmt);
                None
            }
        }
    }

    /// Substitute parameters in an expression with argument expressions
    /// This creates a parameter -> argument mapping and replaces all parameter references
    fn substitute_expression(
        &mut self,
        expr: &hir::HirExpression,
        param_map: &HashMap<String, &hir::HirExpression>,
    ) -> Option<hir::HirExpression> {
        match expr {
            // GenericParam - function parameters are parsed as generic params
            // Check if this matches a function parameter and substitute
            hir::HirExpression::GenericParam(name) => {
                if let Some(arg_expr) = param_map.get(name) {
                    // Clone the argument expression
                    Some((*arg_expr).clone())
                } else {
                    // Not a function parameter, keep as-is
                    Some(expr.clone())
                }
            }

            // Parameter reference - replace with argument expression
            hir::HirExpression::Variable(var_id) => {
                // Look up the variable name in HIR
                let _hir = self.hir?;

                // Check if this variable is actually a function parameter
                // We need to find its name first
                let var_name = self.find_variable_name(*var_id)?;

                if let Some(arg_expr) = param_map.get(&var_name) {
                    // Clone the argument expression
                    Some((*arg_expr).clone())
                } else {
                    // Not a parameter, keep as-is
                    Some(expr.clone())
                }
            }

            // Binary expression - substitute both sides
            hir::HirExpression::Binary(binary) => {
                let left = Box::new(self.substitute_expression(&binary.left, param_map)?);
                let right = Box::new(self.substitute_expression(&binary.right, param_map)?);
                Some(hir::HirExpression::Binary(hir::HirBinaryExpr {
                    left,
                    op: binary.op.clone(),
                    right,
                }))
            }

            // Unary expression - substitute operand
            hir::HirExpression::Unary(unary) => {
                let operand = Box::new(self.substitute_expression(&unary.operand, param_map)?);
                Some(hir::HirExpression::Unary(hir::HirUnaryExpr {
                    op: unary.op.clone(),
                    operand,
                }))
            }

            // If expression - substitute all parts
            hir::HirExpression::If(if_expr) => {
                let condition =
                    Box::new(self.substitute_expression(&if_expr.condition, param_map)?);
                let then_expr =
                    Box::new(self.substitute_expression(&if_expr.then_expr, param_map)?);
                let else_expr =
                    Box::new(self.substitute_expression(&if_expr.else_expr, param_map)?);
                Some(hir::HirExpression::If(hir::HirIfExpr {
                    condition,
                    then_expr,
                    else_expr,
                }))
            }

            // For other expressions that don't contain parameters, clone as-is
            hir::HirExpression::Literal(_)
            | hir::HirExpression::Signal(_)
            | hir::HirExpression::Port(_)
            | hir::HirExpression::Constant(_) => Some(expr.clone()),

            // Recursively handle other expression types as needed
            _ => {
                // For unhandled cases, clone as-is (may need expansion for full support)
                Some(expr.clone())
            }
        }
    }

    /// Substitute parameters and local variables in an expression with argument expressions
    /// This version takes a var_id_to_name map for looking up function-local let bindings
    fn substitute_expression_with_var_map(
        &mut self,
        expr: &hir::HirExpression,
        param_map: &HashMap<String, &hir::HirExpression>,
        var_id_to_name: &HashMap<hir::VariableId, String>,
    ) -> Option<hir::HirExpression> {
        let is_block = matches!(expr, hir::HirExpression::Block { .. });
        eprintln!(
            "[DEBUG] substitute_expression_with_var_map: expr type: {:?}, is_block: {}",
            std::mem::discriminant(expr),
            is_block
        );
        match expr {
            // GenericParam - function parameters are parsed as generic params
            // Check if this matches a function parameter and substitute
            hir::HirExpression::GenericParam(name) => {
                if let Some(arg_expr) = param_map.get(name) {
                    // Clone the argument expression
                    eprintln!(
                        "[DEBUG] GenericParam substitution: '{}' -> {:?}",
                        name,
                        std::mem::discriminant(&**arg_expr)
                    );
                    Some((*arg_expr).clone())
                } else {
                    // Not a function parameter, keep as-is
                    eprintln!(
                        "[DEBUG] GenericParam substitution: '{}' NOT FOUND in param_map, keeping as-is",
                        name
                    );
                    Some(expr.clone())
                }
            }

            // Variable reference - replace with mapped expression
            hir::HirExpression::Variable(var_id) => {
                // Look up variable name using the provided map (for function-local variables)
                if let Some(var_name) = var_id_to_name.get(var_id) {
                    if let Some(arg_expr) = param_map.get(var_name) {
                        // Found in substitution map, replace with expression
                        // BUG #20 FIX: Don't inline match expressions during substitution
                        // Match expressions should be converted once and the variable reference preserved
                        // Otherwise, the match gets converted multiple times (once per use), causing:
                        // 1. Duplicate conversions with different match IDs
                        // 2. Variables from earlier conversions not accessible in later ones
                        // 3. Premature exits from recursive conversion
                        if let hir::HirExpression::Match(_) = &**arg_expr {
                            return Some(expr.clone()); // Keep as Variable reference
                        }

                        // BUG #20 FIX: Also don't inline Block expressions that might contain matches
                        if let hir::HirExpression::Block { .. } = &**arg_expr {
                            return Some(expr.clone()); // Keep as Variable reference
                        }

                        // For simple expressions, safe to inline
                        let cloned = (*arg_expr).clone();
                        return Some(cloned);
                    }
                }

                // Fall back to global variable lookup
                if let Some(var_name) = self.find_variable_name(*var_id) {
                    if let Some(arg_expr) = param_map.get(&var_name) {
                        eprintln!(
                            "[DEBUG] Variable substitution (global): var_name={}, expr type: {:?}",
                            var_name,
                            std::mem::discriminant(&**arg_expr)
                        );
                        if let hir::HirExpression::Match(m) = &**arg_expr {
                            eprintln!(
                                "[DEBUG] Variable (global): substituting Match with {} arms",
                                m.arms.len()
                            );
                        }
                        let cloned = (*arg_expr).clone();
                        if let hir::HirExpression::Match(m) = &cloned {
                            eprintln!(
                                "[DEBUG] Variable (global): cloned Match has {} arms",
                                m.arms.len()
                            );
                        }
                        return Some(cloned);
                    } else {
                        eprintln!(
                            "[DEBUG] Variable (global): name '{}' not found in param_map",
                            var_name
                        );
                    }
                }

                // Not in substitution map, keep as-is
                Some(expr.clone())
            }

            // Binary expression - substitute both sides
            hir::HirExpression::Binary(binary) => {
                if let hir::HirExpression::Variable(left_id) = &*binary.left {}
                if let hir::HirExpression::Variable(right_id) = &*binary.right {}

                let left = Box::new(self.substitute_expression_with_var_map(
                    &binary.left,
                    param_map,
                    var_id_to_name,
                )?);
                if let hir::HirExpression::Variable(left_id) = &*left {}

                let right = Box::new(self.substitute_expression_with_var_map(
                    &binary.right,
                    param_map,
                    var_id_to_name,
                )?);
                if let hir::HirExpression::Variable(right_id) = &*right {}

                Some(hir::HirExpression::Binary(hir::HirBinaryExpr {
                    left,
                    op: binary.op.clone(),
                    right,
                }))
            }

            // Unary expression - substitute operand
            hir::HirExpression::Unary(unary) => {
                let operand = Box::new(self.substitute_expression_with_var_map(
                    &unary.operand,
                    param_map,
                    var_id_to_name,
                )?);
                Some(hir::HirExpression::Unary(hir::HirUnaryExpr {
                    op: unary.op.clone(),
                    operand,
                }))
            }

            // If expression - substitute all parts
            hir::HirExpression::If(if_expr) => {
                if let hir::HirExpression::Binary(bin) = &*if_expr.condition {
                    if let hir::HirExpression::Variable(left_id) = &*bin.left {}
                }

                let condition = Box::new(self.substitute_expression_with_var_map(
                    &if_expr.condition,
                    param_map,
                    var_id_to_name,
                )?);

                if let hir::HirExpression::Binary(bin) = &*condition {
                    if let hir::HirExpression::Variable(left_id) = &*bin.left {}
                }

                let then_expr = Box::new(self.substitute_expression_with_var_map(
                    &if_expr.then_expr,
                    param_map,
                    var_id_to_name,
                )?);
                let else_expr = Box::new(self.substitute_expression_with_var_map(
                    &if_expr.else_expr,
                    param_map,
                    var_id_to_name,
                )?);
                Some(hir::HirExpression::If(hir::HirIfExpr {
                    condition,
                    then_expr,
                    else_expr,
                }))
            }

            // Match expression - substitute all parts
            hir::HirExpression::Match(match_expr) => {
                eprintln!(
                    "[DEBUG] substitute_expression_with_var_map: Match expression with {} arms",
                    match_expr.arms.len()
                );

                // Substitute in the match expression
                let expr = Box::new(self.substitute_expression_with_var_map(
                    &match_expr.expr,
                    param_map,
                    var_id_to_name,
                )?);

                // Substitute in each arm
                let mut arms = Vec::new();
                for (i, arm) in match_expr.arms.iter().enumerate() {
                    eprintln!(
                        "[DEBUG] Match: processing arm {}, pattern: {:?}",
                        i, arm.pattern
                    );

                    let guard = if let Some(guard_expr) = &arm.guard {
                        self.substitute_expression_with_var_map(
                            guard_expr,
                            param_map,
                            var_id_to_name,
                        )
                    } else {
                        None
                    };

                    let arm_expr_result = self.substitute_expression_with_var_map(
                        &arm.expr,
                        param_map,
                        var_id_to_name,
                    );

                    if arm_expr_result.is_none() {
                        eprintln!(
                            "[DEBUG] Match arm {}: arm expression substitution FAILED",
                            i
                        );
                        return None;
                    }
                    let arm_expr = arm_expr_result?;
                    eprintln!(
                        "[DEBUG] Match arm {}: arm expression substitution succeeded",
                        i
                    );

                    arms.push(hir::HirMatchArmExpr {
                        pattern: arm.pattern.clone(),
                        guard,
                        expr: arm_expr,
                    });
                }

                eprintln!(
                    "[DEBUG] Match: substitution complete, final arms count: {}",
                    arms.len()
                );
                Some(hir::HirExpression::Match(hir::HirMatchExpr { expr, arms }))
            }

            // Block expression - substitute statements and result
            hir::HirExpression::Block {
                statements,
                result_expr,
            } => {
                eprintln!(
                    "[DEBUG] **MATCHED** Block substitution: {} statements",
                    statements.len()
                );
                // BUG FIX #18: Use owned HashMap for local variables to allow incremental updates
                // This allows each statement to reference previously defined local variables
                let mut local_var_map: std::collections::HashMap<String, hir::HirExpression> =
                    std::collections::HashMap::new();

                // Substitute all statements in the block
                let mut substituted_statements = Vec::new();
                for (i, stmt) in statements.iter().enumerate() {
                    match stmt {
                        hir::HirStatement::Let(let_stmt) => {
                            eprintln!(
                                "[DEBUG] Block: substituting let statement {} ({})",
                                i, let_stmt.name
                            );
                            // Build a combined map with both params and local vars for substitution
                            let mut combined_map: std::collections::HashMap<
                                String,
                                &hir::HirExpression,
                            > = param_map.clone();
                            for (name, expr) in &local_var_map {
                                combined_map.insert(name.clone(), expr);
                            }

                            let substituted_value = self.substitute_expression_with_var_map(
                                &let_stmt.value,
                                &combined_map,
                                var_id_to_name,
                            )?;
                            if let hir::HirExpression::Match(m) = &substituted_value {
                                eprintln!(
                                    "[DEBUG] Block: let {} = Match with {} arms",
                                    let_stmt.name,
                                    m.arms.len()
                                );
                            }
                            substituted_statements.push(hir::HirStatement::Let(
                                hir::HirLetStatement {
                                    id: let_stmt.id,
                                    name: let_stmt.name.clone(),
                                    mutable: let_stmt.mutable, // Bug #78: Preserve mutability
                                    value: substituted_value.clone(),
                                    var_type: let_stmt.var_type.clone(),
                                },
                            ));
                            // Add this variable to local map immediately for subsequent statements
                            // Use the actual substituted value (e.g., StructLiteral) instead of a Variable reference
                            // Example: let ray_dir = vec3{x,y,z}; vec_dot(ray_dir, ...) should inline the vec3 literal
                            local_var_map.insert(let_stmt.name.clone(), substituted_value);
                            eprintln!(
                                "[DEBUG] Block: added {} (id {:?}) to local_var_map immediately",
                                let_stmt.name, let_stmt.id
                            );
                        }
                        _ => {
                            substituted_statements.push(stmt.clone());
                        }
                    }
                }

                // Build final combined map for result expression
                let mut combined_map: std::collections::HashMap<String, &hir::HirExpression> =
                    param_map.clone();
                for (name, expr) in &local_var_map {
                    combined_map.insert(name.clone(), expr);
                }

                let substituted_result = Box::new(self.substitute_expression_with_var_map(
                    result_expr,
                    &combined_map,
                    var_id_to_name,
                )?);

                eprintln!(
                    "[DEBUG] Block substitution complete: {} statements",
                    substituted_statements.len()
                );
                Some(hir::HirExpression::Block {
                    statements: substituted_statements,
                    result_expr: substituted_result,
                })
            }

            // Tuple literal - substitute each element
            hir::HirExpression::TupleLiteral(elements) => {
                eprintln!(
                    "[DEBUG] TupleLiteral substitution: {} elements",
                    elements.len()
                );
                let mut substituted_elements = Vec::new();
                for (i, elem) in elements.iter().enumerate() {
                    eprintln!(
                        "[DEBUG] TupleLiteral: substituting element {}, type: {:?}",
                        i,
                        std::mem::discriminant(elem)
                    );
                    let substituted =
                        self.substitute_expression_with_var_map(elem, param_map, var_id_to_name)?;
                    if let hir::HirExpression::Match(m) = &substituted {
                        eprintln!(
                            "[DEBUG] TupleLiteral element {}: substituted to Match with {} arms",
                            i,
                            m.arms.len()
                        );
                    }
                    substituted_elements.push(substituted);
                }
                Some(hir::HirExpression::TupleLiteral(substituted_elements))
            }

            // Range expression - substitute base, high, and low
            hir::HirExpression::Range(base, high, low) => {
                eprintln!(
                    "[DEBUG] Range substitution: base type {:?}",
                    std::mem::discriminant(&**base)
                );
                let substituted_base = Box::new(self.substitute_expression_with_var_map(
                    base,
                    param_map,
                    var_id_to_name,
                )?);
                let substituted_high = Box::new(self.substitute_expression_with_var_map(
                    high,
                    param_map,
                    var_id_to_name,
                )?);
                let substituted_low = Box::new(self.substitute_expression_with_var_map(
                    low,
                    param_map,
                    var_id_to_name,
                )?);
                Some(hir::HirExpression::Range(
                    substituted_base,
                    substituted_high,
                    substituted_low,
                ))
            }

            // Index expression - substitute base and index
            hir::HirExpression::Index(base, index) => {
                eprintln!(
                    "[DEBUG] Index substitution: base type {:?}",
                    std::mem::discriminant(&**base)
                );
                let substituted_base = Box::new(self.substitute_expression_with_var_map(
                    base,
                    param_map,
                    var_id_to_name,
                )?);
                let substituted_index = Box::new(self.substitute_expression_with_var_map(
                    index,
                    param_map,
                    var_id_to_name,
                )?);
                Some(hir::HirExpression::Index(
                    substituted_base,
                    substituted_index,
                ))
            }

            // Call expression - substitute arguments
            hir::HirExpression::Call(call) => {
                eprintln!(
                    "[DEBUG] Call substitution: function={}, args={} BEFORE substitution",
                    call.function,
                    call.args.len()
                );
                for (i, arg) in call.args.iter().enumerate() {
                    eprintln!("  arg[{}]: type={:?}", i, std::mem::discriminant(arg));
                }

                let mut substituted_args = Vec::new();
                for (i, arg) in call.args.iter().enumerate() {
                    eprintln!("[DEBUG] Call substitution: substituting arg[{}]", i);
                    let substituted_arg =
                        self.substitute_expression_with_var_map(arg, param_map, var_id_to_name)?;
                    eprintln!(
                        "[DEBUG] Call substitution: arg[{}] substituted successfully, type={:?}",
                        i,
                        std::mem::discriminant(&substituted_arg)
                    );
                    substituted_args.push(substituted_arg);
                }
                eprintln!(
                    "[DEBUG] Call substitution: function={}, {} args AFTER substitution",
                    call.function,
                    substituted_args.len()
                );
                Some(hir::HirExpression::Call(hir::HirCallExpr {
                    function: call.function.clone(),
                    args: substituted_args,
                }))
            }

            // For other expressions that don't contain parameters, clone as-is
            hir::HirExpression::Literal(_)
            | hir::HirExpression::Signal(_)
            | hir::HirExpression::Port(_)
            | hir::HirExpression::Constant(_) => Some(expr.clone()),

            // Cast expression - substitute the inner expression
            hir::HirExpression::Cast(cast) => {
                eprintln!(
                    "[DEBUG] Cast substitution: inner expr type: {:?}, target type: {:?}",
                    std::mem::discriminant(&*cast.expr),
                    cast.target_type
                );
                let substituted_expr = Box::new(self.substitute_expression_with_var_map(
                    &cast.expr,
                    param_map,
                    var_id_to_name,
                )?);
                eprintln!("[DEBUG] Cast substitution: successfully substituted inner expr");
                Some(hir::HirExpression::Cast(hir::HirCastExpr {
                    expr: substituted_expr,
                    target_type: cast.target_type.clone(),
                }))
            }

            // Concat expression - substitute all elements
            hir::HirExpression::Concat(elements) => {
                eprintln!("[DEBUG] Concat substitution: {} elements", elements.len());
                let mut substituted_elements = Vec::new();
                for (i, elem) in elements.iter().enumerate() {
                    eprintln!(
                        "[DEBUG] Concat: substituting element {}, type: {:?}",
                        i,
                        std::mem::discriminant(elem)
                    );
                    let substituted =
                        self.substitute_expression_with_var_map(elem, param_map, var_id_to_name)?;
                    substituted_elements.push(substituted);
                }
                eprintln!(
                    "[DEBUG] Concat substitution: successfully substituted all {} elements",
                    substituted_elements.len()
                );
                Some(hir::HirExpression::Concat(substituted_elements))
            }

            // FieldAccess expression - substitute base expression
            hir::HirExpression::FieldAccess { base, field } => {
                eprintln!(
                    "[DEBUG] FieldAccess substitution: field={}, base type before: {:?}",
                    field,
                    std::mem::discriminant(&**base)
                );
                let substituted_base = Box::new(self.substitute_expression_with_var_map(
                    base,
                    param_map,
                    var_id_to_name,
                )?);
                eprintln!(
                    "[DEBUG] FieldAccess substitution: successfully substituted base, type after: {:?}",
                    std::mem::discriminant(&*substituted_base)
                );
                Some(hir::HirExpression::FieldAccess {
                    base: substituted_base,
                    field: field.clone(),
                })
            }

            // StructLiteral expression - substitute field values
            hir::HirExpression::StructLiteral(struct_lit) => {
                eprintln!(
                    "[DEBUG] StructLiteral substitution: type={}, {} fields BEFORE",
                    struct_lit.type_name,
                    struct_lit.fields.len()
                );
                let mut substituted_fields = Vec::new();
                for (i, field_init) in struct_lit.fields.iter().enumerate() {
                    eprintln!(
                        "[DEBUG] StructLiteral field[{}]: name={}, value type={:?}",
                        i,
                        field_init.name,
                        std::mem::discriminant(&field_init.value)
                    );
                    let substituted_value = self.substitute_expression_with_var_map(
                        &field_init.value,
                        param_map,
                        var_id_to_name,
                    )?;
                    substituted_fields.push(hir::HirStructFieldInit {
                        name: field_init.name.clone(),
                        value: substituted_value,
                    });
                }
                eprintln!(
                    "[DEBUG] StructLiteral substitution: type={}, {} fields AFTER",
                    struct_lit.type_name,
                    substituted_fields.len()
                );
                Some(hir::HirExpression::StructLiteral(hir::HirStructLiteral {
                    type_name: struct_lit.type_name.clone(),
                    fields: substituted_fields,
                }))
            }

            // Recursively handle other expression types as needed
            _ => {
                // For unhandled cases, clone as-is (may need expansion for full support)
                let variant_name = match expr {
                    hir::HirExpression::Literal(_) => "Literal",
                    hir::HirExpression::Signal(_) => "Signal",
                    hir::HirExpression::Port(_) => "Port",
                    hir::HirExpression::Variable(_) => "Variable",
                    hir::HirExpression::Constant(_) => "Constant",
                    hir::HirExpression::GenericParam(_) => "GenericParam",
                    hir::HirExpression::Binary(_) => "Binary",
                    hir::HirExpression::Unary(_) => "Unary",
                    hir::HirExpression::Call(_) => "Call",
                    hir::HirExpression::Index(_, _) => "Index",
                    hir::HirExpression::Range(_, _, _) => "Range",
                    hir::HirExpression::FieldAccess { .. } => "FieldAccess",
                    hir::HirExpression::EnumVariant { .. } => "EnumVariant",
                    hir::HirExpression::AssociatedConstant { .. } => "AssociatedConstant",
                    hir::HirExpression::ArrayRepeat { .. } => "ArrayRepeat",
                    hir::HirExpression::Concat(_) => "Concat",
                    hir::HirExpression::Ternary { .. } => "Ternary",
                    hir::HirExpression::StructLiteral(_) => "StructLiteral",
                    hir::HirExpression::TupleLiteral(_) => "TupleLiteral",
                    hir::HirExpression::ArrayLiteral(_) => "ArrayLiteral",
                    hir::HirExpression::If(_) => "If",
                    hir::HirExpression::Match(_) => "Match",
                    hir::HirExpression::Cast(_) => "Cast",
                    hir::HirExpression::Block { .. } => "Block",
                };
                eprintln!(
                    "[DEBUG] substitute_expression_with_var_map: unhandled expression type: {:?} ({})",
                    std::mem::discriminant(expr),
                    variant_name
                );
                Some(expr.clone())
            }
        }
    }

    /// Find a variable name by ID (helper for parameter substitution)
    fn find_variable_name(&self, var_id: hir::VariableId) -> Option<String> {
        let hir = self.hir?;

        // Check current implementation's variables
        if let Some(entity_id) = self.current_entity_id {
            if let Some(impl_block) = hir
                .implementations
                .iter()
                .find(|impl_b| impl_b.entity == entity_id)
            {
                for var in &impl_block.variables {
                    if var.id == var_id {
                        return Some(var.name.clone());
                    }
                }
            }
        }

        // Check dynamic variables (from let bindings)
        if let Some((_, name, _)) = self.dynamic_variables.get(&var_id) {
            return Some(name.clone());
        }

        None
    }

    /// Check if a function body contains a recursive call (Phase 5)
    fn contains_recursive_call(&self, body: &[hir::HirStatement], func_name: &str) -> bool {
        for stmt in body {
            match stmt {
                hir::HirStatement::Return(Some(expr)) => {
                    if self.expression_contains_call(expr, func_name) {
                        return true;
                    }
                }
                hir::HirStatement::Let(let_stmt) => {
                    if self.expression_contains_call(&let_stmt.value, func_name) {
                        return true;
                    }
                }
                _ => {}
            }
        }
        false
    }

    /// Check if an expression contains a call to a specific function
    #[allow(clippy::only_used_in_recursion)]
    fn expression_contains_call(&self, expr: &hir::HirExpression, func_name: &str) -> bool {
        match expr {
            hir::HirExpression::Call(call) => {
                if call.function == func_name {
                    return true;
                }
                // Check arguments for nested calls
                for arg in &call.args {
                    if self.expression_contains_call(arg, func_name) {
                        return true;
                    }
                }
                false
            }
            hir::HirExpression::Binary(bin) => {
                self.expression_contains_call(&bin.left, func_name)
                    || self.expression_contains_call(&bin.right, func_name)
            }
            hir::HirExpression::Unary(un) => self.expression_contains_call(&un.operand, func_name),
            hir::HirExpression::If(if_expr) => {
                self.expression_contains_call(&if_expr.condition, func_name)
                    || self.expression_contains_call(&if_expr.then_expr, func_name)
                    || self.expression_contains_call(&if_expr.else_expr, func_name)
            }
            hir::HirExpression::Match(match_expr) => {
                if self.expression_contains_call(&match_expr.expr, func_name) {
                    return true;
                }
                for arm in &match_expr.arms {
                    if let Some(guard) = &arm.guard {
                        if self.expression_contains_call(guard, func_name) {
                            return true;
                        }
                    }
                    if self.expression_contains_call(&arm.expr, func_name) {
                        return true;
                    }
                }
                false
            }
            _ => false,
        }
    }

    /// Inline a function call (Phases 2-5: simple functions, let bindings, control flow, recursion check)
    /// Recursively collect all let binding variable IDs and names from statements and expressions
    fn collect_let_bindings(
        &self,
        stmts: &[hir::HirStatement],
        map: &mut HashMap<hir::VariableId, String>,
    ) {
        for stmt in stmts {
            match stmt {
                hir::HirStatement::Let(let_stmt) => {
                    map.insert(let_stmt.id, let_stmt.name.clone());
                    // Recursively collect from the let's value expression
                    self.collect_let_bindings_from_expr(&let_stmt.value, map);
                }
                hir::HirStatement::Return(Some(expr)) => {
                    self.collect_let_bindings_from_expr(expr, map);
                }
                hir::HirStatement::If(if_stmt) => {
                    self.collect_let_bindings(&if_stmt.then_statements, map);
                    if let Some(else_stmts) = &if_stmt.else_statements {
                        self.collect_let_bindings(else_stmts, map);
                    }
                }
                _ => {}
            }
        }
    }

    /// Recursively collect let bindings from an expression
    fn collect_let_bindings_from_expr(
        &self,
        expr: &hir::HirExpression,
        map: &mut HashMap<hir::VariableId, String>,
    ) {
        match expr {
            hir::HirExpression::Block {
                statements,
                result_expr,
            } => {
                self.collect_let_bindings(statements, map);
                self.collect_let_bindings_from_expr(result_expr, map);
            }
            hir::HirExpression::Match(match_expr) => {
                for arm in &match_expr.arms {
                    self.collect_let_bindings_from_expr(&arm.expr, map);
                }
            }
            hir::HirExpression::If(if_expr) => {
                self.collect_let_bindings_from_expr(&if_expr.condition, map);
                self.collect_let_bindings_from_expr(&if_expr.then_expr, map);
                self.collect_let_bindings_from_expr(&if_expr.else_expr, map);
            }
            hir::HirExpression::Binary(bin_expr) => {
                self.collect_let_bindings_from_expr(&bin_expr.left, map);
                self.collect_let_bindings_from_expr(&bin_expr.right, map);
            }
            hir::HirExpression::Unary(un_expr) => {
                self.collect_let_bindings_from_expr(&un_expr.operand, map);
            }
            _ => {}
        }
    }

    /// Convert an FP method call to MIR expression
    /// This is a helper to handle FP methods when type-based detection fails
    fn convert_fp_method_call(&mut self, call: &hir::HirCallExpr) -> Option<Expression> {
        match call.function.as_str() {
            // Binary FP operations
            "add" if call.args.len() == 2 => {
                let left = Box::new(self.convert_expression(&call.args[0])?);
                let right = Box::new(self.convert_expression(&call.args[1])?);
                Some(Expression::Binary {
                    op: BinaryOp::FAdd,
                    left,
                    right,
                })
            }
            "sub" if call.args.len() == 2 => {
                let left = Box::new(self.convert_expression(&call.args[0])?);
                let right = Box::new(self.convert_expression(&call.args[1])?);
                Some(Expression::Binary {
                    op: BinaryOp::FSub,
                    left,
                    right,
                })
            }
            "mul" if call.args.len() == 2 => {
                let left = Box::new(self.convert_expression(&call.args[0])?);
                let right = Box::new(self.convert_expression(&call.args[1])?);
                Some(Expression::Binary {
                    op: BinaryOp::FMul,
                    left,
                    right,
                })
            }
            "div" if call.args.len() == 2 => {
                let left = Box::new(self.convert_expression(&call.args[0])?);
                let right = Box::new(self.convert_expression(&call.args[1])?);
                Some(Expression::Binary {
                    op: BinaryOp::FDiv,
                    left,
                    right,
                })
            }
            // Unary FP operations
            "neg" if call.args.len() == 1 => {
                // Negate is handled as unary minus (0 - x)
                let zero = Box::new(Expression::Literal(Value::Float(0.0)));
                let operand = Box::new(self.convert_expression(&call.args[0])?);
                Some(Expression::Binary {
                    op: BinaryOp::FSub,
                    left: zero,
                    right: operand,
                })
            }
            "abs" if call.args.len() == 1 => {
                // abs is handled as a function call to fabs
                let arg = self.convert_expression(&call.args[0])?;
                Some(Expression::FunctionCall {
                    name: "fabs".to_string(),
                    args: vec![arg],
                })
            }
            "sqrt" if call.args.len() == 1 => {
                let operand = Box::new(self.convert_expression(&call.args[0])?);
                Some(Expression::Unary {
                    op: UnaryOp::FSqrt,
                    operand,
                })
            }
            // Comparison FP operations
            "lt" if call.args.len() == 2 => {
                let left = Box::new(self.convert_expression(&call.args[0])?);
                let right = Box::new(self.convert_expression(&call.args[1])?);
                Some(Expression::Binary {
                    op: BinaryOp::FLess,
                    left,
                    right,
                })
            }
            "gt" if call.args.len() == 2 => {
                let left = Box::new(self.convert_expression(&call.args[0])?);
                let right = Box::new(self.convert_expression(&call.args[1])?);
                Some(Expression::Binary {
                    op: BinaryOp::FGreater,
                    left,
                    right,
                })
            }
            "le" if call.args.len() == 2 => {
                let left = Box::new(self.convert_expression(&call.args[0])?);
                let right = Box::new(self.convert_expression(&call.args[1])?);
                Some(Expression::Binary {
                    op: BinaryOp::FLessEqual,
                    left,
                    right,
                })
            }
            "ge" if call.args.len() == 2 => {
                let left = Box::new(self.convert_expression(&call.args[0])?);
                let right = Box::new(self.convert_expression(&call.args[1])?);
                Some(Expression::Binary {
                    op: BinaryOp::FGreaterEqual,
                    left,
                    right,
                })
            }
            "eq" if call.args.len() == 2 => {
                let left = Box::new(self.convert_expression(&call.args[0])?);
                let right = Box::new(self.convert_expression(&call.args[1])?);
                Some(Expression::Binary {
                    op: BinaryOp::FEqual,
                    left,
                    right,
                })
            }
            "ne" if call.args.len() == 2 => {
                let left = Box::new(self.convert_expression(&call.args[0])?);
                let right = Box::new(self.convert_expression(&call.args[1])?);
                Some(Expression::Binary {
                    op: BinaryOp::FNotEqual,
                    left,
                    right,
                })
            }
            _ => {
                eprintln!(
                    "[DEBUG] convert_fp_method_call: Unknown FP method '{}' with {} args",
                    call.function,
                    call.args.len()
                );
                None
            }
        }
    }

    /// Inline a function call and return the substituted HIR expression (before MIR conversion)
    /// This is used when we need to perform HIR-level operations on the inlined result
    /// (e.g., field access on struct-returning functions)
    fn inline_function_to_hir(&mut self, call: &hir::HirCallExpr) -> Option<hir::HirExpression> {
        // Find the function
        let func = self.find_function(&call.function)?;

        // Clone the data we need
        let params = func.params.clone();
        let body = func.body.clone();

        // Transform early returns
        let body = self.transform_early_returns(body);

        // Check for recursion
        if self.contains_recursive_call(&body, &call.function) {
            eprintln!(
                "Error: Recursive function calls are not supported: function '{}'",
                call.function
            );
            return None;
        }

        // Check arity
        if params.len() != call.args.len() {
            eprintln!(
                "Function {}: expected {} arguments, got {}",
                call.function,
                params.len(),
                call.args.len()
            );
            return None;
        }

        // Build parameter substitution map
        let mut substitution_map = HashMap::new();
        for (param, arg) in params.iter().zip(&call.args) {
            substitution_map.insert(param.name.clone(), arg.clone());
        }

        // Build var_id to name mapping
        let mut var_id_to_name = HashMap::new();
        self.collect_let_bindings(&body, &mut var_id_to_name);

        // Convert body to expression
        let body_expr = self.convert_body_to_expression(&body)?;

        // Substitute parameters
        let substituted_expr = self.substitute_expression_with_var_map(
            &body_expr,
            &substitution_map
                .iter()
                .map(|(k, v)| (k.clone(), v))
                .collect(),
            &var_id_to_name,
        )?;

        // Return the substituted HIR expression (don't convert to MIR)
        Some(substituted_expr)
    }

    fn inline_function_call(&mut self, call: &hir::HirCallExpr) -> Option<Expression> {
        eprintln!(
            "[DEBUG] inline_function_call: {} with {} args",
            call.function,
            call.args.len()
        );

        // BUG FIX #69: Don't attempt to inline FP built-in methods
        // FP methods should be handled by convert_expression's FP method detection.
        // If we reach here for an FP method, treat it as an FP operation directly.
        // This handles cases where type inference fails during nested inlining.
        let is_binary_fp_method = matches!(
            call.function.as_str(),
            "add" | "sub" | "mul" | "div" | "lt" | "gt" | "le" | "ge" | "eq" | "ne"
        ) && call.args.len() == 2;

        let is_unary_fp_method =
            matches!(call.function.as_str(), "neg" | "abs" | "sqrt") && call.args.len() == 1;

        if is_binary_fp_method || is_unary_fp_method {
            eprintln!(
                "[DEBUG] inline_function_call: Detected FP method '{}' with {} args, handling as FP operation",
                call.function,
                call.args.len()
            );
            // Assume this is an FP method call and convert it directly
            // This bypasses the normal FP detection which may fail with complex expressions
            return self.convert_fp_method_call(call);
        }

        // Step 1: Find the function and clone its body to avoid borrow checker issues
        let func = self.find_function(&call.function)?;
        eprintln!(
            "[DEBUG] inline_function_call: found function {}, body has {} stmts",
            call.function,
            func.body.len()
        );

        // Clone the data we need before doing mutable operations
        let params = func.params.clone();
        let body = func.body.clone();

        eprintln!(
            "[DEBUG] inline_function_call: original body has {} statements",
            body.len()
        );
        for (i, stmt) in body.iter().enumerate() {
            if let hir::HirStatement::Let(let_stmt) = stmt {
                eprintln!("  [{}] Let: {}", i, let_stmt.name);
            } else {
                eprintln!("  [{}] {:?}", i, std::mem::discriminant(stmt));
            }
        }

        // Transform early returns into nested if-else before processing
        let body = self.transform_early_returns(body);

        eprintln!(
            "[DEBUG] inline_function_call: after transform_early_returns, body has {} statements",
            body.len()
        );
        for (i, stmt) in body.iter().enumerate() {
            if let hir::HirStatement::Let(let_stmt) = stmt {
                eprintln!("  [{}] Let: {}", i, let_stmt.name);
            } else {
                eprintln!("  [{}] {:?}", i, std::mem::discriminant(stmt));
            }
        }

        // Phase 5: Check for direct recursion
        if self.contains_recursive_call(&body, &call.function) {
            eprintln!(
                "Error: Recursive function calls are not supported: function '{}'",
                call.function
            );
            return None;
        }

        // Step 2: Check arity
        if params.len() != call.args.len() {
            eprintln!(
                "Function {}: expected {} arguments, got {}",
                call.function,
                params.len(),
                call.args.len()
            );
            return None;
        }

        // Step 3: Build initial parameter substitution map (function parameters -> arguments)
        let mut substitution_map = HashMap::new();
        for (param, arg) in params.iter().zip(&call.args) {
            substitution_map.insert(param.name.clone(), arg.clone());
        }
        eprintln!(
            "[DEBUG] inline_function_call: built param map with {} entries",
            substitution_map.len()
        );
        // Debug: Show what's in the param map
        for (param_name, arg_expr) in &substitution_map {
            eprintln!(
                "[DEBUG] inline_function_call: param '{}' -> {:?}",
                param_name,
                std::mem::discriminant(arg_expr)
            );
        }

        // Step 4: Build var_id -> name mapping from ALL let statements (including nested ones)
        // This allows us to look up variable names during substitution
        let mut var_id_to_name = HashMap::new();
        self.collect_let_bindings(&body, &mut var_id_to_name);
        eprintln!(
            "[DEBUG] inline_function_call: built var_id map with {} entries (recursive)",
            var_id_to_name.len()
        );

        // Step 5: Convert statement-based body to expression (handles early returns)
        eprintln!("[DEBUG] inline_function_call: About to convert_body_to_expression");
        let body_expr = self.convert_body_to_expression(&body);
        if body_expr.is_none() {
            eprintln!(
                "[DEBUG] inline_function_call: convert_body_to_expression FAILED - returning None"
            );
            return None;
        }
        let body_expr = body_expr?;
        eprintln!("[DEBUG] inline_function_call: convert_body_to_expression succeeded");

        // Step 6: Substitute parameters in the entire expression (including nested let bindings)
        eprintln!("[DEBUG] inline_function_call: About to substitute_expression_with_var_map");
        let substituted_expr = self.substitute_expression_with_var_map(
            &body_expr,
            &substitution_map
                .iter()
                .map(|(k, v)| (k.clone(), v))
                .collect(),
            &var_id_to_name,
        );
        if substituted_expr.is_none() {
            eprintln!("[DEBUG] inline_function_call: substitute_expression_with_var_map FAILED - returning None");
            return None;
        }
        let substituted_expr = substituted_expr?;
        eprintln!(
            "[DEBUG] inline_function_call: successfully substituted return, converting to MIR"
        );

        // Step 7: Convert the substituted HIR expression to MIR
        let expr_type_name = match &substituted_expr {
            hir::HirExpression::Literal(_) => "Literal",
            hir::HirExpression::Variable(_) => "Variable",
            hir::HirExpression::Binary(_) => "Binary",
            hir::HirExpression::Unary(_) => "Unary",
            hir::HirExpression::Call(_) => "Call",
            hir::HirExpression::Signal(_) => "Signal",
            hir::HirExpression::Port(_) => "Port",
            hir::HirExpression::Index(_, _) => "Index",
            hir::HirExpression::Concat(_) => "Concat",
            hir::HirExpression::Range(_, _, _) => "Range",
            hir::HirExpression::If(_) => "If",
            hir::HirExpression::Match(_) => "Match",
            hir::HirExpression::Cast(_) => "Cast",
            hir::HirExpression::Block { .. } => "Block",
            hir::HirExpression::FieldAccess { .. } => "FieldAccess",
            hir::HirExpression::ArrayLiteral { .. } => "ArrayLiteral",
            _ => "Other",
        };
        eprintln!(
            "[DEBUG] inline_function_call: '{}' substituted expression type: {}",
            call.function, expr_type_name
        );
        let result = self.convert_expression(&substituted_expr);
        if result.is_none() {
            eprintln!(
                "[DEBUG] inline_function_call: '{}' FAILED to convert substituted expression to MIR (type: {})",
                call.function,
                expr_type_name
            );
        } else {
            eprintln!(
                "[DEBUG] inline_function_call: '{}' successfully converted to MIR",
                call.function
            );
        }

        // Step 8: Clean up function-local variables from variable_map to prevent scope leakage
        // Function-local variables should not persist across different function inlining contexts.
        // Without this cleanup, variables from one match arm can incorrectly be reused in another arm.
        //
        // BUG #71 FIX: BUT don't remove variables that have been preserved to dynamic_variables!
        // These variables (preserved by BUG #68) need to persist for use in pending statements.
        for var_id in var_id_to_name.keys() {
            // Only remove if NOT in dynamic_variables (not preserved)
            if !self.dynamic_variables.contains_key(var_id) {
                self.variable_map.remove(var_id);
            }
        }

        result
    }

    /// Helper to convert expression to lvalue (for index/range operations)
    fn expr_to_lvalue(&mut self, expr: &hir::HirExpression) -> Option<LValue> {
        match expr {
            hir::HirExpression::Signal(id) => self.signal_map.get(id).map(|&id| LValue::Signal(id)),
            hir::HirExpression::Port(id) => self.port_map.get(id).map(|&id| LValue::Port(id)),
            hir::HirExpression::Variable(id) => {
                // BUG #20 FIX: Check variable_map first, then fall back to dynamic_variables
                // Dynamic variables are created during function inlining (e.g., in match arm blocks)
                // and must be accessible for Index/Range operations.
                //
                // Example: In Karythra's exec_l0_l1, match arm 9 creates:
                //   let shift_amt = b[4:0];
                //   if sign && shift_amt > 0 { ... }
                // The condition references shift_amt, which is only in dynamic_variables.
                if let Some(&mir_id) = self.variable_map.get(id) {
                    return Some(LValue::Variable(mir_id));
                }

                // Fall back to dynamic_variables (for let-bound variables from inlined functions)
                if let Some((mir_id, _, _)) = self.dynamic_variables.get(id) {
                    return Some(LValue::Variable(*mir_id));
                }

                None
            }
            hir::HirExpression::Range(base, high, low) => {
                // BUG #77 FIX: Support nested range operations (e.g., data[31:0][4:0])
                let base_lval = self.expr_to_lvalue(base)?;
                let high_expr = self.convert_expression(high)?;
                let low_expr = self.convert_expression(low)?;
                Some(LValue::RangeSelect {
                    base: Box::new(base_lval),
                    high: Box::new(high_expr),
                    low: Box::new(low_expr),
                })
            }
            hir::HirExpression::Index(base, index) => {
                // BUG #77 FIX: Support nested index operations (e.g., arr[i][j])
                let base_lval = self.expr_to_lvalue(base)?;
                let index_expr = self.convert_expression(index)?;
                Some(LValue::BitSelect {
                    base: Box::new(base_lval),
                    index: Box::new(index_expr),
                })
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
                if let Some(entity_id) = self.current_entity_id {
                    if let Some(impl_block) =
                        hir.implementations.iter().find(|i| i.entity == entity_id)
                    {
                        if let Some(var) = impl_block.variables.iter().find(|v| v.id == *var_id) {
                            return Some(var.var_type.clone());
                        }
                    }
                }

                // Fall back to dynamic_variables (for let-bound variables from inlined functions)
                if let Some((_, _, var_type)) = self.dynamic_variables.get(var_id) {
                    return Some(var_type.clone());
                }

                None
            }
            hir::HirExpression::GenericParam(name) => {
                // WORKAROUND for Bug #42/43: Parser creates malformed cast expression AST,
                // causing some variable references to appear as GenericParam instead of Variable
                // Try to resolve the name to a variable (should not be needed after parser fix)

                // BUG FIX #7: First check dynamic_variables (from inlined functions)
                if let Some((_, _, var_type)) = self
                    .dynamic_variables
                    .values()
                    .find(|(_, var_name, _)| var_name == name)
                {
                    return Some(var_type.clone());
                }

                // Then check entity implementation variables
                let entity_id = self.current_entity_id?;
                let impl_block = hir.implementations.iter().find(|i| i.entity == entity_id)?;
                if let Some(var) = impl_block.variables.iter().find(|v| v.name == *name) {
                    return Some(var.var_type.clone());
                }
                None
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
            hir::HirExpression::Cast(cast) => {
                // Cast expression: return the target type
                Some(cast.target_type.clone())
            }
            hir::HirExpression::Call(call) => {
                // BUG FIX #7: Infer type from function return type
                // This is needed for chained method calls like vec_dot(a, b).add(c)
                // where vec_dot returns fp32, enabling type inference for the add method

                // CRITICAL FIX: Check if this is an FP method call FIRST
                // FP methods like mul, add, sub, div should return the same type as their receiver
                if !call.args.is_empty() {
                    // Check if receiver (arg[0]) is a float type
                    if let Some(receiver_type) = self.infer_hir_type(&call.args[0]) {
                        if self.is_float_type(&receiver_type) {
                            // FP binary methods return the same float type as receiver
                            if matches!(call.function.as_str(), "add" | "sub" | "mul" | "div") {
                                return Some(receiver_type);
                            }
                            // FP comparison methods return bit
                            if matches!(
                                call.function.as_str(),
                                "lt" | "gt" | "le" | "ge" | "eq" | "ne"
                            ) {
                                return Some(hir::HirType::Bit(1));
                            }
                            // FP unary methods return the same float type
                            if matches!(call.function.as_str(), "sqrt" | "abs") {
                                return Some(receiver_type);
                            }
                        }
                    }
                }

                // Fall back to looking up regular function
                if let Some(func) = self.find_function(&call.function) {
                    func.return_type.clone()
                } else {
                    None
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
            eprintln!(
                "[DEBUG] convert_binary_op: op={:?}, left_expr type={:?}, is_fp={}",
                op,
                ty,
                self.is_float_type(&ty)
            );
            self.is_float_type(&ty)
        } else {
            eprintln!(
                "[DEBUG] convert_binary_op: op={:?}, left_expr type=None, is_fp=false",
                op
            );
            false
        };

        match op {
            hir::HirBinaryOp::Add => {
                if is_fp {
                    eprintln!("[DEBUG] convert_binary_op: Returning BinaryOp::FAdd");
                    BinaryOp::FAdd
                } else {
                    eprintln!("[DEBUG] convert_binary_op: Returning BinaryOp::Add");
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
            hir::HirType::String => {
                panic!("String type is not synthesizable and cannot be used in hardware modules. String types are only supported in testbench contexts.");
            }
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
                                2 => {
                                    eprintln!(
                                        "[BUG #71 CONVERT_TYPE] Custom(\"{}\") -> Vec2(Float32)",
                                        name
                                    );
                                    return DataType::Vec2(Box::new(DataType::Float32));
                                }
                                3 => {
                                    eprintln!(
                                        "[BUG #71 CONVERT_TYPE] Custom(\"{}\") -> Vec3(Float32)",
                                        name
                                    );
                                    return DataType::Vec3(Box::new(DataType::Float32));
                                }
                                4 => {
                                    eprintln!(
                                        "[BUG #71 CONVERT_TYPE] Custom(\"{}\") -> Vec4(Float32)",
                                        name
                                    );
                                    return DataType::Vec4(Box::new(DataType::Float32));
                                }
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

    /// Infer the MIR DataType of a MIR Expression
    /// Infer expression type with access to module for variable type lookup
    /// This version is used when we have access to the partially-built module
    fn infer_expression_type_with_module(&self, expr: &Expression, module: &Module) -> DataType {
        self.infer_expression_type_internal(expr, Some(module))
    }

    /// Infer expression type without module access (fallback for legacy code)
    /// This is used for variables created from pending statements where we don't have HIR type info
    #[allow(clippy::only_used_in_recursion)]
    fn infer_expression_type(&self, expr: &Expression) -> DataType {
        self.infer_expression_type_internal(expr, None)
    }

    /// Internal implementation of expression type inference
    /// module_opt: Optional module for looking up variable types
    #[allow(clippy::only_used_in_recursion)]
    fn infer_expression_type_internal(
        &self,
        expr: &Expression,
        module_opt: Option<&Module>,
    ) -> DataType {
        match expr {
            Expression::Literal(value) => match value {
                Value::Integer(_) => DataType::Int(32),
                Value::Float(_) => DataType::Float32,
                Value::BitVector { width, .. } => DataType::Bit(*width),
                Value::String(_) => DataType::Nat(32), // Fallback
                Value::HighZ => DataType::Logic(1),
                Value::Unknown => DataType::Logic(1), // Unknown value
            },
            Expression::Ref(lval) => {
                // Look up the signal/port/variable type
                match lval {
                    LValue::Signal(sig_id) => {
                        // TODO: Look up signal type from mir.signals
                        // For now, default to 32-bit natural
                        DataType::Nat(32)
                    }
                    LValue::Port(port_id) => {
                        // TODO: Look up port type
                        DataType::Nat(32)
                    }
                    LValue::Variable(var_id) => {
                        // BUG #65/#66 FIX: Look up variable type from module if available
                        if let Some(module) = module_opt {
                            module
                                .variables
                                .iter()
                                .find(|v| v.id == *var_id)
                                .map(|v| v.var_type.clone())
                                .unwrap_or(DataType::Nat(32))
                        } else {
                            // Fallback when no module available
                            DataType::Nat(32)
                        }
                    }
                    LValue::BitSelect { .. } => DataType::Bit(1),
                    LValue::RangeSelect { high, low, .. } => {
                        // Width is (high - low + 1) - but these are expressions, so estimate
                        DataType::Nat(32)
                    }
                    LValue::Concat(_) => DataType::Nat(32),
                }
            }
            Expression::Binary { op, left, right } => {
                // BUG #65/#66 FIX: For FP binary operations, infer Float32 if operands are Float types
                // Check if this is an FP operation (Add, Sub, Mul, Div)
                if matches!(
                    op,
                    BinaryOp::Add
                        | BinaryOp::Sub
                        | BinaryOp::Mul
                        | BinaryOp::Div
                        | BinaryOp::FAdd
                        | BinaryOp::FSub
                        | BinaryOp::FMul
                        | BinaryOp::FDiv
                ) {
                    let left_type = self.infer_expression_type_internal(left, module_opt);
                    let right_type = self.infer_expression_type_internal(right, module_opt);

                    // If either operand is a Float type, result is Float
                    if matches!(
                        left_type,
                        DataType::Float16 | DataType::Float32 | DataType::Float64
                    ) || matches!(
                        right_type,
                        DataType::Float16 | DataType::Float32 | DataType::Float64
                    ) {
                        // Use the wider type
                        match (left_type, right_type) {
                            (DataType::Float64, _) | (_, DataType::Float64) => DataType::Float64,
                            (DataType::Float32, _) | (_, DataType::Float32) => DataType::Float32,
                            (DataType::Float16, DataType::Float16) => DataType::Float16,
                            _ => DataType::Float32, // Fallback
                        }
                    } else {
                        DataType::Nat(32) // Integer arithmetic
                    }
                } else {
                    DataType::Nat(32) // Other binary ops
                }
            }
            Expression::Unary { .. } => DataType::Nat(32),
            Expression::Conditional { then_expr, .. } => {
                // Infer from then branch
                self.infer_expression_type_internal(then_expr, module_opt)
            }
            Expression::FunctionCall { .. } => DataType::Nat(32), // Default for function calls
            Expression::Concat(_) => DataType::Nat(32),
            Expression::Replicate { .. } => DataType::Nat(32),
            Expression::Cast { target_type, .. } => target_type.clone(),
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

    /// Convert MIR DataType back to HIR HirType
    /// BUG FIX #67: Needed to store inferred types in dynamic_variables
    ///
    /// Note: &self is only used in recursive calls, which is intentional for this converter
    #[allow(clippy::only_used_in_recursion)]
    fn convert_mir_to_hir_type(&self, mir_type: &DataType) -> hir::HirType {
        match mir_type {
            DataType::Bit(width) => hir::HirType::Bit(*width as u32),
            DataType::Bool => hir::HirType::Bool,
            DataType::Logic(width) => hir::HirType::Logic(*width as u32),
            DataType::Int(width) => hir::HirType::Int(*width as u32),
            DataType::Nat(width) => hir::HirType::Nat(*width as u32),
            DataType::Clock { .. } => hir::HirType::Clock(None),
            DataType::Reset { .. } => hir::HirType::Reset {
                polarity: hir::HirResetPolarity::ActiveHigh,
                clock_domain: None,
            },
            DataType::Event => hir::HirType::Event,
            DataType::Array(inner, size) => {
                hir::HirType::Array(Box::new(self.convert_mir_to_hir_type(inner)), *size as u32)
            }
            DataType::Float16 => hir::HirType::Float16,
            DataType::Float32 => hir::HirType::Float32,
            DataType::Float64 => hir::HirType::Float64,
            DataType::Vec2(element) => {
                hir::HirType::Vec2(Box::new(self.convert_mir_to_hir_type(element)))
            }
            DataType::Vec3(element) => {
                hir::HirType::Vec3(Box::new(self.convert_mir_to_hir_type(element)))
            }
            DataType::Vec4(element) => {
                hir::HirType::Vec4(Box::new(self.convert_mir_to_hir_type(element)))
            }
            // Parametric types - use default values
            DataType::BitParam { default, .. } => hir::HirType::Bit(*default as u32),
            DataType::LogicParam { default, .. } => hir::HirType::Logic(*default as u32),
            DataType::IntParam { default, .. } => hir::HirType::Int(*default as u32),
            DataType::NatParam { default, .. } => hir::HirType::Nat(*default as u32),
            DataType::BitExpr { default, .. } => hir::HirType::Bit(*default as u32),
            DataType::LogicExpr { default, .. } => hir::HirType::Logic(*default as u32),
            DataType::IntExpr { default, .. } => hir::HirType::Int(*default as u32),
            DataType::NatExpr { default, .. } => hir::HirType::Nat(*default as u32),
            // For complex types, try to convert back properly
            DataType::Struct(struct_type) => {
                // Check if this is an anonymous tuple struct (name starts with "__tuple_")
                if struct_type.name.starts_with("__tuple_") {
                    // Convert back to HIR Tuple type
                    let element_types: Vec<hir::HirType> = struct_type
                        .fields
                        .iter()
                        .map(|field| self.convert_mir_to_hir_type(&field.field_type))
                        .collect();
                    hir::HirType::Tuple(element_types)
                } else {
                    // Named struct - convert to HIR struct type
                    let fields: Vec<hir::HirStructField> = struct_type
                        .fields
                        .iter()
                        .map(|field| hir::HirStructField {
                            name: field.name.clone(),
                            field_type: self.convert_mir_to_hir_type(&field.field_type),
                        })
                        .collect();
                    hir::HirType::Struct(hir::HirStructType {
                        name: struct_type.name.clone(),
                        fields,
                        packed: struct_type.packed,
                    })
                }
            }
            DataType::Enum(_) => hir::HirType::Nat(32),
            DataType::Union(_) => hir::HirType::Nat(32),
        }
    }

    /// Convert struct/vector field access to bit slice
    fn convert_field_access(
        &mut self,
        base: &hir::HirExpression,
        field_name: &str,
    ) -> Option<Expression> {
        eprintln!("[BUG #71 TUPLE FIELD ACCESS] convert_field_access called:");
        eprintln!("[BUG #71 TUPLE FIELD ACCESS]   field_name='{}'", field_name);
        eprintln!(
            "[BUG #71 TUPLE FIELD ACCESS]   base expr type={:?}",
            std::mem::discriminant(base)
        );

        // Map numeric field names (tuple indices) to struct field names
        // e.g., "0" -> "_0", "1" -> "_1", "2" -> "_2"
        let normalized_field_name = if field_name.chars().all(|c| c.is_ascii_digit()) {
            eprintln!(
                "[BUG #71 TUPLE FIELD ACCESS]   Normalized '{}' -> '_{}'",
                field_name, field_name
            );
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
                    // BUG FIX #3: Check context-aware map first when in match arm context
                    let var_id_mir = if let Some(ref prefix) = self.match_arm_prefix {
                        let context_key = (Some(prefix.clone()), *var_id);
                        self.context_variable_map.get(&context_key).copied()
                    } else {
                        None
                    }
                    .or_else(|| self.variable_map.get(var_id).copied())
                    .or_else(|| {
                        // Check dynamic_variables for let bindings from inlined functions
                        self.dynamic_variables
                            .get(var_id)
                            .map(|(mir_id, _, _)| *mir_id)
                    });

                    if let Some(var_id_mir) = var_id_mir {
                        let base_lval = LValue::Variable(var_id_mir);
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
                hir::HirExpression::StructLiteral(struct_lit) => {
                    // BUG FIX #70: Handle field access on struct literals
                    // This occurs when function parameters get substituted with struct literal expressions
                    // Example: vec_dot(ray_dir, ray_dir) where ray_dir is vec3{x:..., y:..., z:...}
                    //          After substitution: vec3{x:..., y:..., z:...}.x
                    //
                    // Extract the specific field from the struct literal and convert just that field
                    for field_init in &struct_lit.fields {
                        if field_init.name == field_name {
                            return self.convert_expression(&field_init.value);
                        }
                    }
                    eprintln!(
                        "[DEBUG] FieldAccess on StructLiteral: field '{}' not found in struct '{}'",
                        field_name, struct_lit.type_name
                    );
                    return None;
                }
                hir::HirExpression::TupleLiteral(elements) => {
                    // BUG FIX #22: Handle field access on tuple literals
                    // This occurs when function inlining creates tuple returns that are then destructured
                    // Example: let (rx, ry, rz) = vec3_add(...) where vec3_add returns (x, y, z)
                    // After inlining: let rx = (x, y, z).0, let ry = (x, y, z).1, etc.
                    //
                    // Parse the field name as a tuple index (e.g., "0", "1", "2")
                    if let Ok(index) = field_name.parse::<usize>() {
                        if index < elements.len() {
                            eprintln!(
                                "[BUG #71 TUPLE] ✅ FieldAccess on TupleLiteral: extracting element {} of {} elements",
                                index, elements.len()
                            );
                            eprintln!(
                                "[BUG #71 TUPLE]   Element type: {:?}",
                                std::mem::discriminant(&elements[index])
                            );
                            let result = self.convert_expression(&elements[index]);
                            if result.is_some() {
                                eprintln!("[BUG #71 TUPLE]   ✅ Successfully extracted element");
                            } else {
                                eprintln!("[BUG #71 TUPLE]   ❌ Failed to extract element");
                            }
                            return result;
                        } else {
                            eprintln!(
                                "[BUG #71 TUPLE] ❌ FieldAccess on TupleLiteral: index {} out of bounds (len={})",
                                index, elements.len()
                            );
                            return None;
                        }
                    } else {
                        eprintln!(
                            "[BUG #71 TUPLE] ❌ FieldAccess on TupleLiteral: field '{}' is not a valid index",
                            field_name
                        );
                        return None;
                    }
                }
                hir::HirExpression::GenericParam(param_name) => {
                    // BUG FIX #70 Part 2: Handle field access on GenericParam (unsubstituted parameter)
                    // This occurs when parameter substitution fails or is incomplete during function inlining
                    // Try to look up the parameter in dynamic_variables
                    let var_id_mir = self
                        .dynamic_variables
                        .values()
                        .find(|(_, name, _)| name == param_name)
                        .map(|(mir_id, _, _)| *mir_id);

                    if let Some(var_id_mir) = var_id_mir {
                        let base_lval = LValue::Variable(var_id_mir);
                        let (high_bit, low_bit) =
                            self.get_field_bit_range(current_base, &normalized_field_name)?;
                        let high_expr = Expression::Literal(Value::Integer(high_bit as i64));
                        let low_expr = Expression::Literal(Value::Integer(low_bit as i64));
                        return Some(Expression::Ref(LValue::RangeSelect {
                            base: Box::new(base_lval),
                            high: Box::new(high_expr),
                            low: Box::new(low_expr),
                        }));
                    }
                    eprintln!(
                        "[DEBUG] FieldAccess on GenericParam '{}': not found in dynamic_variables",
                        param_name
                    );
                    return None;
                }
                hir::HirExpression::Call(call) => {
                    // BUG FIX #74: Handle field access on function call results
                    // Example: vec_cross(a, b).x where vec_cross returns a struct
                    // Inline the function to get HIR expression (before MIR conversion)
                    let inlined_hir_expr = self.inline_function_to_hir(call)?;

                    // Create new FieldAccess with inlined expression as base
                    let field_access = hir::HirExpression::FieldAccess {
                        base: Box::new(inlined_hir_expr),
                        field: field_name.to_string(),
                    };

                    // Recursively convert - this will hit StructLiteral handler if applicable
                    return self.convert_expression(&field_access);
                }
                hir::HirExpression::Block {
                    statements,
                    result_expr,
                } => {
                    // BUG FIX #22: Handle field access on Block expressions
                    // This occurs when function inlining creates Block expressions that return tuples/structs
                    // Example: let (rx, ry, rz) = vec3_add(...) where vec3_add inlines to a Block
                    // After inlining: let rx = <Block>.0, let ry = <Block>.1, etc.
                    //
                    // Strategy: Access the field directly on the result expression of the Block
                    // This avoids converting the entire Block to MIR prematurely
                    eprintln!(
                        "[DEBUG] FieldAccess on Block: accessing field '{}' on result_expr (type: {:?})",
                        field_name,
                        std::mem::discriminant(&**result_expr)
                    );

                    // Create new FieldAccess with the Block's result expression as base
                    let field_access = hir::HirExpression::FieldAccess {
                        base: result_expr.clone(),
                        field: field_name.to_string(),
                    };

                    // Recursively convert the field access
                    // This will hit the appropriate handler (StructLiteral, TupleLiteral, etc.)
                    return self.convert_expression(&field_access);
                }
                hir::HirExpression::If(if_expr) => {
                    // BUG FIX #22: Handle field access on If expressions
                    // This occurs when function inlining creates If expressions that return tuples/structs
                    // Example: let rx = (if cond { tuple1 } else { tuple2 }).0
                    // After inlining: if cond { tuple1.0 } else { tuple2.0 }
                    //
                    // Strategy: Push the field access into both branches of the If
                    eprintln!(
                        "[DEBUG] FieldAccess on If: accessing field '{}', pushing into both branches",
                        field_name
                    );

                    // Create FieldAccess for then branch
                    let then_field_access = hir::HirExpression::FieldAccess {
                        base: if_expr.then_expr.clone(),
                        field: field_name.to_string(),
                    };

                    // Create FieldAccess for else branch
                    let else_field_access = hir::HirExpression::FieldAccess {
                        base: if_expr.else_expr.clone(),
                        field: field_name.to_string(),
                    };

                    // Create new If expression with field access in both branches
                    let new_if = hir::HirExpression::If(hir::HirIfExpr {
                        condition: if_expr.condition.clone(),
                        then_expr: Box::new(then_field_access),
                        else_expr: Box::new(else_field_access),
                    });

                    // Convert the new If expression
                    return self.convert_expression(&new_if);
                }
                _ => {
                    // Complex base - can't handle
                    eprintln!(
                        "[DEBUG] FieldAccess on unsupported base type: {:?}",
                        std::mem::discriminant(current_base)
                    );
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
        eprintln!("[BUG #70 FIELD RANGE] get_field_bit_range called: field_name='{}', base expr discriminant={:?}", field_name, std::mem::discriminant(base));
        // First, check if this is a vector type
        if let Some(base_type) = self.infer_hir_type(base) {
            eprintln!(
                "[BUG #70 FIELD RANGE] Inferred base type: {:?}",
                std::mem::discriminant(&base_type)
            );
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
        eprintln!("[BUG #70 FIELD RANGE] Calling get_expression_struct_type...");
        let struct_type = self.get_expression_struct_type(base);
        if struct_type.is_none() {
            eprintln!("[BUG #70 FIELD RANGE] get_expression_struct_type returned None!");
            return None;
        }
        let struct_type = struct_type?;

        // Clone the fields to avoid borrow checker issues
        let fields: Vec<_> = struct_type
            .fields
            .iter()
            .map(|f| (f.name.clone(), f.field_type.clone()))
            .collect();

        // BUG FIX #15/#17: Calculate field offset correctly for tuples
        // Tuples are packed as {a, b, c} where 'a' is in MSB (highest bits)
        // So for (bit[32], bit[32]), element 0 is at [63:32], element 1 is at [31:0]
        // We need to calculate offsets from MSB downward for tuple fields

        // First, calculate total width to determine if we need MSB-first ordering
        eprintln!(
            "[BUG #70 TUPLE DEBUG] Calculating total width for {} fields:",
            fields.len()
        );
        for (name, ty) in &fields {
            eprintln!(
                "[BUG #70 TUPLE DEBUG]   Field '{}': type discriminant = {:?}",
                name,
                std::mem::discriminant(ty)
            );
        }
        let total_width: usize = fields
            .iter()
            .map(|(_, ty)| {
                let width = self.get_hir_type_width(ty);
                eprintln!("[BUG #70 TUPLE DEBUG]   Width calculated: {}", width);
                width
            })
            .sum();
        eprintln!("[BUG #70 TUPLE DEBUG] Total width: {}", total_width);

        // Check if this is a tuple (fields named "_0", "_1", etc.)
        let is_tuple = fields
            .iter()
            .all(|(name, _)| name.starts_with('_') && name[1..].parse::<usize>().is_ok());

        if is_tuple {
            // For tuples: calculate offset from MSB downward
            let mut current_offset_from_msb = total_width;
            for (field_name_in_struct, field_type) in fields {
                let field_width = self.get_hir_type_width(&field_type);
                current_offset_from_msb -= field_width;
                if field_name_in_struct == field_name {
                    // Found the field - bits are [current_offset_from_msb + width - 1 : current_offset_from_msb]
                    let high_bit = current_offset_from_msb + field_width - 1;
                    let low_bit = current_offset_from_msb;
                    return Some((high_bit, low_bit));
                }
            }
        } else {
            // For regular structs: calculate offset from LSB upward (original behavior)
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
            hir::HirType::String => {
                panic!("String type is not synthesizable and cannot be used in hardware modules. String types are only supported in testbench contexts.");
            }
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
            hir::HirType::Vec3(element_type) => {
                // BUG FIX #70: vec3 is 3 elements
                let element_width = self.get_hir_type_width(element_type);
                eprintln!(
                    "[BUG #70 FIX] Vec3 element width: {}, total: {}",
                    element_width,
                    element_width * 3
                );
                element_width * 3
            }
            hir::HirType::Vec4(element_type) => {
                // BUG FIX #70: vec4 is 4 elements
                let element_width = self.get_hir_type_width(element_type);
                eprintln!(
                    "[BUG #70 FIX] Vec4 element width: {}, total: {}",
                    element_width,
                    element_width * 4
                );
                element_width * 4
            }
            hir::HirType::Custom(type_name) => {
                // BUG FIX #70: Look up custom struct types and calculate their actual width
                eprintln!(
                    "[BUG #70 DEBUG] get_hir_type_width called for Custom type '{}'",
                    type_name
                );
                // Check if this is a known struct type
                if let Some(hir) = self.hir {
                    eprintln!(
                        "[BUG #70 DEBUG] HIR available, searching {} user_defined_types",
                        hir.user_defined_types.len()
                    );
                    // Search in user-defined types
                    for user_type in &hir.user_defined_types {
                        if user_type.name == *type_name {
                            // Check if it's a struct type
                            if let hir::HirType::Struct(struct_type) = &user_type.type_def {
                                let mut total_width = 0;
                                for field in &struct_type.fields {
                                    total_width += self.get_hir_type_width(&field.field_type);
                                }
                                eprintln!("[BUG #70 FIX] Custom type '{}' resolved to struct with width {}", type_name, total_width);
                                return total_width;
                            }
                        }
                    }
                }
                // Fallback: Custom types default to 32 bits if not found
                eprintln!("[BUG #70 WARNING] Custom type '{}' not found in user_defined_types, defaulting to 32 bits", type_name);
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
            // Vec3 and Vec4 handled above with debug output (lines 7017-7036)

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

    /// BUG #33 FIX: Resolve a constant identifier to its MIR expression value
    /// Used for constant patterns in Match expressions
    fn resolve_constant_value(&self, const_name: &str) -> Option<Expression> {
        let hir = self.hir?;

        // Look through all implementations for the constant
        for impl_block in &hir.implementations {
            for constant in &impl_block.constants {
                if constant.name == const_name {
                    // Try to convert the constant's value expression
                    // For simple literals, this should work directly
                    return self.convert_expression_for_constant(&constant.value);
                }
            }
        }

        // Also check global functions for constants (they might be defined at module level)
        // Note: Global constants would be in hir.constants if that field existed
        // For now, we'll just return None if not found in implementations
        None
    }

    /// Convert a constant expression (for pattern matching)
    /// This is a simplified version that only handles literal values
    fn convert_expression_for_constant(&self, expr: &hir::HirExpression) -> Option<Expression> {
        match expr {
            hir::HirExpression::Literal(lit) => {
                Some(Expression::Literal(self.convert_literal_immutable(lit)?))
            }
            hir::HirExpression::Binary(bin) => {
                // For simple binary expressions like 0b010010, handle it
                let left = self.convert_expression_for_constant(&bin.left)?;
                let right = self.convert_expression_for_constant(&bin.right)?;
                let op = self.convert_binary_op(&bin.op, &bin.left);
                Some(Expression::Binary {
                    op,
                    left: Box::new(left),
                    right: Box::new(right),
                })
            }
            _ => None,
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
