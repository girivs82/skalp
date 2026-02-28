use anyhow::Result;
use indexmap::IndexMap;
use skalp_frontend::hir::*;
use skalp_frontend::safety_attributes::ModuleSafetyDefinitions;

use crate::builtins::BuiltinScope;
use crate::syntax::{SyntaxElement, SyntaxKind, SyntaxNode};
use crate::vhdl_types::*;

pub struct VhdlHirBuilder {
    next_entity_id: u32,
    next_port_id: u32,
    next_signal_id: u32,
    next_variable_id: u32,
    next_constant_id: u32,
    next_block_id: u32,
    next_assignment_id: u32,
    next_instance_id: u32,
    next_for_loop_id: u32,

    builtin_scope: BuiltinScope,
    user_types: IndexMap<String, HirType>,
    /// Maps view_name -> (interface_name, field_name -> direction)
    view_defs: IndexMap<String, (String, IndexMap<String, HirPortDirection>)>,
    entity_map: IndexMap<String, EntityId>,
    port_map: IndexMap<String, PortId>,
    signal_map: IndexMap<String, SignalId>,
    variable_map: IndexMap<String, VariableId>,
    constant_map: IndexMap<String, ConstantId>,

    inside_event_block: bool,

    errors: Vec<crate::diagnostics::VhdlError>,
    file_path: Option<std::path::PathBuf>,
}

// ========================================================================
// Helper: extract children and tokens from rowan nodes
// ========================================================================

fn child_nodes(node: &SyntaxNode) -> Vec<SyntaxNode> {
    node.children().collect()
}

fn child_tokens_of_kind(node: &SyntaxNode, kind: SyntaxKind) -> Vec<String> {
    node.children_with_tokens()
        .filter_map(|el| match el {
            SyntaxElement::Token(t) if t.kind() == kind => Some(t.text().to_string()),
            _ => None,
        })
        .collect()
}

fn first_token_text(node: &SyntaxNode, kind: SyntaxKind) -> Option<String> {
    node.children_with_tokens().find_map(|el| match el {
        SyntaxElement::Token(t) if t.kind() == kind => Some(t.text().to_string()),
        _ => None,
    })
}

fn first_child_of_kind(node: &SyntaxNode, kind: SyntaxKind) -> Option<SyntaxNode> {
    node.children().find(|c| c.kind() == kind)
}

fn all_children_of_kind(node: &SyntaxNode, kind: SyntaxKind) -> Vec<SyntaxNode> {
    node.children().filter(|c| c.kind() == kind).collect()
}

/// Get all ident texts from a node (direct children only)
fn ident_texts(node: &SyntaxNode) -> Vec<String> {
    child_tokens_of_kind(node, SyntaxKind::Ident)
}

/// Get the first ident text
fn first_ident(node: &SyntaxNode) -> Option<String> {
    first_token_text(node, SyntaxKind::Ident)
}

/// Check if a token of given kind exists as direct child
fn has_token(node: &SyntaxNode, kind: SyntaxKind) -> bool {
    node.children_with_tokens()
        .any(|el| matches!(el, SyntaxElement::Token(ref t) if t.kind() == kind))
}

/// Collect all non-trivia token texts
fn all_token_texts(node: &SyntaxNode) -> Vec<(SyntaxKind, String)> {
    node.children_with_tokens()
        .filter_map(|el| match el {
            SyntaxElement::Token(t)
                if t.kind() != SyntaxKind::Whitespace && t.kind() != SyntaxKind::Comment =>
            {
                Some((t.kind(), t.text().to_string()))
            }
            _ => None,
        })
        .collect()
}

fn to_pascal_case(name: &str) -> String {
    name.split('_')
        .map(|part| {
            let mut chars: Vec<char> = part.chars().collect();
            if !chars.is_empty() {
                chars[0] = chars[0].to_ascii_uppercase();
            }
            chars.into_iter().collect::<String>()
        })
        .collect()
}

impl VhdlHirBuilder {
    pub fn new(file_path: Option<&std::path::Path>) -> Self {
        Self {
            next_entity_id: 0,
            next_port_id: 0,
            next_signal_id: 0,
            next_variable_id: 0,
            next_constant_id: 0,
            next_block_id: 0,
            next_assignment_id: 0,
            next_instance_id: 0,
            next_for_loop_id: 0,
            builtin_scope: BuiltinScope::new(),
            user_types: IndexMap::new(),
            view_defs: IndexMap::new(),
            entity_map: IndexMap::new(),
            port_map: IndexMap::new(),
            signal_map: IndexMap::new(),
            variable_map: IndexMap::new(),
            constant_map: IndexMap::new(),
            inside_event_block: false,
            errors: Vec::new(),
            file_path: file_path.map(|p| p.to_path_buf()),
        }
    }

    fn alloc_entity_id(&mut self) -> EntityId {
        let id = EntityId(self.next_entity_id);
        self.next_entity_id += 1;
        id
    }
    fn alloc_port_id(&mut self) -> PortId {
        let id = PortId(self.next_port_id);
        self.next_port_id += 1;
        id
    }
    fn alloc_signal_id(&mut self) -> SignalId {
        let id = SignalId(self.next_signal_id);
        self.next_signal_id += 1;
        id
    }
    fn alloc_variable_id(&mut self) -> VariableId {
        let id = VariableId(self.next_variable_id);
        self.next_variable_id += 1;
        id
    }
    fn alloc_constant_id(&mut self) -> ConstantId {
        let id = ConstantId(self.next_constant_id);
        self.next_constant_id += 1;
        id
    }
    fn alloc_block_id(&mut self) -> BlockId {
        let id = BlockId(self.next_block_id);
        self.next_block_id += 1;
        id
    }
    fn alloc_assignment_id(&mut self) -> AssignmentId {
        let id = AssignmentId(self.next_assignment_id);
        self.next_assignment_id += 1;
        id
    }
    fn alloc_instance_id(&mut self) -> InstanceId {
        let id = InstanceId(self.next_instance_id);
        self.next_instance_id += 1;
        id
    }
    fn alloc_for_loop_id(&mut self) -> ForLoopId {
        let id = ForLoopId(self.next_for_loop_id);
        self.next_for_loop_id += 1;
        id
    }

    // ====================================================================
    // Top-level lowering
    // ====================================================================

    pub fn lower(&mut self, root: &SyntaxNode) -> Result<Hir> {
        let file_name = self
            .file_path
            .as_ref()
            .and_then(|p| p.file_stem())
            .and_then(|s| s.to_str())
            .unwrap_or("vhdl_design")
            .to_string();

        let mut hir = Hir::new(file_name);

        // First pass: collect entity names for forward references
        for child in root.children() {
            if child.kind() == SyntaxKind::EntityDecl {
                if let Some(name) = first_ident(&child) {
                    let id = self.alloc_entity_id();
                    let pascal = to_pascal_case(&name);
                    self.entity_map.insert(name.clone(), id);
                    self.entity_map.insert(pascal, id);
                }
                // Reset ID counter — we'll re-allocate in the real pass
                self.next_entity_id -= 1;
            }
        }
        self.next_entity_id = 0;
        self.entity_map.clear();

        // Second pass: full lowering
        for child in root.children() {
            match child.kind() {
                SyntaxKind::LibraryClause => self.lower_library_clause(&child),
                SyntaxKind::UseClause => self.lower_use_clause(&child),
                SyntaxKind::EntityDecl => {
                    if let Some(entity) = self.lower_entity_decl(&child) {
                        hir.entities.push(entity);
                    }
                }
                SyntaxKind::ArchitectureBody => {
                    if let Some(imp) = self.lower_architecture(&child) {
                        hir.implementations.push(imp);
                    }
                }
                SyntaxKind::InterfaceDecl => {
                    self.lower_interface_decl(&child);
                }
                SyntaxKind::ViewDecl => {
                    self.lower_view_decl(&child);
                }
                _ => {}
            }
        }

        Ok(hir)
    }

    // ====================================================================
    // Library/Use
    // ====================================================================

    fn lower_library_clause(&mut self, _node: &SyntaxNode) {
        // Library clause: just note it for context
    }

    fn lower_use_clause(&mut self, node: &SyntaxNode) {
        // Extract the selected name text (e.g., "ieee.std_logic_1164.all")
        if let Some(sel_name) = first_child_of_kind(node, SyntaxKind::SelectedName) {
            let parts: Vec<String> = sel_name
                .children_with_tokens()
                .filter_map(|el| match el {
                    SyntaxElement::Token(t)
                        if t.kind() != SyntaxKind::Whitespace
                            && t.kind() != SyntaxKind::Comment
                            && t.kind() != SyntaxKind::Dot =>
                    {
                        Some(t.text().to_string())
                    }
                    _ => None,
                })
                .collect();
            let full_name = parts.join(".");
            self.builtin_scope.register_use(&full_name);
        }
    }

    // ====================================================================
    // Entity
    // ====================================================================

    fn lower_entity_decl(&mut self, node: &SyntaxNode) -> Option<HirEntity> {
        let name = first_ident(node)?;
        let pascal_name = to_pascal_case(&name);
        let id = self.alloc_entity_id();
        self.entity_map.insert(name.clone(), id);
        self.entity_map.insert(pascal_name.clone(), id);

        // Clear port map for this entity
        self.port_map.clear();

        let mut ports = Vec::new();
        let mut generics = Vec::new();

        // Process generic clause
        if let Some(gc) = first_child_of_kind(node, SyntaxKind::GenericClause) {
            for gd in all_children_of_kind(&gc, SyntaxKind::GenericDecl) {
                if let Some(g) = self.lower_generic_decl(&gd) {
                    generics.push(g);
                }
            }
        }

        // Process port clause
        if let Some(pc) = first_child_of_kind(node, SyntaxKind::PortClause) {
            for pd in all_children_of_kind(&pc, SyntaxKind::PortDecl) {
                ports.extend(self.lower_port_decl(&pd));
            }
        }

        Some(HirEntity {
            id,
            name: pascal_name,
            is_async: false,
            visibility: HirVisibility::Public,
            ports,
            generics,
            clock_domains: Vec::new(),
            assignments: Vec::new(),
            signals: Vec::new(),
            span: None,
            pipeline_config: None,
            vendor_ip_config: None,
            power_domains: Vec::new(),
            power_domain_config: None,
            safety_mechanism_config: None,
            seooc_config: None,
            compiled_ip_config: None,
        })
    }

    fn lower_generic_decl(&mut self, node: &SyntaxNode) -> Option<HirGeneric> {
        let idents = ident_texts(node);
        let name = idents.first()?.clone();

        // Determine generic type from the subtype indication
        let subtype = first_child_of_kind(node, SyntaxKind::SubtypeIndication);
        let param_type = if let Some(ref st) = subtype {
            let type_name = self.subtype_indication_type_name(st);
            match type_name.as_str() {
                "integer" | "natural" | "positive" => HirGenericType::Const(HirType::Nat(32)),
                "boolean" => HirGenericType::Const(HirType::Bool),
                _ => HirGenericType::Const(HirType::Nat(32)),
            }
        } else {
            HirGenericType::Const(HirType::Nat(32))
        };

        // Check for default value
        let default_value = self.extract_default_value(node);

        Some(HirGeneric {
            name,
            param_type,
            default_value,
        })
    }

    fn lower_port_decl(&mut self, node: &SyntaxNode) -> Vec<HirPort> {
        let idents = ident_texts(node);
        if idents.is_empty() {
            return Vec::new();
        }

        // Check for view port: PortDecl contains ViewKw token (no PortDirection child)
        if has_token(node, SyntaxKind::ViewKw) {
            return self.lower_view_port_decl(node, &idents);
        }

        // Extract direction from PortDirection child
        let direction = if let Some(dir_node) = first_child_of_kind(node, SyntaxKind::PortDirection) {
            if has_token(&dir_node, SyntaxKind::InKw) {
                HirPortDirection::Input
            } else if has_token(&dir_node, SyntaxKind::OutKw) {
                HirPortDirection::Output
            } else if has_token(&dir_node, SyntaxKind::InoutKw) || has_token(&dir_node, SyntaxKind::BufferKw) {
                HirPortDirection::Bidirectional
            } else {
                HirPortDirection::Input
            }
        } else {
            HirPortDirection::Input
        };

        // Extract type
        let port_type = if let Some(st) = first_child_of_kind(node, SyntaxKind::SubtypeIndication) {
            self.lower_subtype_indication(&st)
        } else {
            HirType::Logic(1)
        };

        let mut ports = Vec::new();
        for name in &idents {
            let id = self.alloc_port_id();
            self.port_map.insert(name.clone(), id);
            ports.push(HirPort {
                id,
                name: name.clone(),
                direction: direction.clone(),
                port_type: port_type.clone(),
                physical_constraints: None,
                detection_config: None,
                power_domain_config: None,
                isolation_config: None,
                retention_config: None,
            });
        }
        ports
    }

    /// Lower a view port declaration by flattening interface fields into individual ports.
    /// `idents` contains: [port_name, view_name] (both extracted as Ident tokens)
    fn lower_view_port_decl(&mut self, _node: &SyntaxNode, idents: &[String]) -> Vec<HirPort> {
        // idents[0] = port name (e.g., "bus"), idents[1] = view name (e.g., "axi_master")
        let port_name = match idents.first() {
            Some(n) => n.clone(),
            None => return Vec::new(),
        };
        let view_name = match idents.get(1) {
            Some(n) => n.clone(),
            None => return Vec::new(),
        };

        // Look up view definition
        let (interface_name, direction_map) = match self.view_defs.get(&view_name) {
            Some(v) => v.clone(),
            None => return Vec::new(),
        };

        // Look up interface struct type to get field names and types
        let fields = match self.user_types.get(&interface_name) {
            Some(HirType::Struct(st)) => st.fields.clone(),
            _ => return Vec::new(),
        };

        // Flatten: create one HirPort per interface field
        let mut ports = Vec::new();
        for field in &fields {
            let flat_name = format!("{}_{}", port_name, field.name);
            let direction = direction_map
                .get(&field.name)
                .cloned()
                .unwrap_or(HirPortDirection::Input);

            let id = self.alloc_port_id();
            self.port_map.insert(flat_name.clone(), id);
            ports.push(HirPort {
                id,
                name: flat_name,
                direction,
                port_type: field.field_type.clone(),
                physical_constraints: None,
                detection_config: None,
                power_domain_config: None,
                isolation_config: None,
                retention_config: None,
            });
        }
        ports
    }

    // ====================================================================
    // Architecture -> HirImplementation
    // ====================================================================

    fn lower_architecture(&mut self, node: &SyntaxNode) -> Option<HirImplementation> {
        // Architecture idents: arch_name, entity_name
        let idents = ident_texts(node);
        let entity_name = idents.get(1).cloned().unwrap_or_default();
        let pascal_entity = to_pascal_case(&entity_name);

        let entity_id = self
            .entity_map
            .get(&entity_name)
            .or_else(|| self.entity_map.get(&pascal_entity))
            .copied()
            .unwrap_or(EntityId(0));

        // Clear signal/variable maps for this architecture
        self.signal_map.clear();
        self.variable_map.clear();
        self.constant_map.clear();

        let mut signals = Vec::new();
        let mut variables = Vec::new();
        let mut constants = Vec::new();
        let mut event_blocks = Vec::new();
        let mut assignments = Vec::new();
        let mut instances = Vec::new();

        // Process declarations
        for child in node.children() {
            match child.kind() {
                SyntaxKind::SignalDecl => {
                    signals.extend(self.lower_signal_decl(&child));
                }
                SyntaxKind::ConstantDecl => {
                    if let Some(c) = self.lower_constant_decl(&child) {
                        constants.push(c);
                    }
                }
                SyntaxKind::VariableDecl => {
                    if let Some(v) = self.lower_variable_decl(&child) {
                        variables.push(v);
                    }
                }
                SyntaxKind::TypeDecl => {
                    self.lower_type_decl(&child);
                }
                _ => {}
            }
        }

        // Process concurrent statements
        for child in node.children() {
            match child.kind() {
                SyntaxKind::ProcessStmt => {
                    let (eb, proc_vars) = self.lower_process(&child);
                    if let Some(eb) = eb {
                        event_blocks.push(eb);
                    }
                    variables.extend(proc_vars);
                }
                SyntaxKind::ConcurrentSignalAssign => {
                    if let Some(a) = self.lower_concurrent_assign(&child) {
                        assignments.push(a);
                    }
                }
                SyntaxKind::ComponentInst => {
                    if let Some(inst) = self.lower_component_inst(&child) {
                        instances.push(inst);
                    }
                }
                _ => {}
            }
        }

        Some(HirImplementation {
            entity: entity_id,
            signals,
            variables,
            constants,
            functions: Vec::new(),
            event_blocks,
            assignments,
            instances,
            covergroups: Vec::new(),
            formal_blocks: Vec::new(),
            statements: Vec::new(),
        })
    }

    // ====================================================================
    // Signal/Variable/Constant declarations
    // ====================================================================

    fn lower_signal_decl(&mut self, node: &SyntaxNode) -> Vec<HirSignal> {
        let idents = ident_texts(node);
        let signal_type = first_child_of_kind(node, SyntaxKind::SubtypeIndication)
            .map(|st| self.lower_subtype_indication(&st))
            .unwrap_or(HirType::Logic(1));

        let init = self.extract_default_value(node);

        let mut signals = Vec::new();
        for name in &idents {
            let id = self.alloc_signal_id();
            self.signal_map.insert(name.clone(), id);
            signals.push(HirSignal {
                id,
                name: name.clone(),
                signal_type: signal_type.clone(),
                initial_value: init.clone(),
                clock_domain: None,
                span: None,
                memory_config: None,
                trace_config: None,
                cdc_config: None,
                breakpoint_config: None,
                power_config: None,
                safety_config: None,
                power_domain: None,
            });
        }
        signals
    }

    fn lower_variable_decl(&mut self, node: &SyntaxNode) -> Option<HirVariable> {
        let name = first_ident(node)?;
        let var_type = first_child_of_kind(node, SyntaxKind::SubtypeIndication)
            .map(|st| self.lower_subtype_indication(&st))
            .unwrap_or(HirType::Logic(1));
        let init = self.extract_default_value(node);
        let id = self.alloc_variable_id();
        self.variable_map.insert(name.clone(), id);
        Some(HirVariable {
            id,
            name,
            var_type,
            initial_value: init,
            span: None,
        })
    }

    fn lower_constant_decl(&mut self, node: &SyntaxNode) -> Option<HirConstant> {
        let name = first_ident(node)?;
        let const_type = first_child_of_kind(node, SyntaxKind::SubtypeIndication)
            .map(|st| self.lower_subtype_indication(&st))
            .unwrap_or(HirType::Nat(32));
        let value = self
            .extract_default_value(node)
            .unwrap_or(HirExpression::Literal(HirLiteral::Integer(0)));
        let id = self.alloc_constant_id();
        self.constant_map.insert(name.clone(), id);
        Some(HirConstant {
            id,
            name,
            const_type,
            value,
        })
    }

    fn lower_type_decl(&mut self, node: &SyntaxNode) {
        let name = match first_ident(node) {
            Some(n) => n,
            None => return,
        };

        // Enum type
        if let Some(enum_def) = first_child_of_kind(node, SyntaxKind::EnumTypeDef) {
            let variants = ident_texts(&enum_def);
            if !variants.is_empty() {
                let ty = make_enum_type(&name, &variants);
                self.user_types.insert(name, ty);
            }
            return;
        }

        // Record type
        if let Some(rec_def) = first_child_of_kind(node, SyntaxKind::RecordTypeDef) {
            let mut fields = Vec::new();
            for field_node in all_children_of_kind(&rec_def, SyntaxKind::RecordField) {
                let field_names = ident_texts(&field_node);
                let field_type =
                    first_child_of_kind(&field_node, SyntaxKind::SubtypeIndication)
                        .map(|st| self.lower_subtype_indication(&st))
                        .unwrap_or(HirType::Logic(1));
                for fn_name in &field_names {
                    fields.push((fn_name.clone(), field_type.clone()));
                }
            }
            let ty = make_struct_type(&name, &fields);
            self.user_types.insert(name, ty);
            return;
        }

        // Array type
        if let Some(arr_def) = first_child_of_kind(node, SyntaxKind::ArrayTypeDef) {
            let elem_type =
                first_child_of_kind(&arr_def, SyntaxKind::SubtypeIndication)
                    .map(|st| self.lower_subtype_indication(&st))
                    .unwrap_or(HirType::Logic(1));
            // Try to extract size from discrete range
            let size = first_child_of_kind(&arr_def, SyntaxKind::DiscreteRange)
                .and_then(|dr| self.extract_range_size(&dr))
                .unwrap_or(1);
            let ty = make_array_type(elem_type, size);
            self.user_types.insert(name, ty);
        }
    }

    // ====================================================================
    // Process -> HirEventBlock
    // ====================================================================

    fn lower_process(
        &mut self,
        node: &SyntaxNode,
    ) -> (Option<HirEventBlock>, Vec<HirVariable>) {
        let block_id = self.alloc_block_id();

        // Extract sensitivity list
        let sensitivity = first_child_of_kind(node, SyntaxKind::SensitivityList);
        let is_combinational = sensitivity
            .as_ref()
            .map(|sl| has_token(sl, SyntaxKind::AllKw))
            .unwrap_or(false);

        let sens_names: Vec<String> = sensitivity
            .as_ref()
            .map(|sl| {
                sl.children()
                    .filter(|c| c.kind() == SyntaxKind::Name)
                    .filter_map(|n| first_ident(&n).or_else(|| self.name_first_text(&n)))
                    .collect()
            })
            .unwrap_or_default();

        // Process declarations (variables)
        let mut proc_vars = Vec::new();
        for child in node.children() {
            if child.kind() == SyntaxKind::VariableDecl {
                if let Some(v) = self.lower_variable_decl(&child) {
                    proc_vars.push(v);
                }
            }
        }

        // Sequential statements
        self.inside_event_block = !is_combinational;
        let statements = self.lower_sequential_statements(node);
        self.inside_event_block = false;

        // Determine triggers
        let triggers = if is_combinational {
            Vec::new() // Empty triggers = combinational
        } else {
            self.detect_triggers(&sens_names, &statements)
        };

        let event_block = HirEventBlock {
            id: block_id,
            triggers,
            statements,
        };

        (Some(event_block), proc_vars)
    }

    fn detect_triggers(
        &self,
        sens_names: &[String],
        _statements: &[HirStatement],
    ) -> Vec<HirEventTrigger> {
        // For MVP: look for clock signal in sensitivity list
        // Common patterns: "clk", "clock", anything containing "clk"
        let mut triggers = Vec::new();

        for name in sens_names {
            let lower = name.to_ascii_lowercase();
            if lower.contains("clk") || lower.contains("clock") {
                // Clock signal — rising edge
                let signal = self.resolve_signal_ref(name);
                triggers.push(HirEventTrigger {
                    signal,
                    edge: HirEdgeType::Rising,
                });
            } else if lower.contains("rst") || lower.contains("reset") {
                // Reset signal — active high
                let signal = self.resolve_signal_ref(name);
                triggers.push(HirEventTrigger {
                    signal,
                    edge: HirEdgeType::Both,
                });
            }
        }

        if triggers.is_empty() && !sens_names.is_empty() {
            // Fallback: first signal is clock (rising edge)
            let signal = self.resolve_signal_ref(&sens_names[0]);
            triggers.push(HirEventTrigger {
                signal,
                edge: HirEdgeType::Rising,
            });
        }

        triggers
    }

    fn resolve_signal_ref(&self, name: &str) -> HirEventSignal {
        let lower = name.to_ascii_lowercase();
        if let Some(port_id) = self.port_map.get(&lower) {
            HirEventSignal::Port(*port_id)
        } else if let Some(sig_id) = self.signal_map.get(&lower) {
            HirEventSignal::Signal(*sig_id)
        } else {
            // Assume port 0 as fallback
            HirEventSignal::Port(PortId(0))
        }
    }

    // ====================================================================
    // Sequential statements
    // ====================================================================

    fn lower_sequential_statements(&mut self, parent: &SyntaxNode) -> Vec<HirStatement> {
        let mut stmts = Vec::new();
        for child in parent.children() {
            match child.kind() {
                SyntaxKind::IfStmt => {
                    if let Some(s) = self.lower_if_stmt(&child) {
                        stmts.push(s);
                    }
                }
                SyntaxKind::CaseStmt => {
                    if let Some(s) = self.lower_case_stmt(&child) {
                        stmts.push(s);
                    }
                }
                SyntaxKind::ForLoopStmt => {
                    if let Some(s) = self.lower_for_loop(&child) {
                        stmts.push(s);
                    }
                }
                SyntaxKind::SequentialSignalAssign | SyntaxKind::VariableAssignStmt => {
                    if let Some(a) = self.lower_sequential_assign(&child) {
                        stmts.push(HirStatement::Assignment(a));
                    }
                }
                SyntaxKind::NullStmt => {
                    // null statement — no-op
                }
                _ => {}
            }
        }
        stmts
    }

    fn lower_if_stmt(&mut self, node: &SyntaxNode) -> Option<HirStatement> {
        let all_elements: Vec<SyntaxElement> = node.children_with_tokens().collect();
        self.lower_if_from_elements(&all_elements)
    }

    fn lower_if_from_elements(&mut self, elements: &[SyntaxElement]) -> Option<HirStatement> {
        // Find sections delimited by If/Then/Elsif/Else/End tokens
        let mut sections: Vec<IfSection> = Vec::new();
        let mut current_section = IfSection::new(IfSectionKind::If);
        let mut in_condition = true;

        for el in elements {
            let kind = el.kind();
            match kind {
                SyntaxKind::Whitespace | SyntaxKind::Comment => continue,
                SyntaxKind::IfKw => {
                    // Start of if or end if — check context
                    if !current_section.condition_elements.is_empty()
                        || !current_section.body_elements.is_empty()
                    {
                        // This is "end if" — finalize
                        sections.push(current_section);
                        break;
                    }
                    in_condition = true;
                }
                SyntaxKind::ThenKw => {
                    in_condition = false;
                }
                SyntaxKind::ElsifKw => {
                    sections.push(current_section);
                    current_section = IfSection::new(IfSectionKind::Elsif);
                    in_condition = true;
                }
                SyntaxKind::ElseKw => {
                    sections.push(current_section);
                    current_section = IfSection::new(IfSectionKind::Else);
                    in_condition = false;
                }
                SyntaxKind::EndKw => {
                    sections.push(current_section);
                    break;
                }
                SyntaxKind::Semicolon => {
                    // End of "end if;" — just skip
                }
                _ => {
                    if in_condition {
                        current_section.condition_elements.push(el.clone());
                    } else {
                        current_section.body_elements.push(el.clone());
                    }
                }
            }
        }

        if sections.is_empty() {
            return None;
        }

        // Build nested if/elsif/else chain
        self.build_if_chain(&sections, 0)
    }

    fn build_if_chain(&mut self, sections: &[IfSection], idx: usize) -> Option<HirStatement> {
        if idx >= sections.len() {
            return None;
        }

        let section = &sections[idx];

        match section.kind {
            IfSectionKind::If | IfSectionKind::Elsif => {
                let condition = self.lower_expr_from_elements(&section.condition_elements);
                let then_stmts = self.lower_stmts_from_elements(&section.body_elements);

                let else_stmts = if idx + 1 < sections.len() {
                    match sections[idx + 1].kind {
                        IfSectionKind::Elsif => {
                            let elsif_stmt = self.build_if_chain(sections, idx + 1)?;
                            Some(vec![elsif_stmt])
                        }
                        IfSectionKind::Else => {
                            Some(self.lower_stmts_from_elements(&sections[idx + 1].body_elements))
                        }
                        _ => None,
                    }
                } else {
                    None
                };

                Some(HirStatement::If(HirIfStatement {
                    condition,
                    then_statements: then_stmts,
                    else_statements: else_stmts,
                    mux_style: MuxStyle::default(),
                }))
            }
            IfSectionKind::Else => {
                // Else by itself shouldn't appear at top level of chain
                None
            }
        }
    }

    fn lower_case_stmt(&mut self, node: &SyntaxNode) -> Option<HirStatement> {
        // Find the selector expression (first Name/expression child after CaseKw)
        let elements: Vec<SyntaxElement> = node.children_with_tokens().collect();

        // Extract selector: everything between "case" and "is"
        let mut selector_elements = Vec::new();
        let mut in_selector = false;
        for el in &elements {
            match el.kind() {
                SyntaxKind::CaseKw if !in_selector => {
                    in_selector = true;
                }
                SyntaxKind::IsKw if in_selector => {
                    break;
                }
                SyntaxKind::Whitespace | SyntaxKind::Comment => {}
                _ if in_selector => {
                    selector_elements.push(el.clone());
                }
                _ => {}
            }
        }

        let expr = self.lower_expr_from_elements(&selector_elements);

        // Process case alternatives
        let mut arms = Vec::new();
        for child in node.children() {
            if child.kind() == SyntaxKind::CaseAlternative {
                if let Some(arm) = self.lower_case_alternative(&child) {
                    arms.push(arm);
                }
            }
        }

        Some(HirStatement::Match(HirMatchStatement {
            expr,
            arms,
            mux_style: MuxStyle::default(),
        }))
    }

    fn lower_case_alternative(&mut self, node: &SyntaxNode) -> Option<HirMatchArm> {
        // when choices => statements
        let choice_list = first_child_of_kind(node, SyntaxKind::ChoiceList);
        let pattern = if let Some(cl) = choice_list {
            self.lower_choice_list_to_pattern(&cl)
        } else {
            HirPattern::Wildcard
        };

        let statements = self.lower_sequential_statements(node);

        Some(HirMatchArm {
            pattern,
            guard: None,
            statements,
        })
    }

    fn lower_choice_list_to_pattern(&mut self, node: &SyntaxNode) -> HirPattern {
        let choices = all_children_of_kind(node, SyntaxKind::Choice);
        if choices.len() == 1 {
            self.lower_choice_to_pattern(&choices[0])
        } else if choices.is_empty() {
            HirPattern::Wildcard
        } else {
            // Multiple choices — for MVP just use the first one
            self.lower_choice_to_pattern(&choices[0])
        }
    }

    fn lower_choice_to_pattern(&mut self, node: &SyntaxNode) -> HirPattern {
        if has_token(node, SyntaxKind::OthersKw) {
            return HirPattern::Wildcard;
        }

        // Try integer literal
        if let Some(text) = first_token_text(node, SyntaxKind::IntLiteral) {
            if let Ok(val) = text.replace('_', "").parse::<u64>() {
                return HirPattern::Literal(HirLiteral::Integer(val));
            }
        }

        // Try bit string literal
        if let Some(text) = first_token_text(node, SyntaxKind::BitStringLiteral) {
            return HirPattern::Literal(self.parse_bit_string_literal(&text));
        }

        // Try string literal (used as bit pattern)
        if let Some(text) = first_token_text(node, SyntaxKind::StringLiteral) {
            let inner = text.trim_matches('"');
            if inner.chars().all(|c| c == '0' || c == '1') {
                let val = u64::from_str_radix(inner, 2).unwrap_or(0);
                return HirPattern::Literal(HirLiteral::Integer(val));
            }
        }

        // Try char literal
        if let Some(text) = first_token_text(node, SyntaxKind::CharLiteral) {
            let ch = text.trim_matches('\'').chars().next().unwrap_or('0');
            match ch {
                '0' => return HirPattern::Literal(HirLiteral::Integer(0)),
                '1' => return HirPattern::Literal(HirLiteral::Integer(1)),
                _ => return HirPattern::Literal(HirLiteral::Integer(0)),
            }
        }

        // Try identifier (enum variant or variable binding)
        if let Some(name) = first_ident(node) {
            // Check if it's a known enum variant
            for (_type_name, hir_type) in &self.user_types {
                if let HirType::Enum(ref et) = hir_type {
                    if et.variants.iter().any(|v| v.name == name) {
                        return HirPattern::Path(et.name.clone(), name);
                    }
                }
            }
            return HirPattern::Variable(name);
        }

        // Try expression child
        let child_nodes: Vec<SyntaxNode> = node.children().collect();
        if let Some(first) = child_nodes.first() {
            let elements: Vec<SyntaxElement> = vec![SyntaxElement::Node(first.clone())];
            let expr = self.lower_expr_from_elements(&elements);
            if let HirExpression::Literal(lit) = expr {
                return HirPattern::Literal(lit);
            }
        }

        HirPattern::Wildcard
    }

    fn lower_for_loop(&mut self, node: &SyntaxNode) -> Option<HirStatement> {
        let idents = ident_texts(node);
        let iterator = idents.first()?.clone();
        let loop_id = self.alloc_for_loop_id();
        let iter_var_id = self.alloc_variable_id();
        self.variable_map.insert(iterator.clone(), iter_var_id);

        let range = first_child_of_kind(node, SyntaxKind::DiscreteRange)
            .and_then(|dr| self.lower_discrete_range(&dr))
            .unwrap_or(HirRange {
                start: HirExpression::Literal(HirLiteral::Integer(0)),
                end: HirExpression::Literal(HirLiteral::Integer(0)),
                inclusive: true,
                step: None,
            });

        let body = self.lower_sequential_statements(node);

        Some(HirStatement::For(HirForStatement {
            id: loop_id,
            iterator,
            iterator_var_id: iter_var_id,
            range,
            body,
            unroll: None,
        }))
    }

    fn lower_discrete_range(&mut self, node: &SyntaxNode) -> Option<HirRange> {
        let elements: Vec<SyntaxElement> = node
            .children_with_tokens()
            .filter(|el| {
                el.kind() != SyntaxKind::Whitespace && el.kind() != SyntaxKind::Comment
            })
            .collect();

        // Find "to" or "downto" token
        let mut before = Vec::new();
        let mut after = Vec::new();
        let mut direction = None;

        for el in &elements {
            match el.kind() {
                SyntaxKind::ToKw => {
                    direction = Some(RangeDirection::To);
                }
                SyntaxKind::DowntoKw => {
                    direction = Some(RangeDirection::Downto);
                }
                _ => {
                    if direction.is_none() {
                        before.push(el.clone());
                    } else {
                        after.push(el.clone());
                    }
                }
            }
        }

        let left = self.lower_expr_from_elements(&before);
        let right = self.lower_expr_from_elements(&after);

        match direction {
            Some(RangeDirection::To) => Some(HirRange { start: left, end: right, inclusive: true, step: None }),
            Some(RangeDirection::Downto) => Some(HirRange { start: right, end: left, inclusive: true, step: None }),
            None => Some(HirRange { start: left, end: right, inclusive: true, step: None }),
        }
    }

    fn lower_sequential_assign(&mut self, node: &SyntaxNode) -> Option<HirAssignment> {
        let elements: Vec<SyntaxElement> = node
            .children_with_tokens()
            .filter(|el| {
                el.kind() != SyntaxKind::Whitespace && el.kind() != SyntaxKind::Comment
            })
            .collect();

        // Split on <= or :=
        let mut lhs_elements = Vec::new();
        let mut rhs_elements = Vec::new();
        let mut assignment_type = if self.inside_event_block {
            HirAssignmentType::NonBlocking
        } else {
            HirAssignmentType::Combinational
        };
        let mut found_assign = false;

        for el in &elements {
            match el.kind() {
                SyntaxKind::SignalAssign if !found_assign => {
                    found_assign = true;
                    if self.inside_event_block {
                        assignment_type = HirAssignmentType::NonBlocking;
                    }
                }
                SyntaxKind::VarAssign if !found_assign => {
                    found_assign = true;
                    assignment_type = HirAssignmentType::Blocking;
                }
                SyntaxKind::Semicolon => {}
                _ => {
                    if found_assign {
                        rhs_elements.push(el.clone());
                    } else {
                        lhs_elements.push(el.clone());
                    }
                }
            }
        }

        if !found_assign {
            return None;
        }

        let lhs = self.lower_lvalue_from_elements(&lhs_elements);
        let rhs = self.lower_expr_from_elements(&rhs_elements);

        Some(HirAssignment {
            id: self.alloc_assignment_id(),
            lhs,
            assignment_type,
            rhs,
        })
    }

    // ====================================================================
    // Concurrent assignments
    // ====================================================================

    fn lower_concurrent_assign(&mut self, node: &SyntaxNode) -> Option<HirAssignment> {
        let elements: Vec<SyntaxElement> = node
            .children_with_tokens()
            .filter(|el| {
                el.kind() != SyntaxKind::Whitespace && el.kind() != SyntaxKind::Comment
            })
            .collect();

        let mut lhs_elements = Vec::new();
        let mut rhs_elements = Vec::new();
        let mut found_assign = false;
        let mut has_when = false;

        for el in &elements {
            match el.kind() {
                SyntaxKind::SignalAssign if !found_assign => {
                    found_assign = true;
                }
                SyntaxKind::Semicolon => {}
                SyntaxKind::WhenKw => {
                    has_when = true;
                    rhs_elements.push(el.clone());
                }
                _ => {
                    if found_assign {
                        rhs_elements.push(el.clone());
                    } else {
                        lhs_elements.push(el.clone());
                    }
                }
            }
        }

        if !found_assign {
            return None;
        }

        let lhs = self.lower_lvalue_from_elements(&lhs_elements);

        let rhs = if has_when {
            self.lower_conditional_expr(&rhs_elements)
        } else {
            self.lower_expr_from_elements(&rhs_elements)
        };

        Some(HirAssignment {
            id: self.alloc_assignment_id(),
            lhs,
            assignment_type: HirAssignmentType::Combinational,
            rhs,
        })
    }

    fn lower_conditional_expr(&mut self, elements: &[SyntaxElement]) -> HirExpression {
        // value when condition else value when condition else default
        // Split on WhenKw and ElseKw tokens
        let mut segments: Vec<Vec<SyntaxElement>> = Vec::new();
        let mut current = Vec::new();

        for el in elements {
            match el.kind() {
                SyntaxKind::WhenKw | SyntaxKind::ElseKw => {
                    if !current.is_empty() {
                        segments.push(current);
                        current = Vec::new();
                    }
                }
                _ => {
                    current.push(el.clone());
                }
            }
        }
        if !current.is_empty() {
            segments.push(current);
        }

        // Build ternary chain: segments come in pairs (value, condition) + final default
        if segments.len() >= 3 {
            let value = self.lower_expr_from_elements(&segments[0]);
            let condition = self.lower_expr_from_elements(&segments[1]);
            let else_val = if segments.len() > 3 {
                self.lower_conditional_expr_from_segments(&segments[2..])
            } else {
                self.lower_expr_from_elements(&segments[2])
            };
            HirExpression::Ternary {
                condition: Box::new(condition),
                true_expr: Box::new(value),
                false_expr: Box::new(else_val),
            }
        } else if segments.len() == 1 {
            self.lower_expr_from_elements(&segments[0])
        } else {
            HirExpression::Literal(HirLiteral::Integer(0))
        }
    }

    fn lower_conditional_expr_from_segments(
        &mut self,
        segments: &[Vec<SyntaxElement>],
    ) -> HirExpression {
        if segments.len() >= 3 {
            let value = self.lower_expr_from_elements(&segments[0]);
            let condition = self.lower_expr_from_elements(&segments[1]);
            let else_val = self.lower_conditional_expr_from_segments(&segments[2..]);
            HirExpression::Ternary {
                condition: Box::new(condition),
                true_expr: Box::new(value),
                false_expr: Box::new(else_val),
            }
        } else if segments.len() == 1 {
            self.lower_expr_from_elements(&segments[0])
        } else {
            HirExpression::Literal(HirLiteral::Integer(0))
        }
    }

    // ====================================================================
    // Component instantiation
    // ====================================================================

    fn lower_component_inst(&mut self, node: &SyntaxNode) -> Option<HirInstance> {
        let idents = ident_texts(node);
        // Component name could be after "entity", "component", or directly
        let comp_name = idents.first()?.clone();
        let pascal_name = to_pascal_case(&comp_name);

        let entity_id = self
            .entity_map
            .get(&comp_name)
            .or_else(|| self.entity_map.get(&pascal_name))
            .copied()
            .unwrap_or(EntityId(u32::MAX));

        let instance_name = format!("u_{}", comp_name);

        // Port map connections
        let mut connections = Vec::new();
        if let Some(pm) = first_child_of_kind(node, SyntaxKind::PortMap) {
            if let Some(al) = first_child_of_kind(&pm, SyntaxKind::AssociationList) {
                for ae in all_children_of_kind(&al, SyntaxKind::AssociationElement) {
                    if let Some(conn) = self.lower_association_element(&ae) {
                        connections.push(conn);
                    }
                }
            }
        }

        // Generic map
        let mut generic_args = Vec::new();
        if let Some(gm) = first_child_of_kind(node, SyntaxKind::GenericMap) {
            if let Some(al) = first_child_of_kind(&gm, SyntaxKind::AssociationList) {
                for ae in all_children_of_kind(&al, SyntaxKind::AssociationElement) {
                    let elements: Vec<SyntaxElement> = ae
                        .children_with_tokens()
                        .filter(|el| {
                            el.kind() != SyntaxKind::Whitespace
                                && el.kind() != SyntaxKind::Comment
                                && el.kind() != SyntaxKind::Arrow
                        })
                        .collect();
                    // For positional: just the expression
                    // For named: skip formal name and =>
                    let expr = self.lower_expr_from_elements(&elements);
                    generic_args.push(expr);
                }
            }
        }

        Some(HirInstance {
            id: self.alloc_instance_id(),
            name: instance_name,
            entity: entity_id,
            generic_args,
            named_generic_args: IndexMap::new(),
            connections,
            safety_config: None,
            variable_id: None,
        })
    }

    fn lower_association_element(&mut self, node: &SyntaxNode) -> Option<HirConnection> {
        let elements: Vec<(SyntaxKind, String)> = all_token_texts(node);

        // Check for named association: formal => actual
        let arrow_pos = elements.iter().position(|(k, _)| *k == SyntaxKind::Arrow);

        if let Some(pos) = arrow_pos {
            // Named: port_name => expression
            let port_name = elements
                .get(0)
                .map(|(_, t)| t.clone())
                .unwrap_or_default();

            let actual_elements: Vec<SyntaxElement> = node
                .children_with_tokens()
                .filter(|el| {
                    el.kind() != SyntaxKind::Whitespace && el.kind() != SyntaxKind::Comment
                })
                .skip_while(|el| el.kind() != SyntaxKind::Arrow)
                .skip(1) // skip the arrow
                .collect();

            let expr = self.lower_expr_from_elements(&actual_elements);
            Some(HirConnection {
                port: port_name,
                expr,
            })
        } else {
            // Positional — no port name info available
            let all: Vec<SyntaxElement> = node
                .children_with_tokens()
                .filter(|el| {
                    el.kind() != SyntaxKind::Whitespace && el.kind() != SyntaxKind::Comment
                })
                .collect();
            let expr = self.lower_expr_from_elements(&all);
            Some(HirConnection {
                port: String::new(), // positional
                expr,
            })
        }
    }

    // ====================================================================
    // VHDL-2019 Interface
    // ====================================================================

    fn lower_view_decl(&mut self, node: &SyntaxNode) {
        let idents = ident_texts(node);
        // First ident = view name, second ident = interface name
        let view_name = match idents.first() {
            Some(n) => n.clone(),
            None => return,
        };
        let interface_name = match idents.get(1) {
            Some(n) => n.clone(),
            None => return,
        };

        let mut direction_map = IndexMap::new();
        for field_dir in all_children_of_kind(node, SyntaxKind::ViewFieldDirection) {
            let field_name = match first_ident(&field_dir) {
                Some(n) => n,
                None => continue,
            };
            let dir = if has_token(&field_dir, SyntaxKind::InKw) {
                HirPortDirection::Input
            } else if has_token(&field_dir, SyntaxKind::OutKw) {
                HirPortDirection::Output
            } else if has_token(&field_dir, SyntaxKind::InoutKw) {
                HirPortDirection::Bidirectional
            } else {
                HirPortDirection::Input
            };
            direction_map.insert(field_name, dir);
        }

        self.view_defs
            .insert(view_name, (interface_name, direction_map));
    }

    fn lower_interface_decl(&mut self, node: &SyntaxNode) {
        let name = match first_ident(node) {
            Some(n) => n,
            None => return,
        };

        let mut fields = Vec::new();
        for sig_decl in all_children_of_kind(node, SyntaxKind::SignalDecl) {
            let sig_names = ident_texts(&sig_decl);
            let sig_type = first_child_of_kind(&sig_decl, SyntaxKind::SubtypeIndication)
                .map(|st| self.lower_subtype_indication(&st))
                .unwrap_or(HirType::Logic(1));
            for sn in &sig_names {
                fields.push((sn.clone(), sig_type.clone()));
            }
        }

        let ty = make_struct_type(&name, &fields);
        self.user_types.insert(name, ty);
    }

    // ====================================================================
    // Expression lowering from syntax elements
    // ====================================================================

    fn lower_expr_from_elements(&mut self, elements: &[SyntaxElement]) -> HirExpression {
        if elements.is_empty() {
            return HirExpression::Literal(HirLiteral::Integer(0));
        }

        // Single element
        if elements.len() == 1 {
            return self.lower_single_element(&elements[0]);
        }

        // Look for binary operators (lowest precedence first)
        // Logical: and, or, xor, nand, nor, xnor
        if let Some(expr) = self.try_binary_split(elements, &[
            SyntaxKind::AndKw, SyntaxKind::OrKw, SyntaxKind::XorKw,
            SyntaxKind::NandKw, SyntaxKind::NorKw, SyntaxKind::XnorKw,
        ]) {
            return expr;
        }

        // Relational: =, /=, <, <=, >, >=
        if let Some(expr) = self.try_binary_split(elements, &[
            SyntaxKind::Equal, SyntaxKind::NotEqual,
            SyntaxKind::LessThan, SyntaxKind::SignalAssign, // <= as comparison
            SyntaxKind::GreaterThan, SyntaxKind::GreaterEqual,
        ]) {
            return expr;
        }

        // Additive: +, -, &
        if let Some(expr) = self.try_binary_split(elements, &[
            SyntaxKind::Plus, SyntaxKind::Minus, SyntaxKind::Ampersand,
        ]) {
            return expr;
        }

        // Multiplicative: *, /
        if let Some(expr) = self.try_binary_split(elements, &[
            SyntaxKind::Star, SyntaxKind::Slash,
        ]) {
            return expr;
        }

        // Unary prefix: not, -, +
        if elements.len() >= 2 {
            let first_kind = elements[0].kind();
            if first_kind == SyntaxKind::NotKw {
                let operand = self.lower_expr_from_elements(&elements[1..]);
                return HirExpression::Unary(HirUnaryExpr {
                    op: HirUnaryOp::Not,
                    operand: Box::new(operand),
                });
            }
            if first_kind == SyntaxKind::Minus {
                let operand = self.lower_expr_from_elements(&elements[1..]);
                return HirExpression::Unary(HirUnaryExpr {
                    op: HirUnaryOp::Negate,
                    operand: Box::new(operand),
                });
            }
        }

        // If first element is a node, try to lower it
        if let SyntaxElement::Node(ref n) = elements[0] {
            return self.lower_syntax_node_expr(n);
        }

        // Fallback: try to interpret as a single token
        self.lower_single_element(&elements[0])
    }

    fn try_binary_split(
        &mut self,
        elements: &[SyntaxElement],
        ops: &[SyntaxKind],
    ) -> Option<HirExpression> {
        // Find the rightmost operator at depth 0 (not inside parens)
        let mut depth = 0i32;
        let mut best_pos = None;

        for (i, el) in elements.iter().enumerate() {
            match el.kind() {
                SyntaxKind::LParen => depth += 1,
                SyntaxKind::RParen => depth -= 1,
                k if depth == 0 && ops.contains(&k) && i > 0 => {
                    best_pos = Some(i);
                }
                _ => {}
            }
        }

        let pos = best_pos?;
        let lhs_elements = &elements[..pos];
        let rhs_elements = &elements[pos + 1..];

        if lhs_elements.is_empty() || rhs_elements.is_empty() {
            return None;
        }

        let op = match elements[pos].kind() {
            SyntaxKind::AndKw => HirBinaryOp::And,
            SyntaxKind::OrKw => HirBinaryOp::Or,
            SyntaxKind::XorKw => HirBinaryOp::Xor,
            SyntaxKind::NandKw => HirBinaryOp::And, // nand = not(and) — simplified for MVP
            SyntaxKind::NorKw => HirBinaryOp::Or,   // nor = not(or) — simplified
            SyntaxKind::XnorKw => HirBinaryOp::Xor, // xnor = not(xor) — simplified
            SyntaxKind::Equal => HirBinaryOp::Equal,
            SyntaxKind::NotEqual => HirBinaryOp::NotEqual,
            SyntaxKind::LessThan => HirBinaryOp::Less,
            SyntaxKind::SignalAssign => HirBinaryOp::LessEqual, // <= in expr context
            SyntaxKind::GreaterThan => HirBinaryOp::Greater,
            SyntaxKind::GreaterEqual => HirBinaryOp::GreaterEqual,
            SyntaxKind::Plus => HirBinaryOp::Add,
            SyntaxKind::Minus => HirBinaryOp::Sub,
            SyntaxKind::Ampersand => {
                // Concatenation
                let left = self.lower_expr_from_elements(lhs_elements);
                let right = self.lower_expr_from_elements(rhs_elements);
                return Some(HirExpression::Concat(vec![left, right]));
            }
            SyntaxKind::Star => HirBinaryOp::Mul,
            SyntaxKind::Slash => HirBinaryOp::Div,
            _ => return None,
        };

        let left = self.lower_expr_from_elements(lhs_elements);
        let right = self.lower_expr_from_elements(rhs_elements);

        Some(HirExpression::Binary(HirBinaryExpr {
            left: Box::new(left),
            op,
            right: Box::new(right),
            is_trait_op: false,
        }))
    }

    fn lower_single_element(&mut self, element: &SyntaxElement) -> HirExpression {
        match element {
            SyntaxElement::Token(t) => self.lower_token_expr(t.kind(), t.text()),
            SyntaxElement::Node(n) => self.lower_syntax_node_expr(n),
        }
    }

    fn lower_token_expr(&mut self, kind: SyntaxKind, text: &str) -> HirExpression {
        match kind {
            SyntaxKind::IntLiteral => {
                let val = text.replace('_', "").parse::<u64>().unwrap_or(0);
                HirExpression::Literal(HirLiteral::Integer(val))
            }
            SyntaxKind::CharLiteral => {
                let ch = text.trim_matches('\'').chars().next().unwrap_or('0');
                match ch {
                    '0' => HirExpression::Literal(HirLiteral::Integer(0)),
                    '1' => HirExpression::Literal(HirLiteral::Integer(1)),
                    _ => HirExpression::Literal(HirLiteral::Integer(0)),
                }
            }
            SyntaxKind::BitStringLiteral => {
                HirExpression::Literal(self.parse_bit_string_literal(text))
            }
            SyntaxKind::StringLiteral => {
                let inner = text.trim_matches('"');
                if inner.chars().all(|c| c == '0' || c == '1') {
                    let val = u64::from_str_radix(inner, 2).unwrap_or(0);
                    HirExpression::Literal(HirLiteral::Integer(val))
                } else {
                    HirExpression::Literal(HirLiteral::String(inner.to_string()))
                }
            }
            SyntaxKind::BasedLiteral => {
                let val = self.parse_based_literal(text);
                HirExpression::Literal(HirLiteral::Integer(val))
            }
            SyntaxKind::TrueKw => HirExpression::Literal(HirLiteral::Boolean(true)),
            SyntaxKind::FalseKw => HirExpression::Literal(HirLiteral::Boolean(false)),
            SyntaxKind::Ident => self.resolve_name(text),
            SyntaxKind::OthersKw => {
                // (others => '0') should have been handled at aggregate level
                HirExpression::Literal(HirLiteral::Integer(0))
            }
            _ => {
                // Try to resolve as a name
                HirExpression::Literal(HirLiteral::Integer(0))
            }
        }
    }

    fn lower_syntax_node_expr(&mut self, node: &SyntaxNode) -> HirExpression {
        match node.kind() {
            SyntaxKind::Name => self.lower_name_expr(node),
            SyntaxKind::AggregateExpr => self.lower_aggregate_expr(node),
            SyntaxKind::SubtypeIndication => {
                // Type conversion: unsigned(x), std_logic_vector(y)
                self.lower_type_conversion(node)
            }
            _ => {
                // Try to recurse into child elements
                let elements: Vec<SyntaxElement> = node
                    .children_with_tokens()
                    .filter(|el| {
                        el.kind() != SyntaxKind::Whitespace && el.kind() != SyntaxKind::Comment
                    })
                    .collect();
                self.lower_expr_from_elements(&elements)
            }
        }
    }

    fn lower_name_expr(&mut self, node: &SyntaxNode) -> HirExpression {
        let tokens: Vec<(SyntaxKind, String)> = all_token_texts(node);
        if tokens.is_empty() {
            return HirExpression::Literal(HirLiteral::Integer(0));
        }

        let (first_kind, first_text) = &tokens[0];

        // Check for builtin function calls
        match first_kind {
            SyntaxKind::RisingEdgeKw | SyntaxKind::FallingEdgeKw => {
                // rising_edge(clk) -> just return the argument as expression
                // The clock detection is handled at process level
                let args = self.extract_call_args(node);
                if let Some(arg) = args.into_iter().next() {
                    return arg;
                }
                return HirExpression::Literal(HirLiteral::Boolean(true));
            }
            SyntaxKind::ToUnsignedKw | SyntaxKind::ToSignedKw => {
                let args = self.extract_call_args(node);
                return if let Some(first_arg) = args.into_iter().next() {
                    first_arg
                } else {
                    HirExpression::Literal(HirLiteral::Integer(0))
                };
            }
            SyntaxKind::ToIntegerKw | SyntaxKind::ConvIntegerKw => {
                let args = self.extract_call_args(node);
                return if let Some(first_arg) = args.into_iter().next() {
                    first_arg
                } else {
                    HirExpression::Literal(HirLiteral::Integer(0))
                };
            }
            SyntaxKind::ResizeKw => {
                let args = self.extract_call_args(node);
                let mut it = args.into_iter();
                let signal = it.next().unwrap_or(HirExpression::Literal(HirLiteral::Integer(0)));
                return signal; // resize(sig, width) -> sig (width handled by type system)
            }
            SyntaxKind::UnsignedKw | SyntaxKind::SignedKw | SyntaxKind::StdLogicVectorKw
            | SyntaxKind::StdUlogicVectorKw => {
                // Type conversion: unsigned(x) -> cast
                let args = self.extract_call_args(node);
                return if let Some(first_arg) = args.into_iter().next() {
                    first_arg
                } else {
                    HirExpression::Literal(HirLiteral::Integer(0))
                };
            }
            _ => {}
        }

        // Regular name resolution
        let name = first_text.to_ascii_lowercase();

        // Check for function call pattern: name(args)
        if tokens.len() > 1 && tokens.get(1).map(|(k, _)| *k) == Some(SyntaxKind::LParen) {
            // Could be function call or indexing
            let args = self.extract_call_args(node);
            let base = self.resolve_name(&name);

            // If it's a known signal/port with args, it's indexing
            if self.port_map.contains_key(&name) || self.signal_map.contains_key(&name) {
                if args.len() == 1 {
                    return HirExpression::Index(Box::new(base), Box::new(args.into_iter().next().unwrap()));
                }
            }

            // Otherwise it's a function call
            return HirExpression::Call(HirCallExpr {
                function: name,
                type_args: Vec::new(),
                named_type_args: IndexMap::new(),
                args,
                impl_style: ImplStyle::Auto,
            });
        }

        // Check for field access: name.field
        if let Some(dot_pos) = tokens.iter().position(|(k, _)| *k == SyntaxKind::Dot) {
            let base = self.resolve_name(&name);
            if let Some((_, field_name)) = tokens.get(dot_pos + 1) {
                return HirExpression::FieldAccess {
                    base: Box::new(base),
                    field: field_name.clone(),
                };
            }
        }

        // Check for tick/attribute: name'attribute
        if let Some(tick_pos) = tokens.iter().position(|(k, _)| *k == SyntaxKind::Tick) {
            // For MVP: handle common attributes
            if let Some((_, attr_name)) = tokens.get(tick_pos + 1) {
                let lower_attr = attr_name.to_ascii_lowercase();
                let base = self.resolve_name(&name);
                match lower_attr.as_str() {
                    "event" => return base, // 'event used in sensitivity; ignore in expr
                    "range" | "length" | "high" | "low" | "left" | "right" => {
                        return HirExpression::Call(HirCallExpr {
                            function: format!("{}_{}", name, lower_attr),
                            type_args: Vec::new(),
                            named_type_args: IndexMap::new(),
                            args: vec![base],
                            impl_style: ImplStyle::Auto,
                        });
                    }
                    _ => return base,
                }
            }
        }

        self.resolve_name(&name)
    }

    fn extract_call_args(&mut self, node: &SyntaxNode) -> Vec<HirExpression> {
        // Find content between LParen and RParen tokens
        let elements: Vec<SyntaxElement> = node.children_with_tokens().collect();
        let mut inside_parens = false;
        let mut depth = 0;
        let mut current_arg: Vec<SyntaxElement> = Vec::new();
        let mut args = Vec::new();

        for el in &elements {
            let kind = el.kind();
            match kind {
                SyntaxKind::LParen if !inside_parens => {
                    inside_parens = true;
                    depth = 1;
                }
                SyntaxKind::LParen if inside_parens => {
                    depth += 1;
                    current_arg.push(el.clone());
                }
                SyntaxKind::RParen if inside_parens => {
                    depth -= 1;
                    if depth == 0 {
                        if !current_arg.is_empty() {
                            let filtered: Vec<_> = current_arg
                                .iter()
                                .filter(|e| {
                                    e.kind() != SyntaxKind::Whitespace
                                        && e.kind() != SyntaxKind::Comment
                                })
                                .cloned()
                                .collect();
                            if !filtered.is_empty() {
                                args.push(self.lower_expr_from_elements(&filtered));
                            }
                        }
                        break;
                    } else {
                        current_arg.push(el.clone());
                    }
                }
                SyntaxKind::Comma if inside_parens && depth == 1 => {
                    let filtered: Vec<_> = current_arg
                        .iter()
                        .filter(|e| {
                            e.kind() != SyntaxKind::Whitespace && e.kind() != SyntaxKind::Comment
                        })
                        .cloned()
                        .collect();
                    if !filtered.is_empty() {
                        args.push(self.lower_expr_from_elements(&filtered));
                    }
                    current_arg.clear();
                }
                SyntaxKind::Whitespace | SyntaxKind::Comment => {
                    // skip trivia
                }
                _ if inside_parens => {
                    current_arg.push(el.clone());
                }
                _ => {}
            }
        }

        args
    }

    fn lower_aggregate_expr(&mut self, node: &SyntaxNode) -> HirExpression {
        let children = all_children_of_kind(node, SyntaxKind::AggregateElement);

        // Check for (others => value) pattern
        if children.len() == 1 {
            let child = &children[0];
            if has_token(child, SyntaxKind::OthersKw) {
                // (others => '0') -> Literal(0), (others => '1') -> all ones
                let val_elements: Vec<SyntaxElement> = child
                    .children_with_tokens()
                    .filter(|el| {
                        el.kind() != SyntaxKind::Whitespace
                            && el.kind() != SyntaxKind::Comment
                            && el.kind() != SyntaxKind::OthersKw
                            && el.kind() != SyntaxKind::Arrow
                    })
                    .collect();
                return self.lower_expr_from_elements(&val_elements);
            }
        }

        // Regular aggregate — lower to struct literal or array literal
        let mut field_inits = Vec::new();
        let mut positional = Vec::new();

        for child in &children {
            let has_arrow = has_token(child, SyntaxKind::Arrow);
            let elements: Vec<SyntaxElement> = child
                .children_with_tokens()
                .filter(|el| {
                    el.kind() != SyntaxKind::Whitespace && el.kind() != SyntaxKind::Comment
                })
                .collect();

            if has_arrow {
                // Named: key => value
                let arrow_pos = elements.iter().position(|el| el.kind() == SyntaxKind::Arrow);
                if let Some(pos) = arrow_pos {
                    let key_elements = &elements[..pos];
                    let val_elements = &elements[pos + 1..];
                    let key_text = key_elements
                        .first()
                        .and_then(|el| match el {
                            SyntaxElement::Token(t) => Some(t.text().to_string()),
                            _ => None,
                        })
                        .unwrap_or_default();
                    let value = self.lower_expr_from_elements(val_elements);
                    field_inits.push(HirStructFieldInit {
                        name: key_text,
                        value,
                    });
                }
            } else {
                let expr = self.lower_expr_from_elements(&elements);
                positional.push(expr);
            }
        }

        if !field_inits.is_empty() {
            HirExpression::StructLiteral(HirStructLiteral {
                type_name: String::new(), // inferred from context
                generic_args: Vec::new(),
                fields: field_inits,
            })
        } else if !positional.is_empty() {
            HirExpression::ArrayLiteral(positional)
        } else {
            HirExpression::Literal(HirLiteral::Integer(0))
        }
    }

    fn lower_type_conversion(&mut self, _node: &SyntaxNode) -> HirExpression {
        HirExpression::Literal(HirLiteral::Integer(0))
    }

    // ====================================================================
    // LValue lowering
    // ====================================================================

    fn lower_lvalue_from_elements(&mut self, elements: &[SyntaxElement]) -> HirLValue {
        if elements.is_empty() {
            return HirLValue::Signal(SignalId(0));
        }

        // If single Name node
        if elements.len() == 1 {
            if let SyntaxElement::Node(n) = &elements[0] {
                return self.lower_name_lvalue(n);
            }
            if let SyntaxElement::Token(t) = &elements[0] {
                let name = t.text().to_ascii_lowercase();
                return self.resolve_lvalue_name(&name);
            }
        }

        // Try to find the name
        let first = &elements[0];
        if let SyntaxElement::Node(n) = first {
            return self.lower_name_lvalue(n);
        }
        if let SyntaxElement::Token(t) = first {
            let name = t.text().to_ascii_lowercase();
            return self.resolve_lvalue_name(&name);
        }

        HirLValue::Signal(SignalId(0))
    }

    fn lower_name_lvalue(&mut self, node: &SyntaxNode) -> HirLValue {
        let tokens: Vec<(SyntaxKind, String)> = all_token_texts(node);
        if tokens.is_empty() {
            return HirLValue::Signal(SignalId(0));
        }

        let name = tokens[0].1.to_ascii_lowercase();
        let base = self.resolve_lvalue_name(&name);

        // Check for indexing: name(expr)
        if tokens.len() > 1 && tokens.get(1).map(|(k, _)| *k) == Some(SyntaxKind::LParen) {
            let args = self.extract_call_args(node);
            if let Some(idx) = args.into_iter().next() {
                return HirLValue::Index(Box::new(base), idx);
            }
        }

        // Check for field access: name.field
        if let Some(dot_pos) = tokens.iter().position(|(k, _)| *k == SyntaxKind::Dot) {
            if let Some((_, field_name)) = tokens.get(dot_pos + 1) {
                return HirLValue::FieldAccess {
                    base: Box::new(base),
                    field: field_name.clone(),
                };
            }
        }

        base
    }

    // ====================================================================
    // Name resolution
    // ====================================================================

    fn resolve_name(&self, name: &str) -> HirExpression {
        let lower = name.to_ascii_lowercase();
        if let Some(port_id) = self.port_map.get(&lower) {
            HirExpression::Port(*port_id)
        } else if let Some(sig_id) = self.signal_map.get(&lower) {
            HirExpression::Signal(*sig_id)
        } else if let Some(var_id) = self.variable_map.get(&lower) {
            HirExpression::Variable(*var_id)
        } else if let Some(const_id) = self.constant_map.get(&lower) {
            HirExpression::Constant(*const_id)
        } else {
            // Could be a generic parameter or unresolved name
            HirExpression::GenericParam(lower)
        }
    }

    fn resolve_lvalue_name(&self, name: &str) -> HirLValue {
        let lower = name.to_ascii_lowercase();
        if let Some(port_id) = self.port_map.get(&lower) {
            HirLValue::Port(*port_id)
        } else if let Some(sig_id) = self.signal_map.get(&lower) {
            HirLValue::Signal(*sig_id)
        } else if let Some(var_id) = self.variable_map.get(&lower) {
            HirLValue::Variable(*var_id)
        } else {
            // Assume signal — may be forward reference
            HirLValue::Signal(SignalId(0))
        }
    }

    // ====================================================================
    // Subtype indication
    // ====================================================================

    fn lower_subtype_indication(&mut self, node: &SyntaxNode) -> HirType {
        let type_name = self.subtype_indication_type_name(node);
        let range = self.subtype_indication_range(node);

        resolve_vhdl_type(&type_name, range.as_ref(), &self.builtin_scope, &self.user_types)
            .unwrap_or(HirType::Logic(1))
    }

    fn subtype_indication_type_name(&self, node: &SyntaxNode) -> String {
        // First non-trivia token is the type name
        for el in node.children_with_tokens() {
            match el {
                SyntaxElement::Token(t) => {
                    let kind = t.kind();
                    if kind == SyntaxKind::Whitespace || kind == SyntaxKind::Comment {
                        continue;
                    }
                    // Map keyword tokens to type names
                    return match kind {
                        SyntaxKind::StdLogicKw => "std_logic".to_string(),
                        SyntaxKind::StdUlogicKw => "std_ulogic".to_string(),
                        SyntaxKind::StdLogicVectorKw => "std_logic_vector".to_string(),
                        SyntaxKind::StdUlogicVectorKw => "std_ulogic_vector".to_string(),
                        SyntaxKind::UnsignedKw => "unsigned".to_string(),
                        SyntaxKind::SignedKw => "signed".to_string(),
                        SyntaxKind::BooleanKw => "boolean".to_string(),
                        SyntaxKind::IntegerKw => "integer".to_string(),
                        SyntaxKind::NaturalKw => "natural".to_string(),
                        SyntaxKind::PositiveKw => "positive".to_string(),
                        SyntaxKind::RealKw => "real".to_string(),
                        SyntaxKind::BitKw => "bit".to_string(),
                        SyntaxKind::BitVectorKw => "bit_vector".to_string(),
                        SyntaxKind::Ident => t.text().to_ascii_lowercase(),
                        _ => t.text().to_ascii_lowercase(),
                    };
                }
                _ => {}
            }
        }
        "std_logic".to_string()
    }

    fn subtype_indication_range(&mut self, node: &SyntaxNode) -> Option<RangeInfo> {
        // Look for (expr downto/to expr) or range expr to/downto expr
        let elements: Vec<SyntaxElement> = node.children_with_tokens().collect();

        let mut in_parens = false;
        let mut paren_depth = 0;
        let mut range_elements = Vec::new();

        for el in &elements {
            match el.kind() {
                SyntaxKind::LParen => {
                    if !in_parens {
                        in_parens = true;
                        paren_depth = 1;
                    } else {
                        paren_depth += 1;
                        range_elements.push(el.clone());
                    }
                }
                SyntaxKind::RParen => {
                    paren_depth -= 1;
                    if paren_depth == 0 {
                        break;
                    }
                    range_elements.push(el.clone());
                }
                SyntaxKind::RangeKw if !in_parens => {
                    in_parens = true; // treat rest as range
                }
                _ if in_parens && paren_depth >= 1 => {
                    range_elements.push(el.clone());
                }
                _ if in_parens && paren_depth == 0 => {
                    range_elements.push(el.clone());
                }
                _ => {}
            }
        }

        if range_elements.is_empty() {
            return None;
        }

        // Find to/downto
        let mut before = Vec::new();
        let mut after = Vec::new();
        let mut direction = None;

        for el in &range_elements {
            match el.kind() {
                SyntaxKind::Whitespace | SyntaxKind::Comment => {}
                SyntaxKind::ToKw => direction = Some(RangeDirection::To),
                SyntaxKind::DowntoKw => direction = Some(RangeDirection::Downto),
                _ => {
                    if direction.is_none() {
                        before.push(el.clone());
                    } else {
                        after.push(el.clone());
                    }
                }
            }
        }

        let left = self.eval_const_expr(&before);
        let right = self.eval_const_expr(&after);

        Some(RangeInfo {
            left,
            right,
            direction: direction.unwrap_or(RangeDirection::Downto),
        })
    }

    fn eval_const_expr(&mut self, elements: &[SyntaxElement]) -> i64 {
        if elements.is_empty() {
            return 0;
        }
        if elements.len() == 1 {
            if let SyntaxElement::Token(t) = &elements[0] {
                if t.kind() == SyntaxKind::IntLiteral {
                    return t.text().replace('_', "").parse::<i64>().unwrap_or(0);
                }
            }
        }

        // Simple evaluation for patterns like "N-1", "WIDTH-1"
        // Find minus token
        if elements.len() == 3 {
            if elements[1].kind() == SyntaxKind::Minus {
                let left = self.eval_const_expr(&elements[..1]);
                let right = self.eval_const_expr(&elements[2..]);
                return left - right;
            }
            if elements[1].kind() == SyntaxKind::Plus {
                let left = self.eval_const_expr(&elements[..1]);
                let right = self.eval_const_expr(&elements[2..]);
                return left + right;
            }
        }

        // Single node
        if let SyntaxElement::Node(n) = &elements[0] {
            let sub: Vec<SyntaxElement> = n
                .children_with_tokens()
                .filter(|el| {
                    el.kind() != SyntaxKind::Whitespace && el.kind() != SyntaxKind::Comment
                })
                .collect();
            return self.eval_const_expr(&sub);
        }

        0
    }

    // ====================================================================
    // Literal parsing helpers
    // ====================================================================

    fn parse_bit_string_literal(&self, text: &str) -> HirLiteral {
        // X"FF", B"1010", O"77"
        if text.len() < 3 {
            return HirLiteral::Integer(0);
        }
        let prefix = text.chars().next().unwrap().to_ascii_lowercase();
        let inner = text[2..text.len() - 1].replace('_', "");

        let val = match prefix {
            'x' => u64::from_str_radix(&inner, 16).unwrap_or(0),
            'b' => u64::from_str_radix(&inner, 2).unwrap_or(0),
            'o' => u64::from_str_radix(&inner, 8).unwrap_or(0),
            'd' => inner.parse::<u64>().unwrap_or(0),
            _ => 0,
        };
        HirLiteral::Integer(val)
    }

    fn parse_based_literal(&self, text: &str) -> u64 {
        // 16#FF#, 2#1010#
        let parts: Vec<&str> = text.split('#').collect();
        if parts.len() >= 2 {
            let base = parts[0].parse::<u32>().unwrap_or(10);
            let digits = parts[1].replace('_', "");
            u64::from_str_radix(&digits, base).unwrap_or(0)
        } else {
            0
        }
    }

    // ====================================================================
    // Misc helpers
    // ====================================================================

    fn extract_default_value(&mut self, node: &SyntaxNode) -> Option<HirExpression> {
        // Look for := followed by expression
        let elements: Vec<SyntaxElement> = node.children_with_tokens().collect();
        let assign_pos = elements.iter().position(|el| el.kind() == SyntaxKind::VarAssign)?;

        let value_elements: Vec<SyntaxElement> = elements[assign_pos + 1..]
            .iter()
            .filter(|el| {
                el.kind() != SyntaxKind::Whitespace
                    && el.kind() != SyntaxKind::Comment
                    && el.kind() != SyntaxKind::Semicolon
            })
            .cloned()
            .collect();

        if value_elements.is_empty() {
            return None;
        }

        Some(self.lower_expr_from_elements(&value_elements))
    }

    fn extract_range_size(&mut self, node: &SyntaxNode) -> Option<u32> {
        let ri = self.subtype_indication_range(node)?;
        Some(ri.width())
    }

    fn name_first_text(&self, node: &SyntaxNode) -> Option<String> {
        node.children_with_tokens().find_map(|el| match el {
            SyntaxElement::Token(t)
                if t.kind() != SyntaxKind::Whitespace && t.kind() != SyntaxKind::Comment =>
            {
                Some(t.text().to_ascii_lowercase())
            }
            _ => None,
        })
    }

    fn lower_stmts_from_elements(&mut self, elements: &[SyntaxElement]) -> Vec<HirStatement> {
        let mut stmts = Vec::new();
        for el in elements {
            if let SyntaxElement::Node(n) = el {
                match n.kind() {
                    SyntaxKind::IfStmt => {
                        if let Some(s) = self.lower_if_stmt(n) {
                            stmts.push(s);
                        }
                    }
                    SyntaxKind::CaseStmt => {
                        if let Some(s) = self.lower_case_stmt(n) {
                            stmts.push(s);
                        }
                    }
                    SyntaxKind::ForLoopStmt => {
                        if let Some(s) = self.lower_for_loop(n) {
                            stmts.push(s);
                        }
                    }
                    SyntaxKind::SequentialSignalAssign | SyntaxKind::VariableAssignStmt => {
                        if let Some(a) = self.lower_sequential_assign(n) {
                            stmts.push(HirStatement::Assignment(a));
                        }
                    }
                    SyntaxKind::NullStmt => {}
                    _ => {
                        // Try recursive
                        let sub = self.lower_sequential_statements(n);
                        stmts.extend(sub);
                    }
                }
            }
        }
        stmts
    }

}

#[derive(Clone)]
struct IfSection {
    kind: IfSectionKind,
    condition_elements: Vec<SyntaxElement>,
    body_elements: Vec<SyntaxElement>,
}

#[derive(Clone, Copy)]
enum IfSectionKind {
    If,
    Elsif,
    Else,
}

impl IfSection {
    fn new(kind: IfSectionKind) -> Self {
        Self {
            kind,
            condition_elements: Vec::new(),
            body_elements: Vec::new(),
        }
    }
}
