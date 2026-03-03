//! HIR-based schematic data extraction for the schematic viewer.
//!
//! Parses source (skalp or VHDL) → HIR, then extracts structural data
//! (ports, signals, instances, assignments, nets) for visualization.

use crate::FileLanguage;
use serde::Serialize;
use skalp_frontend::hir::*;
use skalp_frontend::Hir;
use std::collections::{HashMap, HashSet};

// ============================================================================
// Serializable types matching the TypeScript interfaces
// ============================================================================

#[derive(Serialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct SchematicData {
    pub entity_name: String,
    pub ports: Vec<SchematicPort>,
    pub signals: Vec<SchematicSignal>,
    pub instances: Vec<SchematicInstance>,
    pub assignments: Vec<SchematicAssignment>,
    pub nets: Vec<SchematicNet>,
    pub entity_line: i32,
    pub impl_line: i32,
}

#[derive(Serialize, Clone)]
pub struct SchematicPort {
    pub name: String,
    pub direction: String,
    #[serde(rename = "type")]
    pub type_str: String,
    pub width: u32,
    pub line: i32,
}

#[derive(Serialize, Clone)]
pub struct SchematicSignal {
    pub name: String,
    #[serde(rename = "type")]
    pub type_str: String,
    pub width: u32,
    pub line: i32,
}

#[derive(Serialize, Clone)]
pub struct SchematicConnection {
    pub port: String,
    pub signal: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub direction: Option<String>,
}

#[derive(Serialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct SchematicInstance {
    pub name: String,
    pub entity_type: String,
    pub connections: Vec<SchematicConnection>,
    pub line: i32,
}

#[derive(Serialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct SchematicAssignment {
    pub lhs: String,
    pub rhs: String,
    pub line: i32,
    pub is_output_port: bool,
}

#[derive(Serialize, Clone)]
pub struct SchematicEndpoint {
    #[serde(rename = "type")]
    pub endpoint_type: String, // "entity_port" | "instance"
    pub name: String,
    pub port: String,
}

#[derive(Serialize, Clone)]
pub struct SchematicNet {
    pub name: String,
    pub width: u32,
    pub driver: SchematicEndpoint,
    pub sinks: Vec<SchematicEndpoint>,
}

// ============================================================================
// Name resolution context
// ============================================================================

struct NameCtx {
    port_names: HashMap<PortId, String>,
    port_directions: HashMap<String, HirPortDirection>,
    signal_names: HashMap<SignalId, String>,
    variable_names: HashMap<VariableId, String>,
    entity_names: HashMap<EntityId, String>,
}

impl NameCtx {
    fn from_hir(hir: &Hir, entity: &HirEntity, imp: Option<&HirImplementation>) -> Self {
        let mut port_names = HashMap::new();
        let mut port_directions = HashMap::new();
        let mut signal_names = HashMap::new();
        let mut variable_names = HashMap::new();
        let mut entity_names = HashMap::new();

        for e in &hir.entities {
            entity_names.insert(e.id, e.name.clone());
        }

        for port in &entity.ports {
            port_names.insert(port.id, port.name.clone());
            port_directions.insert(port.name.clone(), port.direction.clone());
        }

        if let Some(imp) = imp {
            for signal in &imp.signals {
                signal_names.insert(signal.id, signal.name.clone());
            }
            for variable in &imp.variables {
                variable_names.insert(variable.id, variable.name.clone());
            }
        }

        Self {
            port_names,
            port_directions,
            signal_names,
            variable_names,
            entity_names,
        }
    }

    /// Resolve an lvalue to a string name (iterative — walks the chain)
    fn resolve_lvalue(&self, lv: &HirLValue) -> String {
        let mut current = lv;
        let mut suffix_parts: Vec<&str> = Vec::new();
        loop {
            match current {
                HirLValue::Signal(id) => {
                    let base = self.signal_names.get(id).cloned().unwrap_or_default();
                    return self.join_with_suffix(base, &suffix_parts);
                }
                HirLValue::Variable(id) => {
                    let base = self.variable_names.get(id).cloned().unwrap_or_default();
                    return self.join_with_suffix(base, &suffix_parts);
                }
                HirLValue::Port(id) => {
                    let base = self.port_names.get(id).cloned().unwrap_or_default();
                    return self.join_with_suffix(base, &suffix_parts);
                }
                HirLValue::FieldAccess { base, field } => {
                    suffix_parts.push(field);
                    current = base;
                }
                HirLValue::Index(base, _) => {
                    current = base;
                }
                HirLValue::Range(base, _, _) => {
                    current = base;
                }
            }
        }
    }

    fn join_with_suffix(&self, base: String, suffix_parts: &[&str]) -> String {
        if suffix_parts.is_empty() {
            return base;
        }
        let mut result = base;
        for part in suffix_parts.iter().rev() {
            result.push('.');
            result.push_str(part);
        }
        result
    }

    /// Convert expression to string with depth limit to prevent stack overflow
    fn expr_to_string(&self, expr: &HirExpression) -> String {
        self.expr_to_string_depth(expr, 0)
    }

    fn expr_to_string_depth(&self, expr: &HirExpression, depth: usize) -> String {
        const MAX_DEPTH: usize = 64;
        if depth >= MAX_DEPTH {
            return "...".to_string();
        }
        let d = depth + 1;
        match expr {
            HirExpression::Port(id) => self.port_names.get(id).cloned().unwrap_or_default(),
            HirExpression::Signal(id) => self.signal_names.get(id).cloned().unwrap_or_default(),
            HirExpression::Variable(id) => self.variable_names.get(id).cloned().unwrap_or_default(),
            HirExpression::Constant(id) => format!("const_{}", id.0),
            HirExpression::Literal(lit) => format!("{:?}", lit),
            HirExpression::FieldAccess { base, field } => {
                format!("{}.{}", self.expr_to_string_depth(base, d), field)
            }
            HirExpression::Cast(c) => self.expr_to_string_depth(&c.expr, d),
            HirExpression::Binary(b) => {
                format!(
                    "{} {:?} {}",
                    self.expr_to_string_depth(&b.left, d),
                    b.op,
                    self.expr_to_string_depth(&b.right, d)
                )
            }
            HirExpression::Unary(u) => {
                format!("{:?} {}", u.op, self.expr_to_string_depth(&u.operand, d))
            }
            HirExpression::Index(base, idx) => {
                format!(
                    "{}[{}]",
                    self.expr_to_string_depth(base, d),
                    self.expr_to_string_depth(idx, d)
                )
            }
            HirExpression::Concat(parts) => {
                let strs: Vec<_> = parts
                    .iter()
                    .map(|p| self.expr_to_string_depth(p, d))
                    .collect();
                strs.join(" ++ ")
            }
            HirExpression::Call(c) => {
                let args: Vec<_> = c
                    .args
                    .iter()
                    .map(|a| self.expr_to_string_depth(a, d))
                    .collect();
                format!("{}({})", c.function, args.join(", "))
            }
            HirExpression::StructLiteral(sl) => {
                let fields: Vec<_> = sl
                    .fields
                    .iter()
                    .map(|f| format!("{}: {}", f.name, self.expr_to_string_depth(&f.value, d)))
                    .collect();
                format!("{} {{ {} }}", sl.type_name, fields.join(", "))
            }
            HirExpression::Ternary {
                condition,
                true_expr,
                false_expr,
            } => format!(
                "if {} then {} else {}",
                self.expr_to_string_depth(condition, d),
                self.expr_to_string_depth(true_expr, d),
                self.expr_to_string_depth(false_expr, d)
            ),
            HirExpression::GenericParam(name) => name.clone(),
            HirExpression::EnumVariant {
                enum_type, variant, ..
            } => format!("{}::{}", enum_type, variant),
            _ => "...".to_string(),
        }
    }

    /// Collect all signal/port names referenced in an expression (iterative)
    fn collect_referenced_names(&self, expr: &HirExpression, names: &mut HashSet<String>) {
        let mut stack: Vec<&HirExpression> = vec![expr];

        while let Some(e) = stack.pop() {
            match e {
                HirExpression::Port(id) => {
                    if let Some(name) = self.port_names.get(id) {
                        names.insert(name.clone());
                    }
                }
                HirExpression::Signal(id) => {
                    if let Some(name) = self.signal_names.get(id) {
                        names.insert(name.clone());
                    }
                }
                HirExpression::Variable(id) => {
                    if let Some(name) = self.variable_names.get(id) {
                        names.insert(name.clone());
                    }
                }
                HirExpression::Binary(b) => {
                    stack.push(&b.left);
                    stack.push(&b.right);
                }
                HirExpression::Unary(u) => {
                    stack.push(&u.operand);
                }
                HirExpression::Cast(c) => {
                    stack.push(&c.expr);
                }
                HirExpression::FieldAccess { base, .. } => {
                    stack.push(base);
                }
                HirExpression::Index(base, idx) => {
                    stack.push(base);
                    stack.push(idx);
                }
                HirExpression::Range(base, lo, hi) => {
                    stack.push(base);
                    stack.push(lo);
                    stack.push(hi);
                }
                HirExpression::Concat(parts) => {
                    for p in parts {
                        stack.push(p);
                    }
                }
                HirExpression::Call(c) => {
                    for a in &c.args {
                        stack.push(a);
                    }
                }
                HirExpression::Ternary {
                    condition,
                    true_expr,
                    false_expr,
                } => {
                    stack.push(condition);
                    stack.push(true_expr);
                    stack.push(false_expr);
                }
                HirExpression::StructLiteral(sl) => {
                    for f in &sl.fields {
                        stack.push(&f.value);
                    }
                }
                HirExpression::If(if_expr) => {
                    stack.push(&if_expr.condition);
                    stack.push(&if_expr.then_expr);
                    stack.push(&if_expr.else_expr);
                }
                HirExpression::ArrayLiteral(elems) | HirExpression::TupleLiteral(elems) => {
                    for e in elems {
                        stack.push(e);
                    }
                }
                HirExpression::Match(m) => {
                    stack.push(&m.expr);
                    for arm in &m.arms {
                        stack.push(&arm.expr);
                    }
                }
                HirExpression::Block {
                    statements,
                    result_expr,
                } => {
                    // For block expressions, only collect from the result expression
                    // Statement assignments are handled by collect_stmt_refs
                    stack.push(result_expr);
                    for stmt in statements {
                        if let HirStatement::Assignment(a) = stmt {
                            stack.push(&a.rhs);
                        }
                    }
                }
                _ => {}
            }
        }
    }

    /// Collect all names referenced in statements (iterative using explicit stack)
    fn collect_stmt_refs(
        &self,
        stmts: &[HirStatement],
        assigned: &mut HashSet<String>,
        referenced: &mut HashSet<String>,
    ) {
        let mut stmt_stack: Vec<&[HirStatement]> = vec![stmts];

        while let Some(current_stmts) = stmt_stack.pop() {
            for stmt in current_stmts {
                match stmt {
                    HirStatement::Assignment(a) => {
                        let lhs_name = self
                            .resolve_lvalue(&a.lhs)
                            .split('.')
                            .next()
                            .unwrap_or("")
                            .to_string();
                        assigned.insert(lhs_name);
                        self.collect_referenced_names(&a.rhs, referenced);
                    }
                    HirStatement::If(if_stmt) => {
                        self.collect_referenced_names(&if_stmt.condition, referenced);
                        stmt_stack.push(&if_stmt.then_statements);
                        if let Some(else_stmts) = &if_stmt.else_statements {
                            stmt_stack.push(else_stmts);
                        }
                    }
                    HirStatement::Match(m) => {
                        self.collect_referenced_names(&m.expr, referenced);
                        for arm in &m.arms {
                            stmt_stack.push(&arm.statements);
                        }
                    }
                    HirStatement::For(f) => {
                        stmt_stack.push(&f.body);
                    }
                    HirStatement::Block(inner_stmts) => {
                        stmt_stack.push(inner_stmts);
                    }
                    _ => {}
                }
            }
        }
    }
}

// ============================================================================
// Type utilities
// ============================================================================

fn hir_type_to_string(ty: &HirType) -> String {
    match ty {
        HirType::Bit(1) => "bit".to_string(),
        HirType::Bit(n) => format!("bit<{}>", n),
        HirType::Bool => "bool".to_string(),
        HirType::Clock(_) => "clock".to_string(),
        HirType::Reset { .. } => "reset".to_string(),
        HirType::Nat(n) => format!("nat<{}>", n),
        HirType::Int(n) => format!("int<{}>", n),
        HirType::Logic(1) => "logic".to_string(),
        HirType::Logic(n) => format!("logic<{}>", n),
        HirType::Custom(name) => name.clone(),
        HirType::Array(inner, size) => format!("[{}; {}]", hir_type_to_string(inner), size),
        HirType::Float32 => "fp32".to_string(),
        HirType::Float16 => "fp16".to_string(),
        HirType::Float64 => "fp64".to_string(),
        HirType::String => "string".to_string(),
        HirType::Event => "event".to_string(),
        HirType::Stream(inner) => format!("stream<{}>", hir_type_to_string(inner)),
        _ => format!("{:?}", ty),
    }
}

fn infer_width(type_str: &str) -> u32 {
    let t = type_str.trim();
    if t == "bit" || t == "bool" || t == "clock" || t == "reset" {
        return 1;
    }
    if t == "fp32" {
        return 32;
    }
    if t == "fp16" {
        return 16;
    }
    if t == "fp64" {
        return 64;
    }
    // Extract width from bit<N>, nat<N>, int<N>, logic<N>
    if let Some(n) = t
        .strip_prefix("bit<")
        .or_else(|| t.strip_prefix("nat<"))
        .or_else(|| t.strip_prefix("int<"))
        .or_else(|| t.strip_prefix("logic<"))
    {
        if let Some(n) = n.strip_suffix('>') {
            if let Ok(w) = n.parse::<u32>() {
                return w;
            }
        }
    }
    // Custom types — assume bus width
    if t.starts_with(|c: char| c.is_ascii_uppercase()) {
        return 8;
    }
    1
}

fn hir_type_width(ty: &HirType) -> u32 {
    match ty {
        HirType::Bit(n) | HirType::Logic(n) | HirType::Int(n) | HirType::Nat(n) => *n,
        HirType::Bool => 1,
        HirType::Clock(_) | HirType::Reset { .. } => 1,
        HirType::Float32 => 32,
        HirType::Float16 => 16,
        HirType::Float64 => 64,
        _ => {
            let s = hir_type_to_string(ty);
            infer_width(&s)
        }
    }
}

// ============================================================================
// Main entry point
// ============================================================================

pub fn get_schematic(
    source: &str,
    cursor_line: u32,
    language: FileLanguage,
) -> Option<SchematicData> {
    let hir = parse_to_hir(source, language)?;

    // Find entity at cursor
    let (entity, imp) = find_entity_at_cursor(&hir, cursor_line)?;

    let nctx = NameCtx::from_hir(&hir, entity, imp);

    let entity_line = entity
        .span
        .as_ref()
        .map(|s| s.line.saturating_sub(1) as i32)
        .unwrap_or(0);

    // Extract ports
    let ports: Vec<SchematicPort> = entity
        .ports
        .iter()
        .map(|p| {
            let type_str = hir_type_to_string(&p.port_type);
            let width = hir_type_width(&p.port_type);
            SchematicPort {
                name: p.name.clone(),
                direction: match p.direction {
                    HirPortDirection::Input => "in",
                    HirPortDirection::Output => "out",
                    HirPortDirection::Bidirectional => "inout",
                    HirPortDirection::Protocol => "in",
                }
                .to_string(),
                type_str,
                width,
                line: entity_line,
            }
        })
        .collect();

    let Some(imp) = imp else {
        return Some(SchematicData {
            entity_name: entity.name.clone(),
            ports,
            signals: vec![],
            instances: vec![],
            assignments: vec![],
            nets: vec![],
            entity_line,
            impl_line: -1,
        });
    };

    // Extract signals
    let signals: Vec<SchematicSignal> = imp
        .signals
        .iter()
        .map(|s| {
            let type_str = hir_type_to_string(&s.signal_type);
            let width = hir_type_width(&s.signal_type);
            let line = s
                .span
                .as_ref()
                .map(|sp| sp.line.saturating_sub(1) as i32)
                .unwrap_or(-1);
            SchematicSignal {
                name: s.name.clone(),
                type_str,
                width,
                line,
            }
        })
        .collect();

    // Extract instances
    let mut instances: Vec<SchematicInstance> = imp
        .instances
        .iter()
        .map(|inst| {
            let entity_type = nctx
                .entity_names
                .get(&inst.entity)
                .cloned()
                .unwrap_or_else(|| format!("entity_{}", inst.entity.0));
            let connections: Vec<SchematicConnection> = inst
                .connections
                .iter()
                .map(|c| SchematicConnection {
                    port: c.port.clone(),
                    signal: nctx.expr_to_string(&c.expr),
                    direction: None,
                })
                .collect();
            SchematicInstance {
                name: inst.name.clone(),
                entity_type,
                connections,
                line: -1,
            }
        })
        .collect();

    // Extract combinational assignments
    let input_port_names: HashSet<String> = entity
        .ports
        .iter()
        .filter(|p| matches!(p.direction, HirPortDirection::Input))
        .map(|p| p.name.clone())
        .collect();
    let output_port_names: HashSet<String> = entity
        .ports
        .iter()
        .filter(|p| matches!(p.direction, HirPortDirection::Output))
        .map(|p| p.name.clone())
        .collect();
    let port_names: HashSet<String> = entity.ports.iter().map(|p| p.name.clone()).collect();

    let mut assignments: Vec<SchematicAssignment> = Vec::new();
    for a in &imp.assignments {
        let lhs = nctx.resolve_lvalue(&a.lhs);
        let rhs = nctx.expr_to_string(&a.rhs);
        let lhs_base = lhs.split('.').next().unwrap_or(&lhs).to_string();
        assignments.push(SchematicAssignment {
            lhs: lhs.clone(),
            rhs,
            line: -1,
            is_output_port: port_names.contains(&lhs_base),
        });
    }

    // Analyze on-blocks (event blocks) for sequential logic references
    let mut on_block_input_ports = HashSet::new();
    let mut on_block_output_ports = HashSet::new();
    let mut on_block_signals = HashSet::new();

    for eb in &imp.event_blocks {
        let mut assigned = HashSet::new();
        let mut referenced = HashSet::new();
        nctx.collect_stmt_refs(&eb.statements, &mut assigned, &mut referenced);

        for name in &assigned {
            if output_port_names.contains(name) {
                on_block_output_ports.insert(name.clone());
            } else if !input_port_names.contains(name) {
                on_block_signals.insert(name.clone());
            }
        }
        for name in &referenced {
            if input_port_names.contains(name) {
                on_block_input_ports.insert(name.clone());
            }
        }
    }

    // Also scan combinational assignments for field-access input port references
    for a in &assignments {
        for port_name in &input_port_names {
            if a.rhs.contains(&format!("{}.", port_name)) {
                on_block_input_ports.insert(port_name.clone());
            }
        }
    }

    // Collect on-block output ports AND complex-assignment output ports
    let mut logic_output_ports = on_block_output_ports.clone();
    for a in &assignments {
        if !a.is_output_port {
            continue;
        }
        let out_name = a.lhs.split('.').next().unwrap_or(&a.lhs).to_string();
        if logic_output_ports.contains(&out_name) {
            continue;
        }
        // If RHS is not a simple identifier, it's complex logic
        let rhs = a.rhs.trim();
        if !rhs.chars().all(|c| c.is_alphanumeric() || c == '_') {
            logic_output_ports.insert(out_name);
        }
    }

    // Build connection graph
    let nets = build_connection_graph(
        &ports,
        &signals,
        &mut instances,
        &assignments,
        &on_block_input_ports,
        &logic_output_ports,
        &on_block_signals,
    );

    Some(SchematicData {
        entity_name: entity.name.clone(),
        ports,
        signals,
        instances,
        assignments,
        nets,
        entity_line,
        impl_line: -1,
    })
}

// ============================================================================
// Parsing
// ============================================================================

fn parse_to_hir(source: &str, language: FileLanguage) -> Option<Hir> {
    match language {
        FileLanguage::Vhdl => skalp_vhdl::parse_vhdl_source(source, None).ok(),
        FileLanguage::Skalp => skalp_frontend::parse_and_build_hir(source).ok(),
    }
}

fn find_entity_at_cursor(
    hir: &Hir,
    cursor_line: u32,
) -> Option<(&HirEntity, Option<&HirImplementation>)> {
    if hir.entities.is_empty() {
        return None;
    }

    // Try to find entity whose span contains cursor
    let mut target_entity = None;
    for entity in &hir.entities {
        if let Some(ref span) = entity.span {
            let entity_line = span.line.saturating_sub(1) as u32;
            // Check if cursor is near the entity (within a reasonable range)
            if cursor_line >= entity_line && cursor_line <= entity_line + 500 {
                target_entity = Some(entity);
                break;
            }
        }
    }

    // Also check if cursor is within an impl block's range
    if target_entity.is_none() {
        for imp in &hir.implementations {
            let imp_entity = hir.entities.iter().find(|e| e.id == imp.entity);
            if imp_entity.is_some() {
                // Check signals for span proximity
                for sig in &imp.signals {
                    if let Some(ref span) = sig.span {
                        let sig_line = span.line.saturating_sub(1) as u32;
                        if cursor_line >= sig_line.saturating_sub(50)
                            && cursor_line <= sig_line + 500
                        {
                            target_entity = imp_entity;
                            break;
                        }
                    }
                }
                if target_entity.is_some() {
                    break;
                }
            }
        }
    }

    // Default to first entity
    let entity = target_entity.unwrap_or(&hir.entities[0]);

    // Find matching implementation
    let imp = hir.implementations.iter().find(|i| i.entity == entity.id);

    Some((entity, imp))
}

// ============================================================================
// Connection graph builder
// ============================================================================

fn build_connection_graph(
    ports: &[SchematicPort],
    signals: &[SchematicSignal],
    instances: &mut Vec<SchematicInstance>,
    assignments: &[SchematicAssignment],
    on_block_input_ports: &HashSet<String>,
    logic_output_ports: &HashSet<String>,
    on_block_signals: &HashSet<String>,
) -> Vec<SchematicNet> {
    let mut nets = Vec::new();
    let input_port_names: HashSet<&str> = ports
        .iter()
        .filter(|p| p.direction == "in")
        .map(|p| p.name.as_str())
        .collect();
    let output_port_names: HashSet<&str> = ports
        .iter()
        .filter(|p| p.direction == "out")
        .map(|p| p.name.as_str())
        .collect();

    // Set of signals driven by combinational assignments
    let driven_signals: HashSet<String> = assignments
        .iter()
        .map(|a| a.lhs.split('.').next().unwrap_or(&a.lhs).to_string())
        .collect();

    // Direction inference for instance connections
    for inst in instances.iter_mut() {
        for conn in &mut inst.connections {
            let sig = &conn.signal;
            let is_simple = sig.chars().all(|c| c.is_alphanumeric() || c == '_');

            let direction = if !is_simple
                || input_port_names.contains(sig.as_str())
                || driven_signals.contains(sig)
            {
                "in"
            } else {
                "out"
            };

            conn.direction = Some(direction.to_string());
        }
    }

    // Helper: extract tokens from a string
    let extract_tokens = |s: &str| -> Vec<String> {
        s.split(|c: char| !c.is_alphanumeric() && c != '_')
            .filter(|t| !t.is_empty())
            .map(|t| t.to_string())
            .collect()
    };

    // 1. Entity input ports → instance inputs that reference them
    for port in ports {
        if port.direction != "in" {
            continue;
        }
        let mut sinks = Vec::new();
        for inst in instances.iter() {
            for conn in &inst.connections {
                if conn.direction.as_deref() != Some("in") {
                    continue;
                }
                if conn.signal == port.name || conn.signal.starts_with(&format!("{}.", port.name)) {
                    sinks.push(SchematicEndpoint {
                        endpoint_type: "instance".to_string(),
                        name: inst.name.clone(),
                        port: conn.port.clone(),
                    });
                } else {
                    let tokens = extract_tokens(&conn.signal);
                    if tokens.contains(&port.name) {
                        sinks.push(SchematicEndpoint {
                            endpoint_type: "instance".to_string(),
                            name: inst.name.clone(),
                            port: conn.port.clone(),
                        });
                    }
                }
            }
        }
        if !sinks.is_empty() {
            nets.push(SchematicNet {
                name: port.name.clone(),
                width: port.width,
                driver: SchematicEndpoint {
                    endpoint_type: "entity_port".to_string(),
                    name: port.name.clone(),
                    port: port.name.clone(),
                },
                sinks,
            });
        }
    }

    // 2. Instance outputs → entity output ports and/or other instance inputs
    for inst_idx in 0..instances.len() {
        let inst = &instances[inst_idx];
        for conn_idx in 0..inst.connections.len() {
            let conn = &inst.connections[conn_idx];
            if conn.direction.as_deref() != Some("out") {
                continue;
            }
            let sig = conn.signal.clone();
            let inst_name = inst.name.clone();
            let conn_port = conn.port.clone();
            let mut sinks = Vec::new();

            // Direct connection to entity output port
            if output_port_names.contains(sig.as_str()) {
                sinks.push(SchematicEndpoint {
                    endpoint_type: "entity_port".to_string(),
                    name: sig.clone(),
                    port: sig.clone(),
                });
            }

            // Check if referenced in output port assignments
            for a in assignments {
                if !a.is_output_port {
                    continue;
                }
                let out_name = a.lhs.split('.').next().unwrap_or(&a.lhs).to_string();
                if sinks
                    .iter()
                    .any(|s| s.endpoint_type == "entity_port" && s.name == out_name)
                {
                    continue;
                }
                let rhs_tokens = extract_tokens(&a.rhs);
                if a.rhs == sig || rhs_tokens.contains(&sig) {
                    sinks.push(SchematicEndpoint {
                        endpoint_type: "entity_port".to_string(),
                        name: out_name,
                        port: a.lhs.split('.').next().unwrap_or(&a.lhs).to_string(),
                    });
                }
            }

            // Check if referenced in non-output assignments that chain to output ports
            for a in assignments {
                if a.is_output_port {
                    continue;
                }
                let rhs_tokens = extract_tokens(&a.rhs);
                if !rhs_tokens.contains(&sig) {
                    continue;
                }
                let derived = a.lhs.split('.').next().unwrap_or(&a.lhs).to_string();

                // Check if derived signal feeds an output port
                for oa in assignments {
                    if !oa.is_output_port {
                        continue;
                    }
                    let out_name = oa.lhs.split('.').next().unwrap_or(&oa.lhs).to_string();
                    if sinks
                        .iter()
                        .any(|s| s.endpoint_type == "entity_port" && s.name == out_name)
                    {
                        continue;
                    }
                    let oa_tokens = extract_tokens(&oa.rhs);
                    if oa.rhs == derived || oa_tokens.contains(&derived) {
                        sinks.push(SchematicEndpoint {
                            endpoint_type: "entity_port".to_string(),
                            name: out_name,
                            port: oa.lhs.split('.').next().unwrap_or(&oa.lhs).to_string(),
                        });
                    }
                }

                // Check if derived signal feeds another instance
                for other in instances.iter() {
                    if other.name == inst_name {
                        continue;
                    }
                    for oc in &other.connections {
                        if oc.direction.as_deref() != Some("in") {
                            continue;
                        }
                        if oc.signal == derived {
                            sinks.push(SchematicEndpoint {
                                endpoint_type: "instance".to_string(),
                                name: other.name.clone(),
                                port: oc.port.clone(),
                            });
                        }
                    }
                }
            }

            // Check consumed by other instances
            for other in instances.iter() {
                if other.name == inst_name {
                    continue;
                }
                for oc in &other.connections {
                    if oc.direction.as_deref() != Some("in") {
                        continue;
                    }
                    if sinks.iter().any(|s| {
                        s.endpoint_type == "instance" && s.name == other.name && s.port == oc.port
                    }) {
                        continue;
                    }
                    if oc.signal == sig {
                        sinks.push(SchematicEndpoint {
                            endpoint_type: "instance".to_string(),
                            name: other.name.clone(),
                            port: oc.port.clone(),
                        });
                    } else {
                        let tokens = extract_tokens(&oc.signal);
                        if tokens.contains(&sig) {
                            sinks.push(SchematicEndpoint {
                                endpoint_type: "instance".to_string(),
                                name: other.name.clone(),
                                port: oc.port.clone(),
                            });
                        }
                    }
                }
            }

            if !sinks.is_empty() {
                let sig_info = signals.iter().find(|s| s.name == sig);
                nets.push(SchematicNet {
                    name: sig.clone(),
                    width: sig_info.map(|s| s.width).unwrap_or(1),
                    driver: SchematicEndpoint {
                        endpoint_type: "instance".to_string(),
                        name: inst_name.clone(),
                        port: conn_port.clone(),
                    },
                    sinks,
                });
            }
        }
    }

    let mut covered_signals: HashSet<String> = nets.iter().map(|n| n.name.clone()).collect();

    // 3. Input port → assignment chain → instance (multi-hop)
    for port in ports {
        if port.direction != "in" {
            continue;
        }
        if covered_signals.contains(&port.name) {
            continue;
        }

        let mut all_derived = HashSet::new();
        for a in assignments {
            if a.is_output_port {
                continue;
            }
            let rhs_tokens = extract_tokens(&a.rhs);
            if rhs_tokens.contains(&port.name) || a.rhs.contains(&format!("{}.", port.name)) {
                all_derived.insert(a.lhs.split('.').next().unwrap_or(&a.lhs).to_string());
            }
        }

        // Follow chains transitively (up to 4 hops)
        for _ in 0..4 {
            let mut new_derived = Vec::new();
            for a in assignments {
                if a.is_output_port {
                    continue;
                }
                let lhs_sig = a.lhs.split('.').next().unwrap_or(&a.lhs).to_string();
                if all_derived.contains(&lhs_sig) {
                    continue;
                }
                let rhs_tokens = extract_tokens(&a.rhs);
                for d in &all_derived {
                    if rhs_tokens.contains(d) {
                        new_derived.push(lhs_sig.clone());
                        break;
                    }
                }
            }
            if new_derived.is_empty() {
                break;
            }
            for d in new_derived {
                all_derived.insert(d);
            }
        }

        let mut sinks = Vec::new();
        for derived in &all_derived {
            for inst in instances.iter() {
                for conn in &inst.connections {
                    if conn.signal == *derived && conn.direction.as_deref() == Some("in") {
                        sinks.push(SchematicEndpoint {
                            endpoint_type: "instance".to_string(),
                            name: inst.name.clone(),
                            port: conn.port.clone(),
                        });
                    }
                }
            }
        }

        if !sinks.is_empty() {
            nets.push(SchematicNet {
                name: port.name.clone(),
                width: port.width,
                driver: SchematicEndpoint {
                    endpoint_type: "entity_port".to_string(),
                    name: port.name.clone(),
                    port: port.name.clone(),
                },
                sinks,
            });
            covered_signals.insert(port.name.clone());
        }
    }

    // 5. Output port assignments from internal signals
    for a in assignments {
        if !a.is_output_port {
            continue;
        }
        let out_port_name = a.lhs.split('.').next().unwrap_or(&a.lhs).to_string();
        if covered_signals.contains(&out_port_name) {
            continue;
        }

        let out_port = match ports
            .iter()
            .find(|p| p.name == out_port_name && p.direction == "out")
        {
            Some(p) => p,
            None => continue,
        };

        let rhs = a.rhs.trim();
        let rhs_is_simple = rhs.chars().all(|c| c.is_alphanumeric() || c == '_');
        let sink = SchematicEndpoint {
            endpoint_type: "entity_port".to_string(),
            name: out_port_name.clone(),
            port: out_port_name.clone(),
        };

        if rhs_is_simple {
            let mut is_inst_output = false;
            for inst in instances.iter() {
                for conn in &inst.connections {
                    if conn.signal == rhs && conn.direction.as_deref() == Some("out") {
                        is_inst_output = true;
                    }
                }
            }
            if is_inst_output {
                continue;
            }
        }

        let rhs_tokens = extract_tokens(rhs);
        let sig_info = if rhs_is_simple {
            signals.iter().find(|s| s.name == rhs)
        } else {
            None
        };

        // Check if from instance output
        let mut source_inst = None;
        'inst_search: for inst in instances.iter() {
            for conn in &inst.connections {
                if conn.direction.as_deref() != Some("out") {
                    continue;
                }
                if rhs_tokens.contains(&conn.signal) {
                    source_inst = Some((inst.name.clone(), conn.port.clone()));
                    break 'inst_search;
                }
            }
        }

        // Check if from input port
        let mut source_port = None;
        if source_inst.is_none() {
            for tok in &rhs_tokens {
                if let Some(p) = ports
                    .iter()
                    .find(|pp| pp.name == *tok && pp.direction == "in")
                {
                    source_port = Some(p);
                    break;
                }
            }
        }

        if let Some((inst_name, inst_port)) = source_inst {
            nets.push(SchematicNet {
                name: if rhs_is_simple {
                    rhs.to_string()
                } else {
                    out_port_name.clone()
                },
                width: sig_info.map(|s| s.width).unwrap_or(out_port.width),
                driver: SchematicEndpoint {
                    endpoint_type: "instance".to_string(),
                    name: inst_name,
                    port: inst_port,
                },
                sinks: vec![sink],
            });
        } else if let Some(src_port) = source_port {
            nets.push(SchematicNet {
                name: if rhs_is_simple {
                    rhs.to_string()
                } else {
                    out_port_name.clone()
                },
                width: src_port.width,
                driver: SchematicEndpoint {
                    endpoint_type: "entity_port".to_string(),
                    name: src_port.name.clone(),
                    port: src_port.name.clone(),
                },
                sinks: vec![sink],
            });
        } else {
            if logic_output_ports.contains(&out_port_name) {
                continue;
            }
            nets.push(SchematicNet {
                name: if rhs_is_simple {
                    rhs.to_string()
                } else {
                    out_port_name.clone()
                },
                width: sig_info.map(|s| s.width).unwrap_or(out_port.width),
                driver: SchematicEndpoint {
                    endpoint_type: "entity_port".to_string(),
                    name: if rhs_is_simple {
                        rhs.to_string()
                    } else {
                        out_port_name.clone()
                    },
                    port: if rhs_is_simple {
                        rhs.to_string()
                    } else {
                        out_port_name.clone()
                    },
                },
                sinks: vec![sink],
            });
        }
        covered_signals.insert(out_port_name);
    }

    // 6. Signal-mediated connections (internal signals driven by combinational logic)
    let mut logic_signal_outputs = Vec::new();
    for a in assignments {
        if a.is_output_port {
            continue;
        }
        let sig = a.lhs.split('.').next().unwrap_or(&a.lhs).to_string();
        if covered_signals.contains(&sig) {
            continue;
        }

        let mut sinks = Vec::new();
        for inst in instances.iter() {
            for conn in &inst.connections {
                if conn.signal == sig && conn.direction.as_deref() == Some("in") {
                    sinks.push(SchematicEndpoint {
                        endpoint_type: "instance".to_string(),
                        name: inst.name.clone(),
                        port: conn.port.clone(),
                    });
                }
            }
        }
        for oa in assignments {
            if oa.is_output_port && oa.rhs == sig {
                let out_name = oa.lhs.split('.').next().unwrap_or(&oa.lhs).to_string();
                sinks.push(SchematicEndpoint {
                    endpoint_type: "entity_port".to_string(),
                    name: out_name.clone(),
                    port: out_name,
                });
            }
        }

        if !sinks.is_empty() {
            let mut is_inst_output = false;
            for inst in instances.iter() {
                for conn in &inst.connections {
                    if conn.signal == sig && conn.direction.as_deref() == Some("out") {
                        is_inst_output = true;
                    }
                }
            }
            if !is_inst_output {
                let sig_info = signals.iter().find(|s| s.name == sig);
                logic_signal_outputs.push((
                    sig.clone(),
                    sinks,
                    sig_info.map(|s| s.width).unwrap_or(1),
                ));
                covered_signals.insert(sig);
            }
        }
    }

    // 6b. On-block signals consumed by instance inputs
    for ob_sig in on_block_signals {
        if covered_signals.contains(ob_sig) {
            continue;
        }
        let mut sinks = Vec::new();
        for inst in instances.iter() {
            for conn in &inst.connections {
                if conn.direction.as_deref() != Some("in") {
                    continue;
                }
                if conn.signal == *ob_sig {
                    sinks.push(SchematicEndpoint {
                        endpoint_type: "instance".to_string(),
                        name: inst.name.clone(),
                        port: conn.port.clone(),
                    });
                } else {
                    let tokens = extract_tokens(&conn.signal);
                    if tokens.iter().any(|t| t == ob_sig) {
                        sinks.push(SchematicEndpoint {
                            endpoint_type: "instance".to_string(),
                            name: inst.name.clone(),
                            port: conn.port.clone(),
                        });
                    }
                }
            }
        }
        for a in assignments {
            let rhs_tokens = extract_tokens(&a.rhs);
            if !rhs_tokens.iter().any(|t| t == ob_sig) {
                continue;
            }
            if a.is_output_port {
                let out_name = a.lhs.split('.').next().unwrap_or(&a.lhs).to_string();
                sinks.push(SchematicEndpoint {
                    endpoint_type: "entity_port".to_string(),
                    name: out_name.clone(),
                    port: out_name,
                });
            }
        }
        if !sinks.is_empty() {
            logic_signal_outputs.push((ob_sig.clone(), sinks, 1));
            covered_signals.insert(ob_sig.clone());
        }
    }

    // 7. Create synthetic "Logic" instance for uncovered ports
    let mut logic_connections = Vec::new();

    for port_name in on_block_input_ports {
        if covered_signals.contains(port_name) {
            continue;
        }
        logic_connections.push(SchematicConnection {
            port: port_name.clone(),
            signal: port_name.clone(),
            direction: Some("in".to_string()),
        });
    }

    for port_name in logic_output_ports {
        if covered_signals.contains(port_name) {
            continue;
        }
        logic_connections.push(SchematicConnection {
            port: port_name.clone(),
            signal: port_name.clone(),
            direction: Some("out".to_string()),
        });
    }

    for (sig, _, _) in &logic_signal_outputs {
        logic_connections.push(SchematicConnection {
            port: sig.clone(),
            signal: sig.clone(),
            direction: Some("out".to_string()),
        });
    }

    if !logic_connections.is_empty() {
        let logic_inst = SchematicInstance {
            name: "logic".to_string(),
            entity_type: "Logic".to_string(),
            connections: logic_connections.clone(),
            line: -1,
        };
        instances.push(logic_inst);

        for conn in &logic_connections {
            if conn.direction.as_deref() == Some("in") {
                if let Some(port) = ports
                    .iter()
                    .find(|p| p.name == conn.port && p.direction == "in")
                {
                    nets.push(SchematicNet {
                        name: port.name.clone(),
                        width: port.width,
                        driver: SchematicEndpoint {
                            endpoint_type: "entity_port".to_string(),
                            name: port.name.clone(),
                            port: port.name.clone(),
                        },
                        sinks: vec![SchematicEndpoint {
                            endpoint_type: "instance".to_string(),
                            name: "logic".to_string(),
                            port: conn.port.clone(),
                        }],
                    });
                    covered_signals.insert(port.name.clone());
                }
            } else if conn.direction.as_deref() == Some("out") {
                if let Some(port) = ports
                    .iter()
                    .find(|p| p.name == conn.port && p.direction == "out")
                {
                    nets.push(SchematicNet {
                        name: port.name.clone(),
                        width: port.width,
                        driver: SchematicEndpoint {
                            endpoint_type: "instance".to_string(),
                            name: "logic".to_string(),
                            port: conn.port.clone(),
                        },
                        sinks: vec![SchematicEndpoint {
                            endpoint_type: "entity_port".to_string(),
                            name: port.name.clone(),
                            port: port.name.clone(),
                        }],
                    });
                    covered_signals.insert(port.name.clone());
                }
            }
        }

        // Nets for logic signal outputs
        for (sig, sinks, width) in logic_signal_outputs {
            nets.push(SchematicNet {
                name: sig.clone(),
                width,
                driver: SchematicEndpoint {
                    endpoint_type: "instance".to_string(),
                    name: "logic".to_string(),
                    port: sig,
                },
                sinks,
            });
        }
    }

    nets
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_entity_schematic() {
        let source = r#"
entity Counter {
    in clk: clock;
    in rst: reset(active_high);
    in enable: bit;
    out count: nat<8>;
}

impl Counter {
    signal counter_reg: nat<8>;

    on(clk.rise) {
        if rst == 1 {
            counter_reg = 0;
        } else if enable == 1 {
            counter_reg = counter_reg + 1;
        }
    }

    count = counter_reg;
}
"#;

        let data = get_schematic(source, 1, FileLanguage::Skalp);
        assert!(data.is_some());
        let data = data.unwrap();
        assert_eq!(data.entity_name, "Counter");
        assert_eq!(data.ports.len(), 4);
        assert!(data
            .ports
            .iter()
            .any(|p| p.name == "clk" && p.direction == "in"));
        assert!(data
            .ports
            .iter()
            .any(|p| p.name == "count" && p.direction == "out"));
    }

    #[test]
    fn test_signals_and_assignments() {
        let source = r#"
entity Adder {
    in a: bit<8>;
    in b: bit<8>;
    out sum: bit<8>;
}

impl Adder {
    signal internal: bit<8>;

    internal = a + b;
    sum = internal;
}
"#;

        let data = get_schematic(source, 1, FileLanguage::Skalp);
        assert!(data.is_some(), "get_schematic returned None");
        let data = data.unwrap();
        assert_eq!(data.entity_name, "Adder");
        assert_eq!(data.ports.len(), 3);
        assert!(!data.signals.is_empty());
        assert_eq!(data.signals[0].name, "internal");
        assert!(!data.assignments.is_empty());
    }

    #[test]
    fn test_no_entity_returns_none() {
        let source = "// empty file";
        let data = get_schematic(source, 0, FileLanguage::Skalp);
        assert!(data.is_none());
    }

    #[test]
    fn test_vhdl_entity_schematic() {
        let source = r#"
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity counter is
  port (
    clk : in std_logic;
    rst : in std_logic;
    count : out unsigned(7 downto 0)
  );
end entity counter;

architecture rtl of counter is
  signal count_reg : unsigned(7 downto 0);
begin
  process(clk)
  begin
    if rising_edge(clk) then
      if rst = '1' then
        count_reg <= (others => '0');
      else
        count_reg <= count_reg + 1;
      end if;
    end if;
  end process;

  count <= count_reg;
end architecture rtl;
"#;

        let data = get_schematic(source, 5, FileLanguage::Vhdl);
        assert!(data.is_some());
        let data = data.unwrap();
        // VHDL HIR lowerer preserves case from source
        assert!(data.entity_name.eq_ignore_ascii_case("counter"));
        assert!(data.ports.len() >= 2); // at least clk and count
    }
}
