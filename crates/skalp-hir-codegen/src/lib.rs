//! HIR-based code generation for skalp, VHDL, and SystemVerilog
//!
//! Generates source code directly from the HIR (High-level IR), enabling
//! transpilation between hardware description languages without going
//! through MIR lowering.

mod skalp_emit;
mod sv_emit;
mod vhdl_emit;

use anyhow::Result;
use skalp_frontend::hir::*;
use std::collections::HashMap;
use std::fmt::Write;

/// A generated output file: entity name + source code
pub struct GeneratedFile {
    pub name: String,
    pub code: String,
}

/// Generate skalp source code from HIR, one file per entity
pub fn generate_skalp_files(hir: &Hir) -> Result<Vec<GeneratedFile>> {
    let resolver = NameResolver::from_hir(hir);
    let filtered = filter_entities(hir);
    skalp_emit::emit_per_entity(&filtered, hir, &resolver)
}

/// Generate VHDL source code from HIR, one file per entity
pub fn generate_vhdl_files(hir: &Hir) -> Result<Vec<GeneratedFile>> {
    let resolver = NameResolver::from_hir(hir);
    let filtered = filter_entities(hir);
    vhdl_emit::emit_per_entity(&filtered, hir, &resolver)
}

/// Generate SystemVerilog source code from HIR, one file per entity
pub fn generate_systemverilog_files(hir: &Hir) -> Result<Vec<GeneratedFile>> {
    let resolver = NameResolver::from_hir(hir);
    let filtered = filter_entities(hir);
    sv_emit::emit_per_entity(&filtered, hir, &resolver)
}

/// Generate skalp source code from HIR (single string, for backward compat)
pub fn generate_skalp_source(hir: &Hir) -> Result<String> {
    let files = generate_skalp_files(hir)?;
    Ok(files
        .into_iter()
        .map(|f| f.code)
        .collect::<Vec<_>>()
        .join("\n"))
}

/// Generate VHDL source code from HIR (single string, for backward compat)
pub fn generate_vhdl(hir: &Hir) -> Result<String> {
    let files = generate_vhdl_files(hir)?;
    Ok(files
        .into_iter()
        .map(|f| f.code)
        .collect::<Vec<_>>()
        .join("\n"))
}

/// Generate SystemVerilog source code from HIR (single string, for backward compat)
pub fn generate_systemverilog(hir: &Hir) -> Result<String> {
    let files = generate_systemverilog_files(hir)?;
    Ok(files
        .into_iter()
        .map(|f| f.code)
        .collect::<Vec<_>>()
        .join("\n"))
}

/// Filter entities: skip generic templates that have been monomorphized.
/// An entity with non-empty generics is a template; if a concrete specialization
/// (same name prefix + "_" suffix, empty generics) exists, skip the template.
fn filter_entities(hir: &Hir) -> Vec<&HirEntity> {
    let concrete_names: std::collections::HashSet<&str> = hir
        .entities
        .iter()
        .filter(|e| e.generics.is_empty())
        .map(|e| e.name.as_str())
        .collect();

    hir.entities
        .iter()
        .filter(|entity| {
            if entity.generics.is_empty() {
                // Concrete entity — always emit
                true
            } else {
                // Generic template — skip if any monomorphized version exists
                let has_specialization = concrete_names
                    .iter()
                    .any(|name| name.starts_with(&entity.name) && name.len() > entity.name.len());
                !has_specialization
            }
        })
        .collect()
}

/// Maps numeric IDs back to names and types for readable emission
pub(crate) struct NameResolver {
    pub ports: HashMap<PortId, (String, HirType)>,
    pub signals: HashMap<SignalId, (String, HirType)>,
    pub variables: HashMap<VariableId, (String, HirType)>,
    pub constants: HashMap<ConstantId, (String, HirType)>,
    pub entities: HashMap<EntityId, String>,
    pub functions: HashMap<FunctionId, String>,
}

impl NameResolver {
    pub fn from_hir(hir: &Hir) -> Self {
        let mut resolver = Self {
            ports: HashMap::new(),
            signals: HashMap::new(),
            variables: HashMap::new(),
            constants: HashMap::new(),
            entities: HashMap::new(),
            functions: HashMap::new(),
        };

        for entity in &hir.entities {
            resolver.entities.insert(entity.id, entity.name.clone());
            for port in &entity.ports {
                resolver
                    .ports
                    .insert(port.id, (port.name.clone(), port.port_type.clone()));
            }
            for signal in &entity.signals {
                resolver
                    .signals
                    .insert(signal.id, (signal.name.clone(), signal.signal_type.clone()));
            }
        }

        for imp in &hir.implementations {
            for signal in &imp.signals {
                resolver
                    .signals
                    .insert(signal.id, (signal.name.clone(), signal.signal_type.clone()));
            }
            for variable in &imp.variables {
                resolver.variables.insert(
                    variable.id,
                    (variable.name.clone(), variable.var_type.clone()),
                );
            }
            for constant in &imp.constants {
                resolver.constants.insert(
                    constant.id,
                    (constant.name.clone(), constant.const_type.clone()),
                );
            }
            for func in &imp.functions {
                resolver.functions.insert(func.id, func.name.clone());
            }
        }

        for func in &hir.functions {
            resolver.functions.insert(func.id, func.name.clone());
        }

        resolver
    }

    pub fn port_name(&self, id: PortId) -> &str {
        self.ports
            .get(&id)
            .map(|(n, _)| n.as_str())
            .unwrap_or("unknown_port")
    }

    pub fn signal_name(&self, id: SignalId) -> &str {
        self.signals
            .get(&id)
            .map(|(n, _)| n.as_str())
            .unwrap_or("unknown_signal")
    }

    pub fn variable_name(&self, id: VariableId) -> &str {
        self.variables
            .get(&id)
            .map(|(n, _)| n.as_str())
            .unwrap_or("unknown_var")
    }

    pub fn constant_name(&self, id: ConstantId) -> &str {
        self.constants
            .get(&id)
            .map(|(n, _)| n.as_str())
            .unwrap_or("unknown_const")
    }

    pub fn entity_name(&self, id: EntityId) -> &str {
        self.entities
            .get(&id)
            .map(|n| n.as_str())
            .unwrap_or("unknown_entity")
    }

    pub fn port_type(&self, id: PortId) -> Option<&HirType> {
        self.ports.get(&id).map(|(_, t)| t)
    }

    pub fn signal_type(&self, id: SignalId) -> Option<&HirType> {
        self.signals.get(&id).map(|(_, t)| t)
    }

    pub(crate) fn from_empty() -> Self {
        Self {
            ports: HashMap::new(),
            signals: HashMap::new(),
            variables: HashMap::new(),
            constants: HashMap::new(),
            entities: HashMap::new(),
            functions: HashMap::new(),
        }
    }
}

/// Check if an output port is assigned inside any sequential (clocked) event block.
/// Used by SV emitter to decide `output reg` vs `output`.
pub(crate) fn is_port_sequential(port_id: PortId, imp: &HirImplementation) -> bool {
    for eb in &imp.event_blocks {
        let has_clock_edge = eb
            .triggers
            .iter()
            .any(|t| matches!(t.edge, HirEdgeType::Rising | HirEdgeType::Falling));
        if has_clock_edge {
            for stmt in &eb.statements {
                if lvalue_assigns_port(stmt, port_id) {
                    return true;
                }
            }
        }
    }
    false
}

/// Recursively check if a statement assigns to the given port.
fn lvalue_assigns_port(stmt: &HirStatement, port_id: PortId) -> bool {
    match stmt {
        HirStatement::Assignment(assign) => lvalue_contains_port(&assign.lhs, port_id),
        HirStatement::If(if_stmt) => {
            if_stmt
                .then_statements
                .iter()
                .any(|s| lvalue_assigns_port(s, port_id))
                || if_stmt
                    .else_statements
                    .as_ref()
                    .is_some_and(|stmts| stmts.iter().any(|s| lvalue_assigns_port(s, port_id)))
        }
        HirStatement::Match(match_stmt) => match_stmt.arms.iter().any(|arm| {
            arm.statements
                .iter()
                .any(|s| lvalue_assigns_port(s, port_id))
        }),
        HirStatement::Block(stmts) => stmts.iter().any(|s| lvalue_assigns_port(s, port_id)),
        HirStatement::For(for_stmt) => for_stmt
            .body
            .iter()
            .any(|s| lvalue_assigns_port(s, port_id)),
        _ => false,
    }
}

/// Check if an LValue contains a reference to the given port.
fn lvalue_contains_port(lval: &HirLValue, port_id: PortId) -> bool {
    match lval {
        HirLValue::Port(id) => *id == port_id,
        HirLValue::Index(base, _) | HirLValue::Range(base, _, _) => {
            lvalue_contains_port(base, port_id)
        }
        HirLValue::FieldAccess { base, .. } => lvalue_contains_port(base, port_id),
        _ => false,
    }
}

/// Emit comments with the given prefix and indentation
pub(crate) fn emit_comments(out: &mut String, comments: &[String], prefix: &str, indent: &str) {
    for comment in comments {
        let _ = writeln!(out, "{indent}{prefix} {comment}");
    }
}

/// Generate timing constraints TOML for vendor IP entities.
/// Returns None if no vendor IP configuration is present.
pub fn generate_constraints_toml(hir: &Hir) -> Result<Option<String>> {
    for entity in &hir.entities {
        if let Some(ref config) = entity.vendor_ip_config {
            let has_constraints = !config.clocks.is_empty()
                || !config.async_groups.is_empty()
                || !config.input_delays.is_empty()
                || !config.output_delays.is_empty();

            if !has_constraints {
                return Ok(None);
            }

            let mut out = String::new();
            writeln!(out, "# Timing constraints for {}", entity.name).unwrap();
            writeln!(out, "# Generated by Skalp - convert to XDC/SDC as needed").unwrap();
            writeln!(out).unwrap();

            for (port, freq) in &config.clocks {
                writeln!(out, "[clocks.{port}]").unwrap();
                writeln!(out, "port = \"{port}\"").unwrap();
                writeln!(out, "frequency_mhz = {freq}").unwrap();
                writeln!(out).unwrap();
            }

            for (clk1, clk2) in &config.async_groups {
                writeln!(out, "[[async_groups]]").unwrap();
                writeln!(out, "clocks = [\"{clk1}\", \"{clk2}\"]").unwrap();
                writeln!(out).unwrap();
            }

            for (port, delay, clock) in &config.input_delays {
                writeln!(out, "[[input_delays]]").unwrap();
                writeln!(out, "port = \"{port}\"").unwrap();
                writeln!(out, "delay_ns = {delay}").unwrap();
                writeln!(out, "clock = \"{clock}\"").unwrap();
                writeln!(out).unwrap();
            }

            for (port, delay, clock) in &config.output_delays {
                writeln!(out, "[[output_delays]]").unwrap();
                writeln!(out, "port = \"{port}\"").unwrap();
                writeln!(out, "delay_ns = {delay}").unwrap();
                writeln!(out, "clock = \"{clock}\"").unwrap();
                writeln!(out).unwrap();
            }

            return Ok(Some(out));
        }
    }
    Ok(None)
}

/// Generate indentation string
pub(crate) fn indent(level: usize) -> String {
    "    ".repeat(level)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_alu_transpile() {
        let source = r#"
// Arithmetic Logic Unit
entity ALU {
    in a: bit[32]
    in b: bit[32]
    in op: bit[3]
    in clk: clock
    out result: bit[32]
    out zero: bit[1]
}

impl ALU {
    signal result_comb: bit[32]

    // ALU operations
    result_comb = match op {
        0b000 => a + b,
        0b001 => a - b,
        0b010 => a & b,
        0b011 => a | b,
        _ => 0
    };

    zero = if result_comb == 0 { 1 } else { 0 };

    on(clk.rise) {
        result = result_comb
    }
}
"#;
        let hir = skalp_frontend::parse_and_build_hir(source).unwrap();

        let vhdl = generate_vhdl(&hir).unwrap();
        println!("=== VHDL ===\n{vhdl}");
        assert!(vhdl.contains("entity ALU is"));
        assert!(
            !vhdl.contains("match op"),
            "VHDL should not contain skalp match syntax"
        );
        assert!(
            vhdl.contains("when"),
            "VHDL should use when...else for match"
        );

        let sv = generate_systemverilog(&hir).unwrap();
        println!("=== SystemVerilog ===\n{sv}");
        assert!(sv.contains("module ALU"));
        assert!(
            !sv.contains("match op"),
            "SV should not contain skalp match syntax"
        );
        assert!(sv.contains("?"), "SV should use ternary for match");

        let sk = generate_skalp_source(&hir).unwrap();
        println!("=== Skalp (round-trip) ===\n{sk}");
        assert!(sk.contains("entity ALU"));
        assert!(
            sk.contains("0b000"),
            "Binary literals should preserve correct bit order"
        );
    }
}
