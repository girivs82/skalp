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

/// Generate skalp source code from HIR
pub fn generate_skalp_source(hir: &Hir) -> Result<String> {
    let resolver = NameResolver::from_hir(hir);
    skalp_emit::emit_file(hir, &resolver)
}

/// Generate VHDL source code from HIR
pub fn generate_vhdl(hir: &Hir) -> Result<String> {
    let resolver = NameResolver::from_hir(hir);
    vhdl_emit::emit_file(hir, &resolver)
}

/// Generate SystemVerilog source code from HIR
pub fn generate_systemverilog(hir: &Hir) -> Result<String> {
    let resolver = NameResolver::from_hir(hir);
    sv_emit::emit_file(hir, &resolver)
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

/// Emit comments with the given prefix and indentation
pub(crate) fn emit_comments(out: &mut String, comments: &[String], prefix: &str, indent: &str) {
    for comment in comments {
        let _ = writeln!(out, "{indent}{prefix} {comment}");
    }
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
