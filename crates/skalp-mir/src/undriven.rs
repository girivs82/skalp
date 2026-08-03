//! Undriven-output detection
//!
//! A silently undriven output port is almost always a lowering bug (a dropped
//! statement) or a design error — the emitted SystemVerilog would leave the
//! port floating while simulation happily reads zeros. This pass walks every
//! module and reports output ports that are driven by nothing: no continuous
//! assignment, no process assignment, no generate-block assignment, and no
//! child-instance output connection.
//!
//! The compiler fails the build for undriven outputs in modules reachable from
//! the main design (see `compiler.rs` — same scoping as MIR conversion errors,
//! so latent problems in unused stdlib entities don't block unrelated builds).

use crate::mir::{
    Block, Expression, ExpressionKind, GenerateBlockKind, GenerateBody, LValue, LoopStatement, Mir,
    PortDirection, PortId, Statement,
};
use std::collections::HashSet;

/// An undriven output: (module name, port name)
pub type UndrivenOutput = (String, String);

/// Find output ports with no driver in any module of `mir`.
///
/// Modules that are external by construction (vendor IP, pre-compiled IP) are
/// skipped — their MIR is an interface-only placeholder.
pub fn find_undriven_outputs(mir: &Mir) -> Vec<UndrivenOutput> {
    let mut result = Vec::new();

    for module in &mir.modules {
        if module.vendor_ip_config.is_some() || module.compiled_ip_config.is_some() {
            continue;
        }
        // Skip unspecialized generic templates: they keep unresolved parameters
        // and their bodies are not fully lowered — the monomorphized
        // specializations are the modules that actually get emitted.
        if !module.parameters.is_empty() {
            continue;
        }

        let mut driven: HashSet<PortId> = HashSet::new();

        // Continuous assignments
        for assign in &module.assignments {
            collect_lvalue_ports(&assign.lhs, &mut driven);
        }

        // Process bodies
        for process in &module.processes {
            collect_block_ports(&process.body, &mut driven);
        }

        // Generate blocks (preserved generate constructs carry their own
        // assignments and processes)
        for gen in &module.generate_blocks {
            match &gen.kind {
                GenerateBlockKind::For(f) => collect_generate_body(&f.body, &mut driven),
                GenerateBlockKind::If(i) => {
                    collect_generate_body(&i.then_body, &mut driven);
                    if let Some(else_body) = &i.else_body {
                        collect_generate_body(else_body, &mut driven);
                    }
                }
                GenerateBlockKind::Case(c) => {
                    for arm in &c.arms {
                        collect_generate_body(&arm.body, &mut driven);
                    }
                    if let Some(default) = &c.default {
                        collect_generate_body(default, &mut driven);
                    }
                }
            }
        }

        // Child-instance output connections: `.child_out(parent_port)` drives
        // the parent port.
        for instance in &module.instances {
            let child = mir.modules.iter().find(|m| m.id == instance.module);
            for (port_name, expr) in &instance.connections {
                let drives_parent = match child {
                    Some(child) => child
                        .ports
                        .iter()
                        .find(|p| p.name == *port_name)
                        .map(|p| {
                            matches!(p.direction, PortDirection::Output | PortDirection::InOut)
                        })
                        // Unknown port name on a known child: be conservative,
                        // treat as potentially driving.
                        .unwrap_or(true),
                    // Unknown child module: be conservative.
                    None => true,
                };
                if drives_parent {
                    collect_expression_ports(expr, &mut driven);
                }
            }
        }

        for port in &module.ports {
            if matches!(port.direction, PortDirection::Output) && !driven.contains(&port.id) {
                result.push((module.name.clone(), port.name.clone()));
            }
        }
    }

    result
}

fn collect_generate_body(body: &GenerateBody, driven: &mut HashSet<PortId>) {
    for assign in &body.assignments {
        collect_lvalue_ports(&assign.lhs, driven);
    }
    for process in &body.processes {
        collect_block_ports(&process.body, driven);
    }
}

fn collect_block_ports(block: &Block, driven: &mut HashSet<PortId>) {
    for stmt in &block.statements {
        collect_statement_ports(stmt, driven);
    }
}

fn collect_statement_ports(stmt: &Statement, driven: &mut HashSet<PortId>) {
    match stmt {
        Statement::Assignment(assign) => collect_lvalue_ports(&assign.lhs, driven),
        Statement::If(if_stmt) => {
            collect_block_ports(&if_stmt.then_block, driven);
            if let Some(else_block) = &if_stmt.else_block {
                collect_block_ports(else_block, driven);
            }
        }
        Statement::Case(case_stmt) => {
            for item in &case_stmt.items {
                collect_block_ports(&item.block, driven);
            }
            if let Some(default) = &case_stmt.default {
                collect_block_ports(default, driven);
            }
        }
        Statement::Block(block) => collect_block_ports(block, driven),
        Statement::Loop(loop_stmt) => match loop_stmt {
            LoopStatement::For {
                init, update, body, ..
            } => {
                collect_lvalue_ports(&init.lhs, driven);
                collect_lvalue_ports(&update.lhs, driven);
                collect_block_ports(body, driven);
            }
            LoopStatement::While { body, .. } => collect_block_ports(body, driven),
        },
        Statement::ResolvedConditional(rc) => collect_lvalue_ports(&rc.target, driven),
        Statement::Assert(_) | Statement::Assume(_) | Statement::Cover(_) => {}
    }
}

fn collect_lvalue_ports(lvalue: &LValue, driven: &mut HashSet<PortId>) {
    match lvalue {
        LValue::Port(id) => {
            driven.insert(*id);
        }
        LValue::Signal(_) | LValue::Variable(_) => {}
        LValue::BitSelect { base, .. } | LValue::RangeSelect { base, .. } => {
            collect_lvalue_ports(base, driven);
        }
        LValue::Concat(parts) => {
            for part in parts {
                collect_lvalue_ports(part, driven);
            }
        }
    }
}

fn collect_expression_ports(expr: &Expression, driven: &mut HashSet<PortId>) {
    if let ExpressionKind::Ref(lvalue) = &expr.kind {
        collect_lvalue_ports(lvalue, driven);
    }
}

/// Check `mir` for undriven outputs, restricted to the given reachable module
/// names (empty set means check everything). Returns formatted error strings.
pub fn check_undriven_outputs(mir: &Mir, reachable: &HashSet<&str>) -> Vec<String> {
    find_undriven_outputs(mir)
        .into_iter()
        .filter(|(module, _)| reachable.is_empty() || reachable.contains(module.as_str()))
        .map(|(module, port)| {
            format!(
                "output port `{}` of `{}` is never driven — no assignment, process, or instance connection writes it",
                port, module
            )
        })
        .collect()
}
