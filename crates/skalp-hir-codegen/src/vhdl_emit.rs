//! HIR → VHDL source code emitter

use crate::skalp_emit::emit_expression_inline as sk_expr;
use crate::{emit_comments, indent, NameResolver};
use anyhow::Result;
use skalp_frontend::hir::*;
use std::fmt::Write;

pub(crate) fn emit_file(hir: &Hir, resolver: &NameResolver) -> Result<String> {
    let mut out = String::new();

    emit_comments(&mut out, &hir.comments, "--", "");

    // IEEE library preamble
    writeln!(out, "library ieee;")?;
    writeln!(out, "use ieee.std_logic_1164.all;")?;
    writeln!(out, "use ieee.numeric_std.all;")?;
    writeln!(out)?;

    for (i, entity) in hir.entities.iter().enumerate() {
        emit_entity(&mut out, entity, resolver);

        // Find matching implementation
        if let Some(imp) = hir
            .implementations
            .iter()
            .find(|imp| imp.entity == entity.id)
        {
            writeln!(out).unwrap();
            emit_architecture(&mut out, imp, entity, resolver);
        }

        if i + 1 < hir.entities.len() {
            writeln!(out).unwrap();
        }
    }

    Ok(out)
}

fn emit_entity(out: &mut String, entity: &HirEntity, resolver: &NameResolver) {
    emit_comments(out, &entity.comments, "--", "");

    write!(out, "entity {} is", entity.name).unwrap();

    // Generics
    if !entity.generics.is_empty() {
        writeln!(out).unwrap();
        writeln!(out, "    generic (").unwrap();
        let generics: Vec<_> = entity
            .generics
            .iter()
            .filter(|g| {
                matches!(
                    g.param_type,
                    HirGenericType::Const(_) | HirGenericType::Width
                )
            })
            .collect();
        for (i, generic) in generics.iter().enumerate() {
            let ty = match &generic.param_type {
                HirGenericType::Width => "natural".to_string(),
                HirGenericType::Const(t) => emit_type(t),
                _ => "natural".to_string(),
            };
            let sep = if i + 1 < generics.len() { ";" } else { "" };
            let ind = indent(2);
            if let Some(ref default) = generic.default_value {
                writeln!(
                    out,
                    "{ind}{} : {ty} := {}{sep}",
                    generic.name,
                    emit_expr(default, resolver)
                )
                .unwrap();
            } else {
                writeln!(out, "{ind}{} : {ty}{sep}", generic.name).unwrap();
            }
        }
        writeln!(out, "    );").unwrap();
    }

    // Ports
    if !entity.ports.is_empty() {
        writeln!(out).unwrap();
        writeln!(out, "    port (").unwrap();
        for (i, port) in entity.ports.iter().enumerate() {
            emit_comments(out, &port.comments, "--", &indent(2));
            let ind = indent(2);
            let dir = match port.direction {
                HirPortDirection::Input => "in",
                HirPortDirection::Output => "out",
                HirPortDirection::Bidirectional => "inout",
                HirPortDirection::Protocol => "inout",
            };
            let ty = emit_type(&port.port_type);
            let sep = if i + 1 < entity.ports.len() { ";" } else { "" };
            writeln!(out, "{ind}{} : {dir} {ty}{sep}", port.name).unwrap();
        }
        writeln!(out, "    );").unwrap();
    }

    writeln!(out, "end entity {};", entity.name).unwrap();
}

fn emit_architecture(
    out: &mut String,
    imp: &HirImplementation,
    entity: &HirEntity,
    resolver: &NameResolver,
) {
    writeln!(out, "architecture rtl of {} is", entity.name).unwrap();

    // Signal declarations
    for signal in &imp.signals {
        let ind = indent(1);
        emit_comments(out, &signal.comments, "--", &ind);
        let ty = emit_type(&signal.signal_type);
        if let Some(ref init) = signal.initial_value {
            writeln!(
                out,
                "{ind}signal {} : {ty} := {};",
                signal.name,
                emit_expr(init, resolver)
            )
            .unwrap();
        } else {
            writeln!(out, "{ind}signal {} : {ty};", signal.name).unwrap();
        }
    }

    // Variable declarations
    for variable in &imp.variables {
        let ind = indent(1);
        emit_comments(out, &variable.comments, "--", &ind);
        let ty = emit_type(&variable.var_type);
        writeln!(out, "{ind}variable {} : {ty};", variable.name).unwrap();
    }

    // Constants
    for constant in &imp.constants {
        let ind = indent(1);
        emit_comments(out, &constant.comments, "--", &ind);
        let ty = emit_type(&constant.const_type);
        writeln!(
            out,
            "{ind}constant {} : {ty} := {};",
            constant.name,
            emit_expr(&constant.value, resolver)
        )
        .unwrap();
    }

    writeln!(out, "begin").unwrap();

    // Continuous assignments
    for assign in &imp.assignments {
        let ind = indent(1);
        emit_comments(out, &assign.comments, "--", &ind);
        let lhs = emit_lvalue(&assign.lhs, resolver);
        let rhs = emit_expr(&assign.rhs, resolver);
        writeln!(out, "{ind}{lhs} <= {rhs};").unwrap();
    }

    // Instances
    for inst in &imp.instances {
        emit_instance(out, inst, resolver, 1);
    }

    // Event blocks (processes)
    for eb in &imp.event_blocks {
        emit_process(out, eb, resolver, 1);
    }

    writeln!(out, "end architecture rtl;").unwrap();
}

fn emit_process(out: &mut String, eb: &HirEventBlock, resolver: &NameResolver, level: usize) {
    let ind = indent(level);
    emit_comments(out, &eb.comments, "--", &ind);

    // Build sensitivity list from triggers
    let sensitivity: Vec<String> = eb
        .triggers
        .iter()
        .map(|t| match &t.signal {
            HirEventSignal::Port(id) => resolver.port_name(*id).to_string(),
            HirEventSignal::Signal(id) => resolver.signal_name(*id).to_string(),
        })
        .collect();

    writeln!(out, "{ind}process({})", sensitivity.join(", ")).unwrap();
    writeln!(out, "{ind}begin").unwrap();

    // Wrap body in edge detection if needed
    let has_edge = eb
        .triggers
        .iter()
        .any(|t| matches!(t.edge, HirEdgeType::Rising | HirEdgeType::Falling));

    if has_edge {
        let trigger = &eb.triggers[0];
        let sig = match &trigger.signal {
            HirEventSignal::Port(id) => resolver.port_name(*id).to_string(),
            HirEventSignal::Signal(id) => resolver.signal_name(*id).to_string(),
        };
        let edge_fn = match trigger.edge {
            HirEdgeType::Rising => "rising_edge",
            HirEdgeType::Falling => "falling_edge",
            _ => "rising_edge",
        };
        let ind2 = indent(level + 1);
        writeln!(out, "{ind2}if {edge_fn}({sig}) then").unwrap();

        // Check for reset in additional triggers
        let reset_trigger = eb.triggers.iter().skip(1).find(|t| {
            matches!(
                t.edge,
                HirEdgeType::Active | HirEdgeType::Inactive | HirEdgeType::Rising
            )
        });

        if let Some(_reset) = reset_trigger {
            // Emit reset + body pattern
            for stmt in &eb.statements {
                emit_vhdl_statement(out, stmt, resolver, level + 2);
            }
        } else {
            for stmt in &eb.statements {
                emit_vhdl_statement(out, stmt, resolver, level + 2);
            }
        }
        writeln!(out, "{ind2}end if;").unwrap();
    } else {
        for stmt in &eb.statements {
            emit_vhdl_statement(out, stmt, resolver, level + 1);
        }
    }

    writeln!(out, "{ind}end process;").unwrap();
}

fn emit_instance(out: &mut String, inst: &HirInstance, resolver: &NameResolver, level: usize) {
    let ind = indent(level);
    emit_comments(out, &inst.comments, "--", &ind);
    let entity_name = resolver.entity_name(inst.entity);
    write!(out, "{ind}{}: entity work.{entity_name}", inst.name).unwrap();

    // Generic map
    if !inst.generic_args.is_empty() || !inst.named_generic_args.is_empty() {
        writeln!(out).unwrap();
        writeln!(out, "{ind}    generic map (").unwrap();
        let mut args: Vec<String> = Vec::new();
        for arg in &inst.generic_args {
            args.push(emit_expr(arg, resolver));
        }
        for (name, arg) in &inst.named_generic_args {
            args.push(format!("{name} => {}", emit_expr(arg, resolver)));
        }
        for (i, arg) in args.iter().enumerate() {
            let sep = if i + 1 < args.len() { "," } else { "" };
            writeln!(out, "{ind}        {arg}{sep}").unwrap();
        }
        writeln!(out, "{ind}    )").unwrap();
    }

    // Port map
    if !inst.connections.is_empty() {
        writeln!(out).unwrap();
        writeln!(out, "{ind}    port map (").unwrap();
        for (i, conn) in inst.connections.iter().enumerate() {
            let sep = if i + 1 < inst.connections.len() {
                ","
            } else {
                ""
            };
            writeln!(
                out,
                "{ind}        {} => {}{sep}",
                conn.port,
                emit_expr(&conn.expr, resolver)
            )
            .unwrap();
        }
        writeln!(out, "{ind}    );").unwrap();
    } else {
        writeln!(out, ";").unwrap();
    }
}

fn emit_vhdl_statement(
    out: &mut String,
    stmt: &HirStatement,
    resolver: &NameResolver,
    level: usize,
) {
    let ind = indent(level);
    match stmt {
        HirStatement::Assignment(assign) => {
            emit_comments(out, &assign.comments, "--", &ind);
            let lhs = emit_lvalue(&assign.lhs, resolver);
            let rhs = emit_expr(&assign.rhs, resolver);
            match assign.assignment_type {
                HirAssignmentType::Combinational => {
                    writeln!(out, "{ind}{lhs} <= {rhs};").unwrap();
                }
                HirAssignmentType::NonBlocking => {
                    writeln!(out, "{ind}{lhs} <= {rhs};").unwrap();
                }
                HirAssignmentType::Blocking => {
                    writeln!(out, "{ind}{lhs} := {rhs};").unwrap();
                }
            }
        }
        HirStatement::If(if_stmt) => {
            let cond = emit_expr(&if_stmt.condition, resolver);
            writeln!(out, "{ind}if {cond} then").unwrap();
            for s in &if_stmt.then_statements {
                emit_vhdl_statement(out, s, resolver, level + 1);
            }
            if let Some(ref else_stmts) = if_stmt.else_statements {
                writeln!(out, "{ind}else").unwrap();
                for s in else_stmts {
                    emit_vhdl_statement(out, s, resolver, level + 1);
                }
            }
            writeln!(out, "{ind}end if;").unwrap();
        }
        HirStatement::Match(match_stmt) => {
            let expr = emit_expr(&match_stmt.expr, resolver);
            writeln!(out, "{ind}case {expr} is").unwrap();
            for arm in &match_stmt.arms {
                let pat = emit_vhdl_pattern(&arm.pattern);
                writeln!(out, "{ind}    when {pat} =>").unwrap();
                for s in &arm.statements {
                    emit_vhdl_statement(out, s, resolver, level + 2);
                }
            }
            writeln!(out, "{ind}end case;").unwrap();
        }
        HirStatement::Let(let_stmt) => {
            let ty = emit_type(&let_stmt.var_type);
            let val = emit_expr(&let_stmt.value, resolver);
            writeln!(out, "{ind}variable {} : {ty} := {val};", let_stmt.name).unwrap();
        }
        HirStatement::Return(expr) => {
            if let Some(ref e) = expr {
                writeln!(out, "{ind}return {};", emit_expr(e, resolver)).unwrap();
            } else {
                writeln!(out, "{ind}return;").unwrap();
            }
        }
        HirStatement::Expression(expr) => {
            writeln!(out, "{ind}{};", emit_expr(expr, resolver)).unwrap();
        }
        HirStatement::For(for_stmt) => {
            let start = emit_expr(&for_stmt.range.start, resolver);
            let end = emit_expr(&for_stmt.range.end, resolver);
            writeln!(
                out,
                "{ind}for {} in {start} to {end} loop",
                for_stmt.iterator
            )
            .unwrap();
            for s in &for_stmt.body {
                emit_vhdl_statement(out, s, resolver, level + 1);
            }
            writeln!(out, "{ind}end loop;").unwrap();
        }
        HirStatement::Block(stmts) => {
            for s in stmts {
                emit_vhdl_statement(out, s, resolver, level);
            }
        }
        _ => {
            writeln!(out, "{ind}-- unsupported statement").unwrap();
        }
    }
}

fn emit_vhdl_pattern(pat: &HirPattern) -> String {
    match pat {
        HirPattern::Literal(lit) => emit_vhdl_literal(lit),
        HirPattern::Variable(_) => "others".to_string(),
        HirPattern::Wildcard => "others".to_string(),
        HirPattern::Tuple(_) => "others".to_string(),
        HirPattern::Path(_, variant) => variant.clone(),
    }
}

fn emit_vhdl_literal(lit: &HirLiteral) -> String {
    match lit {
        HirLiteral::Integer(n) => n.to_string(),
        HirLiteral::Boolean(b) => {
            if *b {
                "true".to_string()
            } else {
                "false".to_string()
            }
        }
        HirLiteral::Float(f) => format!("{f}"),
        HirLiteral::String(s) => format!("\"{s}\""),
        HirLiteral::BitVector(bits) => {
            let mut s = String::from("\"");
            for b in bits {
                s.push(if *b { '1' } else { '0' });
            }
            s.push('"');
            s
        }
    }
}

fn emit_expr(expr: &HirExpression, resolver: &NameResolver) -> String {
    match expr {
        HirExpression::Literal(lit) => emit_vhdl_literal(lit),
        HirExpression::Signal(id) => resolver.signal_name(*id).to_string(),
        HirExpression::Port(id) => resolver.port_name(*id).to_string(),
        HirExpression::Variable(id) => resolver.variable_name(*id).to_string(),
        HirExpression::Constant(id) => resolver.constant_name(*id).to_string(),
        HirExpression::GenericParam(name) => name.clone(),
        HirExpression::Binary(bin) => {
            let left = emit_expr(&bin.left, resolver);
            let right = emit_expr(&bin.right, resolver);
            let op = emit_vhdl_binary_op(&bin.op);
            format!("({left} {op} {right})")
        }
        HirExpression::Unary(un) => {
            let operand = emit_expr(&un.operand, resolver);
            match un.op {
                HirUnaryOp::Not | HirUnaryOp::BitwiseNot => format!("not {operand}"),
                HirUnaryOp::Negate => format!("-{operand}"),
                HirUnaryOp::AndReduce => format!("and_reduce({operand})"),
                HirUnaryOp::OrReduce => format!("or_reduce({operand})"),
                HirUnaryOp::XorReduce => format!("xor_reduce({operand})"),
            }
        }
        HirExpression::Call(call) => {
            let args: Vec<_> = call.args.iter().map(|a| emit_expr(a, resolver)).collect();
            format!("{}({})", call.function, args.join(", "))
        }
        HirExpression::Index(base, idx) => {
            format!(
                "{}(to_integer({}))",
                emit_expr(base, resolver),
                emit_expr(idx, resolver)
            )
        }
        HirExpression::Range(base, lo, hi) => {
            format!(
                "{}({} downto {})",
                emit_expr(base, resolver),
                emit_expr(hi, resolver),
                emit_expr(lo, resolver)
            )
        }
        HirExpression::FieldAccess { base, field } => {
            format!("{}.{field}", emit_expr(base, resolver))
        }
        HirExpression::EnumVariant { variant, .. } => variant.clone(),
        HirExpression::Concat(exprs) => {
            let parts: Vec<_> = exprs.iter().map(|e| emit_expr(e, resolver)).collect();
            parts.join(" & ")
        }
        HirExpression::Ternary {
            condition,
            true_expr,
            false_expr,
        } => {
            // VHDL doesn't have ternary; use when/else for signal assignments
            format!(
                "{} when {} else {}",
                emit_expr(true_expr, resolver),
                emit_expr(condition, resolver),
                emit_expr(false_expr, resolver)
            )
        }
        HirExpression::Cast(cast) => emit_vhdl_cast(&cast.expr, &cast.target_type, resolver),
        _ => sk_expr(expr, resolver),
    }
}

fn emit_vhdl_cast(expr: &HirExpression, target: &HirType, resolver: &NameResolver) -> String {
    let inner = emit_expr(expr, resolver);
    match target {
        HirType::Nat(_) => format!("unsigned({inner})"),
        HirType::Int(_) => format!("signed({inner})"),
        HirType::Bit(_) => format!("std_logic_vector({inner})"),
        _ => inner,
    }
}

fn emit_vhdl_binary_op(op: &HirBinaryOp) -> &'static str {
    match op {
        HirBinaryOp::Add => "+",
        HirBinaryOp::WidenAdd => "+",
        HirBinaryOp::Sub => "-",
        HirBinaryOp::Mul => "*",
        HirBinaryOp::Div => "/",
        HirBinaryOp::Mod => "mod",
        HirBinaryOp::And => "and",
        HirBinaryOp::Or => "or",
        HirBinaryOp::Xor => "xor",
        HirBinaryOp::Equal => "=",
        HirBinaryOp::NotEqual => "/=",
        HirBinaryOp::Less => "<",
        HirBinaryOp::LessEqual => "<=",
        HirBinaryOp::Greater => ">",
        HirBinaryOp::GreaterEqual => ">=",
        HirBinaryOp::LogicalAnd => "and",
        HirBinaryOp::LogicalOr => "or",
        HirBinaryOp::LeftShift => "sll",
        HirBinaryOp::RightShift => "srl",
    }
}

fn emit_type(ty: &HirType) -> String {
    match ty {
        HirType::Bit(1) => "std_logic".to_string(),
        HirType::Bit(n) => format!("std_logic_vector({} downto 0)", n - 1),
        HirType::Bool => "boolean".to_string(),
        HirType::String => "string".to_string(),
        HirType::Logic(1) => "std_logic".to_string(),
        HirType::Logic(n) => format!("std_logic_vector({} downto 0)", n - 1),
        HirType::Int(n) => format!("signed({} downto 0)", n - 1),
        HirType::Nat(n) => format!("unsigned({} downto 0)", n - 1),
        HirType::Clock(_) => "std_logic".to_string(),
        HirType::Reset { .. } => "std_logic".to_string(),
        HirType::Array(inner, n) => {
            format!("array (0 to {}) of {}", n - 1, emit_type(inner))
        }
        HirType::Custom(name) => name.clone(),
        HirType::Struct(s) => format!("{}_t", s.name.to_lowercase()),
        HirType::Enum(e) => format!("{}_t", e.name.to_lowercase()),
        HirType::Union(u) => format!("{}_t", u.name.to_lowercase()),
        HirType::BitParam(p) => format!("std_logic_vector({p} - 1 downto 0)"),
        HirType::NatParam(p) => format!("unsigned({p} - 1 downto 0)"),
        HirType::IntParam(p) => format!("signed({p} - 1 downto 0)"),
        HirType::LogicParam(p) => format!("std_logic_vector({p} - 1 downto 0)"),
        HirType::BitExpr(e) => {
            let w = sk_expr(e, &NameResolver::from_empty());
            format!("std_logic_vector({w} - 1 downto 0)")
        }
        HirType::NatExpr(e) => {
            let w = sk_expr(e, &NameResolver::from_empty());
            format!("unsigned({w} - 1 downto 0)")
        }
        HirType::IntExpr(e) => {
            let w = sk_expr(e, &NameResolver::from_empty());
            format!("signed({w} - 1 downto 0)")
        }
        _ => format!("-- unsupported type: {:?}", ty),
    }
}

fn emit_lvalue(lval: &HirLValue, resolver: &NameResolver) -> String {
    match lval {
        HirLValue::Signal(id) => resolver.signal_name(*id).to_string(),
        HirLValue::Variable(id) => resolver.variable_name(*id).to_string(),
        HirLValue::Port(id) => resolver.port_name(*id).to_string(),
        HirLValue::Index(base, idx) => {
            format!(
                "{}(to_integer({}))",
                emit_lvalue(base, resolver),
                emit_expr(idx, resolver)
            )
        }
        HirLValue::Range(base, lo, hi) => {
            format!(
                "{}({} downto {})",
                emit_lvalue(base, resolver),
                emit_expr(hi, resolver),
                emit_expr(lo, resolver)
            )
        }
        HirLValue::FieldAccess { base, field } => {
            format!("{}.{field}", emit_lvalue(base, resolver))
        }
    }
}
