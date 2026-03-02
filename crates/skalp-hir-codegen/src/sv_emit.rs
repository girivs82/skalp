//! HIR → SystemVerilog source code emitter

use crate::skalp_emit::emit_expression_inline as sk_expr;
use crate::{emit_comments, indent, NameResolver};
use anyhow::Result;
use skalp_frontend::hir::*;
use std::fmt::Write;

pub(crate) fn emit_per_entity(
    entities: &[&HirEntity],
    hir: &Hir,
    resolver: &NameResolver,
) -> Result<Vec<crate::GeneratedFile>> {
    entities
        .iter()
        .map(|entity| {
            let mut out = String::new();
            emit_comments(&mut out, &entity.comments, "//", "");
            let imp = hir
                .implementations
                .iter()
                .find(|imp| imp.entity == entity.id);
            emit_module(&mut out, entity, imp, resolver);
            Ok(crate::GeneratedFile {
                name: entity.name.clone(),
                code: out,
            })
        })
        .collect()
}

fn emit_module(
    out: &mut String,
    entity: &HirEntity,
    imp: Option<&HirImplementation>,
    resolver: &NameResolver,
) {
    emit_comments(out, &entity.comments, "//", "");

    write!(out, "module {}", entity.name).unwrap();

    // Parameters
    if !entity.generics.is_empty() {
        let params: Vec<_> = entity
            .generics
            .iter()
            .filter(|g| {
                matches!(
                    g.param_type,
                    HirGenericType::Const(_) | HirGenericType::Width
                )
            })
            .collect();
        if !params.is_empty() {
            writeln!(out, " #(").unwrap();
            for (i, generic) in params.iter().enumerate() {
                let ind = indent(1);
                let ty = match &generic.param_type {
                    HirGenericType::Const(t) => emit_sv_param_type(t),
                    _ => "int".to_string(),
                };
                let sep = if i + 1 < params.len() { "," } else { "" };
                if let Some(ref default) = generic.default_value {
                    writeln!(
                        out,
                        "{ind}parameter {ty} {} = {}{sep}",
                        generic.name,
                        emit_expr(default, resolver)
                    )
                    .unwrap();
                } else {
                    writeln!(out, "{ind}parameter {ty} {}{sep}", generic.name).unwrap();
                }
            }
            out.push(')');
        }
    }

    // Ports
    if !entity.ports.is_empty() {
        writeln!(out, " (").unwrap();
        for (i, port) in entity.ports.iter().enumerate() {
            emit_comments(out, &port.comments, "//", &indent(1));
            let ind = indent(1);
            let dir = match port.direction {
                HirPortDirection::Input => "input",
                HirPortDirection::Output => "output",
                HirPortDirection::Bidirectional => "inout",
                HirPortDirection::Protocol => "inout",
            };
            let ty = emit_type(&port.port_type);
            let sep = if i + 1 < entity.ports.len() { "," } else { "" };
            writeln!(out, "{ind}{dir} {ty} {}{sep}", port.name).unwrap();
        }
        writeln!(out, ");").unwrap();
    } else {
        writeln!(out, ";").unwrap();
    }

    writeln!(out).unwrap();

    // Module body
    if let Some(imp) = imp {
        // Signal declarations
        for signal in &imp.signals {
            let ind = indent(1);
            emit_comments(out, &signal.comments, "//", &ind);
            let ty = emit_type(&signal.signal_type);
            if let Some(ref init) = signal.initial_value {
                writeln!(
                    out,
                    "{ind}{ty} {} = {};",
                    signal.name,
                    emit_expr(init, resolver)
                )
                .unwrap();
            } else {
                writeln!(out, "{ind}{ty} {};", signal.name).unwrap();
            }
        }

        // Constants
        for constant in &imp.constants {
            let ind = indent(1);
            emit_comments(out, &constant.comments, "//", &ind);
            let ty = emit_type(&constant.const_type);
            writeln!(
                out,
                "{ind}localparam {ty} {} = {};",
                constant.name,
                emit_expr(&constant.value, resolver)
            )
            .unwrap();
        }

        if !imp.signals.is_empty() || !imp.constants.is_empty() {
            writeln!(out).unwrap();
        }

        // Continuous assignments
        for assign in &imp.assignments {
            let ind = indent(1);
            emit_comments(out, &assign.comments, "//", &ind);
            let lhs = emit_lvalue(&assign.lhs, resolver);
            let rhs = emit_expr(&assign.rhs, resolver);
            writeln!(out, "{ind}assign {lhs} = {rhs};").unwrap();
        }

        // Instances
        for inst in &imp.instances {
            emit_sv_instance(out, inst, resolver, 1);
        }

        // Event blocks (always blocks)
        for eb in &imp.event_blocks {
            emit_always_block(out, eb, resolver, 1);
        }
    }

    writeln!(out, "endmodule").unwrap();
}

fn emit_always_block(out: &mut String, eb: &HirEventBlock, resolver: &NameResolver, level: usize) {
    let ind = indent(level);
    emit_comments(out, &eb.comments, "//", &ind);

    // Determine always type from triggers
    let has_clock_edge = eb
        .triggers
        .iter()
        .any(|t| matches!(t.edge, HirEdgeType::Rising | HirEdgeType::Falling));

    if has_clock_edge {
        // Build sensitivity list
        let sens: Vec<String> = eb
            .triggers
            .iter()
            .map(|t| {
                let sig = match &t.signal {
                    HirEventSignal::Port(id) => resolver.port_name(*id).to_string(),
                    HirEventSignal::Signal(id) => resolver.signal_name(*id).to_string(),
                };
                let edge = match t.edge {
                    HirEdgeType::Rising | HirEdgeType::Active => "posedge",
                    HirEdgeType::Falling | HirEdgeType::Inactive => "negedge",
                    _ => "posedge",
                };
                format!("{edge} {sig}")
            })
            .collect();

        writeln!(out, "{ind}always_ff @({}) begin", sens.join(" or ")).unwrap();
    } else {
        writeln!(out, "{ind}always_comb begin").unwrap();
    }

    for stmt in &eb.statements {
        emit_sv_statement(out, stmt, resolver, level + 1, has_clock_edge);
    }

    writeln!(out, "{ind}end").unwrap();
}

fn emit_sv_instance(out: &mut String, inst: &HirInstance, resolver: &NameResolver, level: usize) {
    let ind = indent(level);
    emit_comments(out, &inst.comments, "//", &ind);
    let entity_name = resolver.entity_name(inst.entity);

    write!(out, "{ind}{entity_name}").unwrap();

    // Parameter overrides
    if !inst.generic_args.is_empty() || !inst.named_generic_args.is_empty() {
        out.push_str(" #(\n");
        let mut args: Vec<String> = Vec::new();
        for (name, arg) in &inst.named_generic_args {
            args.push(format!(".{name}({})", emit_expr(arg, resolver)));
        }
        // Positional args (less common in SV)
        for arg in &inst.generic_args {
            args.push(emit_expr(arg, resolver));
        }
        for (i, arg) in args.iter().enumerate() {
            let sep = if i + 1 < args.len() { "," } else { "" };
            writeln!(out, "{ind}    {arg}{sep}").unwrap();
        }
        write!(out, "{ind})").unwrap();
    }

    write!(out, " {}", inst.name).unwrap();

    // Port connections
    if !inst.connections.is_empty() {
        writeln!(out, " (").unwrap();
        for (i, conn) in inst.connections.iter().enumerate() {
            let sep = if i + 1 < inst.connections.len() {
                ","
            } else {
                ""
            };
            writeln!(
                out,
                "{ind}    .{}({}){sep}",
                conn.port,
                emit_expr(&conn.expr, resolver)
            )
            .unwrap();
        }
        writeln!(out, "{ind});").unwrap();
    } else {
        writeln!(out, "();").unwrap();
    }
}

fn emit_sv_statement(
    out: &mut String,
    stmt: &HirStatement,
    resolver: &NameResolver,
    level: usize,
    is_sequential: bool,
) {
    let ind = indent(level);
    match stmt {
        HirStatement::Assignment(assign) => {
            emit_comments(out, &assign.comments, "//", &ind);
            let lhs = emit_lvalue(&assign.lhs, resolver);
            let rhs = emit_expr(&assign.rhs, resolver);
            let op = if is_sequential { "<=" } else { "=" };
            writeln!(out, "{ind}{lhs} {op} {rhs};").unwrap();
        }
        HirStatement::If(if_stmt) => {
            let cond = emit_expr(&if_stmt.condition, resolver);
            writeln!(out, "{ind}if ({cond}) begin").unwrap();
            for s in &if_stmt.then_statements {
                emit_sv_statement(out, s, resolver, level + 1, is_sequential);
            }
            if let Some(ref else_stmts) = if_stmt.else_statements {
                writeln!(out, "{ind}end else begin").unwrap();
                for s in else_stmts {
                    emit_sv_statement(out, s, resolver, level + 1, is_sequential);
                }
            }
            writeln!(out, "{ind}end").unwrap();
        }
        HirStatement::Match(match_stmt) => {
            let expr = emit_expr(&match_stmt.expr, resolver);
            writeln!(out, "{ind}case ({expr})").unwrap();
            for arm in &match_stmt.arms {
                let pat = emit_sv_pattern(&arm.pattern);
                writeln!(out, "{ind}    {pat}: begin").unwrap();
                for s in &arm.statements {
                    emit_sv_statement(out, s, resolver, level + 2, is_sequential);
                }
                writeln!(out, "{ind}    end").unwrap();
            }
            writeln!(out, "{ind}endcase").unwrap();
        }
        HirStatement::Let(let_stmt) => {
            let ty = emit_type(&let_stmt.var_type);
            let val = emit_expr(&let_stmt.value, resolver);
            writeln!(out, "{ind}{ty} {} = {val};", let_stmt.name).unwrap();
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
            let op = if for_stmt.range.inclusive { "<=" } else { "<" };
            writeln!(
                out,
                "{ind}for (int {} = {start}; {} {op} {end}; {}++) begin",
                for_stmt.iterator, for_stmt.iterator, for_stmt.iterator
            )
            .unwrap();
            for s in &for_stmt.body {
                emit_sv_statement(out, s, resolver, level + 1, is_sequential);
            }
            writeln!(out, "{ind}end").unwrap();
        }
        HirStatement::Block(stmts) => {
            writeln!(out, "{ind}begin").unwrap();
            for s in stmts {
                emit_sv_statement(out, s, resolver, level + 1, is_sequential);
            }
            writeln!(out, "{ind}end").unwrap();
        }
        HirStatement::GenerateFor(gen) => {
            let start = emit_expr(&gen.range.start, resolver);
            let end = emit_expr(&gen.range.end, resolver);
            let op = if gen.range.inclusive { "<=" } else { "<" };
            writeln!(
                out,
                "{ind}for (genvar {} = {start}; {} {op} {end}; {}++) begin",
                gen.iterator, gen.iterator, gen.iterator
            )
            .unwrap();
            emit_sv_generate_body(out, &gen.body, resolver, level + 1);
            writeln!(out, "{ind}end").unwrap();
        }
        HirStatement::GenerateIf(gen) => {
            let cond = emit_expr(&gen.condition, resolver);
            writeln!(out, "{ind}if ({cond}) begin").unwrap();
            emit_sv_generate_body(out, &gen.then_body, resolver, level + 1);
            if let Some(ref else_body) = gen.else_body {
                writeln!(out, "{ind}end else begin").unwrap();
                emit_sv_generate_body(out, else_body, resolver, level + 1);
            }
            writeln!(out, "{ind}end").unwrap();
        }
        _ => {
            writeln!(out, "{ind}// unsupported statement").unwrap();
        }
    }
}

fn emit_sv_generate_body(
    out: &mut String,
    body: &HirGenerateBody,
    resolver: &NameResolver,
    level: usize,
) {
    for signal in &body.signals {
        let ind = indent(level);
        emit_comments(out, &signal.comments, "//", &ind);
        let ty = emit_type(&signal.signal_type);
        writeln!(out, "{ind}{ty} {};", signal.name).unwrap();
    }
    for inst in &body.instances {
        emit_sv_instance(out, inst, resolver, level);
    }
    for eb in &body.event_blocks {
        emit_always_block(out, eb, resolver, level);
    }
    for assign in &body.assignments {
        let ind = indent(level);
        emit_comments(out, &assign.comments, "//", &ind);
        let lhs = emit_lvalue(&assign.lhs, resolver);
        let rhs = emit_expr(&assign.rhs, resolver);
        writeln!(out, "{ind}assign {lhs} = {rhs};").unwrap();
    }
    for stmt in &body.generate_stmts {
        emit_sv_statement(out, stmt, resolver, level, false);
    }
}

fn emit_sv_pattern(pat: &HirPattern) -> String {
    match pat {
        HirPattern::Literal(lit) => emit_sv_literal(lit),
        HirPattern::Variable(_) => "default".to_string(),
        HirPattern::Wildcard => "default".to_string(),
        HirPattern::Tuple(_) => "default".to_string(),
        HirPattern::Path(_, variant) => variant.clone(),
    }
}

fn emit_sv_literal(lit: &HirLiteral) -> String {
    match lit {
        HirLiteral::Integer(n) => n.to_string(),
        HirLiteral::Boolean(b) => {
            if *b {
                "1'b1".to_string()
            } else {
                "1'b0".to_string()
            }
        }
        HirLiteral::Float(f) => format!("{f}"),
        HirLiteral::String(s) => format!("\"{s}\""),
        HirLiteral::BitVector(bits) => {
            let width = bits.len();
            let mut s = format!("{width}'b");
            for b in bits.iter().rev() {
                s.push(if *b { '1' } else { '0' });
            }
            s
        }
    }
}

fn emit_expr(expr: &HirExpression, resolver: &NameResolver) -> String {
    match expr {
        HirExpression::Literal(lit) => emit_sv_literal(lit),
        HirExpression::Signal(id) => resolver.signal_name(*id).to_string(),
        HirExpression::Port(id) => resolver.port_name(*id).to_string(),
        HirExpression::Variable(id) => resolver.variable_name(*id).to_string(),
        HirExpression::Constant(id) => resolver.constant_name(*id).to_string(),
        HirExpression::GenericParam(name) => name.clone(),
        HirExpression::Binary(bin) => {
            let left = emit_expr(&bin.left, resolver);
            let right = emit_expr(&bin.right, resolver);
            let op = emit_sv_binary_op(&bin.op);
            format!("({left} {op} {right})")
        }
        HirExpression::Unary(un) => {
            let operand = emit_expr(&un.operand, resolver);
            let op = match un.op {
                HirUnaryOp::Not => "!",
                HirUnaryOp::Negate => "-",
                HirUnaryOp::BitwiseNot => "~",
                HirUnaryOp::AndReduce => "&",
                HirUnaryOp::OrReduce => "|",
                HirUnaryOp::XorReduce => "^",
            };
            format!("{op}{operand}")
        }
        HirExpression::Call(call) => {
            let args: Vec<_> = call.args.iter().map(|a| emit_expr(a, resolver)).collect();
            format!("{}({})", call.function, args.join(", "))
        }
        HirExpression::Index(base, idx) => {
            format!(
                "{}[{}]",
                emit_expr(base, resolver),
                emit_expr(idx, resolver)
            )
        }
        HirExpression::Range(base, hi, lo) => {
            format!(
                "{}[{}:{}]",
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
            format!("{{{}}}", parts.join(", "))
        }
        HirExpression::Ternary {
            condition,
            true_expr,
            false_expr,
        } => {
            format!(
                "({} ? {} : {})",
                emit_expr(condition, resolver),
                emit_expr(true_expr, resolver),
                emit_expr(false_expr, resolver)
            )
        }
        HirExpression::Cast(cast) => emit_sv_cast(&cast.expr, &cast.target_type, resolver),
        HirExpression::ArrayRepeat { value, count } => {
            format!(
                "'{{{} {{{}}}}}",
                emit_expr(count, resolver),
                emit_expr(value, resolver)
            )
        }
        HirExpression::If(if_expr) => {
            format!(
                "({} ? {} : {})",
                emit_expr(&if_expr.condition, resolver),
                emit_expr(&if_expr.then_expr, resolver),
                emit_expr(&if_expr.else_expr, resolver)
            )
        }
        HirExpression::Match(match_expr) => {
            // Emit as nested ternary: (sel == pat1) ? val1 : (sel == pat2) ? val2 : default
            let sel = emit_expr(&match_expr.expr, resolver);
            let mut default = String::from("'0");
            let mut arms_without_wildcard = Vec::new();
            for arm in &match_expr.arms {
                match &arm.pattern {
                    HirPattern::Wildcard => {
                        default = emit_expr(&arm.expr, resolver);
                    }
                    _ => {
                        arms_without_wildcard.push(arm);
                    }
                }
            }
            let mut out = default;
            for arm in arms_without_wildcard.iter().rev() {
                let pat = emit_sv_pattern(&arm.pattern);
                let val = emit_expr(&arm.expr, resolver);
                out = format!("({sel} == {pat}) ? {val} : {out}");
            }
            out
        }
        _ => sk_expr(expr, resolver),
    }
}

fn emit_sv_cast(expr: &HirExpression, target: &HirType, resolver: &NameResolver) -> String {
    let inner = emit_expr(expr, resolver);
    match target {
        HirType::Int(n) => format!("$signed({n}'({inner}))"),
        HirType::Nat(n) => format!("{n}'({inner})"),
        _ => inner,
    }
}

fn emit_sv_binary_op(op: &HirBinaryOp) -> &'static str {
    match op {
        HirBinaryOp::Add => "+",
        HirBinaryOp::WidenAdd => "+",
        HirBinaryOp::Sub => "-",
        HirBinaryOp::Mul => "*",
        HirBinaryOp::Div => "/",
        HirBinaryOp::Mod => "%",
        HirBinaryOp::And => "&",
        HirBinaryOp::Or => "|",
        HirBinaryOp::Xor => "^",
        HirBinaryOp::Equal => "==",
        HirBinaryOp::NotEqual => "!=",
        HirBinaryOp::Less => "<",
        HirBinaryOp::LessEqual => "<=",
        HirBinaryOp::Greater => ">",
        HirBinaryOp::GreaterEqual => ">=",
        HirBinaryOp::LogicalAnd => "&&",
        HirBinaryOp::LogicalOr => "||",
        HirBinaryOp::LeftShift => "<<",
        HirBinaryOp::RightShift => ">>",
    }
}

fn emit_type(ty: &HirType) -> String {
    match ty {
        HirType::Bit(1) => "logic".to_string(),
        HirType::Bit(n) => format!("logic [{n_1}:0]", n_1 = n - 1),
        HirType::Bool => "logic".to_string(),
        HirType::Logic(1) => "logic".to_string(),
        HirType::Logic(n) => format!("logic [{n_1}:0]", n_1 = n - 1),
        HirType::Int(n) => format!("logic signed [{n_1}:0]", n_1 = n - 1),
        HirType::Nat(n) => format!("logic [{n_1}:0]", n_1 = n - 1),
        HirType::Clock(_) => "logic".to_string(),
        HirType::Reset { .. } => "logic".to_string(),
        HirType::Array(inner, n) => format!("{} [0:{}]", emit_type(inner), n - 1),
        HirType::Custom(name) => name.clone(),
        HirType::Struct(s) => s.name.clone(),
        HirType::Enum(e) => e.name.clone(),
        HirType::BitParam(p) => format!("logic [{p}-1:0]"),
        HirType::NatParam(p) => format!("logic [{p}-1:0]"),
        HirType::IntParam(p) => format!("logic signed [{p}-1:0]"),
        HirType::LogicParam(p) => format!("logic [{p}-1:0]"),
        HirType::BitExpr(e) => {
            let w = sk_expr(e, &NameResolver::from_empty());
            format!("logic [{w}-1:0]")
        }
        HirType::NatExpr(e) => {
            let w = sk_expr(e, &NameResolver::from_empty());
            format!("logic [{w}-1:0]")
        }
        HirType::IntExpr(e) => {
            let w = sk_expr(e, &NameResolver::from_empty());
            format!("logic signed [{w}-1:0]")
        }
        _ => format!("logic /* unsupported: {:?} */", ty),
    }
}

fn emit_sv_param_type(ty: &HirType) -> String {
    match ty {
        HirType::Nat(_) | HirType::Int(_) => "int".to_string(),
        HirType::Bool => "bit".to_string(),
        _ => "int".to_string(),
    }
}

fn emit_lvalue(lval: &HirLValue, resolver: &NameResolver) -> String {
    match lval {
        HirLValue::Signal(id) => resolver.signal_name(*id).to_string(),
        HirLValue::Variable(id) => resolver.variable_name(*id).to_string(),
        HirLValue::Port(id) => resolver.port_name(*id).to_string(),
        HirLValue::Index(base, idx) => {
            format!(
                "{}[{}]",
                emit_lvalue(base, resolver),
                emit_expr(idx, resolver)
            )
        }
        HirLValue::Range(base, hi, lo) => {
            format!(
                "{}[{}:{}]",
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
