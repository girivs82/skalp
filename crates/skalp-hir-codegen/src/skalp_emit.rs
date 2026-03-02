//! HIR → skalp source code emitter

use crate::{emit_comments, indent, NameResolver};
use anyhow::Result;
use skalp_frontend::hir::*;
use std::fmt::Write;

pub(crate) fn emit_file(hir: &Hir, resolver: &NameResolver) -> Result<String> {
    let mut out = String::new();

    emit_comments(&mut out, &hir.comments, "//", "");

    for entity in &hir.entities {
        emit_entity(&mut out, entity, resolver);
        writeln!(out)?;
    }

    for (i, imp) in hir.implementations.iter().enumerate() {
        emit_impl(&mut out, imp, resolver);
        if i + 1 < hir.implementations.len() {
            writeln!(out)?;
        }
    }

    Ok(out)
}

fn emit_entity(out: &mut String, entity: &HirEntity, resolver: &NameResolver) {
    emit_comments(out, &entity.comments, "//", "");

    if entity.is_async {
        out.push_str("async ");
    }
    write!(out, "entity {}", entity.name).unwrap();

    // Generics
    if !entity.generics.is_empty() {
        out.push('<');
        for (i, generic) in entity.generics.iter().enumerate() {
            if i > 0 {
                out.push_str(", ");
            }
            emit_generic(out, generic);
        }
        out.push('>');
    }

    out.push_str(" {\n");

    // Ports
    for port in &entity.ports {
        emit_comments(out, &port.comments, "//", &indent(1));
        let ind = indent(1);
        let dir = match port.direction {
            HirPortDirection::Input => "in",
            HirPortDirection::Output => "out",
            HirPortDirection::Bidirectional => "inout",
            HirPortDirection::Protocol => "port",
        };
        let ty = emit_type(&port.port_type, resolver);
        writeln!(out, "{ind}{dir} {}: {ty}", port.name).unwrap();
    }

    out.push_str("}\n");
}

fn emit_generic(out: &mut String, generic: &HirGeneric) {
    match &generic.param_type {
        HirGenericType::Width => {
            write!(out, "{}", generic.name).unwrap();
        }
        HirGenericType::Const(ty) => {
            write!(out, "{}: {}", generic.name, emit_type_inline(ty)).unwrap();
        }
        HirGenericType::Type => {
            write!(out, "{}: type", generic.name).unwrap();
        }
        HirGenericType::TypeWithBounds(bounds) => {
            write!(out, "{}: {}", generic.name, bounds.join(" + ")).unwrap();
        }
        HirGenericType::ClockDomain => {
            write!(out, "'{}", generic.name).unwrap();
        }
        HirGenericType::PowerDomain { .. } => {
            write!(out, "'{}", generic.name).unwrap();
        }
        HirGenericType::Intent => {
            write!(out, "{}: intent", generic.name).unwrap();
        }
    }
    if let Some(ref default) = generic.default_value {
        write!(
            out,
            " = {}",
            emit_expression_inline(default, &NameResolver::from_empty())
        )
        .unwrap();
    }
}

fn emit_impl(out: &mut String, imp: &HirImplementation, resolver: &NameResolver) {
    let entity_name = resolver.entity_name(imp.entity);
    writeln!(out, "impl {entity_name} {{").unwrap();

    // Signals
    for signal in &imp.signals {
        emit_signal(out, signal, resolver, 1);
    }

    // Variables
    for variable in &imp.variables {
        emit_variable(out, variable, resolver, 1);
    }

    // Constants
    for constant in &imp.constants {
        emit_constant(out, constant, resolver, 1);
    }

    // Functions
    for func in &imp.functions {
        emit_function(out, func, resolver, 1);
    }

    // Assignments (combinational)
    for assign in &imp.assignments {
        emit_assignment(out, assign, resolver, 1);
    }

    // Instances
    for inst in &imp.instances {
        emit_instance(out, inst, resolver, 1);
    }

    // Event blocks
    for eb in &imp.event_blocks {
        emit_event_block(out, eb, resolver, 1);
    }

    out.push_str("}\n");
}

fn emit_signal(out: &mut String, signal: &HirSignal, resolver: &NameResolver, level: usize) {
    let ind = indent(level);
    emit_comments(out, &signal.comments, "//", &ind);
    let ty = emit_type(&signal.signal_type, resolver);
    write!(out, "{ind}signal {}: {ty}", signal.name).unwrap();
    if let Some(ref init) = signal.initial_value {
        write!(out, " = {}", emit_expression_inline(init, resolver)).unwrap();
    }
    writeln!(out).unwrap();
}

fn emit_variable(out: &mut String, variable: &HirVariable, resolver: &NameResolver, level: usize) {
    let ind = indent(level);
    emit_comments(out, &variable.comments, "//", &ind);
    let ty = emit_type(&variable.var_type, resolver);
    write!(out, "{ind}var {}: {ty}", variable.name).unwrap();
    if let Some(ref init) = variable.initial_value {
        write!(out, " = {}", emit_expression_inline(init, resolver)).unwrap();
    }
    writeln!(out).unwrap();
}

fn emit_constant(out: &mut String, constant: &HirConstant, resolver: &NameResolver, level: usize) {
    let ind = indent(level);
    emit_comments(out, &constant.comments, "//", &ind);
    let ty = emit_type(&constant.const_type, resolver);
    writeln!(
        out,
        "{ind}const {}: {ty} = {}",
        constant.name,
        emit_expression_inline(&constant.value, resolver)
    )
    .unwrap();
}

fn emit_function(out: &mut String, func: &HirFunction, resolver: &NameResolver, level: usize) {
    let ind = indent(level);
    emit_comments(out, &func.comments, "//", &ind);
    if func.is_const {
        write!(out, "{ind}const ").unwrap();
    } else {
        write!(out, "{ind}").unwrap();
    }
    write!(out, "fn {}", func.name).unwrap();

    // Generics
    if !func.generics.is_empty() {
        out.push('<');
        for (i, generic) in func.generics.iter().enumerate() {
            if i > 0 {
                out.push_str(", ");
            }
            emit_generic(out, generic);
        }
        out.push('>');
    }

    // Parameters
    out.push('(');
    for (i, param) in func.params.iter().enumerate() {
        if i > 0 {
            out.push_str(", ");
        }
        let ty = emit_type(&param.param_type, resolver);
        write!(out, "{}: {ty}", param.name).unwrap();
    }
    out.push(')');

    // Return type
    if let Some(ref ret_ty) = func.return_type {
        write!(out, " -> {}", emit_type(ret_ty, resolver)).unwrap();
    }

    writeln!(out, " {{").unwrap();
    for stmt in &func.body {
        emit_statement(out, stmt, resolver, level + 1);
    }
    writeln!(out, "{ind}}}").unwrap();
}

fn emit_assignment(
    out: &mut String,
    assign: &HirAssignment,
    resolver: &NameResolver,
    level: usize,
) {
    let ind = indent(level);
    emit_comments(out, &assign.comments, "//", &ind);
    let lhs = emit_lvalue(&assign.lhs, resolver);
    let rhs = emit_expression_inline(&assign.rhs, resolver);
    writeln!(out, "{ind}{lhs} = {rhs}").unwrap();
}

fn emit_instance(out: &mut String, inst: &HirInstance, resolver: &NameResolver, level: usize) {
    let ind = indent(level);
    emit_comments(out, &inst.comments, "//", &ind);
    let entity_name = resolver.entity_name(inst.entity);
    write!(out, "{ind}let {} = {entity_name}", inst.name).unwrap();

    // Generic arguments
    if !inst.generic_args.is_empty() || !inst.named_generic_args.is_empty() {
        out.push('<');
        let mut first = true;
        for arg in &inst.generic_args {
            if !first {
                out.push_str(", ");
            }
            write!(out, "{}", emit_expression_inline(arg, resolver)).unwrap();
            first = false;
        }
        for (name, arg) in &inst.named_generic_args {
            if !first {
                out.push_str(", ");
            }
            write!(out, "{name}: {}", emit_expression_inline(arg, resolver)).unwrap();
            first = false;
        }
        out.push('>');
    }

    // Port connections
    out.push_str(" {\n");
    for conn in &inst.connections {
        let ind2 = indent(level + 1);
        writeln!(
            out,
            "{ind2}{}: {}",
            conn.port,
            emit_expression_inline(&conn.expr, resolver)
        )
        .unwrap();
    }
    writeln!(out, "{ind}}}").unwrap();
}

fn emit_event_block(out: &mut String, eb: &HirEventBlock, resolver: &NameResolver, level: usize) {
    let ind = indent(level);
    emit_comments(out, &eb.comments, "//", &ind);
    write!(out, "{ind}on(").unwrap();
    for (i, trigger) in eb.triggers.iter().enumerate() {
        if i > 0 {
            out.push_str(" | ");
        }
        let sig = match &trigger.signal {
            HirEventSignal::Port(id) => resolver.port_name(*id).to_string(),
            HirEventSignal::Signal(id) => resolver.signal_name(*id).to_string(),
        };
        let edge = match trigger.edge {
            HirEdgeType::Rising => "rise",
            HirEdgeType::Falling => "fall",
            HirEdgeType::Both => "rise", // approximate
            HirEdgeType::Active => "active",
            HirEdgeType::Inactive => "inactive",
        };
        write!(out, "{sig}.{edge}").unwrap();
    }
    writeln!(out, ") {{").unwrap();
    for stmt in &eb.statements {
        emit_statement(out, stmt, resolver, level + 1);
    }
    writeln!(out, "{ind}}}").unwrap();
}

fn emit_statement(out: &mut String, stmt: &HirStatement, resolver: &NameResolver, level: usize) {
    let ind = indent(level);
    match stmt {
        HirStatement::Assignment(assign) => {
            emit_assignment(out, assign, resolver, level);
        }
        HirStatement::If(if_stmt) => {
            let cond = emit_expression_inline(&if_stmt.condition, resolver);
            writeln!(out, "{ind}if ({cond}) {{").unwrap();
            for s in &if_stmt.then_statements {
                emit_statement(out, s, resolver, level + 1);
            }
            if let Some(ref else_stmts) = if_stmt.else_statements {
                writeln!(out, "{ind}}} else {{").unwrap();
                for s in else_stmts {
                    emit_statement(out, s, resolver, level + 1);
                }
            }
            writeln!(out, "{ind}}}").unwrap();
        }
        HirStatement::Match(match_stmt) => {
            let expr = emit_expression_inline(&match_stmt.expr, resolver);
            writeln!(out, "{ind}match {expr} {{").unwrap();
            for arm in &match_stmt.arms {
                let pat = emit_pattern(&arm.pattern);
                write!(out, "    {pat}").unwrap();
                if let Some(ref guard) = arm.guard {
                    write!(out, " if {}", emit_expression_inline(guard, resolver)).unwrap();
                }
                writeln!(out, " => {{").unwrap();
                for s in &arm.statements {
                    emit_statement(out, s, resolver, level + 2);
                }
                writeln!(out, "{}    }}", indent(level)).unwrap();
            }
            writeln!(out, "{ind}}}").unwrap();
        }
        HirStatement::Let(let_stmt) => {
            let ty = emit_type(&let_stmt.var_type, resolver);
            let val = emit_expression_inline(&let_stmt.value, resolver);
            let mutk = if let_stmt.mutable { "mut " } else { "" };
            writeln!(out, "{ind}let {mutk}{}: {ty} = {val}", let_stmt.name).unwrap();
        }
        HirStatement::Return(expr) => {
            if let Some(ref e) = expr {
                writeln!(out, "{ind}return {}", emit_expression_inline(e, resolver)).unwrap();
            } else {
                writeln!(out, "{ind}return").unwrap();
            }
        }
        HirStatement::Expression(expr) => {
            writeln!(out, "{ind}{}", emit_expression_inline(expr, resolver)).unwrap();
        }
        HirStatement::For(for_stmt) => {
            let start = emit_expression_inline(&for_stmt.range.start, resolver);
            let end = emit_expression_inline(&for_stmt.range.end, resolver);
            let range_op = if for_stmt.range.inclusive {
                "..="
            } else {
                ".."
            };
            writeln!(
                out,
                "{ind}for {} in {start}{range_op}{end} {{",
                for_stmt.iterator
            )
            .unwrap();
            for s in &for_stmt.body {
                emit_statement(out, s, resolver, level + 1);
            }
            writeln!(out, "{ind}}}").unwrap();
        }
        HirStatement::Block(stmts) => {
            writeln!(out, "{ind}{{").unwrap();
            for s in stmts {
                emit_statement(out, s, resolver, level + 1);
            }
            writeln!(out, "{ind}}}").unwrap();
        }
        HirStatement::GenerateFor(gen) => {
            let start = emit_expression_inline(&gen.range.start, resolver);
            let end = emit_expression_inline(&gen.range.end, resolver);
            let range_op = if gen.range.inclusive { "..=" } else { ".." };
            writeln!(
                out,
                "{ind}generate for {} in {start}{range_op}{end} {{",
                gen.iterator
            )
            .unwrap();
            emit_generate_body(out, &gen.body, resolver, level + 1);
            writeln!(out, "{ind}}}").unwrap();
        }
        HirStatement::GenerateIf(gen) => {
            let cond = emit_expression_inline(&gen.condition, resolver);
            writeln!(out, "{ind}generate if ({cond}) {{").unwrap();
            emit_generate_body(out, &gen.then_body, resolver, level + 1);
            if let Some(ref else_body) = gen.else_body {
                writeln!(out, "{ind}}} else {{").unwrap();
                emit_generate_body(out, else_body, resolver, level + 1);
            }
            writeln!(out, "{ind}}}").unwrap();
        }
        HirStatement::Barrier(barrier) => {
            writeln!(out, "{ind}barrier // stage {}", barrier.stage_id).unwrap();
        }
        _ => {
            writeln!(out, "{ind}// unsupported statement").unwrap();
        }
    }
}

fn emit_generate_body(
    out: &mut String,
    body: &HirGenerateBody,
    resolver: &NameResolver,
    level: usize,
) {
    for signal in &body.signals {
        emit_signal(out, signal, resolver, level);
    }
    for inst in &body.instances {
        emit_instance(out, inst, resolver, level);
    }
    for eb in &body.event_blocks {
        emit_event_block(out, eb, resolver, level);
    }
    for assign in &body.assignments {
        emit_assignment(out, assign, resolver, level);
    }
    for stmt in &body.generate_stmts {
        emit_statement(out, stmt, resolver, level);
    }
}

fn emit_pattern(pat: &HirPattern) -> String {
    match pat {
        HirPattern::Literal(lit) => emit_literal(lit),
        HirPattern::Variable(name) => name.clone(),
        HirPattern::Wildcard => "_".to_string(),
        HirPattern::Tuple(pats) => {
            let inner: Vec<_> = pats.iter().map(emit_pattern).collect();
            format!("({})", inner.join(", "))
        }
        HirPattern::Path(enum_name, variant) => format!("{enum_name}::{variant}"),
    }
}

fn emit_literal(lit: &HirLiteral) -> String {
    match lit {
        HirLiteral::Integer(n) => n.to_string(),
        HirLiteral::Boolean(b) => b.to_string(),
        HirLiteral::Float(f) => format!("{f}"),
        HirLiteral::String(s) => format!("\"{s}\""),
        HirLiteral::BitVector(bits) => {
            let mut s = String::from("0b");
            for b in bits.iter().rev() {
                s.push(if *b { '1' } else { '0' });
            }
            s
        }
    }
}

pub(crate) fn emit_expression_inline(expr: &HirExpression, resolver: &NameResolver) -> String {
    match expr {
        HirExpression::Literal(lit) => emit_literal(lit),
        HirExpression::Signal(id) => resolver.signal_name(*id).to_string(),
        HirExpression::Port(id) => resolver.port_name(*id).to_string(),
        HirExpression::Variable(id) => resolver.variable_name(*id).to_string(),
        HirExpression::Constant(id) => resolver.constant_name(*id).to_string(),
        HirExpression::GenericParam(name) => name.clone(),
        HirExpression::Binary(bin) => {
            let left = emit_expression_inline(&bin.left, resolver);
            let right = emit_expression_inline(&bin.right, resolver);
            let op = emit_binary_op(&bin.op);
            format!("({left} {op} {right})")
        }
        HirExpression::Unary(un) => {
            let operand = emit_expression_inline(&un.operand, resolver);
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
            let args: Vec<_> = call
                .args
                .iter()
                .map(|a| emit_expression_inline(a, resolver))
                .collect();
            format!("{}({})", call.function, args.join(", "))
        }
        HirExpression::Index(base, idx) => {
            format!(
                "{}[{}]",
                emit_expression_inline(base, resolver),
                emit_expression_inline(idx, resolver)
            )
        }
        HirExpression::Range(base, lo, hi) => {
            format!(
                "{}[{}..{}]",
                emit_expression_inline(base, resolver),
                emit_expression_inline(lo, resolver),
                emit_expression_inline(hi, resolver)
            )
        }
        HirExpression::FieldAccess { base, field } => {
            format!("{}.{field}", emit_expression_inline(base, resolver))
        }
        HirExpression::EnumVariant {
            enum_type, variant, ..
        } => {
            format!("{enum_type}::{variant}")
        }
        HirExpression::Concat(exprs) => {
            let parts: Vec<_> = exprs
                .iter()
                .map(|e| emit_expression_inline(e, resolver))
                .collect();
            format!("{{{}}}", parts.join(", "))
        }
        HirExpression::Ternary {
            condition,
            true_expr,
            false_expr,
        } => {
            format!(
                "if ({}) {{ {} }} else {{ {} }}",
                emit_expression_inline(condition, resolver),
                emit_expression_inline(true_expr, resolver),
                emit_expression_inline(false_expr, resolver)
            )
        }
        HirExpression::StructLiteral(sl) => {
            let fields: Vec<_> = sl
                .fields
                .iter()
                .map(|f| format!("{}: {}", f.name, emit_expression_inline(&f.value, resolver)))
                .collect();
            format!("{} {{ {} }}", sl.type_name, fields.join(", "))
        }
        HirExpression::ArrayLiteral(elems) => {
            let parts: Vec<_> = elems
                .iter()
                .map(|e| emit_expression_inline(e, resolver))
                .collect();
            format!("[{}]", parts.join(", "))
        }
        HirExpression::ArrayRepeat { value, count } => {
            format!(
                "[{}; {}]",
                emit_expression_inline(value, resolver),
                emit_expression_inline(count, resolver)
            )
        }
        HirExpression::TupleLiteral(elems) => {
            let parts: Vec<_> = elems
                .iter()
                .map(|e| emit_expression_inline(e, resolver))
                .collect();
            format!("({})", parts.join(", "))
        }
        HirExpression::Cast(cast) => {
            format!(
                "{} as {}",
                emit_expression_inline(&cast.expr, resolver),
                emit_type_inline(&cast.target_type)
            )
        }
        HirExpression::AssociatedConstant {
            type_name,
            constant_name,
        } => {
            format!("{type_name}::{constant_name}")
        }
        HirExpression::If(if_expr) => {
            format!(
                "if ({}) {{ {} }} else {{ {} }}",
                emit_expression_inline(&if_expr.condition, resolver),
                emit_expression_inline(&if_expr.then_expr, resolver),
                emit_expression_inline(&if_expr.else_expr, resolver)
            )
        }
        HirExpression::Match(match_expr) => {
            let mut s = format!(
                "match {} {{ ",
                emit_expression_inline(&match_expr.expr, resolver)
            );
            for arm in &match_expr.arms {
                write!(
                    s,
                    "{} => {}, ",
                    emit_pattern(&arm.pattern),
                    emit_expression_inline(&arm.expr, resolver)
                )
                .unwrap();
            }
            s.push('}');
            s
        }
        HirExpression::Block {
            statements,
            result_expr,
        } => {
            // For inline emission, just show the result expression
            let _ = statements;
            emit_expression_inline(result_expr, resolver)
        }
    }
}

fn emit_binary_op(op: &HirBinaryOp) -> &'static str {
    match op {
        HirBinaryOp::Add => "+",
        HirBinaryOp::WidenAdd => "+:",
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

pub(crate) fn emit_type(ty: &HirType, _resolver: &NameResolver) -> String {
    emit_type_inline(ty)
}

pub(crate) fn emit_type_inline(ty: &HirType) -> String {
    match ty {
        HirType::Bit(1) => "bit".to_string(),
        HirType::Bit(n) => format!("bit[{n}]"),
        HirType::Bool => "bool".to_string(),
        HirType::String => "string".to_string(),
        HirType::Logic(1) => "logic".to_string(),
        HirType::Logic(n) => format!("logic[{n}]"),
        HirType::Int(n) => format!("int[{n}]"),
        HirType::Nat(n) => format!("nat[{n}]"),
        HirType::Ncl(n) => format!("ncl[{n}]"),
        HirType::Clock(_) => "clock".to_string(),
        HirType::Reset { polarity, .. } => match polarity {
            HirResetPolarity::ActiveHigh => "reset(active_high)".to_string(),
            HirResetPolarity::ActiveLow => "reset(active_low)".to_string(),
        },
        HirType::Event => "event".to_string(),
        HirType::Stream(inner) => format!("stream<{}>", emit_type_inline(inner)),
        HirType::Array(inner, n) => format!("[{}; {n}]", emit_type_inline(inner)),
        HirType::Custom(name) => name.clone(),
        HirType::Struct(s) => s.name.clone(),
        HirType::Enum(e) => e.name.clone(),
        HirType::Union(u) => u.name.clone(),
        HirType::BitParam(p) => format!("bit[{p}]"),
        HirType::LogicParam(p) => format!("logic[{p}]"),
        HirType::IntParam(p) => format!("int[{p}]"),
        HirType::NatParam(p) => format!("nat[{p}]"),
        HirType::NclParam(p) => format!("ncl[{p}]"),
        HirType::BitExpr(e) => format!(
            "bit[{}]",
            emit_expression_inline(e, &NameResolver::from_empty())
        ),
        HirType::LogicExpr(e) => format!(
            "logic[{}]",
            emit_expression_inline(e, &NameResolver::from_empty())
        ),
        HirType::IntExpr(e) => format!(
            "int[{}]",
            emit_expression_inline(e, &NameResolver::from_empty())
        ),
        HirType::NatExpr(e) => format!(
            "nat[{}]",
            emit_expression_inline(e, &NameResolver::from_empty())
        ),
        HirType::NclExpr(e) => format!(
            "ncl[{}]",
            emit_expression_inline(e, &NameResolver::from_empty())
        ),
        HirType::ArrayExpr(inner, size) => format!(
            "[{}; {}]",
            emit_type_inline(inner),
            emit_expression_inline(size, &NameResolver::from_empty())
        ),
        HirType::Float16 => "fp16".to_string(),
        HirType::Float32 => "fp32".to_string(),
        HirType::Float64 => "fp64".to_string(),
        HirType::Vec2(inner) => format!("vec2<{}>", emit_type_inline(inner)),
        HirType::Vec3(inner) => format!("vec3<{}>", emit_type_inline(inner)),
        HirType::Vec4(inner) => format!("vec4<{}>", emit_type_inline(inner)),
        HirType::FpParametric { format } => format!(
            "fp<{}>",
            emit_expression_inline(format, &NameResolver::from_empty())
        ),
        HirType::FixedParametric {
            width,
            frac,
            signed,
        } => format!(
            "fixed<{}, {}, {}>",
            emit_expression_inline(width, &NameResolver::from_empty()),
            emit_expression_inline(frac, &NameResolver::from_empty()),
            emit_expression_inline(signed, &NameResolver::from_empty())
        ),
        HirType::IntParametric { width, signed } => format!(
            "int<{}, {}>",
            emit_expression_inline(width, &NameResolver::from_empty()),
            emit_expression_inline(signed, &NameResolver::from_empty())
        ),
        HirType::VecParametric {
            element_type,
            dimension,
        } => format!(
            "vec<{}, {}>",
            emit_type_inline(element_type),
            emit_expression_inline(dimension, &NameResolver::from_empty())
        ),
        HirType::Tuple(types) => {
            let parts: Vec<_> = types.iter().map(emit_type_inline).collect();
            format!("({})", parts.join(", "))
        }
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
                emit_expression_inline(idx, resolver)
            )
        }
        HirLValue::Range(base, lo, hi) => {
            format!(
                "{}[{}..{}]",
                emit_lvalue(base, resolver),
                emit_expression_inline(lo, resolver),
                emit_expression_inline(hi, resolver)
            )
        }
        HirLValue::FieldAccess { base, field } => {
            format!("{}.{field}", emit_lvalue(base, resolver))
        }
    }
}
