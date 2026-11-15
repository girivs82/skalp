//! `let` bindings of a clocked block (blocking writes of process
//! variables), as the lowerings to simulation and gates drive them.
//!
//! A binding is a per-cycle temporary: its variable is driven
//! combinationally, never registered. A binding is visible only in its own
//! scope, so a variable bound once is driven by that binding's value
//! unconditionally. A variable bound in several places (the same name in two
//! arms of a `match`, a `let mut` reassigned under an `if`) is driven by the
//! last binding whose path is taken, `g2 ? v2 : g1 ? v1 : 0`, each guard the
//! conditions leading to its binding.

use crate::mir::{
    AssignmentKind, BinaryOp, Expression, ExpressionKind, LValue, Statement, UnaryOp, Value,
    VariableId,
};

/// The value driving each variable bound in `statements`, in order of first
/// binding. `width` gives a variable's width (for the 0 where no binding of a
/// variable bound in several places applies). A variable with a binding that
/// reads the variable itself (`x = x + 1`) has no combinational value and is
/// left out.
pub fn merged_variable_bindings(
    statements: &[Statement],
    width: impl Fn(VariableId) -> usize,
) -> Vec<(VariableId, Expression)> {
    #[allow(clippy::type_complexity)]
    let mut by_variable: Vec<(VariableId, Vec<(Option<Expression>, &Expression)>)> = Vec::new();
    for (guard, assign) in guarded_variable_bindings(statements, None) {
        let LValue::Variable(id) = &assign.lhs else {
            continue;
        };
        match by_variable.iter_mut().find(|(v, _)| v == id) {
            Some((_, writes)) => writes.push((guard, &assign.rhs)),
            None => by_variable.push((*id, vec![(guard, &assign.rhs)])),
        }
    }
    let mut merged = Vec::new();
    for (id, writes) in by_variable {
        if let [(_, only)] = writes.as_slice() {
            merged.push((id, (*only).clone()));
            continue;
        }
        if writes
            .iter()
            .any(|(_, rhs)| expression_reads_variable(rhs, id))
        {
            continue;
        }
        let mut value = Expression::with_unknown_type(ExpressionKind::Literal(Value::BitVector {
            width: width(id),
            value: 0,
        }));
        for (guard, rhs) in writes {
            value = match guard {
                None => rhs.clone(),
                Some(cond) => Expression::new(
                    ExpressionKind::Conditional {
                        cond: Box::new(cond),
                        then_expr: Box::new(rhs.clone()),
                        else_expr: Box::new(value),
                    },
                    rhs.ty.clone(),
                ),
            };
        }
        merged.push((id, value));
    }
    merged
}

/// `a && b`, where a missing `a` is true
fn and_guard(a: Option<Expression>, b: Expression) -> Expression {
    match a {
        None => b,
        Some(a) => Expression::with_unknown_type(ExpressionKind::Binary {
            op: BinaryOp::LogicalAnd,
            left: Box::new(a),
            right: Box::new(b),
        }),
    }
}

fn not_expression(e: Expression) -> Expression {
    Expression::with_unknown_type(ExpressionKind::Unary {
        op: UnaryOp::Not,
        operand: Box::new(e),
    })
}

/// `expr == v1 || expr == v2 || ...`
fn case_match(expr: &Expression, values: &[Expression]) -> Option<Expression> {
    values
        .iter()
        .map(|v| {
            Expression::with_unknown_type(ExpressionKind::Binary {
                op: BinaryOp::Equal,
                left: Box::new(expr.clone()),
                right: Box::new(v.clone()),
            })
        })
        .reduce(|a, b| {
            Expression::with_unknown_type(ExpressionKind::Binary {
                op: BinaryOp::LogicalOr,
                left: Box::new(a),
                right: Box::new(b),
            })
        })
}

/// Blocking writes of process variables (`let` bindings) in `statements`,
/// at any depth, in program order, each with the condition under which it
/// runs (`None`: always, given `guard`)
pub fn guarded_variable_bindings(
    statements: &[Statement],
    guard: Option<Expression>,
) -> Vec<(Option<Expression>, &crate::mir::Assignment)> {
    let mut out = Vec::new();
    collect(statements, guard, &mut out);
    out
}

fn collect<'a>(
    statements: &'a [Statement],
    guard: Option<Expression>,
    out: &mut Vec<(Option<Expression>, &'a crate::mir::Assignment)>,
) {
    for statement in statements {
        match statement {
            Statement::Assignment(a)
                if a.kind == AssignmentKind::Blocking && matches!(a.lhs, LValue::Variable(_)) =>
            {
                out.push((guard.clone(), a))
            }
            Statement::Block(b) => collect(&b.statements, guard.clone(), out),
            Statement::If(i) => {
                let then_guard = and_guard(guard.clone(), i.condition.clone());
                collect(&i.then_block.statements, Some(then_guard), out);
                if let Some(e) = &i.else_block {
                    let else_guard = and_guard(guard.clone(), not_expression(i.condition.clone()));
                    collect(&e.statements, Some(else_guard), out);
                }
            }
            Statement::Case(c) => {
                let mut earlier: Option<Expression> = None;
                for item in &c.items {
                    let Some(hit) = case_match(&c.expr, &item.values) else {
                        continue;
                    };
                    // The first matching arm is taken: this one if no earlier one matched
                    let taken = match &earlier {
                        None => hit.clone(),
                        Some(e) => and_guard(Some(not_expression(e.clone())), hit.clone()),
                    };
                    collect(
                        &item.block.statements,
                        Some(and_guard(guard.clone(), taken)),
                        out,
                    );
                    earlier = Some(match earlier {
                        None => hit,
                        Some(e) => Expression::with_unknown_type(ExpressionKind::Binary {
                            op: BinaryOp::LogicalOr,
                            left: Box::new(e),
                            right: Box::new(hit),
                        }),
                    });
                }
                if let Some(d) = &c.default {
                    let default_guard = match earlier {
                        None => guard.clone(),
                        Some(e) => Some(and_guard(guard.clone(), not_expression(e))),
                    };
                    collect(&d.statements, default_guard, out);
                }
            }
            _ => {}
        }
    }
}

/// Whether `expr` reads variable `id`
pub fn expression_reads_variable(expr: &Expression, id: VariableId) -> bool {
    fn lvalue_reads(lv: &LValue, id: VariableId) -> bool {
        match lv {
            LValue::Variable(v) => *v == id,
            LValue::Port(_) | LValue::Signal(_) => false,
            LValue::BitSelect { base, index } => {
                lvalue_reads(base, id) || expression_reads_variable(index, id)
            }
            LValue::RangeSelect { base, high, low } => {
                lvalue_reads(base, id)
                    || expression_reads_variable(high, id)
                    || expression_reads_variable(low, id)
            }
            LValue::Concat(parts) => parts.iter().any(|p| lvalue_reads(p, id)),
        }
    }
    match &expr.kind {
        ExpressionKind::Literal(_) => false,
        ExpressionKind::Ref(lv) => lvalue_reads(lv, id),
        ExpressionKind::Binary { left, right, .. } => {
            expression_reads_variable(left, id) || expression_reads_variable(right, id)
        }
        ExpressionKind::Unary { operand, .. } => expression_reads_variable(operand, id),
        ExpressionKind::Conditional {
            cond,
            then_expr,
            else_expr,
        } => {
            expression_reads_variable(cond, id)
                || expression_reads_variable(then_expr, id)
                || expression_reads_variable(else_expr, id)
        }
        ExpressionKind::Concat(parts) => parts.iter().any(|p| expression_reads_variable(p, id)),
        ExpressionKind::Replicate { count, value } => {
            expression_reads_variable(count, id) || expression_reads_variable(value, id)
        }
        ExpressionKind::FunctionCall { args, .. } => {
            args.iter().any(|a| expression_reads_variable(a, id))
        }
        ExpressionKind::Cast { expr, .. } => expression_reads_variable(expr, id),
        ExpressionKind::TupleFieldAccess { base, .. } => expression_reads_variable(base, id),
        ExpressionKind::FieldAccess { base, .. } => expression_reads_variable(base, id),
    }
}
