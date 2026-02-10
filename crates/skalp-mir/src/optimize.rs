//! Optimization passes for MIR
//!
//! This module contains optimization passes like:
//! - Dead code elimination
//! - Common subexpression elimination
//! - Constant folding
//! - Loop unrolling

use crate::mir::*;
use std::collections::HashSet;

const STACK_RED_ZONE: usize = 256 * 1024;
const STACK_GROW_SIZE: usize = 8 * 1024 * 1024;

/// Optimization pass trait
pub trait OptimizationPass {
    /// Apply the optimization to MIR
    fn apply(&mut self, mir: &mut Mir);

    /// Get the name of this pass
    fn name(&self) -> &str;
}

/// Dead code elimination pass
/// Removes unreachable and unused code
pub struct DeadCodeElimination {
    /// Track which signals are used
    used_signals: HashSet<SignalId>,
    /// Track which variables are used
    used_variables: HashSet<VariableId>,
}

impl DeadCodeElimination {
    /// Create a new dead code elimination pass
    pub fn new() -> Self {
        Self {
            used_signals: HashSet::new(),
            used_variables: HashSet::new(),
        }
    }

    /// Mark signals and variables as used in an expression (iterative)
    fn mark_used_in_expression(&mut self, expr: &Expression) {
        let mut expr_stack: Vec<&Expression> = vec![expr];
        let mut lval_stack: Vec<&LValue> = Vec::new();
        loop {
            // Process expression stack first
            if let Some(e) = expr_stack.pop() {
                match &e.kind {
                    ExpressionKind::Ref(lval) => lval_stack.push(lval),
                    ExpressionKind::Binary { left, right, .. } => {
                        expr_stack.push(left);
                        expr_stack.push(right);
                    }
                    ExpressionKind::Unary { operand, .. } => {
                        expr_stack.push(operand);
                    }
                    ExpressionKind::Conditional {
                        cond,
                        then_expr,
                        else_expr,
                    } => {
                        expr_stack.push(cond);
                        expr_stack.push(then_expr);
                        expr_stack.push(else_expr);
                    }
                    ExpressionKind::Concat(exprs) => {
                        for e in exprs {
                            expr_stack.push(e);
                        }
                    }
                    ExpressionKind::Replicate { count, value } => {
                        expr_stack.push(count);
                        expr_stack.push(value);
                    }
                    ExpressionKind::FunctionCall { args, .. } => {
                        for arg in args {
                            expr_stack.push(arg);
                        }
                    }
                    ExpressionKind::Cast { expr, .. } => {
                        expr_stack.push(expr);
                    }
                    _ => {}
                }
                continue;
            }
            // Then process lvalue stack
            if let Some(lval) = lval_stack.pop() {
                match lval {
                    LValue::Signal(id) => {
                        self.used_signals.insert(*id);
                    }
                    LValue::Variable(id) => {
                        self.used_variables.insert(*id);
                    }
                    LValue::BitSelect { base, index } => {
                        lval_stack.push(base);
                        expr_stack.push(index);
                    }
                    LValue::RangeSelect { base, high, low } => {
                        lval_stack.push(base);
                        expr_stack.push(high);
                        expr_stack.push(low);
                    }
                    LValue::Concat(lvals) => {
                        for lv in lvals {
                            lval_stack.push(lv);
                        }
                    }
                    _ => {}
                }
                continue;
            }
            break;
        }
    }

    /// Mark signals and variables as used in an lvalue (iterative)
    fn mark_used_in_lvalue(&mut self, lval: &LValue) {
        // Reuse mark_used_in_expression infrastructure by creating a dummy wrapper
        // or just iterate on lvalues directly
        let mut lval_stack: Vec<&LValue> = vec![lval];
        let mut expr_stack: Vec<&Expression> = Vec::new();
        loop {
            if let Some(lv) = lval_stack.pop() {
                match lv {
                    LValue::Signal(id) => {
                        self.used_signals.insert(*id);
                    }
                    LValue::Variable(id) => {
                        self.used_variables.insert(*id);
                    }
                    LValue::BitSelect { base, index } => {
                        lval_stack.push(base);
                        expr_stack.push(index);
                    }
                    LValue::RangeSelect { base, high, low } => {
                        lval_stack.push(base);
                        expr_stack.push(high);
                        expr_stack.push(low);
                    }
                    LValue::Concat(lvals) => {
                        for lv in lvals {
                            lval_stack.push(lv);
                        }
                    }
                    _ => {}
                }
                continue;
            }
            if let Some(e) = expr_stack.pop() {
                // For expressions within lvalues, use the full expression marking
                self.mark_used_in_expression(e);
                continue;
            }
            break;
        }
    }

    /// Mark used items in a statement (iterative over nested statements)
    fn mark_used_in_statement(&mut self, stmt: &Statement) {
        let mut stmt_stack: Vec<&Statement> = vec![stmt];
        while let Some(s) = stmt_stack.pop() {
            match s {
                Statement::Assignment(assign) => {
                    self.mark_used_in_expression(&assign.rhs);
                }
                Statement::If(if_stmt) => {
                    self.mark_used_in_expression(&if_stmt.condition);
                    for s in &if_stmt.then_block.statements {
                        stmt_stack.push(s);
                    }
                    if let Some(else_block) = &if_stmt.else_block {
                        for s in &else_block.statements {
                            stmt_stack.push(s);
                        }
                    }
                }
                Statement::Case(case_stmt) => {
                    self.mark_used_in_expression(&case_stmt.expr);
                    for item in &case_stmt.items {
                        for value in &item.values {
                            self.mark_used_in_expression(value);
                        }
                        for s in &item.block.statements {
                            stmt_stack.push(s);
                        }
                    }
                    if let Some(default) = &case_stmt.default {
                        for s in &default.statements {
                            stmt_stack.push(s);
                        }
                    }
                }
                Statement::Loop(loop_stmt) => {
                    match loop_stmt {
                        LoopStatement::For {
                            init,
                            condition,
                            update,
                            body,
                        } => {
                            self.mark_used_in_expression(&init.rhs);
                            self.mark_used_in_expression(condition);
                            self.mark_used_in_expression(&update.rhs);
                            for s in &body.statements {
                                stmt_stack.push(s);
                            }
                        }
                        LoopStatement::While { condition, body } => {
                            self.mark_used_in_expression(condition);
                            for s in &body.statements {
                                stmt_stack.push(s);
                            }
                        }
                    }
                }
                Statement::Block(block) => {
                    for s in &block.statements {
                        stmt_stack.push(s);
                    }
                }
                Statement::ResolvedConditional(resolved) => {
                    for case in &resolved.resolved.cases {
                        self.mark_used_in_expression(&case.condition);
                        self.mark_used_in_expression(&case.value);
                    }
                    self.mark_used_in_expression(&resolved.resolved.default);
                }
                Statement::Assert(assert_stmt) => {
                    self.mark_used_in_expression(&assert_stmt.condition);
                }
                Statement::Assume(assume_stmt) => {
                    self.mark_used_in_expression(&assume_stmt.condition);
                }
                Statement::Cover(cover_stmt) => {
                    self.mark_used_in_expression(&cover_stmt.condition);
                }
            }
        }
    }

    /// Mark used items in a block
    fn mark_used_in_block(&mut self, block: &Block) {
        for stmt in &block.statements {
            self.mark_used_in_statement(stmt);
        }
    }

    /// Remove unused signals from a module
    fn remove_unused_signals(&self, module: &mut Module) {
        module
            .signals
            .retain(|signal| self.used_signals.contains(&signal.id));
    }

    /// Remove unused variables from a module
    fn remove_unused_variables(&self, module: &mut Module) {
        module
            .variables
            .retain(|var| self.used_variables.contains(&var.id));
    }
}

impl Default for DeadCodeElimination {
    fn default() -> Self {
        Self::new()
    }
}

impl OptimizationPass for DeadCodeElimination {
    fn apply(&mut self, mir: &mut Mir) {
        for module in &mut mir.modules {
            // Reset tracking for each module
            self.used_signals.clear();
            self.used_variables.clear();

            // Mark used items in processes
            for process in &module.processes {
                self.mark_used_in_block(&process.body);
            }

            // Mark used items in continuous assignments
            // CRITICAL: Also mark LHS as used, because continuous assignments define
            // combinational signals that might be consumed by module instances or outputs
            for assign in &module.assignments {
                self.mark_used_in_expression(&assign.rhs);
                // Mark LHS as used too - it's a combinational signal that drives something
                self.mark_used_in_lvalue(&assign.lhs);
            }

            // Mark used items in module instances
            for instance in &module.instances {
                for expr in instance.connections.values() {
                    self.mark_used_in_expression(expr);
                }
            }

            // Also mark signals/variables that are assigned to ports as used
            for assign in &module.assignments {
                if let LValue::Port(_) = assign.lhs {
                    self.mark_used_in_expression(&assign.rhs);
                }
            }

            // Remove unused signals and variables
            self.remove_unused_signals(module);
            self.remove_unused_variables(module);
        }
    }

    fn name(&self) -> &str {
        "Dead Code Elimination"
    }
}

/// Constant folding optimization
/// Evaluates constant expressions at compile time
pub struct ConstantFolding;

impl ConstantFolding {
    /// Create a new constant folding pass
    pub fn new() -> Self {
        Self
    }

    /// Try to fold a binary expression
    fn fold_binary(
        &self,
        op: &BinaryOp,
        left: &Expression,
        right: &Expression,
    ) -> Option<Expression> {
        match (&left.kind, &right.kind) {
            (
                ExpressionKind::Literal(Value::Integer(l)),
                ExpressionKind::Literal(Value::Integer(r)),
            ) => {
                let result = match op {
                    BinaryOp::Add => l + r,
                    BinaryOp::Sub => l - r,
                    BinaryOp::Mul => l * r,
                    BinaryOp::Div if *r != 0 => l / r,
                    BinaryOp::Mod if *r != 0 => l % r,
                    BinaryOp::LeftShift => l << r,
                    BinaryOp::RightShift => l >> r,
                    _ => return None,
                };
                // Use the type from the left operand (both should have the same type for these ops)
                Some(Expression::literal(Value::Integer(result), left.ty.clone()))
            }
            _ => None,
        }
    }

    /// Try to fold a unary expression
    fn fold_unary(&self, op: &UnaryOp, operand: &Expression) -> Option<Expression> {
        match &operand.kind {
            ExpressionKind::Literal(Value::Integer(n)) => match op {
                UnaryOp::Negate => {
                    Some(Expression::literal(Value::Integer(-n), operand.ty.clone()))
                }
                UnaryOp::Not if *n == 0 => {
                    Some(Expression::literal(Value::Integer(1), operand.ty.clone()))
                }
                UnaryOp::Not => Some(Expression::literal(Value::Integer(0), operand.ty.clone())),
                _ => None,
            },
            _ => None,
        }
    }

    /// Fold expressions (stacker-protected)
    fn fold_expression(&self, expr: &mut Expression) {
        stacker::maybe_grow(STACK_RED_ZONE, STACK_GROW_SIZE, || {
            self.fold_expression_impl(expr)
        })
    }

    fn fold_expression_impl(&self, expr: &mut Expression) {
        match &mut expr.kind {
            ExpressionKind::Binary { op, left, right } => {
                self.fold_expression(left);
                self.fold_expression(right);
                if let Some(folded) = self.fold_binary(op, left, right) {
                    *expr = folded;
                }
            }
            ExpressionKind::Unary { op, operand } => {
                self.fold_expression(operand);
                if let Some(folded) = self.fold_unary(op, operand) {
                    *expr = folded;
                }
            }
            ExpressionKind::Conditional {
                cond,
                then_expr,
                else_expr,
            } => {
                self.fold_expression(cond);
                self.fold_expression(then_expr);
                self.fold_expression(else_expr);
                if let ExpressionKind::Literal(Value::Integer(n)) = &cond.kind {
                    *expr = if *n != 0 {
                        then_expr.as_ref().clone()
                    } else {
                        else_expr.as_ref().clone()
                    };
                }
            }
            ExpressionKind::Concat(exprs) => {
                for expr in exprs {
                    self.fold_expression(expr);
                }
            }
            ExpressionKind::Cast {
                expr: inner_expr, ..
            } => {
                self.fold_expression(inner_expr);
            }
            _ => {}
        }
    }

    /// Fold statements (iterative over nested statement blocks)
    fn fold_statement(&self, stmt: &mut Statement) {
        let mut stmt_stack: Vec<*mut Statement> = vec![stmt as *mut Statement];
        while let Some(s_ptr) = stmt_stack.pop() {
            // SAFETY: We own unique mutable access to these statements through the
            // original &mut Statement. Each pointer is only used once.
            let s = unsafe { &mut *s_ptr };
            match s {
                Statement::Assignment(assign) => {
                    self.fold_expression(&mut assign.rhs);
                }
                Statement::If(if_stmt) => {
                    self.fold_expression(&mut if_stmt.condition);
                    for s in &mut if_stmt.then_block.statements {
                        stmt_stack.push(s as *mut Statement);
                    }
                    if let Some(else_block) = &mut if_stmt.else_block {
                        for s in &mut else_block.statements {
                            stmt_stack.push(s as *mut Statement);
                        }
                    }
                }
                Statement::Case(case_stmt) => {
                    self.fold_expression(&mut case_stmt.expr);
                    for item in &mut case_stmt.items {
                        for value in &mut item.values {
                            self.fold_expression(value);
                        }
                        for s in &mut item.block.statements {
                            stmt_stack.push(s as *mut Statement);
                        }
                    }
                    if let Some(default) = &mut case_stmt.default {
                        for s in &mut default.statements {
                            stmt_stack.push(s as *mut Statement);
                        }
                    }
                }
                Statement::Block(block) => {
                    for s in &mut block.statements {
                        stmt_stack.push(s as *mut Statement);
                    }
                }
                _ => {}
            }
        }
    }

    /// Fold blocks
    fn fold_block(&self, block: &mut Block) {
        for stmt in &mut block.statements {
            self.fold_statement(stmt);
        }
    }
}

impl Default for ConstantFolding {
    fn default() -> Self {
        Self::new()
    }
}

impl OptimizationPass for ConstantFolding {
    fn apply(&mut self, mir: &mut Mir) {
        for module in &mut mir.modules {
            // Fold in processes
            for process in &mut module.processes {
                self.fold_block(&mut process.body);
            }

            // Fold in continuous assignments
            for assign in &mut module.assignments {
                self.fold_expression(&mut assign.rhs);
            }
        }
    }

    fn name(&self) -> &str {
        "Constant Folding"
    }
}
