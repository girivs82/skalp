//! Optimization passes for MIR
//!
//! This module contains optimization passes like:
//! - Dead code elimination
//! - Common subexpression elimination
//! - Constant folding
//! - Loop unrolling

use crate::mir::*;
use std::collections::HashSet;

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

    /// Mark signals and variables as used in an expression
    fn mark_used_in_expression(&mut self, expr: &Expression) {
        match &expr.kind {
            ExpressionKind::Ref(lval) => self.mark_used_in_lvalue(lval),
            ExpressionKind::Binary { left, right, .. } => {
                self.mark_used_in_expression(left);
                self.mark_used_in_expression(right);
            }
            ExpressionKind::Unary { operand, .. } => {
                self.mark_used_in_expression(operand);
            }
            ExpressionKind::Conditional {
                cond,
                then_expr,
                else_expr,
            } => {
                self.mark_used_in_expression(cond);
                self.mark_used_in_expression(then_expr);
                self.mark_used_in_expression(else_expr);
            }
            ExpressionKind::Concat(exprs) => {
                for expr in exprs {
                    self.mark_used_in_expression(expr);
                }
            }
            ExpressionKind::Replicate { count, value } => {
                self.mark_used_in_expression(count);
                self.mark_used_in_expression(value);
            }
            ExpressionKind::FunctionCall { args, .. } => {
                for arg in args {
                    self.mark_used_in_expression(arg);
                }
            }
            ExpressionKind::Cast { expr, .. } => {
                self.mark_used_in_expression(expr);
            }
            _ => {}
        }
    }

    /// Mark signals and variables as used in an lvalue
    fn mark_used_in_lvalue(&mut self, lval: &LValue) {
        match lval {
            LValue::Signal(id) => {
                self.used_signals.insert(*id);
            }
            LValue::Variable(id) => {
                self.used_variables.insert(*id);
            }
            LValue::BitSelect { base, index } => {
                self.mark_used_in_lvalue(base);
                self.mark_used_in_expression(index);
            }
            LValue::RangeSelect { base, high, low } => {
                self.mark_used_in_lvalue(base);
                self.mark_used_in_expression(high);
                self.mark_used_in_expression(low);
            }
            LValue::Concat(lvals) => {
                for lval in lvals {
                    self.mark_used_in_lvalue(lval);
                }
            }
            _ => {}
        }
    }

    /// Mark used items in a statement
    fn mark_used_in_statement(&mut self, stmt: &Statement) {
        match stmt {
            Statement::Assignment(assign) => {
                self.mark_used_in_expression(&assign.rhs);
                // Don't mark LHS as used, only as defined
            }
            Statement::If(if_stmt) => {
                self.mark_used_in_expression(&if_stmt.condition);
                self.mark_used_in_block(&if_stmt.then_block);
                if let Some(else_block) = &if_stmt.else_block {
                    self.mark_used_in_block(else_block);
                }
            }
            Statement::Case(case_stmt) => {
                self.mark_used_in_expression(&case_stmt.expr);
                for item in &case_stmt.items {
                    for value in &item.values {
                        self.mark_used_in_expression(value);
                    }
                    self.mark_used_in_block(&item.block);
                }
                if let Some(default) = &case_stmt.default {
                    self.mark_used_in_block(default);
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
                        // Mark init assignment as used
                        self.mark_used_in_expression(&init.rhs);

                        self.mark_used_in_expression(condition);

                        // Mark update assignment as used
                        self.mark_used_in_expression(&update.rhs);

                        self.mark_used_in_block(body);
                    }
                    LoopStatement::While { condition, body } => {
                        self.mark_used_in_expression(condition);
                        self.mark_used_in_block(body);
                    }
                }
            }
            Statement::Block(block) => {
                self.mark_used_in_block(block);
            }
            Statement::ResolvedConditional(resolved) => {
                // Mark expressions in the resolved priority mux
                for case in &resolved.resolved.cases {
                    self.mark_used_in_expression(&case.condition);
                    self.mark_used_in_expression(&case.value);
                }
                self.mark_used_in_expression(&resolved.resolved.default);
            }
            Statement::Assert(assert_stmt) => {
                // Mark expressions used in assertion condition
                self.mark_used_in_expression(&assert_stmt.condition);
            }
            Statement::Assume(assume_stmt) => {
                // Mark expressions used in assumption condition
                self.mark_used_in_expression(&assume_stmt.condition);
            }
            Statement::Cover(cover_stmt) => {
                // Mark expressions used in cover condition
                self.mark_used_in_expression(&cover_stmt.condition);
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

    /// Fold expressions recursively
    fn fold_expression(&self, expr: &mut Expression) {
        match &mut expr.kind {
            ExpressionKind::Binary { op, left, right } => {
                // First fold children
                self.fold_expression(left);
                self.fold_expression(right);

                // Then try to fold this expression
                if let Some(folded) = self.fold_binary(op, left, right) {
                    *expr = folded;
                }
            }
            ExpressionKind::Unary { op, operand } => {
                // First fold child
                self.fold_expression(operand);

                // Then try to fold this expression
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

                // If condition is constant, select appropriate branch
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
                // Fold the inner expression, but preserve the cast
                self.fold_expression(inner_expr);
            }
            _ => {}
        }
    }

    /// Fold statements recursively
    fn fold_statement(&self, stmt: &mut Statement) {
        match stmt {
            Statement::Assignment(assign) => {
                self.fold_expression(&mut assign.rhs);
            }
            Statement::If(if_stmt) => {
                self.fold_expression(&mut if_stmt.condition);
                self.fold_block(&mut if_stmt.then_block);
                if let Some(else_block) = &mut if_stmt.else_block {
                    self.fold_block(else_block);
                }
            }
            Statement::Case(case_stmt) => {
                self.fold_expression(&mut case_stmt.expr);
                for item in &mut case_stmt.items {
                    for value in &mut item.values {
                        self.fold_expression(value);
                    }
                    self.fold_block(&mut item.block);
                }
                if let Some(default) = &mut case_stmt.default {
                    self.fold_block(default);
                }
            }
            Statement::Block(block) => {
                self.fold_block(block);
            }
            _ => {}
        }
    }

    /// Fold blocks recursively
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
