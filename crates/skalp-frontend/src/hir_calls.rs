//! Names of the functions a piece of HIR calls.
//!
//! Importing an entity from another module (`use lib::Core;`) has to bring
//! along the free functions its implementation calls, and the functions those
//! call in turn: the merged HIR is compiled on its own, without the imported
//! module, so a helper that is not merged cannot be inlined and the
//! assignment that calls it is lost.

use crate::hir::{
    HirAssignment, HirEventBlock, HirExpression, HirFunction, HirGenerateBody, HirImplementation,
    HirInstance, HirLValue, HirPipelineStage, HirProperty, HirStatement,
};
use indexmap::IndexSet;

/// Every function name called anywhere in `implementation` (directly, not
/// through other functions), in first-seen order.
pub fn called_in_implementation(implementation: &HirImplementation) -> IndexSet<String> {
    let mut calls = CallCollector::default();
    for signal in &implementation.signals {
        if let Some(init) = &signal.initial_value {
            calls.expression(init);
        }
    }
    for variable in &implementation.variables {
        if let Some(init) = &variable.initial_value {
            calls.expression(init);
        }
    }
    for constant in &implementation.constants {
        calls.expression(&constant.value);
    }
    for function in &implementation.functions {
        calls.function(function);
    }
    for block in &implementation.event_blocks {
        calls.event_block(block);
    }
    for assignment in &implementation.assignments {
        calls.assignment(assignment);
    }
    for instance in &implementation.instances {
        calls.instance(instance);
    }
    calls.statements(&implementation.statements);
    calls.names
}

/// Every function name called in `function`'s body.
pub fn called_in_function(function: &HirFunction) -> IndexSet<String> {
    let mut calls = CallCollector::default();
    calls.function(function);
    calls.names
}

#[derive(Default)]
struct CallCollector {
    names: IndexSet<String>,
}

impl CallCollector {
    fn function(&mut self, function: &HirFunction) {
        self.statements(&function.body);
    }

    fn event_block(&mut self, block: &HirEventBlock) {
        self.statements(&block.statements);
    }

    fn assignment(&mut self, assignment: &HirAssignment) {
        self.lvalue(&assignment.lhs);
        self.expression(&assignment.rhs);
    }

    fn instance(&mut self, instance: &HirInstance) {
        for arg in &instance.generic_args {
            self.expression(arg);
        }
        for arg in instance.named_generic_args.values() {
            self.expression(arg);
        }
        for connection in &instance.connections {
            self.expression(&connection.expr);
        }
    }

    fn generate_body(&mut self, body: &HirGenerateBody) {
        for signal in &body.signals {
            if let Some(init) = &signal.initial_value {
                self.expression(init);
            }
        }
        for variable in &body.variables {
            if let Some(init) = &variable.initial_value {
                self.expression(init);
            }
        }
        for constant in &body.constants {
            self.expression(&constant.value);
        }
        for instance in &body.instances {
            self.instance(instance);
        }
        for block in &body.event_blocks {
            self.event_block(block);
        }
        for assignment in &body.assignments {
            self.assignment(assignment);
        }
        self.statements(&body.generate_stmts);
    }

    fn lvalue(&mut self, lvalue: &HirLValue) {
        match lvalue {
            HirLValue::Signal(_) | HirLValue::Variable(_) | HirLValue::Port(_) => {}
            HirLValue::Index(base, index) => {
                self.lvalue(base);
                self.expression(index);
            }
            HirLValue::Range(base, high, low) => {
                self.lvalue(base);
                self.expression(high);
                self.expression(low);
            }
            HirLValue::FieldAccess { base, .. } => self.lvalue(base),
        }
    }

    fn statements(&mut self, statements: &[HirStatement]) {
        for statement in statements {
            self.statement(statement);
        }
    }

    fn statement(&mut self, statement: &HirStatement) {
        match statement {
            HirStatement::Assignment(assignment) => self.assignment(assignment),
            HirStatement::If(if_stmt) => {
                self.expression(&if_stmt.condition);
                self.statements(&if_stmt.then_statements);
                if let Some(else_statements) = &if_stmt.else_statements {
                    self.statements(else_statements);
                }
            }
            HirStatement::Match(match_stmt) => {
                self.expression(&match_stmt.expr);
                for arm in &match_stmt.arms {
                    if let Some(guard) = &arm.guard {
                        self.expression(guard);
                    }
                    self.statements(&arm.statements);
                }
            }
            HirStatement::Flow(flow) => {
                for stage in std::iter::once(&flow.pipeline.start).chain(&flow.pipeline.stages) {
                    match stage {
                        HirPipelineStage::Expression(expr) => self.expression(expr),
                        HirPipelineStage::Block(statements) => self.statements(statements),
                    }
                }
            }
            HirStatement::Block(statements) => self.statements(statements),
            HirStatement::Assert(assert) => self.expression(&assert.condition),
            HirStatement::Assume(assume) => self.expression(&assume.condition),
            HirStatement::Cover(cover) => {
                if let HirProperty::Expression(expr) = &cover.property {
                    self.expression(expr);
                }
            }
            HirStatement::Property(_) | HirStatement::Barrier(_) => {}
            HirStatement::Let(let_stmt) => self.expression(&let_stmt.value),
            HirStatement::Return(value) => {
                if let Some(expr) = value {
                    self.expression(expr);
                }
            }
            HirStatement::Expression(expr) => self.expression(expr),
            HirStatement::For(for_stmt) => {
                self.expression(&for_stmt.range.start);
                self.expression(&for_stmt.range.end);
                if let Some(step) = &for_stmt.range.step {
                    self.expression(step);
                }
                self.statements(&for_stmt.body);
            }
            HirStatement::While(while_stmt) => {
                self.expression(&while_stmt.condition);
                self.statements(&while_stmt.body);
            }
            HirStatement::GenerateFor(generate) => {
                self.expression(&generate.range.start);
                self.expression(&generate.range.end);
                if let Some(step) = &generate.range.step {
                    self.expression(step);
                }
                self.generate_body(&generate.body);
            }
            HirStatement::GenerateIf(generate) => {
                self.expression(&generate.condition);
                self.generate_body(&generate.then_body);
                if let Some(else_body) = &generate.else_body {
                    self.generate_body(else_body);
                }
            }
            HirStatement::GenerateMatch(generate) => {
                self.expression(&generate.expr);
                for arm in &generate.arms {
                    self.generate_body(&arm.body);
                }
            }
        }
    }

    fn expression(&mut self, expr: &HirExpression) {
        match expr {
            HirExpression::Literal(_)
            | HirExpression::Signal(_)
            | HirExpression::Port(_)
            | HirExpression::Variable(_)
            | HirExpression::Constant(_)
            | HirExpression::GenericParam(_)
            | HirExpression::EnumVariant { .. }
            | HirExpression::AssociatedConstant { .. } => {}
            HirExpression::Binary(binary) => {
                self.expression(&binary.left);
                self.expression(&binary.right);
            }
            HirExpression::Unary(unary) => self.expression(&unary.operand),
            HirExpression::Call(call) => {
                self.names.insert(call.function.clone());
                for arg in &call.args {
                    self.expression(arg);
                }
            }
            HirExpression::Index(base, index) => {
                self.expression(base);
                self.expression(index);
            }
            HirExpression::Range(base, high, low) => {
                self.expression(base);
                self.expression(high);
                self.expression(low);
            }
            HirExpression::FieldAccess { base, .. } => self.expression(base),
            HirExpression::ArrayRepeat { value, count } => {
                self.expression(value);
                self.expression(count);
            }
            HirExpression::Concat(elements)
            | HirExpression::TupleLiteral(elements)
            | HirExpression::ArrayLiteral(elements) => {
                for element in elements {
                    self.expression(element);
                }
            }
            HirExpression::Ternary {
                condition,
                true_expr,
                false_expr,
            } => {
                self.expression(condition);
                self.expression(true_expr);
                self.expression(false_expr);
            }
            HirExpression::StructLiteral(literal) => {
                for arg in &literal.generic_args {
                    self.expression(arg);
                }
                for field in &literal.fields {
                    self.expression(&field.value);
                }
            }
            HirExpression::If(if_expr) => {
                self.expression(&if_expr.condition);
                self.expression(&if_expr.then_expr);
                self.expression(&if_expr.else_expr);
            }
            HirExpression::Match(match_expr) => {
                self.expression(&match_expr.expr);
                for arm in &match_expr.arms {
                    if let Some(guard) = &arm.guard {
                        self.expression(guard);
                    }
                    self.expression(&arm.expr);
                }
            }
            HirExpression::Cast(cast) => self.expression(&cast.expr),
            HirExpression::Block {
                statements,
                result_expr,
            } => {
                self.statements(statements);
                self.expression(result_expr);
            }
        }
    }
}
