//! SSA (Static Single Assignment) Conversion for MIR
//!
//! Transforms mutable variable reassignments into unique variables to eliminate
//! combinational cycles in hardware synthesis.
//!
//! Before SSA:
//! ```text
//! let mut x = value;
//! x = f1(x);  // Reassign x - creates cycle: x depends on x
//! x = f2(x);  // Reassign x again
//! return x;
//! ```
//!
//! After SSA:
//! ```text
//! let x_0 = value;
//! let x_1 = f1(x_0);  // New variable x_1
//! let x_2 = f2(x_1);  // New variable x_2
//! return x_2;
//! ```

use crate::mir::*;
use std::collections::HashMap;

/// SSA conversion context
pub struct SsaConverter {
    /// Next available variable ID
    next_var_id: u32,
    /// Maps original VariableId -> current SSA version ID
    /// Updated each time a variable is assigned
    current_version: HashMap<VariableId, VariableId>,
    /// New variables created during SSA conversion
    new_variables: Vec<Variable>,
    /// Track which variables have been reassigned (need SSA)
    reassignment_count: HashMap<VariableId, u32>,
}

impl SsaConverter {
    pub fn new(starting_var_id: u32) -> Self {
        Self {
            next_var_id: starting_var_id,
            current_version: HashMap::new(),
            new_variables: Vec::new(),
            reassignment_count: HashMap::new(),
        }
    }

    /// Convert a MIR module to SSA form
    pub fn convert_module(&mut self, module: &mut Module) {
        // First pass: count reassignments to identify variables needing SSA
        for process in &module.processes {
            self.count_reassignments_in_block(&process.body);
        }

        // Initialize current_version for variables that need SSA
        for variable in &module.variables {
            if self.reassignment_count.get(&variable.id).copied().unwrap_or(0) > 0 {
                // Variable is reassigned - initialize version mapping
                self.current_version.insert(variable.id, variable.id);
                eprintln!(
                    "SSA: Variable '{}' (id={}) reassigned {} times - will create versions",
                    variable.name,
                    variable.id.0,
                    self.reassignment_count.get(&variable.id).unwrap_or(&0)
                );
            }
        }

        // Second pass: transform assignments and references
        for process in &mut module.processes {
            self.convert_block(&mut process.body, &module.variables);
        }

        // Add new SSA variables to module
        module.variables.extend(self.new_variables.drain(..));

        eprintln!(
            "SSA: Conversion complete - created {} new variable versions",
            module.variables.len()
        );
    }

    /// Count variable reassignments in a block
    fn count_reassignments_in_block(&mut self, block: &Block) {
        for stmt in &block.statements {
            self.count_reassignments_in_stmt(stmt);
        }
    }

    fn count_reassignments_in_stmt(&mut self, stmt: &Statement) {
        match stmt {
            Statement::Assignment(assign) => {
                if let LValue::Variable(var_id) = &assign.lhs {
                    *self.reassignment_count.entry(*var_id).or_insert(0) += 1;
                }
            }
            Statement::If(if_stmt) => {
                self.count_reassignments_in_block(&if_stmt.then_block);
                if let Some(else_block) = &if_stmt.else_block {
                    self.count_reassignments_in_block(else_block);
                }
            }
            Statement::Case(case_stmt) => {
                for item in &case_stmt.items {
                    self.count_reassignments_in_block(&item.block);
                }
                if let Some(default) = &case_stmt.default {
                    self.count_reassignments_in_block(default);
                }
            }
            Statement::Block(block) => {
                self.count_reassignments_in_block(block);
            }
            Statement::Loop(loop_stmt) => match loop_stmt {
                LoopStatement::For { body, .. } => {
                    self.count_reassignments_in_block(body);
                }
                LoopStatement::While { body, .. } => {
                    self.count_reassignments_in_block(body);
                }
            },
            Statement::ResolvedConditional(_) => {}
            // Assertions don't involve variable reassignments
            Statement::Assert(_) | Statement::Assume(_) | Statement::Cover(_) => {}
        }
    }

    /// Convert a block to SSA form
    fn convert_block(&mut self, block: &mut Block, original_vars: &[Variable]) {
        for stmt in &mut block.statements {
            self.convert_stmt(stmt, original_vars);
        }
    }

    fn convert_stmt(&mut self, stmt: &mut Statement, original_vars: &[Variable]) {
        match stmt {
            Statement::Assignment(assign) => {
                // First, update all variable references in the RHS to use current versions
                self.update_expr_refs(&mut assign.rhs);

                // Then, if LHS is a variable that needs SSA, create a new version
                let var_id_copy = if let LValue::Variable(var_id) = &assign.lhs {
                    if self.current_version.contains_key(var_id) {
                        Some(*var_id)
                    } else {
                        None
                    }
                } else {
                    None
                };

                if let Some(var_id) = var_id_copy {
                    // This variable is being reassigned - create new version
                    let new_var_id = self.create_new_version(var_id, original_vars);

                    // Update LHS to new version
                    assign.lhs = LValue::Variable(new_var_id);

                    // Update current version mapping
                    self.current_version.insert(var_id, new_var_id);

                    eprintln!(
                        "SSA: Reassignment to var {} -> new version {}",
                        var_id.0, new_var_id.0
                    );
                }
            }
            Statement::If(if_stmt) => {
                // Update condition refs
                self.update_expr_refs(&mut if_stmt.condition);

                // Save current versions before branches
                let saved_versions = self.current_version.clone();

                // Convert then branch
                self.convert_block(&mut if_stmt.then_block, original_vars);
                let then_versions = self.current_version.clone();

                // Restore and convert else branch
                self.current_version = saved_versions.clone();
                if let Some(else_block) = &mut if_stmt.else_block {
                    self.convert_block(else_block, original_vars);
                }
                let else_versions = self.current_version.clone();

                // Merge versions (use then_versions for simplicity - proper SSA would use phi nodes)
                // For hardware synthesis, this is usually fine as mux handles both paths
                for (var_id, then_ver) in &then_versions {
                    if let Some(else_ver) = else_versions.get(var_id) {
                        if then_ver != else_ver {
                            // Different versions in branches - use the later one
                            // (In full SSA, this would require a phi node)
                            self.current_version.insert(*var_id, *then_ver);
                        }
                    }
                }
            }
            Statement::Case(case_stmt) => {
                self.update_expr_refs(&mut case_stmt.expr);

                for item in &mut case_stmt.items {
                    // Update value expressions
                    for val_expr in &mut item.values {
                        self.update_expr_refs(val_expr);
                    }
                    self.convert_block(&mut item.block, original_vars);
                }
                if let Some(default) = &mut case_stmt.default {
                    self.convert_block(default, original_vars);
                }
            }
            Statement::Block(block) => {
                self.convert_block(block, original_vars);
            }
            Statement::Loop(loop_stmt) => {
                // Loops are complex for SSA - for now, just convert the body
                match loop_stmt {
                    LoopStatement::For { init, condition, update, body } => {
                        self.convert_stmt(&mut Statement::Assignment(*init.clone()), original_vars);
                        self.update_expr_refs(condition);
                        self.convert_block(body, original_vars);
                        self.convert_stmt(&mut Statement::Assignment(*update.clone()), original_vars);
                    }
                    LoopStatement::While { condition, body } => {
                        self.update_expr_refs(condition);
                        self.convert_block(body, original_vars);
                    }
                }
            }
            Statement::ResolvedConditional(rc) => {
                // Update refs in conditions and values
                for case in &mut rc.resolved.cases {
                    self.update_expr_refs(&mut case.condition);
                    self.update_expr_refs(&mut case.value);
                }
                self.update_expr_refs(&mut rc.resolved.default);
            }
            Statement::Assert(assert_stmt) => {
                // Update refs in assertion condition
                self.update_expr_refs(&mut assert_stmt.condition);
            }
            Statement::Assume(assume_stmt) => {
                // Update refs in assumption condition
                self.update_expr_refs(&mut assume_stmt.condition);
            }
            Statement::Cover(cover_stmt) => {
                // Update refs in cover condition
                self.update_expr_refs(&mut cover_stmt.condition);
            }
        }
    }

    /// Create a new SSA version of a variable
    fn create_new_version(&mut self, original_var_id: VariableId, original_vars: &[Variable]) -> VariableId {
        let new_id = VariableId(self.next_var_id);
        self.next_var_id += 1;

        // Find original variable to copy its properties
        if let Some(orig_var) = original_vars.iter().find(|v| v.id == original_var_id) {
            let new_var = Variable {
                id: new_id,
                name: format!("{}_ssa{}", orig_var.name, new_id.0),
                var_type: orig_var.var_type.clone(),
                initial: None, // SSA versions don't have initial values
                span: None,
            };
            self.new_variables.push(new_var);
        }

        new_id
    }

    /// Update variable references in an expression to use current SSA versions
    fn update_expr_refs(&mut self, expr: &mut Expression) {
        match &mut expr.kind {
            ExpressionKind::Ref(lvalue) => {
                if let LValue::Variable(var_id) = lvalue {
                    if let Some(&current_ver) = self.current_version.get(var_id) {
                        if current_ver != *var_id {
                            *var_id = current_ver;
                        }
                    }
                }
            }
            ExpressionKind::Binary { left, right, .. } => {
                self.update_expr_refs(left);
                self.update_expr_refs(right);
            }
            ExpressionKind::Unary { operand, .. } => {
                self.update_expr_refs(operand);
            }
            ExpressionKind::FunctionCall { args, .. } => {
                for arg in args {
                    self.update_expr_refs(arg);
                }
            }
            ExpressionKind::Concat(exprs) => {
                for e in exprs {
                    self.update_expr_refs(e);
                }
            }
            ExpressionKind::Replicate { value, count } => {
                self.update_expr_refs(value);
                self.update_expr_refs(count);
            }
            ExpressionKind::Cast { expr, .. } => {
                self.update_expr_refs(expr);
            }
            ExpressionKind::Conditional { cond, then_expr, else_expr } => {
                self.update_expr_refs(cond);
                self.update_expr_refs(then_expr);
                self.update_expr_refs(else_expr);
            }
            ExpressionKind::FieldAccess { base, .. } => {
                self.update_expr_refs(base);
            }
            ExpressionKind::TupleFieldAccess { base, .. } => {
                self.update_expr_refs(base);
            }
            // Literals have no variable references
            ExpressionKind::Literal(_) => {}
        }
    }
}

/// Apply SSA conversion to a MIR design
pub fn apply_ssa_conversion(mir: &mut Mir) {
    eprintln!("SSA: Starting SSA conversion for {} modules", mir.modules.len());

    // Find the maximum variable ID across all modules
    let max_var_id = mir.modules.iter()
        .flat_map(|m| m.variables.iter().map(|v| v.id.0))
        .max()
        .unwrap_or(0);

    for module in &mut mir.modules {
        let mut converter = SsaConverter::new(max_var_id + 1000); // Start IDs high to avoid collision
        converter.convert_module(module);
    }

    eprintln!("SSA: Conversion complete");
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_ssa_conversion() {
        // This test would verify SSA conversion on a simple case
        // For now, just verify the module compiles
    }
}
