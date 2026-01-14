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
use indexmap::IndexMap;

/// SSA conversion context
pub struct SsaConverter {
    /// Next available variable ID
    next_var_id: u32,
    /// Maps original VariableId -> current SSA version ID
    /// Updated each time a variable is assigned
    current_version: IndexMap<VariableId, VariableId>,
    /// New variables created during SSA conversion
    new_variables: Vec<Variable>,
    /// Track which variables have been reassigned (need SSA)
    reassignment_count: IndexMap<VariableId, u32>,
}

impl SsaConverter {
    pub fn new(starting_var_id: u32) -> Self {
        Self {
            next_var_id: starting_var_id,
            current_version: IndexMap::new(),
            new_variables: Vec::new(),
            reassignment_count: IndexMap::new(),
        }
    }

    /// Convert a MIR module to SSA form
    pub fn convert_module(&mut self, module: &mut Module) {
        println!(
            "SSA: Module '{}' has {} variables, {} assignments, {} processes",
            module.name,
            module.variables.len(),
            module.assignments.len(),
            module.processes.len()
        );

        // First pass: count reassignments to identify variables needing SSA
        // Count in process blocks
        for process in &module.processes {
            self.count_reassignments_in_block(&process.body);
        }

        // Also count in continuous assignments (for async entities)
        for assign in &module.assignments {
            if let LValue::Variable(var_id) = &assign.lhs {
                println!("SSA: Found continuous assign to var_{}", var_id.0);
                *self.reassignment_count.entry(*var_id).or_insert(0) += 1;
            }
        }

        // Initialize current_version for variables that need SSA
        // Note: count > 1 means multiple assignments (initial + reassignments)
        println!(
            "SSA: Checking {} variables for SSA needs",
            module.variables.len()
        );
        for variable in &module.variables {
            let count = self
                .reassignment_count
                .get(&variable.id)
                .copied()
                .unwrap_or(0);
            println!(
                "SSA:   Variable '{}' (id={}) has {} assignments",
                variable.name, variable.id.0, count
            );
            // Variables assigned more than once need SSA
            if count > 1 {
                // Variable is reassigned - initialize version mapping
                self.current_version.insert(variable.id, variable.id);
                println!(
                    "SSA: Variable '{}' (id={}) assigned {} times - will create versions",
                    variable.name, variable.id.0, count
                );
            }
        }

        // Second pass: transform assignments and references
        // Transform process blocks
        for process in &mut module.processes {
            self.convert_block(&mut process.body, &module.variables);
        }

        // Transform continuous assignments (for async entities)
        println!(
            "SSA: Converting {} continuous assignments",
            module.assignments.len()
        );
        self.convert_continuous_assignments(&mut module.assignments, &module.variables);

        // Add new SSA variables to module
        module.variables.append(&mut self.new_variables);

        eprintln!(
            "SSA: Conversion complete - created {} new variable versions",
            module.variables.len()
        );
    }

    /// Convert continuous assignments to SSA form
    fn convert_continuous_assignments(
        &mut self,
        assignments: &mut [ContinuousAssign],
        variables: &[Variable],
    ) {
        // Track first assignment to each variable (first assignment keeps original name)
        let mut first_assignment: IndexMap<VariableId, bool> = IndexMap::new();

        for assign in assignments.iter_mut() {
            // First update RHS references to use current versions
            self.update_expr_refs(&mut assign.rhs);

            // Extract var_id if LHS is a variable, and check if it needs SSA
            let var_id_opt = if let LValue::Variable(var_id) = &assign.lhs {
                if self.current_version.contains_key(var_id) {
                    Some(*var_id)
                } else {
                    None
                }
            } else {
                None
            };

            if let Some(var_id) = var_id_opt {
                let is_first = !first_assignment.contains_key(&var_id);
                println!(
                    "SSA:   Processing assign to var_{}, is_first={}",
                    var_id.0, is_first
                );

                if is_first {
                    // First assignment - keep original variable
                    println!(
                        "SSA:   First assignment to var_{}, keeping original",
                        var_id.0
                    );
                    first_assignment.insert(var_id, true);
                    // Update current version to be the original variable
                    self.current_version.insert(var_id, var_id);
                } else {
                    // Create a new SSA version
                    let new_id = VariableId(self.next_var_id);
                    self.next_var_id += 1;

                    // Find the original variable name
                    let original_name = variables
                        .iter()
                        .find(|v| v.id == var_id)
                        .map(|v| v.name.clone())
                        .unwrap_or_else(|| format!("var_{}", var_id.0));

                    // Create a new variable with SSA-versioned name
                    let version_num = self.next_var_id - 1;
                    let original_var = variables.iter().find(|v| v.id == var_id);
                    let new_var = Variable {
                        id: new_id,
                        name: format!("{}_ssa_{}", original_name, version_num),
                        var_type: original_var
                            .map(|v| v.var_type.clone())
                            .unwrap_or_else(|| DataType::Bit(1)),
                        initial: None, // SSA versions don't have initial values
                        span: original_var.and_then(|v| v.span.clone()),
                    };

                    println!(
                        "SSA:   Creating new version {} for {} (id={})",
                        new_var.name, original_name, var_id.0
                    );

                    self.new_variables.push(new_var);

                    // Update LHS to new variable
                    assign.lhs = LValue::Variable(new_id);

                    // Update current version for subsequent references
                    self.current_version.insert(var_id, new_id);
                }
            }
        }
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
                    LoopStatement::For {
                        init,
                        condition,
                        update,
                        body,
                    } => {
                        self.convert_stmt(&mut Statement::Assignment(*init.clone()), original_vars);
                        self.update_expr_refs(condition);
                        self.convert_block(body, original_vars);
                        self.convert_stmt(
                            &mut Statement::Assignment(*update.clone()),
                            original_vars,
                        );
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
    fn create_new_version(
        &mut self,
        original_var_id: VariableId,
        original_vars: &[Variable],
    ) -> VariableId {
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

    /// Update variable references in an LValue to use current SSA versions
    /// BUG #182 FIX: Handle nested LValues like RangeSelect and BitSelect
    fn update_lvalue_refs(&mut self, lvalue: &mut LValue) {
        match lvalue {
            LValue::Variable(var_id) => {
                if let Some(&current_ver) = self.current_version.get(var_id) {
                    if current_ver != *var_id {
                        eprintln!(
                            "SSA: BUG #182 FIX - Updating Variable({}) -> Variable({}) in LValue",
                            var_id.0, current_ver.0
                        );
                        *var_id = current_ver;
                    }
                }
            }
            LValue::RangeSelect { base, .. } => {
                // Recursively update the base LValue
                self.update_lvalue_refs(base);
            }
            LValue::BitSelect { base, index } => {
                // Recursively update the base LValue
                self.update_lvalue_refs(base);
                // Also update any variable refs in the index expression
                self.update_expr_refs(index);
            }
            LValue::Concat(parts) => {
                for part in parts {
                    self.update_lvalue_refs(part);
                }
            }
            // Port and Signal references don't have nested variables
            LValue::Port(_) | LValue::Signal(_) => {}
        }
    }

    /// Update variable references in an expression to use current SSA versions
    fn update_expr_refs(&mut self, expr: &mut Expression) {
        match &mut expr.kind {
            ExpressionKind::Ref(lvalue) => {
                // BUG #182 FIX: Use helper to handle nested LValues
                self.update_lvalue_refs(lvalue);
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
            ExpressionKind::Conditional {
                cond,
                then_expr,
                else_expr,
            } => {
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
    eprintln!(
        "SSA: Starting SSA conversion for {} modules",
        mir.modules.len()
    );

    // Find the maximum variable ID across all modules
    let max_var_id = mir
        .modules
        .iter()
        .flat_map(|m| m.variables.iter().map(|v| v.id.0))
        .max()
        .unwrap_or(0);

    println!("SSA: Found {} modules to process", mir.modules.len());
    for module in &mut mir.modules {
        let mut converter = SsaConverter::new(max_var_id + 1000); // Start IDs high to avoid collision
        converter.convert_module(module);
    }

    println!("SSA: Conversion complete");
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
