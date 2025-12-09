//! Verilog code generation utilities
//!
//! This module provides two code generation paths:
//! 1. LIR-based: Simple gate-level Verilog for basic designs
//! 2. MIR-based: Full SystemVerilog with generate block support for #[preserve_generate]

use crate::BackendError;
use skalp_lir::LirModule;
use skalp_mir::{
    Expression, ExpressionKind, GenerateBlock, GenerateBlockKind, GenerateBody, GenerateCase,
    GenerateCaseArm, GenerateFor, GenerateIf, Mir, Module, Statement, LValue, BinaryOp, UnaryOp,
    ReduceOp, CaseStatement,
};
use skalp_mir::mir::CaseItem;

/// Generate Verilog from LIR (gate-level)
pub fn generate_verilog(module: &LirModule) -> Result<String, BackendError> {
    let mut verilog = String::new();

    // Module declaration
    verilog.push_str(&format!("module {} (\n", module.name));

    // Add ports (simplified)
    for (i, signal) in module.signals.iter().enumerate() {
        if i > 0 {
            verilog.push_str(",\n");
        }
        let direction = if signal.is_input { "input" } else { "output" };
        verilog.push_str(&format!("  {} {}", direction, signal.name));
    }

    verilog.push_str("\n);\n\n");

    // Add signal declarations
    for signal in &module.signals {
        if signal.is_register {
            verilog.push_str(&format!("  reg {};\n", signal.name));
        }
    }

    // Add gate instantiations (simplified)
    for gate in &module.gates {
        verilog.push_str(&format!(
            "  {} {} (.in({}), .out({}));\n",
            gate.gate_type.to_string().to_lowercase(),
            gate.id,
            gate.inputs.join(", "),
            gate.outputs.join(", ")
        ));
    }

    verilog.push_str("\nendmodule\n");

    Ok(verilog)
}

// ============================================================================
// MIR-based SystemVerilog Generation with Generate Block Support
// ============================================================================

/// SystemVerilog code generator for MIR with generate block support
pub struct SvCodegen {
    output: String,
    indent: usize,
}

impl SvCodegen {
    /// Create a new SystemVerilog code generator
    pub fn new() -> Self {
        Self {
            output: String::new(),
            indent: 0,
        }
    }

    /// Generate SystemVerilog from MIR
    pub fn generate_from_mir(mir: &Mir) -> Result<String, BackendError> {
        let mut codegen = Self::new();
        for module in &mir.modules {
            codegen.emit_module(module)?;
        }
        Ok(codegen.output)
    }

    fn emit(&mut self, s: &str) {
        self.output.push_str(&"  ".repeat(self.indent));
        self.output.push_str(s);
    }

    fn emit_line(&mut self, s: &str) {
        self.emit(s);
        self.output.push('\n');
    }

    fn emit_module(&mut self, module: &Module) -> Result<(), BackendError> {
        // Module header
        self.emit_line(&format!("module {} (", module.name));
        self.indent += 1;

        // Emit ports
        let port_count = module.ports.len();
        for (i, port) in module.ports.iter().enumerate() {
            let direction = match port.direction {
                skalp_mir::PortDirection::Input => "input",
                skalp_mir::PortDirection::Output => "output",
                skalp_mir::PortDirection::InOut => "inout",
            };
            let comma = if i < port_count - 1 { "," } else { "" };
            self.emit_line(&format!("{} logic {}{}", direction, port.name, comma));
        }

        self.indent -= 1;
        self.emit_line(");");
        self.output.push('\n');
        self.indent += 1;

        // Emit generate blocks if present
        for gen_block in &module.generate_blocks {
            self.emit_generate_block(gen_block)?;
        }

        self.indent -= 1;
        self.emit_line("endmodule");
        self.output.push('\n');

        Ok(())
    }

    /// Emit a generate block as SystemVerilog
    fn emit_generate_block(&mut self, block: &GenerateBlock) -> Result<(), BackendError> {
        // Emit optional label
        if let Some(label) = &block.label {
            self.emit_line(&format!("// Generate block: {}", label));
        }

        self.emit_line("generate");
        self.indent += 1;

        match &block.kind {
            GenerateBlockKind::For(gen_for) => self.emit_generate_for(gen_for, &block.label)?,
            GenerateBlockKind::If(gen_if) => self.emit_generate_if(gen_if, &block.label)?,
            GenerateBlockKind::Case(gen_case) => self.emit_generate_case(gen_case, &block.label)?,
        }

        self.indent -= 1;
        self.emit_line("endgenerate");
        self.output.push('\n');

        Ok(())
    }

    /// Emit a generate-for loop
    fn emit_generate_for(
        &mut self,
        gen_for: &GenerateFor,
        label: &Option<String>,
    ) -> Result<(), BackendError> {
        let start = self.expr_to_sv(&gen_for.start);
        let end = self.expr_to_sv(&gen_for.end);
        let step = gen_for.step.to_string();

        // Emit genvar declaration
        self.emit_line(&format!("genvar {};", gen_for.genvar));

        // Emit for loop with optional label
        let label_prefix = label
            .as_ref()
            .map(|l| format!("{}: ", l))
            .unwrap_or_default();

        self.emit_line(&format!(
            "{}for ({} = {}; {} < {}; {} = {} + {}) begin",
            label_prefix,
            gen_for.genvar,
            start,
            gen_for.genvar,
            end,
            gen_for.genvar,
            gen_for.genvar,
            step
        ));

        self.indent += 1;
        self.emit_generate_body(&gen_for.body)?;
        self.indent -= 1;

        self.emit_line("end");

        Ok(())
    }

    /// Emit a generate-if conditional
    fn emit_generate_if(
        &mut self,
        gen_if: &GenerateIf,
        label: &Option<String>,
    ) -> Result<(), BackendError> {
        let condition = self.expr_to_sv(&gen_if.condition);

        let label_prefix = label
            .as_ref()
            .map(|l| format!("{}: ", l))
            .unwrap_or_default();

        self.emit_line(&format!("{}if ({}) begin", label_prefix, condition));
        self.indent += 1;
        self.emit_generate_body(&gen_if.then_body)?;
        self.indent -= 1;

        if let Some(else_body) = &gen_if.else_body {
            self.emit_line("end else begin");
            self.indent += 1;
            self.emit_generate_body(else_body)?;
            self.indent -= 1;
        }

        self.emit_line("end");

        Ok(())
    }

    /// Emit a generate-case (from match)
    fn emit_generate_case(
        &mut self,
        gen_case: &GenerateCase,
        label: &Option<String>,
    ) -> Result<(), BackendError> {
        let selector = self.expr_to_sv(&gen_case.selector);

        let label_prefix = label
            .as_ref()
            .map(|l| format!("{}: ", l))
            .unwrap_or_default();

        self.emit_line(&format!("{}case ({})", label_prefix, selector));
        self.indent += 1;

        for arm in &gen_case.arms {
            self.emit_generate_case_arm(arm)?;
        }

        if let Some(default) = &gen_case.default {
            self.emit_line("default: begin");
            self.indent += 1;
            self.emit_generate_body(default)?;
            self.indent -= 1;
            self.emit_line("end");
        }

        self.indent -= 1;
        self.emit_line("endcase");

        Ok(())
    }

    /// Emit a single case arm
    fn emit_generate_case_arm(&mut self, arm: &GenerateCaseArm) -> Result<(), BackendError> {
        let patterns: Vec<String> = arm.patterns.iter().map(|p| self.expr_to_sv(p)).collect();
        let pattern_str = patterns.join(", ");

        self.emit_line(&format!("{}: begin", pattern_str));
        self.indent += 1;
        self.emit_generate_body(&arm.body)?;
        self.indent -= 1;
        self.emit_line("end");

        Ok(())
    }

    /// Emit generate body (signals, processes, assignments, instances, nested generates)
    fn emit_generate_body(&mut self, body: &GenerateBody) -> Result<(), BackendError> {
        // Emit signal declarations
        for signal in &body.signals {
            let width = match &signal.signal_type {
                skalp_mir::DataType::Bit(w) => *w,
                skalp_mir::DataType::Logic(w) => *w,
                skalp_mir::DataType::Nat(w) => *w,
                skalp_mir::DataType::Int(w) => *w,
                _ => 1,
            };
            if width > 1 {
                self.emit_line(&format!("logic [{}:0] {};", width - 1, signal.name));
            } else {
                self.emit_line(&format!("logic {};", signal.name));
            }
        }

        // Emit continuous assignments
        for assign in &body.assignments {
            let lhs = self.lvalue_to_sv(&assign.lhs);
            let rhs = self.expr_to_sv(&assign.rhs);
            self.emit_line(&format!("assign {} = {};", lhs, rhs));
        }

        // Emit processes (always blocks)
        for process in &body.processes {
            self.emit_process(process)?;
        }

        // Emit module instances
        for instance in &body.instances {
            self.emit_instance(instance)?;
        }

        // Emit nested generate blocks
        for nested in &body.nested_generates {
            self.emit_generate_block(nested)?;
        }

        Ok(())
    }

    /// Emit a process (always block)
    fn emit_process(&mut self, process: &skalp_mir::Process) -> Result<(), BackendError> {
        // Determine sensitivity list
        let sens = match &process.sensitivity {
            skalp_mir::SensitivityList::Always => "*".to_string(),
            skalp_mir::SensitivityList::Edge(edges) => {
                edges
                    .iter()
                    .map(|e| {
                        let edge = match e.edge {
                            skalp_mir::EdgeType::Rising => "posedge",
                            skalp_mir::EdgeType::Falling => "negedge",
                            skalp_mir::EdgeType::Both => "",
                            skalp_mir::EdgeType::Active => "posedge",  // Active level
                            skalp_mir::EdgeType::Inactive => "negedge", // Inactive level
                        };
                        format!("{} {}", edge, self.lvalue_to_sv(&e.signal))
                    })
                    .collect::<Vec<_>>()
                    .join(" or ")
            }
            skalp_mir::SensitivityList::Level(signals) => {
                signals
                    .iter()
                    .map(|s| self.lvalue_to_sv(s))
                    .collect::<Vec<_>>()
                    .join(", ")
            }
        };

        self.emit_line(&format!("always @({}) begin", sens));
        self.indent += 1;
        for stmt in &process.body.statements {
            self.emit_statement(stmt)?;
        }
        self.indent -= 1;
        self.emit_line("end");

        Ok(())
    }

    /// Emit a module instance
    fn emit_instance(&mut self, instance: &skalp_mir::ModuleInstance) -> Result<(), BackendError> {
        // Note: ModuleInstance.module is a ModuleId, we'd need module lookup for name
        // For now, just use instance name as placeholder
        self.emit(&format!("module_{} {} (", instance.module.0, instance.name));

        let connections: Vec<String> = instance
            .connections
            .iter()
            .map(|(port, expr)| format!(".{}({})", port, self.expr_to_sv(expr)))
            .collect();

        self.output.push_str(&connections.join(", "));
        self.output.push_str(");\n");

        Ok(())
    }

    /// Emit a statement
    fn emit_statement(&mut self, stmt: &Statement) -> Result<(), BackendError> {
        match stmt {
            Statement::Assignment(assign) => {
                let lhs = self.lvalue_to_sv(&assign.lhs);
                let rhs = self.expr_to_sv(&assign.rhs);
                match assign.kind {
                    skalp_mir::AssignmentKind::Blocking => {
                        self.emit_line(&format!("{} = {};", lhs, rhs));
                    }
                    skalp_mir::AssignmentKind::NonBlocking => {
                        self.emit_line(&format!("{} <= {};", lhs, rhs));
                    }
                }
            }
            Statement::If(if_stmt) => {
                let cond = self.expr_to_sv(&if_stmt.condition);
                self.emit_line(&format!("if ({}) begin", cond));
                self.indent += 1;
                for s in &if_stmt.then_block.statements {
                    self.emit_statement(s)?;
                }
                self.indent -= 1;
                if let Some(else_block) = &if_stmt.else_block {
                    self.emit_line("end else begin");
                    self.indent += 1;
                    for s in &else_block.statements {
                        self.emit_statement(s)?;
                    }
                    self.indent -= 1;
                }
                self.emit_line("end");
            }
            Statement::Case(case_stmt) => {
                let sel = self.expr_to_sv(&case_stmt.expr);
                self.emit_line(&format!("case ({})", sel));
                self.indent += 1;
                for case_item in &case_stmt.items {
                    self.emit_case_item(case_item)?;
                }
                if let Some(default_block) = &case_stmt.default {
                    self.emit_line("default: begin");
                    self.indent += 1;
                    for s in &default_block.statements {
                        self.emit_statement(s)?;
                    }
                    self.indent -= 1;
                    self.emit_line("end");
                }
                self.indent -= 1;
                self.emit_line("endcase");
            }
            Statement::Block(block) => {
                self.emit_line("begin");
                self.indent += 1;
                for s in &block.statements {
                    self.emit_statement(s)?;
                }
                self.indent -= 1;
                self.emit_line("end");
            }
            Statement::Loop(_) => {
                // Loop statements are typically unrolled in synthesis
                self.emit_line("// Loop statement (unrolled)");
            }
            Statement::ResolvedConditional(resolved_cond) => {
                // Emit the original if-else-if form for readability
                // The 'resolved' form is used by lower-level synthesis passes
                let cond = self.expr_to_sv(&resolved_cond.original.condition);
                self.emit_line(&format!("if ({}) begin", cond));
                self.indent += 1;
                for s in &resolved_cond.original.then_block.statements {
                    self.emit_statement(s)?;
                }
                self.indent -= 1;
                if let Some(else_block) = &resolved_cond.original.else_block {
                    self.emit_line("end else begin");
                    self.indent += 1;
                    for s in &else_block.statements {
                        self.emit_statement(s)?;
                    }
                    self.indent -= 1;
                }
                self.emit_line("end");
            }
            // SVA: SystemVerilog Assertions
            Statement::Assert(assert_stmt) => {
                let cond = self.expr_to_sv(&assert_stmt.condition);
                let severity = match assert_stmt.severity {
                    skalp_mir::mir::AssertionSeverity::Info => "$info",
                    skalp_mir::mir::AssertionSeverity::Warning => "$warning",
                    skalp_mir::mir::AssertionSeverity::Error => "$error",
                    skalp_mir::mir::AssertionSeverity::Fatal => "$fatal",
                };
                if let Some(msg) = &assert_stmt.message {
                    self.emit_line(&format!("assert({}) else {}(\"{}\");", cond, severity, msg));
                } else {
                    self.emit_line(&format!("assert({}) else {}(\"Assertion failed\");", cond, severity));
                }
            }
            Statement::Assume(assume_stmt) => {
                let cond = self.expr_to_sv(&assume_stmt.condition);
                if let Some(msg) = &assume_stmt.message {
                    self.emit_line(&format!("assume({}); // {}", cond, msg));
                } else {
                    self.emit_line(&format!("assume({});", cond));
                }
            }
            Statement::Cover(cover_stmt) => {
                let cond = self.expr_to_sv(&cover_stmt.condition);
                if let Some(lbl) = &cover_stmt.label {
                    self.emit_line(&format!("{}: cover({});", lbl, cond));
                } else {
                    self.emit_line(&format!("cover({});", cond));
                }
            }
        }
        Ok(())
    }

    /// Emit a case item
    fn emit_case_item(&mut self, item: &CaseItem) -> Result<(), BackendError> {
        let values: Vec<String> = item.values.iter().map(|v| self.expr_to_sv(v)).collect();
        let pattern = values.join(", ");
        self.emit_line(&format!("{}: begin", pattern));
        self.indent += 1;
        for s in &item.block.statements {
            self.emit_statement(s)?;
        }
        self.indent -= 1;
        self.emit_line("end");
        Ok(())
    }

    /// Convert an LValue to SystemVerilog
    fn lvalue_to_sv(&self, lvalue: &LValue) -> String {
        match lvalue {
            LValue::Signal(sig_id) => format!("signal_{}", sig_id.0),
            LValue::Variable(var_id) => format!("var_{}", var_id.0),
            LValue::Port(port_id) => format!("port_{}", port_id.0),
            LValue::BitSelect { base, index } => {
                format!("{}[{}]", self.lvalue_to_sv(base), self.expr_to_sv(index))
            }
            LValue::RangeSelect { base, high, low } => {
                format!(
                    "{}[{}:{}]",
                    self.lvalue_to_sv(base),
                    self.expr_to_sv(high),
                    self.expr_to_sv(low)
                )
            }
            LValue::Concat(parts) => {
                let parts_sv: Vec<String> = parts.iter().map(|p| self.lvalue_to_sv(p)).collect();
                format!("{{{}}}", parts_sv.join(", "))
            }
        }
    }

    /// Convert an Expression to SystemVerilog
    fn expr_to_sv(&self, expr: &Expression) -> String {
        match &expr.kind {
            ExpressionKind::Literal(value) => self.value_to_sv(value),
            ExpressionKind::Ref(lvalue) => self.lvalue_to_sv(lvalue),
            ExpressionKind::Binary { op, left, right } => {
                let left_sv = self.expr_to_sv(left);
                let right_sv = self.expr_to_sv(right);
                let op_sv = self.binop_to_sv(op);
                format!("({} {} {})", left_sv, op_sv, right_sv)
            }
            ExpressionKind::Unary { op, operand } => {
                let operand_sv = self.expr_to_sv(operand);
                let op_sv = self.unaryop_to_sv(op);
                format!("({}{})", op_sv, operand_sv)
            }
            ExpressionKind::Conditional {
                cond,
                then_expr,
                else_expr,
            } => {
                let cond_sv = self.expr_to_sv(cond);
                let then_sv = self.expr_to_sv(then_expr);
                let else_sv = self.expr_to_sv(else_expr);
                format!("({} ? {} : {})", cond_sv, then_sv, else_sv)
            }
            ExpressionKind::Concat(exprs) => {
                let parts: Vec<String> = exprs.iter().map(|e| self.expr_to_sv(e)).collect();
                format!("{{{}}}", parts.join(", "))
            }
            ExpressionKind::Replicate { count, value } => {
                let count_sv = self.expr_to_sv(count);
                let value_sv = self.expr_to_sv(value);
                format!("{{{}{{ {} }}}}", count_sv, value_sv)
            }
            ExpressionKind::FunctionCall { name, args } => {
                let args_sv: Vec<String> = args.iter().map(|a| self.expr_to_sv(a)).collect();
                format!("{}({})", name, args_sv.join(", "))
            }
            ExpressionKind::Cast { expr, .. } => self.expr_to_sv(expr),
            ExpressionKind::TupleFieldAccess { base, index } => {
                format!("{}[{}]", self.expr_to_sv(base), index)
            }
            ExpressionKind::FieldAccess { base, field } => {
                format!("{}.{}", self.expr_to_sv(base), field)
            }
        }
    }

    fn value_to_sv(&self, value: &skalp_mir::Value) -> String {
        match value {
            skalp_mir::Value::Integer(i) => format!("{}", i),
            skalp_mir::Value::Float(f) => format!("{}", f),
            skalp_mir::Value::BitVector { width, value } => {
                format!("{}'h{:X}", width, value)
            }
            skalp_mir::Value::String(s) => format!("\"{}\"", s),
            skalp_mir::Value::HighZ => "1'bz".to_string(),
            skalp_mir::Value::Unknown => "1'bx".to_string(),
        }
    }

    fn binop_to_sv(&self, op: &BinaryOp) -> &'static str {
        match op {
            BinaryOp::Add => "+",
            BinaryOp::Sub => "-",
            BinaryOp::Mul => "*",
            BinaryOp::Div => "/",
            BinaryOp::Mod => "%",
            BinaryOp::FAdd => "+",
            BinaryOp::FSub => "-",
            BinaryOp::FMul => "*",
            BinaryOp::FDiv => "/",
            BinaryOp::And => "&&",
            BinaryOp::Or => "||",
            BinaryOp::Xor => "^",
            BinaryOp::Equal => "==",
            BinaryOp::NotEqual => "!=",
            BinaryOp::Less => "<",
            BinaryOp::LessEqual => "<=",
            BinaryOp::Greater => ">",
            BinaryOp::GreaterEqual => ">=",
            BinaryOp::FEqual => "==",
            BinaryOp::FNotEqual => "!=",
            BinaryOp::FLess => "<",
            BinaryOp::FLessEqual => "<=",
            BinaryOp::FGreater => ">",
            BinaryOp::FGreaterEqual => ">=",
            BinaryOp::BitwiseAnd => "&",
            BinaryOp::BitwiseOr => "|",
            BinaryOp::BitwiseXor => "^",
            BinaryOp::LeftShift => "<<",
            BinaryOp::RightShift => ">>",
            BinaryOp::LogicalAnd => "&&",
            BinaryOp::LogicalOr => "||",
        }
    }

    fn unaryop_to_sv(&self, op: &UnaryOp) -> &'static str {
        match op {
            UnaryOp::Not => "!",
            UnaryOp::BitwiseNot => "~",
            UnaryOp::Negate => "-",
            UnaryOp::FSqrt => "$sqrt",  // Placeholder for floating-point sqrt
            UnaryOp::FNegate => "-",
            UnaryOp::Reduce(rop) => match rop {
                ReduceOp::And => "&",
                ReduceOp::Or => "|",
                ReduceOp::Xor => "^",
                ReduceOp::Nand => "~&",
                ReduceOp::Nor => "~|",
                ReduceOp::Xnor => "~^",
            },
        }
    }
}

/// Generate SystemVerilog from MIR (public API)
pub fn generate_sv_from_mir(mir: &Mir) -> Result<String, BackendError> {
    SvCodegen::generate_from_mir(mir)
}
