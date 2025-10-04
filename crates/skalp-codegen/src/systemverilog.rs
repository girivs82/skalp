//! SystemVerilog code generation from MIR
//!
//! This module generates proper SystemVerilog code from MIR,
//! preserving sequential logic, event blocks, and assignments.

use anyhow::Result;
use skalp_lir::LirDesign;
use skalp_mir::mir::PriorityMux;
use skalp_mir::{
    Assignment, AssignmentKind, DataType, EdgeType, EnumType, Mir, Module, Process, ProcessKind,
    Statement, StructType,
};
use std::collections::HashSet;

/// Generate SystemVerilog from MIR and LIR
pub fn generate_systemverilog_from_mir(mir: &Mir, lir: &LirDesign) -> Result<String> {
    let mut sv = String::new();

    // Generate each module
    for (mir_module, lir_module) in mir.modules.iter().zip(lir.modules.iter()) {
        sv.push_str(&generate_module(mir_module, lir_module)?);
    }

    Ok(sv)
}

/// Generate a single SystemVerilog module
fn generate_module(mir_module: &Module, lir_module: &skalp_lir::LirModule) -> Result<String> {
    let mut sv = String::new();

    // Collect all unique struct and enum types from ports and signals
    let mut struct_types: HashSet<String> = HashSet::new();
    let mut enum_types: HashSet<String> = HashSet::new();

    collect_types_from_module(mir_module, &mut struct_types, &mut enum_types);

    // Generate typedefs for structs and enums
    sv.push_str(&generate_typedefs(mir_module, &struct_types, &enum_types));

    // Module header with parameters
    sv.push_str(&format!("module {} ", mir_module.name));

    // Generate parameter list if any
    if !mir_module.parameters.is_empty() {
        sv.push_str("#(\n");
        let mut params = Vec::new();
        for param in &mir_module.parameters {
            let param_str = match &param.param_type {
                skalp_mir::GenericParameterType::Type => {
                    // Type parameters become Verilog parameters
                    if let Some(default) = &param.default {
                        format!("    parameter {} = {}", param.name, format_value(default))
                    } else {
                        format!("    parameter {} = 8", param.name) // Default to 8 if no default given
                    }
                }
                skalp_mir::GenericParameterType::Const(_) => {
                    // Const parameters become localparam
                    if let Some(default) = &param.default {
                        format!("    parameter {} = {}", param.name, format_value(default))
                    } else {
                        format!("    parameter {}", param.name)
                    }
                }
                skalp_mir::GenericParameterType::Width => {
                    // Width parameters are regular parameters
                    if let Some(default) = &param.default {
                        format!("    parameter {} = {}", param.name, format_value(default))
                    } else {
                        format!("    parameter {} = 32", param.name)
                    }
                }
                skalp_mir::GenericParameterType::ClockDomain => {
                    // Clock domain parameters are not emitted as Verilog parameters
                    continue;
                }
            };
            params.push(param_str);
        }
        sv.push_str(&params.join(",\n"));
        sv.push_str("\n) ");
    }

    sv.push_str("(\n");

    // Generate port list
    let mut ports = Vec::new();
    for port in &mir_module.ports {
        let direction = match port.direction {
            skalp_mir::PortDirection::Input => "input",
            skalp_mir::PortDirection::Output => "output",
            skalp_mir::PortDirection::InOut => "inout",
        };

        // Determine width from port type
        let width = get_width_spec(&port.port_type);
        ports.push(format!("    {} {}{}", direction, width, port.name));
    }

    if !ports.is_empty() {
        sv.push_str(&ports.join(",\n"));
        sv.push('\n');
    }

    sv.push_str(");\n\n");

    // Generate internal signal declarations
    for signal in &mir_module.signals {
        let width = get_width_spec(&signal.signal_type);

        // Determine if it's a reg or wire based on usage
        let signal_type = if is_register(signal, mir_module) {
            "reg"
        } else {
            "wire"
        };

        sv.push_str(&format!("    {} {}{}", signal_type, width, signal.name));

        // Add initial value if present
        if let Some(init) = &signal.initial {
            sv.push_str(&format!(" = {}", format_value(init)));
        }

        sv.push_str(";\n");
    }

    if !mir_module.signals.is_empty() {
        sv.push('\n');
    }

    // Generate continuous assignments
    for assign in &mir_module.assignments {
        sv.push_str(&format!(
            "    assign {} = {};\n",
            format_lvalue_with_context(&assign.lhs, mir_module),
            format_expression_with_context(&assign.rhs, mir_module)
        ));
    }

    if !mir_module.assignments.is_empty() {
        sv.push('\n');
    }

    // Generate processes (always blocks)
    for process in &mir_module.processes {
        sv.push_str(&generate_process(process, mir_module)?);
        sv.push('\n');
    }

    sv.push_str("endmodule\n");
    Ok(sv)
}

/// Generate an always block from a process
fn generate_process(process: &Process, module: &Module) -> Result<String> {
    let mut sv = String::new();

    // Generate always block header
    match process.kind {
        ProcessKind::Sequential => {
            sv.push_str("    always_ff @(");
            sv.push_str(&format_sensitivity(&process.sensitivity, module));
            sv.push_str(") begin\n");
        }
        ProcessKind::Combinational => {
            sv.push_str("    always_comb begin\n");
        }
        ProcessKind::General => {
            sv.push_str("    always @(");
            sv.push_str(&format_sensitivity(&process.sensitivity, module));
            sv.push_str(") begin\n");
        }
    }

    // Generate body statements
    for statement in &process.body.statements {
        sv.push_str(&generate_statement(statement, module, 2)?);
    }

    sv.push_str("    end\n");
    Ok(sv)
}

/// Generate a statement
fn generate_statement(stmt: &Statement, module: &Module, indent_level: usize) -> Result<String> {
    let indent = "    ".repeat(indent_level);
    let mut sv = String::new();

    match stmt {
        Statement::Assignment(assign) => {
            let op = match assign.kind {
                AssignmentKind::NonBlocking => "<=",
                AssignmentKind::Blocking => "=",
            };
            sv.push_str(&format!(
                "{}{} {} {};\n",
                indent,
                format_lvalue_with_context(&assign.lhs, module),
                op,
                format_expression_with_context(&assign.rhs, module)
            ));
        }
        Statement::If(if_stmt) => {
            sv.push_str(&format!(
                "{}if ({}) begin\n",
                indent,
                format_expression_with_context(&if_stmt.condition, module)
            ));
            for s in &if_stmt.then_block.statements {
                sv.push_str(&generate_statement(s, module, indent_level + 1)?);
            }
            sv.push_str(&format!("{}end", indent));

            if let Some(else_block) = &if_stmt.else_block {
                sv.push_str(" else begin\n");
                for s in &else_block.statements {
                    sv.push_str(&generate_statement(s, module, indent_level + 1)?);
                }
                sv.push_str(&format!("{}end", indent));
            }
            sv.push('\n');
        }
        Statement::Case(case_stmt) => {
            sv.push_str(&format!(
                "{}case ({})\n",
                indent,
                format_expression_with_context(&case_stmt.expr, module)
            ));

            // Generate case items
            for item in &case_stmt.items {
                let values = item
                    .values
                    .iter()
                    .map(|v| format_expression_with_context(v, module))
                    .collect::<Vec<_>>()
                    .join(", ");
                sv.push_str(&format!("    {}{}: begin\n", indent, values));

                for s in &item.block.statements {
                    sv.push_str(&generate_statement(s, module, indent_level + 2)?);
                }
                sv.push_str(&format!("    {}end\n", indent));
            }

            // Generate default case if present
            if let Some(default_block) = &case_stmt.default {
                sv.push_str(&format!("    {}default: begin\n", indent));
                for s in &default_block.statements {
                    sv.push_str(&generate_statement(s, module, indent_level + 2)?);
                }
                sv.push_str(&format!("    {}end\n", indent));
            }

            sv.push_str(&format!("{}endcase\n", indent));
        }
        Statement::Loop(_) => {
            // TODO: Implement loop statement generation
            sv.push_str(&format!("{}// TODO: loop statement\n", indent));
        }
        Statement::Block(block) => {
            sv.push_str(&format!("{}begin\n", indent));
            for s in &block.statements {
                sv.push_str(&generate_statement(s, module, indent_level + 1)?);
            }
            sv.push_str(&format!("{}end\n", indent));
        }
        Statement::ResolvedConditional(resolved) => {
            // Generate as a single ternary assignment - synthesis-resolved form
            let op = match resolved.kind {
                AssignmentKind::NonBlocking => "<=",
                AssignmentKind::Blocking => "=",
            };

            // Build nested ternary expression from priority mux
            let ternary_expr = build_ternary_expression(&resolved.resolved, module);
            sv.push_str(&format!(
                "{}{} {} {};\n",
                indent,
                format_lvalue_with_context(&resolved.target, module),
                op,
                ternary_expr
            ));
        }
    }

    Ok(sv)
}

/// Build nested ternary expression from priority mux
fn build_ternary_expression(mux: &PriorityMux, module: &Module) -> String {
    let mut result = format_expression_with_context(&mux.default, module);

    // Build ternary chain from right to left (lowest to highest priority)
    for case in mux.cases.iter().rev() {
        result = format!(
            "({}) ? ({}) : ({})",
            format_expression_with_context(&case.condition, module),
            format_expression_with_context(&case.value, module),
            result
        );
    }

    result
}

/// Format sensitivity list
fn format_sensitivity(sensitivity: &skalp_mir::SensitivityList, module: &Module) -> String {
    match sensitivity {
        skalp_mir::SensitivityList::Always => "*".to_string(),
        skalp_mir::SensitivityList::Level(signals) => signals
            .iter()
            .map(|s| format_lvalue_with_context(s, module))
            .collect::<Vec<_>>()
            .join(" or "),
        skalp_mir::SensitivityList::Edge(edges) => edges
            .iter()
            .map(|e| {
                let edge_str = match e.edge {
                    EdgeType::Rising => "posedge",
                    EdgeType::Falling => "negedge",
                    EdgeType::Both => "",
                    EdgeType::Active => "",
                    EdgeType::Inactive => "",
                };
                if edge_str.is_empty() {
                    format_lvalue_with_context(&e.signal, module)
                } else {
                    format!(
                        "{} {}",
                        edge_str,
                        format_lvalue_with_context(&e.signal, module)
                    )
                }
            })
            .collect::<Vec<_>>()
            .join(" or "),
    }
}

/// Format an lvalue with module context for name lookup
fn format_lvalue_with_context(lvalue: &skalp_mir::LValue, module: &Module) -> String {
    match lvalue {
        skalp_mir::LValue::Signal(id) => {
            // Find the signal name by ID
            module
                .signals
                .iter()
                .find(|s| s.id == *id)
                .map(|s| s.name.clone())
                .unwrap_or_else(|| format!("signal_{}", id.0))
        }
        skalp_mir::LValue::Variable(id) => {
            // Find the variable name by ID
            module
                .variables
                .iter()
                .find(|v| v.id == *id)
                .map(|v| v.name.clone())
                .unwrap_or_else(|| format!("var_{}", id.0))
        }
        skalp_mir::LValue::Port(id) => {
            // Find the port name by ID
            for port in &module.ports {}
            module
                .ports
                .iter()
                .find(|p| p.id == *id)
                .map(|p| p.name.clone())
                .unwrap_or_else(|| format!("port_{}", id.0))
        }
        skalp_mir::LValue::BitSelect { base, index } => {
            format!(
                "{}[{}]",
                format_lvalue_with_context(base, module),
                format_expression_with_context(index, module)
            )
        }
        skalp_mir::LValue::RangeSelect { base, high, low } => {
            format!(
                "{}[{}:{}]",
                format_lvalue_with_context(base, module),
                format_expression_with_context(high, module),
                format_expression_with_context(low, module)
            )
        }
        skalp_mir::LValue::Concat(lvalues) => {
            let parts: Vec<_> = lvalues
                .iter()
                .map(|lv| format_lvalue_with_context(lv, module))
                .collect();
            format!("{{{}}}", parts.join(", "))
        }
    }
}

/// Format an lvalue (fallback without context)
fn format_lvalue(lvalue: &skalp_mir::LValue) -> String {
    match lvalue {
        skalp_mir::LValue::Signal(id) => format!("signal_{}", id.0),
        skalp_mir::LValue::Variable(id) => format!("var_{}", id.0),
        skalp_mir::LValue::Port(id) => format!("port_{}", id.0),
        skalp_mir::LValue::BitSelect { base, index } => {
            format!("{}[{}]", format_lvalue(base), format_expression(index))
        }
        skalp_mir::LValue::RangeSelect { base, high, low } => {
            format!(
                "{}[{}:{}]",
                format_lvalue(base),
                format_expression(high),
                format_expression(low)
            )
        }
        skalp_mir::LValue::Concat(lvalues) => {
            let parts: Vec<_> = lvalues.iter().map(format_lvalue).collect();
            format!("{{{}}}", parts.join(", "))
        }
    }
}

/// Format an expression with module context
fn format_expression_with_context(expr: &skalp_mir::Expression, module: &Module) -> String {
    match expr {
        skalp_mir::Expression::Literal(val) => format_value(val),
        skalp_mir::Expression::Ref(lval) => format_lvalue_with_context(lval, module),
        skalp_mir::Expression::Binary { op, left, right } => {
            format!(
                "({} {} {})",
                format_expression_with_context(left, module),
                format_binary_op(op),
                format_expression_with_context(right, module)
            )
        }
        skalp_mir::Expression::Unary { op, operand } => {
            format!(
                "{}{}",
                format_unary_op(op),
                format_expression_with_context(operand, module)
            )
        }
        skalp_mir::Expression::Conditional {
            cond,
            then_expr,
            else_expr,
        } => {
            format!(
                "({} ? {} : {})",
                format_expression_with_context(cond, module),
                format_expression_with_context(then_expr, module),
                format_expression_with_context(else_expr, module)
            )
        }
        skalp_mir::Expression::Concat(exprs) => {
            let parts: Vec<_> = exprs
                .iter()
                .map(|e| format_expression_with_context(e, module))
                .collect();
            format!("{{{}}}", parts.join(", "))
        }
        skalp_mir::Expression::Replicate { count, value } => {
            format!(
                "{{{}{{{}}}}}",
                format_expression_with_context(count, module),
                format_expression_with_context(value, module)
            )
        }
        skalp_mir::Expression::FunctionCall { name, args } => {
            let arg_strs: Vec<_> = args
                .iter()
                .map(|a| format_expression_with_context(a, module))
                .collect();
            format!("{}({})", name, arg_strs.join(", "))
        }
    }
}

/// Format an expression (fallback without context)
fn format_expression(expr: &skalp_mir::Expression) -> String {
    match expr {
        skalp_mir::Expression::Literal(val) => format_value(val),
        skalp_mir::Expression::Ref(lval) => format_lvalue(lval),
        skalp_mir::Expression::Binary { op, left, right } => {
            format!(
                "({} {} {})",
                format_expression(left),
                format_binary_op(op),
                format_expression(right)
            )
        }
        skalp_mir::Expression::Unary { op, operand } => {
            format!("{}{}", format_unary_op(op), format_expression(operand))
        }
        skalp_mir::Expression::Conditional {
            cond,
            then_expr,
            else_expr,
        } => {
            format!(
                "({} ? {} : {})",
                format_expression(cond),
                format_expression(then_expr),
                format_expression(else_expr)
            )
        }
        skalp_mir::Expression::Concat(exprs) => {
            let parts: Vec<_> = exprs.iter().map(format_expression).collect();
            format!("{{{}}}", parts.join(", "))
        }
        skalp_mir::Expression::Replicate { count, value } => {
            format!(
                "{{{}{{{}}}}}",
                format_expression(count),
                format_expression(value)
            )
        }
        skalp_mir::Expression::FunctionCall { name, args } => {
            let arg_strs: Vec<_> = args.iter().map(format_expression).collect();
            format!("{}({})", name, arg_strs.join(", "))
        }
    }
}

/// Format a value
fn format_value(value: &skalp_mir::Value) -> String {
    match value {
        skalp_mir::Value::Integer(n) => n.to_string(),
        skalp_mir::Value::BitVector { width, value } => {
            format!("{}'b{:0width$b}", width, value, width = *width)
        }
        skalp_mir::Value::String(s) => format!("\"{}\"", s),
        skalp_mir::Value::Unknown => "'x".to_string(),
        skalp_mir::Value::HighZ => "'z".to_string(),
    }
}

/// Format binary operator
fn format_binary_op(op: &skalp_mir::BinaryOp) -> &'static str {
    match op {
        skalp_mir::BinaryOp::Add => "+",
        skalp_mir::BinaryOp::Sub => "-",
        skalp_mir::BinaryOp::Mul => "*",
        skalp_mir::BinaryOp::Div => "/",
        skalp_mir::BinaryOp::Mod => "%",
        skalp_mir::BinaryOp::And => "&", // Logical operations (same as bitwise in Verilog context)
        skalp_mir::BinaryOp::Or => "|",
        skalp_mir::BinaryOp::Xor => "^",
        skalp_mir::BinaryOp::BitwiseAnd => "&",
        skalp_mir::BinaryOp::BitwiseOr => "|",
        skalp_mir::BinaryOp::BitwiseXor => "^",
        skalp_mir::BinaryOp::LogicalAnd => "&&",
        skalp_mir::BinaryOp::LogicalOr => "||",
        skalp_mir::BinaryOp::Equal => "==",
        skalp_mir::BinaryOp::NotEqual => "!=",
        skalp_mir::BinaryOp::Less => "<",
        skalp_mir::BinaryOp::LessEqual => "<=",
        skalp_mir::BinaryOp::Greater => ">",
        skalp_mir::BinaryOp::GreaterEqual => ">=",
        skalp_mir::BinaryOp::LeftShift => "<<",
        skalp_mir::BinaryOp::RightShift => ">>",
    }
}

/// Format unary operator
fn format_unary_op(op: &skalp_mir::UnaryOp) -> &'static str {
    match op {
        skalp_mir::UnaryOp::BitwiseNot => "~",
        skalp_mir::UnaryOp::Not => "!",
        skalp_mir::UnaryOp::Negate => "-",
        skalp_mir::UnaryOp::Reduce(reduce_op) => match reduce_op {
            skalp_mir::ReduceOp::And => "&",
            skalp_mir::ReduceOp::Or => "|",
            skalp_mir::ReduceOp::Xor => "^",
            skalp_mir::ReduceOp::Nand => "~&",
            skalp_mir::ReduceOp::Nor => "~|",
            skalp_mir::ReduceOp::Xnor => "~^",
        },
    }
}

/// Get width specification string for a data type
fn get_width_spec(data_type: &skalp_mir::DataType) -> String {
    match data_type {
        skalp_mir::DataType::Bit(width)
        | skalp_mir::DataType::Logic(width)
        | skalp_mir::DataType::Int(width)
        | skalp_mir::DataType::Nat(width) => {
            if *width > 1 {
                format!("[{}:0] ", width - 1)
            } else {
                String::new()
            }
        }
        // Parametric types use parameter expression
        skalp_mir::DataType::BitParam { param, default }
        | skalp_mir::DataType::LogicParam { param, default }
        | skalp_mir::DataType::IntParam { param, default }
        | skalp_mir::DataType::NatParam { param, default } => {
            if *default > 1 {
                format!("[{}-1:0] ", param)
            } else {
                String::new()
            }
        }
        skalp_mir::DataType::Clock { .. } => String::new(), // Clocks are single bit
        skalp_mir::DataType::Reset { .. } => String::new(), // Resets are single bit
        skalp_mir::DataType::Event => String::new(),        // Events have no width
        skalp_mir::DataType::Array(element_type, size) => {
            // Array format: [element_width][0:size-1]
            let element_width = get_width_spec(element_type);
            if *size > 1 {
                format!("{}[0:{}] ", element_width.trim(), size - 1)
            } else {
                element_width
            }
        }
        skalp_mir::DataType::Struct(struct_type) => {
            // For structs, calculate total width
            let mut total_width = 0;
            for field in &struct_type.fields {
                total_width += get_type_width(&field.field_type);
            }
            if total_width > 1 {
                format!("[{}:0] ", total_width - 1)
            } else {
                String::new()
            }
        }
        skalp_mir::DataType::Enum(_) => {
            // Enums default to 32-bit
            "[31:0] ".to_string()
        }
        skalp_mir::DataType::Union(union_type) => {
            // Unions use the width of the largest field
            let mut max_width = 0;
            for field in &union_type.fields {
                let width = get_type_width(&field.field_type);
                if width > max_width {
                    max_width = width;
                }
            }
            if max_width > 1 {
                format!("[{}:0] ", max_width - 1)
            } else {
                String::new()
            }
        }
    }
}

/// Get the width in bits of a data type
fn get_type_width(data_type: &skalp_mir::DataType) -> usize {
    match data_type {
        skalp_mir::DataType::Bit(width)
        | skalp_mir::DataType::Logic(width)
        | skalp_mir::DataType::Int(width)
        | skalp_mir::DataType::Nat(width) => *width,
        // Parametric types use their default width for calculations
        skalp_mir::DataType::BitParam { default, .. }
        | skalp_mir::DataType::LogicParam { default, .. }
        | skalp_mir::DataType::IntParam { default, .. }
        | skalp_mir::DataType::NatParam { default, .. } => *default,
        skalp_mir::DataType::Clock { .. } => 1,
        skalp_mir::DataType::Reset { .. } => 1,
        skalp_mir::DataType::Event => 1,
        skalp_mir::DataType::Array(element_type, size) => get_type_width(element_type) * size,
        skalp_mir::DataType::Struct(struct_type) => {
            let mut total_width = 0;
            for field in &struct_type.fields {
                total_width += get_type_width(&field.field_type);
            }
            total_width
        }
        skalp_mir::DataType::Enum(_) => 32, // Default enum width
        skalp_mir::DataType::Union(union_type) => {
            let mut max_width = 0;
            for field in &union_type.fields {
                let width = get_type_width(&field.field_type);
                if width > max_width {
                    max_width = width;
                }
            }
            max_width
        }
    }
}

/// Check if a signal is a register (assigned in sequential blocks)
fn is_register(signal: &skalp_mir::Signal, module: &Module) -> bool {
    // A signal is a register if it's assigned in any sequential process
    for process in &module.processes {
        if process.kind == ProcessKind::Sequential
            && is_signal_assigned_in_block(&signal.id, &process.body)
        {
            return true;
        }
    }
    false
}

/// Check if a signal is assigned in a block
fn is_signal_assigned_in_block(signal_id: &skalp_mir::SignalId, block: &skalp_mir::Block) -> bool {
    for stmt in &block.statements {
        match stmt {
            Statement::Assignment(assign) => {
                if let skalp_mir::LValue::Signal(id) = &assign.lhs {
                    if id == signal_id {
                        return true;
                    }
                }
            }
            Statement::If(if_stmt) => {
                if is_signal_assigned_in_block(signal_id, &if_stmt.then_block) {
                    return true;
                }
                if let Some(else_block) = &if_stmt.else_block {
                    if is_signal_assigned_in_block(signal_id, else_block) {
                        return true;
                    }
                }
            }
            Statement::Block(block) => {
                if is_signal_assigned_in_block(signal_id, block) {
                    return true;
                }
            }
            _ => {}
        }
    }
    false
}

/// Collect all struct and enum types used in a module
fn collect_types_from_module(
    module: &Module,
    struct_types: &mut HashSet<String>,
    enum_types: &mut HashSet<String>,
) {
    // Collect from port types
    for port in &module.ports {
        collect_types_from_datatype(&port.port_type, struct_types, enum_types);
    }

    // Collect from signal types
    for signal in &module.signals {
        collect_types_from_datatype(&signal.signal_type, struct_types, enum_types);
    }
}

/// Recursively collect struct and enum types from a DataType
fn collect_types_from_datatype(
    data_type: &DataType,
    struct_types: &mut HashSet<String>,
    enum_types: &mut HashSet<String>,
) {
    match data_type {
        DataType::Struct(struct_type) => {
            struct_types.insert(struct_type.name.clone());
            // Collect types from struct fields
            for field in &struct_type.fields {
                collect_types_from_datatype(&field.field_type, struct_types, enum_types);
            }
        }
        DataType::Enum(enum_type) => {
            enum_types.insert(enum_type.name.clone());
            // Collect types from enum base type
            collect_types_from_datatype(&enum_type.base_type, struct_types, enum_types);
        }
        DataType::Array(element_type, _) => {
            collect_types_from_datatype(element_type, struct_types, enum_types);
        }
        DataType::Union(union_type) => {
            // For unions, collect field types (treat similar to structs for now)
            for field in &union_type.fields {
                collect_types_from_datatype(&field.field_type, struct_types, enum_types);
            }
        }
        _ => {} // Primitive types don't need typedef collection
    }
}

/// Generate SystemVerilog typedefs for collected struct and enum types
fn generate_typedefs(
    module: &Module,
    struct_type_names: &HashSet<String>,
    enum_type_names: &HashSet<String>,
) -> String {
    let mut typedefs = String::new();

    if struct_type_names.is_empty() && enum_type_names.is_empty() {
        return typedefs;
    }

    // Find the actual type definitions in the module's port/signal types
    let mut generated_structs: HashSet<String> = HashSet::new();
    let mut generated_enums: HashSet<String> = HashSet::new();

    // Generate struct typedefs
    for port in &module.ports {
        generate_typedefs_for_datatype(
            &port.port_type,
            &mut typedefs,
            &mut generated_structs,
            &mut generated_enums,
            struct_type_names,
            enum_type_names,
        );
    }

    for signal in &module.signals {
        generate_typedefs_for_datatype(
            &signal.signal_type,
            &mut typedefs,
            &mut generated_structs,
            &mut generated_enums,
            struct_type_names,
            enum_type_names,
        );
    }

    if !typedefs.is_empty() {
        typedefs.push('\n');
    }

    typedefs
}

/// Generate typedefs for a specific DataType
fn generate_typedefs_for_datatype(
    data_type: &DataType,
    typedefs: &mut String,
    generated_structs: &mut HashSet<String>,
    generated_enums: &mut HashSet<String>,
    target_structs: &HashSet<String>,
    target_enums: &HashSet<String>,
) {
    match data_type {
        DataType::Struct(struct_type) => {
            if target_structs.contains(&struct_type.name)
                && !generated_structs.contains(&struct_type.name)
            {
                // Generate struct fields first (in case they reference other types)
                for field in &struct_type.fields {
                    generate_typedefs_for_datatype(
                        &field.field_type,
                        typedefs,
                        generated_structs,
                        generated_enums,
                        target_structs,
                        target_enums,
                    );
                }

                // Generate the struct typedef
                typedefs.push_str("typedef struct {\n");
                for field in &struct_type.fields {
                    let field_width = get_width_spec(&field.field_type);
                    typedefs.push_str(&format!(
                        "    {}{} {};\n",
                        get_systemverilog_type(&field.field_type),
                        field_width,
                        field.name
                    ));
                }
                typedefs.push_str(&format!("}} {};\n\n", struct_type.name));

                generated_structs.insert(struct_type.name.clone());
            }
        }
        DataType::Enum(enum_type) => {
            if target_enums.contains(&enum_type.name) && !generated_enums.contains(&enum_type.name)
            {
                // Generate base type first
                generate_typedefs_for_datatype(
                    &enum_type.base_type,
                    typedefs,
                    generated_structs,
                    generated_enums,
                    target_structs,
                    target_enums,
                );

                // Generate the enum typedef
                let base_width = get_width_spec(&enum_type.base_type);
                typedefs.push_str(&format!(
                    "typedef enum {}{} {{\n",
                    get_systemverilog_type(&enum_type.base_type),
                    base_width
                ));

                for (i, variant) in enum_type.variants.iter().enumerate() {
                    if i > 0 {
                        typedefs.push_str(",\n");
                    }
                    if let Some(value) = &variant.value {
                        typedefs.push_str(&format!(
                            "    {} = {}",
                            variant.name,
                            format_value(value)
                        ));
                    } else {
                        typedefs.push_str(&format!("    {}", variant.name));
                    }
                }
                typedefs.push_str(&format!("\n}} {};\n\n", enum_type.name));

                generated_enums.insert(enum_type.name.clone());
            }
        }
        DataType::Array(element_type, _) => {
            generate_typedefs_for_datatype(
                element_type,
                typedefs,
                generated_structs,
                generated_enums,
                target_structs,
                target_enums,
            );
        }
        DataType::Union(union_type) => {
            // Handle unions similarly to structs for now
            for field in &union_type.fields {
                generate_typedefs_for_datatype(
                    &field.field_type,
                    typedefs,
                    generated_structs,
                    generated_enums,
                    target_structs,
                    target_enums,
                );
            }
        }
        _ => {} // Primitive types don't need typedef generation
    }
}

/// Get the SystemVerilog type name for a DataType (for typedef generation)
fn get_systemverilog_type(data_type: &DataType) -> &'static str {
    match data_type {
        DataType::Bit(_) | DataType::BitParam { .. } => "bit",
        DataType::Logic(_) | DataType::LogicParam { .. } => "logic",
        DataType::Int(_) | DataType::IntParam { .. } => "int",
        DataType::Nat(_) | DataType::NatParam { .. } => "logic", // Use logic for unsigned naturals
        _ => "logic",                                            // Default to logic for other types
    }
}
