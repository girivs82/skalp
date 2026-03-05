//! HIR → SystemVerilog source code emitter

use crate::skalp_emit::emit_expression_inline as sk_expr;
use crate::{emit_comments, indent, is_port_sequential, NameResolver};
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
            let is_seq = matches!(port.direction, HirPortDirection::Output)
                && imp.is_some_and(|imp| is_port_sequential(port.id, imp));
            let dir = match port.direction {
                HirPortDirection::Input => "input",
                HirPortDirection::Output => {
                    if is_seq {
                        "output reg"
                    } else {
                        "output"
                    }
                }
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

    // Vendor IP wrapper — emit wrapper module instead of normal body
    if let Some(ref vendor_ip) = entity.vendor_ip_config {
        emit_sv_vendor_ip_wrapper(out, entity, vendor_ip, resolver, 1);
        writeln!(out, "endmodule").unwrap();
        return;
    }

    // Entity-level signal declarations (with synthesis attributes)
    for signal in &entity.signals {
        let ind = indent(1);
        emit_comments(out, &signal.comments, "//", &ind);
        emit_sv_power_attributes(out, signal, 1);
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

    // Entity-level breakpoints
    let entity_bp_signals: Vec<_> = entity
        .signals
        .iter()
        .filter(|s| s.breakpoint_config.is_some())
        .collect();
    if !entity_bp_signals.is_empty() {
        let ind = indent(1);
        writeln!(out, "{ind}// Debug Breakpoint Assertions").unwrap();
        for signal in &entity_bp_signals {
            emit_sv_breakpoint(out, signal, 1);
        }
        writeln!(out).unwrap();
    }

    // Entity-level CDC synchronizers
    let entity_cdc_signals: Vec<_> = entity
        .signals
        .iter()
        .filter(|s| s.cdc_config.is_some())
        .collect();
    if !entity_cdc_signals.is_empty() {
        let ind = indent(1);
        writeln!(out, "{ind}// CDC Synchronizer Logic").unwrap();
        for signal in &entity_cdc_signals {
            emit_sv_cdc_synchronizer(out, signal, 1);
        }
        writeln!(out).unwrap();
    }

    // Entity-level power isolation logic
    let entity_iso_signals: Vec<_> = entity
        .signals
        .iter()
        .filter(|s| {
            s.power_config
                .as_ref()
                .is_some_and(|pc| pc.isolation.is_some())
        })
        .collect();
    if !entity_iso_signals.is_empty() {
        let ind = indent(1);
        writeln!(out, "{ind}// Power Isolation Logic").unwrap();
        for signal in &entity_iso_signals {
            emit_sv_isolation_logic(out, signal, 1);
        }
        writeln!(out).unwrap();
    }

    // Module body
    if let Some(imp) = imp {
        // Signal declarations (with synthesis attributes for power/CDC)
        for signal in &imp.signals {
            let ind = indent(1);
            emit_comments(out, &signal.comments, "//", &ind);
            // Emit power synthesis attributes before signal
            emit_sv_power_attributes(out, signal, 1);
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

        // Breakpoint assertions
        let breakpoint_signals: Vec<_> = imp
            .signals
            .iter()
            .filter(|s| s.breakpoint_config.is_some())
            .collect();
        if !breakpoint_signals.is_empty() {
            let ind = indent(1);
            writeln!(out, "{ind}// Debug Breakpoint Assertions").unwrap();
            for signal in &breakpoint_signals {
                emit_sv_breakpoint(out, signal, 1);
            }
            writeln!(out).unwrap();
        }

        // Power isolation logic
        let isolation_signals: Vec<_> = imp
            .signals
            .iter()
            .filter(|s| {
                s.power_config
                    .as_ref()
                    .is_some_and(|pc| pc.isolation.is_some())
            })
            .collect();
        if !isolation_signals.is_empty() {
            let ind = indent(1);
            writeln!(out, "{ind}// Power Isolation Logic").unwrap();
            for signal in &isolation_signals {
                emit_sv_isolation_logic(out, signal, 1);
            }
            writeln!(out).unwrap();
        }

        // CDC synchronizer chains
        let cdc_signals: Vec<_> = imp
            .signals
            .iter()
            .filter(|s| s.cdc_config.is_some())
            .collect();
        if !cdc_signals.is_empty() {
            let ind = indent(1);
            writeln!(out, "{ind}// CDC Synchronizer Logic").unwrap();
            for signal in &cdc_signals {
                emit_sv_cdc_synchronizer(out, signal, 1);
            }
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

        // Formal verification blocks
        for fb in &imp.formal_blocks {
            emit_sv_formal_block(out, fb, resolver, 1);
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
        // Build sensitivity list, resolving Active/Inactive edges to posedge/negedge
        // based on reset polarity from the signal/port type
        let sens: Vec<String> = eb
            .triggers
            .iter()
            .map(|t| {
                let sig = match &t.signal {
                    HirEventSignal::Port(id) => resolver.port_name(*id).to_string(),
                    HirEventSignal::Signal(id) => resolver.signal_name(*id).to_string(),
                };
                let edge = resolve_sv_edge(t, resolver);
                format!("{edge} {sig}")
            })
            .collect();

        writeln!(out, "{ind}always_ff @({}) begin", sens.join(" or ")).unwrap();

        // The HIR body already contains the if/else structure for async reset
        // (when present). Just emit the body directly.
        for stmt in &eb.statements {
            emit_sv_statement(out, stmt, resolver, level + 1, true);
        }
    } else {
        writeln!(out, "{ind}always_comb begin").unwrap();
        for stmt in &eb.statements {
            emit_sv_statement(out, stmt, resolver, level + 1, false);
        }
    }

    writeln!(out, "{ind}end").unwrap();
}

/// Resolve an HirEdgeType to a SystemVerilog edge keyword, looking up
/// reset polarity for Active/Inactive edge types.
fn resolve_sv_edge(trigger: &HirEventTrigger, resolver: &NameResolver) -> &'static str {
    match trigger.edge {
        HirEdgeType::Rising => "posedge",
        HirEdgeType::Falling => "negedge",
        HirEdgeType::Active => {
            if is_reset_active_high(trigger, resolver) {
                "posedge"
            } else {
                "negedge"
            }
        }
        HirEdgeType::Inactive => {
            if is_reset_active_high(trigger, resolver) {
                "negedge"
            } else {
                "posedge"
            }
        }
        HirEdgeType::Both => "posedge", // fallback
    }
}

/// Determine if a reset signal is active-high by looking at its HirType.
fn is_reset_active_high(trigger: &HirEventTrigger, resolver: &NameResolver) -> bool {
    let ty = match &trigger.signal {
        HirEventSignal::Port(id) => resolver.port_type(*id),
        HirEventSignal::Signal(id) => resolver.signal_type(*id),
    };
    match ty {
        Some(HirType::Reset { polarity, .. }) => {
            matches!(polarity, HirResetPolarity::ActiveHigh)
        }
        _ => true, // default to active-high
    }
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
        HirStatement::Assert(assert_stmt) => {
            let cond = emit_expr(&assert_stmt.condition, resolver);
            let severity_fn = match assert_stmt.severity {
                HirAssertionSeverity::Info => "$info",
                HirAssertionSeverity::Warning => "$warning",
                HirAssertionSeverity::Error => "$error",
                HirAssertionSeverity::Fatal => "$fatal",
            };
            if let Some(ref msg) = assert_stmt.message {
                writeln!(out, "{ind}assert({cond}) else {severity_fn}(\"{msg}\");").unwrap();
            } else {
                writeln!(
                    out,
                    "{ind}assert({cond}) else {severity_fn}(\"Assertion failed\");"
                )
                .unwrap();
            }
        }
        HirStatement::Assume(assume_stmt) => {
            let cond = emit_expr(&assume_stmt.condition, resolver);
            if let Some(ref msg) = assume_stmt.message {
                writeln!(out, "{ind}assume({cond}); // {msg}").unwrap();
            } else {
                writeln!(out, "{ind}assume({cond});").unwrap();
            }
        }
        HirStatement::Cover(cover_stmt) => {
            let prop = emit_sv_property(&cover_stmt.property, resolver);
            if let Some(ref name) = cover_stmt.name {
                writeln!(out, "{ind}{name}: cover property ({prop});").unwrap();
            } else {
                writeln!(out, "{ind}cover property ({prop});").unwrap();
            }
        }
        HirStatement::Property(prop_stmt) => {
            let prop = emit_sv_property(&prop_stmt.property, resolver);
            if let Some(ref clk) = prop_stmt.clock {
                let clk_expr = emit_expr(clk, resolver);
                writeln!(
                    out,
                    "{ind}{}: assert property (@(posedge {clk_expr}) {prop});",
                    prop_stmt.name
                )
                .unwrap();
            } else {
                writeln!(out, "{ind}{}: assert property ({prop});", prop_stmt.name).unwrap();
            }
        }
        _ => {
            writeln!(out, "{ind}// unsupported statement").unwrap();
        }
    }
}

/// Emit an SVA property expression.
fn emit_sv_property(prop: &HirProperty, resolver: &NameResolver) -> String {
    match prop {
        HirProperty::Expression(expr) => emit_expr(expr, resolver),
        HirProperty::Sequence(seq) => emit_sv_sequence(seq, resolver),
        HirProperty::Implication {
            antecedent,
            consequent,
        } => {
            format!(
                "{} |-> {}",
                emit_sv_property(antecedent, resolver),
                emit_sv_property(consequent, resolver)
            )
        }
        HirProperty::OverlappingImplication {
            antecedent,
            consequent,
        } => {
            format!(
                "{} |=> {}",
                emit_sv_property(antecedent, resolver),
                emit_sv_property(consequent, resolver)
            )
        }
        HirProperty::And(a, b) => {
            format!(
                "({} and {})",
                emit_sv_property(a, resolver),
                emit_sv_property(b, resolver)
            )
        }
        HirProperty::Or(a, b) => {
            format!(
                "({} or {})",
                emit_sv_property(a, resolver),
                emit_sv_property(b, resolver)
            )
        }
        HirProperty::Not(p) => format!("not ({})", emit_sv_property(p, resolver)),
        HirProperty::Always(p) => format!("always ({})", emit_sv_property(p, resolver)),
        HirProperty::Eventually(p) => {
            format!("s_eventually ({})", emit_sv_property(p, resolver))
        }
        HirProperty::Until {
            left,
            right,
            strong,
        } => {
            let kw = if *strong { "s_until" } else { "until" };
            format!(
                "{} {kw} {}",
                emit_sv_property(left, resolver),
                emit_sv_property(right, resolver)
            )
        }
        HirProperty::Throughout { left, right } => {
            format!(
                "{} throughout {}",
                emit_sv_property(left, resolver),
                emit_sv_property(right, resolver)
            )
        }
        HirProperty::Clocked {
            clock_edge,
            property,
        } => {
            let edge = emit_sv_clock_edge(clock_edge, resolver);
            format!("@({edge}) {}", emit_sv_property(property, resolver))
        }
    }
}

/// Emit an SVA sequence.
fn emit_sv_sequence(seq: &HirSequence, resolver: &NameResolver) -> String {
    let parts: Vec<_> = seq
        .elements
        .iter()
        .map(|e| emit_sv_sequence_element(e, resolver))
        .collect();
    parts.join(" ")
}

/// Emit an SVA sequence element.
fn emit_sv_sequence_element(elem: &HirSequenceElement, resolver: &NameResolver) -> String {
    match elem {
        HirSequenceElement::Expression(expr) => emit_expr(expr, resolver),
        HirSequenceElement::Delay(n) => format!("##{n}"),
        HirSequenceElement::DelayRange(m, n) => format!("##[{m}:{n}]"),
        HirSequenceElement::Repetition(e, n) => {
            format!("{}[*{n}]", emit_sv_sequence_element(e, resolver))
        }
        HirSequenceElement::RepetitionRange(e, m, n) => {
            format!("{}[*{m}:{n}]", emit_sv_sequence_element(e, resolver))
        }
        HirSequenceElement::ConsecutiveRepetition(e, n) => {
            format!("{}[*{n}]", emit_sv_sequence_element(e, resolver))
        }
        HirSequenceElement::ConsecutiveRepetitionRange(e, m, n) => {
            format!("{}[*{m}:{n}]", emit_sv_sequence_element(e, resolver))
        }
        HirSequenceElement::GotoRepetition(e, n) => {
            format!("{}[->{n}]", emit_sv_sequence_element(e, resolver))
        }
        HirSequenceElement::GotoRepetitionRange(e, m, n) => {
            format!("{}[->{m}:{n}]", emit_sv_sequence_element(e, resolver))
        }
        HirSequenceElement::Concatenation(elems) => {
            let parts: Vec<_> = elems
                .iter()
                .map(|e| emit_sv_sequence_element(e, resolver))
                .collect();
            parts.join(" ##0 ")
        }
        HirSequenceElement::Intersection(a, b) => {
            format!(
                "({} intersect {})",
                emit_sv_sequence_element(a, resolver),
                emit_sv_sequence_element(b, resolver)
            )
        }
        HirSequenceElement::Union(a, b) => {
            format!(
                "({} or {})",
                emit_sv_sequence_element(a, resolver),
                emit_sv_sequence_element(b, resolver)
            )
        }
    }
}

/// Emit a clock edge specification for SVA.
fn emit_sv_clock_edge(edge: &HirClockEdge, resolver: &NameResolver) -> String {
    match edge {
        HirClockEdge::Posedge(expr) => format!("posedge {}", emit_expr(expr, resolver)),
        HirClockEdge::Negedge(expr) => format!("negedge {}", emit_expr(expr, resolver)),
        HirClockEdge::Edge(expr) => emit_expr(expr, resolver),
    }
}

/// Emit a vendor IP wrapper module body.
fn emit_sv_vendor_ip_wrapper(
    out: &mut String,
    entity: &HirEntity,
    config: &VendorIpConfig,
    _resolver: &NameResolver,
    level: usize,
) {
    let ind = indent(level);
    let vendor_name = match config.vendor {
        VendorType::Xilinx => "Xilinx (AMD)",
        VendorType::Intel => "Intel (Altera)",
        VendorType::Lattice => "Lattice",
        VendorType::Generic => "Generic",
    };
    writeln!(out, "{ind}// Vendor IP Wrapper: {}", config.ip_name).unwrap();
    writeln!(out, "{ind}// Vendor: {vendor_name}").unwrap();
    if let Some(ref lib) = config.library {
        writeln!(out, "{ind}// Library: {lib}").unwrap();
    }
    if let Some(ref ver) = config.version {
        writeln!(out, "{ind}// Version: {ver}").unwrap();
    }
    writeln!(out, "{ind}// Generated by Skalp - do not edit manually").unwrap();
    writeln!(out).unwrap();

    if config.black_box {
        writeln!(
            out,
            "{ind}// Black-box module - implementation provided externally"
        )
        .unwrap();
        return;
    }

    // Build port mapping (entity_port -> ip_port)
    let port_map: std::collections::HashMap<&str, &str> = config
        .port_map
        .iter()
        .map(|(e, ip)| (e.as_str(), ip.as_str()))
        .collect();

    // IP instantiation
    write!(out, "{ind}{}", config.ip_name).unwrap();

    // Parameters
    if !config.parameters.is_empty() {
        writeln!(out, " #(").unwrap();
        for (i, (name, val)) in config.parameters.iter().enumerate() {
            let sep = if i + 1 < config.parameters.len() {
                ","
            } else {
                ""
            };
            writeln!(out, "{ind}    .{name}({val}){sep}").unwrap();
        }
        write!(out, "{ind})").unwrap();
    }

    writeln!(out, " ip_inst (").unwrap();

    // Port connections
    let mut connections = Vec::new();

    for port in &entity.ports {
        let ip_port = port_map
            .get(port.name.as_str())
            .copied()
            .unwrap_or(&port.name);
        connections.push(format!("{ind}    .{ip_port}({})", port.name));
    }

    // Tie-low ports
    for port in &config.tie_low {
        connections.push(format!("{ind}    .{port}(1'b0)"));
    }

    // Tie-high ports
    for port in &config.tie_high {
        connections.push(format!("{ind}    .{port}(1'b1)"));
    }

    // Unconnected ports
    for port in &config.unconnected {
        connections.push(format!("{ind}    .{port}()"));
    }

    for (i, conn) in connections.iter().enumerate() {
        let sep = if i + 1 < connections.len() { "," } else { "" };
        writeln!(out, "{conn}{sep}").unwrap();
    }
    writeln!(out, "{ind});").unwrap();
}

/// Emit a CDC synchronizer chain for a signal.
fn emit_sv_cdc_synchronizer(out: &mut String, signal: &HirSignal, level: usize) {
    let ind = indent(level);
    let Some(ref cdc) = signal.cdc_config else {
        return;
    };
    let name = &signal.name;
    let stages = cdc.sync_stages;
    let ty = emit_type(&signal.signal_type);
    let from = cdc.from_domain.as_deref().unwrap_or("src");
    let to = cdc.to_domain.as_deref().unwrap_or("dst");

    writeln!(out, "{ind}// CDC: {name} ({from} -> {to})").unwrap();

    match cdc.cdc_type {
        CdcType::TwoFF => {
            // N-stage FF synchronizer chain
            for i in 0..stages {
                writeln!(out, "{ind}(* ASYNC_REG = \"TRUE\" *)").unwrap();
                writeln!(out, "{ind}{ty} {name}_sync_{i};").unwrap();
            }
            // The original signal is aliased to the last sync stage
            writeln!(out, "{ind}assign {name} = {name}_sync_{};", stages - 1).unwrap();
        }
        CdcType::Gray => {
            // Gray code synchronizer: binary→gray, sync, gray→binary
            let width = type_width(&signal.signal_type);
            writeln!(out, "{ind}{ty} {name}_bin_in;").unwrap();
            writeln!(out, "{ind}{ty} {name}_gray;").unwrap();
            for i in 0..stages {
                writeln!(out, "{ind}(* ASYNC_REG = \"TRUE\" *)").unwrap();
                writeln!(out, "{ind}{ty} {name}_gray_sync_{i};").unwrap();
            }
            // Binary to Gray conversion
            writeln!(
                out,
                "{ind}assign {name}_gray = {name}_bin_in ^ ({name}_bin_in >> 1);"
            )
            .unwrap();
            // Gray to Binary decode (XOR chain)
            if width > 0 {
                writeln!(
                    out,
                    "{ind}assign {name}[{msb}] = {name}_gray_sync_{last}[{msb}];",
                    msb = width - 1,
                    last = stages - 1
                )
                .unwrap();
                for i in (0..width - 1).rev() {
                    writeln!(
                        out,
                        "{ind}assign {name}[{i}] = {name}[{next}] ^ {name}_gray_sync_{last}[{i}];",
                        next = i + 1,
                        last = stages - 1
                    )
                    .unwrap();
                }
            }
        }
        CdcType::Pulse => {
            // Toggle + sync + edge detect
            writeln!(out, "{ind}logic {name}_toggle;").unwrap();
            for i in 0..stages {
                writeln!(out, "{ind}(* ASYNC_REG = \"TRUE\" *)").unwrap();
                writeln!(out, "{ind}logic {name}_toggle_sync_{i};").unwrap();
            }
            writeln!(out, "{ind}logic {name}_toggle_prev;").unwrap();
            writeln!(
                out,
                "{ind}assign {name} = {name}_toggle_sync_{} ^ {name}_toggle_prev;",
                stages - 1
            )
            .unwrap();
        }
        CdcType::Handshake => {
            // Req/ack synchronizer chains + data holding register
            writeln!(out, "{ind}logic {name}_req;").unwrap();
            for i in 0..stages {
                writeln!(out, "{ind}(* ASYNC_REG = \"TRUE\" *)").unwrap();
                writeln!(out, "{ind}logic {name}_req_sync_{i};").unwrap();
            }
            writeln!(out, "{ind}logic {name}_ack;").unwrap();
            for i in 0..stages {
                writeln!(out, "{ind}(* ASYNC_REG = \"TRUE\" *)").unwrap();
                writeln!(out, "{ind}logic {name}_ack_sync_{i};").unwrap();
            }
            writeln!(out, "{ind}{ty} {name}_data;").unwrap();
            writeln!(out, "{ind}assign {name} = {name}_data;").unwrap();
        }
        CdcType::AsyncFifo => {
            writeln!(
                out,
                "{ind}// TODO: Async FIFO synchronizer for {name} requires external module"
            )
            .unwrap();
            writeln!(
                out,
                "{ind}// Instantiate an async FIFO IP or use a parameterized module"
            )
            .unwrap();
        }
    }
}

/// Get the bit width of a type (for CDC Gray code computation).
fn type_width(ty: &HirType) -> usize {
    match ty {
        HirType::Bit(n) | HirType::Logic(n) | HirType::Nat(n) | HirType::Int(n) => *n as usize,
        _ => 1,
    }
}

/// Emit synthesis attributes for power intent before a signal declaration.
fn emit_sv_power_attributes(out: &mut String, signal: &HirSignal, level: usize) {
    let ind = indent(level);

    // Power domain comment
    if let Some(ref domain) = signal.power_domain {
        writeln!(out, "{ind}// Power domain: {domain}").unwrap();
    }

    if let Some(ref pc) = signal.power_config {
        if let Some(ref domain_name) = pc.domain_name {
            writeln!(out, "{ind}(* power_domain = \"{domain_name}\" *)").unwrap();
        }

        // Retention attributes
        if let Some(ref ret) = pc.retention {
            writeln!(out, "{ind}(* RETAIN = \"TRUE\" *)").unwrap();
            writeln!(out, "{ind}(* preserve = \"true\" *)").unwrap();
            writeln!(out, "{ind}(* DONT_TOUCH = \"TRUE\" *)").unwrap();
            let strategy = match ret.strategy {
                RetentionStrategy::Auto => "auto",
                RetentionStrategy::BalloonLatch => "balloon_latch",
                RetentionStrategy::ShadowRegister => "shadow_register",
            };
            writeln!(out, "{ind}// Retention strategy: {strategy}").unwrap();
            if let Some(ref save) = ret.save_signal {
                writeln!(out, "{ind}// Save signal: {save}").unwrap();
            }
            if let Some(ref restore) = ret.restore_signal {
                writeln!(out, "{ind}// Restore signal: {restore}").unwrap();
            }
        }

        // Level shifter attributes
        if let Some(ref ls) = pc.level_shift {
            writeln!(out, "{ind}(* LEVEL_SHIFTER = \"TRUE\" *)").unwrap();
            let ls_type = match ls.shifter_type {
                LevelShifterType::Auto => "auto",
                LevelShifterType::LowToHigh => "low_to_high",
                LevelShifterType::HighToLow => "high_to_low",
            };
            writeln!(out, "{ind}// Level shifter: type={ls_type}").unwrap();
            if let Some(ref from) = ls.from_domain {
                writeln!(out, "{ind}// Level shift from domain: {from}").unwrap();
            }
            if let Some(ref to) = ls.to_domain {
                writeln!(out, "{ind}// Level shift to domain: {to}").unwrap();
            }
        }

        // Isolation attributes (logic is emitted separately)
        if let Some(ref iso) = pc.isolation {
            let clamp = match iso.clamp {
                IsolationClamp::Low => "low",
                IsolationClamp::High => "high",
                IsolationClamp::Latch => "latch",
            };
            writeln!(
                out,
                "{ind}// Power Intent: {} isolation clamp={clamp}",
                signal.name
            )
            .unwrap();
            if let Some(ref en) = iso.enable_signal {
                let polarity = if iso.active_high {
                    "active-high"
                } else {
                    "active-low"
                };
                writeln!(out, "{ind}// Isolation enable: {en} ({polarity})").unwrap();
            }
        }
    }
}

/// Emit isolation mux logic for a power-isolated signal.
fn emit_sv_isolation_logic(out: &mut String, signal: &HirSignal, level: usize) {
    let ind = indent(level);
    if let Some(ref pc) = signal.power_config {
        if let Some(ref iso) = pc.isolation {
            if let Some(ref enable) = iso.enable_signal {
                let ty = emit_type(&signal.signal_type);
                let isolated = format!("{}_isolated", signal.name);
                writeln!(out, "{ind}{ty} {isolated};").unwrap();
                let clamp_val = match iso.clamp {
                    IsolationClamp::Low => "'0",
                    IsolationClamp::High => "'1",
                    IsolationClamp::Latch => {
                        // Latch: no clamp, just comment
                        writeln!(
                            out,
                            "{ind}// Latch isolation: {isolated} holds last value when {enable} active"
                        )
                        .unwrap();
                        return;
                    }
                };
                if iso.active_high {
                    writeln!(
                        out,
                        "{ind}assign {isolated} = {enable} ? {clamp_val} : {};",
                        signal.name
                    )
                    .unwrap();
                } else {
                    writeln!(
                        out,
                        "{ind}assign {isolated} = {enable} ? {} : {clamp_val};",
                        signal.name
                    )
                    .unwrap();
                }
            }
        }
    }
}

/// Emit a breakpoint assertion block for a signal.
fn emit_sv_breakpoint(out: &mut String, signal: &HirSignal, level: usize) {
    let ind = indent(level);
    if let Some(ref bp) = signal.breakpoint_config {
        let bp_name = bp.name.as_deref().unwrap_or(&signal.name);
        let condition = bp.condition.as_deref().unwrap_or(&signal.name);
        let msg = bp.message.as_deref().unwrap_or("triggered");

        writeln!(
            out,
            "{ind}// Breakpoint: {bp_name} (condition: {condition})"
        )
        .unwrap();

        if bp.is_error {
            writeln!(
                out,
                "{ind}always @(*) if ({condition}) begin $error(\"BREAKPOINT [{bp_name}]: {msg}\"); $stop; end"
            )
            .unwrap();
        } else {
            writeln!(
                out,
                "{ind}always @(*) if ({condition}) begin $display(\"BREAKPOINT [{bp_name}]: {msg}\"); $stop; end"
            )
            .unwrap();
        }
    }
}

/// Emit an SVA formal verification block.
fn emit_sv_formal_block(
    out: &mut String,
    fb: &HirFormalBlock,
    resolver: &NameResolver,
    level: usize,
) {
    let ind = indent(level);
    if let Some(ref name) = fb.name {
        writeln!(out, "{ind}// Formal verification block: {name}").unwrap();
    }

    // Emit assumptions
    for assumption in &fb.assumptions {
        let prop = emit_sv_property(assumption, resolver);
        writeln!(out, "{ind}assume property ({prop});").unwrap();
    }

    // Emit properties
    for fp in &fb.properties {
        let prop = emit_sv_property(&fp.property, resolver);
        let kw = match fp.property_type {
            HirFormalPropertyType::Safety | HirFormalPropertyType::Invariant => "assert",
            HirFormalPropertyType::Liveness => "assert",
            HirFormalPropertyType::Bounded => "assert",
        };
        if let Some(ref clock) = fp.clock_domain {
            let edge = emit_sv_clock_edge(clock, resolver);
            writeln!(
                out,
                "{ind}{}_prop: {kw} property (@({edge}) {prop});",
                fp.name
            )
            .unwrap();
        } else {
            writeln!(out, "{ind}{}_prop: {kw} property ({prop});", fp.name).unwrap();
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
