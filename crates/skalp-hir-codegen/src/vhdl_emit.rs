//! HIR → VHDL source code emitter

use crate::skalp_emit::emit_expression_inline as sk_expr;
use crate::{emit_comments, indent, NameResolver};
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
            // IEEE library preamble
            writeln!(out, "library ieee;").unwrap();
            writeln!(out, "use ieee.std_logic_1164.all;").unwrap();
            writeln!(out, "use ieee.numeric_std.all;").unwrap();
            writeln!(out).unwrap();
            emit_comments(&mut out, &entity.comments, "--", "");
            emit_entity(&mut out, entity, resolver);
            if let Some(ref vendor_ip) = entity.vendor_ip_config {
                writeln!(out).unwrap();
                emit_vhdl_vendor_ip_wrapper(&mut out, entity, vendor_ip, resolver, 1);
            } else if let Some(imp) = hir
                .implementations
                .iter()
                .find(|imp| imp.entity == entity.id)
            {
                writeln!(out).unwrap();
                emit_architecture(&mut out, imp, entity, resolver);
            }
            Ok(crate::GeneratedFile {
                name: entity.name.clone(),
                code: out,
            })
        })
        .collect()
}

fn emit_entity(out: &mut String, entity: &HirEntity, resolver: &NameResolver) {
    emit_comments(out, &entity.comments, "--", "");

    write!(out, "entity {} is", entity.name).unwrap();

    // Generics
    if !entity.generics.is_empty() {
        writeln!(out).unwrap();
        writeln!(out, "    generic (").unwrap();
        let generics: Vec<_> = entity
            .generics
            .iter()
            .filter(|g| {
                matches!(
                    g.param_type,
                    HirGenericType::Const(_) | HirGenericType::Width
                )
            })
            .collect();
        for (i, generic) in generics.iter().enumerate() {
            let ty = match &generic.param_type {
                HirGenericType::Width => "natural".to_string(),
                HirGenericType::Const(t) => emit_type(t),
                _ => "natural".to_string(),
            };
            let sep = if i + 1 < generics.len() { ";" } else { "" };
            let ind = indent(2);
            if let Some(ref default) = generic.default_value {
                writeln!(
                    out,
                    "{ind}{} : {ty} := {}{sep}",
                    generic.name,
                    emit_expr(default, resolver)
                )
                .unwrap();
            } else {
                writeln!(out, "{ind}{} : {ty}{sep}", generic.name).unwrap();
            }
        }
        writeln!(out, "    );").unwrap();
    }

    // Ports
    if !entity.ports.is_empty() {
        writeln!(out).unwrap();
        writeln!(out, "    port (").unwrap();
        for (i, port) in entity.ports.iter().enumerate() {
            emit_comments(out, &port.comments, "--", &indent(2));
            let ind = indent(2);
            let dir = match port.direction {
                HirPortDirection::Input => "in",
                HirPortDirection::Output => "out",
                HirPortDirection::Bidirectional => "inout",
                HirPortDirection::Protocol => "inout",
            };
            let ty = emit_type(&port.port_type);
            let sep = if i + 1 < entity.ports.len() { ";" } else { "" };
            writeln!(out, "{ind}{} : {dir} {ty}{sep}", port.name).unwrap();
        }
        writeln!(out, "    );").unwrap();
    }

    writeln!(out, "end entity {};", entity.name).unwrap();
}

fn emit_architecture(
    out: &mut String,
    imp: &HirImplementation,
    entity: &HirEntity,
    resolver: &NameResolver,
) {
    writeln!(out, "architecture rtl of {} is", entity.name).unwrap();

    // Entity-level signal declarations
    for signal in &entity.signals {
        let ind = indent(1);
        emit_comments(out, &signal.comments, "--", &ind);
        emit_vhdl_power_attributes(out, signal, 1);
        let ty = emit_type(&signal.signal_type);
        if let Some(ref init) = signal.initial_value {
            writeln!(
                out,
                "{ind}signal {} : {ty} := {};",
                signal.name,
                emit_expr(init, resolver)
            )
            .unwrap();
        } else {
            writeln!(out, "{ind}signal {} : {ty};", signal.name).unwrap();
        }
    }

    // Signal declarations (from implementation)
    for signal in &imp.signals {
        let ind = indent(1);
        emit_comments(out, &signal.comments, "--", &ind);
        // Emit power attributes as VHDL attributes/comments
        emit_vhdl_power_attributes(out, signal, 1);
        let ty = emit_type(&signal.signal_type);
        if let Some(ref init) = signal.initial_value {
            writeln!(
                out,
                "{ind}signal {} : {ty} := {};",
                signal.name,
                emit_expr(init, resolver)
            )
            .unwrap();
        } else {
            writeln!(out, "{ind}signal {} : {ty};", signal.name).unwrap();
        }
    }

    // Variable declarations
    for variable in &imp.variables {
        let ind = indent(1);
        emit_comments(out, &variable.comments, "--", &ind);
        let ty = emit_type(&variable.var_type);
        writeln!(out, "{ind}variable {} : {ty};", variable.name).unwrap();
    }

    // Constants
    for constant in &imp.constants {
        let ind = indent(1);
        emit_comments(out, &constant.comments, "--", &ind);
        let ty = emit_type(&constant.const_type);
        writeln!(
            out,
            "{ind}constant {} : {ty} := {};",
            constant.name,
            emit_expr(&constant.value, resolver)
        )
        .unwrap();
    }

    writeln!(out, "begin").unwrap();

    // Entity-level breakpoint assertions
    let entity_bp_signals: Vec<_> = entity
        .signals
        .iter()
        .filter(|s| s.breakpoint_config.is_some())
        .collect();
    if !entity_bp_signals.is_empty() {
        let ind = indent(1);
        writeln!(out, "{ind}-- Debug Breakpoint Assertions").unwrap();
        for signal in &entity_bp_signals {
            if let Some(ref bp) = signal.breakpoint_config {
                let bp_name = bp.name.as_deref().unwrap_or(&signal.name);
                let condition = bp.condition.as_deref().unwrap_or(&signal.name);
                let msg = bp.message.as_deref().unwrap_or("triggered");
                let severity = if bp.is_error { "failure" } else { "error" };
                writeln!(
                    out,
                    "{ind}assert not ({condition}) report \"BREAKPOINT [{bp_name}]: {msg}\" severity {severity};"
                )
                .unwrap();
            }
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
        writeln!(out, "{ind}-- CDC Synchronizer Logic").unwrap();
        for signal in &entity_cdc_signals {
            emit_vhdl_cdc_synchronizer(out, signal, 1);
        }
    }

    // Breakpoint assertions (from implementation)
    let breakpoint_signals: Vec<_> = imp
        .signals
        .iter()
        .filter(|s| s.breakpoint_config.is_some())
        .collect();
    if !breakpoint_signals.is_empty() {
        let ind = indent(1);
        writeln!(out, "{ind}-- Debug Breakpoint Assertions").unwrap();
        for signal in &breakpoint_signals {
            if let Some(ref bp) = signal.breakpoint_config {
                let bp_name = bp.name.as_deref().unwrap_or(&signal.name);
                let condition = bp.condition.as_deref().unwrap_or(&signal.name);
                let msg = bp.message.as_deref().unwrap_or("triggered");
                let severity = if bp.is_error { "failure" } else { "error" };
                writeln!(
                    out,
                    "{ind}assert not ({condition}) report \"BREAKPOINT [{bp_name}]: {msg}\" severity {severity};"
                )
                .unwrap();
            }
        }
        writeln!(out).unwrap();
    }

    // Continuous assignments
    for assign in &imp.assignments {
        let ind = indent(1);
        emit_comments(out, &assign.comments, "--", &ind);
        let lhs = emit_lvalue(&assign.lhs, resolver);
        let rhs = emit_expr(&assign.rhs, resolver);
        writeln!(out, "{ind}{lhs} <= {rhs};").unwrap();
    }

    // Instances
    for inst in &imp.instances {
        emit_instance(out, inst, resolver, 1);
    }

    // Event blocks (processes)
    for eb in &imp.event_blocks {
        emit_process(out, eb, resolver, 1);
    }

    // CDC synchronizer processes
    let cdc_signals: Vec<_> = imp
        .signals
        .iter()
        .filter(|s| s.cdc_config.is_some())
        .collect();
    if !cdc_signals.is_empty() {
        let ind = indent(1);
        writeln!(out, "{ind}-- CDC Synchronizer Logic").unwrap();
        for signal in &cdc_signals {
            emit_vhdl_cdc_synchronizer(out, signal, 1);
        }
    }

    // Formal verification blocks (as PSL comments)
    for fb in &imp.formal_blocks {
        emit_vhdl_formal_block(out, fb, resolver, 1);
    }

    writeln!(out, "end architecture rtl;").unwrap();
}

fn emit_process(out: &mut String, eb: &HirEventBlock, resolver: &NameResolver, level: usize) {
    let ind = indent(level);
    emit_comments(out, &eb.comments, "--", &ind);

    // Build sensitivity list from triggers
    let sensitivity: Vec<String> = eb
        .triggers
        .iter()
        .map(|t| match &t.signal {
            HirEventSignal::Port(id) => resolver.port_name(*id).to_string(),
            HirEventSignal::Signal(id) => resolver.signal_name(*id).to_string(),
        })
        .collect();

    writeln!(out, "{ind}process({})", sensitivity.join(", ")).unwrap();
    writeln!(out, "{ind}begin").unwrap();

    // Check for clock edge and async reset
    let clock_trigger = eb
        .triggers
        .iter()
        .find(|t| matches!(t.edge, HirEdgeType::Rising | HirEdgeType::Falling));
    let reset_trigger = eb
        .triggers
        .iter()
        .find(|t| matches!(t.edge, HirEdgeType::Active | HirEdgeType::Inactive));

    if let (Some(clk), Some(_rst)) = (clock_trigger, reset_trigger) {
        // Async reset pattern: the HIR body already contains the if/else structure
        // for reset vs clock. We just need the enclosing clock edge check for VHDL.
        // The inner if/else handles the async reset condition.
        let clk_name = match &clk.signal {
            HirEventSignal::Port(id) => resolver.port_name(*id).to_string(),
            HirEventSignal::Signal(id) => resolver.signal_name(*id).to_string(),
        };
        let edge_fn = match clk.edge {
            HirEdgeType::Rising => "rising_edge",
            HirEdgeType::Falling => "falling_edge",
            _ => "rising_edge",
        };

        // Emit the body directly — it contains the if (reset) ... else ... pattern
        // For VHDL async reset, we emit it as-is; the first `if` tests reset,
        // and we transform the `else` to `elsif rising_edge(clk) then`
        emit_vhdl_async_reset_body(out, &eb.statements, resolver, level + 1, edge_fn, &clk_name);
    } else if let Some(clk) = clock_trigger {
        // Synchronous only
        let clk_name = match &clk.signal {
            HirEventSignal::Port(id) => resolver.port_name(*id).to_string(),
            HirEventSignal::Signal(id) => resolver.signal_name(*id).to_string(),
        };
        let edge_fn = match clk.edge {
            HirEdgeType::Rising => "rising_edge",
            HirEdgeType::Falling => "falling_edge",
            _ => "rising_edge",
        };
        let ind2 = indent(level + 1);
        writeln!(out, "{ind2}if {edge_fn}({clk_name}) then").unwrap();
        for stmt in &eb.statements {
            emit_vhdl_statement(out, stmt, resolver, level + 2);
        }
        writeln!(out, "{ind2}end if;").unwrap();
    } else {
        // Combinational process
        for stmt in &eb.statements {
            emit_vhdl_statement(out, stmt, resolver, level + 1);
        }
    }

    writeln!(out, "{ind}end process;").unwrap();
}

/// Emit VHDL async reset body. If the body starts with an if statement (the reset check),
/// transform it to VHDL's `if rst then ... elsif rising_edge(clk) then ... end if;` pattern.
fn emit_vhdl_async_reset_body(
    out: &mut String,
    stmts: &[HirStatement],
    resolver: &NameResolver,
    level: usize,
    edge_fn: &str,
    clk_name: &str,
) {
    let ind = indent(level);
    // The body should be a single if statement: if (reset_cond) { reset_stmts } else { clk_stmts }
    if let Some(HirStatement::If(if_stmt)) = stmts.first() {
        let cond = emit_expr(&if_stmt.condition, resolver);
        writeln!(out, "{ind}if {cond} then").unwrap();
        for s in &if_stmt.then_statements {
            emit_vhdl_statement(out, s, resolver, level + 1);
        }
        writeln!(out, "{ind}elsif {edge_fn}({clk_name}) then").unwrap();
        if let Some(ref else_stmts) = if_stmt.else_statements {
            for s in else_stmts {
                emit_vhdl_statement(out, s, resolver, level + 1);
            }
        }
        writeln!(out, "{ind}end if;").unwrap();
    } else {
        // Fallback: just emit statements under clock edge
        writeln!(out, "{ind}if {edge_fn}({clk_name}) then").unwrap();
        for stmt in stmts {
            emit_vhdl_statement(out, stmt, resolver, level + 1);
        }
        writeln!(out, "{ind}end if;").unwrap();
    }
}

/// Emit a VHDL vendor IP wrapper architecture.
fn emit_vhdl_vendor_ip_wrapper(
    out: &mut String,
    entity: &HirEntity,
    config: &VendorIpConfig,
    _resolver: &NameResolver,
    _level: usize,
) {
    let vendor_name = match config.vendor {
        VendorType::Xilinx => "Xilinx (AMD)",
        VendorType::Intel => "Intel (Altera)",
        VendorType::Lattice => "Lattice",
        VendorType::Generic => "Generic",
    };

    writeln!(out, "architecture rtl of {} is", entity.name).unwrap();
    let ind = indent(1);
    writeln!(out, "{ind}-- Vendor IP Wrapper: {}", config.ip_name).unwrap();
    writeln!(out, "{ind}-- Vendor: {vendor_name}").unwrap();
    if let Some(ref lib) = config.library {
        writeln!(out, "{ind}-- Library: {lib}").unwrap();
    }
    writeln!(out, "{ind}-- Generated by Skalp - do not edit manually").unwrap();

    if config.black_box {
        writeln!(out, "begin").unwrap();
        writeln!(out, "{ind}-- Black-box: implementation provided externally").unwrap();
        writeln!(out, "end architecture rtl;").unwrap();
        return;
    }

    // Component declaration
    writeln!(out, "{ind}component {} is", config.ip_name).unwrap();
    if !config.parameters.is_empty() {
        writeln!(out, "{ind}    generic (").unwrap();
        for (i, (name, _val)) in config.parameters.iter().enumerate() {
            let sep = if i + 1 < config.parameters.len() {
                ";"
            } else {
                ""
            };
            writeln!(out, "{ind}        {name} : natural{sep}").unwrap();
        }
        writeln!(out, "{ind}    );").unwrap();
    }
    writeln!(out, "{ind}end component;").unwrap();

    writeln!(out, "begin").unwrap();

    // Build port mapping
    let port_map: std::collections::HashMap<&str, &str> = config
        .port_map
        .iter()
        .map(|(e, ip)| (e.as_str(), ip.as_str()))
        .collect();

    // IP instantiation
    write!(out, "{ind}ip_inst: {}", config.ip_name).unwrap();

    // Generic map
    if !config.parameters.is_empty() {
        writeln!(out).unwrap();
        writeln!(out, "{ind}    generic map (").unwrap();
        for (i, (name, val)) in config.parameters.iter().enumerate() {
            let sep = if i + 1 < config.parameters.len() {
                ","
            } else {
                ""
            };
            writeln!(out, "{ind}        {name} => {val}{sep}").unwrap();
        }
        writeln!(out, "{ind}    )").unwrap();
    }

    // Port map
    writeln!(out).unwrap();
    writeln!(out, "{ind}    port map (").unwrap();
    let mut connections = Vec::new();
    for port in &entity.ports {
        let ip_port = port_map
            .get(port.name.as_str())
            .copied()
            .unwrap_or(&port.name);
        connections.push(format!("{ip_port} => {}", port.name));
    }
    for port in &config.tie_low {
        connections.push(format!("{port} => '0'"));
    }
    for port in &config.tie_high {
        connections.push(format!("{port} => '1'"));
    }
    for port in &config.unconnected {
        connections.push(format!("{port} => open"));
    }
    for (i, conn) in connections.iter().enumerate() {
        let sep = if i + 1 < connections.len() { "," } else { "" };
        writeln!(out, "{ind}        {conn}{sep}").unwrap();
    }
    writeln!(out, "{ind}    );").unwrap();
    writeln!(out, "end architecture rtl;").unwrap();
}

/// Emit a VHDL CDC synchronizer for a signal.
fn emit_vhdl_cdc_synchronizer(out: &mut String, signal: &HirSignal, level: usize) {
    let ind = indent(level);
    let Some(ref cdc) = signal.cdc_config else {
        return;
    };
    let name = &signal.name;
    let stages = cdc.sync_stages;
    let ty = emit_type(&signal.signal_type);
    let from = cdc.from_domain.as_deref().unwrap_or("src");
    let to = cdc.to_domain.as_deref().unwrap_or("dst");

    writeln!(out, "{ind}-- CDC: {name} ({from} -> {to})").unwrap();

    match cdc.cdc_type {
        CdcType::TwoFF => {
            // Declare sync chain signals
            for i in 0..stages {
                writeln!(
                    out,
                    "{ind}-- attribute ASYNC_REG of {name}_sync_{i} : signal is \"TRUE\";"
                )
                .unwrap();
                writeln!(out, "{ind}signal {name}_sync_{i} : {ty};").unwrap();
            }
            writeln!(out, "{ind}{name} <= {name}_sync_{};", stages - 1).unwrap();
        }
        CdcType::Gray => {
            writeln!(out, "{ind}signal {name}_bin_in : {ty};").unwrap();
            writeln!(out, "{ind}signal {name}_gray : {ty};").unwrap();
            for i in 0..stages {
                writeln!(out, "{ind}signal {name}_gray_sync_{i} : {ty};").unwrap();
            }
            writeln!(
                out,
                "{ind}{name}_gray <= {name}_bin_in xor shift_right({name}_bin_in, 1);"
            )
            .unwrap();
        }
        CdcType::Pulse => {
            writeln!(out, "{ind}signal {name}_toggle : std_logic;").unwrap();
            for i in 0..stages {
                writeln!(out, "{ind}signal {name}_toggle_sync_{i} : std_logic;").unwrap();
            }
            writeln!(out, "{ind}signal {name}_toggle_prev : std_logic;").unwrap();
            writeln!(
                out,
                "{ind}{name} <= {name}_toggle_sync_{} xor {name}_toggle_prev;",
                stages - 1
            )
            .unwrap();
        }
        CdcType::Handshake => {
            writeln!(out, "{ind}signal {name}_req : std_logic;").unwrap();
            for i in 0..stages {
                writeln!(out, "{ind}signal {name}_req_sync_{i} : std_logic;").unwrap();
            }
            writeln!(out, "{ind}signal {name}_ack : std_logic;").unwrap();
            for i in 0..stages {
                writeln!(out, "{ind}signal {name}_ack_sync_{i} : std_logic;").unwrap();
            }
            writeln!(out, "{ind}signal {name}_data : {ty};").unwrap();
            writeln!(out, "{ind}{name} <= {name}_data;").unwrap();
        }
        CdcType::AsyncFifo => {
            writeln!(
                out,
                "{ind}-- TODO: Async FIFO synchronizer for {name} requires external module"
            )
            .unwrap();
        }
    }
}

/// Emit VHDL power intent attributes/comments before a signal declaration.
fn emit_vhdl_power_attributes(out: &mut String, signal: &HirSignal, level: usize) {
    let ind = indent(level);

    if let Some(ref domain) = signal.power_domain {
        writeln!(out, "{ind}-- Power domain: {domain}").unwrap();
    }

    if let Some(ref pc) = signal.power_config {
        if let Some(ref ret) = pc.retention {
            let strategy = match ret.strategy {
                RetentionStrategy::Auto => "auto",
                RetentionStrategy::BalloonLatch => "balloon_latch",
                RetentionStrategy::ShadowRegister => "shadow_register",
            };
            writeln!(out, "{ind}-- Retention: strategy={strategy}").unwrap();
            // VHDL attribute for synthesis
            writeln!(
                out,
                "{ind}attribute preserve of {} : signal is \"true\";",
                signal.name
            )
            .unwrap();
        }
        if let Some(ref ls) = pc.level_shift {
            let ls_type = match ls.shifter_type {
                LevelShifterType::Auto => "auto",
                LevelShifterType::LowToHigh => "low_to_high",
                LevelShifterType::HighToLow => "high_to_low",
            };
            writeln!(out, "{ind}-- Level shifter: type={ls_type}").unwrap();
        }
        if let Some(ref iso) = pc.isolation {
            let clamp = match iso.clamp {
                IsolationClamp::Low => "low",
                IsolationClamp::High => "high",
                IsolationClamp::Latch => "latch",
            };
            writeln!(out, "{ind}-- Isolation: clamp={clamp} for {}", signal.name).unwrap();
        }
    }
}

fn emit_instance(out: &mut String, inst: &HirInstance, resolver: &NameResolver, level: usize) {
    let ind = indent(level);
    emit_comments(out, &inst.comments, "--", &ind);
    let entity_name = resolver.entity_name(inst.entity);
    write!(out, "{ind}{}: entity work.{entity_name}", inst.name).unwrap();

    // Generic map
    if !inst.generic_args.is_empty() || !inst.named_generic_args.is_empty() {
        writeln!(out).unwrap();
        writeln!(out, "{ind}    generic map (").unwrap();
        let mut args: Vec<String> = Vec::new();
        for arg in &inst.generic_args {
            args.push(emit_expr(arg, resolver));
        }
        for (name, arg) in &inst.named_generic_args {
            args.push(format!("{name} => {}", emit_expr(arg, resolver)));
        }
        for (i, arg) in args.iter().enumerate() {
            let sep = if i + 1 < args.len() { "," } else { "" };
            writeln!(out, "{ind}        {arg}{sep}").unwrap();
        }
        writeln!(out, "{ind}    )").unwrap();
    }

    // Port map
    if !inst.connections.is_empty() {
        writeln!(out).unwrap();
        writeln!(out, "{ind}    port map (").unwrap();
        for (i, conn) in inst.connections.iter().enumerate() {
            let sep = if i + 1 < inst.connections.len() {
                ","
            } else {
                ""
            };
            writeln!(
                out,
                "{ind}        {} => {}{sep}",
                conn.port,
                emit_expr(&conn.expr, resolver)
            )
            .unwrap();
        }
        writeln!(out, "{ind}    );").unwrap();
    } else {
        writeln!(out, ";").unwrap();
    }
}

fn emit_vhdl_statement(
    out: &mut String,
    stmt: &HirStatement,
    resolver: &NameResolver,
    level: usize,
) {
    let ind = indent(level);
    match stmt {
        HirStatement::Assignment(assign) => {
            emit_comments(out, &assign.comments, "--", &ind);
            let lhs = emit_lvalue(&assign.lhs, resolver);
            let rhs = emit_expr(&assign.rhs, resolver);
            match assign.assignment_type {
                HirAssignmentType::Combinational => {
                    writeln!(out, "{ind}{lhs} <= {rhs};").unwrap();
                }
                HirAssignmentType::NonBlocking => {
                    writeln!(out, "{ind}{lhs} <= {rhs};").unwrap();
                }
                HirAssignmentType::Blocking => {
                    writeln!(out, "{ind}{lhs} := {rhs};").unwrap();
                }
            }
        }
        HirStatement::If(if_stmt) => {
            let cond = emit_expr(&if_stmt.condition, resolver);
            writeln!(out, "{ind}if {cond} then").unwrap();
            for s in &if_stmt.then_statements {
                emit_vhdl_statement(out, s, resolver, level + 1);
            }
            if let Some(ref else_stmts) = if_stmt.else_statements {
                writeln!(out, "{ind}else").unwrap();
                for s in else_stmts {
                    emit_vhdl_statement(out, s, resolver, level + 1);
                }
            }
            writeln!(out, "{ind}end if;").unwrap();
        }
        HirStatement::Match(match_stmt) => {
            let expr = emit_expr(&match_stmt.expr, resolver);
            writeln!(out, "{ind}case {expr} is").unwrap();
            for arm in &match_stmt.arms {
                let pat = emit_vhdl_pattern(&arm.pattern);
                writeln!(out, "{ind}    when {pat} =>").unwrap();
                for s in &arm.statements {
                    emit_vhdl_statement(out, s, resolver, level + 2);
                }
            }
            writeln!(out, "{ind}end case;").unwrap();
        }
        HirStatement::Let(let_stmt) => {
            let ty = emit_type(&let_stmt.var_type);
            let val = emit_expr(&let_stmt.value, resolver);
            writeln!(out, "{ind}variable {} : {ty} := {val};", let_stmt.name).unwrap();
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
            writeln!(
                out,
                "{ind}for {} in {start} to {end} loop",
                for_stmt.iterator
            )
            .unwrap();
            for s in &for_stmt.body {
                emit_vhdl_statement(out, s, resolver, level + 1);
            }
            writeln!(out, "{ind}end loop;").unwrap();
        }
        HirStatement::Block(stmts) => {
            for s in stmts {
                emit_vhdl_statement(out, s, resolver, level);
            }
        }
        HirStatement::Assert(assert_stmt) => {
            let cond = emit_expr(&assert_stmt.condition, resolver);
            let severity = match assert_stmt.severity {
                HirAssertionSeverity::Info => "note",
                HirAssertionSeverity::Warning => "warning",
                HirAssertionSeverity::Error => "error",
                HirAssertionSeverity::Fatal => "failure",
            };
            if let Some(ref msg) = assert_stmt.message {
                writeln!(
                    out,
                    "{ind}assert {cond} report \"{msg}\" severity {severity};"
                )
                .unwrap();
            } else {
                writeln!(
                    out,
                    "{ind}assert {cond} report \"Assertion failed\" severity {severity};"
                )
                .unwrap();
            }
        }
        HirStatement::Assume(assume_stmt) => {
            // VHDL-2008 doesn't have assume; emit as assert with comment
            let cond = emit_expr(&assume_stmt.condition, resolver);
            if let Some(ref msg) = assume_stmt.message {
                writeln!(out, "{ind}-- assume: {msg}").unwrap();
            }
            writeln!(
                out,
                "{ind}assert {cond} report \"Assumption\" severity warning;"
            )
            .unwrap();
        }
        HirStatement::Cover(cover_stmt) => {
            // VHDL doesn't have native cover; emit as comment + PSL pragma
            let prop = emit_vhdl_property(&cover_stmt.property, resolver);
            if let Some(ref name) = cover_stmt.name {
                writeln!(out, "{ind}-- psl cover {{{prop}}}; -- {name}").unwrap();
            } else {
                writeln!(out, "{ind}-- psl cover {{{prop}}};").unwrap();
            }
        }
        HirStatement::Property(prop_stmt) => {
            let prop = emit_vhdl_property(&prop_stmt.property, resolver);
            writeln!(
                out,
                "{ind}-- psl {}: assert always ({prop});",
                prop_stmt.name
            )
            .unwrap();
        }
        _ => {
            writeln!(out, "{ind}-- unsupported statement").unwrap();
        }
    }
}

/// Emit a VHDL formal verification block (as PSL comments).
fn emit_vhdl_formal_block(
    out: &mut String,
    fb: &HirFormalBlock,
    resolver: &NameResolver,
    level: usize,
) {
    let ind = indent(level);
    if let Some(ref name) = fb.name {
        writeln!(out, "{ind}-- Formal verification: {name}").unwrap();
    }

    for assumption in &fb.assumptions {
        let prop = emit_vhdl_property(assumption, resolver);
        writeln!(out, "{ind}-- psl assume always ({prop});").unwrap();
    }

    for fp in &fb.properties {
        let prop = emit_vhdl_property(&fp.property, resolver);
        writeln!(out, "{ind}-- psl {}: assert always ({prop});", fp.name).unwrap();
    }
}

/// Emit a PSL/VHDL-2008 property expression.
fn emit_vhdl_property(prop: &HirProperty, resolver: &NameResolver) -> String {
    match prop {
        HirProperty::Expression(expr) => emit_expr(expr, resolver),
        HirProperty::Implication {
            antecedent,
            consequent,
        } => {
            format!(
                "{} -> {}",
                emit_vhdl_property(antecedent, resolver),
                emit_vhdl_property(consequent, resolver)
            )
        }
        HirProperty::And(a, b) => {
            format!(
                "({} and {})",
                emit_vhdl_property(a, resolver),
                emit_vhdl_property(b, resolver)
            )
        }
        HirProperty::Or(a, b) => {
            format!(
                "({} or {})",
                emit_vhdl_property(a, resolver),
                emit_vhdl_property(b, resolver)
            )
        }
        HirProperty::Not(p) => format!("not ({})", emit_vhdl_property(p, resolver)),
        HirProperty::Always(p) => format!("always ({})", emit_vhdl_property(p, resolver)),
        HirProperty::Eventually(p) => {
            format!("eventually! ({})", emit_vhdl_property(p, resolver))
        }
        _ => "/* unsupported property */".to_string(),
    }
}

fn emit_vhdl_pattern(pat: &HirPattern) -> String {
    match pat {
        HirPattern::Literal(lit) => emit_vhdl_literal(lit),
        HirPattern::Variable(_) => "others".to_string(),
        HirPattern::Wildcard => "others".to_string(),
        HirPattern::Tuple(_) => "others".to_string(),
        HirPattern::Path(_, variant) => variant.clone(),
    }
}

fn emit_vhdl_literal(lit: &HirLiteral) -> String {
    match lit {
        HirLiteral::Integer(n) => n.to_string(),
        HirLiteral::Boolean(b) => {
            if *b {
                "true".to_string()
            } else {
                "false".to_string()
            }
        }
        HirLiteral::Float(f) => format!("{f}"),
        HirLiteral::String(s) => format!("\"{s}\""),
        HirLiteral::BitVector(bits) => {
            let mut s = String::from("\"");
            for b in bits.iter().rev() {
                s.push(if *b { '1' } else { '0' });
            }
            s.push('"');
            s
        }
    }
}

fn emit_expr(expr: &HirExpression, resolver: &NameResolver) -> String {
    match expr {
        HirExpression::Literal(lit) => emit_vhdl_literal(lit),
        HirExpression::Signal(id) => resolver.signal_name(*id).to_string(),
        HirExpression::Port(id) => resolver.port_name(*id).to_string(),
        HirExpression::Variable(id) => resolver.variable_name(*id).to_string(),
        HirExpression::Constant(id) => resolver.constant_name(*id).to_string(),
        HirExpression::GenericParam(name) => name.clone(),
        HirExpression::Binary(bin) => {
            let left = emit_expr(&bin.left, resolver);
            let right = emit_expr(&bin.right, resolver);
            let op = emit_vhdl_binary_op(&bin.op);
            format!("({left} {op} {right})")
        }
        HirExpression::Unary(un) => {
            let operand = emit_expr(&un.operand, resolver);
            match un.op {
                HirUnaryOp::Not | HirUnaryOp::BitwiseNot => format!("not {operand}"),
                HirUnaryOp::Negate => format!("-{operand}"),
                HirUnaryOp::AndReduce => format!("and_reduce({operand})"),
                HirUnaryOp::OrReduce => format!("or_reduce({operand})"),
                HirUnaryOp::XorReduce => format!("xor_reduce({operand})"),
            }
        }
        HirExpression::Call(call) => {
            let args: Vec<_> = call.args.iter().map(|a| emit_expr(a, resolver)).collect();
            format!("{}({})", call.function, args.join(", "))
        }
        HirExpression::Index(base, idx) => {
            format!(
                "{}(to_integer({}))",
                emit_expr(base, resolver),
                emit_expr(idx, resolver)
            )
        }
        HirExpression::Range(base, hi, lo) => {
            format!(
                "{}({} downto {})",
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
            parts.join(" & ")
        }
        HirExpression::Ternary {
            condition,
            true_expr,
            false_expr,
        } => {
            // VHDL doesn't have ternary; use when/else for signal assignments
            format!(
                "{} when {} else {}",
                emit_expr(true_expr, resolver),
                emit_expr(condition, resolver),
                emit_expr(false_expr, resolver)
            )
        }
        HirExpression::Cast(cast) => emit_vhdl_cast(&cast.expr, &cast.target_type, resolver),
        HirExpression::If(if_expr) => {
            format!(
                "{} when {} else {}",
                emit_expr(&if_expr.then_expr, resolver),
                emit_expr(&if_expr.condition, resolver),
                emit_expr(&if_expr.else_expr, resolver)
            )
        }
        HirExpression::Match(match_expr) => {
            // Emit as nested when...else: val1 when (sel = pat1) else val2 when (sel = pat2) else default
            let sel = emit_expr(&match_expr.expr, resolver);
            let mut parts = Vec::new();
            let mut default = None;
            for arm in &match_expr.arms {
                let val = emit_expr(&arm.expr, resolver);
                match &arm.pattern {
                    HirPattern::Wildcard => {
                        default = Some(val);
                    }
                    _ => {
                        let pat = emit_vhdl_pattern(&arm.pattern);
                        parts.push((val, pat));
                    }
                }
            }
            let mut out = String::new();
            for (val, pat) in &parts {
                if !out.is_empty() {
                    out.push_str(" else ");
                }
                out.push_str(&format!("{val} when ({sel} = {pat})"));
            }
            if let Some(def) = default {
                if !out.is_empty() {
                    out.push_str(" else ");
                }
                out.push_str(&def);
            }
            out
        }
        _ => sk_expr(expr, resolver),
    }
}

fn emit_vhdl_cast(expr: &HirExpression, target: &HirType, resolver: &NameResolver) -> String {
    let inner = emit_expr(expr, resolver);
    match target {
        HirType::Nat(_) => format!("unsigned({inner})"),
        HirType::Int(_) => format!("signed({inner})"),
        HirType::Bit(_) => format!("std_logic_vector({inner})"),
        _ => inner,
    }
}

fn emit_vhdl_binary_op(op: &HirBinaryOp) -> &'static str {
    match op {
        HirBinaryOp::Add => "+",
        HirBinaryOp::WidenAdd => "+",
        HirBinaryOp::Sub => "-",
        HirBinaryOp::Mul => "*",
        HirBinaryOp::Div => "/",
        HirBinaryOp::Mod => "mod",
        HirBinaryOp::And => "and",
        HirBinaryOp::Or => "or",
        HirBinaryOp::Xor => "xor",
        HirBinaryOp::Equal => "=",
        HirBinaryOp::NotEqual => "/=",
        HirBinaryOp::Less => "<",
        HirBinaryOp::LessEqual => "<=",
        HirBinaryOp::Greater => ">",
        HirBinaryOp::GreaterEqual => ">=",
        HirBinaryOp::LogicalAnd => "and",
        HirBinaryOp::LogicalOr => "or",
        HirBinaryOp::LeftShift => "sll",
        HirBinaryOp::RightShift => "srl",
    }
}

fn emit_type(ty: &HirType) -> String {
    match ty {
        HirType::Bit(1) => "std_logic".to_string(),
        HirType::Bit(n) => format!("std_logic_vector({} downto 0)", n - 1),
        HirType::Bool => "boolean".to_string(),
        HirType::String => "string".to_string(),
        HirType::Logic(1) => "std_logic".to_string(),
        HirType::Logic(n) => format!("std_logic_vector({} downto 0)", n - 1),
        HirType::Int(n) => format!("signed({} downto 0)", n - 1),
        HirType::Nat(n) => format!("unsigned({} downto 0)", n - 1),
        HirType::Clock(_) => "std_logic".to_string(),
        HirType::Reset { .. } => "std_logic".to_string(),
        HirType::Array(inner, n) => {
            format!("array (0 to {}) of {}", n - 1, emit_type(inner))
        }
        HirType::Custom(name) => name.clone(),
        HirType::Struct(s) => format!("{}_t", s.name.to_lowercase()),
        HirType::Enum(e) => format!("{}_t", e.name.to_lowercase()),
        HirType::Union(u) => format!("{}_t", u.name.to_lowercase()),
        HirType::BitParam(p) => format!("std_logic_vector({p} - 1 downto 0)"),
        HirType::NatParam(p) => format!("unsigned({p} - 1 downto 0)"),
        HirType::IntParam(p) => format!("signed({p} - 1 downto 0)"),
        HirType::LogicParam(p) => format!("std_logic_vector({p} - 1 downto 0)"),
        HirType::BitExpr(e) => {
            let w = sk_expr(e, &NameResolver::from_empty());
            format!("std_logic_vector({w} - 1 downto 0)")
        }
        HirType::NatExpr(e) => {
            let w = sk_expr(e, &NameResolver::from_empty());
            format!("unsigned({w} - 1 downto 0)")
        }
        HirType::IntExpr(e) => {
            let w = sk_expr(e, &NameResolver::from_empty());
            format!("signed({w} - 1 downto 0)")
        }
        _ => format!("-- unsupported type: {:?}", ty),
    }
}

fn emit_lvalue(lval: &HirLValue, resolver: &NameResolver) -> String {
    match lval {
        HirLValue::Signal(id) => resolver.signal_name(*id).to_string(),
        HirLValue::Variable(id) => resolver.variable_name(*id).to_string(),
        HirLValue::Port(id) => resolver.port_name(*id).to_string(),
        HirLValue::Index(base, idx) => {
            format!(
                "{}(to_integer({}))",
                emit_lvalue(base, resolver),
                emit_expr(idx, resolver)
            )
        }
        HirLValue::Range(base, hi, lo) => {
            format!(
                "{}({} downto {})",
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
