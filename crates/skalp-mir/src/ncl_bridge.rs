//! Handshake bridge between clocked modules and `async` (NCL) instances
//!
//! Every `async` entity has two implicit ports (see
//! `skalp_frontend::hir::NCL_REQ_PORT` / `NCL_ACK_PORT`): a request input and
//! an acknowledge output. A clocked parent drives an async instance with a
//! four-phase handshake: it sets the instance's data inputs, raises
//! `ncl_req`, waits for `ncl_ack`, reads the outputs, lowers `ncl_req` and
//! waits for `ncl_ack` to fall before changing the inputs again.
//!
//! This pass makes that handshake safe in hardware:
//!
//! * the request is registered once more in the parent's clock domain, so the
//!   data inputs (set in the same cycle as the request) have a full clock
//!   period to settle before the async instance sees the request;
//! * the acknowledge is synchronised into the parent's clock domain with two
//!   flip-flops, since it comes from a clockless block.
//!
//! It also gives every `async` module a behavioural acknowledge
//! (`ncl_ack = ncl_req`) when the design does not drive it. Synthesis replaces
//! that with a matched-delay completion signal (see `skalp_lir::ncl_expand`).

use crate::mir::{
    Assignment, AssignmentKind, Block, ContinuousAssign, DataType, EdgeSensitivity, EdgeType,
    Expression, ExpressionKind, LValue, Mir, Module, ModuleId, PortDirection, Process, ProcessId,
    ProcessKind, SensitivityList, Signal, SignalId, Statement, Value,
};
use skalp_frontend::hir::{NCL_ACK_PORT, NCL_REQ_PORT};
use std::collections::HashMap;

/// Signal name suffixes of the bridge registers added to a clocked parent
const REQ_REG_SUFFIX: &str = "__ncl_req_q";
const ACK_RAW_SUFFIX: &str = "__ncl_ack_raw";
const ACK_SYNC_SUFFIX: &str = "__ncl_ack_s1";

/// Apply the handshake bridge to every module of `mir`.
///
/// Returns one message per problem found in a module whose name is in
/// `reachable` (an empty set checks every module).
pub fn apply_ncl_bridges(
    mir: &mut Mir,
    reachable: &std::collections::HashSet<String>,
) -> Vec<String> {
    let mut errors = Vec::new();

    for module in mir.modules.iter_mut().filter(|m| m.is_async) {
        add_behavioural_ack(module);
    }

    let async_modules: HashMap<ModuleId, String> = mir
        .modules
        .iter()
        .filter(|m| m.is_async)
        .map(|m| (m.id, m.name.clone()))
        .collect();
    if async_modules.is_empty() {
        return errors;
    }

    for module in mir.modules.iter_mut().filter(|m| m.is_async) {
        share_parent_request(module, &async_modules);
    }

    for module in mir.modules.iter_mut().filter(|m| !m.is_async) {
        let check = reachable.is_empty() || reachable.contains(&module.name);
        if let Err(e) = bridge_module(module, &async_modules) {
            if check {
                errors.extend(e);
            }
        }
    }
    errors
}

/// An async instance inside an async module is part of its parent's block
/// (it coalesces): its `on()` registers step with the parent's, once per
/// parent request. Connect its unconnected `ncl_req` to the parent's, so the
/// behavioural model steps it on the same edge and synthesis clocks it by the
/// parent's `go` strobe (see `skalp_lir::ncl_expand`).
fn share_parent_request(module: &mut Module, async_modules: &HashMap<ModuleId, String>) {
    let Some(req) = module
        .ports
        .iter()
        .find(|p| p.name == NCL_REQ_PORT && p.direction == PortDirection::Input)
        .map(|p| p.id)
    else {
        return;
    };
    for inst in module
        .instances
        .iter_mut()
        .filter(|i| async_modules.contains_key(&i.module))
    {
        inst.connections
            .entry(NCL_REQ_PORT.to_string())
            .or_insert_with(|| {
                Expression::with_unknown_type(ExpressionKind::Ref(LValue::Port(req)))
            });
    }
}

/// Drive `ncl_ack` from `ncl_req` in an async module that leaves it undriven
fn add_behavioural_ack(module: &mut Module) {
    let Some(ack) = module
        .ports
        .iter()
        .find(|p| p.name == NCL_ACK_PORT && p.direction == PortDirection::Output)
        .map(|p| p.id)
    else {
        return;
    };
    let Some(req) = module
        .ports
        .iter()
        .find(|p| p.name == NCL_REQ_PORT && p.direction == PortDirection::Input)
        .map(|p| p.id)
    else {
        return;
    };
    let driven = module
        .assignments
        .iter()
        .any(|a| lvalue_is_port(&a.lhs, ack))
        || module
            .processes
            .iter()
            .any(|p| block_drives_port(&p.body, ack));
    if !driven {
        module.assignments.push(ContinuousAssign {
            lhs: LValue::Port(ack),
            rhs: Expression::with_unknown_type(ExpressionKind::Ref(LValue::Port(req))),
            span: None,
        });
    }
}

/// The behavioural model of an `async` module's `on()` block: its registers
/// take one write per request, at the rising edge of `ncl_req` (in hardware,
/// the bundled-data `go` strobe, a matched delay after the request). `None`
/// for any other process, or a module without the request port.
pub fn handshake_clocked_process(module: &Module, process: &Process) -> Option<Process> {
    if !module.is_async || process.kind != ProcessKind::Async {
        return None;
    }
    if !block_has_nonblocking(&process.body) {
        return None;
    }
    let req = module
        .ports
        .iter()
        .find(|p| p.name == NCL_REQ_PORT && p.direction == PortDirection::Input)?;
    Some(Process {
        id: process.id,
        kind: ProcessKind::Sequential,
        sensitivity: SensitivityList::Edge(vec![EdgeSensitivity {
            signal: LValue::Port(req.id),
            edge: EdgeType::Rising,
        }]),
        body: process.body.clone(),
        span: process.span.clone(),
    })
}

fn block_has_nonblocking(block: &Block) -> bool {
    block.statements.iter().any(|s| match s {
        Statement::Assignment(a) => a.kind == AssignmentKind::NonBlocking,
        Statement::Block(b) => block_has_nonblocking(b),
        Statement::If(i) => {
            block_has_nonblocking(&i.then_block)
                || i.else_block.as_ref().is_some_and(block_has_nonblocking)
        }
        Statement::Case(c) => {
            c.items
                .iter()
                .any(|item| block_has_nonblocking(&item.block))
                || c.default.as_ref().is_some_and(block_has_nonblocking)
        }
        _ => false,
    })
}

fn lvalue_is_port(lv: &LValue, port: crate::mir::PortId) -> bool {
    matches!(lv, LValue::Port(p) if *p == port)
}

fn block_drives_port(block: &Block, port: crate::mir::PortId) -> bool {
    block.statements.iter().any(|s| match s {
        Statement::Assignment(a) => lvalue_is_port(&a.lhs, port),
        Statement::Block(b) => block_drives_port(b, port),
        Statement::If(i) => {
            block_drives_port(&i.then_block, port)
                || i.else_block
                    .as_ref()
                    .is_some_and(|b| block_drives_port(b, port))
        }
        _ => false,
    })
}

/// The single rising-edge clock of a clocked module's processes, if there is
/// exactly one
fn single_clock(module: &Module) -> Result<Option<LValue>, Vec<LValue>> {
    let mut clocks: Vec<LValue> = Vec::new();
    for process in &module.processes {
        if let SensitivityList::Edge(edges) = &process.sensitivity {
            for e in edges {
                if e.edge == EdgeType::Rising && !clocks.contains(&e.signal) {
                    clocks.push(e.signal.clone());
                }
            }
        }
    }
    match clocks.len() {
        0 => Ok(None),
        1 => Ok(clocks.pop()),
        _ => Err(clocks),
    }
}

fn bridge_module(
    module: &mut Module,
    async_modules: &HashMap<ModuleId, String>,
) -> Result<(), Vec<String>> {
    let async_instances: Vec<usize> = module
        .instances
        .iter()
        .enumerate()
        .filter(|(_, i)| async_modules.contains_key(&i.module))
        .map(|(idx, _)| idx)
        .collect();
    if async_instances.is_empty() {
        return Ok(());
    }

    let mut errors = Vec::new();
    let clock = match single_clock(module) {
        Ok(Some(clk)) => clk,
        Ok(None) => {
            for &idx in &async_instances {
                let inst = &module.instances[idx];
                errors.push(format!(
                    "in `{}`: async instance `{}` of `{}` needs a clocked parent to drive its `{}`/`{}` handshake, but `{}` has no clocked process",
                    module.name,
                    inst.name,
                    async_modules[&inst.module],
                    NCL_REQ_PORT,
                    NCL_ACK_PORT,
                    module.name
                ));
            }
            return Err(errors);
        }
        Err(_) => {
            for &idx in &async_instances {
                let inst = &module.instances[idx];
                errors.push(format!(
                    "in `{}`: async instance `{}` of `{}` is in a module with several clocks; put it in a module with one clock so its `{}`/`{}` handshake has one clock domain",
                    module.name,
                    inst.name,
                    async_modules[&inst.module],
                    NCL_REQ_PORT,
                    NCL_ACK_PORT
                ));
            }
            return Err(errors);
        }
    };

    // Clock domain of the parent's existing registers on this clock, so the
    // bridge registers land in the same domain
    let clock_domain = module.signals.iter().find_map(|s| s.clock_domain);

    let mut statements: Vec<Statement> = Vec::new();
    let mut next_signal = module.signals.iter().map(|s| s.id.0 + 1).max().unwrap_or(0);

    for idx in async_instances {
        let inst_name = module.instances[idx].name.clone();
        let target = async_modules[&module.instances[idx].module].clone();

        let Some(req_expr) = module.instances[idx].connections.get(NCL_REQ_PORT).cloned() else {
            errors.push(format!(
                "in `{}`: async instance `{}` of `{}` is under a clocked parent but its `{}` is not connected; raise `{}` once its inputs are set and wait for `{}.{}`",
                module.name, inst_name, target, NCL_REQ_PORT, NCL_REQ_PORT, inst_name, NCL_ACK_PORT
            ));
            continue;
        };

        let mut new_signal = |module: &mut Module, suffix: &str| -> SignalId {
            let id = SignalId(next_signal);
            next_signal += 1;
            module.signals.push(Signal {
                id,
                name: format!("{}{}", inst_name, suffix),
                signal_type: DataType::Bit(1),
                initial: Some(Value::Integer(0)),
                clock_domain,
                clock_domain_name: None,
                span: None,
                memory_config: None,
                trace_config: None,
                cdc_config: None,
                breakpoint_config: None,
                power_config: None,
                safety_context: None,
                detection_config: None,
                power_domain: None,
            });
            id
        };

        // Request: one more register stage in the parent's domain
        let req_q = new_signal(module, REQ_REG_SUFFIX);
        statements.push(nonblocking(LValue::Signal(req_q), req_expr));
        module.instances[idx].connections.insert(
            NCL_REQ_PORT.to_string(),
            Expression::with_unknown_type(ExpressionKind::Ref(LValue::Signal(req_q))),
        );

        // Acknowledge: two-flop synchroniser into the parent's domain, only
        // when the parent reads it
        if let Some(read) = module.instances[idx].connections.get(NCL_ACK_PORT).cloned() {
            let ExpressionKind::Ref(read_lv) = read.kind else {
                errors.push(format!(
                    "in `{}`: `{}.{}` is connected to an expression; read it as a signal",
                    module.name, inst_name, NCL_ACK_PORT
                ));
                continue;
            };
            let raw = new_signal(module, ACK_RAW_SUFFIX);
            let s1 = new_signal(module, ACK_SYNC_SUFFIX);
            module.instances[idx].connections.insert(
                NCL_ACK_PORT.to_string(),
                Expression::with_unknown_type(ExpressionKind::Ref(LValue::Signal(raw))),
            );
            statements.push(nonblocking(
                LValue::Signal(s1),
                Expression::with_unknown_type(ExpressionKind::Ref(LValue::Signal(raw))),
            ));
            statements.push(nonblocking(
                read_lv,
                Expression::with_unknown_type(ExpressionKind::Ref(LValue::Signal(s1))),
            ));
        }
    }

    if !statements.is_empty() {
        let id = ProcessId(
            module
                .processes
                .iter()
                .map(|p| p.id.0 + 1)
                .max()
                .unwrap_or(0),
        );
        module.processes.push(Process {
            id,
            kind: ProcessKind::Sequential,
            sensitivity: SensitivityList::Edge(vec![EdgeSensitivity {
                signal: clock,
                edge: EdgeType::Rising,
            }]),
            body: Block { statements },
            span: None,
        });
    }

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

fn nonblocking(lhs: LValue, rhs: Expression) -> Statement {
    Statement::Assignment(Assignment {
        lhs,
        rhs,
        kind: AssignmentKind::NonBlocking,
        span: None,
    })
}
