//! Pipeline stage insertion for `#[pipeline(stages = N)]`.
//!
//! An entity's latency is the least number of registers on any path from one
//! of its data inputs (not a clock or a reset) to one of its outputs. `stages = N` asks for a latency of N:
//! when the entity has fewer, the missing `N - L` stages are added here, in
//! MIR, so behavioural simulation, compiled simulation, SystemVerilog and
//! synthesis all see the same, longer, latency.
//!
//! The entity keeps its name and ports and becomes a wrapper around its
//! original body (renamed `<name>__core`). The wrapper delays every input
//! except the clock by `N - L` cycles with plain registers and passes the
//! outputs straight through, so it behaves exactly like the original entity
//! run `N - L` cycles later. The reset input is delayed with the others,
//! which keeps that equivalence: the registers start in reset, and a reset
//! reaches the body when the data around it does.
//!
//! Where the added registers end up is a job for retiming after technology
//! mapping, which moves them into the body to balance the real delays.

use crate::mir::{
    Assignment, AssignmentKind, Block, ContinuousAssign, DataType, EdgeSensitivity, EdgeType,
    Expression, ExpressionKind, LValue, Mir, Module, ModuleId, ModuleInstance, PortDirection,
    PortId, Process, ProcessId, ProcessKind, SensitivityList, Signal, SignalId, Statement, Value,
    VariableId,
};
use indexmap::IndexMap;
use std::collections::{HashMap, VecDeque};

/// A value-carrying node of a module: the latency graph's vertices
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
enum Node {
    Port(PortId),
    Signal(SignalId),
    Variable(VariableId),
}

/// Least register count from each input port to each output port, by name
pub type LatencyMatrix = HashMap<(String, String), u32>;

/// Least register count on any path from a data input (not a clock or a
/// reset) to an output of `module`, or `None` when no input reaches an output
pub fn module_latency(mir: &Mir, module: &Module) -> Option<u32> {
    let mut cache = HashMap::new();
    latency_matrix(mir, module, &mut cache)
        .values()
        .min()
        .copied()
}

/// Insert the stages `#[pipeline(stages = N)]` asks for into every module
/// that carries one. Returns the modules changed, with the latency each had
/// and the latency it has now.
pub fn insert_pipeline_stages(mir: &mut Mir) -> Result<Vec<(String, u32, u32)>, String> {
    let mut report = Vec::new();
    let mut errors = Vec::new();
    let candidates: Vec<usize> = mir
        .modules
        .iter()
        .enumerate()
        .filter(|(_, m)| m.pipeline_config.is_some() && m.parameters.is_empty())
        .map(|(i, _)| i)
        .collect();
    for index in candidates {
        let module = &mir.modules[index];
        let config = module.pipeline_config.clone().expect("filtered on Some");
        let current = module_latency(mir, module).unwrap_or(0);
        let wanted = config.stages;
        if wanted < current {
            errors.push(format!(
                "`{}` has #[pipeline(stages = {wanted})] but already has a latency of {current} cycles; stages is the total latency, so it must be at least {current}",
                module.name
            ));
            continue;
        }
        if wanted == current {
            report.push((module.name.clone(), current, wanted));
            continue;
        }
        match wrap_with_delays(mir, index, wanted - current) {
            Ok(()) => report.push((mir.modules[index].name.clone(), current, wanted)),
            Err(e) => errors.push(e),
        }
    }
    if errors.is_empty() {
        Ok(report)
    } else {
        Err(errors.join("\n  "))
    }
}

/// Turn module `index` into a wrapper that delays its inputs by `extra`
/// cycles around a copy of its original body
fn wrap_with_delays(mir: &mut Mir, index: usize, extra: u32) -> Result<(), String> {
    let original = &mir.modules[index];
    let name = original.name.clone();
    let clocks: Vec<&crate::mir::Port> = original
        .ports
        .iter()
        .filter(|p| matches!(p.port_type, DataType::Clock { .. }))
        .collect();
    let clock = match clocks.as_slice() {
        [one] => (*one).clone(),
        [] => {
            return Err(format!(
                "`{name}` has #[pipeline(stages = ...)] but no clock input, so there is no clock to add stages with"
            ))
        }
        _ => {
            return Err(format!(
                "`{name}` has #[pipeline(stages = ...)] and {} clock inputs; adding stages to a multi-clock entity is not supported",
                clocks.len()
            ))
        }
    };
    if let Some(port) = original
        .ports
        .iter()
        .find(|p| p.direction == PortDirection::InOut)
    {
        return Err(format!(
            "`{name}` has #[pipeline(stages = ...)] and an inout port `{}`, which cannot be delayed",
            port.name
        ));
    }
    let edge = clock_edge(original, clock.id);

    // The original body moves to a new module; the wrapper keeps the
    // original's id, so every instance of it, and the top-module choice,
    // now reach the wrapper
    let core_id = ModuleId(mir.modules.iter().map(|m| m.id.0).max().unwrap_or(0) + 1);
    let mut core_name = format!("{name}__core");
    while mir.modules.iter().any(|m| m.name == core_name) {
        core_name.push('_');
    }
    let mut core = original.clone();
    core.id = core_id;
    core.name = core_name;
    core.pipeline_config = None;

    let mut wrapper = Module::new(original.id, name.clone());
    wrapper.ports = original.ports.clone();
    wrapper.span = original.span.clone();
    wrapper.pipeline_config = original.pipeline_config.clone();
    wrapper.is_from_main_source = original.is_from_main_source;
    wrapper.specialization_of = original.specialization_of.clone();
    wrapper.safety_context = original.safety_context.clone();
    wrapper.power_domain_config = original.power_domain_config.clone();

    let mut next_signal = 0u32;
    let mut new_signal = |wrapper: &mut Module, name: String, ty: DataType, init: Option<Value>| {
        let id = SignalId(next_signal);
        next_signal += 1;
        wrapper.signals.push(Signal {
            id,
            name,
            signal_type: ty,
            initial: init,
            clock_domain: None,
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

    let mut statements = Vec::new();
    let mut connections = IndexMap::new();
    for port in &original.ports {
        let reference = |lvalue| Expression::with_unknown_type(ExpressionKind::Ref(lvalue));
        match port.direction {
            PortDirection::Input if port.id == clock.id => {
                connections.insert(port.name.clone(), reference(LValue::Port(port.id)));
            }
            PortDirection::Input => {
                // Delayed copies start at rest: a reset starts asserted, so the
                // body sees reset until the first real input reaches it
                let init = match port.port_type {
                    DataType::Reset { active_high, .. } => Value::Integer(i64::from(active_high)),
                    _ => Value::Integer(0),
                };
                let mut previous = LValue::Port(port.id);
                for stage in 1..=extra {
                    let id = new_signal(
                        &mut wrapper,
                        format!("{}__stage{stage}", port.name),
                        port.port_type.clone(),
                        Some(init.clone()),
                    );
                    statements.push(Statement::Assignment(Assignment {
                        lhs: LValue::Signal(id),
                        rhs: reference(previous),
                        kind: AssignmentKind::NonBlocking,
                        span: None,
                    }));
                    previous = LValue::Signal(id);
                }
                connections.insert(port.name.clone(), reference(previous));
            }
            PortDirection::Output => {
                let id = new_signal(
                    &mut wrapper,
                    format!("{}__core", port.name),
                    port.port_type.clone(),
                    None,
                );
                connections.insert(port.name.clone(), reference(LValue::Signal(id)));
                wrapper.assignments.push(ContinuousAssign {
                    lhs: LValue::Port(port.id),
                    rhs: reference(LValue::Signal(id)),
                    span: None,
                });
            }
            PortDirection::InOut => unreachable!("rejected above"),
        }
    }
    wrapper.processes.push(Process {
        id: ProcessId(0),
        kind: ProcessKind::Sequential,
        sensitivity: SensitivityList::Edge(vec![EdgeSensitivity {
            signal: LValue::Port(clock.id),
            edge,
        }]),
        body: Block { statements },
        span: None,
    });
    wrapper.instances.push(ModuleInstance {
        name: "core".to_string(),
        module: core_id,
        connections,
        parameters: IndexMap::new(),
        span: None,
        safety_context: None,
    });

    mir.modules[index] = wrapper;
    mir.modules.push(core);
    Ok(())
}

/// The edge the module's own registers use on `clock`, rising if none says
fn clock_edge(module: &Module, clock: PortId) -> EdgeType {
    module
        .processes
        .iter()
        .filter_map(|p| match &p.sensitivity {
            SensitivityList::Edge(edges) => edges
                .iter()
                .find(|e| e.signal == LValue::Port(clock))
                .map(|e| e.edge),
            _ => None,
        })
        .next()
        .unwrap_or(EdgeType::Rising)
}

/// Least latency from each input port to each output port of `module`,
/// with the results for instantiated modules cached by id
fn latency_matrix(
    mir: &Mir,
    module: &Module,
    cache: &mut HashMap<ModuleId, LatencyMatrix>,
) -> LatencyMatrix {
    if let Some(found) = cache.get(&module.id) {
        return found.clone();
    }
    // Edges from what a value reads to what it writes, weighted by the
    // registers between them
    let mut edges: HashMap<Node, Vec<(Node, u32)>> = HashMap::new();
    let mut add = |from: Node, to: Node, weight: u32| {
        edges.entry(from).or_default().push((to, weight));
    };

    for assign in &module.assignments {
        let mut reads = Vec::new();
        expression_reads(&assign.rhs, &mut reads);
        lvalue_index_reads(&assign.lhs, &mut reads);
        for target in lvalue_targets(&assign.lhs) {
            for &read in &reads {
                add(read, target, 0);
            }
        }
    }
    for process in &module.processes {
        let registered = process.kind == ProcessKind::Sequential;
        let mut writes = Vec::new();
        block_writes(&process.body, &[], &mut writes);
        for (target, reads) in writes {
            // A variable in a clocked process holds a value within one cycle
            let weight = u32::from(registered && !matches!(target, Node::Variable(_)));
            for read in reads {
                add(read, target, weight);
            }
        }
    }
    for instance in &module.instances {
        let Some(child) = mir.modules.iter().find(|m| m.id == instance.module) else {
            continue;
        };
        let inner = latency_matrix(mir, child, cache);
        for (input, input_expr) in &instance.connections {
            let mut reads = Vec::new();
            expression_reads(input_expr, &mut reads);
            for (output, output_expr) in &instance.connections {
                let Some(&weight) = inner.get(&(input.clone(), output.clone())) else {
                    continue;
                };
                let ExpressionKind::Ref(target) = &output_expr.kind else {
                    continue;
                };
                for node in lvalue_targets(target) {
                    for &read in &reads {
                        add(read, node, weight);
                    }
                }
            }
        }
    }

    let mut matrix = LatencyMatrix::new();
    for input in module.ports.iter().filter(|p| {
        // Clocks and resets are control: a reset clears a register within
        // one cycle wherever it sits, so it says nothing about the data path
        p.direction == PortDirection::Input
            && !matches!(p.port_type, DataType::Clock { .. } | DataType::Reset { .. })
    }) {
        // 0-1 breadth-first search: weights are register counts
        let mut dist: HashMap<Node, u32> = HashMap::new();
        let mut queue = VecDeque::new();
        dist.insert(Node::Port(input.id), 0);
        queue.push_back(Node::Port(input.id));
        while let Some(node) = queue.pop_front() {
            let d = dist[&node];
            for &(next, w) in edges.get(&node).map(Vec::as_slice).unwrap_or(&[]) {
                let nd = d + w;
                if dist.get(&next).is_none_or(|&old| nd < old) {
                    dist.insert(next, nd);
                    if w == 0 {
                        queue.push_front(next);
                    } else {
                        queue.push_back(next);
                    }
                }
            }
        }
        for output in module
            .ports
            .iter()
            .filter(|p| p.direction == PortDirection::Output)
        {
            if let Some(&d) = dist.get(&Node::Port(output.id)) {
                matrix.insert((input.name.clone(), output.name.clone()), d);
            }
        }
    }
    cache.insert(module.id, matrix.clone());
    matrix
}

/// Every write in `block`, with what it reads, including the conditions it
/// sits under
fn block_writes(block: &Block, conditions: &[Node], writes: &mut Vec<(Node, Vec<Node>)>) {
    for statement in &block.statements {
        statement_writes(statement, conditions, writes);
    }
}

fn statement_writes(
    statement: &Statement,
    conditions: &[Node],
    writes: &mut Vec<(Node, Vec<Node>)>,
) {
    match statement {
        Statement::Assignment(a) => {
            let mut reads = conditions.to_vec();
            expression_reads(&a.rhs, &mut reads);
            lvalue_index_reads(&a.lhs, &mut reads);
            for target in lvalue_targets(&a.lhs) {
                writes.push((target, reads.clone()));
            }
        }
        Statement::If(s) => {
            let mut inner = conditions.to_vec();
            expression_reads(&s.condition, &mut inner);
            block_writes(&s.then_block, &inner, writes);
            if let Some(e) = &s.else_block {
                block_writes(e, &inner, writes);
            }
        }
        Statement::Case(s) => {
            let mut inner = conditions.to_vec();
            expression_reads(&s.expr, &mut inner);
            for item in &s.items {
                let mut item_conditions = inner.clone();
                for value in &item.values {
                    expression_reads(value, &mut item_conditions);
                }
                block_writes(&item.block, &item_conditions, writes);
            }
            if let Some(d) = &s.default {
                block_writes(d, &inner, writes);
            }
        }
        Statement::Block(b) => block_writes(b, conditions, writes),
        Statement::Loop(l) => match l {
            crate::mir::LoopStatement::For {
                init,
                condition,
                update,
                body,
            } => {
                let mut inner = conditions.to_vec();
                expression_reads(condition, &mut inner);
                statement_writes(&Statement::Assignment((**init).clone()), conditions, writes);
                statement_writes(&Statement::Assignment((**update).clone()), &inner, writes);
                block_writes(body, &inner, writes);
            }
            crate::mir::LoopStatement::While { condition, body } => {
                let mut inner = conditions.to_vec();
                expression_reads(condition, &mut inner);
                block_writes(body, &inner, writes);
            }
        },
        Statement::ResolvedConditional(r) => {
            statement_writes(&Statement::If((*r.original).clone()), conditions, writes);
        }
        Statement::Assert(_) | Statement::Assume(_) | Statement::Cover(_) => {}
    }
}

/// The ports, signals and variables an expression reads
fn expression_reads(expr: &Expression, reads: &mut Vec<Node>) {
    match &expr.kind {
        ExpressionKind::Literal(_) => {}
        ExpressionKind::Ref(lvalue) => {
            reads.extend(lvalue_targets(lvalue));
            lvalue_index_reads(lvalue, reads);
        }
        ExpressionKind::Binary { left, right, .. } => {
            expression_reads(left, reads);
            expression_reads(right, reads);
        }
        ExpressionKind::Unary { operand, .. } => expression_reads(operand, reads),
        ExpressionKind::Conditional {
            cond,
            then_expr,
            else_expr,
        } => {
            expression_reads(cond, reads);
            expression_reads(then_expr, reads);
            expression_reads(else_expr, reads);
        }
        ExpressionKind::Concat(parts) => parts.iter().for_each(|p| expression_reads(p, reads)),
        ExpressionKind::Replicate { count, value } => {
            expression_reads(count, reads);
            expression_reads(value, reads);
        }
        ExpressionKind::FunctionCall { args, .. } => {
            args.iter().for_each(|a| expression_reads(a, reads))
        }
        ExpressionKind::Cast { expr, .. } => expression_reads(expr, reads),
        ExpressionKind::TupleFieldAccess { base, .. }
        | ExpressionKind::FieldAccess { base, .. } => expression_reads(base, reads),
    }
}

/// The values an lvalue names (the base of a select, each part of a concat)
fn lvalue_targets(lvalue: &LValue) -> Vec<Node> {
    match lvalue {
        LValue::Port(id) => vec![Node::Port(*id)],
        LValue::Signal(id) => vec![Node::Signal(*id)],
        LValue::Variable(id) => vec![Node::Variable(*id)],
        LValue::BitSelect { base, .. } | LValue::RangeSelect { base, .. } => lvalue_targets(base),
        LValue::Concat(parts) => parts.iter().flat_map(lvalue_targets).collect(),
    }
}

/// What the index expressions of an lvalue read
fn lvalue_index_reads(lvalue: &LValue, reads: &mut Vec<Node>) {
    match lvalue {
        LValue::Port(_) | LValue::Signal(_) | LValue::Variable(_) => {}
        LValue::BitSelect { base, index } => {
            expression_reads(index, reads);
            lvalue_index_reads(base, reads);
        }
        LValue::RangeSelect { base, high, low } => {
            expression_reads(high, reads);
            expression_reads(low, reads);
            lvalue_index_reads(base, reads);
        }
        LValue::Concat(parts) => parts.iter().for_each(|p| lvalue_index_reads(p, reads)),
    }
}
