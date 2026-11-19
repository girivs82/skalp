//! Channels between self-timed async blocks: `stream<T>` ports
//!
//! An `async` entity can declare stream ports:
//!
//! ```text
//! async entity Producer { out o: stream<bit[8]> }
//! async entity Consumer { in i: stream<bit[8]> }
//! impl Producer { on() { o.send(value) } }
//! impl Consumer { on() { let v = i.recv() ... } }
//! ```
//!
//! A block with stream ports is self-timed: it takes a step (one write of its
//! `on()` registers) as soon as every channel the step uses is ready, without
//! waiting for a request from its parent. A step that sends needs the channel
//! empty (the receiver took the previous value); a step that receives needs it
//! full. Which channels a step uses is decided by the conditions around the
//! `send`/`recv` calls, evaluated on the block's state at the start of the
//! step.
//!
//! This pass lowers streams to plain ports before MIR. Each channel is a
//! two-phase bundled-data channel:
//!
//! * the data, a register of the sender (`o`, written by `send`);
//! * `o__req`, the sender's phase bit, toggled by every `send`;
//! * `o__ack`, the receiver's phase bit, toggled by every `recv`.
//!
//! The channel is full when the two phase bits differ. Each block gets a
//! combinational `__ncl_ready` signal ([`NCL_READY_SIGNAL`]): the AND, over
//! its channels, of "not used by the next step, or ready". Synthesis builds
//! the block's step controller from it (`skalp_lir::ncl_expand`); `ncl_req`
//! becomes a run enable from the parent.
//!
//! The parent wires one stream output to one stream input
//! (`inst c = Consumer { i: p.o }`); this pass adds the phase-bit connections
//! in both directions.
//!
//! `taken(s)`, for a stream input `s` of a `match arbitrate(..)`, is 1 in a
//! step whose arbiter took `s` (the step runs `s`'s arm) and 0 otherwise. It
//! can be used anywhere in the block, outside the arm too: with `s` itself,
//! which holds the value waiting on the channel, it lets logic outside the
//! arm act on the value in the same step it is received (grant a request
//! the step takes, say), instead of a step later from a register.
//!
//! `ready(s)` is 1 while channel `s` is ready: a stream output empty (a
//! `send` would not wait), a stream input full (a `recv` would not wait).
//! It reads only phase bits, so it may decide what a step sends or receives:
//! send to whichever of two channels is free, say.
//!
//! Rules, each an error here:
//! * each stream has exactly one `send` (or `recv`) site per block, under
//!   `if`/`match` conditions only (not inside an expression);
//! * a stream input is connected to exactly one stream output of a sibling
//!   instance, and each stream output feeds exactly one input.
//!
//! A cycle of channels in which every block only sends after receiving in
//! the same step can never move a value (they all start empty): that is a
//! warning.
//!
//! That the conditions depend only on the block's registers is checked on the
//! netlist after synthesis, where the whole cone is visible.

use crate::hir::{
    AssignmentId, Hir, HirAssignment, HirAssignmentType, HirBinaryExpr, HirBinaryOp, HirConnection,
    HirEventBlock, HirExpression, HirLValue, HirLiteral, HirPattern, HirPort, HirPortDirection,
    HirSignal, HirStatement, HirType, HirUnaryExpr, HirUnaryOp, PortId, SignalId,
};
use std::collections::{HashMap, HashSet};

/// Combinational signal of a self-timed block: high when every channel its
/// next step uses is ready
pub const NCL_READY_SIGNAL: &str = "__ncl_ready";
/// Behavioural-only input of a self-timed block: the clocked parent's clock,
/// which steps the block in behavioural simulation (one step per rising
/// edge while it is ready). Synthesis leaves it unused: there the block's own
/// matched delays time its steps.
pub const NCL_TICK_PORT: &str = "__ncl_tick";
/// Name prefix of an arbiter's signals in a self-timed block:
/// `__ncl_arb<k>_req` (requests, one per arm) and `__ncl_arb<k>_gnt`
/// (grants). Behaviourally the grant is a fixed-priority pick; synthesis
/// drives it from a mutual-exclusion element instead.
pub const NCL_ARBITER_PREFIX: &str = "__ncl_arb";
/// Suffix of an arbiter's "some arm granted" signal (`__ncl_arb<k>_any`),
/// named when the block's send/recv conditions depend on its grants (see
/// [`grant_settle_needed`]): synthesis delays it until they have settled.
pub const NCL_GRANT_ANY_SUFFIX: &str = "_any";
/// Suffix of a stream's sender phase port
pub const STREAM_REQ_SUFFIX: &str = "__req";
/// Suffix of a stream's receiver phase port
pub const STREAM_ACK_SUFFIX: &str = "__ack";

/// Name of the phase register a block keeps for stream `port`
fn phase_signal(port: &str) -> String {
    format!("__ncl_ph_{port}")
}

/// One stream port of an async entity
#[derive(Debug, Clone)]
struct StreamPort {
    name: String,
    /// Output (send) or input (recv)
    output: bool,
    /// The data port (the stream port itself, retyped to its element type)
    data: PortId,
    /// `<name>__req`
    req: PortId,
    /// `<name>__ack`
    ack: PortId,
}

/// Fresh ids above every id already used in the design
struct Ids {
    port: u32,
    signal: u32,
    assignment: u32,
}

impl Ids {
    fn new(hirs: &[&Hir]) -> Self {
        let mut ids = Ids {
            port: 0,
            signal: 0,
            assignment: 0,
        };
        for hir in hirs {
            for e in &hir.entities {
                for p in &e.ports {
                    ids.port = ids.port.max(p.id.0 + 1);
                }
            }
            for i in &hir.implementations {
                for s in &i.signals {
                    ids.signal = ids.signal.max(s.id.0 + 1);
                }
                for a in &i.assignments {
                    ids.assignment = ids.assignment.max(a.id.0 + 1);
                }
                for b in &i.event_blocks {
                    max_assignment_id(&b.statements, &mut ids.assignment);
                }
                max_assignment_id(&i.statements, &mut ids.assignment);
            }
        }
        ids
    }
    fn port(&mut self) -> PortId {
        self.port += 1;
        PortId(self.port - 1)
    }
    fn signal(&mut self) -> SignalId {
        self.signal += 1;
        SignalId(self.signal - 1)
    }
    fn assignment(&mut self) -> AssignmentId {
        self.assignment += 1;
        AssignmentId(self.assignment - 1)
    }
}

fn max_assignment_id(statements: &[HirStatement], max: &mut u32) {
    for s in statements {
        match s {
            HirStatement::Assignment(a) => *max = (*max).max(a.id.0 + 1),
            HirStatement::If(i) => {
                max_assignment_id(&i.then_statements, max);
                if let Some(e) = &i.else_statements {
                    max_assignment_id(e, max);
                }
            }
            HirStatement::Match(m) => {
                for arm in &m.arms {
                    max_assignment_id(&arm.statements, max);
                }
            }
            HirStatement::Block(b) => max_assignment_id(b, max),
            HirStatement::For(f) => max_assignment_id(&f.body, max),
            _ => {}
        }
    }
}

/// Whether `hir` has any stream port on an async entity
pub fn has_async_streams(hir: &Hir) -> bool {
    hir.entities.iter().any(|e| {
        e.is_async
            && e.ports
                .iter()
                .any(|p| matches!(p.port_type, HirType::Stream(_)))
    })
}

fn new_port(id: PortId, name: String, direction: HirPortDirection, port_type: HirType) -> HirPort {
    HirPort {
        id,
        name,
        comments: Vec::new(),
        direction,
        port_type,
        clock_domain_name: None,
        physical_constraints: None,
        detection_config: None,
        power_domain_config: None,
        isolation_config: None,
        retention_config: None,
    }
}

fn new_signal(id: SignalId, name: String, signal_type: HirType, init: Option<u64>) -> HirSignal {
    HirSignal {
        id,
        name,
        comments: Vec::new(),
        signal_type,
        initial_value: init.map(|v| HirExpression::Literal(HirLiteral::Integer(v))),
        clock_domain: None,
        clock_domain_name: None,
        span: None,
        memory_config: None,
        trace_config: None,
        cdc_config: None,
        breakpoint_config: None,
        power_config: None,
        safety_config: None,
        power_domain: None,
    }
}

fn bin(left: HirExpression, op: HirBinaryOp, right: HirExpression) -> HirExpression {
    HirExpression::Binary(HirBinaryExpr {
        left: Box::new(left),
        op,
        right: Box::new(right),
        is_trait_op: false,
        impl_style: Default::default(),
    })
}

fn not(e: HirExpression) -> HirExpression {
    HirExpression::Unary(HirUnaryExpr {
        op: HirUnaryOp::Not,
        operand: Box::new(e),
    })
}

fn truth() -> HirExpression {
    HirExpression::Literal(HirLiteral::Boolean(true))
}

/// Conjunction of `conds` (true when empty)
fn all(conds: &[HirExpression]) -> HirExpression {
    let mut it = conds.iter().cloned();
    match it.next() {
        None => truth(),
        Some(first) => it.fold(first, |acc, c| bin(acc, HirBinaryOp::LogicalAnd, c)),
    }
}

fn assign(
    ids: &mut Ids,
    lhs: HirLValue,
    rhs: HirExpression,
    kind: HirAssignmentType,
) -> HirAssignment {
    HirAssignment {
        id: ids.assignment(),
        comments: Vec::new(),
        lhs,
        assignment_type: kind,
        rhs,
    }
}

/// A `send`/`recv` call on a stream port: (port, is_send, value)
fn stream_call<'a>(
    expr: &'a HirExpression,
    streams: &HashMap<PortId, StreamPort>,
) -> Option<(PortId, bool, Option<&'a HirExpression>)> {
    let HirExpression::Call(call) = expr else {
        return None;
    };
    let is_send = match call.function.as_str() {
        "send" => true,
        "recv" => false,
        _ => return None,
    };
    let HirExpression::Port(port) = call.args.first()? else {
        return None;
    };
    streams.get(port)?;
    Some((*port, is_send, call.args.get(1)))
}

/// Rewrites the `on()` block of one self-timed entity
struct Lowering<'a> {
    entity: &'a str,
    streams: &'a HashMap<PortId, StreamPort>,
    /// Phase register of each stream
    phase: HashMap<PortId, SignalId>,
    /// Data register of each stream output
    data: HashMap<PortId, SignalId>,
    /// Condition under which the next step uses each stream
    used: HashMap<PortId, HirExpression>,
    /// The conditions around each stream's site, each printed (for the
    /// deadlock check: a site whose conditions include all of another's
    /// is only reached in steps that also reach the other)
    site_conds: HashMap<PortId, HashSet<String>>,
    errors: Vec<String>,
    ids: &'a mut Ids,
    /// Arbiters made by `match arbitrate(..)`
    arbiters: Vec<Arbiter>,
}

/// One `match arbitrate(a, b, ..) { .. }` of a self-timed block
struct Arbiter {
    /// `__ncl_arb<k>_req`: one request per arm, in arm order
    req: SignalId,
    /// `__ncl_arb<k>_gnt`: the grants, one-hot (see [`NCL_ARBITER_PREFIX`])
    gnt: SignalId,
    /// `__ncl_arb<k>_last`: the arm granted last, one-hot (behavioural
    /// round-robin only; unused once synthesis puts in the mutex)
    last: SignalId,
    /// Request expression of each arm
    requests: Vec<HirExpression>,
    /// The stream of each arm, in arm order (for `taken(s)`)
    streams: Vec<PortId>,
    /// Condition under which the step reaches the match
    reached: HirExpression,
}

impl Lowering<'_> {
    fn port_name(&self, port: PortId) -> &str {
        &self.streams[&port].name
    }

    fn toggle(&mut self, port: PortId) -> HirStatement {
        let ph = self.phase[&port];
        HirStatement::Assignment(assign(
            self.ids,
            HirLValue::Signal(ph),
            HirExpression::Unary(HirUnaryExpr {
                op: HirUnaryOp::BitwiseNot,
                operand: Box::new(HirExpression::Signal(ph)),
            }),
            HirAssignmentType::NonBlocking,
        ))
    }

    /// Record a use of `port` under `conds`
    fn use_site(&mut self, port: PortId, conds: &[HirExpression]) {
        if self.used.contains_key(&port) {
            self.errors.push(format!(
                "stream `{p}` of async entity `{e}` is used in more than one place; each stream has one `send` or `recv` per block",
                p = self.port_name(port),
                e = self.entity
            ));
            return;
        }
        self.used.insert(port, all(conds));
        self.site_conds
            .insert(port, conds.iter().map(|c| format!("{c:?}")).collect());
    }

    /// Replace `recv` calls in `expr` with the stream's data, recording each
    /// (with `conds`) in `received`. `nested` is set inside an expression
    /// that only evaluates some of its operands.
    fn rewrite_expr(
        &mut self,
        expr: &mut HirExpression,
        conds: &[HirExpression],
        nested: bool,
        received: &mut Vec<PortId>,
    ) {
        if let Some((port, is_send, _)) = stream_call(expr, self.streams) {
            let name = self.port_name(port).to_string();
            if is_send {
                self.errors.push(format!(
                    "`{name}.send(..)` in async entity `{e}` is a statement, not a value",
                    e = self.entity
                ));
            } else if self.streams[&port].output {
                self.errors.push(format!(
                    "`{name}.recv()` in async entity `{e}`: `{name}` is a stream output",
                    e = self.entity
                ));
            } else if nested {
                self.errors.push(format!(
                    "`{name}.recv()` in async entity `{e}` is inside a conditional expression; receive in an `if` or `match` statement instead",
                    e = self.entity
                ));
            } else {
                self.use_site(port, conds);
                received.push(port);
            }
            *expr = HirExpression::Port(self.streams[&port].data);
            return;
        }
        let conditional = matches!(
            expr,
            HirExpression::Ternary { .. } | HirExpression::If(_) | HirExpression::Match(_)
        );
        let nested = nested || conditional;
        match expr {
            HirExpression::Binary(b) => {
                self.rewrite_expr(&mut b.left, conds, nested, received);
                self.rewrite_expr(&mut b.right, conds, nested, received);
            }
            HirExpression::Unary(u) => self.rewrite_expr(&mut u.operand, conds, nested, received),
            HirExpression::Call(c) => {
                for a in &mut c.args {
                    self.rewrite_expr(a, conds, nested, received);
                }
            }
            HirExpression::Index(a, b) => {
                self.rewrite_expr(a, conds, nested, received);
                self.rewrite_expr(b, conds, nested, received);
            }
            HirExpression::Range(a, b, c) => {
                self.rewrite_expr(a, conds, nested, received);
                self.rewrite_expr(b, conds, nested, received);
                self.rewrite_expr(c, conds, nested, received);
            }
            HirExpression::FieldAccess { base, .. } => {
                self.rewrite_expr(base, conds, nested, received)
            }
            HirExpression::Concat(v)
            | HirExpression::TupleLiteral(v)
            | HirExpression::ArrayLiteral(v) => {
                for e in v {
                    self.rewrite_expr(e, conds, nested, received);
                }
            }
            HirExpression::Ternary {
                condition,
                true_expr,
                false_expr,
            } => {
                self.rewrite_expr(condition, conds, nested, received);
                self.rewrite_expr(true_expr, conds, nested, received);
                self.rewrite_expr(false_expr, conds, nested, received);
            }
            HirExpression::Cast(c) => self.rewrite_expr(&mut c.expr, conds, nested, received),
            HirExpression::ArrayRepeat { value, count } => {
                self.rewrite_expr(value, conds, nested, received);
                self.rewrite_expr(count, conds, nested, received);
            }
            // `recv` inside these is reported by `contains_stream_call`
            HirExpression::If(_) | HirExpression::Match(_) | HirExpression::Block { .. }
                if contains_stream_call(expr, self.streams) =>
            {
                self.errors.push(format!(
                    "a stream `recv()` in async entity `{e}` is inside a conditional expression; receive in an `if` or `match` statement instead",
                    e = self.entity
                ));
            }
            _ => {}
        }
    }

    /// Lower `match arbitrate(s0, s1, ..) { .. }` to an `if` chain on the
    /// grants of an arbiter. Arm `k` (pattern `k`) is taken when the arbiter
    /// grants stream `k`, which it does only once `k` holds a value; the
    /// arm must receive from it. An optional `_ if cond` arm takes a step
    /// without a value, when `cond` (on registers) holds and the arbiter
    /// grants it rather than a stream. The grant is decided once per step
    /// and stays until that step has taken its value, so the step sees one
    /// arm even when values arrive at nearly the same time.
    fn lower_arbitrate(
        &mut self,
        m: &crate::hir::HirMatchStatement,
        conds: &[HirExpression],
    ) -> Option<Vec<HirStatement>> {
        let HirExpression::Call(call) = &m.expr else {
            return None;
        };
        let entity = self.entity.to_string();
        let mut streams = Vec::new();
        for arg in &call.args {
            match arg {
                HirExpression::Port(p) if self.streams.get(p).is_some_and(|s| !s.output) => {
                    streams.push(*p)
                }
                _ => {
                    self.errors.push(format!(
                        "`arbitrate(..)` in async entity `{entity}` takes stream inputs of the block"
                    ));
                    return None;
                }
            }
        }
        if streams.is_empty() {
            self.errors.push(format!(
                "`arbitrate()` in async entity `{entity}` needs at least one stream"
            ));
            return None;
        }
        let n = streams.len();
        let mut arms: Vec<Option<Vec<HirStatement>>> = vec![None; n];
        let mut default: Option<(HirExpression, Vec<HirStatement>)> = None;
        for arm in &m.arms {
            match (&arm.pattern, &arm.guard) {
                (HirPattern::Literal(HirLiteral::Integer(k)), None) if (*k as usize) < n => {
                    let k = *k as usize;
                    if arms[k].is_some() {
                        self.errors.push(format!(
                            "`arbitrate(..)` in async entity `{entity}` has two arms for stream {k}"
                        ));
                    }
                    arms[k] = Some(arm.statements.clone());
                }
                (HirPattern::Wildcard | HirPattern::Variable(_), Some(guard))
                    if default.is_none() =>
                {
                    default = Some((guard.clone(), arm.statements.clone()));
                }
                (HirPattern::Wildcard | HirPattern::Variable(_), None) => {
                    self.errors.push(format!(
                        "the `_` arm of `arbitrate(..)` in async entity `{entity}` needs a condition (`_ if cond =>`): without one the block would never wait"
                    ));
                    return None;
                }
                _ => {
                    self.errors.push(format!(
                        "arms of `arbitrate(..)` in async entity `{entity}` are the stream numbers 0..{n} and one `_ if cond`"
                    ));
                    return None;
                }
            }
        }
        for (k, stream) in streams.iter().enumerate() {
            match &arms[k] {
                None => {
                    self.errors.push(format!(
                        "`arbitrate(..)` in async entity `{entity}` has no arm for stream {k} (`{}`)",
                        self.port_name(*stream)
                    ));
                    return None;
                }
                Some(body) => {
                    let mut receives = false;
                    walk_statements(body, &mut |e| {
                        if matches!(stream_call(e, self.streams), Some((p, false, _)) if p == *stream)
                        {
                            receives = true;
                        }
                    });
                    if !receives {
                        self.errors.push(format!(
                            "arm {k} of `arbitrate(..)` in async entity `{entity}` must receive from `{}`",
                            self.port_name(*stream)
                        ));
                    }
                }
            }
        }

        let req = self.ids.signal();
        let gnt = self.ids.signal();
        let last = self.ids.signal();
        let reached = all(conds);
        let mut requests: Vec<HirExpression> = streams
            .iter()
            .map(|p| {
                let full = bin(
                    HirExpression::Port(self.streams[p].req),
                    HirBinaryOp::NotEqual,
                    HirExpression::Signal(self.phase[p]),
                );
                bin(reached.clone(), HirBinaryOp::LogicalAnd, full)
            })
            .collect();
        if let Some((guard, _)) = &default {
            requests.push(bin(reached.clone(), HirBinaryOp::LogicalAnd, guard.clone()));
        }
        let granted = |k: usize| {
            HirExpression::Index(
                Box::new(HirExpression::Signal(gnt)),
                Box::new(HirExpression::Literal(HirLiteral::Integer(k as u64))),
            )
        };
        // if gnt[0] { arm 0 } else if gnt[1] { .. } else if gnt[n] { default }
        // Each arm records that it was granted, for the round-robin
        // behavioural grant
        let mut mark = |k: usize, body: Vec<HirStatement>| {
            let mut out = vec![HirStatement::Assignment(assign(
                self.ids,
                HirLValue::Signal(last),
                HirExpression::Literal(HirLiteral::Integer(1u64 << k)),
                HirAssignmentType::NonBlocking,
            ))];
            out.extend(body);
            out
        };
        let mut branches: Vec<(HirExpression, Vec<HirStatement>)> = arms
            .into_iter()
            .enumerate()
            .map(|(k, body)| (granted(k), mark(k, body.unwrap_or_default())))
            .collect();
        if let Some((_, body)) = default {
            branches.push((granted(n), mark(n, body)));
        }
        let mut chain: Option<Vec<HirStatement>> = None;
        for (cond, body) in branches.into_iter().rev() {
            chain = Some(vec![HirStatement::If(crate::hir::HirIfStatement {
                condition: cond,
                then_statements: body,
                else_statements: chain,
                mux_style: Default::default(),
            })]);
        }
        if requests.len() > 63 {
            self.errors.push(format!(
                "`arbitrate(..)` in async entity `{entity}` takes at most 63 arms"
            ));
            return None;
        }
        self.arbiters.push(Arbiter {
            req,
            gnt,
            last,
            requests,
            streams,
            reached,
        });
        chain
    }

    fn rewrite_block(&mut self, statements: &mut Vec<HirStatement>, conds: &[HirExpression]) {
        let mut out = Vec::with_capacity(statements.len());
        for mut stmt in statements.drain(..) {
            // `x.send(v)`
            if let HirStatement::Expression(expr) = &stmt {
                if let Some((port, true, value)) = stream_call(expr, self.streams) {
                    let stream = self.streams[&port].clone();
                    let Some(value) = value.cloned() else {
                        self.errors.push(format!(
                            "`{n}.send()` in async entity `{e}` needs a value",
                            n = stream.name,
                            e = self.entity
                        ));
                        continue;
                    };
                    if !stream.output {
                        self.errors.push(format!(
                            "`{n}.send(..)` in async entity `{e}`: `{n}` is a stream input",
                            n = stream.name,
                            e = self.entity
                        ));
                        continue;
                    }
                    let mut value = value;
                    let mut received = Vec::new();
                    self.rewrite_expr(&mut value, conds, false, &mut received);
                    for p in received {
                        out.push(self.toggle(p));
                    }
                    self.use_site(port, conds);
                    out.push(HirStatement::Assignment(assign(
                        self.ids,
                        HirLValue::Signal(self.data[&port]),
                        value,
                        HirAssignmentType::NonBlocking,
                    )));
                    out.push(self.toggle(port));
                    continue;
                }
            }
            // `match arbitrate(a, b, ..) { 0 => .., 1 => .., _ if cond => .. }`
            if let HirStatement::Match(m) = &stmt {
                if matches!(&m.expr, HirExpression::Call(c) if c.function == "arbitrate") {
                    if let Some(mut chain) = self.lower_arbitrate(m, conds) {
                        self.rewrite_block(&mut chain, conds);
                        out.extend(chain);
                    }
                    continue;
                }
            }
            let mut received = Vec::new();
            match &mut stmt {
                HirStatement::Assignment(a) => {
                    self.rewrite_expr(&mut a.rhs, conds, false, &mut received)
                }
                HirStatement::Let(l) => {
                    self.rewrite_expr(&mut l.value, conds, false, &mut received)
                }
                HirStatement::Expression(e) => self.rewrite_expr(e, conds, false, &mut received),
                HirStatement::Block(b) => self.rewrite_block(b, conds),
                HirStatement::If(i) => {
                    self.rewrite_expr(&mut i.condition, conds, false, &mut received);
                    let mut then_conds = conds.to_vec();
                    then_conds.push(i.condition.clone());
                    self.rewrite_block(&mut i.then_statements, &then_conds);
                    if let Some(e) = &mut i.else_statements {
                        let mut else_conds = conds.to_vec();
                        else_conds.push(not(i.condition.clone()));
                        self.rewrite_block(e, &else_conds);
                    }
                }
                HirStatement::Match(m) => {
                    self.rewrite_expr(&mut m.expr, conds, false, &mut received);
                    // Arm k is taken when its pattern matches and no earlier
                    // arm's did
                    let mut earlier: Vec<HirExpression> = Vec::new();
                    for arm in &mut m.arms {
                        let matches = match &arm.pattern {
                            HirPattern::Literal(lit) => Some(bin(
                                m.expr.clone(),
                                HirBinaryOp::Equal,
                                HirExpression::Literal(lit.clone()),
                            )),
                            HirPattern::Path(enum_type, variant) => Some(bin(
                                m.expr.clone(),
                                HirBinaryOp::Equal,
                                HirExpression::EnumVariant {
                                    enum_type: enum_type.clone(),
                                    variant: variant.clone(),
                                },
                            )),
                            HirPattern::Wildcard | HirPattern::Variable(_) => None,
                            _ => {
                                if block_uses_streams(&arm.statements, self.streams) {
                                    self.errors.push(format!(
                                        "stream `send`/`recv` in async entity `{e}` under a match arm with a tuple pattern; use literal, enum or `_` patterns",
                                        e = self.entity
                                    ));
                                }
                                None
                            }
                        };
                        let taken = match (&matches, &arm.guard) {
                            (Some(m), Some(g)) => {
                                bin(m.clone(), HirBinaryOp::LogicalAnd, g.clone())
                            }
                            (Some(m), None) => m.clone(),
                            (None, Some(g)) => g.clone(),
                            (None, None) => truth(),
                        };
                        let mut arm_conds = conds.to_vec();
                        arm_conds.extend(earlier.iter().map(|e| not(e.clone())));
                        arm_conds.push(taken.clone());
                        self.rewrite_block(&mut arm.statements, &arm_conds);
                        earlier.push(taken);
                    }
                }
                HirStatement::For(f) if block_uses_streams(&f.body, self.streams) => {
                    self.errors.push(format!(
                        "stream `send`/`recv` inside a `for` loop in async entity `{e}`",
                        e = self.entity
                    ));
                }
                _ => {}
            }
            for p in received {
                out.push(self.toggle(p));
            }
            out.push(stmt);
        }
        *statements = out;
    }
}

fn contains_stream_call(expr: &HirExpression, streams: &HashMap<PortId, StreamPort>) -> bool {
    // Serialise-free walk: look for a Call of send/recv on a stream port
    let mut found = false;
    walk_expr(expr, &mut |e| {
        if stream_call(e, streams).is_some() {
            found = true;
        }
    });
    found
}

/// Name prefix of the signals that carry what an arbiter took:
/// `__ncl_take_<s>` is the value of stream input `s` (its data while the arm
/// that receives it is granted, 0 otherwise) and `__ncl_take_<s>_gnt` the
/// grant of that arm. Synthesis builds the value from kept AND gates
/// (`skalp_lir::ncl_expand`), so that it only changes with the grant.
pub const NCL_TAKE_PREFIX: &str = "__ncl_take";

/// Replace each `ready(s)` with whether channel `s` is ready: for a stream
/// output, empty (the receiver took the last value, so a `send` would not
/// wait); for a stream input, full (a `recv` would not wait). It reads the
/// block's phase register and the other end's phase bit, the same signals
/// the step's own `ready` reads, so it may decide what a step sends or
/// receives.
fn lower_ready(
    implementation: &mut crate::hir::HirImplementation,
    arbiters: &mut [Arbiter],
    used: &mut HashMap<PortId, HirExpression>,
    phase: &HashMap<PortId, SignalId>,
    streams: &HashMap<PortId, StreamPort>,
    entity: &str,
    errors: &mut Vec<String>,
) {
    let mut replace = |expr: &mut HirExpression| {
        let HirExpression::Call(c) = expr else {
            return;
        };
        if c.function != "ready" {
            return;
        }
        let ready = match c.args.as_slice() {
            [HirExpression::Port(p)] if streams.contains_key(p) => {
                let s = &streams[p];
                let ph = HirExpression::Signal(phase[&s.data]);
                if s.output {
                    bin(ph, HirBinaryOp::Equal, HirExpression::Port(s.ack))
                } else {
                    bin(HirExpression::Port(s.req), HirBinaryOp::NotEqual, ph)
                }
            }
            _ => {
                errors.push(format!(
                    "`ready(..)` in async entity `{entity}` takes one stream port of the block"
                ));
                return;
            }
        };
        *expr = ready;
    };
    for a in &mut implementation.assignments {
        map_expr_mut(&mut a.rhs, &mut replace);
    }
    for cond in used.values_mut() {
        map_expr_mut(cond, &mut replace);
    }
    for arb in arbiters.iter_mut() {
        for r in arb.requests.iter_mut() {
            map_expr_mut(r, &mut replace);
        }
        map_expr_mut(&mut arb.reached, &mut replace);
    }
    map_statements_mut(&mut implementation.statements, &mut replace);
    for block in &mut implementation.event_blocks {
        map_statements_mut(&mut block.statements, &mut replace);
    }
}

/// Whether a step's `ready` needs the grants it depends on to settle first.
///
/// `ready` is the AND of "some arm granted" and, for each channel, "not
/// used, or ready". The step latch reads it at any moment and keeps a step
/// once `ready` has been high, so `ready` must only rise, never glitch. That
/// holds while what each step uses depends on registers alone, and for an
/// arm's own `recv` (its channel is full, or the arm would not have been
/// granted). But a send or receive whose condition depends on a grant
/// (through `taken(s)`, the value taken, or a send inside an arm) changes as
/// the grant rises: "some arm granted" can then reach the latch before a
/// full channel's term has fallen, and the step would start. When any
/// condition depends on a grant, the arbiter's "some arm granted" is named
/// `__ncl_arb<k>_any`, and synthesis delays it by a matched delay that
/// covers those paths (`skalp_lir::ncl_expand`, the "grant settle" check).
fn grant_settle_needed(
    implementation: &crate::hir::HirImplementation,
    arbiters: &[Arbiter],
    used: &HashMap<PortId, HirExpression>,
    streams: &HashMap<PortId, StreamPort>,
) -> bool {
    if arbiters.is_empty() {
        return false;
    }
    // Signals that follow a grant: the grants, what the arbiters took, and
    // everything computed from them
    let mut derived: HashSet<SignalId> = arbiters.iter().map(|a| a.gnt).collect();
    derived.extend(
        implementation
            .signals
            .iter()
            .filter(|s| s.name.starts_with(NCL_TAKE_PREFIX))
            .map(|s| s.id),
    );
    let reads = |expr: &HirExpression, set: &HashSet<SignalId>| {
        let mut hit = false;
        map_expr_mut(&mut expr.clone(), &mut |e| {
            if matches!(e, HirExpression::Signal(id) if set.contains(id)) {
                hit = true;
            }
        });
        hit
    };
    loop {
        let before = derived.len();
        for a in &implementation.assignments {
            if let HirLValue::Signal(id) = a.lhs {
                if !derived.contains(&id) && reads(&a.rhs, &derived) {
                    derived.insert(id);
                }
            }
        }
        if derived.len() == before {
            break;
        }
    }
    let arm_streams: HashSet<PortId> = arbiters
        .iter()
        .flat_map(|a| a.streams.iter().copied())
        .collect();
    used.iter().any(|(port, cond)| {
        let own_arm = streams.get(port).is_some_and(|s| !s.output) && arm_streams.contains(port);
        !own_arm && reads(cond, &derived)
    })
}

/// The block [`lower_taken`] works on: its entity's name, stream ports and
/// port types
struct TakeBlock<'a> {
    entity: &'a str,
    streams: &'a HashMap<PortId, StreamPort>,
    port_types: &'a HashMap<PortId, HirType>,
}

/// `taken(s)` and the value of an arbitrated stream.
///
/// Each `taken(s)` in the block, and in the conditions under which it sends
/// or receives (`used`), becomes the grant of the arm that receives from
/// `s`. An arbiter's requests and the condition that reaches it decide the
/// grant, so `taken` in them would be a loop: an error.
///
/// Every read of an arbitrated stream `s` (in its arm and outside it) reads
/// `s` gated by that grant: the value the step takes, or 0 in a step that
/// does not take `s`. The sender only holds the value still while the
/// channel is full, and the step's logic (its send/recv conditions too) may
/// read it at any moment; through the gate it changes only when the grant
/// does, while it holds still.
fn lower_taken(
    implementation: &mut crate::hir::HirImplementation,
    arbiters: &[Arbiter],
    used: &mut HashMap<PortId, HirExpression>,
    block: &TakeBlock,
    ids: &mut Ids,
    errors: &mut Vec<String>,
) {
    let TakeBlock {
        entity,
        streams,
        port_types,
    } = *block;
    for arb in arbiters {
        let mut uses_taken = false;
        let mut find = |e: &mut HirExpression| {
            if matches!(e, HirExpression::Call(c) if c.function == "taken") {
                uses_taken = true;
            }
        };
        for r in arb.requests.iter().chain(std::iter::once(&arb.reached)) {
            map_expr_mut(&mut r.clone(), &mut find);
        }
        if uses_taken {
            errors.push(format!(
                "`taken(..)` in async entity `{entity}` decides whether a `match arbitrate(..)` is reached or which arm requests; it can only be used after the grant"
            ));
        }
    }
    // the grant and the gated value of each arbitrated stream
    let mut grant_of: HashMap<PortId, SignalId> = HashMap::new();
    let mut value_of: HashMap<PortId, SignalId> = HashMap::new();
    let mut added_signals = Vec::new();
    let mut added_assignments = Vec::new();
    for arb in arbiters {
        for (j, port) in arb.streams.iter().enumerate() {
            let name = &streams[port].name;
            let gnt = ids.signal();
            added_signals.push(new_signal(
                gnt,
                format!("{NCL_TAKE_PREFIX}_{name}_gnt"),
                HirType::Bit(1),
                None,
            ));
            added_assignments.push(assign(
                ids,
                HirLValue::Signal(gnt),
                HirExpression::Index(
                    Box::new(HirExpression::Signal(arb.gnt)),
                    Box::new(HirExpression::Literal(HirLiteral::Integer(j as u64))),
                ),
                HirAssignmentType::Combinational,
            ));
            let value = ids.signal();
            added_signals.push(new_signal(
                value,
                format!("{NCL_TAKE_PREFIX}_{name}"),
                port_types[port].clone(),
                None,
            ));
            added_assignments.push(assign(
                ids,
                HirLValue::Signal(value),
                HirExpression::If(crate::hir::HirIfExpr {
                    condition: Box::new(HirExpression::Signal(gnt)),
                    then_expr: Box::new(HirExpression::Port(*port)),
                    else_expr: Box::new(HirExpression::Literal(HirLiteral::Integer(0))),
                }),
                HirAssignmentType::Combinational,
            ));
            grant_of.insert(*port, gnt);
            value_of.insert(*port, value);
        }
    }
    let mut replace_taken = |expr: &mut HirExpression| {
        let HirExpression::Call(c) = expr else {
            return;
        };
        if c.function != "taken" {
            return;
        }
        let grant = match c.args.as_slice() {
            [HirExpression::Port(p)] if streams.contains_key(p) => match grant_of.get(p) {
                Some(g) => *g,
                None => {
                    errors.push(format!(
                        "`taken({n})` in async entity `{entity}`: `{n}` is not an arm of a `match arbitrate(..)`",
                        n = streams[p].name
                    ));
                    return;
                }
            },
            _ => {
                errors.push(format!(
                    "`taken(..)` in async entity `{entity}` takes one stream input of the block"
                ));
                return;
            }
        };
        *expr = HirExpression::Signal(grant);
    };
    let mut replace_value = |expr: &mut HirExpression| {
        if let HirExpression::Port(p) = expr {
            if let Some(v) = value_of.get(p) {
                *expr = HirExpression::Signal(*v);
            }
        }
    };
    // `taken(s)` first: its argument is the stream itself
    for f in [
        &mut replace_taken as &mut dyn FnMut(&mut HirExpression),
        &mut replace_value,
    ] {
        for a in &mut implementation.assignments {
            map_expr_mut(&mut a.rhs, f);
        }
        for cond in used.values_mut() {
            map_expr_mut(cond, f);
        }
        map_statements_mut(&mut implementation.statements, f);
        for block in &mut implementation.event_blocks {
            map_statements_mut(&mut block.statements, f);
        }
    }
    implementation.signals.extend(added_signals);
    implementation.assignments.extend(added_assignments);
}

/// Call `f` on every expression in `expr`, innermost first
fn map_expr_mut(expr: &mut HirExpression, f: &mut dyn FnMut(&mut HirExpression)) {
    match expr {
        HirExpression::Binary(b) => {
            map_expr_mut(&mut b.left, f);
            map_expr_mut(&mut b.right, f);
        }
        HirExpression::Unary(u) => map_expr_mut(&mut u.operand, f),
        HirExpression::Call(c) => c.args.iter_mut().for_each(|a| map_expr_mut(a, f)),
        HirExpression::Index(a, b) => {
            map_expr_mut(a, f);
            map_expr_mut(b, f);
        }
        HirExpression::Range(a, b, c) => {
            map_expr_mut(a, f);
            map_expr_mut(b, f);
            map_expr_mut(c, f);
        }
        HirExpression::FieldAccess { base, .. } => map_expr_mut(base, f),
        HirExpression::Concat(v)
        | HirExpression::TupleLiteral(v)
        | HirExpression::ArrayLiteral(v) => v.iter_mut().for_each(|e| map_expr_mut(e, f)),
        HirExpression::Ternary {
            condition,
            true_expr,
            false_expr,
        } => {
            map_expr_mut(condition, f);
            map_expr_mut(true_expr, f);
            map_expr_mut(false_expr, f);
        }
        HirExpression::Cast(c) => map_expr_mut(&mut c.expr, f),
        HirExpression::ArrayRepeat { value, count } => {
            map_expr_mut(value, f);
            map_expr_mut(count, f);
        }
        HirExpression::If(i) => {
            map_expr_mut(&mut i.condition, f);
            map_expr_mut(&mut i.then_expr, f);
            map_expr_mut(&mut i.else_expr, f);
        }
        HirExpression::Match(m) => {
            map_expr_mut(&mut m.expr, f);
            for arm in &mut m.arms {
                if let Some(g) = &mut arm.guard {
                    map_expr_mut(g, f);
                }
                map_expr_mut(&mut arm.expr, f);
            }
        }
        HirExpression::Block {
            statements,
            result_expr,
        } => {
            map_statements_mut(statements, f);
            map_expr_mut(result_expr, f);
        }
        HirExpression::StructLiteral(s) => {
            for field in &mut s.fields {
                map_expr_mut(&mut field.value, f);
            }
        }
        _ => {}
    }
    f(expr);
}

fn map_statements_mut(statements: &mut [HirStatement], f: &mut dyn FnMut(&mut HirExpression)) {
    for s in statements {
        match s {
            HirStatement::Assignment(a) => map_expr_mut(&mut a.rhs, f),
            HirStatement::Let(l) => map_expr_mut(&mut l.value, f),
            HirStatement::Expression(e) => map_expr_mut(e, f),
            HirStatement::Block(b) => map_statements_mut(b, f),
            HirStatement::If(i) => {
                map_expr_mut(&mut i.condition, f);
                map_statements_mut(&mut i.then_statements, f);
                if let Some(e) = &mut i.else_statements {
                    map_statements_mut(e, f);
                }
            }
            HirStatement::Match(m) => {
                map_expr_mut(&mut m.expr, f);
                for arm in &mut m.arms {
                    if let Some(g) = &mut arm.guard {
                        map_expr_mut(g, f);
                    }
                    map_statements_mut(&mut arm.statements, f);
                }
            }
            HirStatement::For(l) => map_statements_mut(&mut l.body, f),
            _ => {}
        }
    }
}

fn walk_expr(expr: &HirExpression, f: &mut dyn FnMut(&HirExpression)) {
    f(expr);
    match expr {
        HirExpression::Binary(b) => {
            walk_expr(&b.left, f);
            walk_expr(&b.right, f);
        }
        HirExpression::Unary(u) => walk_expr(&u.operand, f),
        HirExpression::Call(c) => c.args.iter().for_each(|a| walk_expr(a, f)),
        HirExpression::Index(a, b) => {
            walk_expr(a, f);
            walk_expr(b, f);
        }
        HirExpression::Range(a, b, c) => {
            walk_expr(a, f);
            walk_expr(b, f);
            walk_expr(c, f);
        }
        HirExpression::FieldAccess { base, .. } => walk_expr(base, f),
        HirExpression::Concat(v)
        | HirExpression::TupleLiteral(v)
        | HirExpression::ArrayLiteral(v) => v.iter().for_each(|e| walk_expr(e, f)),
        HirExpression::Ternary {
            condition,
            true_expr,
            false_expr,
        } => {
            walk_expr(condition, f);
            walk_expr(true_expr, f);
            walk_expr(false_expr, f);
        }
        HirExpression::Cast(c) => walk_expr(&c.expr, f),
        HirExpression::ArrayRepeat { value, count } => {
            walk_expr(value, f);
            walk_expr(count, f);
        }
        HirExpression::If(i) => {
            walk_expr(&i.condition, f);
            walk_expr(&i.then_expr, f);
            walk_expr(&i.else_expr, f);
        }
        HirExpression::Match(m) => {
            walk_expr(&m.expr, f);
            for arm in &m.arms {
                walk_expr(&arm.expr, f);
            }
        }
        HirExpression::Block {
            statements,
            result_expr,
        } => {
            walk_statements(statements, f);
            walk_expr(result_expr, f);
        }
        _ => {}
    }
}

fn walk_statements(statements: &[HirStatement], f: &mut dyn FnMut(&HirExpression)) {
    for s in statements {
        match s {
            HirStatement::Assignment(a) => walk_expr(&a.rhs, f),
            HirStatement::Let(l) => walk_expr(&l.value, f),
            HirStatement::Expression(e) => walk_expr(e, f),
            HirStatement::Block(b) => walk_statements(b, f),
            HirStatement::If(i) => {
                walk_expr(&i.condition, f);
                walk_statements(&i.then_statements, f);
                if let Some(e) = &i.else_statements {
                    walk_statements(e, f);
                }
            }
            HirStatement::Match(m) => {
                walk_expr(&m.expr, f);
                for arm in &m.arms {
                    walk_statements(&arm.statements, f);
                }
            }
            HirStatement::For(l) => walk_statements(&l.body, f),
            _ => {}
        }
    }
}

fn block_uses_streams(statements: &[HirStatement], streams: &HashMap<PortId, StreamPort>) -> bool {
    let mut found = false;
    walk_statements(statements, &mut |e| {
        if stream_call(e, streams).is_some() {
            found = true;
        }
    });
    found
}

/// Lower every stream port of the async entities of `hir` (see the module
/// documentation). `others` are further design files whose ids must not be
/// reused. Returns one message per problem, and a warning for each cycle
/// of channels that can never move a value (see [`receive_cycles`]).
pub fn lower_async_streams(hir: &mut Hir, others: &[&Hir]) -> StreamLowering {
    let mut errors = Vec::new();
    let mut warnings = Vec::new();
    if !has_async_streams(hir) {
        return StreamLowering { errors, warnings };
    }
    let mut all_hirs: Vec<&Hir> = vec![hir];
    all_hirs.extend_from_slice(others);
    let mut ids = Ids::new(&all_hirs);

    // 1. Ports: `x: stream<T>` -> `x: T`, `x__req`, `x__ack`
    let mut entity_streams: HashMap<crate::hir::EntityId, HashMap<PortId, StreamPort>> =
        HashMap::new();
    for entity in hir.entities.iter_mut().filter(|e| e.is_async) {
        let mut streams = HashMap::new();
        let mut added = Vec::new();
        for port in &mut entity.ports {
            let HirType::Stream(inner) = &port.port_type else {
                continue;
            };
            let output = match port.direction {
                HirPortDirection::Output => true,
                HirPortDirection::Input => false,
                _ => {
                    errors.push(format!(
                        "stream port `{p}` of async entity `{e}` must be `in` or `out`",
                        p = port.name,
                        e = entity.name
                    ));
                    continue;
                }
            };
            port.port_type = (**inner).clone();
            let (req_dir, ack_dir) = if output {
                (HirPortDirection::Output, HirPortDirection::Input)
            } else {
                (HirPortDirection::Input, HirPortDirection::Output)
            };
            let req = ids.port();
            let ack = ids.port();
            added.push(new_port(
                req,
                format!("{}{STREAM_REQ_SUFFIX}", port.name),
                req_dir,
                HirType::Bit(1),
            ));
            added.push(new_port(
                ack,
                format!("{}{STREAM_ACK_SUFFIX}", port.name),
                ack_dir,
                HirType::Bit(1),
            ));
            streams.insert(
                port.id,
                StreamPort {
                    name: port.name.clone(),
                    output,
                    data: port.id,
                    req,
                    ack,
                },
            );
        }
        if !streams.is_empty() {
            if !entity.ports.iter().any(|p| p.name == NCL_TICK_PORT) {
                added.push(new_port(
                    ids.port(),
                    NCL_TICK_PORT.to_string(),
                    HirPortDirection::Input,
                    HirType::Bit(1),
                ));
            }
            entity.ports.extend(added);
            entity_streams.insert(entity.id, streams);
        }
    }

    let entity_port_types: HashMap<PortId, HirType> = hir
        .entities
        .iter()
        .filter(|e| entity_streams.contains_key(&e.id))
        .flat_map(|e| e.ports.iter().map(|p| (p.id, p.port_type.clone())))
        .collect();
    let entity_names: HashMap<crate::hir::EntityId, String> = hir
        .entities
        .iter()
        .map(|e| (e.id, e.name.clone()))
        .collect();

    // 2. Implementations of self-timed entities: phase registers, `on()`
    //    rewrite, the ready signal and the phase ports
    let mut entity_waits: HashMap<crate::hir::EntityId, HashSet<(String, String)>> = HashMap::new();
    for implementation in &mut hir.implementations {
        let Some(streams) = entity_streams.get(&implementation.entity) else {
            continue;
        };
        let entity = entity_names[&implementation.entity].clone();
        let mut ordered: Vec<&StreamPort> = streams.values().collect();
        ordered.sort_by(|a, b| a.name.cmp(&b.name));
        let mut phase = HashMap::new();
        let mut data = HashMap::new();
        for s in &ordered {
            let id = ids.signal();
            implementation.signals.push(new_signal(
                id,
                phase_signal(&s.name),
                HirType::Bit(1),
                Some(0),
            ));
            phase.insert(s.data, id);
            if s.output {
                // the value sent, a register of the sender driving the port
                let port_type = entity_port_types[&s.data].clone();
                let id = ids.signal();
                implementation.signals.push(new_signal(
                    id,
                    format!("__ncl_data_{}", s.name),
                    port_type,
                    Some(0),
                ));
                data.insert(s.data, id);
                implementation.assignments.push(assign(
                    &mut ids,
                    HirLValue::Port(s.data),
                    HirExpression::Signal(id),
                    HirAssignmentType::Combinational,
                ));
            }
        }
        let mut lowering = Lowering {
            entity: &entity,
            streams,
            phase,
            data,
            used: HashMap::new(),
            site_conds: HashMap::new(),
            errors: Vec::new(),
            ids: &mut ids,
            arbiters: Vec::new(),
        };
        let mut on_blocks = 0;
        for block in &mut implementation.event_blocks {
            if !block.triggers.is_empty() {
                if block_uses_streams(&block.statements, streams) {
                    lowering.errors.push(format!(
                        "stream `send`/`recv` in a clocked block of async entity `{entity}`; use `on()`"
                    ));
                }
                continue;
            }
            on_blocks += 1;
            lower_on_block(&mut lowering, block);
        }
        if on_blocks > 1 {
            lowering.errors.push(format!(
                "async entity `{entity}` with stream ports has {on_blocks} `on()` blocks; a self-timed block has one"
            ));
        }
        if block_uses_streams(&implementation.statements, streams)
            || implementation
                .assignments
                .iter()
                .any(|a| contains_stream_call(&a.rhs, streams))
        {
            lowering.errors.push(format!(
                "stream `send`/`recv` outside `on()` in async entity `{entity}`"
            ));
        }
        let mut used = std::mem::take(&mut lowering.used);
        // Which outputs this block only sends in steps that also receive
        // from an input: (input, output)
        let sites = std::mem::take(&mut lowering.site_conds);
        let mut waits = HashSet::new();
        for i in ordered.iter().filter(|s| !s.output) {
            for o in ordered.iter().filter(|s| s.output) {
                if let (Some(ci), Some(co)) = (sites.get(&i.data), sites.get(&o.data)) {
                    if ci.is_subset(co) {
                        waits.insert((i.name.clone(), o.name.clone()));
                    }
                }
            }
        }
        entity_waits.insert(implementation.entity, waits);
        let phase = std::mem::take(&mut lowering.phase);
        let mut arbiters = std::mem::take(&mut lowering.arbiters);
        errors.append(&mut lowering.errors);
        lower_ready(
            implementation,
            &mut arbiters,
            &mut used,
            &phase,
            streams,
            &entity,
            &mut errors,
        );
        lower_taken(
            implementation,
            &arbiters,
            &mut used,
            &TakeBlock {
                entity: &entity,
                streams,
                port_types: &entity_port_types,
            },
            &mut ids,
            &mut errors,
        );

        let settle = grant_settle_needed(implementation, &arbiters, &used, streams);

        // ready = AND over streams of (!used | ok)
        let mut terms = Vec::new();
        for s in &ordered {
            let ph = HirExpression::Signal(phase[&s.data]);
            let Some(cond) = used.get(&s.data) else {
                errors.push(format!(
                    "stream `{n}` of async entity `{entity}` is never {verb}",
                    n = s.name,
                    verb = if s.output { "sent to" } else { "received from" }
                ));
                continue;
            };
            let ok = if s.output {
                // empty: the receiver has taken the last value
                bin(ph, HirBinaryOp::Equal, HirExpression::Port(s.ack))
            } else {
                // full: the sender has put a new value
                bin(HirExpression::Port(s.req), HirBinaryOp::NotEqual, ph)
            };
            terms.push(bin(not(cond.clone()), HirBinaryOp::LogicalOr, ok));
            // the phase port follows the phase register
            let port = if s.output { s.req } else { s.ack };
            implementation.assignments.push(assign(
                &mut ids,
                HirLValue::Port(port),
                HirExpression::Signal(phase[&s.data]),
                HirAssignmentType::Combinational,
            ));
        }
        // Arbiters: requests and grants. Which of several waiting requests a
        // real arbiter grants is not defined, so the behavioural grant is a
        // fair round-robin: the arm after the one granted last wins. A
        // design must not rely on that order. Synthesis replaces the grant
        // with a mutual-exclusion element (see `skalp_lir::ncl_expand`).
        // A step that reaches an arbiter waits for one of its grants.
        for (k, arb) in arbiters.iter().enumerate() {
            let width = arb.requests.len() as u32;
            implementation.signals.push(new_signal(
                arb.req,
                format!("{NCL_ARBITER_PREFIX}{k}_req"),
                HirType::Bit(width),
                None,
            ));
            implementation.signals.push(new_signal(
                arb.gnt,
                format!("{NCL_ARBITER_PREFIX}{k}_gnt"),
                HirType::Bit(width),
                None,
            ));
            let bit = |sig: SignalId, i: usize| {
                HirExpression::Index(
                    Box::new(HirExpression::Signal(sig)),
                    Box::new(HirExpression::Literal(HirLiteral::Integer(i as u64))),
                )
            };
            // Concatenations list the most significant bit first
            let req_bits: Vec<HirExpression> = arb.requests.iter().rev().cloned().collect();
            implementation.assignments.push(assign(
                &mut ids,
                HirLValue::Signal(arb.req),
                if req_bits.len() == 1 {
                    req_bits[0].clone()
                } else {
                    HirExpression::Concat(req_bits)
                },
                HirAssignmentType::Combinational,
            ));
            let n = arb.requests.len();
            implementation.signals.push(new_signal(
                arb.last,
                format!("{NCL_ARBITER_PREFIX}{k}_last"),
                HirType::Bit(width),
                // as if the last arm had been granted: arm 0 goes first
                Some(1u64 << (n - 1)),
            ));
            // Arm i wins when it requests and, for the arm l granted last,
            // none of the arms after l and before i (going round) requests
            let gnt_bits: Vec<HirExpression> = (0..n)
                .rev()
                .map(|i| {
                    let after_last = (0..n)
                        .map(|l| {
                            let mut terms = vec![bit(arb.last, l)];
                            let mut j = (l + 1) % n;
                            while j != i {
                                terms.push(not(bit(arb.req, j)));
                                j = (j + 1) % n;
                            }
                            all(&terms)
                        })
                        .reduce(|a, b| bin(a, HirBinaryOp::LogicalOr, b))
                        .unwrap_or_else(truth);
                    bin(bit(arb.req, i), HirBinaryOp::LogicalAnd, after_last)
                })
                .collect();
            implementation.assignments.push(assign(
                &mut ids,
                HirLValue::Signal(arb.gnt),
                if gnt_bits.len() == 1 {
                    gnt_bits[0].clone()
                } else {
                    HirExpression::Concat(gnt_bits)
                },
                HirAssignmentType::Combinational,
            ));
            let any = (0..arb.requests.len())
                .map(|i| bit(arb.gnt, i))
                .reduce(|a, b| bin(a, HirBinaryOp::LogicalOr, b))
                .unwrap_or_else(truth);
            // A named "some arm granted" that synthesis delays until what
            // the grant decides has settled (see `grant_settle_needed`)
            let any = if settle {
                let id = ids.signal();
                implementation.signals.push(new_signal(
                    id,
                    format!("{NCL_ARBITER_PREFIX}{k}{NCL_GRANT_ANY_SUFFIX}"),
                    HirType::Bit(1),
                    None,
                ));
                implementation.assignments.push(assign(
                    &mut ids,
                    HirLValue::Signal(id),
                    any,
                    HirAssignmentType::Combinational,
                ));
                HirExpression::Signal(id)
            } else {
                any
            };
            terms.push(bin(not(arb.reached.clone()), HirBinaryOp::LogicalOr, any));
        }

        let ready = ids.signal();
        implementation.signals.push(new_signal(
            ready,
            NCL_READY_SIGNAL.to_string(),
            HirType::Bit(1),
            None,
        ));
        implementation.assignments.push(assign(
            &mut ids,
            HirLValue::Signal(ready),
            all(&terms),
            HirAssignmentType::Combinational,
        ));
    }

    // 3. Parents: wire the phase bits of each channel both ways
    for implementation in &mut hir.implementations {
        let parent = entity_names
            .get(&implementation.entity)
            .cloned()
            .unwrap_or_default();
        // instance name -> entity streams
        let instances: Vec<(String, crate::hir::EntityId, Option<crate::hir::VariableId>)> =
            implementation
                .instances
                .iter()
                .map(|i| (i.name.clone(), i.entity, i.variable_id))
                .collect();
        let by_variable: HashMap<crate::hir::VariableId, usize> = instances
            .iter()
            .enumerate()
            .filter_map(|(k, (_, _, v))| v.map(|v| (v, k)))
            .collect();
        let by_name: HashMap<&str, usize> = instances
            .iter()
            .enumerate()
            .map(|(k, (n, _, _))| (n.as_str(), k))
            .collect();
        let stream_by_name = |entity: crate::hir::EntityId, name: &str| -> Option<StreamPort> {
            entity_streams
                .get(&entity)?
                .values()
                .find(|s| s.name == name)
                .cloned()
        };
        // (receiver instance index, its stream input, sender instance index, its stream output)
        let mut links = Vec::new();
        let mut fed: HashSet<(usize, String)> = HashSet::new();
        for (k, inst) in implementation.instances.iter().enumerate() {
            let Some(streams) = entity_streams.get(&inst.entity) else {
                continue;
            };
            for s in streams.values().filter(|s| !s.output) {
                let Some(conn) = inst.connections.iter().find(|c| c.port == s.name) else {
                    errors.push(format!(
                        "stream input `{n}` of `inst {i}` is not connected (in entity `{parent}`)",
                        n = s.name,
                        i = inst.name
                    ));
                    continue;
                };
                let source = match &conn.expr {
                    HirExpression::FieldAccess { base, field } => {
                        let k = match base.as_ref() {
                            HirExpression::Variable(v) => by_variable.get(v).copied(),
                            HirExpression::Signal(_) | HirExpression::Port(_) => None,
                            _ => None,
                        }
                        .or_else(|| match base.as_ref() {
                            HirExpression::GenericParam(n) => by_name.get(n.as_str()).copied(),
                            _ => None,
                        });
                        k.and_then(|src| {
                            stream_by_name(instances[src].1, field)
                                .filter(|so| so.output)
                                .map(|so| (src, so))
                        })
                    }
                    _ => None,
                };
                let Some((src, out)) = source else {
                    errors.push(format!(
                        "stream input `{n}` of `inst {i}` must be connected to a stream output of another instance, as `{n}: other.out` (in entity `{parent}`)",
                        n = s.name,
                        i = inst.name
                    ));
                    continue;
                };
                if !fed.insert((src, out.name.clone())) {
                    errors.push(format!(
                        "stream output `{o}` of `inst {i}` feeds more than one stream input (in entity `{parent}`)",
                        o = out.name,
                        i = instances[src].0
                    ));
                    continue;
                }
                links.push((k, s.clone(), src, out));
            }
        }
        warnings.extend(receive_cycles(
            &parent,
            &links
                .iter()
                .map(|(k, s, src, out)| Link {
                    sender: *src,
                    output: out.name.clone(),
                    receiver: *k,
                    input: s.name.clone(),
                })
                .collect::<Vec<_>>(),
            |inst, input, output| {
                entity_waits
                    .get(&instances[inst].1)
                    .is_some_and(|w| w.contains(&(input.to_string(), output.to_string())))
            },
            |inst| instances[inst].0.as_str(),
        ));
        for (k, s, src, out) in links {
            let field = |inst: usize, port: String| HirExpression::FieldAccess {
                base: Box::new(match implementation.instances[inst].variable_id {
                    Some(v) => HirExpression::Variable(v),
                    None => {
                        HirExpression::GenericParam(implementation.instances[inst].name.clone())
                    }
                }),
                field: port,
            };
            let req = field(src, format!("{}{STREAM_REQ_SUFFIX}", out.name));
            let ack = field(k, format!("{}{STREAM_ACK_SUFFIX}", s.name));
            implementation.instances[k].connections.push(HirConnection {
                port: format!("{}{STREAM_REQ_SUFFIX}", s.name),
                expr: req,
            });
            implementation.instances[src]
                .connections
                .push(HirConnection {
                    port: format!("{}{STREAM_ACK_SUFFIX}", out.name),
                    expr: ack,
                });
        }
        // every stream output must feed something
        for (k, inst) in implementation.instances.iter().enumerate() {
            let Some(streams) = entity_streams.get(&inst.entity) else {
                continue;
            };
            for s in streams.values().filter(|s| s.output) {
                if !fed.contains(&(k, s.name.clone())) {
                    errors.push(format!(
                        "stream output `{n}` of `inst {i}` feeds no stream input (in entity `{parent}`)",
                        n = s.name,
                        i = inst.name
                    ));
                }
            }
        }
    }
    StreamLowering { errors, warnings }
}

/// What [`lower_async_streams`] found: errors stop the build, warnings do
/// not
#[derive(Debug, Default)]
pub struct StreamLowering {
    pub errors: Vec<String>,
    pub warnings: Vec<String>,
}

/// Most deadlock cycles reported per parent
const MAX_CYCLE_WARNINGS: usize = 16;

/// One channel between two instances of a parent
struct Link {
    sender: usize,
    output: String,
    receiver: usize,
    input: String,
}

/// Cycles of channels in which every block only sends on the next channel
/// in steps that also receive from the one before (`waits(instance, input,
/// output)`). Every channel starts empty, so no value ever goes round such a
/// cycle: the blocks on it wait for each other for ever. A warning, not an
/// error: the blocks may do other useful work, and a design may mean a
/// cycle that is never used. One message per cycle, naming its channels.
fn receive_cycles<'a>(
    parent: &str,
    links: &[Link],
    waits: impl Fn(usize, &str, &str) -> bool,
    name: impl Fn(usize) -> &'a str,
) -> Vec<String> {
    // Channel graph: link a -> link b when b leaves the block a enters, and
    // that block waits on a before it sends on b
    let next: Vec<Vec<usize>> = links
        .iter()
        .map(|a| {
            links
                .iter()
                .enumerate()
                .filter(|(_, b)| b.sender == a.receiver && waits(a.receiver, &a.input, &b.output))
                .map(|(j, _)| j)
                .collect()
        })
        .collect();
    // Each cycle once: found from its lowest-numbered channel, through
    // higher-numbered channels only
    let mut warnings = Vec::new();
    for start in 0..links.len() {
        if warnings.len() >= MAX_CYCLE_WARNINGS {
            break;
        }
        let mut path = vec![start];
        let mut iters = vec![0usize];
        while let Some(&at) = path.last() {
            let k = iters.last_mut().expect("parallel stacks");
            let Some(&to) = next[at].get(*k) else {
                path.pop();
                iters.pop();
                continue;
            };
            *k += 1;
            if to == start {
                let channels: Vec<String> = path
                    .iter()
                    .map(|&l| {
                        let l = &links[l];
                        format!(
                            "{}.{} -> {}.{}",
                            name(l.sender),
                            l.output,
                            name(l.receiver),
                            l.input
                        )
                    })
                    .collect();
                warnings.push(format!(
                    "possible deadlock in entity `{parent}`: streams {} form a cycle in which each block only sends after it receives; the channels start empty, so no value can go round it",
                    channels.join(", ")
                ));
            } else if to > start && !path.contains(&to) {
                path.push(to);
                iters.push(0);
            }
        }
    }
    warnings
}

fn lower_on_block(lowering: &mut Lowering<'_>, block: &mut HirEventBlock) {
    lowering.rewrite_block(&mut block.statements, &[]);
}
