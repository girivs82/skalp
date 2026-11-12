//! Register retiming on a mapped gate netlist.
//!
//! Registers move forward through logic (from a cell's inputs to its
//! outputs) to balance the delay between them, keeping the netlist's
//! behaviour cycle for cycle: every path from an input to an output keeps
//! its register count. Only forward moves are made, because a register that
//! moves forward through a cell gets an initial value that is exact: the
//! cell's function of the initial values of the registers it replaces. The
//! stages `#[pipeline(stages = N)]` adds sit at the inputs of an entity
//! ([`skalp_mir::pipelining`]), so forward moves are all it needs.
//!
//! The search is Leiserson and Saxe's FEAS run forwards: each node (a logic
//! cell) has a lag `r <= 0`, the number of times registers have moved
//! through it, and an edge from `u` to `v` holding `w` registers holds
//! `w + r(v) - r(u)` after retiming. While some path starting at a register
//! is longer than the target period, the first cell on it takes one more
//! move. A binary search over the period finds the shortest one reachable.
//!
//! What moves:
//! - Only plain registers (no enable, no reset) on one clock, inside the
//!   retiming regions (instance paths), whose outputs are not clocks,
//!   resets or primary outputs. Other registers stay where they are.
//! - Only logic cells inside the regions that have a known function. Hard
//!   blocks with state, pads and cells driving primary outputs stay put.
//! - A carry chain moves as one block, so no register lands inside it.
//! - Constants supply any number of registers (a register holding a
//!   constant is the constant), so they never block a move.

use crate::gate_netlist::{Cell, CellId, GateNetId, GateNetlist};
use crate::lir::PrimitiveType;
use crate::primitive_eval::{evaluate_primitive, primitive_for_cell};
use crate::tech_library::CellFunction;
use std::collections::{HashMap, HashSet};

/// The delays retiming balances, in picoseconds
pub trait RetimeDelays {
    /// Delay through `cell` from input `input` to output `output`, or `None`
    /// when the input does not affect that output
    fn arc_ps(&self, cell: &Cell, input: usize, output: usize) -> Option<i64>;
    /// Clock-to-output delay of a register
    fn clk_to_q_ps(&self) -> i64;
    /// Setup time of a register's data input
    fn setup_ps(&self) -> i64;
    /// Estimated routing delay of a connection on a net with `fanout` loads
    fn net_ps(&self, fanout: usize) -> i64;
}

/// Delays for a netlist without device timing: each logic cell counts as
/// one level, a carry chain step as a small fraction of one
#[derive(Debug, Clone, Copy)]
pub struct LevelDelays;

impl RetimeDelays for LevelDelays {
    fn arc_ps(&self, cell: &Cell, input: usize, output: usize) -> Option<i64> {
        if cell.function == Some(CellFunction::Ccu2) {
            // Carry in to carry out is the fast path; a sum output is a LUT
            // after the carry
            return Some(match (input, output) {
                (8, 2) => 20,
                (_, 2) => 120,
                _ => 100,
            });
        }
        Some(100)
    }
    fn clk_to_q_ps(&self) -> i64 {
        100
    }
    fn setup_ps(&self) -> i64 {
        0
    }
    fn net_ps(&self, _fanout: usize) -> i64 {
        0
    }
}

/// What to retime and how far
#[derive(Debug, Clone, Default)]
pub struct RetimeOptions {
    /// Instance paths whose registers and logic may move (`top.unit`)
    pub regions: Vec<String>,
    /// Stop once every register-to-register path fits this period; `None`
    /// makes it as short as the regions allow
    pub target_period_ps: Option<i64>,
}

/// What retiming did
#[derive(Debug, Clone, Default)]
pub struct RetimeReport {
    /// Longest path from a movable register, before and after
    pub period_before_ps: i64,
    pub period_after_ps: i64,
    /// Movable registers before, registers that replaced them after
    pub registers_before: usize,
    pub registers_after: usize,
    /// Logic cells registers moved through
    pub cells_crossed: usize,
}

/// Where an edge's value comes from
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Origin {
    /// Output `pin` of logic node `node`
    Node(usize, usize),
    /// A net driven by something that stays put
    Fixed(GateNetId),
    /// A constant net
    Const(GateNetId, bool),
}

/// Where an edge's value goes
#[derive(Debug, Clone, Copy)]
enum Sink {
    /// Input `pin` of logic node `node`
    Node(usize, usize),
    /// Input `pin` of a cell that stays put
    Cell(CellId, usize),
}

#[derive(Debug, Clone)]
struct Edge {
    origin: Origin,
    sink: Sink,
    /// Initial values of the registers on the edge, origin end first
    regs: Vec<bool>,
    /// Loads on the net leaving the origin, for the routing estimate
    fanout: usize,
}

struct Graph<'a> {
    netlist: &'a GateNetlist,
    /// Logic cells that registers may move through
    nodes: Vec<CellId>,
    /// Carry-chain block of each node (nodes of one block share a lag)
    block: Vec<usize>,
    blocks: Vec<Vec<usize>>,
    edges: Vec<Edge>,
    /// Per node, per input pin, the edge into it
    in_edges: Vec<Vec<Option<usize>>>,
    /// Per node, per output pin, the edges out of it
    out_edges: Vec<Vec<Vec<usize>>>,
    /// Registers the graph absorbed (removed when the result is applied)
    movable: Vec<CellId>,
    /// A register to copy for new ones
    template: Option<CellId>,
    /// Region of each node, for naming new registers
    node_region: Vec<usize>,
    regions: Vec<String>,
}

/// Retime the registers in `options.regions`. Returns `None` (leaving the
/// netlist unchanged) when there is nothing to move or nothing gained.
pub fn retime(
    netlist: &mut GateNetlist,
    delays: &dyn RetimeDelays,
    options: &RetimeOptions,
) -> Option<RetimeReport> {
    if options.regions.is_empty() {
        return None;
    }
    let graph = Graph::build(netlist, &options.regions)?;
    let zero = vec![0i64; graph.nodes.len()];
    let before = graph.period(&zero, delays);
    let lower = graph.lower_bound(delays);
    let mut best: Option<(i64, Vec<i64>)> = None;
    if let Some(target) = options.target_period_ps {
        if let Some(lags) = graph.feasible(target, delays) {
            best = Some((target, lags));
        }
    }
    if best.is_none() {
        // Shortest feasible period, to within 1%
        let (mut lo, mut hi) = (lower, before);
        while hi - lo > (hi / 100).max(10) {
            let mid = (lo + hi) / 2;
            match graph.feasible(mid, delays) {
                Some(lags) => {
                    hi = mid;
                    best = Some((mid, lags));
                }
                None => lo = mid,
            }
        }
    }
    let (_, lags) = best?;
    if lags.iter().all(|&r| r == 0) {
        return None;
    }
    let after = graph.period(&lags, delays);
    if tracing::enabled!(tracing::Level::DEBUG) {
        tracing::debug!(
            "retime: before\n  {}",
            graph.critical_path(&zero, delays).join("\n  ")
        );
        tracing::debug!(
            "retime: after\n  {}",
            graph.critical_path(&lags, delays).join("\n  ")
        );
    }
    if after >= before {
        return None;
    }
    let registers_before = graph.movable.len();
    let cells_crossed = lags.iter().filter(|&&r| r != 0).count();
    let regs = graph.realize(&lags)?;
    let plan = graph.plan(&regs)?;
    let registers_after = plan.apply(netlist);
    Some(RetimeReport {
        period_before_ps: before,
        period_after_ps: after,
        registers_before,
        registers_after,
        cells_crossed,
    })
}

/// The constant a tie cell drives
fn constant_of(cell: &Cell) -> Option<bool> {
    match cell.function {
        Some(CellFunction::TieHigh) => Some(true),
        Some(CellFunction::TieLow) => Some(false),
        _ => None,
    }
}

/// Initial value of a register cell
fn register_init(cell: &Cell) -> bool {
    cell.parameters
        .get("INIT")
        .is_some_and(|v| v.trim() == "1" || v.trim() == "1'b1")
}

/// Whether a logic cell has a function retiming can evaluate
fn is_evaluable(cell: &Cell) -> bool {
    if cell.clock.is_some() || cell.reset.is_some() || cell.outputs.is_empty() {
        return false;
    }
    if matches!(
        cell.function,
        Some(
            CellFunction::Ram
                | CellFunction::DistRam16x4
                | CellFunction::InputPad
                | CellFunction::OutputPad
                | CellFunction::TieHigh
                | CellFunction::TieLow
                | CellFunction::Tristate
        )
    ) {
        return false;
    }
    matches!(
        primitive_for_cell(cell),
        PrimitiveType::And { .. }
            | PrimitiveType::Or { .. }
            | PrimitiveType::Nand { .. }
            | PrimitiveType::Nor { .. }
            | PrimitiveType::Xor
            | PrimitiveType::Xnor
            | PrimitiveType::Inv
            | PrimitiveType::AndNot
            | PrimitiveType::OrNot
            | PrimitiveType::Aoi21
            | PrimitiveType::Aoi22
            | PrimitiveType::Oai21
            | PrimitiveType::Oai22
            | PrimitiveType::Buf
            | PrimitiveType::Mux2
            | PrimitiveType::Mux4
            | PrimitiveType::MuxN { .. }
            | PrimitiveType::HalfAdder
            | PrimitiveType::FullAdder
            | PrimitiveType::CarryCell
            | PrimitiveType::Ccu2 { .. }
            | PrimitiveType::Multiplier { .. }
            | PrimitiveType::Lut4 { .. }
            | PrimitiveType::Lut6 { .. }
    )
}

fn region_of(path: &str, regions: &[String]) -> Option<usize> {
    regions.iter().position(|r| {
        path.len() > r.len() && path.starts_with(r.as_str()) && path.as_bytes()[r.len()] == b'.'
    })
}

impl<'a> Graph<'a> {
    fn build(netlist: &'a GateNetlist, regions: &[String]) -> Option<Self> {
        let resolve = |net: GateNetId| netlist.resolve_alias(net);
        let net_count = netlist.nets.len();
        let mut driver: Vec<Option<(CellId, usize)>> = vec![None; net_count];
        let mut fanout = vec![0usize; net_count];
        // Nets that must keep their driver: clocks, resets, primary outputs
        let mut protected = vec![false; net_count];
        for cell in &netlist.cells {
            for (pin, &net) in cell.outputs.iter().enumerate() {
                driver[resolve(net).0 as usize] = Some((cell.id, pin));
            }
            for &net in &cell.inputs {
                fanout[resolve(net).0 as usize] += 1;
            }
            for &net in cell.clock.iter().chain(cell.reset.iter()) {
                protected[resolve(net).0 as usize] = true;
            }
        }
        for &net in &netlist.outputs {
            protected[resolve(net).0 as usize] = true;
        }
        for (index, net) in netlist.nets.iter().enumerate() {
            if net.is_output || net.is_clock || net.is_reset || net.is_detection {
                protected[index] = true;
            }
        }

        // The clock most region registers use
        let mut clocks: HashMap<GateNetId, usize> = HashMap::new();
        for cell in &netlist.cells {
            if cell.function == Some(CellFunction::Dff) && region_of(&cell.path, regions).is_some()
            {
                if let Some(clock) = cell.clock {
                    *clocks.entry(resolve(clock)).or_default() += 1;
                }
            }
        }
        let clock = clocks.into_iter().max_by_key(|&(net, n)| (n, net.0))?.0;

        let is_movable_register = |cell: &Cell| {
            cell.function == Some(CellFunction::Dff)
                && cell.clock.map(resolve) == Some(clock)
                && cell.reset.is_none()
                && cell.inputs.len() == 1
                && cell.outputs.len() == 1
                && region_of(&cell.path, regions).is_some()
                && {
                    let q = resolve(cell.outputs[0]).0 as usize;
                    !protected[q]
                        && fanout[q] > 0
                        && resolve(cell.inputs[0]) != resolve(cell.outputs[0])
                }
        };
        let movable: Vec<CellId> = netlist
            .cells
            .iter()
            .filter(|c| is_movable_register(c))
            .map(|c| c.id)
            .collect();
        if movable.is_empty() {
            return None;
        }
        let is_movable: HashSet<CellId> = movable.iter().copied().collect();

        let mut node_of: HashMap<CellId, usize> = HashMap::new();
        let mut nodes = Vec::new();
        let mut node_region = Vec::new();
        for cell in &netlist.cells {
            let Some(region) = region_of(&cell.path, regions) else {
                continue;
            };
            if is_evaluable(cell)
                && !cell
                    .outputs
                    .iter()
                    .any(|&n| protected[resolve(n).0 as usize])
            {
                node_of.insert(cell.id, nodes.len());
                nodes.push(cell.id);
                node_region.push(region);
            }
        }

        // Carry chains: a carry output feeding a carry input joins two cells
        let mut parent: Vec<usize> = (0..nodes.len()).collect();
        fn find(parent: &mut [usize], x: usize) -> usize {
            let mut root = x;
            while parent[root] != root {
                root = parent[root];
            }
            let mut x = x;
            while parent[x] != root {
                let next = parent[x];
                parent[x] = root;
                x = next;
            }
            root
        }
        for (index, &id) in nodes.iter().enumerate() {
            let cell = &netlist.cells[id.0 as usize];
            if cell.function != Some(CellFunction::Ccu2) || cell.inputs.len() < 9 {
                continue;
            }
            let carry_in = resolve(cell.inputs[8]);
            if let Some((from, 2)) = driver[carry_in.0 as usize] {
                if let Some(&other) = node_of.get(&from) {
                    let (a, b) = (find(&mut parent, index), find(&mut parent, other));
                    parent[a] = b;
                }
            }
        }
        let mut block_ids: HashMap<usize, usize> = HashMap::new();
        let mut blocks: Vec<Vec<usize>> = Vec::new();
        let mut block = vec![0; nodes.len()];
        for (index, slot) in block.iter_mut().enumerate() {
            let root = find(&mut parent, index);
            let id = *block_ids.entry(root).or_insert_with(|| {
                blocks.push(Vec::new());
                blocks.len() - 1
            });
            blocks[id].push(index);
            *slot = id;
        }

        // Edges: from every sink pin back through movable registers
        let mut edges = Vec::new();
        let mut in_edges: Vec<Vec<Option<usize>>> = nodes
            .iter()
            .map(|id| vec![None; netlist.cells[id.0 as usize].inputs.len()])
            .collect();
        let mut out_edges: Vec<Vec<Vec<usize>>> = nodes
            .iter()
            .map(|id| vec![Vec::new(); netlist.cells[id.0 as usize].outputs.len()])
            .collect();
        for cell in &netlist.cells {
            if is_movable.contains(&cell.id) {
                continue;
            }
            let sink_node = node_of.get(&cell.id).copied();
            for (pin, &input) in cell.inputs.iter().enumerate() {
                let mut net = resolve(input);
                let mut regs = Vec::new();
                let mut steps = 0;
                let origin = loop {
                    match driver[net.0 as usize] {
                        Some((d, _)) if is_movable.contains(&d) && steps < 100_000 => {
                            let reg = &netlist.cells[d.0 as usize];
                            regs.push(register_init(reg));
                            net = resolve(reg.inputs[0]);
                            steps += 1;
                        }
                        Some((d, out)) => {
                            let source = &netlist.cells[d.0 as usize];
                            if let Some(&node) = node_of.get(&d) {
                                break Origin::Node(node, out);
                            }
                            match constant_of(source) {
                                Some(value) => break Origin::Const(net, value),
                                None => break Origin::Fixed(net),
                            }
                        }
                        None => break Origin::Fixed(net),
                    }
                };
                if sink_node.is_none() && regs.is_empty() && !matches!(origin, Origin::Node(..)) {
                    continue;
                }
                regs.reverse();
                // A register on a constant is the constant
                if let Origin::Const(..) = origin {
                    regs.clear();
                }
                let sink = match sink_node {
                    Some(node) => Sink::Node(node, pin),
                    None => Sink::Cell(cell.id, pin),
                };
                let index = edges.len();
                edges.push(Edge {
                    origin,
                    sink,
                    regs,
                    fanout: 0,
                });
                if let Some(node) = sink_node {
                    in_edges[node][pin] = Some(index);
                }
                if let Origin::Node(node, out) = origin {
                    out_edges[node][out].push(index);
                }
            }
        }
        // Routing estimate by the fanout of the net leaving the origin
        for edge in &mut edges {
            let net = match edge.origin {
                Origin::Node(node, out) => {
                    resolve(netlist.cells[nodes[node].0 as usize].outputs[out])
                }
                Origin::Fixed(net) | Origin::Const(net, _) => net,
            };
            edge.fanout = fanout[net.0 as usize];
        }
        let template = movable.first().copied();
        Some(Self {
            netlist,
            nodes,
            block,
            blocks,
            edges,
            in_edges,
            out_edges,
            movable,
            template,
            node_region,
            regions: regions.to_vec(),
        })
    }

    fn cell(&self, node: usize) -> &Cell {
        &self.netlist.cells[self.nodes[node].0 as usize]
    }

    /// Registers on an edge after retiming with `lags`
    fn weight(&self, edge: &Edge, lags: &[i64]) -> i64 {
        if let Origin::Const(..) = edge.origin {
            return i64::MAX / 4;
        }
        let from = match edge.origin {
            Origin::Node(node, _) => lags[node],
            _ => 0,
        };
        let to = match edge.sink {
            Sink::Node(node, _) => lags[node],
            Sink::Cell(..) => 0,
        };
        edge.regs.len() as i64 + to - from
    }

    fn net_delay(&self, edge: &Edge, delays: &dyn RetimeDelays) -> i64 {
        delays.net_ps(edge.fanout)
    }

    /// Longest delay from each node input to the next register, with
    /// `lags` applied
    fn departures(&self, lags: &[i64], delays: &dyn RetimeDelays) -> Vec<Vec<i64>> {
        let mut memo: Vec<Vec<Option<i64>>> = self
            .in_edges
            .iter()
            .map(|pins| vec![None; pins.len()])
            .collect();
        let mut done = vec![false; self.nodes.len()];
        let mut on_stack = vec![false; self.nodes.len()];
        for start in 0..self.nodes.len() {
            if done[start] {
                continue;
            }
            // Iterative post-order over zero-register edges
            let mut stack = vec![(start, false)];
            while let Some((node, expanded)) = stack.pop() {
                if done[node] {
                    continue;
                }
                if expanded {
                    on_stack[node] = false;
                    let cell = self.cell(node);
                    let outs: Vec<i64> = (0..cell.outputs.len())
                        .map(|out| {
                            self.out_edges[node][out]
                                .iter()
                                .map(|&e| {
                                    let edge = &self.edges[e];
                                    let net = self.net_delay(edge, delays);
                                    if self.weight(edge, lags) > 0 {
                                        return net + delays.setup_ps();
                                    }
                                    match edge.sink {
                                        Sink::Node(x, pin) => net + memo[x][pin].unwrap_or(0),
                                        Sink::Cell(c, _) => {
                                            let sink = &self.netlist.cells[c.0 as usize];
                                            net + if sink.clock.is_some() {
                                                delays.setup_ps()
                                            } else {
                                                0
                                            }
                                        }
                                    }
                                })
                                .max()
                                .unwrap_or(0)
                        })
                        .collect();
                    for (pin, slot) in memo[node].iter_mut().enumerate() {
                        let worst = outs
                            .iter()
                            .enumerate()
                            .filter_map(|(out, &tail)| {
                                delays.arc_ps(cell, pin, out).map(|arc| arc + tail)
                            })
                            .max()
                            .unwrap_or(0);
                        *slot = Some(worst);
                    }
                    done[node] = true;
                    continue;
                }
                if on_stack[node] {
                    // A zero-register loop: combinational, cannot be timed
                    continue;
                }
                on_stack[node] = true;
                stack.push((node, true));
                for outs in &self.out_edges[node] {
                    for &e in outs {
                        let edge = &self.edges[e];
                        if let Sink::Node(x, _) = edge.sink {
                            if !done[x] && !on_stack[x] && self.weight(edge, lags) == 0 {
                                stack.push((x, false));
                            }
                        }
                    }
                }
            }
        }
        memo.into_iter()
            .map(|pins| pins.into_iter().map(|d| d.unwrap_or(0)).collect())
            .collect()
    }

    /// Delay of the path from something that stays put (a register or a
    /// pad outside the regions, or a cell that cannot move) through `edge`
    /// when no register sits on it. Retiming cannot shorten such a path,
    /// only keep registers from leaving it.
    fn fixed_path(
        &self,
        edge: &Edge,
        lags: &[i64],
        departures: &[Vec<i64>],
        delays: &dyn RetimeDelays,
    ) -> Option<i64> {
        let (Origin::Fixed(_), Sink::Node(x, pin)) = (edge.origin, edge.sink) else {
            return None;
        };
        (self.weight(edge, lags) == 0)
            .then(|| delays.clk_to_q_ps() + self.net_delay(edge, delays) + departures[x][pin])
    }

    /// Delay of the path starting at the register(s) on `edge`, if any
    fn register_path(
        &self,
        edge: &Edge,
        lags: &[i64],
        departures: &[Vec<i64>],
        delays: &dyn RetimeDelays,
    ) -> Option<i64> {
        if matches!(edge.origin, Origin::Const(..)) || self.weight(edge, lags) < 1 {
            return None;
        }
        let net = self.net_delay(edge, delays);
        Some(
            delays.clk_to_q_ps()
                + net
                + match edge.sink {
                    Sink::Node(x, pin) => departures[x][pin],
                    Sink::Cell(c, _) => {
                        if self.netlist.cells[c.0 as usize].clock.is_some() {
                            delays.setup_ps()
                        } else {
                            0
                        }
                    }
                },
        )
    }

    /// Longest path in the regions: from a movable register's position, or
    /// from something that stays put into the regions' logic
    fn period(&self, lags: &[i64], delays: &dyn RetimeDelays) -> i64 {
        let departures = self.departures(lags, delays);
        self.edges
            .iter()
            .filter_map(|e| {
                self.register_path(e, lags, &departures, delays)
                    .or_else(|| self.fixed_path(e, lags, &departures, delays))
            })
            .max()
            .unwrap_or(0)
    }

    /// The cells on the longest path, for the debug log
    fn critical_path(&self, lags: &[i64], delays: &dyn RetimeDelays) -> Vec<String> {
        let departures = self.departures(lags, delays);
        let worst = self
            .edges
            .iter()
            .filter_map(|e| {
                let t = self
                    .register_path(e, lags, &departures, delays)
                    .or_else(|| self.fixed_path(e, lags, &departures, delays))?;
                Some((t, e))
            })
            .max_by_key(|(t, _)| *t);
        let mut steps = Vec::new();
        let Some((t, mut edge)) = worst else {
            return steps;
        };
        steps.push(format!(
            "{t} ps from {}",
            match edge.origin {
                Origin::Fixed(net) =>
                    format!("fixed net {}", self.netlist.nets[net.0 as usize].name),
                _ => "a register".to_string(),
            }
        ));
        while let Sink::Node(x, pin) = edge.sink {
            let cell = self.cell(x);
            steps.push(format!(
                "{} {} (lag {})",
                cell.cell_type, cell.path, lags[x]
            ));
            // Follow the output edge that sets the departure
            let mut next = None;
            let mut best = i64::MIN;
            for out in 0..cell.outputs.len() {
                let Some(arc) = delays.arc_ps(cell, pin, out) else {
                    continue;
                };
                for &e in &self.out_edges[x][out] {
                    let candidate = &self.edges[e];
                    if self.weight(candidate, lags) != 0 {
                        continue;
                    }
                    let tail = match candidate.sink {
                        Sink::Node(y, p) => departures[y][p],
                        Sink::Cell(..) => 0,
                    };
                    if arc + tail > best {
                        best = arc + tail;
                        next = Some(candidate);
                    }
                }
            }
            match next {
                Some(e) => edge = e,
                None => break,
            }
            if steps.len() > 400 {
                break;
            }
        }
        steps
    }

    /// No retiming gets below the slowest single cell between two registers
    fn lower_bound(&self, delays: &dyn RetimeDelays) -> i64 {
        (0..self.nodes.len())
            .map(|node| {
                let cell = self.cell(node);
                let worst = (0..cell.inputs.len())
                    .flat_map(|i| (0..cell.outputs.len()).map(move |o| (i, o)))
                    .filter_map(|(i, o)| delays.arc_ps(cell, i, o))
                    .max()
                    .unwrap_or(0);
                delays.clk_to_q_ps() + worst + delays.setup_ps()
            })
            .max()
            .unwrap_or(0)
    }

    /// Lags that bring every register path within `period`, if forward moves
    /// can
    fn feasible(&self, period: i64, delays: &dyn RetimeDelays) -> Option<Vec<i64>> {
        let mut lags = vec![0i64; self.nodes.len()];
        let limit = 4 * self.nodes.len().min(500) + 16;
        for _ in 0..limit {
            let departures = self.departures(&lags, delays);
            let mut late = vec![false; self.nodes.len()];
            let mut any = false;
            for edge in &self.edges {
                // Too long a path from something that stays put only grows
                // as registers move on
                if self
                    .fixed_path(edge, &lags, &departures, delays)
                    .is_some_and(|t| t > period)
                {
                    return None;
                }
                if let Some(t) = self.register_path(edge, &lags, &departures, delays) {
                    if t > period {
                        if let Sink::Node(x, _) = edge.sink {
                            late[x] = true;
                            any = true;
                        } else {
                            // A register feeding a cell that stays put directly
                            return None;
                        }
                    }
                }
            }
            if !any {
                return Some(lags);
            }
            if !self.close_moves(&mut late, &lags) {
                return None;
            }
            for (node, &moving) in late.iter().enumerate() {
                if moving {
                    lags[node] -= 1;
                }
            }
        }
        None
    }

    /// Grow the set of nodes taking a move until every edge stays legal
    /// (no negative register count). Returns false if no node can move.
    fn close_moves(&self, moving: &mut [bool], lags: &[i64]) -> bool {
        // A node moves with its whole carry block, and pulls along every
        // node feeding it through an edge without registers
        loop {
            let mut changed = false;
            for node in 0..self.nodes.len() {
                if !moving[node] {
                    continue;
                }
                for &other in &self.blocks[self.block[node]] {
                    if !moving[other] {
                        moving[other] = true;
                        changed = true;
                    }
                }
                for &e in self.in_edges[node].iter().flatten() {
                    let edge = &self.edges[e];
                    if let Origin::Node(u, _) = edge.origin {
                        if !moving[u] && self.weight(edge, lags) == 0 {
                            moving[u] = true;
                            changed = true;
                        }
                    }
                }
            }
            if !changed {
                break;
            }
        }
        // A node fed directly by something that stays put cannot move, nor
        // can anything that depends on it moving
        let mut blocked = vec![false; self.nodes.len()];
        loop {
            let mut changed = false;
            for node in 0..self.nodes.len() {
                if !moving[node] || blocked[node] {
                    continue;
                }
                let stuck = self.in_edges[node].iter().flatten().any(|&e| {
                    let edge = &self.edges[e];
                    self.weight(edge, lags) == 0
                        && match edge.origin {
                            Origin::Fixed(_) => true,
                            Origin::Node(u, _) => blocked[u],
                            Origin::Const(..) => false,
                        }
                }) || self.blocks[self.block[node]].iter().any(|&o| blocked[o]);
                if stuck {
                    blocked[node] = true;
                    changed = true;
                }
            }
            if !changed {
                break;
            }
        }
        let mut any = false;
        for node in 0..self.nodes.len() {
            if blocked[node] {
                moving[node] = false;
            }
            any |= moving[node];
        }
        any
    }

    /// Make the moves `lags` asks for one at a time, carrying initial values
    /// through each cell. Returns the edges' final registers.
    fn realize(&self, lags: &[i64]) -> Option<Vec<Vec<bool>>> {
        let mut regs: Vec<Vec<bool>> = self.edges.iter().map(|e| e.regs.clone()).collect();
        let mut done = vec![0i64; self.nodes.len()];
        loop {
            let mut progress = false;
            let mut remaining = false;
            for node in 0..self.nodes.len() {
                while done[node] > lags[node] {
                    let pins = &self.in_edges[node];
                    let ready = pins.iter().all(|e| match e {
                        Some(e) => {
                            matches!(self.edges[*e].origin, Origin::Const(..))
                                || !regs[*e].is_empty()
                        }
                        None => true,
                    });
                    if !ready {
                        remaining = true;
                        break;
                    }
                    let inputs: Vec<bool> = pins
                        .iter()
                        .map(|e| match e {
                            Some(e) => match self.edges[*e].origin {
                                Origin::Const(_, value) => value,
                                _ => regs[*e].pop().expect("checked ready"),
                            },
                            None => false,
                        })
                        .collect();
                    let cell = self.cell(node);
                    let outputs = evaluate_primitive(&primitive_for_cell(cell), &inputs);
                    for (out, edges) in self.out_edges[node].iter().enumerate() {
                        let value = outputs.get(out).copied().unwrap_or(false);
                        for &e in edges {
                            regs[e].insert(0, value);
                        }
                    }
                    done[node] -= 1;
                    progress = true;
                }
            }
            if !remaining {
                break;
            }
            if !progress {
                return None;
            }
        }
        Some(regs)
    }

    /// What applying the retimed registers takes, owned so the netlist can
    /// then be changed
    fn plan(&self, regs: &[Vec<bool>]) -> Option<Plan> {
        let template = self.netlist.cells[self.template?.0 as usize].clone();
        let fallback = template
            .path
            .rsplit_once('.')
            .map_or(template.path.clone(), |(p, _)| p.to_string());
        let edges = self
            .edges
            .iter()
            .zip(regs)
            .map(|(edge, regs)| {
                let root = match edge.origin {
                    Origin::Node(node, out) => self.netlist.resolve_alias(
                        self.netlist.cells[self.nodes[node].0 as usize].outputs[out],
                    ),
                    Origin::Fixed(net) | Origin::Const(net, _) => net,
                };
                let prefix = match (edge.origin, edge.sink) {
                    (Origin::Node(node, _), _) | (_, Sink::Node(node, _)) => {
                        self.regions.get(self.node_region[node]).cloned()
                    }
                    _ => None,
                }
                .unwrap_or_else(|| fallback.clone());
                let (cell, pin) = match edge.sink {
                    Sink::Node(node, pin) => (self.nodes[node], pin),
                    Sink::Cell(cell, pin) => (cell, pin),
                };
                PlannedEdge {
                    root,
                    prefix,
                    regs: regs.clone(),
                    cell,
                    pin,
                }
            })
            .collect();
        Some(Plan {
            template,
            edges,
            removed: self.movable.iter().copied().collect(),
        })
    }
}

struct PlannedEdge {
    root: GateNetId,
    prefix: String,
    regs: Vec<bool>,
    cell: CellId,
    pin: usize,
}

struct Plan {
    template: Cell,
    edges: Vec<PlannedEdge>,
    removed: HashSet<CellId>,
}

impl Plan {
    /// Replace the movable registers with the retimed ones. Returns the
    /// number of registers placed.
    fn apply(self, netlist: &mut GateNetlist) -> usize {
        // Registers on one net share a chain while their values agree:
        // (net, value) -> the register output holding it
        let mut chains: HashMap<(GateNetId, bool), GateNetId> = HashMap::new();
        let mut new_cells: Vec<Cell> = Vec::new();
        let mut rewires: Vec<(CellId, usize, GateNetId)> = Vec::new();
        for edge in &self.edges {
            let mut net = edge.root;
            for &value in &edge.regs {
                net = match chains.get(&(net, value)) {
                    Some(&q) => q,
                    None => {
                        let path = format!("{}.retime{}", edge.prefix, new_cells.len());
                        let q = netlist.add_net_with_name(format!("{path}_q"));
                        let mut cell = self.template.clone();
                        cell.path = path;
                        cell.inputs = vec![net];
                        cell.outputs = vec![q];
                        cell.parameters.shift_remove("INIT");
                        if value {
                            cell.parameters.insert("INIT".to_string(), "1".to_string());
                        }
                        new_cells.push(cell);
                        chains.insert((net, value), q);
                        q
                    }
                };
            }
            rewires.push((edge.cell, edge.pin, net));
        }
        for (cell, pin, net) in rewires {
            netlist.cells[cell.0 as usize].inputs[pin] = net;
        }
        netlist.cells.retain(|c| !self.removed.contains(&c.id));
        let placed = new_cells.len();
        netlist.cells.extend(new_cells);
        netlist.rebuild_net_connectivity();
        netlist.rebuild_cache();
        placed
    }
}

/// The fewest registers to add at the inputs of `region` (as
/// `#[pipeline]` adds them) for forward retiming to bring the region's paths
/// within `target_period_ps`, trying up to `max_extra`. `None` when even
/// `max_extra` is not enough, or the region has no logic to retime.
///
/// Every edge into the region from a net driven outside it gets the extra
/// registers, which is what delaying each input by that many cycles does.
pub fn extra_stages_for_period(
    netlist: &GateNetlist,
    delays: &dyn RetimeDelays,
    region: &str,
    target_period_ps: i64,
    max_extra: u32,
) -> Option<u32> {
    let regions = vec![region.to_string()];
    let base = Graph::build(netlist, &regions)?;
    let mut inside = vec![false; netlist.nets.len()];
    for cell in &netlist.cells {
        if region_of(&cell.path, &regions).is_some() {
            for &net in &cell.outputs {
                inside[netlist.resolve_alias(net).0 as usize] = true;
            }
        }
    }
    let inputs: Vec<usize> = base
        .edges
        .iter()
        .enumerate()
        .filter(|(_, e)| match e.origin {
            Origin::Fixed(net) => !inside[netlist.resolve_alias(net).0 as usize],
            _ => false,
        })
        .map(|(i, _)| i)
        .collect();
    let mut graph = base;
    for extra in 0..=max_extra {
        if extra > 0 {
            for &i in &inputs {
                graph.edges[i].regs.push(false);
            }
        }
        if graph.feasible(target_period_ps, delays).is_some() {
            return Some(extra);
        }
    }
    None
}

/// Retime the netlist's own regions ([`GateNetlist::retime_regions`]) as
/// short as they go, logging what changed
pub fn retime_pipelined(
    netlist: &mut GateNetlist,
    delays: &dyn RetimeDelays,
) -> Option<RetimeReport> {
    let options = RetimeOptions {
        regions: netlist.retime_regions.clone(),
        target_period_ps: None,
    };
    let report = retime(netlist, delays, &options)?;
    tracing::info!(
        "retiming {:?}: longest register path {} -> {} ps, {} registers -> {}, through {} cells",
        options.regions,
        report.period_before_ps,
        report.period_after_ps,
        report.registers_before,
        report.registers_after,
        report.cells_crossed
    );
    Some(report)
}
