//! Register retiming on a mapped gate netlist.
//!
//! Registers move through logic to balance the delay between them, keeping
//! the netlist's behaviour cycle for cycle: every path from an input to an
//! output, and every loop, keeps its register count.
//!
//! - A forward move takes one register off every input of a cell and puts
//!   one on every output. The new register's initial value is exact: the
//!   cell's function of the initial values of the registers it replaces.
//! - A backward move takes one register off every output of a cell and puts
//!   one on every input. Its initial values are a preimage: inputs for which
//!   the cell gives the outputs' initial values. A move with no preimage (or
//!   whose output registers disagree) is not made, so the netlist still
//!   starts in the state it started in.
//!
//! The stages `#[pipeline(stages = N)]` adds sit at the inputs of an entity
//! ([`skalp_mir::pipelining`]). Forward moves carry them into the entity's
//! logic. Backward moves carry them out, into the logic feeding the entity,
//! when that logic is retimable too (`#[retime]`, or a `#[pipeline]` entity
//! around it): a path that starts outside the entity, such as an arbiter
//! and operand select in front of a shared unit, can then be cut. A
//! register never crosses into logic that is not in a region, and a loop
//! keeps its register count, so a request/grant handshake around the unit
//! keeps its timing.
//!
//! The search follows Leiserson and Saxe. Each node (a logic cell) has a
//! lag `r`, the number of backward moves made through it minus the forward
//! ones, and an edge from `u` to `v` holding `w` registers holds
//! `w + r(v) - r(u)` after retiming; cells that stay put have lag 0. Both
//! rules are difference constraints between lags: no edge ends with fewer
//! than zero registers, and a path longer than the period needs a register
//! somewhere along it. The path constraints are generated as timing finds
//! them (for each place a too-long path could start, its shortest
//! too-long stretch), solved by Bellman-Ford, and the timing redone until
//! nothing is too long. A negative cycle means no lags meet them all, and
//! its paths say why. Of the solutions, the one that moves registers least
//! is taken. A binary search over the period finds the shortest one
//! reachable.
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
//!
//! Timing at the edges of the regions counts the logic outside them: a path
//! entering a region starts at the arrival time of the net it enters on (the
//! longest path to it from a register through logic outside the regions),
//! and one leaving a region ends with the delay from there to the next
//! register.

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
    /// Delay of a carry chain's own connection, from one cell's carry out
    /// to the next one's carry in (dedicated wiring, not general routing)
    fn carry_link_ps(&self) -> i64 {
        0
    }
    /// Delays of one connection where the cells are placed, or `None` to
    /// charge [`Self::net_ps`] by its fanout (nothing placed yet)
    fn connection(&self, _connection: &Connection) -> Option<ConnectionDelays> {
        None
    }
}

/// A connection retiming times: from `driver` (`None`: an input of the
/// design) to input `pin` of `load`, possibly through registers
#[derive(Debug, Clone, Copy)]
pub struct Connection<'c> {
    pub driver: Option<&'c Cell>,
    pub load: &'c Cell,
    pub pin: usize,
    /// Loads on the driver's net
    pub fanout: usize,
    /// The registers on it now, nearest the driver first and nearest the
    /// load last (the same register when there is one)
    pub registers: Option<(&'c Cell, &'c Cell)>,
}

/// Routing delays of a [`Connection`], in picoseconds
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ConnectionDelays {
    /// Driver to load, with no register between
    pub direct_ps: i64,
    /// Driver to a register on the connection: the one there now, or for
    /// one retiming adds, a register beside the driver
    pub to_register_ps: i64,
    /// A register on the connection to the load (the one there now, or one
    /// beside the driver)
    pub from_register_ps: i64,
}

impl ConnectionDelays {
    /// The same delay whatever the registers
    pub fn uniform(ps: i64) -> Self {
        Self {
            direct_ps: ps,
            to_register_ps: ps,
            from_register_ps: ps,
        }
    }
}

/// Output of a carry cell that feeds the next cell of its chain
const CARRY_OUT_PIN: usize = 2;

/// Input of a carry cell fed by the previous cell of its chain
const CARRY_IN_PIN: usize = 8;

/// Whether output `pin` of `cell` is a carry chain's carry out
fn is_carry_out(cell: &Cell, pin: usize) -> bool {
    cell.function == Some(CellFunction::Ccu2) && pin == CARRY_OUT_PIN
}

/// Routing delays from the driver of (resolved) net `net` to input `pin`
/// of `load`, through `registers` when there are any: a carry chain's
/// dedicated link, the placed connection, or general routing by fanout
#[allow(clippy::too_many_arguments)]
fn connection_ps(
    netlist: &GateNetlist,
    driver: &[Option<(CellId, usize)>],
    fanout: &[usize],
    net: usize,
    load: &Cell,
    pin: usize,
    registers: Option<(&Cell, &Cell)>,
    delays: &dyn RetimeDelays,
) -> ConnectionDelays {
    let from = driver[net].map(|(id, pin)| (&netlist.cells[id.0 as usize], pin));
    if from.is_some_and(|(cell, pin)| is_carry_out(cell, pin)) {
        return ConnectionDelays::uniform(delays.carry_link_ps());
    }
    let connection = Connection {
        driver: from.map(|(cell, _)| cell),
        load,
        pin,
        fanout: fanout[net],
        registers,
    };
    delays
        .connection(&connection)
        .unwrap_or_else(|| ConnectionDelays::uniform(delays.net_ps(fanout[net])))
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
    /// For each cell of the netlist before, its id after (`None`: a
    /// register retiming replaced). Ids shift as registers are removed.
    pub cell_map: Vec<Option<CellId>>,
    /// Ids of the registers retiming added (the last cells of the netlist)
    pub added: std::ops::Range<usize>,
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
    /// Routing delays along it, with and without registers
    wire: ConnectionDelays,
    /// For an edge from something that stays put: when its value arrives
    /// (clock to output plus the logic outside the regions before it)
    entry_ps: i64,
    /// For an edge into a cell that stays put: the delay from there to the
    /// next register (its setup time, plus the logic outside the regions)
    exit_ps: i64,
}

#[derive(Clone)]
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
    /// Nodes a register may move backward through
    can_back: Vec<bool>,
    /// Arrival time of every (resolved) net outside the regions
    net_arrival: Vec<i64>,
    /// Driver of every (resolved) net
    driver: Vec<Option<(CellId, usize)>>,
    /// Loads on every (resolved) net
    net_fanout: Vec<usize>,
    /// Cells retiming owns (nodes and movable registers)
    owned: HashSet<CellId>,
}

/// Most free inputs of a cell for which a backward move through it tries
/// every input combination for initial values; a wider cell tries all
/// zeros and all ones
const MAX_BACKWARD_INPUTS: usize = 12;

/// Values to try on free inputs for a backward move, given the value each
/// would best take (`prefer`): fewest departures from those first, then
/// fewest ones. Every combination, or past [`MAX_BACKWARD_INPUTS`] the
/// preferred values, all zeros and all ones.
fn input_candidates(prefer: &[Option<bool>]) -> Vec<Vec<bool>> {
    let count = prefer.len();
    let misses = |values: &[bool]| {
        values
            .iter()
            .zip(prefer)
            .filter(|(v, p)| p.is_some_and(|p| p != **v))
            .count()
    };
    if count > MAX_BACKWARD_INPUTS {
        let preferred: Vec<bool> = prefer.iter().map(|p| p.unwrap_or(false)).collect();
        let mut candidates = vec![preferred, vec![false; count], vec![true; count]];
        candidates.dedup();
        return candidates;
    }
    let mut candidates: Vec<(usize, u32, Vec<bool>)> = (0..1u32 << count)
        .map(|mask| {
            let values: Vec<bool> = (0..count).map(|bit| mask >> bit & 1 == 1).collect();
            (misses(&values), mask, values)
        })
        .collect();
    candidates.sort_by_key(|(miss, mask, _)| (*miss, mask.count_ones(), *mask));
    candidates
        .into_iter()
        .map(|(_, _, values)| values)
        .collect()
}

/// Departure times (see `Graph::departures`), per node, per input pin
struct Departures {
    all: Vec<Vec<i64>>,
    to_reg: Vec<Vec<i64>>,
}

impl Departures {
    /// The times that apply to a path through `edge`: one from something
    /// that stays put counts only the ways on to a register that can move
    /// (a path with no such register keeps its delay whatever retiming
    /// does, so it is not retiming's to meet)
    fn for_start(&self, from_fixed: bool) -> &[Vec<i64>] {
        if from_fixed {
            &self.to_reg
        } else {
            &self.all
        }
    }
}

/// Why a path from outside the regions that ends at registers on its first
/// edge cannot be shortened
const ENDS_AT_FIXED_REGISTERS: &str =
    "it ends at registers that are already as far back as they can go";

/// How a path ending at registers in front of a cell names that cell
const REGISTERS_IN_FRONT: &str = "registers in front of";

/// How a path starting at a register names the cell feeding it
const REGISTER_AFTER: &str = "a register after";

/// Departure time of a path that reaches no movable register
const UNREACHED: i64 = -(1 << 40);

/// What `Graph::feasible` found: lags meeting the period on every path
/// retiming can change, and the paths too long whatever the lags
struct Feasible {
    lags: Vec<i64>,
    frozen: Vec<BlockedPath>,
}

/// Arrival times (see `Graph::arrivals`), per node, per output pin
type Arrivals = Vec<Vec<(i64, i64)>>;

/// Why no retiming reaches a period: a path retiming cannot shorten
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BlockedPath {
    /// Its delay in picoseconds
    pub delay_ps: i64,
    /// Whether it starts at something that stays put (outside the regions,
    /// or a register that cannot move) rather than at a movable register
    pub from_outside: bool,
    /// Where it starts, then the cells along it
    pub steps: Vec<String>,
    /// Why retiming stopped there
    pub why: String,
}

/// Where a traced path starts
#[derive(Debug, Clone, Copy)]
enum TraceStart {
    /// At a register on the edge into its first node
    Register,
    /// At something that stays put, through this edge
    Fixed(usize),
    /// At a constant, or where its start could not be traced
    Open,
}

/// A path found too long, kept to explain a failure
#[derive(Debug, Clone)]
struct TooLong {
    delay_ps: i64,
    /// The nodes along it, in order
    nodes: Vec<usize>,
    start: TraceStart,
    /// The edge it leaves its last node on
    end_edge: usize,
    /// Whether it ends in a cell that stays put rather than at a register
    into_fixed: bool,
    /// The lags it was found with
    lags: std::rc::Rc<Vec<i64>>,
}

/// A path traced back from where it is too long
struct Traced {
    /// Its nodes, in order
    nodes: Vec<usize>,
    /// Arrival time at each node's output along it
    times: Vec<i64>,
    /// The edge into the first node, unless it starts at a constant
    first_in: Option<usize>,
    /// Time at the first node's input (0 with no edge into it)
    first_at: i64,
    /// The edges between consecutive nodes
    links: Vec<usize>,
    start: TraceStart,
}

/// Solve the difference constraints `bounds` (`(a, b, c)`: `x[a] - x[b] <=
/// c`) over `count` vertices with `x[zero] = 0`: the largest solution when
/// `largest`, else the smallest. A vertex no constraint bounds that way
/// gets `i64::MAX / 4` (or its negation). Fails with the constraints on a
/// cycle that no solution satisfies.
fn solve_bounds(
    count: usize,
    zero: usize,
    bounds: &[(usize, usize, i64)],
    largest: bool,
) -> Result<Vec<i64>, Vec<usize>> {
    const FAR: i64 = i64::MAX / 4;
    let mut adjacent: Vec<Vec<(usize, i64, usize)>> = vec![Vec::new(); count];
    for (index, &(a, b, c)) in bounds.iter().enumerate() {
        if largest {
            // x[a] <= x[b] + c
            adjacent[b].push((a, c, index));
        } else {
            // with y = -x: y[b] <= y[a] + c
            adjacent[a].push((b, c, index));
        }
    }
    let mut start = vec![FAR; count];
    start[zero] = 0;
    let found = shortest_paths(&adjacent, start, zero)?;
    Ok(if largest {
        found
    } else {
        found.into_iter().map(|y| -y).collect()
    })
}

/// The solution of `bounds` (as for [`solve_bounds`]) that keeps each value
/// as near 0 as it can: the largest one no larger than the smallest
/// solution or 0, whichever is larger
fn solve_lags(
    count: usize,
    zero: usize,
    bounds: &[(usize, usize, i64)],
) -> Result<Vec<i64>, Vec<usize>> {
    const FAR: i64 = i64::MAX / 4;
    let lowest = solve_bounds(count, zero, bounds, false)?;
    let mut adjacent: Vec<Vec<(usize, i64, usize)>> = vec![Vec::new(); count];
    for (index, &(a, b, c)) in bounds.iter().enumerate() {
        adjacent[b].push((a, c, index));
    }
    let start: Vec<i64> = lowest
        .iter()
        .map(|&low| if low <= -FAR { 0 } else { low.max(0) })
        .collect();
    shortest_paths(&adjacent, start, zero)
}

/// Bellman-Ford (queue-based) from the values `start`, over edges `(to,
/// weight, id)`, keeping `pinned` where it starts. Values of `i64::MAX / 4`
/// count as unreached. Fails with the ids of the edges on a negative cycle.
fn shortest_paths(
    adjacent: &[Vec<(usize, i64, usize)>],
    start: Vec<i64>,
    pinned: usize,
) -> Result<Vec<i64>, Vec<usize>> {
    const FAR: i64 = i64::MAX / 4;
    let count = adjacent.len();
    let mut value = start;
    let mut parent: Vec<Option<(usize, usize)>> = vec![None; count];
    let mut queued = vec![false; count];
    let mut queue: std::collections::VecDeque<usize> = std::collections::VecDeque::new();
    for v in 0..count {
        if value[v] < FAR {
            queued[v] = true;
            queue.push_back(v);
        }
    }
    let mut relaxed = 0usize;
    while let Some(v) = queue.pop_front() {
        queued[v] = false;
        let here = value[v];
        for &(to, weight, id) in &adjacent[v] {
            let there = here + weight;
            if there >= value[to] {
                continue;
            }
            value[to] = there;
            parent[to] = Some((v, id));
            if to == pinned {
                // Lowering the fixed vertex is a negative cycle through it
                return Err(cycle_through(&parent, to).unwrap_or_default());
            }
            relaxed += 1;
            if relaxed.is_multiple_of(count.max(1)) {
                if let Some(cycle) = parent_cycle(&parent) {
                    return Err(cycle);
                }
            }
            if !queued[to] {
                queued[to] = true;
                queue.push_back(to);
            }
        }
    }
    Ok(value)
}

/// The edge ids on the parent cycle through `from`, if its parents lead
/// back to a vertex already passed
fn cycle_through(parent: &[Option<(usize, usize)>], from: usize) -> Option<Vec<usize>> {
    let mut seen: HashMap<usize, usize> = HashMap::new();
    let mut path: Vec<(usize, usize)> = Vec::new();
    let mut v = from;
    loop {
        if let Some(&at) = seen.get(&v) {
            return Some(path[at..].iter().map(|&(_, id)| id).collect());
        }
        seen.insert(v, path.len());
        let (p, id) = parent[v]?;
        path.push((v, id));
        v = p;
    }
}

/// The edge ids on a cycle of the parent pointers, if there is one (with
/// Bellman-Ford, such a cycle has negative weight)
fn parent_cycle(parent: &[Option<(usize, usize)>]) -> Option<Vec<usize>> {
    let count = parent.len();
    let mut walk = vec![usize::MAX; count];
    for begin in 0..count {
        if walk[begin] != usize::MAX {
            continue;
        }
        let mut v = begin;
        loop {
            if walk[v] == begin {
                return cycle_through(parent, v);
            }
            if walk[v] != usize::MAX {
                break;
            }
            walk[v] = begin;
            match parent[v] {
                Some((p, _)) => v = p,
                None => break,
            }
        }
    }
    None
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
    let mut graph = Graph::build(netlist, &options.regions, delays, None)?;
    let zero = vec![0i64; graph.nodes.len()];
    let before = graph.period(&zero, delays);
    // A backward move with no initial values to give is ruled out and the
    // search runs again
    for _ in 0..32 {
        let lags = graph.search(options.target_period_ps, before, delays)?;
        if lags.iter().all(|&r| r == 0) {
            return None;
        }
        // Paths retiming can't change still count in the period, so it may
        // not drop even when the rest got faster
        let after = graph.period(&lags, delays);
        let regs = match graph.realize(&lags) {
            Ok(regs) => regs,
            Err(node) if node < graph.nodes.len() && graph.can_back[node] => {
                tracing::debug!(
                    "retime: no initial values for a backward move through {}",
                    graph.cell(node).path
                );
                graph.can_back[node] = false;
                continue;
            }
            Err(_) => return None,
        };
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
        let registers_before = graph.movable.len();
        let cells_crossed = lags.iter().filter(|&&r| r != 0).count();
        let plan = graph.plan(&regs)?;
        let (cell_map, added) = plan.apply(netlist);
        return Some(RetimeReport {
            period_before_ps: before,
            period_after_ps: after,
            registers_before,
            registers_after: added.len(),
            cells_crossed,
            cell_map,
            added,
        });
    }
    None
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

/// Whether the cell path `path` is inside the instance path `region`
fn is_inside(path: &str, region: &str) -> bool {
    path.len() > region.len() && path.starts_with(region) && path.as_bytes()[region.len()] == b'.'
}

/// The innermost region holding the cell path `path`
fn region_of(path: &str, regions: &[String]) -> Option<usize> {
    regions
        .iter()
        .enumerate()
        .filter(|(_, r)| is_inside(path, r))
        .max_by_key(|(_, r)| r.len())
        .map(|(i, _)| i)
}

/// The clock whose registers retiming moves in `regions`: the one most of
/// their registers use
pub fn region_clock(netlist: &GateNetlist, regions: &[String]) -> Option<GateNetId> {
    let mut clocks: HashMap<GateNetId, usize> = HashMap::new();
    for cell in &netlist.cells {
        if cell.function == Some(CellFunction::Dff) && region_of(&cell.path, regions).is_some() {
            if let Some(clock) = cell.clock {
                *clocks.entry(netlist.resolve_alias(clock)).or_default() += 1;
            }
        }
    }
    clocks
        .into_iter()
        .max_by_key(|&(net, n)| (n, net.0))
        .map(|(net, _)| net)
}

/// Timing of the logic outside the retiming regions, which stays put
struct Outside<'n> {
    netlist: &'n GateNetlist,
    /// Driver of each (resolved) net
    driver: &'n [Option<(CellId, usize)>],
    /// Loads of each (resolved) net
    loads: &'n [Vec<(CellId, usize)>],
    fanout: &'n [usize],
    /// Whether a cell is retiming's (a node or a movable register)
    inside: &'n dyn Fn(CellId) -> bool,
    delays: &'n dyn RetimeDelays,
}

/// How a net outside the regions gets its value
enum Source {
    /// A path starts here at this time (a register, a pad, a constant, or a
    /// cell retiming owns)
    Start(i64),
    /// Output `pin` of a combinational cell outside the regions
    Logic(CellId, usize),
}

impl Outside<'_> {
    fn resolve(&self, net: GateNetId) -> usize {
        self.netlist.resolve_alias(net).0 as usize
    }

    /// Routing delay from the driver of `net` to input `pin` of `load`
    fn wire_ps(&self, net: usize, load: &Cell, pin: usize) -> i64 {
        connection_ps(
            self.netlist,
            self.driver,
            self.fanout,
            net,
            load,
            pin,
            None,
            self.delays,
        )
        .direct_ps
    }

    fn source(&self, net: usize) -> Source {
        let Some((id, pin)) = self.driver[net] else {
            return Source::Start(self.delays.clk_to_q_ps());
        };
        let cell = &self.netlist.cells[id.0 as usize];
        if constant_of(cell).is_some() {
            Source::Start(0)
        } else if (self.inside)(id) || cell.clock.is_some() {
            Source::Start(self.delays.clk_to_q_ps())
        } else {
            Source::Logic(id, pin)
        }
    }

    /// Arrival time of every net: the longest path to it from a register
    /// through logic outside the regions (a loop counts as a start)
    fn arrivals(&self) -> Vec<i64> {
        let count = self.netlist.nets.len();
        let mut arrival = vec![0i64; count];
        // 0 not seen, 1 on the stack, 2 done
        let mut state = vec![0u8; count];
        for start in 0..count {
            if state[start] == 2 {
                continue;
            }
            let mut stack = vec![(start, false)];
            while let Some((net, expanded)) = stack.pop() {
                if state[net] == 2 {
                    continue;
                }
                let (id, out) = match self.source(net) {
                    Source::Start(t) => {
                        arrival[net] = t;
                        state[net] = 2;
                        continue;
                    }
                    Source::Logic(id, out) => (id, out),
                };
                let cell = &self.netlist.cells[id.0 as usize];
                if !expanded {
                    if state[net] == 1 {
                        continue;
                    }
                    state[net] = 1;
                    stack.push((net, true));
                    for &input in &cell.inputs {
                        let input = self.resolve(input);
                        if state[input] == 0 {
                            stack.push((input, false));
                        }
                    }
                    continue;
                }
                arrival[net] = (0..cell.inputs.len())
                    .filter_map(|pin| {
                        let arc = self.delays.arc_ps(cell, pin, out)?;
                        let input = self.resolve(cell.inputs[pin]);
                        let at = if state[input] == 2 {
                            arrival[input]
                        } else {
                            self.delays.clk_to_q_ps()
                        };
                        Some(at + self.wire_ps(input, cell, pin) + arc)
                    })
                    .max()
                    .unwrap_or(self.delays.clk_to_q_ps());
                state[net] = 2;
            }
        }
        arrival
    }

    /// Delay from input `pin` of a cell that stays put to the next register,
    /// given each net's tail
    fn exit_from(&self, cell: &Cell, pin: usize, tails: &[i64]) -> i64 {
        if cell.clock.is_some() {
            return self.delays.setup_ps();
        }
        (0..cell.outputs.len())
            .filter_map(|out| {
                let arc = self.delays.arc_ps(cell, pin, out)?;
                let net = self.resolve(cell.outputs[out]);
                Some(arc + tails[net])
            })
            .max()
            .unwrap_or(0)
    }

    /// Tail of every net: the longest delay from its driver to a register
    /// through logic outside the regions, counting the routing to each
    /// load (a load retiming owns ends the path)
    fn tails(&self) -> Vec<i64> {
        let count = self.netlist.nets.len();
        let mut tail = vec![0i64; count];
        let mut state = vec![0u8; count];
        let is_logic = |id: CellId| {
            let cell = &self.netlist.cells[id.0 as usize];
            !(self.inside)(id) && cell.clock.is_none()
        };
        for start in 0..count {
            if state[start] == 2 {
                continue;
            }
            let mut stack = vec![(start, false)];
            while let Some((net, expanded)) = stack.pop() {
                if state[net] == 2 {
                    continue;
                }
                if !expanded {
                    if state[net] == 1 {
                        continue;
                    }
                    state[net] = 1;
                    stack.push((net, true));
                    for &(id, _) in &self.loads[net] {
                        if !is_logic(id) {
                            continue;
                        }
                        for &output in &self.netlist.cells[id.0 as usize].outputs {
                            let output = self.resolve(output);
                            if state[output] == 0 {
                                stack.push((output, false));
                            }
                        }
                    }
                    continue;
                }
                tail[net] = self.loads[net]
                    .iter()
                    .map(|&(id, pin)| {
                        let cell = &self.netlist.cells[id.0 as usize];
                        let wire = self.wire_ps(net, cell, pin);
                        wire + if (self.inside)(id) {
                            0
                        } else if cell.clock.is_some() {
                            self.delays.setup_ps()
                        } else {
                            (0..cell.outputs.len())
                                .filter_map(|out| {
                                    let arc = self.delays.arc_ps(cell, pin, out)?;
                                    let next = self.resolve(cell.outputs[out]);
                                    let after = if state[next] == 2 { tail[next] } else { 0 };
                                    Some(arc + after)
                                })
                                .max()
                                .unwrap_or(0)
                        }
                    })
                    .max()
                    .unwrap_or(0);
                state[net] = 2;
            }
        }
        tail
    }
}

impl<'a> Graph<'a> {
    /// The retiming graph of `regions`, limited to the cells in `keep` when
    /// given (the others stay put)
    fn build(
        netlist: &'a GateNetlist,
        regions: &[String],
        delays: &dyn RetimeDelays,
        keep: Option<&HashSet<CellId>>,
    ) -> Option<Self> {
        let kept = |id: CellId| keep.is_none_or(|k| k.contains(&id));
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

        let clock = region_clock(netlist, regions)?;

        let is_movable_register = |cell: &Cell| {
            cell.function == Some(CellFunction::Dff)
                && cell.clock.map(resolve) == Some(clock)
                && cell.reset.is_none()
                && cell.inputs.len() == 1
                && cell.outputs.len() == 1
                && region_of(&cell.path, regions).is_some()
                && kept(cell.id)
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
                && kept(cell.id)
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
            let carry_in = resolve(cell.inputs[CARRY_IN_PIN]);
            if let Some((from, CARRY_OUT_PIN)) = driver[carry_in.0 as usize] {
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
                // The registers' cells, load end first
                let mut reg_cells: Vec<CellId> = Vec::new();
                let mut steps = 0;
                let origin = loop {
                    match driver[net.0 as usize] {
                        Some((d, _)) if is_movable.contains(&d) && steps < 100_000 => {
                            let reg = &netlist.cells[d.0 as usize];
                            regs.push(register_init(reg));
                            reg_cells.push(d);
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
                let registers = match (reg_cells.last(), reg_cells.first()) {
                    (Some(&first), Some(&last)) if !regs.is_empty() => Some((
                        &netlist.cells[first.0 as usize],
                        &netlist.cells[last.0 as usize],
                    )),
                    _ => None,
                };
                let wire = connection_ps(
                    netlist,
                    &driver,
                    &fanout,
                    net.0 as usize,
                    cell,
                    pin,
                    registers,
                    delays,
                );
                let index = edges.len();
                edges.push(Edge {
                    origin,
                    sink,
                    regs,
                    wire,
                    entry_ps: 0,
                    exit_ps: 0,
                });
                if let Some(node) = sink_node {
                    in_edges[node][pin] = Some(index);
                }
                if let Origin::Node(node, out) = origin {
                    out_edges[node][out].push(index);
                }
            }
        }
        // Timing at the edges of the regions
        let mut loads: Vec<Vec<(CellId, usize)>> = vec![Vec::new(); net_count];
        for cell in &netlist.cells {
            for (pin, &net) in cell.inputs.iter().enumerate() {
                loads[resolve(net).0 as usize].push((cell.id, pin));
            }
        }
        let outside = Outside {
            netlist,
            driver: &driver,
            loads: &loads,
            fanout: &fanout,
            inside: &|id: CellId| node_of.contains_key(&id) || is_movable.contains(&id),
            delays,
        };
        let arrivals = outside.arrivals();
        let tails = outside.tails();
        let owned: HashSet<CellId> = node_of.keys().chain(is_movable.iter()).copied().collect();
        for edge in &mut edges {
            if let Origin::Fixed(net) = edge.origin {
                edge.entry_ps = arrivals[net.0 as usize];
            }
            if let Sink::Cell(cell, pin) = edge.sink {
                edge.exit_ps = outside.exit_from(&netlist.cells[cell.0 as usize], pin, &tails);
            }
        }

        // Until a backward move through a node is found to have no
        // initial values
        let can_back = vec![true; nodes.len()];
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
            can_back,
            net_arrival: arrivals,
            driver,
            net_fanout: fanout,
            owned,
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

    /// Routing delay of an edge with no register on it
    fn wire_direct(&self, edge: &Edge) -> i64 {
        edge.wire.direct_ps
    }

    /// Routing delay from an edge's origin to a register on it
    fn wire_to_register(&self, edge: &Edge) -> i64 {
        edge.wire.to_register_ps
    }

    /// Routing delay from a register on an edge to its sink
    fn wire_after_register(&self, edge: &Edge) -> i64 {
        edge.wire.from_register_ps
    }

    /// Longest delay from each node input to the next register, with
    /// `lags` applied: to any register (`all`), and to a register retiming
    /// may move (`to_reg`, [`UNREACHED`] when there is none)
    fn departures(&self, lags: &[i64], delays: &dyn RetimeDelays) -> Departures {
        let mut memo: Vec<Vec<Option<(i64, i64)>>> = self
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
                    let outs: Vec<(i64, i64)> = (0..cell.outputs.len())
                        .map(|out| {
                            self.out_edges[node][out]
                                .iter()
                                .map(|&e| {
                                    let edge = &self.edges[e];
                                    if self.weight(edge, lags) > 0 {
                                        let t = self.wire_to_register(edge) + delays.setup_ps();
                                        return (t, t);
                                    }
                                    let net = self.wire_direct(edge);
                                    match edge.sink {
                                        Sink::Node(x, pin) => {
                                            let (all, to_reg) =
                                                memo[x][pin].unwrap_or((0, UNREACHED));
                                            (net + all, (net + to_reg).max(UNREACHED))
                                        }
                                        Sink::Cell(..) => (net + edge.exit_ps, UNREACHED),
                                    }
                                })
                                .fold((0, UNREACHED), |a, b| (a.0.max(b.0), a.1.max(b.1)))
                        })
                        .collect();
                    for (pin, slot) in memo[node].iter_mut().enumerate() {
                        let worst = outs
                            .iter()
                            .enumerate()
                            .filter_map(|(out, &(all, to_reg))| {
                                delays
                                    .arc_ps(cell, pin, out)
                                    .map(|arc| (arc + all, (arc + to_reg).max(UNREACHED)))
                            })
                            .fold((0, UNREACHED), |a, b| (a.0.max(b.0), a.1.max(b.1)));
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
        let mut all = Vec::with_capacity(memo.len());
        let mut to_reg = Vec::with_capacity(memo.len());
        for pins in memo {
            let (a, r): (Vec<i64>, Vec<i64>) = pins
                .into_iter()
                .map(|d| d.unwrap_or((0, UNREACHED)))
                .unzip();
            all.push(a);
            to_reg.push(r);
        }
        Departures { all, to_reg }
    }

    /// Delay of the path from something that stays put (a register or a
    /// pad outside the regions, or a cell that cannot move) through `edge`
    /// when no register sits on it. Retiming cannot shorten such a path,
    /// only keep registers from leaving it.
    fn fixed_path(
        &self,
        edge: &Edge,
        lags: &[i64],
        departures: &Departures,
        delays: &dyn RetimeDelays,
    ) -> Option<i64> {
        let Origin::Fixed(_) = edge.origin else {
            return None;
        };
        if self.weight(edge, lags) > 0 {
            // Registers on the edge itself: the path ends there, and they
            // cannot move back past where it starts
            return Some(edge.entry_ps + self.wire_to_register(edge) + delays.setup_ps());
        }
        let Sink::Node(x, pin) = edge.sink else {
            return None;
        };
        let tail = departures.to_reg[x][pin];
        (self.weight(edge, lags) == 0 && tail > UNREACHED / 2)
            .then(|| edge.entry_ps + self.wire_direct(edge) + tail)
    }

    /// Delay of the path starting at the register(s) on `edge`, if any
    fn register_path(
        &self,
        edge: &Edge,
        lags: &[i64],
        departures: &Departures,
        delays: &dyn RetimeDelays,
    ) -> Option<i64> {
        if matches!(edge.origin, Origin::Const(..)) || self.weight(edge, lags) < 1 {
            return None;
        }
        Some(
            delays.clk_to_q_ps()
                + self.wire_after_register(edge)
                + match edge.sink {
                    Sink::Node(x, pin) => departures.all[x][pin],
                    Sink::Cell(..) => edge.exit_ps,
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
            .enumerate()
            .filter_map(|(i, e)| {
                let t = self
                    .register_path(e, lags, &departures, delays)
                    .or_else(|| self.fixed_path(e, lags, &departures, delays))?;
                Some((t, i))
            })
            .max_by_key(|(t, _)| *t);
        match worst {
            Some((t, edge)) => {
                let mut steps = vec![format!("{t} ps")];
                steps.extend(self.path_steps(edge, lags, &departures, delays));
                steps
            }
            None => Vec::new(),
        }
    }

    /// Where the path through `edge` starts, then the cells along it,
    /// following the slowest way on
    fn path_steps(
        &self,
        edge: usize,
        lags: &[i64],
        departures: &Departures,
        delays: &dyn RetimeDelays,
    ) -> Vec<String> {
        let first = &self.edges[edge];
        let mut steps = Vec::new();
        steps.push(match first.origin {
            Origin::Fixed(net) if self.weight(first, lags) == 0 => {
                let name = &self.netlist.nets[net.0 as usize].name;
                let from = self.netlist.nets[net.0 as usize]
                    .driver
                    .map(|d| self.netlist.cells[d.0 as usize].path.clone());
                match from {
                    Some(path) => {
                        format!("net {name} from {path}, arriving at {} ps", first.entry_ps)
                    }
                    None => format!("net {name}, arriving at {} ps", first.entry_ps),
                }
            }
            _ => "a register".to_string(),
        });
        let (nodes, last) = self.walk(edge, lags, departures, delays);
        for node in nodes {
            let cell = self.cell(node);
            steps.push(format!(
                "{} {} (lag {})",
                cell.cell_type, cell.path, lags[node]
            ));
        }
        let last = &self.edges[last];
        if let Sink::Cell(cell, _) = last.sink {
            if self.weight(last, lags) == 0 {
                let cell = &self.netlist.cells[cell.0 as usize];
                steps.push(format!(
                    "{} {} (stays put; {} ps more to a register)",
                    cell.cell_type, cell.path, last.exit_ps
                ));
            }
        }
        steps
    }

    /// The nodes along the path through `edge` that sets its delay, and the
    /// last edge on it
    fn walk(
        &self,
        edge: usize,
        lags: &[i64],
        departures: &Departures,
        delays: &dyn RetimeDelays,
    ) -> (Vec<usize>, usize) {
        let start = &self.edges[edge];
        let from_fixed = matches!(start.origin, Origin::Fixed(_)) && self.weight(start, lags) == 0;
        let departures = departures.for_start(from_fixed);
        let mut nodes = Vec::new();
        let mut edge = edge;
        while let Sink::Node(x, pin) = self.edges[edge].sink {
            if nodes.len() > 100_000 {
                break;
            }
            nodes.push(x);
            let cell = self.cell(x);
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
                        Sink::Cell(..) if from_fixed => UNREACHED,
                        Sink::Cell(..) => candidate.exit_ps,
                    };
                    if arc + tail > best {
                        best = arc + tail;
                        next = Some(e);
                    }
                }
            }
            match next {
                Some(e) => edge = e,
                None => break,
            }
        }
        (nodes, edge)
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

    /// Lags for `target` if it is reachable, else for the shortest period
    /// reachable (to within 1%) below `before`
    fn search(
        &self,
        target: Option<i64>,
        before: i64,
        delays: &dyn RetimeDelays,
    ) -> Option<Vec<i64>> {
        if let Some(target) = target {
            if let Ok(found) = self.feasible(target, delays) {
                return Some(found.lags);
            }
        }
        let mut best = None;
        let (mut lo, mut hi) = (self.lower_bound(delays), before);
        while hi - lo > (hi / 100).max(10) {
            let mid = (lo + hi) / 2;
            match self.feasible(mid, delays) {
                Ok(found) => {
                    hi = mid;
                    best = Some(found.lags);
                }
                Err(_) => lo = mid,
            }
        }
        best
    }

    /// Arrival time at each node output over paths without registers, from
    /// any start (`.0`: a register, or something that stays put) and from a
    /// movable register only (`.1`); [`UNREACHED`] when no such path
    /// reaches it
    fn arrivals(&self, lags: &[i64], delays: &dyn RetimeDelays, skip: &[bool]) -> Arrivals {
        let count = self.nodes.len();
        let mut memo: Vec<Option<Vec<(i64, i64)>>> = vec![None; count];
        let mut on_stack = vec![false; count];
        for start in 0..count {
            if memo[start].is_some() {
                continue;
            }
            let mut stack = vec![(start, false)];
            while let Some((node, expanded)) = stack.pop() {
                if memo[node].is_some() {
                    continue;
                }
                if expanded {
                    on_stack[node] = false;
                    let cell = self.cell(node);
                    let at_pin: Vec<(i64, i64)> = self.in_edges[node]
                        .iter()
                        .map(|e| {
                            let Some(e) = *e else {
                                return (UNREACHED, UNREACHED);
                            };
                            let edge = &self.edges[e];
                            if matches!(edge.origin, Origin::Const(..)) {
                                return (UNREACHED, UNREACHED);
                            }
                            if self.weight(edge, lags) > 0 {
                                let t = delays.clk_to_q_ps() + self.wire_after_register(edge);
                                return (t, t);
                            }
                            let net = self.wire_direct(edge);
                            match edge.origin {
                                // A path too long from where it starts is
                                // reported once, not chased
                                Origin::Fixed(_) if skip.get(e) == Some(&true) => {
                                    (delays.clk_to_q_ps() + net, UNREACHED)
                                }
                                Origin::Fixed(_) => (edge.entry_ps + net, UNREACHED),
                                Origin::Node(u, out) => {
                                    let (all, reg) =
                                        memo[u].as_ref().map_or((UNREACHED, UNREACHED), |a| a[out]);
                                    ((all + net).max(UNREACHED), (reg + net).max(UNREACHED))
                                }
                                Origin::Const(..) => (UNREACHED, UNREACHED),
                            }
                        })
                        .collect();
                    let outs = (0..cell.outputs.len())
                        .map(|out| {
                            at_pin
                                .iter()
                                .enumerate()
                                .filter_map(|(pin, &(all, reg))| {
                                    let arc = delays.arc_ps(cell, pin, out)?;
                                    Some(((all + arc).max(UNREACHED), (reg + arc).max(UNREACHED)))
                                })
                                .fold((UNREACHED, UNREACHED), |a, b| (a.0.max(b.0), a.1.max(b.1)))
                        })
                        .collect();
                    memo[node] = Some(outs);
                    continue;
                }
                if on_stack[node] {
                    // A loop without registers: combinational, not timed
                    continue;
                }
                on_stack[node] = true;
                stack.push((node, true));
                for &e in self.in_edges[node].iter().flatten() {
                    let edge = &self.edges[e];
                    if let Origin::Node(u, _) = edge.origin {
                        if memo[u].is_none() && !on_stack[u] && self.weight(edge, lags) == 0 {
                            stack.push((u, false));
                        }
                    }
                }
            }
        }
        memo.into_iter().map(Option::unwrap_or_default).collect()
    }

    /// The logic outside the regions on the slowest path to `net`, from
    /// where it starts
    fn outside_trace(&self, net: GateNetId, delays: &dyn RetimeDelays) -> Vec<String> {
        let mut steps = Vec::new();
        let mut net = self.netlist.resolve_alias(net).0 as usize;
        while let Some((id, out)) = self.driver[net] {
            let cell = &self.netlist.cells[id.0 as usize];
            steps.push(format!("{} {}", cell.cell_type, cell.path));
            if self.owned.contains(&id)
                || cell.clock.is_some()
                || constant_of(cell).is_some()
                || steps.len() > 400
            {
                break;
            }
            let target = self.net_arrival[net];
            let next = (0..cell.inputs.len()).find_map(|pin| {
                let arc = delays.arc_ps(cell, pin, out)?;
                let input = self.netlist.resolve_alias(cell.inputs[pin]).0 as usize;
                (self.net_arrival[input]
                    + connection_ps(
                        self.netlist,
                        &self.driver,
                        &self.net_fanout,
                        input,
                        cell,
                        pin,
                        None,
                        delays,
                    )
                    .direct_ps
                    + arc
                    == target)
                    .then_some(input)
            });
            match next {
                Some(input) => net = input,
                None => break,
            }
        }
        steps.reverse();
        steps
    }

    #[allow(clippy::too_many_arguments)]
    fn blocked(
        &self,
        edge: usize,
        delay_ps: i64,
        lags: &[i64],
        departures: &Departures,
        delays: &dyn RetimeDelays,
        why: &str,
    ) -> BlockedPath {
        let e = &self.edges[edge];
        let steps = match e.origin {
            Origin::Fixed(net) if self.weight(e, lags) > 0 || why == ENDS_AT_FIXED_REGISTERS => {
                let mut steps = self.outside_trace(net, delays);
                let sink = match e.sink {
                    Sink::Node(x, _) => self.cell(x),
                    Sink::Cell(c, _) => &self.netlist.cells[c.0 as usize],
                };
                steps.push(format!(
                    "{REGISTERS_IN_FRONT} {} {}",
                    sink.cell_type, sink.path
                ));
                steps
            }
            _ => self.path_steps(edge, lags, departures, delays),
        };
        BlockedPath {
            delay_ps,
            from_outside: true,
            steps,
            why: why.to_string(),
        }
    }

    /// The vertex that stands for everything that stays put (lag 0) in the
    /// difference constraints
    fn zero(&self) -> usize {
        self.nodes.len()
    }

    /// The difference constraints every retiming keeps, as `(a, b, c)` for
    /// `lag[a] - lag[b] <= c`: no edge ends with fewer than zero registers,
    /// a carry chain moves as one, and a node no backward move can be made
    /// through (no initial values) never moves back
    fn legal_bounds(&self) -> Vec<(usize, usize, i64)> {
        let zero = self.zero();
        let mut bounds = Vec::new();
        for edge in &self.edges {
            let from = match edge.origin {
                Origin::Node(node, _) => node,
                Origin::Fixed(_) => zero,
                Origin::Const(..) => continue,
            };
            let to = match edge.sink {
                Sink::Node(node, _) => node,
                Sink::Cell(..) => zero,
            };
            if from != to {
                bounds.push((from, to, edge.regs.len() as i64));
            }
        }
        for block in &self.blocks {
            for pair in block.windows(2) {
                bounds.push((pair[0], pair[1], 0));
                bounds.push((pair[1], pair[0], 0));
            }
        }
        for (node, &can_back) in self.can_back.iter().enumerate() {
            if !can_back {
                bounds.push((node, zero, 0));
            }
        }
        bounds
    }

    /// The slowest path without registers into output `out` of `node`,
    /// traced back to where it starts
    fn trace_path(
        &self,
        node: usize,
        out: usize,
        lags: &[i64],
        arrivals: &Arrivals,
        delays: &dyn RetimeDelays,
        skip: &[bool],
    ) -> Traced {
        // Built from the end: nodes, their output times, and the edge into
        // each
        let mut nodes = vec![node];
        let mut times = vec![arrivals[node][out].0];
        let mut into: Vec<usize> = Vec::new();
        let (mut node, mut out) = (node, out);
        let mut first_at = 0;
        let start = loop {
            if nodes.len() > 100_000 {
                break TraceStart::Open;
            }
            let cell = self.cell(node);
            let target = arrivals[node][out].0;
            let mut next = None;
            for (pin, e) in self.in_edges[node].iter().enumerate() {
                let Some(e) = *e else {
                    continue;
                };
                let edge = &self.edges[e];
                let Some(arc) = delays.arc_ps(cell, pin, out) else {
                    continue;
                };
                let net = self.wire_direct(edge);
                let begin = if self.weight(edge, lags) > 0 {
                    delays.clk_to_q_ps() + self.wire_after_register(edge)
                } else if matches!(edge.origin, Origin::Fixed(_)) && skip[e] {
                    delays.clk_to_q_ps() + net
                } else {
                    match edge.origin {
                        Origin::Fixed(_) => edge.entry_ps + net,
                        Origin::Node(u, o) => arrivals[u][o].0 + net,
                        Origin::Const(..) => continue,
                    }
                };
                if begin + arc == target {
                    next = Some((e, begin));
                    break;
                }
            }
            let Some((e, begin)) = next else {
                break TraceStart::Open;
            };
            into.push(e);
            first_at = begin;
            let edge = &self.edges[e];
            if self.weight(edge, lags) > 0 || (matches!(edge.origin, Origin::Fixed(_)) && skip[e]) {
                break TraceStart::Register;
            }
            match edge.origin {
                Origin::Node(u, o) => {
                    node = u;
                    out = o;
                    nodes.push(u);
                    times.push(arrivals[u][o].0);
                }
                Origin::Fixed(_) => break TraceStart::Fixed(e),
                Origin::Const(..) => break TraceStart::Open,
            }
        };
        nodes.reverse();
        times.reverse();
        into.reverse();
        // `into` has an edge into the first node unless the start is open
        let first_in = match start {
            TraceStart::Open => None,
            _ => Some(into[0]),
        };
        let links = if first_in.is_some() {
            &into[1..]
        } else {
            &into[..]
        };
        Traced {
            nodes,
            times,
            first_in,
            first_at: if first_in.is_some() { first_at } else { 0 },
            links: links.to_vec(),
            start,
        }
    }

    /// Constraints for a too-long path ending through `end` (into a cell
    /// that stays put when `into_fixed`, else at a register on it): for
    /// each place it could start a register from, the shortest stretch
    /// from there that is too long needs a register inside it. As
    /// `(first, last, bound)`, read `lag[first] - lag[last] <= bound`.
    fn window_bounds(
        &self,
        path: &Traced,
        end: usize,
        into_fixed: bool,
        period: i64,
        delays: &dyn RetimeDelays,
    ) -> Vec<(usize, usize, i64)> {
        let zero = self.zero();
        let count = path.nodes.len();
        let end_edge = &self.edges[end];
        // Time at the input of node `s` along the path as it is now
        let input_at = |s: usize| -> i64 {
            if s == 0 {
                path.first_at
            } else {
                path.times[s - 1] + self.wire_direct(&self.edges[path.links[s - 1]])
            }
        };
        // The routing after a register put on the link into node `s`
        let after_register = |s: usize| -> i64 {
            if s == 0 {
                path.first_in
                    .map_or(0, |e| self.wire_after_register(&self.edges[e]))
            } else {
                self.wire_after_register(&self.edges[path.links[s - 1]])
            }
        };
        // The routing before a register put on the link out of node `e`
        let before_register = |e: usize| -> i64 {
            if e + 1 < count {
                self.wire_to_register(&self.edges[path.links[e]])
            } else {
                self.wire_to_register(end_edge)
            }
        };
        // Registers originally between node `a` and node `b` (a <= b)
        let mut prefix = vec![0i64; count];
        for k in 1..count {
            prefix[k] = prefix[k - 1] + self.edges[path.links[k - 1]].regs.len() as i64;
        }
        let first_regs = path.first_in.map_or(0, |e| self.edges[e].regs.len() as i64);
        let end_regs = end_edge.regs.len() as i64;
        let mut found = Vec::new();
        // Starts: the fixed start (as -1), then a register before each node
        let fixed_start = matches!(path.start, TraceStart::Fixed(_));
        let mut e = 0usize;
        let begin = if fixed_start { -1i64 } else { 0 };
        for s in begin..count as i64 {
            let (base, from, regs_before) = if s < 0 {
                (input_at(0), zero, first_regs)
            } else {
                let s = s as usize;
                (delays.clk_to_q_ps() + after_register(s), path.nodes[s], 0)
            };
            let first = s.max(0) as usize;
            let offset = input_at(first);
            if e < first {
                e = first;
            }
            // Smallest end from here that is too long at a register
            while e < count
                && base + path.times[e] - offset + before_register(e) + delays.setup_ps() <= period
            {
                e += 1;
            }
            if e < count {
                let regs = regs_before + prefix[e] - prefix[first];
                found.push((from, path.nodes[e], regs - 1));
            } else if into_fixed
                && base + path.times[count - 1] - offset
                    + self.wire_direct(end_edge)
                    + end_edge.exit_ps
                    > period
            {
                let regs = regs_before + prefix[count - 1] - prefix[first] + end_regs;
                found.push((from, zero, regs - 1));
            } else {
                // Later starts are shorter still
                break;
            }
        }
        found
    }

    /// A too-long path as a report
    fn report(
        &self,
        path: &TooLong,
        lags: &[i64],
        delays: &dyn RetimeDelays,
        why: &str,
    ) -> BlockedPath {
        let mut steps = Vec::new();
        match path.start {
            TraceStart::Fixed(e) => {
                let edge = &self.edges[e];
                if let Origin::Fixed(net) = edge.origin {
                    steps.extend(self.outside_trace(net, delays));
                    if steps.is_empty() {
                        let name = &self.netlist.nets[net.0 as usize].name;
                        steps.push(format!("net {name}, arriving at {} ps", edge.entry_ps));
                    }
                }
            }
            TraceStart::Register => steps.push("a register".to_string()),
            TraceStart::Open => {}
        }
        for &node in &path.nodes {
            let cell = self.cell(node);
            steps.push(format!(
                "{} {} (lag {})",
                cell.cell_type, cell.path, lags[node]
            ));
        }
        let end = &self.edges[path.end_edge];
        if let Sink::Cell(cell, _) = end.sink {
            if path.into_fixed {
                let cell = &self.netlist.cells[cell.0 as usize];
                steps.push(format!(
                    "{} {} (stays put; {} ps more to a register)",
                    cell.cell_type, cell.path, end.exit_ps
                ));
            }
        }
        BlockedPath {
            delay_ps: path.delay_ps,
            from_outside: matches!(path.start, TraceStart::Fixed(_)),
            steps,
            why: why.to_string(),
        }
    }

    /// Lags that bring every path within `period`, or the path that keeps
    /// it from being reached.
    ///
    /// Leiserson and Saxe's conditions, generated as they are needed: a
    /// path too long with the current lags needs a register somewhere
    /// along it in any retiming, which is a difference constraint between
    /// the lags of its ends (or lag 0, for an end that stays put). The
    /// constraints and the legal ones ([`Self::legal_bounds`]) are solved
    /// by Bellman-Ford, the timing redone, and any new too-long path adds
    /// its constraint, until none is too long. A negative cycle means no
    /// lags meet them all; its paths are the reason.
    ///
    /// Of all solutions it takes the one that moves registers least: each
    /// lag as near 0 as the constraints let it be. Paths too long whatever
    /// the lags (into or out of things that stay put) are skipped and
    /// reported.
    fn feasible(&self, period: i64, delays: &dyn RetimeDelays) -> Result<Feasible, BlockedPath> {
        let zero = self.zero();
        let mut bounds = self.legal_bounds();
        let mut frozen: Vec<BlockedPath> = Vec::new();
        // Edges whose path can never meet the period. Timing then treats
        // them as a register end (into something that stays put) or start
        // (from it), and a register already on one stays there, so the
        // logic behind them is still timed.
        let mut skip = vec![false; self.edges.len()];
        let mut end_here = vec![false; self.edges.len()];
        let mut lags = vec![0i64; zero];
        // Paths with no node to place a register on: from something that
        // stays put to registers on its first edge, and from registers on
        // an edge into something that stays put
        for (index, edge) in self.edges.iter().enumerate() {
            match (edge.origin, edge.sink) {
                (Origin::Fixed(_), _) => {
                    let t = edge.entry_ps + self.wire_to_register(edge) + delays.setup_ps();
                    if t > period {
                        let departures = self.departures(&lags, delays);
                        frozen.push(self.blocked(
                            index,
                            t,
                            &lags,
                            &departures,
                            delays,
                            ENDS_AT_FIXED_REGISTERS,
                        ));
                        skip[index] = true;
                        if let (Sink::Node(v, _), regs @ 1..) = (edge.sink, edge.regs.len()) {
                            bounds.push((zero, v, regs as i64 - 1));
                        }
                    }
                }
                (Origin::Node(u, _), Sink::Cell(c, _)) => {
                    let t = delays.clk_to_q_ps() + self.wire_after_register(edge) + edge.exit_ps;
                    if t > period {
                        let from = self.cell(u);
                        let sink = &self.netlist.cells[c.0 as usize];
                        frozen.push(BlockedPath {
                            delay_ps: t,
                            from_outside: false,
                            steps: vec![
                                format!("{REGISTER_AFTER} {} {}", from.cell_type, from.path),
                                format!(
                                    "{} {} (stays put; {} ps more to a register)",
                                    sink.cell_type, sink.path, edge.exit_ps
                                ),
                            ],
                            why: "a register feeds a cell that stays put".to_string(),
                        });
                        end_here[index] = true;
                        if !edge.regs.is_empty() {
                            bounds.push((u, zero, edge.regs.len() as i64 - 1));
                        }
                    }
                }
                _ => {}
            }
        }

        let legal = bounds.len();
        // How far each lag can go by these constraints alone, to tell a
        // path no lags shorten from one competing with others
        let (lowest, highest) = match (
            solve_bounds(zero + 1, zero, &bounds, false),
            solve_bounds(zero + 1, zero, &bounds, true),
        ) {
            (Ok(lo), Ok(hi)) => (lo, hi),
            _ => {
                return Err(BlockedPath {
                    delay_ps: 0,
                    from_outside: false,
                    steps: Vec::new(),
                    why: "the registers' loops leave no legal retiming".to_string(),
                })
            }
        };
        // The path behind each constraint after the first `legal` (none for
        // one that keeps a register in place)
        let mut paths: Vec<Option<TooLong>> = Vec::new();
        let mut known: HashMap<(usize, usize), usize> = HashMap::new();
        let rounds = 64 + self.nodes.len().min(4_000);
        for _ in 0..rounds {
            lags = match solve_lags(zero + 1, zero, &bounds) {
                Ok(lags) => lags[..zero].to_vec(),
                Err(cycle) => {
                    // The too-long paths on the cycle compete for the same
                    // registers; name the slowest
                    let on_cycle: Vec<&TooLong> = cycle
                        .iter()
                        .filter(|&&b| b >= legal)
                        .filter_map(|&b| paths[b - legal].as_ref())
                        .collect();
                    let worst = on_cycle.iter().max_by_key(|p| p.delay_ps);
                    let why = match on_cycle.len() {
                        0 | 1 => "registers cannot move far enough to split it".to_string(),
                        n => format!(
                            "it needs the same registers as {} other too-long path{}",
                            n - 1,
                            if n == 2 { "" } else { "s" }
                        ),
                    };
                    return Err(match worst {
                        Some(path) => self.report(path, &path.lags, delays, &why),
                        None => BlockedPath {
                            delay_ps: 0,
                            from_outside: false,
                            steps: Vec::new(),
                            why,
                        },
                    });
                }
            };
            let arrivals = self.arrivals(&lags, delays, &skip);
            let shared_lags = std::rc::Rc::new(lags.clone());
            let mut changed = false;
            let mut too_long = false;
            for (index, edge) in self.edges.iter().enumerate() {
                let Origin::Node(u, out) = edge.origin else {
                    continue;
                };
                let arrival = arrivals[u][out].0;
                if arrival <= UNREACHED / 2 {
                    continue;
                }
                let weight = self.weight(edge, &lags);
                let (delay_ps, into_fixed) = match edge.sink {
                    _ if weight > 0 || end_here[index] => (
                        arrival + self.wire_to_register(edge) + delays.setup_ps(),
                        false,
                    ),
                    Sink::Cell(..) => (arrival + self.wire_direct(edge) + edge.exit_ps, true),
                    Sink::Node(..) => continue,
                };
                if delay_ps <= period {
                    continue;
                }
                too_long = true;
                let traced = self.trace_path(u, out, &lags, &arrivals, delays, &skip);
                let windows = self.window_bounds(&traced, index, into_fixed, period, delays);
                let path = TooLong {
                    delay_ps,
                    nodes: traced.nodes.clone(),
                    start: traced.start,
                    end_edge: index,
                    into_fixed,
                    lags: shared_lags.clone(),
                };
                let mut hopeless = false;
                for &(first, last, bound) in &windows {
                    if first == last && first != zero && bound < 0 {
                        return Err(self.report(
                            &path,
                            &lags,
                            delays,
                            "a single cell is slower than the clock",
                        ));
                    }
                    hopeless |= match (first == zero, last == zero) {
                        (true, true) => bound < 0,
                        (false, true) => lowest[first] > bound,
                        (true, false) => -highest[last] > bound,
                        (false, false) => false,
                    };
                }
                if hopeless {
                    let why = if into_fixed {
                        "it ends at a cell that stays put"
                    } else {
                        ENDS_AT_FIXED_REGISTERS
                    };
                    frozen.push(self.report(&path, &lags, delays, why));
                    // Time the logic behind it as if a register ended (or
                    // started) it there, keeping any register there now
                    let keep = match traced.start {
                        TraceStart::Fixed(e) if !into_fixed => {
                            skip[e] = true;
                            let start = &self.edges[e];
                            match start.sink {
                                Sink::Node(v, _) if self.weight(start, &lags) > 0 => {
                                    Some((zero, v, start.regs.len() as i64 - 1))
                                }
                                _ => None,
                            }
                        }
                        _ => {
                            end_here[index] = true;
                            (weight > 0).then_some((u, zero, edge.regs.len() as i64 - 1))
                        }
                    };
                    if let Some(bound) = keep {
                        bounds.push(bound);
                        paths.push(None);
                    }
                    changed = true;
                    continue;
                }
                for (first, last, bound) in windows {
                    match known.get(&(first, last)) {
                        Some(&b) if bounds[b].2 <= bound => {}
                        Some(&b) => {
                            bounds[b].2 = bound;
                            paths[b - legal] = Some(path.clone());
                            changed = true;
                        }
                        None => {
                            known.insert((first, last), bounds.len());
                            bounds.push((first, last, bound));
                            paths.push(Some(path.clone()));
                            changed = true;
                        }
                    }
                }
            }
            if !too_long {
                return Ok(Feasible { lags, frozen });
            }
            if !changed {
                break;
            }
        }
        let worst = paths.iter().flatten().max_by_key(|p| p.delay_ps);
        Err(match worst {
            Some(path) => self.report(path, &lags, delays, "the search did not settle"),
            None => BlockedPath {
                delay_ps: 0,
                from_outside: false,
                steps: Vec::new(),
                why: "the search did not settle".to_string(),
            },
        })
    }

    /// Make the moves `lags` asks for one at a time, carrying initial values
    /// through each cell. Returns the edges' final registers, or a node a
    /// backward move cannot be made through (its output registers disagree,
    /// or no inputs give their values).
    fn realize(&self, lags: &[i64]) -> Result<Vec<Vec<bool>>, usize> {
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
                while done[node] < lags[node] && self.blocks[self.block[node]].len() > 1 {
                    // A carry chain moves back as one: its cells' inputs
                    // are chosen together, so each carry in is one the
                    // cell before can give
                    if !self.back_through_chain(self.block[node], &mut regs)? {
                        remaining = true;
                        break;
                    }
                    for &member in &self.blocks[self.block[node]] {
                        done[member] += 1;
                    }
                    progress = true;
                }
                while done[node] < lags[node] && self.blocks[self.block[node]].len() == 1 {
                    let outs = &self.out_edges[node];
                    if outs.iter().flatten().any(|&e| regs[e].is_empty()) {
                        remaining = true;
                        break;
                    }
                    // The value each output's registers hold, origin end
                    let mut wanted: Vec<Option<bool>> = Vec::with_capacity(outs.len());
                    for edges in outs {
                        let mut value = None;
                        for &e in edges {
                            let v = regs[e][0];
                            if value.is_some_and(|w| w != v) {
                                return Err(node);
                            }
                            value = Some(v);
                        }
                        wanted.push(value);
                    }
                    let inputs = self.preimage(node, &wanted, &regs).ok_or(node)?;
                    for &e in outs.iter().flatten() {
                        regs[e].remove(0);
                    }
                    for (pin, e) in self.in_edges[node].iter().enumerate() {
                        if let Some(e) = *e {
                            if !matches!(self.edges[e].origin, Origin::Const(..)) {
                                regs[e].push(inputs[pin]);
                            }
                        }
                    }
                    done[node] += 1;
                    progress = true;
                }
            }
            if !remaining {
                break;
            }
            if !progress {
                return Err(usize::MAX);
            }
        }
        Ok(regs)
    }

    /// The value a register put on input edge `edge` by a backward move
    /// should hold: what the registers nearest the origin hold on the other
    /// edges from the same output, so the origin can later move back too
    /// (its output registers must agree) and the edges share one register.
    /// `None` when the edge already holds registers or no sibling does.
    fn preferred_value(&self, edge: usize, regs: &[Vec<bool>]) -> Option<bool> {
        if !regs[edge].is_empty() {
            return None;
        }
        let Origin::Node(u, out) = self.edges[edge].origin else {
            return None;
        };
        self.out_edges[u][out]
            .iter()
            .filter(|&&e| e != edge)
            .find_map(|&e| regs[e].first().copied())
    }

    /// Whether `edge` joins two cells of one carry chain (carry out to
    /// carry in)
    fn is_carry_link(&self, edge: usize) -> bool {
        match (self.edges[edge].origin, self.edges[edge].sink) {
            (Origin::Node(u, CARRY_OUT_PIN), Sink::Node(v, CARRY_IN_PIN)) => {
                self.block[u] == self.block[v] && is_carry_out(self.cell(u), CARRY_OUT_PIN)
            }
            _ => false,
        }
    }

    /// The cells of carry block `block`, from the start of the chain
    fn chain_order(&self, block: usize) -> Vec<usize> {
        let members = &self.blocks[block];
        let fed_by_chain = |node: usize| {
            self.in_edges[node]
                .get(CARRY_IN_PIN)
                .copied()
                .flatten()
                .is_some_and(|e| self.is_carry_link(e))
        };
        let Some(&start) = members.iter().find(|&&n| !fed_by_chain(n)) else {
            return members.clone();
        };
        let mut chain = vec![start];
        while chain.len() < members.len() {
            let last = *chain.last().expect("not empty");
            let next = self.out_edges[last]
                .get(CARRY_OUT_PIN)
                .into_iter()
                .flatten()
                .find(|&&e| self.is_carry_link(e))
                .and_then(|&e| match self.edges[e].sink {
                    Sink::Node(v, _) => Some(v),
                    Sink::Cell(..) => None,
                });
            match next {
                Some(next) if !chain.contains(&next) => chain.push(next),
                _ => break,
            }
        }
        chain
    }

    /// Move one register back through every cell of carry block `block`.
    /// The carry links inside the chain hold no register before or after:
    /// the cells' inputs are found together, cell by cell from the start
    /// of the chain, keeping for each the carry ins the cell before can
    /// give. `Ok(false)` when an output has no register yet; the error is
    /// the cell no inputs work for (or whose output registers disagree).
    fn back_through_chain(&self, block: usize, regs: &mut [Vec<bool>]) -> Result<bool, usize> {
        let chain = self.chain_order(block);
        if chain.len() != self.blocks[block].len() {
            return Err(chain[0]);
        }
        // The values the registers on each cell's outputs hold
        let mut wanted: Vec<Vec<Option<bool>>> = Vec::with_capacity(chain.len());
        for &node in &chain {
            let mut values = Vec::with_capacity(self.out_edges[node].len());
            for edges in &self.out_edges[node] {
                let mut value = None;
                for &e in edges {
                    if self.is_carry_link(e) {
                        continue;
                    }
                    let Some(&v) = regs[e].first() else {
                        return Ok(false);
                    };
                    if value.is_some_and(|w| w != v) {
                        return Err(node);
                    }
                    value = Some(v);
                }
                values.push(value);
            }
            wanted.push(values);
        }
        // Per cell, per carry out value: the carry in and inputs giving it
        let mut choices: Vec<[Option<Vec<bool>>; 2]> = Vec::with_capacity(chain.len());
        let first_carry_in = self.in_edges[chain[0]].get(CARRY_IN_PIN).copied().flatten();
        let mut reach = match first_carry_in.map(|e| self.edges[e].origin) {
            None => [true, false],
            Some(Origin::Const(_, value)) => [!value, value],
            Some(_) => [true, true],
        };
        for (k, &node) in chain.iter().enumerate() {
            let cell = self.cell(node);
            let primitive = primitive_for_cell(cell);
            let pins = &self.in_edges[node];
            let mut base = vec![false; pins.len()];
            let mut free = Vec::new();
            for (pin, e) in pins.iter().enumerate() {
                if pin == CARRY_IN_PIN {
                    continue;
                }
                if let Some(e) = *e {
                    match self.edges[e].origin {
                        Origin::Const(_, value) => base[pin] = value,
                        _ => free.push(pin),
                    }
                }
            }
            let prefer: Vec<Option<bool>> = free
                .iter()
                .map(|&pin| pins[pin].and_then(|e| self.preferred_value(e, regs)))
                .collect();
            let candidates = input_candidates(&prefer);
            let mut found: [Option<Vec<bool>>; 2] = [None, None];
            for carry_in in [false, true] {
                if !reach[carry_in as usize] {
                    continue;
                }
                for values in &candidates {
                    let mut inputs = base.clone();
                    for (&pin, &value) in free.iter().zip(values) {
                        inputs[pin] = value;
                    }
                    if let Some(slot) = inputs.get_mut(CARRY_IN_PIN) {
                        *slot = carry_in;
                    }
                    let outputs = evaluate_primitive(&primitive, &inputs);
                    let fits = wanted[k].iter().enumerate().all(|(out, w)| {
                        w.is_none_or(|w| outputs.get(out).copied().unwrap_or(false) == w)
                    });
                    if !fits {
                        continue;
                    }
                    let carry_out = outputs.get(CARRY_OUT_PIN).copied().unwrap_or(false);
                    if found[carry_out as usize].is_none() {
                        found[carry_out as usize] = Some(inputs);
                    }
                    if found.iter().all(Option::is_some) {
                        break;
                    }
                }
            }
            reach = [found[0].is_some(), found[1].is_some()];
            if reach == [false, false] {
                return Err(node);
            }
            choices.push(found);
        }
        // Back from the end of the chain: each cell's carry in is the
        // carry out the cell before must give
        let mut chosen: Vec<Vec<bool>> = vec![Vec::new(); chain.len()];
        let mut carry_out = if choices[chain.len() - 1][0].is_some() {
            0
        } else {
            1
        };
        for k in (0..chain.len()).rev() {
            let inputs = choices[k][carry_out]
                .clone()
                .expect("reachable by construction");
            carry_out = inputs.get(CARRY_IN_PIN).copied().unwrap_or(false) as usize;
            chosen[k] = inputs;
        }
        for (k, &node) in chain.iter().enumerate() {
            for &e in self.out_edges[node].iter().flatten() {
                if !self.is_carry_link(e) {
                    regs[e].remove(0);
                }
            }
            for (pin, e) in self.in_edges[node].iter().enumerate() {
                if let Some(e) = *e {
                    if !self.is_carry_link(e) && !matches!(self.edges[e].origin, Origin::Const(..))
                    {
                        regs[e].push(chosen[k][pin]);
                    }
                }
            }
        }
        Ok(true)
    }

    /// Inputs of `node` for which its outputs take the `wanted` values
    /// (`None`: any), agreeing with the registers already on sibling edges
    /// where they can ([`Self::preferred_value`]), then fewest ones first;
    /// constant inputs keep their value
    fn preimage(
        &self,
        node: usize,
        wanted: &[Option<bool>],
        regs: &[Vec<bool>],
    ) -> Option<Vec<bool>> {
        let cell = self.cell(node);
        let primitive = primitive_for_cell(cell);
        let pins = &self.in_edges[node];
        let mut base = vec![false; pins.len()];
        let mut free = Vec::new();
        for (pin, e) in pins.iter().enumerate() {
            if let Some(e) = *e {
                match self.edges[e].origin {
                    Origin::Const(_, value) => base[pin] = value,
                    _ => free.push(pin),
                }
            }
        }
        let prefer: Vec<Option<bool>> = free
            .iter()
            .map(|&pin| pins[pin].and_then(|e| self.preferred_value(e, regs)))
            .collect();
        input_candidates(&prefer).into_iter().find_map(|values| {
            let mut inputs = base.clone();
            for (&pin, value) in free.iter().zip(values) {
                inputs[pin] = value;
            }
            let outputs = evaluate_primitive(&primitive, &inputs);
            wanted
                .iter()
                .enumerate()
                .all(|(out, w)| w.is_none_or(|w| outputs.get(out).copied().unwrap_or(false) == w))
                .then_some(inputs)
        })
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
    /// Replace the movable registers with the retimed ones. Returns each
    /// old cell's new id and the ids of the registers added.
    fn apply(self, netlist: &mut GateNetlist) -> (Vec<Option<CellId>>, std::ops::Range<usize>) {
        // New registers take names no cell has yet (an earlier retiming of
        // the same regions named its own the same way)
        let taken: HashSet<String> = netlist.cells.iter().map(|c| c.path.clone()).collect();
        let mut serial = 0usize;
        let mut fresh = |prefix: &str| loop {
            let path = format!("{prefix}.retime{serial}");
            serial += 1;
            if !taken.contains(&path) {
                break path;
            }
        };
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
                        let path = fresh(&edge.prefix);
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
        let mut next = 0u32;
        let cell_map: Vec<Option<CellId>> = netlist
            .cells
            .iter()
            .map(|c| {
                (!self.removed.contains(&c.id)).then(|| {
                    next += 1;
                    CellId(next - 1)
                })
            })
            .collect();
        netlist.cells.retain(|c| !self.removed.contains(&c.id));
        let added = netlist.cells.len()..netlist.cells.len() + new_cells.len();
        netlist.cells.extend(new_cells);
        netlist.rebuild_net_connectivity();
        netlist.rebuild_cache();
        (cell_map, added)
    }
}

/// The fewest registers to add at the inputs of `region` (as
/// `#[pipeline]` adds them) for retiming to bring the paths through
/// `regions` within `target_period_ps`, trying up to `max_extra`.
///
/// `regions` are all the retimable instance paths (`region` among them, or
/// added): the added registers may move forward into `region` and backward
/// into the retimable logic feeding it. Every edge into `region` from
/// outside it gets the extra registers, which is what delaying each input
/// by that many cycles does.
///
/// Returns the stages and the paths too long however many are added (logic
/// outside the regions, for one): they limit the clock, but are not the
/// region's to fix. When even `max_extra` is not enough, the error is the
/// path that stays too long (`None` when the region has no logic to
/// retime).
pub fn extra_stages_for_period(
    netlist: &GateNetlist,
    delays: &dyn RetimeDelays,
    regions: &[String],
    region: &str,
    target_period_ps: i64,
    max_extra: u32,
) -> Result<(u32, Vec<BlockedPath>), Option<BlockedPath>> {
    let mut all = regions.to_vec();
    if !all.iter().any(|r| r == region) {
        all.push(region.to_string());
    }
    let keep = cone_of(netlist, &all, region);
    let base = Graph::build(netlist, &all, delays, Some(&keep)).ok_or(None)?;
    if !base
        .nodes
        .iter()
        .any(|&id| is_inside(&netlist.cells[id.0 as usize].path, region))
    {
        return Err(None);
    }
    let mut inside = vec![false; netlist.nets.len()];
    for cell in &netlist.cells {
        if is_inside(&cell.path, region) {
            for &net in &cell.outputs {
                inside[netlist.resolve_alias(net).0 as usize] = true;
            }
        }
    }
    let driven_inside = |net: GateNetId| inside[netlist.resolve_alias(net).0 as usize];
    let inputs: Vec<usize> = base
        .edges
        .iter()
        .enumerate()
        .filter(|(_, e)| {
            let sink_inside = match e.sink {
                Sink::Node(node, _) => {
                    is_inside(&netlist.cells[base.nodes[node].0 as usize].path, region)
                }
                Sink::Cell(cell, _) => is_inside(&netlist.cells[cell.0 as usize].path, region),
            };
            let origin_inside = match e.origin {
                Origin::Node(node, _) => {
                    is_inside(&netlist.cells[base.nodes[node].0 as usize].path, region)
                }
                Origin::Fixed(net) => driven_inside(net),
                Origin::Const(..) => true,
            };
            sink_inside && !origin_inside
        })
        .map(|(i, _)| i)
        .collect();
    let try_extra = |extra: u32| {
        let mut graph = base.clone();
        for &i in &inputs {
            graph.edges[i]
                .regs
                .extend(std::iter::repeat_n(false, extra as usize));
        }
        graph.feasible(target_period_ps, delays)
    };
    // The paths no number of stages shortens: those still too long with the
    // most stages
    let most = try_extra(max_extra).map_err(Some)?;
    // One through the region's own logic is the region's to fix, and
    // stages can't. (One that only starts or ends at a cell of the region
    // that stays put, such as a register with a reset, is a limit of the
    // logic around it.)
    if let Some(path) = most.frozen.iter().find(|p| {
        p.steps
            .iter()
            .filter(|s| s.ends_with(')') && s.contains(" (lag "))
            .any(|s| s.split_whitespace().any(|w| is_inside(w, region)))
    }) {
        return Err(Some(path.clone()));
    }
    let irreducible = most.frozen.len();
    // The fewest stages that leave only those (more stages never hurt)
    let (mut lo, mut hi) = (0u32, max_extra);
    let mut best = most;
    while lo < hi {
        let mid = (lo + hi) / 2;
        match try_extra(mid) {
            Ok(found) if found.frozen.len() <= irreducible => {
                hi = mid;
                best = found;
            }
            _ => lo = mid + 1,
        }
    }
    Ok((lo, best.frozen))
}

/// The cells of `region`, and the cells of `regions` its logic reaches
/// without a register between: the logic feeding it and the logic it
/// feeds. Registers added at its inputs can move through these; the rest of
/// the regions do not bear on its stages.
fn cone_of(netlist: &GateNetlist, regions: &[String], region: &str) -> HashSet<CellId> {
    let resolve = |net: GateNetId| netlist.resolve_alias(net).0 as usize;
    let mut driver: Vec<Option<CellId>> = vec![None; netlist.nets.len()];
    let mut loads: Vec<Vec<CellId>> = vec![Vec::new(); netlist.nets.len()];
    for cell in &netlist.cells {
        for &net in &cell.outputs {
            driver[resolve(net)] = Some(cell.id);
        }
        for &net in &cell.inputs {
            loads[resolve(net)].push(cell.id);
        }
    }
    let cell = |id: CellId| &netlist.cells[id.0 as usize];
    let in_regions = |id: CellId| region_of(&cell(id).path, regions).is_some();
    let mut keep: HashSet<CellId> = netlist
        .cells
        .iter()
        .filter(|c| is_inside(&c.path, region))
        .map(|c| c.id)
        .collect();
    // Back through the logic feeding the region
    let mut queue: Vec<CellId> = keep.iter().copied().collect();
    let mut seen = keep.clone();
    while let Some(id) = queue.pop() {
        for &net in &cell(id).inputs {
            if let Some(d) = driver[resolve(net)] {
                if in_regions(d) && seen.insert(d) {
                    keep.insert(d);
                    if cell(d).clock.is_none() {
                        queue.push(d);
                    }
                }
            }
        }
    }
    // On through the logic it feeds
    let mut queue: Vec<CellId> = netlist
        .cells
        .iter()
        .filter(|c| is_inside(&c.path, region))
        .map(|c| c.id)
        .collect();
    let mut seen: HashSet<CellId> = queue.iter().copied().collect();
    while let Some(id) = queue.pop() {
        for &net in &cell(id).outputs {
            for &l in &loads[resolve(net)] {
                if in_regions(l) && seen.insert(l) {
                    keep.insert(l);
                    if cell(l).clock.is_none() {
                        queue.push(l);
                    }
                }
            }
        }
    }
    keep
}

/// The longest path through `regions` that retiming times (from a movable
/// register, or into the regions' logic from something that stays put)
/// with `delays` and the registers where they are: its delay in ps, then
/// where it starts and the cells along it. `None` when the regions have
/// nothing to retime.
pub fn longest_path(
    netlist: &GateNetlist,
    delays: &dyn RetimeDelays,
    regions: &[String],
) -> Option<(i64, Vec<String>)> {
    let graph = Graph::build(netlist, regions, delays, None)?;
    let zero = vec![0i64; graph.nodes.len()];
    let mut steps = graph.critical_path(&zero, delays);
    if !steps.is_empty() {
        steps.remove(0);
    }
    Some((graph.period(&zero, delays), steps))
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
