//! Matched delays of bundled-data async blocks
//!
//! An async instance under a clocked parent is lowered to single-rail logic
//! timed by matched delays (see `ncl_expand::expand_to_ncl_bridged`). Each
//! matched delay is `out = in & delay(in)`, mapped to a chain of kept buffer
//! cells and a kept AND (`MATCHED_DELAY_OP`). Its output times either
//!
//! * the registers of the block clocked by it: their data inputs, computed
//!   from the block's inputs, must settle before the clock edge reaches them;
//! * the block's acknowledge output: every other output of the block must
//!   settle before the acknowledge rises, and so must the next state the
//!   block's registers compute from their own outputs (the next control
//!   edge only comes after the parent has seen the acknowledge).
//!
//! A chain resets fast: its first stage is a buffer of the input and every
//! later stage is a kept AND of the input and the stage before. A rising
//! input ripples through the whole chain, but a falling one clears every
//! stage within one gate delay. Without that, a request that falls and rises
//! again before the falling edge has left a plain buffer chain would find
//! the chain's tail still high and raise the control at once, without the
//! delay the data paths need. With it, the return to zero is as quick as the
//! handshake around it.
//!
//! [`check_matched_delays`] measures both against a delay model: library
//! estimates before placement ([`LibraryDelays`]), routed delays after (the
//! place-and-route crate supplies its own [`BundleDelays`]).
//! [`size_matched_delays`] lengthens each chain until its checks pass.
//!
//! A self-timed block that arbitrates between streams (`match arbitrate`)
//! has a mutual-exclusion element in front of its step request. When two
//! requests arrive at nearly the same time the element can take a while to
//! decide (metastability); the chance that it is still undecided after a
//! wait `t` falls as `exp(-t / tau)`. The step strobe `go` only rises once
//! the step request has stayed high for the whole matched delay, so that
//! delay is the resolution wait. The "arbiter" check makes it at least the
//! wait a target mean time between failures needs ([`ArbiterTiming`]).
//!
//! All times are measured from the block's inputs, which the clocked parent
//! holds stable from before it raises the request; the request itself is
//! one of those inputs.

use crate::gate_netlist::{
    Cell, CellId, GateNet, GateNetId, GateNetlist, ARBITER_OP, MATCHED_DELAY_OP, STEP_CONTROL_OP,
};
use crate::tech_library::{CellFunction, TechLibrary};
use std::collections::{HashMap, HashSet};

/// One matched delay found in a netlist
#[derive(Debug, Clone)]
pub struct MatchedDelay {
    /// Hierarchical prefix of the async block it belongs to (`top.u.`)
    pub prefix: String,
    /// The kept AND whose output is the timed control signal
    pub and_cell: CellId,
    /// The chain's stages (a buffer, then ANDs with the source), from the
    /// source to the AND
    pub chain: Vec<CellId>,
    /// The net being delayed (the request, or the previous control)
    pub source: GateNetId,
    /// The delayed control net
    pub control: GateNetId,
}

/// Delays a bundle check needs, in picoseconds
pub trait BundleDelays {
    /// Routing delay of `net` to data input `input` of `cell`
    fn wire_ps(&self, netlist: &GateNetlist, net: GateNetId, cell: &Cell, input: usize) -> i64;
    /// Routing delay of `net` to the clock pin of `cell`
    fn clock_wire_ps(&self, netlist: &GateNetlist, net: GateNetId, cell: &Cell) -> i64;
    /// Delay through `cell` from `input` to `output`, `None` if no path
    fn arc_ps(&self, cell: &Cell, input: usize, output: usize) -> Option<i64>;
    /// Clock-to-output delay of a register
    fn clock_to_out_ps(&self, cell: &Cell) -> i64;
    /// Whether output `output` of clocked `cell` is launched by its clock
    /// (a register output). A clocked cell can also have combinational
    /// outputs, such as the read port of a distributed RAM, whose clock only
    /// writes: those are timed through the cell's arcs instead.
    fn launches(&self, cell: &Cell, _output: usize) -> bool {
        cell.clock.is_some()
    }
    /// Setup time of a register's data input `input`
    fn setup_ps(&self, cell: &Cell, input: usize) -> i64;
    /// Metastability of the arbiters' mutual-exclusion elements, and the
    /// failure rate they must meet
    fn arbiter_timing(&self) -> ArbiterTiming {
        ArbiterTiming::default()
    }
}

/// Seconds in a (Julian) year
const SECONDS_PER_YEAR: f64 = 365.25 * 24.0 * 3600.0;

/// How long an arbiter must wait for its mutual-exclusion element to
/// decide. With requests arriving at rates `f1` and `f2`, the mean time
/// between failures after a wait `t` is `exp(t / tau) / (T0 * f1 * f2)`.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ArbiterTiming {
    /// Resolution time constant of the element, in picoseconds
    pub tau_ps: f64,
    /// Metastability window `T0`, in picoseconds
    pub t0_ps: f64,
    /// Highest rate at which each request can rise, in hertz
    pub request_rate_hz: f64,
    /// Required mean time between failures, per arbiter, in years
    pub mtbf_years: f64,
}

impl Default for ArbiterTiming {
    /// Conservative figures for a latch built from FPGA LUTs, without
    /// measured device data: tau 60 ps, T0 1 ns, requests at up to 200 MHz,
    /// 1000 years between failures
    fn default() -> Self {
        Self {
            tau_ps: 60.0,
            t0_ps: 1000.0,
            request_rate_hz: 200e6,
            mtbf_years: 1000.0,
        }
    }
}

impl ArbiterTiming {
    /// Failure rate of one arbiter that waits nothing, per second
    fn base_rate(&self) -> f64 {
        self.t0_ps * 1e-12 * self.request_rate_hz * self.request_rate_hz
    }

    /// The wait that meets [`Self::mtbf_years`], in picoseconds
    pub fn resolution_ps(&self) -> i64 {
        let x = self.mtbf_years * SECONDS_PER_YEAR * self.base_rate();
        if x <= 1.0 {
            return 0;
        }
        (self.tau_ps * x.ln()).ceil() as i64
    }

    /// Mean time between failures after waiting `wait_ps`, in years
    pub fn mtbf_years_after(&self, wait_ps: i64) -> f64 {
        (wait_ps.max(0) as f64 / self.tau_ps).exp() / self.base_rate() / SECONDS_PER_YEAR
    }
}

/// Result of timing one matched delay
#[derive(Debug, Clone)]
pub struct BundleCheck {
    /// Block prefix (`top.u.`)
    pub prefix: String,
    /// Path of the matched delay's AND cell
    pub delay_path: String,
    /// What the control times: "registers", "acknowledge", "next state",
    /// "channel" or "arbiter"
    pub times: &'static str,
    /// Latest data arrival plus setup, in picoseconds. For an "arbiter"
    /// check, the resolution wait its arbiters need.
    pub data_ps: i64,
    /// Arrival of the control signal, in picoseconds. For an "arbiter"
    /// check, how long the step request must stay high before `go`.
    pub control_ps: i64,
    /// Buffers in the chain
    pub chain_len: usize,
    /// For a "channel" check, the block that sends (its prefix): data and
    /// phase bit from it are timed from its own strobe
    pub sender: Option<String>,
}

impl BundleCheck {
    /// Control arrival minus data arrival: negative means the delay is too short
    pub fn slack_ps(&self) -> i64 {
        self.control_ps - self.data_ps
    }
}

/// The block prefix a matched delay's cell path belongs to
fn prefix_of(path: &str) -> Option<String> {
    path.find("ncl_bundle.").map(|i| path[..i].to_string())
}

/// Find every matched delay in `netlist`
pub fn find_matched_delays(netlist: &GateNetlist) -> Vec<MatchedDelay> {
    let mut result = Vec::new();
    for cell in &netlist.cells {
        if cell.source_op.as_deref() != Some(MATCHED_DELAY_OP)
            || !cell.path.ends_with("_and")
            || cell.inputs.len() < 2
            || cell.outputs.is_empty()
        {
            continue;
        }
        let Some(prefix) = prefix_of(&cell.path) else {
            continue;
        };
        let source = cell.inputs[0];
        // Walk the chain back from the AND's second input to the source
        let mut chain = Vec::new();
        let mut net = cell.inputs[1];
        let mut ok = false;
        for _ in 0..=netlist.cells.len() {
            if net == source {
                ok = true;
                break;
            }
            let Some(driver) = netlist.nets.get(net.0 as usize).and_then(|n| n.driver) else {
                break;
            };
            let Some(stage) = netlist.cells.get(driver.0 as usize) else {
                break;
            };
            if !stage.is_kept() || stage.source_op.as_deref() != Some(MATCHED_DELAY_OP) {
                break;
            }
            // A buffer of the source, or an AND of the source and the
            // previous stage
            net = match stage.inputs.as_slice() {
                [prev] => *prev,
                [s, prev] if *s == source => *prev,
                _ => break,
            };
            chain.push(stage.id);
        }
        if !ok {
            continue;
        }
        chain.reverse();
        result.push(MatchedDelay {
            prefix,
            and_cell: cell.id,
            chain,
            source,
            control: cell.outputs[0],
        });
    }
    result
}

/// Arrival-time propagation inside one block
struct Arrivals<'a> {
    netlist: &'a GateNetlist,
    delays: &'a dyn BundleDelays,
    prefix: &'a str,
    /// Registers launch at their clock's arrival (false: register outputs
    /// are not timed — the previous state, stable since the last edge)
    through_registers: bool,
    memo: HashMap<GateNetId, Option<i64>>,
    visiting: HashSet<GateNetId>,
    /// The input net that set each combinational arrival
    from: HashMap<GateNetId, GateNetId>,
    /// Nets where propagation met a combinational loop and stopped
    loop_cuts: usize,
    /// When set, only these nets from outside the block are timed, at the
    /// given arrival (the other block's strobe is time 0); other inputs are
    /// not part of the check
    sources: Option<HashMap<GateNetId, i64>>,
    /// The timed control nets (outputs of matched delays). They are not
    /// data: a control only changes after the strobe it belongs to, so a
    /// path from one into the block's logic (the arbiter's release from
    /// `done`, the `busy` acknowledge of a self-timed block) is not timed.
    controls: HashSet<GateNetId>,
}

impl<'a> Arrivals<'a> {
    fn new(
        netlist: &'a GateNetlist,
        delays: &'a dyn BundleDelays,
        prefix: &'a str,
        through_registers: bool,
    ) -> Self {
        Self {
            netlist,
            delays,
            prefix,
            through_registers,
            memo: HashMap::new(),
            visiting: HashSet::new(),
            from: HashMap::new(),
            loop_cuts: 0,
            sources: None,
            controls: find_matched_delays(netlist)
                .into_iter()
                .map(|m| m.control)
                .collect(),
        }
    }

    /// Time only from `sources` (see [`Arrivals::sources`])
    fn with_sources(mut self, sources: HashMap<GateNetId, i64>) -> Self {
        self.sources = Some(sources);
        self
    }

    /// The path that sets the arrival at `net`, latest net first, with
    /// each net's arrival
    fn path(&mut self, net: GateNetId) -> Vec<(GateNetId, Option<i64>)> {
        let mut path = vec![(net, self.at(net))];
        let mut net = net;
        while let Some(&prev) = self.from.get(&net) {
            if path.iter().any(|(n, _)| *n == prev) {
                break;
            }
            path.push((prev, self.at(prev)));
            net = prev;
        }
        path
    }
    fn inside(&self, cell: &Cell) -> bool {
        cell.path.starts_with(self.prefix)
    }

    /// Latest arrival at `net`, `None` if nothing timed reaches it
    fn at(&mut self, net: GateNetId) -> Option<i64> {
        if let Some(&a) = self.memo.get(&net) {
            return a;
        }
        if !self.visiting.insert(net) {
            // A loop (state-holding gate): the path through it is not a
            // data path. A self-timed block's own step loop (through its
            // step control and matched delays) is expected: not counted.
            let driver_kept = self.netlist.nets[net.0 as usize]
                .driver
                .is_some_and(|d| self.netlist.cells[d.0 as usize].is_kept());
            if driver_kept {
                return None;
            }
            self.loop_cuts += 1;
            if self.loop_cuts <= 5 {
                let n = &self.netlist.nets[net.0 as usize];
                tracing::debug!(
                    "NCL bundle: combinational loop through {} (driven by {})",
                    n.name,
                    n.driver
                        .map_or("-", |d| self.netlist.cells[d.0 as usize].path.as_str())
                );
            }
            return None;
        }
        let netlist = self.netlist;
        let result = match netlist.nets.get(net.0 as usize).and_then(|n| n.driver) {
            None => match &self.sources {
                Some(sources) => sources.get(&net).copied(),
                None => Some(0),
            },
            Some(driver) => {
                let cell = &netlist.cells[driver.0 as usize];
                if !self.inside(cell) {
                    // A block input: held stable by the parent, or one of
                    // the timed sources
                    match &self.sources {
                        Some(sources) => sources.get(&net).copied(),
                        None => Some(0),
                    }
                } else if let Some(clock) = cell.clock.filter(|_| {
                    self.delays
                        .launches(cell, netlist.nets[net.0 as usize].driver_pin.unwrap_or(0))
                }) {
                    if self.through_registers {
                        self.at(clock).map(|t| {
                            t + self.delays.clock_wire_ps(netlist, clock, cell)
                                + self.delays.clock_to_out_ps(cell)
                        })
                    } else {
                        None
                    }
                } else {
                    let output = netlist.nets[net.0 as usize].driver_pin.unwrap_or(0);
                    let timing_cell = matches!(
                        cell.source_op.as_deref(),
                        Some(MATCHED_DELAY_OP) | Some(STEP_CONTROL_OP)
                    );
                    let mut best: Option<i64> = None;
                    for (i, &input) in cell.inputs.iter().enumerate() {
                        if !timing_cell && self.controls.contains(&input) {
                            continue;
                        }
                        let Some(arc) = self.delays.arc_ps(cell, i, output) else {
                            continue;
                        };
                        if let Some(t) = self.at(input) {
                            let t = t + self.delays.wire_ps(netlist, input, cell, i) + arc;
                            if best.is_none_or(|b| t > b) {
                                self.from.insert(net, input);
                            }
                            best = Some(best.map_or(t, |b| b.max(t)));
                        }
                    }
                    best
                }
            }
        };
        self.visiting.remove(&net);
        self.memo.insert(net, result);
        result
    }
}

/// Time every matched delay of `netlist` with `delays`
pub fn check_matched_delays(netlist: &GateNetlist, delays: &dyn BundleDelays) -> Vec<BundleCheck> {
    let mut checks = Vec::new();
    let matched = find_matched_delays(netlist);
    let controls: HashSet<GateNetId> = matched.iter().map(|m| m.control).collect();

    for md in &matched {
        let and_cell = &netlist.cells[md.and_cell.0 as usize];
        let mut data_only = Arrivals::new(netlist, delays, &md.prefix, false);
        let mut full = Arrivals::new(netlist, delays, &md.prefix, true);
        let Some(control_at) = full.at(md.control) else {
            continue;
        };

        // Registers of the block clocked by this control
        let mut reg_data: Option<i64> = None;
        let mut reg_clock: Option<i64> = None;
        for cell in netlist
            .cells
            .iter()
            .filter(|c| c.clock == Some(md.control) && c.path.starts_with(&md.prefix))
        {
            let clk = control_at + delays.clock_wire_ps(netlist, md.control, cell);
            reg_clock = Some(reg_clock.map_or(clk, |c: i64| c.min(clk)));
            for (i, &input) in cell.inputs.iter().enumerate() {
                if let Some(t) = data_only.at(input) {
                    let t = t + delays.wire_ps(netlist, input, cell, i) + delays.setup_ps(cell, i);
                    reg_data = Some(reg_data.map_or(t, |d: i64| d.max(t)));
                }
            }
        }
        if let (Some(data_ps), Some(control_ps)) = (reg_data, reg_clock) {
            checks.push(BundleCheck {
                prefix: md.prefix.clone(),
                delay_path: and_cell.path.clone(),
                times: "registers",
                data_ps,
                control_ps,
                chain_len: md.chain.len(),
                sender: None,
            });
        }

        // The acknowledge: the block output this control drives (directly or
        // through plain buffers). Matched delays that feed another matched
        // delay time registers only. In a self-timed block (stream ports)
        // the step's `done` (the matched delay after `go`) plays that part:
        // it ends the step, and the next one can start right after.
        let self_timed_done =
            controls.contains(&md.source) && step_control_ready(netlist, &md.prefix).is_some();
        let ack = match block_output_driven_by(netlist, md.control, &md.prefix) {
            Some(ack) => ack,
            None if self_timed_done => md.control,
            None => continue,
        };
        let mut data_ps: Option<i64> = None;
        for (id, net) in netlist.nets.iter().enumerate() {
            let id = GateNetId(id as u32);
            if id == ack || controls.contains(&id) || !is_block_output(netlist, net, &md.prefix) {
                continue;
            }
            if let Some(t) = full.at(id) {
                data_ps = Some(data_ps.map_or(t, |d: i64| d.max(t)));
            }
        }
        let Some(control_ps) = full.at(ack) else {
            continue;
        };
        if let Some(data_ps) = data_ps {
            checks.push(BundleCheck {
                prefix: md.prefix.clone(),
                delay_path: and_cell.path.clone(),
                times: "acknowledge",
                data_ps,
                control_ps,
                chain_len: md.chain.len(),
                sender: None,
            });
        }

        // The next state: registers of the block launch new values when
        // their control rises, and those values feed the logic in front of
        // registers again. The next control edge comes only after the
        // parent has seen the acknowledge, dropped the request and raised it
        // again, so every such path must settle (with setup) before the
        // acknowledge rises.
        let mut next_ps: Option<i64> = None;
        let mut worst: Option<(&Cell, GateNetId)> = None;
        let mut endpoints = 0usize;
        for cell in netlist.cells.iter().filter(|c| {
            c.path.starts_with(&md.prefix) && c.clock.is_some_and(|k| controls.contains(&k))
        }) {
            endpoints += 1;
            for (i, &input) in cell.inputs.iter().enumerate() {
                if let Some(t) = full.at(input) {
                    let t = t + delays.wire_ps(netlist, input, cell, i) + delays.setup_ps(cell, i);
                    if next_ps.is_none_or(|d| t > d) {
                        worst = Some((cell, input));
                    }
                    next_ps = Some(next_ps.map_or(t, |d: i64| d.max(t)));
                }
            }
        }
        // A self-timed block decides on its next step from `ready` as soon
        // as `done` has ended this one: `ready`, computed from the new
        // state, must have settled by then
        if self_timed_done {
            if let Some((cell, input)) = step_control_ready(netlist, &md.prefix) {
                if let Some(t) = full.at(input) {
                    let t = t + delays.wire_ps(netlist, input, cell, 0);
                    if next_ps.is_none_or(|d| t > d) {
                        worst = Some((cell, input));
                    }
                    next_ps = Some(next_ps.map_or(t, |d: i64| d.max(t)));
                    endpoints += 1;
                }
            }
        }
        if full.loop_cuts > 0 || data_only.loop_cuts > 0 {
            tracing::warn!(
                "NCL bundle {}: {} combinational loop(s) met inside the block; paths through them are not timed",
                md.prefix.trim_end_matches('.'),
                full.loop_cuts.max(data_only.loop_cuts)
            );
        }
        if let Some((cell, input)) = worst {
            if tracing::enabled!(tracing::Level::DEBUG) {
                let path: Vec<String> = full
                    .path(input)
                    .into_iter()
                    .map(|(n, t)| {
                        let net = &netlist.nets[n.0 as usize];
                        let driver = net
                            .driver
                            .map_or("-", |d| netlist.cells[d.0 as usize].path.as_str());
                        format!("{} ({}) @ {:?} ps", net.name, driver, t)
                    })
                    .collect();
                tracing::debug!(
                    "NCL bundle {}: {} registers on its strobes; latest next state into {}:\n  {}",
                    md.prefix.trim_end_matches('.'),
                    endpoints,
                    cell.path,
                    path.join("\n  ")
                );
            }
        }
        if let Some(data_ps) = next_ps {
            checks.push(BundleCheck {
                prefix: md.prefix.clone(),
                delay_path: and_cell.path.clone(),
                times: "next state",
                data_ps,
                control_ps,
                chain_len: md.chain.len(),
                sender: None,
            });
        }
    }
    checks.extend(check_channels(netlist, delays, &matched));
    checks.extend(check_arbiters(netlist, delays, &matched));
    checks
}

/// Arbiters of self-timed blocks: the strobe `go` waits a matched delay
/// after the step request, which comes through the block's arbiters; that
/// wait must cover their resolution time ([`ArbiterTiming`])
fn check_arbiters(
    netlist: &GateNetlist,
    delays: &dyn BundleDelays,
    matched: &[MatchedDelay],
) -> Vec<BundleCheck> {
    let timing = delays.arbiter_timing();
    let required = timing.resolution_ps();
    let mut checks = Vec::new();
    for md in matched {
        // `go`'s delay: its source is the step request, driven by the step
        // control gates
        let from_step = netlist.nets[md.source.0 as usize].driver.is_some_and(|d| {
            netlist.cells[d.0 as usize].source_op.as_deref() == Some(STEP_CONTROL_OP)
        });
        let arbitrates = netlist
            .cells
            .iter()
            .any(|c| c.source_op.as_deref() == Some(ARBITER_OP) && c.path.starts_with(&md.prefix));
        if !from_step || !arbitrates {
            continue;
        }
        // Separate walks: the step loop (go feeds back into the step
        // request) leaves whichever net a walk meets second cut
        let request = Arrivals::new(netlist, delays, &md.prefix, true).at(md.source);
        let go = Arrivals::new(netlist, delays, &md.prefix, true).at(md.control);
        let (Some(request), Some(go)) = (request, go) else {
            continue;
        };
        let wait = go - request;
        tracing::debug!(
            "NCL arbiter {}: resolution wait {} ps (needs {} ps), MTBF {:.3e} years",
            md.prefix.trim_end_matches('.'),
            wait,
            required,
            timing.mtbf_years_after(wait)
        );
        checks.push(BundleCheck {
            prefix: md.prefix.clone(),
            delay_path: netlist.cells[md.and_cell.0 as usize].path.clone(),
            times: "arbiter",
            data_ps: required,
            control_ps: wait,
            chain_len: md.chain.len(),
            sender: None,
        });
    }
    checks
}

/// Channels into self-timed blocks (stream ports, see
/// `skalp_frontend::ncl_streams`). A sending block launches the value and
/// toggles its phase bit with the same strobe; the receiving block steps
/// once its `ready` sees the phase bit, a matched delay later. Every path
/// from the sender's registers into the receiver's registers, data and the
/// arbiter's grant alike, must settle before that step's strobe reaches
/// them. Times count from the sender's strobe at its registers.
fn check_channels(
    netlist: &GateNetlist,
    delays: &dyn BundleDelays,
    matched: &[MatchedDelay],
) -> Vec<BundleCheck> {
    let mut checks = Vec::new();
    let control_prefix: HashMap<GateNetId, &str> = matched
        .iter()
        .map(|m| (m.control, m.prefix.as_str()))
        .collect();
    for md in matched {
        // The `go` delay of a self-timed block: fed by its step control
        let fed_by_step = netlist.nets[md.source.0 as usize].driver.is_some_and(|d| {
            netlist.cells[d.0 as usize].source_op.as_deref() == Some(STEP_CONTROL_OP)
        });
        if !fed_by_step {
            continue;
        }
        let inside = |cell: &Cell| cell.path.starts_with(&md.prefix);
        // Registers of other blocks' strobes that feed this block, grouped
        // by the strobe (one group per sending block)
        let mut senders: Vec<(GateNetId, HashMap<GateNetId, i64>)> = Vec::new();
        for (id, net) in netlist.nets.iter().enumerate() {
            let Some(driver) = net.driver else { continue };
            let cell = &netlist.cells[driver.0 as usize];
            let Some(clock) = cell.clock else { continue };
            if inside(cell)
                || control_prefix.get(&clock).is_none_or(|p| *p == md.prefix)
                || !delays.launches(cell, net.driver_pin.unwrap_or(0))
                || !net
                    .fanout
                    .iter()
                    .any(|&(c, _)| inside(&netlist.cells[c.0 as usize]))
            {
                continue;
            }
            let t = delays.clock_wire_ps(netlist, clock, cell) + delays.clock_to_out_ps(cell);
            match senders.iter_mut().find(|(c, _)| *c == clock) {
                Some((_, nets)) => {
                    nets.insert(GateNetId(id as u32), t);
                }
                None => senders.push((clock, HashMap::from([(GateNetId(id as u32), t)]))),
            }
        }
        for (clock, sources) in senders {
            let mut data =
                Arrivals::new(netlist, delays, &md.prefix, false).with_sources(sources.clone());
            let mut control =
                Arrivals::new(netlist, delays, &md.prefix, false).with_sources(sources);
            let Some(control_at) = control.at(md.control) else {
                // The sender's phase bit does not reach this strobe: not a
                // channel into it
                continue;
            };
            let mut data_ps: Option<i64> = None;
            let mut clock_ps: Option<i64> = None;
            for cell in netlist
                .cells
                .iter()
                .filter(|c| c.clock == Some(md.control) && inside(c))
            {
                let clk = control_at + delays.clock_wire_ps(netlist, md.control, cell);
                clock_ps = Some(clock_ps.map_or(clk, |c: i64| c.min(clk)));
                for (i, &input) in cell.inputs.iter().enumerate() {
                    if let Some(t) = data.at(input) {
                        let t =
                            t + delays.wire_ps(netlist, input, cell, i) + delays.setup_ps(cell, i);
                        data_ps = Some(data_ps.map_or(t, |d: i64| d.max(t)));
                    }
                }
            }
            if let (Some(data_ps), Some(control_ps)) = (data_ps, clock_ps) {
                checks.push(BundleCheck {
                    prefix: md.prefix.clone(),
                    delay_path: netlist.cells[md.and_cell.0 as usize].path.clone(),
                    times: "channel",
                    data_ps,
                    control_ps,
                    chain_len: md.chain.len(),
                    sender: control_prefix.get(&clock).map(|p| p.to_string()),
                });
            }
        }
    }
    checks
}

/// The step control gate of the self-timed block `prefix` that reads its
/// `ready` signal, with that input's net (see `LirOp::NclStepControl`)
fn step_control_ready<'a>(netlist: &'a GateNetlist, prefix: &str) -> Option<(&'a Cell, GateNetId)> {
    netlist
        .cells
        .iter()
        .find(|c| {
            c.source_op.as_deref() == Some(STEP_CONTROL_OP)
                && c.path.starts_with(prefix)
                && c.path.ends_with(".st_or")
        })
        .and_then(|c| c.inputs.first().map(|&n| (c, n)))
}

/// Whether `net` is driven inside the block and read outside it (or is a
/// design output)
fn is_block_output(netlist: &GateNetlist, net: &GateNet, prefix: &str) -> bool {
    let Some(driver) = net.driver else {
        return false;
    };
    if !netlist.cells[driver.0 as usize].path.starts_with(prefix) {
        return false;
    }
    net.is_output
        || net
            .fanout
            .iter()
            .any(|(c, _)| !netlist.cells[c.0 as usize].path.starts_with(prefix))
}

/// The block output `control` reaches through plain single-input buffers
fn block_output_driven_by(
    netlist: &GateNetlist,
    control: GateNetId,
    prefix: &str,
) -> Option<GateNetId> {
    let mut net = control;
    for _ in 0..16 {
        let n = &netlist.nets[net.0 as usize];
        if is_block_output(netlist, n, prefix) {
            return Some(net);
        }
        // Follow a lone buffer
        let mut next = None;
        for &(c, _) in &n.fanout {
            let cell = &netlist.cells[c.0 as usize];
            if cell.function == Some(CellFunction::Buf)
                && cell.inputs.len() == 1
                && !cell.is_kept()
                && cell.clock.is_none()
            {
                next = cell.outputs.first().copied();
            }
        }
        net = next?;
    }
    None
}

/// First read-address input of a `CellFunction::DistRam16x4` cell
const DIST_RAM_READ_ADDRESS: usize = 9;

/// Library-based delay estimates for timing matched delays before placement.
///
/// Cell delays are the library's timing arcs; every connection gets a fixed
/// routing estimate. Both sides of a check (the data paths and the delay
/// chain) are estimated the same way; routed delays are checked after
/// place and route.
pub struct LibraryDelays<'a> {
    pub library: &'a TechLibrary,
    /// Routing estimate per connection, in picoseconds
    pub wire_ps: i64,
    /// Cell delay when the library has no arc for a cell, in picoseconds
    pub default_cell_ps: i64,
}

impl<'a> LibraryDelays<'a> {
    /// Estimates for `library` with the default routing estimate
    pub fn new(library: &'a TechLibrary) -> Self {
        Self {
            library,
            wire_ps: 500,
            default_cell_ps: 400,
        }
    }

    fn arc(&self, cell: &Cell, input: usize, output: usize) -> i64 {
        let Some(lib_cell) = self.library.get_cell(&cell.cell_type) else {
            return self.default_cell_ps;
        };
        let Some(timing) = lib_cell.timing.as_ref() else {
            return self.default_cell_ps;
        };
        let worst = |arc: &crate::tech_library::TimingArc| {
            arc.rise_delay_ps(0.0).max(arc.fall_delay_ps(0.0)).round() as i64
        };
        if let (Some(i), Some(o)) = (lib_cell.inputs.get(input), lib_cell.outputs.get(output)) {
            if let Some(arc) = timing.arcs.get(&format!("{}->{}", i, o)) {
                return worst(arc);
            }
        }
        timing
            .arcs
            .values()
            .map(worst)
            .max()
            .unwrap_or(self.default_cell_ps)
    }
}

impl BundleDelays for LibraryDelays<'_> {
    fn wire_ps(&self, _: &GateNetlist, _: GateNetId, _: &Cell, _: usize) -> i64 {
        self.wire_ps
    }
    fn clock_wire_ps(&self, _: &GateNetlist, _: GateNetId, _: &Cell) -> i64 {
        self.wire_ps
    }
    fn arc_ps(&self, cell: &Cell, input: usize, output: usize) -> Option<i64> {
        // A distributed RAM reads through its read address (RAD0..RAD3,
        // after DI0..DI3, WAD0..WAD3 and WRE); its write side only reaches
        // the outputs through the clock.
        if cell.function == Some(CellFunction::DistRam16x4) && input < DIST_RAM_READ_ADDRESS {
            return None;
        }
        Some(self.arc(cell, input, output))
    }
    fn clock_to_out_ps(&self, cell: &Cell) -> i64 {
        self.arc(cell, 0, 0)
    }
    fn launches(&self, cell: &Cell, _output: usize) -> bool {
        // A distributed RAM's read port is combinational from its address
        cell.clock.is_some() && cell.function != Some(CellFunction::DistRam16x4)
    }
    fn setup_ps(&self, cell: &Cell, _input: usize) -> i64 {
        self.library
            .get_cell(&cell.cell_type)
            .and_then(|c| c.setup_ps)
            .map_or(200, i64::from)
    }
}

/// Add `count` stages to a matched delay's chain, just before its AND. Each
/// stage is an AND of the delayed source and the stage before (a copy of the
/// final AND cell), so the chain still clears at once when the source falls.
pub fn extend_matched_delay(netlist: &mut GateNetlist, md: &MatchedDelay, count: usize) {
    if count == 0 {
        return;
    }
    let Some(&last) = md.chain.last() else {
        return;
    };
    let and_idx = md.and_cell.0 as usize;
    let template = netlist.cells[and_idx].clone();
    let mut net = template.inputs[1];
    let last_path = &netlist.cells[last.0 as usize].path;
    let base = last_path
        .rsplit_once('_')
        .map_or(last_path.as_str(), |(b, _)| b)
        .to_string();
    let first = md.chain.len();
    for k in 0..count {
        let out = netlist.add_net(GateNet::new(
            GateNetId(0),
            format!("{}_n{}", base, first + k),
        ));
        let mut stage = template.clone();
        stage.id = CellId(0);
        stage.path = format!("{}_{}", base, first + k);
        stage.inputs = vec![md.source, net];
        stage.outputs = vec![out];
        netlist.add_cell(stage);
        net = out;
    }
    netlist.cells[and_idx].inputs[1] = net;
    netlist.rebuild_net_connectivity();
}

/// Result of sizing the matched delays of a netlist
#[derive(Debug, Clone, Default)]
pub struct SizingReport {
    /// Buffers added in total
    pub buffers_added: usize,
    /// Final checks, one per timed control
    pub checks: Vec<BundleCheck>,
}

/// Lengthen every matched delay until its checks pass with `margin_percent`
/// extra delay over the data paths it times, under `delays`
pub fn size_matched_delays(
    netlist: &mut GateNetlist,
    delays: &dyn BundleDelays,
    margin_percent: i64,
) -> SizingReport {
    let mut report = SizingReport::default();
    // Shortfall of each chain in the previous round: a chain whose
    // shortfall does not shrink when it is lengthened times a path that
    // goes through its own control, which no length can meet
    let mut previous: HashMap<String, i64> = HashMap::new();
    let mut stuck: HashSet<String> = HashSet::new();
    // Each round fixes the delays it can see; later delays depend on earlier
    // ones (a register-timing delay feeds the acknowledge delay), so repeat.
    for _ in 0..64 {
        let checks = check_matched_delays(netlist, delays);
        let matched = find_matched_delays(netlist);
        let mut changed = false;
        for md in &matched {
            let path = &netlist.cells[md.and_cell.0 as usize].path;
            let worst = checks
                .iter()
                .filter(|c| &c.delay_path == path)
                .map(|c| {
                    let required = c.data_ps + c.data_ps.abs() * margin_percent / 100;
                    required - c.control_ps
                })
                .max()
                .unwrap_or(0);
            if worst <= 0 || stuck.contains(path) {
                continue;
            }
            if previous.get(path).is_some_and(|&p| worst >= p) {
                tracing::warn!(
                    "NCL matched delay {}: still {} ps short after lengthening; the data it times depends on the delay itself, so it is left as it is",
                    path,
                    worst
                );
                stuck.insert(path.clone());
                continue;
            }
            previous.insert(path.clone(), worst);
            if tracing::enabled!(tracing::Level::DEBUG) {
                for c in checks.iter().filter(|c| &c.delay_path == path) {
                    tracing::debug!(
                        "NCL sizing {}: {} data {} ps, control {} ps, {} stages",
                        path,
                        c.times,
                        c.data_ps,
                        c.control_ps,
                        c.chain_len
                    );
                }
            }
            // Per buffer: one cell delay plus one connection
            let per_buffer = md
                .chain
                .first()
                .map(|&id| {
                    let cell = &netlist.cells[id.0 as usize];
                    delays.arc_ps(cell, 0, 0).unwrap_or(0)
                        + delays.wire_ps(netlist, cell.inputs[0], cell, 0)
                })
                .unwrap_or(1)
                .max(1);
            let count = ((worst + per_buffer - 1) / per_buffer) as usize;
            extend_matched_delay(netlist, md, count);
            report.buffers_added += count;
            changed = true;
        }
        if !changed {
            report.checks = checks;
            break;
        }
    }
    if report.checks.is_empty() {
        report.checks = check_matched_delays(netlist, delays);
    }
    report
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Every cell 100 ps, connections free, setup free
    struct UnitDelays;

    impl BundleDelays for UnitDelays {
        fn wire_ps(&self, _: &GateNetlist, _: GateNetId, _: &Cell, _: usize) -> i64 {
            0
        }
        fn clock_wire_ps(&self, _: &GateNetlist, _: GateNetId, _: &Cell) -> i64 {
            0
        }
        fn arc_ps(&self, _: &Cell, _: usize, _: usize) -> Option<i64> {
            Some(100)
        }
        fn clock_to_out_ps(&self, _: &Cell) -> i64 {
            100
        }
        fn setup_ps(&self, _: &Cell, _: usize) -> i64 {
            0
        }
    }

    fn comb(
        netlist: &mut GateNetlist,
        path: &str,
        inputs: Vec<GateNetId>,
        output: GateNetId,
        kept: bool,
    ) {
        let mut cell = Cell::new_comb(
            CellId(0),
            "LUT4".into(),
            "test".into(),
            0.0,
            path.into(),
            inputs,
            vec![output],
        );
        if kept {
            cell.source_op = Some(MATCHED_DELAY_OP.to_string());
        }
        netlist.add_cell(cell);
    }

    /// A matched delay `out = from & buf(from)` named `name` in block `top.u.`
    fn matched_delay(netlist: &mut GateNetlist, name: &str, from: GateNetId, out: GateNetId) {
        let chained = netlist.add_net(GateNet::new(GateNetId(0), format!("top.u.{name}_d0")));
        comb(
            netlist,
            &format!("top.u.ncl_bundle.{name}_0"),
            vec![from],
            chained,
            true,
        );
        comb(
            netlist,
            &format!("top.u.ncl_bundle.{name}_and"),
            vec![from, chained],
            out,
            true,
        );
    }

    /// An async block whose register feeds itself through five gates: the
    /// next state settles 500 ps after the register launches, so the
    /// acknowledge must wait for it.
    fn block_with_feedback() -> GateNetlist {
        let mut netlist = GateNetlist::new("top".into(), "test".into());
        let req = netlist.add_input("req".into());
        let ack = netlist.add_output("ack".into());
        let go = netlist.add_net(GateNet::new(GateNetId(0), "top.u.__ncl_go".into()));
        matched_delay(&mut netlist, "go", req, go);
        matched_delay(&mut netlist, "done", go, ack);

        let q = netlist.add_net(GateNet::new(GateNetId(0), "top.u.q".into()));
        let mut net = q;
        for k in 0..5 {
            let next = netlist.add_net(GateNet::new(GateNetId(0), format!("top.u.n{k}")));
            comb(
                &mut netlist,
                &format!("top.u.inc{k}"),
                vec![net],
                next,
                false,
            );
            net = next;
        }
        netlist.add_cell(Cell::new_seq(
            CellId(0),
            "FD1P3IX".into(),
            "test".into(),
            0.0,
            "top.u.state".into(),
            vec![net],
            vec![q],
            go,
            None,
        ));
        netlist.rebuild_net_connectivity();
        netlist
    }

    #[test]
    fn next_state_is_timed_against_the_acknowledge() {
        let netlist = block_with_feedback();
        let checks = check_matched_delays(&netlist, &UnitDelays);
        let next = checks
            .iter()
            .find(|c| c.times == "next state")
            .expect("a next-state check");
        // go at 200, done at 400; the register launches at 300 and its next
        // value settles five gates later
        assert_eq!(next.control_ps, 400);
        assert_eq!(next.data_ps, 800);
        assert_eq!(next.slack_ps(), -400);
        assert!(next.delay_path.ends_with("done_and"));
    }

    /// A distributed RAM written by the strobe: its read port is
    /// combinational from the address, so a read through it times from the
    /// address register, not from the RAM's write clock.
    #[test]
    fn ram_reads_are_timed_from_their_address() {
        struct RamDelays;
        impl BundleDelays for RamDelays {
            fn wire_ps(&self, _: &GateNetlist, _: GateNetId, _: &Cell, _: usize) -> i64 {
                0
            }
            fn clock_wire_ps(&self, _: &GateNetlist, _: GateNetId, _: &Cell) -> i64 {
                0
            }
            fn arc_ps(&self, _: &Cell, _: usize, _: usize) -> Option<i64> {
                Some(100)
            }
            fn clock_to_out_ps(&self, _: &Cell) -> i64 {
                100
            }
            fn setup_ps(&self, _: &Cell, _: usize) -> i64 {
                0
            }
            fn launches(&self, cell: &Cell, _: usize) -> bool {
                cell.clock.is_some() && cell.function != Some(CellFunction::DistRam16x4)
            }
        }

        let mut netlist = GateNetlist::new("top".into(), "test".into());
        let req = netlist.add_input("req".into());
        let ack = netlist.add_output("ack".into());
        let go = netlist.add_net(GateNet::new(GateNetId(0), "top.u.__ncl_go".into()));
        matched_delay(&mut netlist, "go", req, go);
        matched_delay(&mut netlist, "done", go, ack);
        // address register -> three gates -> RAM read -> data register
        let addr_q = netlist.add_net(GateNet::new(GateNetId(0), "top.u.addr".into()));
        let mut net = addr_q;
        for k in 0..3 {
            let next = netlist.add_net(GateNet::new(GateNetId(0), format!("top.u.a{k}")));
            comb(
                &mut netlist,
                &format!("top.u.adec{k}"),
                vec![net],
                next,
                false,
            );
            net = next;
        }
        let rd = netlist.add_net(GateNet::new(GateNetId(0), "top.u.rd".into()));
        let mut ram = Cell::new_seq(
            CellId(0),
            "DPR16X4".into(),
            "test".into(),
            0.0,
            "top.u.ram".into(),
            vec![net],
            vec![rd],
            go,
            None,
        );
        ram.function = Some(CellFunction::DistRam16x4);
        netlist.add_cell(ram);
        let data_q = netlist.add_net(GateNet::new(GateNetId(0), "top.u.data".into()));
        for (path, d, q) in [
            ("top.u.addr_r", data_q, addr_q),
            ("top.u.data_r", rd, data_q),
        ] {
            netlist.add_cell(Cell::new_seq(
                CellId(0),
                "FD1P3IX".into(),
                "test".into(),
                0.0,
                path.into(),
                vec![d],
                vec![q],
                go,
                None,
            ));
        }
        netlist.rebuild_net_connectivity();

        let checks = check_matched_delays(&netlist, &RamDelays);
        let next = checks.iter().find(|c| c.times == "next state").unwrap();
        // go 200, address register out 300, three gates 600, read 700
        assert_eq!(next.data_ps, 700);
    }

    #[test]
    fn sizing_lengthens_the_acknowledge_for_the_next_state() {
        let mut netlist = block_with_feedback();
        let report = size_matched_delays(&mut netlist, &UnitDelays, 0);
        assert_eq!(report.buffers_added, 4);
        assert!(
            report.checks.iter().all(|c| c.slack_ps() >= 0),
            "{:?}",
            report.checks
        );
    }

    /// Stages added to a chain are ANDs with the source, so a falling source
    /// clears every stage at once, and the chain is still found afterwards
    #[test]
    fn extended_chains_reset_fast() {
        let mut netlist = block_with_feedback();
        let before = find_matched_delays(&netlist);
        let done = before
            .iter()
            .find(|md| netlist.cells[md.and_cell.0 as usize].path.contains("done"))
            .unwrap()
            .clone();
        extend_matched_delay(&mut netlist, &done, 3);
        let after = find_matched_delays(&netlist);
        let done = after
            .iter()
            .find(|md| md.and_cell == done.and_cell)
            .expect("chain still found after extending");
        assert_eq!(done.chain.len(), 4);
        for &id in &done.chain[1..] {
            let stage = &netlist.cells[id.0 as usize];
            assert_eq!(stage.inputs.len(), 2, "{}", stage.path);
            assert_eq!(stage.inputs[0], done.source, "{}", stage.path);
        }
        // each stage takes the one before it
        for pair in done.chain.windows(2) {
            let prev = &netlist.cells[pair[0].0 as usize];
            let next = &netlist.cells[pair[1].0 as usize];
            assert_eq!(next.inputs[1], prev.outputs[0]);
        }
    }

    /// A self-timed block whose step request comes through an arbiter
    /// gate: `go` waits one buffer (100 ps) after it
    fn arbitrating_block() -> GateNetlist {
        let mut netlist = GateNetlist::new("top".into(), "test".into());
        let req = netlist.add_input("req".into());
        let ack = netlist.add_output("ack".into());
        let grant = netlist.add_net(GateNet::new(GateNetId(0), "top.u.gnt".into()));
        let mut arb = Cell::new_comb(
            CellId(0),
            "LUT4".into(),
            "test".into(),
            0.0,
            "top.u.ncl_bundle.arb_nand0".into(),
            vec![req],
            vec![grant],
        );
        arb.source_op = Some(ARBITER_OP.to_string());
        netlist.add_cell(arb);
        let step = netlist.add_net(GateNet::new(GateNetId(0), "top.u.__ncl_step_req".into()));
        // `go` feeds back into the step request, as in the real step control
        let go = netlist.add_net(GateNet::new(GateNetId(0), "top.u.__ncl_go".into()));
        let mut st = Cell::new_comb(
            CellId(0),
            "LUT4".into(),
            "test".into(),
            0.0,
            "top.u.ncl_bundle.step.st_and".into(),
            vec![grant, go],
            vec![step],
        );
        st.source_op = Some(STEP_CONTROL_OP.to_string());
        netlist.add_cell(st);
        matched_delay(&mut netlist, "go", step, go);
        matched_delay(&mut netlist, "done", go, ack);
        netlist.rebuild_net_connectivity();
        netlist
    }

    #[test]
    fn arbiter_timing_meets_its_mtbf() {
        let timing = ArbiterTiming::default();
        let wait = timing.resolution_ps();
        // tau * ln(MTBF * T0 * f^2): about 2.5 ns with the defaults
        assert!((2400..2600).contains(&wait), "{wait}");
        assert!(timing.mtbf_years_after(wait) >= timing.mtbf_years);
        assert!(timing.mtbf_years_after(wait - 100) < timing.mtbf_years);
        // ten tau more wait: e^10 times the MTBF
        let ratio = timing.mtbf_years_after(wait + 600) / timing.mtbf_years_after(wait);
        assert!((ratio / 10f64.exp() - 1.0).abs() < 1e-6, "{ratio}");
    }

    #[test]
    fn arbiter_wait_is_checked_and_sized() {
        let mut netlist = arbitrating_block();
        let checks = check_matched_delays(&netlist, &UnitDelays);
        let arb = checks
            .iter()
            .find(|c| c.times == "arbiter")
            .expect("an arbiter check");
        assert!(arb.delay_path.ends_with("go_and"));
        // buffer + AND after the step request
        assert_eq!(arb.control_ps, 200);
        assert_eq!(arb.data_ps, ArbiterTiming::default().resolution_ps());
        assert!(arb.slack_ps() < 0);

        let report = size_matched_delays(&mut netlist, &UnitDelays, 0);
        let arb = report
            .checks
            .iter()
            .find(|c| c.times == "arbiter")
            .expect("an arbiter check after sizing");
        assert!(arb.slack_ps() >= 0, "{arb:?}");
        // a block without an arbiter gets no such check
        let plain = check_matched_delays(&block_with_feedback(), &UnitDelays);
        assert!(plain.iter().all(|c| c.times != "arbiter"));
    }
}
