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
//! A self-timed block decides to step in a latch set by two inputs that
//! change independently of each other, its run request and its `ready`; a
//! block that arbitrates between streams (`match arbitrate`) also has a
//! mutual-exclusion element in front of `ready`. When two inputs change at
//! nearly the same time such an element can take a while to decide
//! (metastability); the chance that it is still undecided after a wait `t`
//! falls as `exp(-t / tau)`. The step strobe `go` only rises once the step
//! request has stayed high for the whole matched delay, so that delay is
//! the resolution wait. The "arbiter" check (or "step decision", for a
//! block without arbiters) makes it at least the wait a target mean time
//! between failures needs ([`ArbiterTiming`]).
//!
//! Channels between self-timed blocks are checked both ways: the value
//! must settle before the receiver's strobe takes it ("channel"), and the
//! sender's next value, which it only launches after the receiver's phase
//! bit has told it the value was taken, must not reach the receiver's
//! registers before that strobe has ("channel hold").
//!
//! All times are measured from the block's inputs, which the clocked parent
//! holds stable from before it raises the request; the request itself is
//! one of those inputs.

use crate::gate_netlist::{
    Cell, CellId, GateNet, GateNetId, GateNetlist, ARBITER_OP, LATCH_D_PIN, LATCH_EN_PIN,
    LATCH_LUT_INIT, LATCH_OP, LATCH_Q_PIN, MATCHED_DELAY_OP, STEP_CONTROL_OP, STEP_LATCH_READY_PIN,
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
    /// Hold time of a register's data input `input`
    fn hold_ps(&self, _cell: &Cell, _input: usize) -> i64 {
        0
    }
    /// Metastability of the arbiters' mutual-exclusion elements, and the
    /// failure rate they must meet
    fn arbiter_timing(&self) -> ArbiterTiming {
        ArbiterTiming::default()
    }
    /// Metastability figures for a self-timed block's step latch, where the
    /// device data has them; otherwise those of an arbiter
    fn step_latch_timing(&self) -> ArbiterTiming {
        self.arbiter_timing()
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
    /// Whether `tau_ps` and `t0_ps` were measured on the device family
    /// (from its device database). Without a measurement they are
    /// [`UNMEASURED_TAU_PS`] and a 1 ns window: a wait sized from them is
    /// safe by a wide margin but not signed off
    pub measured: bool,
}

/// Resolution time constant assumed for a latch in one LUT with its own
/// feedback on a family with no measured figure, in picoseconds. The one
/// family measured so far gave at most 30 ps in its fast-corner delay
/// units, 90 ps with its process, voltage and temperature margin; this is
/// ten times the measured bound, to cover a slower process or a LUT that
/// resolves worse. A loop of several cells resolves far slower (about
/// 0.7-1 ns where measured), which is why these elements are built in one
/// LUT.
pub const UNMEASURED_TAU_PS: f64 = 300.0;

impl Default for ArbiterTiming {
    /// Figures for a family without measured device data: tau
    /// [`UNMEASURED_TAU_PS`], T0 1 ns, requests at up to 200 MHz, 1000
    /// years between failures
    fn default() -> Self {
        Self {
            tau_ps: UNMEASURED_TAU_PS,
            t0_ps: 1000.0,
            request_rate_hz: 200e6,
            mtbf_years: 1000.0,
            measured: false,
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

    /// Figures under which [`Self::resolution_ps`] is `wait_ps`: for a
    /// measurement build that cuts the wait short on purpose to see the
    /// element fail. Not measured, so never signed off.
    pub fn sized_for_wait(self, wait_ps: i64) -> Self {
        let x = self.mtbf_years * SECONDS_PER_YEAR * self.base_rate();
        Self {
            tau_ps: wait_ps.max(1) as f64 / x.max(std::f64::consts::E).ln(),
            measured: false,
            ..self
        }
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
    /// "channel", "channel hold", "arbiter", "step decision", "release"
    /// or "grant settle"
    pub times: &'static str,
    /// Latest data arrival plus setup, in picoseconds. For an "arbiter" or
    /// "step decision" check, the resolution wait the decision needs; for a
    /// "channel hold" check, until when the receiver's registers must hold
    /// (their strobe's arrival plus hold).
    pub data_ps: i64,
    /// Arrival of the control signal, in picoseconds. For an "arbiter" or
    /// "step decision" check, how long the step request stays high before
    /// `go`; for a "channel hold" check, the earliest the sender's next
    /// value reaches the receiver's registers.
    pub control_ps: i64,
    /// Buffers in the chain
    pub chain_len: usize,
    /// For a "channel" or "channel hold" check, the block that sends (its
    /// prefix). A "channel" check times data and phase bit from the
    /// sender's own strobe; a "channel hold" check is reported against the
    /// sender's `go` delay.
    pub sender: Option<String>,
    /// For an "arbiter" or "step decision" check, whether its resolution
    /// wait was sized from assumed figures, the device family having no
    /// measured settle time ([`ArbiterTiming::measured`]): such a wait is
    /// generous but not signed off
    pub unmeasured_settle: bool,
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

/// Path of the matched delay that reopens a block's master latches after
/// its step strobe falls (see `ncl_expand::expand_to_ncl_bridged_with`)
pub const MASTER_OPEN_PATH: &str = "ncl_bundle.master_open";
/// Path of the matched delay that opens a block's slave latches after its
/// step strobe rises (see `ncl_expand::expand_to_ncl_bridged_with`)
pub const SLAVE_OPEN_PATH: &str = "ncl_bundle.slave_open";

/// One bit held by a latch built from logic (`LATCH_OP`, see
/// `tech_mapper::map_latch_bit`): a library latch cell, one LUT whose
/// output feeds back into it, or kept gates with the consensus term
#[derive(Debug, Clone)]
struct LatchBit {
    en: GateNetId,
    d: GateNetId,
    q: GateNetId,
    cells: Vec<CellId>,
}

/// Every latch bit of `netlist`
fn latch_bits(netlist: &GateNetlist) -> Vec<LatchBit> {
    let mut bits = Vec::new();
    // Gate-built latches, by the latch's path: (en, d, q, cells)
    type Parts = (
        Option<GateNetId>,
        Option<GateNetId>,
        Option<GateNetId>,
        Vec<CellId>,
    );
    let mut gates: HashMap<&str, Parts> = HashMap::new();
    let mut order: Vec<&str> = Vec::new();
    for cell in netlist
        .cells
        .iter()
        .filter(|c| c.source_op.as_deref() == Some(LATCH_OP))
    {
        let (Some(&q), true) = (cell.outputs.first(), cell.outputs.len() == 1) else {
            continue;
        };
        if cell.function == Some(CellFunction::Latch) && cell.inputs.len() == 2 {
            bits.push(LatchBit {
                en: cell.inputs[0],
                d: cell.inputs[1],
                q,
                cells: vec![cell.id],
            });
            continue;
        }
        if cell.lut_init == Some(LATCH_LUT_INIT) && cell.inputs.get(LATCH_Q_PIN) == Some(&q) {
            bits.push(LatchBit {
                en: cell.inputs[LATCH_EN_PIN],
                d: cell.inputs[LATCH_D_PIN],
                q,
                cells: vec![cell.id],
            });
            continue;
        }
        let Some((latch, part)) = cell.path.rsplit_once('.') else {
            continue;
        };
        let entry = gates.entry(latch).or_insert_with(|| {
            order.push(latch);
            (None, None, None, Vec::new())
        });
        entry.3.push(cell.id);
        match part {
            "inv" => entry.0 = cell.inputs.first().copied(),
            "load_and" => entry.1 = cell.inputs.first().copied(),
            "or_b" => entry.2 = Some(q),
            _ => {}
        }
    }
    for latch in order {
        match gates.remove(latch) {
            Some((Some(en), Some(d), Some(q), cells)) => bits.push(LatchBit { en, d, q, cells }),
            _ => tracing::warn!(
                "NCL bundle: latch cells under {} are not a latch the timing checks know; not timed",
                latch
            ),
        }
    }
    bits
}

/// Shortest and longest delay from `from` (an input of the latch) to its
/// output, through the latch's own cells and the connections into them;
/// the feedback from the output is not a path
fn latch_path(
    netlist: &GateNetlist,
    delays: &dyn BundleDelays,
    bit: &LatchBit,
    from: GateNetId,
) -> Option<(i64, i64)> {
    fn walk(
        netlist: &GateNetlist,
        delays: &dyn BundleDelays,
        bit: &LatchBit,
        from: GateNetId,
        net: GateNetId,
        depth: usize,
    ) -> Option<(i64, i64)> {
        if net == from {
            return Some((0, 0));
        }
        let n = &netlist.nets[net.0 as usize];
        let driver = n.driver?;
        if depth > bit.cells.len() || !bit.cells.contains(&driver) {
            return None;
        }
        let cell = &netlist.cells[driver.0 as usize];
        let output = n.driver_pin.unwrap_or(0);
        let mut best: Option<(i64, i64)> = None;
        for (i, &input) in cell.inputs.iter().enumerate() {
            if input == bit.q {
                continue;
            }
            let Some(arc) = delays.arc_ps(cell, i, output) else {
                continue;
            };
            if let Some((lo, hi)) = walk(netlist, delays, bit, from, input, depth + 1) {
                let step = delays.wire_ps(netlist, input, cell, i) + arc;
                best = Some(match best {
                    None => (lo + step, hi + step),
                    Some((l, h)) => (l.min(lo + step), h.max(hi + step)),
                });
            }
        }
        best
    }
    walk(netlist, delays, bit, from, bit.q, 0)
}

/// Shortest and longest connection from `net` into the latch's own cells
fn latch_wires(
    netlist: &GateNetlist,
    delays: &dyn BundleDelays,
    bit: &LatchBit,
    net: GateNetId,
) -> Option<(i64, i64)> {
    netlist.nets[net.0 as usize]
        .fanout
        .iter()
        .filter(|(c, _)| bit.cells.contains(c))
        .map(|&(c, pin)| delays.wire_ps(netlist, net, &netlist.cells[c.0 as usize], pin))
        .fold(None, |acc, w| match acc {
            None => Some((w, w)),
            Some((l, h)) => Some((l.min(w), h.max(w))),
        })
}

/// How long a latch needs its data stable before its enable falls: the new
/// value must have gone through the latch and back to its own input, so
/// that the latch holds it once closed
fn latch_setup(netlist: &GateNetlist, delays: &dyn BundleDelays, bit: &LatchBit) -> Option<i64> {
    let (_, through) = latch_path(netlist, delays, bit, bit.d)?;
    let feedback = latch_wires(netlist, delays, bit, bit.q).map_or(0, |(_, h)| h);
    Some(through + feedback)
}

/// A block whose state is held in latches built from logic (see
/// `ncl_expand::expand_to_ncl_bridged_with`), opened from its step strobe
/// `go`.
///
/// A latch is taken to be closed once its enable's fall has gone through
/// it to its output: from then on its own feedback holds the value
/// whatever the data does (a latch built from logic keeps its consensus
/// term, so the data changing then cannot glitch it). That is its hold
/// time.
struct LatchBlock<'a> {
    prefix: &'a str,
    go: &'a MatchedDelay,
    /// How the latches that take the next state are opened
    opener: Opener<'a>,
    /// The latches that take the next state (masters, or the only latches)
    takers: Vec<LatchBit>,
    /// Slaves of takers that are masters: `slave_open = go & delay(go)`
    /// opens them once every master has closed, and they close as `go`
    /// falls
    slaves: Option<(&'a MatchedDelay, Vec<LatchBit>)>,
    /// Pulsed takers without a slave: they launch their new value as the
    /// pulse opens them
    singles: Vec<LatchBit>,
}

/// How a latch block opens the latches that take its next state
enum Opener<'a> {
    /// Open while the block waits: `master_open = !go & delay(!go)` falls
    /// as `go` rises and rises a matched delay after `go` falls
    Transparent { master_open: &'a MatchedDelay },
    /// Open only for `pulse = go & !pulse_end` as `go` rises, with
    /// `pulse_end = go & delay(go)`
    Pulse {
        pulse_end: &'a MatchedDelay,
        pulse: GateNetId,
        /// The gate that makes the pulse
        gate: CellId,
    },
}

/// The latches of a block that take the next state or launch the new one,
/// with their enable and when it changes after `go` rises (at the enable's
/// own net)
struct LatchGroup<'b> {
    en: GateNetId,
    at: i64,
    bits: &'b [LatchBit],
}

/// Path of the matched delay that ends a pulsed-latch block's pulse (see
/// `ncl_expand::expand_to_ncl_bridged_with`)
pub const PULSE_END_PATH: &str = "ncl_bundle.pulse_end";
/// Path of the gate that makes a pulsed-latch block's pulse
pub const PULSE_PATH: &str = "ncl_bundle.pulse";

/// Every latch block of `netlist`
fn latch_blocks<'a>(netlist: &GateNetlist, matched: &'a [MatchedDelay]) -> Vec<LatchBlock<'a>> {
    let bits = latch_bits(netlist);
    let in_block = |prefix: &str, en: GateNetId| -> Vec<LatchBit> {
        bits.iter()
            .filter(|b| {
                b.en == en
                    && b.cells
                        .first()
                        .is_some_and(|&c| netlist.cells[c.0 as usize].path.starts_with(prefix))
            })
            .cloned()
            .collect()
    };
    let path_of = |m: &MatchedDelay| netlist.cells[m.and_cell.0 as usize].path.as_str();
    // The slaves of the block whose `go` is `go`, if it has any
    let slaves_of = |prefix: &str, go: GateNetId| {
        matched
            .iter()
            .find(|m| m.prefix == prefix && m.source == go && path_of(m).contains(SLAVE_OPEN_PATH))
            .map(|so| (so, in_block(prefix, so.control)))
    };
    let mut blocks = Vec::new();
    for md in matched {
        let prefix = md.prefix.as_str();
        if path_of(md).contains(MASTER_OPEN_PATH) {
            // `master_open`'s source is `!go`
            let go_net = netlist.nets[md.source.0 as usize]
                .driver
                .map(|d| &netlist.cells[d.0 as usize])
                .filter(|c| c.inputs.len() == 1 && c.path.starts_with(prefix))
                .map(|c| c.inputs[0]);
            let go = matched.iter().find(|m| Some(m.control) == go_net);
            let slaves = go.and_then(|go| slaves_of(prefix, go.control));
            let (Some(go), Some(slaves)) = (go, slaves) else {
                tracing::warn!(
                    "NCL bundle {}: master latch enable without its step strobe or slave enable; latches not timed",
                    prefix.trim_end_matches('.')
                );
                continue;
            };
            blocks.push(LatchBlock {
                prefix,
                go,
                opener: Opener::Transparent { master_open: md },
                takers: in_block(prefix, md.control),
                slaves: Some(slaves),
                singles: Vec::new(),
            });
        } else if path_of(md).contains(PULSE_END_PATH) {
            // `pulse_end`'s source is `go`; the pulse gate reads both
            let go = matched.iter().find(|m| m.control == md.source);
            let gate = netlist.nets[md.control.0 as usize]
                .fanout
                .iter()
                .map(|&(c, _)| &netlist.cells[c.0 as usize])
                .find(|c| {
                    // (a LUT may read an input on more than one pin)
                    c.path.starts_with(prefix)
                        && c.inputs.contains(&md.source)
                        && c.inputs.iter().all(|&n| n == md.source || n == md.control)
                        && c.outputs.len() == 1
                });
            let (Some(go), Some(gate)) = (go, gate) else {
                tracing::warn!(
                    "NCL bundle {}: pulse end without its step strobe or pulse gate; latches not timed",
                    prefix.trim_end_matches('.')
                );
                continue;
            };
            let pulse = gate.outputs[0];
            let takers = in_block(prefix, pulse);
            let slaves = slaves_of(prefix, go.control);
            let slave_inputs: HashSet<GateNetId> = slaves
                .iter()
                .flat_map(|(_, s)| s.iter().map(|b| b.d))
                .collect();
            let singles = takers
                .iter()
                .filter(|b| !slave_inputs.contains(&b.q))
                .cloned()
                .collect();
            blocks.push(LatchBlock {
                prefix,
                go,
                opener: Opener::Pulse {
                    pulse_end: md,
                    pulse,
                    gate: gate.id,
                },
                takers,
                slaves,
                singles,
            });
        }
    }
    blocks
}

/// When a matched delay's output falls after its source does (at
/// `source_at`): straight through its AND
fn falls_through(
    netlist: &GateNetlist,
    delays: &dyn BundleDelays,
    md: &MatchedDelay,
    source_at: i64,
) -> Option<i64> {
    through_pin(netlist, delays, md.and_cell, md.source, source_at)
}

/// When `cell`'s output changes after its input `net` does (at `at`):
/// the latest of the pins `net` reaches
fn through_pin(
    netlist: &GateNetlist,
    delays: &dyn BundleDelays,
    cell: CellId,
    net: GateNetId,
    at: i64,
) -> Option<i64> {
    let cell = &netlist.cells[cell.0 as usize];
    cell.inputs
        .iter()
        .enumerate()
        .filter(|&(_, &n)| n == net)
        .filter_map(|(pin, _)| {
            Some(at + delays.wire_ps(netlist, net, cell, pin) + delays.arc_ps(cell, pin, 0)?)
        })
        .max()
}

/// Earliest arrival of `en` at any of `bits`' enable pins, after `at` at
/// its own net
fn opens_at(
    netlist: &GateNetlist,
    delays: &dyn BundleDelays,
    bits: &[LatchBit],
    en: GateNetId,
    at: i64,
) -> Option<i64> {
    bits.iter()
        .filter_map(|b| latch_wires(netlist, delays, b, en).map(|(lo, _)| at + lo))
        .min()
}

/// Latest time any of `bits` has closed (its enable's fall gone through
/// it), the enable falling at `at` at its own net
fn closed_at(
    netlist: &GateNetlist,
    delays: &dyn BundleDelays,
    bits: &[LatchBit],
    en: GateNetId,
    at: i64,
) -> Option<i64> {
    bits.iter()
        .filter_map(|b| latch_path(netlist, delays, b, en).map(|(_, hi)| at + hi))
        .max()
}

impl LatchBlock<'_> {
    /// Times from `go` (at its own net, time 0) to the nets of this block
    fn from_go<'n>(
        &self,
        netlist: &'n GateNetlist,
        delays: &'n dyn BundleDelays,
        early: bool,
    ) -> Arrivals<'n>
    where
        Self: 'n,
    {
        let arrivals = Arrivals::new(netlist, delays, self.prefix, false)
            .with_sources(HashMap::from([(self.go.control, 0)]));
        if early {
            arrivals.earliest()
        } else {
            arrivals
        }
    }

    /// The takers' enable: it falls (closing them) `at` after `go` rises
    fn takers(&self, netlist: &GateNetlist, delays: &dyn BundleDelays) -> Option<LatchGroup<'_>> {
        let mut from_go = self.from_go(netlist, delays, false);
        let (en, at) = match self.opener {
            Opener::Transparent { master_open } => {
                // `!go` changes the inverter's delay after `go`
                let not_go = from_go.at(master_open.source)?;
                (
                    master_open.control,
                    falls_through(netlist, delays, master_open, not_go)?,
                )
            }
            Opener::Pulse {
                pulse_end,
                pulse,
                gate,
            } => {
                let end = from_go.at(pulse_end.control)?;
                (
                    pulse,
                    through_pin(netlist, delays, gate, pulse_end.control, end)?,
                )
            }
        };
        Some(LatchGroup {
            en,
            at,
            bits: &self.takers,
        })
    }

    /// The latches that launch the new state, by enable: the slaves, and
    /// the pulsed latches without a slave. Each enable rises (opening them)
    /// `at` after `go` rises (earliest arrivals with `early`).
    fn launchers(
        &self,
        netlist: &GateNetlist,
        delays: &dyn BundleDelays,
        early: bool,
    ) -> Vec<LatchGroup<'_>> {
        let mut groups = Vec::new();
        if let Some((slave_open, slaves)) = &self.slaves {
            if let Some(at) = self.from_go(netlist, delays, early).at(slave_open.control) {
                groups.push(LatchGroup {
                    en: slave_open.control,
                    at,
                    bits: slaves,
                });
            }
        }
        if let (Opener::Pulse { pulse, gate, .. }, false) = (&self.opener, self.singles.is_empty())
        {
            if let Some(at) = through_pin(netlist, delays, *gate, self.go.control, 0) {
                groups.push(LatchGroup {
                    en: *pulse,
                    at,
                    bits: &self.singles,
                });
            }
        }
        groups
    }
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
    /// Earliest instead of latest arrivals, for hold checks. A matched
    /// delay still takes its latest input: it times a rising edge, which
    /// needs both the source and the end of its chain.
    early: bool,
    /// Latch bits by their output: like a register's, a latch's output is
    /// timed from its enable (the latches of a block are never open while
    /// their data changes; the latch checks see to that)
    latches: HashMap<GateNetId, LatchBit>,
    /// Sources of matched delays. A gate that makes one purely from
    /// controls (the `!go` of a latch block's `master_open`) is part of
    /// the timing, not data, and so is a plain buffer of a control (the
    /// acknowledge output of a block, buffered on its way out).
    delay_sources: HashSet<GateNetId>,
    /// Enables of latches: a gate that makes one purely from controls (a
    /// pulsed-latch block's pulse) is part of the timing too
    latch_enables: HashSet<GateNetId>,
}

impl<'a> Arrivals<'a> {
    fn new(
        netlist: &'a GateNetlist,
        delays: &'a dyn BundleDelays,
        prefix: &'a str,
        through_registers: bool,
    ) -> Self {
        let matched = find_matched_delays(netlist);
        let bits = latch_bits(netlist);
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
            controls: matched.iter().map(|m| m.control).collect(),
            early: false,
            latch_enables: bits.iter().map(|b| b.en).collect(),
            latches: bits.into_iter().map(|b| (b.q, b)).collect(),
            delay_sources: matched.iter().map(|m| m.source).collect(),
        }
    }

    /// Earliest arrivals (see [`Arrivals::early`])
    fn earliest(mut self) -> Self {
        self.early = true;
        self
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
        if let Some(&t) = self.sources.as_ref().and_then(|s| s.get(&net)) {
            // A timed source, inside the block or not
            return Some(t);
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
        let result =
            match netlist.nets.get(net.0 as usize).and_then(|n| n.driver) {
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
                    } else if let Some(en) = self.latches.get(&net).map(|b| b.en) {
                        if self.through_registers {
                            let bit = self.latches[&net].clone();
                            let through = latch_path(netlist, self.delays, &bit, en)
                                .map(|(lo, hi)| if self.early { lo } else { hi });
                            self.at(en).zip(through).map(|(t, p)| t + p)
                        } else {
                            None
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
                        ) || ((self.delay_sources.contains(&net)
                            || self.latch_enables.contains(&net)
                            || cell.function == Some(CellFunction::Buf))
                            && !cell.inputs.is_empty()
                            && cell.inputs.iter().all(|i| self.controls.contains(i)));
                        let latest =
                            !self.early || cell.source_op.as_deref() == Some(MATCHED_DELAY_OP);
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
                                let better = if latest {
                                    best.is_none_or(|b| t > b)
                                } else {
                                    best.is_none_or(|b| t < b)
                                };
                                if better {
                                    self.from.insert(net, input);
                                    best = Some(t);
                                }
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
    let latch_blocks = latch_blocks(netlist, &matched);

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
                unmeasured_settle: false,
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
                unmeasured_settle: false,
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
        // Latch state: the masters' data inputs (with the latch's setup:
        // the next state goes into a master, which then holds it, while
        // the block waits), and the slaves' own feedback (a slave holds
        // its new value only once it has gone round)
        for lb in latch_blocks.iter().filter(|lb| lb.prefix == md.prefix) {
            if let Some(takers) = lb.takers(netlist, delays) {
                for m in takers.bits {
                    let (Some(t), Some(setup)) = (full.at(m.d), latch_setup(netlist, delays, m))
                    else {
                        continue;
                    };
                    endpoints += 1;
                    let t = t + setup;
                    if next_ps.is_none_or(|d| t > d) {
                        worst = Some((&netlist.cells[m.cells[0].0 as usize], m.d));
                    }
                    next_ps = Some(next_ps.map_or(t, |d: i64| d.max(t)));
                }
            }
            for launchers in lb.launchers(netlist, delays, false) {
                for s in launchers.bits {
                    let Some(t) = full.at(s.q) else { continue };
                    endpoints += 1;
                    let t = t + latch_wires(netlist, delays, s, s.q).map_or(0, |(_, h)| h);
                    if next_ps.is_none_or(|d| t > d) {
                        worst = Some((&netlist.cells[s.cells[0].0 as usize], s.q));
                    }
                    next_ps = Some(next_ps.map_or(t, |d: i64| d.max(t)));
                }
            }
        }
        // A self-timed block decides on its next step from `ready` as soon
        // as `done` has ended this one: `ready`, computed from the new
        // state, must have settled by then
        if self_timed_done {
            if let Some((cell, pin, input)) = step_control_ready(netlist, &md.prefix) {
                if let Some(t) = full.at(input) {
                    let t = t + delays.wire_ps(netlist, input, cell, pin);
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
                unmeasured_settle: false,
            });
        }
    }
    checks.extend(check_latch_blocks(netlist, delays, &latch_blocks));
    checks.extend(check_channels(netlist, delays, &matched, &latch_blocks));
    checks.extend(check_channel_holds(
        netlist,
        delays,
        &matched,
        &latch_blocks,
    ));
    checks.extend(check_register_holds(netlist, delays, &matched));
    checks.extend(check_arbiters(netlist, delays, &matched));
    checks.extend(check_releases(netlist, delays, &matched));
    checks.extend(check_grant_settles(netlist, delays, &matched));
    checks
}

/// `times` of the check that a latch block's slaves open only once its
/// masters have closed (see [`check_latch_blocks`])
pub const LATCH_OVERLAP: &str = "latch overlap";
/// `times` of the check that a latch block's masters reopen only once its
/// slaves have closed (see [`check_latch_blocks`])
pub const LATCH_REOPEN: &str = "latch reopen";
/// `times` of the check that a latch block's masters have reopened and
/// taken their data before the next step closes them (see
/// [`check_latch_blocks`])
pub const MASTER_OPEN: &str = "master open";
/// `times` of the check that a pulsed-latch block's pulse is long enough
/// for every latch to take its data (see [`check_latch_blocks`])
pub const PULSE_WIDTH: &str = "pulse width";
/// `times` of the check that a pulsed latch's new value does not come back
/// round to a latch of the same pulse while it is still open (see
/// [`check_latch_blocks`])
pub const LATCH_RACE: &str = "latch race";
/// `times` of the check that a pulsed-latch block's pulse gate sees `go`
/// fall no later than the pulse's end (see [`check_latch_blocks`])
pub const PULSE_RETURN: &str = "pulse return";

/// Checks that a longer matched delay does not fix (holds and races): they
/// are reported, and [`size_matched_delays`] leaves them alone
pub fn is_report_only(times: &str) -> bool {
    times == REGISTER_HOLD || times == LATCH_RACE || times == PULSE_RETURN
}

/// The timing of blocks whose state is held in latches (see
/// [`LatchBlock`]). For every scheme:
///
/// * "registers" (sized by `go`): the next state, computed from the block's
///   inputs, has gone through each latch that takes it and back to its own
///   input before that latch's enable falls at it.
///
/// Master-slave pairs:
///
/// * [`LATCH_OVERLAP`] (sized by `slave_open`): every master has closed
///   before any slave opens, so no value passes a master and its slave at
///   once (the slaves' new outputs only reach the masters' inputs after
///   that, so this is the masters' hold too);
/// * [`LATCH_REOPEN`] (sized by `master_open`): when `go` falls, every slave
///   has closed before any master reopens;
/// * [`MASTER_OPEN`] (sized by `go`): a master that reopens after `go`
///   falls has passed its data and back to its own input before the next
///   step closes it. The next `go` can rise as soon as its matched delay
///   after the request returns, which can be at once.
///
/// Pulsed latches:
///
/// * [`PULSE_WIDTH`] (sized by `pulse_end`): the pulse stays up at every
///   latch until the latch has passed its data and back to its own input;
/// * [`LATCH_RACE`] (reported): the earliest new value of any latch reaches
///   no latch's data input before that latch has closed. A longer pulse
///   only makes it worse;
/// * [`PULSE_RETURN`] (reported): when `go` falls, its fall reaches the
///   pulse gate no later than the pulse end's own fall (which follows it),
///   or the gate would pulse again.
///
/// The next state the latches launch is checked against `done` with the
/// other "next state" paths.
fn check_latch_blocks(
    netlist: &GateNetlist,
    delays: &dyn BundleDelays,
    blocks: &[LatchBlock],
) -> Vec<BundleCheck> {
    let mut checks = Vec::new();
    for lb in blocks {
        let check =
            |md: &MatchedDelay, times: &'static str, data_ps: i64, control_ps: i64| BundleCheck {
                prefix: lb.prefix.to_string(),
                delay_path: netlist.cells[md.and_cell.0 as usize].path.clone(),
                times,
                data_ps,
                control_ps,
                chain_len: md.chain.len(),
                sender: None,
                unmeasured_settle: false,
            };
        let Some(takers) = lb.takers(netlist, delays) else {
            continue;
        };
        if takers.bits.is_empty() {
            continue;
        }

        // Setup of the latches that take the next state
        let mut data_only = Arrivals::new(netlist, delays, lb.prefix, false);
        let mut full = Arrivals::new(netlist, delays, lb.prefix, true);
        if let Some(go_at) = full.at(lb.go.control) {
            let mut data_ps: Option<i64> = None;
            let mut control_ps: Option<i64> = None;
            for m in takers.bits {
                if let Some((lo, _)) = latch_wires(netlist, delays, m, takers.en) {
                    let t = go_at + takers.at + lo;
                    control_ps = Some(control_ps.map_or(t, |c: i64| c.min(t)));
                }
                if let (Some(t), Some(setup)) = (data_only.at(m.d), latch_setup(netlist, delays, m))
                {
                    let t = t + setup;
                    data_ps = Some(data_ps.map_or(t, |d: i64| d.max(t)));
                }
            }
            if let (Some(data_ps), Some(control_ps)) = (data_ps, control_ps) {
                checks.push(check(lb.go, "registers", data_ps, control_ps));
            }
        }

        // Masters closed before the slaves open
        if let Some((slave_open, slaves)) = &lb.slaves {
            let closed = closed_at(netlist, delays, takers.bits, takers.en, takers.at);
            let opens = lb
                .from_go(netlist, delays, false)
                .at(slave_open.control)
                .and_then(|at| opens_at(netlist, delays, slaves, slave_open.control, at));
            if let (Some(data_ps), Some(control_ps)) = (closed, opens) {
                checks.push(check(slave_open, LATCH_OVERLAP, data_ps, control_ps));
            }
        }

        match lb.opener {
            Opener::Transparent { master_open } => {
                let Some((slave_open, slaves)) = &lb.slaves else {
                    continue;
                };
                let (mo, so) = (master_open.control, slave_open.control);
                let masters = takers.bits;
                // After `go` falls: the slaves close straight through
                // `slave_open`'s AND, the masters reopen through
                // `master_open`'s chain
                let mut from_go = lb.from_go(netlist, delays, false);
                let (Some(slave_close), Some(master_reopen)) = (
                    falls_through(netlist, delays, slave_open, 0),
                    from_go.at(mo),
                ) else {
                    continue;
                };
                let closed = closed_at(netlist, delays, slaves, so, slave_close);
                let opens = opens_at(netlist, delays, masters, mo, master_reopen);
                if let (Some(data_ps), Some(control_ps)) = (closed, opens) {
                    checks.push(check(master_open, LATCH_REOPEN, data_ps, control_ps));
                }

                // Masters open long enough before the next step: from `go`
                // falling, the next `go` rises at the earliest its chain's
                // rise (beyond the direct path through its AND) later
                let Some(chain) = Arrivals::new(netlist, delays, lb.prefix, false)
                    .with_sources(HashMap::from([(lb.go.source, 0)]))
                    .at(lb.go.control)
                else {
                    continue;
                };
                let Some(direct) = falls_through(netlist, delays, lb.go, 0) else {
                    continue;
                };
                let taken = masters
                    .iter()
                    .filter_map(|m| {
                        let (_, open) = latch_path(netlist, delays, m, mo)?;
                        let feedback = latch_wires(netlist, delays, m, m.q).map_or(0, |(_, h)| h);
                        Some(master_reopen + open + feedback)
                    })
                    .max();
                let closes = opens_at(netlist, delays, masters, mo, chain - direct + takers.at);
                if let (Some(data_ps), Some(control_ps)) = (taken, closes) {
                    checks.push(check(lb.go, MASTER_OPEN, data_ps, control_ps));
                }
            }
            Opener::Pulse {
                pulse_end,
                pulse,
                gate,
            } => {
                let latches = takers.bits;
                let Some(opens) = through_pin(netlist, delays, gate, lb.go.control, 0) else {
                    continue;
                };
                // Long enough: every latch has passed its data and back
                let passed = latches
                    .iter()
                    .filter_map(|b| {
                        let (_, open) = latch_path(netlist, delays, b, pulse)?;
                        let feedback = latch_wires(netlist, delays, b, b.q).map_or(0, |(_, h)| h);
                        Some(opens + open + feedback)
                    })
                    .max();
                let closes = opens_at(netlist, delays, latches, pulse, takers.at);
                if let (Some(data_ps), Some(control_ps)) = (passed, closes) {
                    checks.push(check(pulse_end, PULSE_WIDTH, data_ps, control_ps));
                }

                // Short enough: the earliest new values of the latches
                // without a slave (a slave stays closed while the pulse is
                // up), round to any latch's data input, after that latch
                // has closed
                if !lb.singles.is_empty() {
                    let launched: HashMap<GateNetId, i64> = lb
                        .singles
                        .iter()
                        .filter_map(|b| {
                            let (lo, _) = latch_path(netlist, delays, b, pulse)?;
                            Some((b.q, opens + lo))
                        })
                        .collect();
                    let mut round = Arrivals::new(netlist, delays, lb.prefix, false)
                        .earliest()
                        .with_sources(launched);
                    let mut worst: Option<(i64, i64)> = None;
                    for b in latches.iter() {
                        let (Some(t), Some((wire, _)), Some((_, close))) = (
                            round.at(b.d),
                            latch_wires(netlist, delays, b, b.d),
                            latch_path(netlist, delays, b, pulse),
                        ) else {
                            continue;
                        };
                        let (until, changes) = (takers.at + close, t + wire);
                        if worst.is_none_or(|(u, c)| changes - until < c - u) {
                            worst = Some((until, changes));
                        }
                    }
                    if let Some((until, changes)) = worst {
                        checks.push(check(pulse_end, LATCH_RACE, until, changes));
                    }
                }

                // The pulse gate sees `go` fall no later than the pulse end
                let gate_cell = &netlist.cells[gate.0 as usize];
                let at_pins = |net: GateNetId| {
                    gate_cell
                        .inputs
                        .iter()
                        .enumerate()
                        .filter(move |&(_, &n)| n == net)
                        .map(move |(pin, _)| delays.wire_ps(netlist, net, gate_cell, pin))
                };
                let go_at = at_pins(lb.go.control).max();
                let end_at = falls_through(netlist, delays, pulse_end, 0)
                    .zip(at_pins(pulse_end.control).min())
                    .map(|(falls, wire)| falls + wire);
                if let (Some(go_at), Some(end_at)) = (go_at, end_at) {
                    checks.push(check(pulse_end, PULSE_RETURN, go_at, end_at));
                }
            }
        }
    }
    checks
}

/// Settle times of a self-timed block's logic after its arbiter release
/// rises. Unlike [`Arrivals`], which takes the latest input of every gate
/// (right for a rising edge rippling through a chain), this follows the
/// values the release forces: with the release high and every other input
/// of the block unknown, a gate's output is final as soon as the inputs
/// that have already changed determine it whatever the others do (an AND
/// whose input has fallen, a deferral stage whose request has dropped). A
/// gate whose final value the release does not determine counts from its
/// latest changing input. Registers, matched delays, the step control and
/// anything outside the block do not change in that time.
struct ReleaseArrivals<'a> {
    netlist: &'a GateNetlist,
    delays: &'a dyn BundleDelays,
    prefix: &'a str,
    release: GateNetId,
    /// net -> (settle time, final value: None if not determined)
    memo: HashMap<GateNetId, Option<(i64, Option<bool>)>>,
    visiting: HashSet<GateNetId>,
}

impl<'a> ReleaseArrivals<'a> {
    fn new(
        netlist: &'a GateNetlist,
        delays: &'a dyn BundleDelays,
        prefix: &'a str,
        release: GateNetId,
    ) -> Self {
        Self {
            netlist,
            delays,
            prefix,
            release,
            memo: HashMap::new(),
            visiting: HashSet::new(),
        }
    }

    /// When `net` settles after the release rises, and its final value;
    /// `None` if the release does not reach it
    fn at(&mut self, net: GateNetId) -> Option<(i64, Option<bool>)> {
        if net == self.release {
            return Some((0, Some(true)));
        }
        if let Some(&r) = self.memo.get(&net) {
            return r;
        }
        if !self.visiting.insert(net) {
            // a loop (a gate's own state): it holds while the release acts
            return None;
        }
        let result = self.compute(net);
        self.visiting.remove(&net);
        self.memo.insert(net, result);
        result
    }

    fn compute(&mut self, net: GateNetId) -> Option<(i64, Option<bool>)> {
        let netlist = self.netlist;
        let n = &netlist.nets[net.0 as usize];
        let cell = &netlist.cells[n.driver?.0 as usize];
        if !cell.path.starts_with(self.prefix)
            || cell.clock.is_some()
            || matches!(
                cell.source_op.as_deref(),
                Some(MATCHED_DELAY_OP) | Some(STEP_CONTROL_OP) | Some(LATCH_OP)
            )
        {
            return None;
        }
        let output = n.driver_pin.unwrap_or(0);
        // (arrival at the output through this input, final input value)
        let mut changing: Vec<(usize, i64, Option<bool>)> = Vec::new();
        for (i, &input) in cell.inputs.iter().enumerate() {
            let Some(arc) = self.delays.arc_ps(cell, i, output) else {
                continue;
            };
            if let Some((t, v)) = self.at(input) {
                changing.push((i, t + self.delays.wire_ps(netlist, input, cell, i) + arc, v));
            }
        }
        if changing.is_empty() {
            return None;
        }
        let latest = changing.iter().map(|c| c.1).max().unwrap_or(0);
        let mut known: Vec<Option<bool>> = vec![None; cell.inputs.len()];
        for &(i, _, v) in &changing {
            known[i] = v;
        }
        let Some(final_value) = settled_output(cell, output, &known) else {
            return Some((latest, None));
        };
        // the earliest set of changed inputs that already forces it
        changing.sort_by_key(|c| c.1);
        let mut partial: Vec<Option<bool>> = vec![None; cell.inputs.len()];
        for &(i, t, v) in &changing {
            partial[i] = v;
            if settled_output(cell, output, &partial) == Some(final_value) {
                return Some((t, Some(final_value)));
            }
        }
        Some((latest, Some(final_value)))
    }
}

/// The output of `cell` when the inputs marked `Some` hold those values and
/// the rest may be anything: `Some` if every completion gives the same
/// value. LUTs are evaluated from their truth table and plain gates from
/// their function; anything else is not determined.
fn settled_output(cell: &Cell, output: usize, inputs: &[Option<bool>]) -> Option<bool> {
    use crate::tech_library::CellFunction as F;
    if output != 0 || inputs.len() > 6 {
        return None;
    }
    let eval = |b: &[bool]| -> Option<bool> {
        if let Some(init) = cell.lut_init {
            let index = b
                .iter()
                .enumerate()
                .fold(0usize, |acc, (i, &x)| acc | ((x as usize) << i));
            return Some((init >> index) & 1 == 1);
        }
        let all = |f: &dyn Fn(bool, bool) -> bool, unit: bool| b.iter().fold(unit, |a, &x| f(a, x));
        Some(match cell.function.as_ref()? {
            F::Inv => !*b.first()?,
            F::Buf => *b.first()?,
            F::And2 | F::And3 | F::And4 => all(&|a, x| a && x, true),
            F::Or2 | F::Or3 | F::Or4 => all(&|a, x| a || x, false),
            F::Nand2 | F::Nand3 | F::Nand4 => !all(&|a, x| a && x, true),
            F::Nor2 | F::Nor3 | F::Nor4 => !all(&|a, x| a || x, false),
            F::AndNot => *b.first()? && !*b.get(1)?,
            F::OrNot => *b.first()? || !*b.get(1)?,
            _ => return None,
        })
    };
    let unknown: Vec<usize> = (0..inputs.len()).filter(|&i| inputs[i].is_none()).collect();
    let mut value: Option<bool> = None;
    for combo in 0..(1usize << unknown.len()) {
        let mut b: Vec<bool> = inputs.iter().map(|v| v.unwrap_or(false)).collect();
        for (k, &i) in unknown.iter().enumerate() {
            b[i] = (combo >> k) & 1 == 1;
        }
        let v = eval(&b)?;
        match value {
            None => value = Some(v),
            Some(prev) if prev != v => return None,
            _ => {}
        }
    }
    value
}

/// Name (followed by the arbiter's number) of the matched delay that holds
/// back a self-timed block's "some arm granted" until what the grant
/// decides has settled (see `ncl_expand::expand_to_ncl_bridged_with` and
/// [`check_grant_settles`])
pub const GRANT_SETTLE_DELAY_PATH: &str = "ncl_bundle.grant_settle";

/// A block whose send/recv conditions depend on its arbiters' grants
/// (`skalp_frontend::ncl_streams::grant_settle_needed`) reads "some arm
/// granted" at its step latch through a matched delay. Everything the
/// grants decide must settle at the latch's `ready` input before that
/// delayed signal arrives, or `ready` can glitch up for a step whose
/// channel is not ready. Data: from the grants (every output of the
/// block's arbiter cells, at time 0) to the step latch's `ready` input,
/// leaving out the delayed signal itself. Control: from the grants through
/// "some arm granted" and the delay.
fn check_grant_settles(
    netlist: &GateNetlist,
    delays: &dyn BundleDelays,
    matched: &[MatchedDelay],
) -> Vec<BundleCheck> {
    let mut checks = Vec::new();
    for md in matched {
        let and_cell = &netlist.cells[md.and_cell.0 as usize];
        if !and_cell.path.contains(GRANT_SETTLE_DELAY_PATH) {
            continue;
        }
        let grants: HashMap<GateNetId, i64> = netlist
            .cells
            .iter()
            .filter(|c| {
                c.source_op.as_deref() == Some(ARBITER_OP) && c.path.starts_with(&md.prefix)
            })
            .flat_map(|c| c.outputs.iter().map(|&o| (o, 0)))
            .collect();
        let Some((cell, pin, input)) = step_control_ready(netlist, &md.prefix) else {
            continue;
        };
        let Some(data_ps) = Arrivals::new(netlist, delays, &md.prefix, false)
            .with_sources(grants.clone())
            .at(input)
            .map(|t| t + delays.wire_ps(netlist, input, cell, pin))
        else {
            continue;
        };
        let Some(control_ps) = Arrivals::new(netlist, delays, &md.prefix, false)
            .with_sources(grants)
            .at(md.control)
        else {
            continue;
        };
        checks.push(BundleCheck {
            prefix: md.prefix.clone(),
            delay_path: and_cell.path.clone(),
            times: "grant settle",
            data_ps,
            control_ps,
            chain_len: md.chain.len(),
            sender: None,
            unmeasured_settle: false,
        });
    }
    checks
}

/// Name of the matched delay that holds a self-timed block's arbiter
/// release up after `done` (see `ncl_expand::expand_to_ncl_bridged_with`)
pub const RELEASE_DELAY_PATH: &str = "ncl_bundle.release";

/// The release of a self-timed block that arbitrates: `release = !(!done &
/// delay(!done))` drops the arbiter's requests and holds the step latch
/// off. It must stay up until the dropped requests have cleared the
/// arbiter's grants and `ready` at the step latch; otherwise the latch
/// starts the next step from the grant the arbiter is withdrawing, and the
/// grant changes under that step. Data: from `release` rising until
/// `ready` at the step latch and the arbiters' inputs have settled
/// ([`ReleaseArrivals`]: a gate settles as soon as the inputs that have
/// already changed force its output, so a request that falls clears the
/// arbiter's deferral chains at once rather than through their ripple). Control: how much
/// longer the release stays up than `done`, the rise through the delay's
/// chain (the time `done` itself stays up is left out, which only adds
/// margin).
fn check_releases(
    netlist: &GateNetlist,
    delays: &dyn BundleDelays,
    matched: &[MatchedDelay],
) -> Vec<BundleCheck> {
    let mut checks = Vec::new();
    for md in matched {
        let inside = |cell: &Cell| cell.path.starts_with(&md.prefix);
        // `release`: an inverter of this delay's control that the block's
        // step control reads
        let release = netlist.nets[md.control.0 as usize]
            .fanout
            .iter()
            .map(|&(c, _)| &netlist.cells[c.0 as usize])
            .filter(|c| inside(c) && c.inputs.len() == 1 && c.outputs.len() == 1)
            .map(|c| c.outputs[0])
            .find(|&out| {
                netlist.nets[out.0 as usize].fanout.iter().any(|&(c, _)| {
                    let cell = &netlist.cells[c.0 as usize];
                    inside(cell) && cell.source_op.as_deref() == Some(STEP_CONTROL_OP)
                })
            });
        let Some(release) = release else {
            continue;
        };
        let mut from_release = ReleaseArrivals::new(netlist, delays, &md.prefix, release);
        let mut data_ps: Option<i64> = None;
        let mut endpoint = |t: Option<i64>| {
            if let Some(t) = t {
                data_ps = Some(data_ps.map_or(t, |d: i64| d.max(t)));
            }
        };
        if let Some((cell, pin, input)) = step_control_ready(netlist, &md.prefix) {
            endpoint(
                from_release
                    .at(input)
                    .map(|(t, _)| t + delays.wire_ps(netlist, input, cell, pin)),
            );
        }
        for cell in netlist
            .cells
            .iter()
            .filter(|c| inside(c) && c.source_op.as_deref() == Some(ARBITER_OP))
        {
            for (i, &input) in cell.inputs.iter().enumerate() {
                endpoint(
                    from_release
                        .at(input)
                        .map(|(t, _)| t + delays.wire_ps(netlist, input, cell, i)),
                );
            }
        }
        let Some(data_ps) = data_ps else {
            continue;
        };
        // The chain's rise, beyond the direct path through the AND
        let and_cell = &netlist.cells[md.and_cell.0 as usize];
        let Some(at_control) = Arrivals::new(netlist, delays, &md.prefix, false)
            .with_sources(HashMap::from([(md.source, 0)]))
            .at(md.control)
        else {
            continue;
        };
        let direct = and_cell
            .inputs
            .iter()
            .position(|&n| n == md.source)
            .and_then(|pin| {
                delays
                    .arc_ps(and_cell, pin, 0)
                    .map(|arc| arc + delays.wire_ps(netlist, md.source, and_cell, pin))
            })
            .unwrap_or(0);
        checks.push(BundleCheck {
            prefix: md.prefix.clone(),
            delay_path: and_cell.path.clone(),
            times: "release",
            data_ps,
            control_ps: at_control - direct,
            chain_len: md.chain.len(),
            sender: None,
            unmeasured_settle: false,
        });
    }
    checks
}

/// Decisions of self-timed blocks: the strobe `go` waits a matched delay
/// after the step request. The step request is a latch set by `ncl_req`
/// and `ready`, which change independently of each other (and `ready`
/// comes through the block's arbiters, if it has any), so it can be
/// undecided for a while, like a mutual-exclusion element. The wait must
/// cover that resolution time ([`ArbiterTiming`]); a block with arbiters
/// reports it as "arbiter", any other as "step decision".
fn check_arbiters(
    netlist: &GateNetlist,
    delays: &dyn BundleDelays,
    matched: &[MatchedDelay],
) -> Vec<BundleCheck> {
    // Every step decision goes through the step latch; a block with
    // arbiters also waits for its mutual-exclusion elements, so it needs
    // whichever of the two is longer
    let latch = delays.step_latch_timing();
    let arbiter = delays.arbiter_timing();
    let mut checks = Vec::new();
    for md in matched {
        // `go`'s delay: its source is the step request
        let from_step = fed_by_step(netlist, md);
        let arbitrates = netlist
            .cells
            .iter()
            .any(|c| c.source_op.as_deref() == Some(ARBITER_OP) && c.path.starts_with(&md.prefix));
        if !from_step {
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
        let timing = if arbitrates && arbiter.resolution_ps() > latch.resolution_ps() {
            arbiter
        } else {
            latch
        };
        let timing = match netlist.settle_wait_ps {
            Some(wait) => timing.sized_for_wait(wait),
            None => timing,
        };
        let required = timing.resolution_ps();
        tracing::debug!(
            "NCL step decision {}: resolution wait {} ps (needs {} ps), MTBF {:.3e} years",
            md.prefix.trim_end_matches('.'),
            wait,
            required,
            timing.mtbf_years_after(wait)
        );
        checks.push(BundleCheck {
            prefix: md.prefix.clone(),
            delay_path: netlist.cells[md.and_cell.0 as usize].path.clone(),
            times: if arbitrates {
                "arbiter"
            } else {
                "step decision"
            },
            data_ps: required,
            control_ps: wait,
            chain_len: md.chain.len(),
            sender: None,
            unmeasured_settle: !timing.measured,
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
    latch_blocks: &[LatchBlock],
) -> Vec<BundleCheck> {
    let mut checks = Vec::new();
    let control_prefix: HashMap<GateNetId, &str> = matched
        .iter()
        .map(|m| (m.control, m.prefix.as_str()))
        .collect();
    for md in matched {
        // The `go` delay of a self-timed block
        if !fed_by_step(netlist, md) {
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
        // Senders that hold their state in latches: the latches that launch
        // do so when their enable opens them (times from the sender's `go`)
        for lb in latch_blocks.iter().filter(|lb| lb.prefix != md.prefix) {
            let go = lb.go.control;
            for launchers in lb.launchers(netlist, delays, false) {
                for s in launchers.bits {
                    if inside(&netlist.cells[s.cells[0].0 as usize])
                        || !netlist.nets[s.q.0 as usize]
                            .fanout
                            .iter()
                            .any(|&(c, _)| inside(&netlist.cells[c.0 as usize]))
                    {
                        continue;
                    }
                    let Some((_, through)) = latch_path(netlist, delays, s, launchers.en) else {
                        continue;
                    };
                    let t = launchers.at + through;
                    match senders.iter_mut().find(|(c, _)| *c == go) {
                        Some((_, nets)) => {
                            nets.insert(s.q, t);
                        }
                        None => senders.push((go, HashMap::from([(s.q, t)]))),
                    }
                }
            }
        }
        // This block's own latches that take the values, if it holds its
        // state in latches
        let receiver = latch_blocks
            .iter()
            .find(|lb| lb.go.control == md.control)
            .and_then(|lb| lb.takers(netlist, delays));
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
            if let Some(takers) = &receiver {
                for m in takers.bits {
                    if let Some((lo, _)) = latch_wires(netlist, delays, m, takers.en) {
                        let clk = control_at + takers.at + lo;
                        clock_ps = Some(clock_ps.map_or(clk, |c: i64| c.min(clk)));
                    }
                    if let (Some(t), Some(setup)) = (data.at(m.d), latch_setup(netlist, delays, m))
                    {
                        let t = t + setup;
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
                    unmeasured_settle: false,
                });
            }
        }
    }
    checks
}

/// Hold of channels into self-timed blocks. The step that takes a value
/// toggles the receiver's phase bit with the same strobe that captures the
/// value; the sender may then send the next one, and its registers change.
/// That must not reach the receiver's registers before the strobe that
/// captured the old value has (plus their hold time): the path runs from
/// the receiver's strobe, through its phase bit, the sender's `ready`, step
/// latch and `go` delay, to the sender's registers and back. It is short
/// only when the receiver's strobe reaches some of its registers much later
/// than others, but that strobe comes from fabric logic, so its skew is not
/// bounded by a clock network.
///
/// Times are earliest arrivals from the receiver's strobe (time 0). The
/// check's `data_ps` is when the captured value may first change at the
/// worst register (its strobe arrival plus hold), `control_ps` the earliest
/// the sender's next value gets there; it is reported against the sender's
/// `go` delay, the one that lengthening fixes.
fn check_channel_holds(
    netlist: &GateNetlist,
    delays: &dyn BundleDelays,
    matched: &[MatchedDelay],
    latch_blocks: &[LatchBlock],
) -> Vec<BundleCheck> {
    let mut checks = Vec::new();
    let by_control: HashMap<GateNetId, &MatchedDelay> =
        matched.iter().map(|m| (m.control, m)).collect();
    // Launching outputs of registers on `clock` inside `prefix` that feed
    // cells inside `into`, each at its launch time after `at` (the strobe's
    // arrival at its own net)
    let launches = |clock: GateNetId, prefix: &str, into: &str, at: i64| {
        let mut out: HashMap<GateNetId, i64> = HashMap::new();
        for (id, net) in netlist.nets.iter().enumerate() {
            let Some(driver) = net.driver else { continue };
            let cell = &netlist.cells[driver.0 as usize];
            if cell.clock != Some(clock)
                || !cell.path.starts_with(prefix)
                || !delays.launches(cell, net.driver_pin.unwrap_or(0))
                || !net
                    .fanout
                    .iter()
                    .any(|&(c, _)| netlist.cells[c.0 as usize].path.starts_with(into))
            {
                continue;
            }
            let t = at + delays.clock_wire_ps(netlist, clock, cell) + delays.clock_to_out_ps(cell);
            out.insert(GateNetId(id as u32), t);
        }
        // A block holding its state in latches launches from the latches
        // that open after `go` (its slaves, or its pulsed latches),
        // at the earliest after `go` (`clock`) rises
        if let Some(lb) = latch_blocks.iter().find(|lb| lb.go.control == clock) {
            for launchers in lb.launchers(netlist, delays, true) {
                for s in launchers.bits {
                    if !netlist.nets[s.q.0 as usize]
                        .fanout
                        .iter()
                        .any(|&(c, _)| netlist.cells[c.0 as usize].path.starts_with(into))
                    {
                        continue;
                    }
                    if let Some((lo, _)) = latch_path(netlist, delays, s, launchers.en) {
                        out.insert(s.q, at + launchers.at + lo);
                    }
                }
            }
        }
        out
    };
    for rx in matched.iter().filter(|m| fed_by_step(netlist, m)) {
        let inside = |cell: &Cell| cell.path.starts_with(&rx.prefix);
        // Strobes of other blocks whose registers feed this one
        let mut senders: Vec<&MatchedDelay> = Vec::new();
        for net in &netlist.nets {
            let Some(driver) = net.driver else { continue };
            let cell = &netlist.cells[driver.0 as usize];
            let Some(tx) = cell.clock.and_then(|k| by_control.get(&k)) else {
                continue;
            };
            if tx.prefix == rx.prefix
                || inside(cell)
                || !net
                    .fanout
                    .iter()
                    .any(|&(c, _)| inside(&netlist.cells[c.0 as usize]))
                || senders.iter().any(|s| s.control == tx.control)
            {
                continue;
            }
            senders.push(tx);
        }
        // Blocks holding their state in latches, whose latches feed this one
        for lb in latch_blocks.iter().filter(|lb| lb.prefix != rx.prefix) {
            let launchers = lb.launchers(netlist, delays, true);
            let feeds = launchers.iter().flat_map(|g| g.bits).any(|s| {
                !inside(&netlist.cells[s.cells[0].0 as usize])
                    && netlist.nets[s.q.0 as usize]
                        .fanout
                        .iter()
                        .any(|&(c, _)| inside(&netlist.cells[c.0 as usize]))
            });
            if feeds && !senders.iter().any(|s| s.control == lb.go.control) {
                senders.push(lb.go);
            }
        }
        // This block's own latches that take values, if it holds its state
        // in latches: each holds until its enable's fall has gone through it
        let receiver = latch_blocks
            .iter()
            .find(|lb| lb.go.control == rx.control)
            .and_then(|lb| lb.takers(netlist, delays));
        for tx in senders {
            // The receiver's registers that the sender reads (its phase bit)
            let back = launches(rx.control, &rx.prefix, &tx.prefix, 0);
            if back.is_empty() {
                continue;
            }
            let Some(tx_go) = Arrivals::new(netlist, delays, &tx.prefix, true)
                .earliest()
                .with_sources(back)
                .at(tx.control)
            else {
                // The sender does not wait for this block: no channel
                // back-pressured by it
                continue;
            };
            let forward = launches(tx.control, &tx.prefix, &rx.prefix, tx_go);
            let mut data = Arrivals::new(netlist, delays, &rx.prefix, false)
                .earliest()
                .with_sources(forward);
            let mut worst: Option<(i64, i64)> = None;
            for cell in netlist
                .cells
                .iter()
                .filter(|c| c.clock == Some(rx.control) && inside(c))
            {
                let held = delays.clock_wire_ps(netlist, rx.control, cell);
                for (i, &input) in cell.inputs.iter().enumerate() {
                    let Some(t) = data.at(input) else { continue };
                    let changes = t + delays.wire_ps(netlist, input, cell, i);
                    let until = held + delays.hold_ps(cell, i);
                    if worst.is_none_or(|(u, c)| changes - until < c - u) {
                        worst = Some((until, changes));
                    }
                }
            }
            if let Some(takers) = &receiver {
                for m in takers.bits {
                    let (Some(t), Some((wire, _)), Some((_, close))) = (
                        data.at(m.d),
                        latch_wires(netlist, delays, m, m.d),
                        latch_path(netlist, delays, m, takers.en),
                    ) else {
                        continue;
                    };
                    let changes = t + wire;
                    let until = takers.at + close;
                    if worst.is_none_or(|(u, c)| changes - until < c - u) {
                        worst = Some((until, changes));
                    }
                }
            }
            if let Some((until, changes)) = worst {
                checks.push(BundleCheck {
                    prefix: rx.prefix.clone(),
                    delay_path: netlist.cells[tx.and_cell.0 as usize].path.clone(),
                    times: "channel hold",
                    data_ps: until,
                    control_ps: changes,
                    chain_len: tx.chain.len(),
                    sender: Some(tx.prefix.clone()),
                    unmeasured_settle: false,
                });
            }
        }
    }
    checks
}

/// `times` of the in-block hold check (`check_register_holds`)
pub const REGISTER_HOLD: &str = "register hold";

/// Hold inside a block: a strobe reaches the block's registers at slightly
/// different times, and a register it has already clocked launches its new
/// value, which must not reach another register on the same strobe before
/// the strobe itself has (plus that register's hold time). Times count from
/// the strobe at its own net: data is the earliest arrival of a new value at
/// each register input, the control the strobe's arrival at that register
/// plus its hold time. A longer matched delay does not help a hold, so
/// [`size_matched_delays`] leaves these checks alone; they are reported.
fn check_register_holds(
    netlist: &GateNetlist,
    delays: &dyn BundleDelays,
    matched: &[MatchedDelay],
) -> Vec<BundleCheck> {
    let mut checks = Vec::new();
    for md in matched {
        let inside = |cell: &Cell| cell.path.starts_with(&md.prefix);
        let clocked: Vec<&Cell> = netlist
            .cells
            .iter()
            .filter(|c| c.clock == Some(md.control) && inside(c))
            .collect();
        if clocked.is_empty() {
            continue;
        }
        // every launching output of the strobe's registers, at its launch
        let mut launched: HashMap<GateNetId, i64> = HashMap::new();
        for cell in &clocked {
            let at = delays.clock_wire_ps(netlist, md.control, cell) + delays.clock_to_out_ps(cell);
            for (pin, &out) in cell.outputs.iter().enumerate() {
                if delays.launches(cell, pin) {
                    launched.insert(out, at);
                }
            }
        }
        let mut data = Arrivals::new(netlist, delays, &md.prefix, false)
            .earliest()
            .with_sources(launched);
        let mut worst: Option<(i64, i64)> = None;
        for cell in &clocked {
            let held = delays.clock_wire_ps(netlist, md.control, cell);
            for (i, &input) in cell.inputs.iter().enumerate() {
                let Some(t) = data.at(input) else { continue };
                let changes = t + delays.wire_ps(netlist, input, cell, i);
                let until = held + delays.hold_ps(cell, i);
                if worst.is_none_or(|(u, c)| changes - until < c - u) {
                    worst = Some((until, changes));
                }
            }
        }
        if let Some((until, changes)) = worst {
            checks.push(BundleCheck {
                prefix: md.prefix.clone(),
                delay_path: netlist.cells[md.and_cell.0 as usize].path.clone(),
                times: REGISTER_HOLD,
                data_ps: until,
                control_ps: changes,
                chain_len: md.chain.len(),
                sender: None,
                unmeasured_settle: false,
            });
        }
    }
    checks
}

/// Whether `md` is the `go` delay of a self-timed block: its source is the
/// step request, driven by the step control
fn fed_by_step(netlist: &GateNetlist, md: &MatchedDelay) -> bool {
    netlist.nets[md.source.0 as usize]
        .driver
        .is_some_and(|d| netlist.cells[d.0 as usize].source_op.as_deref() == Some(STEP_CONTROL_OP))
}

/// The step control cell of the self-timed block `prefix` that reads its
/// `ready` signal, with the input pin and net (see `LirOp::NclStepControl`):
/// the single-LUT latch `st_latch` (ready on pin 2), or the `st_start` gate
/// of the gate-built latch (ready on pin 0)
fn step_control_ready<'a>(
    netlist: &'a GateNetlist,
    prefix: &str,
) -> Option<(&'a Cell, usize, GateNetId)> {
    netlist
        .cells
        .iter()
        .filter(|c| c.source_op.as_deref() == Some(STEP_CONTROL_OP) && c.path.starts_with(prefix))
        .find_map(|c| {
            let pin = if c.path.ends_with(".st_latch") {
                STEP_LATCH_READY_PIN
            } else if c.path.ends_with(".st_start") {
                0
            } else {
                return None;
            };
            c.inputs.get(pin).map(|&n| (c, pin, n))
        })
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
    /// Metastability of the step latches and arbiters (one LUT with its own
    /// feedback), from the target's device data where it has them
    pub latch_timing: ArbiterTiming,
}

impl<'a> LibraryDelays<'a> {
    /// Estimates for `library` with the default routing estimate
    pub fn new(library: &'a TechLibrary) -> Self {
        Self {
            library,
            wire_ps: 500,
            default_cell_ps: 400,
            latch_timing: ArbiterTiming::default(),
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
    fn hold_ps(&self, cell: &Cell, _input: usize) -> i64 {
        self.library
            .get_cell(&cell.cell_type)
            .and_then(|c| c.hold_ps)
            .map_or(0, i64::from)
    }
    fn arbiter_timing(&self) -> ArbiterTiming {
        self.latch_timing
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

/// Take the last `count` stages off a matched delay's chain, the AND then
/// taking the stage before them. The first stage always stays, so the chain
/// keeps its fast reset (see [`extend_matched_delay`]). Returns how many
/// stages were removed.
pub fn shorten_matched_delay(netlist: &mut GateNetlist, md: &MatchedDelay, count: usize) -> usize {
    let keep = md.chain.len().saturating_sub(count).max(1);
    if keep >= md.chain.len() {
        return 0;
    }
    let last_kept = md.chain[keep - 1];
    let input = netlist.cells[last_kept.0 as usize].outputs[0];
    netlist.cells[md.and_cell.0 as usize].inputs[1] = input;
    let dropped: HashSet<CellId> = md.chain[keep..].iter().copied().collect();
    netlist.cells.retain(|c| !dropped.contains(&c.id));
    netlist.rebuild_net_connectivity();
    dropped.len()
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
                .filter(|c| &c.delay_path == path && !is_report_only(c.times))
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
    for c in &report.checks {
        tracing::debug!(
            "NCL check {} {}: data {} ps, control {} ps, slack {} ps, {} stages",
            c.delay_path,
            c.times,
            c.data_ps,
            c.control_ps,
            c.slack_ps(),
            c.chain_len
        );
    }
    report
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Every cell 100 ps, connections free, setup free
    /// After the release rises, a request it drops clears a deferral chain
    /// of ANDs at once (each stage's own request input forces it low), not
    /// through the chain's ripple; an OR with an input the release does not
    /// decide settles with its latest changing input
    #[test]
    fn release_settles_a_chain_through_its_forcing_input() {
        let mut netlist = GateNetlist::new("t".into(), "test".into());
        let mut net =
            |name: &str| netlist.add_net(GateNet::new(GateNetId(0), format!("top.u.{name}")));
        let (release, req, other) = (net("release"), net("req"), net("other"));
        let (held, h, d0, d1, d2, d3, o) = (
            net("held"),
            net("h"),
            net("d0"),
            net("d1"),
            net("d2"),
            net("d3"),
            net("o"),
        );
        let lut = |netlist: &mut GateNetlist, name: &str, inputs: Vec<GateNetId>, out, init| {
            let mut cell = Cell::new_comb(
                CellId(0),
                "LUT4".into(),
                "test".into(),
                0.0,
                format!("top.u.{name}"),
                inputs,
                vec![out],
            );
            cell.lut_init = Some(init);
            netlist.add_cell(cell);
        };
        // inverter, AND, buffer
        lut(&mut netlist, "inv", vec![release], held, 0x1);
        lut(&mut netlist, "h", vec![req, held], h, 0x8);
        lut(&mut netlist, "d0", vec![h], d0, 0x2);
        lut(&mut netlist, "d1", vec![h, d0], d1, 0x8);
        lut(&mut netlist, "d2", vec![h, d1], d2, 0x8);
        lut(&mut netlist, "d3", vec![h, d2], d3, 0x8);
        // OR
        lut(&mut netlist, "o", vec![d3, other], o, 0xE);
        let mut at = ReleaseArrivals::new(&netlist, &UnitDelays, "top.u.", release);
        assert_eq!(at.at(held), Some((100, Some(false))));
        assert_eq!(at.at(h), Some((200, Some(false))));
        // every stage falls one gate after `h`, not a gate after the stage before
        assert_eq!(at.at(d1), Some((300, Some(false))));
        assert_eq!(at.at(d3), Some((300, Some(false))));
        assert_eq!(at.at(o), Some((400, None)));
        // nothing the release reaches
        assert_eq!(at.at(other), None);
        // the plain walk takes the ripple
        let mut ripple = Arrivals::new(&netlist, &UnitDelays, "top.u.", false)
            .with_sources(HashMap::from([(release, 0)]));
        assert_eq!(ripple.at(d3), Some(600));
    }

    #[test]
    fn settled_output_needs_every_completion_to_agree() {
        let mut and = Cell::new_comb(
            CellId(0),
            "AND2".into(),
            "test".into(),
            0.0,
            "a".into(),
            vec![GateNetId(1), GateNetId(2)],
            vec![GateNetId(3)],
        );
        and.function = Some(crate::tech_library::CellFunction::And2);
        assert_eq!(settled_output(&and, 0, &[Some(false), None]), Some(false));
        assert_eq!(settled_output(&and, 0, &[Some(true), None]), None);
        assert_eq!(
            settled_output(&and, 0, &[Some(true), Some(true)]),
            Some(true)
        );
        // the same AND as a LUT
        and.function = None;
        and.lut_init = Some(0x8);
        assert_eq!(settled_output(&and, 0, &[None, Some(false)]), Some(false));
        assert_eq!(settled_output(&and, 0, &[None, None]), None);
    }

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
        matched_delay_in(netlist, "top.u.", name, from, out);
    }

    /// A matched delay `out = from & buf(from)` named `name` in block `prefix`
    fn matched_delay_in(
        netlist: &mut GateNetlist,
        prefix: &str,
        name: &str,
        from: GateNetId,
        out: GateNetId,
    ) {
        let chained = netlist.add_net(GateNet::new(GateNetId(0), format!("{prefix}{name}_d0")));
        comb(
            netlist,
            &format!("{prefix}ncl_bundle.{name}_0"),
            vec![from],
            chained,
            true,
        );
        comb(
            netlist,
            &format!("{prefix}ncl_bundle.{name}_and"),
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

    /// The acknowledge leaves the block through a plain buffer (synthesis
    /// adds one where a kept cell's output is renamed to the port): it is
    /// still timed, as the buffered control
    #[test]
    fn a_buffered_acknowledge_is_timed() {
        let mut netlist = GateNetlist::new("top".into(), "test".into());
        let req = netlist.add_input("req".into());
        let x = netlist.add_input("x".into());
        let ack = netlist.add_output("ack".into());
        let y = netlist.add_output("y".into());
        let go = netlist.add_net(GateNet::new(GateNetId(0), "top.u.__ncl_go".into()));
        matched_delay(&mut netlist, "go", req, go);
        let mut buf = Cell::new_comb(
            CellId(0),
            "BUF".into(),
            "test".into(),
            0.0,
            "top.u.aig.phys_buf_ack".into(),
            vec![go],
            vec![ack],
        );
        buf.function = Some(CellFunction::Buf);
        netlist.add_cell(buf);
        let mut net = x;
        for k in 0..4 {
            let next = if k == 3 {
                y
            } else {
                netlist.add_net(GateNet::new(GateNetId(0), format!("top.u.n{k}")))
            };
            comb(&mut netlist, &format!("top.u.f{k}"), vec![net], next, false);
            net = next;
        }
        netlist.rebuild_net_connectivity();
        let checks = check_matched_delays(&netlist, &UnitDelays);
        let ack_check = checks
            .iter()
            .find(|c| c.times == "acknowledge")
            .expect("an acknowledge check");
        // go at 200, the buffer 100 later; y settles four gates after x
        assert_eq!((ack_check.data_ps, ack_check.control_ps), (400, 300));
    }

    /// A block holding its state in a master-slave pair of LUT latches
    /// (as `ncl_expand` builds with latch state): `go = req & delay(req)`,
    /// `master_open = !go & delay(!go)`, `slave_open = go & delay(go)`,
    /// `done = slave_open & delay(slave_open)`. The next state is the
    /// slave's output and the input `x` through five gates.
    fn latch_block() -> GateNetlist {
        use crate::gate_netlist::{
            LATCH_D_PIN, LATCH_EN_PIN, LATCH_LUT_INIT, LATCH_OP, LATCH_Q_PIN,
        };
        let mut netlist = GateNetlist::new("top".into(), "test".into());
        let req = netlist.add_input("req".into());
        let x = netlist.add_input("x".into());
        let ack = netlist.add_output("ack".into());
        let net =
            |n: &mut GateNetlist, name: &str| n.add_net(GateNet::new(GateNetId(0), name.into()));
        let go = net(&mut netlist, "top.u.__ncl_go");
        let go_n = net(&mut netlist, "top.u.__ncl_go_n");
        let mo = net(&mut netlist, "top.u.__ncl_master_open");
        let so = net(&mut netlist, "top.u.__ncl_slave_open");
        matched_delay(&mut netlist, "go", req, go);
        comb(&mut netlist, "top.u.go_n", vec![go], go_n, false);
        matched_delay(&mut netlist, "master_open", go_n, mo);
        matched_delay(&mut netlist, "slave_open", go, so);
        matched_delay(&mut netlist, "done", so, ack);

        let d = net(&mut netlist, "top.u.d");
        let mq = net(&mut netlist, "top.u.s__master");
        let sq = net(&mut netlist, "top.u.s");
        for (path, data, en, q) in [
            ("top.u.s.master.lat0", d, mo, mq),
            ("top.u.s.slave.lat0", mq, so, sq),
        ] {
            let mut inputs = vec![GateNetId(0); 3];
            inputs[LATCH_D_PIN] = data;
            inputs[LATCH_EN_PIN] = en;
            inputs[LATCH_Q_PIN] = q;
            let mut cell = Cell::new_comb(
                CellId(0),
                "LUT4".into(),
                "test".into(),
                0.0,
                path.into(),
                inputs,
                vec![q],
            );
            cell.lut_init = Some(LATCH_LUT_INIT);
            cell.source_op = Some(LATCH_OP.to_string());
            netlist.add_cell(cell);
        }
        let mut prev = net(&mut netlist, "top.u.n0");
        comb(&mut netlist, "top.u.mix", vec![sq, x], prev, false);
        for k in 1..5 {
            let next = if k == 4 {
                d
            } else {
                net(&mut netlist, &format!("top.u.n{k}"))
            };
            comb(
                &mut netlist,
                &format!("top.u.inc{k}"),
                vec![prev],
                next,
                false,
            );
            prev = next;
        }
        netlist.rebuild_net_connectivity();
        netlist
    }

    #[test]
    fn latch_blocks_are_timed_from_their_enables() {
        let netlist = latch_block();
        let checks = check_matched_delays(&netlist, &UnitDelays);
        let find = |times: &str| {
            checks
                .iter()
                .find(|c| c.times == times)
                .unwrap_or_else(|| panic!("a {times} check in {checks:#?}"))
        };
        // go rises at 200; !go falls at 300 and master_open at 400. The next
        // state from `x` settles at 500, plus 100 through the master.
        let setup = find("registers");
        assert!(setup.delay_path.ends_with("go_and"));
        assert_eq!((setup.data_ps, setup.control_ps), (600, 400));
        // After go rises: the master has closed at 300 (master_open falls
        // at 200, then 100 through the LUT); slave_open rises at 200
        let overlap = find(LATCH_OVERLAP);
        assert!(overlap.delay_path.ends_with("slave_open_and"));
        assert_eq!((overlap.data_ps, overlap.control_ps), (300, 200));
        // After go falls: slave_open falls at 100, the slave has closed at
        // 200; master_open rises at 300
        let reopen = find(LATCH_REOPEN);
        assert!(reopen.delay_path.ends_with("master_open_and"));
        assert_eq!((reopen.data_ps, reopen.control_ps), (200, 300));
        // The master has taken its data 100 after it reopens; the next go
        // can rise 100 after go falls (its chain), closing the master 200
        // after that
        let open = find(MASTER_OPEN);
        assert!(open.delay_path.ends_with("go_and"));
        assert_eq!((open.data_ps, open.control_ps), (400, 300));
        // The slave launches at 500 (slave_open at 400 + 100); the next
        // state reaches the master's input five gates later, plus 100
        // through it; done rises at 600
        let next = find("next state");
        assert!(next.delay_path.ends_with("done_and"));
        assert_eq!((next.data_ps, next.control_ps), (1100, 600));
    }

    #[test]
    fn sizing_makes_a_latch_block_meet_every_check() {
        let mut netlist = latch_block();
        let report = size_matched_delays(&mut netlist, &UnitDelays, 0);
        assert!(report.buffers_added > 0);
        for times in [
            "registers",
            LATCH_OVERLAP,
            LATCH_REOPEN,
            MASTER_OPEN,
            "next state",
        ] {
            let c = report
                .checks
                .iter()
                .find(|c| c.times == times)
                .unwrap_or_else(|| panic!("a {times} check"));
            assert!(c.slack_ps() >= 0, "{c:?}");
        }
    }

    /// A latch for a test block: `q = latch(en, d)` as one LUT
    fn test_latch(
        netlist: &mut GateNetlist,
        path: &str,
        d: GateNetId,
        en: GateNetId,
        q: GateNetId,
    ) {
        use crate::gate_netlist::{
            LATCH_D_PIN, LATCH_EN_PIN, LATCH_LUT_INIT, LATCH_OP, LATCH_Q_PIN,
        };
        let mut inputs = vec![GateNetId(0); 3];
        inputs[LATCH_D_PIN] = d;
        inputs[LATCH_EN_PIN] = en;
        inputs[LATCH_Q_PIN] = q;
        let mut cell = Cell::new_comb(
            CellId(0),
            "LUT4".into(),
            "test".into(),
            0.0,
            path.into(),
            inputs,
            vec![q],
        );
        cell.lut_init = Some(LATCH_LUT_INIT);
        cell.source_op = Some(LATCH_OP.to_string());
        netlist.add_cell(cell);
    }

    /// A block whose state is pulsed latches (`pulse_end = go & delay(go)`,
    /// `pulse = go & !pulse_end`): one latch per bit, or with `slaves`
    /// pulsed masters copied by slaves (`slave_open = go & delay(go)`).
    /// The next state is the state and the input `x` through five gates.
    fn pulse_block(slaves: bool) -> GateNetlist {
        let mut netlist = GateNetlist::new("top".into(), "test".into());
        let req = netlist.add_input("req".into());
        let x = netlist.add_input("x".into());
        let ack = netlist.add_output("ack".into());
        let net =
            |n: &mut GateNetlist, name: &str| n.add_net(GateNet::new(GateNetId(0), name.into()));
        let go = net(&mut netlist, "top.u.__ncl_go");
        let end = net(&mut netlist, "top.u.__ncl_pulse_end");
        let pulse = net(&mut netlist, "top.u.__ncl_pulse");
        matched_delay(&mut netlist, "go", req, go);
        matched_delay(&mut netlist, "pulse_end", go, end);
        comb(&mut netlist, "top.u.pulse", vec![go, end], pulse, false);
        let d = net(&mut netlist, "top.u.d");
        let q = net(&mut netlist, "top.u.s");
        if slaves {
            let so = net(&mut netlist, "top.u.__ncl_slave_open");
            matched_delay(&mut netlist, "slave_open", go, so);
            matched_delay(&mut netlist, "done", so, ack);
            let mq = net(&mut netlist, "top.u.s__master");
            test_latch(&mut netlist, "top.u.s.master.lat0", d, pulse, mq);
            test_latch(&mut netlist, "top.u.s.slave.lat0", mq, so, q);
        } else {
            matched_delay(&mut netlist, "done", end, ack);
            test_latch(&mut netlist, "top.u.s.latch.lat0", d, pulse, q);
        }
        let mut prev = net(&mut netlist, "top.u.n0");
        comb(&mut netlist, "top.u.mix", vec![q, x], prev, false);
        for k in 1..5 {
            let next = if k == 4 {
                d
            } else {
                net(&mut netlist, &format!("top.u.n{k}"))
            };
            comb(
                &mut netlist,
                &format!("top.u.inc{k}"),
                vec![prev],
                next,
                false,
            );
            prev = next;
        }
        netlist.rebuild_net_connectivity();
        netlist
    }

    fn find_check<'c>(checks: &'c [BundleCheck], times: &str) -> &'c BundleCheck {
        checks
            .iter()
            .find(|c| c.times == times)
            .unwrap_or_else(|| panic!("a {times} check in {checks:#?}"))
    }

    #[test]
    fn pulsed_latches_are_timed_from_their_pulse() {
        let netlist = pulse_block(false);
        let checks = check_matched_delays(&netlist, &UnitDelays);
        // go at 200; the pulse rises 100 after it and falls 100 after
        // pulse_end (at 400), so at 500
        let setup = find_check(&checks, "registers");
        assert_eq!((setup.data_ps, setup.control_ps), (600, 500));
        // Open at 300, the latch passes its data by 400
        let width = find_check(&checks, PULSE_WIDTH);
        assert!(width.delay_path.ends_with("pulse_end_and"));
        assert_eq!((width.data_ps, width.control_ps), (200, 300));
        // The new value (from 200 after go) is back at the latch's input
        // five gates later; the latch has closed at 400
        let race = find_check(&checks, LATCH_RACE);
        assert_eq!((race.data_ps, race.control_ps), (400, 700));
        // go's fall reaches the gate at once, pulse_end's 100 later
        let ret = find_check(&checks, PULSE_RETURN);
        assert_eq!((ret.data_ps, ret.control_ps), (0, 100));
        // The latch launches (at the latest) as the pulse ends; done at 600
        let next = find_check(&checks, "next state");
        assert_eq!((next.data_ps, next.control_ps), (1200, 600));
        assert!(checks.iter().all(|c| c.times != LATCH_OVERLAP));
    }

    #[test]
    fn pulsed_masters_close_before_their_slaves_open() {
        let netlist = pulse_block(true);
        let checks = check_matched_delays(&netlist, &UnitDelays);
        // Masters closed at 400 after go (pulse falls at 300, then 100
        // through), slaves open at 200
        let overlap = find_check(&checks, LATCH_OVERLAP);
        assert!(overlap.delay_path.ends_with("slave_open_and"));
        assert_eq!((overlap.data_ps, overlap.control_ps), (400, 200));
        // The masters read the slaves, closed while the pulse is up
        assert!(checks.iter().all(|c| c.times != LATCH_RACE));
        let mut netlist = netlist;
        let report = size_matched_delays(&mut netlist, &UnitDelays, 0);
        for times in [
            "registers",
            LATCH_OVERLAP,
            PULSE_WIDTH,
            PULSE_RETURN,
            "next state",
        ] {
            let c = find_check(&report.checks, times);
            assert!(c.slack_ps() >= 0, "{c:?}");
        }
    }

    /// In-block hold: the register's new value comes back to its own input
    /// five gates after it launches. With the strobe reaching the register
    /// late (clock skew) the new value can arrive before the strobe has:
    /// the check reports it, and sizing leaves the delays alone.
    #[test]
    fn register_hold_compares_the_new_value_with_the_strobe() {
        struct Skewed(i64);
        impl BundleDelays for Skewed {
            fn wire_ps(&self, _: &GateNetlist, _: GateNetId, _: &Cell, _: usize) -> i64 {
                0
            }
            fn clock_wire_ps(&self, _: &GateNetlist, _: GateNetId, _: &Cell) -> i64 {
                self.0
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
            fn hold_ps(&self, _: &Cell, _: usize) -> i64 {
                50
            }
        }
        let netlist = block_with_feedback();
        let hold = |delays: &dyn BundleDelays| {
            check_matched_delays(&netlist, delays)
                .into_iter()
                .find(|c| c.times == REGISTER_HOLD)
                .expect("a register hold check")
        };
        // no skew: the new value (100 + 500) comes long after the strobe (0 + 50)
        let ok = hold(&UnitDelays);
        assert_eq!((ok.data_ps, ok.control_ps), (0, 600));
        // the same strobe arrival everywhere cancels: skew only matters
        // between registers, here launch and capture share it
        let same = hold(&Skewed(1000));
        assert_eq!((same.data_ps, same.control_ps), (1050, 1600));
        assert!(same.slack_ps() > 0);
        let mut copy = netlist.clone();
        let report = size_matched_delays(&mut copy, &Skewed(1000), 0);
        assert_eq!(
            find_matched_delays(&copy).len(),
            find_matched_delays(&netlist).len()
        );
        let _ = report;
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

    /// Shortening keeps the chain's head and its AND-with-source stages, and
    /// the shorter chain is still found; it never drops the first stage
    #[test]
    fn shortened_chains_keep_their_head() {
        let mut netlist = block_with_feedback();
        let find_done = |netlist: &GateNetlist| {
            find_matched_delays(netlist)
                .into_iter()
                .find(|md| netlist.cells[md.and_cell.0 as usize].path.contains("done"))
                .unwrap()
        };
        let done = find_done(&netlist);
        extend_matched_delay(&mut netlist, &done, 5);
        let done = find_done(&netlist);
        assert_eq!(done.chain.len(), 6);
        let cells = netlist.cells.len();
        assert_eq!(shorten_matched_delay(&mut netlist, &done, 3), 3);
        assert_eq!(netlist.cells.len(), cells - 3);
        let done = find_done(&netlist);
        assert_eq!(done.chain.len(), 3);
        assert_eq!(
            netlist.cells[done.and_cell.0 as usize].inputs[1],
            netlist.cells[done.chain[2].0 as usize].outputs[0]
        );
        let before = check_matched_delays(&netlist, &UnitDelays);
        // Asking for more than the chain has leaves one stage
        assert_eq!(shorten_matched_delay(&mut netlist, &done, 10), 2);
        let done = find_done(&netlist);
        assert_eq!(done.chain.len(), 1);
        assert_eq!(shorten_matched_delay(&mut netlist, &done, 1), 0);
        // and the delay is shorter by the stages removed
        let after = check_matched_delays(&netlist, &UnitDelays);
        let control = |checks: &[BundleCheck]| {
            checks
                .iter()
                .find(|c| c.times == "next state")
                .map(|c| c.control_ps)
                .unwrap()
        };
        assert_eq!(control(&before) - control(&after), 200);
    }

    /// A self-timed block whose step request comes through an arbiter
    /// gate: `go` waits one buffer (100 ps) after it
    fn arbitrating_block() -> GateNetlist {
        self_timed_block(true)
    }

    /// A self-timed block, with or without an arbiter gate before its step
    /// request: `go` waits one buffer (100 ps) after the step request
    fn self_timed_block(arbiter: bool) -> GateNetlist {
        let mut netlist = GateNetlist::new("top".into(), "test".into());
        let req = netlist.add_input("req".into());
        let ack = netlist.add_output("ack".into());
        let grant = netlist.add_net(GateNet::new(GateNetId(0), "top.u.gnt".into()));
        let mut arb = Cell::new_comb(
            CellId(0),
            "LUT4".into(),
            "test".into(),
            0.0,
            "top.u.ncl_bundle.ready".into(),
            vec![req],
            vec![grant],
        );
        if arbiter {
            arb.path = "top.u.ncl_bundle.arb_nand0".into();
            arb.source_op = Some(ARBITER_OP.to_string());
        }
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
        // tau * ln(MTBF * T0 * f^2): about 12.5 ns with the defaults
        // (unmeasured: tau 300 ps)
        assert!((12_400..12_600).contains(&wait), "{wait}");
        assert!(!timing.measured);
        assert!(timing.mtbf_years_after(wait) >= timing.mtbf_years);
        assert!(timing.mtbf_years_after(wait - 100) < timing.mtbf_years);
        // ten tau more wait: e^10 times the MTBF
        let ratio = timing.mtbf_years_after(wait + 3_000) / timing.mtbf_years_after(wait);
        assert!((ratio / 10f64.exp() - 1.0).abs() < 1e-6, "{ratio}");
    }

    /// A measurement build sizes every decision wait to the length it asks
    /// for, whatever the device's figures, and never counts as signed off
    #[test]
    fn a_measurement_build_sizes_decisions_to_its_wait() {
        for wait in [450, 2_000, 9_000] {
            let timing = ArbiterTiming {
                measured: true,
                ..ArbiterTiming::default()
            }
            .sized_for_wait(wait);
            assert!(
                (wait..=wait + 1).contains(&timing.resolution_ps()),
                "{wait}"
            );
            assert!(!timing.measured);
        }
        let mut netlist = arbitrating_block();
        netlist.settle_wait_ps = Some(2_000);
        let checks = check_matched_delays(&netlist, &UnitDelays);
        let arb = checks
            .iter()
            .find(|c| c.times == "arbiter")
            .expect("arbiter");
        assert!((2_000..=2_001).contains(&arb.data_ps), "{}", arb.data_ps);
        assert!(arb.unmeasured_settle);
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

    /// A self-timed block's step request is a latch set by two independent
    /// inputs (`ncl_req` and `ready`), so `go` must wait out its decision
    /// even with no arbiter; a block stepped by its parent has no such latch
    #[test]
    fn step_decision_wait_is_checked_and_sized() {
        let mut netlist = self_timed_block(false);
        let checks = check_matched_delays(&netlist, &UnitDelays);
        assert!(checks.iter().all(|c| c.times != "arbiter"));
        let decision = checks
            .iter()
            .find(|c| c.times == "step decision")
            .expect("a step decision check");
        assert_eq!(decision.control_ps, 200);
        assert_eq!(decision.data_ps, ArbiterTiming::default().resolution_ps());
        assert!(decision.slack_ps() < 0);
        let report = size_matched_delays(&mut netlist, &UnitDelays, 0);
        let decision = report
            .checks
            .iter()
            .find(|c| c.times == "step decision")
            .expect("a step decision check after sizing");
        assert!(decision.slack_ps() >= 0, "{decision:?}");

        let parent_stepped = check_matched_delays(&block_with_feedback(), &UnitDelays);
        assert!(parent_stepped.iter().all(|c| c.times != "step decision"));
    }

    /// Unit delays with device figures for the step latch: a slower latch
    /// (`tau_ps`) than the arbiter default
    struct LatchDelays(f64);

    impl BundleDelays for LatchDelays {
        fn wire_ps(&self, n: &GateNetlist, net: GateNetId, c: &Cell, i: usize) -> i64 {
            UnitDelays.wire_ps(n, net, c, i)
        }
        fn clock_wire_ps(&self, n: &GateNetlist, net: GateNetId, c: &Cell) -> i64 {
            UnitDelays.clock_wire_ps(n, net, c)
        }
        fn arc_ps(&self, c: &Cell, i: usize, o: usize) -> Option<i64> {
            UnitDelays.arc_ps(c, i, o)
        }
        fn clock_to_out_ps(&self, c: &Cell) -> i64 {
            UnitDelays.clock_to_out_ps(c)
        }
        fn setup_ps(&self, c: &Cell, i: usize) -> i64 {
            UnitDelays.setup_ps(c, i)
        }
        fn step_latch_timing(&self) -> ArbiterTiming {
            ArbiterTiming {
                tau_ps: self.0,
                ..ArbiterTiming::default()
            }
        }
    }

    /// The step decision waits for the step latch's own figures; a block
    /// with arbiters waits for the slower of its latch and its arbiters
    #[test]
    fn step_decision_uses_the_step_latch_timing() {
        let latch = ArbiterTiming {
            tau_ps: 400.0,
            ..ArbiterTiming::default()
        };
        let checks = check_matched_delays(&self_timed_block(false), &LatchDelays(400.0));
        let decision = checks
            .iter()
            .find(|c| c.times == "step decision")
            .expect("a step decision check");
        assert_eq!(decision.data_ps, latch.resolution_ps());

        // slower latch than arbiter: the latch sets the wait
        let checks = check_matched_delays(&arbitrating_block(), &LatchDelays(400.0));
        let arb = checks
            .iter()
            .find(|c| c.times == "arbiter")
            .expect("arbiter");
        assert_eq!(arb.data_ps, latch.resolution_ps());
        // faster latch: the arbiter sets it
        let checks = check_matched_delays(&arbitrating_block(), &LatchDelays(30.0));
        let arb = checks
            .iter()
            .find(|c| c.times == "arbiter")
            .expect("arbiter");
        assert_eq!(arb.data_ps, ArbiterTiming::default().resolution_ps());
    }

    /// A decision wait sized from assumed settle-time figures is marked so
    /// the report can say it is not signed off; one from measured figures is
    /// not
    #[test]
    fn decisions_without_a_measured_settle_time_are_marked() {
        struct Measured;
        impl BundleDelays for Measured {
            fn wire_ps(&self, n: &GateNetlist, net: GateNetId, c: &Cell, i: usize) -> i64 {
                UnitDelays.wire_ps(n, net, c, i)
            }
            fn clock_wire_ps(&self, n: &GateNetlist, net: GateNetId, c: &Cell) -> i64 {
                UnitDelays.clock_wire_ps(n, net, c)
            }
            fn arc_ps(&self, c: &Cell, i: usize, o: usize) -> Option<i64> {
                UnitDelays.arc_ps(c, i, o)
            }
            fn clock_to_out_ps(&self, c: &Cell) -> i64 {
                UnitDelays.clock_to_out_ps(c)
            }
            fn setup_ps(&self, c: &Cell, i: usize) -> i64 {
                UnitDelays.setup_ps(c, i)
            }
            fn arbiter_timing(&self) -> ArbiterTiming {
                ArbiterTiming {
                    tau_ps: 90.0,
                    measured: true,
                    ..ArbiterTiming::default()
                }
            }
        }
        let netlist = arbitrating_block();
        let decisions = |checks: Vec<BundleCheck>| -> Vec<bool> {
            checks
                .into_iter()
                .filter(|c| c.times == "arbiter" || c.times == "step decision")
                .map(|c| c.unmeasured_settle)
                .collect()
        };
        let assumed = decisions(check_matched_delays(&netlist, &UnitDelays));
        assert!(!assumed.is_empty() && assumed.iter().all(|&u| u));
        let measured = decisions(check_matched_delays(&netlist, &Measured));
        assert!(!measured.is_empty() && measured.iter().all(|&u| !u));
        // Other checks never are
        assert!(check_matched_delays(&netlist, &UnitDelays)
            .iter()
            .filter(|c| c.times != "arbiter" && c.times != "step decision")
            .all(|c| !c.unmeasured_settle));
    }

    /// The step control and `go` delay of a self-timed block `prefix`
    /// whose `ready` is `ready`; returns `go`
    fn step_and_go(netlist: &mut GateNetlist, prefix: &str, ready: GateNetId) -> GateNetId {
        let step = netlist.add_net(GateNet::new(
            GateNetId(0),
            format!("{prefix}__ncl_step_req"),
        ));
        let go = netlist.add_net(GateNet::new(GateNetId(0), format!("{prefix}__ncl_go")));
        let mut st = Cell::new_comb(
            CellId(0),
            "LUT4".into(),
            "test".into(),
            0.0,
            format!("{prefix}ncl_bundle.step.st_latch"),
            vec![ready, go],
            vec![step],
        );
        st.source_op = Some(STEP_CONTROL_OP.to_string());
        netlist.add_cell(st);
        matched_delay_in(netlist, prefix, "go", step, go);
        go
    }

    fn register(
        netlist: &mut GateNetlist,
        path: &str,
        d: GateNetId,
        q: GateNetId,
        clock: GateNetId,
    ) {
        netlist.add_cell(Cell::new_seq(
            CellId(0),
            "FD1P3IX".into(),
            "test".into(),
            0.0,
            path.into(),
            vec![d],
            vec![q],
            clock,
            None,
        ));
    }

    /// A channel from self-timed block `top.a.` to `top.b.`: `a` sends its
    /// register `data` once `b`'s phase register `ack` (stepped by `b`'s
    /// `go`) has toggled; `b` captures `data` in its register `take`.
    fn channel() -> GateNetlist {
        let mut netlist = GateNetlist::new("top".into(), "test".into());
        let run = netlist.add_input("run".into());
        let ack_q = netlist.add_net(GateNet::new(GateNetId(0), "top.b.ack".into()));
        let ready_a = netlist.add_net(GateNet::new(GateNetId(0), "top.a.__ncl_ready".into()));
        comb(&mut netlist, "top.a.ready", vec![ack_q], ready_a, false);
        let go_a = step_and_go(&mut netlist, "top.a.", ready_a);
        let data_q = netlist.add_net(GateNet::new(GateNetId(0), "top.a.data".into()));
        let data_d = netlist.add_net(GateNet::new(GateNetId(0), "top.a.data_d".into()));
        comb(&mut netlist, "top.a.next", vec![data_q], data_d, false);
        register(&mut netlist, "top.a.data_reg", data_d, data_q, go_a);

        let go_b = step_and_go(&mut netlist, "top.b.", run);
        let ack_d = netlist.add_net(GateNet::new(GateNetId(0), "top.b.ack_d".into()));
        comb(&mut netlist, "top.b.toggle", vec![ack_q], ack_d, false);
        register(&mut netlist, "top.b.ack_reg", ack_d, ack_q, go_b);
        let take_q = netlist.add_net(GateNet::new(GateNetId(0), "top.b.take".into()));
        register(&mut netlist, "top.b.take_reg", data_q, take_q, go_b);
        netlist.rebuild_net_connectivity();
        netlist
    }

    /// Unit delays, with the receiver's strobe reaching its data register
    /// `late_ps` late and a hold time of `hold_ps`
    struct SkewedDelays {
        late_ps: i64,
        hold_ps: i64,
    }

    impl BundleDelays for SkewedDelays {
        fn wire_ps(&self, _: &GateNetlist, _: GateNetId, _: &Cell, _: usize) -> i64 {
            0
        }
        fn clock_wire_ps(&self, _: &GateNetlist, _: GateNetId, c: &Cell) -> i64 {
            if c.path == "top.b.take_reg" {
                self.late_ps
            } else {
                0
            }
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
        fn hold_ps(&self, _: &Cell, _: usize) -> i64 {
            self.hold_ps
        }
    }

    #[test]
    fn channel_hold_is_timed_round_the_handshake() {
        let netlist = channel();
        let delays = SkewedDelays {
            late_ps: 0,
            hold_ps: 50,
        };
        let checks = check_matched_delays(&netlist, &delays);
        let hold: Vec<_> = checks
            .iter()
            .filter(|c| c.times == "channel hold")
            .collect();
        assert_eq!(hold.len(), 1, "{checks:?}");
        let hold = hold[0];
        assert_eq!(hold.prefix, "top.b.");
        assert_eq!(hold.sender.as_deref(), Some("top.a."));
        assert!(hold.delay_path.starts_with("top.a."));
        // b's ack launches at 100, a's ready at 200, its step request at
        // 300, `go` (both inputs of the matched delay's AND) at 500, `data`
        // at 600; b's register holds until 50
        assert_eq!(hold.control_ps, 600);
        assert_eq!(hold.data_ps, 50);
    }

    #[test]
    fn channel_hold_with_a_late_strobe_lengthens_the_sender() {
        let mut netlist = channel();
        let delays = SkewedDelays {
            late_ps: 1000,
            hold_ps: 50,
        };
        let hold = |netlist: &GateNetlist| {
            check_matched_delays(netlist, &delays)
                .into_iter()
                .find(|c| c.times == "channel hold")
                .expect("a channel hold check")
        };
        let before = hold(&netlist);
        assert_eq!(before.slack_ps(), 600 - 1050);
        let report = size_matched_delays(&mut netlist, &delays, 0);
        assert!(report.buffers_added > 0);
        let after = hold(&netlist);
        assert!(after.slack_ps() >= 0, "{after:?}");
        assert!(after.chain_len > before.chain_len);
    }
}
