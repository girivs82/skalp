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
//! [`check_matched_delays`] measures both against a delay model: library
//! estimates before placement ([`LibraryDelays`]), routed delays after (the
//! place-and-route crate supplies its own [`BundleDelays`]).
//! [`size_matched_delays`] lengthens each chain until its checks pass.
//!
//! All times are measured from the block's inputs, which the clocked parent
//! holds stable from before it raises the request; the request itself is
//! one of those inputs.

use crate::gate_netlist::{Cell, CellId, GateNet, GateNetId, GateNetlist, MATCHED_DELAY_OP};
use crate::tech_library::{CellFunction, TechLibrary};
use std::collections::{HashMap, HashSet};

/// One matched delay found in a netlist
#[derive(Debug, Clone)]
pub struct MatchedDelay {
    /// Hierarchical prefix of the async block it belongs to (`top.u.`)
    pub prefix: String,
    /// The kept AND whose output is the timed control signal
    pub and_cell: CellId,
    /// The chain's buffers, from the source to the AND
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
}

/// Result of timing one matched delay
#[derive(Debug, Clone)]
pub struct BundleCheck {
    /// Block prefix (`top.u.`)
    pub prefix: String,
    /// Path of the matched delay's AND cell
    pub delay_path: String,
    /// What the control times: "registers", "acknowledge" or "next state"
    pub times: &'static str,
    /// Latest data arrival plus setup, in picoseconds
    pub data_ps: i64,
    /// Arrival of the control signal, in picoseconds
    pub control_ps: i64,
    /// Buffers in the chain
    pub chain_len: usize,
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
            let Some(buf) = netlist.cells.get(driver.0 as usize) else {
                break;
            };
            if !buf.is_kept() || buf.inputs.len() != 1 {
                break;
            }
            chain.push(buf.id);
            net = buf.inputs[0];
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
        }
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
            // data path
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
            None => Some(0),
            Some(driver) => {
                let cell = &netlist.cells[driver.0 as usize];
                if !self.inside(cell) {
                    // A block input: held stable by the parent
                    Some(0)
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
                    let mut best: Option<i64> = None;
                    for (i, &input) in cell.inputs.iter().enumerate() {
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
            });
        }

        // The acknowledge: the block output this control drives (directly or
        // through plain buffers). Matched delays that feed another matched
        // delay time registers only.
        let Some(ack) = block_output_driven_by(netlist, md.control, &md.prefix) else {
            continue;
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
            });
        }
    }
    checks
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

/// Add `count` buffers to a matched delay's chain, just before its AND
pub fn extend_matched_delay(netlist: &mut GateNetlist, md: &MatchedDelay, count: usize) {
    if count == 0 {
        return;
    }
    let template = match md.chain.last() {
        Some(&id) => netlist.cells[id.0 as usize].clone(),
        None => return,
    };
    let and_idx = md.and_cell.0 as usize;
    let mut net = netlist.cells[and_idx].inputs[1];
    let base = template
        .path
        .rsplit_once('_')
        .map_or(template.path.as_str(), |(b, _)| b)
        .to_string();
    let first = md.chain.len();
    for k in 0..count {
        let out = netlist.add_net(GateNet::new(
            GateNetId(0),
            format!("{}_n{}", base, first + k),
        ));
        let mut buf = template.clone();
        buf.id = CellId(0);
        buf.path = format!("{}_{}", base, first + k);
        buf.inputs = vec![net];
        buf.outputs = vec![out];
        netlist.add_cell(buf);
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
            if worst <= 0 {
                continue;
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
}
