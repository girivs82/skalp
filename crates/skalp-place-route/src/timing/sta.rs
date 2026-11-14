//! Family-independent static timing analysis of a placed and routed design.
//!
//! The engine walks routed nets and combinational cells to find the longest
//! register-to-register path. What a delay is comes from a [`TimingModel`],
//! which each FPGA family supplies from its own timing data.
//!
//! Each clock is checked against its own frequency ([`ClockTargets`]), with
//! the clock's arrival at every register taken from its routed clock net, so
//! skew counts. [`analyze`] checks setup with a slow-corner model;
//! [`analyze_hold`] checks hold with a fast-corner one. Paths between clocks
//! are not timed. Setup times paths from input pads as launched at the clock
//! source; hold does not time them.

use super::clock_targets::ClockTargets;
use super::{ClockSummary, CriticalPath, HoldReport, PathStep, TimingReport};
use crate::bitstream::config::PlacedDesign;
use crate::collections::{HashMap, HashMapExt, HashSet, HashSetExt};
use crate::device::{CellPin, Device, PipId, WireId, FF_PIN_CLK};
use skalp_lir::gate_netlist::{Cell, CellId, GateNetId};
use std::collections::BTreeMap;

const PS_PER_NS: f64 = 1000.0;

/// Arrival of a net no timed path reaches, in early (hold) analysis: far
/// enough out that adding path delays to it stays far out.
const NO_PATH: i64 = i64::MAX / 4;

/// An input a clocked cell samples.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TimingEndpoint {
    /// Index into the cell's inputs
    pub input: usize,
    /// The router pin that input arrives on
    pub pin: usize,
    /// Setup time in picoseconds
    pub setup_ps: i64,
    /// Hold time in picoseconds
    pub hold_ps: i64,
}

/// One FPGA family's delays.
pub trait TimingModel {
    /// Delay through a routing switch, in picoseconds.
    fn interconnect_ps(&self, pip: PipId) -> i64;

    /// When an output is launched by a clock edge or has no timed inputs: the
    /// clock-to-output delay, or 0 for pads and constants. `None` if the
    /// output is a combinational function of the cell's inputs.
    fn launch_ps(&self, cell: &Cell, output: usize) -> Option<i64>;

    /// Combinational delay from an input to an output. `None` if there is no
    /// path between them.
    fn arc_ps(&self, cell: &Cell, input: usize, output: usize) -> Option<i64>;

    /// Inputs a clocked cell samples, with their setup times.
    fn endpoints(&self, cell: &Cell) -> Vec<TimingEndpoint>;

    /// Router pins an input reaches. Most inputs have one.
    fn input_pins(&self, _cell: &Cell, input: usize) -> Vec<usize> {
        vec![input]
    }

    /// Whether an input arrives on dedicated wiring the router doesn't handle.
    fn is_dedicated(&self, _cell: &Cell, _input: usize) -> bool {
        false
    }
}

/// Worst arrival at a net's driver output: the latest in setup analysis, the
/// earliest in hold analysis.
#[derive(Debug, Clone, Copy)]
struct Arrival {
    time: i64,
    /// The net on the driver's input that sets the arrival
    from: Option<GateNetId>,
    /// Routing delay into the driver on that input
    route_ps: i64,
    /// Delay through the driver from that input, or its launch delay
    cell_ps: i64,
}

struct Engine<'a, D: Device + ?Sized> {
    device: &'a D,
    model: &'a dyn TimingModel,
    design: &'a PlacedDesign<'a>,
    /// Per-net map from a routed wire to the pip that drives it
    drivers: HashMap<GateNetId, HashMap<WireId, PipId>>,
    /// Worst arrival time at each net, and the input that set it
    arrivals: HashMap<GateNetId, Arrival>,
    visiting: HashSet<GateNetId>,
    /// Earliest arrivals (hold) rather than latest (setup)
    early: bool,
    /// Cells of bundled-data async blocks: clocked timing does not cross
    /// them (their own matched delays time them, see [`check_bundles`])
    async_cells: HashSet<CellId>,
}

impl<D: Device + ?Sized> Engine<'_, D> {
    /// Routing delay of a net from its source to one input pin of a cell.
    fn route_delay(&mut self, net: GateNetId, cell: CellId, pin: usize) -> i64 {
        let Some(loc) = self.design.placement.placements.get(&cell) else {
            return 0;
        };
        let Some(mut wire) =
            self.device
                .bel_pin_wire(loc.tile_x, loc.tile_y, loc.bel_index, CellPin::Input(pin))
        else {
            return 0;
        };
        let Some(route) = self.design.routing.routes.get(&net) else {
            return 0;
        };
        let device = self.device;
        let drivers = self.drivers.entry(net).or_insert_with(|| {
            route
                .pips
                .iter()
                .filter_map(|&p| device.pip(p).map(|info| (info.dst_wire, p)))
                .collect()
        });
        let mut total = 0;
        let mut hops = 0;
        while let Some(&pip) = drivers.get(&wire) {
            total += self.model.interconnect_ps(pip);
            let Some(info) = device.pip(pip) else { break };
            wire = info.src_wire;
            hops += 1;
            if hops > 10_000 {
                break;
            }
        }
        total
    }

    /// Arrival time where nothing timed launches a path: time zero for
    /// setup, which times such inputs from the clock source; none for hold.
    fn untimed(&self) -> i64 {
        if self.early {
            NO_PATH
        } else {
            0
        }
    }

    /// When a cell's clock reaches it, from the clock net's source.
    fn clock_arrival(&mut self, cell: &Cell) -> i64 {
        cell.clock
            .map_or(0, |clock| self.route_delay(clock, cell.id, FF_PIN_CLK))
    }

    fn net_arrival(&mut self, net: GateNetId) -> i64 {
        if let Some(arrival) = self.arrivals.get(&net) {
            return arrival.time;
        }
        if !self.visiting.insert(net) {
            return self.untimed();
        }
        let netlist = self.design.netlist;
        let info = &netlist.nets[net.0 as usize];
        let driver = info.driver.and_then(|id| netlist.get_cell(id));
        let output = info.driver_pin.unwrap_or(0);
        let arrival = match driver {
            // An async block's output: its handshake, not this clock, says
            // when it is read
            Some(cell) if self.async_cells.contains(&cell.id) => Arrival {
                time: self.untimed(),
                from: None,
                route_ps: 0,
                cell_ps: 0,
            },
            Some(cell) => self.output_time(cell, output),
            None => Arrival {
                time: self.untimed(),
                from: None,
                route_ps: 0,
                cell_ps: 0,
            },
        };
        self.visiting.remove(&net);
        self.arrivals.insert(net, arrival);
        arrival.time
    }

    /// Arrival of an input at the cell (latest, or earliest in hold
    /// analysis) and the routing delay part of it, or `None` if it is not
    /// timed.
    fn input_time(&mut self, cell: &Cell, input: usize) -> Option<(i64, GateNetId, i64)> {
        let net = *cell.inputs.get(input)?;
        if self.design.tied.value(cell.id, input).is_some() {
            return None;
        }
        let arrival = self.net_arrival(net);
        if arrival >= NO_PATH {
            return None;
        }
        if self.model.is_dedicated(cell, input) {
            return Some((arrival, net, 0));
        }
        let delays = self
            .model
            .input_pins(cell, input)
            .into_iter()
            .map(|pin| self.route_delay(net, cell.id, pin))
            .collect::<Vec<_>>();
        let routing = if self.early {
            delays.into_iter().min()
        } else {
            delays.into_iter().max()
        }
        .unwrap_or(0);
        Some((arrival + routing, net, routing))
    }

    fn output_time(&mut self, cell: &Cell, output: usize) -> Arrival {
        if let Some(launch) = self.model.launch_ps(cell, output) {
            // Pads and constants launch nothing hold has to check
            if self.early && cell.clock.is_none() {
                return Arrival {
                    time: NO_PATH,
                    from: None,
                    route_ps: 0,
                    cell_ps: 0,
                };
            }
            // A register launches when its clock reaches it: the clock's
            // routing shows as the routing into the launching cell.
            let clock = self.clock_arrival(cell);
            return Arrival {
                time: clock + launch,
                from: None,
                route_ps: clock,
                cell_ps: launch,
            };
        }
        let mut best = Arrival {
            time: self.untimed(),
            from: None,
            route_ps: 0,
            cell_ps: 0,
        };
        for input in 0..cell.inputs.len() {
            let Some(delay) = self.model.arc_ps(cell, input, output) else {
                continue;
            };
            if let Some((time, net, route_ps)) = self.input_time(cell, input) {
                let candidate = time + delay;
                let better = if self.early {
                    candidate < best.time
                } else {
                    candidate > best.time
                };
                if better {
                    best = Arrival {
                        time: candidate,
                        from: Some(net),
                        route_ps,
                        cell_ps: delay,
                    };
                }
            }
        }
        best
    }

    /// The nets on the worst path into `net`, from its start.
    fn nets_through(&self, mut net: GateNetId) -> Vec<GateNetId> {
        let mut nets = vec![net];
        let mut seen = HashSet::from_iter([net]);
        while let Some(Arrival {
            from: Some(previous),
            ..
        }) = self.arrivals.get(&net).copied()
        {
            if !seen.insert(previous) {
                break;
            }
            nets.push(previous);
            net = previous;
        }
        nets.reverse();
        nets
    }

    fn path_steps(&self, nets: &[GateNetId]) -> Vec<PathStep> {
        let netlist = self.design.netlist;
        nets.iter()
            .map(|&net| {
                let info = &netlist.nets[net.0 as usize];
                let driver = info.driver.and_then(|id| netlist.get_cell(id));
                let arrival = self.arrivals.get(&net).copied();
                PathStep {
                    net: info.name.clone(),
                    cell: driver.map(|c| c.path.clone()).unwrap_or_default(),
                    cell_type: driver.map(|c| c.cell_type.clone()).unwrap_or_default(),
                    tile: driver
                        .and_then(|c| self.design.placement.placements.get(&c.id))
                        .map(|loc| (loc.tile_x, loc.tile_y)),
                    arrival: arrival.map_or(0.0, |a| a.time as f64 / PS_PER_NS),
                    route_in: arrival.map_or(0.0, |a| a.route_ps as f64 / PS_PER_NS),
                    cell_delay: arrival.map_or(0.0, |a| a.cell_ps as f64 / PS_PER_NS),
                }
            })
            .collect()
    }
}

/// Analyze a placed and routed design against a clock frequency in MHz.
pub fn analyze<D: Device + ?Sized>(
    device: &D,
    model: &dyn TimingModel,
    design: &PlacedDesign<'_>,
    targets: &ClockTargets,
) -> TimingReport {
    let mut engine = Engine {
        device,
        model,
        design,
        drivers: HashMap::new(),
        arrivals: HashMap::new(),
        visiting: HashSet::new(),
        early: false,
        async_cells: async_block_cells(design.netlist),
    };

    let netlist = design.netlist;
    let mut per_clock: BTreeMap<u32, (usize, f64)> = BTreeMap::new();
    let mut paths = Vec::new();

    for cell in &netlist.cells {
        let Some(clock) = cell.clock else { continue };
        if engine.async_cells.contains(&cell.id) {
            continue;
        }
        let target_ns = targets.required_ns(clock);
        let entry = per_clock.entry(clock.0).or_insert((0, 0.0));
        entry.0 += 1;
        let capture = engine.clock_arrival(cell);
        for endpoint in model.endpoints(cell) {
            let Some(routed) = engine.input_time_at(cell, endpoint) else {
                continue;
            };
            let (time, net) = routed;
            // From the capturing edge at the clock's source
            let delay_ns = (time + endpoint.setup_ps - capture) as f64 / PS_PER_NS;
            let worst = per_clock.get_mut(&clock.0).expect("present");
            worst.1 = worst.1.max(delay_ns);
            let nets = engine.nets_through(net);
            let path = nets
                .iter()
                .map(|n| netlist.nets[n.0 as usize].name.clone())
                .collect::<Vec<_>>();
            paths.push((
                CriticalPath {
                    delay: delay_ns,
                    required: target_ns,
                    slack: target_ns - delay_ns,
                    source: path.first().cloned().unwrap_or_default(),
                    destination: cell.path.clone(),
                    path,
                    steps: Vec::new(),
                },
                nets,
                clock,
            ));
        }
    }

    // Worst slack first: with clocks at different frequencies, the longest
    // path need not be the one that fails by most.
    paths.sort_by(|a, b| a.0.slack.total_cmp(&b.0.slack));
    let failing_paths = paths.iter().filter(|p| p.0.slack < 0.0).count();
    let worst = paths.first().map(|p| (p.0.delay, p.0.slack, p.2));
    paths.truncate(10);
    let paths: Vec<CriticalPath> = paths
        .into_iter()
        .map(|(mut path, nets, _)| {
            path.steps = engine.path_steps(&nets);
            path
        })
        .collect();

    let clock_summaries = per_clock
        .iter()
        .map(|(&net, &(registers, delay))| {
            let clock = GateNetId(net);
            ClockSummary {
                name: netlist.nets[net as usize].name.clone(),
                frequency: if delay > 0.0 {
                    PS_PER_NS / (delay + targets.uncertainty_ns)
                } else {
                    targets.mhz(clock)
                },
                register_count: registers,
                worst_slack: targets.required_ns(clock) - delay,
            }
        })
        .collect();

    // The report's headline is the clock with the worst slack. Its
    // frequencies count the uncertainty in the period, as the check does.
    let (target_frequency, design_frequency, worst_slack) = match worst {
        Some((delay, slack, clock)) if delay > 0.0 => (
            targets.mhz(clock),
            PS_PER_NS / (delay + targets.uncertainty_ns),
            slack,
        ),
        _ => (targets.default_mhz, targets.default_mhz, 0.0),
    };
    TimingReport {
        target_frequency,
        design_frequency,
        worst_negative_slack: (-worst_slack).max(0.0),
        failing_paths,
        meets_timing: failing_paths == 0,
        clock_summaries,
        critical_paths: paths,
        hold: None,
        bundles: Vec::new(),
    }
}

/// Hold check of a placed and routed design: the earliest each register
/// input can change after a clock edge, against the time it must hold after
/// that edge reaches the register. `model` gives the fast corner's delays.
pub fn analyze_hold<D: Device + ?Sized>(
    device: &D,
    model: &dyn TimingModel,
    design: &PlacedDesign<'_>,
    targets: &ClockTargets,
) -> HoldReport {
    let mut engine = Engine {
        device,
        model,
        design,
        drivers: HashMap::new(),
        arrivals: HashMap::new(),
        visiting: HashSet::new(),
        early: true,
        async_cells: async_block_cells(design.netlist),
    };
    let margin_ps = targets.hold_margin_ns * PS_PER_NS;
    let mut paths = Vec::new();
    for cell in &design.netlist.cells {
        if cell.clock.is_none() || engine.async_cells.contains(&cell.id) {
            continue;
        }
        let capture = engine.clock_arrival(cell);
        for endpoint in model.endpoints(cell) {
            let Some((time, net)) = engine.input_time_at(cell, endpoint) else {
                continue;
            };
            let delay = (time - capture) as f64 / PS_PER_NS;
            let required = (endpoint.hold_ps as f64 + margin_ps) / PS_PER_NS;
            paths.push((delay, required, net, cell));
        }
    }
    paths.sort_by(|a, b| (a.0 - a.1).total_cmp(&(b.0 - b.1)));
    let violations = paths.iter().filter(|p| p.0 < p.1).count();
    let worst_slack = paths.first().map_or(0.0, |p| p.0 - p.1);
    let netlist = design.netlist;
    let paths = paths
        .into_iter()
        .take(10)
        .map(|(delay, required, net, cell)| {
            let nets = engine.nets_through(net);
            let path: Vec<String> = nets
                .iter()
                .map(|n| netlist.nets[n.0 as usize].name.clone())
                .collect();
            CriticalPath {
                delay,
                required,
                slack: delay - required,
                source: path.first().cloned().unwrap_or_default(),
                destination: cell.path.clone(),
                path,
                steps: engine.path_steps(&nets),
            }
        })
        .collect();
    HoldReport {
        worst_slack,
        violations,
        paths,
    }
}

impl<D: Device + ?Sized> Engine<'_, D> {
    /// Arrival at an endpoint, on the router pin that endpoint names.
    fn input_time_at(&mut self, cell: &Cell, endpoint: TimingEndpoint) -> Option<(i64, GateNetId)> {
        let net = *cell.inputs.get(endpoint.input)?;
        if self.design.tied.value(cell.id, endpoint.input).is_some() {
            return None;
        }
        let arrival = self.net_arrival(net);
        if arrival >= NO_PATH {
            return None;
        }
        Some((arrival + self.route_delay(net, cell.id, endpoint.pin), net))
    }
}

/// Routed delays for checking the matched delays of bundled-data async
/// blocks (see `skalp_lir::ncl_bundle`).
struct RoutedBundleDelays<'e, 'a, D: Device + ?Sized> {
    engine: std::cell::RefCell<&'e mut Engine<'a, D>>,
    model: &'a dyn TimingModel,
}

impl<D: Device + ?Sized> skalp_lir::ncl_bundle::BundleDelays for RoutedBundleDelays<'_, '_, D> {
    fn wire_ps(
        &self,
        _netlist: &skalp_lir::gate_netlist::GateNetlist,
        net: GateNetId,
        cell: &Cell,
        input: usize,
    ) -> i64 {
        if self.model.is_dedicated(cell, input) {
            return 0;
        }
        let mut engine = self.engine.borrow_mut();
        self.model
            .input_pins(cell, input)
            .into_iter()
            .map(|pin| engine.route_delay(net, cell.id, pin))
            .max()
            .unwrap_or(0)
    }

    fn clock_wire_ps(
        &self,
        _netlist: &skalp_lir::gate_netlist::GateNetlist,
        net: GateNetId,
        cell: &Cell,
    ) -> i64 {
        self.engine
            .borrow_mut()
            .route_delay(net, cell.id, FF_PIN_CLK)
    }

    fn arc_ps(&self, cell: &Cell, input: usize, output: usize) -> Option<i64> {
        self.model.arc_ps(cell, input, output)
    }

    fn clock_to_out_ps(&self, cell: &Cell) -> i64 {
        self.model.launch_ps(cell, 0).unwrap_or(0)
    }

    fn launches(&self, cell: &Cell, output: usize) -> bool {
        cell.clock.is_some() && self.model.launch_ps(cell, output).is_some()
    }

    fn setup_ps(&self, cell: &Cell, input: usize) -> i64 {
        self.model
            .endpoints(cell)
            .iter()
            .find(|e| e.input == input)
            .map_or(0, |e| e.setup_ps)
    }
}

/// Check the matched delays of bundled-data async blocks against routed
/// delays from `model` (one corner).
pub fn check_bundles<D: Device + ?Sized>(
    device: &D,
    model: &dyn TimingModel,
    design: &PlacedDesign<'_>,
) -> Vec<skalp_lir::ncl_bundle::BundleCheck> {
    let mut engine = Engine {
        device,
        model,
        design,
        drivers: HashMap::new(),
        arrivals: HashMap::new(),
        visiting: HashSet::new(),
        early: false,
        async_cells: HashSet::new(),
    };
    let delays = RoutedBundleDelays {
        engine: std::cell::RefCell::new(&mut engine),
        model,
    };
    skalp_lir::ncl_bundle::check_matched_delays(design.netlist, &delays)
}

/// Worst-corner summary of bundle checks made at several named corners
pub fn summarize_bundles(
    corners: &[(&str, Vec<skalp_lir::ncl_bundle::BundleCheck>)],
) -> Vec<super::BundleReport> {
    let mut by_key: BTreeMap<(String, &'static str), super::BundleReport> = BTreeMap::new();
    for (corner, checks) in corners {
        for c in checks {
            let slack_ns = c.slack_ps() as f64 / PS_PER_NS;
            let key = (c.delay_path.clone(), c.times);
            let worse = by_key.get(&key).is_none_or(|r| slack_ns < r.slack_ns);
            if worse {
                by_key.insert(
                    key,
                    super::BundleReport {
                        block: c.prefix.trim_end_matches('.').to_string(),
                        delay: c.delay_path.clone(),
                        times: c.times.to_string(),
                        data_ns: c.data_ps as f64 / PS_PER_NS,
                        control_ns: c.control_ps as f64 / PS_PER_NS,
                        slack_ns,
                        chain_len: c.chain_len,
                        corner: corner.to_string(),
                    },
                );
            }
        }
    }
    by_key.into_values().collect()
}

/// Cells of the bundled-data async blocks of a netlist (found from their
/// matched delays)
fn async_block_cells(netlist: &skalp_lir::gate_netlist::GateNetlist) -> HashSet<CellId> {
    let prefixes: Vec<String> = skalp_lir::ncl_bundle::find_matched_delays(netlist)
        .into_iter()
        .map(|m| m.prefix)
        .collect();
    let mut cells = HashSet::new();
    if prefixes.is_empty() {
        return cells;
    }
    for cell in &netlist.cells {
        if prefixes.iter().any(|p| cell.path.starts_with(p.as_str())) {
            cells.insert(cell.id);
        }
    }
    cells
}
