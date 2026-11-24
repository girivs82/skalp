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
use super::{ClockSummary, CriticalPath, FailingInstance, HoldReport, PathStep, TimingReport};
use crate::bitstream::config::PlacedDesign;
use crate::collections::{HashMap, HashMapExt, HashSet, HashSetExt};
use crate::device::{CellPin, Device, PipId, WireId, FF_PIN_CLK};
use skalp_lir::gate_netlist::{Cell, CellId, GateNetId};
use std::collections::{BTreeMap, BTreeSet};

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
    /// The cell input whose clock samples it, for a block with several
    /// clocks (a DDR gearbox takes data on its slow clock); `None`: the
    /// cell's own clock
    pub clock: Option<usize>,
}

/// How fast a storage element built in the fabric leaves a metastable state
/// (see `skalp_lir::ncl_bundle::ArbiterTiming`), from the device database
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Metastability {
    /// Resolution time constant, in picoseconds, margins included
    pub tau_ps: f64,
    /// Metastability window, in picoseconds
    pub t0_ps: f64,
}

/// Device-database name of the element a self-timed block's step latch and
/// its arbiters' mutual exclusions are built as where the library has LUTs:
/// one 4-input LUT with its output fed back
/// (`skalp_lir::gate_netlist::STEP_LATCH_INIT`,
/// `skalp_lir::gate_netlist::ARBITER_MUTEX_INIT`)
pub const STEP_LATCH_ELEMENT: &str = "lut4_feedback_latch";

/// One FPGA family's delays.
pub trait TimingModel {
    /// Delay through a routing switch, in picoseconds.
    fn interconnect_ps(&self, pip: PipId) -> i64;

    /// The delay class of a routing switch the model has no delay for, which
    /// [`interconnect_ps`](Self::interconnect_ps) then times as 0 ps: its
    /// class name, or its wire names where it has no class. `None` where the
    /// model times the switch.
    fn untimed_pip(&self, _pip: PipId) -> Option<String> {
        None
    }

    /// When an output is launched by a clock edge or is a pad's: the
    /// clock-to-output delay, or 0 for pads. `None` if the output is a
    /// combinational function of the cell's inputs, or a constant (a cell
    /// with no inputs that is not a pad: it launches no path).
    fn launch_ps(&self, cell: &Cell, output: usize) -> Option<i64>;

    /// Combinational delay from an input to an output. `None` if there is no
    /// path between them.
    fn arc_ps(&self, cell: &Cell, input: usize, output: usize) -> Option<i64>;

    /// Inputs a clocked cell samples, with their setup times.
    fn endpoints(&self, cell: &Cell) -> Vec<TimingEndpoint>;

    /// The cell input whose clock launches an output that
    /// [`launch_ps`](Self::launch_ps) gives a clock-to-output delay for, for
    /// a block with several clocks; `None`: the cell's own clock
    fn launch_clock(&self, _cell: &Cell, _output: usize) -> Option<usize> {
        None
    }

    /// Router pins an input reaches. Most inputs have one.
    fn input_pins(&self, _cell: &Cell, input: usize) -> Vec<usize> {
        vec![input]
    }

    /// Whether an input arrives on dedicated wiring the router doesn't handle.
    fn is_dedicated(&self, _cell: &Cell, _input: usize) -> bool {
        false
    }

    /// Measured metastability of a fabric element, by its device-database
    /// name (such as [`STEP_LATCH_ELEMENT`]); `None` where the device data
    /// has none
    fn metastability(&self, _element: &str) -> Option<Metastability> {
        None
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
    /// Only registers on this clock launch paths (hold inside one async
    /// block, whose registers share its step clock)
    launch_clock: Option<GateNetId>,
}

impl<D: Device + ?Sized> Engine<'_, D> {
    /// Routing delay of a net from its source to one input pin of a cell.
    /// Where the device cannot name the pin's wire, the router's delay for
    /// the whole net stands in for it.
    fn route_delay(&mut self, net: GateNetId, cell: CellId, pin: usize) -> i64 {
        let Some(loc) = self.design.placement.placements.get(&cell) else {
            return 0;
        };
        let Some(route) = self.design.routing.routes.get(&net) else {
            return 0;
        };
        let Some(mut wire) =
            self.device
                .bel_pin_wire(loc.tile_x, loc.tile_y, loc.bel_index, CellPin::Input(pin))
        else {
            return i64::from(route.delay);
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

    /// The clock net on cell input `input`, or the cell's own clock
    fn clock_net(cell: &Cell, input: Option<usize>) -> Option<GateNetId> {
        match input {
            Some(input) => cell.inputs.get(input).copied(),
            None => cell.clock,
        }
    }

    /// When the clock on cell input `input` (the cell's own clock for
    /// `None`) reaches the cell, from the clock net's source
    fn clock_arrival_on(&mut self, cell: &Cell, input: Option<usize>) -> i64 {
        let Some(clock) = Self::clock_net(cell, input) else {
            return 0;
        };
        let pin = match input {
            Some(input) => self
                .model
                .input_pins(cell, input)
                .first()
                .copied()
                .unwrap_or(input),
            None => FF_PIN_CLK,
        };
        self.route_delay(clock, cell.id, pin)
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
            let clock_input = self.model.launch_clock(cell, output);
            let clock_net = Self::clock_net(cell, clock_input);
            // Pads launch nothing hold has to check
            let other_clock = self
                .launch_clock
                .is_some_and(|clock| clock_net != Some(clock));
            if self.early && (clock_net.is_none() || other_clock) {
                return Arrival {
                    time: NO_PATH,
                    from: None,
                    route_ps: 0,
                    cell_ps: 0,
                };
            }
            // A register launches when its clock reaches it: the clock's
            // routing shows as the routing into the launching cell.
            let clock = self.clock_arrival_on(cell, clock_input);
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
        // A constant (a cell with no inputs, or one whose every timed arc
        // comes from a constant) never changes, so it launches no path
        let mut arcs = 0;
        let mut timed = 0;
        for input in 0..cell.inputs.len() {
            let Some(delay) = self.model.arc_ps(cell, input, output) else {
                continue;
            };
            arcs += 1;
            if let Some((time, net, route_ps)) = self.input_time(cell, input) {
                timed += 1;
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
        if timed == 0 && (cell.inputs.is_empty() || arcs > 0) {
            best.time = NO_PATH;
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
        launch_clock: None,
    };

    let netlist = design.netlist;
    let mut per_clock: BTreeMap<u32, (usize, f64)> = BTreeMap::new();
    let mut paths = Vec::new();

    for cell in &netlist.cells {
        if engine.async_cells.contains(&cell.id) {
            continue;
        }
        let endpoints = model.endpoints(cell);
        // Each clock the cell samples on, counted once
        let mut clocks: Vec<GateNetId> = Vec::new();
        for endpoint in &endpoints {
            if let Some(clock) = Engine::<D>::clock_net(cell, endpoint.clock) {
                if !clocks.contains(&clock) {
                    clocks.push(clock);
                }
            }
        }
        for clock in &clocks {
            per_clock.entry(clock.0).or_insert((0, 0.0)).0 += 1;
        }
        for endpoint in endpoints {
            let Some(clock) = Engine::<D>::clock_net(cell, endpoint.clock) else {
                continue;
            };
            let target_ns = targets.required_ns(clock);
            let capture = engine.clock_arrival_on(cell, endpoint.clock);
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
    let mut by_instance: BTreeMap<&str, (usize, f64)> = BTreeMap::new();
    for (path, _, _) in paths.iter().filter(|p| p.0.slack < 0.0) {
        let instance = path
            .destination
            .rsplit_once('.')
            .map_or("", |(parent, _)| parent);
        let entry = by_instance.entry(instance).or_insert((0, 0.0));
        entry.0 += 1;
        entry.1 = entry.1.min(path.slack);
    }
    let mut failing_by_instance: Vec<FailingInstance> = by_instance
        .into_iter()
        .map(|(instance, (endpoints, worst_slack))| FailingInstance {
            instance: instance.to_string(),
            endpoints,
            worst_slack,
        })
        .collect();
    failing_by_instance.sort_by(|a, b| a.worst_slack.total_cmp(&b.worst_slack));
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
        failing_by_instance,
        untimed_switches: Vec::new(),
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
    let async_cells = async_block_cells(design.netlist);
    let engine = |launch_clock, async_cells| Engine {
        device,
        model,
        design,
        drivers: HashMap::new(),
        arrivals: HashMap::new(),
        visiting: HashSet::new(),
        early: true,
        async_cells,
        launch_clock,
    };
    let margin_ps = targets.hold_margin_ns * PS_PER_NS;
    // Synchronous registers, timed through everything but async blocks;
    // then each async block's step clock on its own, from its registers to
    // its registers. Its step clock comes from fabric logic, so its skew can
    // be large; what enters from other blocks is held by the handshake.
    let mut groups: Vec<(Option<GateNetId>, Vec<&Cell>)> = vec![(None, Vec::new())];
    let mut by_clock: BTreeMap<u32, Vec<&Cell>> = BTreeMap::new();
    for cell in &design.netlist.cells {
        if async_cells.contains(&cell.id) {
            if let Some(clock) = cell.clock {
                by_clock.entry(clock.0).or_default().push(cell);
            }
        } else {
            groups[0].1.push(cell);
        }
    }
    groups.extend(
        by_clock
            .into_iter()
            .map(|(clock, cells)| (Some(GateNetId(clock)), cells)),
    );
    let mut engines = Vec::new();
    let mut paths = Vec::new();
    for (launch_clock, cells) in groups {
        let mut e = engine(
            launch_clock,
            if launch_clock.is_some() {
                HashSet::new()
            } else {
                async_cells.clone()
            },
        );
        for cell in cells {
            for endpoint in model.endpoints(cell) {
                if Engine::<D>::clock_net(cell, endpoint.clock).is_none() {
                    continue;
                }
                let capture = e.clock_arrival_on(cell, endpoint.clock);
                let Some((time, net)) = e.input_time_at(cell, endpoint) else {
                    continue;
                };
                let delay = (time - capture) as f64 / PS_PER_NS;
                let required = (endpoint.hold_ps as f64 + margin_ps) / PS_PER_NS;
                paths.push((delay, required, net, cell, engines.len()));
            }
        }
        engines.push(e);
    }
    paths.sort_by(|a, b| (a.0 - a.1).total_cmp(&(b.0 - b.1)));
    let violations = paths.iter().filter(|p| p.0 < p.1).count();
    let worst_slack = paths.first().map_or(0.0, |p| p.0 - p.1);
    let netlist = design.netlist;
    let paths = paths
        .into_iter()
        .take(10)
        .map(|(delay, required, net, cell, group)| {
            let engine = &engines[group];
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

impl<D: Device + ?Sized> RoutedBundleDelays<'_, '_, D> {
    /// Metastability figures of a latch in one LUT with its own feedback,
    /// from the device data where it has them
    fn lut_latch_timing(&self) -> skalp_lir::ncl_bundle::ArbiterTiming {
        let default = skalp_lir::ncl_bundle::ArbiterTiming::default();
        match self.model.metastability(STEP_LATCH_ELEMENT) {
            Some(m) => skalp_lir::ncl_bundle::ArbiterTiming {
                tau_ps: m.tau_ps,
                t0_ps: m.t0_ps,
                measured: true,
                ..default
            },
            None => default,
        }
    }
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

    fn hold_ps(&self, cell: &Cell, input: usize) -> i64 {
        self.model
            .endpoints(cell)
            .iter()
            .find(|e| e.input == input)
            .map_or(0, |e| e.hold_ps)
    }

    fn step_latch_timing(&self) -> skalp_lir::ncl_bundle::ArbiterTiming {
        self.lut_latch_timing()
    }

    /// An arbiter's mutual exclusion is also one LUT with its own feedback
    /// (`skalp_lir::gate_netlist::ARBITER_MUTEX_INIT`), the same element as
    /// the step latch
    fn arbiter_timing(&self) -> skalp_lir::ncl_bundle::ArbiterTiming {
        self.lut_latch_timing()
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
        launch_clock: None,
    };
    let delays = RoutedBundleDelays {
        engine: std::cell::RefCell::new(&mut engine),
        model,
    };
    skalp_lir::ncl_bundle::check_matched_delays(design.netlist, &delays)
}

/// Every delay of a routed design that a timed simulation needs, from
/// `model` (one corner), keyed to the routed netlist
pub fn annotate_delays<D: Device + ?Sized>(
    device: &D,
    model: &dyn TimingModel,
    design: &PlacedDesign<'_>,
    corner: &str,
) -> skalp_lir::timed_sim::DelayAnnotation {
    let mut engine = Engine {
        device,
        model,
        design,
        drivers: HashMap::new(),
        arrivals: HashMap::new(),
        visiting: HashSet::new(),
        early: false,
        async_cells: HashSet::new(),
        launch_clock: None,
    };
    let delays = RoutedBundleDelays {
        engine: std::cell::RefCell::new(&mut engine),
        model,
    };
    skalp_lir::timed_sim::DelayAnnotation::capture(design.netlist, &delays, corner)
}

/// The routing load of every net of a routed design, for switching
/// energy: the delays (from `model`, at `corner`) of the routing switches
/// each net uses, summed with each switch once
pub fn net_loads(
    model: &dyn TimingModel,
    netlist: &skalp_lir::gate_netlist::GateNetlist,
    routing: &crate::router::RoutingResult,
    corner: &str,
) -> skalp_lir::switching_energy::NetLoads {
    let mut routing_ps = vec![0i64; netlist.nets.len()];
    let mut switches = vec![0u32; netlist.nets.len()];
    for (net, route) in &routing.routes {
        let Some(slot) = routing_ps.get_mut(net.0 as usize) else {
            continue;
        };
        let mut pips = route.pips.clone();
        pips.sort_unstable_by_key(|p| p.0);
        pips.dedup();
        *slot = pips.iter().map(|&p| model.interconnect_ps(p).max(0)).sum();
        switches[net.0 as usize] = pips.len() as u32;
    }
    skalp_lir::switching_energy::NetLoads {
        corner: corner.to_string(),
        routing_ps,
        switches,
    }
}

/// The routed switches `model` has no delay for (timed as 0 ps), one entry
/// per delay class with how many switches and nets use it and one net for an
/// example, most-used first. A routed path through such a switch is faster
/// in timing than on silicon by however long the switch really takes.
pub fn untimed_switches(
    model: &dyn TimingModel,
    netlist: &skalp_lir::gate_netlist::GateNetlist,
    routing: &crate::router::RoutingResult,
) -> Vec<super::UntimedSwitches> {
    let mut by_class: BTreeMap<String, (usize, BTreeSet<u32>)> = BTreeMap::new();
    for (&net, route) in &routing.routes {
        let mut pips = route.pips.clone();
        pips.sort_unstable_by_key(|p| p.0);
        pips.dedup();
        for pip in pips {
            if let Some(class) = model.untimed_pip(pip) {
                let entry = by_class.entry(class).or_default();
                entry.0 += 1;
                entry.1.insert(net.0);
            }
        }
    }
    let mut out: Vec<super::UntimedSwitches> = by_class
        .into_iter()
        .map(|(class, (switches, nets))| super::UntimedSwitches {
            class,
            switches,
            nets: nets.len(),
            example_net: nets
                .iter()
                .next()
                .and_then(|&n| netlist.nets.get(n as usize))
                .map(|n| n.name.clone())
                .unwrap_or_default(),
        })
        .collect();
    out.sort_by(|a, b| b.nets.cmp(&a.nets).then(a.class.cmp(&b.class)));
    out
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
            let unmeasured =
                c.unmeasured_settle || by_key.get(&key).is_some_and(|r| r.unmeasured_settle);
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
                        unmeasured_settle: unmeasured,
                    },
                );
            } else if let Some(r) = by_key.get_mut(&key) {
                r.unmeasured_settle |= unmeasured;
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bitstream::prepare::TiedInputs;
    use crate::device::ice40::{Ice40Device, Ice40Variant};
    use crate::placer::PlacementResult;
    use crate::router::RoutingResult;
    use skalp_lir::gate_netlist::{GateNet, GateNetlist};

    /// Registers launch 100 ps after their clock; a logic cell's first input
    /// is 9 ns slow, its others 100 ps; registers sample input 0
    struct Stub;

    impl TimingModel for Stub {
        fn interconnect_ps(&self, _pip: PipId) -> i64 {
            0
        }
        fn launch_ps(&self, cell: &Cell, _output: usize) -> Option<i64> {
            cell.clock.map(|_| 100)
        }
        fn arc_ps(&self, cell: &Cell, input: usize, _output: usize) -> Option<i64> {
            if cell.clock.is_some() {
                return None;
            }
            Some(if input == 0 { 9000 } else { 100 })
        }
        fn endpoints(&self, cell: &Cell) -> Vec<TimingEndpoint> {
            if cell.clock.is_none() {
                return Vec::new();
            }
            vec![TimingEndpoint {
                input: 0,
                pin: 0,
                setup_ps: 0,
                hold_ps: 0,
                clock: None,
            }]
        }
    }

    /// A constant never changes, so the slow arc from the one that drives
    /// a logic cell's first input is no path: the register-to-register path
    /// through the cell's other input is the one timed
    #[test]
    fn constants_launch_no_path() {
        let mut netlist = GateNetlist::new("top".into(), "test".into());
        let net = |netlist: &mut GateNetlist, name: &str| {
            netlist.add_net(GateNet::new(GateNetId(0), name.to_string()))
        };
        let clk = net(&mut netlist, "clk");
        let constant = net(&mut netlist, "constant");
        let q = net(&mut netlist, "q");
        let d = net(&mut netlist, "d");
        netlist.add_cell(Cell::new_comb(
            CellId(0),
            "LUT4".into(),
            "test".into(),
            0.0,
            "constant_driver".into(),
            Vec::new(),
            vec![constant],
        ));
        netlist.add_cell(Cell::new_comb(
            CellId(0),
            "LUT4".into(),
            "test".into(),
            0.0,
            "logic".into(),
            vec![constant, q],
            vec![d],
        ));
        netlist.add_cell(Cell::new_seq(
            CellId(0),
            "DFF".into(),
            "test".into(),
            0.0,
            "reg".into(),
            vec![d],
            vec![q],
            clk,
            None,
        ));
        let (placement, routing, tied) = (
            PlacementResult::default(),
            RoutingResult::default(),
            TiedInputs::default(),
        );
        let design = PlacedDesign {
            netlist: &netlist,
            placement: &placement,
            routing: &routing,
            tied: &tied,
        };
        let device = Ice40Device::new(Ice40Variant::Hx1k);
        let report = analyze(&device, &Stub, &design, &ClockTargets::uniform(100.0));
        let path = report
            .critical_paths
            .first()
            .expect("the register is timed");
        assert_eq!(path.source, "q", "{path:?}");
        assert!((path.delay - 0.2).abs() < 1e-9, "{path:?}");
        assert!(report.meets_timing);
    }

    /// Times every switch but numbers 7 and 8, which it has no delay for
    struct PartlyTimed;

    impl TimingModel for PartlyTimed {
        fn interconnect_ps(&self, pip: PipId) -> i64 {
            if self.untimed_pip(pip).is_some() {
                0
            } else {
                100
            }
        }
        fn untimed_pip(&self, pip: PipId) -> Option<String> {
            match pip.0 {
                7 => Some("entry -> mid".into()),
                8 => Some("A -> B (no class)".into()),
                _ => None,
            }
        }
        fn launch_ps(&self, _cell: &Cell, _output: usize) -> Option<i64> {
            None
        }
        fn arc_ps(&self, _cell: &Cell, _input: usize, _output: usize) -> Option<i64> {
            None
        }
        fn endpoints(&self, _cell: &Cell) -> Vec<TimingEndpoint> {
            Vec::new()
        }
    }

    /// Routed switches the model can't time are reported by class, each
    /// switch once per net, with the nets that use them, most-used first;
    /// timed switches are not
    #[test]
    fn untimed_switches_are_reported_by_class() {
        let mut netlist = GateNetlist::new("top".into(), "test".into());
        let a = netlist.add_net(GateNet::new(GateNetId(0), "a".into()));
        let b = netlist.add_net(GateNet::new(GateNetId(0), "b".into()));
        let c = netlist.add_net(GateNet::new(GateNetId(0), "c".into()));
        let mut routing = RoutingResult::default();
        for (net, pips) in [(a, vec![7, 7, 3]), (b, vec![7, 8]), (c, vec![1, 2])] {
            let mut route = crate::router::Route::new(net);
            route.pips = pips.into_iter().map(PipId).collect();
            routing.routes.insert(net, route);
        }
        let untimed = untimed_switches(&PartlyTimed, &netlist, &routing);
        let got: Vec<(&str, usize, usize)> = untimed
            .iter()
            .map(|u| (u.class.as_str(), u.switches, u.nets))
            .collect();
        assert_eq!(
            got,
            vec![("entry -> mid", 2, 2), ("A -> B (no class)", 1, 1)]
        );
        assert_eq!(untimed[0].example_net, "a");
        assert_eq!(untimed[1].example_net, "b");
    }
}
