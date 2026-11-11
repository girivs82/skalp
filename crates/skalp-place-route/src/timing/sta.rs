//! Family-independent static timing analysis of a placed and routed design.
//!
//! The engine walks routed nets and combinational cells to find the longest
//! register-to-register path. What a delay is comes from a [`TimingModel`],
//! which each FPGA family supplies from its own timing data.
//!
//! Scope: one clock at a time, without clock skew. Paths from and to the IO
//! pads are not timed.

use super::{ClockSummary, CriticalPath, PathStep, TimingReport};
use crate::bitstream::config::PlacedDesign;
use crate::collections::{HashMap, HashMapExt, HashSet, HashSetExt};
use crate::device::{CellPin, Device, PipId, WireId};
use skalp_lir::gate_netlist::{Cell, CellId, GateNetId};
use std::collections::BTreeMap;

const PS_PER_NS: f64 = 1000.0;

/// An input a clocked cell samples.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TimingEndpoint {
    /// Index into the cell's inputs
    pub input: usize,
    /// The router pin that input arrives on
    pub pin: usize,
    /// Setup time in picoseconds
    pub setup_ps: i64,
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

/// Worst arrival at a net's driver output.
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

    fn net_arrival(&mut self, net: GateNetId) -> i64 {
        if let Some(arrival) = self.arrivals.get(&net) {
            return arrival.time;
        }
        if !self.visiting.insert(net) {
            return 0;
        }
        let netlist = self.design.netlist;
        let info = &netlist.nets[net.0 as usize];
        let driver = info.driver.and_then(|id| netlist.get_cell(id));
        let output = info.driver_pin.unwrap_or(0);
        let arrival = match driver {
            Some(cell) => self.output_time(cell, output),
            None => Arrival {
                time: 0,
                from: None,
                route_ps: 0,
                cell_ps: 0,
            },
        };
        self.visiting.remove(&net);
        self.arrivals.insert(net, arrival);
        arrival.time
    }

    /// Latest arrival of an input at the cell and the routing delay part of
    /// it, or `None` if it is not timed.
    fn input_time(&mut self, cell: &Cell, input: usize) -> Option<(i64, GateNetId, i64)> {
        let net = *cell.inputs.get(input)?;
        if self.design.tied.value(cell.id, input).is_some() {
            return None;
        }
        let arrival = self.net_arrival(net);
        if self.model.is_dedicated(cell, input) {
            return Some((arrival, net, 0));
        }
        let routing = self
            .model
            .input_pins(cell, input)
            .into_iter()
            .map(|pin| self.route_delay(net, cell.id, pin))
            .max()
            .unwrap_or(0);
        Some((arrival + routing, net, routing))
    }

    fn output_time(&mut self, cell: &Cell, output: usize) -> Arrival {
        if let Some(launch) = self.model.launch_ps(cell, output) {
            return Arrival {
                time: launch,
                from: None,
                route_ps: 0,
                cell_ps: launch,
            };
        }
        let mut worst = Arrival {
            time: 0,
            from: None,
            route_ps: 0,
            cell_ps: 0,
        };
        for input in 0..cell.inputs.len() {
            let Some(delay) = self.model.arc_ps(cell, input, output) else {
                continue;
            };
            if let Some((time, net, route_ps)) = self.input_time(cell, input) {
                if time + delay > worst.time {
                    worst = Arrival {
                        time: time + delay,
                        from: Some(net),
                        route_ps,
                        cell_ps: delay,
                    };
                }
            }
        }
        worst
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
    target_mhz: f64,
) -> TimingReport {
    let mut engine = Engine {
        device,
        model,
        design,
        drivers: HashMap::new(),
        arrivals: HashMap::new(),
        visiting: HashSet::new(),
    };

    let netlist = design.netlist;
    let target_ns = PS_PER_NS / target_mhz;
    let mut per_clock: BTreeMap<u32, (usize, f64)> = BTreeMap::new();
    let mut paths = Vec::new();

    for cell in &netlist.cells {
        let Some(clock) = cell.clock else { continue };
        let entry = per_clock.entry(clock.0).or_insert((0, 0.0));
        entry.0 += 1;
        for endpoint in model.endpoints(cell) {
            let Some(routed) = engine.input_time_at(cell, endpoint) else {
                continue;
            };
            let (time, net) = routed;
            let delay_ns = (time + endpoint.setup_ps) as f64 / PS_PER_NS;
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
            ));
        }
    }

    paths.sort_by(|a, b| b.0.delay.total_cmp(&a.0.delay));
    let failing_paths = paths.iter().filter(|p| p.0.slack < 0.0).count();
    let worst_delay = paths.first().map_or(0.0, |p| p.0.delay);
    paths.truncate(10);
    let paths: Vec<CriticalPath> = paths
        .into_iter()
        .map(|(mut path, nets)| {
            path.steps = engine.path_steps(&nets);
            path
        })
        .collect();

    let clock_summaries = per_clock
        .iter()
        .map(|(&net, &(registers, delay))| ClockSummary {
            name: netlist.nets[net as usize].name.clone(),
            frequency: if delay > 0.0 {
                PS_PER_NS / delay
            } else {
                target_mhz
            },
            register_count: registers,
            worst_slack: target_ns - delay,
        })
        .collect();

    TimingReport {
        target_frequency: target_mhz,
        design_frequency: if worst_delay > 0.0 {
            PS_PER_NS / worst_delay
        } else {
            target_mhz
        },
        worst_negative_slack: (worst_delay - target_ns).max(0.0),
        failing_paths,
        meets_timing: failing_paths == 0,
        clock_summaries,
        critical_paths: paths,
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
        Some((arrival + self.route_delay(net, cell.id, endpoint.pin), net))
    }
}
