//! Static timing analysis on the real Nexus device data.
//!
//! Uses prjoxide's delay tables: per-pip interconnect classes and per-cell
//! timing arcs. The tables were characterized on CrossLink-NX; CertusPro-NX has
//! none of its own and shares the Nexus fabric, so these are estimates for it.
//!
//! Scope: register-to-register paths on one clock, without clock skew. Paths
//! from and to the IO pads are not timed.

use super::NexusDevice;
use crate::bitstream::prepare::{constant_value, is_pad, TiedInputs};
use crate::device::{CellPin, Device, PipId, WireId, DIST_RAM_PIN_RAD, FF_PIN_CE, FF_PIN_LSR};
use crate::error::{PlaceRouteError, Result};
use crate::placer::PlacementResult;
use crate::router::RoutingResult;
use crate::timing::{ClockSummary, CriticalPath, TimingReport};
use serde::Deserialize;
use skalp_lir::gate_netlist::{Cell, CellId, GateNetId, GateNetlist};
use skalp_lir::tech_library::CellFunction;
use std::collections::{BTreeMap, HashMap, HashSet};
use std::path::Path;

/// Speed grade table of the Versa board's LFCPNX-100-9 part.
const SPEED_GRADE: &str = "12";

const PS_PER_NS: f64 = 1000.0;

#[derive(Debug, Deserialize)]
struct InterconnectFile {
    pip_classes: HashMap<String, PipClass>,
}

#[derive(Debug, Deserialize)]
struct PipClass {
    base: (i64, i64),
}

#[derive(Debug, Deserialize)]
struct CellsFile {
    celltypes: HashMap<String, CellTiming>,
}

#[derive(Debug, Deserialize)]
struct CellTiming {
    iopaths: Vec<IoPath>,
    #[serde(default)]
    setupholds: Vec<SetupHold>,
}

#[derive(Debug, Deserialize)]
struct IoPath {
    from_pin: String,
    to_pin: String,
    maxv: i64,
}

#[derive(Debug, Deserialize)]
struct SetupHold {
    pin: String,
    max_setup: i64,
}

/// Worst-case delays from prjoxide's timing database, in picoseconds.
pub struct TimingDb {
    pip_classes: HashMap<String, i64>,
    lut: i64,
    clk_to_q: i64,
    setup: HashMap<String, i64>,
    ccu2: HashMap<(String, String), i64>,
}

impl TimingDb {
    /// Load the delay tables from the prjoxide database.
    pub fn load(db_root: &Path, family: &str) -> Result<Self> {
        let dir = db_root.join(family).join("timing");
        let read = |name: String| -> Result<String> {
            std::fs::read_to_string(dir.join(&name))
                .map_err(|e| PlaceRouteError::BitstreamFailed(format!("reading {name}: {e}")))
        };
        let bad = |name: &str, e: serde_json::Error| {
            PlaceRouteError::BitstreamFailed(format!("parsing {name}: {e}"))
        };

        let interconnect: InterconnectFile =
            serde_json::from_str(&read(format!("interconnect_{SPEED_GRADE}.json"))?)
                .map_err(|e| bad("interconnect", e))?;
        let cells: CellsFile = serde_json::from_str(&read(format!("cells_{SPEED_GRADE}.json"))?)
            .map_err(|e| bad("cells", e))?;

        let cell = |name: &str| {
            cells.celltypes.get(name).ok_or_else(|| {
                PlaceRouteError::BitstreamFailed(format!("No timing data for {name}"))
            })
        };
        let lut = cell("OXIDE_COMB:LUT4")?
            .iopaths
            .iter()
            .map(|p| p.maxv)
            .max()
            .unwrap_or(0);
        let ff = cell("OXIDE_FF:PPP:SYNC")?;
        let clk_to_q = ff.iopaths.iter().map(|p| p.maxv).max().unwrap_or(0);
        let setup = ff
            .setupholds
            .iter()
            .map(|s| (s.pin.clone(), s.max_setup))
            .collect();
        let ccu2 = cell("OXIDE_COMB:CCU2")?
            .iopaths
            .iter()
            .map(|p| ((p.from_pin.clone(), p.to_pin.clone()), p.maxv))
            .collect();

        Ok(Self {
            pip_classes: interconnect
                .pip_classes
                .into_iter()
                .map(|(name, class)| (name, class.base.1))
                .collect(),
            lut,
            clk_to_q,
            setup,
            ccu2,
        })
    }
}

/// The name and tile of a routing wire, split for pip classification.
fn wire_parts(name: &str) -> (i32, i32, String) {
    if let Some(global) = name.strip_prefix("G:") {
        return (0, 0, global.to_string());
    }
    let parsed = name.strip_prefix('R').and_then(|rest| {
        let (row, rest) = rest.split_once('C')?;
        let (column, base) = rest.split_once('_')?;
        Some((column.parse().ok()?, row.parse().ok()?, base.to_string()))
    });
    parsed.unwrap_or((0, 0, name.to_string()))
}

/// Name of a `CCU2` input by index.
fn ccu2_input(index: usize) -> String {
    match index {
        8 => "FCI".to_string(),
        i => format!("{}{}", ['A', 'B', 'C', 'D'][i % 4], i / 4),
    }
}

fn ccu2_output(index: usize) -> &'static str {
    match index {
        0 => "F0",
        1 => "F1",
        _ => "FCO",
    }
}

struct Sta<'a> {
    device: &'a NexusDevice,
    netlist: &'a GateNetlist,
    placement: &'a PlacementResult,
    routing: &'a RoutingResult,
    tied: &'a TiedInputs,
    db: &'a TimingDb,
    /// Per-net map from a routed wire to the pip that drives it
    drivers: HashMap<GateNetId, HashMap<WireId, PipId>>,
    /// Worst arrival time at each net and the net it came through
    arrivals: HashMap<GateNetId, (i64, Option<GateNetId>)>,
    visiting: HashSet<GateNetId>,
}

impl Sta<'_> {
    fn pip_delay(&self, pip: PipId) -> i64 {
        let Some(info) = self.device.pip(pip) else {
            return 0;
        };
        let (Some(src), Some(dst)) = (
            self.device.wire(info.src_wire),
            self.device.wire(info.dst_wire),
        ) else {
            return 0;
        };
        let (sx, sy, source) = wire_parts(&src.name);
        let (dx, dy, destination) = wire_parts(&dst.name);
        prjoxide::pip_classes::classify_pip(0, 0, &source, dx - sx, dy - sy, &destination)
            .and_then(|class| self.db.pip_classes.get(&class).copied())
            .unwrap_or(0)
    }

    /// Routing delay of a net from its source to one input pin of a cell.
    fn route_delay(&mut self, net: GateNetId, cell: CellId, pin: usize) -> i64 {
        let Some(loc) = self.placement.placements.get(&cell) else {
            return 0;
        };
        let Some(mut wire) =
            self.device
                .bel_pin_wire(loc.tile_x, loc.tile_y, loc.bel_index, CellPin::Input(pin))
        else {
            return 0;
        };
        let Some(route) = self.routing.routes.get(&net) else {
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
        let path: Vec<PipId> = {
            let mut path = Vec::new();
            while let Some(&pip) = drivers.get(&wire) {
                path.push(pip);
                let Some(info) = device.pip(pip) else { break };
                wire = info.src_wire;
                if path.len() > 10_000 {
                    break;
                }
            }
            path
        };
        path.into_iter().map(|p| self.pip_delay(p)).sum()
    }

    fn net_arrival(&mut self, net: GateNetId) -> i64 {
        if let Some(&(time, _)) = self.arrivals.get(&net) {
            return time;
        }
        if !self.visiting.insert(net) {
            return 0;
        }
        let driver = self.netlist.nets[net.0 as usize]
            .driver
            .and_then(|id| self.netlist.get_cell(id));
        let driver_pin = self.netlist.nets[net.0 as usize].driver_pin.unwrap_or(0);
        let (time, from) = match driver {
            Some(cell) => self.cell_output_time(cell, driver_pin),
            None => (0, None),
        };
        self.visiting.remove(&net);
        self.arrivals.insert(net, (time, from));
        time
    }

    /// Latest arrival of a net at an input, or `None` if it isn't timed.
    fn input_time(&mut self, cell: &Cell, index: usize, pin: usize) -> Option<(i64, GateNetId)> {
        let net = *cell.inputs.get(index)?;
        if self.tied.value(cell.id, index).is_some() {
            return None;
        }
        Some((
            self.net_arrival(net) + self.route_delay(net, cell.id, pin),
            net,
        ))
    }

    fn cell_output_time(&mut self, cell: &Cell, output: usize) -> (i64, Option<GateNetId>) {
        if is_pad(cell) || constant_value(cell).is_some() {
            return (0, None);
        }
        if cell.function == Some(CellFunction::DistRam16x4) {
            let mut worst = (0, None);
            for address in 0..4 {
                let net = cell.inputs[9 + address];
                let mut time = self.net_arrival(net);
                time += (0..4)
                    .map(|lut| self.route_delay(net, cell.id, DIST_RAM_PIN_RAD + 4 * address + lut))
                    .max()
                    .unwrap_or(0);
                if time + self.db.lut > worst.0 {
                    worst = (time + self.db.lut, Some(net));
                }
            }
            return worst;
        }
        if cell.clock.is_some() {
            return (self.db.clk_to_q, None);
        }
        if cell.function == Some(CellFunction::Ccu2) {
            let to = ccu2_output(output);
            let mut worst = (0, None);
            for index in 0..cell.inputs.len().min(9) {
                let arc = (ccu2_input(index), to.to_string());
                let Some(&delay) = self.db.ccu2.get(&arc) else {
                    continue;
                };
                let timed = if index == 8 {
                    let net = cell.inputs[8];
                    (self.tied.value(cell.id, 8).is_none()).then(|| (self.net_arrival(net), net))
                } else {
                    self.input_time(cell, index, index)
                };
                if let Some((time, net)) = timed {
                    if time + delay > worst.0 {
                        worst = (time + delay, Some(net));
                    }
                }
            }
            return worst;
        }
        let mut worst = (0, None);
        for index in 0..cell.inputs.len() {
            if let Some((time, net)) = self.input_time(cell, index, index) {
                if time + self.db.lut > worst.0 {
                    worst = (time + self.db.lut, Some(net));
                }
            }
        }
        worst
    }

    /// Router pin and setup time of each input a clocked cell samples.
    fn endpoints(&self, cell: &Cell) -> Vec<(usize, usize, i64)> {
        let setup = |name: &str| self.db.setup.get(name).copied().unwrap_or(0);
        if cell.function == Some(CellFunction::DistRam16x4) {
            return (0..=8).map(|i| (i, i, 0)).collect();
        }
        (0..cell.inputs.len())
            .map(|index| match (cell.cell_type.as_str(), index) {
                (_, 0) => (0, 0, setup("M")),
                ("OXIDE_FF_EN", 1) | ("OXIDE_FF_RSTE", 2) => (index, FF_PIN_CE, setup("CE")),
                (_, i) => (i, FF_PIN_LSR, setup("LSR")),
            })
            .collect()
    }

    fn path_through(&self, mut net: GateNetId) -> Vec<String> {
        let mut path = vec![self.netlist.nets[net.0 as usize].name.clone()];
        let mut seen = HashSet::from([net]);
        while let Some(&(_, Some(previous))) = self.arrivals.get(&net) {
            if !seen.insert(previous) {
                break;
            }
            path.push(self.netlist.nets[previous.0 as usize].name.clone());
            net = previous;
        }
        path.reverse();
        path
    }
}

/// Analyze a placed and routed design against a clock frequency in MHz.
pub fn analyze(
    device: &NexusDevice,
    netlist: &GateNetlist,
    placement: &PlacementResult,
    routing: &RoutingResult,
    tied: &TiedInputs,
    db: &TimingDb,
    target_mhz: f64,
) -> TimingReport {
    let mut sta = Sta {
        device,
        netlist,
        placement,
        routing,
        tied,
        db,
        drivers: HashMap::new(),
        arrivals: HashMap::new(),
        visiting: HashSet::new(),
    };

    let target_ns = PS_PER_NS / target_mhz;
    let mut per_clock: BTreeMap<u32, (usize, f64)> = BTreeMap::new();
    let mut paths = Vec::new();

    for cell in &netlist.cells {
        let Some(clock) = cell.clock else { continue };
        let entry = per_clock.entry(clock.0).or_insert((0, 0.0));
        entry.0 += 1;
        for (index, pin, setup) in sta.endpoints(cell) {
            let Some((time, net)) = sta.input_time(cell, index, pin) else {
                continue;
            };
            let delay_ns = (time + setup) as f64 / PS_PER_NS;
            if delay_ns > per_clock[&clock.0].1 {
                per_clock.get_mut(&clock.0).expect("present").1 = delay_ns;
            }
            paths.push(CriticalPath {
                delay: delay_ns,
                required: target_ns,
                slack: target_ns - delay_ns,
                source: sta.path_through(net).first().cloned().unwrap_or_default(),
                destination: cell.path.clone(),
                path: sta.path_through(net),
            });
        }
    }

    paths.sort_by(|a, b| b.delay.total_cmp(&a.delay));
    let failing_paths = paths.iter().filter(|p| p.slack < 0.0).count();
    let worst_delay = paths.first().map_or(0.0, |p| p.delay);
    paths.truncate(10);

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
        ..Default::default()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn wire_names_split_into_location_and_base() {
        assert_eq!(
            wire_parts("R2C97_H01W0300"),
            (97, 2, "H01W0300".to_string())
        );
        assert_eq!(wire_parts("G:VCC"), (0, 0, "VCC".to_string()));
    }

    #[test]
    fn carry_pins_are_named_by_half() {
        assert_eq!(ccu2_input(0), "A0");
        assert_eq!(ccu2_input(7), "D1");
        assert_eq!(ccu2_input(8), "FCI");
    }

    #[test]
    fn delays_come_from_the_database() {
        let Some(db) = super::super::prjoxide_load::find_database() else {
            eprintln!("PRJOXIDE_DB not found — skipping");
            return;
        };
        let timing = TimingDb::load(&db, "LIFCL").unwrap();
        assert_eq!(timing.lut, 213);
        assert_eq!(timing.clk_to_q, 307);
        assert_eq!(timing.setup.get("M"), Some(&0));
    }
}
