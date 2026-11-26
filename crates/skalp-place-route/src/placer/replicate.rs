//! Replicating high-fanout drivers after placement.
//!
//! A cell that drives many loads spread over the die reaches the far ones
//! late: one output feeds a routing tree that spans every load, and the
//! placer, pulled every way by the loads, often leaves the driver far from
//! its own inputs too. When such a driver (a flip-flop, or a LUT or gate)
//! starts or carries a failing path, [`Placer::replicate_high_fanout`]
//! splits its loads by where they are placed, keeps the original with the
//! group nearest it, and gives each other group its own copy of the cell.
//! A copy has the original's inputs (and, for a flip-flop, its clock,
//! enable, set/reset and initial value), so it carries the same value every
//! cycle.
//!
//! The loads are halved along the longer side of their bounding box, once,
//! twice and so on down to groups of [`ReplicationConfig::max_fanout`], and
//! the driver is split the fewest times that reach the target (or come
//! close to the best any split reaches): each copy takes a site and routing
//! into it. Each copy goes on the free site, among those nearest points
//! from the driver's latest-arriving input to the middle of its group, that
//! leaves the group's paths the most slack, so a copy whose own inputs are
//! late sits nearer them.
//!
//! The loads of one control group (flip-flops that share their clock
//! enable and set/reset, see [`Device::ff_control_group`]) whose enable or
//! set/reset the driver feeds move together, so the group's flip-flops keep
//! one control set. A driver whose output is a clock is left alone.
//!
//! Replication runs on the netlist place and route works on, after
//! synthesis, so no equivalent-register or logic merging sees the copies;
//! each copy carries the [`REPLICA_OF_PARAM`] parameter naming the cell it
//! copies. Timing comes from the placement estimate
//! ([`estimate_slacks`]); a driver is copied only when the worst slack of
//! its paths (through its inputs, the cell, and on to its loads) improves.

use super::control_sets::control_sets;
use super::sites::{bel_types_compatible, SiteMap};
use super::timing_estimate::{estimate_slacks, SlackEstimate};
use super::{cell_bel_type, PlacementLoc, PlacementResult, Placer};
use crate::bitstream::prepare::ff_pin;
use crate::collections::{HashMap, HashMapExt};
use crate::device::{Device, FF_PIN_CE, FF_PIN_LSR};
use crate::error::Result;
use crate::timing::clock_targets::ClockTargets;
use crate::timing::DelayModel;
use serde::{Deserialize, Serialize};
use skalp_lir::gate_netlist::{Cell, CellId, GateNet, GateNetId, GateNetlist};

/// Parameter on a cell made by replication: the path of the cell it copies
pub const REPLICA_OF_PARAM: &str = "REPLICA_OF";

/// When and how far [`Placer::replicate_high_fanout`] replicates drivers
#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct ReplicationConfig {
    /// Replicate at all
    pub enabled: bool,
    /// Fewest loads a driver must have to be replicated
    pub min_fanout: usize,
    /// Splitting stops at groups of this many loads: the fewest loads a
    /// copy is made for (a single load, or a control group, bigger than it
    /// stays whole)
    pub max_fanout: usize,
    /// Replicate a driver whose paths have less slack than this (ns) by
    /// the placement estimate, and split it until they reach it if they
    /// can. A routed net with many loads comes out a few hundred ps later
    /// than the estimate prices its connections (measured on an LFCPNX
    /// build: about 0.2 ns at 8 tiles and 0.4 ns at 20 for a thousand
    /// loads), so the default margin also takes drivers that are close to
    /// failing.
    pub slack_margin_ns: f64,
    /// Most drivers replicated in one run (the most critical first)
    pub max_drivers: usize,
}

impl Default for ReplicationConfig {
    fn default() -> Self {
        Self {
            enabled: true,
            min_fanout: 32,
            max_fanout: 32,
            slack_margin_ns: 0.3,
            max_drivers: 64,
        }
    }
}

/// One driver replicated
#[derive(Debug, Clone, PartialEq)]
pub struct ReplicatedDriver {
    /// Path of the original cell
    pub path: String,
    /// The net it drives
    pub net: String,
    /// Loads it drove
    pub loads: usize,
    /// Copies added (the original stays, driving one group)
    pub copies: usize,
    /// Worst estimated slack (ns) of paths through it before
    pub slack_before_ns: f64,
    /// Worst estimated slack (ns) of paths through it and its copies after
    pub slack_after_ns: f64,
}

/// A placed netlist with high-fanout drivers replicated
pub struct Replication {
    pub netlist: GateNetlist,
    pub placement: PlacementResult,
    pub drivers: Vec<ReplicatedDriver>,
}

/// How a load reads the driver's output
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum LoadPin {
    /// Input `pin` of the cell's `inputs`
    Input(usize),
    /// The cell's set/reset (its `reset` net)
    Reset,
}

#[derive(Debug, Clone, Copy)]
struct Load {
    cell: usize,
    pin: LoadPin,
}

/// Loads that must stay on one driver
#[derive(Debug, Clone)]
struct Unit {
    loads: Vec<Load>,
    /// Tile position
    at: (f64, f64),
}

/// What keeps loads together: a control group of flip-flops, or one cell
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
enum UnitKey {
    ControlGroup(u32, u32, usize),
    Cell(usize),
}

impl<D: Device + Clone> Placer<D> {
    /// Replicate the placed drivers of `netlist` that drive at least
    /// [`ReplicationConfig::min_fanout`] loads and start a path with less
    /// than [`ReplicationConfig::slack_margin_ns`] slack by the placement
    /// estimate, each path end being timed against the clock in `clocks`
    /// capturing it (the default frequency for a pad), and against half its
    /// period for a path between a clock's rising and falling edges.
    /// `None` when nothing is replicated.
    pub fn replicate_high_fanout(
        &self,
        netlist: &GateNetlist,
        placement: &PlacementResult,
        clocks: &ClockTargets,
    ) -> Result<Option<Replication>> {
        let config = self.config.replication;
        if !config.enabled || netlist.is_ncl || config.max_fanout == 0 {
            return Ok(None);
        }
        let model = self.delay_model();
        let mut sites = self.site_map(netlist, placement);
        let mut out = netlist.clone();
        let mut placed = placement.clone();
        let mut drivers = Vec::new();
        // Drivers looked at and left as they were
        let mut left: std::collections::HashSet<usize> = std::collections::HashSet::new();
        // One driver a round, the worst first, timing again after each: a
        // copy changes the paths through the drivers before and after it
        while drivers.len() < config.max_drivers {
            let position = |id: CellId| {
                placed
                    .get(id)
                    .map(|loc| (loc.tile_x as f64, loc.tile_y as f64))
            };
            let candidates: Vec<(usize, Vec<Load>)> =
                candidate_loads(&out, &placed, config.min_fanout)
                    .into_iter()
                    .filter(|(d, _)| !left.contains(d))
                    .collect();
            if candidates.is_empty() {
                break;
            }
            let estimate = estimate_slacks(&out, &model, position, clocks);
            let mut plans: Vec<(f64, usize, Vec<Load>)> = Vec::new();
            for (driver, loads) in candidates {
                let at = position(CellId(driver as u32)).unwrap_or_default();
                let before = driver_slack(&out, &estimate, &model, driver, &position, at, &loads);
                if before < config.slack_margin_ns {
                    plans.push((before, driver, loads));
                }
            }
            plans.sort_by(|a, b| a.0.total_cmp(&b.0).then(a.1.cmp(&b.1)));
            let sets = control_sets(&out);
            let mut accepted = None;
            for (before, driver, loads) in plans {
                let units = self.units(&out, &placed, &loads);
                let plan = (units.len() >= 2)
                    .then(|| {
                        self.best_split(
                            &out, &estimate, &model, &position, &sites, &sets, driver, units,
                        )
                    })
                    .flatten()
                    .filter(|plan| plan.after >= before + MIN_GAIN_NS);
                match plan {
                    Some(plan) => {
                        accepted = Some((before, driver, plan));
                        break;
                    }
                    None => {
                        log::debug!(
                            "replication: {} left as it is (worst slack {before:.3} ns)",
                            out.cells[driver].path
                        );
                        left.insert(driver);
                    }
                }
            }
            let Some((before, driver, plan)) = accepted else {
                break;
            };
            let SplitPlan {
                groups,
                copies,
                after,
                sites: trial,
            } = plan;
            sites = trial;
            left.insert(driver);
            let loads = groups
                .iter()
                .map(|g| g.iter().map(|u| u.loads.len()).sum::<usize>())
                .sum();
            for (k, &(g, loc)) in copies.iter().enumerate() {
                let copy = add_replica(&mut out, driver, k + 1, &group_loads(&groups[g]));
                placed.placements.insert(copy, loc);
                // A copy drives fewer loads than replication looks at
                left.insert(copy.0 as usize);
            }
            let cell = &out.cells[driver];
            drivers.push(ReplicatedDriver {
                path: cell.path.clone(),
                net: out.nets[cell.outputs[0].0 as usize].name.clone(),
                loads,
                copies: copies.len(),
                slack_before_ns: before,
                slack_after_ns: after,
            });
        }
        if drivers.is_empty() {
            return Ok(None);
        }
        Ok(Some(Replication {
            netlist: out,
            placement: placed,
            drivers,
        }))
    }

    /// The BELs `placement` takes, flip-flops with their control sets
    fn site_map(&self, netlist: &GateNetlist, placement: &PlacementResult) -> SiteMap {
        let sets = control_sets(netlist);
        let mut sites = SiteMap::new(&self.device).with_fill_limit(self.fill_limit(netlist));
        // A carry cell also uses its slot's LUTs, which the site map does
        // not know of
        let carry_luts: HashMap<usize, Vec<usize>> = self
            .device
            .carry_chain_layout()
            .map(|layout| {
                layout
                    .slots
                    .into_iter()
                    .map(|s| (s.carry_bel, s.lut_bels))
                    .collect()
            })
            .unwrap_or_default();
        let mut ids: Vec<(&CellId, &PlacementLoc)> = placement.placements.iter().collect();
        ids.sort_by_key(|(id, _)| id.0);
        for (id, loc) in ids {
            sites.take(loc.tile_x, loc.tile_y, loc.bel_index, sets.get(id).copied());
            if loc.bel_type == super::BelType::Carry {
                for &lut in carry_luts.get(&loc.bel_index).into_iter().flatten() {
                    sites.take(loc.tile_x, loc.tile_y, lut, None);
                }
            }
        }
        sites
    }

    /// How to copy cell index `driver` of `netlist`: halve its loads once,
    /// twice, ... down to groups of [`ReplicationConfig::max_fanout`], and
    /// take the fewest halvings that reach the slack margin (or come close
    /// to the best any reaches), since each copy costs a site and the
    /// routing into it. `None` when no split finds sites.
    #[allow(clippy::too_many_arguments)]
    fn best_split(
        &self,
        netlist: &GateNetlist,
        estimate: &SlackEstimate,
        model: &DelayModel,
        position: &impl Fn(CellId) -> Option<(f64, f64)>,
        sites: &SiteMap,
        sets: &HashMap<CellId, super::control_sets::ControlSet>,
        driver: usize,
        units: Vec<Unit>,
    ) -> Option<SplitPlan> {
        let config = self.config.replication;
        let cell = &netlist.cells[driver];
        let home = position(cell.id)?;
        let set = sets.get(&cell.id).copied();
        let required = cell_bel_type(cell);
        let source = latest_input_at(netlist, estimate, driver, position, home);
        let slack_of = |at: (f64, f64), loads: &[Load]| {
            driver_slack(netlist, estimate, model, driver, position, at, loads)
        };
        let mut tried: Vec<SplitPlan> = Vec::new();
        let mut last_groups = 0;
        for levels in 1..=MAX_SPLIT_LEVELS {
            let groups = split(units.clone(), config.max_fanout, levels);
            if groups.len() == last_groups {
                break;
            }
            last_groups = groups.len();
            let Some(plan) =
                self.plan_copies(groups, sites, home, source, required, set, &slack_of)
            else {
                continue;
            };
            let reached = plan.after >= config.slack_margin_ns;
            tried.push(plan);
            if reached {
                break;
            }
        }
        let best = tried.iter().map(|p| p.after).reduce(f64::max)?;
        let enough = best.min(config.slack_margin_ns) - CLOSE_TO_BEST_NS;
        tried.into_iter().find(|p| p.after >= enough)
    }

    /// Copies of a driver at `home` for `groups` of its loads: the
    /// original keeps the group whose middle is nearest it, and each other
    /// group gets a copy on the free site that leaves the group's paths the
    /// most slack (`slack_of` a driver at a tile for some loads) among the
    /// sites nearest points from `source` (its latest input) to the group's
    /// middle. `None` when no copy finds a site.
    #[allow(clippy::too_many_arguments)]
    fn plan_copies(
        &self,
        groups: Vec<Vec<Unit>>,
        sites: &SiteMap,
        home: (f64, f64),
        source: (f64, f64),
        required: super::BelType,
        set: Option<super::control_sets::ControlSet>,
        slack_of: &impl Fn((f64, f64), &[Load]) -> f64,
    ) -> Option<SplitPlan> {
        let middles: Vec<(f64, f64)> = groups.iter().map(|g| middle(g)).collect();
        let keep = (0..groups.len())
            .min_by(|&a, &b| {
                distance(middles[a], home)
                    .total_cmp(&distance(middles[b], home))
                    .then(a.cmp(&b))
            })
            .unwrap_or(0);
        let mut trial = sites.clone();
        let mut copies: Vec<(usize, PlacementLoc)> = Vec::new();
        let mut after = f64::INFINITY;
        let mut original_loads: Vec<Load> = Vec::new();
        for (g, &(x, y)) in middles.iter().enumerate() {
            let loads = group_loads(&groups[g]);
            let mut best: Option<((u32, u32, usize), f64)> = None;
            if g != keep {
                for step in 0..=PLACE_STEPS {
                    let t = step as f64 / PLACE_STEPS as f64;
                    let point = (source.0 + (x - source.0) * t, source.1 + (y - source.1) * t);
                    let Some(site) = trial.find_nearest(
                        point.0,
                        point.1,
                        required,
                        set.as_ref(),
                        &bel_types_compatible,
                    ) else {
                        continue;
                    };
                    let slack = slack_of((site.0 as f64, site.1 as f64), &loads);
                    if best.is_none_or(|(_, b)| slack > b + 1e-9) {
                        best = Some((site, slack));
                    }
                }
            }
            match best {
                Some(((tx, ty, bel), slack)) if trial.take(tx, ty, bel, set) => {
                    copies.push((g, self.replica_loc(tx, ty, bel)));
                    after = after.min(slack);
                }
                // The kept group, and groups no copy found a site for,
                // stay on the original
                _ => original_loads.extend(loads),
            }
        }
        if copies.is_empty() {
            return None;
        }
        after = after.min(slack_of(home, &original_loads));
        Some(SplitPlan {
            groups,
            copies,
            after,
            sites: trial,
        })
    }

    /// The loads of a driver gathered into the units that must share a
    /// driver, each at its tile
    fn units(
        &self,
        netlist: &GateNetlist,
        placement: &PlacementResult,
        loads: &[Load],
    ) -> Vec<Unit> {
        let mut by_key: std::collections::BTreeMap<UnitKey, Unit> =
            std::collections::BTreeMap::new();
        for &load in loads {
            let cell = &netlist.cells[load.cell];
            let Some(loc) = placement.get(cell.id) else {
                continue;
            };
            let control = match load.pin {
                LoadPin::Reset => true,
                LoadPin::Input(pin) => {
                    cell.clock.is_some()
                        && cell_bel_type(cell).is_ff()
                        && matches!(ff_pin(cell, pin), FF_PIN_CE | FF_PIN_LSR)
                }
            };
            let group = control
                .then(|| {
                    self.device
                        .ff_control_group(loc.tile_x, loc.tile_y, loc.bel_index)
                })
                .flatten();
            let key = match group {
                Some(group) => UnitKey::ControlGroup(loc.tile_x, loc.tile_y, group),
                None => UnitKey::Cell(load.cell),
            };
            by_key
                .entry(key)
                .or_insert_with(|| Unit {
                    loads: Vec::new(),
                    at: (loc.tile_x as f64, loc.tile_y as f64),
                })
                .loads
                .push(load);
        }
        // A flip-flop of a control group whose enable the driver feeds
        // may read it on its data input too: it moves with its group
        let grouped: std::collections::HashSet<usize> = by_key
            .iter()
            .filter(|(k, _)| matches!(k, UnitKey::ControlGroup(..)))
            .flat_map(|(_, u)| u.loads.iter().map(|l| l.cell))
            .collect();
        let mut units: Vec<Unit> = Vec::new();
        let mut moved: Vec<Load> = Vec::new();
        for (key, unit) in by_key {
            match key {
                UnitKey::Cell(c) if grouped.contains(&c) => moved.extend(unit.loads),
                _ => units.push(unit),
            }
        }
        for load in moved {
            if let Some(unit) = units
                .iter_mut()
                .find(|u| u.loads.iter().any(|l| l.cell == load.cell))
            {
                unit.loads.push(load);
            }
        }
        units
    }

    fn replica_loc(&self, x: u32, y: u32, bel: usize) -> PlacementLoc {
        let bel_type = self
            .device
            .tile_at(x, y)
            .and_then(|t| t.bels().get(bel).map(|b| b.bel_type))
            .unwrap_or(super::BelType::Dff);
        PlacementLoc::new(x, y, bel, bel_type)
    }
}

/// Whether replication may copy `cell`: a flip-flop, or a LUT or gate
/// (in one LUT, or a wide LUT over a slice's pair), with one output, that
/// no optimization must keep as it is and no constraint ties to a site.
/// A wide LUT's site shares BELs with the LUTs and flip-flop under it; the
/// site map's footprints keep a copy off those already taken.
fn replicable(cell: &Cell) -> bool {
    let bel = cell_bel_type(cell);
    let logic = if cell.clock.is_some() {
        bel.is_ff() && !cell.inputs.is_empty()
    } else {
        matches!(bel, super::BelType::Lut4 | super::BelType::WideLut)
            && cell.function.is_some()
            && !cell.inputs.is_empty()
            && !super::is_constant_cell(&cell.cell_type)
            && !crate::bitstream::prepare::is_pad(cell)
    };
    logic && cell.outputs.len() == 1 && !cell.is_kept() && !cell.parameters.contains_key("LOC")
}

/// Each placed cell [`replicable`] driving at least `min_fanout` loads, by
/// cell index, with its loads. A driver whose output clocks anything is
/// left out.
fn candidate_loads(
    netlist: &GateNetlist,
    placement: &PlacementResult,
    min_fanout: usize,
) -> Vec<(usize, Vec<Load>)> {
    let mut by_net: HashMap<GateNetId, usize> = HashMap::new();
    for (index, cell) in netlist.cells.iter().enumerate() {
        if !replicable(cell) || !placement.is_placed(cell.id) {
            continue;
        }
        let q = netlist.resolve_alias(cell.outputs[0]);
        let fanout = netlist.nets.get(q.0 as usize).map_or(0, |n| n.fanout.len());
        if fanout >= min_fanout {
            by_net.insert(q, index);
        }
    }
    let mut loads: HashMap<usize, Vec<Load>> = HashMap::new();
    let mut clocks: std::collections::HashSet<usize> = std::collections::HashSet::new();
    for (index, cell) in netlist.cells.iter().enumerate() {
        let driving = |net: GateNetId| by_net.get(&netlist.resolve_alias(net)).copied();
        for (pin, &net) in cell.inputs.iter().enumerate() {
            if let Some(r) = driving(net) {
                loads.entry(r).or_default().push(Load {
                    cell: index,
                    pin: LoadPin::Input(pin),
                });
            }
        }
        if let Some(r) = cell.reset.and_then(driving) {
            loads.entry(r).or_default().push(Load {
                cell: index,
                pin: LoadPin::Reset,
            });
        }
        if let Some(r) = cell.clock.and_then(driving) {
            clocks.insert(r);
        }
    }
    let mut found: Vec<(usize, Vec<Load>)> = loads
        .into_iter()
        .filter(|(r, l)| !clocks.contains(r) && l.len() >= min_fanout)
        .collect();
    found.sort_by_key(|(r, _)| *r);
    found
}

/// Worst estimated slack (ns) of driver `driver` placed at tile `at`
/// driving `loads` (each at its cell's tile). A register launches its
/// output on the clock and must also take its own inputs in time from where
/// their drivers are; a combinational cell's output follows its inputs, so
/// the connections into it are part of every path through it.
fn driver_slack(
    netlist: &GateNetlist,
    estimate: &SlackEstimate,
    model: &DelayModel,
    driver: usize,
    position: &impl Fn(CellId) -> Option<(f64, f64)>,
    at: (f64, f64),
    loads: &[Load],
) -> f64 {
    let wire = |from: (f64, f64), to: (f64, f64)| {
        let dx = (from.0 - to.0).abs().round() as u32;
        let dy = (from.1 - to.1).abs().round() as u32;
        model.connection_delay(dx, dy)
    };
    let cell = &netlist.cells[driver];
    let registered = cell.clock.is_some();
    let mut worst = f64::INFINITY;
    // The paths launched on each clock edge, timed apart
    for timing in &estimate.edges {
        // Each input's arrival at `at`, and the slack it leaves a register
        let mut inputs_at = f64::NEG_INFINITY;
        for &input in &cell.inputs {
            let input = netlist.resolve_alias(input);
            let arrival = timing.arrival[input.0 as usize];
            let into = netlist.nets[input.0 as usize]
                .driver
                .and_then(position)
                .map_or(0.0, |from| wire(from, at));
            inputs_at = inputs_at.max(arrival + into);
            if registered {
                worst = worst.min(timing.slack(arrival, into, driver));
            }
        }
        let launch = if registered {
            let q = netlist.resolve_alias(cell.outputs[0]);
            timing.arrival[q.0 as usize]
        } else {
            inputs_at + model.cell_delay(&cell.cell_type)
        };
        for load in loads {
            let LoadPin::Input(_) = load.pin else {
                continue;
            };
            let sink = &netlist.cells[load.cell];
            let Some(to) = position(sink.id) else {
                continue;
            };
            worst = worst.min(timing.slack(launch, wire(at, to), load.cell));
        }
    }
    worst
}

/// Points tried for each copy, from the driver's latest input to the
/// middle of its loads
const PLACE_STEPS: usize = 4;

/// Most times a driver's loads are halved (up to 2^this groups)
const MAX_SPLIT_LEVELS: usize = 8;

/// A split counts as reaching the best when its worst slack is within this
/// (ns) of the best split's
const CLOSE_TO_BEST_NS: f64 = 0.05;

/// Least improvement (ns) of a driver's worst slack worth its copies
const MIN_GAIN_NS: f64 = 0.02;

/// One way to copy a driver: its loads in groups, a site for each group's
/// copy, the worst slack that leaves, and the sites taken
struct SplitPlan {
    groups: Vec<Vec<Unit>>,
    copies: Vec<(usize, PlacementLoc)>,
    after: f64,
    sites: SiteMap,
}

fn group_loads(group: &[Unit]) -> Vec<Load> {
    group.iter().flat_map(|u| u.loads.iter().copied()).collect()
}

/// Where the input of `driver` that arrives last comes from (its driver's
/// tile), or `home` (where the driver is) when no input has a placed driver
fn latest_input_at(
    netlist: &GateNetlist,
    estimate: &SlackEstimate,
    driver: usize,
    position: &impl Fn(CellId) -> Option<(f64, f64)>,
    home: (f64, f64),
) -> (f64, f64) {
    netlist.cells[driver]
        .inputs
        .iter()
        .filter_map(|&input| {
            let input = netlist.resolve_alias(input);
            let from = netlist.nets[input.0 as usize].driver.and_then(position)?;
            let arrival = estimate
                .edges
                .iter()
                .map(|t| t.arrival[input.0 as usize])
                .fold(f64::NEG_INFINITY, f64::max);
            Some((arrival, from))
        })
        .max_by(|a, b| a.0.total_cmp(&b.0))
        .map_or(home, |(_, from)| from)
}

/// Split `units` by where they are, `levels` times over, into groups:
/// halve along the longer side of the units' bounding box, at the
/// load-weighted median. A group of at most `max_loads` loads, or of one
/// unit, is not split further.
fn split(mut units: Vec<Unit>, max_loads: usize, levels: usize) -> Vec<Vec<Unit>> {
    let total: usize = units.iter().map(|u| u.loads.len()).sum();
    if levels == 0 || total <= max_loads || units.len() < 2 {
        return vec![units];
    }
    let span = |f: fn(&Unit) -> f64| {
        let (lo, hi) = units
            .iter()
            .map(f)
            .fold((f64::INFINITY, f64::NEG_INFINITY), |(lo, hi), v| {
                (lo.min(v), hi.max(v))
            });
        hi - lo
    };
    let along_x = span(|u| u.at.0) >= span(|u| u.at.1);
    units.sort_by(|a, b| {
        let (ka, kb) = if along_x {
            ((a.at.0, a.at.1), (b.at.0, b.at.1))
        } else {
            ((a.at.1, a.at.0), (b.at.1, b.at.0))
        };
        ka.0.total_cmp(&kb.0)
            .then(ka.1.total_cmp(&kb.1))
            .then(a.loads[0].cell.cmp(&b.loads[0].cell))
    });
    let mut taken = 0;
    let mut cut = units.len() - 1;
    for (i, unit) in units.iter().enumerate() {
        taken += unit.loads.len();
        if taken * 2 >= total {
            cut = (i + 1).min(units.len() - 1);
            break;
        }
    }
    let high = units.split_off(cut);
    let mut groups = split(units, max_loads, levels - 1);
    groups.extend(split(high, max_loads, levels - 1));
    groups
}

/// The middle of a group's bounding box: where a driver is no further than
/// it must be from its furthest load
fn middle(group: &[Unit]) -> (f64, f64) {
    let (mut x0, mut y0, mut x1, mut y1) = (
        f64::INFINITY,
        f64::INFINITY,
        f64::NEG_INFINITY,
        f64::NEG_INFINITY,
    );
    for u in group {
        x0 = x0.min(u.at.0);
        y0 = y0.min(u.at.1);
        x1 = x1.max(u.at.0);
        y1 = y1.max(u.at.1);
    }
    ((x0 + x1) / 2.0, (y0 + y1) / 2.0)
}

fn distance(a: (f64, f64), b: (f64, f64)) -> f64 {
    (a.0 - b.0).abs() + (a.1 - b.1).abs()
}

/// Add copy number `k` of cell `driver` to `netlist`, driving `loads`
/// in place of the original. Returns the copy's id.
fn add_replica(netlist: &mut GateNetlist, driver: usize, k: usize, loads: &[Load]) -> CellId {
    let original = netlist.cells[driver].clone();
    let q = original.outputs[0];
    let base = netlist.nets[q.0 as usize].name.clone();
    let mut name = format!("{base}__rep{k}");
    let mut n = k;
    while netlist.get_net_id(&name).is_some() {
        n += 1;
        name = format!("{base}__rep{n}");
    }
    let net = netlist.add_net(GateNet::new(GateNetId(0), name));
    let mut copy: Cell = original.clone();
    copy.path = format!("{}__rep{}", original.path, n);
    copy.outputs = vec![net];
    copy.parameters
        .insert(REPLICA_OF_PARAM.to_string(), original.path.clone());
    let id = netlist.add_cell(copy);
    for load in loads {
        let cell_id = netlist.cells[load.cell].id;
        match load.pin {
            LoadPin::Input(pin) => {
                let old = netlist.cells[load.cell].inputs[pin];
                netlist.cells[load.cell].inputs[pin] = net;
                if let Some(old) = netlist.nets.get_mut(old.0 as usize) {
                    if let Some(at) = old.fanout.iter().position(|&f| f == (cell_id, pin)) {
                        old.fanout.remove(at);
                    }
                }
                netlist.nets[net.0 as usize].fanout.push((cell_id, pin));
            }
            LoadPin::Reset => netlist.cells[load.cell].reset = Some(net),
        }
    }
    id
}

#[cfg(test)]
mod tests {
    use super::*;

    fn unit(cell: usize, x: f64, y: f64) -> Unit {
        Unit {
            loads: vec![Load {
                cell,
                pin: LoadPin::Input(0),
            }],
            at: (x, y),
        }
    }

    #[test]
    fn split_halves_along_the_longer_side_until_groups_fit() {
        let units: Vec<Unit> = (0..64)
            .map(|i| unit(i, (i % 16) as f64 * 4.0, (i / 16) as f64))
            .collect();
        let groups = split(units, 16, MAX_SPLIT_LEVELS);
        assert_eq!(groups.len(), 4);
        for g in &groups {
            assert_eq!(g.iter().map(|u| u.loads.len()).sum::<usize>(), 16);
            // Cut along x (64 tiles wide, 4 tall): each group spans 16 tiles
            let xs: Vec<f64> = g.iter().map(|u| u.at.0).collect();
            let span = xs.iter().cloned().fold(f64::NEG_INFINITY, f64::max)
                - xs.iter().cloned().fold(f64::INFINITY, f64::min);
            assert!(span <= 12.0, "group spans {span} tiles");
        }
    }

    #[test]
    fn split_stops_after_the_levels_asked_for() {
        let units: Vec<Unit> = (0..64).map(|i| unit(i, i as f64, 0.0)).collect();
        assert_eq!(split(units.clone(), 1, 0).len(), 1);
        assert_eq!(split(units.clone(), 1, 1).len(), 2);
        assert_eq!(split(units.clone(), 1, 3).len(), 8);
        // The load limit stops it first
        assert_eq!(split(units, 16, 8).len(), 4);
    }

    #[test]
    fn a_unit_bigger_than_the_limit_stays_whole() {
        let mut big = unit(0, 0.0, 0.0);
        for c in 1..40 {
            big.loads.push(Load {
                cell: c,
                pin: LoadPin::Input(1),
            });
        }
        let groups = split(vec![big, unit(50, 10.0, 0.0)], 16, MAX_SPLIT_LEVELS);
        assert_eq!(groups.len(), 2);
        assert_eq!(groups[0][0].loads.len(), 40);
    }

    /// Logic tiles of a LIFCL-40, column by column, with their BELs' types,
    /// and the index of the `n`th BEL of a type in a tile's list
    fn logic_tiles(
        device: &crate::device::nexus::NexusDevice,
    ) -> Vec<(u32, u32, Vec<crate::placer::BelType>)> {
        let (w, h) = device.grid_size();
        let mut tiles = Vec::new();
        for x in 0..w {
            for y in 0..h {
                let Some(tile) = device.tile_at(x, y) else {
                    continue;
                };
                let bels: Vec<crate::placer::BelType> =
                    tile.bels().iter().map(|b| b.bel_type).collect();
                if bels.contains(&crate::placer::BelType::WideLut) {
                    tiles.push((x, y, bels));
                }
            }
        }
        tiles
    }

    fn nth_bel(bels: &[crate::placer::BelType], t: crate::placer::BelType, n: usize) -> usize {
        bels.iter()
            .enumerate()
            .filter(|(_, &b)| b == t || (t == crate::placer::BelType::Dff && b.is_ff()))
            .nth(n)
            .map(|(i, _)| i)
            .unwrap()
    }

    /// A rising-edge register whose output crosses the die through three
    /// LUTs to a LUT driving 64 registers, half beside it at the die's
    /// left edge and half at its right edge: the far half end a path of
    /// about 6 ns. The 64 registers load on the falling edge with
    /// `falling`.
    fn half_cycle_design(falling: bool) -> (GateNetlist, PlacementResult) {
        use crate::device::nexus::{NexusDevice, NexusVariant};
        use crate::placer::BelType;
        use skalp_lir::gate_netlist::{FF_CLOCK_EDGE_PARAM, FF_CLOCK_FALLING};

        let device = NexusDevice::new(NexusVariant::Lifcl40);
        let tiles = logic_tiles(&device);
        let mut n = GateNetlist::new("t".to_string(), "test".to_string());
        let net = |n: &mut GateNetlist, name: &str| {
            n.add_net(GateNet::new(GateNetId(0), name.to_string()))
        };
        let cell = |kind: &str, name: &str, inputs: Vec<GateNetId>, out: GateNetId| {
            Cell::new_comb(
                CellId(0),
                kind.to_string(),
                "nexus".to_string(),
                0.0,
                name.to_string(),
                inputs,
                vec![out],
            )
        };
        let mut placed = PlacementResult::new();
        let mut put = |id: CellId, tile: &(u32, u32, Vec<BelType>), t: BelType, k: usize| {
            placed.placements.insert(
                id,
                PlacementLoc::new(tile.0, tile.1, nth_bel(&tile.2, t, k), t),
            );
        };
        let clk = net(&mut n, "clk");
        let a = net(&mut n, "a");
        let (left, right) = (&tiles[0], &tiles[tiles.len() - 1]);
        // src (left) -> l1 (right) -> l2 (left) -> l3 (right) -> we (left)
        let q = net(&mut n, "q");
        let mut src = cell("OXIDE_FF", "src", vec![a], q);
        src.clock = Some(clk);
        let src = n.add_cell(src);
        put(src, left, BelType::Dff, 0);
        let mut x2 = q;
        for (k, tile) in [right, left, right].into_iter().enumerate() {
            let x = net(&mut n, &format!("x{k}"));
            let l = n.add_cell(cell("OXIDE_COMB", &format!("l{k}"), vec![x2], x));
            put(l, tile, BelType::Lut4, 2 + k);
            x2 = x;
        }
        let we = net(&mut n, "we");
        let mut driver = cell("OXIDE_COMB", "we", vec![x2], we);
        driver.function = Some(skalp_lir::tech_library::CellFunction::Inv);
        let driver = n.add_cell(driver);
        put(driver, left, BelType::Lut4, 1);
        let sides = tiles[1..33]
            .iter()
            .chain(tiles[tiles.len() - 33..tiles.len() - 1].iter());
        for (i, tile) in sides.enumerate() {
            let o = net(&mut n, &format!("o{i}"));
            let mut load = cell("OXIDE_FF", &format!("load{i}"), vec![we], o);
            load.clock = Some(clk);
            if falling {
                load.parameters.insert(
                    FF_CLOCK_EDGE_PARAM.to_string(),
                    FF_CLOCK_FALLING.to_string(),
                );
            }
            let load = n.add_cell(load);
            put(load, tile, BelType::Dff, 0);
        }
        (n, placed)
    }

    /// A path from a rising-edge register to falling-edge ones has half a
    /// period, as the analyzer after routing has it: at 100 MHz a 6 ns path
    /// meets a whole period but fails half of one, so its driver is copied
    /// only when the loads are on the falling edge
    #[test]
    fn a_rising_to_falling_path_gets_half_a_period() {
        use crate::device::nexus::{NexusDevice, NexusVariant};
        use crate::placer::PlacerConfig;

        let device = NexusDevice::new(NexusVariant::Lifcl40);
        let placer = Placer::new(PlacerConfig::default(), device);
        let clocks = ClockTargets::uniform(100.0);
        let model = placer.delay_model();
        // The far loads' path: 5 to 10 ns from the source's clock edge
        let (rising, placed) = half_cycle_design(false);
        let position = |id: CellId| {
            placed
                .get(id)
                .map(|loc| (loc.tile_x as f64, loc.tile_y as f64))
        };
        let estimate = estimate_slacks(&rising, &model, position, &clocks);
        assert_eq!(estimate.edges.len(), 1);
        let worst = estimate.edges[0]
            .input_required
            .iter()
            .zip(&rising.cells)
            .filter(|(_, c)| c.path.starts_with("load"))
            .map(|(&required, c)| {
                let d = rising.nets[c.inputs[0].0 as usize].driver.unwrap();
                let (from, to) = (position(d).unwrap(), position(c.id).unwrap());
                let wire = model
                    .connection_delay((from.0 - to.0).abs() as u32, (from.1 - to.1).abs() as u32);
                required - estimate.edges[0].arrival[c.inputs[0].0 as usize] - wire
            })
            .fold(f64::INFINITY, f64::min);
        let delay = 10.0 - model.dff_setup - worst;
        assert!(
            (5.5..9.0).contains(&delay),
            "the far loads' path takes {delay:.2} ns"
        );
        assert!(
            placer
                .replicate_high_fanout(&rising, &placed, &clocks)
                .unwrap()
                .is_none(),
            "replicated a path that meets a whole period"
        );

        let (falling, placed) = half_cycle_design(true);
        let found = placer
            .replicate_high_fanout(&falling, &placed, &clocks)
            .unwrap()
            .expect("the half-period path's driver was not replicated");
        let we = &found.drivers[0];
        assert_eq!(we.path, "we");
        assert!(we.slack_before_ns < 0.0, "{we:?}");
        assert!(we.slack_after_ns > we.slack_before_ns, "{we:?}");
    }

    /// A wide LUT (two LUTs and the flip-flop of a slice) driving loads far
    /// across the die is copied onto wide-LUT sites whose LUTs and
    /// flip-flop no placed cell uses
    #[test]
    fn a_wide_lut_is_copied_onto_free_slices() {
        use crate::device::nexus::{NexusDevice, NexusVariant};
        use crate::placer::{BelType, PlacerConfig};
        use skalp_lir::tech_library::CellFunction;

        let device = NexusDevice::new(NexusVariant::Lifcl40);
        let (w, h) = device.grid_size();
        // Logic tiles, by column, with their BELs' types
        let mut tiles: Vec<(u32, u32, Vec<BelType>)> = Vec::new();
        for x in 0..w {
            for y in 0..h {
                let Some(tile) = device.tile_at(x, y) else {
                    continue;
                };
                let bels: Vec<BelType> = tile.bels().iter().map(|b| b.bel_type).collect();
                if bels.contains(&BelType::WideLut) {
                    tiles.push((x, y, bels));
                }
            }
        }
        let bel = |bels: &[BelType], t: BelType, n: usize| {
            bels.iter()
                .enumerate()
                .filter(|(_, &b)| b == t || (t == BelType::Dff && b.is_ff()))
                .nth(n)
                .map(|(i, _)| i)
                .unwrap()
        };

        let mut n = GateNetlist::new("t".to_string(), "test".to_string());
        let net = |n: &mut GateNetlist, name: &str| {
            n.add_net(GateNet::new(GateNetId(0), name.to_string()))
        };
        let cell = |kind: &str, name: &str, inputs: Vec<GateNetId>, out: GateNetId| {
            Cell::new_comb(
                CellId(0),
                kind.to_string(),
                "nexus".to_string(),
                0.0,
                name.to_string(),
                inputs,
                vec![out],
            )
        };
        let clk = net(&mut n, "clk");
        let a = net(&mut n, "a");
        let s = net(&mut n, "s");
        let wide = net(&mut n, "wide");
        let mut placed = PlacementResult::new();
        // The source in the middle of the die, the wide LUT at its left
        // edge beside half of its loads, the other half at the right edge
        let (x0, y0, bels0) = &tiles[tiles.len() / 2];
        let (xw, yw, belsw) = &tiles[0];
        let mut src = cell("OXIDE_FF", "src", vec![a], s);
        src.clock = Some(clk);
        let src = n.add_cell(src);
        placed.placements.insert(
            src,
            PlacementLoc::new(*x0, *y0, bel(bels0, BelType::Dff, 1), BelType::Dff),
        );
        let mut driver = cell("OXIDE_LUT5", "wide", vec![s, s, s, s, s], wide);
        driver.function = Some(CellFunction::Lut5);
        let driver = n.add_cell(driver);
        placed.placements.insert(
            driver,
            PlacementLoc::new(*xw, *yw, bel(belsw, BelType::WideLut, 0), BelType::WideLut),
        );
        // 64 loads, and in each of their tiles the first LUT taken, which
        // rules out the first slice's wide LUT there
        let far: Vec<&(u32, u32, Vec<BelType>)> = tiles
            .iter()
            .skip(1)
            .step_by(3)
            .take(32)
            .chain(tiles.iter().rev().step_by(3).take(32))
            .collect();
        for (i, (x, y, bels)) in far.into_iter().enumerate() {
            let q = net(&mut n, &format!("q{i}"));
            let mut load = cell("OXIDE_FF", &format!("load{i}"), vec![wide], q);
            load.clock = Some(clk);
            let load = n.add_cell(load);
            placed.placements.insert(
                load,
                PlacementLoc::new(*x, *y, bel(bels, BelType::Dff, 3), BelType::Dff),
            );
            let o = net(&mut n, &format!("f{i}"));
            let filler = n.add_cell(cell("OXIDE_COMB", &format!("filler{i}"), vec![a], o));
            placed.placements.insert(
                filler,
                PlacementLoc::new(*x, *y, bel(bels, BelType::Lut4, 0), BelType::Lut4),
            );
        }

        let placer = Placer::new(PlacerConfig::default(), device.clone());
        let found = placer
            .replicate_high_fanout(&n, &placed, &ClockTargets::uniform(1000.0))
            .unwrap()
            .expect("the wide LUT was not replicated");
        let copies: Vec<&Cell> = found.netlist.cells[n.cells.len()..].iter().collect();
        assert!(!copies.is_empty());
        let mut used = std::collections::HashSet::new();
        for (_, loc) in placed.cells() {
            used.insert((loc.tile_x, loc.tile_y, loc.bel_index));
        }
        for copy in copies {
            assert_eq!(copy.function, Some(CellFunction::Lut5));
            let loc = found.placement.get(copy.id).expect("copy placed");
            assert_eq!(
                loc.bel_type,
                BelType::WideLut,
                "{} not on a wide LUT",
                copy.path
            );
            assert!(used.insert((loc.tile_x, loc.tile_y, loc.bel_index)));
            for b in device.bel_footprint(loc.tile_x, loc.tile_y, loc.bel_index) {
                assert!(
                    !used.contains(&(loc.tile_x, loc.tile_y, b)),
                    "{} at ({}, {}) shares BEL {b} with a placed cell",
                    copy.path,
                    loc.tile_x,
                    loc.tile_y
                );
            }
        }
    }
}
