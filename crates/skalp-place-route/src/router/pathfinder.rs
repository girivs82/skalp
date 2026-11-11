//! PathFinder Negotiated Congestion Router
//!
//! Implements the PathFinder algorithm for FPGA routing with congestion negotiation.
//! The algorithm:
//! 1. Route all nets (potentially with conflicts)
//! 2. Track congestion per wire
//! 3. Rip up congested nets and reroute with increased costs
//! 4. Iterate until no congestion

use super::astar::{AStarRouter, SearchBounds};
use super::{BranchInfo, Route, RoutingResult};
use crate::collections::{HashMap, HashMapExt, HashSet, HashSetExt};
use crate::device::{BelType, CellPin, Device, PipId, WireId};
use crate::error::{PlaceRouteError, Result};
use crate::placer::{PlacementLoc, PlacementResult};
use rayon::prelude::*;
use skalp_lir::gate_netlist::{CellId, GateNetId, GateNetlist};
use skalp_lir::ncl_constraints::MatchedDelayGroup;

/// How many tiles beyond a net's bounding box its route searches may go at
/// first. A net that stays congested gets twice the margin each time it is
/// rerouted, so it can detour further; a search that finds no path inside
/// its region retries over the whole device. (Capping the margin at 12
/// tiles left a 15K-cell core on the real LFCPNX-100 stuck at about 100 overused
/// wires: the congestion sits at the edge of the placed region, and nets
/// must detour well around it.)
const SEARCH_MARGIN: u32 = 3;

/// Most nets routed side by side in one batch ([`PathFinder::route_in_batches`])
const MAX_BATCH: usize = 256;

/// Least nets in a batch, however often its routes clash
const MIN_BATCH: usize = 8;

/// How many times a net is rerouted keeping its uncongested branches before
/// it is rerouted whole
const PARTIAL_REROUTES: u32 = 2;

/// Factor by which the present-congestion cost grows each rip-up iteration
const PRESENT_FACTOR_GROWTH: f64 = 1.3;

/// A net whose route has more wires than this starts each sink's search
/// only from the route wires near the sink ([`TreeIndex::seeds_near`]).
/// Starting from the whole route made a net's routing time grow with the
/// square of its fanout: a 7,300-sink reset took 25 s per routing pass.
const LOCAL_SEED_MIN_TREE: usize = 256;

/// Route wires a sink's search starts from, at the least, before the
/// region around the sink stops growing
const LOCAL_SEEDS: usize = 32;

/// A net's route wires by tile, to find the part of the route near a sink.
struct TreeIndex {
    by_tile: HashMap<(u32, u32), Vec<WireId>>,
    /// Bounding box of the tiles: (min x, max x, min y, max y)
    bbox: (u32, u32, u32, u32),
}

impl TreeIndex {
    fn new() -> Self {
        Self {
            by_tile: HashMap::new(),
            bbox: (u32::MAX, 0, u32::MAX, 0),
        }
    }

    fn insert(&mut self, tile: Option<(u32, u32)>, wire: WireId) {
        if let Some((x, y)) = tile {
            self.by_tile.entry((x, y)).or_default().push(wire);
            let b = &mut self.bbox;
            *b = (b.0.min(x), b.1.max(x), b.2.min(y), b.3.max(y));
        }
    }

    /// Route wires in the smallest square around `(x, y)` (side doubling
    /// from 3 tiles) holding at least [`LOCAL_SEEDS`] of them, or `None` once
    /// that square would hold the whole route anyway. A branch to the sink
    /// leaves the route close to it, so a search seeded there finds the same
    /// branches as one seeded with every wire, at a cost independent of the
    /// net's size.
    fn seeds_near(&self, x: u32, y: u32) -> Option<Vec<WireId>> {
        let (min_x, max_x, min_y, max_y) = self.bbox;
        let mut radius = 1u32;
        loop {
            let covers_all = x.saturating_sub(radius) <= min_x
                && x + radius >= max_x
                && y.saturating_sub(radius) <= min_y
                && y + radius >= max_y;
            if covers_all {
                return None;
            }
            let near = |tx: u32, ty: u32| tx.abs_diff(x) <= radius && ty.abs_diff(y) <= radius;
            let side = 2 * radius as usize + 1;
            let mut seeds = Vec::new();
            if side * side < self.by_tile.len() {
                for tx in x.saturating_sub(radius)..=x + radius {
                    for ty in y.saturating_sub(radius)..=y + radius {
                        if let Some(wires) = self.by_tile.get(&(tx, ty)) {
                            seeds.extend_from_slice(wires);
                        }
                    }
                }
            } else {
                for (&(tx, ty), wires) in &self.by_tile {
                    if near(tx, ty) {
                        seeds.extend_from_slice(wires);
                    }
                }
            }
            if seeds.len() >= LOCAL_SEEDS {
                // Same seeds in the same order on every run
                seeds.sort_unstable_by_key(|w| w.0);
                return Some(seeds);
            }
            radius *= 2;
        }
    }
}

/// Wire usage and PathFinder history cost, indexed by wire. Every routing
/// wire has capacity 1. Dense, because the router's search asks for the
/// cost of every wire it reaches; `touched` lists the wires ever used, so
/// the per-iteration scans do not walk the whole device.
struct CongestionTracker {
    /// Nets using each wire
    usage: Vec<u32>,
    /// Accumulated overuse per wire (PathFinder history)
    history: Vec<f64>,
    /// Whether a wire is in `touched`
    seen: Vec<bool>,
    /// Wires that have ever been used
    touched: Vec<WireId>,
}

impl CongestionTracker {
    fn new(wire_count: usize) -> Self {
        Self {
            usage: vec![0; wire_count],
            history: vec![0.0; wire_count],
            seen: vec![false; wire_count],
            touched: Vec::new(),
        }
    }

    /// Grow the tables for a device that did not report its wire count
    fn ensure(&mut self, wire: WireId) -> usize {
        let i = wire.0 as usize;
        if i >= self.usage.len() {
            self.usage.resize(i + 1, 0);
            self.history.resize(i + 1, 0.0);
            self.seen.resize(i + 1, false);
        }
        i
    }

    /// Add usage for a wire
    fn add_usage(&mut self, wire: WireId) {
        let i = self.ensure(wire);
        self.usage[i] += 1;
        if !self.seen[i] {
            self.seen[i] = true;
            self.touched.push(wire);
        }
    }

    /// Nets using a wire
    fn usage(&self, wire: WireId) -> u32 {
        self.usage.get(wire.0 as usize).copied().unwrap_or(0)
    }

    /// Remove usage for a wire
    fn remove_usage(&mut self, wire: WireId) {
        if let Some(u) = self.usage.get_mut(wire.0 as usize) {
            *u = u.saturating_sub(1);
        }
    }

    /// Update history costs for congested wires
    fn update_history(&mut self) {
        for &w in &self.touched {
            let i = w.0 as usize;
            if self.usage[i] > 1 {
                self.history[i] += (self.usage[i] - 1) as f64;
            }
        }
    }

    /// Get maximum congestion
    fn max_congestion(&self) -> f64 {
        self.touched
            .iter()
            .map(|w| self.usage[w.0 as usize])
            .max()
            .unwrap_or(0) as f64
    }

    /// Sum over wires of nets beyond capacity
    fn total_overuse(&self) -> u64 {
        self.touched
            .iter()
            .map(|w| self.usage[w.0 as usize].saturating_sub(1) as u64)
            .sum()
    }

    /// Check if any wire is overused
    fn has_overuse(&self) -> bool {
        self.touched.iter().any(|w| self.usage[w.0 as usize] > 1)
    }

    /// Get overused wires
    fn overused_wires(&self) -> Vec<WireId> {
        self.touched
            .iter()
            .copied()
            .filter(|w| self.usage[w.0 as usize] > 1)
            .collect()
    }
}

/// The congestion one net is routed against: every net's current route
/// but its own (`own`, the wires of its route still counted in `tracker`)
#[derive(Clone, Copy)]
struct NetView<'a> {
    tracker: &'a CongestionTracker,
    own: Option<&'a HashSet<WireId>>,
}

impl NetView<'_> {
    /// Nets other than this one using `wire`
    fn usage(&self, wire: WireId) -> u32 {
        self.tracker.usage(wire) - self.own_use(wire)
    }

    fn own_use(&self, wire: WireId) -> u32 {
        u32::from(self.own.is_some_and(|own| own.contains(&wire)))
    }

    /// Cost multiplier for this net using `wire` (PathFinder):
    /// `(1 + history_factor * history) * (1 + present_factor * overuse)`,
    /// where `overuse` is how far over capacity (1) the wire would be with
    /// this net added. An unused wire with no history costs 1.
    fn cost_factor(&self, wire: WireId, present_factor: f64, history_factor: f64) -> f64 {
        let i = wire.0 as usize;
        let usage = self.usage(wire);
        let history = self.tracker.history.get(i).copied().unwrap_or(0.0);
        (1.0 + history_factor * history) * (1.0 + present_factor * usage as f64)
    }
}

/// Net criticality information for timing-driven routing
#[derive(Debug, Clone)]
struct NetCriticality {
    /// Net ID
    net_id: GateNetId,
    /// Criticality score (0.0 = not critical, 1.0 = most critical)
    criticality: f64,
    /// Estimated slack in ns (negative = timing violation)
    #[allow(dead_code)]
    slack: f64,
    /// Bounding box size
    bbox: u32,
}

/// PathFinder router
pub struct PathFinder<'a, D: Device> {
    device: &'a D,
    max_iterations: usize,
    history_factor: f64,
    present_factor: f64,
    /// Timing weight for timing-driven routing (0.0 = ignore timing, 1.0 = full timing)
    timing_weight: f64,
}

impl<'a, D: Device + Clone> PathFinder<'a, D> {
    /// Create a new PathFinder router
    pub fn new(
        device: &'a D,
        max_iterations: usize,
        history_factor: f64,
        present_factor: f64,
    ) -> Self {
        Self {
            device,
            max_iterations,
            history_factor,
            present_factor,
            timing_weight: 0.0, // Default: no timing-driven routing
        }
    }

    /// Create a timing-driven PathFinder router
    pub fn new_timing_driven(
        device: &'a D,
        max_iterations: usize,
        history_factor: f64,
        present_factor: f64,
        timing_weight: f64,
    ) -> Self {
        Self {
            device,
            max_iterations,
            history_factor,
            present_factor,
            timing_weight,
        }
    }

    /// Route every net that has a driver and a sink using PathFinder
    pub fn route(
        &self,
        netlist: &GateNetlist,
        placement: &PlacementResult,
    ) -> Result<RoutingResult> {
        let nets: Vec<GateNetId> = netlist.nets.iter().map(|n| n.id).collect();
        self.route_nets(netlist, placement, &nets)
    }

    /// Route the nets `nets` (those with a driver and a sink) using
    /// PathFinder. Nets another router handles (global networks, carry
    /// chains) must be left out: routing them here as well would spend time
    /// on routes that are thrown away, and their wires would count as
    /// congestion against the nets that keep theirs.
    pub fn route_nets(
        &self,
        netlist: &GateNetlist,
        placement: &PlacementResult,
        nets: &[GateNetId],
    ) -> Result<RoutingResult> {
        let mut result = RoutingResult::new();
        let mut congestion = CongestionTracker::new(self.device.wire_count());
        let astar = AStarRouter::with_delay_model(
            self.device,
            crate::timing::DelayModel::for_family(self.device.family()),
        );

        // Build matched-delay lookup from NCL constraints (empty for non-NCL designs)
        let matched_delay_lookup: HashMap<GateNetId, &MatchedDelayGroup> = netlist
            .ncl_constraints
            .as_ref()
            .map(|c| {
                c.matched_delay_groups
                    .iter()
                    .map(|g| (g.fork_net, g))
                    .collect()
            })
            .unwrap_or_default();

        // Calculate net criticality for timing-driven routing
        let mut net_criticalities = self.calculate_net_criticalities(netlist, placement);

        // Boost criticality of NCL matched-delay nets for routing priority
        for crit in &mut net_criticalities {
            if matched_delay_lookup.contains_key(&crit.net_id) {
                crit.criticality = crit.criticality.max(0.8);
            }
        }

        // First entry per net (as a linear search would find)
        let mut criticality_by_net: HashMap<GateNetId, usize> = HashMap::new();
        for (i, c) in net_criticalities.iter().enumerate() {
            criticality_by_net.entry(c.net_id).or_insert(i);
        }

        // Collect nets to route with their criticality score
        // Clock/reset nets are routed here when GBUF routing is unavailable
        let mut nets_with_criticality: Vec<_> = nets
            .iter()
            .filter_map(|id| netlist.nets.get(id.0 as usize))
            .filter(|net| net.driver.is_some() && !net.fanout.is_empty())
            .map(|net| {
                let crit = criticality_by_net
                    .get(&net.id)
                    .map(|&i| net_criticalities[i].clone())
                    .unwrap_or(NetCriticality {
                        net_id: net.id,
                        criticality: 0.0,
                        slack: f64::MAX,
                        bbox: self.calculate_net_bbox(net, placement),
                    });
                crit
            })
            .collect();

        // Sort by criticality (most critical first) for timing-driven routing
        // Fall back to bbox for non-timing-driven routing
        if self.timing_weight > 0.0 {
            // Timing-driven: route most critical nets first
            nets_with_criticality.sort_by(|a, b| {
                b.criticality
                    .partial_cmp(&a.criticality)
                    .unwrap_or(std::cmp::Ordering::Equal)
            });
        } else {
            // Non-timing-driven: route by bbox size (larger first)
            nets_with_criticality.sort_by_key(|x| std::cmp::Reverse(x.bbox));
        }

        let nets_to_route: Vec<_> = nets_with_criticality.iter().map(|c| c.net_id).collect();

        // Build criticality map for cost calculations
        let criticality_map: HashMap<GateNetId, f64> = nets_with_criticality
            .iter()
            .map(|c| (c.net_id, c.criticality))
            .collect();

        // Initial routing - use congestion-aware routing from the start
        // This reduces the number of conflicts that need to be resolved
        let t_start = std::time::Instant::now();
        let initial: Vec<NetJob> = nets_to_route
            .iter()
            .map(|&net_id| NetJob {
                net_id,
                margin: SEARCH_MARGIN,
                keep: false,
            })
            .collect();
        self.route_in_batches(
            initial,
            netlist,
            placement,
            &mut congestion,
            &astar,
            &criticality_map,
            &matched_delay_lookup,
            (self.present_factor, self.history_factor),
            &mut result,
        )?;

        // Iterative rip-up and reroute with convergence detection
        // Classic PathFinder: escalate present_factor each iteration to force
        // nets off congested wires
        let mut congestion_history: Vec<f64> = Vec::new();
        // Times each net has been rerouted
        let mut reroute_counts: HashMap<GateNetId, u32> = HashMap::new();
        let base_present_factor = self.present_factor;
        let base_history_factor = self.history_factor;

        for iteration in 0..self.max_iterations {
            log::info!(
                "pathfinder: iteration {iteration} at {:.1} s: {} overused wires, total overuse {}",
                t_start.elapsed().as_secs_f64(),
                congestion.overused_wires().len(),
                congestion.total_overuse()
            );
            if log::log_enabled!(log::Level::Debug) {
                // By wire class (the same wire in every tile), most first
                let mut by_class: HashMap<String, usize> = HashMap::new();
                for w in congestion.overused_wires() {
                    let class = self
                        .device
                        .wire(w)
                        .map_or("?".to_string(), |w| super::lookahead::class_key(&w.name));
                    *by_class.entry(class).or_default() += 1;
                }
                let mut by_class: Vec<(String, usize)> = by_class.into_iter().collect();
                by_class.sort_by(|a, b| b.1.cmp(&a.1).then_with(|| a.0.cmp(&b.0)));
                by_class.truncate(30);
                log::debug!("pathfinder: overused wires by class: {by_class:?}");
                // Where they are: by 8x8-tile block, most first
                let mut by_block: HashMap<(u32, u32), usize> = HashMap::new();
                for w in congestion.overused_wires() {
                    if let Some(wire) = self.device.wire(w) {
                        *by_block
                            .entry((wire.tile_x / 8 * 8, wire.tile_y / 8 * 8))
                            .or_default() += 1;
                    }
                }
                let mut by_block: Vec<((u32, u32), usize)> = by_block.into_iter().collect();
                by_block.sort_by(|a, b| b.1.cmp(&a.1).then_with(|| a.0.cmp(&b.0)));
                by_block.truncate(20);
                log::debug!("pathfinder: overused wires by 8x8 block (x, y): {by_block:?}");
                let sample: Vec<String> = congestion
                    .overused_wires()
                    .into_iter()
                    .take(40)
                    .filter_map(|w| self.device.wire(w).map(|wire| wire.name.clone()))
                    .collect();
                log::debug!("pathfinder: some overused wires: {sample:?}");
            }
            // Check convergence: no congestion
            if !congestion.has_overuse() {
                result.iterations = iteration;
                break;
            }

            // Stall check, after at least 15 iterations: stop once the best
            // total overuse of the last 8 iterations is under 2% below the
            // best before them. The best, not the latest: as the present
            // factor climbs, overuse wanders up and down while still falling
            // overall, and comparing two single iterations stopped such runs
            // at an upswing. (The maximum overuse is no measure of progress:
            // it sits at 2 while the number of shared wires falls.)
            congestion_history.push(congestion.total_overuse() as f64);
            if congestion_history.len() >= 15 && stalled(&congestion_history, 8, 0.02) {
                result.iterations = iteration;
                break;
            }

            // PathFinder: the cost of sharing a wire grows geometrically, so a
            // dense region that only resolves through long detours still
            // converges; the history term grows linearly.
            let iter_present_factor =
                base_present_factor * PRESENT_FACTOR_GROWTH.powi(iteration as i32);
            let iter_history_factor = base_history_factor * (1.0 + iteration as f64 * 0.2);

            // Update history costs BEFORE rip-up so we capture the actual congestion
            congestion.update_history();

            // Find nets needing reroute: congested wires OR skew violations
            let overused = congestion.overused_wires();
            let overused_set: HashSet<_> = overused.iter().copied().collect();

            let reroute_nets: HashSet<GateNetId> = result
                .routes
                .iter()
                .filter(|(_, route)| route.wires.iter().any(|w| overused_set.contains(w)))
                .map(|(&id, _)| id)
                .collect();

            // In the initial routing order (largest or most critical first),
            // so a run is deterministic
            let reroute_list: Vec<GateNetId> = nets_to_route
                .iter()
                .copied()
                .filter(|id| reroute_nets.contains(id))
                .collect();
            log::info!("pathfinder: rerouting {} nets", reroute_list.len());

            // Rip up and reroute each net against the current routes of all
            // the others, as if one after another ([`Self::route_in_batches`]).
            // (Ripping up every congested net first
            // let the early ones spread into wires the rest still needed,
            // and congestion barely fell.)
            let jobs: Vec<NetJob> = reroute_list
                .iter()
                .map(|&net_id| {
                    // A net still congested gets a wider search region each
                    // time. Its first reroutes keep the branches clear of
                    // other nets; one that stays congested is rerouted whole,
                    // since the branches it keeps may be what blocks the
                    // others.
                    let reroutes = reroute_counts.entry(net_id).or_insert(0);
                    *reroutes += 1;
                    let margin = SEARCH_MARGIN.saturating_mul(1 << (*reroutes).min(16));
                    NetJob {
                        net_id,
                        margin,
                        keep: *reroutes <= PARTIAL_REROUTES,
                    }
                })
                .collect();
            self.route_in_batches(
                jobs,
                netlist,
                placement,
                &mut congestion,
                &astar,
                &criticality_map,
                &matched_delay_lookup,
                (iter_present_factor, iter_history_factor),
                &mut result,
            )?;

            result.iterations = iteration + 1;
        }

        // Calculate final metrics
        result.congestion = congestion.max_congestion();
        result.wirelength = result.routes.values().map(|r| r.wires.len() as u64).sum();

        // Report final skew violations (total: cell + wire delay)
        for (&net_id, group) in &matched_delay_lookup {
            if let Some(route) = result.routes.get(&net_id) {
                let skew = Self::total_branch_skew(route, group);
                if skew > group.max_skew_ps {
                    result.skew_violations += 1;
                    result.max_skew_violation_ps = result.max_skew_violation_ps.max(skew);
                }
            }
        }

        result.unrouted_connections = result.routes.values().map(|r| r.unrouted_sinks).sum();
        if result.unrouted_connections > 0 {
            let mut nets: Vec<&str> = result
                .routes
                .iter()
                .filter(|(_, r)| r.unrouted_sinks > 0)
                .filter_map(|(id, _)| netlist.nets.get(id.0 as usize).map(|n| n.name.as_str()))
                .collect();
            nets.sort_unstable();
            nets.truncate(10);
            log::warn!(
                "routing: {} sink connections have no path (nets {})",
                result.unrouted_connections,
                nets.join(", ")
            );
        }

        // Success requires no wire overuse and every connection routed.
        // Skew is handled by delaying the ready/completion signal, not by the router.
        // Skew metrics are reported for diagnostics only.
        result.success = result.congestion <= 1.0 && result.unrouted_connections == 0;

        Ok(result)
    }

    /// Reroute `jobs` (in routing order), replacing each net's current
    /// route, much as if they were routed one after another.
    ///
    /// Nets go in batches that are routed in parallel, each against the
    /// routes as they stand less its own, then recorded in order. A net
    /// whose new route takes a wire that a net before it in the batch has
    /// just taken was routed without seeing that net's route, so it is not
    /// recorded: it keeps its old route and goes to the front of the next
    /// batch ([`settle_batch`]). Every other route is the one a search after
    /// the earlier nets would have found, give or take wires their old
    /// routes freed. (Ripping up a whole batch first let its early nets
    /// spread into the wires the later ones were using, and the late passes
    /// stopped converging; batching only nets whose search regions don't
    /// overlap left those passes routing one net at a time.) A batch
    /// shrinks while its nets keep clashing and grows back when they stop.
    /// The batches depend only on the routes, never on the number of
    /// threads, so the result is the same on any machine.
    #[allow(clippy::too_many_arguments)]
    fn route_in_batches(
        &self,
        jobs: Vec<NetJob>,
        netlist: &GateNetlist,
        placement: &PlacementResult,
        congestion: &mut CongestionTracker,
        astar: &AStarRouter<D>,
        criticality_map: &HashMap<GateNetId, f64>,
        matched_delay_lookup: &HashMap<GateNetId, &MatchedDelayGroup>,
        (present_factor, history_factor): (f64, f64),
        result: &mut RoutingResult,
    ) -> Result<()> {
        let total = jobs.len();
        let mut pending: std::collections::VecDeque<NetJob> = jobs.into();
        let mut size = MAX_BATCH;
        let (mut batches, mut deferred) = (0usize, 0usize);
        let t_start = std::time::Instant::now();
        // Search time of whole-net and partial reroutes, and the slowest nets
        let mut net_time = [0.0f64; 2];
        let mut slow: Vec<(f64, String, usize, u32, bool)> = Vec::new();
        while !pending.is_empty() {
            let take = size.min(pending.len());
            let batch: Vec<NetJob> = pending.drain(..take).collect();
            batches += 1;
            // Each net's current route stays in place while the batch is
            // routed, and each search discounts only its own
            let old: Vec<HashSet<WireId>> = batch
                .iter()
                .map(|job| {
                    result
                        .routes
                        .get(&job.net_id)
                        .map(|r| Self::shared_wires(r).into_iter().collect())
                        .unwrap_or_default()
                })
                .collect();
            let frozen: &CongestionTracker = congestion;
            let routes: Vec<(Result<Route>, f64)> = batch
                .par_iter()
                .zip(old.par_iter())
                .map(|(job, own)| {
                    let t_net = std::time::Instant::now();
                    let route = self.route_net_with_costs_internal(
                        job.net_id,
                        netlist,
                        placement,
                        NetView {
                            tracker: frozen,
                            own: Some(own),
                        },
                        astar,
                        criticality_map.get(&job.net_id).copied().unwrap_or(0.0),
                        present_factor,
                        history_factor,
                        matched_delay_lookup.contains_key(&job.net_id),
                        job.margin,
                        result.routes.get(&job.net_id).filter(|_| job.keep),
                    );
                    (route, t_net.elapsed().as_secs_f64())
                })
                .collect();
            let mut new = Vec::with_capacity(routes.len());
            for (job, (route, secs)) in batch.iter().zip(routes) {
                net_time[usize::from(job.keep)] += secs;
                if secs > 1.0 {
                    let net = &netlist.nets[job.net_id.0 as usize];
                    slow.push((
                        secs,
                        net.name.clone(),
                        net.fanout.len(),
                        job.margin,
                        job.keep,
                    ));
                }
                new.push(route?);
            }
            let shared: Vec<Vec<WireId>> = new.iter().map(Self::shared_wires).collect();
            let commit = settle_batch(&shared, &old);
            let mut retry = Vec::new();
            for ((job, route), ((wires, own), commit)) in batch
                .into_iter()
                .zip(new)
                .zip(shared.into_iter().zip(old).zip(commit))
            {
                if commit {
                    for &wire in &own {
                        congestion.remove_usage(wire);
                    }
                    for wire in wires {
                        congestion.add_usage(wire);
                    }
                    result.routes.insert(job.net_id, route);
                } else {
                    retry.push(job);
                }
            }
            deferred += retry.len();
            // Halve the batch while more than a quarter of it clashes;
            // double it while none does
            if retry.len() * 4 > take {
                size = (size / 2).max(MIN_BATCH);
            } else if retry.is_empty() {
                size = (size * 2).min(MAX_BATCH);
            }
            for job in retry.into_iter().rev() {
                pending.push_front(job);
            }
        }
        log::debug!(
            "pathfinder: routed {total} nets in {batches} batches in {:.1} s, \
             {deferred} routes clashed and were redone; searches took {:.1} s whole, \
             {:.1} s keeping branches",
            t_start.elapsed().as_secs_f64(),
            net_time[0],
            net_time[1],
        );
        slow.sort_by(|a, b| b.0.total_cmp(&a.0));
        slow.truncate(15);
        for (secs, name, sinks, margin, keep) in slow {
            log::debug!(
                "pathfinder: net {name} ({sinks} sinks, margin {margin}, keep {keep}) took {secs:.1} s"
            );
        }
        Ok(())
    }

    /// The routing wires a route occupies: all but its source and sink wires,
    /// which are BEL pin wires dedicated to their cells, not shared routing
    /// resources.
    fn shared_wires(route: &Route) -> Vec<WireId> {
        let sinks: HashSet<WireId> = route.sinks.iter().copied().collect();
        route
            .wires
            .iter()
            .copied()
            .filter(|w| *w != route.source && !sinks.contains(w))
            .collect()
    }

    /// Compute total skew (cell_delay + wire_delay) across branches.
    ///
    /// If `group.branch_cell_delays_ps` is populated (parallel to `group.dest_cells`),
    /// each branch's total delay = cell_delay + wire_delay. The skew is max - min
    /// across total delays. Falls back to wire-only skew when cell delays are absent.
    fn total_branch_skew(route: &Route, group: &MatchedDelayGroup) -> f64 {
        if route.branches.len() < 2 {
            return 0.0;
        }

        let has_cell_delays = group.branch_cell_delays_ps.len() == group.dest_cells.len()
            && !group.branch_cell_delays_ps.is_empty();

        if !has_cell_delays {
            // Fallback: wire-only skew
            return route.branch_skew_ps() as f64;
        }

        // Map dest_cell → cell_delay for O(1) lookup
        let cell_delay_map: HashMap<CellId, f64> = group
            .dest_cells
            .iter()
            .zip(group.branch_cell_delays_ps.iter())
            .map(|(&cell, &delay)| (cell, delay))
            .collect();

        let total_delays: Vec<f64> = route
            .branches
            .iter()
            .map(|b| {
                let cell_delay = b
                    .dest_cell
                    .and_then(|c| cell_delay_map.get(&c).copied())
                    .unwrap_or(0.0);
                cell_delay + b.delay_ps as f64
            })
            .collect();

        let max = total_delays
            .iter()
            .cloned()
            .fold(f64::NEG_INFINITY, f64::max);
        let min = total_delays.iter().cloned().fold(f64::INFINITY, f64::min);
        (max - min).max(0.0)
    }

    /// Check if any matched-delay net has a skew violation (total: cell + wire)
    /// Retained for diagnostics; not used in routing loop (ready-signal-delay handles skew).
    #[allow(dead_code)]
    fn has_skew_violations(
        &self,
        result: &RoutingResult,
        matched_delay_lookup: &HashMap<GateNetId, &MatchedDelayGroup>,
    ) -> bool {
        matched_delay_lookup.iter().any(|(&net_id, group)| {
            result
                .routes
                .get(&net_id)
                .map(|r| Self::total_branch_skew(r, group) > group.max_skew_ps)
                .unwrap_or(false)
        })
    }

    /// Route a single net (without congestion costs)
    #[allow(dead_code)]
    fn route_net(
        &self,
        net_id: GateNetId,
        netlist: &GateNetlist,
        placement: &PlacementResult,
        _congestion: &CongestionTracker,
        astar: &AStarRouter<D>,
    ) -> Result<Route> {
        let net = &netlist.nets[net_id.0 as usize];
        let mut route = Route::new(net_id);

        // Get source location - skip if driver not found
        let driver_id = match net.driver {
            Some(id) => id,
            None => return Ok(route), // No driver, return empty route
        };

        // Skip nets where driver isn't placed (may be I/O or removed cell)
        let source_loc = match placement.get(driver_id) {
            Some(loc) => loc,
            None => return Ok(route), // Driver not placed, skip this net
        };

        // Get source wire - select based on BEL index to avoid wire conflicts
        let source_wires = self.device.tile_wires(source_loc.tile_x, source_loc.tile_y);
        let source_wire = self
            .select_bel_wire(&source_wires, source_loc.bel_index)
            .ok_or_else(|| {
                PlaceRouteError::RoutingFailed(format!(
                    "No wires in tile ({}, {})",
                    source_loc.tile_x, source_loc.tile_y
                ))
            })?;

        route.source = source_wire;

        // Route to each sink
        for (sink_id, _pin) in &net.fanout {
            let sink_loc = match placement.get(*sink_id) {
                Some(loc) => loc,
                None => continue,
            };

            let sink_wires = self.device.tile_wires(sink_loc.tile_x, sink_loc.tile_y);
            let sink_wire = match self.select_bel_wire(&sink_wires, sink_loc.bel_index) {
                Some(w) => w,
                None => continue,
            };

            route.sinks.push(sink_wire);

            // Find path using A*
            match astar.find_path(source_wire, sink_wire, &[]) {
                Ok((wires, pips)) => {
                    // Add unique wires
                    for wire in wires {
                        if !route.wires.contains(&wire) {
                            route.wires.push(wire);
                        }
                    }
                    route.pips.extend(pips);

                    // Calculate delay
                    for &pip_id in &route.pips {
                        if let Some(pip) = self.device.pip(pip_id) {
                            route.delay += pip.delay;
                        }
                    }
                }
                Err(_) => {
                    // Failed to route this sink - continue trying others
                }
            }
        }

        Ok(route)
    }

    /// Route a net with congestion costs and optional timing awareness
    #[allow(dead_code)]
    fn route_net_with_costs(
        &self,
        net_id: GateNetId,
        netlist: &GateNetlist,
        placement: &PlacementResult,
        congestion: &CongestionTracker,
        astar: &AStarRouter<D>,
        net_criticality: f64,
    ) -> Result<Route> {
        self.route_net_with_costs_internal(
            net_id,
            netlist,
            placement,
            NetView {
                tracker: congestion,
                own: None,
            },
            astar,
            net_criticality,
            self.present_factor,
            self.history_factor,
            false,
            SEARCH_MARGIN,
            None,
        )
    }

    /// Route a net with congestion costs, optional timing awareness, and optional
    /// per-branch delay tracking for matched-delay (NCL fork) nets.
    ///
    /// When `is_matched` is true, populates `route.branches` with a `BranchInfo`
    /// per sink, tracking per-branch wires, PIPs, and delay for skew analysis.
    #[allow(clippy::too_many_arguments)]
    fn route_net_with_costs_internal(
        &self,
        net_id: GateNetId,
        netlist: &GateNetlist,
        placement: &PlacementResult,
        congestion: NetView<'_>,
        astar: &AStarRouter<D>,
        net_criticality: f64,
        present_factor: f64,
        history_factor: f64,
        is_matched: bool,
        margin: u32,
        previous: Option<&Route>,
    ) -> Result<Route> {
        let net = &netlist.nets[net_id.0 as usize];
        let mut route = Route::new(net_id);

        // Get source location - skip if driver not found
        let driver_id = match net.driver {
            Some(id) => id,
            None => return Ok(route), // No driver, return empty route
        };

        // Skip nets where driver isn't placed (may be I/O or removed cell)
        let source_loc = match placement.get(driver_id) {
            Some(loc) => loc,
            None => return Ok(route), // Driver not placed, skip this net
        };

        // Get source wire using proper BEL pin lookup
        let source_wire = self
            .get_source_wire(driver_id, source_loc, net_id, netlist)
            .ok_or_else(|| {
                PlaceRouteError::RoutingFailed(format!(
                    "No source wire for cell at ({}, {})",
                    source_loc.tile_x, source_loc.tile_y
                ))
            })?;

        route.source = source_wire;

        // Build wire cost map from congestion, with timing awareness for critical nets
        let timing_factor = net_criticality * self.timing_weight;
        // Critical nets weigh congestion less, but never ignore it, or a
        // heavily weighted net could hold a shared wire forever
        let relief = timing_factor.min(1.0);
        let adjusted_present_factor = present_factor * (1.0 - relief * 0.5);
        let adjusted_history_factor = history_factor * (1.0 - relief * 0.3);

        // Cost multiplier for entering a wire: 1.0 for a wire no net has
        // used, the PathFinder congestion cost otherwise. Computed on demand
        // for the wires the search reaches; building it as a map of every
        // used wire for every net made routing quadratic in design size. A
        // critical net's delay is charged by the search itself, through each
        // PIP's timing term.
        let graph = astar.graph();
        let wire_cost = |wire: WireId| -> f64 {
            congestion.cost_factor(wire, adjusted_present_factor, adjusted_history_factor)
        };
        // Wires already in the route (route.wires keeps first-use order)
        let mut route_wire_set: HashSet<WireId> = HashSet::new();
        // Delay from the source to each wire of the route; route.delay is
        // the largest over the sinks
        let mut arrival: HashMap<WireId, u32> = [(source_wire, 0)].into_iter().collect();

        let timing_weight = net_criticality * self.timing_weight;
        let bounds = self.search_bounds(net, placement, margin);

        // Each sink's pin wire (None: placed, but no pin wire)
        let mut sinks: Vec<(CellId, &PlacementLoc, Option<WireId>)> = net
            .fanout
            .iter()
            .filter_map(|&(sink_id, pin_idx)| {
                let loc = placement.get(sink_id)?;
                Some((
                    sink_id,
                    loc,
                    self.get_sink_wire(sink_id, loc, pin_idx, netlist),
                ))
            })
            .collect();
        // Nearest sinks first, so the route grows outward from the source and
        // each later sink finds route wires close by
        sinks.sort_by_key(|(_, loc, _)| {
            loc.tile_x.abs_diff(source_loc.tile_x) + loc.tile_y.abs_diff(source_loc.tile_y)
        });
        let mut tree_index = TreeIndex::new();

        // Rerouting a net: keep the part of its previous route that shares
        // no wire with another net, and route only the sinks it no longer
        // reaches. A net with hundreds of sinks and one congested branch
        // then costs one branch, not a whole new tree.
        let mut kept_sinks: HashMap<WireId, usize> = HashMap::new();
        if let Some(previous) = previous.filter(|p| !is_matched && p.source == source_wire) {
            let expected: Vec<WireId> = sinks.iter().filter_map(|s| s.2).collect();
            if let Some((wires, pips, reached)) = Self::prune_route(
                previous,
                &expected,
                |w| congestion.usage(w) > 0,
                |p| self.device.pip(p).map(|pip| (pip.src_wire, pip.dst_wire)),
            ) {
                for &w in &wires {
                    route_wire_set.insert(w);
                    tree_index.insert(graph.tile(w), w);
                }
                route.wires = wires;
                // In tree order, so each PIP's source already has its time
                for &pip in &pips {
                    if let Some(p) = self.device.pip(pip) {
                        let t = arrival.get(&p.src_wire).copied().unwrap_or(0) + p.delay;
                        arrival.entry(p.dst_wire).or_insert(t);
                    }
                }
                route.pips = pips;
                for w in reached {
                    route.delay = route.delay.max(arrival.get(&w).copied().unwrap_or(0));
                    *kept_sinks.entry(w).or_default() += 1;
                }
            }
        }

        for (sink_id, sink_loc, sink_wire) in sinks {
            let sink_id = &sink_id;
            let sink_wire = match sink_wire {
                Some(w) => w,
                None => {
                    log::debug!(
                        "pathfinder: net {} sink {:?} pin at {:?} has no pin wire",
                        net.name,
                        sink_id,
                        sink_loc
                    );
                    route.unrouted_sinks += 1;
                    continue;
                }
            };

            route.sinks.push(sink_wire);
            if let Some(n) = kept_sinks.get_mut(&sink_wire).filter(|n| **n > 0) {
                *n -= 1; // still reached by the kept part of the route
                continue;
            }

            // Find path using A* with timing-aware costs. A sink branches off
            // the net's route so far; a matched-delay (NCL fork) net keeps a
            // separate path per sink so each branch's delay is its own.
            // The search stays inside the net's bounding box plus `margin`
            // tiles, since an unbounded search for a congested net floods
            // the device; only if no path fits there does it go further.
            let tree = if is_matched || route.wires.is_empty() {
                std::slice::from_ref(&source_wire)
            } else {
                &route.wires[..]
            };
            // A large route: start from its wires near the sink, and from
            // the whole route only if no path leaves those
            let local = (tree.len() > LOCAL_SEED_MIN_TREE)
                .then(|| tree_index.seeds_near(sink_loc.tile_x, sink_loc.tile_y))
                .flatten();
            // A critical sink pays for the delay already spent reaching the
            // tree wire it branches from (edge timing terms are in 50 ps)
            let start_cost =
                |w: WireId| timing_weight * arrival.get(&w).copied().unwrap_or(0) as f64 / 50.0;
            let search = |seeds: &[WireId]| {
                astar
                    .find_path_bounded(
                        seeds,
                        sink_wire,
                        &[],
                        wire_cost,
                        timing_weight,
                        start_cost,
                        bounds,
                    )
                    .or_else(|e| match bounds {
                        Some(_) => astar.find_path_bounded(
                            seeds,
                            sink_wire,
                            &[],
                            wire_cost,
                            timing_weight,
                            start_cost,
                            None,
                        ),
                        None => Err(e),
                    })
            };
            let path_result = match &local {
                Some(seeds) => search(seeds).or_else(|_| search(tree)),
                None => search(tree),
            }
            .map(|(wires, pips, _)| (wires, pips));

            // A path searched from part of the tree (or from the source) can
            // run through other wires of the tree; it joins the tree at the
            // last one, so no wire gets a second driver
            let path_result = path_result.map(|(wires, pips)| {
                if is_matched {
                    (wires, pips)
                } else {
                    Self::graft(wires, pips, &route_wire_set)
                }
            });
            match path_result {
                Ok((wires, pips)) => {
                    // This branch's delay from the source: the arrival time at
                    // the tree wire it starts from plus its own PIPs
                    let branch_delay = self.record_arrivals(&mut arrival, &wires, &pips);

                    // Add wires/PIPs to the route-level aggregates
                    for wire in &wires {
                        if route_wire_set.insert(*wire) {
                            route.wires.push(*wire);
                            tree_index.insert(graph.tile(*wire), *wire);
                        }
                    }
                    route.pips.extend(&pips);
                    route.delay = route.delay.max(branch_delay);

                    // Record per-branch info for matched-delay nets
                    if is_matched {
                        route.branches.push(BranchInfo {
                            sink: sink_wire,
                            dest_cell: Some(*sink_id),
                            wires,
                            pips,
                            delay_ps: branch_delay,
                        });
                    }
                }
                Err(_) => {
                    // Primary routing failed - try LUT input permutation for LUT4 sinks
                    let mut reached = false;
                    if sink_loc.bel_type == BelType::Lut4 {
                        if let Some((alt_wire, alt_wires, alt_pips)) = self
                            .try_alternate_lut_inputs(
                                *sink_id,
                                sink_loc,
                                source_wire,
                                &wire_cost,
                                timing_weight,
                                astar,
                                netlist,
                            )
                        {
                            let (alt_wires, alt_pips) = if is_matched {
                                (alt_wires, alt_pips)
                            } else {
                                Self::graft(alt_wires, alt_pips, &route_wire_set)
                            };
                            let branch_delay =
                                self.record_arrivals(&mut arrival, &alt_wires, &alt_pips);

                            if route.sinks.last() == Some(&sink_wire) {
                                route.sinks.pop();
                            }
                            route.sinks.push(alt_wire);

                            for wire in &alt_wires {
                                if route_wire_set.insert(*wire) {
                                    route.wires.push(*wire);
                                    tree_index.insert(graph.tile(*wire), *wire);
                                }
                            }
                            route.pips.extend(&alt_pips);
                            route.delay = route.delay.max(branch_delay);
                            reached = true;

                            if is_matched {
                                route.branches.push(BranchInfo {
                                    sink: alt_wire,
                                    dest_cell: Some(*sink_id),
                                    wires: alt_wires,
                                    pips: alt_pips,
                                    delay_ps: branch_delay,
                                });
                            }
                        }
                    }
                    if !reached {
                        let name = |w: WireId| self.device.wire(w).map(|wire| wire.name.clone());
                        log::debug!(
                            "pathfinder: net {} has no path from {:?} to {:?}",
                            net.name,
                            name(source_wire),
                            name(sink_wire)
                        );
                        route.unrouted_sinks += 1;
                    }
                }
            }
        }

        Ok(route)
    }

    /// The part of a net's previous route worth keeping when it is rerouted:
    /// its tree from the source with every wire `shared(w)` (used by
    /// another net) removed, along with everything downstream of such a
    /// wire, and then trimmed to the branches that still reach a sink.
    /// Returns the kept wires, PIPs (in tree order, parents first) and the
    /// sinks still reached; `None` if the route is not a plain tree over
    /// `expected` sinks (a matched-delay or alternate-pin route), which is
    /// then rerouted whole.
    fn prune_route(
        route: &Route,
        expected: &[WireId],
        shared: impl Fn(WireId) -> bool,
        pip_ends: impl Fn(PipId) -> Option<(WireId, WireId)>,
    ) -> Option<(Vec<WireId>, Vec<PipId>, Vec<WireId>)> {
        let mut a = route.sinks.clone();
        let mut b = expected.to_vec();
        a.sort_by_key(|w| w.0);
        b.sort_by_key(|w| w.0);
        if a != b || route.wires.is_empty() {
            return None;
        }
        // Children of each wire; every wire but the source has one parent
        let mut children: HashMap<WireId, Vec<(PipId, WireId)>> = HashMap::new();
        let mut has_parent: HashSet<WireId> = HashSet::new();
        for &pip in &route.pips {
            let (src, dst) = pip_ends(pip)?;
            if !has_parent.insert(dst) || dst == route.source {
                return None;
            }
            children.entry(src).or_default().push((pip, dst));
        }
        // Breadth-first from the source, not entering shared wires
        let sink_set: HashSet<WireId> = route.sinks.iter().copied().collect();
        let mut order: Vec<(Option<PipId>, WireId)> = vec![(None, route.source)];
        let mut i = 0;
        while i < order.len() {
            let w = order[i].1;
            i += 1;
            for &(pip, child) in children.get(&w).map(Vec::as_slice).unwrap_or(&[]) {
                if !sink_set.contains(&child) && shared(child) {
                    continue;
                }
                order.push((Some(pip), child));
            }
        }
        // Keep a wire if a reached sink lies at or below it (children come
        // after their parent in `order`, so walk it backwards)
        let index: HashMap<WireId, usize> = order
            .iter()
            .enumerate()
            .map(|(i, &(_, w))| (w, i))
            .collect();
        let mut keep = vec![false; order.len()];
        keep[0] = true;
        for j in (1..order.len()).rev() {
            let w = order[j].1;
            if sink_set.contains(&w) {
                keep[j] = true;
            }
            if keep[j] {
                let parent = order[j]
                    .0
                    .and_then(&pip_ends)
                    .and_then(|(src, _)| index.get(&src).copied());
                if let Some(pj) = parent {
                    keep[pj] = true;
                }
            }
        }
        let mut wires = Vec::new();
        let mut pips = Vec::new();
        let mut reached = Vec::new();
        for (j, &(pip, w)) in order.iter().enumerate() {
            if !keep[j] {
                continue;
            }
            wires.push(w);
            if let Some(p) = pip {
                pips.push(p);
            }
            if sink_set.contains(&w) {
                reached.push(w);
            }
        }
        // A sink wire listed twice (two pins on one wire) is reached for both
        let reached: Vec<WireId> = route
            .sinks
            .iter()
            .copied()
            .filter(|s| reached.contains(s))
            .collect();
        Some((wires, pips, reached))
    }

    /// The part of a path (`wires`, joined by `pips`: `pips[i]` drives
    /// `wires[i + 1]`) from the last of its wires already in a net's route
    /// tree (`tree`) on. A path that started on the tree and ran through
    /// other tree wires would give those a second driver; from its last tree
    /// wire on it only adds new wires. A path that touches no tree wire is
    /// returned whole.
    fn graft(
        wires: Vec<WireId>,
        pips: Vec<crate::device::PipId>,
        tree: &HashSet<WireId>,
    ) -> (Vec<WireId>, Vec<crate::device::PipId>) {
        match wires.iter().rposition(|w| tree.contains(w)) {
            Some(last) if last > 0 && pips.len() + 1 == wires.len() => {
                (wires[last..].to_vec(), pips[last..].to_vec())
            }
            _ => (wires, pips),
        }
    }

    /// Record the arrival time (delay from the net's source) of each wire of
    /// a branch `wires` joined by `pips`, which starts on a wire already in
    /// `arrival`. Returns the arrival time at its last wire.
    fn record_arrivals(
        &self,
        arrival: &mut HashMap<WireId, u32>,
        wires: &[WireId],
        pips: &[crate::device::PipId],
    ) -> u32 {
        let mut t = wires
            .first()
            .and_then(|w| arrival.get(w))
            .copied()
            .unwrap_or(0);
        for (wire, pip) in wires.iter().skip(1).zip(pips) {
            t += self.device.pip(*pip).map(|p| p.delay).unwrap_or(0);
            arrival.entry(*wire).or_insert(t);
        }
        t
    }

    /// Enforce MAXSKEW constraint on a matched-delay net by ripping up fast
    /// branches and rerouting them through longer wire paths.
    ///
    /// Uses **total delay** (cell_delay + wire_delay) per branch to identify the
    /// fastest branch. This handles the common case where cell-path asymmetry
    /// (e.g. one branch through a comparator chain, another direct to a TH gate)
    /// causes skew that the router must compensate with wire-delay imbalance.
    ///
    /// Algorithm:
    /// 1. Compute total skew = max(cell+wire) - min(cell+wire) across branches
    /// 2. If skew <= max_skew_ps, return route unchanged
    /// 3. Identify branch with lowest total delay (fastest arrival)
    /// 4. Rip up that branch's wires/PIPs
    /// 5. Block its wires so A* finds a longer alternate path
    /// 6. Repeat up to `max_attempts`
    ///
    /// Retained for potential future use; not called in current ready-signal-delay flow.
    #[allow(clippy::too_many_arguments)]
    #[allow(dead_code)]
    fn enforce_skew_constraint(
        &self,
        mut route: Route,
        group: &MatchedDelayGroup,
        _netlist: &GateNetlist,
        _placement: &PlacementResult,
        _congestion: &CongestionTracker,
        astar: &AStarRouter<D>,
        _net_criticality: f64,
        _present_factor: f64,
        _history_factor: f64,
    ) -> Result<Route> {
        let max_attempts = 5;

        // Build cell delay lookup from constraint group
        let cell_delay_map: HashMap<CellId, f64> = group
            .dest_cells
            .iter()
            .zip(group.branch_cell_delays_ps.iter())
            .map(|(&cell, &delay)| (cell, delay))
            .collect();

        let has_cell_delays = !cell_delay_map.is_empty();

        for _attempt in 0..max_attempts {
            if route.branches.len() < 2 {
                return Ok(route);
            }

            // Compute total skew (cell + wire)
            let total_skew = Self::total_branch_skew(&route, group);
            if total_skew <= group.max_skew_ps {
                return Ok(route);
            }

            // Find branch with minimum total delay (fastest arrival — needs more wire)
            let fast_idx = if has_cell_delays {
                route
                    .branches
                    .iter()
                    .enumerate()
                    .min_by(|(_, a), (_, b)| {
                        let a_total = a
                            .dest_cell
                            .and_then(|c| cell_delay_map.get(&c).copied())
                            .unwrap_or(0.0)
                            + a.delay_ps as f64;
                        let b_total = b
                            .dest_cell
                            .and_then(|c| cell_delay_map.get(&c).copied())
                            .unwrap_or(0.0)
                            + b.delay_ps as f64;
                        a_total
                            .partial_cmp(&b_total)
                            .unwrap_or(std::cmp::Ordering::Equal)
                    })
                    .map(|(i, _)| i)
                    .unwrap()
            } else {
                // Fallback: wire-only (minimum wire delay)
                route
                    .branches
                    .iter()
                    .enumerate()
                    .min_by_key(|(_, b)| b.delay_ps)
                    .map(|(i, _)| i)
                    .unwrap()
            };

            // Remove the fast branch from the route
            let fast_branch = route.branches.remove(fast_idx);

            // Remove fast branch's PIPs from route-level aggregates
            for pip in &fast_branch.pips {
                if let Some(pos) = route.pips.iter().position(|p| p == pip) {
                    route.pips.remove(pos);
                }
            }
            // Remove fast branch's wires that aren't used by other branches or source
            let other_wires: HashSet<WireId> = route
                .branches
                .iter()
                .flat_map(|b| b.wires.iter().copied())
                .chain(std::iter::once(route.source))
                .collect();
            route.wires.retain(|w| other_wires.contains(w));
            route.delay = route.delay.saturating_sub(fast_branch.delay_ps);

            // Remove the fast branch's sink from route.sinks
            if let Some(pos) = route.sinks.iter().position(|s| *s == fast_branch.sink) {
                route.sinks.remove(pos);
            }

            // Block the fast branch's wires to force A* to find an alternate (longer) path
            let blocked: Vec<WireId> = fast_branch
                .wires
                .iter()
                .copied()
                .filter(|w| *w != route.source && *w != fast_branch.sink)
                .collect();

            // Reroute the fast branch with blocked wires
            let wire_costs: HashMap<WireId, f64> = HashMap::new();
            match astar.find_path_timing_aware(
                route.source,
                fast_branch.sink,
                &blocked,
                &wire_costs,
                0.0, // No timing weight — we want A* to find *any* alternate path
            ) {
                Ok((new_wires, new_pips)) => {
                    let new_delay: u32 = new_pips
                        .iter()
                        .map(|&pip_id| self.device.pip(pip_id).map(|p| p.delay).unwrap_or(0))
                        .sum();

                    // Add the rerouted branch back
                    route.sinks.push(fast_branch.sink);
                    for wire in &new_wires {
                        if !route.wires.contains(wire) {
                            route.wires.push(*wire);
                        }
                    }
                    route.pips.extend(&new_pips);
                    route.delay += new_delay;

                    route.branches.push(BranchInfo {
                        sink: fast_branch.sink,
                        dest_cell: fast_branch.dest_cell,
                        wires: new_wires,
                        pips: new_pips,
                        delay_ps: new_delay,
                    });
                }
                Err(_) => {
                    // Reroute failed — put the original branch back and stop trying
                    route.sinks.push(fast_branch.sink);
                    for wire in &fast_branch.wires {
                        if !route.wires.contains(wire) {
                            route.wires.push(*wire);
                        }
                    }
                    route.pips.extend(&fast_branch.pips);
                    route.delay += fast_branch.delay_ps;
                    route.branches.push(fast_branch);
                    break;
                }
            }
        }

        Ok(route)
    }

    /// Get the output wire for a cell (source of a net)
    fn get_source_wire(
        &self,
        cell_id: CellId,
        loc: &PlacementLoc,
        net_id: GateNetId,
        netlist: &GateNetlist,
    ) -> Option<WireId> {
        // Get cell type to determine which wire to use
        let cell = crate::utils::find_cell(netlist, cell_id)?;

        // The device's own pin model, when it has one
        let output = netlist.nets[net_id.0 as usize]
            .driver_pin
            .or_else(|| cell.outputs.iter().position(|&n| n == net_id))
            .unwrap_or(0);
        if let Some(wire) = self.device.bel_pin_wire(
            loc.tile_x,
            loc.tile_y,
            loc.bel_index,
            CellPin::Output(output),
        ) {
            return Some(wire);
        }

        // Convert BEL index to LC index
        let lc_idx = if loc.bel_index >= 16 {
            7 // Carry uses LC7
        } else {
            loc.bel_index / 2
        };

        match loc.bel_type {
            BelType::Lut4 => {
                // LUT output wire
                self.device
                    .lut_output_wire(loc.tile_x, loc.tile_y, lc_idx)
                    .or_else(|| self.fallback_source_wire(loc, lc_idx))
            }
            BelType::Dff | BelType::DffE | BelType::DffSr | BelType::DffSrE => {
                // DFF output is same as LUT output (shared in iCE40)
                self.device
                    .lut_output_wire(loc.tile_x, loc.tile_y, lc_idx)
                    .or_else(|| self.fallback_source_wire(loc, lc_idx))
            }
            BelType::IoCell => {
                // I/O input wire (data coming from pad into fabric)
                let iob_idx = loc.bel_index % 2;
                // Check if this is an input cell
                if cell.cell_type.contains("INPUT") || cell.inputs.is_empty() {
                    self.device
                        .io_input_wire(loc.tile_x, loc.tile_y, iob_idx)
                        .or_else(|| self.fallback_source_wire(loc, iob_idx))
                } else {
                    // Output cell - shouldn't be a source
                    self.fallback_source_wire(loc, iob_idx)
                }
            }
            _ => self.fallback_source_wire(loc, lc_idx),
        }
    }

    /// Get the input wire for a cell (sink of a net)
    fn get_sink_wire(
        &self,
        cell_id: CellId,
        loc: &PlacementLoc,
        pin_idx: usize,
        netlist: &GateNetlist,
    ) -> Option<WireId> {
        // The device's own pin model, when it has one
        if let Some(wire) = self.device.bel_pin_wire(
            loc.tile_x,
            loc.tile_y,
            loc.bel_index,
            CellPin::Input(pin_idx),
        ) {
            return Some(wire);
        }

        // Convert BEL index to LC index
        let lc_idx = if loc.bel_index >= 16 {
            7 // Carry uses LC7
        } else {
            loc.bel_index / 2
        };

        // Get cell type to determine which wire to use
        let cell = crate::utils::find_cell(netlist, cell_id)?;

        match loc.bel_type {
            BelType::Lut4 => {
                // Use the pin index directly as LUT input index (0-3)
                let input_idx = pin_idx.min(3);
                self.device
                    .lut_input_wire(loc.tile_x, loc.tile_y, lc_idx, input_idx)
                    .or_else(|| self.fallback_sink_wire(loc, lc_idx))
            }
            BelType::Dff | BelType::DffE | BelType::DffSr | BelType::DffSrE => {
                // DFF data input - uses same wire as LUT input 0 in iCE40
                // (The DFF input comes from the LUT output in the same LC,
                // but when driven externally it uses input 0)
                self.device
                    .lut_input_wire(loc.tile_x, loc.tile_y, lc_idx, 0)
                    .or_else(|| self.fallback_sink_wire(loc, lc_idx))
            }
            BelType::IoCell => {
                // I/O output wire (data going to pad from fabric)
                let iob_idx = loc.bel_index % 2;
                if cell.cell_type.contains("OUTPUT") || cell.outputs.is_empty() {
                    self.device
                        .io_output_wire(loc.tile_x, loc.tile_y, iob_idx)
                        .or_else(|| self.fallback_sink_wire(loc, iob_idx))
                } else {
                    self.fallback_sink_wire(loc, iob_idx)
                }
            }
            _ => self.fallback_sink_wire(loc, lc_idx),
        }
    }

    /// Fallback to using tile wires when BEL pin wires aren't available
    fn fallback_source_wire(&self, loc: &PlacementLoc, idx: usize) -> Option<WireId> {
        let wires = self.device.tile_wires(loc.tile_x, loc.tile_y);
        self.select_bel_wire(&wires, idx)
    }

    /// Fallback to using tile wires when BEL pin wires aren't available
    fn fallback_sink_wire(&self, loc: &PlacementLoc, idx: usize) -> Option<WireId> {
        let wires = self.device.tile_wires(loc.tile_x, loc.tile_y);
        self.select_bel_wire(&wires, idx)
    }

    /// Select a wire for a specific BEL index
    /// In iCE40, each LC has associated local wires (local_g0 for LC0, local_g1 for LC1, etc.)
    /// BEL indices: LUT0=0, DFF0=1, LUT1=2, DFF1=3, ..., LUT7=14, DFF7=15, Carry=16
    fn select_bel_wire(&self, wires: &[WireId], bel_index: usize) -> Option<WireId> {
        if wires.is_empty() {
            return None;
        }

        // Convert BEL index to LC index: LUT/DFF pairs share an LC
        // LUT at even index, DFF at odd index -> both map to LC = bel_index / 2
        // Carry (bel_index 16) maps to LC 7 (the last one)
        let lc_idx = if bel_index >= 16 {
            7 // Carry uses LC7
        } else {
            bel_index / 2
        };

        // Each LC has its own local wire (local_g0 through local_g7)
        // If we have enough wires, use the LC-specific one
        if lc_idx < wires.len() {
            Some(wires[lc_idx])
        } else {
            // Fall back to first wire if not enough wires in tile
            wires.first().copied()
        }
    }

    /// Try alternate LUT input pins when primary routing fails
    /// Returns (sink_wire, wires, pips) if an alternate route is found
    #[allow(clippy::too_many_arguments)]
    fn try_alternate_lut_inputs(
        &self,
        _sink_id: CellId,
        sink_loc: &PlacementLoc,
        source_wire: WireId,
        wire_cost: &dyn Fn(WireId) -> f64,
        timing_weight: f64,
        astar: &AStarRouter<D>,
        _netlist: &GateNetlist,
    ) -> Option<(WireId, Vec<WireId>, Vec<crate::device::PipId>)> {
        // Convert BEL index to LC index
        let lc_idx = if sink_loc.bel_index >= 16 {
            7
        } else {
            sink_loc.bel_index / 2
        };

        // Try all 4 LUT input pins (0-3)
        // We already tried the original pin, so try the others
        for alt_input_idx in 0..4 {
            let alt_wire = match self
                .device
                .bel_pin_wire(
                    sink_loc.tile_x,
                    sink_loc.tile_y,
                    sink_loc.bel_index,
                    CellPin::Input(alt_input_idx),
                )
                .or_else(|| {
                    self.device.lut_input_wire(
                        sink_loc.tile_x,
                        sink_loc.tile_y,
                        lc_idx,
                        alt_input_idx,
                    )
                }) {
                Some(w) => w,
                None => continue,
            };

            // Try routing to this alternate input
            if let Ok((wires, pips)) =
                astar.find_path_with_cost_fn(source_wire, alt_wire, &[], wire_cost, timing_weight)
            {
                // Found a route to an alternate LUT input
                // Note: The caller should update the LUT permutation accordingly
                return Some((alt_wire, wires, pips));
            }
        }

        None
    }

    /// The region a net's route searches stay in: the bounding box of its
    /// driver and sinks widened by `margin` tiles, or `None` (the whole
    /// device) once that covers the device.
    fn search_bounds(
        &self,
        net: &skalp_lir::gate_netlist::GateNet,
        placement: &PlacementResult,
        margin: u32,
    ) -> Option<SearchBounds> {
        let mut tiles = net
            .driver
            .iter()
            .chain(net.fanout.iter().map(|(c, _)| c))
            .filter_map(|&c| placement.get(c))
            .map(|loc| (loc.tile_x, loc.tile_y));
        let (x, y) = tiles.next()?;
        let (mut min_x, mut min_y, mut max_x, mut max_y) = (x, y, x, y);
        for (x, y) in tiles {
            min_x = min_x.min(x);
            min_y = min_y.min(y);
            max_x = max_x.max(x);
            max_y = max_y.max(y);
        }
        let (width, height) = self.device.grid_size();
        let b = SearchBounds {
            min_x: min_x.saturating_sub(margin),
            min_y: min_y.saturating_sub(margin),
            max_x: max_x.saturating_add(margin),
            max_y: max_y.saturating_add(margin),
        };
        let covers_device =
            b.min_x == 0 && b.min_y == 0 && b.max_x + 1 >= width && b.max_y + 1 >= height;
        (!covers_device).then_some(b)
    }

    /// Calculate bounding box size (half-perimeter wirelength) for a net
    fn calculate_net_bbox(
        &self,
        net: &skalp_lir::gate_netlist::GateNet,
        placement: &PlacementResult,
    ) -> u32 {
        let mut min_x = u32::MAX;
        let mut max_x = 0u32;
        let mut min_y = u32::MAX;
        let mut max_y = 0u32;

        // Driver location
        if let Some(driver_id) = net.driver {
            if let Some(loc) = placement.get(driver_id) {
                min_x = min_x.min(loc.tile_x);
                max_x = max_x.max(loc.tile_x);
                min_y = min_y.min(loc.tile_y);
                max_y = max_y.max(loc.tile_y);
            }
        }

        // Sink locations
        for (sink_id, _) in &net.fanout {
            if let Some(loc) = placement.get(*sink_id) {
                min_x = min_x.min(loc.tile_x);
                max_x = max_x.max(loc.tile_x);
                min_y = min_y.min(loc.tile_y);
                max_y = max_y.max(loc.tile_y);
            }
        }

        // Half-perimeter wirelength (HPWL)
        if min_x <= max_x && min_y <= max_y {
            (max_x - min_x) + (max_y - min_y)
        } else {
            0
        }
    }

    /// Calculate net criticalities for timing-driven routing.
    ///
    /// When timing_weight > 0, uses pre-routing STA with estimated wire delays
    /// to compute real slack-based criticality. Falls back to heuristic
    /// (bbox + fanout) when timing_weight == 0.
    fn calculate_net_criticalities(
        &self,
        netlist: &GateNetlist,
        placement: &PlacementResult,
    ) -> Vec<NetCriticality> {
        let mut criticalities = Vec::new();

        // Timing-driven: the same slack estimate the placer uses, on the
        // placed cells with this device family's delays
        if self.timing_weight > 0.0 {
            let model = crate::timing::DelayModel::for_family(self.device.family());
            let critical =
                crate::placer::timing_estimate::net_criticalities(netlist, &model, |id| {
                    placement
                        .get(id)
                        .map(|loc| (loc.tile_x as f64, loc.tile_y as f64))
                });
            for net in &netlist.nets {
                if net.driver.is_none() || net.fanout.is_empty() {
                    continue;
                }
                let criticality = critical.get(net.id.0 as usize).copied().unwrap_or(0.0);
                criticalities.push(NetCriticality {
                    net_id: net.id,
                    criticality,
                    slack: 1.0 - criticality,
                    bbox: self.calculate_net_bbox(net, placement),
                });
            }
            return criticalities;
        }

        // Fallback: heuristic criticality (non-timing-driven mode)
        let mut max_bbox = 1u32;
        let mut max_fanout = 1usize;

        for net in &netlist.nets {
            if net.driver.is_none() || net.fanout.is_empty() {
                continue;
            }

            let bbox = self.calculate_net_bbox(net, placement);
            max_bbox = max_bbox.max(bbox);
            max_fanout = max_fanout.max(net.fanout.len());
        }

        for net in &netlist.nets {
            if net.driver.is_none() || net.fanout.is_empty() {
                continue;
            }

            let bbox = self.calculate_net_bbox(net, placement);
            let fanout = net.fanout.len();
            let is_timing_critical = self.is_timing_critical_net(net, netlist);

            let bbox_factor = bbox as f64 / max_bbox as f64;
            let fanout_factor = fanout as f64 / max_fanout as f64;
            let timing_bonus = if is_timing_critical { 0.3 } else { 0.0 };

            let criticality = (0.4 * bbox_factor + 0.3 * fanout_factor + timing_bonus).min(1.0);

            let estimated_delay_ns = bbox as f64 * 0.5 + fanout as f64 * 0.1;
            let target_period_ns = 10.0;
            let slack = target_period_ns - estimated_delay_ns;

            criticalities.push(NetCriticality {
                net_id: net.id,
                criticality,
                slack,
                bbox,
            });
        }

        criticalities
    }

    /// Check if a net is timing-critical (connected to registers)
    fn is_timing_critical_net(
        &self,
        net: &skalp_lir::gate_netlist::GateNet,
        netlist: &GateNetlist,
    ) -> bool {
        // Check if driver is a register
        if let Some(driver_id) = net.driver {
            if let Some(cell) = crate::utils::find_cell(netlist, driver_id) {
                if cell.clock.is_some() {
                    return true;
                }
            }
        }

        // Check if any sink is a register
        for (sink_id, _) in &net.fanout {
            if let Some(cell) = crate::utils::find_cell(netlist, *sink_id) {
                if cell.clock.is_some() {
                    return true;
                }
            }
        }

        false
    }
}

/// One net to (re)route in a routing pass
#[derive(Debug, Clone, Copy)]
struct NetJob {
    net_id: GateNetId,
    /// Search margin beyond the net's bounding box
    margin: u32,
    /// Keep the uncongested part of the net's current route
    keep: bool,
}

/// Which routes of a batch, in order, to record. Each was found against
/// the routes as they stood before the batch, less its own old route
/// (`old`); it is recorded if it takes no wire that a route recorded before
/// it in the batch newly took, so it is still the route it would have been
/// had it been routed after them. (The first is always recorded, so every
/// batch makes progress.)
fn settle_batch(new: &[Vec<WireId>], old: &[HashSet<WireId>]) -> Vec<bool> {
    let mut taken: HashSet<WireId> = HashSet::new();
    new.iter()
        .zip(old)
        .map(|(wires, old)| {
            let clear = wires.iter().all(|w| !taken.contains(w));
            if clear {
                taken.extend(wires.iter().copied().filter(|w| !old.contains(w)));
            }
            clear
        })
        .collect()
}

/// Whether the best of the last `window` values of `history` is less than
/// `fraction` below the best before them
fn stalled(history: &[f64], window: usize, fraction: f64) -> bool {
    if history.len() <= window {
        return false;
    }
    let (before, recent) = history.split_at(history.len() - window);
    let best_before = before.iter().copied().fold(f64::INFINITY, f64::min);
    let best_recent = recent.iter().copied().fold(f64::INFINITY, f64::min);
    best_before > 0.0 && (best_before - best_recent) / best_before < fraction
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::device::nexus::{NexusDevice, NexusVariant};
    use skalp_lir::gate_netlist::{Cell, GateNet};

    /// Two LUT-to-LUT nets in separate logic tiles of a Nexus device
    fn two_nets(device: &NexusDevice) -> (GateNetlist, PlacementResult, [GateNetId; 2]) {
        let mut netlist = GateNetlist::new("two_nets".to_string(), "nexus".to_string());
        let mut nets = Vec::new();
        for i in 0..4 {
            nets.push(netlist.add_net(GateNet::new(GateNetId(i), format!("n{i}"))));
        }
        let mut cells = Vec::new();
        // a -> b over n1, c -> d over n3 (n0 and n2 are undriven inputs)
        for (inputs, outputs) in [
            (vec![nets[0]], vec![nets[1]]),
            (vec![nets[1]], vec![]),
            (vec![nets[2]], vec![nets[3]]),
            (vec![nets[3]], vec![]),
        ] {
            let id = CellId(cells.len() as u32);
            cells.push(netlist.add_cell(Cell::new_comb(
                id,
                "OXIDE_COMB".to_string(),
                "nexus".to_string(),
                0.0,
                format!("c{}", id.0),
                inputs,
                outputs,
            )));
        }

        let (w, h) = device.grid_size();
        let logic: Vec<(u32, u32)> = (0..h)
            .flat_map(|y| (0..w).map(move |x| (x, y)))
            .filter(|&(x, y)| device.bel_pin_wire(x, y, 0, CellPin::Output(0)).is_some())
            .collect();
        let mut placement = PlacementResult::new();
        for (cell, &(x, y)) in cells.iter().zip(&logic[logic.len() / 2..]) {
            placement
                .placements
                .insert(*cell, PlacementLoc::new(x, y, 0, BelType::Lut4));
        }
        (netlist, placement, [nets[1], nets[3]])
    }

    #[test]
    fn a_batch_records_routes_that_take_no_wire_an_earlier_one_took() {
        let w = |ids: &[u32]| ids.iter().map(|&i| WireId(i)).collect::<Vec<_>>();
        let set = |ids: &[u32]| ids.iter().map(|&i| WireId(i)).collect::<HashSet<_>>();
        let new = [
            w(&[1, 2, 3]),
            w(&[3, 4]), // takes 3 from the first
            w(&[4, 5]), // 4 belongs to a route that was not recorded: clear
            w(&[5]),    // takes 5 from the third
            w(&[2, 6]), // 2 the first already held before the batch: clear
            w(&[]),
        ];
        let old = [set(&[2]), set(&[]), set(&[]), set(&[]), set(&[]), set(&[])];
        assert_eq!(
            settle_batch(&new, &old),
            [true, false, true, false, true, true]
        );
        assert_eq!(settle_batch(&[], &[]), Vec::<bool>::new());
    }

    #[test]
    fn stall_check_looks_at_the_best_not_the_latest() {
        // Falling, then wandering above an earlier low: not stalled while the
        // window holds a new best
        let history = [
            400.0, 300.0, 250.0, 182.0, 174.0, 160.0, 157.0, 156.0, 139.0, 150.0, 173.0,
        ];
        assert!(!stalled(&history, 8, 0.02));
        // Eight iterations without beating 139 by 2%: stalled
        let flat = [
            400.0, 139.0, 150.0, 141.0, 145.0, 139.0, 142.0, 160.0, 140.0, 138.0,
        ];
        assert!(stalled(&flat, 8, 0.02));
        // Too short to judge
        assert!(!stalled(&[5.0, 5.0], 8, 0.02));
    }

    /// A large route's search for a sink starts from the route wires near
    /// the sink: the smallest square holding enough of them.
    #[test]
    fn tree_seeds_come_from_the_route_near_the_sink() {
        let mut tree = TreeIndex::new();
        let mut next = 0u32;
        let mut wire = || {
            next += 1;
            WireId(next)
        };
        // A route along row 10 from column 0 to 100, 4 wires per tile
        for x in 0..=100 {
            for _ in 0..4 {
                tree.insert(Some((x, 10)), wire());
            }
        }
        // A sink beside column 50: 3 x 3 tiles hold 12 wires, too few;
        // 5 x 5 hold 20, still too few; 9 x 9 hold 36
        let seeds = tree.seeds_near(50, 12).expect("a local part");
        assert_eq!(seeds.len(), 36);
        // A route no wider than the square: no local part, the whole route
        let mut small = TreeIndex::new();
        for x in 0..3 {
            small.insert(Some((x, 0)), WireId(x));
        }
        assert!(small.seeds_near(1, 1).is_none());
    }

    #[test]
    fn rerouting_keeps_the_branches_clear_of_other_nets() {
        // source 0 -> 1 -> 2 -> sink 3, and 1 -> 4 -> 5 -> sink 6, plus a
        // dead end 1 -> 7; pip k drives wire k
        let w = WireId;
        let ends = |p: PipId| {
            let parent = [0, 0, 1, 2, 1, 4, 5, 1][p.0 as usize];
            Some((WireId(parent), WireId(p.0)))
        };
        let mut route = Route::new(GateNetId(0));
        route.source = w(0);
        route.sinks = vec![w(3), w(6)];
        route.wires = (0..8).map(w).collect();
        route.pips = (1..8).map(PipId).collect();

        // Wire 5 is shared: the branch to sink 6 goes, and so does the dead end
        let (wires, pips, reached) =
            PathFinder::<NexusDevice>::prune_route(&route, &[w(6), w(3)], |x| x == w(5), ends)
                .unwrap();
        assert_eq!(wires, vec![w(0), w(1), w(2), w(3)]);
        assert_eq!(pips, vec![PipId(1), PipId(2), PipId(3)]);
        assert_eq!(reached, vec![w(3)]);

        // Nothing shared: both branches stay, the dead end still goes
        let (wires, _, reached) =
            PathFinder::<NexusDevice>::prune_route(&route, &[w(3), w(6)], |_| false, ends).unwrap();
        assert_eq!(wires.len(), 7);
        assert!(!wires.contains(&w(7)));
        assert_eq!(reached, vec![w(3), w(6)]);

        // Sinks that differ from the net's pins: rerouted whole
        assert!(PathFinder::<NexusDevice>::prune_route(&route, &[w(3)], |_| false, ends).is_none());
    }

    #[test]
    fn a_branch_joins_the_tree_at_its_last_tree_wire() {
        let w = WireId;
        let tree: HashSet<WireId> = [w(0), w(1), w(2), w(3)].into_iter().collect();
        // Searched from wire 1 (a seed near the sink), the path runs
        // 1 -> 10 -> 3 -> 11 -> sink 12: wire 3 is already driven in the
        // tree, so the branch starts there
        let (wires, pips) = PathFinder::<NexusDevice>::graft(
            vec![w(1), w(10), w(3), w(11), w(12)],
            vec![PipId(10), PipId(3), PipId(11), PipId(12)],
            &tree,
        );
        assert_eq!(wires, vec![w(3), w(11), w(12)]);
        assert_eq!(pips, vec![PipId(11), PipId(12)]);
        // A path that leaves the tree for good is kept whole
        let (wires, pips) = PathFinder::<NexusDevice>::graft(
            vec![w(2), w(10), w(11)],
            vec![PipId(10), PipId(11)],
            &tree,
        );
        assert_eq!(wires, vec![w(2), w(10), w(11)]);
        assert_eq!(pips.len(), 2);
    }

    #[test]
    fn routes_only_the_nets_it_is_given() {
        let device = NexusDevice::new(NexusVariant::Lifcl40);
        let (netlist, placement, [first, second]) = two_nets(&device);
        let pathfinder = PathFinder::new(&device, 10, 1.0, 1.5);

        let one = pathfinder
            .route_nets(&netlist, &placement, &[first])
            .unwrap();
        assert!(one.success);
        assert_eq!(one.routes.keys().copied().collect::<Vec<_>>(), vec![first]);
        let route = &one.routes[&first];
        assert_eq!(route.unrouted_sinks, 0);
        assert_eq!(route.sinks.len(), 1);
        assert!(route.wires.contains(&route.sinks[0]));

        let all = pathfinder.route(&netlist, &placement).unwrap();
        assert!(all.success);
        let mut routed: Vec<_> = all.routes.keys().copied().collect();
        routed.sort_by_key(|n| n.0);
        assert_eq!(routed, vec![first, second]);
    }
}
