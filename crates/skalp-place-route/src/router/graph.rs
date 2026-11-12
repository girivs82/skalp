//! Compact routing graph for the router's searches.
//!
//! The device describes its routing resources as `Wire` and `Pip` records and
//! a per-wire list of driven PIPs, which on the large Lattice devices is a
//! hash map. A route search touches millions of edges, and following each one
//! through that hash map plus two record lookups (the PIP, then the wire it
//! reaches for the heuristic) made routing memory-bound. [`RoutingGraph`]
//! flattens what the search needs into arrays indexed by wire: the outgoing
//! edges of every wire in one contiguous slice, each with its destination,
//! PIP and costs, and every wire's tile.

use crate::collections::{HashMap, HashMapExt};
use crate::device::{Device, PipId, WireId, WireType};
use crate::timing::DelayModel;
use std::sync::OnceLock;

use super::lookahead::{self, Lookahead};

/// One PIP as the search sees it
#[derive(Debug, Clone, Copy)]
pub struct Edge {
    /// Wire the PIP drives
    pub dst: WireId,
    /// The PIP itself
    pub pip: PipId,
    /// Base routing cost ([`crate::device::Pip::cost`])
    pub cost: f32,
    /// Delay term, scaled by a net's timing weight in timing-driven searches
    pub timing: f32,
}

/// Flattened adjacency of a device's routing wires
#[derive(Debug, Clone, Default)]
pub struct RoutingGraph {
    /// `edges[offsets[w]..offsets[w + 1]]` are the PIPs wire `w` drives
    offsets: Vec<u32>,
    edges: Vec<Edge>,
    /// Tile of each wire
    tile: Vec<(u32, u32)>,
    /// Intrinsic delay of each wire (ps)
    delay: Vec<u32>,
    /// Least cost of moving `d` tiles along x (`hop_cost_x[d]`) or y,
    /// beyond which it grows linearly at `hop_slope_*` per tile
    hop_cost_x: Vec<f64>,
    hop_cost_y: Vec<f64>,
    hop_slope_x: f64,
    hop_slope_y: f64,
    /// The same for the PIPs' timing terms: a bound on the delay of moving
    /// `d` tiles, which a timing-driven search adds (scaled by the net's
    /// timing weight) to the cost bound
    hop_timing_x: Vec<f64>,
    hop_timing_y: Vec<f64>,
    hop_timing_slope_x: f64,
    hop_timing_slope_y: f64,
    /// Whether some PIP moves along both axes at once
    diagonal: bool,
    /// Wires of a global distribution network (clock spines and branches)
    global: Vec<bool>,
    /// `rev_edges[rev_offsets[w]..rev_offsets[w + 1]]` are the wires with a
    /// PIP driving wire `w`, with that PIP's base cost
    rev_offsets: Vec<u32>,
    rev_edges: Vec<(WireId, f32)>,
    /// Class of each wire ([`lookahead::class_key`]) and the number of classes
    class: Vec<u32>,
    class_count: usize,
    /// Measured costs by wire class, built on first use
    lookahead: OnceLock<Lookahead>,
}

/// What a path must still spend inside its sink's tile, for
/// [`RoutingGraph::cost_lower_bound`]: the least cost from each wire of the
/// tile to the sink without leaving the tile, and the least such cost from a
/// wire a path can enter the tile on
#[derive(Debug, Clone, Default)]
pub struct SinkApproach {
    tile: (u32, u32),
    within_tile: HashMap<WireId, f64>,
    entry: f64,
    /// The sink is reached from another tile by a PIP cheaper than the
    /// displacement bound allows (one into a hard block pin from a pad far
    /// away, left out of that bound): distance gives no bound then.
    far_entry: bool,
}

impl SinkApproach {
    /// Whether distance gives no bound on reaching the sink (see `far_entry`)
    pub fn far_entry(&self) -> bool {
        self.far_entry
    }

    /// A lower bound on the cost from `wire`, in tile `tile`, to the sink:
    /// a path staying in the sink's tile spends at least the in-tile
    /// distance, and one that leaves it re-enters on an entry wire
    fn remaining(&self, wire: WireId, tile: (u32, u32)) -> f64 {
        if tile == self.tile {
            self.within_tile
                .get(&wire)
                .map_or(self.entry, |&d| d.min(self.entry))
        } else {
            self.entry
        }
    }
}

impl RoutingGraph {
    /// Build the graph of `device`'s wires `0..device.wire_count()`. An
    /// edge's timing term (in 50 ps units) is the PIP's own delay where the
    /// device data gives one, so the search sees the delays the timing
    /// analysis will report; otherwise the delay model's typed PIP delay.
    pub fn build<D: Device + ?Sized>(device: &D, delay_model: Option<&DelayModel>) -> Self {
        let n = device.wire_count();
        let mut offsets = Vec::with_capacity(n + 1);
        let mut edges = Vec::new();
        let mut tile = Vec::with_capacity(n);
        let mut delay = Vec::with_capacity(n);
        let mut global = Vec::with_capacity(n);
        let mut class = Vec::with_capacity(n);
        let mut class_ids: HashMap<String, u32> = HashMap::new();
        offsets.push(0);
        for i in 0..n {
            let id = WireId(i as u32);
            let wire = device.wire(id);
            tile.push(wire.map_or((0, 0), |w| (w.tile_x, w.tile_y)));
            delay.push(wire.map_or(0, |w| w.delay));
            global.push(wire.is_some_and(|w| matches!(w.wire_type, WireType::Global(_))));
            let key = wire.map_or_else(String::new, |w| lookahead::class_key(&w.name));
            let next = class_ids.len() as u32;
            class.push(*class_ids.entry(key).or_insert(next));
            for &pip_id in device.wire_src_pips(id) {
                let Some(pip) = device.pip(pip_id) else {
                    continue;
                };
                let timing = match (delay_model, wire, device.wire(pip.dst_wire)) {
                    _ if pip.delay > 0 => pip.delay as f64 / 50.0,
                    (Some(model), Some(src), Some(dst)) => {
                        model.pip_delay_typed(&src.wire_type, &dst.wire_type) / 0.05
                    }
                    (Some(model), _, _) => model.pip_delay / 0.05,
                    (None, _, _) => pip.delay as f64 / 50.0,
                };
                edges.push(Edge {
                    dst: pip.dst_wire,
                    pip: pip_id,
                    cost: pip.cost() as f32,
                    timing: timing as f32,
                });
            }
            offsets.push(edges.len() as u32);
        }
        let mut graph = Self {
            offsets,
            edges,
            tile,
            delay,
            global,
            class,
            class_count: class_ids.len(),
            ..Self::default()
        };
        graph.compute_reverse_edges();
        graph.compute_hop_costs();
        graph
    }

    /// The reverse adjacency (wires driving each wire)
    fn compute_reverse_edges(&mut self) {
        let n = self.wire_count();
        let mut count = vec![0u32; n + 1];
        for e in &self.edges {
            if let Some(c) = count.get_mut(e.dst.0 as usize + 1) {
                *c += 1;
            }
        }
        for i in 0..n {
            count[i + 1] += count[i];
        }
        let mut fill = count.clone();
        let mut rev = vec![(WireId(0), 0.0f32); count[n] as usize];
        for w in 0..n {
            for e in &self.edges[self.offsets[w] as usize..self.offsets[w + 1] as usize] {
                let Some(slot) = fill.get_mut(e.dst.0 as usize) else {
                    continue;
                };
                rev[*slot as usize] = (WireId(w as u32), e.cost);
                *slot += 1;
            }
        }
        self.rev_offsets = count;
        self.rev_edges = rev;
    }

    /// Wires with a PIP driving `wire`, with the PIP's base cost
    fn drivers(&self, wire: WireId) -> &[(WireId, f32)] {
        let i = wire.0 as usize;
        match (self.rev_offsets.get(i), self.rev_offsets.get(i + 1)) {
            (Some(&a), Some(&b)) => &self.rev_edges[a as usize..b as usize],
            _ => &[],
        }
    }

    /// The cost a path to `sink` must spend inside the sink's tile after it
    /// last changes tile: a search backwards from the sink over PIPs that
    /// stay in the tile. Those PIPs move no distance, so the displacement
    /// bound counts none of them and the two add up.
    pub fn sink_approach(&self, sink: WireId) -> SinkApproach {
        let Some(tile) = self.tile(sink) else {
            return SinkApproach::default();
        };
        let mut within_tile: HashMap<WireId, f64> = HashMap::new();
        let mut heap = std::collections::BinaryHeap::new();
        within_tile.insert(sink, 0.0);
        heap.push(std::cmp::Reverse((OrderedCost(0.0), sink.0)));
        let mut entry = f64::INFINITY;
        let mut far_entry = false;
        while let Some(std::cmp::Reverse((OrderedCost(d), w))) = heap.pop() {
            let w = WireId(w);
            if within_tile.get(&w).is_some_and(|&best| d > best) {
                continue;
            }
            for &(src, cost) in self.drivers(w) {
                if self.tile(src) != Some(tile) {
                    // A path can enter the tile on `w`
                    entry = entry.min(d);
                    // (Global networks are left out of the bound on purpose;
                    // their PIPs, such as a constant's, say nothing here.)
                    if let Some((x, y)) = self.tile(src).filter(|_| !self.is_global(src)) {
                        let bound = self.cost_lower_bound(x.abs_diff(tile.0), y.abs_diff(tile.1));
                        far_entry |= (cost as f64) < bound;
                    }
                    continue;
                }
                let nd = d + cost as f64;
                if within_tile.get(&src).is_none_or(|&best| nd < best) {
                    within_tile.insert(src, nd);
                    heap.push(std::cmp::Reverse((OrderedCost(nd), src.0)));
                }
            }
        }
        SinkApproach {
            tile,
            within_tile,
            entry: if entry.is_finite() { entry } else { 0.0 },
            far_entry,
        }
    }

    /// Lower bounds on the cost of moving a given number of tiles, from the
    /// PIPs' own displacements (the change of tile from the wire they leave
    /// to the wire they drive) and costs. A path that moves `d` tiles along
    /// an axis uses PIPs whose displacements along it add up to at least
    /// `d`, so the cheapest such multiset of PIPs bounds its cost.
    ///
    /// PIPs into a wire that drives nothing (a hard block's input pin) are
    /// left out too: such a PIP can only end a path, at that pin, and a
    /// path into a sink reached this way is bounded by [`Self::sink_approach`]
    /// instead. The CertusPro-NX ADC takes inputs straight from pads up to 82
    /// columns away, at the cost of one PIP; counting those made the bound
    /// one PIP for 82 tiles, so a search to a sink 40 tiles away expanded up
    /// to a million wires.
    ///
    /// PIPs into or out of a global distribution network are left out. A
    /// clock branch reaches across the device for the cost of one PIP, so
    /// counting it made the bound about one PIP's cost at any distance (on
    /// the real LFCPNX-100 graph), and every sink search expanded tens of
    /// thousands of wires. Without them the bound holds for paths in the
    /// general fabric; a path through a global network may cost less than
    /// it says, so a search finds such a path only if it is no dearer than
    /// the fabric's.
    fn compute_hop_costs(&mut self) {
        // Cheapest PIP per displacement length, per axis
        let mut best_x: Vec<f64> = Vec::new();
        let mut best_y: Vec<f64> = Vec::new();
        let mut timing_x: Vec<f64> = Vec::new();
        let mut timing_y: Vec<f64> = Vec::new();
        let note = |best: &mut Vec<f64>, d: u32, cost: f64| {
            let d = d as usize;
            if best.len() <= d {
                best.resize(d + 1, f64::INFINITY);
            }
            best[d] = best[d].min(cost);
        };
        for w in 0..self.wire_count() {
            let (sx, sy) = self.tile[w];
            for e in &self.edges[self.offsets[w] as usize..self.offsets[w + 1] as usize] {
                let Some(&(tx, ty)) = self.tile.get(e.dst.0 as usize) else {
                    continue;
                };
                let d = e.dst.0 as usize;
                if self.global[w] || self.global[d] {
                    continue;
                }
                let drives_nothing = self
                    .offsets
                    .get(d + 1)
                    .is_none_or(|&end| end == self.offsets[d]);
                if drives_nothing {
                    continue;
                }
                let (dx, dy) = (sx.abs_diff(tx), sy.abs_diff(ty));
                if dx > 0 && dy > 0 {
                    self.diagonal = true;
                }
                if dx > 0 {
                    note(&mut best_x, dx, e.cost as f64);
                    note(&mut timing_x, dx, e.timing as f64);
                }
                if dy > 0 {
                    note(&mut best_y, dy, e.cost as f64);
                    note(&mut timing_y, dy, e.timing as f64);
                }
            }
        }
        log::debug!("routing graph: least PIP cost by x displacement {best_x:?}");
        log::debug!("routing graph: least PIP cost by y displacement {best_y:?}");
        log::debug!("routing graph: least PIP timing by x displacement {timing_x:?}");
        log::debug!("routing graph: least PIP timing by y displacement {timing_y:?}");
        (self.hop_cost_x, self.hop_slope_x) = Self::covering_costs(&best_x);
        (self.hop_cost_y, self.hop_slope_y) = Self::covering_costs(&best_y);
        (self.hop_timing_x, self.hop_timing_slope_x) = Self::covering_costs(&timing_x);
        (self.hop_timing_y, self.hop_timing_slope_y) = Self::covering_costs(&timing_y);
    }

    /// From the cheapest PIP per displacement (`best[k]` for `k` tiles), the
    /// least cost of PIPs whose displacements add up to at least `d`, for
    /// `d` up to twice the longest displacement, and the least cost per
    /// tile (which bounds the cost of longer moves).
    fn covering_costs(best: &[f64]) -> (Vec<f64>, f64) {
        let longest = best.len().saturating_sub(1);
        if longest == 0 {
            return (vec![0.0], 0.0);
        }
        let slope = (1..=longest)
            .filter(|&k| best[k].is_finite())
            .map(|k| best[k] / k as f64)
            .fold(f64::INFINITY, f64::min);
        let n = 2 * longest + 1;
        let mut cost = vec![0.0; n];
        for d in 1..n {
            cost[d] = (1..=longest)
                .filter(|&k| best[k].is_finite())
                .map(|k| best[k] + cost[d.saturating_sub(k)])
                .fold(f64::INFINITY, f64::min);
        }
        (cost, slope)
    }

    fn axis_cost(table: &[f64], slope: f64, d: u32) -> f64 {
        let d = d as usize;
        match table.get(d) {
            Some(&c) => c,
            None => {
                // Past the table: its last entry plus the cheapest rate
                let last = table.len() - 1;
                table[last] + slope * (d - last) as f64
            }
        }
    }

    /// A lower bound on the cost from `wire` to the sink `approach`
    /// describes (A*'s heuristic): the displacement bound to the sink's tile
    /// plus what the path must still spend inside it
    pub fn cost_to_sink(&self, wire: WireId, approach: &SinkApproach) -> f64 {
        let Some((x, y)) = self.tile(wire) else {
            return 0.0;
        };
        if approach.far_entry {
            return approach.remaining(wire, (x, y));
        }
        let (dx, dy) = (x.abs_diff(approach.tile.0), y.abs_diff(approach.tile.1));
        self.cost_lower_bound(dx, dy) + approach.remaining(wire, (x, y))
    }

    /// The router's estimate of the cost from `wire` to the sink
    /// `approach` describes: the cost measured from a wire of the same class
    /// to a tile at the same displacement ([`Lookahead`]), plus what the
    /// path must spend inside the sink's tile, and never less than
    /// [`Self::cost_to_sink`]. Sinks out of the measured radius take the
    /// measured cost to the nearest tile within it plus the bound for the
    /// rest of the way.
    pub fn estimate_to_sink(&self, wire: WireId, approach: &SinkApproach) -> f64 {
        let bound = self.cost_to_sink(wire, approach);
        let Some((x, y)) = self.tile(wire) else {
            return bound;
        };
        if (x, y) == approach.tile || approach.far_entry {
            return bound;
        }
        let Some(&class) = self.class.get(wire.0 as usize) else {
            return bound;
        };
        let radius = lookahead::RADIUS;
        let dx = approach.tile.0 as i32 - x as i32;
        let dy = approach.tile.1 as i32 - y as i32;
        let (near_dx, near_dy) = (dx.clamp(-radius, radius), dy.clamp(-radius, radius));
        let Some(measured) = self.lookahead().cost(class, near_dx, near_dy) else {
            return bound;
        };
        let beyond = self.cost_lower_bound(dx.abs_diff(near_dx), dy.abs_diff(near_dy));
        bound.max(measured + beyond + approach.entry)
    }

    /// The router's estimate of the summed timing terms from `wire` to the
    /// sink `approach` describes, as [`Self::estimate_to_sink`] is for base
    /// costs: the least delay measured from a wire of the same class to a
    /// tile at the same displacement, never less than
    /// [`Self::timing_lower_bound`]
    pub fn estimate_timing_to_sink(&self, wire: WireId, approach: &SinkApproach) -> f64 {
        let Some((x, y)) = self.tile(wire) else {
            return 0.0;
        };
        if approach.far_entry {
            // Distance gives no bound on reaching such a sink
            return 0.0;
        }
        let (tx, ty) = approach.tile;
        let bound = self.timing_lower_bound(x.abs_diff(tx), y.abs_diff(ty));
        if (x, y) == approach.tile {
            return bound;
        }
        let Some(&class) = self.class.get(wire.0 as usize) else {
            return bound;
        };
        let radius = lookahead::RADIUS;
        let dx = tx as i32 - x as i32;
        let dy = ty as i32 - y as i32;
        let (near_dx, near_dy) = (dx.clamp(-radius, radius), dy.clamp(-radius, radius));
        let Some(measured) = self.lookahead().delay(class, near_dx, near_dy) else {
            return bound;
        };
        let beyond = self.timing_lower_bound(dx.abs_diff(near_dx), dy.abs_diff(near_dy));
        bound.max(measured + beyond)
    }

    /// The lookahead, measured on first use
    pub fn lookahead(&self) -> &Lookahead {
        self.lookahead
            .get_or_init(|| Lookahead::build(self, &self.class, self.class_count))
    }

    /// A lower bound on the cost of a path from a wire to one `dx` and `dy`
    /// tiles away (A*'s heuristic). Never more than any real path's cost,
    /// since every wire entered costs at least its PIP's base cost.
    pub fn cost_lower_bound(&self, dx: u32, dy: u32) -> f64 {
        if self.hop_cost_x.is_empty() {
            return 0.0;
        }
        let x = Self::axis_cost(&self.hop_cost_x, self.hop_slope_x, dx);
        let y = Self::axis_cost(&self.hop_cost_y, self.hop_slope_y, dy);
        if self.diagonal {
            // One PIP may serve both axes: only the larger part is certain
            x.max(y)
        } else {
            x + y
        }
    }

    /// A lower bound on the summed timing terms of a path from a wire to
    /// one `dx` and `dy` tiles away, as [`Self::cost_lower_bound`] is for
    /// base costs. A timing-driven search's cost is base cost plus timing
    /// weight times timing, and each part is bounded on its own.
    pub fn timing_lower_bound(&self, dx: u32, dy: u32) -> f64 {
        if self.hop_timing_x.is_empty() {
            return 0.0;
        }
        let x = Self::axis_cost(&self.hop_timing_x, self.hop_timing_slope_x, dx);
        let y = Self::axis_cost(&self.hop_timing_y, self.hop_timing_slope_y, dy);
        if self.diagonal {
            x.max(y)
        } else {
            x + y
        }
    }

    /// Number of wires
    pub fn wire_count(&self) -> usize {
        self.tile.len()
    }

    /// PIPs driven by `wire`
    #[inline]
    pub fn edges(&self, wire: WireId) -> &[Edge] {
        let i = wire.0 as usize;
        match (self.offsets.get(i), self.offsets.get(i + 1)) {
            (Some(&a), Some(&b)) => &self.edges[a as usize..b as usize],
            _ => &[],
        }
    }

    /// Whether `wire` belongs to a global distribution network
    pub(super) fn is_global(&self, wire: WireId) -> bool {
        self.global.get(wire.0 as usize).copied().unwrap_or(false)
    }

    /// Tile of `wire`
    #[inline]
    pub fn tile(&self, wire: WireId) -> Option<(u32, u32)> {
        self.tile.get(wire.0 as usize).copied()
    }

    /// Intrinsic delay of `wire` (ps)
    #[inline]
    pub fn wire_delay(&self, wire: WireId) -> u32 {
        self.delay.get(wire.0 as usize).copied().unwrap_or(0)
    }
}

/// An `f64` cost ordered for a heap (costs are never NaN)
#[derive(Debug, Clone, Copy)]
struct OrderedCost(f64);

impl PartialEq for OrderedCost {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other).is_eq()
    }
}

impl Eq for OrderedCost {}

impl PartialOrd for OrderedCost {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for OrderedCost {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.total_cmp(&other.0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::device::ice40::{Ice40Device, Ice40Variant};

    #[test]
    fn graph_matches_device_pips() {
        let device = Ice40Device::new(Ice40Variant::Hx1k);
        let graph = RoutingGraph::build(&device, None);
        assert_eq!(graph.wire_count(), device.wire_count());
        let mut total = 0;
        for i in 0..device.wire_count() {
            let w = WireId(i as u32);
            let from_device: Vec<PipId> = device.wire_src_pips(w).to_vec();
            let from_graph: Vec<PipId> = graph.edges(w).iter().map(|e| e.pip).collect();
            assert_eq!(from_graph, from_device, "wire {i}");
            for e in graph.edges(w) {
                let pip = device.pip(e.pip).unwrap();
                assert_eq!(e.dst, pip.dst_wire);
                assert_eq!(e.cost, pip.cost() as f32);
            }
            let wire = device.wire(w).unwrap();
            assert_eq!(graph.tile(w), Some((wire.tile_x, wire.tile_y)));
            total += from_device.len();
        }
        assert!(total > 0);
    }

    #[test]
    fn cost_lower_bound_never_exceeds_a_shortest_path() {
        use crate::router::AStarRouter;
        let device = Ice40Device::new(Ice40Variant::Hx1k);
        let graph = RoutingGraph::build(&device, None);
        let astar = AStarRouter::new(&device);
        assert_eq!(graph.cost_lower_bound(0, 0), 0.0);
        assert!(graph.cost_lower_bound(5, 0) > 0.0);
        assert!(graph.cost_lower_bound(10, 3) >= graph.cost_lower_bound(5, 3));
        // Against exact path costs (unit wire costs: the sum of PIP costs)
        let (w, h) = device.grid_size();
        let pin = |x, y| {
            Device::bel_pin_wire(&device, x, y, 0, crate::device::CellPin::Output(0))
                .or_else(|| Device::lut_output_wire(&device, x, y, 0))
        };
        let sink = |x, y| Device::lut_input_wire(&device, x, y, 0, 0);
        let mut checked = 0;
        for (x0, y0, x1, y1) in [
            (2, 2, w - 3, h - 3),
            (3, 5, 9, 5),
            (6, 2, 6, 12),
            (4, 4, 5, 6),
        ] {
            let (Some(src), Some(dst)) = (pin(x0, y0), sink(x1, y1)) else {
                continue;
            };
            let Ok((_, _, cost)) =
                astar.find_path_bounded(&[src], dst, &[], |_| 1.0, 0.0, |_| 0.0, None)
            else {
                continue;
            };
            let (sx, sy) = graph.tile(src).unwrap();
            let (tx, ty) = graph.tile(dst).unwrap();
            let bound = graph.cost_lower_bound(sx.abs_diff(tx), sy.abs_diff(ty));
            assert!(bound <= cost + 1e-9, "bound {bound} above path cost {cost}");
            // With what the path spends inside the sink's tile added, the
            // bound still holds, and it is tighter
            let approach = graph.sink_approach(dst);
            let to_sink = graph.cost_to_sink(src, &approach);
            assert!(
                to_sink <= cost + 1e-9,
                "bound {to_sink} above path cost {cost}"
            );
            assert!(to_sink >= bound);
            // The measured lookahead tightens it further, and on this
            // regular fabric still never passes the real cost
            let estimate = graph.estimate_to_sink(src, &approach);
            assert!(estimate >= to_sink);
            assert!(
                estimate <= cost + 1e-6,
                "estimate {estimate} above path cost {cost}"
            );
            checked += 1;
        }
        assert!(checked > 0);
    }

    /// An edge's timing term is its PIP's own delay (in 50 ps units) where
    /// the device gives one, and the timing bound never exceeds a path's
    /// summed timing terms: a timing-driven search's cost (base cost plus
    /// timing weight times timing) is bounded by the two bounds together
    #[test]
    fn timing_terms_follow_pip_delays_and_are_bounded() {
        use crate::router::AStarRouter;
        let device = Ice40Device::new(Ice40Variant::Hx1k);
        let model = DelayModel::ice40_default();
        let graph = RoutingGraph::build(&device, Some(&model));
        let mut with_delay = 0;
        for i in 0..device.wire_count() {
            for e in graph.edges(WireId(i as u32)) {
                let pip = device.pip(e.pip).unwrap();
                if pip.delay > 0 {
                    assert_eq!(e.timing, (pip.delay as f64 / 50.0) as f32);
                    with_delay += 1;
                }
            }
        }
        assert!(with_delay > 0);

        let astar = AStarRouter::with_delay_model(&device, model);
        let timing_weight = 2.0;
        let (w, h) = device.grid_size();
        let pin = |x, y| {
            Device::bel_pin_wire(&device, x, y, 0, crate::device::CellPin::Output(0))
                .or_else(|| Device::lut_output_wire(&device, x, y, 0))
        };
        let sink = |x, y| Device::lut_input_wire(&device, x, y, 0, 0);
        let mut checked = 0;
        for (x0, y0, x1, y1) in [(2, 2, w - 3, h - 3), (3, 5, 9, 5), (6, 2, 6, 12)] {
            let (Some(src), Some(dst)) = (pin(x0, y0), sink(x1, y1)) else {
                continue;
            };
            let Ok((wires, _, cost)) =
                astar.find_path_bounded(&[src], dst, &[], |_| 1.0, timing_weight, |_| 0.0, None)
            else {
                continue;
            };
            let (sx, sy) = graph.tile(src).unwrap();
            let (tx, ty) = graph.tile(dst).unwrap();
            let (dx, dy) = (sx.abs_diff(tx), sy.abs_diff(ty));
            assert!(graph.timing_lower_bound(dx, dy) > 0.0);
            let bound =
                graph.cost_lower_bound(dx, dy) + timing_weight * graph.timing_lower_bound(dx, dy);
            assert!(bound <= cost + 1e-6, "bound {bound} above path cost {cost}");
            // The measured delays tighten the timing part, and with the
            // measured costs still never pass the real cost on this fabric
            let approach = graph.sink_approach(dst);
            let timing = graph.estimate_timing_to_sink(src, &approach);
            assert!(timing >= graph.timing_lower_bound(dx, dy));
            let estimate = graph.estimate_to_sink(src, &approach) + timing_weight * timing;
            assert!(
                estimate <= cost + 1e-6,
                "estimate {estimate} above path cost {cost}"
            );
            assert!(wires.len() > 1);
            checked += 1;
        }
        assert!(checked > 0);
    }

    /// On real device data: the estimate from a wire one PIP outside a
    /// sink's tile never exceeds what that PIP and the rest of the way to the
    /// sink cost. Hard block pins taken straight from far pads (the
    /// CertusPro-NX ADC's) are where it used to break.
    #[test]
    fn cost_to_sink_never_exceeds_a_last_hop_on_real_device() {
        use crate::device::nexus::{prjoxide_load, NexusDevice, NexusVariant};
        let Some(db) = prjoxide_load::find_database() else {
            eprintln!("PRJOXIDE_DB not found — skipping");
            return;
        };
        let device = NexusDevice::from_prjoxide_bbox(NexusVariant::Lfcpnx100, &db, (0, 0, 158, 73))
            .expect("build device");
        let graph = RoutingGraph::build(&device, None);
        let mut checked = 0usize;
        for i in (0..graph.wire_count()).step_by(53) {
            let sink = WireId(i as u32);
            let approach = graph.sink_approach(sink);
            for (&w, &inside) in &approach.within_tile {
                for &(src, cost) in graph.drivers(w) {
                    if graph.tile(src) == Some(approach.tile) || graph.is_global(src) {
                        continue;
                    }
                    let estimate = graph.cost_to_sink(src, &approach);
                    let actual = cost as f64 + inside;
                    assert!(
                        estimate <= actual + 1e-9,
                        "estimate {estimate} above {actual} from {src:?} to {sink:?}"
                    );
                    checked += 1;
                }
            }
        }
        assert!(checked > 0);
    }
}
