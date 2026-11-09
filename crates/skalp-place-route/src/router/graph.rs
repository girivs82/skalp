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

use crate::device::{Device, PipId, WireId};
use crate::timing::DelayModel;

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
    /// Whether some PIP moves along both axes at once
    diagonal: bool,
}

impl RoutingGraph {
    /// Build the graph of `device`'s wires `0..device.wire_count()`. With a
    /// delay model, an edge's timing term is the model's typed PIP delay
    /// (in 50 ps units); otherwise the PIP's own delay.
    pub fn build<D: Device + ?Sized>(device: &D, delay_model: Option<&DelayModel>) -> Self {
        let n = device.wire_count();
        let mut offsets = Vec::with_capacity(n + 1);
        let mut edges = Vec::new();
        let mut tile = Vec::with_capacity(n);
        let mut delay = Vec::with_capacity(n);
        offsets.push(0);
        for i in 0..n {
            let id = WireId(i as u32);
            let wire = device.wire(id);
            tile.push(wire.map_or((0, 0), |w| (w.tile_x, w.tile_y)));
            delay.push(wire.map_or(0, |w| w.delay));
            for &pip_id in device.wire_src_pips(id) {
                let Some(pip) = device.pip(pip_id) else {
                    continue;
                };
                let timing = match (delay_model, wire, device.wire(pip.dst_wire)) {
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
            ..Self::default()
        };
        graph.compute_hop_costs();
        graph
    }

    /// Lower bounds on the cost of moving a given number of tiles, from the
    /// PIPs' own displacements (the change of tile from the wire they leave
    /// to the wire they drive) and costs. A path that moves `d` tiles along
    /// an axis uses PIPs whose displacements along it add up to at least
    /// `d`, so the cheapest such multiset of PIPs bounds its cost.
    fn compute_hop_costs(&mut self) {
        // Cheapest PIP per displacement length, per axis
        let mut best_x: Vec<f64> = Vec::new();
        let mut best_y: Vec<f64> = Vec::new();
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
                let (dx, dy) = (sx.abs_diff(tx), sy.abs_diff(ty));
                if dx > 0 && dy > 0 {
                    self.diagonal = true;
                }
                if dx > 0 {
                    note(&mut best_x, dx, e.cost as f64);
                }
                if dy > 0 {
                    note(&mut best_y, dy, e.cost as f64);
                }
            }
        }
        (self.hop_cost_x, self.hop_slope_x) = Self::covering_costs(&best_x);
        (self.hop_cost_y, self.hop_slope_y) = Self::covering_costs(&best_y);
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
            let Ok((_, _, cost)) = astar.find_path_bounded(&[src], dst, &[], |_| 1.0, 0.0, None)
            else {
                continue;
            };
            let (sx, sy) = graph.tile(src).unwrap();
            let (tx, ty) = graph.tile(dst).unwrap();
            let bound = graph.cost_lower_bound(sx.abs_diff(tx), sy.abs_diff(ty));
            assert!(bound <= cost + 1e-9, "bound {bound} above path cost {cost}");
            checked += 1;
        }
        assert!(checked > 0);
    }
}
