//! Routing congestion estimated from a placement, for placement to avoid.
//!
//! Each net is assumed to need its half-perimeter of wire, spread evenly over
//! its bounding box (RUDY: a net `w` by `h` tiles asks `(w + h) / (w * h)`
//! tile-lengths of wire of every tile in its box). The demand is summed per
//! bin of tiles and set against the routing the device has there: the
//! general routing wires that start in the bin's tiles. A bin asked for more
//! than its share is congested, and the router will struggle there, however
//! short the wiring is overall.

use crate::collections::{HashMap, HashMapExt};
use crate::device::Device;
use crate::placer::{is_routed_net, PlacementResult};
use skalp_lir::gate_netlist::{GateNet, GateNetlist};

/// Tiles per side of a bin
const BIN: u32 = 4;

/// Wire demand, in tile-lengths, that one general routing wire of a tile
/// can carry before the router has trouble there. Calibrated on Nexus
/// (LFCPNX-100): a 7K-cell core
/// whose busiest 8x8 blocks ask for about 90 tile-lengths a tile routes in a
/// handful of passes (busiest bins at 0.96 of this capacity), a 52K-cell one
/// whose busiest blocks ask for 130 to 150 (1.3 of it) needs dozens.
pub(crate) const DEMAND_PER_WIRE: f64 = 1.7;

/// Tiles `x0..=x1` × `y0..=y1` a net's pins span
pub(crate) type NetBox = (u32, u32, u32, u32);

/// Per-bin wire demand of a placement and the device's supply
pub(crate) struct CongestionMap {
    bins_x: u32,
    bins_y: u32,
    /// Demand a bin can take before it counts as congested (0: no routing)
    capacity: Vec<f64>,
    /// Current demand of each bin
    demand: Vec<f64>,
    /// Each net's box, or none for a net that adds no demand
    boxes: Vec<Option<NetBox>>,
    /// Sum over bins of [`overflow_cost`]
    total: f64,
}

impl CongestionMap {
    /// The device's routing supply, or none for a device that does not say
    /// which of its wires are general routing
    pub fn new<D: Device + ?Sized>(device: &D) -> Option<Self> {
        let (width, height) = device.grid_size();
        let bins_x = width.div_ceil(BIN);
        let bins_y = height.div_ceil(BIN);
        let mut capacity = vec![0.0; (bins_x * bins_y) as usize];
        let mut any = false;
        for y in 0..height {
            for x in 0..width {
                let wires = device
                    .tile_wires(x, y)
                    .into_iter()
                    .filter(|&w| device.is_general_routing_wire(w))
                    .count();
                any |= wires > 0;
                capacity[((y / BIN) * bins_x + x / BIN) as usize] += wires as f64 * DEMAND_PER_WIRE;
            }
        }
        any.then(|| Self {
            bins_x,
            bins_y,
            demand: vec![0.0; capacity.len()],
            capacity,
            boxes: Vec::new(),
            total: 0.0,
        })
    }

    /// Add every net of `netlist` as placed in `placement`
    pub fn build(&mut self, netlist: &GateNetlist, placement: &PlacementResult) {
        self.demand.iter_mut().for_each(|d| *d = 0.0);
        self.boxes = netlist
            .nets
            .iter()
            .map(|net| {
                is_routed_net(net, netlist)
                    .then(|| net_box(net, placement))
                    .flatten()
            })
            .collect();
        for b in self.boxes.clone().into_iter().flatten() {
            self.add_box(b, 1.0);
        }
        self.total = (0..self.demand.len())
            .map(|i| overflow_cost(self.demand[i], self.capacity[i]))
            .sum();
    }

    /// Total cost of demand over capacity
    pub fn total(&self) -> f64 {
        self.total
    }

    /// The change in [`Self::total`] if the nets `changes` lists had the
    /// boxes given there, and the per-bin demand changes that make it
    pub fn delta(&self, changes: &[(usize, Option<NetBox>)]) -> (f64, Vec<(usize, f64)>) {
        let mut bins: HashMap<usize, f64> = HashMap::new();
        for &(net, new) in changes {
            let old = self.boxes.get(net).copied().flatten();
            if old == new {
                continue;
            }
            if let Some(b) = old {
                self.for_bins(b, |i, d| *bins.entry(i).or_default() -= d);
            }
            if let Some(b) = new {
                self.for_bins(b, |i, d| *bins.entry(i).or_default() += d);
            }
        }
        let mut changes: Vec<(usize, f64)> = bins.into_iter().collect();
        changes.sort_unstable_by_key(|&(i, _)| i);
        let delta = changes
            .iter()
            .map(|&(i, d)| {
                overflow_cost(self.demand[i] + d, self.capacity[i])
                    - overflow_cost(self.demand[i], self.capacity[i])
            })
            .sum();
        (delta, changes)
    }

    /// Record the boxes and bin changes of an accepted move
    pub fn apply(&mut self, boxes: &[(usize, Option<NetBox>)], bins: &[(usize, f64)]) {
        for &(i, d) in bins {
            let before = overflow_cost(self.demand[i], self.capacity[i]);
            self.demand[i] += d;
            self.total += overflow_cost(self.demand[i], self.capacity[i]) - before;
        }
        for &(net, b) in boxes {
            if let Some(slot) = self.boxes.get_mut(net) {
                *slot = b;
            }
        }
    }

    /// Total demand over total capacity
    pub fn mean_load(&self) -> f64 {
        let capacity: f64 = self.capacity.iter().sum();
        if capacity > 0.0 {
            self.demand.iter().sum::<f64>() / capacity
        } else {
            0.0
        }
    }

    /// The busiest bins, as (tile x, tile y, demand, capacity), most
    /// demand over capacity first
    pub fn busiest(&self, n: usize) -> Vec<(u32, u32, f64, f64)> {
        let mut bins: Vec<usize> = (0..self.demand.len())
            .filter(|&i| self.capacity[i] > 0.0)
            .collect();
        let ratio = |i: usize| self.demand[i] / self.capacity[i];
        bins.sort_by(|&a, &b| ratio(b).total_cmp(&ratio(a)).then(a.cmp(&b)));
        bins.truncate(n);
        bins.into_iter()
            .map(|i| {
                let i32 = i as u32;
                (
                    i32 % self.bins_x * BIN,
                    i32 / self.bins_x * BIN,
                    self.demand[i],
                    self.capacity[i],
                )
            })
            .collect()
    }

    fn add_box(&mut self, b: NetBox, sign: f64) {
        let mut add = Vec::new();
        self.for_bins(b, |i, d| add.push((i, d)));
        for (i, d) in add {
            self.demand[i] += sign * d;
        }
    }

    /// Call `f(bin, demand)` for each bin the box covers, with the demand
    /// the net puts on it
    fn for_bins(&self, (x0, y0, x1, y1): NetBox, mut f: impl FnMut(usize, f64)) {
        let (w, h) = ((x1 - x0 + 1) as f64, (y1 - y0 + 1) as f64);
        let per_tile = (w + h) / (w * h);
        for by in y0 / BIN..=(y1 / BIN).min(self.bins_y - 1) {
            let ty = (y1.min(by * BIN + BIN - 1) + 1).saturating_sub(y0.max(by * BIN));
            for bx in x0 / BIN..=(x1 / BIN).min(self.bins_x - 1) {
                let tx = (x1.min(bx * BIN + BIN - 1) + 1).saturating_sub(x0.max(bx * BIN));
                f(
                    (by * self.bins_x + bx) as usize,
                    per_tile * (tx * ty) as f64,
                );
            }
        }
    }
}

/// The tiles a net's placed pins span, or none for a net of fewer than two
/// placed pins or one on dedicated clock routing
pub(crate) fn net_box(net: &GateNet, placement: &PlacementResult) -> Option<NetBox> {
    if net.is_clock {
        return None;
    }
    let mut pins = net
        .driver
        .iter()
        .chain(net.fanout.iter().map(|(c, _)| c))
        .filter_map(|c| placement.placements.get(c));
    let first = pins.next()?;
    let mut b = (first.tile_x, first.tile_y, first.tile_x, first.tile_y);
    let mut n = 1;
    for loc in pins {
        b.0 = b.0.min(loc.tile_x);
        b.1 = b.1.min(loc.tile_y);
        b.2 = b.2.max(loc.tile_x);
        b.3 = b.3.max(loc.tile_y);
        n += 1;
    }
    (n >= 2).then_some(b)
}

/// Cost of a bin's demand beyond its capacity: the square of the excess,
/// relative to the capacity, so a bin twice over costs four times one
/// halfway over and a bin under capacity costs nothing
fn overflow_cost(demand: f64, capacity: f64) -> f64 {
    if capacity <= 0.0 {
        return 0.0;
    }
    let over = (demand - capacity).max(0.0);
    over * over / capacity
}

#[cfg(test)]
mod tests {
    use super::*;

    fn map(bins_x: u32, bins_y: u32, capacity: f64) -> CongestionMap {
        let n = (bins_x * bins_y) as usize;
        CongestionMap {
            bins_x,
            bins_y,
            capacity: vec![capacity; n],
            demand: vec![0.0; n],
            boxes: Vec::new(),
            total: 0.0,
        }
    }

    #[test]
    fn a_net_spreads_its_half_perimeter_over_its_box() {
        let m = map(4, 4, 100.0);
        // 8 x 2 tiles: 10 tile-lengths over 16 tiles, two bins of 8 each
        let mut bins = Vec::new();
        m.for_bins((2, 0, 9, 1), |i, d| bins.push((i, d)));
        let total: f64 = bins.iter().map(|b| b.1).sum();
        assert!((total - 10.0).abs() < 1e-9, "{bins:?}");
        assert_eq!(bins.len(), 3);
        assert!((bins[0].1 - 2.0 * 2.0 * 10.0 / 16.0).abs() < 1e-9);
        assert!((bins[1].1 - 4.0 * 2.0 * 10.0 / 16.0).abs() < 1e-9);
    }

    #[test]
    fn a_net_from_a_constant_adds_no_demand() {
        use crate::device::BelType;
        use crate::placer::PlacementLoc;
        use skalp_lir::gate_netlist::{Cell, CellId, GateNetId};

        let mut netlist = GateNetlist::new("t".to_string(), "nexus".to_string());
        let net = |n: &mut GateNetlist, name: &str| {
            n.add_net(GateNet::new(GateNetId(0), name.to_string()))
        };
        let cell = |n: &mut GateNetlist, ty: &str, name: &str, ins, outs| {
            n.add_cell(Cell::new_comb(
                CellId(0),
                ty.to_string(),
                "nexus".to_string(),
                0.0,
                name.to_string(),
                ins,
                outs,
            ))
        };
        let zero = net(&mut netlist, "zero");
        let a = net(&mut netlist, "a");
        let b = net(&mut netlist, "b");
        cell(&mut netlist, "TIE0_X1", "tie", vec![], vec![zero]);
        let l0 = cell(&mut netlist, "OXIDE_COMB", "l0", vec![zero], vec![a]);
        let l1 = cell(&mut netlist, "OXIDE_COMB", "l1", vec![zero, a], vec![b]);

        // The LUTs sit 12 tiles apart; the constant is not placed
        let mut placement = PlacementResult::new();
        placement
            .placements
            .insert(l0, PlacementLoc::new(0, 0, 0, BelType::Lut4));
        placement
            .placements
            .insert(l1, PlacementLoc::new(12, 0, 0, BelType::Lut4));

        let mut m = map(4, 1, 100.0);
        m.build(&netlist, &placement);
        // Only net a: 13 x 1 tiles, 14 tile-lengths of wire
        let total: f64 = m.demand.iter().sum();
        assert!((total - 14.0).abs() < 1e-9, "{:?}", m.demand);
        assert_eq!(m.boxes[zero.0 as usize], None);
        assert_eq!(m.boxes[a.0 as usize], Some((0, 0, 12, 0)));
    }

    #[test]
    fn only_demand_over_capacity_costs() {
        let mut m = map(2, 1, 4.0);
        m.boxes = vec![None, None];
        // A 1 x 2-tile net in bin 0 asks 1.5 tile-lengths a tile, 3 in all
        let (delta, bins) = m.delta(&[(0, Some((0, 0, 0, 1)))]);
        assert_eq!(delta, 0.0);
        m.apply(&[(0, Some((0, 0, 0, 1)))], &bins);
        assert!((m.demand[0] - 3.0).abs() < 1e-9);
        // A second one takes bin 0 to 6: 2 over a capacity of 4
        let (delta, bins) = m.delta(&[(1, Some((1, 0, 1, 1)))]);
        assert!((delta - 1.0).abs() < 1e-9);
        m.apply(&[(1, Some((1, 0, 1, 1)))], &bins);
        assert!((m.total() - 1.0).abs() < 1e-9);
        // Moving it into bin 1 clears the overflow
        let (delta, _) = m.delta(&[(1, Some((4, 0, 4, 1)))]);
        assert!((delta + 1.0).abs() < 1e-9);
    }
}
