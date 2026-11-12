//! Routing congestion estimated from a placement, for placement to avoid.
//!
//! Each net is assumed to need its wire spread evenly over its bounding box
//! (RUDY: a net of length `L` over `w` by `h` tiles asks `L / (w * h)`
//! tile-lengths of wire of every tile in its box). A net of a few pins needs
//! its half-perimeter `w + h`; one of many pins needs a tree reaching each of
//! them, much longer than that ([`net_length`]). The demand is summed per
//! bin of tiles and set against the routing the device has there: the
//! general routing wires that start in the bin's tiles. A bin asked for more
//! than its share is congested, and the router will struggle there, however
//! short the wiring is overall.
//!
//! A bin's overflow matters less when there is free routing next to it, for
//! the router to detour through, and more when the free routing is far away.
//! So the cost also looks at the blocks of bins around each bin, out to
//! [`REGION_RADII`]: demand a block can't hold must leave it, and the larger
//! the full block, the longer the detour it costs.

use crate::collections::{HashMap, HashMapExt};
use crate::device::Device;
use crate::placer::{is_routed_net, PlacementResult};
use skalp_lir::gate_netlist::{GateNet, GateNetlist};
use std::cell::RefCell;

/// Tiles per side of a bin
const BIN: u32 = 4;

/// Wire demand, in tile-lengths, that one general routing wire of a tile
/// can carry before the router has trouble there. Calibrated on Nexus
/// (LFCPNX-100): a 7K-cell core
/// whose busiest 8x8 blocks ask for about 90 tile-lengths a tile routes in a
/// handful of passes (busiest bins at 0.96 of this capacity), a 52K-cell one
/// whose busiest blocks ask for 130 to 150 (1.3 of it) needs dozens.
/// (Calibrated with half-perimeter demand. With [`net_length`] the same
/// placements ask about a fifth more; the 7K-cell core's busiest bins sit
/// at about 0.93 after annealing and it routes as before.)
pub(crate) const DEMAND_PER_WIRE: f64 = 1.7;

/// Share of its capacity a block of bins can take before it counts as
/// congested. A full bin among free ones is fine: the router detours
/// through its neighbours. A whole block at capacity leaves it nowhere to
/// go, and PathFinder stalls with a handful of wires still shared (a
/// 48K-cell core on LFCPNX-100 whose annealing left a 3 x 6-bin block at
/// 1.0 of [`DEMAND_PER_WIRE`] did not converge; at 0.88 it routes in 31
/// passes).
const REGION_LOAD: f64 = 0.88;

/// Sizes of the blocks around each bin, in bins on each side of it
const REGION_RADII: [i64; 3] = [1, 2, 3];

/// Weight of the overflow cost of a block of radius `r`. Demand a block
/// can't hold travels about `r` bins further than demand that only leaves
/// its bin, and every bin is in `(2r + 1)^2` such blocks, so a uniform
/// overload costs about `r` times what it costs bin by bin.
fn region_weight(r: i64) -> f64 {
    r as f64 / ((2 * r + 1) * (2 * r + 1)) as f64
}

thread_local! {
    /// Working space for [`CongestionMap::delta`], one per annealing thread
    static SCRATCH: RefCell<RegionScratch> = RefCell::default();
}

/// Per-block demand changes of a move being priced
#[derive(Default)]
struct RegionScratch {
    /// Change of each block (zero for those not touched)
    delta: Vec<f64>,
    /// Blocks with a change, in the order first touched
    touched: Vec<usize>,
}

/// Tiles `x0..=x1` × `y0..=y1` a net's pins span, and how many pins
pub(crate) type NetBox = (u32, u32, u32, u32, u32);

/// Length of a rectilinear Steiner tree through `n` points spread over an
/// area `A`, as a multiple of `sqrt(n * A)` (for many random points the
/// tree tends to about 0.7 to 0.8 of that)
const STEINER_FACTOR: f64 = 0.75;

/// Wire, in tile-lengths, a net of `pins` pins over `w` by `h` tiles needs:
/// its half-perimeter, or for a net of many pins spread over its box, the
/// tree that reaches them all. A net driving 279 pins across 80 x 14 tiles
/// needs about 400, not 94.
fn net_length(w: f64, h: f64, pins: u32) -> f64 {
    (w + h).max(STEINER_FACTOR * (pins as f64 * w * h).sqrt())
}

/// Per-bin wire demand of a placement and the device's supply
pub(crate) struct CongestionMap {
    bins_x: u32,
    bins_y: u32,
    /// Demand a bin can take before it counts as congested (0: no routing)
    capacity: Vec<f64>,
    /// Current demand of each bin
    demand: Vec<f64>,
    /// Demand each block can take before it counts as congested, indexed
    /// by radius (in [`REGION_RADII`] order), then middle bin
    region_capacity: Vec<f64>,
    /// Current demand of each block, indexed as `region_capacity`
    region_demand: Vec<f64>,
    /// Each net's box, or none for a net that adds no demand
    boxes: Vec<Option<NetBox>>,
    /// Sum over bins of [`overflow_cost`], plus the weighted sum over
    /// blocks
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
        any.then(|| Self::with_capacity(bins_x, bins_y, capacity))
    }

    fn with_capacity(bins_x: u32, bins_y: u32, capacity: Vec<f64>) -> Self {
        let mut map = Self {
            bins_x,
            bins_y,
            demand: vec![0.0; capacity.len()],
            region_capacity: vec![0.0; capacity.len() * REGION_RADII.len()],
            region_demand: vec![0.0; capacity.len() * REGION_RADII.len()],
            capacity,
            boxes: Vec::new(),
            total: 0.0,
        };
        let mut region_capacity = std::mem::take(&mut map.region_capacity);
        for i in 0..map.capacity.len() {
            let c = map.capacity[i] * REGION_LOAD;
            map.for_regions(i, |r| region_capacity[r] += c);
        }
        map.region_capacity = region_capacity;
        map
    }

    /// Call `f(block)` for each block bin `i` is in: for each radius `r`,
    /// those whose middle bins are within `r` of it
    fn for_regions(&self, i: usize, mut f: impl FnMut(usize)) {
        let (w, h) = (self.bins_x as usize, self.bins_y as usize);
        let (bx, by) = (i % w, i / w);
        for (k, &r) in REGION_RADII.iter().enumerate() {
            let r = r as usize;
            let base = k * w * h;
            for y in by.saturating_sub(r)..=(by + r).min(h - 1) {
                let row = base + y * w;
                for x in bx.saturating_sub(r)..=(bx + r).min(w - 1) {
                    f(row + x);
                }
            }
        }
    }

    /// Cost of bin `i` at demand `d`
    fn bin_cost(&self, i: usize, d: f64) -> f64 {
        overflow_cost(d, self.capacity[i])
    }

    /// Cost of block `r` at demand `d`
    fn region_cost(&self, r: usize, d: f64) -> f64 {
        let k = r / self.capacity.len();
        region_weight(REGION_RADII[k]) * overflow_cost(d, self.region_capacity[r])
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
        let mut region_demand = vec![0.0; self.region_demand.len()];
        for i in 0..self.demand.len() {
            let d = self.demand[i];
            self.for_regions(i, |r| region_demand[r] += d);
        }
        self.region_demand = region_demand;
        self.total = (0..self.demand.len())
            .map(|i| self.bin_cost(i, self.demand[i]))
            .sum::<f64>()
            + (0..self.region_demand.len())
                .map(|r| self.region_cost(r, self.region_demand[r]))
                .sum::<f64>();
    }

    /// Set the demand from nets given as the cells on each (`pins`, cell
    /// indices) at continuous positions `x`, `y` (in tiles), as global
    /// placement has them
    pub fn build_from_positions(&mut self, pins: &[Vec<usize>], x: &[f64], y: &[f64]) {
        let (w, h) = (self.bins_x * BIN, self.bins_y * BIN);
        let tile = |v: f64, n: u32| (v.max(0.0) as u32).min(n - 1);
        self.demand.iter_mut().for_each(|d| *d = 0.0);
        self.boxes.clear();
        for net in pins.iter().filter(|p| p.len() >= 2) {
            let (mut x0, mut y0, mut x1, mut y1) = (u32::MAX, u32::MAX, 0, 0);
            for &i in net {
                let (tx, ty) = (tile(x[i], w), tile(y[i], h));
                (x0, y0, x1, y1) = (x0.min(tx), y0.min(ty), x1.max(tx), y1.max(ty));
            }
            self.add_box((x0, y0, x1, y1, net.len() as u32), 1.0);
        }
        let mut region_demand = vec![0.0; self.region_demand.len()];
        for i in 0..self.demand.len() {
            let d = self.demand[i];
            self.for_regions(i, |r| region_demand[r] += d);
        }
        self.region_demand = region_demand;
    }

    /// How far over its limit the routing at tile `(x, y)` is asked to
    /// go: the largest demand over capacity of its bin and of each block
    /// around it (blocks against [`REGION_LOAD`] of theirs), so a bin among
    /// free ones is not pressed but one inside a full block is
    pub fn pressure(&self, x: f64, y: f64) -> f64 {
        let bx = ((x.max(0.0) as u32) / BIN).min(self.bins_x - 1);
        let by = ((y.max(0.0) as u32) / BIN).min(self.bins_y - 1);
        let i = (by * self.bins_x + bx) as usize;
        let ratio = |d: f64, c: f64| if c > 0.0 { d / c } else { 0.0 };
        let bin = ratio(self.demand[i], self.capacity[i]);
        // A bin's blocks are those centred on it, one per radius
        let n = self.capacity.len();
        (0..REGION_RADII.len())
            .map(|k| {
                ratio(
                    self.region_demand[k * n + i],
                    self.region_capacity[k * n + i],
                )
            })
            .fold(bin, f64::max)
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
        let bins_delta: f64 = changes
            .iter()
            .map(|&(i, d)| self.bin_cost(i, self.demand[i] + d) - self.bin_cost(i, self.demand[i]))
            .sum();
        // Each block's change, gathered in a dense array (this runs for
        // every move the annealer tries), then summed in the order the
        // blocks were first touched, which depends only on `changes`
        let regions_delta = SCRATCH.with_borrow_mut(|scratch| {
            let RegionScratch { delta, touched } = scratch;
            if delta.len() < self.region_demand.len() {
                delta.resize(self.region_demand.len(), 0.0);
            }
            for &(i, d) in &changes {
                self.for_regions(i, |r| {
                    if delta[r] == 0.0 {
                        touched.push(r);
                    }
                    delta[r] += d;
                });
            }
            let mut regions_delta = 0.0;
            for &r in touched.iter() {
                let d = std::mem::take(&mut delta[r]);
                if d != 0.0 {
                    regions_delta += self.region_cost(r, self.region_demand[r] + d)
                        - self.region_cost(r, self.region_demand[r]);
                }
            }
            touched.clear();
            regions_delta
        });
        (bins_delta + regions_delta, changes)
    }

    /// Record the boxes and bin changes of an accepted move
    pub fn apply(&mut self, boxes: &[(usize, Option<NetBox>)], bins: &[(usize, f64)]) {
        for &(i, d) in bins {
            let before = self.bin_cost(i, self.demand[i]);
            self.demand[i] += d;
            self.total += self.bin_cost(i, self.demand[i]) - before;
            let mut region_demand = std::mem::take(&mut self.region_demand);
            let mut total = self.total;
            self.for_regions(i, |r| {
                let before = self.region_cost(r, region_demand[r]);
                region_demand[r] += d;
                total += self.region_cost(r, region_demand[r]) - before;
            });
            self.region_demand = region_demand;
            self.total = total;
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
    fn for_bins(&self, (x0, y0, x1, y1, pins): NetBox, mut f: impl FnMut(usize, f64)) {
        let (w, h) = ((x1 - x0 + 1) as f64, (y1 - y0 + 1) as f64);
        let per_tile = net_length(w, h, pins) / (w * h);
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
    let mut b = (first.tile_x, first.tile_y, first.tile_x, first.tile_y, 1);
    let mut n = 1;
    for loc in pins {
        b.0 = b.0.min(loc.tile_x);
        b.1 = b.1.min(loc.tile_y);
        b.2 = b.2.max(loc.tile_x);
        b.3 = b.3.max(loc.tile_y);
        n += 1;
    }
    b.4 = n;
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
        CongestionMap::with_capacity(bins_x, bins_y, vec![capacity; (bins_x * bins_y) as usize])
    }

    #[test]
    fn a_net_spreads_its_half_perimeter_over_its_box() {
        let m = map(4, 4, 100.0);
        // 8 x 2 tiles: 10 tile-lengths over 16 tiles, two bins of 8 each
        let mut bins = Vec::new();
        m.for_bins((2, 0, 9, 1, 2), |i, d| bins.push((i, d)));
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
        assert_eq!(m.boxes[a.0 as usize], Some((0, 0, 12, 0, 2)));
    }

    #[test]
    fn only_demand_over_capacity_costs() {
        let mut m = map(2, 1, 4.0);
        m.boxes = vec![None, None];
        // A 1 x 2-tile net in bin 0 asks 1.5 tile-lengths a tile, 3 in all
        let (delta, bins) = m.delta(&[(0, Some((0, 0, 0, 1, 2)))]);
        assert_eq!(delta, 0.0);
        m.apply(&[(0, Some((0, 0, 0, 1, 2)))], &bins);
        assert!((m.demand[0] - 3.0).abs() < 1e-9);
        // A second one takes bin 0 to 6: 2 over a capacity of 4
        let (delta, bins) = m.delta(&[(1, Some((1, 0, 1, 1, 2)))]);
        assert!((delta - 1.0).abs() < 1e-9);
        m.apply(&[(1, Some((1, 0, 1, 1, 2)))], &bins);
        assert!((m.total() - 1.0).abs() < 1e-9);
        // Moving it into bin 1 clears the overflow
        let (delta, _) = m.delta(&[(1, Some((4, 0, 4, 1, 2)))]);
        assert!((delta + 1.0).abs() < 1e-9);
    }

    #[test]
    fn a_full_block_costs_but_one_full_bin_does_not() {
        // One bin at capacity among empty neighbours costs nothing
        let mut m = map(3, 3, 10.0);
        m.demand[4] = 10.0;
        m.region_demand.iter_mut().for_each(|d| *d = 10.0);
        let (delta, _) = m.delta(&[]);
        assert_eq!(delta, 0.0);
        assert_eq!(m.region_cost(4, m.region_demand[4]), 0.0);
        // A bin over capacity costs less with free neighbours than with full
        // ones. A 4 x 4-tile net in one bin asks 8 tile-lengths; two of
        // them take the middle bin of a 5 x 5 map to 16 of 10.
        let middle = |i: usize| (i, Some((8, 8, 11, 11, 2)));
        let second = |m: &mut CongestionMap| {
            let (_, bins) = m.delta(&[middle(0)]);
            m.apply(&[middle(0)], &bins);
            m.delta(&[middle(1)]).0
        };
        let mut lone = map(5, 5, 10.0);
        lone.boxes = vec![None; 2];
        let lone = second(&mut lone);
        // Its own overflow, 6 over 10, and nothing for the blocks around it
        assert!((lone - 3.6).abs() < 1e-9, "{lone}");
        let mut crowded = map(5, 5, 10.0);
        crowded.boxes = vec![None; 27];
        for b in (0..25u32).filter(|&b| b != 12) {
            let (x, y) = (b % 5 * BIN, b / 5 * BIN);
            let net = (b as usize + 2, Some((x, y, x + 3, y + 3, 2)));
            let (_, bins) = crowded.delta(&[net]);
            crowded.apply(&[net], &bins);
        }
        let crowded = second(&mut crowded);
        assert!(crowded > lone, "{crowded} vs {lone}");

        // Every bin at 0.95 of capacity: no bin is over, the middle block is
        let mut m = map(3, 3, 10.0);
        m.boxes = vec![None; 9];
        for i in 0..9u32 {
            // A 1 x 4-tile net in the middle of bin i asks 5 tile-lengths;
            // two of them 10 in all, then trim to 9.5
            let (x, y) = (i % 3 * BIN, i / 3 * BIN);
            let (_, bins) = m.delta(&[(i as usize, Some((x, y, x, y + 3, 2)))]);
            m.apply(&[(i as usize, Some((x, y, x, y + 3, 2)))], &bins);
        }
        let per_bin = m.demand[0];
        let scale = 9.5 / per_bin;
        let bins: Vec<(usize, f64)> = (0..9).map(|i| (i, m.demand[i] * (scale - 1.0))).collect();
        m.apply(&[], &bins);
        assert!(m.demand.iter().all(|&d| (d - 9.5).abs() < 1e-9));
        // Middle 3 x 3 block: 85.5 against 0.88 of 90
        let over = 85.5 - 90.0 * REGION_LOAD;
        let expected = region_weight(1) * over * over / (90.0 * REGION_LOAD);
        assert!(m.total() >= expected - 1e-9, "{}", m.total());
        assert!((m.region_cost(4, m.region_demand[4]) - expected).abs() < 1e-9);
    }

    #[test]
    fn a_net_of_many_pins_needs_more_than_its_half_perimeter() {
        // Two or three pins: the half-perimeter
        assert_eq!(net_length(4.0, 4.0, 2), 8.0);
        assert_eq!(net_length(10.0, 1.0, 3), 11.0);
        // 280 pins over 80 x 14 tiles: a tree of about 0.75 sqrt(n A)
        let l = net_length(80.0, 14.0, 280);
        assert!((l - 0.75 * (280.0f64 * 80.0 * 14.0).sqrt()).abs() < 1e-9);
        assert!(l > 4.0 * 94.0, "{l}");
    }
}
