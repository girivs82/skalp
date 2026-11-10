//! Cell spreading for analytical placement.
//!
//! A quadratic wirelength solve pulls every cell towards the middle of the
//! device: on its own it puts thousands of cells on top of each other, and
//! handing out the nearest free site to each of them in turn scatters a
//! cluster over rings of tiles in no useful order. Spreading moves the solved
//! positions to a distribution the device can hold, keeping the cells'
//! relative order, so the next solve (anchored towards the spread positions,
//! as in SimPL) and the final site assignment both start from something legal.
//!
//! Spreading is by recursive bisection: a region's cells are sorted along its
//! longer side and split at the share of the region's capacity that lies on
//! each side of the cut, down to single tiles. Capacity is counted per
//! resource ([`Resource`]) from the device's BELs, so the same code serves any
//! family: a cell only competes with cells that need the same kind of BEL.

use super::sites::fill_cap;
use super::BelType;
use crate::collections::{HashMap, HashMapExt};
use crate::device::{Device, TileType};

/// A kind of placement resource a tile provides and cells use up.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum Resource {
    /// LUTs (carry cells and LUT RAM that are built from LUTs use these too)
    Lut,
    /// Flip-flops
    Ff,
    /// Any other BEL type, counted on its own
    Other(BelType),
}

impl Resource {
    fn of(bel_type: BelType) -> Self {
        if bel_type.is_lut() {
            Resource::Lut
        } else if bel_type.is_ff() {
            Resource::Ff
        } else {
            Resource::Other(bel_type)
        }
    }
}

/// How much of each resource a cell of some BEL type uses.
pub(crate) type Demand = Vec<(Resource, f64)>;

/// Per-resource capacity of every tile, and what each BEL type uses up.
pub(crate) struct CapacityMap {
    width: u32,
    height: u32,
    /// Per resource: capacity of each tile (row-major), as a 2-D prefix sum
    /// with one extra row and column of zeros
    prefix: HashMap<Resource, Vec<f64>>,
    /// Per BEL type a cell may need: the resources one such cell uses
    demand: HashMap<BelType, Demand>,
}

impl CapacityMap {
    /// Count the resources of every tile of `device`, each tile's share
    /// capped by `fill_limit` the way [`super::sites::SiteMap`] caps it.
    pub fn new<D: Device + ?Sized>(device: &D, fill_limit: f64) -> Self {
        let (width, height) = device.grid_size();
        // A carry cell whose slot is made of LUTs (Nexus CCU2: two LUTs per
        // carry) uses those LUTs; one with its own BEL (iCE40) uses that.
        let carry_luts = device
            .carry_chain_layout()
            .and_then(|l| l.slots.first().map(|s| s.lut_bels.len()))
            .filter(|&n| n > 0);
        let mut demand: HashMap<BelType, Demand> = HashMap::new();
        let mut capacity: HashMap<Resource, Vec<f64>> = HashMap::new();
        for y in 0..height {
            for x in 0..width {
                let Some(tile) = device.tile_at(x, y) else {
                    continue;
                };
                let bels = tile.bels();
                let mut counts: HashMap<Resource, usize> = HashMap::new();
                for (i, bel) in bels.iter().enumerate() {
                    let footprint = device.bel_footprint(x, y, i);
                    let shares_luts = (bel.bel_type == BelType::Carry && carry_luts.is_some())
                        || footprint
                            .iter()
                            .any(|&b| bels.get(b).is_some_and(|b| b.bel_type.is_lut()));
                    if !shares_luts {
                        *counts.entry(Resource::of(bel.bel_type)).or_default() += 1;
                    }
                    demand.entry(bel.bel_type).or_insert_with(|| {
                        if bel.bel_type == BelType::Carry {
                            if let Some(n) = carry_luts {
                                return vec![(Resource::Lut, n as f64)];
                            }
                        }
                        let mut uses: HashMap<Resource, f64> = HashMap::new();
                        if !shares_luts {
                            *uses.entry(Resource::of(bel.bel_type)).or_default() += 1.0;
                        }
                        for b in footprint.iter().filter_map(|&b| bels.get(b)) {
                            *uses.entry(Resource::of(b.bel_type)).or_default() += 1.0;
                        }
                        let mut uses: Demand = uses.into_iter().collect();
                        uses.sort_by_key(|(r, _)| format!("{r:?}"));
                        uses
                    });
                }
                // I/O and other special tiles take their cells from fixed
                // sites; only logic, memory and DSP tiles hold spread cells.
                if matches!(
                    tile.tile_type(),
                    TileType::IoTop | TileType::IoBottom | TileType::IoLeft | TileType::IoRight
                ) {
                    continue;
                }
                for (resource, n) in counts {
                    let cap = capacity
                        .entry(resource)
                        .or_insert_with(|| vec![0.0; (width * height) as usize]);
                    cap[(y * width + x) as usize] += fill_cap(n, fill_limit) as f64;
                }
            }
        }
        let prefix = capacity
            .into_iter()
            .map(|(r, cap)| (r, prefix_sum(&cap, width, height)))
            .collect();
        Self {
            width,
            height,
            prefix,
            demand,
        }
    }

    /// The resources a cell needing `bel_type` uses (one of its own kind when
    /// the device has no such BEL)
    pub fn demand(&self, bel_type: BelType) -> Demand {
        // Any flip-flop BEL can hold any flip-flop cell, so a type the
        // device lacks still counts against the flip-flops it can use.
        self.demand
            .get(&bel_type)
            .cloned()
            .unwrap_or_else(|| vec![(Resource::of(bel_type), 1.0)])
    }

    /// Capacity for `resource` in tiles `x0..x1` × `y0..y1`
    fn capacity(&self, resource: Resource, (x0, y0, x1, y1): Region) -> f64 {
        let Some(p) = self.prefix.get(&resource) else {
            return 0.0;
        };
        let w = self.width as usize + 1;
        let at = |x: u32, y: u32| p[y as usize * w + x as usize];
        at(x1, y1) - at(x0, y1) - at(x1, y0) + at(x0, y0)
    }

    fn whole(&self) -> Region {
        (0, 0, self.width, self.height)
    }
}

fn prefix_sum(cap: &[f64], width: u32, height: u32) -> Vec<f64> {
    let w = width as usize + 1;
    let mut p = vec![0.0; w * (height as usize + 1)];
    for y in 0..height as usize {
        for x in 0..width as usize {
            p[(y + 1) * w + x + 1] =
                cap[y * width as usize + x] + p[y * w + x + 1] + p[(y + 1) * w + x] - p[y * w + x];
        }
    }
    p
}

/// Tiles `x0..x1` × `y0..y1`
type Region = (u32, u32, u32, u32);

/// A cell to spread: its index into the position arrays and the resources it
/// uses (its main resource first)
pub(crate) struct SpreadCell {
    pub index: usize,
    pub demand: Demand,
}

/// Spread `cells` (positions in `x`, `y`) over the device so no tile is asked
/// for more of a resource than it has, keeping the cells' relative order.
/// Returns the spread position of every cell in `cells`, in the same order.
///
/// The cells of each resource are spread on their own, all within one region
/// around the cells' centre that is grown until it holds every resource the
/// design needs at `density` of its capacity: a design then stays compact
/// instead of being stretched over the whole device.
pub(crate) fn spread(
    capacity: &CapacityMap,
    cells: &[SpreadCell],
    x: &[f64],
    y: &[f64],
    density: f64,
) -> Vec<(f64, f64)> {
    let mut out: Vec<(f64, f64)> = cells.iter().map(|c| (x[c.index], y[c.index])).collect();
    if cells.is_empty() {
        return out;
    }

    // Group cells by their main resource
    let mut groups: HashMap<Resource, Vec<usize>> = HashMap::new();
    let mut needed: HashMap<Resource, f64> = HashMap::new();
    for (i, cell) in cells.iter().enumerate() {
        if let Some(&(main, _)) = cell.demand.first() {
            groups.entry(main).or_default().push(i);
        }
        for &(r, amount) in &cell.demand {
            *needed.entry(r).or_default() += amount;
        }
    }

    let root = root_region(capacity, cells, x, y, &needed, density);
    let mut resources: Vec<Resource> = groups.keys().copied().collect();
    resources.sort_by_key(|r| format!("{r:?}"));
    for resource in resources {
        let members = &groups[&resource];
        let weight = |i: usize| {
            cells[i]
                .demand
                .iter()
                .find(|(r, _)| *r == resource)
                .map_or(1.0, |&(_, a)| a)
        };
        let demand: f64 = members.iter().map(|&i| weight(i)).sum();
        // A resource the root region lacks (RAM or DSP columns elsewhere)
        // spreads over the whole device
        let region = if capacity.capacity(resource, root) * density >= demand {
            root
        } else {
            capacity.whole()
        };
        if capacity.capacity(resource, region) <= 0.0 {
            continue; // nothing on the device holds it: leave as solved
        }
        let mut items: Vec<(usize, f64, f64, f64)> = members
            .iter()
            .map(|&i| (i, out[i].0, out[i].1, weight(i)))
            .collect();
        bisect(capacity, resource, region, &mut items, &mut out);
    }
    out
}

/// The smallest region around the cells' centre (grown a tile at a time on
/// every side) that holds `needed` of each resource at `density`.
fn root_region(
    capacity: &CapacityMap,
    cells: &[SpreadCell],
    x: &[f64],
    y: &[f64],
    needed: &HashMap<Resource, f64>,
    density: f64,
) -> Region {
    let n = cells.len() as f64;
    let cx = cells.iter().map(|c| x[c.index]).sum::<f64>() / n;
    let cy = cells.iter().map(|c| y[c.index]).sum::<f64>() / n;
    let (w, h) = (capacity.width, capacity.height);
    let cx = (cx.max(0.0) as u32).min(w.saturating_sub(1));
    let cy = (cy.max(0.0) as u32).min(h.saturating_sub(1));
    let fits = |region: Region| {
        needed.iter().all(|(&r, &amount)| {
            capacity.capacity(r, capacity.whole()) * density < amount
                || capacity.capacity(r, region) * density >= amount
        })
    };
    let mut r = 0;
    loop {
        let region = (
            cx.saturating_sub(r),
            cy.saturating_sub(r),
            (cx + r + 1).min(w),
            (cy + r + 1).min(h),
        );
        if fits(region) || region == capacity.whole() {
            return region;
        }
        r += 1;
    }
}

/// Split `items` (cell, x, y, weight) over `region` by recursive bisection,
/// writing each cell's final position to `out`.
fn bisect(
    capacity: &CapacityMap,
    resource: Resource,
    region: Region,
    items: &mut [(usize, f64, f64, f64)],
    out: &mut [(f64, f64)],
) {
    if items.is_empty() {
        return;
    }
    let (x0, y0, x1, y1) = region;
    let (w, h) = (x1 - x0, y1 - y0);
    if w <= 1 && h <= 1 {
        // One tile: keep the cells' order within it, spread over its area
        let n = items.len() as f64;
        for (k, item) in items.iter().enumerate() {
            let f = (k as f64 + 0.5) / n;
            out[item.0] = (x0 as f64 + 0.25 + 0.5 * f, y0 as f64 + 0.5);
        }
        return;
    }
    let vertical_cut = w >= h;
    let (low, high) = if vertical_cut {
        let mid = x0 + w / 2;
        ((x0, y0, mid, y1), (mid, y0, x1, y1))
    } else {
        let mid = y0 + h / 2;
        ((x0, y0, x1, mid), (x0, mid, x1, y1))
    };
    let cap_low = capacity.capacity(resource, low);
    let cap_high = capacity.capacity(resource, high);
    if cap_low <= 0.0 {
        return bisect(capacity, resource, high, items, out);
    }
    if cap_high <= 0.0 {
        return bisect(capacity, resource, low, items, out);
    }
    let key = |item: &(usize, f64, f64, f64)| if vertical_cut { item.1 } else { item.2 };
    items.sort_by(|a, b| key(a).total_cmp(&key(b)).then(a.0.cmp(&b.0)));
    let total: f64 = items.iter().map(|i| i.3).sum();
    let target = total * cap_low / (cap_low + cap_high);
    let mut split = 0;
    let mut acc = 0.0;
    while split < items.len() && acc + items[split].3 / 2.0 <= target {
        acc += items[split].3;
        split += 1;
    }
    let (a, b) = items.split_at_mut(split);
    bisect(capacity, resource, low, a, out);
    bisect(capacity, resource, high, b, out);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::device::ice40::{Ice40Device, Ice40Variant};

    #[test]
    fn spreading_stays_within_capacity_and_keeps_order() {
        let device = Ice40Device::new(Ice40Variant::Hx1k);
        let capacity = CapacityMap::new(&device, 1.0);
        let (w, h) = device.grid_size();
        // 400 LUT cells all solved onto the middle of the device, in a line
        let n = 400;
        let x: Vec<f64> = (0..n).map(|i| w as f64 / 2.0 + i as f64 * 1e-3).collect();
        let y = vec![h as f64 / 2.0; n];
        let cells: Vec<SpreadCell> = (0..n)
            .map(|index| SpreadCell {
                index,
                demand: capacity.demand(BelType::Lut4),
            })
            .collect();
        let spread_pos = spread(&capacity, &cells, &x, &y, 1.0);

        let mut per_tile: HashMap<(u32, u32), usize> = HashMap::new();
        for &(px, py) in &spread_pos {
            *per_tile.entry((px as u32, py as u32)).or_default() += 1;
        }
        for (&(tx, ty), &count) in &per_tile {
            let cap = capacity.capacity(Resource::Lut, (tx, ty, tx + 1, ty + 1));
            assert!(
                count as f64 <= cap,
                "tile ({tx}, {ty}) holds {count} > {cap}"
            );
        }
        // 400 LUTs at 8 per tile need 50 tiles: spreading fills the
        // smallest square around the cells that holds them (8 × 8 logic
        // tiles, one more where it crosses a RAM column) evenly, instead of
        // stretching them over the device
        let xs = per_tile.keys().map(|t| t.0);
        let ys = per_tile.keys().map(|t| t.1);
        let span_x = xs.clone().max().unwrap() - xs.min().unwrap() + 1;
        let span_y = ys.clone().max().unwrap() - ys.min().unwrap() + 1;
        assert!(per_tile.len() >= 50 && per_tile.len() <= 64);
        assert!(
            span_x <= 9 && span_y <= 9,
            "spread over {span_x} × {span_y} tiles"
        );
        // Order along x is kept: cells further right in the solve stay right
        assert!(spread_pos[0].0 <= spread_pos[n - 1].0);
    }
}
