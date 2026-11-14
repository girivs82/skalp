//! Free-site bookkeeping for placement.
//!
//! [`SiteMap`] answers "nearest free BEL that can hold this cell" in time
//! proportional to the tiles actually inspected: it walks square rings
//! outward from the target (each tile visited once), and skips full tiles and
//! tiles without a compatible BEL in O(1). The previous search rescanned the
//! whole square at every radius and probed a hash map per BEL, which made
//! initial placement cubic in the filled area — on a 15K-cell Nexus design it
//! did not finish within an hour.
//!
//! A fill limit ([`SiteMap::with_fill_limit`]) caps how many BELs of each
//! type a tile hands out, so a design spreads over more tiles instead of
//! packing every tile full: a full tile needs more signals in and out than
//! its local routing can carry once its neighbours are full too.

use super::control_sets::ControlSet;
use super::BelType;
use crate::collections::{HashMap, HashMapExt};
use crate::device::Device;

/// Whether a BEL of type `available` can implement a cell needing `required`.
pub fn bel_types_compatible(available: BelType, required: BelType) -> bool {
    match (available, required) {
        // A basic Dff BEL can implement any DFF variant
        // (iCE40 DFFs are all the same hardware with different config bits)
        (BelType::Dff, BelType::DffE)
        | (BelType::Dff, BelType::DffSr)
        | (BelType::Dff, BelType::DffSrE)
        // Any FF type can implement a basic DFF
        | (BelType::DffE, BelType::Dff)
        | (BelType::DffSr, BelType::Dff)
        | (BelType::DffSrE, BelType::Dff)
        | (BelType::DffSrE, BelType::DffE)
        | (BelType::DffSrE, BelType::DffSr) => true,
        _ => available == required,
    }
}

/// One tile's BELs and which of them are taken.
#[derive(Debug, Clone, Default)]
struct TileSites {
    /// (BEL type, taken) per BEL index
    bels: Vec<(BelType, bool)>,
    /// Number of BELs not taken
    free: usize,
    /// Per BEL index, the other BELs a cell there uses up
    /// ([`Device::bel_footprint`]); empty for most tiles
    footprint: Vec<Vec<usize>>,
    /// Per BEL type in the tile: (type, BELs of it, BELs of it taken)
    kinds: Vec<(BelType, usize, usize)>,
    /// Per BEL index, its flip-flop control group
    /// ([`Device::ff_control_group`]); empty when the tile has none
    groups: Vec<Option<usize>>,
}

impl TileSites {
    fn note_taken(&mut self, bel_type: BelType) {
        if let Some(k) = self.kinds.iter_mut().find(|k| k.0 == bel_type) {
            k.2 += 1;
        }
    }

    /// Whether another BEL of `bel_type` may be taken under `fill_limit`
    fn below_limit(&self, bel_type: BelType, fill_limit: f64) -> bool {
        self.kinds
            .iter()
            .find(|k| k.0 == bel_type)
            .is_some_and(|&(_, total, taken)| taken < fill_cap(total, fill_limit))
    }
}

/// Most of `total` BELs of one type a tile may hold under `fill_limit`
/// (at least one, so every tile stays usable)
pub fn fill_cap(total: usize, fill_limit: f64) -> usize {
    ((total as f64 * fill_limit).ceil() as usize).clamp(1.min(total), total)
}

/// Occupancy of every BEL on the device.
#[derive(Debug, Clone)]
pub struct SiteMap {
    width: u32,
    height: u32,
    tiles: Vec<TileSites>,
    /// Share of each tile's BELs of a type that may be taken (1.0: all)
    fill_limit: f64,
    /// Control set of the flip-flops in each (tile index, control group)
    /// holding any
    group_sets: HashMap<(usize, usize), ControlSet>,
}

impl SiteMap {
    /// All BELs of `device`, none taken.
    pub fn new<D: Device + ?Sized>(device: &D) -> Self {
        let (width, height) = device.grid_size();
        let mut tiles = Vec::with_capacity((width * height) as usize);
        for y in 0..height {
            for x in 0..width {
                let bels: Vec<(BelType, bool)> = device
                    .tile_at(x, y)
                    .map(|t| t.bels().iter().map(|b| (b.bel_type, false)).collect())
                    .unwrap_or_default();
                let free = bels.len();
                let footprint: Vec<Vec<usize>> = (0..bels.len())
                    .map(|i| device.bel_footprint(x, y, i))
                    .collect();
                let footprint = if footprint.iter().all(Vec::is_empty) {
                    Vec::new()
                } else {
                    footprint
                };
                let mut kinds: Vec<(BelType, usize, usize)> = Vec::new();
                for &(t, _) in &bels {
                    match kinds.iter_mut().find(|k| k.0 == t) {
                        Some(k) => k.1 += 1,
                        None => kinds.push((t, 1, 0)),
                    }
                }
                let groups: Vec<Option<usize>> = (0..bels.len())
                    .map(|i| device.ff_control_group(x, y, i))
                    .collect();
                let groups = if groups.iter().all(Option::is_none) {
                    Vec::new()
                } else {
                    groups
                };
                tiles.push(TileSites {
                    bels,
                    free,
                    footprint,
                    kinds,
                    groups,
                });
            }
        }
        Self {
            width,
            height,
            tiles,
            fill_limit: 1.0,
            group_sets: HashMap::new(),
        }
    }

    /// Hand out at most `fill_limit` (0..=1) of each tile's BELs of a type
    /// ([`fill_cap`]). BELs taken with [`Self::take`] still count.
    pub fn with_fill_limit(mut self, fill_limit: f64) -> Self {
        self.fill_limit = fill_limit.clamp(0.0, 1.0);
        self
    }

    fn tile_index(&self, x: u32, y: u32) -> usize {
        (y * self.width + x) as usize
    }

    /// Mark a BEL taken, along with the BELs in its footprint (which no
    /// other cell may then use). A flip-flop's control set (`set`) claims its
    /// control group for flip-flops with the same one. Returns false if it was
    /// already taken or does not exist.
    pub fn take(&mut self, x: u32, y: u32, bel_index: usize, set: Option<ControlSet>) -> bool {
        if x >= self.width || y >= self.height {
            return false;
        }
        let idx = self.tile_index(x, y);
        let group = self.tiles[idx].groups.get(bel_index).copied().flatten();
        let tile = &mut self.tiles[idx];
        match tile.bels.get_mut(bel_index) {
            Some((t, taken)) if !*taken => {
                *taken = true;
                let t = *t;
                tile.free -= 1;
                tile.note_taken(t);
            }
            _ => return false,
        }
        if let Some(blocked) = tile.footprint.get(bel_index).cloned() {
            for b in blocked {
                if let Some((t, taken)) = tile.bels.get_mut(b) {
                    if !*taken {
                        *taken = true;
                        let t = *t;
                        tile.free -= 1;
                        tile.note_taken(t);
                    }
                }
            }
        }
        if let (Some(set), Some(group)) = (set, group) {
            self.group_sets.entry((idx, group)).or_insert(set);
        }
        true
    }

    /// Whether a BEL is taken (a missing BEL counts as taken).
    #[cfg(test)]
    pub fn is_taken(&self, x: u32, y: u32, bel_index: usize) -> bool {
        if x >= self.width || y >= self.height {
            return true;
        }
        self.tiles[self.tile_index(x, y)]
            .bels
            .get(bel_index)
            .is_none_or(|&(_, taken)| taken)
    }

    /// First free BEL in tile (x, y) for `required`: an exact type match first,
    /// then any BEL `compatible(available, required)` accepts.
    fn free_bel_in_tile(
        &self,
        x: u32,
        y: u32,
        required: BelType,
        set: Option<&ControlSet>,
        compatible: &impl Fn(BelType, BelType) -> bool,
    ) -> Option<usize> {
        let idx = self.tile_index(x, y);
        let tile = &self.tiles[idx];
        if tile.free == 0 {
            return None;
        }
        // A flip-flop fits a control group holding none or its own control set
        let open = |bel: usize, t: BelType, taken: bool| {
            !taken && tile.below_limit(t, self.fill_limit) && self.group_fits(idx, bel, set)
        };
        tile.bels
            .iter()
            .enumerate()
            .position(|(i, &(t, taken))| t == required && open(i, t, taken))
            .or_else(|| {
                tile.bels
                    .iter()
                    .enumerate()
                    .position(|(i, &(t, taken))| compatible(t, required) && open(i, t, taken))
            })
    }

    /// Whether a flip-flop with control set `set` may join the BELs of
    /// control group `group` of tile `idx`
    fn group_fits(&self, idx: usize, bel: usize, set: Option<&ControlSet>) -> bool {
        match (set, self.tiles[idx].groups.get(bel)) {
            (Some(set), Some(&Some(group))) => self
                .group_sets
                .get(&(idx, group))
                .is_none_or(|held| held == set),
            _ => true,
        }
    }

    /// The free BELs of tile (x, y) that can hold `required` (with control
    /// set `set`, for a flip-flop) under the fill limit
    pub fn open_bels(
        &self,
        x: u32,
        y: u32,
        required: BelType,
        set: Option<&ControlSet>,
        compatible: impl Fn(BelType, BelType) -> bool,
    ) -> Vec<usize> {
        if x >= self.width || y >= self.height {
            return Vec::new();
        }
        let idx = self.tile_index(x, y);
        let tile = &self.tiles[idx];
        tile.bels
            .iter()
            .enumerate()
            .filter(|&(i, &(t, taken))| {
                !taken
                    && compatible(t, required)
                    && tile.below_limit(t, self.fill_limit)
                    && self.group_fits(idx, i, set)
            })
            .map(|(i, _)| i)
            .collect()
    }

    /// Take a BEL a cell held before, if it is still free and its control
    /// group still takes `set` (the fill limit does not apply: the cell was
    /// there already)
    pub fn retake(&mut self, x: u32, y: u32, bel_index: usize, set: Option<ControlSet>) -> bool {
        if x >= self.width || y >= self.height {
            return false;
        }
        let idx = self.tile_index(x, y);
        let free = self.tiles[idx]
            .bels
            .get(bel_index)
            .is_some_and(|&(_, taken)| !taken);
        free && self.group_fits(idx, bel_index, set.as_ref()) && self.take(x, y, bel_index, set)
    }

    /// Take the free BEL nearest to (x, y) (Chebyshev rings, nearest tile first
    /// within a ring) that can hold `required`. Returns (tile_x, tile_y, bel_index).
    /// `set`: the cell's control set, when it is a flip-flop.
    pub fn take_nearest(
        &mut self,
        x: f64,
        y: f64,
        required: BelType,
        set: Option<ControlSet>,
        compatible: impl Fn(BelType, BelType) -> bool,
    ) -> Option<(u32, u32, usize)> {
        let found = self.find_nearest(x, y, required, set.as_ref(), &compatible)?;
        self.take(found.0, found.1, found.2, set);
        Some(found)
    }

    /// The free BEL nearest to (x, y) that can hold `required`, without taking it.
    pub fn find_nearest(
        &self,
        x: f64,
        y: f64,
        required: BelType,
        set: Option<&ControlSet>,
        compatible: &impl Fn(BelType, BelType) -> bool,
    ) -> Option<(u32, u32, usize)> {
        if self.width == 0 || self.height == 0 {
            return None;
        }
        let cx = (x.max(0.0) as u32).min(self.width - 1) as i64;
        let cy = (y.max(0.0) as u32).min(self.height - 1) as i64;
        let max_radius = self.width.max(self.height) as i64;
        let (w, h) = (self.width as i64, self.height as i64);

        for r in 0..=max_radius {
            // Ring at Chebyshev distance r; keep the candidate nearest in
            // Euclidean distance so the result does not depend on scan order.
            let mut best: Option<(i64, u32, u32, usize)> = None;
            let consider = |tx: i64, ty: i64, best: &mut Option<(i64, u32, u32, usize)>| {
                if tx < 0 || ty < 0 || tx >= w || ty >= h {
                    return;
                }
                let d2 = (tx - cx) * (tx - cx) + (ty - cy) * (ty - cy);
                if best.is_some_and(|b| b.0 <= d2) {
                    return;
                }
                if let Some(bel) =
                    self.free_bel_in_tile(tx as u32, ty as u32, required, set, compatible)
                {
                    *best = Some((d2, tx as u32, ty as u32, bel));
                }
            };
            if r == 0 {
                consider(cx, cy, &mut best);
            } else {
                for dx in -r..=r {
                    consider(cx + dx, cy - r, &mut best);
                    consider(cx + dx, cy + r, &mut best);
                }
                for dy in (-r + 1)..r {
                    consider(cx - r, cy + dy, &mut best);
                    consider(cx + r, cy + dy, &mut best);
                }
            }
            if let Some((_, tx, ty, bel)) = best {
                return Some((tx, ty, bel));
            }
            // Past every edge of the device: nothing left to search.
            if cx - r <= 0 && cy - r <= 0 && cx + r >= w - 1 && cy + r >= h - 1 {
                break;
            }
        }
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::device::ice40::{Ice40Device, Ice40Variant};

    #[test]
    fn nearest_free_site_fills_outward_and_reports_exhaustion() {
        let device = Ice40Device::new(Ice40Variant::Hx1k);
        let mut sites = SiteMap::new(&device);
        let (w, h) = device.grid_size();
        let total_luts: usize = (0..h)
            .flat_map(|y| (0..w).map(move |x| (x, y)))
            .filter_map(|(x, y)| device.tile_at(x, y))
            .map(|t| {
                t.bels()
                    .iter()
                    .filter(|b| b.bel_type == BelType::Lut4)
                    .count()
            })
            .sum();
        assert!(total_luts > 0);

        let first = sites
            .take_nearest(
                w as f64 / 2.0,
                h as f64 / 2.0,
                BelType::Lut4,
                None,
                bel_types_compatible,
            )
            .expect("a free LUT");
        assert!(sites.is_taken(first.0, first.1, first.2));

        let mut taken = 1;
        while sites
            .take_nearest(
                w as f64 / 2.0,
                h as f64 / 2.0,
                BelType::Lut4,
                None,
                bel_types_compatible,
            )
            .is_some()
        {
            taken += 1;
        }
        assert_eq!(taken, total_luts, "every LUT is handed out exactly once");
    }

    #[test]
    fn fill_limit_spreads_cells_over_more_tiles() {
        let device = Ice40Device::new(Ice40Variant::Hx1k);
        let (w, h) = device.grid_size();
        let tiles_used = |limit: f64| {
            let mut sites = SiteMap::new(&device).with_fill_limit(limit);
            let mut used = std::collections::BTreeMap::new();
            for _ in 0..64 {
                let (x, y, _) = sites
                    .take_nearest(
                        w as f64 / 2.0,
                        h as f64 / 2.0,
                        BelType::Lut4,
                        None,
                        bel_types_compatible,
                    )
                    .expect("a free LUT");
                *used.entry((x, y)).or_insert(0usize) += 1;
            }
            used
        };
        let full = tiles_used(1.0);
        let half = tiles_used(0.5);
        // iCE40 logic tiles have 8 LUTs: 4 per tile at half fill
        assert_eq!(full.len(), 8);
        assert_eq!(half.len(), 16);
        assert!(half.values().all(|&n| n <= 4));
        assert_eq!(fill_cap(8, 0.5), 4);
        assert_eq!(fill_cap(8, 0.7), 6);
        assert_eq!(fill_cap(8, 0.0), 1);
        assert_eq!(fill_cap(0, 0.5), 0);
    }
}
