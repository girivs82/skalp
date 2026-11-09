//! Free-site bookkeeping for placement.
//!
//! [`SiteMap`] answers "nearest free BEL that can hold this cell" in time
//! proportional to the tiles actually inspected: it walks square rings
//! outward from the target (each tile visited once), and skips full tiles and
//! tiles without a compatible BEL in O(1). The previous search rescanned the
//! whole square at every radius and probed a hash map per BEL, which made
//! initial placement cubic in the filled area — on a 15K-cell Nexus design it
//! did not finish within an hour.

use super::BelType;
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
}

/// Occupancy of every BEL on the device.
#[derive(Debug, Clone)]
pub struct SiteMap {
    width: u32,
    height: u32,
    tiles: Vec<TileSites>,
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
                tiles.push(TileSites { bels, free });
            }
        }
        Self {
            width,
            height,
            tiles,
        }
    }

    fn tile_index(&self, x: u32, y: u32) -> usize {
        (y * self.width + x) as usize
    }

    /// Mark a BEL taken. Returns false if it was already taken or does not exist.
    pub fn take(&mut self, x: u32, y: u32, bel_index: usize) -> bool {
        if x >= self.width || y >= self.height {
            return false;
        }
        let idx = self.tile_index(x, y);
        let tile = &mut self.tiles[idx];
        match tile.bels.get_mut(bel_index) {
            Some((_, taken)) if !*taken => {
                *taken = true;
                tile.free -= 1;
                true
            }
            _ => false,
        }
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
        compatible: &impl Fn(BelType, BelType) -> bool,
    ) -> Option<usize> {
        let tile = &self.tiles[self.tile_index(x, y)];
        if tile.free == 0 {
            return None;
        }
        tile.bels
            .iter()
            .position(|&(t, taken)| !taken && t == required)
            .or_else(|| {
                tile.bels
                    .iter()
                    .position(|&(t, taken)| !taken && compatible(t, required))
            })
    }

    /// Take the free BEL nearest to (x, y) (Chebyshev rings, nearest tile first
    /// within a ring) that can hold `required`. Returns (tile_x, tile_y, bel_index).
    pub fn take_nearest(
        &mut self,
        x: f64,
        y: f64,
        required: BelType,
        compatible: impl Fn(BelType, BelType) -> bool,
    ) -> Option<(u32, u32, usize)> {
        let found = self.find_nearest(x, y, required, &compatible)?;
        self.take(found.0, found.1, found.2);
        Some(found)
    }

    /// The free BEL nearest to (x, y) that can hold `required`, without taking it.
    pub fn find_nearest(
        &self,
        x: f64,
        y: f64,
        required: BelType,
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
                if let Some(bel) = self.free_bel_in_tile(tx as u32, ty as u32, required, compatible)
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
                bel_types_compatible,
            )
            .is_some()
        {
            taken += 1;
        }
        assert_eq!(taken, total_luts, "every LUT is handed out exactly once");
    }
}
