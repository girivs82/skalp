//! How long a routed connection between two logic cells takes, by how far
//! apart their tiles are, measured on a device's own routing graph.
//!
//! The placer needs a delay for every connection of every candidate move,
//! long before anything is routed. A formula over wire spans is only as good
//! as its idea of which wires a route takes; on the real Nexus graph the one
//! in [`DelayModel::estimated_path_delay`](super::DelayModel) charged a
//! 12-tile hop 0.13 ns more than a neighbouring one, where routes really take
//! about 0.5 ns more, so the placer saw little reason to keep critical cells
//! together. Instead, [`RouteDelayTable::measure`] runs a shortest-delay
//! search (summing [`Pip::delay`](crate::device::Pip)) from the LUT outputs of
//! a few logic tiles to the LUT inputs of every other one, as nextpnr's
//! router lookahead does, and keeps the delay for each tile offset.

use crate::device::{Device, WireId};
use std::cmp::Reverse;
use std::collections::BinaryHeap;

/// Logic tiles the search starts from: spread around the device's middle, so
/// the table is not shaped by one tile's quirks or by the edge of the die.
const SAMPLE_OFFSETS: [(i64, i64); 4] = [(0, 0), (-3, 2), (4, -3), (2, 4)];

/// Delay (ns) of a routed connection from a LUT output to a LUT input whose
/// tiles are `dx` columns and `dy` rows apart.
#[derive(Debug, Clone, PartialEq)]
pub struct RouteDelayTable {
    width: usize,
    height: usize,
    /// ns, indexed by `dy * width + dx`
    delays: Vec<f64>,
}

impl RouteDelayTable {
    /// Measure the table on `device`'s routing graph. `None` when the device
    /// has no logic tiles with LUT pin wires, or its PIPs carry no delays
    /// (synthetic models): the caller keeps its formula then.
    pub fn measure<D: Device + ?Sized>(device: &D) -> Option<Self> {
        let (width, height) = device.grid_size();
        let logic: Vec<(u32, u32)> = (0..height)
            .flat_map(|y| (0..width).map(move |x| (x, y)))
            .filter(|&(x, y)| device.lut_output_wire(x, y, 0).is_some())
            .collect();
        if logic.is_empty() {
            return None;
        }
        // Middle of the logic area, then the logic tile nearest each sample
        // offset from it.
        let mid = |axis: fn(&(u32, u32)) -> u32| {
            let (lo, hi) = logic
                .iter()
                .map(axis)
                .fold((u32::MAX, 0), |(lo, hi), v| (lo.min(v), hi.max(v)));
            (lo as i64 + hi as i64) / 2
        };
        let centre = (mid(|t| t.0), mid(|t| t.1));
        let mut samples: Vec<(u32, u32)> = SAMPLE_OFFSETS
            .iter()
            .filter_map(|&(ox, oy)| {
                let target = (centre.0 + ox, centre.1 + oy);
                logic
                    .iter()
                    .copied()
                    .min_by_key(|&(x, y)| (x as i64 - target.0).abs() + (y as i64 - target.1).abs())
            })
            .collect();
        samples.sort_unstable();
        samples.dedup();

        let (w, h) = (width as usize, height as usize);
        let mut sum = vec![0.0f64; w * h];
        let mut count = vec![0u32; w * h];
        let mut any_delay = false;
        let mut dist = vec![u32::MAX; device.wire_count()];
        for &(sx, sy) in &samples {
            let sources: Vec<WireId> = lut_pins(1, |lc, _| device.lut_output_wire(sx, sy, lc))
                .into_iter()
                .collect();
            any_delay |= shortest_delays(device, &sources, &mut dist);
            for &(x, y) in &logic {
                // A connection lands on any input of any LUT of the tile;
                // their mean is what a typical one takes.
                let reached: Vec<u32> = lut_pins(4, |lc, pin| device.lut_input_wire(x, y, lc, pin))
                    .into_iter()
                    .map(|wire| dist[wire.0 as usize])
                    .filter(|&d| d != u32::MAX)
                    .collect();
                if reached.is_empty() {
                    continue;
                }
                let mean = reached.iter().map(|&d| d as f64).sum::<f64>() / reached.len() as f64;
                let i = (y.abs_diff(sy) as usize) * w + x.abs_diff(sx) as usize;
                sum[i] += mean / 1000.0;
                count[i] += 1;
            }
        }
        if !any_delay {
            return None;
        }
        let mut delays: Vec<f64> = sum
            .iter()
            .zip(&count)
            .map(|(&s, &c)| if c > 0 { s / c as f64 } else { f64::NAN })
            .collect();
        fill_monotone(&mut delays, w, h);
        Some(Self {
            width: w,
            height: h,
            delays,
        })
    }

    /// A table from explicit values (ns, `dy * width + dx`), for tests.
    #[cfg(test)]
    pub(crate) fn from_values(width: usize, height: usize, delays: Vec<f64>) -> Self {
        assert_eq!(delays.len(), width * height);
        Self {
            width,
            height,
            delays,
        }
    }

    /// Delay (ns) of a connection between tiles `dx` columns and `dy` rows
    /// apart. Offsets past the table extend it along its last step.
    pub fn delay(&self, dx: u32, dy: u32) -> f64 {
        let along = |d: u32, len: usize| -> (usize, f64) {
            let last = len - 1;
            let d = d as usize;
            if d <= last {
                (d, 0.0)
            } else {
                (last, (d - last) as f64)
            }
        };
        let (x, beyond_x) = along(dx, self.width);
        let (y, beyond_y) = along(dy, self.height);
        let at = |x: usize, y: usize| self.delays[y * self.width + x];
        let step_x = if x > 0 { at(x, y) - at(x - 1, y) } else { 0.0 };
        let step_y = if y > 0 { at(x, y) - at(x, y - 1) } else { 0.0 };
        at(x, y) + beyond_x * step_x.max(0.0) + beyond_y * step_y.max(0.0)
    }
}

/// The wires `pin(lc, k)` gives for the eight LUTs `lc` of a tile and the
/// first `pins` of each LUT's pins (deduplicated, in wire order).
fn lut_pins(pins: usize, pin: impl Fn(usize, usize) -> Option<WireId>) -> Vec<WireId> {
    let mut wires: Vec<WireId> = (0..8)
        .flat_map(|lc| (0..pins).map(move |k| (lc, k)))
        .filter_map(|(lc, k)| pin(lc, k))
        .collect();
    wires.sort_unstable_by_key(|w| w.0);
    wires.dedup();
    wires
}

/// Least summed PIP delay (ps) from any of `sources` to every wire, into
/// `dist` (`u32::MAX`: unreachable). Returns whether any PIP on the way had a
/// delay.
fn shortest_delays<D: Device + ?Sized>(device: &D, sources: &[WireId], dist: &mut [u32]) -> bool {
    dist.fill(u32::MAX);
    let mut heap = BinaryHeap::new();
    for &s in sources {
        dist[s.0 as usize] = 0;
        heap.push(Reverse((0u32, s.0)));
    }
    let mut any_delay = false;
    while let Some(Reverse((d, wire))) = heap.pop() {
        if d > dist[wire as usize] {
            continue;
        }
        for &pip_id in device.wire_src_pips(WireId(wire)) {
            let Some(pip) = device.pip(pip_id) else {
                continue;
            };
            any_delay |= pip.delay > 0;
            let next = d.saturating_add(pip.delay);
            let slot = &mut dist[pip.dst_wire.0 as usize];
            if next < *slot {
                *slot = next;
                heap.push(Reverse((next, pip.dst_wire.0)));
            }
        }
    }
    any_delay
}

/// Fill offsets no search reached and make the table never decrease with
/// distance: a route can always go as far and then some, so a farther tile
/// is never quicker to reach (a measured dip is a quirk of the sampled tiles).
fn fill_monotone(delays: &mut [f64], width: usize, height: usize) {
    for y in 0..height {
        for x in 0..width {
            let i = y * width + x;
            let left = if x > 0 { delays[i - 1] } else { f64::NAN };
            let below = if y > 0 { delays[i - width] } else { f64::NAN };
            let floor = left.max(below); // NaN only when both are
            delays[i] = match (delays[i].is_nan(), floor.is_nan()) {
                (true, _) => floor,
                (false, true) => delays[i],
                (false, false) => delays[i].max(floor),
            };
        }
    }
    // Anything still unknown (nothing reached at a smaller offset either)
    // takes the smallest known delay.
    let least = delays
        .iter()
        .copied()
        .filter(|d| !d.is_nan())
        .fold(f64::INFINITY, f64::min);
    let least = if least.is_finite() { least } else { 0.0 };
    for d in delays.iter_mut().filter(|d| d.is_nan()) {
        *d = least;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn offsets_past_the_table_extend_its_last_step() {
        // 3x2: rows 0.1 0.2 0.4 / 0.3 0.4 0.6
        let t = RouteDelayTable::from_values(3, 2, vec![0.1, 0.2, 0.4, 0.3, 0.4, 0.6]);
        assert_eq!(t.delay(1, 0), 0.2);
        assert_eq!(t.delay(2, 1), 0.6);
        assert!((t.delay(4, 0) - 0.8).abs() < 1e-9, "{}", t.delay(4, 0));
        assert!((t.delay(2, 3) - 1.0).abs() < 1e-9, "{}", t.delay(2, 3));
    }

    #[test]
    fn gaps_are_filled_and_dips_flattened() {
        let nan = f64::NAN;
        let mut d = vec![0.1, nan, 0.3, 0.5, 0.2, nan];
        fill_monotone(&mut d, 3, 2);
        assert_eq!(d, vec![0.1, 0.1, 0.3, 0.5, 0.5, 0.5]);
    }

    #[test]
    fn the_synthetic_nexus_model_gets_a_growing_table() {
        let device =
            crate::device::nexus::NexusDevice::new(crate::device::nexus::NexusVariant::Lifcl40);
        let t = RouteDelayTable::measure(&device).expect("LUT pins and PIP delays");
        assert!(t.delay(0, 0) <= t.delay(3, 0));
        assert!(
            t.delay(3, 0) < t.delay(20, 10),
            "{} {}",
            t.delay(3, 0),
            t.delay(20, 10)
        );
    }

    /// On the real LIFCL-40 graph a connection costs about a third of a
    /// nanosecond next door and grows by tens of picoseconds per tile.
    #[cfg(feature = "nexus-bitstream")]
    #[test]
    fn real_nexus_delays_grow_with_distance() {
        use crate::device::nexus::prjoxide_load::find_database;
        use crate::device::nexus::NexusDevice;
        let Some(db) = find_database() else {
            eprintln!("prjoxide database not found; skipping");
            return;
        };
        let variant = crate::device::nexus::NexusVariant::Lifcl40;
        let mut device = NexusDevice::from_prjoxide_bbox(variant, &db, (10, 10, 40, 40)).unwrap();
        let timing = crate::device::nexus::timing::TimingDb::load(&db, variant).unwrap();
        device.set_pip_delays(&timing);
        let t = RouteDelayTable::measure(&device).expect("table");
        let (near, mid, far) = (t.delay(1, 0), t.delay(6, 0), t.delay(12, 6));
        eprintln!("1: {near:.3}  6: {mid:.3}  12+6: {far:.3}");
        assert!(near > 0.1 && near < 0.6, "{near}");
        assert!(mid > near && far > mid, "{near} {mid} {far}");
    }
}
