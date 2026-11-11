//! Router lookahead: the cost of reaching a tile from a wire, learned from
//! the routing graph by wire class.
//!
//! The displacement bound ([`RoutingGraph::cost_lower_bound`]) counts each
//! PIP by how far it moves a wire's home tile, and a long wire's home is one
//! end of it, so a single PIP can move the home a dozen tiles. On a Nexus
//! device that made the bound three PIPs for paths costing nine to twelve,
//! and the search expanded far more than it needed. Like VPR's lookahead,
//! this measures instead: for each class of wire (the same wire in every
//! tile, such as one track of the 6-tile horizontal wires), a search from a
//! wire of the class near the middle of the device records the least cost of
//! reaching each tile around it. A wire of that class anywhere else is
//! estimated by its displacement to the sink's tile.
//!
//! The fabric repeats, but not perfectly (block RAM and DSP columns, IO
//! edges), so the estimate is not a strict bound everywhere: it is the
//! least of two sample wires of the class, and the router uses it for speed
//! the way VPR does.

use super::graph::RoutingGraph;
use crate::device::WireId;
use rayon::prelude::*;
use std::cmp::Reverse;
use std::collections::BinaryHeap;

/// Tiles around a sample wire whose costs are measured, along each axis
pub(super) const RADIUS: i32 = 20;

const SIDE: usize = (2 * RADIUS + 1) as usize;

/// Least cost from a wire of each class to each tile within [`RADIUS`]
#[derive(Debug, Clone, Default)]
pub struct Lookahead {
    /// Per class, `SIDE * SIDE` costs indexed by displacement (infinite
    /// where no path was found), or none for a class too rare to measure
    tables: Vec<Option<Box<[f32]>>>,
}

impl Lookahead {
    /// Measure every class with at least `min_count` wires
    pub(super) fn build(graph: &RoutingGraph, class: &[u32], class_count: usize) -> Self {
        let mut members: Vec<Vec<WireId>> = vec![Vec::new(); class_count];
        for (w, &c) in class.iter().enumerate() {
            members[c as usize].push(WireId(w as u32));
        }
        let (width, height) = class
            .iter()
            .enumerate()
            .filter_map(|(w, _)| graph.tile(WireId(w as u32)))
            .fold((0, 0), |(a, b), (x, y)| (a.max(x + 1), b.max(y + 1)));
        let tiles = (width as usize * height as usize).max(1);
        // A class present in a good share of the tiles is part of the
        // repeating fabric; the rest (hard block pins) keep the plain bound
        let min_count = (tiles / 8).max(16);
        let centre = (width as i64 / 2, height as i64 / 2);
        let samples = [
            centre,
            (centre.0 + RADIUS as i64 / 2, centre.1 + RADIUS as i64 / 3),
        ];
        let n = graph.wire_count();
        let tables: Vec<Option<Box<[f32]>>> = members
            .par_iter()
            .map_init(
                || Scratch::new(n),
                |scratch, wires| {
                    if wires.len() < min_count {
                        return None;
                    }
                    let mut table = vec![f32::INFINITY; SIDE * SIDE].into_boxed_slice();
                    for &(sx, sy) in &samples {
                        let start = wires.iter().copied().min_by_key(|&w| {
                            let (x, y) = graph.tile(w).unwrap_or((0, 0));
                            ((x as i64 - sx).abs() + (y as i64 - sy).abs(), w.0)
                        })?;
                        scratch.measure(graph, start, &mut table);
                    }
                    Some(table)
                },
            )
            .collect();
        let measured = tables.iter().filter(|t| t.is_some()).count();
        log::debug!("router lookahead: {measured} of {class_count} wire classes measured");
        Self { tables }
    }

    /// Least measured cost from a wire of class `class` to a tile `dx`, `dy`
    /// away, if the class was measured and the tile is within the radius
    /// and was reached
    pub(super) fn cost(&self, class: u32, dx: i32, dy: i32) -> Option<f64> {
        if dx.abs() > RADIUS || dy.abs() > RADIUS {
            return None;
        }
        let table = self.tables.get(class as usize)?.as_ref()?;
        let index = (dy + RADIUS) as usize * SIDE + (dx + RADIUS) as usize;
        let cost = table[index];
        cost.is_finite().then_some(cost as f64)
    }
}

/// One thread's search state, reused across classes
struct Scratch {
    dist: Vec<f32>,
    touched: Vec<u32>,
}

impl Scratch {
    fn new(n: usize) -> Self {
        Self {
            dist: vec![f32::INFINITY; n],
            touched: Vec::new(),
        }
    }

    /// Search outward from `start` over base PIP costs, keeping to the
    /// tiles within a little more than the radius, and lower each tile's
    /// entry of `table` to the least cost of any wire homed there
    fn measure(&mut self, graph: &RoutingGraph, start: WireId, table: &mut [f32]) {
        let Some((x0, y0)) = graph.tile(start) else {
            return;
        };
        let reach = RADIUS + 2;
        let mut heap = BinaryHeap::new();
        self.dist[start.0 as usize] = 0.0;
        self.touched.push(start.0);
        heap.push(Reverse((Cost(0.0), start.0)));
        while let Some(Reverse((Cost(d), w))) = heap.pop() {
            if d > self.dist[w as usize] {
                continue;
            }
            let Some((x, y)) = graph.tile(WireId(w)) else {
                continue;
            };
            let (dx, dy) = (x as i32 - x0 as i32, y as i32 - y0 as i32);
            if dx.abs() <= RADIUS && dy.abs() <= RADIUS {
                let index = (dy + RADIUS) as usize * SIDE + (dx + RADIUS) as usize;
                table[index] = table[index].min(d);
            }
            for e in graph.edges(WireId(w)) {
                let to = e.dst.0 as usize;
                if graph.is_global(e.dst) {
                    continue;
                }
                let Some((tx, ty)) = graph.tile(e.dst) else {
                    continue;
                };
                if (tx as i32 - x0 as i32).abs() > reach || (ty as i32 - y0 as i32).abs() > reach {
                    continue;
                }
                let nd = d + e.cost;
                if nd < self.dist[to] {
                    if self.dist[to].is_infinite() {
                        self.touched.push(to as u32);
                    }
                    self.dist[to] = nd;
                    heap.push(Reverse((Cost(nd), to as u32)));
                }
            }
        }
        for w in self.touched.drain(..) {
            self.dist[w as usize] = f32::INFINITY;
        }
    }
}

/// An `f32` cost ordered for the search heap
#[derive(Clone, Copy, PartialEq)]
struct Cost(f32);

impl Eq for Cost {}

impl PartialOrd for Cost {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Cost {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.total_cmp(&other.0)
    }
}

/// The class of a wire, from its name: the same wire in every tile shares
/// it. Tile coordinates (`R12C40`) and the trailing number of each part
/// (the track or the slot: `H06E0003` is a track of the 6-tile wires
/// heading east, `JF1_SLICEC` a slice output) are dropped.
pub(super) fn class_key(name: &str) -> String {
    let mut key = String::with_capacity(name.len());
    for part in name.split('_') {
        if is_tile_coordinate(part) {
            continue;
        }
        let stem = part.trim_end_matches(|c: char| c.is_ascii_digit());
        if !key.is_empty() {
            key.push('_');
        }
        key.push_str(stem);
    }
    key
}

/// `R<row>C<column>`
fn is_tile_coordinate(part: &str) -> bool {
    let Some(rest) = part.strip_prefix('R') else {
        return false;
    };
    let Some((row, column)) = rest.split_once('C') else {
        return false;
    };
    !row.is_empty()
        && !column.is_empty()
        && row.bytes().all(|b| b.is_ascii_digit())
        && column.bytes().all(|b| b.is_ascii_digit())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn class_key_drops_tile_and_track() {
        assert_eq!(class_key("R2C97_H06E0003"), "H06E");
        assert_eq!(class_key("R13C9_JF1_SLICEC"), "JF_SLICEC");
        assert_eq!(class_key("R13C9_JF0_SLICEC"), class_key("R40C2_JF7_SLICEC"));
        assert_ne!(class_key("R2C97_H06E0003"), class_key("R2C97_H02E0003"));
        assert_eq!(class_key("sp4_h_r_3"), "sp_h_r_");
    }
}
