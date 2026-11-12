//! A* Shortest Path Router
//!
//! Implements A* pathfinding for FPGA routing.

use super::graph::{RoutingGraph, SinkApproach};
use crate::collections::{HashMap, HashMapExt, HashSet};
use crate::device::{Device, PipId, WireId};
use crate::error::{PlaceRouteError, Result};
use crate::timing::DelayModel;
use std::cmp::Ordering;
use std::collections::BinaryHeap;
use std::sync::{Mutex, OnceLock};

/// A* search node
#[derive(Clone)]
struct AStarNode {
    wire: WireId,
    g_score: f64, // Cost from start
    f_score: f64, // g_score + heuristic
}

impl Eq for AStarNode {}

impl PartialEq for AStarNode {
    fn eq(&self, other: &Self) -> bool {
        self.wire == other.wire
    }
}

impl Ord for AStarNode {
    fn cmp(&self, other: &Self) -> Ordering {
        // Reverse for min-heap. Among equal f-scores, expand the node
        // nearest the sink first (largest g): wires of the same cost are
        // common, and without the tie-break the search widens over every
        // equal-cost prefix before it goes deeper.
        other
            .f_score
            .partial_cmp(&self.f_score)
            .unwrap_or(Ordering::Equal)
            .then_with(|| {
                self.g_score
                    .partial_cmp(&other.g_score)
                    .unwrap_or(Ordering::Equal)
            })
    }
}

impl PartialOrd for AStarNode {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

/// Per-wire search state reused across searches. An entry is valid only if
/// its stamp equals the current search's generation, so starting a search
/// costs O(1) instead of clearing (or allocating) arrays sized to every wire
/// on the device: on a large device that per-sink allocation dominated
/// routing time.
#[derive(Default)]
struct SearchScratch {
    generation: u32,
    /// Generation in which `g` / `came_from` were last written
    seen: Vec<u32>,
    /// Generation in which the wire was closed
    closed: Vec<u32>,
    g: Vec<f64>,
    came_from: Vec<Option<(WireId, PipId)>>,
}

impl SearchScratch {
    /// Start a new search over `num_wires` wires
    fn begin(&mut self, num_wires: usize) {
        if self.seen.len() != num_wires {
            self.seen = vec![0; num_wires];
            self.closed = vec![0; num_wires];
            self.g = vec![f64::MAX; num_wires];
            self.came_from = vec![None; num_wires];
            self.generation = 0;
        }
        self.generation = self.generation.wrapping_add(1);
        if self.generation == 0 {
            // Stamps wrapped: old entries could look current, so clear them
            self.seen.iter_mut().for_each(|s| *s = 0);
            self.closed.iter_mut().for_each(|s| *s = 0);
            self.generation = 1;
        }
    }

    fn g(&self, w: WireId) -> f64 {
        let i = w.0 as usize;
        if self.seen[i] == self.generation {
            self.g[i]
        } else {
            f64::MAX
        }
    }

    fn set(&mut self, w: WireId, g: f64, from: Option<(WireId, PipId)>) {
        let i = w.0 as usize;
        self.seen[i] = self.generation;
        self.g[i] = g;
        self.came_from[i] = from;
    }

    fn came_from(&self, w: WireId) -> Option<(WireId, PipId)> {
        let i = w.0 as usize;
        if self.seen[i] == self.generation {
            self.came_from[i]
        } else {
            None
        }
    }

    fn is_closed(&self, w: WireId) -> bool {
        self.closed[w.0 as usize] == self.generation
    }

    fn close(&mut self, w: WireId) {
        self.closed[w.0 as usize] = self.generation;
    }
}

/// A [`SearchScratch`] borrowed from a router's pool for one search and
/// returned to it when the search ends
struct ScratchGuard<'p> {
    pool: &'p Mutex<Vec<SearchScratch>>,
    scratch: Option<SearchScratch>,
}

impl<'p> ScratchGuard<'p> {
    fn take(pool: &'p Mutex<Vec<SearchScratch>>) -> Self {
        let scratch = pool
            .lock()
            .unwrap_or_else(|e| e.into_inner())
            .pop()
            .unwrap_or_default();
        Self {
            pool,
            scratch: Some(scratch),
        }
    }
}

impl std::ops::Deref for ScratchGuard<'_> {
    type Target = SearchScratch;
    fn deref(&self) -> &SearchScratch {
        self.scratch.as_ref().expect("scratch is held until drop")
    }
}

impl std::ops::DerefMut for ScratchGuard<'_> {
    fn deref_mut(&mut self) -> &mut SearchScratch {
        self.scratch.as_mut().expect("scratch is held until drop")
    }
}

impl Drop for ScratchGuard<'_> {
    fn drop(&mut self) {
        if let Some(scratch) = self.scratch.take() {
            self.pool
                .lock()
                .unwrap_or_else(|e| e.into_inner())
                .push(scratch);
        }
    }
}

/// Tiles a search may enter: wires whose tile lies outside are not expanded.
/// Routing a net inside its bounding box (plus a margin) keeps a search from
/// flooding the whole device when the direct way is congested.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SearchBounds {
    pub min_x: u32,
    pub min_y: u32,
    pub max_x: u32,
    pub max_y: u32,
}

impl SearchBounds {
    /// Whether tile (x, y) is inside
    #[inline]
    pub fn contains(&self, x: u32, y: u32) -> bool {
        x >= self.min_x && x <= self.max_x && y >= self.min_y && y <= self.max_y
    }
}

/// A* router
pub struct AStarRouter<'a, D: Device> {
    device: &'a D,
    delay_model: Option<DelayModel>,
    /// The device's routing graph, built on the first search
    graph: OnceLock<RoutingGraph>,
    /// Search scratch not in use: a search takes one (or makes one) and
    /// gives it back, so searches can run on several threads at once
    scratch: Mutex<Vec<SearchScratch>>,
}

impl<'a, D: Device> AStarRouter<'a, D> {
    /// Create a new A* router
    pub fn new(device: &'a D) -> Self {
        Self {
            device,
            delay_model: None,
            graph: OnceLock::new(),
            scratch: Mutex::default(),
        }
    }

    /// Create an A* router with a delay model for wire-type-aware PIP costs
    pub fn with_delay_model(device: &'a D, delay_model: DelayModel) -> Self {
        Self {
            device,
            delay_model: Some(delay_model),
            graph: OnceLock::new(),
            scratch: Mutex::default(),
        }
    }

    /// The routing graph the searches run on
    pub fn graph(&self) -> &RoutingGraph {
        self.graph
            .get_or_init(|| RoutingGraph::build(self.device, self.delay_model.as_ref()))
    }

    /// Find a path from source to sink wire
    pub fn find_path(
        &self,
        source: WireId,
        sink: WireId,
        blocked: &[WireId],
    ) -> Result<(Vec<WireId>, Vec<PipId>)> {
        self.find_path_with_costs(source, sink, blocked, &HashMap::new())
    }

    /// Find a path with custom wire costs (for congestion-aware routing)
    /// wire_costs: additional cost multiplier per wire (typically from congestion)
    pub fn find_path_with_costs(
        &self,
        source: WireId,
        sink: WireId,
        blocked: &[WireId],
        wire_costs: &HashMap<WireId, f64>,
    ) -> Result<(Vec<WireId>, Vec<PipId>)> {
        self.find_path_timing_aware(source, sink, blocked, wire_costs, 0.0)
    }

    /// Find a path with timing-aware cost function
    /// wire_costs: congestion cost multiplier per wire
    /// timing_weight: how much to weight PIP delays (0.0 = ignore, 1.0 = fully weight)
    pub fn find_path_timing_aware(
        &self,
        source: WireId,
        sink: WireId,
        blocked: &[WireId],
        wire_costs: &HashMap<WireId, f64>,
        timing_weight: f64,
    ) -> Result<(Vec<WireId>, Vec<PipId>)> {
        self.find_path_with_cost_fn(
            source,
            sink,
            blocked,
            |w| wire_costs.get(&w).copied().unwrap_or(1.0),
            timing_weight,
        )
    }

    /// Find a path where `wire_cost(w)` gives the cost multiplier of entering
    /// wire `w`. Lets a caller compute congestion costs on demand for the
    /// wires the search actually reaches, instead of materializing a map of
    /// every used wire for each net.
    pub fn find_path_with_cost_fn(
        &self,
        source: WireId,
        sink: WireId,
        blocked: &[WireId],
        wire_cost: impl Fn(WireId) -> f64,
        timing_weight: f64,
    ) -> Result<(Vec<WireId>, Vec<PipId>)> {
        self.find_path_from_tree(
            std::slice::from_ref(&source),
            sink,
            blocked,
            wire_cost,
            timing_weight,
        )
    }

    /// [`Self::find_path_with_cost_fn`] from any wire of `tree` (a net's
    /// route so far, starting with its source wire): the search starts from
    /// all of them at cost 0, so a new sink branches off the nearest point of
    /// the tree instead of running a separate path back to the source. The
    /// returned path starts at the tree wire it branches from.
    pub fn find_path_from_tree(
        &self,
        tree: &[WireId],
        sink: WireId,
        blocked: &[WireId],
        wire_cost: impl Fn(WireId) -> f64,
        timing_weight: f64,
    ) -> Result<(Vec<WireId>, Vec<PipId>)> {
        self.find_path_bounded(tree, sink, blocked, wire_cost, timing_weight, |_| 0.0, None)
            .map(|(wires, pips, _)| (wires, pips))
    }

    /// [`Self::find_path_from_tree`] that only expands wires inside `bounds`
    /// (the tree's own wires may lie anywhere), also returning the path's
    /// cost. Fails if no path stays inside; a caller wanting any path
    /// retries without bounds. The search starts from each tree wire at
    /// `start_cost(wire)`: a timing-driven search charges the delay already
    /// spent reaching that wire, so a critical sink does not branch off the
    /// far end of a long tree just because the branch from there is short.
    #[allow(clippy::too_many_arguments)]
    pub fn find_path_bounded(
        &self,
        tree: &[WireId],
        sink: WireId,
        blocked: &[WireId],
        wire_cost: impl Fn(WireId) -> f64,
        timing_weight: f64,
        start_cost: impl Fn(WireId) -> f64,
        bounds: Option<SearchBounds>,
    ) -> Result<(Vec<WireId>, Vec<PipId>, f64)> {
        let source = *tree.first().ok_or_else(|| {
            PlaceRouteError::RoutingFailed("route search with no source wire".to_string())
        })?;
        let graph = self.graph();
        let num_wires = graph.wire_count();
        let in_graph = |w: WireId| (w.0 as usize) < num_wires;
        if !in_graph(sink) {
            return Err(PlaceRouteError::RoutingFailed(format!(
                "Sink wire {:?} not found",
                sink
            )));
        }
        let blocked_set: HashSet<_> = blocked.iter().copied().collect();
        let (target_x, target_y) = graph.tile(sink).unwrap_or((0, 0));
        let approach = graph.sink_approach(sink);

        let mut scratch = ScratchGuard::take(&self.scratch);
        scratch.begin(num_wires);
        let mut open_set = BinaryHeap::new();

        // Initialize with the source (and the rest of the tree)
        for &start in tree {
            if !in_graph(start) || blocked_set.contains(&start) {
                continue;
            }
            let g = start_cost(start);
            if scratch.g(start) <= g {
                continue;
            }
            scratch.set(start, g, None);
            open_set.push(AStarNode {
                wire: start,
                g_score: g,
                f_score: g + self.heuristic(
                    graph,
                    start,
                    target_x,
                    target_y,
                    &approach,
                    timing_weight,
                ),
            });
        }

        while let Some(current) = open_set.pop() {
            if current.wire == sink {
                let (wires, pips) = Self::reconstruct_path(source, sink, &scratch);
                return Ok((wires, pips, current.g_score));
            }
            if scratch.is_closed(current.wire) {
                continue;
            }
            scratch.close(current.wire);
            let current_g = current.g_score;

            // Expand neighbors via PIPs (forward routing - current wire is source)
            for edge in graph.edges(current.wire) {
                let neighbor = edge.dst;
                if scratch.is_closed(neighbor) {
                    continue;
                }
                if let Some(b) = bounds {
                    if neighbor != sink {
                        let (x, y) = graph.tile(neighbor).unwrap_or((0, 0));
                        if !b.contains(x, y) {
                            continue;
                        }
                    }
                }
                if !blocked_set.is_empty() && blocked_set.contains(&neighbor) {
                    continue;
                }

                let pip_cost = edge.cost as f64 + timing_weight * edge.timing as f64;
                let tentative_g = current_g + pip_cost * wire_cost(neighbor);
                if tentative_g < scratch.g(neighbor) {
                    scratch.set(neighbor, tentative_g, Some((current.wire, edge.pip)));
                    open_set.push(AStarNode {
                        wire: neighbor,
                        g_score: tentative_g,
                        f_score: tentative_g
                            + self.heuristic(
                                graph,
                                neighbor,
                                target_x,
                                target_y,
                                &approach,
                                timing_weight,
                            ),
                    });
                }
            }
        }

        // No path found
        Err(PlaceRouteError::RoutingFailed(format!(
            "No path found from wire {:?} to {:?}",
            source, sink
        )))
    }

    /// A lower bound on the cost from `wire` to the target tile: the
    /// device's own estimate when it has one, otherwise one derived from the
    /// routing graph's PIPs. It must never exceed a real path's cost, or the
    /// search returns needlessly long routes.
    fn heuristic(
        &self,
        graph: &RoutingGraph,
        wire: WireId,
        target_x: u32,
        target_y: u32,
        approach: &SinkApproach,
        timing_weight: f64,
    ) -> f64 {
        let Some((x, y)) = graph.tile(wire) else {
            return 0.0;
        };
        let dx = x.abs_diff(target_x);
        let dy = y.abs_diff(target_y);
        // A timing-driven search also pays the PIPs' timing terms; without
        // a bound on those the search expands every wire that is cheap in
        // base cost however slow it is
        let timing = if timing_weight > 0.0 {
            timing_weight * graph.estimate_timing_to_sink(wire, approach)
        } else {
            0.0
        };
        if let Some(estimate) = self.device.route_cost_estimate(dx, dy) {
            return if dx + dy == 0 { 0.0 } else { estimate + timing };
        }
        // Derived from the routing graph itself, for any device: a lower
        // bound on paths through the general fabric
        graph.estimate_to_sink(wire, approach) + timing
    }

    /// Reconstruct the path ending at `sink` from the search scratch
    fn reconstruct_path(
        source: WireId,
        sink: WireId,
        scratch: &SearchScratch,
    ) -> (Vec<WireId>, Vec<PipId>) {
        let mut wires = vec![sink];
        let mut pips = Vec::new();
        let mut current = sink;

        while let Some((prev, pip)) = scratch.came_from(current) {
            wires.push(prev);
            pips.push(pip);
            current = prev;

            if current == source {
                break;
            }
        }

        wires.reverse();
        pips.reverse();

        (wires, pips)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::device::nexus::{NexusDevice, NexusVariant};
    use crate::device::CellPin;

    /// A LUT output in a logic tile near the middle of the device and a LUT
    /// input in the logic tile `dx` tiles to its right
    fn pins(device: &NexusDevice, dx: u32) -> (WireId, WireId, (u32, u32), (u32, u32)) {
        let (w, h) = device.grid_size();
        let y = h / 2;
        (w / 4..w - dx)
            .find_map(|x0| {
                let src = device.bel_pin_wire(x0, y, 0, CellPin::Output(0))?;
                let dst = device.bel_pin_wire(x0 + dx, y, 0, CellPin::Input(0))?;
                Some((src, dst, (x0, y), (x0 + dx, y)))
            })
            .expect("two logic tiles in a row")
    }

    /// A search from a tree starts each tree wire at its start cost: a
    /// wire next to the sink that is free to start from is where the path
    /// begins, and once starting there costs more than the whole way from
    /// the source, the path runs from the source instead
    #[test]
    fn a_tree_wire_is_charged_its_start_cost() {
        let device = NexusDevice::new(NexusVariant::Lifcl40);
        let astar = AStarRouter::new(&device);
        let (src, dst, _, _) = pins(&device, 8);
        let (direct, _, direct_cost) = astar
            .find_path_bounded(&[src], dst, &[], |_| 1.0, 0.0, |_| 0.0, None)
            .expect("a path");
        // The wire just before the sink on that path, as a second tree wire
        let near = direct[direct.len() - 2];
        assert_ne!(near, src);
        let tree = [src, near];

        let (wires, _, cost) = astar
            .find_path_bounded(&tree, dst, &[], |_| 1.0, 0.0, |_| 0.0, None)
            .expect("a path");
        assert_eq!(wires.first(), Some(&near));
        assert!(cost < direct_cost);

        let charged = |w: WireId| if w == near { direct_cost + 1.0 } else { 0.0 };
        let (wires, _, cost) = astar
            .find_path_bounded(&tree, dst, &[], |_| 1.0, 0.0, charged, None)
            .expect("a path");
        assert_eq!(wires.first(), Some(&src));
        assert!((cost - direct_cost).abs() < 1e-6);
    }

    #[test]
    fn bounded_search_stays_inside_its_region() {
        let device = NexusDevice::new(NexusVariant::Lifcl40);
        let astar = AStarRouter::new(&device);
        let (src, dst, (x0, y), (x1, _)) = pins(&device, 8);

        let unbounded = astar.find_path(src, dst, &[]).expect("a path");
        let bounds = SearchBounds {
            min_x: x0,
            min_y: y.saturating_sub(2),
            max_x: x1,
            max_y: y + 2,
        };
        let (wires, pips, cost) = astar
            .find_path_bounded(&[src], dst, &[], |_| 1.0, 0.0, |_| 0.0, Some(bounds))
            .expect("a path inside the net's box");
        assert_eq!(wires.first(), Some(&src));
        assert_eq!(wires.last(), Some(&dst));
        assert_eq!(pips.len() + 1, wires.len());
        // Unit wire costs: the cost is the sum of the PIP costs
        let pip_costs: f64 = pips
            .iter()
            .map(|p| device.pip(*p).unwrap().cost() as f64)
            .sum();
        assert!((cost - pip_costs).abs() < 1e-6);
        for w in &wires {
            let (x, y) = astar.graph().tile(*w).unwrap();
            assert!(bounds.contains(x, y), "wire {w:?} at ({x}, {y}) is outside");
        }
        // Every PIP joins consecutive wires of the path
        for (pair, pip) in wires.windows(2).zip(&pips) {
            let p = device.pip(*pip).unwrap();
            assert_eq!((p.src_wire, p.dst_wire), (pair[0], pair[1]));
        }
        assert!(!unbounded.0.is_empty());

        // A region that holds the source but not the way to the sink fails,
        // rather than leaving it
        let tight = SearchBounds {
            min_x: x0,
            min_y: y,
            max_x: x0,
            max_y: y,
        };
        assert!(astar
            .find_path_bounded(&[src], dst, &[], |_| 1.0, 0.0, |_| 0.0, Some(tight))
            .is_err());
    }

    #[test]
    fn routing_graph_is_built_once_and_matches_the_device() {
        let device = NexusDevice::new(NexusVariant::Lifcl40);
        let astar = AStarRouter::new(&device);
        let (src, dst, _, _) = pins(&device, 3);
        astar.find_path(src, dst, &[]).expect("a path");
        let graph: *const RoutingGraph = astar.graph();
        astar.find_path(src, dst, &[]).expect("a path");
        assert!(std::ptr::eq(graph, astar.graph()));
        assert_eq!(astar.graph().wire_count(), device.wire_count());
        for e in astar.graph().edges(src) {
            assert_eq!(device.pip(e.pip).unwrap().src_wire, src);
        }
    }
}
