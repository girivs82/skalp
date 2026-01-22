//! A* Shortest Path Router
//!
//! Implements A* pathfinding for FPGA routing.

use crate::device::{Device, PipId, WireId};
use crate::error::{PlaceRouteError, Result};
use std::cmp::Ordering;
use std::collections::{BinaryHeap, HashMap, HashSet};

/// A* search node
#[derive(Clone)]
#[allow(dead_code)]
struct AStarNode {
    wire: WireId,
    g_score: f64, // Cost from start
    f_score: f64, // g_score + heuristic
    parent: Option<(WireId, PipId)>,
}

impl Eq for AStarNode {}

impl PartialEq for AStarNode {
    fn eq(&self, other: &Self) -> bool {
        self.wire == other.wire
    }
}

impl Ord for AStarNode {
    fn cmp(&self, other: &Self) -> Ordering {
        // Reverse for min-heap
        other
            .f_score
            .partial_cmp(&self.f_score)
            .unwrap_or(Ordering::Equal)
    }
}

impl PartialOrd for AStarNode {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

/// A* router
pub struct AStarRouter<'a, D: Device> {
    device: &'a D,
}

impl<'a, D: Device> AStarRouter<'a, D> {
    /// Create a new A* router
    pub fn new(device: &'a D) -> Self {
        Self { device }
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
        let blocked_set: HashSet<_> = blocked.iter().copied().collect();

        // Get target coordinates for heuristic
        let sink_wire = self.device.wire(sink).ok_or_else(|| {
            PlaceRouteError::RoutingFailed(format!("Sink wire {:?} not found", sink))
        })?;
        let target_x = sink_wire.tile_x;
        let target_y = sink_wire.tile_y;

        let mut open_set = BinaryHeap::new();
        let mut came_from: HashMap<WireId, (WireId, PipId)> = HashMap::new();
        let mut g_scores: HashMap<WireId, f64> = HashMap::new();
        let mut closed_set: HashSet<WireId> = HashSet::new();

        // Initialize with source
        let h = self.heuristic(source, target_x, target_y);
        g_scores.insert(source, 0.0);
        open_set.push(AStarNode {
            wire: source,
            g_score: 0.0,
            f_score: h,
            parent: None,
        });

        while let Some(current) = open_set.pop() {
            // Check if we reached the sink
            if current.wire == sink {
                return Ok(self.reconstruct_path(source, sink, &came_from));
            }

            // Skip if already processed
            if closed_set.contains(&current.wire) {
                continue;
            }
            closed_set.insert(current.wire);

            // Get current g-score
            let current_g = g_scores.get(&current.wire).copied().unwrap_or(f64::MAX);

            // Expand neighbors via PIPs (forward routing - current wire is source)
            for pip_id in self.device.wire_src_pips(current.wire) {
                let pip = match self.device.pip(pip_id) {
                    Some(p) => p,
                    None => continue,
                };

                // Get the destination wire (the one we're routing to)
                // Since we're using wire_src_pips, current.wire should be pip.src_wire
                let neighbor = pip.dst_wire;

                // Skip blocked wires
                if blocked_set.contains(&neighbor) {
                    continue;
                }

                // Skip already visited
                if closed_set.contains(&neighbor) {
                    continue;
                }

                // Calculate cost with optional timing awareness
                // pip_cost includes base cost + delay contribution
                let base_pip_cost = pip.cost() as f64;
                // For timing-driven routing, add extra cost for high-delay PIPs
                let timing_cost = if timing_weight > 0.0 {
                    timing_weight * (pip.delay as f64 / 50.0) // Normalize delay contribution
                } else {
                    0.0
                };
                let pip_cost = base_pip_cost + timing_cost;
                let wire_cost = wire_costs.get(&neighbor).copied().unwrap_or(1.0);
                let tentative_g = current_g + pip_cost * wire_cost;

                // Check if this is a better path
                let neighbor_g = g_scores.get(&neighbor).copied().unwrap_or(f64::MAX);
                if tentative_g < neighbor_g {
                    // Record this path
                    came_from.insert(neighbor, (current.wire, pip_id));
                    g_scores.insert(neighbor, tentative_g);

                    let h = self.heuristic(neighbor, target_x, target_y);
                    let f = tentative_g + h;

                    open_set.push(AStarNode {
                        wire: neighbor,
                        g_score: tentative_g,
                        f_score: f,
                        parent: Some((current.wire, pip_id)),
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

    /// Manhattan distance heuristic
    fn heuristic(&self, wire: WireId, target_x: u32, target_y: u32) -> f64 {
        let wire_info = match self.device.wire(wire) {
            Some(w) => w,
            None => return 0.0,
        };

        let dx = (wire_info.tile_x as i32 - target_x as i32).unsigned_abs();
        let dy = (wire_info.tile_y as i32 - target_y as i32).unsigned_abs();

        // Scale by expected wire cost
        (dx + dy) as f64 * 2.0
    }

    /// Reconstruct path from came_from map
    fn reconstruct_path(
        &self,
        source: WireId,
        sink: WireId,
        came_from: &HashMap<WireId, (WireId, PipId)>,
    ) -> (Vec<WireId>, Vec<PipId>) {
        let mut wires = vec![sink];
        let mut pips = Vec::new();
        let mut current = sink;

        while let Some(&(prev, pip)) = came_from.get(&current) {
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
