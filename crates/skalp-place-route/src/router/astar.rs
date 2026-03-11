//! A* Shortest Path Router
//!
//! Implements A* pathfinding for FPGA routing.

use crate::device::{Device, PipId, WireId};
use crate::error::{PlaceRouteError, Result};
use crate::timing::DelayModel;
use std::cmp::Ordering;
use std::collections::{BinaryHeap, HashMap, HashSet};

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
    delay_model: Option<DelayModel>,
}

impl<'a, D: Device> AStarRouter<'a, D> {
    /// Create a new A* router
    pub fn new(device: &'a D) -> Self {
        Self {
            device,
            delay_model: None,
        }
    }

    /// Create an A* router with a delay model for wire-type-aware PIP costs
    pub fn with_delay_model(device: &'a D, delay_model: DelayModel) -> Self {
        Self {
            device,
            delay_model: Some(delay_model),
        }
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

        // Use Vec-indexed arrays for O(1) lookups when wire_count is known,
        // falling back to HashMap for unknown wire domains.
        let num_wires = self.device.wire_count();
        let use_vec = num_wires > 0;

        let mut open_set = BinaryHeap::new();

        // Vec-based storage for O(1) access (indexed by WireId)
        let mut g_scores_vec: Vec<f64> = if use_vec {
            vec![f64::MAX; num_wires]
        } else {
            Vec::new()
        };
        let mut closed_vec: Vec<bool> = if use_vec {
            vec![false; num_wires]
        } else {
            Vec::new()
        };
        let mut came_from_vec: Vec<Option<(WireId, PipId)>> = if use_vec {
            vec![None; num_wires]
        } else {
            Vec::new()
        };

        // HashMap fallback for unknown wire domains
        let mut g_scores_map: HashMap<WireId, f64> = HashMap::new();
        let mut closed_map: HashSet<WireId> = HashSet::new();
        let mut came_from_map: HashMap<WireId, (WireId, PipId)> = HashMap::new();

        // Helper closures via inline functions below
        macro_rules! get_g {
            ($w:expr) => {
                if use_vec {
                    g_scores_vec[$w.0 as usize]
                } else {
                    g_scores_map.get(&$w).copied().unwrap_or(f64::MAX)
                }
            };
        }
        macro_rules! set_g {
            ($w:expr, $v:expr) => {
                if use_vec {
                    g_scores_vec[$w.0 as usize] = $v;
                } else {
                    g_scores_map.insert($w, $v);
                }
            };
        }
        macro_rules! is_closed {
            ($w:expr) => {
                if use_vec {
                    closed_vec[$w.0 as usize]
                } else {
                    closed_map.contains(&$w)
                }
            };
        }
        macro_rules! set_closed {
            ($w:expr) => {
                if use_vec {
                    closed_vec[$w.0 as usize] = true;
                } else {
                    closed_map.insert($w);
                }
            };
        }
        macro_rules! set_came_from {
            ($w:expr, $from:expr, $pip:expr) => {
                if use_vec {
                    came_from_vec[$w.0 as usize] = Some(($from, $pip));
                } else {
                    came_from_map.insert($w, ($from, $pip));
                }
            };
        }

        // Initialize with source
        let h = self.heuristic(source, target_x, target_y);
        set_g!(source, 0.0);
        open_set.push(AStarNode {
            wire: source,
            g_score: 0.0,
            f_score: h,
        });

        while let Some(current) = open_set.pop() {
            // Check if we reached the sink
            if current.wire == sink {
                return Ok(if use_vec {
                    self.reconstruct_path_vec(source, sink, &came_from_vec)
                } else {
                    self.reconstruct_path(source, sink, &came_from_map)
                });
            }

            // Skip if already processed
            if is_closed!(current.wire) {
                continue;
            }
            set_closed!(current.wire);

            // Use g-score from the node itself (already computed when pushed)
            let current_g = current.g_score;

            // Expand neighbors via PIPs (forward routing - current wire is source)
            for pip_id in self.device.wire_src_pips(current.wire) {
                let pip = match self.device.pip(pip_id) {
                    Some(p) => p,
                    None => continue,
                };

                let neighbor = pip.dst_wire;

                // Skip blocked wires
                if blocked_set.contains(&neighbor) {
                    continue;
                }

                // Skip already visited
                if is_closed!(neighbor) {
                    continue;
                }

                // Calculate cost with optional timing awareness
                let base_pip_cost = pip.cost() as f64;
                let timing_cost = if timing_weight > 0.0 {
                    if let Some(ref model) = self.delay_model {
                        let src_wire = self.device.wire(current.wire);
                        let dst_wire = self.device.wire(neighbor);
                        let typed_delay = match (src_wire, dst_wire) {
                            (Some(sw), Some(dw)) => {
                                model.pip_delay_typed(&sw.wire_type, &dw.wire_type)
                            }
                            _ => model.pip_delay,
                        };
                        timing_weight * (typed_delay / 0.05)
                    } else {
                        timing_weight * (pip.delay as f64 / 50.0)
                    }
                } else {
                    0.0
                };
                let pip_cost = base_pip_cost + timing_cost;
                let wire_cost = wire_costs.get(&neighbor).copied().unwrap_or(1.0);
                let tentative_g = current_g + pip_cost * wire_cost;

                // Check if this is a better path
                let neighbor_g = get_g!(neighbor);
                if tentative_g < neighbor_g {
                    set_came_from!(neighbor, current.wire, pip_id);
                    set_g!(neighbor, tentative_g);

                    let h = self.heuristic(neighbor, target_x, target_y);
                    let f = tentative_g + h;

                    open_set.push(AStarNode {
                        wire: neighbor,
                        g_score: tentative_g,
                        f_score: f,
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

    /// Wire-type-aware Manhattan distance heuristic.
    ///
    /// Estimates minimum cost to reach the target tile, accounting for
    /// available wire types: span-12 covers 12 tiles/hop, span-4 covers
    /// 4 tiles/hop, local covers 1 tile/hop. This provides a tighter
    /// (but still admissible) heuristic than flat Manhattan distance.
    fn heuristic(&self, wire: WireId, target_x: u32, target_y: u32) -> f64 {
        let wire_info = match self.device.wire(wire) {
            Some(w) => w,
            None => return 0.0,
        };

        let dx = (wire_info.tile_x as i32 - target_x as i32).unsigned_abs();
        let dy = (wire_info.tile_y as i32 - target_y as i32).unsigned_abs();
        let dist = dx + dy;

        if dist == 0 {
            return 0.0;
        }

        // Estimate minimum hops using available wire types:
        // span-12: ~2 cost units per hop, covers 12 tiles
        // span-4: ~2 cost units per hop, covers 4 tiles
        // local: ~1 cost unit per hop, covers 1 tile
        let span12_hops = dist / 12;
        let remaining = dist % 12;
        let span4_hops = remaining / 4;
        let local_hops = remaining % 4;

        // Cost per hop: span-12 PIP ≈ 2.0, span-4 PIP ≈ 2.0, local PIP ≈ 1.5
        // This is admissible (≤ actual cost) since real paths may need more hops
        span12_hops as f64 * 2.0 + span4_hops as f64 * 2.0 + local_hops as f64 * 1.5
    }

    /// Reconstruct path from Vec-based came_from
    fn reconstruct_path_vec(
        &self,
        source: WireId,
        sink: WireId,
        came_from: &[Option<(WireId, PipId)>],
    ) -> (Vec<WireId>, Vec<PipId>) {
        let mut wires = vec![sink];
        let mut pips = Vec::new();
        let mut current = sink;

        while let Some((prev, pip)) = came_from[current.0 as usize] {
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

    /// Reconstruct path from HashMap-based came_from (fallback)
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
