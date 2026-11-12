//! Simulated Annealing Placement Optimization
//!
//! Refines placement using simulated annealing with HPWL cost function.
//! Supports timing-driven placement by incorporating estimated path delays.
//! Includes parallel move evaluation for improved performance on multi-core systems.

use super::congestion::{net_box, CongestionMap, NetBox};
use super::control_sets::{control_sets, fits_control_group, ControlSet};
use super::{is_routed_net, PlacementLoc, PlacementResult};
use crate::collections::{HashMap, HashMapExt, HashSet, HashSetExt};
use crate::device::{BelType, Device};
use crate::error::Result;
use crate::packing::CarryChain;
use crate::timing::DelayModel;
use rand::{Rng, SeedableRng};
use rayon::prelude::*;
use skalp_lir::gate_netlist::{CellId, GateNetId, GateNetlist};
use std::sync::atomic::{AtomicUsize, Ordering};

/// Result of a placement-move cost evaluation:
/// (total delta, per-net (index, new_wl, new_timing) updates, the other
/// cost terms' updates).
type CostDelta = (f64, Vec<(usize, f64, f64)>, SideUpdates);

/// Updates to the cost terms beyond per-net wirelength and timing, kept
/// from a move's evaluation for when it is accepted
#[derive(Default)]
struct SideUpdates {
    /// Proximity (group index, new cost)
    proximity: Vec<(usize, f64)>,
    /// Congestion: each affected net's new box
    net_boxes: Vec<(usize, Option<NetBox>)>,
    /// Congestion: the demand change of each bin those boxes touch
    bin_demand: Vec<(usize, f64)>,
}

/// Weight of routing congestion ([`CongestionMap::total`]) against
/// wirelength. Within capacity congestion costs nothing; past it, a move
/// that adds wire but lowers the overflow cost by more than half as much
/// is taken, so nets detour around hot spots rather than through them.
const CONGESTION_WEIGHT: f64 = 2.0;

/// Probability of accepting an average uphill move at the start of an anneal
/// from a random placement (VPR).
const RANDOM_START_ACCEPTANCE: f64 = 0.95;
/// The same when refining an already good placement: cold enough that the
/// anneal does not scramble it, warm enough to climb out of legalization's
/// local minima.
const REFINE_START_ACCEPTANCE: f64 = 0.1;
/// Initial move window, in tiles, when refining
const REFINE_RLIM: f64 = 3.0;
/// Moves sampled to calibrate the initial temperature
const CALIBRATION_MOVES: usize = 1000;
/// Freeze when the temperature drops below this fraction of the average
/// cost per net (VPR uses 0.005)
const FREEZE_FRACTION: f64 = 0.005;

/// Seed used unless [`SimulatedAnnealing::with_seed`] gives another
/// (the default of [`super::PlacerConfig::seed`])
const DEFAULT_SEED: u64 = 42;

/// Offset of the temperature-calibration stream from the annealer's seed,
/// so calibration draws different moves from the first annealing moves
const CALIBRATION_STREAM: u64 = 12_303;

/// NCL proximity group for placement co-location enforcement.
/// Tracks which cells must be placed close together and the tile distance limit.
struct ProximityGroupInfo {
    cells: Vec<CellId>,
    max_distance_tiles: u32,
}

/// Tracks proximity cost for NCL cell groups.
/// Cost = sum over groups of max(0, actual_max_distance - allowed_distance)².
/// Quadratic penalty drives cells within the allowed distance.
struct ProximityCostTracker {
    groups: Vec<ProximityGroupInfo>,
    /// Cell → which groups it belongs to (indices into `groups`)
    cell_to_groups: HashMap<CellId, Vec<usize>>,
    /// Per-group cost
    group_costs: Vec<f64>,
    /// Total proximity cost
    total_cost: f64,
}

impl ProximityCostTracker {
    fn build(netlist: &GateNetlist, placement: &PlacementResult) -> Self {
        let constraints = netlist.ncl_constraints.as_ref();
        let groups: Vec<ProximityGroupInfo> = constraints
            .map(|c| {
                c.proximity_groups
                    .iter()
                    .map(|pg| ProximityGroupInfo {
                        cells: pg.cells.clone(),
                        max_distance_tiles: pg.max_distance_tiles,
                    })
                    .collect()
            })
            .unwrap_or_default();

        let mut cell_to_groups: HashMap<CellId, Vec<usize>> = HashMap::new();
        for (i, group) in groups.iter().enumerate() {
            for &cell in &group.cells {
                cell_to_groups.entry(cell).or_default().push(i);
            }
        }

        let group_costs: Vec<f64> = groups
            .iter()
            .map(|g| Self::compute_group_cost(g, placement))
            .collect();
        let total_cost = group_costs.iter().sum();

        Self {
            groups,
            cell_to_groups,
            group_costs,
            total_cost,
        }
    }

    fn compute_group_cost(group: &ProximityGroupInfo, placement: &PlacementResult) -> f64 {
        // Max Manhattan distance between any two placed cells in the group
        let locs: Vec<(i32, i32)> = group
            .cells
            .iter()
            .filter_map(|c| {
                placement
                    .placements
                    .get(c)
                    .map(|loc| (loc.tile_x as i32, loc.tile_y as i32))
            })
            .collect();

        if locs.len() < 2 {
            return 0.0;
        }

        let mut max_dist: i32 = 0;
        for i in 0..locs.len() {
            for j in (i + 1)..locs.len() {
                let d = (locs[i].0 - locs[j].0).abs() + (locs[i].1 - locs[j].1).abs();
                max_dist = max_dist.max(d);
            }
        }

        let excess = (max_dist - group.max_distance_tiles as i32).max(0) as f64;
        excess * excess // quadratic penalty
    }

    /// Compute the delta in proximity cost for a new placement.
    /// Only recomputes groups that contain moved cells.
    fn compute_delta(
        &self,
        moved_cells: &[CellId],
        new_placement: &PlacementResult,
    ) -> (f64, Vec<(usize, f64)>) {
        let mut affected_groups: HashSet<usize> = HashSet::new();
        for cell in moved_cells {
            if let Some(gids) = self.cell_to_groups.get(cell) {
                affected_groups.extend(gids);
            }
        }

        let mut delta = 0.0;
        let mut updates = Vec::new();
        for &gid in &affected_groups {
            let new_cost = Self::compute_group_cost(&self.groups[gid], new_placement);
            delta += new_cost - self.group_costs[gid];
            updates.push((gid, new_cost));
        }

        (delta, updates)
    }

    fn apply_updates(&mut self, updates: &[(usize, f64)]) {
        for &(gid, new_cost) in updates {
            self.total_cost += new_cost - self.group_costs[gid];
            self.group_costs[gid] = new_cost;
        }
    }
}

/// Incremental cost cache for O(affected_nets) cost updates instead of O(all_nets).
///
/// Pre-computes per-net HPWL and timing costs, and maintains a cell→nets index
/// so that move evaluation only recomputes costs for nets connected to moved cells.
struct NetCostCache {
    /// Per-net HPWL cost (already weighted by sqrt(fanout))
    net_wl_cost: Vec<f64>,
    /// Per-net timing cost: the sum over its connections of delay times
    /// the connection's criticality²
    net_timing_cost: Vec<f64>,
    /// Criticality of each connection, by net index and then in the order
    /// of the net's fanout; empty when placing for wirelength only
    connection_criticalities: Vec<Vec<f64>>,
    /// Total wirelength cost (sum of net_wl_cost)
    total_wl_cost: f64,
    /// Total timing cost (sum of net_timing_cost)
    total_timing_cost: f64,
    /// Factor that puts timing cost on the scale of wirelength cost: their
    /// ratio in the placement the cache was built for. Timing is in ns and
    /// wirelength in tiles, and a tile of wire costs only tens of ps, so
    /// unscaled the timing term all but vanished next to wirelength whatever
    /// the timing weight (VPR normalises both terms the same way).
    timing_scale: f64,
    /// Cell → list of net indices that the cell participates in
    cell_to_nets: HashMap<CellId, Vec<usize>>,
    /// NCL proximity cost tracker
    proximity: ProximityCostTracker,
    /// Routing demand against supply, on devices that report their routing
    congestion: Option<CongestionMap>,
}

impl NetCostCache {
    /// Build the cache from scratch for a given placement.
    fn build(
        placement: &PlacementResult,
        netlist: &GateNetlist,
        connection_criticalities: Vec<Vec<f64>>,
        delay_model: &DelayModel,
        timing_weight: f64,
    ) -> Self {
        let num_nets = netlist.nets.len();
        let mut net_wl_cost = vec![0.0f64; num_nets];
        let mut net_timing_cost = vec![0.0f64; num_nets];
        let mut cell_to_nets: HashMap<CellId, Vec<usize>> = HashMap::new();

        for (i, net) in netlist.nets.iter().enumerate() {
            // A net placement need not wire costs nothing, and no move
            // touches it
            if !is_routed_net(net, netlist) {
                continue;
            }
            // Build cell_to_nets index
            if let Some(driver) = net.driver {
                cell_to_nets.entry(driver).or_default().push(i);
            }
            for (cell_id, _) in &net.fanout {
                cell_to_nets.entry(*cell_id).or_default().push(i);
            }

            // Compute HPWL + timing in a single pass over the net's cells
            let criticalities = if timing_weight > 0.0 {
                connection_criticalities
                    .get(i)
                    .map_or(&[][..], Vec::as_slice)
            } else {
                &[]
            };
            let (wl, timing) = Self::compute_net_costs(net, placement, delay_model, criticalities);
            net_wl_cost[i] = wl;
            net_timing_cost[i] = timing;
        }

        // Deduplicate net indices per cell (a cell can appear as both driver and fanout)
        for nets in cell_to_nets.values_mut() {
            nets.sort_unstable();
            nets.dedup();
        }

        let total_wl_cost: f64 = net_wl_cost.iter().sum();
        let total_timing_cost: f64 = net_timing_cost.iter().sum();
        let timing_scale = if total_timing_cost > 0.0 {
            total_wl_cost / total_timing_cost
        } else {
            1.0
        };
        let proximity = ProximityCostTracker::build(netlist, placement);

        Self {
            net_wl_cost,
            net_timing_cost,
            connection_criticalities,
            total_wl_cost,
            total_timing_cost,
            timing_scale,
            cell_to_nets,
            proximity,
            congestion: None,
        }
    }

    /// Price timing with new connection criticalities (by net index, in
    /// fanout order), keeping the timing scale set when the cache was built
    /// so costs before and after stay comparable.
    fn retime(
        &mut self,
        placement: &PlacementResult,
        netlist: &GateNetlist,
        connection_criticalities: Vec<Vec<f64>>,
        delay_model: &DelayModel,
    ) {
        self.connection_criticalities = connection_criticalities;
        for (i, net) in netlist.nets.iter().enumerate() {
            if !is_routed_net(net, netlist) {
                continue;
            }
            let criticalities = self
                .connection_criticalities
                .get(i)
                .map_or(&[][..], Vec::as_slice);
            self.net_timing_cost[i] =
                Self::compute_net_costs(net, placement, delay_model, criticalities).1;
        }
        self.total_timing_cost = self.net_timing_cost.iter().sum();
    }

    /// Add a routing congestion term for `device`, if it reports which of
    /// its wires are general routing
    fn with_congestion<D: Device + ?Sized>(
        mut self,
        device: &D,
        netlist: &GateNetlist,
        placement: &PlacementResult,
    ) -> Self {
        self.congestion = CongestionMap::new(device).map(|mut map| {
            map.build(netlist, placement);
            map
        });
        self
    }

    /// Get the combined cost using the same formula as calculate_combined_cost.
    /// Proximity cost is added with a fixed weight — it's a hard constraint penalty,
    /// not a soft trade-off like timing vs wirelength.
    fn combined_cost(&self, timing_weight: f64) -> f64 {
        let base = if timing_weight == 0.0 {
            self.total_wl_cost
        } else {
            (1.0 - timing_weight) * self.total_wl_cost
                + timing_weight * self.timing_scale * self.total_timing_cost
        };
        // Proximity penalty scaled to be comparable to wirelength cost.
        // Weight of 10.0 makes 1-tile excess equivalent to 10 HPWL units.
        base + 10.0 * self.proximity.total_cost
            + self
                .congestion
                .as_ref()
                .map_or(0.0, |c| CONGESTION_WEIGHT * c.total())
    }

    /// Collect the unique set of net indices affected by moving the given cells.
    fn affected_nets(&self, cells: &[CellId]) -> Vec<usize> {
        let mut nets: HashSet<usize> = HashSet::new();
        for cell in cells {
            if let Some(cell_nets) = self.cell_to_nets.get(cell) {
                nets.extend(cell_nets);
            }
        }
        nets.into_iter().collect()
    }

    /// Compute the cost delta for a new placement, updating only affected nets.
    /// Returns (delta, net updates, proximity updates) for later commit.
    #[allow(clippy::too_many_arguments)]
    fn compute_delta(
        &self,
        affected: &[usize],
        moved_cells: &[CellId],
        new_placement: &PlacementResult,
        netlist: &GateNetlist,
        delay_model: &DelayModel,
        timing_weight: f64,
    ) -> CostDelta {
        let mut wl_delta = 0.0;
        let mut timing_delta = 0.0;
        let mut updates = Vec::with_capacity(affected.len());

        for &net_idx in affected {
            let net = &netlist.nets[net_idx];
            let criticalities = if timing_weight > 0.0 {
                self.connection_criticalities
                    .get(net_idx)
                    .map_or(&[][..], Vec::as_slice)
            } else {
                &[]
            };
            let (new_wl, new_timing) =
                Self::compute_net_costs(net, new_placement, delay_model, criticalities);

            wl_delta += new_wl - self.net_wl_cost[net_idx];
            timing_delta += new_timing - self.net_timing_cost[net_idx];
            updates.push((net_idx, new_wl, new_timing));
        }

        let base_delta = if timing_weight == 0.0 {
            wl_delta
        } else {
            (1.0 - timing_weight) * wl_delta + timing_weight * self.timing_scale * timing_delta
        };

        // Proximity cost delta (only recomputes affected groups)
        let (prox_delta, prox_updates) = self.proximity.compute_delta(moved_cells, new_placement);
        let mut delta = base_delta + 10.0 * prox_delta;

        // Congestion: the affected nets' new boxes
        let (mut net_boxes, mut bin_demand) = (Vec::new(), Vec::new());
        if let Some(map) = &self.congestion {
            let boxes: Vec<(usize, Option<NetBox>)> = affected
                .iter()
                .map(|&i| (i, net_box(&netlist.nets[i], new_placement)))
                .collect();
            let (congestion_delta, bins) = map.delta(&boxes);
            delta += CONGESTION_WEIGHT * congestion_delta;
            (net_boxes, bin_demand) = (boxes, bins);
        }

        (
            delta,
            updates,
            SideUpdates {
                proximity: prox_updates,
                net_boxes,
                bin_demand,
            },
        )
    }

    /// Apply cached updates after a move is accepted.
    fn apply_updates(&mut self, net_updates: &[(usize, f64, f64)], side: &SideUpdates) {
        for &(net_idx, new_wl, new_timing) in net_updates {
            self.total_wl_cost += new_wl - self.net_wl_cost[net_idx];
            self.total_timing_cost += new_timing - self.net_timing_cost[net_idx];
            self.net_wl_cost[net_idx] = new_wl;
            self.net_timing_cost[net_idx] = new_timing;
        }
        self.proximity.apply_updates(&side.proximity);
        if let Some(map) = &mut self.congestion {
            map.apply(&side.net_boxes, &side.bin_demand);
        }
    }

    /// Compute HPWL and timing cost for a net in a single pass over its cells.
    /// Returns (wirelength_cost, timing_cost). `criticalities` are the net's
    /// connection criticalities in fanout order (empty: no timing cost);
    /// connections below [`MIN_TIMED_CRITICALITY`] cost no timing.
    fn compute_net_costs(
        net: &skalp_lir::gate_netlist::GateNet,
        placement: &PlacementResult,
        delay_model: &DelayModel,
        criticalities: &[f64],
    ) -> (f64, f64) {
        let mut min_x = f64::MAX;
        let mut max_x = f64::MIN;
        let mut min_y = f64::MAX;
        let mut max_y = f64::MIN;
        let mut connected = 0u32;

        let driver = net.driver.and_then(|d| placement.placements.get(&d));
        if let Some(loc) = driver {
            let x = loc.tile_x as f64;
            let y = loc.tile_y as f64;
            min_x = x;
            max_x = x;
            min_y = y;
            max_y = y;
            connected += 1;
        }

        let mut timing = 0.0;
        for (k, &(cell_id, pin)) in net.fanout.iter().enumerate() {
            if let Some(loc) = placement.placements.get(&cell_id) {
                let x = loc.tile_x as f64;
                let y = loc.tile_y as f64;
                min_x = min_x.min(x);
                max_x = max_x.max(x);
                min_y = min_y.min(y);
                max_y = max_y.max(y);
                connected += 1;

                let criticality = criticalities.get(k).copied().unwrap_or(0.0);
                if let (Some(from), true) = (driver, criticality >= MIN_TIMED_CRITICALITY) {
                    let delay = delay_model.bel_connection_delay(
                        (from.tile_x, from.tile_y, from.bel_index),
                        (loc.tile_x, loc.tile_y, loc.bel_index),
                        pin,
                    );
                    timing += delay * criticality * criticality;
                }
            }
        }

        let wl = if connected >= 2 {
            let hpwl = (max_x - min_x) + (max_y - min_y);
            hpwl * (connected as f64).sqrt()
        } else {
            0.0
        };

        (wl, timing)
    }
}

/// Temperature steps between re-estimates of connection criticality during
/// annealing. Criticality is first estimated on the starting placement; as
/// cells move, paths that were short grow critical and critical ones get
/// slack, and timing cost priced on stale criticality pulls the wrong cells
/// together. Re-timing every 5 steps beat once-only, every 2 and every 20 on
/// a RISC-V board design (8 seeds, LIFCL-40 77.7 -> 80.4 MHz, LFCPNX-100
/// 77.5 -> 79.1 MHz) and added about 4% to annealing time.
const RETIME_INTERVAL: usize = 5;

/// Connections less critical than this carry no timing cost in the annealer:
/// they have slack to spare, and wirelength alone places them well.
const MIN_TIMED_CRITICALITY: f64 = 0.1;

/// Passes of directed detail moves after annealing, criticality re-estimated
/// before each.
const DETAIL_PASSES: usize = 2;

/// Record of changes made by apply_move_in_place, for efficient undo.
struct UndoRecord {
    /// (cell_id, previous_location) — restore these on undo
    placement_changes: Vec<(CellId, PlacementLoc)>,
    /// (key, previous_value) — restore these in loc_map on undo
    /// None means the key didn't exist before (should be removed on undo)
    loc_map_changes: Vec<((u32, u32, usize), Option<CellId>)>,
}

/// Move operation for simulated annealing
#[derive(Debug, Clone)]
enum Move {
    /// Move a cell to a new location; a cell already there takes the
    /// mover's old location (a swap)
    Relocate(CellId, PlacementLoc),
    /// Relocate an entire carry chain to a new column/start position
    RelocateCarryChain(usize, u32, u32), // chain_id, new_x, new_start_y
}

/// Simulated annealing optimizer
pub struct SimulatedAnnealing<'a, D: Device> {
    device: &'a D,
    initial_temp: f64,
    cooling_rate: f64,
    maxiterations: usize,
    /// Timing weight (0.0 = pure wirelength, 1.0 = pure timing)
    timing_weight: f64,
    /// Delay model for timing estimation
    delay_model: DelayModel,
    /// Enable parallel move evaluation
    parallel: bool,
    /// Number of parallel moves to evaluate per batch
    parallel_batch_size: usize,
    /// Carry chain membership: cell_id → chain_id
    carry_chain_membership: HashMap<CellId, usize>,
    /// Carry chains for chain-aware moves
    carry_chains: Vec<CarryChain>,
    /// The initial placement is already good (analytical + legalized):
    /// refine it locally instead of re-annealing from a hot, global start
    refine: bool,
    /// Seed of every random stream the annealer draws from, so a placement
    /// is reproducible for a given seed
    seed: u64,
    /// Share of each tile's BELs of a type cells may take
    /// ([`super::sites::fill_cap`]): a move may not add a cell to a tile
    /// already holding that many
    fill_limit: f64,
}

/// Move-generation data that stays fixed for a whole annealing run, so a
/// move costs O(1) instead of a scan over every cell.
struct MoveContext {
    /// All placed cells a move may take to any compatible BEL
    cells: Vec<CellId>,
    /// Cells whose BEL uses up other BELs of its tile (distributed RAM,
    /// wide LUTs), by BEL type. One moves only by swapping with another of
    /// its type: the two footprints trade places, so every BEL that was
    /// reserved stays reserved.
    footprint_cells: HashMap<BelType, Vec<CellId>>,
    /// Number of cells in `footprint_cells`
    footprint_count: usize,
    /// Cells on critical nets (criticality > 0.5), most critical quarter
    critical: Vec<CellId>,
    /// BELs no move may target: the LUTs under fixed two-bit carry cells
    /// and under distributed-RAM cells
    reserved: HashSet<(u32, u32, usize)>,
    /// Control set of every flip-flop: flip-flops of one control group
    /// ([`Device::ff_control_group`]) must all have the same one
    control_sets: HashMap<CellId, ControlSet>,
}

/// Whether cells of `bel_type` use up other BELs of their tile and so stay
/// where the legalizer put them.
fn has_footprint(bel_type: BelType) -> bool {
    matches!(bel_type, BelType::DistRam | BelType::WideLut)
}

impl MoveContext {
    /// `carry_layout`: on a device whose carry cells take LUTs (Lattice CCU2),
    /// the legalizer has placed every chain on consecutive carry positions;
    /// those chains stay fixed (they are not moved and are not move targets)
    /// and the LUTs under them are reserved.
    ///
    /// Distributed-RAM and wide-LUT cells stay fixed too: their BELs use up
    /// LUTs of their tile (`footprint`, [`Device::bel_footprint`]), which the
    /// legalizer kept free and which no move may then fill.
    fn build(
        placement: &PlacementResult,
        netlist: &GateNetlist,
        net_criticalities: &HashMap<GateNetId, f64>,
        chain_membership: &HashMap<CellId, usize>,
        carry_layout: Option<&crate::device::CarryChainLayout>,
        footprint: &dyn Fn(u32, u32, usize) -> Vec<usize>,
    ) -> Self {
        let mut reserved = HashSet::new();
        for loc in placement.placements.values() {
            if has_footprint(loc.bel_type) {
                reserved.insert((loc.tile_x, loc.tile_y, loc.bel_index));
                for lut in footprint(loc.tile_x, loc.tile_y, loc.bel_index) {
                    reserved.insert((loc.tile_x, loc.tile_y, lut));
                }
            }
        }
        if let Some(layout) = carry_layout {
            for cell in chain_membership.keys() {
                if let Some(loc) = placement.placements.get(cell) {
                    if let Some(slot) = layout.slots.iter().find(|s| s.carry_bel == loc.bel_index) {
                        for &lut in &slot.lut_bels {
                            reserved.insert((loc.tile_x, loc.tile_y, lut));
                        }
                    }
                }
            }
        }
        let movable = |c: &CellId| carry_layout.is_none() || !chain_membership.contains_key(c);
        let cells: Vec<CellId> = placement
            .placements
            .iter()
            .filter(|(c, loc)| !has_footprint(loc.bel_type) && movable(c))
            .map(|(&c, _)| c)
            .collect();
        let mut footprint_cells: HashMap<BelType, Vec<CellId>> = HashMap::new();
        for (c, loc) in &placement.placements {
            if has_footprint(loc.bel_type) && movable(c) {
                footprint_cells.entry(loc.bel_type).or_default().push(*c);
            }
        }
        // Same order as `cells`, which follows the placement map
        for list in footprint_cells.values_mut() {
            list.sort_unstable_by_key(|c| c.0);
        }
        let footprint_count = footprint_cells.values().map(Vec::len).sum();
        let mut critical: Vec<(CellId, f64)> = Vec::new();
        if !net_criticalities.is_empty() {
            for &cell_id in cells.iter().chain(footprint_cells.values().flatten()) {
                if let Some(cell) = netlist.get_cell(cell_id) {
                    let crit = cell
                        .outputs
                        .iter()
                        .chain(cell.inputs.iter())
                        .filter_map(|n| net_criticalities.get(n))
                        .fold(0.0f64, |m, &c| m.max(c));
                    if crit > 0.5 {
                        critical.push((cell_id, crit));
                    }
                }
            }
        }
        critical.sort_by(|a, b| b.1.partial_cmp(&a.1).unwrap_or(std::cmp::Ordering::Equal));
        let top_n = (critical.len() / 4).max(1).min(critical.len());
        Self {
            cells,
            footprint_cells,
            footprint_count,
            critical: critical[..top_n].iter().map(|&(c, _)| c).collect(),
            reserved,
            control_sets: control_sets(netlist),
        }
    }
}

impl<'a, D: Device> SimulatedAnnealing<'a, D> {
    /// Create a new simulated annealing optimizer (wirelength-driven)
    pub fn new(device: &'a D, initial_temp: f64, cooling_rate: f64, maxiterations: usize) -> Self {
        Self {
            device,
            initial_temp,
            cooling_rate,
            maxiterations,
            timing_weight: 0.0, // Pure wirelength mode
            delay_model: DelayModel::ice40_default(),
            parallel: false,
            parallel_batch_size: 64,
            carry_chain_membership: HashMap::new(),
            carry_chains: Vec::new(),
            refine: false,
            seed: DEFAULT_SEED,
            fill_limit: 1.0,
        }
    }

    /// Create a timing-driven simulated annealing optimizer
    pub fn new_timing_driven(
        device: &'a D,
        initial_temp: f64,
        cooling_rate: f64,
        maxiterations: usize,
        timing_weight: f64,
    ) -> Self {
        Self {
            device,
            initial_temp,
            cooling_rate,
            maxiterations,
            timing_weight: timing_weight.clamp(0.0, 1.0),
            delay_model: DelayModel::ice40_default(),
            parallel: false,
            parallel_batch_size: 64,
            carry_chain_membership: HashMap::new(),
            carry_chains: Vec::new(),
            refine: false,
            seed: DEFAULT_SEED,
            fill_limit: 1.0,
        }
    }

    /// Fill each tile's BELs of a type at most this far (0..=1)
    pub fn with_fill_limit(mut self, fill_limit: f64) -> Self {
        self.fill_limit = fill_limit;
        self
    }

    /// Seed the annealer's random moves and acceptances
    pub fn with_seed(mut self, seed: u64) -> Self {
        self.seed = seed;
        self
    }

    /// Set carry chains for chain-aware moves
    pub fn with_carry_chains(mut self, carry_chains: Vec<CarryChain>) -> Self {
        for chain in &carry_chains {
            for &cell_id in &chain.cells {
                self.carry_chain_membership.insert(cell_id, chain.chain_id);
            }
        }
        self.carry_chains = carry_chains;
        self
    }

    /// Treat the initial placement as already good (e.g. analytical placement
    /// followed by legalization). The anneal then starts cold, with a small
    /// move window, so it refines that placement instead of first scrambling
    /// it: from a hot start with chip-wide moves, a 15K-cell Nexus design
    /// never got back to its legalized starting cost.
    pub fn with_refinement(mut self, refine: bool) -> Self {
        self.refine = refine;
        self
    }

    /// Create with a specific delay model
    pub fn with_delay_model(mut self, delay_model: DelayModel) -> Self {
        self.delay_model = delay_model;
        self
    }

    /// Enable parallel move evaluation
    pub fn with_parallel(mut self, parallel: bool) -> Self {
        self.parallel = parallel;
        self
    }

    /// Set parallel batch size (number of moves to evaluate in parallel)
    pub fn with_batch_size(mut self, batch_size: usize) -> Self {
        self.parallel_batch_size = batch_size.max(1);
        self
    }

    /// Optimize a placement using simulated annealing
    pub fn optimize(
        &self,
        initial: PlacementResult,
        netlist: &GateNetlist,
    ) -> Result<PlacementResult> {
        if self.parallel {
            self.optimize_parallel(initial, netlist)
        } else {
            self.optimize_serial(initial, netlist)
        }
    }

    /// Initial temperature: the average uphill cost delta of sample moves,
    /// scaled so such a move is accepted with probability
    /// [`RANDOM_START_ACCEPTANCE`] from a random start (VPR) or
    /// [`REFINE_START_ACCEPTANCE`] when refining a good placement.
    #[allow(clippy::too_many_arguments)]
    fn calibrate_temperature(
        &self,
        placement: &PlacementResult,
        loc_map: &HashMap<(u32, u32, usize), CellId>,
        ctx: &MoveContext,
        cost_cache: &NetCostCache,
        netlist: &GateNetlist,
        rlim: u32,
    ) -> f64 {
        let mut rng = rand::rngs::StdRng::seed_from_u64(self.seed.wrapping_add(CALIBRATION_STREAM));
        let mut cal_placement = placement.clone();
        let mut cal_loc_map = loc_map.clone();
        let mut uphill_sum = 0.0f64;
        let mut uphill_count = 0u32;
        for _ in 0..CALIBRATION_MOVES.min(ctx.cells.len() * 2) {
            let move_op = self.generate_move(&cal_placement, ctx, &cal_loc_map, rlim, &mut rng);
            let affected = self.moved_cells_with_displaced(&move_op, &cal_placement, &cal_loc_map);
            let undo = self.apply_move_in_place(&mut cal_placement, &mut cal_loc_map, &move_op);
            if !self.control_sets_hold(ctx, &affected, &cal_placement, &cal_loc_map) {
                Self::undo_move(&mut cal_placement, &mut cal_loc_map, undo);
                continue;
            }
            let affected_nets = cost_cache.affected_nets(&affected);
            let (delta, _, _) = cost_cache.compute_delta(
                &affected_nets,
                &affected,
                &cal_placement,
                netlist,
                &self.delay_model,
                self.timing_weight,
            );
            // Calibration only measures deltas; it keeps no move
            Self::undo_move(&mut cal_placement, &mut cal_loc_map, undo);
            if delta > 0.0 {
                uphill_sum += delta;
                uphill_count += 1;
            }
        }
        if uphill_count == 0 {
            return self.initial_temp;
        }
        let target = if self.refine {
            REFINE_START_ACCEPTANCE
        } else {
            RANDOM_START_ACCEPTANCE
        };
        // exp(-avg / T) = target
        (uphill_sum / uphill_count as f64) / -target.ln()
    }

    /// Initial move window in tiles: the whole device from a random start,
    /// a few tiles when refining.
    fn initial_rlim(&self) -> f64 {
        let (width, height) = self.device.grid_size();
        let max = width.max(height).max(1) as f64;
        if self.refine {
            REFINE_RLIM.min(max)
        } else {
            max
        }
    }

    /// VPR range limit update: grow the window while more than 44% of moves
    /// are accepted, shrink it below that.
    fn next_rlim(&self, rlim: f64, acceptance_rate: f64) -> f64 {
        let (width, height) = self.device.grid_size();
        let max = width.max(height).max(1) as f64;
        (rlim * (1.0 - 0.44 + acceptance_rate)).clamp(1.0, max)
    }

    /// VPR adaptive cooling: cool fast while nearly every move is accepted
    /// (or almost none are), and at the configured rate in the productive
    /// middle band.
    fn next_temperature(&self, temperature: f64, acceptance_rate: f64) -> f64 {
        let alpha = if acceptance_rate > 0.96 {
            0.5
        } else if acceptance_rate > 0.8 {
            0.9
        } else if acceptance_rate > 0.15 {
            self.cooling_rate
        } else {
            0.8
        };
        temperature * alpha
    }

    /// VPR exit criterion: the temperature is a small fraction of the average
    /// cost per net, so no uphill move of consequence would still be taken.
    fn is_frozen(temperature: f64, cost: f64, netlist: &GateNetlist) -> bool {
        temperature < FREEZE_FRACTION * cost / netlist.nets.len().max(1) as f64
    }

    /// Serial simulated annealing optimization
    fn optimize_serial(
        &self,
        initial: PlacementResult,
        netlist: &GateNetlist,
    ) -> Result<PlacementResult> {
        let mut rng = rand::rngs::StdRng::seed_from_u64(self.seed);
        let mut current = initial;

        // Compute net criticalities for timing-driven mode
        let (net_criticalities, connection_criticalities) = if self.timing_weight > 0.0 {
            self.compute_criticalities(netlist, &current)
        } else {
            (HashMap::new(), Vec::new())
        };

        // Build incremental cost cache: O(nets) once, then O(affected) per move
        let mut cost_cache = NetCostCache::build(
            &current,
            netlist,
            connection_criticalities,
            &self.delay_model,
            self.timing_weight,
        )
        .with_congestion(self.device, netlist, &current);
        let mut current_cost = cost_cache.combined_cost(self.timing_weight);
        let carry_layout = self.device.carry_chain_layout();
        let move_ctx = MoveContext::build(
            &current,
            netlist,
            &net_criticalities,
            &self.carry_chain_membership,
            carry_layout.as_ref(),
            &|x, y, bel| self.device.bel_footprint(x, y, bel),
        );
        if move_ctx.cells.is_empty() {
            current.cost = current_cost;
            return Ok(current);
        }

        // Reverse mapping: location -> cell_id
        let mut location_to_cell: HashMap<(u32, u32, usize), CellId> = current
            .placements
            .iter()
            .map(|(&cid, loc)| ((loc.tile_x, loc.tile_y, loc.bel_index), cid))
            .collect();

        let mut rlim = self.initial_rlim();
        let mut temperature = self.calibrate_temperature(
            &current,
            &location_to_cell,
            &move_ctx,
            &cost_cache,
            netlist,
            rlim.round() as u32,
        );
        let mut best = current.clone();
        let mut best_cost = current_cost;

        let cells = &move_ctx.cells;
        let moves_per_temp = cells.len().max(100);

        // Scale iterations to design size — tiny designs converge quickly
        let effectiveiterations = self.maxiterations.min(cells.len() * 100 + 100);

        for iteration in 0..effectiveiterations {
            let mut accepted_moves = 0u32;
            let window = rlim.round() as u32;

            for _ in 0..moves_per_temp {
                // Generate a random move (biased toward critical cells in timing mode)
                let move_op = if self.timing_weight > 0.0 && rng.gen::<f64>() < 0.3 {
                    self.generate_critical_path_move(
                        &current,
                        &move_ctx,
                        &location_to_cell,
                        window,
                        &mut rng,
                    )
                } else {
                    self.generate_move(&current, &move_ctx, &location_to_cell, window, &mut rng)
                };

                // Determine affected cells before mutation
                let affected =
                    self.moved_cells_with_displaced(&move_op, &current, &location_to_cell);

                // Apply move in-place (no clone), get undo record
                let undo = self.apply_move_in_place(&mut current, &mut location_to_cell, &move_op);
                if !self.control_sets_hold(&move_ctx, &affected, &current, &location_to_cell) {
                    Self::undo_move(&mut current, &mut location_to_cell, undo);
                    continue;
                }

                // Incremental cost delta: only recompute nets connected to moved cells
                let affected_nets = cost_cache.affected_nets(&affected);
                let (delta, net_updates, prox_updates) = cost_cache.compute_delta(
                    &affected_nets,
                    &affected,
                    &current,
                    netlist,
                    &self.delay_model,
                    self.timing_weight,
                );

                let accept = delta <= 0.0 || rng.gen::<f64>() < (-delta / temperature).exp();
                if accept {
                    accepted_moves += 1;
                    cost_cache.apply_updates(&net_updates, &prox_updates);
                    current_cost = cost_cache.combined_cost(self.timing_weight);
                } else {
                    Self::undo_move(&mut current, &mut location_to_cell, undo);
                }
            }

            // Snapshot the best placement once per temperature step. Cloning
            // the whole placement on every improving move made each early
            // (mostly improving) move O(cells): quadratic on large designs.
            if current_cost < best_cost {
                best = current.clone();
                best_cost = current_cost;
            }

            let acceptance_rate = accepted_moves as f64 / moves_per_temp as f64;
            if iteration % 10 == 0 {
                log::debug!(
                    "anneal step {}: T={:.4} window {} accepted {:.1}% cost {:.1} (best {:.1})",
                    iteration,
                    temperature,
                    window,
                    100.0 * acceptance_rate,
                    current_cost,
                    best_cost
                );
            }
            if acceptance_rate < 0.001 || Self::is_frozen(temperature, current_cost, netlist) {
                log::debug!(
                    "anneal frozen after {} steps: cost {:.1} (best {:.1})",
                    iteration + 1,
                    current_cost,
                    best_cost
                );
                break;
            }
            temperature = self.next_temperature(temperature, acceptance_rate);
            rlim = self.next_rlim(rlim, acceptance_rate);

            if self.timing_weight > 0.0 && (iteration + 1) % RETIME_INTERVAL == 0 {
                // Cells have moved since criticality was last estimated:
                // re-time, and restart the best-so-far from here, since
                // costs under the old criticalities no longer compare.
                let (_, connections) = self.compute_criticalities(netlist, &current);
                cost_cache.retime(&current, netlist, connections, &self.delay_model);
                current_cost = cost_cache.combined_cost(self.timing_weight);
                best = current.clone();
                best_cost = current_cost;
            }
        }

        Ok(self.finish(
            current,
            current_cost,
            best,
            best_cost,
            cost_cache,
            &move_ctx,
            netlist,
        ))
    }

    /// Parallel simulated annealing optimization
    ///
    /// Evaluates multiple moves in parallel and selects the best improving moves.
    /// This provides speedup on multi-core systems while maintaining solution quality.
    fn optimize_parallel(
        &self,
        initial: PlacementResult,
        netlist: &GateNetlist,
    ) -> Result<PlacementResult> {
        let mut current = initial;

        // Compute net criticalities for timing-driven mode
        let (net_criticalities, connection_criticalities) = if self.timing_weight > 0.0 {
            self.compute_criticalities(netlist, &current)
        } else {
            (HashMap::new(), Vec::new())
        };

        // Build incremental cost cache
        let mut cost_cache = NetCostCache::build(
            &current,
            netlist,
            connection_criticalities,
            &self.delay_model,
            self.timing_weight,
        )
        .with_congestion(self.device, netlist, &current);
        let mut current_cost = cost_cache.combined_cost(self.timing_weight);
        let mut best = current.clone();
        let mut best_cost = current_cost;

        let carry_layout = self.device.carry_chain_layout();
        let move_ctx = MoveContext::build(
            &current,
            netlist,
            &net_criticalities,
            &self.carry_chain_membership,
            carry_layout.as_ref(),
            &|x, y, bel| self.device.bel_footprint(x, y, bel),
        );
        if move_ctx.cells.is_empty() {
            current.cost = current_cost;
            return Ok(current);
        }

        // Build reverse mapping: location -> cell_id
        let mut location_to_cell: HashMap<(u32, u32, usize), CellId> = HashMap::new();
        for (&cell_id, loc) in &current.placements {
            location_to_cell.insert((loc.tile_x, loc.tile_y, loc.bel_index), cell_id);
        }

        let mut rlim = self.initial_rlim();
        let mut temperature = self.calibrate_temperature(
            &current,
            &location_to_cell,
            &move_ctx,
            &cost_cache,
            netlist,
            rlim.round() as u32,
        );

        let cells = &move_ctx.cells;
        let batch_size = self.parallel_batch_size;
        let batches_per_temp = (cells.len().max(100) / batch_size).max(1);

        // Scale iterations to design size — tiny designs converge quickly
        let effectiveiterations = self.maxiterations.min(cells.len() * 100 + 100);

        // Thread-safe counter for accepted moves (for statistics)
        let accepted_count = AtomicUsize::new(0);

        for iteration in 0..effectiveiterations {
            let window = rlim.round() as u32;
            let mut accepted_batches = 0usize;
            // Process batches at this temperature
            for _ in 0..batches_per_temp {
                // Generate a batch of candidate moves using different seeds
                let moves: Vec<Move> = (0..batch_size)
                    .into_par_iter()
                    .map(|i| {
                        let mut rng = rand::rngs::StdRng::seed_from_u64(
                            self.seed
                                .wrapping_add(iteration as u64 * 1000)
                                .wrapping_add(i as u64),
                        );
                        if self.timing_weight > 0.0 && rng.gen::<f64>() < 0.3 {
                            self.generate_critical_path_move(
                                &current,
                                &move_ctx,
                                &location_to_cell,
                                window,
                                &mut rng,
                            )
                        } else {
                            self.generate_move(
                                &current,
                                &move_ctx,
                                &location_to_cell,
                                window,
                                &mut rng,
                            )
                        }
                    })
                    .collect();

                // Evaluate all moves in parallel using incremental cost
                #[allow(clippy::type_complexity)]
                let evaluated: Vec<(
                    Move,
                    PlacementResult,
                    HashMap<(u32, u32, usize), CellId>,
                    f64,
                    Vec<(usize, f64, f64)>,
                    SideUpdates,
                )> = moves
                    .into_par_iter()
                    .map(|move_op| {
                        let (new_placement, new_loc_map) =
                            self.apply_move(&current, &location_to_cell, &move_op);
                        let affected =
                            self.moved_cells_with_displaced(&move_op, &current, &location_to_cell);
                        if !self.control_sets_hold(
                            &move_ctx,
                            &affected,
                            &new_placement,
                            &new_loc_map,
                        ) {
                            // An illegal move: never accepted
                            return (
                                move_op,
                                new_placement,
                                new_loc_map,
                                f64::INFINITY,
                                Vec::new(),
                                SideUpdates::default(),
                            );
                        }
                        let affected_nets = cost_cache.affected_nets(&affected);
                        let (delta, net_updates, prox_updates) = cost_cache.compute_delta(
                            &affected_nets,
                            &affected,
                            &new_placement,
                            netlist,
                            &self.delay_model,
                            self.timing_weight,
                        );
                        (
                            move_op,
                            new_placement,
                            new_loc_map,
                            delta,
                            net_updates,
                            prox_updates,
                        )
                    })
                    .collect();

                // Find the best improving move (or best move if temperature allows)
                let mut best_move_idx: Option<usize> = None;
                let mut best_delta = f64::MAX;
                let mut rng =
                    rand::rngs::StdRng::seed_from_u64(self.seed.wrapping_add(iteration as u64));

                for (idx, (_, _, _, delta, _, _)) in evaluated.iter().enumerate() {
                    // Check acceptance
                    let accept = if *delta <= 0.0 {
                        true
                    } else {
                        let probability = (-*delta / temperature).exp();
                        rng.gen::<f64>() < probability
                    };

                    if accept && *delta < best_delta {
                        best_move_idx = Some(idx);
                        best_delta = *delta;
                    }
                }

                // Apply the best accepted move
                if let Some(idx) = best_move_idx {
                    let (_, new_placement, new_loc_map, _, net_updates, prox_updates) =
                        evaluated.into_iter().nth(idx).unwrap();
                    current = new_placement;
                    location_to_cell = new_loc_map;
                    cost_cache.apply_updates(&net_updates, &prox_updates);
                    current_cost = cost_cache.combined_cost(self.timing_weight);
                    accepted_count.fetch_add(1, Ordering::Relaxed);
                    accepted_batches += 1;
                }
            }

            // Snapshot the best placement once per temperature step
            if current_cost < best_cost {
                best = current.clone();
                best_cost = current_cost;
            }

            let acceptance_rate = accepted_batches as f64 / batches_per_temp as f64;
            if acceptance_rate < 0.001 || Self::is_frozen(temperature, current_cost, netlist) {
                break;
            }
            if self.timing_weight > 0.0 && (iteration + 1) % RETIME_INTERVAL == 0 {
                let (_, connections) = self.compute_criticalities(netlist, &current);
                cost_cache.retime(&current, netlist, connections, &self.delay_model);
                current_cost = cost_cache.combined_cost(self.timing_weight);
                best = current.clone();
                best_cost = current_cost;
            }
            temperature = self.next_temperature(temperature, acceptance_rate);
            rlim = self.next_rlim(rlim, acceptance_rate);
        }

        Ok(self.finish(
            current,
            current_cost,
            best,
            best_cost,
            cost_cache,
            &move_ctx,
            netlist,
        ))
    }

    /// End annealing: take the better of the current and best placements,
    /// make the directed moves of [`super::detail`] on it, and record its
    /// cost and timing score.
    #[allow(clippy::too_many_arguments)]
    fn finish(
        &self,
        mut current: PlacementResult,
        current_cost: f64,
        best: PlacementResult,
        best_cost: f64,
        mut cost_cache: NetCostCache,
        ctx: &MoveContext,
        netlist: &GateNetlist,
    ) -> PlacementResult {
        if best_cost < current_cost {
            // The cache follows `current`: rebuild it for `best`, on the
            // same timing scale and criticalities that priced it
            let scale = cost_cache.timing_scale;
            let criticalities = std::mem::take(&mut cost_cache.connection_criticalities);
            cost_cache = NetCostCache::build(
                &best,
                netlist,
                criticalities,
                &self.delay_model,
                self.timing_weight,
            )
            .with_congestion(self.device, netlist, &best);
            cost_cache.timing_scale = scale;
            current = best;
        }
        let mut loc_map: HashMap<(u32, u32, usize), CellId> = current
            .placements
            .iter()
            .map(|(&cid, loc)| ((loc.tile_x, loc.tile_y, loc.bel_index), cid))
            .collect();
        for pass in 0..DETAIL_PASSES {
            if self.timing_weight > 0.0 {
                let (_, connections) = self.compute_criticalities(netlist, &current);
                cost_cache.retime(&current, netlist, connections, &self.delay_model);
            }
            let before = cost_cache.combined_cost(self.timing_weight);
            let moved =
                self.refine_directed(&mut current, &mut loc_map, ctx, &mut cost_cache, netlist);
            log::debug!(
                "detail pass {}: {} cells moved, cost {:.1} -> {:.1}",
                pass + 1,
                moved,
                before,
                cost_cache.combined_cost(self.timing_weight)
            );
            if moved == 0 {
                break;
            }
        }
        current.cost = cost_cache.combined_cost(self.timing_weight);
        if self.timing_weight > 0.0 {
            current.timing_score = self.estimate_timing_score(&current, netlist);
        }
        current
    }

    /// Try each move [`super::detail::candidates`] names, in its order: put
    /// the cell on whichever acceptable BEL of the named tiles lowers the
    /// cost most, if any does. Returns how many cells moved.
    fn refine_directed(
        &self,
        placement: &mut PlacementResult,
        loc_map: &mut HashMap<(u32, u32, usize), CellId>,
        ctx: &MoveContext,
        cost_cache: &mut NetCostCache,
        netlist: &GateNetlist,
    ) -> usize {
        let movable: HashSet<CellId> = ctx.cells.iter().copied().collect();
        let candidates =
            super::detail::candidates(netlist, placement, &cost_cache.connection_criticalities);
        let mut moved = 0;
        for candidate in candidates {
            let cell = candidate.cell;
            if !movable.contains(&cell) {
                continue;
            }
            let bel_type = placement.placements[&cell].bel_type;
            let mut chosen: Option<(f64, PlacementLoc)> = None;
            for &(x, y) in &candidate.tiles {
                for bel in self.acceptable_bels(placement, ctx, loc_map, cell, x, y) {
                    let target = PlacementLoc::new(x, y, bel, bel_type);
                    let Some((delta, _, _)) = self.try_move(
                        &Move::Relocate(cell, target),
                        placement,
                        loc_map,
                        ctx,
                        cost_cache,
                        netlist,
                        false,
                    ) else {
                        continue;
                    };
                    if chosen.is_none_or(|(best, _)| delta < best) {
                        chosen = Some((delta, target));
                    }
                }
            }
            if let Some((delta, target)) = chosen {
                if delta < -1e-9 {
                    let (_, net_updates, side) = self
                        .try_move(
                            &Move::Relocate(cell, target),
                            placement,
                            loc_map,
                            ctx,
                            cost_cache,
                            netlist,
                            true,
                        )
                        .expect("the move held its control sets a moment ago");
                    cost_cache.apply_updates(&net_updates, &side);
                    moved += 1;
                }
            }
        }
        moved
    }

    /// Price `move_op`: its cost delta, or `None` when it would break a
    /// control group. The placement is left moved if `keep`, else as it was.
    #[allow(clippy::too_many_arguments)]
    fn try_move(
        &self,
        move_op: &Move,
        placement: &mut PlacementResult,
        loc_map: &mut HashMap<(u32, u32, usize), CellId>,
        ctx: &MoveContext,
        cost_cache: &NetCostCache,
        netlist: &GateNetlist,
        keep: bool,
    ) -> Option<CostDelta> {
        let affected = self.moved_cells_with_displaced(move_op, placement, loc_map);
        let undo = self.apply_move_in_place(placement, loc_map, move_op);
        if !self.control_sets_hold(ctx, &affected, placement, loc_map) {
            Self::undo_move(placement, loc_map, undo);
            return None;
        }
        let affected_nets = cost_cache.affected_nets(&affected);
        let delta = cost_cache.compute_delta(
            &affected_nets,
            &affected,
            placement,
            netlist,
            &self.delay_model,
            self.timing_weight,
        );
        if !keep {
            Self::undo_move(placement, loc_map, undo);
        }
        Some(delta)
    }

    /// Generate a random move: a random cell goes to a random compatible BEL
    /// within `rlim` tiles of where it is (swapping with any occupant).
    fn generate_move<R: Rng>(
        &self,
        placement: &PlacementResult,
        ctx: &MoveContext,
        loc_map: &HashMap<(u32, u32, usize), CellId>,
        rlim: u32,
        rng: &mut R,
    ) -> Move {
        let cells = &ctx.cells;
        let pick = rng.gen_range(0..cells.len() + ctx.footprint_count);
        let Some(&cell) = cells.get(pick) else {
            let mut index = pick - cells.len();
            for list in ctx.footprint_cells.values() {
                if index < list.len() {
                    return self.generate_footprint_swap(placement, ctx, list[index], rlim, rng);
                }
                index -= list.len();
            }
            unreachable!("footprint_count counts footprint_cells");
        };

        // A carry chain cell on a device without a fixed chain layout: half
        // the time move the whole chain
        if self.carry_chain_membership.contains_key(&cell) && rng.gen::<f64>() < 0.5 {
            let chain_id = self.carry_chain_membership[&cell];
            let (width, height) = self.device.grid_size();
            let new_x = rng.gen_range(1..width.saturating_sub(1).max(2));
            let new_y = rng.gen_range(1..height.saturating_sub(1).max(2));
            return Move::RelocateCarryChain(chain_id, new_x, new_y);
        }

        self.generate_window_move(placement, ctx, loc_map, cell, rlim, rng)
    }

    /// Move `cell_id` to a random compatible, unreserved BEL in a random tile
    /// within `rlim` tiles (VPR-style range limit). If that BEL is taken, the
    /// occupant moves to `cell_id`'s old BEL, so the occupant must be movable
    /// and fit there. Returns a no-op move if no such BEL turns up.
    fn generate_window_move<R: Rng>(
        &self,
        placement: &PlacementResult,
        ctx: &MoveContext,
        loc_map: &HashMap<(u32, u32, usize), CellId>,
        cell_id: CellId,
        rlim: u32,
        rng: &mut R,
    ) -> Move {
        let loc = placement.placements[&cell_id];
        let (width, height) = self.device.grid_size();
        let r = rlim.max(1) as i64;

        const ATTEMPTS: usize = 8;
        for _ in 0..ATTEMPTS {
            let x = (loc.tile_x as i64 + rng.gen_range(-r..=r)).clamp(0, width as i64 - 1) as u32;
            let y = (loc.tile_y as i64 + rng.gen_range(-r..=r)).clamp(0, height as i64 - 1) as u32;
            // Reservoir-sample one acceptable BEL of the tile
            let mut chosen = None;
            let acceptable = self.acceptable_bels(placement, ctx, loc_map, cell_id, x, y);
            for (seen, bel_idx) in (1u32..).zip(acceptable) {
                if rng.gen_range(0..seen) == 0 {
                    chosen = Some(bel_idx);
                }
            }
            if let Some(bel_idx) = chosen {
                // Keep the cell's own requirement as its bel_type
                return Move::Relocate(cell_id, PlacementLoc::new(x, y, bel_idx, loc.bel_type));
            }
        }
        Move::Relocate(cell_id, loc)
    }

    /// The BELs of tile (x, y) that `cell_id` may move to: compatible, not
    /// reserved, not its own, within the tile fill cap, and, when taken, held
    /// by a movable occupant that fits `cell_id`'s old BEL. Flip-flops keep
    /// their control groups to one control set either way.
    fn acceptable_bels(
        &self,
        placement: &PlacementResult,
        ctx: &MoveContext,
        loc_map: &HashMap<(u32, u32, usize), CellId>,
        cell_id: CellId,
        x: u32,
        y: u32,
    ) -> Vec<usize> {
        let loc = placement.placements[&cell_id];
        let Some(tile) = self.device.tile_at(x, y) else {
            return Vec::new();
        };
        let source_bel_type = self
            .device
            .tile_at(loc.tile_x, loc.tile_y)
            .and_then(|t| t.bels().get(loc.bel_index).map(|b| b.bel_type))
            .unwrap_or(loc.bel_type);
        // Moving into another tile adds a cell to it, unless it swaps with
        // one: a tile at its fill cap only takes swaps
        let tile_full = self.fill_limit < 1.0
            && (x, y) != (loc.tile_x, loc.tile_y)
            && self.tile_at_fill_cap(tile, x, y, source_bel_type, loc_map, ctx);
        let mut bels = Vec::new();
        for (bel_idx, bel) in tile.bels().iter().enumerate() {
            if (x, y, bel_idx) == (loc.tile_x, loc.tile_y, loc.bel_index)
                || !Self::bel_types_compatible(bel.bel_type, loc.bel_type)
                || ctx.reserved.contains(&(x, y, bel_idx))
            {
                continue;
            }
            let occupant = loc_map.get(&(x, y, bel_idx));
            if occupant.is_none() && tile_full {
                continue;
            }
            if let Some(&occupant) = occupant {
                if self.carry_chain_membership.contains_key(&occupant)
                    || !Self::bel_types_compatible(
                        source_bel_type,
                        placement.placements[&occupant].bel_type,
                    )
                {
                    continue;
                }
            }
            // Flip-flops share a control group only with their own control
            // set: the cell at its new BEL, and any occupant swapped back to
            // the cell's old one
            let lookup = |x: u32, y: u32, b: usize| loc_map.get(&(x, y, b)).copied();
            let target = PlacementLoc::new(x, y, bel_idx, loc.bel_type);
            let leaving: Vec<CellId> = occupant.copied().into_iter().collect();
            if !fits_control_group(
                self.device,
                &ctx.control_sets,
                cell_id,
                &target,
                &leaving,
                lookup,
            ) || occupant.is_some_and(|&o| {
                !fits_control_group(self.device, &ctx.control_sets, o, &loc, &[cell_id], lookup)
            }) {
                continue;
            }
            bels.push(bel_idx);
        }
        bels
    }

    /// Whether every flip-flop a move touched (`affected`) shares its control
    /// group, after the move (`placement`, `loc_map`), only with flip-flops
    /// of its own control set.
    fn control_sets_hold(
        &self,
        ctx: &MoveContext,
        affected: &[CellId],
        placement: &PlacementResult,
        loc_map: &HashMap<(u32, u32, usize), CellId>,
    ) -> bool {
        affected.iter().all(|&cell| {
            placement.placements.get(&cell).is_none_or(|loc| {
                fits_control_group(self.device, &ctx.control_sets, cell, loc, &[], |x, y, b| {
                    loc_map.get(&(x, y, b)).copied()
                })
            })
        })
    }

    /// Whether tile (x, y) already holds as many cells on BELs of the
    /// moving cell's BEL type (`bel_type`) as the fill limit allows. BELs
    /// reserved under carry chains and distributed RAM count as held.
    fn tile_at_fill_cap(
        &self,
        tile: &dyn crate::device::Tile,
        x: u32,
        y: u32,
        bel_type: BelType,
        loc_map: &HashMap<(u32, u32, usize), CellId>,
        ctx: &MoveContext,
    ) -> bool {
        let (mut total, mut held) = (0usize, 0usize);
        for (i, bel) in tile.bels().iter().enumerate() {
            if bel.bel_type != bel_type {
                continue;
            }
            total += 1;
            if loc_map.contains_key(&(x, y, i)) || ctx.reserved.contains(&(x, y, i)) {
                held += 1;
            }
        }
        held >= super::sites::fill_cap(total, self.fill_limit)
    }

    /// Get all cells affected by a move, including displaced occupants.
    fn moved_cells_with_displaced(
        &self,
        move_op: &Move,
        placement: &PlacementResult,
        loc_map: &HashMap<(u32, u32, usize), CellId>,
    ) -> Vec<CellId> {
        match move_op {
            Move::Relocate(cell_id, new_loc) => {
                let new_key = (new_loc.tile_x, new_loc.tile_y, new_loc.bel_index);
                if let Some(&occupant) = loc_map.get(&new_key) {
                    if occupant != *cell_id {
                        return vec![*cell_id, occupant];
                    }
                }
                vec![*cell_id]
            }
            Move::RelocateCarryChain(chain_id, new_x, new_start_y) => {
                let mut cells = Vec::new();
                if let Some(chain) = self.carry_chains.get(*chain_id) {
                    let (_, height) = self.device.grid_size();
                    let chain_cell_set: HashSet<CellId> = chain.cells.iter().copied().collect();
                    for (i, &cell_id) in chain.cells.iter().enumerate() {
                        cells.push(cell_id);
                        // Check for displaced occupants
                        let tile_y =
                            (*new_start_y + (i as u32 / 8)).min(height.saturating_sub(2).max(1));
                        let bel_idx = i % 8;
                        let key = (*new_x, tile_y, bel_idx);
                        if let Some(&occupant) = loc_map.get(&key) {
                            if !chain_cell_set.contains(&occupant)
                                && placement.placements.contains_key(&occupant)
                            {
                                cells.push(occupant);
                            }
                        }
                    }
                }
                cells
            }
        }
    }

    /// Apply a move to create a new placement
    fn apply_move(
        &self,
        placement: &PlacementResult,
        loc_map: &HashMap<(u32, u32, usize), CellId>,
        move_op: &Move,
    ) -> (PlacementResult, HashMap<(u32, u32, usize), CellId>) {
        let mut new_placement = placement.clone();
        let mut new_loc_map = loc_map.clone();

        match move_op {
            Move::Relocate(cell_id, new_loc) => {
                let old_loc = placement.placements[cell_id];

                // Check if new location is occupied
                let new_key = (new_loc.tile_x, new_loc.tile_y, new_loc.bel_index);
                if let Some(&occupant) = loc_map.get(&new_key) {
                    if occupant != *cell_id {
                        // Swap with occupant, preserving each cell's bel_type
                        new_placement.placements.insert(*cell_id, *new_loc);
                        let occupant_bel_type = placement.placements[&occupant].bel_type;
                        let displaced_loc = PlacementLoc::new(
                            old_loc.tile_x,
                            old_loc.tile_y,
                            old_loc.bel_index,
                            occupant_bel_type,
                        );
                        new_placement.placements.insert(occupant, displaced_loc);

                        let old_key = (old_loc.tile_x, old_loc.tile_y, old_loc.bel_index);
                        new_loc_map.insert(new_key, *cell_id);
                        new_loc_map.insert(old_key, occupant);
                    }
                } else {
                    // Simple move to empty location
                    new_placement.placements.insert(*cell_id, *new_loc);

                    let old_key = (old_loc.tile_x, old_loc.tile_y, old_loc.bel_index);
                    new_loc_map.remove(&old_key);
                    new_loc_map.insert(new_key, *cell_id);
                }
            }
            Move::RelocateCarryChain(chain_id, new_x, new_start_y) => {
                // Move entire carry chain to new column/start position
                if let Some(chain) = self.carry_chains.get(*chain_id) {
                    let (_, height) = self.device.grid_size();
                    for (i, &cell_id) in chain.cells.iter().enumerate() {
                        if let Some(&old_loc) = placement.placements.get(&cell_id) {
                            let old_key = (old_loc.tile_x, old_loc.tile_y, old_loc.bel_index);
                            new_loc_map.remove(&old_key);

                            // Place consecutively: tile_y advances every 8 LCs
                            let lc_offset = i;
                            let tile_y = (*new_start_y + (lc_offset as u32 / 8))
                                .min(height.saturating_sub(2).max(1));
                            let bel_idx = lc_offset % 8;

                            let new_loc =
                                PlacementLoc::new(*new_x, tile_y, bel_idx, BelType::Carry);

                            let new_key = (new_loc.tile_x, new_loc.tile_y, new_loc.bel_index);
                            // If occupied by non-chain cell, displace it to the carry's old position
                            // but preserve the displaced cell's original bel_type
                            if let Some(&occupant) = loc_map.get(&new_key) {
                                if !chain.cells.contains(&occupant) {
                                    let occupant_bel_type = placement
                                        .placements
                                        .get(&occupant)
                                        .map(|loc| loc.bel_type)
                                        .unwrap_or(old_loc.bel_type);
                                    let displaced_loc = PlacementLoc::new(
                                        old_loc.tile_x,
                                        old_loc.tile_y,
                                        old_loc.bel_index,
                                        occupant_bel_type,
                                    );
                                    new_placement.placements.insert(occupant, displaced_loc);
                                    new_loc_map.insert(old_key, occupant);
                                }
                            }
                            new_placement.placements.insert(cell_id, new_loc);
                            new_loc_map.insert(new_key, cell_id);
                        }
                    }
                }
            }
        }

        (new_placement, new_loc_map)
    }

    /// Apply a move in-place, returning an UndoRecord for reversal.
    /// This avoids cloning the entire placement HashMap on every move attempt.
    fn apply_move_in_place(
        &self,
        placement: &mut PlacementResult,
        loc_map: &mut HashMap<(u32, u32, usize), CellId>,
        move_op: &Move,
    ) -> UndoRecord {
        let mut undo = UndoRecord {
            placement_changes: Vec::new(),
            loc_map_changes: Vec::new(),
        };

        match move_op {
            Move::Relocate(cell_id, new_loc) => {
                let old_loc = placement.placements[cell_id];
                let old_key = (old_loc.tile_x, old_loc.tile_y, old_loc.bel_index);
                let new_key = (new_loc.tile_x, new_loc.tile_y, new_loc.bel_index);

                if let Some(&occupant) = loc_map.get(&new_key) {
                    if occupant != *cell_id {
                        let occupant_old_loc = placement.placements[&occupant];

                        // Record undo
                        undo.placement_changes.push((*cell_id, old_loc));
                        undo.placement_changes.push((occupant, occupant_old_loc));
                        undo.loc_map_changes.push((old_key, Some(*cell_id)));
                        undo.loc_map_changes.push((new_key, Some(occupant)));

                        // Apply: swap cell with occupant
                        placement.placements.insert(*cell_id, *new_loc);
                        let displaced_loc = PlacementLoc::new(
                            old_loc.tile_x,
                            old_loc.tile_y,
                            old_loc.bel_index,
                            occupant_old_loc.bel_type,
                        );
                        placement.placements.insert(occupant, displaced_loc);
                        loc_map.insert(new_key, *cell_id);
                        loc_map.insert(old_key, occupant);
                    }
                } else {
                    // Record undo
                    undo.placement_changes.push((*cell_id, old_loc));
                    undo.loc_map_changes.push((old_key, Some(*cell_id)));
                    undo.loc_map_changes.push((new_key, None));

                    // Apply: simple move
                    placement.placements.insert(*cell_id, *new_loc);
                    loc_map.remove(&old_key);
                    loc_map.insert(new_key, *cell_id);
                }
            }
            Move::RelocateCarryChain(chain_id, new_x, new_start_y) => {
                if let Some(chain) = self.carry_chains.get(*chain_id) {
                    let (_, height) = self.device.grid_size();
                    // Snapshot current loc_map state for new keys before any mutations
                    let chain_cell_set: HashSet<CellId> = chain.cells.iter().copied().collect();

                    for (i, &cell_id) in chain.cells.iter().enumerate() {
                        if let Some(&old_loc) = placement.placements.get(&cell_id) {
                            let old_key = (old_loc.tile_x, old_loc.tile_y, old_loc.bel_index);

                            let tile_y = (*new_start_y + (i as u32 / 8))
                                .min(height.saturating_sub(2).max(1));
                            let bel_idx = i % 8;
                            let new_loc =
                                PlacementLoc::new(*new_x, tile_y, bel_idx, BelType::Carry);
                            let new_key = (new_loc.tile_x, new_loc.tile_y, new_loc.bel_index);

                            // Record undo for old key
                            undo.placement_changes.push((cell_id, old_loc));
                            undo.loc_map_changes
                                .push((old_key, loc_map.get(&old_key).copied()));

                            // Check for displaced occupant
                            if let Some(&occupant) = loc_map.get(&new_key) {
                                if !chain_cell_set.contains(&occupant) {
                                    let occ_loc = placement.placements[&occupant];
                                    undo.placement_changes.push((occupant, occ_loc));
                                    undo.loc_map_changes.push((new_key, Some(occupant)));

                                    let displaced_loc = PlacementLoc::new(
                                        old_loc.tile_x,
                                        old_loc.tile_y,
                                        old_loc.bel_index,
                                        occ_loc.bel_type,
                                    );
                                    placement.placements.insert(occupant, displaced_loc);
                                    loc_map.insert(old_key, occupant);
                                }
                            } else {
                                undo.loc_map_changes.push((new_key, None));
                                loc_map.remove(&old_key);
                            }

                            placement.placements.insert(cell_id, new_loc);
                            loc_map.insert(new_key, cell_id);
                        }
                    }
                }
            }
        }

        undo
    }

    /// Undo a move applied by apply_move_in_place.
    fn undo_move(
        placement: &mut PlacementResult,
        loc_map: &mut HashMap<(u32, u32, usize), CellId>,
        undo: UndoRecord,
    ) {
        // Restore placement locations
        for (cell_id, old_loc) in undo.placement_changes {
            placement.placements.insert(cell_id, old_loc);
        }
        // Restore loc_map entries (in reverse order to handle chain dependencies)
        for (key, old_value) in undo.loc_map_changes.into_iter().rev() {
            match old_value {
                Some(cell_id) => {
                    loc_map.insert(key, cell_id);
                }
                None => {
                    loc_map.remove(&key);
                }
            }
        }
    }

    /// Estimate delay for a single net based on current placement
    fn estimate_net_delay(
        &self,
        net_id: GateNetId,
        placement: &PlacementResult,
        netlist: &GateNetlist,
    ) -> f64 {
        let net = match netlist.nets.get(net_id.0 as usize) {
            Some(n) => n,
            None => return 0.0,
        };

        // Get driver location
        let driver_loc = match net.driver.and_then(|d| placement.placements.get(&d)) {
            Some(loc) => loc,
            None => return 0.0,
        };

        // Calculate worst-case delay to any sink
        let mut max_delay: f64 = 0.0;

        for &(sink_id, pin) in &net.fanout {
            if let Some(sink_loc) = placement.placements.get(&sink_id) {
                let routing_delay = self.delay_model.bel_connection_delay(
                    (driver_loc.tile_x, driver_loc.tile_y, driver_loc.bel_index),
                    (sink_loc.tile_x, sink_loc.tile_y, sink_loc.bel_index),
                    pin,
                );

                // Add cell delay for the driver
                let cell_delay = if let Some(driver_id) = net.driver {
                    if let Some(cell) = netlist.get_cell(driver_id) {
                        self.delay_model.cell_delay(&cell.cell_type)
                    } else {
                        self.delay_model.lut4_delay
                    }
                } else {
                    0.0
                };

                let total_delay = cell_delay + routing_delay;
                max_delay = max_delay.max(total_delay);
            }
        }

        max_delay
    }

    /// Criticality of each net (that of its most critical connection) and
    /// of each connection (see [`timing_estimate::connection_criticalities`]).
    ///
    /// [`timing_estimate::connection_criticalities`]: super::timing_estimate::connection_criticalities
    fn compute_criticalities(
        &self,
        netlist: &GateNetlist,
        placement: &PlacementResult,
    ) -> (HashMap<GateNetId, f64>, Vec<Vec<f64>>) {
        let bel = |id: CellId| {
            placement
                .placements
                .get(&id)
                .map(|loc| (loc.tile_x, loc.tile_y, loc.bel_index))
        };
        let connections = super::timing_estimate::connection_criticalities(
            netlist,
            &self.delay_model,
            |driver, sink, pin| match (bel(driver), bel(sink)) {
                (Some(from), Some(to)) => self.delay_model.bel_connection_delay(from, to, pin),
                _ => 0.0,
            },
        );
        let nets = netlist
            .nets
            .iter()
            .zip(&connections)
            .map(|(net, c)| (net.id, c.iter().copied().fold(0.0, f64::max)))
            .collect();
        (nets, connections)
    }

    /// Generate a move biased toward cells on critical paths
    fn generate_critical_path_move<R: Rng>(
        &self,
        placement: &PlacementResult,
        ctx: &MoveContext,
        loc_map: &HashMap<(u32, u32, usize), CellId>,
        rlim: u32,
        rng: &mut R,
    ) -> Move {
        if ctx.critical.is_empty() {
            return self.generate_move(placement, ctx, loc_map, rlim, rng);
        }
        let cell_id = ctx.critical[rng.gen_range(0..ctx.critical.len())];
        if has_footprint(placement.placements[&cell_id].bel_type) {
            return self.generate_footprint_swap(placement, ctx, cell_id, rlim, rng);
        }
        self.generate_window_move(placement, ctx, loc_map, cell_id, rlim, rng)
    }

    /// Swap `cell_id`, whose BEL uses up other BELs of its tile, with another
    /// cell of its BEL type within `rlim` tiles. Returns a no-op move if no
    /// such cell turns up.
    fn generate_footprint_swap<R: Rng>(
        &self,
        placement: &PlacementResult,
        ctx: &MoveContext,
        cell_id: CellId,
        rlim: u32,
        rng: &mut R,
    ) -> Move {
        let loc = placement.placements[&cell_id];
        let Some(peers) = ctx.footprint_cells.get(&loc.bel_type) else {
            return Move::Relocate(cell_id, loc);
        };
        const ATTEMPTS: usize = 16;
        let r = rlim.max(1);
        for _ in 0..ATTEMPTS {
            let other = peers[rng.gen_range(0..peers.len())];
            if other == cell_id {
                continue;
            }
            let target = placement.placements[&other];
            if target.tile_x.abs_diff(loc.tile_x) <= r && target.tile_y.abs_diff(loc.tile_y) <= r {
                return Move::Relocate(
                    cell_id,
                    PlacementLoc::new(target.tile_x, target.tile_y, target.bel_index, loc.bel_type),
                );
            }
        }
        Move::Relocate(cell_id, loc)
    }

    /// Estimate timing score for final placement (worst slack)
    fn estimate_timing_score(&self, placement: &PlacementResult, netlist: &GateNetlist) -> f64 {
        let (criticalities, _) = self.compute_criticalities(netlist, placement);

        // Sum of critical path delays
        let mut max_delay = 0.0f64;
        for net in &netlist.nets {
            let delay = self.estimate_net_delay(net.id, placement, netlist);
            let criticality = criticalities.get(&net.id).copied().unwrap_or(0.0);
            if criticality > 0.5 {
                max_delay = max_delay.max(delay);
            }
        }

        max_delay
    }

    /// Check if two BEL types are compatible for placement
    /// In iCE40, all DFF variants use the same hardware with different config bits
    fn bel_types_compatible(available: BelType, required: BelType) -> bool {
        if available == required {
            return true;
        }
        matches!(
            (available, required),
            // A basic Dff BEL can implement any DFF variant
            (BelType::Dff, BelType::DffE)
                | (BelType::Dff, BelType::DffSr)
                | (BelType::Dff, BelType::DffSrE)
                // Any FF type can implement a basic DFF
                | (BelType::DffE, BelType::Dff)
                | (BelType::DffSr, BelType::Dff)
                | (BelType::DffSrE, BelType::Dff)
                | (BelType::DffSrE, BelType::DffE)
                | (BelType::DffSrE, BelType::DffSr)
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::device::nexus::{NexusDevice, NexusVariant};
    use rand::SeedableRng;
    use skalp_lir::gate_netlist::{Cell, GateNet};

    /// A net's timing cost follows each connection's own criticality: moving
    /// its non-critical sink away costs nothing, moving the critical one away
    /// costs that connection's extra delay.
    #[test]
    fn timing_cost_is_priced_per_connection() {
        let mut net = GateNet::new(GateNetId(0), "n".to_string());
        net.driver = Some(CellId(0));
        net.fanout = vec![(CellId(1), 0), (CellId(2), 0)];
        let model = DelayModel::nexus();
        let place = |critical_at: u32, idle_at: u32| {
            let mut p = PlacementResult::new();
            for (id, x) in [(0, 10), (1, critical_at), (2, idle_at)] {
                p.placements
                    .insert(CellId(id), PlacementLoc::new(x, 10, 0, BelType::Lut4));
            }
            p
        };
        let crit = [1.0, 0.0];
        let cost = |p: &PlacementResult| NetCostCache::compute_net_costs(&net, p, &model, &crit).1;
        let base = cost(&place(11, 12));
        assert_eq!(cost(&place(11, 30)), base, "the idle sink is free to move");
        let far = cost(&place(30, 12));
        let expected = model.connection_delay(20, 0) - model.connection_delay(1, 0);
        assert!(
            (far - base - expected).abs() < 1e-9,
            "{far} {base} {expected}"
        );
        // No criticalities: wirelength only
        assert_eq!(
            NetCostCache::compute_net_costs(&net, &place(30, 12), &model, &[]).1,
            0.0
        );
    }

    /// Re-timing prices the same placement exactly as a cache built with the
    /// new criticalities would, and keeps the timing scale it started with.
    #[test]
    fn retiming_reprices_timing_on_the_original_scale() {
        let mut netlist = GateNetlist::new("t".to_string(), "test".to_string());
        let n = netlist.add_net(GateNet::new(GateNetId(0), "n".to_string()));
        netlist.nets[n.0 as usize].driver = Some(CellId(0));
        netlist.nets[n.0 as usize].fanout = vec![(CellId(1), 0), (CellId(2), 0)];
        let mut placement = PlacementResult::new();
        for (id, x) in [(0, 10), (1, 14), (2, 25)] {
            placement
                .placements
                .insert(CellId(id), PlacementLoc::new(x, 10, 0, BelType::Lut4));
        }
        let model = DelayModel::nexus();
        let mut cache =
            NetCostCache::build(&placement, &netlist, vec![vec![1.0, 0.0]], &model, 0.5);
        let scale = cache.timing_scale;
        cache.retime(&placement, &netlist, vec![vec![0.0, 1.0]], &model);
        let fresh = NetCostCache::build(&placement, &netlist, vec![vec![0.0, 1.0]], &model, 0.5);
        assert!((cache.total_timing_cost - fresh.total_timing_cost).abs() < 1e-12);
        assert_eq!(cache.timing_scale, scale);
        assert!(cache.total_timing_cost > 0.0);
    }

    /// Wide LUTs (whose BEL takes its slice's two LUTs) move by swapping
    /// with each other, so their footprints trade places; nothing else ever
    /// lands on a footprint BEL.
    #[test]
    fn footprint_cells_move_by_swapping() {
        let device = NexusDevice::new(NexusVariant::Lifcl40);
        let (width, height) = device.grid_size();
        // Two logic tiles with wide-LUT sites, three tiles apart
        let wide_sites = |x: u32, y: u32| -> Vec<usize> {
            device
                .tile_at(x, y)
                .map(|t| {
                    t.bels()
                        .iter()
                        .enumerate()
                        .filter(|(_, b)| b.bel_type == BelType::WideLut)
                        .map(|(i, _)| i)
                        .collect()
                })
                .unwrap_or_default()
        };
        let (x0, y0) = (0..width)
            .flat_map(|x| (0..height).map(move |y| (x, y)))
            .find(|&(x, y)| !wide_sites(x, y).is_empty() && !wide_sites(x + 3, y).is_empty())
            .expect("a logic tile");
        let (x1, y1) = (x0 + 3, y0);

        let mut netlist = GateNetlist::new("wide".to_string(), "nexus".to_string());
        let net = |n: &mut GateNetlist, name: &str| {
            n.add_net(GateNet::new(GateNetId(0), name.to_string()))
        };
        let a = net(&mut netlist, "a");
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
        let w0_out = net(&mut netlist, "w0");
        let w1_out = net(&mut netlist, "w1");
        let w0 = cell(&mut netlist, "OXIDE_LUT5", "w0", vec![a], vec![w0_out]);
        let w1 = cell(&mut netlist, "OXIDE_LUT5", "w1", vec![a], vec![w1_out]);
        let l_out = net(&mut netlist, "l");
        let l = cell(
            &mut netlist,
            "OXIDE_COMB",
            "l",
            vec![w0_out, w1_out],
            vec![l_out],
        );

        let mut placement = PlacementResult::new();
        let s0 = wide_sites(x0, y0)[0];
        let s1 = wide_sites(x1, y1)[0];
        placement
            .placements
            .insert(w0, PlacementLoc::new(x0, y0, s0, BelType::WideLut));
        placement
            .placements
            .insert(w1, PlacementLoc::new(x1, y1, s1, BelType::WideLut));
        let lut = device
            .tile_at(x0, y0 + 1)
            .and_then(|t| t.bels().iter().position(|b| b.bel_type == BelType::Lut4))
            .expect("a LUT");
        placement
            .placements
            .insert(l, PlacementLoc::new(x0, y0 + 1, lut, BelType::Lut4));
        let mut loc_map: HashMap<(u32, u32, usize), CellId> = HashMap::new();
        for (&c, loc) in &placement.placements {
            loc_map.insert((loc.tile_x, loc.tile_y, loc.bel_index), c);
        }

        let annealer = SimulatedAnnealing::new(&device, 1.0, 0.9, 10);
        let ctx = MoveContext::build(
            &placement,
            &netlist,
            &HashMap::new(),
            &HashMap::new(),
            None,
            &|x, y, bel| device.bel_footprint(x, y, bel),
        );
        assert_eq!(ctx.cells, vec![l]);
        assert_eq!(ctx.footprint_count, 2);

        let mut rng = rand::rngs::StdRng::seed_from_u64(1);
        let mut swapped = false;
        for _ in 0..200 {
            let Move::Relocate(mover, target) =
                annealer.generate_move(&placement, &ctx, &loc_map, 10, &mut rng)
            else {
                panic!("no carry chains here");
            };
            let key = (target.tile_x, target.tile_y, target.bel_index);
            if mover == l {
                assert!(
                    !ctx.reserved.contains(&key),
                    "a LUT moved onto a wide LUT's BELs"
                );
            } else if placement.placements[&mover] != target {
                let other = if mover == w0 { w1 } else { w0 };
                assert_eq!(
                    loc_map.get(&key),
                    Some(&other),
                    "a wide LUT moves only by a swap"
                );
                swapped = true;
            }
        }
        assert!(swapped, "wide LUTs never moved");
    }

    /// After annealing, a flip-flop left tiles away from the LUT feeding its
    /// data input moves into that LUT's tile, which shortens the net.
    #[test]
    fn detail_moves_pull_a_register_into_its_drivers_tile() {
        let device = NexusDevice::new(NexusVariant::Lifcl40);
        let (width, height) = device.grid_size();
        let first_bel = |x: u32, y: u32, ty: BelType| {
            device
                .tile_at(x, y)
                .and_then(|t| t.bels().iter().position(|b| b.bel_type == ty))
        };
        let (x0, y0) = (0..width)
            .flat_map(|x| (0..height).map(move |y| (x, y)))
            .find(|&(x, y)| {
                first_bel(x, y, BelType::Lut4).is_some()
                    && first_bel(x + 8, y, BelType::DffSrE).is_some()
            })
            .expect("two logic tiles");

        let mut netlist = GateNetlist::new("t".to_string(), "nexus".to_string());
        let net = |n: &mut GateNetlist, name: &str| {
            n.add_net(GateNet::new(GateNetId(0), name.to_string()))
        };
        let (clk, a, d, q) = (
            net(&mut netlist, "clk"),
            net(&mut netlist, "a"),
            net(&mut netlist, "d"),
            net(&mut netlist, "q"),
        );
        let cell = |ty: &str, name: &str, i, o| {
            Cell::new_comb(
                CellId(0),
                ty.to_string(),
                "nexus".to_string(),
                0.0,
                name.to_string(),
                vec![i],
                vec![o],
            )
        };
        let lut = netlist.add_cell(cell("OXIDE_COMB", "lut", a, d));
        let mut ff = cell("OXIDE_FF", "ff", d, q);
        ff.clock = Some(clk);
        let ff = netlist.add_cell(ff);

        let mut placement = PlacementResult::new();
        placement.placements.insert(
            lut,
            PlacementLoc::new(
                x0,
                y0,
                first_bel(x0, y0, BelType::Lut4).unwrap(),
                BelType::Lut4,
            ),
        );
        placement.placements.insert(
            ff,
            PlacementLoc::new(
                x0 + 8,
                y0,
                first_bel(x0 + 8, y0, BelType::DffSrE).unwrap(),
                BelType::DffSrE,
            ),
        );
        let mut loc_map: HashMap<(u32, u32, usize), CellId> = placement
            .placements
            .iter()
            .map(|(&c, l)| ((l.tile_x, l.tile_y, l.bel_index), c))
            .collect();

        let annealer = SimulatedAnnealing::new(&device, 1.0, 0.9, 10);
        let ctx = MoveContext::build(
            &placement,
            &netlist,
            &HashMap::new(),
            &HashMap::new(),
            None,
            &|x, y, bel| device.bel_footprint(x, y, bel),
        );
        let mut cache =
            NetCostCache::build(&placement, &netlist, Vec::new(), &annealer.delay_model, 0.0);
        let before = cache.combined_cost(0.0);

        let moved =
            annealer.refine_directed(&mut placement, &mut loc_map, &ctx, &mut cache, &netlist);
        assert_eq!(moved, 1);
        let at = placement.placements[&ff];
        assert_eq!((at.tile_x, at.tile_y), (x0, y0));
        assert_eq!(loc_map.get(&(x0, y0, at.bel_index)), Some(&ff));
        assert!(cache.combined_cost(0.0) < before);

        // Nothing left to pull in
        let moved =
            annealer.refine_directed(&mut placement, &mut loc_map, &ctx, &mut cache, &netlist);
        assert_eq!(moved, 0);
    }
}
