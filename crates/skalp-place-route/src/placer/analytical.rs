//! Analytical Placement
//!
//! Implements quadratic wirelength minimization for initial placement.
//! The algorithm:
//! 1. Builds a connectivity matrix (Laplacian) from the netlist
//! 2. Solves Lx = b using conjugate gradient
//! 3. Produces continuous cell positions

use super::sites::{bel_types_compatible, SiteMap};
use super::spreading::{spread, CapacityMap, SpreadCell};
use super::timing_estimate;
use super::{is_constant_cell, PlacementLoc, PlacementResult};
use crate::collections::{HashMap, HashMapExt};
use crate::device::Device;
use crate::error::{PlaceRouteError, Result};
use crate::packing::CarryChain;
use crate::timing::DelayModel;
use skalp_lir::gate_netlist::{CellId, GateNetlist};

/// Nets with more pins than this use a star model instead of a clique.
const CLIQUE_MAX_PINS: usize = 8;

/// Most rounds of spreading and re-solving
const MAX_SPREAD_ROUNDS: usize = 30;

/// Spreading is done once the solved wirelength is this close to the
/// spread wirelength: further rounds would move little
const SPREAD_CONVERGED: f64 = 0.9;

/// Pull towards the spread positions added per round, relative to a
/// two-pin net (weight 1/2)
const ANCHOR_WEIGHT: f64 = 0.02;

/// Share of the (fill-limited) capacity of the region a design is spread
/// over that it may fill
const SPREAD_DENSITY: f64 = 0.9;

/// Residual, relative to the right-hand side, at which a solve stops
const CG_RELATIVE_TOLERANCE: f64 = 1e-5;

/// Carry cells per tile when the device has no chain layout (iCE40: one
/// per logic cell)
const CARRY_CELLS_PER_TILE: usize = 8;

/// The quadratic wirelength system `A x = b` for each axis: `A` as its
/// off-diagonal entries per row plus its diagonal.
struct QuadraticSystem {
    off_diag: Vec<Vec<(usize, f64)>>,
    diag: Vec<f64>,
    bx: Vec<f64>,
    by: Vec<f64>,
}

/// A carry chain's cells (position indices, chain order) and how chains
/// run on the device.
struct ChainShape {
    cells: Vec<usize>,
    step: (i32, i32),
    per_tile: usize,
}

impl ChainShape {
    /// Lay the chain out straight along the chain direction, centred where
    /// its cells are, as the legalizer will have to place it.
    fn straighten(&self, x: &mut [f64], y: &mut [f64]) {
        let n = self.cells.len() as f64;
        let cx = self.cells.iter().map(|&i| x[i]).sum::<f64>() / n;
        let cy = self.cells.iter().map(|&i| y[i]).sum::<f64>() / n;
        let span = self.cells.len().div_ceil(self.per_tile) as f64;
        for (k, &i) in self.cells.iter().enumerate() {
            let along = (k / self.per_tile) as f64 + 0.5 - span / 2.0;
            x[i] = cx + self.step.0 as f64 * along;
            y[i] = cy + self.step.1 as f64 * along;
        }
    }
}

/// Whether a cell is an I/O pad, held at its pin rather than spread.
fn is_io_cell(cell: &skalp_lir::gate_netlist::Cell) -> bool {
    cell.cell_type.contains("IO") || cell.cell_type.starts_with("SB_IO")
}

/// Position indices of each net's placed cells, for nets of two or more.
fn net_pins(netlist: &GateNetlist, cell_indices: &HashMap<CellId, usize>) -> Vec<Vec<usize>> {
    netlist
        .nets
        .iter()
        .map(|net| {
            net.driver
                .iter()
                .chain(net.fanout.iter().map(|(c, _)| c))
                .filter_map(|c| cell_indices.get(c).copied())
                .collect::<Vec<usize>>()
        })
        .filter(|pins| pins.len() >= 2)
        .collect()
}

/// Half-perimeter wirelength of `pins` at positions `x`, `y`.
fn hpwl(pins: &[Vec<usize>], x: &[f64], y: &[f64]) -> f64 {
    pins.iter()
        .map(|net| {
            let (mut x0, mut x1, mut y0, mut y1) = (f64::MAX, f64::MIN, f64::MAX, f64::MIN);
            for &i in net {
                x0 = x0.min(x[i]);
                x1 = x1.max(x[i]);
                y0 = y0.min(y[i]);
                y1 = y1.max(y[i]);
            }
            (x1 - x0) + (y1 - y0)
        })
        .sum()
}

/// Extra weight of a net on the worst path in timing-driven placement (a net
/// with slack to spare keeps weight 1).
const TIMING_NET_WEIGHT: f64 = 8.0;
/// How sharply the extra weight falls off with slack: criticality is raised
/// to this power, so only nets near the worst path gain much.
const TIMING_CRITICALITY_EXPONENT: i32 = 4;

/// Analytical placer using quadratic wirelength minimization
pub struct AnalyticalPlacer<'a, D: Device> {
    device: &'a D,
    /// Delays for timing-driven net weights; `None` places for wirelength only
    delay_model: Option<DelayModel>,
    /// Resolved I/O constraints: signal_name → (tile_x, tile_y, bel_idx)
    resolved_io_constraints: HashMap<String, (u32, u32, usize)>,
    /// Share of each tile's BELs of a type cells may take
    /// ([`SiteMap::with_fill_limit`])
    fill_limit: f64,
}

impl<'a, D: Device> AnalyticalPlacer<'a, D> {
    /// Create a new analytical placer
    pub fn new(device: &'a D) -> Self {
        Self {
            device,
            delay_model: None,
            resolved_io_constraints: HashMap::new(),
            fill_limit: 1.0,
        }
    }

    /// Weight nets by timing criticality, estimated with these delays
    pub fn with_timing(mut self, delay_model: DelayModel) -> Self {
        self.delay_model = Some(delay_model);
        self
    }

    /// Fill each tile's BELs of a type at most this far (0..=1)
    pub fn with_fill_limit(mut self, fill_limit: f64) -> Self {
        self.fill_limit = fill_limit;
        self
    }

    /// Perform analytical placement with carry chain column constraints
    pub fn place_with_carry_chains(
        &self,
        netlist: &GateNetlist,
        carry_chains: &[CarryChain],
    ) -> Result<PlacementResult> {
        // Build cell index map, excluding constant cells (hardwired VCC/GND on FPGA)
        let cell_indices: HashMap<CellId, usize> = netlist
            .cells
            .iter()
            .filter(|cell| !is_constant_cell(&cell.cell_type))
            .enumerate()
            .map(|(i, cell)| (cell.id, i))
            .collect();

        if cell_indices.is_empty() {
            return Ok(PlacementResult::new());
        }

        let (x_positions, y_positions) = self.global_place(netlist, &cell_indices, carry_chains)?;
        self.positions_to_placement(netlist, &cell_indices, &x_positions, &y_positions)
    }

    /// Perform analytical placement
    pub fn place(&self, netlist: &GateNetlist) -> Result<PlacementResult> {
        // Build cell index map, excluding constant cells (hardwired VCC/GND on FPGA)
        let cell_indices: HashMap<CellId, usize> = netlist
            .cells
            .iter()
            .filter(|cell| !is_constant_cell(&cell.cell_type))
            .enumerate()
            .map(|(i, cell)| (cell.id, i))
            .collect();

        if cell_indices.is_empty() {
            return Ok(PlacementResult::new());
        }

        let (x_positions, y_positions) = self.global_place(netlist, &cell_indices, &[])?;
        self.positions_to_placement(netlist, &cell_indices, &x_positions, &y_positions)
    }

    /// Set resolved I/O constraints (signal_name → tile location).
    /// When set, constrained I/Os use physical pin locations as anchors.
    pub fn with_resolved_constraints(
        mut self,
        constraints: HashMap<String, (u32, u32, usize)>,
    ) -> Self {
        self.resolved_io_constraints = constraints;
        self
    }

    /// Build the quadratic wirelength system: the net Laplacian, with I/O
    /// cells tied to their pins and every other cell weakly to the centre.
    fn build_system(
        &self,
        netlist: &GateNetlist,
        cell_indices: &HashMap<CellId, usize>,
        net_weights: Option<&[f64]>,
    ) -> QuadraticSystem {
        let n = cell_indices.len();

        // Build sparse connectivity matrix (Laplacian) — O(nnz) storage & multiply
        // Each row stores only non-zero entries: Vec<(column_index, value)>
        let mut laplacian: Vec<Vec<(usize, f64)>> = vec![Vec::new(); n];
        // Diagonal stored separately for fast accumulation
        let mut diag = vec![0.0f64; n];
        let mut bx = vec![0.0f64; n];
        let mut by = vec![0.0f64; n];

        // Process each net to build connectivity
        for (net_index, net) in netlist.nets.iter().enumerate() {
            let net_weight = net_weights.map_or(1.0, |w| w[net_index]);
            let mut connected_cells: Vec<CellId> = Vec::new();

            if let Some(driver) = net.driver {
                if cell_indices.contains_key(&driver) {
                    connected_cells.push(driver);
                }
            }

            for (cell_id, _) in &net.fanout {
                if cell_indices.contains_key(cell_id) {
                    connected_cells.push(*cell_id);
                }
            }

            if connected_cells.len() < 2 {
                continue;
            }

            let k = connected_cells.len();
            let mut connect = |ci: usize, cj: usize, weight: f64| {
                diag[ci] += weight;
                diag[cj] += weight;
                laplacian[ci].push((cj, -weight));
                laplacian[cj].push((ci, -weight));
            };
            if k <= CLIQUE_MAX_PINS {
                // Clique model: every pin pair, weight 1/k
                let weight = net_weight / k as f64;
                for i in 0..k {
                    for j in (i + 1)..k {
                        connect(
                            cell_indices[&connected_cells[i]],
                            cell_indices[&connected_cells[j]],
                            weight,
                        );
                    }
                }
            } else {
                // Star model around the first pin (the driver when there is
                // one): k-1 edges instead of k(k-1)/2, which on a 2,000-pin
                // register-file address net is the difference between ~2K and
                // ~2M matrix entries. Weight 1/2 per edge keeps the net's total
                // weight equal to the clique's (k(k-1)/2 pairs × 1/k).
                let hub = cell_indices[&connected_cells[0]];
                let weight = 0.5 * net_weight;
                for cell in &connected_cells[1..] {
                    connect(hub, cell_indices[cell], weight);
                }
            }
        }

        // Add anchor points for I/O cells
        let (width, height) = self.device.grid_size();
        let initial_positions: Vec<(f64, f64)> = vec![(width as f64 / 2.0, height as f64 / 2.0); n];
        for cell in &netlist.cells {
            let idx = match cell_indices.get(&cell.id) {
                Some(&i) => i,
                None => continue, // constant cell, not placed
            };

            if is_io_cell(cell) {
                let anchor_weight = 100.0;
                diag[idx] += anchor_weight;

                let signal_name = cell.path.split('.').next_back().unwrap_or(&cell.path);
                let signal_name = signal_name
                    .strip_suffix("_ibuf")
                    .or_else(|| signal_name.strip_suffix("_obuf"))
                    .unwrap_or(signal_name);

                let (ax, ay) =
                    if let Some(&(tx, ty, _bel)) = self.resolved_io_constraints.get(signal_name) {
                        (tx as f64, ty as f64)
                    } else {
                        self.get_io_anchor(
                            cell.id,
                            netlist,
                            &initial_positions,
                            cell_indices,
                            width,
                            height,
                        )
                    };
                bx[idx] += anchor_weight * ax;
                by[idx] += anchor_weight * ay;
            } else {
                let center_weight = 0.01;
                diag[idx] += center_weight;
                bx[idx] += center_weight * (width as f64 / 2.0);
                by[idx] += center_weight * (height as f64 / 2.0);
            }
        }

        // Merge duplicate off-diagonal entries per row and sort for cache efficiency
        for row in &mut laplacian {
            row.sort_unstable_by_key(|&(col, _)| col);
            // Merge duplicates
            let mut merged: Vec<(usize, f64)> = Vec::with_capacity(row.len());
            for &(col, val) in row.iter() {
                if let Some(last) = merged.last_mut() {
                    if last.0 == col {
                        last.1 += val;
                        continue;
                    }
                }
                merged.push((col, val));
            }
            *row = merged;
        }

        QuadraticSystem {
            off_diag: laplacian,
            diag,
            bx,
            by,
        }
    }

    /// Convert continuous positions to discrete BEL placements
    fn positions_to_placement(
        &self,
        netlist: &GateNetlist,
        cell_indices: &HashMap<CellId, usize>,
        x_positions: &[f64],
        y_positions: &[f64],
    ) -> Result<PlacementResult> {
        let (width, height) = self.device.grid_size();
        let mut result = PlacementResult::new();
        let mut sites = SiteMap::new(self.device).with_fill_limit(self.fill_limit);
        // Flip-flops share a control group only with the same control set
        let sets = super::control_sets::control_sets(netlist);

        for cell in &netlist.cells {
            let idx = match cell_indices.get(&cell.id) {
                Some(&i) => i,
                None => continue, // constant cell, not placed
            };
            let bel_type = super::cell_bel_type(cell);

            let x = x_positions[idx].clamp(0.0, (width - 1) as f64);
            let y = y_positions[idx].clamp(0.0, (height - 1) as f64);

            let (tile_x, tile_y, bel_index) = sites
                .take_nearest(
                    x,
                    y,
                    bel_type,
                    sets.get(&cell.id).copied(),
                    bel_types_compatible,
                )
                .ok_or_else(|| {
                    PlaceRouteError::PlacementFailed(format!(
                        "No valid location for BEL type {:?}",
                        bel_type
                    ))
                })?;

            result.placements.insert(
                cell.id,
                PlacementLoc::new(tile_x, tile_y, bel_index, bel_type),
            );
        }

        Ok(result)
    }

    /// Continuous positions for every cell: a quadratic wirelength solve,
    /// then rounds of spreading the cells over the device and solving again
    /// with each cell pulled towards its spread position (SimPL), until the
    /// solve no longer undoes the spreading. Returns the last spread
    /// positions, which the device can hold.
    fn global_place(
        &self,
        netlist: &GateNetlist,
        cell_indices: &HashMap<CellId, usize>,
        carry_chains: &[CarryChain],
    ) -> Result<(Vec<f64>, Vec<f64>)> {
        let n = cell_indices.len();
        let mut system = self.build_system(netlist, cell_indices, None);
        let (width, height) = self.device.grid_size();
        let centre = || vec![width as f64 / 2.0; n];
        let mut x =
            Self::conjugate_gradient_sparse(&system.off_diag, &system.diag, &system.bx, centre());
        let mut y = Self::conjugate_gradient_sparse(
            &system.off_diag,
            &system.diag,
            &system.by,
            vec![height as f64 / 2.0; n],
        );

        // Cells to spread: everything but I/O, which stays tied to its pins
        let capacity = CapacityMap::new(self.device, self.fill_limit);
        let mut movable: Vec<SpreadCell> = Vec::new();
        for cell in &netlist.cells {
            let Some(&index) = cell_indices.get(&cell.id) else {
                continue;
            };
            if is_io_cell(cell) {
                continue;
            }
            movable.push(SpreadCell {
                index,
                demand: capacity.demand(super::cell_bel_type(cell)),
            });
        }
        if movable.is_empty() {
            return Ok((x, y));
        }
        let chains = self.chain_shapes(carry_chains, cell_indices);
        let pins = net_pins(netlist, cell_indices);

        let mut targets: Vec<(f64, f64)> = Vec::new();
        for round in 0..MAX_SPREAD_ROUNDS {
            targets = spread(&capacity, &movable, &x, &y, SPREAD_DENSITY);
            let mut tx = x.clone();
            let mut ty = y.clone();
            for (cell, &(px, py)) in movable.iter().zip(&targets) {
                tx[cell.index] = px;
                ty[cell.index] = py;
            }
            for chain in &chains {
                chain.straighten(&mut tx, &mut ty);
            }
            let spread_wl = hpwl(&pins, &tx, &ty);
            let solved_wl = hpwl(&pins, &x, &y);
            log::debug!(
                "analytical: round {round}: solved HPWL {solved_wl:.0}, spread HPWL {spread_wl:.0}"
            );
            targets = movable.iter().map(|c| (tx[c.index], ty[c.index])).collect();
            if solved_wl >= SPREAD_CONVERGED * spread_wl {
                break;
            }

            // Timing-driven: weight each net by how critical it is with the
            // cells where spreading put them.
            if let Some(model) = &self.delay_model {
                let critical = timing_estimate::net_criticalities(netlist, model, |id| {
                    cell_indices.get(&id).map(|&i| (tx[i], ty[i]))
                });
                let weights: Vec<f64> = critical
                    .iter()
                    .map(|c| 1.0 + TIMING_NET_WEIGHT * c.powi(TIMING_CRITICALITY_EXPONENT))
                    .collect();
                system = self.build_system(netlist, cell_indices, Some(&weights));
            }

            // Pull every movable cell towards its spread position, harder
            // each round, and solve again from where the cells are
            let weight = ANCHOR_WEIGHT * (round + 1) as f64;
            let mut diag = system.diag.clone();
            let mut bx = system.bx.clone();
            let mut by = system.by.clone();
            for cell in &movable {
                let i = cell.index;
                diag[i] += weight;
                bx[i] += weight * tx[i];
                by[i] += weight * ty[i];
            }
            x = Self::conjugate_gradient_sparse(&system.off_diag, &diag, &bx, x);
            y = Self::conjugate_gradient_sparse(&system.off_diag, &diag, &by, y);
        }

        for (cell, &(px, py)) in movable.iter().zip(&targets) {
            x[cell.index] = px;
            y[cell.index] = py;
        }
        Ok((x, y))
    }

    /// Each carry chain's cells in chain order, with the device's chain
    /// direction and positions per tile.
    fn chain_shapes(
        &self,
        carry_chains: &[CarryChain],
        cell_indices: &HashMap<CellId, usize>,
    ) -> Vec<ChainShape> {
        // Without a layout, chains run up a column of logic cells (iCE40)
        let (step, per_tile) = match self.device.carry_chain_layout() {
            Some(layout) => (layout.step, layout.slots.len().max(1)),
            None => ((0, 1), CARRY_CELLS_PER_TILE),
        };
        carry_chains
            .iter()
            .map(|chain| ChainShape {
                cells: chain
                    .cells
                    .iter()
                    .filter_map(|c| cell_indices.get(c).copied())
                    .collect(),
                step,
                per_tile,
            })
            .filter(|c| !c.cells.is_empty())
            .collect()
    }

    /// Sparse conjugate gradient solver for Ax = b, with a Jacobi (diagonal)
    /// preconditioner, starting from `x0`.
    ///
    /// Uses sparse Laplacian (off-diagonal entries) + separate diagonal vector.
    /// Matrix-vector multiply is O(nnz) instead of O(n²). It stops once the
    /// residual is a small fraction of `b`: positions are in tiles, so a
    /// far tighter solve changes nothing a placement can show.
    fn conjugate_gradient_sparse(
        off_diag: &[Vec<(usize, f64)>],
        diag: &[f64],
        b: &[f64],
        x0: Vec<f64>,
    ) -> Vec<f64> {
        let n = b.len();
        let mut x = x0;
        let multiply = |v: &[f64], out: &mut [f64]| {
            for i in 0..n {
                let mut sum = diag[i] * v[i];
                for &(j, val) in &off_diag[i] {
                    sum += val * v[j];
                }
                out[i] = sum;
            }
        };
        let mut ap = vec![0.0; n];
        multiply(&x, &mut ap);
        let mut r: Vec<f64> = b.iter().zip(&ap).map(|(bi, ai)| bi - ai).collect();
        let inv_diag: Vec<f64> = diag
            .iter()
            .map(|&d| if d.abs() > 1e-12 { 1.0 / d } else { 1.0 })
            .collect();
        let mut z: Vec<f64> = r.iter().zip(&inv_diag).map(|(ri, m)| ri * m).collect();
        let mut p = z.clone();
        let mut r_dot_z: f64 = r.iter().zip(&z).map(|(a, b)| a * b).sum();

        let b_norm = b.iter().map(|v| v * v).sum::<f64>().sqrt();
        let tolerance = (CG_RELATIVE_TOLERANCE * b_norm).max(1e-9);
        let max_iterations = (n * 2).max(10);

        for _ in 0..max_iterations {
            if r.iter().map(|v| v * v).sum::<f64>().sqrt() < tolerance {
                break;
            }
            multiply(&p, &mut ap);
            let p_dot_ap: f64 = p.iter().zip(&ap).map(|(a, b)| a * b).sum();
            if p_dot_ap.abs() < 1e-12 {
                break;
            }
            let alpha = r_dot_z / p_dot_ap;
            for i in 0..n {
                x[i] += alpha * p[i];
                r[i] -= alpha * ap[i];
                z[i] = r[i] * inv_diag[i];
            }
            let r_dot_z_new: f64 = r.iter().zip(&z).map(|(a, b)| a * b).sum();
            let beta = r_dot_z_new / r_dot_z;
            r_dot_z = r_dot_z_new;
            for i in 0..n {
                p[i] = z[i] + beta * p[i];
            }
        }
        x
    }

    /// Get I/O anchor position based on connectivity to internal cells.
    ///
    /// Strategy:
    /// 1. For clock/reset nets, place at bottom center (global buffer locations)
    /// 2. For other I/Os, find connected internal cells and compute their center of gravity,
    ///    then project to the nearest perimeter position.
    /// 3. Fall back to even distribution around perimeter for unconnected I/Os.
    fn get_io_anchor(
        &self,
        cell_id: CellId,
        netlist: &GateNetlist,
        cell_positions: &[(f64, f64)],
        cell_indices: &HashMap<CellId, usize>,
        width: u32,
        height: u32,
    ) -> (f64, f64) {
        // Check if this I/O drives or sinks a clock/reset net
        for net in &netlist.nets {
            let connected =
                net.driver == Some(cell_id) || net.fanout.iter().any(|(id, _)| *id == cell_id);
            if connected && net.is_clock {
                return (width as f64 / 2.0, 0.5);
            }
            if connected && net.is_reset {
                return (width as f64 / 2.0 + 1.0, 0.5);
            }
        }

        // Find center of gravity of connected internal cells
        let mut cx = 0.0f64;
        let mut cy = 0.0f64;
        let mut count = 0u32;

        for net in &netlist.nets {
            let io_connected =
                net.driver == Some(cell_id) || net.fanout.iter().any(|(id, _)| *id == cell_id);
            if !io_connected {
                continue;
            }

            // Collect positions of all connected cells (excluding self)
            if let Some(driver) = net.driver {
                if driver != cell_id {
                    if let Some(&idx) = cell_indices.get(&driver) {
                        cx += cell_positions[idx].0;
                        cy += cell_positions[idx].1;
                        count += 1;
                    }
                }
            }
            for (sink_id, _) in &net.fanout {
                if *sink_id != cell_id {
                    if let Some(&idx) = cell_indices.get(sink_id) {
                        cx += cell_positions[idx].0;
                        cy += cell_positions[idx].1;
                        count += 1;
                    }
                }
            }
        }

        if count > 0 {
            cx /= count as f64;
            cy /= count as f64;

            // Project center of gravity to nearest perimeter position
            let w = width as f64;
            let h = height as f64;

            // Distance to each edge
            let d_left = cx;
            let d_right = w - 1.0 - cx;
            let d_bottom = cy;
            let d_top = h - 1.0 - cy;
            let min_d = d_left.min(d_right).min(d_bottom).min(d_top);

            if min_d == d_left {
                (0.5, cy.clamp(0.5, h - 1.5))
            } else if min_d == d_right {
                (w - 1.5, cy.clamp(0.5, h - 1.5))
            } else if min_d == d_bottom {
                (cx.clamp(0.5, w - 1.5), 0.5)
            } else {
                (cx.clamp(0.5, w - 1.5), h - 1.5)
            }
        } else {
            // No connected internal cells — distribute evenly around perimeter
            let path = &netlist
                .cells
                .iter()
                .find(|c| c.id == cell_id)
                .map(|c| c.path.as_str())
                .unwrap_or("");
            let hash = path.bytes().fold(0u64, |acc, b| acc.wrapping_add(b as u64));
            let perimeter = 2 * (width + height) as u64;
            let pos = hash % perimeter;
            let w = width as u64;
            let h = height as u64;

            if pos < w {
                (pos as f64, 0.5) // Bottom
            } else if pos < w + h {
                ((width - 1) as f64 - 0.5, (pos - w) as f64) // Right
            } else if pos < 2 * w + h {
                ((2 * w + h - pos) as f64, (height - 1) as f64 - 0.5) // Top
            } else {
                (0.5, (2 * w + 2 * h - pos) as f64) // Left
            }
        }
    }
}
