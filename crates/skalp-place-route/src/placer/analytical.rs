//! Analytical Placement
//!
//! Implements quadratic wirelength minimization for initial placement.
//! The algorithm:
//! 1. Builds a connectivity matrix (Laplacian) from the netlist
//! 2. Solves Lx = b using conjugate gradient
//! 3. Produces continuous cell positions

use super::{BelType, PlacementLoc, PlacementResult};
use crate::device::Device;
use crate::error::{PlaceRouteError, Result};
use crate::packing::CarryChain;
use skalp_lir::gate_netlist::{CellId, GateNetlist};
use std::collections::HashMap;

/// Analytical placer using quadratic wirelength minimization
pub struct AnalyticalPlacer<'a, D: Device> {
    device: &'a D,
    /// Resolved I/O constraints: signal_name → (tile_x, tile_y, bel_idx)
    resolved_io_constraints: HashMap<String, (u32, u32, usize)>,
}

impl<'a, D: Device> AnalyticalPlacer<'a, D> {
    /// Create a new analytical placer
    pub fn new(device: &'a D) -> Self {
        Self {
            device,
            resolved_io_constraints: HashMap::new(),
        }
    }

    /// Perform analytical placement with carry chain column constraints
    pub fn place_with_carry_chains(
        &self,
        netlist: &GateNetlist,
        carry_chains: &[CarryChain],
    ) -> Result<PlacementResult> {
        let n = netlist.cells.len();
        if n == 0 {
            return Ok(PlacementResult::new());
        }

        // Build cell index map
        let cell_indices: HashMap<CellId, usize> = netlist
            .cells
            .iter()
            .enumerate()
            .map(|(i, cell)| (cell.id, i))
            .collect();

        // Solve for continuous positions (same as place())
        let (mut x_positions, y_positions) = self.solve_positions(netlist, &cell_indices)?;

        // Anchor carry chains: force all cells in a chain to same x-coordinate
        self.anchor_carry_chains(&mut x_positions, carry_chains, &cell_indices);

        // Convert to discrete placements
        self.positions_to_placement(netlist, &cell_indices, &x_positions, &y_positions)
    }

    /// Perform analytical placement
    pub fn place(&self, netlist: &GateNetlist) -> Result<PlacementResult> {
        let n = netlist.cells.len();
        if n == 0 {
            return Ok(PlacementResult::new());
        }

        let cell_indices: HashMap<CellId, usize> = netlist
            .cells
            .iter()
            .enumerate()
            .map(|(i, cell)| (cell.id, i))
            .collect();

        let (x_positions, y_positions) = self.solve_positions(netlist, &cell_indices)?;
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

    /// Solve for continuous x, y positions via Laplacian + conjugate gradient
    fn solve_positions(
        &self,
        netlist: &GateNetlist,
        cell_indices: &HashMap<CellId, usize>,
    ) -> Result<(Vec<f64>, Vec<f64>)> {
        let n = netlist.cells.len();

        // Build connectivity matrix (Laplacian)
        let mut laplacian = vec![vec![0.0f64; n]; n];
        let mut bx = vec![0.0f64; n];
        let mut by = vec![0.0f64; n];

        // Process each net to build connectivity
        for net in &netlist.nets {
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

            let weight = 1.0 / connected_cells.len() as f64;

            for i in 0..connected_cells.len() {
                for j in (i + 1)..connected_cells.len() {
                    let ci = cell_indices[&connected_cells[i]];
                    let cj = cell_indices[&connected_cells[j]];

                    laplacian[ci][ci] += weight;
                    laplacian[cj][cj] += weight;
                    laplacian[ci][cj] -= weight;
                    laplacian[cj][ci] -= weight;
                }
            }
        }

        // Add anchor points for I/O cells
        // Use center positions as initial estimates for connectivity-driven placement
        let (width, height) = self.device.grid_size();
        let initial_positions: Vec<(f64, f64)> = vec![(width as f64 / 2.0, height as f64 / 2.0); n];
        for cell in &netlist.cells {
            let idx = cell_indices[&cell.id];

            if cell.cell_type.contains("IO") || cell.cell_type.starts_with("SB_IO") {
                let anchor_weight = 100.0;
                laplacian[idx][idx] += anchor_weight;

                // Use resolved physical pin location if available from constraints
                let signal_name = cell.path.split('.').next_back().unwrap_or(&cell.path);
                let signal_name = signal_name
                    .strip_suffix("_ibuf")
                    .or_else(|| signal_name.strip_suffix("_obuf"))
                    .unwrap_or(signal_name);

                let (ax, ay) = if let Some(&(tx, ty, _bel)) =
                    self.resolved_io_constraints.get(signal_name)
                {
                    (tx as f64, ty as f64)
                } else {
                    self.get_io_anchor(
                        cell.id, netlist, &initial_positions, cell_indices, width, height,
                    )
                };
                bx[idx] += anchor_weight * ax;
                by[idx] += anchor_weight * ay;
            } else {
                let center_weight = 0.01;
                laplacian[idx][idx] += center_weight;
                bx[idx] += center_weight * (width as f64 / 2.0);
                by[idx] += center_weight * (height as f64 / 2.0);
            }
        }

        let x_positions = self.conjugate_gradient(&laplacian, &bx, n)?;
        let y_positions = self.conjugate_gradient(&laplacian, &by, n)?;

        Ok((x_positions, y_positions))
    }

    /// Convert continuous positions to discrete BEL placements
    fn positions_to_placement(
        &self,
        netlist: &GateNetlist,
        _cell_indices: &HashMap<CellId, usize>,
        x_positions: &[f64],
        y_positions: &[f64],
    ) -> Result<PlacementResult> {
        let (width, height) = self.device.grid_size();
        let mut result = PlacementResult::new();
        let mut used_locations: HashMap<(u32, u32, usize), bool> = HashMap::new();

        for (idx, cell) in netlist.cells.iter().enumerate() {
            let cell_id = cell.id;
            let bel_type = self.cell_to_bel_type(&cell.cell_type);

            let x = x_positions[idx].clamp(0.0, (width - 1) as f64);
            let y = y_positions[idx].clamp(0.0, (height - 1) as f64);

            let (tile_x, tile_y, bel_index) =
                self.find_nearest_valid_location(x, y, bel_type, &mut used_locations)?;

            result.placements.insert(
                cell_id,
                PlacementLoc::new(tile_x, tile_y, bel_index, bel_type),
            );
        }

        Ok(result)
    }

    /// Anchor carry chain cells to the same x-coordinate (column constraint)
    fn anchor_carry_chains(
        &self,
        x_positions: &mut [f64],
        carry_chains: &[CarryChain],
        cell_indices: &HashMap<CellId, usize>,
    ) {
        for chain in carry_chains {
            if chain.cells.is_empty() {
                continue;
            }

            // Compute centroid x of all chain cells
            let mut sum_x = 0.0;
            let mut count = 0;
            for &cell_id in &chain.cells {
                if let Some(&idx) = cell_indices.get(&cell_id) {
                    sum_x += x_positions[idx];
                    count += 1;
                }
            }

            if count == 0 {
                continue;
            }

            let centroid_x = sum_x / count as f64;

            // Force all chain cells to the centroid x
            for &cell_id in &chain.cells {
                if let Some(&idx) = cell_indices.get(&cell_id) {
                    x_positions[idx] = centroid_x;
                }
            }

            // Also anchor associated LUTs and DFFs to same column
            for lut_id in chain.associated_luts.iter().flatten() {
                if let Some(&idx) = cell_indices.get(lut_id) {
                    x_positions[idx] = centroid_x;
                }
            }
            for dff_id in chain.associated_dffs.iter().flatten() {
                if let Some(&idx) = cell_indices.get(dff_id) {
                    x_positions[idx] = centroid_x;
                }
            }
        }
    }

    /// Conjugate gradient solver for Ax = b
    fn conjugate_gradient(&self, a: &[Vec<f64>], b: &[f64], n: usize) -> Result<Vec<f64>> {
        let mut x = vec![0.0; n];
        let mut r = b.to_vec();
        let mut p = r.clone();

        let max_iterations = n * 2;
        let tolerance = 1e-6;

        for _ in 0..max_iterations {
            // r_dot_r = r · r
            let r_dot_r: f64 = r.iter().map(|&ri| ri * ri).sum();

            if r_dot_r.sqrt() < tolerance {
                break;
            }

            // Ap = A * p
            let mut ap = vec![0.0; n];
            for i in 0..n {
                for (j, &p_j) in p.iter().enumerate() {
                    ap[i] += a[i][j] * p_j;
                }
            }

            // p_dot_ap = p · Ap
            let p_dot_ap: f64 = p.iter().zip(ap.iter()).map(|(&pi, &api)| pi * api).sum();

            if p_dot_ap.abs() < 1e-12 {
                break;
            }

            let alpha = r_dot_r / p_dot_ap;

            // x = x + alpha * p
            for i in 0..n {
                x[i] += alpha * p[i];
            }

            // r_new = r - alpha * Ap
            let mut r_new = vec![0.0; n];
            for i in 0..n {
                r_new[i] = r[i] - alpha * ap[i];
            }

            let r_new_dot: f64 = r_new.iter().map(|&ri| ri * ri).sum();
            let beta = r_new_dot / r_dot_r;

            // p = r_new + beta * p
            for i in 0..n {
                p[i] = r_new[i] + beta * p[i];
            }

            r = r_new;
        }

        Ok(x)
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
            let connected = net.driver == Some(cell_id)
                || net.fanout.iter().any(|(id, _)| *id == cell_id);
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
            let io_connected = net.driver == Some(cell_id)
                || net.fanout.iter().any(|(id, _)| *id == cell_id);
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
            let path = &netlist.cells.iter().find(|c| c.id == cell_id)
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

    /// Convert cell type to BEL type
    fn cell_to_bel_type(&self, cell_type: &str) -> BelType {
        if cell_type.contains("LUT") || cell_type.starts_with("SB_LUT") {
            BelType::Lut4
        } else if cell_type.contains("DFFE") || cell_type.starts_with("SB_DFFE") {
            BelType::DffE
        } else if cell_type.contains("DFF") || cell_type.starts_with("SB_DFF") {
            BelType::Dff
        } else if cell_type.contains("CARRY") || cell_type.starts_with("SB_CARRY") {
            BelType::Carry
        } else if cell_type.contains("IO") || cell_type.starts_with("SB_IO") {
            BelType::IoCell
        } else if cell_type.contains("RAM") || cell_type.starts_with("SB_RAM") {
            BelType::RamSlice
        } else if cell_type.contains("GB") || cell_type.starts_with("SB_GB") {
            BelType::GlobalBuf
        } else if cell_type.contains("PLL") || cell_type.starts_with("SB_PLL") {
            BelType::Pll
        } else if cell_type.contains("MAC")
            || cell_type.starts_with("SB_MAC")
            || cell_type.contains("MULT")
        {
            BelType::DspSlice
        } else {
            BelType::Lut4
        }
    }

    /// Find nearest valid location for a BEL type
    fn find_nearest_valid_location(
        &self,
        x: f64,
        y: f64,
        bel_type: BelType,
        used: &mut HashMap<(u32, u32, usize), bool>,
    ) -> Result<(u32, u32, usize)> {
        let (width, height) = self.device.grid_size();

        // Search in expanding squares
        for radius in 0..width.max(height) {
            let x_start = (x as i32 - radius as i32).max(0) as u32;
            let x_end = (x as u32 + radius).min(width - 1);
            let y_start = (y as i32 - radius as i32).max(0) as u32;
            let y_end = (y as u32 + radius).min(height - 1);

            for ty in y_start..=y_end {
                for tx in x_start..=x_end {
                    if let Some(tile) = self.device.tile_at(tx, ty) {
                        // Check each BEL in the tile
                        for (bel_idx, bel) in tile.bels().iter().enumerate() {
                            if bel.bel_type == bel_type && !used.contains_key(&(tx, ty, bel_idx)) {
                                used.insert((tx, ty, bel_idx), true);
                                return Ok((tx, ty, bel_idx));
                            }
                            // Also allow compatible BEL types
                            if self.bel_types_compatible(bel.bel_type, bel_type)
                                && !used.contains_key(&(tx, ty, bel_idx))
                            {
                                used.insert((tx, ty, bel_idx), true);
                                return Ok((tx, ty, bel_idx));
                            }
                        }
                    }
                }
            }
        }

        Err(PlaceRouteError::PlacementFailed(format!(
            "No valid location for BEL type {:?}",
            bel_type
        )))
    }

    /// Check if two BEL types are compatible
    fn bel_types_compatible(&self, available: BelType, required: BelType) -> bool {
        match (available, required) {
            // A basic Dff BEL can implement any DFF variant
            // (iCE40 DFFs are all the same hardware with different config bits)
            (BelType::Dff, BelType::DffE) => true,
            (BelType::Dff, BelType::DffSr) => true,
            (BelType::Dff, BelType::DffSrE) => true,
            // Any FF type can implement a basic DFF
            (BelType::DffE, BelType::Dff) => true,
            (BelType::DffSr, BelType::Dff) => true,
            (BelType::DffSrE, BelType::Dff) => true,
            (BelType::DffSrE, BelType::DffE) => true,
            (BelType::DffSrE, BelType::DffSr) => true,
            _ => available == required,
        }
    }
}
