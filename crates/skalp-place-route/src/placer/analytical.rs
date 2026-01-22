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
use skalp_lir::gate_netlist::{CellId, GateNetlist};
use std::collections::HashMap;

/// Analytical placer using quadratic wirelength minimization
pub struct AnalyticalPlacer<'a, D: Device> {
    device: &'a D,
}

impl<'a, D: Device> AnalyticalPlacer<'a, D> {
    /// Create a new analytical placer
    pub fn new(device: &'a D) -> Self {
        Self { device }
    }

    /// Perform analytical placement
    pub fn place(&self, netlist: &GateNetlist) -> Result<PlacementResult> {
        let n = netlist.cells.len();
        if n == 0 {
            return Ok(PlacementResult::new());
        }

        // Map cells to indices using their position in the Vec (more reliable than cell.id)
        // This handles cases where cell.id might not match the Vec index
        let cell_indices: HashMap<CellId, usize> = netlist
            .cells
            .iter()
            .enumerate()
            .map(|(i, cell)| (cell.id, i))
            .collect();

        // Build connectivity matrix (Laplacian)
        let mut laplacian = vec![vec![0.0f64; n]; n];
        let mut bx = vec![0.0f64; n];
        let mut by = vec![0.0f64; n];

        // Process each net to build connectivity
        for net in &netlist.nets {
            let mut connected_cells: Vec<CellId> = Vec::new();

            // Add driver (only if it exists in the cell list)
            if let Some(driver) = net.driver {
                if cell_indices.contains_key(&driver) {
                    connected_cells.push(driver);
                }
            }

            // Add fanout cells (only if they exist in the cell list)
            for (cell_id, _) in &net.fanout {
                if cell_indices.contains_key(cell_id) {
                    connected_cells.push(*cell_id);
                }
            }

            // Skip nets with 0 or 1 connections
            if connected_cells.len() < 2 {
                continue;
            }

            // Weight inversely proportional to net size (clique model)
            let weight = 1.0 / connected_cells.len() as f64;

            // Add connections to Laplacian
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
        let (width, height) = self.device.grid_size();
        for cell in &netlist.cells {
            let cell_id = cell.id;
            let idx = cell_indices[&cell_id];

            // Check if this is an I/O cell - anchor to boundary
            if cell.cell_type.contains("IO") || cell.cell_type.starts_with("SB_IO") {
                // I/O cells get fixed positions at boundaries
                let anchor_weight = 100.0;
                laplacian[idx][idx] += anchor_weight;

                // Place on boundary based on net name hints
                let (ax, ay) = self.get_io_anchor(&cell.path, width, height);
                bx[idx] += anchor_weight * ax;
                by[idx] += anchor_weight * ay;
            } else {
                // Non-I/O cells get weak center pull
                let center_weight = 0.01;
                laplacian[idx][idx] += center_weight;
                bx[idx] += center_weight * (width as f64 / 2.0);
                by[idx] += center_weight * (height as f64 / 2.0);
            }
        }

        // Solve using conjugate gradient
        let x_positions = self.conjugate_gradient(&laplacian, &bx, n)?;
        let y_positions = self.conjugate_gradient(&laplacian, &by, n)?;

        // Convert continuous positions to discrete placements
        let mut result = PlacementResult::new();
        let mut used_locations: HashMap<(u32, u32, usize), bool> = HashMap::new();

        for (idx, cell) in netlist.cells.iter().enumerate() {
            let cell_id = cell.id;
            let bel_type = self.cell_to_bel_type(&cell.cell_type);

            // Get continuous position
            let x = x_positions[idx].clamp(0.0, (width - 1) as f64);
            let y = y_positions[idx].clamp(0.0, (height - 1) as f64);

            // Find nearest valid tile
            let (tile_x, tile_y, bel_index) =
                self.find_nearest_valid_location(x, y, bel_type, &mut used_locations)?;

            result.placements.insert(
                cell_id,
                PlacementLoc::new(tile_x, tile_y, bel_index, bel_type),
            );
        }

        Ok(result)
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

    /// Get I/O anchor position based on port name
    fn get_io_anchor(&self, path: &str, width: u32, height: u32) -> (f64, f64) {
        // Try to infer position from name
        let path_lower = path.to_lowercase();

        if path_lower.contains("clk") || path_lower.contains("clock") {
            // Clocks typically at bottom
            return (width as f64 / 2.0, 0.5);
        }

        if path_lower.contains("rst") || path_lower.contains("reset") {
            // Resets typically at bottom
            return (width as f64 / 2.0 + 1.0, 0.5);
        }

        // Default: distribute around perimeter
        let hash = path.bytes().fold(0u64, |acc, b| acc.wrapping_add(b as u64));
        let side = hash % 4;

        match side {
            0 => (0.5, (hash % height as u64) as f64), // Left
            1 => ((width - 1) as f64 - 0.5, (hash % height as u64) as f64), // Right
            2 => ((hash % width as u64) as f64, 0.5),  // Bottom
            _ => ((hash % width as u64) as f64, (height - 1) as f64 - 0.5), // Top
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
