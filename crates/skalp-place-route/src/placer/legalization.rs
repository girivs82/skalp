//! Placement Legalization
//!
//! Snaps analytical placement to valid BEL sites while minimizing displacement.

use super::{BelType, PlacementLoc, PlacementResult};
use crate::device::Device;
use crate::error::{PlaceRouteError, Result};
use crate::packing::CarryChain;
use skalp_lir::gate_netlist::{CellId, GateNetlist};
use std::collections::{HashMap, HashSet};

/// Legalizer for snapping placements to valid BEL sites
pub struct Legalizer<'a, D: Device> {
    device: &'a D,
}

impl<'a, D: Device> Legalizer<'a, D> {
    /// Create a new legalizer
    pub fn new(device: &'a D) -> Self {
        Self { device }
    }

    /// Legalize a placement result
    pub fn legalize(
        &self,
        placement: PlacementResult,
        netlist: &GateNetlist,
    ) -> Result<PlacementResult> {
        self.legalize_with_carry_chains(placement, netlist, &[])
    }

    /// Legalize with carry chain constraints: chains get consecutive LC slots first
    pub fn legalize_with_carry_chains(
        &self,
        placement: PlacementResult,
        netlist: &GateNetlist,
        carry_chains: &[CarryChain],
    ) -> Result<PlacementResult> {
        let mut result = PlacementResult::new();
        let mut used_bels: HashMap<(u32, u32, usize), CellId> = HashMap::new();

        // Phase 1: Legalize carry chains into consecutive LC slots
        let chain_cells = self.legalize_carry_chains(
            &placement,
            carry_chains,
            &mut result,
            &mut used_bels,
        )?;

        // Phase 2: Legalize remaining cells
        let mut cells: Vec<(CellId, PlacementLoc)> = placement
            .placements
            .into_iter()
            .filter(|(id, _)| !chain_cells.contains(id))
            .collect();
        cells.sort_by(|a, b| {
            let a_score = a.1.tile_y * 1000 + a.1.tile_x;
            let b_score = b.1.tile_y * 1000 + b.1.tile_x;
            a_score.cmp(&b_score)
        });

        for (cell_id, loc) in cells {
            let _cell = netlist.get_cell(cell_id);
            let bel_type = loc.bel_type;

            let legal_loc =
                self.find_legal_location(loc.tile_x, loc.tile_y, bel_type, &used_bels)?;

            used_bels.insert(
                (legal_loc.tile_x, legal_loc.tile_y, legal_loc.bel_index),
                cell_id,
            );

            result.placements.insert(cell_id, legal_loc);
        }

        Ok(result)
    }

    /// Legalize carry chains into consecutive LC slots in the same column.
    /// Returns the set of cell IDs that were placed as part of chains.
    fn legalize_carry_chains(
        &self,
        placement: &PlacementResult,
        carry_chains: &[CarryChain],
        result: &mut PlacementResult,
        used_bels: &mut HashMap<(u32, u32, usize), CellId>,
    ) -> Result<HashSet<CellId>> {
        let mut chain_cells = HashSet::new();
        let (width, height) = self.device.grid_size();

        for chain in carry_chains {
            if chain.cells.is_empty() {
                continue;
            }

            let n = chain.cells.len();

            // Find the column (tile_x) where most chain cells are currently placed
            let mut col_votes: HashMap<u32, usize> = HashMap::new();
            for &cell_id in &chain.cells {
                if let Some(loc) = placement.placements.get(&cell_id) {
                    *col_votes.entry(loc.tile_x).or_insert(0) += 1;
                }
            }
            let best_col = col_votes
                .into_iter()
                .max_by_key(|&(_, count)| count)
                .map(|(col, _)| col)
                .unwrap_or(width / 2);

            // Find contiguous available LC slots in the best column.
            // Each tile has 8 LCs (bel_index 0..7). A chain of N needs N consecutive slots.
            // Search expanding from best_col.
            let placed = self.find_consecutive_lc_slots(
                chain, best_col, n, width, height, used_bels, result,
            );

            if !placed {
                // Fallback: try every column
                let mut found = false;
                for col in 0..width {
                    if col == best_col {
                        continue;
                    }
                    if self.find_consecutive_lc_slots(
                        chain, col, n, width, height, used_bels, result,
                    ) {
                        found = true;
                        break;
                    }
                }
                if !found {
                    // Last resort: place without consecutive constraint
                    for &cell_id in &chain.cells {
                        let loc = placement.placements.get(&cell_id).cloned().unwrap_or(
                            PlacementLoc::new(best_col, height / 2, 0, BelType::Carry),
                        );
                        let legal = self.find_legal_location(
                            loc.tile_x,
                            loc.tile_y,
                            BelType::Carry,
                            used_bels,
                        )?;
                        used_bels.insert(
                            (legal.tile_x, legal.tile_y, legal.bel_index),
                            cell_id,
                        );
                        result.placements.insert(cell_id, legal);
                        chain_cells.insert(cell_id);
                    }
                    continue;
                }
            }

            for &cell_id in &chain.cells {
                chain_cells.insert(cell_id);
            }

            // Also place associated LUTs at same (tile, lc) as their carry
            for (i, lut_opt) in chain.associated_luts.iter().enumerate() {
                if let Some(lut_id) = lut_opt {
                    if let Some(carry_loc) = result.placements.get(&chain.cells[i]) {
                        // Place LUT at same tile and LC index
                        let lut_loc = PlacementLoc::new(
                            carry_loc.tile_x,
                            carry_loc.tile_y,
                            carry_loc.bel_index,
                            BelType::Lut4,
                        );
                        result.placements.insert(*lut_id, lut_loc);
                        chain_cells.insert(*lut_id);
                    }
                }
            }

            // Also place associated DFFs
            for (i, dff_opt) in chain.associated_dffs.iter().enumerate() {
                if let Some(dff_id) = dff_opt {
                    if let Some(carry_loc) = result.placements.get(&chain.cells[i]) {
                        let dff_loc = PlacementLoc::new(
                            carry_loc.tile_x,
                            carry_loc.tile_y,
                            carry_loc.bel_index,
                            BelType::Dff,
                        );
                        result.placements.insert(*dff_id, dff_loc);
                        chain_cells.insert(*dff_id);
                    }
                }
            }
        }

        Ok(chain_cells)
    }

    /// Try to find N consecutive LC slots in a given column.
    /// Returns true if successful and updates result + used_bels.
    #[allow(clippy::too_many_arguments)]
    fn find_consecutive_lc_slots(
        &self,
        chain: &CarryChain,
        col: u32,
        n: usize,
        _width: u32,
        height: u32,
        used_bels: &mut HashMap<(u32, u32, usize), CellId>,
        result: &mut PlacementResult,
    ) -> bool {
        // Collect all available LC slots in this column (sorted by tile_y, then lc_index)
        let mut available_slots: Vec<(u32, usize)> = Vec::new(); // (tile_y, bel_index)

        for ty in 1..height.saturating_sub(1) {
            // Skip boundary tiles (0 and height-1 are IO)
            if let Some(tile) = self.device.tile_at(col, ty) {
                for (bel_idx, bel) in tile.bels().iter().enumerate() {
                    if (bel.bel_type == BelType::Carry
                        || bel.bel_type == BelType::Lut4
                        || bel.bel_type == BelType::Dff)
                        && !used_bels.contains_key(&(col, ty, bel_idx))
                    {
                        available_slots.push((ty, bel_idx));
                    }
                }
            }
        }

        // Find a contiguous run of N slots
        if available_slots.len() < n {
            return false;
        }

        // Try to find N consecutive slots (consecutive means bel_idx increments,
        // wrapping across tiles: tile_y increments when bel_idx reaches 8)
        for start in 0..=(available_slots.len() - n) {
            let first = available_slots[start];
            let mut consecutive = true;

            for offset in 1..n {
                let expected_lc = first.1 + offset;
                let expected_ty = first.0 + (expected_lc as u32 / 8);
                let expected_idx = expected_lc % 8;

                let actual = available_slots[start + offset];
                if actual.0 != expected_ty || actual.1 != expected_idx {
                    consecutive = false;
                    break;
                }
            }

            if consecutive {
                // Place chain cells in these slots
                for (i, &cell_id) in chain.cells.iter().enumerate() {
                    let (ty, bel_idx) = available_slots[start + i];
                    let loc = PlacementLoc::new(col, ty, bel_idx, BelType::Carry);
                    used_bels.insert((col, ty, bel_idx), cell_id);
                    result.placements.insert(cell_id, loc);
                }
                return true;
            }
        }

        false
    }

    /// Find a legal location near the given coordinates
    fn find_legal_location(
        &self,
        x: u32,
        y: u32,
        bel_type: BelType,
        used: &HashMap<(u32, u32, usize), CellId>,
    ) -> Result<PlacementLoc> {
        let (width, height) = self.device.grid_size();

        // Search in expanding rings
        for radius in 0..width.max(height) {
            // Generate coordinates in the ring
            for dy in -(radius as i32)..=(radius as i32) {
                for dx in -(radius as i32)..=(radius as i32) {
                    // Only process the ring (not the interior)
                    if dx.unsigned_abs() != radius && dy.unsigned_abs() != radius {
                        continue;
                    }

                    let tx = (x as i32 + dx).clamp(0, (width - 1) as i32) as u32;
                    let ty = (y as i32 + dy).clamp(0, (height - 1) as i32) as u32;

                    if let Some(tile) = self.device.tile_at(tx, ty) {
                        // Find unused BEL of matching type
                        for (idx, bel) in tile.bels().iter().enumerate() {
                            let is_compatible = bel.bel_type == bel_type
                                || self.bel_types_compatible(bel.bel_type, bel_type);

                            if is_compatible && !used.contains_key(&(tx, ty, idx)) {
                                return Ok(PlacementLoc::new(tx, ty, idx, bel.bel_type));
                            }
                        }
                    }
                }
            }
        }

        Err(PlaceRouteError::PlacementFailed(format!(
            "No legal location found for BEL type {:?} near ({}, {})",
            bel_type, x, y
        )))
    }

    /// Check if BEL types are compatible
    fn bel_types_compatible(&self, available: BelType, required: BelType) -> bool {
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
