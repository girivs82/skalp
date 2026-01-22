//! Placement Legalization
//!
//! Snaps analytical placement to valid BEL sites while minimizing displacement.

use super::{BelType, PlacementLoc, PlacementResult};
use crate::device::Device;
use crate::error::{PlaceRouteError, Result};
use skalp_lir::gate_netlist::{CellId, GateNetlist};
use std::collections::HashMap;

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
        let mut result = PlacementResult::new();
        let mut used_bels: HashMap<(u32, u32, usize), CellId> = HashMap::new();

        // Sort cells by their current placement (process from bottom-left)
        let mut cells: Vec<(CellId, PlacementLoc)> = placement.placements.into_iter().collect();
        cells.sort_by(|a, b| {
            let a_score = a.1.tile_y * 1000 + a.1.tile_x;
            let b_score = b.1.tile_y * 1000 + b.1.tile_x;
            a_score.cmp(&b_score)
        });

        // Legalize each cell
        for (cell_id, loc) in cells {
            let _cell = netlist.get_cell(cell_id);
            let bel_type = loc.bel_type;

            // Find nearest valid and unused BEL
            let legal_loc =
                self.find_legal_location(loc.tile_x, loc.tile_y, bel_type, &used_bels)?;

            // Mark BEL as used
            used_bels.insert(
                (legal_loc.tile_x, legal_loc.tile_y, legal_loc.bel_index),
                cell_id,
            );

            result.placements.insert(cell_id, legal_loc);
        }

        Ok(result)
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
