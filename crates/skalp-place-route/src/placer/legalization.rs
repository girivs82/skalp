//! Placement Legalization
//!
//! Snaps analytical placement to valid BEL sites while minimizing displacement.

use super::sites::{bel_types_compatible, SiteMap};
use super::{BelType, PlacementLoc, PlacementResult};
use crate::collections::{HashMap, HashMapExt, HashSet, HashSetExt};
use crate::device::{CarryChainLayout, Device, TileType};
use crate::error::{PlaceRouteError, Result};
use crate::packing::CarryChain;
use skalp_lir::gate_netlist::{CellId, GateNetlist};

/// Legalizer for snapping placements to valid BEL sites
pub struct Legalizer<'a, D: Device> {
    device: &'a D,
    /// Share of each tile's BELs of a type cells may take
    /// ([`SiteMap::with_fill_limit`])
    fill_limit: f64,
}

impl<'a, D: Device> Legalizer<'a, D> {
    /// Create a new legalizer
    pub fn new(device: &'a D) -> Self {
        Self {
            device,
            fill_limit: 1.0,
        }
    }

    /// Fill each tile's BELs of a type at most this far (0..=1)
    pub fn with_fill_limit(mut self, fill_limit: f64) -> Self {
        self.fill_limit = fill_limit;
        self
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
        _netlist: &GateNetlist,
        carry_chains: &[CarryChain],
    ) -> Result<PlacementResult> {
        let mut result = PlacementResult::new();
        let mut used_bels: HashMap<(u32, u32, usize), CellId> = HashMap::new();

        // Phase 1: Legalize carry chains into consecutive carry positions
        let chain_cells = match self.device.carry_chain_layout() {
            Some(layout) => self.legalize_carry_chains_with_layout(
                &placement,
                carry_chains,
                &layout,
                &mut result,
                &mut used_bels,
            )?,
            None => {
                self.legalize_carry_chains(&placement, carry_chains, &mut result, &mut used_bels)?
            }
        };

        // Phase 2: Legalize remaining cells
        let mut cells: Vec<(CellId, PlacementLoc)> = placement
            .placements
            .into_iter()
            .filter(|(id, _)| !chain_cells.contains(id))
            .collect();
        // Distributed RAM first: its site needs six free LUTs of one tile,
        // which ordinary logic placed earlier would scatter
        cells.sort_by_key(|(_, loc)| {
            (
                loc.bel_type != BelType::DistRam,
                loc.tile_y * 1000 + loc.tile_x,
            )
        });

        let mut sites = SiteMap::new(self.device).with_fill_limit(self.fill_limit);
        for &(x, y, bel) in used_bels.keys() {
            sites.take(x, y, bel);
        }
        for (cell_id, loc) in cells {
            let bel_type = loc.bel_type;
            let (tx, ty, idx) = sites
                .take_nearest(
                    loc.tile_x as f64,
                    loc.tile_y as f64,
                    bel_type,
                    bel_types_compatible,
                )
                .ok_or_else(|| {
                    PlaceRouteError::PlacementFailed(format!(
                        "No legal location found for BEL type {:?} near ({}, {})",
                        bel_type, loc.tile_x, loc.tile_y
                    ))
                })?;
            // Record the BEL's own type, as the ring search always has
            let placed_type = self
                .device
                .tile_at(tx, ty)
                .and_then(|t| t.bels().get(idx).map(|b| b.bel_type))
                .unwrap_or(bel_type);
            result
                .placements
                .insert(cell_id, PlacementLoc::new(tx, ty, idx, placed_type));
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
            let placed = self
                .find_consecutive_lc_slots(chain, best_col, n, width, height, used_bels, result);

            if !placed {
                // Fallback: try every column
                let mut found = false;
                for col in 0..width {
                    if col == best_col {
                        continue;
                    }
                    if self
                        .find_consecutive_lc_slots(chain, col, n, width, height, used_bels, result)
                    {
                        found = true;
                        break;
                    }
                }
                if !found {
                    // Last resort: place without consecutive constraint
                    for &cell_id in &chain.cells {
                        let loc =
                            placement.placements.get(&cell_id).cloned().unwrap_or(
                                PlacementLoc::new(best_col, height / 2, 0, BelType::Carry),
                            );
                        let legal = self.find_legal_location(
                            loc.tile_x,
                            loc.tile_y,
                            BelType::Carry,
                            used_bels,
                        )?;
                        used_bels.insert((legal.tile_x, legal.tile_y, legal.bel_index), cell_id);
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
    ///
    /// Each iCE40 logic tile has 8 Logic Cells (LC 0-7). In the device model,
    /// each LC has a LUT4 BEL at even index (2*lc) and a DFF BEL at odd index
    /// (2*lc+1). Carry chains need consecutive LCs, so we track LC indices (0-7)
    /// and map back to device BEL indices when placing.
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
        // Collect available LC slots (one per LC, indexed 0-7) sorted by tile_y, then lc
        let mut available_slots: Vec<(u32, usize)> = Vec::new(); // (tile_y, lc_index)

        for ty in 1..height.saturating_sub(1) {
            // Skip boundary tiles (0 and height-1 are IO)
            if let Some(tile) = self.device.tile_at(col, ty) {
                let bels = tile.bels();
                for lc in 0..8usize {
                    let lut_bel_idx = lc * 2;
                    let dff_bel_idx = lc * 2 + 1;
                    // LC is available if it has a LUT4 BEL and neither slot is used
                    if lut_bel_idx < bels.len()
                        && bels[lut_bel_idx].bel_type == BelType::Lut4
                        && !used_bels.contains_key(&(col, ty, lut_bel_idx))
                        && !used_bels.contains_key(&(col, ty, dff_bel_idx))
                    {
                        available_slots.push((ty, lc));
                    }
                }
            }
        }

        // Find a contiguous run of N slots
        if available_slots.len() < n {
            return false;
        }

        // Try to find N consecutive LC slots. Consecutive means LC index increments
        // by 1 each step, wrapping across tiles (tile_y increments when LC reaches 8).
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
                // Place chain cells at the LUT BEL index of each LC
                for (i, &cell_id) in chain.cells.iter().enumerate() {
                    let (ty, lc_idx) = available_slots[start + i];
                    let bel_idx = lc_idx * 2; // Map LC index to device LUT4 BEL index
                    let loc = PlacementLoc::new(col, ty, bel_idx, BelType::Carry);
                    // Mark both LUT and DFF slots as used to prevent other cells
                    // from being placed in the same LC
                    used_bels.insert((col, ty, bel_idx), cell_id);
                    used_bels.insert((col, ty, bel_idx + 1), cell_id);
                    result.placements.insert(cell_id, loc);
                }
                return true;
            }
        }

        false
    }

    /// Nearest unused BEL that can hold `bel_type`, given the BELs already used.
    /// Only the carry-chain fallback needs this; bulk legalization uses [`SiteMap`].
    fn find_legal_location(
        &self,
        x: u32,
        y: u32,
        bel_type: BelType,
        used: &HashMap<(u32, u32, usize), CellId>,
    ) -> Result<PlacementLoc> {
        let mut sites = SiteMap::new(self.device).with_fill_limit(self.fill_limit);
        for &(ux, uy, ubel) in used.keys() {
            sites.take(ux, uy, ubel);
        }
        let (tx, ty, idx) = sites
            .find_nearest(x as f64, y as f64, bel_type, &bel_types_compatible)
            .ok_or_else(|| {
                PlaceRouteError::PlacementFailed(format!(
                    "No legal location found for BEL type {:?} near ({}, {})",
                    bel_type, x, y
                ))
            })?;
        let placed_type = self
            .device
            .tile_at(tx, ty)
            .and_then(|t| t.bels().get(idx).map(|b| b.bel_type))
            .unwrap_or(bel_type);
        Ok(PlacementLoc::new(tx, ty, idx, placed_type))
    }

    /// Place each carry chain on consecutive carry positions of `layout`
    /// (slot 0..n of a tile, then slot 0 of the tile one `step` along), as
    /// close as possible to where analytical placement put its cells. A chain
    /// position takes its carry BEL and the LUT BELs the carry cell uses.
    fn legalize_carry_chains_with_layout(
        &self,
        placement: &PlacementResult,
        carry_chains: &[CarryChain],
        layout: &CarryChainLayout,
        result: &mut PlacementResult,
        used_bels: &mut HashMap<(u32, u32, usize), CellId>,
    ) -> Result<HashSet<CellId>> {
        let mut chain_cells = HashSet::new();
        let (width, height) = self.device.grid_size();
        let slots = layout.slots.len();
        if slots == 0 {
            return Ok(chain_cells);
        }
        let max_bel = layout
            .slots
            .iter()
            .flat_map(|s| std::iter::once(s.carry_bel).chain(s.lut_bels.iter().copied()))
            .max()
            .unwrap_or(0);

        // Position `i` steps along the chain from (x, y, slot).
        let advance = |x: u32, y: u32, slot: usize, i: usize| -> Option<(u32, u32, usize)> {
            let p = slot + i;
            let tiles = (p / slots) as i64;
            let nx = x as i64 + layout.step.0 as i64 * tiles;
            let ny = y as i64 + layout.step.1 as i64 * tiles;
            if nx < 0 || ny < 0 || nx >= width as i64 || ny >= height as i64 {
                return None;
            }
            Some((nx as u32, ny as u32, p % slots))
        };
        let position_free =
            |used: &HashMap<(u32, u32, usize), CellId>, x: u32, y: u32, slot: usize| -> bool {
                let Some(tile) = self.device.tile_at(x, y) else {
                    return false;
                };
                if tile.tile_type() != TileType::Logic || tile.bels().len() <= max_bel {
                    return false;
                }
                let s = &layout.slots[slot];
                !used.contains_key(&(x, y, s.carry_bel))
                    && s.lut_bels.iter().all(|&l| !used.contains_key(&(x, y, l)))
            };

        // Longest chains first: they are the hardest to fit.
        let mut order: Vec<&CarryChain> = carry_chains
            .iter()
            .filter(|c| !c.cells.is_empty())
            .collect();
        order.sort_by_key(|c| std::cmp::Reverse(c.cells.len()));

        for chain in order {
            let n = chain.cells.len();
            // Target: centroid of the chain's analytical positions, shifted so
            // the chain is centred on it along the chain direction.
            let locs: Vec<&PlacementLoc> = chain
                .cells
                .iter()
                .filter_map(|c| placement.placements.get(c))
                .collect();
            let (cx, cy) = if locs.is_empty() {
                (width as f64 / 2.0, height as f64 / 2.0)
            } else {
                (
                    locs.iter().map(|l| l.tile_x as f64).sum::<f64>() / locs.len() as f64,
                    locs.iter().map(|l| l.tile_y as f64).sum::<f64>() / locs.len() as f64,
                )
            };
            let span_tiles = n.div_ceil(slots) as f64;
            let tx = (cx - layout.step.0 as f64 * span_tiles / 2.0).round();
            let ty = (cy - layout.step.1 as f64 * span_tiles / 2.0).round();
            let tx = tx.clamp(0.0, (width - 1) as f64) as i64;
            let ty = ty.clamp(0.0, (height - 1) as f64) as i64;

            let fits = |used: &HashMap<(u32, u32, usize), CellId>, x: u32, y: u32, slot: usize| {
                (0..n).all(|i| {
                    advance(x, y, slot, i)
                        .is_some_and(|(px, py, ps)| position_free(used, px, py, ps))
                })
            };

            // Nearest start (Chebyshev rings around the target) where the whole chain fits.
            let mut start = None;
            'search: for r in 0..=(width.max(height) as i64) {
                for dy in -r..=r {
                    for dx in -r..=r {
                        if dx.abs() != r && dy.abs() != r {
                            continue;
                        }
                        let (x, y) = (tx + dx, ty + dy);
                        if x < 0 || y < 0 || x >= width as i64 || y >= height as i64 {
                            continue;
                        }
                        for slot in 0..slots {
                            if fits(used_bels, x as u32, y as u32, slot) {
                                start = Some((x as u32, y as u32, slot));
                                break 'search;
                            }
                        }
                    }
                }
            }
            let Some((sx, sy, sslot)) = start else {
                return Err(PlaceRouteError::PlacementFailed(format!(
                    "no room for a carry chain of {} cells",
                    n
                )));
            };

            for (i, &cell_id) in chain.cells.iter().enumerate() {
                let (x, y, slot) = advance(sx, sy, sslot, i).expect("checked by fits");
                let s = &layout.slots[slot];
                used_bels.insert((x, y, s.carry_bel), cell_id);
                for &l in &s.lut_bels {
                    used_bels.insert((x, y, l), cell_id);
                }
                result.placements.insert(
                    cell_id,
                    PlacementLoc::new(x, y, s.carry_bel, BelType::Carry),
                );
                chain_cells.insert(cell_id);
            }
        }

        Ok(chain_cells)
    }
}
