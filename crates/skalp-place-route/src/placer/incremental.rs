//! Placing a netlist again after a small change, keeping the placement it
//! had.
//!
//! A flow that changes a placed and routed netlist a little (a few cells
//! added or removed) and then places it again would otherwise start over:
//! every cell moves, every route changes, and with them every delay the
//! change was meant to fix. Here each cell the earlier placement holds (found
//! by its path) keeps its BEL, and each new cell goes to the free BEL
//! nearest the cells it connects to.

use super::control_sets::control_sets;
use super::sites::{bel_types_compatible, SiteMap};
use super::{cell_bel_type, is_constant_cell, PlacementLoc, PlacementResult, Placer};
use crate::collections::{HashMap, HashSet, HashSetExt};
use crate::device::{BelType, Device};
use skalp_lir::gate_netlist::{CellId, GateNetlist};

/// What [`Placer::place_around`] did
#[derive(Debug, Clone, Default)]
pub struct PlacedAround {
    /// The placement
    pub placement: PlacementResult,
    /// Cells that kept their BEL
    pub kept: usize,
    /// Cells placed beside the cells they connect to
    pub added: usize,
}

impl<D: Device + Clone> Placer<D> {
    /// Place `netlist` keeping the BEL each of its cells had in `previous`
    /// (a placement by cell path), and putting each cell `previous` lacks
    /// next to the cells it connects to. `None` when that cannot be done:
    /// a cell is not plain logic (a LUT or flip-flop: hard blocks, carry
    /// chains and RAM need the full placer), a kept BEL no longer suits its
    /// cell, or no BEL is free nearby. The caller then places from scratch.
    pub fn place_around(
        &self,
        netlist: &GateNetlist,
        previous: &HashMap<String, PlacementLoc>,
    ) -> Option<PlacedAround> {
        let sets = control_sets(netlist);
        let fill_limit = self.fill_limit(netlist);
        let mut sites = SiteMap::new(&self.device).with_fill_limit(fill_limit);
        // A carry cell also uses its slot's LUTs, which the site map does
        // not know of
        let carry_luts: HashMap<usize, Vec<usize>> = self
            .device
            .carry_chain_layout()
            .map(|layout| {
                layout
                    .slots
                    .into_iter()
                    .map(|s| (s.carry_bel, s.lut_bels))
                    .collect()
            })
            .unwrap_or_default();
        let mut placement = PlacementResult::new();
        let mut new_cells = Vec::new();
        let mut seen_paths: HashSet<&str> = HashSet::new();
        for cell in &netlist.cells {
            if is_constant_cell(&cell.cell_type) {
                continue;
            }
            // Two cells with one path cannot both be the earlier one
            let earlier = seen_paths
                .insert(cell.path.as_str())
                .then(|| previous.get(&cell.path))
                .flatten();
            match earlier {
                Some(loc) => {
                    let set = sets.get(&cell.id).copied();
                    let fits = bel_types_compatible(loc.bel_type, cell_bel_type(cell));
                    if !fits || !sites.retake(loc.tile_x, loc.tile_y, loc.bel_index, set) {
                        log::info!(
                            "placer: '{}' cannot keep its BEL; placing from scratch",
                            cell.path
                        );
                        return None;
                    }
                    if loc.bel_type == BelType::Carry {
                        for &lut in carry_luts.get(&loc.bel_index).into_iter().flatten() {
                            sites.take(loc.tile_x, loc.tile_y, lut, None);
                        }
                    }
                    placement.placements.insert(cell.id, *loc);
                }
                None => new_cells.push(cell.id),
            }
        }
        let kept = placement.placements.len();
        // Place the new cells nearest the ones they connect to, those next
        // to placed cells first (a new chain grows out from its placed end)
        let mut pending: Vec<CellId> = new_cells;
        while !pending.is_empty() {
            let before = pending.len();
            let mut still = Vec::new();
            for id in pending {
                let cell = &netlist.cells[id.0 as usize];
                let required = cell_bel_type(cell);
                if !matches!(
                    required,
                    BelType::Lut4 | BelType::Dff | BelType::DffE | BelType::DffSr | BelType::DffSrE
                ) {
                    log::info!(
                        "placer: new cell '{}' needs a {:?} BEL; placing from scratch",
                        cell.path,
                        required
                    );
                    return None;
                }
                let Some((x, y)) = self.neighbours_centre(netlist, &placement, id) else {
                    still.push(id);
                    continue;
                };
                let set = sets.get(&id).copied();
                let (tx, ty, bel) =
                    sites.take_nearest(x, y, required, set, bel_types_compatible)?;
                let bel_type = self
                    .device
                    .tile_at(tx, ty)
                    .and_then(|t| t.bels().get(bel).map(|b| b.bel_type))
                    .unwrap_or(required);
                placement
                    .placements
                    .insert(id, PlacementLoc::new(tx, ty, bel, bel_type));
            }
            if still.len() == before {
                // Connected to nothing placed: no place to start from
                log::info!(
                    "placer: {} new cells connect to no placed cell; placing from scratch",
                    still.len()
                );
                return None;
            }
            pending = still;
        }
        let added = placement.placements.len() - kept;
        Self::check_control_sets(&self.device, &placement, netlist).ok()?;
        placement.wirelength = self.calculate_wirelength(&placement, netlist);
        placement.utilization = self.calculate_utilization(&placement);
        placement.cost = self.calculate_cost(&placement, netlist);
        Some(PlacedAround {
            placement,
            kept,
            added,
        })
    }

    /// The mean tile of the placed cells cell `id` connects to (its
    /// inputs' drivers first; its outputs' loads when no driver is placed)
    fn neighbours_centre(
        &self,
        netlist: &GateNetlist,
        placement: &PlacementResult,
        id: CellId,
    ) -> Option<(f64, f64)> {
        let cell = &netlist.cells[id.0 as usize];
        let mean = |locs: Vec<(u32, u32)>| {
            (!locs.is_empty()).then(|| {
                let n = locs.len() as f64;
                (
                    locs.iter().map(|l| l.0 as f64).sum::<f64>() / n,
                    locs.iter().map(|l| l.1 as f64).sum::<f64>() / n,
                )
            })
        };
        let placed_cell = |c: CellId| {
            let other = netlist.get_cell(c)?;
            if is_constant_cell(&other.cell_type) {
                return None;
            }
            placement.get(c).map(PlacementLoc::coords)
        };
        let drivers: Vec<(u32, u32)> = cell
            .inputs
            .iter()
            .filter_map(|&net| netlist.nets.get(netlist.resolve_alias(net).0 as usize))
            .filter_map(|net| net.driver.and_then(placed_cell))
            .collect();
        mean(drivers).or_else(|| {
            let loads: Vec<(u32, u32)> = cell
                .outputs
                .iter()
                .filter_map(|&net| netlist.nets.get(net.0 as usize))
                .flat_map(|net| net.fanout.iter().filter_map(|&(c, _)| placed_cell(c)))
                .collect();
            mean(loads)
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::collections::HashMapExt;
    use crate::device::ice40::{Ice40Device, Ice40Variant};
    use crate::placer::PlacerConfig;
    use skalp_lir::gate_netlist::{Cell, GateNet, GateNetId};

    /// A chain of `n` LUTs from net `a`, named `top.c<i>`
    fn chain(n: usize) -> GateNetlist {
        let mut netlist = GateNetlist::new("chain".to_string(), "ice40".to_string());
        let mut net = netlist.add_net(GateNet::new(GateNetId(0), "a".into()));
        for i in 0..n {
            net = add_lut(&mut netlist, &format!("top.c{i}"), net);
        }
        netlist
    }

    fn add_lut(netlist: &mut GateNetlist, path: &str, input: GateNetId) -> GateNetId {
        let out = netlist.add_net(GateNet::new(GateNetId(0), format!("{path}_o")));
        netlist.add_cell(Cell::new_comb(
            CellId(0),
            "SB_LUT4".to_string(),
            "ice40".to_string(),
            0.0,
            path.to_string(),
            vec![input],
            vec![out],
        ));
        netlist.rebuild_net_connectivity();
        out
    }

    fn by_path(
        netlist: &GateNetlist,
        placement: &PlacementResult,
    ) -> HashMap<String, PlacementLoc> {
        netlist
            .cells
            .iter()
            .filter_map(|c| Some((c.path.clone(), *placement.get(c.id)?)))
            .collect()
    }

    /// Cells keep their BELs; a cell added at the end of a chain goes next
    /// to the cell driving it; a removed cell's BEL is free again
    #[test]
    fn placing_around_keeps_cells_and_puts_new_ones_beside_their_driver() {
        let device = Ice40Device::new(Ice40Variant::Hx1k);
        let mut netlist = chain(40);
        let mut placer = Placer::new(PlacerConfig::default(), device.clone());
        let first = placer.place(&netlist).expect("place");
        let previous = by_path(&netlist, &first);

        let last = netlist.cells.last().unwrap().outputs[0];
        add_lut(&mut netlist, "top.extra", last);
        let around = placer
            .place_around(&netlist, &previous)
            .expect("place around");
        assert_eq!((around.kept, around.added), (40, 1));
        let again = by_path(&netlist, &around.placement);
        for (path, loc) in &previous {
            assert_eq!(again[path], *loc, "{path} moved");
        }
        let extra = again["top.extra"];
        let driver = previous["top.c39"];
        let distance = extra.tile_x.abs_diff(driver.tile_x) + extra.tile_y.abs_diff(driver.tile_y);
        assert!(
            distance <= 1,
            "new cell {extra:?} far from its driver {driver:?}"
        );
        assert!(
            again.values().filter(|l| **l == extra).count() == 1,
            "new cell shares a BEL"
        );
    }

    /// A new cell that needs a block the incremental placement does not
    /// handle (or a kept BEL that no longer suits its cell) sends the caller
    /// back to the full placer
    #[test]
    fn placing_around_refuses_what_it_cannot_place() {
        let device = Ice40Device::new(Ice40Variant::Hx1k);
        let netlist = chain(4);
        let mut placer = Placer::new(PlacerConfig::default(), device.clone());
        let first = placer.place(&netlist).expect("place");
        let mut previous = by_path(&netlist, &first);

        let mut with_ram = netlist.clone();
        let out = with_ram.add_net(GateNet::new(GateNetId(0), "r".into()));
        let input = with_ram.cells[3].outputs[0];
        with_ram.add_cell(Cell::new_comb(
            CellId(0),
            "SB_RAM40_4K".to_string(),
            "ice40".to_string(),
            0.0,
            "top.ram".to_string(),
            vec![input],
            vec![out],
        ));
        with_ram.rebuild_net_connectivity();
        assert!(placer.place_around(&with_ram, &previous).is_none());

        // Two cells said to have held one BEL
        let shared = previous["top.c0"];
        previous.insert("top.c1".to_string(), shared);
        assert!(placer.place_around(&netlist, &previous).is_none());
    }

    /// A kept carry cell keeps its slot's LUTs: new LUTs go elsewhere
    #[test]
    fn placing_around_leaves_a_carry_cells_luts_alone() {
        use crate::device::nexus::{NexusDevice, NexusVariant};
        use crate::device::TileType;
        let device = NexusDevice::new(NexusVariant::Lfcpnx100);
        let layout = device.carry_chain_layout().expect("carry layout");
        let (width, height) = device.grid_size();
        let (x, y) = (0..height)
            .flat_map(|y| (0..width).map(move |x| (x, y)))
            .find(|&(x, y)| {
                device
                    .tile_at(x, y)
                    .is_some_and(|t| t.tile_type() == TileType::Logic && !t.bels().is_empty())
            })
            .expect("a logic tile");
        let slot = &layout.slots[0];

        let mut netlist = GateNetlist::new("carry".to_string(), "nexus".to_string());
        let a = netlist.add_net(GateNet::new(GateNetId(0), "a".into()));
        let s0 = netlist.add_net(GateNet::new(GateNetId(0), "s0".into()));
        let mut carry = Cell::new_comb(
            CellId(0),
            "CCU2".to_string(),
            "nexus".to_string(),
            0.0,
            "top.carry".to_string(),
            vec![a],
            vec![s0],
        );
        carry.function = Some(skalp_lir::CellFunction::Ccu2);
        netlist.add_cell(carry);
        netlist.rebuild_net_connectivity();
        for i in 0..8 {
            add_lut(&mut netlist, &format!("top.l{i}"), s0);
        }
        let mut previous = HashMap::new();
        previous.insert(
            "top.carry".to_string(),
            PlacementLoc::new(x, y, slot.carry_bel, BelType::Carry),
        );
        let placer = Placer::new(PlacerConfig::default(), device.clone());
        let around = placer
            .place_around(&netlist, &previous)
            .expect("place around");
        assert_eq!((around.kept, around.added), (1, 8));
        for cell in netlist.cells.iter().filter(|c| c.path.starts_with("top.l")) {
            let loc = around.placement.get(cell.id).unwrap();
            assert!(
                !(loc.coords() == (x, y) && slot.lut_bels.contains(&loc.bel_index)),
                "{} took a LUT of the carry cell's slot",
                cell.path
            );
        }
    }
}
