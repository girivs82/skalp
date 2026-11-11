//! Flip-flop control sets.
//!
//! The flip-flops of one [`Device::ff_control_group`] (a Lattice slice, a
//! whole iCE40 logic tile) share their clock, clock enable and set/reset
//! inputs, so only flip-flops whose control signals all match may sit in the
//! same group. A flip-flop without a reset placed beside one with a reset
//! would otherwise be reset with it, or (on Nexus, where the unused reset is
//! held off by inverting the slice's LSR input) hold the other in reset.

use crate::bitstream::prepare::{constant_value, ff_pin};
use crate::collections::{HashMap, HashMapExt};
use crate::device::{Device, FF_PIN_CE, FF_PIN_LSR};
use skalp_lir::gate_netlist::{CellId, GateNetId, GateNetlist};

use super::{cell_bel_type, PlacementLoc, PlacementResult};

/// The control signals of a flip-flop. A clock enable tied high and a
/// set/reset tied low are the same as none.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ControlSet {
    pub clock: Option<GateNetId>,
    pub reset: Option<GateNetId>,
    pub enable: Option<GateNetId>,
}

/// The control set of every flip-flop of `netlist`.
pub fn control_sets(netlist: &GateNetlist) -> HashMap<CellId, ControlSet> {
    let constants: HashMap<GateNetId, bool> = netlist
        .cells
        .iter()
        .filter_map(|c| Some((*c.outputs.first()?, constant_value(c)?)))
        .collect();
    let mut sets = HashMap::new();
    for cell in &netlist.cells {
        if cell.clock.is_none() || !cell_bel_type(cell).is_ff() {
            continue;
        }
        let mut set = ControlSet {
            clock: cell.clock,
            reset: cell.reset.filter(|net| constants.get(net) != Some(&false)),
            enable: None,
        };
        for (index, &net) in cell.inputs.iter().enumerate() {
            match ff_pin(cell, index) {
                FF_PIN_LSR if constants.get(&net) != Some(&false) => set.reset = Some(net),
                FF_PIN_CE if constants.get(&net) != Some(&true) => set.enable = Some(net),
                _ => {}
            }
        }
        sets.insert(cell.id, set);
    }
    sets
}

/// Flip-flops in one control group whose control sets differ, as
/// (tile x, tile y, group, the cells there).
pub fn control_set_conflicts<D: Device + ?Sized>(
    device: &D,
    placement: &PlacementResult,
    sets: &HashMap<CellId, ControlSet>,
) -> Vec<(u32, u32, usize, Vec<CellId>)> {
    let mut groups: HashMap<(u32, u32, usize), Vec<CellId>> = HashMap::new();
    for (&cell, loc) in &placement.placements {
        if !sets.contains_key(&cell) {
            continue;
        }
        if let Some(group) = device.ff_control_group(loc.tile_x, loc.tile_y, loc.bel_index) {
            groups
                .entry((loc.tile_x, loc.tile_y, group))
                .or_default()
                .push(cell);
        }
    }
    let mut conflicts: Vec<_> = groups
        .into_iter()
        .filter(|(_, cells)| cells.windows(2).any(|w| sets[&w[0]] != sets[&w[1]]))
        .map(|((x, y, g), mut cells)| {
            cells.sort_unstable_by_key(|c| c.0);
            (x, y, g, cells)
        })
        .collect();
    conflicts.sort_unstable_by_key(|c| (c.0, c.1, c.2));
    conflicts
}

/// Whether `cell` may move to `to`, given where every other cell is
/// (`occupant`): every other flip-flop of the destination's control group,
/// except the cells in `leaving`, must have `cell`'s control set.
pub fn fits_control_group<D: Device + ?Sized>(
    device: &D,
    sets: &HashMap<CellId, ControlSet>,
    cell: CellId,
    to: &PlacementLoc,
    leaving: &[CellId],
    occupant: impl Fn(u32, u32, usize) -> Option<CellId>,
) -> bool {
    let Some(set) = sets.get(&cell) else {
        return true;
    };
    let Some(group) = device.ff_control_group(to.tile_x, to.tile_y, to.bel_index) else {
        return true;
    };
    let Some(tile) = device.tile_at(to.tile_x, to.tile_y) else {
        return true;
    };
    (0..tile.bels().len())
        .filter(|&b| b != to.bel_index)
        .filter(|&b| device.ff_control_group(to.tile_x, to.tile_y, b) == Some(group))
        .filter_map(|b| occupant(to.tile_x, to.tile_y, b))
        .filter(|other| *other != cell && !leaving.contains(other))
        .all(|other| sets.get(&other).is_none_or(|s| s == set))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::device::ice40::{Ice40Device, Ice40Variant};
    use crate::device::nexus::{NexusDevice, NexusVariant};
    use crate::placer::{PlacementAlgorithm, Placer, PlacerConfig};
    use skalp_lir::gate_netlist::{Cell, GateNet};
    use skalp_lir::CellFunction;

    /// Flip-flops on one data net, so placement wants them side by side:
    /// `plain` without a reset, `reset` with one, and `other_clock` on a
    /// second clock.
    fn mixed_flip_flops(plain: usize, reset: usize, other_clock: usize) -> GateNetlist {
        let mut netlist = GateNetlist::new("mixed".to_string(), "ice40".to_string());
        let net =
            |n: &mut GateNetlist, name: &str| n.add_net(GateNet::new(GateNetId(0), name.into()));
        let clk = net(&mut netlist, "clk");
        let clk2 = net(&mut netlist, "clk2");
        let rst = net(&mut netlist, "rst");
        let d = net(&mut netlist, "d");
        let kinds = std::iter::repeat_n((clk, None), plain)
            .chain(std::iter::repeat_n((clk, Some(rst)), reset))
            .chain(std::iter::repeat_n((clk2, None), other_clock));
        for (i, (clock, reset)) in kinds.enumerate() {
            let q = net(&mut netlist, &format!("q{i}"));
            let inputs = std::iter::once(d).chain(reset).collect();
            let mut ff = Cell::new_seq(
                CellId(0),
                if reset.is_some() { "SB_DFFR" } else { "SB_DFF" }.to_string(),
                "ice40".to_string(),
                0.0,
                format!("top.r{i}"),
                inputs,
                vec![q],
                clock,
                reset,
            );
            ff.function = Some(if reset.is_some() {
                CellFunction::DffR
            } else {
                CellFunction::Dff
            });
            netlist.add_cell(ff);
        }
        netlist
    }

    #[test]
    fn reset_and_clock_make_control_sets() {
        let netlist = mixed_flip_flops(1, 1, 1);
        let sets = control_sets(&netlist);
        let set = |path: &str| sets[&netlist.cells.iter().find(|c| c.path == path).unwrap().id];
        assert_eq!(set("top.r0").reset, None);
        assert!(set("top.r1").reset.is_some());
        assert_ne!(set("top.r0"), set("top.r1"));
        assert_ne!(set("top.r0"), set("top.r2"));
        assert_eq!(set("top.r0").clock, set("top.r1").clock);
    }

    /// Every placement algorithm keeps flip-flops of different control sets
    /// out of one iCE40 logic tile (whose eight cells share clock, enable
    /// and set/reset), even when the design packs them densely.
    #[test]
    fn placement_keeps_control_sets_apart() {
        let device = Ice40Device::new(Ice40Variant::Hx1k);
        let netlist = mixed_flip_flops(20, 20, 12);
        let sets = control_sets(&netlist);
        for algorithm in [
            PlacementAlgorithm::AnalyticalTimingDriven,
            PlacementAlgorithm::AnalyticalWithRefinement,
            PlacementAlgorithm::SimulatedAnnealing,
            PlacementAlgorithm::Random,
        ] {
            let config = PlacerConfig {
                algorithm: algorithm.clone(),
                max_tile_fill: Some(1.0),
                ..PlacerConfig::default()
            };
            let mut placer = Placer::new(config, device.clone());
            let placement = placer
                .place(&netlist)
                .unwrap_or_else(|e| panic!("{algorithm:?}: {e}"));
            assert_eq!(placement.placements.len(), netlist.cells.len());
            assert!(
                control_set_conflicts(&device, &placement, &sets).is_empty(),
                "{algorithm:?} mixed control sets in a tile"
            );
        }
    }

    /// A placement that mixes control sets in one group is reported.
    #[test]
    fn conflicts_name_the_group_and_cells() {
        let device = Ice40Device::new(Ice40Variant::Hx1k);
        let netlist = mixed_flip_flops(1, 1, 0);
        let sets = control_sets(&netlist);
        let (x, y) = (1..13)
            .flat_map(|x| (1..17).map(move |y| (x, y)))
            .find(|&(x, y)| device.ff_control_group(x, y, 1).is_some())
            .expect("a logic tile");
        let ffs: Vec<usize> = (0..device.tile_at(x, y).unwrap().bels().len())
            .filter(|&b| device.ff_control_group(x, y, b).is_some())
            .take(2)
            .collect();
        let mut placement = PlacementResult::new();
        for (cell, &bel) in netlist.cells.iter().zip(&ffs) {
            placement.placements.insert(
                cell.id,
                PlacementLoc::new(x, y, bel, crate::device::BelType::Dff),
            );
        }
        let conflicts = control_set_conflicts(&device, &placement, &sets);
        assert_eq!(conflicts.len(), 1);
        assert_eq!((conflicts[0].0, conflicts[0].1), (x, y));
        assert_eq!(conflicts[0].3.len(), 2);
    }

    /// The flip-flop BELs of the first Nexus logic tile, in slice order
    /// (two per slice: A, A, B, B, C, C, D, D).
    fn nexus_logic_tile_ffs(device: &NexusDevice) -> (u32, u32, Vec<usize>) {
        let (width, height) = device.grid_size();
        (0..height)
            .flat_map(|y| (0..width).map(move |x| (x, y)))
            .find_map(|(x, y)| {
                let tile = device.tile_at(x, y)?;
                let ffs: Vec<usize> = (0..tile.bels().len())
                    .filter(|&b| tile.bels()[b].bel_type.is_ff())
                    .collect();
                (ffs.len() == 8).then_some((x, y, ffs))
            })
            .expect("a Nexus logic tile")
    }

    /// On Nexus, slices A and B take clock, enable and set/reset from one
    /// set of tile wires (JCLK0, JCE0, JLSR0) and slices C and D from the
    /// other, so the control group is the slice pair, not the slice.
    #[test]
    fn nexus_slice_pairs_share_control_lines() {
        let device = NexusDevice::new(NexusVariant::Lfcpnx100);
        let (x, y, ffs) = nexus_logic_tile_ffs(&device);
        let groups: Vec<Option<usize>> = ffs
            .iter()
            .map(|&b| device.ff_control_group(x, y, b))
            .collect();
        let a_b = groups[0];
        let c_d = groups[4];
        assert!(a_b.is_some() && c_d.is_some());
        assert_ne!(a_b, c_d);
        assert!(groups[..4].iter().all(|g| *g == a_b), "{groups:?}");
        assert!(groups[4..].iter().all(|g| *g == c_d), "{groups:?}");
    }

    /// A flip-flop without a reset in slice A beside flip-flops with one in
    /// slice B is a conflict: on silicon the pair's shared LSR held slice B's
    /// flip-flops in reset, and a processor's ALU stored wrong bits. The same
    /// flip-flops in different slice pairs are fine.
    #[test]
    fn nexus_reset_and_plain_flip_flops_in_one_slice_pair_conflict() {
        let device = NexusDevice::new(NexusVariant::Lfcpnx100);
        let (x, y, ffs) = nexus_logic_tile_ffs(&device);
        let netlist = mixed_flip_flops(1, 2, 0);
        let sets = control_sets(&netlist);
        let place = |bels: [usize; 3]| {
            let mut placement = PlacementResult::new();
            for (cell, bel) in netlist.cells.iter().zip(bels) {
                placement.placements.insert(
                    cell.id,
                    PlacementLoc::new(x, y, ffs[bel], crate::device::BelType::Dff),
                );
            }
            control_set_conflicts(&device, &placement, &sets)
        };
        // r0 (no reset) in slice A, r1 and r2 (reset) in slice B
        let conflicts = place([1, 2, 3]);
        assert_eq!(conflicts.len(), 1);
        assert_eq!(conflicts[0].3.len(), 3);
        // r1 and r2 in slices C and D instead
        assert!(place([1, 4, 6]).is_empty());
    }

    /// The placer keeps reset and plain flip-flops out of one Nexus slice
    /// pair even when the design packs them densely.
    #[test]
    fn nexus_placement_keeps_slice_pairs_to_one_control_set() {
        let device = NexusDevice::new(NexusVariant::Lifcl40);
        let netlist = mixed_flip_flops(20, 20, 12);
        let sets = control_sets(&netlist);
        for algorithm in [
            PlacementAlgorithm::AnalyticalTimingDriven,
            PlacementAlgorithm::SimulatedAnnealing,
        ] {
            let config = PlacerConfig {
                algorithm: algorithm.clone(),
                max_tile_fill: Some(1.0),
                ..PlacerConfig::default()
            };
            let mut placer = Placer::new(config, device.clone());
            let placement = placer
                .place(&netlist)
                .unwrap_or_else(|e| panic!("{algorithm:?}: {e}"));
            assert_eq!(placement.placements.len(), netlist.cells.len());
            assert!(
                control_set_conflicts(&device, &placement, &sets).is_empty(),
                "{algorithm:?} mixed control sets in a slice pair"
            );
        }
    }
}
