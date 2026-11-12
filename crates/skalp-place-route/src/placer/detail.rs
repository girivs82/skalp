//! Directed moves for detailed placement, after annealing.
//!
//! Annealing proposes random moves within a shrinking window, so a move that
//! puts a cell right next to the one it must sit beside is proposed only by
//! chance. On a RISC-V board design that left 106 of 293 LUT-to-flip-flop
//! connections five or more tiles apart, where nextpnr's packer puts every
//! one in the same tile. This module names the moves worth trying instead;
//! the annealer prices each with its own cost (wirelength, timing,
//! congestion) and keeps the ones that lower it.

use super::PlacementResult;
use skalp_lir::gate_netlist::{CellId, GateNetlist};

/// Connections at least this critical get their sink pulled towards the
/// driver.
const CRITICAL: f64 = 0.5;

/// A cell to try moving and the tiles to try, nearest first.
pub(super) struct Candidate {
    pub cell: CellId,
    pub tiles: Vec<(u32, u32)>,
}

/// Moves worth trying on `placement`:
///
/// - each flip-flop whose data input is driven by a combinational cell, into
///   its driver's tile (the connection then stays on the tile's local
///   routing), and failing that the driver into the flip-flop's tile;
/// - the sink of each connection at least [`CRITICAL`] (by
///   `connection_criticalities`, per net in fanout order) that is not
///   already in or next to its driver's tile, into the driver's tile or one
///   beside it.
///
/// Most critical first, so the moves that matter most get the free BELs.
pub(super) fn candidates(
    netlist: &GateNetlist,
    placement: &PlacementResult,
    connection_criticalities: &[Vec<f64>],
) -> Vec<Candidate> {
    let at = |id: CellId| placement.placements.get(&id).map(|l| (l.tile_x, l.tile_y));
    let mut found: Vec<(f64, Candidate)> = Vec::new();
    for (n, net) in netlist.nets.iter().enumerate() {
        let Some(driver) = net.driver else {
            continue;
        };
        let Some(driver_cell) = netlist.get_cell(driver) else {
            continue;
        };
        let Some(from) = at(driver) else {
            continue;
        };
        let combinational = driver_cell.clock.is_none() && !driver_cell.inputs.is_empty();
        for (k, &(sink, pin)) in net.fanout.iter().enumerate() {
            let Some(to) = at(sink) else {
                continue;
            };
            if to == from {
                continue;
            }
            let criticality = connection_criticalities
                .get(n)
                .and_then(|c| c.get(k))
                .copied()
                .unwrap_or(0.0);
            let feeds_register_data = combinational
                && pin == 0
                && netlist
                    .get_cell(sink)
                    .is_some_and(|c| c.clock.is_some() && is_flip_flop(&c.cell_type));
            let far = from.0.abs_diff(to.0) + from.1.abs_diff(to.1) > 1;
            if feeds_register_data {
                // The flip-flop into the driver's tile, or else the driver
                // into the flip-flop's (when the flip-flop's own fanout
                // holds it where it is)
                found.push((
                    2.0 + criticality,
                    Candidate {
                        cell: sink,
                        tiles: vec![from],
                    },
                ));
                found.push((
                    1.0 + criticality,
                    Candidate {
                        cell: driver,
                        tiles: vec![to],
                    },
                ));
            } else if criticality >= CRITICAL && far {
                found.push((
                    criticality,
                    Candidate {
                        cell: sink,
                        tiles: around(from),
                    },
                ));
            }
        }
    }
    found.sort_by(|a, b| b.0.total_cmp(&a.0).then(a.1.cell.0.cmp(&b.1.cell.0)));
    found.into_iter().map(|(_, c)| c).collect()
}

/// A tile and the four beside it, the tile itself first.
fn around((x, y): (u32, u32)) -> Vec<(u32, u32)> {
    let mut tiles = vec![(x, y), (x + 1, y), (x, y + 1)];
    if x > 0 {
        tiles.push((x - 1, y));
    }
    if y > 0 {
        tiles.push((x, y - 1));
    }
    tiles
}

/// Whether a clocked cell type is a plain flip-flop (one data input on pin
/// 0), as opposed to a RAM, DSP or other clocked block.
fn is_flip_flop(cell_type: &str) -> bool {
    let t = cell_type.to_ascii_uppercase();
    t.contains("FF") || t.starts_with("DFF") || t.starts_with("SB_DFF") || t.starts_with("FD1")
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::device::BelType;
    use crate::placer::PlacementLoc;
    use skalp_lir::gate_netlist::{Cell, GateNet, GateNetId};

    /// lut (x=10) -> ff (x=20) and lut -> far comb sink (x=30)
    fn design() -> (GateNetlist, PlacementResult) {
        let mut n = GateNetlist::new("t".to_string(), "test".to_string());
        let clk = n.add_net(GateNet::new(GateNetId(0), "clk".to_string()));
        let a = n.add_net(GateNet::new(GateNetId(0), "a".to_string()));
        let x = n.add_net(GateNet::new(GateNetId(0), "x".to_string()));
        let q = n.add_net(GateNet::new(GateNetId(0), "q".to_string()));
        let y = n.add_net(GateNet::new(GateNetId(0), "y".to_string()));
        let comb = |name: &str, i: GateNetId, o: GateNetId| {
            Cell::new_comb(
                CellId(0),
                "OXIDE_COMB".to_string(),
                "nexus".to_string(),
                0.0,
                name.to_string(),
                vec![i],
                vec![o],
            )
        };
        let lut = n.add_cell(comb("lut", a, x));
        let mut ff = comb("ff", x, q);
        ff.cell_type = "OXIDE_FF".to_string();
        ff.clock = Some(clk);
        let ff = n.add_cell(ff);
        let far = n.add_cell(comb("far", x, y));
        let mut p = PlacementResult::new();
        for (id, tx) in [(lut, 10), (ff, 20), (far, 30)] {
            p.placements
                .insert(id, PlacementLoc::new(tx, 5, 0, BelType::Lut4));
        }
        (n, p)
    }

    #[test]
    fn registers_go_to_their_driver_and_critical_sinks_close_in() {
        let (n, p) = design();
        let x = n.nets.iter().position(|net| net.name == "x").unwrap();
        let name = |c: &Candidate| n.get_cell(c.cell).unwrap().path.clone();

        // Not critical: only the register and its driver are pulled
        // together, the register first
        let mut crit = vec![Vec::new(); n.nets.len()];
        crit[x] = vec![0.0; n.nets[x].fanout.len()];
        let found = candidates(&n, &p, &crit);
        assert_eq!(found.len(), 2);
        assert_eq!(name(&found[0]), "ff");
        assert_eq!(found[0].tiles, vec![(10, 5)]);
        assert_eq!(name(&found[1]), "lut");
        assert_eq!(found[1].tiles, vec![(20, 5)]);

        // Critical: the far comb sink too, to the driver's tile or beside it
        crit[x] = vec![0.9; n.nets[x].fanout.len()];
        let found = candidates(&n, &p, &crit);
        assert_eq!(found.len(), 3);
        let far = found.iter().find(|c| name(c) == "far").unwrap();
        assert_eq!(far.tiles[0], (10, 5));
        assert!(far.tiles.contains(&(11, 5)) && far.tiles.contains(&(9, 5)));
    }
}
