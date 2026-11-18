//! Switching energy of a routed design from the toggles of a timed
//! simulation ([`crate::timed_sim`]).
//!
//! Each toggle of a net charges or discharges the capacitance it drives:
//! the routing wires place and route chose for it, the input pins it
//! reaches, and the driving cell's own internal nodes. A toggle costs
//! `C V² / 2`. Place and route gives each net's routing as the sum of the
//! delays of the switches it uses ([`NetLoads`]): a routing segment's delay
//! grows with the capacitance it drives, so that sum stands for the wire
//! capacitance of the net's whole tree (each switch once, however many sinks
//! share it) on any family.
//!
//! The capacitances in [`EnergyModel`] are estimates, not a vendor's figures,
//! so the result is an estimate. Two designs on the same family, simulated
//! the same way, compare fairly; the absolute value is a rough guide.

use crate::gate_netlist::{Cell, GateNetlist};
use serde::{Deserialize, Serialize};

/// The routing load of each net of a routed netlist, from place and route
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct NetLoads {
    /// The corner whose switch delays `routing_ps` sums
    pub corner: String,
    /// By net index: the sum of the delays of the routing switches the net
    /// uses, each once (0 for a net with no routing)
    pub routing_ps: Vec<i64>,
    /// By net index: how many routing switches the net uses
    pub switches: Vec<u32>,
}

/// Capacitances that turn toggles into energy, in femtofarads
#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct EnergyModel {
    /// Supply voltage, in volts
    pub vdd: f64,
    /// Wire capacitance per picosecond of routing switch delay
    pub routing_ff_per_ps: f64,
    /// An input pin a net reaches (data or clock)
    pub pin_ff: f64,
    /// Inside a clocked cell, at each toggle of its clock pin
    pub clocked_cell_ff: f64,
    /// Inside a logic cell (LUT, carry), per toggle of an output
    pub logic_cell_ff: f64,
    /// Inside a hard block (multiplier, block RAM), per toggle of an output
    pub hard_block_ff: f64,
}

impl Default for EnergyModel {
    /// Round figures for a 28 nm FPGA fabric at 1.0 V: a routing hop of
    /// ~100-150 ps drives ~10-15 fF, an input pin ~1.5 fF, a LUT's internal
    /// tree ~5 fF, a flip-flop's internal clocking ~3 fF, a hard block's
    /// datapath ~20 fF per output bit that changes
    fn default() -> Self {
        Self {
            vdd: 1.0,
            routing_ff_per_ps: 0.1,
            pin_ff: 1.5,
            clocked_cell_ff: 3.0,
            logic_cell_ff: 5.0,
            hard_block_ff: 20.0,
        }
    }
}

/// Where switching energy went, in femtojoules
#[derive(Debug, Clone, Copy, Default, PartialEq, Serialize, Deserialize)]
pub struct EnergyParts {
    /// Routing wires of nets other than clocks
    pub routing_fj: f64,
    /// Input pins of nets other than clocks
    pub pins_fj: f64,
    /// Inside logic cells and hard blocks
    pub cells_fj: f64,
    /// Clock nets: their routing, pins and the clocked cells' internal
    /// clocking
    pub clock_fj: f64,
}

impl EnergyParts {
    pub fn total_fj(&self) -> f64 {
        self.routing_fj + self.pins_fj + self.cells_fj + self.clock_fj
    }

    fn add(&mut self, other: &EnergyParts) {
        self.routing_fj += other.routing_fj;
        self.pins_fj += other.pins_fj;
        self.cells_fj += other.cells_fj;
        self.clock_fj += other.clock_fj;
    }
}

/// Switching energy of a design over a stretch of simulation
#[derive(Debug, Clone, Default, PartialEq, Serialize, Deserialize)]
pub struct EnergyReport {
    /// All of it
    pub total: EnergyParts,
    /// By the hierarchy of the driving cell (its path cut to `depth`
    /// names), largest first
    pub by_block: Vec<(String, EnergyParts)>,
    /// Each net that toggled (by net index), with its energy
    pub by_net: Vec<(u32, EnergyParts)>,
    /// Toggles of every net, and of clock nets
    pub toggles: u64,
    pub clock_toggles: u64,
}

/// What kind of cell drives a net, for its internal energy
fn driver_ff(cell: &Cell, model: &EnergyModel) -> f64 {
    use crate::tech_library::CellFunction;
    if cell.dsp_ports().is_some()
        || matches!(
            cell.function,
            Some(CellFunction::Ram | CellFunction::DistRam16x4)
        )
    {
        model.hard_block_ff
    } else if cell.clock.is_some() {
        // A register's output toggle: its clocking is counted at its
        // clock pin
        0.0
    } else {
        model.logic_cell_ff
    }
}

/// The hierarchy of a cell path, cut to `depth` names (`top.a.b.c` at
/// depth 3 is `top.a.b`)
fn block_of(path: &str, depth: usize) -> &str {
    match path.match_indices('.').nth(depth.saturating_sub(1)) {
        Some((at, _)) => &path[..at],
        None => path,
    }
}

/// The switching energy of `toggles` (by net index, as
/// [`crate::timed_sim::TimedSim::toggles`] counts them) on `netlist` with
/// its routing `loads`
pub fn switching_energy(
    netlist: &GateNetlist,
    loads: &NetLoads,
    toggles: &[u64],
    model: &EnergyModel,
    depth: usize,
) -> EnergyReport {
    let half_v2 = 0.5 * model.vdd * model.vdd;
    // Nets that clock a cell, and how many cells each clocks
    let mut clocked = vec![0u32; netlist.nets.len()];
    for cell in &netlist.cells {
        if let Some(clock) = cell.clock {
            clocked[netlist.resolve_alias(clock).0 as usize] += 1;
        }
    }
    let mut report = EnergyReport::default();
    let mut blocks: std::collections::BTreeMap<&str, EnergyParts> = Default::default();
    for (index, net) in netlist.nets.iter().enumerate() {
        let count = toggles.get(index).copied().unwrap_or(0);
        if count == 0 {
            continue;
        }
        report.toggles += count;
        let n = count as f64;
        let routing_ff = loads.routing_ps.get(index).copied().unwrap_or(0).max(0) as f64
            * model.routing_ff_per_ps;
        // Clock pins are not in a net's fanout list
        let pins_ff = (net.fanout.len() + clocked[index] as usize) as f64 * model.pin_ff;
        let driver = net.driver.map(|d| &netlist.cells[d.0 as usize]);
        let mut parts = EnergyParts::default();
        if clocked[index] > 0 || net.is_clock {
            report.clock_toggles += count;
            parts.clock_fj = n
                * half_v2
                * (routing_ff + pins_ff + f64::from(clocked[index]) * model.clocked_cell_ff);
        } else {
            parts.routing_fj = n * half_v2 * routing_ff;
            parts.pins_fj = n * half_v2 * pins_ff;
            parts.cells_fj = n * half_v2 * driver.map_or(0.0, |c| driver_ff(c, model));
        }
        report.total.add(&parts);
        report.by_net.push((index as u32, parts));
        let block = driver.map_or("(inputs)", |c| block_of(&c.path, depth));
        blocks.entry(block).or_default().add(&parts);
    }
    let mut by_block: Vec<(String, EnergyParts)> = blocks
        .into_iter()
        .map(|(b, p)| (b.to_string(), p))
        .collect();
    by_block.sort_by(|a, b| b.1.total_fj().total_cmp(&a.1.total_fj()));
    report.by_block = by_block;
    report
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::gate_netlist::{CellId, GateNetId};
    use crate::tech_library::CellFunction;

    /// a -> LUT -> d -> register (clocked by clk) -> q
    fn design() -> (GateNetlist, GateNetId, GateNetId, GateNetId, GateNetId) {
        let mut nl = GateNetlist::new("t".into(), "test".into());
        let clk = nl.add_clock("clk".into());
        let a = nl.add_input("a".into());
        let d = nl.add_net(crate::gate_netlist::GateNet::new(GateNetId(0), "d".into()));
        let q = nl.add_output("q".into());
        let mut lut = Cell::new_comb(
            CellId(0),
            "LUT4".into(),
            "test".into(),
            0.0,
            "top.logic.lut".into(),
            vec![a],
            vec![d],
        );
        lut.lut_init = Some(0x2);
        nl.add_cell(lut);
        let mut ff = Cell::new_seq(
            CellId(0),
            "DFF".into(),
            "test".into(),
            0.0,
            "top.regs.ff".into(),
            vec![d],
            vec![q],
            clk,
            None,
        );
        ff.function = Some(CellFunction::Dff);
        nl.add_cell(ff);
        (nl, clk, a, d, q)
    }

    #[test]
    fn toggles_cost_their_routing_pins_and_cells() {
        let (nl, clk, _a, d, _q) = design();
        let mut loads = NetLoads {
            corner: "slow".into(),
            routing_ps: vec![0; nl.nets.len()],
            switches: vec![0; nl.nets.len()],
        };
        loads.routing_ps[d.0 as usize] = 200;
        loads.routing_ps[clk.0 as usize] = 1000;
        let mut toggles = vec![0u64; nl.nets.len()];
        toggles[d.0 as usize] = 4;
        toggles[clk.0 as usize] = 10;
        let model = EnergyModel {
            vdd: 2.0,
            ..EnergyModel::default()
        };
        let report = switching_energy(&nl, &loads, &toggles, &model, 2);
        // C V^2 / 2 with V = 2: twice the capacitance, in fJ
        let routing = 4.0 * 2.0 * (200.0 * model.routing_ff_per_ps);
        let pins = 4.0 * 2.0 * model.pin_ff;
        let cells = 4.0 * 2.0 * model.logic_cell_ff;
        let clock =
            10.0 * 2.0 * (1000.0 * model.routing_ff_per_ps + model.pin_ff + model.clocked_cell_ff);
        let close = |a: f64, b: f64| (a - b).abs() < 1e-9;
        assert!(close(report.total.routing_fj, routing));
        assert!(close(report.total.pins_fj, pins));
        assert!(close(report.total.cells_fj, cells));
        assert!(close(report.total.clock_fj, clock));
        assert_eq!((report.toggles, report.clock_toggles), (14, 10));
        // The clock comes from outside; the LUT's net is its block's
        let blocks: Vec<&str> = report.by_block.iter().map(|(b, _)| b.as_str()).collect();
        assert_eq!(blocks, ["(inputs)", "top.logic"]);
    }

    #[test]
    fn nothing_switching_costs_nothing() {
        let (nl, ..) = design();
        let loads = NetLoads::default();
        let report = switching_energy(
            &nl,
            &loads,
            &vec![0; nl.nets.len()],
            &EnergyModel::default(),
            3,
        );
        assert_eq!(report.total.total_fj(), 0.0);
        assert!(report.by_block.is_empty());
    }

    #[test]
    fn blocks_cut_paths_to_a_depth() {
        assert_eq!(block_of("top.k.pool.fma", 3), "top.k.pool");
        assert_eq!(block_of("top.k", 3), "top.k");
        assert_eq!(block_of("top", 1), "top");
    }
}
