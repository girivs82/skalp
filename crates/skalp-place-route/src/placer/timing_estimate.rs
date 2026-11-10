//! Slack-based net criticality for timing-driven placement.
//!
//! A static timing estimate on cell positions, before routing: each cell
//! costs its [`DelayModel::cell_delay`], and each connection the delay of a
//! route as long as the Manhattan distance between its ends
//! ([`DelayModel::estimated_path_delay`] plus a hop into and out of the
//! fabric). Paths start at registers, pads and constants and end at register
//! and pad inputs, as in the analyzer that runs after routing.
//!
//! A net's criticality is `1 - slack / worst_delay`: 1 on the worst path, 0 on
//! a net with a whole clock period to spare. Only how critical nets are
//! relative to each other matters to the placer, so a rough delay model is
//! enough.

use crate::collections::HashMap;
use crate::timing::DelayModel;
use skalp_lir::gate_netlist::{Cell, CellId, GateNetId, GateNetlist};
use std::collections::VecDeque;

/// Fixed cost of any routed connection, in units of
/// [`DelayModel::pip_belpin_to_local`] + [`DelayModel::pip_local_to_belpin`]
/// (a hop out of the driver's tile and into the sink's).
fn connection_delay(model: &DelayModel, from: (f64, f64), to: (f64, f64)) -> f64 {
    let distance = (from.0 - to.0).abs() + (from.1 - to.1).abs();
    model.pip_belpin_to_local
        + model.pip_local_to_belpin
        + model.estimated_path_delay(distance.round() as u32)
}

/// Whether a cell starts and ends timing paths: registers, RAMs and other
/// clocked cells, and cells with no inputs (pads, constants).
fn is_path_boundary(cell: &Cell) -> bool {
    cell.clock.is_some() || cell.inputs.is_empty()
}

/// Criticality of each net in `netlist` (by index), with cells at `position`.
/// Nets with no placed driver and sinks get 0.
pub fn net_criticalities(
    netlist: &GateNetlist,
    model: &DelayModel,
    position: impl Fn(CellId) -> Option<(f64, f64)>,
) -> Vec<f64> {
    let nets = &netlist.nets;
    let cells_by_id: HashMap<CellId, &Cell> =
        HashMap::from_iter(netlist.cells.iter().map(|c| (c.id, c)));
    let driver = |net: GateNetId| -> Option<&Cell> {
        nets.get(net.0 as usize)
            .and_then(|n| n.driver)
            .and_then(|id| cells_by_id.get(&id).copied())
    };
    let wire = |net: GateNetId, sink: &Cell| -> f64 {
        match (driver(net).and_then(|d| position(d.id)), position(sink.id)) {
            (Some(from), Some(to)) => connection_delay(model, from, to),
            _ => 0.0,
        }
    };

    // Combinational cells in topological order (Kahn). Cells on a
    // combinational loop are never released and keep a zero arrival.
    let combinational: Vec<&Cell> = netlist
        .cells
        .iter()
        .filter(|c| !is_path_boundary(c))
        .collect();
    let index: HashMap<CellId, usize> =
        HashMap::from_iter(combinational.iter().enumerate().map(|(i, c)| (c.id, i)));
    let mut pending = vec![0usize; combinational.len()];
    let mut fanout: Vec<Vec<usize>> = vec![Vec::new(); combinational.len()];
    for (i, cell) in combinational.iter().enumerate() {
        for &input in &cell.inputs {
            if let Some(&from) = driver(input).and_then(|d| index.get(&d.id)) {
                pending[i] += 1;
                fanout[from].push(i);
            }
        }
    }
    let mut queue: VecDeque<usize> = (0..combinational.len())
        .filter(|&i| pending[i] == 0)
        .collect();
    let mut order = Vec::with_capacity(combinational.len());
    while let Some(i) = queue.pop_front() {
        order.push(i);
        for &next in &fanout[i] {
            pending[next] -= 1;
            if pending[next] == 0 {
                queue.push_back(next);
            }
        }
    }

    // Arrival at each net's driver output.
    let mut arrival = vec![0.0f64; nets.len()];
    for cell in netlist.cells.iter().filter(|c| is_path_boundary(c)) {
        let launch = if cell.clock.is_some() {
            model.dff_clk_to_q
        } else {
            0.0
        };
        for &out in &cell.outputs {
            arrival[out.0 as usize] = launch;
        }
    }
    for &i in &order {
        let cell = combinational[i];
        let latest = cell
            .inputs
            .iter()
            .map(|&input| arrival[input.0 as usize] + wire(input, cell))
            .fold(0.0, f64::max);
        let out_time = latest + model.cell_delay(&cell.cell_type);
        for &out in &cell.outputs {
            arrival[out.0 as usize] = out_time;
        }
    }

    // Path ends: inputs of registers and pads.
    let mut worst = 0.0f64;
    for cell in netlist.cells.iter().filter(|c| is_path_boundary(c)) {
        for &input in &cell.inputs {
            worst = worst.max(arrival[input.0 as usize] + wire(input, cell) + model.dff_setup);
        }
    }
    if worst <= 0.0 {
        return vec![0.0; nets.len()];
    }

    // Required time at each net's driver output, backwards from the ends.
    let mut required = vec![f64::INFINITY; nets.len()];
    for cell in netlist.cells.iter().filter(|c| is_path_boundary(c)) {
        for &input in &cell.inputs {
            let r = worst - model.dff_setup - wire(input, cell);
            let slot = &mut required[input.0 as usize];
            *slot = slot.min(r);
        }
    }
    for &i in order.iter().rev() {
        let cell = combinational[i];
        let out_required = cell
            .outputs
            .iter()
            .map(|o| required[o.0 as usize])
            .fold(f64::INFINITY, f64::min);
        if !out_required.is_finite() {
            continue;
        }
        let at_inputs = out_required - model.cell_delay(&cell.cell_type);
        for &input in &cell.inputs {
            let r = at_inputs - wire(input, cell);
            let slot = &mut required[input.0 as usize];
            *slot = slot.min(r);
        }
    }

    arrival
        .iter()
        .zip(&required)
        .map(|(&a, &r)| {
            if r.is_finite() {
                (1.0 - (r - a) / worst).clamp(0.0, 1.0)
            } else {
                0.0
            }
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use skalp_lir::gate_netlist::{GateNet, GateNetlist};

    /// reg a -> lut x -> lut y -> reg b, and reg a -> reg c directly.
    fn chain() -> GateNetlist {
        let mut n = GateNetlist::new("t".to_string(), "test".to_string());
        let clk = n.add_net(GateNet::new(GateNetId(0), "clk".to_string()));
        let q = n.add_net(GateNet::new(GateNetId(0), "q".to_string()));
        let x = n.add_net(GateNet::new(GateNetId(0), "x".to_string()));
        let y = n.add_net(GateNet::new(GateNetId(0), "y".to_string()));
        let qb = n.add_net(GateNet::new(GateNetId(0), "qb".to_string()));
        let qc = n.add_net(GateNet::new(GateNetId(0), "qc".to_string()));
        let mut ff = |name: &str, d: GateNetId, q: GateNetId| {
            let mut c = Cell::new_comb(
                CellId(0),
                "OXIDE_FF".to_string(),
                "nexus".to_string(),
                0.0,
                name.to_string(),
                vec![d],
                vec![q],
            );
            c.clock = Some(clk);
            n.add_cell(c);
        };
        ff("a", y, q);
        ff("b", y, qb);
        ff("c", q, qc);
        for (name, input, output) in [("lx", q, x), ("ly", x, y)] {
            n.add_cell(Cell::new_comb(
                CellId(0),
                "OXIDE_COMB".to_string(),
                "nexus".to_string(),
                0.0,
                name.to_string(),
                vec![input],
                vec![output],
            ));
        }
        n
    }

    #[test]
    fn the_long_path_is_critical_and_the_short_one_is_not() {
        let n = chain();
        let positions: HashMap<CellId, (f64, f64)> = HashMap::from_iter(
            n.cells
                .iter()
                .enumerate()
                .map(|(i, c)| (c.id, (i as f64, 0.0))),
        );
        let crit = net_criticalities(&n, &DelayModel::nexus(), |id| positions.get(&id).copied());
        let by_name = |name: &str| crit[n.nets.iter().position(|x| x.name == name).unwrap()];
        assert!(by_name("x") > 0.99, "x {}", by_name("x"));
        assert!(by_name("y") > 0.99, "y {}", by_name("y"));
        assert!(by_name("q") > 0.99, "q starts the long path");
        assert!(by_name("qc") < 0.01, "qc ends nowhere");
    }
}
