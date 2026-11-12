//! Slack-based net criticality for timing-driven placement.
//!
//! A static timing estimate on cell positions, before routing: each cell
//! costs its [`DelayModel::cell_delay`], and each connection
//! [`DelayModel::connection_delay`] for the tile offset between its ends
//! (measured on the device's routing graph when the placer could). Paths start at registers, pads and constants and end at register
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

/// Delay of a connection between cells at `from` and `to`
/// ([`DelayModel::connection_delay`] at their tile offset).
fn connection_delay(model: &DelayModel, from: (f64, f64), to: (f64, f64)) -> f64 {
    let dx = (from.0 - to.0).abs().round() as u32;
    let dy = (from.1 - to.1).abs().round() as u32;
    model.connection_delay(dx, dy)
}

/// Whether a cell starts and ends timing paths: registers, RAMs and other
/// clocked cells, and cells with no inputs (pads, constants).
fn is_path_boundary(cell: &Cell) -> bool {
    cell.clock.is_some() || cell.inputs.is_empty()
}

/// Arrival and required times of every net's driver output, on a placement.
struct Analysis {
    /// Latest arrival at each net's driver output (ns)
    arrival: Vec<f64>,
    /// Latest time each net's driver output may switch (ns); infinite for
    /// nets that end no timing path
    required: Vec<f64>,
    /// Latest time each cell may see its inputs (ns), by cell index
    input_required: Vec<f64>,
    /// Delay of the worst path (ns); 0 when there is none
    worst: f64,
}

/// Static timing of `netlist` with cells at `position`.
fn analyse(
    netlist: &GateNetlist,
    model: &DelayModel,
    position: &impl Fn(CellId) -> Option<(f64, f64)>,
) -> Analysis {
    let nets = &netlist.nets;
    let cell_index: HashMap<CellId, usize> =
        HashMap::from_iter(netlist.cells.iter().enumerate().map(|(i, c)| (c.id, i)));
    let driver = |net: GateNetId| -> Option<&Cell> {
        nets.get(net.0 as usize)
            .and_then(|n| n.driver)
            .and_then(|id| cell_index.get(&id).map(|&i| &netlist.cells[i]))
    };
    let wire = |net: GateNetId, sink: &Cell| -> f64 {
        match (driver(net).and_then(|d| position(d.id)), position(sink.id)) {
            (Some(from), Some(to)) => connection_delay(model, from, to),
            _ => 0.0,
        }
    };

    // Combinational cells in topological order (Kahn). Cells on a
    // combinational loop are never released and keep a zero arrival.
    let combinational: Vec<usize> = (0..netlist.cells.len())
        .filter(|&i| !is_path_boundary(&netlist.cells[i]))
        .collect();
    let index: HashMap<CellId, usize> = HashMap::from_iter(
        combinational
            .iter()
            .enumerate()
            .map(|(k, &i)| (netlist.cells[i].id, k)),
    );
    let mut pending = vec![0usize; combinational.len()];
    let mut fanout: Vec<Vec<usize>> = vec![Vec::new(); combinational.len()];
    for (k, &i) in combinational.iter().enumerate() {
        for &input in &netlist.cells[i].inputs {
            if let Some(&from) = driver(input).and_then(|d| index.get(&d.id)) {
                pending[k] += 1;
                fanout[from].push(k);
            }
        }
    }
    let mut queue: VecDeque<usize> = (0..combinational.len())
        .filter(|&k| pending[k] == 0)
        .collect();
    let mut order = Vec::with_capacity(combinational.len());
    while let Some(k) = queue.pop_front() {
        order.push(combinational[k]);
        for &next in &fanout[k] {
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
        let cell = &netlist.cells[i];
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

    // Required times, backwards from the ends.
    let mut required = vec![f64::INFINITY; nets.len()];
    let mut input_required = vec![f64::INFINITY; netlist.cells.len()];
    for (i, cell) in netlist.cells.iter().enumerate() {
        if is_path_boundary(cell) {
            input_required[i] = worst - model.dff_setup;
            for &input in &cell.inputs {
                let r = input_required[i] - wire(input, cell);
                let slot = &mut required[input.0 as usize];
                *slot = slot.min(r);
            }
        }
    }
    for &i in order.iter().rev() {
        let cell = &netlist.cells[i];
        let out_required = cell
            .outputs
            .iter()
            .map(|o| required[o.0 as usize])
            .fold(f64::INFINITY, f64::min);
        if !out_required.is_finite() {
            continue;
        }
        input_required[i] = out_required - model.cell_delay(&cell.cell_type);
        for &input in &cell.inputs {
            let r = input_required[i] - wire(input, cell);
            let slot = &mut required[input.0 as usize];
            *slot = slot.min(r);
        }
    }

    Analysis {
        arrival,
        required,
        input_required,
        worst,
    }
}

/// Criticality of each net in `netlist` (by index), with cells at `position`:
/// that of its most critical connection. Nets with no placed driver and
/// sinks get 0.
pub fn net_criticalities(
    netlist: &GateNetlist,
    model: &DelayModel,
    position: impl Fn(CellId) -> Option<(f64, f64)>,
) -> Vec<f64> {
    let a = analyse(netlist, model, &position);
    if a.worst <= 0.0 {
        return vec![0.0; netlist.nets.len()];
    }
    a.arrival
        .iter()
        .zip(&a.required)
        .map(|(&arrival, &required)| {
            if required.is_finite() {
                (1.0 - (required - arrival) / a.worst).clamp(0.0, 1.0)
            } else {
                0.0
            }
        })
        .collect()
}

/// Criticality of each connection of each net in `netlist` (by net index,
/// then in the order of the net's `fanout`), with cells at `position`.
///
/// A net's sinks need not be equally critical: one may sit on the worst path
/// while the others have time to spare. Pricing each connection by its own
/// criticality (as VPR and nextpnr do) lets the placer pull the critical sink
/// in without dragging the rest along.
pub fn connection_criticalities(
    netlist: &GateNetlist,
    model: &DelayModel,
    position: impl Fn(CellId) -> Option<(f64, f64)>,
) -> Vec<Vec<f64>> {
    let a = analyse(netlist, model, &position);
    let cell_index: HashMap<CellId, usize> =
        HashMap::from_iter(netlist.cells.iter().enumerate().map(|(i, c)| (c.id, i)));
    netlist
        .nets
        .iter()
        .enumerate()
        .map(|(n, net)| {
            let from = net.driver.and_then(&position);
            net.fanout
                .iter()
                .map(|(sink, _)| {
                    let Some(&i) = cell_index.get(sink) else {
                        return 0.0;
                    };
                    let required = a.input_required[i];
                    if a.worst <= 0.0 || !required.is_finite() {
                        return 0.0;
                    }
                    let wire = match (from, position(*sink)) {
                        (Some(from), Some(to)) => connection_delay(model, from, to),
                        _ => 0.0,
                    };
                    let slack = required - (a.arrival[n] + wire);
                    (1.0 - slack / a.worst).clamp(0.0, 1.0)
                })
                .collect()
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

    /// A net driving both the long path and a register right away: only the
    /// connection on the long path is critical.
    #[test]
    fn each_connection_has_its_own_criticality() {
        let n = chain();
        let positions: HashMap<CellId, (f64, f64)> = HashMap::from_iter(
            n.cells
                .iter()
                .enumerate()
                .map(|(i, c)| (c.id, (i as f64, 0.0))),
        );
        let model = DelayModel::nexus();
        let per_net = net_criticalities(&n, &model, |id| positions.get(&id).copied());
        let crit = connection_criticalities(&n, &model, |id| positions.get(&id).copied());
        let q = n.nets.iter().position(|x| x.name == "q").unwrap();
        let sink_name = |k: usize| {
            let id = n.nets[q].fanout[k].0;
            n.cells.iter().find(|c| c.id == id).unwrap().path.clone()
        };
        assert_eq!(crit[q].len(), 2);
        for (k, &c) in crit[q].iter().enumerate() {
            match sink_name(k).as_str() {
                "lx" => assert!(c > 0.99, "q -> lx {c}"),
                "c" => assert!(c < 0.5, "q -> c {c}"),
                other => panic!("unexpected sink {other}"),
            }
        }
        // A net is as critical as its most critical connection.
        for (net, conns) in per_net.iter().zip(&crit) {
            let most = conns.iter().copied().fold(0.0, f64::max);
            assert!(
                (net - most).abs() < 1e-9 || conns.is_empty(),
                "{net} vs {most}"
            );
        }
    }
}
