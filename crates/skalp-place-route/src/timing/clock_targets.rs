//! The frequency each clock of a design runs at, for timing analysis.
//!
//! A clock's frequency is fixed by where it comes from: a board oscillator on
//! a clock pin, an on-chip oscillator, or a PLL output derived from its
//! reference clock. Those are properties of the hardware, so they win over a
//! configured target, which applies only to clocks whose source is unknown.

use crate::collections::{HashMap, HashMapExt};
use skalp_lir::gate_netlist::{Cell, GateNetId, GateNetlist};
use skalp_lir::tech_library::CellFunction;

/// Frequencies, in MHz, to time each clock net against.
#[derive(Debug, Clone)]
pub struct ClockTargets {
    /// For clocks whose source frequency is unknown
    pub default_mhz: f64,
    /// Clocks whose frequency is known from their source
    pub per_net: HashMap<GateNetId, f64>,
    /// Taken off every clock period before paths are checked, in ns: clock
    /// jitter and uncertainty plus the user's setup margin
    pub uncertainty_ns: f64,
    /// Added to every hold requirement, in ns
    pub hold_margin_ns: f64,
}

impl ClockTargets {
    /// Every clock at one frequency.
    pub fn uniform(mhz: f64) -> Self {
        Self {
            default_mhz: mhz,
            per_net: HashMap::new(),
            uncertainty_ns: 0.0,
            hold_margin_ns: 0.0,
        }
    }

    /// Frequency of a clock net, in MHz.
    pub fn mhz(&self, net: GateNetId) -> f64 {
        self.per_net.get(&net).copied().unwrap_or(self.default_mhz)
    }

    /// Time a path on a clock net may take, in ns: the period less the
    /// uncertainty.
    pub fn required_ns(&self, net: GateNetId) -> f64 {
        1000.0 / self.mhz(net) - self.uncertainty_ns
    }

    /// Follow clock frequencies through a netlist. `source` gives the
    /// frequencies a cell drives, by output index, given the frequencies
    /// already known on its input nets: a clock pad gives its board
    /// frequency, a PLL its outputs from its reference clock. Plain buffers
    /// pass their input's frequency on.
    pub fn derive(
        netlist: &GateNetlist,
        default_mhz: f64,
        source: impl Fn(&Cell, &dyn Fn(GateNetId) -> Option<f64>) -> Vec<(usize, f64)>,
    ) -> Self {
        let mut per_net: HashMap<GateNetId, f64> = HashMap::new();
        // Each pass settles at least one more stage of a clock chain (pad,
        // PLL, buffer); real chains are a few stages deep.
        loop {
            let mut found = Vec::new();
            for cell in &netlist.cells {
                let known = |net: GateNetId| per_net.get(&net).copied();
                let driven: Vec<(usize, f64)> = match cell.function {
                    Some(CellFunction::Buf) | Some(CellFunction::ClkBuf) => cell
                        .inputs
                        .first()
                        .and_then(|&net| known(net))
                        .map(|mhz| vec![(0, mhz)])
                        .unwrap_or_default(),
                    _ => source(cell, &known),
                };
                for (output, mhz) in driven {
                    if let Some(&net) = cell.outputs.get(output) {
                        if !per_net.contains_key(&net) {
                            found.push((net, mhz));
                        }
                    }
                }
            }
            if found.is_empty() {
                break;
            }
            per_net.extend(found);
        }
        Self {
            default_mhz,
            per_net,
            uncertainty_ns: 0.0,
            hold_margin_ns: 0.0,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use skalp_lir::gate_netlist::{CellId, GateNet};

    fn cell(id: u32, function: Option<CellFunction>, inputs: &[u32], outputs: &[u32]) -> Cell {
        let mut cell = Cell::new_comb(
            CellId(id),
            format!("C{id}"),
            "lib".into(),
            0.0,
            format!("top.c{id}"),
            inputs.iter().map(|&n| GateNetId(n)).collect(),
            outputs.iter().map(|&n| GateNetId(n)).collect(),
        );
        cell.function = function;
        cell
    }

    fn netlist(cells: Vec<Cell>, nets: u32) -> GateNetlist {
        let mut netlist = GateNetlist::new("top".into(), "lib".into());
        for n in 0..nets {
            netlist
                .nets
                .push(GateNet::new(GateNetId(n), format!("n{n}")));
        }
        netlist.cells = cells;
        netlist
    }

    #[test]
    fn frequencies_follow_pads_buffers_and_generators() {
        // pad -> n0 -> doubler -> n1 -> buffer -> n2; n3 has no known source
        let design = netlist(
            vec![
                cell(0, Some(CellFunction::InputPad), &[], &[0]),
                cell(1, None, &[0], &[1]),
                cell(2, Some(CellFunction::ClkBuf), &[1], &[2]),
                cell(3, Some(CellFunction::InputPad), &[], &[3]),
            ],
            4,
        );
        let targets = ClockTargets::derive(&design, 50.0, |cell, known| match cell.id.0 {
            0 => vec![(0, 25.0)],
            1 => known(cell.inputs[0]).map_or(vec![], |mhz| vec![(0, mhz * 2.0)]),
            _ => vec![],
        });
        assert_eq!(targets.mhz(GateNetId(0)), 25.0);
        assert_eq!(targets.mhz(GateNetId(1)), 50.0);
        assert_eq!(targets.mhz(GateNetId(2)), 50.0);
        assert_eq!(targets.mhz(GateNetId(3)), 50.0, "unknown source: default");
        assert!(!targets.per_net.contains_key(&GateNetId(3)));
    }
}
