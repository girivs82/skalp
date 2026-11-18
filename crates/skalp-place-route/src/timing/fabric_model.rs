//! A [`TimingModel`] built from a family's [`DelayModel`] tables, for
//! families without a per-cell timing database. It lets the routed-design
//! timing engine (and so timed simulation) work on any device.

use super::sta::{TimingEndpoint, TimingModel};
use super::DelayModel;
use crate::device::{Device, PipId};
use skalp_lir::gate_netlist::Cell;

/// Nanoseconds to picoseconds
fn ps(ns: f64) -> i64 {
    (ns * 1000.0).round() as i64
}

/// One family's delays from its [`DelayModel`]: every combinational cell
/// takes its type's delay from each input to each output, clocked cells
/// launch after clock-to-out and sample with the register setup and hold,
/// and a routing switch takes the device's own delay where it has one, or
/// the table's delay for its wire types
pub struct FabricTiming<'a, D: Device + ?Sized> {
    pub device: &'a D,
    pub delays: &'a DelayModel,
}

impl<D: Device + ?Sized> TimingModel for FabricTiming<'_, D> {
    fn interconnect_ps(&self, pip: PipId) -> i64 {
        let Some(info) = self.device.pip(pip) else {
            return ps(self.delays.pip_delay);
        };
        if info.delay > 0 {
            return i64::from(info.delay);
        }
        match (
            self.device.wire(info.src_wire),
            self.device.wire(info.dst_wire),
        ) {
            (Some(src), Some(dst)) => {
                ps(self.delays.pip_delay_typed(&src.wire_type, &dst.wire_type))
            }
            _ => ps(self.delays.pip_delay),
        }
    }

    fn launch_ps(&self, cell: &Cell, _output: usize) -> Option<i64> {
        if cell.clock.is_some() {
            Some(ps(self.delays.register_clock_to_q()))
        } else if cell.inputs.is_empty()
            && (cell.cell_type.contains("IO") || cell.cell_type.contains("IB"))
        {
            // An input pad, whose own delay is counted here (a constant,
            // with no inputs either, launches nothing)
            Some(ps(self.delays.io_input_delay))
        } else {
            None
        }
    }

    fn arc_ps(&self, cell: &Cell, input: usize, _output: usize) -> Option<i64> {
        if cell.clock.is_some() || input >= cell.inputs.len() {
            return None;
        }
        Some(ps(self.delays.cell_delay(&cell.cell_type)))
    }

    fn endpoints(&self, cell: &Cell) -> Vec<TimingEndpoint> {
        if cell.clock.is_none() {
            return Vec::new();
        }
        let (setup_ps, hold_ps) = (
            ps(self.delays.register_setup()),
            ps(self.delays.register_hold()),
        );
        // A reset is pin `inputs.len()`, as in the bundle delays' pin order
        let pins = cell.inputs.len() + usize::from(cell.reset.is_some());
        (0..pins)
            .map(|input| TimingEndpoint {
                input,
                pin: input,
                setup_ps,
                hold_ps,
                clock: None,
            })
            .collect()
    }
}
