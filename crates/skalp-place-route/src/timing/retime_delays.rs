//! A device's delays for retiming before placement
//! ([`skalp_lir::retime`]): cell arcs from the family's [`TimingModel`],
//! and one routing estimate per connection, since nothing is placed yet.

use super::sta::TimingModel;
use skalp_lir::gate_netlist::Cell;
use skalp_lir::retime::RetimeDelays;

/// Extra routing delay per load on a net, in picoseconds
const PER_LOAD_PS: i64 = 15;

/// [`RetimeDelays`] from a device's timing model
pub struct DeviceRetimeDelays<'a> {
    model: &'a dyn TimingModel,
    clk_to_q_ps: i64,
    setup_ps: i64,
    connection_ps: i64,
}

impl<'a> DeviceRetimeDelays<'a> {
    /// `connection_ps` is the routing delay charged for each connection, as
    /// for two cells a couple of tiles apart
    pub fn new(
        model: &'a dyn TimingModel,
        clk_to_q_ps: i64,
        setup_ps: i64,
        connection_ps: i64,
    ) -> Self {
        Self {
            model,
            clk_to_q_ps,
            setup_ps,
            connection_ps,
        }
    }
}

impl RetimeDelays for DeviceRetimeDelays<'_> {
    fn arc_ps(&self, cell: &Cell, input: usize, output: usize) -> Option<i64> {
        self.model.arc_ps(cell, input, output)
    }
    fn clk_to_q_ps(&self) -> i64 {
        self.clk_to_q_ps
    }
    fn setup_ps(&self) -> i64 {
        self.setup_ps
    }
    fn net_ps(&self, fanout: usize) -> i64 {
        self.connection_ps + PER_LOAD_PS * fanout.saturating_sub(1) as i64
    }
}
