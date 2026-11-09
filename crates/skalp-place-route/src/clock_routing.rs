//! Route clock nets on a device's dedicated clock network.
//!
//! Each clock pin is reached by a breadth-first search upstream through the
//! dedicated clock wires, never through the general fabric, so clocks arrive
//! with low skew and leave the general routing to data. Clock wires are claimed
//! here, before general routing, so data nets cannot be routed through them.
//! Which wires count as general comes from
//! [`Device::is_general_routing_wire`].

use crate::device::{is_clock_pin, CellPin, Device, PipId, WireId};
use crate::error::{PlaceRouteError, Result};
use crate::placer::PlacementResult;
use crate::router::Route;
use skalp_lir::gate_netlist::{GateNetId, GateNetlist};
use std::collections::{HashMap, HashSet, VecDeque};

/// Clock nets routed on dedicated wires.
#[derive(Debug, Default)]
pub struct ClockRouting {
    /// One route per clock net
    pub routes: Vec<Route>,
    /// Every wire the routes use
    pub claimed: HashSet<WireId>,
}

fn clock_pin_wire<D: Device + ?Sized>(
    device: &D,
    placement: &PlacementResult,
    cell: skalp_lir::gate_netlist::CellId,
    pin: CellPin,
) -> Option<WireId> {
    let loc = placement.placements.get(&cell)?;
    device.bel_pin_wire(loc.tile_x, loc.tile_y, loc.bel_index, pin)
}

/// Search upstream from `sink` to any wire already on this net.
fn find_path<D: Device + ?Sized>(
    device: &D,
    sink: WireId,
    tree: &HashSet<WireId>,
    other_nets: &HashMap<WireId, GateNetId>,
) -> Option<Vec<PipId>> {
    let mut visited: HashMap<WireId, PipId> = HashMap::new();
    let mut queue = VecDeque::from([sink]);
    let mut seen: HashSet<WireId> = HashSet::from([sink]);

    while let Some(wire) = queue.pop_front() {
        for pip_id in device.wire_pips(wire) {
            let Some(pip) = device.pip(pip_id) else {
                continue;
            };
            if device.is_general_routing_wire(pip.dst_wire)
                || other_nets.contains_key(&pip.src_wire)
            {
                continue;
            }
            if !seen.insert(pip.src_wire) {
                continue;
            }
            visited.insert(pip.src_wire, pip_id);
            if tree.contains(&pip.src_wire) {
                let mut path = Vec::new();
                let mut cursor = pip.src_wire;
                while let Some(&step) = visited.get(&cursor) {
                    path.push(step);
                    cursor = device.pip(step)?.dst_wire;
                }
                return Some(path);
            }
            queue.push_back(pip.src_wire);
        }
    }
    None
}

/// Route every clock pin of a design prepared for routing.
pub fn route_clock_nets<D: Device + ?Sized>(
    device: &D,
    netlist: &GateNetlist,
    placement: &PlacementResult,
) -> Result<ClockRouting> {
    let mut routing = ClockRouting::default();
    let mut owners: HashMap<WireId, GateNetId> = HashMap::new();

    for net in &netlist.nets {
        let mut clock_pins: Vec<_> = net
            .fanout
            .iter()
            .filter(|(_, pin)| is_clock_pin(*pin))
            .copied()
            .collect();
        if clock_pins.is_empty() {
            continue;
        }
        clock_pins.sort_unstable_by_key(|(cell, pin)| (cell.0, *pin));

        // A clock arriving on a bare input with no pad has nowhere to start from.
        let Some(driver) = net.driver else {
            eprintln!(
                "warning: clock net '{}' has no pad or driver, so it is left unrouted",
                net.name
            );
            continue;
        };
        let source = clock_pin_wire(
            device,
            placement,
            driver,
            CellPin::Output(net.driver_pin.unwrap_or(0)),
        )
        .ok_or_else(|| {
            PlaceRouteError::RoutingFailed(format!("Clock net '{}' has no source wire", net.name))
        })?;

        let mut route = Route::new(net.id);
        route.source = source;
        let mut tree = HashSet::from([source]);
        for (cell, pin) in clock_pins {
            let sink =
                clock_pin_wire(device, placement, cell, CellPin::Input(pin)).ok_or_else(|| {
                    PlaceRouteError::RoutingFailed(format!(
                        "A clock pin on net '{}' has no wire",
                        net.name
                    ))
                })?;
            route.sinks.push(sink);
            if tree.contains(&sink) {
                continue;
            }
            let path = find_path(device, sink, &tree, &owners).ok_or_else(|| {
                PlaceRouteError::RoutingFailed(format!(
                    "Clock net '{}' has no dedicated path to {}",
                    net.name,
                    device.wire(sink).map_or("a clock pin", |w| w.name.as_str())
                ))
            })?;
            for pip_id in path {
                let Some(pip) = device.pip(pip_id) else {
                    continue;
                };
                route.delay += pip.delay;
                route.pips.push(pip_id);
                route.wires.push(pip.dst_wire);
                tree.insert(pip.dst_wire);
            }
        }
        route.wires.push(source);
        for &wire in &tree {
            owners.insert(wire, net.id);
            routing.claimed.insert(wire);
        }
        routing.routes.push(route);
    }
    Ok(routing)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::device::nexus::prjoxide_graph::node_name;
    use crate::device::nexus::{prjoxide_load::find_database, resolve_wire};
    use crate::device::nexus::{NexusDevice, NexusVariant};

    #[test]
    fn oscillator_reaches_a_clock_pin_on_dedicated_wires() {
        let Some(db) = find_database() else {
            eprintln!("PRJOXIDE_DB not found — skipping");
            return;
        };
        let device =
            NexusDevice::from_prjoxide_bbox(NexusVariant::Lfcpnx100, &db, (73, 0, 159, 40))
                .expect("build device");
        let wire = |x, y, name: &str| {
            device
                .wire_by_name(&node_name(&resolve_wire(x, y, name).unwrap()))
                .unwrap()
        };
        let osc = wire(137, 0, "S1:JHFCLKOUT_OSC_CORE");
        let sink = wire(100, 20, "JCLK_SLICEA");

        let path = find_path(&device, sink, &HashSet::from([osc]), &HashMap::new())
            .expect("a dedicated path");
        assert!(!path.is_empty());
        for pip in path {
            let dst = device.pip(pip).unwrap().dst_wire;
            assert!(!device.is_general_routing_wire(dst));
        }
    }
}
