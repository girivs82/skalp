//! Route clock nets on the Nexus dedicated clock network.
//!
//! Each clock pin is reached by a breadth-first search upstream through the
//! global wires (spines, branches and clock muxes), never through the general
//! `H0x`/`V0x` fabric wires, so clocks arrive with low skew and leave the
//! general routing to data. Clock wires are claimed here, before general
//! routing, so data nets cannot be routed through them.

use super::NexusDevice;
use crate::device::{CellPin, Device, PipId, WireId, FF_PIN_CLK};
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

/// Whether the destination wire of a pip is general fabric routing.
///
/// Wire names carry a `R<row>C<col>_` location prefix, then `H0x` or `V0x` for
/// the general spans.
fn is_general_wire(name: &str) -> bool {
    let base = name.rsplit('_').next().unwrap_or(name);
    let base = name
        .strip_prefix('R')
        .and_then(|rest| rest.split_once('_'))
        .map_or(base, |(_, rest)| rest);
    let mut chars = base.chars();
    matches!(chars.next(), Some('H') | Some('V')) && chars.next() == Some('0')
}

fn clock_pin_wire(
    device: &NexusDevice,
    placement: &PlacementResult,
    cell: skalp_lir::gate_netlist::CellId,
    pin: CellPin,
) -> Option<WireId> {
    let loc = placement.placements.get(&cell)?;
    device.bel_pin_wire(loc.tile_x, loc.tile_y, loc.bel_index, pin)
}

/// Search upstream from `sink` to any wire already on this net.
fn find_path(
    device: &NexusDevice,
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
            let is_general = device
                .wire(pip.dst_wire)
                .is_some_and(|w| is_general_wire(&w.name));
            if is_general || other_nets.contains_key(&pip.src_wire) {
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
pub fn route_clock_nets(
    device: &NexusDevice,
    netlist: &GateNetlist,
    placement: &PlacementResult,
) -> Result<ClockRouting> {
    let mut routing = ClockRouting::default();
    let mut owners: HashMap<WireId, GateNetId> = HashMap::new();

    for net in &netlist.nets {
        let mut clock_pins: Vec<_> = net
            .fanout
            .iter()
            .filter(|(_, pin)| *pin == FF_PIN_CLK)
            .map(|(cell, _)| *cell)
            .collect();
        if clock_pins.is_empty() {
            continue;
        }
        clock_pins.sort_unstable_by_key(|cell| cell.0);

        let driver = net.driver.ok_or_else(|| {
            PlaceRouteError::RoutingFailed(format!("Clock net '{}' has no driver", net.name))
        })?;
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
        for cell in clock_pins {
            let sink = clock_pin_wire(device, placement, cell, CellPin::Input(FF_PIN_CLK))
                .ok_or_else(|| {
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

    #[test]
    fn oscillator_reaches_a_clock_pin_on_dedicated_wires() {
        use super::super::prjoxide_graph::node_name;
        use super::super::{prjoxide_load::find_database, resolve_wire, NexusVariant};

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
            assert!(!is_general_wire(&device.wire(dst).unwrap().name));
        }
    }

    #[test]
    fn general_wires_are_recognized() {
        assert!(is_general_wire("R2C97_H01W0300"));
        assert!(is_general_wire("R2C97_V02S0601"));
        assert!(is_general_wire("R2C97_H00L0100"));
    }

    #[test]
    fn clock_network_wires_are_not_general() {
        assert!(!is_general_wire("R2C97_BRANCH:HPBX0000"));
        assert!(!is_general_wire("R19C109_SPINE:VPSX0000"));
        assert!(!is_general_wire("R36C76_JCLK0"));
        assert!(!is_general_wire("G:JOSCHF_TMID_CORE_TMIDMUX"));
    }
}
