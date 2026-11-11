//! Route clock nets on a device's dedicated clock network.
//!
//! Each clock pin is reached by a breadth-first search upstream through the
//! dedicated clock wires, never through the general fabric, so clocks arrive
//! with low skew and leave the general routing to data. Clock wires are claimed
//! here, before general routing, so data nets cannot be routed through them.
//! Which wires count as general comes from
//! [`Device::is_general_routing_wire`].
//!
//! A source with no dedicated path of its own (an input pad that is not wired
//! to the clock network, say) enters the network through the nearest point
//! the fabric can drive: a short path on general wires takes the clock there,
//! and the dedicated wires take it to every sink, so the sinks still see a
//! low-skew clock.

use crate::collections::{HashMap, HashMapExt, HashSet};
use crate::device::{is_clock_pin, CellPin, Device, PipId, WireId};
use crate::error::{PlaceRouteError, Result};
use crate::placer::PlacementResult;
use crate::router::Route;
use skalp_lir::gate_netlist::{GateNetId, GateNetlist};
use std::collections::VecDeque;

/// Clock nets routed on dedicated wires.
#[derive(Debug, Default)]
pub struct ClockRouting {
    /// One route per clock net
    pub routes: Vec<Route>,
    /// Every wire the routes use
    pub claimed: HashSet<WireId>,
    /// Clock nets left to general routing: their source has no dedicated path
    pub general: HashSet<GateNetId>,
    /// Inputs other than clock pins that a clock net reaches on its dedicated
    /// tree (a PLL's reference clock, say), as (cell, router pin)
    pub dedicated_inputs: HashSet<(skalp_lir::gate_netlist::CellId, usize)>,
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

/// Every wire `source` reaches on dedicated wires (never entering the general
/// fabric or a wire another clock net holds), each with the PIP that first
/// reached it in a breadth-first search: following those PIPs back from any
/// reached wire gives a shortest path, and together the paths form a tree.
///
/// One search serves every sink of the net. Searching upstream from each sink
/// to the tree built so far explored a large part of the clock network per
/// sink, which on a design with 12,800 flip-flops took over a minute.
fn reach_from<D: Device + ?Sized>(
    device: &D,
    source: WireId,
    other_nets: &HashMap<WireId, GateNetId>,
) -> HashMap<WireId, PipId> {
    let mut reached: HashMap<WireId, PipId> = HashMap::new();
    let mut seen: HashSet<WireId> = HashSet::from_iter([source]);
    let mut queue = VecDeque::from([source]);
    while let Some(wire) = queue.pop_front() {
        for &pip_id in device.wire_src_pips(wire) {
            let Some(pip) = device.pip(pip_id) else {
                continue;
            };
            if device.is_general_routing_wire(pip.dst_wire)
                || other_nets.contains_key(&pip.dst_wire)
                || !seen.insert(pip.dst_wire)
            {
                continue;
            }
            reached.insert(pip.dst_wire, pip_id);
            queue.push_back(pip.dst_wire);
        }
    }
    reached
}

/// The PIPs from the nearest wire of `tree` down to `sink`, in that order,
/// along the search tree `reached`; `None` if the search never reached `sink`.
fn path_to<D: Device + ?Sized>(
    device: &D,
    sink: WireId,
    tree: &HashSet<WireId>,
    reached: &HashMap<WireId, PipId>,
) -> Option<Vec<PipId>> {
    let mut path = Vec::new();
    let mut cursor = sink;
    while !tree.contains(&cursor) {
        let &pip = reached.get(&cursor)?;
        path.push(pip);
        cursor = device.pip(pip)?.src_wire;
    }
    path.reverse();
    Some(path)
}

/// Every dedicated wire some path of dedicated wires leads from to `sink`.
fn reaching<D: Device + ?Sized>(
    device: &D,
    sink: WireId,
    other_nets: &HashMap<WireId, GateNetId>,
) -> HashSet<WireId> {
    let mut seen: HashSet<WireId> = HashSet::from_iter([sink]);
    let mut queue = VecDeque::from([sink]);
    while let Some(wire) = queue.pop_front() {
        for pip_id in device.wire_pips(wire) {
            let Some(pip) = device.pip(pip_id) else {
                continue;
            };
            let src = pip.src_wire;
            if device.is_general_routing_wire(src)
                || other_nets.contains_key(&src)
                || !seen.insert(src)
            {
                continue;
            }
            queue.push_back(src);
        }
    }
    seen
}

/// Most entry points tried before a clock net is left to general routing.
const MAX_ENTRY_TRIES: usize = 32;
/// Most wires searched for an entry point.
const MAX_ENTRY_SEARCH: usize = 2_000_000;

/// A way into the dedicated network for a source that has none of its own:
/// the PIPs from `source` to an entry wire, fewest hops first, and the
/// dedicated search tree from that entry, which reaches every one of `sinks`.
fn enter_network<D: Device + ?Sized>(
    device: &D,
    source: WireId,
    sinks: &[WireId],
    other_nets: &HashMap<WireId, GateNetId>,
) -> Option<(Vec<PipId>, WireId, HashMap<WireId, PipId>)> {
    let first = *sinks.first()?;
    let candidates = reaching(device, first, other_nets);
    let mut came_from: HashMap<WireId, PipId> = HashMap::new();
    let mut seen: HashSet<WireId> = HashSet::from_iter([source]);
    let mut queue = VecDeque::from([source]);
    let mut tries = 0;
    while let Some(wire) = queue.pop_front() {
        if wire != source && candidates.contains(&wire) {
            let reached = reach_from(device, wire, other_nets);
            if sinks.iter().all(|s| *s == wire || reached.contains_key(s)) {
                let mut path = Vec::new();
                let mut cursor = wire;
                while cursor != source {
                    let &pip = came_from.get(&cursor)?;
                    path.push(pip);
                    cursor = device.pip(pip)?.src_wire;
                }
                path.reverse();
                return Some((path, wire, reached));
            }
            tries += 1;
            if tries >= MAX_ENTRY_TRIES {
                return None;
            }
            // A part of the network that misses some sinks: don't go further
            // in through it.
            continue;
        }
        if seen.len() > MAX_ENTRY_SEARCH {
            return None;
        }
        for &pip_id in device.wire_src_pips(wire) {
            let Some(pip) = device.pip(pip_id) else {
                continue;
            };
            if other_nets.contains_key(&pip.dst_wire) || !seen.insert(pip.dst_wire) {
                continue;
            }
            came_from.insert(pip.dst_wire, pip_id);
            queue.push_back(pip.dst_wire);
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

        let mut sinks = Vec::with_capacity(clock_pins.len());
        for &(cell, pin) in &clock_pins {
            let sink =
                clock_pin_wire(device, placement, cell, CellPin::Input(pin)).ok_or_else(|| {
                    PlaceRouteError::RoutingFailed(format!(
                        "A clock pin on net '{}' has no wire",
                        net.name
                    ))
                })?;
            sinks.push(sink);
        }

        let mut route = Route::new(net.id);
        route.source = source;
        let mut tree = HashSet::from_iter([source]);
        let mut reached = reach_from(device, source, &owners);
        if !sinks.iter().all(|s| reached.contains_key(s)) {
            // No dedicated path from the source itself: enter the network
            // through the fabric.
            let Some((entry_path, entry, entry_reached)) =
                enter_network(device, source, &sinks, &owners)
            else {
                eprintln!(
                    "warning: clock net '{}' has no way onto the dedicated clock network, so it uses general routing and may have high skew",
                    net.name
                );
                routing.general.insert(net.id);
                continue;
            };
            log::info!(
                "clock net '{}' enters the clock network at {} through {} fabric PIPs",
                net.name,
                device.wire(entry).map_or("?", |w| w.name.as_str()),
                entry_path.len()
            );
            for pip_id in entry_path {
                let Some(pip) = device.pip(pip_id) else {
                    continue;
                };
                route.delay += pip.delay;
                route.pips.push(pip_id);
                route.wires.push(pip.dst_wire);
                tree.insert(pip.dst_wire);
            }
            reached = entry_reached;
        }
        for &sink in &sinks {
            route.sinks.push(sink);
            if tree.contains(&sink) {
                continue;
            }
            let path = path_to(device, sink, &tree, &reached)
                .expect("every sink was reached by the dedicated search");
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
        // Other inputs on the net that the clock network reaches go on it
        // too: the general router cannot use the wires claimed here, and
        // some (a PLL's reference clock) are reachable only through them.
        for &(cell, pin) in net.fanout.iter().filter(|(_, pin)| !is_clock_pin(*pin)) {
            let Some(sink) = clock_pin_wire(device, placement, cell, CellPin::Input(pin)) else {
                continue;
            };
            let path = if tree.contains(&sink) {
                Vec::new()
            } else {
                match path_to(device, sink, &tree, &reached) {
                    Some(path) => path,
                    None => continue,
                }
            };
            for pip_id in path {
                let Some(pip) = device.pip(pip_id) else {
                    continue;
                };
                route.delay += pip.delay;
                route.pips.push(pip_id);
                route.wires.push(pip.dst_wire);
                tree.insert(pip.dst_wire);
            }
            route.sinks.push(sink);
            routing.dedicated_inputs.insert((cell, pin));
        }
        route.wires.push(source);
        for &wire in &tree {
            owners.insert(wire, net.id);
            // The source wire stays open to the general router: only this
            // net's driver drives it, and the net's other sinks (a PLL's
            // reference input, say) are routed from it.
            if wire != source {
                routing.claimed.insert(wire);
            }
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

        let reached = reach_from(&device, osc, &HashMap::new());
        let path =
            path_to(&device, sink, &HashSet::from_iter([osc]), &reached).expect("a dedicated path");
        assert!(!path.is_empty());
        for pip in path {
            let dst = device.pip(pip).unwrap().dst_wire;
            assert!(!device.is_general_routing_wire(dst));
        }
    }

    /// Logic is only placed where the oscillator reaches its clock pin on
    /// dedicated wires within the loaded window: the window's left edge
    /// column gets its clock branch from outside the window.
    #[test]
    fn every_placeable_logic_tile_has_a_dedicated_clock() {
        let Some(db) = find_database() else {
            eprintln!("PRJOXIDE_DB not found — skipping");
            return;
        };
        let device =
            NexusDevice::from_prjoxide_bbox(NexusVariant::Lfcpnx100, &db, (73, 0, 159, 40))
                .expect("build device");
        let wire = |x, y, name: &str| {
            resolve_wire(x, y, name).and_then(|w| device.wire_by_name(&node_name(&w)))
        };
        let osc = wire(137, 0, "S1:JHFCLKOUT_OSC_CORE").unwrap();
        let reached = reach_from(&device, osc, &HashMap::new());
        let mut checked = 0;
        for x in 73..=159u32 {
            let y = 20;
            let has_logic = device
                .tile_at(x, y)
                .is_some_and(|t| t.bels().iter().any(|b| b.bel_type.is_ff()));
            if !has_logic {
                continue;
            }
            let sink = wire(x, y, "JCLK_SLICEA").unwrap();
            assert!(
                reached.contains_key(&sink),
                "logic tile at column {x} has no dedicated clock path"
            );
            checked += 1;
        }
        assert!(checked > 40);
    }

    /// A clock pad with no dedicated path of its own (on the LFCPNX-100, the
    /// 125 MHz board clock's pad reaches the clock network only through a
    /// delay cell the database doesn't describe) enters the network after a
    /// few fabric hops, and the rest of the way is dedicated.
    #[test]
    fn a_pad_without_a_dedicated_path_enters_the_clock_network() {
        let Some(db) = find_database() else {
            eprintln!("PRJOXIDE_DB not found — skipping");
            return;
        };
        let device = NexusDevice::from_prjoxide_bbox(NexusVariant::Lfcpnx100, &db, (0, 0, 80, 60))
            .expect("build device");
        let wire = |x, y, name: &str| {
            device
                .wire_by_name(&node_name(&resolve_wire(x, y, name).unwrap()))
                .unwrap()
        };
        let pad = wire(0, 42, "JPADDI_SEIO33_CORE_IOA");
        let sinks = [wire(40, 20, "JCLK_SLICEA"), wire(60, 50, "JCLK_SLICEB")];
        assert!(
            !sinks
                .iter()
                .all(|s| reach_from(&device, pad, &HashMap::new()).contains_key(s)),
            "the pad has a dedicated path after all"
        );

        let (path, entry, reached) =
            enter_network(&device, pad, &sinks, &HashMap::new()).expect("an entry point");
        assert!(path.len() <= 8, "{} fabric PIPs", path.len());
        assert!(!device.is_general_routing_wire(entry));
        for sink in sinks {
            let tail = path_to(&device, sink, &HashSet::from_iter([entry]), &reached)
                .expect("a dedicated path from the entry");
            let names: Vec<&str> = tail
                .iter()
                .map(|&p| {
                    device
                        .wire(device.pip(p).unwrap().dst_wire)
                        .unwrap()
                        .name
                        .as_str()
                })
                .collect();
            assert!(
                names.iter().all(|n| !is_general(&device, n)),
                "general wire after the entry: {names:?}"
            );
            assert!(
                names.iter().any(|n| n.contains("DCC")),
                "the path skips the clock buffers: {names:?}"
            );
        }
    }

    fn is_general(device: &NexusDevice, name: &str) -> bool {
        device
            .wire_by_name(name)
            .is_some_and(|w| device.is_general_routing_wire(w))
    }
}
