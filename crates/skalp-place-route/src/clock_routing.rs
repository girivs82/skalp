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

/// The PIPs of a shortest path from one of the wires `from` to `sink`, off
/// the wires of `tree` (they have a driver already) and of other nets.
fn branch_from<D: Device + ?Sized>(
    device: &D,
    from: &[WireId],
    tree: &HashSet<WireId>,
    sink: WireId,
    other_nets: &HashMap<WireId, GateNetId>,
) -> Option<Vec<PipId>> {
    if from.contains(&sink) {
        return Some(Vec::new());
    }
    let mut came_from: HashMap<WireId, PipId> = HashMap::new();
    let mut seen: HashSet<WireId> = tree.clone();
    seen.extend(from.iter().copied());
    let mut queue: VecDeque<WireId> = from.iter().copied().collect();
    let starts: HashSet<WireId> = from.iter().copied().collect();
    while let Some(wire) = queue.pop_front() {
        if wire == sink {
            let mut path = Vec::new();
            let mut cursor = wire;
            while !starts.contains(&cursor) {
                let &pip = came_from.get(&cursor)?;
                path.push(pip);
                cursor = device.pip(pip)?.src_wire;
            }
            path.reverse();
            return Some(path);
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

/// Most wires searched for an entry point.
const MAX_ENTRY_SEARCH: usize = 2_000_000;

/// The ways into the dedicated network that lead to `sink`: the dedicated
/// wires above it (off other nets) that a general wire drives. Empty when
/// nothing in the fabric can reach `sink` any more: on a device whose clock
/// pins take their clock from the network only, the network's entries
/// above `sink` are all held by other clocks.
fn entries_to<D: Device + ?Sized>(
    device: &D,
    sink: WireId,
    other_nets: &HashMap<WireId, GateNetId>,
) -> HashSet<WireId> {
    reaching(device, sink, other_nets)
        .into_iter()
        .filter(|&wire| {
            device.wire_pips(wire).into_iter().any(|pip_id| {
                device.pip(pip_id).is_some_and(|pip| {
                    device.is_general_routing_wire(pip.src_wire)
                        && !other_nets.contains_key(&pip.src_wire)
                })
            })
        })
        .collect()
}

/// A way into the dedicated network: the fabric PIPs from the source to the
/// entry wire, the entry wire, and the dedicated search tree from it
type NetworkEntry = (Vec<PipId>, WireId, HashMap<WireId, PipId>);

/// Why a source with no dedicated path of its own found no way into the
/// network for all of its sinks
#[derive(Debug, PartialEq, Eq)]
enum NoEntry {
    /// No single entry reaches every sink, but each sink has one: general
    /// routing can still reach them
    NoCommonEntry,
    /// The fabric cannot reach this sink at all
    Unreachable(WireId),
}

/// A way into the dedicated network for a source that has none of its own:
/// the PIPs from `source` to an entry wire, fewest hops first, and the
/// dedicated search tree from that entry, which reaches every one of `sinks`.
///
/// Only entries from which every sink can be reached are searched for, so
/// the nearest one the fabric reaches is taken, however many entries other
/// clocks hold. A sink with no free entry above it is reported at once:
/// general routing could not reach it either.
///
/// The nearest entry whose whole route (fabric path and the dedicated paths
/// to every sink) uses only switches the timing data has a delay for wins
/// over a nearer one that does not: a switch with no delay is timed as 0 ps,
/// so every clock edge through it would arrive later on silicon than the
/// analysis says. Only when no entry is fully timed is the nearest taken.
fn enter_network<D: Device + ?Sized>(
    device: &D,
    source: WireId,
    sinks: &[WireId],
    other_nets: &HashMap<WireId, GateNetId>,
    is_untimed: &dyn Fn(PipId) -> bool,
) -> std::result::Result<NetworkEntry, NoEntry> {
    // Wires that reach each sink on their own reach them all together:
    // the paths belong to one net, so they may share wires.
    let mut distinct: Vec<WireId> = sinks.to_vec();
    distinct.sort_unstable_by_key(|w| w.0);
    distinct.dedup();
    let mut common: Option<HashSet<WireId>> = None;
    for &sink in &distinct {
        let entries = entries_to(device, sink, other_nets);
        if entries.is_empty() {
            return Err(NoEntry::Unreachable(sink));
        }
        common = Some(match common {
            None => entries,
            Some(common) => common.intersection(&entries).copied().collect(),
        });
    }
    let common = common.unwrap_or_default();
    if common.is_empty() {
        return Err(NoEntry::NoCommonEntry);
    }

    let mut came_from: HashMap<WireId, PipId> = HashMap::new();
    let mut seen: HashSet<WireId> = HashSet::from_iter([source]);
    let mut queue = VecDeque::from([source]);
    // The nearest entry, kept in case no entry is timed all the way
    let mut nearest: Option<NetworkEntry> = None;
    // Entries not yet looked at: once all have been, the search can stop
    let mut left = common.len();
    while let Some(wire) = queue.pop_front() {
        if wire != source && common.contains(&wire) {
            left -= 1;
            let reached = reach_from(device, wire, other_nets);
            if !sinks.iter().all(|s| *s == wire || reached.contains_key(s)) {
                // Cannot happen: each sink was reached from this wire
                continue;
            }
            let mut path = Vec::new();
            let mut cursor = wire;
            while cursor != source {
                let Some(&pip) = came_from.get(&cursor) else {
                    return Err(NoEntry::NoCommonEntry);
                };
                path.push(pip);
                cursor = device
                    .pip(pip)
                    .map(|p| p.src_wire)
                    .ok_or(NoEntry::NoCommonEntry)?;
            }
            path.reverse();
            let entry = (path, wire, reached);
            if entry_is_timed(device, &entry, sinks, is_untimed) {
                return Ok(entry);
            }
            if nearest.is_none() {
                nearest = Some(entry);
            }
            if left == 0 {
                break;
            }
        }
        if seen.len() > MAX_ENTRY_SEARCH {
            break;
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
    nearest.ok_or(NoEntry::NoCommonEntry)
}

/// Whether every switch of a way into the network (its fabric path, and
/// the dedicated paths from its entry to each of `sinks`) has a delay in
/// the timing data
fn entry_is_timed<D: Device + ?Sized>(
    device: &D,
    (path, entry, reached): &NetworkEntry,
    sinks: &[WireId],
    is_untimed: &dyn Fn(PipId) -> bool,
) -> bool {
    if path.iter().any(|&pip| is_untimed(pip)) {
        return false;
    }
    let mut checked: HashSet<WireId> = HashSet::from_iter([*entry]);
    for &sink in sinks {
        let mut cursor = sink;
        while checked.insert(cursor) {
            let Some(&pip) = reached.get(&cursor) else {
                break;
            };
            if is_untimed(pip) {
                return false;
            }
            let Some(info) = device.pip(pip) else {
                break;
            };
            cursor = info.src_wire;
        }
    }
    true
}

/// Route every clock pin of a design prepared for routing.
///
/// `is_untimed` tells the switches the timing data has no delay for; a clock
/// made in the fabric enters the network where it avoids them, if it can
/// (see [`enter_network`]).
pub fn route_clock_nets<D: Device + ?Sized>(
    device: &D,
    netlist: &GateNetlist,
    placement: &PlacementResult,
    is_untimed: &dyn Fn(PipId) -> bool,
) -> Result<ClockRouting> {
    let mut routing = ClockRouting::default();
    let mut owners: HashMap<WireId, GateNetId> = HashMap::new();
    // Clock nets the fabric cannot get to one of their clock pins at all
    let mut stranded: Vec<String> = Vec::new();

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
        // The fabric wires from the source to the network, when it has no
        // dedicated path of its own
        let mut stem: Vec<WireId> = Vec::new();
        if !sinks.iter().all(|s| reached.contains_key(s)) {
            // No dedicated path from the source itself: enter the network
            // through the fabric.
            let (entry_path, entry, entry_reached) = match enter_network(
                device, source, &sinks, &owners, is_untimed,
            ) {
                Ok(entry) => entry,
                Err(NoEntry::Unreachable(sink)) => {
                    log::info!(
                        "clock net '{}' cannot reach clock pin {}",
                        net.name,
                        device.wire(sink).map_or("?", |w| w.name.as_str())
                    );
                    stranded.push(net.name.clone());
                    continue;
                }
                Err(NoEntry::NoCommonEntry) => {
                    let unreached: Vec<&str> = sinks
                        .iter()
                        .filter(|s| !reached.contains_key(s))
                        .take(5)
                        .filter_map(|&s| device.wire(s).map(|w| w.name.as_str()))
                        .collect();
                    eprintln!(
                            "warning: clock net '{}' has no way onto the dedicated clock network, so it uses general routing and may have high skew (dedicated wires from its source miss {})",
                            net.name,
                            unreached.join(", ")
                        );
                    routing.general.insert(net.id);
                    continue;
                }
            };
            log::info!(
                "clock net '{}' enters the clock network at {} through {} fabric PIPs",
                net.name,
                device.wire(entry).map_or("?", |w| w.name.as_str()),
                entry_path.len()
            );
            log::debug!(
                "clock net '{}' fabric entry from {}: {}",
                net.name,
                device.wire(source).map_or("?", |w| w.name.as_str()),
                entry_path
                    .iter()
                    .filter_map(|&p| device.pip(p))
                    .filter_map(|p| device.wire(p.dst_wire).map(|w| w.name.as_str()))
                    .collect::<Vec<_>>()
                    .join(" -> ")
            );
            for pip_id in entry_path {
                let Some(pip) = device.pip(pip_id) else {
                    continue;
                };
                route.delay += pip.delay;
                route.pips.push(pip_id);
                route.wires.push(pip.dst_wire);
                tree.insert(pip.dst_wire);
                stem.push(pip.dst_wire);
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
        // A source that enters the network through the fabric usually has a
        // single way out of its tile, and the stem now holds it, so the
        // general router could not reach the net's other inputs (logic that
        // also reads a clock made in the fabric). Branch them off the stem
        // here.
        if !stem.is_empty() {
            let mut from: Vec<WireId> = vec![source];
            from.extend(stem.iter().copied());
            for &(cell, pin) in net.fanout.iter().filter(|(_, pin)| !is_clock_pin(*pin)) {
                if routing.dedicated_inputs.contains(&(cell, pin)) {
                    continue;
                }
                let Some(sink) = clock_pin_wire(device, placement, cell, CellPin::Input(pin))
                else {
                    continue;
                };
                let Some(path) = branch_from(device, &from, &tree, sink, &owners) else {
                    return Err(PlaceRouteError::RoutingFailed(format!(
                        "Clock net '{}' enters the clock network through the fabric, and one of its other inputs cannot be reached from that path",
                        net.name
                    )));
                };
                for pip_id in path {
                    let Some(pip) = device.pip(pip_id) else {
                        continue;
                    };
                    route.pips.push(pip_id);
                    route.wires.push(pip.dst_wire);
                    tree.insert(pip.dst_wire);
                    from.push(pip.dst_wire);
                }
                route.sinks.push(sink);
                routing.dedicated_inputs.insert((cell, pin));
            }
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
    if !stranded.is_empty() {
        // General routing would search for these until it gave up: on this
        // device the pins take their clock from the dedicated network only,
        // and every way onto it that leads to them is in use.
        return Err(PlaceRouteError::ClockEntriesExhausted {
            nets: stranded,
            routed: routing.routes.len(),
        });
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

    /// The LFCPNX-100's 125 MHz board clock pad (P24, a PCLK pin) reaches the
    /// clock network on dedicated wires, through its pin's delay cell
    /// (DLLDEL), which the routing turns on.
    #[test]
    fn a_clock_pin_pad_reaches_the_clock_network_through_its_delay_cell() {
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
        let reached = reach_from(&device, pad, &HashMap::new());
        for sink in [wire(40, 20, "JCLK_SLICEA"), wire(60, 50, "JCLK_SLICEB")] {
            let path = path_to(&device, sink, &HashSet::from_iter([pad]), &reached)
                .expect("a dedicated path from the pad");
            let names: Vec<&str> = path
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
                names.iter().any(|n| n.contains("CLKOUT_DLLDEL")),
                "the path skips the delay cell: {names:?}"
            );
            assert!(
                names.iter().all(|n| !is_general(&device, n)),
                "general wire on the path: {names:?}"
            );
        }
    }

    /// A pad with no dedicated path of its own (an ordinary I/O pin, not a
    /// clock pin) enters the network after a few fabric hops, and the rest
    /// of the way is dedicated.
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
        let sinks = [wire(40, 20, "JCLK_SLICEA"), wire(60, 50, "JCLK_SLICEB")];
        // The first left-edge pad that cannot reach the sinks on dedicated wires
        let pad = (1..60u32)
            .filter_map(|y| {
                let key = resolve_wire(0, y, "JPADDI_SEIO33_CORE_IOA")?;
                device.wire_by_name(&node_name(&key))
            })
            .find(|&pad| {
                let reached = reach_from(&device, pad, &HashMap::new());
                !sinks.iter().all(|s| reached.contains_key(s))
            })
            .expect("a left-edge pad with no dedicated clock path");

        let (path, entry, reached) =
            enter_network(&device, pad, &sinks, &HashMap::new(), &|_| false)
                .expect("an entry point");
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

    /// The nearest free entry is found however many entries nearer the
    /// source other clocks hold: the search used to give up after 32
    /// entries, leaving the clock to general routing that cannot reach a
    /// clock pin on this device.
    #[test]
    fn a_clock_enters_through_the_one_entry_left_free() {
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
        // A slice output: a clock made in the fabric
        let source = wire(30, 30, "JF0");
        let sinks = [wire(40, 20, "JCLK_SLICEA"), wire(60, 50, "JCLK_SLICEB")];
        let free = HashMap::new();
        let common: HashSet<WireId> = entries_to(&device, sinks[0], &free)
            .intersection(&entries_to(&device, sinks[1], &free))
            .copied()
            .collect();
        assert!(
            common.len() > 32,
            "{} entries reach both sinks",
            common.len()
        );
        let (_, nearest, _) = enter_network(&device, source, &sinks, &free, &|_| false).unwrap();

        // Another clock holds every entry but the one farthest from the
        // nearest (by tile), so the search passes all the others first
        let tile_of = |w: WireId| {
            let name = &device.wire(w).unwrap().name;
            let (r, c) = name[1..].split_once('C').unwrap();
            let c: String = c.chars().take_while(|ch| ch.is_ascii_digit()).collect();
            (r.parse::<i64>().unwrap(), c.parse::<i64>().unwrap())
        };
        let (nr, nc) = tile_of(nearest);
        let farthest = *common
            .iter()
            .max_by_key(|&&w| {
                let (r, c) = tile_of(w);
                ((r - nr).abs() + (c - nc).abs(), w.0)
            })
            .unwrap();
        let other = GateNetId(u32::MAX);
        let taken: HashMap<WireId, GateNetId> = common
            .iter()
            .filter(|&&w| w != farthest)
            .map(|&w| (w, other))
            .collect();
        let (path, entry, reached) =
            enter_network(&device, source, &sinks, &taken, &|_| false).expect("the free entry");
        assert_eq!(entry, farthest);
        assert!(path
            .iter()
            .all(|&p| !taken.contains_key(&device.pip(p).unwrap().dst_wire)));
        for sink in sinks {
            assert!(reached.contains_key(&sink));
        }
    }

    /// A clock made in the fabric enters the network past a nearer entry
    /// whose switches the timing data has no delay for, and takes the
    /// nearest entry only when no entry is timed all the way.
    #[test]
    fn a_clock_enters_where_every_switch_is_timed() {
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
        let source = wire(30, 30, "JF0");
        let sinks = [wire(40, 20, "JCLK_SLICEA"), wire(60, 50, "JCLK_SLICEB")];
        let free = HashMap::new();
        let (_, nearest, _) = enter_network(&device, source, &sinks, &free, &|_| false).unwrap();

        // Every switch out of the nearest entry has no delay
        let untimed = |pip: PipId| device.pip(pip).is_some_and(|p| p.src_wire == nearest);
        let entry = enter_network(&device, source, &sinks, &free, &untimed).unwrap();
        assert_ne!(entry.1, nearest);
        assert!(entry_is_timed(&device, &entry, &sinks, &untimed));
        assert!(!entry.0.iter().any(|&p| untimed(p)));

        // Nothing timed anywhere: the nearest entry still routes the clock
        let (_, fallback, _) = enter_network(&device, source, &sinks, &free, &|_| true).unwrap();
        assert_eq!(fallback, nearest);
    }

    /// With every entry above a clock pin held by other clocks, the pin
    /// cannot be reached from the fabric at all, and that is known at once
    /// instead of after general routing gives up.
    #[test]
    fn a_clock_pin_with_every_entry_taken_is_reported_unreachable() {
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
        let source = wire(30, 30, "JF0");
        let sinks = [wire(40, 20, "JCLK_SLICEA"), wire(60, 50, "JCLK_SLICEB")];
        let other = GateNetId(u32::MAX);
        let taken: HashMap<WireId, GateNetId> = entries_to(&device, sinks[1], &HashMap::new())
            .into_iter()
            .map(|w| (w, other))
            .collect();
        assert!(entries_to(&device, sinks[1], &taken).is_empty());
        match enter_network(&device, source, &sinks, &taken, &|_| false) {
            Err(NoEntry::Unreachable(sink)) => assert!(sinks.contains(&sink)),
            other => panic!("expected an unreachable sink, got {:?}", other.map(|r| r.1)),
        }
    }

    fn is_general(device: &NexusDevice, name: &str) -> bool {
        device
            .wire_by_name(name)
            .is_some_and(|w| device.is_general_routing_wire(w))
    }
}
