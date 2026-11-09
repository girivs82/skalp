//! Compact routing graph for the router's searches.
//!
//! The device describes its routing resources as `Wire` and `Pip` records and
//! a per-wire list of driven PIPs, which on the large Lattice devices is a
//! hash map. A route search touches millions of edges, and following each one
//! through that hash map plus two record lookups (the PIP, then the wire it
//! reaches for the heuristic) made routing memory-bound. [`RoutingGraph`]
//! flattens what the search needs into arrays indexed by wire: the outgoing
//! edges of every wire in one contiguous slice, each with its destination,
//! PIP and costs, and every wire's tile.

use crate::device::{Device, PipId, WireId};
use crate::timing::DelayModel;

/// One PIP as the search sees it
#[derive(Debug, Clone, Copy)]
pub struct Edge {
    /// Wire the PIP drives
    pub dst: WireId,
    /// The PIP itself
    pub pip: PipId,
    /// Base routing cost ([`crate::device::Pip::cost`])
    pub cost: f32,
    /// Delay term, scaled by a net's timing weight in timing-driven searches
    pub timing: f32,
}

/// Flattened adjacency of a device's routing wires
#[derive(Debug, Clone, Default)]
pub struct RoutingGraph {
    /// `edges[offsets[w]..offsets[w + 1]]` are the PIPs wire `w` drives
    offsets: Vec<u32>,
    edges: Vec<Edge>,
    /// Tile of each wire
    tile: Vec<(u32, u32)>,
    /// Intrinsic delay of each wire (ps)
    delay: Vec<u32>,
}

impl RoutingGraph {
    /// Build the graph of `device`'s wires `0..device.wire_count()`. With a
    /// delay model, an edge's timing term is the model's typed PIP delay
    /// (in 50 ps units); otherwise the PIP's own delay.
    pub fn build<D: Device + ?Sized>(device: &D, delay_model: Option<&DelayModel>) -> Self {
        let n = device.wire_count();
        let mut offsets = Vec::with_capacity(n + 1);
        let mut edges = Vec::new();
        let mut tile = Vec::with_capacity(n);
        let mut delay = Vec::with_capacity(n);
        offsets.push(0);
        for i in 0..n {
            let id = WireId(i as u32);
            let wire = device.wire(id);
            tile.push(wire.map_or((0, 0), |w| (w.tile_x, w.tile_y)));
            delay.push(wire.map_or(0, |w| w.delay));
            for &pip_id in device.wire_src_pips(id) {
                let Some(pip) = device.pip(pip_id) else {
                    continue;
                };
                let timing = match (delay_model, wire, device.wire(pip.dst_wire)) {
                    (Some(model), Some(src), Some(dst)) => {
                        model.pip_delay_typed(&src.wire_type, &dst.wire_type) / 0.05
                    }
                    (Some(model), _, _) => model.pip_delay / 0.05,
                    (None, _, _) => pip.delay as f64 / 50.0,
                };
                edges.push(Edge {
                    dst: pip.dst_wire,
                    pip: pip_id,
                    cost: pip.cost() as f32,
                    timing: timing as f32,
                });
            }
            offsets.push(edges.len() as u32);
        }
        Self {
            offsets,
            edges,
            tile,
            delay,
        }
    }

    /// Number of wires
    pub fn wire_count(&self) -> usize {
        self.tile.len()
    }

    /// PIPs driven by `wire`
    #[inline]
    pub fn edges(&self, wire: WireId) -> &[Edge] {
        let i = wire.0 as usize;
        match (self.offsets.get(i), self.offsets.get(i + 1)) {
            (Some(&a), Some(&b)) => &self.edges[a as usize..b as usize],
            _ => &[],
        }
    }

    /// Tile of `wire`
    #[inline]
    pub fn tile(&self, wire: WireId) -> Option<(u32, u32)> {
        self.tile.get(wire.0 as usize).copied()
    }

    /// Intrinsic delay of `wire` (ps)
    #[inline]
    pub fn wire_delay(&self, wire: WireId) -> u32 {
        self.delay.get(wire.0 as usize).copied().unwrap_or(0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::device::ice40::{Ice40Device, Ice40Variant};

    #[test]
    fn graph_matches_device_pips() {
        let device = Ice40Device::new(Ice40Variant::Hx1k);
        let graph = RoutingGraph::build(&device, None);
        assert_eq!(graph.wire_count(), device.wire_count());
        let mut total = 0;
        for i in 0..device.wire_count() {
            let w = WireId(i as u32);
            let from_device: Vec<PipId> = device.wire_src_pips(w).to_vec();
            let from_graph: Vec<PipId> = graph.edges(w).iter().map(|e| e.pip).collect();
            assert_eq!(from_graph, from_device, "wire {i}");
            for e in graph.edges(w) {
                let pip = device.pip(e.pip).unwrap();
                assert_eq!(e.dst, pip.dst_wire);
                assert_eq!(e.cost, pip.cost() as f32);
            }
            let wire = device.wire(w).unwrap();
            assert_eq!(graph.tile(w), Some((wire.tile_x, wire.tile_y)));
            total += from_device.len();
        }
        assert!(total > 0);
    }
}
