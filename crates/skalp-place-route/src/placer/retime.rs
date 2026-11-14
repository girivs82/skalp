//! Retiming after placement.
//!
//! Retiming before placement ([`skalp_lir::retime`]) charges every
//! connection one typical routing delay, since nothing is placed yet. Once
//! cells have places, a connection between far tiles is slower than that
//! and one within a tile faster, so the registers that balanced the paths
//! by the estimate may not balance them as placed. [`Placer::retime_placed`]
//! retimes the same regions again with each connection's delay from where
//! its ends are, keeping the design's latency (retiming moves registers but
//! never adds or removes a cycle on any path), and then gives the registers
//! it added places:
//!
//! - A register on a connection keeps the place it had when retiming leaves
//!   it where it was (the same net, the same position along it).
//! - A new register goes beside the cell driving it: in the driver's tile
//!   when a flip-flop there is free and its control group takes it, on the
//!   flip-flop the driver reaches fastest (a Nexus LUT feeds the register
//!   beside it without general routing), else on the nearest free one.
//!
//! The timing retiming balances assumes exactly that: a connection holding
//! registers it keeps is timed through their places, and one it puts new
//! registers on is timed from a register beside its driver.

use super::control_sets::{control_sets, ControlSet};
use super::sites::{bel_types_compatible, SiteMap};
use super::{cell_bel_type, PlacementLoc, PlacementResult, Placer};
use crate::collections::{HashMap, HashMapExt};
use crate::device::Device;
use crate::error::{PlaceRouteError, Result};
use crate::timing::DelayModel;
use skalp_lir::gate_netlist::{Cell, CellId, GateNetId, GateNetlist};
use skalp_lir::retime::{
    longest_path, region_clock, retime, Connection, ConnectionDelays, RetimeDelays, RetimeOptions,
    RetimeReport,
};

/// [`RetimeDelays`] for a placed netlist: cell arcs and register timing
/// from `base`, each connection's routing from where its ends are placed
pub struct PlacedRetimeDelays<'a, D: Device + ?Sized> {
    base: &'a dyn RetimeDelays,
    device: &'a D,
    model: &'a DelayModel,
    placement: &'a PlacementResult,
}

impl<'a, D: Device + ?Sized> PlacedRetimeDelays<'a, D> {
    pub fn new(
        base: &'a dyn RetimeDelays,
        device: &'a D,
        model: &'a DelayModel,
        placement: &'a PlacementResult,
    ) -> Self {
        Self {
            base,
            device,
            model,
            placement,
        }
    }

    fn bel(&self, cell: &Cell) -> Option<(u32, u32, usize)> {
        self.placement
            .get(cell.id)
            .map(|loc| (loc.tile_x, loc.tile_y, loc.bel_index))
    }

    /// Delay (ps) from a cell on `from` to input `pin` of a cell on `to`
    fn between(&self, from: (u32, u32, usize), to: (u32, u32, usize), pin: usize) -> i64 {
        ns_to_ps(self.model.bel_connection_delay(from, to, pin))
    }

    /// Delay (ps) from a cell on `driver` to a register beside it: the
    /// fastest flip-flop of its tile
    fn to_register_beside(&self, driver: (u32, u32, usize)) -> i64 {
        let (x, y, _) = driver;
        let beside = self.device.tile_at(x, y).and_then(|tile| {
            tile.bels()
                .iter()
                .enumerate()
                .filter(|(_, bel)| bel.bel_type.is_ff())
                .map(|(index, _)| self.between(driver, (x, y, index), 0))
                .min()
        });
        beside.unwrap_or_else(|| ns_to_ps(self.model.connection_delay(0, 0)))
    }
}

fn ns_to_ps(ns: f64) -> i64 {
    (ns * 1000.0).round() as i64
}

impl<D: Device + ?Sized> RetimeDelays for PlacedRetimeDelays<'_, D> {
    fn arc_ps(&self, cell: &Cell, input: usize, output: usize) -> Option<i64> {
        self.base.arc_ps(cell, input, output)
    }
    fn clk_to_q_ps(&self) -> i64 {
        self.base.clk_to_q_ps()
    }
    fn setup_ps(&self) -> i64 {
        self.base.setup_ps()
    }
    fn net_ps(&self, fanout: usize) -> i64 {
        self.base.net_ps(fanout)
    }
    fn carry_link_ps(&self) -> i64 {
        self.base.carry_link_ps()
    }
    fn connection(&self, connection: &Connection) -> Option<ConnectionDelays> {
        let driver = self.bel(connection.driver?)?;
        let load = self.bel(connection.load)?;
        let direct_ps = self.between(driver, load, connection.pin);
        Some(match connection.registers {
            Some((first, last)) => ConnectionDelays {
                direct_ps,
                to_register_ps: self.between(driver, self.bel(first)?, 0),
                from_register_ps: self.between(self.bel(last)?, load, connection.pin),
            },
            None => ConnectionDelays {
                direct_ps,
                to_register_ps: self.to_register_beside(driver),
                from_register_ps: ns_to_ps(
                    self.model
                        .connection_delay(driver.0.abs_diff(load.0), driver.1.abs_diff(load.1)),
                ),
            },
        })
    }
}

/// A netlist retimed after placement, placed
pub struct PlacedRetiming {
    pub netlist: GateNetlist,
    pub placement: PlacementResult,
    pub report: RetimeReport,
    /// Registers that kept the place a register had there before
    pub kept_in_place: usize,
    /// New registers placed in their driver's tile
    pub beside_driver: usize,
}

/// Where a register sits in the chain of registers retiming owns on a net:
/// the net the chain starts from, how many registers come before it, and
/// its initial value
type ChainKey = (GateNetId, usize, bool);

/// The [`ChainKey`] of register `cell`, the registers of its chain being
/// those `in_chain` accepts
fn chain_key(netlist: &GateNetlist, cell: &Cell, in_chain: impl Fn(CellId) -> bool) -> ChainKey {
    let init = cell
        .parameters
        .get("INIT")
        .is_some_and(|v| v == "1" || v == "1'b1");
    let mut depth = 0;
    let mut net = netlist.resolve_alias(cell.inputs[0]);
    while let Some(driver) = netlist.nets[net.0 as usize].driver {
        if !in_chain(driver) || depth > netlist.cells.len() {
            break;
        }
        depth += 1;
        net = netlist.resolve_alias(netlist.cells[driver.0 as usize].inputs[0]);
    }
    (net, depth, init)
}

impl<D: Device + Clone> Placer<D> {
    /// Retime `netlist`'s regions ([`GateNetlist::retime_regions`]) with the
    /// connection delays of `placement`, aiming at `target_period_ps` (as
    /// short as they go when `None`, or when it can't be reached), and
    /// place the registers that results in. `base` gives the cells' own
    /// delays. `None` when there is nothing to retime or nothing to gain.
    pub fn retime_placed(
        &self,
        netlist: &GateNetlist,
        placement: &PlacementResult,
        base: &dyn RetimeDelays,
        target_period_ps: Option<i64>,
    ) -> Result<Option<PlacedRetiming>> {
        if netlist.retime_regions.is_empty() {
            return Ok(None);
        }
        let model = self.delay_model();
        let delays = PlacedRetimeDelays::new(base, &self.device, &model, placement);
        let options = RetimeOptions {
            regions: netlist.retime_regions.clone(),
            target_period_ps,
        };
        let mut retimed = netlist.clone();
        let Some(report) = retime(&mut retimed, &delays, &options) else {
            if let Some((ps, steps)) = longest_path(netlist, &delays, &options.regions) {
                let verdict = match target_period_ps {
                    Some(t) if ps <= t => format!(", within the {t} ps target"),
                    Some(t) => format!(", over the {t} ps target, and no retiming shortens it"),
                    None => ", and no retiming shortens it".to_string(),
                };
                log::info!(
                    "retiming after placement: longest placed path through the regions {ps} ps{verdict}{}",
                    if steps.is_empty() {
                        String::new()
                    } else {
                        format!(":\n    {}", steps.join("\n    "))
                    }
                );
            }
            return Ok(None);
        };
        let sets = control_sets(&retimed);
        let fill_limit = self.fill_limit(&retimed);
        let mut sites = SiteMap::new(&self.device).with_fill_limit(fill_limit);
        let mut placed = PlacementResult {
            placements: Default::default(),
            ..placement.clone()
        };
        // Every cell retiming kept stays where it was
        for (old, new) in report.cell_map.iter().enumerate() {
            let (Some(new), Some(loc)) = (new, placement.get(CellId(old as u32))) else {
                continue;
            };
            sites.take(
                loc.tile_x,
                loc.tile_y,
                loc.bel_index,
                sets.get(new).copied(),
            );
            placed.placements.insert(*new, *loc);
        }
        // Where the registers retiming replaced were, by place in their chain
        let removed = |id: CellId| report.cell_map.get(id.0 as usize) == Some(&None);
        let mut was_at: HashMap<ChainKey, PlacementLoc> = HashMap::new();
        for (old, new) in report.cell_map.iter().enumerate() {
            let cell = &netlist.cells[old];
            if new.is_some() || cell.inputs.is_empty() {
                continue;
            }
            if let Some(loc) = placement.get(cell.id) {
                was_at.insert(chain_key(netlist, cell, removed), *loc);
            }
        }
        let added = report.added.clone();
        let is_added = |id: CellId| added.contains(&(id.0 as usize));
        let mut kept_in_place = 0;
        let mut beside_driver = 0;
        for index in added.clone() {
            let cell = &retimed.cells[index];
            let set = sets.get(&cell.id).copied();
            let required = cell_bel_type(cell);
            let key = chain_key(&retimed, cell, is_added);
            if let Some(loc) = was_at.get(&key) {
                if sites.retake(loc.tile_x, loc.tile_y, loc.bel_index, set) {
                    placed.placements.insert(cell.id, *loc);
                    kept_in_place += 1;
                    continue;
                }
            }
            let loc = self
                .beside_driver(&retimed, &placed, &mut sites, &model, cell, required, set)
                .inspect(|_| beside_driver += 1)
                .or_else(|| {
                    let (x, y) = self.driver_tile(&retimed, &placed, cell)?;
                    let (tx, ty, bel) = sites.take_nearest(
                        x as f64,
                        y as f64,
                        required,
                        set,
                        bel_types_compatible,
                    )?;
                    Some(self.loc_at(tx, ty, bel))
                })
                .ok_or_else(|| {
                    PlaceRouteError::PlacementFailed(format!(
                        "No free flip-flop for register '{}' added by retiming after placement",
                        cell.path
                    ))
                })?;
            placed.placements.insert(cell.id, loc);
        }
        Ok(Some(PlacedRetiming {
            netlist: retimed,
            placement: placed,
            report,
            kept_in_place,
            beside_driver,
        }))
    }

    /// The tile of the cell driving register `cell`'s data input
    fn driver_tile(
        &self,
        netlist: &GateNetlist,
        placement: &PlacementResult,
        cell: &Cell,
    ) -> Option<(u32, u32)> {
        let net = netlist.resolve_alias(cell.inputs[0]);
        let driver = netlist.nets[net.0 as usize].driver?;
        placement.get(driver).map(PlacementLoc::coords)
    }

    /// Take the flip-flop of the driver's tile the driver reaches fastest
    #[allow(clippy::too_many_arguments)]
    fn beside_driver(
        &self,
        netlist: &GateNetlist,
        placement: &PlacementResult,
        sites: &mut SiteMap,
        model: &DelayModel,
        cell: &Cell,
        required: super::BelType,
        set: Option<ControlSet>,
    ) -> Option<PlacementLoc> {
        let net = netlist.resolve_alias(cell.inputs[0]);
        let driver = placement.get(netlist.nets[net.0 as usize].driver?)?;
        let (x, y) = driver.coords();
        let from = (x, y, driver.bel_index);
        let bel = sites
            .open_bels(x, y, required, set.as_ref(), bel_types_compatible)
            .into_iter()
            .min_by(|&a, &b| {
                let delay = |bel| model.bel_connection_delay(from, (x, y, bel), 0);
                delay(a).total_cmp(&delay(b)).then(a.cmp(&b))
            })?;
        sites.take(x, y, bel, set).then(|| self.loc_at(x, y, bel))
    }

    fn loc_at(&self, x: u32, y: u32, bel: usize) -> PlacementLoc {
        let bel_type = self
            .device
            .tile_at(x, y)
            .and_then(|t| t.bels().get(bel).map(|b| b.bel_type))
            .unwrap_or(super::BelType::Dff);
        PlacementLoc::new(x, y, bel, bel_type)
    }
}

/// The period retiming after placement aims at: the one the regions'
/// clock must meet, from `period_of` (in ps, given the clock net)
pub fn placed_retime_target(
    netlist: &GateNetlist,
    period_of: impl Fn(GateNetId) -> i64,
) -> Option<i64> {
    region_clock(netlist, &netlist.retime_regions).map(period_of)
}
