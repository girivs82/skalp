//! Prepare a placed design for routing and configuration.
//!
//! Family-independent: turns a netlist's connectivity into what the router and
//! bitstream emitters need, handling the connections with no routing (constants
//! and dedicated carry paths) and fixing IO cells on their assigned sites.

use crate::device::{BelType, Device, DIST_RAM_PIN_RAD, FF_PIN_CE, FF_PIN_CLK, FF_PIN_LSR};
use crate::error::{PlaceRouteError, Result};
use crate::placer::PlacementResult;
use skalp_lir::gate_netlist::{Cell, CellId, GateNet, GateNetId, GateNetlist};
use skalp_lir::tech_library::CellFunction;
use std::collections::HashMap;

fn emit_err(message: String) -> PlaceRouteError {
    PlaceRouteError::BitstreamFailed(message)
}

/// Constant inputs found while preparing a netlist for routing.
#[derive(Debug, Default)]
pub struct TiedInputs {
    /// (cell, input index) → the constant it reads
    values: HashMap<(CellId, usize), bool>,
}

impl TiedInputs {
    /// The constant an input reads, if it is tied.
    pub fn value(&self, cell: CellId, index: usize) -> Option<bool> {
        self.values.get(&(cell, index)).copied()
    }
}

pub(crate) fn constant_value(cell: &Cell) -> Option<bool> {
    match cell.function {
        Some(CellFunction::TieHigh) => Some(true),
        Some(CellFunction::TieLow) => Some(false),
        _ if crate::placer::is_constant_cell(&cell.cell_type) => {
            Some(cell.cell_type.contains('1') || cell.cell_type.contains("VCC"))
        }
        _ => None,
    }
}

fn is_carry(cell: &Cell) -> bool {
    cell.function == Some(CellFunction::Ccu2)
}

pub(crate) fn is_pad(cell: &Cell) -> bool {
    matches!(
        cell.function,
        Some(CellFunction::InputPad)
            | Some(CellFunction::OutputPad)
            | Some(CellFunction::BidirPad)
            | Some(CellFunction::ClockPad)
    )
}

fn is_flip_flop(cell: &Cell) -> bool {
    cell.clock.is_some()
}

/// Router pin number of a flip-flop's `index`-th input.
fn ff_pin(cell: &Cell, index: usize) -> usize {
    match (cell.cell_type.as_str(), index) {
        ("OXIDE_FF_EN", 1) | ("OXIDE_FF_RSTE", 2) => FF_PIN_CE,
        ("OXIDE_FF_RST", 1) | ("OXIDE_FF_RSTE", 1) => FF_PIN_LSR,
        _ => index,
    }
}

/// Which net drives each constant cell's output.
fn constant_nets(netlist: &GateNetlist) -> HashMap<GateNetId, bool> {
    netlist
        .cells
        .iter()
        .filter_map(|cell| {
            let value = constant_value(cell)?;
            Some((*cell.outputs.first()?, value))
        })
        .collect()
}

/// Rewrite a netlist's connectivity into what the router should route.
///
/// Rebuilds every net's fanout from the cells, because clock pins are absent
/// from it. Leaves out connections with no routing: constants (set with
/// [`emit_bits`]) and the dedicated carry path from one `CCU2` to the next.
pub fn prepare_for_routing(netlist: &GateNetlist) -> (GateNetlist, TiedInputs) {
    let constants = constant_nets(netlist);
    let mut prepared = netlist.clone();
    let mut tied = TiedInputs::default();
    for net in &mut prepared.nets {
        net.fanout.clear();
        // Routed as ordinary signals: LFCPNX clock tree data is incomplete.
        net.is_clock = false;
        net.is_reset = false;
    }

    let connect = |nets: &mut Vec<GateNet>, net: GateNetId, cell: CellId, pin: usize| {
        nets[net.0 as usize].fanout.push((cell, pin));
    };
    for cell in &netlist.cells {
        if constant_value(cell).is_some() {
            continue;
        }
        for (index, &net) in cell.inputs.iter().enumerate() {
            if let Some(&value) = constants.get(&net) {
                tied.values.insert((cell.id, index), value);
                continue;
            }
            if is_carry(cell) && index == 8 {
                continue;
            }
            if cell.function == Some(CellFunction::DistRam16x4) && (9..=12).contains(&index) {
                // Each read address bit feeds all four storage LUTs.
                for lut in 0..4 {
                    let pin = DIST_RAM_PIN_RAD + 4 * (index - 9) + lut;
                    connect(&mut prepared.nets, net, cell.id, pin);
                }
                continue;
            }
            let pin = if is_flip_flop(cell) {
                ff_pin(cell, index)
            } else {
                index
            };
            connect(&mut prepared.nets, net, cell.id, pin);
        }
        if let Some(clock) = cell.clock {
            connect(&mut prepared.nets, clock, cell.id, FF_PIN_CLK);
        }
        if let Some(reset) = cell.reset {
            connect(&mut prepared.nets, reset, cell.id, FF_PIN_LSR);
        }
    }
    (prepared, tied)
}

/// Copy a netlist for the placer, hiding pads so only logic gets placed.
pub fn logic_only_netlist(netlist: &GateNetlist) -> GateNetlist {
    let mut logic = netlist.clone();
    for cell in &mut logic.cells {
        if is_pad(cell) {
            cell.cell_type = "TIE_PAD".to_string();
            cell.function = None;
            cell.parameters.clear();
        }
    }
    logic
}

/// Fix each pad on the site its `LOC` names.
pub fn place_pads(
    device: &dyn Device,
    netlist: &GateNetlist,
    placement: &mut PlacementResult,
) -> Result<()> {
    let packages = device.packages();
    let pins = packages
        .values()
        .next()
        .map(|p| &p.pins)
        .ok_or_else(|| emit_err("No package pin data for this device".to_string()))?;

    for cell in netlist.cells.iter().filter(|c| is_pad(c)) {
        let Some(loc) = cell.parameters.get("LOC") else {
            return Err(emit_err(format!(
                "Pad '{}' has no pin assignment. Annotate the port with @ {{ pin: \"...\" }}",
                cell.path
            )));
        };
        let &(x, y, bel) = pins.get(loc).ok_or_else(|| {
            let mut known: Vec<&str> = pins.keys().map(String::as_str).collect();
            known.sort_unstable();
            emit_err(format!(
                "Pin '{}' on '{}' has no known location. Known pins: {}",
                loc,
                cell.path,
                known.join(", ")
            ))
        })?;
        placement.placements.insert(
            cell.id,
            crate::placer::PlacementLoc::new(x, y, bel as usize, BelType::IoCell),
        );
    }
    Ok(())
}
