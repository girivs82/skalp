//! Prepare a placed design for routing and configuration.
//!
//! Family-independent: turns a netlist's connectivity into what the router and
//! bitstream emitters need, handling the connections with no routing (constants
//! and dedicated carry paths) and fixing IO cells on their assigned sites.

use crate::collections::{HashMap, HashMapExt};
use crate::device::{BelType, Device, DIST_RAM_PIN_RAD, FF_PIN_CE, FF_PIN_CLK, FF_PIN_LSR};
use crate::error::{PlaceRouteError, Result};
use crate::placer::PlacementResult;
use skalp_lir::gate_netlist::{Cell, CellId, GateNet, GateNetId, GateNetlist};
use skalp_lir::tech_library::CellFunction;

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

fn is_multiplier(cell: &Cell) -> bool {
    cell.dsp_ports().is_some()
}

fn is_flip_flop(cell: &Cell) -> bool {
    cell.clock.is_some()
}

/// Router pin number of a flip-flop's `index`-th input.
pub(crate) fn ff_pin(cell: &Cell, index: usize) -> usize {
    match (cell.function.as_ref(), index) {
        (Some(CellFunction::DffE), 1) | (Some(CellFunction::DffRE), 2) => FF_PIN_CE,
        (Some(CellFunction::DffR), 1) | (Some(CellFunction::DffRE), 1) => FF_PIN_LSR,
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

/// Give block RAM and multiplier inputs tied low a real driver where the device
/// needs one.
///
/// A device may be able to hold some RAM inputs high or low with configuration
/// alone; the others must be driven. This adds a constant-zero LUT per RAM that
/// needs one, so those inputs are routed like any signal.
pub fn drive_constant_inputs(netlist: &GateNetlist, device: &dyn Device) -> GateNetlist {
    let constants = constant_nets(netlist);
    let mut driven = netlist.clone();
    for ram in netlist.cells.iter() {
        let ports = ram.ram_ports();
        if ports.is_none() && !is_multiplier(ram) {
            continue;
        }
        let inputs: Vec<usize> = (0..ram.inputs.len())
            .filter(|&i| {
                constants.get(&ram.inputs[i]) == Some(&false)
                    && match &ports {
                        Some(ports) => device.ram_input_needs_low_driver(ports, i),
                        None => device.dsp_input_needs_low_driver(ram, i),
                    }
            })
            .collect();
        let extra_low_inputs = device.dsp_low_driven_input_count(ram);
        if inputs.is_empty() && extra_low_inputs == 0 {
            continue;
        }
        let net = driven.add_net(GateNet::new(GateNetId(0), format!("{}.zero", ram.path)));
        let mut zero = Cell::new_comb(
            CellId(0),
            "LUT4".to_string(),
            ram.library.clone(),
            0.0,
            format!("{}.zero_driver", ram.path),
            Vec::new(),
            vec![net],
        );
        zero.function = Some(CellFunction::Lut4);
        zero.lut_init = Some(0);
        driven.add_cell(zero);
        let copy = driven
            .cells
            .iter_mut()
            .find(|c| c.id == ram.id)
            .expect("the RAM cell was copied");
        for input in inputs {
            copy.inputs[input] = net;
        }
        if extra_low_inputs > 0 {
            copy.parameters.insert(
                crate::device::DSP_EXTRA_INPUTS_PARAM.to_string(),
                copy.inputs.len().to_string(),
            );
            copy.inputs.extend(vec![net; extra_low_inputs]);
        }
    }
    drive_constant_flip_flop_data(&mut driven, &constants);
    driven
}

/// Replace a constant flip-flop data input with a LUT that outputs it.
///
/// A flip-flop's data input cannot be held at a constant by configuration, so
/// one constant LUT per value drives every flip-flop that needs it.
fn drive_constant_flip_flop_data(netlist: &mut GateNetlist, constants: &HashMap<GateNetId, bool>) {
    let mut drivers: HashMap<bool, GateNetId> = HashMap::new();
    for index in 0..netlist.cells.len() {
        let cell = &netlist.cells[index];
        if !is_flip_flop(cell) || cell.ram_ports().is_some() || is_multiplier(cell) {
            continue;
        }
        let Some(&value) = cell.inputs.first().and_then(|net| constants.get(net)) else {
            continue;
        };
        let library = cell.library.clone();
        let net = *drivers.entry(value).or_insert_with(|| {
            let name = format!("constant_ff_data_{}", u8::from(value));
            let net = netlist.add_net(GateNet::new(GateNetId(0), name.clone()));
            let mut driver = Cell::new_comb(
                CellId(0),
                "LUT4".to_string(),
                library,
                0.0,
                format!("{name}_driver"),
                Vec::new(),
                vec![net],
            );
            driver.function = Some(CellFunction::Lut4);
            driver.lut_init = Some(if value { 0xFFFF } else { 0 });
            netlist.add_cell(driver);
            net
        });
        netlist.cells[index].inputs[0] = net;
    }
}

/// Rewrite a netlist's connectivity into what the router should route.
///
/// Rebuilds every net's fanout from the cells, because clock pins are absent
/// from it. Leaves out connections with no routing: constants (set with
/// [`emit_bits`]) and the dedicated carry path from one `CCU2` to the next.
pub fn prepare_for_routing(
    netlist: &GateNetlist,
    device: &dyn Device,
) -> (GateNetlist, TiedInputs) {
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
        if is_multiplier(cell) {
            for (index, &net) in cell.inputs.iter().enumerate() {
                if let Some(&value) = constants.get(&net) {
                    tied.values.insert((cell.id, index), value);
                } else if let Some(pin) = device.dsp_input_pin(cell, index) {
                    connect(&mut prepared.nets, net, cell.id, pin);
                }
            }
            continue;
        }
        if let Some(ports) = cell.ram_ports() {
            for (index, &net) in cell.inputs.iter().enumerate() {
                if let Some(&value) = constants.get(&net) {
                    tied.values.insert((cell.id, index), value);
                } else if let Some(pin) = device.ram_input_pin(&ports, index) {
                    connect(&mut prepared.nets, net, cell.id, pin);
                }
            }
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
    let pins = device
        .default_package()
        .and_then(|name| packages.get(name))
        .or_else(|| packages.values().next())
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
        let pad_wire = device
            .bel_pin_wire(x, y, bel as usize, crate::device::CellPin::Input(0))
            .or_else(|| device.bel_pin_wire(x, y, bel as usize, crate::device::CellPin::Output(0)));
        if pad_wire.is_none() {
            return Err(emit_err(format!(
                "Pin '{loc}' on '{}' is on an IO site with no device data",
                cell.path
            )));
        }
        placement.placements.insert(
            cell.id,
            crate::placer::PlacementLoc::new(x, y, bel as usize, BelType::IoCell),
        );
    }
    Ok(())
}
