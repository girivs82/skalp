//! Prepare a placed design for routing and configuration.
//!
//! Family-independent: turns a netlist's connectivity into what the router and
//! bitstream emitters need, handling the connections with no routing (constants
//! and dedicated carry paths) and fixing IO cells on their assigned sites.

use crate::collections::{HashMap, HashMapExt};
use crate::device::{
    BelType, Device, DIST_RAM_PIN_RAD, FF_PIN_CE, FF_PIN_CLK, FF_PIN_LSR, WIDE_LUT_PIN_SEL,
};
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

/// Put block RAM connections on the device's RAM pins, for a flow that routes
/// the netlist as it is (without [`prepare_for_routing`]).
///
/// A RAM cell lists its inputs and outputs in [`RamPorts`] order; the device
/// says which pin each lands on ([`Device::ram_input_pin`],
/// [`Device::ram_output_pin`]). Inputs with no pin, and inputs tied low that
/// configuration holds low, are left unconnected.
///
/// [`RamPorts`]: skalp_lir::gate_netlist::RamPorts
pub fn map_ram_pins(netlist: &GateNetlist, device: &dyn Device) -> GateNetlist {
    let constants = constant_nets(netlist);
    let mut mapped = netlist.clone();
    for cell in &netlist.cells {
        let Some(ports) = cell.ram_ports() else {
            continue;
        };
        for (index, &net) in cell.inputs.iter().enumerate() {
            let pin = device.ram_input_pin(&ports, index).filter(|_| {
                constants.get(&net) != Some(&false)
                    || device.ram_input_needs_low_driver(&ports, index)
            });
            let fanout = &mut mapped.nets[net.0 as usize].fanout;
            if let Some(entry) = fanout.iter().position(|&(c, i)| c == cell.id && i == index) {
                match pin {
                    Some(pin) => fanout[entry].1 = pin,
                    None => {
                        fanout.remove(entry);
                    }
                }
            }
        }
        for (index, &net) in cell.outputs.iter().enumerate() {
            mapped.nets[net.0 as usize].driver_pin = Some(device.ram_output_pin(&ports, index));
        }
    }
    mapped
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
    drive_constant_pll_inputs(&mut driven, &constants, device);
    drive_constant_pad_outputs(&mut driven, &constants);
    driven
}

/// Give an output pad that shows a constant a LUT that outputs it. Nothing
/// else drives the pad's data input then: left unrouted, the input mux in
/// front of it passes whatever wire it selects by default (on the
/// CertusPro-NX, an LED meant to stay on followed a counter bit).
fn drive_constant_pad_outputs(netlist: &mut GateNetlist, constants: &HashMap<GateNetId, bool>) {
    let mut drivers: HashMap<bool, GateNetId> = HashMap::new();
    for index in 0..netlist.cells.len() {
        let cell = &netlist.cells[index];
        if cell.function != Some(CellFunction::OutputPad) {
            continue;
        }
        let Some(&value) = cell.inputs.first().and_then(|net| constants.get(net)) else {
            continue;
        };
        let library = cell.library.clone();
        let net = *drivers.entry(value).or_insert_with(|| {
            let name = format!("constant_pad_output_{}", u8::from(value));
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

/// Replace each constant input of a PLL with a LUT that outputs it: a PLL's
/// pins come through the fabric, whose input muxes cannot hold a constant.
fn drive_constant_pll_inputs(
    netlist: &mut GateNetlist,
    constants: &HashMap<GateNetId, bool>,
    device: &dyn Device,
) {
    let mut drivers: HashMap<bool, GateNetId> = HashMap::new();
    for index in 0..netlist.cells.len() {
        let cell = &netlist.cells[index];
        let tied: Vec<(usize, bool)> = (0..cell.inputs.len())
            .filter(|&i| device.pll_input_pin(cell, i).is_some())
            .filter_map(|i| Some((i, *constants.get(&cell.inputs[i])?)))
            .collect();
        if tied.is_empty() {
            continue;
        }
        let library = cell.library.clone();
        for (input, value) in tied {
            let net = *drivers.entry(value).or_insert_with(|| {
                let name = format!("constant_pll_input_{}", u8::from(value));
                let net = netlist.add_net(GateNet::new(GateNetId(0), name.clone()));
                let mut driver = Cell::new_comb(
                    CellId(0),
                    "LUT4".to_string(),
                    library.clone(),
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
            netlist.cells[index].inputs[input] = net;
        }
    }
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
        let pll_pins = (0..cell.outputs.len().max(cell.inputs.len())).any(|i| {
            device.pll_input_pin(cell, i).is_some() || device.pll_output_pin(cell, i).is_some()
        });
        if pll_pins {
            // Constant inputs were given drivers ([`drive_constant_inputs`])
            for (index, &net) in cell.inputs.iter().enumerate() {
                if let Some(pin) = device.pll_input_pin(cell, index) {
                    connect(&mut prepared.nets, net, cell.id, pin);
                }
            }
            for (index, &net) in cell.outputs.iter().enumerate() {
                if let Some(pin) = device.pll_output_pin(cell, index) {
                    prepared.nets[net.0 as usize].driver_pin = Some(pin);
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
            for (index, &net) in cell.outputs.iter().enumerate() {
                prepared.nets[net.0 as usize].driver_pin =
                    Some(device.ram_output_pin(&ports, index));
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
            if cell.function == Some(CellFunction::Lut5) {
                // A wide LUT's inputs 0..3 feed both of its LUTs; input 4 is
                // the select of the mux between them.
                if index < 4 {
                    connect(&mut prepared.nets, net, cell.id, index);
                    connect(&mut prepared.nets, net, cell.id, 4 + index);
                } else {
                    connect(&mut prepared.nets, net, cell.id, WIDE_LUT_PIN_SEL);
                }
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn constant_pad_outputs_get_a_driver() {
        let mut netlist = GateNetlist::new("top".to_string(), "test".to_string());
        let one = netlist.add_net(GateNet::new(GateNetId(0), "one".to_string()));
        let pad = netlist.add_net(GateNet::new(GateNetId(0), "led_pad".to_string()));
        let mut tie = Cell::new_comb(
            CellId(0),
            "TIE1_X1".to_string(),
            "test".to_string(),
            0.0,
            "tie".to_string(),
            Vec::new(),
            vec![one],
        );
        tie.function = Some(CellFunction::TieHigh);
        netlist.add_cell(tie);
        let mut obuf = Cell::new_comb(
            CellId(0),
            "SEIO33_OB".to_string(),
            "test".to_string(),
            0.0,
            "led_obuf".to_string(),
            vec![one],
            vec![pad],
        );
        obuf.function = Some(CellFunction::OutputPad);
        netlist.add_cell(obuf);

        let device =
            crate::device::ice40::Ice40Device::new(crate::device::ice40::Ice40Variant::Hx1k);
        let driven = drive_constant_inputs(&netlist, &device);
        let obuf = driven.cells.iter().find(|c| c.path == "led_obuf").unwrap();
        let driver = driven
            .cells
            .iter()
            .find(|c| c.outputs.contains(&obuf.inputs[0]))
            .expect("the pad's input has a driver");
        assert_eq!(driver.function, Some(CellFunction::Lut4));
        assert_eq!(driver.lut_init, Some(0xFFFF));
    }
}
