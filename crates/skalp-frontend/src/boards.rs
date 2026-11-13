//! Development boards: binding a design's ports to a declared board's pins,
//! and checking that every pin is used the way the board allows.
//!
//! A board (`board Name { ... }`) lists its FPGA's pins in named groups, each
//! with its direction from the FPGA's side, its I/O settings, or what it is
//! wired to when a design must leave it alone. A port bound with
//! `@ Name.group` (or `@ Name.group[hi:lo]`) takes its pins and I/O settings
//! from the board. A design that binds no port to a board is not checked.

use crate::hir::{
    BoardPinKind, Hir, HirBoard, HirBoardPin, HirPort, HirPortDirection, HirType, PinLocation,
};
use std::collections::HashMap;

/// Bits a port of this type carries on pins, where that is known.
fn pin_width(ty: &HirType) -> Option<u32> {
    match ty {
        HirType::Bit(n) | HirType::Logic(n) | HirType::Int(n) | HirType::Nat(n) => Some(*n),
        HirType::Bool | HirType::Clock(_) | HirType::Reset { .. } | HirType::Event => Some(1),
        _ => None,
    }
}

fn kind_name(kind: BoardPinKind) -> &'static str {
    match kind {
        BoardPinKind::Input => "an input to the FPGA",
        BoardPinKind::Output => "an output of the FPGA",
        BoardPinKind::Inout => "bidirectional",
        BoardPinKind::Reserved => "reserved",
    }
}

fn direction_name(direction: &HirPortDirection) -> &'static str {
    match direction {
        HirPortDirection::Input => "an input",
        HirPortDirection::Output => "an output",
        HirPortDirection::Bidirectional => "inout",
        HirPortDirection::Protocol => "a protocol port",
    }
}

/// Whether a port of `direction` may use pins of `kind`.
fn direction_fits(direction: &HirPortDirection, kind: BoardPinKind) -> bool {
    matches!(
        (direction, kind),
        (
            HirPortDirection::Input,
            BoardPinKind::Input | BoardPinKind::Inout
        ) | (
            HirPortDirection::Output,
            BoardPinKind::Output | BoardPinKind::Inout
        ) | (HirPortDirection::Bidirectional, BoardPinKind::Inout)
    )
}

fn balls(location: &PinLocation) -> Vec<String> {
    match location {
        PinLocation::Single(pin) => vec![pin.clone()],
        PinLocation::Multiple(pins) => pins.clone(),
        PinLocation::Differential { positive, negative } => {
            vec![positive.clone(), negative.clone()]
        }
    }
}

/// What a group is wired to, or its kind, for messages.
fn describe(board: &HirBoard, pin: &HirBoardPin) -> String {
    match &pin.connected {
        Some(what) => format!("{}.{} ({what})", board.name, pin.name),
        None => format!("{}.{}", board.name, pin.name),
    }
}

/// Give every port bound to a board pin its pins and I/O settings, and check
/// each entity that uses a board: every port on its pins in a direction the
/// board allows, of the width bound, at the board's I/O standard, no reserved
/// pin used, and no pin used twice. All problems are reported together.
pub fn resolve_board_bindings(hir: &mut Hir) -> Result<(), Vec<String>> {
    let mut errors = Vec::new();
    let boards = hir.boards.clone();
    for entity in &mut hir.entities {
        let mut used_board: Option<&HirBoard> = None;
        for port in &mut entity.ports {
            let Some(binding) = port
                .physical_constraints
                .as_ref()
                .and_then(|c| c.board_pin.clone())
            else {
                continue;
            };
            let at = format!("{}.{}", entity.name, port.name);
            let Some(board) = boards.iter().find(|b| b.name == binding.board) else {
                errors.push(format!(
                    "{at}: no board named '{}' (declare it, or import it with `use skalp::boards::{}`)",
                    binding.board, binding.board
                ));
                continue;
            };
            if let Some(other) = used_board.filter(|b| b.name != board.name) {
                errors.push(format!(
                    "{at}: binds to board '{}', but {} already uses board '{}'",
                    board.name, entity.name, other.name
                ));
                continue;
            }
            used_board = Some(board);
            let Some(pin) = board.pins.iter().find(|p| p.name == binding.pin) else {
                let names: Vec<_> = board.pins.iter().map(|p| p.name.as_str()).collect();
                errors.push(format!(
                    "{at}: board '{}' has no pin '{}' (it has {})",
                    board.name,
                    binding.pin,
                    names.join(", ")
                ));
                continue;
            };
            if let Err(e) = bind_port(port, board, pin, binding.bits) {
                errors.push(format!("{at}: {e}"));
            }
        }
        if let Some(board) = used_board {
            check_entity_pins(&entity.name, &entity.ports, board, &mut errors);
        }
    }
    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

/// Fill a bound port's constraints from its board pin group.
fn bind_port(
    port: &mut HirPort,
    board: &HirBoard,
    pin: &HirBoardPin,
    bits: Option<(u32, u32)>,
) -> Result<(), String> {
    if pin.kind == BoardPinKind::Reserved {
        return Err(format!(
            "{} is reserved on the board; a design may not use it",
            describe(board, pin)
        ));
    }
    if !direction_fits(&port.direction, pin.kind) {
        return Err(format!(
            "the port is {}, but {} is {}",
            direction_name(&port.direction),
            describe(board, pin),
            kind_name(pin.kind)
        ));
    }
    let location = pin
        .constraints
        .pin_location
        .as_ref()
        .ok_or_else(|| format!("{} has no pins", describe(board, pin)))?;
    let location = match (bits, location) {
        (None, location) => location.clone(),
        (Some((hi, lo)), PinLocation::Multiple(all)) => {
            if hi < lo || hi as usize >= all.len() {
                return Err(format!(
                    "bits [{hi}:{lo}] are outside {}, which has {} pins",
                    describe(board, pin),
                    all.len()
                ));
            }
            let picked = all[lo as usize..=hi as usize].to_vec();
            if picked.len() == 1 {
                PinLocation::Single(picked[0].clone())
            } else {
                PinLocation::Multiple(picked)
            }
        }
        (Some((0, 0)), single) => single.clone(),
        (Some((hi, lo)), _) => {
            return Err(format!(
                "bits [{hi}:{lo}] are outside {}, which has one pin",
                describe(board, pin)
            ))
        }
    };
    let bound = match &location {
        PinLocation::Multiple(pins) => pins.len() as u32,
        _ => 1,
    };
    if let Some(width) = pin_width(&port.port_type) {
        if width != bound {
            return Err(format!(
                "the port is {width} bit(s) wide, but it is bound to {bound} pin(s) of {}",
                describe(board, pin)
            ));
        }
    }
    let binding = port
        .physical_constraints
        .as_ref()
        .and_then(|c| c.board_pin.clone());
    let mut constraints = pin.constraints.clone();
    constraints.pin_location = Some(location);
    constraints.board_pin = binding;
    port.physical_constraints = Some(constraints);
    Ok(())
}

/// Check every pin an entity's ports use against the board it is on.
fn check_entity_pins(entity: &str, ports: &[HirPort], board: &HirBoard, errors: &mut Vec<String>) {
    // Which group each of the board's balls belongs to
    let mut groups: HashMap<String, &HirBoardPin> = HashMap::new();
    for pin in &board.pins {
        if let Some(location) = &pin.constraints.pin_location {
            for ball in balls(location) {
                groups.insert(ball, pin);
            }
        }
    }
    let mut users: HashMap<String, String> = HashMap::new();
    for port in ports {
        let Some(constraints) = &port.physical_constraints else {
            continue;
        };
        let Some(location) = &constraints.pin_location else {
            continue;
        };
        let at = format!("{entity}.{}", port.name);
        for ball in balls(location) {
            if let Some(first) = users.insert(ball.clone(), port.name.clone()) {
                errors.push(format!(
                    "{at}: pin {ball} is already used by {entity}.{first}"
                ));
            }
            // Bound ports were checked against their own group already
            if constraints.board_pin.is_some() {
                continue;
            }
            let Some(pin) = groups.get(&ball) else {
                continue;
            };
            if pin.kind == BoardPinKind::Reserved {
                errors.push(format!(
                    "{at}: pin {ball} is {}, which is reserved on the board",
                    describe(board, pin)
                ));
            } else if !direction_fits(&port.direction, pin.kind) {
                errors.push(format!(
                    "{at}: pin {ball} is {}, which is {}, but the port is {}",
                    describe(board, pin),
                    kind_name(pin.kind),
                    direction_name(&port.direction)
                ));
            } else if let (Some(want), Some(have)) =
                (&pin.constraints.io_standard, &constraints.io_standard)
            {
                if want != have {
                    errors.push(format!(
                        "{at}: pin {ball} is {}, which runs at {want}, but the port asks for {have}",
                        describe(board, pin)
                    ));
                }
            }
        }
    }
}
